import {
  useInfiniteQuery,
  useMutation,
  useQuery,
  useQueryClient,
  type QueryClient,
} from "@tanstack/react-query";
import { ApiError, mutate, request, type MutationOutcome } from "./client";
import type {
  IdentityInfo,
  Item,
  Kind,
  Me,
  MilestoneProgress,
  Repository,
  State,
  TimelineEvent,
  TokenInfo,
} from "./types";

export const keys = {
  me: ["me"] as const,
  tokens: ["tokens"] as const,
  identities: ["identities"] as const,
  repos: ["repos"] as const,
  repo: (owner: string, name: string) => ["repo", owner, name] as const,
  items: (owner: string, name: string, filters: ItemFilters) =>
    ["items", owner, name, filters] as const,
  item: (owner: string, name: string, id: string) => ["item", owner, name, id] as const,
  timeline: (owner: string, name: string, id: string) => ["timeline", owner, name, id] as const,
  progress: (owner: string, name: string, id: string) => ["progress", owner, name, id] as const,
};

export type ItemFilters = { kind?: Kind; state?: State };

const repoPath = (owner: string, name: string) =>
  `/api/repos/${encodeURIComponent(owner)}/${encodeURIComponent(name)}`;

export const useMe = () =>
  useQuery({
    queryKey: keys.me,
    queryFn: async () => (await request<Me>("/api/me")).value,
    // Both outcomes are answers, not transient faults: 401 is "no credential"
    // and 403 is "proxy identity not linked". Retrying either just delays the
    // sign-in view.
    retry: (count, error) =>
      !(error instanceof ApiError && (error.isUnauthorized || error.isForbidden)) && count < 2,
    staleTime: 60_000,
  });

export const useTokens = () =>
  useQuery({
    queryKey: keys.tokens,
    queryFn: async () => (await request<{ tokens: TokenInfo[] }>("/api/tokens")).value.tokens,
  });

export const useIdentities = () =>
  useQuery({
    queryKey: keys.identities,
    queryFn: async () => (await request<{ identities: IdentityInfo[] }>("/api/identities")).value.identities,
  });

export const useRepositories = () =>
  useQuery({
    queryKey: keys.repos,
    queryFn: async () =>
      (await request<{ repositories: Repository[] }>("/api/repos")).value.repositories,
  });

/**
 * A repository read, cached together with the tracker revision it produced.
 * Mutations consume that revision as their If-Match precondition, so the cache
 * is the single source of truth for "what the client last saw".
 */
export type RepoSnapshot = { repository: Repository; revision: string; items: Item[] };

export const useRepository = (owner: string, name: string) =>
  useQuery({
    queryKey: keys.repo(owner, name),
    queryFn: async () => {
      const { value } = await request<{
        repository: Repository;
        revision: string;
        files: Record<string, string>;
        items: Item[];
      }>(repoPath(owner, name));
      return { repository: value.repository, revision: value.revision, items: value.items };
    },
  });

/**
 * Paged item listing.
 *
 * The list deliberately targets /items rather than /issues: the server's
 * collection filter only recognises issues (issue plus bug), milestones and
 * pulls, so epic, task and followup items are reachable nowhere else.
 *
 * A cursor pins the tracker revision it was produced from, which is what makes
 * paging stable while other writers advance the tracker.
 */
export const useItems = (owner: string, name: string, filters: ItemFilters) =>
  useInfiniteQuery({
    queryKey: keys.items(owner, name, filters),
    initialPageParam: null as string | null,
    queryFn: async ({ pageParam }) => {
      const query = new URLSearchParams({ limit: "50" });
      if (filters.kind !== undefined) query.set("kind", filters.kind);
      if (filters.state !== undefined) query.set("state", filters.state);
      if (pageParam !== null) query.set("cursor", pageParam);
      const { value } = await request<{
        revision: string;
        data: { items: Item[]; next_cursor: string | null };
      }>(`${repoPath(owner, name)}/items?${query.toString()}`);
      return value;
    },
    getNextPageParam: (last) => last.data.next_cursor,
  });

export const useItem = (owner: string, name: string, id: string) =>
  useQuery({
    queryKey: keys.item(owner, name, id),
    queryFn: async () => {
      const { value } = await request<{ revision: string; data: Item }>(
        `${repoPath(owner, name)}/items/${encodeURIComponent(id)}`,
      );
      return value.data;
    },
  });

export const useTimeline = (owner: string, name: string, id: string) =>
  useQuery({
    queryKey: keys.timeline(owner, name, id),
    queryFn: async () => {
      const { value } = await request<{ revision: string; data: { events: TimelineEvent[] } }>(
        `${repoPath(owner, name)}/items/${encodeURIComponent(id)}/timeline`,
      );
      return value.data.events;
    },
  });

export const useMilestoneProgress = (owner: string, name: string, id: string) =>
  useQuery({
    queryKey: keys.progress(owner, name, id),
    queryFn: async () => {
      const { value } = await request<{ revision: string; data: MilestoneProgress }>(
        `${repoPath(owner, name)}/milestones/${encodeURIComponent(id)}/progress`,
      );
      return value.data;
    },
  });

/**
 * Reads the revision the client most recently observed for a repository.
 * Returns null when nothing has been read yet, which forces the caller to
 * fetch before mutating rather than guessing a precondition.
 */
const cachedRevision = (client: QueryClient, owner: string, name: string): string | null => {
  const snapshot = client.getQueryData<RepoSnapshot>(keys.repo(owner, name));
  return snapshot?.revision ?? null;
};

export type TrackerMutation<TVariables> = {
  owner: string;
  name: string;
  /** Builds the request path and body for one logical action. */
  build: (variables: TVariables) => { path: string; body: unknown; method?: string };
};

/**
 * Drives a precondition-guarded tracker mutation.
 *
 * Two invariants the server depends on:
 *
 *  - One idempotency key per user action, reused across every retry of that
 *    action. Chilin stores the key with a request fingerprint, so a replayed
 *    identical request returns the original result rather than acting twice.
 *  - The If-Match revision must be the one the client actually read. On 412
 *    the repository is refetched and the mutation retried once against the
 *    fresh revision; a second failure surfaces as a genuine conflict.
 */
export const useTrackerMutation = <TVariables, TResult>(
  config: TrackerMutation<TVariables>,
) => {
  const client = useQueryClient();
  return useMutation<MutationOutcome<TResult>, ApiError, TVariables>({
    mutationFn: async (variables) => {
      const { path, body, method } = config.build(variables);
      // Generated once per action, deliberately outside the retry loop.
      const idempotencyKey = crypto.randomUUID();

      const attempt = async (revision: string) =>
        mutate<TResult>(path, {
          ...(method !== undefined ? { method } : {}),
          body,
          revision,
          idempotencyKey,
        });

      let revision = cachedRevision(client, config.owner, config.name);
      if (revision === null) {
        const refreshed = await client.fetchQuery<RepoSnapshot>({
          queryKey: keys.repo(config.owner, config.name),
        });
        revision = refreshed.revision;
      }

      try {
        return await attempt(revision);
      } catch (error) {
        if (!(error instanceof ApiError) || !error.isStale) throw error;
        // Someone else advanced the tracker. Re-read and replay the same key:
        // if our write already landed, the server returns it as a replay.
        const refreshed = await client.fetchQuery<RepoSnapshot>({
          queryKey: keys.repo(config.owner, config.name),
        });
        return await attempt(refreshed.revision);
      }
    },
    onSuccess: (outcome) => {
      // Adopt the post-mutation revision immediately so a follow-up action
      // does not need a round trip to learn its own precondition.
      client.setQueryData<RepoSnapshot>(keys.repo(config.owner, config.name), (previous) =>
        previous === undefined ? previous : { ...previous, revision: outcome.revision },
      );
      // Any tracker write can change item content, membership and history, so
      // every read derived from the tracker is dropped rather than guessed at.
      for (const prefix of ["repo", "items", "item", "timeline", "progress"]) {
        void client.invalidateQueries({ queryKey: [prefix, config.owner, config.name] });
      }
    },
  });
};

/** A credential mutation: no tracker precondition, but an intent header. */
export const useCredentialMutation = <TVariables, TResult>(
  build: (variables: TVariables) => { path: string; method: string; body?: unknown },
  invalidate: readonly unknown[],
) => {
  const client = useQueryClient();
  return useMutation<TResult, ApiError, TVariables>({
    mutationFn: async (variables) => {
      const { path, method, body } = build(variables);
      const { value } = await request<TResult>(path, {
        method,
        intent: true,
        ...(body !== undefined ? { body } : {}),
      });
      return value;
    },
    onSuccess: () => {
      void client.invalidateQueries({ queryKey: invalidate });
    },
  });
};
