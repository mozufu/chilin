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

export type ItemFilters = { kind?: Kind; state?: State };

/**
 * A point in tracker history. The two fields are mutually exclusive on the
 * server (Chilin.Store.snapshotForRead), so they are modelled as a tagged
 * choice rather than two optional fields that could both be set.
 */
export type History = { at: "now" } | { at: "revision"; revision: string } | { at: "time"; asOf: string };

export const LIVE: History = { at: "now" };

const historyQuery = (history: History): Record<string, string> => {
  if (history.at === "revision") return { at_revision: history.revision };
  if (history.at === "time") return { as_of: history.asOf };
  return {};
};

export const keys = {
  me: ["me"] as const,
  tokens: ["tokens"] as const,
  identities: ["identities"] as const,
  repos: ["repos"] as const,
  permissions: (owner: string, name: string) => ["permissions", owner, name] as const,
  // History is part of every tracker read key: the same path at two points in
  // history is two distinct resources, and caching them together would show
  // stale content when the viewer travels.
  repo: (owner: string, name: string, history: History = LIVE) =>
    ["repo", owner, name, history] as const,
  items: (owner: string, name: string, filters: ItemFilters, history: History = LIVE) =>
    ["items", owner, name, filters, history] as const,
  item: (owner: string, name: string, id: string, history: History = LIVE) =>
    ["item", owner, name, id, history] as const,
  timeline: (owner: string, name: string, id: string, history: History = LIVE) =>
    ["timeline", owner, name, id, history] as const,
  progress: (owner: string, name: string, id: string, history: History = LIVE) =>
    ["progress", owner, name, id, history] as const,
};

const repoPath = (owner: string, name: string) =>
  `/api/repos/${encodeURIComponent(owner)}/${encodeURIComponent(name)}`;

export const useMe = () =>
  useQuery({
    queryKey: keys.me,
    queryFn: async () => (await request<Me>("/api/me")).value,
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

export const useRepository = (owner: string, name: string, history: History = LIVE) =>
  useQuery({
    queryKey: keys.repo(owner, name, history),
    queryFn: async () => {
      const query = new URLSearchParams(historyQuery(history)).toString();
      const { value } = await request<{
        repository: Repository;
        revision: string;
        files: Record<string, string>;
        items: Item[];
      }>(`${repoPath(owner, name)}${query === "" ? "" : `?${query}`}`);
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
export const useItems = (owner: string, name: string, filters: ItemFilters, history: History = LIVE) =>
  useInfiniteQuery({
    queryKey: keys.items(owner, name, filters, history),
    initialPageParam: null as string | null,
    queryFn: async ({ pageParam }) => {
      const query = new URLSearchParams({ limit: "50", ...historyQuery(history) });
      if (filters.kind !== undefined) query.set("kind", filters.kind);
      if (filters.state !== undefined) query.set("state", filters.state);
      // A cursor already pins its revision, and the server rejects a cursor
      // that disagrees with at_revision, so the pin is dropped once paging.
      if (pageParam !== null) {
        query.delete("at_revision");
        query.set("cursor", pageParam);
      }
      const { value } = await request<{
        revision: string;
        data: { items: Item[]; next_cursor: string | null };
      }>(`${repoPath(owner, name)}/items?${query.toString()}`);
      return value;
    },
    getNextPageParam: (last) => last.data.next_cursor,
  });

export const useItem = (owner: string, name: string, id: string, history: History = LIVE) =>
  useQuery({
    queryKey: keys.item(owner, name, id, history),
    queryFn: async () => {
      const query = new URLSearchParams(historyQuery(history)).toString();
      const { value } = await request<{ revision: string; data: Item }>(
        `${repoPath(owner, name)}/items/${encodeURIComponent(id)}${query === "" ? "" : `?${query}`}`,
      );
      return value.data;
    },
  });

export const useTimeline = (owner: string, name: string, id: string, history: History = LIVE) =>
  useQuery({
    queryKey: keys.timeline(owner, name, id, history),
    queryFn: async () => {
      const query = new URLSearchParams(historyQuery(history)).toString();
      const { value } = await request<{ revision: string; data: { events: TimelineEvent[] } }>(
        `${repoPath(owner, name)}/items/${encodeURIComponent(id)}/timeline${query === "" ? "" : `?${query}`}`,
      );
      return value.data.events;
    },
  });

export const useMilestoneProgress = (
  owner: string,
  name: string,
  id: string,
  history: History = LIVE,
) =>
  useQuery({
    queryKey: keys.progress(owner, name, id, history),
    queryFn: async () => {
      const query = new URLSearchParams(historyQuery(history)).toString();
      const { value } = await request<{ revision: string; data: MilestoneProgress }>(
        `${repoPath(owner, name)}/milestones/${encodeURIComponent(id)}/progress${query === "" ? "" : `?${query}`}`,
      );
      return value.data;
    },
  });

export type Permission = { user: string; access: "read" | "write" | "admin" };

export const usePermissions = (owner: string, name: string, enabled: boolean) =>
  useQuery({
    queryKey: keys.permissions(owner, name),
    enabled,
    queryFn: async () =>
      (await request<{ permissions: Permission[] }>(`${repoPath(owner, name)}/permissions`)).value
        .permissions,
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

/**
 * Grants or revokes repository access. Unlike a tracker write this carries no
 * revision precondition: permissions live in the registry, not in tracker.git.
 */
export const usePermissionMutation = (owner: string, name: string) => {
  const client = useQueryClient();
  return useMutation<void, ApiError, { user: string; access: Permission["access"] | null }>({
    mutationFn: async ({ user, access }) => {
      const base = `${repoPath(owner, name)}/permissions`;
      if (access === null) {
        await request<void>(`${base}/${encodeURIComponent(user)}`, { method: "DELETE" });
        return;
      }
      await request<void>(base, { method: "POST", body: { user, access } });
    },
    onSuccess: () => {
      void client.invalidateQueries({ queryKey: keys.permissions(owner, name) });
    },
  });
};
