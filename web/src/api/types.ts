// Mirrors the wire contract in src/Chilin/Types.hs and src/Chilin/Store.hs.
// Every field here is emitted by an explicit ToJSON instance or an `object`
// literal on the server; nothing is derived from Haskell record selectors.

export type Actor = { id: string; admin: boolean };

export type Repository = { id: string; owner: string; name: string; public: boolean };

export type TokenInfo = { id: string; label: string; created_at: string };

export type IdentityInfo = { provider: string; subject: string; user: string };

// Myque.Item enumerations, in schema order.
export const KINDS = ["task", "issue", "bug", "milestone", "epic", "followup"] as const;
export const STATES = ["open", "active", "blocked", "deferred", "done", "cancelled"] as const;

export type Kind = (typeof KINDS)[number];
export type State = (typeof STATES)[number];

// `done` and `cancelled` are terminal: the server requires `closed` on those
// and forbids it otherwise.
export const TERMINAL_STATES: readonly State[] = ["done", "cancelled"];

export type ItemMetadata = {
  item_id: string;
  author: string;
  assignees: string[];
  milestone: string | null;
};

export type PullReview = {
  actor: string;
  head_oid: string;
  verdict: string;
  body: string;
  created_at: string;
  operation_id: string;
};

export type Pull = {
  item_id: string;
  author: string;
  status: string;
  draft: boolean;
  source_owner: string;
  source_name: string;
  source_id: string;
  source_ref: string;
  target_ref: string;
  head_oid: string;
  target_oid: string;
  reviews: PullReview[];
  merge_oid: string | null;
};

export type Item = {
  id: string;
  kind: Kind;
  key: string | null;
  title: string;
  body: string;
  state: State;
  created: string;
  updated: string | null;
  closed: string | null;
  tags: string[];
  parent: string | null;
  depends: string[];
  blocks: string[];
  related: string[];
  duplicate_of: string | null;
  supersedes: string[];
  // Injected by Store.renderItem when the corresponding record exists.
  metadata?: ItemMetadata;
  // A pull request is a `task` item that also carries a pull record, so
  // `kind` alone cannot distinguish the two; presence of `pull` is the test.
  pull?: Pull;
  milestone?: { item_id: string; due_at: string | null };
};

export type Me = { user: Actor; identities: IdentityInfo[] };

/** Every read response carries the tracker revision it was computed from. */
export type Snapshot<T> = { revision: string; data: T };

export type MutationEnvelope<T> = { revision: string; data: T; replayed: boolean };
