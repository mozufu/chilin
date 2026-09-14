import type { MutationEnvelope } from "./types";

/**
 * A structured server error. The backend answers every failure with
 * `{"error":{"status":n,"message":s}}` (Chilin.Server.errorResponse), so the
 * status is always recoverable without inspecting prose.
 */
export class ApiError extends Error {
  readonly status: number;

  constructor(status: number, message: string) {
    super(message);
    this.name = "ApiError";
    this.status = status;
  }

  /** The caller's base revision was stale; re-read before retrying. */
  get isStale(): boolean {
    return this.status === 412;
  }

  get isUnauthorized(): boolean {
    return this.status === 401;
  }

  get isForbidden(): boolean {
    return this.status === 403;
  }

  get isConflict(): boolean {
    return this.status === 409;
  }
}

/**
 * Bearer credential held for the session. Kept in memory only: persisting it
 * in localStorage would turn any XSS into permanent credential theft, and the
 * proxy-authenticated deployment does not need it at all.
 */
let bearer: string | null = null;

export const setBearer = (token: string | null): void => {
  bearer = token;
};

/**
 * Mutations that carry no If-Match precondition still need cross-site
 * protection. A custom header suffices: a cross-origin form cannot set one,
 * and a cross-origin fetch is blocked outright because chilin sends no CORS
 * headers.
 */
const INTENT_HEADER = "X-Chilin-Intent";

type RequestOptions = {
  method?: string;
  body?: unknown;
  /** Base revision for a tracker mutation, sent as If-Match. */
  revision?: string;
  /** Stable key identifying one logical mutation across retries. */
  idempotencyKey?: string;
  /** Marks a credential route that needs the intent header. */
  intent?: boolean;
  signal?: AbortSignal;
};

export type Envelope<T> = {
  value: T;
  /** Parsed from the ETag header; present on every tracker read and write. */
  revision: string | null;
};

const parseETag = (raw: string | null): string | null => {
  if (raw === null) return null;
  const unquoted = raw.replace(/^W\//, "").replace(/^"|"$/g, "");
  return unquoted.length > 0 ? unquoted : null;
};

const errorFrom = async (response: Response): Promise<ApiError> => {
  let message = response.statusText || `request failed with ${response.status}`;
  try {
    const body: unknown = await response.json();
    if (
      typeof body === "object" &&
      body !== null &&
      "error" in body &&
      typeof body.error === "object" &&
      body.error !== null &&
      "message" in body.error &&
      typeof body.error.message === "string"
    ) {
      message = body.error.message;
    }
  } catch {
    // A non-JSON body means an intermediary answered; keep the status text.
  }
  return new ApiError(response.status, message);
};

export const request = async <T>(path: string, options: RequestOptions = {}): Promise<Envelope<T>> => {
  const headers = new Headers();
  if (bearer !== null) headers.set("Authorization", `Bearer ${bearer}`);
  if (options.body !== undefined) headers.set("Content-Type", "application/json");
  if (options.revision !== undefined) headers.set("If-Match", `"${options.revision}"`);
  if (options.idempotencyKey !== undefined) headers.set("Idempotency-Key", options.idempotencyKey);
  if (options.intent === true) headers.set(INTENT_HEADER, "1");

  const init: RequestInit = {
    method: options.method ?? "GET",
    headers,
    // Proxy-issued session cookies must ride along; the deployment is
    // same-origin so this never widens the credential surface.
    credentials: "same-origin",
  };
  if (options.body !== undefined) init.body = JSON.stringify(options.body);
  if (options.signal !== undefined) init.signal = options.signal;

  const response = await fetch(path, init);
  if (!response.ok) throw await errorFrom(response);

  const revision = parseETag(response.headers.get("ETag"));
  if (response.status === 204) return { value: undefined as T, revision };
  return { value: (await response.json()) as T, revision };
};

/** A tracker mutation response, with its post-mutation revision. */
export type MutationOutcome<T> = { data: T; revision: string; replayed: boolean };

/**
 * Performs a precondition-guarded mutation.
 *
 * The idempotency key is supplied by the caller and deliberately *not*
 * regenerated on retry: chilin records the key with the request fingerprint,
 * so replaying an identical request returns the original result instead of
 * duplicating the effect (Chilin.Store.commitMutation).
 */
export const mutate = async <T>(
  path: string,
  options: { method?: string; body: unknown; revision: string; idempotencyKey: string; signal?: AbortSignal },
): Promise<MutationOutcome<T>> => {
  const envelope = await request<MutationEnvelope<T>>(path, {
    method: options.method ?? "POST",
    body: options.body,
    revision: options.revision,
    idempotencyKey: options.idempotencyKey,
    ...(options.signal !== undefined ? { signal: options.signal } : {}),
  });
  return {
    data: envelope.value.data,
    revision: envelope.value.revision,
    replayed: envelope.value.replayed,
  };
};
