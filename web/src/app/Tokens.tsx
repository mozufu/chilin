import { useState } from "react";
import { keys, useCredentialMutation, useTokens } from "../api/queries";
import type { TokenInfo } from "../api/types";
import { Button, Empty, ErrorNotice, Field, Panel, Spinner, inputClass } from "./ui";

type Created = { token: TokenInfo; secret: string };

/**
 * Token administration. The secret exists in exactly one response body, so the
 * view holds it until dismissed and says plainly that it cannot be recovered.
 */
export const Tokens = () => {
  const tokens = useTokens();
  const [label, setLabel] = useState("");
  const [created, setCreated] = useState<Created | null>(null);

  const create = useCredentialMutation<{ label: string }, Created>(
    (variables) => ({ path: "/api/tokens", method: "POST", body: { label: variables.label } }),
    keys.tokens,
  );

  const revoke = useCredentialMutation<{ id: string }, void>(
    (variables) => ({ path: `/api/tokens/${encodeURIComponent(variables.id)}`, method: "DELETE" }),
    keys.tokens,
  );

  return (
    <div className="space-y-4">
      <Panel
        title="Access tokens"
        description="Used for git clone, push, and API access. The last remaining token cannot be revoked."
      >
        {tokens.isPending && <Spinner label="Loading tokens" />}
        {tokens.error !== null && <ErrorNotice error={tokens.error} />}
        {tokens.data !== undefined &&
          (tokens.data.length === 0 ? (
            <Empty>No tokens.</Empty>
          ) : (
            <ul className="divide-y divide-neutral-800">
              {tokens.data.map((token) => (
                <li key={token.id} className="flex items-center justify-between gap-4 py-2.5">
                  <div className="min-w-0">
                    <p className="truncate text-sm text-neutral-200">{token.label}</p>
                    <p className="font-mono text-xs text-neutral-600">
                      {token.id.slice(0, 12)}… · created {token.created_at}
                    </p>
                  </div>
                  <Button
                    variant="danger"
                    disabled={revoke.isPending || tokens.data.length === 1}
                    onClick={() => {
                      // A revealed secret whose token no longer exists is
                      // misleading, so it is dropped alongside the token.
                      if (created?.token.id === token.id) setCreated(null);
                      revoke.mutate({ id: token.id });
                    }}
                  >
                    Revoke
                  </Button>
                </li>
              ))}
            </ul>
          ))}
        {revoke.error !== null && (
          <div className="mt-3">
            <ErrorNotice error={revoke.error} />
          </div>
        )}
      </Panel>

      <Panel title="Issue a token">
        <form
          className="space-y-3"
          onSubmit={(event) => {
            event.preventDefault();
            create.mutate(
              { label: label.trim() },
              {
                onSuccess: (result) => {
                  setCreated(result);
                  setLabel("");
                },
              },
            );
          }}
        >
          <Field label="Label" hint="what this credential is for">
            <input
              className={inputClass}
              value={label}
              onChange={(event) => setLabel(event.target.value)}
              placeholder="laptop"
            />
          </Field>
          {create.error !== null && <ErrorNotice error={create.error} />}
          <Button type="submit" variant="primary" disabled={create.isPending || label.trim().length === 0}>
            {create.isPending ? "Issuing…" : "Issue token"}
          </Button>
        </form>

        {created !== null && (
          <div className="mt-4 rounded-lg border border-emerald-900/60 bg-emerald-950/30 px-4 py-3">
            <p className="text-xs font-medium text-emerald-300">
              Copy this now — it is shown once and cannot be recovered.
            </p>
            <code className="mt-2 block break-all rounded bg-neutral-950 px-3 py-2 font-mono text-xs text-emerald-200">
              {created.secret}
            </code>
            <div className="mt-3 flex gap-2">
              <Button onClick={() => void navigator.clipboard.writeText(created.secret)}>Copy</Button>
              <Button onClick={() => setCreated(null)}>Dismiss</Button>
            </div>
          </div>
        )}
      </Panel>
    </div>
  );
};
