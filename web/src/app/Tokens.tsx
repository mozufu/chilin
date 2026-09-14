import { useState } from "react";
import { keys, useCredentialMutation, useSshKeys, useTokens } from "../api/queries";
import type { SshKeyInfo, TokenInfo } from "../api/types";
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

      <SshKeys />
    </div>
  );
};


/**
 * SSH key administration. Unlike a token, a public key is not a secret, so it
 * is shown in full and the server reports the fingerprint it derived — the
 * same value `ssh-keygen -lf` prints, so a user can confirm they registered
 * the key they meant to.
 */
const SshKeys = () => {
  const sshKeys = useSshKeys();
  const [label, setLabel] = useState("");
  const [key, setKey] = useState("");

  const add = useCredentialMutation<{ label: string; key: string }, { key: SshKeyInfo }>(
    (variables) => ({ path: "/api/ssh-keys", method: "POST", body: variables }),
    keys.sshKeys,
  );

  const remove = useCredentialMutation<{ id: string }, void>(
    (variables) => ({ path: `/api/ssh-keys/${encodeURIComponent(variables.id)}`, method: "DELETE" }),
    keys.sshKeys,
  );

  return (
    <Panel title="SSH keys" description="Public keys allowed to clone and push over SSH">
      {sshKeys.isPending && <Spinner label="Loading SSH keys" />}
      {sshKeys.error !== null && <ErrorNotice error={sshKeys.error} />}
      {sshKeys.data !== undefined &&
        (sshKeys.data.length === 0 ? (
          <Empty>No SSH keys.</Empty>
        ) : (
          <ul className="divide-y divide-neutral-800">
            {sshKeys.data.map((entry) => (
              <li key={entry.id} className="flex items-center justify-between gap-4 py-2.5">
                <div className="min-w-0">
                  <p className="truncate text-sm text-neutral-200">{entry.label}</p>
                  <p className="truncate font-mono text-xs text-neutral-600">
                    {entry.fingerprint} · added {entry.created_at}
                  </p>
                </div>
                <Button variant="danger" disabled={remove.isPending} onClick={() => remove.mutate({ id: entry.id })}>
                  Remove
                </Button>
              </li>
            ))}
          </ul>
        ))}
      {remove.error !== null && (
        <div className="mt-3">
          <ErrorNotice error={remove.error} />
        </div>
      )}

      <form
        className="mt-4 space-y-3 border-t border-neutral-800 pt-4"
        onSubmit={(event) => {
          event.preventDefault();
          add.mutate(
            { label: label.trim(), key: key.trim() },
            {
              onSuccess: () => {
                setLabel("");
                setKey("");
              },
            },
          );
        }}
      >
        <Field label="Label" hint="which machine this key lives on">
          <input
            className={inputClass}
            value={label}
            onChange={(event) => setLabel(event.target.value)}
            placeholder="laptop"
          />
        </Field>
        <Field label="Public key" hint="contents of ~/.ssh/id_ed25519.pub">
          <textarea
            className={`${inputClass} h-24 font-mono text-xs`}
            value={key}
            onChange={(event) => setKey(event.target.value)}
            placeholder="ssh-ed25519 AAAAC3Nz… you@laptop"
          />
        </Field>
        {add.error !== null && <ErrorNotice error={add.error} />}
        <Button
          type="submit"
          variant="primary"
          disabled={add.isPending || label.trim().length === 0 || key.trim().length === 0}
        >
          {add.isPending ? "Adding…" : "Add SSH key"}
        </Button>
      </form>
    </Panel>
  );
};