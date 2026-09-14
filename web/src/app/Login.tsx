import { useState } from "react";
import { useQueryClient } from "@tanstack/react-query";
import { request, setBearer } from "../api/client";
import { keys } from "../api/queries";
import type { Me } from "../api/types";
import { Button, ErrorNotice, Field, inputClass } from "./ui";

/**
 * Token sign-in for deployments without a forward-auth proxy, and for
 * administrators bootstrapping one. When a proxy is in front, the session is
 * already established and /api/me succeeds without ever reaching this view.
 */
export const Login = ({ proxyRejected }: { proxyRejected: boolean }) => {
  const client = useQueryClient();
  const [token, setToken] = useState("");
  const [error, setError] = useState<unknown>(null);
  const [busy, setBusy] = useState(false);

  const submit = async () => {
    setBusy(true);
    setError(null);
    setBearer(token.trim());
    try {
      // Validate before persisting the credential in the query cache, so a bad
      // token never leaves the app in a half-authenticated state.
      const me = await request<Me>("/api/me");
      client.setQueryData(keys.me, me.value);
    } catch (failure) {
      setBearer(null);
      setError(failure);
    } finally {
      setBusy(false);
    }
  };

  return (
    <div className="mx-auto flex min-h-full max-w-md flex-col justify-center gap-6 px-6">
      <div>
        <h1 className="text-xl font-semibold text-neutral-100">Chilin</h1>
        <p className="mt-1 text-sm text-neutral-500">Git hosting with a myque collaboration store</p>
      </div>

      {proxyRejected && (
        <div className="rounded-lg border border-amber-900/60 bg-amber-950/30 px-4 py-3 text-sm text-amber-200/90">
          Your proxy identity is not linked to a Chilin account. An administrator must link it before
          you can sign in that way.
        </div>
      )}

      <form
        className="space-y-4"
        onSubmit={(event) => {
          event.preventDefault();
          void submit();
        }}
      >
        <Field label="Access token" hint="64 hex characters">
          <input
            className={inputClass}
            type="password"
            autoComplete="current-password"
            value={token}
            onChange={(event) => setToken(event.target.value)}
            placeholder="Paste a token issued by Chilin"
          />
        </Field>
        {error !== null && <ErrorNotice error={error} />}
        <Button type="submit" variant="primary" disabled={busy || token.trim().length === 0}>
          {busy ? "Verifying…" : "Sign in"}
        </Button>
      </form>

      <p className="text-xs text-neutral-600">
        The token is held in memory for this tab only. It is never written to storage, so a page
        reload signs you out.
      </p>
    </div>
  );
};
