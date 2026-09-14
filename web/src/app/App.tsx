import { useState } from "react";
import { useQueryClient } from "@tanstack/react-query";
import { ApiError, setBearer } from "../api/client";
import { useMe } from "../api/queries";
import type { Repository } from "../api/types";
import { Login } from "./Login";
import { Repositories } from "./Repositories";
import { Tokens } from "./Tokens";
import { Workspace } from "./tracker/Workspace";
import { ErrorNotice, Spinner } from "./ui";

type Tab = "repositories" | "tokens";

export const App = () => {
  const client = useQueryClient();
  const me = useMe();
  const [tab, setTab] = useState<Tab>("repositories");
  const [active, setActive] = useState<Repository | null>(null);

  if (me.isPending) {
    return (
      <div className="flex h-full items-center justify-center">
        <Spinner label="Signing in" />
      </div>
    );
  }

  // 401 means no credential at all; 403 means the proxy asserted an identity
  // that no administrator has linked yet. Both land on the sign-in view, but
  // only the second needs an explanation.
  if (me.error instanceof ApiError && (me.error.isUnauthorized || me.error.isForbidden)) {
    return <Login proxyRejected={me.error.isForbidden} />;
  }

  if (me.error !== null || me.data === undefined) {
    return (
      <div className="mx-auto max-w-lg px-6 py-16">
        <ErrorNotice error={me.error ?? new Error("no session")} />
      </div>
    );
  }

  const identity = me.data.identities[0];

  return (
    <div className="mx-auto flex h-full max-w-4xl flex-col px-6">
      <header className="flex items-center justify-between border-b border-neutral-800 py-4">
        <div className="flex items-baseline gap-3">
          <h1 className="text-sm font-semibold text-neutral-100">Chilin</h1>
          <nav className="flex gap-1">
            {(["repositories", "tokens"] as const).map((entry) => (
              <button
                key={entry}
                type="button"
                onClick={() => {
                  setTab(entry);
                  setActive(null);
                }}
                className={`rounded-md px-2.5 py-1 text-xs capitalize transition ${
                  tab === entry
                    ? "bg-neutral-800 text-neutral-100"
                    : "text-neutral-500 hover:text-neutral-300"
                }`}
              >
                {entry}
              </button>
            ))}
          </nav>
        </div>
        <div className="flex items-center gap-3 text-xs">
          <span className="text-neutral-400">
            {me.data.user.id}
            {me.data.user.admin && <span className="ml-1.5 text-amber-500">admin</span>}
            {identity !== undefined && (
              <span className="ml-1.5 text-neutral-600">
                via {identity.provider}:{identity.subject}
              </span>
            )}
          </span>
          <button
            type="button"
            className="text-neutral-500 hover:text-neutral-300"
            onClick={() => {
              setBearer(null);
              client.clear();
            }}
          >
            Sign out
          </button>
        </div>
      </header>

      <main className="flex-1 overflow-y-auto py-6">
        {tab === "tokens" ? (
          <Tokens />
        ) : active !== null ? (
          <Workspace owner={active.owner} name={active.name} onClose={() => setActive(null)} />
        ) : (
          <Repositories me={me.data.user} onOpen={setActive} />
        )}
      </main>
    </div>
  );
};
