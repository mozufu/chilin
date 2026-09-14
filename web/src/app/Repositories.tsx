import { useState } from "react";
import { useMutation, useQueryClient } from "@tanstack/react-query";
import { ApiError, request } from "../api/client";
import { keys, useRepositories } from "../api/queries";
import type { Actor, Repository } from "../api/types";
import { Button, Empty, ErrorNotice, Field, Panel, Spinner, inputClass } from "./ui";

const CloneHints = ({ repo }: { repo: Repository }) => {
  const origin = window.location.origin;
  const http = `${origin}/${repo.owner}/${repo.name}.git`;
  // The tracker is exposed as a second, read-only repository holding the
  // canonical collaboration state.
  const tracker = `${origin}/${repo.owner}/${repo.name}.tracker.git`;
  return (
    <div className="mt-3 space-y-2 border-t border-neutral-800 pt-3">
      {[
        { label: "Clone", value: `git clone ${http}` },
        { label: "Tracker (read-only)", value: `git clone ${tracker}` },
      ].map((entry) => (
        <div key={entry.label}>
          <p className="text-xs text-neutral-500">{entry.label}</p>
          <div className="mt-1 flex items-center gap-2">
            <code className="min-w-0 flex-1 truncate rounded bg-neutral-950 px-2.5 py-1.5 font-mono text-xs text-neutral-300">
              {entry.value}
            </code>
            <Button onClick={() => void navigator.clipboard.writeText(entry.value)}>Copy</Button>
          </div>
        </div>
      ))}
      <p className="text-xs text-neutral-600">
        Authenticate with a token as the HTTP password; the username is ignored.
      </p>
    </div>
  );
};

export const Repositories = ({
  me,
  onOpen,
}: {
  me: Actor;
  onOpen: (repo: Repository) => void;
}) => {
  const client = useQueryClient();
  const repositories = useRepositories();
  const [expanded, setExpanded] = useState<string | null>(null);
  const [owner, setOwner] = useState(me.id);
  const [name, setName] = useState("");
  const [isPublic, setPublic] = useState(false);

  // Repository creation is not a tracker mutation: it has no base revision,
  // and the server answers with the initial revision of a fresh tracker.
  const create = useMutation<Repository, ApiError, { owner: string; name: string; public: boolean }>({
    mutationFn: async (variables) => {
      const { value } = await request<{ repository: Repository; revision: string }>("/api/repos", {
        method: "POST",
        body: variables,
      });
      return value.repository;
    },
    onSuccess: () => {
      setName("");
      void client.invalidateQueries({ queryKey: keys.repos });
    },
  });

  return (
    <div className="space-y-4">
      <Panel title="Repositories" description="Repositories you own, collaborate on, or that are public">
        {repositories.isPending && <Spinner label="Loading repositories" />}
        {repositories.error !== null && <ErrorNotice error={repositories.error} />}
        {repositories.data !== undefined &&
          (repositories.data.length === 0 ? (
            <Empty>No repositories yet.</Empty>
          ) : (
            <ul className="divide-y divide-neutral-800">
              {repositories.data.map((repo) => (
                <li key={repo.id} className="py-3">
                  <div className="flex items-center justify-between gap-4">
                    <button
                      type="button"
                      onClick={() => onOpen(repo)}
                      className="min-w-0 flex-1 text-left"
                    >
                      <p className="truncate text-sm text-neutral-200 hover:text-white">
                        <span className="text-neutral-500">{repo.owner}/</span>
                        {repo.name}
                      </p>
                      <p className="text-xs text-neutral-600">{repo.public ? "public" : "private"}</p>
                    </button>
                    <Button onClick={() => setExpanded(expanded === repo.id ? null : repo.id)}>
                      {expanded === repo.id ? "Hide" : "Clone"}
                    </Button>
                  </div>
                  {expanded === repo.id && <CloneHints repo={repo} />}
                </li>
              ))}
            </ul>
          ))}
      </Panel>

      <Panel title="Create a repository">
        <form
          className="space-y-3"
          onSubmit={(event) => {
            event.preventDefault();
            create.mutate({ owner: owner.trim(), name: name.trim(), public: isPublic });
          }}
        >
          <div className="grid grid-cols-2 gap-3">
            <Field label="Owner">
              <input className={inputClass} value={owner} onChange={(e) => setOwner(e.target.value)} />
            </Field>
            <Field label="Name">
              <input
                className={inputClass}
                value={name}
                onChange={(e) => setName(e.target.value)}
                placeholder="project"
              />
            </Field>
          </div>
          <label className="flex items-center gap-2 text-xs text-neutral-300">
            <input
              type="checkbox"
              checked={isPublic}
              onChange={(e) => setPublic(e.target.checked)}
              className="size-3.5 accent-sky-600"
            />
            Public — readable without authentication
          </label>
          {create.error !== null && <ErrorNotice error={create.error} />}
          <Button
            type="submit"
            variant="primary"
            disabled={create.isPending || name.trim().length === 0 || owner.trim().length === 0}
          >
            {create.isPending ? "Creating…" : "Create repository"}
          </Button>
        </form>
      </Panel>
    </div>
  );
};
