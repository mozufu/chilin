import { useState } from "react";
import { useTrackerMutation } from "../../api/queries";
import { KINDS, type Kind } from "../../api/types";
import { Button, ErrorNotice, Field, Panel, inputClass } from "../ui";

/**
 * Creation posts to /items rather than a per-kind collection: the server only
 * recognises issues, milestones and pulls as collections, so epic, task and
 * followup items cannot be created anywhere else.
 */
export const CreateItem = ({
  owner,
  name,
  onCreated,
}: {
  owner: string;
  name: string;
  onCreated: (id: string) => void;
}) => {
  const [kind, setKind] = useState<Kind>("issue");
  const [title, setTitle] = useState("");
  const [body, setBody] = useState("");

  const create = useTrackerMutation<{ kind: Kind; title: string; body: string }, { item_id: string }>({
    owner,
    name,
    build: (variables) => ({
      path: `/api/repos/${encodeURIComponent(owner)}/${encodeURIComponent(name)}/items`,
      body:
        variables.body.trim().length > 0
          ? { kind: variables.kind, title: variables.title, body: variables.body }
          : { kind: variables.kind, title: variables.title },
    }),
  });

  return (
    <Panel title="New item">
      <form
        className="space-y-3"
        onSubmit={(event) => {
          event.preventDefault();
          create.mutate(
            { kind, title: title.trim(), body },
            {
              onSuccess: (outcome) => {
                setTitle("");
                setBody("");
                onCreated(outcome.data.item_id);
              },
            },
          );
        }}
      >
        <div className="flex flex-wrap gap-1.5">
          {KINDS.map((entry) => (
            <button
              key={entry}
              type="button"
              onClick={() => setKind(entry)}
              className={`rounded-full px-2.5 py-0.5 text-xs capitalize transition ${
                kind === entry
                  ? "bg-neutral-200 text-neutral-900"
                  : "bg-neutral-800 text-neutral-400 hover:text-neutral-200"
              }`}
            >
              {entry}
            </button>
          ))}
        </div>
        <Field label="Title">
          <input
            className={inputClass}
            value={title}
            onChange={(event) => setTitle(event.target.value)}
            placeholder="Describe the work"
          />
        </Field>
        <Field label="Body" hint="optional, Markdown">
          <textarea
            rows={3}
            value={body}
            onChange={(event) => setBody(event.target.value)}
            className="w-full rounded-md border border-neutral-700 bg-neutral-950 px-3 py-2 text-sm text-neutral-100 outline-none placeholder:text-neutral-600 focus:border-sky-700"
          />
        </Field>
        {create.error !== null && <ErrorNotice error={create.error} />}
        <Button type="submit" variant="primary" disabled={create.isPending || title.trim().length === 0}>
          {create.isPending ? "Creating…" : "Create item"}
        </Button>
      </form>
    </Panel>
  );
};
