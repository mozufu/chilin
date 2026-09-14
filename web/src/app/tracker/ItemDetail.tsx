import { useState } from "react";
import { useItem, useTimeline, useTrackerMutation, type History } from "../../api/queries";
import { STATES, type Item, type State } from "../../api/types";
import { Button, ErrorNotice, Panel, Spinner } from "../ui";
import { RelationGraph } from "./RelationGraph";

const Discussion = ({
  owner,
  name,
  id,
  history,
  readOnly,
}: {
  owner: string;
  name: string;
  id: string;
  history: History;
  readOnly: boolean;
}) => {
  const timeline = useTimeline(owner, name, id, history);
  const [body, setBody] = useState("");

  const comment = useTrackerMutation<{ body: string }, unknown>({
    owner,
    name,
    build: (variables) => ({
      path: `/api/repos/${encodeURIComponent(owner)}/${encodeURIComponent(name)}/items/${encodeURIComponent(id)}/comments`,
      body: { body: variables.body },
    }),
  });

  return (
    <Panel title="Discussion" description="Comments and state changes, in recorded order">
      {timeline.isPending && <Spinner label="Loading timeline" />}
      {timeline.error !== null && <ErrorNotice error={timeline.error} />}

      <ol className="space-y-3">
        {(timeline.data ?? []).map((event) => {
          // Comments are only recoverable from their events; chilin exposes no
          // comment collection to read.
          const posted = event.data.comment;
          if (posted !== undefined) {
            return (
              <li key={event.event_id} className="rounded-lg border border-neutral-800 bg-neutral-950/60 px-3 py-2">
                <p className="text-xs text-neutral-500">
                  <span className="text-neutral-300">{posted.author}</span> · {posted.created_at}
                </p>
                <p className="mt-1 whitespace-pre-wrap text-sm text-neutral-200">{posted.body}</p>
              </li>
            );
          }
          const from = event.data.previous_state;
          const to = event.data.state;
          return (
            <li key={event.event_id} className="px-1 text-xs text-neutral-500">
              <span className="text-neutral-400">{event.actor_id}</span>{" "}
              {from != null && to !== undefined && from !== to ? (
                <>
                  moved <span className="text-neutral-300">{from}</span> →{" "}
                  <span className="text-neutral-300">{to}</span>
                </>
              ) : (
                <span>{event.type.replace(/\./g, " ")}</span>
              )}{" "}
              · {event.recorded_at}
            </li>
          );
        })}
      </ol>

      {!readOnly && (
        <form
          className="mt-4 space-y-2"
          onSubmit={(event) => {
            event.preventDefault();
            comment.mutate({ body }, { onSuccess: () => setBody("") });
          }}
        >
          <textarea
            rows={3}
            value={body}
            onChange={(event) => setBody(event.target.value)}
            placeholder="Leave a comment"
            className="w-full rounded-md border border-neutral-700 bg-neutral-950 px-3 py-2 text-sm text-neutral-100 outline-none placeholder:text-neutral-600 focus:border-sky-700"
          />
          {comment.error !== null && <ErrorNotice error={comment.error} />}
          <Button type="submit" variant="primary" disabled={comment.isPending || body.trim().length === 0}>
            {comment.isPending ? "Posting…" : "Comment"}
          </Button>
        </form>
      )}
    </Panel>
  );
};

const StateControl = ({
  owner,
  name,
  item,
}: {
  owner: string;
  name: string;
  item: Item;
}) => {
  const update = useTrackerMutation<{ state: State }, unknown>({
    owner,
    name,
    build: (variables) => ({
      path: `/api/repos/${encodeURIComponent(owner)}/${encodeURIComponent(name)}/items/${encodeURIComponent(item.id)}`,
      method: "PATCH",
      body: { state: variables.state },
    }),
  });

  // A pull's canonical task may not be completed directly; only the merge
  // transaction may produce `done` (Chilin.Items.guardPull).
  const mergeOwned = item.pull !== undefined;

  return (
    <div className="space-y-2">
      <div className="flex flex-wrap gap-1.5">
        {STATES.map((state) => (
          <button
            key={state}
            type="button"
            disabled={update.isPending || state === item.state || (mergeOwned && state === "done")}
            onClick={() => update.mutate({ state })}
            className={`rounded-full px-2.5 py-0.5 text-xs capitalize transition disabled:opacity-30 ${
              state === item.state
                ? "bg-neutral-200 text-neutral-900"
                : "bg-neutral-800 text-neutral-400 hover:text-neutral-200"
            }`}
          >
            {state}
          </button>
        ))}
      </div>
      {mergeOwned && (
        <p className="text-xs text-neutral-600">
          This item backs a pull request; only merging can complete it.
        </p>
      )}
      {update.error !== null && <ErrorNotice error={update.error} />}
    </div>
  );
};

export const ItemDetail = ({
  owner,
  name,
  id,
  history,
  onBack,
  onOpen,
}: {
  owner: string;
  name: string;
  id: string;
  history: History;
  onBack: () => void;
  onOpen: (id: string) => void;
}) => {
  const item = useItem(owner, name, id, history);
  const readOnly = history.at !== "now";

  if (item.isPending) return <Spinner label="Loading item" />;
  if (item.error !== null) return <ErrorNotice error={item.error} />;
  if (item.data === undefined) return null;

  const value = item.data;

  return (
    <div className="space-y-4">
      <button type="button" onClick={onBack} className="text-xs text-neutral-500 hover:text-neutral-300">
        ← Back to items
      </button>

      <Panel
        title={value.title}
        description={`${value.kind} · ${value.state}${value.key != null ? ` · ${value.key}` : ""}`}
      >
        {value.body.trim().length > 0 && (
          <pre className="mb-4 whitespace-pre-wrap font-sans text-sm text-neutral-300">{value.body.trim()}</pre>
        )}
        <dl className="grid grid-cols-2 gap-x-6 gap-y-1.5 text-xs">
          <Row label="Author" value={value.metadata?.author ?? "—"} />
          <Row label="Created" value={value.created} />
          <Row label="Updated" value={value.updated ?? "—"} />
          <Row label="Closed" value={value.closed ?? "—"} />
          <Row
            label="Assignees"
            value={
              value.metadata?.assignees !== undefined && value.metadata.assignees.length > 0
                ? value.metadata.assignees.join(", ")
                : "—"
            }
          />
          <Row label="Tags" value={value.tags.length > 0 ? value.tags.join(", ") : "—"} />
        </dl>
        {!readOnly && (
          <div className="mt-4 border-t border-neutral-800 pt-4">
            <StateControl owner={owner} name={name} item={value} />
          </div>
        )}
      </Panel>

      <RelationGraph owner={owner} name={name} history={history} item={value} onOpen={onOpen} />
      <Discussion owner={owner} name={name} id={id} history={history} readOnly={readOnly} />
    </div>
  );
};

const Row = ({ label, value }: { label: string; value: string }) => (
  <>
    <dt className="text-neutral-600">{label}</dt>
    <dd className="truncate text-neutral-300">{value}</dd>
  </>
);
