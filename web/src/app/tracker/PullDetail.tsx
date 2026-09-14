import { useState } from "react";
import {
  useMergePull,
  usePullDiff,
  useTrackerMutation,
  type History,
} from "../../api/queries";
import type { Item, Pull, PullReview } from "../../api/types";
import { Button, Empty, ErrorNotice, Panel, Spinner } from "../ui";
import { DiffView } from "./DiffView";

const VERDICTS = [
  { key: "approve", label: "Approve", tone: "border-emerald-800 bg-emerald-950 text-emerald-200" },
  { key: "request_changes", label: "Request changes", tone: "border-red-900 bg-red-950 text-red-200" },
  { key: "comment", label: "Comment", tone: "border-neutral-700 bg-neutral-800 text-neutral-200" },
] as const;

const VERDICT_TONE: Record<string, string> = {
  approve: "text-emerald-400",
  request_changes: "text-red-400",
  comment: "text-neutral-400",
};

const Reviews = ({ pull, reviews }: { pull: Pull; reviews: PullReview[] }) => (
  <Panel title="Reviews" description="A verdict binds to the exact head it was given at">
    {reviews.length === 0 ? (
      <Empty>No reviews yet.</Empty>
    ) : (
      <ul className="space-y-2">
        {reviews.map((review) => {
          // A verdict at an older head no longer counts toward merge
          // eligibility (Chilin.Pulls.mergePull), so it is marked as such
          // rather than silently appearing to approve the current changes.
          const current = review.head_oid === pull.head_oid;
          return (
            <li
              key={review.operation_id}
              className="rounded-lg border border-neutral-800 bg-neutral-950/60 px-3 py-2"
            >
              <p className="text-xs">
                <span className="text-neutral-300">{review.actor}</span>{" "}
                <span className={VERDICT_TONE[review.verdict] ?? "text-neutral-400"}>
                  {review.verdict.replace("_", " ")}
                </span>
                <span className="text-neutral-600"> · {review.created_at}</span>
                {!current && <span className="ml-2 text-amber-600">stale head</span>}
              </p>
              {review.body.trim().length > 0 && (
                <p className="mt-1 whitespace-pre-wrap text-sm text-neutral-300">{review.body}</p>
              )}
            </li>
          );
        })}
      </ul>
    )}
  </Panel>
);

const ReviewForm = ({
  owner,
  name,
  id,
  headOid,
}: {
  owner: string;
  name: string;
  id: string;
  headOid: string;
}) => {
  const [body, setBody] = useState("");
  const review = useTrackerMutation<{ verdict: string; body: string }, unknown>({
    owner,
    name,
    build: (variables) => ({
      path: `/api/repos/${encodeURIComponent(owner)}/${encodeURIComponent(name)}/pulls/${encodeURIComponent(id)}/reviews`,
      // The head is submitted explicitly: the server rejects the review if the
      // source branch moved, rather than recording a verdict on unseen code.
      body: { head_oid: headOid, verdict: variables.verdict, body: variables.body },
    }),
  });

  return (
    <Panel title="Submit a review">
      <textarea
        rows={3}
        value={body}
        onChange={(event) => setBody(event.target.value)}
        placeholder="Optional comment"
        className="w-full rounded-md border border-neutral-700 bg-neutral-950 px-3 py-2 text-sm text-neutral-100 outline-none placeholder:text-neutral-600 focus:border-sky-700"
      />
      <div className="mt-3 flex flex-wrap gap-2">
        {VERDICTS.map((entry) => (
          <button
            key={entry.key}
            type="button"
            disabled={review.isPending}
            onClick={() => review.mutate({ verdict: entry.key, body }, { onSuccess: () => setBody("") })}
            className={`rounded-md border px-3 py-1.5 text-xs font-medium transition disabled:opacity-40 ${entry.tone}`}
          >
            {entry.label}
          </button>
        ))}
      </div>
      {review.error !== null && (
        <div className="mt-3">
          <ErrorNotice error={review.error} />
        </div>
      )}
    </Panel>
  );
};

const MergeControl = ({
  owner,
  name,
  id,
  pull,
}: {
  owner: string;
  name: string;
  id: string;
  pull: Pull;
}) => {
  const [finalising, setFinalising] = useState(false);
  const merge = useMergePull(owner, name, id);

  const approvals = pull.reviews.filter(
    (review) =>
      review.head_oid === pull.head_oid && review.verdict === "approve" && review.actor !== pull.author,
  );
  const blocked = pull.reviews.some(
    (review) => review.head_oid === pull.head_oid && review.verdict === "request_changes",
  );

  // These mirror the server's preconditions so the reason is visible before
  // the attempt; the server still enforces them.
  const reason = pull.draft
    ? "Draft pulls cannot be merged."
    : blocked
      ? "An outstanding changes request at this head blocks merging."
      : approvals.length === 0
        ? "A non-author approval at the current head is required."
        : null;

  return (
    <Panel title="Merge" description={`${pull.source_ref} → ${pull.target_ref}`}>
      {reason !== null && <p className="mb-3 text-xs text-amber-500">{reason}</p>}
      <Button
        variant="primary"
        disabled={merge.isPending || reason !== null}
        onClick={() =>
          merge.mutate(
            {
              head_oid: pull.head_oid,
              target_oid: pull.target_oid,
              onPending: () => setFinalising(true),
            },
            { onSettled: () => setFinalising(false) },
          )
        }
      >
        {finalising ? "Finalising…" : merge.isPending ? "Merging…" : "Merge pull request"}
      </Button>
      {finalising && (
        <p className="mt-2 text-xs text-neutral-500">
          The merge was prepared and is being finalised; waiting for the recorded outcome.
        </p>
      )}
      {merge.error !== null && (
        <div className="mt-3">
          <ErrorNotice error={merge.error} />
        </div>
      )}
    </Panel>
  );
};

const Changes = ({ owner, name, id }: { owner: string; name: string; id: string }) => {
  const diff = usePullDiff(owner, name, id);
  return (
    <Panel title="Changes" description="Computed between the recorded base and head commits">
      {diff.isPending && <Spinner label="Loading diff" />}
      {diff.error !== null && <ErrorNotice error={diff.error} />}
      {diff.data !== undefined && <DiffView patch={diff.data.diff} />}
    </Panel>
  );
};

export const PullDetail = ({
  owner,
  name,
  id,
  history,
  item,
}: {
  owner: string;
  name: string;
  id: string;
  history: History;
  item: Item;
}) => {
  const pull = item.pull;
  if (pull === undefined) return null;

  const readOnly = history.at !== "now";
  const open = pull.status === "open";

  return (
    <div className="space-y-4">
      <Panel
        title={item.title}
        description={`${pull.status}${pull.draft ? " · draft" : ""} · ${pull.source_owner}/${pull.source_name}:${pull.source_ref} → ${pull.target_ref}`}
      >
        <dl className="grid grid-cols-2 gap-x-6 gap-y-1.5 text-xs">
          <dt className="text-neutral-600">Author</dt>
          <dd className="text-neutral-300">{pull.author}</dd>
          <dt className="text-neutral-600">Head</dt>
          <dd className="truncate font-mono text-neutral-300">{pull.head_oid.slice(0, 12)}</dd>
          <dt className="text-neutral-600">Base</dt>
          <dd className="truncate font-mono text-neutral-300">{pull.target_oid.slice(0, 12)}</dd>
          {pull.merge_oid !== null && (
            <>
              <dt className="text-neutral-600">Merge</dt>
              <dd className="truncate font-mono text-emerald-400">{pull.merge_oid.slice(0, 12)}</dd>
            </>
          )}
        </dl>
      </Panel>

      <Changes owner={owner} name={name} id={id} />
      <Reviews pull={pull} reviews={pull.reviews} />

      {!readOnly && open && (
        <>
          <ReviewForm owner={owner} name={name} id={id} headOid={pull.head_oid} />
          <MergeControl owner={owner} name={name} id={id} pull={pull} />
        </>
      )}
    </div>
  );
};
