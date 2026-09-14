import { useItems, useMilestoneProgress, type History } from "../../api/queries";
import type { Item } from "../../api/types";
import { Empty, ErrorNotice, Panel, Spinner } from "../ui";

const ProgressBar = ({
  owner,
  name,
  id,
  history,
}: {
  owner: string;
  name: string;
  id: string;
  history: History;
}) => {
  const progress = useMilestoneProgress(owner, name, id, history);

  if (progress.isPending) return <div className="h-1.5 animate-pulse rounded-full bg-neutral-800" />;
  if (progress.error !== null) return <ErrorNotice error={progress.error} />;
  if (progress.data === undefined) return null;

  const { total, done, cancelled, remaining } = progress.data;
  // A milestone with no members has no meaningful ratio; showing 0% would
  // imply work that does not exist.
  const pct = total === 0 ? 0 : Math.round((done / total) * 100);

  return (
    <div className="space-y-1.5">
      <div className="flex h-1.5 overflow-hidden rounded-full bg-neutral-800">
        <div className="bg-emerald-600" style={{ width: `${total === 0 ? 0 : (done / total) * 100}%` }} />
        <div className="bg-neutral-600" style={{ width: `${total === 0 ? 0 : (cancelled / total) * 100}%` }} />
      </div>
      <p className="text-xs text-neutral-500">
        {total === 0 ? (
          "No items assigned"
        ) : (
          <>
            <span className="text-emerald-400">{done} done</span>
            {cancelled > 0 && <span className="text-neutral-500"> · {cancelled} cancelled</span>}
            <span className="text-neutral-400"> · {remaining} remaining</span>
            <span className="text-neutral-600"> · {pct}%</span>
          </>
        )}
      </p>
    </div>
  );
};

export const Milestones = ({
  owner,
  name,
  history,
  onOpen,
}: {
  owner: string;
  name: string;
  history: History;
  onOpen: (id: string) => void;
}) => {
  const query = useItems(owner, name, { kind: "milestone" }, history);
  const milestones = query.data?.pages.flatMap((page) => page.data.items) ?? [];

  return (
    <Panel title="Milestones" description="Completion counted from member item states">
      {query.isPending && <Spinner label="Loading milestones" />}
      {query.error !== null && <ErrorNotice error={query.error} />}
      {query.data !== undefined &&
        (milestones.length === 0 ? (
          <Empty>No milestones yet.</Empty>
        ) : (
          <ul className="space-y-4">
            {milestones.map((milestone: Item) => (
              <li key={milestone.id} className="space-y-2">
                <div className="flex items-baseline justify-between gap-4">
                  <button
                    type="button"
                    onClick={() => onOpen(milestone.id)}
                    className="min-w-0 truncate text-sm text-neutral-200 hover:text-white"
                  >
                    {milestone.title}
                  </button>
                  <span className="shrink-0 text-xs text-neutral-600">
                    {milestone.milestone?.due_at ?? "no due date"}
                  </span>
                </div>
                <ProgressBar owner={owner} name={name} id={milestone.id} history={history} />
              </li>
            ))}
          </ul>
        ))}
    </Panel>
  );
};
