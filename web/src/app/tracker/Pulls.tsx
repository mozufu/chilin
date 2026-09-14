import { useItems, type History } from "../../api/queries";
import type { Item } from "../../api/types";
import { Button, Empty, ErrorNotice, Panel, Spinner } from "../ui";

const STATUS_TONE: Record<string, string> = {
  open: "text-emerald-400",
  merged: "text-violet-400",
  closed: "text-neutral-500",
};

export const Pulls = ({
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
  // Pull requests are task items carrying a pull record, so they are filtered
  // by that record rather than by kind; /items?kind=task would also return
  // ordinary tasks.
  const query = useItems(owner, name, {}, history);
  const pulls = (query.data?.pages.flatMap((page) => page.data.items) ?? []).filter(
    (item: Item) => item.pull !== undefined,
  );

  return (
    <div className="space-y-3">
      <Panel title="Pull requests" description="Branch merges reviewed against a recorded head">
        {query.isPending && <Spinner label="Loading pull requests" />}
        {query.error !== null && <ErrorNotice error={query.error} />}
        {query.data !== undefined &&
          (pulls.length === 0 ? (
            <Empty>No pull requests.</Empty>
          ) : (
            <ul className="divide-y divide-neutral-800">
              {pulls.map((item) => {
                const pull = item.pull;
                if (pull === undefined) return null;
                return (
                  <li key={item.id}>
                    <button
                      type="button"
                      onClick={() => onOpen(item.id)}
                      className="flex w-full items-center gap-3 py-3 text-left transition hover:bg-neutral-800/30"
                    >
                      <span className="min-w-0 flex-1">
                        <span className="block truncate text-sm text-neutral-200">{item.title}</span>
                        <span className="block truncate font-mono text-xs text-neutral-600">
                          {pull.source_ref} → {pull.target_ref}
                        </span>
                      </span>
                      {pull.draft && (
                        <span className="rounded bg-neutral-800 px-1.5 py-0.5 text-xs text-neutral-400">
                          draft
                        </span>
                      )}
                      {pull.reviews.length > 0 && (
                        <span className="text-xs text-neutral-600">
                          {pull.reviews.length} review{pull.reviews.length === 1 ? "" : "s"}
                        </span>
                      )}
                      <span className={`text-xs ${STATUS_TONE[pull.status] ?? "text-neutral-400"}`}>
                        {pull.status}
                      </span>
                    </button>
                  </li>
                );
              })}
            </ul>
          ))}
      </Panel>
      {query.hasNextPage && (
        <Button onClick={() => void query.fetchNextPage()} disabled={query.isFetchingNextPage}>
          {query.isFetchingNextPage ? "Loading…" : "Load more"}
        </Button>
      )}
    </div>
  );
};
