import { useState } from "react";
import { useItems, type History, type ItemFilters } from "../../api/queries";
import { KINDS, STATES, type Item, type Kind, type State } from "../../api/types";
import { Button, Empty, ErrorNotice, Spinner } from "../ui";

const KIND_TONE: Record<Kind, string> = {
  task: "bg-neutral-800 text-neutral-300",
  issue: "bg-sky-950 text-sky-300",
  bug: "bg-red-950 text-red-300",
  milestone: "bg-violet-950 text-violet-300",
  epic: "bg-amber-950 text-amber-300",
  followup: "bg-teal-950 text-teal-300",
};

const STATE_TONE: Record<State, string> = {
  open: "text-emerald-400",
  active: "text-sky-400",
  blocked: "text-red-400",
  deferred: "text-amber-400",
  done: "text-neutral-500",
  cancelled: "text-neutral-600",
};

const Chip = ({
  label,
  active,
  onClick,
}: {
  label: string;
  active: boolean;
  onClick: () => void;
}) => (
  <button
    type="button"
    onClick={onClick}
    className={`rounded-full px-2.5 py-0.5 text-xs capitalize transition ${
      active ? "bg-neutral-200 text-neutral-900" : "bg-neutral-800 text-neutral-400 hover:text-neutral-200"
    }`}
  >
    {label}
  </button>
);

export const ItemList = ({
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
  const [filters, setFilters] = useState<ItemFilters>({});
  const query = useItems(owner, name, filters, history);
  const items = query.data?.pages.flatMap((page) => page.data.items) ?? [];

  const toggle = <K extends keyof ItemFilters>(field: K, value: ItemFilters[K]) =>
    setFilters((previous) => {
      const next = { ...previous };
      if (previous[field] === value) delete next[field];
      else next[field] = value;
      return next;
    });

  return (
    <div className="space-y-4">
      <div className="space-y-2">
        <div className="flex flex-wrap items-center gap-1.5">
          <span className="mr-1 text-xs text-neutral-600">kind</span>
          {KINDS.map((kind) => (
            <Chip
              key={kind}
              label={kind}
              active={filters.kind === kind}
              onClick={() => toggle("kind", kind)}
            />
          ))}
        </div>
        <div className="flex flex-wrap items-center gap-1.5">
          <span className="mr-1 text-xs text-neutral-600">state</span>
          {STATES.map((state) => (
            <Chip
              key={state}
              label={state}
              active={filters.state === state}
              onClick={() => toggle("state", state)}
            />
          ))}
        </div>
      </div>

      {query.isPending && <Spinner label="Loading items" />}
      {query.error !== null && <ErrorNotice error={query.error} />}

      {query.data !== undefined &&
        (items.length === 0 ? (
          <Empty>No items match these filters.</Empty>
        ) : (
          <ul className="divide-y divide-neutral-800 rounded-xl border border-neutral-800 bg-neutral-900/50">
            {items.map((item) => (
              <ItemRow key={item.id} item={item} onOpen={onOpen} />
            ))}
          </ul>
        ))}

      {query.hasNextPage && (
        <Button onClick={() => void query.fetchNextPage()} disabled={query.isFetchingNextPage}>
          {query.isFetchingNextPage ? "Loading…" : "Load more"}
        </Button>
      )}
    </div>
  );
};

const ItemRow = ({ item, onOpen }: { item: Item; onOpen: (id: string) => void }) => (
  <li>
    <button
      type="button"
      onClick={() => onOpen(item.id)}
      className="flex w-full items-center gap-3 px-4 py-3 text-left transition hover:bg-neutral-800/40"
    >
      <span className={`rounded px-1.5 py-0.5 text-xs ${KIND_TONE[item.kind]}`}>{item.kind}</span>
      <span className="min-w-0 flex-1 truncate text-sm text-neutral-200">{item.title}</span>
      {/* A pull request is a task item carrying a pull record, so the badge
          comes from that record rather than from the kind. */}
      {item.pull !== undefined && (
        <span className="rounded bg-indigo-950 px-1.5 py-0.5 text-xs text-indigo-300">pull</span>
      )}
      {item.metadata?.milestone != null && (
        <span className="text-xs text-violet-400">milestone</span>
      )}
      <span className={`text-xs ${STATE_TONE[item.state]}`}>{item.state}</span>
    </button>
  </li>
);
