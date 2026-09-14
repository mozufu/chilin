import { useItems } from "../../api/queries";
import type { Item } from "../../api/types";
import { Empty, Panel } from "../ui";

/**
 * The six relation edges myque records on an item. Only `parent` and `depends`
 * are writable through PATCH (Chilin.Items.updateItem); the others are set by
 * an import and are therefore presented read-only rather than as controls that
 * would be rejected.
 */
const EDGES = [
  { key: "parent", label: "Parent", tone: "text-violet-300", writable: true },
  { key: "depends", label: "Depends on", tone: "text-amber-300", writable: true },
  { key: "blocks", label: "Blocks", tone: "text-red-300", writable: false },
  { key: "related", label: "Related", tone: "text-sky-300", writable: false },
  { key: "duplicate_of", label: "Duplicate of", tone: "text-neutral-400", writable: false },
  { key: "supersedes", label: "Supersedes", tone: "text-teal-300", writable: false },
] as const;

const targets = (item: Item, key: (typeof EDGES)[number]["key"]): string[] => {
  const value = item[key];
  if (value === null || value === undefined) return [];
  return Array.isArray(value) ? value : [value];
};

export const RelationGraph = ({
  owner,
  name,
  item,
  onOpen,
}: {
  owner: string;
  name: string;
  item: Item;
  onOpen: (id: string) => void;
}) => {
  // UUIDv7 is time-ordered, so items created in the same second share a long
  // prefix; a truncated id identifies nothing. Titles come from the item list
  // already in cache, and the id remains as the tooltip.
  const listed = useItems(owner, name, {});
  const titles: Record<string, string> = Object.fromEntries(
    (listed.data?.pages ?? [])
      .flatMap((page) => page.data.items)
      .map((entry) => [entry.id, entry.title]),
  );

  const present = EDGES.map((edge) => ({ edge, ids: targets(item, edge.key) })).filter(
    (entry) => entry.ids.length > 0,
  );

  return (
    <Panel
      title="Relations"
      description="How this item connects to the rest of the tracker"
    >
      {present.length === 0 ? (
        <Empty>No relations recorded.</Empty>
      ) : (
        <div className="space-y-3">
          {present.map(({ edge, ids }) => (
            <div key={edge.key} className="flex gap-3">
              <span className={`w-28 shrink-0 text-xs ${edge.tone}`}>
                {edge.label}
                {!edge.writable && <span className="ml-1 text-neutral-700">·ro</span>}
              </span>
              <div className="flex min-w-0 flex-wrap gap-1.5">
                {ids.map((id) => (
                  <button
                    key={id}
                    type="button"
                    title={id}
                    onClick={() => onOpen(id)}
                    className="max-w-64 truncate rounded border border-neutral-800 bg-neutral-950 px-2 py-0.5 text-xs text-neutral-400 transition hover:border-neutral-600 hover:text-neutral-200"
                  >
                    {titles[id] ?? `${id.slice(0, 8)}…`}
                  </button>
                ))}
              </div>
            </div>
          ))}
        </div>
      )}
      <p className="mt-3 text-xs text-neutral-700">
        Edges marked ·ro arrive through imports; the API accepts only parent and depends on update.
      </p>
    </Panel>
  );
};
