import { useState } from "react";
import { useRepository } from "../../api/queries";
import { ErrorNotice, Spinner } from "../ui";
import { CreateItem } from "./CreateItem";
import { ItemDetail } from "./ItemDetail";
import { ItemList } from "./ItemList";
import { Milestones } from "./Milestones";

type Section = "items" | "milestones" | "new";

export const Workspace = ({
  owner,
  name,
  onClose,
}: {
  owner: string;
  name: string;
  onClose: () => void;
}) => {
  const repository = useRepository(owner, name);
  const [section, setSection] = useState<Section>("items");
  const [openItem, setOpenItem] = useState<string | null>(null);

  if (repository.isPending) return <Spinner label="Loading repository" />;
  if (repository.error !== null) return <ErrorNotice error={repository.error} />;

  return (
    <div className="space-y-4">
      <div className="flex items-center justify-between gap-4">
        <div className="flex items-baseline gap-3">
          <button type="button" onClick={onClose} className="text-xs text-neutral-500 hover:text-neutral-300">
            ← Repositories
          </button>
          <h2 className="text-sm text-neutral-200">
            <span className="text-neutral-500">{owner}/</span>
            {name}
          </h2>
        </div>
        <span className="font-mono text-xs text-neutral-700">
          {repository.data?.revision.slice(0, 8)}
        </span>
      </div>

      {openItem === null ? (
        <>
          <nav className="flex gap-1">
            {(["items", "milestones", "new"] as const).map((entry) => (
              <button
                key={entry}
                type="button"
                onClick={() => setSection(entry)}
                className={`rounded-md px-2.5 py-1 text-xs capitalize transition ${
                  section === entry
                    ? "bg-neutral-800 text-neutral-100"
                    : "text-neutral-500 hover:text-neutral-300"
                }`}
              >
                {entry === "new" ? "new item" : entry}
              </button>
            ))}
          </nav>

          {section === "items" && <ItemList owner={owner} name={name} onOpen={setOpenItem} />}
          {section === "milestones" && <Milestones owner={owner} name={name} onOpen={setOpenItem} />}
          {section === "new" && (
            <CreateItem
              owner={owner}
              name={name}
              onCreated={(id) => {
                setSection("items");
                setOpenItem(id);
              }}
            />
          )}
        </>
      ) : (
        <ItemDetail
          owner={owner}
          name={name}
          id={openItem}
          onBack={() => setOpenItem(null)}
          onOpen={setOpenItem}
        />
      )}
    </div>
  );
};
