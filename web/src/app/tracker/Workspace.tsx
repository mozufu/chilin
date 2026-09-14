import { useState } from "react";
import { LIVE, useRepository, type History } from "../../api/queries";
import { ErrorNotice, Spinner } from "../ui";
import { Collaborators } from "./Collaborators";
import { CreateItem } from "./CreateItem";
import { HistoryControl } from "./HistoryControl";
import { ItemDetail } from "./ItemDetail";
import { ItemList } from "./ItemList";
import { Milestones } from "./Milestones";
import { Pulls } from "./Pulls";

type Section = "items" | "pulls" | "milestones" | "people" | "new";

export const Workspace = ({
  owner,
  name,
  isAdmin,
  onClose,
}: {
  owner: string;
  name: string;
  isAdmin: boolean;
  onClose: () => void;
}) => {
  const [history, setHistory] = useState<History>(LIVE);
  const [section, setSection] = useState<Section>("items");
  const [openItem, setOpenItem] = useState<string | null>(null);
  const repository = useRepository(owner, name, history);

  // Every write targets the current revision, so history is a read-only lens.
  // Offering edit controls here would produce writes the user did not intend
  // against content they are not looking at.
  const historical = history.at !== "now";
  const sections: Section[] = historical
    ? ["items", "pulls", "milestones"]
    : ["items", "pulls", "milestones", "people", "new"];



  return (
    <div className="space-y-4">
      <div className="flex items-start justify-between gap-4">
        <div className="flex items-baseline gap-3">
          <button type="button" onClick={onClose} className="text-xs text-neutral-500 hover:text-neutral-300">
            ← Repositories
          </button>
          <h2 className="text-sm text-neutral-200">
            <span className="text-neutral-500">{owner}/</span>
            {name}
          </h2>
        </div>
        <HistoryControl
          history={history}
          revision={repository.data?.revision}
          onChange={(next) => {
            setHistory(next);
            setOpenItem(null);
            setSection("items");
          }}
        />
      </div>

      {/* A rejected revision or timestamp is reported without discarding the
          control, so the value can be corrected rather than retyped blind. */}
      {repository.error !== null && <ErrorNotice error={repository.error} />}

      {/* The spinner replaces only the content: rendering it instead of the
          whole workspace would hide the control needed to leave a bad
          revision, stranding the viewer. */}
      {repository.isPending && repository.error === null && <Spinner label="Loading repository" />}

      {repository.error === null &&
        !repository.isPending &&
        (openItem === null ? (
          <>
            <nav className="flex gap-1">
              {sections.map((entry) => (
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

            {section === "items" && (
              <ItemList owner={owner} name={name} history={history} onOpen={setOpenItem} />
            )}
            {section === "pulls" && (
              <Pulls owner={owner} name={name} history={history} onOpen={setOpenItem} />
            )}
            {section === "milestones" && (
              <Milestones owner={owner} name={name} history={history} onOpen={setOpenItem} />
            )}
            {section === "people" && (
              <Collaborators owner={owner} name={name} canAdminister={isAdmin} />
            )}
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
            history={history}
            onBack={() => setOpenItem(null)}
            onOpen={setOpenItem}
          />
        ))}
    </div>
  );
};
