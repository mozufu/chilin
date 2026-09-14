import { useState } from "react";
import { LIVE, type History } from "../../api/queries";

/**
 * Selects the point in tracker history the workspace reads from.
 *
 * The server accepts either a revision or a timestamp, never both
 * (Chilin.Store.snapshotForRead), and resolves a timestamp to the base
 * revision of the earliest event recorded after it. Both are therefore offered
 * as one choice rather than two independent fields.
 */
export const HistoryControl = ({
  history,
  revision,
  onChange,
}: {
  history: History;
  revision: string | undefined;
  onChange: (history: History) => void;
}) => {
  const [open, setOpen] = useState(false);
  const [draft, setDraft] = useState("");

  const viewing = history.at !== "now";

  return (
    <div className="space-y-2">
      <div className="flex items-center gap-2">
        <button
          type="button"
          onClick={() => setOpen(!open)}
          className={`rounded-md px-2 py-1 text-xs transition ${
            viewing
              ? "bg-amber-900/40 text-amber-200 hover:bg-amber-900/60"
              : "text-neutral-500 hover:text-neutral-300"
          }`}
        >
          {viewing ? "Viewing history" : "History"}
        </button>
        <span className="font-mono text-xs text-neutral-700">{revision?.slice(0, 8)}</span>
        {viewing && (
          <button
            type="button"
            onClick={() => {
              onChange(LIVE);
              setOpen(false);
              setDraft("");
            }}
            className="text-xs text-neutral-400 underline-offset-2 hover:text-neutral-200 hover:underline"
          >
            Return to now
          </button>
        )}
      </div>

      {open && (
        <div className="space-y-2 rounded-lg border border-neutral-800 bg-neutral-900/60 px-3 py-2.5">
          <p className="text-xs text-neutral-500">
            Enter an ISO 8601 instant to see the tracker as it stood then, or a 40-character revision
            to pin an exact commit.
          </p>
          <div className="flex gap-2">
            <input
              value={draft}
              onChange={(event) => setDraft(event.target.value)}
              placeholder="2026-09-14T00:00:00Z"
              className="min-w-0 flex-1 rounded-md border border-neutral-700 bg-neutral-950 px-2.5 py-1 font-mono text-xs text-neutral-100 outline-none placeholder:text-neutral-700 focus:border-amber-700"
            />
            <button
              type="button"
              disabled={draft.trim().length === 0}
              onClick={() => {
                const value = draft.trim();
                // A 40-character hex string is a revision; anything else is
                // offered to the server as a timestamp, which validates it.
                onChange(
                  /^[0-9a-f]{40}$/.test(value)
                    ? { at: "revision", revision: value }
                    : { at: "time", asOf: value },
                );
                setOpen(false);
              }}
              className="rounded-md border border-amber-800 bg-amber-950/60 px-2.5 py-1 text-xs text-amber-200 transition hover:bg-amber-900/60 disabled:opacity-30"
            >
              View
            </button>
          </div>
        </div>
      )}

      {viewing && (
        <p className="rounded-md border border-amber-900/50 bg-amber-950/20 px-3 py-1.5 text-xs text-amber-200/80">
          Historical view — editing is disabled. Writes always apply to the current revision.
        </p>
      )}
    </div>
  );
};
