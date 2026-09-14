import { useState } from "react";
import { parsePatch, type FileDiff } from "../../api/diff";
import { Empty } from "../ui";

const STATUS_TONE: Record<FileDiff["status"], string> = {
  added: "text-emerald-400",
  removed: "text-red-400",
  renamed: "text-sky-400",
  modified: "text-neutral-400",
};

const FileEntry = ({ file }: { file: FileDiff }) => {
  const [open, setOpen] = useState(true);

  return (
    <div className="overflow-hidden rounded-lg border border-neutral-800">
      <button
        type="button"
        onClick={() => setOpen(!open)}
        className="flex w-full items-center gap-3 bg-neutral-900/70 px-3 py-2 text-left transition hover:bg-neutral-800/70"
      >
        <span className="text-xs text-neutral-600">{open ? "▾" : "▸"}</span>
        <span className="min-w-0 flex-1 truncate font-mono text-xs text-neutral-300">
          {file.oldPath !== null && <span className="text-neutral-600">{file.oldPath} → </span>}
          {file.path}
        </span>
        <span className={`text-xs ${STATUS_TONE[file.status]}`}>{file.status}</span>
        {!file.binary && (
          <span className="shrink-0 font-mono text-xs">
            <span className="text-emerald-500">+{file.additions}</span>{" "}
            <span className="text-red-500">−{file.deletions}</span>
          </span>
        )}
      </button>

      {open && (
        <div className="bg-neutral-950">
          {file.binary ? (
            <p className="px-3 py-2 text-xs text-neutral-600">Binary file — contents not shown.</p>
          ) : file.hunks.length === 0 ? (
            // A rename with no edits produces no hunks; saying so beats an
            // empty panel that looks like a rendering failure.
            <p className="px-3 py-2 text-xs text-neutral-600">No content changes.</p>
          ) : (
            file.hunks.map((hunk) => (
              <div key={hunk.header}>
                <p className="bg-neutral-900/50 px-3 py-1 font-mono text-xs text-neutral-600">
                  {hunk.header}
                </p>
                <table className="w-full border-collapse font-mono text-xs">
                  <tbody>
                    {hunk.lines.map((line, index) => (
                      <tr
                        key={`${hunk.header}-${index}`}
                        className={
                          line.kind === "added"
                            ? "bg-emerald-950/40"
                            : line.kind === "removed"
                              ? "bg-red-950/40"
                              : ""
                        }
                      >
                        <td className="w-12 select-none border-r border-neutral-900 px-2 text-right text-neutral-700">
                          {line.before ?? ""}
                        </td>
                        <td className="w-12 select-none border-r border-neutral-900 px-2 text-right text-neutral-700">
                          {line.after ?? ""}
                        </td>
                        <td
                          className={`whitespace-pre-wrap break-all px-2 ${
                            line.kind === "added"
                              ? "text-emerald-200"
                              : line.kind === "removed"
                                ? "text-red-200"
                                : line.kind === "meta"
                                  ? "text-neutral-600"
                                  : "text-neutral-400"
                          }`}
                        >
                          {line.kind === "added" ? "+" : line.kind === "removed" ? "−" : " "}
                          {line.text}
                        </td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>
            ))
          )}
        </div>
      )}
    </div>
  );
};

export const DiffView = ({ patch }: { patch: string }) => {
  const files = parsePatch(patch);

  if (files.length === 0) {
    return <Empty>No changes between the recorded base and head.</Empty>;
  }

  const additions = files.reduce((total, file) => total + file.additions, 0);
  const deletions = files.reduce((total, file) => total + file.deletions, 0);

  return (
    <div className="space-y-2">
      <p className="text-xs text-neutral-500">
        {files.length} file{files.length === 1 ? "" : "s"} ·{" "}
        <span className="text-emerald-500">+{additions}</span>{" "}
        <span className="text-red-500">−{deletions}</span>
      </p>
      {files.map((file) => (
        <FileEntry key={file.path} file={file} />
      ))}
    </div>
  );
};
