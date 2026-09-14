/**
 * Unified diff parsing.
 *
 * Chilin produces the patch with `git diff --binary` (Chilin.Pulls.pullDiff),
 * so the input is a standard unified diff except that binary files carry an
 * encoded literal block instead of hunks. Those are reported as binary rather
 * than rendered, since the bytes mean nothing to a reviewer.
 */

export type LineKind = "context" | "added" | "removed" | "meta";

export type DiffLine = {
  kind: LineKind;
  text: string;
  /** Line number in the pre-image, absent for added lines. */
  before: number | null;
  /** Line number in the post-image, absent for removed lines. */
  after: number | null;
};

export type Hunk = { header: string; lines: DiffLine[] };

export type FileDiff = {
  path: string;
  oldPath: string | null;
  status: "added" | "removed" | "modified" | "renamed";
  binary: boolean;
  additions: number;
  deletions: number;
  hunks: Hunk[];
};

const HUNK = /^@@ -(\d+)(?:,(\d+))? \+(\d+)(?:,(\d+))? @@/;

// `git diff` quotes paths containing unusual bytes; the quoted form is C-style.
const unquote = (raw: string): string => {
  if (!raw.startsWith('"')) return raw;
  try {
    return JSON.parse(raw) as string;
  } catch {
    return raw;
  }
};

const stripPrefix = (raw: string): string => {
  const path = unquote(raw);
  return path.startsWith("a/") || path.startsWith("b/") ? path.slice(2) : path;
};

export const parsePatch = (patch: string): FileDiff[] => {
  const files: FileDiff[] = [];
  const lines = patch.split("\n");
  let current: FileDiff | null = null;
  let hunk: Hunk | null = null;
  let beforeAt = 0;
  let afterAt = 0;

  const closeHunk = () => {
    if (current !== null && hunk !== null) current.hunks.push(hunk);
    hunk = null;
  };

  for (const line of lines) {
    if (line.startsWith("diff --git ")) {
      closeHunk();
      if (current !== null) files.push(current);
      // The trailing b/ path is authoritative for the current name; the
      // a/ path only differs for renames and deletions.
      const parts = line.slice("diff --git ".length);
      const split = parts.lastIndexOf(" b/");
      const path = split === -1 ? parts : stripPrefix(parts.slice(split + 1));
      current = {
        path,
        oldPath: null,
        status: "modified",
        binary: false,
        additions: 0,
        deletions: 0,
        hunks: [],
      };
      continue;
    }
    if (current === null) continue;

    if (line.startsWith("new file mode")) {
      current.status = "added";
      continue;
    }
    if (line.startsWith("deleted file mode")) {
      current.status = "removed";
      continue;
    }
    if (line.startsWith("rename from ")) {
      current.oldPath = unquote(line.slice("rename from ".length));
      current.status = "renamed";
      continue;
    }
    if (line.startsWith("GIT binary patch") || line.startsWith("Binary files ")) {
      current.binary = true;
      closeHunk();
      continue;
    }
    // Everything between a binary marker and the next file is encoded payload.
    if (current.binary) continue;

    const match = HUNK.exec(line);
    if (match !== null) {
      closeHunk();
      beforeAt = Number(match[1]);
      afterAt = Number(match[3]);
      hunk = { header: line, lines: [] };
      continue;
    }
    if (hunk === null) continue;

    if (line.startsWith("+")) {
      current.additions += 1;
      hunk.lines.push({ kind: "added", text: line.slice(1), before: null, after: afterAt });
      afterAt += 1;
    } else if (line.startsWith("-")) {
      current.deletions += 1;
      hunk.lines.push({ kind: "removed", text: line.slice(1), before: beforeAt, after: null });
      beforeAt += 1;
    } else if (line.startsWith("\\")) {
      // "\ No newline at end of file" annotates the previous line.
      hunk.lines.push({ kind: "meta", text: line, before: null, after: null });
    } else if (line.startsWith(" ") || line === "") {
      hunk.lines.push({ kind: "context", text: line.slice(1), before: beforeAt, after: afterAt });
      beforeAt += 1;
      afterAt += 1;
    }
  }

  closeHunk();
  if (current !== null) files.push(current);
  return files;
};
