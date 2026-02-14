import React, { useState, useEffect, useRef } from "react";
import { Diagnostic } from "../compiler/types";

interface Props {
  content: string;
  onChange: (c: string) => void;
  datasetName: string;
  diagnostics: Diagnostic[];
}

interface Line {
  content: string;
}

const TOTAL_COLS = 80;

export const IspfEditor: React.FC<Props> = ({
  content,
  onChange,
  datasetName,
  diagnostics,
}) => {
  const [lines, setLines] = useState<Line[]>([]);
  const [mode, setMode] = useState<"INSERT" | "OVERWRITE">("INSERT");

  const inputRefs = useRef<Map<number, HTMLInputElement>>(new Map());

  // ===== LOAD EXTERNAL CONTENT =====
  useEffect(() => {
    const split = content.split("\n").map((l) => ({
      content: l.padEnd(TOTAL_COLS, " ").substring(0, TOTAL_COLS),
    }));

    if (split.length === 0) split.push({ content: " ".repeat(TOTAL_COLS) });

    setLines(split);
  }, [content]);

  // ===== UPDATE EXTERNAL STATE =====
  const syncExternal = (newLines: Line[]) => {
    const joined = newLines.map((l) => l.content.trimEnd()).join("\n");
    onChange(joined);
  };

  // ===== HANDLE KEY =====
  const handleKeyDown = (
    e: React.KeyboardEvent<HTMLInputElement>,
    row: number
  ) => {
    const input = e.currentTarget;
    const col = input.selectionStart ?? 0;
    const current = lines[row].content;

    // TOGGLE MODE
    if (e.key === "Insert") {
      e.preventDefault();
      setMode((m) => (m === "INSERT" ? "OVERWRITE" : "INSERT"));
      return;
    }

    // ENTER
    if (e.key === "Enter") {
      e.preventDefault();

      const before = current.substring(0, col);
      const after = current.substring(col);

      const newLines = [...lines];
      newLines[row].content = before.padEnd(TOTAL_COLS, " ");

      newLines.splice(row + 1, 0, {
        content: after.trimStart().padEnd(TOTAL_COLS, " "),
      });

      setLines(newLines);
      syncExternal(newLines);

      setTimeout(() => {
        const next = inputRefs.current.get(row + 1);
        next?.focus();
        next?.setSelectionRange(0, 0);
      }, 0);

      return;
    }

    // BACKSPACE
    if (e.key === "Backspace") {
      if (col === 0 && row > 0) {
        e.preventDefault();

        const prev = lines[row - 1].content.trimEnd();
        const merged =
          (prev + current.trimStart()).padEnd(TOTAL_COLS, " ");

        const newLines = [...lines];
        newLines[row - 1].content = merged.substring(0, TOTAL_COLS);
        newLines.splice(row, 1);

        setLines(newLines);
        syncExternal(newLines);

        setTimeout(() => {
          const prevInput = inputRefs.current.get(row - 1);
          prevInput?.focus();
          prevInput?.setSelectionRange(prev.length, prev.length);
        }, 0);

        return;
      }
    }

    // NORMAL CHAR
    if (e.key.length === 1 && !e.ctrlKey && !e.metaKey && !e.altKey) {
      e.preventDefault();

      if (col >= TOTAL_COLS) return;

      const char = e.key.toUpperCase();
      let newLine = current;

      if (mode === "INSERT") {
        newLine =
          (current.substring(0, col) +
            char +
            current.substring(col))
            .substring(0, TOTAL_COLS);
      } else {
        newLine =
          current.substring(0, col) +
          char +
          current.substring(col + 1);
      }

      const newLines = [...lines];
      newLines[row].content = newLine.padEnd(TOTAL_COLS, " ");

      setLines(newLines);
      syncExternal(newLines);

      setTimeout(() => {
        input.setSelectionRange(col + 1, col + 1);
      }, 0);
    }
  };

  // ===== AUTO SCROLL INFINITE =====
  const handleScroll = (e: React.UIEvent<HTMLDivElement>) => {
    const el = e.currentTarget;
    if (el.scrollTop + el.clientHeight >= el.scrollHeight - 50) {
      setLines((prev) => [
        ...prev,
        { content: " ".repeat(TOTAL_COLS) },
      ]);
    }
  };

  return (
    <div className="flex flex-col h-full bg-black font-mono text-green-500 text-lg">
      
      {/* HEADER */}
      <div className="bg-blue-900 text-white px-2 py-1 flex justify-between text-sm font-bold">
        <span>
          EDIT {datasetName}
        </span>

        <span
          className="cursor-pointer text-yellow-400"
          onClick={() =>
            setMode((m) =>
              m === "INSERT" ? "OVERWRITE" : "INSERT"
            )
          }
        >
          MODE: {mode}
        </span>
      </div>

      {/* EDITOR */}
      <div
        className="flex-1 overflow-auto"
        onScroll={handleScroll}
      >
        {lines.map((line, i) => (
          <div key={i} className="flex">
            <div className="w-[6ch] text-right text-cyan-500 pr-2">
              {(i + 1).toString().padStart(6, "0")}
            </div>

            <input
              ref={(el) => {
                if (el) inputRefs.current.set(i, el);
              }}
              value={line.content}
              onChange={() => {}}
              onKeyDown={(e) => handleKeyDown(e, i)}
              className="bg-transparent outline-none w-[80ch] text-transparent caret-white"
              style={{ caretShape: "block" as any }}
              spellCheck={false}
            />
          </div>
        ))}
      </div>
    </div>
  );
};
