import React, { useState, useEffect, useRef, useLayoutEffect } from 'react';
import { Diagnostic } from '../compiler/types';
import { Lexer } from '../compiler/lexer';
import { TokenType } from '../compiler/types';
import { getTokenClass, ISPF_THEME } from '../utils/ispfTheme';

interface IspfEditorProps {
    content: string;
    onChange: (content: string) => void;
    onExit: () => void;
    onRun: () => void;
    onClear: () => void;
    onLoad: () => void;
    onSave: () => void;
    datasetName: string;
    diagnostics: Diagnostic[];
}

interface LineState {
    cmd: string;
    content: string;
}

export const IspfEditor: React.FC<IspfEditorProps> = ({
    content,
    onChange,
    onExit,
    onRun,
    onClear,
    onLoad,
    onSave,
    datasetName,
    diagnostics
}) => {

    const TOTAL_COLS = 80;
    const INDICATOR_COL = 6;
    const AREA_A_COL = 7;
    const AREA_B_COL = 11;
    const MAX_CODE_COL = 71;

    const [lines, setLines] = useState<LineState[]>([]);
    const [headerError, setHeaderError] = useState("");

    const lineInputsRef = useRef<Map<number, HTMLInputElement>>(new Map());
    const pendingCursor = useRef<{ line: number; col: number } | null>(null);

    const internalContentRef = useRef("");

    /* =============================
       HEADER DIAGNOSTICS
    ============================== */

    useEffect(() => {
        const critical = diagnostics.find(d => d.severity === 'ERROR' || d.severity === 'SEVERE');
        const warn = diagnostics.find(d => d.severity === 'WARNING');
        if (critical) setHeaderError(critical.code);
        else if (warn) setHeaderError(warn.code);
        else setHeaderError("");
    }, [diagnostics]);

    /* =============================
       SYNC EXTERNAL CONTENT (SAFE)
    ============================== */

    useEffect(() => {
        if (content === internalContentRef.current) return;

        const raw = content.split('\n');
        const processed = raw.map(l => ({
            cmd: '',
            content: l.padEnd(TOTAL_COLS, ' ').substring(0, TOTAL_COLS)
        }));

        while (processed.length < 24) {
            processed.push({ cmd: '', content: ' '.repeat(TOTAL_COLS) });
        }

        setLines(processed);
        internalContentRef.current = processed.map(l => l.content).join('\n');
    }, [content]);

    useLayoutEffect(() => {
        if (pendingCursor.current) {
            const { line, col } = pendingCursor.current;
            const input = lineInputsRef.current.get(line);
            if (input) {
                input.focus();
                const safeCol = Math.max(0, Math.min(TOTAL_COLS, col));
                input.setSelectionRange(safeCol, safeCol);
            }
            pendingCursor.current = null;
        }
    });

    const updateContent = (newLines: LineState[]) => {
        setLines(newLines);
        const joined = newLines.map(l => l.content).join('\n');
        internalContentRef.current = joined;
        onChange(joined);
    };

    /* =============================
       KEYBOARD ENGINE
    ============================== */

    const handleKeyDown = (e: React.KeyboardEvent<HTMLInputElement>, index: number) => {

        const target = e.currentTarget;
        const cursor = target.selectionStart ?? 0;
        const currentLine = lines[index].content;

        /* FUNCTION KEYS */
        if (e.key === 'F3') { e.preventDefault(); onExit(); return; }
        if (e.key === 'F4') { e.preventDefault(); onRun(); return; }
        if (e.key === 'F5') { e.preventDefault(); onClear(); return; }
        if (e.key === 'F6') { e.preventDefault(); onLoad(); return; }
        if (e.key === 'F7') { e.preventDefault(); onSave(); return; }

        /* ENTER */
        if (e.key === 'Enter') {
            e.preventDefault();

            const newLines = [...lines];
            const prefix = currentLine.substring(0, cursor);
            const suffix = currentLine.substring(cursor);

            newLines[index].content =
                prefix.padEnd(TOTAL_COLS, ' ').substring(0, TOTAL_COLS);

            const newLineContent =
                (" ".repeat(AREA_B_COL) + suffix.trimStart())
                    .padEnd(TOTAL_COLS, ' ')
                    .substring(0, TOTAL_COLS);

            newLines.splice(index + 1, 0, { cmd: '', content: newLineContent });

            updateContent(newLines);
            pendingCursor.current = { line: index + 1, col: AREA_B_COL };
            return;
        }

        /* BACKSPACE */
        if (e.key === 'Backspace') {
            if (cursor === 0 && index > 0) {
                e.preventDefault();

                const prevLine = lines[index - 1].content;
                const merged =
                    (prevLine.trimEnd() + currentLine.trimStart())
                        .padEnd(TOTAL_COLS, ' ')
                        .substring(0, TOTAL_COLS);

                const newLines = [...lines];
                newLines[index - 1].content = merged;
                newLines.splice(index, 1);

                updateContent(newLines);
                pendingCursor.current = {
                    line: index - 1,
                    col: prevLine.trimEnd().length
                };
                return;
            }

            if (cursor > 0) {
                e.preventDefault();

                const prefix = currentLine.substring(0, cursor - 1);
                const suffix = currentLine.substring(cursor);
                const newLine =
                    (prefix + suffix + " ")
                        .substring(0, TOTAL_COLS);

                const newLines = [...lines];
                newLines[index].content = newLine;

                updateContent(newLines);
                pendingCursor.current = { line: index, col: cursor - 1 };
                return;
            }
        }

        /* DELETE */
        if (e.key === 'Delete') {
            e.preventDefault();
            if (cursor >= TOTAL_COLS) return;

            const prefix = currentLine.substring(0, cursor);
            const suffix = currentLine.substring(cursor + 1);
            const newLine =
                (prefix + suffix + " ")
                    .substring(0, TOTAL_COLS);

            const newLines = [...lines];
            newLines[index].content = newLine;

            updateContent(newLines);
            pendingCursor.current = { line: index, col: cursor };
            return;
        }

        /* NORMAL TYPING */
        if (e.key.length === 1 && !e.ctrlKey && !e.metaKey && !e.altKey) {
            e.preventDefault();
            if (cursor >= TOTAL_COLS) return;

            const char = e.key.toUpperCase();
            const prefix = currentLine.substring(0, cursor);
            const suffix = currentLine.substring(cursor, TOTAL_COLS - 1);

            const newLine =
                (prefix + char + suffix)
                    .padEnd(TOTAL_COLS, ' ')
                    .substring(0, TOTAL_COLS);

            const newLines = [...lines];
            newLines[index].content = newLine;

            updateContent(newLines);
            pendingCursor.current = { line: index, col: cursor + 1 };
        }
    };

    /* =============================
       RENDER
    ============================== */

    const renderHighlightedLine = (text: string) => {
        try {
            const tokens = new Lexer(text).tokenize().filter(t => t.type !== TokenType.EOF);
            return tokens.map((t, i) => (
                <span key={i} className={getTokenClass(t.type)}>
                    {t.value}
                </span>
            ));
        } catch {
            return text;
        }
    };

    return (
        <div className="flex flex-col h-full bg-black font-mono text-green-500 text-xl w-full overflow-hidden border-r border-gray-800">

            <div className="bg-blue-900 text-white p-1 flex justify-between px-2 text-[10px] font-bold shrink-0">
                <div className="flex gap-4">
                    <span>EDIT {datasetName}</span>
                    <span className="text-yellow-400 font-bold">{headerError}</span>
                </div>
                <span>COL 001 080</span>
            </div>

            <div className="flex-1 overflow-y-auto bg-black">

                {lines.map((line, i) => (
                    <div key={i} className="flex h-[1.5rem] items-center">

                        <div className="w-[6ch] text-right text-cyan-500/60 border-r border-gray-800 mr-[1ch] font-bold">
                            {(i + 1).toString().padStart(6, '0')}
                        </div>

                        <div className="relative w-[80ch] h-full">
                            <div className="absolute inset-0 whitespace-pre pointer-events-none">
                                {renderHighlightedLine(line.content)}
                            </div>

                            <input
                                ref={el => {
                                    if (el) lineInputsRef.current.set(i, el);
                                    else lineInputsRef.current.delete(i);
                                }}
                                value={line.content}
                                onKeyDown={(e) => handleKeyDown(e, i)}
                                spellCheck={false}
                                className="relative z-10 w-full h-full bg-transparent outline-none uppercase font-bold text-transparent caret-white"
                                style={{ width: '80ch' }}
                            />
                        </div>

                    </div>
                ))}

            </div>
        </div>
    );
};
