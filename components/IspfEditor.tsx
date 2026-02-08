
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
    const [lines, setLines] = useState<LineState[]>([]);
    const [headerError, setHeaderError] = useState("");
    
    const isLocalUpdate = useRef(false);
    const lineInputsRef = useRef<Map<number, HTMLInputElement>>(new Map());
    const pendingCursor = useRef<{ line: number, col: number } | null>(null);

    const INDICATOR_COL = 6; 
    const AREA_A_COL = 7;    
    const AREA_B_COL = 11;   
    const MAX_CODE_COL = 71; 

    useEffect(() => {
        const critical = diagnostics.find(d => d.severity === 'ERROR' || d.severity === 'SEVERE');
        const warn = diagnostics.find(d => d.severity === 'WARNING');
        if (critical) setHeaderError(critical.code);
        else if (warn) setHeaderError(warn.code);
        else setHeaderError("");
    }, [diagnostics]);

    useEffect(() => {
        if (!isLocalUpdate.current) {
            const raw = content.split('\n');
            const processed = raw.map(l => ({ cmd: '', content: l.padEnd(80, ' ').substring(0, 80) }));
            while (processed.length < 24) processed.push({ cmd: '', content: ' '.repeat(80) });
            setLines(processed);
        }
        isLocalUpdate.current = false;
    }, [content]);

    useLayoutEffect(() => {
        if (pendingCursor.current) {
            const { line, col } = pendingCursor.current;
            const input = lineInputsRef.current.get(line);
            if (input) {
                input.focus();
                const safeCol = Math.max(0, Math.min(80, col));
                input.setSelectionRange(safeCol, safeCol);
            }
            pendingCursor.current = null;
        }
    });

    const updateContent = (newLines: LineState[]) => {
        isLocalUpdate.current = true;
        setLines(newLines);
        const joined = newLines.map(l => l.content.trimEnd()).join('\n');
        onChange(joined);
    };

    const handleSelect = (e: React.SyntheticEvent<HTMLInputElement>, index: number) => {
        const target = e.currentTarget;
        if (target.selectionStart !== null && target.selectionStart < INDICATOR_COL) {
            target.setSelectionRange(INDICATOR_COL, INDICATOR_COL);
        }
    };

    const handleContentKeyDown = (e: React.KeyboardEvent<HTMLInputElement>, index: number) => {
        // Global Functional Keys in Editor
        if (e.key === 'F3') { e.preventDefault(); onExit(); return; }
        if (e.key === 'F4') { e.preventDefault(); onRun(); return; }
        if (e.key === 'F5') { e.preventDefault(); onClear(); return; }
        if (e.key === 'F6') { e.preventDefault(); onLoad(); return; }
        if (e.key === 'F7') { e.preventDefault(); onSave(); return; }

        const target = e.currentTarget;
        const cursor = target.selectionStart || 0;
        const currentLine = lines[index].content;

        if (e.key === 'Home') {
            e.preventDefault();
            const firstChar = currentLine.substring(INDICATOR_COL).search(/\S/);
            const dest = firstChar === -1 ? AREA_A_COL : firstChar + INDICATOR_COL;
            target.setSelectionRange(dest, dest);
            return;
        }

        if (e.key === 'End') {
            e.preventDefault();
            const trimmed = currentLine.substring(0, MAX_CODE_COL + 1).trimEnd();
            target.setSelectionRange(trimmed.length, trimmed.length);
            return;
        }

        if (e.key === 'Tab') {
            e.preventDefault();
            let nextStop = AREA_A_COL;
            if (e.shiftKey) {
                if (cursor > AREA_B_COL) nextStop = AREA_B_COL;
                else if (cursor > AREA_A_COL) nextStop = AREA_A_COL;
                else nextStop = INDICATOR_COL;
            } else {
                if (cursor < AREA_A_COL) nextStop = AREA_A_COL;
                else if (cursor < AREA_B_COL) nextStop = AREA_B_COL;
                else nextStop = Math.min(MAX_CODE_COL, Math.floor((cursor + 4) / 4) * 4);
            }
            target.setSelectionRange(nextStop, nextStop);
            return;
        }

        if (e.key === 'ArrowUp') {
            e.preventDefault();
            if (index > 0) {
                const prevInput = lineInputsRef.current.get(index - 1);
                if (prevInput) {
                    prevInput.focus();
                    prevInput.setSelectionRange(cursor, cursor);
                }
            }
            return;
        }

        if (e.key === 'ArrowDown') {
            e.preventDefault();
            if (index < lines.length - 1) {
                const nextInput = lineInputsRef.current.get(index + 1);
                if (nextInput) {
                    nextInput.focus();
                    nextInput.setSelectionRange(cursor, cursor);
                }
            }
            return;
        }

        if (e.key === 'Enter') {
            e.preventDefault();
            const newLines = [...lines];
            
            let startCol = AREA_A_COL;
            const contentPart = currentLine.substring(INDICATOR_COL, MAX_CODE_COL + 1).trimStart();
            const isAreaB = currentLine[AREA_B_COL] !== ' ' || (currentLine.substring(AREA_A_COL, AREA_B_COL).trim() === "");
            if (isAreaB) startCol = AREA_B_COL;

            const prefix = currentLine.substring(0, cursor);
            const suffix = currentLine.substring(cursor);
            
            newLines[index].content = prefix.padEnd(80, ' ');
            const newLineText = " ".repeat(startCol) + suffix.trimStart();
            newLines.splice(index + 1, 0, { cmd: '', content: newLineText.padEnd(80, ' ') });
            
            updateContent(newLines);
            pendingCursor.current = { line: index + 1, col: startCol };
            return;
        }

        if (e.key === 'Backspace' && cursor > INDICATOR_COL) {
            e.preventDefault();
            const prefix = currentLine.substring(0, cursor - 1);
            const suffix = currentLine.substring(cursor, MAX_CODE_COL + 1);
            const idArea = currentLine.substring(MAX_CODE_COL + 1);
            const newLine = (prefix + suffix + " ").substring(0, MAX_CODE_COL + 1) + idArea;
            const nl = [...lines];
            nl[index].content = newLine;
            updateContent(nl);
            pendingCursor.current = { line: index, col: cursor - 1 };
        }

        if (e.key === 'Delete' && cursor <= MAX_CODE_COL) {
            e.preventDefault();
            const prefix = currentLine.substring(0, cursor);
            const suffix = currentLine.substring(cursor + 1, MAX_CODE_COL + 1);
            const idArea = currentLine.substring(MAX_CODE_COL + 1);
            const newLine = (prefix + suffix + " ").substring(0, MAX_CODE_COL + 1) + idArea;
            const nl = [...lines];
            nl[index].content = newLine;
            updateContent(nl);
            pendingCursor.current = { line: index, col: cursor };
        }

        if (e.key.length === 1 && !e.ctrlKey && !e.altKey && !e.metaKey) {
            e.preventDefault();
            if (cursor > MAX_CODE_COL) return; 

            const char = e.key.toUpperCase();
            const prefix = currentLine.substring(0, cursor);
            const suffix = currentLine.substring(cursor, MAX_CODE_COL); 
            const idArea = currentLine.substring(MAX_CODE_COL + 1);
            
            const newLine = (prefix + char + suffix).substring(0, MAX_CODE_COL + 1) + idArea;
            const nl = [...lines];
            nl[index].content = newLine;
            updateContent(nl);
            pendingCursor.current = { line: index, col: cursor + 1 };
        }
    };

    const handlePaste = (e: React.ClipboardEvent, index: number) => {
        e.preventDefault();
        const text = e.clipboardData.getData('text').toUpperCase();
        const rawRows = text.split(/\r?\n/);
        
        const newLines = [...lines];
        let currentRow = index;

        rawRows.forEach(row => {
            if (currentRow >= newLines.length) {
                newLines.push({ cmd: '', content: ' '.repeat(80) });
            }
            
            let cleanRow = row.trimEnd();
            if (!cleanRow.startsWith(' ')) {
                cleanRow = " ".repeat(AREA_B_COL) + cleanRow;
            } else if (cleanRow.length < INDICATOR_COL) {
                 cleanRow = " ".repeat(INDICATOR_COL) + cleanRow;
            }

            newLines[currentRow].content = cleanRow.padEnd(80, ' ').substring(0, 80);
            currentRow++;
        });

        updateContent(newLines);
    };

    const renderHighlightedLine = (text: string) => {
        if (text.length > 6 && (text[INDICATOR_COL] === '*' || text[INDICATOR_COL] === '/')) {
            return <span className={ISPF_THEME.colors.COMMENT}>{text}</span>;
        }
        try {
            const tokens = new Lexer(text).tokenize().filter(t => t.type !== TokenType.EOF);
            const elements: React.ReactNode[] = [];
            let currentIdx = 0; 
            tokens.forEach((token, i) => {
                const tokenStart = token.column - 1; 
                if (tokenStart > currentIdx) {
                    elements.push(<span key={`gap-${i}`} className="text-transparent">{text.substring(currentIdx, tokenStart)}</span>);
                }
                const className = getTokenClass(token.type);
                elements.push(<span key={`tok-${i}`} className={className}>{token.value}</span>);
                currentIdx = tokenStart + token.value.length;
            });
            if (currentIdx < text.length) {
                elements.push(<span key="gap-end" className="text-transparent">{text.substring(currentIdx)}</span>);
            }
            return elements;
        } catch (e) {
            return <span className={ISPF_THEME.colors.DEFAULT}>{text}</span>;
        }
    };

    return (
        <div className="flex flex-col h-full bg-black font-mono text-green-500 text-xl relative w-full overflow-hidden select-none border-r border-gray-800">
            <div className="bg-blue-900 text-white p-1 flex justify-between px-2 text-[10px] font-bold shrink-0">
                <div className="flex gap-4">
                  <span>EDIT       {datasetName}</span>
                  <span className="text-yellow-400 font-bold">{headerError}</span>
                </div>
                <span>COL 001 080</span>
            </div>
            
            <div className="flex-1 overflow-y-auto overflow-x-hidden relative custom-scrollbar bg-black">
                <div className="sticky top-0 z-20 bg-black border-b border-gray-800 text-blue-400 w-full pl-[6ch] pointer-events-none text-sm">
                    <div className="whitespace-pre font-bold opacity-70 py-1 leading-none">
                        <span className="text-yellow-600">      * A   B                                                               </span><br/>
                        <span>----+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8</span>
                    </div>
                </div>

                {lines.map((line, i) => {
                    const lineDiags = diagnostics.filter(d => d.line === i + 1);
                    const hasError = lineDiags.some(d => d.severity === 'ERROR' || d.severity === 'SEVERE');
                    const hasWarning = !hasError && lineDiags.some(d => d.severity === 'WARNING');
                    
                    let bgClass = "bg-transparent";
                    let numClass = "text-cyan-500/60";
                    
                    if (hasError) {
                        bgClass = "bg-red-900/10";
                        numClass = "bg-red-900/90 text-white";
                    } else if (hasWarning) {
                        bgClass = "bg-yellow-900/10";
                        numClass = "bg-yellow-700/80 text-white";
                    }

                    return (
                        <div key={i} className={`flex h-[1.5rem] group relative w-full items-center ${bgClass}`}>
                            <div className={`w-[6ch] text-right border-r border-gray-800 mr-[1ch] shrink-0 font-bold text-base select-none transition-colors ${numClass}`}>
                                <input
                                    className="w-full bg-transparent text-right outline-none uppercase font-bold focus:text-white"
                                    value={line.cmd || (i + 1).toString().padStart(6, '0')}
                                    onChange={(e) => {
                                        const nl = [...lines];
                                        nl[i].cmd = e.target.value;
                                        setLines(nl);
                                    }}
                                    maxLength={6}
                                />
                            </div>
                            
                            <div className="relative w-[80ch] shrink-0 h-full">
                                <div className="absolute inset-0 whitespace-pre pointer-events-none z-0 text-base" style={{ lineHeight: '1.5rem' }}>
                                    {renderHighlightedLine(line.content)}
                                </div>
                                <input 
                                    ref={el => { if (el) lineInputsRef.current.set(i, el); else lineInputsRef.current.delete(i); }}
                                    className="relative z-10 w-full h-full bg-transparent outline-none uppercase font-bold text-transparent caret-white selection:bg-blue-500/30 p-0 m-0 border-none text-base"
                                    value={line.content}
                                    onChange={(e) => {
                                        const nl = [...lines];
                                        nl[i].content = e.target.value.padEnd(80, ' ').substring(0, 80);
                                        updateContent(nl);
                                    }}
                                    onKeyDown={(e) => handleContentKeyDown(e, i)}
                                    onSelect={(e) => handleSelect(e, i)}
                                    onPaste={(e) => handlePaste(e, i)}
                                    maxLength={80}
                                    spellCheck={false}
                                    style={{ width: '80ch', lineHeight: '1.5rem' }}
                                />
                                
                                {lineDiags.length > 0 && (
                                    <div className="absolute left-[82ch] top-0 hidden group-hover:block z-50 pointer-events-none">
                                        <div className="bg-black border border-gray-700 p-2 shadow-2xl min-w-[300px]">
                                            {lineDiags.map((d, idx) => (
                                                <div key={idx} className={`text-xs font-mono mb-1 ${d.severity === 'ERROR' || d.severity === 'SEVERE' ? 'text-red-400' : 'text-yellow-400'}`}>
                                                    {d.code}: {d.message}
                                                </div>
                                            ))}
                                        </div>
                                    </div>
                                )}
                            </div>
                        </div>
                    );
                })}
            </div>
        </div>
    );
};
