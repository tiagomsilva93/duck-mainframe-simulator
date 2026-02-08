
import React, { useEffect, useRef, useState } from 'react';
import { ScreenChar } from '../compiler/runtime';

interface ScreenProps {
    buffer: ScreenChar[];
    onInput: (buffer: ScreenChar[]) => void;
    onCommand: (command: string) => void;
    onCellClick?: (index: number, row: number, col: number) => void;
    active: boolean;
    cursorPosition?: number;
}

export const Screen: React.FC<ScreenProps> = ({ 
    buffer, 
    onInput, 
    onCommand, 
    onCellClick,
    active, 
    cursorPosition 
}) => {
    const [localBuffer, setLocalBuffer] = useState<ScreenChar[]>(buffer);
    const [cursor, setCursor] = useState(0);
    const containerRef = useRef<HTMLDivElement>(null);

    useEffect(() => {
        setLocalBuffer(buffer);
    }, [buffer]);

    useEffect(() => {
        if (cursorPosition !== undefined) {
            setCursor(cursorPosition);
        }
    }, [cursorPosition]);

    const handleKeyDown = (e: React.KeyboardEvent) => {
        if (!active) return;
        
        if (e.key === 'F3') {
            e.preventDefault();
            onCommand('EXIT');
            return;
        }

        if (e.key === 'Enter') {
            e.preventDefault();
            onInput(localBuffer);
            return;
        }

        if (e.key === 'ArrowRight') {
            setCursor((p) => Math.min(p + 1, 1919));
            return;
        }
        if (e.key === 'ArrowLeft') {
            setCursor((p) => Math.max(p - 1, 0));
            return;
        }
        if (e.key === 'ArrowDown') {
            setCursor((p) => Math.min(p + 80, 1919));
            return;
        }
        if (e.key === 'ArrowUp') {
            setCursor((p) => Math.max(p - 80, 0));
            return;
        }

        if (e.key.length === 1) {
            const newBuf = [...localBuffer];
            newBuf[cursor] = { ...newBuf[cursor], char: e.key.toUpperCase() };
            setLocalBuffer(newBuf);
            setCursor((p) => Math.min(p + 1, 1919));
        }
        
        if (e.key === 'Backspace') {
             const newBuf = [...localBuffer];
             const target = Math.max(0, cursor - 1);
             newBuf[target] = { ...newBuf[target], char: ' ' };
             setLocalBuffer(newBuf);
             setCursor(target);
        }
    };

    const handleClick = (e: React.MouseEvent, i: number) => {
        const row = Math.floor(i / 80);
        const col = i % 80;
        setCursor(i);
        if (onCellClick) {
            onCellClick(i, row, col);
        }
    };

    return (
        <div 
            ref={containerRef}
            className="w-full h-full bg-black text-green-500 font-mono text-xl outline-none overflow-hidden relative p-1"
            style={{ 
                display: 'grid', 
                gridTemplateColumns: `repeat(80, 1ch)`, 
                gridTemplateRows: `repeat(24, 1.5rem)`,
                fontFamily: "'VT323', monospace",
                lineHeight: '1.5rem',
                fontSize: 'min(1.5rem, 3.5vw)' // Adaptive font for mobile
            }}
            tabIndex={0}
            onKeyDown={handleKeyDown}
            autoFocus
        >
            {localBuffer.map((cell, i) => {
                const isCursor = i === cursor;
                const row = Math.floor(i / 80);
                // Heuristic for mobile clickable links (rows 8-12 on Home Screen)
                const isLikelyLink = row >= 8 && row <= 12 && cell.char !== ' ' && cell.attr !== 2;

                return (
                    <div 
                        key={i} 
                        onClick={(e) => handleClick(e, i)}
                        className={`
                            flex items-center justify-center
                            ${isCursor && active ? 'bg-green-500 text-black animate-pulse' : ''}
                            ${cell.attr === 1 ? 'text-white font-bold' : ''}
                            ${cell.attr === 2 ? 'text-red-500 font-bold underline decoration-dotted' : ''}
                            ${isLikelyLink ? 'cursor-pointer hover:bg-green-900/30' : 'cursor-text'}
                        `}
                    >
                        {cell.char}
                    </div>
                );
            })}
        </div>
    );
};
