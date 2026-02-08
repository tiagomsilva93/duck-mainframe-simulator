
import React, { useEffect } from 'react';

interface ReadmeViewProps {
    onExit: () => void;
}

export const ReadmeView: React.FC<ReadmeViewProps> = ({ onExit }) => {
    
    useEffect(() => {
        const handleKeyDown = (e: KeyboardEvent) => {
            if (e.key === 'F3' || e.key === 'Escape') {
                e.preventDefault();
                onExit();
            }
        };
        window.addEventListener('keydown', handleKeyDown);
        return () => window.removeEventListener('keydown', handleKeyDown);
    }, [onExit]);

    return (
        <div className="w-full h-full bg-[#050505] text-green-500 font-mono p-4 md:p-8 overflow-auto flex justify-center">
            <div className="max-w-4xl w-full border-2 border-green-900/50 p-6 md:p-10 shadow-2xl bg-black relative">
                
                {/* Header Decoration */}
                <div className="absolute top-0 left-0 w-full h-2 bg-gradient-to-r from-green-900 via-green-500 to-green-900 opacity-50"></div>

                <pre className="whitespace-pre-wrap text-lg md:text-xl leading-relaxed font-bold">
{`
 WEB MAINFRAME COBOL SIMULATOR — DUCK
 -----------------------------------

 This project simulates a classic mainframe
 environment using modern web technologies.

 MAIN GOALS
 ----------
 - Provide a COBOL editor with column control
 - Respect classic COBOL rules (Area A / B)
 - Simulate ISPF navigation
 - Offer a nostalgic experience inspired by
   mainframe terminals and 16-bit games

 CURRENT FEATURES
 ----------------
 - COBOL Program Editor
 - Syntax structure validation
 - Basic command simulation (DISPLAY, ACCEPT, IF)
 - File load into editor

 FUTURE FEATURES
 ---------------
 - COBOL to JSON converter
 - Assembly to JSON converter
 - DB2 query simulation

 CREATED FOR STUDY AND NOSTALGIA ❤️
`}
                </pre>

                <div className="mt-12 pt-4 border-t border-green-900/50 text-center text-yellow-400 animate-pulse">
                    PRESS F3 TO RETURN
                </div>
            </div>
        </div>
    );
};
