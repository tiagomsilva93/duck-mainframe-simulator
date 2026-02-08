
import React, { useState, useEffect, useCallback } from 'react';

interface HomeMenuProps {
    onNavigate: (feature: string) => void;
}

const MENU_OPTIONS = [
    { id: 'EDITOR', label: '1 - PROGRAM EDITOR COBOL', active: true },
    { id: 'COBOL_JSON', label: '2 - CONVERT COBOL TO JSON', active: false },
    { id: 'ASM_JSON', label: '3 - CONVERT ASSEMBLY TO JSON', active: false },
    { id: 'DB2', label: '4 - SIMULATION DB2 QUERIES', active: false },
    { id: 'README', label: '5 - READ-ME', active: true }
];

export const HomeMenu: React.FC<HomeMenuProps> = ({ onNavigate }) => {
    const [selectedIndex, setSelectedIndex] = useState(0);
    const [showInactiveAlert, setShowInactiveAlert] = useState(false);

    const handleSelection = useCallback(() => {
        const option = MENU_OPTIONS[selectedIndex];
        if (option.active) {
            onNavigate(option.id);
        } else {
            setShowInactiveAlert(true);
        }
    }, [selectedIndex, onNavigate]);

    const handleKeyDown = useCallback((e: KeyboardEvent) => {
        if (showInactiveAlert) {
            if (e.key === 'Enter' || e.key === 'Escape') {
                setShowInactiveAlert(false);
            }
            return;
        }

        switch (e.key) {
            case 'ArrowUp':
                setSelectedIndex(prev => (prev > 0 ? prev - 1 : MENU_OPTIONS.length - 1));
                break;
            case 'ArrowDown':
                setSelectedIndex(prev => (prev < MENU_OPTIONS.length - 1 ? prev + 1 : 0));
                break;
            case 'Enter':
                handleSelection();
                break;
            case '1': setSelectedIndex(0); setTimeout(() => onNavigate('EDITOR'), 100); break;
            case '2': setSelectedIndex(1); setTimeout(() => setShowInactiveAlert(true), 100); break;
            case '3': setSelectedIndex(2); setTimeout(() => setShowInactiveAlert(true), 100); break;
            case '4': setSelectedIndex(3); setTimeout(() => setShowInactiveAlert(true), 100); break;
            case '5': setSelectedIndex(4); setTimeout(() => onNavigate('README'), 100); break;
        }
    }, [showInactiveAlert, handleSelection, onNavigate]);

    useEffect(() => {
        window.addEventListener('keydown', handleKeyDown);
        return () => window.removeEventListener('keydown', handleKeyDown);
    }, [handleKeyDown]);

    return (
        <div className="w-full h-full bg-[#101010] flex flex-col items-center justify-center font-mono relative overflow-hidden text-lg">
            
            {/* Retro Background Grid Effect */}
            <div className="absolute inset-0 opacity-10 pointer-events-none" 
                 style={{ 
                     backgroundImage: 'linear-gradient(#00ff00 1px, transparent 1px), linear-gradient(90deg, #00ff00 1px, transparent 1px)', 
                     backgroundSize: '40px 40px' 
                 }}>
            </div>

            {/* Header */}
            <div className="mb-12 text-center z-10">
                <h1 className="text-4xl md:text-6xl font-bold text-yellow-400 mb-2 tracking-widest drop-shadow-[4px_4px_0_rgba(0,0,0,1)]">
                    DUCK MAINFRAME
                </h1>
                <div className="text-cyan-400 text-xl tracking-[0.5em] animate-pulse">SIMULATOR SYSTEM</div>
            </div>

            {/* Menu List */}
            <div className="flex flex-col gap-4 w-full max-w-2xl z-10">
                {MENU_OPTIONS.map((option, index) => {
                    const isSelected = index === selectedIndex;
                    return (
                        <div 
                            key={option.id}
                            className={`
                                relative p-4 cursor-pointer transition-all duration-100 ease-in-out border-l-4
                                flex items-center group
                                ${isSelected 
                                    ? 'bg-green-900/40 border-green-400 text-white translate-x-4' 
                                    : 'border-transparent text-green-700/80 hover:text-green-500'
                                }
                            `}
                            onClick={() => {
                                setSelectedIndex(index);
                                if (option.active) onNavigate(option.id);
                                else setShowInactiveAlert(true);
                            }}
                            onMouseEnter={() => setSelectedIndex(index)}
                        >
                            {/* Mascot */}
                            <div className={`
                                mr-4 text-3xl transition-opacity duration-200
                                ${isSelected ? 'opacity-100 scale-125' : 'opacity-0 scale-75'}
                            `}>
                                🦆
                            </div>

                            {/* Text */}
                            <span className={`
                                font-bold tracking-wider text-xl md:text-2xl
                                ${isSelected ? 'drop-shadow-[2px_2px_0_rgba(0,0,0,0.8)]' : ''}
                            `}>
                                {option.label}
                            </span>
                        </div>
                    );
                })}
            </div>

            {/* Footer decoration */}
            <div className="absolute bottom-8 text-gray-600 text-sm">
                USE <span className="text-gray-400">↑ ↓</span> TO SELECT &nbsp;•&nbsp; <span className="text-gray-400">ENTER</span> TO CONFIRM
            </div>

            {/* Inactive Alert Modal */}
            {showInactiveAlert && (
                <div className="absolute inset-0 bg-black/80 flex items-center justify-center z-50">
                    <div className="bg-black border-4 border-red-600 p-8 text-center shadow-[10px_10px_0px_0px_rgba(50,0,0,1)] max-w-md w-full animate-bounce-short">
                        <div className="text-red-500 text-6xl mb-4">⚠️</div>
                        <h2 className="text-white text-2xl font-bold mb-4 uppercase tracking-widest border-b border-red-900 pb-2">
                            Option Not Available
                        </h2>
                        <p className="text-red-300 mb-8 leading-relaxed text-lg">
                            THIS FUNCTION WILL BE<br/>IMPLEMENTED SOON.
                        </p>
                        <div className="text-gray-500 text-sm animate-pulse">
                            PRESS ENTER TO RETURN
                        </div>
                    </div>
                </div>
            )}
        </div>
    );
};
