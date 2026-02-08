
import React from 'react';

interface MainframeLayoutProps {
  children: React.ReactNode;
  onRun: () => void;
  onExit: () => void;
  onClear?: () => void;
  onLoad?: () => void;
  onSave?: () => void;
  statusMessage: string;
  viewMode: 'HOME' | 'ISPF' | 'EDITOR' | 'README';
}

export const MainframeLayout: React.FC<MainframeLayoutProps> = ({ 
    children, 
    onRun, 
    onExit, 
    onClear,
    onLoad,
    onSave,
    statusMessage, 
    viewMode 
}) => {
  return (
    <div className="min-h-screen bg-neutral-900 p-4 flex flex-col items-center justify-center font-mono text-sm">
      
      {/* 3270 Container */}
      <div className="w-full max-w-6xl bg-black border-4 border-gray-600 rounded-lg shadow-2xl overflow-hidden flex flex-col h-[85vh]">
        
        {/* Header / Status Bar */}
        <div className="bg-blue-900 text-white p-2 flex justify-between items-center border-b border-gray-600 shrink-0">
            <div className="flex items-center gap-2">
                {/* Branding Icon */}
                <span className="text-xl">🦆</span>
                <span className="font-bold tracking-widest text-yellow-300">DUCK MAINFRAME SIMULATOR</span>
                <span className="text-xs bg-blue-800 px-2 py-0.5 rounded text-gray-300 border border-blue-700">v1.0</span>
            </div>
            <div className="text-cyan-300 hidden sm:block">
                USER=STUDENT &nbsp; TERM=3278-2 &nbsp; {new Date().toLocaleTimeString()}
            </div>
        </div>

        {/* Content Area */}
        <div className="flex-1 flex flex-col md:flex-row relative overflow-hidden">
            {children}
        </div>

        {/* Footer Keys - Contextual */}
        <div className="bg-black text-cyan-400 p-2 border-t border-gray-700 flex flex-wrap gap-x-6 gap-y-1 text-sm select-none font-bold tracking-wide shrink-0 font-[VT323] text-lg items-center">
            
            {/* HOME VIEW KEYS */}
            {viewMode === 'HOME' && (
                <div className="text-gray-500 text-xs">
                    SELECT OPTION TO CONTINUE
                </div>
            )}

            {/* README VIEW KEYS */}
            {viewMode === 'README' && (
                <div onClick={onExit} className="cursor-pointer hover:bg-cyan-900 hover:text-white px-1 rounded transition-colors">
                    F3=EXIT
                </div>
            )}

            {/* ISPF VIEW KEYS (Legacy Support) */}
            {viewMode === 'ISPF' && (
                <>
                    <div onClick={onExit} className="cursor-pointer hover:bg-cyan-900 hover:text-white px-1 rounded transition-colors">
                        F3=EXIT
                    </div>
                </>
            )}

            {/* EDITOR VIEW KEYS */}
            {viewMode === 'EDITOR' && (
                <>
                    <div onClick={onExit} className="cursor-pointer hover:bg-cyan-900 hover:text-white px-1 rounded transition-colors">
                        F3=HOME
                    </div>
                    <div onClick={onRun} className="cursor-pointer hover:bg-cyan-900 hover:text-white px-1 rounded transition-colors">
                        F4=RUN
                    </div>
                    <div onClick={onClear} className="cursor-pointer hover:bg-cyan-900 hover:text-white px-1 rounded transition-colors">
                        F5=CLEAR
                    </div>
                    <div onClick={onLoad} className="cursor-pointer hover:bg-cyan-900 hover:text-white px-1 rounded transition-colors">
                        F6=LOAD
                    </div>
                    <div onClick={onSave} className="cursor-pointer hover:bg-cyan-900 hover:text-white px-1 rounded transition-colors">
                        F7=SAVE
                    </div>
                </>
            )}
            
            <div className="ml-auto text-yellow-400 animate-pulse px-1">
                {statusMessage}
            </div>
            
            {/* Minimal Footer Branding */}
            <div className="w-full text-center text-xs text-gray-600 mt-1 font-sans border-t border-gray-900 pt-1">
                DUCK Mainframe Simulator • Educational Purpose Only
            </div>
        </div>
      </div>
    </div>
  );
};
