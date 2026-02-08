
import React, { useMemo, useEffect, useRef } from 'react';
import { Diagnostic } from '../compiler/types';

interface SysoutProps {
  logs: string[];
  diagnostics: Diagnostic[];
}

export const Sysout: React.FC<SysoutProps> = ({ logs, diagnostics }) => {
  const scrollRef = useRef<HTMLDivElement>(null);

  useEffect(() => {
    if (scrollRef.current) {
        scrollRef.current.scrollTop = scrollRef.current.scrollHeight;
    }
  }, [logs, diagnostics]);

  // Categorização de Diagnósticos (B3)
  const errors = useMemo(() => diagnostics.filter(d => d.severity === 'ERROR' || d.severity === 'SEVERE'), [diagnostics]);
  const warnings = useMemo(() => diagnostics.filter(d => d.severity === 'WARNING'), [diagnostics]);
  const infos = useMemo(() => diagnostics.filter(d => d.severity === 'INFO'), [diagnostics]);

  return (
    <div className="flex-1 h-full flex flex-col bg-[#080808] border-l-2 border-gray-800 overflow-hidden">
      <div className="bg-neutral-800 text-yellow-300 px-2 py-1 text-xs border-b border-gray-700 font-bold flex justify-between shrink-0 select-none">
        <span>SYSOUT / COMPILER REPORT</span>
        <span>STEP: ALL</span>
      </div>
      
      <div 
        ref={scrollRef}
        className="flex-1 p-3 font-mono overflow-auto custom-scrollbar select-text" 
        style={{ fontFamily: "'VT323', monospace", fontSize: '1.15rem', lineHeight: '1.2rem' }}
      >
        <div className="mb-4 text-cyan-800 text-xs uppercase tracking-widest border-b border-gray-900 pb-1">
            DUCK-OS v0.1 COMPILATION SERVICES<br/>
            -------------------------------------------------
        </div>

        {/* Seção de ERROS */}
        {errors.length > 0 && (
            <div className="mb-4">
                <div className="text-red-500 font-bold underline mb-1">*** COMPILATION ERRORS (BLOCKING) ***</div>
                {errors.map((d, i) => (
                    <div key={`err-${i}`} className="text-red-400 mb-0.5 ml-2">
                        {d.code} L{d.line.toString().padStart(4, '0')} C{d.column.toString().padStart(2, '0')}: {d.message}
                    </div>
                ))}
            </div>
        )}

        {/* Seção de WARNINGS */}
        {warnings.length > 0 && (
            <div className="mb-4">
                <div className="text-yellow-600 font-bold mb-1">*** COMPILATION WARNINGS ***</div>
                {warnings.map((d, i) => (
                    <div key={`warn-${i}`} className="text-yellow-500/80 mb-0.5 ml-2">
                        {d.code} L{d.line.toString().padStart(4, '0')}: {d.message}
                    </div>
                ))}
            </div>
        )}

        {/* Seção de INFO */}
        {infos.length > 0 && (
            <div className="mb-4">
                <div className="text-blue-500 font-bold mb-1">*** INFORMATION MESSAGES ***</div>
                {infos.map((d, i) => (
                    <div key={`info-${i}`} className="text-blue-400/80 mb-0.5 ml-2">
                        {d.code} L{d.line.toString().padStart(4, '0')}: {d.message}
                    </div>
                ))}
            </div>
        )}

        {/* Seção de LOGS DE EXECUÇÃO */}
        {logs.length > 0 && (
            <div className="mt-4 border-t border-gray-800 pt-4">
                <div className="text-cyan-700 font-bold mb-2">*** EXECUTION OUTPUT (SYSPRINT) ***</div>
                {logs.map((log, i) => (
                    <div key={i} className="mb-0.5 whitespace-pre-wrap text-green-400">
                        {log}
                    </div>
                ))}
            </div>
        )}
        
        {logs.length === 0 && diagnostics.length === 0 && (
            <div className="text-gray-700 italic opacity-50 flex flex-col items-center justify-center h-full gap-2">
                <span className="text-2xl">⚡</span>
                <span>NO DIAGNOSTICS - READY TO SUBMIT</span>
            </div>
        )}
      </div>
    </div>
  );
};
