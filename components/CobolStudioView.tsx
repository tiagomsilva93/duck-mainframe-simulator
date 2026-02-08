
import React from 'react';
import { IspfEditor } from './IspfEditor';
import { Sysout } from './Sysout';
import { Diagnostic } from '../compiler/types';
import { ValidationError } from '../compiler/validator';

interface CobolStudioViewProps {
    content: string;
    onChange: (val: string) => void;
    saveStatus: 'SAVED' | 'CHANGES';
    logs: string[];
    diagnostics: Diagnostic[];
    onRun: () => void;
    onExit: () => void;
    onClear: () => void;
    onLoad: () => void;
    onSave: () => void;
    onLoadExample: (id: string) => void;
    onReset: () => void;
}

export const CobolStudioView: React.FC<CobolStudioViewProps> = ({ 
    content, 
    onChange, 
    saveStatus,
    logs, 
    diagnostics, 
    onRun, 
    onExit,
    onClear,
    onLoad,
    onSave
}) => {
    const editorErrors: ValidationError[] = diagnostics.map(d => ({
        line: d.line,
        column: d.column,
        code: d.code,
        message: d.message,
        severity: d.severity
    }));

    return (
        <div className="flex w-full h-full bg-black overflow-hidden select-none">
            
            {/* Editor (60%) - Expanded and cleaner without sidebar */}
            <div className="w-[60%] h-full flex flex-col shrink-0 relative z-10 border-r-2 border-gray-800">
                <IspfEditor 
                    content={content} 
                    onChange={onChange} 
                    onExit={onExit} 
                    onRun={onRun}
                    onClear={onClear}
                    onLoad={onLoad}
                    onSave={onSave}
                    datasetName="'DUCK.COBOL.SOURCE(MAIN)'"
                    diagnostics={editorErrors}
                />
            </div>

            {/* SYSOUT (40%) */}
            <div className="flex-1 h-full min-w-0 bg-[#080808]">
                <Sysout logs={logs} diagnostics={diagnostics} />
            </div>
        </div>
    );
};
