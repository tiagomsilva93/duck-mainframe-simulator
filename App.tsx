
import React, { useState, useCallback, useEffect, useRef, useMemo } from 'react';
import { MainframeLayout } from './components/MainframeLayout';
import { CobolStudioView } from './components/CobolStudioView';
import { InputModal } from './components/InputModal';
import { HomeMenu } from './components/HomeMenu';
import { ReadmeView } from './components/ReadmeView';
import { Lexer } from './compiler/lexer';
import { Parser } from './compiler/parser';
import { Runtime } from './compiler/runtime';
import { Validator } from './compiler/validator';
import { SemanticValidator } from './compiler/semantic';
import { Preprocessor } from './compiler/preprocessor';
import { SAMPLE_CODE, COBOL_EXAMPLES } from './constants';
import { Diagnostic } from './compiler/types';

interface InputRequest {
  variableName: string;
  picType: 'X' | '9';
  picLength: number;
}

const STORAGE_KEY = 'duck_cobol_source';

const App: React.FC = () => {
  const [currentView, setCurrentView] = useState<'HOME' | 'COBOL_STUDIO' | 'README'>('HOME');
  const [status, setStatus] = useState("SISTEMA ATIVO");
  const [saveStatus, setSaveStatus] = useState<'SAVED' | 'CHANGES'>('SAVED');
  
  const [datasetContent, setDatasetContent] = useState(() => {
      return localStorage.getItem(STORAGE_KEY) || SAMPLE_CODE;
  });

  const [logs, setLogs] = useState<string[]>([]);
  const [diagnostics, setDiagnostics] = useState<Diagnostic[]>([]);

  const cobolRuntimeRef = useRef<Runtime | null>(null);

  const [isInputOpen, setIsInputOpen] = useState(false);
  const [inputData, setInputData] = useState<InputRequest>({ variableName: '', picType: 'X', picLength: 0 });
  const inputResolveRef = useRef<((value: string) => void) | null>(null);

  // --- Centralized Navigation Logic ---

  const goToHome = useCallback(() => {
      setCurrentView('HOME');
      setStatus("MENU PRINCIPAL");
  }, []);

  const handleMenuNavigate = (feature: string) => {
      if (feature === 'EDITOR') {
          setCurrentView('COBOL_STUDIO');
          setStatus("PROGRAM EDITOR");
      } else if (feature === 'README') {
          setCurrentView('README');
          setStatus("READ-ME");
      }
  };

  const handleClearOutput = () => {
      setLogs([]);
      setDatasetContent(""); 
      setDiagnostics([]);
      setStatus("TELA LIMPA");
  };

  const handleSaveFile = useCallback(() => {
    const blob = new Blob([datasetContent], { type: 'text/plain' });
    const url = URL.createObjectURL(blob);
    const link = document.createElement('a');
    link.href = url;
    link.download = 'PROGRAM.txt';
    link.click();
    URL.revokeObjectURL(url);
    setStatus("ARQUIVO SALVO");
  }, [datasetContent]);

  const handleLoadFile = useCallback(() => {
    const input = document.createElement('input');
    input.type = 'file';
    input.accept = '.txt';
    input.onchange = (e) => {
        const file = (e.target as HTMLInputElement).files?.[0];
        if (file) {
            const reader = new FileReader();
            reader.onload = (event) => {
                const rawText = (event.target?.result as string) || "";
                const lines = rawText.split(/\r?\n/);
                const normalized = lines.map((line, i) => {
                    let content = line.toUpperCase();
                    // Heuristic: If it doesn't look like standard COBOL (no 6-digit sequence),
                    // move it to Area B (Column 12) for safer compiler interaction.
                    if (content.trim().length > 0 && !/^\d{6}/.test(content)) {
                        content = " ".repeat(11) + content;
                    }
                    return content.padEnd(80, ' ').substring(0, 80);
                }).join('\n');
                
                setDatasetContent(normalized);
                setSaveStatus('CHANGES');
                setStatus("ARQUIVO CARREGADO");
            };
            reader.readAsText(file);
        }
    };
    input.click();
  }, []);

  // --- Validation ---
  useEffect(() => {
      const validateTimer = setTimeout(() => {
          runValidation();
      }, 600); 

      const saveTimer = setTimeout(() => {
          localStorage.setItem(STORAGE_KEY, datasetContent);
          setSaveStatus('SAVED');
      }, 1500);

      return () => {
          clearTimeout(validateTimer);
          clearTimeout(saveTimer);
      };
  }, [datasetContent]);

  const runValidation = () => {
      try {
          const prep = Preprocessor.process(datasetContent, new Map());
          const layoutDiags = Validator.validate(prep.expandedSource);
          
          let allDiags: Diagnostic[] = [...layoutDiags];

          try {
              const tokens = new Lexer(prep.expandedSource).tokenize();
              const parser = new Parser(tokens);
              const ast = parser.parse();
              const semanticDiags = SemanticValidator.validate(ast);
              allDiags = [...allDiags, ...semanticDiags];
          } catch (e: any) {
              const msg = e.message || "SYNTAX ERROR";
              const lineMatch = msg.match(/Line (\d+)/);
              const colMatch = msg.match(/Column (\d+)/);
              allDiags.push({
                  line: lineMatch ? parseInt(lineMatch[1]) : 1,
                  column: colMatch ? parseInt(colMatch[1]) : 1,
                  code: msg.split(' ')[0] || 'IGYPS-S',
                  message: msg.includes(':') ? msg.split(':').slice(1).join(':').trim() : msg,
                  severity: 'ERROR'
              });
          }
          
          setDiagnostics(allDiags);
      } catch (e) {
          console.error("Critical Validation Failure", e);
      }
  };

  const handleEditorChange = (newContent: string) => {
      setDatasetContent(newContent);
      setSaveStatus('CHANGES');
  };

  const handleLoadExample = (id: string) => {
      const example = COBOL_EXAMPLES.find(e => e.id === id);
      if (example) {
          setDatasetContent(example.code);
          setLogs([`EXEMPLO CARREGADO: ${example.name}`]);
          setStatus("PRONTO");
          setSaveStatus('CHANGES');
          setDiagnostics([]);
      }
  };

  const handleResetCode = () => {
      if (confirm("RESET SOURCE: Todas as alterações serão perdidas. Confirmar?")) {
          setDatasetContent(SAMPLE_CODE);
          setLogs(["CÓDIGO RESETADO."]);
          setStatus("REINICIADO");
          setSaveStatus('CHANGES');
          setDiagnostics([]);
      }
  };

  const runCompilationProcess = async () => {
    setLogs([]);
    const hasErrors = diagnostics.some(d => d.severity === 'ERROR' || d.severity === 'SEVERE');
    
    if (hasErrors) {
        setLogs([
            "IGYPS0001-S EXECUTION ABORTED DUE TO COMPILATION ERRORS.",
            "VERIFIQUE O RELATÓRIO DE DIAGNÓSTICOS ACIMA."
        ]);
        setStatus("EXECUÇÃO ABORTADA");
        return;
    }

    setStatus("EXECUTANDO...");
    setLogs(["IGYDS0001I PROGRAM EXECUTED (SIMULATED)", "RETURN CODE = 0000", "-------------------------------------------------"]);

    const prep = Preprocessor.process(datasetContent, new Map());
    
    try {
        const tokens = new Lexer(prep.expandedSource).tokenize();
        const ast = new Parser(tokens).parse();
        
        const runtime = new Runtime(
            (varName, type, len) => new Promise(res => {
                setInputData({ variableName: varName, picType: type, picLength: len });
                setIsInputOpen(true);
                inputResolveRef.current = res;
            }),
            () => {},
            () => Promise.resolve()
        );
        cobolRuntimeRef.current = runtime;

        const res = await runtime.run(ast);
        setLogs(prev => [...prev, ...res.output, "-------------------------------------------------", res.errors.length > 0 ? "JOB FAILED" : "STEP COMPLETED RC=0000"]);
        setStatus(res.errors.length > 0 ? "JOB FAILED" : "COMPLETED RC=0");
        
    } catch (e: any) {
        setLogs(prev => [...prev, `CRITICAL RUNTIME ERROR: ${e.message}`]);
        setStatus("ERRO DE RUNTIME");
    }
  };

  return (
    <MainframeLayout 
        onRun={runCompilationProcess} 
        onExit={goToHome}
        onClear={handleClearOutput}
        onLoad={handleLoadFile}
        onSave={handleSaveFile}
        statusMessage={status}
        viewMode={currentView === 'HOME' ? 'HOME' : currentView === 'README' ? 'README' : 'EDITOR'}
    >
        <InputModal 
            isOpen={isInputOpen}
            variableName={inputData.variableName}
            picType={inputData.picType}
            picLength={inputData.picLength}
            onSubmit={(val) => { setIsInputOpen(false); inputResolveRef.current?.(val); }}
        />

        {currentView === 'HOME' && (
            <HomeMenu onNavigate={handleMenuNavigate} />
        )}

        {currentView === 'README' && (
            <ReadmeView onExit={goToHome} />
        )}

        {currentView === 'COBOL_STUDIO' && (
             <CobolStudioView 
                content={datasetContent}
                onChange={handleEditorChange}
                saveStatus={saveStatus}
                logs={logs}
                diagnostics={diagnostics}
                onRun={runCompilationProcess}
                onExit={goToHome}
                onClear={handleClearOutput}
                onLoad={handleLoadFile}
                onSave={handleSaveFile}
                onLoadExample={handleLoadExample}
                onReset={handleResetCode}
             />
        )}
    </MainframeLayout>
  );
};

export default App;
