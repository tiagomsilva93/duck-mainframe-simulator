
import { ScreenChar } from './runtime';

// --- Types ---

export interface IspfAttribute {
    char: string;
    type: 'TEXT' | 'INPUT' | 'OUTPUT';
    color: 'WHITE' | 'GREEN' | 'RED' | 'BLUE' | 'YELLOW' | 'TURQ' | 'PINK';
    intens: 'HIGH' | 'LOW';
    caps?: boolean;
}

export interface IspfField {
    row: number;
    col: number;
    length: number;
    variable?: string;
    attribute: IspfAttribute;
    value: string;
}

export interface IspfPanel {
    name: string;
    attrs: Map<string, IspfAttribute>;
    bodyRows: string[]; // Raw body text
    procScript: string[];
    fields: IspfField[]; // Calculated layout
}

export interface VariablePool {
    [key: string]: string;
}

// --- Constants ---

const DEFAULT_ATTRS: IspfAttribute[] = [
    { char: '%', type: 'TEXT', color: 'WHITE', intens: 'HIGH' },
    { char: '+', type: 'TEXT', color: 'GREEN', intens: 'LOW' },
    { char: '_', type: 'INPUT', color: 'RED', intens: 'HIGH', caps: false },
    { char: '#', type: 'OUTPUT', color: 'YELLOW', intens: 'HIGH' } // Custom for sim
];

// --- Runtime ---

export class IspfRuntime {
    private variables: VariablePool = {};
    private sharedPool: VariablePool = {};
    private panelStack: string[] = []; // Stack of Panel Names
    private panelRepo: Map<string, IspfPanel> = new Map();
    
    // Screen State
    public screenBuffer: ScreenChar[];
    public cursor: { row: number, col: number } = { row: 0, col: 0 };
    public message: string = "";

    constructor() {
        this.screenBuffer = Array(24 * 80).fill(null).map(() => ({ char: ' ', attr: 0 }));
        // Init System Variables
        this.variables['ZUSER'] = 'USER';
        this.variables['ZTIME'] = new Date().toLocaleTimeString('en-US', { hour12: false, hour: '2-digit', minute: '2-digit' });
        this.variables['ZDATE'] = new Date().toLocaleDateString();
        this.variables['ZCMD'] = '';
    }

    /**
     * Registers a panel with an explicit name.
     */
    public registerPanel(name: string, source: string) {
        const panel = this.parsePanel(name, source);
        this.panelRepo.set(name, panel);
    }

    public start(panelName: string) {
        this.panelStack = [panelName];
        this.renderCurrentPanel();
    }

    public getCurrentPanel(): IspfPanel | undefined {
        if (this.panelStack.length === 0) return undefined;
        return this.panelRepo.get(this.panelStack[this.panelStack.length - 1]);
    }

    // --- Parsing ---

    private parsePanel(name: string, source: string): IspfPanel {
        const lines = source.split('\n');
        let section: 'NONE' | 'ATTR' | 'BODY' | 'PROC' = 'NONE';
        
        const panel: IspfPanel = {
            name: name,
            attrs: new Map(),
            bodyRows: [],
            procScript: [],
            fields: []
        };

        // Load Defaults
        DEFAULT_ATTRS.forEach(a => panel.attrs.set(a.char, a));

        for (const line of lines) {
            const trim = line.trim();
            if (trim.startsWith(')ATTR')) { section = 'ATTR'; continue; }
            if (trim.startsWith(')BODY')) { section = 'BODY'; continue; }
            if (trim.startsWith(')PROC')) { section = 'PROC'; continue; }
            if (trim.startsWith(')END')) { section = 'NONE'; break; }

            if (section === 'ATTR') {
                const char = line.trim().substring(0, 1);
                if (char) {
                    const attr = this.parseAttribute(char, line);
                    panel.attrs.set(char, attr);
                }
            } else if (section === 'BODY') {
                panel.bodyRows.push(line);
            } else if (section === 'PROC') {
                panel.procScript.push(trim);
            }
        }
        
        this.compileBody(panel);
        return panel;
    }

    private parseAttribute(char: string, line: string): IspfAttribute {
        const type = line.includes('TYPE(INPUT)') ? 'INPUT' : (line.includes('TYPE(OUTPUT)') ? 'OUTPUT' : 'TEXT');
        const color = line.match(/COLOR\((\w+)\)/)?.[1] as any || 'GREEN';
        const intens = line.includes('INTENS(HIGH)') ? 'HIGH' : 'LOW';
        const caps = line.includes('CAPS(ON)');
        return { char, type, color, intens, caps };
    }

    private compileBody(panel: IspfPanel) {
        panel.fields = [];
        panel.bodyRows.forEach((rowStr, rIndex) => {
            let colIndex = 0;
            while (colIndex < rowStr.length) {
                const char = rowStr[colIndex];
                const attr = panel.attrs.get(char);

                if (attr) {
                    let content = "";
                    let vName = "";
                    let k = colIndex + 1;
                    while (k < rowStr.length && !panel.attrs.has(rowStr[k])) {
                        content += rowStr[k];
                        k++;
                    }
                    const len = k - colIndex - 1;
                    
                    if (attr.type !== 'TEXT') {
                         const match = content.match(/^([A-Z0-9#@$]+)(\s*)/);
                         if (match) {
                             vName = match[1];
                         }
                    }

                    panel.fields.push({
                        row: rIndex,
                        col: colIndex,
                        length: len < 0 ? 0 : len,
                        variable: vName || undefined,
                        attribute: attr,
                        value: content
                    });
                    colIndex = k;
                } else {
                    colIndex++;
                }
            }
        });
    }

    public handleInput(buffer: ScreenChar[]) {
        const panel = this.getCurrentPanel();
        if (!panel) return;

        panel.fields.forEach(f => {
            if (f.attribute.type === 'INPUT') {
                let val = "";
                const startIdx = f.row * 80 + f.col + 1;
                for (let i = 0; i < f.length; i++) {
                    if (buffer[startIdx + i]) {
                        val += buffer[startIdx + i].char;
                    }
                }
                if (f.variable) {
                    this.variables[f.variable] = val.trim();
                }
            }
        });

        this.executeProc(panel);
        this.renderCurrentPanel();
    }

    private executeProc(panel: IspfPanel) {
        const zcmd = this.variables['ZCMD'] || '';
        if (zcmd !== '') {
            if (zcmd.startsWith('=')) {
                const jump = zcmd.substring(1);
                if (jump === 'X') {
                    this.panelStack = []; 
                }
            }
        }
    }

    public evalZSEL(): string | null {
        const cmd = this.variables['ZCMD'] ? this.variables['ZCMD'].trim().toUpperCase() : '';
        // Mandatory: ENTER or 1 opens Editor
        if (cmd === '1' || cmd === '') return 'PGM(STUDIO)';
        if (cmd === 'X') return 'EXIT';
        return null;
    }

    public clearZCMD() {
        this.variables['ZCMD'] = '';
    }

    public renderCurrentPanel() {
        const panel = this.getCurrentPanel();
        this.clearScreen();

        if (!panel) {
            this.cursor = { row: 0, col: 0 };
            return;
        }

        panel.fields.forEach(f => {
            let content = f.value;
            if (f.variable && this.variables[f.variable] !== undefined) {
                content = this.variables[f.variable];
            }
            content = content.padEnd(f.length, ' ').substring(0, f.length);

            const attrIdx = f.row * 80 + f.col;
            if (attrIdx < 1920) {
                this.screenBuffer[attrIdx] = { char: ' ', attr: this.mapAttr(f.attribute) };
                for (let i = 0; i < f.length; i++) {
                    if (attrIdx + 1 + i < 1920) {
                        this.screenBuffer[attrIdx + 1 + i] = { 
                            char: content[i], 
                            attr: this.mapAttr(f.attribute) 
                        };
                    }
                }
            }
        });

        const zcmdField = panel.fields.find(f => f.variable === 'ZCMD');
        const firstInput = panel.fields.find(f => f.attribute.type === 'INPUT');
        const target = zcmdField || firstInput;
        
        if (target) {
            this.cursor = { row: target.row, col: target.col + 1 };
        } else {
            this.cursor = { row: 0, col: 0 };
        }
        
        if (this.message) {
            this.writeString(23, 1, this.message, 1);
            this.message = "";
        }
    }

    private clearScreen() {
        for (let i = 0; i < 1920; i++) {
            this.screenBuffer[i] = { char: ' ', attr: 0 };
        }
    }

    private writeString(row: number, col: number, str: string, attr: number = 0) {
        let idx = (row * 80) + col;
        for (let i = 0; i < str.length; i++) {
            if (idx < 1920) {
                this.screenBuffer[idx] = { char: str[i], attr };
                idx++;
            }
        }
    }

    private mapAttr(a: IspfAttribute): number {
        if (a.type === 'INPUT') return 2;
        if (a.intens === 'HIGH') return 1;
        return 0;
    }
}
