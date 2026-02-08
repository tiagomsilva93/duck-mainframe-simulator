
import { 
  ProgramNode, StatementNode, ExpressionNode, ConditionNode, 
  VariableDeclarationNode, PerformStatement, AcceptStatement, DisplayStatement, TokenType, MoveStatement, Severity
} from './types';
import { ValidationError } from './validator';

interface SymbolInfo {
  name: string;
  level: number;
  type: 'X' | '9';
  length: number;
  line: number;
}

export class SemanticValidator {
  private static symbolTable: Map<string, SymbolInfo> = new Map();
  private static errors: ValidationError[] = [];

  public static validate(ast: ProgramNode): ValidationError[] {
    this.symbolTable.clear();
    this.errors = [];

    // Verificação de Identificação Básica
    if (!ast.id || ast.id === "UNKNOWN") {
        this.addError(1, 1, 'IGYPS2121-S', "PROGRAM-ID IS MISSING OR INVALID.", 'SEVERE');
    }

    // Pass 1: Tabela de Símbolos
    const allVariables = [
      ...(ast.dataDivision.fileSection?.flatMap(fd => fd.recordName ? [{ name: fd.recordName, level: 1, picType: 'X', picLength: fd.recordLength } as any] : []) || []),
      ...ast.dataDivision.workingStorage,
      ...ast.dataDivision.linkageSection
    ];
    this.buildSymbolTable(allVariables);

    // Pass 2: Lógica da Procedure Division
    this.validateStatements(ast.procedureDivision.statements);

    return this.errors;
  }

  private static buildSymbolTable(variables: VariableDeclarationNode[]) {
    variables.forEach(v => {
      const name = v.name.toUpperCase();
      if (this.symbolTable.has(name)) {
        this.addError(0, 0, 'IGYPS2112-S', `DUPLICATE DATA-NAME '${name}'.`, 'SEVERE');
      } else {
        this.symbolTable.set(name, {
          name,
          level: v.level,
          type: v.picType,
          length: v.picLength,
          line: 0
        });
      }
    });
  }

  private static validateStatements(statements: StatementNode[]) {
    statements.forEach(stmt => {
      const line = stmt.line || 0;
      switch (stmt.type) {
        case 'MOVE':
          this.validateMove(stmt as MoveStatement);
          break;
        case 'ADD':
        case 'SUBTRACT':
        case 'MULTIPLY':
        case 'DIVIDE':
          this.validateArithmetic(stmt.value, stmt.target, line, stmt.type);
          break;
        case 'DISPLAY':
          this.validateDisplay(stmt as DisplayStatement);
          break;
        case 'ACCEPT':
          this.validateAccept(stmt as AcceptStatement);
          break;
        case 'IF':
          this.validateCondition(stmt.condition, line);
          this.validateStatements(stmt.thenBody);
          if (stmt.elseBody) this.validateStatements(stmt.elseBody);
          break;
        case 'PERFORM':
          this.validatePerform(stmt as PerformStatement);
          break;
      }
    });
  }

  private static validateMove(stmt: MoveStatement) {
    const line = stmt.line || 0;
    const targetSym = this.validateVariable(stmt.target, line);
    if (!targetSym) return;

    if (typeof stmt.source === 'number') {
      if (targetSym.type !== '9') {
        this.addError(line, 0, 'IGYPS2104-E', `MOVE NUMERIC TO ALPHANUMERIC '${stmt.target}' IS NOT RECOMMENDED.`, 'WARNING');
      }
    } else if (typeof stmt.source === 'string' && (stmt.source.startsWith('"') || stmt.source.startsWith("'"))) {
      if (targetSym.type === '9') {
        this.addError(line, 0, 'IGYPS2104-S', `INCOMPATIBLE MOVE: STRING TO NUMERIC '${stmt.target}'.`, 'SEVERE');
      }
    } else if (typeof stmt.source === 'string') {
        const sourceSym = this.validateVariable(stmt.source, line);
        if (sourceSym && sourceSym.type !== targetSym.type) {
             this.addError(line, 0, 'IGYPS2104-W', `TYPE MISMATCH IN MOVE: ${sourceSym.type} TO ${targetSym.type}.`, 'WARNING');
        }
    }
  }

  private static validateArithmetic(value: string | number, target: string, line: number, verb: string) {
    const targetSym = this.validateVariable(target, line);
    if (targetSym && targetSym.type !== '9') {
      this.addError(line, 0, 'IGYPS2113-S', `TARGET '${target}' MUST BE NUMERIC FOR ${verb}.`, 'SEVERE');
    }
  }

  private static validateDisplay(stmt: DisplayStatement) {
    const line = stmt.line || 0;
    stmt.values.forEach(val => {
      if (typeof val === 'string' && !val.startsWith('"') && !val.startsWith("'")) {
        this.validateVariable(val, line);
      }
    });
  }

  private static validateAccept(stmt: AcceptStatement) {
    this.validateVariable(stmt.target, stmt.line || 0);
  }

  private static validatePerform(stmt: PerformStatement) {
    if (stmt.until) this.validateCondition(stmt.until, stmt.line || 0);
    this.validateStatements(stmt.body);
  }

  private static validateCondition(cond: ConditionNode, line: number) {
    const check = (op: any) => {
        if (typeof op === 'string' && !op.startsWith('"') && !op.startsWith("'")) this.validateVariable(op, line);
    };
    check(cond.left);
    check(cond.right);
  }

  private static validateVariable(name: string, line: number): SymbolInfo | undefined {
    const sym = this.symbolTable.get(name.toUpperCase());
    if (!sym) {
      this.addError(line, 0, 'IGYPS2001-S', `UNDEFINED IDENTIFIER '${name}'.`, 'SEVERE');
      return undefined;
    }
    return sym;
  }

  private static addError(line: number, col: number, code: string, message: string, severity: Severity) {
    this.errors.push({ line, column: col, code, message, severity });
  }
}
