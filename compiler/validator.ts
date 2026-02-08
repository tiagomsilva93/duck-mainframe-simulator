
import { Severity } from './types';

export interface ValidationError {
  line: number;
  column: number;
  code: string;
  message: string;
  severity: Severity; 
}

/**
 * Validator: Authority for COBOL Layout and Reference Format rules.
 * Handles Area A/B, Indicator Area, and Sequence/Info columns.
 */
export class Validator {
  // Headers that MUST start in Area A (Cols 8-11)
  private static AREA_A_HEADERS = new Set([
    'IDENTIFICATION', 'PROGRAM-ID', 'ENVIRONMENT', 'CONFIGURATION', 'INPUT-OUTPUT',
    'DATA', 'FILE', 'WORKING-STORAGE', 'LOCAL-STORAGE', 'LINKAGE', 'PROCEDURE',
    'DIVISION', 'SECTION', 'FD', 'SD', 'RD', 'CD'
  ]);

  public static validate(source: string): ValidationError[] {
    const errors: ValidationError[] = [];
    const lines = source.split('\n');
    
    for (let i = 0; i < lines.length; i++) {
      const rawLine = lines[i];
      const lineNum = i + 1;
      
      if (rawLine.trim().length === 0) continue;

      // 1. Columns 73-80 (Information Area) - Warning/Info only
      if (rawLine.length > 72) {
         const overflow = rawLine.substring(72);
         if (overflow.trim().length > 0) {
             errors.push({
                 line: lineNum,
                 column: 73,
                 code: 'IGYPS0073-I', 
                 message: 'TEXT FOUND IN COLUMNS 73-80 (IGNORED BY COMPILER)',
                 severity: 'INFO'
             });
         }
      }

      // 2. Column 7 (Indicator Area)
      if (rawLine.length >= 7) {
        const indicator = rawLine[6];
        // Valid indicators: Space (normal), * (comment), / (comment/page), - (continuation), D (debug)
        const validIndicators = [' ', '*', '/', '-', 'D', 'd'];
        
        if (!validIndicators.includes(indicator)) {
            errors.push({
                line: lineNum,
                column: 7,
                code: 'IGYPS2004-S',
                message: `INVALID INDICATOR '${indicator}' IN COLUMN 7`,
                severity: 'SEVERE'
            });
            continue;
        }

        // If comment, we skip layout checks for the rest of the line
        if (indicator === '*' || indicator === '/') continue;
      }

      // 3. Area A (8-11) and Area B (12-72) Reference Format
      if (rawLine.length < 8) continue;

      const content = rawLine.substring(7, Math.min(rawLine.length, 72));
      const firstCharIdx = content.search(/\S/);
      
      if (firstCharIdx === -1) continue; // Only whitespace in code area

      const startCol = firstCharIdx + 8; // 1-based
      const firstWordMatch = content.match(/^\s*(\S+)/);
      const firstWord = firstWordMatch ? firstWordMatch[1].toUpperCase().replace(/\.$/, '') : "";
      
      const isAreaA = startCol >= 8 && startCol <= 11;
      const isAreaB = startCol >= 12;

      // Rule: Division/Section Headers and certain level numbers must be in Area A
      if (this.AREA_A_HEADERS.has(firstWord)) {
          if (isAreaB) {
              errors.push({
                  line: lineNum,
                  column: startCol,
                  code: 'IGYPS1088-S',
                  message: `HEADER '${firstWord}' MUST START IN AREA A (COLS 8-11)`,
                  severity: 'SEVERE'
              });
          }
      }

      // Rule: Level 01 and 77 must be in Area A
      if (firstWord === '01' || firstWord === '77') {
          if (isAreaB) {
              errors.push({
                  line: lineNum,
                  column: startCol,
                  code: 'IGYPS1099-S',
                  message: `LEVEL NUMBER ${firstWord} MUST START IN AREA A`,
                  severity: 'SEVERE'
              });
          }
      }

      // Rule: Level 66 and 88 must be in Area B
      if (firstWord === '66' || firstWord === '88') {
          if (isAreaA) {
              errors.push({
                  line: lineNum,
                  column: startCol,
                  code: 'IGYPS1100-S',
                  message: `LEVEL NUMBER ${firstWord} MUST START IN AREA B (COLS 12-72)`,
                  severity: 'SEVERE'
              });
          }
      }
    }

    return errors;
  }
}
