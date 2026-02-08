
import { TokenType } from '../compiler/types';

export const ISPF_THEME = {
  colors: {
    DEFAULT: 'text-green-100', // Variables / Unknown
    KEYWORD: 'text-cyan-300 font-bold', // COBOL Verbs
    LITERAL_STRING: 'text-green-400', // "HELLO"
    LITERAL_NUMBER: 'text-white', // 123
    COMMENT: 'text-gray-500 italic', // * Comment
    DIVISION: 'text-blue-400 font-bold', // DIVISIONS
    SECTION: 'text-blue-300 font-bold', // SECTIONS
    LEVEL: 'text-yellow-500 font-bold', // 01, 77
    ERROR_BG: 'bg-red-900/50', // Error line background
    ERROR_TEXT: 'text-red-300', // Error text override
  }
};

export function getTokenClass(type: TokenType): string {
  switch (type) {
    case TokenType.IDENTIFICATION:
    case TokenType.DIVISION:
    case TokenType.PROGRAM_ID:
    case TokenType.ENVIRONMENT:
    case TokenType.CONFIGURATION:
    case TokenType.INPUT_OUTPUT:
    case TokenType.FILE_CONTROL:
    case TokenType.DATA:
    case TokenType.FILE:
    case TokenType.WORKING_STORAGE:
    case TokenType.LINKAGE:
    case TokenType.PROCEDURE:
      return ISPF_THEME.colors.DIVISION;

    case TokenType.SECTION:
      return ISPF_THEME.colors.SECTION;

    case TokenType.MOVE:
    case TokenType.ADD:
    case TokenType.SUBTRACT:
    case TokenType.MULTIPLY:
    case TokenType.DIVIDE:
    case TokenType.COMPUTE:
    case TokenType.IF:
    case TokenType.THEN:
    case TokenType.ELSE:
    case TokenType.END_IF:
    case TokenType.PERFORM:
    case TokenType.UNTIL:
    case TokenType.TIMES:
    case TokenType.END_PERFORM:
    case TokenType.ACCEPT:
    case TokenType.DISPLAY:
    case TokenType.STOP:
    case TokenType.RUN:
    case TokenType.CALL:
    case TokenType.GOBACK:
    case TokenType.EXIT:
    case TokenType.OPEN:
    case TokenType.CLOSE:
    case TokenType.READ:
    case TokenType.WRITE:
    case TokenType.EXEC:
    case TokenType.CICS:
    case TokenType.END_EXEC:
    case TokenType.RETURN_CICS:
      return ISPF_THEME.colors.KEYWORD;

    case TokenType.LITERAL_STRING:
      return ISPF_THEME.colors.LITERAL_STRING;

    case TokenType.LITERAL_NUMBER:
      return ISPF_THEME.colors.LITERAL_NUMBER;
      
    case TokenType.PIC:
    case TokenType.VALUE:
    case TokenType.TO:
    case TokenType.FROM:
    case TokenType.BY:
    case TokenType.INTO:
      return 'text-purple-300'; // Connectors

    default:
      return ISPF_THEME.colors.DEFAULT;
  }
}
