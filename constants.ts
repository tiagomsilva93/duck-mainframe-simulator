
export const COBOL_EXAMPLES = [
  {
    id: 'DUCKMAIN',
    name: 'DUCK MAIN DEMO',
    code: `000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. DUCK-DEMO.
000300 DATA DIVISION.
000400 WORKING-STORAGE SECTION.
000500 01 WS-WELCOME  PIC X(25) VALUE "WELCOME TO DUCK MAINFRAME".
000600 PROCEDURE DIVISION.
000700     DISPLAY WS-WELCOME.
000800     DISPLAY "COBOL IS RUNNING IN YOUR BROWSER!".
000900     STOP RUN.`
  }
];

export const SAMPLE_CODE = COBOL_EXAMPLES[0].code;

export const PANEL_ISR_PRIM = `)ATTR
  % TYPE(TEXT) INTENS(HIGH) COLOR(WHITE)
  + TYPE(TEXT) INTENS(LOW)  COLOR(GREEN)
  _ TYPE(INPUT) INTENS(HIGH) COLOR(RED) CAPS(ON)
  # TYPE(TEXT) INTENS(HIGH) COLOR(YELLOW)
)BODY
%-----------------------  🦆 DUCK MAINFRAME SIMULATOR  ------------------------
%OPTION ===>_ZCMD                                                              +
%
%
%   #Educational Web Simulator inspired by IBM Mainframe (ISPF)
%   #Learn COBOL concepts in a simple and interactive way.
%
%   %1+  Open Program Editor      +<== START HERE
%
%   +X   Documentation            (Coming Soon)
%   +X   Examples                 (Coming Soon)
%
%
%   +This is a simulator, not a real mainframe environment.
%   +Everything runs in your browser.
%
%
%
%
%
%
%
%
%
%   +Press%ENTER+to Open Editor.
%   +Press%F3+to Exit/Reload.
%
)PROC
)END`;

export const PANEL_ISPOPT = `)ATTR
  % TYPE(TEXT) INTENS(HIGH) COLOR(WHITE)
  + TYPE(TEXT) INTENS(LOW)  COLOR(GREEN)
  _ TYPE(INPUT) INTENS(HIGH) COLOR(RED) CAPS(ON)
)BODY
%-----------------------  DUCK SETTINGS  -----------------------------------
%OPTION ===>_ZCMD                                                              +
%
%
%   Log/List ...
%
%   Function keys ...
%
+Press%F3+to Exit.
)PROC
)END`;
