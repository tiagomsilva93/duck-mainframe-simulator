
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

// Updated to a valid didactic template to ensure a professional start
export const SAMPLE_CODE = `       IDENTIFICATION DIVISION.
       PROGRAM-ID. DUCKHELLO.

      * -------------------------------------------------
      * DUCK MAINFRAME SIMULATOR
      * Example COBOL Program
      * -------------------------------------------------

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 WS-MESSAGE        PIC X(40)
          VALUE 'HELLO FROM DUCK MAINFRAME SIMULATOR'.

       01 WS-COUNTER        PIC 9(02) VALUE 1.

       PROCEDURE DIVISION.
       MAIN-PARA.

           DISPLAY WS-MESSAGE.

           PERFORM VARYING WS-COUNTER FROM 1 BY 1
               UNTIL WS-COUNTER > 3
               DISPLAY 'EXECUTION COUNT: ' WS-COUNTER
           END-PERFORM.

           DISPLAY 'PROGRAM FINISHED SUCCESSFULLY.'.

           STOP RUN.`;

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
