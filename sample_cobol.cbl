       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO-WORLD.
       
       ENVIRONMENT DIVISION.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01  WS-NAME     PIC X(20).
           01  WS-COUNTER  PIC 9(3).
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "Enter your name: ".
           ACCEPT WS-NAME.
           
           PERFORM VARYING WS-COUNTER FROM 1 BY 1
               UNTIL WS-COUNTER > 3
               DISPLAY "Hello, " WS-NAME
           END-PERFORM.
           
           IF WS-COUNTER > 2
               DISPLAY "Counter is greater than 2"
           ELSE
               DISPLAY "Counter is 2 or less"
           END-IF.
           
           STOP RUN.
