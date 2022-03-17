      ******************************************************************
      *PROGRAM : PROJECT 4   Mailing Labels                            *
      *AUTHOR  : Mario Garcia                                          *
      *DATE    : 03/16/2022                                            *
      *ABSTRACT: Use of UNSTRING and STRING                            *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LNAME-P03-MAILING-LABELS.
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUST-LIST  ASSIGN TO '..\p04-cust-list.csv'
                             ORGANIZATION IS LINE SEQUENTIAL.
           SELECT LABEL-FILE ASSIGN TO '..\p04-labels.txt'
                             ORGANIZATION IS LINE SEQUENTIAL.
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD  CUST-LIST.
       01  CUST-REC                    PIC X(91).

       FD  LABEL-FILE.
       01  LABEL-REC                   PIC X(80).
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.

       01  WS-RECIPIENTS.
           03  WS-FULLNAME             PIC X(31).
           03  WS-ADDRESS              PIC X(25).
           03  WS-STATECITY           PIC X(22).
           03  WS-ZIP                  PIC X(9).
           03 WS-FORMAT-ZIP.
              05 WS-ZIP-P1        PIC X(5).
              05 WS-ZIP-P2        PIC X(4).
           03  WS-FINAL-ZIP.
                05 WS-FINAL-ZIP-1       PIC X(5).
                05 FILLER PIC X VALUE "-".
                05 WS-FINAL-ZIP-2      PIC X(4).
           03  WS-LNAME                PIC X(15).
           03  WS-FNAME                PIC X(15).
           03  WS-CITY                 PIC X(20).
           03  WS-STATE                PIC X(2).
           03  WS-LASTLINE             PIC X(39).
           03  WS-BLANK                PIC X VALUE SPACES.
           03  WS-DASH               PIC X VALUE "-".
           03  WS-COMMA               PIC XX VALUE ", ".
       01  PRN-RECIPIENTS.
           03  PRN-FULLNAME             PIC X(31).
           03  PRN-LNAME                PIC X(15).
           03  PRN-FNAME                PIC X(15).
           03  PRN-ADDRESS              PIC X(25).
           03  PRN-LASTLINE             PIC X(39).

       01  CHAR-CNT                    PIC 9(2) VALUE ZERO.
       01  WS-CTR                      PIC 9(6)    VALUE ZERO.
       01  WS-FLAGS.
           03  WS-EOF-FLAG             PIC X       VALUE 'N'.
               88  EOF                             VALUE 'Y'.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       100-MAIN.
           OPEN INPUT  CUST-LIST.
           OPEN OUTPUT LABEL-FILE.

           PERFORM UNTIL EOF
               READ CUST-LIST
                   AT END
                       MOVE 'Y' TO WS-EOF-FLAG
                   NOT AT END
                       ADD 1 TO WS-CTR
                       PERFORM 200-PROCESS-RECORD
           END-PERFORM.

           DISPLAY SPACES.
           DISPLAY 'LABELS PRINTED: ', WS-CTR.
           CLOSE CUST-LIST
                 LABEL-FILE.
           STOP RUN.
      *-----------------------------------------------------------------
       200-PROCESS-RECORD.
               MOVE SPACES TO WS-LNAME
               MOVE SPACES TO WS-FNAME
               MOVE SPACES TO WS-ADDRESS
               MOVE SPACES TO WS-CITY
               MOVE SPACES TO WS-STATE
               MOVE SPACES TO WS-ZIP
               UNSTRING CUST-REC DELIMITED BY "*" OR ","
                 INTO WS-LNAME, WS-FNAME,  WS-ADDRESS, WS-CITY,
                 WS-STATE,  WS-ZIP
               END-UNSTRING

              MOVE SPACES TO WS-FULLNAME
               STRING WS-FNAME DELIMITED BY SPACE
                     WS-BLANK DELIMITED BY SIZE
                     WS-LNAME DELIMITED BY SPACE
                     INTO WS-FULLNAME
                 END-STRING

             MOVE SPACES TO WS-ZIP-P1
             MOVE SPACES TO WS-ZIP-P2
             MOVE SPACES TO WS-FINAL-ZIP-1
             MOVE SPACES TO WS-FINAL-ZIP-2
             MOVE WS-ZIP TO WS-FORMAT-ZIP
             MOVE WS-ZIP-P1 TO WS-FINAL-ZIP-1
             MOVE WS-ZIP-P2 TO WS-FINAL-ZIP-2

             MOVE SPACES TO WS-LASTLINE
               STRING WS-CITY DELIMITED BY SPACE
                     WS-COMMA DELIMITED BY SIZE
                     WS-STATE DELIMITED BY SPACE
                     WS-BLANK DELIMITED BY SIZE
                     WS-BLANK DELIMITED BY SIZE
                     WS-FINAL-ZIP DELIMITED BY SPACE
                     INTO WS-LASTLINE
               END-STRING

               MOVE FUNCTION UPPER-CASE(WS-FULLNAME) TO PRN-FULLNAME
               DISPLAY PRN-FULLNAME
               MOVE FUNCTION UPPER-CASE(WS-ADDRESS) TO PRN-ADDRESS
               DISPLAY PRN-ADDRESS
               MOVE FUNCTION UPPER-CASE(WS-LASTLINE) TO PRN-LASTLINE
               DISPLAY PRN-LASTLINE
               WRITE LABEL-REC FROM PRN-FULLNAME
               AFTER ADVANCING 1 LINE
               WRITE LABEL-REC FROM PRN-ADDRESS
               AFTER ADVANCING 1 LINE
               WRITE LABEL-REC FROM PRN-LASTLINE
               AFTER ADVANCING 1 LINE
               MOVE SPACES TO PRN-LASTLINE
               WRITE LABEL-REC FROM PRN-LASTLINE

               DISPLAY SPACES.

