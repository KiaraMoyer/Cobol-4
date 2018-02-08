       identification division.
       program-id. CBLKLM04.
       AUTHOR. KIARA MOYER
       DATE-WRITTEN. 1/20/2018
      ******************************************************************
      * CASE PROBLEM 04. THIS PROGRAM USES VALIDATION TO PRINT OUT AN
      * ERROR REPORT FOR INVALID DATA AS WELL AS A REPORT FOR VALID DATA
      ****************************************************************** 
     

       environment division. 
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT POP-MASTER
               ASSIGN TO 'C:\COBOL\CBLPOPSL.DAT'
                   ORGANIZATION IS LINE SEQUENTIAL.
           SELECT VALID-PRTOUT
               ASSIGN TO 'C:\COBOL\CBLPOPSL.PRT'
                   ORGANIZATION IS RECORD SEQUENTIAL.
           SELECT INVALID-PRTOUT
               ASSIGN TO 'C:\COBOL\CBLPOPER.PRT'
                   ORGANIZATION IS RECORD SEQUENTIAL.

       data division.
       FILE SECTION.

       FD POP-MASTER
           LABEL RECORD IS STANDARD
           DATA RECORD IS I-REC
           RECORD CONTAINS 71 CHARACTERS.
       01 I-REC.
           05 P-LNAME              PIC X(15).
           05 P-FNAME              PIC X(15).
           05 P-ADDRESS            PIC X(15).
           05 P-CITY               PIC X(10).
           05 P-STATE              PIC XX.
               88 VALID-STATES
               VALUE 'IA','IL','MO','MI','NE','WI'.
           05 P-ZIP                PIC 9(9).
           05 P-POP-TYPE           PIC 99.
               88 VALID-POP        VALUE 1 THRU 12.
           05 P-CASES              PIC 99.
           05 P-TEAM               PIC X.

       FD VALID-PRTOUT
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 132 CHARACTERS
           DATA RECORD IS PRTLINE
           LINAGE IS 60 WITH FOOTING AT 56.
           01 PRTLINE              PIC X(132).

       FD INVALID-PRTOUT
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 132 CHARACTERS
           DATA RECORD IS PRINTLINE
           LINAGE IS 60 WITH FOOTING AT 56.
           01 PRINTLINE              PIC X(132).


       working-storage section.
       01 MISC.
           05 EOF                  PIC X       VALUE 'F'.
           05 ERRSW                PIC X       VALUE 'T'.
           05 CUR-DATE.
               10 CUR-YEAR         PIC X(4).
               10 CUR-MONTH        PIC XX.
               10 CUR-DAY          PIC XX.
           05 C-PCTR               PIC 99      VALUE 0.
           05 C-DEPOSIT            PIC 999V99  VALUE 0.
           05 C-COKE               PIC 999     VALUE 0.
           05 C-DIET-COKE          PIC 999     VALUE 0.
           05 C-MELLO-Y            PIC 999     VALUE 0.
           05 C-CHERRY-COKE        PIC 999     VALUE 0.
           05 C-DIET-CHERRY        PIC 999     VALUE 0.
           05 C-SPRITE             PIC 999     VALUE 0.
           05 C-DIET-SPRITE        PIC 999     VALUE 0.
           05 C-DASANI             PIC 999     VALUE 0.
           05 C-C2                 PIC 999     VALUE 0.
           05 C-PIBB               PIC 999     VALUE 0.
           05 C-LEMON              PIC 999     VALUE 0.
           05 C-VANILLA            PIC 999     VALUE 0.
           05 C-TOTAL              PIC 9(5)V99 VALUE 0.
           05 C-ATOTAL             PIC 9(5)V99 VALUE 0.
           05 C-BTOTAL             PIC 9(5)V99 VALUE 0.
           05 C-CTOTAL             PIC 9(5)V99 VALUE 0.
           05 C-DTOTAL             PIC 9(5)V99 VALUE 0.
           05 C-ETOTAL             PIC 9(5)V99 VALUE 0.
           05 C-WINNER             PIC 9(5)V99 VALUE 0.
       01 HEADINGS1.
           05 FILLER               PIC X(6)    VALUE 'DATE: '.
           05 H1-DATE.
               10 H1-MONTH         PIC 99.
               10 FILLER           PIC X       VALUE '/'.
               10 H1-DAY           PIC 99.
               10 FILLER           PIC X       VALUE '/'.
               10 H1-YEAR          PIC 9999.
           05 FILLER               PIC X(36)   VALUE SPACES.
           05 FILLER               PIC X(28)   
                   VALUE 'ALBIA SOCCER CLUB FUNDRAISER'.
           05 FILLER               PIC X(43)   VALUE SPACES.
           05 FILLER               PIC X(6)    VALUE 'PAGE: '.
           05 H1-PAGE              PIC Z9.
       01 HEADINGS2.
           05 FILLER               PIC X(59)   VALUE SPACES.
           05 FILLER               PIC X(14)     
                   VALUE 'MOYER DIVISION'.
           05 FILLER               PIC X(59)   VALUE SPACES.
       01 HEADINGS3.
           05 FILLER               PIC X(60)   VALUE SPACES.
           05 FILLER               PIC X(12)   VALUE 'SALES REPORT'.
           05 FILLER               PIC X(60)   VALUE SPACES.
       01 ERR-HEADINGS1.
           05 FILLER               PIC X(6)    VALUE 'DATE: '.
           05 H1-DATE.
               10 H1-MONTH         PIC 99.
               10 FILLER           PIC X       VALUE '/'.
               10 H1-DAY           PIC 99.
               10 FILLER           PIC X       VALUE '/'.
               10 H1-YEAR          PIC 9999.
           05 FILLER               PIC X(36)   VALUE SPACES.
           05 FILLER               PIC X(28)   
                   VALUE 'ALBIA SOCCER CLUB FUNDRAISER'.
           05 FILLER               PIC X(43)   VALUE SPACES.
           05 FILLER               PIC X(6)    VALUE 'PAGE: '.
           05 H1-PAGE              PIC Z9.
       01 ERR-HEADINGS2.
           05 FILLER               PIC X(9)    VALUE ' COBKLM05'.
           05 FILLER               PIC X(50)   VALUE SPACES.
           05 FILLER               PIC X(14)     
                   VALUE 'MOYER DIVISION'.
           05 FILLER               PIC X(59)   VALUE SPACES.
       01 ERR-HEADINGS3.
           05 FILLER               PIC X(60)   VALUE SPACES.
           05 FILLER               PIC X(12)   VALUE 'ERROR REPORT'.
           05 FILLER               PIC X(60)   VALUE SPACES.
       01 COL-HEADINGS.    
           05 FILLER               PIC XXX     VALUE SPACES.
           05 FILLER               PIC X(9)    VALUE 'LAST NAME'.
           05 FILLER               PIC X(8)    VALUE SPACES.
           05 FILLER               PIC X(10)   VALUE 'FIRST NAME'.
           05 FILLER               PIC X(7)    VALUE SPACES.
           05 FILLER               PIC X(4)    VALUE 'CITY'.
           05 FILLER               PIC X(7)    VALUE SPACES.
           05 FILLER               PIC X(5)    VALUE 'STATE'.
           05 FILLER               PIC X       VALUE SPACES.
           05 FILLER               PIC X(8)    VALUE 'ZIP CODE'.
           05 FILLER               PIC X(4)    VALUE SPACES.
           05 FILLER               PIC X(8)    VALUE 'POP TYPE'.
           05 FILLER               PIC X(13)   VALUE SPACES.
           05 FILLER               PIC X(8)    VALUE 'QUANTITY'.
           05 FILLER               PIC X(7)    VALUE SPACES.
           05 FILLER               PIC X(11)   VALUE 'DEPOSIT AMT'.
           05 FILLER               PIC X(6)    VALUE SPACES.
           05 FILLER               PIC X(11)   VALUE 'TOTAL SALES'.
           05 FILLER               PIC XX      VALUE SPACES.
       01 COL-ERR-HEADINGS.
           05 FILLER               PIC X(12)   VALUE 'ERROR RECORD'.
           05 FILLER               PIC X(60)   VALUE SPACES.
           05 FILLER               PIC X(17)
                   VALUE 'ERROR DESCRIPTION'.
           05 FILLER               PIC X(43)   VALUE SPACES.
       01 DETAIL-LINE.
           05 FILLER               PIC XXX     VALUE SPACES.
           05 O-L-NAME             PIC X(15).
           05 FILLER               PIC XX      VALUE SPACES.
           05 O-F-NAME             PIC X(15).
           05 FILLER               PIC XX      VALUE SPACES.
           05 O-CITY               PIC X(10).
           05 FILLER               PIC XXX     VALUE SPACES.
           05 O-ZIP                PIC X(9).
           05 FILLER               PIC XX      VALUE SPACES.
           05 O-POP-TYPE           PIC X(16).
           05 FILLER               PIC X(8)    VALUE SPACES.
           05 O-QTY                PIC Z9.
           05 FILLER               PIC X(11)   VALUE SPACES.
           05 O-DEPOSIT            PIC $$$$V99.
           05 FILLER               PIC X(10)    VALUE SPACES.
           05 O-TOTAL              PIC $$,$$$V99.
       01 GRAND-TOTALS.
           05 FILLER               PIC X(13)   VALUE 'GRAND TOTALS:'.
       01 GRAND-TOTALS2.
           05 FILLER               PIC XXX     VALUE SPACES.
           05 FILLER               PIC X(15)   VALUE 'COKE'.
           05 FILLER               PIC X       VALUE SPACES.
           05 O-COKE               PIC Z(6)9.
           05 FILLER               PIC XXX     VALUE SPACES.
           05 FILLER               PIC X(15)   VALUE 'DIET COKE'.
           05 FILLER               PIC X       VALUE SPACES.
           05 O-DIET-COKE          PIC Z(6)9.
           05 FILLER               PIC XXX     VALUE SPACES.
           05 FILLER               PIC X(15)   VALUE 'MELLO YELLO'.
           05 FILLER               PIC X       VALUE SPACES.
           05 O-MEL-YEL            PIC Z(6)9.
       01 GRAND-TOTALS3.
           05 FILLER               PIC XXX     VALUE SPACES.
           05 FILLER               PIC X(15)   VALUE 'CHERRY COKE'.
           05 FILLER               PIC X       VALUE SPACES.
           05 O-CHERRY             PIC Z(6)9.
           05 FILLER               PIC XXX     VALUE SPACES.
           05 FILLER               PIC X(16)   VALUE 'DIET CHERRY COKE'.
           05 FILLER               PIC X       VALUE SPACES.
           05 O-DIET-CHERRY        PIC Z(6)9.
           05 FILLER               PIC XXX     VALUE SPACES.
           05 FILLER               PIC X(15)   VALUE 'SPRITE'.
           05 FILLER               PIC X       VALUE SPACES.
           05 O-SPRITE        PIC Z(6)9.
       01 GRAND-TOTALS4.
           05 FILLER               PIC XXX     VALUE SPACES.
           05 FILLER               PIC X(15)   VALUE 'DIET SPRITE'.
           05 FILLER               PIC X       VALUE SPACES.
           05 O-DIET-SPRITE        PIC Z(6)9.
           05 FILLER               PIC XXX     VALUE SPACES.
           05 FILLER               PIC X(15)   VALUE 'DASANI'.
           05 FILLER               PIC X       VALUE SPACES.
           05 O-DASANI             PIC Z(6)9.
           05 FILLER               PIC XXX     VALUE SPACES.
           05 FILLER               PIC X(15)   VALUE 'C2'.
           05 FILLER               PIC X       VALUE SPACES.
           05 O-C2                 PIC Z(6)9.
       01 GRAND-TOTALS5.
           05 FILLER               PIC XXX     VALUE SPACES.
           05 FILLER               PIC X(15)   VALUE 'MR. PIBB'.
           05 FILLER               PIC X       VALUE SPACES.
           05 O-PIBB               PIC Z(6)9.
           05 FILLER               PIC XXX     VALUE SPACES.
           05 FILLER               PIC X(15)   VALUE 'DIET LEMON COKE'.
           05 FILLER               PIC X       VALUE SPACES.
           05 O-LEMON              PIC Z(6)9.
           05 FILLER               PIC XXX     VALUE SPACES.
           05 FILLER               PIC X(15)   VALUE 'VANILLA COKE'.
           05 FILLER               PIC X       VALUE SPACES.
           05 O-VANILLA            PIC Z(6)9.
       01 TEAM-LINE.
           05 FILLER               PIC X(13)   VALUE 'TEAM TOTALS:'.
       01 A-LINE.
           05 FILLER               PIC XXX     VALUE SPACES.
           05 FILLER               PIC X       VALUE 'A'.
           05 FILLER               PIC X       VALUE SPACES.
           05 O-A-TOTAL            PIC $(12)V99.
        01 B-LINE.
           05 FILLER               PIC XXX     VALUE SPACES.
           05 FILLER               PIC X       VALUE 'B'.
           05 FILLER               PIC X       VALUE SPACES.
           05 O-B-TOTAL            PIC $(12)V99.
        01 C-LINE.
           05 FILLER               PIC XXX     VALUE SPACES.
           05 FILLER               PIC X       VALUE 'C'.
           05 FILLER               PIC X       VALUE SPACES.
           05 O-C-TOTAL            PIC $(12)V99.
        01 D-LINE.
           05 FILLER               PIC XXX     VALUE SPACES.
           05 FILLER               PIC X       VALUE 'D'.
           05 FILLER               PIC X       VALUE SPACES.
           05 O-D-TOTAL            PIC $(12)V99.
       01 E-LINE.
           05 FILLER               PIC XXX     VALUE SPACES.
           05 FILLER               PIC X       VALUE 'E'.
           05 FILLER               PIC X       VALUE SPACES.
           05 O-E-TOTAL            PIC $(12)V99.
       01 TEAM-LINE2.
           05 FILLER               PIC XXX     VALUE SPACES.
           05 FILLER               PIC X(5)    VALUE 'TEAM '.
           05 O-WINNER             PIC X.
           05 FILLER               PIC X(18)   
                   VALUE ' IS THE WINNER!!!'.
       01 ERR-DETAIL-LINE.
           05 O-RECORD             PIC X(71).
           05 FILLER               PIC X(10)   VALUE SPACES.
           05 O-MSG                PIC X(49).
       01 BLANK-LINE.
           05 FILLER               PIC X(132)  VALUE SPACES.

       procedure division.
       L1-MAIN.
           PERFORM L2-INIT.
           PERFORM L2-MAINLINE
               UNTIL EOF = 'T'.
           PERFORM L2-TOTALS.
           PERFORM L2-CLOSING.

       L2-INIT.
           MOVE FUNCTION CURRENT-DATE TO CUR-DATE.
           MOVE CUR-DAY               TO H1-DAY OF HEADINGS1.
           MOVE CUR-MONTH             TO H1-MONTH OF HEADINGS1.
           MOVE CUR-YEAR              TO H1-YEAR OF HEADINGS1.
           MOVE CUR-DAY               TO H1-DAY OF ERR-HEADINGS1.
           MOVE CUR-MONTH             TO H1-MONTH OF ERR-HEADINGS1.
           MOVE CUR-YEAR              TO H1-YEAR OF ERR-HEADINGS1.
           OPEN INPUT POP-MASTER.
           OPEN OUTPUT VALID-PRTOUT.
           OPEN OUTPUT INVALID-PRTOUT.
           PERFORM L3-READ.
           IF ERRSW EQUALS 'T'
               PERFORM L4-ERROR-HEADINGS
           ELSE 
               PERFORM L4-HEADINGS
           END-IF.

       L2-MAINLINE.
           PERFORM L3-VALIDATE
               THRU L3-VALIDATE-EXIT.
           IF ERRSW EQUALS 'T'
               PERFORM L4-ERROR
           ELSE 
               PERFORM L2-CALCS
               PERFORM L2-MOVES.
          PERFORM L3-READ.

       L2-CALCS.
           EVALUATE P-STATE
               WHEN 'IA'
                  COMPUTE C-DEPOSIT = P-CASES * 24 * .05
               WHEN 'NE'
                   COMPUTE C-DEPOSIT = P-CASES * 24 * .05
               WHEN 'WI'
                   COMPUTE C-DEPOSIT = P-CASES * 24 * .05
               WHEN 'MI'
                   COMPUTE C-DEPOSIT = P-CASES * 24 * .10
               WHEN 'MO'
                   MOVE 0 TO C-DEPOSIT
               WHEN 'IL'
                   MOVE 0 TO C-DEPOSIT
           END-EVALUATE.

           EVALUATE P-POP-TYPE
               WHEN '1'
                   MOVE 'COKE' TO O-POP-TYPE
                   COMPUTE C-COKE = C-COKE + P-CASES
               WHEN '2'
                   MOVE 'DIET COKE' TO O-POP-TYPE
                   COMPUTE C-DIET-COKE = C-DIET-COKE + P-CASES
               WHEN '3'
                   MOVE 'MELLO YELLO' TO O-POP-TYPE
                   COMPUTE C-MELLO-Y = C-MELLO-Y + P-CASES
               WHEN '4'
                   MOVE 'CHERRY COKE'  TO O-POP-TYPE
                   COMPUTE C-CHERRY-COKE = C-CHERRY-COKE + P-CASES
               WHEN '5'
                   MOVE 'DIET CHERRY COKE' TO O-POP-TYPE
                   COMPUTE C-DIET-CHERRY = C-DIET-CHERRY + P-CASES
               WHEN '6'
                   MOVE 'SPRITE' TO O-POP-TYPE
                   COMPUTE C-SPRITE = C-SPRITE + P-CASES
               WHEN '7'
                   MOVE 'DIET SPRITE'  TO O-POP-TYPE
                   COMPUTE C-DIET-SPRITE = C-DIET-SPRITE + P-CASES
               WHEN '8'
                   MOVE 'DASANI' TO O-POP-TYPE
                   COMPUTE C-DASANI = C-DASANI + P-CASES
               WHEN '9'
                   MOVE 'C2' TO O-POP-TYPE
                   COMPUTE C-C2 = C-C2 + P-CASES
               WHEN '10'
                   MOVE 'MR. PIBB' TO O-POP-TYPE
                   COMPUTE C-PIBB = C-PIBB + P-CASES
               WHEN '11'
                   MOVE 'DIET LEMON COKE' TO O-POP-TYPE
                   COMPUTE C-LEMON = C-LEMON + P-CASES
               WHEN '12'
                   MOVE 'VANILLA COKE' TO O-POP-TYPE
                   COMPUTE C-VANILLA = C-VANILLA + P-CASES
           END-EVALUATE.

           COMPUTE C-TOTAL = 18.71 * P-CASES + C-DEPOSIT.
      * MAKE SURE TO CALCUALTE WHICH TEAM SOLD THE MOST!
           EVALUATE P-TEAM
               WHEN 'A'
                   COMPUTE C-ATOTAL = C-ATOTAL + C-TOTAL
                  IF C-ATOTAL > C-WINNER
                      MOVE C-ATOTAL TO C-WINNER
                      MOVE P-TEAM TO O-WINNER
                  END-IF
               WHEN 'B'
                   COMPUTE C-BTOTAL = C-BTOTAL + C-TOTAL
                   IF C-BTOTAL > C-WINNER
                       MOVE C-BTOTAL TO C-WINNER
                       MOVE P-TEAM TO O-WINNER
                   END-IF
               WHEN 'C'
                   COMPUTE C-CTOTAL = C-CTOTAL + C-TOTAL
                   IF C-CTOTAL > C-WINNER
                       MOVE C-CTOTAL TO C-WINNER
                       MOVE P-TEAM TO O-WINNER
                   END-IF
               WHEN 'D'
                   COMPUTE C-DTOTAL = C-DTOTAL + C-TOTAL
                   IF C-DTOTAL > C-WINNER
                       MOVE C-DTOTAL TO C-WINNER
                       MOVE P-TEAM TO O-WINNER
                   END-IF
               WHEN 'E'
                   COMPUTE C-ETOTAL = C-ETOTAL + C-TOTAL
                   IF C-ETOTAL > C-WINNER
                       MOVE C-ETOTAL TO C-WINNER
                       MOVE P-TEAM TO O-WINNER
                   END-IF
           END-EVALUATE.

       L2-MOVES.
           MOVE P-LNAME TO O-L-NAME.
           MOVE P-FNAME TO O-F-NAME.
           MOVE P-CITY TO O-CITY.
           MOVE P-ZIP TO O-ZIP.
           MOVE P-CASES TO O-QTY.
           MOVE C-DEPOSIT TO O-DEPOSIT.
           MOVE C-TOTAL TO O-TOTAL.
           WRITE PRTLINE FROM DETAIL-LINE
               AFTER ADVANCING 1 LINE 
                   AT EOP 
                       WRITE PRTLINE FROM BLANK-LINE
                           PERFORM L4-HEADINGS.


       L2-TOTALS.
           MOVE C-COKE TO O-COKE.
           MOVE C-DIET-COKE TO O-DIET-COKE.
           MOVE C-MELLO-Y TO O-MEL-YEL.
           MOVE C-CHERRY-COKE TO O-CHERRY.
           MOVE C-DIET-CHERRY TO O-DIET-CHERRY.
           MOVE C-SPRITE TO O-SPRITE.
           MOVE C-DIET-SPRITE TO O-DIET-SPRITE.
           MOVE C-DASANI TO O-DASANI.
           MOVE C-C2 TO O-C2.
           MOVE C-PIBB TO O-PIBB.
           MOVE C-LEMON TO O-LEMON.
           MOVE C-VANILLA TO O-VANILLA.
           MOVE C-ATOTAL TO O-A-TOTAL.
           MOVE C-BTOTAL TO O-B-TOTAL.
           MOVE C-CTOTAL TO O-C-TOTAL.
           MOVE C-DTOTAL TO O-D-TOTAL.
           MOVE C-ETOTAL TO O-E-TOTAL.
           WRITE PRTLINE FROM BLANK-LINE.
           WRITE PRTLINE FROM BLANK-LINE.
           WRITE PRTLINE FROM GRAND-TOTALS.
           WRITE PRTLINE FROM BLANK-LINE.
           WRITE PRTLINE FROM GRAND-TOTALS2.
           WRITE PRTLINE FROM BLANK-LINE.
           WRITE PRTLINE FROM GRAND-TOTALS3.
           WRITE PRTLINE FROM BLANK-LINE.
           WRITE PRTLINE FROM GRAND-TOTALS4.
           WRITE PRTLINE FROM GRAND-TOTALS5.
           WRITE PRTLINE FROM BLANK-LINE.
           WRITE PRTLINE FROM BLANK-LINE.
           WRITE PRTLINE FROM BLANK-LINE.
           WRITE PRTLINE FROM TEAM-LINE.
           WRITE PRTLINE FROM BLANK-LINE.
           WRITE PRTLINE FROM A-LINE.
           WRITE PRTLINE FROM BLANK-LINE.
           WRITE PRTLINE FROM B-LINE.
           WRITE PRTLINE FROM BLANK-LINE.
           WRITE PRTLINE FROM C-LINE.
           WRITE PRTLINE FROM BLANK-LINE.
           WRITE PRTLINE FROM D-LINE.
           WRITE PRTLINE FROM BLANK-LINE.
           WRITE PRTLINE FROM E-LINE.
           WRITE PRTLINE FROM BLANK-LINE.
           WRITE PRTLINE FROM TEAM-LINE2.



       L2-CLOSING.
           CLOSE POP-MASTER.
           CLOSE VALID-PRTOUT.
           CLOSE INVALID-PRTOUT.

           STOP RUN.

       L3-READ.
           READ POP-MASTER
               AT END 
                   MOVE 'T' TO EOF.

       L3-VALIDATE.
             MOVE 'T' TO ERRSW.
            IF P-LNAME EQUAL SPACES
               MOVE 'ERROR. LAST NAME REQUIRED' TO O-MSG.
                GO TO L3-VALIDATE-EXIT.
            IF P-FNAME EQUAL SPACES
               MOVE 'ERROR. FIRST NAME REQUIRED' TO O-MSG.
                GO TO L3-VALIDATE-EXIT.
            IF P-ADDRESS EQUAL SPACES 
                MOVE 'ERROR. ADDRESS REQUIRED' TO O-MSG.
                GO TO L3-VALIDATE-EXIT.
            IF P-CITY EQUAL SPACES 
                MOVE 'ERROR. CITY REQUIRED' TO O-MSG.
                GO TO L3-VALIDATE-EXIT.
            IF NOT VALID-STATES
                MOVE 'INVALID STATE' TO O-MSG
                GO TO L3-VALIDATE-EXIT.
            IF P-ZIP NOT NUMERIC 
                MOVE 'ZIP CODE MUST BE NUMERIC' TO O-MSG
                GO TO L3-VALIDATE-EXIT.
            IF NOT VALID-POP
                MOVE 'INVALID POP TYPE' TO O-MSG
                GO TO L3-VALIDATE-EXIT.
            IF P-CASES NOT GREATER THAN 0
                MOVE 'NUMBER OF CASES MUST BE GREATER THAN 0' TO O-MSG
                GO TO L3-VALIDATE-EXIT.
           MOVE 'F' TO ERRSW.
       
       L3-VALIDATE-EXIT.
           EXIT.

       L4-ERROR.
           MOVE I-REC TO O-RECORD.
           WRITE PRINTLINE FROM ERR-DETAIL-LINE
               AFTER ADVANCING 1 LINE. 

       L4-ERROR-HEADINGS.
           ADD 1 TO C-PCTR.
           MOVE C-PCTR TO H1-PAGE of ERR-HEADINGS1.
           WRITE PRINTLINE FROM ERR-HEADINGS1
               AFTER ADVANCING PAGE.
           WRITE PRINTLINE FROM ERR-HEADINGS2
               AFTER ADVANCING 1 LINE.
           WRITE PRINTLINE FROM ERR-HEADINGS3
               AFTER ADVANCING 1 LINE.
           WRITE PRINTLINE FROM BLANK-LINE.
           WRITE PRINTLINE FROM COL-ERR-HEADINGS.
           WRITE PRINTLINE FROM BLANK-LINE.
       
       L4-HEADINGS.
           ADD 1 TO C-PCTR.
           MOVE C-PCTR TO H1-PAGE of HEADINGS1.
           WRITE PRTLINE FROM HEADINGS1
               AFTER ADVANCING PAGE.
           WRITE PRTLINE FROM HEADINGS2
               AFTER ADVANCING 1 LINE.
           WRITE PRTLINE FROM HEADINGS3
               AFTER ADVANCING 1 LINE.
           WRITE PRTLINE FROM BLANK-LINE.
           WRITE PRTLINE FROM COL-HEADINGS.
           WRITE PRTLINE FROM BLANK-LINE.


           

          
       
           
       end program CBLKLM04.