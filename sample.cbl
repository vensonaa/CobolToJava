IDENTIFICATION DIVISION.
       PROGRAM-ID. ORDER-PROCESSOR.
       AUTHOR. LEGACY-SYSTEM.
       DATE-WRITTEN. 01/15/1995.
       DATE-COMPILED. 08/12/2025.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-MAINFRAME.
       OBJECT-COMPUTER. IBM-MAINFRAME.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE ASSIGN TO 'CUSTMAST'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CUST-ID
               FILE STATUS IS WS-CUST-STATUS.
           
           SELECT ORDER-FILE ASSIGN TO 'ORDERINP'
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-ORDER-STATUS.
           
           SELECT REPORT-FILE ASSIGN TO 'RPTOUT'
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-REPORT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       
       FD  CUSTOMER-FILE
           RECORD CONTAINS 200 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  CUSTOMER-RECORD.
           05 CUST-ID                  PIC X(10).
           05 CUST-NAME                PIC X(30).
           05 CUST-ADDRESS             PIC X(50).
           05 CUST-CREDIT-LIMIT        PIC 9(7)V99.
           05 CUST-CURRENT-BALANCE     PIC S9(7)V99.
           05 CUST-DISCOUNT-RATE       PIC V999.
           05 CUST-STATUS              PIC X(1).
           05 CUST-LAST-ORDER-DATE     PIC 9(8).
           05 FILLER                   PIC X(90).

       FD  ORDER-FILE
           RECORD CONTAINS 150 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  ORDER-RECORD.
           05 ORD-CUSTOMER-ID          PIC X(10).
           05 ORD-ORDER-NUMBER         PIC X(12).
           05 ORD-ORDER-DATE           PIC 9(8).
           05 ORD-ITEM-COUNT           PIC 99.
           05 ORD-ITEMS OCCURS 5 TIMES.
              10 ORD-ITEM-CODE         PIC X(8).
              10 ORD-ITEM-QTY          PIC 9(5).
              10 ORD-ITEM-PRICE        PIC 9(5)V99.
           05 FILLER                   PIC X(18).

       FD  REPORT-FILE
           RECORD CONTAINS 132 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-RECORD               PIC X(132).

       WORKING-STORAGE SECTION.
       
       01  WS-FILE-STATUS.
           05 WS-CUST-STATUS           PIC XX.
           05 WS-ORDER-STATUS          PIC XX.
           05 WS-REPORT-STATUS         PIC XX.

       01  WS-COUNTERS.
           05 WS-ORDERS-PROCESSED      PIC 9(6) VALUE ZERO.
           05 WS-ORDERS-REJECTED       PIC 9(6) VALUE ZERO.
           05 WS-TOTAL-REVENUE         PIC 9(9)V99 VALUE ZERO.
           05 WS-REPORT-LINE-COUNT     PIC 99 VALUE ZERO.

       01  WS-CALCULATION-FIELDS.
           05 WS-ORDER-TOTAL           PIC 9(7)V99.
           05 WS-DISCOUNT-AMOUNT       PIC 9(7)V99.
           05 WS-NET-AMOUNT            PIC 9(7)V99.
           05 WS-TAX-AMOUNT            PIC 9(7)V99.
           05 WS-FINAL-AMOUNT          PIC 9(7)V99.
           05 WS-NEW-BALANCE           PIC S9(7)V99.
           05 WS-AVAILABLE-CREDIT      PIC S9(7)V99.

       01  WS-CONSTANTS.
           05 WS-TAX-RATE              PIC V999 VALUE .085.
           05 WS-MAX-LINES-PER-PAGE    PIC 99 VALUE 55.

       01  WS-DATE-FIELDS.
           05 WS-CURRENT-DATE.
              10 WS-CURR-YEAR          PIC 9(4).
              10 WS-CURR-MONTH         PIC 99.
              10 WS-CURR-DAY           PIC 99.
           05 WS-FORMATTED-DATE        PIC X(10).

       01  WS-FLAGS.
           05 WS-END-OF-FILE-SW        PIC X VALUE 'N'.
              88 END-OF-FILE           VALUE 'Y'.
           05 WS-CUSTOMER-FOUND-SW     PIC X VALUE 'N'.
              88 CUSTOMER-FOUND        VALUE 'Y'.
           05 WS-ORDER-VALID-SW        PIC X VALUE 'Y'.
              88 ORDER-VALID           VALUE 'Y'.

       01  WS-REPORT-HEADERS.
           05 WS-HEADER-1.
              10 FILLER                PIC X(40) VALUE SPACES.
              10 FILLER                PIC X(25) 
                 VALUE 'ORDER PROCESSING REPORT'.
              10 FILLER                PIC X(67) VALUE SPACES.
           05 WS-HEADER-2.
              10 FILLER                PIC X(10) VALUE 'ORDER NO.'.
              10 FILLER                PIC X(15) VALUE 'CUSTOMER ID'.
              10 FILLER                PIC X(25) VALUE 'CUSTOMER NAME'.
              10 FILLER                PIC X(15) VALUE 'ORDER TOTAL'.
              10 FILLER                PIC X(15) VALUE 'DISCOUNT'.
              10 FILLER                PIC X(15) VALUE 'NET AMOUNT'.
              10 FILLER                PIC X(37) VALUE 'STATUS'.

       01  WS-DETAIL-LINE.
           05 WS-DTL-ORDER-NO          PIC X(12).
           05 FILLER                   PIC X(3) VALUE SPACES.
           05 WS-DTL-CUST-ID           PIC X(10).
           05 FILLER                   PIC X(5) VALUE SPACES.
           05 WS-DTL-CUST-NAME         PIC X(20).
           05 FILLER                   PIC X(5) VALUE SPACES.
           05 WS-DTL-ORDER-TOTAL       PIC $$,$$$,$$9.99.
           05 FILLER                   PIC X(3) VALUE SPACES.
           05 WS-DTL-DISCOUNT          PIC $$,$$$,$$9.99.
           05 FILLER                   PIC X(3) VALUE SPACES.
           05 WS-DTL-NET-AMOUNT        PIC $$,$$$,$$9.99.
           05 FILLER                   PIC X(3) VALUE SPACES.
           05 WS-DTL-STATUS            PIC X(20).
           05 FILLER                   PIC X(20) VALUE SPACES.

       PROCEDURE DIVISION.
       
       0000-MAIN-PROCESS.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-ORDERS UNTIL END-OF-FILE
           PERFORM 3000-FINALIZE
           STOP RUN.

       1000-INITIALIZE.
           OPEN INPUT ORDER-FILE
           OPEN I-O CUSTOMER-FILE
           OPEN OUTPUT REPORT-FILE
           
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           PERFORM 1100-FORMAT-DATE
           PERFORM 1200-PRINT-HEADERS
           PERFORM 1300-READ-FIRST-ORDER.

       1100-FORMAT-DATE.
           MOVE WS-CURR-MONTH TO WS-FORMATTED-DATE(1:2)
           MOVE '/' TO WS-FORMATTED-DATE(3:1)
           MOVE WS-CURR-DAY TO WS-FORMATTED-DATE(4:2)
           MOVE '/' TO WS-FORMATTED-DATE(6:1)
           MOVE WS-CURR-YEAR TO WS-FORMATTED-DATE(7:4).

       1200-PRINT-HEADERS.
           MOVE WS-HEADER-1 TO REPORT-RECORD
           WRITE REPORT-RECORD
           MOVE SPACES TO REPORT-RECORD
           WRITE REPORT-RECORD
           MOVE WS-HEADER-2 TO REPORT-RECORD
           WRITE REPORT-RECORD
           MOVE ALL '-' TO REPORT-RECORD
           WRITE REPORT-RECORD
           MOVE 4 TO WS-REPORT-LINE-COUNT.

       1300-READ-FIRST-ORDER.
           READ ORDER-FILE
               AT END MOVE 'Y' TO WS-END-OF-FILE-SW
               NOT AT END CONTINUE
           END-READ.

       2000-PROCESS-ORDERS.
           PERFORM 2100-VALIDATE-ORDER
           IF ORDER-VALID
               PERFORM 2200-LOOKUP-CUSTOMER
               IF CUSTOMER-FOUND
                   PERFORM 2300-CALCULATE-ORDER
                   PERFORM 2400-CHECK-CREDIT-LIMIT
                   IF ORDER-VALID
                       PERFORM 2500-UPDATE-CUSTOMER
                       PERFORM 2600-PRINT-DETAIL-APPROVED
                       ADD 1 TO WS-ORDERS-PROCESSED
                       ADD WS-FINAL-AMOUNT TO WS-TOTAL-REVENUE
                   ELSE
                       PERFORM 2700-PRINT-DETAIL-REJECTED
                       ADD 1 TO WS-ORDERS-REJECTED
                   END-IF
               ELSE
                   PERFORM 2700-PRINT-DETAIL-REJECTED
                   ADD 1 TO WS-ORDERS-REJECTED
               END-IF
           ELSE
               PERFORM 2700-PRINT-DETAIL-REJECTED
               ADD 1 TO WS-ORDERS-REJECTED
           END-IF
           
           PERFORM 2800-READ-NEXT-ORDER.

       2100-VALIDATE-ORDER.
           MOVE 'Y' TO WS-ORDER-VALID-SW
           
           IF ORD-CUSTOMER-ID = SPACES OR LOW-VALUES
               MOVE 'N' TO WS-ORDER-VALID-SW
           END-IF
           
           IF ORD-ORDER-NUMBER = SPACES OR LOW-VALUES
               MOVE 'N' TO WS-ORDER-VALID-SW
           END-IF
           
           IF ORD-ITEM-COUNT = ZERO OR ORD-ITEM-COUNT > 5
               MOVE 'N' TO WS-ORDER-VALID-SW
           END-IF.

       2200-LOOKUP-CUSTOMER.
           MOVE 'N' TO WS-CUSTOMER-FOUND-SW
           MOVE ORD-CUSTOMER-ID TO CUST-ID
           
           READ CUSTOMER-FILE
               KEY IS CUST-ID
               INVALID KEY CONTINUE
               NOT INVALID KEY
                   IF CUST-STATUS = 'A'
                       MOVE 'Y' TO WS-CUSTOMER-FOUND-SW
                   END-IF
           END-READ.

       2300-CALCULATE-ORDER.
           MOVE ZERO TO WS-ORDER-TOTAL
           
           PERFORM VARYING WS-ITEM-SUB FROM 1 BY 1
               UNTIL WS-ITEM-SUB > ORD-ITEM-COUNT
               COMPUTE WS-ORDER-TOTAL = WS-ORDER-TOTAL +
                   (ORD-ITEM-QTY(WS-ITEM-SUB) * 
                    ORD-ITEM-PRICE(WS-ITEM-SUB))
           END-PERFORM
           
           COMPUTE WS-DISCOUNT-AMOUNT = 
               WS-ORDER-TOTAL * CUST-DISCOUNT-RATE
           
           COMPUTE WS-NET-AMOUNT = 
               WS-ORDER-TOTAL - WS-DISCOUNT-AMOUNT
           
           COMPUTE WS-TAX-AMOUNT = WS-NET-AMOUNT * WS-TAX-RATE
           
           COMPUTE WS-FINAL-AMOUNT = WS-NET-AMOUNT + WS-TAX-AMOUNT.

       01  WS-ITEM-SUB                 PIC 99.

       2400-CHECK-CREDIT-LIMIT.
           COMPUTE WS-NEW-BALANCE = 
               CUST-CURRENT-BALANCE + WS-FINAL-AMOUNT
           
           COMPUTE WS-AVAILABLE-CREDIT = 
               CUST-CREDIT-LIMIT - WS-NEW-BALANCE
           
           IF WS-AVAILABLE-CREDIT < ZERO
               MOVE 'N' TO WS-ORDER-VALID-SW
           END-IF.

       2500-UPDATE-CUSTOMER.
           MOVE WS-NEW-BALANCE TO CUST-CURRENT-BALANCE
           MOVE ORD-ORDER-DATE TO CUST-LAST-ORDER-DATE
           
           REWRITE CUSTOMER-RECORD
               INVALID KEY DISPLAY 'ERROR UPDATING CUSTOMER: ' CUST-ID
           END-REWRITE.

       2600-PRINT-DETAIL-APPROVED.
           PERFORM 2900-CHECK-PAGE-BREAK
           MOVE ORD-ORDER-NUMBER TO WS-DTL-ORDER-NO
           MOVE ORD-CUSTOMER-ID TO WS-DTL-CUST-ID
           MOVE CUST-NAME(1:20) TO WS-DTL-CUST-NAME
           MOVE WS-ORDER-TOTAL TO WS-DTL-ORDER-TOTAL
           MOVE WS-DISCOUNT-AMOUNT TO WS-DTL-DISCOUNT
           MOVE WS-FINAL-AMOUNT TO WS-DTL-NET-AMOUNT
           MOVE 'APPROVED' TO WS-DTL-STATUS
           MOVE WS-DETAIL-LINE TO REPORT-RECORD
           WRITE REPORT-RECORD
           ADD 1 TO WS-REPORT-LINE-COUNT.

       2700-PRINT-DETAIL-REJECTED.
           PERFORM 2900-CHECK-PAGE-BREAK
           MOVE ORD-ORDER-NUMBER TO WS-DTL-ORDER-NO
           MOVE ORD-CUSTOMER-ID TO WS-DTL-CUST-ID
           IF CUSTOMER-FOUND
               MOVE CUST-NAME(1:20) TO WS-DTL-CUST-NAME
           ELSE
               MOVE 'UNKNOWN CUSTOMER' TO WS-DTL-CUST-NAME
           END-IF
           MOVE ZERO TO WS-DTL-ORDER-TOTAL
           MOVE ZERO TO WS-DTL-DISCOUNT
           MOVE ZERO TO WS-DTL-NET-AMOUNT
           MOVE 'REJECTED' TO WS-DTL-STATUS
           MOVE WS-DETAIL-LINE TO REPORT-RECORD
           WRITE REPORT-RECORD
           ADD 1 TO WS-REPORT-LINE-COUNT.

       2800-READ-NEXT-ORDER.
           READ ORDER-FILE
               AT END MOVE 'Y' TO WS-END-OF-FILE-SW
               NOT AT END CONTINUE
           END-READ.

       2900-CHECK-PAGE-BREAK.
           IF WS-REPORT-LINE-COUNT > WS-MAX-LINES-PER-PAGE
               PERFORM 2950-NEW-PAGE
           END-IF.

       2950-NEW-PAGE.
           MOVE SPACES TO REPORT-RECORD
           WRITE REPORT-RECORD AFTER ADVANCING PAGE
           PERFORM 1200-PRINT-HEADERS.

       3000-FINALIZE.
           PERFORM 3100-PRINT-SUMMARY
           CLOSE ORDER-FILE
           CLOSE CUSTOMER-FILE  
           CLOSE REPORT-FILE.

       3100-PRINT-SUMMARY.
           MOVE SPACES TO REPORT-RECORD
           WRITE REPORT-RECORD
           WRITE REPORT-RECORD
           
           STRING 'TOTAL ORDERS PROCESSED: ' 
                  WS-ORDERS-PROCESSED
                  DELIMITED BY SIZE
                  INTO REPORT-RECORD
           WRITE REPORT-RECORD
           
           STRING 'TOTAL ORDERS REJECTED:  ' 
                  WS-ORDERS-REJECTED
                  DELIMITED BY SIZE
                  INTO REPORT-RECORD
           WRITE REPORT-RECORD
           
           STRING 'TOTAL REVENUE:          $' 
                  WS-TOTAL-REVENUE
                  DELIMITED BY SIZE
                  INTO REPORT-RECORD
           WRITE REPORT-RECORD.
