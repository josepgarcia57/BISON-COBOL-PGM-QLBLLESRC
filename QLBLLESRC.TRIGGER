       IDENTIFICATION DIVISION.
      *================================================================*
       PROGRAM-ID.    UPDTRG     INITIAL.
       AUTHOR.        PEDRO GARCIA
       INSTALLATION.  BBI SA.
       DATE-WRITTEN.  2017-MAR-13.
     š*----------------------------------------------------------------*
     š*   APLICACO .......: BPC - Centralização de Responsabilidades   *
     š*   OBJECTIVO ......: Este Interface destina-se a Extrair Info   *
     š*                     Estática de Instrumentos e Contratos       *
     š*   ANALISTA .......: PEDRO GARCIA                               *
     š*   PROGRAMADOR.....:                                            *
     š*================================================================*
     š*=                                                           ====*
     š*= This trigger program is called when a record is updated   ====*
     š*= in the ATMTRANS file.                                     ====*
     š*= This program will check the balance of ACCTS and          ====*
     š*= the total amount in ATMS.If either one of the amounts     ====*
     š*= is not enough to meet the withdrawal, an exception        ====*
     š*= message is signalled to the application.                  ====*
     š*= If both ACCTS and ATMS files have enough money, this      ====*
     š*==ATMIDs of 10001 and 10002 will be updated in the ATMTRANS   *=*
     š*==file with the following data:                               *=*
     š*==                                                            *=*
     š*==ATMID   ACCTID   TCODE    AMOUNT                            *=*
     š*==--------------------------------                            *=*
     š*==10001   20001      W       25.00                            *=*
     š*==10002   20002      W      900.00                            *=*
     š*================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-AS400.
       OBJECT-COMPUTER. IBM-AS400.
       SPECIAL-NAMES. I-O-FEEDBACK IS FEEDBACK-JUNK.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACC-FILE ASSIGN TO DATABASE-ACCTS
                  ORGANIZATION IS INDEXED
                  ACCESS IS RANDOM
                  RECORD KEY IS ACCTN
                  FILE STATUS IS STATUS-ERR1.

           SELECT ATM-FILE ASSIGN TO DATABASE-ATMS
                  ORGANIZATION IS INDEXED
                  ACCESS IS RANDOM
                  RECORD KEY IS ATMN
                  FILE STATUS IS STATUS-ERR2.

      *************************************************************
      *                   COMMITMENT CONTROL AREA.                *
      *************************************************************
       I-O-CONTROL.
           COMMITMENT CONTROL FOR ATM-FILE, ACC-FILE.

      *************************************************************
      *                  DATA DIVISION                            *
      *************************************************************

       DATA DIVISION.
       FILE SECTION.
       FD  ATM-FILE
           LABEL RECORDS ARE STANDARD.
       01  ATM-REC.
       COPY DDS-ATMFILE OF ATMS.

       FD  ACC-FILE
           LABEL RECORDS ARE STANDARD.
       01  ACC-REC.
       COPY DDS-ACCFILE OF ACCTS.


      *************************************************************
      *             WORKING-STORAGE SECTION                       *
      *************************************************************
       WORKING-STORAGE SECTION.
       01  STATUS-ERR1         PIC XX.
       01  STATUS-ERR2         PIC XX.
       01  TEMP-PTR USAGE IS POINTER.

       01 NUMBERS-1.
         03 NUM1          PIC 9(10).
         03 NUM2          PIC 9(10).
         03 NUM3          PIC 9(10).

       01  FEEDBACK-STUFF      PIC X(500) VALUE SPACES.

      *************************************************************
      * MESSAGE FOR SIGNALLING ANY TRIGGER ERROR                  *
      * - Define any  message ID and message file in the following*
      *   message data.                                           *
      *************************************************************
       01 SNDPGMMSG-PARMS.
         03 SND-MSG-ID        PIC X(7)    VALUE "TRG9999".
         03 SND-MSG-FILE      PIC X(20)   VALUE "MSGF      LIB1    ".
         03 SND-MSG-DATA      PIC X(25)   VALUE "Trigger Error".
         03 SND-MSG-LEN       PIC 9(8) BINARY  VALUE 25.
         03 SND-MSG-TYPE      PIC X(10)   VALUE "*ESCAPE   ".
         03 SND-PGM-QUEUE     PIC X(10)   VALUE "*     ".
         03 SND-PGM-STACK-CNT PIC 9(8) BINARY  VALUE 1.
         03 SND-MSG-KEY       PIC X(4)    VALUE "    ".
         03 SND-ERROR-CODE.
           05 PROVIDED          PIC 9(8) BINARY VALUE 66.
           05 AVAILABLE         PIC 9(8) BINARY VALUE 0.
           05 RTN-MSG-ID        PIC X(7) VALUE " ".
           05 FILLER            PIC X(1) VALUE " ".
           05 RTN-DATA          PIC X(50) VALUE " ".

      *************************************************************
      *             LINKAGE         SECTION                       *
      *  PARM 1 is the trigger buffer                             *
      *  PARM 2 is the length of the trigger buffer               *
      *************************************************************
       LINKAGE SECTION.
       01 PARM-1-AREA.
         03 FILE-NAME        PIC X(10).
         03 LIB-NAME         PIC X(10).
         03 MEM-NAME         PIC X(10).
         03 TRG-EVENT        PIC X.
         03 TRG-TIME         PIC X.
         03 CMT-LCK-LVL      PIC X.
         03 FILLER           PIC X(3).
         03 DATA-AREA-CCSID  PIC 9(8)  BINARY.
         03 FILLER           PIC X(8).
         03 DATA-OFFSET.
           05 OLD-REC-OFF      PIC 9(8)  BINARY.
           05 OLD-REC-LEN      PIC 9(8)  BINARY.
           05 OLD-REC-NULL-MAP PIC 9(8)  BINARY.
           05 OLD-REC-NULL-LEN PIC 9(8)  BINARY.
           05 NEW-REC-OFF      PIC 9(8)  BINARY.
           05 NEW-REC-LEN      PIC 9(8)  BINARY.
           05 NEW-REC-NULL-MAP PIC 9(8)  BINARY.
           05 NEW-REC-NULL-LEN PIC 9(8)  BINARY.
           05 FILLER           PIC X(16).
         03 RECORD-JUNK.
           05 OLD-RECORD       PIC X(16).
           05 OLD-NULL-MAP     PIC X(4).
           05 NEW-RECORD       PIC X(16).
           05 NEW-NULL-MAP     PIC X(4).

         01 PARM-2-AREA.
         03  TRGBUFL             PIC X(2).

         01 INPUT-RECORD2.
         COPY DDS-TRANS OF ATMTRANS.

         05 OFFSET-NEW-REC2   PIC 9(8)  BINARY.

*****************************************************************
******                   PROCEDURE DIVISION                     *
*****************************************************************
       PROCEDURE DIVISION USING PARM-1-AREA, PARM-2-AREA.
       MAIN-PROGRAM SECTION.
       000-MAIN-PROGRAM.
           OPEN I-O ATM-FILE.
           OPEN I-O ACC-FILE.

           MOVE 0 TO BAL.

      *************************************************************
      * SET UP THE OFFSET POINTER AND COPY THE NEW RECORD.        *
      *************************************************************
           SET TEMP-PTR TO ADDRESS OF PARM-1-AREA.
           SET TEMP-PTR UP BY NEW-REC-OFFSET.
           SET ADDRESS OF INPUT-RECORD2 TO TEMP-PTR.
           MOVE INPUT-RECORD2 TO INPUT-RECORD.

      ************************************************************
      * READ THE RECORD FROM THE ACCTS FILE                      *
      ************************************************************
           MOVE ACCTID TO ACCTN.
           READ ACC-FILE
                INVALID KEY PERFORM 900-OOPS
                NOT INVALID KEY PERFORM 500-ADJUST-ACCOUNT.

      *************************************************************
      * READ THE RECORD FROM THE ATMS FILE.                       *
      *************************************************************
           MOVE ATMID TO ATMN.
           READ ATM-FILE
                INVALID KEY PERFORM 950-OOPS
                NOT INVALID KEY PERFORM 550-ADJUST-ATM-BAL.
           CLOSE ATM-FILE.
           CLOSE ACC-FILE.
           GOBACK.

*******************************************************************
*******************************************************************
*******THIS PROCEDURE IS USED IF THERE IS NOT ENOUGH MONEY IN THE  ****
*******ACCTS FOR THE WITHDRAWAL.                                    ****
*******************************************************************
       200-NOT-ENOUGH-IN-ACC.
           DISPLAY "NOT ENOUGH MONEY IN ACCOUNT.".
           CLOSE ATM-FILE.
           CLOSE ACC-FILE.
           PERFORM 999-SIGNAL-ESCAPE.
           GOBACK.

*******************************************************************
*******THIS PROCEDURE IS USED IF THERE IS NOT ENOUGH MONEY IN THE
*******ATMS FOR THE WITHDRAWAL.
*******************************************************************
       250-NOT-ENOUGH-IN-ATM.
           DISPLAY "NOT ENOUGH MONEY IN ATM.".
           CLOSE ATM-FILE.
           CLOSE ACC-FILE.
           PERFORM 999-SIGNAL-ESCAPE.
           GOBACK.

*******************************************************************
*******THIS PROCEDURE IS USED TO ADJUST THE BALANCE FOR THE ACCOUNT OF
*******THE PERSON WHO PERFORMED THE TRANSACTION.
*******************************************************************
       500-ADJUST-ACCOUNT.
           IF TCODE = "W" THEN
              IF (BAL < AMOUNT) THEN
                 PERFORM 200-NOT-ENOUGH-IN-ACC
              ELSE
                 SUBTRACT AMOUNT FROM BAL
                 REWRITE  ACC-REC
           ELSE IF TCODE = "D" THEN
              ADD AMOUNT TO BAL
              REWRITE  ACC-REC
           ELSE DISPLAY "TRANSACTION CODE ERROR, CODE IS: ", TCODE.

*******************************************************************
*******THIS PROCEDURE IS USED TO ADJUST THE BALANCE OF THE ATM FILE ***
*******FOR THE AMOUNT OF MONEY IN ATM AFTER A TRANSACTION.          ***
*******************************************************************
       550-ADJUST-ATM-BAL.
           IF TCODE = "W" THEN
              IF (ATMAMT < AMOUNT) THEN
                 PERFORM 250-NOT-ENOUGH-IN-ATM
              ELSE
                 SUBTRACT AMOUNT FROM ATMAMT
                 REWRITE  ATM-REC
           ELSE IF TCODE = "D" THEN
              ADD AMOUNT TO ATMAMT
              REWRITE  ATM-REC
           ELSE DISPLAY "TRANSACTION CODE ERROR, CODE IS: ", TCODE.

************************************************************  *******
*******THIS PROCEDURE IS USED IF THERE THE KEY VALUE THAT IS USED IS **
*******NOT FOUND IN THE ACCTS FILE.                                  **
*******************************************************************
       900-OOPS.
           DISPLAY "INVALID KEY: ", ACCTN, "  ACCOUNT FILE STATUS: ",
                    STATUS-ERR1.
           CLOSE ATM-FILE.
           CLOSE ACC-FILE.
           PERFORM 999-SIGNAL-ESCAPE.
           GOBACK.

*******************************************************************
*******THIS PROCEDURE IS USED IF THERE THE KEY VALUE THAT IS USED IS **
*******NOT FOUND IN THE ATM FILE.                                    **
*******************************************************************
       950-OOPS.
           DISPLAY "INVALID KEY: ", ATMN, "  ATM FILE STATUS: ",
                    STATUS-ERR2.
           CLOSE ATM-FILE.
           CLOSE ACC-FILE.
           PERFORM 999-SIGNAL-ESCAPE.
           GOBACK.

*******************************************************************
*******SIGNAL ESCAPE TO THE APPLICATION                        ********
*******************************************************************
       999-SIGNAL-ESCAPE.

           CALL "QMHSNDPM" USING SND-MSG-ID,
                                 SND-MSG-FILE,
                                 SND-MSG-DATA,
                                 SND-MSG-LEN,
                                 SND-MSG-TYPE,
                                 SND-PGM-QUEUE,
                                 SND-PGM-STACK-CNT,
                                 SND-MSG-KEY,
                                 SND-ERROR-CODE.
      *DISPLAY RTN-MSG-ID.
      *DISPLAY RTN-DATA.
