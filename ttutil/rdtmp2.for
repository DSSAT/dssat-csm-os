      SUBROUTINE RDTMP2 (ITASK,IUNIT,NAMLIS,VARTYP,VARRAY,ILIND,
     $                   IARLEN,INDPNT,ILPNT,INFND,INSETS)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      INTEGER ITASK,IUNIT,ILIND,IARLEN,INDPNT,ILPNT,INFND,INSETS
      CHARACTER NAMLIS*(*),VARTYP*1,VARRAY*1
      DIMENSION NAMLIS(ILIND),VARTYP(ILIND),VARRAY(ILIND)
      DIMENSION IARLEN(ILPNT),INDPNT(ILPNT)

**    local variables and function types
      INTEGER INL,IREC,ILBUF,IIL,INFLD,J,J1,J2
      SAVE

*     length of variable name
      INL = LEN (NAMLIS(1))

      IF (ITASK.EQ.1) THEN
*        read last record used from record 1
         READ (IUNIT,REC=1) IREC,ILBUF,IIL

*        write header of temporary with last datarecord used
         WRITE (IUNIT,REC=1) IREC,ILBUF,IIL,INFND,INSETS
         INFLD = INSETS * INFND

*        write variable types and names
         J2 = 0
10       IF (J2.LT.INFND) THEN
            J1 = J2 + 1
            J2 = MIN (J2+ILBUF/(2+INL), INFND)
            IREC = IREC + 1
            WRITE (IUNIT,REC=IREC)
     $       (VARTYP(J),VARRAY(J),NAMLIS(J),J=J1,J2)
         GOTO 10
         END IF

*        write array length and pointer for all fields
         J2 = 0
20       IF (J2.LT.INFLD) THEN
            J1 = J2 + 1
            J2 = MIN (J2+ILBUF/(2*IIL), INFLD)
            IREC = IREC + 1
            WRITE (IUNIT,REC=IREC) (IARLEN(J),INDPNT(J),J=J1,J2)
         GOTO 20
         END IF


      ELSE IF (ITASK.EQ.2) THEN
*        read index from file ; recover pointer info from first record
         READ (IUNIT,REC=1) IREC,ILBUF,IIL,INFND,INSETS
         INFLD = INSETS * INFND

*        read variable types and names
         J2 = 0
30       IF (J2.LT.INFND) THEN
            J1 = J2 + 1
            J2 = MIN (J2+ILBUF/(2+INL), INFND)
            IREC = IREC + 1
            READ (IUNIT,REC=IREC)
     $       (VARTYP(J),VARRAY(J),NAMLIS(J),J=J1,J2)
         GOTO 30
         END IF

*        read array lengths and pointers
         J2 = 0
40       IF (J2.LT.INFLD) THEN
            J1 = J2 + 1
            J2 = MIN (J2+ILBUF/(2*IIL), INFLD)
            IREC = IREC + 1
            READ (IUNIT,REC=IREC) (IARLEN(J),INDPNT(J),J=J1,J2)
         GOTO 40
         END IF
      END IF


      RETURN
      END
