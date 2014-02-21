SUBROUTINE OUTSEL (PRSEL,IMNPRS,INPRS,IPFORM,MESSAG)
  use Module_OutDat
  IMPLICIT NONE

! FORMAL_PARAMETERS:
  INTEGER IMNPRS,INPRS,IPFORM
  CHARACTER*(*) PRSEL,MESSAG
  DIMENSION PRSEL(IMNPRS)

! local variables and function types
  INTEGER IPRS
  LOGICAL TABGEN
  SAVE

! checks
  IF (.NOT.((IPFORM >=  4 .AND. IPFORM <=  6) .OR. &
            (IPFORM >= 14 .AND. IPFORM <= 16))) &
   CALL FATALERR ('OUTSEL', 'Illegal value of IPFORM')
   IF (INPRS.GT.IMNPRS) CALL FATALERR ('OUTSEL', 'illegal value INPRS')

! output table(s) to file
  TABGEN = .FALSE.
  DO IPRS=0,INPRS
     IF (IPRS > 0) THEN
!       check element of output table list
        TABGEN = (PRSEL(IPRS) == '<TABLE>')
        IF (.NOT.TABGEN) CALL OUTDAT (3, 0, PRSEL(IPRS), 0.0)
     END IF

!    write formatted table
     IF (TABGEN.OR.(IPRS == INPRS)) CALL OUTDAT (IPFORM, 0, MESSAG, 0.)
  END DO
END
