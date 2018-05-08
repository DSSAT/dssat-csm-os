      SUBROUTINE RDSETS (IUNIT,IULOGx,SETFIL,INS)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      INTEGER IUNIT,IULOGx,INS
      CHARACTER*(*) SETFIL

**    local variables ; dummy set
      DOUBLE PRECISION D(1),DM
      REAL             R(1),RM
      INTEGER          I(1),IM
      CHARACTER*80     C(1),CM
      LOGICAL          L(1),LM

*     include files
      INCLUDE 'rdstainf.inc'

*     other
      INTEGER IL,IS,ILF
      CHARACTER*132 SCRREC
      LOGICAL WGIVEN
      INTEGER SCRREC_L
      SAVE

      DATA WGIVEN /.FALSE./

*     analyse rerun file
      IL = 0
      CALL RDDATA (1,'RDSETS',IUNIT,IULOGx,SETFIL,INS,' ',' ',
     $             D,R,I,C,L,1,1,1,1,1,IL,
     $             DM,RM,IM,CM,LM)

*     message to AMBUSY ; set 0 selected
      IS = 0
      CALL AMBUSY (1,'RDFROM',IS)

*     messages to screen
      IF (IULOG.GT.0 .AND. INS.GT.0) THEN
         ILF = LEN_TRIM (SETFIL)
         IF (TOSCR) WRITE (*,'(1X,A)')
     $    'Message from RDSETS: A logfile report is written'
         IF (TOLOG) WRITE (IULOG,'(1X,A)')
     $    'Message from RDSETS: A logfile report is written'
         SCRREC_L = 21
         SCRREC = 'about the use of the '
         CALL ADDINT (SCRREC,SCRREC_L,INS)
         CALL ADDSTF (SCRREC,SCRREC_L,' parameter sets on ')
         CALL ADDSTR (SCRREC,SCRREC_L,SETFIL(1:ILF))
         IF (TOSCR) WRITE (*,'(1X,A)') SCRREC(1:SCRREC_L)
         IF (TOLOG) WRITE (IULOG,'(1X,A)') SCRREC(1:SCRREC_L)
         WGIVEN = .FALSE.
      ELSE IF (INS.GT.0.AND..NOT.WGIVEN) THEN
         IF (TOSCR) WRITE (*,'(1X,A)')
     $    'WARNING from RDSETS: no logfile is used !!'
         IF (TOLOG) WRITE (IULOG,'(1X,A)')
     $    'WARNING from RDSETS: no logfile is used !!'
         WGIVEN = .TRUE.
      END IF

      RETURN
      END
