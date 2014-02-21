      SUBROUTINE RDFDOR (XNAME,XMIN,XMAX,X,ILDEC,IVALS)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      DOUBLE PRECISION X,XMIN,XMAX
      INTEGER ILDEC,IVALS
      CHARACTER*(*) XNAME
      DIMENSION X(ILDEC)

**    local variables ; dummy set
      DOUBLE PRECISION      DM
      REAL             R(1),RM
      INTEGER          I(1),IM
      CHARACTER*80     C(1),CM
      LOGICAL          L(1),LM

*     include files
      INCLUDE 'rdstainf.inc'

*     other
      INTEGER IS,I1,ILX
      DOUBLE PRECISION XL
      CHARACTER Q*1, LXNAME*31
      SAVE

      DATA Q /''''/

      CALL RDDATA (7,'RDFDOR',0,0,' ',IS,XNAME,'D',
     $             X,R,I,C,L,ILDEC,1,1,1,1,IVALS,
     $             DM,RM,IM,CM,LM)

      LXNAME = XNAME
      ILX    = LEN_TRIM (LXNAME)

      DO 10 I1=1,IVALS
         XL = X(I1)
         IF (XMAX.LT.XMIN) THEN
            IF (TOSCR) WRITE (*,'(1X,4A)')
     &        'ERROR in RDFDOR: Range not valid of identifier ',
     &         Q,LXNAME(1:ILX),Q
            IF (TOLOG) WRITE (IULOG,'(1X,4A)')
     &        'ERROR in RDFDOR: Range not valid of identifier ',
     &         Q,LXNAME(1:ILX),Q
            CALL FATALERR (' ',' ')
         ELSE IF ((XL.LT.XMIN.OR.XL.GT.XMAX).AND.XL.NE.DM) THEN
            IF (TOSCR) WRITE (*,
     &        '(1X,4A,I3,3A,/,T19,A,G12.5,A,G12.5,A,G12.5,A)')
     &        'ERROR in RDFDOR: Range error of identifier ',
     &         Q,LXNAME(1:ILX),'(',I1,')',Q,',',
     &        'value =',XL,', range = [',XMIN,',',XMAX,']'
            IF (TOLOG) WRITE (IULOG,
     &        '(1X,4A,I3,3A,/,T19,A,G12.5,A,G12.5,A,G12.5,A)')
     &        'ERROR in RDFDOR: Range error of identifier ',
     &         Q,LXNAME(1:ILX),'(',I1,')',Q,',',
     &        'value =',XL,', range = [',XMIN,',',XMAX,']'
            CALL FATALERR (' ',' ')
         END IF
10    CONTINUE

      RETURN
      END
