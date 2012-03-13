      SUBROUTINE RDARER (XNAME,XMIN,XMAX,X,ILDEC,IFND)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      INTEGER ILDEC,IFND
      REAL X,XMIN,XMAX
      CHARACTER*(*) XNAME
      DIMENSION X(ILDEC)

**    local variables ; dummy set
      DOUBLE PRECISION D(1),DM
      REAL                  RM
      INTEGER          I(1),IM
      CHARACTER*80     C(1),CM
      LOGICAL          L(1),LM

*     include files
      INCLUDE 'rdstainf.inc'

*     other
      INTEGER IS,I1,ILX
      REAL XL
      CHARACTER Q*1, LXNAME*31
      SAVE

      DATA Q /''''/

*     the number of values is set by RDDATA
      CALL RDDATA (6,'RDARER',0,0,' ',IS,XNAME,'R',
     $             D,X,I,C,L,1,ILDEC,1,1,1,IFND,
     $             DM,RM,IM,CM,LM)

      LXNAME = XNAME
      ILX    = LEN_TRIM (LXNAME)

      DO 10 I1=1,IFND
         XL = X(I1)
         IF (XMAX.LT.XMIN) THEN
            IF (TOSCR) WRITE (*,'(1X,4A)')
     &        'ERROR in RDARER: Range not valid of identifier ',
     &         Q,LXNAME(1:ILX),Q
            IF (TOLOG) WRITE (IULOG,'(1X,4A)')
     &        'ERROR in RDARER: Range not valid of identifier ',
     &         Q,LXNAME(1:ILX),Q
            CALL FATALERR (' ',' ')
         ELSE IF ((XL.LT.XMIN.OR.XL.GT.XMAX).AND.XL.NE.RM) THEN
            IF (TOSCR) WRITE (*,
     &        '(1X,4A,I3,3A,/,T19,A,G12.5,A,G12.5,A,G12.5,A)')
     &        'ERROR in RDARER: Range error of identifier ',
     &         Q,LXNAME(1:ILX),'(',I1,')',Q,',',
     &        'value =',XL,', range = [',XMIN,',',XMAX,']'
            IF (TOLOG) WRITE (IULOG,
     &        '(1X,4A,I3,3A,/,T19,A,G12.5,A,G12.5,A,G12.5,A)')
     &        'ERROR in RDARER: Range error of identifier ',
     &         Q,LXNAME(1:ILX),'(',I1,')',Q,',',
     &        'value =',XL,', range = [',XMIN,',',XMAX,']'
            CALL FATALERR (' ',' ')
         END IF
10    CONTINUE

      RETURN
      END
