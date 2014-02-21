      SUBROUTINE RDSDOR (XNAME,XMIN,XMAX,X)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      DOUBLE PRECISION X, XMIN, XMAX
      CHARACTER*(*) XNAME

**    local variables ; dummy set
      DOUBLE PRECISION D(1),DM
      REAL             R(1),RM
      INTEGER          I(1),IM
      CHARACTER*80     C(1),CM
      LOGICAL          L(1),LM

*     include files
      INCLUDE 'rdstainf.inc'

*     other
      INTEGER IL,IS,ILX
      CHARACTER Q*1,LXNAME*31
      SAVE

      DATA Q /''''/

*     read a single DOUBLE PRECISION number
*     (value of IL is arbitrary) !!
      IL = 0
      CALL RDDATA (5,'RDSDOR',0,0,' ',IS,XNAME,'D',D,R,I,C,L,
     $             1,1,1,1,1,IL,
     $             DM,RM,IM,CM,LM)

      X      = D(1)
      LXNAME = XNAME
      ILX    = LEN_TRIM (LXNAME)
      IF (XMAX.LT.XMIN) THEN
         IF (TOSCR) WRITE (*,'(1X,4A)')
     &     'ERROR in RDSDOR: Range not valid of identifier ',
     &      Q,LXNAME(1:ILX),Q
         IF (TOLOG) WRITE (IULOG,'(1X,4A)')
     &     'ERROR in RDSDOR: Range not valid of identifier ',
     &      Q,LXNAME(1:ILX),Q
         CALL FATALERR (' ',' ')
      ELSE IF ((X.LT.XMIN.OR.X.GT.XMAX).AND.X.NE.DM) THEN
         IF (TOSCR) WRITE (*,
     &     '(1X,5A,/,T19,A,G12.5,A,G12.5,A,G12.5,A)')
     &     'ERROR in RDSDOR: Range error of identifier ',
     &      Q,LXNAME(1:ILX),Q,',',
     &     'value =',X,', range = [',XMIN,',',XMAX,']'
         IF (TOLOG) WRITE (IULOG,
     &     '(1X,5A,/,T19,A,G12.5,A,G12.5,A,G12.5,A)')
     &     'ERROR in RDSDOR: Range error of identifier ',
     &      Q,LXNAME(1:ILX),Q,',',
     &     'value =',X,', range = [',XMIN,',',XMAX,']'
         CALL FATALERR (' ',' ')
      END IF

      RETURN
      END
