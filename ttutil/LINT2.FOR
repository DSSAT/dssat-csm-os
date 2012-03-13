      REAL FUNCTION LINT2 (TABNAM,TABLE,ILTAB,X)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      CHARACTER*(*) TABNAM
      INTEGER ILTAB
      REAL TABLE(ILTAB), X
 
**    local variables
      INTEGER I1, IUP, IL, UNLOG
      REAL SLOPE, TINY
      PARAMETER (TINY=1.E-7)
      LOGICAL ERR, TOSCR, TOLOG
      SAVE
 
*     initialize
      ERR  = .FALSE.
 
*     desired output type for messages
      CALL MESSINQ (TOSCR, TOLOG, UNLOG)

*     check on value of ILTAB
      IF (MOD(ILTAB,2).NE.0 .OR. ILTAB.LE.2) THEN
         IL = MAX (1, LEN_TRIM (TABNAM))
         IF (TOSCR) WRITE (*,'(2A,/,A)')
     &    ' Number of elements in interpolation table: ',TABNAM(1:IL),
     &    ' not correct !'
         IF (TOLOG) WRITE (UNLOG,'(2A,/,A)')
     &    ' Number of elements in interpolation table: ',TABNAM(1:IL),
     &    ' not correct !'
         ERR = .TRUE.
 
      ELSE
         IUP = 0
         DO 10 I1=3,ILTAB,2
*           check on ascending order of X-values in function
            IF (TABLE(I1).LE.TABLE(I1-2)) THEN
               IL = MAX (1, LEN_TRIM (TABNAM))
               IF (TOSCR) WRITE (*,'(2A,/,A,I4)')
     &          ' X-coordinates in interpolation table: ',TABNAM(1:IL),
     &          ' not in ascending order at point',I1
               IF (TOLOG) WRITE (UNLOG,'(2A,/,A,I4)')
     &          ' X-coordinates in interpolation table: ',TABNAM(1:IL),
     &          ' not in ascending order at point',I1
               ERR = .TRUE.
            END IF
            IF (IUP.EQ.0 .AND. TABLE(I1).GE.X) IUP = I1
10       CONTINUE
      END IF
 
      IF (.NOT.ERR .AND. X.LT.TABLE(1)) THEN
         IUP = 3
         IF ((TABLE(1)-X) .GT. ABS(X)*TINY) THEN
            IL = MAX (1, LEN_TRIM (TABNAM))
            IF (TOSCR) WRITE (*,'(A,G13.5,/,2A)')
     &       ' WARNING in LINT2: X-value below defined region at X=',X,
     &       ' in interpolation table: ',TABNAM(1:IL)
            IF (TOLOG) WRITE (UNLOG,'(A,G13.5,/,2A)')
     &       ' WARNING in LINT2: X-value below defined region at X=',X,
     &       ' in interpolation table: ',TABNAM(1:IL)
         END IF
      ELSE IF (.NOT.ERR .AND. X.GT.TABLE(ILTAB-1)) THEN
         IUP = ILTAB-1
         IF ((X-TABLE(ILTAB-1)) .GT. ABS(X)*TINY) THEN
            IL = MAX (1, LEN_TRIM (TABNAM))
            IF (TOSCR) WRITE (*,'(A,G13.5,/,2A)')
     &       ' WARNING in LINT2: X-value above defined region at X=',X,
     &       ' in interpolation table: ',TABNAM(1:IL)
            IF (TOLOG) WRITE (UNLOG,'(A,G13.5,/,2A)')
     &       ' WARNING in LINT2: X-value above defined region at X=',X,
     &       ' in interpolation table: ',TABNAM(1:IL)
         END IF
      END IF
 
      IF (ERR) CALL FATALERR ('LINT2',' ')
 
*     interpolation and extrapolation
      SLOPE = (TABLE(IUP+1)-TABLE(IUP-1))/(TABLE(IUP)-TABLE(IUP-2))
      LINT2 = TABLE(IUP-1)+(X-TABLE(IUP-2))*SLOPE
 
      RETURN
      END
