      SUBROUTINE MOVAVR (ITASK,NAME,IP,IN,OUT)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      INTEGER ITASK,IP
      REAL IN, OUT
      CHARACTER*(*) NAME

**    local variables
      INTEGER MNP,MNNAM,NNAM
      PARAMETER (MNP=100,MNNAM=10)
      REAL ARR(MNP,MNNAM)
      INTEGER NPA(MNNAM)
      CHARACTER*31 NAMESA(MNNAM),LNAME
      INTEGER I1,N,IFINDC,NP
      SAVE

      IF (ITASK.EQ.1) THEN

*        initialize storage arrays

         DO 10 I1=1,MNNAM
            NPA(I1)    = 0
            NAMESA(I1) = ' '
10       CONTINUE
         NNAM = 0
      ELSE

*        calculate running average

*        look up NAME in list of names
         IF (IP.GT.MNP) CALL FATALERR ('MOVAVR',
     &      'maximum number of points exceeded')
         LNAME = NAME
         N = IFINDC (NAMESA,MNNAM,1,NNAM,LNAME)
         IF (N.EQ.0) THEN
*           NAME was not found in list of names, add to list
            NNAM = NNAM+1
            IF (NNAM.GT.MNNAM) CALL FATALERR ('MOVAVR','too many names')
            N = NNAM
            NAMESA(N) = LNAME
         END IF

*        shift old values for NAME
         NP = MIN (NPA(N)+1, IP)

         DO 20 I1=NP,2,-1
            ARR(I1,N) = ARR(I1-1,N)
20       CONTINUE

*        add new value to array
         ARR(1,N) = IN

*        calculate running average by summing and dividing
         OUT = 0.
         DO 30 I1=1,NP
            OUT = OUT+ARR(I1,N)
30       CONTINUE
         OUT    = OUT/REAL (NP)
         NPA(N) = NP
      END IF

      RETURN
      END
