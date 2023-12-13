C=======================================================================
C  NCHECK_organic, Subroutine
C
C  Checks for negative values of soil N and prints report if found.
C-----------------------------------------------------------------------
C  Revision history
C  12/22/1999 CHP written
C  03/16/2000 GH  Incorporated in CROPGRO
C               Note: File names etc. should be dynamically created 
C               Check time stamp
!  02/25/2005 CHP Split NCHECK into organic and inorganic.
!  03/01/2005 CHP Changed HUMC to SSOMC and HUMN to SSOME to match 
!                 Century variable names.
C-----------------------------------------------------------------------
      SUBROUTINE NCHECK_organic(CONTROL, N_ELEMS,
     &    FOM, FON, FOP, FPOOL, NLAYR, SSOMC, SSOME)      !Input

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE 
      EXTERNAL NWRITE
!-----------------------------------------------------------------------
      INTEGER L, N_ELEMS, NLAYR, YRDOY

      REAL FOM(NL), FON(NL), FOP(NL), FPOOL(NL,3)
      REAL SSOMC(0:NL), SSOME(0:NL,NELEM)

      TYPE (ControlType) CONTROL
      YRDOY   = CONTROL % YRDOY

!-----------------------------------------------------------------------
!     Check for negative soil N values
      DO L = 1, NLAYR
        IF (FOM(L) < -0.001)     CALL NWRITE(L, FOM(L),     4)
        IF (SSOMC(L) < -0.001)   CALL NWRITE(L, SSOMC(L),   6)
        IF (FPOOL(L,1) < -0.001) CALL NWRITE(L, FPOOL(L,1), 8)
        IF (FPOOL(L,2) < -0.001) CALL NWRITE(L, FPOOL(L,2), 9)
        IF (FPOOL(L,3) < -0.001) CALL NWRITE(L, FPOOL(L,3),10)
!       Underflow trapping
        IF (FOM(L)     < 0.00001) FOM(L)     = 0.0
        IF (SSOMC(L)   < 0.00001) SSOMC(L)   = 0.0
        IF (FPOOL(L,1) < 0.00001) FPOOL(L,1) = 0.0
        IF (FPOOL(L,2) < 0.00001) FPOOL(L,2) = 0.0
        IF (FPOOL(L,3) < 0.00001) FPOOL(L,3) = 0.0

        IF (N_ELEMS > 0) THEN
          IF (FON(L) < -0.001)     CALL NWRITE(L, FON(L),    5)
          IF (SSOME(L,N) < -0.001) CALL NWRITE(L, SSOME(L,N),7)
!         Underflow trapping
          IF (FON(L) < 0.00001) FON(L)  = 0.0
          IF (SSOME(L,N) < 0.00001) SSOME(L,N) = 0.0
        ENDIF

        IF (N_ELEMS > 1) THEN
          IF (FOP(L) < -0.001)     CALL NWRITE(L, FOP(L),    12)
          IF (SSOME(L,P) < -0.001) CALL NWRITE(L, SSOME(L,P),11)
!         Underflow trapping
          IF (FOP(L) < 0.00001) FOP(L)  = 0.0
          IF (SSOME(L,P) < 0.00001) SSOME(L,P) = 0.
        ENDIF
      ENDDO

!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE NCHECK_organic

C=======================================================================
C  NWRITE, Subroutine
C
C  Writes negative values of soil N to Warning.OUT file
C-----------------------------------------------------------------------
C  Revision history
!  03/16/00 CHP written
!-----------------------------------------------------------------------
      SUBROUTINE NWRITE(L, VALUE, CODE)

!-----------------------------------------------------------------------
      IMPLICIT NONE
      EXTERNAL WARNING

      CHARACTER*78  MSG(10)
      INTEGER CODE, L
      REAL VALUE

!-----------------------------------------------------------------------
      MSG(3) = 'Value will be set to zero.'

      SELECT CASE (CODE)
      CASE (4)
        WRITE(MSG(1),300) L
        WRITE(MSG(2),
     &    "('Fresh organic matter = ',F10.3,'kg[N]/ha')") VALUE
        CALL WARNING(3, "CCHECK", MSG)
        VALUE = 0.0
      
      CASE (5)
        WRITE(MSG(1),100) L
        WRITE(MSG(2),
     &  "('N in fresh organic matter = ',F10.3,'kg[N]/ha')") VALUE
        CALL WARNING(3, "NCHECK", MSG)
        VALUE = 0.0

      CASE (12)
        WRITE(MSG(1),200) L
        WRITE(MSG(2),
     &  "('P in fresh organic matter = ',F10.3,'kg[P]/ha')") VALUE
        CALL WARNING(3, "PCHECK", MSG)
        VALUE = 0.0

      CASE (6)
        WRITE(MSG(1),300) L
        WRITE(MSG(2),
     &  "('Soil organic matter = ',F10.3,'kg[N]/ha')") VALUE
        CALL WARNING(3, "CCHECK", MSG)
        VALUE = 0.0

      CASE (7)
        WRITE(MSG(1),100) L
        WRITE(MSG(2),
     &  "('Soil organic N = ',F10.3,'kg[N]/ha')") VALUE
        CALL WARNING(3, "NCHECK", MSG)
        VALUE = 0.0

      CASE (11)
        WRITE(MSG(1),200) L
        WRITE(MSG(2),
     &  "('Soil organic P = ',F10.3,'kg[N]/ha')") VALUE
        CALL WARNING(3, "PCHECK", MSG)
        VALUE = 0.0

      CASE (8)
        WRITE(MSG(1),300) L
        WRITE(MSG(2),
     &  "('Soil carbohydrate = ',F10.3,'kg[N]/ha')") VALUE
        CALL WARNING(3, "CCHECK", MSG)
        VALUE = 0.0

      CASE (9)
        WRITE(MSG(1),300) L
        WRITE(MSG(2),
     &  "('Soil cellulose = ',F10.3,'kg[N]/ha')") VALUE
        CALL WARNING(3, "CCHECK", MSG)
        VALUE = 0.0

      CASE (10)
        WRITE(MSG(1),300) L
        WRITE(MSG(2),
     &  "('Soil lignin = ',F10.3,'kg[N]/ha')") VALUE
        CALL WARNING(3, "CCHECK", MSG)
        VALUE = 0.0

      END SELECT

  100 FORMAT('Negative soil N value in layer ',I3)
  200 FORMAT('Negative soil P value in layer ',I3)
  300 FORMAT('Negative soil C value in layer ',I3)

      RETURN
      END SUBROUTINE NWRITE

!==========================================================================
