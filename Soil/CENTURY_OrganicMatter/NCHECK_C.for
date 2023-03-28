!=======================================================================
!  NCHECK_C, Subroutine for CENTURY-based SOM/residue module of DSSAT.
!  Checks for negative values of soil N and prints report if found.
!-----------------------------------------------------------------------
!  Revision history
!  12/22/1999 CHP Written
!  02/04/2000 AJG Modified for CENTURY-based SOM/residue module.
!  11/14/2003 CHP Removed call to WARNING for residue-N values.  
!
!  Called: CENTURY
!  Calls : NWRITE_C
!-----------------------------------------------------------------------

      SUBROUTINE NCHECK_C (CONTROL, 
     &  ADDMETABEFLAG, FRLSTR, FRMETFLAG, METABC,         !Input
     &  METABE, NLAYR, SOM1C, SOM1E, SOM2C,               !Input
     &  SOM2E, SOM23E, SOM3C, SOM3E, STRUCC, STRUCE)      !Input

!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         !which contain control information, soil
                         !parameters, hourly weather data.
      IMPLICIT NONE
      EXTERNAL WARNING, NWRITE_C
!     ------------------------------------------------------------------
!     NL defined in ModuleDefs.for.
      INTEGER DYNAMIC, L, N_ELEMS, NLAYR, SRFC
      PARAMETER (SRFC = 0)

      REAL FRLSTR(0:NL), METABC(0:NL), 
     &  SOM1C(0:NL), SOM2C(NL), SOM3C(NL),
     &  STRUCC(0:NL)
      REAL METABE(0:NL,NELEM), SOM1E(0:NL,NELEM), SOM2E(NL,NELEM),
     &  SOM23E(NL,NELEM), SOM3E(NL,NELEM), STRUCE(0:NL,NELEM)

      CHARACTER*6, PARAMETER :: ERRKEY = "NCHECK"
      CHARACTER*78 MSG(10)

      LOGICAL ADDMETABEFLAG, FRMETFLAG

      TYPE (ControlType) CONTROL
      DYNAMIC = CONTROL % DYNAMIC
      N_ELEMS = CONTROL % N_ELEMS

!***********************************************************************
!***********************************************************************
!     Rate section - Warning messages for Residue N 
!***********************************************************************
      IF (DYNAMIC == RATE) THEN
!     ------------------------------------------------------------------
!       Warning that C:E ratio in subroutine PARTIT is unlikely wide.
        IF (FRMETFLAG) THEN
          WRITE (MSG(1), 800) 
          WRITE (MSG(2), 801) 
          WRITE (MSG(3), 802) 
          WRITE (MSG(4), 803) 
          CALL WARNING (4, ERRKEY, MSG)
!         Set back to FALSE; otherwise it prints the warning every day.
          FRMETFLAG = .FALSE.
        ENDIF   !End of IF block on FRMETFLAG

800   FORMAT('The fraction of metabolic material in newly applied',
     &    ' residue')
801   FORMAT('or senesced biomass that was added to the soil')
802   FORMAT('is <0 or >1. This may give a negative structural',
     &    ' fraction.') 
803   FORMAT('This is due to an incorrect lignin or N concentration.')

!       Warning that C:E ratio in subroutine PARTIT is unlikely wide.
        IF (ADDMETABEFLAG) THEN
          WRITE (MSG(1), '(A)') 
     & 'The carbon/nutrient ratio (C/N or C/P) of the residue'
          WRITE (MSG(2), '(A)') 
     & 'or senesced biomass that was added to the soil'
          WRITE (MSG(3), '(A)') 
     & 'is outside the common range to which the CESTR'
          WRITE (MSG(4), '(A,A,A)') 'parameter in the file SOMFX',
     & ModelVerTxt,'.SDA applies. You have a'
          WRITE (MSG(5), '(A)') 
     & 'residue with an extremely low nutrient concentration.'
          WRITE (MSG(6), '(A)') 
     & 'Please check your data and/or adapt the CESTR value.'
          CALL WARNING (6, ERRKEY, MSG)
!         Set back to FALSE; otherwise it prints the warning every day.
          ADDMETABEFLAG = .FALSE.
        ENDIF   !End of IF block on ADDMETABEFLAG


***********************************************************************
!***********************************************************************
!     INTEGRATE
!***********************************************************************
      ELSEIF (DYNAMIC == INTEGR) THEN
!     ------------------------------------------------------------------
        DO L = 0, NLAYR   !Including SRFC layer.
!         Check for negative SOM/residue and soil-N values.
          IF (METABC(L) < -0.001)   CALL NWRITE_C(L, METABC(L),1)
          IF (STRUCC(L) < -0.001)   CALL NWRITE_C(L, STRUCC(L),2)
          IF (FRLSTR(L) < -0.001 .OR. FRLSTR(L) > 1.) 
     &                              CALL NWRITE_C (L,FRLSTR(L),3)
          IF (SOM1C(L) < -0.001)    CALL NWRITE_C(L, SOM1C(L), 4)
          IF (L > SRFC) THEN
            IF (SOM2C(L) < -0.001)  CALL NWRITE_C(L, SOM2C(L),5)
            IF (SOM3C(L) < -0.001)  CALL NWRITE_C(L, SOM3C(L),6)
          ENDIF

!     ----------------------------------------------------------------
!         Check N components
          IF (METABE(L,N) < -0.001) CALL NWRITE_C (L, METABE(L,N), 7)
          IF (STRUCE(L,N) < -0.001) CALL NWRITE_C (L, STRUCE(L,N), 8)
          IF (SOM1E(L,N) < -0.001)  CALL NWRITE_C (L, SOM1E(L,N),  9)

          IF (L > SRFC) THEN
            IF (SOM2E(L,N) < -0.001)CALL NWRITE_C (L, SOM2E(L,N), 10)
            IF (SOM3E(L,N) < -0.001)CALL NWRITE_C (L, SOM3E(L,N), 11)
          ENDIF
!     ----------------------------------------------------------------
!         Check P components
          IF (N_ELEMS > 1) THEN
            IF (METABE(L,P) < -0.001) CALL NWRITE_C (L, METABE(L,P),12)
            IF (STRUCE(L,P) < -0.001) CALL NWRITE_C (L, STRUCE(L,P),13)
            IF (SOM1E(L,P) < -0.001)  CALL NWRITE_C (L, SOM1E(L,P), 14)

            IF (L > SRFC) THEN
              IF (SOM23E(L,P) < -0.001)CALL NWRITE_C(L, SOM2E(L,P), 15)
            ENDIF
          ENDIF
!     ------------------------------------------------------
        ENDDO   !End of soil layer loop.

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF

!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE NCHECK_C


!=======================================================================
!  NWRITE_C, Subroutine for CENTURY-based SOM/residue module.
!
!  Writes negative values of soil N to Warning.OUT file
!-----------------------------------------------------------------------
!  Revision history
!  03/16/00 CHP written.
!  03/28/00 AJG Modified for CENTURY-based SOM/residue module.
!-----------------------------------------------------------------------
      SUBROUTINE NWRITE_C (L, VALUE, CODE)

!     ------------------------------------------------------------------
      IMPLICIT NONE
      EXTERNAL WARNING

      INTEGER CODE, L
      REAL VALUE
      CHARACTER*78 MSG(10)

!     ------------------------------------------------------------------
      SELECT CASE (CODE)
        CASE (1)
          WRITE (MSG(1), 130) L
          WRITE (MSG(2), 140) VALUE
130       FORMAT(
     &      'Negative SOM/residue value in layer ', I3)
140       FORMAT ('Metabolic carbon (METABC) = ', F10.3, ' kg[C]/ha')
          CALL WARNING (2, "CCHECK", MSG)
        
        CASE (2)
          WRITE (MSG(1), 130) L
          WRITE (MSG(2), 150) VALUE
150       FORMAT('Structural carbon (STRUCC) = ', F10.3, ' kg[C]/ha')
          CALL WARNING (2, "CCHECK", MSG)

        CASE (3)
          IF (VALUE > 1) THEN
            WRITE (MSG(1),155) L
          ELSE
            WRITE (MSG(1), 130) L
          ENDIF
          WRITE (MSG(2), 160) VALUE
155       FORMAT ('Lignin fraction > 1 in layer ', I3)
160       FORMAT ('Structural lignin (FRLSTR) = ', F10.3, '%')
          CALL WARNING (2, "CCHECK", MSG)

        CASE (4)
          WRITE (MSG(1), 130) L
          WRITE (MSG(2), 170) VALUE
170       FORMAT ('SOM1 carbon (SOM1C) = ', F10.3, ' kg[C]/ha')
          CALL WARNING (2, "CCHECK", MSG)

        CASE (5)
          WRITE (MSG(1), 130) L
          WRITE (MSG(2), 180) VALUE
180       FORMAT ('SOM2 carbon (SOM2C) = ', F10.3, 'kg[C]/ha')
          CALL WARNING (2, "CCHECK", MSG)

        CASE (6)
          WRITE (MSG(1), 130) L
          WRITE (MSG(2), 190) VALUE
190       FORMAT ('SOM3 carbon (SOM3C) = ', F10.3, 'kg[C]/ha')
          CALL WARNING (2, "CCHECK", MSG)

        CASE (7)
          WRITE (MSG(1), 130) L
          WRITE (MSG(2), 200) VALUE
200       FORMAT ('Metabolic Nitrogen (METABE(N)) = ', F10.3,
     &      'kg[N]/ha')
          CALL WARNING (2, "NCHECK", MSG)

        CASE (8)
          WRITE (MSG(1), 130) L
          WRITE (MSG(2), 210) VALUE
210       FORMAT('Structural Nitrogen (STRUCE(N)) = ', F10.3,
     &      'kg[N]/ha')
          CALL WARNING (2, "NCHECK", MSG)

        CASE (9)
          WRITE (MSG(1), 130) L
          WRITE (MSG(2), 220) VALUE
220       FORMAT('SOM1 Nitrogen (SOM1E(N)) = ', F10.3, 'kg[N]/ha')
          CALL WARNING (2, "NCHECK", MSG)

        CASE (10)
          WRITE (MSG(1), 130) L
          WRITE (MSG(2), 230) VALUE
230       FORMAT('SOM2 Nitrogen (SOM2E(N)) = ', F10.3, 'kg[N]/ha')
          CALL WARNING (2, "NCHECK", MSG)

        CASE (11)
          WRITE (MSG(1), 130) L
          WRITE (MSG(2), 240) VALUE
240       FORMAT('SOM3 Nitrogen (SOM3E(N)) = ', F10.3, 'kg[N]/ha')
          CALL WARNING (2, "NCHECK", MSG)

        CASE (12)
          WRITE (MSG(1), 130) L
          WRITE (MSG(2), 300) VALUE
300       FORMAT ('Metabolic Phosphorus (METABE(P)) = ', F10.3,
     &      'kg[P]/ha')
          CALL WARNING (2, "PCHECK", MSG)

        CASE (13)
          WRITE (MSG(1), 130) L
          WRITE (MSG(2), 310) VALUE
310       FORMAT('Structural Phosphorus (STRUCE(P)) = ', F10.3,
     &      'kg[P]/ha')
          CALL WARNING (2, "PCHECK", MSG)

        CASE (14)
          WRITE (MSG(1), 130) L
          WRITE (MSG(2), 320) VALUE
320       FORMAT('SOM1 Phosphorus (SOM1E(P)) = ', F10.3, 'kg[P]/ha')
          CALL WARNING (2, "PCHECK", MSG)

        CASE (15)
          WRITE (MSG(1), 130) L
          WRITE (MSG(2), 330) VALUE
330       FORMAT('SOM2&3 Phosphorus (SOM23E(P)) = ', F10.3, 'kg[P]/ha')
          CALL WARNING (2, "PCHECK", MSG)
      END SELECT

      RETURN
      END SUBROUTINE NWRITE_C


