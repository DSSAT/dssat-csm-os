!======================================================================
!  WH_KUPTAK, Subroutine
!
!  Determines K uptake
!----------------------------------------------------------------------
!  Revision history
!
!  10/31/2007 US / CHP Written based on WH_NUPTAK
!  06/27/2011 FSR created WH_KUPTAK.for for APSIM NWheat adaptation.
!----------------------------------------------------------------------
!  Called by: WH_GROSUB 
!
!  Calls  : None
!----------------------------------------------------------------------
      SUBROUTINE WH_KUPTAK(
     &    ISWPOT, NLAYR, SKi_Avail, UNH4, UNO3,           !Input
     &    KUPTAKE, KSTRES)                                !Output

      USE ModuleDefs
      IMPLICIT  NONE
      SAVE

      CHARACTER*1 ISWPOT
      INTEGER L, NLAYR

      REAL        UNH4(NL), UNO3(NL)        !N UPTAKE         
      REAL        KUPTAKE(NL)     !K UPTAKE
      REAL        SKi_Avail(NL)   !K available for uptake         
      REAL        KSTRES, KUPTAKE_TOT, NUPTAKE_TOT 

!----------------------------------------------------------------------
      KUPTAKE = 0.0
      IF (ISWPOT .NE. 'Y') THEN
        KSTRES = 1.0
        RETURN
      ENDIF

      DO L = 1, NLAYR
        KUPTAKE(L) = MIN(UNH4(L)+UNO3(L),SKi_Avail(L))
        KUPTAKE(L) = MAX(KUPTAKE(L), 0.0)
      ENDDO
      KUPTAKE_TOT = SUM(KUPTAKE)
      NUPTAKE_TOT = SUM(UNH4)+SUM(UNO3)

      IF (NUPTAKE_TOT > 1.E-6) THEN
        KSTRES = KUPTAKE_TOT / NUPTAKE_TOT
      ELSE
        KSTRES = 1.0
      ENDIF

      RETURN
      END SUBROUTINE WH_KUPTAK
!======================================================================
