!=======================================================================
!  SoilMixing, Subroutine, C.H. Porter
!  Generic soil mixing utility.  

!  Subroutine SoilMixing is called directly if no pseudo-integration
!     of variables is done.  
!  SoilMixing assumes arrays go from 1 to NL; No zero layer is allowed.

!  SoilMix generic routines in module ModSoilMix are used for the 
!     N, P and organic matter routines, which need to add in 
!     today's fertilizer and organic matter applications prior to 
!     mixing.  A call to SoilMix references the generic interface 
!     routines contained in ModSoilMix.  This module will differentiate 
!     between 1- and 2-dimensional arrays.
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  12/10/2003 CHP Written
!  03/09/2006 CHP Keep input array separately
!=======================================================================

      SUBROUTINE SoilMixing(DLAYR, MIXPCT, NLAYR, TDEP, VALUE_IN, 
     &    VALUE_MIXED) 

!-----------------------------------------------------------------------
      USE ModuleDefs
      IMPLICIT NONE
      SAVE

      INTEGER L, NLAYR, NLAYRT
      REAL CUMDEP, MIXPCT, SUMMIXED, TDEP
      REAL, DIMENSION(NL) :: DLAYR, ZLYR, VALUE_IN, VALUE_MIXED,
     &    AMT_UNMIXED, AMT_2B_MIXED

!-----------------------------------------------------------------------
      VALUE_MIXED  = VALUE_IN
      AMT_2B_MIXED = 0.0
      AMT_UNMIXED  = 0.0

!     Set up soil layers within tillage depth
      CUMDEP = 0.0
      SUMMIXED = 0.0
      DO L = 1, NLAYR
        CUMDEP = CUMDEP + DLAYR(L)
        ZLYR(L) = DLAYR(L)
        IF (CUMDEP < TDEP) THEN
          AMT_2B_MIXED(L)= VALUE_IN(L) * MIXPCT/100.
          AMT_UNMIXED(L) = VALUE_IN(L) - AMT_2B_MIXED(L)
          SUMMIXED = SUMMIXED + AMT_2B_MIXED(L)
        ELSEIF (ABS(TDEP - CUMDEP) .LT. 0.5 .OR. TDEP .LT. CUMDEP) THEN
          ZLYR(L) = ZLYR(L) - (CUMDEP - TDEP)
          AMT_2B_MIXED(L)= VALUE_IN(L) * ZLYR(L)/DLAYR(L) * MIXPCT/100.
          AMT_UNMIXED(L) = VALUE_IN(L) - AMT_2B_MIXED(L)
          SUMMIXED = SUMMIXED + AMT_2B_MIXED(L)
          NLAYRT = L
          EXIT
        ENDIF
      ENDDO

!     Add mixed portion back into each layer
      DO L = 1, NLAYRT
        VALUE_MIXED(L) = SUMMIXED * ZLYR(L) / TDEP    !Mixed
     &           + AMT_UNMIXED(L)                     !Unmixed
      ENDDO

!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE SoilMixing

!=======================================================================


!=======================================================================
!  SoilMix Generic routines

!  Prepares pseudo-integration for mixing variables of 1- and 2- 
!    dimensional arrays. 
!  Need to supply the lower bound of the soil 
!    dimension (i.e., zero for variables which contain a surface layer
!    and one for variables which don't).
!  For 2-dimensional arrays, also need to supply the element in the 
!    array which is to be mixed, i.e. 1 for N and 2 for P.  
!    Need separate calls to SoilMix for each element.
!  Returns the revised "Dlt" variable after mixing.
!  Surface layer is not included in mixing.  Incorporation of surface
!     matter is assumed to have been done prior to this step.

!  Generic subroutines are differentiated by the different number or
!     types of arguments.  In this case, SoilMix_1D and SoilMix_2D  
!     differ by the inclusion of the argument IELEM in SoilMix_2D 
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  07/12/2006 CHP Written
!  03/26/2007 CHP Changed to integrate mixed values rather than add to 
!                 DLT variables.  Rate calculations after mixing were
!                 causing negative values for some variables.
!=======================================================================

      MODULE ModSoilMix

      INTERFACE SoilMix
        MODULE PROCEDURE SoilMix_1D, SoilMix_2D
      END INTERFACE

!----------------------------------------------------------------------
      CONTAINS

!=======================================================================
!     1D array (soil layers only)
      SUBROUTINE SoilMix_1D (Var, DltVar, LB,
     &    DLAYR, MIXPCT, NLAYR, TILLDEPTH)

!     ------------------------------------------------------------------
      USE ModuleDefs
      IMPLICIT NONE
      EXTERNAL SoilMixing
      SAVE

      INTEGER L, LB, NLAYR
      REAL MIXPCT, TILLDEPTH
      REAL, DIMENSION(LB:NL) :: Var, DltVar
      REAL, DIMENSION(NL) :: CVAL, CVAL_MIXED, DLAYR

!     ------------------------------------------------------------------
        DO L = 1, NLAYR
!         Pseudo-integration
          CVAL(L) = Var(L) + DltVar(L)
        ENDDO

!       Mixing routine returns array after mixing
        CALL SoilMixing(DLAYR, MIXPCT, NLAYR, TILLDEPTH, CVAL, 
     &     CVAL_MIXED) 

        DO L = 1, NLAYR
!         Revised "dlt" variable includes changes due to mixing.
!          DltVar(L) = CVAL_MIXED(L) - Var(L)
!         Integrate mixed values here 3/26/07
          Var(L) = CVAL_MIXED(L)
          DltVar(L) = 0.0
        ENDDO

        IF (LB ==0) THEN
          Var(0) = Var(0) + DltVar(0)
          DltVar(0) = 0.0
        ENDIF
!     ------------------------------------------------------------------
      RETURN
      END SUBROUTINE SoilMix_1D

!=======================================================================
!     2D array (soil layers and elements)
      SUBROUTINE SoilMix_2D (Var, DltVar, LB, IELEM, 
     &    DLAYR, MIXPCT, NLAYR, TILLDEPTH)

!     ------------------------------------------------------------------
      USE ModuleDefs
      IMPLICIT NONE
      EXTERNAL SoilMixing
      SAVE

      INTEGER IELEM, L, LB, NLAYR
      REAL MIXPCT, TILLDEPTH
      REAL, DIMENSION(LB:NL,*) :: Var, DltVar
      REAL, DIMENSION(NL) :: CVAL, CVAL_MIXED, DLAYR

!     ------------------------------------------------------------------
        DO L = 1, NLAYR
!         Pseudo-integration
          CVAL(L) = Var(L,IELEM) + DltVar(L,IELEM)
        ENDDO

!       Mixing routine returns array after mixing
        CALL SoilMixing(DLAYR, MIXPCT, NLAYR, TILLDEPTH, CVAL, 
     &     CVAL_MIXED) 

        DO L = 1, NLAYR
!         Revised "dlt" variable includes changes due to mixing.
!          DltVar(L,IELEM) = CVAL_MIXED(L) - Var(L,IELEM)
!         Integrate mixed values here 3/26/07
          Var(L,IELEM) = CVAL_MIXED(L)
          DltVar(L,IELEM) = 0.0
        ENDDO

        IF (LB ==0) THEN
          Var(0,IELEM) = Var(0,IELEM) + DltVar(0,IELEM)
          DltVar(0,IELEM) = 0.0
        ENDIF
!     ------------------------------------------------------------------
      RETURN
      END SUBROUTINE SoilMix_2D

!=======================================================================
      END MODULE ModSoilMix
