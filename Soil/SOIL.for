!=======================================================================
!  COPYRIGHT 1998-2025
!                      DSSAT Foundation
!                      University of Florida, Gainesville, Florida
!                      International Fertilizer Development Center
!                     
!  ALL RIGHTS RESERVED
!=======================================================================
!  SOIL, Subroutine
!-----------------------------------------------------------------------
!  Soil Processes subroutine.  Calls the following modules:
!     SOILDYN     - integrates soil properties variables
!     WATBAL      - soil water balance
!     SoilN_inorg - inorganic soil N (from NTRANS)
!     SoilPi      - inorganic soil P
!     SoilKi      - inorganic soil K
!     SoilOrg     - Ceres soil organic matter (from NTRANS)
!     CENTURY     - Century soil organic matter
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  11/02/2001 CHP Written
!  04/20/2002 GH  Modified for crop rotations
!  10/24/2005 CHP Put weather variables in constructed variable. 
!  10/25/2005 CHP Removed NTRANS_OLD module
!  02/22/2006 CHP Added tiledrain.
!  03/03/2006 CHP Added tillage (A.Andales & WDBatchelor).
!  03/21/2006 CHP Added mulch effects
!  07/14/2006 CHP Added P model, split inorganic and organic N routines,
!                 move fertilizer and organic matter placement routines
!                 to management module.
!  10/31/2007 CHP Added simple K model.
!  01/26/2023 CHP Reduce compile warnings: add EXTERNAL stmts, remove 
!                 unused variables, shorten lines. 
C=====================================================================

      SUBROUTINE SOIL(CONTROL, ISWITCH, 
     &    ES, FERTDATA, FracRts, HARVRES, IRRAMT,         !Input
     &    KTRANS, KUptake, OMAData, PUptake, RLV,         !Input
     &    SENESCE, ST, SWDELTX,TILLVALS, UNH4, UNO3,      !Input
     &    WEATHER, XHLAI,                                 !Input
     &    FLOODN, FLOODWAT, MULCH, UPFLOW,                !I/O
     &    NH4_plant, NO3_plant, SKi_AVAIL, SNOW,          !Output
     &    SPi_AVAIL, SOILPROP, SomLitC, SomLitE,          !Output
     &    SW, SWDELTS, SWDELTU, UPPM, WINF, YREND)        !Output

!-----------------------------------------------------------------------
      USE ModuleDefs
      USE FloodModule
      USE GHG_mod
      IMPLICIT NONE
      EXTERNAL SOILDYN, WATBAL, CENTURY, SoilOrg, SoilNi, SoilPi, SoilKi
      SAVE
!-----------------------------------------------------------------------
!     Interface variables:
!-----------------------------------------------------------------------
!     Input:
      TYPE (ControlType) , INTENT(IN) :: CONTROL
      TYPE (SwitchType)  , INTENT(IN) :: ISWITCH
      REAL               , INTENT(IN) :: ES
      TYPE (FertType)    , INTENT(IN) :: FERTDATA
      REAL, DIMENSION(NL), INTENT(IN) :: FracRts
      Type (ResidueType) , INTENT(IN) :: HARVRES
      REAL               , INTENT(IN) :: IRRAMT
      REAL               , INTENT(IN) :: KTRANS
      TYPE (OrgMatAppType),INTENT(IN) :: OMAData
      REAL, DIMENSION(NL), INTENT(IN) :: PUptake, KUptake
      REAL, DIMENSION(NL), INTENT(IN) :: RLV
      Type (ResidueType) , INTENT(IN) :: SENESCE
!     REAL               , INTENT(IN) :: SRFTEMP 
      REAL, DIMENSION(NL), INTENT(IN) :: ST
      REAL, DIMENSION(NL), INTENT(IN) :: SWDELTX
      TYPE (TillType)    , INTENT(IN) :: TILLVALS
      REAL, DIMENSION(NL), INTENT(IN) :: UNH4, UNO3
      TYPE (WeatherType) , INTENT(IN) :: WEATHER
      REAL               , INTENT(IN) :: XHLAI

      REAL, DIMENSION(NL) :: SomLit 

!     Input/Output:
      REAL, DIMENSION(NL), INTENT(INOUT) :: UPFLOW
      TYPE (FloodNType),   INTENT(INOUT) :: FLOODN
      TYPE (FloodWatType), INTENT(INOUT) :: FLOODWAT
      TYPE (MulchType),    INTENT(INOUT) :: MULCH

!     Output:
      REAL, DIMENSION(NL), INTENT(OUT) :: NH4_plant
      REAL, DIMENSION(NL), INTENT(OUT) :: NO3_plant
      REAL, DIMENSION(NL), INTENT(OUT) :: SPi_AVAIL, SKi_AVAIL
      REAL               , INTENT(OUT) :: SNOW
      TYPE (SoilType)    , INTENT(OUT) :: SOILPROP
      REAL,DIMENSION(0:NL),INTENT(OUT) :: SomLitC
      REAL,DIMENSION(0:NL,NELEM),INTENT(OUT) :: SomLitE
      REAL, DIMENSION(NL), INTENT(OUT) :: SW
      REAL, DIMENSION(NL), INTENT(OUT) :: SWDELTS
      REAL, DIMENSION(NL), INTENT(OUT) :: SWDELTU
      REAL               , INTENT(OUT) :: WINF
      REAL, DIMENSION(NL), INTENT(OUT) :: UPPM
      INTEGER            , INTENT(OUT) :: YREND

!-----------------------------------------------------------------------
!     Local variables:
      INTEGER DYNAMIC
      CHARACTER*1  MESOM

      REAL, DIMENSION(0:NL) :: newCO2 !DayCent
      REAL, DIMENSION(NL) :: DRN
      REAL, DIMENSION(NL) :: SPi_Labile, NO3, NH4
      REAL, DIMENSION(0:NL) :: LITC, SSOMC
      REAL, DIMENSION(0:NL,NELEM) :: IMM, MNR
      
!     Added for tile drainage:
      REAL TDFC
      INTEGER TDLNO

!     Added for methane
      REAL DRAIN
      TYPE (CH4_type) CH4_data

!-----------------------------------------------------------------------
!     Transfer values from constructed data types into local variables.
      DYNAMIC = CONTROL % DYNAMIC
      MESOM   = ISWITCH % MESOM

!***********************************************************************
!     Call Soil Dynamics module 
!      IF (DYNAMIC < OUTPUT) THEN
        CALL SOILDYN(CONTROL, ISWITCH, 
     &    KTRANS, MULCH, SomLit, SomLitC, SW, TILLVALS,   !Input
     &    WEATHER, XHLAI,                                 !Input
     &    SOILPROP)                                       !Output
!      ENDIF

!     Call WATBAL first for all except seasonal initialization
      IF (DYNAMIC /= SEASINIT) THEN
        CALL WATBAL(CONTROL, ISWITCH, 
     &    ES, IRRAMT, SOILPROP, SWDELTX,                  !Input
     &    TILLVALS, WEATHER,                              !Input
     &    FLOODWAT, MULCH, SWDELTU,                       !I/O
     &    DRAIN, DRN, SNOW, SW, SWDELTS,                  !Output
     &    TDFC, TDLNO, UPFLOW, WINF)                      !Output
      ENDIF

!     Soil organic matter modules
      IF (MESOM .EQ. 'P') THEN
!       Parton (Century-based) soil organic matter module
        CALL CENTURY(CONTROL, ISWITCH, 
     &  DRAIN, FERTDATA, FLOODWAT, FLOODN, HARVRES,   !Input
     &  NH4, NO3, OMADATA, RLV, SENESCE,              !Input
     &  SOILPROP, SPi_Labile, ST, SW, TILLVALS,       !Input
     &  CH4_data, IMM, LITC, MNR, MULCH, newCO2,      !Output
     &  SomLit, SomLitC, SomLitE, SSOMC)              !Output
      ELSE
!      ELSEIF (MESOM .EQ. 'G') THEN
!       Godwin (Ceres-based) soil organic matter module (formerly NTRANS)
        CALL SoilOrg (CONTROL, ISWITCH, 
     &    DRAIN, FERTDATA, FLOODWAT, FLOODN, HARVRES,     !Input
     &    NH4, NO3, OMAData, RLV,                         !Input
     &    SENESCE, SOILPROP, SPi_Labile, ST, SW, TILLVALS,!Input
     &    CH4_data, IMM, LITC, MNR, MULCH, newCO2,        !Output
     &    SomLit, SomLitC, SomLitE, SSOMC)                !Output
      ENDIF

!     Inorganic N (formerly NTRANS)
      CALL SoilNi (CONTROL, ISWITCH, 
     &    CH4_data, DRN, ES, FERTDATA, FLOODWAT, IMM,     !Input
     &    LITC, MNR, newCO2, SNOW, SOILPROP, SSOMC, ST,   !Input
     &    SW, TDFC, TDLNO, TILLVALS, UNH4, UNO3, UPFLOW,  !Input
     &    WEATHER, XHLAI,                                 !Input
     &    FLOODN,                                         !I/O
     &    NH4, NO3, NH4_plant, NO3_plant, UPPM)           !Output

!     Inorganic P
      CALL SoilPi(CONTROL, ISWITCH, FLOODWAT, 
     &    FERTDATA, IMM, MNR, PUptake, SOILPROP,          !Input
     &    FracRts, SW, TillVals,                          !Input
     &    SPi_AVAIL, SPi_Labile, YREND)                   !Output

!     Inorganic K
      CALL SoilKi(CONTROL, ISWITCH, 
     &    FERTDATA, KUptake, SOILPROP, TILLVALS,          !Input
     &    SKi_Avail)                                      !Output

      IF (DYNAMIC == SEASINIT) THEN
!       Soil water balance -- call last for initialization
        CALL WATBAL(CONTROL, ISWITCH, 
     &    ES, IRRAMT, SOILPROP, SWDELTX,                  !Input
     &    TILLVALS, WEATHER,                              !Input
     &    FLOODWAT, MULCH, SWDELTU,                       !I/O
     &    DRAIN, DRN, SNOW, SW, SWDELTS,                  !Output
     &    TDFC, TDLNO, UPFLOW, WINF)                      !Output
      ENDIF

!***********************************************************************

!-----------------------------------------------------------------------

      RETURN
      END SUBROUTINE SOIL

!=======================================================================
