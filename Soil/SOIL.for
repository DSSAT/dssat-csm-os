!=======================================================================
!  COPYRIGHT 1998-2010 The University of Georgia, Griffin, Georgia
!                      University of Florida, Gainesville, Florida
!                      Iowa State University, Ames, Iowa
!                      International Center for Soil Fertility and 
!                       Agricultural Development, Muscle Shoals, Alabama
!                      University of Guelph, Guelph, Ontario
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
!  08/15/2011 JW  Add NH4 and NO3 outputs for SOILDYN. 
!                 They are calculated in RUNINIT an used by century 
C=====================================================================

      SUBROUTINE SOIL(CONTROL, ISWITCH, 
     &    EOP, ES, FERTDATA, HARVRES, IRRAMT, KTRANS,     !Input
     &    KUptake, OMAData, PUptake, SENESCE, ST,         !Input
     &    FracRts, SWDELTX,TILLVALS, UNH4, UNO3, UPFLOW,  !Input
     &    WEATHER, XHLAI, FLOODN, FLOODWAT, MULCH,        !I/O
     &    NH4, NO3, SKi_AVAIL, SNOW, SPi_AVAIL, SOILPROP, !Output
     &    SomLitC, SomLitE,                               !Output
     &    SOILPROP_furrow, SW, SWDELTS, SWDELTU, UPPM,    !Output
     &    SWFAC, TRWU, TRWUP, TURFAC, WINF, Cells, YREND) !Output

!-----------------------------------------------------------------------
      USE ModuleDefs
      USE Cells_2D
      USE FloodModule
      IMPLICIT NONE
      SAVE
!-----------------------------------------------------------------------
!     Interface variables:
!-----------------------------------------------------------------------
!     Input:
      TYPE (ControlType) , INTENT(IN) :: CONTROL
      TYPE (SwitchType)  , INTENT(IN) :: ISWITCH
      REAL               , INTENT(IN) :: ES, EOP
      TYPE (FertType)    , INTENT(IN) :: FERTDATA
      Type (ResidueType) , INTENT(IN) :: HARVRES
      REAL               , INTENT(IN) :: IRRAMT
      REAL               , INTENT(IN) :: KTRANS
      TYPE (OrgMatAppType),INTENT(IN) :: OMAData
      REAL, DIMENSION(NL), INTENT(IN) :: PUptake, KUptake
      Type (ResidueType) , INTENT(IN) :: SENESCE
!     REAL               , INTENT(IN) :: SRFTEMP 
      REAL, DIMENSION(NL), INTENT(IN) :: ST
      REAL, DIMENSION(NL), INTENT(IN) :: FracRts
      REAL, DIMENSION(NL), INTENT(IN) :: SWDELTX
      TYPE (TillType)    , INTENT(IN) :: TILLVALS
      REAL, DIMENSION(NL), INTENT(IN) :: UNH4, UNO3
      TYPE (WeatherType) , INTENT(IN) :: WEATHER
      REAL               , INTENT(IN) :: XHLAI
      Type (CellType) Cells(MaxRows,MaxCols)
      REAL, DIMENSION(NL) :: SomLit 

!     Input/Output:
      REAL, DIMENSION(NL), INTENT(INOUT) :: UPFLOW
      TYPE (FloodNType)   FLOODN
      TYPE (FloodWatType) FLOODWAT
      TYPE (MulchType)    MULCH

!     Output:
      REAL, DIMENSION(NL), INTENT(OUT) :: NH4
      REAL, DIMENSION(NL), INTENT(OUT) :: NO3
      REAL, DIMENSION(NL), INTENT(OUT) :: UPPM
      REAL, DIMENSION(NL), INTENT(OUT) :: SPi_AVAIL, SKi_AVAIL
      REAL               , INTENT(OUT) :: SNOW
      TYPE (SoilType)    , INTENT(OUT) :: SOILPROP
      REAL, DIMENSION(NL), INTENT(OUT) :: SW
      REAL, DIMENSION(NL), INTENT(OUT) :: SWDELTS
      REAL, DIMENSION(NL), INTENT(OUT) :: SWDELTU
      REAL               , INTENT(OUT) :: WINF
      REAL               , INTENT(OUT) :: SWFAC, TRWU, TRWUP, TURFAC
      INTEGER            , INTENT(OUT) :: YREND
      REAL, DIMENSION(0:NL) :: SomLitC
      REAL, DIMENSION(0:NL,NELEM) :: SomLitE

!-----------------------------------------------------------------------
!     Local variables:
      CHARACTER*1  MESOM, MEHYD

      INTEGER DYNAMIC

      REAL, DIMENSION(0:NL) :: newCO2 !DayCent
      REAL, DIMENSION(NL) :: DRN
      REAL, DIMENSION(NL) :: SPi_Labile
      REAL, DIMENSION(0:NL) :: LITC, SSOMC
      REAL, DIMENSION(0:NL,NELEM) :: IMM, MNR
      TYPE (SoilType) SOILPROP_furrow, SOILPROP_profile
      
!     Added for tile drainage:
      REAL TDFC
      INTEGER TDLNO

!-----------------------------------------------------------------------
!     Transfer values from constructed data types into local variables.
      DYNAMIC = CONTROL % DYNAMIC
      MESOM   = ISWITCH % MESOM
      MEHYD   = ISWITCH % MEHYD

!***********************************************************************
!     Call Soil Dynamics module 
!      IF (DYNAMIC < OUTPUT) THEN
        CALL SOILDYN(CONTROL, ISWITCH, 
     &    KTRANS, MULCH, SomLit, SomLitC, SW, TILLVALS,   !Input
     &    WEATHER, XHLAI,                                 !Input
     &    CELLS, SOILPROP, SOILPROP_furrow,               !Output
     &    SOILPROP_profile, NH4, NO3)                     !Output
!      ENDIF

!     Call WATBAL first for all except seasonal initialization
      IF (DYNAMIC /= SEASINIT) THEN
        SELECT CASE (MEHYD)
        CASE('G','C')   !Green-Ampt method - drip irrigation
          CALL WatBal2D(CONTROL, ISWITCH,
     &    EOP, ES, IRRAMT, SOILPROP, SOILPROP_FURROW,     !Input 
     &    WEATHER,                                        !Input
     &    Cells, SW, SWDELTS, SWFAC, TURFAC, TRWU, TRWUP) !Output
        CASE DEFAULT
          CALL WATBAL(CONTROL, ISWITCH, 
     &    ES, IRRAMT, SOILPROP, SWDELTX,                  !Input
     &    TILLVALS, WEATHER,                              !Input
     &    FLOODWAT, MULCH, SWDELTU,                       !I/O
     &    DRN, SNOW, SW, SWDELTS,                         !Output
     &    TDFC, TDLNO, UPFLOW, WINF)                      !Output
        END SELECT
      ENDIF

!     Soil organic matter modules
      IF (MESOM .EQ. 'P') THEN
!       Parton (Century-based) soil organic matter module
!       When DYNAMIC = RUNINIT, NH4 and NO3 are from SOILDYN. 
!       When DYNAMIC = RATE, NH4 and NO3 are from previous day's SoilOrg and Soilni
        CALL CENTURY(CONTROL, ISWITCH, 
     &    FERTDATA, FLOODWAT, FLOODN, HARVRES, NH4,       !Input
     &    NO3, OMADATA, SENESCE, SOILPROP, SPi_Labile,    !Input
     &    ST, SW, TILLVALS,                               !Input
     &    IMM, LITC, MNR, MULCH, SomLit, SomLitC,         !Output
     &    SomLitE, SSOMC,                                 !Output
     &    newCO2)             !for DayCent in SOILNI added by PG
      ELSE
!      ELSEIF (MESOM .EQ. 'G') THEN
!       Godwin (Ceres-based) soil organic matter module (formerly NTRANS)
        CALL SoilOrg (CONTROL, ISWITCH, CELLS,
     &    FLOODWAT, FLOODN, HARVRES, NH4, NO3, OMAData,   !Input
     &    SENESCE, SOILPROP, SPi_Labile, ST, SW, TILLVALS,!Input
     &    IMM, LITC, MNR, MULCH, newCO2, SomLit, SomLitC, !Output
     &    SomLitE, SSOMC)                                 !Output
      ENDIF

!     Inorganic N (formerly NTRANS)
      SELECT CASE (MEHYD)
      CASE('G','C')   !2D method - drip irrigation
        CALL SoilNi_2D (CONTROL, ISWITCH, 
     &    FERTDATA, IMM, LITC, MNR, SOILPROP,             !Input
     &    SOILPROP_profile, SSOMC, ST, WEATHER,           !Input
     &    Cells,                                          !Input,Output
     &    NH4, NO3, UPPM)                                 !Output
      CASE DEFAULT
        CALL SoilNi (CONTROL, ISWITCH, 
     &    DRN, ES, FERTDATA, FLOODWAT, IMM, LITC, MNR,    !Input
     &    newCO2, SNOW, SOILPROP, SSOMC, ST, SW, TDFC,    !Input
     &    TDLNO, TILLVALS, UNH4, UNO3, UPFLOW, WEATHER,   !Input
     &    XHLAI,                                          !Input
     &    FLOODN,                                         !I/O
     &    NH4, NO3, UPPM)                                 !Output
      END SELECT

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
        SELECT CASE (MEHYD)
        CASE('G','C')   !2d - drip irrigation
          CALL WatBal2D(CONTROL, ISWITCH,
     &    EOP, ES, IRRAMT, SOILPROP, SOILPROP_FURROW,     !Input 
     &    WEATHER,                                        !Input
     &    Cells, SW, SWDELTS, SWFAC, TURFAC, TRWU, TRWUP) !Output

          SNOW = 0.0; TDFC = 0.0; TDLNO = 0 
          WINF = 0.0; SWDELTU = 0.0
        CASE DEFAULT
          CALL WATBAL(CONTROL, ISWITCH, 
     &    ES, IRRAMT, SOILPROP, SWDELTX,                  !Input
     &    TILLVALS, WEATHER,                              !Input
     &    FLOODWAT, MULCH, SWDELTU,                       !I/O
     &    DRN, SNOW, SW, SWDELTS,                         !Output
     &    TDFC, TDLNO, UPFLOW, WINF)                      !Output
        END SELECT
      ENDIF

!***********************************************************************

!-----------------------------------------------------------------------

      RETURN
      END SUBROUTINE SOIL

!=======================================================================
