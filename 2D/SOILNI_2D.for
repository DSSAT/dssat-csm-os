C Change SW to SWV
C=======================================================================
C  COPYRIGHT 1998-2007 The University of Georgia, Griffin, Georgia
C                      University of Florida, Gainesville, Florida
C                      Iowa State University, Ames, Iowa
C                      International Center for Soil Fertility and 
C                       Agricultural Development, Muscle Shoals, Alabama
C                      University of Guelph, Guelph, Ontario
C  ALL RIGHTS RESERVED
C=======================================================================
C=======================================================================
C  SoilNi_2D, Subroutine
C
C  Determines 2 dimensional inorganic N transformations
C  This routine was modified from SoilNi.for 
C-----------------------------------------------------------------------
C  Revision history
C  . . . . . .  Written
C  06-01-2010 JZW / CHP Modified SoilNi for 2D.
!  08/15/2011 Rename RowFrac to ColFrac
!             Add arguments NFlux_L, NFlux_R, NFlux_D, NFlux_U
!             set NFlux_L, NFlux_R, NFlux_D, NFlux_U for Cell_Ndetail 
!  03/15/2013 Add DripRow by JZW
C-----------------------------------------------------------------------
C  Called : SOIL
C  Calls  : NCHECK_inorg_2D, NFLUX_2D, 
C           SOILNI_init_2D
C=======================================================================

      SUBROUTINE SoilNi_2D (CONTROL, ISWITCH, 
     &    FERTDATA, IMM, MNR, SOILPROP,           !Input
!    &    SOILPROP_profile, SSOMC, ST, WEATHER,   !Input
     &    SSOMC, ST, WEATHER,                     !Input
     &    Cells,                                  !Input,Output
     &    NH4, NO3, UPPM)                         !Output

!-----------------------------------------------------------------------
      USE Cells_2D
      USE GHG_mod
      USE ModuleData
      USE ModSoilMix
      IMPLICIT  NONE
      EXTERNAL SOILNI_INIT_2D, NCHECK_INORG_2D, NFLUX_2D, DENIT_CERES, 
     &  INCDAT, YR_DOY, DENIT_CERES_2D, SOILNIBAL_2D,OPSOILNI

      SAVE
!-----------------------------------------------------------------------
      CHARACTER*1 ISWNIT, MEHYD !, MEGHG

      LOGICAL IUON

      INTEGER DOY, DYNAMIC, INCDAT, IUYRDOY, IUOF, L, I, J
      INTEGER NLAYR, FurRow1, FurCol1
      INTEGER NSOURCE, YEAR, YRDOY
      INTEGER, DIMENSION(MaxRows,MaxCols) :: Cell_Type   

      REAL HalfRow, BEDWD, FertFactor
      REAL AD, AK, ALGFIX !, CW  
      REAL NFAC, NNOM
      REAL SNH4_AVAIL, SNO3_AVAIL, SUMFERT
      REAL SWEF, TFUREA   !, TFDENIT
      REAL WFSOM, WFUREA, XL, XMIN    !WFDENIT, 
!     REAL TMINERN(MaxCols), TIMMOBN(MaxCols), TLCH, TLCHD
!     REAL TLCH, TNOX
!     REAL TNH4(MaxCols), TNH4NO3, TNO3(MaxCols), UHYDR
!     REAL, DIMENSION(MaxCols) :: WTNUP, TNITRIFY, TUREA
!     REAL, DIMENSION(MaxRows) :: TNH4, TNO3, TMINERN, TIMMOBN
!     REAL, DIMENSION(MaxRows) :: WTNUP, TNITRIFY, TUREA
      REAL TNH4, TNO3,TUREA
!     Real TNOXD_Lr(NL)
      Real TNH4NO3, UHYDR
      REAL ADCOEF(NL)
      REAL, DIMENSION(MaxRows,MaxCols) ::  DLTSNH4_2D, DLTSNO3_2D
      REAL, DIMENSION(MaxRows,MaxCols) ::  DLTUREA_2D !, HFlux, VFlux
      REAL KG2PPM(NL), WCR(NL), NH4(NL), NO3(NL), UPPM(NL), 
     &         SNO3(NL), DLTSNO3(NL), SW(NL)  !, LITC(0:NL)
      REAL, DIMENSION(MaxRows,MaxCols) :: NH4_2D, NO3_2D,SNH4_2D,SNO3_2D
      REAL, DIMENSION(MaxRows,MaxCols) :: UREA_2D, UPPM_2D
      REAL PH(NL), LL(NL), DUL(NL), SAT(NL), BD(NL), DLAYR(NL)
      REAL SSOMC(0:NL), ST(NL)
      REAL, DIMENSION(MaxRows,MaxCols) :: SWV, TFNITY ! , Width 
      REAL, DIMENSION(MaxRows,MaxCols) :: UNH4_2D, UNO3_2D  !Uptake
      REAL, DIMENSION(MaxRows,MaxCols) :: ColFrac
      
      REAL IMM(0:NL,NELEM), MNR(0:NL,NELEM)

!     Variables added for flooded conditions analysis:
      
      INTEGER NBUND, NSWITCH
      INTEGER FERTDAY
      INTEGER DLAG_2D(MaxRows,MaxCols), LFD10  !REVISED-US
      REAL TMAX, TMIN, SRAD !, XHLAI, RAIN, SNOW, FLOOD, 
      REAL TKELVIN, TFACTOR, WFPL, WF2
      REAL PHFACT, T2, TLAG   !, DNFRATE
      REAL CUMFNRO
      REAL TOTAML
      REAL TOTFLOODN
!      REAL TOMINFOM, TOMINSOM, TNIMBSOM
      
      REAL, DIMENSION(MaxRows,MaxCols) ::NFlux_L,NFlux_R,NFlux_D,NFlux_U
      REAL, DIMENSION(MaxRows,MaxCols) ::NFlux_UREA_L,NFlux_UREA_R,
     &          NFlux_UREA_D,NFlux_UREA_U
      REAL TLeachD_UREA

!     *** TEMP CHP DEBUGGIN
!      REAL TNOM(MaxCols)
      REAL NNOM_a, NNOM_b
!     TEMP CHP
!      real TotDailyNetMiner, TotUptake
!      real TotNO3, TotNH4, TotN, LastTotN
!      real TotDeltNO3, TotDeltNH4, TotDeltN, LastTotDeltN
!
!      data TotN /0.0/
!      data TotDeltN /0.0/

      TYPE (N2O_type) N2O_DATA
!          Cumul,     Daily,    Layer ppm,     Layer kg
      REAL CNOX,      TNOXD,                   DENITRIF(MaxRows,MaxCols) !Denitrification
      REAL CNITRIFY,  TNITRIFY, NITRIFppm,     NITRIF(MaxRows,MaxCols)   !Nitrification 
      REAL CMINERN,   TMINERN              !Mineralization
      REAL CIMMOBN,   TIMMOBN              !Immobilization
      REAL CNETMINRN                       !Net mineralization
      REAL CNUPTAKE,  WTNUP                !N uptake
      REAL CLeach,    TLeachD              !N leaching
      REAL CNTILEDR,  NTILEDR              !N loss to tile (HJ added)
!      REAL CN2Onitrif,TN2OnitrifD,             N2Onitrif(NL)!N2O from nitrification
!      REAL CN2Odenit, TN2OdenitD,              N2ODenit(NL) !N2O from denitrification
!      REAL CNOflux,   TNOfluxD,                NOflux(NL)   !NO flux
!      REAL                                     nNOflux(NL)  !NO from nitrification
!      REAL                                     dNOflux(NL)  !NO from denitrification
!      REAL CN2,       TN2D,                    N2flux(NL)   !N2 (all from detnitrification)
      
!     Added for GHG model
!     REAL, DIMENSION(NL) :: dD0, NO_N2O_ratio
!     REAL, DIMENSION(0:NL) :: newCO2
!     REAL pn2onitrif, NH4_to_NO, NITRIF_to_NO
!     real nox_puls, krainNO, potential_NOflux, NITRIF_remaining
!     real canopy_reduction, NOAbsorp

!-----------------------------------------------------------------------
!     Constructed variables are defined in ModuleDefs.
      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH
      TYPE (SoilType)    SOILPROP !, SOILPROP_profile
      TYPE (FertType)    FERTDATA
      TYPE (WeatherType) WEATHER
      Type (CellType) Cells(MaxRows,MaxCols)
      !TYPE (CellStrucType) Struc(MaxRows,MaxCols)

      INTERFACE
        SUBROUTINE SoilNiBal(CONTROL, ISWITCH, 
     &      ALGFIX, CIMMOBN, CMINERN, CUMFNRO, FERTDATA, NBUND, CLeach,
     &      CNTILEDR, TNH4, TNO3, TOTAML, TOTFLOODN, TUREA, WTNUP,
     &      N2O_data) 
          USE GHG_mod
          USE FertType_mod
          TYPE (ControlType), INTENT(IN) :: CONTROL
          TYPE (SwitchType),  INTENT(IN) :: ISWITCH
          TYPE (FertType),    INTENT(IN) :: FertData
          TYPE (N2O_type), INTENT(IN), OPTIONAL :: N2O_DATA
          INTEGER, INTENT(IN) :: NBUND
          REAL, INTENT(IN) :: ALGFIX, CIMMOBN, CMINERN, CUMFNRO, CLeach,
     &      CNTILEDR, TNH4, TNO3, TOTAML, TOTFLOODN, TUREA, WTNUP
        END SUBROUTINE SoilNiBal
      END INTERFACE

!     Transfer values from constructed data types into local variables.
      DYNAMIC = CONTROL % DYNAMIC
      YRDOY   = CONTROL % YRDOY
      MEHYD   = ISWITCH % MEHYD
!      MEGHG   = ISWITCH % MEGHG
      FERTDAY = FERTDATA % FERTDAY

      SRAD = WEATHER % SRAD
      TMAX = WEATHER % TMAX
      TMIN = WEATHER % TMIN
!      RAIN = WEATHER % RAIN
      
      SWV = Cells % State % SWV
      UNH4_2D = Cells % Rate % NH4Uptake
      UNO3_2D = Cells % Rate % NO3Uptake
      SNH4_2D = Cells % State % SNH4
      SNO3_2D = Cells % State % SNO3

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!     ------------------------------------------------------------------
      ADCOEF = SOILPROP % ADCOEF 
      BD     = SOILPROP % BD     
      DLAYR  = SOILPROP % DLAYR  
      DUL    = SOILPROP % DUL    
      KG2PPM = SOILPROP % KG2PPM  
      LL     = SOILPROP % LL     
      NLAYR  = SOILPROP % NLAYR  
      PH     = SOILPROP % PH     
      SAT    = SOILPROP % SAT    
      WCR    = SOILPROP % WCR
      
      NSWITCH = ISWITCH % NSWI
      ISWNIT  = ISWITCH % ISWNIT

!     BED & Cell info
      Cell_Type = CELLS % Struc % CellType
      BEDWD   = BedDimension % BEDWD
      HalfRow = BedDimension % ROWSPC_cm / 2.
      FurRow1 = BedDimension % FurRow1
      FurCol1 = BedDimension % FurCol1
      ColFrac = BedDimension % ColFrac
      
!     Initialization
      TMINERN  = 0.0  !mineralization
      TIMMOBN  = 0.0  !immobilization
      TNITRIFY = 0.0  !nitrification
      TNOXD    = 0.0  !denitrification
      TLeachD  = 0.0  !leaching

      !*** temp debugging chp
!      TNOM = 0.0

!     Seasonal cumulative values, kg[N]/ha
      CMINERN  = 0.0  !mineralization
      CIMMOBN  = 0.0  !immobilization
      CNETMINRN= 0.0  !net mineralization
      CNITRIFY = 0.0  !nitrification
      CNUPTAKE = 0.0  !cumulative N uptake
      CNOX     = 0.0  !denitrification
      CLeach   = 0.0  !leaching
      CNTILEDR = 0.0  !N loss to tile drainage     !HJ added
      WTNUP  = 0.0    !N uptake
      TOTAML = 0.0    !Ammonia volatilization
!      N2Onitrif = 0.0 !N2O[N] from nitrification
!      CN2Odenit = 0.0 !N2O[N] from nitrification
!      CNOflux   = 0.0 !NO
!      CN2       = 0.0 !N2

      nitrif = 0.0
      denitrif = 0.0
!     N2O_data % wfps = 0.0
      
!!     proportion of N2O from nitrification PG calibrated this variable for DayCent
!      pn2Onitrif = .001

      TFNITY = 0.0    !
      IUOF   = 0
      IUON   = .FALSE.
      DLTSNO3 = 0.0
      SW = 0.0
      DLTSNO3_2D  = 0.0     
      DLTSNH4_2D  = 0.0      
      DLTUREA_2D  = 0.0
      DLAG_2D   = 0   !REVISED-US
      TOTFLOODN = 0.0
!      DO L = 1, NLAYR
!        N2O_data % wfps(L) = min (1.0, sw(L) / soilprop % poros(L))
!      ENDDO

      !Initialize uptake variables here, or they will have residual
      !  value on first day of multi-season runs.
      UNH4_2D   = 0.0
      UNO3_2D   = 0.0

      IF (INDEX('N',ISWNIT) > 0) RETURN

!     Set initial SOM and nitrogen conditions for each soil layer.
      CALL SoilNi_init_2D(CONTROL, 
     &    Cell_Type, SOILPROP, ST, NH4, NO3,      !Input
     &    NH4_2D, NO3_2D, SNH4_2D, SNO3_2D,       !Input
     &    TFNITY, UREA_2D)                        !Output

      CALL NCHECK_inorg_2D(CONTROL, 
     &  NH4_2D, NO3_2D, SNH4_2D, SNO3_2D, UREA_2D)         !Input

        CALL NFLUX_2D (DYNAMIC, 
     &  ADCOEF, BD, CELLS, ColFrac, DUL, SNO3_2D,         !Input
     &  NSOURCE, SWV,                                     !Input
     &  DLTSNO3_2D, CLeach, TLeachD, NFlux_L, NFlux_R, NFlux_D, NFlux_U)!Output

      SWEF = 0.9-0.00038*(DLAYR(1)-30.)**2

      LFD10 = CONTROL % YRSIM
      
      DO J = 1, NColsTot
        DO L = 1, NRowsTot
          SNO3(L)  = SNO3(L)  + SNO3_2D(L, J) * ColFrac(L, J)
!          SNH4(L)  = SNH4(L)  + SNH4_2D(L, J) * ColFrac(L, J)
          SNO3(L)  = SNO3(L)  + SNO3_2D(L, J) * ColFrac(L, J)
        END DO
      END DO
      
!      SELECT CASE(MEGHG)
!        CASE("1")
!          CALL Denit_DayCent (CONTROL, ISWNIT, 
!     &    dD0, newCO2, NO3, SNO3, SOILPROP, SW,       !Input
!     &    DLTSNO3,                                    !I/O
!     &    CNOX, TNOXD, N2O_data)                      !Output
!        CASE DEFAULT

!*******************************************************************************
!     temp chp - remove denitrification
    !     CALL Denit_Ceres (CONTROL, ISWNIT, 
    !&    DUL, FLOOD, KG2PPM, LITC, NLAYR, NO3, SAT,  !Input
    !&    SSOMC, SNO3, ST, SW,                        !Input
    !&    DLTSNO3,                                    !I/O
    !&    CNOX, TNOXD, N2O_data)                      !Output
          DENITRIF = 0.0
          CNOX = 0.0
          TNOXD = 0.0
!        END SELECT
!*******************************************************************************

!      CALL N2Oemit(CONTROL, ISWITCH, dD0, SOILPROP, N2O_DATA) 

!      CALL OpN2O(CONTROL, ISWITCH, SOILPROP, newCO2, N2O_DATA) 

!      IF (CONTROL%RUN .EQ. 1 .OR. INDEX('QF',CONTROL%RNMODE) .LE. 0)THEN
!        call nox_pulse (dynamic, rain, snow, nox_puls)
!      ENDIF

!      CALL ArrayHandler(CELLS,CONTROL,SOILPROP,SNO3_2D,"NO3",0.0,200.)

!     TEMP CHP
!      call SUM_N(Cell_Type, DLTSNO3_2D, DLTSNH4_2D, !Input
!     &      ColFrac, SNO3_2D, SNH4_2D,                    !Input
!     &      TotNO3, TotNH4, TotDeltNO3, TotDeltNH4,       !Output
!     &      TotN, TotDeltN, LastTotN, LastTotDeltN)       !Output
!      if (TotN + TotDeltN .NE. LastTotN) THEN
!        TotN = TotN + 0
!      endif

!***********************************************************************
!***********************************************************************
!     DAILY RATE CALCULATIONS 
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!     ------------------------------------------------------------------
      IF (INDEX('N',ISWNIT) > 0) RETURN

!     Initialize Soil N process rates for this time step.
      DLTUREA_2D = 0.0
      DLTSNO3_2D = 0.0 
      DLTSNH4_2D = 0.0 
      TNH4 = 0
      TNO3 = 0
      TUREA = 0 
      TNOXD = 0

!      TotUptake = 0.0

      DO L = 1, NRowsTot
        DO J = 1, NColsTot 
!         Update with yesterday's plant N uptake
          SNO3_2D(L, J) = SNO3_2D(L, J) - UNO3_2D(L, J)
          SNH4_2D(L, J) = SNH4_2D(L, J) - UNH4_2D(L, J)
!         KG2PPM(L) Conversion factor to switch from kg [N] / ha to ug [N] / g 
!         KG2PPM(L) = 10. / (BD(L) * DLAYR(L))
          NO3_2D(L, J)  = SNO3_2D(L, J) * KG2PPM(L) 
          NH4_2D(L, J)  = SNH4_2D(L, J) * KG2PPM(L) 

!          TotUptake = TotUptake + (UNO3_2D(L,J)+UNH4_2D(L,J))*ColFrac(L, J)
        ENDDO
      ENDDO

!-------------------------------------------------------------------------
!     FERTILIZER
!-------------------------------------------------------------------------
      IF (FERTDAY == YRDOY) THEN
!       Fertilizer was placed today
        SUMFERT = 0.0
        
        DO L = 1, NRowsTot 
          SUMFERT = SUMFERT + FERTDATA % ADDSNO3(L) + 
     &                     FERTDATA % ADDSNH4(L) + FERTDATA % ADDUREA(L)

          SELECT CASE (TRIM(FERTDATA % AppType))
          CASE ('BANDED','POINT')
!           Banded or point application goes to cell L,1
            DLTSNO3_2D(L,1) = DLTSNO3_2D(L,1) + FERTDATA % ADDSNO3(L)
     &           / ColFrac(L,1)
            DLTSNH4_2D(L,1) = DLTSNH4_2D(L,1) + FERTDATA % ADDSNH4(L)
     &           / ColFrac(L,1)
            DLTUREA_2D(L,1) = DLTUREA_2D(L,1) + FERTDATA % ADDUREA(L)
     &           / ColFrac(L,1)
           
          CASE ('DRIP')
!           Drip fertigation goes to cell DripRow,DripCol
            J = BedDimension % DripCol(FERTDATA%DrpRefIdx)
            I = BedDimension % DripRow(FERTDATA%DrpRefIdx)
            If (I .EQ. L) THEN
                DLTSNO3_2D(I,J) = DLTSNO3_2D(I,J) + FERTDATA %ADDSNO3(L)
     &              / ColFrac(I,J)
                DLTSNH4_2D(I,J) = DLTSNH4_2D(I,J) + FERTDATA %ADDSNH4(L)
     &              / ColFrac(I,J)
                DLTUREA_2D(I,J) = DLTUREA_2D(I,J) + FERTDATA %ADDUREA(L)
     &              / ColFrac(I,J)
            END IF
      ! Changed by JZW
!            DLTSNO3_2D(1,J) = DLTSNO3_2D(1,J) + FERTDATA % ADDSNO3(L)
!     &          / ColFrac(1, J)
!            DLTSNH4_2D(1,J) = DLTSNH4_2D(1,J) + FERTDATA % ADDSNH4(L)
!     &          / ColFrac(1, J)
!            DLTUREA_2D(1,J) = DLTUREA_2D(1,J) + FERTDATA % ADDUREA(L)
!     &          / ColFrac(1, J)

          CASE DEFAULT
            DO J = 1, NColsTot
              SELECT CASE (Cell_Type(L,J))
!             Within the bed, fertilizer is concentrated in bed cells
              CASE (3); 
                FertFactor = BedDimension%ROWSPC_cm /BEDWD
              CASE (4,5); FertFactor = 1.0
              CASE DEFAULT; CYCLE
              END SELECT

              DLTSNO3_2D(L, J) = DLTSNO3_2D(L, J) + 
     &          FERTDATA % ADDSNO3(L) * FertFactor
              DLTSNH4_2D(L, J) = DLTSNH4_2D(L, J) + 
     &          FERTDATA % ADDSNH4(L) * FertFactor 
              DLTUREA_2D(L, J) = DLTUREA_2D(L, J) + 
     &          FERTDATA % ADDUREA(L) * FertFactor
            ENDDO
          END SELECT

          IF (FERTDATA % ADDUREA(L) > 1.E-4) THEN
            IUON = .TRUE.
            IUYRDOY = INCDAT(YRDOY, 21)
            CALL YR_DOY (IUYRDOY, YEAR, IUOF)
          ENDIF
        ENDDO
      ENDIF

!     ----------------------------------------------------------------
!     If DOY=IUOF (has been set in Fert_Place), then all the urea has
!     hydrolyzed already.
      CALL YR_DOY (CONTROL%YRDOY, YEAR, DOY)
      IF (DOY .EQ. IUOF) THEN
        DO L = 1, NRowsTot
          DO J = 1, NColsTot
            DLTSNH4_2D(L, J) = DLTSNH4_2D(L, J) + UREA_2D(L, J)
            DLTUREA_2D(L, J) = DLTUREA_2D(L,J) - UREA_2D(L, J)
          END DO
        END DO
        IUON = .FALSE.
      ENDIF

!!-----------------------------------------------------------------------
!!     NOx pulse multiplier from DayCent
!      call nox_pulse (dynamic, rain, snow, nox_puls)
!      krainNO = nox_puls

!     ------------------------------------------------------------------
!     Loop through soil layers for rate calculations
!     ------------------------------------------------------------------
      TMINERN  = 0.0

!     TEMP CHP
!      TotDailyNetMiner = 0.0

      TIMMOBN  = 0.0
      TNITRIFY = 0.0
      TNOXD    = 0.0  !denitrification
      TLeachD  = 0.0  !leaching
      NTILEDR = 0.0   !N loss to tile !HJ added
      DO J = 1, NColsTot
        NNOM = 0.0
        DO L = 1, NRowsTot
          XMIN    = 0.
          IF (Cell_Type(L, J) .EQ. 0) THEN 
              cycle
          ENDIF
!       ----------------------------------------------------------------
!       Environmental limitation factors for the soil processes.
!       ----------------------------------------------------------------
          IF (SWV(L, J) .LE. DUL(L)) THEN
            AD  = WCR(L)
!           Soil water factor WFSOM.
            WFSOM = (SWV(L, J) - AD) / (DUL(L) - AD)
          ELSE
!           If the soil water content is higher than the drained upper 
!           limit (field capacity), calculate the excess water as fraction
!           of the maximum excess (i.e. when saturated).
            XL = (SWV(L, J) - DUL(L)) / (SAT(L) - DUL(L))
!           Soil water factor WFSOM.
            WFSOM = 1.0 - 0.5 * XL
          ENDIF   !End of IF block on SW vs. DUL.

!         Limit the soil water factors between 0 and 1.
          WFSOM = AMAX1 (AMIN1 (WFSOM, 1.), 0.)

!         Calculate the soil temperature factor for the urea hydrolysis.
          TFUREA = (ST(L) / 40.) + 0.20
          TFUREA = AMAX1 (AMIN1 (TFUREA, 1.), 0.)
        
!-------------------------------------------------------------------------
!         UREA hydrolysis
!-------------------------------------------------------------------------
          IF (IUON) THEN
!           Calculate the maximum hydrolysis rate of urea.
            AK = -1.12 + 1.31*SSOMC(L)*1.E-4 * KG2PPM(L) + 0.203*PH(L)
     &                - 0.155 * SSOMC(L) * 1.E-4 * KG2PPM(L) * PH(L)
            AK = AMAX1 (AK, 0.25)
        
!           Calculate the soil water factor for the urea hydrolysis, and
!           limit it between 0 and 1.
            WFUREA = WFSOM + 0.20
            WFUREA = AMAX1 (AMIN1 (WFUREA, 1.), 0.)
        
!           Calculate the amount of urea that hydrolyses.
            UHYDR = AK * AMIN1 (WFUREA, TFUREA) * (UREA_2D(L, J) 
     &              + DLTUREA_2D(L, J))
            UHYDR = AMIN1 (UHYDR, UREA_2D(L, J)+DLTUREA_2D(L, J))
        
            DLTUREA_2D(L, J) = DLTUREA_2D(L, J) - UHYDR 
            DLTSNH4_2D(L, J) = DLTSNH4_2D(L, J) + UHYDR 
          ENDIF   !End of IF block on IUON.
        
!-------------------------------------------------------------------------
!         Net mineralization rate - Nitrogen.
!-------------------------------------------------------------------------
!         Add in residual from previous layer to preserve N balance
          NNOM = NNOM + MNR(L,N) - IMM(L,N)

          SELECT CASE (Cell_Type(L,J))
          CASE (3) !bed
            IF (L == 1)  THEN
!             First layer takes mineralization from surface also.
!             JZW for plastic case, should have no mineralize on the surface
              NNOM = NNOM + MNR(0,N) - IMM(0,N) 
            ENDIF
          CASE (5)  !furrow
            IF (L == FurRow1)  THEN
!             First layer takes mineralization from surface also.
              NNOM = NNOM + MNR(0,N) - IMM(0,N) 
            ENDIF
          END SELECT

!         Mineralization
!         --------------
!         If the net N release from all SOM sources (NNOM) is positive,
!         add the mineralized N to the NH4 pool.
          IF (NNOM .GE. 0.0) THEN
            DLTSNH4_2D(L, J) = DLTSNH4_2D(L, J) + NNOM

!           temp chp
!            TotDailyNetMiner = TotDailyNetMiner + NNOM * ColFrac(L, J)

            NNOM = 0.

          ELSE
!         Immobilization
!         --------------
!           If NNOM is < 0, there is immobilization. If the N demand
!           of the immobilization is greater than the amount of NH4
!           available, take all NH4, leaving behind a minimum amount of
!           NH4 equal to XMIN (NNOM is negative!).
            IF (ABS(NNOM) .GT. (SNH4_2D(L, J) - XMIN)) THEN
              NNOM_a = -(SNH4_2D(L, J) - XMIN + DLTSNH4_2D(L, J))
              NNOM_b = NNOM - NNOM_a

              DLTSNH4_2D(L, J) = -(SNH4_2D(L, J) - XMIN)

!           temp chp
!            TotDailyNetMiner = TotDailyNetMiner 
!     &          - (SNH4_2D(L, J) - XMIN) * ColFrac(L, J)


              NNOM = NNOM_b

              SNO3_AVAIL = SNO3_2D(L, J) + DLTSNO3_2D(L, J)
              IF (ABS(NNOM) .GT. (SNO3_AVAIL - XMIN)) THEN
                !Not enough SNO3 to fill remaining NNOM, leave residual
                DLTSNO3_2D(L, J) = DLTSNO3_2D(L, J) + XMIN - SNO3_AVAIL

!           temp chp
!            TotDailyNetMiner = TotDailyNetMiner + 
!     &              (XMIN - SNO3_AVAIL) * ColFrac(L, J)

                NNOM = NNOM + SNO3_AVAIL - XMIN

              ELSE
!               Get the remainder of the immobilization from nitrate (NNOM
!               is negative!)
                DLTSNO3_2D(L, J) = DLTSNO3_2D(L, J) + NNOM

!           temp chp
!            TotDailyNetMiner = TotDailyNetMiner + NNOM * ColFrac(L, J)

                NNOM = 0.

              ENDIF

            ELSE
!             Reduce soil NH4 by the immobilization (NNOM is
!             negative!).
              DLTSNH4_2D(L, J) = DLTSNH4_2D(L, J) + NNOM

!           temp chp
!            TotDailyNetMiner = TotDailyNetMiner + NNOM * ColFrac(L, J)

              NNOM = 0.0
            ENDIF   !End of IF block on ABS(NNOM).
          ENDIF   !End of IF block on NNOM.

!     TEMP CHP - mineralization
!      call SUM_N(Cell_Type, DLTSNO3_2D, DLTSNH4_2D, !Input
!     &      ColFrac, SNO3_2D, SNH4_2D,                    !Input
!     &      TotNO3, TotNH4, TotDeltNO3, TotDeltNH4,       !Output
!     &      TotN, TotDeltN, LastTotN, LastTotDeltN)       !Output
!      if (MNR(L,N) -IMM(L,N) - TotDeltN + LastTotDeltN > 0.00001) THEN
!        TotN = TotN + 0
!      endif

!-----------------------------------------------------------------------
!         Nitrification section
!-----------------------------------------------------------------------
!         Nitrification based on Gilmour
          TKELVIN = ST(L) + 273.0
          TFACTOR = EXP(-6572 / TKELVIN + 21.4)  !6602
          WFPL = SWV(L, J) / SAT(L) 
          IF (SWV(L, J) .GT. DUL(L)) THEN
            WF2 = -2.5 * WFPL + 2.55
          ELSE
            IF (WFPL .GT. 0.4) THEN
              WF2 = 1.0
            ELSE
              WF2 = 3.15 * WFPL - 0.1
            ENDIF
          ENDIF
          WF2 = AMAX1 (WF2, 0.0)
          WF2 = AMIN1 (1.0, WF2)

          PHFACT  = AMIN1 (1.0, 0.33 * PH(L) - 1.36)

          T2   = AMAX1 (0.0,(TFNITY(L, J) - 1.0))
          TLAG = 0.075 * T2**2
          TLAG = AMIN1 (TLAG,1.0)
          IF (NBUND <= 0.) THEN
            TLAG = 1.0
          ENDIF
          NFAC = AMAX1(0.0, AMIN1(1.0, TFACTOR * WF2 * PHFACT * TLAG))
          NITRIFppm = NFAC * NH4_2D(L, J)

          IF (NSWITCH .EQ. 5) THEN
            NITRIF(L, J) = 0.0
          ELSE
!           NITRIFppm in ppm; NITRIF in kg/ha
!           changed by PG from * kg2ppm
            NITRIF(L, J)  = NITRIFppm / KG2PPM(L) 
          ENDIF

          IF (NH4_2D(L, J).LE. 0.01) THEN
            TFNITY(L, J) = 0.0
          ELSE
            IF (SWV(L, J) .LT. SAT(L)) THEN
              TFNITY(L, J) = TFNITY(L, J) + 1.0
            ENDIF
          ENDIF

          XMIN = 0.0
          SNH4_AVAIL = AMAX1(0.0, SNH4_2D(L, J) + DLTSNH4_2D(L, J)-XMIN)
          NITRIF(L, J) = AMIN1(NITRIF(L, J), SNH4_AVAIL)

          DLTSNO3_2D(L, J) = DLTSNO3_2D(L, J) + NITRIF(L, J)
          DLTSNH4_2D(L, J) = DLTSNH4_2D(L, J) - NITRIF(L, J)
!         This contribution from NH4 can be considered as part of the nitrification process
          TNITRIFY = TNITRIFY  + NITRIF(L, J) * ColFrac(L, J)

!     TEMP CHP nitrification
!      call SUM_N(Cell_Type, DLTSNO3_2D, DLTSNH4_2D, !Input
!     &      ColFrac, SNO3_2D, SNH4_2D,                    !Input
!     &      TotNO3, TotNH4, TotDeltNO3, TotDeltNH4,       !Output
!     &      TotN, TotDeltN, LastTotN, LastTotDeltN)       !Output
!      if (TotDeltN - LastTotDeltN > 0.00001 .OR. TotDeltN - LastTotDeltN < -0.00001) THEN
!        TotN = TotN + 0
!      endif

        END DO   !End of soil column loop.
      End Do !End of soil layer loop. 

!-----------------------------------------------------------------------
!       Denitrification section
!-----------------------------------------------------------------------
      IF (NSWITCH .EQ. 6) THEN
        DENITRIF = 0.0
      ELSE

!        SELECT CASE(MEGHG)
!        CASE("1","2")
!          CALL Denit_DayCent (CONTROL, ISWNIT, 
!     &    dD0, newCO2, NO3, SNO3, SOILPROP, SW,       !Input
!     &    DLTSNO3,                                    !I/O
!     &    CNOX, TNOXD, N2O_data)                      !Output
!
!        CASE DEFAULT

!*******************************************************************************
!     temp chp - remove denitrification
    !     CALL Denit_Ceres_2D (CONTROL, ISWNIT, 
    !&    DUL, KG2PPM, LITC, NLAYR, NO3_2D,           !Input
    !&    SAT, SSOMC, SNO3_2D, ST, SWV, Cells,        !Input
    !&    ColFrac,                                    !Input
    !&    DLTSNO3_2D,                                 !I/O
    !&    CNOX, TNOXD, DENITRIF)                      !Output
!        END SELECT
          DENITRIF = 0.0
          CNOX = 0.0
          TNOXD = 0.0
      ENDIF
!*******************************************************************************

!      CALL PUT('NITR','TNOXD',TNOXD) 
!      N2ODenit = N2O_data % N2ODenit

      !     TEMP CHP denitrification
!      call SUM_N(Cell_Type, DLTSNO3_2D, DLTSNH4_2D, !Input
!     &      ColFrac, SNO3_2D, SNH4_2D,                    !Input
!     &      TotNO3, TotNH4, TotDeltNO3, TotDeltNH4,       !Output
!     &      TotN, TotDeltN, LastTotN, LastTotDeltN)       !Output
!      if (DENITRIF(L, J) + TotDeltN .NE. LastTotN) THEN
!        TotN = TotN + 0
!      endif

!     ------------------------------------------------------------------
!     Downward and upward N movement with the water flow.
!     ------------------------------------------------------------------
      TLeachD = 0.0
      NTILEDR = 0.0   !HJ added

      IF (IUON) THEN
        NSOURCE = 1    !Urea.
        CALL NFLUX_2D (DYNAMIC, 
     &    ADCOEF, BD, CELLS, ColFrac, DUL, UREA_2D,              !Input
     &    NSOURCE, SWV,                                          !Input
     &    DLTUREA_2D, CLeach, TLeachD_UREA,                      !Output
     &    NFlux_UREA_L, NFlux_UREA_R, NFlux_UREA_D, NFlux_UREA_U)!Output
      ENDIF

      NSOURCE = 2   !NO3.
      CALL NFLUX_2D (DYNAMIC, 
     &    ADCOEF, BD, CELLS, ColFrac, DUL, SNO3_2D,       !Input
     &    NSOURCE, SWV,                                   !Input
     &    DLTSNO3_2D, CLeach, TLeachD, NFlux_L, NFlux_R,  !Output
     &    NFlux_D, NFlux_U)                               !Output
     
      IF (IUON) THEN
        TLeachD = TLeachD + TLeachD_UREA
        NFlux_L = NFlux_L + NFlux_UREA_L
        NFlux_R = NFlux_R + NFlux_UREA_R
        NFlux_D = NFlux_D + NFlux_UREA_D
        NFlux_U = NFlux_U + NFlux_UREA_U
      ENDIF

!     TEMP CHP
!      call SUM_N(Cell_Type, DLTSNO3_2D, DLTSNH4_2D, !Input
!     &      ColFrac, SNO3_2D, SNH4_2D,                    !Input
!     &      TotNO3, TotNH4, TotDeltNO3, TotDeltNH4,       !Output
!     &      TotN, TotDeltN, LastTotN, LastTotDeltN)       !Output

  !    CAll Interpolate2Layers_2D(TNOXD, Cells%Struc, NLAYR,  !input
   !  &         TNOXD_Lr) 
!       JZW in SOILNI, the TNOXD is scalar, put a scalar variable. In Interpolate2Layers_2D, TNOXD should be 2D
!     Cumulative denitrification across the total soil profile
      CALL PUT('NITR','TNOXD', TNOXD) 
      !CALL PUT('NITR','TNOXD',ARNTRF) !Daily nitrification rate
      CALL PUT('NITR','TLCHD', TLeachD) 

      Cells % State % SNH4 = SNH4_2D  !kg/ha
      Cells % State % SNO3 = SNO3_2D  !kg/ha
      Cells % State % UREA = UREA_2D

      ! Convert 2D to 1D
      ! Integrated cell variable into layer variable   
      CAll Interpolate2Layers_2D(NH4_2D, Cells%Struc, NLAYR,  !input
     &         NH4)                                           !Output
      CAll Interpolate2Layers_2D(NO3_2D, Cells%Struc, NLAYR,  !input
     &         NO3)                                           !Output

!***********************************************************************
!***********************************************************************
!     END OF FIRST DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF

!***********************************************************************
!***********************************************************************
!     DAILY INTEGRATION (also performed for seasonal initialization)
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT .OR. DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------
      IF (INDEX('N',ISWNIT) > 0) RETURN

!     Loop through soil layers for integration
      DO L = 1, NRowsTot
        DO J = 1, NColsTot
          SNO3_2D(L, J) = SNO3_2D(L, J) + DLTSNO3_2D(L, J)    
          SNH4_2D(L, J) = SNH4_2D(L, J) + DLTSNH4_2D(L, J)    
          UREA_2D(L, J) = UREA_2D(L, J) + DLTUREA_2D(L, J)

!         Underflow trapping
          IF (ABS(SNO3_2D(L, J)) .LT. 1.E-8) SNO3_2D(L, J) = 0.0
          IF (ABS(SNH4_2D(L, J)) .LT. 1.E-8) SNH4_2D(L, J) = 0.0
          IF (ABS(UREA_2D(L, J)) .LT. 1.E-8) UREA_2D(L, J) = 0.0

!         Conversions.
          NO3_2D(L, J) = SNO3_2D(L, J) * KG2PPM(L)
          NH4_2D(L, J) = SNH4_2D(L, J) * KG2PPM(L)
          UPPM_2D(L,J) = UREA_2D(L, J) * KG2PPM(L)
        ENDDO
      ENDDO  
                                                
!     Call NCHECK to check for and fix negative values.
      CALL NCHECK_inorg_2D(CONTROL, 
     &      NH4_2D, NO3_2D, SNH4_2D, SNO3_2D, UREA_2D) !Input 
     
!     Soil profile accumulations.
      TNH4   = 0.0
      TNO3   = 0.0
      TUREA  = 0.0
      DO L = 1, NRowsTot
        DO J = 1, NColsTot
            if (Cell_Type(L,J) .EQ. 0) then
!              if (SNO3_2D(L, J) .NE. 0 .OR. SNH4_2D(L, J) .NE. 0) then
!                TNH4  = TNH4  +  0
!              endif
                cycle
            endif
!          TNH4  = TNH4  + SNH4_2D(L, J)
!          TNO3  = TNO3  + SNO3_2D(L, J)
!          TUREA = TUREA + UREA_2D(L, J)
          TNH4  = TNH4  + SNH4_2D(L, J) * ColFrac(L, J)
          TNO3  = TNO3  + SNO3_2D(L, J) * ColFrac(L, J)
          TUREA = TUREA + UREA_2D(L, J) * ColFrac(L, J)
!         WTNUP in g[N]/m2 - convert to kg/ha in CNUPTAKE (below) 
          WTNUP = WTNUP + (UNO3_2D(L,J) + UNH4_2D(L,J))/10.*ColFrac(L,J)

!          if (L == Cell_detail%row .and. J == Cell_detail%col) then
!             Cell_Ndetail % NFlux_L_out  = NFlux_L(L, J) !CELLS % RATE % SWFlux_L
!             Cell_Ndetail % NFlux_R_out  = NFlux_R(L, J)
!             Cell_Ndetail % NFlux_D_out  = NFlux_D(L, J)
!             Cell_Ndetail % NFlux_U_out  = NFlux_U(L, J) 
!          elseif (L == Cell_detail%row - 1 .and. J == Cell_detail%col) then
!              Cell_Ndetail % NFlux_U_in  = NFlux_D(L, J)
!          elseif (L == Cell_detail%row + 1 .and. J == Cell_detail%col) then
!              Cell_Ndetail % NFlux_D_in  = NFlux_U(L, J)
!          elseif (L == Cell_detail%row .and. J == Cell_detail%col - 1) then
!              Cell_Ndetail % NFlux_L_in  = NFlux_R(L, J)
!          elseif (L == Cell_detail%row .and. J == Cell_detail%col + 1) then
!              Cell_Ndetail % NFlux_R_in  = NFlux_L(L, J)
!          Endif

             Cell_Ndetail % NFlux_L_out(L, J)  = NFlux_L(L, J) 
             Cell_Ndetail % NFlux_R_out(L, J)  = NFlux_R(L, J)
             Cell_Ndetail % NFlux_D_out(L, J)  = NFlux_D(L, J)
             Cell_Ndetail % NFlux_U_out(L, J)  = NFlux_U(L, J) 
          if (L > 1) then
              if (Cell_Type(L - 1, J) .NE. 0) then
                Cell_Ndetail % NFlux_U_in(L, J)  = NFlux_D(L - 1, J)
     &                          * ColFrac(L-1, j) / ColFrac(L, J)
              endif
          endif
          if (L < NRowsTot) then
              Cell_Ndetail % NFlux_D_in(L, J)  = NFlux_U(L + 1, J)
     &                           * ColFrac(L+1, j) / ColFrac(L, J)
          endif
          if (J > 1) then
              Cell_Ndetail % NFlux_L_in(L, J)  = NFlux_R(L, J - 1)
     &                           * ColFrac(L, j-1) / ColFrac(L, J)
          endif
          if (J < NColsTot) then
              if (Cell_Type(L, J + 1) .NE. 0) then
                Cell_Ndetail % NFlux_R_in(L, J)  = NFlux_L(L, J + 1)
     &                               * ColFrac(L, j+1) / ColFrac(L, J)
              endif
          Endif
        ENDDO
      ENDDO

      TNH4NO3 = TNH4 + TNO3

      CELLS%State%SNH4 = SNH4_2D
      CELLS%State%SNO3 = SNO3_2D
      
!     Convert 2D to 1D
!     Integrated cell variable into layer variable   
!     JZW require MaxRows > NLAYR
      CAll Interpolate2Layers_2D(NH4_2D, Cells%Struc, NLAYR,  !input
     &         NH4)                                           !Output
      CAll Interpolate2Layers_2D(NO3_2D, Cells%Struc, NLAYR,  !input
     &         NO3)                                           !Output
!      CAll Interpolate2Layers_2D(SNO3_2D, Cells%Struc, NLAYR,  !input
!     &         SNO3)                                           !Output
      CAll Interpolate2Layers_2D(UPPM_2D, Cells%Struc, NLAYR,  !input
     &         UPPM)                                           !Output

!     Transfer daily mineralization values for use by Cassava model
!      CALL GET('ORGC','TOMINFOM' ,TOMINFOM) !Miner from FOM (kg/ha)
!      CALL GET('ORGC','TOMINSOM' ,TOMINSOM) !Miner from SOM (kg/ha)
!      CALL GET('ORGC','TNIMBSOM', TNIMBSOM) !Immob (kg/ha)
!
!      TMINERN = TOMINFOM + TOMINSOM
!      TIMMOBN = TNIMBSOM
      
      TMINERN = 0.0
      TIMMOBN = 0.0
      DO L = 1, NRowsTot
          TMINERN = TMINERN + MNR(L, N) / BedDimension % BedFrac(L, 1)
          TIMMOBN = TIMMOBN + IMM(L, N) / BedDimension % BedFrac(L, 1)
      END DO

!     Seasonal cumulative values
      CMINERN  = CMINERN  + TMINERN 
      CIMMOBN  = CIMMOBN  + TIMMOBN 
      CNITRIFY = CNITRIFY + TNITRIFY
      CNETMINRN= CMINERN  - CIMMOBN
      CNUPTAKE = WTNUP * 10. !WTNUP is cumulative N uptake in g[N]/m2

      IF (DYNAMIC .EQ. SEASINIT) THEN
!         TOTAML is not assigned yet. It should be sum of cell value when assign

!**********************************************************************************
        CALL SoilNiBal (CONTROL, ISWITCH, 
     &    ALGFIX, CIMMOBN, CMINERN, CUMFNRO, FERTDATA, NBUND, CLeach,  
     &    CNTILEDR, TNH4, TNO3, TOTAML, TOTFLOODN, TUREA, WTNUP,
     &    N2O_data) 

        Call SoilNiBal_2D (CONTROL, ISWITCH, 
     &    FERTDATA, TNH4, TNO3, TUREA,   
     &    Cells, DENITRIF, IMM, MNR) 

        CALL OpSoilNi(CONTROL, ISWITCH, SoilProp, 
     &    CIMMOBN, CMINERN, CNETMINRN, CNITRIFY, CNUPTAKE, 
     &    FertData, NH4, NO3, 
     &    CLeach, CNTILEDR, TNH4, TNH4NO3, TNO3, TUREA, CNOX, TOTAML)

!        CALL OpSoilNi_2D(CONTROL, ISWITCH, SoilProp, 
!     &    CIMMOBN, CMINERN, CNETMINRN, CNITRIFY, CNUPTAKE, 
!     &    FertData, NH4_2D, NO3_2D, 
!     &    CLeach, TNH4, TNH4NO3, TNO3, TNOXD, TOTAML)
      ENDIF

      Cells % State % SNH4 = SNH4_2D  !kg/ha
      Cells % State % SNO3 = SNO3_2D  !kg/ha
      Cells % State % UREA = UREA_2D

!***********************************************************************
!***********************************************************************
!     OUTPUT
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT .OR. DYNAMIC .EQ. SEASEND) THEN
C-----------------------------------------------------------------------
      IF (INDEX('N',ISWNIT) > 0) RETURN

C     Write daily output
      CALL OpSoilNi(CONTROL, ISWITCH, SoilProp, 
     &    CIMMOBN, CMINERN, CNETMINRN, CNITRIFY, CNUPTAKE, 
     &    FertData, NH4, NO3, 
     &    CLeach, CNTILEDR, TNH4, TNH4NO3, TNO3, TUREA, CNOX, TOTAML)

!      CALL OpSoilNi_2D(CONTROL, ISWITCH, SoilProp, 
!     &    CIMMOBN, CMINERN, CNETMINRN, CNITRIFY, CNUPTAKE, 
!     &    FertData, NH4_2D, NO3_2D, 
!     &    CLeach, TNH4, TNH4NO3, TNO3, TNOXD, TOTAML)
!**********************************************************************************
        CALL SoilNiBal (CONTROL, ISWITCH, 
     &    ALGFIX, CIMMOBN, CMINERN, CUMFNRO, FERTDATA, NBUND, CLeach,  
     &    CNTILEDR, TNH4, TNO3, TOTAML, TOTFLOODN, TUREA, WTNUP,
     &    N2O_data) 

        Call SoilNiBal_2D (CONTROL, ISWITCH, 
     &    FERTDATA, TNH4, TNO3, TUREA,   
     &    Cells, DENITRIF, IMM, MNR) 

!      CALL ArrayHandler(CELLS,CONTROL,SOILPROP,SNO3_2D,"NO3",0.0,200.)

C***********************************************************************
C***********************************************************************
C     END OF SECOND DYNAMIC IF CONSTRUCT
C***********************************************************************
      ENDIF
C-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE SoilNi_2D

!=======================================================================
! NTRANS Variables - updated 08/20/2003
!-----------------------------------------------------------------------
! AD             Lowest possible volumetric water content of a soil layer 
!                  when it is air dry. For the top soil layer AD may be 
!                  lower than the lower limit LL, because this layer may dry 
!                  out due to water evaporation at the soil surface.
!                  (cm3 [H2O]/ cm3 [soil])
! ADCOEF(L)      Anion adsorption coefficient for soil layer L;  for reduced 
!                  anion (nitrate) flow in variable-charge soils (ADCOEF = 0 
!                  implies no anion retention) (cm3 (H2O] / g [soil])
! ADDSNH4(L)     Rate of change of ammonium in soil layer L (kg [N] / ha / d)
! ADDSNO3(L)     Rate of change of nitrate in soil layer L (kg [N] / ha / d)
! ADDUREA(L)     Rate of change of urea content in soil layer L
!                     (kg [N] / ha / d)
! AK             Maximum hydrolysis rate of urea (i.e. proportion of urea 
!                  that will hydrolyze in 1 day under optimum conditions). 
!                  AK >= 0.25. Also: Today's value of the nitrification 
!                  potential, calculated from the previous day's value (d-1)
! ALGFIX         N in algae (kg [N] / ha)
! ALI            
! AMTFER         Cumulative amount of N in fertilizer applications
! BD(L)          Bulk density, soil layer L (g [soil] / cm3 [soil])
! BD1            Bulk density of oxidized layer (g [soil] / cm3 [soil])
! CIMMOBN        Cumulative seasonal net immobilization of N in soil profile (kg[N]/ha) 
! CLeach         Season cumulative N leached from soil (kg [N] / ha)
! CMINERN        Cumulative seasonal mineralization of N in soil profile (kg[N]/ha)
! CNETMINRN      Cumulative seasonal net mineralization of N in soil profile (kg[N]/ha) 
!                = mineralization - immobilization
! CNITRIFY       Cumulative nitrification (kg [N] / ha)
! CNUPTAKE       Cumulative N uptake
! ColFrac(col)   Width of each column as a fraction of the total row width
! CONTROL        Composite variable containing variables related to control 
!                  and/or timing of simulation.  The structure of the 
!                  variable (ControlType) is defined in ModuleDefs.for. 
! CUMFNRO        Cumulative N lost in runoff over bund (kg [N] / ha)
! CW             Water extractable SOM carbon (µg [C] / g [soil])
! DENITRIF(L, J) Denitrification rate (kg [N] / ha / d)
! DLAG_2D(L, J)    Number of days with soil water content greater than the 
!                  drained upper limit for soil layer L.  For 
!                  denitrification to occur, DLAG must be greater than 4 (4 days
!                  lag period before denitrification begins). 
! DLAYR(L)       Thickness of soil layer L (cm)
! DLTFON(L)      Rate of change of N in fresh organic residue  in soil layer 
!                  L (kg [N] / ha / d)
! DLTFPOOL(L,J)  Rate of change of FOM pool (FPOOL) in soil layer L 
!                  (J=1=carbohydrate, 2=cellulose, 3=lignin)
!                  (kg [residue pool] / ha / d)
! DLTSNH4_2D(L,J))Rate of change of ammonium in soil cell
!                   (kg [N] / ha / d)
! DLTSNO3_2D(L,J) Rate of change of nitrate in soil cell (kg [N] / ha / d)
! DLTUREA_2D(L,J) Rate of change of urea content in soil cell
!                   (kg [N] / ha / d)
! DMINR          Maximum decomposition rate constant of stable organic 
!                  matter (d-1)
! DNFRATE        Maximum denitrification rate (kg [N] / ha / d)
! DOY            Current day of simulation (d)
! DRN(L)         Drainage rate through soil layer L (cm/d)
! DSNC           Depth to which C and N are integrated across all soil 
!                  layers for output in CARBON.OUT (cm)
! DUL(L)         Volumetric soil water content at Drained Upper Limit in 
!                  soil layer L (cm3[water]/cm3[soil])
! FERTDAY        Date of last fertilizer application (YYYYDDD)
! FERTDATA       Fertilizer type
! FertFactor     Ratio between ROWS space and BEDWD
! FOM(L)         Fresh organic residue in soil layer L
!                  (kg [dry matter] / ha)
! FON(L)         Nitrogen in fresh organic matter in soil layer L
!                  (kg [N] / ha)
! FPOOL(L,J)     FOM pool in soil layer L: J=1:carbohydrate, 2:cellulose, 
! HFlux          The daily soil water flux from cell (i,j) to cell (i,j+1) in cm3.  
! HUMFRAC        Humus fraction that decomposes (fraction)     
! IMM            Cell immobilization of element IEL (1=N, 2=P) in soil layer L . (kg/ha)
!                The amounts of inorganic N and P that are removed from 
!                 the soil (immobilized) when organic matter decomposes.   
! ISWITCH        Composite variable containing switches which control flow 
!                  of execution for model.  The structure of the variable 
!                  (SwitchType) is defined in ModuleDefs.for. 
! ISWNIT         Nitrogen simulation switch (Y or N) 
! IUOF           Critical Julian day when all urea is assumed to be 
!                  hydrolyzed (this is assumed to occur 21 days after the 
!                  urea application) (d)
! IUON           Flag indicating presence of urea (true or false) 
! KG2PPM(L)      Conversion factor to switch from kg [N] / ha to µg [N] / g 
!                  [soil] for soil layer L 
! LFD10          Date, 10 days after last fertilization.  Used to determine 
!                  whether hourly flood chemistry computations will be done 
!                  (see DAILY variable). (YYYYDDD)
! LITC           Carbon in fresh organic matter in units of kg[C]/ha
!                  soil evaporation: + = upward, -- = downward (cm/d)
!                  3:lignin (kg [residue pool] / ha)
! LL(L)          Volumetric soil water content in soil layer L at lower 
!                  limit (cm3 [water] / cm3 [soil])
! MaxCols
! MaxRows        Equals to NL
! MNR(L,IEL)     Cell mineralization of element IEL (1=N, 2=P) in soil layer cell 
!                  The amounts of inorganic N and P that are added to the soil
!                  when organic matter decomposes.  (kg/ha/d)
! NBUND          Number of bund height records 
! NColsTot       
! NH4_2D(L,J)    Ammonium N in soil cell (µg[N] / g[soil])
! NH4(L)         Ammonium N in soil layer L (µg[N] / g[soil])
! NITRIFppm      Nitrification rate (kg [N] / ha - d)
! NITRIF(L,J)    Cell daily nitrification rate (kg [N] / ha / d)
! NL             Maximum number of soil layers = 20 
! NLAYR          Actual number of soil layers 
! NNOM           Net mineral N release from all SOM sources (kg [N] / ha)
! NO3_2D(L,J)    Nitrate N in soil cell (µg[N] / g[soil])
! NO3(L)         Nitrate N in soil layer L (µg[N] / g[soil])
! NRowsTot       Equals to NLAYR
! NSOURCE        Flag for N source (1 = urea, 2 = NO3) 
! NSWITCH        Nitrogen switch - can be used to control N processes (0-No 
!                  N simulated, 1-N simulated, 5-Nitrification off, 
!                  6-denitrification off, 7-floodwater loss off, 8-drainage 
!                  losses off, 9-leaching off, 10-runoff losses off 
! OXLAYR         Composite variable which contains data about oxidation 
!                  layer.  See ModuleDefs.for for structure. 
! PH(L)          pH in soil layer L 
! PHFACT         Effect of pH on various soil N processes; value varies with 
!                  routine 
! SAT(L)         Volumetric soil water content in layer L at saturation
!                  (cm3 [water] / cm3 [soil])
! SNH4_2D(L,J)   Total extractable ammonium N in soil layer L (kg [N] / ha)
! SNH4_AVAIL     Maximum amount of NH4 that may nitrify (kg [N] / (ha - d))
! SNO3_2D(L,J)   Total extractable nitrate N in soil layer L (kg [N] / ha)
! SNO3_AVAIL     NO3 available for denitrification (kg [N] / ha)
! SOILPROP       Composite variable containing soil properties including 
!                  bulk density, drained upper limit, lower limit, pH, 
!                  saturation water content.  Structure defined in ModuleDefs. 
! SRAD           Solar radiation (MJ/m2-d)
! SSOMC(L)       Carbon in stable organic matter (humus) (kg [C] / ha)
! ST(L)          Soil temperature in soil layer L (°C)
! SUMFERT
! SWV(L,J)       Volumetric soil water content in layer L
!                  (cm3 [water] / cm3 [soil])
! SWEF           Soil water evaporation fraction; fraction of lower limit 
!                  content to which evaporation can reduce soil water 
!                  content in top layer (fraction)
! T2             Temperature factor for nitrification 
! TFACTOR        Temperature factor for nitrification 
! TFDENIT        Temperature factor for denitrification rate (range 0-1) 
! TFNITY(L,J)    Yesterday's soil temperature factor for nitrification 
!                  (range 0-1) 
! TFUREA         Soil temperature factor for urea hydrolysis (range 0-1) 
! TIMMOBILIZE    Cumulative N immoblized (kg [N] / ha)
! TIMMOBN(J)     Cumulative N immobilization (kg [N] / ha /d)for Jth column
! TKELVIN        Soil temperature (oK)
! TLAG           Temperature factor for nitrification (0-1) 
! TLeachD        Daily cumulative N leached from soil (kg [N] / ha)
! TMAX           Maximum daily temperature (°C)
! TMIN           Minimum daily temperature (°C)
! TMINERALIZE    Cumulative mineralization (kg [N] / ha)
! TMINERN        Daily cumulative mineralization (kg [N]/ha/d) 
! TNH4           Daily total extractable ammonium N in soil profile (kg [N] / ha /d) in soil profile
! TNH4NO3        Daily total amount of inorganic N (NH4 and NO3) across soil 
!                  profile (kg [N] / ha)
! TNITRIFY       Daily cumulative nitrification (kg [N]/ha/d)
! TNO3           Daily total extractable nitrate N in soil profile (kg [N] / ha /d)
! CNOX           Season cumulative denitrification across the total soil profile adding to 
!                  the nitrous oxide (NOx) pool of the air (kg [N] / ha)
! TNOXD          Daily total denitrification across the total soil profile adding to 
!                  the nitrous oxide (NOx) pool of the air (kg [N] / ha)
! TotDailyNetMiner
! TOTAML         Cumulative ammonia volatilization (kg [N] / ha)
! TotUptake      Total uptake
! TUREA          Daily total urea in soil profile (kg [N] / ha /d)
! UHYDR          Rate of urea hydrolysis (kg [N] / ha - d)
! UNH4_2D(L,J)   Rate of root uptake of NH4, computed in NUPTAK
!                 (kg [N] / ha - d)
! UNO3_2D(L,J)   Rate of root uptake of NO3, computed in NUPTAK
!                 (kg [N] / ha -d)
! UPFLOW(L)      Movement of water between unsaturated soil layers due to 
! UREA_2D(L,J)   Amount of urea in soil cell (kg [N] / ha)
! UREA(L)        Amount of urea in soil layer L (kg [N] / ha)
! VFlux          The daily soil water flux from cell (i,j) to cell (i+1,j) in cm3. 
! WF2            Water factor for nitrification 
! WFDENIT        Soil water factor for denitrification rate (range 0-1) 
! WFPL           Water factor for nitrification 
! WFSOM          Reduction factor for (JZW should be SOM) FOM decay based on soil water content 
!                  (no reduction for SW = DUL, 100% reduction for SW = LL, 
!                  50% reduction for SW = SAT) 
! WFUREA         Reduction of urea hydrolysis due to soil water content 
!                  (range = 0-1; no reduction for SW = DUL, 20% reduction 
!                  for SW = LL, 70% reduction for SW = SAT (fraction)
! WTNUP          Daily Cumulative N uptake (g[N] / m2 /d) in soil profile
! XL             Excess water (above DUL) as a fraction of the maximum 
!                  amount of excess water (i.e. saturated). (fraction)
! XMIN           Amount of NH4 that cannot be immobilized but stays behind 
!                  in soil as NH4; Also, Amount of NO3 that cannot denitrify 
!                  but stays behind in the soil as NO3 (kg [N] / ha)
! YEAR           Year of current date of simulation 
! YRDOY          Current day of simulation (YYDDD)
!***********************************************************************
! Jin Question:

!Line 184:  UREA_2D = Cells % Rate % UREA, UNH4, UNO3, these are cell rate?  
!    UREA is a state variable.  UNH4 and UNO3 are uptake rates.

! if NHFlux, NVFlux are cell rate, then water HFlux, VFlux should be cell rate also?
!    All fluxes are rates.

! Line 751: should be CNUPTAKE = CNUPTAKE + WTNUP * 10.
!    WTNUP is cumulative N uptake in g[N]/m2

! Change all of DO J = 1, NColsTot DO L = 1, NRowsTot to Do Row first, do col second
!    Order does not matter

! line 756: TOTAML is output of Flood_Chem and OXLAYER. For 2D model, it is unknown 
! line 756: TOTAML is daily variable?
!    set TOTAML to zero for our 2D model

! DLTSNO3_2D(L) Rate of change of nitrate in soil layer (kg [N] / ha / d)
! But we changed to cell DLTSNO3_2D(L, J) Rate of change of nitrate in soil cell (kg [N] / ha / d)
! the unit should be kg/d?
!    I want to keep the units as kg/ha so that the masses from each cell are additive to get
!    a total mass for the field.

! Line 500 DLTSNO3_2D(L, J) = DLTSNO3_2D(L, J) + ARNTRF changed to per cell

! CNITRIFY & CNUPTAKE are seasonal? 

! Line 626: Change TNOX(J)  = TNOX(J)  + DENITRIF(L, J) * ColFrac(L) ! should be ColFrac(J)
! to          TNOX(L)  = TNOX(L)  + DENITRIF(L, J)* ColFrac(L) /1.E-8 ! should be ColFrac(J)

! SoilCellUtils_2D.for add real HFlux, VFlux for CellRateType
! then in WatBal2D.for line 197 add cells % Rate % HFlux = cells % Rate % HFlux + SWFh_ts
! then soil.for call SOILNI_2D(CONTROL, ISWITCH,cells % Rate % HFlux/cells%width, cells % Rate % VFlux/Cells%width, --- )
! to replace SOILNI(CONTROL, ISWITCH, DRN (cm/d)
! SWFh_ts in cm2[water]
! When SoilNI_2D call NFLUX_2D add 

!1D:  Soil.for call Watbal which out put DRN, then soil.for call Soilni which input DRN, then soilNI call NFLUX which input DRN
! 
! What do we want for Cell_Ndetail % NHFlux? how to calculate NFLUX?

! do we need to convert the SNO3_2D(L, J) to layer variable?

! if Cell_Type is pass and defined? width is not defined



!=========================================================================================
!TEMP CHP
      SUBROUTINE SUM_N(Cell_Type, DLTSNO3_2D, DLTSNH4_2D, !Input
     &      ColFrac, SNO3_2D, SNH4_2D,                    !Input
     &      TotNO3, TotNH4, TotDeltNO3, TotDeltNH4,       !Output
     &      TotN, TotDeltN, LastTotN, LastTotDeltN)       !Output

      use Cells_2d
      implicit none
      save 

      

      integer i, j
      real TotNO3, TotNH4, TotDeltNO3, TotDeltNH4
      real TotN, LastTotN, TotDeltN, LastTotDeltN
      real, dimension(MaxRows,MaxCols) :: ColFrac
      real, dimension(MaxRows,MaxCols) :: SNO3_2D, SNH4_2D
      real, dimension(MaxRows,MaxCols) :: DLTSNO3_2D, DLTSNH4_2D
      INTEGER, dimension(MaxRows,MaxCols) :: Cell_Type

      LastTotN = TotN
      LastTotDeltN = TotDeltN

      TotNO3 = 0.0
      TotNH4 = 0.0
      TotDeltNO3 = 0.0
      TotDeltNH4 = 0.0

      do i = 1, NRowsTot
        do j = 1, NColsTot
          select case (cell_type(i,j))
          case (3,4,5)
            TotNO3 = TotNO3 + SNO3_2D(i,j) * ColFrac(i, j)
            TotNH4 = TotNH4 + SNH4_2D(i,j) * ColFrac(i, j)
            TotDeltNO3 = TotDeltNO3 + DLTSNO3_2D(i,j) ! * ColFrac(i, j)
            TotDeltNH4 = TotDeltNH4 + DLTSNH4_2D(i,j) ! * ColFrac(i, j)
          end select
        enddo
      enddo

      TotN = TotNO3 + TotNH4
      TotDeltN = TotDeltNO3 + TotDeltNH4

      RETURN
      END SUBROUTINE SUM_N
!=========================================================================================
