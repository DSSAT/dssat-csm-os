!=======================================================================
C  COPYRIGHT 1998-2010 The University of Georgia, Griffin, Georgia
C                      University of Florida, Gainesville, Florida
C                      Iowa State University, Ames, Iowa
C                      International Center for Soil Fertility and 
C                       Agricultural Development, Muscle Shoals, Alabama
C                      University of Guelph, Guelph, Ontario
C  ALL RIGHTS RESERVED
C=======================================================================
C=======================================================================
C  SoilNi, Subroutine
C
C  Determines inorganic N transformations
C  This routine was modified from NTRANS when the module was split into
C  organic and inorganic sections.
!  2023 - Integration of 2D soil water and N routines now work with 2D 
!    arrays even in 1D mode. Some processes are still 1D so 2D arrays are
!    collapsed into 1D array as needed
C-----------------------------------------------------------------------
C  Revision history
C  . . . . . .  Written
C  08-08-1992 WTB Modified for version 3 input/output (matches LeGRO).
C  02-08-1993 PWW Header revision and minor changes. 
C  06/09/1999 AJG Completely revised the soil N and SOM module
C  06/21/1999 CHP Modular format.
C  03/29/2000 GH  Corrections for SARNC, CW, and DNRATE
C  01/01/2000 CHP/AJG Integrated the CENTURY-based and CERES-based SOM
C               modules with CHP's modular structure.
C  03/17/2000 GH  Incorporated in CROPGRO 
C  06/12/2002 UPS/CHP Added modifications for flooded conditions
C                   Added ammonia volatilization (OXLAYER) and replaced
C                     nitrification section based on Gilmour
C  11/07/2003 CHP Added tile drainage to NFLUX
C  11/18/2003 CHP Changed calculation of CW based on email from UPS
C  04/20/2004 US  Modified DLAG, removed IFDENIT
!  05/18/2004 AJG Renamed variables for P module:
!  01/14/2005 CHP/UPS Split NTRANS into separate organic and 
!                  inorganic routines.
!  02/25/2005 CHP changed HUMC to SSOMC to match Century variable name
!  04/13/2005 CHP changed subroutine name to SoilNi.for (was SoilN_inorg)
C  06-01-2010 JZW / CHP Modified SoilNi for 2D.
!  08/15/2011 Rename RowFrac to ColFrac
!             Add arguments NFlux_L, NFlux_R, NFlux_D, NFlux_U
!             set NFlux_L, NFlux_R, NFlux_D, NFlux_U for Cell_Ndetail 
!  03/15/2013 Add DripRow by JZW
!  06/12/2014 CHP DayCent calcs for N2O emissions from Peter Grace
!  11/21/2017 HJ  modified this subroutine to include N loss to tile
!  01/26/2023 CHP Reduce compile warnings: add EXTERNAL stmts, remove 
!                 unused variables, shorten lines. 
C=======================================================================

      SUBROUTINE SoilNi (CONTROL, ISWITCH, 
     &    CH4_data, DRN, ES, FERTDATA, FLOODWAT, IMM,     !Input
     &    LITC, MNR, newCO2, SNOW, SOILPROP, SSOMC, ST,   !Input
     &    SW, TDFC, TDLNO, TILLVALS, UNH4, UNO3, UPFLOW,  !Input
     &    WEATHER, XHLAI,                                 !Input
     &    CELLS, FLOODN,                                  !I/O
     &    NH4, NO3, NH4_plant, NO3_plant, UPPM)           !Output

!-----------------------------------------------------------------------
      USE Cells_2D
      USE GHG_mod
      USE FertType_mod
      USE ModuleData
      USE FloodModule
      USE ModSoilMix
      IMPLICIT  NONE
      EXTERNAL DENIT_CERES, INCDAT, YR_DOY, OPSOILNI, 
     &  SOILNI_INIT, NCHECK_INORG, FLOOD_CHEM, OXLAYER, DENIT_DAYCENT, 
     &  NOX_PULSE, INCYD, DAYCENT_DIFFUSIVITY, NFLUX
      EXTERNAL NFLUX_2D !, CellNDetail_2D

      SAVE
!-----------------------------------------------------------------------
      CHARACTER*1 ISWNIT, MEGHG, MEHYD

      LOGICAL IUON, Sim2D

!     2D variables
      Type (CellType) Cells(MaxRows,MaxCols)
      INTEGER FurRow1, FurCol1
      INTEGER, DIMENSION(MaxRows,MaxCols) :: Cell_Type, DLAG_2D
      REAL HalfRow, BEDWD, FertFactor
      REAL, DIMENSION(MaxRows,MaxCols) :: DLTSNH4_2D, DLTSNO3_2D
      REAL, DIMENSION(MaxRows,MaxCols) :: DLTUREA_2D
      REAL, DIMENSION(MaxRows,MaxCols) :: NH4_2D, NO3_2D,SNH4_2D,SNO3_2D
      REAL, DIMENSION(MaxRows,MaxCols) :: UREA_2D, UPPM_2D
      REAL, DIMENSION(MaxRows,MaxCols) :: SWV, TFNITY_2D
      REAL, DIMENSION(MaxRows,MaxCols) :: UNH4_2D, UNO3_2D
      REAL, DIMENSION(MaxRows,MaxCols) :: ColFrac, BedFrac
      REAL, DIMENSION(MaxRows,MaxCols) :: NFlux_L,NFlux_R,NFlux_D
      REAL, DIMENSION(MaxRows,MaxCols) :: NFlux_U
      REAL, DIMENSION(MaxRows,MaxCols) :: NFlux_UREA_L, NFlux_UREA_R
      REAL, DIMENSION(MaxRows,MaxCols) :: NFlux_UREA_D, NFlux_UREA_U
!     REAL, DIMENSION(MaxRows,MaxCols) :: MINERN_2D, IMMOBN_2D
      REAL, DIMENSION(MaxRows,MaxCols) :: NITRIFppm, NITRIF_2D
      REAL, DIMENSION(MaxRows,MaxCols) :: N2Onitrif_2D, nNOflux_2D

      INTEGER DOY, DYNAMIC, INCDAT, IUYRDOY, IUOF, I, J, L
      INTEGER NLAYR
      INTEGER NSOURCE, YEAR, YRDOY

      REAL AD, AK, ALGFIX 
      REAL NFAC, NNOM
      REAL SNH4_AVAIL, SNO3_AVAIL, SUMFERT
      REAL SWEF, TFUREA
      REAL TNH4, TNH4NO3, TNO3, UHYDR
      REAL WFSOM, WFUREA, XL, XMIN
      REAL TUREA, SurfaceVal

      REAL ADCOEF(NL), BD(NL), DLAYR(NL)
      REAL DLTSNH4(NL), DLTSNO3(NL), DLTUREA(NL), DRN(NL), DUL(NL)
      REAL UPFLOW(NL)
      REAL KG2PPM(NL), LITC(0:NL), LL(NL) 
      REAL NH4(NL), NO3(NL), PH(NL), SAT(NL), SNH4(NL)
      REAL SNO3(NL), SSOMC(0:NL), ST(NL), SW(NL), WCR(NL)
      REAL TFNITY(NL), UNH4(NL), UNO3(NL), UREA(NL), UPPM(NL)
      REAL NH4_plant(NL), NO3_plant(NL)

      REAL IMM(0:NL,NELEM), MNR(0:NL,NELEM)

!     Variables added for flooded conditions analysis:
      TYPE (FloodWatType) FLOODWAT    
      TYPE (FloodNType)   FloodN        
      TYPE (OxLayerType)  OXLAYR   

      INTEGER NBUND, NSWITCH
      INTEGER FERTDAY
      INTEGER INCYD, LFD10  !DLAG(NL), 
      REAL ALI, FLOOD, ES, TMAX, TMIN, SRAD, XHLAI, RAIN, SNOW
      REAL TKELVIN, TFACTOR, WFPL, WF2
      REAL PHFACT, T2, TLAG   !, DNFRATE
      REAL CUMFNRO
      REAL BD1
      REAL TOTAML, TOTFLOODN, TotUptake

      TYPE (N2O_type)    N2O_DATA
!          Cumul,     Daily,    Layer ppm,     Layer kg
      REAL CNOX,      TNOXD,                   DENITRIF(NL) !Denitrif
      REAL CNITRIFY,  TNITRIFY,                NITRIF(NL)   !Nitrif 
      REAL CMINERN,   TMINERN                               !Mineraliz
      REAL CIMMOBN,   TIMMOBN                               !Immobiliz
      REAL CNETMINRN                                        !Net miner
      REAL CNUPTAKE,  WTNUP                                 !N uptake
      REAL CLeach,    TLeachD                               !N leaching
      REAL TLeachD_UREA
      REAL CNTILEDR,  NTILEDR                               !N tile loss
      REAL CN2Onitrif,TN2OnitrifD,             N2Onitrif(NL)!N2O nitrif
      REAL CN2Odenit, TN2OdenitD,              N2ODenit(NL) !N2O denitr
      REAL CNOflux,   TNOfluxD,                NOflux(NL)   !NO flux
      REAL                                     nNOflux(NL)  !NO nitrif
      REAL                                     dNOflux(NL)  !NO denitri
      REAL CN2,       TN2D,                    N2flux(NL)   !N2 detnitr

!     Added for GHG model
      TYPE (CH4_type) CH4_data
      REAL, DIMENSION(NL) :: dD0, NO_N2O_ratio
      REAL, DIMENSION(0:NL) :: newCO2
      REAL pn2onitrif, NH4_to_NO, NITRIF_to_NO
      real nox_puls, krainNO, potential_NOflux, NITRIF_remaining
!     real canopy_reduction, NOAbsorp

!     Added for tile drainage:
      REAL TDFC
      INTEGER TDLNO

!     Added for tillage
      INTEGER TILDATE
      REAL MIXPCT, TDEP

!     2D integration
      REAL RESID3, RESID4, RESID5, SNO3_TEMP, SNH4_TEMP, UREA_TEMP
      REAL, DIMENSION(NL) :: DLTSNO3_SAVE, DLTSNH4_SAVE, DLTUREA_SAVE
      REAL, DIMENSION(NL) :: DLTSNO3_DIFF, DLTSNH4_DIFF, DLTUREA_DIFF
      REAL, DIMENSION(MaxRows,MaxCols) ::DLTSNO3_DIFF_2D, 
     &                  DLTSNH4_DIFF_2D, DLTUREA_DIFF_2D
!     *** TEMP DEBUGGIN CHP
      REAL TNOM
      REAL NNOM_a, NNOM_b

      REAL CumSumFert

!-----------------------------------------------------------------------
!     Constructed variables are defined in ModuleDefs.
      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH
      TYPE (SoilType)    SOILPROP
      TYPE (FertType)    FERTDATA
      TYPE (TillType)    TILLVALS
      TYPE (WeatherType) WEATHER

!     Interface required because N2O_data is optional variable
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

**************************************************************************
!     temp chp
!     I want to know everything that happens on this date at the cell level.
      INTEGER ReportDate, J1,L1, P1
      ReportDate = 1995044
      L1 = NRowsTot
      J1 = NColsTot
**************************************************************************

!     Transfer values from constructed data types into local variables.
      DYNAMIC = CONTROL % DYNAMIC
      YRDOY   = CONTROL % YRDOY

      SWV = Cells % State % SWV
      UNH4_2D = Cells % Rate % NH4Uptake
      UNO3_2D = Cells % Rate % NO3Uptake
      SNH4_2D = Cells % State % SNH4
      SNO3_2D = Cells % State % SNO3

!     BED & Cell info
      Cell_Type = CELLS % Struc % Cell_Type
      BEDWD   = BedDimension % BEDWD
      HalfRow = BedDimension % ROWSPC_cm / 2.
      FurRow1 = BedDimension % FurRow1
      FurCol1 = BedDimension % FurCol1
      ColFrac = BedDimension % ColFrac
      BedFrac = BedDimension % BedFrac

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
      MEHYD   = ISWITCH % MEHYD
      MEGHG   = ISWITCH % MEGHG

      NBUND = FLOODWAT % NBUND
      FLOOD = FLOODWAT % FLOOD

      FERTDAY = FERTDATA % FERTDAY

      SRAD = WEATHER % SRAD
      TMAX = WEATHER % TMAX
      TMIN = WEATHER % TMIN
      RAIN = WEATHER % RAIN

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!     ------------------------------------------------------------------
        IF (INDEX('GC',ISWITCH % MEHYD) > 0) THEN
          Sim2D = .TRUE.
        ELSE
          Sim2D = .FALSE.
        ENDIF

!       Today's values
        TMINERN  = 0.0  !mineralization
        TIMMOBN  = 0.0  !immobilization
        TNITRIFY = 0.0  !nitrification
        TNOXD    = 0.0  !denitrification
        TLeachD  = 0.0  !leaching

        !*** temp debugging chp
        TNOM = 0.0

!       Seasonal cumulative values, kg[N]/ha
        CMINERN  = 0.0  !mineralization
        CIMMOBN  = 0.0  !immobilization
        CNETMINRN= 0.0  !net mineralization
        CNITRIFY = 0.0  !nitrification
        CNUPTAKE = 0.0  !cumulative N uptake
        CNOX     = 0.0  !denitrification
        CLeach   = 0.0  !leaching
        CNTILEDR = 0.0  !N loss to tile drainage     !HJ added
        WTNUP    = 0.0  !N uptake

        TOTAML = 0.0    !Ammonia volatilization

        CN2Onitrif=0.0  !N2O[N] from nitrification
        CN2Odenit =0.0  !N2O[N] from nitrification
        CNOflux   = 0.0 !NO
        CN2       = 0.0 !N2
        CumSumFert= 0.0 !Total fertilizer

        nitrif = 0.0
        denitrif = 0.0
        N2O_data % wfps = 0.0

!       proportion of N2O from nitrification PG calibrated this variable for DayCent
        pn2Onitrif = .001  
!       pn2Onitrif = .02  
!       chp - from DayCent - is this the equivalent value?
!       double turnovfrac = 0.02;
! chp - tried pn2Onitrif = .02, but n2o emissions are way too high.

        TFNITY = 0.0
        IUOF   = 0
        IUON   = .FALSE.

        DLTSNO3 = 0.0
        DLTSNH4 = 0.0
        DLTUREA = 0.0
        DLTSNO3_2D  = 0.0
        DLTSNH4_2D  = 0.0
        DLTUREA_2D  = 0.0
        DLAG_2D   = 0   !REVISED-US

!       Initialize uptake variables here, or they will have residual
!         value on first day of multi-season runs.
        UNH4_2D   = 0.0
        UNO3_2D   = 0.0

        DO L = 1, NLAYR
          N2O_data % wfps(L) = min (1.0, sw(L) / soilprop % poros(L))
        ENDDO

!       IF (INDEX('N',ISWNIT) > 0) RETURN

!       Set initial SOM and nitrogen conditions for each 1D soil layer and 2D soil cell.
        CALL SoilNi_init(CONTROL, 
     &      Cell_Type, SOILPROP, ST, NH4, NO3,      !Input
     &      NH4_2D, NO3_2D, SNH4, SNH4_2D, SNO3,    !Output
     &      SNO3_2D, TFNITY_2D, UPPM, UREA, UREA_2D)   !Output

!       2D values are checked every day
        CALL NCHECK_inorg(CONTROL, Sim2D,
     &    NH4_2D, NO3_2D, SNH4_2D, SNO3_2D, UREA_2D) !Input

        IF (SIM2D) THEN
!         Initialize 2D N flux routines (1D is rate only)
          CALL NFLUX_2D (DYNAMIC,
     &      ADCOEF, BD, CELLS, ColFrac, DUL, SNO3_2D,   !Input
     &      1, SWV,                                     !Input
     &      DLTSNO3_2D, CLeach, TLeachD,                !Output
     &      NFlux_L, NFlux_R, NFlux_D, NFlux_U)         !Output
        ENDIF

        SWEF = 0.9-0.00038*(DLAYR(1)-30.)**2

!       Initialize flooded N if flooding is a possibility.
        CALL FLOOD_CHEM(CONTROL, ISWITCH, 
     &    FLOODWat, LFD10, SOILPROP, SNO3, SNH4, UREA,    !Input
     &    SRAD, SW, TMAX, TMIN, XHLAI,                    !Input
     &    DLTSNH4, DLTSNO3, DLTUREA, FLOODN, OXLAYR,      !I/O
     &    ALGFIX, BD1, CUMFNRO, TOTAML, TOTFLOODN)        !Output

!       Initialize TOTAML
        CALL OXLAYER(CONTROL,
     &    BD1, ES, FERTDATA, FLOODWAT, LFD10,             !Input
     &    NSWITCH, SNH4, SNO3, SOILPROP, SRAD, ST,        !Input
     &    SW, TMAX, TMIN, UREA, XHLAI,                    !Input
     &    DLTSNH4, DLTSNO3, DLTUREA, OXLAYR,              !I/O
     &    ALI, TOTAML)                                    !Output

        LFD10 = CONTROL % YRSIM

!       Initialize denitrification routines (1D processes for now)
        SELECT CASE(MEGHG)
        CASE("1")
          CALL Denit_DayCent (CONTROL, ISWNIT, 
     &    dD0, newCO2, NO3, SNO3, SOILPROP,       !Input
     &    DLTSNO3,                                !I/O
     &    CNOX, TNOXD, N2O_data)                  !Output

        CASE DEFAULT
          CALL Denit_Ceres (CONTROL, ISWNIT, 
     &    DUL, KG2PPM, LITC, NLAYR, NO3, SAT,         !Input
     &    SSOMC, SNO3, ST, SW,                        !Input
     &    DLTSNO3,                                    !I/O
     &    CNOX, TNOXD, N2O_data)                      !Output
        END SELECT
!     GHG emissions, 1D processes for now
      CALL N2Oemit(CONTROL, ISWITCH, dD0, SOILPROP, N2O_DATA) 
      CALL OpN2O(CONTROL, ISWITCH, SOILPROP, N2O_DATA) 
      CALL OPGHG(CONTROL, ISWITCH, N2O_data, CH4_data)

      IF (CONTROL%RUN .EQ. 1 .OR. INDEX('QF',CONTROL%RNMODE) .LE. 0)THEN
        call nox_pulse (dynamic, rain, snow, nox_puls)
      ENDIF

!***********************************************************************
!***********************************************************************
!     DAILY RATE CALCULATIONS 
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!     ------------------------------------------------------------------
      IF (INDEX('N',ISWNIT) > 0) RETURN

**************************************************************************
!     TEMP CHP
      IF (YRDOY == ReportDate) THEN
        P1 = 10 !Start of rate section
        WRITE(555,555) ((P1,"SNO3",L,J,SNO3_2D(L,J),J=1,J1),L=1,L1)
        WRITE(555,555) ((P1,"SNH4",L,J,SNH4_2D(L,J),J=1,J1),L=1,L1)
  555   FORMAT(500(I3,1X,A10,2I3,F10.6,/))  !Format for the entire array
  556   FORMAT(I3,1X,A10,2I3,F10.6) !Format for one element of the array
      ENDIF
**************************************************************************

!     Initialize Soil N process rates for this time step.
      DLTUREA_2D = 0.0
      DLTSNO3_2D = 0.0
      DLTSNH4_2D = 0.0
      DLTUREA = 0.0
      DLTSNO3 = 0.0
      DLTSNH4 = 0.0
      TNH4 = 0.0
      TNO3 = 0.0
      TUREA = 0.0
      TNOXD = 0.0
      TotUptake = 0.0

      DO L = 1, NRowsTot
        DO J = 1, NColsTot 
!         Update with yesterday's plant N uptake
          SNO3_2D(L, J) = SNO3_2D(L, J) - UNO3_2D(L, J)
          SNH4_2D(L, J) = SNH4_2D(L, J) - UNH4_2D(L, J)
!         KG2PPM(L) Conversion factor to switch from kg [N] / ha to ug [N] / g
          SELECT CASE(Cell_type(L,J))
!          CASE (3)
!            NO3_2D(L, J)  = SNO3_2D(L, J) * KG2PPM(L) / BedFrac(L,J)
!            NH4_2D(L, J)  = SNH4_2D(L, J) * KG2PPM(L) / BedFrac(L,J)
!          CASE (4,5)
          CASE (3,4,5)
            NO3_2D(L, J)  = SNO3_2D(L, J) * KG2PPM(L) / ColFrac(L,J)
            NH4_2D(L, J)  = SNH4_2D(L, J) * KG2PPM(L) / ColFrac(L,J)
          END SELECT

          TotUptake = TotUptake + (UNO3_2D(L,J) +UNH4_2D(L,J)) !kg/ha
        ENDDO
      ENDDO

!     Convert 2D arrays to 1D after N uptake 
      CALL Cell2Layer_2D(SNO3_2D, CELLS % Struc, NLAYR, SNO3)
      CALL Cell2Layer_2D(SNH4_2D, CELLS % Struc, NLAYR, SNH4)

!     Must calculate WTNUP here or it won't be guaranteed to match N
!     removed from the soil today and the balance will be off.
!     WTNUP is cumulative
      CALL Cell2Layer_2D(UNO3_2D,    CELLS % Struc, NLAYR, UNO3)
      CALL Cell2Layer_2D(UNH4_2D,    CELLS % Struc, NLAYR, UNH4)
      DO L = 1, NLAYR
        WTNUP = WTNUP + (UNO3(L) + UNH4(L)) / 10.    !g[N]/m2 cumul.
      ENDDO

!-------------------------------------------------------------------------
!     Check for fertilizer added today or active slow release fertilizers 
      IF (FERTDAY == YRDOY .OR. NActiveSR .GT. 0) THEN
        SUMFERT = 0.0
        DO L = 1, NRowsTot 
          DLTSNO3(L) = DLTSNO3(L) + FERTDATA % ADDSNO3(L)
          DLTSNH4(L) = DLTSNH4(L) + FERTDATA % ADDSNH4(L)
          DLTUREA(L) = DLTUREA(L) + FERTDATA % ADDUREA(L)
          IF (FERTDATA % ADDUREA(L) > 1.E-4) THEN
            IUON = .TRUE.
            IUYRDOY = INCDAT(YRDOY, 21)
            CALL YR_DOY (IUYRDOY, YEAR, IUOF)
          ENDIF
          SUMFERT = SUMFERT + FERTDATA % ADDSNO3(L) + 
     &                   FERTDATA % ADDSNH4(L) + FERTDATA % ADDUREA(L)

          SELECT CASE (TRIM(FERTDATA % AppType))
          CASE ('BANDED','POINT')
!           Banded or point application goes to  column 1, for each layer
            DLTSNO3_2D(L,1) = DLTSNO3_2D(L,1) + FERTDATA % ADDSNO3(L)
            DLTSNH4_2D(L,1) = DLTSNH4_2D(L,1) + FERTDATA % ADDSNH4(L)
            DLTUREA_2D(L,1) = DLTUREA_2D(L,1) + FERTDATA % ADDUREA(L)

          CASE ('DRIP')
!           Drip fertigation goes to cell DripRow,DripCol
            J = BedDimension % DripCol(FERTDATA%DrpRefIdx)
            I = BedDimension % DripRow(FERTDATA%DrpRefIdx)
            If (I .EQ. L) THEN
                DLTSNO3_2D(I,J) = DLTSNO3_2D(I,J) + FERTDATA %ADDSNO3(L)
                DLTSNH4_2D(I,J) = DLTSNH4_2D(I,J) + FERTDATA %ADDSNH4(L)
                DLTUREA_2D(I,J) = DLTUREA_2D(I,J) + FERTDATA %ADDUREA(L)
            END IF

          CASE DEFAULT
            DO J = 1, NColsTot
! CHP 2023-10-16 - Check how this works for flat system. Where does the fertilizer go?
              SELECT CASE (Cell_Type(L,J))
!             Within the bed, fertilizer is concentrated in bed cells
              CASE (3); 
                FertFactor = BedDimension%ROWSPC_cm / BEDWD
              CASE (4,5); FertFactor = 1.0
              CASE DEFAULT; CYCLE
              END SELECT

              DLTSNO3_2D(L, J) = DLTSNO3_2D(L, J) + 
     &          FERTDATA % ADDSNO3(L) * FertFactor * ColFrac(L,J)
              DLTSNH4_2D(L, J) = DLTSNH4_2D(L, J) + 
     &          FERTDATA % ADDSNH4(L) * FertFactor * ColFrac(L,J)
              DLTUREA_2D(L, J) = DLTUREA_2D(L, J) + 
     &          FERTDATA % ADDUREA(L) * FertFactor * ColFrac(L,J)
            ENDDO
          END SELECT

        ENDDO
        CumSumFert = CumSumFert + SUMFERT

        IF (.NOT. SIM2D) THEN
!         Fertilizer added directly to flooded field
          IF (FLOOD > 1.E-4) THEN
            FLOODN % DLTFNH4  = FLOODN % DLTFNH4  + FERTDATA % ADDFNH4
            FLOODN % DLTFNO3  = FLOODN % DLTFNO3  + FERTDATA % ADDFNO3
            FLOODN % DLTFUREA = FLOODN % DLTFUREA + FERTDATA % ADDFUREA
            IF (FERTDATA % ADDFUREA > 1.E-4) THEN 
              IUON = .TRUE.
              IUYRDOY = INCDAT(YRDOY, 21)
              CALL YR_DOY (IUYRDOY, YEAR, IUOF)
            ENDIF
          ENDIF
          
          OXLAYR % DailyCalc = .FALSE.
          LFD10 = INCYD(YRDOY, 10)  !Use YRDOY format, increment 10 days
        ENDIF
      ENDIF

!     ------------------------------------------------------------------
!     1D only simulations
!     Tillage and flood calcs are done only for 1D simulations
!     ------------------------------------------------------------------
      IF (.NOT. SIM2D) THEN
!       Check if tillage occurred today -- if so, mix soil N within
!         tillage depth
        IF (TILLVALS % NTIL .GT. 0) THEN
          TILDATE = TILLVALS % TILDATE
          IF (YRDOY .EQ. TILDATE) THEN
!           Call mixing routine for soil properties
            MIXPCT = TILLVALS % TILMIX
            TDEP = TILLVALS % TILDEP
            CALL SoilMix (SNO3, DLTSNO3, 1, DLAYR, MIXPCT, NLAYR, TDEP)
            CALL SoilMix (SNH4, DLTSNH4, 1, DLAYR, MIXPCT, NLAYR, TDEP)
            CALL SoilMix (UREA, DLTUREA, 1, DLAYR, MIXPCT, NLAYR, TDEP)

          ENDIF
        ENDIF

!       Calculate rates of change of flood N components and interaction
!           with top soil layer.
        IF (NBUND > 0) THEN
          CALL FLOOD_CHEM(CONTROL, ISWITCH, 
     &      FLOODWat, LFD10, SOILPROP, SNO3, SNH4, UREA,    !Input
     &      SRAD, SW, TMAX, TMIN, XHLAI,                    !Input
     &      DLTSNH4, DLTSNO3, DLTUREA, FLOODN, OXLAYR,      !I/O
     &      ALGFIX, BD1, CUMFNRO, TOTAML, TOTFLOODN)        !Output
        ENDIF
        
!       This needs to be called from NTRANS for upland cases, too.
        IF (FLOOD .LE. 0.0) THEN
          CALL OXLAYER(CONTROL,
     &      BD1, ES, FERTDATA, FLOODWAT, LFD10,             !Input
     &      NSWITCH, SNH4, SNO3, SOILPROP, SRAD, ST,        !Input
     &      SW, TMAX, TMIN, UREA, XHLAI,                    !Input
     &      DLTSNH4, DLTSNO3, DLTUREA, OXLAYR,              !I/O
     &      ALI, TOTAML)                                    !Output
        ENDIF

!       Convert state and DELTA variables to 2D for next set of processes
        CALL Layer2Cell_2D(                              
     &   CELLS % Struc, NLAYR, DLAYR, SNO3, 0.0,            !Input
     &   SNO3_2D)                                           !Output
        
        CALL Layer2Cell_2D(                              
     &   CELLS % Struc, NLAYR, DLAYR, SNH4, 0.0,            !Input
     &   SNH4_2D)                                           !Output
        
        CALL Layer2Cell_2D(                              
     &   CELLS % Struc, NLAYR, DLAYR, UREA, 0.0,            !Input
     &   UREA_2D)                                           !Output

        CALL Layer2Cell_2D(                              
     &   CELLS % Struc, NLAYR, DLAYR, DLTSNO3, 0.0,         !Input
     &   DLTSNO3_2D)                                        !Output
        
        CALL Layer2Cell_2D(                              
     &   CELLS % Struc, NLAYR, DLAYR, DLTSNH4, 0.0,         !Input
     &   DLTSNH4_2D)                                        !Output
        
        CALL Layer2Cell_2D(                              
     &   CELLS % Struc, NLAYR, DLAYR, DLTUREA, 0.0,         !Input
     &   DLTUREA_2D)                                        !Output

      ENDIF !End of 1D-only simulations
!     ------------------------------------------------------------------

!     Back to 2D calculations
!     ----------------------------------------------------------------
!     If DOY=IUOF (has been set in Fert_Place), then all the urea has
!     hydrolyzed already.
      CALL YR_DOY (CONTROL%YRDOY, YEAR, DOY)
      IF (DOY .EQ. IUOF) THEN
        DO L = 1, NRowsTot
          DO J = 1, NColsTot
            DLTSNH4_2D(L,J) = DLTSNH4_2D(L,J) + UREA_2D(L,J)
            DLTUREA_2D(L,J) = DLTUREA_2D(L,J) - UREA_2D(L,J)
          END DO
        END DO
        IF (FLOOD > 1.E-4) THEN
          FLOODN % DLTFNH4  = FLOODN % DLTFNH4  + FLOODN % FLDU
          FLOODN % DLTFUREA = FLOODN % DLTFUREA - FLOODN % FLDU
        ENDIF
        IUON = .FALSE.
      ENDIF

!-----------------------------------------------------------------------
!     NOx pulse multiplier from DayCent
      call nox_pulse (dynamic, rain, snow, nox_puls)
      krainNO = nox_puls

!     For now, keep denitrification at 1D
!     Diffusivity rate calculation from DayCent
      call DayCent_diffusivity(dD0, sw, soilprop)
!-----------------------------------------------------------------------

!     Loop thru soil layers (rows) and columns. 1D simulations have only
!     one column.
!     ------------------------------------------------------------------
      NNOM     = 0.0
      TMINERN  = 0.0
      TIMMOBN  = 0.0
      TNITRIFY = 0.0
      TNOXD    = 0.0  !denitrification
      TLeachD  = 0.0  !leaching
      NTILEDR  = 0.0  !N loss to tile !HJ added
      NITRIF   = 0.0
      NITRIF_2D   = 0.0
      TN2OnitrifD = 0.0  !N2O from nitrification
      TN2OdenitD  = 0.0  !N2O from denitrification
      N2Onitrif = 0.0
      TNOfluxD  = 0.0   !NO
      NOflux    = 0.0
      nNOflux   = 0.0
      dNOflux   = 0.0
      TN2D      = 0.0
      N2flux    = 0.0

      DO J = 1, NColsTot
        DO L = 1, NRowsTot

!         Only process cells that have soil (i.e., not in the furrow)
          SELECT CASE(Cell_type(L,J))
          CASE(3,4,5); CONTINUE
          CASE DEFAULT; CYCLE
          END SELECT

!       ----------------------------------------------------------------
!       Environmental limitation factors for the soil processes.
!       ----------------------------------------------------------------
        IF (SWV(L,J) .LE. DUL(L)) THEN
          IF (L .EQ. 1) THEN
!           For the topsoil layer, the lowest possible volumetric water
!           content (air-dry water content) may be lower than the lower
!           limit, because this layer may dry out due to water
!           evaporation at the soil surface.
            AD = LL(L) * SWEF
          ELSE
!           Set the air dry water content equal to the lower limit.
            AD  = LL(L)
          ENDIF

!         Soil water factor WFSOM.
          WFSOM = (SWV(L,J) - AD) / (DUL(L) - AD)

        ELSE
!         If the soil water content is higher than the drained upper 
!         limit (field capacity), calculate the excess water as fraction
!         of the maximum excess (i.e. when saturated).
          XL = (SWV(L,J) - DUL(L)) / (SAT(L) - DUL(L))

!         Soil water factor WFSOM.
          WFSOM = 1.0 - 0.5 * XL

        ENDIF   !End of IF block on SW vs. DUL.

!       PH factor (from RICE model)
        IF (FLOOD .GT. 0.0) THEN
           WFSOM    = 0.75
        ENDIF

!       Limit the soil water factors between 0 and 1.
        WFSOM = AMAX1 (AMIN1 (WFSOM, 1.), 0.)

!       Calculate the soil temperature factor for the urea hydrolysis.
        TFUREA = (ST(L) / 40.) + 0.20
        TFUREA = AMAX1 (AMIN1 (TFUREA, 1.), 0.)

!       Water filled pore space
        N2O_data % wfps(L) = min (1.0, swV(L,J) / soilprop % poros(L))

!-----------------------------------------------------------------------
!       UREA hydrolysis
!-----------------------------------------------------------------------
        IF (IUON) THEN
!         Calculate the maximum hydrolysis rate of urea.
          AK = -1.12 + 1.31 * SSOMC(L) * 1.E-4 * KG2PPM(L) + 0.203*PH(L)
     &              - 0.155 * SSOMC(L) * 1.E-4 * KG2PPM(L) * PH(L)
          AK = AMAX1 (AK, 0.25)

!         If urease inhibitor is active, reduce hydrolysis rate
          IF ((YRDOY .LT. UIData % UIEND) .AND. 
     &        (L .LE. UIData % UILYR)) THEN
            AK = AK * (1.0 - UIData % UIEFF/100.)
          ENDIF

!         Calculate the soil water factor for the urea hydrolysis, and
!         limit it between 0 and 1.
          IF (FLOOD .GT. 0.0) THEN
             WFUREA = 1.0
          ELSE
            WFUREA = WFSOM + 0.20
            WFUREA = AMAX1 (AMIN1 (WFUREA, 1.), 0.)
          ENDIF

!         Calculate the amount of urea that hydrolyses.
          UHYDR = AK * AMIN1 (WFUREA, TFUREA) 
     &          * (UREA_2D(L,J) + DLTUREA_2D(L,J)) * ColFrac(L,J)
          UHYDR = AMIN1 (UHYDR, UREA_2D(L,J) + DLTUREA_2D(L,J))

          DLTUREA_2D(L,J) = DLTUREA_2D(L,J) - UHYDR 
          DLTSNH4_2D(L,J) = DLTSNH4_2D(L,J) + UHYDR 
        ENDIF   !End of IF block on IUON.

!-----------------------------------------------------------------------
!       Net mineralization rate - Nitrogen.
!-----------------------------------------------------------------------
!       Mineralization and immobilization processes were computed in 1D
!       Distribute the values evenly across a row.
        IF (L == 1) THEN
!         First layer takes mineralization from surface also.
!         NNOM = MINERALIZE(0,N) + MINERALIZE(1,N)
          NNOM = (MNR(0,N) + MNR(1,N) - IMM(0,N) - IMM(1,N)) 
        ELSE
!         Add in residual from previous layer to preserve N balance
          NNOM = NNOM + (MNR(L,N) - IMM(L,N)) 
        ENDIF

!       Proportion total NNOM to columns
        SELECT CASE(Cell_type(L,J))
        CASE (3)  ; NNOM = NNOM * BedFrac(L,J)
        CASE (4,5); NNOM = NNOM * ColFrac(L,J)
        CASE DEFAULT; CYCLE
        END SELECT

        !*** temp debugging chp
        TNOM = TNOM + NNOM

!       Mineralization
!       --------------
!       If the net N release from all SOM sources (NNOM) is positive,
!       add the mineralized N to the NH4 pool.
        IF (NNOM .GE. 0.0) THEN
          DLTSNH4_2D(L,J) = DLTSNH4_2D(L,J) + NNOM

**************************************************************************
!     TEMP CHP
      IF (YRDOY == ReportDate) THEN
        P1 = 15 !Mineralization
        WRITE(555,556) P1,"NNOM",L,J,NNOM
      ENDIF
**************************************************************************

          NNOM = 0.

        ELSE
!       Immobilization
!       --------------
!         If NNOM is < 0, there is immobilization. If the N demand
!         of the immobilization is greater than the amount of NH4
!         available, take all NH4, leaving behind a minimum amount of
!         NH4 equal to XMIN (NNOM is negative!).

          IF (ABS(NNOM) .GT. (SNH4_2D(L,J) + DLTSNH4_2D(L,J)-XMIN)) THEN
            NNOM_a = -(SNH4_2D(L,J) - XMIN + DLTSNH4_2D(L,J))
            NNOM_b = NNOM - NNOM_a

            DLTSNH4_2D(L,J) = -(SNH4_2D(L,J) - XMIN)
            NNOM = NNOM_b

            SNO3_AVAIL = SNO3_2D(L,J) + DLTSNO3_2D(L,J)
            IF (ABS(NNOM) .GT. (SNO3_AVAIL + DLTSNO3_2D(L,J)-XMIN)) THEN
              !Not enough SNO3 to fill remaining NNOM, leave residual
              DLTSNO3_2D(L,J) = DLTSNO3_2D(L,J) + XMIN - SNO3_AVAIL
              NNOM = NNOM + SNO3_AVAIL - XMIN
            ELSE
!             Get the remainder of the immobilization from nitrate (NNOM
!             is negative!)
              DLTSNO3_2D(L,J) = DLTSNO3_2D(L,J) + NNOM
              NNOM = 0.
            ENDIF

          ELSE
!           Reduce soil NH4 by the immobilization (NNOM is
!           negative!).
            DLTSNH4_2D(L,J) = DLTSNH4_2D(L,J) + NNOM
            NNOM = 0.0
          ENDIF   !End of IF block on ABS(NNOM).
        ENDIF   !End of IF block on NNOM.


**************************************************************************
!     TEMP CHP
      IF (YRDOY == ReportDate) THEN
        write(560,'(a,f10.6)') "tnom",tnom
        WRITE(555,556) P1,"DLTSNO3",L,J,DLTSNO3_2D(L,J)
        WRITE(555,556) P1,"DLTSNH4",L,J,DLTSNH4_2D(L,J)
      ENDIF
**************************************************************************

!-----------------------------------------------------------------------
!       Nitrification section
!-----------------------------------------------------------------------
!       Nitrification based on Gilmour
        TKELVIN = ST(L) + 273.0
        TFACTOR = EXP(-6572 / TKELVIN + 21.4)  !6602
        WFPL = SWV(L,J) / SAT(L)
        IF (SWV(L,J) .GT. DUL(L)) THEN
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

        IF (FLOOD .GT. 0.0) THEN
          PHFACT = 1.0
          WF2    = 0.0     
          TFNITY_2D(L,J) = 0.0
        ENDIF

        T2   = AMAX1 (0.0,(TFNITY_2D(L,J) - 1.0))
        TLAG = 0.075 * T2**2
        TLAG = AMIN1 (TLAG,1.0)
        IF (NBUND <= 0.) THEN
          TLAG = 1.0
        ENDIF
        NFAC = AMAX1(0.0, AMIN1(1.0, TFACTOR * WF2 * PHFACT * TLAG))

!       If nitrification inhibitor is active, reduce rate
        IF ((YRDOY .LT. NIData % NIEND) .AND. 
     &      (L .LE. NIData % NILYR)) THEN
          NFAC = NFAC * (1.0 - NIData % NIEFF/100.)
        ENDIF

        NITRIFppm(L,J) = NFAC * NH4_2D(L,J)

        IF (NSWITCH .EQ. 5) THEN
          NITRIF_2D(L,J) = 0.0
        ELSE
!         NITRIFppm in ppm; NITRIF in kg/ha  changed by PG from * kg2ppm
          SELECT CASE (Cell_Type(L,J))
!          CASE(3)
!            NITRIF_2D(L,J)  = NITRIFppm(L,J) / KG2PPM(L) * BedFrac(L,J)
!          CASE(4,5)
          CASE(3,4,5)
            NITRIF_2D(L,J)  = NITRIFppm(L,J) / KG2PPM(L) * ColFrac(L,J)
          END SELECT
        ENDIF

        IF (NH4_2D(L,J).LE. 0.01) THEN
          TFNITY_2D(L,J) = 0.0
        ELSE
          IF (SWV(L,J) .LT. SAT(L)) THEN
            TFNITY_2D(L,J) = TFNITY_2D(L,J) + 1.0
          ENDIF
        ENDIF
           
        XMIN = 0.0
        SNH4_AVAIL = AMAX1(0.0, SNH4_2D(L,J) + DLTSNH4_2D(L,J) - XMIN)
        NITRIF_2D(L,J) = AMIN1(NITRIF_2D(L,J), SNH4_AVAIL)
        DLTSNH4_2D(L,J) = DLTSNH4_2D(L,J) - NITRIF_2D(L,J)

!       Update available NH4 for the next step
        SNH4_AVAIL = AMAX1(0.0, SNH4_2D(L,J) + DLTSNH4_2D(L,J) - XMIN)

!       ------------------------------------------------------------------
!       N2, N2O, NO fluxes from Nitrification
!       ------------------------------------------------------------------
        if (NITRIF_2D(L,J) > 1.E-6) then

!         for N2O using a proportion of nitrification from original daycent PG
          N2ONitrif_2D(L,J) = pN2Onitrif * NITRIF_2D(L,J) 
          NITRIF_remaining = NITRIF_2D(L,J) - N2ONitrif_2D(L,J)

!         NO flux 
          NO_N2O_ratio(L) = 8.0+(18.0*atan(0.75*PI*(10*dD0(L)-1.86)))/PI
!         0.5 for agricultural systems
          NO_N2O_ratio(L) = NO_N2O_ratio(L) * 0.5
          potential_NOflux = NO_N2O_ratio(L) * krainNO*N2ONitrif_2D(L,J)

          if (potential_NOflux <= NITRIF_remaining) then
            NITRIF_to_NO = potential_NOflux
            NH4_to_NO = 0.0
            NITRIF_remaining = NITRIF_remaining - NITRIF_to_NO
          else 
!           /* take N out of ammonimum to get max NOflux possible */
            NITRIF_to_NO = NITRIF_remaining

!     chp 10/26/2017 - don't remove any from NH4 pool - 
!       affects some experiments, including CCPO9002.BNX, trt 13
            NH4_to_NO = 0.0
!            NH4_to_NO = AMIN1(SNH4_AVAIL,
!     &                         (potential_NOflux - NITRIF_remaining))
            NITRIF_remaining = 0.0
          endif

          nNOflux_2D(L,J) = AMAX1(NITRIF_to_NO + NH4_to_NO, 0.0)
          NITRIF_2D(L,J) = NITRIF_2D(L,J) + NH4_to_NO

        else 
          NO_N2O_ratio(L) = 0.0
          N2ONitrif_2D(L,J) = 0.0
          nNOflux_2D(L,J) = 0.0
          NOflux(L)    = 0.0
          NITRIF_to_NO = 0.0
          NH4_to_NO    = 0.0
          NITRIF_remaining = 0.0
        endif

        DLTSNO3_2D(L,J) = DLTSNO3_2D(L,J) + NITRIF_remaining
        DLTSNH4_2D(L,J) = DLTSNH4_2D(L,J) - NH4_to_NO
!       This contribution from NH4 can be considered as part of the nitrification process
        TNITRIFY   = TNITRIFY   + NITRIF(L) 

        ENDDO  !End of soil row (layer) loop
      END DO   !End of soil column loop



**************************************************************************
!     TEMP CHP
      IF (YRDOY == ReportDate) THEN
        P1 = 20 !Nitrification
        WRITE(555,555)((P1,"NITRIF",L,J,NITRIF_2D(L,J),J=1,J1),L=1,L1)
        WRITE(555,555)((P1,"DLTSNO3",L,J,DLTSNO3_2D(L,J),J=1,J1),L=1,L1)
        WRITE(555,555)((P1,"DLTSNH4",L,J,DLTSNH4_2D(L,J),J=1,J1),L=1,L1)
      ENDIF
**************************************************************************

!*************************************************************************************************
!*************************************************************************************************
!     FROM HERE START 1D PROCESSES. NEED TO CONVERT DLTSNH3_2D AND DLTSNH4_2D TO 1D
!*************************************************************************************************
!*************************************************************************************************

!     Convert NITRIF, DLTSNO3 and DLTSNH4 from 2D to 1D for next set of processes
!     use the utility 
      CALL Cell2Layer_2D(
     &  DLTSNO3_2D, CELLS % Struc, NLAYR,  !Input
     &  DLTSNO3)                           !Output

      CALL Cell2Layer_2D(
     &  DLTSNH4_2D, CELLS % Struc, NLAYR,  !Input
     &  DLTSNH4)                           !Output

      CALL Cell2Layer_2D(
     &  DLTUREA_2D, CELLS % Struc, NLAYR,  !Input
     &  DLTUREA)                           !Output

      CALL Cell2Layer_2D(
     &  NITRIF_2D, CELLS % Struc, NLAYR,  !Input
     &  NITRIF)                           !Output

      CALL Cell2Layer_2D(
     &  N2ONitrif_2D, CELLS % Struc, NLAYR,  !Input
     &  N2ONitrif)                           !Output

      CALL Cell2Layer_2D(
     &  nNOflux_2D, CELLS % Struc, NLAYR,  !Input
     &  nNOflux)                           !Output

      N2O_data % NITRIF   = NITRIF
      N2O_data % N2Onitrif  = N2Onitrif

!     Keep the values of DLTSNO3 and DLTSNH4 because we need to add the new part to the 
!       2D arrays.
      DLTSNO3_SAVE = DLTSNO3
      DLTSNH4_SAVE = DLTSNH4
      DLTUREA_SAVE = DLTUREA

!-----------------------------------------------------------------------
!       Denitrification section
!-----------------------------------------------------------------------
      IF (NSWITCH .EQ. 6) THEN
        DENITRIF = 0.0
      ELSE

        SELECT CASE(MEGHG)
        CASE("1","2")
          CALL Denit_DayCent (CONTROL, ISWNIT, 
     &    dD0, newCO2, NO3, SNO3, SOILPROP,       !Input
     &    DLTSNO3,                                !I/O
     &    CNOX, TNOXD, N2O_data)                  !Output

        CASE DEFAULT
          CALL Denit_Ceres (CONTROL, ISWNIT, 
     &    DUL, KG2PPM, LITC, NLAYR, NO3, SAT,         !Input
     &    SSOMC, SNO3, ST, SW,                        !Input
     &    DLTSNO3,                                    !I/O
     &    CNOX, TNOXD, N2O_data)                      !Output
        END SELECT
      ENDIF

      CALL PUT('NITR','TNOXD',TNOXD) 
      N2ODenit = N2O_data % N2ODenit

!       ------------------------------------------------------------------
!       N2, N2O, NO fluxes from Denitrification
!       ------------------------------------------------------------------
!      /* Now compute NOflux from denitrification (new calculation */
!      /* For denitrification, krainNO is >= 1.0 -mdh 6/22/00 */
!      N2ODenit = N2O_DATA % N2ODenit

!     chp 10/4/2017. Believe it or not, this check for N2Odenit causes a 
!         mass imbalance. Take it out and all is OK.
      !if (sum(n2odenit) > 1.e-9) then
        DO L = 1, NLAYR
          potential_NOflux = NO_N2O_ratio(L) * N2ODenit(L) 
     &                                       * AMIN1(1.0, krainNO)
          SNH4_AVAIL = AMAX1(0.0, SNH4(L) + DLTSNH4(L) - XMIN)

          if (potential_NOflux <= SNH4_AVAIL) then
!           Take all N out of ammonimum pool
            dNOflux(L) = potential_NOflux
            DLTSNH4(L) = DLTSNH4(L) - potential_NOflux
          else 
!           Take N out of available ammonium, then convert some Dn2oflux to NOflux
            dNOflux(L) = SNH4_AVAIL
            DLTSNH4(L) = DLTSNH4(L) - SNH4_AVAIL
            potential_NOflux = potential_NOflux - SNH4_AVAIL

!     chp 10/5/2017
!     This next bit causes a mass imbalance because N2ODenit has
!         already been computed and accumulated. Would need to
!         restructure where N2ODenit is accumulated to fix it.
!     This is rarely needed and amounts are very small. Ignore for now.
            !if (potential_NOflux <= N2ODenit(L)) then
            !  dNOflux(L) = dNOflux(L) + potential_NOflux
            !  N2ODenit(L) = N2ODenit(L) - potential_NOflux
            !else
            !  dNOflux(L) = dNOflux(L) + N2ODenit(L)
            !  N2ODenit(L) = 0.0
            !endif
          endif
        ENDDO

!       Recalculated total denitrified N2O because it may have been modified above.
        !TN2OdenitD = 0.0
!       Sum total NOflux for the day
        DO L = 1, NLAYR
          NOflux(L) = nNOflux(L) + dNOflux(L)
          TNOfluxD = TNOfluxD + NOflux(L)
          !TN2OdenitD = TN2OdenitD + N2ODenit(L)   
        ENDDO

! Don't need the plant resorption routine because:
! 1 - amounts are small
! 2 - we don't have a mechanism to add back to plants
! 3 - it only affects NO, which we aren't tracking anyway, except for balance
 
!        if (XHLAI > 0.0) then
!!         canopy_reduction appears to be the reabsorbed fraction.
!!             This equation is a parabola with the minimum about 8.28. It reduces
!!             absorption for LAI above that so limit LAI to 8.0. The problem seems
!!             to be the simple biomass to LAI ratio. 200 bu/acre corn is
!!             aglivC > 1100 and an LAI > 34. Obviously its not all leaves. */
!          if (XHLAI > 8.0) then
!            canopy_reduction = 0.4428
!          else
!            canopy_reduction = 0.0077*XHLAI*XHLAI + -0.13 * XHLAI + 0.99
!          endif
!
!          NOabsorp = TNOfluxD * (1.0 - canopy_reduction)
!          TNOfluxD = TNOfluxD - NOabsorp    !reduce the NOflux by absorption
!          NOflux = NOflux * canopy_reduction
!          nNOflux = nNOflux * canopy_reduction
!          dNOflux = dNOflux * canopy_reduction
!        else
!          canopy_reduction = 0.0
!        endif
!      endif
!
!      write(557,'(i7,2e10.3)') yrdoy, 1.0 - canopy_reduction, NOabsorp

      N2O_DATA % NOflux = NOflux

!     ------------------------------------------------------------------
!     N emissions to atmosphere
!     ------------------------------------------------------------------
      CALL N2Oemit(CONTROL, ISWITCH, dD0, SOILPROP, N2O_DATA) 

!*************************************************************************************************
!*************************************************************************************************

!     END OF 1D GHG PROCESSES, NOW BACK TO 2D.
!     FIRST NEED TO CONVERT DLT-N VALUES BACK TO 2D

!     Look at only the differences in DLTSNO3 and DLTSNH4 due to GHG processes
      DLTSNO3_DIFF = DLTSNO3 - DLTSNO3_SAVE
      DLTSNH4_DIFF = DLTSNH4 - DLTSNH4_SAVE
      DLTUREA_DIFF = DLTUREA - DLTUREA_SAVE

!     Convert DLTSNO3 and DLTSNH4 differences  to 2D arrays
      CALL Layer2Cell_2D(                              
     & CELLS % Struc, NLAYR, DLAYR, DLTSNO3_DIFF, 0.0,        !Input
     & DLTSNO3_DIFF_2D)                                       !Output

      CALL Layer2Cell_2D(                              
     & CELLS % Struc, NLAYR, DLAYR, DLTSNH4_DIFF, 0.0,        !Input
     & DLTSNH4_DIFF_2D)                                       !Output

      CALL Layer2Cell_2D(                              
     & CELLS % Struc, NLAYR, DLAYR, DLTUREA_DIFF, 0.0,        !Input
     & DLTUREA_DIFF_2D)                                       !Output

      RESID3 = 0.0
      RESID4 = 0.0
      RESID5 = 0.0

!     Add these differences into the 2D DLT arrays, cell by cell, correcting for negative values
      DO L = 1, NRowsTot
        DO J = 1, NColsTot
!         Add in NO3 differences due to GHG and NFLUX processes
          DLTSNO3_2D(L,J) = DLTSNO3_2D(L,J) + DLTSNO3_DIFF_2D(L,J)
!         Add in residual from previous cell (if any)
          DLTSNO3_2D(L,J) = DLTSNO3_2D(L,J) + RESID3
!         pseudo-integration
          SNO3_TEMP = SNO3_2D(L,J) + DLTSNO3_2D(L,J)
          IF (SNO3_TEMP .LT. 0.0) THEN
!           Maximum DLTSNO3 = SNO3 at the beginning of the day
            DLTSNO3_2D(L,J) = - SNO3_2D(L,J)
!           RESID3 = Negative SNO3 value passed on to next cell
            RESID3 = SNO3_TEMP
          ELSE
            RESID3 = 0.0
          ENDIF

!         Add in NH4 differences due to GHG and NFLUX processes
          DLTSNH4_2D(L,J) = DLTSNH4_2D(L,J) + DLTSNH4_DIFF_2D(L,J)

!         Add in residual from previous cell (if any)
          DLTSNH4_2D(L,J) = DLTSNH4_2D(L,J) + RESID4

!         pseudo-integration
          SNH4_TEMP = SNH4_2D(L,J) + DLTSNH4_2D(L,J)
          IF (SNH4_TEMP .LT. 0.0) THEN
!           Maximum DLTSNH4 = SNH4 at the beginning of the day
            DLTSNH4_2D(L,J) = - SNH4_2D(L,J)

!           RESID4 = Negative NH4 value passed on to next cell
            RESID4 = SNH4_TEMP
          ELSE
            RESID4 = 0.0
          ENDIF

!         Add in urea differences due to GHG and NFLUX processes
          DLTUREA_2D(L,J) = DLTUREA_2D(L,J) + DLTUREA_DIFF_2D(L,J)
!         Add in residual from previous cell (if any)
          DLTUREA_2D(L,J) = DLTUREA_2D(L,J) + RESID5
!         pseudo-integration
          UREA_TEMP = UREA_2D(L,J) + DLTUREA_2D(L,J)
          IF (UREA_TEMP .LT. 0.0) THEN
!           Maximum DLTSNH4 = SNH4 at the beginning of the day
            DLTUREA_2D(L,J) = - UREA_2D(L,J)
!           RESID5 = Negative urea value passed on to next cell
            RESID5 = UREA_TEMP
          ELSE
            RESID5 = 0.0
          ENDIF
        ENDDO
      ENDDO


**************************************************************************
!     TEMP CHP
      IF (YRDOY == ReportDate) THEN
        P1 = 22  !After adding in 1D process rates
        WRITE(555,555)((P1,"DLTSNO3",L,J,DLTSNO3_2D(L,J),J=1,J1),L=1,L1)
        WRITE(555,555)((P1,"DLTSNH4",L,J,DLTSNH4_2D(L,J),J=1,J1),L=1,L1)
      ENDIF
**************************************************************************

!*************************************************************************************************
!*************************************************************************************************
!    NFLUX is done in 1D for 1D simulations and 2D for 2D simulations. That is, the DLTUREA and
!     DLTSNO3 are updated for 1D simulations. The DLTUREA_2D and DLTSNO3_2D are updated for the 2D 
!     simulation. 

!     At this point, the 1D processes above have changed the 1D DLT variables. 1D NFLUX
!     will vary those 1D DLT variables further. 

!     If it is a 2D simulation, then the 1D processes above have not yet been incorporated into the
!     2D flux variable. Calculate the 2D NFLUX first, then add in the 1D process effects to the 
!     2D DLT variables.
!*************************************************************************************************
!     ------------------------------------------------------------------
!     Downward and upward N movement with the water flow.
!     ------------------------------------------------------------------
      TLeachD = 0.0
      NTILEDR = 0.0   !HJ added

      IF (.NOT. SIM2D) THEN  !1D simulation
        IF (IUON) THEN
          NSOURCE = 1    !Urea.
          CALL NFLUX ( 
     &      ADCOEF, BD, DLAYR, DRN, DUL, UPFLOW, NLAYR,     !Input
     &      UREA, NSOURCE, SW, TDFC, TDLNO,                 !Input
     &      DLTUREA, CLeach, TLeachD, CNTILEDR, NTILEDR)    !Output !HJ
        ENDIF

        NSOURCE = 2   !NO3.
        CALL NFLUX ( 
     &    ADCOEF, BD, DLAYR, DRN, DUL, UPFLOW, NLAYR,       !Input
     &    SNO3, NSOURCE, SW, TDFC, TDLNO,                   !Input
     &    DLTSNO3, CLeach, TLeachD, CNTILEDR, NTILEDR)      !Output !HJ

      ELSE !2D simulation
        IF (IUON) THEN
          NSOURCE = 1    !Urea.
          CALL NFLUX_2D (DYNAMIC, 
     &      ADCOEF, BD, CELLS, ColFrac, DUL, UREA_2D,       !Input
     &      NSOURCE, SWV,                                   !Input
     &      DLTUREA_2D, CLeach, TLeachD_UREA,               !Output
     &      NFlux_UREA_L, NFlux_UREA_R, NFlux_UREA_D,       !Output
     &      NFlux_UREA_U)                                   !Output
        ENDIF

        NSOURCE = 2   !NO3.
        CALL NFLUX_2D (DYNAMIC, 
     &      ADCOEF, BD, CELLS, ColFrac, DUL, SNO3_2D,       !Input
     &      NSOURCE, SWV,                                   !Input
     &      DLTSNO3_2D, CLeach, TLeachD, NFlux_L, NFlux_R,  !Output
     &      NFlux_D, NFlux_U)                               !Output
        
        IF (IUON) THEN
          TLeachD = TLeachD + TLeachD_UREA
          NFlux_L = NFlux_L + NFlux_UREA_L
          NFlux_R = NFlux_R + NFlux_UREA_R
          NFlux_D = NFlux_D + NFlux_UREA_D
          NFlux_U = NFlux_U + NFlux_UREA_U
        ENDIF

        CNTILEDR = 0.0
        NTILEDR = 0.0
      ENDIF

      CALL PUT('NITR','TLCHD',TLeachD) 

**************************************************************************
!     TEMP CHP
      IF (YRDOY == ReportDate) THEN
        P1 = 25 !nflux
        WRITE(555,555)((P1,"NFlux_L",L,J,NFlux_L(L,J),J=1,J1),L=1,L1)
        WRITE(555,555)((P1,"NFlux_R",L,J,NFlux_R(L,J),J=1,J1),L=1,L1)
        WRITE(555,555)((P1,"NFlux_D",L,J,NFlux_D(L,J),J=1,J1),L=1,L1)
        WRITE(555,555)((P1,"NFlux_U",L,J,NFlux_U(L,J),J=1,J1),L=1,L1)
        WRITE(555,555)((P1,"DLTSNO3",L,J,DLTSNO3_2D(L,J),J=1,J1),L=1,L1)
        WRITE(555,555)((P1,"DLTSNH4",L,J,DLTSNH4_2D(L,J),J=1,J1),L=1,L1)
      ENDIF
**************************************************************************

!*************************************************************************************************
!*************************************************************************************************
!     TEMP CHP
!      call SUM_N(Cell_Type, DLTSNO3_2D, DLTSNH4_2D, !Input
!     &      ColFrac, SNO3_2D, SNH4_2D,                    !Input
!     &      TotNO3, TotNH4, TotDeltNO3, TotDeltNH4,       !Output
!     &      TotN, TotDeltN, LastTotN, LastTotDeltN)       !Output

      Cells % State % SNH4 = SNH4_2D  !kg/ha
      Cells % State % SNO3 = SNO3_2D  !kg/ha
      Cells % State % UREA = UREA_2D

!***********************************************************************
!     NEED TO CONVERT 2D to 1D HERE FIRST!
!***********************************************************************
!     Plant available N should be available in 2D!!!
!     Psuedo-integration - for plant-available N
      DO L = 1, NLAYR
        NO3_plant(L) = (SNO3(L) + DLTSNO3(L)) * KG2PPM(L)
        NH4_plant(L) = (SNH4(L) + DLTSNH4(L)) * KG2PPM(L)
      ENDDO

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


**************************************************************************
!     TEMP CHP
      IF (YRDOY == ReportDate) THEN
        SELECT CASE(DYNAMIC)
        CASE(2); P1 = 0  !Initialization
        CASE(4); P1 = 50 !Start of integration
        END SELECT

        DO L = 1, L1
          DO J = 1, J1
            WRITE(555,556) P1,"SNO3      ",L,J,SNO3_2D(L,J)
            WRITE(555,556) P1,"SNH4      ",L,J,SNH4_2D(L,J)
            WRITE(555,556) P1,"DLTSNO3   ",L,J,DLTSNO3_2D(L,J)
            WRITE(555,556) P1,"DLTSNH4   ",L,J,DLTSNH4_2D(L,J)
          ENDDO
        ENDDO

      ENDIF
**************************************************************************

      IF (DYNAMIC .EQ. INTEGR) THEN
!       Update flood N components.
        IF (NBUND > 0) THEN
          CALL FLOOD_CHEM(CONTROL, ISWITCH, 
     &    FLOODWat, LFD10, SOILPROP, SNO3, SNH4, UREA,    !Input
     &    SRAD, SW, TMAX, TMIN, XHLAI,                    !Input
     &    DLTSNH4, DLTSNO3, DLTUREA, FLOODN, OXLAYR,      !I/O
     &    ALGFIX, BD1, CUMFNRO, TOTAML, TOTFLOODN)        !Output
        ENDIF
      
        !Update oxidation layer variables
        CALL OXLAYER(CONTROL,
     &    BD1, ES, FERTDATA, FLOODWAT, LFD10,              !Input
     &    NSWITCH, SNH4, SNO3, SOILPROP, SRAD, ST,        !Input
     &    SW, TMAX, TMIN, UREA, XHLAI,                    !Input
     &    DLTSNH4, DLTSNO3, DLTUREA, OXLAYR,              !I/O
     &    ALI, TOTAML)                                    !Output
      ENDIF

!     Loop through soil layers for integration
      DO L = 1, NRowsTot
        DO J = 1, NColsTot
          SNO3_2D(L, J) = SNO3_2D(L, J) + DLTSNO3_2D(L, J)    

!!     *******************************
!!     temp chp
!      IF (L ==1 .AND. J==1) THEN
!        write(1234,'(I7,2F10.5,2X,A)') 
!     &    YRDOY, SNH4_2D(L,J), DLTSNH4_2D(L,J), "Before"
!      ENDIF
!!     *******************************

          SNH4_2D(L, J) = SNH4_2D(L, J) + DLTSNH4_2D(L, J)    

!!     *******************************
!!     temp chp
!      IF (L ==1 .AND. J==1) THEN
!        write(1234,'(I7,2F10.5,2X,A)') 
!     &    YRDOY, SNH4_2D(L,J), DLTSNH4_2D(L,J), "After"
!      ENDIF
!!     *******************************

          UREA_2D(L, J) = UREA_2D(L, J) + DLTUREA_2D(L, J)

!!         Underflow trapping
!          IF (ABS(SNO3_2D(L, J)) .LT. 1.E-8) SNO3_2D(L, J) = 0.0
!          IF (ABS(SNH4_2D(L, J)) .LT. 1.E-8) SNH4_2D(L, J) = 0.0
!          IF (ABS(UREA_2D(L, J)) .LT. 1.E-8) UREA_2D(L, J) = 0.0

!         Concentration
          SELECT CASE(Cell_type(L,J))
!          CASE(3)
!             NO3_2D(L, J) = SNO3_2D(L, J) * KG2PPM(L) / BedFrac(L,J)
!             NH4_2D(L, J) = SNH4_2D(L, J) * KG2PPM(L) / BedFrac(L,J)
!             UPPM_2D(L,J) = UREA_2D(L, J) * KG2PPM(L) / BedFrac(L,J)
!          CASE(4,5)
          CASE(3,4,5)
             NO3_2D(L, J) = SNO3_2D(L, J) * KG2PPM(L) / ColFrac(L,J)
             NH4_2D(L, J) = SNH4_2D(L, J) * KG2PPM(L) / ColFrac(L,J)
             UPPM_2D(L,J) = UREA_2D(L, J) * KG2PPM(L) / ColFrac(L,J)
          END SELECT
        ENDDO
      ENDDO  

!     Call NCHECK to check for and fix negative values.
      CALL NCHECK_inorg(CONTROL, Sim2D,
     &    NH4_2D, NO3_2D, SNH4_2D, SNO3_2D, UREA_2D) !Input  

!     Soil profile accumulations.
      TNH4   = 0.0
      TNO3   = 0.0
      TUREA  = 0.0
      TN2OnitrifD = 0.0

      DO L = 1, NRowsTot

!       1D accumulations and integrations
        TN2OnitrifD = TN2OnitrifD + N2Onitrif(L)

        DO J = 1, NColsTot
          if (Cell_Type(L,J) .EQ. 0) then
            cycle
          endif

!         2D accumulations and integrations
          TNH4  = TNH4  + SNH4_2D(L, J)
          TNO3  = TNO3  + SNO3_2D(L, J)
          TUREA = TUREA + UREA_2D(L, J)

!     Already calculated WTNUP in rate section
!         Calculate this where uptake is removed from the soil.
!         WTNUP in g[N]/m2 - convert to kg/ha in CNUPTAKE (below) 
!         WTNUP = WTNUP + (UNO3_2D(L,J) + UNH4_2D(L,J))/10.*ColFrac(L,J)
!         WTNUP = WTNUP + (UNO3(L) + UNH4(L)) / 10.    !g[N]/m2 cumul.

!!         For detailed cell output (commented out for now)
!             Cell_Ndetail % NFlux_L_out(L, J)  = NFlux_L(L, J) 
!             Cell_Ndetail % NFlux_R_out(L, J)  = NFlux_R(L, J)
!             Cell_Ndetail % NFlux_D_out(L, J)  = NFlux_D(L, J)
!             Cell_Ndetail % NFlux_U_out(L, J)  = NFlux_U(L, J) 
!          if (L > 1) then
!              if (Cell_Type(L - 1, J) .NE. 0) then
!                Cell_Ndetail % NFlux_U_in(L, J)  = NFlux_D(L - 1, J)
!     &                          * ColFrac(L-1, j) / ColFrac(L, J)
!              endif
!          endif
!          if (L < NRowsTot) then
!              Cell_Ndetail % NFlux_D_in(L, J)  = NFlux_U(L + 1, J)
!     &                           * ColFrac(L+1, j) / ColFrac(L, J)
!          endif
!          if (J > 1) then
!              Cell_Ndetail % NFlux_L_in(L, J)  = NFlux_R(L, J - 1)
!     &                           * ColFrac(L, j-1) / ColFrac(L, J)
!          endif
!          if (J < NColsTot) then
!              if (Cell_Type(L, J + 1) .NE. 0) then
!                Cell_Ndetail % NFlux_R_in(L, J)  = NFlux_L(L, J + 1)
!     &                               * ColFrac(L, j+1) / ColFrac(L, J)
!              endif
!          Endif
        ENDDO
      ENDDO

      TNH4NO3 = TNH4 + TNO3

      CELLS%State%SNH4 = SNH4_2D
      CELLS%State%SNO3 = SNO3_2D
      
!     Convert 2D to 1D
!     Use Interpolate2Layers_2D for concentration variables
      CAll Interpolate2Layers_2D(NO3_2D, Cells%Struc, NLAYR,  !input
     &         NO3)                                           !Output
      CAll Interpolate2Layers_2D(NH4_2D, Cells%Struc, NLAYR,  !input
     &         NH4)                                           !Output
      CAll Interpolate2Layers_2D(UPPM_2D, Cells%Struc, NLAYR,  !input
     &         UPPM)                                           !Output

!     Use Cell2Layer_2D for mass variables
      CALL Cell2Layer_2D(
     &  SNO3_2D, Cells%Struc, NLAYR,                      !Input
     &  SNO3, SurfaceVal)                                 !Output
      CALL Cell2Layer_2D(
     &  SNH4_2D, Cells%Struc, NLAYR,                      !Input
     &  SNH4, SurfaceVal)                                 !Output
      CALL Cell2Layer_2D(
     &  UREA_2D, Cells%Struc, NLAYR,                      !Input
     &  UREA, SurfaceVal)                                 !Output

      TMINERN = 0.0
      TIMMOBN = 0.0
      DO L = 1, NRowsTot

        IF (L ==1) THEN
          TMINERN = MNR(0,N) + MNR(1,N)
          TIMMOBN = IMM(0,N) + IMM(1,N)
        ELSE
          TMINERN = TMINERN + MNR(L,N)
          TIMMOBN = TIMMOBN + IMM(L,N)
        ENDIF
      ENDDO

!     Seasonal cumulative values
      CMINERN  = CMINERN  + TMINERN       !mineralization
      CIMMOBN  = CIMMOBN  + TIMMOBN       !immobilization
      CNETMINRN= CMINERN  - CIMMOBN       !net mineralization

      CNITRIFY   = CNITRIFY   + TNITRIFY      !Nitrification
      CN2Onitrif = CN2Onitrif + TN2OnitrifD   !N2O from nitrification
      CNOflux    = CNOflux    + TNOfluxD      !NO flux
!     These are accumulated in the Denit routines:
!     CNOX       = CNOX       + TNOXD         !Denitrification
!     CN2Odenit  = CN2Odenit  + TN2OdenitD    !N2O from denitrification
!     CN2        = CN2        + TN2D          !N2 flux                 

      CNUPTAKE = WTNUP * 10.

      N2O_data % NITRIF   = NITRIF
      N2O_data % TNITRIFY = TNITRIFY
      N2O_data % CNITRIFY = CNITRIFY

      N2O_data % N2Onitrif  = N2Onitrif
      N2O_data % TN2OnitrifD= TN2OnitrifD
      N2O_data % CN2Onitrif = CN2Onitrif

      N2O_data % NOflux  = NOflux
      N2O_data % TNOfluxD= TNOfluxD
      N2O_data % CNOflux = CNOflux

!     HJ added CNTILEDR in SoilNiBal and OpSoilNi
      IF (DYNAMIC .EQ. SEASINIT) THEN
        CALL SoilNiBal (CONTROL, ISWITCH,
     &    ALGFIX, CIMMOBN, CMINERN, CUMFNRO, FERTDATA, NBUND, CLeach,  
     &    CNTILEDR, TNH4, TNO3, TOTAML, TOTFLOODN, TUREA, CNUPTAKE,
     &    N2O_data) 

!        Call CellNDetail_2D (CONTROL, ISWITCH, 
!     &    FERTDATA, TNH4, TNO3, TUREA,   
!     &    Cells, DENITRIF, IMM, MNR) 

        CALL OpSoilNi(CONTROL, ISWITCH, SoilProp, 
     &    CIMMOBN, CMINERN, CNETMINRN, CNITRIFY, CNUPTAKE, 
     &    FertData, NH4, NO3, 
     &    CLeach, CNTILEDR, TNH4, TNH4NO3, TNO3, TUREA, CNOX, TOTAML)

!        CALL OpSoilNi_2D(CONTROL, ISWITCH, SoilProp, 
!     &    CIMMOBN, CMINERN, CNETMINRN, CNITRIFY, CNUPTAKE, 
!     &    FertData, NH4_2D, NO3_2D, 
!     &    CLeach, TNH4, TNH4NO3, TNO3, TNOXD, TOTAML)
      ENDIF

      NO3_plant = NO3
      NH4_plant = NH4

      Cells % State % SNH4 = SNH4_2D  !kg/ha
      Cells % State % SNO3 = SNO3_2D  !kg/ha
      Cells % State % UREA = UREA_2D


**************************************************************************
!     TEMP CHP
      IF (YRDOY == ReportDate) THEN
        SELECT CASE(DYNAMIC)
        CASE(2); P1 = 1  !End of initialization
        CASE(4); P1 = 60 !End of integration
        END SELECT
        WRITE(555,555)((P1,"SNO3",L,J,SNO3_2D(L,J),J=1,J1),L=1,L1)
        WRITE(555,555)((P1,"SNH4",L,J,SNH4_2D(L,J),J=1,J1),L=1,L1)
      ENDIF
**************************************************************************

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

      IF (NBUND > 0) THEN
        CALL FLOOD_CHEM(CONTROL, ISWITCH, 
     &    FLOODWat, LFD10, SOILPROP, SNO3, SNH4, UREA,    !Input
     &    SRAD, SW, TMAX, TMIN, XHLAI,                    !Input
     &    DLTSNH4, DLTSNO3, DLTUREA, FLOODN, OXLAYR,      !I/O
     &    ALGFIX, BD1, CUMFNRO, TOTAML, TOTFLOODN)        !Output
      ENDIF

      CALL SoilNiBal (CONTROL, ISWITCH,
     &    ALGFIX, CIMMOBN, CMINERN, CUMFNRO, FERTDATA, NBUND, CLeach,  
     &    CNTILEDR, TNH4, TNO3, TOTAML, TOTFLOODN, TUREA, CNUPTAKE,
     &    N2O_data) 

!        Call CellNDetail_2D (CONTROL, ISWITCH, 
!     &    FERTDATA, TNH4, TNO3, TUREA,   
!     &    Cells, DENITRIF, IMM, MNR) 

      CALL OpN2O(CONTROL, ISWITCH, SOILPROP, N2O_DATA) 

      CALL OPGHG(CONTROL, ISWITCH, N2O_data, CH4_data)

C***********************************************************************
C***********************************************************************
C     END OF SECOND DYNAMIC IF CONSTRUCT
C***********************************************************************
      ENDIF
C-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE SoilNi

!=======================================================================
! NTRANS Variables - updated 08/20/2003
!-----------------------------------------------------------------------
! AD            Lowest possible volumetric water content of a soil layer 
!                 when it is air dry. For the top soil layer AD may be 
!                 lower than the lower limit LL, because this layer may dry 
!                 out due to water evaporation at the soil surface.
!                 (cm3 [H2O]/ cm3 [soil])
! ADCOEF(L)     Anion adsorption coefficient for soil layer L;  for reduced 
!                 anion (nitrate) flow in variable-charge soils (ADCOEF = 0 
!                 implies no anion retention) (cm3 (H2O] / g [soil])
! ADDSNH4(L)     Rate of change of ammonium in soil layer L (kg [N] / ha / d)
! ADDSNO3(L)     Rate of change of nitrate in soil layer L (kg [N] / ha / d)
! ADDUREA(L)     Rate of change of urea content in soil layer L
!                     (kg [N] / ha / d)
! AK            Maximum hydrolysis rate of urea (i.e. proportion of urea 
!                 that will hydrolyze in 1 day under optimum conditions). 
!                 AK >= 0.25. Also: Today's value of the nitrification 
!                  potential, calculated from the previous day's value (d-1)
! ALGFIX        N in algae (kg [N] / ha)
! ALI            
! AMTFER        Amount of N in fertilizer applications
! BD(L)         Bulk density, soil layer L (g [soil] / cm3 [soil])
! BD1           Bulk density of oxidized layer (g [soil] / cm3 [soil])
! CIMMOBN        Cumulative seasonal net immobilization of N in soil profile (kg[N]/ha) 
! CLeach         Season cumulative N leached from soil (kg [N] / ha)
! CMINERN        Cumulative seasonal mineralization of N in soil profile (kg[N]/ha)
! CNETMINRN      Cumulative seasonal net mineralization of N in soil profile (kg[N]/ha) 
!                = mineralization - immobilization
! CNITRIFY       Cumulative nitrification (kg [N] / ha)
! CNUPTAKE       Cumulative N uptake
! ColFrac(col)   Width of each column as a fraction of the total row width
! CONTROL       Composite variable containing variables related to control 
!                 and/or timing of simulation.  The structure of the 
!                 variable (ControlType) is defined in ModuleDefs.for. 
! CUMFNRO       Cumulative N lost in runoff over bund (kg [N] / ha)
! CW             Water extractable SOM carbon (µg [C] / g [soil])
! DENITRIF(L, J) Denitrification rate (kg [N] / ha / d)
! DLAG_2D(L, J)    Number of days with soil water content greater than the 
!                 drained upper limit for soil layer L.  For 
!                 denitrification to occur, DLAG must be greater than 4 (4 days
!                 lag period before denitrification begins). 
! DLAYR(L)      Thickness of soil layer L (cm)
! DLTFON(L)     Rate of change of N in fresh organic residue  in soil layer 
!                 L (kg [N] / ha / d)
! DLTFPOOL(L,J) Rate of change of FOM pool (FPOOL) in soil layer L 
!                 (J=1=carbohydrate, 2=cellulose, 3=lignin)
!                 (kg [residue pool] / ha / d)
! DLTSNH4(L)    Rate of change of ammonium in soil layer L
!                (kg [N] / ha / d)
! DLTSNH4_2D(L,J))Rate of change of ammonium in soil cell
!                   (kg [N] / ha / d)
! DLTSNO3(L)    Rate of change of nitrate in soil layer L (kg [N] / ha / d)
! DLTSNO3_2D(L,J) Rate of change of nitrate in soil cell (kg [N] / ha / d)
! DLTUREA(L)    Rate of change of urea content in soil layer L
!                (kg [N] / ha / d)
! DLTUREA_2D(L,J) Rate of change of urea content in soil cell
!                   (kg [N] / ha / d)
! DMINR         Maximum decomposition rate constant of stable organic 
!                 matter (d-1)
! DNFRATE       Maximum denitrification rate (kg [N] / ha / d)
! DOY           Current day of simulation (d)
! DRN(L)        Drainage rate through soil layer L (cm/d)
! DSNC          Depth to which C and N are integrated across all soil 
!                 layers for output in CARBON.OUT (cm)
! DUL(L)        Volumetric soil water content at Drained Upper Limit in 
!                 soil layer L (cm3[water]/cm3[soil])
! ES            Actual soil evaporation rate (mm/d)
! FERTDAY        Date of last fertilizer application (YYYYDDD)
! FERTDATA       Fertilizer type
! FertFactor     Ratio between ROWS space and BEDWD
! FLOOD         Current depth of flooding (mm)
! FLOODN        Composite variable which contains flood nitrogen mass and 
!                 concentrations. Structure of variable is defined in 
!                 ModuleDefs.for. (var.)
! FLOODWAT      Composite variable containing information related to bund 
!                 management. Structure of variable is defined in 
!                 ModuleDefs.for. 
! UPFLOW(L)     Movement of water between unsaturated soil layers due to 
!                 soil evaporation: + = upward, -- = downward (cm/d)
! FOM(L)        Fresh organic residue in soil layer L
!                (kg [dry matter] / ha)
! FON(L)        Nitrogen in fresh organic matter in soil layer L
!                (kg [N] / ha)
! FPOOL(L,J)    FOM pool in soil layer L: J=1:carbohydrate, 2:cellulose, 
!                 3:lignin (kg [residue pool] / ha)
! SSOMC(L)       Carbon in stable organic matter (humus) (kg [C] / ha)
! HUMFRAC       Humus fraction that decomposes (fraction)
! IMM            Cell immobilization of element IEL (1=N, 2=P) in soil layer L . (kg/ha)
!                The amounts of inorganic N and P that are removed from 
!                 the soil (immobilized) when organic matter decomposes.   
! ISWITCH       Composite variable containing switches which control flow 
!                 of execution for model.  The structure of the variable 
!                 (SwitchType) is defined in ModuleDefs.for. 
! ISWNIT         Nitrogen simulation switch (Y or N) 
! IUOF          Critical Julian day when all urea is assumed to be 
!                 hydrolyzed (this is assumed to occur 21 days after the 
!                 urea application) (d)
! IUON          Flag indicating presence of urea (true or false) 
! KG2PPM(L)     Conversion factor to switch from kg [N] / ha to mg [N] /kg 
!                 [soil] for soil layer L 
! LFD10         Date, 10 days after last fertilization.  Used to determine 
!                 whether hourly flood chemistry computations will be done 
!                 (see DAILY variable). (YYYYDDD)
! LITC           Carbon in fresh organic matter in units of kg[C]/ha
!                  soil evaporation: + = upward, -- = downward (cm/d)
! LL(L)         Volumetric soil water content in soil layer L at lower 
!                 limit (cm3 [water] / cm3 [soil])
! MaxCols
! MaxRows        Equals to NL
! MNR(L,IEL)     Cell mineralization of element IEL (1=N, 2=P) in soil layer cell 
!                  The amounts of inorganic N and P that are added to the soil
!                  when organic matter decomposes.  (kg/ha/d)
! NBUND         Number of bund height records 
! NColsTot       
! NH4_2D(L,J)    Ammonium N in soil cell (µg[N] / g[soil])
! NH4(L)        Ammonium N in soil layer L (�g[N] / g[soil])
! NITRIFppm        Nitrification rate (kg [N] / ha - d)
! NITRIF(L,J)    Cell daily nitrification rate (kg [N] / ha / d)
! NL            Maximum number of soil layers = 20 
! NLAYR         Actual number of soil layers 
! NNOM          Net mineral N release from all SOM sources (kg [N] / ha)
! NO3_2D(L,J)    Nitrate N in soil cell (µg[N] / g[soil])
! NO3(L)         Nitrate N in soil layer L (µg[N] / g[soil])
! NRowsTot       Equals to NLAYR
! NSOURCE       Flag for N source (1 = urea, 2 = NO3) 
! NSWITCH       Nitrogen switch - can be used to control N processes (0-No 
!                 N simulated, 1-N simulated, 5-Nitrification off, 
!                 6-denitrification off, 7-floodwater loss off, 8-drainage 
!                 losses off, 9-leaching off, 10-runoff losses off 
! OXLAYR        Composite variable which contains data about oxidation 
!                 layer.  See ModuleDefs.for for structure. 
! PH(L)         pH in soil layer L 
! PHFACT        Effect of pH on various soil N processes; value varies with 
!                 routine 
! SAT(L)        Volumetric soil water content in layer L at saturation
!                (cm3 [water] / cm3 [soil])
! SNH4(L)       Total extractable ammonium N in soil layer L (kg [N] / ha)
! SNH4_2D(L,J)   Total extractable ammonium N in soil layer L (kg [N] / ha)
! SNH4_AVAIL    Maximum amount of NH4 that may nitrify (kg [N] / (ha - d))
! SNO3(L)       Total extractable nitrate N in soil layer L (kg [N] / ha)
! SNO3_2D(L,J)   Total extractable nitrate N in soil layer L (kg [N] / ha)
! SNO3_AVAIL    NO3 available for denitrification (kg [N] / ha)
! SOILPROP      Composite variable containing soil properties including 
!                 bulk density, drained upper limit, lower limit, pH, 
!                 saturation water content.  Structure defined in ModuleDefs. 
! SRAD          Solar radiation (MJ/m2-d)
! SSOMC(L)       Carbon in stable organic matter (humus) (kg [C] / ha)
! ST(L)         Soil temperature in soil layer L (�C)
! SUMFERT
! SW(L)         Volumetric soil water content in layer L
!                (cm3 [water] / cm3 [soil])
! SWV(L,J)       Volumetric soil water content in layer L
! SWEF          Soil water evaporation fraction; fraction of lower limit 
!                 content to which evaporation can reduce soil water 
!                 content in top layer (fraction)
! T2            Temperature factor for nitrification 
! TFACTOR       Temperature factor for nitrification 
! TFDENIT       Temperature factor for denitrification rate (range 0-1) 
! TFNITY_2D(L,J) 
! TFNITY(L)     Yesterday�s soil temperature factor for nitrification 
!                 (range 0-1) 
! TFUREA        Soil temperature factor for urea hydrolysis (range 0-1) 
! TIMMOBILIZE   Cumulative N immoblized (kg [N] / ha)
! TIMMOBN(J)     Cumulative N immobilization (kg [N] / ha /d)for Jth column
! TKELVIN       Soil temperature (oK)
! TLAG          Temperature factor for nitrification (0-1) 
! CLeach        Cumulative N leached from soil (kg [N] / ha)
! TMAX          Maximum daily temperature (�C)
! TMIN          Minimum daily temperature (�C)
! TMINERALIZE   Cumulative mineralization (kg [N] / ha)
! TMINERN        Daily cumulative mineralization (kg [N]/ha/d) 
! TNH4          Total extractable ammonium N in soil profile (kg [N] / ha)
! TNH4NO3       Total amount of inorganic N (NH4 and NO3) across soil 
!                 profile (kg [N] / ha)
! CNITRIFY      Cumulative nitrification (kg [N] / ha)
! TNO3          Total extractable nitrate N in soil profile (kg [N] / ha)
! CNOX          Cumulative denitrification across the total soil profile adding to 
!                 the nitrous oxide (NOx) pool of the air (kg [N] / ha)
! TOTAML        Cumulative ammonia volatilization (kg [N] / ha)
! TOTFLOODN     Current N in flood water (kg [N] / ha)
! TUREA         Total urea in soil profile (kg [N] / ha)
! UHYDR         Rate of urea hydrolysis (kg [N] / ha - d)
! UNH4(L)       Rate of root uptake of NH4, computed in NUPTAK
!                (kg [N] / ha - d)
! UNH4_2D(L,J)   Rate of root uptake of NH4, computed in NUPTAK
!                 (kg [N] / ha - d)
! UNO3(L)       Rate of root uptake of NO3, computed in NUPTAK
!                (kg [N] / ha -d)
! UPFLOW(L)      Movement of water between unsaturated soil layers due to 
! UNO3_2D(L,J)   Rate of root uptake of NO3, computed in NUPTAK
!                 (kg [N] / ha -d)
! UREA(L)       Amount of urea in soil layer L (kg [N] / ha)
! UREA_2D(L,J)   Amount of urea in soil cell (kg [N] / ha)
! WF2           Water factor for nitrification 
! WFDENIT       Soil water factor for denitrification rate (range 0-1) 
! WFPL          Water factor for nitrification 
! WFSOM         Reduction factor for SOM decay based on soil water content 
!                 (no reduction for SW = DUL, 100% reduction for SW = LL, 
!                 50% reduction for SW = SAT) 
! WFUREA        Reduction of urea hydrolysis due to soil water content 
!                 (range = 0-1; no reduction for SW = DUL, 20% reduction 
!                 for SW = LL, 70% reduction for SW = SAT (fraction)
! WTNUP         Cumulative N uptake (g[N] / m2)
! XHLAI         Healthy leaf area index (m2[leaf] / m2[ground])
! XL            Excess water (above DUL) as a fraction of the maximum 
!                 amount of excess water (i.e. saturated). (fraction)
! XMIN          Amount of NH4 that cannot be immobilized but stays behind 
!                 in soil as NH4; Also, Amount of NO3 that cannot denitrify 
!                 but stays behind in the soil as NO3 (kg [N] / ha)
! YEAR          Year of current date of simulation 
! YRDOY          Current day of simulation (YYDDD)
!***********************************************************************

