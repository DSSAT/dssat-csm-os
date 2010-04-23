!***********************************************************************
!  SOILCBAL_C   Subroutine for CENTURY-based SOM/residue module of DSSAT.
!
!  Purpose: Calculate the carbon balance of the crop and of SOM/litter.
!
!  REVISION HISTORY
!  03/10/2001 AJG Written.
!  03/09/2002 AJG Removed the code on crop carbon (now a separate routine).
!  11/01/2002 AJG Adapted for seasonal/sequential runs with output for 
!                 both an individual season and a complete sequential run.
!                 
!  Called: CENTURY
!  Calls : ERROR, GETLUN, HEADER, YR_DOY
!***********************************************************************

      SUBROUTINE SOILCBAL_C (CONTROL, ISWITCH, 
     &  ACCCO2, LITC, OMAData, SENESCE,                   !Input
     &  SOM1C, TLITC, TSOMC, YRDOY)                       !Input

!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         !which contain control information, soil
                         !parameters, hourly weather data.

      IMPLICIT  NONE
      SAVE
!     ------------------------------------------------------------------
      LOGICAL FEXIST

      CHARACTER*1  IDETC, IDETL, RNMODE
      CHARACTER*11 ERRKEY
      CHARACTER*12 SCBAL
      PARAMETER (ERRKEY = 'SCBAL')

      INTEGER DAS, DOY, DYNAMIC, ERRNUM, INCDAT, LUNSNC, RUN, SOIL,
     &  SRFC, YR, YRDOY, FROP

      PARAMETER (SRFC = 0, SOIL = 1)

      REAL CBALANCE, CBALANCE1, CUMRES, END_AND_OUTPUT, END_AND_OUTPUT1,
     & oldCUMRES, oldSENESSUMC, oriTLITCI_SOILSRFC, oriTSOMCI_SOILSRFC,
     & SENESSUMC, START_AND_INPUT, START_AND_INPUT1, TLITC,
     & TLITC_SOILSRFC, TLITCI_SOILSRFC, TSOMC, TSOMC_SOILSRFC,
     & TSOMCI_SOILSRFC

      REAL ACCCO2(0:1), LITC(0:NL), oldACCCO2(0:1), SOM1C(0:NL)


!-----------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.
      TYPE (ControlType)   CONTROL
      TYPE (SwitchType)    ISWITCH
      TYPE (OrgMatAppType) OMAData    !Organic matter application
      TYPE (ResidueType)   SENESCE

!     ------------------------------------------------------------------
      !Don't print unless C output requested.
      IDETC   = ISWITCH % IDETC
      IDETL   = ISWITCH % IDETL
      IF (IDETC == 'N' .OR. INDEX('AD',IDETL) < 1) RETURN

!     ------------------------------------------------------------------
!     Transfer values from constructed data types into local variables.
      DYNAMIC = CONTROL % DYNAMIC
      DAS     = CONTROL % DAS
      FROP    = CONTROL % FROP
      RUN     = CONTROL % RUN
      RNMODE  = CONTROL % RNMODE

!***********************************************************************
!***********************************************************************
!     SEASONAL INITIALIZATION
!***********************************************************************
      IF (DYNAMIC == SEASINIT) THEN
!***********************************************************************
        SCBAL = 'SoilCBal.OUT'
        CALL GETLUN ('SCBAL', LUNSNC)

        INQUIRE (FILE = SCBAL, EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT = LUNSNC, FILE = SCBAL, STATUS = 'OLD',
     &      IOSTAT = ERRNUM, POSITION = 'APPEND')
        ELSE
          OPEN (UNIT = LUNSNC, FILE = SCBAL, STATUS = 'NEW',
     &      IOSTAT = ERRNUM)
          WRITE (LUNSNC,'("*Soil Carbon Balance")')
        ENDIF

!       If the file can't be found, call an error.
        IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY, ERRNUM, 'SCBAL', 0)

!       Get initial values of organic matter placement and senescence
        CUMRES = OMAData % CumResWt
        SENESSUMC = 0.4 * SENESCE % CumResWt

!       Set the initial amount of carbon in SOM+litter at the beginning
!       of the new season, as a reference point for the seasonal balance.
        TSOMCI_SOILSRFC = TSOMC + SOM1C(SRFC)
        TLITCI_SOILSRFC = TLITC + LITC(SRFC)

        IF (INDEX ('QF', RNMODE) > 0 .AND. RUN == 1) THEN
!         Keep the values at the start of the sequential run.
          oriTSOMCI_SOILSRFC = TSOMCI_SOILSRFC
          oriTLITCI_SOILSRFC = TLITCI_SOILSRFC
          oldCUMRES       = 0.0
          oldSENESSUMC    = 0.0
          oldACCCO2(SRFC) = 0.0
          oldACCCO2(SOIL) = 0.0

        ELSE
!         Keep the values at the end of the previous season.
          oldCUMRES       = CUMRES
          oldSENESSUMC    = SENESSUMC
          oldACCCO2(SRFC) = ACCCO2(SRFC)
          oldACCCO2(SOIL) = ACCCO2(SOIL)
        ENDIF

!       Write headers for output file.
        CALL HEADER (SEASINIT, LUNSNC, RUN)

      IF (INDEX ('QF', RNMODE) <= 0) THEN
!       Header for single-season runs or seasonal runs.
        WRITE (LUNSNC, '(3A, /, 67X, A)') 
     &    '@YEAR DOY   DAS   TSOMLITC      TSOMC    TLITC  CUMRESC',
     &    '   SENESC   ACCCO2   ACCCO2   ACCCO2   START+IN',
     &    '    END+OUT CBALANCE', '(SRFC)   (SOIL)    (TOT)'

      ELSE
!       Header for sequential runs.
        WRITE (LUNSNC, 50) 
     &    ' THIS SEASON ONLY  ',
     &    ' COMPLETE MULTI-SEASON RUN (SEQUENTIAL RUNS ONLY) ',
     &    '@YEAR DOY   DAS   TSOMLITC      TSOMC    TLITC  CUMRESC',
     &    '   SENESC   ACCCO2   ACCCO2   ACCCO2   START+IN',
     &    '    END+OUT CBALANCE  CUMRESC   SENESC   ACCCO2   ACCCO2',
     &    '   ACCCO2   START+IN    END+OUT CBALANCE',
     &    '(SRFC)   (SOIL)    (TOT)', '(SRFC)   (SOIL)    (TOT)'
      ENDIF

50    FORMAT (48('.'), 27('='), A, 28('='), 3X, 12('='), A,
     &  12('='), /, 4A, /, 67X, A, 52X, A)

      ENDIF
!***********************************************************************
!***********************************************************************
!     DAILY CALCULATIONS 
!***********************************************************************
      IF (DYNAMIC == INTEGR .OR. DYNAMIC == SEASINIT) THEN
!***********************************************************************
!       Set today's amount of carbon in SOM+litter.
        TSOMC_SOILSRFC = TSOMC + SOM1C(SRFC)
        TLITC_SOILSRFC = TLITC + LITC(SRFC)

        CUMRES = OMAData % CumResWt
        SENESSUMC = 0.4 * SENESCE % CumResWt

!       ****************************************************************
!       Carbon balance of a single-season run, seasonal run.
!         Note that the balance is done per season, so by deducting the
!         oldXXXX variables, the amount at the end of the last season is 
!         deducted.
!       ****************************************************************
        START_AND_INPUT = TSOMCI_SOILSRFC + TLITCI_SOILSRFC + 
     &     0.4 * (CUMRES - oldCUMRES) + (SENESSUMC - oldSENESSUMC)

        END_AND_OUTPUT = TSOMC_SOILSRFC + TLITC_SOILSRFC + ACCCO2(SRFC)
     &     - oldACCCO2(SRFC) + ACCCO2(SOIL) - oldACCCO2(SOIL)

        CBALANCE = END_AND_OUTPUT - START_AND_INPUT

        IF (DYNAMIC == SEASINIT) THEN
          CALL YR_DOY(INCDAT(YRDOY,-1), YR, DOY)
        ELSE
          CALL YR_DOY(YRDOY, YR, DOY)
        ENDIF

!       For a sequential run, don't print here, as it will be done in
!       the next section.
        IF (INDEX ('QF', RNMODE) <= 0) THEN
!         Check for output frequency
          IF (MOD(DAS,FROP) == 0 .OR. DYNAMIC == SEASINIT) THEN  
            WRITE (LUNSNC,100) YR, DOY, DAS, 
     &      TSOMC_SOILSRFC + TLITC_SOILSRFC, TSOMC_SOILSRFC, 
     &      TLITC_SOILSRFC, 0.4 * CUMRES, SENESSUMC - oldSENESSUMC,
     &      ACCCO2(SRFC) - oldACCCO2(SRFC), 
     &      ACCCO2(SOIL) - oldACCCO2(SOIL),
     &      ACCCO2(SRFC) - oldACCCO2(SRFC) + ACCCO2(SOIL) -
     &      oldACCCO2(SOIL),
     &      START_AND_INPUT, END_AND_OUTPUT, CBALANCE

100         FORMAT (1X, I4, 1X, I3, 1X, I5, 2F11.2, 6F9.2, 2F11.2, F9.2)
          ENDIF

!       ******************************************************************
!       Carbon balance of the whole sequential run from day 0 onwards.
!       ******************************************************************
        ELSEIF (INDEX ('QF', RNMODE) > 0) THEN
          START_AND_INPUT1 = oriTSOMCI_SOILSRFC + oriTLITCI_SOILSRFC + 
     &       0.4 * (CUMRES) + (SENESSUMC)

          END_AND_OUTPUT1 = TSOMC_SOILSRFC + TLITC_SOILSRFC +
     &       ACCCO2(SRFC) + ACCCO2(SOIL)

          CBALANCE1 = END_AND_OUTPUT - START_AND_INPUT

!         Check for output frequency
          IF (MOD(DAS,FROP) == 0) THEN  !Daily output every FROP days,
            WRITE (LUNSNC,200) YR, DOY, DAS, 
!           Section for this season.
     &      TSOMC_SOILSRFC + TLITC_SOILSRFC, TSOMC_SOILSRFC, 
     &      TLITC_SOILSRFC,
     &      0.4 * (CUMRES - oldCUMRES),
     &      SENESSUMC - oldSENESSUMC,
     &      ACCCO2(SRFC) - oldACCCO2(SRFC), 
     &      ACCCO2(SOIL) - oldACCCO2(SOIL),
     &      ACCCO2(SRFC) - oldACCCO2(SRFC) +
     &        ACCCO2(SOIL) - oldACCCO2(SOIL),
     &      START_AND_INPUT, END_AND_OUTPUT, CBALANCE,
!           Section for the whole sequential run.
     &      0.4 * CUMRES, SENESSUMC, ACCCO2(SRFC), ACCCO2(SOIL),
     &      ACCCO2(SRFC) + ACCCO2(SOIL), START_AND_INPUT1, 
     &      END_AND_OUTPUT1, CBALANCE1

200         FORMAT (1X, I4, 1X, I3, 1X, I5, 2F11.2, 6F9.2, 2F11.2, 
     &      6F9.2, 2F11.2, F9.2)
          ENDIF
        ENDIF

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
      
!***********************************************************************
      RETURN
      END SUBROUTINE SOILCBAL_C
