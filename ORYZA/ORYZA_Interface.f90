!=======================================================================
!  ORYZA_Interface, Subroutine
!  DSSAT interface for ORYZA2000 rice growth routine.
!-----------------------------------------------------------------------
!  REVISION       HISTORY
!  01/26/2011 TL/CHP Written.
!  04/06/2012 CHP Added dedicated unit numbers for all output files.
!                 Open ORYZA log file here.
!                 CRP file can be in data or genotype directory  
!=======================================================================
      SUBROUTINE ORYZA_Interface (CONTROL, ISWITCH,               &    !Input
          EOP, FLOODWAT, HARVFRAC, NH4, NO3, SOILPROP,            &    !Input
          SomLitC, SomLitE,                                       &    !Input
          ST, SW, TRWUP, UPPM, WEATHER, YRPLT, YREND, OR_OUTPUT,  &    !Input
          CANHT, HARVRES, KTRANS, KSEVAP, MDATE, NSTRES, PORMIN,  &    !Output
          RLV, RWUMX, SENESCE, STGDOY, UNH4, UNO3, UH2O, XLAI)         !Output

      USE ModuleDefs
      USE FloodModule
      USE ModuleData
      USE Public_Module		!VARIABLES
      USE RootGrowth
      USE Interface_IpSoil

      IMPLICIT NONE
      SAVE

      CHARACTER*6  ERRKEY
      PARAMETER (ERRKEY = 'ORYZA ')

      CHARACTER*30  FILEIO
      CHARACTER*78  MSG(4)
      CHARACTER*120 FILEIOCS

      INTEGER DOY1, DYNAMIC, TN, RUNI, RN, ON
      INTEGER REP, STEP, CN, YRHAR, YREND, YRDOY
      INTEGER MDATE, L, NLAYR
      INTEGER SN, YEAR1
      INTEGER STGDOY(20), YRPLT
      INTEGER, PARAMETER :: NL_OR = 10

      REAL WUPT, EOP, ET, TRWUP
!      REAL KCAN, KEP
      REAL KTRANS, KSEVAP
      REAL NSTRES, XLAI, NFP
      REAL PORMIN, RWUMX
      REAL CANHT, TOTIR

      REAL, DIMENSION(NL) :: DS
      REAL, DIMENSION(2)  :: HARVFRAC

!     Soil water
      REAL, DIMENSION(NL) :: BD, ST, SW, WPkPa
      REAL, DIMENSION(NL_OR) :: SANDX, CLAYX, MSKPA

!     Soil N
      REAL, DIMENSION(NL) :: NH4, NO3, UPPM, UNO3, UNH4, UH2O, RLV
      REAL SomLitC(0:NL), SomLitE(0:NL,NELEM)
      REAL, DIMENSION(NL_OR) :: SNH4X, SNO3X, SOC, SON, SUREA
      REAL SNN0C, SNN1C, PRLIG, PSLIG

!-----Formal ORYZA parameters
      INTEGER       ITASK , IUNITD, IUNITL, CROPSTA, IDOY, TDATE
      INTEGER       IUNITD2, IUNITD3, IUNITDA
      LOGICAL       OR_OUTPUT, TERMNL
      CHARACTER (128) FILEI1, FILEIT, FILEI2
      CHARACTER (11) ESTAB
      REAL    YEAR, DOY , TIME, DELT  , LAT  , RDD, TRC, NFLV, NSLLV
      REAL    TMMN, TMMX, TKLT  , ZRTMS, LRSTRS, LDSTRS, LESTRS, NRT
      REAL    PCEW, CPEW, DAE , LAIROL, ZRT  , DVS, RNSTRS, WCL(10), WL0, DLDR
      REAL    LAI, LLV  , SLA , WLVG  , WST  , WSO, GSO, GGR, GST, GRT, GLV, PLTR 
      REAL    TRW, TRWL(10), TKL(10), WRT, WRR14
      REAL    HU, WAGT, WLVD, WRR, NGR, PLOWPAN, TNSOIL, NACR
      REAL    ANSO, ANLV, ANST, ANLD, ANRT
      REAL    LAPE, DVSI, WLVGI, WSTI, WRTI, WSOI, ZRTI

!     FOR EXPERIMENT FILE 
      INTEGER YRSIM, EDATE, iPAGE, IRRCOD 
      CHARACTER*1, ISWWAT, ISWNIT, PLME,IIRRI
      REAL PLPH, PLTPOP, PLANTS, PLDP,IRRI, WL0MIN, KPAMIN, WLODAY, SLMIN
      REAL IRMTAB(300), RIRRIT(750),ISTAGET(900), TMCTB(750)

!     FILEIO data
      CHARACTER*1  PLDS
      INTEGER INCDAT
      REAL PAGE, ROWSPC, SDWTPL, ATEMP

!     Output data needed for DSSAT output files
      CHARACTER*10 STNAME(20) 
      INTEGER ISDATE, ISTAGE 

      TYPE (ControlType) CONTROL
      TYPE (SoilType)    SOILPROP
      TYPE (SwitchType)  ISWITCH
      Type (ResidueType) HARVRES
      Type (ResidueType) SENESCE
      TYPE (WeatherType) WEATHER
      TYPE (FloodWatType)FLOODWAT
      
      COMMON /FSECM1/ YEAR,DOY,IUNITD,IUNITL,TERMNL
      
      DYNAMIC = CONTROL % DYNAMIC
      YRDOY   = CONTROL % YRDOY
      CALL YR_DOY(YRDOY, YEAR1, DOY1)

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per planting season
!***********************************************************************
      IF (DYNAMIC == SEASINIT) THEN

        ITASK = 1
        TIME =0.0
        TERMNL = .FALSE.
        ALLOCATE(pv)                              !Added by TaoLi, 24 April 2011
        Call SET_ZERO_PV
!       ORYZA does its own root water uptake - how to handle?
!       Variables required by DSSAT for root water uptake calculations
        PORMIN = 0.0  !Minimum pore space required for supplying oxygen to roots for 
!                      optimal growth and function (cm3/cm3)
        RWUMX  = 0.03 !Maximum water uptake per unit root length, constrained by soil 
!                      water (cm3[water] / cm [root])
        RLV = 0.0

        FILEIO  = CONTROL % FILEIO
        YRSIM   = CONTROL % YRSIM
        ISWWAT  = ISWITCH % ISWWAT
        ISWNIT  = ISWITCH % ISWNIT
        LAT     = WEATHER % XLAT
      
        DS     = SOILPROP % DS    
        NLAYR  = SOILPROP % NLAYR  

        IF (NLAYR > NL_OR) THEN
          MSG(1) = "Too many layers for ORYZA2000 model."
          WRITE(MSG(2),'("Number of layers = ",I3)') NLAYR
          WRITE(MSG(3),'("Max # of layers  = ",I3)') NL_OR
          MSG(4) = "Program will stop."
          CALL WARNING(4,ERRKEY,MSG)
          CALL ERROR(ERRKEY,75," ",0)
        ENDIF

        CALL WaterPotential(SW, SOILPROP, FLOODWAT % FLOOD,  &    !Input
          WPkPa)                                !Output
        CALL PotentialOp(CONTROL, ISWITCH, SOILPROP, WPkPa)

!       Transfer from DSSAT SOILPROP variables to ORYZA variables, by layer
        PV % PNL = NLAYR
        DO L = 1, NLAYR
          BD(L)     = SOILPROP % BD(L)    
          SANDX(L)  = SOILPROP % SAND(L) / 100.
          CLAYX(L)  = SOILPROP % CLAY(L) / 100.

!         These are only used for a seasonal initialization, not needed daily
          SOC(L)    = SomLitC(L)    !Soil organic C (kg/ha)
          SON(L)    = SomLitE(L,1)  !Soil organic N (kg/ha)

!         ORYZA public variables:
          PV%PDLAYER(L) = SOILPROP % DLAYR(L)*10.0       !CONVERT LAYER THICKNESS FROM cm TO mm
          TKL(L) = PV%PDLAYER(L)/1000.0      !CONVERT SOIL LAYER THICKNESS FROM mm TO m
          PV%PWCST(L) = SOILPROP % SAT(L)
          PV%PWCFC(L) = SOILPROP % DUL(L)
          PV%PWCWP(L) = SOILPROP % LL(L)

!         These will also be transferred daily
          PV % PNO3(L) = NO3(L) / SOILPROP % KG2PPM(L) !Convert from ppm to kg/ha
          PV % PNH4(L) = NH4(L) / SOILPROP % KG2PPM(L) !Convert from ppm to kg/ha
          SUREA(L) = UPPM(L) / SOILPROP % KG2PPM(L)
          SNH4X(L) = PV % PNH4(L)
          SNO3X(L) = PV % PNO3(L)
          MSKPA(L) = WPkPa(L)
          WCL(L) = SW(L)
          pv%PSWC = WCL(L)
        END DO

        FILEIOCS(1:30) = FILEIO
        TN = 0
        RN = 0
        SN = 0
        ON = 0
        CN = 0  !Crop component
        REP = 1
        STEP = 1
        RUNI = 1
        TOTIR = 0.0

        HU = 0.0
        WAGT = 0.0
        WLVD = 0.0
        WLVG = 0.0
        WST = 0.0
        WSO = 0.0
        WRT = 0.0
        WRR14 = 0.0
        NGR = 0.0
        GSO = 0.0
        GGR = 0.0
        GST = 0.0
        GLV = 0.0
        PLTR = 0.0
        LLV = 0.0
        WRR = 0.0
        SLA = 0.0
        LAI = 0.0
        LAIROL = 0.0
        ZRT = 0.0
        DVS = 0.0
        DLDR =0.0
        TRC = 0.0
        ANSO = 0.0
        ANLV = 0.0
        ANST = 0.0
        ANLD = 0.0
        ANRT = 0.0
        CROPSTA = 0

!       Water & N stresses
        LRSTRS = 1.0
        LDSTRS = 1.0
        LESTRS = 1.0
        PCEW   = 1.0
        CPEW   = 1.0
        RNSTRS = 1.0

        CANHT = 0.0   !Canopy height
        STGDOY= 9999999   !Dates for developement stages

!        KCAN  = 0.85  !Canopy light extinction coef
!        KEP   = 1.0   !Energy extinction coef
        KSEVAP = 0.85
        KTRANS = 1.0  
!        KSEVAP = -99.
!        KTRANS = 0.5  

!       Depth to plowpan (m)
        PLOWPAN = FLOODWAT % PLOWPAN
        IF (PLOWPAN < .01) THEN
!         No plowpan specified - check for WR values close to zero
!         PLOWPAN = (DS(NLAYR) + 10.)/100.0   !converted into m
          DO L = 2, NLAYR
            IF (SOILPROP % WR(L) < 0.001) THEN
              PLOWPAN = DS(L-1) /100.0    !converted into m
              EXIT
            ENDIF
          ENDDO
        ENDIF

        WL0  = FLOODWAT % FLOOD

!       Read DSSAT cultivar and planting data from FILEIO
        CALL OR_IPRICE (CONTROL,                       &
          FILEI1, PLANTS, PLTPOP, PLME, PLDS,      &
          ROWSPC, PLDP, SDWTPL, PAGE, ATEMP, PLPH, &
          LAPE, DVSI, WLVGI, WSTI, WRTI, WSOI, ZRTI, &
          STGDOY, STNAME) 
 
        STGDOY(14) = YRDOY !start of simulation
        ISTAGE = 14

!       Lignin content of senesced root and surface matter - read from file
        CALL IPSOIL (CONTROL, CROP='RI',               &  !Input
          PRLIG=PRLIG, PSLIG=PSLIG)

!       Cumulative senesced N to surface and soil
        SNN0C = 0.0  ; SNN1C = 0.0
        DO L=1, NLAYR
         pv%PResC(L,1) = 0.0
         PV%PResN(L,1) = 0.0
        enddo
!----------------------------------------------------------------
!       ORYZA initialization section - moved up to initialization section
        !initialize OBSSYS routine
        CALL OBSINI

        !get emergence date and transplanting date
        TDATE = YRPLT
        IF(INDEX(PLME,"T").GT.0) THEN
!           By definition, DSSAT YRPLT = transplant date.  We need to keep this definition
!           for the FILEX data for compatibility with other models.  But we need to start calling
!           ORYZA on the sowing date, because the model calculates growth in the seed bed.
!           So - for transplants, re-set YRPLT = sowing date so that ORYZA is called prior
!           to transplant date.
            iPAGE = NINT(PAGE)
            EDATE = INCDAT(YRPLT,-iPAGE)
            YRPLT = EDATE  
        ELSE
            EDATE = YRPLT
        END IF

!       Used STRING
!       ESSENTIAL INFORMATION MUST BE PROVIDED FROM UPPER LAYER
!       FILEI1 = 'D:\...\...\IR72.CRP
!       FILEIT = 'D:\...\...\...\N150.exp
        FILEIT = CONTROL % FILEX(1:8) // ".EXP"
        FILEI2 = CONTROL % FILEX(1:8) // ".SOL"
        IIRRI  = ISWITCH % IIRRI
!       IRCOD  = 0  !Potential production  - need to get value for water limited. 
        !THE 'IRCOD', IT IS NOT USED IN THE ROUTINE YET, COMMENT IT OUT TEMPORARY.

        !GENERATE EXPERIMENT AND SOIL FILES
        CALL UPPERC(ISWWAT)
        CALL UPPERC(ISWNIT)
        CALL ExperimentFileEdit(FILEIT, YRSIM, EDATE,& 
                ISWWAT, ISWNIT, PLME, iPAGE, PLPH, PLTPOP, PLANTS, PLANTS, PLDP, &
                IIRRI, IRRCOD, IRMTAB, RIRRIT, IRRI, WL0MIN, KPAMIN, SLMIN, WLODAY, ISTAGET, TMCTB, &
                LAPE, DVSI, WLVGI, WSTI, WRTI, WSOI, ZRTI)

        TERMNL = .FALSE.
        IF ((ISWNIT == 'Y').OR.(ISWWAT == 'Y')) THEN
            PV%PROOT_NUTRIENT = .TRUE.
            CALL SOILFILEEDIT(FILEI2, NLAYR, TKL, SANDX, CLAYX, BD, SOC, SON, SNH4X, SNO3X, SUREA, PLOWPAN)
        ELSE
            PV%PROOT_NUTRIENT = .FALSE.; FILEI2=""
        END IF

        IF(INDEX(ISWWAT,"N")>0) THEN
            TRW =TRC
        ENDIF

        CALL GETLUN("ORYZA1",IUNITD)
        CALL GETLUN("ORYZA1A",IUNITDA)  !This is because ORYZA routines randomly add 1 to the unit number
        CALL GETLUN("ORYZA2",IUNITL)
        CALL GETLUN("ORYZA3",IUNITD2)
        CALL GETLUN("ORYZA4",IUNITD3)

!        IUNITD = IUNITD+10
!       IUNITL = IUNITL+20
        
        DELT = 1.0  !TIME STEP IS 1.0
        IDOY = INT(DOY)
        DAE = 0.0        
        
!       open a temporary file for ORYZA2000 outputs
        OPEN(UNIT = IUNITD2, FILE = "ORYZA_RES.DAT")
        OPEN(UNIT = IUNITD3, FILE = "ORYZA_CLI.DAT")
        WRITE(IUNITD2,'(A)') "DOY,DAE,DVS,ZRT,LAI,LLV,WLVD,WLVG,WST,WSO,WRR14,WRT,GSO,GGR,GST,GLV,WAGT" 

        OPEN(UNIT = IUNITL, FILE = "ORYZA_LOG.OUT")

!***********************************************************************
!***********************************************************************
!     Rate - daily
!***********************************************************************
      ELSEIF (DYNAMIC == RATE) THEN

        ITASK = 2

!       Transfer DSSAT variables into ORYZA variables
        RDD   = WEATHER % SRAD*1000000.0
        TMMX  = WEATHER % TMAX
        TMMN  = WEATHER % TMIN
        TRC   = EOP

        WL0  = FLOODWAT % FLOOD  !the surface water depth in mm
        pv%pwl0 = wl0

        CALL WaterPotential(SW, SOILPROP, FLOODWAT % FLOOD, WPkPa)

        DO L = 1, NLAYR
          WCL(L) = SW(L)      !Soil water content (mm3/mm3)
          pv%Pswc(L)   = SW(L)      !Soil water content (mm3/mm3)
          pv%pSOILTx(L) = ST(L)      !Soil temperature (oC)
          PV % PNO3(L) = NO3(L) / SOILPROP % KG2PPM(L) !NO3 (kg/ha)
          PV % PNH4(L) = NH4(L) / SOILPROP % KG2PPM(L) !NH4 (kg/ha)
          SNH4X(L) = PV % PNH4(L)
          SNO3X(L) = PV % PNO3(L)
          MSKPA(L) = WPkPa(L)
        ENDDO

!-----  Set CROPSTA: 0=before sowing; 1=day of sowing; 2=in seedbed;
!                  3=day of transplanting; 4=main growth period

!       YRPLT = sowing date for direct seed 
        IF (YRDOY == YRPLT) THEN
          STGDOY(7) = YRDOY
          ISTAGE = 7
        ENDIF

        IF(YRDOY.LT.EDATE) THEN
           CROPSTA = 0             
        END IF

        IF (CROPSTA .EQ. 3) CROPSTA = 4

        IF (CROPSTA .EQ. 2) THEN
           IF (YRDOY.EQ.TDATE) THEN
!             Transplant
              CROPSTA = 3
              STGDOY(11) = YRDOY
              ISTAGE = 11
           ENDIF
        END IF

        IF (CROPSTA .EQ. 1) THEN
           IF (INDEX(PLME,"T").GT.0) THEN
              CROPSTA = 2
           ELSE 
              CROPSTA = 4
           END IF
        END IF

        IF (CROPSTA .EQ. 0) THEN
           IF (YRDOY.EQ.EDATE) THEN
!              Emergence
               CROPSTA = 1
               STGDOY(9) = YRDOY
               ISTAGE = 9
            ENDIF
        END IF

        DOY = REAL(DOY1);YEAR = real(YEAR1); IDOY = DOY1
        CALL GET('SPAM','ET',  ET)
        CALL Get('MGMT','TOTIR', TOTIR)

!***********************************************************************
!***********************************************************************
!     Integration - daily
!***********************************************************************
     ELSEIF (DYNAMIC == INTEGR) THEN

        ITASK = 3
        TIME = TIME+DELT

        YRHAR = YREND
        WUPT  = TRWUP


!***********************************************************************
!***********************************************************************
!     Daily or seasonal output
!***********************************************************************
      ELSE

        ITASK = 0
        CALL PotentialOp(CONTROL, ISWITCH, SOILPROP, WPkPa)

        IF (DYNAMIC == SEASEND) THEN
          STGDOY(20) = YRDOY
          ISTAGE = 20
          YREND = YRDOY
          TERMNL = .TRUE.
          DEALLOCATE(PV) 
          CALL RDDTMP (IUNITD)  !delete all temporary files
          CLOSE(IUNITD2)
          CLOSE(IUNITD3)
        ENDIF

      ENDIF

!***********************************************************************
!***********************************************************************
    IF(.NOT.TERMNL .AND. DYNAMIC > RUNINIT) THEN
        IF(INDEX(ISWWAT,"Y").GT.0) THEN

            CALL WSTRESS2 (ITASK,  DELT,   OR_OUTPUT, IUNITD, IUNITL, FILEI1, FILEIT, &
                          TRC,    ZRT,    TKL,    NLAYR,    CROPSTA, &
                          WCL,    pv%PWCWP,   MSKPA, &
                          TRW,    TRWL,   LRSTRS, LDSTRS, LESTRS, PCEW, CPEW)
        !       Check for potential production condition  
        ELSEIF(INDEX(ISWWAT, "N").GT.0) THEN              !POTENTIAL WATER CONDITION
            TRC = EOP; TRW = TRC; TKLT = SUM(TKL); ZRTMS = TKLT   !THE TOTAL TRANSPIRATION EQUALS TO POTENTIAL TRANSPIRATION
            CALL WNOSTRESS (NLAYR, TRW, TRWL, ZRT, TKL, LRSTRS, LDSTRS, LESTRS, PCEW, CPEW)
        END IF

!   TRC = EOP
!   ZRT         Root depth (cm)
!   TKL         Soil thickness (m)
!   pv%PWCWP    Wilting point (mm3/mm3)
!   MSKPA       Water potential (kPa)
!   TRW         Actual transpiration rate (output from ORYZA) (mm/d)
!   TRWL        Actual transpiration rate layers (mm/d) (Output from ORYZA)
!   

        CALL ORYZA1(ITASK,  IUNITD, IUNITL, FILEI1, FILEI2,FILEIT, &
                        OR_OUTPUT, TERMNL, IDOY  , DOY, &
                        TIME,   DELT,   LAT,    RDD,    TMMN,   TMMX, &
                        NFLV,   NSLLV,  NRT,	RNSTRS,                 &
                        ESTAB,  TKLT,   ZRTMS,  CROPSTA, &
                        LRSTRS, LDSTRS, LESTRS, PCEW,  CPEW, TRC, &
                        DAE,    SLA, LAI,    LAIROL, ZRT,    DVS, &
                        LLV,    DLDR, WLVG, WST, WSO, GSO, GGR, GST, GLV, GRT, &
                        PLTR, WCL, WL0, WRT, WRR14, NGR, HU)

        IF(INDEX(ISWNIT, "N").GT.0) THEN           !POTENTIAL NITROGEN CONDITION
            CALL NNOSTRESS2(DELT, IUNITD, IUNITL, ITASK, FILEI1, FILEIT, &
                           CROPSTA, DVS, WLVG, LAI, SLA, NFLV, NSLLV, RNSTRS)
        ELSE
            CALL NCROP3 (ITASK, IUNITD, IUNITL, FILEI1, FILEI2, FILEIT, DELT, TIME, OR_OUTPUT, &
                        TERMNL, DVS, LLV, DLDR, WLVG, WST, WSO, GSO, GST, GLV, GRT, &
                        PLTR, LAI, SLA, CROPSTA, TNSOIL, NACR, ANSO, ANLV, ANST, ANLD, &
                       ANRT, NFLV, NSLLV,NRT, RNSTRS)
        END IF

      IF (ITASK == 2) THEN
        DO L = 1, NLAYR
          UNH4(L) = SNH4X(L) - PV % PNH4(L)    !NH4 uptake (kg/ha)
          UNO3(L) = SNO3X(L) - PV % PNO3(L)    !NO3 uptake (kg/ha)
          UH2O(L) = TRWL(L)                    !H2O uptake (mm/d)
        ENDDO 
!       NSTRES = NFP
        NSTRES = RNSTRS

      ELSEIF (ITASK == 3) THEN
        XLAI   = LAI
        WLVD = WLVD+ (DLDR+LLV)*DELT        
        WAGT = WST + WLVG + WSO + WLVD
        WRR  = WRR14 * 0.86

!       Add senesced roots to soil fresh organic matter pools
!       pv%presC(L,1) = roots (for SENESCE and HARVRES)
!       PV%PResN(L,1) =  N in roots at harvest (for SENESCE and HARVRES)
        DO L=1, NLAYR
          SENESCE % ResWt(L)  = pv%PResC(L,1) / 0.40
          SENESCE % ResLig(L) = SENESCE % ResWt(L) * PRLIG
          SENESCE % ResE(L,1) = PV%PResN(L,1)
          SNN1C = SNN1C + SENESCE % ResWt(L)
          RLV(L) = pv%prootden(L) !* 1.E-10
        ENDDO

!       Temporary
        WRITE(IUNITD2,5000) DOY,DAE,DVS,ZRT,LAI,LLV,WLVD, WLVG, WST, WSO, WRR14, WRT,&
                            GSO, GGR, GST, GLV,WAGT
        WRITE(IUNITD3,6000) 1,YEAR, DOY, RDD/1000.0, TMMN, TMMX, -99.0, -99.0           
        
        IF (DVS >= 0.65 .AND. STGDOY(2) > YRDOY) THEN
!         Panicle initiation date DVS = 0.65
          STGDOY(2) = YRDOY
          ISTAGE = 2
        ELSEIF (DVS >= 1.0 .AND. STGDOY(3) > YRDOY) THEN
!         Anthesis date when DVS = 1.0
          STGDOY(3) = YRDOY
          ISTAGE = 3
          ISDATE = YRDOY
        ELSEIF (TERMNL.AND. STGDOY(6) > YRDOY) THEN
!         Maturity date when DVS =2.0
          STGDOY(6) = YRDOY
          ISTAGE = 6
          MDATE = YRDOY
        ENDIF 
      
      ENDIF
    ENDIF

    IF (TERMNL .AND. ITASK > 0) THEN
      MDATE = YRDOY
      YREND = YRDOY
      CALL RDDTMP (IUNITD)

      IF (DYNAMIC == INTEGR) THEN 
!       Transfer harvest residue from senescence variable to 
!       harvest residue variable on day of harvest.
!       This includes all leftover root mass on day of harvest
        HARVRES = SENESCE
        SENESCE % ResWt  = 0.0
        SENESCE % ResLig = 0.0
        SENESCE % ResE   = 0.0

!       For surface residues, use HARVFRAC, if available
        IF (HARVFRAC(2) < 1.E-6) HARVFRAC(2) = 0.10
        HARVRES % ResWt(0)  = (WLVG + WLVD + WST) * HARVFRAC(2)
        HARVRES % ResLig(0) = HARVRES % ResWt(0) * PSLIG
        HARVRES % ResE(0,1) = (ANLV + ANLD + ANST) * HARVFRAC(2)

        SNN0C = HARVRES % ResWt(0)

!       wlvg = total leaf biomass - default 10% left in field (kg/ha)
!       wlvd = dead leaf biomass - default 10% left in field (kg/ha)
!       wst  = stem - default 10% left in field (kg/ha)
!       ANSO= N in storage + grain (kg[N]/ha) (not used here)
!       ANLV = N in leaf (kg[N]/ha)
!       ANST = N in stem (kg[N]/ha)
!       ANLD = N in dead leaf (kg[N]/ha) (only for W limited)

!       *** NEED TO HANDLE HERE?
!       for potential production, all N levels are at optimum - read from crop file
      ENDIF
    ENDIF

    SELECT CASE(DYNAMIC)
    CASE(RUNINIT, SEASINIT, OUTPUT, SEASEND)
      CALL OR_OPGROW (CONTROL, ISWITCH, SOILPROP,      &
         CPEW, DVS, HU, LAI, LDSTRS, LESTRS, LRSTRS,   &
         NFLV, NGR, RNSTRS, PCEW, RDCL,                &
         WAGT, WLVD, WLVG, WRR, WRT, WSO, WST, YRPLT, ZRT, &
         NACR, ANRT, ANLV, ANSO, ANST, ANLD, SNN1C)

      CALL OR_OPHARV (CONTROL, ISWITCH,                 &
         HARVFRAC, ISDATE, ISTAGE, LAI, LESTRS, MDATE,  & !Input
         NGR, NSLLV, PCEW, STGDOY, STNAME,              & !Input
         WAGT, WLVD, WLVG, WRR, WSO, WST, YRPLT)          !Input
    END SELECT

!-----------------------------------------------------------------------
5000 FORMAT(2(F5.0,","),(F7.4,","),13(F12.3,","),F12.3)
6000 FORMAT(1(I3,","),2(F5.0,","),(F12.3,","),4(F12.3,","),F12.3)
      RETURN
      END SUBROUTINE ORYZA_Interface

!=====================================================================

!=======================================================================
!  RI_IPGROSUB, Subroutine
!
!  Reads FILEIO for ORYZA_Interface routine
!  Reads new biomass initialization file for initial biomass
!   Name: aabbyyxx.crI, where aabbyyxx.cr = same as FILEX name.
!  05/07/2002 CHP Written
!  08/12/2003 CHP Added I/O error checking
!  12/05/2011 CHP added biomass initialization file
!=======================================================================
      SUBROUTINE OR_IPRICE (CONTROL,               &
          FILEI1, PLANTS, PLTPOP, PLME, PLDS,      &
          ROWSPC, PLDP, SDWTPL, PAGE, ATEMP, PLPH, &
          LAPE, DVSI, WLVGI, WSTI, WRTI, WSOI, ZRTI, &
          STGDOY, STNAME) 

      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT     NONE

      CHARACTER*1   PLME, PLDS
      CHARACTER*2   CROP
      CHARACTER*6   VARTY
      CHARACTER*10  STNAME(20) 
      CHARACTER*12  FILEIB, FILEX   
      CHARACTER*20  VARNAME
      CHARACTER*30  FILEIO
      CHARACTER*78  MSG(2)
      CHARACTER*80  PATHEX, PATH_GENO
      CHARACTER*92  PATHIB
      CHARACTER*128 FILEI1, CHAR, F1_SAVE

      CHARACTER*7  ERRKEY, SECTION
      PARAMETER (ERRKEY = 'IPORICE')

      INTEGER ISECT, LINC, LNUM, LUNIO, ERR, FOUND, STGDOY(20), TRT
      REAL PLANTS, PLTPOP, ROWSPC, PLDP, SDWTPL, PAGE, ATEMP, PLPH
      REAL LAPE, DVSI, WLVGI, WSTI, WRTI, WSOI, ZRTI 

      LOGICAL GoodFile, FEXIST

!     The variable "CONTROL" is of type "ControlType".
      TYPE (ControlType) CONTROL
      FILEIO = CONTROL % FILEIO
      LUNIO  = CONTROL % LUNIO

      GoodFile = .TRUE.
      LNUM = 0

!-----------------------------------------------------------------------
!       Read data from FILEIO for use in PLANT module
!-----------------------------------------------------------------------
      OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,0)

!-----------------------------------------------------------------------
!    Read name of FILEX - to get name of intial biomass file (if present)
!    Read path for genotype file (for ORYZA CRP file location, if not in
!       experiment path).
!-----------------------------------------------------------------------
      READ (LUNIO,'(3(/),15X,A12,1X,A80)',IOSTAT=ERR) FILEX, PATHEX
      LNUM = LNUM + 4
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)

      READ (LUNIO,'(4(/),28X,A)') PATH_GENO
      LNUM = LNUM + 5
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)

!-----------------------------------------------------------------------
!    Read Planting Details Section
!-----------------------------------------------------------------------
!P   PDATE   EDATE  PPOP  PPOE  PLME  PLDS  PLRS  PLRD  PLDP  PLWT  PAGE  PENV  PLPH  SPRL
!  1992195     -99 999.0 125.0     T     H   20.    0.   5.0    0.   12.  25.0   5.0   0.0

      SECTION = '*PLANT'
      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) CALL ERROR(SECTION, 42, FILEIO, LNUM)
      READ (LUNIO,70, IOSTAT=ERR) PLANTS, PLTPOP, PLME, PLDS, ROWSPC, PLDP, SDWTPL, PAGE, ATEMP, PLPH
   70 FORMAT (18X,2F6.0,2(5X,A1),F6.0,6X,5F6.0)
      LNUM = LNUM + 1
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)

!-----------------------------------------------------------------------
!    Read Cultivar parameter section
!-----------------------------------------------------------------------
!     Look for 2nd cultivar section in FILEIO
      SECTION = '*CULTI'
      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) CALL ERROR(SECTION, 42, FILEIO, LNUM)
      READ(LUNIO,'(A6,1X,A16,1X,A128)', IOSTAT=ERR) VARTY, VARNAME, FILEI1
      LNUM = LNUM + 1
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)

      CLOSE (LUNIO)

!-----------------------------------------------------------------------
!    Check for location of ORYZA crop file
!    If not in the current directory, look in the genotype directory
!-----------------------------------------------------------------------
    INQUIRE (FILE = FILEI1,EXIST = FEXIST)
    F1_SAVE = FILEI1
    IF (.NOT. FEXIST) THEN
      FILEI1 = TRIM(PATH_GENO) // FILEI1
      INQUIRE (FILE = FILEI1,EXIST = FEXIST)
      IF (.NOT. FEXIST) THEN
        MSG(1) = "ORYZA crop file does not exist."
        WRITE(MSG(2),'(A)') FILEI1(1:78)
        CALL WARNING(2,ERRKEY,MSG)
        CALL ERROR(ERRKEY,29,TRIM(F1_SAVE),0)
      ENDIF
    ENDIF

!-----------------------------------------------------------------------
!   Find and read initial biomass file
    FILEIB = FILEX(1:11) // "I"
    PathIB = TRIM(PATHEX) // FILEIB
    INQUIRE (FILE = PATHIB, EXIST = GoodFile)

    OPEN (LUNIO, FILE = PathIB, STATUS = 'OLD', IOSTAT=ERR)
    IF (ERR .NE. 0) GoodFile = .FALSE.

    IF (GoodFile) THEN
      SECTION = '*EXP. DATA (I)'
      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) GoodFile = .FALSE.
    ENDIF

    DO WHILE (GoodFile)
      CALL IGNORE (LUNIO,LNUM,ISECT,CHAR)
      IF (ISECT .NE. 1) THEN
        GoodFile = .FALSE.; EXIT
      ENDIF

      READ(CHAR,'(I2,7F8.0)', IOSTAT=ERR) &
        TRT, LAPE, DVSI, WLVGI, WSTI, WRTI, WSOI, ZRTI 
      IF (ERR .NE. 0) THEN
        GoodFile = .FALSE.; EXIT
      ENDIF

      IF (TRT == CONTROL % TRTNUM) EXIT
    ENDDO

!   No valid initial biomass file - use default values depending
!       on planting method
    IF (.NOT. GoodFile) THEN
      SELECT CASE (PLME)
      CASE ('S')
        LAPE   = 0.00005    ! Initial leaf area per plant"
        DVSI   = 0.0        ! Initial development stage"
        WLVGI  = 0.0        ! Initial leaf weight"
        WSTI   = 0.0        ! Initial stem weight"
        WRTI   = 0.0        ! Initial stem weight"
        WSOI   = 0.0        ! Initial weight storage organs"
        ZRTI   = 0.0001     ! Initial root depth (m)"
      CASE DEFAULT
        LAPE   = 0.0001     ! Initial leaf area per plant"
        DVSI   = 0.0        ! Initial development stage"
        WLVGI  = 0.0        ! Initial leaf weight"
        WSTI   = 0.0        ! Initial stem weight"
        WRTI   = 0.0        ! Initial stem weight"
        WSOI   = 0.0        ! Initial weight storage organs"
        ZRTI   = 0.0001     ! Initial root depth (m)"
      END SELECT
    ENDIF

!-----------------------------------------------------------------------
      ROWSPC = ROWSPC / 100.0

!     ORYZA life cycle:
!      DVS  Stage
!     0.00  Emergence 
!     0.40  Start of photoperiod sensitive phase
!     0.65  Panicle initiation
!     1.00  50% flowering
!     2.00  Physiological maturity

!     Stages from CERES-Rice:  (X indicates that stages are transferred to DSSAT
      STNAME(1)  = 'End Juveni'
      STNAME(2)  = 'Pan Init  '  !DVS = 0.65  !X
      STNAME(3)  = 'Heading   '  !DVS = 1.0
      STNAME(4)  = 'Beg Gr Fil'
      STNAME(5)  = 'End Mn Fil'
      STNAME(6)  = 'Maturity  '  !DVS = 2.0  !X
      STNAME(7)  = 'Sowing    '  !
      STNAME(8)  = 'Germinate '
      STNAME(9)  = 'Emergence '  !DVS = 0.0  !X
      STNAME(10) = 'Prgerm Sow'
      STNAME(11) = 'Transplant'  !X
      STNAME(12) = 'End Ti Fil'
      STNAME(13) = '          '
      STNAME(14) = 'Start Sim '  !X
      STNAME(15) = '          '
      STNAME(16) = '          '
      STNAME(17) = '          '
      STNAME(18) = '          '
      STNAME(19) = '          '
      STNAME(20) = 'Harvest   '

      STGDOY     = 9999999  

       RETURN
       END SUBROUTINE OR_IPRICE
 !=======================================================================
SUBROUTINE SET_ZERO_PV

	  USE PUBLIC_MODULE
	
		pv%CRUN=0;				pv%PROOT_NUTRIENT=.FALSE.
		pv%pond_active ='NO';	pv%PYear=0;pv%Pdoy=0
		pv%Pdae=0;				pv%Pdat=0
		pv%Pno3=0.0;			pv%Pnh4=0.0
		pv%Purea=0.0;			pv%pond_no3=0.0
		pv%pond_nh4=0.0;		pv%pond_urea=0.0
		pv%Psoc=0.0;			pv%Pson=0.0
		pv%pph=7.0;				pv%PFOM_type=0.0
		pv%PFOM_C=0.0;			pv%PFOM_N=0.0
		pv%Pnl=0;				pv%Pdlayer=0.0
		pv%Pbd=0.0;				pv%Psand=0.0
		pv%Pclay=0.0;			pv%Pkst=0.0
		pv%Pwcst=0.0;			pv%Pwcfc=0.0
		pv%Pwcwp=0.0;			pv%Pwcad=0.0
		pv%PplowDepth=0.0;		pv%psoiltx=0.0
		pv%Pwl0=0.0;			pv%Pswc=0.0
		pv%Pwflux=0.0;			pv%PTRWL=0.0
		pv%Prunoff=0.0;			pv%Pdrain=0.0
		pv%Pirrig=0.0;			pv%Plai=0.001
		pv%PResNum=0;			pv%PResName=''
		pv%PResType='';			pv%PResC=0.0
		pv%PResN=0.0;			pv%dlt_res_c_biom=0.0
		pv%dlt_res_c_hum=0.0;	pv%ProotD=0.0
		pv%ProotDen=0.0;		pv%PSROOTL=0.0
		pv%PRMINT=0.0;			pv%PROPTT=0.0
		pv%PRTBS=0.0;			pv%PRCNL=0.0
		pv%PMAXD=0.0;			pv%PSODT=0.0
		pv%PmaxT=0.0;			pv%PminT=0.0
		pv%PRad=0.0;			pv%PRain=0.0
		pv%PPressur=0.0;		pv%Pwind=0.0
		pv%PETp=0.0;			pv%PETa=0.0
		pv%Pevap=0.0;			pv%Ptrans=0.0
		pv%PDt=0.0;				pv%PRdn=0.0;		
	  
END SUBROUTINE SET_ZERO_PV
