!=======================================================================
!  CROPSIM-CERES CEREAL GROWTH AND DEVELOPMENT MODULE  Version 010115
!
!  Last edit 050415  Changes to the way Gencalc runs handled 
!
!  Developed from Ceres3.5 and Cropsim. 

!  Ceres was modified to fit within the frameworks of Cropsim and CSM, 
!  and to conform to a number of the concepts that were used in the 
!  construction of Cropsim and CSM ... completion of all rate 
!  calculations before state variable updating, no embedding of 
!  variables in the code, modules to read their own inputs and to 
!  generate their own outputs,etc.. In so doing, a number of  formulae 
!  in the original Ceres with embedded coefficients were simplified and 
!  the coefficients placed in external 'coefficient' files (cultivar,
!  ecotype, or species), and some concepts (eg.how vernalization and 
!  senescence are dealt with) and tools from Cropsim were used. This 
!  modified model should thus not be considered as Ceres, but as a 
!  derivative of Ceres best referred to as Cropsim-Ceres rather Ceres 
!  per se.. 
!
!  The module is used within both the Cropsim and CSM models.
! 
!  2023-01-26 CHP Reduce compile warnings: add EXTERNAL stmts, remove 
!                 unused variables, shorten lines.                    
!=======================================================================

      ! For Cropsim
!     SUBROUTINE CSCER (FILEIOIN, RUN, TN, RN,             !Command line
!    & ISWWAT, ISWNIT, IDETO, IDETG, IDETL, FROP,          !Controls
!    & SN, ON, RUNI, REP, YEAR, DOY, STEP, CN,             !Run+loop
!    & SRAD, TMAX, TMIN, CO2, RAIN, DEWDUR,                !Weather
!    & DAYLT, WINDSP, ST, EO,                              !Weather
!    & NLAYR, DLAYR, DEPMAX, LL, DUL, SAT, BD, SHF, SLPF,  !Soil states
!    & SNOW, SW, NO3LEFT, NH4LEFT,                         !H2o,N states
!    & YEARPLT, YEARPLTCSM, HARVFRAC,                      !Pl.date
!    & PARIP, EOP, TRWUP,                                  !Resources
!    & LAI, KCAN, KEP,                                     !States
!    & RLV, NFP, RWUPM, RWUMX, CANHT, LAIL,                !States
!    & UNO3ALG, UNH4ALG, UH2O,                             !Uptake
!    & SENCALG, SENNALG, SENLGALG,                         !Senescence
!    & RESCALG, RESNALG, RESLGALG,                         !Residues
!    & STGDOY,                                             !Stage dates
!    & DYNAMIC)                                            !Control

      ! For CSM
      SUBROUTINE CSCER (FILEIOIN, RUN, TN, RN, RNMODE,     !Command line
     & ISWWAT, ISWNIT, IDETS, IDETO, IDETG, IDETL, FROP,   !Controls
     & SN, ON, RUNI, REP, YEAR, DOY, STEP, CN,             !Run+loop
     & SRAD, TMAX, TMIN, CO2, RAIN, TOTIR,                 !Weather
     & DAYLT, WINDSP, ST, EO,                              !Weather
     & NLAYR, DLAYR, DEPMAX, LL, DUL, SAT, BD, SHF, SLPF,  !Soil states
     & SNOW, SW, NO3LEFT, NH4LEFT,                         !H2o,N states
     & YEARPLTCSM, HARVFRAC,                               !Pl.date
     & EOP, EP, ET, TRWUP,                                 !Resources
     & LAI, KCAN, KEP,                                     !States
     & RLV, NFP, RWUPM, RWUMX, CANHT,                      !States
     & UNO3ALG, UNH4ALG, UH2O,                             !Uptake
     & SENCALG, SENNALG, SENLGALG,                         !Senescence
     & RESCALG, RESNALG, RESLGALG,                         !Residues
     & STGDOY,                                             !Stage dates
     & DYNAMIC)                                            !Control

! 2023-01-25 CHP removed unused variables from argument list
!    & WEATHER, SOILPROP, CONTROL,

      ! Substantive changes as worked on comparison
      !   1. Reserves overflow. Introduced
      !   2. Tillering.
      !        Fibonacci factors to match CSCRP
      !        AMIN1 function for stresses (instead of multiplicative)
      !        Assimilates factor talen out
      !   3. Leaf appearance. Fof barley, rate of change of daylength 
      !                       algorithm taken out.
      !   4. Introduced RSUSE to help maintain leaf growth
      
      ! Need to change.
      !   1. Seed reserves (ch2o and N use and early root growth.

      ! For incorporation in CSM should:
      !    Change argument above.
      !    Eliminate '!' from SUMVALS call.
      !    Comment out call to Disease module.
      
      ! Changes for 4.50
      !
      !  A. Changes that should not affect results.
      ! 
      !  1.  Added subroutine code to the main program for those 
      !      subroutines used when running in CSM. Deleted the
      !      subroutines. 
      !  2.  Cleaned the list of variable declarations.
      !  2.  Added a few additional outputs, especially to WORK.OUT
      !  4.  Added outputs (-99's mostly) when run stops because 
      !      of missing weather or equivalent problem.
      !     
      !  A.  Changes that MAY affect results.
      !     
      ! 1.  Took out the 0.6 adjustment for RLWR (SPE file) that stemmed
      !     from the original Ceres 'exudation' loss of dry matter sent 
      !     to the roots. With the 0.6 factor the 'real' RLWR parameter 
      !     would have been 1.633. It was left at 0.98 rather than being
      !     changed to 1.633 because there was very little water stress 
      !     being predicted for experiments in which there should have 
      !     been stress. 
      !     
      ! 2.  Set upper water threshold for photosynthesis (WFPU in ECO
      !     file) to 1.0 following discussion with KJB.  
      !     
      ! 3.  Set upper water threshold for growth (WFGU in ECO
      !     file) to 1.3 following discussion with KJB. 
      !     
      ! 4.  Brought the soil fertility factor SLPF into the
      !     module and now use it to modify photosynthesis in conformity
      !     with other CSM crop modules. 
      !     
      ! 5.  Brought the planting date when running a sequence in CSM 
      !     (ie.under DSSAT) across from the planting section 
      !     of CSM. 
      !     
      ! 6.  Brought the harvest fractions (both product and by-product) 
      !     when running in CSM (IE.under DSSAT) across for
      !     all modes of operation.
      !     
      ! 7.  Changed the planting coding to ensure that neither 
      !     growth nor development occur on the day of planting. This 
      !     contrasts with the previous version, in which growth and 
      !     development ocurred on the day of planting. Phenology will
      !     be affected by this change.
      !     
      ! 8.  Changed the meaning of the so called water factor for root  
      !     growth (WFRG) in the species file. It previously was
      !     interpreted as a multiplier to be applied to the soil water
      !     'potential' to determine the degree to which root 
      !     growth would be reduced below potential. It is now 
      !     interpreted as an upper threshold value that is applied 
      !     to the soil water 'potential' to determine the factor 
      !     (now called the water factor for root growth) used to 
      !     reduce root growth below potential. The change 
      !     alters the degree to which root growth is reduced in 
      !     layers in which the water content is below the upper 
      !     limit. The parameter can be VERY IMPORTANT. It is now
      !     used 'as is' (not the square root) in the algoorithm for
      !     root depth growth.
      !     
      ! 9.  Eliminated the second root growth water factor (WFRDG),which
      !     was used as a multiplier to determine the impact of the 
      !     photosynthesis water factor on root depth growth.   
      !
      ! 10. Took out the N factor from the algorithm determining root
      !     dry matter distribution with depth.
      !
      ! 11. Corrected the algorithm used to calculate water 'potential'
      !     at the seed depth. Previously, if seding was in layer 1, the
      !     algorithm was always giving the potential of layer 2. It
      !     should produce an interpolated value, which hopefully it
      !     now does.
      !
      ! 12. Introduced code to re-initialize the leaf loss through cold
      !     parameter so that leaf area did not continue to be lost 
      !     after a cold spell ended.          

      ! Needed!
      ! 1. Leaf # and time to terminal spikelet too great
      !    when leaf # > 15. Need limit. See LAMS
      ! 2. Yield and kernel # from early and late plantings
      !    high. Need stress factor that affects kernel set
      !    See MCCR
      ! 3. Reserves can go to zero during grain fill, then
      !    recover. Need accelerated senescence! See KSAS.
      ! 4. The saturation factor for water uptake (SATFACL)
      !    gave far too great stress for one Australian data
      !    set (CSGR9802). It is now controlled (for standalone
      !    Cropsim, not CSM) by a switch in the ecotype file 
      !    that is currently set to 0 .. hence no saturation 
      !    effect. Need to use switch in CSM somehow.

      USE OSDefinitions
      USE CSVOUTPUT  ! VSH
      USE ModuleDefs
      USE CER_First_Trans_m
      
      IMPLICIT NONE
      EXTERNAL CER_Init, CER_Growth, CER_Integrate, CER_Output

!      TYPE (ControlType), intent (in) :: CONTROL ! Defined in ModuleDefs
!      TYPE (WeatherType), intent (in) :: WEATHER ! Defined in ModuleDefs
!      TYPE (SoilType), intent (in) ::   SOILPROP ! Defined in ModuleDefs
    
!     INTEGER ADAT10, CSTIMDIF, CSINCDAT, DAPCALC
      INTEGER CN, DOY, DYNAMIC, DYNAMICI, FROP, NLAYR, ON, REP, RN          
      INTEGER RUN, RUNI, SN, STEP, STGDOY(20), TN, YEAR
!     INTEGER TVICOLNM
      INTEGER YEARPLTCSM !, CSYDOY, TVILENT
      
      REAL CANHT, EO, EP, EOP, ET, LAI, KCAN, KEP, NFP
      REAL BD(NL), DAYLT, DEPMAX, LL(NL), SAT(NL)
      REAL DLAYR(NL), DUL(NL), UNO3ALG(NL), SENLGALG(0:NL), UNH4ALG(NL)
      REAL RESWALG(0:NL), RESWAL(0:NL), RESNAL(0:NL), RESLGAL(0:NL)
      REAL HARVFRAC(2)
      REAL RESCALG(0:NL), RESLGALG(0:NL), RESNALG(0:NL), RLV(NL)
      REAL RAIN, RWUPM, RWUMX, SLPF, SHF(NL)
      REAL ST(0:NL), SENNALG(0:NL), SENCALG(0:NL), TRWUP, UH2O(NL)
      REAL SW(NL), NO3LEFT(NL), NH4LEFT(NL)
      REAL CO2, TMAX, TMIN, SRAD, WINDSP, SNOW
      REAL TOTIR !, TFAC4, YVALXY, YVAL1

      CHARACTER*1   IDETG, ISWNIT, ISWWAT, IDETL, IDETO, IDETS
      CHARACTER*1   RNMODE
      CHARACTER*250 FILEIOIN
!     CHARACTER*10  TL10FROMI  


        INTEGER ACOUNT
        CHARACTER*6, DIMENSION(EvaluateNum) :: OLAB, OLAP !OLAP in dap
        CHARACTER*40 DESCRIP(EvaluateNum)    
        
        
        ACOUNT = 40  !Number of FILEA headings.
!       Headings in FILEA for Measured data. The following measured data will show up in overview.out
        DATA OLAB /
     &    'ADAT  ', 
     &    'MDAT  ', 
     &    'HWAM  ', 
     &    'H#AM  ',
     &    'HWUM  ', 
     &    'H#UM  ',
     &    'CWAM  ', 
     &    'LAIX  ', 
     &    'HIAM  ', 
     &    'GNAM  ', 
     &    'CNAM  ', 
     &    'GN%M  ', 
     &    'CWAA  ',
     &    'CNAA  ',
     &    'L#SM  ',
     &    'EDAT  ',
     &    'HWAH  ',
     &    'GWUM  ',
     &    'BWAH  ',
     &    'T#AM  ',
     &    'VNAM  ',
!     &    'LN%A  ',
     &    'LNCA  ',
!    &    'GNCM  ',
     &    'HN%M  ',
!     &    'HNCM  ',
     &    'VN%M  ',
     &    'VN%D  ',
     &    'VNCD  ',
     &    'LNOSM ',
     &    'DRDAT ',
     &    'TSDAT ',
     &    'A1DAT ',
     &    'LLDAT ',
     &    'SPDAT ',
     &    'JDAT  ',
     &    'GS059 ',
     &    6*'    '/

      YEARDOY = YEAR*1000 + DOY

      IF (DYNAMIC.EQ.RUNINIT .OR. DYNAMIC.EQ.SEASINIT) THEN

!       Assign descriptions to Measured and Simulated data 
!         from DATA.CDE.
        CALL GETDESC(ACOUNT, OLAB, DESCRIP)

        CALL CER_Init (LAI, CANHT,
     &     CN, DOY, HARVFRAC,
     &     FILEIOIN, FROP, IDETL,
     &     KCAN, KEP, NFP, ON,
     &     RESCALG, RESLGALG, RESNALG, RLV, RN, RNMODE,
     &     RUN, RUNI, RWUMX, RWUPM, 
     &     UH2O, YEAR, SLPF, SN,
     &     STGDOY, TN, TRWUP, DYNAMIC)

      ELSEIF (DYNAMIC.EQ.RATE) THEN

        CALL CER_Growth (BD, CANHT, CO2, DAYLT,
     &     DLAYR, DUL, EO, EOP, ISWNIT, ISWWAT,
     &     KEP, LL, NFP, NH4LEFT, NLAYR , NO3LEFT,
     &     RLV, RNMODE, SAT , SENCALG, SENNALG,
     &     SHF, SLPF, SNOW, SRAD, ST, STGDOY, SW,
     &     TMAX, TMIN, TRWUP, UH2O, UNH4ALG, UNO3ALG, 
     &     WINDSP, YEARPLTCSM, LAI)

        IF (YEARDOY.GE.YEARPLT) THEN   

          IF (XSTAGE.LT.LRETS) THEN
            SENWALG(0) = (SENLFG+SENSTG) * PLTPOP*10.0
            SENNALG(0) = (SENNLFG+SENNSTG) * PLTPOP*10.0
            SENCALG(0) = (SENLFG+SENSTG) * 0.4 * PLTPOP*10.0
            SENLGALG(0) =
     &       (SENLFG*LLIGP/100+SENSTG*SLIGP/100) * PLTPOP*10.0
          ENDIF

          ! Root senescence
          DO L = 1, NLAYR
            SENWALG(L) = RTWTSL(L) * PLTPOP*10.0
            SENNALG(L) = RTNSL(L) * PLTPOP*10.0
            SENCALG(L) = SENWALG(L) * 0.4
            SENLGALG(L) = SENWALG(L) * RLIGP/100.0
          ENDDO

          DYNAMICI = 0

        ENDIF

      ELSEIF (DYNAMIC.EQ.INTEGR) THEN

        CALL CER_Integrate (LAI, CANHT, CO2,
     &     DAYLT, DEPMAX, DLAYR, DOY, EOP, EP, ET, KCAN,
     &     HARVFRAC, ISWWAT, LL, NFP, NLAYR,
     &     RAIN, RESCALG, RESLGALG, RESNALG, RLV,
     &     RESWALG, RESWAL, RESNAL, RESLGAL,
     &     SRAD, STGDOY, SW, TMAX, TMIN,
     &     YEAR)

      ELSEIF (DYNAMIC.EQ.OUTPUT .OR. 
     &        DYNAMIC.EQ.SEASEND .AND. SEASENDOUT.NE.'Y') THEN

        CALL CER_Output (OLAB, LAI, CANHT, CN, DOY,
     &     DYNAMIC, EOP, IDETG, IDETL, IDETO, IDETS,
     &     ISWNIT, ISWWAT, NFP, ON, REP,
     &     RLV, RN, RNMODE, RUN, RUNI, SN, STEP, STGDOY,
     &     TOTIR, TN, YEAR)
     
      ELSEIF (DYNAMIC.EQ.SEASEND) THEN

        CLOSE (NOUTPG)
        INQUIRE (FILE = OUTPN, EXIST = FEXIST)
        IF (FEXIST) CLOSE (NOUTPN)
        CLOSE (NOUTPG2)
        CLOSE (NOUTPGF)
!        CLOSE (FNUMWRK)

      ENDIF   ! Tasks

      cswdis = 'N'
!     IF(cswdis.EQ.'Y')
!    X CALL Disease(   If needed must check argument variables  )
      
      END  ! CSCER040

!=======================================================================
!  CSTRANS Subroutine
!  Calculates potential plant evaporation (ie.transpiration) rate
!-----------------------------------------------------------------------
!  Revision history
!  01/01/89 JR  Written
!  01/01/89 JWJ Modified for climate change using ETRATIO subroutine.
!  12/05/93 NBP Made into subroutine and changed to TRATIO function.
!  10/13/97 CHP Modified for modular format.
!  11/25/97 CHP Put in file TRANS.for w/ TRATIO and BLRRES
!  09/01/99 GH  Incorporated into CROPGRO
!  01/13/00 NBP Added DYNAMIC contruct to input KCAN
!  04/09/01 LAH Modified for CROPGRO-SIM
!=======================================================================

      SUBROUTINE CSTRANS(ISWWAT,                          !Control
     & TMAX, TMIN, WINDSP, CO2, EO,                       !Weather
     & CROP, LAI, KEP,                                    !Crop,LAI
     & eop,                                               !Pot.pl.evap
     & DYNAMICI)                                          !Control

      IMPLICIT NONE
      EXTERNAL VPSLOP
      SAVE

      REAL          BLRESD        ! Boundary layer resistance      s/m
      REAL          BLRESE1       ! Boundary layer resistance      s/m
      REAL          BLRESE2       ! Boundary layer resistance      s/m
      REAL          BLRESE3       ! Boundary layer resistance      s/m
      REAL          BLRESE4       ! Boundary layer resistance      s/m
      REAL          BLRESE5       ! Boundary layer resistance      s/m
      REAL          BLRESE6       ! Boundary layer resistance      s/m
      REAL          BLRESEN       ! Boundary layer resistance      s/m
      REAL          BLRESRC1      ! Boundary layer resistance      s/m
      REAL          BLRESRC2      ! Boundary layer resistance      s/m
      REAL          BLRESRS1      ! Boundary layer resistance      s/m
      REAL          BLRESRS2      ! Boundary layer resistance      s/m
      REAL          BLRESX        ! Boundary layer resistance      s/m
      REAL          BLRESZ0       ! Boundary layer resistance      s/m
      REAL          BLRESZ1       ! Boundary layer resistance      s/m
      REAL          CHIGHT        ! Reference height for crop      m
      REAL          CO2           ! CO2 concentration in air       vpm
      CHARACTER*2   CROP          ! Crop identifier (ie. WH, BA)   text
      REAL          DELTA         ! Slope,sat vapor pres/tem curve Pa/K
      INTEGER       DYNAMICI      ! Module control,internal        code
      REAL          EO            ! Potential evapotranspiration   mm/d
      REAL          EOP           ! Potential evaporation,plants   mm/d
      REAL          GAMMA         ! Variable in Penman formula     #
      CHARACTER*1   ISWWAT        ! Soil water balance switch Y/N  code
      REAL          KEP           ! Extinction coeff for SRAD      #
      REAL          LAI           ! Leaf area index                #
      REAL          LAIMAX        ! Leaf area index,maximum        #
      REAL          LHV           ! Latent heat of vaporization    J/g
      REAL          RA            ! Atmospheric resistance         s/m
      REAL          RATIO         ! Ratio of LAI to maximum LAI    #
      REAL          RB            ! Leaf resistance addition fac   s/m
      REAL          RL            ! Canopy resistance for CO2      s/m
      REAL          RLC           ! Canopy resistance,actual CO2   s/m
      REAL          RLF           ! Leaf stomatal res,330.0 ppmCO2 s/m
      REAL          RLFC          ! Leaf stomatal resistance       s/m
      INTEGER       RUNINIT       ! Control variable,initiation    #
      REAL          TMAX          ! Temperature maximum            C
      REAL          TMIN          ! Temperature minimum            C
      REAL          TRATIO        ! Function,relative tr rate      #
      REAL          UAVG          ! Average wind speed             m/s
      REAL          VPSLOP        ! Slope,sat vapor pres/tem curve Pa/K
      REAL          WINDSP        ! Wind speed                     km/d
      REAL          XDEN          ! Transpiration,actual CO2       g/m2
      REAL          XNUM          ! Transpiration,standaard CO2    g/m2

      PARAMETER     (RUNINIT=1)

      EOP = 0.0
      DYNAMICI = DYNAMICI    ! Temporary until used for initialisation

      IF(ISWWAT.EQ.'Y')THEN
        IF (LAI .LT. 0.01) THEN
          TRATIO = 1.0
          GO TO 9999    ! Don't calculate tratio if LAI very small
        ENDIF

        ! Initialize.
        IF (WINDSP .LE. 0.0) WINDSP = 86.4
        UAVG = WINDSP / 86.4
        RB = 10.0
        LAIMAX = 3.5

        ! Set canopy height
        CHIGHT = 1.0

        ! Canopy resistances, RL and RLC.
        ! RLF = Leaf stomatal resistance at 330.0 ppm CO2, s/m
        ! RLFC = Leaf stomatal resistance at other CO2 conc., s/m
        ! (Allen, 1986), Plant responses to rising CO2.
        IF (INDEX('MZMLSG',CROP) .GT. 0) THEN
           ! C-4 Crops  EQ 7 from Allen (1986) for corn.
          RLF =(1.0/(0.0328 - 5.49E-5*330.0 + 2.96E-8 * 330.0**2))+RB
          RLFC=(1.0/(0.0328 - 5.49E-5* CO2  + 2.96E-8 * CO2  **2))+RB
        ELSE
          ! C-3 Crops
          RLF  = 9.72 + 0.0757 * 330.0 + 10.0
          RLFC = 9.72 + 0.0757 *  CO2  + 10.0
        ENDIF

        RL = RLF / LAI
        RLC = RLFC / LAI

        ! Boundary layer resistance (Jagtap and Jones, 1990).
        BLRESEN = 3.0
        BLRESZ1 = 0.01
        BLRESX = 2.0
        BLRESD = 0.7 * CHIGHT**0.979
        BLRESZ0 = 0.13 * CHIGHT**0.997

        BLRESE1 = EXP(BLRESEN*(1. - (BLRESD + BLRESZ0) / CHIGHT))
        BLRESE2 = EXP(BLRESEN)
        BLRESE3 = CHIGHT/(BLRESEN*(CHIGHT-BLRESD))
        BLRESE4 = ALOG((BLRESX - BLRESD)/BLRESZ0)
        BLRESE5 = 0.4 * 0.4 * UAVG
        BLRESE6 = ALOG((BLRESX - BLRESD) / (CHIGHT - BLRESD))

        BLRESRS2 = BLRESE4 * BLRESE3 * (BLRESE2 - BLRESE1)/BLRESE5
        BLRESRC2 = BLRESE4*(BLRESE6+BLRESE3*(BLRESE1-1.))/BLRESE5
        BLRESRS1 = ALOG(BLRESX/BLRESZ1)*
     &             ALOG((BLRESD+BLRESZ0)/BLRESZ1)/BLRESE5
        BLRESRC1 = (ALOG(BLRESX/BLRESZ1)**2)/BLRESE5
        BLRESRC1 = BLRESRC1-BLRESRS1

        RATIO = LAI/LAIMAX
        IF (RATIO .GT. 1.0) RATIO = 1.0
        RA = BLRESRC1 + (BLRESRC2 - BLRESRC1) * RATIO

        ! Transpiration ratio (CO2=330 vpm gives 1.0)
        DELTA = VPSLOP((TMAX+TMIN)/2.0) / 100.0
        LHV    = 2500.9 - 2.345*(TMAX+TMIN)/2.0
        GAMMA  = 1013.0*1.005/(LHV*0.622)
        XNUM = DELTA + GAMMA*(1.0+RL/RA)
        XDEN = DELTA + GAMMA*(1.0+RLC/RA)
        TRATIO = XNUM / XDEN

 9999   CONTINUE

        EOP = EO * (1.0-EXP(-LAI*KEP)) * TRATIO
        EOP = MAX(EOP,0.0)

      ENDIF

      RETURN

      END  ! CSTRANS

!=======================================================================
!  CSROOTWU Subroutine
!  Root water uptake rate for each soil layer and total rate.
!-----------------------------------------------------------------------
!  Revision history
!  01/01/89 JR  Written
!  12/05/93 NBP Made into subroutine.
!  01/18/96 JWJ Added flooding effect on water uptake
!  01/06/96 GH  Added soil water excess stress
!  10/10/97 CHP Updated for modular format.
!  09/01/99 GH  Incorporated in CROPGRO
!  01/10/00 NBP Added SAVE for stored variables and set SWCON2=RWU=0.0
!  01/12/00 NBP Removed FILECC from input
!  01/25/00 NBP Added IOSTAT to READ statements to set ERRNUM.  Cleaned.
!  05/09/01 LAH Modified for CROPSIM
!=======================================================================

      SUBROUTINE CSROOTWU(ISWWAT,                          !Control
     & NLAYR, DLAYR, LL, SAT,                              !Soil
     & EOP,                                                !Pot.evap.
     & RLV, RWUPM, RWUMX,                                  !Crop state
     & SW,                                                 !Soil h2o
     & UH2O, TRWUP,                                        !H2o uptake
     & DYNAMICI)                                           !Control

      IMPLICIT NONE
      EXTERNAL GETLUN
      SAVE

      INTEGER       NL            ! Maximum number soil layers,20
      PARAMETER     (NL=20)       !

      REAL          BLAYER        ! Depth at base of layer         cm
      INTEGER       DYNAMICI      ! Module control,internal        code
      REAL          DLAYR(20)     ! Depth of soil layers           cm
      REAL          EOP           ! Potential evaporation,plants   mm/d
      !INTEGER       FNUMWRK       ! File number,work file          #
      CHARACTER*1   ISWWAT        ! Soil water balance switch Y/N  code
      INTEGER       L             ! Loop counter                   #
      REAL          LL(NL)        ! Lower limit,soil h2o           #
      INTEGER       NLAYR         ! Actual number of soil layers   #
      REAL          RWUPM         ! Pore space threshold,pl effect #
      REAL          RLV(20)       ! Root length volume by layer    #
      REAL          RLVSUM        ! Temporary RLV sum              #
      INTEGER       RUNINIT       ! Control variable,initiation    #
      REAL          RWU(20)       ! Root water uptake by layer     mm/d
      REAL          RWUMX         ! Root water uptake,maximum      mm2/m
      REAL          RWUP          ! Root water uptake,potential    cm/d
      REAL          SAT(20)       ! Saturated limit,soil           #
      REAL          SATFACL       ! Soil water excess stress factr #
      REAL          SW(20)        ! Soil water content             #
      REAL          SWAFOLD       ! Soil water availability CERES  #
      REAL          SWCON1        ! Constant for root water uptake #
      REAL          SWCON2(NL)    ! Variable for root water uptake #
      REAL          SWCON3        ! Constant for root water uptake #
      REAL          TRWUP         ! Total water uptake,potential   cm
      REAL          TSS(NL)       ! Number of days saturated       d
      REAL          UH2O(NL)      ! Uptake of water                cm/d
      REAL          WFSAT         ! Soil water excess stress fact  #
      REAL          WUF           ! Water uptake factor            #
      REAL          WUP(NL)       ! Water uptake                   cm/d
      REAL          WUT           ! Water uptake,total             cm/d

      PARAMETER     (RUNINIT=1)


      IF (ISWWAT.NE.'Y') RETURN

      IF (DYNAMICI.EQ.RUNINIT) THEN

        !CALL Getlun('WORK.OUT',fnumwrk)

        ! Compute SWCON2 for each soil layer.  Adjust SWCON2 for very
        ! high LL to avoid water uptake limitations.
        SATFACL = 1.0
        DO L = 1,NL
          SWCON2(L) = 0.0
          RWUP = 0.0
          RWU(L) = 0.0
        ENDDO
        DO L = 1,NLAYR
          SWCON2(L) = 120. - 250. * LL(L)
          IF (LL(L) .GT. 0.30) SWCON2(L) = 45.0
        ENDDO

        ! Set SWCON1 and SWCON3.
        SWCON1 = 1.32E-3
        SWCON3 = 7.01

      ENDIF

      TRWUP   = 0.0
      RLVSUM = 0.0
      BLAYER = 0.0

      DO L = 1,NLAYR
        BLAYER = BLAYER + DLAYR(L)
      ENDDO

      DO L = 1,NLAYR
        IF (RLV(L).LE.0.00001 .OR. SW(L).LE.LL(L)) THEN
          RWUP = 0.
        ELSE
          RWUP = SWCON1*EXP(MIN((SWCON2(L)*(SW(L)-LL(L))),40.))/
     &    (SWCON3-ALOG(RLV(L)))
          ! Excess water effect
          ! RWUPM = Minimum pore space required for supplying oxygen
          ! TSS(L) = number of days soil layer L has been saturated
          IF ((SAT(L)-SW(L)) .GE. RWUPM) THEN
            TSS(L) = 0.
          ELSE
            TSS(L) = TSS(L) + 1.
          ENDIF
          ! 2 days after saturation before water uptake is affected
          IF (TSS(L).GT.2.0 .AND. RWUPM.GT.0.0) THEN
             SATFACL = MIN(1.0,MAX(0.0,(SAT(L)-SW(L))/RWUPM))
          ELSE
             SATFACL = 1.0
          ENDIF
          WFSAT = 1.0 - (1.0-SATFACL)       
          RWUP = MIN(RWUP,RWUMX*WFSAT)
          RWUP = MIN(RWUP,RWUMX)
        ENDIF
        IF (RLV(L).GT.0.0) THEN
          SWAFOLD = AMIN1(1.0,AMAX1(0.0,(RWUP*RLV(L))/(SW(L)-LL(L))))
          WUP(L) = SWAFOLD*DLAYR(L)*(SW(L)-LL(L))
          TRWUP = TRWUP+WUP(L)
        ELSE
          WUP(L) = 0.0
        ENDIF
      ENDDO

      IF (TRWUP .GT. 0.0) THEN
        IF (EOP*0.1 .GE. TRWUP) THEN
          WUF = 1.0
        ELSE
          WUF = (EOP*0.1) / TRWUP
        ENDIF
        WUT = 0.0
        DO L = 1, NLAYR
          UH2O(L) = WUP(L) * WUF
          WUT = WUT + UH2O(L)
        END DO
      ELSE        !No root extraction of soil water
        WUT = 0.0
        DO L = 1,NLAYR
          UH2O(L) = 0.0
        ENDDO
      ENDIF

      RETURN

      END  ! CSROOTWU

!=======================================================================
!  CSLAYERS Subroutine
!  Leaf distribution module
!-----------------------------------------------------------------------
!  Revision history
!  1. Written for Cropsim                         L.A.H.      ?-?-98
!=======================================================================

      SUBROUTINE Cslayers
     X (chtpc,clapc,              ! Canopy characteristics
     X pltpop,lai,canht,          ! Canopy aspects
     X lcnum,lap,lap0,            ! Leaf cohort number and size
     X LAIL)                      ! Leaf area indices by layer

      IMPLICIT NONE
      EXTERNAL YVALXY
      SAVE

      INTEGER       clx           ! Canopy layers,maximum          #
      INTEGER       lcx           ! Leaf cohort number,maximum     #
      PARAMETER     (clx=30)      ! Canopy layers,maximum          #
      PARAMETER     (lcx=100)     ! Leaf cohort number,maximum     #

      REAL          caid          ! Canopy area index              m2/m2
      REAL          canfr         ! Canopy fraction                #
      REAL          canht         ! Canopy height                  cm
      !REAL         clpos         ! Canopy layer,temporary         #
      REAL          clthick       ! Canopy layer thickness,temp    cm
      !INTEGER      cltot         ! Canopy layer number,total      #
      INTEGER       clnum         ! Canopy layer number,species    #
      !REAL         cltotfr       ! Canopy fraction in top layer   #
      !INTEGER      cn            ! Component                      #
      REAL          chtpc(10)     ! Canopy ht % assoc with lf area %
      INTEGER       l             ! Loop counter                   #
      REAL          clapc(10)     ! Canopy lf area % w height      %
      REAL          lai           ! Leaf lamina area index         m2/m2
      !REAL         lailad(clx)   ! Lf lamina area index,active    m2/m2
      REAL          lailatmp      ! Leaf lamina area,active,temp   m2/m2
      REAL          lail(clx)     ! Leaf lamina area index         m2/m2
      REAL          lailtmp       ! Leaf lamina area,temporary     m2/m2
      REAL          lap(lcx)      ! Leaf lamina area,cohort        cm2/p
      REAL          lap0          ! Leaf lamina area gr,cohort     cm2/p
      REAL          lapp(lcx)     ! Leaf lamina area,infected      cm2/p
      REAL          laps(lcx)     ! Leaf lamina area,senescent     cm2/p
      INTEGER       lcnum         ! Leaf cohort number             #
      INTEGER       lcnumr        ! Leaf cohorts remaining         #
      REAL          lfrltmp       ! Leaves above bottom of layer   fr
      REAL          lfrutmp       ! Leaves above top of layer      fr
      REAL          pltpop        ! Plant population               #/m2
      INTEGER       tvi1          ! Temporary variable,integer     #
      !INTEGER      tvilc         ! Temporary value,lf cohort      #
      REAL          YVALXY        ! Y value from function          #

!-----------------------------------------------------------------------

      caid = lai
      DO L = 1,25
        LAPP(L) = lap(l)  ! Temporary - avoid warning
        LAPS(L) = 0.0
      ENDDO

!-----------------------------------------------------------------------

      IF (caid.LT.0.0) RETURN
      IF (canht.LE.0.0) RETURN

!-----------------------------------------------------------------------

      ! Establish layer thickness (Must be ok for tallest species!)
      clthick=10.0                     ! Layer thickness (cm)

!-----------------------------------------------------------------------

      ! Determine layer number for species
      IF(MOD(canht,clthick).GT.0)THEN
       clnum=AINT(canht/clthick)+1
      ELSE
       clnum=AINT(canht/clthick)
      ENDIF
      clnum = MAX(clx,clnum)

      ! Distribute leaf area over layers

      DO tvi1=30,1,-1                  ! Do all layers;top down (old=1)
       lail(tvi1)=0.0                  ! Lai by layer
      ENDDO

      lfrutmp=1.0

      lfrutmp=0.0
      DO tvi1=clnum,1,-1               ! Do over layers down to soil
       canfr = (canht-(clthick*(tvi1-1))) / canht
       IF (canfr.GT.0.0) THEN
         lfrltmp = YVALXY(CHTPC,CLAPC,CANFR)
         lail(tvi1)=lai*(lfrltmp-lfrutmp)
         lfrutmp=lfrltmp
        ENDIF
      ENDDO

!-----------------------------------------------------------------------

      ! Calculate active leaf area in layers
      ! Disease damage

      lcnumr=lcnum
      lailtmp=lap0*pltpop*0.0001
      lailatmp=lap0*pltpop*0.0001

c     DO tvi1=cltot,1,-1               ! Do over layers down to soil
c      lail(tvi1)=lailtmp
c      lailad(tvi1)=lailatmp
c      lailtmp=0.0
c      lailatmp=0.0
c
c      DO tvilc=lcnumr,0,-1            ! Do damage,living cohorts
c       IF(tvilc.GT.0)THEN
c        lail(tvi1)=lail(tvi1)+
c    x   (lap(tvilc)-laps(tvilc))*pltpop*0.0001
c        lailad(tvi1)=lailad(tvi1)+
c    x   (lap(tvilc)-laps(tvilc)-lapp(tvilc))*pltpop*0.0001
c        ! Could adjust above for effect on activity as well as area -
c        ! ie multiply by a 0-1 factor dependent on pathogen and area
c       ENDIF
c
c       IF(caid.GT.0.AND.
c    x  lail(tvi1).GE.cails(cn,tvi1)*(lai/caid))THEN
c        lailtmp=lail(tvi1)-cails(cn,tvi1)*(lai/caid)
c        lail(tvi1)=lail(tvi1)-lailtmp
c        IF(tvilc.GT.0)THEN
c         IF(lap(tvilc)-laps(tvilc).GT.0)THEN
c          lailatmp=lailtmp*
c    x     (lap(tvilc)-laps(tvilc)-lapp(tvilc))/
c    x     (lap(tvilc)-laps(tvilc))
c          lailad(tvi1)=lailad(tvi1)-lailatmp
c         ENDIF
c        ENDIF
c        EXIT
c       ENDIF
c
c      ENDDO
c      lcnumr=tvilc
c      ! End damaged and senescent area section
c
c     ENDDO

      RETURN

      END  ! CSLAYERS


