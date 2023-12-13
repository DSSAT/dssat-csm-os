!-----------------------------------------------------------------------
!  CROPSIM CEREAL GROWTH AND DEVELOPMENT MODULE  Version 010115
!
!  Version changed to accomodate new CUL,ECO files;minor other changes
!
!  Last edit 050415  Few more writes to Work.out 
!  2023-01-26 CHP Reduce compile warnings: add EXTERNAL stmts, remove 
!                 unused variables, shorten lines.                    
!-----------------------------------------------------------------------
  
      SUBROUTINE CSCRP(FILEIOIN, RUN, TN, RN, RNMODE,                                        
     &  ISWWAT, ISWNIT, ISWDIS, MESOM,                      !Controls
     &  IDETS, IDETO, IDETG, IDETL, FROP,                   !Controls
     &  SN, ON, RUNI, REP, YEAR, DOY, STEP, CN,             !Run+loop
     &  SRAD, TMAX, TMIN, TAIRHR, RAIN, CO2, TDEW,          !Weather
     &  DRAIN, RUNOFF, IRRAMT,                              !Water
     &  DAYLT, WINDSP, DEWDUR, CLOUDS, ST, EO, ES,          !Weather        
     &  NLAYR, DLAYR, DEPMAX, LL, DUL, SAT, BD, SHF, SLPF,  !Soil states
     &  SNOW, SW, NO3LEFT, NH4LEFT, FERNIT,                !H2O,N states
     &  TLCHD, TNIMBSOM, TNOXD,                            !N components
     &  TOMINFOM, TOMINSOM, TOMINSOM1, TOMINSOM2, TOMINSOM3,!N comp
     &  YEARPLTCSM, HARVFRAC,                               !Pl.date
     &  PARIP, PARIPA, EOP, EP, ET, TRWUP, ALBEDOS,         !Resources
     &  LAI, KCAN, KEP,                                     !States
     &  RLV, NFP, RWUPM, RWUMX, CANHT, LAIL, LAILA,         !States
     &  UNO3, UNH4, UH2O,                                   !Uptake
     &  SENCALG, SENNALG, SENLALG,                          !Senescence
     &  RESCALG, RESNALG, RESLGALG,                         !Residues
     &  STGYEARDOY, GSTAGE,                                 !Stage dates   
!    &  WEATHER, SOILPROP, CONTROL,                         
     &  DYNAMIC)                                            !Control

!     2023-01-20 chp removed unused variables from argument list
!     WEATHER, SOILPROP, CONTROL, 

!       Staging in comparison with Ceres
!        1 Germinate               1 Germinate
!        2 Terminal spikelet       2 Terminal spikelet
!        3 Pseudo-stem
!        4 End leaf                3 End leaf
!        5 Head emerge             4 End spike growth
!        6 Anthesis
!        7 End anthesis
!        8 Milk end/start dough    5 Begin rapid grain geroowth
!        9 Hard dough              6 Physiological maturity  
!       10 Harvest                 6.9 Harvest 
      
!       Changes made after sending model to Jeff for Agmip work.
!        1. Variables passed to main module for outputting in Summary.out 
!        2. Version check for genotype files re-introduced
!        3. Phenology temperature responses made to work for differrent 
!           phases (was not working so responses for phase 1 for all)
!        4. Initialized tfdnext. Not initialized so sequence of runs 
!           used previous tfdnext for 1st phase, hence ADAT differed
!           for sequence of runs.
!        5. Capability to start at time of 1st irrig or at emergence 
!           introduced
!        6. Aitken formula introduced to calculate leaf number
!           to emerge after terminal spikelet.
!        7. Radiation effect on grain number set at anthesis
!        8. Potential grain size reduced if canopy size over
!           threshold.
!        9. Temperature responses changed.
!       10. Minor 'bugs' corrected.
      
!       NB. Some additional controls that are not handled by the CSM 
!           inputs routine are read directly from the x-file in the
!           section of code dealing with 'additional controls'. The
!           same approach could be used to input grazing information
!           if CSM is not modified to handle the additional grazing
!           inputs necessary. 
         
     
!       NB. TN, et.are not provided when running under CSM (DS4 files). 
!           They are read in XREADC in the CSREA module. 

!       Changes for AZMC  01/02/2014
!        1. Account for 'haying off' (Using TKGF from SPE file)
!        2. SRAD in phase 5 (head emergence to anthesis) effect on grain #
!           (using G#RF,G#RT from ECO file)
!        3. Potential grain size reduced as temperature in phase 5 increases
!           (Using GWTAF,GWTAT from ECO file)  

!       Changes for RORO  28/11/2013
!        1. HINM and VNPCM calculated without retained dead leaf
!        2. Root N taken out of remobilization pool
!           (N movement to grain from stem first,leaves 2nd.) 
!        3. Stem N limits changed. End minimum from 0.4 to 0.15
!        4. Labile N set at 10%
!        5. NUSEFAC left as NLABPC/100.0. No increase in late grain fill
!        6. NUPWF left at 1.0 after trying 0.0 (1.0->0.0 with changes 07/10/2014).
!        7. Calibration to increase tiller death (TDFAC 6->9); increase
!           leaf area (LAFV 0.1->0.3).  NB. Do not have a response to N
!           if potential leaf area too small (LAIS or increase factors)
!        8. Kernel wt.(GWTS) set too high at 50 to enable better 
!           simulation of yield. Often cannot simulate both K.Wt.and
!           yield ... maybe k.wt measurement after too good winnowing,
!           or moisture % problems.
!        9. Vegetative N% too low for higher N treatments ... need to
!           translocate less when have high soilN. 

!       Changes for KSAS  02/12/2013
!        1. Established flag set when LAI>0.0,not when shoot/root>2 
!           (But ? this as allows re-use in the fall)
!        2. May need to take reserve CH2O out of calculation of VNPCM

!       Changes for SWSW  05/12/2013
!        1. Minimum grain N% from species to ecotype characteristic.
      

!       For incorporation in CSM should:
!        Ensure that Alt-plant routine:
!         Sending control ISWDIS
!         Sending DEWDUR(=-99.0?), CLOUDS(=0.0?), ES, ALBEDO(=0.2)
!         Setting dummies for PARIP, PARIPA, LAIL, LAILA
!        Eliminate '!' from SUMVALS call.
!        Need to do something with metadata name if INH file

!       And to run well in CSM should:
!          Read ICIN (with -99 in profile) and ICSW variables

!       Temporary changes .. need firming-up:
!          Height increase on emergence day has initialisation value

!       Thoughts about possible changes:
!          1. Phyllochron senescence delayed if low light interception
!          2. Leaf and stem N top-up reduced in stages 4 and 5 (NTUPF)
!          3. Organise echo of input variables in Work.out into logical 
!             groups

!       Questions
!          1. What x-file information is essential to run
!          2. Impact of N uptake variable (NUPCF,NUPNF,NUPWF) 

!       Checks needed 
!          1. Algorthms for fraction of day at changeovers
!          2. Blips in response curves and reasons for these
!          3  All declarations,initialisations to 0 or -99

      USE OSDefinitions

      USE ModuleDefs
      USE CRP_First_Trans_m
      
      IMPLICIT NONE
      EXTERNAL DISEASE, CSCRPLAYERS, CRP_RUNINIT, CRP_SEASINIT, 
     &  CRP_GROWTH, CRP_INTEGRATE, CRP_OUTPUT

!     TYPE (ControlType), intent (in) :: CONTROL ! Defined in ModuleDefs
!     TYPE (WeatherType), intent (in) :: WEATHER ! Defined in ModuleDefs
!     TYPE (SoilType), intent (in) ::   SOILPROP ! Defined in ModuleDefs
    
      INTEGER CN, DOY, DYNAMIC, FROP, NLAYR, ON, REP, RN          
      INTEGER RUN, RUNI, SN, STEP, STGYEARDOY(20), TN, YEAR
      INTEGER YEARPLTCSM 
!     INTEGER CSTIMDIF, CSYDOY, DAPCALC, TVICOLNM
!     INTEGER CSIDLAYR, CSYEARDOY

      REAL ALBEDOS, BD(NL), GSTAGE, LAI, CANHT, CLOUDS, CO2  !, DAYL
      REAL DAYLT
      REAL DEPMAX, DEWDUR, DLAYR(NL), DRAIN, DUL(NL), EO, EOP, EP          
      REAL ES, ET, FERNIT, HARVFRAC(2), IRRAMT, KCAN, KEP, LAIL(30)
      REAL LAILA(30), LL(NL), NFP, NH4LEFT(NL), NO3LEFT(NL), PARIP
      REAL PARIPA, RAIN, SNOW
      REAL RESCALG(0:NL), RESLGALG(0:NL), RESNALG(0:NL), RLV(NL)
      REAL RWUMX, RWUPM, SAT(NL), SENCALG(0:NL), SENLALG(0:NL), TDEW
      REAL SENNALG(0:NL), SHF(NL), SLPF, SRAD, ST(0:NL), SW(NL), RUNOFF    
      REAL TLCHD, TAIRHR(24), TMAX, TMIN, TNIMBSOM, TNOXD, TOMINFOM                                                  
      REAL TOMINSOM, TOMINSOM1, TOMINSOM2, TOMINSOM3, TRWUP, UH2O(NL)
      REAL UNH4(NL), UNO3(NL), WINDSP    
!     REAL CSVPSAT, TFAC4, YVALXY, CSYVAL
  
      CHARACTER(LEN=1)   IDETG, IDETL, IDETO, IDETS, ISWDIS, ISWNIT      
      CHARACTER(LEN=1)   ISWWAT, MESOM, RNMODE      
      CHARACTER(LEN=250) FILEIOIN   
!     CHARACTER(LEN=10)  TL10FROMI                         

      INTRINSIC AMAX1,AMIN1,EXP,FLOAT,INDEX,INT,LEN,MAX,MIN,MOD,NINT
      INTRINSIC SQRT,ABS,TRIM

!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN    ! Initialization                           
!***********************************************************************

        CALL CRP_RunInit (CN, DOY, FILEIOIN, FROP, IDETL, ISWNIT,
     &     ON, RN, RNMODE, RUN, SN, TN, YEAR)
                    
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN    ! Initialization                      
!***********************************************************************
          
        CALL CRP_SeasInit (ALBEDOS, GSTAGE, LAI, CANHT, CLOUDS,
     &     CN, DEWDUR, DOY, HARVFRAC, ISWDIS, ISWNIT,
     &     KCAN, KEP, LAIL, LAILA, NFP, ON, PARIP,
     &     PARIPA, RESCALG, RESLGALG, RESNALG, RLV, RN, RNMODE,
     &     RUN, RUNI, RWUMX, RWUPM, SENCALG,
     &     UH2O, UNH4, UNO3, YEAR, SENLALG, SENNALG, SLPF, SN,
     &     STGYEARDOY, TAIRHR, TN, TRWUP)
            
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN                                              
!***********************************************************************

        call CRP_Growth (BD, CLOUDS, CO2, DAYLT,
     &    DLAYR, DOY, DUL, EO, EOP, ES, ISWDIS, ISWNIT , ISWWAT,
     &    KCAN, KEP, LL, NFP, NH4LEFT, NLAYR , NO3LEFT, PARIP,
     &    PARIPA, RLV, RNMODE, SAT , SENCALG, SENLALG, SENNALG,
     &    SHF, SLPF, SNOW, SRAD, ST, STGYEARDOY, SW, TDEW,
     &    TMAX, TMIN, TRWUP, UH2O, UNH4, UNO3, 
     &    WINDSP, YEAR, YEARPLTCSM, LAI,
     &    IDETG)

!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN                                            
!***********************************************************************

        CALL CRP_Integrate (ALBEDOS, BD, GSTAGE, LAI, CANHT, CO2,
     &     DAYLT, DEPMAX, DLAYR, DOY, DRAIN, EOP, EP, ET, FERNIT,
     &     IRRAMT, ISWNIT, ISWWAT, LL, NFP, NH4LEFT, NLAYR,
     &     NO3LEFT, RAIN, RESCALG, RESLGALG, RESNALG, RLV, RUNOFF,
     &     SRAD, STGYEARDOY, SW, TLCHD, TMAX, TMIN, TNIMBSOM,
     &     TNOXD, TOMINFOM, TOMINSOM, TOMINSOM1, TOMINSOM2,
     &     TOMINSOM3, YEAR)

!***********************************************************************
      ELSEIF (DYNAMIC.EQ.OUTPUT .AND. STEP.EQ.STEPNUM. OR.
     &        DYNAMIC.EQ.SEASEND .AND. SEASENDOUT.NE.'Y') THEN
!***********************************************************************
  
        CALL CRP_Output (GSTAGE, LAI, CANHT, CN, DAYLT, DOY, 
     &     DYNAMIC, EOP, IDETG, IDETL, IDETO, IDETS, ISWNIT,
     &     ISWWAT, KCAN, MESOM, NFP, NLAYR, ON, REP, RLV,
     &     RN, RNMODE, RUN, RUNI, SN, SRAD, STGYEARDOY, TN,
     &     TNIMBSOM, TOMINSOM1, UNH4 , UNO3, YEAR)

!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASEND) THEN                                         
!***********************************************************************

        EXCODEPREV = EXCODE

        CLOSE (NOUTPG)
        CLOSE (NOUTPG2)
        CLOSE (NOUTPGF)
        CLOSE (NOUTPN)
!        CLOSE (FNUMWRK)

!***********************************************************************
      ENDIF ! End of INITIATION-RATES-INTEGRATE-OUTPUT-SEASEND construct
!***********************************************************************
      ! Store previous dynamic setting
      DYNAMICPREV = DYNAMIC

!***********************************************************************
!    Call other modules
!***********************************************************************
      IF (LENDIS.GT.0.AND.LENDIS.LT.3) THEN
        IF (ISWDIS(LENDIS:LENDIS).NE.'N')
     X   CALL Disease(Spdirfle,run,runi,step,  ! Run+crop component
     X    fropadj,outhed,                      ! Loop info.
     X    year,doy,dap,                        ! Dates
     X    didat,digfac,diffacr,                ! Disease details
     X    dcdat,dcfac,dcdur,dctar,             ! Disease control
     &    tmax,tmin,dewdur,                    ! Drivers - weather
     &    pla,plas,pltpop,                     ! States - leaves
     &    lnumsg,lap,LAPP,laps,                ! States - leaves
!    &    stgyeardoy,                          ! Stage dates
     &    didoy,                               ! Disease initiation
     &    dynamic)                             ! Control
      ENDIF

      IF (DYNAMIC.EQ.INTEGR.AND.LNUMSG.GT.0) CALL Cscrplayers
     & (chtpc,clapc,                        ! Canopy characteristics
     & pltpop,lai,canht,                    ! Canopy aspects
     & lnumsg,lap,lapp,laps,                ! Leaf cohort number,size
     & LAIL,LAILA,                          ! Leaf area indices,layers
     & LAIA)                                ! Leaf area index,active

      END SUBROUTINE CSCRP

!-----------------------------------------------------------------------
!  CSCRPROOTWU Subroutine
!  Root water uptake rate for each soil layer and total rate.
!-----------------------------------------------------------------------

      SUBROUTINE CSCRPROOTWU(ISWWAT,                       !Control
     & NLAYR, DLAYR, LL, SAT, WFEU, MEWNU,                 !Soil
     & EOP,                                                !Pot.evap.
     & RLV, RWUPM, RLFWU, RWUMX, RTDEP,                    !Crop state
     & SW, WTDEP,                                          !Soil h2o
     & uh2o, trwup, trwu)                                  !H2o uptake

      IMPLICIT NONE
      EXTERNAL GETLUN, WARNING

      INTEGER,PARAMETER::NL=20         ! Maximum number soil layers,20

      REAL          BASELAYER     ! Depth at base of layer         cm
      REAL          DLAYR(20)     ! Depth of soil layers           cm
      REAL          DLAYRTMP(20)  ! Depth of soil layers with root cm
      REAL          EOP           ! Potential evaporation,plants   mm/d
      INTEGER       FNUMWRK       ! File number,work file          #
      !LOGICAL       FOPEN         ! File open indicator
      INTEGER       L             ! Loop counter                   #
      REAL          LL(NL)        ! Lower limit,soil h2o           #
      INTEGER       NLAYR         ! Actual number of soil layers   #
      REAL          RLFWU         ! Root length factor,water,upper /cm2
      REAL          RLFW          ! Root length factor,water uptak #
      REAL          RLV(20)       ! Root length volume by layer    /cm2
      REAL          RLVTMP(20)    ! Root length volume by layer    #
      REAL          RTDEP         ! Root depth                     cm
      REAL          RWUMX         ! Root water uptake,maximum      mm2/m
      REAL          RWUP          ! Root water uptake,potential    cm/d
      REAL          SAT(20)       ! Saturated limit,soil           #
      REAL          SW(20)        ! Soil water content             #
      REAL          SWCON1        ! Constant for root water uptake #
      REAL          SWCON2(NL)    ! Variable for root water uptake #
      REAL          SWCON3        ! Constant for root water uptake #
      REAL          TRWU          ! Total water uptake             mm
      REAL          TRWUP         ! Total water uptake,potential   cm
      REAL          TSS(NL)       ! Number of days saturated       d
      REAL          UH2O(NL)      ! Uptake of water                cm/d
      REAL          WTDEP         ! Water table depth              cm
      REAL          WFEU          ! Water factor,evapotp,upper     #
      REAL          WFEL          ! Water factor,evapotp,lower     #
      REAL          WFEWU         ! Water excess fac,water uptake  #
      REAL          RWUPM         ! Pors size for max uptake       fr
      REAL          WUF           ! Water uptake factor            #
      REAL          WUP(NL)       ! Water uptake                   cm/d
      REAL          WUPR          ! Water pot.uptake/demand        #

      CHARACTER (LEN=1)   ISWWAT  ! Soil water balance switch Y/N
      CHARACTER (LEN=1)   MEWNU   ! Switch,root water uptake method
      CHARACTER (LEN=78)  MESSAGE(10)   ! Messages for Warning.out

      INTRINSIC ALOG,AMAX1,AMIN1,EXP,MAX,MIN

      SAVE

      IF (ISWWAT.EQ.'N') RETURN

      IF (FNUMWRK.LE.0.0) THEN

        !CALL GETLUN ('WORK.OUT',FNUMWRK)
        !INQUIRE (FILE = 'WORK.OUT',OPENED = FOPEN)
        !IF (.NOT.FOPEN) OPEN (UNIT = FNUMWRK,FILE = 'WORK.OUT')

        ! Compute SWCON2 for each soil layer.  Adjust SWCON2 for very
        ! high LL to avoid water uptake limitations.
        WFEWU = 1.0
        DO L = 1,NL
          SWCON2(L) = 0.0
          RWUP = 0.0
        ENDDO
        DO L = 1,NLAYR
          SWCON2(L) = 120. - 250. * LL(L)
          IF (LL(L) .GT. 0.30) SWCON2(L) = 45.0
        ENDDO

        ! Set SWCON1 and SWCON3.
        SWCON1 = 1.32E-3
        SWCON3 = 7.01

        WFEL = 0.0

      ENDIF

      WUP = 0.0
      TRWUP   = 0.0
      BASELAYER = 0.0

      DO L = 1,NLAYR
        DLAYRTMP(L) = DLAYR(L)
        RLVTMP(L) = RLV(L)
        BASELAYER = BASELAYER + DLAYR(L)
        IF (RTDEP.GT.0.0.AND.RTDEP.LT.BASELAYER) THEN
        ! LAH Attempt to increase RLV when first penetrate a layer
        ! DLAYRTMP(L) = RTDEP-(BASELAYER-DLAYR(L))
        ! IF (DLAYRTMP(L).LE.0.0) EXIT
        ! RLVTMP(L) = RLV(L)*DLAYR(L)/DLAYRTMP(L)
        ENDIF
      ENDDO

      DO L = 1,NLAYR
        RWUP = 0.
        IF (RLVTMP(L).LE.0.00001 .OR. SW(L).LE.LL(L)) THEN
          ! 1,* Use water below LL if just germinated and next layer >LL
          IF (L.EQ.1.AND.RTDEP.LE.DLAYR(1)) THEN
            IF (SW(2).GT.LL(2)) WUP(L) = EOP*0.1
            WRITE(Message(1),'(A57)') 
     &      'To avoid early stress,h2o uptake set equal to demand.  '
            CALL WARNING(1,'CSCRP',MESSAGE)
!            WRITE(Fnumwrk,*)' '
!            WRITE(Fnumwrk,'(A58)') 
!     &      ' To avoid early stress,h2o uptake set equal to demand.  '
          ENDIF
        ELSE
          RWUP = SWCON1*EXP(MIN((SWCON2(L)*(SW(L)-LL(L))),40.))/
     &    (SWCON3-ALOG(RLVTMP(L)))
          ! Excess water effect
          WFEWU = 1.0
          IF (RWUPM.GT.0.0) THEN
            ! RWUPM = Relative position in SAT-DUL range before effect
            ! TSS(L) = number of days soil layer L has been saturated
            IF ((SAT(L)-SW(L)) .LT. RWUPM) THEN
              TSS(L) = 0.
            ELSE
              TSS(L) = TSS(L) + 1.
            ENDIF
            ! 2 days after saturation before water uptake is affected
            IF (TSS(L).GT.2.0) THEN
               WFEWU = MIN(1.0,MAX(0.0,(SAT(L)-SW(L))/RWUPM))
               WFEWU = 1.0 - (1.0-WFEWU)
               IF (WFEWU.LT.0.0) THEN
                 WRITE(Message(1),'(A52,I3,a26,F4.2)')
     &           ' Water uptake resticted by saturation,layer',L,
     &           ' Uptake saturation factor ',wfewu
                 CALL WARNING(1,'CSCRP',MESSAGE)
!                 WRITE(Fnumwrk,*)' '
!                 WRITE(Fnumwrk,'(A52,I3,a26,F4.2)')
!     &           ' Water uptake resticted by saturation,layer',L,
!     &           ' Uptake saturation factor ',wfewu
               ENDIF
            ENDIF
          ENDIF
          RWUP = MIN(RWUP,RWUMX*WFEWU)
          WUP(L) = RWUP*RLVTMP(L)*DLAYRTMP(L)
          ! Alternative method.Linear decline below threshold RLV
          IF (MEWNU.EQ.'W'.OR.MEWNU.EQ.'B') THEN
            RLFW = 1.0
            IF (RLFWU.GT.0.0) RLFW = AMAX1(0.,AMIN1(1.,RLV(l)/RLFWU))
            WUP(L) = DLAYRTMP(L)*(SW(L)-LL(L))*RLFW
          ENDIF
        ENDIF
        TRWUP = TRWUP+WUP(L)
        IF (RLVTMP(L).LE.0.0) EXIT
      ENDDO

      IF (TRWUP.GT.0.0) THEN
        IF (EOP.GT.0.0) THEN
          WUPR = TRWUP/(EOP*0.1)
        ELSE
          WUPR = 0.0
        ENDIF
        IF (WUPR.GE.WFEU) THEN
          WUF = (EOP*0.1) / TRWUP
        ELSEIF (WUPR.LT.WFEU) THEN
          ! Old function below
          !WUF = 1.0/WFEU + (1.0-1.0/WFEU)*(1.0-(TRWUP/(EOP*0.1*WFEU)))
          WUF = 1.0-(AMAX1(0.0,WUPR-WFEL)/(WFEU-WFEL))*(1.0-1.0/WFEU)
        ENDIF

        TRWU = 0.0
        DO L = 1, NLAYR
          UH2O(L) = WUP(L) * WUF
          TRWU = TRWU + UH2O(L)
        END DO

        IF (WTDEP.GT.0.0.AND.RTDEP.GE.WTDEP) THEN
          TRWU = EOP*0.1
          TRWUP = 20.0*TRWU
        ENDIF

      ELSE        !No root extraction of soil water
        TRWU = 0.0
        DO L = 1,NLAYR
          UH2O(L) = 0.0
        ENDDO
      ENDIF

      RETURN

      END  ! CSCRPROOTWU

!***********************************************************************
!  CSCRPLAYERS Subroutine
!  Leaf distribution module
!-----------------------------------------------------------------------

      SUBROUTINE Cscrplayers
     X (chtpc,clapc,                   ! Height-leaf area distribution
     X plpop,                          ! Plant population
     X lai,canht,                      ! Canopy area indices and height
     X lnumsg,lap,lapp,laps,           ! Leaf cohort number and size
     X LAIL,LAILA,                     ! Leaf area indices by layers
     X LAIA)                           ! Leaf area index,active

      IMPLICIT NONE
      EXTERNAL YVALXY, GETLUN

      INTEGER       clx           ! Canopy layers,maximum          #
      INTEGER       lcx           ! Leaf cohort number,maximum     #
      PARAMETER     (clx=30)      ! Canopy layers,maximum          #
      PARAMETER     (lcx=500)     ! Leaf cohort number,maximum     #

      REAL          caid          ! Canopy area index              m2/m2
      REAL          cailds(5,clx) ! Canopy area index,spp,layer    m2/m2
      REAL          canfrl        ! Canopy fraction,bottom of layr #
      REAL          canht         ! Canopy height                  cm
      REAL          clbase        ! Canopy base layer height       cm
      REAL          clthick       ! Canopy layer thickness         cm
      REAL          clthick1      ! Canopy top layer thickness     cm
      INTEGER       cltot         ! Canopy layer number,total      #
      REAL          chtpc(10)     ! Canopy ht % for lf area        %
      REAL          clapc(10)     ! Canopy lf area % down to ht    %
      REAL          lai           ! Leaf lamina area index         m2/m2
      REAL          laia          ! Leaf lamina area index,active  m2/m2
      REAL          lail(clx)     ! Leaf lamina area index         m2/m2
      REAL          laila(clx)    ! Lf lamina area index,active    m2/m2
      REAL          lailatmp      ! Leaf lamina area,active,temp   m2/m2
      REAL          lailtmp       ! Leaf lamina area,temporary     m2/m2
      REAL          lap(0:lcx)    ! Leaf lamina area,cohort        cm2/p
      REAL          lapp(lcx)     ! Leaf lamina area,infected      cm2/p
      REAL          laps(lcx)     ! Leaf lamina area,senescent     cm2/p
      REAL          lfrltmp       ! Leaves above bottom of layer   fr
      REAL          lfrutmp       ! Leaves above top of layer      fr
      INTEGER       lnumsg        ! Leaf cohort number             #
      REAL          plpop         ! Plant population               #/m2
      INTEGER       spp           ! Species                        #
      !INTEGER       fnumwrk       ! Unit number for work file      #
      INTEGER       tvi1          ! Temporary variable,integer     #
      INTEGER       tvilc         ! Temporary value,lf cohort      #
      REAL          yvalxy        ! Y value from function          #

      !LOGICAL       fopen         ! File status indicator          code

      INTRINSIC     AINT,MOD,INT

      SAVE

      spp = 1
      caid = lai
      lail = 0.0
      laila = 0.0
      laia=0.0                         ! LAI of active leaf

      IF(caid.LE.0.0)RETURN

!      IF (FNUMWRK.LE.0.OR.FNUMWRK.GT.1000) THEN
!        CALL Getlun ('WORK.OUT',fnumwrk)
!        INQUIRE (FILE = 'WORK.OUT',OPENED = fopen)
!        IF (.NOT.fopen) OPEN (UNIT = fnumwrk,FILE = 'WORK.OUT')
!      ENDIF

      lfrutmp=1.0
      clthick=10.0                     ! Starting layer thickness (cm)

  10  CONTINUE
      cltot=INT(AINT(canht/clthick)+1.0)
      clthick1=MOD(canht,clthick)
      IF (clthick1.LE.0.0) THEN
        cltot = cltot - 1
        clthick1 = clthick
      ENDIF

      IF(cltot.GT.clx)THEN             ! Cannot have > clx layers
       clthick=clthick*2.0
       GOTO 10
      ENDIF

      cailds=0.0                       ! Caid by layer for species
      lailtmp=0.0
      lailatmp=0.0

      DO tvi1=cltot,1,-1               ! Do over layers down to soil

       IF (TVI1.EQ.CLTOT) THEN
         clbase = canht - clthick1
       ELSE
         clbase = clbase - clthick
       ENDIF
       canfrl = clbase / canht
       lfrltmp = YVALXY(CHTpc,CLApc,CANFRL)

       cailds(spp,tvi1)=caid*(lfrutmp-lfrltmp)

       DO tvilc=lnumsg,1,-1            ! Do over living cohorts
        IF(tvilc.GT.0)THEN
         lail(tvi1)=lail(tvi1)+lailtmp+
     x   (lap(tvilc)-laps(tvilc))*plpop*0.0001
         laila(tvi1)=laila(tvi1)+lailatmp
         IF ((lap(tvilc)-laps(tvilc)).GT.0.0)
     x    laila(tvi1)=laila(tvi1)+
     x    (lap(tvilc)-laps(tvilc)-lapp(tvilc))*plpop*0.0001

         ! Could adjust above for effect on activity as well as area
         ! ie multiply by a 0-1 factor dependent on pathogen and area
         lailtmp=0.0
         lailatmp=0.0
        ENDIF

        IF(caid.GT.0.AND.
     x   lail(tvi1).GE.cailds(spp,tvi1)*(lai/caid))THEN
          lailtmp=lail(tvi1)-cailds(spp,tvi1)*(lai/caid)
          lailatmp=laila(tvi1)*lailtmp/lail(tvi1)
          lail(tvi1)=lail(tvi1)-lailtmp
          laila(tvi1)=laila(tvi1)-lailatmp
        ENDIF

       ENDDO

       laia = laia + laila(tvi1)

       lfrutmp=lfrltmp

      ENDDO

      RETURN
      END

!-----------------------------------------------------------------------
!  EVAPO Subroutine
!  Calculates evapotranspiration,canopy temperature,resistances
!-----------------------------------------------------------------------

      SUBROUTINE EVAPO(MEEVP,                              !Input,method
     & SRAD, CLOUDS, TMAX, TMIN, TDEW, WINDSP,             !Input,wether
     & ALBEDO, RATM, RCROP,                                !Input,crop
     & EO,                                                 !Input,pot.ev
     & EOPEN, EOMP, EOPT, EOE, TCAN,                       !Output
     & TASKFLAG)                                           !Calculation
      
      ! Should be that MEEVP indicates method, TASKFLAG indicates what 
      ! to be calculated (evap,resistance,canopy temperature).BUT not
      ! running this way 05/12/12 LAH. Needs work!

      IMPLICIT NONE
      EXTERNAL CSVPSAT, VPSLOP

      REAL          TVR1
      REAL          ALBEDO        ! Reflectance of soil-crop surf  fr
      REAL          CLOUDS        ! Relative cloudiness factor,0-1 #
      REAL          CSVPSAT       ! Saturated vapor pressure air   Pa
      REAL          DAIR          ! Density of air
      REAL          EEQ           ! Equilibrium evaporation        mm/d
      REAL          EO            ! Potential evapotranspiration   mm/d
      REAL          EOPT          ! Potential evapot.,Priestly-T   mm/d
      REAL          EOPEN         ! Potential evapot.,Penman       mm/d
      REAL          EOMP          ! Potential evap,Penman-Monteith mm/d
      REAL          EOE           ! Potential evapot.,Energy budgt mm/d
      REAL          VPSAT         ! Vapor pressure of air          Pa
      REAL          G             ! Soil heat flux density         MJ/m2
      REAL          LHVAP         ! Latent head of vaporization    J/kg
      REAL          PATM          ! Pressure of air = 101300.0     Pa
      REAL          PSYCON        ! Psychrometric constant         Pa/K
      REAL          RADB          ! Net outgoing thermal radiation MJ/m2
      REAL          RNET          ! Net radiation                  MJ/m2
      REAL          RNETMG        ! Radiant energy portion,Penman  mm/d
      REAL          RT            ! Gas const*temperature          #
      REAL          S             ! Change of sat vp with temp     Pa/K
      REAL          SBZCON        ! Stefan Boltzmann = 4.093E-9    MJ/m2
      REAL          SHAIR         ! Specific heat of air = 1005.0
      REAL          SLANG         ! Long wave solar radiation      MJ/m2
      REAL          SRAD          ! Solar radiation                MJ/m2
      REAL          TD            ! Approximation,av daily temp    C
      REAL          TDEW          ! Dewpoint temperature           C
      REAL          TK4           ! Temperature to 4th power       K**4
      REAL          TMAX          ! Maximum daily temperature      C
      REAL          TMIN          ! Minimum daily temperature      C
      REAL          VPD           ! Vapor pressure deficit         Pa
      REAL          VPSLOP        ! Sat vapor pressure v temp      Pa/K
      REAL          WFNFAO        ! FAO 24 hour wind function      #
      REAL          WINDSP        ! Wind speed                     m/s
!     INTEGER       FNUMWRK       ! File number,work file          #
!     LOGICAL       FOPEN         ! File open indicator            code
      REAL          emisa
      REAL          TAIR
      REAL          VPAIR
      REAL          emisac
      REAL          DLW           ! Downward long wave radiation   MJ/m2
      REAL          SIGMA
      REAL          RHOA
      REAL          CP
      REAL          hfluxc
      REAL          lefluxc
      REAL          vpaircan
      REAL          ULW
      REAL          RATM
      REAL          RATMSTORE
      REAL          HTVAP
      REAL          APRESS
      REAL          TCAN
      REAL          INPUT
      REAL          OUTPUT
      INTEGER       LOOPS
      REAL          ADDVAR
      REAL          SUBVAR
      REAL          RCROP
      REAL          RSADJ         ! Stomatal res,adjusted Co2+H2o
!     REAL          TVR2
      REAL          TVR3
!     REAL          TVR4
!     REAL          TVR5

      CHARACTER (LEN=1)   MEEVP         ! Evaptr calculation method
      CHARACTER (LEN=1)   TASKFLAG      ! Flag for required task

      INTRINSIC AMAX1,EXP,SQRT,ABS

      SAVE

      PARAMETER     (PATM=101300.0)    !
      PARAMETER     (SBZCON=4.903E-9)  !(MJ/K4/m2/d) fixed constant
      PARAMETER     (SHAIR=1005.0)     ! MJ/kg/K?? or need*10-5

      TCAN = -99.0
      
! FO - 11/20/2020 - Removed unused statement for WORKS.OUT
!      CSYCA uses this subroutine as well.
!      IF (FNUMWRK.LE.0.OR.FNUMWRK.GT.1000) THEN
!        CALL Getlun ('WORK.OUT',fnumwrk)
!        INQUIRE (FILE = 'WORK.OUT',OPENED = fopen)
!        IF (.NOT.fopen) OPEN (UNIT = fnumwrk,FILE = 'WORK.OUT')
!      ENDIF

      RT = 8.314 * ((TMAX+TMIN)*0.5 + 273.0)             ! N.m/mol ??
      VPAIR = CSVPSAT(TDEW)                              ! Pa
      DAIR = 0.1*18.0/RT*((PATM-VPAIR)/0.622+VPAIR)      ! kg/m3
      LHVAP = (2501.0-2.373*(TMAX+TMIN)*0.5)*1000.0      ! J/kg
      PSYCON = SHAIR * PATM / (0.622*LHVAP)              ! Pa/K
      VPSAT = (CSVPSAT(TMAX)+CSVPSAT(TMIN)) / 2.0        ! Pa
      VPD = VPSAT - VPAIR                                ! Pa
      S = (VPSLOP(TMAX)+VPSLOP(TMIN)) / 2.0              ! Pa/K

      ! Default for use when no RATM specified for EO
      IF (TASKFLAG.EQ.'O') THEN
        RCROP = 0.0
        IF (RATMSTORE.LE.0.0) RATMSTORE = 300.0
        RATM = RATMSTORE
        TASKFLAG = 'A'
      ELSE
        RATMSTORE = RATM
      ENDIF

      ! Adjusted resistance
      IF (TASKFLAG.EQ.'R') THEN
         RSADJ =
     &    ((((((S*RNETMG+(DAIR*1.0E-2*SHAIR*1.0E-6*VPD)
     &    /(RATM*1.157407E-05))/
     &    TVR1)-S))/PSYCON)-1)*(RATM*1.157407E-05)
         TVR3 = RSADJ/1.157407E-05
         RETURN
      ENDIF

      IF (TASKFLAG.EQ.'A'.OR.TASKFLAG.EQ.MEEVP) THEN
        ! Penman
        ! Net radiation (MJ/m2/d). RADB constants Jensen et al (1989)
        ! for semi-humid conditions. 0.005 changes 0.158 from kPa to Pa.
        G = 0.0
        TK4 = ((TMAX+273.)**4+(TMIN+273.)**4) / 2.0
        RADB = SBZCON*TK4*(0.4-0.005*SQRT(VPAIR))*(1.1*(1.-CLOUDS)-0.1)
        RNET= (1.0-ALBEDO)*SRAD - RADB
        RNETMG = (RNET-G) / LHVAP * 1.0E6 ! MJ/m2.d to mm/day
        ! Resistance using FAO wind function. Multipliers for WNDFAO are
        ! 1000 times smaller than Jensen et al (1979) to convert VPD Pa
        ! to kPa.
        WFNFAO = 0.0027 * (1.0+0.01*WINDSP)
        EOPEN = (S*RNETMG + PSYCON*WFNFAO*VPD) / (S+PSYCON)
      ENDIF

      ! Monteith-Penman
      IF (TASKFLAG.EQ.'A'.OR.TASKFLAG.EQ.MEEVP) THEN
        EOMP = ((S*RNETMG+(DAIR*1.0E-2*SHAIR*1.0E-6*VPD)
     &   /(RATM*1.157407E-05))/
     &   (S+PSYCON*(1+rcrop/ratm)))
      ENDIF

      ! Priestley-Taylor (so-called!)
      IF (TASKFLAG.EQ.'A'.OR.TASKFLAG.EQ.MEEVP) THEN
        TD = 0.60*TMAX+0.40*TMIN
        SLANG = SRAD*23.923
        EEQ = SLANG*(2.04E-4-1.83E-4*ALBEDO)*(TD+29.0)
        EOPT = EEQ*1.1
        IF (TMAX .GT. 35.0) THEN
          EOPT = EEQ*((TMAX-35.0)*0.05+1.1)
        ELSE IF (TMAX .LT. 5.0) THEN
          EOPT = EEQ*0.01*EXP(0.18*(TMAX+20.0))
        ENDIF
        ! From CSM (Gives slightly different results.Maybe albedo)
        !  TD = 0.60*TMAX+0.40*TMIN
        !  IF (XHLAI .LE. 0.0) THEN
        !   ALBEDO = MSALB
        !  ELSE
        !   ALBEDO = 0.23-(0.23-MSALB)*EXP(-0.75*XHLAI)
        !  ENDIF
        !  SLANG = SRAD*23.923
        !  EEQ = SLANG*(2.04E-4-1.83E-4*ALBEDO)*(TD+29.0)
        !  EO = EEQ*1.1
        !  IF (TMAX .GT. 35.0) THEN
        !   EO = EEQ*((TMAX-35.0)*0.05+1.1)
        !  ELSE IF (TMAX .LT. 5.0) THEN
        !   EO = EEQ*0.01*EXP(0.18*(TMAX+20.0))
        !  ENDIF
        !  EO = MAX(EO,0.0001)
      ENDIF

      ! Energy budget
      IF (TASKFLAG.EQ.'A'.OR.TASKFLAG.EQ.MEEVP.OR.TASKFLAG.EQ.'C') THEN
        tair = (tmax+tmin)/2.0
        sigma=5.673*1.0E-8     ! Stephan Bolzman constant (J m-2k-4s-1)
        !emisa=0.72+0.005*tair
        emisa=0.61+0.05*sqrt(vpair/100)! Emissivity,cloudless Campbell
        emisac=emisa+clouds*(1.-emisa-(8.0/(tair+273.0)))
        dlw=emisac*sigma*(tair+273.0)**4.
        cp=1.005                           ! Specific heat (J g-1 c-1)
        apress=100000.0                    ! Atmospheric pressure (pa)
        rhoa=(3.4838*apress/(tair+273.))   ! Air density  (g/m3)
        htvap=2500.3-2.297*tair            ! Latent ht  J g-1
        dlw = dlw*(60.0*60.0*24.0*1.0E-6)  ! MJ/,2.d <-- J/m2.s
        tcan = tair
        loops = 0
        addvar = 1.0
        subvar = 1.0
 333    CONTINUE
        loops = loops + 1
        vpaircan = CSVPSAT(tcan)                    ! Pa
        hfluxc=(cp*rhoa*(tcan-tair)/(ratm*0.6))     ! Heat J/m2.s
        hfluxc = hfluxc/(1.0E6/(60.0*60.0*24.0))    ! MJ/,2.d <-- J/m2.s
        IF (taskflag.NE.'C') THEN
          ! Calculate leflux (ie.EO}
          lefluxc=(htvap*rhoa*0.622/apress*(vpaircan-vpair)/
     X      (rcrop+ratm*0.6))                       ! H2o,plant J/m2.s
          lefluxc = lefluxc/(1.0E6/(60.0*60.0*24.0))! MJ/,2.d <-- J/m2.s
        ELSEIF (TASKFLAG.EQ.'C') THEN
          ! Use eo brought into routine (Calculate canopy temp)
          lefluxc = eo/1.0E6*lhvap  
        ENDIF
        ulw=sigma*(tcan+273.)**4.   ! Upward long wave radiation
        ulw = ulw/(1.0E6/(60.0*60.0*24.0))  ! MJ/,2.d <-- J/m2.s
        input = (srad+dlw)
        output = ulw + hfluxc + lefluxc
        IF (input.GT.output) THEN
          IF (addvar.GE.1.0) subvar = 0.5
          IF (addvar.GE.0.5.AND.addvar.LT.1.0) subvar = 0.3
          IF (addvar.GE.0.3.AND.addvar.LT.0.5) subvar = 0.2
          IF (addvar.GE.0.2.AND.addvar.LT.0.3) subvar = 0.1
          tcan = tcan + addvar
          IF (loops.LT.20.AND.ABS(input-output).GE.1.0) GO TO 333
        ENDIF
        IF (input.LT.output) THEN
          IF (subvar.GE.1.0) addvar = 0.5
          IF (subvar.GE.0.5.AND.subvar.LT.1.0) addvar = 0.3
          IF (subvar.GE.0.3.AND.subvar.LT.0.5) addvar = 0.2
          IF (subvar.GE.0.2.AND.subvar.LT.0.3) addvar = 0.1
          TCAN = TCAN - subvar
          IF (loops.LT.20.AND.ABS(input-output).GE.1.0) GO TO 333
        ENDIF
        eoe = lefluxc/lhvap*1.0E6
      ENDIF

      RETURN

      END  ! EVAPO


      ! Stuff for exploring temperature responses
      !   DO  L = 1,50
      !    TMEAN = FLOAT(L)
      !    tmax = tmean
      !    tmin = tmean
      !   ! Below is the function for max phs (Pm) for AFRC
      !   PM =
      !  &1.0/3.21447*
      !  &(0.044*6.0*1.0E9*(tmean+273.0)*
      !  &exp(-14200/(1.987*(tmean+273.0))))/
      !  &(1.0+exp(-47000/(1.987*(tmean+273.0)))*exp(153.4/1.987))
      !   ! And below for actual
      !   RA = 30.0  ! s/m
      !   RM = 400.0 ! s/m
      !   VPD = 0.0
      !   QPAR = 600.0
      !   ALPHA = 0.009   ! mg/J
      !   RS = 1.56*75.0*(1.0+100.0/QPAR)*(1.0-0.3*VPD)
      !   ! RS = 234,156,137 s/m at QPAR'S of 100,300,600 (0.0VPD)
      !   RP = RA + RS + RM
      !   PMAX = 0.995*QPAR/RP
      !   PHSA = (0.995/PMAX)*((1.0/(ALPHA*QPAR))+(1.0/PM))
      !   PHSB = -((1.0/(ALPHA*QPAR))+(1.0/PM)+(1.0/PMAX))
      !   PGROSS =( -PHSB - SQRT(PHSB**2-4.0*PHSA))/(2.0*PHSA)
      !   Below is Ceres grain fill
      !   IF (Tmean .GT. 10.0) THEN
      !     RGFILL =
      !      0.65+(0.0787-0.00328*(TMAX-TMIN))*(Tmean-10.)**0.8
      !   ENDIF
      !   IF (Tmean .LE. 10.0) THEN
      !      RGFILL = 0.065*Tmean
      !   ENDIF
      !   Below is AFRC grain fill
      !   rgfill = 0.045*tmean + 0.4
      !   Below is for Swheat
      !   write(1,'(3f10.5)')TMEAN,(0.000913*TMEAN+0.003572),
      !  &  (0.000913*TMEAN+0.003572)/((0.000913*15.0+0.003572))
      !   ENDDO
      !   STOP

      ! Wang and Engel daylength factor algorithm
      ! pps0 =  8.65
      ! IF (DAYL.GT.Pps0) THEN
      !   tvr1 = 4.0/(ppthr-pps0)
      !   DF1(1) =
      !    AMAX1(0.0,AMIN1(1.,1.0 - exp(-tvr1*(dayl-pps0))))
      ! ELSE
      !   DF1(1) = 0.0
      ! ENDIF
      ! df1(2) = df1(1)
      ! df2 = df1(1)

