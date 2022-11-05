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
!=======================================================================

      ! For Cropsim
!     SUBROUTINE XXCER (FILEIOIN, RUN, TN, RN,             !Command line
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
      SUBROUTINE XXCER (FILEIOIN, RUN, TN, RN, RNMODE,     !Command line
     & ISWWAT, ISWNIT, IDETS, IDETO, IDETG, IDETL, FROP,   !Controls
     & SN, ON, RUNI, REP, YEAR, DOY, STEP, CN,             !Run+loop
     & SRAD, TMAX, TMIN, CO2, RAIN, TOTIR,                 !Weather
     & DAYLT, WINDSP, ST, EO,                              !Weather
     & NLAYR, DLAYR, DEPMAX, LL, DUL, SAT, BD, SHF, SLPF,  !Soil states
     & SNOW, SW, NO3LEFT, NH4LEFT,                         !H2o,N states
     & YEARPLT, YEARPLTCSM, HARVFRAC,                      !Pl.date
     & EOP, EP, ET, TRWUP,                                 !Resources
     & LAI, KCAN, KEP,                                     !States
     & RLV, NFP, RWUPM, RWUMX, CANHT,                      !States
     & UNO3ALG, UNH4ALG, UH2O,                             !Uptake
     & SENCALG, SENNALG, SENLGALG,                         !Senescence
     & RESCALG, RESNALG, RESLGALG,                         !Residues
     & STGDOY,                                             !Stage dates
     & WEATHER, SOILPROP, CONTROL,
     & DYNAMIC)                                            !Control

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

      TYPE (ControlType), intent (in) :: CONTROL ! Defined in ModuleDefs
      TYPE (WeatherType), intent (in) :: WEATHER ! Defined in ModuleDefs
      TYPE (SoilType), intent (in) ::   SOILPROP ! Defined in ModuleDefs
    
      INTEGER ADAT10, CSTIMDIF, CSINCDAT, DAPCALC
      INTEGER GSTAGE, HARVFRAC
      INTEGER CN, DOY, DYNAMIC, DYNAMICI, FROP, NLAYR, ON, REP, RN          
      INTEGER RUN, RUNI, SN, STEP, STGDOY(20), TN, YEAR
      INTEGER TVICOLNM
      INTEGER CSYDOY, YEARPLTCSM, YEARPLT, TVILENT
      
      REAL CANHT, EO, EP, EOP, ET, LAI, KCAN, KEP, NFP
      REAL BD(NL), DAYLT, DEPMAX, LL(NL), SAT(NL)
      REAL DLAYR(20), DUL(20), UNO3ALG(20), SENLGALG(0:20), UNH4ALG(20)
      REAL RESWALG(0:20), RESWAL(0:20), RESNAL(0:20), RESLGAL(0:20)
      REAL PARIP
      REAL RESCALG(0:NL), RESLGALG(0:NL), RESNALG(0:NL), RLV(NL)
      REAL RAIN, RWUPM, RWUMX, SLPF, SHF(NL)
      REAL ST(0:NL), SENNALG(0:NL), SENCALG(0:NL), TRWUP, UH2O(NL)
      REAL SW(20), UNH4(NL), UNO3(NL), NO3LEFT(NL), NH4LEFT(NL)
      REAL CO2, TMAX, TMIN, SRAD, WINDSP, SNOW
      REAL TFAC4, TOTIR, YVALXY, YVAL1

      CHARACTER*10  BASTGNAM(20), WHSTGNAM(20)
      CHARACTER*1   IDETG, ISWNIT, ISWWAT, IDETL, IDETO, IDETS
      CHARACTER*1   RNMODE
      CHARACTER*250 FILEIOIN
      CHARACTER*10  TL10FROMI  

      ! Condition at end of phase
      DATA BASTGNAM/'Max Prim  ','End Veg   ','End Ear Gr',
     1              'Bg Gr Fill','End Gr Fil','Harvest   ',
     2              'Sowing    ','Germinate ','Emergence ',
     3              'Failure   ','End Crop  ','          ',
     4              '          ','          ','          ',
     5              '          ','          ','          ',
     6              '          ','          '/

      DATA WHSTGNAM/'Term Spklt','End Veg   ','End Ear Gr',
     1              'Beg Gr Fil','End Gr Fil','Harvest   ',
     2              'Sowing    ','Germinate ','Emergence ',
     3              'Failure   ','End crop  ','          ',
     4              '          ','          ','          ',
     5              '          ','          ','          ',
     6              '          ','          '/

      YEARDOY = YEAR*1000 + DOY

      IF (DYNAMIC.EQ.RUNINIT .OR. DYNAMIC.EQ.SEASINIT) THEN

        CALL CER_Init (LAI, CANHT,
     &     CN, DOY, HARVFRAC, ISWNIT,
     &     FILEIOIN, FROP, IDETL,
     &     ISWWAT, KCAN, KEP, NFP, ON, PARIP,
     &     RESCALG, RESLGALG, RESNALG, RLV, RN, RNMODE,
     &     RUN, RUNI, RWUMX, RWUPM, SENCALG,
     &     UH2O, UNH4, UNO3, YEAR, SENNALG, SLPF, SN,
     &     STGDOY, TN, TRWUP, BASTGNAM, WHSTGNAM, DYNAMIC)
        WRITE(*,*) "PASSOOOIUUU"

      ELSEIF (DYNAMIC.EQ.RATE) THEN
      
          ! Update so that temporary outputs in rate have correct DAP
          DAE = MAX(0,CSTIMDIF(STGDOY(9),YEARDOY))
          DAP = MAX(0,CSTIMDIF(STGDOY(7),YEARDOY))
          DAS = MAX(0,CSTIMDIF(YEARSIM,YEARDOY))
          IF (ISTAGE.EQ.6) DAPM = DAPM + 1
      
        ! LAH 07/12/2008 For problem of date calculation.
        ! YEARPLTCSM established by CSM;brought across in argument list.
        ! LAH 29/06/11 Added automatic as well
        IF (FILEIOT.EQ.'DS4') THEN
!         IF (IPLTI.EQ.'A' .OR. (INDEX('FQN',RNMODE) > 0)) THEN
          IF (IPLTI.EQ.'A' .OR. IPLTI.EQ.'F' .OR. 
     &       (INDEX('FQNY',RNMODE) > 0)) THEN
            YEARPLTP = YEARPLTCSM
          ENDIF  
        ENDIF

        IF (FILEIOT.EQ.'XFL') WRITE(fnumwrk,'(A28,I3,I8,2F6.2)')
     &   ' CN,YEARDOY,XSTAGE1,LEAFNUM ',cn,yeardoy,xstage,lnumsd
     
        IF (YEARDOY.LT.YEARPLTP)
     &  WRITE(fnumwrk,*) 'yeardoy,YEARPLT,YEARPLTP   ',
     &                     yeardoy,YEARPLT,YEARPLTP

        CFLINIT = 'N'    ! Reset initiation flag for next run

        IF (YEARPLT.GT.9000000) THEN            ! If before planting
          ! Initialize planting depth temperature and water variables
          TSDEP = 0.0
          CUMSW = 0.0
          AVGSW = 0.0
          IF(YEARPLTP.GT.0 .AND. YEARPLTP.LT.9000000)THEN
            IF(YEARDOY.EQ.YEARPLTP)THEN
C  FO - 05/07/2020 Add new Y4K subroutine call to convert YRDOY              
              !YEARPLT = CSYEARDOY(YEARPLTP)
              CALL Y4K_DOY(YEARPLTP,FILEIO,0,ERRKEY,3)
              YEARPLT = YEARPLTP
              PLTPOP = PLTPOPP
              TNUM = 1.0
            ENDIF
          ELSE
            ! Automatic planting
            ! Check window for automatic planting,PWDINF<YEARPLTP<PWDINL
            IF (YEARDOY.GE.PWDINF.AND.YEARDOY.LE.PWDINL) THEN
              ! Within planting window.
              ! Determine if soil temperature and soil moisture ok
              ! Obtain soil temperature, TSDEP, at 10 cm depth
              I = 1
              XDEP = 0.0
              DO WHILE (XDEP .LT. 10.0)
                XDEP = XDEP + DLAYR(I)
                TSDEP = ST(I)
                I = I + 1
              END DO
              ! Compute average soil moisture as percent, AVGSW
              I = 1
              XDEP = 0.0
              CUMSW = 0.0
              DO WHILE (XDEP .LT. SWPLTD)
                XDEPL = XDEP
                XDEP = XDEP + DLAYR(I)
                IF (DLAYR(I) .LE. 0.) THEN
                  !IF SOIL DEPTH IS LOWER THAN SWPLTD -US
                  XDEP = SWPLTD
                  CYCLE
                ENDIF
                DTRY = MIN(DLAYR(I),SWPLTD - XDEPL)
                CUMSW = CUMSW + DTRY *
     &           (MAX(SW(I) - LL(I),0.0)) / (DUL(I) - LL(I))
                I = I + 1
              END DO
              AVGSW = (CUMSW / SWPLTD) * 100.0
              WRITE (fnumwrk,*) 'Date thresholds ',pwdinf,pwdinl
              WRITE (fnumwrk,*) 'Water thresholds ',swpltl,swplth
              WRITE (fnumwrk,*) 'Water ',avgsw
              WRITE (fnumwrk,*) 'Temperature thresholds ',pttn,ptx
              WRITE (fnumwrk,*) 'Temperature ',tsdep
              IF (TSDEP .GE. PTTN .AND. TSDEP .LE. PTX) THEN
                IF (AVGSW .GE. SWPLTL .AND. AVGSW .LE. SWPLTH) THEN
                  YEARPLT = YEARDOY
                  PLTPOP = PLTPOPP
                  CFLFAIL = 'N'
                ENDIF
              ENDIF
            ELSE
              IF (YEARDOY.GT.PWDINL) THEN
                CFLFAIL = 'Y'
                STGDOY(1) = -99
                STGDOY(2) = -99
                STGDOY(3) = -99
                STGDOY(4) = -99
                STGDOY(5) = -99
                STGDOY(6) = -99
                STGDOY(7) = -99
                STGDOY(8) = -99
                STGDOY(9) = -99
                STGDOY(10) = YEARDOY
                STGDOY(11) = YEARDOY
                ISTAGE = 7
                XSTAGE = 7.0
                WRITE (fnumwrk,*) ' '
                WRITE (fnumwrk,*)
     &           'Automatic planting failure on ',yeardoy
              ENDIF
            ENDIF
          ENDIF

          IF (YEARDOY.EQ.YEARPLTP) WRITE (fnumwrk,*)
     &      'Planting on: ',yeardoy
          WRITE (fnumwrk,*)
     &      'Initialising soil profile and other N aspects on: ',yeardoy

          STGDOY(7) = YEARPLT
          SEEDN = SEEDNI
          SEEDRS = SEEDRSI
          SEEDRSAV = SEEDRS
                    
          H2OPROFILEI = 0.0
          H2OROOTZONEI = 0.0
          AH2OPROFILEI = 0.0
          AH2OROOTZONEI = 0.0
          SNO3PROFILEI = 0.0
          SNH4PROFILEI = 0.0
          SNO3ROOTZONEI = 0.0
          SNH4ROOTZONEI = 0.0
          DO L = 1, NLAYR
            FAC(L) = 10.0/(BD(L)*DLAYR(L))
            SNO3PROFILEI = SNO3PROFILEI + (NO3LEFT(L)/FAC(L))
            SNH4PROFILEI = SNH4PROFILEI + (NH4LEFT(L) / FAC(L))
            AH2OPROFILEI = AH2OPROFILEI + ((SW(L)-LL(L))*DLAYR(L))*10.0
            H2OPROFILEI = H2OPROFILEI + SW(L)*DLAYR(L)*10.0
            IF (RLV(L).GT.0.0) THEN
              AH2OROOTZONEI = AH2OROOTZONEI+((SW(L)-LL(L))*DLAYR(L))*10.
              H2OROOTZONEI = H2OROOTZONEI+SW(L)*DLAYR(L)*10.
              SNO3ROOTZONEI = SNO3ROOTZONEI + (NO3LEFT(L)/FAC(L))
              SNH4ROOTZONEI = SNH4ROOTZONEI + (NH4LEFT(L)/FAC(L))
            ENDIF
          END DO

        ENDIF

        ! Reset control flag
        !IF (YEARDOY.GE.YEARPLT) DYNAMICI = 0

        IF (YEARDOY.GE.YEARPLT) THEN   

          ! Photosynthetically active radiation
          PARAD = PARADFAC*SRAD

          ! Mean temperature
          TMEAN = (TMAX+TMIN)/2.0
C-GH      IF (snow.GT.0) THEN
          IF (snow .GT. 0.0) THEN
            tmeans = 0.0
          ELSE
            tmeans = tmean
          ENDIF

          ! Day and night temperatures
          TDAY = TMEAN
          TNIGHT = TMEAN
          ! NB.These could be set in various ways. In Ceres 3.5,there
          ! were various modifications for different processes. Work
          ! with G.McMaster, however, showed that (at least for
          ! development) these were no better than using the daily
          ! mean. Hence the day and night temperatures are set equal
          ! to the daily mean. Other simple settings could be:
          ! TDAY = TMAX
          ! TNIGHT = TMIN
          ! TDAY = TMEAN + 0.5*(TMAX-TMEAN)
          ! TNIGHT = TMIN + 0.5*(TMEAN-TMIN)
          ! And more complex settings could involve using the hourly
          ! temperatures, or modifying values depending on the
          ! difference between TMAX and TMIN.
          TMEAN20S = 0.0
          SRAD20S = 0.0
          STRESS20S = 0.0
          TMEANNUM = TMEANNUM + 1.0
          DO L = 20,2,-1
            TMEAND(L) = TMEAND(L-1)
            TMEAN20S = TMEAN20S + TMEAND(L)
            SRADD(L) = SRADD(L-1)
            SRAD20S = SRAD20S + SRADD(L)
            STRESS(L) = STRESS(L-1)
            STRESS20S = STRESS20S + STRESS(L)
          ENDDO
          TMEAND(1) = TMEAN
          TMEAN20S = TMEAN20S + TMEAND(1)
          SRADD(1) = SRAD
          SRAD20S = SRAD20S + SRAD
          STRESS(1) = AMIN1(WFG,NFG)
          STRESS20S = STRESS20S + STRESS(1)
          IF (TMEANNUM.GE.20.0) THEN
            IF (TMEANNUM.EQ.20.0) TMEAN20P = TMEAN20S/20.0
            TMEAN20 = TMEAN20S/20.0
            SRAD20 = SRAD20S/20.0
            STRESS20 = STRESS20S/20.0
          ELSE
            TMEAN20 = 0.0
            SRAD20 = 0.0
            STRESS20 = 0.0
          ENDIF
          IF (ADAT.GT.0) THEN
            ADAT10 = CSINCDAT(ADAT,10)
            !IF (YEARDOY.EQ.ADAT10) TMEAN20A = TMEAN20
            !IF (YEARDOY.EQ.ADAT10) SRAD20A = SRAD20
            IF (XSTAGE.GE.ASTAGEND.AND.TMEAN20A.LE.0.0) THEN
              TMEAN20A = TMEAN20
              SRAD20A = SRAD20
              STRESS20A = STRESS20
            ENDIF
          ENDIF

!  For investigation of temperature responses
!          WRITE(fnumwrk,*)' tmean   tfd   tfg  tfdg    tt'
!          DO 9821 TVI1 = 0,40
!           TMEAN = FLOAT(TVI1)
!           TMEANS = TMEAN
!         End of stuff to investigate temperature responses          
           
          ! Thermal time
          IF (ISTAGE.GT.4 .AND. ADAT.GT.0 .AND. ISTAGE.LE.6) THEN
            Tfout = TFAC4(trdv2,tmean,TT)
          ELSE
            Tfout = TFAC4(trdv1,tmeans,TT)
            TTmax = trdv1(2) - trdv1(1)
            IF (trgem(3).GT.0.0) THEN
              Tfgem = TFAC4(trgem,tmeans,TTGEM)
            ELSE
              Ttgem = tt
            ENDIF    
          ENDIF

          ! Thermal time averages for various periods
          IF (ISTAGE.LT.7.AND.ISTAGE.GE.4) THEN
            TT20S = 0.0
            TTNUM = TTNUM + 1
            DO L = 20,2,-1
              TTD(L) = TTD(L-1)
              TT20S = TT20S + TTD(L)
            ENDDO
            TTD(1) = TT
            TT20S = TT20S + TTD(1)
            IF (TTNUM.GE.20.0) THEN
              TT20 = TT20S/20.0
            ELSE
              TT20 = -99.0
            ENDIF
          ENDIF

          ! Temperature factors
          IF (ISTAGE.GE.9 .OR. ISTAGE.LE.2) THEN
            Tfv = TFAC4(trvrn,tmeans,TTOUT)
            Tfh = TFAC4(trlth,tmeans,TTOUT)
          ELSE
            TFV = 0.0
            TFH = 0.0
          ENDIF
          !IF (ISTAGE.LE.2) THEN
            Tfg = TFAC4(trlfg,tmean,TTOUT)
          !ENDIF
          IF (ISTAGE.LE.6) THEN
            ! The original temperature response for photosynthesis had
            ! a more rapid rise at low temperatures than the 4 cardinal
            ! temperatures response now used. This is shown below in the
            ! original data:
            !TREFS  TFGR  TFPR  TFVR  TFHR TFGFR TFGNR TFGNM
            ! -5.0   -99   -99  0.00  0.00   -99   -99   -99
            !  0.0  0.00  0.00  1.00  1.00  0.00  0.00  0.00
            !  2.0   -99  0.40   -99   -99   -99   -99   -99
            !  5.0   -99   -99   -99  1.00   -99   -99   -99
            !  7.0   -99  0.70  1.00   -99   -99   -99   -99
            ! 10.0  1.00  0.85   -99  0.00   -99   -99   -99
            ! 15.0  1.00  1.00  0.00   -99   -99   -99   -99
            ! 16.0   -99   -99   -99   -99  1.00  1.00  1.00
            ! 20.0  1.00  1.00   -99   -99   -99   -99   -99
            ! 25.0  1.00   -99   -99   -99   -99   -99   -99
            ! 26.0   -99  0.85   -99   -99   -99   -99   -99
            ! 30.0   -99  0.50   -99   -99   -99   -99   -99
            ! 35.0  0.00  0.00   -99   -99  1.00  1.00  1.00
            ! 45.0   -99   -99   -99   -99  1.00  1.00  1.00
            ! The original call to obtain TFP was:
            ! TFP = TFCALC2(TREFS,TFPR,'20',TDAY,TDAY,SNOW)
            Tfp = TFAC4(trphs,tmean,TTOUT)
            ! Ceres35 PRFT = 1.0-0.0025*((0.25*TMIN+0.75*TMAX)-18.0)**2
          ENDIF
          IF (ISTAGE.EQ.4.OR.ISTAGE.EQ.5) THEN
            Tfgf = TFAC4(trgfw,tmean,TTOUT)
            Tfgn = TFAC4(trgfn,tmean,TTOUT)
          ENDIF
!         To investigate temperature responses          
!          write(fnumwrk,'(5f6.2)')tmean,tfout,tfg,tfout*tfg,tt 
!9821      continue
!          write(fnumwrk,*)'! ttmax = ',trdv1(2)-trdv1(1)
!          stop
!         End of stuff to investigate temperature responses          
          ! Radiation interception (if from competition model)
          IF (PARIP.GE.0.0) THEN
            PARI = PARIP/100.0
            WRITE(fnumwrk,'(A39,F6.2,A11,I2)')
     &       ' PARI from competition model          :',PARI,
     &       ' Component:',CN
            WRITE(fnumwrk,'(A39,F6.2,7X,F6.2)')
     &       ' Leaf area (laminae). Index,Per plant: ',
     &       LAI,PLA-SENLA
          ENDIF

          IF (fileiot(1:2).NE.'DS')
     &     CALL CSTRANS(ISWWAT,                            !Control
     &     TMAX, TMIN, WINDSP, CO2, EO,                    !Weather
     &     CROP, LAI, KEP,                                 !Crop,LAI
     &     eop,                                            !Pot.pl.evap
     &     DYNAMICI)                                       !Control

          IF (fileiot(1:2).NE.'DS')
     &     CALL CSROOTWU(ISWWAT,                           !Control
     &     NLAYR, DLAYR, LL, SAT,                          !Soil
     &     EOP,                                            !Pot.evap.
     &     RLV, RWUPM, RWUMX,                              !Crop state
     &     SW,                                             !Soil h2o
     &     uh2o, trwup,                                    !H2o uptake
     &     DYNAMICI)                                       !Control

          ! Water status factors
          WFG = 1.0
          WFP = 1.0
          WFT = 1.0
          IF (ISWWAT.EQ.'Y' .AND. ISTAGE.LT.7) THEN
            IF (EOP.GT.0.0) THEN
              WUPR = TRWUP/(EOP*0.1)
              WFG = AMAX1(0.0,AMIN1(1.0,WUPR/WFGU))
              WFP = AMAX1(0.0,AMIN1(1.0,WUPR/WFPU))
              IF (XSTAGE.GE.4.0) WFP = 1.0-(1.0-WFP)*WFPGF !Grain fill
              WFT = AMAX1(0.0,AMIN1(1.0,(WUPR-WFTL)/(WFTU-WFTL)))
            ENDIF
          ENDIF

          ! Nitrogen status factors
          NFG = 1.0
          NFP = 1.0
          NFT = 1.0
          IF (ISWNIT.EQ.'Y' .AND. ISTAGE.LT.7) THEN
            LMNCG = LMNC + NFGL * (LCNC-LMNC)
            LCNCG = LMNC + NFGU * (LCNC-LMNC)
            LMNCP = LMNC + NFPL * (LCNC-LMNC)
            LCNCP = LMNC + NFPU * (LCNC-LMNC)
            LCNCSEN = LMNC + NFSU * (LCNC-LMNC)
            LMNCT = LMNC + NFTL * (LCNC-LMNC)
            LCNCT = LMNC + NFTU * (LCNC-LMNC)
            IF (LFWT.GT.0.0) THEN
              NFG = AMIN1(1.0,AMAX1(0.0,(LANC-LMNCG)/(LCNCG-LMNCG)))
              NFP = AMIN1(1.0,AMAX1(0.0,(LANC-LMNCP)/(LCNCP-LMNCP)))
              NFT = AMIN1(1.0,AMAX1(0.0,(LANC-LMNCT)/(LCNCT-LMNCT)))
            ENDIF
          ENDIF
          
          ! Daylength factor. 
          IF (ISTAGE.GE.7) THEN
            DF = PPFPE
          ELSEIF (ISTAGE.GE.1) THEN
            IF (P1D.GE.0.0) THEN      ! Long day plants
              ! Below is a possible age adjusted P1D. But not used
              P1DA=AMAX1(.0,(P1D/10000)-(P1D/10000)*P1DAFAC*(LNUMSD-5.))
              !P1DAFAC is an age adjustment factor
              DF = AMAX1(0.0,AMIN1(1.0,1.0-(P1D/10000)*(P1DT-DAYLT)**2))
            ELSE                      ! Short day plants
              DF = 
     &         AMAX1(0.0,AMIN1(1.0,1.0-(ABS(P1D)/1000)*(DAYLT-P1DT)))
            ENDIF
          ENDIF  
          
          IF (RSTAGE.GT.PPEND .AND.ISTAGE.LT.7) THEN
            DF = 1.0
          ENDIF
          
          ! Light intensity factor
          LIF2 = 1.0
          IF (CROP.EQ.'BA' .AND. SRAD.LE.10.0) THEN
            LIF2 = 1.0 - ( 10.0-SRAD)**2/PLTPOP
          ENDIF
          
          ! Developmental units
          DU = TT*VF*DF*LIF2    ! NB. Changed from Ceres 3.5
          ! DU = TT*AMIN1(VF,DF)*LIF2    ! Ceres 3.5
          DU = AMAX1(0.0, DU)
          
          ! Water factor for germination
          IF (ISWWAT.EQ.'Y' .AND. ISTAGE.GT.7) THEN
            DO L = 1, NLAYR
              CUMDEP = CUMDEP + DLAYR(L)
             IF (SDEPTH.LT.CUMDEP) GO TO 100
            END DO
  100       CONTINUE
            L0 = L                       ! L0 is layer with seed
            DO L = 1,NLAYR
              SWP(L) = 
     &         AMIN1(1.0,AMAX1(.0,((SW(L)-LL(L))/(DUL(L)-LL(L)))))
            ENDDO
            ! LAH Changed after query by Hong via GH
            !SWP(0) = AMIN1(1.0,AMAX1(0.0,(SWP(1)-(SWP(2)-SWP(1)))))
            !SWP(0) is a value at the soil surface
            SWP(0) = AMIN1(1.0,AMAX1(0.0,(SWP(1)-0.5*(SWP(2)-SWP(1)))))
            IF (L0.GT.1) THEN
              SWPSD = SWP(L0)
            ELSE
              ! LAH Changed after query by Hong via GH
              ! SDEPTH*(SWP(2)-SWP(0))))
              SWPSD = SWP(0) + (SDEPTH/DLAYR(1))*(SWP(2)-SWP(0))
            ENDIF
            WFGE = AMAX1(0.0,AMIN1(1.0,(SWPSD/WFGEU)))
          ELSE
            WFGE = 1.0
          ENDIF
          
          ! Germination units
          !GEU = TT*WFGE
          GEU = TTGEM*WFGE
          
          ! Initializations for Growth calculations
          CARBO = 0.0
          GROST = 0.0
          GROLF = 0.0
          GROLFP = 0.0
          GROGRP = 0.0
          GROGRPA = 0.0
          GROGRST = 0.0
          GRORS = 0.0
          GRORSSD = 0.0
          GRORSPM = 0.0
          GRORSP = 0.0
          GROST = 0.0
          GROSTP = 0.0
          PCARB = 0.0
          PLAS = 0.0
          PLASS = 0.0
          PLTLOSS = 0.0
          RSFR = 0.0
          RTRESP = 0.0
          RTWTG = 0.0
          RTWTGRS = 0.0
          TNUMD = 0.0
          TNUMG = 0.0
          TNUMLOSS = 0.0
          DO L = 1,2
            PLAG(L) = 0.0
            PLAGT(L) = 0.0
            PLAGTP(L) = 0.0
          ENDDO
          XSTAGEFS = 0.0
          
          ! 'Light intensity' factor (0.6,1.0 at SRAD's of 0,7)
          LIF1 = 1.0
          IF (CROP.EQ.'BA' .AND. SRAD.LE.10.0) THEN
            LIF1 = 1.0 - ((10.0-SRAD)**2*PLTPOP*0.000025)
          ENDIF
          
          ! CO2 factor
          CO2FP = YVALXY(CO2RF,CO2F,CO2)
          
          ! Temperature factors
          ! Must check. One cold night --> no phs next day!!
          
          ! Tops partition fraction; standard first,then adjustment
          IF (PTFS(ISTAGE+1).GT.0)
     &     PTFSS = PTFS(ISTAGE) + (PTFS(ISTAGE+1)-PTFS(ISTAGE))*SSTAGE
          IF (PTFA(ISTAGE).GT.0) THEN
           PTF = AMIN1(PTFX,PTFSS + PTFA(ISTAGE)*AMIN1(WFP,NFG,LIF1))
          ELSE
           PTF = AMIN1(PTFX,PTFSS)
          ENDIF
          
          ! Within tops distribution fractions
          LFFR = 1.0
          STFRSTG = 0.0
          STFRSTG = (STFR(ISTAGE)
     &       + ((STFR(ISTAGE+1)-STFR(ISTAGE))*(XSTAGE-FLOAT(ISTAGE))))
          IF (ISTAGE.GE.3.AND.ISTAGE.LT.7) STFRSTG = 1.0
          IF (ISTAGE.GT.6) STFRSTG = 0.0
          LFFR = 1.0 - STFRSTG                                          
          IF (XSTAGE.GT.P4SGE) THEN
            RSFR = 1.0
          ELSE
            RSFR = RSPCS/100.0
          ENDIF
          
          ! CO2 Assimilation
          PCARB = PARUV * PARAD/PLTPOP * PARI
          IF (XSTAGE.GE.3.0) THEN
            IF (PARUR.GT.0.0) THEN
             PCARB = PARUR * PARAD/PLTPOP * PARI
            ELSE
             PCARB = PARUV * PARAD/PLTPOP * PARI
            ENDIF  
          ENDIF
          
          ! PCARB = 7.5 * PARAD**0.6/PLTPOP*(1.0-EXP(-0.85*LAI)) !Ceres3
          ! Y1 = 1.5 - 0.768 * ((ROWSPC * 0.01)**2 * PLTPOP)**0.1
          ! PCARB = 1.48 * SRAD/PLTPOP * (1.0 - EXP(-Y1 * LAI)) ! Maize
          ! Following is as in Ceres 3.5. Now eliminated minimum choice
          ! CARBO = AMAX1(0.0,PCARB*CO2FP*TFP*AMIN1(WFP,NFP)*RSFP)
          
          ! LAH 07/08/2008 Added SLPF to match other models.
          !CARBO = AMAX1(0.0,PCARB*CO2FP*TFP*WFP*NFP*RSFP)
          CARBO = AMAX1(0.0,PCARB*CO2FP*TFP*WFP*NFP*RSFP*SLPF)
          ! Following is to stop assim once mature
          IF (XSTAGE.GT.6.0) CARBO = AMAX1(0.0,CARBO*(6.3-XSTAGE)/0.3)
          
          ! Available carbohydrate for growth
          IF (ISTAGE.EQ.6) THEN
            CARBOAT = 0.0
            CARBOR = 0.0
            CARBOASD = 0.0
            CARBOAPM = CARBO
          ELSE
            ! Can mobilize more than required! 
            ! Hence CH2O reserves may increase
            ! SDAVFR operates for both tops and roots
            IF (SEEDRSAV.GT.1.0E-9) THEN
              CARBOASD = SDAFR*(TT/STDAY)*SEEDRSAV
            ELSE
              CARBOASD = 0.0 
            ENDIF  
            SEEDRSAV = SEEDRSAV - CARBOASD
            CARBOAT = CARBOASD+CARBO*PTF
            CARBOR = CARBO*(1.0-PTF)
            CARBOAPM = 0.0
          ENDIF

          IF (XSTAGE.GE.LAFST.AND.XSTAGE.LT.7.0) THEN
            IF(LNUMSG.GT.0 .AND. LNSWITCH.LE.0.0) THEN
              LNSWITCH = LNUMSD
              WRITE(fnumwrk,*)' '
              WRITE(fnumwrk,*)
     &         'Leaf number when size increment changed ',lnswitch
              LASWITCH = lapot(lnumsg)
              WRITE(fnumwrk,*)
     &         'Leaf p.size when size increment changed ',Laswitch
               WRITE(fnumwrk,*)
     &         'Next p.size when size increment changed ',
     &          Lapot(lnumsg+1)
            ENDIF
          ENDIF   
          
          IF (LNUMSG.GT.0) THEN
            IF (LNUMSG.GT.0.AND.LNUMSG.LT.LNUMX) THEN
              IF (LNSWITCH.LE.0.0) THEN
                LAPOT(LNUMSG+1) = 
     &            AMAX1(LAPOT(1),LAPOT(LNUMSG))*(1.0+LAFV)
              ELSE  
                LAPOT(LNUMSG+1) = LAPOT(LNUMSG)*(1.0+LAFR)
              ENDIF
              IF (LAPOT(LNUMSG+1).GT.LAXS) LAPOT(LNUMSG+1) = LAXS
              ! NB. In Ceres 3.5 LAPOT(n) was LAPOT(1)*(LNUMSD**0.5) 
            ENDIF    
          ENDIF
          
          ! Growth potentials
          IF (ISTAGE.LE.2) THEN
            LAW = AMAX1(LAWS*LAWFRMN,LAWS-(LAWS*LAWCF)*(LNUMSG-1))
            ! LAW=Leaf area/weight (specific leaf area).Chages with lf #
            IF (LNUMSG.GT.0) THEN
              ! In Ceres overall temperature response for lf growth was:
              ! EGFT = 1.2 - 0.0042*(TEMPM-17.0)**2 
              ! Here, temperature response is a composite of temp response
              ! of development (leaf # increase) and leaf expansion.
              ! So, EGFT would equal TFD*TFG 
              ! Assimilates may control expansion if no reserve available
              ! Current leaf expands completely at current phint
              ! Leaves expand for 1 PHINT
              ! For leaf area growth (PLAG) Ceres 3.5 used: 
              !  PLAG(1) = LA1S * (LNUMSD**0.5) * ....
              ! (with LA1S = 7.5 (= LAPOT(1),potential area of leaf 1)
              PLAG(1) = LAPOT(LNUMSG) * AMIN1(WFG,NFG) * TFG *
     &         AMIN1(TT/PHINT,(FLOAT(LNUMSG)-LNUMSD)) 
              ! NB. Temp response of development (TT) taken into acount 
              ! If new leaf. Will expand at current or new phint
              IF ((TT/PHINT).GT.(FLOAT(LNUMSG)-LNUMSD)) THEN
                ! If new leaf will be first leaf in 3rd phint phase
                IF (LNUMSD.LE.(PHINTL(1)+PHINTL(2)).AND.
     &              LNUMSD+(TT/PHINT).GT.(PHINTL(1)+PHINTL(2))) THEN
                    TTTMP = TT - PHINT*(FLOAT(LNUMSG)-LNUMSD)
                    PLAG(2) = LAPOT(lnumsg+1) * AMIN1(WFG,NFG) * TFG *
     &                        TTTMP/(PHINTS*PHINTF(3))
                ! If new leaf will be first in 2nd phint phase
                ELSEIF (LNUMSD.LE.PHINTL(1).AND.
     &                  LNUMSD+(TT/PHINT).GT.PHINTL(1)) THEN
                  TTTMP = TT - PHINT*(FLOAT(LNUMSG)-LNUMSD)
                  PLAG(2) = LAPOT(lnumsg+1) * AMIN1(WFG,NFG) * TFG *
     &             TTTMP/(PHINTS)
                ELSE
                ! New leaf in current phint phase
                  PLAG(2) = LAPOT(lnumsg+1) * AMIN1(WFG,NFG) * TFG *
     &            (TT/PHINT-(FLOAT(LNUMSG)-LNUMSD))
                ENDIF
              ! No new leaf
              ELSE
                PLAG(2) = 0.0
              ENDIF
              IF (TT.GT.0.0) THEN
                WFLFNUM(LNUMSG) = WFLFNUM(LNUMSG)+1.0
                WFLFSUM(LNUMSG) = WFLFSUM(LNUMSG)+WFG
                NFLFSUM(LNUMSG) = NFLFSUM(LNUMSG)+NFG
                TFLFSUM(LNUMSG) = TFLFSUM(LNUMSG)+TFG
                WFLF(LNUMSG) = WFLFSUM(LNUMSG)/WFLFNUM(LNUMSG)
                NFLF(LNUMSG) = NFLFSUM(LNUMSG)/WFLFNUM(LNUMSG)
                TFLF(LNUMSG) = TFLFSUM(LNUMSG)/WFLFNUM(LNUMSG)
              ENDIF
              PLAGTP(1) = PLAG(1)
              PLAGTP(2) = PLAG(2)
              DO L = 1,INT(TNUM)
                ! LAH MAY 2013 CHECK THIS ... PLAG SHOULD BE PLAGTP??
                IF (L.LT.20) THEN
                  PLAGTP(1) = PLAGTP(1) + PLAG(1)*LATFR(L+1)
     &                      * AMAX1(0.0,AMIN1(1.0,(TNUM-1.0)))
                  PLAGTP(2) = PLAGTP(2) + PLAG(2)*LATFR(L+1)
     &                      * AMAX1(0.0,AMIN1(1.0,(TNUM-1.0)))
                ELSE
                  TNUMOUT = TNUMOUT + 1
                  IF (TNUMOUT.LT.2)
     &             WRITE(fnumwrk,*)'Tiller number at limit of 20! '
                ENDIF
              ENDDO
            ENDIF
          ENDIF
          
          IF (LAW.GT.0.0) 
     &      GROLFP = ((PLAGTP(1)+PLAGTP(2))/LAW)/(1.0-LSHFR)          
          IF (LFFR.GT.0.0) THEN
            GROSTP = GROLFP*(STFRSTG/LFFR)
          ELSE
            GROSTP = CARBOAT*(1.0-RSFR)
          ENDIF  
          GRORSP = CARBOAT*RSFR
          IF (ISTAGE.EQ.4.OR.ISTAGE.EQ.5) THEN
            GROGRP = AMAX1(0.0,LAGSTAGE*TFGF*GRNUM*G2*DU*0.001)
            IF (LAGSTAGE.GT.0.0.AND.TFGF.LT.1.0) THEN
              WRITE(fnumwrk,'(A44,F6.2)')
     &         ' Temperature limit on grain growth at xstage',xstage
              TLIMIT = TLIMIT+1
            ENDIF
          ENDIF
          
          ! Actual growth
          GROLF = AMIN1(GROLFP,CARBOAT*LFFR)
                  ! From CSCRP
                  ! Leaf weight increase from plant reserves
                  GROLFRS = 0.0
                  IF (GROLF.LT.GROLFP) THEN
                    GROLFRS = AMIN1(RSWT*RSUSE,GROLFP-GROLF)
                    GROLF = GROLF+GROLFRS
                  ENDIF
          IF (LNUMSG.GT.0.AND.GROLFP.GT.0.0) THEN
            AFLFSUM(LNUMSG) = AFLFSUM(LNUMSG)
     &       + AMIN1(1.0,((CARBOAT*LFFR)/GROLFP))            
          ENDIF
          IF (LNUMSG.GT.0.AND.WFLFNUM(LNUMSG).GT.0.0)
     &     AFLF(LNUMSG) = AFLFSUM(LNUMSG)/WFLFNUM(LNUMSG)
          
          GROST = AMIN1(GROSTP,CARBOAT*STFRSTG*(1.0-RSFR))
          
          IF (GROGRP.GT.GRORSP+RSWT) THEN
            GROGRST = 0.0
            IF (GROST.GT.0.0) THEN
              GROGRST = AMIN1(GROST,GROGRP-(GRORSP+RSWT))
              IF (GROGRST.GT.0.0) THEN
                WRITE(fnumwrk,*)'CH2O destined for stem used for grain'
              ENDIF
            ENDIF
            IF (GROGRP.GT.GRORSP+RSWT+GROGRST) THEN
              WRITE(fnumwrk,*)'CH2O limit on grain growth.'
              CH2OLIM = CH2OLIM+1
              if (grnum > 1.e-6) then
              WRITE(fnumwrk,'(A15,F6.2,A5,F6.3,A10)') ' CH2O shortage:',
     &         (GROGRP-(GRORSP+RSWT+GROGRST)),' g/p ',
     &         (GROGRP-(GRORSP+RSWT+GROGRST))/GRNUM,' g/kernel '
              endif
            ENDIF
          ENDIF
          GROGRPA = AMIN1(GROGRP,GRORSP+RSWT+GROGRST)
          
          GRORSSD = -(CARBOASD-AMAX1(.0,CARBOASD-GROLF-GROST-GROGRPA))
          GRORS = AMAX1(-RSWT,
     &     (CARBOAT-CARBOASD-GRORSSD)-GROLF-GROST-GROGRPA)
     

              ! Reserves to ROOT if conc too great (overflow!)
              RTWTGRS = 0.0
              ! Determine potential new concentration
              ! NB. Chaff is simply a part of stem;hence not separate here

!----------------------------------------------------------------
! palderman commit of 2019-07-29 in private repo e5c680514bce4af85d9f463c11a3e5ad522252f8
!              IF (LFWT+GROLF+STWT+GROST.GT.0.0) TVR1 = ! Conc
!     &          (RSWT+GRORS-SENRS)/
!     &          ((LFWT+GROLF-SENLFG-SENLFGRS)
!     &          +(STWT+GROST)+(RSWT+GRORS))

              IF (((LFWT+GROLF-SENLFG-SENLFGRS) ! Prevent divide by zero
     &              +(STWT+GROST)+(RSWT+GRORS)) .GT. 0.0)THEN
                  TVR1 = ! Conc
     &              (RSWT+GRORS-SENRS)/
     &              ((LFWT+GROLF-SENLFG-SENLFGRS)
     &              +(STWT+GROST)+(RSWT+GRORS))
              ELSE
                  TVR1 = 0.0
              END IF
!----------------------------------------------------------------

              IF(TVR1.LT.0.0.AND.TVR1.GT.-1.0E-07) TVR1 = 0.0
              IF (TVR1.GT.RSPCX/100.0) THEN   ! If potential>max        
                TVR2 = RSWT+GRORS-SENRS       ! What rswt could be
                TVR3 =                        ! What rswt should be 
     &           ((RSPCX/100.0)
     &           *(LFWT+GROLF-SENLFG-SENLFGRS
     &           +STWT+GROST))
     &           /(1.0-(RSPCX/100.0))
                RTWTGRS = (TVR2 - TVR3) 
                ! Determine FINAL new concentration
                IF (LFWT+GROLF+STWT+GROST.GT.0.0) TVR5 = 
     &            (RSWT+GRORS-SENRS-RTWTGRS)/
     &            ((LFWT+GROLF-SENLFG-SENLFGRS)
     &            +(STWT+GROST)
     &            +(RSWT+GRORS-SENRS-RTWTGRS))
              ENDIF
     
          GRORSPM = CARBOAPM
          
          IF (PLAGTP(1)+PLAGTP(2).GT.0.0) THEN
            PLAGT(1) = GROLF*(1.0-LSHFR)*LAW*
     &       (PLAGTP(1)/(PLAGTP(1)+PLAGTP(2)))
            PLAGT(2) = GROLF*(1.0-LSHFR)*LAW*
     &       (PLAGTP(2)/(PLAGTP(1)+PLAGTP(2)))
          ENDIF
          DO L = 1,2
           IF(PLAGTP(1)+PLAGTP(2).GT.0.0)
     &       PLAG(L) = PLAG(L)*(PLAGT(1)+PLAGT(2))/(PLAGTP(1)+PLAGTP(2))
          ENDDO
          
          ! Growth adjusted for respiration
          RTWTG = AMAX1(0.0,(CARBOR+RTWTGRS)*(1.0-RRESP)) ! LAH 280211 
         
          ! Leaf senescence
          IF (LNUMSG.GT.0.AND.LNUMSG.GT.LLIFE) THEN
            PLASTMP = AMAX1(0.0,LAP(LNUMSG-LLIFE)*TT/PHINT)
            ! Senesces over 1 PHINT
            ! Senescence cannot be greater than area remaining for
            ! senescing leaf. May have been killed by cold,etc..
            LASENLF = AMAX1(0.0,LAP(LNUMSG-LLIFE)-LAPS(LNUMSG-LLIFE))
            PLASTMP = AMIN1(LASENLF,PLASTMP)
          ELSE
            PLASTMP = 0.0
          ENDIF
          
          PLAS = 0.0
          IF (ISTAGE.EQ.1) THEN
            PLAS = PLASTMP          ! Dependent on leaf longevity
          ELSEIF (ISTAGE.EQ.2) THEN
            PLAS = AMIN1(PLASTMP,   ! Dependent on input coefficient 
     &       PLASF(ISTAGE)*GPLA(ISTAGE-1)*DU/PD(ISTAGE))
          ELSEIF (ISTAGE.EQ.3.OR.ISTAGE.EQ.4.OR.ISTAGE.EQ.5) THEN
            ! Determine if N shortage triggers final senescence
            IF (ISWNIT.NE.'N'.AND.XSTAGE.GT.5.0.AND.LCNF.LT.NFSF) THEN
              XSTAGEFS = XSTAGE
              GPLASENF = AMAX1(0.0,PLA-SENLA)
            ENDIF
            ! Calculate leaf area senesced
            IF (XSTAGE.GT.5.0.AND.
     &        XSTAGEFS.GT.0.0.AND.XSTAGE.LT.LSENS) THEN
              PLAS = GPLASENF*(XSTAGE-XSTAGEFS)/(LSENE-XSTAGEFS)
            ELSE
              IF (XSTAGE.GT.LSENS) THEN
                IF (GPLASENS.LT.0.0) GPLASENS = AMAX1(0.0,PLA-SENLA)
                ! NB. Leaf senescence ends at stage LSENE (6.?)
                PLAS = GPLASENS*(XSTAGE-XSTAGEP)/(LSENE-LSENS)
              ELSE
                PLAS = PLASF(ISTAGE) * GPLA(ISTAGE-1)*DU/PD(ISTAGE)
              ENDIF
            ENDIF
          ELSEIF (ISTAGE.EQ.6) THEN 
            ! Originally senesced over 10 standard days after stage 6
            !PLAS = GPLA(ISTAGE-1)*TT/20.0*0.1
            ! Following is to use LSPHE
            PLAS = GPLASENS*(XSTAGE-XSTAGEP)/(LSENE-LSENS)
          ENDIF
          
          ! Increased senescence if reserves fall too low
          ! NB. 3% of green leaf area senesces ... must check
          IF (RSCD.LT.0.10 .AND. ISTAGE.GE.4. AND. PLA.GT.0.0) THEN
           PLAS = PLAS + AMAX1(0.0,0.03*(PLA-PLAS-SENLA))
           WRITE(fnumwrk,'(A52,I4)')
     &      ' Senescence accelerated because low reserves on day:',doy
          ENDIF
          
          ! Overall check to restrict senescence to what available
          PLAS = AMAX1(0.0,AMIN1(PLAS,PLA-SENLA))
          
          ! Tillering
          IF (GROLFP+GROSTP.GT.0.0) THEN
            TNUMAFAC = AMIN1(1.0,(GROLF+GROST)/(GROLFP+GROSTP))
          ELSE
            TNUMAFAC = 0.0
          ENDIF
          IF (LNUMSD.GE.TI1LF) THEN
            ! LAH SEPT 2009  Maybe introduce TILPE and make variable
            !TILPE = 2.0   ! Had been changed to 3.0
            IF (XSTAGE.LT.TILPE) THEN
              IF (LNUMSD.LT.ti1lf+3) THEN    ! Fibonacci factors
                tnumiff=1.0
              ELSEIF(LNUMSD.GE.ti1lf+3 .AND. LNUMSD.LT.ti1lf+4) THEN
                tnumiff=1.5
              ! PUT = 1.5 TO MATCH ADJUSTMENT IN CSCRP  
              ELSEIF(LNUMSD.GE.ti1lf+4 .AND. LNUMSD.LT.ti1lf+5) THEN
                tnumiff=1.5  ! 3.0
              ELSEIF(LNUMSD.GE.ti1lf+5 .AND. LNUMSD.LT.ti1lf+6) THEN
                tnumiff=1.5  ! 4.0
              ELSEIF(LNUMSD.GE.ti1lf+6 .AND. LNUMSD.LT.ti1lf+7) THEN
                tnumiff=1.5  ! 6.0
              ENDIF
              ! CHANGED BACK TO USING AMIN1 FUNCTION TO MATCH CSCRP
              TNUMG = TT/PHINT * TNUMIFF * AMIN1(WFT,NFT,LIF1)
              !TNUMG = TT/PHINT * TNUMIFF * WFT*NFT*LIF1
              ! TAKEN OUT TO MATCH CSCRP
              ! IF (LNUMSD.GT.TI1LF+3) TNUMG = TNUMG * TNUMAFAC
            ENDIF
          ELSE
            TNUMG = 0.0
          ENDIF
          
          ! TIFAC = Tillering rate factor (multiplier) 
          TNUMG = TNUMG * TIFAC
          
          ! Tiller death
          RTSW = 1.0
          TILWT = 0.0
          TILSW = G3 * CUMDU/PTH(5)
          IF (TNUM.GT.0.0) TILWT = (LFWT+STWT+RSWT+GRWT)/TNUM
          IF (TILSW.GT.0.0) RTSW = TILWT/TILSW
          !TILDS = 2.0     ! Ceres 3.5 = 2.4
          !TILDE = 4.0     ! Ceres 3.5 = 5.8
          IF (XSTAGE.GE.TILDS .AND. XSTAGE.LT.TILDE)
     &     TNUMD = 
     &       AMAX1(0.0,(TNUM-1.0)*(1.0-RTSW)*(TT/20.0)*(TILDF/100.0))
             ! 20.0 is to change TT to standard days          
          ! Root respiration
          RTRESP = (CARBOR+RTWTGRS) * RRESP
          
          XSTAGEP = XSTAGE

          ! Canopy height                                               
          IF (XSTAGE.LT.7) THEN
            IF (XSTAGE.GT.1.0) THEN
              CANHTG =
     &         AMAX1(0.0,(CANHTS*AMIN1(1.0,(XSTAGE-1.0)/4.0)-CANHT))
            ELSEIF (XSTAGE.EQ.1.0 .AND. PLAGT(1).GT.0.0) THEN
              ! Height growth on day of emergence or if no development
              CANHTG = 0.5
            ENDIF
          ENDIF
          
          ! Root growth initializations
          RTWTGS = 0.0
          DO L = 1, NLAYR
            RTWTGL(L) = 0.0
            RTWTSL(L) = 0.0
            RTNSL(L) = 0.0
          ENDDO
      
          ! Root growth calculations
          IF (ISTAGE.LT.6 .OR. ISTAGE.GE.8) THEN
         
            ! Establish root tip layer
            CUMDEP = 0.0
            L = 0
            DO WHILE ((RTDEP.GE.CUMDEP) .AND. (L.LT.NLAYR))
              L = L + 1
              CUMDEP = CUMDEP + DLAYR(L)
            END DO
          
            ! Water factor for root depth growth
            IF (ISWWAT.NE.'N') THEN
              WFRG = AMAX1(0.1,AMIN1(1.0,
     &         ((SW(L)-LL(L))/(DUL(L)-LL(L)))/WFRGU))
              IF (L.EQ.1) THEN
                ! Layer 2 SWDF used if less stress, because
                ! elongation most likely at base of layer
                TVR1 = AMAX1(0.1,AMIN1(1.0,
     &          ((SW(2)-LL(2))/(DUL(2)-LL(2)))/WFRGU))
                IF (TVR1.GT.WFRG) WFRG = TVR1
              ENDIF
            ELSE  
              WFRG = 1.0
            ENDIF
          
            ! Root depth growth
            RTDEPG = 0.0                                                
            IF (ISTAGE.LT.7) THEN                                      
              IF (RTWTG.GT.0.0) THEN                                   
                IF (CUMTT.LT.RDGTH) THEN
                  RTDEPG = (TT/STDAY)*RDGS1                           
                ELSE                                                   
                  RTDEPG = (TT/STDAY)*RDGS2                           
                ENDIF                     
                ! Add water content and hospitality factors 
                IF (ISWWAT.NE.'N') 
     &            RTDEPG = RTDEPG * SQRT(AMAX1(0.3,SHF(L))) * WFRG
              ELSE                                                     
                ! This is to handle planting on the surface and to     
                ! allow root depth growth immediately after emergence  
                IF (DAE.LT.20) RTDEPG = (TT/STDAY)*RDGS1               
              ENDIF                                                    
            ELSEIF (ISTAGE.GE.9) THEN      ! Germination to emergence  
              RTDEPG = (TT/STDAY)*RDGS1                                
            ENDIF                                                      
          
            ! Root dry matter from seed if not enough assimilate
            ! Root length/depth growth ratio (120) derived from Ceres3.5
            ! Used in pre-emergence stage to calculate depth growth 
            ! and then need to calculate total root growth
            IF (RTWTG.LT.RLDGR*RTDEPG/((RLWR*1.0E4)*PLTPOP)) THEN
              RTWTGS = AMAX1(0.0,AMIN1(SDAFR*SEEDRSAV,
     &        (RLDGR*RTDEPG/((RLWR*1.0E4)*PLTPOP) - RTWTG)))
              IF (RTWTGS < 1.E-10) RTWTGS = 0.0
              SEEDRSAV = SEEDRSAV - RTWTGS
            ENDIF
                                                                       
            ! Root dry matter distribution with depth
            L = 0                                                      
            CUMDEP = 0.0
            RTDEPTMP = RTDEP+RTDEPG
            DO WHILE ((CUMDEP.LE.RTDEPTMP) .AND. (L.LT.NLAYR))
              L = L + 1
              CUMDEP = CUMDEP + DLAYR(L)
              SWDF = AMAX1(0.1,AMIN1(1.0,
     &         ((SW(L)-LL(L))/(DUL(L)-LL(L)))/WFRG))
              ! Original CERES
              !F (ISWNIT.EQ.'Y') THEN
              !  RNFAC = AMAX1(0.01,
              !       (1.0-(1.17*EXP(NFRG*(NO3LEFT(L)+NH4LEFT(L))))))
              !ELSE
              !  RNFAC = 1.0
              !ENDIF
              ! Below from Cropsim
                IF (ISWNIT.NE.'N'.AND.NCRG.GT.0.0) THEN
                  NFRG = AMIN1(1.0,
     &             AMAX1(0.1,(NO3LEFT(L)+NH4LEFT(L))/NCRG))
                ELSE
                  NFRG = 1.0
                ENDIF 
              ! LAH HAWAII 2007   N factor taken out.
              ! LAH GUELPH 2010   Replaced with Cropsim function.
              ! RLDF(L) = AMIN1(SWDF,RNFAC)*SHF(L)*DLAYR(L)
              ! RLDF(L) = SWDF*SHF(L)*DLAYR(L)
              ! Below from CROPSIM
              RLDF(L) = AMIN1(WFRG,NFRG)*SHF(L)*DLAYR(L)
            END DO
            
            ! Distribution factor adjusted for root tip layer
            IF (L.GT.0) RLDF(L) = 
     &        RLDF(L)*(1.0-(CUMDEP-RTDEPTMP)/DLAYR(L)) 
            L1 = L
            
            TRLDF = 0.0
            DO  L = 1, L1
              TRLDF = TRLDF + RLDF(L)
            END DO
            RTWTGL = 0.0 ! LAH Feb 28 2011
            IF (TRLDF.GT.1.E-8 .AND. (RTWTG+RTWTGS)> 1.E-10) THEN
              DO  L = 1, L1
                RTWTGL(L) = (RLDF(L)/TRLDF)*(RTWTG+RTWTGS)
              END DO
            ENDIF
                   
            ! Root senescence   
            RTWTS = 0.0
            RTWTSL = 0.0 
            RTNSL = 0.0 
            DO L = 1, L1
              RTWTSL(L) = RTWTL(L)*RSEN/100.0*TT/STDAY ! NB. Temp effect
              RTWTS = RTWTS + RTWTL(L)*RSEN/100.0*TT/STDAY
              RTNSL(L) = AMIN1(RTWTSL(L)*RMNC,RTWTSL(L)*RANC)
            ENDDO

          ENDIF
          
          ! Devernalization
          VDLOST = 0.0                                                  
          IF (CUMVD.LT.10.0 .AND. TMAX.GT.30.0) THEN                    
            VDLOST = 0.5*(TMAX-30.0)
          ENDIF
          
          ! Loss of cold hardiness
          IF (ISTAGE.GE.9 .OR. ISTAGE.LT.7) THEN
            IF (ISTAGE.GE.2) THEN
              HARDILOS = AMAX1(0.0,(TMAX-10.0)*0.1)
            ELSE
              HARDILOS = AMAX1(0.0,(TMAX-10.0)*0.01)
            ENDIF
          ENDIF
          
          IF (ISTAGE.LE.7.0) THEN
            TNUMLOSS = 0.0
            PLTLOSS = 0.0
            PLASC = 0.0
            CKCOLD = 0.0
            ! Leaf senescence
            IF ((TMIN+TMAX)/2.0.LT.TKLF) THEN
              CKCOLD = (TKLF-(TMIN+TMAX)/2.0)*0.20
              ! 20% loss for every degree below Tklf
            ENDIF
            ! Following is original (as CK)
            ! CKCOLD = AMAX1(0.00,AMIN1(0.96,
            ! (0.020*HARDI-0.10)*(TMIN*0.85+TMAX*0.15+10.0+.25*SNOW)))
            PLASC = AMAX1(0.0,
     &            AMIN1(CKCOLD*(PLA-SENLA),((PLA-SENLA)-TNUM*0.035)))
            IF (PLASC.GT.0.0.AND.(PLA-SENLA).GT.0.0) 
     &       WRITE(fnumwrk,'(A30,I4,A14,F4.1)')
     &       ' Leaves damaged by cold on day',doy,
     &       ' Fraction lost',plasc/(pla-senla)
          
            ! Tiller and plant death
            IF (TKILL.GT.(TMIN+TMAX)/2.0) THEN
              IF (TNUM.GE.1.0) THEN
                TNUMLOSS=TNUM *
     &           (1.0-(0.9-0.02*ABS(((TMIN+TMAX)/2.0-TKILL))**2))
              ENDIF
              IF (TNUM-TNUMLOSS.GE.1.0) THEN
                WRITE (FNUMWRK,900)
     &           DOY,TKILL,(TMIN+TMAX)/2.0,HARDI,TNUM,PLTPOP
 900            FORMAT (' Crop was damaged by cold on day',I4,/,
     &            ' TKILL =',F5.1,5X,'TMEAN=',F5.1,5X,
     &            'HARDI=',F5.2,5X,'TNUM =',  F7.2,5X,'PLTPOP=',F4.0)
              ELSE
                PLTLOSS =
     &           PLTPOP*(1.0-(0.95-0.02*((TMIN+TMAX)/2.0-TKILL)**2))
                 IF (ISTAGE.GE.4) PLTLOSS = 0.0
                IF (PLTPOP-PLTLOSS.GE.0.05*PLTPOPP) THEN
                  WRITE (FNUMWRK,900) DOY,TKILL,
     &             (TMIN+TMAX)/2.0,HARDI,TNUM,PLTPOP
                ELSE
                  CFLFAIL = 'Y'
                  PLTLOSS = AMIN1(PLTPOP,PLTLOSS)
                  IF (ISTAGE.GE.4) PLTLOSS = 0.0
                  WRITE (FNUMWRK,1100) DOY,TKILL,(TMIN+TMAX)/2.0,HARDI
 1100             FORMAT (' At least 95% killed by cold on day',I4,/,
     &            ' TKILL =',F5.1,5X,'TMEAN =',F5.1,5X,
     &            'HARDII =',F5.2)
                ENDIF
              ENDIF
            ENDIF
          ENDIF

          ! Senescent leaf - NB. N loss has a big effect if low N
          LSENNF = 0.0
          IF (LANC.GT.0.0) LSENNF = (1.0-((LANC-LMNC)/LANC))
          PLASS = 0.0
          PLAST = 0.0
          SENLFG = 0.0
          SENLFGRS = 0.0
          SENRS = 0.0
          SENNLFG = 0.0
          SENNLFGRS = 0.0
          ! Following is to lose some leaf area when tillers die
          IF (TNUMD.GT.0.0) PLAST = TNUMD * ((PLA-SENLA)/TNUM) * LALOSSF
          IF (ISWWAT.NE.'N') THEN
            IF (PLA-SENLA.GT.0.0.AND.WUPR.LT.WFSU) THEN
              PLASS = AMAX1(0.0,AMIN1((PLA-SENLA)-PLAS,
     &            (PLA-SENLA)*LLOSW))
            ENDIF
          ENDIF
          IF (ISWNIT.NE.'N') THEN
            ! Low N accelerated senescence
            IF (PLA-SENLA.GT.0.0.AND.LANC.LT.LCNCSEN) THEN
              PLASS = PLASS +  AMAX1(0.0,AMIN1((PLA-SENLA)-PLAS,
     &            (PLA-SENLA)*LLOSN))
            ENDIF
          ENDIF
          IF (PLA-SENLA.GT.0.0) THEN
            IF (PLASC.GT.0.0) THEN
              ! If cold kill
              SENLFG = AMIN1(LFWT,LFWT*PLASC/(PLA-SENLA))
              SENRS = AMIN1(RSWT,RSWT*PLASC/(PLA-SENLA))
            ELSE
              ! If normal senescence
              SENLFG = AMIN1(LFWT*LWLOS,(AMAX1(0.0,         
     &         (LFWT*((PLAS+PLASS)/(PLA-SENLA))*LWLOS))))
              SENLFGRS = AMIN1(LFWT*(1.0-LWLOS),(AMAX1(0.0,         
     &        (LFWT*((PLAS+PLASS)/(PLA-SENLA))*(1.0-LWLOS)))))
            ENDIF
            SENNLFG = AMIN1((LEAFN-GRAINNGL)*LSENNF,
     &                (SENLFG+SENLFGRS)*LANC*LSENNF)
            SENNLFGRS = AMIN1((LEAFN-GRAINNGL)*(1.0-LSENNF),
     &                  (SENLFG+SENLFGRS)*LANC*(1.0-LSENNF))
            IF (((SENNLFG+SENNLFGRS)-LEAFN).GT.1.0E-8) THEN
              WRITE(fnumwrk,'(A40,F6.2)')
     &         ' Adjusted N removal from leaves at stage',xstage
              SENNLFGRS = LEAFN-SENNLFG
              IF (SENNLFGRS.LT.0.0) THEN
                SENNLFG = SENNLFG - ABS(SENNLFGRS)
                SENNLFGRS = 0.0
                IF (SENNLFG.LT.0.0) SENNLFG = 0.0
              ENDIF
            ENDIF
          ENDIF

          ! Senescent stem
          SENSTG = 0.0
          SENNSTG = 0.0
          SENNSTGRS = 0.0
          SSENF = 0.0
          IF (XSTAGE.GT.SSSTG .AND. XSTAGE.LT.6.0) THEN
            SENSTG = AMAX1(0.0,STWT*(SSEN/100.0)*(TT/STDAY))
            IF (SANC.GT.0.0) SSENF = (1.0-((SANC-SMNC)/SANC))
            SENNSTG = SENSTG*SANC*SSENF
            SENNSTGRS = SENSTG*SANC*(1.0-SSENF)
            IF (SENNSTG+SENNSTGRS.GT.STEMN) THEN
              WRITE(fnumwrk,*)'N removal from stem > stem N'
              SENNSTG = STEMN-SENNSTGRS
            ENDIF
          ENDIF

          ! N Uptake
          IF (ISWNIT.EQ.'Y') THEN                                       
      
            ! N uptake initializations                        
            DTOPN = 0.0
            DROOTN = 0.0
            DSTEMN = 0.0
            DLEAFN = 0.0
            DSTOVN = 0.0
            GRAINNG = 0.0
            NUPD = 0.0
            NUAG = 0.0
            NUF = 0.0
            SEEDNR = 0.0
            SEEDNRA = 0.0
            SEEDNRB = 0.0
            SEEDNT = 0.0
            SEEDNTA = 0.0
            SEEDNTB = 0.0
            RSNUSET = 0.0
            RSNUSER = 0.0
            RSNV = 0.0
            ANDEM = 0.0
            NDEMSOIL = 0.0
            RNDEM = 0.0
            RNDEMSOIL = 0.0
            RNDEM1 = 0.0
            TNDEM = 0.0
            TNDEMSOIL = 0.0
            TRLV = 0.0
            DROOTNA = 0.0
            GRAINNGR = 0.0
            GRAINNGV = 0.0
            NPOOLL = 0.0
            NPOOLST = 0.0
            NPOOLV = 0.0
              
            SNO3PROFILE = 0.0
            SNH4PROFILE = 0.0
            SNO3ROOTZONE = 0.0
            SNH4ROOTZONE = 0.0
            DO L = 1, NLAYR
              UNO3ALG(L) = 0.0
              UNH4ALG(L) = 0.0
              RNO3U(L) = 0.0
              RNH4U(L) = 0.0
              TRLV = TRLV + RLV(L)
              FAC(L) = 10.0/(BD(L)*DLAYR(L))
              SNO3(L) = NO3LEFT(L) / FAC(L)
              SNH4(L) = NH4LEFT(L) / FAC(L)
              SNO3PROFILE = SNO3PROFILE + SNO3(L)
              SNH4PROFILE = SNH4PROFILE + SNH4(L)
              IF (RLV(L).GT.0.0) THEN
                SNO3ROOTZONE = SNO3ROOTZONE + SNO3(L)
                SNH4ROOTZONE = SNH4ROOTZONE + SNH4(L)
              ENDIF
            END DO
          
            ! Grain N uptake.
            NSINK = 0.0
            IF (GRNUM.GT.0.0 .AND. XSTAGE.LT.6.0) THEN
              NSINK = AMIN1(GROGRPA*(GRNMX/100.0),
     &         LAGSTAGE*TFGN*GRNUM*G2*DU*.001*(GRNS/100.))
            ENDIF
          
            ! N uptake factor after maturity
            IF (XSTAGE.GT.6.0 .AND. XSTAGE.LT.7.0) THEN
              NUFACM = AMAX1(0.0,1.0 - (XSTAGE-6.0)/(6.5-6.0))
            ELSE
              NUFACM = 1.0
            ENDIF
            
            LNDEM = NUFACM *
     &              ((LFWT-SENLFG-SENLFGRS)*AMAX1(0.0,NTUPF*(LCNC-LANC))
     &              + GROLF*LCNC)
            SNDEM = NUFACM *
     &              ((STWT-SENSTG)*AMAX1(0.0,NTUPF*(SCNC-SANC))
     &              + AMAX1(0.0,GROST)*SCNC)
            RNDEM = NUFACM *
     &              ((RTWT-RTWTS) * AMAX1(0.0,NTUPF*(RCNC-RANC))
     &              + (RTWTG+RTWTGS) * RCNC)
     
            ! Reserve N use
            RSNUSEG = 0.0
            RSNUSET = 0.0
            RSNUSER = 0.0
            IF (NSINK.GT.0.0) RSNUSEG = AMAX1(0.0,AMIN1(NSINK,RSN))
            RSNV = RSN - RSNUSEG
            IF (LNDEM+SNDEM+RNDEM.GT.0.0 .AND. RSNV.GT.0.0) THEN
              IF (LNDEM+SNDEM+RNDEM.LT.RSNV) THEN
                RSNUSET = LNDEM + SNDEM
                RSNUSER = RNDEM
                TNDEM = 0.0
                RNDEM = 0.0
              ELSE
                RSNUSET = RSNV * (LNDEM+SNDEM)/(LNDEM+SNDEM+RNDEM)
                RSNUSER = RSNV * RNDEM/(LNDEM+SNDEM+RNDEM)
                TNDEM = (LNDEM+SNDEM) - RSNUSET
                RNDEM = RNDEM - RSNUSER
              ENDIF
              IF (RSNUSET.LT.1E-20) RSNUSET = 0.0
              IF (RSNUSER.LT.1E-20) RSNUSER = 0.0
            ELSE
              TNDEM = LNDEM + SNDEM
              RNDEM = RNDEM
            ENDIF
          
            ! Seed N use (Basic)
            ! Need a little seed use regardless of deficit after uptake
            SEEDNTB = 0.0
            SEEDNRB = 0.0
            IF (XSTAGE.LT.4 .OR. XSTAGE.GT.6) THEN
              ! Originaal: 
              !IF (SEEDN.GT.1.0E-6) THEN
              !  SEEDNTB = AMIN1(TNDEM,SEEDN*0.020)  
              !  SEEDNRB = AMIN1(RNDEM,AMAX1(0.0,SEEDN*0.025-SEEDNTB))
              !ENDIF
              ! To match Cropsim  
              IF (RTWT.LE.0.0) THEN
                SEEDNTB = AMIN1(TNDEM,SEEDN*0.2)  
                SEEDNRB = AMIN1(RNDEM,SEEDN*0.2)
              ELSE
                ! Some use of seed (0.2 need) even if may not be needed
                SEEDNTB = AMAX1(0.0,AMIN1(0.2*TNDEM,SEEDN*0.2))
                SEEDNRB = AMAX1(0.0,AMIN1(0.2*RNDEM,SEEDN*0.2))
              ENDIF
            ENDIF
          
            IF ((NSINK-RSNUSEG).GT.0.0) THEN    
              TNDEMSOIL = TNDEM - SEEDNTB + (NSINK-RSNUSEG)
            ELSE
              TNDEMSOIL = TNDEM - SEEDNTB
            ENDIF  
            RNDEMSOIL = RNDEM - SEEDNRB
            
            NDEMSOIL = TNDEMSOIL + RNDEMSOIL
            ANDEM = NDEMSOIL * PLTPOP*10.0
            TRNU = 0.0
            RNO3U(L) = 0.0
            RNH4U(L) = 0.0
            ! Calculate potential N uptake in soil layers with roots
            DO L=1,NLAYR
              IF (RLV(L) .GT. 1.E-6) THEN
                FNH4 = 1.0 - EXP(-0.08 * NH4LEFT(L))
                FNO3 = 1.0 - EXP(-0.08 * NO3LEFT(L))
                ! The following limits are those in NUPTAK
                IF (FNO3 .LT. 0.04) FNO3 = 0.0  
                IF (FNO3 .GT. 1.0)  FNO3 = 1.0
                IF (FNH4 .LT. 0.04) FNH4 = 0.0  
                IF (FNH4 .GT. 1.0)  FNH4 = 1.0
                SMDFR = (SW(L) - LL(L)) / (DUL(L) - LL(L))
                IF (SMDFR .LT. 0.0) SMDFR = 0.0
                IF (SW(L) .GT. DUL(L)) THEN
                  SMDFR = 1.0 - (SW(L) - DUL(L)) / (SAT(L) - DUL(L))
                  ! Wet soil effect not implemented
                  SMDFR = 1.0
                ENDIF
                RFAC = RLV(L) * SMDFR * SMDFR * DLAYR(L) * 100.0
                !  RLV = Rootlength density (cm/cm3);SMDFR = relative drought factor
                !  RTNO3 + RTNH4 = Nitrogen uptake / root length (mg N/cm)  
                !  RNO3U + RNH4  = Nitrogen uptake (kg N/ha)
                RNO3U(L) = RFAC * FNO3 * RTNO3
                RNH4U(L) = RFAC * FNH4 * RTNH4
                RNO3U(L) = MAX(0.0,RNO3U(L))
                RNH4U(L) = MAX(0.0,RNH4U(L))
                TRNU = TRNU + RNO3U(L) + RNH4U(L) !kg[N]/ha
              ENDIF
            ENDDO
 
            ! Ratio (NUPR) to indicate N supply for output
            IF (ANDEM.GT.0) THEN
              NUPR = TRNU/ANDEM
            ELSE
              IF (TRNU.LE.0) THEN
                NUPR = 0.0
              ELSE
                NUPR = 10.0
              ENDIF
            ENDIF
            ! Factor (NUF) to reduce N uptake to level of demand
            IF (TRNU.GT.0.0) NUF = AMIN1(1.0,ANDEM/TRNU)
            ! Actual N uptake by layer roots based on demand (kg/ha)
            DO L = 1, MIN(L1,NLAYR)
              UNO3ALG(L) = RNO3U(L)*NUF
              XMIN = NO3MN/FAC(L) ! Original XMIN = 0.25/FAC(L)
              UNO3ALG(L) = MAX(0.0,MIN (UNO3ALG(L),SNO3(L) - XMIN))
              UNH4ALG(L) = RNH4U(L)*NUF
              IF (FAC(L).GT.0.0) THEN
                XMIN = NH4MN/FAC(L) ! Original XMIN = 0.5/FAC(L)
              ELSE
                XMIN = 0.0
              ENDIF  
              UNH4ALG(L) = MAX(0.0,MIN (UNH4ALG(L),SNH4(L) - XMIN))
              NUAG = NUAG + UNO3ALG(L) + UNH4ALG(L)
            END DO
          
            NUPD = NUAG/(PLTPOP*10.0)
          
            ! Change in above and below ground N
            IF (NDEMSOIL.LE.0.0 .OR. NUPD.LE.0.0) THEN
              DTOPN = 0.0 + RSNUSET
              DROOTN = 0.0 + RSNUSER
            ELSE
              DTOPN = (TNDEMSOIL/NDEMSOIL)*NUPD + RSNUSET
              DROOTN = (RNDEMSOIL/NDEMSOIL)*NUPD + RSNUSER
            ENDIF
          
            ! Make sure that roots do not fall below minimum
            IF ((RTWTG+RTWTGS)*RMNC.GT.DROOTN) THEN
              DROOTNA = AMIN1(DTOPN,((RTWTG+RTWTGS)*RMNC)-DROOTN)
              DROOTN = DROOTN + DROOTNA
              DTOPN = DTOPN - DROOTNA
            ENDIF
          
            ! Use N allotted to tops and roots for grain if filling     
            IF ((NSINK-RSNUSEG).GT.0.0) THEN
              IF ((NSINK-RSNUSEG).GE.DTOPN+DROOTN) THEN
                GRAINNG = DTOPN + DROOTN
                DSTOVN = 0.0
                DROOTN = 0.0
              ELSE
                GRAINNG = (NSINK-RSNUSEG)
                TVR1 = DTOPN+DROOTN-(NSINK-RSNUSEG)
                IF (TNDEM+RNDEM > 1.E-6) THEN
                DSTOVN = TNDEM/(TNDEM+RNDEM) * TVR1
                DROOTN = RNDEM/(TNDEM+RNDEM) * TVR1
                ELSE
                  DSTOVN = 0.0
                  DROOTN = 0.0
                ENDIF
              ENDIF
            ELSE
              DSTOVN = DTOPN
            ENDIF
          
            ! Use additional seed if not sufficient so far              
            IF (XSTAGE.LT.4 .OR. XSTAGE.GT.6) THEN
              SEEDNTA = AMAX1(0.0,
     &         AMIN1(SEEDN-SEEDNTB-SEEDNRB,TNDEMSOIL-DSTOVN))
              SEEDNRA = AMAX1 (0.0,
     &         AMIN1(SEEDN-SEEDNTB-SEEDNTA-SEEDNRB,RNDEMSOIL-DROOTN))
              SEEDNT = SEEDNTB + SEEDNTA
              SEEDNR = SEEDNRB + SEEDNRA
            ENDIF
          
            ! Move N to grain from roots and tops if necessary
            IF((NSINK-RSNUSEG).GT.0..AND.(NSINK-RSNUSEG).GT.GRAINNG)THEN
              NPOOLR = AMAX1 (0.0,(XNFS/100.0)*(RTWT*(RANC-RMNC)))
              IF (NPOOLR.GT.(NSINK-RSNUSEG)-GRAINNG) THEN
                GRAINNGR = (NSINK-RSNUSEG)-GRAINNG
              ELSE
                GRAINNGR = NPOOLR
              ENDIF
              IF ((NSINK-RSNUSEG).GT.GRAINNG+GRAINNGR) THEN
                ! N use limit 
                IF (XSTAGE.GE.5.0.AND.XSTAGE.LE.6.0) THEN
                  NUSELIM = AMIN1(1.0,DU/((6.0-XSTAGE)*PD(5)))
                ELSEIF (XSTAGE.GE.4.0.AND.XSTAGE.LE.5.0) THEN
                  NUSELIM = DU/(((5.0-XSTAGE)*PD(4))+PD(5))
                ENDIF
                NUSEFAC = AMAX1(NUSELIM,(XNFS/100.0))
                NPOOLL = AMAX1 (0.0,
     &           NUSEFAC*((LFWT-SENLFG-SENLFGRS)*(LANC-LMNC)))
                NPOOLST = AMAX1 (0.0,
     &           NUSEFAC*((STWT-SENSTG)*(SANC-SMNC)))
                NPOOLV = NPOOLL + NPOOLST
                IF (NPOOLV.GT.(NSINK-RSNUSEG)-(GRAINNG+GRAINNGR)) THEN
                  GRAINNGV = (NSINK-RSNUSEG)-GRAINNG-GRAINNGR
                ELSE
                  GRAINNGV = NPOOLV
                ENDIF
              ENDIF
            ENDIF
          
            ! Split tops aspects into leaf and stem aspects
            IF (LNDEM+SNDEM.GT.0.0) THEN
              DLEAFN = DSTOVN * LNDEM / (LNDEM+SNDEM)
              DSTEMN = DSTOVN * SNDEM / (LNDEM+SNDEM)
            ELSE
              DLEAFN = 0.0
              DSTEMN = 0.0
            ENDIF
            IF (NPOOLL+NPOOLST.GT.0.0) THEN
              GRAINNGL = GRAINNGV * NPOOLL / (NPOOLL+NPOOLST)
              GRAINNGS = GRAINNGV * NPOOLST / (NPOOLL+NPOOLST)
            ELSE
              GRAINNGL = 0.0
              GRAINNGS = 0.0
            ENDIF
          
          ENDIF

          ! Minimum grain N control
          GRORSGR = 0.0
          IF (ISWNIT.NE.'N'.AND.GRNMN.GT.0.0) THEN
            GROGRPN =
     &       (GRAINNG+GRAINNGR+GRAINNGL+GRAINNGS+RSNUSEG)*(100./GRNMN)
            IF (GROGRPN.LT.GROGRPA.AND.GRAINANC*100.0.LE.GRNMN) THEN
              GRORSGR = GROGRPA - GROGRPN
              GROGR = GROGRPN
            ELSE
              GROGR = GROGRPA
              GRWTTMP = GRWT + GROGRPA
              IF (GRWTTMP.GT.0.0) THEN
                GRAINNTMP = GRAINN +
     &          (GRAINNG+GRAINNGR+GRAINNGL+GRAINNGS+RSNUSEG)
                IF (GRAINNTMP/GRWTTMP*100.0 .LT. GRNMN) THEN
                  GRWTTMP = GRAINNTMP*(100.0/GRNMN)
                  GROGR = GRWTTMP - GRWT
                  IF (GROGR.LT.GROGRPA) GRORSGR = GROGRPA - GROGR
                ENDIF
              ENDIF
            ENDIF
            IF (GROGR.LT.GROGRPA) THEN
              WRITE(fnumwrk,'(A42,F4.2)')
     &         ' N limit on grain growth. N at minimum of ',grnmn
              NLIMIT = NLIMIT + 1
            ENDIF
          ELSE
            GROGR = GROGRPA
          ENDIF

          ! Maximum grain weight control
          GROGRADJ = 0.0
          IF (GRNUM.GT.0.0) THEN
            IF ((GRWT+GROGR)/GRNUM - G2KWT/1000.0 > 1.E-5) THEN
              WRITE(fnumwrk,*)'Maximum kernel wt reached on:',YEARDOY
              GROGRADJ = GROGR - (G2KWT/1000.0-(GRWT/GRNUM))*GRNUM
            ENDIF
          ENDIF

          ! Growth variables expressed on area basis
          ! C assimilation
          CARBOA = CARBO*PLTPOP*10.0
          ! N assimilation
          !NUAG = NUPD*PLTPOP*10.0
          ! Above ground senescence
          SENWALG(0) = 0.0
          SENNALG(0) = 0.0
          SENCALG(0) = 0.0
          SENLGALG(0) = 0.0
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

        IF (YEARDOY.GE.YEARPLT) THEN

          ! Dry weights
          ! LAH No growth/development on planting day
          IF (YEARDOY.GT.YEARPLT) THEN   
            SENRSC = SENRSC + SENRS
            CARBOC = CARBOC + CARBO
            RESPC = RESPC + RTRESP
            LFWT = LFWT + GROLF - SENLFG - SENLFGRS
            IF (LFWT.LT.1.0E-12) THEN
              IF (LFWT.LT.0.0) 
     &          WRITE(fnumwrk,*)'Leaf weight less than 0! ',LFWT
              LFWT = 0.0
            ENDIF
            STWT = STWT + GROST - SENSTG - GROGRST
            IF (STWT.LT.1.0E-06) THEN
              IF (STWT.LT.0.0) 
     &         WRITE(fnumwrk,*)'Stem weight less than 0! ',STWT
              STWT = 0.0
            ENDIF
            GRWT = GRWT + GROGR - GROGRADJ
            RSWT = RSWT + GRORS + GRORSGR + GROGRADJ + SENLFGRS - 
     &             SENRS - RTWTGRS
            
            ! Reserves distribution 
            ! Max concentration in leaves increases through life cycle.
            LLRSWT = AMIN1(RSWT,
     &       LFWT*(1.0-LSHFR)*RSCLX*CUMDU/(Pd(1)+pd(2)+pd(3)+pd(4)))
            LSHRSWT = AMIN1(RSWT-LLRSWT,
     &       LFWT*LSHFR*RSCLX*CUMDU/(Pd(1)+pd(2)+pd(3)+pd(4)))
            IF (STWT.GT.0.0) THEN
!-GH        IF (STWT+CHWT.GT.0.0) THEN
              STRSWT = (RSWT-LLRSWT-LSHRSWT)*(STWT-CHWT)/STWT
              CHRSWT = (RSWT-LLRSWT-LSHRSWT)*CHWT/STWT
            ELSE
              STRSWT = (RSWT-LLRSWT-LSHRSWT)
              CHRSWT = 0.0
            ENDIF
            IF (XSTAGE.GE.LRETS) THEN
              DEADWT = DEADWT + SENLFG + SENSTG
            ELSE
              SENWL(0) = SENWL(0) + (SENLFG+SENSTG)
              SENCL(0) = SENCL(0) + (SENLFG+SENSTG) * 0.4
              SENLGL(0) = SENLGL(0)+(SENLFG*LLIGP/100+SENSTG*SLIGP/100)
            ENDIF
            RTWT = 0.0
            DO L = 1, NLAYR
              RTWTL(L) = RTWTL(L) + RTWTGL(L) - RTWTSL(L)
              SENWL(L) = SENWL(L) + RTWTSL(L)
              SENCL(L) = SENCL(L) + RTWTSL(L) * 0.4
              SENLGL(L) = SENLGL(L) + RTWTSL(L) * RLIGP/100.0
              ! Totals
              RTWT = RTWT + RTWTL(L)
              SENWS = SENWS + RTWTSL(L)
              SENCS = SENCS + RTWTSL(L) * 0.4
              SENLGS = SENLGS + RTWTSL(L) * RLIGP/100.0
            END DO
          ENDIF 
          SEEDRS = AMAX1(0.0,SEEDRS+GRORSSD-RTWTGS)
          SEEDRSAV = SEEDRS

          IF (ISTAGE.GE.6) RSWTPM = RSWTPM + GRORSPM

          ! Chaff. Calculated for output. 
          ! STWT includes chaff, but STWAD excludes chaff.
          IF (XSTAGE.GT.CHSTG) THEN
            IF (GROST.GT.0.0) CHWT = CHWT + GROST*CHFR
          ENDIF

          IF (GRNUM.GT.0.0) THEN
            GWUD = GRWT/GRNUM
          ELSE
            GWUD = 0.0
          ENDIF
            
          GWGD = GWUD*1000.0

          HIAD = 0.0
          SHRTD = 0.0
          IF ((LFWT+STWT+GRWT+RSWT+DEADWT).GT.0.0)
     &     HIAD = GRWT/(LFWT+STWT+GRWT+RSWT+DEADWT)
          IF (RTWT.GT.0.0)
     &     SHRTD = (LFWT+STWT+GRWT+RSWT+DEADWT) / RTWT

          ! Reserve concentration and factor
          RSCD = 0.0
          IF (LFWT+STWT.GT.0.0)
     &     RSCD = RSWT/(LFWT+STWT+RSWT)

          ! Radiation use efficiency
          PARUED = 0.0
          PARADCUM = PARADCUM + PARAD
          IF (PARAD*PARI.GT.0.0) THEN
            PARUED = CARBO*PLTPOP/(PARAD*PARI)
            PARADICUM = PARADICUM + PARAD*PARI
          ENDIF  

          ! Height
          CANHT = CANHT + CANHTG

          ! Leaf areas
          PLA = PLA + PLAGT(1) + PLAGT(2)
          SENLA = SENLA + PLAS + PLASS + PLASC + PLAST
          IF (XSTAGE.GE.LRETS) THEN
            SENLARETAINED = SENLARETAINED
     &                        + (PLAS+PLASS+PLASC+PLAST)
          ELSE
            SENLALITTER = SENLALITTER + (PLAS+PLASS+PLASC+PLAST)
          ENDIF
          IF (LNUMSG.GT.0) THEN
            LAP(LNUMSG) = LAP(LNUMSG) + PLAGT(1)
            LATL(1,LNUMSG) = LATL(1,LNUMSG)+PLAG(1)
            IF (PLAG(2).GT.0.0) THEN
              IF (LNUMSG.LT.LNUMX) THEN
                LAP(LNUMSG+1) = LAP(LNUMSG+1) + PLAGT(2)
                LATL(1,LNUMSG+1) = LATL(1,LNUMSG+1)+PLAG(2)
              ELSEIF (LNUMSG.GE.LNUMX) THEN
                LAP(LNUMSG) = LAP(LNUMSG) + PLAGT(2)
                LATL(1,LNUMSG) = LATL(1,LNUMSG)+PLAG(2)
              ENDIF
            ENDIF
          ENDIF

          IF (ISTAGE.GT.0) GPLA(ISTAGE) = AMAX1(0.0,PLA-SENLA)
          LAI = AMAX1 (0.0,(PLA-SENLA)*PLTPOP*0.0001)
          LAIX = AMAX1(LAIX,LAI)

          PLASTMP = PLAS + PLASS + PLASC + PLAST
          ! Distribute senesced leaf over leaf positions
          IF (LNUMSG.GT.0 .AND. PLASTMP.GT.0) THEN
            DO L = 1, LNUMSG
              IF (LAP(L)-LAPS(L).GT.PLASTMP) THEN
                LAPS(L) = LAPS(L) + PLASTMP
                PLASTMP = 0.0
              ELSE
                PLASTMP = PLASTMP - (LAP(L)-LAPS(L))
                LAPS(L) = LAP(L)
              ENDIF
              IF (PLASTMP.LE.0.0) EXIT
            ENDDO
          ENDIF

          IF (fileiot(1:2).NE.'DS') THEN
          IF (LNUMSG.GT.0) CALL Cslayers
     X     (chtpc,clapc,               ! Canopy characteristics
     X     pltpop,lai,canht,           ! Canopy aspects
     X     lnumsg,lap,lap(lnumsg),     ! Leaf cohort number and size
     X     LAIL)                       ! Leaf area indices by layer
          ENDIF

          ! PAR interception
          IF (PARIP.LT.0.0.AND.LAI.GT.0.0) THEN
            PARI = (1.0 - EXP(-KCAN*(LAI+AWNAI)))
            !WRITE(fnumwrk,'(A28,F5.3)')
     X      ! '  PARI from one-crop model: ',PARI
            ! For maize, kcan is calculated as:
            ! 1.5 - 0.768*((rowspc*0.01)**2*pltpop)**0.1
            ! eg. 1.5 - 0.768*((75*0.01)**2*6.0)**0.1  =  0.63
          ELSE
            PARI = 0.0
          ENDIF

          ! Specific leaf area
          SLA = -99.0
          IF (LFWT.GT.0) SLA = (PLA-SENLA) / (LFWT*(1.0-LSHFR))
          ! Warning if SLA too low
          IF (SLA.LT.0.0.AND.SLA.GT.-90.0) THEN
            WRITE(fnumwrk,'(A21,F8.3,A12)')
     X       '  SLA below zero at: ',sla,' Reset to 0'
            SLA = -99.0
          ENDIF

          ! Leaf sheath area
          IF (RSTAGE.LE.4.0) THEN
            LSHAI = (LFWT*LSHFR*LSHAWS)*PLTPOP*0.0001
          ELSE
            ! 2.0 senesces per day
            LSHAI =LSHAI * (1.0-(2.0/100.0)*TT/20.0)  
           ENDIF  

          ! Stem area 
          SAID = AMAX1 (0.0,(STWT*SAWS*PLTPOP*0.0001))

          ! Tillers (Limited to maximum of 20)
          TNUM = AMIN1(20.0,AMAX1(1.0,TNUM+TNUMG-TNUMD-TNUMLOSS))
          IF (LNUMSG.GT.0) TNUML(LNUMSG) = TNUM

          ! Plants
          PLTPOP = PLTPOP - PLTLOSS   

          IF (PLTPOP < 1.E-5) THEN
            WRITE(fnumwrk,'(I5,1X,I3.3,
     &        " PLTPOP is zero or negative. Set to zero.",/)') YEAR, DOY
            PLTPOP = AMAX1(PLTPOP, 1.E-5)
          ENDIF

          ! Root depth and length
          IF (SDEPTH.GT.0.0 .AND.RTDEP.LE.0.0) RTDEP = SDEPTH
          RTDEP = AMIN1 (RTDEP+RTDEPG,DEPMAX)
          
          DO L = 1, NLAYR
            RLV(L)=RTWTL(L)*RLWR*PLTPOP/DLAYR(L)
            IF (L.EQ.NLAYR.AND.RLV(L).GT.0.0)THEN
              IF (RTSLXDATE.LE.0) RTSLXDATE = YEARDOY
            ENDIF
          END DO
          ! Originally: RLV(L)=RTWTL(L)*(RLWR/0.6)*PLTPOP/DLAYR(L)
          ! 0.6 above kept to keep similarity with Ceres,
          ! in which a value of 0.98 for RLWR was applied to assimilate
          ! going to root. This was multiplied by a factor of 0.6 to 
          ! account for root repiration to get actual root dry weight 
          ! increase.

          ! Vernalization.  NB. Starts at germination
          CUMVD = AMAX1(0.0,CUMVD+TFV-VDLOST)
          IF (ISTAGE.GE.7 .OR. ISTAGE.LE.1) THEN
            IF (P1V.GT.0.0) THEN
              IF (p1v.GT.0.0) THEN
                VRNSTAGE =AMAX1(0.,AMIN1(1.,CUMVD/p1v))
              ELSE
                VRNSTAGE = 1.0
              ENDIF
              !VF = AMAX1(0.,AMAX1(0.,AMIN1(1.,CUMVD/P1V)))
              ! BELOW FROM CSCRP
              VF = AMAX1(0.,(1.0-VEFF) + VEFF*VRNSTAGE)
            ELSE
              VF = 1.0   
            ENDIF
          ELSEIF (ISTAGE.GT.1 .AND. ISTAGE.LT.7) THEN
              VF = 1.0
          ENDIF

          ! Cold hardiness
          HARDAYS = AMAX1(HARDAYS+TFH-HARDILOS,0.0)
          HARDI = AMIN1(1.0,HARDAYS/HDUR)
          TKILL = LT50S + (LT50H-LT50S)*HARDI

          ! Nitrogen
          NUPC = NUPC + NUPD
          LEAFN = LEAFN + DLEAFN + SEEDNT
     &          - GRAINNGL - SENNLFG - SENNLFGRS
          IF (LEAFN.LT.1.0E-10) LEAFN = 0.0
          STEMN = STEMN + DSTEMN
     &          - GRAINNGS - SENNSTG - SENNSTGRS
          IF (STEMN.LT.1.0E-10) STEMN = 0.0
          ROOTNS = 0.0
          DO L = 1, NLAYR
            SENNL(L) = SENNL(L) + RTNSL(L)
            ROOTNS = ROOTNS + RTNSL(L)
            SENNS = SENNS + RTNSL(L)
          END DO
          ROOTN = ROOTN + DROOTN + SEEDNR - GRAINNGR - ROOTNS
          SEEDN = SEEDN - SEEDNR - SEEDNT
          IF (SEEDN.LT.1.0E-6) SEEDN = 0.0
          GRAINN = GRAINN + GRAINNG + GRAINNGL + GRAINNGS + GRAINNGR
     &           + RSNUSEG
          RSN = RSN - RSNUSEG - RSNUSER - RSNUSET
     &        + SENNLFGRS + SENNSTGRS
          IF (XSTAGE.GE.LRETS) THEN
            DEADN = DEADN + SENNLFG + SENNSTG
          ELSE
            SENNL(0) = SENNL(0) + (SENNLFG+SENNSTG)
          ENDIF

          ! Harvest index for N
          HIND = 0.0
          IF ((LEAFN+STEMN+GRAINN+RSN+DEADN).GT.0.0)
     &     HIND = GRAINN/(LEAFN+STEMN+GRAINN+RSN+DEADN)

          ! Variables expressed per unit ground area:living plant
          CARBOAC = CARBOC*PLTPOP*10.0
          RESPAC = RESPC*PLTPOP*10.0

          CWAD = AMAX1(0.0,(LFWT+STWT+GRWT+RSWT+DEADWT)*PLTPOP*10.0)
          DWAD = AMAX1(0.0,DEADWT*PLTPOP*10.0)
          GWAD = AMAX1(0.0,GRWT*PLTPOP*10.0)
          LLWAD = AMAX1(0.0,LFWT*(1.0-LSHFR)*10.0*PLTPOP)
          LSHWAD = AMAX1(0.0,LFWT*LSHFR*10.0*PLTPOP)
          CHWAD = AMAX1(0.0,CHWT*PLTPOP*10.0)
          ! NB.No reserves in chaff
          RSWAD = AMAX1(0.0,RSWT*PLTPOP*10.0)
          RSWADPM = AMAX1(0.0,RSWTPM*PLTPOP*10.0)
          RWAD = AMAX1(0.0,RTWT*PLTPOP*10.0)
          SDWAD = AMAX1(0.0,(SEEDRS+SDCOAT)*10.0*PLTPOP)
          STWAD = AMAX1(0.0,(STWT-CHWT)*10.0*PLTPOP)
          ! NB. Stem weigh here excludes chaff wt.
          STLSRWAD = AMAX1(0.0,(STWT-CHWT+LFWT*LSHFR+RSWT)*10.0*PLTPOP)
          LLRSWAD = AMAX1(0.0,LLRSWT*PLTPOP*10.0)
          LSHRSWAD = AMAX1(0.0,LSHRSWT*PLTPOP*10.0)
          STRSWAD = AMAX1(0.0,STRSWT*PLTPOP*10.0)
          CHRSWAD = AMAX1(0.0,CHRSWT*PLTPOP*10.0)

          SENWAS = SENWS*10.0*PLTPOP
          SENCAS = SENCS*10.0*PLTPOP
          SENLGAS = SENLGS*10.0*PLTPOP
          SENWAL(0) = SENWL(0)*PLTPOP*10.0
          SENCAL(0) = SENCL(0)*PLTPOP*10.0
          SENLGAL(0) = SENLGL(0)*PLTPOP*10.0
          DO L =1,NLAYR
            RTWTAL(L) = RTWTL(L)*PLTPOP*10.0
            SENWAL(L) = SENWL(L)*PLTPOP*10.0
            SENCAL(L) = SENCL(L)*PLTPOP*10.0
            SENLGAL(L) = SENLGL(L)*PLTPOP*10.0
          ENDDO

          TWAD = (SEEDRS+SDCOAT+RTWT+LFWT+STWT+GRWT+RSWT+DEADWT)
     &         * PLTPOP*10.0
          VWAD = (LFWT+STWT+RSWT+DEADWT)*PLTPOP * 10.0
          EWAD = (GRWT+CHWT)*PLTPOP * 10.0

          IF (GRNUM.GT.0.0) THEN 
            GRNUMAD = GRNUM*PLTPOP
          ELSE
            GRNUMAD = 0.0
          ENDIF  
          TNUMAD = TNUM*PLTPOP

          NUAD = NUPC*PLTPOP*10.0
          CNAD = (LEAFN+STEMN+GRAINN+RSN+DEADN)*PLTPOP*10.0
          DNAD = DEADN*PLTPOP*10.0
          GNAD = GRAINN*PLTPOP*10.0
          LLNAD = LEAFN*(1.0-LSHFR)*PLTPOP*10.0
          RNAD = ROOTN*PLTPOP*10.0
          RSNAD = RSN*PLTPOP*10.0
          SDNAD = SEEDN*PLTPOP*10.0
          SNAD = STEMN*PLTPOP*10.0
          TNAD = (ROOTN+LEAFN+STEMN+RSN+GRAINN+SEEDN+DEADN)*PLTPOP*10.0
          VNAD = (LEAFN+STEMN+RSN+DEADN)*PLTPOP*10.0

          SENNAS = SENNS*10.0*PLTPOP
          SENNAL(0) = SENNL(0)*PLTPOP*10.0
          DO L =1,NLAYR
            SENNAL(L) = SENNL(L)*PLTPOP*10.0
          ENDDO

          ! STAGES:Reproductive development (Rstages)
          CUMDU = CUMDU + DU
          IF (GESTAGE.GE.1.0) CUMTU = CUMTU + TT

          IF (CUMDU.LT.PTH(0) .AND. PD(0) > 0.) THEN
            RSTAGE = CUMDU/PD(0)
          ELSE
            DO L = 6,1,-1
              IF (CUMDU.GE.PTH(L-1)) THEN
                RSTAGE = FLOAT(L) + (CUMDU-PTH(L-1))/PD(L)
                ! Following is necessary because xstages non-sequential
                RSTAGE = AMIN1(6.9,RSTAGE)
                EXIT
              ENDIF
            ENDDO
          ENDIF
          IF (CROP.EQ.'MZ'.AND.PDADJ.LE.-99.0.AND.RSTAGE.GT.2.0) THEN
            PDADJ = (CUMTU-TT-PD(0))/(CUMDU-DU-PD(0))
            WRITE(fnumwrk,'(A26,F6.1)')
     &       ' Phase adjustment         ',(PDADJ-1.0)*PD(2)
            WRITE(fnumwrk,'(A24)')'   PHASE OLD_END NEW_END'
            DO L = 2,10
              PTHOLD = PTH(L)
              PTH(L) = PTH(L) + AMAX1(0.0,PDADJ-1.0)*PD(2)
              WRITE(fnumwrk,'(I8,2F8.1)')L,PTHOLD,PTH(L)
            ENDDO
          ENDIF

          ! STAGES:Germination and emergence (Gstages)
          ! NB 0.5 factor used to equate to Zadoks)
          IF (ISTAGE.GT.7) CUMGEU = CUMGEU + GEU
          IF (CUMGEU.LT.PEGD) THEN
            GESTAGE = AMIN1(1.0,CUMGEU/PEGD*0.5)
          ELSE
            GESTAGE = AMIN1(1.0,0.5+0.5*(CUMGEU-PEGD)/(PEMRG*SDEPTH))
          ENDIF

          ! STAGES:Leaf numbers
          IF (LNUMSG.GT.0 .AND. ISTAGE.LE.2) THEN
            ! If new leaf to be produced
            IF ((TT/PHINT).GT.(FLOAT(LNUMSG)-LNUMSD)) THEN 
              ! If new leaf will be in 3rd phint phase
              IF (LNUMSD.LE.(PHINTL(1)+PHINTL(2)).AND.
     &          LNUMSD+(TT/PHINT).GT.(PHINTL(1)+PHINTL(2))) THEN
                  TTTMP = TT - PHINT*(FLOAT(LNUMSG)-LNUMSD)
                  LNUMSD = LNUMSD+(TT-TTTMP)/PHINT+
     &             TTTMP/(PHINTS*PHINTF(3))
              ! If new leaf will be in 2nd phint phase
              ELSEIF (LNUMSD.LE.PHINTL(1).AND.
     &          LNUMSD+(TT/PHINT).GT.PHINTL(1)) THEN
                  TTTMP = TT - PHINT*(FLOAT(LNUMSG)-LNUMSD)
                  LNUMSD = LNUMSD+(TT-TTTMP)/PHINT+
     &             TTTMP/PHINTS
              ELSE
              ! New leaf in current phint phase
              LNUMSD = AMIN1(FLOAT(LNUMX-1)+0.9,LNUMSD+TT/PHINT)
              ENDIF
              LNUMSD = AMIN1(FLOAT(LNUMX-1)+0.9,LNUMSD)
            ! NO new leaf  
            ELSE
              LNUMSD = AMIN1(FLOAT(LNUMX-1)+0.9,LNUMSD+TT/PHINT)
            ENDIF
          ENDIF
          IF (LNUMSD.GE.FLOAT(LNUMX-1)+0.9) THEN
            IF (CCOUNTV.EQ.0) WRITE (fnumwrk,'(A35,I4)')
     &       ' Maximum leaf number reached on day',DOY
            CCOUNTV = CCOUNTV + 1
            IF (CCOUNTV.EQ.50) THEN
              WRITE (fnumwrk,'(A47,/,A44,/A26)')
     &         ' 50 days after maximum leaf number! Presumably ',
     &         ' vernalization requirement could not be met!',
     &         ' Will assume crop failure.'
              CFLFAIL = 'Y'
            ENDIF
          ENDIF
          LNUMSG = INT(LNUMSD)+1
          IF (LNUMSG.EQ.100) THEN
           WRITE (fnumwrk,'(A47,/,A47,/,A47,/,A26)')
     &      ' Maximum leaf number reached! Presumably       ',
     &      ' vernalization requirement could not be met,   ',
     &      ' or photoperiod too short.                     ',
     &      ' Will assume crop failure.'
            CFLFAIL = 'Y'
          ENDIF
          LCNUM = INT(LNUMSD)+1

          ! STAGES:Overall development (Istages)      Zadoks  Rstages
          ! 8 - Sowing date                             00      0.0
          ! 9 - Germination                             05      1.0
          ! 1 - Emergence                               10      1.0
          ! 2 - End spikelet production (=t spikelet)   ??      2.0
          ! 3 - End leaf growth                         40      3.0
          ! 4 - End spike growth                        50      4.0
          ! 5 - End lag phase of grain growth           80      5.0
          ! 6 - End grain fill (Physiological maturity) 90      6.0
          ! 7 - Harvest maturity or harvest             92      6.9
          ! More precisely translated as:
          !  Xstage Zstage
          !      .1  1.00
          !     1.0
          !     2.0
          !     2.5 31.00  Jointing after tsp at 2.0+PD2(1)/PD(2)
          !     3.0 40.00
          !     4.0 57.00
          !     5.0 71.37
          !     5.5 80.68
          !     6.0 90.00
          ! Possible new rstages:
          !  Rstnew?               Xstage Zstage
          !       0 Wetted up         1.0     0.0
          !     1.0 End juvenile
          !     2.0 Double ridges
          !     3.0 Terminal spikelet 2.0
          !         Jointing          2.?    31.0
          !     4.0 Last leaf         3.0    39.0
          !     5.0 Spike emergence          50.0
          !         End spike growth  4.0
          !     6.0 Start anthesis           60.0
          !     7.0 End anthesis             70.0
          !     8.0 End lag           5.0    71.4
          !         End milk          5.5    80.7
          !     9.0 End grain fill    6.0    90.0
          !    10.0 Harvest           6.9    92.0

          IF (ISTAGE.EQ.7) THEN                       ! Pre-planting
            ISTAGE = 8
            XSTAGE = 8.0
          ELSEIF (ISTAGE.EQ.8) THEN                   ! Planted
            XSTAGE = FLOAT(ISTAGE) + GESTAGE*2.0
            IF(GESTAGE.GE.0.5) THEN
              ISTAGE = 9
              XSTAGE = FLOAT(ISTAGE) + (GESTAGE-0.5)*2.0
            ENDIF
          ELSEIF (ISTAGE.EQ.9) THEN                   ! Germination
            XSTAGE = FLOAT(ISTAGE) + (GESTAGE-0.5)*2.0
            IF(GESTAGE.GE.1.0) THEN
              ISTAGE = 1
              XSTAGE = 1.0
            ENDIF
          ELSE                                        ! Emergence on
            ISTAGE = INT(RSTAGE)
            XSTAGE = AMIN1(6.9,RSTAGE)                ! < 7 (=pre-plant)
          ENDIF
          ! Secondary stages
          SSTAGE = AMAX1(0.0,AMIN1(1.0,(XSTAGE-AINT(XSTAGE))))

          ! STAGES:Overall development (Zadoks)
          ! 01=begining of seed imbibition (assumed to be at planting)
          ! 05=germination (assumed to be when the radicle emerged)
          ! 09=coleoptile thru soil surface
          ! 10=first leaf emerged from the coleoptile (= emergence)
          ! 11=first leaf fully expanded --> 1n=nth leaf fully expanded
          ! 20=first tiller appeared on some plants --> 2n=nth tiller
          ! 30=f(reproductive stage)

          IF (XSTAGE.GE.8.0 .AND. XSTAGE.LE.9.0) THEN
            ZSTAGE =  ((XSTAGE-8.0)/2.0)*10.0
          ELSEIF (XSTAGE.GT.9.0) THEN
            ZSTAGE = (0.5+((XSTAGE-9.0)/2.0))*10.0
          ELSEIF (XSTAGE.GE.0.0 .AND. XSTAGE.LE.(2.0+pd2fr(1))) THEN
            IF (TNUM.LT.2.0) THEN
              ZSTAGE = AMIN1(20.0,10.0 + LNUMSD)
            ELSE
              ZSTAGE = AMIN1(30.0,20.0 + (TNUM-1.0))
            ENDIF
            IF (ZSTAGE.LT.ZSTAGEP) ZSTAGE = ZSTAGEP
            ZSTAGEP = ZSTAGE
          ELSEIF (XSTAGE.GT.(2.0+pd2fr(1)) .AND. XSTAGE.LE.3.0) THEN
            ZSTAGE = 30.0 + 10.0*(XSTAGE-(2.0+pd2fr(1)))/(1.0-pd2fr(1))
          ELSEIF (XSTAGE.GT.3.0 .AND. XSTAGE.LE.4.0) THEN
            ZSTAGE = 40.0 + 10.0*(XSTAGE-3.0)
          ELSEIF (XSTAGE.GT.4.0 .AND. XSTAGE.LE.5.0) THEN
            IF (XSTAGE.LT.ASTAGE) THEN
              ZSTAGE = 50.0 + 10.0*((XSTAGE-4.0)/(ASTAGE-4.0))
            ELSEIF (XSTAGE.GE.ASTAGE.AND.XSTAGE.LT.ASTAGEND) THEN
              ZSTAGE = 60.0 + 10.0*((XSTAGE-ASTAGE)/(ASTAGEND-ASTAGE))
            ELSE
              ZSTAGE = 70.0 + 10.0*((XSTAGE-ASTAGEND)/(5.0-ASTAGEND))
            ENDIF
          ELSEIF (XSTAGE.GT.5.0 .AND. XSTAGE.LE.6.0) THEN
            ZSTAGE = 80.0 + 10.0*(XSTAGE-5.0)
          ELSEIF (XSTAGE.GT.6.0 .AND. XSTAGE.LE.7.0) THEN
            ZSTAGE = 90.0 + 10.0*(XSTAGE-6.0)
          ENDIF
          
          GO TO 9345   ! Jump over WDBachelor-CERES calculations
          ! Following were introduced for comparative purposes only
          ! Original CRITICAL AND MINIMA N concentrations.
          ! Below from w_nfacto as reworked by WDB
          ! Conversions from XSTAGE to ZADOKS growth stage
          ! From CROPSIM-CERES
          IF (XSTAGE.GE.8.0 .AND. XSTAGE.LE.9.0) THEN
            ZSTAGE =  ((XSTAGE-8.0)/2.0)
          ELSEIF (XSTAGE.GT.9.0) THEN
            ZSTAGE = (0.5+((XSTAGE-9.0)/2.0))
          ENDIF
          ! From CERES 9WDB)
          IF (XSTAGE .LE. 2.0) THEN
              ZSTAGE = XSTAGE
          ENDIF
          IF (XSTAGE .GT. 2.0 .AND. XSTAGE .LE. 3.0) THEN
              ZSTAGE = 2.00 + 2.0000*(XSTAGE-2.0)
          ENDIF
          IF (XSTAGE .GT. 3.0 .AND. XSTAGE .LE. 4.0) THEN
              ZSTAGE = 4.00 + 1.7000*(XSTAGE-3.0)
          ENDIF
          IF (XSTAGE .GT. 4.0 .AND. XSTAGE .LE. 4.4) THEN
              ZSTAGE = 5.70 + 0.8000*(XSTAGE-4.0)
          ENDIF
          IF (XSTAGE .GT. 4.4 .AND. XSTAGE .LE. 6.0) THEN
              ZSTAGE = 6.02 + 1.8625*(XSTAGE-4.4)
          ENDIF
          YSTAGE = XSTAGE
          ZS2    = ZSTAGE*ZSTAGE
          ZS3    = ZS2*ZSTAGE
          ZS4    = ZS3*ZSTAGE
          IF (P1V .GE. 0.03) THEN
             TCNP = -5.0112400-6.350677*ZSTAGE+14.9578400*SQRT(ZSTAGE)
     1              +0.2238197*ZS2
           ELSE
             TCNP =  7.4531813-1.7907829*ZSTAGE+0.6092849*SQRT(ZSTAGE)
     1              +0.0933967*ZS2
          ENDIF
          IF (ZSTAGE .GT. 6.0) THEN
             TCNP = TCNP - (ZSTAGE-6.0)*0.140
          ENDIF
          TCNP  = TCNP/100.0
          TMNC  = (2.97-0.455*XSTAGE)/100.0
          RCNP  = (2.10-0.14*SQRT(ZSTAGE))*0.01 ! RCNC in original Ceres
          RMNP  = 0.75 * RCNP
          ZSTAGE = ZSTAGE * 10.0
9345      CONTINUE     

          ! Stage dates and characteristics
          ! NB. Characeristics are at end of phase
          IF (ISTAGE.NE.ISTAGEP.AND.ISTAGEP.GT.0) THEN
            STGDOY(ISTAGEP) = YEARDOY
            CWADSTG(ISTAGEP) = CWAD
            LAISTG(ISTAGEP) = LAI
            LNUMSTG(ISTAGEP) = LNUMSD
            CNADSTG(ISTAGEP) = CNAD
          ENDIF
          
          ! Double ridge factors by calibration from LAMS experiment
          drf1 = 1.9
          drf2 = 0.058
          drf3 = 3.3
          !DRSTAGE = AMAX1(1.1,drf1-drf2*(LNUMSD-drf3))
          ! Changed to same as in CCSRP
          DRSTAGE = 1.6
          IF (DRDAT.EQ.-99 .AND. RSTAGE.GE.DRSTAGE) THEN
            DRDAT = YEARDOY
            WRITE(fnumwrk,*)' '
            WRITE(fnumwrk,*)'Double ridges. Stage,Leaf#: ',
     &       DRSTAGE,LNUMSD
             ! NB. Experimental. DR occurs at later apical stage when
             !     leaf # less, earlier when leaf # greater (ie.when
             !     early planting of winter type).
          ENDIF
          IF (TSDAT.EQ.-99 .AND. RSTAGE.GE.2.00) THEN
            TSDAT = YEARDOY
            LNUMTS = LNUMSD
            ! Final leaf# algorithm. LAH Coefficients need to be in file
            FLN = LNUMSD + (2.8 + 0.1 * LNUMTS)
            TVR1 = MOD(FLN,(FLOAT(INT(FLN))))
            IF (TVR1.LT.0.5) THEN
              FLN = FLOAT(INT(FLN))-0.001
            ELSE
              FLN = FLOAT(INT(FLN))+0.999
            ENDIF
            PD2ADJ = ((FLN-LNUMTS)) * PHINTS
            WRITE(fnumwrk,*)' '  
            WRITE(fnumwrk,'(A25,I12)')' Terminal spikelet       ',tsdat
            WRITE(fnumwrk,*)' Terminal spilelet leaf #       ',LNUMTS  
            WRITE(fnumwrk,*)' Final leaf # (Aitken formula)  ',FLN      
            WRITE(fnumwrk,*)' P2 Durations Input,From Aitken ',
     &       PD(2),PD2ADJ
            IF (PD(2).LE.0.0) THEN
              PD(2) = PD2ADJ
              DO L = 2,10
                PTH(L) = PTH(L-1) + PD(L)
              ENDDO
              WRITE(fnumwrk,*)' AITKEN FORMULA USED TO CALCULATE P2  '
            ENDIF 
          ENDIF  
          IF (JDAT.EQ.-99 .AND. RSTAGE.GE.2.0+PD2(1)/PD(2)) THEN
            JDAT = YEARDOY
            WRITE (fnumwrk,*) ' '
            WRITE (fnumwrk,'(A25,I12)') ' Jointing:               ',jdat
          ENDIF
          IF (LLDAT.EQ.-99 .AND. RSTAGE.GE.3.0) THEN
            LLDAT = YEARDOY
            FLNMODEL = LNUMSD
            WRITE (fnumwrk,'(A25,I12)')' Last leaf emergence:    ',lldat
          ENDIF
          IF (IEDAT.EQ.-99 .AND. RSTAGE.GE.4.0) THEN
            IEDAT = YEARDOY
            WRITE (fnumwrk,'(A25,I12)')' Inflorescence emergence:',iedat
          ENDIF
          IF (ADAT.LE.0.0 .AND. RSTAGE.GE.4.0+PD4(1)/PD(4)) THEN
            ADAT = YEARDOY
            WRITE (fnumwrk,'(A25,I12)') ' Anthesis:               ',adat
            RSWAA = RSWAD
            RSCA = RSCD
            CWAA = CWAD
            CNAA = CNAD
            LNPCA = LANC*100.0
            ADATEND = -99
          ENDIF
          IF (ADATEND.LE.0.0 .AND.
     &      RSTAGE.GE.4.0+(PD4(1)+PD4(2))/PD(4)) THEN
            ADATEND = YEARDOY
            TMEAN20A = TMEAN20
            SRAD20A = SRAD20
            STRESS20A = STRESS20
            GRNUM = (LFWT+STWT+RSWT)*G1CWT
            WRITE (fnumwrk,'(A25,I12)')
     &       ' End of anthesis:        ',adatend
            WRITE (fnumwrk,'(A27,F7.1)')
     &       ' Prior 20d mean temperature   ',tmean20a
            WRITE (fnumwrk,'(A27,F7.1)')
     &       ' Prior 20d mean stress factor ',stress20a
            WRITE (fnumwrk,'(A27)')'  NB. 1.0 = 0 stress          '
            WRITE (fnumwrk,*)'Grain #/m2,Nfg ',GRNUM*PLTPOP,NFG
            IF ((GRNUM*PLTPOP).LT.100.0) THEN
              WRITE (fnumwrk,*)'Crop failure - few grains set!'
            ENDIF
          ENDIF
          IF (RSTAGE.GT.ASTAGEND) THEN
            LAGSTAGE = AMAX1(0.0,
     &       AMIN1(1.0,(RSTAGE-ASTAGEND)/(5.0-ASTAGEND)))
          ENDIF

          ! Average nitrogen and water stress, and environmental vars
          ! LAH Changed from 6 to 7 JUNE 2008
          IF (ISTAGEP.GT.0 .AND. ISTAGEP.LE.7) THEN
            NFPC = NFPC + NFP
            NFGC = NFGC + NFG
            WFPC = WFPC + WFP
            WFGC = WFGC + WFG
            ICSDUR = ICSDUR + 1
            tmaxsump = tmaxsump + tmax
            tminsump = tminsump + tmin
            sradsum = sradsum + srad
            daylsum = daylsum + daylt
            co2sum = co2sum + co2
            rainsum = rainsum + rain
            etsum = etsum + et
            epsum = epsum + ep
            ICSDUR0 = ICSDUR0 + 1
            NFPC0 = NFPC0 + NFP
            NFGC0 = NFGC0 + NFG
            WFPC0 = WFPC0 + WFP
            WFGC0 = WFGC0 + WFG
            tmaxsump0 = tmaxsump0 + tmax
            tminsump0 = tminsump0 + tmin
            sradsum0 = sradsum0 + srad
            daylsum0 = daylsum0 + daylt
            co2sum0 = co2sum0 + co2
            rainsum0 = rainsum0 + rain
            etsum0 = etsum0 + et
            epsum0 = epsum0 + ep
            IF (ICSDUR.GT.0) THEN
              NFPAV(ISTAGEP) = NFPC / ICSDUR
              NFGAV(ISTAGEP) = NFGC / ICSDUR
              WFPAV(ISTAGEP) = WFPC / ICSDUR
              WFGAV(ISTAGEP) = WFGC / ICSDUR
              DAYSC(ISTAGEP) = ICSDUR
              tmaxav(ISTAGEP) = tmaxsump / ICSDUR
              tminav(ISTAGEP) = tminsump / ICSDUR 
              sradav(ISTAGEP) = sradsum / ICSDUR
              daylav(ISTAGEP) = daylsum / ICSDUR
              co2av(ISTAGEP) = co2sum / ICSDUR
              raincp(ISTAGEP) = rainsum 
              etc(ISTAGEP)   = etsum 
              epc(ISTAGEP)   = epsum 
              DAYSC(0) = ICSDUR0
              NFPAV(0) = NFPC0 / ICSDUR0
              NFGAV(0) = NFGC0 / ICSDUR0
              WFPAV(0) = WFPC0 / ICSDUR0
              WFGAV(0) = WFGC0 /ICSDUR0
              tmaxav(0) = tmaxsump0 / ICSDUR0
              tminav(0) = tminsump0 / ICSDUR0 
              sradav(0) = sradsum0 / ICSDUR0
              daylav(0) = daylsum0 / ICSDUR0
              co2av(0) = co2sum0 / ICSDUR0
              raincp(0) = rainsum0 
              etc(0)   = etsum0 
              epc(0)   = epsum0 
            ENDIF
            IF (ISTAGE.NE.ISTAGEP) THEN
              NFPAV(ISTAGE) = 1.0
              NFGAV(ISTAGE) = 1.0
              WFPAV(ISTAGE) = 1.0
              WFGAV(ISTAGE) = 1.0
              tmaxav(ISTAGE) = -99.0
              tminav(ISTAGE) = -99.0 
              sradav(ISTAGE) = -99.0
              daylav(ISTAGE) = -99.0
              co2av(ISTAGE) = -99.0
              etc(ISTAGE)   = 0.0
              epc(ISTAGE)   = 0.0
              raincp(ISTAGE) = -99.0
              NFPC = 0.0
              NFGC = 0.0
              WFPC = 0.0
              WFGC = 0.0
              ICSDUR = 0
              tmaxsump = 0.0 
              tminsump = 0.0 
              sradsum = 0.0 
              daylsum = 0.0 
              co2sum = 0.0 
              rainsum = 0.0 
              etsum = 0.0 
              epsum = 0.0 
              IF (ISTAGE.EQ.6) THEN
                NFPAV(ISTAGE) = NFP
                NFGAV(ISTAGE) = NFG
                WFPAV(ISTAGE) = WFP
                WFGAV(ISTAGE) = WFG
                tmaxav(ISTAGE) = TMAX
                tminav(ISTAGE) = TMIN 
                sradav(ISTAGE) = SRAD
                daylav(ISTAGE) = DAYLT
                co2av(ISTAGE) = co2
                etc(ISTAGE)   = 0
                epc(ISTAGE)   = 0
                raincp(ISTAGE) = 0
              ENDIF
            ENDIF
          ENDIF

          ! Phyllochron intervals
          IF (CROP.EQ.'BA'.AND.ISTAGE.NE.ISTAGEP.AND.ISTAGE.EQ.1) THEN
            tvr1 = 77.5 - 232.6*(DAYLT-DAYLTP)
            WRITE(FNUMWRK,*)' '
            WRITE(FNUMWRK,*)
     &       ' PHINT calculated from daylength change: ',tvr1
            WRITE(FNUMWRK,*)
     &       ' PHINT being used:                       ',phints 
          ENDIF
          IF (LNUMSG.GT.0) THEN
            IF (LNUMSD.LT.PHINTL(1)) THEN
              PHINT = PHINTS*PHINTF(1)
            ELSEIF (LNUMSD.GE.PHINTL(1)+PHINTL(2)) THEN
              PHINT = PHINTS*PHINTF(3)
            ELSE
              PHINT = PHINTS
            ENDIF  
          ENDIF

          ! Critical and minimum N concentrations
          IF (ISTAGE.LT.7) THEN
            LCNC = YVAL1(LCNCS,'0','9',XSTAGE)
            SCNC = YVAL1(SCNCS,'0','9',XSTAGE)
            RCNC = YVAL1(RCNCS,'0','9',XSTAGE)
            LMNC = YVAL1(LMNCS,'0','9',XSTAGE)
            SMNC = YVAL1(SMNCS,'0','9',XSTAGE)
            RMNC = YVAL1(RMNCS,'0','9',XSTAGE)
          ELSE
            RCNC = RCNCS(0) + (RCNCS(1)-RCNCS(0))*((XSTAGE-8.0)/2.0)
            RMNC = RMNCS(0) + (RCNCS(1)-RCNCS(0))*((XSTAGE-8.0)/2.0)
          ENDIF

          ! N concentrations and adjustments
          ! (Adjustements to account for changes in criticals)
          RANC = 0.0
          LANC = 0.0
          SANC = 0.0
          VANC = 0.0
          IF (RTWT.GT.0.0) RANC = ROOTN / RTWT
          IF (LFWT.GT.0.0) LANC = LEAFN / LFWT
          IF (STWT.GT.0.0) SANC = STEMN / STWT
          IF (VWAD.GT.0.0) VANC = VNAD/VWAD
          RSNGR = AMAX1(0.0,RTWT*(RANC-RCNC))
          RSNGL = AMAX1(0.0,LFWT*(LANC-LCNC))
          RSNGS = AMAX1(0.0,STWT*(SANC-SCNC))
          RSN = RSN + RSNGR + RSNGL + RSNGS
          ROOTN = ROOTN - RSNGR
          LEAFN = LEAFN - RSNGL
          STEMN = STEMN - RSNGS
          IF (RTWT.GT.0.0) RANC = ROOTN/RTWT
          IF (LFWT.GT.0) LANC = LEAFN/LFWT
          IF (STWT.GT.0.0) SANC = STEMN/STWT
          IF (VWAD.GT.0.0) VANC = VNAD/VWAD
          IF (LANC.LT.0.0) THEN
            WRITE(fnumwrk,*)'LANC below 0 with value of ',LANC
            WRITE(fnumwrk,*)'LEAFN,LFWT had values of   ',LEAFN,LFWT
            LANC = AMAX1(0.0,LANC)
          ENDIF
          IF (LFWT+STWT.GT.0.0) VCNC = 
     &     (LCNC*AMAX1(0.0,LFWT)+SCNC*AMAX1(0.0,STWT))/
     &     (AMAX1(0.0,LFWT)+AMAX1(0.0,STWT))
          IF (LFWT+STWT.GT.0.0) VMNC = 
     &     (LMNC*AMAX1(0.0,LFWT)+SMNC*AMAX1(0.0,STWT))/
     &     (AMAX1(0.0,LFWT)+AMAX1(0.0,STWT))

          CANANC = 0.0
          SDNC = 0.0
          GRAINANC = 0.0
          IF ((LFWT+STWT+GRWT+RSWT+DEADWT).GT.0.0)
     &     CANANC = (LEAFN+STEMN+GRAINN+RSN+DEADN)/
     &      (LFWT+STWT+GRWT+RSWT+DEADWT)
          IF (SEEDRS.GT.0.0) SDNC = SEEDN/(SEEDRS+SDCOAT)
          IF (GRWT.GT.0) GRAINANC = GRAINN/GRWT

          LCNF = 0.0
          SCNF = 0.0
          RCNF = 0.0
          IF (LCNC.GT.0.0) LCNF = LANC/LCNC
          IF (LCNF.GT.1.0001 .OR. LCNF.LT.0.0) THEN
            WRITE(fnumwrk,*)'LCNF out of limits with value of ',LCNF
            LCNF = AMAX1(0.0,AMIN1(1.0,LCNF))
          ENDIF
          IF (SCNC.GT.0.0.AND.STWT.GT.1.0E-10) SCNF = SANC/SCNC
          IF (RCNC.GT.0.0.AND.RTWT.GT.0.0) RCNF = RANC/RCNC

          ! Harvesting conditions
          IF (IHARI.EQ.'A' .AND. ISTAGE.EQ.6) THEN
            ! Here need to check out if possible to harvest.
            IF (YEARDOY.GE.HFIRST) THEN
              IF (SW(1).GE.SWPLTL.AND.SW(1).LE.SWPLTH) YEARHARF=YEARDOY
            ENDIF
            ! Check if past earliest date; check if not past latest date
            ! Check soil water
            ! If conditions met set YEARHARF = YEARDOY
            ! (Change YEARHARF to more something more appropriate)
          ENDIF

          ! Harvesting or failure
          IF (DAP.GE.90 .AND. ISTAGE.EQ.8) THEN
            CFLFAIL = 'Y'
            WRITE (FNUMWRK,*)'No germination within 90 days of sowing!'
          ENDIF
          
          IF (IHARI.NE.'A'.AND.DAPM.GE.90) THEN
            CFLFAIL = 'Y'
            WRITE (FNUMWRK,*)'90 days after physiological maturity!'
            WRITE (FNUMWRK,*)'Harvesting triggered!'
          ENDIF
          
          IF (IHARI.NE.'A'.AND.ISTAGE.GE.4.AND.ISTAGE.LT.7) THEN
            IF (TT20.NE.-99.0.AND.TT20.LE.0.0) THEN
              CFLFAIL = 'Y'
              WRITE (FNUMWRK,*)'20day thermal time mean = 0!'
              WRITE (FNUMWRK,*)'Harvesting triggered!'
            ENDIF
          ENDIF

          IF (CFLFAIL.EQ.'Y' .OR.
     &     IHARI.EQ.'R'.AND.YEARHARF.GT.-99.AND.YEARHARF.EQ.YEARDOY .OR.
     &     IHARI.EQ.'D'.AND.YEARHARF.GT.-99.AND.YEARHARF.EQ.DAP .OR.
     &     IHARI.EQ.'G'.AND.YEARHARF.GT.-99.AND.YEARHARF.LE.XSTAGE .OR.
     &     IHARI.EQ.'A'.AND.YEARHARF.GT.-99.AND.YEARHARF.EQ.YEARDOY .OR.
     &     IHARI.EQ.'M'.AND.XSTAGE.GE.6.0.AND.XSTAGE.LT.7.0 .OR.
     &     YEARHARF.LE.-99 .AND. XSTAGE.GE.6.9.AND.XSTAGE.LT.7.0) THEN
            IF (CFLFAIL.EQ.'Y') THEN
              STGDOY(10) = YEARDOY
              IF (STGDOY(9).EQ.9999999) STGDOY(9) = -99
              IF (STGDOY(8).EQ.9999999) STGDOY(8) = -99
              IF (STGDOY(5).EQ.9999999) STGDOY(5) = -99
              IF (STGDOY(4).EQ.9999999) STGDOY(4) = -99
              IF (STGDOY(3).EQ.9999999) STGDOY(3) = -99
              IF (STGDOY(2).EQ.9999999) STGDOY(2) = -99
              IF (STGDOY(1).EQ.9999999) STGDOY(1) = -99
              WFPAV(6) = WFPAV(ISTAGE)
              NFPAV(6) = NFPAV(ISTAGE)
            ENDIF
            IF (STGDOY(10).EQ.9999999) STGDOY(10) = -99
            STGDOY(6) = YEARDOY
            STGDOY(11) = YEARDOY
            CWADSTG(6) = CWAD
            LAISTG(6) = LAI
            LNUMSTG(6) = LNUMSD
            CNADSTG(6) = CNAD
            
            ! If running CSM use harvfrac to handle automatic mngement
            ! LAH May 2009 Was NE  CHP to check
            IF (FILEIOT .EQ. 'DS4') THEN
              hpc = harvfrac(1)*100.0   ! Harvest %
              hbpc = harvfrac(2)*100.0
            ENDIF  
 
            WRITE(fnumwrk,*)' '
            WRITE(fnumwrk,*)'HARVEST REACHED ',YEARDOY
            
            
            PARIUEM = -99.0
            IF (PARADCUM.GT.0.0.AND.PARADICUM.GT.0.0) THEN
              WRITE (fnumwrk,*)' '
              WRITE (fnumwrk,'(A53,F5.1,F4.1)')
     &         ' OVERALL PAR USE EFFICIENCY (INCIDENT,INTERCEPTED) = ',
     &         CWAD/PARADCUM/10.0, CWAD/PARADICUM/10.0 
               PARIUEM = CWAD/PARADICUM/10.0
            ENDIF  
                  
            WRITE(FNUMWRK,*) ' '
            WRITE(FNUMWRK,*) 'INORGANIC NO3 AND NH4 (kg/ha)'
            WRITE(FNUMWRK,*) ' PROFILE:  '
            WRITE(FNUMWRK,'(A15,2F6.1)')
     &        '  SEASON START:',SNO3PROFILEI,SNH4PROFILEI
            WRITE(FNUMWRK,'(A15,2F6.1)')
     &        '  SEASON END:  ',SNO3PROFILE,SNH4PROFILE
            WRITE(FNUMWRK,*) ' ROOTZONE: '
            WRITE(FNUMWRK,'(A15,2F6.1)')
     &        '  SEASON END:  ',SNO3ROOTZONE,SNH4ROOTZONE
 
            WRITE(FNUMWRK,*) ' '
            WRITE(FNUMWRK,*) 'TOTAL AND AVAILABLE WATER (mm) '
            WRITE(FNUMWRK,*) ' PROFILE:  '
            WRITE(FNUMWRK,'(A15,2F6.1)')
     &        '  SEASON START:',H2OPROFILEI,AH2OPROFILEI
            WRITE(FNUMWRK,'(A15,2F6.1)') 
     &       '  SEASON END:  ',H2OPROFILE,AH2OPROFILE
            WRITE(FNUMWRK,*) ' ROOTZONE: '
            WRITE(FNUMWRK,'(A15,2F6.1)')
     &        '  SEASON END:  ',H2OROOTZONE,AH2OROOTZONE
            
            ! Reset crop stage
            ISTAGE = 7
            XSTAGE = 7.0
          ENDIF

          IF(IHARI.EQ.'R'.AND.YRHARF.GT.-99.AND.YEARDOY.LT.YEARHARF)THEN
            IF (XSTAGE.GT.6.9 .AND. YRHARFF .NE. 'Y') THEN
              ! This loop is necessary because of non-sequential staging
              IF (XSTAGE.LT.7.0) THEN
                WRITE(fnumwrk,*)
     &           'WAITING FOR HARVEST! YEARDOY,YRHAR ',YEARDOY,YRHARF
                YRHARFF = 'Y'
              ENDIF
            ENDIF
          ENDIF

          ! After harvest residues
          IF (STGDOY(11).EQ.YEARDOY) THEN
            ! Surface
            RESWALG(0) = VWAD*(1.0-HBPC/100.0) + GWAD*(1.0-HPC/100.0)
            RESNALG(0) = (LEAFN+STEMN+DEADN)*PLTPOP*10.*(1.-HBPC/100.)
     &                 + GNAD*(1.0-HPC/100.0)
            RESCALG(0) = RESWALG(0) * 0.4
            RESLGALG(0) = LLWAD*LLIGP/100.0*(1.0-HBPC/100.0)
     &                  + LSHWAD*SLIGP/100.0*(1.0-HBPC/100.0)
     &                  + STWAD*SLIGP/100.0*(1.0-HBPC/100.0)
     &                  + GWAD*GLIGP/100.0*(1.0-HPC/100.0)
            ! Soil
            DO L = 1, NLAYR
              RESWALG(L) = RTWTL(L)*PLTPOP*10.0
              RESNALG(L) = RTWTL(L)*PLTPOP*10.0 * RANC
              RESCALG(L) = RTWTL(L)*PLTPOP*10.0 * 0.4
              RESLGALG(L) = RTWTL(L)*PLTPOP*10.0 * RLIGP/100.0
            ENDDO

            ! Surface
            RESWAL(0) = RESWAL(0) + RESWALG(0)
            RESNAL(0) = RESNAL(0) + RESNALG(0)
            RESCAL(0) = RESCAL(0) + RESCALG(0)
            RESLGAL(0) = RESLGAL(0) + RESLGALG(0)
            ! Soil
            DO L = 1, NLAYR
              RESWAL(L) = RESWAL(L) + RESWALG(L)
              RESNAL(L) = RESNAL(L) + RESNALG(L)
              RESCAL(L) = RESCAL(L) + RESCALG(L)
              RESLGAL(L) = RESLGAL(L) + RESLGALG(L)
            ENDDO
          ENDIF

          ! Weather summary variables
          CUMTT = CUMTT + TT
          TMAXX = AMAX1(TMAXX,TMAX)
          TMINN = AMIN1(TMINN,TMIN)
          CO2MAX = AMAX1(CO2MAX,CO2)
          RAINC = RAINC + RAIN
          IF (ADAT.LT.0) RAINCA = RAINCA + RAIN
          SRADC = SRADC + SRAD
          IF (XSTAGE.GE.5.0 .AND. XSTAGE.LT.6.0) THEN
            GFTSUM = GFTSUM + TMEAN
            GFDSUM = GFDSUM + 1
            GFTMEAN = GFTSUM/GFDSUM
          ENDIF
          IF (XSTAGE.GE.5.7 .AND. XSTAGE.LT.6.0) THEN
            GMTSUM = GMTSUM + TMEAN
            GMDSUM = GMDSUM + 1
            GMTMEAN = GMTSUM/GMDSUM
          ENDIF
          IF (XSTAGE.GE.8.0 .AND. XSTAGE.LT.10.0) THEN
            GETSUM = GETSUM + TMEAN
            GEDSUM = GEDSUM + 1
            GETMEAN = GETSUM/GEDSUM
          ENDIF
          CALL Calendar (year,doy,dom,month)
          IF (DOM.GT.1) THEN
            TMAXSUM = TMAXSUM + TMAX
            TMINSUM = TMINSUM + TMIN
            DAYSUM = DAYSUM + 1.0
          ELSE
            IF (DAYSUM.GT.0) THEN
              IF (TMAXM.LT.TMAXSUM/DAYSUM) TMAXM=TMAXSUM/DAYSUM
              IF (TMINM.GT.TMINSUM/DAYSUM) TMINM=TMINSUM/DAYSUM
            ENDIF
              TMAXSUM = TMAX
              TMINSUM = TMIN
              DAYSUM =  1
          ENDIF

          ! N fertilizer applications
          IF (NFERT.GT.0.AND.IFERI.EQ.'R'.AND.YEARDOY.EQ.YEARPLT) THEN
            DO I = 1, NFERT
              IF (FDAY(I).GT.YEARDOY) EXIT
              IF (FDAY(I).LE.-99) EXIT
              AMTNIT = AMTNIT + ANFER(I)
            END DO
            IF (FILEIOT.EQ.'XFL') WRITE(fnumwrk,*)' '
            WRITE(fnumwrk,'(A24,I4,A6)')
     &       ' Fertilizer N pre-plant ',NINT(amtnit),' kg/ha'
          ENDIF
          IF (NFERT.GT.0.AND.IFERI.EQ.'R'.AND.YEARDOY.GT.YEARPLT) THEN
            DO I = 1, NFERT
              IF (FDAY(I).GT.YEARDOY) EXIT
              IF (FDAY(I).EQ.YEARDOY) THEN
                AMTNIT = AMTNIT + ANFER(I)
                WRITE(fnumwrk,'(A14,I4,A10,I9,A13,I4,A6)')
     &          ' Fertilizer N ',NINT(anfer(i)),' kg/ha on ',
     &          YEARDOY,'     To date ',NINT(amtnit),' kg/ha'
              ENDIF
            END DO
          ENDIF

          ! Adjustment of kernel growth rate
          ! Originally set temperature response here
          IF (ISTAGE.EQ.5.AND.ISTAGEP.EQ.4) THEN
            WRITE(fnumwrk,*)'Start of linear kernel growth    '
            WRITE(fnumwrk,*)' Original kernel growth rate (G2) ',g2
!           chp handle zero divide
            IF (GRNUM .GT. 1.E-6) THEN
              G2 = (G2KWT-(GRWT/GRNUM)*1000.0) / (PD(5)*(6.0-XSTAGE))
            ELSE
              G2 = (G2KWT) / (PD(5)*(6.0-XSTAGE))
            ENDIF
            WRITE(fnumwrk,*)' Adjusted kernel growth rate (G2) ',g2
            WRITE(fnumwrk,*)' (Adjustment because growing at lag rate',
     &      ' for overlap into linear filling period)'
          ENDIF

          ! Stored variables (For use next day or step)
          ISTAGEP = ISTAGE
          ZSTAGEP = ZSTAGE
          RSTAGEP = RSTAGE
          DAYLTP = DAYLT

          ! Soil water aspects
          BLAYER = 0.0
          H2OA = 0.0
          IF (ISWWAT.EQ.'Y') THEN
            DO L = 1, NLAYR
              DLAYRTMP(L) = DLAYR(L)
              BLAYER = BLAYER + DLAYR(L)
              IF (RTDEP.GT.0.0.AND.RTDEP.LT.BLAYER) THEN
                DLAYRTMP(L) = RTDEP-(BLAYER-DLAYR(L))
                IF (DLAYRTMP(L).LE.0.0) EXIT
              ENDIF
              H2OA = H2OA + 10.0*AMAX1(0.0,(SW(L)-LL(L))*DLAYRTMP(L))
            ENDDO
            IF (EOP.GT.0.0) THEN
              WAVR = H2OA/EOP
            ELSE
              WAVR = 99.9
            ENDIF
          ENDIF
 
          H2OPROFILE = 0.0
          H2OROOTZONE = 0.0
          AH2OPROFILE = 0.0
          AH2OROOTZONE = 0.0
          DO L = 1, NLAYR
            AH2OPROFILE = AH2OPROFILE + ((SW(L)-LL(L))*DLAYR(L))*10.0
            H2OPROFILE = H2OPROFILE + SW(L)*DLAYR(L)*10.0
            IF (RLV(L).GT.0.0) THEN
              AH2OROOTZONE = AH2OROOTZONE + ((SW(L)-LL(L))*DLAYR(L))*10.
              H2OROOTZONE = H2OROOTZONE + SW(L)*DLAYR(L)*10.
            ENDIF
          END DO

        ENDIF


      ELSEIF (DYNAMIC.EQ.OUTPUT .OR. 
     &        DYNAMIC.EQ.SEASEND .AND. SEASENDOUT.NE.'Y') THEN

        IF (YEARDOY.GE.YEARPLT .AND. STEP.EQ.STEPNUM) THEN             
 
          ! General file header
          IF (TN.LT.10) THEN
            WRITE (OUTHED,7104) RUNRUNI(1:5),EXCODE,TN,TRUNNAME
 7104       FORMAT ('*RUN ',A5,':',A10,' ',I1,' ',A40,'  ')
          ELSEIF (TN.GE.10. AND. TN.LT.100) THEN
           WRITE (OUTHED,7105) RUNRUNI,EXCODE,TN,RN,TRUNNAME
 7105      FORMAT ('*RUN ',A5,A10,' ',I2,',',I1,' ',A40,' ')
          ELSEIF (TN.GE.10 .AND. TN.LT.100) THEN
           WRITE (OUTHED,7106) RUNRUNI,EXCODE,TN,RN,TRUNNAME
 7106      FORMAT ('*RUN ',A5,A10,' ',I3,',',I1,' ',A40)
          ENDIF
 
          ! If seeding day
          IF (YEARDOY.EQ.STGDOY(7)) THEN
            CNCHAR = ' '
            IF (CN.EQ.1) THEN
              OUTPG = 'PlantGro.OUT'
              OUTPG2 = 'PlantGr2.OUT'
              OUTPGF = 'PlantGrf.OUT'
              OUTPN = 'PlantN.OUT  '

! File names are changed at end of simulation by CSM
! Changing names here eliminates wheat output in sequence runs.
              !IF (FNAME.EQ.'Y') THEN
              !  OUTPG = EXCODE(1:8)//'.OPG'
              !  OUTPG2 = EXCODE(1:8)//'.OG2'
              !  OUTPGF = EXCODE(1:8)//'.OGF'
              !  OUTPN = EXCODE(1:8)//'.ONI'
              !ENDIF

              CALL GETLUN ('PlantGro.OUT',NOUTPG)
              CALL GETLUN ('PlantN.OUT',NOUTPN)
              CALL GETLUN ('PlantGr2.OUT',NOUTPG2)
              CALL GETLUN ('PlantGrf.OUT',NOUTPGF)
            ELSE
              CNCHAR = TL10FROMI(CN)
              OUTPG = 'PlantGro.OU'//CNCHAR(1:1)
              OUTPN = 'PlantN.OU'//CNCHAR(1:1)
              CALL GETLUN (OUTPG,NOUTPG)
              CALL GETLUN (OUTPN,NOUTPN)
              CALL GETLUN (OUTPG2,NOUTPG2)
              CALL GETLUN (OUTPGF,NOUTPGF)
            ENDIF
            ! Open output file(s)
            IF (RUN.EQ.1 .AND. RUNI.LE.1) THEN
              OPEN (UNIT = NOUTPG, FILE = OUTPG)
              WRITE (NOUTPG,'(A27)')
     &        '$GROWTH ASPECTS OUTPUT FILE'
              OPEN (UNIT = NOUTPG2, FILE = OUTPG2)
              WRITE (NOUTPG2,'(A37)')
     &        '$GROWTH ASPECTS SECONDARY OUTPUT FILE'
              OPEN (UNIT = NOUTPN, FILE = OUTPN)
              WRITE (NOUTPN,'(A35)')
     &        '$PLANT NITROGEN ASPECTS OUTPUT FILE'
              OPEN (UNIT = NOUTPGF, FILE = OUTPGF)
              WRITE (NOUTPGF,'(A26)')
     &        '$GROWTH FACTOR OUTPUT FILE'
              CLOSE (NOUTPG)
              CLOSE (NOUTPG2)
              CLOSE (NOUTPN)
              CLOSE (NOUTPGF)
            ENDIF
 
            IF (IDETG.NE.'N'.AND.IDETL.NE.'0') THEN
              OPEN (UNIT = NOUTPG, FILE = OUTPG, STATUS='UNKNOWN',
     &        POSITION = 'APPEND')
              OPEN (UNIT = NOUTPN, FILE = OUTPN, STATUS = 'UNKNOWN',
     &        POSITION = 'APPEND')
     
              IF (FILEIOT(1:2).EQ.'DS') THEN
                CALL HEADER(2, NOUTPG, RUN)
                CALL HEADER(2, NOUTPN, RUN)
              ELSE
               WRITE (NOUTPG,'(/,A77,/)') OUTHED
               WRITE (NOUTPN,'(/,A77,/)') OUTHED
               WRITE (NOUTPG,103) MODEL
               WRITE (NOUTPN,103) MODEL
  103          FORMAT (' MODEL            ',A8)
               WRITE (NOUTPG,1031) MODNAME
               WRITE (NOUTPN,1031) MODNAME
 1031          FORMAT (' MODULE           ',A8)
               WRITE (NOUTPG,104)
     &          EXCODE(1:8),' ',EXCODE(9:10),'  ',ENAME(1:47)
               WRITE (NOUTPN,104)
     &          EXCODE(1:8),' ',EXCODE(9:10),'  ',ENAME(1:47)
  104          FORMAT (' EXPERIMENT       ',A8,A1,A2,A2,A47)
               WRITE (NOUTPG,102) TN,TNAME
               WRITE (NOUTPN,102) TN,TNAME
  102          FORMAT (' TREATMENT',I3,'     ',A25)
               WRITE (NOUTPG,107) CROP,VARNO,VRNAME
               WRITE (NOUTPN,107) CROP,VARNO,VRNAME
  107          FORMAT (' GENOTYPE         ',A2,A6,'  ',A16)
               CALL Calendar (year,doy,dom,month)
               WRITE (NOUTPG,108) month,dom,NINT(pltpop),NINT(rowspc)
               WRITE (NOUTPN,108) month,dom,NINT(pltpop),NINT(rowspc)
  108          FORMAT(' ESTABLISHMENT    ',A3,I3,2X,I4,' plants/m2 in ',
     &          I3,' cm rows',/)
              ENDIF
              ! Write variable headings
              WRITE (NOUTPG,2201)
 2201         FORMAT ('@YEAR DOY   DAS   DAP TMEAN TKILL',
     &        '  GSTD  L#SD',
     &        ' PARID PARUD  AWAD',
     &        '  LAID  SAID  CAID',
     &        '  TWAD SDWAD  RWAD  CWAD  LWAD  SWAD  HWAD  HIAD',
     &        ' CHWAD  EWAD RSWAD SNWPD SNWLD SNWSD',
     &        '  RS%D',
     &        '  H#AD  HWUD',
     &        '  T#AD  SLAD  RDPD  PTFD',
     &        '  SWXD WAVRD',
     &        ' WUPRD  WFTD  WFPD  WFGD',
     &        '  NFTD  NFPD  NFGD NUPRD',
     &        '  TFPD  TFGD',
     &        ' VRNFD DYLFD')
     
              WRITE (NOUTPN,2251)
!             2021-02-15 chp Change NUAD to NUAC in header.
 2251         FORMAT ('@YEAR DOY   DAS   DAP TMEAN  GSTD  NUAC',
     &        '  TNAD SDNAD  RNAD  CNAD  LNAD  SNAD  HNAD  HIND',
     &        ' RSNAD SNNPD SNN0D SNN1D',
     B        '  RN%D  LN%D  SN%D  HN%D SDN%D  VN%D',
     C        ' LN%RD SN%RD RN%RD  VCN%  VMN% NUPRD',
     &        ' NDEMD')
  
              ! Delete PlantN if N switched off   
              IF (ISWNIT.NE.'Y') THEN
                CLOSE (UNIT=NOUTPN, STATUS = 'DELETE')
              ENDIF
     
              OPEN (UNIT = NOUTPG2, FILE = OUTPG2, STATUS='UNKNOWN',
     &        POSITION = 'APPEND')
              IF (FILEIOT(1:2).EQ.'DS') THEN
                CALL HEADER(2, NOUTPG2, RUN)
              ELSE
                WRITE (NOUTPG2,'(/,A79,/)') OUTHED
                WRITE (NOUTPG2,103) MODEL
                WRITE (NOUTPG2,1031) MODNAME
                WRITE (NOUTPG2,104)
     &           EXCODE(1:8),' ',EXCODE(9:10),'  ',ENAME(1:47)
                WRITE (NOUTPG2,102) TN,TNAME
                WRITE (NOUTPG2,107) CROP,VARNO,VRNAME
                WRITE(NOUTPG2,108)
     &           month,dom,yearplt,NINT(pltpopp),NINT(rowspc)
              ENDIF 
              WRITE (NOUTPG2,2252)
 2252         FORMAT ('@YEAR DOY   DAS   DAP TMEAN  GSTD  RSTD',
     A        ' LAIPD LAISD  LAID  CHTD SDWAD SNWLD SNWSD',
     a        '  H#AD  HWUD',
     B        ' SHRTD  PTFD  RDPD',
     C        '  RL1D  RL2D  RL3D  RL4D  RL5D  RL6D',
     D        '  RL7D  RL8D  RL9D RL10D')
     
            ! PlantGroReductionFactors
              OPEN (UNIT = NOUTPGF, FILE = OUTPGF, STATUS='UNKNOWN',
     &        POSITION = 'APPEND')  !chp 4/16/10
              IF (FILEIOT(1:2).EQ.'DS') THEN
                CALL HEADER(2, NOUTPGF, RUN)
              ELSE
                WRITE (NOUTPGF,'(/,A79,/)') OUTHED
                WRITE (NOUTPGF,103) MODEL
                WRITE (NOUTPGF,1031) MODNAME
                WRITE (NOUTPGF,104)
     &         EXCODE(1:8),' ',EXCODE(9:10),'  ',ENAME(1:47)
                WRITE (NOUTPGF,102) TN,TNAME
                WRITE (NOUTPGF,107) CROP,VARNO,VRNAME
                CALL Calendar (year,doy,dom,month)
                WRITE(NOUTPGF,108)
     &           month,dom,yearplt,NINT(pltpopp),NINT(rowspc)
              ENDIF
              WRITE (NOUTPGF,2215)
 2215         FORMAT ('!........DATES.......  TEMP STAGE ',
     N        ' ...PHENOLOGY.... ',
     1        ' .......PHOTOSYNTHESIS....... ', 
     M        ' .....GROWTH.....  ..TILLERS. ',
     2        'WATER STRESS DETERMINANTS',
     2        ' N STRESS DETERMINANTS       ')
              WRITE (NOUTPGF,2205)
 2205         FORMAT ('@YEAR DOY   DAS   DAP TMEAN  GSTD',
     N        '    DU VRNFD DYLFD TFGEM  WFGE',
     1        '  TFPD  WFPD  NFPD CO2FD RSFPD', 
     M        '  TFGD  WFGD  NFGD  WFTD  NFTD',
     &        ' WAVRD WUPRD  SWXD  EOPD',
     &        '  SNXD LN%RD SN%RD RN%RD      ')
     
            ELSE
              OPEN (UNIT=NOUTPG, FILE=OUTPG, STATUS = 'UNKNOWN')
              CLOSE (UNIT=NOUTPG, STATUS = 'DELETE')
              OPEN (UNIT=NOUTPN, FILE=OUTPN, STATUS = 'UNKNOWN')
              CLOSE (UNIT=NOUTPN, STATUS = 'DELETE')
              OPEN (UNIT=NOUTPG2, FILE=OUTPG2, STATUS = 'UNKNOWN')
              CLOSE (UNIT=NOUTPG2, STATUS = 'DELETE')
              OPEN (UNIT=NOUTPGF, FILE=OUTPGF, STATUS = 'UNKNOWN')
              CLOSE (UNIT=NOUTPGF, STATUS = 'DELETE')
            ENDIF
          ENDIF
 
          IF ((MOD(DAS,FROPADJ).EQ.0)
     &     .OR. (YEARDOY.EQ.STGDOY(7))
     &     .OR. (YEARDOY.EQ.STGDOY(11))) THEN
            IF (IDETG.NE.'N'.OR.IDETL.EQ.'0') THEN
              OUTCHOICE = 1
              ! Note possibilities. To change must recompile.
              IF (OUTCHOICE.EQ.1) THEN
                ! 1. Include reserves. Stem includes sheath
                LLWADOUT = AMAX1(0.0,LLWAD+LLRSWAD)
                LSHWADOUT = AMAX1(0.0,LSHWAD+LSHRSWAD)
                STWADOUT = AMAX1(0.0,STWAD+STRSWAD+LSHWAD+LSHRSWAD)
                SAIDOUT = SAID + LSHAI
                CHWADOUT = AMAX1(0.0,CHWAD+CHRSWAD)
                IF (LFWT.GT.1.0E-6) 
     &           SLAOUT = (PLA-SENLA) / (LFWT*(1.0-LSHFR)+LLRSWT)
              ELSEIF (OUTCHOICE.EQ.2) THEN
                ! 2. No reserves. Stem does not includes sheath
                LLWADOUT = AMAX1(0.0,LLWAD)
                LSHWADOUT = AMAX1(0.0,LSHWAD)
                STWADOUT = AMAX1(0.0,STWAD)
                SAIDOUT = SAID
                CHWADOUT = AMAX1(0.0,CHWAD)
                SLAOUT = -99.0
                IF (LFWT.GT.1.0E-6) 
     &           SLAOUT = (PLA-SENLA) / (LFWT*(1.0-LSHFR))
              ENDIF  
              IF (SLA.LE.0.0) SLAOUT = -99.0
              
              CALL Csopline(senw0c,senwal(0))
              CALL Csopline(senwsc,senwas)
              IF (PARIP.GE.0.0) THEN
                PARIOUT = PARIP/100.0
              ELSE
                PARIOUT = PARI
              ENDIF
              CALL Csopline(hwudc,gwud)

              IF (IDETG.NE.'N') THEN
              WRITE (NOUTPG,
     &        '(I5,I4,2I6,2F6.1,
     &        2F6.2,
     &        2F6.2,F6.1,F6.2,F6.3,
     &        F6.2,
     &        7I6,F6.2,
     &        4I6,2A6,
     &        F6.1,
     &        I6,A6,
     &        2I6,2F6.2,
     &        2F6.1,
     &        4F6.2,
     &        3F6.2,F6.1,
     &        2F6.2,
     &        2F6.2,
     &        F6.1)')      
     &        YEAR,DOY,DAS,DAP,TMEAN,TKILL,
     &        ZSTAGE,LNUMSD,   ! Zadoks staging
!     &        XSTAGE,LNUMSD,    ! Ceres staging
     &        PARIOUT,PARUED,AMIN1(999.9,CARBOA),
     &        LAI,SAIDOUT,LAI+SAIDOUT,
     &        NINT(TWAD),
     &        NINT(SDWAD),
     &        NINT(RWAD),
     &        NINT(CWAD),
     &        NINT(LLWADOUT),
     &        NINT(STWADOUT),NINT(GWAD),HIAD,
     &        NINT(CHWADOUT),
     &        NINT(EWAD),
     &        NINT(RSWAD),
     &        NINT(DWAD),SENW0C,SENWSC,
     &        RSCD*100.0,
     &        NINT(GRNUMAD),HWUDC,
              ! NB. SLA includes reserves   
     &        NINT(TNUMAD),NINT(SLAOUT),RTDEP/100.0,PTF,
     &        H2OA,AMIN1(99.9,WAVR),
     &        AMIN1(15.0,WUPR),1.0-WFT,1.0-WFP,1.0-WFG,
     &        1.0-NFT,1.0-NFP,1.0-NFG,AMIN1(2.0,NUPR),
     &        1.0-TFP,1.0-TFG,
     &        1.0-VF,1.0-DF 

!     VSH CSV output corresponding to PlantGro.OUT
      IF (FMOPT == 'C') THEN 
         CALL CsvOut(EXCODE, RUN,TN,RN,SN, ON, REP, CN, YEAR,DOY,
     &DAS, DAP, TMEAN, TKILL, ZSTAGE, LNUMSD, PARIOUT, PARUED, CARBOA, 
     &LAI, SAIDOUT, TWAD, SDWAD, RWAD, CWAD, LLWADOUT, STWADOUT, GWAD,
     &HIAD, CHWADOUT, EWAD, RSWAD, DWAD, SENW0C, SENWSC, RSCD, GRNUMAD,
     &HWUDC, TNUMAD, SLAOUT, RTDEP, PTF, H2OA, WAVR, WUPR, WFT, WFP,
     &WFG, NFT, NFP, NFG, NUPR, TFP, TFG, VF, DF,
     &vCsvlineCsCer, vpCsvlineCsCer, vlngthCsCer)
     
         CALL LinklstCsCer(vCsvlineCsCer)
      END IF

              LAIPROD = PLA*PLTPOP*0.0001
              CALL Csopline(laic,lai)
              CALL Csopline(laiprodc,laiprod)
              CALL Csopline(canhtc,canht)
              CALL Csopline(gstagec,Zstage)
              !L = MAX(1,LNUMSG-INT(LLIFG))
              WRITE (NOUTPG2,503)
     A         YEAR,DOY,DAS,DAP,TMEAN,GSTAGEC,RSTAGE,
     B         LAIPRODC,SENLA*PLTPOP*0.0001,LAIC,CANHTC,SDWAD,
     &         SENW0C,SENWSC,
     &         NINT(GRNUMAD),hwudc,
     D         SHRTD,PTF,RTDEP/100.0,(RLV(I),I=1,10)
  503         FORMAT(
     A         I5,I4,2I6,F6.1,A6,F6.2,
     B         A6,F6.2,A6,A6,F6.1,2A6,I6,A6,
     D         2F6.2,F6.3,10F6.2)
             
!     VSH CSV output corresponding to PlantGr2.OUT
      IF (FMOPT == 'C') THEN
         CALL CsvOutPlGr2(EXCODE, RUN,TN,RN,SN, ON, REP, CN, YEAR,
     &DOY, DAS, DAP, TMEAN, GSTAGEC, RSTAGE, LAIPRODC, SENLA, PLTPOP, 
     &LAIC, CANHTC, SDWAD, SENW0C, SENWSC, GRNUMAD, hwudc, SHRTD, PTF,
     &RTDEP, NL, RLV,
     &vCsvlinePlGr2, vpCsvlinePlGr2, vlngthPlGr2)
     
         CALL LinklstPlGr2(vCsvlinePlGr2)
      END IF 
            
              ! Plant Growth factors outputs
              WRITE (NOUTPGF,507)
     A        YEAR,DOY,DAS,DAP,TMEAN,ZSTAGE,
     B        DU,1.0-VF,1.0-DF,1.0-TFGEM,1.0-WFGE,
     C        1.0-TFP,1.0-WFP,1.0-NFP,1.0-CO2FP,1.0-RSFP,
     D        1.0-TFG,1.0-WFG,1.0-NFG,1.0-WFT,1.0-NFT,
     H        AMIN1(99.9,WAVR),AMIN1(15.0,WUPR),H2OA,EOP,
     I        SNH4PROFILE+SNO3PROFILE,LCNF,SCNF,RCNF
  507         FORMAT(
     a        I5,I4,2I6,F6.1,F6.1,
     b        F6.1,4F6.2,
     c        5F6.2,
     d        5F6.2,
     e        2F6.2,2F6.1,F6.1,3F6.2)
 
 !    VSH CSV output corresponding to PlantGrf.OUT
      IF (FMOPT == 'C') then
       CALL CsvOutPlGrf(EXCODE, RUN,TN,RN,SN, ON, REP, CN, YEAR,DOY,
     &DAS, DAP, TMEAN, ZSTAGE, DU, VF, DF, TFGEM, WFGE, TFP, WFP, NFP, 
     &CO2FP, RSFP, TFG, WFG, NFG, WFT, NFT, WAVR, WUPR, H2OA, EOP, 
     &SNH4PROFILE, SNO3PROFILE, LCNF, SCNF, RCNF, 
     &vCsvlinePlGrf, vpCsvlinePlGrf, vlngthPlGrf)
     
         CALL LinklstPlGrf(vCsvlinePlGrf)
      END IF
 
!!             2021-02-14 chp 
!!             NUAD should be a daily variable, but here it's cumulative. 
!!             Introduce a new variable that is daily.
!              Nuptake_daily = NUAD - NUAD_Y

              ! Plant N outputs
              IF (ISWNIT.EQ.'Y') THEN
                CALL Csopline(senn0c,sennal(0))
                CALL Csopline(sennsc,sennas)
                WRITE (NOUTPN,'(
     &           I5,I4,2I6,F6.1,F6.2,F6.1,
     &           F6.1,F6.2,
     &           F6.2,
     &           3F6.1,
     &           1F6.1,3F6.2,2A6,
     &           3F6.3,
     &           3F6.3,
     &           3F6.2,
     &           F6.1,F6.2,
     &           2F6.2)')
     &           YEAR,DOY,DAS,DAP,TMEAN,ZSTAGE,NUAD,
!    &           YEAR,DOY,DAS,DAP,TMEAN,ZSTAGE,
!    &           Nuptake_daily,
     &           TNAD,SDNAD,
     &           RNAD,
     &           CNAD,LLNAD,SNAD,
     &           GNAD,HIND,RSNAD,DNAD,SENN0C,SENNSC,
     &           RANC*100.0, LANC*100.0, SANC*100.0,
     &           GRAINANC*100.0,SDNC*100.0, VANC*100.0,
     &           LCNF, SCNF, RCNF,
     &           VCNC*100.0, VMNC*100.0, AMIN1(2.0,NUPR),ANDEM

!     VSH
      IF (FMOPT == 'C') then  
         CALL CsvOutPlNCsCer(EXCODE, RUN, TN, 
     &RN, REP, YEAR, DOY, DAS, DAP, 
     &TMEAN,ZSTAGE,NUAD,TNAD,SDNAD,RNAD,CNAD,LLNAD,SNAD, GNAD, 
     &HIND,RSNAD,DNAD,SENN0C,SENNSC,RANC, LANC, SANC, GRAINANC, 
     &SDNC, VANC,LCNF, SCNF, RCNF,VCNC, VMNC, NUPR,ANDEM,  
     &vCsvlinePlNCsCer, vpCsvlinePlNCsCer, vlngthPlNCsCer)
     
         CALL LinklstPlNCsCer(vCsvlinePlNCsCer)
      END IF 

              ENDIF  ! ISWNIT = Y

!!             2021-02-14 chp 
!!             Keep cumulative value for use tomorrow.
!              NUAD_Y = NUAD

            ENDIF    ! IDETG.NE.'N'
            ENDIF    ! IDETG.NE.'N'.OR.IDETL.EQ.'0'
          ENDIF      ! MOD(FROPADJ)

          ! Harvest date or failure writes
          IF (STGDOY(11).EQ.YEARDOY .OR.
     &     DYNAMIC.EQ.SEASEND .AND. SEASENDOUT.NE.'Y') THEN
     
            IF (DYNAMIC.EQ.SEASEND) THEN
              WRITE (fnumwrk,*)' '
              WRITE (fnumwrk,'(A46,A25)')
     &         ' RUN TERMINATED PREMATURELY (PROBABLY BECAUSE ',
     &         'OF MISSING WEATHER DATA) '
            ENDIF
            
            WRITE(fnumwrk,*)' '
            WRITE(fnumwrk,'(A17,I2)')' CROP COMPONENT: ',CN
            WRITE(fnumwrk,'(A32,F8.1)')
     &       '  DEAD MATERIAL LEFT ON SURFACE  ',SENWAL(0)
            WRITE(fnumwrk,'(A32,F8.1)')
     &       '  DEAD MATERIAL LEFT IN SOIL     ',SENWAS
            WRITE(fnumwrk,'(A32,F8.1)')
     &       '  ROOT WEIGHT AT HARVEST         ',RWAD
            WRITE (fnumwrk,*) ' '
            WRITE (fnumwrk,'(A20,A10,I3)')
     &       ' ROOTS BY LAYER FOR ',excode,tn
            WRITE (fnumwrk,'(A19)')
     &       '  LAYER  RTWT   RLV'
            DO L=1,NLAYR
              WRITE (fnumwrk,'(I6,F7.1,F6.2)')
     &        L,RTWTAL(L),RLV(L)
            ENDDO
            IF (RTSLXDATE.GT.0) THEN
              WRITE(fnumwrk,'(A30,I7)')
     &         '  FINAL SOIL LAYER REACHED ON ',RTSLXDATE
              WRITE(fnumwrk,'(A23,I7,A1)')
     &         '  (MATURITY/FAILURE ON ',YEARDOY,')'
            ELSE  
              WRITE(fnumwrk,*)' FINAL SOIL LAYER NOT REACHED '
            ENDIF
            WRITE (fnumwrk,*) ' '
            WRITE (fnumwrk,'(A15,A10,I3)')' N BALANCE FOR ',excode,tn
            WRITE (fnumwrk,'(A25,F8.4)')'   N UPTAKE + SEED       ',
     &       NUAD+SDNAP
            WRITE (fnumwrk,'(A25,3F8.4)')'   TOTAL N SENESCED      ',
     &       SENNAL(0)+SENNAS,SENNAL(0),SENNAS
            WRITE (fnumwrk,'(A25,F8.4)')'   N IN DEAD MATTER      ',
     &       DNAD
            WRITE (fnumwrk,'(A25,F8.4)')'   TOTAL N IN PLANT      ',
     &       TNAD
            WRITE (fnumwrk,'(A25,F8.4)')'   BALANCE (A-(B+C+D))   ',
     &       NUAD+SDNAP
     &       - (SENNAL(0)+SENNAS)
     &       - TNAD
            IF (TNAD.GT.0.0 .AND.
     &       ABS(NUAD+SDNAP-(SENNAL(0)+SENNAS)-TNAD)/TNAD.GT.0.01)
     &       WRITE(fnumwrk,'(A26,A10,A1,I2)')
     &       '   PROBLEM WITH N BALANCE ',EXCODE,' ',TN
            WRITE (fnumwrk,*) ' '
            WRITE (fnumwrk,'(A18,A10,I3)')' CH2O BALANCE FOR ',excode,tn
            WRITE (fnumwrk,'(A27, F11.4)')'   SEED + CH2O FIXED A     ',
     &       SDRATE+CARBOAC
            WRITE (fnumwrk,'(A27, F11.4)')'   CH2O RESPIRED B         ',
     &       RESPAC
     
            WRITE (fnumwrk,'(A27,3F11.4)')'   CH2O SENESCED C  Tops,rt',
     &       SENWAL(0)+SENWAS,SENWAL(0),SENWAS                          
            WRITE (fnumwrk,'(A27,F11.4)') '   CH2O LF RESERVES LOST C2',
     &       SENRSC*10.0*PLTPOP
            WRITE (fnumwrk,'(A27, F11.4)')'   CH2O IN LIVE+DEAD D     ',
     &       TWAD
            WRITE (fnumwrk,'(A27, F11.4)')'   CH2O IN DEAD MATTER     ',
     &       DWAD
            WRITE (fnumwrk,'(A27, F11.4)')'   CH2O IN LIVE PLANT      ',
     &       TWAD-DWAD
            WRITE (fnumwrk,'(A27, F11.4)')'   POST MATURITY RESERVES E',
     &       RSWADPM
            WRITE (fnumwrk,'(A27, F11.4)')'   BALANCE (A-(B+C+C2+D+E))',
     &         SDRATE+CARBOAC-RESPAC-(SENWAL(0)+SENWAS)
     &       - TWAD-RSWADPM-(SENRSC*10.0*PLTPOP)
            IF (TWAD.GT.0.0 .AND.
     &       ABS(SDRATE+CARBOAC-RESPAC-(SENWAL(0)+SENWAS)
     &       - TWAD-RSWADPM-(SENRSC*10.0*PLTPOP)     )
     &       /TWAD .GT. 0.01)
     &       WRITE(fnumwrk,'(A29,A10,A1,I2)')
     &       '   PROBLEM WITH CH2O BALANCE ',EXCODE,' ',TN

            WRITE (fnumwrk,*) ' '
            WRITE (fnumwrk,'(A22,A10,I3)')
     &       ' STAGE CONDITIONS FOR ',excode,tn
            WRITE (fnumwrk,'(A38,F6.1)')
     &       '  Temperature mean,germ+emergence      ',GETMEAN
            WRITE (fnumwrk,'(A38,F6.1)')
     &       '  Temperature mean,first 20 days       ',TMEAN20P
            WRITE (fnumwrk,'(A38,F6.1)')
     &       '  Temperature mean,20d around anthesis ',TMEAN20A
            WRITE (fnumwrk,'(A38,F6.1)')
     &       '  Solar radn. mean,20d around anthesis ',SRAD20A
            WRITE (fnumwrk,'(A38,F6.1)')
     &       '  Stress fac. mean,20d around anthesis ',STRESS20A
            WRITE (fnumwrk,'(A38,F6.1)')
     &       '  Temperature mean,grain filling       ',GFTMEAN
            WRITE (fnumwrk,'(A38,F6.1)')
     &       '  Temperature mean,grain maturing      ',GMTMEAN
            WRITE (fnumwrk,*) ' '
            WRITE (fnumwrk,'(A17,A10,I3)')' STAGE DATES FOR ',excode,tn
            WRITE (fnumwrk,'(A26)')
     &       '  STAGE   DATE  STAGE NAME'
            DO I = 1, 11
              WRITE (fnumwrk,'(I7,I8,A1,A10)')
     &               I,STGDOY(I),' ',STNAME(I)
            ENDDO
            WRITE (fnumwrk,*) ' '
            WRITE (fnumwrk,'(A27,A10,I3)')
     &       ' LEAF NUMBER AND SIZES FOR ',excode,tn
            WRITE (fnumwrk,'(A15,F4.1)') '   LEAF NUMBER ',LNUMSD
            WRITE (fnumwrk,'(A55)')
     &       '   LEAF AREAP AREA1 AREAT AREAS TNUML  WFLF  NFLF  AFLF'
            IF (LNUMSG.GT.0) THEN
              DO I = 1, LNUMSG
                WRITE (fnumwrk,'(I7,8F6.1)')
     &           I,LAPOT(I),LATL(1,I),LAP(I),LAPS(I),TNUML(I),
     &            1.0-WFLF(I),1.0-NFLF(I),1.0-AFLF(I)
              ENDDO
            ELSE
              WRITE (fnumwrk,*) ' Leaf number < 1!'
            ENDIF
            WRITE (fnumwrk,*) ' '
            WRITE (fnumwrk,'(A28,A10,I3)')
     &       ' STRESS FACTOR AVERAGES FOR ',excode,tn
            WRITE (fnumwrk,'(A55)')
     &       '  PHASE  H2O(PS)   H2O(GR)   N(PS)     N(GR)  PHASE END'
            DO tvi1=1,5
              WRITE (fnumwrk,'(I6,F8.2,3F10.2,2X,A10)')
     &        tvi1,1.0-wfpav(tvi1),1.0-wfgav(tvi1),
     &        1.0-nfpav(tvi1),1.0-nfgav(tvi1),stname(tvi1)
            ENDDO
            WRITE (fnumwrk,'(A42)')
     &       '  NB 0.0 = minimum ; 1.0 = maximum stress.'
            WRITE (fnumwrk,*) ' '
            WRITE (fnumwrk,'(A22,A10,I3)')
     &       ' RESERVES STATUS FOR ',excode,tn
            WRITE (fnumwrk,'(A20,I6)')'  Kg/ha at anthesis ',NINT(RSWAA)
            WRITE (fnumwrk,'(A20,I6)')'  Kg/ha at maturity ',NINT(RSWAD)
            IF (cwaa.GT.0) WRITE (fnumwrk,'(A20,F6.1)')
     &       '  % at anthesis     ',rsca*100.0
            IF (lfwt+stwt+rswt.GT.0) WRITE (fnumwrk,'(A20,F6.1)')
     &       '  % at maturity     ',rswt/(lfwt+stwt+rswt)*100.0
            WRITE (fnumwrk,*) ' '
            WRITE (fnumwrk,'(A20,F6.2)')'  Reserves coeff    ',RSPCS
            WRITE (fnumwrk,'(A20,F6.2)')'  Stem gr end stage ',P4SGE
            WRITE (fnumwrk,'(A20,F6.2)')
     &       '  Anthesis stage    ',(4.0+PD4(1)/PD(4))
            WRITE (fnumwrk,*) ' '
            IF (grnum.GT.0.0) WRITE (fnumwrk,'(A20,F6.1)')
     &       '  Grain weight mg   ',GRWT/GRNUM*1000.0
            WRITE (fnumwrk,'(A20,F6.1)')
     &       '  Grain weight coeff',g2kwt
! FO/GH - 11-13-2021 - Removed division by zero issue for GRNUM
            IF (GRNUM.GT.0.0) THEN
              WRITE (fnumwrk,'(A34)')
     &         '  Some limitation on grain growth!'
              WRITE(fnumwrk,'(A22,I4)')'   Days of Ch2o limit ',ch2olim
              WRITE(fnumwrk,'(A22,I4)')'   Days of N limit    ',nlimit
              WRITE(fnumwrk,'(A22,I4)')'   Days of temp limit ',tlimit
            ENDIF
            IF (grwt.GT.0.0) WRITE (fnumwrk,'(A20,F6.1)')
     &       '  Grain N %         ',grainn/grwt*100.0
            WRITE (fnumwrk,'(A20,F6.1)')
     &       '  Minimum grain N % ',grnmn
            WRITE (fnumwrk,'(A20,F6.1)')
     &       '  Standard grain N %',grns
     
            ! BEGIN MAIN OUTPUTS 
            
            CR = CROP
            GWAM = GWAD
            PWAM = GWAD + CHWAD
            GWUM = GWUD
            NUAM = NUAD
            HNUMAM = GRNUMAD
            HIAM = HIAD
            LNUMSM = LNUMSD
            TNUMAM = TNUMAD
            SENWATC = SENWAL(0) + SENWAS
            CWAM = CWAD
            VWAM = VWAD
            CNAM = CNAD
            VNAM = VNAD
            SENNATC = SENNAL(0) + SENNAS
            GNAM = GNAD
            RWAM = RWAD
            RNAM = RNAD
            RSWAM = RSWAD
            HINM = HIND
            GNPCM = GRAINANC *100.0
            VNPCM = VANC * 100.0 
            VNAA = CNAA
            
            IF (tnumam.GT.0.0) hnumgm = hnumam/tnumam
            gwgm = gwum*1000.0    ! mg

            ! Check that -99 not multiplied or divided 
            IF (hnumgm.LT.0.0) hnumgm = -99
            IF (hnumam.LT.0.0) hnumam = -99
            IF (hnumgmm.LT.0.0) hnumgmm = -99
            IF (hnumamm.LT.0.0) hnumamm = -99

            ! Put N variables to -99 if N switched off
            IF (ISWNIT.EQ.'N') THEN
              gnpcm = -99
              vnpcm = -99
              cnam = -99
              gnam = -99
              hinm = -99
              sdnap = -99
              rnam = -99
              nuam = -99
            ENDIF
            
            WRITE (fnumwrk,*) ' '

            IF (DYNAMIC.EQ.SEASEND) THEN
            
              WRITE(fnumwrk,*)  'WRITING END OF RUN OUTPUTS     '

              ! Simulated outputs only
              !  IDETG (GROUT in controls (Y,N))  Plant growth outputs
              !   Y->Work_details+Plantgro+Plantgr2+Plantgrf
              !      +PlantN(If N switched on)
              !   FROUT->#=number of days between outputs
              !  IDETS (SUMRY in controls (Y,N)) Summary outputs
              !   Y->Summary+Plantsum+Work(Harvest)                        
              !
              ! Simulated+Measured outputs
              !  IDETO (OVVEW in controls (Y,E,N)) Overview outputs
              !   Y->Overview+Evaluate(+Measured if IDETG=Y)
              !   E->Evaluate only
              !  IDETL (VBOSE in controls (0,N,Y,D,A))
              !   Y->Leaves+Phases+Measured                 
              !   D->+Phenols+Phenolm+Plantres+Plantrem
              !   A->Errora+Errors+Errort+Full Reads
              !   0,A are meta switches:
              !    0 switches everything to N apart from IDETS,which->Y,      
              !      and IDETO,which->E when RNMODE is not N (seasonal)
              !    A switches ALL outputs on  
              
              laix = -99.0
              gwam = -99.0
              gwum = -99.0
              hnumam = -99.0
              hnumgm = -99.0
              hiam = -99.0
              lnumsm = -99.0
              tnumam = -99.0
              cwam = -99.0
              vwam = -99.0
              rwam = -99.0
              carboac = -99.0
              senwatc = -99.0
              rswam = -99.0
              sennatc = -99.0
              cnam = -99.0
              rnam = -99.0
              vnam = -99.0
              gnam = -99.0
              hinm = -99.0
              gnpcm = -99.0
              cwaa = -99.0
              vnaa = -99.0
              lnpca = -99.0
              stress20a = -99.0
              mday = -99
              hayear = -99
              haday = -99
            ELSE 
              WRITE(fnumwrk,*)  'WRITING HARVEST DAY OUTPUTS         '
            ENDIF  
            
            IF (STEP.NE.1) THEN
              WRITE (fnumwrk,*) ' '
              WRITE (fnumwrk,*) ' Step number greater than 1!'
              WRITE (fnumwrk,*) ' Not set up for hourly runs!'
              WRITE (fnumwrk,*) ' Will skip final outputs.'
              GO TO 8888
            ENDIF
            WRITE(fnumwrk,*)
     &       ' Harvest percentage (Technology coeff) ',hpc
            
            CNCHAR = ' '
            CNCHAR2 = '  '
            IF (CN.EQ.1) THEN
              OUT = 'OUT'
              CNCHAR2= '1 '
            ELSE
              CNCHAR = TL10FROMI(CN)
              OUT = 'OU'//CNCHAR(1:1)
              CNCHAR2(1:1) = CNCHAR(1:1)
            ENDIF
            
!            CALL CSYR_DOY (STGDOY(7),PLYEAR,PLDAY)
            CALL YR_DOY (STGDOY(7),PLYEAR,PLDAY)
            IF (ADAT.GT.0) THEN
!              CALL CSYR_DOY (ADAT,AYEAR,ADAY)
              CALL YR_DOY (ADAT,AYEAR,ADAY)
            ELSE
              AYEAR = -99
              ADAY = -99
            ENDIF
            IF (STGDOY(5).GT.0) THEN
!              CALL CSYR_DOY (STGDOY(5),MYEAR,MDAY)
              CALL YR_DOY (STGDOY(5),MYEAR,MDAY)
            ELSE
              MYEAR = -99
              MDAY = -99
            ENDIF
!            CALL CSYR_DOY (STGDOY(11),HAYEAR,HADAY)
            CALL YR_DOY (STGDOY(11),HAYEAR,HADAY)
            
!-----------------------------------------------------------------------
            
            ! SCREEN OUTPUTS (CROPSIM)
            
            IF (FILEIOT(1:3).EQ.'XFL'.AND.CN.EQ.1.AND.RNMODE.NE.'T')THEN
            
              IF (OUTCOUNT.LE.0 .OR. OUTCOUNT.EQ.25) THEN
                WRITE (*,499)
  499           FORMAT ('   RUN EXCODE      TN RN',
     X           ' TNAME..................',
     X           '.. REP  RUNI S O C CR  GWAM')
              ENDIF
              IF (OUTCOUNT .EQ. 25) THEN
                OUTCOUNT = 1
              ELSE
                OUTCOUNT = OUTCOUNT + 1
              ENDIF
              WRITE (*,410) run,excode,tn,rn,tname(1:25),
     X        rep,runi,sn,on,cn,cr,NINT(gwam)
  410         FORMAT (I6,1X,A10,I4,I3,1X,A25,
     X        I4,I6,I2,I2,I2,1X,A2,I6)
            ENDIF
            
            ! END OF SCREEN OUTPUTS
            
!-----------------------------------------------------------------------
            
            ! PLANT SUMMARY
           
            IF ((IDETS.NE.'N'.AND.IDETL.NE.'0').OR.IDETL.EQ.'A') THEN

              WRITE (fnumwrk,*) ' '                       
              WRITE (fnumwrk,*) 'Writing PLANT SUMMARY'
              
              FNAMETMP = ' '
              FNAMETMP(1:12) = 'Plantsum.'//out
              
              IF (RUN.EQ.1 .AND. RUNI.EQ.1) THEN
               OPEN (UNIT = FNUMTMP,FILE = FNAMETMP,STATUS = 'UNKNOWN')
               WRITE (FNUMTMP,'(A14)') '$PLANT SUMMARY'
               WRITE (FNUMTMP,*) ' '
               WRITE (fnumtmp,96)
               WRITE (fnumtmp,99)
   96          FORMAT ('*PLANTSUM',/)
   99          FORMAT ('@  RUN EXCODE    TRNO RN ',                     
     X         'TNAME..................',
     X         '.. REP  RUNI S O C    CR',
     X         ' PYEAR  PDOY  TSAP  ADAP  MDAP   FLN FLDAP',
     x         ' HYEAR  HDAY SDWAP CWAHC  CWAM',
     X         ' PARUE',
     X         '  HWAM  HWAH  BWAH  HWUM  H#AM  H#UM',
     x         ' SDNAP  CNAM  HNAM  RNAM  TNAM  NUAM  HN%M  VN%M',
     E         ' D1INI D2INI D3INI ')
               CLOSE(fnumtmp)
              ENDIF
              
              OPEN (UNIT=fnumtmp,FILE=FNAMETMP,POSITION='APPEND')

              IF (tsdat.GT.0) THEN 
                tsdap = Dapcalc(tsdat,plyear,plday)
              ELSE
                tsdap = -99
              ENDIF  
              IF (lldat.GT.0) THEN 
                lldap = Dapcalc(lldat,plyear,plday)
              ELSE
                lldap = -99
              ENDIF  
              IF (adat.LT.9999999) THEN 
                adap = Dapcalc(adat,plyear,plday)
              ELSE
                adap = -99
              ENDIF  
              IF (stgdoy(5).LT.9999999) THEN 
                 mdap = Dapcalc(stgdoy(5),plyear,plday)
              ELSE 
                mdap = -99
                stgdoy(5) = -99
              ENDIF     
              
              WRITE (fnumtmp,400) run,excode,tn,rn,tname(1:25),
     X          rep,runi,sn,on,cn,cr,
     X          plyear,plday,tsdap,adap,mdap,
     x          flnmodel,lldap,hayear,haday,
     X          NINT(sdrate),
     X          0,NINT(cwam),pariuem,NINT(gwam),
     X          NINT(gwam*hpc/100.0),NINT(cwam-gwam),
     X          gwgm/1000.0,NINT(hnumam),NINT(hnumgm),
     X          sdnap,NINT(cnam),NINT(gnam),NINT(rnam),
     X          NINT(AMAX1(-99.0,cnam+rnam)),NINT(nuad),
     X          gnpcm,vnpcm,'   -99','   -99','   -99'
  400         FORMAT (I6,1X,A10,I4,I3,1X,A25,
     X         I4,I6,I2,I2,I2,4X,A2,
     X         I6,I6,I6,I6,I6,F6.1,I6,I6,I6,I6,
     X         I6,
     X         I6,F6.1,I6,
     X         I6,I6,
     X         1X,F5.3,I6,I6,
     X         F6.1,I6,I6,I6,
     X         I6,I6,
     X         F6.2,F6.2,
     X         3A6)
              
              CLOSE(fnumtmp)       
             
            ELSE
            
               OPEN (UNIT=FNUMTMP,FILE='Plantsum.out',STATUS='UNKNOWN')
               CLOSE (UNIT=FNUMTMP, STATUS = 'DELETE')
               
            ENDIF  ! END OF PLANT SUMMARY
            
!-----------------------------------------------------------------------            
            
            ! LEAVES
           
            IF (IDETL.EQ.'Y'.OR.IDETL.EQ.'A') THEN

              IF (CN.EQ.1) THEN
                IF (FNUMLVS.LE.0.OR.FNUMLVS.GT.1000) THEN
                  CALL Getlun ('Leaves.OUT',fnumlvs)
                ENDIF
                IF (RUN.EQ.1 .AND. RUNI.EQ.1) THEN
                 OPEN(UNIT=FNUMLVS,FILE='Leaves.OUT',STATUS='UNKNOWN')
                 WRITE (FNUMLVS,'(A11)') '$LEAF SIZES'
                 CLOSE(FNUMLVS)
                ENDIF
                OPEN(UNIT=FNUMLVS,FILE='Leaves.OUT',POSITION='APPEND')
                WRITE (FNUMLVS,'(/,A77,/)') OUTHED
                WRITE (FNUMLVS,'(A14,F6.1)') '! LEAF NUMBER ',LNUMSD
                IF (LNSWITCH.GT.0.0) THEN
                  WRITE(FNUMLVS,'(A42,F6.1)')
     &             '! LEAF NUMBER WHEN INCREASE FACTOR CHANGED',lnswitch
                  WRITE(FNUMLVS,'(A35,F6.2)')
     &             '! AREA OF LEAF WHEN FACTOR CHANGED  ',laswitch
                ENDIF     
                WRITE (FNUMLVS,'(/,A42,A30)')
     &          '@ LNUM AREAP AREA1 AREAT AREAS  T#PL  T#AL',
     &          '  WFLF  NFLF  AFLF  TFLF DAYSG'
                IF (LNUMSG.GT.0) THEN
                  DO I = 1, LNUMSG
                    WRITE (fnumlvs,'(I6,5F6.1,I6,5F6.1)')
     &               I,LAPOT(I),LATL(1,I),LAP(I),LAPS(I),
     &               TNUML(I),NINT(TNUML(I)*PLTPOP),
     &               1.0-WFLF(I),1.0-NFLF(I),1.0-AFLF(I),1.0-TFLF(I),
     &               WFLFNUM(I)
                  ENDDO
                ENDIF
                IF (run.EQ.1.AND.runi.EQ.1) THEN
                  WRITE(fnumlvs,*)' '
                  WRITE(fnumlvs,'(A37)')
     &             '! LNUM  = Number of leaf on main axis'
                  WRITE(fnumlvs,'(A38)')
     &             '! AREAP = Potential area of leaf (cm2)'
                  WRITE(fnumlvs,'(A41)')
     &             '! AREA1 = Area of leaf on main axis (cm2)'
                  WRITE(fnumlvs,'(A44,A16)')
     &             '! AREAT = Area of leaves on all axes at leaf',
     &             ' position (cm2) '
                  WRITE(fnumlvs,'(A50,A6)')
     &             '! AREAS = Area of leaves senesced at leaf position',
     &             ' (cm2)'
                  WRITE(fnumlvs,'(A46)')
     &             '! T#PL  = Tiller number/plant at leaf position'
                  WRITE(fnumlvs,'(A49)')
     &             '! T#AL  = Tiller number/area(m2) at leaf position'
                  WRITE(fnumlvs,'(A38,A17)')
     &             '! WFLF  = Water stress factor for leaf',
     &             ' (0-1,0=0 stress)'
                  WRITE(fnumlvs,'(A51)')
     &             '! NFLF  = N stress factor for leaf (0-1,0=0 stress)'
                  WRITE(fnumlvs,'(A36,A24)')
     &             '! AFLF  = Assimilate factor for leaf',
     &             ' (0-1,0=0 no limitation)'
                  WRITE(fnumlvs,'(A37,A24)')
     &              '! TFLF  = Temperature factor for leaf',
     &              ' (0-1,1=0 no limitation)'
                  WRITE(fnumlvs,'(A37)')
     &              '! DAYSG = Number of days of growth   '
                ENDIF
                CLOSE (FNUMLVS)
              ENDIF

            ELSE
            
               OPEN (UNIT=FNUMTMP,FILE='Leaves.OUT',STATUS='UNKNOWN')
               CLOSE (UNIT=FNUMTMP, STATUS = 'DELETE')
               
            ENDIF  ! END OF LEAVES
            
!-----------------------------------------------------------------------
            
            ! A-FILE READS
            
            adatm = -99
            adatt = -99
            adayh = -99
            carboacm = -99
            cnaam = -99
            cnamm = -99
            cwadt = -99
            cwamm = -99
            drdatm = -99
            edatm = -99
            gnamm = -99
            gnpcmm = -99
            gstdm = -99
            gwadt = -99
            hiadt = -99
            hiamm = -99
            hinmm = -99
            hnumamm = -99
            hnumat = -99
            hnumet = -99
            hnumgmm = -99
            hwadm = -99
            hwahm = -99
            gwamm = -99
            gwut = -99
            gwumm = -99
            jdatm = -99
            laixm = -99
            lnaam = -99
            lnpcam = -99
            lnumsmm = -99
            lnumt = -99
            lwaam = -99
            mdatm = -99
            mdatt = -99
            nuamm = -99
            rnamm = -99
            rswamm = -99
            rwamm = -99
            sennatcm = -99
            senwatcm = -99
            tnumamm = -99
            tnumt = -99
            tsdatm = -99
            vnamm = -99
            vnpcmm = -99
            vwamm = -99                       
            cwaam = -99
            
            CALL LTRIM2 (FILEIO,filenew)
            FILELEN = TVILENT(FILENEW)
            ! Jed Greene, Citadel - keep index from neg
            FILELEN = MAX(FILELEN-12, 0) 
            
            IF (TVILENT(FILEADIR).GT.3) THEN
              IF (FILEADIR(TVILENT(FILEADIR):
     &            TVILENT(FILEADIR)).NE.SLASH)THEN
                FILEA = FILEADIR(1:TVILENT(FILEADIR))//
     &           SLASH //EXCODE(1:8)//'.'//EXCODE(9:10)//'A'
              ELSE
                FILEA = FILEADIR(1:TVILENT(FILEADIR))//
     &           EXCODE(1:8)//'.'//EXCODE(9:10)//'A'
              ENDIF
            ELSE
              FILEA = FILENEW(1:FILELEN-12)//EXCODE(1:8)//'.'//
     &         EXCODE(9:10)//'A'
            ENDIF       
            FEXISTA = .FALSE.
            INQUIRE (FILE = FILEA,EXIST = FEXISTA)
            IF (.not.FEXISTA) THEN
              WRITE (fnumwrk,*) 'A-file not found!'
            ELSE
              WRITE (fnumwrk,*) 'A-file found: ',filea(1:60)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'GWAM',gwamm)
              IF (gwamm.LE.0.0)
     &         CALL AREADR (FILEA,TN,RN,SN,ON,CN,'HWAM',gwamm)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'HWAH',hwahm)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'HWUM',gwumm)
              IF (gwumm.le.0.0)
     &         CALL AREADR (FILEA,TN,RN,SN,ON,CN,'GWUM',gwumm)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'LAIX',laixm)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'CWAM',cwamm)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'BWAH',vwamm)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'CWAA',cwaam)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'T#AM',tnumamm)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'H#AM',hnumamm)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'H#UM',hnumgmm)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'L#SM',lnumsmm)
            
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'CNAM',cnamm)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'VNAM',vnamm)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'CNAA',cnaam)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'GNAM',gnamm)
              IF (gnamm.LE.0.0) 
     &         CALL AREADR (FILEA,TN,RN,SN,ON,CN,'HNAM',gnamm)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'LN%A',lnpcam)
              IF (lnpcam.LE.0.0)
     &         CALL AREADR (FILEA,TN,RN,SN,ON,CN,'LNCA',lnpcam)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'GN%M',gnpcmm)
              IF (gnpcmm.LE.0.0)
     &         CALL AREADR (FILEA,TN,RN,SN,ON,CN,'GNCM',gnpcmm)
              IF (gnpcmm.LE.0.0)
     &         CALL AREADR (FILEA,TN,RN,SN,ON,CN,'HN%M',gnpcmm)
              IF (gnpcmm.LE.0.0)
     &         CALL AREADR (FILEA,TN,RN,SN,ON,CN,'HNCM',gnpcmm)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'VN%M',vnpcmm)
              IF (vnpcmm.le.0.0)
     &         CALL AREADR (FILEA,TN,RN,SN,ON,CN,'VN%D',vnpcmm)
              IF (vnpcmm.le.0.0)
     &         CALL AREADR (FILEA,TN,RN,SN,ON,CN,'VNCD',vnpcmm)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'L#SM',lnumsmm)
              IF (lnumsmm.le.0.0)
     &         CALL AREADR (FILEA,TN,RN,SN,ON,CN,'LNOSM',lnumsmm)
            
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'HIAM',hiamm)
              IF (HIAMM.GE.1.0) HIAMM = HIAMM/100.0
            
              CALL AREADI (FILEA,TN,RN,SN,ON,CN,'EDAT',edatm)
              CALL AREADI (FILEA,TN,RN,SN,ON,CN,'DRDAT',drdatm)
              CALL AREADI (FILEA,TN,RN,SN,ON,CN,'TSDAT',tsdatm)
              CALL AREADI (FILEA,TN,RN,SN,ON,CN,'A1DAT',a1datm)
              CALL AREADI (FILEA,TN,RN,SN,ON,CN,'LLDAT',lldatm)
              CALL AREADI (FILEA,TN,RN,SN,ON,CN,'SPDAT',spdatm)
              CALL AREADI (FILEA,TN,RN,SN,ON,CN,'ADAT',adatm)
              CALL AREADI (FILEA,TN,RN,SN,ON,CN,'JDAT',jdatm)
              IF (ADATM.LE.0) THEN
                CALL AREADI (FILEA,TN,RN,SN,ON,CN,'GS059',adatm)
                IF (ADATM.GT.0) THEN
                  ADATM = ADATM + 2
                  WRITE (fnumwrk,*) 'WARNING  ADAT = GS059 + 2'
                ENDIF
              ENDIF
              IF (ADATM.LE.0) THEN
                CALL AREADI (FILEA,TN,RN,SN,ON,CN,'ADAY',adaym)
                CALL AREADI (FILEA,TN,RN,SN,ON,CN,'YEAR',ayearm)
                ADATM = CSYDOY(AYEARM,ADAYM)
              ENDIF
              CALL AREADI (FILEA,TN,RN,SN,ON,CN,'MDAT',mdatm)
              IF (MDATM.LE.0) THEN
                CALL AREADI (FILEA,TN,RN,SN,ON,CN,'MDAY',mdaym)
                CALL AREADI (FILEA,TN,RN,SN,ON,CN,'YEAR',myearm)
                MDATM = CSYDOY(MYEARM,MDAYM)
              ENDIF
            ENDIF
            
            ! If nothing in A-file,use X-file
            IF (EDATM.LE.0) edatm = emdatm   
            
            ! END OF A-FILE READS
            
!-----------------------------------------------------------------------
            
            ! CHECK DATA AND USE EQUIVALENTS IF NECESSARY
            
            ! Product wt at maturity
            IF (hwahm.GT.0 .AND. gwamm.LE.0) gwamm = hwahm/(hpc/100.0)
            
            ! Product wt at harvest
            IF (gwamm.GT.0 .AND. hwahm.LE.0) hwahm = gwamm*(hpc/100.0)
            
            ! Canopy wt at maturity
            IF (vwamm.GT.0 .AND. gwamm.GT.0) cwamm = vwamm+gwamm
            
            ! Vegetative wt at maturity
            IF (gwamm.GT.0 .AND. cwamm.GT.0) vwamm = cwamm-gwamm
            
            ! Harvest index at maturity
            IF (hiamm.LE.0.0) THEN
              IF (cwamm.GT.0 .AND. gwamm.GT.0) hiamm = gwamm/cwamm
            ELSE
              IF (cwamm.GT.0 .AND. gwamm.GT.0) THEN
                hiammtmp = gwamm/cwamm
                IF (hiammtmp/hiam.GT.1.1 .OR. hiammtmp/hiam.LT.0.9) THEN
                  IF (ABS(hiammtmp-hiamm)/hiamm.GT.0.05) THEN
                    WRITE (fnumwrk,*) 'Reported HI not consistent',
     &               ' with yield and total weight data!!'
                    WRITE (fnumwrk,*) ' Reported HI   ',hiamm
                    WRITE (fnumwrk,*) ' Calculated HI ',hiammtmp
                    WRITE (fnumwrk,*) ' Will use reported value '
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
            
            ! Product unit wt at maturity
            IF (gwumm.GT.1.0) gwumm = gwumm/1000.0 ! mg->g
            IF (gwumm.LE.0 .AND. hnumamm.GT.0) THEN
              IF (gwamm.GT.0.0) gwumm=gwamm*0.1/hnumamm  ! kg->g
            ELSE
              IF (gwamm.gt.0.0.AND.hnumamm.GT.0.0) THEN
                gwumyld = gwamm*0.1/hnumamm
                IF (ABS(gwumyld-gwumm)/gwumm.GT.0.05) THEN
                  WRITE (fnumwrk,*) 'Reported kernel wt.not consistent',
     &            ' with yield and kernel # data!!'
                  WRITE (fnumwrk,*) ' Reported wt   ',gwumm
                  WRITE (fnumwrk,*) ' Calculated wt ',gwumyld
                  WRITE (fnumwrk,*) '   Yield       ',gwamm
                  WRITE (fnumwrk,*) '   Kernel #    ',hnumamm
                  WRITE (fnumwrk,*) ' Will use reported value '
                ENDIF
              ENDIF
            ENDIF
            gwgmm = gwumm*1000.0  ! mg
            
            ! Product number at maturity
            IF (HNUMAMM.LE..0.AND.HNUMGMM.GT..0.AND.TNUMAMM.GT..0) THEN
             HNUMAMM = HNUMGMM * TNUMAMM
             WRITE(fnumwrk,*)'Tiller # * grains/tiller used for HNUMAMM'
            ENDIF
            IF (hnumgmm.LE.0. AND. tnumamm.GT.0 .AND. hnumamm.GT.0) THEN
              hnumgmm = hnumamm/tnumamm
              WRITE(fnumwrk,*)'Grains/area / tiller # used for HNUMGMM'
            ENDIF
            
            ! Tiller number at maturity
            IF (tnumamm.LE.0 .AND. hnumamm.GT.0. AND. hnumgmm.GT.0)
     &         tnumamm = hnumamm/hnumgmm
            
            ! Canopy N at maturity
            IF (vnamm.GT.0 .AND. gnamm.GT.0 .AND. cnamm.LE.0)
     &        cnamm = vnamm + gnamm
            
            ! Vegetative N at maturity
            IF (vnamm.LE.0) THEN
             IF (gnamm.GE.0 .AND. cnamm.GT.0) vnamm=cnamm-gnamm
            ENDIF
            
            ! Product N harvest index at maturity
            IF (cnamm.GT.0 .AND. gnamm.GT.0) hinmm=gnamm/cnamm
            
            ! Vegetative N concentration at maturity
            IF (vnpcmm.LE.0) THEN
             IF (vwamm.GT.0 .AND. vnamm.GT.0) vnpcmm = (vnamm/vwamm)*100
            ENDIF
            
            ! Product N concentration at maturity
            IF (gnpcmm.LE.0) THEN
             IF (gwamm.GT.0 .AND. gnamm.GT.0) gnpcmm = (gnamm/gwamm)*100
            ENDIF
            
            ! Leaf N concentration at maturity
            IF (lnpcam.LE.0 .AND. lnaam.GT.0 .AND. lwaam.GT.0.0)
     &        lnpcam = lnaam/lwaam
     
            ! END OF CHECKING DATA

!-----------------------------------------------------------------------

            ! T-FILE READS AND MEASURED.OUT WRITES
            
            IF ((IDETG.NE.'N'.AND.IDETL.NE.'0').OR.
     &           IDETL.EQ.'D'.OR.IDETL.EQ.'A') THEN
            
              ! T-FILE READS AND MEASURED.OUT WRITES
            
              WRITE (fnumwrk,*)
     &         'Trying to read T-file and write MEASURED.OUT'
            
              Fnametmp = ' '
              Fnametmp(1:12) = 'Measured.OUT'
              IF (RUN.EQ.1 .AND. RUNI.EQ.1) THEN
               CALL Getlun ('FILET',FNUMT)
               OPEN (UNIT = FNUMTMP,FILE = FNAMETMP,STATUS = 'UNKNOWN')
               WRITE (FNUMTMP,'(A22)') '$TIME_COURSE(MEASURED)'
               CLOSE(FNUMTMP)
              ENDIF
            
              STARNUMO = STARNUMO + 1  ! # datasets in sim output file
              
              IF (TVILENT(FILEADIR).GT.3) THEN
                IF (FILEADIR(TVILENT(FILEADIR):
     &            TVILENT(FILEADIR)).NE.SLASH)THEN
                  FILET = FILEADIR(1:TVILENT(FILEADIR))//
     &           SLASH //EXCODE(1:8)//'.'//EXCODE(9:10)//'T'
                ELSE
                  FILET = FILEADIR(1:TVILENT(FILEADIR))//
     &           EXCODE(1:8)//'.'//EXCODE(9:10)//'T'
                ENDIF
              ELSE
                FILET = FILENEW(1:FILELEN-12)//EXCODE(1:8)//'.'//
     &          EXCODE(9:10)//'T'
              ENDIF       
              FEXISTT  = .FALSE.
              INQUIRE (FILE = FILET,EXIST = FEXISTT)
            
              CFLTFILE = 'N'
              LAIXT = -99.0
              VALUER = -99.0
            
              IF (.not.FEXISTT) THEN
                WRITE (fnumwrk,*) 'T-file not found: ',filet(1:60)
              ELSE
                WRITE (fnumwrk,*) 'T-file found: ',filet(1:60)
                TLINENUM = 0
                OPEN (UNIT = FNUMT,FILE = FILET)
                OPEN (UNIT = FNUMTMP,FILE = FNAMETMP,POSITION ='APPEND')
                COLNUM = 1
                L1 = 0
                DO
8453              CONTINUE                    
                  READ(FNUMT,'(A180)',END = 5555)LINET
                  IF (LINET(1:1).EQ.'!') GO TO 8453
                  TLINENUM = TLINENUM + 1  ! Only to check if file empty
                  IF (LINET(1:5).EQ.'*FLAG'.OR.LINET(1:5).EQ.'*CODE'.OR.
     &                LINET(1:5).EQ.'*GENE') THEN
                    DO
8454                  CONTINUE                    
                      READ(FNUMT,'(A180)',END = 5555)LINET
                      IF (LINET(1:1).EQ.'!') GO TO 8454
                      IF (LINET(1:1).EQ.'*') THEN
                        IF (LINET(1:7).EQ.'*DATA(T') EXIT
                        IF (LINET(1:7).EQ.'*EXP.DA') EXIT
                        IF (LINET(1:7).EQ.'*EXP. D') EXIT
                        IF (LINET(1:7).EQ.'*AVERAG') EXIT
                        IF (LINET(1:7).EQ.'*TIME_C') EXIT
                      ENDIF
                    ENDDO
                  ENDIF
                  L1 = 0
                  L2 = 0
                  IF (LINET(1:7).EQ.'*DATA(T' .OR.
     &             LINET(1:7).EQ.'*EXP.DA' .OR.
     &             LINET(1:7).EQ.'*EXP. D' .OR.
     &             LINET(1:7).EQ.'*TIME_C' .OR.
     &             LINET(1:7).EQ.'*AVERAG') THEN
                    TNCHAR = TL10FROMI(TN)
                    LENLINE = LEN(TRIM(LINET))
                    IF(LINET(1:7).EQ.'*EXP.DA'.OR.
     &                LINET(1:7).EQ.'*EXP. D')THEN
                      GROUP = 'A'
                      DO L = 1,30
                        IF (LINET(L:L+1).EQ.': ') L1 = L+2
                        IF (LINET(L:L).EQ.':' .AND. 
     &                    LINET(L+1:L+1).NE.' ')
     &                    L1 = L+1
                        IF (L1.GT.0.AND.L.GT.L1+9.AND.
     &                   LINET(L:L).NE.' ') THEN
                          L2 = L  ! Start of group information in t-file
                          EXIT
                        ENDIF
                      ENDDO
                      CALL LTRIM (TNAME)
                      LENTNAME = MIN(15,LEN(TRIM(TNAME)))
                      LENGROUP = MIN(L2+14,LENLINE)
                      IF (LEN(TRIM(TNCHAR)).EQ.1) THEN
                        LINESTAR = '*DATA(T):'//LINET(L1:L1+9)//
     &                  ' '//TNCHAR(1:1)//' C'//
     &                  CNCHAR2//TNAME(1:LENTNAME)//
     &                  ' '//LINET(L2:LENGROUP)
                      ELSEIF (LEN(TRIM(TNCHAR)).EQ.2) THEN              
                        LINESTAR = '*DATA(T):'//LINET(L1:L1+9)//
     &                  ' '//TNCHAR(1:2)//' C'//CNCHAR2//
     &                  TNAME(1:LENTNAME)//
     &                  ' '//LINET(L2:LENGROUP)
                      ELSEIF (LEN(TRIM(TNCHAR)).EQ.3) THEN
                        LINESTAR = '*DATA(T):'//LINET(L1:L1+9)//
     &                  ' '//TNCHAR(1:3)//' C'//CNCHAR2//
     &                  TNAME(1:LENTNAME)//
     &                  ' '//LINET(L2:LENGROUP)
                      ENDIF
                    ENDIF
                    IF (LINET(1:7).EQ.'*DATA(T') THEN
                      GROUP = 'D'
                      DO L = 1,30
                        IF (LINET(L:L).EQ.' ') L1 = L-1
                        IF (L1.NE.0 .AND. LINET(L:L).NE.' ') THEN
                          L2 = L  ! Start of group information in t-file
                          EXIT
                        ENDIF
                      ENDDO
                      CALL LTRIM (TNAME)
                      LENTNAME = MIN(15,LEN(TRIM(TNAME)))
                      LENGROUP = MIN(L2+14,LENLINE)
                      IF (LEN(TRIM(TNCHAR)).EQ.1) THEN
                        LINESTAR = LINET(1:L1)//' TN:'//
     &                  TNCHAR(1:1)//' C'//CNCHAR2//TNAME(1:LENTNAME)//
     &                  ' '//LINET(L2:LENGROUP)
                      ELSEIF (LEN(TRIM(TNCHAR)).EQ.2) THEN
                        LINESTAR = LINET(1:L1)//' TN:'//
     &                  TNCHAR(1:2)//' C'//CNCHAR2//TNAME(1:LENTNAME)//
     &                  ' '//LINET(L2:LENGROUP)
                      ELSEIF (LEN(TRIM(TNCHAR)).EQ.3) THEN
                        LINESTAR = LINET(1:L1)//' TN:'//
     &                  TNCHAR(1:3)//' C'//CNCHAR2//TNAME(1:LENTNAME)//
     &                  ' '//LINET(L2:LENGROUP)
                      ENDIF
                    ENDIF
                    IF (LINET(1:7).EQ.'*AVERAG' .OR.
     &                  LINET(1:7).EQ.'*TIME_C') THEN
                      GROUP = 'A'
                      DO L = 1,30
                        IF (LINET(L:L).EQ.' ') L1 = L-1
                        IF (L1.NE.0 .AND. LINET(L:L).NE.' ') THEN
                          L2 = L  ! Start of group information in t-file
                          EXIT
                        ENDIF
                      ENDDO
                      CALL LTRIM (TNAME)
                      LENTNAME = MIN(15,LEN(TRIM(TNAME)))
                      LENGROUP = MIN(L2+14,LENLINE)
                      IF (LEN(TRIM(TNCHAR)).EQ.1) THEN
                        LINESTAR = LINET(1:L1)//' TN:'//
     &                  TNCHAR(1:1)//' C'//CNCHAR2//TNAME(1:LENTNAME)//
     &                  ' '//LINET(L2:LENGROUP)
                      ELSEIF (LEN(TRIM(TNCHAR)).EQ.2) THEN
                        LINESTAR = LINET(1:L1)//' TN:'//
     &                  TNCHAR(1:2)//' C'//CNCHAR2//TNAME(1:LENTNAME)//
     &                  ' '//LINET(L2:LENGROUP)
                      ELSEIF (LEN(TRIM(TNCHAR)).EQ.3) THEN
                        LINESTAR = LINET(1:L1)//' TN:'//
     &                  TNCHAR(1:3)//' C'//CNCHAR2//TNAME(1:LENTNAME)//
     &                  ' '//LINET(L2:LENGROUP)
                      ENDIF
                    ENDIF
                  ELSEIF (LINET(1:1).EQ.'@') THEN
                    DATECOL = Tvicolnm(linet,'DATE')
                    YEARCOL = Tvicolnm(linet,'YEAR')
                    DOYCOL = Tvicolnm(linet,'DOY')
                    IF (DOYCOL.LE.0) DOYCOL = Tvicolnm(linet,'DAY')
                    LENLINE = LEN(TRIM(LINET))
                    LINET(LENLINE+1:LENLINE+12) = '   DAP   DAS'
                    LINET(1:1) = '@'
                    WRITE (FNUMTMP,*) ' '
                    WRITE (FNUMTMP,'(A80)') LINESTAR(1:80)
                    WRITE (FNUMTMP,*) ' '
                    WRITE (FNUMTMP,'(A180)') LINET(1:180)
                    STARNUMM = STARNUMM + 1       ! Number of datasets  
                    CFLTFILE = 'Y'
                  ELSE
                    IF (LINET(1:1).NE.'!') THEN 
                      LENLINE = LEN(TRIM(LINET))
                      IF (LENLINE.GT.3)THEN
                        CALL Getstri (LINET,COLNUM,VALUEI)
                      ENDIF
                    ENDIF
                    IF (VALUEI.EQ.TN) THEN
                      IF (DATECOL.GT.0.OR.DOYCOL.GT.0) THEN
                        IF (DATECOL.GT.0) THEN
                          CALL Getstri (LINET,DATECOL,DATE)
                        ELSEIF (DATECOL.LE.0) THEN
                          CALL Getstri (LINET,DOYCOL,DOY)
                          CALL Getstri (LINET,YEARCOL,YEAR)
                          IF (YEAR.GT.2000) YEAR = YEAR-2000
                          IF (YEAR.GT.1900) YEAR = YEAR-1900
                          DATE = YEAR*1000+DOY
                        ENDIF
                        DAP = MAX(0,CSTIMDIF(YEARPLT,DATE))
                        DAS = MAX(0,CSTIMDIF(YEARSIM,DATE))
                        DAPCHAR = TL10FROMI(DAP)
                        IF (LEN(TRIM(DAPCHAR)).EQ.1) THEN
                          DAPWRITE = '     '//DAPCHAR(1:1)
                        ELSEIF (LEN(TRIM(DAPCHAR)).EQ.2) THEN
                          DAPWRITE = '    '//DAPCHAR(1:2)
                        ELSEIF (LEN(TRIM(DAPCHAR)).EQ.3) THEN
                          DAPWRITE = '   '//DAPCHAR(1:3)
                        ENDIF
                        LENLINE = LEN(TRIM(LINET))
                        LINET(LENLINE+1:LENLINE+6) = DAPWRITE(1:6)
                        DAPCHAR = TL10FROMI(DAS)
                        IF (LEN(TRIM(DAPCHAR)).EQ.1) THEN
                          DAPWRITE = '     '//DAPCHAR(1:1)
                        ELSEIF (LEN(TRIM(DAPCHAR)).EQ.2) THEN
                          DAPWRITE = '    '//DAPCHAR(1:2)
                        ELSEIF (LEN(TRIM(DAPCHAR)).EQ.3) THEN
                          DAPWRITE = '   '//DAPCHAR(1:3)
                        ENDIF
                        LENLINE = LEN(TRIM(LINET))
                        LINET(LENLINE+1:LENLINE+6) = DAPWRITE(1:6)
                      ENDIF
                      WRITE (FNUMTMP,'(A180)') LINET
                    ENDIF
                  ENDIF
                ENDDO
 5555           CONTINUE
                ! If T-file was empty
                IF (TLINENUM.LT.4) THEN
                  WRITE (fnumwrk,*) 'T-file was empty!'
                ENDIF
              ENDIF
            
              CLOSE(FNUMT)
              CLOSE(FNUMTMP)
              
            ELSE
               OPEN (UNIT=FNUMTMP,FILE='MEASURED.OUT',STATUS='UNKNOWN')
               CLOSE (UNIT=FNUMTMP, STATUS = 'DELETE')
            ENDIF ! END OF T-FILE READS AND MEASURED.OUT WRITES
          
            
!-----------------------------------------------------------------------
            
            ! Check that -99 not multiplied or divided 
            IF (hnumgmm.LT.0.0) hnumgmm = -99
            IF (hnumamm.LT.0.0) hnumamm = -99
          
            ! Create character equivalents for outputing
            IF (gwumm.LE.0) THEN
              gwummc = ' -99.0'
            ELSE
              gwummc = ' '
              WRITE (gwummc,'(F6.3)') gwumm
            ENDIF
            IF (gwum.LE.0) THEN
              gwumc = ' -99.0'
            ELSE
              gwumc = ' '
              WRITE (gwumc,'(F6.3)') gwum
            ENDIF
            IF (hiamm.LE.0) THEN
              hiammc = ' -99.0'
            ELSE
              hiammc = ' '
              WRITE (hiammc,'(F6.3)') hiamm
            ENDIF
            IF (hiam.LE.0) THEN
              hiamc = ' -99.0'
            ELSE
              hiamc = ' '
              WRITE (hiamc,'(F6.3)') hiam
            ENDIF
            IF (hinmm.LE.0) THEN
              hinmmc = ' -99.0'
            ELSE
              hinmmc = ' '
              WRITE (hinmmc,'(F6.3)') hinmm
            ENDIF
            IF (hinm.LE.0) THEN
              hinmc = ' -99.0'
            ELSE
              hinmc = ' '
              WRITE (hinmc,'(F6.3)') hinm
            ENDIF
            IF (vnpcmm.LE.0) THEN
              vnpcmmc = ' -99.0'
            ELSE
              vnpcmmc = ' '
              WRITE (vnpcmmc,'(F6.3)') vnpcmm
            ENDIF
            IF (vnpcm.LE.0) THEN
              vnpcmc = ' -99.0'
            ELSE
              vnpcmc = ' '
              WRITE (vnpcmc,'(F6.3)') vnpcm
            ENDIF
            IF (gnpcm.LE.0) THEN
              gnpcmc = ' -99.0'
            ELSE
              gnpcmc = ' '
              WRITE (gnpcmc,'(F6.3)') gnpcm
            ENDIF
            IF (gnpcmm.LE.0) THEN
              gnpcmmc = ' -99.0'
            ELSE
              gnpcmmc = ' '
              WRITE (gnpcmmc,'(F6.3)') gnpcmm
            ENDIF

            ! Calculate DAP data
            IF (stgdoy(9).LT.9999999) THEN 
              edap = Dapcalc(stgdoy(9),plyear,plday)
            ELSE
              EDAP = -99
              stgdoy(9) = -99
            ENDIF  
            IF (edatm.GT.0) THEN
             edapm = Dapcalc(edatm,plyear,plday)
            ELSE
             edapm = -99
            ENDIF 
            IF (drdat.GT.0) THEN 
              drdap = Dapcalc(drdat,plyear,plday)
            ELSE
             drdap = -99
            ENDIF  
            IF (drdatm.GT.0) THEN
             drdapm = Dapcalc(drdatm,plyear,plday)
            ELSE
             drdapm = -99
            ENDIF 
            IF (tsdat.GT.0) THEN 
              tsdap = Dapcalc(tsdat,plyear,plday)
            ELSE
             tsdap = -99
            ENDIF  
            IF (tsdatm.GT.0) THEN
             tsdapm = Dapcalc(tsdatm,plyear,plday)
            ELSE
             tsdapm = -99
            ENDIF 
            IF (jdat.GT.0) THEN 
              jdap = Dapcalc(jdat,plyear,plday)
            ELSE
             jdap = -99
            ENDIF  
            IF (jdatm.GT.0) THEN
             jdapm = Dapcalc(jdatm,plyear,plday)
            ELSE
             jdapm = -99
            ENDIF 
            IF (adat.LT.9999999) THEN 
              adap = Dapcalc(adat,plyear,plday)
            ELSE
              adap = -99
            ENDIF  
            IF (adatm.GT.0) THEN
              adapm = Dapcalc(adatm,plyear,plday)
            ELSE
              adapm = -99
            ENDIF  
            IF (stgdoy(5).LT.9999999) THEN 
              mdap = Dapcalc(stgdoy(5),plyear,plday)
            ELSE 
              mdap = -99
              stgdoy(5) = -99
            ENDIF     
            IF (mdatm.GT.0) THEN
              mdapm = Dapcalc(mdatm,plyear,plday)
            ELSE
              mdapm = -99 
            ENDIF
            
!-----------------------------------------------------------------------
            
            ! SCREEN WRITES FOR DSSAT IN SENSITIVITY RNMODE
            
            IF (FILEIOT(1:3).EQ.'DS4' .AND. CN.EQ.1
     &                                .AND. RNMODE.EQ.'E') THEN         
            
              CALL CSCLEAR5
              WRITE(*,9589)
              WRITE(*,*) ' '
              WRITE(*,9588)
              WRITE(*,9600)
              DO L = 7, 9
!                CALL CSYR_DOY(STGDOY(L),YEAR,DOY)
                CALL YR_DOY(STGDOY(L),YEAR,DOY)
                CALL Calendar(year,doy,dom,month)
                CNCTMP = 0.0
                IF (CWADSTG(L).GT..0) CNCTMP = CNADSTG(L)/CWADSTG(L)*100
                WRITE (*,'(I8,I4,1X,A3,I4,1X,I1,1X,A10,I6,F6.2,
     &           F6.1,I6,F6.1,F6.2,F6.2)')
     &           STGDOY(L),DOM,MONTH,
     &           Dapcalc(stgdoy(L),plyear,plday),L,STNAME(L),
     &           NINT(CWADSTG(L)),LAISTG(L),LNUMSTG(L),
     &           NINT(CNADSTG(L)),CNCTMP,1.0-WFPAV(L),1.0-NFPAV(L)
              ENDDO
              DO L = 1, 6
!                CALL CSYR_DOY(STGDOY(L),YEAR,DOY)
                CALL YR_DOY(STGDOY(L),YEAR,DOY)
                CALL Calendar(year,doy,dom,month)
                CNCTMP = 0.0
                IF (CWADSTG(L).GT..0) CNCTMP = CNADSTG(L)/CWADSTG(L)*100
                IF (STGDOY(L).GT.0) THEN
                  WRITE (*,'(I8,I4,1X,A3,I4,1X,I1,1X,A10,I6,F6.2,
     &             F6.1,I6,F6.1,F6.2,F6.2)')
     &             STGDOY(L),DOM,MONTH,
     &             Dapcalc(stgdoy(L),plyear,plday),L,STNAME(L),
     &             NINT(CWADSTG(L)),LAISTG(L),LNUMSTG(L),
     &             NINT(CNADSTG(L)),CNCTMP,1.0-WFPAV(L),1.0-NFPAV(L)
                ENDIF
              ENDDO
            
              WRITE(*,*)' '
              WRITE(*,*)' Press ENTER to continue'
              PAUSE ' '
              CALL CSCLEAR5
           
              WRITE (*,206)
              WRITE (*,3051) MAX(-99,edap),MAX(-99,edapm),
     x         adap,adapm,
     x         mdap,mdapm,
     x         NINT(gwam),NINT(gwamm),
     x         gwumc,gwummc,
     x         NINT(hnumam),NINT(hnumamm),
     x         hnumgm,hnumgmm,
     x         hiam,hiamm,
     x         laix,laixm,
     x         lnumsm,lnumsmm,
     x         NINT(tnumam),NINT(tnumamm),
     x         NINT(cwam),NINT(cwamm),
     x         NINT(vwam),NINT(vwamm),
     x         NINT(rwam),NINT(rwamm),
     x         NINT(carboac),NINT(carboacm),
     x         NINT(senwatc),NINT(senwatcm),
     x         NINT(rswam),NINT(rswamm)
 3051         FORMAT (
     x        6X, 'Emergence (DAP)                 ',4X,I7,  6X,I7,  /,
     x        6X, 'Anthesis (DAP)                  ',4X,I7,  6X,I7,  /,
     x        6X, 'Maturity (DAP)                  ',4X,I7,  6X,I7,  /,
     x        6X, 'Product wt (kg dm/ha;no loss)   ',4X,I7, 6X,I7,  /,
     x        6X, 'Product unit weight (g dm)      ',5X,A6, 7X,A6,  /,
     x        6X, 'Product number (no/m2)          ',4X,I7,  6X,I7,  /,
     x        6X, 'Product number (no/group)       ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Product harvest index (ratio)   ',4X,F7.2,6X,F7.2,/,
     x        6X, 'Maximum leaf area index         ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Final leaf number (one axis)    ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Final shoot number (#/m2)       ',4X,I7  ,6X,I7  ,/,
     x        6X, 'Canopy (tops) wt (kg dm/ha)     ',4X,I7,  6X,I7,  /,
     x        6X, 'Vegetative wt (kg dm/ha)        ',4X,I7,  6X,I7,  /,
     x        6X, 'Root wt (kg dm/ha)              ',4X,I7,  6X,I7,  /,
     x        6X, 'Assimilate wt (kg dm/ha)        ',4X,I7,  6X,I7,  /,
     x        6X, 'Senesced wt (kg dm/ha)          ',4X,I7,  6X,I7,  /,
     x        6X, 'Reserves wt (kg dm/ha)          ',4X,I7,  6X,I7)

              WRITE(*,*)' '
              WRITE(*,*)' Press ENTER to continue'
              PAUSE ' '
              CALL CSCLEAR5
     
              WRITE (*,206)
              WRITE (*,3052)                              
     x         nuam,nuamm,
     x         sennatc,sennatcm,
     x         cnam,cnamm,
     x         rnam,rnamm,
     x         vnam,vnamm,
     x         gnam,gnamm,
     x         hinm,hinmm,
     x         gnpcm,gnpcmm,
     x         vnpcm,vnpcmm,
     x         NINT(cwaa),NINT(cwaam),
     x         vnaa,cnaam,
     x         lnpca,lnpcam
 3052         FORMAT (
     x        6X, 'N uptake (kg/ha)                 ',4X,F7.1,6X,F7.1,/,
     x        6X, 'N senesced (kg/ha)               ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Above-ground N (kg/ha)           ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Root N (kg/ha)                   ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Vegetative N (kg/ha)             ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Product N (kg/ha)                ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Product N harvest index (ratio)  ',4X,F7.2,6X,F7.2,/,
     x        6X, 'Product N (%)                    ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Vegetative N (%)                 ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Leaf+stem wt,anthesis (kg dm/ha) ',4X,  I7,6X,I7,  /,
     x        6X, 'Leaf+stem N,anthesis (kg/ha)     ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Leaf N,anthesis (%)              ',4X,F7.1,6X,F7.1)

              WRITE(*,*)' '
              WRITE(*,*)' Press ENTER to continue'
              PAUSE ' '
              CALL CSCLEAR5
              CALL CSCLEAR5
              CALL CSCLEAR5
              CALL CSCLEAR5
            
            ENDIF
            
            ! END OF SCREEN WRITES FOR DSSAT SENSITIVITY RNMODE
            
!-----------------------------------------------------------------------

            IF (IDETO.NE.'N'.OR.IDETL.EQ.'A') THEN
            
              ! PLANT EVALUATION (MEASURED - SIMULATED COMPARISONS)
C  FO - 07/16/2021 Added more characters for H#AMS and H#GMS because of GLUE error.
              
              WRITE (fnumwrk,*) 'Writing EVALUATION'
              
              EVHEADER = ' '
              FNAMETMP = ' '
              EVHEADER(1:14) = '*EVALUATION : '
              FNAMETMP(1:12) = 'Evaluate.OUT'
              IF (RUN.EQ.1 .AND. RUNI.EQ.1) THEN
               OPEN (UNIT = FNUMTMP,FILE = FNAMETMP,STATUS = 'UNKNOWN')
               WRITE (FNUMTMP,'(A17)') '$PLANT EVALUATION'
               CLOSE(FNUMTMP)
               EVALOUT = 0
               EVHEADNM = 0
              ENDIF
              
              IF (EVHEADNM.EQ.0) THEN
                IF (EXCODE.NE.EXCODEP.AND.EVALOUT.GT.1 .OR.
     &              RUN.EQ.1.AND.RUNI.EQ.1) THEN
                 EVHEADNM = EVHEADNM + 1
                 OPEN (UNIT = FNUMTMP,FILE = FNAMETMP,
     &            POSITION = 'APPEND')
                 WRITE (FNUMTMP,*) ' '
                 IF (EVHEADNM.EQ.1) THEN
                   WRITE (FNUMTMP,993) EVHEADER,EXCODE,
     &              ENAME(1:25),MODNAME
  993              FORMAT (A14,A10,'  ',A25,2X,A8,/)
                 ELSE
                   WRITE (FNUMTMP,1995) EVHEADER, 
     &             'MANY??????','  REMAINING EXPERIMENTS  ',MODNAME
 1995              FORMAT (A14,A10,A25,A8,/)
                 ENDIF
                 WRITE (FNUMTMP,994)
  994            FORMAT ('@RUN',
     x            ' EXCODE    ',
     x            '  TRNO RN',
     x            ' CR',
     x            ' EDAPS EDAPM',
     x            ' DRAPS DRAPM',
     x            ' TSAPS TSAPM',
     x            ' ADAPS ADAPM',
     x            ' MDAPS MDAPM',
     x            ' HWAMS HWAMM',
     x            ' HWUMS HWUMM',
     x            '    H#AMS H#AMM',
     x            '    H#GMS H#GMM',
     x            ' LAIXS LAIXM',
     x            ' L#SMS L#SMM',
     x            ' T#AMS T#AMM',
     x            ' CWAMS CWAMM',
     x            ' VWAMS VWAMM',
     x            ' HIAMS HIAMM',
     x            ' HN%MS HN%MM',
     x            ' VN%MS VN%MM',
     x            ' CNAMS CNAMM',
     x            ' HNAMS HNAMM',
     x            ' HINMS HINMM')
                 CLOSE(FNUMTMP)
                ENDIF
              ENDIF
                
              IF (EXCODE.NE.EXCODEP) EVALOUT = 0
              EVALOUT = EVALOUT + 1
              
              IF (STGDOY(5).GT.0 .AND. ADAT.GT.0) THEN
                AMDAYS = Dapcalc(stgdoy(5),plyear,plday) 
     X                 - Dapcalc(adat,plyear,plday)
              ELSE
                AMDAYS = -99
              ENDIF 
              IF (MDATM.GT.0 .AND. ADATM.GT.0) THEN
                AMDAYM = Dapcalc(mdatm,plyear,plday) 
     X                 - Dapcalc(adatm,plyear,plday)
              ELSE
                AMDAYM = -99
              ENDIF 
              
              OPEN (UNIT = FNUMTMP,FILE = FNAMETMP,POSITION = 'APPEND') 
              
              WRITE (FNUMTMP,8404) RUN,EXCODE,TN,RN,CR,
     x        edap,edapm,
     x        drdap,drdapm,
     x        tsdap,tsdapm,
     x        adap,adapm,
     x        mdap,mdapm,         
     x        NINT(gwam),NINT(gwamm),
     x        gwumc,gwummc,
     x        NINT(hnumam),NINT(hnumamm),
     x        hnumgm,hnumgmm,
     x        laix,laixm,
     x        lnumsm,lnumsmm,
     x        NINT(tnumam),NINT(tnumamm),
     x        NINT(cwam),NINT(cwamm),
     x        NINT(vwam),NINT(vwamm),
     x        hiamc,hiammc,
     x        gnpcm,gnpcmm,
     x        vnpcmc,vnpcmmc,
     x        NINT(cnam),NINT(cnamm),
     x        NINT(gnam),NINT(gnamm),
     x        hinmc,hinmmc             
              
 8404         FORMAT (I4,1X,A10,I6,I3,1X,A2,
     x        I6,I6,
     x        I6,I6,
     x        I6,I6,
     x        I6,I6,
     x        I6,I6,
     x        I6,I6,
     x        A6,A6,
     x        1X,I8,I6,
     x        1X,F8.1,F6.1,
     x        F6.1,F6.1,
     x        F6.1,F6.1,
     x        I6,I6,
     x        I6,I6,
     x        I6,I6,
     x        A6,A6,
     x        F6.1,F6.1,
     x        A6,A6,
     x        I6,I6,
     x        I6,I6,
     x        A6,A6)
!     x        I7,I7,
!     x        I7,I7,
!     x        I6,I6)
              
              Close(FNUMTMP)       

!     VSH CSV output corresponding to Evaluate.OUT
      IF (FMOPT == 'C') then
         CALL CsvOutEvalCsCer(EXCODE, RUN, TN, RN,  REP, 
     &CR,Edap,Edapm,Drdap,Drdapm,Tsdap,Tsdapm,Adap,Adapm,Mdap,
     &Mdapm, Gwam, Gwamm, Gwumc, Gwummc, Hnumam, Hnumamm, Hnumgm,
     &Hnumgmm,Laix,Laixm, Lnumsm,Lnumsmm,Tnumam, Tnumamm, Cwam, 
     &Cwamm, Vwam, Vwamm, Hiamc,Hiammc,Gnpcm,Gnpcmm,Vnpcmc,Vnpcmmc,
     &Cnam, Cnamm, Gnam, Gnamm, Hinmc, Hinmmc,  
     &vCsvlineEvalCsCer, vpCsvlineEvalCsCer, vlngthEvalCsCer) 
     
         CALL LinklstEvalCsCer(vCsvlineEvalCsCer)
      END IF 
      
            ELSE
            
               OPEN (UNIT=FNUMTMP,FILE='EVALUATE.OUT',STATUS='UNKNOWN')
               CLOSE (UNIT=FNUMTMP, STATUS = 'DELETE')
               
            ENDIF  ! END OF PLANT EVALUATION
            
!-----------------------------------------------------------------------
            
            ! OVERVIEW
            
            IF (IDETO.EQ.'Y'.OR.IDETL.EQ.'A') THEN

              WRITE (fnumwrk,*) 'Writing OVERVIEW'
              
              FNAMETMP = ' '
              ! TF - Updated OVERVIEW.OUT name to avoid issues
              ! with case sensitive systems (07/27/2021) 
              FNAMETMP(1:12) = 'OVERVIEW.'//out
              
              IF (FILEIOT(1:2).EQ.'DS') THEN
                IF (RUN.EQ.1 .AND. RUNI.EQ.1) THEN
                  OPEN (UNIT = FNUMTMP, FILE = FNAMETMP)
                  WRITE(FNUMTMP,'("*SIMULATION OVERVIEW FILE")')
                ELSE
                  INQUIRE (FILE = FNAMETMP, EXIST = FEXIST)
                  IF (FEXIST) THEN
                    INQUIRE (FILE = 'OVERVIEW.OUT',OPENED = fopen)
                    IF (.NOT.fopen) THEN
                      OPEN (UNIT = FNUMTMP,FILE = FNAMETMP,
     &                 POSITION = 'APPEND')
                    ENDIF
                  ENDIF  
                ENDIF
                WRITE (FNUMTMP,*) ' '
                CALL HEADER(1, FNUMTMP, RUN)
              ELSE
                IF (RUN.EQ.1 .AND. RUNI.EQ.1) THEN
                  OPEN (UNIT = FNUMTMP, FILE = FNAMETMP)
                  WRITE (FNUMTMP,'(A15)') '$OVERVIEW'
                ELSE
                 OPEN (UNIT = FNUMTMP,FILE = FNAMETMP,POSITION='APPEND')
                ENDIF
              
                WRITE (FNUMTMP,'(/,A77,/)') OUTHED
                WRITE (FNUMTMP,103) MODEL
                WRITE (FNUMTMP,1031) MODNAME
                WRITE (FNUMTMP,1032) FILENEW
 1032           FORMAT (' FILE             ',A12)
                WRITE (FNUMTMP,104)
     &           EXCODE(1:8),' ',EXCODE(9:10),'  ',ENAME(1:47)
                WRITE (FNUMTMP,102) TN, TNAME
                WRITE (FNUMTMP,107) CR,VARNO,VRNAME
                CALL Calendar (plyear,plday,dom,month)
                WRITE (FNUMTMP,108)month,dom,NINT(pltpop),NINT(rowspc)
                WRITE (fnumtmp,109) tmaxx,tminn,NINT(co2max)
  109           FORMAT (' ENVIRONMENT      ','Tmax (max): ',F4.1,
     &           '  Tmin (min): ',F5.1,
     &           '  Co2 (max):',I4)
                WRITE (fnumtmp,110) iswwat, iswnit
  110           FORMAT (' MODEL SWITCHES   ','Water: ',A1,
     &           '  Nitrogen: ',A1)
              ENDIF
              
              !edap = Dapcalc(stgdoy(9),plyear,plday)
              !edapm = Dapcalc(edatm,plyear,plday)
              IF (edapm.GT.200) THEN
                WRITE (Fnumwrk,*)' '
                WRITE (Fnumwrk,'(A31,A31,A11)')
     &           'Measured emergence over 200DAP ',
     &           'Maybe reported before planting.',
     &           'Check files'
              ENDIF
              !adap = Dapcalc(adat,plyear,plday)
              !adapm = Dapcalc(adatm,plyear,plday)
              !mdap = Dapcalc(stgdoy(5),plyear,plday)
              !mdapm = Dapcalc(mdatm,plyear,plday)
              
              WRITE(FNUMTMP,9589)
              WRITE(fnumtmp,*)' '
              WRITE(FNUMTMP,'(A11,I4,A3,A60)')
     &         ' RUN NO.   ',RUN,'  ',ENAME                             
              IF (DYNAMIC.EQ.SEASEND) THEN
                WRITE(fnumtmp,*)' '
                WRITE(fnumtmp,'(A50,A25)')
     &          ' NB. RUN TERMINATED PREMATURELY (PROBABLY BECAUSE ',
     &          'OF MISSING WEATHER DATA) '
              ENDIF
              WRITE(fnumtmp,9588)
              WRITE(fnumtmp,9600)
              DO L = 7, 9
!                 CALL CSYR_DOY(STGDOY(L),YEAR,DOY)
                 CALL YR_DOY(STGDOY(L),YEAR,DOY)
                 CALL Calendar(year,doy,dom,month)
                 CNCTMP = 0.0
                 IF (CWADSTG(L).GT.0.0) 
     &             CNCTMP = CNADSTG(L)/CWADSTG(L)*100
                 WRITE (fnumtmp,'(I8,I4,1X,A3,I4,1X,I1,1X,A10,I6,F6.2, 
     &            F6.1,I6,F6.1,F6.2,F6.2)')
     &            STGDOY(L),DOM,MONTH,
     &            Dapcalc(stgdoy(L),plyear,plday),L,STNAME(L),
     &            NINT(CWADSTG(L)),LAISTG(L),LNUMSTG(L),
     &            NINT(CNADSTG(L)),CNCTMP,1.0-WFPAV(L),1.0-NFPAV(L)
              ENDDO
              DO L = 1, 6
!                CALL CSYR_DOY(STGDOY(L),YEAR,DOY)
                CALL YR_DOY(STGDOY(L),YEAR,DOY)
                CALL Calendar(year,doy,dom,month)
                CNCTMP = 0.0
                IF (CWADSTG(L).GT.0.0)CNCTMP = CNADSTG(L)/CWADSTG(L)*100
                IF (STGDOY(L).GT.0.AND.STGDOY(L).LT.9999999) THEN
                  WRITE (fnumtmp,'(I8,I4,1X,A3,I4,1X,I1,1X,A10,I6,F6.2,
     &             F6.1,I6,F6.1,F6.2,F6.2)')
     &             STGDOY(L),DOM,MONTH,
     &             Dapcalc(stgdoy(L),plyear,plday),L,STNAME(L),
     &             NINT(CWADSTG(L)),LAISTG(L),LNUMSTG(L),
     &             NINT(CNADSTG(L)),CNCTMP,1.0-WFPAV(L),1.0-NFPAV(L)
                ENDIF
              ENDDO
              IF (RUN.EQ.1 .AND. RUNI.EQ.1) THEN
              WRITE(fnumtmp,*)' '
              WRITE(fnumtmp,*)'BIOMASS = Above-ground dry wt (kg/ha)'
              WRITE(fnumtmp,*)'LEAF AREA  = Leaf area index (m2/m2)'
              WRITE(fnumtmp,*)
     &         'LEAF NUMBER  = Leaf number produced on main axis'
              WRITE(fnumtmp,*)'CROP N  = Above-ground N (kg/ha)'
              WRITE(fnumtmp,*)'CROP N% = Above-ground N conc (%)'
              WRITE(fnumtmp,*)
     &         'H2O STRESS = Photosynthesis stress,average (0-1,0=none)'
              WRITE(fnumtmp,*)
     &         'N STRESS = Photosynthesis stress,average (0-1,0=none)'
              ENDIF
              WRITE(fnumtmp,*)' '
              
              WRITE (FNUMTMP,206)
              WRITE (FNUMTMP,305) MAX(-99,edap),MAX(-99,edapm),
     x         adap,adapm,
     x         mdap,mdapm,
     x         NINT(gwam),NINT(gwamm),
     x         gwumc,gwummc,
     x         NINT(hnumam),NINT(hnumamm),
     x         hnumgm,hnumgmm,
     x         hiam,hiamm,
     x         laix,laixm,
     x         lnumsm,lnumsmm,
     x         NINT(tnumam),NINT(tnumamm),
     x         NINT(cwam),NINT(cwamm),
     x         NINT(vwam),NINT(vwamm),
     x         NINT(rwam),NINT(rwamm),
     x         NINT(carboac),NINT(carboacm),
     x         NINT(senwatc),NINT(senwatcm),
     x         NINT(rswam),NINT(rswamm),
     x         nuam,nuamm,
     x         sennatc,sennatcm,
     x         cnam,cnamm,
     x         rnam,rnamm,
     x         vnam,vnamm,
     x         gnam,gnamm,
     x         hinm,hinmm,
     x         gnpcm,gnpcmm,
     x         vnpcm,vnpcmm,
     x         NINT(cwaa),NINT(cwaam),
     x         vnaa,cnaam,
     x         lnpca,lnpcam
              
  305         FORMAT (
     x        6X, 'Emergence (DAP)                  ',4X,I7,  6X,I7,  /,
     x        6X, 'Anthesis (DAP)                   ',4X,I7,  6X,I7,  /,
     x        6X, 'Maturity (DAP)                   ',4X,I7,  6X,I7,  /,
     x        6X, 'Product wt (kg dm/ha;no loss)    ',4X,I7, 6X,I7,  /,
     x        6X, 'Product unit weight (g dm)       ',5X,A6, 7X,A6,  /,
     x        6X, 'Product number (no/m2)           ',4X,I7,  6X,I7,  /,
     x        6X, 'Product number (no/group)        ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Product harvest index (ratio)    ',4X,F7.2,6X,F7.2,/,
     x        6X, 'Maximum leaf area index          ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Final leaf number (one axis)     ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Final shoot number (#/m2)        ',4X,I7  ,6X,I7  ,/,
     x        6X, 'Canopy (tops) wt (kg dm/ha)      ',4X,I7,  6X,I7,  /,
     x        6X, 'Vegetative wt (kg dm/ha)         ',4X,I7,  6X,I7,  /,
     x        6X, 'Root wt (kg dm/ha)               ',4X,I7,  6X,I7,  /,
     x        6X, 'Assimilate wt (kg dm/ha)         ',4X,I7,  6X,I7,  /,
     x        6X, 'Senesced wt (kg dm/ha)           ',4X,I7,  6X,I7,  /,
     x        6X, 'Reserves wt (kg dm/ha)           ',4X,I7,  6X,I7,  /,
     x        6X, 'N uptake (kg/ha)                 ',4X,F7.1,6X,F7.1,/,
     x        6X, 'N senesced (kg/ha)               ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Above-ground N (kg/ha)           ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Root N (kg/ha)                   ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Vegetative N (kg/ha)             ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Product N (kg/ha)                ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Product N harvest index (ratio)  ',4X,F7.2,6X,F7.2,/,
     x        6X, 'Product N (%)                    ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Vegetative N (%)                 ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Leaf+stem wt,anthesis (kg dm/ha) ',4X,  I7,6X,I7,  /,
     x        6X, 'Leaf+stem N,anthesis (kg/ha)     ',4X,F7.1,6X,F7.1,/,
     x        6X, 'Leaf N,anthesis (%)              ',4X,F7.1,6X,F7.1)
              
              IF (run.EQ.1.AND.runi.EQ.1) THEN
            
              WRITE (FNUMTMP,'(/,A55,/,A55)')
     x        '  Seed N must be added to N uptake to obtain a         ',
     x        '  balance with N in above-ground plus root material    '
              WRITE (FNUMTMP,'(/,17(A55,/))')
     x        '  Measured data are obtained from the A file,either    ',
     x        '  directly or by calculation from other other variables',
     x        '  using the expressions given below:                   ',
     x        '                                                       ',
     x        '  Product wt       Harvest wt (HWAH) /                 ',
     x        '                               (Harvest%/100) (HPC)    ',
     x        '  Canopy wt        Grain wt (HWAM)+Vegetative wt (VWAm)',
     x        '  Vegetative wt    Canopy wt (CWAM) - grain wt (HWAM)  ',
     x        '    = leaf+stem+retained dead material                 ',
     x        '  Product unit wt  Grain yield (HWAM)/Grain # (G#AM)   ',
     x        '  Product #/area   Product#/tiller (H#SM) *            ',
     x        '                                   tiller number (T#AM)',
     x        '  Product #/group  Product#/area (H#AM) /              ',
     x        '                                tiller number (T#AM)   ',
     x        '  Harvest index    Product wt (HWAM)/Canopy wt.(CWAM)  ',
     x        '                                                       ',
     x        '  The same procedure is followed for nitrogen aspects  '
     
              ENDIF
            
              WRITE(fnumtmp,5500)
            
              PFPAV = -99.0
              PFGAV = -99.0
              DASH = ' - '
              DO tvI1 = 1,5  
                IF (TVI1.EQ.1)THEN
                  WRITE(fnumtmp,600) stname(8), dash, stname(tvi1), 
     &             daysc(tvI1), TMAXav(tvI1),TMINav(tvI1),sRADav(tvI1),
     &             DAYLav(tvI1),RAINcp(tvI1),ETC(tvI1),1.0-wfpav(tvi1),
     &             1.0-wfgav(tvi1), 1.0-nfpav(tvi1), 1.0-nfgav(tvi1), 
     &             PFPAV(TVI1), PFGAV(TVI1)
                ELSE
                  IF (DAYSC(tvi1).GT.0) THEN
                    WRITE(fnumtmp,600) stname(tvi1-1),dash,stname(tvi1),
     &              daysc(tvI1), TMAXav(tvI1),TMINav(tvI1),sRADav(tvI1),
     &              DAYLav(tvI1),RAINcp(tvI1),ETC(tvI1),1.0-wfpav(tvi1),
     &              1.0-wfgav(tvi1), 1.0-nfpav(tvi1), 1.0-nfgav(tvi1), 
     &              PFPAV(TVI1), PFGAV(TVI1)
                  ENDIF
                ENDIF
  600           FORMAT(1X,A10,A3,A10,I5,3F6.1,F7.2,2F7.1,4F7.3,2F7.2)
  610           FORMAT(1X,A10,13X,I5,3F6.1,F7.2,2I7,6F7.3)
              ENDDO
              IF (daysc(5).gt.0) then
                WRITE(fnumtmp,*) ' ' 
                WRITE(fnumtmp,600) stname(8),dash,stname(5), 
     &           daysc(0), TMAXav(0),TMINav(0),sRADav(0), 
     &           DAYLav(0), RAINcp(0),ETC(0), 1.0-wfpav(0), 
     &           1.0-wfgav(0), 1.0-nfpav(0), 1.0-nfgav(0), 
     &           PFPAV(0), PFGAV(0)
              ENDIF     
            
              !Resource productivity calculations
   
              DMP_Rain = -99.
              GrP_Rain = -99.
              DMP_ET = -99.
              GrP_ET = -99.
              DMP_EP = -99.
              GrP_EP = -99.
              DMP_Irr = -99.    
              GrP_Irr = -99.
 
              DMP_NApp = -99.
              GrP_NApp = -99.
              DMP_NUpt = -99.
              GrP_NUpt = -99.
 
              IF (RAINCP(0) > 1.E-3) THEN
                DMP_Rain = CWAM / RAINCp(0) 
                GrP_Rain = GWAM  / RAINCp(0)
              ENDIF
            
              IF (ETC(0) > 1.E-3) THEN
                DMP_ET = CWAM / ETC(0) 
                GrP_ET = GWAM  / ETC(0) 
              ENDIF
            
              IF (EPC(0) > 1.E-3) THEN
                DMP_EP = CWAM / EPC(0) 
                GrP_EP = GWAM  / EPC(0) 
              ENDIF
 
              IF (TOTIR > 1.E-3) THEN
                DMP_Irr = CWAM / TOTIR 
                GrP_Irr = GWAM  / TOTIR
              ENDIF
 
              IF (ISWNIT.NE.'N') THEN
                IF (Amtnit > 1.E-3) THEN
                  DMP_NApp = CWAM / Amtnit
                  GrP_NApp = GWAM  / Amtnit
                ENDIF
            
                IF (NUAD > 1.E-3) THEN
                  DMP_NUpt = CWAM / NUAD
                  GrP_NUpt = GWAM  / NUAD
                ENDIF
              ENDIF !ISWNIT == 'Y'
                        
              ! Productiviity outputs not written if run aborted        
              IF (DYNAMIC.NE.SEASEND) THEN
                WRITE (FNUMTMP, 1200) daysc(0), 
     &           RAINCP(0), DMP_Rain*0.1,DMP_Rain,GrP_Rain*0.1,GrP_Rain,
     &           ETC(0),  DMP_ET*0.1,   DMP_ET,   GrP_ET*0.1,   GrP_ET, 
     &           EPc(0),  DMP_EP*0.1,   DMP_EP,   GrP_EP*0.1,   GrP_EP
 
                IF (TOTIR > 1.E-3) THEN
                 WRITE(FNUMTMP, 1210) 
     &            TOTIR, DMP_Irr*0.1, DMP_Irr, GrP_Irr*0.1, GrP_Irr
                ENDIF
 
                IF (ISWNIT.NE.'N') THEN
                  IF (Amtnit > 1.E-3) THEN
                   WRITE(FNUMTMP, 1220) Amtnit, DMP_NApp, GrP_NApp 
                  ENDIF
                          
                  IF (NUAD > 1.E-3) THEN
                   WRITE(FNUMTMP, 1230) NUAD, DMP_NUpt,GrP_NUpt
                  ENDIF
                ENDIF !ISWNIT == 'Y'
              ENDIF
            
              WRITE(FNUMTMP,270)
              IF (CR.EQ.'WH') THEN 
                WRITE(FNUMTMP,300) 'WHEAT', NINT(GWAM)
              ELSEIF (CR.EQ.'BA') THEN 
                WRITE(FNUMTMP,300) 'BARLEY', NINT(GWAM)
              ENDIF  
              WRITE(FNUMTMP,'(110("*"))')
            
              CLOSE(FNUMTMP)   

            ELSE
            
               OPEN (UNIT=FNUMTMP,FILE='OVERVIEW,OUT')
               CLOSE (UNIT=FNUMTMP, STATUS = 'DELETE')
               
            ENDIF  ! END OF OVERVIEW

!-----------------------------------------------------------------------
            
            ! CSM SUMMARY ... not used in CROPSIM
            
            ! Store Summary labels and values in arrays to send to
            ! OPSUM routine for printing.  Integers are temporarily
            ! saved as real numbers for placement in real array.
            
            LABEL(1) = 'ADAT '; VALUE(1) = FLOAT(adat)
            LABEL(2) = 'MDAT '; VALUE(2) = FLOAT(stgdoy(5))
            LABEL(3) = 'DWAP '; VALUE(3) = sdrate
            LABEL(4) = 'CWAM '; VALUE(4) = cwam
            LABEL(5) = 'HWAM '; VALUE(5) = gwam
            LABEL(6) = 'HWAH '; VALUE(6) = gwam * hpc / 100.
            LABEL(7) = 'BWAH '; VALUE(7) = vwam * hbpc / 100. 
            
            LABEL(8)  = 'HWUM '; VALUE(8) = gwum
            LABEL(9)  = 'H#AM '; VALUE(9) = hnumam
            LABEL(10) = 'H#UM '; VALUE(10) = hnumgm
            LABEL(11) = 'NUCM '; VALUE(11) = nuad
            LABEL(12) = 'CNAM '; VALUE(12) = cnam
            LABEL(13) = 'GNAM '; VALUE(13) = gnam
            LABEL(14) = 'PWAM '; VALUE(14) = PWAM    
            LABEL(15) = 'LAIX '; VALUE(15) = LAIX    
            LABEL(16) = 'HIAM '; VALUE(16) = HIAM    
 
            LABEL(17) = 'DMPPM'; VALUE(17) = DMP_Rain 
            LABEL(18) = 'DMPEM'; VALUE(18) = DMP_ET                     
            LABEL(19) = 'DMPTM'; VALUE(19) = DMP_EP                     
            LABEL(20) = 'DMPIM'; VALUE(20) = DMP_Irr
            LABEL(21) = 'DPNAM'; VALUE(21) = DMP_NApp
            LABEL(22) = 'DPNUM'; VALUE(22) = DMP_NUpt
            
            LABEL(23) = 'YPPM ' ; VALUE(23) = GrP_Rain                  
            LABEL(24) = 'YPEM ' ; VALUE(24) = GrP_ET                   
            LABEL(25) = 'YPTM ' ; VALUE(25) = GrP_EP                    
            LABEL(26) = 'YPIM ' ; VALUE(26) = GrP_Irr
            LABEL(27) = 'YPNAM' ; VALUE(27) = GrP_NApp
            LABEL(28) = 'YPNUM' ; VALUE(28) = GrP_NUpt
 
            LABEL(29) = 'EDAT ' ; VALUE(29) = FLOAT(STGDOY(9))  
            
            LABEL(30) = 'NDCH ' ; VALUE(30) = FLOAT(DAYSC(0)) 
            LABEL(31) = 'TMINA' ; VALUE(31) = TMINAV(0)       
            LABEL(32) = 'TMAXA' ; VALUE(32) = TMAXAV(0)       
            LABEL(33) = 'SRADA' ; VALUE(33) = SRADAV(0)       
            LABEL(34) = 'DAYLA' ; VALUE(34) = DAYLAV(0)       
            LABEL(35) = 'CO2A ' ; VALUE(35) = CO2AV(0)        
            LABEL(36) = 'PRCP ' ; VALUE(36) = RAINCP(0)       
            LABEL(37) = 'ETCP ' ; VALUE(37) = ETC(0)      

            IF (FILEIOT(1:2).EQ.'DS') CALL SUMVALS (SUMNUM,LABEL,VALUE)
            
            ! END OF CSM SUMMARY
            
!-----------------------------------------------------------------------
            
            ! PLANT RESPONSES (SIMULATED)
            
            IF (IDETL.EQ.'D'.OR.IDETL.EQ.'A') THEN

              WRITE (fnumwrk,*) 'Writing PLANT RESPONSES (SIMULATED)'
              
              FNAMETMP = ' '
              FNAMETMP(1:12) = 'Plantres.'//out
              IF (RUN.EQ.1 .AND. RUNI.EQ.1) THEN
                OPEN (UNIT = FNUMTMP,FILE = FNAMETMP,STATUS = 'UNKNOWN')
                WRITE (FNUMTMP,'(A28)') '$PLANT RESPONSES (SIMULATED)'
                CLOSE(FNUMTMP)
              ENDIF
              
              IF (EXCODE.NE.EXCODEP.OR.TNAME(1:1).EQ.'*') THEN
                OPEN (UNIT=FNUMTMP,FILE=FNAMETMP,POSITION='APPEND')
                WRITE (FNUMTMP,*) ' '
                WRITE (TLINETMP,9951) EXCODE,MODNAME
 9951           FORMAT ('*RESPONSES(S):',A10,'  ',A8)
                IF (TNAME(1:1).EQ.'*') THEN
                  WRITE (FNUMTMP,'(A180)') TLINETMP
                ELSE
                  WRITE (FNUMTMP,'(A180)') TLINETMP
                ENDIF
                PLDAYP = 0
                WRITE (FNUMTMP,97)
   97           FORMAT ('@  RUN',
     x          ' EXCODE    ',
     x          'TRNO RN',
     x          '    CR',
     x          '  PDOY  EDAP',
     x          ' TSDAP  ADAP  MDAP',
     x          '  HWAM  HWUM',
     x          '  H#AM  H#GM  LAIX  L#SM  T#AM',
     x          '  CWAM  VWAM  HIAM  RWAM',
     x          '  HN%M  TNAM',
     x          '  CNAM  HNAM',
     x          '  HINM PLPOP',
     x          '  NICM',
     x          ' SRADA TMAXA TMINA  PRCP')
              ELSE
                OPEN (UNIT = FNUMTMP,FILE = FNAMETMP,POSITION ='APPEND')
              ENDIF
              
              IF (plday.LT.pldayp) THEN
                IF (varno.EQ.varnop) THEN
                  pldayp = plday + 365
                ELSE
                  pldayp = plday
                ENDIF
              ELSE
                pldayp = plday
              ENDIF
              varnop = varno
              
              WRITE (FNUMTMP,7401) RUN,EXCODE,TN,RN,CR,
     x         PLDAYP,
     x         edap,tsdap,adap,mdap,
     x         NINT(gwam),gwumc,
     x         NINT(hnumam),NINT(hnumgm),laix,lnumsm,tnumam,
     x         NINT(cwam),
     x         NINT(vwam),hiamc, NINT(rwam),
     x         gnpcmc,NINT(AMAX1(-99.0,cnam+rnam)),
     x         NINT(cnam),NINT(gnam),
     x         hinmc,pltpop,
     x         NINT(amtnit),
     &         sradav(0),tmaxav(0),tminav(0),NINT(raincp(0))
              
 7401          FORMAT (I6,1X,A10,1X,I3,I3,4X,A2,
     x         I6,
     x         I6,I6,I6,I6,
     x         I6,A6,
     x         I6,I6,F6.1,F6.1,F6.1,
     x         I6,
     x         I6,A6,I6,
     x         A6,I6,
     x         I6,I6,
     x         A6,F6.1,
     x         I6,
     x         3F6.1,I6)
              
              CLOSE(FNUMTMP)       ! END OF PLANT RESPONSES (SIMULATED)
            
            ELSE
               OPEN(UNIT=FNUMTMP,FILE='Plantres.out',STATUS = 'UNKNOWN')
               CLOSE(UNIT=FNUMTMP,STATUS='DELETE')
            ENDIF
            
!-----------------------------------------------------------------------
            
            ! PLANT RESPONSES (MEASURED)

            IF (IDETL.EQ.'D'.OR.IDETL.EQ.'A') THEN
            
              WRITE (fnumwrk,*) 'Writing PLANT RESPONSES (MEASURED)'
              
              FNAMETMP = ' '
              FNAMETMP(1:12) = 'Plantrem.'//out
              IF (RUN.EQ.1 .AND. RUNI.EQ.1) THEN
               OPEN (UNIT = FNUMTMP,FILE = FNAMETMP,STATUS = 'UNKNOWN')
               WRITE (FNUMTMP,'(A27)') '$PLANT RESPONSES (MEASURED)'
               CLOSE(FNUMTMP)
              ENDIF
              
              IF (EXCODE.NE.EXCODEP .OR.TNAME(1:1).EQ.'*') THEN
                OPEN (UNIT = FNUMTMP,FILE = FNAMETMP,POSITION='APPEND')
                WRITE (FNUMTMP,*) ' '
                TLINETMP = ' '
                WRITE (TLINETMP,99511) EXCODE,MODNAME
99511           FORMAT ('*RESPONSES(M):',A10,'  ',A8)
                IF (TNAME(1:1).EQ.'*') THEN
                  WRITE (FNUMTMP,'(A180)') TLINETMP
                ELSE
                  WRITE (FNUMTMP,'(A180)') TLINETMP
                ENDIF
                WRITE (FNUMTMP,97)
              ELSE
                OPEN (UNIT = FNUMTMP,FILE = FNAMETMP,POSITION='APPEND')
              ENDIF
              
              IF (CNAMM.GT.0.0 .AND. RNAMM.GT.0.0) THEN
                tnamm = cnamm+rnamm
              ELSE
                tnamm = -99.0
              ENDIF
              
              WRITE (FNUMTMP,7402) RUN,EXCODE,TN,RN,CR,
     x         PLDAYP,
     x         MAX(-99,edapm),
     x         tsdapm,
     x         Dapcalc(adatm,plyear,plday),
     x         Dapcalc(mdatm,plyear,plday),
     x         NINT(gwamm),gwummc,
     x         NINT(hnumamm),NINT(hnumgmm),laixm,lnumsmm,tnumamm,
     x         NINT(cwamm),
     x         NINT(vwamm),hiammc, NINT(rwamm),
     x         gnpcmmc,NINT(tnamm),
     x         NINT(cnamm),NINT(gnamm),
     x         hinmmc,pltpop,
     x         NINT(amtnit),
     &         sradav(0),tmaxav(0),tminav(0),NINT(raincp(0))
              
 7402          FORMAT (I6,1X,A10,1X,I3,I3,4X,A2,
     x         I6,
     x         I6,I6,I6,I6,
     x         I6,A6,
     x         I6,I6,F6.1,F6.1,F6.1,
     x         I6,
     x         I6,A6,I6,
     x         A6,I6,
     x         I6,I6,
     x         A6,F6.1,
     x         I6,
     x         3F6.1,I6)
              
              CLOSE(FNUMTMP)       

            ELSE
               OPEN(UNIT=FNUMTMP,FILE='Plantrem.out',STATUS = 'UNKNOWN')
               CLOSE(UNIT=FNUMTMP,STATUS='DELETE')
            ENDIF    ! END OF PLANT RESPONSES (MEASURED)
            
!-----------------------------------------------------------------------

            IF (IDETL.EQ.'A') THEN
            
              ! PLANT ERRORS (A-file data)
              
              WRITE (fnumwrk,*) 'Writing PLANT ERRORS (A)'
              
              FNAMETMP = ' '
              FNAMETMP(1:12) = 'Plantera.'//out
              IF (RUN.EQ.1 .AND. RUNI.EQ.1) THEN
                OPEN (UNIT = FNUMTMP,FILE = FNAMETMP,STATUS = 'UNKNOWN')
                WRITE (FNUMTMP,'(A21)') '$ERRORS (A-FILE DATA)'
                WRITE (FNUMTMP,501)
                CLOSE(FNUMTMP)
              ENDIF
              
              IF (EXCODE.NE.EXCODEP) THEN
                OPEN (UNIT = FNUMTMP,FILE = FNAMETMP,POSITION='APPEND')
                WRITE (FNUMTMP,996) OUTHED(11:77)
  996           FORMAT (/,'*ERRORS(A):',A67,/)
                WRITE (FNUMTMP,896)
  896           FORMAT ('@  RUN',
     x          ' EXCODE    ',
     B          '   TRNO RN',
     C          '    CR',
     D          '    EDAP   EDAPE',
     E          '    ADAP   ADAPE',
     F          '    MDAP   MDAPE',
     G          '    HWAH   HWAHE',
     H          '    HWUM   HWUME',
     I          '    H#AM   H#AME',
     J          '    H#GM   H#GME',
     K          '    LAIX   LAIXE',
     L          '    L#SM   L#SME',
     M          '    S#AM   S#AME',
     N          '    CWAM   CWAME',
     O          '    VWAM   VWAME',
     P          '    HIAM   HIAME',
     Q          '    HN%M   HN%ME',
     R          '    CNAM   CNAME',
     S          '    HNAM   HNAME')
                CLOSE(FNUMTMP)
              ENDIF
              
              OPEN (UNIT = FNUMTMP,FILE = FNAMETMP,POSITION = 'APPEND')
              
              IF (edatm.GT.0) THEN
               emdaterr = Dapcalc(stgdoy(9),plyear,plday)-
     X         Dapcalc(edatm,plyear,plday)
              ELSE
               emdaterr = -99
              Endif
              IF (adatm.GT.0) THEN
               adaterr = Dapcalc(adat,plyear,plday)-
     X         Dapcalc(adatm,plyear,plday)
              ELSE
               adaterr = -99
              Endif
              IF (mdatm.GT.0) THEN
               mdaterr = Dapcalc(stgdoy(5),plyear,plday)-
     X         Dapcalc(mdatm,plyear,plday)
              ELSE
               mdaterr = -99
              Endif
              IF (hwahm.GT.0 .AND. gwam.GT.0 .AND. hpc.GT.0) THEN
               hwaherr = 100.*(gwam*hpc/100.-hwahm)/(gwam*hpc/100.)
               IF (hwaherr.GT.99999.0) hwaherr = 99999.0
               IF (hwaherr.LT.-9999.0) hwaherr = -9999.0
              ELSE
               hwaherr = -99
              ENDIF
              IF (gwumm.GT.0 .AND. gwum.GT.0) THEN
               gwumerr = 100.0*(gwum-gwumm)/gwum
              ELSE
               gwumerr = -99
              ENDIF
              IF (hnumamm.GT.0 .AND. hnumam.GT.0) THEN
               hnumaerr = 100.0*(hnumam-hnumamm)/(hnumam)
              ELSE
               hnumaerr = -99
              ENDIF
              IF (hnumgmm.GT.0 .AND. hnumgm.GT.0) THEN
               hnumgerr = 100.0*((hnumgm-hnumgmm)/hnumgm)
              ELSE
               hnumgerr = -99
              ENDIF
              IF (laixm.GT.0 .AND. laix.GT.0) THEN
               laixerr = 100.0*((laix-laixm)/laix)
              ELSE
               laixerr = -99
              ENDIF
              IF (lnumsmm.GT.0 .AND. lnumsm.GT.0) THEN
               lnumserr = 100.0*((lnumsm-lnumsmm)/lnumsm)
              ELSE
               lnumserr = -99
              ENDIF
              IF (tnumamm.GT.0 .AND. tnumam.GT.0) THEN
               tnumaerr = 100.0*((tnumam-tnumamm)/tnumam)
              ELSE
               tnumaerr = -99
              Endif
              IF (cwamm.GT.0 .AND. cwam.GT.0) THEN
               cwamerr = 100.0*(cwam-cwamm)/cwam
              ELSE
               cwamerr = -99
              Endif
              IF (vwamm.GT.0 .AND. vwam.GT.0) THEN
               vwamerr = 100.0*(vwam-vwamm)/vwam
              ELSE
               vwamerr = -99
              Endif
              IF (hiamm.GT.0 .AND. hiam.GT.0) THEN
               hiamerr = 100.0*(hiam-hiamm)/hiam
              ELSE
               hiamerr = -99
              Endif
              IF (gnpcmm.GT.0 .AND. gnpcm.GT.0) THEN
               gnpcmerr = 100.0*(gnpcm-gnpcmm)/gnpcm
              ELSE
               gnpcmerr = -99
              Endif
              IF (cnamm.GT.0 .AND. cnam.GT.0) THEN
               cnamerr = 100.0*(cnam-cnamm)/cnam
              ELSE
               cnamerr = -99
              Endif
              IF (gnamm.GT.0 .AND. gnam.GT.0) THEN
               gnamerr = 100.0*(gnam-gnamm)/gnam
              ELSE
               gnamerr = -99
              Endif
              
              WRITE (FNUMTMP,8401) RUN,EXCODE,TN,RN,CR,
     x         edap,emdaterr,
     x         adap,adaterr,
     x         mdap,mdaterr,
     x         NINT(gwam),NINT(hwaherr),
     x         gwumc,NINT(gwumerr),
     x         NINT(hnumam),NINT(hnumaerr),
     x         hnumgm,NINT(hnumgerr),
     x         laix,NINT(laixerr),
     x         lnumsm,NINT(lnumserr),
     x         NINT(tnumam),NINT(tnumaerr),
     x         NINT(cwam),NINT(cwamerr),
     x         NINT(vwam),NINT(vwamerr),
     x         hiamc,NINT(hiamerr),
     x         gnpcm,NINT(gnpcmerr),
     x         NINT(cnam),NINT(cnamerr),
     x         NINT(gnam),NINT(gnamerr)

 8401         FORMAT (I6,1X,A10,1X,I6,I3,4X,A2,
     A         I8,  I8,
     B         I8,  I8,
     C         I8,  I8,
     D         I8,  I8,
     E      2X,A6,  I8,
     F         I8,  I8,
     G         F8.1,I8,
     H         F8.1,I8,
     I         F8.1,I8,
     J         I8  ,I8,
     K         I8,  I8,
     L         I8,  I8,
     M      2X,A6,  I8,
     N         F8.1,I8,
     O         I8,  I8,
     P         I8,  I8)
              
              CLOSE(FNUMTMP)       

            ELSE
               OPEN(UNIT=FNUMTMP,FILE='Plantera.out',STATUS = 'UNKNOWN')
               CLOSE(UNIT=FNUMTMP,STATUS='DELETE')
            ENDIF    ! END OF PLANT ERRORS (A)
           
!-----------------------------------------------------------------------
            
            ! PLANT ERRORS (TIME-COURSE)

            IF (IDETL.EQ.'A') THEN
            
              IF (CFLTFILE.NE.'Y' .OR. FROPADJ.GT.1) THEN
              
                WRITE (fnumwrk,*) 'Cannot write PLANT ERRORS (T)'
                IF (FROPADJ.GT.1)
     &           WRITE (fnumwrk,*) 'Frequency of output > 1 day'  
                IF (RUN.EQ.1 .AND. RUNI.EQ.1) CFLHEAD = 'Y'
              
              ELSE
              
                WRITE (fnumwrk,*) 'Writing PLANT ERRORS (T)'
              
                FNAMETMP = ' '
                FNAMETMP(1:12) = 'Plantert.'//out
                IF (RUN.EQ.1 .AND. RUNI.EQ.1 .OR. CFLHEAD.EQ.'Y') THEN
                 CFLHEAD = 'N'
                 OPEN (UNIT=FNUMTMP,FILE=FNAMETMP,STATUS='UNKNOWN')
                 WRITE (FNUMTMP,'(A21)') '$ERRORS (T-FILE DATA)'
                 WRITE (FNUMTMP,1501)
                 CLOSE(FNUMTMP)
                ENDIF
              
                INQUIRE (FILE = 'PlantGro.OUT',OPENED = fopen)
                IF (fopen) CLOSE (NOUTPG)
              
                STARNUM = 0
                OPEN (UNIT=FNUMT,FILE='Measured.out',STATUS='UNKNOWN')
                DO WHILE (TLINET(1:1).NE.'@')
                  TLINET = ' '
                  READ (FNUMT,1502,END=1600,ERR=1600) TLINET
 1502             FORMAT(A180)
                  IF (TLINET(1:1).EQ.'*') STARNUM = STARNUM + 1
                  IF (TLINET(1:1).EQ.'@') THEN
                    IF (STARNUM.NE.STARNUMM) THEN
                      TLINET = ' '
                      READ (FNUMT,1502,END=1600,ERR=1600) TLINET
                    ENDIF
                  ENDIF
                ENDDO
                tlinet(1:1) = ' '
                STARNUM = 0
              
                OPEN (UNIT=NOUTPG,FILE='PlantGro.OUT',STATUS='UNKNOWN')
              
                DO WHILE (TLINEGRO(1:1).NE.'@')
                  TLINEGRO = ' '
                  READ (NOUTPG,'(A180)') TLINEGRO
                  IF (TLINEGRO(1:4).EQ.'*RUN') STARNUM = STARNUM + 1
                  IF (TLINEGRO(1:1).EQ.'@') THEN
                    IF (STARNUM.NE.STARNUMO) THEN
                      TLINEGRO = ' '
                      READ (NOUTPG,'(A180)') TLINEGRO
                    ENDIF
                  ENDIF
                ENDDO
                tlinegro(1:1) = ' '
              
                ! Find headers from Measured file
                DO L = 1,20
                  CALL Getstr(tlinet,l,thead(l))
                  IF (THEAD(L)(1:3).EQ.'-99') EXIT
                  IF (THEAD(L)(1:3).EQ.'DAP') tfdapcol = l
                ENDDO
                TFCOLNUM = L-1
                IF (TFCOLNUM.LE.0) THEN
                  WRITE (FNUMWRK,*) 'No columns found in T-file!'
                  GO TO 7777
                ENDIF
              
                ! Make new header line
                TLINETMP = ' '
                TLINETMP(1:1) = '@'
                DO L = 1, TFCOLNUM
                  TLPOS = (L-1)*6+1
                  IF (THEAD(L).EQ.'TRNO'.OR.THEAD(L).EQ.'YEAR'.OR.
     &              THEAD(L).EQ.'DATE') THEN
                    TLINETMP(TLPOS+2:TLPOS+5)=THEAD(L)(1:4)
                  ELSEIF(THEAD(L).EQ.'DOY'.OR.THEAD(L).EQ.'DAP' .OR.
     &              THEAD(L).EQ.'DAS'.OR.THEAD(L).EQ.'DAY') THEN
                    TLINETMP(TLPOS+3:TLPOS+5)=THEAD(L)(1:3)
                  ELSE
                    WRITE (TCHAR,'(I6)') NINT(ERRORVAL*100.0)
                    TLINETMP(TLPOS+1:TLPOS+4) = THEAD(L)(1:4)
                    TLINETMP(TLPOS+5:TLPOS+5) = 'E'
                  ENDIF
                ENDDO
              
                ! Find corresponding columns in PlantGro.OUT
                DO L = 1,TFCOLNUM
                  pgrocol(l) = Tvicolnm(tlinegro,thead(l))
                ENDDO
              
                OPEN (UNIT=FNUMTMP,FILE=FNAMETMP,POSITION='APPEND')
              
                 WRITE (FNUMTMP,2996) OUTHED(11:77)
 2996            FORMAT (/,'*ERRORS(T):',A67,/)
                 tlinet(1:1) = '@'
                 WRITE (FNUMTMP,'(A180)') TLINETMP
              
                ! Read data lines, match dates, calculate errors, write
                DO L1 = 1,200
                  TLINET = ' '
                  READ (FNUMT,7778,ERR=7777,END=7777) TLINET
 7778             FORMAT(A180)
                  IF (TLINET(1:1).EQ.'*') GO TO 7777
                  IF (TLINET(1:6).EQ.'      ') GO TO 7776
                  IF (TLINET(1:1).EQ.'!') GO TO 7776
                  CALL Getstri(tlinet,tfdapcol,tfdap) 
                  IF (TFDAP.LE.0) THEN
                    WRITE (FNUMWRK,*) 'DAP in T-file <= 0!'
                    GO TO 7777
                  ENDIF
                  DO WHILE (tfdap.NE.pgdap)
                    TLINEGRO = ' '
                    READ (NOUTPG,7779,ERR=7777,END=7777) TLINEGRO
                    CALL Getstri(tlinegro,pgrocol(tfdapcol),pgdap)
                    IF (PGDAP.LT.0) THEN
                      WRITE (FNUMWRK,*) 'DAP in Plantgro file < 0!'
                      GO TO 7777
                    ENDIF
                  ENDDO
 7779             FORMAT(A180)
                  TLINETMP = ' '
                  DO L = 1, TFCOLNUM
                    CALL Getstrr(tlinet,l,tfval)
                    CALL Getstrr(tlinegro,pgrocol(l),pgval)
                    ERRORVAL = 0.0
                    IF (TFVAL.GT.0.0 .AND. 
     &               PGVAL.NE.-99 .AND.PGVAL.NE.0.0) THEN
                      ERRORVAL = 100.0 * (PGVAL - TFVAL) / PGVAL
                    ELSE
                      ERRORVAL = -99.0
                    ENDIF
                    IF (THEAD(L).EQ.'TRNO'.OR.THEAD(L).EQ.'YEAR' .OR.
     &                THEAD(L).EQ.'DOY'.OR.THEAD(L).EQ.'DAP' .OR.
     &                THEAD(L).EQ.'DAY' .OR.
     &                THEAD(L).EQ.'DAS'.OR.THEAD(L).EQ.'DATE') THEN
                      CALL Getstri(tlinet,l,tvi1)
                      WRITE (TCHAR,'(I6)') TVI1
                    ELSE
                      WRITE (TCHAR,'(I6)') NINT(ERRORVAL)
                    ENDIF
                    TLPOS = (L-1)*6+1
                    TLINETMP(TLPOS:TLPOS+5)=TCHAR
                  ENDDO
                  WRITE (FNUMTMP,'(A180)') TLINETMP
 7776             CONTINUE
                ENDDO
              
 7777           CONTINUE
                GO TO 1601
              
 1600           CONTINUE
                WRITE(fnumwrk,*)'End of file reading Measured.out'
                WRITE(fnumwrk,*)'Starnum and starnumm were: ',         
     &            starnum,starnumm
 1601           CONTINUE
              
                CLOSE (FNUMTMP)      
                CLOSE (FNUMT)
                CLOSE (NOUTPG)
                IF (FOPEN) THEN
                OPEN (UNIT=NOUTPG,FILE='PlantGro.OUT',POSITION='APPEND')
                ENDIF
              ENDIF

            ELSE
               OPEN(UNIT=FNUMTMP,FILE='Plantert.out',STATUS = 'UNKNOWN')
               CLOSE(UNIT=FNUMTMP,STATUS='DELETE')
            ENDIF      ! END OF PLANT ERRORS (TIME-COURSE)
            
!-----------------------------------------------------------------------
            
 8888       CONTINUE   ! Jump to here if cannot write outputs
            
!-----------------------------------------------------------------------
            
            EXCODEP = EXCODE
            WRITE (fnumwrk,*) ' '
            WRITE (fnumwrk,*) 'END OF HARVEST DAY OUTPUTS'
            WRITE (fnumwrk,*) 'WILL BEGIN NEW CYCLE (IF CALLED FOR)'
            WRITE (fnumwrk,*) ' '
            
            SEASENDOUT = 'Y'
 
            ! END MAIN OUTPUTS

            ! Need to re-initialize here because of automatic
            ! fertilization routines in DSSAT
            NFG = 1.0
            NFP = 1.0
            NFT = 1.0
            WFG = 1.0
            WFP = 1.0
            WFT = 1.0

          
            IF (IDETO.EQ.'E'.OR.IDETO.EQ.'N') THEN
                      
            ! CSM SUMMARY ... not used in CROPSIM
            
            ! Store Summary labels and values in arrays to send to
            ! OPSUM routine for printing.  Integers are temporarily
            ! saved as real numbers for placement in real array.
            ! (Nnot done earlier because no Overview called for)
              !Resource productivity calculations
   
              DMP_Rain = -99.
              GrP_Rain = -99.
              DMP_ET = -99.
              GrP_ET = -99.
              DMP_EP = -99.
              GrP_EP = -99.
              DMP_Irr = -99.    
              GrP_Irr = -99.
 
              DMP_NApp = -99.
              GrP_NApp = -99.
              DMP_NUpt = -99.
              GrP_NUpt = -99.
 
              IF (RAINCP(0) > 1.E-3) THEN
                DMP_Rain = CWAM / RAINCp(0) 
                GrP_Rain = GWAM  / RAINCp(0)
              ENDIF
            
              IF (ETC(0) > 1.E-3) THEN
                DMP_ET = CWAM / ETC(0) 
                GrP_ET = GWAM  / ETC(0) 
              ENDIF
            
              IF (EPC(0) > 1.E-3) THEN
                DMP_EP = CWAM / EPC(0) 
                GrP_EP = GWAM  / EPC(0) 
              ENDIF
 
              IF (TOTIR > 1.E-3) THEN
                DMP_Irr = CWAM / TOTIR 
                GrP_Irr = GWAM  / TOTIR
              ENDIF
 
              IF (ISWNIT.NE.'N') THEN
                IF (Amtnit > 1.E-3) THEN
                  DMP_NApp = CWAM / Amtnit
                  GrP_NApp = GWAM  / Amtnit
                ENDIF
            
                IF (NUAD > 1.E-3) THEN
                  DMP_NUpt = CWAM / NUAD
                  GrP_NUpt = GWAM  / NUAD
                ENDIF
              ENDIF !ISWNIT == 'Y'
            
              LABEL(1) = 'ADAT '; VALUE(1) = FLOAT(adat)
              LABEL(2) = 'MDAT '; VALUE(2) = FLOAT(stgdoy(5))
              LABEL(3) = 'DWAP '; VALUE(3) = sdrate
              LABEL(4) = 'CWAM '; VALUE(4) = cwam
              LABEL(5) = 'HWAM '; VALUE(5) = gwam
              LABEL(6) = 'HWAH '; VALUE(6) = gwam * hpc / 100.
              LABEL(7) = 'BWAH '; VALUE(7) = vwam * hbpc / 100. 
            
              LABEL(8)  = 'HWUM '; VALUE(8) = gwum
              LABEL(9)  = 'H#AM '; VALUE(9) = hnumam
              LABEL(10) = 'H#UM '; VALUE(10) = hnumgm
              LABEL(11) = 'NUCM '; VALUE(11) = nuad
              LABEL(12) = 'CNAM '; VALUE(12) = cnam
              LABEL(13) = 'GNAM '; VALUE(13) = gnam
              LABEL(14) = 'PWAM '; VALUE(14) = PWAM    
              LABEL(15) = 'LAIX '; VALUE(15) = LAIX    
              LABEL(16) = 'HIAM '; VALUE(16) = HIAM    
 
              LABEL(17) = 'DMPPM'; VALUE(17) = DMP_Rain 
              LABEL(18) = 'DMPEM'; VALUE(18) = DMP_ET                   
              LABEL(19) = 'DMPTM'; VALUE(19) = DMP_EP                  
              LABEL(20) = 'DMPIM'; VALUE(20) = DMP_Irr
              LABEL(21) = 'DPNAM'; VALUE(21) = DMP_NApp
              LABEL(22) = 'DPNUM'; VALUE(22) = DMP_NUpt
            
              LABEL(23) = 'YPPM ' ; VALUE(23) = GrP_Rain                
              LABEL(24) = 'YPEM ' ; VALUE(24) = GrP_ET                 
              LABEL(25) = 'YPTM ' ; VALUE(25) = GrP_EP                  
              LABEL(26) = 'YPIM ' ; VALUE(26) = GrP_Irr
              LABEL(27) = 'YPNAM' ; VALUE(27) = GrP_NApp
              LABEL(28) = 'YPNUM' ; VALUE(28) = GrP_NUpt
 
              LABEL(29) = 'EDAP ' ; VALUE(29) = FLOAT(EDAP)     
              
              LABEL(30) = 'NDCH ' ; VALUE(30) = FLOAT(DAYSC(0)) 
              LABEL(31) = 'TMINA' ; VALUE(31) = TMINAV(0)       
              LABEL(32) = 'TMAXA' ; VALUE(32) = TMAXAV(0)       
              LABEL(33) = 'SRADA' ; VALUE(33) = SRADAV(0)       
              LABEL(34) = 'DAYLA' ; VALUE(34) = DAYLAV(0)       
              LABEL(35) = 'CO2A ' ; VALUE(35) = CO2AV(0)        
              LABEL(36) = 'PRCP ' ; VALUE(36) = RAINCP(0)       
              LABEL(37) = 'ETCP ' ; VALUE(37) = ETC(0)      

              IF (FILEIOT(1:2).EQ.'DS') CALL SUMVALS(SUMNUM,LABEL,VALUE)
            ENDIF

            ! To prevent massive Work.out files
            IF (FILEIOT.EQ.'DS4') CLOSE(FNUMWRK)

          ENDIF

        ENDIF        ! YEARDOY >= YEARPLT


      ELSEIF (DYNAMIC.EQ.SEASEND) THEN

        CLOSE (NOUTPG)
        INQUIRE (FILE = OUTPN, EXIST = FEXIST)
        IF (FEXIST) CLOSE (NOUTPN)
        CLOSE (NOUTPG2)
        CLOSE (NOUTPGF)
        CLOSE (FNUMWRK)

      ENDIF   ! Tasks

      cswdis = 'N'
!     IF(cswdis.EQ.'Y')
!    X CALL Disease(   If needed must check argument variables  )

  206          FORMAT(
     &          /,"*MAIN GROWTH AND DEVELOPMENT VARIABLES",//,   
     &             "@",5X,"VARIABLE",T44,"SIMULATED     MEASURED",/,  
     &                 6X,"--------",T44,"---------     --------")  
  270 FORMAT(/,'------------------------------------------------------',
     &'--------------------------------------------------------')
  300       FORMAT(/,10X,A," YIELD : ",I8," kg/ha    [Dry weight] ",/)

  501  FORMAT(/,
     & '! This file presents the differences between simulated and',/,
     & '! single-time measured values (eg.date of anthesis,yield) for'/,
     & '! individual runs. The abbreviations are based on those',/,
     & '! listed in the DATA.CDE file, with the simple abbreviation',/,
     & '! indicating the simulated value,the basic abbreviation plus',/,
     & '! a final E the error. The units for the latter are days for',/,
     & '! time differences (EDAP,ADAP,MDAP) and percentages of',/,
     & '! simulated values for the remainder.,'/,
     & ' ',/,
     & '! As usual, a -99 indicates that no data were available.')
     
  502  FORMAT(/,
     & '! This file summarizes the error differences reported in',/,
     & '! the files PLANTERA.OUT and PLANTERT.OUT. The errors in',/,
     & '! these files have been averaged taking actual values first,'/,
     & '! then the absolute value of the original errors. The number',/,
     & '! of runs used is indicated in the RUNS column.',/,
     & ' ',/,
     & '! The abbreviations are based on those listed in the',/,
     & '! DATA.CDE file, with the abbreviation plus a final #',/,
     & '! indicating the number of values actually used for',/,
     & '! averaging, the basic abbreviation plus a final E the',/,
     & '! averaged error. The units for the latter are days for',/,
     & '! time differences (EDAP,ADAP,MDAP) and percentages of',/,
     & '! simulated values for the remainder.',/,
     & ' ',/,
     & '! The major heading (the * line) includes codes for model',/,
     & '! and plant module. The batch column, as generated, is',/,
     & '! filled with a 1, and the batchcode with the code for',/,
     & '! the last experiment in the PLANTERA/T.OUT files. The',/,
     & '! entries in these columns can (and should) be changed if',/,
     & '! an overall summary file is constructed by combining files',/,
     & '! from different model and module runs.',/,
     & ' ',/,
     & '! As usual, a -99 indicates that no data were available.')

 1501  FORMAT(/,
     & '! This file summarizes the differences between simulated',/,
     & '! and measured values for individual runs. Abbreviations',/,
     & '! are based on those listed in the DATA.CDE file, but with',/,
     & '! an E added to indicate error. The units for the errors',/,
     & '! are % of simulated values (ie.100*[SIM-MEAS]/SIM).'/,
     & ' ',/,
     & '! A -99 indicates that no data were available. Here, this'/,
     & '! could be simulated as well as measured data.')

 1200       FORMAT(
     &      '------------------------------------------------------',
     &      '--------------------------------------------------------',
     &///,'*RESOURCE PRODUCTIVITY',
     &//,' Growing season length:', I4,' days ',
     &//,' Precipitation during growth season',T42,F7.1,' mm[rain]',
     & /,'   Dry Matter Productivity',T42,F7.2,' kg[DM]/m3[rain]',
     &                           T75,'=',F7.1,' kg[DM]/ha per mm[rain]',
     & /,'   Yield Productivity',T42,F7.2,' kg[grain yield]/m3[rain]',
     &                       T75,'=',F7.1,' kg[yield]/ha per mm[rain]',
     &//,' Evapotranspiration during growth season',T42,F7.1,' mm[ET]',
     & /,'   Dry Matter Productivity',T42,F7.2,' kg[DM]/m3[ET]',
     &                            T75,'=',F7.1,' kg[DM]/ha per mm[ET]',
     & /,'   Yield Productivity',T42,F7.2,' kg[grain yield]/m3[ET]',
     &                       T75,'=',F7.1,' kg[yield]/ha per mm[ET]',
     &//,' Transpiration during growth season',T42,F7.1,' mm[EP]',
     & /,'   Dry Matter Productivity',T42,F7.2,' kg[DM]/m3[EP]',
     &                            T75,'=',F7.1,' kg[DM]/ha per mm[EP]',
     & /,'   Yield Productivity',T42,F7.2,' kg[grain yield]/m3[EP]',
     &                       T75,'=',F7.1,' kg[yield]/ha per mm[EP]')
 1210 FORMAT(
     & /,' Irrigation during growing season',T42,F7.1,' mm[irrig]',
     & /,'   Dry Matter Productivity',T42,F7.2,' kg[DM]/m3[irrig]',
     &                       T75,'=',F7.1,' kg[DM]/ha per mm[irrig]',
     & /,'   Yield Productivity',T42,F7.2,' kg[grain yield]/m3[irrig]',
     &                       T75,'=',F7.1,' kg[yield]/ha per mm[irrig]')
 1220 FORMAT(
     & /,' N applied during growing season',T42,F7.1,' kg[N applied]/ha'
     & /,'   Dry Matter Productivity',T42,F7.1,' kg[DM]/kg[N applied]',
     & /,'   Yield Productivity',T42,F7.1,' kg[yield]/kg[N applied]')
 1230 FORMAT(
     & /,' N uptake during growing season',T42,F7.1,' kg[N uptake]/ha'
     & /,'   Dry Matter Productivity',T42,F7.1,' kg[DM]/kg[N uptake]',
     & /,'   Yield Productivity',T42,F7.1,' kg[yield]/kg[N uptake]')

 5500  FORMAT(/,'*ENVIRONMENTAL AND STRESS FACTORS',//,
     &' |-----Development Phase------|-------------Environment--------',
     &'------|----------------Stress-----------------|',/,
     &30X,'|--------Average-------|---Cumulative--|         (0=Min, 1=',
     &'Max Stress)         |',/,
     &25X,'Time  Temp  Temp Solar Photop         Evapo |----Water---|-',
     &'-Nitrogen--|--Phosphorus-|',/,
     &25X,'Span   Max   Min   Rad  [day]   Rain  Trans  Photo',9X,'Pho',
     &'to         Photo',/,
     &25X,'days    øC    øC MJ/m2     hr     mm     mm  synth Growth ',
     &' synth Growth  synth Growth',/,110('-'))

 9588       FORMAT(
     &      /,' ...... DATE ......  GROWTH STAGE BIOMASS   LEAF  
     &     CROP N      STRESS')     
 9589         FORMAT(//,
     &     '*SIMULATED CROP AND SOIL STATUS AT MAIN DEVELOPMENT STAGES')
 9600       FORMAT(' YEARDOY DOM MON DAP ............. kg/ha AREA NUMBER
     &  kg/ha   %   H2O    N')

      END  ! CSCER040
