!=======================================================================
!  FRESHWT, Subroutine, C.H.Porter, K.J.Boote, J.I.Lizaso, G. Hoogenboom
!-----------------------------------------------------------------------
!  Computes fresh pod weigt
!-----------------------------------------------------------------------
!  REVISION       HISTORY
!  05/09/2007     Written. KJB, CHP, JIL, RR
!  02/27/2008 JIL Added pod quality for snap bean. JIL
!  10/02/2020 ??  Add fresh weight for bell pepper
!  04/01/2021 FO  Added MultiHarvest. (FO, GH, VSH, AH, KJB)
!  07/09/2022 GH  Add fresh weight for cucumber using tomato
!  08/01/2022 FO  Updated source code format
!  01/13/2023 FO  Updated variables in output file FreshWt.OUT
!  03/17/2023 FO  Major changes for Multi-Harvest
!  07/27/2023 FO  Adding output for Harv. Fresh Pod weight.
!  05/16/2024 FO  Inc. output space from 8 to 9 chars FreshWt.OUT 
!-----------------------------------------------------------------------
!  Called from:  PODS
!=======================================================================

      SUBROUTINE FRESHWT(DYNAMIC, ISWFWT,                
     &        YRPLT, XMAGE, NR2TIM, PHTIM,                      !Input 
     &        WTSD,SDNO,WTSHE,SHELN,                            !Input 
     &        HPODWT,HSDWT,HSHELWT)                             !Output

!-----------------------------------------------------------------------
      USE ModuleDefs 
      USE ModuleData
      
      IMPLICIT NONE
      EXTERNAL GET_CROPD, INFO, GETLUN, HEADER, YR_DOY, TIMDIF,
     & ERROR, WARNING
      SAVE

      CHARACTER*1   ISWFWT
      CHARACTER*2   CROP
      CHARACTER*6   ERRKEY
      PARAMETER (ERRKEY = 'FRSHWT')
      CHARACTER*12 FWFile
      CHARACTER*16 CROPD
      CHARACTER*78 MSG(4)

      INTEGER DAP, DAS, DOY, DYNAMIC, ERRNUM, I
      INTEGER NOUTPF, NPP, NR2TIM, TIMDIF
      INTEGER YEAR, YRDOY, YRPLT, HARVF
      INTEGER CHNUM,CPODN,CMFNM

      REAL AvgDMC, AvgDPW, AvgFPW, PodDiam, PodLen
      REAL PAGE, XMAGE, PodAge
      REAL TDPW, TFPW
      REAL CLASS(7)

      REAL, DIMENSION(NCOHORTS) :: DMC, DryPodWt, FreshPodWt, PHTIM
      REAL, DIMENSION(NCOHORTS) :: SDNO, SHELN, WTSD, WTSHE, XPAGE
      
      REAL :: TWTSH,HRPN
      REAL :: HRSN, HRDSD, HRDSH 
      REAL :: TOSDN,TOWSD,TOSHN,TOWSH,TOPOW,TOFPW
      REAL :: MTFPW,MTDPW,MTDSD,MTDSH,MFNUM
      REAL :: RTFPW,HPODWT,HSDWT,HSHELWT,HFPOW
      REAL :: HRVD, HRVF
      REAL :: CHPDT,CHFPW

!     In-season harvest weight CHP 2024-06-27
      REAL ISH_wt

      LOGICAL FEXIST

      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH
      CALL GET(CONTROL)

      YRDOY = CONTROL % YRDOY
!***********************************************************************
!***********************************************************************
!     Run initialization - run once per simulation
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
!-----------------------------------------------------------------------
        IF (INDEX('Y',ISWFWT) < 1 .OR. 
     &    INDEX('N,0',ISWITCH%IDETL) > 0) RETURN
        
        FWFile  = 'FreshWt.OUT '
        CALL GETLUN('FWOUT',  NOUTPF)

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
        CALL GET(ISWITCH)
        
!     Switch for fresh weight calculations
        IF (INDEX('Y',ISWFWT) < 1 .OR. 
     &    INDEX('N,0',ISWITCH%IDETL) > 0) RETURN

        CROP   = CONTROL%CROP

!     Currently only works for tomato, green bean, bell pepper, strawberry,
!         and cucumber. Add other crops later. 
!     Send a message if not available crop
        IF (INDEX('CU,GB,PR,SR,TM',CROP) < 0) THEN
          CALL GET_CROPD(CROP, CROPD)
          WRITE(MSG(1),'(A)') 
     &  "Fresh weight calculations not currently available for "
          WRITE(MSG(2),'(A2,1X,A16)') CROP, CROPD
          CALL INFO(2,ERRKEY,MSG)
        ENDIF

        IF (INDEX('CU,GB,PR,SR,TM',CROP) .GT. 0 
     &    .AND. XMAGE .LT. 1.0) THEN
          CALL GET_CROPD(CROP, CROPD)
          MSG(1) = 'Please check the value of XMAGE in Ecotype file.'
          MSG(2) = 'XMAGE cannot be lower than 1.0.'
          WRITE(MSG(3),'("XMAGE = ",F8.2)') XMAGE
          WRITE(MSG(4),'(A2,1X,A16)') CROP, CROPD
          CALL WARNING(4, ERRKEY, MSG)
          CALL ERROR (ERRKEY,1,'',0)
        ENDIF     
!::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        INQUIRE (FILE= FWFile, EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN(UNIT = NOUTPF, FILE = FWFile, STATUS = 'OLD',
     &    IOSTAT = ERRNUM, POSITION = 'APPEND')
        ELSE
          OPEN (UNIT = NOUTPF, FILE = FWFile, STATUS = 'NEW',
     &    IOSTAT = ERRNUM)
          WRITE(NOUTPF,'("*Fresh Weight Output File")')
          
!         2023-01-13 FO - Added Header variables description
!         Note: Please follow the standard code. Comment of the section
!         in the first line. Next line '!'variable name separed by two 
!         spaces the description and units in parenthesis.
          WRITE(NOUTPF,'(A)') '',
     &    '!----------------------------',
     &    '! Variable Descriptions (unit)',
     &    '!----------------------------',
     &    '!YEAR  Year of current date of simulation',
     &    '!DOY  Day of year (d)',
     &    '!DAS  Days after start of simulation (d)',
     &    '!PDMCD  Dry matter con. of harvested product (fraction)',
     &    '!AFPWD  Average fresh fruit (pod, ear) weight (g/fruit)',
     &    '!ADPWD  Average dry fruit (pod, ear) weight (g/fruit)',
     &    '!PAGED  Age of oldest pod or ear (days)'
          SELECT CASE (CROP)
            CASE ('GB')       ! Snap bean
              WRITE(NOUTPF,'(A)')
     &    '!FCULD  Fresh weight for cull fruit/pods (kg/ha)',
     &    '!FSZ1D  Fresh weight for size class 1 (kg/ha)',
     &    '!FSZ2D  Fresh weight for size class 2 (kg/ha)',
     &    '!FSZ3D  Fresh weight for size class 3 (kg/ha)',
     &    '!FSZ4D  Fresh weight for size class 4 (kg/ha)',
     &    '!FSZ5D  Fresh weight for size class 5 (kg/ha)',
     &    '!FSZ6D  Fresh weight for size class 6 (kg/ha)'
          END SELECT
          WRITE(NOUTPF,'(A)')
     &    '!XMAGE  Required pod age for Multi-Harvest (days)',
     &    '!TOSDN  Total Number of seeds (#/ha)',
     &    '!TOWSD  Total Seed mass (kg/ha)',
     &    '!TOSHN  Total Number of shells (#/ha)',
     &    '!TOWSH  Total Shell mass (kg/ha)',
     &    '!TOPOW  Total pod weight (kg/ha)',
     &    '!TOFPW  Total pod (ear) fresh weight (kg/ha)',
     &    '!MTFPW  Fresh weight of mature fruits (kg/ha)',
     &    '!MTDPW  Dry weight of mature fruits (sd and sh)(kg/ha)',
     &    '!MTDSD  Seed mass of mature fruits (kg/ha)',
     &    '!MTDSH  Shell mass of mature fruits (kg/ha)',
     &    '!HSHELWT  Harvested shell weight (kg/ha)',
     &    '!HSDWT  Harvested seed weight (kg/ha)',
     &    '!HPODWT  Harvested pod weight (kg/ha)',
     &    '!HFPOW  Harvested fresh pod weight (kg/ha)',
     &    '!CPODN  Cum. pod weight (kg/ha)',
     &    '!CMFNM  Cum. mature fruit number (#)',
     &    '!CHPDT  Cum. har. pod weight of mat. fruits (kg/ha)',
     &    '!CHFPW  Cum. har. fresh weight of mat. fruits (kg/ha)',
     &    '!CHNUM  Cum. harvest number (#)'
        ENDIF

        CALL HEADER(SEASINIT, NOUTPF, CONTROL%RUN)

     
!     Change header to PWAD1 (was PWAD) because GBuild requires 
!     unique headers (PlantGro also lists PWAD).  Should have same
!     value, but slightly off. Why?

!     Need to look at how GBuild handles P#AD and SH%D here, too.

        SELECT CASE (CROP)
          CASE ('CU')       ! Cucumber
            WRITE (NOUTPF,230)
          CASE ('GB')       ! Snap bean
            WRITE (NOUTPF,231)
          CASE ('PR')       ! Bell pepper
            WRITE (NOUTPF,230)            
          CASE ('SR')       ! Strawberry
            WRITE (NOUTPF,230)                        
          CASE ('TM')       ! Tomato
            WRITE (NOUTPF,230)
          CASE DEFAULT
            WRITE (NOUTPF,230)
        END SELECT
        
  230 FORMAT('@YEAR DOY   DAS   DAP',
     &    '    PDMCD    AFPWD',
     &    '    ADPWD    PAGED',
     &    '    XMAGE    CHNUM',   
     &    '    TOSHN    TOWSH    MTDSH    HSHEL',
     &    '    TOPOW    HPODW    CHPDT    CPODN',
     &    '    TOFPW    MTFPW    MTDPW    HFPOW    CHFPW    CMFNM',
     &    '    TOSDN    TOWSD    MTDSD    HSDWT')
     
  231 FORMAT('@YEAR DOY   DAS   DAP',
     &    '    PDMCD    AFPWD',
     &    '    ADPWD    PAGED',
     &    ' FCULD FSZ1D FSZ2D FSZ3D FSZ4D FSZ5D FSZ6D',
     &    '    XMAGE    CHNUM',   
     &    '    TOSHN    TOWSH    MTDSH    HSHEL',
     &    '    TOPOW    HPODW    CHPDT    CPODN',
     &    '    TOFPW    MTFPW    MTDPW    HFPOW    CHFPW    CMFNM',
     &    '    TOSDN    TOWSD    MTDSD    HSDWT')

        AvgDMC  = 0.0
        AvgDPW  = 0.0
        AvgFPW  = 0.0
        PodAge  = 0.0
        TDPW    = 0.0
        TFPW    = 0.0
        
        TWTSH   = 0.0
        HSDWT   = 0.0 
        HSHELWT = 0.0
        RTFPW   = 0.0
        HPODWT  = 0.0
        HFPOW   = 0.0

        CHPDT   = 0.0
        CHFPW   = 0.0
        CMFNM   = 0
        CHNUM   = 0
        CPODN   = 0

        HRVD    = 0.0
        HRVF    = 0.0
        HRSN    = 0.0
        HRPN    = 0.0
        HRDSD   = 0.0
        HRDSH   = 0.0      

        CALL PUT('MHARVEST','ISH_date',-99)
        CALL PUT('MHARVEST','ISH_wt',  -99.)

!***********************************************************************
!***********************************************************************
!     DAILY RATE/INTEGRATION
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------
!     chp 2024-12-11 Need to continue with calculations for multiple harvests
!                     regardless of IDETL value.
!       IF (INDEX('Y',ISWFWT) < 1 .OR. 
!    &    INDEX('N,0',ISWITCH%IDETL) > 0) RETURN
        IF (INDEX('Y',ISWFWT) < 1)  RETURN

        ! Total values
        TOSDN   = 0.0      
        TOWSD   = 0.0      
        TOSHN   = 0.0      
        TOWSH   = 0.0
        TOPOW   = 0.0
        TOFPW   = 0.0
        
        ! Mature in the basket
        MTFPW   = 0.0
        MTDPW   = 0.0
        MTDSD   = 0.0
        MTDSH   = 0.0
        MFNUM   = 0.0  
        
        ! Ready to harvest
        HSDWT   = 0.0 
        HSHELWT = 0.0
        HPODWT  = 0.0
        HFPOW   = 0.0
        HARVF   = 0
        CALL GET('MHARVEST','HARVF',HARVF)   
           
        DO I = 1, 7
          CLASS(I) = 0.0
        ENDDO
!-----------------------------------------------------------------------
        DO NPP = 1, NR2TIM + 1
          PAGE = PHTIM(NR2TIM + 1) - PHTIM(NPP)
          XPAGE(NPP) = PAGE
            
!       Dry matter concentration (fraction)
          SELECT CASE (CROP)
            CASE ('CU')       ! Cucumber
              DMC(NPP) = (5. + 7.2 * EXP(-7.5 * PAGE / 40.)) / 100.
            CASE ('GB')       ! Snap bean
    !         DMC(NPP) = 0.0465 + 0.0116 * EXP(0.161 * PAGE)
              DMC(NPP) = 0.023 + 0.0277 * EXP(0.116 * PAGE)  
            CASE ('PR')       ! Bell pepper
              DMC(NPP) = (5. + 7.2 * EXP(-7.5 * PAGE / 40.)) / 100.
            CASE ('SR')       ! Strawberry
              !Fixed value for Strawberry. 
              !From Code from Ken Boote / VSH
              DMC(NPP) = 0.16 
            CASE ('TM')       ! Tomato
              DMC(NPP) = (5. + 7.2 * EXP(-7.5 * PAGE / 40.)) / 100.
            CASE DEFAULT
              DMC(NPP) = (5. + 7.2 * EXP(-7.5 * PAGE / 40.)) / 100.
          END SELECT

!         Fresh weight (g/pod)
          IF (SHELN(NPP) > 0.0) THEN
            FreshPodWt(NPP) = (WTSD(NPP) + WTSHE(NPP)) / DMC(NPP) /
     &                          SHELN(NPP)  !g/pod
            DryPodWt(NPP) = (WTSD(NPP) + WTSHE(NPP))/SHELN(NPP) !g/pod
          ELSE
            FreshPodWt(NPP) = 0.0
          ENDIF

!         Snap bean quality
          IF (CROP .EQ. 'GB') THEN
!         PodDiam = mm/pod; PodLen = cm/pod
            PodDiam = 8.991 *(1.0-EXP(-0.438*(FreshPodWt(NPP)+0.5))) 
            PodLen  = 14.24 *(1.0-EXP(-0.634*(FreshPodWt(NPP)+0.46)))

            IF (PodDiam .LT. 4.7625) THEN
!           Culls
              CLASS(7) = CLASS(7) + (WTSD(NPP) + WTSHE(NPP)) / DMC(NPP) 
            ELSEIF (PodDiam .LT. 5.7547) THEN
!           Sieve size 1
              CLASS(1) = CLASS(1) + (WTSD(NPP) + WTSHE(NPP)) / DMC(NPP) 
            ELSEIF (PodDiam .LT. 7.3422) THEN
!           Sieve size 2
              CLASS(2) = CLASS(2) + (WTSD(NPP) + WTSHE(NPP)) / DMC(NPP)
            ELSEIF (PodDiam .LT. 8.3344) THEN
!           Sieve size 3
              CLASS(3) = CLASS(3) + (WTSD(NPP) + WTSHE(NPP)) / DMC(NPP)
            ELSEIF (PodDiam .LT. 9.5250) THEN
!           Sieve size 4
              CLASS(4) = CLASS(4) + (WTSD(NPP) + WTSHE(NPP)) / DMC(NPP)
            ELSEIF (PodDiam .LT. 10.7156) THEN
!           Sieve size 5
              CLASS(5) = CLASS(5) + (WTSD(NPP) + WTSHE(NPP)) / DMC(NPP)
            ELSE
!           Sieve size 6
              CLASS(6) = CLASS(6) + (WTSD(NPP) + WTSHE(NPP)) / DMC(NPP)
            ENDIF
          ENDIF

          !Total Seed number
          TOSDN = TOSDN + SDNO(NPP)
          
          !Total weight seed
          TOWSD = TOWSD + WTSD(NPP)
          
          !Total shell number
          TOSHN = TOSHN + SHELN(NPP)
          
          !Total weight shell
          TOWSH = TOWSH + WTSHE(NPP)
          
          !Total  Pod weight
          TOPOW = TOPOW + WTSD(NPP) + WTSHE(NPP)
          
          !Total Fresh Pod weight
          TOFPW = TOFPW + (WTSD(NPP) + WTSHE(NPP)) / DMC(NPP)
        
          ! Accumulating in the basket for harvesting (MultiHarvest)
          IF (page >= XMAGE) THEN
             !Fresh weight of mature fruits
             MTFPW = MTFPW + (WTSD(NPP) + WTSHE(NPP)) / DMC(NPP)
             !Dry weight of mature fruits (seed and shell)
             MTDPW = MTDPW + WTSD(NPP) + WTSHE(NPP)
             !Seed mass of mature fruits - wtsd 
             ! = seed mass for cohort
             MTDSD = MTDSD + WTSD(NPP)
             !Shell mass of mature fruits - wtshe 
             ! = shell mass for cohort
             MTDSH = MTDSH + WTSHE(NPP)
             !Number of mature fruits
             MFNUM = MFNUM + SHELN(NPP)
          ENDIF
          
          ! Apply Harvest
          IF(HARVF == 1 .AND. page >= XMAGE) THEN
            HSHELWT = MTDSH 
            HSDWT   = MTDSD
            HPODWT  = MTDPW
            HFPOW   = MTFPW
            SHELN(NPP) = 0.0
            WTSHE(NPP) = 0.0
            WTSD(NPP)  = 0.0
            SDNO(NPP)  = 0.0
            ISH_wt = HSHELWT + HSDWT + HPODWT + HFPOW
            CALL PUT('MHARVEST','ISH_date',YRDOY)
            CALL PUT('MHARVEST','ISH_wt',  ISH_wt)
          ENDIF
        ENDDO  ! NPP

!       Prepare model outputs
        PodAge = XPAGE(1)
        ! Avg. total shell number and pod weight
        IF (TOSHN > 0.0) THEN
          AvgFPW = TOFPW / TOSHN
          AvgDPW = TOPOW / TOSHN
          CPODN  = CPODN + NINT(TOPOW)
          CMFNM  = CMFNM + NINT(MFNUM)
        ELSE
          AvgFPW = 0.0
          AvgDPW = 0.0
        ENDIF
        ! Avg. Dry matter content
        IF (TOFPW > 0.0) THEN
          AvgDMC = TOPOW / TOFPW
        ELSE
          AvgDMC = 0.0
        ENDIF
        ! Cumulative har. amounts
        IF(HARVF .EQ. 1) THEN
          CHPDT = CHPDT + HPODWT
          CHFPW = CHFPW + MTFPW
          CHNUM = CHNUM + 1
        ENDIF
        
!***********************************************************************
!***********************************************************************
!     DAILY OUTPUT
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. OUTPUT) THEN
!-----------------------------------------------------------------------
        IF (INDEX('Y',ISWFWT) < 1 .OR. 
     &    INDEX('N,0',ISWITCH%IDETL) > 0) RETURN
     
        YRDOY = CONTROL % YRDOY
        IF (YRDOY .LT. YRPLT .OR. YRPLT .LT. 0) RETURN

        DAS = CONTROL % DAS

!       Daily output every FROP days
        IF (MOD(DAS,CONTROL%FROP) == 0) THEN  

        CALL YR_DOY(YRDOY, YEAR, DOY) 
        DAP = MAX(0,TIMDIF(YRPLT,YRDOY))
        IF (DAP > DAS) DAP = 0

        SELECT CASE (CROP)
         CASE ('CU')        ! Cucumber
            WRITE(NOUTPF, 1000) YEAR, DOY, DAS, DAP, 
     &      AvgDMC, AvgFPW, AvgDPW, 
     &      PodAge,
     &      XMAGE, CHNUM,
     &      TOSHN,TOWSH*10.,MTDSH*10.,HSHELWT*10.,
     &      TOPOW*10.,HPODWT*10.,CHPDT*10.,CPODN*10,
     &      NINT(TOFPW*10.),MTFPW*10.,MTDPW*10.,
     &      HFPOW*10.,CHFPW*10.,CMFNM,
     &      TOSDN,TOWSD*10.,MTDSD*10.,HSDWT*10.
          CASE ('GB')       ! Snap bean
            WRITE(NOUTPF, 2000) YEAR, DOY, DAS, DAP, 
     &      AvgDMC, AvgFPW, AvgDPW, 
     &      PodAge,NINT(CLASS(7)),NINT(CLASS(1)),
     &      NINT(CLASS(2)),NINT(CLASS(3)),NINT(CLASS(4)),
     &      NINT(CLASS(5)),NINT(CLASS(6)),
     &      XMAGE, CHNUM,
     &      TOSHN,TOWSH*10.,MTDSH*10.,HSHELWT*10.,
     &      TOPOW*10.,HPODWT*10.,CHPDT*10.,CPODN*10,
     &      NINT(TOFPW*10.),MTFPW*10.,MTDPW*10.,
     &      HFPOW*10.,CHFPW*10.,CMFNM,
     &      TOSDN,TOWSD*10.,MTDSD*10.,HSDWT*10.
         CASE ('PR')        ! Bell pepper
            WRITE(NOUTPF, 1000) YEAR, DOY, DAS, DAP, 
     &      AvgDMC, AvgFPW, AvgDPW, 
     &      PodAge,
     &      XMAGE, CHNUM,
     &      TOSHN,TOWSH*10.,MTDSH*10.,HSHELWT*10.,
     &      TOPOW*10.,HPODWT*10.,CHPDT*10.,CPODN*10,
     &      NINT(TOFPW*10.),MTFPW*10.,MTDPW*10.,
     &      HFPOW*10.,CHFPW*10.,CMFNM,
     &      TOSDN,TOWSD*10.,MTDSD*10.,HSDWT*10.
         CASE ('SR')        ! Strawberry
            WRITE(NOUTPF, 1000) YEAR, DOY, DAS, DAP, 
     &      AvgDMC, AvgFPW, AvgDPW, 
     &      PodAge,
     &      XMAGE, CHNUM,
     &      TOSHN,TOWSH*10.,MTDSH*10.,HSHELWT*10.,
     &      TOPOW*10.,HPODWT*10.,CHPDT*10.,CPODN*10,
     &      NINT(TOFPW*10.),MTFPW*10.,MTDPW*10.,
     &      HFPOW*10.,CHFPW*10.,CMFNM,
     &      TOSDN,TOWSD*10.,MTDSD*10.,HSDWT*10.
          CASE ('TM')       ! Tomato
            WRITE(NOUTPF, 1000) YEAR, DOY, DAS, DAP, 
     &      AvgDMC, AvgFPW, AvgDPW, 
     &      PodAge,
     &      XMAGE, CHNUM,
     &      TOSHN,TOWSH*10.,MTDSH*10.,HSHELWT*10.,
     &      TOPOW*10.,HPODWT*10.,CHPDT*10.,CPODN*10,
     &      NINT(TOFPW*10.),MTFPW*10.,MTDPW*10.,
     &      HFPOW*10.,CHFPW*10.,CMFNM,
     &      TOSDN,TOWSD*10.,MTDSD*10.,HSDWT*10.
          CASE DEFAULT
            WRITE(NOUTPF, 1000) YEAR, DOY, DAS, DAP, 
     &      AvgDMC, AvgFPW, AvgDPW, 
     &      PodAge,
     &      XMAGE, CHNUM,
     &      TOSHN,TOWSH*10.,MTDSH*10.,HSHELWT*10.,
     &      TOPOW*10.,HPODWT*10.,CHPDT*10.,CPODN*10,
     &      NINT(TOFPW*10.),MTFPW*10.,MTDPW*10.,
     &      HFPOW*10.,CHFPW*10.,CMFNM,
     &      TOSDN,TOWSD*10.,MTDSD*10.,HSDWT*10.
        END SELECT

 1000   FORMAT(1X,I4,1X,I3.3,2(1X,I5),
     &    F9.1,F9.1,F9.1,F9.1,
     &    F9.1, I9,
     &    4(F9.1),
     &    3(F9.1),I9,
     &    I9,4(F9.1),I9,
     &    4(F9.1))
 2000   FORMAT(1X,I4,1X,I3.3,2(1X,I5),
     &    F9.1,F9.1,F9.1,F9.1,
     &    7(1X,I5),
     &    F9.1, I9,
     &    4(F9.1),
     &    3(F9.1),I9,
     &    I9,4(F9.1),I9,
     &    4(F9.1))

      ENDIF

!***********************************************************************
!***********************************************************************
!     SEASONAL SUMMARY
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. SEASEND) THEN
!-----------------------------------------------------------------------

        CLOSE (NOUTPF)

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE FRESHWT
!=======================================================================

