!=======================================================================
C  MODULE GHG_mod
C  06/15/2014 CHP Written
!  10/01/2021 CHP Remove CO2 outputs from here. These are now reported
!                 by the methane module.
!  01/26/2023 CHP Reduce compile warnings: add EXTERNAL stmts, remove 
!                 unused variables, shorten lines. 
!=======================================================================

      MODULE GHG_mod
!     Contains data definitions for N2O generation routines in SOILNI
      USE ModuleDefs

!     Data construct for control variables
      TYPE N2O_type
!            Daily        Cumul        Layer         
        REAL TNOXD,       CNOX,        DENITRIF(NL)  ![N] Denitrified
!       REAL TN2OdenitD,  CN2Odenit,   N2Odenit(NL)  !N2O[N] from denit
        REAL TN2OdenitD,  CN2Odenit,   N2Odenit(NL)  
        REAL TN2OnitrifD, CN2Onitrif,  N2ONitrif(NL) !N2O[N] from nitr

        REAL TN2D,        CN2,         N2flux(NL)    !N2[N] from denit
!                                      N2Oflux = N2Odenit + N2ONitrif
        REAL                           N2OFLUX(NL)   
        REAL TNOfluxD,    CNOflux,     NOflux(NL)    !NO flux from nitr

        REAL TNITRIFY,    CNITRIFY,    NITRIF(NL)    ![N] Nitrified 

        REAL N2_emitted,  CN2_emitted                !N2[N] emitted
        REAL N2O_emitted, CN2O_emitted               !N2O[N] emitted
        REAL NO_emitted,  CNO_emitted                !NO[N] emitted

        REAL TNGSoil  !N2, N2O, and NO in soil

        REAL, DIMENSION(NL) :: WFPS 
      END TYPE N2O_type

      TYPE CH4_type
        REAL CH4Consumption, CH4Emission, CH4Leaching, CH4Stored
        REAL CumCH4Consumpt, CumCH4Emission, CumCH4Leaching
        REAL CO2emission, CumCO2Emission                      
      END TYPE CH4_type

      CONTAINS

C=======================================================================
C  N2Oemit, Subroutine, P. Grace
C  Computes N2, N2O daily and cumulative emissions
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  09/18/2015 CHP Written, based on PG code.
!  05/30/2016 CHP remove diffusion model
!=======================================================================

      SUBROUTINE N2Oemit(CONTROL, ISWITCH, dD0, SOILPROP, N2O_DATA) 
!-------------------------------------------------------------------
      IMPLICIT NONE
      SAVE
!-----------------------------------------------------------------------

      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH
      TYPE (SoilType)    SOILPROP
      TYPE (N2O_type)    N2O_DATA

!     CHARACTER*1  IDETL  
      CHARACTER*1  IDETN, ISWNIT, ISWWAT  
      CHARACTER*10, PARAMETER :: OUTGHG = 'N2O.OUT'

      INTEGER DAS, DYNAMIC, L, NLAYR, YRDOY

      REAL N2flux(NL)                !N2
      real N2Oflux(nl), NOflux(nl)  
      REAL N2Odenit(NL)              !N2O from denitrification
      REAL N2ONitrif(NL)             !N2O from nitrification
      real wfps(nl)                  ! PG   

!          Daily        Cumul        
      REAL N2_emitted,  CN2_emitted  !N2 emitted
      REAL N2O_emitted, CN2O_emitted !N2O emitted
      REAL NO_emitted,  CNO_emitted  !NO emitted

!LOCAL VARIABLES
      real n2o_soil(nl), n2o_diffused ! PG
      real n2_soil(nl),  n2_diffused  ! PG
      real no_soil(nl),  no_diffused  ! chp
      real RateDiffus, TNGSoil
      real dD0(nl)

!-----------------------------------------------------------------------
!     Transfer values from constructed data types into local variables.
      DYNAMIC = CONTROL % DYNAMIC

      DAS     = CONTROL % DAS
      YRDOY   = CONTROL % YRDOY

      N2Odenit = N2O_data % N2Odenit  
      N2Onitrif= N2O_data % N2ONitrif  
      N2flux   = N2O_data % N2flux   
      NOflux   = N2O_data % NOflux   
      WFPS     = N2O_data % WFPS  

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
C-----------------------------------------------------------------------
      IDETN  = ISWITCH % IDETN
      ISWWAT = ISWITCH % ISWWAT
      ISWNIT = ISWITCH % ISWNIT

      N2_emitted   = 0.0
      N2O_emitted  = 0.0
      NO_emitted   = 0.0
      CN2_emitted  = 0.0
      CN2O_emitted = 0.0
      CNO_emitted  = 0.0

!     For sequenced runs, only read values for RUN 1
      IF (INDEX('QF',CONTROL%RNMODE) .LE. 0 .OR. CONTROL%RUN .EQ. 1)THEN
        NLAYR   = SOILPROP % NLAYR
        n2o_soil = 0.0
        no_soil  = 0.0
        n2_soil  = 0.0
        TNGsoil = 0.0
      endif
      
!***********************************************************************
!***********************************************************************
!     DAILY RATES
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. RATE) THEN
      IF (ISWWAT == 'N' .OR. ISWNIT == 'N') RETURN

C-----------------------------------------------------------------------
! 05/20/2016 - chp remove diffusion model
! Simple representation of diffusion of N2O and N2 emissions 14 June 2015
! Developed as N2O and duplicated as N2
! Diffusion of N2O from layers based on WFPS PGrace 14 June 2015
! N2O produced on any day in any layer and diffused upwards is directly proportional to (1-WFPS) (fraction)
! N2O not diffused from layer (WFPS) is added to the next day's total n2odenit for that layer
! N2O emitted to the atmosphere on any day is from layer 1 only.
! n2odenit is the mass of N2O (kg N/ha) produced in a layer on any day due to denitrification
! n2o_diffused is mass diffused (kg N/ha) per layer
! n2o_soil is mass remaining in soil (kg N/ha) AFTER diffusion
! n2o emitted (output as g N/ha in N2O.OUT) is total emission from layer 1 on any day

!     For N mass balance, account for N gas in soil
      TNGsoil  = 0.0

      DO L = NLAYR, 1, -1
          RateDiffus = dD0(L) !PG031017

!         Update soil state variables based on new N2 and N2O today (flux)
          n2oflux(L) = max(0.0, N2ONitrif(L) + n2odenit(L)) 
          n2flux(L)  = max(0.0, n2flux(L))
          noflux(L)  = max(0.0, noflux(L))

          n2o_soil(L) = n2o_soil(L) + n2oflux(L)           
          n2_soil(L)  = n2_soil(L)  + n2flux(L)
          no_soil(L)  = no_soil(L)  + noflux(L)

          n2o_diffused = n2o_soil(L) * RateDiffus
          n2_diffused  = n2_soil(L)  * RateDiffus
          no_diffused  = no_soil(L)  * RateDiffus
    
          if (L == 1) then   !LAYER ONE
            n2o_emitted = n2o_diffused
            n2_emitted  = n2_diffused
            no_emitted  = no_diffused
          else
            n2o_soil(L-1) = n2o_soil(L-1) + n2o_diffused
!            if (L == 2) n2o_in1 = n2o_diffused
            n2_soil(L-1) = n2_soil(L-1) + n2_diffused
            no_soil(L-1) = no_soil(L-1) + no_diffused
          endif

          n2o_soil(L) = n2o_soil(L) - n2o_diffused
          n2_soil(L)  = n2_soil(L)  - n2_diffused
          no_soil(L)  = no_soil(L)  - no_diffused

          TNGsoil  = TNGsoil  + n2o_soil(L) + n2_soil(L) + no_soil(L)
      ENDDO
      
      CN2O_emitted = CN2O_emitted + N2O_emitted
      CN2_emitted  = CN2_emitted  + N2_emitted
      CNO_emitted  = CNO_emitted  + NO_emitted

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      N2O_data % n2o_emitted  = n2o_emitted
      N2O_data % n2_emitted   = n2_emitted
      N2O_data % no_emitted   = no_emitted
      N2O_data % CN2O_emitted = CN2O_emitted
      N2O_data % CN2_emitted  = CN2_emitted
      N2O_data % CNO_emitted  = CNO_emitted
      N2O_data % N2Oflux      = N2Oflux
      N2O_data % N2flux       = N2flux
      N2O_data % NOflux       = NOflux
      N2O_data % TNGSoil      = TNGSoil

      RETURN
      END SUBROUTINE N2Oemit
C-------------------------------------------------------------------
C
!======================================================================


!=======================================================================
! Denit_DayCent Variables 
!-----------------------------------------------------------------------
! min_nitrate     = minimum nitrate concentration required in a soil layer 
!                   for trace gas calclution (ppm N)
!***********************************************************************


C=======================================================================
C  OpN2O, Subroutine, C.H.Porter, P. Grace
C  Generates output for daily soil N2O routines
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  06/15/2014 CHP Written
C  05/01/2022 FO  Added N2O.csv output
!=======================================================================

      SUBROUTINE OpN2O(CONTROL, ISWITCH, SOILPROP, N2O_DATA) 
!-------------------------------------------------------------------
      USE CsvOutput 
      USE Linklist
      IMPLICIT NONE
      EXTERNAL GETLUN, HEADER, YR_DOY, SUMVALS
      SAVE
!-----------------------------------------------------------------------

      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH
      TYPE (SoilType)    SOILPROP
      TYPE (N2O_type)    N2O_DATA

      CHARACTER*1  IDETN, ISWNIT, ISWWAT, RNMODE
      CHARACTER*10, PARAMETER :: OUTGHG = 'N2O.OUT'
      CHARACTER*500 FRMT, FRMT2

      INTEGER DAS, DOY, DYNAMIC, ERRNUM, FROP, I, L, REPNO
      INTEGER N_LYR, GHGLUN, RUN, YEAR, YRDOY, SPACES

!          Cumul      Daily     Layer         
      REAL CNOX,      TNOXD,    DENITRIF(NL)  !Denitrification
      REAL CNITRIFY,  TNITRIFY, NITRIF(NL)    !Nitrification 
      REAL CN2,       TN2D,     N2flux(NL)    !N2
!     REAL CN2O,      TN2OD,    N2Oflux(NL)   
!                           N2O total (nitrification + denitrification)
      REAL                      N2Oflux(NL)   
      REAL CNOflux,   TNOfluxD, NOflux(NL)    !NO total flux
!     Added N2Odenit for N2O from denitrification only and daily and cumulative variables
      REAL CN2Odenit, TN2OdenitD, N2Odenit(NL)   
!     Daily total and cumulative totals for N2ONitrif
      REAL CN2Onitrif, TN2OnitrifD, N2ONitrif(NL)     

      REAL n2o_emitted, n2_emitted, CN2O_emitted, CN2_emitted  
      REAL no_emitted, CNO_emitted  
      
!     Convert Variables
      REAL cvN2O_emitted, cvN2_emitted, cvNO_emitted, cvTNOXD
      INTEGER cvTNITRIFY
      REAL cvTN2OdenitD, cvTN2OnitrifD, cvTN2D, cvTNOfluxD
      REAL cvDENITRIF(NL), cvNITRIF(NL), cvN2Oflux(NL) 
      REAL cvN2flux(NL), cvNOflux(NL)  
     
!     REAL TOTCO2, CumTotCO2  !NDN20, NIT20, N2O20, N2F20, NOF20, 
!     REAL newCO2(0:NL)
      real wfps(nl)         ! PG   !bd(nl),sw(nl),poros(nl)

      LOGICAL FEXIST

!     Arrays which contain data for printing in SUMMARY.OUT file
      INTEGER, PARAMETER :: SUMNUM = 2
      CHARACTER*5, DIMENSION(SUMNUM) :: LABEL
      REAL, DIMENSION(SUMNUM) :: VALUE

      FMOPT  = ISWITCH % FMOPT

!-----------------------------------------------------------------------
!     Transfer values from constructed data types into local variables.
      DYNAMIC = CONTROL % DYNAMIC

      IDETN  = ISWITCH % IDETN
      ISWWAT = ISWITCH % ISWWAT
      ISWNIT = ISWITCH % ISWNIT

      IF (ISWWAT == 'N' .OR. ISWNIT == 'N') RETURN

      DAS     = CONTROL % DAS
      YRDOY   = CONTROL % YRDOY

      CNOX     = N2O_data % CNOX      
      TNOXD    = N2O_data % TNOXD     
      DENITRIF = N2O_data % DENITRIF   
      CN2      = N2O_data % CN2      
      TN2D     = N2O_data % TN2D
      N2Oflux  = N2O_data % N2Oflux 
      CNOflux  = N2O_data % CNOflux    
      TNOfluxD = N2O_data % TNOfluxD    
      N2flux   = N2O_data % n2flux   
      NOflux   = N2O_data % noflux   

!     added n2odenit and daily and cumulative variables      
      Cn2odenit = N2O_data % Cn2odenit
      Tn2odenitd= N2O_data % Tn2odenitd
      n2odenit  = N2O_data % n2odenit  
      CNITRIFY = N2O_data % CNITRIFY     
      TNITRIFY = N2O_data % TNITRIFY 
      NITRIF   = N2O_data % NITRIF  
      WFPS     = N2O_data % WFPS  

!     add daily total and cumulative N2ONitrif variables
      CN2ONITRIF = N2O_data % CN2ONITRIF
      TN2ONITRIFD= N2O_data % TN2ONITRIFD
      N2ONitrif  = N2O_data % N2ONitrif
      
      N_LYR = MIN(10, MAX(4,SOILPROP%NLAYR))
!***********************************************************************
!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
C-----------------------------------------------------------------------
C     Variable heading for N2O.OUT
C-----------------------------------------------------------------------
!     CumTotCO2  = 0.0
      CNOX       = -99.0      
      TNOXD      = -99.0  
      DENITRIF   = -99.0   
      CN2        = -99.0 
      TN2D       = -99.0
      N2Oflux    = -99.0
      CNOflux    = -99.0   
      TNOfluxD   = -99.0    
      N2flux     = -99.0 
      NOflux     = -99.0 
      Cn2odenit  = -99.0
      Tn2odenitd = -99.0
      n2odenit   = -99.0
      CNITRIFY   = -99.0
      TNITRIFY   = -99.0
      NITRIF     = -99.0  
      WFPS       = -99.0  
      CN2ONITRIF = -99.0
      TN2ONITRIFD= -99.0
      N2ONitrif  = -99.0

!     chp 10/20/2017. At FAO request. Temporarily hide N2O output
!     No output unless detail switch is on.
!For N2O_out branch, switch output back on.
!      IDETL = ISWITCH % IDETL
!      IF (INDEX('AD',IDETL) == 0) RETURN

      IF (IDETN .EQ. 'Y') THEN

        FROP    = CONTROL % FROP
        RNMODE  = CONTROL % RNMODE
        REPNO   = CONTROL % REPNO
        RUN     = CONTROL % RUN

      IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN
        CALL GETLUN(OUTGHG, GHGLUN)
        INQUIRE (FILE = OUTGHG, EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT = GHGLUN, FILE = OUTGHG, STATUS = 'OLD',
     &      IOSTAT = ERRNUM, POSITION = 'APPEND')
        ELSE
          OPEN (UNIT = GHGLUN, FILE = OUTGHG, STATUS = 'NEW',
     &      IOSTAT = ERRNUM)
          WRITE(GHGLUN,'("*N2O emissions output file")')
        ENDIF

        IF (RNMODE .NE. 'Q' .OR. RUN .EQ. 1) THEN
          !For first run of a sequenced run, use replicate
          ! number instead of run number in header.
          IF (RNMODE .EQ. 'Q') THEN
            CALL HEADER(SEASINIT, GHGLUN, REPNO)
          ELSE
            CALL HEADER(SEASINIT, GHGLUN, RUN)
          ENDIF

          WRITE(GHGLUN,'("!",14X,A,A,A)')
     & "|------------------------------ Cumulative -----------",
     & "------------------|------------------------------ Daily --",
     & "--------------------------------|"

          SPACES = 163
          WRITE(FRMT,
     &     '(A,A,A,A,A,A,I3.3,A,A,I3.3,A,A,I3.3,A,A,I3.3,A,A,I3.3,A)')

     & '("!",14X,',
     & '" N2Oemit  N2emit  NOemit   Denit  Nitrif   ",',
     & '"N2O-denit+nit      N2      NO",',
     & '" N2Oemit  N2emit  NOemit   Denit  Nitrif   ",',
     & '"N2O-denit+nit      N2      NO",',
     & 'T',SPACES,
     & ',"Denitrification (g[N]/ha) by soil depth (cm):",',
     & 'T',(SPACES+N_LYR*8),
     & ',"Nitrification (g[N]/ha) by soil depth (cm):",',
     & 'T',(SPACES+2*N_LYR*8),
     & ',"N2O flux (g[N]/ha) by soil depth (cm):",',
     & 'T',(SPACES+3*N_LYR*8),
     & ',"N2 flux (g[N]/ha) by soil depth (cm):",',
     & 'T',(SPACES+4*N_LYR*8),
     & ',"NO flux (g[N]/ha) by soil depth (cm):")'

          WRITE(GHGLUN,TRIM(FRMT))

          WRITE(GHGLUN,'("!",17X,A,A,A,T160,50A8)')
     &"kg/ha   kg/ha   kg/ha   kg/ha   kg/ha   kg/ha   kg/ha  ",
     &" kg/ha   kg/ha    g/ha    g/ha    g/ha    g/ha ",
     &"   g/ha    g/ha    g/ha    g/ha    g/ha",
     &        (SoilProp%LayerText(L),L=1,N_LYR),
     &        (SoilProp%LayerText(L),L=1,N_LYR),
     &        (SoilProp%LayerText(L),L=1,N_LYR),
     &        (SoilProp%LayerText(L),L=1,N_LYR),
     &        (SoilProp%LayerText(L),L=1,N_LYR) 

          WRITE(GHGLUN,"(A)",ADVANCE='NO') 
     & "@YEAR DOY   DAS" //
     & "   N2OEC    N2EC    NOEC    NDNC" // 
     & "    NITC   N2ODC   N2ONC   N2FLC   NOFLC" // 
     & "  N2OGED   N2GED   NOGED    NDND" // 
     & "   NITRD   N2ODD   N2OND   N2FLD   NOFLD"
          IF (N_LYR < 10) THEN
            WRITE (GHGLUN,105)
     &        ('NDN',L,'D',L=1,N_LYR), 
     &        ('NIT',L,'D',L=1,N_LYR),
     &        ('N2O',L,'D',L=1,N_LYR), 
     &        ('N2F',L,'D',L=1,N_LYR), 
     &        ('NOF',L,'D',L=1,N_LYR) 
  105       FORMAT(50("   ",A,I1,A))
          ELSE
            WRITE (GHGLUN,110)
     &        ('NDN',L,'D',L=1,9),'   NDN10', 
     &        ('NIT',L,'D',L=1,9),'   NIT10',
     &        ('N2O',L,'D',L=1,9),'   N2O10', 
     &        ('N2F',L,'D',L=1,9),'   N2F10',
     &        ('NOF',L,'D',L=1,9),'   NOF10'
  110       FORMAT(5(9("   ",A,I1,A),A8))
          ENDIF
        ENDIF
      ENDIF
      
      ENDIF ! Close FMOPT

!     NDN20 = 0.0
!     NIT20 = 0.0
!     N2O20 = 0.0
!     N2F20 = 0.0
!     NOF20 = 0.0

!***********************************************************************
!***********************************************************************
!     DAILY OUTPUT
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. OUTPUT) THEN
C-----------------------------------------------------------------------
!      IF (INDEX('AD',IDETL) == 0) RETURN

!     TOTCO2 = SUM(newCO2)
!     CumTotCO2 = CumTotCO2 + TOTCO2

      n2o_emitted  = N2O_data % n2o_emitted  
      n2_emitted   = N2O_data % n2_emitted   
      no_emitted   = N2O_data % no_emitted   
      CN2O_emitted = N2O_data % CN2O_emitted 
      CN2_emitted  = N2O_data % CN2_emitted  
      CNO_emitted  = N2O_data % CNO_emitted  
    
      IF (IDETN == 'N') RETURN
      IF (MOD(DAS, FROP) .NE. 0) RETURN

      CALL YR_DOY(YRDOY, YEAR, DOY) 
      
      
!     Conver Variables (CV)
      cvN2O_emitted =  N2O_emitted*1000.
      cvN2_emitted = N2_emitted*1000. 
      cvNO_emitted = NO_emitted*1000. 
      cvTNOXD = TNOXD*1000. 
      cvTNITRIFY = NINT(TNITRIFY*1000.)
      cvTN2OdenitD = TN2OdenitD*1000.
      cvTN2OnitrifD = TN2OnitrifD*1000.
      cvTN2D = TN2D*1000.
      cvTNOfluxD = TNOfluxD*1000.
      DO I = 1, N_LYR
        cvDENITRIF(I) = DENITRIF(I)*1000.
        cvNITRIF(I)   = NITRIF(I)*1000.
        cvN2Oflux(I)  = N2Oflux(I)*1000.
        cvN2flux(I)   = N2flux(I)*1000.
        cvNOflux(I)   = NOflux(I)*1000.
      END DO
      
      IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN    ! FO-OPEN FILE

      WRITE(FRMT2,'(A,A,A,I2.2,A,I2.2,A,I2.2,A)') 
     &   '(1X,I4,1X,I3.3,I6,',
     &   '3F8.2,F8.2,F8.1,4F8.3,',
     &   '3F8.1, F8.1,I8,4F8.2,',
     &   N_LYR, 'F8.1,', N_LYR,'F8.0,', 3*N_LYR, 'F8.1)'

        IF (IDETN .EQ. 'Y') THEN
          WRITE (GHGLUN,TRIM(FRMT2)) YEAR, DOY, DAS, 
     &      CN2O_emitted, CN2_emitted, CNO_emitted, 
     &      CNOX, CNITRIFY, CN2Odenit, CN2Onitrif, CN2, CNOflux,
     &      cvN2O_emitted, cvN2_emitted, cvNO_emitted, 
     &      cvTNOXD, cvTNITRIFY, cvTN2OdenitD, 
     &      cvTN2OnitrifD, cvTN2D, cvTNOfluxD, 
     &      (cvDENITRIF(I), i=1,N_LYR), (cvNITRIF(I),I=1,N_LYR),
     &      (cvN2Oflux(i), i=1,n_lyr), (cvN2flux(i),i=1,n_lyr), 
     &      (cvNOflux(i),i=1,n_lyr)
        ENDIF


      ENDIF ! Close FMOPT

C     05/01/2022 FO Added csv output for N2O.csv
!     CSV output corresponding to N2O.csv
      IF (FMOPT == 'C') THEN
          CALL CsvOutN2O(EXPNAME,CONTROL%RUN, CONTROL%TRTNUM,
     &      CONTROL%ROTNUM, CONTROL%REPNO,YEAR, DOY, DAS,
     &      CN2O_emitted, CN2_emitted, CNO_emitted, 
     &      CNOX, CNITRIFY, CN2Odenit, CN2Onitrif, CN2, CNOflux,
     &      cvN2O_emitted, cvN2_emitted, cvNO_emitted, 
     &      cvTNOXD, cvTNITRIFY, cvTN2OdenitD, 
     &      cvTN2OnitrifD, cvTN2D, cvTNOfluxD, 
     &      cvDENITRIF, cvNITRIF,
     &      cvN2Oflux, cvN2flux, 
     &      cvNOflux, N_LYR,
     &      vCsvline, vpCsvline, vlngth)
     
          CALL LinklstN2O(vCsvline)
      END IF      
!***********************************************************************
!***********************************************************************
!     SEASEND
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. SEASEND) THEN
C-----------------------------------------------------------------------
!      IF (INDEX('AD',IDETL) == 0) RETURN
      !Close daily output files.
      CLOSE(GHGLUN)

!     Store Summary.out labels and values in arrays to send to
!     OPSUM routines for printing.  Integers are temporarily 
!     saved as real numbers for placement in real array.
      LABEL(1)  = 'N2OEM'; VALUE(1)  = CN2O_emitted  !kg/ha

!     Send labels and values to OPSUM
      CALL SUMVALS (SUMNUM, LABEL, VALUE) 

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE OpN2O
C-------------------------------------------------------------------
C

C=======================================================================
C  OpGHG, Subroutine, C.H.Porter, P. Grace
C  Generates output for daily soil N2O routines
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  06/15/2014 CHP Written
!  05/10/2024 CHP Added estimates of net CO2 emissions per PG
!=======================================================================

      SUBROUTINE OpGHG(CONTROL, ISWITCH, N2O_data, CH4_data) 
!-------------------------------------------------------------------
      USE ModuleData
      IMPLICIT NONE
      EXTERNAL GETLUN, HEADER, YR_DOY, SUMVALS
      SAVE
!-----------------------------------------------------------------------

      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH
      TYPE (N2O_type)    N2O_DATA
      TYPE (CH4_type)    CH4_DATA

      CHARACTER*1  IDETN, ISWNIT, ISWWAT, RNMODE
      CHARACTER*10, PARAMETER :: OUTGHG = 'GHG.OUT'

      INTEGER DAS, DOY, DYNAMIC, ERRNUM, FROP, REPNO
      INTEGER GHGLUN, RUN, YEAR, YRDOY

      REAL CO2EC, CO2ENC, N2OEC, CH4EC
      REAL CO2GED, CO2END, N2OGED, CH4GED 
      REAL CCEQC, NCEQC, MCEQC, TCEQC
      REAL TSOMC, TSOMC_init, CO2ENC_Y 
      LOGICAL FEXIST

!     Arrays which contain data for printing in SUMMARY.OUT file
      INTEGER, PARAMETER :: SUMNUM = 1  !CO2EM now from GHG module
      CHARACTER*5, DIMENSION(SUMNUM) :: LABEL
      REAL, DIMENSION(SUMNUM) :: VALUE

!-----------------------------------------------------------------------
!     Transfer values from constructed data types into local variables.
      DYNAMIC = CONTROL % DYNAMIC

      IDETN  = ISWITCH % IDETN
      ISWWAT = ISWITCH % ISWWAT
      ISWNIT = ISWITCH % ISWNIT

      IF (ISWWAT == 'N' .OR. ISWNIT == 'N') RETURN

      DAS     = CONTROL % DAS
      YRDOY   = CONTROL % YRDOY

!***********************************************************************
!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
C-----------------------------------------------------------------------
C     Variable heading for GHG.OUT
C-----------------------------------------------------------------------
      IF (IDETN .EQ. 'Y') THEN

        FROP    = CONTROL % FROP
        RNMODE  = CONTROL % RNMODE
        REPNO   = CONTROL % REPNO
        RUN     = CONTROL % RUN

        CALL GETLUN(OUTGHG, GHGLUN)
        INQUIRE (FILE = OUTGHG, EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT = GHGLUN, FILE = OUTGHG, STATUS = 'OLD',
     &      IOSTAT = ERRNUM, POSITION = 'APPEND')
        ELSE
          OPEN (UNIT = GHGLUN, FILE = OUTGHG, STATUS = 'NEW',
     &      IOSTAT = ERRNUM)
          WRITE(GHGLUN,'("*GHG emissions output file")')
        ENDIF

        IF (RNMODE .NE. 'Q' .OR. RUN .EQ. 1) THEN
          !For first run of a sequenced run, use replicate
          ! number instead of run number in header.
          IF (RNMODE .EQ. 'Q') THEN
            CALL HEADER(SEASINIT, GHGLUN, REPNO)
          ELSE
            CALL HEADER(SEASINIT, GHGLUN, RUN)
          ENDIF

          WRITE(GHGLUN,'("!",15X,A,A,A)')
     &"|---------- Daily (g/ha) ----------",
     &"|------- Cumulative (kg/ha) --------",
     &"|---- Cum CO2-equiv kg[CO2eq]/ha ---|"

          WRITE(GHGLUN,'("!",14X,A,A,A)')
     &" Soil CO2  Net CO2      N2O      CH4",
     &" Soil CO2  Net CO2      N2O      CH4",
     &"  Net CO2      N2O      CH4    Total"

          WRITE(GHGLUN,'("!",14X,A,A,A)')
     &"  g[C]/ha  g[C]/ha  g[N]/ha  g[C]/ha",
     &" kg[C]/ha kg[C]/ha kg[N]/ha kg[C]/ha",
     &"    CO2eq    CO2eq    CO2eq    CO2eq"

          WRITE(GHGLUN,'(A,A,A,A)') "@YEAR DOY   DAS",
     &"   CO2GED   CO2END   N2OGED   CH4GED",
     &"    CO2EC   CO2ENC    N2OEC    CH4EC",
     &"    CCEQC    NCEQC    MCEQC    TCEQC"

        ENDIF
      ENDIF

      CALL GET('ORGC','TSOMC',TSOMC_init)
      CO2ENC_Y = 0.0

!***********************************************************************
!***********************************************************************
!     DAILY OUTPUT
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. OUTPUT .OR. DYNAMIC .EQ. SEASINIT) THEN
C-----------------------------------------------------------------------
!      IF (INDEX('AD',IDETL) == 0) RETURN

      IF (IDETN == 'N') RETURN
      IF (MOD(DAS, FROP) .NE. 0) RETURN

      CALL YR_DOY(YRDOY, YEAR, DOY) 

      CO2GED = CH4_data % CO2emission * 1000.  !g/d
      N2OGED = N2O_data % N2O_emitted * 1000.  !g/d
      CH4GED = CH4_data % CH4Emission * 1000.  !g/d

      CO2EC = CH4_data % CumCO2Emission       !kg/d
      N2OEC = N2O_data % CN2O_emitted         !kg/d
      CH4EC = CH4_data % CumCH4Emission       !kg/d

!     CHP per Peter Grace, 2024-05-08
!     Estimate net CO2 emissions based on SOC. This is the integration
!       of all additions and losses to the system and is the accepted
!       method for calculating global CO2 emissions.
!     TSOMC = total soil organic matter (kg[C]/ha) = SOM1 + SOM2 + SOM3
!       Does not include fresh organic matter.
      CALL GET('ORGC','TSOMC',TSOMC)
      CO2ENC = (TSOMC_init - TSOMC )      !cumul net CO2 emissions kg/d
      CO2END = (CO2ENC - CO2ENC_Y) * 1000. !daily net CO2 emissions g/d
      CO2ENC_Y = CO2ENC

!     Calculation of cumulative CO2-equivalent emissions
!     CO2 - convert from units of C to units of CO2
!     CCEQC = CO2EC * 3.67 
      CCEQC = CO2ENC * 3.67 !use estimated net CO2 emissions

!     N2O - convert from N to N2O and multiply by 298
      NCEQC = N2OEC * 1.571 * 298.

!     CH4 - convert from C to CH4 and multiply by 25
      MCEQC = CH4EC * 1.33 * 25.

!     Total CO2-equivalent
      TCEQC = CCEQC + NCEQC + MCEQC

        IF (IDETN .EQ. 'Y') THEN
          WRITE (GHGLUN,'(I5,I4.3,I6,2I9,2F9.2,2I9,2F9.2,4I9)')  
     &      YEAR, DOY, DAS, 
     &      NINT(CO2GED), NINT(CO2END), N2OGED, CH4GED, 
     &      NINT(CO2EC), NINT(CO2ENC), N2OEC, CH4EC,
     &      NINT(CCEQC), NINT(NCEQC), NINT(MCEQC), NINT(TCEQC)
        ENDIF

      
!***********************************************************************
!***********************************************************************
!     SEASEND
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. SEASEND) THEN
C-----------------------------------------------------------------------
!     Store Summary.out labels and values in arrays to send to
!     OPSUM routines for printing.
      LABEL(1)  = 'CO2EM'; VALUE(1)  = CO2ENC

!     Send labels and values to OPSUM
      CALL SUMVALS (SUMNUM, LABEL, VALUE) 
!      IF (INDEX('AD',IDETL) == 0) RETURN
      !Close daily output files.
      CLOSE(GHGLUN)

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE OpGHG
C-------------------------------------------------------------------
!======================================================================
      END MODULE GHG_mod
!======================================================================

