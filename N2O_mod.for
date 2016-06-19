!=======================================================================
C  MODULE N2O_mod
C  06/15/2014 CHP Written
!=======================================================================

      MODULE N2O_mod
!     Contains data definitions for N2O generation routines in SOILNI
      USE ModuleDefs

!     Data construct for control variables
      TYPE N2O_type
!            Daily        Cumul        Layer         
        REAL TNOXD,       CNOX,        DENITRIF(NL)  ![N] Denitrified
        REAL TN2OdenitD,  CN2Odenit,   n2odenit(NL)  !N2O[N] from denitrification
        REAL TN2D,        CN2,         n2flux(NL)    !N2[N] from denitrification

        REAL TNITRIFY,    CNITRIFY,    NITRIF(NL)    ![N] Nitrified 
        REAL TN2OnitrifD, CN2Onitrif,  n2onitrif(NL) !N2O[N] from nitrification

 	  REAL                           N2OFLUX(NL)   !N2Oflux = N2Odenit + N2Onitrif
        REAL N2_emitted,  CN2_emitted                !N2[N] emitted
        REAL N2O_emitted, CN2O_emitted               !N2O[N] emitted

        REAL, DIMENSION(NL) :: WFPS 
      END TYPE N2O_type

      CONTAINS

C=======================================================================
C  N2Oemit, Subroutine, P. Grace
C  Computes N2, N2O daily and cumulative emissions
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  09/18/2015 CHP Written, based on PG code.
!  05/30/2016 CHP remove diffusion model
!=======================================================================

      SUBROUTINE N2Oemit(CONTROL, ISWITCH, SOILPROP, N2O_DATA) 
!-------------------------------------------------------------------
      IMPLICIT NONE
      SAVE
!-----------------------------------------------------------------------

      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH
      TYPE (SoilType)    SOILPROP
      TYPE (N2O_type)    N2O_DATA

      CHARACTER*1  IDETN, ISWNIT, ISWWAT  
      CHARACTER*10, PARAMETER :: OUTSN2O = 'N2O.OUT'

      INTEGER DAS, DYNAMIC, L, NLAYR, YRDOY

      REAL N2flux(NL)                !N2
      REAL N2Odenit(NL)              !N2O from denitrification
      REAL N2Onitrif(NL)             !N2O from nitrification
      real wfps(nl)                  ! PG   

!          Daily        Cumul        
      REAL N2_emitted,  CN2_emitted  !N2 emitted
      REAL N2O_emitted, CN2O_emitted !N2O emitted

!LOCAL VARIABLES
      real n2o_soil(nl), n2o_diffused ! PG
      real n2_soil(nl),  n2_diffused  ! PG
      real N2Oflux(nl)  
      real RateDiffus
!     real, parameter :: DiffFactor = 0.5
      real DiffFactor

!-----------------------------------------------------------------------
!     Transfer values from constructed data types into local variables.
      DYNAMIC = CONTROL % DYNAMIC

      DAS     = CONTROL % DAS
      YRDOY   = CONTROL % YRDOY

      N2Odenit = N2O_data % N2Odenit  
      N2Onitrif= N2O_data % N2Onitrif  
      N2flux   = N2O_data % N2flux   
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

      N2_emitted = 0.0
      N2O_emitted = 0.0
      CN2_emitted = 0.0
      CN2O_emitted = 0.0

      NLAYR   = SOILPROP % NLAYR
      DiffFactor = SOILPROP % DiffFactor
      IF (DiffFactor <= 0.0) DiffFactor = 0.5
      SOILPROP % DiffFactor = DiffFactor
      
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

      DO L = NLAYR, 1, -1
          n2oflux(L) = n2onitrif(L) + n2odenit(L)                
          if (n2oflux(L).lt. 0.0) then
              n2oflux(L) = 0.0
          endif

          if (n2flux(L).lt. 0.0) then
              n2flux(L) = 0.0
          endif
          
          if (L.ge.2) then
            RateDiffus = (1.0 - WFPS(L)) * DiffFactor
            n2o_diffused = (n2oflux(L) + n2o_soil(L)) * RateDiffus
            n2o_soil(L) = (n2oflux(L) + n2o_soil(L)) *(1.0 - RateDiffus)
            n2o_soil(L-1) = n2o_soil(L-1) + n2o_diffused

            n2_diffused = (n2flux(L) + n2_soil(L)) * RateDiffus
            n2_soil(L) = (n2flux(L) + n2_soil(L)) * (1.0 - RateDiffus)
            n2_soil(L-1) = n2_soil(L-1) + n2_diffused
          endif
      ENDDO
      
      n2o_emitted = n2oflux(1) + n2o_soil(1)  !LAYER ONE
      n2o_soil(1) = 0.0
      CN2O_emitted = CN2O_emitted + N2O_emitted

      n2_emitted = n2flux(1) + n2_soil(1)   !LAYER ONE
      n2_soil(1) = 0.0
      CN2_emitted  = CN2_emitted  + N2_emitted

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      N2O_data % n2o_emitted  = n2o_emitted
      N2O_data % n2_emitted   = n2_emitted
      N2O_data % CN2O_emitted = CN2O_emitted
      N2O_data % CN2_emitted  = CN2_emitted
      N2O_data % N2Oflux      = N2Oflux
      N2O_data % N2flux       = N2flux

      RETURN
      END SUBROUTINE N2Oemit
C-------------------------------------------------------------------
C
!======================================================================


C=======================================================================
C  OpN2O, Subroutine, C.H.Porter, P. Grace
C  Generates output for daily soil N2O routines
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  06/15/2014 CHP Written
!=======================================================================

      SUBROUTINE OpN2O(CONTROL, ISWITCH, SOILPROP, newCO2, N2O_DATA) 
!-------------------------------------------------------------------
      IMPLICIT NONE
      SAVE
!-----------------------------------------------------------------------

      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH
      TYPE (SoilType)    SOILPROP
      TYPE (N2O_type)    N2O_DATA

      CHARACTER*1  IDETN, ISWNIT, ISWWAT, RNMODE
      CHARACTER*10, PARAMETER :: OUTSN2O = 'N2O.OUT'
      CHARACTER*500 FRMT, FRMT2

      INTEGER DAS, DOY, DYNAMIC, ERRNUM, FROP, I, L, REPNO
      INTEGER N_LYR, NOUTDN, RUN, YEAR, YRDOY, SPACES

!          Cumul      Daily     Layer         
      REAL CNOX,      TNOXD,    DENITRIF(NL)  !Denitrification
      REAL CNITRIFY,  TNITRIFY, NITRIF(NL)    !Nitrification 
      REAL TN2D,      CN2,      N2flux(NL)    !N2
!     REAL CN2O,      TN2OD,    N2Oflux(NL)   !N2O total (nitrification + denitrification)
      REAL                      N2Oflux(NL)   !N2O total (nitrification + denitrification)
!     Added N2Odenit for N2O from denitrification only and daily and cumulative variables
      REAL CN2Odenit, TN2OdenitD, N2Odenit(NL)   !N2O from denitrification only 
!     Daily total and cumulative totals for n2onitrif
      REAL CN2Onitrif, TN2OnitrifD, N2Onitrif(NL) ! N2O from nitrification only    

      REAL n2o_emitted, n2_emitted, CN2O_emitted, CN2_emitted  
       

      REAL NDN20, NIT20, N2O20, N2F20, newCO2(0:NL), TOTCO2, CumTotCO2
      real wfps(nl)         ! PG   !bd(nl),sw(nl),poros(nl)

      LOGICAL FEXIST

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
      n2flux   = N2O_data % n2flux   

!     added n2odenit and daily and cumulative variables      
      Cn2odenit = N2O_data % Cn2odenit
      Tn2odenitd= N2O_data % Tn2odenitd
      n2odenit  = N2O_data % n2odenit  
      CNITRIFY = N2O_data % CNITRIFY     
      TNITRIFY = N2O_data % TNITRIFY 
      NITRIF   = N2O_data % NITRIF  
      WFPS     = N2O_data % WFPS  

!     add daily total and cumulative N2ONITRIF variables
      CN2ONITRIF = N2O_data % CN2ONITRIF
      TN2ONITRIFD= N2O_data % TN2ONITRIFD
      N2ONITRIF  = N2O_data % N2ONITRIF

!***********************************************************************
!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
C-----------------------------------------------------------------------
C     Variable heading for N2O.OUT
C-----------------------------------------------------------------------
      IF (IDETN .EQ. 'Y') THEN

        FROP    = CONTROL % FROP
        RNMODE  = CONTROL % RNMODE
        REPNO   = CONTROL % REPNO
        RUN     = CONTROL % RUN

        CumTotCO2 = 0.0

        CALL GETLUN(OUTSN2O, NOUTDN)
        INQUIRE (FILE = OUTSN2O, EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT = NOUTDN, FILE = OUTSN2O, STATUS = 'OLD',
     &      IOSTAT = ERRNUM, POSITION = 'APPEND')
        ELSE
          OPEN (UNIT = NOUTDN, FILE = OUTSN2O, STATUS = 'NEW',
     &      IOSTAT = ERRNUM)
          WRITE(NOUTDN,'("*N2O emissions output file")')
        ENDIF

        IF (RNMODE .NE. 'Q' .OR. RUN .EQ. 1) THEN
          !For first run of a sequenced run, use replicate
          ! number instead of run number in header.
          IF (RNMODE .EQ. 'Q') THEN
            CALL HEADER(SEASINIT, NOUTDN, REPNO)
          ELSE
            CALL HEADER(SEASINIT, NOUTDN, RUN)
          ENDIF

          N_LYR = MIN(10, MAX(4,SOILPROP%NLAYR))

          WRITE(NOUTDN,'("!",14X,A,A,A)')
     & "|---------------------- Cumulative --------------------------",
     & "---|---------------------- Daily ----------------------------",
     & "------|"

          SPACES = 147
          WRITE(FRMT,'(A,A,A,A,A,A,I3.3,A,A,I3.3,A,A,I3.3,A,A,I3.3,A)')

     & '("!",14X,',
     & '" N20emit  N2emit     CO2   Denit  Nitrif   ",',
     & '"N2O-denit+nit      N2",',
     & '" N20emit  N2emit     CO2   Denit  Nitrif   ",',
     & '"N2O-denit+nit      N2",',
     & 'T',SPACES,
     & ',"Denitrification (g[N]/ha) by soil depth (cm):",',
     & 'T',(SPACES+N_LYR*8),
     & ',"Nitrification (kg[N]/ha) by soil depth (cm):",',
     & 'T',(SPACES+2*N_LYR*8),
     & ',"N2O flux (g[N]/ha) by soil depth (cm):",',
     & 'T',(SPACES+3*N_LYR*8),
     & ',"N2 flux (g[N]/ha) by soil depth (cm):")'

          WRITE(NOUTDN,TRIM(FRMT))

          WRITE(NOUTDN,'("!",17X,A,A,T144,40A8)')
     &"kg/ha   kg/ha   kg/ha   kg/ha   kg/ha   kg/ha   kg/ha   kg/ha  ",
     &"  g/ha    g/ha    g/ha    g/ha    g/ha    g/ha    g/ha    g/ha",
     &        (SoilProp%LayerText(L),L=1,N_LYR),
     &        (SoilProp%LayerText(L),L=1,N_LYR),
     &        (SoilProp%LayerText(L),L=1,N_LYR),
     &        (SoilProp%LayerText(L),L=1,N_LYR) 

          WRITE(NOUTDN,"(A)",ADVANCE='NO') 
     & "@YEAR DOY   DAS" //
     & "   N2OEC    N2EC   CO2TC    NDNC" // 
     & "    NITC   N2ODC   N2ONC   N2FLC" // 
     & "   N2OED    N2ED   CO2TD    NDND" // 
     & "   NITRD   N2ODD   N2OND   N2FLD"
          IF (N_LYR < 10) THEN
            WRITE (NOUTDN,105)
     &        ('NDN',L,'D',L=1,N_LYR), 
     &        ('NIT',L,'D',L=1,N_LYR),
     &        ('N2O',L,'D',L=1,N_LYR), 
     &        ('N2F',L,'D',L=1,N_LYR) 
  105       FORMAT(40("   ",A,I1,A))
          ELSE
            WRITE (NOUTDN,110)
     &        ('NDN',L,'D',L=1,9),'   NDN10', 
     &        ('NIT',L,'D',L=1,9),'   NIT10',
     &        ('N2O',L,'D',L=1,9),'   N2O10', 
     &        ('N2F',L,'D',L=1,9),'   N2F10'
  110       FORMAT(4(9("   ",A,I1,A),A8))
          ENDIF
        ENDIF
      ENDIF

      NDN20 = 0.0
      NIT20 = 0.0
      N2O20 = 0.0
      N2F20 = 0.0

!***********************************************************************
!***********************************************************************
!     DAILY OUTPUT
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. OUTPUT .OR. DYNAMIC .EQ. SEASINIT) THEN
C-----------------------------------------------------------------------
      IF (IDETN == 'N') RETURN
      IF (MOD(DAS, FROP) .NE. 0) RETURN

      CALL YR_DOY(YRDOY, YEAR, DOY) 

      TOTCO2 = SUM(newCO2)
      CumTotCO2 = CumTotCO2 + TOTCO2

      n2o_emitted  = N2O_data % n2o_emitted  
      n2_emitted   = N2O_data % n2_emitted   
      CN2O_emitted = N2O_data % CN2O_emitted 
      CN2_emitted  = N2O_data % CN2_emitted  
      
       WRITE(FRMT2,'(A,A,A,I2.2,A,I2.2,A,I2.2,A)') 
     &   '(1X,I4,1X,I3.3,I6,',
     &   '2F8.2,I8,F8.2,F8.1,3F8.3,',
     &   '2F8.1,I8,F8.1,I8,3F8.2,',
     &   4*N_LYR, 'F8.1)'

        IF (IDETN .EQ. 'Y') THEN
          WRITE (NOUTDN,TRIM(FRMT2)) YEAR, DOY, DAS, 
     &      CN2O_emitted, CN2_emitted, NINT(CumTotCO2),
     &      CNOX, CNITRIFY, CN2Odenit, CN2Onitrif, CN2, 
     &      N2O_emitted*1000., N2_emitted*1000., NINT(TOTCO2*1000.), 
     &      TNOXD*1000., NINT(TNITRIFY*1000.), TN2OdenitD*1000., 
     &         TN2OnitrifD*1000., TN2D*1000.,
     &      (DENITRIF(I)*1000., i=1,N_LYR), (NITRIF(I)*1000.,I=1,N_LYR),
     &      (N2Oflux(i)*1000., i=1,n_lyr), (N2flux(i)*1000.,i=1,n_lyr)
        ENDIF

!***********************************************************************
!***********************************************************************
!     SEASEND
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. SEASEND) THEN
C-----------------------------------------------------------------------
      !Close daily output files.
      CLOSE(NOUTDN)

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
!======================================================================
      END MODULE N2O_mod
!======================================================================

