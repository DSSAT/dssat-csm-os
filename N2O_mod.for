!=======================================================================
C  MODULE N2O_mod
C  06/15/2014 CHP Written
!=======================================================================

      MODULE N2O_mod
!     Contains data definitions for N2O generation routines in SOILNI
      USE ModuleDefs

!     Data construct for control variables
      TYPE N2O_type
        REAL CN2,  CN2O,  CNOX,  CNITRIFY
        REAL TN2D, TN2OD, TNOXD, TNITRIFY
        REAL, DIMENSION(NL) :: DENITRIF, N2OFLUX, N2ONITIRF, N2FLUX, 
     &     NITRIF, WFPS
      END TYPE N2O_type

      CONTAINS

C=======================================================================
C  OpN2O, Subroutine, C.H.Porter, P. Grace
C  Generates output for daily soil N2O routines
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  06/15/2014 CHP Written
!=======================================================================

      SUBROUTINE OpN2O(CONTROL, ISWITCH, SOILPROP, newCO2, N2O_DATA, SW,
     &BD) 
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
      CHARACTER*300 FRMT, FRMT2

      INTEGER DAS, DOY, DYNAMIC, ERRNUM, FROP, I, L, REPNO
      INTEGER N_LYR, NOUTDN, RUN, YEAR, YRDOY, SPACES

!          Cumul      Daily     Layer         
      REAL CNOX,      TNOXD,    DENITRIF(NL)  !Denitrification
      REAL CN2,       TN2D,     n2flux(NL)    !N2
      REAL CN2O,      TN2OD,    n2oflux(NL)   !N2O 
      REAL CNITRIFY,  TNITRIFY, NITRIF(NL)    !Nitrification 

      REAL FRAC, NDN20, NIT20, N2O20, N2F20, newCO2(NL), TOTCO2
      real bd(nl),wfps(nl),sw(nl)         ! PG   !,poros(nl)
      real n2o_emitted, n2o_soil(nl), n2o_diffused  ! PG
      real n2_emitted, n2_soil(nl), n2_diffused  ! PG

!     Temp variables for Output.dat file:
!      REAL TNH4, TNH4NO3, TNO3
!      REAL NO3(NL), NH4(NL)

!      REAL TNITRIFYD
!      REAL, DIMENSION(NL) :: N2ONITIRF, WFPS 

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
      n2flux   = N2O_data % n2flux   
      CN2O     = N2O_data % CN2O        
      TN2OD    = N2O_data % TN2OD     
      n2oflux  = N2O_data % n2oflux  
      CNITRIFY = N2O_data % CNITRIFY     
      TNITRIFY = N2O_data % TNITRIFY 
      NITRIF   = N2O_data % NITRIF  
      WFPS     = N2O_data % WFPS  

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
C-----------------------------------------------------------------------
C     Variable heading for SoilN.OUT
C-----------------------------------------------------------------------
      IF (IDETN .EQ. 'Y') THEN

        FROP    = CONTROL % FROP
        RNMODE  = CONTROL % RNMODE
        REPNO   = CONTROL % REPNO
        RUN     = CONTROL % RUN

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

          SPACES = 117
          WRITE(FRMT,'(A,I3.3,A,A,I3.3,A,A,I3.3,A,A,I3.3,A)')
     &    '("!",T',SPACES,
     &    ',"Denitrification (g[N]/ha) by soil depth (cm):",',
     &    'T',(SPACES+N_LYR*8),
     &    ',"Nitrification (kg[N]/ha) by soil depth (cm):",',
     &    'T',(SPACES+2*N_LYR*8),
     &    ',"N2O flux (g[N]/ha) by soil depth (cm):",',
     &    'T',(SPACES+3*N_LYR*8),
     &    ',"N2 flux (g[N]/ha) by soil depth (cm):")'

          WRITE(NOUTDN,TRIM(FRMT))

          WRITE(NOUTDN,'("!",17X,A,A,T112,40A8)')
     &  "kg/ha   kg/ha   kg/ha   kg/ha   kg/ha   kg/ha   kg/ha   kg/ha",
     &  "    g/ha   kg/ha    g/ha    g/ha",
     &        (SoilProp%LayerText(L),L=1,N_LYR),
     &        (SoilProp%LayerText(L),L=1,N_LYR),
     &        (SoilProp%LayerText(L),L=1,N_LYR),
     &        (SoilProp%LayerText(L),L=1,N_LYR) 

          WRITE(NOUTDN,"(A)",ADVANCE='NO') 
     & "@YEAR DOY   DAS    NDNC    NITC   N2OFC   N2FLC   NDN20" //
     & "   NIT20   N2O20   N2F20    NDND   NITRD   N2OFD   N2FLD"
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
  110       FORMAT(4(9("   ",A,I1,A),A8),"  newCO2    N2O     N2") !PG
          ENDIF

!          CALL YR_DOY(INCDAT(YRDOY,-1), YEAR, DOY)
!          WRITE (NOUTDN,TRIM(FRMT2)) YEAR, DOY, DAS, 
!     &       CNOX, CNITRIFY, CN2O, CN2,
!     &       TNOXD*1000., TNITRIFY, TN2OD*1000., TN2D*1000.,
!     &       (DENITRIF(I)*1000.,I=1,N_LYR), (NITRIF(I),I=1,N_LYR),
!     &       (n2oflux(i)*1000., i=1, n_lyr), (N2FLUX(i)*1000.,i=1,n_lyr)
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

      IF (MOD(DAS, FROP) .EQ. 0) THEN
        CALL YR_DOY(YRDOY, YEAR, DOY) 

      NDN20 = NDN20 + DENITRIF(1)
      NIT20 = NIT20 + NITRIF(1)
      N2O20 = N2O20 + n2oflux(1)
      N2F20 = N2F20 + N2FLUX(1)

      DO L = 2, SOILPROP % NLAYR
        IF (SOILPROP % DS(L) <= 20.) THEN
!         Entire layer is in top 20 cm
          NDN20 = NDN20 + DENITRIF(L)
          NIT20 = NIT20 + NITRIF(L)
          N2O20 = N2O20 + N2OFLUX(L)
          N2F20 = N2F20 + N2FLUX(L)

        ELSEIF (SOILPROP % DS(L-1) < 20.) THEN
!         A portion (FRAC) of layer is in top 20 cm
          FRAC = (20. - SOILPROP % DS(L-1)) / SOILPROP % DLAYR(L)
          NDN20 = NDN20 + FRAC * DENITRIF(L)
          NIT20 = NIT20 + FRAC * NITRIF(L)
          N2O20 = N2O20 + FRAC * N2OFLUX(L)
          N2F20 = N2F20 + FRAC * N2FLUX(L)
        ENDIF
      ENDDO

      TOTCO2 = SUM(newCO2)
      
! Simple representation of diffusion of N2O and N2 emissions 14 June 2015
! Developed as N2O and duplicated as N2
! Diffusion of N2O from layers based on WFPS PGrace 14 June 2015
! N2O produced on any day in any layer and diffused upwards is directly proportional to WFPS (fraction)
! N2O not diffused from layer (1-WFPS) is added to the next day's total N2Oflux for that layer
! N2O emitted to the atmosphere on any day is from layer 1 only.
! n2oflux is the mass of N2O (kg N/ha) produced in a layer on any day
! n2o_diffused is mass diffused (kg N/ha) per layer
! n2o_soil is mass remaining in soil (kg N/ha) AFTER diffusion
! n2o emitted (output as g N/ha in N2O.OUT) is total emission from layer 1 on any day

! N2O section     
      DO L = 1, N_LYR
          if (n2oflux(L).lt. 0.0) then
              n2oflux(L) = 0.0
          endif
!          POROS(L)  = 1.0 - BD(L) / 2.65
!          wfps(L) = min (1.0, sw(L) / SOILPROP % poros(L))
          if (L.ge.2) then
             n2o_diffused = (n2oflux(L) + n2o_soil(L)) * (1.0 - WFPS(L))
             n2o_soil(L) = (n2oflux(L) + n2o_soil(L)) * WFPS(L)
             n2o_soil(L-1) = n2o_soil(L-1) + n2o_diffused
          endif
      ENDDO
      
      n2o_emitted = n2oflux(1)+n2o_soil(1)
      n2o_soil(1) = 0.0
 
! N2 section - same basis as N2O
      DO L = 1, N_LYR
          if (n2flux(L).lt. 0.0) then
              n2flux(L) = 0.0
          endif
          if (L.ge.2) then
             n2_diffused = (n2flux(L) + n2_soil(L)) * (1.0 - WFPS(L))
             n2_soil(L) = (n2flux(L) + n2_soil(L)) * WFPS(L)
             n2_soil(L-1) = n2_soil(L-1) + n2_diffused
          endif
      ENDDO
      
      n2_emitted = n2flux(1)+n2_soil(1)
      n2_soil(1) = 0.0
      
! End PG diffusion

          WRITE(FRMT2,'(A,I2.2,A,I2.2,A,I2.2,A)') 
     &   '(1X,I4,1X,I3.3,I6,2(F8.3,F8.1,2F8.3),F8.2,F8.3,F8.2,F8.3,',
     &   N_LYR,   'F8.3,', 
     &   N_LYR,   'F8.3,',
     &   2*N_LYR, 'F8.3,F8.3,F8.3,F8.3)'

        IF (IDETN .EQ. 'Y') THEN
          WRITE (NOUTDN,TRIM(FRMT2)) YEAR, DOY, DAS, 
     &       CNOX, CNITRIFY, CN2O, CN2, NDN20, NIT20, N2O20, N2F20,
     &       TNOXD*1000., TNITRIFY, TN2OD*1000., TN2D*1000.,
     &       (DENITRIF(I)*1000.,I=1,N_LYR), (NITRIF(I),I=1,N_LYR),
     &       (n2oflux(i)*1000., i=1, n_lyr), (N2FLUX(i)*1000.,i=1,n_lyr)
     &       , TOTCO2, n2o_emitted*1000, n2_emitted*1000    !PG
      ENDIF
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

