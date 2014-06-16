!=======================================================================
C  MODULE N2O_mod
C  06/15/2014 CHP Written
!=======================================================================

      MODULE N2O_mod
!     Contains data definitions for N2O generation routines in SOILNI
      USE ModuleDefs

!     Data construct for control variables
      TYPE N2O_type
        REAL TN2, TN2D, TN2O, TN2OD, TNOX, TNOXD, TNITRIFY
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

      SUBROUTINE OpN2O(CONTROL, ISWITCH, SOILPROP, N2O_DATA) 
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
      CHARACTER*50 FRMT

      INTEGER DAS, DOY, DYNAMIC, ERRNUM, FROP, I, INCDAT, L, REPNO
      INTEGER N_LYR, NOUTDN, RUN, YEAR, YRDOY, SPACES
      INTEGER NAPFER(NELEM)

!          Cumul      Daily     Layer         
      REAL CNOX,      TNOXD,    DENITRIF(NL)  !Denitrification
      REAL CN2,       TN2D,     n2flux(nl)    !N2
      REAL CN2O,      TN2OD,    n2oflux(nl)   !N2O 
      REAL CNITRIFY,  TNITRIFY, NITRIF(NL)    !Nitrification 

!     Temp variables for Output.dat file:
      REAL TNH4, TNH4NO3, TNO3
      REAL NO3(NL), NH4(NL)

      REAL TNITRIFYD
      REAL, DIMENSION(NL) :: N2ONITIRF, WFPS 

      LOGICAL FEXIST

!-----------------------------------------------------------------------
!     Transfer values from constructed data types into local variables.
      DAS     = CONTROL % DAS
      DYNAMIC = CONTROL % DYNAMIC
      FROP    = CONTROL % FROP
      RNMODE  = CONTROL % RNMODE
      REPNO   = CONTROL % REPNO
      RUN     = CONTROL % RUN
      YRDOY   = CONTROL % YRDOY

      IDETN  = ISWITCH % IDETN
      ISWWAT = ISWITCH % ISWWAT
      ISWNIT = ISWITCH % ISWNIT

      IF (ISWWAT .EQ. 'N' .OR. ISWNIT .EQ. 'N') THEN
        RETURN
      ENDIF

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
C-----------------------------------------------------------------------
C     Variable heading for SoilN.OUT
C-----------------------------------------------------------------------
      IF (IDETN .EQ. 'Y') THEN
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
          WRITE(NOUTDN,'(A1,T62,A)',ADVANCE='NO') 
     &        "!","NO3 (ppm) by soil depth (cm):"
          SPACES = N_LYR * 8 - 29
          WRITE(FRMT,'(A,I2,A)')
     &     '(',SPACES,'X,"NH4 (ppm) by soil depth (cm):")'
          WRITE(NOUTDN,FRMT)
          WRITE(NOUTDN,'("!",T57,20A8)')
     &        (SoilProp%LayerText(L),L=1,N_LYR), 
     &        (SoilProp%LayerText(L),L=1,N_LYR)

          IF (N_LYR < 10) THEN
            WRITE (NOUTDN,105, ADVANCE='NO')
     &        ('NI',L,'D',L=1,N_LYR), 
     &        ('NH',L,'D',L=1,N_LYR) 
  105       FORMAT(20("    ",A2,I1,A1))
          ELSE
            WRITE (NOUTDN,110, ADVANCE='NO')
     &        ('NI',L,'D',L=1,9),'    NI10', 
     &        ('NH',L,'D',L=1,9),'    NH10'
  110       FORMAT(2(9("    ",A2,I1,A1),A8),"    ")
          ENDIF
! PG
!          WRITE (NOUTDN,115)
!  115     FORMAT('NMNC    NITC    NDNC    NIMC    AMLC   NNMNC    NUCM')
! new section included for daily outputs of nitrification and denitrification, N2O and wfps
          WRITE (NOUTDN,115, advance='no')
  115     FORMAT('NMNC    NITC    NDNC    NIMC    AMLC   NNMNC    NUCM')
          IF (N_LYR < 10) THEN
            WRITE (NOUTDN,125)
     &        ('NRF',L,'D',L=1,N_LYR), 
     &        ('DEN',L,'D',L=1,N_LYR),
     &        ('N2O',L,'D',L=1,N_LYR), !PG
     &        ('WFP',L,'D',L=1,N_LYR)
  125       FORMAT(40("    ",A2,I1,A1))
          ELSE
            WRITE (NOUTDN,135)
     &        ('NRF',L,'D',L=1,9),'   NRF10', 
     &        ('DEN',L,'D',L=1,9),'   DEN10',
     &        ('N2O',L,'D',L=1,9),'   N2O10', !PG
     &        ('WFP',L,'D',L=1,9),'   WFP10'
  135       FORMAT(4(9("    ",A2,I1,A1),A8),"    ")
          ENDIF
! PG
          CALL YR_DOY(INCDAT(YRDOY,-1), YEAR, DOY)
          WRITE (NOUTDN,310) YEAR, DOY, DAS, 0, 
     &       0, 0.0, TNH4NO3, TNO3, TNH4, 
     &       (NO3(I),I=1,N_LYR), (NH4(I),I=1,N_LYR),
     &       0.0, (nitrif(i),i=1,n_lyr), (denitrif(i),i=1,n_lyr), ! added by PG
     &       (n2oflux(i), i=1, n_lyr), (wfps(i), i=1,n_lyr)       ! added by PG
        ENDIF
      ENDIF

!***********************************************************************
!***********************************************************************
!     DAILY OUTPUT
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. OUTPUT .OR. DYNAMIC .EQ. SEASINIT) THEN
C-----------------------------------------------------------------------
      IF (MOD(DAS, FROP) .EQ. 0) THEN
        CALL YR_DOY(YRDOY, YEAR, DOY) 

        IF (IDETN .EQ. 'Y') THEN
          WRITE (NOUTDN,310) YEAR, DOY, DAS, 
     &       TNOXD,
     &       (nitrif(i),i=1,n_lyr), (denitrif(i),i=1,n_lyr), ! added by PG
     &       (n2oflux(i), i=1,n_lyr), (n2flux(i), i=1,n_lyr), 
     &       (wfps(i),i=1,n_lyr)     ! added by PG
  310     FORMAT(1X,I4,1X,I3.3,3(1X,I5),1X,F7.1,1X,F6.1,2F7.1,
     &       20(F8.2), 47(F8.2))
        ENDIF
      ENDIF

!***********************************************************************
!***********************************************************************
!     SEASEND
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. SEASEND) THEN
C-----------------------------------------------------------------------
      !Close daily output files.
      IF (IDETN .EQ. 'Y') CLOSE(NOUTDN)

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

