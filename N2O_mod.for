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
      CHARACTER*150 FRMT

      INTEGER DAS, DOY, DYNAMIC, ERRNUM, FROP, I, INCDAT, L, REPNO
      INTEGER N_LYR, NOUTDN, RUN, YEAR, YRDOY, SPACES

!          Cumul      Daily     Layer         
      REAL CNOX,      TNOXD,    DENITRIF(NL)  !Denitrification
      REAL CN2,       TN2D,     n2flux(nl)    !N2
      REAL CN2O,      TN2OD,    n2oflux(nl)   !N2O 
      REAL CNITRIFY,  TNITRIFY, NITRIF(NL)    !Nitrification 

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

          SPACES = 85
          WRITE(FRMT,'(A,I2.2,A)')
     &    '("!",T',SPACES,
     &    '"Denitrification (g[N]/ha) by soil depth (cm):")'
          WRITE(NOUTDN,FRMT, ADVANCE='NO') 

          SPACES = SPACES + N_LYR * 8
!          WRITE(FRMT,'(A,I3.3,A)')
!     &    '(T',SPACES,',"Nitrification (g[N]/ha) by soil depth (cm):")'
!         WRITE(NOUTDN,FRMT,ADVANCE='NO')
          WRITE(NOUTDN,'(T165,A)',ADVANCE='NO') 
     &       "Nitrification (g[N]/ha) by soil depth (cm):"

          SPACES = SPACES + N_LYR * 8
          WRITE(FRMT,'(A,I3.3,A)')
     &    '(T',SPACES,',"N2O flux (g[N]/ha) by soil depth (cm):")'
          WRITE(NOUTDN,FRMT,ADVANCE='NO')

          SPACES = SPACES + N_LYR * 8
          WRITE(FRMT,'(A,I3.3,A)')
     &    '(T',SPACES,',"N2 flux (g[N]/ha) by soil depth (cm):")'
          WRITE(NOUTDN,FRMT)

          WRITE(NOUTDN,'("!",T80,40A8)')
     &        (SoilProp%LayerText(L),L=1,N_LYR),
     &        (SoilProp%LayerText(L),L=1,N_LYR),
     &        (SoilProp%LayerText(L),L=1,N_LYR),
     &        (SoilProp%LayerText(L),L=1,N_LYR) 

          WRITE(NOUTDN,"(A)",ADVANCE='NO') 
     & "@YEAR DOY   DAS    NDNC    NITC   N2OFC   N2FLC    NDND" //
     & "   NITRD   N2OFD   N2FLD"
          IF (N_LYR < 10) THEN
            WRITE (NOUTDN,105)
     &        ('NDN',L,'D',L=1,N_LYR), 
     &        ('NIT',L,'D',L=1,N_LYR),
     &        ('N2O',L,'D',L=1,N_LYR), 
     &        ('N2F',L,'D',L=1,N_LYR) 
  105       FORMAT(40("    ",A2,I1,A1))
          ELSE
            WRITE (NOUTDN,110)
     &        ('NDN',L,'D',L=1,9),'   NDN10', 
     &        ('NIT',L,'D',L=1,9),'   NIT10',
     &        ('N2O',L,'D',L=1,9),'   N2O10', 
     &        ('N2F',L,'D',L=1,9),'   N2F10'
  110       FORMAT(4(9("    ",A2,I1,A1),A8),"   ")
          ENDIF

          CALL YR_DOY(INCDAT(YRDOY,-1), YEAR, DOY)
          WRITE (NOUTDN,310) YEAR, DOY, DAS, 
     &       CNOX, CNITRIFY, CN2O, CN2,
     &       TNOXD, TNITRIFY, TN2OD, TN2D,
     &       (DENITRIF(I),I=1,N_LYR), (NITRIF(I),I=1,N_LYR),
     &       (n2oflux(i), i=1, n_lyr), (N2FLUX(i), i=1,n_lyr)
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
     &       CNOX, CNITRIFY, CN2O, CN2,
     &       TNOXD, TNITRIFY, TN2OD, TN2D,
     &       (DENITRIF(I),I=1,N_LYR), (NITRIF(I),I=1,N_LYR),
     &       (n2oflux(i), i=1, n_lyr), (N2FLUX(i), i=1,n_lyr)
  310     FORMAT(1X,I4,1X,I3.3,I6,48F8.2)
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

