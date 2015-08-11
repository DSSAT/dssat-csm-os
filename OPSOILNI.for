C=======================================================================
C  OpSoilNi, Subroutine, C.H.Porter from Soil Nitrogen and Carbon 
C     portions of OPDAY
C  Generates output for daily soil Nitrogen and Carbon data
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  11/16/2001 CHP Written
C  06/07/2002 GH  Modified for crop rotations
C  08/20/2002 GH  Modified for Y2K
!  05/18/2004 AJG Renamed variables for P module:
!                         NAPNIT   to NAPFER
!                         AMTNIT   to AMTFER
!     Note:  in Century, AMTFER is an array for soil elements, in
!                 NTRANS, AMTFER is a scalar and represents only N.
!  01/18/2005 CHP changed AMTFER to array of 3 for consistency
!-----------------------------------------------------------------------
!  Called from:   SoilNi (formerly NTRANS)
!  Calls:         None
!=======================================================================

      SUBROUTINE OpSoilNi(CONTROL, ISWITCH, SoilProp, 
     &    CIMMOBN, CMINERN, CNETMINRN, CNITRIFY, CNUPTAKE, 
     &    FertData, NH4, NO3, 
     &    TLCH, TNH4, TNH4NO3, TNO3, TNOX, TOTAML)
C-------------------------------------------------------------------
C
C  Soil Nitrogen Aspects OUTPUT File
C  Soil Carbon Aspects OUTPUT File
C
!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      SAVE
!-----------------------------------------------------------------------

      CHARACTER*1  IDETN, ISWNIT, ISWWAT, RNMODE
      CHARACTER*10, PARAMETER :: OUTSN = 'SoilNi.OUT'

      INTEGER DAS, DOY, DYNAMIC, ERRNUM, FROP, I, INCDAT, L, REPNO
      INTEGER N_LYR, NOUTDN, RUN, YEAR, YRDOY, SPACES
      INTEGER NAPFER(NELEM)

      REAL AMTFER(NELEM)
      REAL TNH4, TNH4NO3, TNO3
      REAL NO3(NL), NH4(NL)

      LOGICAL FEXIST

!     Arrays which contain data for printing in SUMMARY.OUT file
      INTEGER, PARAMETER :: SUMNUM = 4
!      INTEGER, PARAMETER :: N=1, P=2
      CHARACTER*4, DIMENSION(SUMNUM) :: LABEL
      CHARACTER*50 FRMT
      REAL, DIMENSION(SUMNUM) :: VALUE

!     Cumulative seasonal
      REAL CMINERN        !mineralization
      REAL CIMMOBN        !immobilization
      REAL CNITRIFY       !nitrification
      REAL TNOX           !Denitrification
      REAL TLCH           !Leaching
      REAL TOTAML         !ammonia volatilization
      REAL CNETMINRN      !net mineralization (mineralized-immobilized)
      REAL CNUPTAKE       !N uptake
!-----------------------------------------------------------------------
      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH
      TYPE (FertType)    FertData
      TYPE (SoilType)    SOILPROP

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

      AMTFER = FertData % AMTFER
      NAPFER = FertData % NAPFER

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
        CALL GETLUN(OUTSN, NOUTDN)
        INQUIRE (FILE = OUTSN, EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT = NOUTDN, FILE = OUTSN, STATUS = 'OLD',
     &      IOSTAT = ERRNUM, POSITION = 'APPEND')
        ELSE
          OPEN (UNIT = NOUTDN, FILE = OUTSN, STATUS = 'NEW',
     &      IOSTAT = ERRNUM)
          WRITE(NOUTDN,'("*Soil Inorganic Nitrogen daily output file")')
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

          WRITE (NOUTDN,100, ADVANCE='NO')
  100     FORMAT('@YEAR DOY   DAS',
     &     '  NAPC  NI#M    NLCC   NIAD   NITD   NHTD')
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
          WRITE (NOUTDN,115)
  115     FORMAT('NMNC    NITC    NDNC    NIMC    AMLC   NNMNC    NUCM')

          CALL YR_DOY(INCDAT(YRDOY,-1), YEAR, DOY) 
          WRITE (NOUTDN,310) YEAR, DOY, DAS, 0, 
     &       0, 0.0, TNH4NO3, TNO3, TNH4, 
     &       (NO3(I),I=1,N_LYR), (NH4(I),I=1,N_LYR),
     &       0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 
     &       0.0
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
          WRITE (NOUTDN,310) YEAR, DOY, DAS, NINT(AMTFER(N)), 
!     &       NAPFER, TLCH, TNH4NO3, NINT(THUMN), TNO3, TNH4, 
     &       NAPFER(N), TLCH, TNH4NO3, TNO3, TNH4, 
     &       (NO3(I),I=1,N_LYR), (NH4(I),I=1,N_LYR),
     &       CMINERN, CNITRIFY, TNOX, CIMMOBN, TOTAML, CNETMINRN, 
     &       CNUPTAKE
  310     FORMAT(1X,I4,1X,I3.3,3(1X,I5),1X,F7.1,1X,F6.1,2F7.1,
     &       20(F8.2), 10F8.2)
        ENDIF

      ENDIF

!***********************************************************************
!***********************************************************************
!     SEASEND
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. SEASEND) THEN
C-----------------------------------------------------------------------
      !Write end of season summary info for SUMMARY.OUT file
      !Scratch file has been opened by subroutine OPSUM, so
      !just need to retrieve correct unit number.
!      IF (IDETS .EQ. 'Y' .OR. IDETS .EQ. 'A') THEN
!         Store Summary.out labels and values in arrays to send to
!         OPSUM routines for printing.  Integers are temporarily 
!         saved aS real numbers for placement in real array.
          LABEL(1)  = 'NI#M'; VALUE(1)  = FLOAT(NAPFER(N))
          LABEL(2)  = 'NICM'; VALUE(2)  = AMTFER(N)
          LABEL(3)  = 'NLCM'; VALUE(3)  = TLCH
          LABEL(4)  = 'NIAM'; VALUE(4)  = TNH4NO3

          !Send labels and values to OPSUM
          CALL SUMVALS (SUMNUM, LABEL, VALUE) 
!      ENDIF

      !Close daily output files.
      IF (IDETN .EQ. 'Y') CLOSE(NOUTDN)

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE OpSoilNi
C-------------------------------------------------------------------
C
