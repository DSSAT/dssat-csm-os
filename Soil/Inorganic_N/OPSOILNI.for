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
!  11/21/2017 HJ  added CNTILEDR to soil N output
!-----------------------------------------------------------------------
!  Called from:   SoilNi (formerly NTRANS)
!  Calls:         None
!=======================================================================
!     HJ added CNTILEDR
      SUBROUTINE OpSoilNi(CONTROL, ISWITCH, SoilProp, 
     &    CIMMOBN, CMINERN, CNETMINRN, CNITRIFY, CNUPTAKE, 
     &    FertData, NH4, NO3,
     &    CLeach, CNTILEDR, TNH4, TNH4NO3, TNO3, TUREA, CNOX, TOTAML)
!-----------------------------------------------------------------------
      USE ModuleDefs
!     VSH
      USE CsvOutput 
      USE Linklist
      IMPLICIT NONE
      EXTERNAL GETLUN, HEADER, INCDAT, SUMVALS, YR_DOY
      SAVE
!-----------------------------------------------------------------------

      CHARACTER*1  IDETN, ISWNIT, ISWWAT, RNMODE
      CHARACTER*10, PARAMETER :: OUTSN = 'SoilNi.OUT'

      INTEGER DAS, DOY, DYNAMIC, ERRNUM, FROP, I, INCDAT, L, REPNO
      INTEGER N_LYR, NOUTDN, RUN, YEAR, YRDOY, SPACES
      INTEGER NAPFER(NELEM)

      REAL AMTFER(NELEM)
      REAL TNH4, TNH4NO3, TNO3, TUREA
      REAL NO3(NL), NH4(NL)

      LOGICAL FEXIST

!     Arrays which contain data for printing in SUMMARY.OUT file
      INTEGER, PARAMETER :: SUMNUM = 5
      CHARACTER*5, DIMENSION(SUMNUM) :: LABEL
      CHARACTER*50 FRMT1, FRMT2
      REAL, DIMENSION(SUMNUM) :: VALUE

!     Cumulative seasonal
      REAL CMINERN        !mineralization
      REAL CIMMOBN        !immobilization
      REAL CNITRIFY       !nitrification
      REAL CNOX           !Denitrification
      REAL CLeach         !Leaching
      REAL CNTILEDR       !N loss to tile drainage    !HJ added
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
      FMOPT  = ISWITCH % FMOPT    ! VSH

      AMTFER = FertData % AMTFER
      NAPFER = FertData % NAPFER

      IF (ISWWAT .EQ. 'N' .OR. ISWNIT .EQ. 'N') RETURN

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
C-----------------------------------------------------------------------
C     Variable heading for SoilN.OUT
C-----------------------------------------------------------------------
      IF (IDETN .EQ. 'Y') THEN
        IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN   ! VSH
          CALL GETLUN(OUTSN, NOUTDN)
          INQUIRE (FILE = OUTSN, EXIST = FEXIST)
          IF (FEXIST) THEN
            OPEN (UNIT = NOUTDN, FILE = OUTSN, STATUS = 'OLD',
     &        IOSTAT = ERRNUM, POSITION = 'APPEND')
          ELSE
            OPEN (UNIT = NOUTDN, FILE = OUTSN, STATUS = 'NEW',
     &        IOSTAT = ERRNUM)
            WRITE(NOUTDN,
     &        '("*Soil Inorganic Nitrogen daily output file")')
          ENDIF
        ENDIF   ! VSH
        
        IF (RNMODE .NE. 'Q' .OR. RUN .EQ. 1) THEN
          !For first run of a sequenced run, use replicate
          ! number instead of run number in header.
          
          IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN   ! VSH
            IF (RNMODE .EQ. 'Q') THEN
              CALL HEADER(SEASINIT, NOUTDN, REPNO)
            ELSE
              CALL HEADER(SEASINIT, NOUTDN, RUN)
            ENDIF
          ENDIF   ! VSH
          
          N_LYR = MIN(10, MAX(4,SOILPROP%NLAYR))
          CALL YR_DOY(INCDAT(YRDOY,-1), YEAR, DOY) 

          IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN   ! VSH
            WRITE(NOUTDN,'(A1,T136,A)',ADVANCE='NO') 
     &        "!","NO3 (ppm) at soil dep. (cm):"
          
            SPACES = (N_LYR - 4) * 8 + 4
          
            WRITE(FRMT1,'(A,I2.2,A)') 
     &       '(',SPACES,'X,"NH4 (ppm) at soil dep. (cm):")'
            WRITE(NOUTDN, FRMT1, ADVANCE='NO')

            WRITE(FRMT2,'(A,I2.2,A)') 
     &       '(T',SPACES,'X,"Total Inorganic N @dep(ppm):")'
            WRITE(NOUTDN, FRMT2)

            WRITE(NOUTDN,'("!",T132,30A8)')
     &        (SoilProp%LayerText(L),L=1,N_LYR), 
     &        (SoilProp%LayerText(L),L=1,N_LYR),
     &        (SoilProp%LayerText(L),L=1,N_LYR)

            WRITE (NOUTDN,100, ADVANCE='NO')
  100       FORMAT('@YEAR DOY   DAS  NAPC  NI#M',
     &       '    NIAD    NITD    NHTD   NURTD    NMNC    NITC    NDNC',
C-GH &       '    NIMC    AMLC   NNMNC    NUCM    NLCC    TDFC')
     &       '    NIMC    AMLC   NNMNC    NUCM    NLCC    TDNC')

            IF (N_LYR < 10) THEN
              WRITE (NOUTDN,105)
     &          ('NI',L,'D',L=1,N_LYR), 
     &          ('NH',L,'D',L=1,N_LYR), 
     &          ('NT',L,'D',L=1,N_LYR) 
  105         FORMAT(30("    ",A2,I1,A1))
            ELSE
              WRITE (NOUTDN,110)
     &          ('NI',L,'D',L=1,9),'    NI10', 
     &          ('NH',L,'D',L=1,9),'    NH10',
     &          ('NT',L,'D',L=1,9),'    NT10' 
  110         FORMAT(3(9("    ",A2,I1,A1),A8),"    ")
            ENDIF

            WRITE (NOUTDN,310) YEAR, DOY, DAS, 0, 
     &       0, TNH4NO3, TNO3, TNH4, TUREA,
     &       0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 
     &       0.0, 0.0, 0.0,                           !HJ 0.0
     &       (NO3(I),I=1,N_LYR), (NH4(I),I=1,N_LYR),
     &       (NO3(I)+NH4(I),I=1,N_LYR)
          END IF   ! VSH

!         VSH
          IF (FMOPT == 'C') THEN 
             AMTFER = 0.0
             NAPFER = 0
             CALL CsvOutSoilNi(EXPNAME, RUN, CONTROL%TRTNUM, 
     &         CONTROL%ROTNUM, CONTROL%REPNO, YEAR, DOY, DAS,  
     &         N, AMTFER, NAPFER, 0.0,TNH4NO3,TNO3,TNH4, N_LYR, NO3,NH4,
     &         0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     &         vCsvlineSoilNi, vpCsvlineSoilNi, vlngthSoilNi)
          
             CALL LinklstSoilNi(vCsvlineSoilNi)
          END IF
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
          IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN   ! VSH
            WRITE (NOUTDN,310) YEAR, DOY, DAS, NINT(AMTFER(N)), 
!     &       NAPFER, TLCH, TNH4NO3, NINT(THUMN), TNO3, TNH4, 
!             HJ added CNTILEDR 
     &       NAPFER(N), TNH4NO3, TNO3, TNH4, TUREA,
     &       CMINERN, CNITRIFY, CNOX, CIMMOBN, TOTAML, CNETMINRN,
     &       CNUPTAKE, CLeach, CNTILEDR,
     &       (NO3(I),I=1,N_LYR), (NH4(I),I=1,N_LYR), 
     &       (NO3(I)+NH4(I),I=1,N_LYR)
  310       FORMAT(1X,I4,1X,I3.3,3(1X,I5),4F8.1,
     &       10F8.2,2F8.1, !HJ modified
     &       30F8.2) 
          END IF   ! VSH
          
!         VSH
          IF (FMOPT == 'C') THEN 
            CALL CsvOutSoilNi(EXPNAME, RUN, CONTROL%TRTNUM, 
     &        CONTROL%ROTNUM, CONTROL%REPNO, YEAR, DOY, DAS,  
     &        N, AMTFER, NAPFER, CLEACH ,TNH4NO3,TNO3,TNH4, N_LYR,
     &        NO3, NH4, CMINERN, CNITRIFY, CNOX, CIMMOBN, TOTAML, 
     &        CNETMINRN, CNUPTAKE,
     &        vCsvlineSoilNi, vpCsvlineSoilNi, vlngthSoilNi)
     
            CALL LinklstSoilNi(vCsvlineSoilNi)
          END IF
      
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
          LABEL(3)  = 'NLCM'; VALUE(3)  = CLeach
          LABEL(4)  = 'NIAM'; VALUE(4)  = TNH4NO3
          LABEL(5)  = 'NMINC';VALUE(5)  = CNETMINRN

          !Send labels and values to OPSUM
          CALL SUMVALS (SUMNUM, LABEL, VALUE) 
!      ENDIF

      !Close daily output files.
!      IF (IDETN .EQ. 'Y') CLOSE(NOUTDN)
      IF ((IDETN == 'Y') 
     & .AND. (FMOPT == 'A'.OR.FMOPT == ' ')) CLOSE(NOUTDN)   ! VSH
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
