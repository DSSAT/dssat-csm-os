!=======================================================================
!  OpSoilPi, Subroutine, U. Singh and C.H.Porter
!-----------------------------------------------------------------------
!  Inorganic Phosphorus
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  01/24/2005 CHP Written
!  06/17/2006 AJG Renamed all root-zone and no-root-zone variables with
!                 Rts and noRts.
!-----------------------------------------------------------------------
!  Called: SoilPi
!  Calls: GETLUN, HEADER, YR_DOY, SUMVALS
!=====================================================================

      SUBROUTINE OpSoilPi(CONTROL, ISWITCH, 
     &  CImmobP, CMinerP, FertData, PUptake, !FracRts,  
!    &  ProfLab2Act, ProfAct2Lab, ProfAct2Sta, ProfSta2Act,  
     &  SOILPROP, SPi_AVAIL, SPiAvlProf, PiLabile,
     &  SPiSolProf, SPiLabProf, SPiActProf, SPiStaProf, SPiTotProf)
!    &  SPiSolRtsProf, SPiSolNoRtsProf, SPiLabRtsProf, SPiLabNoRtsProf)
!    &  PiSolRts, PiSolNoRts, PiLabRts, PiLabNoRts,  
!    &  SPiLabRts, SPiLabNoRts, SPiSolRts, SPiSolNoRts)

C-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
!     VSH
      USE CsvOutput 
      USE Linklist
      IMPLICIT NONE
      EXTERNAL GETLUN, HEADER, YR_DOY, SUMVALS
      SAVE

      CHARACTER*1  IDETL, IDETP, ISWPHO, RNMODE
      CHARACTER*8  LayerText(11)
      CHARACTER*10, PARAMETER :: OUTSP = 'SoilPi.OUT'

      INTEGER DAS, DOY, DYNAMIC, ERRNUM, FROP
      INTEGER L, LUN, NLAYR, REPNO, RUN, YEAR, YRDOY

      REAL SPiAvlProf, SPiSolProf, SPiLabProf
      REAL SPiActProf, SPiStaProf, SPiTotProf
!     REAL ProfLab2Act, ProfAct2Lab, ProfAct2Sta, ProfSta2Act
      Real CumFertP, CMinerP, CImmobP, CumUptakeP

      REAL, DIMENSION(NL) :: PUptake, SPi_AVAIL !, FracRts
      REAL, DIMENSION(NL) :: PiLabile
!     REAL, DIMENSION(NL) :: PiSolRts, PiSolNoRts
!     REAL, DIMENSION(NL) :: PiLabRts, PiLabNoRts
!     REAL, DIMENSION(NL) :: SPiSolRts, SPiSolNoRts
!     REAL, DIMENSION(NL) :: SPiLabRts, SPiLabNoRts

!      REAL SPiSolRtsProf, SPiSolNoRtsProf
!      REAL SPiLabRtsProf, SPiLabNoRtsProf

      LOGICAL FEXIST, DOPRINT

!     Arrays which contain data for printing in SUMMARY.OUT file
      INTEGER, PARAMETER :: SUMNUM = 4
      CHARACTER*4, DIMENSION(SUMNUM) :: LABEL
      REAL, DIMENSION(SUMNUM) :: VALUE

!-----------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.
      TYPE (ControlType)  CONTROL
      TYPE (FertType)     FERTDATA
      TYPE (SwitchType)   ISWITCH
      TYPE (SoilType)     SOILPROP

!     Transfer values from constructed data types into local variables.
      DAS     = CONTROL % DAS
      DYNAMIC = CONTROL % DYNAMIC
      FROP    = CONTROL % FROP
      REPNO   = CONTROL % REPNO
      RNMODE  = CONTROL % RNMODE
      RUN     = CONTROL % RUN
      YRDOY   = CONTROL % YRDOY

      CumFertP = FertData % AMTFER(P)

      NLAYR = SOILPROP % NLAYR
         
      FMOPT = ISWITCH % FMOPT   ! VSH

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      CMinerP    = 0.0
      CImmobP    = 0.0
      CumUptakeP = 0.0
      
      IDETP = ISWITCH % IDETP
      ISWPHO= ISWITCH % ISWPHO
      IDETL = ISWITCH % IDETL
      IF (IDETP .NE. 'Y' .OR. ISWPHO .EQ. 'N' .OR. IDETL == '0') RETURN

      IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN   ! VSH
      CALL GETLUN(OUTSP, LUN)

      INQUIRE (FILE = OUTSP, EXIST = FEXIST)
      IF (FEXIST) THEN
        OPEN (UNIT = LUN, FILE = OUTSP, STATUS = 'OLD',
     &    IOSTAT = ERRNUM, POSITION = 'APPEND')
      ELSE
        OPEN (UNIT = LUN, FILE = OUTSP, STATUS = 'NEW',
     &    IOSTAT = ERRNUM)
        WRITE(LUN,'("*Soil Inorganic Phosphorus daily output file")')
      ENDIF
      END IF   ! VSH
      
      IF (RUN .EQ. 1 .OR. INDEX('QF',RNMODE) .LE. 0) THEN
        IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN   ! VSH
        !For sequenced run, use replicate
        ! number instead of run number in header.
        IF (RNMODE .EQ. 'Q') THEN
          CALL HEADER(SEASINIT, LUN, REPNO)
        ELSE
          CALL HEADER(SEASINIT, LUN, RUN)
        ENDIF
        END IF   ! VSH
        
!       Text describing soil layer depths
        LayerText = SoilProp % LayerText
!       LayerText(11) is for layers 5 thru NLAYR
        IF (NLAYR > 4) THEN
          LayerText(5) = SOILPROP % LayerText(11)
        ENDIF

        IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN   ! VSH
        WRITE (LUN,'("!",T112,"P avail (kg/ha) by soil depth (cm):",
     &    T157,"P uptake (kg/ha) by soil depth (cm):",
     &    T202,"Labile P (ppm) by soil depth (cm):")')
        WRITE (LUN, '("!",T106,15(1X,A8))') 
     &    (LayerText(L),L=1,5),(LayerText(L),L=1,5),(LayerText(L),L=1,5)
        WRITE (LUN,120)
  120   FORMAT('@YEAR DOY   DAS',
     &  '     PIAD    PAVLD    PSOLD    PLABD    PACTD    PSTAD',
!    &  '  SPLabRt  SPLabNR  SPSolRt  SPSolNR',
     &  '     PAPC     PMNC     PIMC     PUPC',
     &  '    PAV1D    PAV2D    PAV3D    PAV4D    PAV5D',
     &  '    PUP1D    PUP2D    PUP3D    PUP4D    PUP5D', !,
     &  '    PLAB1    PLAB2    PLAB3    PLAB4    PLAB5')
!!       Temp variables
!     &  '    ST2AC    AC2ST    AC2LA    LA2AC',
!     &  '    FRRT1    FRRT2    FRRT3    FRRT4    FRRT5',
!     &  '   PSOLR1   PSOLR2   PSOLR3   PSOLR4   PSOLR5',
!     &  '   PSOLN1   PSOLN2   PSOLN3   PSOLN4   PSOLN5',
!     &  '   PLABR1   PLABR2   PLABR3   PLABR4   PLABR5',
!     &  '   PLABN1   PLABN2   PLABN3   PLABN4   PLABN5',
!     &  '  SPSOLR1  SPSOLR2  SPSOLR3  SPSOLR4  SPSOLR5',
!     &  '  SPSOLN1  SPSOLN2  SPSOLN3  SPSOLN4  SPSOLN5',
!     &  '  SPLABR1  SPLABR2  SPLABR3  SPLABR4  SPLABR5',
!     &  '  SPLABN1  SPLABN2  SPLABN3  SPLABN4  SPLABN5')
         END IF   ! VSH
      ENDIF

      ENDIF
    
!!********************************************************************
!!     Temporary output file
!      CALL GETLUN("DetailP.OUT", LUN2)
!
!      INQUIRE (FILE = "DetailP.OUT", EXIST = FEXIST)
!      IF (FEXIST) THEN
!        OPEN (UNIT = LUN2, FILE = "DetailP.OUT", STATUS = 'OLD',
!     &    IOSTAT = ERRNUM, POSITION = 'APPEND')
!      ELSE
!        OPEN (UNIT = LUN2, FILE = "DetailP.OUT", STATUS = 'NEW',
!     &    IOSTAT = ERRNUM)
!        WRITE(LUN2,'("*Soil Inorganic Phosphorus daily DETAIL file")')
!      ENDIF
!
!      IF (RUN .EQ. 1 .OR. INDEX('QF',RNMODE) .LE. 0) THEN
!     
!        !For sequenced run, use replicate
!        ! number instead of run number in header.
!        IF (RNMODE .EQ. 'Q') THEN
!          CALL HEADER(SEASINIT, LUN2, REPNO)
!        ELSE
!          CALL HEADER(SEASINIT, LUN2, RUN)
!        ENDIF
!
!        WRITE (LUN2,120)
!  120   FORMAT('@YEAR DOY   DAS',
!     &  '   SPiLR1   SPiLR2   SPiLR3   SPiLR4   SPiLR5',
!     &  '   SPiLN1   SPiLN2   SPiLN3   SPiLN4   SPiLN5',
!     &  '   SPiSR1   SPiSR2   SPiSR3   SPiSR4   SPiSR5',
!     &  '   SPiSN1   SPiSN2   SPiSN3   SPiSN4   SPiSN5',
!     &  '    PiSR1    PiSR2    PiSR3    PiSR4    PiSR5',
!     &  '    PiSN1    PiSN2    PiSN3    PiSN4    PiSN5',
!     &  '      SW1      SW2      SW3      SW4      SW5',
!     &  '    FRRT1    FRRT2    FRRT3    FRRT4    FRRT5')
!      ENDIF

!      WRITE(,) (SPiLabRts(L),L=1,5), (SPiLabNoRts(L),L=1,5), SPiSolRts(L), SPiSolNoRts(L),
!     &        PiSolRts(L), PiSolNoRts(L), 
!***********************************************************************
!***********************************************************************
!     OUTPUT
!***********************************************************************
      IF (DYNAMIC == OUTPUT .OR. DYNAMIC == SEASINIT) THEN
!-----------------------------------------------------------------------

      DO L = 1, NLAYR
        CumUptakeP = CumUptakeP + PUptake(L)
      ENDDO

!     Daily printout
      IF (IDETP .NE. 'Y' .OR. ISWPHO .EQ. 'N' .OR. IDETL == '0') RETURN

      DOPRINT = .FALSE.
      SELECT CASE(DYNAMIC)
      CASE (SEASINIT)
        IF (RUN == 1 .OR. INDEX('QF',RNMODE) <= 0) DOPRINT = .TRUE.
      CASE (OUTPUT)
        IF (MOD(DAS, FROP) == 0) DOPRINT = .TRUE.
      END SELECT


      IF (DOPRINT) THEN    
        CALL YR_DOY(YRDOY, YEAR, DOY) 
        IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN   ! VSH
        WRITE (LUN,300) YEAR, DOY, DAS, 
     &    SPiTotProf, SPiAvlProf, SPiSolProf, 
     &    SPiLabProf, SPiActProf, SPiStaProf, 
!    &    SPiLabRtsProf, SPiLabNoRtsProf, SPiSolRtsProf,SPiSolNoRtsProf,
     &    CumFertP, CMinerP, CImmobP, CumUptakeP,
     &    SPi_AVAIL(1:5), PUptake(1:5),   !,
     &    PiLabile(1:5)
!!       Temp variables:
!     &    ProfSta2Act, ProfAct2Sta, ProfAct2Lab, ProfLab2Act, 
!     &    (FracRts(L),L=1,5), (PiSolRts(L),L=1,5), (PiSolNoRts(L),L=1,5)
!     &   ,(PiLabRts(L),L=1,5), (PiLabNoRts(L),L=1,5),
!     &    (SPiSolRts(L),L=1,5), (SPiSolNoRts(L),L=1,5)
!     &   ,(SPiLabRts(L),L=1,5), (SPiLabNoRts(L),L=1,5)
  300   FORMAT(1X,I4,1X,I3.3,1X,I5,
     &    F9.1, 2F9.3, 3F9.1, F9.1, 3F9.2, 10F9.3, 5F9.2)
        END IF   ! VSH
      ENDIF

!     VSH
      IF (FMOPT == 'C') THEN 
         CALL CsvOutSoilPi(EXPNAME, CONTROL%RUN, CONTROL%TRTNUM, 
     &CONTROL%ROTNUM, CONTROL%REPNO, YEAR, DOY, DAS, 
     &SPiTotProf, SPiAvlProf, SPiSolProf, SPiLabProf, SPiActProf, 
     &SPiStaProf, CumFertP, CMinerP, CImmobP, CumUptakeP, SPi_AVAIL, 
     &PUptake, PiLabile,  
     &vCsvlineSoilPi, vpCsvlineSoilPi, vlngthSoilPi)
     
         CALL LinklstSoilPi(vCsvlineSoilPi)
      END IF
      
!***********************************************************************
!***********************************************************************
!     SEASEND
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASEND) THEN
!-----------------------------------------------------------------------
!     Write end of season summary info for SUMMARY.OUT file
!     Scratch file has been opened by subroutine OPSUM, so
!     just need to retrieve correct unit number.

!     Store Summary.out labels and values in arrays to send to
!     OPSUM routines for printing.  Integers are temporarily 
!     saved as real numbers for placement in real array.

      LABEL(1) = 'PI#M'; VALUE(1) = FLOAT(FertData % NAPFER(P)) 
      LABEL(2) = 'PICM'; VALUE(2) = CumFertP      !P applied (kg/ha)
      LABEL(3) = 'PUPC'; VALUE(3) = CumUptakeP    !Cumul P uptake(kg/ha)
      LABEL(4) = 'SPAM'; VALUE(4) = SPiTotProf    !Soil P at mat (kg/ha)

!     Send labels and values to OPSUM
      CALL SUMVALS (SUMNUM, LABEL, VALUE) 

      IF (IDETP .NE. 'Y' .OR. ISWPHO .EQ. 'N' .OR. IDETL == '0') RETURN

!       Also print on last day if not already done.
        IF (MOD(DAS, FROP) .NE. 0) THEN
          CALL YR_DOY(YRDOY, YEAR, DOY) 

          IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN   ! VSH
          WRITE (LUN,300) YEAR, DOY, DAS, 
     &    SPiTotProf, SPiAvlProf, SPiSolProf, 
     &    SPiLabProf, SPiActProf, SPiStaProf, 
!    &    SPiLabRtsProf, SPiLabNoRtsProf, SPiSolRtsProf,SPiSolNoRtsProf,
     &    CumFertP, CMinerP, CImmobP, CumUptakeP,
     &    SPi_AVAIL(1:5), PUptake(1:5)  !,
!!       Temp variables:
!     &    ProfSta2Act, ProfAct2Sta, ProfAct2Lab, ProfLab2Act, 
!     &    (FracRts(L),L=1,5), (PiSolRts(L),L=1,5), (PiSolNoRts(L),L=1,5)
!     &   ,(PiLabRts(L),L=1,5), (PiLabNoRts(L),L=1,5),
!     &    (SPiSolRts(L),L=1,5), (SPiSolNoRts(L),L=1,5)
!     &   ,(SPiLabRts(L),L=1,5), (SPiLabNoRts(L),L=1,5)
          END IF   ! VSH
        ENDIF

!       Close daily output files.
        CLOSE (LUN)

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************

      RETURN
      END SUBROUTINE OpSoilPi

!=======================================================================
