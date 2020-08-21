C=======================================================================
C  OpETPhot, Subroutine, C.H.Porter from hourly energy balance
C     portions of OPCARB.
C  Generates daily output for ETPHOT routine.
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  02/08/2002 CHP Written
C  08/20/2002 GH  Modified for Y2K
C-----------------------------------------------------------------------
C  Called from:   ETPHOT
C  Calls:         None
C=======================================================================
      SUBROUTINE OpETPhot(CONTROL, ISWITCH,
     &   PCINPD, PG, PGNOON, PCINPN, SLWSLN, SLWSHN,
     &   PNLSLN, PNLSHN, LMXSLN, LMXSHN, TGRO, TGROAV,
     &    Enoon, Tnoon, ETNOON, WINDN, TCANn, CWSHn, CWSLn,        !Output
     &    LSHnn, LSLnn, ETnit, TEMnit, Enit, Tnit, WINnit,!Output
     &    TCnit, TSRnit, TSRFN, CSHnit, CSLnit, LSHnit, LSLnit,
     &    GN, LHN, LHEATN, RSSHN, RSSLN, RSSSN, SHN, SHEATN,
     &    GMT, LHT, LHEATT, RSSHT, RSSLT, RSSST, SHT, SHEATT,
C         previous five output lines added by Bruce Kimball DEC14
     &      TAnn,TAnit,TGROnn,TGROnit,TGRODY,
C           previous line added by Bruce Kimall on 9MAR15
C     &   RBSHN,RBSLN,RBSSN,RBSHT,RBSLT,RBSST,
C       preveious line added by BAK on 10DEC2015
CSVC     &        AGEQESLN, CO2QESLN, QEFFSLN)
     &        AGEQESLN, CO2QESLN, QEFFSLN,
CSVC     &	DAYG,DAYLH,DAYSH,DAYRN) !Output
     &	DAYG,DAYLH,DAYSH,DAYRN,RNn,
     &    PARn,RADn, CISLn, CISHn,C2SHn,C2SLn) !Output
CSVC


C-------------------------------------------------------------------
C
C  ETPHOT OUTPUT File
C
C-------------------------------------------------------------------
      USE ModuleDefs
       ! VSH
      USE CsvOutput
      USE Linklist
      IMPLICIT NONE
      SAVE

      CHARACTER*1 IDETC, RNMODE
      CHARACTER*10 OUTETP

CSVC CHARACTER*1  MEEVP
	CHARACTER*1  MEEVP

      INTEGER DAS, DOY, DYNAMIC, ERRNUM, FROP, NOUTDC
      INTEGER RUN, YEAR, YRDOY, TSV2

      REAL PCINPD,PG, PGNOON, PCINPN
      REAL SLWSLN, SLWSHN, PNLSLN, PNLSHN, LMXSLN, LMXSHN
      REAL TGRO(TS), TGROAV
!           changed from 24 to TS by Bruce Kimball on 9JAN17
      REAL Enoon, Tnoon, ETNOON, WINDN, TCANn, CWSHn, CWSLn,
     &    LSHnn, LSLnn, ETnit, TEMnit, Enit, Tnit, WINnit,
     &    TCnit, TSRnit(3), TSRFN(3), CSHnit, CSLnit, LSHnit, LSLnit,
     &    GN, LHN, LHEATN(3,1), RSSHN, RSSLN, RSSSN, SHN, SHEATN(3,1),
     &    GMT, LHT, LHEATT(3,1), RSSHT, RSSLT, RSSST, SHT, SHEATT(3,1)
C         previous five output lines added by Bruce Kimball DEC14
      Real TGROnn,TGROnit,TAnn,TAnit,TGRODY
C           previous line added by Bruce Kimball on 9MAR15
      Real RBSHN,RBSLN,RBSSN,RBSHT,RBSLT,RBSST,C2SHn,C2SLn
C       preveious line added by BAK on 10DEC2015
      real  AGEQESLN, CO2QESLN, QEFFSLN, RNn, PARn, RADn

CSVC      REAL DAYG,DAYLH,DAYSH,DAYRN !Output
      REAL DAYG,DAYLH,DAYSH,DAYRN,CISHn,CISLn,VPDSL,VPDSH !Output
CSVC
      LOGICAL FEXIST

!-----------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.

      TYPE (ControlType) CONTROL

!     The variable "ISWITCH" is of type "SwitchType".
      TYPE (SwitchType) ISWITCH

      DAS     = CONTROL % DAS
      DYNAMIC = CONTROL % DYNAMIC
      FROP    = CONTROL % FROP
      RNMODE  = CONTROL % RNMODE
      RUN     = CONTROL % RUN
      YRDOY   = CONTROL % YRDOY

      MEEVP  = ISWITCH % MEEVP    ! SVC
      FMOPT = ISWITCH % FMOPT     ! VSH

      IDETC   = ISWITCH % IDETC
      IF (IDETC .NE. 'Y') RETURN

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
        IF (FMOPT == 'A' .OR. FMOPT == ' ' ) THEN       ! VSH
        OUTETP = 'ETPhot.OUT'
        CALL GETLUN(OUTETP, NOUTDC)

        INQUIRE (FILE = OUTETP, EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT = NOUTDC, FILE = OUTETP, STATUS = 'OLD',
     &      IOSTAT = ERRNUM, POSITION = 'APPEND')
        ELSE
          OPEN (UNIT = NOUTDC, FILE = OUTETP, STATUS = 'NEW',
     &      IOSTAT = ERRNUM)
          WRITE(NOUTDC,
     &     '("*EVAPOTRANSPIRATION / PHOTOSYNTHESIS DAILY OUTPUT FILE")')
        ENDIF

        !Write headers
        CALL HEADER(SEASINIT, NOUTDC, RUN)

        WRITE (NOUTDC,120)
  120   FORMAT('@YEAR DOY   DAS',
     &   '    LI%D   PHAD   PHAN    LI%N   SLLN   SLHN',
CSVC     &   '   N%LN   N%HN   LMLN   LMHN   TGON   TGAV')
     &   '   N%LN   N%HN   LMLN   LMHN   TGON   TGAV',
     &   '  CISLn  CISHn  C2SLn  C2SHn  CWSLn  CWSHn',
     &   '   RADn   PARn',     
CSVC     &   '   DAYG   DAYLH  LMLN   DAYSH  DAYRN')
     &   '   DAYG  DAYLH  DAYSH  DAYRN     Gn    LHn',
     &   '    SHn    RNn  TCANn TSRFn1 TSRFn2 TSRFn3')
CSVC
C

        END IF   ! VSH
!***********************************************************************
!***********************************************************************
!     Daily Output
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT .OR. DYNAMIC .EQ. SEASEND) THEN
C-----------------------------------------------------------------------
!       VSH
        IF ((NOUTDC == 0).AND. (FMOPT == 'A'.OR. FMOPT == ' ')) RETURN

        IF ((DYNAMIC .EQ. OUTPUT.AND. MOD(DAS,FROP) .EQ. 0) .OR.
     &      (DYNAMIC .EQ. SEASEND .AND. MOD(DAS,FROP) .NE. 0) .OR.
     &       DAS == 1) THEN

        IF(MEEVP .NE. "Z") then       ! SVC
	     DAYG  =-99.                   ! SVC
         DAYLH =-99.                   ! SVC
	     DAYSH =-99.                   ! SVC
	     DAYRN =-99.                   ! SVC
	     ENDIF                         ! SVC

          CALL YR_DOY(YRDOY, YEAR, DOY)

          IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN      ! VSH
              TSV2 = INT(TS/2)
          WRITE (NOUTDC,300) YEAR, DOY, DAS,
     &        PCINPD, PG, PGNOON, PCINPN, SLWSLN, SLWSHN,
CSVC     &        PNLSLN, PNLSHN, LMXSLN, LMXSHN, TGRO(TS/2), TGROAV
     &        PNLSLN, PNLSHN, LMXSLN, LMXSHN, TGRO(TSV2), TGROAV,
     &        CISLn, CISHn,C2SLn,C2SHn,CWSLn, CWSHn, RADn, PARn,
CSVC     & DAYG,DAYLH,DAYSH,DAYRN
     &     DAYG,DAYLH,DAYSH,DAYRN,Gn,LHn, SHn, RNn,
     &        TCANn,TSRFN(1),TSRFN(2),TSRFN(3)
C         above line added on 4Aug20 BAK
CSVC
C     &    Enoon, Tnoon, ETNOON, , VPDSHn VPDSLn CiSLn CiSHn PPFDn RADHn
C     &    , ETnit, TEMnit, Enit,
C     &    Tnit, WINnit, TCnit, TSRnit, TSRFN,
C     &    , GN, LHN, LHEATN,
C     &    RSSHN, RSSLN, RSSSN, SHN, SHEATN,GMT,
C     &    LHT, LHEATT, RSSHT, RSSLT, RSSST, SHT,
C     &    SHEATT,
C         previous SEVEN output lines added by Bruce Kimball on 2DEC14
C     &    TAnn,TAnit,TGROnn,TGROnit,TGRODY,
C         previous line added by Bruce Kimball on 9MAR15
C     &   RBSHN,RBSLN,RBSSN,RBSHT,RBSLT,RBSST
C       preveious line added by BAK on 10DEC2015

 300      FORMAT(1X,I4,1X,I3.3,1X,I5,
     &      1X,F7.2,1X,F6.2,1X,F7.2,1X,F6.2,1X,F6.2,1X,F6.2,
CSVC     &      1X,F6.2,1X,F6.2,1X,F6.2,1X,F6.2,1X,F6.2,1X,F6.2)
     &      1X,F6.2,1X,F6.2,1X,F6.2,1X,F6.2,1X,F6.2,1X,F6.2,
CSVC     &      1X,F6.2,1X,F6.2,1X,F6.2,1X,F6.2)
     &      1X,F6.3,1X,F6.3,1X,F6.3,1X,F6.3,1X,F6.3,1X,F6.3,
     &      1X,F6.1,1X,F6.1,
CSVC
     &      1X,F6.2,1X,F6.2,1X,F6.2,1X,F6.2,1X,F6.1,1X,F6.1,
     &      1X,F6.1,1X,F6.1,
     &      1X,F6.2,1X,F6.2,1X,F6.2,1X,F6.2)
C     &      1X,F6.3,1X,F6.3,1X,F6.3,1X,F6.3,1X,F6.3,1X,F6.3,
C     &      1X,F6.3,1X,F6.3,1X,F6.3,1X,F6.3,1X,F6.3,1X,F6.3,
C     &      1X,F6.3,1X,F6.3,1X,F6.3,1X,F6.3,1X,F6.3,1X,F6.3,
C     &      1X,F6.2,1X,F6.2,1X,F6.2,1X,F6.2,1X,F6.2,1X,F6.2,
C     &      1X,F6.1,1X,F6.1,1X,F6.1,1X,F6.1,1X,F6.1,1X,F6.1)

          END IF   ! VSH

        !     VSH
      IF (FMOPT == 'C') THEN
         CALL CsvOutETPhot(EXPNAME, CONTROL%RUN, CONTROL%TRTNUM,
     &CONTROL%ROTNUM, CONTROL%REPNO, YEAR, DOY, DAS,
     &PCINPD, PG, PGNOON, PCINPN, SLWSLN, SLWSHN, PNLSLN,
     &PNLSHN, LMXSLN, LMXSHN, TGRO, TGROAV,
     &vCsvlineETPhot, vpCsvlineETPhot, vlngthETPhot)

         CALL LinklstETPhot(vCsvlineETPhot)
      END IF

        ENDIF

        IF ((DYNAMIC .EQ. SEASEND)
     & .AND. ((FMOPT == 'A') .OR. (FMOPT == ' '))) THEN   ! VSH
          CLOSE (NOUTDC)
        ENDIF

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE OpETPhot

C=======================================================================
