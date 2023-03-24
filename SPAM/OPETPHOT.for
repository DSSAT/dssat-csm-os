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
     &   PNLSLN, PNLSHN, LMXSLN, LMXSHN, TGRO, TGROAV)

! 2023/01-26 chp removed unused variables in argument list:
!     &    Enoon, Tnoon, ETNOON, WINDN, TCANn, CSHnn, CSLnn,       !Output
!     &    LSHnn, LSLnn, ETnit, TEMnit, Enit, Tnit, WINnit,   !Output
!     &    TCnit, TSRnit, TSRFN, CSHnit, CSLnit, LSHnit, LSLnit,
!     &    GN, LHN, LHEATN, RSSHN, RSSLN, RSSSN, SHN, SHEATN,
!     &    GMT, LHT, LHEATT, RSSHT, RSSLT, RSSST, SHT, SHEATT,
!     &      TAnn,TAnit,TGROnn,TGROnit,TGRODY,
!     &   RBSHN,RBSLN,RBSSN,RBSHT,RBSLT,RBSST,
!     &        AGEQESLN, CO2QESLN, QEFFSLN)

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
      EXTERNAL GETLUN, HEADER, YR_DOY
      SAVE

      CHARACTER*1 IDETC, RNMODE
      CHARACTER*10 OUTETP

      INTEGER DAS, DOY, DYNAMIC, ERRNUM, FROP, NOUTDC
      INTEGER RUN, YEAR, YRDOY !, TSV2

      REAL PCINPD,PG, PGNOON, PCINPN
      REAL SLWSLN, SLWSHN, PNLSLN, PNLSHN, LMXSLN, LMXSHN
      REAL TGRO(TS), TGROAV
!           changed from 24 to TS by Bruce Kimball on 9JAN17
!      REAL Enoon, Tnoon, ETNOON, WINDN, TCANn, CSHnn, CSLnn,
!     &    LSHnn, LSLnn, ETnit, TEMnit, Enit, Tnit, WINnit,
!     &    TCnit, TSRnit(3), TSRFN(3), CSHnit, CSLnit, LSHnit, LSLnit,
!     &    GN, LHN, LHEATN(3,1), RSSHN, RSSLN, RSSSN, SHN, SHEATN(3,1),
!     &    GMT, LHT, LHEATT(3,1), RSSHT, RSSLT, RSSST, SHT, SHEATT(3,1)
C         previous five output lines added by Bruce Kimball DEC14
!      Real TGROnn,TGROnit,TAnn,TAnit,TGRODY
C           previous line added by Bruce Kimball on 9MAR15
!      Real RBSHN,RBSLN,RBSSN,RBSHT,RBSLT,RBSST
C       preveious line added by BAK on 10DEC2015

!      real  AGEQESLN, CO2QESLN, QEFFSLN


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
     &   '   N%LN   N%HN   LMLN   LMHN   TGON   TGAV')
C
C   Commented out extra variables on 12Jul17 for
C     "publication purposes. Bruce Kimball
C     &   '    ENN    TNN    ETn   WDNN   TCNN   CSHn',
C     &   '   CSLn   LSHn   LSLn   ETnt   TEMt   Enit',
C     &   '   Tnit   WINn   TCnt  TSR1t  TSR2t  TSR3t',
C     &   '  TSR1n  TSR2n  TSR3n   CSHt   CSLt   LSHt',
C     &   '   LSLt     GN    LHN   LH1N   LH2N   LH3N',
C     &   '   RSHN   RSLN   RSSN    SHN   SH1N   SH2N',
C     &   '   SH3N    GMT    LHT   LH1T   LH2T  LHE3T',
C     &   '   RSHT   RSLT   RSST    SHT   SH1T   SH2T',
C     &   '   SH3T   TAnn   TAnt   TG12   TG24   TGDY',
C     &   '  RBSHN  RBSLN   BSSN  RBSHT  RBSLT  RBSST',
C     &   '   N%LN   N%HN   LMLN   LMHN   TGNN   TGAV')
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

          CALL YR_DOY(YRDOY, YEAR, DOY)

          IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN      ! VSH
          WRITE (NOUTDC,300) YEAR, DOY, DAS,
     &        PCINPD, PG, PGNOON, PCINPN, SLWSLN, SLWSHN,
     &        PNLSLN, PNLSHN, LMXSLN, LMXSHN, TGRO(TS/2), TGROAV
C     &    Enoon, Tnoon, ETNOON, WINDN, TCANn, CSHnn,
C     &    CSLnn, LSHnn, LSLnn, ETnit, TEMnit, Enit,
C     &    Tnit, WINnit, TCnit, TSRnit, TSRFN, CSHnit,
C     &    CSLnit, LSHnit, LSLnit, GN, LHN, LHEATN,
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
     &      1X,F6.2,1X,F6.2,1X,F6.2,1X,F6.2,1X,F6.2,1X,F6.2)
C     &      1X,F6.2,1X,F6.2,1X,F6.2,1X,F6.2,1X,F6.1,1X,F6.1,
C     &      1X,F6.3,1X,F6.3,1X,F6.3,1X,F6.3,1X,F6.3,1X,F6.3,
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
