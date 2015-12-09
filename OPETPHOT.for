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
     &    Enoon, Tnoon, ETNOON, WINDN, TCANn, CSHnn, CSLnn,        !Output
     &    LSHnn, LSLnn, ETnit, TEMnit, Enit, Tnit, WINnit,!Output
     &    TCnit, TSRnit, TSRFN, CSHnit, CSLnit, LSHnit, LSLnit,
     &    GN, LHN, LHEATN, RSSHN, RSSLN, RSSSN, SHN, SHEATN,
     &    GMT, LHT, LHEATT, RSSHT, RSSLT, RSSST, SHT, SHEATT,
C         previous five output lines added by Bruce Kimball DEC14
     &      TAnn,TAnit,TGROnn,TGROnit,TGRODY)
C           previous line added by Bruce Kimall on 9MAR15
C-------------------------------------------------------------------
C
C  ETPHOT OUTPUT File
C
C-------------------------------------------------------------------
      USE ModuleDefs
      IMPLICIT NONE
      SAVE

      CHARACTER*1 IDETC, RNMODE
      CHARACTER*10 OUTETP

      INTEGER DAS, DOY, DYNAMIC, ERRNUM, FROP, NOUTDC
      INTEGER RUN, YEAR, YRDOY

      REAL PCINPD,PG, PGNOON, PCINPN
      REAL SLWSLN, SLWSHN, PNLSLN, PNLSHN, LMXSLN, LMXSHN
      REAL TGRO(24), TGROAV
      REAL Enoon, Tnoon, ETNOON, WINDN, TCANn, CSHnn, CSLnn,
     &    LSHnn, LSLnn, ETnit, TEMnit, Enit, Tnit, WINnit,
     &    TCnit, TSRnit(3), TSRFN(3), CSHnit, CSLnit, LSHnit, LSLnit,
     &    GN, LHN, LHEATN(3,1), RSSHN, RSSLN, RSSSN, SHN, SHEATN(3,1),
     &    GMT, LHT, LHEATT(3,1), RSSHT, RSSLT, RSSST, SHT, SHEATT(3,1)
C         previous five output lines added by Bruce Kimball DEC14
      Real TGROnn,TGROnit,TAnn,TAnit,TGRODY
C           previous line added by Bruce Kimball on 9MAR15

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

      IDETC   = ISWITCH % IDETC
      IF (IDETC .NE. 'Y') RETURN

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
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
  120   FORMAT(' @YEAR DOY   DAS',
     &   '    LI%D   PHAD   PHAN    LI%N   SLLN   SLHN',
     &   '   N%LN   N%HN   LMLN   LMHN   TGON   TGAV',
     &   '   ENN    TNN    ETn    WDNN   TCNN    CSHn         CSLn    ',
     &   ' LSHn   LSLn   ETnt   TEMt   Enit   Tnit   WINn   TCnt',
     &   '   TSR1t  TSR2t TSR3t   TSR1n  TSR2n TSR3n',
     &   '      CSHt        CSLt     LSHt   LSLt  ', 
     &   '   GN      LHN        LH1N    LH2N    LH3N     RSHN',
     &   '      RSLN     RSSN      SHN      SH1N    SH2N    SH3N',
     &   '   GMT      LHT      LH1T    LH2T    LHE3T     RSHT',
     &   '      RSLT    RSST     SHT     SH1T    SH2T    SH3T',
     &   '      TAnn    TAnt     TG12    TG24    TGDY')


!***********************************************************************
!***********************************************************************
!     Daily Output
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT .OR. DYNAMIC .EQ. SEASEND) THEN
C-----------------------------------------------------------------------
        IF (NOUTDC == 0) RETURN

        IF ((DYNAMIC .EQ. OUTPUT.AND. MOD(DAS,FROP) .EQ. 0) .OR.
     &      (DYNAMIC .EQ. SEASEND .AND. MOD(DAS,FROP) .NE. 0) .OR. 
     &       DAS == 1) THEN 

          CALL YR_DOY(YRDOY, YEAR, DOY) 

          WRITE (NOUTDC,300) YEAR, DOY, DAS, 
     &        PCINPD, PG, PGNOON, PCINPN, SLWSLN, SLWSHN, 
     &        PNLSLN, PNLSHN, LMXSLN, LMXSHN, TGRO(12), TGROAV,
     &    Enoon, Tnoon, ETNOON, WINDN, TCANn, CSHnn, CSLnn,
     &    LSHnn, LSLnn, ETnit, TEMnit, Enit, Tnit, WINnit,
     &    TCnit, TSRnit, TSRFN, CSHnit, CSLnit, LSHnit, LSLnit,
     &    GN, LHN, LHEATN, RSSHN, RSSLN, RSSSN, SHN, SHEATN,
     &    GMT, LHT, LHEATT, RSSHT, RSSLT, RSSST, SHT, SHEATT,
C         previous FIVE output lines added by Bruce Kimball on 2DEC14
     &    TAnn,TAnit,TGROnn,TGROnit,TGRODY
C         previous line added by Bruce Kimball on 9MAR15
 300     FORMAT(1X,I4,1X,I3.3,1X,I5,
     &      F8.2,2(1X,F6.2),F8.2,6(1X,F6.2),2(1X,F6.1),
     &      5(1x,F6.2),2(1x,E11.4),14(1x,F6.3),2(1x,E11.4),2(1x,F6.3),
     &      5(1x,F8.3),19(1x,E11.4),
     &       5(1x,F8.3))
        ENDIF

        IF (DYNAMIC .EQ. SEASEND) THEN
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
