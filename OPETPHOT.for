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
  120   FORMAT('@YEAR DOY   DAS',
     &   '    LI%D   PHAD   PHAN    LI%N   SLLN   SLHN',
     &   '   N%LN   N%HN   LMLN   LMHN   TGNN   TGAV')

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
     &        PNLSLN, PNLSHN, LMXSLN, LMXSHN, TGRO(12), TGROAV
 300      FORMAT(1X,I4,1X,I3.3,1X,I5,
     &        F8.2,2(1X,F6.2),F8.2,6(1X,F6.2),2(1X,F6.1))
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
