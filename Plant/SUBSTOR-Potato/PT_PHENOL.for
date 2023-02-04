C=======================================================================
C  PT_PHENOL, Subroutine
C
C  Determines phenological stage
C-----------------------------------------------------------------------
C  Revision history
C
C                 Written
C  02/07/1993 PWW Header revision and minor changes
C  08/27/2001 CHP Modified for modular format.
C  06/11/2002 GH  Modified for Y2K
C=======================================================================

      SUBROUTINE PT_PHENOL (
     &    DLAYR, FILEIO, GRAINN, ISWWAT, LL, MDATE, NLAYR,!Input
     &    NSTRES, PLTPOP, RTWT, ST, SW, SWFAC, TMAX, TMIN,!Input
     &    TOPSN, TWILEN, XLAI, YRDOY, YRPLT, YRSIM,       !Input
     &    APTNUP, CUMDTT, DTT, GNUP, GRORT, ISDATE,       !Output
     &    ISTAGE, MAXLAI, PLANTS, RTF, SEEDRV,            !Output
     &    STGDOY, STT, TOTNUP, XSTAGE, YREMRG,            !Output
     &    DYNAMIC)

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT  NONE
      EXTERNAL PT_IPPHEN, YR_DOY, PT_THTIME, PT_PHASEI
      SAVE

      LOGICAL   COND, EMERGE

      CHARACTER*1  ISWWAT
      CHARACTER*2  CROP
      CHARACTER*30 FILEIO

      INTEGER DOY, DYNAMIC, IEMRG, ISDATE, ISTAGE, L0, I, L
      INTEGER MDATE, NDAS, NLAYR, YEAR, YRDOY, YRPLT, YRSIM
      INTEGER YREMRG
      INTEGER STGDOY(20)

      REAL APTNUP, CTII, CUMDTT, CUMSTT, DTT
      REAL GNUP, GRAINN, GRF, GRORT, GROSPR, XLAI, MAXLAI
      REAL NSTRES, P2, TWILEN, PLANTS, PLTPOP
      REAL RDLF, RTF, RTWT, SDEPTH, STT, SWFAC, SEEDAV
      REAL SEEDRV, SENLA, SPGROF, SPRLAP, SPRLTH, SPRWT, SWSD
      REAL TC, TCPLUS, TEMP, TII, TMAX, TMIN, TOPSN, TOTNUP, TSPRWT
      REAL XDEPTH, XDTT, XPLANT, XSTAGE

      REAL, DIMENSION(NL) :: DLAYR, LL, ST, SW

!***********************************************************************
!***********************************************************************
!     Seasonal Initialization - Called once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      CALL PT_IPPHEN(
     &    FILEIO, 
     &    CROP, IEMRG, P2, PLANTS, SDEPTH, SPRLAP, TC, TCPLUS)

      ISTAGE = 5
      XSTAGE = 0.0

      DO I = 1, 20
         STGDOY(I) = 9999999
      END DO

      STGDOY(14) = YRSIM
      XPLANT     = PLANTS

!     From main program CHP 08/27/01
      SPGROF = (.167*(1.0-.82*EXP(-.065*SPRLAP)))/.0344
!      S1     = SIN (XLAT*0.01745)
!      C1     = COS (XLAT*0.01745)

!     From INPLNT
      APTNUP = 0.0
      CUMDTT = 0.0
      CUMSTT = 0.0
      DTT    = 0.0
      GNUP   = 0.0
      MAXLAI = 0.0
      RTF    = 0.0
      SENLA  = 0.0
      TOTNUP = 0.0
      ISDATE = 0

!***********************************************************************
!***********************************************************************
!     Daily rate calculations
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!-----------------------------------------------------------------------
      CALL YR_DOY (YRDOY, YEAR, DOY)

!      XANC   = TANC*100.0
      APTNUP = TOPSN*10.0*PLANTS
      TOTNUP = APTNUP

      IF (ISTAGE .NE. 5) THEN
         CALL PT_THTIME (  
     &      ISTAGE, L0, ST, TMAX, TMIN,                   !Input
     &      DTT, STT)                                     !Output

         CUMDTT = CUMDTT + DTT            ! Update thermal time
         CUMSTT = CUMSTT + STT
      END IF

      IF (ISTAGE .LT. 3) THEN             ! Relative temp. factor
          TEMP = TMIN*0.75 + TMAX*0.25    ! post-emergent for
          IF (TEMP .LE. 4.0) THEN         ! tuber inititation
              RTF = 0.0
           ELSEIF (TEMP .GT. 4.0 .AND. TEMP .LE. 10.0) THEN
              RTF = 1. - (1./36.)*(10.0-TEMP)**2
              RTF = AMAX1 (RTF,0.0)
           ELSEIF (TEMP .GT. 10.0 .AND. TEMP .LE. TC) THEN
              RTF = 1.0                           

!          Externalize TCPlus variable (was hardwired to 8.0)
           ELSEIF (TEMP .GT. TC .AND. TEMP .LE. TC+8.0) THEN
!           ELSEIF (TEMP .GT. TC .AND. TEMP .LE. TC+TCPLUS) THEN
!             Use linear function between TC and TCPLUS
              RTF = 1.0 - (1./64.)*(TEMP-TC)**2 
!             original QUADRATIC FUNCTION
!             RTF = 1.0 - (TEMP - TC)/TCPLUS    !linear function 
              RTF = AMAX1 (RTF,0.0)
           ELSE
              RTF = 0.0
          END IF
      END IF

C-----------------------------------------------------------------------
      SELECT CASE (ISTAGE)
        !
        ! ISTAGE 5: pre-planting
        ! ISTAGE 6: planting to germination
        ! ISTAGE 7: germination to emergence
        ! ISTAGE 1: vegetative to initiation
        ! ISTAGE 2: initiation to maturity
        !
C-----------------------------------------------------------------------
        CASE (5)
          !
          ! Planting Date
          !
          IF (YRDOY .EQ. YRPLT) THEN
              SPRLTH = 0.0
              NDAS   = 0
              PLANTS = XPLANT
              XDEPTH = 0.0

              DO L = 1, NLAYR
                 XDEPTH = XDEPTH + DLAYR(L)
                 IF (SDEPTH .LT. XDEPTH) EXIT
              END DO

              L0 = L         ! sets layer for determining soil T effect

              STGDOY(ISTAGE) = YRDOY
              CALL PT_PHASEI (
     &         ISTAGE, CUMDTT, XPLANT, SPRLAP,            !I/O
     &         CTII, CUMSTT, MAXLAI, SENLA, TSPRWT, XDTT) !Output
          END IF
C-----------------------------------------------------------------------
        CASE (6)
          !
          ! Germination Date
          !
          COND = .TRUE.
          NDAS = NDAS + 1

          IF (NDAS .GE. 30) THEN
             COND = .FALSE.
             !
             ! Force maturity
             !
             ISTAGE = 2
             STGDOY(ISTAGE) = YRDOY
             CALL PT_PHASEI ( 
     &         ISTAGE, CUMDTT, XPLANT, SPRLAP,            !I/O
     &         CTII, CUMSTT, MAXLAI, SENLA, TSPRWT, XDTT) !Output
          END IF

          IF (ISWWAT .EQ. 'Y') THEN
             IF (SW(L0) .GT. LL(L0)) THEN
                SWSD = (SW(L0)-LL(L0))*0.65+(SW(L0+1)-LL(L0+1))*0.35
                IF (SWSD .LT. 0.02) THEN
                   COND = .FALSE.
                ENDIF
             END IF
          END IF

          IF (COND .AND. CUMSTT .GE. 7.35) THEN
             STGDOY(ISTAGE) = YRDOY
             CALL PT_PHASEI ( 
     &         ISTAGE, CUMDTT, XPLANT, SPRLAP,            !I/O
     &         CTII, CUMSTT, MAXLAI, SENLA, TSPRWT, XDTT) !Output
          END IF
C-----------------------------------------------------------------------
        CASE (7)
          !
          ! Seedling Emergence Date
          !
          NDAS = NDAS + 1
          IF (SPRLAP .EQ. 0.0) THEN
             GROSPR = 28.4*STT           ! Sadler (1961), cited in
           ELSE                          ! Moorby & Milthorpe (1975)
             GROSPR = 0.284*STT*SPGROF
          END IF

          SPRWT  = GROSPR*.0272*STT
          GRORT  = SPRWT

          SEEDAV = STT*1.5               ! SEEDAV not > 1.5 g/pl/d

          IF (GRORT+SPRWT .LE. SEEDAV) THEN
!S/B??    IF (GRORT+SPRWT .LE. SEEDRV) THEN
             SEEDRV = SEEDRV - (GRORT + SPRWT)
           ELSE
             GRF    = SEEDAV/(GRORT + SPRWT) ! GRF = growth reduction
             GRORT  = GRF   * GRORT
             SPRWT  = GRF   * GRORT
             GROSPR = GRF   * GROSPR
          END IF

          SPRLTH = SPRLTH + GROSPR         ! Daily update
          TSPRWT = SPRWT  + TSPRWT
          RTWT   = RTWT   + GRORT

          EMERGE  = .FALSE.
          !
          ! Was 0, changed to -99 - WTB, 06/26/97
          !
          IF (IEMRG .EQ. -99) THEN         ! Determine if emergence
             IF (SPRLTH .GE. SDEPTH) THEN  ! has occurred
                EMERGE = .TRUE.
             END IF
           ELSE
             IF (DOY .EQ. IEMRG) THEN
                EMERGE = .TRUE.
             END IF
          END IF

          IF (EMERGE) THEN                  ! If emerged, then...
             STGDOY(ISTAGE) = YRDOY
             YREMRG = YRDOY       !CHP 12/4/01
             XSTAGE = 1.0
             DTT    = STT
             CALL PT_PHASEI ( 
     &         ISTAGE, CUMDTT, XPLANT, SPRLAP,            !I/O
     &         CTII, CUMSTT, MAXLAI, SENLA, TSPRWT, XDTT) !Output
          END IF

C-----------------------------------------------------------------------
        CASE (1)
          ! Vegetative growth.
          ! Determine when Tuber Growth begins
          !
          MAXLAI = AMAX1 (MAXLAI,XLAI)      ! Maximum XLAI season
          NDAS   = NDAS + 1

!          DEC    = 0.4093 * SIN(0.0172 * (DOY - 82.2))
!          DLV    = (-S1 * SIN(DEC) - 0.1047) / (C1 * COS(DEC))
!          DLV    = AMAX1 (DLV,-0.87)
!
!          TWILEN  = 7.639 * ACOS(DLV)       ! TWILEN=f(latitude, DOY)

          IF (TWILEN .LE. 12.0) THEN        ! RDLF=relative daylength
             RDLF = 1.0                    ! factor; varies by cv.
           ELSE
             RDLF = (1.-P2) + (P2/144.)*(24.-TWILEN)**2
             RDLF = AMAX1 (RDLF,0.0)
          END IF
          !
          ! TII=tuber induction index modified by RTF, RDLF, & 
          !   deficit factors
          !
          TII    = RDLF*RTF + 0.5*(1.0-AMIN1(SWFAC, NSTRES))
          CTII   = CTII + TII
          XSTAGE = 1.0  + CTII/20.0

          IF (CTII .GE. 20.0) THEN         ! Initiation at CTII=20
             STGDOY(ISTAGE) = YRDOY
             ISDATE         = YRDOY
             CALL PT_PHASEI ( 
     &         ISTAGE, CUMDTT, XPLANT, SPRLAP,            !I/O
     &         CTII, CUMSTT, MAXLAI, SENLA, TSPRWT, XDTT) !Output
             IF (ISWWAT .NE. 'N') THEN
                ! XANC = TANC*100.0
             ENDIF
          END IF
C-----------------------------------------------------------------------
        CASE (2)
          ! Tuber growth.
          ! Determine Maturity Date
          !
          XSTAGE = 2.0 + (CUMDTT-XDTT)/100.0
          NDAS   = NDAS + 1
          MAXLAI = AMAX1 (MAXLAI,XLAI)      ! Maximum XLAI season
          GNUP   = GRAINN*PLTPOP*10.0

!         modified by modified by RR 02/15/2016
!         IF (XLAI .LT. 0.1*MAXLAI .OR. YRDOY .EQ. MDATE) THEN !Original function
          IF (XLAI .LT. 0.01*MAXLAI .OR. YRDOY .EQ. MDATE) THEN 
             STGDOY(ISTAGE) = YRDOY
             CALL PT_PHASEI (
     &         ISTAGE, CUMDTT, XPLANT, SPRLAP,            !I/O
     &         CTII, CUMSTT, MAXLAI, SENLA, TSPRWT, XDTT) !Output
             IF (ISWWAT .NE. 'N') THEN
                ! XANC = TANC*100.0
             ENDIF
          END IF      
        CASE DEFAULT
          STOP 'Illegal value for ISTAGE in PHENOL module'
          END SELECT
!       open (unit = 7, file = "c:\\visual\\SLFTTMAXOutput.txt")
!        write (7, *) DTT,CUMDTT, XDTT,YRDOY ! print in file
          
!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE PT_PHENOL


C=======================================================================
C  PT_IPPHEN, Subroutine
C
C  Input for potato phenology
C-----------------------------------------------------------------------
C  Revision history
C
C  08/28/2001 CHP Written
C  08/12/2003 CHP Added I/O error checking
C=======================================================================

      SUBROUTINE PT_IPPHEN(
     &    FILEIO, 
     &    CROP, IEMRG, P2, PLANTS, SDEPTH, SPRLAP, TC, TCPLUS)

C-----------------------------------------------------------------------
      IMPLICIT NONE
      EXTERNAL GETLUN, FIND, ERROR

      INTEGER LUNIO
      CHARACTER*1, PARAMETER :: BLANK = ' '
      CHARACTER*6, PARAMETER :: ERRKEY = 'PHENPT '

      CHARACTER*2   CROP
      CHARACTER*6   SECTION
      CHARACTER*30  FILEIO

      INTEGER ERR, FOUND, LINC, LNUM, IEMRG

      REAL P2, PLANTS, SDEPTH, SPRLAP, TC, TCPLUS

!-----------------------------------------------------------------------
!     Read data from FILEIO for use in phenology module
      CALL GETLUN('FILEIO', LUNIO)
      OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,0)
      LNUM = 0

C    Read Crop Code from Cultivars Section
      SECTION = '*CULTI'
      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILEIO, LNUM)
      ELSE
        READ (LUNIO,'(3X,A2)', IOSTAT=ERR) CROP ; LNUM = LNUM + 1
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
      ENDIF

C    Read Planting Details Section
      SECTION = '*PLANT'
      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILEIO, LNUM)
      ELSE
        READ (LUNIO,'(15X,I3,1X,F5.1,31X,F5.1,25X,F5.0)', IOSTAT=ERR) 
     &            IEMRG, PLANTS, SDEPTH, SPRLAP ; LNUM = LNUM + 1
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
      ENDIF

C     Read crop genetic information
      SECTION = '*CULTI'
      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILEIO, LNUM)
      ELSE
        IF (INDEX ('PT',CROP) .GT. 0) THEN
          READ (LUNIO,'(49X,3F6.0)', IOSTAT=ERR) P2, TC, TCPLUS
          LNUM = LNUM + 1
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
        ENDIF
      ENDIF

      CLOSE (LUNIO)

C-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE PT_IPPHEN
C=======================================================================

