C=======================================================================
C  PT_MATURITY, Subroutine
C
C  Determines plant physiological maturity
C-----------------------------------------------------------------------
C  Revision history
C
C               Written
C  12/07/2023 MSKhan added the subject sub-routine following 
C  Khan et al., 2023 
C=======================================================================
      
      SUBROUTINE PT_MATURITY(CONTROL,
     &    TUBWT, PLTPOP, DTT, STT,                !Input
     &    MDATE, TB, WB, WMAX)               !Output
      
C-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.  
      IMPLICIT NONE
      SAVE
      
      INTEGER DYNAMIC, YRDOY, MDATE, TB
      
      REAL TUBWT, PLTPOP
      REAL YIELD, FRYLD
      REAL DTT, STT, TDIFF
      REAL W, WB, WPREV, WDIFF
      REAL CM, RM, WDIFFRATE
      REAL G2, G3, WMAX, TUBDM
      REAL, PARAMETER :: THRESH = 0.05
      LOGICAL FOUND_WMAX
      
      TYPE (ControlType) CONTROL
      
      
      YRDOY  = CONTROL % YRDOY
      DYNAMIC = CONTROL % DYNAMIC

      FRYLD = 0.0
      YIELD = 0.0
      W = 0.0
      WDIFF = 0.0
      WDIFFRATE = 0.0

      ! Re-initialise before each new season simulation
      if (DYNAMIC .EQ. SEASINIT) THEN
          FOUND_WMAX = .FALSE.
          WMAX = 0.0
          WPREV = 0.0
          TB = 0
          MDATE = 0
          ! Read the rates G2 and G3 from cultivar file
          CALL PT_IPTUBERRT(CONTROL, G2, G3)
          RETURN
      ENDIF

      ! CM = 23.2 ! Growth rate in the linear phase, g/m^2 day
      CM = G3
      RM = 0.34 ! Relative growth rate in exponential phase g/m^2
      TUBDM = 0.2 !Default value = 0.2

      FRYLD = (TUBWT*10.*PLTPOP/1000.)/TUBDM   ! Fresh yield, from PT_OPGROW L.No. 273
      YIELD  = TUBWT*10.*PLTPOP              !  Dry yield, from PT_SUBSTOR L.No. 302

!     FRYLD is Mg/Ha while while CM and RM are in g/m^2
!     Multiply by 0.01 for g/m^2 to Mg/Ha (1Ha = 10000 m^2,
!     1Mg=1000000g)
      ! FRYLDB = ((CM/RM)*log(2.0)) * 0.01   ! WB
      WB = ((CM/RM)*log(2.0)) * 0.01          ! WB

      W = FRYLD   ! uncommented: based on fresh yield
      !W = YIELD  ! uncommented: based on dry yield
      !TDIFF = DTT
      TDIFF = STT !Reason? REF to PT_GROSUB L.No 515; 628)

      ! Find first point (Tb) where W greater than or equal to Wb
      IF (.NOT. FOUND_WMAX .AND. W .GE. WB) THEN
        IF (TB .EQ. 0) THEN 
        TB = YRDOY !ISDATE means tuber inititation date

      ENDIF

        ! Approximate the derivative/rate of change of the tuber weight
        ! using finite difference method. The point where derivate becomes
        ! almost 0 (tunable via threshold) is the point where tuber weight
        ! becomes maximum, indicating maturity
        IF (WPREV .GT. 0.0 .AND. TDIFF .GT. 0.0) THEN ! Avoid division by 0
          WDIFF = ABS(W - WPREV)                      !Y2-Y1
          WDIFFRATE = WDIFF / TDIFF                   !Y2-Y1/X2-X1
          IF (WDIFFRATE .LE. THRESH) THEN
            FOUND_WMAX = .TRUE.
            MDATE = YRDOY
            WMAX = W
          ENDIF
        END IF
        WPREV = W
      END IF

      RETURN
      END SUBROUTINE PT_MATURITY

      SUBROUTINE PT_IPTUBERRT(CONTROL,
     &    G2, G3)                                    !Output
      USE ModuleDefs
      IMPLICIT NONE
      EXTERNAL GETLUN, FIND
      REAL G2, G3
      INTEGER LUNIO, ERR, LINC, LNUM, FOUND
      TYPE (ControlType) CONTROL
      CHARACTER*30  FILEIO
      CHARACTER*6  SECTION
      CHARACTER*2   CROP
      CHARACTER*6, PARAMETER :: ERRKEY = 'MATURITY'
    
      FILEIO = CONTROL % FILEIO

      CALL GETLUN('FILEIO', LUNIO)

      OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT=ERR)

      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,0)

      ! Find and discard the first cultivar section
      SECTION = '*CULTI'

      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILEIO, LNUM)
      ELSE
        READ (LUNIO,'(3X,A2)', IOSTAT=ERR) CROP ; LNUM = LNUM + 1
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
      ENDIF
      
      ! Find the second cultivar section containing G3
      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILEIO, LNUM)
      ELSE
        IF (INDEX ('PT',CROP) .GT. 0) THEN
          READ (LUNIO,'(31X,2F6.0)',IOSTAT=ERR) G2, G3
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
        ENDIF
      ENDIF

      CLOSE(LUNIO)
 
      RETURN
      END SUBROUTINE PT_IPTUBERRT
