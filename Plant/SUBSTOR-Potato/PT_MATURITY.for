      SUBROUTINE PT_MATURITY(CONTROL,
     &    TUBWT, PLTPOP, DTT, STT,                !Input
     &    MDATE, ISDATE, WMAX)                    !Output
      USE ModuleDefs
      IMPLICIT NONE
      SAVE
      REAL TUBWT, PLTPOP
      REAL YIELD, FRYLD
      REAL DTT, STT, TDELTA
      REAL W, WB, WPREV, WDELTA
      REAL CM, RM, SLOPE
      REAL G2, G3, WMAX
      REAL, PARAMETER :: THRESH = 0.1
      LOGICAL MAXYIELDED, TUBINITFOUND
      INTEGER YRDOY, MDATE, ISDATE
      TYPE (ControlType) CONTROL

      YRDOY   = CONTROL % YRDOY

      CALL PT_IPTUBERRT(CONTROL, G2, G3)  !Output

      ! CM = 23.2 ! Growth rate in the linear phase, g/m^2
      CM = G3
      RM = 0.34 ! Relative growth rate in exponential phase g/m^2

      FRYLD = (TUBWT*10.*PLTPOP/1000.)/0.2   ! Fresh yield
      YIELD  = TUBWT*10.*PLTPOP               ! Dry yield

!     FRYLD is Mg/Ha while while CM and RM are in g/m^2
!     Multiply by 0.01 for g/m^2 to Mg/Ha (1Ha = 10000 m^2,
!     1Mg=1000000g)
      ! FRYLDB = ((CM/RM)*log(2.0)) * 0.01;   ! WB
      WB = ((CM/RM)*log(2.0)) * 0.01;         ! WB

      !W = FRYLD   ! uncommented: based on fresh yield
      W = YIELD    ! uncommented: based on dry yield
      WDELTA = 0.0
      SLOPE = 0.0
      !TDELTA = DTT
      TDELTA = STT

      !IF (.NOT. TB_STILL_NOT_FOUND .AND. W .GE. WB) THEN !Find the first point when W greater than or equal to Wb
      IF (.NOT. TUBINITFOUND .AND. W .GE. WB) THEN !Finds Tb
          TUBINITFOUND = .TRUE.
          ISDATE = YRDOY !ISDATE means tubr inititation date
      ENDIF

      !IF (.NOT. TE_STILL_NOT_FOUND) THEN !Keep calculating if Te not found
      IF (.NOT. MAXYIELDED) THEN
        IF (WPREV .GT. 0.0 .AND. TDELTA .GT. 0.) THEN ! Avoid division by 0
          WDELTA = ABS(W - WPREV)                     !Y2-Y1
          SLOPE = WDELTA / TDELTA                     !Y2-Y1/X2-X1
          IF (SLOPE .LT. THRESH .AND. W .GT. WB) THEN  !slope can be nearly 0 at two inflection points: at WB and at WMAX (finds TE)
            MAXYIELDED = .TRUE.
            ! Print for testing
!            PRINT 101, "WMAX:", FRYLD, ", G2: ", G2, ", G3: ", G3
! 101        FORMAT (A5, F6.2, A6, F6.2, A6, F6.2)
            MDATE = YRDOY
            WMAX = W
          END IF
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
