C=======================================================================
C  TILLAGE, Subroutine, C.H. Porter
C  Tillage routine reads tillage parameters.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  09/17/2001 CHP  Written
C  04/16/2002 GH   Modified for sequence analysis
C  08/01/2002 CHP  Merged RUNINIT and SEASINIT into INIT section
C  08/20/2002 GH   Modified for Y2K
C  08/12/2003 CHP  Added I/O error checking
C  03/03/2006 CHP  Added A. Andales, WDB tillage routines
!  10/25/2007 CHP/GH Changed name of tillage operations file to 
!                     TILOP045.SDA
!  04/30/2008 CHP Path for SDA files set in DSSATPRO file
C-----------------------------------------------------------------------
C  Called : MgmtOps
C  Calls  : 
C=======================================================================

      SUBROUTINE TILLAGE(CONTROL, ISWITCH, SOILPROP,      !Input
     &    TILLVALS, TILLNO)                               !Output

C-----------------------------------------------------------------------
      USE ModuleDefs
      IMPLICIT NONE
      EXTERNAL ERROR, FIND, GETLUN, IGNORE, PATH, WARNING, YR_DOY
      SAVE

      CHARACTER*1 ISWTIL, RNMODE, BLANK
      PARAMETER (BLANK=' ')
      CHARACTER*6 SECTION
      CHARACTER*7,  PARAMETER :: ERRKEY = 'TILLAGE'
      CHARACTER*12 FILETL
      CHARACTER*12 NAMEF
      CHARACTER*30 FILEIO
      CHARACTER*78 MSG(NAPPL)
      CHARACTER*80 PATHSD
      CHARACTER*90 CHAR
      CHARACTER*92 TILFILE

      INTEGER DYNAMIC, ERRNUM, FOUND, I, IDATE, J, L, PFLAG
      INTEGER MULTI, NTIL, YR, YRDIF, YRDOY, YRSIM, RUN, NLAYR
      INTEGER, DIMENSION(NAPPL) :: TILLDATE, NLYRS
      INTEGER LUNIO, LUNTIL, LNUM, ISECT, TILLNO, NTil_today

      REAL FACTOR, TILDEP, TILMIX, TILRESINC
      REAL, DIMENSION(NAPPL) :: CNP, RINP, MIXT, HPAN, TDEP, SSDT
      REAL, DIMENSION(NAPPL, NL) :: DEP, BDP, SWCNP
      CHARACTER*5, DIMENSION(NAPPL) :: TILOP
      CHARACTER*32, DIMENSION(NAPPL) :: TIL_DESC
      LOGICAL FEXIST     !EOF, 

!     Define constructed variable types based on definitions in
!     ModuleDefs.for.
      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH
      TYPE (SoilType)    SOILPROP
      TYPE (TillType)    TILLVALS

      DYNAMIC = CONTROL % DYNAMIC
      RUN     = CONTROL % RUN
      YRDOY   = CONTROL % YRDOY

      NLAYR = SOILPROP % NLAYR

C***********************************************************************
C***********************************************************************
C    Input and Initialization 
C***********************************************************************
      IF (DYNAMIC .EQ. INIT) THEN
C-----------------------------------------------------------------------
      FILEIO  = CONTROL % FILEIO
      LUNIO   = CONTROL % LUNIO
      MULTI   = CONTROL % MULTI
      YRSIM   = CONTROL % YRSIM
      YRDIF   = CONTROL % YRDIF
      RNMODE  = CONTROL % RNMODE
!      ISWWAT  = ISWITCH % ISWWAT
      ISWTIL  = ISWITCH % ISWTIL

      IF (INDEX('YR',ISWTIL) > 0) THEN

      FILETL = 'TILOP' // ModelVerTxt // '.SDA'

C-----------------------------------------------------------------------
C     Read FILEIO
C-----------------------------------------------------------------------
      OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT=ERRNUM)
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,0)

C-----------------------------------------------------------------------
C     Read Tillage Section
C-----------------------------------------------------------------------
      SECTION = '*TILLA'
      CALL FIND(LUNIO, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) CALL ERROR(SECTION, 42, FILEIO, LNUM)
      NTIL  = 0
      DO I = 1,NAPPL
        READ(LUNIO,'(3X,I7,1X,A90)',ERR=30,END=30) TILLDATE(I), CHAR
        LNUM = LNUM + 1

        READ(CHAR,'(A5,1X,F5.0)',IOSTAT=ERRNUM) TILOP(I), TDEP(I) 
        IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY, ERRNUM, FILEIO, LNUM)
        NTIL  = NTIL  + 1
      ENDDO
   30 CONTINUE

      CLOSE (LUNIO)

C-----------------------------------------------------------------------
C     Adjust for crop rotations
C-----------------------------------------------------------------------
      IF (RNMODE .EQ. 'Q' .AND. TILLDATE(1) .LT. YRSIM) THEN
        DO I = 1, NTIL
          CALL YR_DOY(TILLDATE(I),YR,IDATE)
          TILLDATE(I) = (YR + YRDIF) * 1000 + IDATE
        END DO
      ENDIF

C-----------------------------------------------------------------------
C     Adjust for multi year runs
C-----------------------------------------------------------------------
      IF (MULTI .GT. 1) THEN
        DO I = 1, NTIL
          CALL YR_DOY(TILLDATE(I),YR,IDATE)
          TILLDATE(I) = (YR + MULTI - 1) * 1000 + IDATE
        END DO
      ENDIF
      ELSE
        NTIL = 0
      ENDIF

C----------------------------------------------------------------------
C     Read tillage operations file
C----------------------------------------------------------------------
!      IF (ISWWAT .EQ. 'Y' .AND. NTIL .GT. 0) THEN
      IF (NTIL .GT. 0) THEN

C-----------------------------------------------------------------------
C       Open the TILOP???.SDA file 
C-----------------------------------------------------------------------
        TILFILE = FILETL
        INQUIRE (FILE = TILFILE, EXIST = FEXIST)
        IF (.NOT. FEXIST) THEN
          CALL PATH('STD',CONTROL%DSSATP,PATHSD,PFLAG,NAMEF)
          TILFILE = TRIM(PATHSD) // FILETL
        ENDIF

        INQUIRE(FILE=TILFILE, EXIST=FEXIST)
        IF (.NOT. FEXIST) THEN
          !Tillage file doesn't exist in soils directory
          MSG(1) = "Tillage operations file not found:"
          MSG(2) = TILFILE
          MSG(3) = ' Program will stop.'
          CALL WARNING(3, ERRKEY, MSG)
          CALL ERROR(ERRKEY,29,FILETL,0)
        ENDIF

!       Open tillage operation file.
        CALL GETLUN('FILETL', LUNTIL)
        OPEN (UNIT=LUNTIL, FILE=TILFILE, STATUS='OLD', IOSTAT=ERRNUM)
        IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILETL,0)

!       Loop through tillage operations to find tillage operaton code.
        TILLOPS: DO I=1, NTIL
          REWIND (LUNTIL)

          READLOOP: DO WHILE (.TRUE.)    !.NOT. EOF(LUNTIL)
            CALL IGNORE(LUNTIL, LNUM, ISECT, CHAR)
            IF (ISECT == 0) EXIT READLOOP
            IF (CHAR(2:6) .EQ. TILOP(I)) THEN
              READ(CHAR,'(7X,A32)') TIL_DESC(I)
!             Found the right tillage operation, read after-tillage
!             values of BD and SWCN for top 3 soil layers.
              CALL IGNORE(LUNTIL, LNUM, ISECT, CHAR)
              READ(CHAR,'(5F6.0)') 
     &            CNP(I), RINP(I), SSDT(I), MIXT(I), HPAN(I) 
              IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILETL,LNUM)

              LYRLOOP: DO L = 1, NL
                CALL IGNORE(LUNTIL, LNUM, ISECT, CHAR)
                IF (CHAR(1:1) .EQ. '*') EXIT LYRLOOP
                !Tillage changes to BD and SWCN read as percentages
                READ(CHAR,'(3F6.0)') 
     &                DEP(I,L), BDP(I,L), SWCNP(I,L)
                IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILETL,LNUM)
                IF (DEP(I,L) < 0.01) EXIT LYRLOOP
                NLYRS(I) = L

              ENDDO LYRLOOP
              EXIT READLOOP
            ENDIF 
          ENDDO READLOOP 

!         Tillage depth can be no greater than maximum depth read
!           from tillage characteristics file.
!         TDEP(I) = MIN(TDEP(I), DEP(I,NLYRS(I)))
!         Scale depths for implement if user-specified depth is less
          IF (TDEP(I) < DEP(I,NLYRS(I))) THEN
            WRITE(MSG(1),'(A,A)') "User-specified tillage depth ",
     &          "less than that specified by tillage implement."
            CALL YR_DOY(TILLDATE(I),YR,IDATE)
            WRITE(MSG(2),'(A,2I4)')"Tillage operation date: ", YR, IDATE
            WRITE(MSG(3),'(A5,": ",A32)') TILOP(I), TIL_DESC(I)
            WRITE(MSG(4),'(A,F5.1,A)')"Implement depth =       ",
     &                         DEP(I,NLYRS(I)), " cm"
            WRITE(MSG(5),'(A,F5.1,A)')"User-specified depth of ",
     &                         TDEP(I)," cm will be used."
            CALL WARNING(5,ERRKEY,MSG)
            FACTOR = TDEP(I) / DEP(I,NLYRS(I))
            DO J = 1, NLYRS(I)
              DEP(I,J) = DEP(I,J) * FACTOR
            ENDDO
          ELSEIF (TDEP(I) - DEP(I,NLYRS(I)) > 0.5) THEN
            WRITE(MSG(1),'(A,A)') "User-specified tillage depth ",
     &          "greater than that specified by tillage implement."
            CALL YR_DOY(TILLDATE(I),YR,IDATE)
            WRITE(MSG(2),'(A,2I4)')"Tillage operation date: ", YR, IDATE
            WRITE(MSG(3),'(A5,": ",A32)') TILOP(I), TIL_DESC(I)
            WRITE(MSG(4),'(A,F5.1,A)')"User-specified depth =",
     &                         TDEP(I)," cm"
            WRITE(MSG(5),'(A,F5.1,A)')"Implement depth of    ",
     &                         DEP(I,NLYRS(I))," cm will be used."
            CALL WARNING(5,ERRKEY,MSG)
            TDEP(I) = DEP(I,NLYRS(I))
          ENDIF 

        ENDDO TILLOPS  !Tillage operations loop
        CLOSE (LUNTIL)

!!       Echo tillage operations to INFO file
!        MSG(1) = "Tillage operations read from experiment file"
!        MSG(2) = " YEAR DOY CODE  DEP Description"
!        DO I = 1, NTIL
!          CALL YR_DOY(TILLDATE(I),YR,IDATE)
!          WRITE(MSG(I+2),'(I5,I4.3,1X,A5,I4,1X,A)')
!     &          YR, IDATE, TILOP(I), NINT(TDEP(I)), TIL_DESC(I)
!        ENDDO
!        CALL INFO(NTIL+2,ERRKEY,MSG)
      ELSE        !No tillage operations if NTIL <= 0
        NTIL = 0
      ENDIF

      TILLVALS % NTIL      = NTIL
      TILLVALS % TILDATE   = 0
      TILLVALS % TILDEP    = 0.0
      TILLVALS % TILMIX    = 0.0
      TILLVALS % TILRESINC = 0.0
      TILLVALS % NTil_today= 0
      TILLVALS % NLYRS     = 0
      TILLVALS % CNP       = 0.0
      TILLVALS % TDEP      = 0.0
      TILLVALS % BDP       = 0.0
      TILLVALS % DEP       = 0.0
      !TILLVALS % SSDT      = 0.0
      TILLVALS % SWCNP     = 0.0

      TILLNO = 0

!***********************************************************************
!***********************************************************************
!     Daily rate calculations
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!-----------------------------------------------------------------------
      IF (NTIL .LE. 0) RETURN

!     Determine if there are any tillage operations today.
      NTIL_today = 0
      TILDEP = 0.0
      TILMIX = 0.0
      TILRESINC = 0.0
      TILLVALS % TILDATE   = 0

      DO I = 1, NTIL
        IF (YRDOY .EQ. TILLDATE(I) 
     &        .AND. NLYRS(I) .GT. 0 .AND. TDEP(I) .GT. 0.0) THEN
          TILLVALS % TILDATE = YRDOY

!         Total number of tillage events so far this simulation
          TILLNO = TILLNO + 1 

!         Allow up to 3 tillage passes today
          NTIL_today = NTIL_today + 1

!         Save maximum values for today's tillage events for depth, 
!         mixing percent, residue incorporation percent.  These will
!         be used by other modules for soil mixing.
          TILDEP    = MAX(TDEP(I), TILDEP)  
          TILMIX    = MAX(MIXT(I), TILMIX) 
          TILRESINC = MAX(RINP(I), TILRESINC) 

!         Soil properties are updated based on multiple tillage passes
          TILLVALS % TDEP(NTIL_today) = TDEP(I)
          TILLVALS % CNP(NTIL_today)  = CNP(I)
          !TILLVALS % SSDT(NTIL_today) = SSDT(I)
          TILLVALS % NLYRS(NTIL_today) = NLYRS(I)
          DO L = 1, NL
            TILLVALS % DEP(NTIL_today,L)  = DEP(I,L)
            TILLVALS % BDP(NTIL_today,L)  = BDP(I,L)
            TILLVALS % SWCNP(NTIL_today,L)= SWCNP(I,L)
          ENDDO
        ENDIF
      ENDDO

      TILLVALS % TILDEP   = TILDEP
      TILLVALS % TILMIX   = TILMIX
      TILLVALS % TILRESINC= TILRESINC
      TILLVALS % NTIL_today=NTIL_today

C***********************************************************************
C***********************************************************************
C     END OF DYNAMIC IF CONSTRUCT
C***********************************************************************
      ENDIF
C***********************************************************************
      RETURN
      END SUBROUTINE TILLAGE

