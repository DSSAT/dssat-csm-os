C=======================================================================
C  DORMANCY Subroutine 6/20/03 SJR 
C  Fall Dormancy with cold hardening for perennial grasses and legumes
C  Separate functions for effects on partitioning, photosynthesis, mobilization
C  Generate a reduction factor for each for use in appropriate modules
C  Factor is 0-1 value adjusted for cultivar sensitivity to daylength
C  Cold hardening lowers the minimum survivable temperature for the crop
C  with increased exposure to low temperatures.
C  This subroutine also provides a death rate to allow partial 
C  or total depletion of the stand by a freeze event
C----------------------------------------------------------------------
C  Called by: CROPGRO
C  Calls    : None
C=======================================================================

      SUBROUTINE FOR_DORMANCY( CONTROL,
     &    DAYL, TMIN,                                       !Input
     &    DRMST, FREEZ2, FRZDC, PPGFAC, PPTFAC, PPMFAC,     !Output
     &  FNPGD, FNPMD, FNPTD, FRZDHD, FRZHRD, HARD1,         !Output
     &  HARD2, RCHDP, RDRMG, RDRMM, RDRMT, TYPDHD, TYPHRD,  !Output
     &  TYPPGD, TYPPMD, TYPPTD)                             !Output
     
      USE ModuleDefs     !Definitions of constructed variable types, 
        ! which contain control information, soil
        ! parameters, hourly weather data.

C-----------------------------------------------------------------------
      IMPLICIT NONE
      EXTERNAL FIND, IGNORE, ERROR, CURV
      SAVE
C-----------------------------------------------------------------------
      CHARACTER*1   BLANK
      CHARACTER*3   TYPPGD,TYPPTD, TYPPMD, TYPHRD, TYPDHD
      CHARACTER*6   SECTION, ECOTYP, ECONO, ERRKEY, DRMST
      CHARACTER*12  FILEIO, FILEC, FILEE
      CHARACTER*16  ECONAM
      CHARACTER*80  PATHCR, PATHEC, CHAR


      CHARACTER*92  FILECC, FILEGC
      CHARACTER*255 C255
      
!      INTEGER RUNINIT, SEASINIT, RATE, EMERG, INTEGR, OUTPUT, SEASEND, 
!     $                  DYNAMIC
!      PARAMETER (RUNINIT = 1, SEASINIT = 2, EMERG = 3, RATE = 3,
!     &            INTEGR = 4, OUTPUT=5, SEASEND = 6)

      INTEGER DYNAMIC, II, LUNIO, LUNECO
      INTEGER LUNCRP, ISECT, ERRNUM, PATHL
      INTEGER ERR, LINC, LNUM, FOUND

      PARAMETER (BLANK = ' ')
      PARAMETER (ERRKEY = 'DORMAN')

      PARAMETER (LUNIO=21, LUNCRP=10, LUNECO=10)

      REAL DAYL, CURV, TMIN
      REAL PPGFAC, PPTFAC, PPMFAC, RDRMT, RDRMG, RDRMM
      REAL FNPGD(4), FNPTD(4), FNPMD(4), FRZHRD(4), FRZDHD(4)
      REAL DAYLY, RCHDP
      REAL DHARDR, FREEZ2, FRZDC, HARD1, HARD2, HARDR


C     The variable "CONTROL" is of type "ControlType".

      TYPE (ControlType) CONTROL
!      TYPE (SwitchType) ISWITCH
C     Transfer values from constructed data types into local variables.
      DYNAMIC = CONTROL % DYNAMIC
      FILEIO  = CONTROL % FILEIO
!      LUNIO   = CONTROL % LUNIO
!      LUNECO  = CONTROL % LUNECO

C***********************************************************************
C     Run Initialization - Called once per simulation
C***********************************************************************

      IF (DYNAMIC .EQ. RUNINIT) THEN

C-----------------------------------------------------------------------
C     Initialize Dormancy variables
C-----------------------------------------------------------------------
     
      PPGFAC = 0.0
      PPTFAC = 0.0
      PPMFAC = 0.0
      DRMST = 'NODORM'
      DAYLY = 0.0
      RDRMT = 1.0
      RDRMG = 1.0
      RDRMM = 1.0


C-----------------------------------------------------------------------
C     Read in values from temporary file, which were previously input
C       in Subroutine IPIBS.
C-----------------------------------------------------------------------
      OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT=ERRNUM)
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,0)

      READ (LUNIO,100) FILEC, PATHCR
100   FORMAT(//////,15X,A12,1X,A80)

      READ (LUNIO,105) FILEE, PATHEC
105   FORMAT(15X,A12,1X,A80)
C-----------------------------------------------------------------------
C     Subroutine FIND finds appropriate SECTION in a file by
C     searching for the specified 6-character string at beginning
C     of each line.
C-----------------------------------------------------------------------
        SECTION = '*HARVE'
        CALL FIND(LUNIO, SECTION, LNUM, FOUND)
        IF (FOUND .EQ. 0) CALL ERROR (ERRKEY,2,FILEIO,LNUM)

C-----------------------------------------------------------------------
C     Find and read Cultivar Section
C-----------------------------------------------------------------------
        SECTION = '*CULTI'
        CALL FIND(LUNIO, SECTION, LNUM, FOUND)
        IF (FOUND .EQ. 0) CALL ERROR (ERRKEY,2,FILEIO,LNUM)
        READ(LUNIO,1650) ECONO
 1650   FORMAT(24X,A6)


      CLOSE (LUNIO)

C-----------------------------------------------------------------------
C     Open FILEE
C-----------------------------------------------------------------------
        
        LNUM = 0
        PATHL  = INDEX(PATHEC,BLANK)
        IF (PATHL .LE. 1) THEN
        FILEGC = FILEE
        ELSE
        FILEGC = PATHEC(1:(PATHL-1)) // FILEE
        ENDIF

C-----------------------------------------------------------------------
C    Read Ecotype Parameter File
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------      
C      READ  FILEE 
C-----------------------------------------------------------------------
      OPEN (LUNECO,FILE = FILEGC,STATUS = 'OLD',IOSTAT=ERR)
        IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEE,0)

      
        ECOTYP = '      '
        DO WHILE (ECOTYP .NE. ECONO)
  
        CALL IGNORE(LUNECO, LNUM, ISECT, C255)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEGC,0)
        IF ((ISECT .EQ. 1) .AND. (C255(1:1) .NE. '*')) THEN
        READ (C255,'(A6,1X,A16,103X,4F6.0)',IOSTAT=ERR)
     &    ECOTYP, ECONAM, RDRMT,RDRMG,RDRMM, RCHDP


 
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEGC,LNUM)
        IF (ECOTYP .EQ. ECONO) THEN
        EXIT
        ENDIF
        ELSE IF (ISECT .EQ. 0) THEN
        IF (ECONO .EQ. 'DFAULT') CALL ERROR(ERRKEY,3,FILEGC,LNUM)
        ECONO = 'DFAULT'
        REWIND(LUNECO)
        ENDIF
      ENDDO
        CLOSE (LUNECO)

C-----------------------------------------------------------------------
C    Open FILEC
C-----------------------------------------------------------------------

     
      LINC = 0
      PATHL  = INDEX(PATHCR,BLANK)
      IF (PATHL .LE. 1) THEN
        FILECC = FILEC
      ELSE
        FILECC = PATHCR(1:(PATHL-1)) // FILEC
      ENDIF

      OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,0)

C-----------------------------------------------------------------------
C READ DORMANCY PARAMETERS *******************
C-----------------------------------------------------------------------
        SECTION = '!*DORM'
        CALL FIND(LUNCRP, SECTION, LINC, FOUND)
        IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILECC, LINC)
        ELSE

      CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)

      READ(CHAR,'(4F6.0,3X,A3)',IOSTAT=ERR) (FNPTD(II),II=1,4), TYPPTD
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LINC)

      
      CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)

      READ(CHAR,'(4F6.0,3X,A3)',IOSTAT=ERR) (FNPMD(II),II=1,4), TYPPMD
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LINC)

      CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)

      READ(CHAR,'(4F6.0,3X,A3)',IOSTAT=ERR) (FNPGD(II),II=1,4), TYPPGD
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LINC)

!        ENDIF
!-----------------------------------------------------------------------
!     Find and read Temperature threshold for cold hardening in 
!-----------------------------------------------------------------------
        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        READ(CHAR,'(3F6.0)',IOSTAT=ERR)HARD1,HARD2,FRZDC
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LINC)

        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        READ(CHAR,'(4F6.0,3X,A)',IOSTAT=ERR)
     &    (FRZHRD(II), II=1,4), TYPHRD
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LINC)

        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        READ(CHAR,'(4F6.0,3X,A)',IOSTAT=ERR)
     &    (FRZDHD(II), II=1,4), TYPDHD
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LINC)
        ENDIF

      CLOSE (LUNCRP)
    

!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
!***********************************************************************
!    Initialize yesterdays daylength to that passed in from PLANT
!***********************************************************************

        
      ELSE IF (DYNAMIC .EQ. SEASINIT) THEN

      DAYLY=DAYL

C-----------------------------------------------------------------------
C      All cultivars share the same freeze-killing temperature before 
C      cold hardening
C
C      Minimum survivable temperature after hardening varies with the 
C      cold hardening potential of the cultivar
C-----------------------------------------------------------------------
      FREEZ2=HARD1
      HARD2 = HARD1-(HARD1-HARD2)*RCHDP
C-----------------------------------------------------------------------
C
C   THE FOLLOWING GENERATES HEADINGS FOR NEW OUTPUT FILE DORMANT.OUT
C
C-----------------------------------------------------------------------
!      OPEN(UNIT = NOUTDT, FILE = OUTT, STATUS = 'UNKNOWN')
!
!      IF (IDETL .EQ. 'Y') THEN
!      
!
C-----------------------------------------------------------------------
C     Variable heading for DORMANT.OUT
C-----------------------------------------------------------------------
!
!        WRITE (NOUTDT,2202) NREP,TITLET,
!     &    MODEL,CROPD,EXPER,CROP,ENAME,TRTNO,TITLET, ECOTYP, ECONAM
! 2202   FORMAT (/,'*RUN ',I3,8X,': ',A25,/,
!     &    1X,'MODEL',10X,':',1X,A8,' - ',A10,/,
!     &    1X,'EXPERIMENT',5X,':',1X,A8,1X,A2,4X,A47,/,
!     &    1X,'TREATMENT',I3, 3X,':',1X,A25,/,
!     &        1X,'ECOTYPE',8X,':',1X,A6,1X,A16,/)
!  
!        
!        WRITE (NOUTDT,2203)
! 2203   FORMAT('@DATE',
!     &  ' DAYL  DRMST  PPGFAC   PRTFAC  WTLF  WCRLF  LFDM    STMWT',
!     &  '  WCRST  STDM    STRWT   WCRSR  SRDM   RTWT  WCRRT   RTDM')
!      ENDIF


      ELSE IF (DYNAMIC .EQ. EMERG) THEN
!      NONE
C***********************************************************************
C     Daily Rate Calculations 
C***********************************************************************
      

      ELSE IF (DYNAMIC .EQ. RATE) THEN

!      NONE
C***********************************************************************
C     Daily Integration 
C***********************************************************************

      ELSE IF (DYNAMIC .EQ. INTEGR) THEN

C***********************************************************************
C     Calculate cold-hardening status for day
C     Killing freeze temperature decreases as cold hardening proceeds.
C      Cold hardening is reversible while days are getting shorter.
C      Maximum rate of hardening (degrees C decrease in FREEZ2 per day) 
C      occurs at FRZHRD(1) with fractional rates between FRZHRD(1) and FRZHRD(2)
C      Cold hardening is reversed between FRZHRD(2) and FRZHRD(3).
C      Dehardening will not occur until daylength begins to increase.
C      Dehardening is not reversible.
C      FRZDC is the rate of plant/tissue death per degree C below FREEZ2.
C      This allows gradual killing of the stand with increased rate at 
C      lower temperatures.
C      Adapted from ALFACOLD model (Kanneganti et al., 1998, Agron J. 90:687-697)
C      Note: this routine allows hardening and dehardening to occur on the 
C      same day with reverse hardening and dehardening combining to 
C      accelerate dehardening at higher temperatures.
C***********************************************************************

      IF (DAYL .LE. DAYLY) THEN

      HARDR = CURV(TYPHRD,FRZHRD(1),FRZHRD(2),FRZHRD(3),FRZHRD(4),TMIN)
      DHARDR = 0.0

      ELSE
      HARDR = CURV(TYPHRD,FRZHRD(1),FRZHRD(2),FRZHRD(3),FRZHRD(4),TMIN)
      DHARDR = CURV(TYPDHD,FRZDHD(1),FRZDHD(2),FRZDHD(3),FRZDHD(4),TMIN)

      ENDIF

      FREEZ2 = FREEZ2 - (HARDR - DHARDR)*RCHDP

      FREEZ2 = MIN(HARD1, FREEZ2)
      FREEZ2 = MAX(FREEZ2, HARD2)





C***********************************************************************
C     Calculate Partitioning, Pg, and mobilization reduction
C     factors and dormancy state for day
C***********************************************************************
        PPTFAC=CURV(TYPPTD,FNPTD(1),FNPTD(2),FNPTD(3),FNPTD(4),DAYL)
        PPTFAC=RDRMT * PPTFAC
        PPTFAC=MIN (PPTFAC,1.0)

!            FNPGD(4) = RDRMG * FNPGD(4)
        PPGFAC=CURV(TYPPGD,FNPGD(1),FNPGD(2),FNPGD(3),FNPGD(4),DAYL)
        PPGFAC= PPGFAC/RDRMG
        PPGFAC=MIN (PPGFAC,1.0)

!            FNPMD(4) = RDRMM * FNPMD(4)
        PPMFAC=CURV(TYPPMD,FNPMD(1),FNPMD(2),FNPMD(3),FNPMD(4),DAYL)
        PPMFAC=PPMFAC/RDRMM
        PPMFAC=MIN (PPMFAC,1.0)

        IF (PPTFAC .GT. 0.0 .OR. PPGFAC .LT. 1.0
     &    .OR. PPMFAC .LT. 1.0) THEN
        DRMST='DORM'
        ELSE
        DRMST='NODORM'
        ENDIF


      DAYLY=DAYL

      ELSE IF (DYNAMIC .EQ. OUTPUT) THEN
C-----------------------------------------------------------------------
C     Calculate new variables for DORMANCY.OUT
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
C     Sent daily growth and partitioning detail to DORMANCY.OUT
C-----------------------------------------------------------------------

      

!        IF (IDETL .EQ. 'Y') THEN
C-----------------------------------------------------------------------
C     Print out dormancy parameters - TEMPORARY
C-----------------------------------------------------------------------
!
!            WRITE (NOUTDT,401) YRDOY, DAYL, DRMST, PPGFAC,
!     &              PPTFAC, WTLF, WCRLF, LFDM,
!     &              STMWT, WCRST, STDM, STRWT, WCRSR, SRDM,
!     &              RTWT, WCRRT, RTDM
! 401      FORMAT (1X,I5,1X,F5.2,1X,A6,1X,F5.3,1X,F5.3,1X,F6.1,
!     &              1X,F6.1,1X,F6.1,1X,F7.1,1X,F6.1,1X,F6.1,1X,F7.1,
!     &              1X,F6.1,1X,F6.1,1X,F6.1,1X,F6.1,1X,F6.1)
!        ENDIF

!***********************************************************************
!     SEASEND
!***********************************************************************
 
      ELSE IF (DYNAMIC .EQ. SEASEND) THEN
C-----------------------------------------------------------------------
C     Close DORMANCY.OUT
C-----------------------------------------------------------------------

!      CLOSE (NOUTDT)
      
      END IF
      RETURN
      END    !SUBROUTINE DORMANCY

!-----------------------------------------------------------------------
!     DORMANT VARIABLES
!-----------------------------------------------------------------------
! BLANK         ' '
! C255       255 character record
! DAYL         Current daylength (hours)
! DAYLY         Yesterdays daylength (hours)
! DRMST         Dormancy status (NODORM=not dormant, DORM=dormant - reversible, 
! DYNAMIC         Controls run sequence: DYNAMIC =RUNINIT, SEASINIT, RATE, 
!             EMERG, INTEGR, OUTPUT, or SEASEND
! ECONAM     Ecotype name - not used
! ECONO      Used to match ECOTYP in .ECO file
! ECOTYP     Ecotype code
! ERR
! ERRKEY 
! ERRNUM
! FILEC, FILEE   Filenames for Crop and Species files
! FILECC, FILEGC File+pathname for Crop and Eco files
! FILEIO    Filename for Input file
! FNPGD(1)  Base daylength for CURV function for daylength effect on Pg
!              for short-day dormancy (daylength when dormancy is maximum) 
! FNPGD(2)  Daylength threshold where dormancy effect begins
!              for daylength effect on Pg (for short-day dormancy)
! FNPGD(3)  Longest daylength threshold where there is no dormancy effect
!              for daylength effect on Pg (for long-day dormancy)
! FNPGD(4)  Daylength when dormancy effect is maximum 
!              for daylength effect on Pg (long-day dormancy)
! FNPMD(1)  Base daylength for CURV function for daylength effect on mobilization
!              for short-day dormancy (daylength when dormancy is maximum)
! FNPMD(2)  Daylength threshold where dormancy effect begins
!              for daylength effect on mobilization (for short-day dormancy)
! FNPMD(3)  Longest daylength threshold where there is no dormancy effect
!              for daylength effect on mobilization (for long-day dormancy)
! FNPMD(4)  Daylength when dormancy effect is maximum 
!              for daylength effect on mobilization (long-day dormancy)
! FNPTD(1)  Ignored for short-day dormancy
! FNPTD(2)  Daylength threshold where dormancy effect is maximum
!              for daylength effect on partitioning (for short-day dormancy)
! FNPTD(3)  Shortest daylength threshold where there is no dormancy effect
!              for daylength effect on partitioning (for short-day dormancy)
! FNPTD(4)  Minimum relative effect of dormancy when crop is non-dormant (set to 0.0)
! FREEZ2    Temperature below which plant growth stops completely. (°C)
! FRZDC        Freezing death coefficient  - percentage tissue/population death per day per degree below FREEZ2)
! FRZDHD(1) Minimum temperature at which dehardening begins (relative rate=0)
! FRZDHD(2) Temperature at which dehardening reaches maximum rate (relative rate=1)
! FRZDHD(3) Not used
! FRZDHD(4) Maximum (absolute) rate of dehardening (degrees C increase above HARD2 per day)n of STRWT and PLNTPOP)
! FRZHRD(1) Temperature at which cold hardening reaches maximum rate (relative rate=1)
! FRZHRD(2) Temperature below which cold hardening begins (relative rate=0)
! FRZHRD(3) Temperature at which hardening is reversed at maximum rate (relative rate=-1)
! FRZHRD(4) Maximum (absolute) rate of cold hardening (degrees C decrease towards HARD2 per day)
! HARD1        Killing low temperature before cold hardening (begins killing storage organ)
! HARD2        Killing low temperature after cold hardening (begins killing storage organ)
! ISECT
! LNUM
! LUNECO    Logical unit number for ECO files
! LUNIO     Input file logical unit no.
! PATHL
! PPGFAC        Reduction in photosynthetic rate due to dormancy 
! PPMFAC        Reduction in mobilization rate due to dormancy 
! PPTFAC        Reduction in partitioning to vegetative tissues during dormancy
! RCHDP        Ecotype relative cold hardening potential (0-1)
! RDRMG     Relative sensitivity of ecotype to daylength/dormancy effects on Pg
! RDRMM     Relative sensitivity of ecotype to daylength/dormancy effects on 
!              mobilization
! RDRMT     Relative sensitivity of ecotype to daylength/dormancy effects on 
!           partitioning to perenniating tissues
! SECTION   Heading name in input files
! TMIN        Daily average temperature
! TYPDHD        Response type for cold dehardening 
! TYPHRD         Response type for cold hardening
! TYPPGD        Type of response curve for effect of daylength/dormancy on Pg
! TYPPMD    Type of response curve for effect of daylength/dormancy on
!           mobilization 
! TYPPTD    Type of response curve for effect of daylength/dormancy on
!           partitioning to perenniating organ
!-----------------------------------------------------------------------
!     END SUBROUTINE DORMANT
!-----------------------------------------------------------------------
