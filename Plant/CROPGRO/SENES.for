C=======================================================================
C  SENES, Subroutine, K.J. Boote, J.W. Jones and G. Hoogenboom
C  Calculates leaf senescence due to natural aging, drought stress,
C  light stress, and physiological maturity.
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  05/01/1989     Written.
C  01/20/1997 GH  Revised.
C  06/16/1998 CHP revised for modular format.
C  05/11/1999 GH  Incorporated in CROPGRO
C  06/19/2001 GH  Fix SSNDOT, SSDOT
C  08/12/2003 CHP Added I/O error checking
C  06/30/2006 CHP/CDM Added optional KCAN to ECO file.
C-----------------------------------------------------------------------
C  Called : PLANT
C  Calls  : ERROR, FIND, IGNORE
C========================================================================

      SUBROUTINE SENES(DYNAMIC,
     &    FILECC, CLW, DTX, KCAN, NR7, NRUSLF, PAR,       !Input
     &    RHOL, SLAAD, STMWT, SWFAC, VSTAGE, WTLF, XLAI,  !Input
     &    SLDOT, SLNDOT, SSDOT, SSNDOT)                   !Output

C-----------------------------------------------------------------------
      USE ModuleDefs
      USE ModuleData
      IMPLICIT NONE
      EXTERNAL GETLUN, FIND, ERROR, IGNORE, TABEX
      SAVE

      CHARACTER*6  ERRKEY, SECTION
      CHARACTER*80 CHAR
      CHARACTER*92 FILECC
      PARAMETER (ERRKEY = 'SENES')

      INTEGER I, II, LUNCRP, ERR, LINC, LNUM, ISECT
      INTEGER DYNAMIC
      INTEGER NR7, DAS
      INTEGER FOUND
      INTEGER NSWAB
      PARAMETER (NSWAB = 5)

      REAL DTX, NRUSLF, PAR, RATTP, RHOL, SLAAD
      REAL STMWT, VSTAGE, CLW, WTLF, XLAI
      REAL SLDOT, SLNDOT, SSDOT, SSNDOT
      REAL ICMP, KCAN, PORPT, SENDAY
      REAL SENRT2, SENRTE, TCMP
      REAL LCMP, LTSEN, PORLFT, LFSEN, SWFAC, WSLOSS, TABEX
      REAL SENMAX(4), SENPOR(4), XSENMX(4), XSTAGE(4)
      REAL SWFCAB(NSWAB)

      TYPE (ControlType) CONTROL

!***********************************************************************
!***********************************************************************
!     Run Initialization - Called once per simulation
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
!-----------------------------------------------------------------------
!     Read in values from input file, which were previously input
!       in Subroutine IPCROP.
!-----------------------------------------------------------------------
      CALL GETLUN('FILEC', LUNCRP)
      OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,0)
      LNUM = 0
!-----------------------------------------------------------------------
!    Find and Read Photosynthesis Section
!-----------------------------------------------------------------------
!     Subroutine FIND finds appropriate SECTION in a file by
!     searching for the specified 6-character string at beginning
!     of each line.
!-----------------------------------------------------------------------
!CHP 7/30/2004 - Get KCAN from main routine.
!                May be overriden by value in ECOTYPE file.
!      SECTION = '!*PHOT'
!      CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
!      IF (FOUND .EQ. 0) THEN
!        CALL ERROR(SECTION, 42, FILECC, LNUM)
!      ELSE
!          ISECT = 2
!          DO WHILE (ISECT .NE. 1)
!            CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
!            IF (ISECT .EQ. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
!          ENDDO
!          READ(CHAR,'(12X,F6.0)',IOSTAT=ERR) KCAN
!          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
!      ENDIF
!-----------------------------------------------------------------------
!    Find and Read Partitioning Section
!-----------------------------------------------------------------------
      SECTION = '!*VEGE'
      CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILECC, LNUM)
      ELSE
        DO I=1,4
          ISECT = 2
          DO WHILE (ISECT .NE. 1)
            CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
          ENDDO
        ENDDO
        READ(CHAR,'(6X,F6.0)',IOSTAT=ERR) PORPT
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
      ENDIF

!-----------------------------------------------------------------------
!    Find and Read Senescence Section
!    NOTE: First search for Section finds '!*LEAF GROWTH PARAMETERS'
!          Second search finds '!*LEAF SENESCENCE FACTORS'
!-----------------------------------------------------------------------
      SECTION = '!*LEAF'
      CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILECC, LNUM)
      ENDIF

      CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        READ(CHAR,'(3F6.0)',IOSTAT=ERR) SENRTE, SENRT2, SENDAY
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        READ(CHAR,'(2F6.0)',IOSTAT=ERR) ICMP, TCMP
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        READ(CHAR,'(8F6.0)',IOSTAT=ERR)
     &      (XSTAGE(II),II=1,4),(XSENMX(II),II=1,4)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        READ(CHAR,'(8F6.0)',IOSTAT=ERR)
     &      (SENPOR(II),II=1,4),(SENMAX(II),II=1,4)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
      ENDIF

      CLOSE (LUNCRP)

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      SSDOT  = 0.0
      SLDOT  = 0.0
      SLNDOT = 0.0
      SSNDOT = 0.0
      RATTP  = 1.0

      DO I = 1,5
        SWFCAB(I) = 1.0
      ENDDO

!***********************************************************************
!***********************************************************************
!     DAILY RATE/INTEGRATION
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------
!     DAS   = MAX(0,TIMDIF(YRSIM,YRDOY))
      CALL GET(CONTROL)
      DAS = CONTROL % DAS

      !Update value of RATTP.
      DO I = NSWAB,2,-1
        SWFCAB(I) = SWFCAB(I-1)
      ENDDO
      SWFCAB(1) = SWFAC
      RATTP = SWFCAB(NSWAB)

      SSDOT = 0.0
      SLDOT = 0.0
      SLNDOT = 0.0
      SSNDOT = 0.0
      IF (DAS .LE. NR7 .AND. VSTAGE .GE. 1.0) THEN
C-----------------------------------------------------------------------
C     This section calculates natural senescence prior to the
C     beginning of seed growth
C-----------------------------------------------------------------------
        IF (VSTAGE .GE. 5.0) THEN
          PORLFT = 1.0 - TABEX(SENPOR,XSTAGE,VSTAGE,4)
          IF ((WTLF * ( 1.0 - RHOL)) .GT. CLW*PORLFT) THEN
            SLDOT = WTLF * ( 1.0 - RHOL) - CLW * PORLFT
          ENDIF
        ENDIF
C-----------------------------------------------------------------------
C     This section calculates leaf senescence due to N mobilization.
C     Concentration of N in stems and leaves must
C     be recalculated since senescing leaves and petioles are assumed
C     to contain no more mineable Protein--i.e., N content of senesced
C     leaves and petioles is different from canopy average.
C-----------------------------------------------------------------------
        LFSEN = SENRTE * NRUSLF / 0.16
        LFSEN = MIN(WTLF,LFSEN)
        SLDOT = SLDOT + LFSEN
        SLDOT = MIN(WTLF,SLDOT)
C-----------------------------------------------------------------------
C     This section calculates senescence due to low light in lower
C     canopy.  First compute LAI at which light compensation is reached
C     then allow LAI above this amount to be senesced over TCMP thermal
C     days.
C-----------------------------------------------------------------------
        LTSEN = 0.0
        IF (PAR .GT. 0.) THEN
          LCMP = -(1. / KCAN) * ALOG(ICMP / PAR)
          LTSEN = DTX * (XLAI - LCMP) / TCMP
          LTSEN = MAX(0.0, LTSEN)
        ENDIF
C-----------------------------------------------------------------------
C     Convert area loss to biomass(m2 *10000cm2/m2)/(cm2/g)=g/m2
C-----------------------------------------------------------------------
        SLDOT = SLDOT + LTSEN * 10000. / SLAAD
C-----------------------------------------------------------------------
C     Calculate senescence due to water stress.
C-----------------------------------------------------------------------
        WSLOSS = SENDAY * (1. - RATTP) * WTLF
        IF (WSLOSS .GT. 0.0) THEN
          PORLFT = 1.0 - TABEX(SENMAX, XSENMX, VSTAGE, 4)
          WSLOSS = MIN(WSLOSS, WTLF - CLW * PORLFT)
          WSLOSS = MAX(WSLOSS, 0.0)
          SLNDOT = WSLOSS
        ENDIF
        SLDOT = SLDOT + SLNDOT
        SSDOT = SLDOT * PORPT
        SSDOT = MIN(SSDOT,0.1*STMWT)
        SSNDOT = SLNDOT * PORPT
        SSNDOT = MIN(SSDOT,SSNDOT)
C-----------------------------------------------------------------------
C     This section calculates senescence of leaves and petioles
C     after R7.
C-----------------------------------------------------------------------
      ELSEIF (DAS .GT. NR7) THEN
        IF (WTLF .GT. 0.0001) THEN
          SLDOT = WTLF * SENRT2
          SLNDOT = SLDOT
          SSDOT = SLDOT * PORPT
          SSNDOT = SSDOT
        ELSE
          SLDOT = 0.0
          SSDOT = 0.0
          SLNDOT = 0.0
          SSNDOT = 0.0
        ENDIF
        IF (STMWT .LT. 0.0001) THEN
          SLNDOT = 0.0
          SSNDOT = 0.0
        ENDIF
      ENDIF

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END ! SUBROUTINE SENES
!***********************************************************************
!     SENES VARIABLE DEFINITIONS:
!-----------------------------------------------------------------------
! CHAR      Contains the contents of last record read 
! CLW       Cumulative leaf growth (g[leaf]/m2)
! DAS       Days after start of simulation (days)
! DTX       Thermal time that occurs in a real day based on vegetative 
!             development temperature function (thermal days / day)
! ERR       Error code for file operation 
! FILECC    Path plus filename for species file (*.spe) 
! FOUND     Indicator that good data was read from file by subroutine FIND 
!             (0 - End-of-file encountered, 1 - NAME was found) 
! ICMP      Light compensation point for senescence of lower leaves because 
!             of excessive self-shading by crop canopy (moles / m2 / day)
! ISECT     Data record code (0 - End of file encountered, 1 - Found a good 
!             line to read, 2 - End of Section in file encountered, denoted 
!             by * in column 1
! KCAN      Canopy light extinction coefficient for daily PAR, for 
!             equidistant plant spacing, modified when in-row and between 
!             row spacings are not equal 
! LCMP      LAI at which today's light compensation (ICMP) is reached
!             (m2[leaf] / m2[ground])
! LNUM      Current line number of input file 
! LTSEN     Senescence of lower leaves due to self-shading of canopy
!             (1/day)
! LUNCRP    Logical unit number for FILEC (*.spe file) 
! NR7       Day when 50% of plants first have yellowing or maturing pods
!             (days)
! NRUSLF    N actually mobilized from leaves in a day (g[N]/m2-d)
! PAR       Daily photosynthetically active radiation or photon flux 
!             density (moles[quanta]/m2-d)
! PORLFT    Proportion of leaf weight grown which will have been senesced 
!             if no water stress has occurred prior to this V-stage 
! PORPT     Ratio of petiole to leaf weight 
! RATTP     Factor used in determining increased senescence due to water 
!             stress 
! RHOL      Fraction of leaf which is carbohydrate (g [CH20] / g[leaf])
! SECTION   Section name in input file 
! SENDAY    Maximum fraction of existing leaf weight which can be senesced 
!             on day N as a function of severe water stress 4 days earlier. 
! SENMAX(I) Maximum proportion of total leaf weight as a function of 
!             V-stage (XSENMX(I)) which can be senesced due to water stress. 
! SENPOR(I) Proportion of leaf weight grown which will have been senesced 
!             by a given V- stage (XSTAGE(I)) if no water stress has 
!             occurred prior to this V-stage (XSTAGE(I)) -- normal 
!             vegetative senescence does not occur if prior water stress 
!             has already  reduced leaf  
! SENRT2    Factor by which leaf weight is multiplied to determine 
!             senescence each day after NR7 (g(leaf) / g(protein loss))
! SENRTE    Factor by which protein mined from leaves each day is 
!             multiplied to determine LEAF senescence.
!             (g(leaf) / g(protein loss))
! LFSEN     Leaf senescence due to N mobilization (g[leaf] / m2[ground])
! SLAAD     Specific leaf area, excluding weight of C stored in leaves
!             (cm2[leaf] / g[leaf])
! SLDOT     Defoliation due to daily leaf senescence (g/m2/day)
! SLNDOT    Leaf senescence due to water stress (g/m2/day)
! SSDOT     Daily senescence of petioles (g / m2 / d)
! SSNDOT    Petiole senescence due to water stress (g/m2/day)
! STMWT     Dry mass of stem tissue, including C and N
!             (g[stem] / m2[ground)
! TABEX     Function subroutine - Lookup utility 
! TCMP      Time constant for senescence of lower leaves because of 
!             excessive self-shading by crop canopy (thermal days)
! TIMDIF    Integer function which calculates the number of days between 
!             two Julian dates (da)
! VSTAGE    Number of nodes on main stem of plant 
! WSLOSS    Leaf senescence due to water stress (g/m2/day)
! WTLF      Dry mass of leaf tissue including C and N
!             (g[leaf] / m2[ground])
! XLAI      Leaf area (one side) per unit of ground area
!             (m2[leaf] / m2[ground])
! XSENMX(I) V-stage at which maximum fraction of cumulative leaf growth 
!             vulnerable to loss due to water stress is SENMAX(I).
!             (# leaf nodes)
! XSTAGE(I) V-stage at which SENPOR(I) fraction of cumulative leaf growth 
!             will have been senesced if no water stress occurred.
!             (# leaf nodes)
! YRDOY     Current day of simulation (YYDDD)
! YRSIM     Start of simulation date (YYDDD)
!-----------------------------------------------------------------------
!     END SUBROUTINE SENES
!-----------------------------------------------------------------------
