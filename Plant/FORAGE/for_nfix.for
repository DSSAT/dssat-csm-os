C=======================================================================
C  FOR_NFIX, Subroutine, J.W. Jones, G. Hoogenboom, and K.J. Boote
C-----------------------------------------------------------------------
C  Calculates nitrogen fixation
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  01/09/1989 JWJ Written.
C  01/03/1993 WTB Modified.
C  07/14/1995 JWH Modified to reserve some C for nodule growth.
C  09/26/1995 KJB Modified to put temperature, water deficit, flooding,
C                 and age effects, not on RGR or SNA, but on growth or
C                 N-fix per unit land area.  Helped greatly for water
C                 deficit effects.  Also, C not used for N-fixation
C                 because of water deficit, is not allowed to be used
C                 for growth of nodules, except a small amount.
C  10/07/1995 KJB Modified to exclude evaporating (layer 1) zone from
C                 the nodule zone (DNOD).  Cancelled 6/25/97 KJB
C  01/19/1996 KJB Add a eigtht day memory of soil water deficiet (SWMEM8)
C                 on specific nodule activity.
C  06/25/1997 KJB Modified to use TURFAC to affect nodule growth and NFIX
C                 rather than SWFACT (frac avail. soil water in nodule
C                 zone.  Poor experience with it and recent lit indicates
C                 that nodules respond more to plant water status than to
C                 soil water around nodules.  Also, now we can use more
C                 shallow nodule zone (starting at top to 30 cm) to
C                 compute soil temperature effect and flooding effect.
C  02/02/1998 GH  Deleted CUSTOV, not used
C  07/07/1998 CHP Modified for modular format
C  05/11/1999 GH  Incorporated in CROPGRO
C  06/26/2001 GH  Correct DAS
C  06/11/2002 GH  Modified for Y2K
C-----------------------------------------------------------------------
C  Called from:  PLANT
C  Calls:        FIND, ERROR, IGNORE
C=======================================================================

      SUBROUTINE FOR_NFIX(
     &  AGRNOD, CNODMN, CTONOD, DLAYR, DXR57,                   !Input
     &  FILECC, FILEIO, NLAYR, NR7, PLTPOP,                     !Input
     &  SAT, ST, SW, TURFAC, YRDOY, YRSIM,                      !Input
     &  CNOD, DWNOD, DWNODA, NDTH, NFIXN,                       !Output
     &  NODGR, WTNFX, SENNOD,                                   !Output
     &  DYNAMIC)                                                !Control

C-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
        ! which contain control information, soil
        ! parameters, hourly weather data.
      IMPLICIT NONE
      EXTERNAL GETLUN, FIND, ERROR, IGNORE, TIMDIF, CURV
      SAVE

      CHARACTER*3 TYPFXT,TYPNGT,TYPFXD,TYPFXW,TYPFXA
      CHARACTER*6 ERRKEY
      PARAMETER (ERRKEY = 'NFIX')

      CHARACTER*6  SECTION
      CHARACTER*30 FILEIO
      CHARACTER*80 C80
      CHARACTER*92 FILECC

      INTEGER LUNIO

      INTEGER LUNCRP, ERR, LNUM, FOUND, ISECT
      INTEGER I, II, NR7, NLAYR, J, YRDOY, YRSIM, DAS
      INTEGER DYNAMIC, TIMDIF

      REAL NDTHMX, NFIXN, NODRGM, NDTH, NODGR
      REAL NODRGR, NFXAGE
      REAL SWFACT, FLDACT, CNFACT, FRCNM
      REAL ACSTF, ACSTG, DSW, FLDSUM, FLAYR, TNFIX, TNGRO
      REAL CLEFT, SNACT, RNDTH, CTONOD, DWNODI, DWNOD, SDWNOD, CNOD
      REAL CUSFIX, DNOD, SNACTM, EFNFIX, EFINOC, CNODCR, CNODGR
      REAL AGRNOD, RFIXN, DXR57
      REAL CURV
      REAL EPORS, PLTPOP, DWNODA, WTNFX, PRONOD, CNODMN, TURFAC
      REAL SWMEM8, PCSFIX, CNOFIX, PNFIXN
      REAL FNFXT(4), FNNGT(4), FNFXD(4), FNFXW(4), FNFXA(4)
      REAL SWMEM(9)
      REAL DLAYR(NL), SAT(NL), SW(NL), ST(NL)
      REAL LAYERFRAC(NL), SENNOD(NL), DSWP
!       REAL DUL(NL), LL(NL)

!***********************************************************************
!***********************************************************************
!     Run Initialization - Called once per simulation
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
!-----------------------------------------------------------------------
      CALL GETLUN('FILEIO', LUNIO)
      OPEN (LUNIO, FILE = FILEIO,STATUS = 'OLD',IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,0)
!-----------------------------------------------------------------------
C    Find and Read Field Section
C-----------------------------------------------------------------------
      LNUM = 1
      SECTION = '*INITI'
      CALL FIND(LUNIO, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILEIO, LNUM)
      ELSE
        READ(LUNIO,'(28X,2F6.0)',IOSTAT=ERR) EFINOC, EFNFIX
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
      ENDIF

      IF (EFINOC .LE. 0.0) EFINOC = 1.0
      IF (EFNFIX .LE. 0.0) EFNFIX = 1.0

      CLOSE (LUNIO)
!-----------------------------------------------------------------------
!     Read in values from input file, which were previously input
!       in Subroutine IPCROP.
!-----------------------------------------------------------------------
      CALL GETLUN('FILEC', LUNCRP)
      OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,0)
!-----------------------------------------------------------------------
!    Find and Read Respiration Section
!-----------------------------------------------------------------------
!     Subroutine FIND finds appropriate SECTION in a file by
!     searching for the specified 6-character string at beginning
!     of each line.
!-----------------------------------------------------------------------
      LNUM = 1
      SECTION = '!*RESP'
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(18X,F6.0)',IOSTAT=ERR) RFIXN
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
      ENDIF
!-----------------------------------------------------------------------
!    Find and Read Plant Composition Section
!-----------------------------------------------------------------------
      SECTION = '!*PLAN'
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(12X,F6.0)',IOSTAT=ERR) PRONOD
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
      ENDIF
!-----------------------------------------------------------------------
!    Find and Read Nitrogen Fixation Section
!-----------------------------------------------------------------------
      SECTION = '!*NITR'
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(3F6.0,6X,2F6.0)',IOSTAT=ERR)
     &    SNACTM, NODRGM, DWNODI, NDTHMX, CNODCR
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(4F6.0,3X,A3)',IOSTAT=ERR)(FNNGT(II),II=1,4), TYPNGT
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(4F6.0,3X,A3)',IOSTAT=ERR)(FNFXT(II),II=1,4), TYPFXT
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(4F6.0,3X,A3)',IOSTAT=ERR)(FNFXD(II),II=1,4), TYPFXD
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(4F6.0,3X,A3)',IOSTAT=ERR)(FNFXW(II),II=1,4), TYPFXW
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(4F6.0,3X,A3)',IOSTAT=ERR)(FNFXA(II),II=1,4), TYPFXA
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
      ENDIF

      CLOSE(LUNCRP)

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      CNOD   = 0.0
      DWNOD  = 0.0    
      DWNODA = 0.0  
      NDTH   = 0.0    
      NFIXN  = 0.0    
      NODGR  = 0.0    
      WTNFX  = 0.0    
      SDWNOD = 0.0 
      SENNOD = 0.0 

      DNOD   = 30.0

C***********************************************************************
C***********************************************************************
C     DAILY RATE/INTEGRATION
C***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
C-----------------------------------------------------------------------
      DAS = MAX(0,TIMDIF(YRSIM,YRDOY))
C-----------------------------------------------------------------------
C   Set initial nodule mass to DWNODI as read from crop species file
C-----------------------------------------------------------------------
      IF (SDWNOD .LT. 1.) THEN
        DWNOD  = DWNODI * PLTPOP
        SDWNOD = 1.0
        DWNODA = DWNODI * PLTPOP
        WTNFX  = DWNODA * 0.16 * PRONOD
        DO J = 1,8
        SWMEM(J) = 1.0
        ENDDO
      ENDIF

!-----------------------------------------------------------------------
C   Initialize soil water and temperature factors (top DNOD cm of soil)
C-----------------------------------------------------------------------
      SWFACT = 1.0
      FLDACT = 1.0
      ACSTF  = 0.0
      ACSTG  = 0.0
      DSW    = 0.0
      FLDSUM = 0.0
C-----------------------------------------------------------------------
C     Calculate carbon allocated per unit of nodule biomass:
C     CNODCR = C requirement for nodule respiration (g C/g nodule/d)
C-----------------------------------------------------------------------
      CNFACT = 1.
      IF (DWNOD .GT. 0.) THEN
        FRCNM = CTONOD/DWNOD
        IF (FRCNM .LT. CNODCR) CNFACT = FRCNM / CNODCR
      ENDIF
C-----------------------------------------------------------------------
C   Calculate soil water and temperature factors for each layer to DNOD
C-----------------------------------------------------------------------
      LAYERFRAC = 0.0
      DSWP = 0.0
      DNOD = 50.0
      DO I = 1,NLAYR
        FLAYR = 1.0
        DSW = DSW + DLAYR(I)
        IF (DSW .GT. DNOD) FLAYR = (DNOD-(DSW-DLAYR(I)))/DLAYR(I)

        ACSTF = ACSTF + DLAYR(I) * FLAYR *
     &     CURV(TYPFXT,FNFXT(1),FNFXT(2),FNFXT(3),FNFXT(4),ST(I))

        ACSTG = ACSTG + DLAYR(I) * FLAYR *
     &     CURV(TYPNGT,FNNGT(1),FNNGT(2),FNNGT(3),FNNGT(4),ST(I))

        EPORS = MAX(SAT(I) - SW(I), 0.0)
        FLDSUM = FLDSUM + DLAYR(I) * FLAYR *
     &     CURV(TYPFXW,FNFXW(1),FNFXW(2),FNFXW(3),FNFXW(4),EPORS)

        IF (I .EQ. 1) THEN
        LAYERFRAC(1) = DSW / DNOD
        ELSE
        LAYERFRAC(I) = (DSW - DSWP)*FLAYR / DNOD
        ENDIF
        DSWP = DSW
        IF ( FLAYR .LT. 1.0 ) GOTO 400

      ENDDO
C-----------------------------------------------------------------------
C   Constraints due to soil water and T and average nodule age:
C   TNFIX : soil T effect on N2 fixation
C   TNGRO : soil T effect on nodule growth
C   SWFACT: soil water deficit effect on N2 fixation and nodule growth
C   FLDACT: soil water flooding effect on N2 fixation and nodule growth
C   NFXAGE: average nodule age effect on nodule growth
C-----------------------------------------------------------------------
400   TNFIX  = ACSTF / DNOD
      TNGRO  = ACSTG / DNOD
      FLDACT = FLDSUM / DNOD

      SWFACT = CURV(TYPFXD,FNFXD(1),FNFXD(2),FNFXD(3),FNFXD(4),TURFAC)
      NFXAGE = CURV(TYPFXA,FNFXA(1),FNFXA(2),FNFXA(3),FNFXA(4),DXR57)
C-----------------------------------------------------------------------
C DETERMINE MEMORY OF PREVIOUS EIGHT DAYS OF SOIL WATER DEFICITS
C-----------------------------------------------------------------------
      DO J= 8,2,-1
        SWMEM(J) = SWMEM(J-1)
      ENDDO
      SWMEM(1) = SWFACT

      SWMEM8 = 0.0
      DO J = 1,8
        SWMEM8 = SWMEM8 + SWMEM(J)
      ENDDO
      SWMEM8 = SWMEM8/8
C-----------------------------------------------------------------------
C     Reserve CNODMN for nodule growth.  JWH 7/9/95
C-----------------------------------------------------------------------
      CLEFT = CTONOD - CNODMN
C-----------------------------------------------------------------------
C    Compute Specific Nodule Activity taking into account the maximum
C       activity of the nodules (SNACTM), and strain effects only.
C    9/27/95 moved temp, water deficit, and soil water flooding effects
C    below to the primary rate.  We are not getting proper stress effects.
C-----------------------------------------------------------------------
      SNACT  = SNACTM  * EFNFIX
C-----------------------------------------------------------------------
C       Compute nodule death rate as function of SW deficit, SW flooding,
!                               and carbon deficit (chp)
C-----------------------------------------------------------------------
      RNDTH = NDTHMX * MAX((1.-FLDACT),(1.-SWFACT),(1.-CNFACT))
      NDTH = MIN(1.0,RNDTH) * DWNOD               !g/m2
      DO I = 1, NLAYR
        SENNOD(I) = NDTH * LAYERFRAC(I) * 10.     !kg/ha
      ENDDO
C-----------------------------------------------------------------------
C    Compute N-Fixation
C
C-----------------------------------------------------------------------
      IF (DAS .LT. NR7) THEN
        PNFIXN = MIN((CLEFT * 0.16 / RFIXN), (DWNOD * SNACT)) * TNFIX
        NFIXN = PNFIXN * MIN(SWFACT, SWMEM8, FLDACT)
      ELSE
        PNFIXN = 0.0
        NFIXN = 0.0
      ENDIF
C-----------------------------------------------------------------------
C    Compute C Used for N-Fixation
C-----------------------------------------------------------------------
      PCSFIX = (PNFIXN / 0.16) * RFIXN
      CUSFIX = (NFIXN  / 0.16) * RFIXN
      CNOFIX = PCSFIX - CUSFIX
C-----------------------------------------------------------------------
C     Compute C Left to Grow New Nodule Mass
C     Includes minimum reserved for nodule growth (CNODMN) plus any C
C     left after N fixation.  JWH 7/11/95
C-----------------------------------------------------------------------
      CLEFT = MAX(0.0,CLEFT - CUSFIX- 0.9*CNOFIX) + CNODMN
C-----------------------------------------------------------------------
C    Compute Potential Growth of Nodules (Demand)
C    EFNFIX = strain efficiency
C    EFINOC = inoculation effectiveness (or rhizobium density factor)
C-----------------------------------------------------------------------
      IF (DAS .LT. NR7) THEN
        NODRGR = NODRGM  * EFNFIX * EFINOC
      ELSE
        NODRGR = 0.0
      ENDIF
C-----------------------------------------------------------------------
C    Compute Nodule Growth, Limiting by Either Supply or Demand for C
C-----------------------------------------------------------------------
      NODGR = MIN(CLEFT/AGRNOD,DWNOD*NODRGR)
     &    * TNGRO * MIN(SWFACT,FLDACT) * NFXAGE
      CNODGR = NODGR * AGRNOD
C-----------------------------------------------------------------------
C    Compute C used in N-Fixation and Nodule Growth (Including
C    Respiration Costs) Today
C-----------------------------------------------------------------------
      CNOD = CUSFIX + CNODGR

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END ! SUBROUTINE FOR_NFIX
!=======================================================================
! ACSTF    Weighted average soil temperature effect on N fixation (cm)
! ACSTG    Weighted average soil temperature effect on nodule growth (cm)
! AGRNOD   CH2O requirement for nodule growth (g[CH2O] / g[nodule])
! CLEFT    C left to grow new nodule mass (g[CH2O] / m2 / d)
! CNFACT   Ratio of C needed from nodules for vegetative and reproductive 
!            growth to C needed for nodule respiration (stress factor) 
! CNOD     C used in N-Fixation and nodule growth (including respiration 
!            costs) today (g[CH2O] / m2 / d)
! CNODCR   C requirement for nodule respiration (g[C] / g[nodule] / d)
! CNODGR   C used in nodule growth today (g[CH2O] / m2 / d)
! CNODMN   Minimum C reserved for nodule growth (g[CH2O] / m2 / d)
! CNOFIX   C used for N fixation (g[CH2O] / m2 / d)
! CTONOD   C to allocate to nodules to fix N needed for reproductive and 
!            vegetative growth (g[CH2O] / m2 / d)
! CURV     Function subroutine 
! CUSFIX   C used in N-Fixation today (g[CH2O] / m2 / d)
! DAS      Days after start of simulation (days)
! DLAYR(L) Soil Depth in layer L (cm)
! DNOD     Depth of nodule zone (cm)
! DSW      Accumulated soil depth (cm)
! DWNOD    Current nodule mass (g[nodule] / m2)
! DWNODA   Cumulative nodule growth (g[nodule] / m2)
! DWNODI   Initial nodule mass per plant (g[nodule] / plant)
! DXR57    Relative time between first seed (NR5) and physiological 
!            maturity (NR7) 
! DYNAMIC  Module control variable; =RUNINIT, SEASINIT, RATE, EMERG, 
!            INTEGR, OUTPUT, or SEASEND 
! EFINOC   Inoculation effectiveness (or rhizobium density factor) 
! EFNFIX   Strain efficiency 
! EPORS    Soil water content above saturation (cm3/cm3)
! ERR      Error code for file operation 
! ERRKEY   Subroutine name for error file 
! FILECC   Path plus filename for species file (*.spe) 
! FILEIO   Filename for INP file (e.g., IBSNAT35.INP) 
! FLAYR    Fraction of layer within nodule zone 
! FLDACT   Soil water flooding effect on N2 fixation and nodule growth 
!            (0-1) 
! FLDSUM   Weighted average flooding conditions effect on N fixation and 
!            nodule growth and nodule death rate (cm)
! FNFXA(I) Critical points in development from first seed to physiological 
!            maturity for reducing N fixation due to canopy physiological age 
! FNFXD(I) Effects of soil water stress factor (TURFAC) on N fixation 
! FNFXT(I) Critical temperature points for function to reduce N fixation 
!            rate when temperature is not optimal (°C)
! FNFXW(I) Critical soil volumetric water contents for reducing N fixation 
!            when the soil is too wet (flooded) (cm3/cm3)
! FNNGT(I) Critical temperature points for function to reduce Nodule growth 
!            rates when temperature is not optimal (°C)
! FRCNM    C required for reproductive and vegetative growth per nodule 
!            mass (g[CH2O] / g[nodule] / d)
! LUNCRP   Logical unit number for FILEC (*.spe file) 
! LUNIO    Logical unit number for FILEIO 
! NDTH     Nodule death rate (g[nodule] / m2 / d)
! NDTHMX   Maximum relative death rate of nodules under flooded or dry 
!            conditions (g[nodule] / g[nodule] / d)
! NFIXN    Amount of N fixed during the day (g[N] / m2 / d)
! NFXAGE   Average nodule age effect on nodule growth (0-1) 
! NL       maximum number of soil layers = 20 
! NLAYR    Number of soil layers 
! NODGR    New nodule growth (g[nod] / m2 / d)
! NODRGM   Maximum nodule relative growth rate (g[nodule] / g[nodule] / d)
! NODRGR   Effective nodule relative growth rate
!            (g[nodule] / g[nodule] / d)
! NR7      Day when 50% of plants first have yellowing or maturing pods
!            (days)
! PCSFIX   Potential C used in N-Fixation today (g[CH2O] / m2 / d)
! PLTPOP   Plant population (# plants / m2)
! PNFIXN   Potential amount of N fixation today (g[N] / m2 / d)
! PRONOD   Protein composition in nodules (g[protein] / g[nodule])
! RFIXN    CH2O required for biological N fixation (g[CH2O] / g[protein])
! RNDTH    Nodule death rate (g[nodule] / g[nodule] / d)
! SAT(L)   Volumetric soil water content in layer L at saturation
!            (cm3 [water] / cm3 [soil])
! SDWNOD   Denotes first entrance to NFIX subroutine 
! SNACT    Specific nodule activity (g [nodule])
! SNACTM   Maximum activity of the nodules (g [nodule])
! ST(L)    Soil temperature in soil layer L (°C)
! SW(L)    Volumetric soil water content in layer L
!            (cm3 [water] / cm3 [soil])
! SWFACT   Soil water deficit effect on N2 fixation and nodule growth (0-1)
!            
! SWMEM(I) Values of last 8 days of soil water deficit factor (SWFACT) 
! SWMEM8   Average of last 8 days of soil water deficit factor 
! TNFIX    Soil temperature effect on N2 fixation (0-1) 
! TNGRO    Soil temperature effect on nodule growth (0-1) 
! TURFAC   Water stress factor for expansion (0 - 1) 
! TYPFXA   Type of function for canopy age effects on N fixation 
! TYPFXD   Type of function for dry soil effects on N fixation 
! TYPFXT   Type of function for temperature effects on N fixation rates 
! TYPFXW   Type of function for wet soil effects on N fixation 
! TYPNGT   Type of function for temperature effects on Nodule growth rates 
! WTNFX    Cumulative weight of N fixed (g[N] / m2)
! YRDOY    Current day of simulation (YYDDD)
! YRSIM    Start of simulation date (YYDDD)
!-----------------------------------------------------------------------
!       END SUBROUTINE FOR_NFIX
!=======================================================================

