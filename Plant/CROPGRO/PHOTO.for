C=======================================================================
C  PHOTO, Subroutine, K.J.Boote, J.W.Jones, G. Hoogenboom
C  Compute daily photosynthetic using canopy (C) method.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/01/1989 KJB Written.
C  06/02/1993 NBP Made into subroutine.
C  02/11/1994 NBP Corrected dimension (15) for XPGSLW, YPGSLW
C  04/24/1994 NBP Replaced TAIRHR, TAVG with TGRO, TGROAV.
C  09/25/1995 KJB Light interception for ET the same for canopy and leaf
C                 models
C  11/11/1999 CHP Modified for modular format
C  01/13/2000 NBP Added SWFAC effect on PG
C  06/11/2002 GH  Modified for Y2K
C  08/12/2003 CHP Added I/O error checking
C  03/24/2004 CHP Added P stress based on Bostick model
C  07/30/2004 CHP Added KC_SLOPE to SPE file and KC_ECO to ECO file.
!  06/11/2007 CHP PStres1 affects photosynthesis
!-----------------------------------------------------------------------
!  Called from:   Main
!  Calls:         PHOTIP
C=======================================================================

      SUBROUTINE PHOTO(CONTROL, 
     &    BETN, CO2, DXR57, EXCESS, KCAN, KC_SLOPE,       !Input
     &    NR5, PAR, PStres1, SLPF, RNITP, SLAAD,          !Input
     &    SWFAC, TDAY, XHLAI, XPOD,                       !Input
     &  AGEFAC, PG)                                       !Output

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      EXTERNAL PHOTIP, CURV, TABEX
      SAVE

      CHARACTER*3  TYPPGN, TYPPGT
      CHARACTER*30 FILEIO

      INTEGER DYNAMIC
      INTEGER DAS, NR5

      REAL AGEFAC, AGEFCC, AGEREF, A0, BETN, CCEFF, CCK, CCMAX, 
     &  CCMP, CO2, COLDSTR, CUMSTR, CURV, DXR57, EXCESS,
     &  KCAN, KCANR, KC_SLOPE, LMXSTD, LNREF, PAR, PARMAX, PG, PGFAC, 
     &  SLPF, PGLFMX, PGREF, PGSLW, PHTHRS10, PHTMAX, PRATIO, 
     &  PTSMAX, RNITP, ROWSPC, SLAAD, SLW, SPACNG, SWFAC, 
     &  TABEX, TDAY, TPGFAC, XHLAI, XPOD
      REAL E_FAC

      REAL FNPGN(4), FNPGT(4) 
      REAL XPGSLW(15), YPGSLW(15) 

!     Added with P module
      REAL PStres1

!-----------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.

!     The variable "CONTROL" is of type "ControlType".
      TYPE (ControlType) CONTROL

!     Transfer values from constructed data types into local variables.
      DAS     = CONTROL % DAS
      DYNAMIC = CONTROL % DYNAMIC
      FILEIO  = CONTROL % FILEIO

C***********************************************************************
C***********************************************************************
C     Run Initialization - Called once per simulation
C***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
C-----------------------------------------------------------------------
      CALL PHOTIP(FILEIO,  
     &  CCEFF, CCMAX, CCMP, FNPGN, FNPGT, LMXSTD, LNREF, PARMAX,   
     &  PGREF, PHTHRS10, PHTMAX, ROWSPC, TYPPGN, TYPPGT, XPGSLW, YPGSLW)

C-----------------------------------------------------------------------
C     Adjust canopy photosynthesis for GENETIC input value of
C     maximum leaf photosyntheses (LMXSTD).  Exponential curve from
C     from the hedgerow photosynthesis model (Boote et al, 199?).
C-----------------------------------------------------------------------
      IF (PGREF .GT. 0.) THEN
        PGLFMX = (1. - EXP(-1.6 * LMXSTD)) / (1. - EXP(-1.6 * PGREF))
      ELSE
        PGLFMX = 1.0
      ENDIF

C***********************************************************************
C***********************************************************************
C     Seasonal initialization - run once per season
C***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
C-----------------------------------------------------------------------
      AGEFAC  = 1.0
      CUMSTR  = 0.0
      COLDSTR = 0.0
      PG      = 0.0

C***********************************************************************
C***********************************************************************
C     Daily rate calculations
C***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
C-----------------------------------------------------------------------
C     Calculate maximum photosynthesis as function of PAR, g CH2O/m2
C-----------------------------------------------------------------------
      PTSMAX = PHTMAX * (1.0 - EXP(-(1.0 / PARMAX) * PAR))

C-----------------------------------------------------------------------
C     Calculate reduction in photosynthesis due to incomplete canopy.
C-----------------------------------------------------------------------
      IF (BETN .LE. ROWSPC) THEN
        SPACNG = BETN / ROWSPC
      ELSE
        SPACNG = ROWSPC / BETN
      ENDIF
!chp per CDM:      KCANR = KCAN - (1. - SPACNG) * 0.1
      KCANR = KCAN - (1. - SPACNG) * KC_SLOPE
      PGFAC = 1. - EXP(-KCANR * XHLAI)

C-----------------------------------------------------------------------
C     Compute reduction in PG based on the average daylight temperature.
C-----------------------------------------------------------------------
      TPGFAC = CURV(TYPPGT,FNPGT(1),FNPGT(2),FNPGT(3),FNPGT(4),TDAY)

C-----------------------------------------------------------------------
C     Compute reduction in PG as a function of leaf N concentration.
C-----------------------------------------------------------------------
      AGEFAC = CURV(TYPPGN,FNPGN(1),FNPGN(2),FNPGN(3),FNPGN(4),RNITP)
      AGEREF = CURV(TYPPGN,FNPGN(1),FNPGN(2),FNPGN(3),FNPGN(4),LNREF)
      AGEFAC = AGEFAC / AGEREF

C-----------------------------------------------------------------------
C     9/24/95 KJB,JWJ Decrease sensitivity of canopy PG to N function
C     to mimic behavior of leaf version.  AGEFAC corresponds to leaf
C     PG vs. leaf N. Function does not act strongly on QE, thus, we
C     need to scale effect back on daily canopy version.
C-----------------------------------------------------------------------
      AGEFCC = (1.0 - EXP(-2.0 * AGEFAC)) / (1. - EXP(-2.0 * 1.0))

C-----------------------------------------------------------------------
C     Compute canopy pg response to changes in specific leaf weight.
C     (Dornhoff and Shibles, 197?).
C-----------------------------------------------------------------------
      IF (SLAAD .GT. 0.0) THEN
        SLW = 1. / SLAAD
      ELSE
        SLW = 0.0099
      ENDIF
      PGSLW = TABEX(YPGSLW, XPGSLW, SLW, 10)

C-----------------------------------------------------------------------
C     Adjust canopy photosynthesis for CO2 concentration assuming a
C     reference value of CO2 of 330 ppmv.
C-----------------------------------------------------------------------
      CCK = CCEFF / CCMAX
      A0 = -CCMAX * (1. - EXP(-CCK * CCMP))
      PRATIO = A0 + CCMAX * (1. - EXP(-CCK * CO2))

!***********************************************************************
!***********************************************************************
!     Daily integration
!***********************************************************************
!      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------
C     Effect of daylength on daily gross PG, computer by KJB with
C     stand alone model, and used by Ernie Piper in his dissertation
C     see page 127 for the equation and conditions.  Nornamized to
C     about 13 hours where the function is 1.00.  Actually
C     normalized to a bit higher between 13 and 14 hours.
C
C     DLFAC = 1.0 + 0.6128 - 0.01786*DAYL + 0.006875*DAYL*DAYL
C    &        - 0.000247*DAYL*DAYL*DAYL
C
C     Compute daily gross photosynthesis (g CH2O/m2/d)
C-----------------------------------------------------------------------
!      PG =  PTSMAX * SLPF * PGFAC * TPGFAC * AGEFCC * PGSLW

!      PG =  PTSMAX * SLPF * PGFAC * TPGFAC * MIN(AGEFCC, PSTRES2) * 
!     &            PGSLW * PRATIO * PGLFMX * SWFAC

!     CHP 05/07/2004 
!     AGEFCC can be > 1.0, so don't want to use minimum of 
!     PStres1 and AGEFCC.  (PStres1 is always 1.0 or below).
      IF (AGEFCC .GE. 1.0) THEN
        E_FAC = AGEFCC * PStres1
      ELSE
        E_FAC = MIN(AGEFCC, PStres1)
      ENDIF

      PG =  PTSMAX * SLPF * PGFAC * TPGFAC * E_FAC * 
     &            PGSLW * PRATIO * PGLFMX * SWFAC

!From WDB (chp 10/21/03):
!        PG = PG * MIN(SWFAC ,2*(1-SATFAC) )
!        PGN = PGN * MIN(SWFAC,2*(1-SATFAC) )

C-----------------------------------------------------------------------
C     9/27/95 KJB added cumulative water stress effect on PG after R5.
C     CUMULATIVE STRESS (WATER, TEMP) UPON PG CAPACITY.  REDUCE FROM POT.
C     PG AFTER R5, WITH TWO FUNCTIONS.  ONE DEPENDING ON THE PRIMARY
C     STRESS AND THE OTHER DEPENDING ON DISTANCE FROM R5 TO R7, DXR57
C     INITIALLY THE STRESS IS SOIL WATER, BUT THIS MAY WORK FOR COLD TEMP.
C     0.5 IS A SCALAR, POSSIBLY COULD GO HIGHER.  MAX CUMSTR IS 0.2856
C     FOR 78-RF, 0.1858 FOR EGLIN-88, THEN 0.528 FOR 84-RF.  DECREASES
C     YIELD 77, 52, 50, AND 16 kg/ha FOR 78RF, EGLIN-88, 84RF, AND 81REP
C     MINOR DECREASE IN SEED SIZE.
C     1/19/96, USING XPOD CAUSES IT TO BE LESS EFFECTIVE EARLY UNTIL FULL
C     PARTITIONING TO POD OCCURS (MAYBE JUST SEED, SEE XPOD CALCULATION).
C     12/19/95 Changed scalar to 0.4.  Too strong for peanut.  Also,
C     2/6/96 Changed scalar to 0.3.  Too strong for 78RF too. Also,
C     the problem is really with seed growth potential, not as much on PG.
C-----------------------------------------------------------------------
C     NEEDS A FUNCTION.  SEE TMIN AND CHILL IN LEAF SUBROUTINE
C     AND THEN ADD A SCALAR?
C       COLDSTR =  COLDSTR + DXR57 * (F(TMIN?)*XPOD / PHTHRS(10)
C       PG = PG * (1.0 - MAX(0.4*CUMSTR,1.0*COLDSTR))
C-----------------------------------------------------------------------
      IF (DAS .GT. NR5) THEN
        CUMSTR =  CUMSTR + DXR57 * (1.0 - SWFAC) * XPOD / PHTHRS10
        COLDSTR = 0.0
        PG = PG * (1.0 - 0.3 * CUMSTR)
      ELSE
        CUMSTR = 0.0
        COLDSTR = 0.0
      ENDIF

      PG = PG * EXCESS

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END !SUBROUTINE PHOTO

C=======================================================================
C  PHOTIP, Subroutine, N.B. Pickering
C  Read input parameters for daily photosynthesis.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  09/25/99     Written.
!-----------------------------------------------------------------------
C  Output:
C  Local :
!-----------------------------------------------------------------------
!  Called: PG
!  Calls : None
C=======================================================================

      SUBROUTINE PHOTIP(FILEIO,  
     &  CCEFF, CCMAX, CCMP, FNPGN, FNPGT, LMXSTD, LNREF, PARMAX,   
     &  PGREF, PHTHRS10, PHTMAX, ROWSPC, TYPPGN, TYPPGT, XPGSLW, YPGSLW)

!-----------------------------------------------------------------------
      IMPLICIT NONE
      EXTERNAL GETLUN, FIND, ERROR, IGNORE

      CHARACTER*1  BLANK
      CHARACTER*3  TYPPGN, TYPPGT
      CHARACTER*6  ERRKEY, SECTION
      CHARACTER*12 FILEC
      CHARACTER*30 FILEIO
      CHARACTER*80 PATHCR,CHAR
      CHARACTER*92 FILECC

      INTEGER LUNIO, LINC, LNUM, FOUND
      INTEGER II, PATHL, LUNCRP, ERR, ISECT

      REAL CCEFF, CCMAX, CCMP, LMXSTD, LNREF, 
     &  PARMAX, PGREF, PHTHRS10, PHTMAX, ROWSPC, XPGSLW(15)

      REAL FNPGN(4),FNPGT(4)
      REAL YPGSLW(15)

      PARAMETER (BLANK  = ' ')
      PARAMETER (ERRKEY = 'PHOTIP')

!-----------------------------------------------------------------------
!     Open and read FILEIO
!-----------------------------------------------------------------------
      CALL GETLUN('FILEIO', LUNIO)
      OPEN (LUNIO, FILE = FILEIO,STATUS = 'OLD',IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,0)

!-----------------------------------------------------------------------
      READ(LUNIO,50,IOSTAT=ERR) FILEC, PATHCR; LNUM = 7
   50 FORMAT(//////,15X,A12,1X,A80)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)

!-----------------------------------------------------------------------
C    Read Planting Details Section
!-----------------------------------------------------------------------
      SECTION = '*PLANT'
      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILEIO, LNUM)
      ELSE
        READ(LUNIO,'(42X,F6.0)',IOSTAT=ERR) ROWSPC ; LNUM = LNUM + 1
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
      ENDIF

C     CROPGRO uses ROWSPC as m
      ROWSPC = ROWSPC / 100.

!-----------------------------------------------------------------------
C    Read Cultivars Section
!-----------------------------------------------------------------------
      SECTION = '*CULTI'
      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILEIO, LNUM)
      ELSE
        READ(LUNIO,'(60X,F6.0,6X,F6.0)',IOSTAT=ERR) PHTHRS10, LMXSTD
        LNUM = LNUM + 1
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
      ENDIF

C-----------------------------------------------------------------------
      CLOSE (LUNIO)

C-----------------------------------------------------------------------
      LNUM = 0
      PATHL  = INDEX(PATHCR,BLANK)
      IF (PATHL .LE. 1) THEN
        FILECC = FILEC
      ELSE
        FILECC = PATHCR(1:(PATHL-1)) // FILEC
      ENDIF
      CALL GETLUN('FILEC', LUNCRP)
      OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,0)

C-----------------------------------------------------------------------
C READ PHOTOSYNTHESIS PARAMETERS *******************
C-----------------------------------------------------------------------
    5 CONTINUE
      CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
      IF (ISECT .EQ. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
      IF (ISECT .EQ. 2) GO TO 5
      READ(CHAR,'(5F6.2)',IOSTAT=ERR) PARMAX, PHTMAX
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

      CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
      READ(CHAR,'(3F6.1)',IOSTAT=ERR) CCMP, CCMAX, CCEFF
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

      CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
      READ(CHAR,'(4F6.0,3X,A3)',IOSTAT=ERR) (FNPGN(II),II=1,4), TYPPGN
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

      CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
      READ(CHAR,'(4F6.0,3X,A3)',IOSTAT=ERR) (FNPGT(II),II=1,4), TYPPGT
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

      CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
      CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
      CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
      CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
      CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
      READ(CHAR,'(18X,2F6.0)',IOSTAT=ERR) LNREF, PGREF
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

      CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
      READ(CHAR,'(10F6.0)',IOSTAT=ERR) (XPGSLW(II),II = 1,10)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

      CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
      READ(CHAR,'(10F6.0)',IOSTAT=ERR) (YPGSLW(II),II = 1,10)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

C-----------------------------------------------------------------------
      CLOSE (LUNCRP)

      END !SUBROUTINE PHOTIP
!=======================================================================
! Variable definitions for PHOTO and PHOTIP
!=======================================================================
! AGEFAC   Relative effect of current leaf N on canopy photosynthesis (0-1)
!            (fraction)
! AGEFCC   Effect of AGEFAC on photosynthesis 
! AGEREF   Reference value calculated at reference leaf N value to 
!            normalize the effect of N for different plant species 
! BETN     Spacing between plants along a row (m / plant)
! BLANK    Blank character 
! CCEFF    Relative efficiency of CO2 assimilation used in equation to 
!            adjust canopy photosynthesis with CO2 concentrations 
! CCK      Computed exponent for relationship between CO2 and canopy 
!            photosynthesis (=CCEFF / CCMAX) 
! CCMAX    Maximum daily canopy photosynthesis relative to photosynthesis 
!            at a CO2 concentration of 330 vpm 
! CCMP     Canopy CO2 compensation point (CO2 at which daily PG is 0.0) 
! CHAR     Contains the contents of last record read 
! CO2      Atmospheric carbon dioxide concentration (µmol[CO2] / mol[air])
! COLDSTR  Cold weather stress factor for photosynthesis (not currently 
!            used) 
! CUMSTR   Cumulative stress factor for photosynthesis after start of seed 
!            production 
! CURV     Function subroutine 
! DAS      Days after start of simulation (d)
! DXR57    Relative time between first seed (NR5) and physiological 
!            maturity (NR7) 
! ERR      Error code for file operation 
! ERRKEY   Subroutine name for error file 
! EXCESS   Factor based on excess PG used to affect tomorrow's PG 
!            calculation 
! FILEC    Filename for SPE file (e.g., SBGRO980.SPE) 
! FILECC   Path plus filename for species file (*.spe) 
! FILEIO   Filename for input file (e.g., IBSNAT35.INP) 
! FNPGN(I) Critical leaf N concentration for function to reduce 
!            photosynthesis due to low leaf N levels (4 values for function 
!            CURV) 
! FNPGT(I) Critical values of temperature for the functions to reduce 
!            canopy PG under non-optimal temperatures (in function CURV) 
! FOUND    Indicator that good data was read from file by subroutine FIND 
!            (0 - End-of-file encountered, 1 - NAME was found) 
! ISECT    Indicator of completion of IGNORE routine: 0 - End of file 
!            encountered, 1 - Found a good line to read, 2 - End of Section 
!            in file encountered denoted by * in column 1.  
! KCAN     Canopy light extinction coefficient for daily PAR, for 
!            equidistant plant spacing, modified when in-row and between 
!            row spacing are not equal 
! KCANR    Canopy light extinction coefficient, reduced for incomplete 
!            canopy 
! LMXSTD   Maximum leaf photosyntheses for standard cultivar 
! LNREF    Value of leaf N above which canopy PG is maximum (for standard 
!            cultivar) 
! LNUM     Current line number of input file 
! LUNCRP   Logical unit number for FILEC (*.spe file) 
! LUNIO    Logical unit number for FILEIO 
! NR5      Day when 50% of plants have pods with beginning seeds (days)
! PAR      Daily photosynthetically active radiation or photon flux density
!            (moles[quanta]/m2-d)
! PARMAX   Value of PAR at which photosynthesis is 63% of the maximum value 
!            (PHTMAX). Used in daily canopy photosynthesis calculations
!            (moles[quanta]/m2-d)
! PATHCR   Pathname for SPE file or FILEE. 
! PATHL    Number of characters in path name (path plus filename for FILEC)
!            
! PG       Daily gross photosynthesis (g[CH2O] / m2 / d)
! PGFAC    Multiplier to compute daily canoy PG as a function of leaf area 
!            index (LAI) 
! PGLFMX   Multiplier for daily canopy photosynthesis to account for 
!            cultivar differences in leaf photosynthesis capabilities 
! PGREF    Reference value for leaf level photosynthesis used in canopy 
!            light response curve (µmol[CO2] / m2-s)
! PGSLW    Relative effect of leaf thickness (SLW) on daily canopy PG 
! PHTHRS10 Threshold time that must accumulate in phase 10 for the next 
!            stage to occur.  Equivalent to PHTHRS(10) in Subroutine 
!            PHENOLOG. 
! PHTMAX   Maximum amount of CH20 which can be produced if 
!            photosynthetically active radiation (PAR) is very high (3 
!            times PARMAX) and all other factors are optimal (g[CH2O]/m2-d)
! PRATIO   Relative effect of CO2 on canopy PG, for multiplying by PG 
!            computed for 330 vpm 
! PTSMAX   Potential amount of CH20 which can be produced at the specified 
!            PAR for full canopy (LAI>8), all other factors optimal
!            (g[CH2O]/m2-d)
! RNITP    True nitrogen concentration in leaf tissue for photosynthesis 
!            reduction. (%)
! ROWSPC   Row spacing (m)
! SECTION  Section name in input file 
! SLAAD    Specific leaf area, excluding weight of C stored in leaves
!            (cm2[leaf] / g[leaf])
! SLPF     Empirical multiplier to adjust daily canopy PG due to unknown 
!            soil or environmental factors that persist in a given location 
! SLW      Specific leaf weight (g[leaf] / m2[leaf])
! SPACNG   Ratio of distance between plants in a row to distance between 
!            rows (or vice versa - always < 1) 
! SWFAC    Effect of soil-water stress on photosynthesis, 1.0=no stress, 
!            0.0=max stress 
! TABEX    Function subroutine - Lookup utility 
! TDAY     Average temperature during daylight hours (°C)
! TIMDIF   Integer function which calculates the number of days between two 
!            Julian dates (da)
! TPGFAC   Reduction in specific leaf area due to daytime temperature being 
!            less than optimal (0-1) 
! TYPPGN   Type of function for the leaf N effects on PG 
! TYPPGT   Character variable specifying the type of function to use for 
!            the relationship between temperature and PG (for use in 
!            function subroutine CURV) 
! XHLAI    Leaf area index (m2[leaf] / m2[ground])
! XPGSLW(I) Array of SLW values for table look-up function, used with YPGSLW
!            (g[leaf] / m2[leaf])
! XPOD     Growth partitioning to pods which slows node appearance
!            (fraction)
! YPGSLW(I) Array of PG values corresponding to SLW values in array XPGSLW
!            (g[CH2O] / m2 / d)
!=======================================================================
! END SUBROUTINE PHOTO
!=======================================================================
