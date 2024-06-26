C=======================================================================
C  PT_ROOTGR, Subroutine
C
C  Determines root growth
C-----------------------------------------------------------------------
C  Revision history
C
C  Written
C  09/  /1988 EA & BB Modified by E. Alocilja & B. Baer 
C  04/  /1989 TJ Modified by T. Jou
C  02/08/1989 PWW Header revision and minor changes 
C  02/08/1993 PWW Added switch block, etc. 
C  08/23/2001 CHP Modified for modular format
C-----------------------------------------------------------------------
C  INPUT  : None
C
C  LOCAL  : RLDF,RNFAC,RLNEW,RLVF,SWDF,TRLDF,RNLF,L,L1
C
C  OUTPUT : None
C-----------------------------------------------------------------------
C  Called : WATBAL
C
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  RLDF() : A root length density factor for soil layer L used to calculate
C           new root growth distribution - unitless
C  RNFAC  : Zero to unity factor describing mineral N availability effect on
C           root growth in Layer L
C  RLNEW  : New root length to be added to the total root system length -
C           cm.  root per sq. cm. ground
C  RLVF   :
C  SWDF   : Soil water deficit factor for Layer L used to calculate root
C           growth and water uptake - unitless value between 0 and 1
C  TRLDF  : An intermediate calculation used to calculate distribution of
C           new root growth in soil
C  RNLF   : Intermediate factor used to calculate distribution of new root
C           growth in the soil - unitless value between 0 and 1
C  L,L1   : Loop counter
C=======================================================================

      SUBROUTINE PT_ROOTGR (DYNAMIC, 
     &    DLAYR, DS, DTT, DUL, FILEIO, GRORT, ISWNIT,     !Input
     &    LL, NH4, NLAYR, NO3, PLTPOP, SHF, SW, SWFAC,    !Input
     &    CUMDEP, RLV, RTDEP)                             !Output

!-----------------------------------------------------------------------
      USE ModuleDefs
      IMPLICIT  NONE
      EXTERNAL PT_IPROOT
      SAVE

      LOGICAL FIRST
      CHARACTER*1   ISWNIT
      CHARACTER*30 FILEIO

      INTEGER DYNAMIC, L, L1, NLAYR

      REAL CUMDEP, DEP, DEPMAX, DTT, GRORT, PLTPOP
      REAL RLINIT, RLNEW, RLWR, RNFAC, RNLF, RTDEP, RTDEPI
      REAL SDEPTH, SWDF, SWFAC, TRLDF, TRLV, RLV_init

      REAL, DIMENSION(NL) :: DLAYR, DS, DUL, ESW, LL 
      REAL, DIMENSION(NL) :: NH4, NO3, RLDF, RLV, SHF, SW
      REAL TotRootMass

!***********************************************************************
!***********************************************************************
!     Seasonal Initialization - Called once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      CALL PT_IPROOT(FILEIO,                    !Input
     &               RLWR, SDEPTH)              !Output

      FIRST = .TRUE.

      DO L = 1, NL
         RLV(L) = 0.0
      END DO

      DEPMAX = DS(NLAYR)
      CUMDEP = 0.0
      RTDEP  = 0.0

!***********************************************************************
!***********************************************************************
!     Daily rate calculations
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!-----------------------------------------------------------------------
!     Initial root distribution:  
      IF (FIRST) THEN
        FIRST  = .FALSE.

!       INITIALIZE ROOT DEPTH AT EMERGENCE
!       After planting date, when Root growth rate >0, could be before Emergence date
!       RTDEPI = SDEPTH  
        RTDEPI = MIN(20.0,DS(NLAYR))     !CHP per JWJ                 
        RTDEP = RTDEPI

!       RLWR 1E4 cm/g
        RLINIT = GRORT * RLWR * PLTPOP 
!   cm[root]     g[root]   1E4 cm[root]   plants   1E-4 m2
!  ----------- = ------- * ------------ * ------ * -------
!  cm2[ground]    plant       g[root]       m2       cm2

!       RLV is a concentration. The value is the same in all soil layers thru
!         the rooting depth at initialization.
        RLV_init = RLINIT / RTDEPI
!      cm[root]    cm[root]       1       cm[soil]
!      --------- = --------- * -------- * --------
!      cm3[soil]   cm2[soil]   cm[soil]   cm[soil]

        CUMDEP = 0.
!       RLINIT is in cm[root]/cm2[ground]
        DO L = 1,NLAYR
          DEP = MIN(RTDEPI - CUMDEP, DLAYR(L))
          CUMDEP = CUMDEP + DEP
          RLV(L) = RLV_init * DEP / DLAYR(L)
          IF (CUMDEP .GE. RTDEPI) EXIT
        ENDDO

!***********************************************************************
      ELSE
!       Daily root growth and distribution
        RLNEW  = GRORT * RLWR * PLTPOP  !CHP    
        TRLDF  = 0.0
        CUMDEP = 0.0
        RNFAC  = 1.0

        DO L = 1, NLAYR
           L1     = L
           ESW(L) = DUL(L) - LL(L)
           CUMDEP = CUMDEP + DLAYR(L)
           SWDF   = 1.0
           IF (SW(L)-LL(L) .LT. 0.25*ESW(L)) THEN
              SWDF = 4.0*(SW(L)-LL(L))/ESW(L)
           ENDIF
           SWDF = AMAX1 (SWDF,0.0)
           IF (ISWNIT .NE. 'N') THEN
!             RNFAC = 1.0 - (1.17 * EXP(-0.15 * TOTIN)
!             RNFAC = 1.0 - (1.17 * EXP(-0.15 * (SNH4(L) + SNO3(L))))
              RNFAC = 1.0 - (1.17 * EXP(-0.15 * (NH4(L) + NO3(L))))
              RNFAC = AMAX1 (RNFAC,0.01)
           ENDIF
           RLDF(L) = AMIN1(SWDF,RNFAC)*SHF(L)*DLAYR(L)
           IF (CUMDEP .LT. RTDEP) THEN
              TRLDF   = TRLDF + RLDF(L)
            ELSE
              RTDEP   = RTDEP + DTT*1.3*AMIN1((SWFAC*2.0),SWDF)
              RTDEP   = AMIN1 (RTDEP,DEPMAX)
              RLDF(L) = RLDF(L)*(1.0-(CUMDEP-RTDEP)/DLAYR(L))
              TRLDF   = TRLDF + RLDF(L)
              EXIT
           END IF
        END DO

!-------------------------------------------------------------------------
        IF (TRLDF .GE. RLNEW*0.00001) THEN
           RNLF = RLNEW/TRLDF
           DO L = 1, L1
              RLV(L) = RLV(L)+RLDF(L)*RNLF/DLAYR(L)-0.005*RLV(L)
              RLV(L) = AMAX1 (RLV(L),0.0)
              RLV(L) = AMIN1 (RLV(L),5.0)
           END DO
        END IF

      ENDIF

      TRLV = 0.0
      DO L = 1, NLAYR
        TRLV = TRLV + RLV(L) * DLAYR(L)
      ENDDO

!     RLWR  Root length to weight ratio, (1E4 cm/g)
      TotRootMass = (TRLV / RLWR) * 10.
!                 cm[root]   g[root]   10000 cm2   10(kg/ha)
!        kg/ha  = -------- * ------- * -------- * ---------
!                cm2[soil]   cm[root]     m2         (g/m2)

!        CumRootMass=CumRootMass+ GRORT * PLTPOP *  10 ! 1 ha = 10000m2
       ! kg[root]       kg        g      # plants     kg/ha
       !----------- = --------+ ------ * --------*  --------
       ! ha             ha       plant      m2         g/m2

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE PT_ROOTGR
C=======================================================================


C=======================================================================
C  PT_IPROOT, Subroutine
C
C  Input data for potato root module
C-----------------------------------------------------------------------
C  Revision history
C
C  08/23/2001 CHP Written
C  10/25/2002 CHP Modified read format for Y2K
C  08/12/2003 CHP Added I/O error checking
C-----------------------------------------------------------------------

      SUBROUTINE PT_IPROOT(FILEIO,                    !Input
     &                     RLWR, SDEPTH)              !Output

!     ------------------------------------------------------------------

      IMPLICIT NONE
      EXTERNAL GETLUN, FIND, ERROR, IGNORE

      INTEGER LUNIO, LUNCRP
      CHARACTER*1, PARAMETER :: BLANK = ' '
      CHARACTER*6, PARAMETER :: ERRKEY = 'ROOTGR'

      CHARACTER*6   SECTION
      CHARACTER*12  FILEC
      CHARACTER*30  FILEIO
      CHARACTER*80  PATHCR
      CHARACTER*92  FILECC
      CHARACTER*180 CHAR

      INTEGER ERR, FOUND, ISECT, LINC, LNUM, PATHL

      REAL RLWR, SDEPTH
!     LOGICAL EOF
!-----------------------------------------------------------------------
!     Read data from FILEIO for use in ROOTGR module
      CALL GETLUN('FILEIO', LUNIO)
      OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,0)

      READ(LUNIO,'(6(/),15X,A12,1X,A80)', IOSTAT=ERR) FILEC, PATHCR
      LNUM = 7
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)

C-----------------------------------------------------------------------
C    Read Planting Details Section
C-----------------------------------------------------------------------
      SECTION = '*PLANT'
      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILEIO, LNUM)
      ELSE
        READ (LUNIO,'(55X,F5.1)', IOSTAT=ERR) SDEPTH ; LNUM = LNUM + 1
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
      ENDIF

      CLOSE (LUNIO)

C-----------------------------------------------------------------------
C     Read Crop Parameters from FILEC
C-----------------------------------------------------------------------
      LNUM   = 0
      PATHL  = INDEX (PATHCR,BLANK)
      IF (PATHL .LE. 1) THEN
         FILECC = FILEC
       ELSE
         FILECC = PATHCR(1:(PATHL-1)) // FILEC
      ENDIF
      CALL GETLUN('FILEC', LUNCRP)
      OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEC,0)

!     EOF not portable. CHP 7/24/2007
!     DO WHILE (.NOT. EOF (LUNCRP))
      DO WHILE (ERR == 0)
        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
!       IF (ISECT .EQ. 0) CALL ERROR(ERRKEY,33,FILECC,LNUM)
        IF (ISECT .EQ. 0) EXIT
        IF (ISECT .EQ. 2) CYCLE
        IF (CHAR(10:13) .EQ. 'RLWR') THEN
          READ (CHAR,'(14X,F6.0)',IOSTAT=ERR) RLWR
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEC,LNUM)
          EXIT
        ENDIF
      ENDDO

      CLOSE (LUNCRP)

C-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE PT_IPROOT
C=======================================================================
!==============================================================================
!-----------------------------------------------------------------------
! Variable definitions
!-----------------------------------------------------------------------
! CUMDEP       The buttom of current row
! DTT          Growing degree days today, degrees C 
! ESW(L)       Plant extractable soil water by layer (= DUL - LL) (cm3/cm3)
! L,L1         Loop counter
! GRORT        Root growth rate, g/plant/day
! NH4(L)       Ammonium N in soil layer L (¦Ìg[N] / g[soil])
! PLTPOP       Plant population (# plants / m2)
! RLDF(L)      A root length density factor for soil layer L used to calculate new root growth distribution 
!              It's intermediat calculated value was in cm, but finally - unitless
! RLINIT       Initial root density (cm[root]/cm2[ground])
! RLNEW        New root growth added to the total root system length (cm[root]/cm2[ground])
! RLV(L)       Root length density for soil layer L (cm[root] / cm3[soil]) 
! RLWR         Root length to weight ration, (10^4 cm[root]/g[root])  
! RNFAC        Zero to unity factor describing mineral N availability effect on
!              root growth in Layer L
! RNLF         Intermediate factor used to calculate distribution of new root. 
! RTDEP        Root length at the begining of the day (cm)
! RTDEPnew     Root length at the end of the day (cm)
! SHF          Soil hospitality factor 0-1,  PT_SUBSTOR.FOR(98): SHF = SOILPROP % WR
! SWDF         Soil water deficit factor for Layer L used to calculate root
!              growth and water uptake - unitless value between 0 and 1   
! SWFAC        Effect of soil-water stress on photosynthesis, 1.0=no stress,0.0=max stress 
! TRLDF        An intermediate calculation used to calculate distribution of
!              new root growth in soil (cm)
! TRLV         Total root length per square cm soil today (cm[root]/cm2[soil])
!***********************************************************************
! END SUBROUTINES PT_ROOTGR, PT_IPROOT
!=======================================================================