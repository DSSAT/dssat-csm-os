!=======================================================================
!  RootSoilVol, Subroutine, C.H.Porter
!-----------------------------------------------------------------------
!  Determines volume of soil in direct contact with roots for use by
!    soil phosphorus module.
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  01/24/2005 CHP Written
!  06/17/2006 AJG Renamed all root-zone and no-root-zone variables with
!                 Rts and noRts.
!  03/20/2007 CHP Initialize root soil volume prior to plant growth
!                 for banded or hill fertilizer applications.
!  03/21/2007 CHP Keep FracRts as fraction of total layer depth, 
!                 rather than SVRts as absolute volume. This is for 
!                 conservation of mass with tillage and compaction.
!  04/10/2008 CHP / US Root radius is read from species file
!=====================================================================

      SUBROUTINE RootSoilVol(DYNAMIC, ISWPHO,   
     &    DLAYR, DS, NLAYR,           !Input from all routines
     &    PLTPOP, RLV, RTDEP, FILECC, !Input from plant routine
     &    FracRts,                    !Output
!     List this one last - plant routines do not need to use it
     &    LAYER, AppType)      !Input from soil module (for banded fert)

!-----------------------------------------------------------------------
      USE ModuleDefs 
      USE ModuleData
      IMPLICIT NONE
      EXTERNAL FIND, GETLUN, WARNING, ERROR, IGNORE, INFO
      SAVE

!     Input (required)
      CHARACTER*1,         INTENT(IN)           :: ISWPHO
      INTEGER,             INTENT(IN)           :: DYNAMIC, NLAYR
      REAL, DIMENSION(NL), INTENT(IN)           :: DS, DLAYR

!     Output (required)
      REAL, DIMENSION(NL), INTENT(OUT)          :: FracRts

!     Input from Plant module (not used when called from Soil routine)
      REAL,                INTENT(IN), OPTIONAL :: PLTPOP, RTDEP
      REAL, DIMENSION(NL), INTENT(IN), OPTIONAL :: RLV
      CHARACTER*92,        INTENT(IN), OPTIONAL :: FILECC
!     Input from soil module (used for banded or hill fertilizer applications)
      INTEGER,             INTENT(IN), OPTIONAL :: LAYER
      CHARACTER*7,         INTENT(IN), OPTIONAL :: AppType

!-----------------------------------------------------------------------
      CHARACTER*6, PARAMETER :: ERRKEY='RTSOLV'
      CHARACTER*6 SECTION
      CHARACTER*30 FILEIO
      CHARACTER*78 MSG(5) 
      CHARACTER*120 CHAR

      INTEGER ERR, FOUND, ISECT
      INTEGER L, LNUM, LUNIO, LUNCRP

      REAL FracRootVol, PlantPop, PLANTS, RLVTOT, ROWSPC, Z

      LOGICAL FIRST
      TYPE (ControlType) CONTROL

!     Radius of soil around new root growth (m)
!     CHP / US Changed root radius to 3mm 3/27/2008
!     4/8/2008 CHP / US Root radius is a species parameter
!     REAL, PARAMETER :: ROOTRAD = 0.0028 !0.002 m = 2 mm
      REAL ROOTRAD
!      REAL, PARAMETER :: PI = 3.14159

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!     Called from plant routines
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      FracRts = 0.0
      IF (ISWPHO .EQ. 'N') RETURN

      CALL GET(CONTROL)
      FILEIO = CONTROL % FILEIO
      LUNIO  = CONTROL % LUNIO
!-----------------------------------------------------------------------
!     Open and read FILEIO to get ROWSPC
      OPEN (LUNIO, FILE = FILEIO,STATUS = 'OLD',IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,0)

!     Read Planting Details Section
      SECTION = '*PLANT'
      CALL FIND(LUNIO, SECTION, LNUM, FOUND) 
      IF (FOUND .EQ. 0) CALL ERROR(SECTION, 42, FILEIO, LNUM)
      READ(LUNIO,'(18X,2F6.0,12X,F6.0)',IOSTAT=ERR) 
     &    PLANTS, PlantPop, ROWSPC
      LNUM = LNUM + 1
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
      CLOSE (LUNIO)

!     If Planting population not defined, use PLTPOP (at emergence).
!     This is used if P fertilizer is applied in hills prior to planting.
      IF (PlantPop < 1.E-3) PlantPop = PLANTS 

!     CROPGRO uses ROWSPC as m
      ROWSPC = ROWSPC / 100.
      FIRST = .TRUE.

!     ------------------------------------------------------------------
!     Read Species file for P parameters
      CALL GETLUN('FILEC', LUNCRP)
      OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,0)

!     Find and Read Phosphorus Section
      SECTION = '*PHOSP'
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        MSG(1)='Phosphorus input section not found in species file.'
        MSG(2)=FILECC
        MSG(3)= 'Can not simulate phosphorus for this crop. ' //
     &          'Program will stop.'
        CALL WARNING(3,ERRKEY,MSG)
        CALL ERROR(SECTION, 42, FILECC, LNUM)
      ENDIF

      ROOTRAD = -99.
!     Look for ROOTRAD text
      DO WHILE (ERR == 0)
        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        IF (ISECT /= 1) EXIT
        IF (INDEX(CHAR,"ROOTRAD") > 0) THEN
          READ(CHAR,'(F8.0)',IOSTAT=ERR) ROOTRAD
          IF (ERR == 0) EXIT
        ENDIF
      ENDDO

      IF (ROOTRAD < 1.E-6) ROOTRAD = 0.002 !default value 2mm
      WRITE(MSG(1),'(A,F7.4,A)') 
     &  "Radius around root from which P can be extracted = ", 
     &  ROOTRAD, " mm"
      CALL INFO(1,ERRKEY,MSG)

      CLOSE (LUNCRP)

!***********************************************************************
!***********************************************************************
!     Rate Calculations 
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE .OR. DYNAMIC .EQ. INTEGR) THEN
C-----------------------------------------------------------------------
      IF (ISWPHO .EQ. 'N') RETURN

      IF (PRESENT(LAYER)) THEN
!       Called from soil routines - initialize for banded or hill fert.
        SELECT CASE (TRIM(AppType))
        CASE ('HILL', 'POINT')    ! 5cm x 5cm square
          FracRootVol = 0.05 * 0.05 * PlantPop

        CASE DEFAULT   !('BANDED ')    ! 5cm band
          IF (ROWSPC > 1.E-6) THEN
            FracRootVol = 0.05 / ROWSPC
          ELSE
            FracRootVol = 0.05
          ENDIF
        END SELECT
        FracRts(LAYER) = MAX(FracRts(LAYER), FracRootVol)
        FracRts(LAYER) = MIN(FracRts(LAYER), 1.0)

      ELSE
!       Called from plant routines
!       If no roots, return
        RLVTOT = SUM(RLV)
        IF (RLVTOT < 1.E-5) THEN
          FIRST = .TRUE.
        
        ELSEIF (FIRST) THEN
!         Initialize soil volume upon emergence
          DO L = 1, NLAYR
            IF (L == 1 .OR. DS(L) < RTDEP) THEN 
              Z = DLAYR(L)
            ELSE 
              Z = RTDEP - DS(L-1)
            ENDIF
      
!           Assume plant roots are initially in contact with a volume 
!             of soil of 5cm x 5cm 
            FracRootVol = (0.05 * 0.05 * Z / DLAYR(L)) * PLTPOP
!             fraction  =   m   *   m  *  fraction     *  m-2

!           3/20/2007 May have early initialization of root volume for 
!             banded fertilizer. Don't decrease value from that.
            FracRts(L) = MAX(FracRts(L), FracRootVol)

!           Plant spacing may limit root growth volume per plant:
            FracRts(L) = MIN(FracRts(L), 1.0, 
     &        Z / DLAYR(L) * 0.05 / ROWSPC)
!    &        Z / DLAYR(L) * 0.05 * (1./ (ROWSPC * PLTPOP)) * PLTPOP)
!               fraction   *   m  *          m              *  m-2
      
            IF (Z < DLAYR(L)) EXIT
          ENDDO
          FIRST = .FALSE.
      
        ELSE 
!         Increase in soil volume adjacent to roots subsequent to
!         initialization at emergence
          DO L = 1, NLAYR
! Note: need to bring in actual new growth.  Today's RLV includes new growth
! plus losses due to senescence.  Will underestimate the supply of P adjacent
! to new roots.
            FracRootVol = RLV(L) * PI * ROOTRAD * ROOTRAD * 1.E4
!             fraction  = cm/cm3 *         m     *   m    * cm2/m2
            FracRts(L) = MAX(FracRootVol, FracRts(L))
            FracRts(L) = MIN(1.0, FracRts(L))
            IF (FracRts(L) > .99) FracRts(L) = 1.0
          ENDDO
        ENDIF
      ENDIF

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************

      RETURN
      END SUBROUTINE RootSoilVol

!=======================================================================
! Input Variables:		
! DLAYR(L) Thickness of soil layer L	cm
! DS(L)    Depth to bottom of soil layer L	cm
! ISWPHO   Phosphorus simulation switch (Y or N)	--
! NLAYR    Actual number of soil layers	--
! PLTPOP   Plant Population	#/m2
! RLV(L)   Root length density for soil layer L	cm[root] / cm3[soil]
! RTDEP    Root depth	cm

! Output Variables:	
! FracRts  Fraction of soil volume in direct contact with roots	
!=======================================================================
