C***********************************************************************
C  SoilNi_init_2D, Subroutine
C
C  Purpose: Do soil N initialization for 2D cells.
!  Input layer depths for initial NO3 and NH4 are adjusted for bed
!     construction
C-----------------------------------------------------------------------
C  REVISION HISTORY 
C  06/01/2010 CHP / JZW Written
!  08/15/2011  Move the NH4I and NO3I reading and handling from this subroutine to Subroutine CellInit_2D
!                because the initia condition in SoilCNPinit_C is called by Century and
!                SoilNi_init_2D is after the call of Century
!              Add NH4 and No3 to the argument of this subroutine
C=======================================================================

      SUBROUTINE SoilNi_init_2D(CONTROL, 
     &    Cell_Type, SOILPROP, SOILPROP_profile, ST, NH4, NO3,!Input
     &    NH4_2D, NO3_2D, SNH4_2D, SNO3_2D, TFNITY, UREA_2D) !Output
      
!-----------------------------------------------------------------------
      USE Cells_2D
      IMPLICIT  NONE
      SAVE

      CHARACTER*1 RNMODE 
      CHARACTER*6 ERRKEY, SECTION, DUMMY
      CHARACTER*30 FILEIO
      PARAMETER (ERRKEY = 'SOILNI')

      INTEGER ERRNUM, FOUND, FurRow1, J, L, LNUM, M
      INTEGER LUNIO, NLAYR, NL_init, RUN
      INTEGER, DIMENSION(MaxRows,MaxCols) :: Cell_Type

      REAL BedHt, BedNH4, BedNO3, DepAdjust, DigDep
      REAL, DIMENSION(NL) :: Depth, DS, KG2PPM, NH4, NO3, ST
      REAL, DIMENSION(NL) :: DEP_Adj, NH4_Adj, NO3_Adj
      REAL, DIMENSION(MaxRows,MaxCols) :: NH4_2D, NO3_2D,TFNITY
      REAL, DIMENSION(MaxRows,MaxCols) :: SNH4_2D, SNO3_2D, UREA_2D 

!-----------------------------------------------------------------------
      TYPE (ControlType) CONTROL
      TYPE (SoilType)    SOILPROP, SOILPROP_profile

      FILEIO  = CONTROL % FILEIO
      LUNIO   = CONTROL % LUNIO
      RNMODE  = CONTROL % RNMODE
      RUN     = CONTROL % RUN

      KG2PPM = SOILPROP % KG2PPM    
      NLAYR  = SOILPROP % NLAYR 
      DS     = SOILPROP % DS 

      BedHt  = BedDimension % BedHt
      DigDep = BedDimension % DigDep
      FurRow1= BedDimension % FurRow1

      NH4_2D = 0.0
      NO3_2D = 0.0

!***********************************************************************
      IF (RUN .EQ. 1 .OR. INDEX('QF',RNMODE) .LE. 0) THEN
!!       --------------------------------------------------------------
!!       Open the FILEIO input file.
!        OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT = ERRNUM)
!        IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY, ERRNUM, FILEIO, 0)
!!       --------------------------------------------------------------
!!       Find and Read INITIAL CONDITIONS Section.
!!       --------------------------------------------------------------
!        SECTION = '*INITI'
!        CALL FIND (LUNIO, SECTION, LNUM, FOUND)
!        IF (FOUND .EQ. 0) CALL ERROR (SECTION, 42, FILEIO, LNUM)
!
!!       Read line before layer data 
!        READ (LUNIO, '(A6)', IOSTAT = ERRNUM) DUMMY
!        LNUM = LNUM + 1
!        IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY, ERRNUM, FILEIO, LNUM)

!        NL_init = 0
!        DO L = 1, NL
!          LNUM = LNUM + 1
!          READ(LUNIO,'(F8.0,6X,2F6.0)',IOSTAT=ERRNUM) 
!     &          Depth(L), NH4(L), NO3(L)
!          IF (ERRNUM .NE. 0) EXIT
!          NL_init = NL_init + 1
!          NH4(L) = MAX(NH4(L), 0.0)
!          NO3(L) = MAX(NO3(L), 0.0)
!        ENDDO
!        CLOSE (LUNIO)

!!       Force input depths to go to bottom of profile
!        IF (Depth(NL_init) < DS(NLAYR)) THEN
!          NL_init = NL_init + 1
!          Depth(NL_init) = DS(NLAYR)
!          NH4(NL_init) = 0.0
!          NO3(NL_init) = 0.0
!        ENDIF

!!       If this is a raised bed system, N in bed must be mixed thru
!!         depth of DigDep (depth of excavation for native soil profile
!!         to construct raised bed).
!        IF (BedDimension % RaisedBed) THEN
!          Call MixMassMass(NH4, SOILPROP_profile, DigDep, BedNH4)
!          Call MixMassMass(NO3, SOILPROP_profile, DigDep, BedNO3)

!!         Adjust for input data layers for raised bed construction
!!         Top layer is of bed thickness and uniform properties
!          M = 1         !Input data layers adjusted for raised bed 
!          DEP_ADJ(1) = BedHt
!          NH4_ADJ(1) = BedNH4
!          NO3_ADJ(1) = BedNO3
!
!!         DepAdjust is the additional height above native soil profile 
!!         due to construction of bed
 !         DepAdjust = BedHt - DigDep    ! Jin : bed width may not equal to furrow width??
!          DO L = 1, NL_init
!            IF (Depth(L) < DigDep) CYCLE
!            M = M + 1
!            DEP_ADJ(M) = Depth(L) + DepAdjust
!            NH4_ADJ(M) = NH4(L)
!            NO3_ADJ(M) = NO3(L)
!          ENDDO

!!         LMATCH matches input layer depths to bed soil profile depths
!          CALL LMATCH (M, DEP_ADJ, NH4_ADJ, NLAYR, DS)
!          CALL LMATCH (M, DEP_ADJ, NO3_ADJ, NLAYR, DS)
!
!          NH4 = NH4_ADJ
!          NO3 = NO3_ADJ

!        ELSE
!!         Not raised bed - use soil profile as input, no mixing
!!         LMATCH matches input layer depths to soil profile depths
!          CALL LMATCH (NL_init, Depth, NH4, NLAYR, DS)
!          CALL LMATCH (NL_init, Depth, NO3, NLAYR, DS)
!        ENDIF
        DO L = 1, NRowsTot
!         --------------------------------------------------------------
!         Calculate yesterday's soil temperature factor. When
!         calculating the nitrification, TFNITY will be compared with
!         today's soil temperature factor, and the maximum will apply.
          IF (ST(L) .LT. 5.0) THEN
            TFNITY(L,1) = 0.0
          ELSE
            TFNITY(L,1) = 0.0009766 * ST(L) * ST(L)
          ENDIF

          DO J = 1, NColsTot
            TFNITY(L,J) = TFNITY(L,1)

!           --------------------------------------------------------------
!           Initialize soil mineral nitrogen and urea.
!           --------------------------------------------------------------
            SELECT CASE (Cell_type(L,J))
            CASE (3,4,5)
              NO3_2D(L, J) = NO3(L)
              NH4_2D(L, J) = NH4(L)
            
!             Convert the N concentrations to kg[N] / ha per soil layer.
              SNO3_2D(L, J) = NO3_2D(L, J) / KG2PPM(L) 
              SNH4_2D(L, J) = NH4_2D(L, J) / KG2PPM(L) 
            
!             Initialize urea.
              UREA_2D(L, J) = 0.0
            END SELECT
          ENDDO
        END DO   !End of soil layer loop.
      ENDIF  !End of RUN if-construct

!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE SoilNi_init_2D

!=======================================================================
! SOILNI_2D Variables
!
! ERRKEY     Subroutine name for error file 
! ERRNUM     Error number for input 
! FILEIO     Filename for input file (e.g., IBSNAT35.INP) 
! FOUND      Indicator that good data was read from file by subroutine FIND 
!              (0 - End-of-file encountered, 1 - NAME was found) 
! KG2PPM(L)  Conversion factor to switch from kg [N] / ha to µg [N] / g 
!              [soil] for soil layer L 
! LNUM       Current line number of input file 
! LUNIO      Logical unit number for FILEIO 
! NH4_2D(L,J)Ammonium N in soil layer L (µg[N] / g[soil])
! NL         Maximum number of soil layers = 20 
! NLAYR      Actual number of soil layers 
! NO3_2D(L)     Nitrate in soil layer L (µg[N] / g[soil])
! SECTION    Section name in input file 
! SNH4_2D(L,J) Total extractable ammonium N in soil layer L (kg [N] / ha)
! SNO3_2D(L)   Total extractable nitrate N in soil layer L (kg [N] / ha)
! ST(L)      Soil temperature in soil layer L (°C)
! SW(L)      Volumetric soil water content in layer L
!              (cm3 [water] / cm3 [soil])
! TFNITY(L)  Yesterday’s soil temperature factor for nitrification (range 
!              0-1) 
! UREA_2D(L,J) Amount of urea in soil layer L (kg [N] / ha)
!=======================================================================

!the initial condition for SW and NO3_2D should be two dimensions.