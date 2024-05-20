!***********************************************************************
!  SoilNi_init, Subroutine
!
!  Purpose: Do soil N initialization for 1D layers and 2D cells.
!  This was modified from SOILNI when NTRANS was split into organic and 
!    inorganic sections.  SOILNI was split into SOILNI_inorganic and 
!    SOILNI_organic.
!  Input layer depths for initial NO3 and NH4 are adjusted for bed
!     construction.
!-----------------------------------------------------------------------
!  REVISION HISTORY 
!  02/08/1993 PWW Header revision and minor changes.
!  02/20/1996 GH  Written.
!  02/26/1998 WTB Fixed HUMC/HUMN calculations.
!  06/09/1999 AJG Completely revised the soil N and SOM module, and made
!               a new SOM module based on the CENTURY model.
!  06/21/1999 CHP Modular format
!  03/16/2000 GH  Incorporated in CROPGRO
!  06/11/2002 GH  Modified for Y2K
!  08/12/2003 CHP Added I/O error checking
!                 No re-initialization done for sequenced runs
!  01/14/2005 CHP/UPS Split NTRANS into separate organic and 
!                  inorganic routines.
!  06/01/2010 CHP / JZW Modified for 2D model initialization
!  08/15/2011  Move the NH4I and NO3I reading and handling from this subroutine to CellInit_2D
!                because the initial condition in SoilCNPinit_C is called by Century and
!                SoilNi_init_2D is after the call of Century
!              Add NH4 and No3 to the argument of this subroutine
!  10/31/2023 CHP integrated 1D and 2D versions.
!=======================================================================

      SUBROUTINE SoilNi_init(CONTROL, 
     &    Cell_Type, SOILPROP, ST, NH4, NO3,      !Input
     &    NH4_2D, NO3_2D, SNH4, SNH4_2D, SNO3,    !Output
     &    SNO3_2D, TFNITY, UPPM, UREA, UREA_2D)   !Output

!-----------------------------------------------------------------------
      USE Cells_2D
      IMPLICIT  NONE
      SAVE

!     2D variables
      INTEGER, DIMENSION(MaxRows,MaxCols) :: Cell_Type
      REAL, DIMENSION(MaxRows,MaxCols) :: ColFrac, BedFrac
      REAL, DIMENSION(MaxRows,MaxCols) :: NH4_2D, NO3_2D, TFNITY
      REAL, DIMENSION(MaxRows,MaxCols) :: SNH4_2D, SNO3_2D, UREA_2D

      CHARACTER*1 RNMODE 

      INTEGER L,J  !ERRNUM, FOUND, LNUM
      INTEGER RUN

      REAL KG2PPM(NL)
      REAL NH4(NL), NO3(NL)
      REAL SNH4(NL), SNO3(NL)
      REAL UREA(NL), UPPM(NL)
      REAL ST(NL) 

!-----------------------------------------------------------------------
!     Constructed variables are defined in ModuleDefs.
      TYPE (ControlType) CONTROL
      TYPE (SoilType)    SOILPROP

!     Transfer values from constructed data types into local variables.
      RNMODE  = CONTROL % RNMODE
      RUN     = CONTROL % RUN
      KG2PPM = SOILPROP % KG2PPM    
      ColFrac= BedDimension % ColFrac
      BedFrac= BedDimension % BedFrac

      NH4_2D = 0.0
      NO3_2D = 0.0
      UREA_2D = 0.0
      UREA = 0.0
      Uppm = 0.0

!***********************************************************************
      IF (RUN .EQ. 1 .OR. INDEX('QF',RNMODE) .LE. 0) THEN
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

!         Convert the N concentrations to kg[N] / ha per soil layer.
          SNO3(L) = NO3(L) / KG2PPM(L)
          SNH4(L) = NH4(L) / KG2PPM(L)

          DO J = 1, NColsTot
            TFNITY(L,J) = TFNITY(L,1)
!           --------------------------------------------------------------
!           Initialize soil mineral nitrogen and urea.
!           --------------------------------------------------------------
            NO3_2D(L, J) = NO3(L)
            NH4_2D(L, J) = NH4(L)
!           Convert the N concentrations to kg[N] / ha per soil layer.
            SELECT CASE (Cell_type(L,J))
            CASE (3)
              SNO3_2D(L, J) = SNO3(L) * BedFrac(L,J)
              SNH4_2D(L, J) = SNH4(L) * BedFrac(L,J)
            CASE(4,5)
              SNO3_2D(L, J) = SNO3(L) * ColFrac(L,J)
              SNH4_2D(L, J) = SNH4(L) * ColFrac(L,J)
            END SELECT
          ENDDO
        END DO   !End of soil layer loop.
      ENDIF  !End of RUN if-construct

!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE SoilNi_init

!=======================================================================
! SOILNI Variables
!
! KG2PPM(L)  Conversion factor to switch from kg [N] / ha to µg [N] / g 
!              [soil] for soil layer L 
! NH4(L)     1D Ammonium N in soil layer L (µg[N] / g[soil])
! NH4_2D(L,J)2D Ammonium N in soil cell L,J (µg[N] / g[soil])
! NLAYR      Actual number of soil layers 
! NO3(L)     1D Nitrate in soil layer L (µg[N] / g[soil])
! NO3_2D(L)  2D Nitrate in soil cell L,J (µg[N] / g[soil])
! SECTION    Section name in input file 
! SNH4(L)    1D Total extractable ammonium N in soil layer L (kg [N] / ha)
! SNH4_2D(L,J) Total extractable ammonium N in cell L,J (kg [N] / ha)
! SNO3(L)    1D Total extractable nitrate N in soil layer L (kg [N] / ha)
! SNO3_2D(L) 2D Total extractable nitrate N in cell L,J (kg [N] / ha)
! ST(L)      Soil temperature in soil layer L (°C)
! SW(L)      Volumetric soil water content in layer L
!              (cm3 [water] / cm3 [soil])
! TFNITY(L)  Yesterday’s soil temperature factor for nitrification (range 0-1) 
! UREA(L)    1D Amount of urea in soil layer L (kg [N] / ha)
! UREA_2D(L,J) 2D Amount of urea in soil cell L,J (kg [N] / ha)
!=======================================================================
