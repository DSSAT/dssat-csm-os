C=======================================================================
C  NUPTAK_2D, Subroutine
C  Determines N uptake (adapted from CERES)
C-----------------------------------------------------------------------
C  Revision history
C  09/01/1989 JWJ,GH Written
C  03/01/1993 WTB Modified.
C  01/20/1997 GH  Modified.
C  07/10/1998 CHP modified for modular format.
C  05/11/1998 GH  Incorporated in CROPGRO
!  07/23/2010 JZW, CHP converted to 2D
!  06/30/2011 Rename RowFrac to ColFrac 
C-----------------------------------------------------------------------
C  Called from:  CROPGRO
C=======================================================================

      SUBROUTINE NUPTAK_2D(DYNAMIC,
     &    CELLS, DLAYR, DUL, FILECC, KG2PPM, LL,          !Input
     &    NDMSDR, NDMTOT, SAT,                            !Input  
     &    TRNH4U, TRNO3U, TRNU, UNH4, UNO3)               !Output

!-----------------------------------------------------------------------
      USE Cells_2D
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      EXTERNAL GETLUN, ERROR, FIND, IGNORE
      SAVE
      
      Type (CellType) Cells(MaxRows,MaxCols)
!     INTEGER, DIMENSION(MaxRows,MaxCols) :: Cell_Type 
      REAL, DIMENSION(MaxRows, MaxCols) :: ColFrac
      CHARACTER*6 ERRKEY
      PARAMETER (ERRKEY = 'NUPTAK')
      CHARACTER*6 SECTION
      CHARACTER*80 CHAR
      CHARACTER*92 FILECC

      INTEGER I, J, LUNCRP, ERR, LNUM, ISECT, FOUND, L, DYNAMIC, FurCol1
!     INTEGER NLAYR

      REAL NUF, XMIN, HalfRow, BEDWD
      REAL DLAYR(NL), LL(NL), DUL(NL), SAT(NL) !, SW(NL)
      REAL KG2PPM(NL)
      REAL TRNO3U, TRNH4U, TRNU
      REAL NDMTOT, NDMSDR, ANDEM, FNH4, FNO3, SMDFR, RFAC
      REAL RTNO3, RTNH4, MXNH4U, MXNO3U
      REAL, DIMENSION(MaxRows,MaxCols) :: NO3_2D, NH4_2D, RLV_2D
      REAL, DIMENSION(MaxRows,MaxCols) :: SNO3_2D, SNH4_2D, SWV,RNH4U_2D
      REAL, DIMENSION(MaxRows,MaxCols) :: UNO3_2D, UNH4_2D, RNO3U_2D
      REAL UNO3(NL), UNH4(NL)

!     temp chp
      Real sumRLV

      SWV    = CELLS % State % SWV
      RLV_2D = CELLS % State % RLV
      SNO3_2D = CELLS % State % SNO3
      SNH4_2D = CELLS % State % SNH4

!     temp chp
      sumRLV = sum(rlv_2d)

!***********************************************************************
!***********************************************************************
!     Run Initialization - Called once per simulation
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
!-----------------------------------------------------------------------
!     ***** READ ROOT GROWTH PARAMETERS *****************
!-----------------------------------------------------------------------
!     Read in values from input file, which were previously input
!       in Subroutine IPCROP.
!-----------------------------------------------------------------------
      CALL GETLUN('FILEC', LUNCRP)
      OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,0)

!-----------------------------------------------------------------------
!    Find and Read Photosynthesis Section
!-----------------------------------------------------------------------
!     Subroutine FIND finds appropriate SECTION in a file by
!     searching for the specified 6-character string at beginning
!     of each line.
!-----------------------------------------------------------------------
      SECTION = '!*ROOT'
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILECC, LNUM)
      ELSE
        DO I = 1, 3
          CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
          IF (ISECT .EQ. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
        ENDDO
        READ(CHAR,'(2F6.0)',IOSTAT=ERR) RTNO3, RTNH4
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
      ENDIF

      CLOSE (LUNCRP)
      
      HalfRow = BedDimension % ROWSPC_cm / 2
      BEDWD   = BedDimension % BEDWD
      FurCol1 = BedDimension % FurCol1 
      ColFrac = BedDimension % ColFrac

!!     These should be done globally
!      DO j = 1, NColsTot
!        SELECT CASE(CELLS(FurRow1,j) % Struc % CellType)
!        CASE (3) !case of 
!          ColFrac(j) = Width(1,j) / HalfRow
!        CASE (4,5)
!        CASE DEFAULT; Cycle
!        END SELECT
!      ENDDO

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      TRNO3U = 0.0 
      TRNH4U = 0.0 
      TRNU   = 0.0 
      UNH4   = 0.0
      UNO3   = 0.0
      UNH4_2D = 0.0
      UNO3_2D = 0.0
      RLV_2D  = 0.0
      
      CELLS % RATE % NH4Uptake = UNH4_2D    !kg[N]/ha
      CELLS % RATE % NO3Uptake = UNO3_2D    !kg[N]/ha

!***********************************************************************
!***********************************************************************
!     DAILY RATE/INTEGRATION
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------
C   Initialize variables
C-----------------------------------------------------------------------
      TRNU   = 0.0
      TRNO3U = 0.0
      TRNH4U = 0.0
      NUF    = 0.0
      XMIN   = 0.0
      RNO3U_2D = 0.0
      RNH4U_2D = 0.0
      UNH4_2D  = 0.0
      UNO3_2D  = 0.0
      UNH4  = 0.0
      UNO3  = 0.0

      DO L = 1, NRowsTot
        DO J = 1, NColsTot
          !KG2PPM(L) = 10. / (BD(L) * DLAYR(L))
          NO3_2D(L, J) = SNO3_2D(L, J) * KG2PPM(L)
          NH4_2D(L, J) = SNH4_2D(L, J) * KG2PPM(L)
        ENDDO
      ENDDO
C-----------------------------------------------------------------------
C   Determine crop N demand (kg N/ha), after subtracting mobilized N
C-----------------------------------------------------------------------
      ANDEM = (NDMTOT - NDMSDR) * 10.0
      IF (ANDEM .GT. 1.E-9) THEN
C-----------------------------------------------------------------------
C   Calculate potential N uptake in soil layers with roots
C-----------------------------------------------------------------------
        DO L = 1, NRowsTot 
          DO J = 1, NColsTot  
            IF (RLV_2D(L, J) .GT. 1.E-6) THEN
              FNH4 = 1.0 - EXP(-0.08 * NH4_2D(L, J))
              FNO3 = 1.0 - EXP(-0.08 * NO3_2D(L, J))
              IF (FNO3 .LT. 0.04) FNO3 = 0.0  
              IF (FNO3 .GT. 1.0)  FNO3 = 1.0
              IF (FNH4 .LT. 0.04) FNH4 = 0.0  
              IF (FNH4 .GT. 1.0)  FNH4 = 1.0

!             SMDFR = relative drought factor
              SMDFR = (SWV(L, J) - LL(L)) / (DUL(L) - LL(L))
              IF (SMDFR .LT. 1.E-9) SMDFR = 0.0
              IF (SWV(L, J) .GT. DUL(L)) THEN
                SMDFR = 1.0 - (SWV(L, J) - DUL(L)) / (SAT(L) - DUL(L))
              ENDIF

!             RLV = Rootlength density (cm/cm3)
              RFAC = RLV_2D(L, J) * SMDFR * SMDFR * DLAYR(L)
!             cm[root]/cm2[soil] = cm[root]/cm3[soil] * cm[soil]

!             RTNO3 + RTNH4 = Nitrogen uptake / root length (mg N/cm)
!             RNO3U + RNH4  = Nitrogen uptake (kg N/ha)
              RNO3U_2D(L, J) = RFAC * FNO3 * RTNO3 * 100.0
              RNH4U_2D(L, J) = RFAC * FNH4 * RTNH4 * 100.0
!             kg[N]   cm[root]     mg[N]     100 kg/ha
!             ----- = --------- * -------- * ---------
!               ha    cm2[soil]   cm[root]     mg/cm2

              RNO3U_2D(L, J) = MAX(0.0,RNO3U_2D(L, J))
              RNH4U_2D(L, J) = MAX(0.0,RNH4U_2D(L, J))

!             kg[N]/ha
              TRNU = TRNU + (RNO3U_2D(L, J) + RNH4U_2D(L, J))*
     &              ColFrac(L, J)
            ENDIF
          ENDDO
        ENDDO
C-----------------------------------------------------------------------
C   Calculate N uptake in soil layers with roots based on demand (kg/ha)
C-----------------------------------------------------------------------
        IF (ANDEM .GT. TRNU) THEN
          ANDEM = TRNU
        ENDIF

        IF (TRNU .GT. 0.001) THEN
          NUF = ANDEM / TRNU
          DO L = 1, NRowsTot
            DO J = 1, NColsTot
              IF (RLV_2D(L, J) .GT. 0.0) THEN
                UNO3_2D(L, J) = RNO3U_2D(L, J) * NUF  !kg[N]/ha
                UNH4_2D(L, J) = RNH4U_2D(L, J) * NUF  !kg[N]/ha

!               XMIN = minimum amount NO3 left after uptake (kg[N]/ha)
                XMIN    = 0.25 / KG2PPM(L)
                MXNO3U  = MAX(0.0,(SNO3_2D(L, J) - XMIN))
                IF (UNO3_2D(L, J) .GT. MXNO3U) THEN
                  UNO3_2D(L, J) = MXNO3U
                ENDIF

!               XMIN = minimum amount NH4 left after uptake (kg[N]/ha)
                XMIN = 0.5 / KG2PPM(L)
                MXNH4U  = MAX(0.0,(SNH4_2D(L, J) - XMIN))
                IF (UNH4_2D(L, J) .GT. MXNH4U) UNH4_2D(L, J) = MXNH4U
                TRNO3U  = TRNO3U + UNO3_2D(L, J) * ColFrac(L, J)
                TRNH4U  = TRNH4U + UNH4_2D(L, J) * ColFrac(L, J)
                UNO3(L) = UNO3(L) + UNO3_2D(L, J) * ColFrac(L, J)
                UNH4(L) = UNH4(L) + UNH4_2D(L, J) * ColFrac(L, J)
              ENDIF
            ENDDO
          ENDDO

!         Convert uptake to g/m^2 for plant routines
          TRNO3U = TRNO3U / 10.0
          TRNH4U = TRNH4U / 10.0
          TRNU   = TRNO3U + TRNH4U
        ENDIF
      ENDIF

      CELLS % RATE % NH4Uptake = UNH4_2D    !kg[N]/ha
      CELLS % RATE % NO3Uptake = UNO3_2D    !kg[N]/ha
!      CAll Interpolate2Layers_2D(UNO3_2D, Cells%Struc, NLAYR,  !input
!     &         UNO3)                                           !Output
!      CAll Interpolate2Layers_2D(UNH4_2D, Cells%Struc, NLAYR,  !input
!     &         UNH4)                                           !Output

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE NUPTAK_2D
C=======================================================================

!-----------------------------------------------------------------------
!       Variable definitions
!-----------------------------------------------------------------------
! ANDEM       Total crop N demand (kg[N]/ha)
! CHAR        Contains the contents of last record read 
! DLAYR(L)    Soil thickness in layer L (cm)
! DUL(L)      Volumetric soil water content at Drained Upper Limit in soil 
!               layer L (cm3 [H2O] /cm3 [soil])
! ERR         Error code for file operation 
! ERRKEY      Subroutine name for error file 
! FILECC      Path plus filename for species file (*.spe) 
! FNH4        Potential NH4 availability factor 
! FNO3        Potential NO3 availability factor 
! KG2PPM(L)   Conversion factor to switch from kg [N] / ha to ug [N] / g 
!               [soil] for soil layer L 
! LL(L)       Volumetric soil water content in soil layer L at lower limit
!               ( cm3/cm3)
! LUNCRP      Logical unit number for FILEC (*.spe file) 
! MXNH4U      Maximum NH4 uptake from soil (kg N/ha)
! MXNO3U      Maximum NO3 uptake from soil (kg N/ha)
! NDMSDR      Amount of Mobilized N which can be used for seed growth
!               (g[N] / m2 / d)
! NDMTOT      Total N demand (g[N] / m2 / d)
! NH4_2D(L,J) Ammonium N in soil cell (µg[N] / g[soil])
! NL          Maximum number of soil layers = 20 
! NLAYR       Number of soil layers 
! NO3_2D(L,J) Nitrate in soil cell (µg[N] / g[soil])
! NUF         N uptake fraction (ratio of demand to N uptake), <= 1.0 
! RFAC        Nitrogen uptake conversion factor ((kg N/ha) / (mg N / cm root))
! RLV_2D(L,J) Root length density for soil cell ((cm root / cm3 soil))
! RNH4U_2D(L,J) Ammonium uptake (kg N/ha)
! RNO3U_2D(L,J) Nitrate uptake (kg N/ha)
! RTNH4       Ammonium uptake per unit root length (mg N / cm)
! RTNO3       Nitrate uptake per unit root length (mg N / cm)
! SAT(L)      Volumetric soil water content in layer L at saturation
!               (cm3 [water] / cm3 [soil])
! SMDFR    Relative drought factor 
! SNH4(L)  Total extractable ammonium N in soil layer L (kg [N] / ha)
! SNO3(L)  Total extractable nitrate N in soil layer L (kg [N] / ha)
! SW(L)    Volumetric soil water content in layer L
!            (cm3 [water] / cm3 [soil])
! TRNH4U   Total N uptake in ammonium form in a day (g[N] / m2 / d)
! TRNO3U   Total N uptake in nitrate form in a day (g[N] / m2 / d)
! TRNU     Total N uptake in a day (kg[N] / ha / d)
! UNH4     Uptake of NH4 from soil (interim value) (kg N/ha)
! UNO3     Uptake of NO3 from soil (interim value) (kg N/ha)
! UNH4_2D(L, J)  Uptake of NH4 from cell
! UNO3_2D(L, J)  Uptake of NO3 from cell
! XMIN     Amount of NH4 that cannot be immobilized but stays behind in 
!            soil as NH4; Also, Amount of NO3 that cannot denitrify but 
!            stays behind in the soil as NO3 (kg [N] / ha)
!-----------------------------------------------------------------------
!       END SUBROUTINE NUPTAK_2D
!=======================================================================
