!=======================================================================
!  ROOTY_2D, Subroutine, 
!     C.H.Porter modified for simple 2D algorithm
!-----------------------------------------------------------------------
!
!  Calculates root growth, extension, respiration, and senescence

!  Horizontal dimension is measured from center of row (planting centerline)
!     to mid point between rows, such that only half of each row is
!     modelled.  All growth and soil processes are assumed to be 
!     symmetrical around planting centerline.

!-----------------------------------------------------------------------
!  REVISION HISTORY
!  01/09/1989 GH  Written. (ROOTS.for)
!  09/29/1995 KJB Changed to lessen effect of water deficit on root depth
!                 increase.
!  01/19/1996 JWJ Added effects of excess water.
!  01/20/1996 KJB Increase root extension rate under drought stress
!  09/13/1998 CHP Modified for modular format
!  09/14/1998 CHP Changed TROOT to TRLV to match same variable in ROOTDM
!                 Changed SWDF1 to SWFAC to match variable name in WATBAL
!  05/11/1999 GH  Incorporated in CROPGRO
!  02/21/2005 SJR Moved ISWWAT condition here to allow computation of 
!                 root senescence even when water not simulated.
!  10/04/2005 SJR Include senescence due to water stress in total 
!                 daily senescence.
!  10/20/2005 CHP Added optional minimum root mass for senescence, 
!                 RTWTMIN, to species file
!  01/19/2006 CHP Fixed discrepancies between plant root senescence  
!                 calculated and that sent to soil routines for addition
!                 to organic matter. 
!  02/12/2009 CHP Modified for 2D water balance routine. 
!-----------------------------------------------------------------------
!  Called by  :  PLANT
!  Calls      :  IPROOT, INROOT
!=======================================================================

      SUBROUTINE ROOTY_2D(DYNAMIC,
     &    AGRRT, CELLS, CROP, DTX, FILECC, FRRT,          !Input
     &    ISWWAT, PLTPOP, RTWT, SOILPROP,                 !Input
     &    SWFAC, VSTAGE, WRDOTN, WTNEW,                   !Input
     &    RLV, RLV_2D, RTDEP, RTWID, SATFAC,              !Output
     &    SENRT, SRDOT)                                   !Output
!-----------------------------------------------------------------------
      USE Cells_2D
      USE ModuleData
      IMPLICIT NONE
      EXTERNAL IPROOT_2D, INROOT_2D, TABEX, OPROOTS_2D, AGGREGATE_ROOTS
      SAVE

      CHARACTER*1 ISWWAT
      CHARACTER*2 CROP
      CHARACTER*92 FILECC

      INTEGER DYNAMIC, NLAYR

      REAL CUMDEP, DTX, FRRT,
     &  RFAC1, RFAC2, RFAC3,
     &  RLDSM, RLNEW,  
     &  RTSDF, RTSEN, RTWT, SRDOT, SWDF, SWFAC,
     &  TRLDF, TRLV, WRDOTN   
      REAL FACTOR, HalfRow, HalfBed
      REAL CGRRT, AGRRT
      REAL SWEXF, PORMIN, RTEXF, RTSURV
      REAL SUMEX, SUMRL, SATFAC
      REAL PLTPOP, WTNEW, VSTAGE
      REAL TABEX              !Function subroutine located in UTILS.FOR

      REAL TRLV_MIN, RLSENTOT, RTWTMIN
      REAL CumWid, LastCumDep, LastCumWid

!     2D variables:
      INTEGER Row, Col, FirstRow, LastRow, LastCol 
      REAL RTDEPnew, RowSpc_cm, RTLenNew
      REAL RTWIDr(MaxRows), RTWIDnew(MaxRows), WidMax(MaxRows)
      REAL RTDEP, RTDEPI, DEPMAX, RTWID, RTWIDI
      REAL TotRootMass, CumRootMass, rlv_max
      REAL WidFrac(MaxRows,MaxCols), DepFrac(MaxRows,MaxCols) 
      REAL XRTFAC(4), YRTFAC(4)
      REAL XRTFACH(4), YRTFACH(4)
      REAL RFAC2H, CellArea(MaxRows,MaxCols)
      TYPE (CellType) CELLS(MaxRows,MaxCols)
      TYPE (CellStrucType) Struc(MaxRows,MaxCols)
      REAL, DIMENSION(MaxRows,MaxCols) :: RtLen_2D, RootLength, 
     &    RLNew_2D, RLVnew_2D

!     Local soil arrays
      INTEGER, DIMENSION(MaxRows,MaxCols) :: TypeCell
      REAL, DIMENSION(MaxRows,MaxCols) :: DUL, ESW, LL, RLDF
      REAL, DIMENSION(MaxRows,MaxCols) :: RLSEN, RLV_2D, RLV_WS, RRLF
      REAL, DIMENSION(MaxRows,MaxCols) :: Thick, Width, SAT, SENRT_2D
      REAL, DIMENSION(MaxRows,MaxCols) :: SWV, CelRootArea
      REAL, DIMENSION(NL) :: RLV, SENRT
      REAL TotRootArea

      TYPE (SoilType) SOILPROP
      NLAYR = SOILPROP % NLAYR

!***********************************************************************
!***********************************************************************
!     Run Initialization - Called once per simulation
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
!-----------------------------------------------------------------------
      CALL IPROOT_2D(FILECC,                              !Input
     &  PORMIN, RFAC1, RLDSM, RTDEPI, RTEXF, RTSEN, RTSDF,!Output
     &  RTWIDI, RTWTMIN, XRTFAC, YRTFAC, XRTFACH, YRTFACH)!Output

      SRDOT    = 0.0
      RLV      = 0.0
      RLV_2D   = 0.0
      RTWIDr   = 0.0
      SENRT_2D = 0.0
      SUMEX    = 0.0
      SUMRL    = 0.0
      SATFAC   = 0.0
      RTDEP    = 0.0
      RTWID    = 0.0
      TRLV     = 0.0

      RFAC3 = RFAC1
      RLNEW = 0.0
      ROWSPC_cm = BedDimension % ROWSPC_cm

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      STRUC = CELLS%STRUC
      Thick = STRUC%THICK
      Width = STRUC%WIDTH
      CellArea = STRUC%CellArea
      TypeCell = STRUC%Cell_Type
      DUL = CELLS%STATE%DUL
      LL  = CELLS%STATE%LL
      SAT = CELLS%STATE%SAT

!     Calculate maximum depth in each column and width in each row
      FirstRow = 0
      WidMax = 0.0
      DepMax = 0.0
      DO Row = 1, NRowsTot
        SELECT CASE(TypeCell(Row,1))
        CASE(0,1,2)  !Ignore surface water or litter cells and furrow
!         Go on to next row
          CYCLE
        CASE(3,4,5)
!         Maximum depth is the same for all columns, calc for column 1
          DepMax = DepMax + Thick(Row,1)
          IF (FirstRow == 0) FirstRow = Row
        CASE (:-1,6:)  !less than zero, or greater than 5
          EXIT
        END SELECT

        DO Col = 1, NColsTot
          SELECT CASE(TypeCell(Row,Col))
          CASE (3,4,5)
!           Maximum width can vary with depth for bedded systems
            WidMax(Row) = WidMax(Row) + Width(Row,Col)
          END SELECT
        ENDDO
      ENDDO
      
!-----------------------------------------------------------------------
!     ROOT DEPTH INCREASE RATE WITH TIME, cm/physiological day
!-----------------------------------------------------------------------
      IF (CROP .NE. 'FA') THEN
        RFAC2 = TABEX(YRTFAC,XRTFAC,0.0,4)
        RFAC2H = TABEX(YRTFACH, XRTFACH, 0.0, 4)
      ENDIF
      
      SRDOT    = 0.0
      RLV      = 0.0
      RLV_2D   = 0.0
      RTWIDr   = 0.0
      SENRT_2D = 0.0
      SUMEX    = 0.0
      SUMRL    = 0.0
      SATFAC   = 0.0
      RTDEP    = 0.0
      RTWID    = 0.0
      TRLV     = 0.0
      RFAC3 = RFAC1
      
      CumRootMass = 0.0

!     Width of half row (cm) used to scale up to field area basis. 
      HalfRow = BedDimension % ROWSPC_cm / 2.
      HalfBed = BedDimension % BEDWD / 2.

      rlv_max = 0.0
      CALL OPRoots_2D(TotRootMass, RFAC3, RLV_2D, Thick, Width)

!***********************************************************************
!***********************************************************************
!     EMERGENCE CALCULATIONS - Performed once per season upon emergence
!         or transplanting of plants
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. EMERG) THEN
!-----------------------------------------------------------------------
!       Call INROOT for initialization of root variables on
!       day of emergence.  (GROW emergence initialization
!       must preceed call to INROOT.)
!-----------------------------------------------------------------------
      CALL INROOT_2D(CELLS,
     &  DepMax, FirstRow, FRRT, PLTPOP, RFAC1,             !Input
     &  ROWSPC_CM,                                         !Input
     &  RTDEPI, RTWIDI, Thick, WidMax, Width, WTNEW,       !Input
     &  RLV_2D, RTDEP, RTWID, RTWIDr)                      !Output

      RFAC3 = RFAC1

      CumRootMass = WTNEW * FRRT * PLTPOP * 10. 
!        kg[root]  g[tissue] g[root]    plants   kg/ha
!        -------- = ----- * --------- * ------ * ----- 
!           ha      plant   g[tissue]     m2      g/m2

      CALL Aggregate_Roots(CELLS,
     &    FirstRow, RFAC3, RLV_2D, SOILPROP,  !Input
     &    RLV, RootLength, RtLen_2D,          !Output
     &    TotRootMass, TRLV)                  !Output

      LastRow = 1
      LastCol = 1

!***********************************************************************
!***********************************************************************
!     DAILY RATE/INTEGRATION
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------
!     Calculate Root Depth Rate of Increase, Physiological Day (RFAC2)
!-----------------------------------------------------------------------
      RFAC2 = TABEX(YRTFAC, XRTFAC, VSTAGE, 4)
      RFAC2H = TABEX(YRTFACH, XRTFACH, VSTAGE, 4)

!     RLNEW calculated below is the same as in the 1D model. 
!     It represents new root growth today over the entire field.
      RLNEW = WRDOTN * RFAC1 * 1.E-4 
!     cm[root]        g[root]   cm[root]   m2
!  ---------------- = ------- * -------- * ---
!  cm2[land area]-d     m2-d     g[root]   cm2

!     For the 2D model, we need to scale it down to half the field 
!       being modeled
      RTLenNew = RLNEW * HalfRow
!        cm[root]        cm[root]
!     ------------- = -------------- * cm[row-width]
!     cm[rowlength]   cm2[land area]

      CGRRT = AGRRT * WRDOTN
      SWV = CELLS%STATE%SWV

!-----------------------------------------------------------------------
!     Calculate root length per cm row-length and initiate growth by cell
!-----------------------------------------------------------------------
      RRLF   = 0.0
      RLSEN  = 0.0
      RLVnew_2D  = 0.0

!     Update RFAC3 based on yesterday's RTWT and TRLV
      IF (RTWT .GE. 0.0001 .AND. TRLV .GE. 0.00001) THEN
!       RTWT has not yet been updated today, so use yesterday's
!       value and don't subtract out today's growth - chp 11/13/00
        RFAC3 = TRLV / RTWT * 10000.0
!       cm[root]      cm[root]        m2      cm2
!       -------- = -------------- * ------- * ---
!        g[root]   cm2[land area]   g[root]    m2
      ELSE
        RFAC3 = RFAC1
      ENDIF

!     10/20/2005 Limit RLV decrease due to senscence to 
!       a minimum resulting root weight 
      IF (RTWTMIN > 0.0) THEN
!       Same units as TRLV (cm[root]/cm2[land area])
        TRLV_MIN = RTWTMIN * RFAC3 * 1.E-4   
!       cm[root]   g[root]   cm[root]   m2
!       -------- = ------- * -------- * ---
!          cm2       m2      g[root]    cm2
      ELSE
!       Set TRLV_MIN to zero -- no minimum root mass
        TRLV_MIN = 0.0
      ENDIF
      
!-----------------------------------------------------------------------
      RLDF   = 0.0
      TRLDF  = 0.0
      CUMDEP = 0.0
      SUMEX  = 0.0
      SUMRL  = 0.0
      RLV_WS = 0.0
      RLSEN  = 0.0
      RTDEPnew = RTDEP
      RTWIDnew = RTWIDr

!     First, root expansion.
!     Root depth is calculated in column 1 only.
!     Root width is calculated for each row. 
      RowLoop: DO Row = FirstRow, NRowsTot
        IF (TypeCell(Row,1) < 3 .OR. TypeCell(Row,1) > 5) CYCLE
        LastCumdep = CUMDEP
        CUMDEP = CUMDEP + Thick(Row,1)
        CumWid = 0.0
        ColLoop: Do Col = 1, NColsTot
          LastCumWid = CumWid
          CumWid = CumWid + Width(Row,Col)

          SWDF = 1.0
          SWEXF = 1.0
          IF (ISWWAT .EQ. 'Y') THEN
            IF (SAT(Row,Col) - SWV(Row,Col) .LT. PORMIN) THEN
              SWEXF = (SAT(Row,Col) - SWV(Row,Col)) / PORMIN
              SWEXF = MIN(SWEXF, 1.0)
            ENDIF
!           SUMRL and SUMEX are weighted by cell area (instead of DLAYR)
            SUMRL = SUMRL + RLV_2D(Row,Col) * CellArea(Row,Col)
            SUMEX = SUMEX + RLV_2D(Row,Col) * CellArea(Row,Col) * 
     &              (1.0 - SWEXF)
            ESW(Row,Col) = DUL(Row,Col) - LL(Row,Col)
            IF (SWV(Row,Col) - LL(Row,Col) .LT. 0.25*ESW(Row,Col)) THEN
              SWDF = (SWV(Row,Col) - LL(Row,Col)) / (0.25*ESW(Row,Col))
              SWDF = MAX(SWDF, 0.0)
            ENDIF
          ENDIF

!-----------------------------------------------------------------------
!         Water stress senescence 
!         TRLV and RLNEW are scaled for entire field (same as 1D model)
          RTSURV = MIN(1.0,(1.-RTSDF*(1.-SWDF)),(1.-RTEXF*(1.-SWEXF)))
          IF (RLV_2D(Row,Col) > RLDSM .AND. TRLV + RLNEW > TRLV_MIN)THEN
            RLV_WS(Row,Col) = RLV_2D(Row,Col) * (1.0 - RTSURV)
          ELSE
            RLV_WS(Row,Col) = 0.0
          ENDIF

!-----------------------------------------------------------------------
!         Weighting factor for each cell, RLDF, based on WR, cell area
!           and water factors.  
!         WR gets replaced with Z function from Fernando's model. Modified 
!         daily due to soil impedance, soil deficiencies, other stuff.
          RLDF(Row,Col) = CELLS(Row,Col)%STATE%WR * CellArea(Row,Col) * 
     &                     MIN(SWDF,SWEXF)

!-----------------------------------------------------------------------
!         Calculate new vertical growth in column 1 only
          IF (COL == 1) THEN
!           WidFrac(Row,Col) = 1.0 
            IF (RTDEP >= CUMDEP) THEN
              DepFrac(Row,Col) = 1.0
            ELSEIF (RTDEP >= LastCumDep)THEN
!             Roots have partially filled the depth of this cell
              IF (CELLS(Row,Col)%STATE%WR > 0.0 .AND. RLNEW > 0.0) THEN
                IF (Row == 1) THEN
                  RTDEPnew = RTDEP + DTX * RFAC2
                ELSE
                  RTDEPnew = RTDEP + DTX * RFAC2 * 
     &            MIN(SWDF,SWEXF) * (1. + 0.25 * (1. - MAX(SWFAC,0.40)))
                ENDIF
                RTDEPnew = MIN(RTDEPnew, DEPMAX)
              ENDIF
              DepFrac(Row,Col) = MIN(1.0, 1. - (CUMDEP - RTDEPnew) / 
     &                         Thick(Row,Col))
              IF (Row > LastRow) THEN
!               New roots have just grown into this cell
                LastRow = Row
!CHP                RTWIDnew(Row) = Width(Row,Col)
                DepFrac(Row,Col) = MIN(1.0, 1. - (CUMDEP - RTDEPnew) / 
     &                         Thick(Row,Col))
              ENDIF
            ELSE
!             No roots in this cell
              DepFrac(Row,Col) = 0.0
              EXIT RowLoop
            ENDIF
          ENDIF
!-----------------------------------------------------------------------
!CHP          ELSE
!           Calculate new horizontal growth in this cell (RTWIDnew) and 
!             horizontal portion of cell occupied by roots (WidFrac)
!           Horizontal root growth only occurs when DepFrac of adjacent 
!             cell is > 0.99.  No need to calculate for Column 1, since
!             width fraction is initialized to 1.0 there.
            IF (RTWIDr(Row) >= CumWid) THEN
              WidFrac(Row,Col) = 1.0
              IF (col > 1) THEN
                DepFrac(Row,Col) = min(1.0, DepFrac(Row, col-1)) 
              ENDIF
            ELSEIF (RTWIDr(Row) >= LastCumWid) THEN
!             Roots have partially filled the width of this cell
              IF (CELLS(Row,Col)%STATE%WR > 0.0 .AND. RLNEW > 0.0) THEN
                IF (Row == 1) THEN
                  RTWIDnew(Row) = RTWIDr(Row) + DTX * RFAC2H
                ELSE
                  RTWIDnew(Row) = RTWIDr(Row) + DTX * RFAC2H 
     &                * MIN(SWDF,SWEXF) 
     &                * (1. + 0.25 * (1. - MAX(SWFAC,0.40)))
                ENDIF
                RTWIDnew(Row) = MIN(RTWIDnew(Row), WIDMAX(Row))
!              Else
!!               JZW need to check if it is correct here
!                DepFrac(Row,Col) = 0.0 
              ENDIF
              WidFrac(Row,Col) = MIN(1.0, 1. - (CumWid - RTWIDnew(Row))
     &                        / Width(Row,Col))
              IF (Col > LastCol) LastCol = Col
              DepFrac(Row,Col) = 1.0
            ELSE
!             No roots in this cell
              WidFrac(Row,Col) = 0.0
              DepFrac(Row,Col) = 0.0
            ENDIF

!           Check for new roots in this cell
            IF (RTWIDnew(Row) > LastCumWid .AND. 
     &          RTWIDr(Row) <= LastCumWid) THEN
!             New roots have just grown into this cell
              WidFrac(Row,Col) = MIN(1.0, 1. - (CumWid - RTWIDnew(Row)) 
     &                        / Width(Row,Col))
!             JZW change May 9,2012 
              IF (col > 1) THEN
                DepFrac(Row,Col) = min(1.0, DepFrac(Row, col-1)) 
              ENDIF
              IF (Col > LastCol) LastCol = Col
            ENDIF
!CHP          ENDIF

!-----------------------------------------------------------------------
!         Apply factor for this cell
          RLDF(Row,Col) =RLDF(Row,Col)*DepFrac(Row,Col)*WidFrac(Row,Col)
          CelRootArea(Row,Col) = CellArea(Row,Col)
     &               * DepFrac(Row,Col) * WidFrac(Row,Col)
!         Sum of all factors
          TRLDF = TRLDF + RLDF(Row,Col)
          TotRootArea = TotRootArea +  CelRootArea(Row,Col)
          IF (RTWIDnew(Row) < CumWid) EXIT ColLoop
          
        ENDDO ColLoop
      ENDDO RowLoop

      RTDEP  = RTDEPnew
      RTWIDr = RTWIDnew

!-----------------------------------------------------------------------
!     Calculate root senescence, update root length density for each cell.
!-----------------------------------------------------------------------
      IF (SUMRL .GT. 0.0) THEN
         SATFAC = SUMEX/SUMRL
      ELSE
         SATFAC = 0.0
      ENDIF

      SRDOT = 0.0
      RLSENTOT = 0.0

      DO Row = FirstRow, LastRow
        DO Col = 1, LastCol
          IF (TRLDF .LT. 0.00001) THEN
            RRLF(Row,Col) = 0.0
          ELSE
            RRLF(Row,Col) = RLDF(Row,Col) / TRLDF
          ENDIF
!-------------------------------------------------------------------------
          RLNew_2D(row,col) = RTLenNew * RRLF(Row,Col)
!             cm[root]         cm[root]
!         ---------------- = ----------------
!         cm[row length]-d   cm[row length]-d

          RLVnew_2D(row,col) = RLNew_2D(row,col) / CellArea(row,col)
!         cm[root]       cm[root]        1
!         -------- = ---------------- * ---
!           cm3-d    cm[row length]-d   cm2

          IF (TRLV + RLNEW > TRLV_MIN) THEN
            RLSEN(Row,Col) = RLV_2D(Row,Col) * RTSEN * DTX
          ELSE
            RLSEN(Row,Col) = 0.0
          ENDIF

!         Limit total senescence in each layer to existing RLV
          IF (RLSEN(Row,Col) + RLV_WS(Row,Col) > RLV_2D(Row,Col) + 
     &        RLVnew_2D(Row,Col)) THEN
            RLSEN(Row,Col) = RLV_2D(Row,Col) + RLVnew_2D(Row,Col) - 
     &        RLV_WS(Row,Col)
          ENDIF 

!         RLSENTOT is profile senescence, water stress and natural cm/cm2[land area]
          RLSENTOT = RLSENTOT  + (RLSEN(Row,Col) + RLV_WS(Row,Col)) * 
     &                         CellArea(Row,Col)
!         cm[root]       cm[root]
!       ------------- = ----------- * cm2
!       cm[rowlength]   cm3[ground]
        ENDDO
      ENDDO

!     Convert RLSENTOT into units of TRLV
      RLSENTOT = RLSENTOT / HalfRow
!        cm[root]        cm[root]           1
!     -------------- = ------------- * -------------
!     cm2[land area]   cm[rowlength]   cm[row width]

!     If senescence too high (results in TRLV < TRLV_MIN) then
!       reduce senescence in each layer by factor.
      IF (RLSENTOT > 1.E-6 .AND. TRLV + RLNEW - RLSENTOT < TRLV_MIN)THEN
      !    cm/cm2               cm/cm2  cm/cm2   cm/cm2     cm/cm2
        FACTOR = (TRLV + RLNEW - TRLV_MIN) / RLSENTOT
        FACTOR = MAX(0.0, MIN(1.0, FACTOR))
        RLSEN  = RLSEN  * FACTOR
        RLV_WS = RLV_WS * FACTOR
        RLSENTOT = RLSENTOT * FACTOR
      ENDIF
     
!     Update RLV and TRLV based on today's growth and senescence
      TRLV = 0.0
      DO Row = FirstRow, LastRow
        DO Col = 1, LastCol
          RLV_2D(Row,Col) = RLV_2D(Row,Col) + RLNew_2D(Row,Col) 
!           cm/cm3        =     cm/cm3      +   cm/cm3
     &                    - RLSEN(Row,Col) - RLV_WS(Row,Col)
!                         -     cm/cm3      -   cm/cm3

!         Track senescence in each cell for adding C and N to soil
          SENRT_2D(Row,Col) = (RLSEN(Row,Col) + RLV_WS(Row,Col))
     &                      * CellArea(Row,Col) / HalfRow / RFAC3 * 1.E5
!                     cm[root]   cm2[widxdep]   g[root]   1E4 cm2    10(kg/ha)
!           kg/ha  =  -------- * ------------ * ------- * -------- * ---------
!                    cm3[soil]    cm[wid]       cm[root]     m2       (g/m2)

          SENRT_2D(Row,Col) = AMAX1(SENRT_2D(Row,Col), 0.0)
          SRDOT = SRDOT + SENRT_2D(Row,Col)/10.        !g/m2

          IF (RTWIDr(Row) > RTWID) RTWID = RTWIDr(Row)

          if (rlv_2d(Row,Col) > rlv_max) then
            rlv_max = rlv_2d(Row,Col)
          endif
        ENDDO
      ENDDO

!     Total root senescence = water stress + natural senescence
      SRDOT = AMAX1(SRDOT, 0.0)

      CumRootMass = CumRootMass + WRDOTN * 10. - SRDOT * 10.

      CALL Aggregate_Roots(CELLS,
     &    FirstRow, RFAC3, RLV_2D, SOILPROP,  !Input
     &    RLV, RootLength, RtLen_2D,          !Output
     &    TotRootMass, TRLV)                  !Output

      CALL Cell2Layer_2D(SENRT_2D, Struc, NRowsTot, SENRT)
      CELLS%STATE%RLV = RLV_2D

!***********************************************************************
!***********************************************************************
!     OUTPUT and SEASEND
!***********************************************************************
      ELSEIF (DYNAMIC == OUTPUT .OR. DYNAMIC == SEASEND) THEN
!-----------------------------------------------------------------------
      CALL OPRoots_2D(TotRootMass, RFAC3, RLV_2D, Thick, Width)

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************

      RETURN
      END SUBROUTINE ROOTY_2D
!=======================================================================


!=======================================================================
!  IPROOT Subroutine
!  Reads root parameters from input files.
!----------------------------------------------------------------------
!  REVISION HISTORY
!  09/13/1998 CHP Written
!  08/12/2003 CHP Added I/O error checking
!-----------------------------------------------------------------------
!  Called : ROOTS
!  Calls  : FIND, ERROR, IGNORE
!=======================================================================
      SUBROUTINE IPROOT_2D(FILECC,                        !Input
     &  PORMIN, RFAC1, RLDSM, RTDEPI, RTEXF, RTSEN, RTSDF,!Output
     &  RTWIDI, RTWTMIN, XRTFAC, YRTFAC, XRTFACH, YRTFACH)!Output

!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
!     NL defined in ModuleDefs.for

      IMPLICIT NONE
      EXTERNAL GETLUN, ERROR, FIND, IGNORE, WARNING
      SAVE

      CHARACTER*6 ERRKEY
      PARAMETER (ERRKEY = '2DROOT')

      CHARACTER*6 SECTION
      CHARACTER (len=7) RWMTXT
      CHARACTER*78 MSG(2)
      CHARACTER*80 CHAR
      CHARACTER*92 FILECC

      INTEGER LUNCRP, ERR, LNUM, ISECT, FOUND, II

      REAL RTDEPI, RTWIDI, RLDSM, PORMIN
      REAL RFAC1, RTSEN, RTSDF, RTEXF
      REAL XRTFAC(4), YRTFAC(4), XRTFACH(4), YRTFACH(4)
      REAL RTWTMIN

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
!    Find and Read root section
!-----------------------------------------------------------------------
      SECTION = '!*ROOT'
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        READ(CHAR,'(5F6.0)',IOSTAT=ERR) RTDEPI,RFAC1,RTSEN,RLDSM,RTSDF
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        READ(CHAR,'(8F6.0)',IOSTAT=ERR)(XRTFAC(II),YRTFAC(II),II = 1,4)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        READ(CHAR,'(12X,2F6.0)',IOSTAT=ERR) PORMIN, RTEXF
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
        
!       2D (horizontal) root growth parameters 
!       if not found, set equal to vertical parameters with message      
        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        READ(CHAR,'(F6.0,38X,A7)',IOSTAT=ERR) RTWIDI
        IF (ERR /= 0 .OR. RTWIDI < 0 .OR. INDEX(CHAR,'RTWIDI') < 1) THEN
          RTWIDI = RTDEPI
          XRTFACH = XRTFAC
          YRTFACH = YRTFAC
          MSG(1) = "No horizontal root growth parameters specified " // 
     &      "for 2D model."
          MSG(2) = "Vertical parameters will be applied to " // 
     &      "horizontal root growth."
          CALL WARNING(2,ERRKEY,MSG)
          BACKSPACE (LUNCRP)
        ELSE
          CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
          READ(CHAR,'(8F6.0)',IOSTAT=ERR)
     &      (XRTFACH(II),YRTFACH(II),II = 1,4)
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
        ENDIF

!       Optional minimum root mass for senescence (g/m2)
        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
!       Cheryl let use ignore RWMTXT, *spe file do not have this variable 
        READ(CHAR,'(F6.0,T45,A7)',IOSTAT=ERR) RTWTMIN, RWMTXT 
        IF (ERR /= 0 .OR. RWMTXT /= 'RTWTMIN' .OR. RTWTMIN < 0.) THEN
          RTWTMIN = 0.0
        ENDIF
      ENDIF

      CLOSE (LUNCRP)

!***********************************************************************
      RETURN
      END SUBROUTINE IPROOT_2D
!=======================================================================

!=======================================================================
!  INROOT Subroutine
!  Initializes root variables at emergence.
!----------------------------------------------------------------------
!  REVISION HISTORY
!  04/01/1991 GH  Adapted for CROPGRO
!  06/17/1998 CHP Modified for modular format
!  05/11/1999 GH  Incorporated in CROPGRO
!  02/21/2009 CHP Adapted for 2D roots
!-----------------------------------------------------------------------
!  Called : CROPGRO
!  Calls  : None
!=======================================================================
      SUBROUTINE INROOT_2D(CELLS,
     &  DepMax, FirstRow, FRRT, PLTPOP, RFAC1,    !Input
     &  ROWSPC_CM,                                         !Input
     &  RTDEPI, RTWIDI, Thick, WidMax, Width, WTNEW,       !Input
     &  RLV_2D, RTDEP, RTWID, RTWIDr)                      !Output

!     ------------------------------------------------------------------
      USE Cells_2D
      IMPLICIT NONE

      INTEGER Row, Col, FirstRow
      REAL FRRT, DepMax, PLTPOP, RFAC1, RLINIT, WidMax(MaxRows), WTNEW
      REAL X, Z
      REAL RTDEPI, RTDEP, LastCumDep, CumDep
      REAL RTWIDI, RTWID, LastCumWid, CumWid, RTWIDr(MaxRows)
      REAL TotRootArea, RLV_AVG, ROWSPC_CM
      REAL, DIMENSION(MaxRows,MaxCols) :: Thick, Width, CellArea
      REAL, DIMENSION(MaxRows,MaxCols) :: RLV_2D, RootArea
      INTEGER, DIMENSION(MaxRows,MaxCols) :: Cell_type
      TYPE (CellType), INTENT(IN) :: CELLS(MaxRows,MaxCols)
      Cell_Type = CELLS%STRUC%Cell_Type
!-----------------------------------------------------------------------
      RTDEPI = MAX(MIN(RTDEPI, DepMax), Thick(1,1))
      RTWIDI = MAX(MIN(RTWIDI, WidMax(1)), Width(1,1))
      RTDEP = RTDEPI
      RTWID = RTWIDI

!     Total root length
      RLINIT = WTNEW * FRRT * PLTPOP * RFAC1 * 1.E-4
!     cm[root]     g[root]   plants  cm[root]   m2
!    ---------- = ------- * ------ * ------- * ---   
!     cm2[soil]    plant      m2     g[root]   cm2

!     Overall RLV is a concentration. The value should be equal
!         in all cells at initialization.
!     RLV is higher in 2D cells than in 1D layers because the roots are not
!       found in every cell so the cells with roots are more densly populated.
!       (Rowspc / RTWIDi) concentrates the RLV in the cells that have roots.
      RLV_AVG = RLINIT / RTDEPI * (ROWSPC_cm / RTWIDi)
!    cm[root]    cm[root]       1       cm[soil]
!    --------- = --------- * -------- * --------
!    cm3[soil]   cm2[soil]   cm[soil]   cm[soil]

!     Initialize variables for root distribution in cells
      RTWIDr = 0.0  !total width of roots in a half row (array)
      X = 0.0  !root width within a cell
      Z = 0.0  !root depth within a cell
      RootArea = 0.  !cell area containing roots
      TotRootArea = 0.0  !total root area in half row

!     Distribute root length and width evenly thru cells with roots
      CUMDEP = 0.
      RowLoop: DO Row = FirstRow, NRowsTot
        LastCumDep = CUMDEP
        CUMDEP = CUMDEP + Thick(Row,1)
        IF (RTDEPI >= CUMDEP) THEN
          Z = Thick(Row,1)
        ELSEIF (RTDEPI > LastCumDep) THEN
          Z = RTDEPI - LastCumDep 
        ELSE
          Z = 0.0
          EXIT RowLoop
        ENDIF

        CumWid = 0.
        ColLoop: DO Col = 1,NColsTot
          SELECT CASE(Cell_type(row,col))
          CASE(3,4,5)
            LastCumWid = CumWid
            CumWid = CumWid + Width(Row,Col)
            CellArea(Row,Col) = Width(Row,Col) * Thick(Row,Col)
            IF (RTWIDI / 2.0 >= CumWid) THEN
              X = Width(Row,Col)
            ELSEIF (RTWIDI / 2.0 > LastCumWid) THEN
              X = RTWIDI / 2.0 - LastCumWid
            ELSE
              X = 0.0
              EXIT ColLoop
            ENDIF
            RtWidr(row) = RtWidr(row) + X
            
            RootArea(Row,Col) = X * Z
            TotRootArea = TotRootArea + RootArea(Row,Col)
          END SELECT
        ENDDO ColLoop
      ENDDO RowLoop

!     RLV is homogeneous in the intial root zone. Some cells are not fully
!       filled with roots, so cell RLV may be proportionally lower.
      RLV_2D = 0.0
      DO Row = 1, NRowsTot 
        DO Col = 1, NColsTot
          IF (RootArea(Row,Col) > 1.E-6) THEN
            RLV_2D(row,col) = RLV_AVG 
     &           * RootArea(row,col) / CellArea(row,col)
          ENDIF
        ENDDO
      ENDDO

!***********************************************************************
      RETURN
      END SUBROUTINE INROOT_2D
!=======================================================================



!=======================================================================
!     Subroutine  Aggregate_Roots converts 2D RLV and root mass to 1D
!-----------------------------------------------------------------------
      SUBROUTINE Aggregate_Roots(CELLS,
     &    FirstRow, RFAC3, RLV_2D, SOILPROP,  !Input
     &    RLV, RootLength, RtLen_2D,          !Output
     &    TotRootMass, TRLV)                  !Output

      Use Cells_2D
      IMPLICIT NONE
      SAVE

      TYPE (CellType), INTENT(IN) :: CELLS(MaxRows,MaxCols)
      INTEGER, INTENT(IN) :: FirstRow
      REAL, DIMENSION(MaxRows,MaxCols), INTENT(IN) :: RLV_2D
      REAL, INTENT(IN) :: RFAC3
      TYPE (SoilType), INTENT(IN) :: SOILPROP

      REAL, DIMENSION(NL), INTENT(OUT) :: RLV
      REAL, INTENT(OUT) :: TRLV, TotRootMass

      REAL, DIMENSION(MaxRows,MaxCols), INTENT(OUT) :: RtLen_2D, 
     &    RootLength
      INTEGER, DIMENSION(MaxRows,MaxCols) :: Cell_Type
      REAL TotalRootLength, Rowspc_cm

      INTEGER Row, Col, L, NLAYR
      REAL, DIMENSION(NL) :: DLAYR, RtLen_1D
      TYPE (CellStrucType) Struc(MaxRows,MaxCols)
      REAL, DIMENSION(MaxRows,MaxCols) :: CellArea, ColFrac, 
     &    Width, Thick

!     Variables available in 2D CELLS
      STRUC = CELLS%STRUC
      Thick = STRUC%THICK
      Width = STRUC%WIDTH
      ColFrac = BedDimension % ColFrac
      Rowspc_cm = BedDimension % Rowspc_cm
      Cell_Type = STRUC%Cell_Type
      CellArea = STRUC % CellArea

      DLAYR = SOILPROP % DLAYR
      NLAYR = SOILPROP % NLAYR

!-----------------------------------------------------------------------
      TRLV = 0.0
      TotalRootLength = 0.0

      DO Row = FirstRow, NRowsTot
        DO Col = 1, NColsTot
          SELECT CASE(Cell_Type(row,col))
          CASE(3,4,5)
!           RootLength is the total root length in each cell and is
!             additive across a row.
            RootLength(row,col) = RLV_2D(row,col) * CellArea(row,col)
!                    cm[root]     cm[root]
!                 ------------- = --------- * cm2[cell area]
!                 cm[rowlength]   cm3[soil]

            TotalRootLength = TotalRootLength + RootLength(row,col)

            RtLen_2D(Row,Col) = RootLength(row,col) / Rowspc_cm
!                cm[root]          cm[root]      1
!                ---------    = ------------- * ---- 
!                cm2[soil]      cm[rowlength]    cm
          END SELECT
        ENDDO
      ENDDO

      TRLV = TotalRootLength * 2.0 / Rowspc_cm
!     cm[root]     cm[root]       1
!     -------- = ------------- * ----
!        cm2     cm[rowlength]    cm

      TotRootMass = TRLV / RFAC3 * 1.E4 * 10.
!               cm[root]    g[root]    1E4 cm2    10(kg/ha)
!      kg/ha  = -------- * -------- * -------- * ---------
!                  cm2     cm[root]      m2       (g/m2)

!     Aggregate root length in cells across a row to get the total 
!     root length in each layer. 
      CALL Cell2Layer_2D(
     &   RtLen_2D, Struc, NLAYR,               !Input
     &   RtLen_1D)                             !Output

      DO L = 1, NRowsTot
        RLV(L) = RtLen_1D(L) / DLAYR(L)
!    cm[root]       cm[root]            1
!  ----------- = -------------- * -----------------
!  cm3[ground]   cm2[row length]   cm[row thickness]
      ENDDO

      RETURN
      END Subroutine Aggregate_Roots
!=======================================================================


!-----------------------------------------------------------------------
!       Variable definitions
!-----------------------------------------------------------------------
! AGRRT     Mass of CH2O required for new root growth (g[CH2O] / g[root])
! CGRRT     Carbon demand for new root growth (g[CH2O] / m2 / d)
! CROP      Crop identification code 
! CUMDEP    Cumulative depth of soil profile (cm)
! DEP       Cumulative soil depth (cm)
! DEPMAX    Maximum depth of reported soil layers (cm)
! CellArea  Soil area (depth x width) (cm2)
! CumRootMass Cumulative growth of roots (kg/ha)
! DS(Row,Col)     Cumulative depth in soil layer L (cm)
! DTX       Thermal time that occurs in a real day based on vegetative 
!             development temperature function (thermal days / day)
! DUL(Row,Col)    Volumetric soil water content at Drained Upper Limit in soil 
!             layer L (cm3 [H2O] /cm3 [soil])
! ESW(Row,Col)    Plant extractable soil water by layer (= DUL - LL) (cm3/cm3)
! FILECC    Path plus filename for species file (*.spe) 
! FRRT      Fraction of vegetative tissue growth that goes to roots on a 
!             day (g[root] / g[veg])
! GRESPR(Row,Col) Growth respiration for new root growth in layer L 
! LL(Row,Col)     Volumetric soil water content in soil layer L at lower limit
!             ( cm3/cm3)
! LUNCRP    Logical unit number for FILEC (*.spe file) 
! LUNIO     Logical unit number for FILEIO 
! MRESPR(Row,Col) Maintenance respiration for new root growth in layer L 
! NL        Maximum number of soil layers = 20 
! NLAYR     Number of soil layers 
! PG        Daily gross photosynthesis (g[CH2O] / m2 / d)
! PLTPOP    Plant population (# plants / m2)
! PORMIN    Minimum pore space required for supplying oxygen to roots for 
!             optimal growth and function (cm3/cm3)
! RESPS(Row,Col)  Total respiration for new root growth in layer L 
! RFAC1     Root length per unit  root weight. (cm[root]/g[root])
! RFAC2     Root depth increase rate with time (cm / physiol. day)
! RFAC2H    Root width increase rate with time (cm / physiol. day)
! RFAC3     Ratio of root length to root weight at the current time (cm[root]/g[root])
! RLDF(Row,Col)   Combined weighting factor to determine root growth distribution
! RLDSM     Minimum root length density in a given layer, below which 
!             drought-induced senescence is not allowed.
!             (cm [root ]/ cm3 [soil])
! RLINIT    Initial root density (cm[root]/cm[row length])
! RLNEW     New root growth added (cm[root]/cm2[land area]/d)
! RLNEW_2D(row,col) New root growth added per cell (cm[root]/cm2[land area]/d)
! RLSEN(Row,Col)  Root length density senesced today (cm[root]/ cm3[soil])
! RLV_2D(Row,Col)    Root length density for soil layer L (cm[root] / cm3[soil])
! RLV_WS(Row,Col) Cell root density reduced by water stress
! RLVnew_2D(row,col) New RLV added today per cell (cm[root] / cm3[soil])
! RO        Respiration coefficient that depends on total plant mass
!             (g[CH2O] / g[tissue])
! RootLength(row,col) Root length per row length (cm[root]/cm[row length])
! RP        proportion of the day's photosynthesis which is respired in the 
!             maintenance process 
! RRLF(Row,Col)   Root length density factor ratio (RLDF(Row,Col) / TRLDF) 
! RTDEPc(Col)     Root depth for each soil column (cm)
! RTWIDr(Row)     Root width for each soil row (cm)
! RTDEPI    Depth of roots on day of plant emergence. (cm)
! RtLen_1D(L) Root length per area (cm[root]/cm2[land area])
! RtLen_2D(Row,Col) Root length per area (cm[root]/cm2[land area])
! RTLenNew  New root length today per row length (cm[roots]/cm[row length])
! RTWIDI    Width of roots on day of plant emergence. (cm)
! RTEXF     Fraction root death per day under oxygen depleted soil 
! RTSDF     Maximum fraction of root length senesced in a given layer per 
!             physiological day when water content in a given layer falls 
!             below 25 % of extractable soil water. 
! RTSEN     Fraction of existing root length which can be senesced per 
!             physiological day. (fraction / ptd)
! RTSURV    Fraction survival of roots on a given day, taking into account 
!             death due to excess or deficit water conditions 
! RTWT      Dry mass of root tissue, including C and N
!             (g[root] / m2[ground])
! SAT(Row,Col)    Volumetric soil water content in layer L at saturation
!             (cm3 [water] / cm3 [soil])
! SRDOT     Daily root senescence (g / m2 / d)
! SUMEX     Sum over all layers of water excess factor times depth
! SUMRL     Sum of root length density (integral over depth)
! SWV(Row,Col)     Volumetric soil water content in cell(Row,Col)
!             (cm3 [water] / cm3 [soil])
! SWDF      Soil water deficit factor for layer with deepest roots (0-1) 
! SWEXF     Excess water stress factor for layer with deepest roots (0-1) 
! SWFAC     Effect of soil-water stress on photosynthesis, 1.0=no stress, 
!             0.0=max stress 
! TABEX     Function subroutine - Lookup utility 
! TotalRootLength Total root length (cm[root]/cm[row length])
! TotRootMass Current mass of roots (kg/ha)
! TRLDF     Total root length density factor for root depth (cm)
! TRLV      Total root length per square cm soil today (cm[root]/cm2[land area])
! TRLV_min  Minimum root length per square cm for senescence to occur (cm[root]/cm2[land area])
! VSTAGE    Number of nodes on main stem of plant 
! WR(Row,Col)     Root hospitality factor, used to computer root water uptake 
! WRDOTN    Dry weight growth rate of new root tissue including N but not C 
!             reserves (g[root] / m2[ground]-d)
! WTNEW     Initial mass of seedling or seed (g / plant)
! XRTFAC(I) V-stage at which rate of increase in root depth per 
!             physiological day is YRTFAC(I). (# leaf nodes)
! YRTFAC(I) Rate of increase in root depth per degree day at V-stage 
!             XRTFAC(I). (cm / (physiol. day))
! XRTFACH(I) V-stage at which rate of increase in root width per 
!             physiological day is YRTFAC(I). (# leaf nodes)
! YRTFACH(I) Rate of increase in root width per degree day at V-stage 
!             XRTFAC(I). (cm / (physiol. day))
!***********************************************************************
!      END SUBROUTINES ROOTS, IPROOT, and INROOT
!=======================================================================


!=======================================================================
!  OPRoots_2D, Subroutine, C.H.Porter from Soil Water portions of OPDAY
!  Generates output for daily soil water data
!-----------------------------------------------------------------------
!  REVISION       HISTORY
!  07/02/2009 CHP Written
!-----------------------------------------------------------------------
!  Called from:   WatBal2D
!  Calls:         None
!=======================================================================
      SUBROUTINE OPRoots_2D(TotRootMass, RFAC3, RLV_2D, Thick, Width)
!                                 kg/ha,  cm/g, cm/cm3,   cm , cm
!-----------------------------------------------------------------------
      USE Cells_2D
      USE ModuleData
      IMPLICIT NONE
      EXTERNAL YR_DOY, GETLUN, HEADER, INCDAT
      SAVE

      REAL, DIMENSION(MaxRows,MaxCols), INTENT(IN) :: RLV_2D,Thick,Width
      REAL, INTENT(IN) :: TotRootMass, RFAC3

      CHARACTER*1 IDETG, IDETL, RNMODE
      CHARACTER*10 OUTRoot
      PARAMETER (OUTRoot = 'Root2D.OUT')
      CHARACTER*17 FMT

      INTEGER COL, DAS, DOY, DYNAMIC, ERRNUM, FROP
      INTEGER NOUTDW, ROW, RUN
      INTEGER YEAR, YRDOY, REPNO, YRSTART, INCDAT

      LOGICAL FEXIST, DOPRINT

!-----------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.
      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH
      
      CALL GET(CONTROL)

      DAS     = CONTROL % DAS
      DYNAMIC = CONTROL % DYNAMIC
      FROP    = CONTROL % FROP
      RUN     = CONTROL % RUN
      RNMODE  = CONTROL % RNMODE
      REPNO   = CONTROL % REPNO
      YRDOY   = CONTROL % YRDOY

      CALL YR_DOY(YRDOY, YEAR, DOY) 

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC == SEASINIT) THEN
!-----------------------------------------------------------------------
!   Set initial values to calculate average values
!-----------------------------------------------------------------------
      CALL GET(ISWITCH)
      IDETL   = ISWITCH % IDETL
      IDETG   = ISWITCH % IDETG

      IF (IDETG == 'N' .OR. IDETL == '0') THEN
        DOPRINT = .FALSE.
      ELSE
        DOPRINT = .TRUE.
      ENDIF
      IF (.NOT. DOPRINT) RETURN

!-----------------------------------------------------------------------
!   Generate headings for output file
!-----------------------------------------------------------------------
      CALL GETLUN('OUTRoot', NOUTDW)
      INQUIRE (FILE = OUTRoot, EXIST = FEXIST)
      IF (FEXIST) THEN
        OPEN (UNIT = NOUTDW, FILE = OUTRoot, STATUS = 'OLD',
     &    IOSTAT = ERRNUM, POSITION = 'APPEND')
      ELSE
        OPEN (UNIT = NOUTDW, FILE = OUTRoot, STATUS = 'NEW',
     &    IOSTAT = ERRNUM)
        WRITE(NOUTDW,'("*2D ROOTS DAILY OUTPUT FILE")')
      ENDIF

!-----------------------------------------------------------------------
!     Variable heading for WATER.OUT
!-----------------------------------------------------------------------
      IF (RNMODE .NE. 'Q' .OR. RUN .EQ. 1) THEN
        IF (RNMODE .EQ. 'Q') THEN
          CALL HEADER(SEASINIT, NOUTDW, REPNO)
        ELSE
          CALL HEADER(SEASINIT, NOUTDW, RUN)
        ENDIF

        YRSTART = YRDOY
        CALL YR_DOY(INCDAT(YRSTART,-1),YEAR,DOY)
      ENDIF

!***********************************************************************
!***********************************************************************
      ENDIF !DYNAMIC CONTROL
!***********************************************************************
!***********************************************************************
!     Daily Output
!***********************************************************************
      IF (DYNAMIC == SEASINIT .OR. DYNAMIC == OUTPUT .OR. 
     &      DYNAMIC == SEASEND) THEN
!-----------------------------------------------------------------------
      IF (DOPRINT) THEN
!           Print initial conditions, 
        IF (DYNAMIC == SEASINIT .OR.
!           Print every FROP days, and
     &     (DYNAMIC .EQ. OUTPUT .AND. MOD(DAS, FROP) .EQ. 0) .OR. 
!           Print on last day if not already done.
     &     (DYNAMIC .EQ. SEASEND  .AND. MOD(DAS, FROP) .NE. 0)) THEN

          Write(NOUTDW,'(/,"Year DOY:",I5,I4.3)') YEAR, DOY
          Write(NOUTDW,'("Root Mass =     ",F10.2," kg/ha")')TotRootMass
          Write(NOUTDW,'("Root L:M ratio =",F10.2," cm/g")') RFAC3

          Write(NOUTDW,'("  Column ->",20I10)') (Col, Col=1, NColsTOT)
          Write(NOUTDW,'("Width(cm)->",20F10.3)') 
     &                  (width(1,Col),Col = 1, NColsTOT)
          Write(NOUTDW,'("      Thick")') 
          Write(NOUTDW,'("Lyr    (cm)   ------- ",
     &  "RLV (cm[root]/cm3[soil] -------")')
          WRITE(FMT,'("(I3,F8.1,",I2,"F10.4)")') NColsTot 
          DO Row = 1, NRowsTot  
            Write(NOUTDW,FMT)     
     &      Row, Thick(Row,1), (RLV_2D(Row,Col),Col = 1, NColsTOT) 
          Enddo 

        ENDIF
      ENDIF

!***********************************************************************
!***********************************************************************
!     SEASEND - Sesaonal Output
!***********************************************************************
        IF (DYNAMIC .EQ. SEASEND) THEN
!-----------------------------------------------------------------------
            !Close daily output files.
            CLOSE (NOUTDW)
        ENDIF
!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE OPRoots_2D
!=======================================================================
