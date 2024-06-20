C=======================================================================
C  PT_ROOTGR_2D, Subroutine
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
C  RLDF(i, j) : A root length density factor for soil layer L used to calculate
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

      SUBROUTINE PT_ROOTGR_2D (DYNAMIC, ISWWAT, CELLS, YRDOY,
     &    DTT, FILEIO, GRORT, ISWNIT, PLTPOP, SWFAC,    !Input
     &    SOILPROP,                                     !Input
     &    CUMDEP, RLV, RLV_2D, RTDEP)                   !Output

!-----------------------------------------------------------------------
      USE ModuleDefs
      USE Cells_2D
      IMPLICIT  NONE
      EXTERNAL PT_IPROOT_2D, AGGREGATE_ROOTS, PT_OPROOTS_2D, 
     &  PT_INROOT_2D
      SAVE

      LOGICAL FIRST
      CHARACTER*1   ISWNIT, ISWWAT
      CHARACTER*30 FILEIO

      INTEGER DYNAMIC, YRDOY0, YRDOY
      INTEGER ROW, Col, LastCol, LastRow, iniRT_StartRow
      INTEGER FirstRow
      REAL HalfRow, ROWSPC_cm, CumRootMass

      REAL CUMDEP, DEPMAX, DTT, GRORT, PLTPOP, RTSEN   
      REAL RLNEW, RLWR, RNFAC, RTDEP
      REAL SDEPTH, SWDF, SWFAC, TRLDF, TRLV
      REAL  CumWid, LastCumDep, LastCumWid
      REAL HalfBed, RLV_max, RFAC3
      REAL PORMIN, SWEXF, RTSURV, RTEXF, RLDSM, RTSDF, RTWTMIN, TRLV_MIN
      REAL RTDEPnew, RTWID, RTLSenes, RTMasSenes, RLSENTOT
      REAL RTWIDr(MaxRows), RTWIDnew(MaxRows), WidMax(MaxRows)
      REAL WidFrac(MaxRows,MaxCols), DepFrac(MaxRows,MaxCols) 
      REAL TotRootMass, TotRootArea, CelRootArea(MaxRows,MaxCols)

      INTEGER, DIMENSION(MaxRows,MaxCols) :: TypeCell
      REAL, DIMENSION(MaxRows,MaxCols) :: NO3_2D, NH4_2D
      REAL, DIMENSION(MaxRows,MaxCols) :: RLV_2D, RLDF, SAT, DUL, LL
      REAL, DIMENSION(MaxRows,MaxCols) :: Thick, Width, CellArea, ESW
      REAL, DIMENSION(MaxRows,MaxCols) :: RLV_WS, WR, SWV

      TYPE (CellType) CELLS(MaxRows,MaxCols)
      TYPE (CellStrucType) Struc(MaxRows,MaxCols)

      REAL, DIMENSION(NL) :: RLV
      TYPE (SoilType) SOILPROP

!***********************************************************************
!***********************************************************************
!     Seasonal Initialization - Called once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      CALL PT_IPROOT_2D(FILEIO, RTEXF, RLDSM, RTSDF,        !Input
     &     RTSEN, RTWTMIN, PORMIN, RLWR, SDEPTH, ROWSPC_cm) !Output

      YRDOY0 = YRDOY
      RLV      = 0.0
      RLV_2D   = 0.0
      RTWIDr   = 0.0
      RTDEP    = 0.0
      RTWID    = 0.0
      TRLV     = 0.0
      rlv_max  = 0.0
      RLNEW    = 0.0
      CUMDEP   = 0.0
      RTLSenes = 0.0
      CumRootMass = 0.0
      TotRootMass = 0.0
      RFAC3 = RLWR *1.E-4

!     Variables available in 2D CELLS
      STRUC = CELLS%STRUC
      Thick = STRUC%THICK
      Width = STRUC%WIDTH
      CellArea = STRUC%CellArea
      TypeCell = STRUC%Cell_Type
      DUL = CELLS%STATE%DUL
      LL  = CELLS%STATE%LL
      SAT = CELLS%STATE%SAT
      WR  = CELLS%STATE%WR

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

!     at emergence, assume that the initial root area starts from Row 1
!     chp 2024-04-10 I think that we were starting to think about organic
!     matter layer(s) above the first soil layer. Not ever really implemented
!     so maybe don't really need this iniRT_StartRow anymore. But OK, it's assumed
!     to be the top layer so shouldn't matter.
      iniRT_StartRow = 1 
      FIRST = .TRUE.

!     Width of half row (cm) used to scale up to field area basis. 
      HalfRow = BedDimension % ROWSPC_cm / 2
      HalfBed = BedDimension % BEDWD / 2

!      CALL Aggregate_Roots(CELLS,
!     &    FirstRow, HalfRow, RLV_2D,          !2D Input
!     &    RFAC3, SOILPROP,                    !1D Input
!     &    RLV, TRLV, TotRootMass)             !1D Output

      LastRow = 1
      LastCol = 1

      CALL PT_OPRoots_2D(TotRootMass, RLWR,RLV_2D,RLV,DepFrac, WidFrac, 
     &    Thick, Width, RTDEP, RTWID, RTWIDr, CumRootMass, RTMasSenes,
     &    GRORT, DTT, SDEPTH, LastRow, LastCol)

!***********************************************************************
!***********************************************************************
!     Daily rate calculations 
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!-----------------------------------------------------------------------
      SWV = CELLS%STATE%SWV 
      TotRootArea=0.
      CelRootArea=0.

!     10/20/2005 Limit RLV decrease due to senscence to 
!       a minimum resulting root weight 
      IF (RTWTMIN > 0.0) THEN
!       Same units as TRLV (cm[root]/cm[row-length])
        TRLV_MIN = RTWTMIN * RLWR * HalfRow * 1.E-4
!        cm[root]      g[root]   cm[root]                    m2
!      -----------   = ------- * -------- * cm[Row Width] * ----
!      cm[row length]    m2      g[root]                    cm2
      ELSE
!       Set TRLV_MIN to zero -- no minimum root mass
        TRLV_MIN = 0.0
      ENDIF

!-----------------------------------------
!     Initial root distribution:
      IF (FIRST) THEN
!       After planting date, when Root growth rate >0, could be before Emergence date
        FIRST  = .FALSE.

!       CHP 5/29/03 - Added this section based on CROPGRO initialization
!           at emergence. 
!       INITIALIZE ROOT DEPTH AT EMERGENCE
!       DISTRIBUTE ROOT LENGTH EVENLY IN ALL LAYERS TO A DEPTH OF
!       RTDEPTI (ROOT DEPTH AT EMERGENCE)
        Call PT_INROOT_2D(
     &    DepMax, GRORT, HalfRow, iniRT_StartRow, PLTPOP,   !Input
     &    RLWR, SDEPTH, SOILPROP, Thick, TypeCell,          !Input
     &    WidMax, Width,                                    !Input
     &    RLV_2D, RTDEP, RTWID, RTWIDr, DepFrac, WidFrac)   !Output

!***********************************************************************
      ELSE !if not first, i.e not initial
!-----------------------------------------

!       Daily root growth and distribution
        TRLDF  = 0.0
        CUMDEP = 0.0
        RNFAC  = 1.0
        RTDEPnew = RTDEP 

!       Here, RTDEP is previous day's maximum root depth 
!       RTDEPnmew will be Today's maximum root depth 
        RTWIDnew = RTWIDr !it is array

!------------------------------------------------------------------
!     CHP 2024-04-12: The following is dimensionally incorrect but 
!       fixing it breaks the model.
        RLNew  =  GRORT * RLWR *  PLTPOP !* 1.E-4
!      cm[root]    g[root]   cm[root]   plants   m2
!      --------- = ------- * -------- * ------ * ---
!      cm2[ground]  plant    g[root]      m2     cm2
!------------------------------------------------------------------

!       First, root expansion.
!       Root depth is calculated in column 1 only.
!       Root width is calculated for each row. 
        RowLoop: DO Row = FirstRow, NRowsTot
          IF (TypeCell(Row,1) < 3 .OR. TypeCell(Row,1) > 5) CYCLE
          LastCumdep = CUMDEP
!         now calculate DepFrac layer by layer, 
!         LastCumdep is the top of the calculated layer, CUMDEP is buttom of the calculated layer
          CUMDEP = CUMDEP + Thick(Row,1)
          CumWid = 0.0
          ColLoop: Do Col = 1, NColsTot

            LastCumWid = CumWid
            CumWid = CumWid + Width(Row,Col)

!           Starting to calculate RLDF
            SWDF = 1.0
            SWEXF = 1.0
            IF (ISWWAT .EQ. 'Y') THEN  
              IF (SAT(Row,Col) - SWV(Row,Col) .LT. PORMIN) THEN
!               this will be used to calculate excess water senesence
                SWEXF = (SAT(Row,Col) - SWV(Row,Col)) / PORMIN
                SWEXF = MIN(SWEXF, 1.0)
              ENDIF
              ESW(ROW, Col) = DUL(ROW,Col) - LL(ROW,Col)
              SWDF   = 1.0
              IF (SWV(ROW,Col)-LL(ROW,Col) .LT. 0.25*ESW(ROW,Col)) THEN
                SWDF = 4.0*(SWV(ROW,Col) - LL(ROW,Col)) / ESW(ROW,Col)
              ENDIF
              SWDF = AMAX1 (SWDF,0.0) 
            ENDIF

!---------------------------------------------------------------------
!           Water stress senescence 
            RTSURV = MIN(1.0,(1.-RTSDF*(1.-SWDF)),(1.-RTEXF*(1.-SWEXF)))

            IF (RLV_2D(Row,Col) > RLDSM .AND. TRLV+RLNEW > TRLV_MIN)THEN
              RLV_WS(Row,Col) = RLV_2D(Row,Col) * (1.0 - RTSURV)
            ELSE
              RLV_WS(Row,Col) = 0.0
            ENDIF

            IF (ISWNIT .NE. 'N') THEN 
!             RNFAC = 1.0 - (1.17 * EXP(-0.15 * TOTIN)
!             RNFAC = 1.0 - (1.17 * EXP(-0.15 * (SNH4(L) + SNO3(L))))
              RNFAC = 1.0 - (1.17 * EXP(-0.15 * 
     &            (NO3_2D(row,col) + NH4_2D(row,col))))
              RNFAC = AMAX1 (RNFAC,0.01)
            ENDIF
!---------------------------------------------------------------------
!           Weighting factor for each cell, RLDF, based on WR (i.e. SHF), cell area
!           and water factors.  
!           RLDF(Row,Col) =AMIN1(SWDF,RNFAC)*SHF(Row)*CellArea(Row,Col) 
            RLDF(Row,Col) =AMIN1(SWDF,RNFAC)*WR(Row,col)*
     &                          CellArea(Row,Col)
!           End of calculation RLDF
            
!           Calculate new vertical growth in column 1 only
            IF (COL == 1) THEN
!             Starting to calculate DepFrac
              IF (RTDEP >= CUMDEP) THEN 
                DepFrac(Row,Col) = 1.0
              ELSEIF (RTDEP >= LastCumDep)THEN
!               Roots have partially filled the depth of this cell
                IF (WR(row,col) > 0. .AND. RLNEW >0.) THEN
!                 The following 1.3 is an assumed parameter to affect the root depth grow, should goes to *.spe file
!                 RTDEPnew = RTDEP + DTT * 1.3 *
                  RTDEPnew = RTDEP + DTT * 1.0 *
     &                       AMIN1((SWFAC * 2.0 ), SWDF)
                  RTDEPnew = MIN(RTDEPnew, DEPMAX)
                ENDIF
                DepFrac(Row,Col) = MIN(1.0, 1. - (CUMDEP - RTDEPnew)/
     &                         Thick(Row,Col))
!               if the new root is more than one row, take minum
!               In the following statement, lastRow is Yesterday's Last Row of root
                IF (Row > LastRow) LastRow = Row
!               JZW: we'd better to add exit statement
              ELSE
!               No roots in this cell 
!               JZW this is equivalent exit the do loop of row
                DepFrac(Row,Col) = 0.0
              ENDIF ! end IF (RTDEP >= CUMDEP) 

!             Check for new roots in this cell
              IF (RTDEPnew > LastCumDep .AND. 
     &          RTDEP <= LastCumDep) THEN
!               New roots have just grown into this cell
                RTWIDnew(Row) = Width(Row,Col)
                ! If new root grow more than one row, the DepFrac=1
                DepFrac(Row,Col) = MIN(1.0, 1. - (CUMDEP - RTDEPnew) / 
     &                         Thick(Row,Col))
                IF (Row > LastRow) LastRow = Row    
              ENDIF
!             finish calculate DepFrac

            ELSE ! if col!=1, calculate WidFrac
!             Calculate new horizontal growth in this cell (RTWIDnew) 
!             horizontal portion of cell occupied by roots (WidFrac)
!             Horizontal root growth only occurs when DepFrac of adjacent 
!               cell is > 0.99 (JZW: This 0.99 did not realized in the codes).  No need to calculate for Column 1, since
!               bug statement 0.99 is missing
!               width fraction is initialized to 1.0 there.
              IF (RTWIDr(Row) >= CumWid) THEN
                WidFrac(Row,Col) = 1.0
!               JZW change May 9,2012 
                DepFrac(Row,Col) = min(1.0, DepFrac(Row, col-1)) 
              ELSEIF (RTWIDr(Row) >= LastCumWid) THEN
!               Roots have partially filled the width of this cell
                IF (WR(row,col) > 0.0 .AND. RLNEW >0.0) THEN
!                 The following 0.6 is an assumed parameter to affect the root width grow, should goes to *.spe file
!                 RTWIDnew(Row) = RTWIDr(Row) + DTT * 0.6 *
                  RTWIDnew(Row) = RTWIDr(Row) + DTT * 1.0 *
     &                            AMIN1((SWFAC*2.0),SWDF) 
                  RTWIDnew(Row) = MIN(RTWIDnew(Row), WIDMAX(Row))
                Else
!                 JZW need to check if it is correct here
                  DepFrac(Row,Col) = 0.0 
                ENDIF
                WidFrac(Row,Col) = MIN(1.0, 1. - (CumWid -RTWIDnew(Row))
     &                          / Width(Row,Col))    
                IF (Col > LastCol) LastCol = Col
              ELSE
!               No roots in this cell
                WidFrac(Row,Col) = 0.0
                DepFrac(Row,Col) = 0.0
              ENDIF ! end of partial filly filled this col

!             Check for new roots in this cell
              IF (RTWIDnew(Row) > LastCumWid .AND. 
     &                RTWIDr(Row) <= LastCumWid) THEN
!               New roots have just grown into this cell
                WidFrac(Row,Col) = MIN(1.0, 1. -(CumWid-RTWIDnew(Row))
     &                          / Width(Row,Col))
              
!               JZW change May 9,2012 
                DepFrac(Row,Col) = min(1.0, DepFrac(Row, col-1)) 
                IF (Col > LastCol) LastCol = Col 
              ENDIF ! end if new grow in this cell
            ENDIF !! end of  col!=1  

!           Re calculate the DepFrac from Row=1 to SeedRow
            IF (Row < iniRT_StartRow) Then 
!             current row is above initial root start row
              DepFrac(Row, Col) =0. 
!           Elseif (Row == iniRT_StartRow) then !current row is in initial root start row
!             IF (CELLS(Row,Col)%STATE%WR > 0. .AND. RLNEW >0.) THEN
!               !RTDEPnew = RTDEP + DTT * 1.3 *
!               RTDEPnew = RTDEP + DTT * 1.0 *
!     &                       AMIN1((SWFAC * 2.0 ), SWDF)
!                RTDEPnew = MIN(RTDEPnew, DEPMAX)
!              Endif
!              if (RTDEPnew .LE. CUMDEP) then !root depth is within SeedRow
!                  DepFrac(Row,Col) = (RTDEPnew - SDEPTH)/Thick(Row,Col)
!              else ! rootdepth is deeper then seed row
!                  DepFrac(Row,Col) = (CUMDEP - SDEPTH)/Thick(Row,Col)
!              Endif
            Endif ! end of seed row

            If (DepFrac(Row, 1) .GT. 0. ) then
              WidFrac(Row, 1) = 1.0
            else 
              WidFrac(Row, 1) = 0.0
            endif

!-----------------------------------------------------------------------
!           Apply factor for this cell
            RLDF(Row,Col) = 
     &               RLDF(Row,Col)*DepFrac(Row,Col)*WidFrac(Row,Col)
            CelRootArea(Row,Col) =CellArea(Row,Col)
     &               *DepFrac(Row,Col)*WidFrac(Row,Col)
!           Sum of all factors
            TRLDF = TRLDF + RLDF(Row,Col)
            TotRootArea = TotRootArea +  CelRootArea(Row,Col)

            IF (RTWIDnew(Row) < CumWid) EXIT ColLoop
          ENDDO ColLoop 
        ENDDO RowLoop
        RTDEP  = RTDEPnew 
        RTWIDr = RTWIDnew ! it is array

!-------------------------------------------------------------------------
        if ((TRLDF .LT. 1.E-5) .and. (RLNEW .GT. 1.0E-3)) then 
             write (*,*) "Total root length fraction is zero while ",
     &           "there is root grow"
             stop
        Endif

        RLSENTOT = 0.0

!       IF (TRLDF .GE. RLNEW*0.00001) THEN ! JZW ask CHP: different unit, how to compare????
!          RLNEW and RLINIT IS in cm[root]/cm[ground]/d, TRLDF is in cm2 TRLDF has same unit as RLDF for now
        RLDF = RLDF / TRLDF ! RLDF is unitless now


        DO Row = 1, LastRow !JZW LastRow is the last row of root
          DO Col = 1, LastCol 
            IF (TypeCell(Row,Col)<3 .OR. TypeCell(Row,Col) > 5) CYCLE
            RTLSenes = 
     &            RTLSenes + 0.005*RLV_2D(Row,Col) * CellArea(Row,Col)
!           To calculate LastCol need RTWIDr(Row), LastCumWid, RTWIDI

            RLV_2D(Row,Col) = RLV_2D(Row,Col)
     &           + RLDF(Row,Col) * RLNEW /CellArea(Row,Col)
!             cm         cm     1
!            -------  = ---- * ----
!             cm3        cm    cm2

!           Subtract root senescence and check for negative value
            RLV_2D(Row,Col) = RLV_2D(Row,Col) - 0.005 * RLV_2D(Row,Col)
            RLV_2D(Row,Col) = AMAX1 (RLV_2D(Row,Col),0.0)
          END DO
        ENDDO
      ENDIF ! end of IF not (FIRST)

      TRLV = 0.0
      DO Row = 1, LastRow
        Do Col = 1, LastCol
          IF (TypeCell(Row,Col) < 3 .OR. TypeCell(Row,Col) > 5) CYCLE
          TRLV = TRLV + RLV_2D(Row,Col) * CellArea(Row,Col) 
!          cm     cm      cm
!         -----= ---- + ------- * cm2
!          cm     cm      cm3
!          JZW, TRLV is calculated in PT_Aggregate_Roots, we do not need to calculate here
        End do

        IF (RTWIDr(Row) > RTWID) RTWID = RTWIDr(Row) 
!       RTWID is not used, it can be as output of this subroutine for watch variable
      ENDDO

      CALL Aggregate_Roots(CELLS,
     &    FirstRow, HalfRow, RLV_2D,          !2D Input
     &    RFAC3, SOILPROP,                    !1D Input
     &    RLV, TRLV, TotRootMass)             !1D Output

       CELLS%STATE%RLV = RLV_2D

       DO Row = 1, LastRow 
         RLV(Row) = AMAX1 (RLV(Row),0.0)
       Enddo

!      RLWR  Root length to weight ration, (cm/g)*1E-4 
       RTMasSenes = (RTLSenes /HalfRow/ RLWR) * 10.
       TotRootMass=(TRLV /HalfRow/ RLWR) * 10.
!                  cm    1     g   10000 cm2   10(kg/ha)
!         kg/ha  = ---*---- * -- * -------- * ---------
!                  cm    cm   cm       m2       (g/m2)

        CumRootMass=CumRootMass+GRORT * PLTPOP *  10 ! 1 ha = 10000m2
       ! kg[root]       kg     g      # plants     kg/ha
       !----------- = -----+ ------ * --------*  --------
       ! ha             ha    plant      m2         g/m2

!***********************************************************************
      ELSEIF (DYNAMIC == OUTPUT .OR. DYNAMIC == SEASEND) THEN
!-----------------------------------------------------------------------

!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE PT_ROOTGR_2D
C=======================================================================


C=======================================================================
C  PT_IPROOT_2D, Subroutine
C
C  Input data for potato root module
C-----------------------------------------------------------------------
C  Revision history
C
C  08/23/2001 CHP Written
C  10/25/2002 CHP Modified read format for Y2K
C  08/12/2003 CHP Added I/O error checking
C-----------------------------------------------------------------------

      SUBROUTINE PT_IPROOT_2D(FILEIO, RTEXF, RLDSM,RTSDF,       !Input
     &     RTSEN, RTWTMIN, PORMIN, RLWR, SDEPTH, ROWSPC_cm)      !Output

!     ------------------------------------------------------------------

      IMPLICIT NONE
      EXTERNAL GETLUN, ERROR, FIND, IGNORE

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
      REAL PORMIN, RTEXF, RLDSM, RTSDF, RTWTMIN, RTSEN
      REAL RLWR, SDEPTH, ROWSPC_cm
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
        !READ (LUNIO,'(55X,F5.1)', IOSTAT=ERR) SDEPTH ; LNUM = LNUM + 1
         READ (LUNIO,'(43X, F5.1, 6X, F5.1)', IOSTAT=ERR)  
     &    ROWSPC_cm, SDEPTH ; LNUM = LNUM + 1  
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
        !       Optional minimum root mass for senescence (g/m2)
        IF (CHAR(10:13) .EQ. 'PORM') THEN
          READ (CHAR,'(14X,F6.0)',IOSTAT=ERR) PORMIN
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEC,LNUM)
        ENDIF
        IF (CHAR(10:13) .EQ. 'RLWR') THEN 
          READ (CHAR,'(14X,F6.0)',IOSTAT=ERR) RLWR
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEC,LNUM)
          !EXIT
        ENDIF
        IF (CHAR(10:16) .EQ. 'RTWTMIN') THEN
          READ (CHAR,'(16X,F4.0)',IOSTAT=ERR) RTWTMIN 
          ! JZW need to solve What is wrong for the formate????
          IF (ERR /= 0 .OR. RTWTMIN < 0.) THEN
            RTWTMIN = 0.0
          Endif
        ENDIF
        IF (CHAR(10:14) .EQ. 'RTEXF') THEN
          READ (CHAR,'(14X,F6.0)',IOSTAT=ERR) RTEXF
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEC,LNUM)
        ENDIF
        IF (CHAR(10:14) .EQ. 'RTSDF') THEN
          READ (CHAR,'(14X,F6.0)',IOSTAT=ERR) RTSDF
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEC,LNUM)
        ENDIF
        IF (CHAR(10:14) .EQ. 'RLDSM') THEN
          READ (CHAR,'(14X,F6.0)',IOSTAT=ERR) RLDSM
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEC,LNUM)
        ENDIF
        IF (CHAR(10:14) .EQ. 'RTSEN') THEN
          READ (CHAR,'(14X,F6.0)',IOSTAT=ERR) RTSEN
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEC,LNUM)
          EXIT
        ENDIF
      ENDDO

      CLOSE (LUNCRP)

!!     Convert RLWR from 1E4 cm/g to cm/g
!      RLWR = RLWR / 1.E4

C-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE PT_IPROOT_2D
!=======================================================================

!=======================================================================
!  PT_INROOT Subroutine
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
      SUBROUTINE PT_INROOT_2D(
     &    DepMax, GRORT, HalfRow, iniRT_StartRow, PLTPOP,   !Input
     &    RLWR, SDEPTH, SOILPROP, Thick, TypeCell,          !Input
     &    WidMax, Width,                                    !Input
     &    RLV_2D, RTDEP, RTWID, RTWIDr, DepFrac, WidFrac)   !Output

!     ------------------------------------------------------------------
      USE Cells_2D
      IMPLICIT NONE

      INTEGER Row, Col, iniRT_StartRow, NLAYR
      INTEGER, DIMENSION(MaxRows,MaxCols) :: TypeCell
      REAL DepMax, RLINIT, WidMax(MaxRows)
      REAL HalfRow, X, Z, GRORT, PLTPOP, RLWR, SDEPTH
      REAL RTDEPI, RTDEP, LastCumDep, CumDep
      REAL RTWIDI, RTWID, LastCumWid, CumWid, RTWIDr(MaxRows)
      REAL TotRootArea
      REAL, DIMENSION(MaxRows,MaxCols) :: Thick, Width, CellArea
      REAL, DIMENSION(MaxRows,MaxCols) :: RLV_2D, RootArea
      REAL WidFrac(MaxRows,MaxCols), DepFrac(MaxRows,MaxCols) 
      TYPE (SoilType) SOILPROP
      REAL, DIMENSION(NL) :: DS
      REAL Conc_factor

!-----------------------------------------------------------------------
      NLAYR = SOILPROP % NLAYR
      DS    = SOILPROP % DS

      RTDEPI = MAX(MIN(20.0, DS(NLAYR), DepMax), Thick(1,1))
      RTDEP = RTDEPI

!     Initial root width (specify half because we are modeling half a row)
      RTWIDI = max (WIDTH(iniRT_StartRow,1),  RTDEPI / 2.0)
      IF (BedDimension%BEDWD > 0.0) THEN
        RTWIDI = MIN(BedDimension%BEDWD / 2.0, RTWIDI)
      ENDIF
      RTWIDI = MAX(MIN(RTWIDI, WidMax(1)), Width(1,1))
      RTWID = RTWIDI

      RLV_2D = 0.0
      RootArea = 0.  !cell area containing roots
      TotRootArea = 0.0
      RTWIDr = 0.0
      X = 0.0
      Z = 0.0

!     Distribute root length and width evenly thru cells
      CUMDEP = 0.
      RowLoop: DO Row = 1, NRowsTot 
      !RowLoop: DO Row = 2, NRowsTot ! First layer has no root
        LastCumDep = CUMDEP
        CUMDEP = CUMDEP + Thick(Row,1)
        IF (RTDEPI >= CUMDEP) THEN 
!         RootDepth deeper then current row
          If (Row .LT. iniRT_StartRow) Then
!           Z = portion of cell occupied by root (before today's new growth) 
            Z = 0.
          else
            Z = Thick(Row,1)
          endif
        ELSEIF (RTDEPI > LastCumDep) THEN 
!         Root Depth is in current row
          If (Row .LT. iniRT_StartRow) Then
            Z = 0.
          elseif (Row .EQ. iniRT_StartRow) Then
            Z = RTDEPI - SDEPTH
          else
            Z = RTDEPI - LastCumDep
          endif 
        ELSE
!         This row is below the roots
          Z = 0.0
          EXIT RowLoop
        ENDIF
        
        IF (Row == iniRT_StartRow .OR. Z > 0.98 * Thick(Row,1)) THEN 
          RTWIDr(Row) = RTWIDI
        ELSEIF (Z > 0.0) THEN
          RTWIDr(Row) = WIDTH(Row,1) 
        ENDIF

!       For rows with roots, distribute roots to columns
        CumWid = 0.
        ColLoop: DO Col = 1,NColsTot
          IF (TypeCell(Row,Col) < 3 .OR. TypeCell(Row,Col) > 5) CYCLE
          LastCumWid = CumWid
          CumWid = CumWid + Width(Row,Col)
          CellArea(Row,Col) = Width(Row,Col) * Thick(Row,Col)
          IF (RTWIDI >= CumWid) THEN
!           X is the horizental portion of cell occupied by root 
            X = Width(Row,Col)
          ELSEIF (RTWIDI > LastCumWid) THEN
            X = RTWIDI - LastCumWid
          ELSE
            X = 0.0
            EXIT ColLoop
          ENDIF
          If (Row .LT. iniRT_StartRow) X = 0.
          DepFrac(Row,Col) = MIN(1.0, Z/Thick(Row,Col))
          WidFrac(Row,Col) = MIN(1.0, X/WIDTH(Row,Col))
          IF (ROW == 1 .OR. COL == 1 .OR. Z > 0.98 * Thick(Row,Col))THEN
            RootArea(Row,Col) = X * Z
          ENDIF
          TotRootArea = TotRootArea + RootArea(Row,Col)
        ENDDO ColLoop
      ENDDO RowLoop

!     CHP 2024-04-12: The following is dimensionally incorrect but 
!       fixing it breaks the model.
          RLINIT = GRORT * RLWR * PLTPOP !* 1.E-4
!      cm[root]    g[root]   cm[root]   plants   m2
!      --------- = ------- * -------- * ------ * ---
!      cm2[ground]  plant    g[root]      m2     cm2

      DO Row = 1, NRowsTot 
        DO Col = 1, NColsTot
          IF (RootArea(Row,Col) > 1.E-6) THEN
!           RLV is concentrated in a few cells and will be larger (per cell)
!             than in the 1D model. Total cm of root and g of root are the same.
            Conc_factor = HalfRow / Width(row,col)
            RLV_2D(row,col) = RLINIT / Thick(row,col) * Conc_factor
!                cm[root]      cm[root]      1
!               ----------- = --------- * --------
!                cm3[soil]    cm2[soil]   cm[soil]
          ENDIF
        ENDDO
      ENDDO

!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE PT_INROOT_2D
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
      SUBROUTINE PT_OPRoots_2D(TotRootMass, RLWR,RLV_2D, RLV,DepFrac, 
      !                              kg/ha,  cm/g, cm/cm3, cm/cm3
     &   WidFrac, Thick, Width, RTDEP, RTWID, RTWIDr, CumRootMass,
     &  RTMasSenes, GRORT, DTT, SDEPTH, LastRow, LastCol)

!-----------------------------------------------------------------------
      USE Cells_2D
      USE ModuleData
      IMPLICIT NONE
      EXTERNAL YR_DOY, GETLUN, HEADER, INCDAT
      SAVE
      
      INTEGER LastRow, LastCol

      REAL, DIMENSION(MaxRows,MaxCols), INTENT(IN) :: RLV_2D,Thick,Width
      REAL, DIMENSION(MaxRows,MaxCols), INTENT(IN) :: DepFrac, WidFrac
      REAL, INTENT(IN) :: TotRootMass, RLWR, RTDEP, RTWID, CumRootMass
      REAL RTMasSenes, GRORT, DTT, SDEPTH 
      REAL, DIMENSION(MaxRows), INTENT(IN) :: RTWIDr, RLV

      CHARACTER*1 IDETG, IDETL, RNMODE
      CHARACTER*13 OUTRoot1
      CHARACTER*14 OUTRoot2, OUTRoot3
!     CHARACTER*7 FileName
      !PARAMETER (OUTRoot1 = 'PT_RLV_2D.OUT')
      CHARACTER*17 FMT

      INTEGER COL, DAS, DOY, DYNAMIC, ERRNUM, FROP
      INTEGER NOUTRLV, NOUTDPF, NOUTWDF, ROW, RUN
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
      OUTRoot1 = 'PT_RLV_2D.OUT'
      CALL GETLUN('OUTRoot1',  NOUTRLV)
      INQUIRE (FILE = OUTRoot1, EXIST = FEXIST)
      IF (FEXIST) THEN
        OPEN (UNIT = NOUTRLV, FILE = OUTRoot1, STATUS = 'OLD',
     &    IOSTAT = ERRNUM, POSITION = 'APPEND')
      ELSE
        OPEN (UNIT = NOUTRLV, FILE = OUTRoot1, STATUS = 'NEW',
     &    IOSTAT = ERRNUM)
        WRITE(NOUTRLV,'("*2D Cell Root RLV_2D DAILY OUTPUT FILE")')
      ENDIF

      OUTRoot2 = 'PT_DepFrac.OUT'
      CALL GETLUN('OUTRoot2', NOUTDPF)
      INQUIRE (FILE = OUTRoot2, EXIST = FEXIST)
      IF (FEXIST) THEN
        OPEN (UNIT = NOUTDPF, FILE = OUTRoot2, STATUS = 'OLD',
     &    IOSTAT = ERRNUM, POSITION = 'APPEND')
      ELSE
        OPEN (UNIT = NOUTDPF, FILE = OUTRoot2, STATUS = 'NEW',
     &    IOSTAT = ERRNUM)
        WRITE(NOUTDPF,'("*2D Cell ROOTS DepFrac DAILY OUTPUT FILE")')
      ENDIF

      OUTRoot3 = 'PT_WidFrac.OUT'
      CALL GETLUN('OUTRoot3', NOUTWDF)
      INQUIRE (FILE = OUTRoot3, EXIST = FEXIST)
      IF (FEXIST) THEN
        OPEN (UNIT = NOUTWDF, FILE = OUTRoot3, STATUS = 'OLD',
     &    IOSTAT = ERRNUM, POSITION = 'APPEND')
      ELSE
        OPEN (UNIT = NOUTWDF, FILE = OUTRoot3, STATUS = 'NEW',
     &    IOSTAT = ERRNUM)
        WRITE(NOUTWDF,'("*2D Cell ROOTS WiDFrac DAILY OUTPUT FILE")')
      ENDIF

!-----------------------------------------------------------------------
!     Variable heading for WATER.OUT
!-----------------------------------------------------------------------
      IF (RNMODE .NE. 'Q' .OR. RUN .EQ. 1) THEN
        IF (RNMODE .EQ. 'Q') THEN
          CALL HEADER(SEASINIT, NOUTRLV, REPNO)
          CALL HEADER(SEASINIT, NOUTDPF, REPNO)
          CALL HEADER(SEASINIT, NOUTWDF, REPNO)
        ELSE
          CALL HEADER(SEASINIT, NOUTRLV, RUN)
          CALL HEADER(SEASINIT, NOUTDPF, REPNO)
          CALL HEADER(SEASINIT, NOUTWDF, REPNO)
        ENDIF
        Write(NOUTRLV,'(" Seed Depth     : ",F10.2,
     &        " cm")') SDEPTH
        Write(NOUTDPF,'(" Seed Depth     : ",F10.2,
     &        " cm")') SDEPTH
        Write(NOUTWDF,'(" Seed Depth is  : ",F10.2,
     &        " cm")') SDEPTH
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

          Write(NOUTRLV,'(/,"Year DOY:",I5,I4.3, ", DAS:",I3)') YEAR, 
     &          DOY, DAS
          Write(NOUTRLV,'("CumRootMass based on GRART=",F8.2,
     &        " kg/ha")') CumRootMass
          Write(NOUTRLV,'("Root Mass based on RLV = ",F10.2," kg/ha")')
     &          TotRootMass
          Write(NOUTRLV,'("Root Mass Senes        = ",F10.2," kg/ha")')
     &          RTMasSenes
          Write(NOUTRLV,'("Root L:M ratio =",F10.2," cm/g")') RLWR
          Write(NOUTRLV,'("Grow Rate (GRORT) :",F10.2,"g/plant; 
     &     Growing degree days (DTT)", F6.2)') GRORT, DTT
          Write(NOUTRLV,'("Width(cm)->",20F10.3)') 
     &                  (width(1,Col),Col = 1, NColsTOT)
          Write(NOUTRLV,'("      Thick")') 
          Write(NOUTRLV,'("Lyr    (cm)   ------- ",
     &      "RLV (cm[root]/cm3[soil] -------")')
          WRITE(FMT,'("(I3,F8.1,",I2,"F10.4)")') (NColsTot+1) 
          DO Row = 1, NRowsTot  
            Write(NOUTRLV,FMT)  Row, Thick(Row,1),     
     &     (RLV_2D(Row,Col),Col = 1, NColsTOT), RLV(Row)
          Enddo 
          
!         Output DepFrac
          Write(NOUTDPF,'(/,"Year DOY:",I5,I4.3, ", DAS:",I3)') YEAR, 
     &             DOY, DAS
          Write(NOUTDPF,'("Root Depth =    ",F10.2," cm")') RTDep
          Write(NOUTDPF,'("Last Row =    ", I2)') LastRow
          Write(NOUTDPF,'("  Column ->",20I10)') (Col, Col=1, NColsTOT)
          Write(NOUTDPF,'("Width(cm)->",20F10.3)') 
     &                  (width(1,Col),Col = 1, NColsTOT)
          Write(NOUTDPF,'("      Thick")') 
          Write(NOUTDPF,'("Lyr    (cm)   ------- ",
     &      "DepFrac -------")')
          WRITE(FMT,'("(I3,F8.1,",I2,"F10.4)")') NColsTot 
          DO Row = 1, NRowsTot  
            Write(NOUTDPF,FMT)     
     &      Row, Thick(Row,1), (DepFrac(Row,Col),Col = 1, NColsTOT) 
          Enddo 
          
!         Out put WidFrac
          Write(NOUTWDF,'(/,"Year DOY:",I5,I4.3, ", DAS:",I3)') YEAR, 
     &             DOY, DAS
          Write(NOUTWDF,'("Root Width =",F10.2," cm")') RTWid
          Write(NOUTWDF,'("Last Col =    ", I2)') LastCol
!         Write(NOUTWDF,'("  Column ->",8I10, A14)') 
!     &      (Col, Col=1, NColsTOT), "   RTWidth(Row)"
          Write(NOUTWDF,'("Width(cm)->",20F10.3)') 
     &                  (width(1,Col),Col = 1, NColsTOT)
          Write(NOUTWDF,'("      Thick")') 
          Write(NOUTWDF,'("Lyr    (cm)   ------- ",
     &      "WidFrac -------")')
          WRITE(FMT,'("(I3,F8.1,",I2,"F10.4)")') (NColsTot +1) 
          DO Row = 1, NRowsTot  
            Write(NOUTWDF,FMT)  Row, Thick(Row,1),    
     &      (WidFrac(Row,Col),Col = 1, NColsTOT), RTWIDr(Row)
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
            CLOSE (NOUTRLV)
            CLOSE (NOUTDPF)
            CLOSE (NOUTWDF)
        ENDIF
!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE PT_OPRoots_2D
!=======================================================================


!-----------------------------------------------------------------------
! Variable definitions
!-----------------------------------------------------------------------
! CUMDEP       The buttom of current row
! CumWid       The width of the right side of the current column
! DepFrac      Fracton of the root in a row thickness
! DTT          Growing degree days today, degrees C 
! ESW(Row,Col) Plant extractable soil water by layer (= DUL - LL) (cm3/cm3)
! GRORT        Root growth rate, g/plant/day
! ISWNIT    Nitrogen simulation switch (Y or N) 
! ISWWAT    Water simulation control switch (Y or N) 
! LastCumdep   The top of current layer
! LastCumWif   The width of the left side of the current column
! LastRow      the deepest of the row which is occupied by the root
! NH4(L)       Ammonium N in soil layer L (µg[N] / g[soil])
! PLTPOP       Plant population (# plants / m2)
! PORMIN       Minimum pore space required for supplying oxygen to roots for 
!              optimal growth and function (cm3/cm3)
! RLDF(Row,Col)A root length density factor for soil layer L used to calculate new root growth distribution 
!              It's intermediat calculated value was in cm2, but finally - unitless
! RLDSM        Minimum root length density in a given layer, below which 
!             drought-induced senescence is not allowed.
!             (cm [root ]/ cm3 [soil])
! RLINIT       Initial root density (2D model: cm[root]/cm[row length])
!              In 1D model, it is in cm[root]/cm2[ground]
! RLNEW        New root growth added to the total root system length (For 2D: cm[root]/cm[ground]/d, for 1D sunroutine: cm[root]/cm2[ground])
! RLV(Row)     Root lenth volume for specific row in cm[root]/cm3[ground] 
! RLV(L)    Root length density for soil layer L (cm[root] / cm3[soil]) 
! RLV_2D(Row, Col) Root length density for soil cell in cm root / cm3 soil
! RLV_WS(Row,Col) Cell root density reduced by flood????
! RLWR         Root length to weight ration, (cm[root]/g[root]). Input from species file as 1E4 cm[root]/g[root] and converted after read. 
! RNFAC        Zero to unity factor describing mineral N availability effect on
!              root growth in Layer L
! RNLF         Intermediate factor used to calculate distribution of new root(1/cm2[ground]/d)
! RTDEP        Root length in col=1 at the begining of the day (cm)
! RTDEPnew     Root length in col=1 at the end of the day (cm)
! RTWID        Maximum width used for watch variable
! RTEXF        Fraction root death per day under oxygen depleted soil 
! RTSDF        Maximum fraction of root length senesced in a given layer per 
!              physiological day when water content in a given layer falls 
!              below 25 % of extractable soil water. 
! RTSURV       Fraction survival of roots on a given day, taking into account 
!              death due to excess or deficit water conditions 
! RTWIDr(Row)  Root width for each row
! RTWIDnew(row)Root width for specific row at the end of the day (calculated up to last col)
! RTWTMIN      minimum root mass per layer; used to limit senescence
!                 (g/m2) (species file parameter)
! SATFAC       Root length weighted soil water excess stress factor ( 0 = no 
!              stress; 1 = saturated stress )  It is in the writing output of PT_OPGROW, but never used in Calculation
!              SATFAC = SUMEX/SUMRL 
! SHF          Soil hospitality factor 0-1,  PT_SUBSTOR.FOR(98): SHF = SOILPROP % WR
! SWDF         Soil water deficit factor for Layer L used to calculate root
!              growth and water uptake - unitless value between 0 and 1 
! SWEXF        Excess water stress factor for layer with deepest roots (0-1) 
! SWFAC        Effect of soil-water stress on photosynthesis, 1.0=no stress,0.0=max stress 
! TRLDF        An intermediate calculation used to calculate distribution of
!              new root growth in soil (cm2)
! TRLV         Total root length per unit row length soil today (cm[root]/cm[row length])
! TRLV_MIN     conversion of RTWTMIN to RLV units per layer (cm/cm)
! WR(L)        Root hospitality factor, used to compute root distribution
!***********************************************************************
! END SUBROUTINES PT_ROOTGR_2D, PT_IPROOT_2D
!=======================================================================
