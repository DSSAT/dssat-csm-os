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
C-----------------------------------------------------------------------
C                         Adjustable Parameter
C iniRT_StartRow = 1
C RTDEPI = MAX(Thick(1,1), 2. * SDEPTH)              
C RTWIDI = max (WIDTH(iniRT_StartRow,1),  RTDEPI / 2.0)
C RTDEPnew = RTDEP + DTT * 1.0 * AMIN1((SWFAC * 2.0 ), SWDF) 
C RTWIDnew(Row) = RTWIDr(Row) + DTT * 1.0 * AMIN1((SWFAC*2.0),SWDF) 
C if (RLV(Row) .GE. 5.) RLDF(Row,Col)= 0 where 5 is adjustable
C Z = Thick(Row,1) !CUMDEP - SDEPTH ! Root can growing both up and down direction
C When emergen, tthe root grow for both up and down direction
! When DAS =41, daily rate call land is dead, infinity loop, Wbal_2D_ts is not created
! bugs   Write(NOUTRLV,'("  Column ->",11I10, A7)')  should changed if column # changed
!  Write(NOUTWDF,'("  Column ->",11I10, A14)') should changed if column # changed
! The NO3 is 1D for now
! The column # setting in cell ini, is manual changed for BMP project 
C=======================================================================

      SUBROUTINE PT_ROOTGR_2D (DYNAMIC, ISWWAT, CELLS, YRDOY,
     &    DLAYR, DS, DTT, FILEIO, GRORT, ISWNIT,     !Input
     &    NH4, NLAYR, NO3, PLTPOP, SHF, SWFAC,    !Input
     &    CUMDEP, RLV, RTDEP)                             !Output

!-----------------------------------------------------------------------
      USE Cells_2D
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT  NONE
      SAVE

      LOGICAL FIRST
      CHARACTER*1   ISWNIT, ISWWAT
      CHARACTER*30 FILEIO

      INTEGER DYNAMIC, L, J, NLAYR, YRDOY0, YRDOY, DAS
      INTEGER ROW, Col, LastCol, LastRow, iniRT_StartRow
      INTEGER CSTIMDIF      ! Time difference function
      REAL HalfRow, ROWSPC_cm, CumRootMass

      REAL CUMDEP, DEP, DEPMAX, DTT, GRORT, PLTPOP, RTSEN
      REAL RLINIT, RLNEW, RLWR, RNFAC, RNLF, RTDEP, RTDEPI, RTWIDI
      REAL SDEPTH, SWDF, SWFAC, TRLDF, TRLV
      REAL CumWid, LastCumDep, LastCumWid
      REAL PORMIN, SWEXF, RTSURV, RTEXF, RLDSM, RTSDF, RTWTMIN, TRLV_MIN
      REAL RTDEPnew, RTWID, RTLSenes, RTMasSenes, RLSENTOT
      REAL RTWIDr(MaxRows), RTWIDnew(MaxRows), WidMax(MaxRows)
      REAL WidFrac(MaxRows,MaxCols), DepFrac(MaxRows,MaxCols) 
      REAL TotRootMass, RFAC3, TotRootArea, CelRootArea(MaxRows,MaxCols)
      ! above two variables are required in Aggregate_Roots. Here do not need

      REAL, DIMENSION(NL) :: DLAYR, DS
      REAL, DIMENSION(NL) :: NO3, NH4, RLV, RLVTEMP, SHF ! SW, RLDF
      !REAL, DIMENSION(MaxRows,MaxCols) :: NO3_2D, NH4_2D
      REAL, DIMENSION(MaxRows,MaxCols) :: RLV_2D, RLDF, SAT, DUL, LL,SWV
      REAL, DIMENSION(MaxRows,MaxCols) :: Thick, Width, CellArea, ESW
      REAL, DIMENSION(MaxRows,MaxCols) :: RLV_WS 
      
      INTEGER, DIMENSION(MaxRows,MaxCols) :: TypeCell
      TYPE (CellType) CELLS(MaxRows,MaxCols)
      TYPE (CellStrucType) Struc(MaxRows,MaxCols)


!***********************************************************************
!***********************************************************************
!     Seasonal Initialization - Called once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      ! We only need RTWIDI to calculate LastCol
      YRDOY0 = YRDOY
      STRUC = CELLS%STRUC
      Thick = STRUC%THICK
      Width = STRUC%WIDTH
      CellArea = STRUC%CellArea
      TypeCell = STRUC%CellType
      DUL = CELLS%STATE%DUL
      LL  = CELLS%STATE%LL
      SAT = CELLS%STATE%SAT
      CALL PT_IPROOT_2D(FILEIO, RTEXF, RLDSM, RTSDF,        !Input
     &     RTSEN, RTWTMIN, PORMIN, RLWR, SDEPTH, ROWSPC_cm) !Output
      HalfRow = ROWSPC_cm  / 2.
      RLV_2D   = 0.0
      RLV = 0.0
      WidMax = 0.0
      DO Row = 1, NRowsTot
        Do Col = 1, NColsTot
            IF (TypeCell(Row,Col) < 3 .OR. TypeCell(Row,Col) > 5) CYCLE
            IF (TypeCell(Row,Col) .EQ. 3) 
     &          WIDMAX(Row) = BedDimension % BEDWD / 2
            IF (TypeCell(Row,Col) .EQ. 4 .OR. TypeCell(Row,Col) .EQ. 5) 
     &          WIDMAX(Row) = HalfRow
        Enddo
      Enddo

!      DO Row = 1, NRowsTot
!         if (SDEPTH .GE. DS(ROW)) continue
!         if (SDEPTH .LT. DS(ROW)) then
!           iniRT_StartRow = Row
!            ! SeedRowFrac =1.0 -  SDEPTH / Thick(ROW,1)
!           Exit
!         endif
!       Enddo
      iniRT_StartRow = 1 ! when emergy, assume that the initial root area is start from Row 1
!********* TEMPORARY CHP *********************************
!     RLWR Sensitivity
!      SELECT CASE(RUN)
!        CASE(1); RLWR = 0.50
!        CASE(2); RLWR = 0.75
!        CASE(3); RLWR = 2.5
!        CASE(4); RLWR = 5.0
!        CASE(5); RLWR = 7.5
!        CASE(6); RLWR = 10.0
!      END SELECT
!*********************************************************

      FIRST = .TRUE.

      ! DO L = 1, NL
      !   RLV_2D(L) = 0.0
      ! END DO
      RLV_2D = 0.0
      DEPMAX = DS(NRowsTot) !DEPMAX = DS(NLAYR)
      CUMDEP = 0.0
      RTDEP  = 0.0 
      CumRootMass = 0.0
      RTLSenes = 0.
   
      
    
      CALL PT_Aggregate_Roots(
     &    DLAYR, HalfRow,                               !Input
     &    NLAYR, RLV_2D, Struc,                         !Input
     &    RLV, TRLV)                                    !Output
      !LastRow = 2 ! 1st layer has no root !  LastRow = 1
      LastRow = 1 
      LastCol = 1
      CALL PT_OPRoots_2D(TotRootMass, RLWR,RLV_2D,RLV,DepFrac, WidFrac, 
     &    Thick, Width, RTDEP, RTWID, RTWIDr, CumRootMass, RTMasSenes,
     &    GRORT, DTT, SDEPTH, LastRow, LastCol)
! There is bug for small BEDWD when writting output
!       WRITE (92,1110)
! 1110 FORMAT('Row, Col,',
!     & ' RTDEP, CUMDEP, LastCumDep, RTDEPnew, Depfrac, LastRow, ',    
!     & ' RTWIDr,CumWid,LastCumWid,RTWIDnew,Widfrac,LastCol,RLDF,',
!     &   'RLV_2D in cm root / cm3 soil,RNFAC,NH4(Row), NO3(Row)')   

!***********************************************************************
!***********************************************************************
!     Daily rate calculations 
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------
      SWV = CELLS%STATE%SWV 
      TotRootArea=0.
      CelRootArea=0.
      
!     10/20/2005 Limit RLV decrease due to senscence to 
!       a minimum resulting root weight 
      IF (RTWTMIN > 0.0) THEN
!       Same units as TRLV (cm[root]/cm[row-length])
         TRLV_MIN = RTWTMIN * RLWR * HalfRow      
!        cm[root]        g    cm[root]                m2
!      -----------   =  --- * -------- * cm[width] * ----
!      cm[row length]    m2    g[root]                cm2
      ELSE
!       Set TRLV_MIN to zero -- no minimum root mass
        TRLV_MIN = 0.0
      ENDIF
      
      
!     Initial root distribution:  
      IF (FIRST) THEN
!     After planting date, call here when Root growth rate >0, this day could be before Emergence date
!********* TEMPORARY CHP *********************************
!     RLWR Sensitivity
!     Write to Overview.out file - can't do it when value is 
!       set because file is not open yet.
!      CALL GETLUN('OUTO',L)   !Get unit # for Overview.out
!      WRITE(L,*) ' Sensitivity analysis. RLWR = ',RLWR  
!*********************************************************
        ! kelly said about 7 or more days, the potato starting to have root
        RTDEPI = MAX(Thick(1,1), 2. * SDEPTH)  
        !RTDEPI = MIN(20.0,DS(NLAYR))     !CHP per JWJ   
        ! Initial root width (specify half because we are modeling half a row)             
        RTWIDI = max (WIDTH(iniRT_StartRow,1),  RTDEPI / 2.0)   
        RTWIDI = MIN(BedDimension%BEDWD / 2.0, RTWIDI)      
        ! Tomato 2D use *.spe to give RTWIDI, YRTFACH, XRTFACH. Potato 2D does not need to change *.spe
        ! ROOOTS_2D use YRTFACH, XRTFACH to calculate RFAC2H which is used to calculate RTWIDnew 
        
        FIRST  = .FALSE.
        
C-------------------------------------------------------------------------
!       CHP 5/29/03 - Added this section based on CROPGRO initialization
!           at emergence. 
C       INITIALIZE ROOT DEPTH AT EMERGENCE
C       DISTRIBUTE ROOT LENGTH EVENLY IN ALL LAYERS TO A DEPTH OF
C       RTDEPTI (ROOT DEPTH AT EMERGENCE)
C-------------------------------------------------------------------------
        Call PT_INROOT_2D(TypeCell, SDEPTH, iniRT_StartRow,
     &  DepMax, HalfRow, GRORT, RLWR, PLTPOP,              !Input
     &  RTDEPI, RTWIDI, Thick, WidMax, Width,              !Input
     &  RLV_2D, RTDEP, RTWID, RTWIDr, DepFrac, WidFrac)                      !Output

        
!***********************************************************************
      ELSE !if not first, i.e not initial
!     Daily root growth and distribution

        TRLDF  = 0.0
        CUMDEP = 0.0
        !CUMDEP = Thick(1,1) ! first layer has no root
        RNFAC  = 1.0
        RTDEPnew = RTDEP 
        ! Here, RTDEP is previous day's maximum root depth 
        ! RTDEPnmew will be Today's maximum root depth 
        RTWIDnew = RTWIDr !it is array
        
!     First, root expansion.
!     Root depth is calculated in column 1 only.
!     Root width is calculated for each row. 
        RowLoop: DO Row = 1, NRowsTot 
        
          LastCumdep = CUMDEP      
          ! now calculate DepFrac layer by layer, 
          ! LastCumdep is the top of the calculated layer, CUMDEP is buttom of the calculated layer
          CUMDEP = CUMDEP + Thick(Row,1)
          CumWid = 0.0
          ColLoop: Do Col = 1, NColsTot
            IF (TypeCell(Row,Col) < 3 .OR. TypeCell(Row,Col) > 5) CYCLE
!            IF (TypeCell(Row,Col) .EQ. 3) 
!     &          WIDMAX(Row) = BedDimension % BEDWD / 2
!            IF (TypeCell(Row,Col) .EQ. 4 .OR. TypeCell(Row,Col) .EQ. 5) 
!     &          WIDMAX(Row) = HalfRow
            !RLNew   =  GRORT * RLWR *  PLTPOP * WIDMAX(Row)
            RLNew   =  GRORT * RLWR *  PLTPOP *  HalfRow ! This statement could be before do loop
            LastCumWid = CumWid
            CumWid = CumWid + Width(Row,Col)
            
!           Starting to calculate RLDF
            SWDF = 1.0
            SWEXF = 1.0
            IF (ISWWAT .EQ. 'Y') THEN ! ISWWAT Water simulation control switch (Y or N) 
              IF (SAT(Row, Col)-SWV(Row, Col) .LT. PORMIN) THEN
                ! this will be used to calculate teh water table water senese
                SWEXF = (SAT(Row, Col) - SWV(Row, Col)) / PORMIN
                SWEXF = MIN(SWEXF, 1.0)
              ENDIF
              ESW(ROW, Col) = DUL(ROW, Col) - LL(ROW, Col)
              SWDF   = 1.0
              IF (SWV(ROW,Col)-LL(ROW,Col) .LT. 0.25*ESW(ROW, Col)) THEN
                SWDF = 4.0*(SWV(ROW, Col)-LL(ROW, Col))/ESW(ROW, Col)
              ENDIF
              SWDF = AMAX1 (SWDF,0.0) 
            endif
            ! Water stress senescence 
            RTSURV = MIN(1.0,(1.-RTSDF*(1.-SWDF)),(1.-RTEXF*(1.-SWEXF)))
           
            IF (RLV_2D(Row,Col) > RLDSM .AND. TRLV+RLNEW > TRLV_MIN)THEN
              RLV_WS(Row,Col) = RLV_2D(Row,Col) * (1.0 - RTSURV)
            ELSE
              RLV_WS(Row,Col) = 0.0
            ENDIF     
            IF (ISWNIT .NE. 'N') THEN ! ISWNIT    Nitrogen simulation switch (Y or N) 
!             RNFAC = 1.0 - (1.17 * EXP(-0.15 * TOTIN)
!             RNFAC = 1.0 - (1.17 * EXP(-0.15 * (SNH4(L) + SNO3(L))))
              RNFAC = 1.0 - (1.17 * EXP(-0.15 * (NH4(ROW) + NO3(ROW))))
              ! JZW NH4 and NO3 need to be 2D !!!
              RNFAC = AMAX1 (RNFAC,0.01)
            ENDIF
!-----------------------------------------------------------------------
!           Weighting factor for each cell, RLDF, based on WR (i.e. SHF), cell area
!           and water factors.  
            RLDF(Row,Col) =AMIN1(SWDF,RNFAC)*SHF(Row)*CellArea(Row,Col) 
!           End of calculation RLDF

          
!           Calculate new vertical growth in column 1 only
            IF (COL == 1) THEN
!             Starting to calculate DepFrac
              IF (RTDEP >= CUMDEP) THEN ! rootdepth is deeper than current row
                DepFrac(Row,Col) = 1.0
              ELSEIF (RTDEP >= LastCumDep)THEN
!               Roots have partially filled the depth of this cell
                IF (CELLS(Row,Col)%STATE%WR > 0. .AND. RLNEW >0.) THEN
                  ! The following 1.3 is an assumed parameter to affect the root depth grow, should goes to *.spe file
                  !RTDEPnew = RTDEP + DTT * 1.3 *
                  RTDEPnew = RTDEP + DTT * 1.0 *
     &                       AMIN1((SWFAC * 2.0 ), SWDF)
                  RTDEPnew = MIN(RTDEPnew, DEPMAX)
                ENDIF
                DepFrac(Row,Col) = MIN(1.0, 1. - (CUMDEP - RTDEPnew)/
     &                         Thick(Row,Col))
                ! if the new root is more than one row, take minum
                ! In the following statement, lastRow is Yesterday's Last Row of root
                IF (Row > LastRow) LastRow = Row
                ! JZW: we'd better to add exit statement
              ELSE
!               No roots in this cell 
               !JZW this is equivalent exit the do loop of row
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
!            finish calculate DepFrac
              
            Else ! if col!=1, calculate WidFrac
  !           Calculate new horizontal growth in this cell (RTWIDnew) 
!             horizontal portion of cell occupied by roots (WidFrac)
!           Horizontal root growth only occurs when DepFrac of adjacent 
!             cell is > 0.99 (JZW: This 0.99 did not realized in the codes).  No need to calculate for Column 1, since
! bug statement 0.99 is missing
!             width fraction is initialized to 1.0 there.
              IF (RTWIDr(Row) >= CumWid) THEN
                WidFrac(Row,Col) = 1.0
                DepFrac(Row,Col) = min(1.0, DepFrac(Row, col-1)) ! JZW change May 9,2012 
              ELSEIF (RTWIDr(Row) >= LastCumWid) THEN
!             Roots have partially filled the width of this cell
                IF (CELLS(Row,Col)%STATE%WR > 0.0 .AND. RLNEW >0.0) THEN
                  ! The following 0.6 is an assumed parameter to affect the root width grow, should goes to *.spe file
                  !RTWIDnew(Row) = RTWIDr(Row) + DTT * 0.6 *
                  RTWIDnew(Row) = RTWIDr(Row) + DTT * 1.0 *
     &                          AMIN1((SWFAC*2.0),SWDF) 
                  RTWIDnew(Row) = MIN(RTWIDnew(Row), WIDMAX(Row))
                Else
                  DepFrac(Row,Col) = 0.0 !JZW need to check if it is correct here
                ENDIF
                WidFrac(Row,Col) = MIN(1.0, 1. - (CumWid -RTWIDnew(Row))
     &                        / Width(Row,Col))    
                IF (Col > LastCol) LastCol = Col
              ELSE
!               No roots in this cell
                  WidFrac(Row,Col) = 0.0
                  DepFrac(Row,Col) = 0.0
              ENDIF ! end of partial filly filled this col
             
    !         Check for new roots in this cell
              IF (RTWIDnew(Row) > LastCumWid .AND. 
     &              RTWIDr(Row) <= LastCumWid) THEN
!             New roots have just grown into this cell
                WidFrac(Row,Col) = MIN(1.0, 1. -(CumWid-RTWIDnew(Row))
     &                        / Width(Row,Col))
      
                DepFrac(Row,Col) = min(1.0, DepFrac(Row, col-1)) ! JZW change May 9,2012 
                IF (Col > LastCol) LastCol = Col 
              ENDIF ! end if new grow in this cell
              
            ENDIF !! end of  col!=1  
            
            ! Re calculate the DepFrac from Row=1 to SeedRow
            IF (Row < iniRT_StartRow) Then ! current row is above initial root start row
              DepFrac(Row, Col) =0. 
!            Elseif (Row == iniRT_StartRow) then !current row is in initial root start row
!              IF (CELLS(Row,Col)%STATE%WR > 0. .AND. RLNEW >0.) THEN
!                !RTDEPnew = RTDEP + DTT * 1.3 *
!                RTDEPnew = RTDEP + DTT * 1.0 *
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
!         Apply factor for this cell
            RLDF(Row,Col) = 
     &               RLDF(Row,Col)*DepFrac(Row,Col)*WidFrac(Row,Col)
            CelRootArea(Row,Col) =CellArea(Row,Col)
     &               *DepFrac(Row,Col)*WidFrac(Row,Col)
!         Sum of all factors
           ! if (RLV(Row) .GE. 5.) RLDF(Row,Col)= 0. JZW test May 9, 2012
            TRLDF = TRLDF + RLDF(Row,Col)
            TotRootArea = TotRootArea +  CelRootArea(Row,Col)
!            WRITE (92,1120)Row, Col,
!     &        RTDEP, CUMDEP, LastCumDep, 
!     &        RTDEPnew, Depfrac(Row,Col), LastRow,  
!     &        RTWIDr(Row),CumWid, LastCumWid,RTWIDnew(Row),
!     &        Widfrac(Row,Col),LastCol,RLDF(Row,Col), RLV_2D(Row,Col),
!     &        RNFAC, NH4(Row), NO3(ROW)
          
 1120 FORMAT(2(I4,","),5(F6.2,","),I2,",",
     &    5(F6.2,","),I2,",",F6.2,4(",",F6.2))   
           
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
        
        
        !IF (TRLDF .GE. RLNEW*0.00001) THEN ! JZW ask CHP: different unit, how to compare????
         ! RLNEW and RLINIT IS in cm[root]/cm[ground]/d, TRLDF is in cm2 TRLDF has same unit as RLDF for now
           RLDF = RLDF /TRLDF ! RLDF is unitless now
           
           ! RLDF(Row,Col) is AMIN1(SWDF,RNFAC)*SHF(Row)*CelRootArea(Row,Col)/TotRootArea Same cellRootArea may have different dense 
           !DO L = 1, L1
           DO Row = 1, LastRow !JZW LastRow is the last row of root
             DO Col = 1, LastCol 
               IF (TypeCell(Row,Col)<3 .OR. TypeCell(Row,Col) > 5) CYCLE
               RTLSenes = 
     &              RTLSenes + 0.005*RLV_2D(Row,Col) * CellArea(Row,Col)
             ! To calculate LastCol need RTWIDr(Row), LastCumWid, RTWIDI
               RLV_2D(Row,Col) = RLV_2D(Row,Col)
     &             +RLDF(Row,Col) * RLNEW /CellArea(Row,Col)
   ! &             +RLDF(Row,Col)*RNLF/CellArea(Row,Col)
     &             -0.005*RLV_2D(Row,Col)  
             !  cm         cm     1
             ! -------  = ---- * ----
             !  cm3        cm    cm2
             ! Root senescence may make RLV_2D<0
               RLV_2D(Row,Col) = AMAX1 (RLV_2D(Row,Col),0.0)
               !RLV_2D(Row,Col) = AMIN1 (RLV_2D(Row,Col),5.0)
               ! Make RLV limitation instead of RLV_2D
             END DO
           ENDDO
        !END IF
      ENDIF ! end of IF not (FIRST)
      TRLV = 0.0
      DO Row = 1, LastRow
        Do Col = 1, LastCol
          IF (TypeCell(Row,Col) < 3 .OR. TypeCell(Row,Col) > 5) CYCLE
            TRLV = TRLV + RLV_2D(Row,Col) * CellArea(Row,Col) 
           ! cm     cm      cm
           !-----= ---- + ------- * cm2
           ! cm     cm      cm3
            ! JZW, TRLV is calculated in PT_Aggregate_Roots, we do not need to calculate here
        End do
      
        IF (RTWIDr(Row) > RTWID) RTWID = RTWIDr(Row) 
        ! RTWID is not used, it can be as output of this subroutine for watch variable
      ENDDO
      !Write(92,1125) TRLV
 1125 Format("TRLV=", F8.1)
      CALL PT_Aggregate_Roots(
     &    DLAYR, HalfRow,                              !Input
     &    NLAYR, RLV_2D, Struc,                        !Input
     &    RLV, TRLV)                                   !Output
      ! For 1D DAS=1, RLINIT=0.245, RLV(L=1 to 4)=0.245/DLAYER=0.049, RLINT in cm/cm2, 
      ! Roots.for indicate RLV is in cm/cm3, OpGrow indicate !RLV is in cm/cm3, PlantGro.out indicate RLV is cm3/cm3, Roots_2D.for indicate RLV_2D is in cm/cm3
      ! For 2D DAS=1, RLINIT = 0.245 * HALFRow= 11, RLV_2D(Row=1 to 4,Col=1) = 11*25/100/25=0.1102, RLV=0.0183 
     
       DAS = MAX(0,CSTIMDIF(YRDOY0,YRDOY))
        if (DAS .eq. 39) then
         continue
        endif
       !write(92,1130) YRDOY, DAS, RTDEP, TRLV
 1130  Format("YRDAY=", I8, ",DAS=", I4,", RTDEP=",F6.2, ",TRLV=",F8.1) 
 
! !     Limited RLV(Row) to 5.  The roots to be cut off for above 5.0 should be put in some other cells. Too difficult to  do
!       DO Row = 1, LastRow 
!         !RLV(Row) = AMAX1 (RLV(Row),0.0)
!         If (RLV(Row) .GT. 5.) then
!           Do Col = 1, LastCol
!             IF (TypeCell(Row,Col) < 3 .OR. TypeCell(Row,Col) > 5) CYCLE
!             RLV_2D (Row, Col) = RLV_2D(Row, Col) * (5./RLV(Row)) 
!           Enddo
!           RLV(Row) = 5.0  ! AMIN1 (RLV(Row),5.0)         
!           CALL PT_Aggregate_Roots(
!     &          DLAYR, HalfRow,                              !Input
!     &          NLAYR, RLV_2D, Struc,                        !Input
!     &          RLVTemp, TRLV) 
!           if ( ( (RLVTemp(Row)-RLV(Row)) . GT. 0001) .or.
!     &           (RLV(ROW) .LT. 0)) then
!             Write(*,*) "RLV Calculation is worong"
!             stop
!           endif
!           
!         Endif
!          write(92,1140)Row, RTWIDr(ROW), RLV(ROW)
!       Enddo
 1140  Format("Row=", I2, ", RIWIDr=", F6.2, ",  RLV=", F8.4, 
     &  "cm3[root]/cm3[ground)")
       
        CELLS%STATE%RLV = RLV_2D       
       
       DO Row = 1, LastRow 
         RLV(Row) = AMAX1 (RLV(Row),0.0)
        
          
       Enddo
 
   
! RLWR  Root length to weight ration, (cm/g)*1E-4 
        RTMasSenes = (RTLSenes /HalfRow/ RLWR) * 10.
        TotRootMass=(TRLV /HalfRow/ RLWR) * 10.
!                   cm    1    g * 1E-4   10000 cm2   10(kg/ha)
!          kg/ha  = ---*---- * ------- * -------- * ---------
!                   cm    cm    cm          m2         (g/m2)

        CumRootMass=CumRootMass+GRORT * PLTPOP *  10 ! 1 ha = 10000m2
       ! kg[root]       kg     g      # plants     kg/ha
       !----------- = -----+ ------ * --------*  --------
       ! ha             ha    plant      m2         g/m2
!        Write(93,931) "2D,RLNEW,",RLnew, ",cm/cm,CmRtMs,", CumRootMass,
!     &              ",kg/ha,totRtMs,",  TotRootMass, ",kg/ha"
 931    format (A9,F7.3,A14,F8.3,A15, F8.3,A6)   
!***********************************************************************
      ELSEIF (DYNAMIC == OUTPUT .OR. DYNAMIC == SEASEND) THEN
!-----------------------------------------------------------------------
!      CALL PT_OPRoots_2D(TotRootMass, RLWR,RLV_2D,RLV,DepFrac, WidFrac, 
!     &  Thick, Width, RTDEP, RTWID, RTWIDr, CumRootMass, RTMasSenes,
!     &  GRORT, DTT, SDEPTH, LastRow, LastCol)
! There is bug for small BEDWD in output writing
      
     
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

C-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE PT_IPROOT_2D
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
      SUBROUTINE PT_INROOT_2D(TypeCell, SDEPTH, iniRT_StartRow,
     &  DepMax, HalfRow, GRORT, RLWR, PLTPOP,              !Input
     &  RTDEPI, RTWIDI, Thick, WidMax, Width,              !Input
     &  RLV_2D, RTDEP, RTWID, RTWIDr, DepFrac, WidFrac)    !Output

!     ------------------------------------------------------------------
      USE Cells_2D
      IMPLICIT NONE

      INTEGER Row, Col, iniRT_StartRow
      INTEGER, DIMENSION(MaxRows,MaxCols) :: TypeCell
      REAL DepMax, RLINIT, WidMax(MaxRows)
      REAL HalfRow, X, Z, GRORT, PLTPOP, RLWR, SDEPTH
      REAL RTDEPI, RTDEP, LastCumDep, CumDep
      REAL RTWIDI, RTWID, LastCumWid, CumWid, RTWIDr(MaxRows)
      REAL TotRootArea
      REAL, DIMENSION(MaxRows,MaxCols) :: Thick, Width, CellArea
      REAL, DIMENSION(MaxRows,MaxCols) :: RLV_2D, RootArea
      REAL WidFrac(MaxRows,MaxCols), DepFrac(MaxRows,MaxCols) 
      TYPE (CellType) CELLS(MaxRows,MaxCols)
!-----------------------------------------------------------------------
      RTDEPI = MAX(MIN(RTDEPI, DepMax), Thick(1,1))
      RTWIDI = MAX(MIN(RTWIDI, WidMax(1)), Width(1,1))
      RLV_2D = 0.0
      RootArea = 0.  !cell area containing roots
      TotRootArea = 0.0
      RTDEP = RTDEPI
      RTWID = RTWIDI
      RTWIDr = 0.0
      X = 0.0
      Z = 0.0

!     Distribute root length and width evenly thru cells
      CUMDEP = 0.
      !CUMDEP =Thick(1,1) ! First layer has no root
      RowLoop: DO Row = 1, NRowsTot 
      !RowLoop: DO Row = 2, NRowsTot ! First layer has no root
        LastCumDep = CUMDEP
        CUMDEP = CUMDEP + Thick(Row,1)
        IF (RTDEPI >= CUMDEP) THEN ! RootDepth is deeper then current row
          If (Row .LT. iniRT_StartRow) Then
            Z = 0.
          elseif (Row .EQ. iniRT_StartRow) Then
            Z = Thick(Row,1) !CUMDEP - SDEPTH ! Root can growing both up and down direction
          else
            Z = Thick(Row,1)
          endif
          ! Z is the vertical portion of cell which is occupied by root (before add today's grow) 
        ELSEIF (RTDEPI > LastCumDep) THEN ! Root Depth is in current row
          If (Row .LT. iniRT_StartRow) Then
            Z = 0.
          elseif (Row .EQ. iniRT_StartRow) Then
            Z = RTDEPI - SDEPTH
          else
            Z = RTDEPI - LastCumDep
          endif 
        ELSE
          Z = 0.0
          EXIT RowLoop
        ENDIF
        
        IF (Row == iniRT_StartRow .OR. Z > 0.98 * Thick(Row,1)) THEN 
          RTWIDr(Row) = RTWIDI  ! JZW has question, initial is only for teh 1st column?????
        ELSEIF (Z > 0.0) THEN
          RTWIDr(Row) = WIDTH(Row,1) ! JZW has question: this is contradict with X calculation??
        ENDIF
        
        CumWid = 0.
        ColLoop: DO Col = 1,NColsTot
          IF (TypeCell(Row,Col) < 3 .OR. TypeCell(Row,Col) > 5) CYCLE
          LastCumWid = CumWid
          CumWid = CumWid + Width(Row,Col)
          CellArea(Row,Col) = Width(Row,Col) * Thick(Row,Col)
          IF (RTWIDI >= CumWid) THEN
            X = Width(Row,Col)
            ! Z is the horizental portion of cell which is occupied by root 
          ELSEIF (RTWIDI > LastCumWid) THEN
            X = RTWIDI - LastCumWid
          ELSE
            X = 0.0
            EXIT ColLoop
          ENDIF
          If (Row .LT. iniRT_StartRow) X = 0.
          DepFrac(Row,Col) = MIN(1.0, Z/Thick(Row,Col))
          WidFrac(Row,Col) = MIN(1.0, X/WIDTH(Row,Col))
          !IF (ROW == 2 .OR. COL == 1 .OR. Z > 0.98 * Thick(Row,Col))THEN ! first layer has no root
          IF (ROW == 1 .OR. COL == 1 .OR. Z > 0.98 * Thick(Row,Col))THEN
            RootArea(Row,Col) = X * Z
          ENDIF
          TotRootArea = TotRootArea + RootArea(Row,Col)
        ENDDO ColLoop
      ENDDO RowLoop
!     Calculate root senescence due to water table
     
      !DO Row = 2, NRowsTot ! 1st layer has no root
      DO Row = 1, NRowsTot 
        DO Col = 1, NColsTot
           ! in 1D subroutine, RLINIT is in cm[root]/cm2[ground]
          ! in 2D subroutine, RLINIT is in cm[root]/cm[row length] 
          !       RLINIT = WTNEW * FRRT * PLTPOP * RFAC1 * DEP / ( RTDEP *
!   !    &     10000 )  !JZW ask Cheryl, WTNEW is un-known for potato
            ! In PT_RootGr, RLINIT is wrong. It is one days's root data not 18 days
          !RLINIT   =   GRORT    *  RLWR    *  PLTPOP /10000
          ! Debug PLTPOP=5.1, RLWR = 2.5
          !RLINIT   =  GRORT * RLWR *  PLTPOP * HalfRow/10000 
  !        IF (TypeCell(Row,Col) .eq. 3 ) then
  !          RLINIT   =  GRORT * RLWR *  PLTPOP * BedDimension % BEDWD/2
  !        elseif ( (TypeCell(Row,Col) .eq. 4) .OR. 
  !   &       (TypeCell(Row,Col) .eq. 5) ) then 
            RLINIT   =  GRORT * RLWR *  PLTPOP * HalfRow 
          !  cm[root]       g       cm   # plants           m2
          !  ----------- = ------ * ---- * --------* cm * --------
          ! cm[RowLength]   plant    g       m2            10000cm2
          ! JZW GROUT is previous days data
  !        endif
          IF (RootArea(Row,Col) > 1.E-6) THEN
          ! RLINIT    Initial root density (cm[root]/cm[row length]) ! JZW question why do the following report to Cheryl??
            RLV_2D(Row,Col) = RLINIT  * RootArea(Row,Col) / TotRootArea
            RLV_2D(Row,Col) = RLV_2D(Row,Col) / CellArea(Row,Col)
!            cm[root]         cm[root]      1  
!           ----------- = -------------- * ----
!            cm3[soil]    cm[row length]   cm2 
          ENDIF
        ENDDO
      ENDDO
      ! JZW question: RLINIT is only for today's root grow. Before today, already have RLV_2D???
      !JZW question: we should calculate RTDEPnew here. RTDEP should be large than RTDEPI? 
      !Write(93,*) "2D,RLINIT,",RLINIT,"cm/cm"
!***********************************************************************
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
      CHARACTER*7 FileName
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
          Write(NOUTRLV,'("Root L:M ratio =",F10.2," cm/g")') RLWR*1.E4
          Write(NOUTRLV,'("Grow Rate (GRORT) :",F10.2,"g/plant; 
     &     Growing degree days (DTT)", F6.2)') GRORT, DTT
!          Write(NOUTRLV,'("  Column ->",8I10, A7)') !JZW ask CHP how??the 10I10 may need to be change if the NColsTot changed 
!     &         (Col, Col=1, NColsTOT), "    RLV"
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
          
          ! Output DepFrac
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
          
          ! Out put WidFrac
           Write(NOUTWDF,'(/,"Year DOY:",I5,I4.3, ", DAS:",I3)') YEAR, 
     &             DOY, DAS
          Write(NOUTWDF,'("Root Width =",F10.2," cm")') RTWid
          Write(NOUTWDF,'("Last Col =    ", I2)') LastCol
!          Write(NOUTWDF,'("  Column ->",8I10, A14)') 
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
      SUBROUTINE PT_Aggregate_Roots(
     &    DLAYR, HalfRow,                                 !Input
     &    NLAYR, RLV_2D, Struc,                           !Input
     &    RLV, TRLV)                                      !Output

      Use Cells_2D
      IMPLICIT NONE
      SAVE

      INTEGER Row, Col, L, NLAYR
      REAL HalfRow, TRLV
      REAL, DIMENSION(NL) :: DLAYR, RLV
      REAL, DIMENSION(MaxRows,MaxCols) :: RLV_2D, Width, Thick, RtLen
      TYPE (CellStrucType) Struc(MaxRows,MaxCols)
      INTEGER, DIMENSION(MaxRows,MaxCols) :: TypeCell

      Width = Struc%Width
      Thick = Struc%Thick
      TypeCell = STRUC%CellType
      
      TRLV = 0.0
      DO Row = 1, NRowsTot
        DO Col = 1, NColsTot
          !RLV_2D is zero for the TypeCell<3 and TypeCell>5
          RtLen(Row,Col) =RLV_2D(Row,Col)*THICK(Row,Col)*Width(Row,Col)
!             cm[root]         cm[root]
!          -------------- =   ----------- * cm[cell depth] * cm[cell width]
!          cm[row length]     cm3[ground]

          TRLV = TRLV + RtLen(Row,Col)
        ENDDO
      ENDDO

!     Aggregate cells across a row to get layer total.  Units for layers
!     are in cm[root]/cm[row length]
      CALL Cell2Layer_2D(
     &   RtLen, Struc, NLAYR,                 !Input
     &   RLV)                                  !Output
      ! JZW for das=1, RLV_2D(row=1 to 4, Col =1) = 0.1102, RtLen(1,1) = 2.75, TRLV = 11.02, RLV(Row=1 to 4)= 2.75, after the following do loop, RLV(Row=1 to 4) = 2.75/5/30=0.0183
      DO L = 1, NLAYR
         IF (TypeCell(L,1) .EQ. 3) then 
           RLV(L) = RLV(L) / DLAYR(L)/( BedDimension % BEDWD / 2) ! JZW: Cheryl does not want this statement , Check????
         else
           RLV(L) = RLV(L) / HalfRow / DLAYR(L)
         endif
!    cm[root]       cm[root]            1               1
!  ----------- = -------------- * ------------- * -----------------
!  cm3[ground]   cm[row length]   cm[row width]   cm[row thickness]
      ENDDO

      RETURN
      END Subroutine PT_Aggregate_Roots
!==============================================================================
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
! RLWR         Root length to weight ration, (cm/g)*1E-4 
! RNFAC        Zero to unity factor describing mineral N availability effect on
!              root growth in Layer L
! RNLF         Intermediate factor used to calculate distribution of new root(1/cm2[ground]/d)
! RTDEP        Root length in col=1 at the begining of the day
! RTDEPnew     Root length in col=1 at the end of the day
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
! RLNew should divided 10000?????
! Initial RLV_2D should not divided total root area
!RNLF = RLNEW/TRLDF
!TRLDF = TRLDF + RLDF(Row,Col)
!
!WCR is Residual water content
!WR is Root hospitality factor
! PT_SUBSTOR: SHF    = SOILPROP % WR
! PT_ROOTGR.for: RLDF(L) = AMIN1(SWDF,RNFAC)*SHF(L)*DLAYR(L) unit is cm, wrong??
! RLINIT calculation sholud not divided the total area

! ROOTS.for:    RLINIT    Initial root density (cm[root]/cm2[ground])
! ROOTS_2D.for: RLINIT    Initial root density (cm[root]/cm[row length])
! ROOTS_2D.for: RLNEW     New root growth added (cm[root]/cm[row length]/d)
! ROOTS.for:    RLV       cm[root]/cm3[ground]
! ROOTS_2D.for: RLV_2D    cm[root] / cm3[soil] unit compare with paper??


! Root initial
!1D Model:  RLV(L) = RLINIT / DLAYR(L) = GRORT * RLWR * PLTPOP/DLAYR(L) = 0.019 *2.5*5.1=0.2437/5=0.0488
! Above is wrong, it should be devided the Root depth instead of DLAYR !!!!!!!!!!!!!!!!!! Line 138 and line 184 in PT_ROOTGR are wrong
!2D Model: RLINIT = GRORT * RLWR * PLTPOP * BDWD/2 = 0.019 * 2.5*5.1 * 30 = 7.313
!           RLV_2D(Row=2,Col=1) = RLINIT  * RootArea(Row,Col) / TotRootArea = 7.313*25/100 =1.828
!           RLV_2D(Row,Col) = RLV_2D(Row,Col) / CellArea(Row,Col)=1.828/25 = 0.073
!           RLV = 0.01218

! In PT_GROSUB,  GRORT   = (GROLF + GROSTM)* RTPAR
!                GROLF  = PLAG/LALWR; GROSTM = GROLF*0.75; RTPAR = 0.5 - 0.5*(XSTAGE - 1.0)
!                RLGR   = 0.50*DTT             ! From Ingram & McCloud (1984)
!          PLAG   = EXP(RLGR)*PLA - PLA
!          PLAG   = PLAG*AMIN1 (TURFAC, AGEFAC, 1.0)
!          PLA     = 25.0        !cm2/plant 
!           GRORT g/plant
!          DATA  LALWR /270./      !leaf area:leaf wt. ratio (cm2/g) 
! PLWR is from *.spe file in cm/g?

!////////////
!Bugs: day 67, RLV_2D has no change, but sensence, Cumulated Root increased
! When all of RLV > 5, there is no balanced
! Paper seed place: 12-15cm? How first layer has root?
!
!       IF (ISWWAT .EQ. 'Y') THEN
!          IF (SAT(L)-SW(L) .LT. PORMIN) THEN
!            SWEXF = (SAT(L) - SW(L)) / PORMIN
!            SWEXF = MIN(SWEXF, 1.0)
!          ENDIF
!
!          SUMEX = SUMEX + DLAYR(L) * RLV(L) * (1.0 - SWEXF)
!          SUMRL = SUMRL + DLAYR(L) * RLV(L)
!
!          ESW(L) = DUL(L) - LL(L)
!          IF (SW(L) - LL(L) .LT. 0.25*ESW(L)) THEN
!            SWDF = (SW(L) - LL(L)) / (0.25*ESW(L))
!            SWDF = MAX(SWDF, 0.0)
!          ENDIF
!        ENDIF
!C-----------------------------------------------------------------------
!
!        RTSURV = MIN(1.0,(1.-RTSDF*(1.-SWDF)),(1.-RTEXF*(1.-SWEXF)))
!        IF (RLV(L) .GT. RLDSM .AND. TRLV + RLNEW > TRLV_MIN) THEN
!!         1/14/2005 CHP Don't subtract water stress senescence 
!!           yet - combine with natural senescence and check to see if 
!!           enough RLV for senescence to occur (TRLV > TRLV_MIN)
!          !RLV(L) = RLV(L) * RTSURV
!          RLV_WS(L) = RLV(L) * (1.0 - RTSURV)
!        ELSE
!          RLV_WS(L) = 0.0
!        ENDIF