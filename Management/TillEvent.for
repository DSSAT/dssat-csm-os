C=======================================================================
C  TillEvent, Subroutine, C.H. Porter
C  Update soil parameters based on tillage event.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  09/17/2001 CHP  Written
C  04/16/2002 GH   Modified for sequence analysis
C  08/01/2002 CHP  Merged RUNINIT and SEASINIT into INIT section
C  08/20/2002 GH   Modified for Y2K
C  08/12/2003 CHP  Added I/O error checking
C  03/03/2006 CHP  Added A. Andales, WDB tillage routines
C-----------------------------------------------------------------------
C  Called : MgmtOps
C  Calls  : 
C=======================================================================

      SUBROUTINE TillEvent(CONTROL, 
     &    BD, BD_BASE, CN, CN_BASE, DLAYR, DL_BASE,       !Input
     &    DS_BASE, SAT, SAT_BASE, SWCN, SC_BASE,          !Input
     &    NLAYR, TILLVALS,                                !Input
     &    BD_TILLED, CN_TILLED, DL_TILLED,                !Output
     &    DS_TILLED, SAT_TILLED, SC_TILLED)               !OutpuT

!     2023-01-26 chp removed unused variables in argument list:
!       DS

!-----------------------------------------------------------------------
      USE ModuleDefs
      IMPLICIT NONE
      EXTERNAL INFO, LMATCH
      SAVE

      CHARACTER*8, PARAMETER :: ERRKEY = 'TILEVENT'
      CHARACTER*78 MSG(10)

      INTEGER DYNAMIC, I, L, M, NS, NLAYRI
      INTEGER YRDOY, RUN, NLAYR
      INTEGER, DIMENSION(NAPPL) :: NLYRS
      INTEGER NTil_today

      REAL CN, CN_BASE, CNTEMP, CN_TILLED, XCN
      REAL DEPTH, THICK, PREV_DEPTH, CUMDEPTH    !BDPMIN, 
      REAL, DIMENSION(NL) :: BD,    BD_BASE, BDTEMP, BD_TILLED, XBD
      REAL, DIMENSION(NL) :: DLAYR, DL_BASE, DLTEMP, DL_TILLED, XDL
      REAL, DIMENSION(NL) ::        DS_BASE, DSTEMP, DS_TILLED, XDS
!     REAL, DIMENSION(NL) :: RGIMPF,RGTEMP, RG_TILLED, XRGIF, RG_BASE
      REAL, DIMENSION(NL) :: SAT,   SAT_BASE, SATTEMP,SAT_TILLED,XSAT
      REAL, DIMENSION(NL) :: SWCN,            SCTEMP, SC_TILLED, XSWCN
      REAL, DIMENSION(NL) :: SC_BASE
      REAL, DIMENSION(NAPPL) :: CNP, TDEP
      REAL, DIMENSION(NAPPL, NL) :: DEP, BDP, SWCNP
      LOGICAL TILL

!     Tolerance for layer thicknesses in tillage calculations
      REAL, PARAMETER :: TOL = 0.5

!     Define constructed variable types based on definitions in
!     ModuleDefs.for.
      TYPE (ControlType) CONTROL
      TYPE (TillType)    TILLVALS

!     !Minimum % change in BD that will result in RGIMPF of 1.0.
!     DATA BDPMIN /-30.0/
                      
      DYNAMIC = CONTROL % DYNAMIC
      RUN     = CONTROL % RUN
      YRDOY   = CONTROL % YRDOY

!-----------------------------------------------------------------------
!!  Values included in TILLVALS:
!       INTEGER NTIL      !Total number of tillage events in FILEX
!       INTEGER TILDATE   !Date of current tillage event
!
!!      Maximum values for multiple events in a single day
!       REAL TILDEP, TILMIX, TILRESINC
!
!!       Irrigation amount which affects tilled soil properties 
!!          expressed in units of equivalent rainfall depth
!       REAL TIL_IRR   
!
!!      Allows multiple tillage passes in a day
!       INTEGER NTil_today !number of tillage events today (max 3)
!       INTEGER, DIMENSION(NAPPL) :: NLYRS
!       REAL, DIMENSION(NAPPL) :: CNP, TDEP
!       REAL, DIMENSION(NAPPL,NL) :: BDP, DEP, SWCNP

!-----------------------------------------------------------------------
      NTil_today= TILLVALS % NTil_today
      NLYRS = TILLVALS % NLYRS
      CNP   = TILLVALS % CNP
      TDEP  = TILLVALS % TDEP
      BDP   = TILLVALS % BDP
      DEP   = TILLVALS % DEP
      SWCNP = TILLVALS % SWCNP
      
!     Copy today's values into array to be modified incrementally 
!     by tillage operations
!     X-values have same layering as soil profile,
!     TEMP-values may have more layers depending on tillage input
      XBD   = BD_BASE        
      XCN   = CN_BASE        
      XDL   = DL_BASE
      XDS   = DS_BASE
!     XRGIF = RGIMPF  
      XSAT  = SAT_BASE
      XSWCN = SC_BASE

      DO I = 1, NTil_today
        !Convert CN from % change to absolute units.
        IF (CNP(I) .GT. 0.0) THEN
          !--- INCREASE CN -----------------------------------
          !CHP CNTEMP = MIN(CN_BASE, XCN)
          CNTEMP = XCN * (1.0 + CNP(I)/100.)
          CNTEMP = MIN(CNTEMP, 98.0)
          !Don't allow this tillage event to decrease CN
          CNTEMP = MAX(CN, CNTEMP)
        ELSE
          !--- DECREASE CN -----------------------------------
          !CHP CNTEMP = MAX(CN_BASE, XCN)
          CNTEMP = XCN * (1.0 + CNP(I)/100.)
          !Don't allow this tillage event to increase CN
          CNTEMP = MIN(CN, CNTEMP)
        ENDIF

        !Define layers for each tillage / soil layer combination
        ! through tillage depth, TDEP.  These combined layer depths 
        ! are DSTEMP and correspond to BDTEMP & SCTEMP.
        M = 1   !Tillage layer index
        NS = 1   !Soil layer index
        TILL = .TRUE. !Within tillage depth if true
        PREV_DEPTH = 0.
        CUMDEPTH = 0.0
        DO L = 1, NL  !Combined tillage / soil layer index
          IF (NS .GT. NLAYR) EXIT  !Below soil profile depth -> quit

!         Depth and thickness of this soil/till layer before 
!           applying this tillage event
          DEPTH = XDS(NS)
          IF (TILL) THEN
            DEPTH = MIN(DEPTH, TDEP(I))
            IF (DEP(I,M) > 0.0) DEPTH = MIN(DEPTH, DEP(I,M))
          ENDIF
          THICK = DEPTH - PREV_DEPTH   
          PREV_DEPTH = DEPTH

          IF (TILL) THEN
!          Bulk density calcs depend on if this event will compact
!          or loosen soil
!        --------- REDUCE BULK DENSITY  ---------------------------
            IF (BDP(I,M) .LE. 0.0) THEN
              !Loosen soil, reduce bulk density - start with 
              ! maximum of current & initial BD value
              !CHP BDTEMP(NS) = MAX(BD_BASE(NS), XBD(NS))
              !Apply percent change
              BDTEMP(L) = XBD(NS) * (1.0 + BDP(I,M)/100.)
              !Don't allow this tillage event to increase BD
              BDTEMP(L) = MIN(BD(NS), BDTEMP(L))

!              !----- INCREASE ROOT GROWTH IMPEDANCE FACTOR  -------
!              !Calculate effects on root growth impedance factor - 
!              !  based on % change to BD.
!              !BDPMIN is minimum % change in BD that will result in 
!              !  RGIMPF = 1.0 (maximum loosening of soil).  BDP values
!              !  less than this (more negative) will result in 
!              !  RGIMPF = 1.0.  BDP values higher than this (less 
!              !  negative) up to zero will result in RGIMPF between 
!              !  1.0 and RG_BASE. For BDP > 0 (compaction) go back
!              !  to original RGIMPF values.
!              IF (BDP(I,M) .LE. BDPMIN) THEN
!                !Maximum loosening of soil
!                RGTEMP(L) = 1.0
!              ELSE
!                !CHP RGTEMP(L) = MIN(RG_BASE(NS), XRGIF(NS))
!                RGTEMP(L) = RG_BASE(NS) + 
!     &                       (1.0 - RG_BASE(NS)) * BDP(I,M) / BDPMIN
!                !Don't reduce existing RGIMPF - only increase
!                !CHP RGTEMP(L) = MAX(XRGIF(NS), RGTEMP(L))
!              ENDIF

              !----- INCREASE SATURATED WATER CONTENT  ------------
              SATTEMP(L) = 0.95 * (1.0 - BDTEMP(L) / 2.66)
              !Don't reduce existing SAT value
              SATTEMP(L) = MAX(SAT(NS), SATTEMP(L))

        !--------- INCREASE BULK DENSITY --------------------------
            ELSE
              !Compaction of soil, increase BD - start with minimum 
              ! of current & initial BD value
              !CHP BDTEMP(NS) = MIN(BD_BASE(NS), XBD(NS))
              !Apply percent change
              BDTEMP(L) = XBD(NS) * (1.0 + BDP(I,M)/100.)
              !Don't allow this tillage event to decrease BD
              BDTEMP(L) = MAX(BD(NS), BDTEMP(L))

!              !----- RESET ROOT GROWTH IMPEDANCE FACTOR  ----------
!              !Soil compaction, go back to original RGIMPF values
!              RGTEMP(L) = RG_BASE(NS)

              !----- DECREASE SATURATED WATER CONTENT  ------------
              SATTEMP(L) = 0.95 * (1.0 - BDTEMP(L) / 2.66)
              !Don't increase existing SAT value with compaction
              SATTEMP(L) = MIN(SAT(NS), SATTEMP(L))
             
            ENDIF

            !Hydraulic conductivity calcs depend on if this event will
            !  compact or loosen soil
            !Check for -99 values of SWCN
            IF (SC_BASE(L) .GT. 0.0) THEN
              IF (SWCNP(I,M) .GT. 0.0) THEN
              !----- INCREASE HYDRAULIC CONDUCTIVITY --------------
                !Loosen soil, increase hydraulic conductivity, start
                !  with minimum of current and initial SWCN values
                !CHP SCTEMP(L) = MIN(SC_BASE(NS), XSWCN(NS)) 
                !Apply percent change
                SCTEMP(L) = XSWCN(NS) * (1.0 + SWCNP(I,M)/100.)
                !Don't allow this tillage event to decrease SWCN
                SCTEMP(L) = MAX(SWCN(NS), SCTEMP(L))
              ELSE
              !----- DECREASE HYDRAULIC CONDUCTIVITY --------------
                !Compaction of soil, decrease hydraulic conductivity,
                !  Start with maximum of current and initial SWCN
                !CHP SCTEMP(L) = MAX(SC_BASE(NS), XSWCN(NS)) 
                !Apply percent change
                SCTEMP(L) = XSWCN(NS) * (1.0 + SWCNP(I,M)/100.)
                !Don't allow this tillage event to increase SWCN
                SCTEMP(L) = MIN(SWCN(NS), SCTEMP(L))
              ENDIF
            ELSE    !For -99 value, do nothing
              SCTEMP(L) = SC_BASE(NS)
            ENDIF

            DLTEMP(L) = THICK * XBD(NS) / BDTEMP(L)
            CUMDEPTH = CUMDEPTH + DLTEMP(L)
            DSTEMP(L) = CUMDEPTH

            IF (ABS(XDS(NS)  - DEPTH) .LT. TOL) THEN
              XDS(NS) = DSTEMP(L)
              IF (NS == 1) THEN
                XDL(NS) = XDS(NS)
              ELSE
                XDL(NS) = XDS(NS) - XDS(NS-1)
              ENDIF
              NS = NS + 1 !soil
            ENDIF
            IF (ABS(DEP(I,M) - DEPTH) .LT. TOL) M = M + 1   !till
            IF (ABS(TDEP(I)  - DEPTH) .LT. TOL) THEN
              TILL = .FALSE.
!             Modify tillage depth to match post-tilled depth so that mixing
!               will occur to the correct depth
              TDEP(I) = DEPTH
            ENDIF

          ELSE  !.NOT. TILL
            !Below tillage depth set after-tillage values to 
            ! current soil layer values
            BDTEMP(L)  = BD(NS)
            DLTEMP(L)  = THICK
            CUMDEPTH = CUMDEPTH + THICK
            DSTEMP(L)  = CUMDEPTH
            XDS(NS) = CUMDEPTH
            IF (NS == 1) THEN
              XDL(NS) = XDS(NS)
            ELSE
              XDL(NS) = XDS(NS) - XDS(NS-1)
            ENDIF
            SATTEMP(L) = SAT(NS)
            SCTEMP(L)  = SWCN(NS)
!           RGTEMP(L)  = RGIF(NS)
            NS = NS + 1
          ENDIF

        ENDDO

        NLAYRI = L - 1
        !Convert from combined soil/tillage layers to soil layers
        CALL LMATCH(NLAYRI, DSTEMP, BDTEMP, NLAYR, XDS)
!       CALL LMATCH(NLAYRI, DSTEMP, RGTEMP, NLAYR, XDS)
        CALL LMATCH(NLAYRI, DSTEMP, SATTEMP,NLAYR, XDS)
        CALL LMATCH(NLAYRI, DSTEMP, SCTEMP, NLAYR, XDS)

        !Copy modified values to array to be used if another tillage
        ! event occurs today
        XBD   = BDTEMP
        XCN   = CNTEMP
!       XRGIF = RGTEMP
        XSAT  = SATTEMP
        XSWCN = SCTEMP

        MSG(1) = "             -- Lyr Thick (cm)--  --Blk dens (g/cm3)"
     &    // "- ---- SOM-BD --------"
        MSG(2) = "               DL1    DL2    DL3    BD1    BD2    BD3"
     &    // "   BDS1   BDS2   BDS3"
        WRITE(MSG(3),'(A11,9F7.3)')"Before till",
     &      DLAYR(1), DLAYR(2), DLAYR(3), BD(1), BD(2), BD(3), 
     &                     BD_BASE(1), BD_BASE(2), BD_BASE(3)
        WRITE(MSG(4),'(A11,6F7.3)')"After till ",
     &      XDL(1),XDL(2),XDL(3),XBD(1),XBD(2),XBD(3)
        CALL INFO(4,ERRKEY,MSG)

      ENDDO   !Loop thru tillage events today

      BD_TILLED = XBD
      CN_TILLED = XCN  
      DL_TILLED = XDL
      DS_TILLED = XDS
!     RG_TILLED = XRGIF
      SAT_TILLED= XSAT 
      SC_TILLED = XSWCN

!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE TillEvent

C=======================================================================
