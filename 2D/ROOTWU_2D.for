C=======================================================================
C  ROOTWU_2D, Subroutine, J.T. Ritchie
C  Calculates root water uptake rate for each soil layer and total rate.
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  01/01/1989 JR  Written
C  12/05/1993 NBP Made into subroutine.
C  01/18/1996 JWJ Added flooding effect on water uptake
C  01/06/1996 GH  Added soil water excess stress
C  10/10/1997 CHP Updated for modular format.
C  09/01/1999 GH  Incorporated in CROPGRO
C  01/10/2000 NBP Added SAVE for stored variables and set SWCON2=RWUP_2D_ts=0.0
C  01/12/2000 NBP Removed FILECC from input
C  01/25/2000 NBP Added IOSTAT to READ statements to set ERRNUM.  Cleaned.
C  06/21/2001 GH  Added seasonal initialiation
C  09/17/2001 CHP Input PORMIN and RWUMX from Plant module.
!  02/26/2009 CHP Modify for 2D rootsD
!  02/27/2009 CHP Change units to mm for TRWUP_calc and RWUP_2D_ts
!  08/21/2009 CHP Change to sub-hourly, variable time step. Remove MGAR.
C                   
C-----------------------------------------------------------------------
C Called by: SPAM_2D
C Calls:     None
C=======================================================================
      SUBROUTINE ROOTWU_2D(DYNAMIC, TimeIncr,            !Input 
     &    Cells, EOP_ts, SWV_avail,                       !Input 
     &    RWU_2D_ts, RWUP_2D_ts, TRWU_ts, TRWUP_ts)       !Output

C-----------------------------------------------------------------------
      USE Cells_2D
      Use ModuleData    !temp chp
      IMPLICIT NONE
      SAVE
C-----------------------------------------------------------------------
      INTEGER DYNAMIC
      INTEGER i, j
      INTEGER, DIMENSION(MaxRows,MaxCols) :: Cell_type

      REAL EXPFAC, ROWSPC_cm, SWEXF, TimeIncr
      REAL RootLimit, Scale2Hour
      REAL SWCON1, SWCON3, PORMIN, RWUMX, WUF
      REAL, DIMENSION(MaxRows,MaxCols) :: CellArea, LL, RLV_2D
      REAL, DIMENSION(MaxRows,MaxCols) :: SAT
      REAL, DIMENSION(MaxRows,MaxCols) :: SWCON2
      REAL, DIMENSION(MaxRows,MaxCols) :: TSS, TSS_last

      Double Precision EOP_ts, TRWU_ts,  TRWUP_ts
      Double Precision, DIMENSION(MaxRows,MaxCols) :: RWU_2D_ts
      Double Precision, DIMENSION(MaxRows,MaxCols) :: RWUP_2D_ts
      Double Precision, DIMENSION(MaxRows,MaxCols) :: RWUP_vf
      Double Precision, DIMENSION(MaxRows,MaxCols) :: SWV_avail, SWV_D
      TYPE (CellType) CELLS(MaxRows,MaxCols)

      TYPE (WeatherType) WEATHER

      PARAMETER (SWCON1 = 1.32E-3)
      PARAMETER (SWCON3 = 7.01)


!!     temp chp
!      integer lun2
!      type (controltype) control
!      call get (control)

!***********************************************************************
!***********************************************************************
!     Seasonal Initialization - Called once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
!     Compute SWCON2 for each soil layer.  Adjust SWCON2 for extremely
!     high LL to avoid water uptake limitations.
!-----------------------------------------------------------------------
      LL  = CELLS%STATE%LL
      SAT = CELLS%STATE%SAT
      CellArea = CELLS%STRUC%CellArea
      Cell_TYPE = CELLS%STRUC%CellType

      TSS    = 0.0
      TSS_LAST = 0.0
      TRWU_ts   = 0.0
      TRWUP_ts  = 0.0
      SWCON2 = 0.0        

      CALL GET('PLANT', 'RWUMX',  RWUMX)
      CALL GET('PLANT', 'PORMIN', PORMIN)

      Rowspc_cm = BedDimension % RowSpc_cm
      DO i = 1, NRowsTot
        DO j = 1, NColsTot
          SWCON2(i,j) = 120. - 250. * LL(i,j)
          IF (LL(i,j) > 0.30) SWCON2(i,j) = 45.0
        ENDDO  
      ENDDO

!      CALL GETLUN('RWU_2D.CSV',LUN2)
!      OPEN (UNIT=LUN2, FILE='RWU_2D.CSV')
!      WRITE(LUN2,'(A)') "2D, variable time step root water uptake"
!      WRITE(LUN2,'(A)') "TIME(d), EOP, TRWUP, TRWU"

!***********************************************************************
!***********************************************************************
!     DAILY RATE CALCULATIONS
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
C-----------------------------------------------------------------------
      RLV_2D = Cells%State%RLV
      SWV_D = SWV_avail 
      TRWUP_ts  = 0.0
      TRWU_ts   = 0.0
      RWUP_2D_ts = 0.0
      RWU_2D_ts = 0.0

      CALL GET(WEATHER)
      Scale2Hour = 24. / WEATHER % DAYL

      IF (EOP_ts < 1.E-9) RETURN

      DO i = 1, NRowsTot
        DO j = 1, NColsTot

          SELECT CASE (Cell_type(i,j))
          CASE(3,4,5); CONTINUE
          CASE DEFAULT; CYCLE
          END SELECT

         IF (RLV_2D(i,j) < 1.E-5 .OR. SWV_D(i,j) <= LL(i,j)) THEN
            RWUP_2D_ts(i,j) = 0.
          ELSE
!           ------------------------------------------------------------
!           Soil limitation
            EXPFAC = MIN((SWCON2(i,j) * (SWV_D(i,j) - LL(i,j))), 40.)
            RWUP_2D_ts(i,j) = SWCON1 * 
     &                        EXP(EXPFAC) / (SWCON3 - ALOG(RLV_2D(i,j)))
!           RWUP_2D_ts in cm3[water]/cm[root]-d

!           ------------------------------------------------------------
!           Root limitation
!           Effects of saturated soil
            IF ((SAT(i,j) - SWV_D(i,j)) >= PORMIN) THEN
               TSS(i,j) = 0.
            ELSE
               TSS(i,j) = TSS(i,j) + TimeIncr   !minutes
            ENDIF
!           Delay of 2 days after soil layer is saturated before root
!           water uptake is affected
            IF (TSS(i,j) .GT. 2880.) THEN   !2880 minutes = 2 days
               SWEXF = (SAT(i,j) - SWV_D(i,j)) / PORMIN
               SWEXF = MAX(SWEXF,0.0)
            ELSE
               SWEXF = 1.0
            ENDIF
            SWEXF = MIN(SWEXF,1.0)

!           Root limitation should be scaled up based on max that can
!               be extracted by roots in an hour.  Daily value underestimates
!               hourly limit.  Soil water supply limitation is OK as-is.
            RootLimit = RWUMX * SWEXF * Scale2Hour

!           ------------------------------------------------------------
!           Actual potential root uptake is minimum of root limited rate
!               and soil limited rate
            RWUP_2D_ts(i,j) = MIN(RWUP_2D_ts(i,j), RootLimit)
!           RWUP_2D_ts in cm3[water]/cm[root]-d

!           Convert to volumetric fraction and limit to available water
            RWUP_vf(i,j) = RWUP_2D_ts(i,j) * RLV_2D(i,j)
!           cm3[water]     cm3[water]   cm[root] 
!           -----------  = ---------- * --------- 
!           cm3[soil]-d    cm[root]-d   cm3[soil] 
          
            RWUP_2D_ts(i,j) = RWUP_vf(i,j) *CellArea(i,j)/(ROWSPC_cm/2.)
!           cm[water]   cm3[water]       cm3[soil]     cm[row length]
!           --------- = ----------- * -------------- * --------------
!               d       cm3[soil]-d   cm[row length]      cm2[soil]
          
!           Scale to time step.  Units are mm (i.e., total for this time step)
            RWUP_2D_ts(i,j) = RWUP_2D_ts(i,j) * 10./ 24. *(TimeIncr/60.)
!                              cm[water]        mm    d
!              mm[water]    =  ---------      * -- * -- * hr
!                                  d            cm   hr
            
            TRWUP_ts = TRWUP_ts + RWUP_2D_ts(i,j)       !mm
          ENDIF
        ENDDO
      ENDDO

!-----------------------------------------------------------------------
!     Scale back root water extraction, if greater than demand 
      IF (EOP_ts .LT. TRWUP_ts .AND. TRWUP_ts > 1.E-9) THEN
        WUF = EOP_ts / TRWUP_ts
      ELSE
        WUF = 1.0
      ENDIF

      TRWU_ts = 0.0
      DO i = 1, NRowsTot
        DO j = 1, NColsTot
          RWU_2D_ts(i,j) = RWUP_2D_ts(i,j) * WUF
          SELECT CASE (Cells(i,j)%Struc%CellType)
          CASE (3,4,5)
            TRWU_ts = TRWU_ts + RWU_2D_ts(i,j)
          END SELECT
        ENDDO
      ENDDO
      
!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************

      RETURN
      END SUBROUTINE ROOTWU_2D

!-----------------------------------------------------------------------
!     ROOTWU VARIABLE DEFINITIONS:
!-----------------------------------------------------------------------
! DLAYR(i,j)  Soil thickness in layer L (cm)
! LL(i,j)     Volumetric soil water content in soil layer L at lower limit
!             (cm3/cm3)
! NL        Maximum number of soil layers = 20 
! NLAYR     Actual number of soil layers 
! PORMIN    Minimum pore space required for supplying oxygen to roots for 
!             optimal growth and function (cm3/cm3)
! RLV_2D(i,j)    Root length density for soil layer L ((cm root / cm3 soil))
! RWUP_2D_ts(i,j)    Root water uptake from soil layer L in current time step(cm/d)
! RWUMX     Maximum water uptake per unit root length, constrained by soil 
!             water (cm3[water] / cm [root])
! SAT(i,j)    Volumetric soil water content in layer L at saturation
!             (cm3 [water] / cm3 [soil])
! SWV_D(i,j)  Volumetric soil water content in layer L
!             (cm3 [water] / cm3 [soil])
! SWCON1    Constant used in determining root water uptake 
! SWCON2(i,j) Variable used in determining root water uptake, dependant on 
!             lower limit in layer L 
! SWCON3    Constant used in determining root water uptake 
! SWEXF     Excess water stress factor for layer with deepest roots (0-1) 
! TRWUP_calc     Total potential daily root water uptake (mm/d)
! TSS(i,j)    Number of days soil layer L has been saturated (d)
!-----------------------------------------------------------------------
!     END SUBROUTINE ROOTWU
!-----------------------------------------------------------------------
