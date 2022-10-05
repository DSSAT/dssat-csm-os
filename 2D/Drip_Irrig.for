!=======================================================================
!  Drip_IRRIG, Subroutine
!
!  Determines drip irrigation - works with MGAR water balance routine
!-----------------------------------------------------------------------
!  Revision history
!
!  09/04/2008 CHP Written
!  07/27/2010 CHP Drip irrigation emitter can be offset from centerline.
!  03/30/2011 CHP Add Automatic Management for IIRRI
!=======================================================================

      SUBROUTINE Drip_IRRIG (CONTROL,  
     &    AUTO, IRRAPL, DripDat, DripEvntEntr,    !Input
     &    DripDur, DripInt, DripNum, DripRefLN,   !Input
     &    DripLN, DripOfset, DripDep, DripRate,   !Input
     &    DripSpc, DripStart, EFFIRR, IIRRI,      !Input
     &    NDRIP, YRDOY, YRPLT, IRRAPL_cm2,        !Input
     &    DEPIR)                                  !Output

      USE ModuleDefs
      USE ModuleData
      USE Cells_2D
      IMPLICIT NONE
      EXTERNAL ERROR, FIND, TIMDIF, SWDEFICIT
      SAVE

      !REAL,               Intent(IN), Optional :: IRRAPL_cm2
      CHARACTER*1 IIRRI   !, MEHYD
      CHARACTER*7, PARAMETER :: ERRKEY = 'DRIPIRR'
!     CHARACTER*78 MSG(1)
      INTEGER DAP, DYNAMIC, I, J, IDL, JJ
      INTEGER NDRIP, TIMDIF, YRDOY, YRPLT
      INTEGER, DIMENSION(NAPPL) :: DripDat, DripEvntEntr
      INTEGER, DIMENSION(NAPPL, NDrpEvnt) :: DripNum
      REAL DEPIR,EFFIRR,IrrRate, ROWSPC_CM,SPD,TotIrrRate
      INTEGER, DIMENSION(NDrpLn):: DripLN
      REAL, DIMENSION(NDrpLn) :: DripSpc, DripOfset, DripDep
      REAL, DIMENSION(NAPPL, NDrpEvnt) :: DripInt, DripRefLN
      REAL, DIMENSION(NAPPL, NDrpEvnt) :: DripRate, DripDur, DripStart
      REAL IRRAPL, temp, BEDWD, BEDHT, IRRAPL_cm2

      CHARACTER*6 SECTION
      INTEGER ERR, FOUND, LINC, LNUM, LUNIO
      LOGICAL AUTO

      TYPE (DripIrrType) DripIrrig(NDrpLn)
      TYPE (ControlType) CONTROL

      DYNAMIC = CONTROL % DYNAMIC

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT .OR. DYNAMIC .EQ. SEASINIT) THEN
C-----------------------------------------------------------------------
      DO IDL = 1, NDrpLn
        DripIrrig(IDL) % DripRate  = 0.0
        DripIrrig(IDL) % DripNum   = 0
        DripIrrig(IDL) % DripStart = 0.0  
        DripIrrig(IDL) % DripDur   = 0.0    
        DripIrrig(IDL) % DripInt   = 0.0    
      END DO
      DripIrrig % DripSpc   = DripSpc
      DripIrrig % DripOfset = DripOfset
      DripIrrig % DripDep = DripDep
      DripIrrig % DripLN  = DripLN
      DripIrrig % DripEvntEntr = 0
      CALL PUT(DripIrrig)
      BEDHT = BedDimension % BEDHT
      BEDWD = BedDimension % BEDWD
      IrrRate = 0.0
      TotIrrRate = 0.0
      DEPIR = 0.0

!     IF (INDEX('GC',MEHYD) < 1 .OR. NDRIP < 1) RETURN
      IF (NDRIP < 1) RETURN

!-----------------------------------------------------------------------
!    Get row spacing from FILEIO
        LUNIO = CONTROL % LUNIO
        OPEN (LUNIO, FILE = CONTROL%FILEIO,STATUS = 'OLD',IOSTAT=ERR)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,CONTROL%FILEIO,0)
        LNUM = 0
        SECTION = '*PLANT'
        CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
        IF (FOUND == 0) THEN
          CALL ERROR(SECTION, 42, CONTROL%FILEIO, LNUM)
        ELSE
          READ(LUNIO,'(42X,F6.0,12X,F6.0)',IOSTAT=ERR)ROWSPC_CM
          LNUM = LNUM + 1
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,CONTROL%FILEIO,LNUM)
        ENDIF
        CLOSE (LUNIO)

!***********************************************************************
!***********************************************************************
!     DAILY RATE CALCULATIONS
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
      DO IDL = 1, NDrpLn
        DripIrrig(IDL) % DripRate = 0.
        DripIrrig(IDL) % DripNum = 0
        DripIrrig(IDL) % DripStart = 0.  
        DripIrrig(IDL) % DripDur = 0.    
        DripIrrig(IDL) % DripInt = 0.
      END DO
      DripIrrig % IrrRate = 0.
      DripIrrig % DripEvntEntr = 0
      DAP = MAX(0,TIMDIF(YRPLT,YRDOY))
      DEPIR = 0
      CALL PUT(DripIrrig)
      IF (AUTO) THEN
        ! MSG(1) ="There is no auto Drip irrigation in DSSAT 2D"
        ! jiN ADD THE FOLLOWING  in Jan, 2015
        !   DripIrrig % DripRate(1) from X file reading
          ! Question if IRRCOD=5 and MEHYD NE 'GC'
              DripEvntEntr = 1  
        !CALL WARNING(1,ERRKEY,MSG)
        !CALL ERROR(ERRKEY,0,"X file Auto",-99)
        ! The following codes has not been tested
        !starting at 8:00am, an hour interview, if >40 minutes split   
        !DO J = 1, NDrpEvnt !CHERYL DID THE FOLLOWING
           IrrRate = 0.
           TotIrrRate = 0.
        DO J = 1, 1 ! Jin assume only one drip event entry
!         IRRAPL is Supplemental irrigation amount for auto irr
          IF (IRRAPL > 1.E-6) THEN 
            DripIrrig(1) % DripEvntEntr = 1
            DripIrrig(1) % DripRate(J)  = DripRate(1, J) * EFFIRR
            DripIrrig(1) % DripStart(J) = 8.0  
           
!          DripIrrig % DripRate(J)  = DripRate(NDRIP, J) * EFFIRR
!          DripIrrig % DripStart(J) = DripStart(NDRIP, J)  
!          DripIrrig % DripInt(J)   = DripInt(NDRIP, J) 
            temp= 0.
!!           Calculate drip duration based on known quantity and drip rate
!            !DripIrrig % DripDur(J) = 
!   ! &        IRRAPL/DripRate(NDRIP,J)*DripSpc * ROWSPC_CM / 10. / 3600.        
!            !!!If (IRRAPL . LE. BEDHT*10.) Then  
!              !!!temp = IRRAPL /
!     &          !!!(DripRate(1,J)* EFFIRR)*DripSpc * BEDWD / 10./3600. !half bed
!     
!!           hr          mm/d        cm/emitter * cm
!!           -- =    ------------- * ---------------
!!            d      cm3/s-emitter     mm/cm * s/hr
!            !!!else 
!              !!!temp = DripSpc * BEDWD * BEDHT/3600./
!     &        !!!       (DripRate(1,J)* EFFIRR)
!              !!!temp = temp + (IRRAPL- BEDHT*10.)/(EFFIRR*
!     &        !!!  DripRate(1,J))*DripSpc * BEDWD / 10./3600.
!            !!!endif 
            temp = IRRAPL_cm2/(DripRate(1,J)* EFFIRR)*DripSpc(1)/3600.
            !IrrRate = IRRAPL
            !DripIrrig % IrrRate = IRRAPL    
!      rounding assume irrigation duration is about 40. minutes, 0.67hr
       DripIrrig(1) % DripNum(1)   = nint(temp/0.67) 
       if (DripIrrig(1)%DripNum(1).EQ.0)DripIrrig(1)%DripNum(1)=1
       if (DripIrrig(1)%DripNum(1).EQ.1)DripIrrig(1)%DripInt(J)=0. !hr
       DripIrrig(1) % DripDur(1) = temp /DripIrrig(1) % DripNum(1) 
       SPD = DripIrrig(1)% DripNum(1) * DripIrrig(1) % DripDur(1) *3600.
!        s/d =    #/day     *   hr        * s/hr 
       !This will be used for output water balance
       IrrRate = IrrRate + 
     &    DripRate(1,J)/ (DripSpc(1) * ROWSPC_CM) * 10. * SPD
       Write(99,*)YRDOY, "IRR DUR=", DripIrrig(1) % DripDur(1),
     &          "DripNum=", DripIrrig(1) % DripNum(1), "IRRAPL=", IRRAPL
          ELSE
            DripIrrig(1) % IrrRate = 0.0    
            DripIrrig(1) % DripNum(J)   = 0
            DripIrrig(1) % DripDur(J) = 0.0
            IrrRate = 0.0
          ENDIF
        End do
        CALL PUT(DripIrrig)
        TotIrrRate = IrrRate
      
!       Drip irrigation schedule maintained until new schedule specified
        DEPIR = MAX(0.0, TotIrrRate)
      ELSE
!-----------------------------------------------------------------------
!** IIRRI = R - As Reported in FIELD SCHEDULE. If day of irrigation then.
!** IIRRI = D - As Reported in FIELD SCHEDULE, days after planting
!** IIRRI = P - As Reported through last reported day (YYYYDDD), then automatic
!               to re-fill profile (as in option A)
!** IIRRI = W - As Reported through last reported day (YYYYDDD), then automatic
!               adding AIRAMT each time
!-----------------------------------------------------------------------
        DO I = 1, NDRIP
          IF ((INDEX('RPW',IIRRI) >0 .AND. DripDat(I) .EQ. YRDOY) .OR.
     &       (INDEX('D',IIRRI)   > 0 .AND. DripDat(I) .EQ. DAP)) THEN
            IrrRate = 0
            TotIrrRate = 0
!             New drip irrigation schedule today
            Do J = 1, DripEvntEntr(I)
              DO IDL = 1, NDripLnTOT
                IF (DripRefLN(I, J) .EQ. DripIrrig(IDL)%DripLN) THEN
          JJ = DripIrrig(IDL) % DripEvntEntr
          JJ = JJ + 1
!         # of event entry on current day
          DripIrrig(IDL) % DripEvntEntr = JJ 
          DripIrrig(IDL) % DripRate(JJ)  = DripRate(I, J) * EFFIRR
          DripIrrig(IDL) % DripNum(JJ)   = DripNum(I, J)
          DripIrrig(IDL) % DripStart(JJ) = DripStart(I, J)  
          DripIrrig(IDL) % DripDur(JJ)   = DripDur(I, J)    
          DripIrrig(IDL) % DripInt(JJ)   = DripInt(I, J)    
          SPD = DripNum(I, J) * DripDur(I, J) * 3600. 
!         s/d =    #/day     *   hr        * s/hr 
          IrrRate = DripRate(I,J)/ (DripSpc(IDL) * ROWSPC_CM) * 10. *SPD
          !IrrRate = DripRate(I)/ (DripSpc * ROWSPC_CM) * 10. * SPD
!                     cm3/s              emitter         mm     s
!           mm/d  =   -------   /        -------        * --  * -
!                    emitter             cm * cm         cm     d
          DEPIR = DEPIR + IrrRate
          DripIrrig(IDL) % IrrRate = DripIrrig(IDL) % IrrRate + IrrRate    
                END IF
              END DO
            end do
            CALL PUT(DripIrrig)
            TotIrrRate = DEPIR
            EXIT
          ENDIF
        Enddo
      ENDIF ! if (not AUTO)

!***********************************************************************
      ENDIF
!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************

      RETURN
      END SUBROUTINE Drip_IRRIG

!=======================================================================
C=====================================================================
!   Drip_IRRIG VARIABLE DEFINITIONS:
!-----------------------------------------------------------------------
! DEPIR     Total irrigation depth for today (mm)
C=======================================================================
C=======================================================================
C  SWDEFICIT_bed, Subroutine
C  Determines soil water deficit for automatic irrigation requirments
!    for bedded system
C-----------------------------------------------------------------------

      SUBROUTINE SWDEFICIT_bed(
     &    DSOIL, DLAYR, DUL, LL, NLAYR, SW, THETAU,       !Input
     &    ATHETA, SWDEF, SWDEF_cm2)                       !Output

      USE ModuleDefs
      USE Cells_2D
      IMPLICIT NONE

      INTENT(IN) DSOIL, DLAYR, DUL, LL, NLAYR, SW
      INTENT(OUT) ATHETA, SWDEF

      INTEGER L, NLAYR
      REAL, DIMENSION(NL) :: DLAYR, DUL, LL, SW
      REAL ATHETA, DEPMAX, DSOIL, SWDEF, TSWTOP, WET1, XDEP, XDEPL
      REAL MULT, MULT_bed, SWDEF_cm2, ROWSPC_cm, BEDWD
      REAL THETAU
      ROWSPC_cm = BedDimension % ROWSPC_cm
      BEDWD = BedDimension % BEDWD
      ATHETA = 1.0
      SWDEF = 0. 
      SWDEF_cm2 = 0.
      IF (BedDimension % RaisedBed) THEN
!       If raised bed, then volume of water deficit is smaller due to
!       presence of furrow.
        ! MULT_bed = BedDimension % BEDWD / BedDimension % ROWSPC_cm
        MULT_bed = BEDWD / ROWSPC_cm
        WET1 = 0.0
        DEPMAX = 0.0
        TSWTOP = 0.0
        DEPMAX = 0.0
        SWDEF_cm2 = 0.0
        DO L = 1,NLAYR
          IF (DEPMAX .LT. DSOIL) THEN
            XDEPL  = DEPMAX
            DEPMAX = DEPMAX + DLAYR(L)
            IF (DEPMAX .GT. DSOIL) THEN
!             DSOIL(cm) Is from IMDEP, in @Auto management of X file
              XDEP = (DSOIL - XDEPL) 
            ELSE
              XDEP = DLAYR(L)
            ENDIF
            IF (L < BedDimension % FurRow1) THEN
              MULT = MULT_bed
              WET1   = WET1   + (DUL(L) - LL(L)) * XDEP * MULT
              TSWTOP = TSWTOP + (SW(L)  - LL(L)) * XDEP * MULT
              SWDEF_cm2 = SWDEF_cm2+max(0.,(DUL(L)-SW(L)))*XDEP*BEDWD
            ELSE
              MULT = 1.0 
 !           ENDIF
              WET1   = WET1   + (DUL(L) - LL(L)) * XDEP * MULT
              TSWTOP = TSWTOP + (SW(L)  - LL(L)) * XDEP * MULT
              SWDEF_cm2=SWDEF_cm2+max(0.,(DUL(L)-SW(L)))*XDEP*ROWSPC_cm
            ENDIF
            ! DUL is in cm3 [H2O] /cm3 [soil]
          ENDIF
        ENDDO
      
        ATHETA = TSWTOP / WET1
        SWDEF  = MAX(0.0,(WET1 - TSWTOP))

      ELSE
!       If no raised bed, just call the original routine.
        CALL SWDEFICIT(
     &    DSOIL, DLAYR, DUL, LL, NLAYR, SW, THETAU,       !Input
     &    ATHETA, SWDEF)                       !Output
        SWDEF_cm2 = SWDEF * ROWSPC_cm
      ENDIF 
      RETURN
      END SUBROUTINE SWDEFICIT_bed

C=======================================================================


