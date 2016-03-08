C=======================================================================
C  IRRIG, Subroutine
C  Determines when irrigation occurs
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  09/01/1988 BB  Restructured from WATBAL
C  02/07/1993 PWW Header revision and minor changes
C  02/25/1994 BDB Added Fixed amount auto irrigation
C  04/19/1994 BDB Added NAP calc. to auto irrigations
C  09/19/1995 GH  Modified to match GRO code
C  07/11/1996 GH  Separated irrigation and precipitation
C  03/23/1997 JWJ Added P and W options
C  10/17/1997 CHP Updated for modular format.
C  09/01/1999 GH  Incorporated into CROPGRO
C  10/15/2001 CHP IRRIG now called from Operations Management module
C                   (MGMTOPS). Output variables are no longer exported.
C  04/16/2002 GH  Adjusted for crop rotations
C  06/12/2002 CHP/US Added flooded irrigation option from rice model
C  08/01/2002 CHP Merged RUNINIT and SEASINIT into INIT section
C  08/19/2002 GH  Modified for Y2K
C  08/12/2003 CHP Added I/O error checking
C  08/12/2003 CHP Fixed problem with automatic irrigation
C  10/28/2004 CHP Fixed problem with multiple applications on same day.
!  06/06/2006 CHP Export TIL_IRR, the irrigation amount which affects 
!                 soil dynamics (excludes drip irrigation).
!  01/11/2007 CHP Changed GETPUT calls to GET and PUT
!  04/18/2013 CHP Added error checking for irrigation amount. It is 
!                   operation-specific, so checking was removed from 
!                   input module.
!  02/08/2016 JOSE Added limited irrigation.
C-----------------------------------------------------------------------
C  Called by: WATBAL
C  Calls  : None
C=======================================================================
      SUBROUTINE IRRIG(CONTROL, ISWITCH,
     &    RAIN, SOILPROP, SW, MDATE, YRPLT , STGDOY,       !Input
     &    FLOODWAT, IIRRI, IRRAMT, NAP, TIL_IRR, TOTIR)   !Output

!-----------------------------------------------------------------------
      USE ModuleDefs 
      USE ModuleData
      USE FloodModule
      IMPLICIT NONE
      SAVE
!-----------------------------------------------------------------------
      CHARACTER*6 ERRKEY
!      CHARACTER*70 IrrText
      PARAMETER (ERRKEY = 'IRRIG')

      CHARACTER*1  IIRRI, ISWWAT, PLME, RNMODE, MESOM !, DEFIR
      CHARACTER*6  SECTION
      CHARACTER*30 FILEIO
      CHARACTER*90 CHAR
      CHARACTER*78 MSG(10)

      INTEGER AIRRCOD, DAP, DYNAMIC, ERRNUM, FOUND, I, IDATE
      INTEGER LUNIO, LINC, LNUM
      INTEGER MULTI, NAP, NAPW, JIRR, NLAYR, NTBL, NMSG
      INTEGER YR, MDATE, RUN, YRDOY, YRPLT, YRSIM, TIMDIF
      INTEGER YRDIF
      INTEGER, DIMENSION(NAPPL) :: IDLAPL, IRRCOD
      INTEGER, DIMENSION(NAPPL) :: JULAPL, JULWTB, JWTBRD
      INTEGER STGDOY(20)

      REAL AIRAMT, AIRAMX, ATHETA, DEPIR, DSOIL, DSOILX
      REAL EFFIRR, EFFIRX, IRRAMT
      REAL SWDEF, THETAC, THETCX, TOTAPW, TOTEFFIRR, TOTIR
      REAL DLAYR(NL), DS(NL), DUL(NL), LL(NL), SW(NL)
      REAL, DIMENSION(NAPPL) :: AMIR, AMT, WTABL

!  Added for flooded field management
      LOGICAL PUDDLED
      INTEGER NBUND, NCOND, NPERC
      INTEGER IBDAT(NAPPL), IIRRCV(NAPPL), IPDAT(NAPPL) !, IIRRP(100)
      INTEGER CONDAT(NAPPL)   !, IIRRC(NAPPL)
      REAL BUND(NAPPL), IPERC(NAPPL), PWAT(NAPPL), COND(NAPPL)
      REAL RAIN, IRRAPL, TIL_IRR, PLOWPAN
      
	  REAL AVWAT    ! Water available for irrigation at planting (mm)
	  REAL AVWATT    ! Water available for irrigation today (mm)

	  LOGICAL IDECV  ! Output of function IDECF
      LOGICAL IDECF  ! Function, determines if irrigation is needed (for A and L)
!-----------------------------------------------------------------------
      TYPE (ControlType)  CONTROL
      TYPE (SwitchType)   ISWITCH
      TYPE (SoilType)     SOILPROP
      TYPE (FloodWatType) FLOODWAT
      Type (MgmtType)     MGMT

!     Transfer values from constructed data types into local variables.
      DYNAMIC = CONTROL % DYNAMIC
      YRDOY   = CONTROL % YRDOY

      IIRRI  = ISWITCH % IIRRI

      DLAYR  = SOILPROP % DLAYR  
      DS     = SOILPROP % DS
      DUL    = SOILPROP % DUL    
      LL     = SOILPROP % LL     
      NLAYR  = SOILPROP % NLAYR  

      PUDDLED= FLOODWAT % PUDDLED

      CALL Get('MGMT','AVWAT', AVWAT)

C***********************************************************************
C***********************************************************************
C    Input and Initialization 
C***********************************************************************
      IF (DYNAMIC .EQ. INIT) THEN
C-----------------------------------------------------------------------
      FILEIO  = CONTROL % FILEIO
      LUNIO   = CONTROL % LUNIO
      MULTI   = CONTROL % MULTI
      YRDIF   = CONTROL % YRDIF
      RNMODE  = CONTROL % RNMODE
      RUN     = CONTROL % RUN
      YRSIM   = CONTROL % YRSIM

      ISWWAT  = ISWITCH % ISWWAT

      TOTAPW = 0
      NAP    = 0
      NAPW   = 0  !irrigation application
      NBUND  = 0  !# bunds
      NTBL   = 0  !# water tables
      NCOND  = 0  !# irrigation applications (same as NAPW??)
      NPERC  = 0  !# percs

      IRRAMT = 0.0
      NAP    = 0
      TOTIR  = 0.
      TOTEFFIRR = 0.
      TIL_IRR = 0.0

!     If in limited irrigation mode, check if there is water for irrigation
      IF ((TOTIR .GE. AVWAT) .AND. (IIRRI .EQ. 'L')) IIRRI = "N"

      IF (ISWWAT .EQ. 'Y') THEN
      !Data is read if not sequenced or seasonal run or for first
      !  season of sequence or seasonal runs.
!        IF ((INDEX('QF',RNMODE) .EQ. 0 .OR. RUN .EQ. 1) 
!     &        .AND. MULTI .LE. 1) THEN

          JIRR = 0.0
          OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT=ERRNUM)
          IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,0)
          LNUM = 0

C-----------------------------------------------------------------------
!     May be needed for MgmtOps output later (OPOPS)
!      SELECT CASE (IIRRI)
!      CASE ('R')
!        IrrText = 'As reported in field schedule in YYDDD format'
!      CASE ('D')
!        IrrText = 'As reported in field schedule in days after ' //
!     &       'planting)'
!      CASE ('A')
!        IrrText = 'Automatic irrigation based on soil water deficit'
!      CASE ('F')
!        IrrText = 'Automatic irrigation with fixed amount (AIRAMT)'
!      CASE ('P')
!        IrrText = 'As reported through last reported day, then ' //
!     &       'automatic to re-fill profile (as in option A)'
!      CASE ('W')
!        IrrText = 'As reported through last reported day, then ' //
!     &       'automatic with fixed amount.'
!       CASE ('N')    !No irrigation
!      END SELECT

C-----------------------------------------------------------------------
C      Read Automatic Management
C-----------------------------------------------------------------------
          IF (INDEX('AFPWL', ISWITCH % IIRRI) > 0) THEN
            SECTION = '!AUTOM'
            CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
            IF (FOUND .EQ. 0) CALL ERROR(SECTION, 42, FILEIO, LNUM)
            READ(LUNIO,'(/,14X,2(1X,F5.0),16X,I2,2(1X,F5.0))',
     &        IOSTAT=ERRNUM) DSOIL, THETAC, AIRRCOD, AIRAMT, EFFIRR
            LNUM = LNUM + 2
            IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)
          ENDIF

C-----------------------------------------------------------------------
!     Find and Read Planting Details Section
C-----------------------------------------------------------------------
          SECTION = '*PLANT'
          CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
          IF (FOUND .EQ. 0) CALL ERROR (SECTION, 42, FILEIO,LNUM)
          READ(LUNIO,'(35X,A1)', IOSTAT=ERRNUM) PLME
          LNUM = LNUM + 1
          IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)

C-----------------------------------------------------------------------
C    Find and Read Irrigation Section
C-----------------------------------------------------------------------
          SECTION = '*IRRIG'
          CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
          IF (FOUND .EQ. 0) CALL ERROR(SECTION, 42, FILEIO, LNUM)
          READ(LUNIO,'(3X,F5.3,2(1X,F5.0),19X,F5.1)', IOSTAT=ERRNUM)
     &      EFFIRX, DSOILX, THETCX, AIRAMX
          LNUM = LNUM + 1
          IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)
          JIRR = 0
          DO I = 1,NAPPL
!           READ(LUNIO,'(3X,I7,3X,I3,1X,F5.0,1X,I5)',IOSTAT=ERRNUM,
!     &        ERR=50)  IDLAPL(I), IRRCOD(I), AMT(I)   !, IIRRC(I)
            READ(LUNIO,'(3X,I7,3X,A90)',ERR=50, END=50) IDLAPL(I),CHAR
            LNUM = LNUM + 1

            READ(CHAR,'(I3,1X,F5.0,1X,I5)',IOSTAT=ERRNUM) 
     &                 IRRCOD(I), AMT(I) 
            IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)
            JIRR = JIRR + 1
          ENDDO
  
   50     CONTINUE
          CLOSE (LUNIO)
!        ENDIF

C-----------------------------------------------------------------------
C     Set Irrigation Management
C-----------------------------------------------------------------------
        IF (IIRRI .EQ. 'R' .OR. IIRRI .EQ. 'D') THEN
          IF (EFFIRX .GT. 0) EFFIRR = EFFIRX
          IF (THETCX .GT. 0) THETAC = THETCX
          IF (DSOILX .GT. 0)  DSOIL = DSOILX
          IF (AIRAMX .GT. 0) AIRAMT = AIRAMX
        ENDIF

!     AMTMIN was not being used -- should it be used in place
!             of AIRAMT?  CHP
        !IF (AIRAMT .GT. 0.0) THEN   
        !  AMTMIN = AIRAMT
        !ELSE
        !  AMTMIN = 5.0
        !ENDIF

C       DSOIL in the CROPGRO model has units of cm
C       THETA has units of %
C
        IF (DSOIL .LE. 0.0) THEN
          DSOIL = (DLAYR(1) + DLAYR(2) + DLAYR(3))
        ENDIF
      ENDIF

!-----------------------------------------------------------------------
!     Sort time series records into water table and irrigation
!-----------------------------------------------------------------------
      CONDAT = 0
      IBDAT  = 0
      IIRRCV = 0
      IPDAT  = 0
      JULAPL = 0
      JWTBRD = 0
      JULWTB = 0

      AMIR  = 0.0
      BUND  = 0.0
      COND  = 0.0
      IPERC = 0.0
      PWAT  = 0.0
      WTABL = 0.0

      PUDDLED = .FALSE.
      PLOWPAN = 0.0

!-----------------------------------------------------------------------
!     Irrigation Codes: IRRCOD
!         1:  Furrow irrigation of specified amount (mm)
!         2:  Alternating furrows; irrigation of specified amount (mm)
!         3:  Flood irrigation of specified amount (mm)
!         4:  Sprinkler irrigation of specified amount (mm)
!         5:  Drip or trickle irrigation of specified amount (mm)
!         6:  Single irrigation to specified total flood depth (mm)
!         7:  Water table depth (cm)
!         8:  Percolation rate (mm/d)
!         9:  Bund height (mm)
!        10:  Puddling (Puddled if IRRCOD = 10 record is present)
!        11:  Maintain constant specified flood depth (mm)

      NMSG = 1
      MSG(1) = "Irrigation data issues:"

!     Transfer irrigation dates to appropriate arrays.
      IF (JIRR .GT. 0) THEN
        DO I = 1,JIRR

          SELECT CASE (IRRCOD(I))

          !------------------------------
           CASE (1:6)    
!          Regular irrigation (bunded or upland)

             IF (AMT(I) < -1.E-6) THEN
               AMT(I) = 0.0
               NMSG = NMSG + 1
               MSG(NMSG)=
     &           "Irrigation amount < zero. Zero irrigation applied."
               CYCLE
             ENDIF

             NCOND         = NCOND + 1        
             CONDAT(NCOND) = IDLAPL(I)
             IIRRCV(NCOND) = IRRCOD(I)
             COND(NCOND)   = AMT(I)

             ! Regular irrigation upland fields
             !
             NAPW = NAPW + 1
             JULAPL(NAPW) = IDLAPL(I)
             AMIR(NAPW)   = AMT(I)

          !------------------------------
           CASE (7)
!          Water table

             IF (AMT(I) < -1.E-6) THEN
               AMT(I) = 0.0
               NMSG = NMSG + 1
               MSG(NMSG)=
     &    "Depth to water table < zero. No change to water table depth."
               CYCLE
             ENDIF

             NTBL = NTBL + 1
!            JWTBRD stores original values
             JWTBRD(NTBL) = IDLAPL(I)  
!            JULWTB can be modified for sequenced or multi-year runs  
             JULWTB(NTBL) = IDLAPL(I)
             WTABL(NTBL)   = AMT(I)         !cm
!            PWAT(NTBL)    = AMT(I)/10.0

          !------------------------------
           CASE (8)
!          Percolation

             IF (AMT(I) < -1.E-6) THEN
               AMT(I) = 2.0
               NMSG = NMSG + 1
               MSG(NMSG) = "Percolation rate < zero; set to 2 mm/d."
               CYCLE
             ENDIF

             NPERC         = NPERC + 1
             IPDAT(NPERC)  = IDLAPL(I)
             IPERC(NPERC)  = AMT(I)/10.0

          !------------------------------
           CASE (9)
!          Bunding

             IF (AMT(I) < -1.E-6) THEN
               AMT(I) = 0.0
               NMSG = NMSG + 1
               MSG(NMSG)=
     &           "Bund height < zero. No change to bund height."
               CYCLE
             ENDIF

             NBUND         = NBUND + 1
             IBDAT(NBUND)  = IDLAPL(I)
             BUND(NBUND)   = AMT(I)

          !------------------------------
           CASE (10)
!          Puddling for rice added

             IF (AMT(I) < -1.E-6) THEN
!            IF (AMT(I) < 1) THEN
               AMT(I) = -1
               NMSG = NMSG + 1
               MSG(NMSG) = 
     &      "Plowpan depth < zero; No plowpan wil be used (ORYZA only)."
!            Remove CYCLE stmt (CHP 8/25/2014)
             ENDIF

             PUDDLED = .TRUE.
!            Depth of puddling input in cm, convert to m
             PLOWPAN = AMT(I) / 100.   
             IF (PLOWPAN < 0.01) THEN
               PLOWPAN = -1.   
             ENDIF

          !------------------------------
           CASE (11)
!          Maintain constant specified flood depth until next irrigation record

             IF (AMT(I) < -1.E-6) THEN
               AMT(I) = 2.0
               NMSG = NMSG + 1
               MSG(NMSG) = 
     &    "Constant flood depth value < zero; No change to flood depth."
               CYCLE
             ENDIF

             NCOND         = NCOND + 1        
             CONDAT(NCOND) = IDLAPL(I)
             IIRRCV(NCOND) = IRRCOD(I)
             COND(NCOND)   = AMT(I)

         END SELECT
        ENDDO
        IF (NMSG > 1) CALL WARNING(NMSG, ERRKEY, MSG)
      ENDIF

!!     Check for using flooded conditions with Century method of 
!!     soil organic matter.  
!      MESOM = ISWITCH % MESOM
!      IF (NBUND .GT. 0 .AND. MESOM .EQ. 'P') THEN
!!        MSG(1) = 'Flooded field not available with Century SOM model.'
!!        MSG(2) = 'Change either management or SOM method in FILEX.'
!!        MSG(3) = 'Program will stop.'
!
!        MSG(1) = 
!     &'Century model was not intended to be used with flooded fields.'
!        MSG(2) = 'Change either management or SOM method in FILEX.'
!        CALL WARNING(2, ERRKEY, MSG)
!!        WRITE(*,'(/,A78,/,A78,/,A78,/)') MSG(1), MSG(2), MSG(3)
!!        CALL ERROR(ERRKEY, 10, "", 0)
!      ENDIF

!-----------------------------------------------------------------------
!     Adjust irrigation dates for multi-year simulations
!     This section was taken from the MRUN subroutine.
!-----------------------------------------------------------------------
      IF (MULTI .GT. 1 .AND. JIRR .GT. 0 .AND. IIRRI .NE. 'D') THEN
        IF (NAPW .GT. 0 .AND. JULAPL(1) .LT. YRSIM) THEN
          DO I = 1, NAPW
            CALL YR_DOY(JULAPL(I),YR,IDATE)
            JULAPL(I) = (YR + MULTI - 1) * 1000 + IDATE
          ENDDO
        ENDIF

        IF (NTBL .GT. 0 .AND. JULWTB(1) .LT. YRSIM) THEN
          DO I = 1, NTBL
            CALL YR_DOY(JULWTB(I),YR,IDATE)
            JULWTB(I) = (YR + MULTI - 1) * 1000 + IDATE
          ENDDO
        ENDIF

        IF (NPERC .GT. 0 .AND. IPDAT(1) .LT. YRSIM) THEN
          DO I = 1, NPERC
            CALL YR_DOY(IPDAT(I),YR,IDATE)
            IPDAT(I) = (YR + MULTI - 1) * 1000 + IDATE
          ENDDO
        ENDIF

        IF (NBUND .GT. 0 .AND. IBDAT(1) .LT. YRSIM) THEN
          DO I = 1, NBUND
            CALL YR_DOY(IBDAT(I),YR,IDATE)
            IBDAT(I) = (YR + MULTI - 1) * 1000 + IDATE
          ENDDO
        ENDIF

        IF (NCOND .GT. 0 .AND. CONDAT(1) .LT. YRSIM) THEN
          DO I = 1, NCOND
            CALL YR_DOY(CONDAT(I),YR,IDATE)
            CONDAT(I) = (YR + MULTI - 1) * 1000 + IDATE
          ENDDO
        ENDIF
      ENDIF

C-----------------------------------------------------------------------
C     Adjust for crop rotations
C-----------------------------------------------------------------------
      IF (RNMODE .EQ. 'Q') THEN
        IF (NAPW .GT. 0 .AND. JULAPL(1) .LT. YRSIM .AND. IIRRI.NE.'D')
     &      THEN
          DO I = 1, NAPW
            CALL YR_DOY(JULAPL(I),YR,IDATE)
            JULAPL(I) = (YR + YRDIF) * 1000 + IDATE
          END DO
        ENDIF

        IF (NTBL .GT. 0 .AND. JULWTB(1) .LT. YRSIM .AND. IIRRI.NE.'D')
     &      THEN
          DO I = 1, NTBL
            CALL YR_DOY(JULWTB(I),YR,IDATE)
            JULWTB(I) = (YR + YRDIF) * 1000 + IDATE
          END DO
        ENDIF

        IF (NPERC.GT. 0 .AND. IPDAT(1) .LT. YRSIM .AND. IIRRI.NE.'D')
     &      THEN
          DO I = 1, NPERC
            CALL YR_DOY(IPDAT(I),YR,IDATE)
            IPDAT(I) = (YR + YRDIF) * 1000 + IDATE
          END DO
        ENDIF

        IF (NBUND .GT. 0 .AND. IBDAT(1) .LT. YRSIM .AND. IIRRI.NE.'D')
     &      THEN
          DO I = 1, NBUND
            CALL YR_DOY(IBDAT(I),YR,IDATE)
            IBDAT(I) = (YR + YRDIF) * 1000 + IDATE
          END DO
        ENDIF

        IF (NCOND .GT. 0 .AND. CONDAT(1) .LT. YRSIM .AND. IIRRI.NE.'D')
     &      THEN
          DO I = 1, NCOND
            CALL YR_DOY(CONDAT(I),YR,IDATE)
            CONDAT(I) = (YR + YRDIF) * 1000 + IDATE
          END DO
        ENDIF
      ENDIF

!     Adjust IPERC values if necessary.
      DO I = 1, NPERC
        IF (DS(NLAYR) .LE. 20.0 .AND. IPERC(I) .GT. 0.) THEN
          IPERC(I) = IPERC(I) * 20.
        ENDIF
        IF (IPERC(I) .LE. 0.0) THEN
          IF (DS(NLAYR) .LE. 20.) THEN
            IPERC(I) = IPERC(1) * 20.0
          ELSE
            IPERC(I) = IPERC(1)
          ENDIF
        ENDIF
      END DO

!     Initialize cumulative seasonal irrigation
      TOTIR = 0.0
      NAP = 0

!      IF (NBUND .GT. 0) THEN
        CALL FLOOD_IRRIG (SEASINIT, 
     &    BUND, COND, CONDAT, IBDAT, IIRRCV, IIRRI,       !Input
     &    IPDAT, IPERC, JULWTB, NBUND, NCOND, NPERC, NTBL,!Input
     &    PUDDLED, PWAT, RAIN, SOILPROP, SW, YRDOY, YRPLT,!Input
     &    FLOODWAT,                                       !I/O
     &    DEPIR)                                          !Output
!      ENDIF

!     Store NBUND in composite variable. Used as a trigger for 
!       potential flooding.
      FLOODWAT % NBUND   = NBUND

!     Transfer data to ModuleData
      CALL PUT('MGMT','TOTIR',TOTIR)
      CALL PUT('MGMT','EFFIRR',EFFIRR)
      CALL PUT('MGMT','IRRAMT',IRRAMT)
      CALL PUT('MGMT','DEPIR', DEPIR)

      FLOODWAT % PUDDLED = PUDDLED
      FLOODWAT % PLOWPAN = PLOWPAN

!***********************************************************************
!***********************************************************************
!     DAILY RATE CALCULATIONS
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
C-----------------------------------------------------------------------
      DEPIR  = 0.
      IRRAMT = 0.
      IRRAPL = 0.0

!     Irrigation amount that affects soil properties after a tillage
!     event, expressed as equivalent rainfall depth (mm).
      TIL_IRR = 0.0

!-----------------------------------------------------------------------
!     Check to see if flood irrigation is done today
!-----------------------------------------------------------------------
      IF (NBUND .GT. 0) THEN
        CALL FLOOD_IRRIG (RATE, 
     &    BUND, COND, CONDAT, IBDAT, IIRRCV, IIRRI,       !Input
     &    IPDAT, IPERC, JULWTB, NBUND, NCOND, NPERC, NTBL,!Input
     &    PUDDLED, PWAT, RAIN, SOILPROP, SW, YRDOY, YRPLT,!Input
     &    FLOODWAT,                                       !I/O
     &    DEPIR)                                          !Output
        IF (DEPIR > 1.E-3) NAP = NAP + 1
      ELSE

!-----------------------------------------------------------------------
!     Now handle regular irrigation events
!-----------------------------------------------------------------------
      SELECT CASE (IIRRI)

C-----------------------------------------------------------------------
C** IIRRI = R - As Reported in FIELD SCHEDULE. If day of irrigation then.
C-----------------------------------------------------------------------
      CASE ('R')
        IF (NAPW .GT. 0) THEN
          LOOP1: DO I = 1, NAPW
            IF (JULAPL(I) .EQ. YRDOY) THEN
              DEPIR = DEPIR + AMIR(I)
              SELECT CASE(IRRCOD(I))
                CASE(1:4,6); TIL_IRR = TIL_IRR + AMIR(I)
              END SELECT
              NAP = NAP + 1
            ELSEIF (JULAPL(I) .GT. YRDOY) THEN
              EXIT LOOP1
            ENDIF
          END DO LOOP1
        ENDIF

C-----------------------------------------------------------------------
C** IIRRI = D - As Reported in FIELD SCHEDULE, days after planting
C-----------------------------------------------------------------------
      CASE ('D')
        IF (NAPW .GT. 0) THEN
          DAP = MAX(0,TIMDIF(YRPLT,YRDOY))
          LOOP2: DO I = 1, NAPW
            IF (JULAPL(I) .EQ. DAP) THEN
              DEPIR = DEPIR + AMIR(I)
              SELECT CASE(IRRCOD(I))
                CASE(1:4,6); TIL_IRR = TIL_IRR + AMIR(I)
              END SELECT
              NAP = NAP + 1
            ELSEIF (JULAPL(I) .GT. DAP) THEN
              EXIT LOOP2
            ENDIF
          END DO LOOP2
        ENDIF

C-----------------------------------------------------------------------
C** IIRRI = A - Automatic irrigation or F-Fixed Amount Automatic Irrigation
C-----------------------------------------------------------------------
      CASE ('A', 'F')
        IF ((YRDOY .GE. YRPLT .AND. YRDOY .LE. MDATE ).OR. 
     &      (YRDOY .GE. YRPLT .AND. MDATE .LE.  -99)) THEN

          CALL SWDEFICIT(
     &        DSOIL, DLAYR, DUL, LL, NLAYR, SW,           !Input
     &        ATHETA, SWDEF)                              !Output

          IDECV = IDECF(ATHETA, THETAC, STGDOY, YRDOY, MDATE)   ! Decide whether or not to irrigate based on deficit irrigation criteria
          IF(IDECV) THEN
!          IF (ATHETA .LE. THETAC*0.01) THEN
!         A soil water deficit exists - automatic irrigation today.

            IF (IIRRI .EQ. 'A') THEN
C             Determine supplemental irrigation amount.
C             Compensate for expected water loss due to soil evaporation
C             and transpiration today.
C             Estimate that an average of 5 mm of water will be lost.
              IRRAPL = SWDEF*10 + 5.0
              IRRAPL = MAX(0.,IRRAPL)

            ELSE IF (IIRRI .EQ. 'F') THEN
C             Apply fixed irrigation amount
              IRRAPL = AIRAMT
            ENDIF

            SELECT CASE(AIRRCOD)
              CASE(1:4,6); TIL_IRR = TIL_IRR + IRRAPL
            END SELECT

            DEPIR = DEPIR + IRRAPL
            NAP = NAP + 1
!           chp 3/20/2014 these are not used and result in array bounds errors
!             in long simulations.
            !JULAPL(NAP) = YRDOY
            !AMIR(NAP)   = IRRAPL
          ENDIF
        ENDIF


C-----------------------------------------------------------------------
C** IIRRI = L - Limited irrigation (based on Automatic irrigation)
C-----------------------------------------------------------------------
      CASE ('L')

        IF ((YRDOY .GE. YRPLT .AND. YRDOY .LE. MDATE ).OR.
     &      (YRDOY .GE. YRPLT .AND. MDATE .LE.  -99)) THEN

          CALL SWDEFICIT(
     &        DSOIL, DLAYR, DUL, LL, NLAYR, SW,           !Input
     &        ATHETA, SWDEF)                              !Output

          IDECV = IDECF(ATHETA, THETAC, STGDOY, YRDOY, MDATE)   ! ADDED TO WORK WITH DEFICIT IRRIGATION
          IF(IDECV) THEN
!          IF (ATHETA .LE. THETAC*0.01) THEN
!         A soil water deficit exists - automatic irrigation today.

C           Determine supplemental irrigation amount.
C           Compensate for expected water loss due to soil evaporation
C           and transpiration today.
C           Estimate that an average of 5 mm of water will be lost.
            IRRAPL = SWDEF*10 + 5.0
            IRRAPL = MAX(0.,IRRAPL)

            SELECT CASE(AIRRCOD)
              CASE(1:4,6); TIL_IRR = TIL_IRR + IRRAPL
            END SELECT

            DEPIR = DEPIR + IRRAPL
            AVWATT = AVWAT - TOTIR ! Calculate water available today
            IF ((DEPIR .GT. AVWATT) .AND. (IIRRI .EQ. 'L')) THEN
              DEPIR = AVWATT   ! IF irrigation greater than water available, limit irrigation
            ENDIF

            NAP = NAP + 1

          ENDIF
        ENDIF


C-----------------------------------------------------------------------
C** IIRRI = P - As Reported through last reported day, then automatic
C          to re-fill profile (as in option A)
C   IIRRI = W - As Reported through last reported day, then automatic
C**         adding AIRAMT each time
C-----------------------------------------------------------------------
      CASE ('P', 'W')
        IF (NAPW .GT. 0) THEN
          LOOP3: DO I = 1, NAPW
            IF (JULAPL(I) .EQ. YRDOY) THEN
              DEPIR = DEPIR + AMIR(I)
              NAP = NAP + 1
            ELSEIF (JULAPL(I) .GT. YRDOY) THEN
              EXIT LOOP3
            ENDIF
          END DO LOOP3
        ENDIF

C-----------------------------------------------------------------------
C       If Today's date is after the last record in the IRRIG section
c           check to see if automatic irrigation is needed (P or W option)
C-----------------------------------------------------------------------

        IF (YRDOY .GT. JULAPL(NAPW))THEN
          !Past end of records - automatic irrigation.

          IF ((YRDOY .GE. YRPLT .AND. YRDOY .LE. MDATE ).OR. 
     &        (YRDOY .GE. YRPLT .AND. MDATE .LE.  -99)) THEN

            CALL SWDEFICIT(
     &        DSOIL, DLAYR, DUL, LL, NLAYR, SW,           !Input
     &        ATHETA, SWDEF)                              !Output

            IF (ATHETA .LE. THETAC*0.01) THEN
!           A soil water deficit exists - automatic irrigation today.

              IF (IIRRI .EQ. 'P') THEN
C               Determine supplemental irrigation amount.
C               Compensate for expected water loss due to soil evaporation
C               and transpiration today.
C               Estimate that an average of 5 mm of water will be lost.
                IRRAPL = SWDEF*10 + 5.0
                IRRAPL = MAX(0.,IRRAPL)

              ELSE IF (IIRRI .EQ. 'W') THEN
C               Apply fixed irrigation amount
                IRRAPL = AIRAMT
              ENDIF

              DEPIR = DEPIR + IRRAPL
              NAP = NAP + 1
              !JULAPL(NAP+1) = YRDOY
              !AMIR(NAP+1)   = IRRAPL

            ENDIF
          ENDIF
        ENDIF

      END SELECT
      ENDIF
C-----------------------------------------------------------------------
C    *********    IRRIGATE     **********
C-----------------------------------------------------------------------
      IF (EFFIRR .GT. 0.0) THEN
        IRRAMT = DEPIR*EFFIRR
      ELSE
        IRRAMT = DEPIR
      ENDIF

      CALL PUT('MGMT','IRRAMT',IRRAMT)  !Effective irrig amt today (mm)
      
!***********************************************************************
!***********************************************************************
!     DAILY INTEGRATION CALCULATIONS
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
C-----------------------------------------------------------------------
      IF (DEPIR .GT. 0.0) THEN
        !NAP    = NAP + 1
        TOTIR  = TOTIR + DEPIR
        TOTEFFIRR = TOTEFFIRR + IRRAMT
      ENDIF

!     Transfer data to ModuleData
      CALL PUT('MGMT','DEPIR', DEPIR)   !Total irrig amt today (mm)
      CALL PUT('MGMT','TOTIR', TOTIR)   !Total applied irrigation (mm)
      CALL PUT('MGMT','EFFIRR',EFFIRR)  !Effective irrigation %
      CALL PUT('MGMT','IRRAMT',IRRAMT)  !Effective irrig amt today (mm)

!***********************************************************************
      ENDIF
!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      FLOODWAT % PUDDLED = PUDDLED
      
      RETURN
      END SUBROUTINE IRRIG
C=======================================================================

C=======================================================================
C  SWDEFICIT, Subroutine
C  Determines soil water deficit for automatic irrigation requirments
C-----------------------------------------------------------------------

      SUBROUTINE SWDEFICIT(
     &    DSOIL, DLAYR, DUL, LL, NLAYR, SW,               !Input
     &    ATHETA, SWDEF)                                  !Output

      USE ModuleDefs
      IMPLICIT NONE

      INTENT(IN) DSOIL, DLAYR, DUL, LL, NLAYR, SW
      INTENT(OUT) ATHETA, SWDEF

      INTEGER L, NLAYR
      REAL, DIMENSION(NL) :: DLAYR, DUL, LL, SW
      REAL ATHETA, DEPMAX, DSOIL, SWDEF, TSWTOP, WET1, XDEP, XDEPL

      WET1 = 0.0
      DEPMAX = 0.0
      TSWTOP = 0.0
      DEPMAX = 0.0

      DO L = 1,NLAYR
        IF (DEPMAX .LT. DSOIL) THEN
          XDEPL  = DEPMAX
          DEPMAX = DEPMAX + DLAYR(L)
          IF (DEPMAX .GT. DSOIL) THEN
            XDEP = (DSOIL - XDEPL)
          ELSE
            XDEP = DLAYR(L)
          ENDIF
          WET1 = WET1   + (DUL(L) - LL(L)) * XDEP
          TSWTOP = TSWTOP + (SW(L) - LL(L)) * XDEP
        ENDIF
      ENDDO

      ATHETA = TSWTOP / WET1
      SWDEF  = MAX(0.0,(WET1 - TSWTOP))

      RETURN
      END SUBROUTINE SWDEFICIT

C=======================================================================

C=======================================================================
C  IDECF, Function
C  Determines if water deficit exists to triger automatic or limited irrigation

!     The following models are currently supported in IDECF, for all others select N in DEFIR:
!         'CRGRO' - CROPGRO
!         'CSCER' - CERES Wheat, Barley
!         'MLCER' - CERES-Millet
!         'MZCER' - CERES-Maize
!         'RICER' - CERES-Rice
!         'SGCER' - CERES-Sorghum
!         'SWCER' - CERES-Sweet corn

      FUNCTION IDECF(ATHETA,THETAC,STGDOY,YRDOY, MDATE) RESULT(R)

       USE ModuleData

       REAL, INTENT(IN) :: ATHETA, THETAC  ! INPUT  I = ATHETA and J = THETAC
       REAL SWFAC
       INTEGER STGDOY(20)             ! GROWTH STAGE ONSET OR END IN YRDOY
       LOGICAL R                 ! OUTPUT
       REAL AVWAT, THETAC2, SITH1, SITH2
       INTEGER FIST1, FIST2, YRDOY, MDATE
       CHARACTER*1 DEFIR

       CHARACTER*6 ERRKEY
       PARAMETER (ERRKEY = 'IRRIG')
       INTEGER ERRNUM, LNUM
       CHARACTER*8 MODEL
       CHARACTER*8 MODEL_WO_V  ! MODEL WITHOUT VERSION

       CALL Get('MGMT','AVWAT', AVWAT)
       CALL Get('MGMT','FIST1', FIST1)
       CALL Get('MGMT','FIST2', FIST2)
       CALL Get('MGMT','THETAC2', THETAC2)
       CALL Get('MGMT','DEFIR', DEFIR)
       CALL Get('MGMT','SWFAC', SWFAC)
       CALL Get('MGMT','SITH1', SITH1)
       CALL Get('MGMT','SITH2', SITH2)

       MODEL = SAVE_data % Control % MODEL
       MODEL_WO_V = MODEL(1:5)


        SELECT CASE (DEFIR)
         CASE ('N')  ! NO DEFICIT IRRIGATION
             R = (ATHETA .LE. THETAC*0.01)
         CASE ('V', 'S')  ! DEFICIT IRRIGATION BASED ON SOIL VOLUMETRIC WATER CONTENT
             ! IS THIS ONE OF THE FULLY IRRIGATED STAGES?
          IF(
     &       ((SITH1 .GT. 1.0) .OR. (SITH2 .GT. 1.0)) .OR.
     &       ((SITH1 .LT. 0.0) .OR. (SITH2 .LT. 0.0))
     &        ) THEN
               WRITE (*, *) "=========================================",
     &                      "======================================"
               WRITE (*, *) "ERROR: Invalid input for SITH1 or SITH2, ",
     &                      "please enter a number between 0 and 1."
               WRITE (*, *) "=========================================",
     &                      "======================================"
               STOP
          ENDIF
          SELECT CASE (MODEL_WO_V)
           CASE ('CSCER','MLCER', 'MZCER', 'RICER', 'SGCER', 'SWCER')
            IF(
     &         ((FIST1 .GE. 6) .OR. (FIST2 .GE. 6)) .OR.
     &         ((FIST1 .LE. 0) .OR. (FIST2 .LE. 0))
     &        ) THEN
              WRITE (*, *) "======================================="
              WRITE (*, *) "ERROR: Invalid input for FIST1 or FIST2"
              WRITE (*, *) "======================================="
              STOP
            ELSE IF(
     &         ((FIST1 .EQ. 1) .OR. (FIST2 .EQ. 1)) .AND.
     &         ((YRDOY .GE. STGDOY(9)) .AND. (YRDOY.LT. STGDOY(1))).OR. ! IS STAGE 1 IS FULLY IRRIGATIED ?
     &         ((FIST1 .EQ. 2) .OR. (FIST2 .EQ. 2)) .AND.
     &         ((YRDOY .GE. STGDOY(1)) .AND. (YRDOY.LT. STGDOY(2))).OR. ! IS STAGE 2 IS FULLY IRRIGATIED ?
     &         ((FIST1 .EQ. 3) .OR. (FIST2 .EQ. 3)) .AND.
     &         ((YRDOY .GE. STGDOY(2)) .AND. (YRDOY.LT. STGDOY(3))).OR. ! IS STAGE 3 IS FULLY IRRIGATIED ?
     &         ((FIST1 .EQ. 4) .OR. (FIST2 .EQ. 4)) .AND.
     &         ((YRDOY .GE. STGDOY(3)) .AND. (YRDOY.LT. STGDOY(4))).OR. ! IS STAGE 4 IS FULLY IRRIGATIED ?
     &         ((FIST1 .EQ. 5) .OR. (FIST2 .EQ. 5)) .AND.
     &          (YRDOY .GE. STGDOY(4))
     &       ) THEN                                                    ! IS STAGE 5 IS FULLY IRRIGATIED ?
             SELECT CASE(DEFIR)
              CASE('V')
               R = (ATHETA .LE. THETAC2*0.01)                              ! IRRIGATE WITH FULL IRRIGATION CRITERIA
              CASE('S')
               R = (SWFAC .LE. SITH2)
             END SELECT
            ELSE
             SELECT CASE(DEFIR)
              CASE('V')
               R = (ATHETA .LE. THETAC*0.01)                              ! IRRIGATE BASED ON DEFICIT IRRIGATION CRITERIA
              CASE('S')
               R = (SWFAC .LE. SITH1)
             END SELECT
            ENDIF
           CASE ('CRGRO')
            IF(
     &         ((2  .EQ. FIST1) .OR. (2  .EQ. FIST2)) .OR.
     &         ((4  .EQ. FIST1) .OR. (4  .EQ. FIST2)) .OR.
     &         ((FIST1 .GE.  6) .OR. (FIST2 .GE.  6)) .OR.
     &         ((FIST1 .LE. -1) .OR. (FIST2 .LE. -1))
     &        ) THEN
             WRITE (*, *) "========================================"
             WRITE (*, *) "ERROR: Invalid input for FIST1 or FIST2."
             WRITE (*, *) "========================================"
             STOP
            ELSE IF(
     &         ((FIST1 .EQ. 0) .OR. (FIST2 .EQ. 0)) .AND.
     &         ((YRDOY .GE. STGDOY(1)) .AND. (YRDOY.LT. STGDOY(5))).OR. ! IS STAGE 0 IS FULLY IRRIGATIED ?
     &         ((FIST1 .EQ. 1) .OR. (FIST2 .EQ. 1)) .AND.
     &         ((YRDOY .GE. STGDOY(5)) .AND. (YRDOY.LT. STGDOY(6))).OR. ! IS STAGE 1 IS FULLY IRRIGATIED ?
     &         ((FIST1 .EQ. 3) .OR. (FIST2 .EQ. 3)) .AND.
     &         ((YRDOY .GE. STGDOY(6)) .AND. (YRDOY.LT. STGDOY(8))).OR. ! IS STAGE 3 IS FULLY IRRIGATIED ?
     &         ((FIST1 .EQ. 5) .OR. (FIST2 .EQ. 5)) .AND.
     &         ((YRDOY .GE. STGDOY(8)) .AND. (YRDOY.LT. STGDOY(11)))    ! IS STAGE 5 IS FULLY IRRIGATIED ?
     &       ) THEN
             R = (ATHETA .LE. THETAC2*0.01)                              ! IRRIGATE WITH FULL IRRIGATION CRITERIA
            ELSE
             R = (ATHETA .LE. THETAC*0.01)                              ! IRRIGATE BASED ON DEFICIT IRRIGATION CRITERIA
            ENDIF
           CASE DEFAULT                                                 ! ERROR MESSAGE IN SCREEN ONLY!
            WRITE (*, *) "===========================================",
     &                   "============================"
            WRITE (*, *) "ERROR: Deficit irrigation is not supported ",
     &                   "for the crop model selected."
            WRITE (*, *) "===========================================",
     &                   "============================"
            STOP
          END SELECT
        CASE DEFAULT
         WRITE (*, *) "====================================="
         WRITE (*, *) "ERROR: Invalid DEFIR input in X file."
         WRITE (*, *) "====================================="
         STOP
        END SELECT

      END FUNCTION IDECF

C-----------------------------------------------------------------------
C=======================================================================

!***********************************************************************
!***********************************************************************
!     IRRIG VARIABLE DEFINITIONS:
!-----------------------------------------------------------------------
! AIRAMT    Amount of irrigation applied, if fixed, for automatic 
!             irrigation (mm)
! AIRAMX    Fixed irrigation amount (mm)
! AMIR(I)   Irrigation depth of the Ith application (mm)
! AMT(I)    Irrigation amount, depth of water/water table, bund height, or 
!             percolation rate  (mm or mm/day)
! ATHETA    Average available water in top irrigation management depth of 
!             soil (%)
! DAP       Number of days after planting (d)
! DEPIR     Irrigation depth for today (mm)
! DLAYR(L)  Soil thickness in layer L (cm)
! DSOIL     Irrigation management depth (cm)
! DSOILX    Management depth for automatic irrigation (cm)
! EFFIRR    Irrigation application efficiency (cm/cm)
! EFFIRX     
! ERRKEY    Subroutine name for error file 
! ERRNUM    Error number for input 
! FILEIO    Filename for input file (e.g., IBSNAT35.INP) 
! FOUND     Indicator that good data was read from file by subroutine FIND 
!             (0 - End-of-file encountered, 1 - NAME was found) 
! IDATE     Day of irrigation or fertilizer application (d)
! IDLAPL(I) Irrigation or water table dates read from input file. 
! IIRRI     Irrigation switch R=on reported dates, D=as reported, days 
!             after planting, A=automatic, when rqd., F=automatic w/ fixed 
!             amt, P=as reported thru last reported day then automatic, 
!             W=as reported thru last reported day then fixed amount, N=not 
!             irrigated 
! IRRAMT    Irrigation amount (mm)
! IRRCOD(I) Irrigation operation code: 1=Furrow, 2=Alternating furrows, 
!             3=flood, 4=Sprinkler, 5=Drip or trickle, 6=Flood depth, 
!             7=Water table depth, 8=Percolation rate, 9=Bund height
!             (mm or mm/day)
! JULAPL    Julian date for scheduled irrigation application (DOY)
! JULWTB    Julian date for scheduled irrigation application (YYDDD)
! JWTBRD    Recorded water table dates, saved for date modification on 
!             seasonal or sequenced runs 
! LNUM      Current line number of input file 
! LUNIO     Logical unit number for FILEIO 
! MULTI     Current simulation year (=1 for first or single simulation, 
!             =NYRS for last seasonal simulation) 
! NAP       Number of irrigation applications 
! NAPW      Number of irrigation values read from input file 
! JIRR      Number of irrigation records 
! NL        Maximum number of soil layers = 20 
! NTBL      Number of water table values read 
! SECTION   Section name in input file 
! SWDEF     Soil water deficit (cm)
! THETAC    Threshold, % of maximum available water triggering irrigation
!             (%)
! THETCX    Threshold for automatic aplication, % of maximum avail water.
!             (%)
! TOTAPW    Cumulative irrigation applied (mm)
! TOTIR     Total seasonal irrigation (mm)
! WTABL     Water table record associated with JULWTB (cm)
! YR        Year portion of date 
! YRDIF     Function subroutine which calculates number of days between two 
!             dates (da)
! YRDOY     Current day of simulation (YYDDD)
! MDATE     Harvest maturity date (YYDDD)
! YRPLT     Planting date (YYDDD)
! YRSIM     Start of simulation date (YYDDD)
!-----------------------------------------------------------------------
!     END SUB-MODULE IRRIG
!-----------------------------------------------------------------------
