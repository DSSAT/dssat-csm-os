C=======================================================================
C  CSP_CANOPY, Subroutine, FSR adapted from CANOPY for CASUPRO sugarcane.
C  G. Hoogenboom, K.J. Boote, J.W. Jones
C  Calculates canopy height and canopy width as a function of node number,
C  air temperature, drought stress, daylength, and radiation. 
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  05/01/1989 Written.
C  04/24/1994 NBP Changed TAIRHR to TGRO.  Edited indentation.
C  01/19/1996 KJB Include PAR effect on expansion.
C  07/15/1998 CHP Modified for modular format
C  05/15/1999 GH  Incorporated into CROPGRO
C  01/22/2003 KJB Add checks for minimum canopy height and width.
C  08/12/2003 CHP Revised I/O error checking
C  06/30/2004 CHP/CDM Added KC_SLOPE to SPE file and KC_ECO to ECO file.
C                 Added optional KCAN to ECO file.
C  09/30/2007 FSR Incorporated SHADOW and adapted to CASUPRO sugarcane model. 
C-----------------------------------------------------------------------
C  Called by : CSP_PHOTO
C  Calls     : CSP_SHADOW, ERROR, FIND, IGNORE
C========================================================================

      SUBROUTINE CSP_CANOPY(CONTROL, 
     &    DAS, DeltaLeafNum, ECONO, FILECC,               !Input
     &    FILEGC, FRACSH, FRSHV, KCAN,                    !Input
     &    LeafNum, PAR, ROWSPC, Smax, StalkState,         !Input 
     &    TGRO, TURFAC, WEATHER, XHLAI,                   !Input
     &    CANHT, CANWH, FRSHAV, H, StkHt)                 !Output
C-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      EXTERNAL FIND, ERROR, GETLUN, IGNORE, CSP_SHADOW, TABEX
      SAVE

      CHARACTER*6 ERRKEY
      PARAMETER (ERRKEY = 'CANOPY')
      CHARACTER*4 StalkState(NumOfStalks,10)
	CHARACTER*6   SECTION
      CHARACTER*6   ECOTYP, ECONO
	CHARACTER*30  FILEIO
      CHARACTER*80  CHAR
      CHARACTER*92  FILECC, FILEGC
      CHARACTER*255 C255


      INTEGER I, II, DAS, DYNAMIC, H, LUNCRP, LUNECO, LUNIO 
      INTEGER ERR, LINC, LNUM, ISECT, NHOUR
      INTEGER FOUND, Smax, Stalk

      LOGICAL DAYTIM

      REAL AZIR, AZZON(TS), BETA(TS), BETN, ROWSPC, TURFAC 
      REAL CANHT, CANWH, FRACSH, FRSHAV, FRSHV, XHLAI
      REAL HS, KCAN, MINSHD, PAR, PLTPOP
      REAL HWTEM, RCANHT, RCANWH, PARNOD, HPAR, WPAR
      REAL SNDN, SNUP, TABEX, TINCR
      REAL XHWPAR(10), XHWTEM(10), YHWPAR(10), YHWTEM(10)
      REAL XVSHT(15), YVSHT(15), YVSWH(15)
      REAL TGRO(TS), tmp
	REAL, DIMENSION(1:NumOfStalks) :: StkHt
      REAL, DIMENSION(0:NumOfDays,NumOfStalks) :: DeltaLeafNum,
     &                  LeafNum  

      PARAMETER (TINCR=24.0/TS)
       
      TYPE (WeatherType) WEATHER
      TYPE (ControlType) CONTROL

       DYNAMIC = CONTROL % DYNAMIC
	 FILEIO  = CONTROL % FILEIO
       LUNIO   = CONTROL % LUNIO

       AZZON   = WEATHER % AZZON 
       BETA    = WEATHER % BETA   
       SNDN    = WEATHER % SNDN   
       SNUP    = WEATHER % SNUP   

!***********************************************************************
!***********************************************************************
!     Run Initialization - Called once per simulation
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN

!-----------------------------------------------------------------------
!     Read Experiment File data from FILEIO (DSSAT**.INP) 
!-----------------------------------------------------------------------
      OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,0)
      LNUM = 0
!-----------------------------------------------------------------------
      SECTION = '*PLANT'
      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILEIO, LNUM)
      ELSE
        READ(LUNIO,'(24X,F6.0,12X,2F6.0)',IOSTAT=ERR)PLTPOP,ROWSPC,AZIR
      LNUM = LNUM + 1
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)

      ENDIF      
      CLOSE (LUNIO)
C-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!     Read in values from SPE (Species) Input file
!-----------------------------------------------------------------------
      CALL GETLUN('FILEC', LUNCRP)
      OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,0)
      LNUM = 0
!-----------------------------------------------------------------------
!    Find and Read Photosynthesis Section
!-----------------------------------------------------------------------
!     Subroutine FIND finds appropriate SECTION in a file by
!     searching for the specified 6-character string at beginning
!     of each line.
!-----------------------------------------------------------------------
!CHP 7/30/2004 - Get KCAN from main routine.
!                May be overriden by value in ECOTYPE file.
!      SECTION = '!*PHOT'
!      CALL FIND(LUNCRP, SECTION, LINC, FOUND); LNUM = LNUM + LINC
!      IF (FOUND .EQ. 0) THEN
!        CALL ERROR(SECTION, 42, FILECC, LNUM)
!      ELSE
!        ISECT = 2
!        DO WHILE (ISECT .NE. 1)
!          CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
!          IF (ISECT .EQ. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
!        ENDDO
!        READ(C255,'(12X,F6.0)',IOSTAT=ERR) KCAN
!        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
!      ENDIF
!-----------------------------------------------------------------------
C     ***** READ CANOPY HEIGHT & WIDTH PARAMETERS ******************
C-----------------------------------------------------------------------
      SECTION = '*#CANO'
      CALL FIND(LUNCRP, SECTION, LINC, FOUND); LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
        READ(C255,'(5F6.0)',IOSTAT=ERR)(XHWTEM(II),II = 1,5)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
        READ(C255,'(5F6.0)',IOSTAT=ERR)(YHWTEM(II),II = 1,5)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
        READ(C255,'(8F6.0)',IOSTAT=ERR)(XHWPAR(II),II = 1,8)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
        READ(C255,'(8F6.0)',IOSTAT=ERR)(YHWPAR(II),II = 1,8)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
      ENDIF

      CLOSE (LUNCRP)

C-----------------------------------------------------------------------
C    Read Ecotype Parameter File
C-----------------------------------------------------------------------
      CALL GETLUN('FILEE', LUNECO)
      OPEN (LUNECO,FILE = FILEGC,STATUS = 'OLD',IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEGC,0)
		ECOTYP = '      '
		LNUM = 0

C------ Find particular ECOTYPE ----------------------------------------

		SECTION = ECONO
		CALL FIND(LUNECO, SECTION, LNUM, FOUND)
		IF (FOUND .EQ. 0) THEN
			CALL ERROR(SECTION, 42, FILECC, LNUM)
		ELSE          
              CALL IGNORE(LUNECO,LNUM,ISECT,CHAR) !KCAN_ECO
              CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)
              CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)
              CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)
              CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)

		      CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)
		      READ(CHAR,'(14X,F6.3)',IOSTAT=ERR) MINSHD 
 		      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEGC,LNUM)
 		      
              CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)
              CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)
              CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)
              CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)
              CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)
              CALL IGNORE(LUNECO,LNUM,ISECT,CHAR) !YVTR
  
			CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)
			READ(CHAR,'(14X,10F6.0)',IOSTAT=ERR)(XVSHT(II),II = 1,10) 
			IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEGC,LNUM)
  
			CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)
			READ(CHAR,'(14X,10F6.1)',IOSTAT=ERR)(YVSHT(II),II = 1,10) 
			IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEGC,LNUM)
	
			CALL IGNORE(LUNECO,LNUM,ISECT,CHAR) !ZVSDI
  
			CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)
			READ(CHAR,'(14X,10F6.2)',IOSTAT=ERR)(YVSWH(II),II = 1,10) 
			IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEGC,LNUM)
	
      ENDIF ! (ERR .NE. 0)
      CLOSE (LUNECO)

      CANHT = 0.
      CANWH = 0.
      DO Stalk = 1,Smax 
	   StkHt(Stalk) = 0.
      END DO
!***********************************************************************
!***********************************************************************
!     SEASONAL INITIALIZATION 
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      CANHT = 0.0
      CANWH = 0.0
	ROWSPC = ROWSPC/100
      FRACSH = 0.0
	FRSHAV = 0.0

      IF (ROWSPC.GT.0.0 .AND. PLTPOP.GT.0.0) THEN
        BETN = 1.0 / (ROWSPC*PLTPOP)
      ELSE
        BETN = 0.0
      ENDIF
!***********************************************************************
!***********************************************************************
!     EMERGENCE CALCULATIONS - Performed once per season upon emergence
!         or transplanting of plants
!***********************************************************************
!      ELSEIF (DYNAMIC .EQ. EMERG) THEN
!-----------------------------------------------------------------------
!  Note: no EMERG in CSP_GROW_CANE, so this does not get done

!        CANHT  = TABEX(YVSHT,XVSHT,LeafNum(DAS,Stalk),10)       
!        CANWH  = TABEX(YVSWH,XVSHT,LeafNum(DAS,Stalk),10)       
!***********************************************************************
!***********************************************************************
!     DAILY RATE/INTEGRATION
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------
C     Calculate effect of temperature on canopy expansion, HWTEM
C-----------------------------------------------------------------------
      HWTEM = 0.0
      DO I = 1,TS
        HWTEM = HWTEM + TABEX(YHWTEM,XHWTEM,TGRO(I),5)
      ENDDO
      HWTEM = HWTEM /TS
C 24 changed to TS on 5Jul17 by Bruce Kimball


C-----------------------------------------------------------------------
C     Calculate effect of day's PAR on canopy expansion, HPAR.
C     ASSUME THAT UPPER 30% OF CANOPY SHADES THE GROWING POINT
C     WPAR IS EFFECT ON WIDTH.  SHADE DOES NOT MAKE MUCH WIDER. LOWER K?
C-----------------------------------------------------------------------
C     IF (XLAI .GT. 0.1) THEN
C        PARNOD = PAR * EXP(-KCAN*0.3*(XLAI-0.1))
C     ELSE
C        PARNOD = PAR
C     ENDIF
C-----------------------------------------------------------------------
      PARNOD = PAR * EXP(-KCAN*(0.3*XHLAI))
      HPAR = TABEX(YHWPAR,XHWPAR,PARNOD,8)
      WPAR = TABEX(YHWPAR,XHWPAR,PAR,8)
C-----------------------------------------------------------------------
C     Calculate rate of increase in canopy height and update height, CANHT
C-----------------------------------------------------------------------
!! Height is only dependent on the tallest stalk.
!! Calculate each, selecting MAX. 

!! This should be verified against a spreadsheet example to make sure
!! it is behaving as intended  - FSR

	CANHT = 0.
  
      DO Stalk = 1,Smax 
        IF (StalkState(Stalk,1) .EQ. 'LIVE') THEN 
		
		tmp = TABEX(YVSHT,XVSHT,LeafNum(DAS,Stalk),10)

          RCANHT= DeltaLeafNum(DAS,Stalk) 
     &        * TABEX(YVSHT,XVSHT,LeafNum(DAS,Stalk),10) 
     &        * HWTEM * TURFAC * HPAR / 100    ! cm to m
          StkHt(Stalk) = StkHt(Stalk) + RCANHT

!     Set minimum Canopy height based on lookup function
		CANHT = MAX(CANHT, TABEX(YVSHT,XVSHT, 0.0, 10)/100)

!     Select tallest stalk as canopy height
		CANHT = MAX(CANHT, StkHt(Stalk)) ! 
        
	  ELSE 
	     StkHt(Stalk) = 0.

        END IF

      END DO 

C-----------------------------------------------------------------------
C    Calculate rate of increase in canopy width and update width. 
!    
!    Width is affected by each stalk, though it is assumed that the  
!    cumulative effect diminishes with each successive stalk.
!-----------------------------------------------------------------------

      DO Stalk = 1,Smax 
        IF (StalkState(Stalk,1) .EQ. 'LIVE') THEN 

		RCANWH= MAX(0.0,DeltaLeafNum(DAS,Stalk)) 
     &			* TABEX(YVSWH,XVSHT,LeafNum(DAS,Stalk),10)
     &			* HWTEM * TURFAC * WPAR / 100    ! cm to m

!     Part of each tiller width is added 
		CANWH = CANWH + (RCANWH * 1/(Stalk))
!!		CANWHtmp = CANWH

!     Set minimum Canopy width based on lookup function
		CANWH = MAX(CANWH, TABEX(YVSWH, XVSHT, 0.0, 10)/100)  

!     Limit Canopy width to row spacing
		CANWH = MIN(CANWH,ROWSPC)  
          
		tmp = TABEX(YVSWH,XVSHT,LeafNum(DAS,Stalk),10) ! debug temp lines 

        END IF
      END DO 

!!		CANWHtmp = CANWH
C=======================================================================
C     24-hour DO loop for calling CSP_SHADOW and averaging FRACSH
      
	NHOUR = 0.
	FRSHAV = 0.

      DO H=1,TS

C       Calculate real and solar time.

          HS = REAL(H) * TINCR
          IF (HS.GT.SNUP .AND. HS.LT.SNDN) THEN
            DAYTIM = .TRUE.
          ELSE
            DAYTIM = .FALSE.
          ENDIF
C-----------------------------------------------------------------------
          IF (XHLAI .GT. 1.E-4) THEN

C       Calculate fraction shaded for vertical sun position.

             IF (H .EQ. 1) THEN

             CALL CSP_SHADOW(
     &         AZIR, AZZON,BETA(1),BETN,CANHT,CANWH,H,ROWSPC,   !Input
!    &         WEATHER,                                         !Input
     &         FRSHV)                                           !Output

             ENDIF
C-----------------------------------------------------------------------
C       Calculate fraction shaded.

             IF (DAYTIM) THEN
!!!             IF (H .EQ. 15) THEN
             
		   NHOUR = NHOUR + 1
		       
             CALL CSP_SHADOW(
     &         AZIR, AZZON, BETA, BETN, CANHT, CANWH, H, ROWSPC,!Input
!    &         WEATHER,                                         !Input
     &         FRACSH)                                          !Output
          
		    FRSHAV = FRSHAV + FRACSH
		 
	    ENDIF

      ENDIF
	  
	ENDDO
C=======================================================================
      IF (NHOUR > 0.)  THEN

         
          FRSHAV = FRSHAV / NHOUR

	ENDIF
!***********************************************************************
C===Introduced to avoid excessive, very early in-canopy shading - FSR == 
      FRSHAV = MAX(FRSHAV,MINSHD)
!***********************************************************************

!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF !(DYNAMIC .EQ. 
!***********************************************************************
      RETURN
      END ! SUBROUTINE CANOPY
!=======================================================================
C=======================================================================
C  CSP_SHADOW, Subroutine, N.B. Pickering, J.W. Jones
C  Calculates fraction shaded for sun and row geometry using ellipses.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  03/14/1991 NBP Written
C  11/15/1991 NBP Modified
C  11/23/1993 NBP Included more error checks and limits
C  09/30/2007 FSR Adapted to CASUPRO sugarcane model as CSP_SHADOW
C-----------------------------------------------------------------------
C  Called from: CANOPY
C  Calls:
C=======================================================================

      SUBROUTINE CSP_SHADOW(
     &  AZIR, AZZON, BETA, BETN, CANHT, CANWH, H, ROWSPC, !Input
!    &  WEATHER,                                          !Input 
     &  FRACSH)                                           !Output

C Note: AZIR & ROWSPC read by SUBROUTINE PGINP; copy code?
C       AZZON & BETA from WEATHER constructed data type 

      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
 
      IMPLICIT NONE
      SAVE

	INTEGER H 

      REAL A,B,C1,C2,C3,C4,AZIMD,AZIR,AZZON(TS),BETA(TS),BETN,CANHT,
     &  CANWH,ETA,FRACSH,GAMMA,RBETA,ROWSPC,
     &  SHADE,SHLEN,SHPERP,STOUCH,TINCR,ZERO

      PARAMETER (ZERO=1.0E-6)
      PARAMETER (TINCR=24.0/TS)

!     Transfer values from constructed data types into local variables.
!     TYPE (WeatherType) WEATHER
!       AZZON  = WEATHER % AZZON 
!       BETA   = WEATHER % BETA   

C     Set fraction shaded to 0.1 for stalks with zero width
C     or height.  
C     Was set to zero, but that caused "underflow" errors
C     within other modules.  Output is identical, however. 

      IF (CANWH .LE. ZERO .OR. CANHT .LE. ZERO) THEN
        FRACSH = 0.1

C     Set fraction shaded to 1.0 for full cover.

      ELSE IF (CANWH .GE. ROWSPC) THEN
        FRACSH = 1.0

C     Calculate fraction shaded.

      ELSE

C       Adjust BETA if sun exactly overhead or at horizon.  Calculate
C       acute positive angle between sun and row azimuths. Initialize
C       other constants.

        RBETA = MIN(MAX(BETA(H)*RAD,1.0E-6),PI/2.0-1.0E-6)
        AZIMD = ABS(AZZON(H)-AZIR)*RAD
        IF (AZIMD .GT. PI/2.0) AZIMD = PI - AZIMD
        A = (CANWH/CANHT)**2
        GAMMA = ATAN(A*TAN(RBETA))
        C1 = A*(TAN(RBETA))**2
        C2 = (A*TAN(RBETA))**2

C       Calculate shadow length assuming elliptical plant.

        SHLEN = CANHT * COS(RBETA-GAMMA) / SIN(RBETA) *
     &    SQRT((1.0+C2)/(1.0+C1))
        B = (SHLEN/CANWH)**2
        C3 = B*(TAN(AZIMD))**2
        C4 = (B*TAN(AZIMD))**2
        STOUCH = SHLEN / (COS(AZIMD) * SQRT(1.+C3))

C       CALCULATE FRACTION SHADED.

C       Sun parallel to row.  Shadow limited to BETN.

        IF (AZIMD .LE. ZERO) THEN
          SHLEN = MIN(SHLEN,BETN)
          SHADE = 0.25 * PI * SHLEN * CANWH

C       Sun not parallel to row.

        ELSE

C         Calculate perpendicular shadow length.

          AZIMD = MAX(AZIMD,1.0E-6)
          ETA = ATAN(1.0/(B*TAN(AZIMD)))
          SHPERP = CANWH*SIN(AZIMD+ETA)*SQRT((1.0+C4)/(1.0+C3))

C         Hedgerow (plant shadows overlap).

          IF (STOUCH .GE. BETN) THEN

C           Shadow length is perpendicular and limited to ROWSPC.

            SHLEN = MIN(SHPERP,ROWSPC)
            SHADE = SHLEN * BETN

C         Individual plants.

          ELSE

C           Limit shadow length to within one ROWSPC.

            IF (SHPERP .GT. ROWSPC) SHLEN = SHLEN * ROWSPC/SHPERP
            SHADE = 0.25 * PI * SHLEN * CANWH

          ENDIF
        ENDIF

        FRACSH = MIN(SHADE/(ROWSPC*BETN),1.0)

      ENDIF

      FRACSH = MIN(MAX(FRACSH,1.0E-6),1.0)

      END !SUBROUTINE CSP_SHADOW
!=======================================================================
! CANOPY Definitions:  updated 25 Feb 2004
!-----------------------------------------------------------------
! BETN      Spacing between plants along a row (m / plant)
! C255      255-character record read from file 
! CANHT     Canopy height (m)
! CANWH     Canopy width normal to row (m)
! DeltaLeafNum(i,j) [replaces RVSTAGE]
! ECONO     Ecotype code - used to match ECOTYP in .ECO file 
! ECOTYP    Ecotype code for this simulation 
! ERR       Error code for file operation 
! FILECC    Path plus filename for species file (*.spe) 
! FILEGC    Pathname plus filename for ECO file 
! FOUND     Indicator that good data was read from file by subroutine FIND 
!             (0 - End-of-file encountered, 1 - NAME was found) 
! FRACSH    Hourly shadow fraction of ground area.  Processed into FRSHAV.
! FRSHAV    Average of daylight fraction of ground area in plant shadow 
! H         Internal hourly counter (hr)
! HPAR      Effect of day's PAR on canopy expansion 
! HS        Internal hourly counter (hour)
! HWTEM     Effect of temperature on canopy expansion 
! ISECT     Indicator of completion of IGNORE routine: 0 - End of file 
!             encountered, 1 - Found a good line to read, 2 - End of 
!             Section in file encountered denoted by * in column 1. 
! KCAN      Canopy light extinction coefficient for daily PAR, for 
!             equidistant plant spacing, modified when in-row and between 
!             row spacing are not equal 
! LINC      Line number of input file 
! LNUM      Current line number of input file 
! LUNCRP    Logical unit number for FILEC (*.spe file) 
! LUNECO    Logical unit number for FILEE (*.eco file) 
! MINSHD    Minimum value allowed for FRSHAV (ground area fraction in plant shadow)
! PAR       Daily photosynthetically active radiation or photon flux 
!             density (moles[quanta]/m2-d)
! PARNOD    Effective PAR at growth point (moles[quanta]/m2-d)
! RCANHT    Rate of increase in canopy height (m/d)
! RCANWH    Rate of increase in canopy width (m/d)
! ROWSPC    Row spacing (m)
! RVSTGE    Rate of VSTAGE change (nodes/day) [CSP_CANOPY uses DeltaLeafNum]
! SECTION   Section name in input file 
! TGRO(I)   Hourly canopy temperature (°C)
! TURFAC    Water stress factor for expansion (0 - 1) 
! VSTAGE    Number of nodes on main stem of plant (nodes) [CSP_CANOPY uses LeafNum]
! WPAR      Effect of PAR on canopy width 
! XHWPAR(I) PAR values for table look-up for modifying height and width 
!             growth rate, particularly to allow etiliolation at low PAR 
!             values (mol/day)
! XHWTEM    Temperatures in a table look-up function for modifying height 
!             and width growth rates (°C)
! XHLAI     Green leaf area (one side) per unit of ground area
!            (m2[leaf] / m2[ground])
! XVSHT     Node number on main stem for use in computing height and width 
!             growth rates 
! YHWPAR(I) Relative increase in height and width growth rates with low PAR 
!             as given in XHWPAR 
! YHWTEM(I) Relative (0-1) expansion in height and width with temperatures 
!             given in XHWTEM 
! YVSHT     Length of internode (m) Vs position on the main stem defined by 
!             XVSHT (m/node)
! YVSWH     Increase in canopy width per node developed on the main stem
!            (m/node)
!***********************************************************************
!      END SUBROUTINE CANOPY
!=======================================================================

