C=======================================================================
C  CSP_SENES, Subroutine for CASUPRO sugarcane model, based on 
C  SENES, Subroutine, K.J. Boote, J.W. Jones and G. Hoogenboom
C  Calculates leaf senescence due to natural aging, drought stress,
C  light stress, and physiological maturity.

C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  05/01/1989     Written.
C  01/20/1997 GH  Revised.
C  06/16/1998 CHP revised for modular format.
C  05/11/1999 GH  Incorporated in CROPGRO
C  06/19/2001 GH  Fix SSNDOT, SSDOT
C  11/09/2001 O.H. Daza modified for sugarcane
C  08/12/2003 CHP Added I/O error checking
C  08/27/2003 FSR Incorporated in CASUPRO for DSSAT 4.0
C  07/26/2004 CHP Removed variables which were not being used
C  07/08/2005 FSR Modified to for leaf senescence by stalk
C  12/12/2005 FSR Modified low-light senescence.
C  10/04/2007 FSR Natural senescence removed
C-----------------------------------------------------------------------
C  Called : CASUPRO
C  Calls  : ERROR, FIND, IGNORE
C========================================================================
      SUBROUTINE CSP_SENES(CONTROL, DYNAMIC,
     &    CAB, ECONO, FILECC, FILEGC,                 !Input
     &    LeafMaint, LeafNum, LFmntDEF,               !Input
     &    MinGr, Ph1P, PhenoStage,                    !Input
     &    PI1, RATTP, Smax,                           !Input
     &    StalkState, STKmntDEF, STKWT, STKWTP,       !Input
     &    SumTTStalk, SWFAC, TMIN, XLAI,              !Input
     &    YRDOY, YRPLT,                               !Input
     &    Kill, SLDOT, SLNDOT, SSDOT, SSNDOT)         !Output

!OHD -  Include senescence of roots as SRDOT, SRNDOT
!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
      USE ModuleData

      IMPLICIT NONE
      EXTERNAL ERROR, FIND, GETLUN, IGNORE, TABEX, TIMDIF
      SAVE

      CHARACTER*1  RNMODE  
      CHARACTER*4 StalkState(NumOfStalks,10)
      CHARACTER*6  ECOTYP, ECONO, ERRKEY, SECTION
      CHARACTER*30 FILEIO
      CHARACTER*80 CHAR
      CHARACTER*92 FILECC, FILEGC
      PARAMETER (ERRKEY = 'SENES')

      INTEGER I, II, CAB, LUNCRP, LUNECO, ERR, ISECT, LNUM, Zone 
      INTEGER DYNAMIC, Smax, Stalk, TIMDIF
      INTEGER DAS, YRDOY, YRPLT !, YRSIM
      INTEGER FOUND
      INTEGER NSWAB
	INTEGER DAP, REPNO, RUN, LUN
      !INTEGER WLUN

      INTEGER, DIMENSION(1:NumOfStalks) :: Kill
	INTEGER, PARAMETER :: CanopyLayers=3

      PARAMETER (NSWAB = 5)

      REAL a  !, GRLF, GRSU  
      REAL MinGr, Ph1P, PI1 !KCAN
      REAL PORLOST, RATTP 
      REAL SENDAY, SLNDOT   
	REAL SSDOT, SSNDOT, StkSenFrac, SWFAC   
      REAL TABEX, TMIN  
      REAL WatSEN, XLAI  
	REAL SWFCAB(NSWAB)
	REAL SENMAX(4), ShdMAX(4), SENPOR(4), YLOSS(4)
	REAL XSENMX(4), XShdMX(4), XSTAGE(4), XTMIN(4)

	REAL, DIMENSION(1:NumOfStalks) :: SenLfCld, 
     &                SenLfNat, SenLfShd, SenLfWat, SLDOT,  
     &                TotalDEF, CanopyMaint  !SuDEF, 
	REAL, DIMENSION(0:NumOfDays) :: STKWTP          
	REAL, DIMENSION(0:NumOfDays, NumOfStalks) :: LeafNum, 
     &                STKmntDEF, STKWT, SumTTStalk  !LeafArea, 
	REAL, DIMENSION(1:NumOfStalks,CanopyLayers) :: CanopySenes,
     &                LeafMaint, LFmntDEF 
	!LOGICAL FEXIST
	INTEGER OpenStatus, PhenoStage
!-----------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.
      TYPE (ControlType) CONTROL
      TYPE (SwitchType) ISWITCH

      YRDOY   = CONTROL % YRDOY
      DAS     = CONTROL % DAS
      DYNAMIC = CONTROL % DYNAMIC
      FILEIO  = CONTROL % FILEIO
      REPNO   = CONTROL % REPNO
      RUN     = CONTROL % RUN
      RNMODE  = CONTROL % RNMODE
      YRDOY   = CONTROL % YRDOY
!!    IDETW   = ISWITCH % IDETW
!-----------------------------------------------------------------------
	
! Days after planting
      DAP = MAX(0,TIMDIF(YRPLT,YRDOY))

!***********************************************************************
!***********************************************************************
!     Run Initialization - Called once per simulation
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
!-----------------------------------------------------------------------
      CALL GET(ISWITCH)
      IF (INDEX('Y'  ,ISWITCH%IDETG) > 0 .AND. 
     &    INDEX('YDA',ISWITCH%IDETL) > 0) THEN

	  ! Open temp file to write results from CSP_PHENOL.for
        CALL GETLUN('CSP_SENES', LUN)
        OPEN(UNIT = LUN, FILE = "CSP_SENES.OUT", STATUS = "UNKNOWN",
     &    ACTION = "WRITE", POSITION = "REWIND", IOSTAT = OpenStatus)
      ENDIF
!-----------------------------------------------------------------------
!     Open FILEE (.ECO or EcoType) Input file
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

			CALL IGNORE(LUNECO,LNUM,ISECT,CHAR) ! KCAN_ECO 
			CALL IGNORE(LUNECO,LNUM,ISECT,CHAR) ! SMAX
			CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)
			CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)
			CALL IGNORE(LUNECO,LNUM,ISECT,CHAR) ! LfShdFac
			CALL IGNORE(LUNECO,LNUM,ISECT,CHAR) ! MINSHD

			CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)
			READ(CHAR,'(14X,F6.5)',IOSTAT=ERR) StkSenFrac 
			IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEGC,LNUM)

			CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)
			READ(CHAR,'(14X,F6.2)',IOSTAT=ERR) SENDAY
 			IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEGC,LNUM)
	
			CALL IGNORE(LUNECO,LNUM,ISECT,CHAR) !XDAY(6)
			CALL IGNORE(LUNECO,LNUM,ISECT,CHAR) !YSLA(6)
			CALL IGNORE(LUNECO,LNUM,ISECT,CHAR) ! XLI
			CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)
			CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)
			CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)
			CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)
			CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)
			CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)
			CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)
			CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)
			CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)
			CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)
			CALL IGNORE(LUNECO,LNUM,ISECT,CHAR) ! YFRSU

			CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)
			READ(CHAR,'(14X,4F6.2)',IOSTAT=ERR) (XSENMX(II),II=1,4)
 			IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEGC,LNUM)

			CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)
			READ(CHAR,'(14X,4F6.2)',IOSTAT=ERR) (SENMAX(II),II=1,4)
 			IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEGC,LNUM)

			CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)
			READ(CHAR,'(14X,4F6.2)',IOSTAT=ERR) (XShdMX(II),II=1,4)
 			IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEGC,LNUM)

			CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)
			READ(CHAR,'(14X,4F6.3)',IOSTAT=ERR) (ShdMAX(II),II=1,4)
 			IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEGC,LNUM)

			CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)
			READ(CHAR,'(14X,4F6.2)',IOSTAT=ERR) (XSTAGE(II),II=1,4)
 			IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEGC,LNUM)

			CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)
			READ(CHAR,'(14X,4F6.4)',IOSTAT=ERR) (SENPOR(II),II=1,4)
 			IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEGC,LNUM)

			CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)
			READ(CHAR,'(14X,4F6.2)',IOSTAT=ERR) (XTMIN(II),II=1,4)
 			IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEGC,LNUM)

			CALL IGNORE(LUNECO,LNUM,ISECT,CHAR)
			READ(CHAR,'(14X,4F6.2)',IOSTAT=ERR) (YLOSS(II),II=1,4)
 			IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEGC,LNUM)

      ENDIF ! (ERR .NE. 0)
      CLOSE (LUNECO)  ! Ecotype Parameter File

!-----------------------------------------------------------------------
!     Open FILEC (.SPE or Species) Input file
!-----------------------------------------------------------------------
      CALL GETLUN('FILEC', LUNCRP)
      OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,0)
      LNUM = 0
!***********************************************************************
! Echoes input data  (OHD debugging and calibrating code)

! Open file to write results from CSP_SENES
!      CALL GETLUN('WORK.OUT',WLUN)
!      OPEN(UNIT = WLUN, FILE = "WORK.OUT", STATUS = "UNKNOWN",
!     &   ACTION = "WRITE", POSITION = "APPEND", IOSTAT = OpenStatus)

!      WRITE(WLUN,*)
!      WRITE(WLUN,*)
!      WRITE(WLUN,'(1X,"RESULTS FROM CSP_SENES.for")')
!      WRITE(WLUN,'(1X,"-------------------------")')
!      WRITE(WLUN,*)
!      WRITE(WLUN,'(1X,"*** FILECC : ",A80)') FILECC

      SECTION = '*#PHOT'  
!      WRITE(WLUN,*)
!      WRITE(WLUN,'(1X,"SECTION: ",A6)') SECTION
!      WRITE(WLUN,'(1X,"KCAN   : ",F6.2)') KCAN

      SECTION = '*SENES'
!      WRITE(WLUN,*)
!      WRITE(WLUN,'(1X,"SECTION: ",A6)') SECTION
!      WRITE(WLUN,'(1X,"SENDAY : ",F6.2)') SENDAY

!      WRITE(WLUN,*)
!      WRITE(WLUN,'(1X,"I      : ",4I6)') (I,I=1,4)
!      WRITE(WLUN,'(1X,"XSTAGE : ",4F6.2)') (XSTAGE(II),II=1,4)
!      WRITE(WLUN,'(1X,"XSENMX : ",4F6.2)') (XSENMX(II),II=1,4)
!      WRITE(WLUN,*)
!      WRITE(WLUN,'(1X,"I      : ",4I6)') (I,I=1,4)
!      WRITE(WLUN,'(1X,"SENPOR : ",4F6.2)') (SENPOR(II),II=1,4)
!      WRITE(WLUN,'(1X,"SENMAX : ",4F6.2)') (SENMAX(II),II=1,4)

!      WRITE(WLUN,*)
!      WRITE(WLUN,'(1X,"END RESULTS FROM CSP_SENES.for")')

!      CLOSE (WLUN)

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      SSDOT  = 0.0  !Senescence of stalks
      SLNDOT = 0.0  !Senescence of leaves due to water
      SSNDOT = 0.0  !Senescence of stalks due to water
      RATTP  = 1.0

      DO I = 1,5
        SWFCAB(I) = 1.0
      ENDDO

      DO Stalk = 1,Smax
       ! Initialization of senescence variables for each stalk
		Kill(Stalk)       = 0
		SLDOT(Stalk)      = 0.
		SenLfCld(Stalk)   = 0. 
		SenLfNat(Stalk)   = 0.
		SenLfShd(Stalk)   = 0. 
		SenLfWat(Stalk)   = 0. 
		TotalDEF          = 0.
      END DO  ! Stalk

!-----------------------------------------------------------------------
! Open output file to write results from CSP_SENES.for
!-----------------------------------------------------------------------
      IF (INDEX('Y'  ,ISWITCH%IDETG) > 0 .AND. 
     &    INDEX('YDA',ISWITCH%IDETL) > 0) THEN

!       Variable heading for CSP_SENES.OUT
        WRITE(LUN,'(1X,"RESULTS FROM CSP_SENES.for")')
        WRITE(LUN,*)
	  
        WRITE(LUN,'(1X,"All senescense values represent the percentage
     &   of total leaf dry weight lost daily")')
        WRITE(LUN,*)

        WRITE(LUN,'(1X,"  YRDOY DAS DAP PHSTG   LAI")',
     &    ADVANCE="NO")

	  DO Stalk = 1,Smax
          WRITE(LUN,'("  SLDOT",I2)',ADVANCE="NO") Stalk
        END DO
        
	  DO Stalk = 1,Smax
          WRITE(LUN,'(" SenLfNat",I2)',ADVANCE="NO") Stalk
        END DO
        
	  DO Stalk = 1,Smax
          WRITE(LUN,'(" SenLfShd",I2)',ADVANCE="NO") Stalk
        END DO
              
	  DO Stalk = 1,Smax
          WRITE(LUN,'(" SenLfWat",I2)',ADVANCE="NO") Stalk
        END DO
              
	  DO Stalk = 1,Smax
          WRITE(LUN,'(" SenLfCld",I2)',ADVANCE="NO") Stalk
        END DO
      ENDIF
C-----------------------------------------------------------------------

!***********************************************************************
!***********************************************************************
!     DAILY RATE/INTEGRATION
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------

!     DAS   = MAX(0,TIMDIF(YRSIM,YRDOY))

C   Initialize variables
      DO Stalk = 1,Smax
		SenLfCld(Stalk) = 0. 
!!		SenLfNat(Stalk) = 0.
		SenLfShd(Stalk) = 0.
		SenLfWat(Stalk) = 0.
		SLDOT(Stalk)    = 0.
	    CanopyMaint(Stalk) = 0.

		DO Zone = 1,3
		  CanopySenes(Stalk,Zone) = 0.
!!!            Not sure when or why the following variables
!!!	         were initialized here, but removed 01-14-09 (FSR)
!!!		  LeafMaint(Stalk,Zone)   = 0. 
!!!		  LFmntDEF(Stalk,Zone)    = 0.

		ENDDO
      ENDDO

      !Update value of RATTP.  SWFCAB(I) is a record of last 5 days of 
      !water stress, which is applied with a lag of NSWAB 
      DO I = NSWAB,2,-1
        SWFCAB(I) = SWFCAB(I-1)
      ENDDO
      SWFCAB(1) = SWFAC
      RATTP = SWFCAB(NSWAB)
      
C-----------------------------------------------------------------------
C     This section calculates natural (aging) senescence by stalk - FSR
C     Natural senescence removed at suggestion of K. Boote - FSR
C-----------------------------------------------------------------------
!      DO Stalk = 1,Smax        
!	    IF (StalkState(Stalk,1) .EQ. 'LIVE') THEN 
!
!          SenLfNat(Stalk) = TABEX(SENPOR,XSTAGE,LeafNum(DAS,Stalk),4)
!
!	    END IF 	   
!
!	END DO
!-----------------------------------------------------------------------
!     This section calculates senescence due to low light in lower
!     canopy as the most important driver of leaf area senescence in non-
!     stressed sugarcane: "Observations of differences in leaf senescence 
!     between edge rows and inner rows in field imply that the main factor
!     limiting green leaf number in non-water/nutrient stressed sugarcane 
!     is the lack of radiation in the lower canopy layer."  
!     [Liu & Bull (2001) pg 191, Simulation of biomass and sugar 
!     accumulation in sugarcane using a process-based model. Ecological 
!     Modelling 144, pp 181-211].
!
!     Based on maintenance respiration deficit (STKmntDEF) and 
!     limited by the maximum daily proportion of total leaf loss due to 
!     shading.  Does not use "light compensation" method from CropGro. 
!     - FSR
! 
	DO Stalk = 1,Smax

        IF (StalkState(Stalk,1) .EQ. 'LIVE') THEN  

          PORLOST = TABEX(ShdMAX, XShdMX, LeafNum(DAS,Stalk), 4)

	! Sum all zones into CanopyMaint, so that ratio 'a' is based
	! on entire canopy maint resp demand, and summing of PORLOST 
	! does not result in exceeding intended daily maximum. 

		DO Zone = 1,3
		   CanopyMaint(Stalk) 
     &				= CanopyMaint(Stalk) + LeafMaint(Stalk,Zone)
	 	END DO


		DO Zone = 1,3

			IF (LFmntDEF(Stalk,Zone) < 0 
     &            .AND. CanopyMaint(Stalk) > 0 ) THEN
		           		           
			 a = LFmntDEF(Stalk,Zone)/CanopyMaint(Stalk)  
				       
               CanopySenes(Stalk,Zone) = MIN(PORLOST, a * (-1.0))  
                                                  !for portability
			 SenLfShd(Stalk)=SenLfShd(Stalk)+CanopySenes(Stalk,Zone)

     &          
			END IF  !(LFmntDEF(Stalk,Zone) < 0)

	 	END DO
	  END IF !(StalkState(Stalk,1) .EQ. 'LIVE')
	
	END DO
C-----------------------------------------------------------------------
C     This section calculates senescence due to cold temperatures:
C     frost or freezing. - FSR 

	IF (TMIN .LE. XTMIN(4)) THEN
			PORLOST = TABEX(YLOSS, XTMIN, TMIN, 4)

          DO Stalk = 1,Smax
			SenLfCld(Stalk) = PORLOST  	   
		END DO

	END IF 
	

C-----------------------------------------------------------------------
C     Calculate senescence due to water stress.
C      Water stress was linked to each stalk by substituting  
C	 LeafNum(DAS,Stalk) for VSTAGE.  FSR

!   Original CropGro code revised to leave senescence as a fraction, 
!   and to senesce according to the leaf number on each stalk.
!   Old code: 
!!!		WatSEN = SENDAY * (1. - RATTP)
!!!		IF (WatSEN .GT. 0.0) THEN
!!!	    PORLOST = TABEX(SENMAX, XSENMX, VSTAGE, 4)
!!!		  WatSEN = MIN(WatSEN, PORLOST)
!!!		  WatSEN = MAX(WatSEN, 0.0)

!!!          DO Stalk = 1,Smax
!!!			SenLfWat(Stalk) = WatSEN  	   
!!!		END DO
!!!	END IF 

      DO Stalk = 1,Smax  
	    IF (StalkState(Stalk,1) .EQ. 'LIVE') THEN       
		    WatSEN = SENDAY * (1. - RATTP)

	 	   IF (WatSEN .GT. 0.0) THEN
			  PORLOST = TABEX(SENMAX, XSENMX, LeafNum(DAS,Stalk), 4)
			  WatSEN = MIN(WatSEN, PORLOST)
			  WatSEN = MAX(WatSEN, 0.0)
	          SenLfWat(Stalk) = WatSEN
	       END IF ! WatSEN .GT. 0.0

	     END IF  ! StalkState(Stalk,1) .EQ. 'LIVE'

	END DO  ! Stalk = 1,Smax

! PORLOST Proportion of leaf weight that may be senesced  		
! RATTP   Factor used in determining increased senescence due to water 
!         stress 
! SENDAY  Maximum fraction of existing leaf weight which can be senesced 
!         on day N as a function of severe water stress 4 days earlier. 
! WTLF    Dry mass of leaf tissue including C and N (g[leaf] / m2[ground])
!-----------------------------------------------------------------------
! Total daily leaf senescence
! SLDOT     Defoliation due to daily leaf senescence
!           (proportion of leaf area senesced, by stalk)
	 
      DO Stalk = 1,Smax

	 SLDOT(Stalk)= 
     &		MAX(SenLfShd(Stalk), SenLfWat(Stalk), SenLfCld(Stalk)) 
			
	END DO
!-----------------------------------------------------------------------
! Stalk Senescence 
!-----------------------------------------------------------------------
! STKmntDEF(i,j) CH2O deficit for maintenance respiration requirement,
!              for day i and stalk j. Used also for calculating 
!              senescence.  (g[CH2O] / stalk / day) 
! TotalDEF(j) Accumulation of daily maintenance respiration deficit.
!              for stalk j  (<=0)
		 
!  Kill(Stalk) =  0 when stalk is alive
!              =  1 when stalk is killed
!              = -1 when stalk is dead and counted
!
      DO Stalk = 1,Smax
	 IF (StalkState(Stalk,1) .EQ. 'LIVE') THEN       
	  TotalDEF(Stalk) = TotalDEF(Stalk) + STKmntDEF(DAS,Stalk)

	  IF (STKWT(DAS-1,Stalk) > 0. .AND. Kill(Stalk) .NE. -1) THEN

	    IF ((-TotalDEF(Stalk) / STKWT(DAS-1,Stalk) >= StkSenFrac) 
! The above IF statement is the main test for stalk senescence, but a
! problem was found where well-established stalks with some 20 nodes
! lingered with a miniscule, but above 0, weight, and little or no 
! leaf area.  So the following additional test was inserted to kill
! stalks if they reach early adulthood (TT beyond CAB X 2 leaves) with 
! a minimum weight (MinGr).  
     &	  .OR. (STKWT(DAS-1,Stalk) .LT. MinGr   .AND.       
     &       (SumTTStalk(DAS,Stalk) - Ph1P) > 2 * CAB * PI1)) THEN
		 
	          Kill(Stalk) = 1

	       ELSE 
                Kill(Stalk) = 0
	    END IF

! Following code is for the case where stalk never grows, so never
! gains STKWT, yet never dies (protected from 'Kill' by the
! "IF (STKWT(DAS-1,Stalk) > 0." statement above.  Ph1P is the  
! threshold thermal time to sprouting and is therefore an appropriate 
! adjustment for the comparison between accumulated thermal time and
! leaf number.   

		ELSE IF (STKWT(DAS-1,Stalk) .EQ. 0.          .AND. 
     &             (SumTTStalk(DAS,Stalk) - Ph1P) > CAB * PI1) THEN
	       Kill(Stalk) = 1	

! The following prevents stalks in water scarce conditions from growing
! in very small incrementsand and surviving as nearly microscopic units
! during the LeafNum < CAB stage.  (after adding Ratoon Senescence, this
! does not seem to be necessary - FSR)

!!!		ELSE IF (STKWT(DAS-1,Stalk) .EQ. 0.      .AND. 
!!!     &	 LeafNum(DAS,Stalk) .GE. CAB             .AND.
!!!     &     LeafArea(DAS,Stalk)/LeafNum(DAS,Stalk) < 1.5) THEN
!!!		     Kill(Stalk) = 1

	  END IF !(STKWT(DAS-1,Stalk) > 0. .AND. Kill(Stalk) .NE. -1)
	 END IF !(StalkState(Stalk,1) .EQ. 'LIVE')
		 
      END DO  ! Stalk

!-----------------------------------------------------------------------
! Plant Senescence  (note: try eliminating this section FSR)
!-----------------------------------------------------------------------
! Entire plant dies if total stalk weight falls to 0 during 
! phenostage 4 or 5.
! Note PhnoStage 4 begins about 3 months after planting. 

      DO Stalk = 1,Smax
	 IF (StalkState(Stalk,1) .EQ. 'LIVE'  .AND. 
     &     PhenoStage > 3                   .AND. 
     &     STKWTP(DAS) .EQ. 0.0)  THEN

	 	    Kill(Stalk) = 1
	  
	 END IF !(StalkState(Stalk,1) .EQ. 'LIVE') etc
      END DO  ! Stalk
!-----------------------------------------------------------------------
! Set canopy zone maintenance deficits to zero for recently dead stalks.
! Note that there may be other variables to set to zero (FSR).
!-----------------------------------------------------------------------
      DO Stalk = 1,Smax
		IF (Kill(Stalk) .EQ. 1)  THEN	 	    

			LFmntDEF (Stalk,1) = 0.
			LFmntDEF (Stalk,2) = 0.
			LFmntDEF (Stalk,3) = 0.
	  
		END IF !((Kill(Stalk) .EQ. 1)
      END DO  ! Stalk

!-----------------------------------------------------------------------
! Root senescence is calculated in subroutine CSP_ROOTS.for
!-----------------------------------------------------------------------
!***********************************************************************
!     Daily Output
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT) THEN
!**********************************************************************
!	Write growth details by stalk to output file CSP_SENES.OUT - FSR 

      IF (INDEX('Y'  ,ISWITCH%IDETG) > 0 .AND. 
     &    INDEX('YDA',ISWITCH%IDETL) > 0) THEN

        WRITE(LUN,'(/1X,I7,1X,I3,1X,I3,4X,I2,1X,F5.2)',
     &      ADVANCE="NO"), 
     &      YRDOY, DAS, DAP, PhenoStage, XLAI

        DO Stalk = 1,Smax     
	    IF (Kill(Stalk) > -1) THEN !generates zeros in columns from DAS=0
            WRITE(LUN,'(1X,F8.4)', ADVANCE="NO") SLDOT(Stalk)*100
          ELSE
	      WRITE(LUN,'(9X)', ADVANCE="NO") 
	    END IF
        END DO
!
        DO Stalk = 1,Smax   
	    IF (Kill(Stalk) > -1) THEN 
            WRITE(LUN,'(5X,F6.4)', ADVANCE="NO") SenLfNat(Stalk)*100
          ELSE
	      WRITE(LUN,'(11X)', ADVANCE="NO") 
	    END IF
        END DO
!
        DO Stalk = 1,Smax  
	    IF (Kill(Stalk) > -1) THEN 
            WRITE(LUN,'(F11.3)', ADVANCE="NO") SenLfShd(Stalk)*100
          ELSE
	      WRITE(LUN,'(11X)', ADVANCE="NO") 
	    END IF
        END DO
!
        DO Stalk = 1,Smax  
	    IF (Kill(Stalk) > -1) THEN 
            WRITE(LUN,'(5X,F6.3)', ADVANCE="NO") SenLfWat(Stalk)*100
          ELSE
	      WRITE(LUN,'(11X)', ADVANCE="NO")
	    END IF 
        END DO

        DO Stalk = 1,Smax     
	    IF (Kill(Stalk) > -1) THEN !generates zeros in columns from DAS=0
            WRITE(LUN,'(3X,F8.4)', ADVANCE="NO") SenLfCld(Stalk)*100
          ELSE
	      WRITE(LUN,'(11X)', ADVANCE="NO") 
	    END IF
        END DO
      ENDIF
!-----------------------------------------------------------------------
!**********************************************************************

      !Close daily output files.
!***********************************************************************
!     Seasonal Output
!***********************************************************************
        IF (DYNAMIC .EQ. SEASEND) THEN
C-----------------------------------------------------------------------

          CLOSE (LUN)
        ENDIF

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END ! SUBROUTINE CSP_SENES
!***********************************************************************
!     SENES VARIABLE DEFINITIONS:
!-----------------------------------------------------------------------
! CanopyMaint(i) Leaf maintenance respiration demand for sum of canopy zones.
! CanopySenes(i,j) per/day percentage or proportion loss of leaf weight
!	      for stalk i, canopy zone j.  Total loss for three canopy
!           zones sums to  SenLfShd(j)  
! CHAR      Contains the contents of last record read 
! CLW       Cumulative leaf growth (g[leaf]/m2)
! DAS       Days after start of simulation (days)
! DTX       Thermal time that occurs in a real day based on vegetative 
!             development temperature function (thermal days / day)
! DYNAMIC   Module control variable; =RUNINIT, SEASINIT, RATE, EMERG, 
!             INTEGR, OUTPUT, or FINAL 
! ERR       Error code for file operation 
! FILECC    Path plus filename for species file (*.spe) 
! FOUND     Indicator that good data was read from file by subroutine FIND 
!             (0 - End-of-file encountered, 1 - NAME was found) 
! GRSU      Growth respiration costs, g[CH2O] / g[sucrose]
! ISECT     Data record code (0 - End of file encountered, 1 - Found a good 
!             line to read, 2 - End of Section in file encountered, denoted 
!             by * in column 1
! KCAN      Canopy light extinction coefficient for daily PAR, for 
!             equidistant plant spacing, modified when in-row and between 
!             row spacings are not equal 
! LNUM      Current line number of input file 
! LUNCRP    Logical unit number for FILEC (*.spe file) 
! STKmntDEF(i,j) CH2O deficit for maintenance respiration requirement, for
!             day i and stalk j. Relevant for senescence.
!             (g[CH2O] / stalk / day)
! NRUSLF    N actually mobilized from leaves in a day (g[N]/m2-d)
! PAR       Daily photosynthetically active radiation or photon flux 
!             density (moles[quanta]/m2-d)
! PORLFT    Proportion of leaf weight grown which will have been senesced 
!             if no water stress has occurred prior to this V-stage 
! PORLOST    Proportion of leaf weight which may be senesced. 
! RATTP     Factor used in determining increased senescence due to water 
!             stress 
! RHOL      Fraction of leaf which is carbohydrate (g [CH20] / g[leaf])
! SECTION   Section name in input file 
! SENDAY    Maximum fraction of existing leaf weight which can be senesced 
!             on day N as a function of severe water stress 4 days earlier. 
! SenLfNat(j)-percentage or proportion loss to per/day rates of leaf
!			growth on stalk j due to natural aging (FSR)
! SenLfShd(j)-percentage or proportion loss to per/day rates of leaf
!			growth on stalk j due to shading (FSR)
! SenLfWat(j)-percentage or proportion loss to per/day rates of leaf
!			growth on stalk j due to water stress (FSR)
! SENMAX(I) Maximum proportion of total leaf weight as a function of 
!             V-stage (XSENMX(I)) which can be senesced due to water stress. 
! SENPOR(I) Proportion of leaf weight grown which will have been senesced 
!             by a given V- stage (XSTAGE(I)) if no water stress has 
!             occurred prior to this V-stage (XSTAGE(I)) -- normal 
!             vegetative senescence does not occur if prior water stress 
!             has already  reduced leaf  
! SLAAD     Specific leaf area, excluding weight of C stored in leaves
!             (cm2[leaf] / g[leaf])
! SLDOT     Defoliation due to daily leaf senescence
!           (proportion of leaf area senesced, by stalk)
! SSDOT     Daily senescence of stalks (g/m2/d)
! SSNDOT    Stalk senescence due to water stress (g/m2/d)
! StalkState(j,k)  Condition k of stalk j.  Currently, conditions are:
!                  k = 1 (LIVE or DEAD); 2 (PRIM or TILR); 3-10 unused          
! StkSenFrac Threshold maintenance respiration deficit to stalk weight ratio 
!           (TotalDEF(Stalk) / STKWT(DAS-1,Stalk) at which stalk death occurs.
! STMWT     Dry mass of stem tissue, including C and N
!             (g[stem] / m2[ground])
! SuDEF(j)   sucrose used for maintenance respiration when PG is insufficient. 
!            by stalk j   (g[sucrose]/stalk / day)
! TABEX     Function subroutine - Lookup utility 
! TIMDIF    Integer function which calculates the number of days between 
!             two Julian dates (da)
! TotalDEF(i,j) Accumulation of daily maintenance respiration deficit (zero or negative)
! VSTAGE    Number of nodes on main stem of plant 
! WatSEN    Proportion of leaf weight senesced due to water stress
! WTLF      Dry mass of leaf tissue including C and N
!             (g[leaf] / m2[ground])
! XLAI      Leaf area (one side) per unit of ground area
!             (m2[leaf] / m2[ground])
! XSENMX(I) V-stage at which maximum fraction of cumulative leaf growth 
!             vulnerable to loss due to water stress is SENMAX(I).
!             (# leaf nodes)
! XSTAGE(I) V-stage at which SENPOR(I) fraction of cumulative leaf growth 
!             will have been senesced if no water stress occurred.
!             (# leaf nodes)
! YRDOY     Current day of simulation (YYDDD)
! YRSIM     Start of simulation date (YYDDD)
!-----------------------------------------------------------------------
!     END SUBROUTINE CSP_SENES
!-----------------------------------------------------------------------