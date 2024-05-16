C=======================================================================
C  CSP_OPHARV, Subroutine G. Hoogenboom, J. W. Jones
C  Generates output for seasonal data.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/01/1990 GH  Written
C  11/02/1999 CHP Changed TSOC to THUMC, TSIN to THUMN, AMTRES to CUMRES 
C  07/01/2000 GH  Eliminated common block statements
C  03/03/2002 GH  Modified logic for reading fileA
C  06/11/2002 GH  Modified for Y2K
C  08/12/2003 CHP Added I/O error checking and changed call to READA
!  05/03/2004 CHP Added P stresses to OPVIEW call 
!  10/24/2005 CHP Added environmental & stress factors to Overview.OUT
C  02/09/2007 GH  Add path for FileA
!  02/11/2010 CHP Added EDAT to output
C=======================================================================

      SUBROUTINE CSP_OPHARV(CONTROL, ISWITCH, 
     &    AGEFAC, BADMD, TillerCount, LAIMX,              !Input
     &    LAIXD, LeafNum, LFWTHa, LSWTHa,                 !Input
     &    HARVFRAC,NSTRES, PLTPOP, PLWT, PStres1,         !Input
     &    PStres2, PhenoStage, StalkPopul, StalkState,    !Input 
     &    STGDOY, STKFWTHa, StkHt, STKWTHa,               !Input
     &    STNAME, SUWTHa, SWFAC, TOPWT, TURFAC,           !Input
     &    WTNCAN, WTNUP, XLAI, YRPLT)                     !Input

C-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      EXTERNAL GETDESC, OPVIEW, READA, READA_Dates, CHANGE_DESC, 
     &   SUMVALS, EvaluateDat, ERROR, TIMDIF, INCDAT, READA_Y4K
      SAVE

      CHARACTER*1  RNMODE,IDETO,IPLTI
      CHARACTER*6, PARAMETER :: ERRKEY = 'OPHARV'
      CHARACTER*10 STNAME(20)
      CHARACTER*12 FILEA
      CHARACTER*30 FILEIO
	CHARACTER*80 PATHEX
      CHARACTER*4 StalkState(NumOfStalks,10)

      INTEGER ACOUNT, count, DAP, DAS, Stalk, StkDecDay 
      INTEGER DYNAMIC, EDAT0, EDAT1, EDAT2    
      INTEGER MaxStkDay, ERRNUM, TillerCount, ISENS, LAIXD
      INTEGER LNUM, LUNIO, RUN, SPDAT, YIELD
      INTEGER YRDOY, YRPLT,YRSIM, YREMRG, INCDAT
      INTEGER PhenoStage
      INTEGER TIMDIF, TRT_ROT
      INTEGER STGDOY(20)
!   Local variables used in conversion of FileA YRDOY char inputs
!     to DAP output integers.
      INTEGER DEM0, DEM1, DEM2, DLFX, DPOP, DSPT, DTMX 
      INTEGER IEM0, IEM1, IEM2, ILFX, IPOP, ISPT, ITMX   

      REAL AvgHeight, AvgLfAr, AvgNode !AELH, 
	REAL AvgLfWt, BADMD, BIOMAS, BWAH !, CANHT
      REAL CWAM, HeightSumA, HeightSumP, HIAM
	REAL HWAH, HWAM, LAIH, LAIMX, LFNUM
      REAL MaxStkPop, NodeSum, PLTPOP, PLWT 
	REAL SNAH, SMFMH, SSTKH, SUCH, TOPWT, TRSH !, VSTAGE
      REAL WTNCAN, WTNUP, XLAI !, WTNST
      REAL, DIMENSION(2) :: HARVFRAC
      REAL, DIMENSION(0:NumOfDays) :: LFWTHa, LSWTHa, 
     &      STKFWTHa, STKWTHa, SUWTHa,StalkPopul
	REAL, DIMENSION(1:NumOfStalks) :: StkHt
      REAL, DIMENSION(0:NumOfDays,NumOfStalks) :: LeafNum

!     Arrays which contain data for printing in SUMMARY.OUT file
!       (OPSUM subroutine)
      INTEGER, PARAMETER :: SUMNUM = 10
      CHARACTER*4, DIMENSION(SUMNUM) :: LABEL
      REAL, DIMENSION(SUMNUM) :: VALUE

!     Arrays which contain Simulated and Measured data for printing
!       in OVERVIEW.OUT and EVALUATE.OUT files (OPVIEW subroutine)
      CHARACTER*6 OLAB(40), OLAP(40)  !OLAP modified for dap
      CHARACTER*12  X(40)
      CHARACTER*8 Simulated(40), Measured(40)
      CHARACTER*50 DESCRIP(40)

!     P module
      REAL PStres1, PStres2

!     Variables added for environmental and stress factors output
      REAL AGEFAC, NSTRES, SWFAC, TURFAC
      TYPE (PlStresType) PlantStres

!-----------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.
      TYPE (ControlType) CONTROL
      TYPE (SwitchType) ISWITCH

!     Transfer values from constructed data types into local variables.
      DAS    = CONTROL % DAS
      DYNAMIC= CONTROL % DYNAMIC
      FILEIO = CONTROL % FILEIO
      LUNIO  = CONTROL % LUNIO
      RUN    = CONTROL % RUN
      RNMODE = CONTROL % RNMODE
      YRDOY  = CONTROL % YRDOY
      YRSIM  = CONTROL % YRSIM

      IDETO = ISWITCH % IDETO
      IPLTI = ISWITCH % IPLTI

      ACOUNT = 21  !Number of possible FILEA headers for this crop

!     Define headings for observed data file (FILEA)
      DATA OLAB / !  Definition
               !     -----------
     & 'SMFMH ', ! 1  Millable cane fresh weight at harvest, (t/ha)
     & 'SUCH  ', ! 2  Sucrose at harvest (t/ha) 
     & 'SSTKH ', ! 3  Stalk (structure, no sucrose) dry wt @ harv (t/ha)
     & 'BADMH ', ! 4  Above-ground biomass (live+dead) at harvest (t/ha)
     & 'TRSH  ', ! 5  Above-ground biomass minus stalks harvested (t/ha)
     & 'LAIX  ', ! 6  Max LAI this season (m2[leaf]/m2[ground])
     & 'LAIXD ', ! 7  Day of max leaf area index (DAP)
     & 'LAIH  ', ! 8  Leaf area index, at harvest (m2[leaf]/m2[ground])
     & 'LAASH ', ! 9  Leaf area: avg leaf area /stalk @ harv (cm2/stalk)
     & 'LWASH ', ! 10 Leaf weight: avg leaf wt / stalk @ harv (g/stalk)
     & 'NAPSH ', ! 11 Node number: avg nodes per stalk @ harvest 
     & 'SPDAT ', ! 12 Sprouting day, primary stalk bud from sett  (DAP)
     & 'EDAT0 ', ! 13 Primary stalk emergence day (DAP)
     & 'EDAT1 ', ! 14 First tiller emergence day (DAP) 
     & 'EDAT2 ', ! 15 Second tiller emergence day (DAP)
     & 'T#MX  ', ! 16 Tiller maximum population (stalks/m2)
     & 'T#MXD ', ! 17 Day of tiller maximum population (DAP)
     & 'TDECD ', ! 18 Initiation day, tiller population decline (DAP)
     & 'S#AH  ', ! 19 Stalk population at harvest (stalks/m2)
     & 'SHTH  ', ! 20 Stalk height: average at harvest (m) 
     & 'HIAM  ', ! 21 Harvest index [sucrose/(stalk+sucrose)dm] 
     & 19*'      '/  !19 labels of 40 not used for sugarcane

!***********************************************************************
!***********************************************************************
!     RUN INITIALIZATION
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
C-----------------------------------------------------------------------
!     Read FILEIO
      OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT=ERRNUM)
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,0)

      READ (LUNIO,'(55X,I5)',IOSTAT=ERRNUM) ISENS; LNUM = 1   
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)
      READ (LUNIO,'(3(/),15X,A12,1X,A80)',IOSTAT=ERRNUM) FILEA,
     &     PATHEX
      LNUM = LNUM + 4
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,0)
  
      CLOSE (LUNIO)

!     Assign descriptions to Measured and Simulated data 
!         from DATA.CDE.
      CALL GETDESC(ACOUNT, OLAB, DESCRIP)
      OLAP = OLAB

      CALL OPVIEW(CONTROL, 
     &    BIOMAS, ACOUNT, DESCRIP, IDETO, LFNUM, 
     &    Measured, PlantStres, Simulated, STGDOY, 
     &    STNAME, WTNCAN, XLAI, YIELD, YRPLT, PhenoStage)

C***********************************************************************
C***********************************************************************
C     SEASONAL INITIALIZATION
C***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
C-----------------------------------------------------------------------
      Simulated = ' '
      Measured  = ' '
      BIOMAS		= 0.
	count		= 0.
	EDAT0		= 0.
	EDAT1		= 0.
	EDAT2		= 0.
	MaxStkPop	= 0.
	SPDAT		= 0.
	StkDecDay	= 0.
      YIELD		= 0.

!     Establish # and names of stages for environmental stress summary
      PlantStres % ACTIVE = .FALSE.
      PlantStres % StageName = '                       '
      PlantStres % NSTAGES = 1
      PlantStres % ACTIVE = .FALSE.
      PlantStres % StageName(0) = 'Planting to Harvest    '
      PlantStres % StageName(1) = 'Emergence -Harvest     '

      CALL OPVIEW(CONTROL, 
     &    BIOMAS, ACOUNT, DESCRIP, IDETO, LFNUM, 
     &    Measured, PlantStres, Simulated, STGDOY, 
     &    STNAME, WTNCAN, XLAI, YIELD, YRPLT, PhenoStage)

!***********************************************************************
!***********************************************************************
!     DAILY CALCULATIONS AND OUTPUT 
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. OUTPUT) THEN
C-----------------------------------------------------------------------
      BIOMAS = TOPWT*10.
      PlantStres % W_grow  = TURFAC
      PlantStres % W_phot   = SWFAC 
      PlantStres % N_grow  = NSTRES
      PlantStres % N_phot  = AGEFAC
      PlantStres % P_grow = PSTRES2
      PlantStres % P_phot = PSTRES1
      PlantStres % ACTIVE = .FALSE.

      IF (YRDOY > STGDOY(1) .AND. YRDOY <= STGDOY(16)) THEN
        PlantStres % ACTIVE(0) = .TRUE.
      ENDIF

      IF (YRDOY > STGDOY(3) .AND. YRDOY <= STGDOY(16)) THEN
        PlantStres % ACTIVE(1) = .TRUE.
      ENDIF

!  Days after planting calculated with TIMDIF, an integer function 
!     which calculates the number of days between two Julian dates
      DAP = MAX(0,TIMDIF(YRPLT,YRDOY))

!  Primary stalk bud germination day (from sett, underground)
      IF (SPDAT .EQ. 0. .AND. PhenoStage .GE. 2) THEN
        SPDAT = DAP
      ENDIF

!  Primary stalk emergence day (from soil)
      IF (EDAT0 .EQ. 0. .AND. PhenoStage .GE. 3) THEN
        EDAT0 = DAP
      ENDIF

!  First tiller emergence day (from soil)
      IF (EDAT1 .EQ. 0. .AND. TillerCount .EQ. 1) THEN
        EDAT1 = DAP
      ENDIF

!  Second tiller emergence day (from soil)
      IF (EDAT2 .EQ. 0. .AND. TillerCount .EQ. 2) THEN 
        EDAT2 = DAP
      ENDIF

!  Day of tiller maximum population density
	IF (StalkPopul(DAS) .GT. StalkPopul(DAS-1)) THEN
	    MaxStkPop = MAX(StalkPopul(DAS), MaxStkPop)
	    IF (MaxStkPop .EQ. StalkPopul(DAS)) THEN
			MaxStkDay = DAP
	    ENDIF
	ENDIF

!  Day of initiation of tiller population decline
	IF (StkDecDay .EQ. 0 .AND. DAS .GT. 2) THEN
		IF ((StalkPopul(DAS-1) - StalkPopul(DAS)) .GT. 0. .AND.  
     &       (StalkPopul(DAS-1) - StalkPopul(DAS-2)) .GE. 0.)  THEN
			   
			   StkDecDay = DAP
		ENDIF
	ENDIF
				
		IF (StkDecDay .LT. MaxStkDay) THEN
			    StkDecDay = 0.
		ENDIF

!  Send data to Overview.out data on days where stages occur
      CALL OPVIEW(CONTROL, 
     &    BIOMAS, ACOUNT, DESCRIP, IDETO, LFNUM, 
     &    Measured, PlantStres, Simulated, STGDOY, 
     &    STNAME, WTNCAN, XLAI, YIELD, YRPLT, PhenoStage)

!***********************************************************************
!***********************************************************************
!     Seasonal Output 
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. SEASEND) THEN
!-----------------------------------------------------------------------
!     Compute values to be sent to Overview, Summary and Evaluate files.

!  Millable fresh sugarcane harvested, (default frac is 100 %)
      SMFMH = (STKFWTHa(DAS) / 1000) * HARVFRAC(1)

!  Sucrose weight at harvest (tn/ha)
	SUCH = SUWTHa(DAS) / 1000

!  Stalk (structure; no sugars) dry weight
	SSTKH = STKWTHa(DAS)/1000

!  Harvestable dry weight
      HWAM =  (STKWTHa(DAS) + SUWTHa(DAS)) 

!  Actual millable dry weight yield harvested (default is 100 %)
      HWAH = (STKWTHa(DAS) + SUWTHa(DAS)) * HARVFRAC(1)

!  Canopy weight -- (live & dead) stalk + leaf + sucrose
	CWAM = BADMD
!!!   BADMD from CSP_OPGROW

!  Residue or trash weight (above-grnd biomass minus stalks harvested)
	TRSH = (CWAM - HWAM) /1000

!  Leaf Area Index at Harvest
	LAIH = XLAI

!  Average leaf area per stalk at harvest (cm2 / stalk)
      IF (StalkPopul(DAS) .GT. 0.) THEN
          AvgLfAr = XLAI * 10000 / StalkPopul(DAS)
	  ELSE
		AvgLfAr = 0.
      ENDIF

!  Average leaf weight per stalk at harvest  (g dry leaf / stalk)
      IF (StalkPopul(DAS) .GT. 0.) THEN
		AvgLfWt = LFWTHa(DAS)  / (StalkPopul(DAS) * 10)	  
	  ELSE
		AvgLfWt = 0.
      ENDIF

!  Average node (leaf) number per stalk at harvest
	NodeSum = 0.
      DO Stalk = 1, NumOfStalks
		IF (StalkState(Stalk,1) .EQ. 'LIVE') THEN
			count = count + 1
!!!			NodeSumP = NodeSumP + LeafNum(DAS,Stalk)
			NodeSum = NodeSum + INT(LeafNum(DAS,Stalk))
		END IF
	END DO

!!!	NodeSumA = NodeSumP * PLTPOP
!!!	IF (StalkPopul(DAS) .GT. 0.) THEN
!!!		AvgNode = NodeSumA / StalkPopul(DAS)
!!!	  ELSE
!!!		AvgNode = 0.

	IF (count .GT. 0.) THEN
		AvgNode = NodeSum / count
	  ELSE
		AvgNode = 0.
	ENDIF

	count = 0.

!  Stalk population at harvest (stalks / m2)
	SNAH = StalkPopul(DAS)

!  Average stalk height at harvest
	HeightSumP = 0.
      DO Stalk = 1, NumOfStalks
		IF (StalkState(Stalk,1) .EQ. 'LIVE') THEN
			HeightSumP = HeightSumP + StkHt(Stalk)
		END IF
	END DO
	HeightSumA = HeightSumP * PLTPOP
	IF (StalkPopul(DAS) .GT. 0.) THEN
          AvgHeight = HeightSumA / StalkPopul(DAS)
	  ELSE
		AvgHeight = 0.
      ENDIF

!  Harvest index at maturity = sucrose/(stalk dm + sucrose)
      IF (HWAM .GT. 0.0 .AND. SUCH .GE. 0.0) THEN
          HIAM = SUCH / (HWAM/1000)  ! kg --> tons 
       ELSE
		  HIAM = 0.
      ENDIF


!  Byproduct harvested is leaf weight
      BWAH = (LFWTHa(DAS)+LSWTHa(DAS)) * HARVFRAC(2)

!-----------------------------------------------------------------------
!     Read Measured (measured) data from FILEA
!-----------------------------------------------------------------------
      IF ((INDEX('YE',IDETO) > 0 .OR. INDEX('IAEBCGDT',RNMODE) .GT. 0) 
     &  .OR. (INDEX('AY',ISWITCH%IDETS) .GT. 0 .AND. 
     &      CONTROL%CROP .NE. 'FA')) THEN
         IF (INDEX('FQ',RNMODE) > 0) THEN
           TRT_ROT = CONTROL % ROTNUM
         ELSE
           TRT_ROT = CONTROL % TRTNUM
         ENDIF
         !CALL READA (FILEA, PATHEX, OLAB, TRT_ROT, YRSIM, X)
         CALL READA_Y4K(FILEA, PATHEX,OLAB, TRT_ROT, YRSIM, X)

!     Convert Date of max leaf area index from YRDOY to DAP.  
!     and change descriptions to match.
        CALL READA_Dates(X(7), YRSIM, ILFX)
        IF (ILFX .GT. 0 .AND. IPLTI .EQ. 'R' .AND. ISENS .EQ. 0) THEN
          DLFX = TIMDIF(YRPLT,ILFX)
        ELSE
          DLFX  = -99
        ENDIF
        !CALL CHANGE_DESC(DESCRIP(7)) 
        OLAP(7) = 'LAIXD '  !Finds this name in Data.cde. P=DAP; T=YRDOY
        CALL GetDesc(1,OLAP(7), DESCRIP(7))

!     Convert primary stk bud sprouts from sett from YRDOY to DAP.  
!     and change descriptions to match.
        CALL READA_Dates(X(12), YRSIM, ISPT)
        IF (ISPT .GT. 0 .AND. IPLTI .EQ. 'R' .AND. ISENS .EQ. 0) THEN
          DSPT = TIMDIF(YRPLT,ISPT)
        ELSE
          DSPT  = -99
        ENDIF
        !CALL CHANGE_DESC(DESCRIP(12)) 
        OLAP(12) = 'SPDAP ' !Finds this name in Data.cde. P=DAP; T=YRDOY
        CALL GetDesc(1,OLAP(12), DESCRIP(12))

!     Convert primary stk emergence from soil from YRDOY to DAP.  
!     and change descriptions to match.
        CALL READA_Dates(X(13), YRSIM, IEM0)
        IF (IEM0 .GT. 0 .AND. IPLTI .EQ. 'R' .AND. ISENS .EQ. 0) THEN
          DEM0 = TIMDIF(YRPLT,IEM0)
        ELSE
          DEM0  = -99
        ENDIF
        !CALL CHANGE_DESC(DESCRIP(13)) 
        OLAP(13) = 'EDAT0 ' !Finds this name in Data.cde. P=DAP; T=YRDOY
        CALL GetDesc(1,OLAP(13), DESCRIP(13))

!     Convert first tiller emergence from YRDOY to DAP.  
!     and change descriptions to match.
        CALL READA_Dates(X(14), YRSIM, IEM1)
        IF (IEM1 .GT. 0 .AND. IPLTI .EQ. 'R' .AND. ISENS .EQ. 0) THEN
          DEM1 = TIMDIF(YRPLT,IEM1)
        ELSE
          DEM1  = -99
        ENDIF
        !CALL CHANGE_DESC(DESCRIP(14)) 
        OLAP(14) = 'EDAT1 ' !Finds this name in Data.cde. P=DAP; T=YRDOY
        CALL GetDesc(1,OLAP(14), DESCRIP(14))

!     Convert second tiller emergence from YRDOY to DAP.  
!     and change descriptions to match.
        CALL READA_Dates(X(15), YRSIM, IEM2)
        IF (IEM2 .GT. 0 .AND. IPLTI .EQ. 'R' .AND. ISENS .EQ. 0) THEN
          DEM2 = TIMDIF(YRPLT,IEM2)
        ELSE
          DEM2  = -99
        ENDIF
        !CALL CHANGE_DESC(DESCRIP(15)) 
        OLAP(15) = 'EDAT2 ' !Finds this name in Data.cde. P=DAP; T=YRDOY
        CALL GetDesc(1,OLAP(15), DESCRIP(15))

!     Convert tiller maximum population from YRDOY to DAP.  
!     and change descriptions to match.
        CALL READA_Dates(X(17), YRSIM, ITMX)
        IF (ITMX .GT. 0 .AND. IPLTI .EQ. 'R' .AND. ISENS .EQ. 0) THEN
          DTMX = TIMDIF(YRPLT,ITMX)
        ELSE
          DTMX  = -99
        ENDIF
        !CALL CHANGE_DESC(DESCRIP(17)) 
        OLAP(17) = 'T#MXD ' !Finds this name in Data.cde. P=DAP; T=YRDOY
        CALL GetDesc(1,OLAP(17), DESCRIP(17))

!     Convert initiation of tiller population decline from YRDOY to DAP
!     and change descriptions to match.
        CALL READA_Dates(X(18), YRSIM, IPOP)
        IF (IPOP .GT. 0 .AND. IPLTI .EQ. 'R' .AND. ISENS .EQ. 0) THEN
          DPOP = TIMDIF(YRPLT,IPOP)
        ELSE
          DPOP  = -99
        ENDIF
        !CALL CHANGE_DESC(DESCRIP(18)) 
        OLAP(18) = 'TDECD '  ! Finds this name in Data.cde.
        CALL GetDesc(1,OLAP(18), DESCRIP(18))

!-----------------------------------------------------------------------
!     Store Simulated and Measured data for this season.
      WRITE(Simulated(1),'(F8.2)')SMFMH;	 
                                    WRITE(Measured(1),'(A8)')TRIM(X(1))
      WRITE(Simulated(2),'(F8.2)')SUCH;    
                                    WRITE(Measured(2),'(A8)')TRIM(X(2))
      WRITE(Simulated(3),'(F8.2)')SSTKH;   
                                    WRITE(Measured(3),'(A8)')TRIM(X(3))
      WRITE(Simulated(4),'(F8.2)')CWAM/1000;    
						WRITE(Measured(4),'(A8)')TRIM(X(4))
      WRITE(Simulated(5),'(F8.2)')TRSH;    
                                    WRITE(Measured(5),'(A8)')TRIM(X(5))
      WRITE(Simulated(6),'(F8.2)') LAIMX;  
                                    WRITE(Measured(6),'(A8)')TRIM(X(6))
      WRITE(Simulated(7),'(I8)')LAIXD;     
                                    WRITE(Measured(7),'(I8)')DLFX
      WRITE(Simulated(8),'(F8.2)') LAIH;   
                                    WRITE(Measured(8),'(A8)')TRIM(X(8))
      WRITE(Simulated(9),'(F8.0)')AvgLfAr; 
                                    WRITE(Measured(9),'(A8)')TRIM(X(9))
      WRITE(Simulated(10),'(F8.2)')AvgLfWt; 
					     WRITE(Measured(10),'(A8)')TRIM(X(10))
      WRITE(Simulated(11),'(F8.2)')AvgNode; 
					     WRITE(Measured(11),'(A8)')TRIM(X(11))
      WRITE(Simulated(12),'(I8)')SPDAT;     
						WRITE(Measured(12),'(I8)')DSPT
      WRITE(Simulated(13),'(I8)')EDAT0;     
						WRITE(Measured(13),'(I8)')DEM0
      WRITE(Simulated(14),'(I8)')EDAT1;     
						WRITE(Measured(14),'(I8)')DEM1
      WRITE(Simulated(15),'(I8)')EDAT2;     
						WRITE(Measured(15),'(I8)')DEM2
      WRITE(Simulated(16),'(F8.2)')MaxStkPop; 
					     WRITE(Measured(16),'(A8)')TRIM(X(16))
      WRITE(Simulated(17),'(I8)')MaxStkDay;     
						WRITE(Measured(17),'(I8)')DTMX  
      WRITE(Simulated(18),'(I8)')StkDecDay;     
						WRITE(Measured(18),'(I8)')DPOP
      WRITE(Simulated(19),'(F8.2)')SNAH; 
					     WRITE(Measured(19),'(A8)')TRIM(X(19))
      WRITE(Simulated(20),'(F8.2)')AvgHeight; 
					     WRITE(Measured(20),'(A8)')TRIM(X(20))
      WRITE(Simulated(21),'(F8.2)')HIAM; 
					     WRITE(Measured(21),'(A8)')TRIM(X(21))
     
      ENDIF  

      YREMRG = INCDAT(YRPLT, EDAT0)

!-----------------------------------------------------------------------
!     Send data to OPSUM for SUMMARY.OUT file.
!-----------------------------------------------------------------------
!     Store Summary.out labels and values in arrays to send to
!     OPSUM routines for printing.  Integers are temporarily 
!     saved as real numbers for placement in real array.
!     If more labels & values are added, increase SUMNUM above
      LABEL(1) = 'DWAP'; VALUE(1) = PLWT*0.3 
                                         !mt/ha; PLWT=fresh wt; 70% H2O
      LABEL(2) = 'CWAM'; VALUE(2) = CWAM
      LABEL(3) = 'HWAM'; VALUE(3) = HWAM
      LABEL(4) = 'HWAH'; VALUE(4) = HWAH
      LABEL(5) = 'BWAH'; VALUE(5) = (CWAM - HWAM)/10
      LABEL(6) = 'NUCM'; VALUE(6) = WTNUP*10.
      LABEL(7) = 'CNAM'; VALUE(7) = WTNCAN*10.
      LABEL(8) = 'HIAM'; VALUE(8) = HIAM
	Label(9) = 'LAIX'; VALUE(9) = LAIMX
      LABEL(10)= 'EDAT'; VALUE(10)= YREMRG

      !Send labels and values to OPSUM
      CALL SUMVALS (SUMNUM, LABEL, VALUE) 

!-----------------------------------------------------------------------
!     Call Overview.out routine
      BIOMAS = TOPWT*10.
      YIELD  = NINT(HWAH) ! millable yield (dry weight kg/ha) 
      
      CALL OPVIEW(CONTROL, 
     &    BIOMAS, ACOUNT, DESCRIP, IDETO, LFNUM, 
     &    Measured, PlantStres, Simulated, STGDOY, 
     &    STNAME, WTNCAN, XLAI, YIELD, YRPLT, PhenoStage)

!-----------------------------------------------------------------------
      !Send Measured and Simulated datat to OPSUM
      IF(INDEX('YE',IDETO) > 0 .OR. INDEX('IAEBCGD',RNMODE) .GT. 0) THEN
        CALL EvaluateDat (ACOUNT, Measured, Simulated, DESCRIP, OLAP) 
      ENDIF

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE CSP_OPHARV
C=======================================================================

