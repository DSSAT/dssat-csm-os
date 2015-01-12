!*****************************************************************************************************************
!this routinue is developed for calculating root growth in
!vertica soil profile, adopted from early work of Dr. Tao Li
!The modified bi-exponetial distribution function is used to describe the root vertical profile, the new root carbon 
!will toward to the soil layer where the areation, penetration, soil water, soil temperature and soil nutrient are
!the best.
!*****************************************************************************************************************
!Variable name	unit		property		description
!_________________________________________________________________________________________________________________
!ROOTC			kg C/ha		array,real		Root carbon in soil layers
!ROOTN			kg N/ha		array, real		Root NITROGEN in soil layers
!RDCL			kg C/ha		array, real		Root DEATH CARBON LOSS in soil layers
!RDNL			kg N/ha		array, real		Root DEATH NITROGEN LOSS in soil layers
!RDENSITY		cm/ha/m		array, real		Root LENGTH DENSITY in soil layers
!NROOTC			kg C/ha		real			New carbon allocated to root
!NROOTN			kg N/ha		real			New nitrogen allocated to root 
!MAXD			cm			real			The maximum rooting depth of a variety
!SWCX			--			arry, real		Soil water contents in layers
!SNH4X			kg N/ha		array, real		soil NH4-N contents in layers	
!SNO3X			kg N/ha		array, real		Soil NO3-N content in layers
!SOILTX			oC			array, real		Soil temperature in layers
!SANDX			%			array, real		Soil sand contents in layers
!CLAYX			%			array, real		Clay content in layers
!BDX			Mg/m3		array, real		Soil bulk density in layers
!IROOTD			cm			real			Initial root depth at sowing depth for direct-seeding
!												or transplanting depth
!ROPTT			oC			real			Optimum temperature for root growth
!RMINT			oC			real			Minimum temperature for root growth
!RTBS			oC			real            The lowest temperature for root to survive 
!SROOTL			cm/g C		real			Special root length
!RCNL			--			real			The C:N ratio of death root (lowest root C:N ratio)
!SDEP			cm			real			The depth of soil layers
!PLOW1			m			real			the depth of plowpan
!LAYT			cm			real			The thickness of soil layers
!THETAF			--			real			The soil field capacity
!THETAS			--          real			The soil saturated water content
!THETAW			--          real            The soil wilting point
!THETAA			--			REAL			The air dry soil water content
!SL				--			integer			The number of soil layers
!DVS			--			real			DEVELOPMENT STAG
!RRCC			kg C/ha/t	REAL,ARRAY		ROOT CARBON CHANGE RATE PER TIME STEP
!RRNC			kg N/ha/t	REAL,ARRAY		ROOT NITROGEN CHANGE RATE PER TIME STEP
!RRDNL			kg N/ha/t	REAL,ARRAY		ROOT DEATH NITROGEN LOSS RATE PER TIME STEP
!RRDCL			kg C/ha/t	REAL,ARRAY		ROOT DEATH CARBON LOSS RATE PER TIME STEP
!RRDENSIT		cm/ha/t		REAL,ARRAY		ROOT LENGTH DENSITY CHANGE RATE PER TIME STEP
!MAXNO			--			REAL			THE POINT OF ROOTING CLUSTER
!REFFECD		--			INTEGER			THE SOIL LAYER ABOVE WHICH THE ROOT IS MORE 95% OF TOTAL
!ROOTNC										the the root N:C ratio inprevious day
!RMINDEID   --      real       Minimum fraction of root death under optimal conditions
!________________________________________________________________________________________________________________

!&&&& tHIS ROUTINE NEED CHANGE INTO RATE CALCULATION &&&&&&&
!&&&& NEED TO ADD INPUT OF SOIL TEXTURE INTO SOIL FILE, BD CAN BE CALCULATED FROM 'THETAS'
!&&&& NEED TO ADD ROOT GROWTH PARAMETERS INTO CROP FILES, AND READ IT IN
MODULE RootGrowth
!=================================================================================================================
!Global variables:: Because of the use of POINTER for global variables, the local variable which has same name would
!					not be affects for their values and references
!=================================================================================================================
!1. Soil Parameters: All array have dimension 0 for surface residue or pond, broadcast fertilizer also goes into it
!-----------------------------------------------------------------------------------------------------------------
!	type rootGlobal
		 REAL RRCC(10),RRNC(10), RRDCL(10), RRDNL(10),RRDENSIT(10)
		 REAL ROOTC(10),RDCL(10),RDNL(10),ROOTN(10),RDENSITY(10)
		 REAL NROOTC, NROOTN
		 REAL IROOTD, ROPTT, RMINT, RTBS, MAXDEP, SODT
             REAL REFFECD, REFCD_O, ROOTNC,ROOTMAXN
!	end type rootGlobal

!	type RootParameter
		 real MAXD, RCNL, SROOTL
!	end type rootParameter
	
	  SAVE

END MODULE RootGrowth

SUBROUTINE ROOTG(CROPSTA,DVS, DELT, LROOTC, LROOTN, PLANTMOD)				


	 USE public_module	!VARIABLES
	 USE rootgrowth
     IMPLICIT NONE
	 REAL LROOTC, LROOTN   !LROOTC & LROOTN will have input from BioRice but not ORYZA, The senescence of root C & N in kg/ha
	!ROOT GROWTH 
     REAL basicR(15), LWF(15)
     REAL SSL(15), SAL(15), STL(15), NEFF(15)
	
	 INTEGER SL,OLDNO,PLANTMOD
	 REAL CLAYX(0:10), SANDX(0:10), BD(0:10), LAYT(0:10), SDEP(0:10)
	 REAL SPH(0:10), SNH4X(0:10), SNO3X(0:10), UREA(0:10) !, SCEC(0:10), SNH3X(0:10)
	 REAL PLOWL1 !, SOC(0:10), SON(0:10)
!SOIL VARIABLES CONCERN WITH ROOT GROWTH       
      REAL SOILTX(0:10), THETAW(15), THETAF(15), THETAS(15), THETAA(15) !, LAYT(15)
	! Soil stength, areation, temperature factor of soil layers
       REAL  CWP, totalSN, ASF1, ASF2,  tmpValue, DELT !, TIME
       INTEGER i, j, L, MDL,CROPSTA, J1, J2
    PARAMETER (L=10)
!	 real TrootD  !root depth under optimal conditions
	 REAL TPFmx, TWFmx, TTFmx, LWFmx !THE MINIMUM OF THE FACTOR IS SET TO 0 
	 REAL KB1, KB2,TOTALRC, TOTALRN, TotalDRC, TotalDRN, MaxNo
	 REAL SOILN(L),WCL(L),WL0, DVS, TotalLittleFall, LiveRootC
       REAL TMPV2, TEMPV2, tmpV3, TMPV1,TMPV4,TMPN1,TMPN2,TMPN3,TMPN4
	 real, allocatable::tmpF2(:)
	 REAL BDx, BDo, SBD  !Variables DO bulk density at no rooting, max rootin and soil BD.
	 REAL WFP		 !soil water factor, surface water level
	 REAL RMINDIED
	 REAL MAXDDVS1	!THE ROOT DEEPTH AT DVS=1.0
	 LOGICAL SWITCHR  !DETERMIN WHETHER ROOT HAS REACH IT MAXIMUM DEPTH
!	----FOR CALCULATING TOTAL SENESCENCE ROOT C&N
      REAL NSR, CSRTN, CSRT, RDRC(15), RDRN(15), CXX, NXX
	!Structural root N, N determined root C, Structural root C, Root died C rate, Root died N rate
    SAVE

!+	get daily data from public module
	 sl = pv%pnl;  WL0 = PV%PWL0
	 do i = 1, sl
		clayx(i) = pv%pclay(i)*100.0;	sandx(i) = pv%psand(i)*100.0
		soiltx(i) = pv%psoiltx(i);;		bd(i) = pv%pbd(i)
		layt(i) = pv%pdlayer(i)/10.0;	sdep(i) = sum(layt(1:i))  !pdalayer in mm, layt in cm, sdep in cm
		sph(i) = pv%pph(i);				snh4x(i) = pv%pnh4(i)
		sno3x(i) = pv%pno3(i);			urea(i) = pv%purea(i)
		thetaw(i)= pv%pwcwp(i);			thetaf(i) = pv%pwcfc(i)
		thetas(i) = pv%pwcst(i);		thetaa(i) = pv%pwcad(i)
		WCL(I) = PV%PSWC(I)
	 enddo
	 IF(PV%PPLOWDEPTH.GT.0.0) THEN
		plowl1 = pv%pplowdepth *100.0	!Pplowdepth in m, convert into cm
	 ELSE
		plowl1 = SUM(PV%PDLaYER)/10.0		!from mm into cm
	 endif
	 if(IROOTD.lt.0.001) then
		IROOTD = 0.05     !The default initial root depth is 5 cm
	 endif
!	''''Read data in and calculating basic data
	SoilN=0.0; SSL=0.0; SAL=0.0; STL=0.0;  LWF=0.0	
	RRDCL=0.0; basicR=0.0;RRCC=0.0;MaxNo=0.0; NEFF=0.0
	TOTALRC=0.0; TOTALRN=0.0; TotalDRC=0.0; TOTALDRN=0.0
!Root downward development function: Borg and Grimes (1986) derived a sinusoidal
!function to describe root growth with time, which has the form:
!zj =RZmax{0.5 . 0.5sin[3.03(DAP/DTM) . 1.47]}, where zj is the root depth on jth day,and DAP and
!DTM are days after planning and days to maturity of the crop under consideration. 
!Borg, H., and D.W. Grimes. 1986. Depth development of roots with time: an empirical description.
!Trans.ASAE. 29:194-197.

!---DIRECT SEED
    IF(CROPSTA.EQ.1) THEN
      OLDNO = 0
    ENDIF
!----FOR TRANSPLANT DAY, ROOT REDISTRIBUTING AGAIN
	 IF(CROPSTA.EQ.3) THEN
		 DO I=1, SL
			TOTALRC = TOTALRC + ROOTC(I)
			TOTALRN = TOTALRN + ROOTN(I)
			ToTalDRC=totaldrc+rdcl(i)
			totaldrn=totaldrn+rdnl(i)
			ROOTC(I) = 0.0; ROOTN(I) = 0.0
			RDCL(I) = 0.0; RDNL(I) = 0.0
			RDENSITY(I)= 0.0;RRDENSIT(i) = 0.0
			RRCC(i)=0.0; RRNC(I)=0.0
			RRDCL(I)=0.0; RRDNL(I)=0.0
		 ENDDO
		 NROOTC = TOTALRC + (NROOTC+totaldrc)
		 NROOTN = NROOTC*TOTALRN /TOTALRC    
		 REFFECD = IROOTD			!SETTING THE REFERENCE DEPTH BE TRANSPLANT DEPTH AT TRANSPLANT, unit in m
		! IROOTD = 0.05					!IROOTD in m
		 REFCD_O=REFFECD      !Change it into m
		 OLDNO=INT(IROOTD*100.0)			!SETTING THE OPTIMAL AT TRANSPLANT DEPTH AT TRANSPLANTING DAY, TAOLI, 30NOV,2011		 
		 DO I=1, SL
			 IF(i.eq.1) THEN
				 IF(IROOTD*100.0.LE.LAYT(1)) THEN       !IROOTD in m
					ROOTC(1) = NROOTC
					ROOTN(1) = NROOTN
					!rdcl(i)=totaldrc
					!rdnl(i)=totaldrn
					GOTO 2000
				 ENDIF
			 ELSE
                         IF((IROOTD*100.0.GT.SUM(LAYT(1:(I-1)))).AND.(IROOTD*100.0.LE.SUM(LAYT(1:I)))) THEN
					ROOTC(I) = NROOTC; ROOTN(I) = NROOTN
					rdcl(i)=totaldrc; rdnl(i)=totaldrn
					GOTO 2000
				 ENDIF	
			 ENDIF
		 ENDDO
	 ENDIF

!	** calculate the total root senescence C and N based on the assumption of new root would die immediately
      TOTALDRC =0.0; TOTALDRN = 0.0; LROOTC = 0.0; LROOTN = 0.0
      TOTALDRC = SUM(ROOTC(1:SL)); TOTALDRN = SUM(ROOTN(1:SL))
      IF(TOTALDRC.GT.0) THEN
	DO I = 1, SL
                ROOTN(I) = ROOTC(I)* TOTALDRN/TOTALDRC  !ASSUMMED THE c:n IS THE SAME FOR ALL ROOTS IN DIFFERENT LAYERS.     
	END DO
      END IF
	IF(PLANTMOD.EQ.1) then      !PLANTMOD =1 FOR ORYZA1, PLANTMOD = 2 FOR BIORICE
		if((TOTALDRC.GT.0.0).and.(TOTALDRN.GT.0.0)) THEN
                  CSRT = MAX(0.0,(TOTALDRC+NROOTC) *0.85)
                  NSR = MAX(0.0,(TOTALDRN+NROOTN)*0.85)   !assummed the structure C and N is 85% of total value
			CSRTN = NSR/(2.0*RCNL)		!assumed that the optimal nitrogen content is double of minimal one.
			LROOTC = MAX(MIN(CSRT-0.00001, CSRT -MIN(CSRT, CSRTN)),0.0)
			LROOTN = LROOTC * RCNL
		else
			LROOTC =0.0; LROOTN = 0.0
		ENDIF			
	END IF
	
      IF((TOTALDRN/TOTALDRC).GT.ROOTMAXN) THEN
      !  LROOTN =  LROOTN + MAX(0.0,(TOTALDRN-TOTALDRC*ROOTMAXN)) 
      END IF
      
	IF(SL.LE.1) THEN	!-------SIMPLE EXPONENTIAL FUNCTION IS USED TO CALCULATE THE ROOT DISTRIBUTION AND ROOTING DEPTH WITHIN LAYER
		MAXDEP=LAYT(1)	!ROOT IS LIMITED WITHIN ONE LAYER, MaxDep in cm
		 IF(PLOWL1.GT.0.0) THEN 
			 MAXDEP=MIN(MAXDEP, PLOWL1)		!SUPPOSE THERE IS RARE ROOT BELOW PLOWPAN, both in cm
		 ENDIF
		RRCC(1)=ROOTC(1) + NROOTC
		RRNC(1)=ROOTN(1) + NROOTN
		!THE ROOT DISTRIBUTION WILL BE CALCULATE IN EACH CM OF SOIL, THEN DETERMINE THE ROOT DEPTH
		TEMPV2=0.0
		KB2= -LOG(0.05)/MAX(0.00001,MAXDEP)
		 DO I=1, INT(MAXDEP+0.5)
			 TEMPV2=TEMPV2 + NROOTC*EXP(-KB2*I)
			 IF((TEMPV2/MAX(0.00001,NROOTC)).GE.0.95) THEN
				 REFFECD=max(real(I)/100.0,REFCD_O)		!IN m, For one layer soil, only the rooting depth is useful
				 REFCD_O=REFFECD											!Both in m
				 EXIT 
			 ENDIF
		 ENDDO
		REFFECD=MIN(LAYT(1)/100.0,REFFECD)       !Layt in cm, change into m
		RRCC(1)=(RRCC(1)-RootC(1)-LROOTC)/DELT
		RRNC(1)=(RRNC(1)-ROOTN(1)-LROOTN)/DELT		!RCNL IS IN kg N kg-1 ROOT DM
		RRDCL(1)=LROOTC/DELT	
		RRDNL(1)=LROOTN/DELT
	ELSE  !TWO COMBINED EXPONENTIAL FUNCTIONS IS USED FOR ROOT DISTRIBUTION AND ROOTING DEPTH IN MULTIPLE LAYERS

!-------Get the POSSIBLE maximum rooting depth
		 allocate (tmpf2(INT(SDEP(sl))+1))				!SDEP in cm
		!suppose the root reach its maximum depth while DVS=1.0 (i.e. the vegetation stage), and 
		!linearly relates each other
		IF(DVS.LT.1.0) THEN
			SWITCHR = .FALSE.
		ELSEIF((DVS.GE.1.0).AND.(.NOT.SWITCHR)) THEN
			MAXDDVS1 = REFCD_O
			SWITCHR =.TRUE.
		ENDIF
		if(dvs.GT.1.0) then
                  MAXDEP= MAXDDVS1*100.0 !*MAX(0.0, SIN(1.57+1.57/1.5*(DVS-1.0)))*100.0  !The maximum depth does not change anymore, TAOLI 10 Jan 2012   !!
		ELSE
		    maxDep =  maxd *MAX(0.0, SIN(1.57*DVS)) !(0.5-0.5*sin(3.03*dvs/(2.0-dvs)+1.147)) !maxd in cm
		endif
		 IF(REFFECD.GT.0.0) THEN		
			 maxDep = MIN(MAXD,maxdep) !REFFECD*100.0)				!REFFECD in m, changed into cm
		 ELSE
			 MAXDEP=SDEP(1)																		!SDEP in cm
		 ENDIF 
		 If(maxDep.LE.2.0 *IROOTD*100.0) Then		!IROOTD	in m		
			 maxDep = 2.0 * IROOTD*100.0		!IROOTD IS IN m, maxdep in cm. 
			 !!make sure the MaxDep was not less than 2 time of sowing or transplanting depth, TAOLI, 30NOV, 2011
		 ENDIF
			
		 If(maxDep.GT.SDEP(SL)) Then 
			 maxDep = SDEP(SL)
		 ENDIF
		 IF(PLOWL1.GT.0.0) THEN
			 MAXDEP=MIN(MAXDEP,PLOWL1)			!PLOWL1 in cm
		 ENDIF
!------DETERMINE THE LAYER OF MAXIMUM ROOTING DEPTH
		 DO I=1, SL
			 IF((MAXDEP.GT.SDEP(I-1)).AND.(MAXDEP.LE.SDEP(I))) THEN
				 MDL=MIN(I,SL)
				 EXIT 
			 ENDIF
		 ENDDO
	
!---determine the most good condition layer where the new root carbon will concentrate in
!   IF(NROOTC.GT.0.0) THEN					!IT USE PREVIOUS REDISTRIBUTION FACTOR AT TRANSPLANT DAT
		 totalSN = 0.0; TPFmx=0.0; TWFmx=0.0; TTFmx=0.0
		 DO 100 i = 1, MDL
		
			 If(SNO3X(i).LE.0.0) SNO3X(i) = 0
			 If(SNH4X(i).LE.0.0) SNH4X(i) = 0
			 SoilN(i) = MAX(0.0, SNO3X(i) + SNH4X(i))                      !in g N/m2, 
			 !!USE MAX TO AVIOD THE NEGATIVE VALUE, DO SAME THING FOR SSL SAL, STL AND LWF, TAOLI, 30NOV, 2011
			 IF((SDEP(I).GT.PLOWL1).and.(plowl1.gt.0.0)) SOILN(I)=SOILN(I)/50.0
			   IF(SOILN(I).GE.TOTALSN) THEN
				 	  totalSN=SoilN(i)    !Get the highest value
			   ENDIF

!!---------DETERMINE PENETRATION RESISTANCE, from A. Canarache, 1990. bY TAOLI,APRIL 10, 2009
			      BDx=exp((-1.26+0.02*clayx(i)+7.53*log(bd(i))/log(10.0))*log(10.0))			!THE RPs,
				  BDo=44.9+0.163*CLAYX(I)					!THE TPm
			      SBD=100.0*(1.0-BD(I)/2.65)						!THE TP
			      SBD=0.875+0.32*(BDO-SBD)/BDO			!THE f
			      BDO=100.0*(1.0-0.38*BD(I))/BD(I)					!THE S
			      SBD=SBD*BDO								!THE Sf
                  BDO=-EXP((-0.44+0.00114*CLAYX(I)+1.27*LOG(BD(I))/LOG(10.0)+ &
				        0.0267*CLAYX(I)*LOG(BD(I))/LOG(10.0))*LOG(10.0))			!THE M
			      SSL(I)=EXP((LOG(BDX)/LOG(10.0)+BDO*(LOG(10000.0*WCL(I)/BD(I)/SBD)- &   !WCL IS IN CM3/CM3
				        LOG(50.0))/LOG(10.0))*LOG(10.0))	!THE PENETRATION RESISTANCE INCREASE WITH LARGER SSL
				  IF((SSL(I).GE.3.0).OR.(SDEP(I).GT.PLOWL1)) THEN
						SSL(I) = 1/(SSL(I)+0.000001)/50.0
				  ELSE
						SSL(I)=1/(SSL(I)+0.000001)							!CONVERTING RESISTANCE INTO PENETRATION,LARGE VALUE MEANS EASIER PENETRATING 
				  ENDIF
				  SSL(I) = MAX(0.0, SSL(I))
				  IF(SSL(I).GE.TPFmx) THEN 
					 TPFmx=SSL(I)							
				  ENDIF

!-------DETERMINE THE SOIL WATER (Areation) EFFECTS ON ROOT GROWTH
			 IF(WL0.GT.0.0) THEN
			!The soil is saturated, we suppose only surface layer provides available oxygen condition
				 IF(I.LE.1) THEN 
					 SAL(I)=1.0
				 ELSE
					 !SAL(I)= exp(-0.06931*(sdep(i-1)+0.5*layt(i))) !(SDEP(1)/(SDEP(I)+SDEP(I-1)))**2.0
					 SAL(I)= MAX(0.0,exp(-0.02378*(sdep(i-1)+0.5*layt(i))))
				 ENDIF
			 ELSE 
				 CWP = 0.4 + 0.004 * ClayX(i)
				 WFP = WCL(i) / ThetaS(i)
				 If(WFP.GT.CWP) Then
					 SAL(i) = MAX(0.0,SODT + (1.0 - WFP) * (1.0 - SODT) / (1.0 - CWP)) !SODT: the tolerance of air deficiency, subemerngency, bigger is better tolerance
				 Else
					 SAL(i) = 1.0
				 EndIf
			 ENDIF
			 IF(SDEP(I).GT.PLOWL1) SAL(I)=SAL(I)/50.0
			 IF(SAL(I).GT.TWFmx) THEN 
				 TWFmx = SAL(I)
			 ENDIF
!-------Determine the direct effects of soil water on root growth
			 IF(WL0.GT.0.0) THEN
			 	   LWF(I) = 1.0  !exp(-0.06931*(sdep(i-1)+0.5*layt(i)))		!Under saturated condition, the oxygen availability decrease 30% every 15 cm
				   LWFMX = 1.0
			 Else
			 		 LWF(I) = MAX(0.0,1.0 - ((PV%PWCST(I)-WCL(I))/(PV%PWCST(I)-PV%PWCWP(I)))**2.0)  !** OSMATIC/1.5
			 		 IF(LWF(I).GT.LWFMX) THEN
			 		    LWFMX =LWF(I)
			 		 ENDIF
			 ENDIF

!-------DETERMINE SOIL TEMPERATURE EFFECTS ON ROOT GROWTH	    
			 If(soiltx(i).GE.RMINT) Then
				 STL(i) =Sin(1.57 * (soiltx(i) - RMINT) / (ROPTT - RTBS))
				 STL(i)=max(STL(i),0.0)
			 Else
				 STL(i) = 0
			 EndIf
			 IF(SDEP(I).GT.PLOWL1) STL(I)=STL(I)/50.0
				 IF(STL(I).GE.TTFmx) THEN 
					 TTFmx=STL(I)			
				 ENDIF
100		 ENDDO
		 DO I=1, MDL						!SCALING ALL FACTOR INTO 0 TO 1.
			 IF(TOTALSN.LE.0.0) THEN
				 NEFF(I)=1.0
			 ELSE
				 NEFF(I)=SOILN(I)/TOTALSN
			 ENDIF
			 IF(TPFMX.LE.0.0) THEN
				 SSL(I)=1.0
			 ELSE
				 SSL(I)=SSL(I)/TPFmx
			 ENDIF
			 IF(TWFMX.LE.0.0) THEN
				 SAL(I)=1.0
			 ELSE
				 SAL(I)=SAL(I)/TWFmx
			 ENDIF
			 IF(TTFMX.LE.0.0) THEN
				 STL(I)=1.0
			 ELSE
				 STL(I)=STL(I)/TTFmx
			 ENDIF
			 IF(LWFMX.LE.0.0) THEN
			 	  LWF(I) = 1.0
			 ELSE
			 	  LWF(I) = LWF(I)/LWFMX
			 ENDIF
		 ENDDO
	   
		
!----DETERMINE THE NEW ROOT CONCENTRATING PLACE, WE SUPPOSED THAT MOST NEW ROOTS WILL BE DEVELOPED
!----AT THE SOIL LAYER IN WHICH THE RELATIVE BEST COMBINING ENVIRONMENTAL CONDITIONS EQUAVEULENTLY 
!----CONSIDERING THE ROOT PANETRATION RESISTANCE, SOIL OXYGEN, SOIL WATER AND NITROGEN.				
		 If(maxDep.LE.IROOTD*100.0) Then
			 MaxNo = INT(IROOTD*100.0 + 0.5)
		 Else
			 MaxNo = 0.0; ASF1 = 0.0; ASF2 = 0.0
			 DO i = 1, MDL
				 ASF2 =NEFF(I)+STL(I)+SAL(I)+SSL(I)+ LWF(I) !each factor has equal contribution to root growth
				 If(ASF2.GT.ASF1) Then		!IF TWO LAYERS HAVE SAME CONDITIONS, THE UPPER LAYER WILL BE TAKEN
					 ASF1 = ASF2; MaxNo = real(i)
				 EndIf
			 ENDDO
			 MaxNo=max(1.0,MaxNo)			 	
			 MaxNo = SDEP(int(MaxNo) - 1)+0.5*LAYT(int(MAXNO))
			 MAXNO=MIN(MAXDEP,MIN(MAXD,MAXNO))
			 IF(MAXNO.GE.MAXDEP) THEN
				MAXNO=OLDNO + (MAXDEP-OLDNO)*0.5
			 ENDIF
			 OLDNO=INT(MAXNO)
		 EndIf
	
!-----Calculating distribution by BY UNSTREMETRICAL BI-EXPONENTIAL DISTRIBUTION function	
		 ASF1 = 0.0; ASF2 = 0.0; j = 1

!-----THE EXPONENTIAL COEFFICIENTS KB1 FOR INCREASE AND KB2 FOR DECREASE
         IF(WL0.GT.0.0) THEN   !IF FLOODED, DISTRIBUTION ONLY FOLLOW SINGLE EXPONENTION. TAOLI, 21FEB 2012
            KB1 = -LOG(0.05)/MAXDEP; TMPV3 =0.0; TMPV4 =0.0
         ELSE
		 KB1 = -Log(0.05) / MAXNO; TMPV3 =0.0; TMPV4 =0.0
		 kB2 = -Log(0.05) / (MAXDEP - MAXNO)
             END IF
		 DO i = 1, Int(maxDep + 0.5)
              IF(WL0.GT.0.0) THEN   !IF FLOODED, DISTRIBUTION ONLY FOLLOW SINGLE EXPONENTION. TAOLI, 21FEB 2012
                  tmpValue =Exp(-Kb1 * I)
                  TMPVALUE = TMPVALUE *(NEFF(J) + SSL(J) + SAL(J) + STL(J)+LWF(J))
              ELSE  !IF NON FLOODED, DISTRIBUTION DOUBLE EXPONENTION. TAOLI, 21FEB 2012
			 IF(I.LE.MAXNO) THEN
				 tmpValue =Exp(-Kb1 * (MAXNO - I))
			 ELSEIF(I.GT.MAXNO) THEN
				 TMPVALUE=Exp(-kb2 * (I - MAXNO))
			 ELSE
				 TMPVALUE=0.0
			 ENDIF 
			   TMPVALUE = TMPVALUE *(NEFF(J) + SSL(J) + SAL(J) + STL(J)+LWF(J))
                END IF                     
			   ASF2 =ASF2+TMPVALUE
			   IF(TMPVALUE.GT.0.0)	TMPV3 = TMPV3 + 1.0/MAX(0.00001,TMPVALUE)
			 If((i.GT.INT(SDEP(j - 1))).And.(i.LT.INT(SDEP(j)))) Then
				 ASF1 = ASF1 + tmpValue
				IF(TMPVALUE.GT.0.0) TMPV4 = TMPV4+1.0/MAX(0.00001,TMPVALUE)
				 j = j
				 RRCC(j) = ASF1; RRNC(J)=ASF1
			 Else
				 ASF1 = ASF1 + tmpValue;
				 IF(TMPVALUE.GT.0.0) TMPV4 = TMPV4+1.0/MAX(0.00001,TMPVALUE)
				 RRCC(j) = ASF1; RRNC(j) = ASF1; ASF1 = 0.0
				 RDRC(J) = TMPV4; RDRN(J) = TMPV4; TMPV4 = 0.0
				 j = j + 1
			 EndIf
    
		 ENDDO
    !ENDIF   !For the new root carbon positive
  ENDIF !FOR THE SINGLE OR MULTPILE LAYER ROOTING
!	Supposed that the root senecence only happen in the layer where have root older than one day
      TMPV4 = 0.0;TMPV2 =ASF2;
	DO I=1,SL
		IF(ROOTC(I).GT.0.0) THEN
			TMPV4 =TMPV4 + RDRC(I)
		ELSE
			TMPV4=TMPV4;RDRC(I) = 0.0; RDRN(i) = 0.0
		END IF
	ENDDO
!     ''''Readjust the C and N allocation if the new root C and N if negative
      IF(NROOTC.LT.0.0) THEN
        ASF2 = 0.0
        DO I=1, SL
            IF(ROOTC(I).GT.0.0) THEN
                ASF2 =ASF2+RRCC(I)
            ELSE
                RRCC(I) = 0.0
            END IF
        END DO
      END IF
	
!	'''Calculating the new DM distribution in soil layers
		TMPV3 = 0.0;TMPN3 = 0.0      
  DO i = 1, SL		 
		 If(ASF2.GT.0.0) Then
				RRCC(i) = NRootC * RRCC(i) / ASF2    !IN kg DM/ha
            !    RRNC(i) = NRootN * RRNC(i) / TMPV2    !IN kg N/ha
                TMPV3 = TMPV3+MAX(0.0,ROOTC(I)+RRCC(I))
            !    TMPN3=TMPN3+MAX(0.0,ROOTN(I)+RRNC(I))                         
		 EndIF
	 ENDDO
		IF((TMPV3.GT.0.0).AND.(LROOTC.GT.0.0)) THEN
			DO I=1, SL
				RRDCL(I) = LROOTC*MAX(0.0,ROOTC(I)+RRCC(I))/TMPV3
			END DO
		ELSE
			RRDCL = 0.0
		END IF
	
       !REDISTRIBUTING THE UNENOUGH DETECTION OR SENSCENCE FROM UP LAYERS
       !MODIFIED THE REDISTRIBUTION SECTION, TAOLI, 25Jan 2012
		TMPV3=0.0;TMPN3=0.0
		  !FROM DEEPEST TO FIRST LAYER
		DO I=SL, 2, -1
			IF((ROOTC(I)+RRCC(I)-RRDCL(I)).LT.0.0) THEN
				TMPV3=RRDCL(I)-(ROOTC(I)+RRCC(I))   
					  !in this situation, the RRCC would be negative, because RRDCL can not be larger than ROOTC
					  !correcte TMPV3=ROOTC(I)+RRCC(I) to TMPV3=ROOTC(I)+RRCC(I)-RRDCL(I), 
					  !the unenogh detection will be put into up layer 
				IF(RRDCL(I).GE.0.0) THEN
					RRCC(I-1) = RRCC(I-1)- TMPV3
					RRCC(I) = RRCC(I) + TMPV3                
				ELSEIF(RRCC(I).GE.0.0) THEN            
					RRDCL(I-1) = RRDCL(I-1) - TMPV3
					RRDCL(I)= RRDCL(I) + TMPV3                
				END IF
				CXX = ROOTC(I)+RRCC(I)-RRDCL(I)         
				J1=I;J2=I     
					ENDIF
				ENDDO
				
      !FROM FIRST TO DEEPEST LAYER
      DO I = 1, sl !(MAX(J1,J2)-1)
          IF((ROOTC(I)+RRCC(I)-RRDCL(I)).LT.0.0) THEN
                  TMPV3=ROOTC(I)+RRCC(I)-RRDCL(I)   
                  RRCC(I) = RRDCL(I) -ROOTC(I)
                  RRCC(I+1) = RRCC(I+1) + TMPV3
	 ENDIF 
             CXX = ROOTC(I)+RRCC(I)-RRDCL(I)            
      END DO  
							                                                  
!	!Calculating senescence of root ???NEED IMPROVEMENT.
	SDEP(0) = 0.0; TotalLittleFall = 0.0; LiveRootC = 0.0
	tmpv2=0.0;tmpv3=0.0;tmpf2(int(sdep(sl))+1)=0.0
	RMINDIED = 0.016
	 DO i = sl, 1, -1
			If((ROOTC(i)+ RRCC(I)-RRDCL(I)).GT.0.0) Then
		!	'''---for tmpF2 and tmpF3
			   tmpV2 =0.0; TMPV3 =0.0
			   IF(SDEP(I).GE.MAXDEP) Then
			   		TMPV3 = MAXDEP
			   		DO J=INT(MAXDEP)+1, SDEP(I)
			   			TMPF2(J) = 0.0
			   		ENDDO
			   Else
			   	  TMPV3 = SDEP(I)
			   ENDIF
			   do j=  int(SDEP(i-1))+1, int(TMPV3)
				    tmpF2(j) = exp(-0.03*j)-exp(-0.03*(int(TMPV3)+1))  !IT IMPLIES THAT THE 95% OF ROOT OCCURRING IN UP 100 CM
				    tmpV2 =tmpV2+tmpF2(j)
			   enddo
			   do j=  int(TMPV3), int(SDEP(i-1))+1, -1
				    tmpf2(j) = tmpF2(j)/tmpV2 *MAX(0.0, (RRCC(I)-RRDCL(i)))
			   enddo
		   Else
		     do j=  int(SDEP(i)), int(SDEP(i-1))+1, -1
				   tmpf2(j) = 0.0
		     enddo
	   EndIf
              !---------DETERMINE THE ROOT C AND N CHANGE RATE, AND ROOT DEATH RATE
			IF((RRCC(I)+ROOTC(I)-RRDCL(I)).LT.0.0) RRDCL(I) = RRCC(I)+ROOTC(I)			
			RRCC(i)=(RRCC(I)-RRDCL(i))/DELT      !-RRDCL(i)
			RRDCL(I)=RRDCL(I)/DELT      
			RRDNL(I)=RRDCL(I)/DELT*RCNL          !nitrogen littel fall 
	    RRDENSIT(i) = SROOTL / LAYT(I)/1000.0 !*(1.0+((sdep(i-1)+layt(i)*0.5)/(0.8*maxd))**1.0)
		!----THE SROOTL WOULD VARY WITH SOIL WATER CONDITION AND GROWTH STAGE, REFERENCES???
		!----Tahere Azhiri-Sigari et al. 2000, Plant Prod. Sci., 3(2), 180-188.
		!----Also assumed that the root become thin in deeper soil layer
	 ENDDO
      !----DETERMINE THE EFFECTIVE ROOT LAYER nitrogen change
		TMPN3 = SUM(ROOTN)   !total nitrogen in root in previous day
		TMPN4=SUM(RRDCL)*RCNL*DELT  !total root littel fall in this day
		TMPN3 = TMPN3 + NROOTN - TMPN4 !Total nitrogen in this day after little fall     
		TMPV3 = SUM(ROOTC)+SUM(RRCC)*DELT   !Total root carbon in this dat after littel fall
		IF(TMPV3.GT.0.0) THEN
			DO I=1, SL
				RRNC(I) =  (ROOTC(I)+RRCC(I)*DELT)*TMPN3/TMPV3   !THE TOTAL N IN THIS LAYER ACCORDING TO C:N IN THIS DAY
				RRNC(I) = (RRNC(I)-ROOTN(I))/DELT   !THE CHANGE RATE OF ROOT N IN THIS DAY
			END DO
		ELSE   !ALL ROOT CARBON TO BE ZERO, THEN NITROGEN ALSO TO BE ZERO
			DO I=1, SL
				RRNC(I)=-ROOTN(I)
			END DO
		END IF

		!Determine the rooting depth
	 TMPVALUE=0.0
	 DO I=1, int(SDEP(SL))
		 if(tmpv2.gt.0.0) then
			 tmpf2(i) =max(0.0, tmpf2(i)) 
		 else
			 tmpf2(i) =0.0
		 endif
		   TMPVALUE=TMPVALUE+tmpf2(i)
	 ENDDO
	 !get root actual depth (m)
	 pv%prootad = 0.0
	 DO I=1, int(SDEP(SL))
		 if(tmpf2(i).LE.0.0) then		
			pv%prootad = i
			exit
		 endif	
	 ENDDO 

		TMPV2=0.0;tmpV3 =0.0
	 IF(TMPVALUE.GT.0.0) THEN
                   !TMPV2 = 0.95*TMPVALUE
                   TMPV2 = 1.0*TMPVALUE
		 DO I=1, int(SDEP(SL))
		 	 !95% OF TOTAL ROOT MASS IS THE LAYER OF EFFECTIVE ROOTING LAYER
!			 tmpV3 = tmpf2(i)*srootl/100.0    !in cm/m3 within 1 cm layer
!			 tmpV2 =	10.0/1000.0*srootl		!Critical value is 10.0 g root/m3 effective water and nutrient uptake
!			 if(tmpV3.lt.tmpV2) then			!Critical value is 100.0 cm/m3,
				TMPV3=TMPV3+TMPF2(I) 
			 IF(TMPV3.GE.TMPV2) THEN
				 if(dvs.gt.1.0) then
					REFFECD=min(REFCD_O, max(IROOTD,real(i/100.0)))    !IROOTD in m, REFFECD and REFCD_O in m
				 !IF((DVS.GT.1.0).AND.(REFFECD.GT.REFCD_O)) THEN
					!	REFFECD=REFCD_O
					REFCD_O=REFFECD
				 ELSE
					REFFECD=max(REFCD_O, max(IROOTD,real(i/100.0)))
					REFCD_O=REFFECD
				 ENDIF
				 EXIT
			 ENDIF	
		 ENDDO 
	 else
		 REFCD_O=REFCD_O
		 REFFECD=REFCD_O
	 ENDIF
	 REFFECD=MIN(SDEP(SL)/100.0,REFFECD)
	!ROOT N TO C RATIO
	 TMPV2=0.0; TMPV3=0.0
	 DO I=1, SL
		 TMPV2=TMPV2+RRNC(I)*DELT + ROOTN(I)
		 TMPV3 = TMPV3 + RRCC(I)*DELT + ROOTC(I)
	 ENDDO
	 IF(TMPV3.GT.0.0) THEN
             ROOTNC = max(0.0,TMPV2/TMPV3)      !kg N kg-1 ROOT DM
	 ELSE
		 ROOTNC = 0.008
	 ENDIF

	!set public variable for other routines
	 pv%prootd = reffecd
	 do i=1, sl

	 enddo
	deallocate (tmpf2)
2000      LROOTC =0.0; LROOTN = 0.0            
	END SUBROUTINE
