      
!THIS MODULE IS USED TO PREDEFINE ALL COMMON VARIABLES INSTEAD OF USE COMMON BLOCKS in *.inc files
!_____________________________________________________________________________________________
!VARIABLE NAME      UNIT            PROPERTY      MEANS
!NL                  --                        I4                  THE NUMBERS OF SOIL LAYERS
!ROOTC                  kg C/ha            array,R4      Root carbon in soil layers
!ROOTN                  kg N/ha            array, R4      Root NITROGEN in soil layers
!RDCL                  kg C/ha            array, R4      Root DEATH CARBON LOSS in soil layers
!RDNL                  kg N/ha            array, R4      Root DEATH NITROGEN LOSS in soil layers
!RDENSITY            cm/ha/m            array, R4      Root LENGTH DENSITY in soil layers
!NROOTC                  kg C/ha            R4                  New carbon allocated to root
!NROOTN                  kg N/ha            R4                  New nitrogen allocated to root 
!MAXD                  cm                  R4                  The maximum rooting depth of a variety
!SWCX                  --                  arry, R4      Soil water contents in layers
!SNH4X                  kg N/ha            array, R4      soil NH4-N contents in layers      
!SNO3X                  kg N/ha            array, R4      Soil NO3-N content in layers
!STX                  oC                  array, R4      Soil temperature in layers
!SANDX                  %                  array, R4      Soil sand contents in layers
!CLAYX                  %                  array, R4      Clay content in layers
!BDX                  Mg/m3            array, R4      Soil bulk density in layers
!IROOTD                  m                  R4                  Initial root depth at sowing depth for direct-seeding
!                                                                  or transplanting depth
!ROPTT                  oC                  R4                  Optimum temperature for root growth
!RMINT                  oC                  R4                  Minimum temperature for root growth
!RTBS                  oC                  R4          The lowest temperature for root to survive 
!SROOTL                  cm/g C            R4                  Special root length
!RCNL                  --                  R4                  The C:N ratio of death root (lowest root C:N ratio)
!SDEP                  cm                  R4                  The depth of soil layers
!KST                  cm/d            R4, ARRAY      SATURATED HYDRAULIC CONDUCTIVITY
!TKL                  m                  R4,ARRAY      The thickness of EACH SOIL LAYER
!TKLT                  m                  R4                  TOTAL THICKNESS OF ALL SOIL LAYERS
!WCL                  --                  R4,ARRAY      SOIL WATER CONTENT IN EACH LAYER
!WCLI                  --                  R4,ARRAY      INITIAL WATER CONTENT OF SOIL LAYER
!WCLINT                  --                  R4                  INTERPOLATION TABLE FOR WATER CONTENT FOR SOIL LAYERS
!WCLQT                  --                  R4,ARRAY      ACTUAL SOIL WATER CONTENT OF EACH SOIL LAYER
!WCFC                  --                  R4,ARRAY      The soil field capacity
!WCST                  --          R4,ARRAY      The soil saturated water content
!WCSTRP                  --                  R4, ARRAY      SATURATED WATER CONTENT OF RIPENED SOIL
!WCWP                  --          R4,ARRAY    The soil wilting point
!WCAD                  --                  R4,ARRAY      The air dry soil water content
!VGA                  /cm                  R4,ARRAY      van GENUCHTEN ALPHA PARAMETER
!VGL                  --                  R4,ARRAY      van GENUCHTEN LAMBDA PARAMETER
!VGN                  --                  R4,ARRAY      van GENUCHTEN n PARAMETER
!VGR                  --                  R4,ARRAY      van GENUCHTEN RESIDUAL WATER CONTENT
!PN                        --                  R4,ARRAY      PARAMETER n IN POWER FUNCTION FOR HYDRAULIC CONDUCTIVITY
!SWITKH                  --                  LOGICAL            HYDRAULIC CONDUCTIVITY SWITCH
!DVS                  --                  R4                  DEVELOPMENT STAG
!RRCC                  kg C/ha/t      R4,ARRAY      ROOT CARBON CHANGE RATE PER TIME STEP
!RRNC                  kg N/ha/t      R4,ARRAY      ROOT NITROGEN CHANGE RATE PER TIME STEP
!ROOTNC				kg N/kg ROOT DM								THE N TO C RATIO OF ROOT
!RRDNL                  kg N/ha/t      R4,ARRAY      ROOT DEATH NITROGEN LOSS RATE PER TIME STEP
!RRDCL                  kg C/ha/t      R4,ARRAY      ROOT DEATH CARBON LOSS RATE PER TIME STEP
!RRDENSIT            cm/ha/t            R4,ARRAY      ROOT LENGTH DENSITY CHANGE RATE PER TIME STEP
!MAXNO                  --                  R4                  THE POINT OF ROOTING CLUSTER
!REFFECD            m                  I4                  THE ROOT DEPTH ABOVE WHICH THE ROOT IS MORE 95% OF TOTAL
!cCO2                  PPM                  R4                  THE REFERENCE ATMOSPHERIC [CO2]
!cKNF                  --                  R4                  REFERENCE EXTINCTION COEFFICIENT OF LIGHT IN CANOPY
!cNFLV                  --                  R4          REFERENCE EXTINCTION COEFFICIENT OF NITROGEN IN CANOPY 
!cREDFT                  --                  R4                  REFERENCE DIRECT RADIATION FRACTION
!ILZMAX                  --                  I4                  INITIAL MAX GROUND WATER LAYERS
!IZWTB                  d,m                  I4                  INITIAL TABLE WITH GROUNDWATER TABLE DEPTH AS FUNCTION OF DAY NUMBER
!ZWA                  cm                  R4                  DEPTH THAT GROUNDWATER RECEDES IN CASE OF NO RECHARGE
!ZWB                  --                  R4                  SENSITIVITY FACTOR OF RECHARGE OF GROUNDWATER TABLE
!MAXGW                  cm                  R4                  MAXIMUM GROUNDWATER TABLE DEPTH                  
!MINGW                  cm                  R4                  MINIMUM GROUNDWATER TABLE DEPTH
!ZWTBI                  cm                  R4                  INITIAL DEPTH OF GRUNDWATER TABLE BELOW SOIL SURFACE
!ZWTB                  d,m                  R4                  TABLE WITH GROUNDAWATER TABLE DEPTH AS FUNCTION OF DAY NUMBER
!DVS                                    R4                  DEVELOPMENT STAGE
!DELT                  d                  R4                  SIMULATING TIME STEP
!SODT                  --                  R4                  THE TOLERANCE OF SOIL OXGENY DEFICIENCY (0 TO 1)
!IUNITD                  --                  I4                  Unit that can be used for input files (-)           C/IN *
!IUNITL                  --                  I4                  Unit for log file messages (-)                      C/IN *
!FILEI2                  --                  C*                  Name of input file no. 2 (-)                        C/IN *
!OUTPUT                  --                  L4                  Flag to indicate if output should be done (-)        C/I *
!DOY                  d                  R4                  Day number (January 1 = 1) (d)                        I  *
!YEAR                  y                  R4                  Year number 
!DELT                  d                  R4                  Time step of integration (d)                          T  *
!TIME                  d                  R4                  Time of simulation (d)                                T  *
!CROPSTA            --                  I4                  Crop stage (-)                                        I  *
!ESTAB                  --                  C*                  Mode of establishment (-)                             I  *
!RAIN                  mm/d            R4                  Daily amount of rainfall (mm d-1)                     I  *
!EVSC                  mm/d            R4                  Potential soil evaporation rate (mm d-1)              I  *
!TRWL                  mm/d            R4,Array      Actual transpiration rate/layer (mm d-1)     I  *
!TRW                  mm/d            R4                  Actual transpiration rate (mm d-1)                    I  *
!IR                        mm/d            R4                  Amount of daily irrigation (mm d-1)                   I  *
!ZRTMS                  m                  R4                  Maximum rooting depth of soil profile (m)             O  *
!TKLP                  m                  R4,Array      Layer thicknesses (m)                        O  *
!WL0                  mm                  R4                  Depth of ponded water (mm)                            O  *
!MSKPA                  kPa                  R4,Array      Soil water tension/soil layer (kPa)          O  *
!IFLAG                                    I4
!ISTAT1                                    I4
!ISTAT2                                    I4
!ISTN                                    I4
!ANGA                                    R4
!ANGB                                    R4
!ELEV                                    R4
!LAT                                    R4
!LONG                                    R4
!RDD                                    R4
!TMMN                                    R4
!TMMX                                    R4
!VP                                          R4
!WN                                          R4 
!WTRMES                                    L4
!WTRTER                                    L4
!WTRDIR                                    C80
!CNTR                                    C7 
!WSTAT                                    C6  
!DUMMY                                    C1 
!IUNITR                                    I4
!IUNITD                                    I4
!IUNITO                                    I4
!IUNITL                                    I4
!IUNITC                                    I4
!FILEON                                    C128
!FILEOL                                    C128
!FILEIC                                    C128
!FILEIR                                    C128
!FILEIT                                    C128
!FILEI1                                    C128
!FILEI2                                    C128
!FILEI3                                    C128
!FILEI4                                    C128
!FILEI5                                    C128
!INOD                                    I4
!IOD                                    I4
!IMNOD                                    I4
!IOBSD                                    I4,ARRAY            WITH IMNOD DEMINSIONS 
!TRC                  mm/d            R4                  potential transpiration rate of crop              I  !    
! 
!TRWL                  mm/d            R4                  Actual transpiration rate per layer               I  !                                                                 *            
!________________________________________________________________________________________________________________

!MODULE VARIABLES
!=================================================================================================================
!Global variables:: Because of the use of POINTER for global variables, the local variable which has same name would
!					not be affects for their values and references
!=================================================================================================================
!1. Soil Parameters: All array have dimension 0 for surface residue or pond, broadcast fertilizer also goes into it
!-----------------------------------------------------------------------------------------------------------------

!	INTEGER SL
!	REAL CLAYX(0:10), SANDX(0:10), SILTX(0:10), BD(0:10), LAYT(0:10), SDEP(0:10)
!	REAL SPH(0:10), SCEC(0:10), SNH4X(0:10), SNO3X(0:10), SNH3X(0:10), UREA(0:10)
!	REAL PLOWL1, SOC(0:10), SON(0:10)

!	  REAL RRCC(10),RRNC(10), RRDCL(10), RRDNL(10),RRDENSIT(10)
!      REAL ROOTN(10),ROOTC(10), RDCL(10), RDNL(10), RDENSITY(10)
!      REAL NROOTC, NROOTN, MAXD, RCNL, SROOTL, theRootC
!      REAL IROOTD, ROPTT, RMINT, RTBS, MAXDEP, SODT
!      REAL REFFECD, REFCD_O, ROOTNC     
 !END MODULE VARIABLES

 module public_module
!	Purpose:
!	this module is used to store the public values that are used by different
!	sub-models, subroutines and other modules
!	Note:
!	All variables in this module are started with 'P' to indicate public_variables,
!	these variables are update and recalled at thier needed place. If it is array, 
!	the lower boundary is 0, the higher boundary is 15. The lower deminsion '0' is used 
!	for surface residue or surface ponded water pool.
!
	 type public_variables
		   	
	

!-------------------------------------------------------------------------------------------------------------------
!3. Management Parameters
!-------------------------------------------------------------------------------------------------------------------


!-------------------------------------------------------------------------------------------------------------------
!4. Output Parameters
!-------------------------------------------------------------------------------------------------------------------
	
!		 REAL STATE(120)			!This variable is used to store state variables which need write into output file
	
!--------------------------------------------------------------------------------------------------------------------
!5. CONTROL AND MODEL RUNNING PARAMETERS
!--------------------------------------------------------------------------------------------------------------------
	
		 INTEGER CRUN,  STARTRUN
		 LOGICAL PROOT_NUTRIENT, KILLSOIL, NBALANCE
		 character*10 pond_active
	!	public control parameters
		 integer PYear			!count current year at the end of last run
		 integer Pdoy			!count the days in a year at the end of last run
		 integer Pdae			!count the days afetr emergence
		 integer Pdat			!count the days after transplanting
	!	public soil chemical properties
		 real Pno3(0:15)		!soil NO3 content (kg/ha)
		 real Pnh4(0:15)		!soil NH4 content (kg/ha)
		 real PmineralizedN(0:15) !daily mineralized nitrogen in each soil layer (kh N/ha), negative is immobilized
		 real Purea(0:15)		!soil urea content (kg/ha)
		 real    pond_no3        ! mineral N as nitrate in pond (kg/ha)
		 real    pond_nh4        ! mineral N as ammonium in pond (kg/ha)
		 real    pond_urea       ! urea in pond (kg/ha)
		 real Psoc(0:15)		!soil organic carbon content (kg C/ha)
		 real Pson(0:15)		!soil organic nitrogen content (kg N/ha)
		 real pph(0:15)		!soil and surface(pond) pH
		 real PFOM_type(0:15)	!soil fresh organic residue types in a given day
		 real PFOM_C(0:15)	!soil fresh organic carbon (kg C/ha) in a given day
		 real PFOM_N(0:15)	!soil fresh organic nitrogen (kg N/ha) in a given day, 0 for surface residue
	!	 public soil physical properties
		 integer Pnl				!soil layers
		 real Pdlayer(15)		!Soil layer thickness (mm)
		 real Pbd(15)			!Soil bulk density (g/cm3)
		 real Psand(15)		!soil sand content (fraction) 
		 real Pclay(15)		!soil clay content (fraction)
		 real Pkst(15)		!soil saturated water conductivity (cm/h)
		 real Pwcst(15)		!soil saturate water content (cm3/cm3)
		 real Pwcfc(15)		!soil field capacity (cm3/cm3)
		 real Pwcwp(15)		!soil wilting point (cm3/cm3)
		 real Pwcad(15)		!soil air drying water content (cm3/cm3)
		 real PplowDepth		!plow pan depth (m)
		 real Psoiltx(0:15)	!soil temperature (oC)
		 real Pwl0			!surface water level (mm)
		 real Pswc(0:15)		!soil water content (cm3/cm3)
		 real Pwflux(0:15)	!soil water flux (cm/day), it quantify the net changes in a given layer
		 REAL PTRWL(15)	!soil water uptake for quantify the interaction of water and nitrogen in nutrient uptake
		 real Prunoff			!soil surface runoff (mm/day)
		 real Pdrain			!soil drainage (mm/day)
		 REAL Pirrig			!IRRIGATION AMOUNT (MM)
	!	 public plant variable
		 real Plai			!daily green leaf area index
		 integer PResNum			!number of residue, maximum for 10
		 character(16) PResName(15)	!Residue name
		 Integer PResType(15)	!Residue type associate with each residue
		 real PResC(0:15,15)	!daily plant little fall carbon (kg C/ha), '0' use for above ground, 1 to 15 used for root
		 						!second deminsion is corresponding residues
		 real PResN(0:15,15)	!daily plant little fall nitrogen (kg N/ha), '0' use for above ground, 1 to 15 used for root
		 real         dlt_res_c_biom(15)    ! carbon from residues to biomass
		 real         dlt_res_c_hum(15)     ! carbon from residues to humic    
		 real ProotD			!daily plant root depth of 95% concentration (m)
		 real PRootAD			!daily plant root actual depth (m)
		 real ProotDen(15)	!daily root total length (cm)
		 real PSROOTL			!special root length cm/g C
     real PRMINT			!minimum temperature for root growth
     real PROPTT			!optimum temperature of root growth
     real PRTBS			!minimim temperature for root to survive
     real PRCNL			!lowest root nitrogen content (residue root N content)
		 real PMAXD			!MAXIMUM ROOTING DEPTH (CM)
		 real PSODT            !TOLERANCE OF OXYGEN DEFICIENCY
	!	 Public climate variable	
		 real PmaxT			!daily maximum air temperature (oC)
		 real PminT			!daily minimum air temperature
		 real PRad			!daily radiation (Mj/day)
		 real PRain			!daily precipitation (mm)
		 real PPressur		!daily vapor pressure (KPa)
		 real Pwind			!daily average wind speed (m/s)
		 real PETp			!potential evapotranspiration (mm/day)
		 real PETa			!actual evapotranspiration (mm/day)
		 real Pevap			!Actual evaporation from surface (mm/day)
		 real Ptrans			!Actual transpiration from plant (mm/day)
		 REAL PDt			!The temperature differences between canopy and air (oC)
		 REAL PRdn			!The daily net radiation (MJ/day)
		 REAL PLAALA        !The adjustment on phenology base, maximu, optimal sue to elevation, latitude
		 LOGICAL ISPHENAD   !The PHENOLOGY ADJUSTMENT only be used for large spatial scale modeling
		 CHARACTER*256 OPSTRING    !USE IT TO HOLD THE OUTPUT VARIABLE IN OP.DAT         
	 end type public_variables
	 !following TYPE will be used in the rotation in a given soil
	 type temporary_soil_information
	!	public soil chemical properties
		 real Xsno3(0:15)		!soil NO3 content (kg/ha)
		 real Xsnh4(0:15)		!soil NH4 content (kg/ha)
		 real Xurea(0:15)		!soil urea content (kg/ha)
		 real Xpond_no3        ! mineral N as nitrate in pond (kg/ha)
		 real Xpond_nh4        ! mineral N as ammonium in pond (kg/ha)
		 real Xpond_urea       ! urea in pond (kg/ha)
		 real Xsoc(0:15)		!soil organic carbon content (kg C/ha)
		 real Xson(0:15)		!soil organic nitrogen content (kg N/ha)
		 real Xph(0:15)		!soil and surface(pond) pH
		 real XFOM_type(0:15)	!soil fresh organic residue types in a given day
		 real XFOM_C(0:15)	!soil fresh organic carbon (kg C/ha) in a given day
		 real XFOM_N(0:15)	!soil fresh organic nitrogen (kg N/ha) in a given day, 0 for surface residue
	!	 public soil physical properties
		 integer Xnl				!soil layers
		 real Xdlayer(15)		!Soil layer thickness (mm)
		 real Xbd(15)			!Soil bulk density (g/cm3)
		 real Xsand(15)		!soil sand content (fraction) 
		 real Xclay(15)		!soil clay content (fraction)
		 real Xkst(15)		!soil saturated water conductivity (cm/h)
		 real Xwcst(15)		!soil saturate water content (cm3/cm3)
		 real Xwcfc(15)		!soil field capacity (cm3/cm3)
		 real Xwcwp(15)		!soil wilting point (cm3/cm3)
		 real Xwcad(15)		!soil air drying water content (cm3/cm3)
		 real XplowDepth		!plow pan depth (m)
		 real Xsoilt(0:15)	!soil temperature (oC)
		 real Xwl0			!surface water level (mm)
		 real Xswc(0:15)		!soil water content (cm3/cm3)
		 integer XResNum			!number of residue, maximum for 10
		 character(16) XResName(15)	!Residue name
		 Integer XResType(15)	!Residue type associate with each residue
		 real XResC(0:15,15)	!daily plant little fall carbon (kg C/ha), '0' use for above ground, 1 to 15 used for root
		 						!second deminsion is corresponding residues
		 real XResN(0:15,15)	!daily plant little fall nitrogen (kg N/ha), '0' use for above ground, 1 to 15 used for root
		 real Xdlt_res_c_biom(15)    ! carbon from residues to biomass
		 real Xdlt_res_c_hum(15)     ! carbon from residues to humic    

	 end type temporary_soil_information
!	common /InstancePointers/ pv
!	save InstancePointers
	 type(public_variables),pointer::pv
	 type(temporary_soil_information), pointer::tsi
!	 SAVE         !&#@TAOLI
end module public_module

module GP
!      common block for communication with input routine
   real cCO2, cKNF, cNFLV, cREDFT, cAMaxSLN, cMinSLN     
end module GP

module GWT
   INTEGER    ILZMAX, IZWTB
   PARAMETER (ILZMAX=400)
   REAL       ZWA, ZWB, MAXGW, MINGW, ZWTBI,ZWTB(ILZMAX)
end module GWT

module HYDCON
   REAL KST(10), WCAD(10), WCSTRP(10)
end module HYDCON

module NUCHT
   REAL  VGA(10), VGL(10), VGN(10), VGR(10)
end module NUCHT
	  
module power
   REAL PN(10)
end module power

MODULE SPAW
	REAL SPAWA(10), SPAWB(10)
END MODULE SPAW

module swit
  INTEGER SWITKH
end module swit

MODULE CROP_N
   REAL xFNLV, xNFLV
END MODULE CROP_N