           
C********************END OF FUNCTION ROUTINES***************************

       SUBROUTINE PHOTOS (
c     [I] Not sure
     - CWSI,
c     [I] Photosynthesis algorithm choice number (PRN file)
     - MDL,
c     [I] An indicator of anaerobic stress
     - ANAERF,
c     [I] Day of year
c     - IDOY,
c     [I] Latitude
     - ALAT,
c     [I] A coefficient for solar radiation ?
c     - SCV,
c     [I] Not sure
     - IW,
c     [I] Allow neg growth? PRN file
     - NEGGRO,
c     [O] Change in dry weight today (i.e. photosynthate)
     - DWDT,
c     [O] Ken Boote's photos. technique: Light interception
c     - bootli,
c     [I] Important for choice of photos. tech.
     - coderd,
c     [I] The height of the canopy today
     - canhgt,
c     [I] Rowspacing
     - rowspc,
c     [I] Quantity of runoff today?
     - RUNOFF, 
c     [I] Allow lodging? (PRN file)
     - allowlodg,
c     [O] Effect of lodging on light interception
     - LODGE_LI, 
c     [IO] The partitioning object
     - Part, 
c     [IO] The growth object
     - Growth, 
c     [I] The Climate object
     - Climate,
c     [I] soil photosythesis fertility factor (CHP 2/14/2011)
     - SLPF, 
c     [O] Output object
     - Out,
c     [I] Simulation switches
     - ISWITCH, 
c     [I] The DSSAT control variable
     -Control, 
c     [I] Atmospheric CO2 concentration
     & CO2           )

c     Use the Canegro modules:
c     ::::::::::::::::::::::::
      USE CNG_ModuleDefs
c     Use DSSAT modules
c     :::::::::::::::::
      USE ModuleDefs
      IMPLICIT NONE
      SAVE

c     Declare composite variables:
c     ::::::::::::::::::::::::::::
c     The DSSAT simulation control object
c     :::::::::::::::::::::::::::::::::::
      Type (SwitchType) ISWITCH

c     The Climate 'object'
c     :::::::::::::::::::
      TYPE(ClimateType) Climate

c     The Partitioning object:
c     :::::::::::::::::::::::
      TYPE(PartType) Part

c     The Growth object
c     :::::::::::::::::
      TYPE(GrothType) Growth

c     The output object:
c     ::::::::::::::::::
      TYPE (OutType) Out

c     The DSSAT control object:
c     :::::::::::::::::::::::::
      TYPE (ControlType) Control

c     Previously undeclared variables:
c     ::::::::::::::::::::::::::::::::
c     See variables.mdb
c     :::::::::::::::::
c         Climate vars:
c         :::::::::::::
          REAL     SOLRAD
          REAL     TEMPMN
          REAL     TEMPMX
          REAL     WINDSP

c         Growth vars:
c         ::::::::::::
          REAL     GRORT
          REAL     LAI
          REAL     LI

c         Part vars:
c         ::::::::::
          REAL     AERLDM
          REAL     FDWDT
          REAL     FLODGE
          REAL     PARCE
          REAL     RESPCF
          REAL     ROOTF
          REAL     STKDM
          REAL     STKWM
          REAL     TDMAS

c         Parameters
c         ::::::::::
          REAL     ALAT
c          REAL     ALLOWLODG
          REAL     ANAERF
c          REAL     BOOTLI
          REAL     CANHGT
          REAL     CODERD
          REAL     CWSI
          REAL     DWDT
          INTEGER  IDOY
          INTEGER  IW
c          REAL     LODGE_LI
          INTEGER  MDL
          INTEGER  NEGGRO
       
          REAL     ROWSPC
          REAL     RUNOFF
          REAL     SLPF   !CHP 2/14/2011
c          REAL     SCV
c     ::::::::::::::::::::::::::::::::

c     Local variables:
c     ::::::::::::::::


          REAL     GROSSP
          REAL     LODGE
          REAL     OLDFLODGE
          REAL     PAR
c         MJ, 2007-05-15: Respiration coeff
          REAL     RESPQ10
c         Conversion coeff:
          REAL     Respcon
          REAL     RESPGCF


c         For reading species/cult coeffs:
          LOGICAL SPE_ERR

c         Maximum photosynthetic conversion efficiency
c         Units: g/MJ (grams of photosynthate per MJ solar radiation (per sq m??))
          REAL MX_PAR_CONV


c         Base temperature for photosynthesis (plant does not 
c         photosynthesise if ambient temperature below this)
          REAL TBasePhotos


c     LODGING VARIABLES: 
c     ::::::::::::::::::::::::::::::::::    
c         aerial mass effect on lodging:
          REAL lodgeam
c         soil water effect on lodging:
	    REAL lodgeswc
c         wind effect on lodging
          REAL lodgewind
c         Allow lodging? (1 = yes, 0 = no)
          INTEGER allowlodg    
c         Severity of lodging (calculated here)
          REAL LODGE_LI
          REAL LG_RATING, LG_AMBASE, LG_AMRANGE, LG_GP_REDUC
c         Max reduction in fractional interception due to lodging
          REAL LG_MAX_RATING, LG_CRIT_WIND
          REAL PRECIPM
c         For reading cultivar values:
          LOGICAL C_ERR

c     MJ, July 2011.  CO2 'fertilisation effect'
c     ::::::::::::::::::::::::::::::::::::::::::          
c     Array of CO2 concentrations (ppm)
      REAL  CO2X(10)    
c     Array of corresponding relative photosynthesis rates
      REAL  CO2Y(10)    
c     Atmospheric CO2 content (ppm)
      REAL, INTENT(IN) ::  CO2            
c     Linear interpolation function
c     (defined in UTILS.FOR, provided by CSM)
      REAL  TABEX
c     Loop counter
      INTEGER I
c     Short name for species coeff name:
      CHARACTER*10 CO2_READ
c     Relative photosynthesis rate:
      REAL PCO2

!         Unit number for output
!          INTEGER SCLUN   !CHP

c     ::::::::::::::::::::::::::::::::::

      PARAMETER(Respcon = 0.0033871)


c     *****************************************************************

c     Copy values from 'objects' to local variables:
c     ::::::::::::::::::::::::::::::::::::::::::::::
c         Climate vars:
c         :::::::::::::
          SOLRAD = Climate%SOLRAD
          TEMPMN = Climate%TEMPMN
          TEMPMX = Climate%TEMPMX
          WINDSP = Climate%WINDSP

c         Growth vars:
c         ::::::::::::
          GRORT  = Growth%GRORT
          LAI    = Growth%LAI
          LI     = Growth%LI   

c         Part vars:
c         ::::::::::
          AERLDM = Part%AERLDM
          FDWDT  = Part%FDWDT
          FLODGE = Part%FLODGE
          PARCE  = Part%PARCE
          RESPCF = Part%RESPCF
          ROOTF  = Part%ROOTF
          STKDM  = Part%STKDM
          STKWM  = Part%STKWM
          TDMAS  = Part%TDMAS

      IF (CONTROL%DYNAMIC .EQ. SEASINIT) THEN
c         :::::::::::::::::::::::::::::::::::

c     Cultivar / species coeff:
c     Species coefficients...
c     ::::::::::::::::::::::::
         
c          SCV    = 0.2
c         # remove SCV completely!
c         ARE THESE ACTUALLY USED???????????????????????????????????
c          CALL GET_SPECIES_COEFF(SCV,    'SCV',    Control, SPE_ERR)

c         Respiration coefficient: 
c         Increase in respiration rate per 10 C rise in temperature
          RESPQ10 = 1.68
          CALL GET_SPECIES_COEFF(RESPQ10,    'RespQ10', 
     &                           Control, SPE_ERR)

c         
          RESPGCF  = .242
          CALL GET_SPECIES_COEFF(RESPGCF,    'RESPGCF', 
     &                           Control, SPE_ERR)


c     :::::::::::::::::::::::::

c     Get lodging values from cultivar file:
c     ::::::::::::::::::::::::::::::::::::::
c         First set default values:
          LG_RATING   =   8.
          LG_AMBASE   = 220.
          LG_AMRANGE  =  30.
          LG_GP_REDUC =   0.28

c         Then attempt to read from file:
c         :::::::::::::::::::::::::::::::
c         Lodge rating
c          CALL GET_CULTIVAR_COEFF(LG_RATING, 'LG_RATING', 
c     &                                         Control, C_ERR)

c         Aerial mass at which lodging starts:
          CALL GET_CULTIVAR_COEFF(LG_AMBASE, 'LG_AMBASE', 
     &                                         Control, C_ERR)

c         Range in Aerial mass in which lodging occurs
          CALL GET_CULTIVAR_COEFF(LG_AMRANGE, 'LG_AMRANGE', 
     &                                         Control, C_ERR)

c         Max photosynthesis reduction of fully-lodged cane
          CALL GET_CULTIVAR_COEFF(LG_GP_REDUC, 'LG_GP_REDUC', 
     &                                         Control, C_ERR)

c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c     Now get lodging species variables:
c     ::::::::::::::::::::::::::::::::::
c         Set default values
          LG_MAX_RATING = 8.
          LG_CRIT_WIND  = 200.

c         Read from file
c         ::::::::::::::
c         Maximum lodging rating (ha ha... just in case the 
c         plant breeders manage to make an extra-badly-lodging
c         variety!)
          CALL GET_SPECIES_COEFF(LG_MAX_RATING,'LG_RATING',
     &                           Control, SPE_ERR)

c         Critical wind speed that causes lodging
c     ## Error!!!
          CALL GET_SPECIES_COEFF(LG_CRIT_WIND,'LG_CRIT_WIND',
     &                           Control, SPE_ERR)

c      Photosynthesis variables:
c     :::::::::::::::::::::::::::::::
c         Maximum photosynthetic rate (g/MJ)
c         Default:
          MX_PAR_CONV = 9.9
c         Read from cultivar file:
          CALL GET_CULTIVAR_COEFF(MX_PAR_CONV, 'MaxPARCE', 
     &                                         Control, C_ERR)  

c         Base temperature for photosynthesis:
c         Default:
          TBasePhotos = 7.
          CALL GET_SPECIES_COEFF(TBasePhotos,'TBasePhotos',
     &                           Control, SPE_ERR)

c         MJ, July 2011
c         Read in the array of CO2 sensitivities:
c         This is cumbersome but is compatible with the existing
c         coefficient-reading subroutine.
c         X-values (CO2 PPM)
          DO I=1, 10
            WRITE(CO2_READ, '(5HCO2X_, I0)') I
c           Initialise default relative values to 1 always:
            CO2X(I) = I * 100.  ! 100, 200, etc ppm
c           Attempt to read from species file
            CALL GET_SPECIES_COEFF(CO2X(I), TRIM(CO2_READ), 
     &                           Control, SPE_ERR)
          ENDDO
c         Y-values (CO2 PPM)
          DO I=1, 10
            WRITE(CO2_READ, '(5HCO2Y_, I0)') I
c           Initialise default relative values to 1 always:
            CO2Y(I) = 1.  ! always 1 by default
c           Attempt to read from species file
            CALL GET_SPECIES_COEFF(CO2Y(I), TRIM(CO2_READ), 
     &                           Control, SPE_ERR)
          ENDDO


!       WRITE(*, '(10(F10.3))') CO2X
!       WRITE(*, '(10(F10.3))') CO2Y

c     Initialisation:
c     :::::::::::::::
          GROSSP = 0.
          Out%GROSSP = 0.
          Out%DWDT   = 0.
          PARCE  = 0.
          TDMAS  = 0.
          Oldflodge = 0.
          aerldm = 0.
          stkdm  = 0.
          stkwm  = 0.
          Part%FLODGE = 0.
          Flodge = 0.

c         Allow negative growth = 0 for now
          NEGGRO = 0
c         Daily increment in biomass (I think)
c         Move to init section?
          DWDT = 0.
c         Ken Boote tech. LI
c          BOOTLI = 0.
c         Necessary for different solar radiation alg.
          CODERD = 2.
c     ::::::::::::::




      ELSEIF (CONTROL%DYNAMIC .EQ. RATE) THEN
c         :::::::::::::::::::::::::::::::::::

c     Work out the day of the year from YRDOY
      IDOY = CONTROL%YRDOY -  ((CONTROL%YRDOY / 1000)*1000.)

c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c     Added 3/10/2003 - sdb 
c     MJ, Feb 2008: replaced the following line:
c     respcf = 1.68 **((((tempmx+tempmn)/2.0)-10.0)/ 10.0) * Respcon
c     with:
      respcf = RESPQ10 **((((tempmx+tempmn)/2.0)-10.0)/ 10.0) * Respcon
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c     MJ, Feb 2008: 
c     1. changed 9.9 (max photosynthetic conversion rate g/MJ)
c        to MX_PAR_CONV and made this a cultivar coeffcient:
c     2. Changed 7.0 (base temperature for photosynthesis) to TBasePhotos 
c	 parce = 9.9 *(1.0 - exp(-0.08 *(((tempmx+tempmn)/2.0) - 7.0)))
c     We may want to give some thought to what the 0.08 term refers to...
c     It affects the response of photosynthesis to temperature.

	parce = MX_PAR_CONV *
     &        (1.0 - exp(-0.08 *
     &        (((tempmx+tempmn)/2.0) - TBasePhotos)))
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c     MJ, Feb 2008: Michiel Smit (I think) noticed that PARCE is 
c     negative when temperature drops below TBasePhotos.  This means
c     that GROSSP is negative as well.  
          PARCE = MAX(0., PARCE)
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


       IF(MDL.EQ.1) THEN
c Simple Hesketh/McCree approach............................................
c Heat of combustion of Dm is assumed to be 4200 cal/g; PAR is in cal/cm2
       if(coderd.eq.1.) solrad=solrad/0.09454
C CNB JUN2002 - SWITCHED THE STATEMENT BELOW OFF SINCE PHOTOS NOW WORKS
C               ON MJ/m². THE CHANGE WAS VERIFIED AND WORKS CORRECT 
C       if(coderd.eq.2.) solrad=solrad/(4.187*0.01)

c CNB&AS Lodging factor Jan 2002
c We've included a reduction in RUE due to lodging here after some
c indication of the work by Singh 1998 ASSCT


c    CANESIM lodging (MJ, 2006/09/06)
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c

c     ** NOTE **
c     Add weight of intercepted rainwater to AERL Wet Mass for lodging
c     purposes.
c         Weight of intercepted rainwater
          PRECIPM = 0.
c     Cultivar variables:
c          LG_RATING   =   8.
c          LG_AMBASE   = 220.
c          LG_AMRANGE  =  30.
c          LG_GP_REDUC =   0.28

c          LG_MAX_RATING = 8.
c          LG_CRIT_WIND  = 200.

c         Previous value of lodging
      	oldflodge = flodge
c         Initialise lodging variables:
          flodge    =  0.
          lodgeswc  =  0.
          lodgewind =  0.
          lodgeam   =  0.
          LODGE_LI  =  0.

c     Override the allowlodge setting:
c     Assume that allowlodge is ON
c     Then is lodgerating is 0 (or less), do not
c     bother calculating lodging...

c     MJ, Mar 2008: lodgerating has been deprecated...
c     Following line changed:
c      IF (LG_RATING .LT. 0.01) ALLOWLODG = 0
      IF (LG_AMBASE .GT. 998.) ALLOWLODG = 0
c     Now, lodging is disabled entirely if LG_AMBASE = 999 or more


      IF (ALLOWLODG .EQ. 1) THEN
c         Soil water content effect on lodging:
c         :::::::::::::::::::::::::::::::::::::
c         Only simulate the water effect on lodging if water balance is
c         simulated:
          IF (ISWITCH%ISWWAT .EQ. 'Y') THEN
c             High runoff = high SWC = high lodge risk
                  IF(RUNOFF .gt. 0.) THEN 
                      lodgeswc = 2. 
c	                WRITE(*, '(A, F8.4)') 'LODGESWC: ', lodgeswc
	            ENDIF
          ELSE
              lodgeswc = 0. 
          ENDIF
c         :::::::::::::::::::::::::::::::::::::

c         Wind effect on lodging:
c         :::::::::::::::::::::::
c             lodgewind=amax1(0.0,amin1(2.0,(windsp-200.0)/100.0)) !risk factor for wind
c             ::::::::::::::::::::::::::::::::
              lodgewind = MAX(0., MIN(2., (WINDSP - LG_CRIT_WIND) 
     &                                    / LG_CRIT_WIND))
c         ::::::::::::::::::::::::::::::::

c         High aerial mass effect on lodging:
c         :::::::::::::::::::::::::::::::::::
c             To avoid div/0 error, and to make sense:
	        IF (STKDM .gt. 1.) THEN
c                 Note: 220 should be a variety parameter
                  lodgeam = LG_RATING * (((aerldm/(stkdm/stkwm)+PRECIPM)
     &                      - LG_AMBASE) / LG_AMRANGE)
c                Combined effects on lodging:
c                ::::::::::::::::::::::::::::      	
                 lodge = lodgeam + lodgewind + lodgeswc

	        ENDIF
c         :::::::::::::::::::::::::::::::::::
      


              !lodge range 0..8
              if (lodge.lt.0.0) lodge=0.0
              if (lodge .gt. LG_MAX_RATING) lodge = LG_MAX_RATING
c         ::::::::::::::::::::::::::::

c         Effect of lodging on light interception:
c         These effects are applied in CANOP3.for
c         ::::::::::::::::::::::::::::::::::::::::
              LODGE_LI = lodge / LG_MAX_RATING
c         ::::::::::::::::::::::::::::::::::::::::

c         Reduce photosynthesis accordingly:
c         ::::::::::::::::::::::::::::::::::
      	    flodge=amax1(lodge/LG_MAX_RATING,oldflodge)

c          WRITE(*,*) 'Lodging: ', LODGE_LI, LODGE, LODGEAM
      ENDIF

          PAR=SOLRAD*0.5

c         MJ, July 2011
c         :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c         CO2 'fertilisation effect' - taken from CERES-Maize
c         CO2Y, CO2X are arrays defining relative photosynthesis rate
c         (Y) to atmospheric CO2 concentration (X, ppm).
c         TABEX interpolates a relative pg value for whatever the CO2
c         concentration is.  10 is the length of the datapoint array,
c         defined in the SPECIES file.
          PCO2 = 1.
          PCO2  = TABEX (CO2Y,CO2X,CO2,10) 
          
          
c         reduce photosynthesis by maximum 28% due to lodging. Based on Singh 1998 ASSCT
c         PARCE IS g/MJ
          GROSSP=PARCE*PAR*LI*(1.-(flodge * LG_GP_REDUC)) * PCO2

          Out%PAR = PAR
!      CALL GETLUN('WORK.OUT',SCLUN)
c      WRITE(SCLUN, '(I5, A, A, F10.5)') IDOY, ' SRAD is ',ACHAR(9), 
c     -                               Climate%SOLRAD
c     :::::::::::::::::::::::::::::::::

	!convert from g/m² to t/ha
	    GROSSP=GROSSP*(10000.0/1000000.0)
          DWDT=(1.0 - RESPGCF)*(GROSSP-respcf*TDMAS)

c     Some output:
c      WRITE(*, *) 'GROSSP', GROSSP, '; DWDT', DWDT



C 'TD' is mean daylight temperature used in a regression with LCA 
C  photosynthesis where a Pmax was at 33 deg C.

       ELSE

       ENDIF
C NET CARBON GAIN IS REDUCED BY WATER AND ANAEROBIC STRESS
c       RESPWT=PGROSS-DWDT
!      DWDT=AMIN1(DWDT*CWSI*ANAERF,DWDT)
!      14 Feb 2011 CHP added SLPF factor
       DWDT=AMIN1(DWDT*CWSI*ANAERF*SLPF,DWDT)

C If negative growth is allowed it cannot occur before a limited TDMAS has 
C accumulated
       IF(DWDT.LT.0.0) THEN
        IF(TDMAS.GT.5.0.AND.NEGGRO.EQ.1) THEN
              WRITE(*,*)'Growth is negative'  
        ELSE
              DWDT=0.0
        ENDIF
       ENDIF

c      Integrate TDMAS
       TDMAS=TDMAS+DWDT

c      Set FDWDT to net change in aboveground dry mass
	 FDWDT=DWDT

C      ALLOCATION OF GROWTH TO ROOTS * 100 TO CONVERT T/HA TO g/m^2
       ROOTF=AMIN1(ROOTF,0.7) 

c     GRORT now assigned in SC_Partit.for      
c       GRORT=DWDT*ROOTF*100.0

       if(coderd.eq.1.) solrad=solrad*0.09454
C SDB  3/6/2003 this gave incorrect solrad values replaced by below
C       if(coderd.eq.2.) solrad=solrad*(4.187*0.01)
       if(coderd.eq.2.) solrad=solrad

c     Copy values from local variables to 'objects':
c     ::::::::::::::::::::::::::::::::::::::::::::::
c         Climate vars:
c         :::::::::::::
          Climate%SOLRAD = SOLRAD
          Climate%TEMPMN = TEMPMN
          Climate%TEMPMX = TEMPMX
          Climate%WINDSP = WINDSP

c         Growth vars:
c         ::::::::::::
c          Growth%GRORT   = GRORT
          Growth%LAI     = LAI
          Growth%LI      = LI   

c         Part vars:
c         ::::::::::
          Part%AERLDM = AERLDM
          Part%FDWDT  = FDWDT
          Part%FLODGE = FLODGE
          Part%PARCE  = PARCE
          Part%RESPCF = RESPCF
          Part%ROOTF  = ROOTF
          Part%STKDM  = STKDM
          Part%STKWM  = STKWM
          Part%TDMAS  = TDMAS

          Out%GROSSP = GROSSP
          Out%DWDT   = DWDT 
          PAR = 0.


       IF(MDL.EQ.1) RETURN
       IF(IW.NE.1) RETURN
c       WRITE(16,100)DOY,LAI,bootLI,PMAXML,SOLRAD,DPAR,DAYKB,DAYINT,
c     -   QECAN, ENEFF2,PGROSS,RMCOEF,RESPWT,DWDT


c     End of RATE calculation
c     :::::::::::::::::::::::      
      ELSEIF (CONTROL%DYNAMIC .EQ. SEASEND) THEN
          oldflodge = 0.
          lodge     = 0.
          PAR = 0.
          Out%PAR = 0.
      ENDIF
      
c     ::::::::::
      

 100   FORMAT(F4.0,2F6.2,17F8.3)
       RETURN 
       END 

