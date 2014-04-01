!==============================================================================
! Subroutine SALUS, GENERIC SALUS CROP MODEL FOR DSSAT
! Simple version of SALUS (System Approach to Land Use Sustainability) crop
! model to simulate crop growth, development and yield. Progress toward
! maturity is modeled using the concept of relative thermal time. Biomass
! computation uses  the radiation use efficiency approach.
!==============================================================================
! 06/24/2009  Translated from Visual Basic (Kofikuma Dzotsi)
! 05/10/2010  Added new equation for root partitioning coefficient (KD)
! 07/26/2010  Added environment extremes (KD)
! 07/27/2010  Implemented exponential functional for computing root depth factor
! ... see SAL_Subs
! 08/25/2011  Added TFREEZE and FREEZED to stop growth and dev at cold temperatures
! 11/16/2011  Added "dynamic" harvest index (reduced with slow biomass accumulation)
!==============================================================================

subroutine SALUS(control, iswitch, weather, soilprop, st,            & !Input
                 yrplt, eop, sw, rwu, trwup, nh4, no3, spi_avail,    & !Input
                 kcan, mdate, rlv, xhlai, uno3, unh4, puptake)         !Output
!------------------------------------------------------------------------------  
use ModuleDefs
implicit none
save
!------------------------------------------------------------------------------ 
character(len=1) :: iswwat, iswnit, iswpho
character(len=6) :: section, varno, errkey
character(len=12) :: filec
character(len=16) :: vrname
character(len=24) :: ctime, showtime
character(len=30) :: fileio
character(len=80) :: pathcr
integer :: currentime, time
logical :: fexist, freezed, killed, germinate, emerge
integer :: err, layer, cumslowdev, cumslowdevstop, dae, dap, doy, doyp, dynamic, errnum, linc, lnum, lunio
integer :: mature, mdate, nlayr, yrdoy, yrend, yrplt, found
real :: biomass, bioatlaimax, biomassc, biomassinc, biomassroot, biomassrootc, cumtt, dbiomass, dbiomassroot
real :: deltalai, emgint, emgslp, grainyield, grainyieldc, hrvindex, hrvindstr, kcan, laimax, laip1, laip2
real :: plantpop, rellai, rellaip1, rellaip2, rellaimax, rellais, reltt, relttemerge, relttsn, relttsn2
real :: relrue, rootdepth, rootpartcoeff, rowspacing, rue, ruemax, snparlai, snparrue, sowdepth, srad
real :: stresrue, streslai, tbasedev, teff, tfreeze, tmax, tmin, toptdev, ttaccumulator, ttaccumulator1
real :: ttemerge, ttgerminate, ttmature, xhlai, xlai, rellaiyest, dtt, wpseed

! Constants
real, parameter :: coldfac = 1.00, heatfac = 1.00, relttp1 = 0.15, relttp2 = 0.50       
            
! Root Growth and Water Uptake
real :: droughtfac, eop, ep1, rlwr, trwup
real, dimension(nl):: rwu, st, sw, dlayr, rootlayerfrac, rootmasslayer, rlv
      
! N Model
real :: NConcAct, NConcMin, NConcOpt, NConcOpt_par(3), NDemand_Kg, NFAC, NPlant_Kg, NSupply_Tot, SWaterFac	  
real, dimension(nl):: NH4, NO3, SNH4, SNO3, UNO3, UNH4
real, dimension(nl):: NSupply_kg, NSupplyRed_Kg, NUptake
      
! P Model        
real PConcAct, PConcMin, PConcOpt, PConcOpt_par(3), PDemand_Kg, PSupply_Tot, PFAC, PPlant_Kg
real, dimension(nl):: PSupplyRed_Kg, PUptake, SPi_AVAIL      
	
!------------------------------------------------------------------
! Constructed variable types based on definitions in ModuleDefs.for
Type (ControlType) control
Type (SwitchType)  iswitch
Type (SoilType)    soilprop
Type (WeatherType) weather
 
! Transfer values from constructed data types into local variables
dlayr   = soilprop % dlayr
dynamic = control  % dynamic
fileio  = control  % fileio
iswnit  = iswitch  % iswnit
iswpho  = iswitch  % iswpho
iswwat  = iswitch  % iswwat
nlayr   = soilprop % nlayr      
srad    = weather  % srad  
tmax    = weather  % tmax  
tmin    = weather  % tmin 
yrdoy   = control  % yrdoy      
 
!==============================================================================
! INITIALIZATION AND INPUT DATA
!==============================================================================
! IF(DYNAMIC.EQ.RUNINIT .OR. DYNAMIC.EQ.SEASINIT) THEN
if(dynamic == seasinit) then
!==============================================================================       
        
!-------------------------------------------------------------------------
!Initialize Variables
!-------------------------------------------------------------------------
biomass = 0.0
bioatlaimax = 0.0
biomassc = 0.0
biomassinc = 0.0
biomassroot = 0.0 
biomassrootc = 0.0	
cumslowdev = 0
cumslowdevstop = 0
cumtt = 0.0
dae = -1
dap = 0
dbiomass = 0.0 
dbiomassroot = 0.0
deltalai = 0.0	
dtt = 0.0
emerge = .false.
emgint = 0.0
emgslp = 0.0
freezed = .false.
germinate = .false.
hrvindex = 0.0 
hrvindstr = 0.0
kcan = 0.0
killed = .false. 
laimax = 0.0
laip1 = 0.0
laip2 = 0.0
layer = 0
mature = 0
mdate = -99
plantpop = 0.0
rellai = 0.0
rellaip1 = 0.0
rellaip2 = 0.0
rellaimax = 0.0      
rellais = 0.0
reltt = 0.0
relttemerge = 0.0
relttsn = 0.0
relttsn2 = 0.0
relrue = 0.0
rootdepth = 0.0
rootpartcoeff = 0.0
rowspacing = 0.0
rue = 0.0
ruemax = 0.0 
snparlai = 0.0	
snparrue = 0.0
sowdepth = 0.0
streslai = 0.0
stresrue = 0.0
tbasedev = 0.0
teff = 0.0
tfreeze = 0.0
toptdev = 0.0					    
ttaccumulator = 0.0
ttaccumulator1 = 0.0		
ttemerge = 0.0
ttgerminate = 0.0
ttmature = 0.0
xhlai = 0.0
xlai = 0.0	
rellaiyest=0.0    
wpseed = 0.0  
	            
!Root growth and water uptake
droughtfac = 1.0
ep1 = 0.0
rlwr = 0.0
do layer = 1, nlayr
   RootLayerFrac(layer) = 0.0
   RootMassLayer(layer) = 0.0
   RLV(layer) = 0.0
end do
           
! N Model
NConcAct = 0.0
NConcMin = 0.0
NConcOpt = 0.0
NDemand_Kg = 0.0 
NFAC = 1.0
NPlant_Kg = 0.0
NSupply_Tot = 0.0
SWaterFac = 1.0 
	
do layer = 1, 3
   NConcOpt_par(layer) = 0.0
end do
      
do layer = 1, nlayr
   SNH4(layer) = 0.0
   SNO3(layer) = 0.0
   UNO3(layer) = 0.0
   UNH4(layer) = 0.0
   NSupply_kg(layer) = 0.0
   NSupplyRed_Kg(layer) = 0.0
   NUptake(layer) = 0.0
end do	
    
! P Model        
PConcAct = 0.0
PConcMin = 0.0
PConcOpt = 0.0
PDemand_Kg = 0.0
PSupply_Tot = 0.0
PFAC = 1.0
PPlant_Kg = 0.0
	
do layer = 1, 3
   PConcOpt_par(layer) = 0.0
end do
      
do layer = 1, nlayr
   PSupplyRed_Kg(layer) = 0.0
   PUptake(layer) = 0.0
end do		
 
!-------------------------------------------------------------------------
! Read input file name (ie. DSSAT40.INP) and path
!-------------------------------------------------------------------------
call getlun('fileio', lunio)
open(lunio, file=fileio, status='old', iostat=err)  
if(err /= 0) call error(errkey, err, fileio, 0)

read(lunio, '(////////,15x,a12,1x,a80)' , iostat=err) filec, pathcr; lnum = lnum + 9
if(err /= 0) call error(errkey, err, fileio, lnum)

!-------------------------------------------------------------------------
! Read Planting Details Section
! SALUS needs three inputs from here: PLANTPOP,ROWSPACING,SOWDEPTH 
!-------------------------------------------------------------------------
section = '*PLANT'
call find(lunio, section, linc, found) ; lnum = lnum + linc
if(found == 0) then
   call error(section, 42, fileio, lnum)
else
   read(lunio, '(25x,f5.0,13x,f5.0,7x,f5.0)', iostat=err) plantpop, rowspacing, sowdepth
   lnum = lnum + 1  
   if(err /= 0) call error(errkey, err, fileio, lnum)
end if

!-------------------------------------------------------------------------
! Read SALUS crop model parameters from DSSAT.INP (pars from SALUS045.CUL)
!-------------------------------------------------------------------------
section = '*CULTI'
call find(lunio, section, linc, found) ; lnum = lnum + linc
if(found == 0) then
   call error(section, 42, fileio, lnum)
else
   read(lunio, '(a6, 1x, a15, 1x, 20f10.3)', iostat=err) varno, vrname,                                  &
        emgint, emgslp, ttgerminate, hrvindex, laimax, rellaip1, rellaip2, relttsn, snparlai, ttmature,  &
        ruemax, snparrue, tbasedev, toptdev, rlwr, wpseed, relttsn2, streslai, stresrue, tfreeze  
        lnum = lnum + 1 
   if(err /= 0) call error(errkey, err, fileio, lnum)
 
! Need to add nutrient concentrations at different stages of growth to cultivar parameters, units in kg/kg
! Multiply by 100 to obtain percent or g/100g
NConcOpt_par(1) = 0.0440
NConcOpt_par(2) = 0.0164
NConcOpt_par(3) = 0.0128

PConcOpt_par(1) = 0.0062
PConcOpt_par(2) = 0.0023
PConcOpt_par(3) = 0.0018

end if
close(lunio)

! Compute thermal time to emergence based on Urs' equation:
ttemerge = emgint + emgslp*sowdepth   

call SALUS_Roots(dynamic, soilprop, dBiomassRoot, dtt, xhlai, laimax, rlwr, st, sw, plantpop, sowdepth,    &  !Input
     RootDepth, RootLayerFrac, RootMassLayer,RLV)	                                                          !Output     
   
if(iswnit /= 'N') then         
call SALUS_NPuptake(dynamic, rwu, RootLayerFrac, dae, biomass, biomassroot, NConcOpt_par, NSupply_kg, 	& !Input
     reltt, relttemerge, soilprop, NConcAct, NConcMin, NConcOpt, NSupplyRed_Kg,  	                    & !Output
     NSupply_Tot, NDemand_Kg, NPlant_Kg, NUptake, SWaterFac, NFac)                                        !Output
end if
   
if(iswpho /= 'N') then
call SALUS_NPuptake(dynamic, rwu, RootLayerFrac, dae, biomass, biomassroot, NConcOpt_par, NSupply_kg, 	& !Input
     reltt, relttemerge, soilprop, NConcAct, NConcMin, NConcOpt, NSupplyRed_Kg,  	                    & !Output
     NSupply_Tot, NDemand_Kg, NPlant_Kg, NUptake, SWaterFac, NFac)                                        !Output
end if

! Open Salus.out to store output variables
inquire (file='SALUS.OUT', exist=fexist)
if(fexist) then
  open(unit=555, file='SALUS.OUT', status='old', iostat=errnum, position='rewind')
else
  open(unit=555, file='SALUS.OUT', status='new', iostat=errnum)
endif

currentime = time()
showtime = ctime(currentime:currentime)

write(555, '("Results of SALUS-Simple Plant Growth Model Simulation")')
write(555, '(/a)') showtime
write(555, 12)        
   12	  FORMAT(                                                          &
     / '  YRDOY    DAP     DTT   CUMTT   RELTT     RUE    dLAI   dBIOM     '&
//'       XHLAI      ROOT   BIOMASS  WATFAC',                                 &
     /,'  yrdoy    dap   deg-d   deg-d   (0-1)    g/MJ m2/m2/d  g/m2/d     '&
//'        m2/m2     kg/ha     kg/ha   (0-1)',                                &
     /,'  -----   ----   -----   -----   -----    ---- -------  ------     '&
//'        -----     -----     -----   -----')
   	
! Initialize Output PlantGrow.OUT
call SALUS_Opgrow(control, iswitch, dtt, mdate, biomassrootc, biomassc, xhlai, yrplt, droughtfac)

! Initialize Output Summary.OUT
call SALUS_Opharv(control, mdate, biomass, grainyield, xhlai)

!==============================================================================
! RATE CALCULATIONS
!==============================================================================
else if(dynamic == rate) then
!==============================================================================
if(dae .gt. -1) then
! repeat everyday, but after emergence
  dae = dae + 1
endif
dap = yrdoy - yrplt
rellaiyest = rellai

!------------------------------------
! Compute Water Stress Factors       
!------------------------------------
droughtfac  = 1.0
!        TURFAC = 1.0
if(iswwat /= 'n') then
   if(eop > 0.0) then
      ep1 = eop * 0.1
!              IF (TRWUP / EP1 .LT. RWUEP1) THEN
!                 TURFAC = (1./RWUEP1) * TRWUP / EP1
!              ENDIF
    if(ep1 >= trwup) then
       droughtfac = trwup / ep1
    end if
   end if
end if	

!------------------------------------
! Calculate daily thermal time (DTT) for development       
!------------------------------------        
call dailyThermalTime(tmax, tmin, tbasedev, toptdev, ttmature, teff, reltt, dtt, cumtt)

!------------------------------------
! Added 11/16/2011 - Get Biomass right after LAIMAX (at flowering) for use in correcting harvest index     
!------------------------------------  
if(bioatlaimax == 0.0) then
   if(reltt >= relttsn) then
      bioatlaimax = biomass
  end if
end if
        
!------------------------------------
! Check for germination and emergence       
!------------------------------------ 	
if(dtt > 0.0) then
   if(.not. emerge) then
	  if(.not. germinate) then
		 call germination(ttaccumulator1, ttgerminate, ttmature, dtt, ttemerge, killed, germinate)	
         if(killed) then
            mdate = yrdoy
            write(555, '(/, "Crop model stopped on day ", I7, " ..too small value of TT to maturity.")') mdate
            return
         end if   !Crop killed
              
	  else !If crop has already germinated then
		 call emergence(ttaccumulator, dtt, ttemerge, ttmature, emerge)	
		 if(germinate .and. emerge) then
		    dae = 0
! Initialize root weight, aboveground biomass weight, root depth, and root length density at emergence  
			call initializeRoots(reltt, plantpop, sowdepth, dlayr, nlayr, rlwr, wpseed,    &   !Inputs
                 biomassroot, biomass, rootdepth, rlv)                                         !Outputs
		 end if
	  end if ! EndIf crop has germinated
   else ! If crop has already emerged then
	  call maturity(reltt, mature)
	  if(mature == 1) then
		 mdate = yrdoy
		 !11/16/2011 - Modification to harvest index with low accumulation of biomass after LAIMAX as discussed with Bruno today on Rm 201
		 hrvindstr = hrvindex * min(1.00, (biomass-bioatlaimax)/(0.55*biomass))
		 grainyield = biomass * hrvindstr
		 return
	  end if

! Compute potential LAI S-Curve parameters
	  call estimateLaiPar(relttp1, relttp2, rellaip1, rellaip2, laip1, laip2)
		
! Calculate Leaf Area Index based on ALMANAC's simple functions of relative LAI vs. relative development time:
	  call leafAreaIndex(laimax, laip1, laip2, relttsn, snparlai, droughtfac, heatfac, coldfac, nfac, pfac,   &
     	   reltt, xhlai, relttsn2, streslai, rellai, deltalai, rellais, rellaimax, rellaiyest)
 
! Calculate Radiation Use Efficiency based on ALMANAC's simple function of relative RUE vs. relative development time:	
	  call radiationUseEfficiency(relttsn, ruemax, snparrue, reltt, droughtfac, heatfac, coldfac, nfac, pfac, stresrue, &
           relrue, rue)	
			 
! Calculate daily biomass increment (C fixed) in g m-2 based on ALMANAC and CERES
	  call biomassIncrement(plantpop, rowspacing, srad, droughtfac, heatfac, coldfac, nfac, pfac,   &
     	   rue, xhlai, kcan, biomassinc)	                !Missing CO2 effect
			 
! Partition C fixed to various organs, for the moment only tops and roots:		 
	  call partitioning(biomassinc, reltt, rootpartcoeff, dbiomass, dbiomassroot)      
        
! Calculate root growth, partitioning to soil layers and root lengh volume (RLV)        
      call SALUS_Roots(dynamic, soilprop, dbiomassroot, dtt, xhlai, laimax, rlwr, st, sw, plantpop, sowdepth,   & !Input
            RootDepth, RootLayerFrac, RootMassLayer, rlv)	                                                      !Output		
			  	  
   end if ! End if crop has emerged	
	 
   if(emerge) then
	  relttemerge = 0.0
   else
	  relttemerge = relttemerge + dtt/ttmature
   end if
     		  
end if ! End if DTT is greater than 0.0		

! Check for frost kill; kill crop if too slow accumulation of DTT or if TMIN < TFREEZE
call environment(reltt, relttsn, tmin, cumslowdev, dtt, cumslowdevstop, tfreeze, killed, freezed)
if(killed) then
   mdate = yrdoy
   !11/16/2011 - Modification to harvest index with low accumulation of biomass after LAIMAX as discussed with Bruno today on Rm 201
hrvindstr = hrvindex * min(1.00, (biomass-bioatlaimax)/(0.55*biomass))
grainyield = biomass * hrvindstr
write(*, '(/, "Model stopped on day ", I7, " due to slow development.")') mdate
return
end if
		
! Frost
if(freezed) then
 !11/16/2011 - Modification to harvest index with low accumulation of biomass after LAIMAX as discussed with Bruno today on Rm 201
hrvindstr = hrvindex * min(1.00, (biomass-bioatlaimax)/(0.55*biomass))
grainyield = biomass * hrvindstr
write(*, '(/, "Model stopped on day ", I7, " due to frost.")') mdate
return
end if          
  
!==============================================================================
! INTEGRATION
!==============================================================================
else if(dynamic == integr) then
!==============================================================================
xhlai = max(xhlai, 0.0)
biomass = max(biomass+dbiomass, 0.0)
biomassroot = max(biomassroot+dbiomassroot, 0.0)
 
! Convert biomass from g/m-2 to kg ha-1	        
call convertb(biomass, biomassroot, grainyield, biomassc, biomassrootc, grainyieldc)  

! Activate N routine if N limitation is simulated:
if(iswnit /='N') then
   ! N uptake calculations:
   do layer = 1, nlayr
     SNO3(layer) = NO3(layer) / SOILPROP % KG2PPM(layer)
     SNH4(layer) = NH4(layer) / SOILPROP % KG2PPM(layer)
     NSupply_kg(layer) = SNO3(layer) + SNH4(layer)
   end do	    
   
   call SALUS_NPuptake(dynamic, rwu, RootLayerFrac, dae, biomass, biomassroot, NConcOpt_par, NSupply_kg, 	& !Input
        reltt, relttemerge, soilprop, NConcAct, NConcMin, NConcOpt, NSupplyRed_Kg,  	                    & !Output
        NSupply_Tot, NDemand_Kg, NPlant_Kg, NUptake, SWaterFac, NFac)                                        !Output
   
   !  NSupply_Tot = SUM(NSupply_kg)
   do layer = 1, nlayr
      UNH4(layer) = Nuptake(layer) * SNH4(layer) / (SNO3(layer) + SNH4(layer))
      UNO3(layer) = Nuptake(layer) * SNO3(layer) / (SNO3(layer) + SNH4(layer))
   end do
end if
        
! Activate P routine if P limitation is simulated
if(iswpho.ne.'N') then
   call SALUS_NPuptake(dynamic, rwu, RootLayerFrac, dae, biomass, biomassroot, NConcOpt_par, NSupply_kg, 	& !Input
        reltt, relttemerge, soilprop, NConcAct, NConcMin, NConcOpt, NSupplyRed_Kg,  	                    & !Output
        NSupply_Tot, NDemand_Kg, NPlant_Kg, NUptake, SWaterFac, NFac)                                        !Output
end if        

!==============================================================================
! OUTPUT
!==============================================================================
else if(dynamic == output) then
!==============================================================================
write(555, '(2i7, 1f8.2, 1f8.1, 5f8.3, 2f10.1, f8.3, 2f8.2, 5f8.3,1i7)') yrdoy, dap, dtt,  &
     cumtt, reltt, rue, deltalai, dbiomass, xhlai, biomassrootc, biomassc, droughtfac,     &
     laip1, laip2, rellai, rellaiyest, trwup, ep1, rlv(2), dae      

call SALUS_Opgrow(control, iswitch, dtt, mdate, biomassrootc, biomassc, xhlai, yrplt, droughtfac)
call SALUS_Opharv(control, mdate, biomass, grainyield, xhlai)     

!==============================================================================
! SEASON END
!==============================================================================
else if(dynamic == seasend) then
!==============================================================================
CLOSE(1)
call SALUS_Opgrow(control, iswitch, dtt, mdate, biomassrootc, biomassc, xhlai, yrplt, droughtfac)
call SALUS_Opharv(control, mdate, biomass, grainyield, xhlai)   

!==============================================================================
! END OF DYNAMIC 'IF' CONSTRUCT
!==============================================================================
endif
!==============================================================================
return
end subroutine SALUS
!==============================================================================


!------------------------------------------------------------------------------
! VARIABLE DEFINITION  
!------------------------------------------------------------------------------
!biomass			Total plant dry matter weight (g m-2)
!biomassc		    Total plant dry matter weight converted to kg ha-1 (kg ha-1)
!biomassinc		    Daily biomass increment (g m-2)
!biomassroot		Root dry matter weight (g m-2)
!biomassrootc	    Root dry matter weight converted to kg ha-1 (kg ha-1)
!coldfac		    Low temperature reduction factor (0-1)
!control            Composite variable containing variables related to control and/or timing of simulation.  The structure of the variable 
!                   (ControlType) is defined in ModuleDefs.for. 
!cumslowdev		    Cumulative slow development days (days) 
!cumslowdevstop	    Cumulative slow development days to stop crop model (days)	
!dae				Days after emergence
!dap				Days after planting
!dbiomass		    Incremental total plant dry matter weight (g m-2 d-1)
!dbiomassroot	    Incremental root dry matter weight (g m-2 d-1)
!dlai			    Daily increase in leaf area index (m2 m-2 d-1)
!doy				Julian day
!doyp		        Date of planting (Julian day)
!droughtfac	        Drought reduction factor (0-1)
!dtt				Daily thermal time increment (degree-days)
!dyn				Dynamic control variable
!emerge			    Emergence flag; True when crop has emerged
!emgint		        Intercept of emergence thermal time calculation
!emgslp		        Slope of emergence thermal time calculation
!frbforrt	        Fraction of new root biomass for root growth
!freezed            Logical- True if TMIN <= TFREEZE 
!heatfac		    High temperature reduction factor (0-1)
!iswitch            Composite variable containing switches which control flow of execution for model.  The structure of the variable 
!                   (SwitchType) is defined in ModuleDefs.for. 
!kcan               Canopy light extinction coefficient for daily PAR, for equidistant plant spacing
!germinate		    Germination flag; True when crop has germinated
!killed			    True when crop is killed by adverse conditions
!lai				Canopy leaf area index (m2 m-2)
!laimax		        Maximum expected Leaf Area Index (m2 m-2)
!plantpop	        Plant population (m-2)
!mature			    Maturity flag (0/1); 1 when crop has matured
!mdate              Harvest maturity date (YYYYDDD)
!nfac		        Nitrogen deficiency factor (0-1)
!parfr		        Fraction of solar radiation useable by plants (PAR)
!pfac		        Phosphorus deficiency factor (0-1)
!rellai			    Relative LAI (0-1)
!rellaip1	        Relative LAI at point 1 on the potential LAI S-curve (0-1)
!rellaip2	        Relative LAI at point 2 on the potential LAI S-curve (0-1)
!relrue			    Relative RUE (0-1)
!reltt			    Relative thermal time during life of plant (0-1)
!relttp1            Relative thermal time at point 1 on the potential LAI S-curve (0-1)
!relttp2            Relative thermal time at point 2 on the potential LAI S-curve (0-1)
!relttsn		    Relative thermal time at beginning of senescence (0-1)
!rootpartcoeff	    Fraction of assimilate that goes to roots (0-1)
!rowspacing	        Row spacing (cm)
!rue				Radiation Use Efficiency (g MJ-1)
!ruemax		        Maximum expected Radiation Use Efficiency (g MJ-1)
!snparlai	        Parameter for shape of potential LAI curve after beginning of senescence (0-1)
!snparrue	        Parameter for shape of potential RUE curve after beginning of senescence (0-1)
!sowdepth	        Sowing depth (cm)
!srad		        Daily solar radiation (MJ m-2)
!tbasedev	        Base temperature for development (degree C)
!teff			    Daily effective temperature (degree C)
!tfreeze            Freezing temperature in the sense that if TMIN <= TFREEZE, growth stops (deg C)
!tmax		        Daily maximum temperature (degree C)
!tmin		        Daily minimum temperature (degree C)
!toptdev		    Optimum temperature for development (degree C)
!ttgerminate	    Thermal time planting to germination (degree-days)
!ttemerge		    Thermal time germination to emergence (degree-days)	
!ttmature	        Thermal time planting to maturity (degree-days)
!xhlai              Healthy leaf area index (m2[leaf] / m2[ground])
