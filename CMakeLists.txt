##########################################################################################
# CMake project file for DSSAT-CSM
##########################################################################################
# Define the project and the dependencies that it has
##########################################################################################

CMAKE_MINIMUM_REQUIRED(VERSION 2.8.5)

if("${CMAKE_SOURCE_DIR}" STREQUAL "${CMAKE_BINARY_DIR}")
   message(FATAL_ERROR "DSSAT in-source builds are not permitted. \nMake a separate folder" 
   						"for building:\nmkdir build; cd build; cmake ..\nBefore that," 
   						"remove the files already created:\n"
   						"CMakeCache.txt and CMakeFiles\n")
endif("${CMAKE_SOURCE_DIR}" STREQUAL "${CMAKE_BINARY_DIR}")

PROJECT(DSSAT-CSM Fortran )

##########################################################################################
# DSSAT_VERSION from ModuleDefs.for
##########################################################################################

file(READ "${CMAKE_SOURCE_DIR}/ModuleDefs.for" File_Content)

# Take DSSAT_VERSION from ModuleDefs.for
STRING(REGEX REPLACE ".*Major = ([0-9]+).*" "\\1" MAJOR "${File_Content}")
STRING(REGEX REPLACE ".*Minor = ([0-9]+).*" "\\1" MINOR "${File_Content}")
STRING(REGEX REPLACE ".*Model = ([0-9]+).*" "\\1" MODEL "${File_Content}")
STRING(REGEX REPLACE ".*Build = ([0-9]+).*" "\\1" BUILD "${File_Content}")
MESSAGE(STATUS "MAJOR: ${MAJOR} MINOR: ${MINOR} MODEL: ${MODEL} BUILD: ${BUILD}")

# Set the version
SET(VERSION ${MAJOR}.${MINOR}.${MODEL})

##########################################################################################

# Add our local modlues to the module path
SET(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake/Modules/")

# Organize output files.  On Windows this also keeps .dll files next
# to the .exe files that need them, making tests easy to run.
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod)

# Keep Uncomment if Fortran 90 support is required
IF(NOT CMAKE_Fortran_COMPILER_SUPPORTS_F90)
    MESSAGE(FATAL_ERROR "Fortran compiler does not support F90")
ENDIF(NOT CMAKE_Fortran_COMPILER_SUPPORTS_F90)

# This INCLUDE statement executes code that sets the compile flags for DEBUG,
# RELEASE, and TESTING.  You should  review this file and make sure the flags 
# are to your liking.
INCLUDE(${CMAKE_MODULE_PATH}/SetFortranFlags.cmake) 

# There is an error in CMAKE with this flag for pgf90.  Unset it
GET_FILENAME_COMPONENT(FCNAME ${CMAKE_Fortran_COMPILER} NAME)
IF(FCNAME STREQUAL "pgf90")
    UNSET(CMAKE_SHARED_LIBRARY_LINK_Fortran_FLAGS)
ENDIF(FCNAME STREQUAL "pgf90")

##########################################################################################
# Define the actual files and folders that make the build
##########################################################################################

# Define the executable name and OSDefinitions file
IF(WIN32)
    SET(EXECUTABLE_NAME dscsm0${MAJOR}${MINOR}.exe)
    SET(OSDefinitions "OSDefsWINDOWS.for")    
    add_definitions(-DWIN32=1})
ELSE()
    SET(EXECUTABLE_NAME dscsm0${MAJOR}${MINOR})
    SET(OSDefinitions "OSDefsLINUX.for")    
ENDIF(WIN32)

# Add the source files
ADD_EXECUTABLE(${EXECUTABLE_NAME} 
	${OSDefinitions}
	ModuleDefs.for
	ASMDM.for
	AUTHAR.for
	AUTPLT.for
	CANOPY.for
	CE_RATIO_C.for
	CENTURY.for
	CHEMICAL.for
	CO2VAL.for
	CROPGRO.for
	CSCAS_Interface.for
	CSCAS.FOR
	CSCER.for
	CSCERES_Interface.for
	CSCRP.for
	CSCRP_Interface.for
	CSDISEASE.for
	CSM.for
	CSP_CANOPY.for
	CSP_CASUPRO.for
	CSP_DEMAND.for
	CSP_GROW.for
	CSP_GROW_CANE.for
	CSP_HRes.for
	CSP_INCOMP.for
	CSP_INCOMP_OUT.for
	CSP_INPHENOL.for
	CSP_IPDMND_OUT.for
	CSP_IPPHENOL.for
	CSP_IPPLNT.for
	CSP_MOBIL.for
	CSP_NUPTAK.for
	CSP_OPGROW.for
	CSP_OPHARV.for
	CSP_PHENOL.for
	CSP_PHOTO.for
	CSP_RESPIR.for
	CSP_ROOTS.for
	CSP_SENES.for
	CSP_VEGGR.for
	CSREADS.for
	CSUTS.for
	CSVOUTS/csvlinklist.f90
	CSVOUTS/csvoutput.f90
	DATES.for
	DECRAT_C.for
	DEMAND.for
	EFLOW_C.for
	EQUIL2.for
	ERROR.for
	ESR_SoilEvap.for
	ETPHOT.for
	ETPHR.for
	FCHEM.for
	Fert_Place.for
	Flood_Chem.for
	Flood_Irrig.for
	FLOODI.for
	for_asmdm.for
	for_canopy.for
	for_ch2oref.for
	for_demand.for
	for_dormancy.for
	for_freeze.for
	for_grow.for
	for_harv.for
	for_hres_cgro.for
	for_incomp.for
	for_ipparm.for
	for_ippest.for
	for_ipphenol.for
	for_ipplnt.for
	for_ipprog.for
	for_lindm.for
	for_mobil.for
	for_nfix.for
	for_nuptak.for
	for_opgrow.for
	for_opharv.for
	for_opmob.for
	for_oppest.for
	for_opview.for
	for_pest.for
	for_pestcp.for
	for_phenol.for
	for_photo.for
	for_plantnbal.for
	for_poddet.for
	for_pods.for
	for_respir.for
	for_rootdm.for
	for_roots.for
	for_rstages.for
	for_sdcomp.for
	for_seeddm.for
	for_senmob.for
	for_vegdm.for
	for_veggr.for
	forage.for
	FREEZE.for
	FreshWt.for
	GROW.for
	HMET.for
	HRes_CGRO.for
	HResCeres.for
	IMMOBLIMIT_C.for
	INCOMP.for
	INCORPOR_C.for
	INFIL.for
	Info.for
	input_sub.for
	INSOIL.for
	INSTGE.for
	INTRO.for
	INVAR.for
	IPCHEM.for
	IPECO.for
	IPENV.for
	ipexp.for
	IPHedley_C.for
	IPHedley_inorg.for
	IPIBS.for
	IPMAN.for
	IPPARM.for
	IPPEST.for
	Ipphenol.for
	IPPLNT.for
	IPPROG.for
	IPSIM.for
	IPSLIN.for
	IPSOIL.for
	IPSOIL_Inp.for
	IPTILL.for
	IPVAR.for
	IPWTH_alt.for
	IRRIG.for
	LAND.for
	LINDM.for
	linux.f90
	LITDEC_C.for
	LMATCH.for
	MgmtOps.for
	ML_CERES.for
	ML_GROSUB.for
	ML_NFACT.for
	ml_NUPTAK.for
	ML_opharv.for
	ML_OPGROW.FOR
	ML_PHASEI.for
	ML_PHENOL.for
	ML_rootgr.for
	ML_TILLSUB.for
	MOBIL.for
	MULCHEVAP.for
	MULCHLAYER.for
	MULCHWAT.for
	MZ_CERES.for
	MZ_GROSUB.for
	MZ_IX_GROSUB.for
	MZ_IX_KNUMBER.for
	MZ_IX_LEAFAREA.for
	MZ_IX_NUPTAK.for
	MZ_IX_PHENOL.for
	MZ_IX_PHOTSYNT.for
	MZ_IX_PLANTG.for
	MZ_IX_RADABS.for
	MZ_IX_RESPIR.for
	MZ_KUPTAK.for
	MZ_NFACTO.for
	MZ_NUPTAK.for
	MZ_OPGROW.for
	MZ_OPHARV.for
	MZ_OPNIT.for
	MZ_PHENOL.for
	MZ_ROOTS.for
	NCHECK_C.for
	NCHECK_inorg.for
	NCHECK_organic.for
	NFIX.for
	NFLUX.for
	NUPTAK.for
	OM_Place.for
	OP_OBS/DS1900.f90
	OP_OBS/GETOBS.f90
	OP_OBS/INDEXX.f90
	OP_OBS/INQOBS.f90
	OP_OBS/INTGR2.f90
	OP_OBS/OBSINI.f90
	OP_OBS/OBSSYS.f90
	OP_OBS/OBSTRG.f90
	OP_OBS/OPCF.f90
	OP_OBS/OPINIT.f90
	OP_OBS/OPNF.f90
	OP_OBS/OPREAD.f90
	OP_OBS/OPSC.f90
	OP_OBS/OPSTOR.f90
	OP_OBS/OPSYS.f90
	OP_OBS/OPWRITE.f90
	OP_OBS/TTHEAD.f90
	OPETPHOT.for
	OpFlood.for
	OPFLOODN.for
	OPGEN.for
	OPGeneric.for
	Opgrow.for
	OPHARV.for
	OPHEAD.for
	OPMULCH.for
	OPPEST.for
	OpPlantP.for
	OpSoilKi.for
	OPSOILNI.for
	OpSoilOrg.for
	OpSoilPi.for
	OPSOMLIT_C.for
	OpStemp.for
	OPSTRESS.for
	OPSUM.for
	OPTEMPXY2K.for
	optempy2k.for
	OPVIEW.for
	OPWBAL.for
	OPWEATH.for
	ORYZA/G_Exper.f90
	ORYZA/G_Soil.f90
	ORYZA/GPPARGET.f90
	ORYZA/GPPARSET.f90
	ORYZA/Ncrop3.f90
	ORYZA/NightT.f90
	ORYZA/Nnostress2.f90
	ORYZA/OR_Opgrow.f90
	ORYZA/OR_Opharv.for
	ORYZA/OR_PHENOL.f90
	ORYZA/OR_ROOTG.f90
	ORYZA/ORYZA1.f90
	ORYZA/ORYZA_Interface.f90
	ORYZA/PARTITION.f90
	ORYZA/quadpack.f90
	ORYZA/SASTRO.f90
	ORYZA/SGPC1.f90
	ORYZA/SGPC2.f90
	ORYZA/SGPCDT.f90
	ORYZA/SGPL.f90
	ORYZA/SRDPRF.f90
	ORYZA/SSKYC.f90
	ORYZA/SUBCBC.f90
	ORYZA/SUBCD2.f90
	ORYZA/SUBDD.f90
	ORYZA/SUBGRN.f90
	ORYZA/SUBLAI3.f90
	ORYZA/SVPS1.f90
	ORYZA/VARIABLE.f90
	ORYZA/wnostress.f90
	ORYZA/WStress2.f90
	OXLAYER.for
	P_CASUPRO.for
	P_CERES.for
	P_CGRO.for
	P_IPPLNT.for
	p_plant.for
	P_Uptake.for
	Paddy_Mgmt.for
	PARTIT_C.for
	PATH.for
	PEST.for
	PESTCP.for
	PET.for
	PHENOL.for
	PHOTO.for
	plant.for
	PlantNBal.for
	PODDET.for
	PODS.for
	PPlantSubs.for
	PT_GROSUB.for
	PT_NFACTO.for
	PT_NUPTAK.for
	PT_OPGROW.for
	PT_OPHARV.for
	PT_PHASEI.for
	PT_PHENOL.for
	PT_ROOTGR.for
	PT_SUBSTOR.for
	PT_THTIME.for
	READS.for
	RESPIR.for
	RETC_VG.for
	RI_Calcshk.for
	RI_GNURSE.for
	RI_Grosub.for
	RI_Ipcrop.for
	RI_KUPTAK.for
	RI_Nfacto.for
	RI_Nuptak.for
	RI_Opgrow.for
	RI_Opharv.for
	RI_Phenol.for
	RI_Rootgr.for
	RI_Tillsub.for
	RI_Transpl_g.for
	RI_Transpl_p.for
	RICE.for
	RNOFF.for
	ROOTDM.for
	ROOTS.for
	RootSoilVol.for
	ROOTWU.for
	RPLACE_C.for
	RStages.for
	RunList.for
	SAL_Stemp.for
	SATFLO.for
	SC_CanesimCanopy.for
	SC_Canop3.for
	SC_CNG_mods.for
	SC_CNGRO.for
	SC_COEFFS.for
	SC_OPHARV.for
	SC_OUTPUT.for
	SC_PARTIT.for
	SC_PHENOL.for
	SC_PHOTOS.for
	SC_Poplt3.for
	SC_ROOTG.for
	SDCOMP.for
	SECLI.for
	SECROP.for
	SEEDDM.for
	SEFERT.for
	SEFLD.for
	SEFREQ.for
	SEHARV.for
	SEINIT.for
	SEIRR.for
	SENES.for
	SENESADD_C.for
	SENS.for
	SEPEST.for
	SEPLT.for
	SERES.for
	SESIM.for
	SESOIL.for
	SETIME.for
	SEVAR.for
	SEWTH.for
	SG_CERES.for
	SG_GROSUB.for
	SG_NFACT.for
	sg_NUPTAK.for
	SG_OPHARV.for
	SG_PHASEI.for
	SG_PHENOL.for
	SG_ROOTGR.for
	SLigCeres.for
	SOIL.for
	SoilCBal_C.for
	SoilCNPinit_C.for
	SOILDYN.for
	SOILEV.for
	SoilK_init.for
	SoilKi.for
	SoilMixing.for
	SoilNBalSum.for
	SOILNI.for
	SoilNi_init.for
	SoilNiBal.for
	SoilNoBal.for
	SoilNoBal_C.for
	SoilNoPoBal.for
	SoilOrg.for
	SoilOrg_init.for
	SoilPBalSum.for
	SoilPi.for
	SoilPi_init.for
	soilpibal.for
	SoilPoBal.for
	SoilPoBal_C.for
	SOLAR.for
	SOMDEC_C.for
	SOMFIX_C.for
	SOMINIT_c.for
	SOMLITPRINT_C.for
	SPAM.for
	SPSUBS.for
	STEMP.for
	STEMP_EPIC.for
	SW_FreshWt.for
	SW_GROSUB.for
	TextureClass.for
	TILEDRAIN.for
	Tillage.for
	TillEvent.for
	TR_Calcshk.for
	TR_Grosub.for
	TR_Ipcrop.for
	TR_Nfacto.for
	TR_Nuptak.for
	TR_OPGROW.for
	TR_Opharv.for
	TR_Phenol.for
	TR_Rootgr.for
	TR_SUBSTOR.for
	TR_Tillsub.for
	TR_Transpl_g.for
	TR_Transpl_p.for
	TRANS.for
	TSOMLIT_C.for
	ttutil/addinf.for
	ttutil/addint.for
	ttutil/addrea.for
	ttutil/addref.for
	ttutil/addstf.for
	ttutil/addstr.for
	ttutil/ambusy.for
	ttutil/copfl2.for
	ttutil/decchk.for
	ttutil/decdou.for
	ttutil/decint.for
	ttutil/decrea.for
	ttutil/decrec.for
	ttutil/dectim.for
	ttutil/delfil.for
	ttutil/dtardp.for
	ttutil/dtdpar.for
	ttutil/dtdpst.for
	ttutil/dtfsecmp.for
	ttutil/dtfsedp.for
	ttutil/dtleap.for
	ttutil/dtnow.f90
	ttutil/dtsys.for
	ttutil/entcha.for
	ttutil/entdch.for
	ttutil/entddo.for
	ttutil/entdin.for
	ttutil/entdou.for
	ttutil/entdre.for
	ttutil/entdti.for
	ttutil/entdyn.for
	ttutil/enthlp.for
	ttutil/entint.for
	ttutil/entrea.for
	ttutil/enttim.for
	ttutil/entyno.for
	ttutil/extens.for
	ttutil/fatalerr.f90
	ttutil/flexist.for
	ttutil/flname.for
	ttutil/fopengstandard.f90
	ttutil/fopens.for
	ttutil/getrec.for
	ttutil/getun.for
	ttutil/getun2.for
	ttutil/ifindc.f90
	ttutil/ifindi.for
	ttutil/INSW.for
	ttutil/INTGRL.for
	ttutil/istart.for
	ttutil/iunifl.for
	ttutil/LIMIT.for
	ttutil/lint.for
	ttutil/LINT2.for
	ttutil/lowerc.for
	ttutil/messini.f90
	ttutil/messinq.f90
	ttutil/messwrt.for
	ttutil/movavr.for
	ttutil/notnul.for
	ttutil/openlogf.f90
	ttutil/outar2.for
	ttutil/outcom.for
	ttutil/outdat.f90
	ttutil/outplt.for
	ttutil/outsel.f90
	ttutil/parsword.for
	ttutil/rchrsrc.for
	ttutil/rdacha.for
	ttutil/rdador.for
	ttutil/rdadou.for
	ttutil/rdainr.for
	ttutil/rdaint.for
	ttutil/rdalog.for
	ttutil/rdarea.for
	ttutil/rdarer.for
	ttutil/rdatim.for
	ttutil/rddata.for
	ttutil/rddtmp.for
	ttutil/rderr.for
	ttutil/rderri.for
	ttutil/rdfcha.for
	ttutil/rdfdor.for
	ttutil/rdfdou.for
	ttutil/rdfinr.for
	ttutil/rdfint.for
	ttutil/rdflog.for
	ttutil/rdfrea.for
	ttutil/rdfrer.for
	ttutil/rdfrom.for
	ttutil/rdftim.for
	ttutil/rdinar.for
	ttutil/rdindt.for
	ttutil/rdindx.for
	ttutil/rdinit.for
	ttutil/rdinlv.for
	ttutil/rdinne.for
	ttutil/rdinqr.for
	ttutil/rdinqr2.for
	ttutil/rdinqr3.for
	ttutil/rdlex.for
	ttutil/rdmcha.for
	ttutil/rdmdef.for
	ttutil/rdmdou.for
	ttutil/rdmint.for
	ttutil/rdmlog.for
	ttutil/rdmrea.for
	ttutil/rdmtim.for
	ttutil/rdpars.for
	ttutil/rdscha.for
	ttutil/rdsctb.for
	ttutil/rdsdor.for
	ttutil/rdsdou.for
	ttutil/rdsets.for
	ttutil/rdsinr.for
	ttutil/rdsint.for
	ttutil/rdslog.for
	ttutil/rdsrea.for
	ttutil/rdsrer.for
	ttutil/rdstim.for
	ttutil/rdtmp1.for
	ttutil/rdtmp2.for
	ttutil/reaand.for
	ttutil/reanor.for
	ttutil/recread.f90
	ttutil/recreadi.f90
	ttutil/recreadt.f90
	ttutil/remove.for
	ttutil/sfindg.for
	ttutil/sortch.for
	ttutil/sortin.for
	ttutil/str_copy.for
	ttutil/swpi4.for
	ttutil/TIMER2.for
	ttutil/ttutil.f90
	ttutil/ttutilprefs.f90
	ttutil/ttuver.for
	ttutil/unifl.for
	ttutil/upperc.f90
	ttutil/usedun.for
	ttutil/ver4_23.for
	ttutil/warning_OR.for
	ttutil/words.for
	ttutil/wracha.for
	ttutil/wradou.for
	ttutil/wraint.for
	ttutil/wralog.for
	ttutil/wrarea.for
	ttutil/wratim.for
	ttutil/wrinit.for
	ttutil/wrscha.for
	ttutil/wrsdou.for
	ttutil/wrsint.for
	ttutil/wrslog.for
	ttutil/wrsrea.for
	ttutil/wrstim.for
	UTILS.for
	VEGDM.for
	VEGGR.for
	Warning.for
	WATBAL.for
	WBAL.for
	WBSUBS.for
	weathr.for
	WEATHR_Inp.for
	WGEN.for
	WH_APSIM.for
	WH_COLD.for
	WH_GROSUB.for
	WH_KUPTAK.for
	WH_module.f90
	WH_NFACTO.for
	WH_NUPTAK.for
	WH_OPGROW.for
	WH_OPHARV.for
	WH_OPNIT.for
	WH_PHENOL.for
	WH_ROOTS.for
	WH_SW_SUBS.for
	WH_temp.for
	windows.f90
	WTHMOD.for
	WTHSET.for
	YCA/CSYCA.f90
	YCA/CSYCA_Interface.for
	YCA/YCA_Albedo_Check_m.f90
	YCA/YCA_Environment.f90
	YCA/YCA_First_Trans_m.f90
	YCA/YCA_Formats_m.f90
	YCA/YCA_Growth.f90
	YCA/YCA_Growth_Distribute.f90
	YCA/YCA_Growth_Evapo.f90
	YCA/YCA_Growth_Init.f90
	YCA/YCA_Growth_NUptake.f90
	YCA/YCA_Growth_Part.f90
	YCA/YCA_Growth_Photo.f90
	YCA/YCA_Growth_Rates.f90
	YCA/YCA_Growth_Senesce.f90
	YCA/YCA_Integ_AgesWts.f90
	YCA/YCA_Integ_EndCrop.f90
	YCA/YCA_Integ_HstFail.f90
	YCA/YCA_Integ_LA.f90
	YCA/YCA_Integ_N.f90
	YCA/YCA_Integ_Nconc.f90
	YCA/YCA_Integ_SeasEnd.f90
	YCA/YCA_Integ_Stages.f90
	YCA/YCA_Integ_WthrSum.f90
	YCA/YCA_Integrate.f90
	YCA/YCA_Node.f90
	YCA/YCA_Out_CrpSim.f90
	YCA/YCA_Out_Error.f90
	YCA/YCA_Out_Eval.f90
	YCA/YCA_Out_LfTier.f90
	YCA/YCA_Out_ModFail.f90
	YCA/YCA_Out_PlantSum.f90
	YCA/YCA_Out_PlGrow.f90
	YCA/YCA_Out_ReInit.f90
	YCA/YCA_Out_Sens.f90
	YCA/YCA_Out_StoreVars.f90
	YCA/YCA_Out_Work.f90
	YCA/YCA_Out_WrkPhenRes.f90
	YCA/YCA_Output.f90
	YCA/YCA_Photosynthesis.f90
	YCA/YCA_PrePlant.f90
	YCA/YCA_RunInit.f90
	YCA/YCA_SeasInit.f90
	YCA/YCA_SeasInit_Final.f90
	YCA/YCA_SeasInit_PlHarvDat.f90
	YCA/YCA_SeasInit_ReadGeno.f90
	YCA/YCA_SeasInit_ReadXfile.f90
	YCA/YCA_SeasInit_SetStage.f90
	YCA/YCA_SeasInit_VarInit.f90
	YCA/YCA_VPDEffect.f90
)
#####################################
# Tell how to install this executable
#####################################

IF(WIN32)
    SET(CMAKE_INSTALL_PREFIX "C:\\DSSAT46")
ELSE()
    SET(CMAKE_INSTALL_PREFIX ~/DSSAT46)
ENDIF(WIN32)
INSTALL(TARGETS ${EXECUTABLE_NAME} RUNTIME DESTINATION ${CMAKE_INSTALL_PREFIX})

# Add a distclean target to the Makefile
ADD_CUSTOM_TARGET(distclean 
    COMMAND ${CMAKE_COMMAND} -P ${CMAKE_SOURCE_DIR}/distclean.cmake
)

message( "-- Flags" )
message( "   FFLAGS       ${CMAKE_Fortran_FLAGS}" )
message( "   RELEASE      ${CMAKE_Fortran_FLAGS_RELEASE}" )
message( "   DEBUG        ${CMAKE_Fortran_FLAGS_DEBUG}" )
message( "-- Build Info" )
message( "   BUILD TYPE   ${CMAKE_BUILD_TYPE}" )
message( "   VERSION      ${MAJOR}.${MINOR}.${MODEL}.${BUILD}" )
message( "   I. PREFIX    ${CMAKE_INSTALL_PREFIX}" )
message( "   Executable   ${EXECUTABLE_NAME}" )
