# CMake project file for DSSAT-CSM

##################################################
# Define the project and the dependencies that it has
##################################################

CMAKE_MINIMUM_REQUIRED(VERSION 2.8.5)
PROJECT(DSSAT-CSM Fortran )

# Set the version
#SET(VERSION X.X.X)

# Add our local modlues to the module path
SET(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake/Modules/")

# Keep Uncomment if Fortran 90 support is required
IF(NOT CMAKE_Fortran_COMPILER_SUPPORTS_F90)
    MESSAGE(FATAL_ERROR "Fortran compiler does not support F90")
ENDIF(NOT CMAKE_Fortran_COMPILER_SUPPORTS_F90)

# Set some options the user may choose
# Uncomment the below if you want the user to choose a parallelization library
#OPTION(USE_MPI "Use the MPI library for parallelization" OFF)
#OPTION(USE_OPENMP "Use OpenMP for parallelization" OFF)

# This INCLUDE statement executes code that sets the compile flags for DEBUG,
# RELEASE, and TESTING.  You should  review this file and make sure the flags 
# are to your liking.
INCLUDE(${CMAKE_MODULE_PATH}/SetFortranFlags.cmake) 
# Locate and set parallelization libraries.  There are some CMake peculiarities
# taken care of here, such as the fact that the FindOpenMP routine doesn't know
# about Fortran.
#INCLUDE(${CMAKE_MODULE_PATH}/SetParallelizationLibrary.cmake)
# Setup the LAPACK libraries.  This also takes care of peculiarities, such as
# the fact the searching for MKL requires a C compiler, and that the results
# are not stored in the cache. 
#INCLUDE(${CMAKE_MODULE_PATH}/SetUpLAPACK.cmake)

# There is an error in CMAKE with this flag for pgf90.  Unset it
GET_FILENAME_COMPONENT(FCNAME ${CMAKE_Fortran_COMPILER} NAME)
IF(FCNAME STREQUAL "pgf90")
    UNSET(CMAKE_SHARED_LIBRARY_LINK_Fortran_FLAGS)
ENDIF(FCNAME STREQUAL "pgf90")

############################################################
# Define the actual files and folders that make up the build
############################################################

# Define the executable name
SET(CSMEXE dscsm046.exe)

# Define some directories
SET(SRCCSM ${CMAKE_SOURCE_DIR})
SET(LIB ${CMAKE_SOURCE_DIR}/lib)
SET(BIN ${CMAKE_SOURCE_DIR}/bin)

# Have the .mod files placed in the lib folder
SET(CMAKE_Fortran_MODULE_DIRECTORY ${LIB})

# The source for the dssat-csm binary and have it placed in the bin folder
#ADD_SUBDIRECTORY(${CMAKE_SOURCE_DIR} ${BIN})

########################################
# Set up how to compile the source files
########################################

# Add the source files
SET(CSM_FILES 
	${SRCCSM}/VEGGR.for
	${SRCCSM}/PET.for
	${SRCCSM}/SC_OUTPUT.for
	${SRCCSM}/CSP_CANOPY.for
	${SRCCSM}/NCHECK_organic.for
	${SRCCSM}/CSCERES_Interface.for
	${SRCCSM}/OpPlantP.for
	${SRCCSM}/CSP_GROW_CANE.for
	${SRCCSM}/IPMAN.for
	${SRCCSM}/MZ_IX_LEAFAREA.for
	${SRCCSM}/NCHECK_C.for
	${SRCCSM}/TR_Transpl_g.for
	${SRCCSM}/WH_APSIM.FOR
	${SRCCSM}/CSUTS.for
	${SRCCSM}/ML_NFACT.for
	${SRCCSM}/RI_Grosub.for
	${SRCCSM}/CSP_PHENOL.for
	${SRCCSM}/Ipphenol.for
	${SRCCSM}/CRSIMDEF.for
	${SRCCSM}/RI_Nfacto.for
	${SRCCSM}/LAND.for
	${SRCCSM}/MZ_OPGROW.for
	${SRCCSM}/MgmtOps.for
	${SRCCSM}/IPECO.for
	${SRCCSM}/RI_KUPTAK.for
	${SRCCSM}/IPVAR.for
	${SRCCSM}/ttutil/rdslog.for
	${SRCCSM}/ttutil/rdacha.for
	${SRCCSM}/ttutil/rdstim.for
	${SRCCSM}/ttutil/addstf.for
	${SRCCSM}/ttutil/TIMER2.for
	${SRCCSM}/ttutil/ttutil.f90
	${SRCCSM}/ttutil/rdfdor.for
	${SRCCSM}/ttutil/rdftim.for
	${SRCCSM}/ttutil/addref.for
	${SRCCSM}/ttutil/addrea.for
	${SRCCSM}/ttutil/rdarea.for
	${SRCCSM}/ttutil/fatalerr.f90
	${SRCCSM}/ttutil/wrstim.for
	${SRCCSM}/ttutil/rddata.for
	${SRCCSM}/ttutil/wrslog.for
	${SRCCSM}/ttutil/rdsint.for
	${SRCCSM}/ttutil/rderr.for
	${SRCCSM}/ttutil/rdfrom.for
	${SRCCSM}/ttutil/wrarea.for
	${SRCCSM}/ttutil/reaand.for
	${SRCCSM}/ttutil/entdou.for
	${SRCCSM}/ttutil/entdti.for
	${SRCCSM}/ttutil/ifindc.f90
	${SRCCSM}/ttutil/wrscha.for
	${SRCCSM}/ttutil/decchk.for
	${SRCCSM}/ttutil/INSW.for
	${SRCCSM}/ttutil/getun.for
	${SRCCSM}/ttutil/dectim.for
	${SRCCSM}/ttutil/dtdpar.for
	${SRCCSM}/ttutil/entdyn.for
	${SRCCSM}/ttutil/rdaint.for
	${SRCCSM}/ttutil/openlogf.f90
	${SRCCSM}/ttutil/rdsdou.for
	${SRCCSM}/ttutil/rdinqr2.for
	${SRCCSM}/ttutil/messini.f90
	${SRCCSM}/ttutil/parsword.for
	${SRCCSM}/ttutil/dtardp.for
	${SRCCSM}/ttutil/decint.for
	${SRCCSM}/ttutil/rdsets.for
	${SRCCSM}/ttutil/rdtmp1.for
	${SRCCSM}/ttutil/decrea.for
	${SRCCSM}/ttutil/wradou.for
	${SRCCSM}/ttutil/rdalog.for
	${SRCCSM}/ttutil/dtdpst.for
	${SRCCSM}/ttutil/notnul.for
	${SRCCSM}/ttutil/rdinit.for
	${SRCCSM}/ttutil/wralog.for
	${SRCCSM}/ttutil/rdmtim.for
	${SRCCSM}/ttutil/rdinne.for
	${SRCCSM}/ttutil/ambusy.for
	${SRCCSM}/ttutil/rdsrer.for
	${SRCCSM}/ttutil/getrec.for
	${SRCCSM}/ttutil/decdou.for
	${SRCCSM}/ttutil/swpi4.for
	${SRCCSM}/ttutil/unifl.for
	${SRCCSM}/ttutil/rdinqr3.for
	${SRCCSM}/ttutil/rdainr.for
	${SRCCSM}/ttutil/wraint.for
	${SRCCSM}/ttutil/recread.f90
	${SRCCSM}/ttutil/rdmcha.for
	${SRCCSM}/ttutil/rdinqr.for
	${SRCCSM}/ttutil/warning_OR.for
	${SRCCSM}/ttutil/addstr.for
	${SRCCSM}/ttutil/rdfrea.for
	${SRCCSM}/ttutil/outplt.for
	${SRCCSM}/ttutil/entddo.for
	${SRCCSM}/ttutil/delfil.for
	${SRCCSM}/ttutil/movavr.for
	${SRCCSM}/ttutil/outdat.f90
	${SRCCSM}/ttutil/rdscha.for
	${SRCCSM}/ttutil/rdmdou.for
	${SRCCSM}/ttutil/rdmrea.for
	${SRCCSM}/ttutil/wrsdou.for
	${SRCCSM}/ttutil/dtfsedp.for
	${SRCCSM}/ttutil/sfindg.for
	${SRCCSM}/ttutil/rdsrea.for
	${SRCCSM}/ttutil/wratim.for
	${SRCCSM}/ttutil/dtleap.for
	${SRCCSM}/ttutil/LIMIT.for
	${SRCCSM}/ttutil/rdadou.for
	${SRCCSM}/ttutil/remove.for
	${SRCCSM}/ttutil/rdmdef.for
	${SRCCSM}/ttutil/rdfcha.for
	${SRCCSM}/ttutil/upperc.f90
	${SRCCSM}/ttutil/entyno.for
	${SRCCSM}/ttutil/rdtmp2.for
	${SRCCSM}/ttutil/sortin.for
	${SRCCSM}/ttutil/rdflog.for
	${SRCCSM}/ttutil/rderri.for
	${SRCCSM}/ttutil/rddtmp.for
	${SRCCSM}/ttutil/iunifl.for
	${SRCCSM}/ttutil/outsel.f90
	${SRCCSM}/ttutil/entint.for
	${SRCCSM}/ttutil/addinf.for
	${SRCCSM}/ttutil/dtnow.f90
	${SRCCSM}/ttutil/rchrsrc.for
	${SRCCSM}/ttutil/outcom.for
	${SRCCSM}/ttutil/entrea.for
	${SRCCSM}/ttutil/rdatim.for
	${SRCCSM}/ttutil/lint.for
	${SRCCSM}/ttutil/messinq.f90
	${SRCCSM}/ttutil/decrec.for
	${SRCCSM}/ttutil/usedun.for
	${SRCCSM}/ttutil/dtfsecmp.for
	${SRCCSM}/ttutil/getun2.for
	${SRCCSM}/ttutil/rdfrer.for
	${SRCCSM}/ttutil/messwrt.for
	${SRCCSM}/ttutil/sortch.for
	${SRCCSM}/ttutil/ttuver.for
	${SRCCSM}/ttutil/entdin.for
	${SRCCSM}/ttutil/istart.for
	${SRCCSM}/ttutil/rdmlog.for
	${SRCCSM}/ttutil/fopens.for
	${SRCCSM}/ttutil/rdlex.for
	${SRCCSM}/ttutil/wracha.for
	${SRCCSM}/ttutil/str_copy.for
	${SRCCSM}/ttutil/copfl2.for
	${SRCCSM}/ttutil/lowerc.for
	${SRCCSM}/ttutil/extens.for
	${SRCCSM}/ttutil/rdinlv.for
	${SRCCSM}/ttutil/rdfint.for
	${SRCCSM}/ttutil/ver4_23.for
	${SRCCSM}/ttutil/rdsctb.for
	${SRCCSM}/ttutil/dtsys.for
	${SRCCSM}/ttutil/rdindt.for
	${SRCCSM}/ttutil/LINT2.for
	${SRCCSM}/ttutil/rdindx.for
	${SRCCSM}/ttutil/enthlp.for
	${SRCCSM}/ttutil/enttim.for
	${SRCCSM}/ttutil/reanor.for
	${SRCCSM}/ttutil/rdinar.for
	${SRCCSM}/ttutil/recreadt.f90
	${SRCCSM}/ttutil/recreadi.f90
	${SRCCSM}/ttutil/entdch.for
	${SRCCSM}/ttutil/rdpars.for
	${SRCCSM}/ttutil/words.for
	${SRCCSM}/ttutil/rdador.for
	${SRCCSM}/ttutil/fopengstandard.f90
	${SRCCSM}/ttutil/rdsinr.for
	${SRCCSM}/ttutil/addint.for
	${SRCCSM}/ttutil/rdfinr.for
	${SRCCSM}/ttutil/entcha.for
	${SRCCSM}/ttutil/outar2.for
	${SRCCSM}/ttutil/ifindi.for
	${SRCCSM}/ttutil/entdre.for
	${SRCCSM}/ttutil/flname.for
	${SRCCSM}/ttutil/INTGRL.for
	${SRCCSM}/ttutil/rdarer.for
	${SRCCSM}/ttutil/wrsrea.for
	${SRCCSM}/ttutil/wrinit.for
	${SRCCSM}/ttutil/rdfdou.for
	${SRCCSM}/ttutil/rdsdor.for
	${SRCCSM}/ttutil/flexist.for
	${SRCCSM}/ttutil/ttutilprefs.f90
	${SRCCSM}/ttutil/rdmint.for
	${SRCCSM}/ttutil/wrsint.for
	${SRCCSM}/CSP_RESPIR.for
	${SRCCSM}/OPSTRESS.for
	${SRCCSM}/SEFERT.for
	${SRCCSM}/Opgrow.for
	${SRCCSM}/CROPGRO.for
	${SRCCSM}/PT_OPGROW.for
	${SRCCSM}/TR_Tillsub.for
	${SRCCSM}/PARTIT_C.for
	${SRCCSM}/TR_Nuptak.for
	${SRCCSM}/SoilPi.for
	${SRCCSM}/FCHEM.for
	${SRCCSM}/CSP_GROW.for
	${SRCCSM}/Flood_Irrig.for
	${SRCCSM}/SG_PHENOL.for
	${SRCCSM}/P_IPPLNT.for
	${SRCCSM}/RETC_VG.for
	${SRCCSM}/MULCHLAYER.for
	${SRCCSM}/SOMINIT_c.for
	${SRCCSM}/TR_Phenol.for
	${SRCCSM}/WH_temp.for
	${SRCCSM}/SoilNoBal_C.for
	${SRCCSM}/IPHedley_inorg.for
	${SRCCSM}/TillEvent.for
	${SRCCSM}/RI_GNURSE.for
	${SRCCSM}/optempy2k.for
	${SRCCSM}/SC_Canop3.for
	${SRCCSM}/MULCHEVAP.for
	${SRCCSM}/ML_PHENOL.for
	${SRCCSM}/SECROP.for
	${SRCCSM}/WTHMOD.for
	${SRCCSM}/FREEZE.for
	${SRCCSM}/SoilMixing.for
	${SRCCSM}/SATFLO.for
	${SRCCSM}/RI_Transpl_p.for
	${SRCCSM}/OpSoilPi.for
	${SRCCSM}/WH_OPNIT.FOR
	${SRCCSM}/HRes_CGRO.for
	${SRCCSM}/WBSUBS.for
	${SRCCSM}/CSP_CASUPRO.for
	${SRCCSM}/SoilNoBal.for
	${SRCCSM}/SEIRR.for
	${SRCCSM}/SEWTH.for
	${SRCCSM}/SALUS/SALUS_Roots.f90
	${SRCCSM}/SALUS/SALUS_Opgrow.f90
	${SRCCSM}/SALUS/SALUS_Opharv.f90
	${SRCCSM}/SALUS/SALUS.f90
	${SRCCSM}/SALUS/SALUS_NPuptake.f90
	${SRCCSM}/SALUS/SALUS_Subs.f90
	${SRCCSM}/WH_ROOTS.FOR
	${SRCCSM}/SEHARV.for
	${SRCCSM}/SG_CERES.for
	${SRCCSM}/RI_Opharv.for
	${SRCCSM}/IPWTH_alt.for
	${SRCCSM}/OpFlood.for
	${SRCCSM}/NUPTAK.for
	${SRCCSM}/WH_module.f90
	${SRCCSM}/SoilNi_init.for
	${SRCCSM}/SC_PARTIT.for
	${SRCCSM}/MZ_PHENOL.for
	${SRCCSM}/GROW.for
	${SRCCSM}/OpSoilKi.for
	${SRCCSM}/Warning.for
	${SRCCSM}/RI_Tillsub.for
	${SRCCSM}/WTHSET.for
	${SRCCSM}/SoilKi.for
	${SRCCSM}/plant.for
	${SRCCSM}/TSOMLIT_C.for
	${SRCCSM}/INCORPOR_C.for
	${SRCCSM}/SETIME.for
	${SRCCSM}/SOILEV.for
	${SRCCSM}/Tillage.for
	${SRCCSM}/OPWBAL.for
	${SRCCSM}/RunList.for
	${SRCCSM}/SEFREQ.for
	${SRCCSM}/Flood_Chem.for
	${SRCCSM}/ML_OPGROW.FOR
	${SRCCSM}/SEINIT.for
	${SRCCSM}/PEST.for
	${SRCCSM}/CSP_MOBIL.for
	${SRCCSM}/OPHEAD.for
	${SRCCSM}/CSP_ROOTS.for
	${SRCCSM}/SEFLD.for
	${SRCCSM}/SDCOMP.for
	${SRCCSM}/NCHECK_inorg.for
	${SRCCSM}/RStages.for
	${SRCCSM}/ESR_SoilEvap.for
	${SRCCSM}/SW_FreshWt.for
	${SRCCSM}/RI_Ipcrop.for
	${SRCCSM}/sg_NUPTAK.for
	${SRCCSM}/SG_ROOTGR.for
	${SRCCSM}/SoilK_init.for
	${SRCCSM}/RI_Transpl_g.for
	${SRCCSM}/AUTPLT.for
	${SRCCSM}/CSP_VEGGR.for
	${SRCCSM}/SEPLT.for
	${SRCCSM}/SG_OPHARV.for
	${SRCCSM}/IPCHEM.for
	${SRCCSM}/MZ_IX_RADABS.for
	${SRCCSM}/OPSOMLIT_C.for
	${SRCCSM}/PHOTO.for
	${SRCCSM}/CSDISEASE.for
	${SRCCSM}/PT_ROOTGR.for
	${SRCCSM}/CSP_PHOTO.for
	${SRCCSM}/CSP_DEMAND.for
	${SRCCSM}/INVAR.for
	${SRCCSM}/EQUIL2.for
	${SRCCSM}/MZ_IX_PHENOL.for
	${SRCCSM}/IPHedley_C.for
	${SRCCSM}/input_sub.for
	${SRCCSM}/STEMP_EPIC.for
	${SRCCSM}/MZ_IX_RESPIR.for
	${SRCCSM}/DEMAND.for
	${SRCCSM}/SG_PHASEI.for
	${SRCCSM}/WBAL.for
	${SRCCSM}/MZ_IX_PHOTSYNT.for
	${SRCCSM}/CSP_INCOMP.for
	${SRCCSM}/SC_CanesimCanopy.for
	${SRCCSM}/TRANS.for
	${SRCCSM}/INTRO.for
	${SRCCSM}/ML_opharv.for
	${SRCCSM}/TR_SUBSTOR.for
	${SRCCSM}/SoilNiBal.for
	${SRCCSM}/SoilPBalSum.for
	${SRCCSM}/OPETPHOT.for
	${SRCCSM}/SC_COEFFS.for
	${SRCCSM}/OPPEST.for
	${SRCCSM}/IPENV.for
	${SRCCSM}/PT_GROSUB.for
	${SRCCSM}/SC_ROOTG.for
	${SRCCSM}/WGEN.for
	${SRCCSM}/SoilPi_init.for
	${SRCCSM}/Info.for
	${SRCCSM}/EFLOW_C.for
	${SRCCSM}/RICE.for
	${SRCCSM}/MZ_OPNIT.for
	${SRCCSM}/ipexp.for
	${SRCCSM}/HResCeres.for
	${SRCCSM}/PT_PHASEI.for
	${SRCCSM}/OP_OBS/OPNF.f90
	${SRCCSM}/OP_OBS/OBSSYS.f90
	${SRCCSM}/OP_OBS/OPSC.f90
	${SRCCSM}/OP_OBS/INTGR2.f90
	${SRCCSM}/OP_OBS/TTHEAD.f90
	${SRCCSM}/OP_OBS/DS1900.f90
	${SRCCSM}/OP_OBS/OPWRITE.f90
	${SRCCSM}/OP_OBS/OPSYS.f90
	${SRCCSM}/OP_OBS/OPREAD.f90
	${SRCCSM}/OP_OBS/OPINIT.f90
	${SRCCSM}/OP_OBS/OPCF.f90
	${SRCCSM}/OP_OBS/INQOBS.f90
	${SRCCSM}/OP_OBS/OBSTRG.f90
	${SRCCSM}/OP_OBS/OPSTOR.f90
	${SRCCSM}/OP_OBS/GETOBS.f90
	${SRCCSM}/OP_OBS/INDEXX.f90
	${SRCCSM}/OP_OBS/OBSINI.f90
	${SRCCSM}/ROOTDM.for
	${SRCCSM}/WH_PHENOL.for
	${SRCCSM}/WH_GROSUB.for
	${SRCCSM}/ETPHR.for
	${SRCCSM}/IPSOIL_Inp.for
	${SRCCSM}/RI_Opgrow.for
	${SRCCSM}/SC_Poplt3.for
	${SRCCSM}/ML_GROSUB.for
	${SRCCSM}/PT_NUPTAK.for
	${SRCCSM}/SESIM.for
	${SRCCSM}/CO2VAL.for
	${SRCCSM}/IPPROG.for
	${SRCCSM}/OPHARV.for
	${SRCCSM}/MZ_IX_PLANTG.for
	${SRCCSM}/IPSIM.for
	${SRCCSM}/SERES.for
	${SRCCSM}/P_CASUPRO.for
	${SRCCSM}/SW_GROSUB.for
	${SRCCSM}/SEPEST.for
	${SRCCSM}/SPAM.for
	${SRCCSM}/MZ_GROSUB.for
	${SRCCSM}/SC_PHOTOS.for
	${SRCCSM}/IPPLNT.for
	${SRCCSM}/RPLACE_C.for
	${SRCCSM}/SOMLITPRINT_C.for
	${SRCCSM}/MZ_IX_KNUMBER.for
	${SRCCSM}/OPTEMPXY2K.for
	${SRCCSM}/SG_GROSUB.for
	${SRCCSM}/SoilPoBal.for
	${SRCCSM}/PODDET.for
	${SRCCSM}/SAL_Stemp.for
	${SRCCSM}/PT_PHENOL.for
	${SRCCSM}/SEVAR.for
	${SRCCSM}/SENS.for
	${SRCCSM}/ERROR.for
	${SRCCSM}/READS.for
	${SRCCSM}/STEMP.for
	${SRCCSM}/CSCRP.for
	${SRCCSM}/CSCER.for
	${SRCCSM}/MZ_KUPTAK.for
	${SRCCSM}/OPGEN.for
	${SRCCSM}/RNOFF.for
	${SRCCSM}/INCOMP.for
	${SRCCSM}/WH_SW_SUBS.for
	${SRCCSM}/TR_Calcshk.for
	${SRCCSM}/PT_NFACTO.for
	${SRCCSM}/OPFLOODN.for
	${SRCCSM}/SOMFIX_C.for
	${SRCCSM}/ASMDM.for
	${SRCCSM}/RI_Nuptak.for
	${SRCCSM}/NFIX.for
	${SRCCSM}/SoilOrg.for
	${SRCCSM}/OM_Place.for
	${SRCCSM}/SPSUBS.for
	${SRCCSM}/WH_COLD.for
	${SRCCSM}/PODS.for
	${SRCCSM}/CSCRP_Interface.for
	${SRCCSM}/OPGeneric.for
	${SRCCSM}/SG_NFACT.for
	${SRCCSM}/ModuleDefs.for
	${SRCCSM}/CENTURY.for
	${SRCCSM}/AUTHAR.for
	${SRCCSM}/WH_OPHARV.FOR
	${SRCCSM}/ML_PHASEI.for
	${SRCCSM}/P_Uptake.for
	${SRCCSM}/ML_TILLSUB.for
	${SRCCSM}/ORYZA/wnostress.f90
	${SRCCSM}/ORYZA/SRDPRF.f90
	${SRCCSM}/ORYZA/Ncrop3.f90
	${SRCCSM}/ORYZA/OR_Opharv.for
	${SRCCSM}/ORYZA/OR_Opgrow.f90
	${SRCCSM}/ORYZA/SASTRO.f90
	${SRCCSM}/ORYZA/GPPARSET.f90
	${SRCCSM}/ORYZA/VARIABLE.f90
	${SRCCSM}/ORYZA/SVPS1.f90
	${SRCCSM}/ORYZA/G_Exper.f90
	${SRCCSM}/ORYZA/SGPL.f90
	${SRCCSM}/ORYZA/SGPC1.f90
	${SRCCSM}/ORYZA/ORYZA1.f90
	${SRCCSM}/ORYZA/NightT.f90
	${SRCCSM}/ORYZA/quadpack.f90
	${SRCCSM}/ORYZA/WStress2.f90
	${SRCCSM}/ORYZA/SGPCDT.f90
	${SRCCSM}/ORYZA/SUBCBC.f90
	${SRCCSM}/ORYZA/GPPARGET.f90
	${SRCCSM}/ORYZA/SGPC2.f90
	${SRCCSM}/ORYZA/Nnostress2.f90
	${SRCCSM}/ORYZA/PARTITION.f90
	${SRCCSM}/ORYZA/SUBGRN.f90
	${SRCCSM}/ORYZA/SUBCD2.f90
	${SRCCSM}/ORYZA/OR_PHENOL.f90
	${SRCCSM}/ORYZA/G_Soil.f90
	${SRCCSM}/ORYZA/SUBLAI3.f90
	${SRCCSM}/ORYZA/ORYZA_Interface.f90
	${SRCCSM}/ORYZA/OR_ROOTG.f90
	${SRCCSM}/ORYZA/SUBDD.f90
	${SRCCSM}/ORYZA/SSKYC.f90
	${SRCCSM}/IPSLIN.for
	${SRCCSM}/CSP_NUPTAK.for
	${SRCCSM}/OPSOILNI.for
	${SRCCSM}/HMET.for
	${SRCCSM}/weathr.for
	${SRCCSM}/OpStemp.for
	${SRCCSM}/RI_Phenol.for
	${SRCCSM}/TR_Transpl_p.for
	${SRCCSM}/OPVIEW.for
	${SRCCSM}/CSP_IPPHENOL.for
	${SRCCSM}/RI_Calcshk.for
	${SRCCSM}/SC_CNG_mods.for
	${SRCCSM}/PT_OPHARV.for
	${SRCCSM}/SLigCeres.for
	${SRCCSM}/P_CERES.for
	${SRCCSM}/WEATHR_Inp.for
	${SRCCSM}/SC_PHENOL.for
	${SRCCSM}/VEGDM.for
	${SRCCSM}/OPWEATH.for
	${SRCCSM}/SoilCNPinit_C.for
	${SRCCSM}/MZ_OPHARV.for
	${SRCCSM}/ETPHOT.for
	${SRCCSM}/CSP_IPDMND_OUT.for
	${SRCCSM}/WH_NUPTAK.FOR
	${SRCCSM}/INFIL.for
	${SRCCSM}/DATES.for
	${SRCCSM}/WATBAL.for
	${SRCCSM}/CSP_INCOMP_OUT.for
	${SRCCSM}/ROOTWU.for
	${SRCCSM}/SoilCBal_C.for
	${SRCCSM}/PT_THTIME.for
	${SRCCSM}/PESTCP.for
	${SRCCSM}/CHEMICAL.for
	${SRCCSM}/IRRIG.for
	${SRCCSM}/CSP_INPHENOL.for
	${SRCCSM}/IPPEST.for
	${SRCCSM}/SOIL.for
	${SRCCSM}/SENES.for
	${SRCCSM}/TR_Opharv.for
	${SRCCSM}/PlantNBal.for
	${SRCCSM}/TR_OPGROW.for
	${SRCCSM}/RootSoilVol.for
	${SRCCSM}/PPlantSubs.for
	${SRCCSM}/CANOPY.for
	${SRCCSM}/DECRAT_C.for
	${SRCCSM}/CSCAS.for
	${SRCCSM}/CSP_OPHARV.for
	${SRCCSM}/TR_Nfacto.for
	${SRCCSM}/TR_Grosub.for
	${SRCCSM}/IPTILL.for
	${SRCCSM}/CE_RATIO_C.for
	${SRCCSM}/SoilOrg_init.for
	${SRCCSM}/SECLI.for
	${SRCCSM}/UTILS.for
	${SRCCSM}/SOILDYN.for
	${SRCCSM}/PT_SUBSTOR.for
	${SRCCSM}/TextureClass.for
	${SRCCSM}/MULCHWAT.for
	${SRCCSM}/SoilNoPoBal.for
	${SRCCSM}/TR_Ipcrop.for
	${SRCCSM}/FreshWt.for
	${SRCCSM}/OpSoilOrg.for
	${SRCCSM}/FLOODI.for
	${SRCCSM}/TR_Rootgr.for
	${SRCCSM}/CSP_HRes.for
	${SRCCSM}/CSREADS.for
	${SRCCSM}/Fert_Place.for
	${SRCCSM}/MZ_IX_NUPTAK.for
	${SRCCSM}/WH_KUPTAK.FOR
	${SRCCSM}/MZ_IX_GROSUB.for
	${SRCCSM}/INSTGE.for
	${SRCCSM}/P_CGRO.for
	${SRCCSM}/SOLAR.for
	${SRCCSM}/SOMDEC_C.for
	${SRCCSM}/PATH.for
	${SRCCSM}/WH_NFACTO.for
	${SRCCSM}/SC_OPHARV.for
	${SRCCSM}/TILEDRAIN.for
	${SRCCSM}/ROOTS.for
	${SRCCSM}/MZ_CERES.for
	${SRCCSM}/ml_NUPTAK.for
	${SRCCSM}/MZ_NUPTAK.for
	${SRCCSM}/SESOIL.for
	${SRCCSM}/IPPARM.for
	${SRCCSM}/LINDM.for
	${SRCCSM}/soilpibal.for
	${SRCCSM}/CSP_OPGROW.for
	${SRCCSM}/ML_CERES.for
	${SRCCSM}/CSP_IPPLNT.for
	${SRCCSM}/RI_Rootgr.for
	${SRCCSM}/ML_rootgr.for
	${SRCCSM}/IPIBS.for
	${SRCCSM}/Paddy_Mgmt.for
	${SRCCSM}/SOILNI.for
	${SRCCSM}/IPSOIL.for
	${SRCCSM}/CSP_SENES.for
	${SRCCSM}/NFLUX.for
	${SRCCSM}/SC_CNGRO.for
	${SRCCSM}/SoilPoBal_C.for
	${SRCCSM}/p_plant.for
	${SRCCSM}/LMATCH.for
	${SRCCSM}/SEEDDM.for
	${SRCCSM}/OXLAYER.for
	${SRCCSM}/MZ_ROOTS.for
	${SRCCSM}/LITDEC_C.for
	${SRCCSM}/SoilNBalSum.for
	${SRCCSM}/INSOIL.for
	${SRCCSM}/SENESADD_C.for
	${SRCCSM}/WH_OPGROW.FOR
	${SRCCSM}/CSCAS_Interface.for
	${SRCCSM}/RESPIR.for
	${SRCCSM}/IMMOBLIMIT_C.for
	${SRCCSM}/MZ_NFACTO.for
	${SRCCSM}/CSM.for
	${SRCCSM}/PHENOL.for
	${SRCCSM}/MOBIL.for
	${SRCCSM}/OPSUM.for
	${SRCCSM}/OPMULCH.for
)

# Define the executable in terms of the source files
ADD_EXECUTABLE(${CSMEXE} ${CSM_FILES})

#####################################################
# Add the needed libraries and special compiler flags
#####################################################

# This links CSM to others if exists (separate them by space)
TARGET_LINK_LIBRARIES(${CSMEXE})

# Uncomment if you need to link to BLAS and LAPACK
#TARGET_LINK_LIBRARIES(${CSMEXE} ${BLAS_LIBRARIES}
#                                ${LAPACK_LIBRARIES}
#                                ${CMAKE_THREAD_LIBS_INIT})

# Uncomment if you have parallization
#IF(USE_OPENMP)
#    SET_TARGET_PROPERTIES(${CSMEXE} PROPERTIES
#                          COMPILE_FLAGS "${OpenMP_Fortran_FLAGS}"
#                          LINK_FLAGS "${OpenMP_Fortran_FLAGS}")
#ELSEIF(USE_MPI)
#    SET_TARGET_PROPERTIES(${CSMEXE} PROPERTIES
#                          COMPILE_FLAGS "${MPI_Fortran_COMPILE_FLAGS}"
#                          LINK_FLAGS "${MPI_Fortran_LINK_FLAGS}")
#    INCLUDE_DIRECTORIES(${MPI_Fortran_INCLUDE_PATH})
#    TARGET_LINK_LIBRARIES(${CSMEXE} ${MPI_Fortran_LIBRARIES})
#ENDIF(USE_OPENMP)

#####################################
# Tell how to install this executable
#####################################

IF(WIN32)
    SET(CMAKE_INSTALL_PREFIX "C:\\Program Files")
ELSE()
    SET(CMAKE_INSTALL_PREFIX ~/tmp)
ENDIF(WIN32)
INSTALL(TARGETS ${CSMEXE} RUNTIME DESTINATION bin)

# Add a distclean target to the Makefile
ADD_CUSTOM_TARGET(distclean 
    COMMAND ${CMAKE_COMMAND} -P ${CMAKE_SOURCE_DIR}/distclean.cmake
)

