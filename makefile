FC  = gfortran
w32: FC = i686-w64-mingw32-gfortran
w64: FC = x86_64-w64-mingw32-gfortran
FCFLAGS = -Wall -fd-lines-as-comments -finit-character=32 -ffixed-line-length-none -ffree-line-length-none -nocpp -g
FLFLAGS = -static -Wall

EFILE = dscsm046.so
w32: EFILE = dscsm046.exe
w64: EFILE = dscsm046.exe

OD = linux/obj
w32: OD = w32/obj
w64: OD = w64/obj
cleanw32: OD = w32/obj
cleanw64: OD = w64/obj

INCLUDE = -I$(OD) -J$(OD)

BD = linux/build
w32: BD = w32/build
w64: BD = w64/build
cleanw32: BD = w32/build
cleanw64: BD = w64/build

OBJS =	\
	$(OD)/CENTURY.o\
	$(OD)/CROPGRO.o\
	$(OD)/CSCAS_Interface.o\
	$(OD)/CSCERES_Interface.o\
	$(OD)/CSCRP_Interface.o\
	$(OD)/CSM.o\
	$(OD)/CSP_HRes.o\
	$(OD)/CSP_PHOTO.o\
	$(OD)/ESR_SoilEvap.o\
	$(OD)/ETPHOT.o\
	$(OD)/ETPHR.o\
	$(OD)/FCHEM.o\
	$(OD)/Fert_Place.o\
	$(OD)/Flood_Chem.o\
	$(OD)/forage.o\
	$(OD)/for_asmdm.o\
	$(OD)/for_canopy.o\
	$(OD)/for_ch2oref.o\
	$(OD)/for_demand.o\
	$(OD)/for_dormancy.o\
	$(OD)/for_freeze.o\
	$(OD)/for_grow.o\
	$(OD)/for_harv.o\
	$(OD)/for_hres_cgro.o\
	$(OD)/for_incomp.o\
	$(OD)/for_ipparm.o\
	$(OD)/for_ippest.o\
	$(OD)/for_ipphenol.o\
	$(OD)/for_ipplnt.o\
	$(OD)/for_ipprog.o\
	$(OD)/for_lindm.o\
	$(OD)/for_mobil.o\
	$(OD)/for_nfix.o\
	$(OD)/for_nuptak.o\
	$(OD)/for_opgrow.o\
	$(OD)/for_opharv.o\
	$(OD)/for_opmob.o\
	$(OD)/for_oppest.o\
	$(OD)/for_opview.o\
	$(OD)/for_pestcp.o\
	$(OD)/for_pest.o\
	$(OD)/for_phenol.o\
	$(OD)/for_photo.o\
	$(OD)/for_plantnbal.o\
	$(OD)/for_poddet.o\
	$(OD)/for_pods.o\
	$(OD)/for_respir.o\
	$(OD)/for_rootdm.o\
	$(OD)/for_roots.o\
	$(OD)/for_rstages.o\
	$(OD)/for_sdcomp.o\
	$(OD)/for_seeddm.o\
	$(OD)/for_senmob.o\
	$(OD)/for_vegdm.o\
	$(OD)/for_veggr.o\
	$(OD)/HMET.o\
	$(OD)/HResCeres.o\
	$(OD)/HRes_CGRO.o\
	$(OD)/IMMOBLIMIT_C.o\
	$(OD)/INCORPOR_C.o\
	$(OD)/INFIL.o\
	$(OD)/input_sub.o\
	$(OD)/ipexp.o\
	$(OD)/IPHedley_C.o\
	$(OD)/IPHedley_inorg.o\
	$(OD)/Ipphenol.o\
	$(OD)/IPWTH_alt.o\
	$(OD)/MgmtOps.o\
	$(OD)/ML_CERES.o\
	$(OD)/ML_rootgr.o\
	$(OD)/ModuleDefs.o\
	$(OD)/MZ_GROSUB.o\
	$(OD)/MZ_IX_GROSUB.o\
	$(OD)/MZ_IX_KNUMBER.o\
	$(OD)/MZ_IX_LEAFAREA.o\
	$(OD)/MZ_IX_PHENOL.o\
	$(OD)/MZ_IX_PHOTSYNT.o\
	$(OD)/MZ_IX_PLANTG.o\
	$(OD)/MZ_IX_RADABS.o\
	$(OD)/MZ_NFACTO.o\
	$(OD)/MZ_PHENOL.o\
	$(OD)/OM_Place.o\
	$(OD)/OpFlood.o\
	$(OD)/Opgrow.o\
	$(OD)/OpPlantP.o\
	$(OD)/OpSoilKi.o\
	$(OD)/OpSoilOrg.o\
	$(OD)/OpSoilPi.o\
	$(OD)/OpStemp.o\
	$(OD)/OPSUM.o\
	$(OD)/optempy2k.o\
	$(OD)/OPWBAL.o\
	$(OD)/Paddy_Mgmt.o\
	$(OD)/plant.o\
	$(OD)/p_plant.o\
	$(OD)/PPlantSubs.o\
	$(OD)/PT_GROSUB.o\
	$(OD)/PT_NFACTO.o\
	$(OD)/PT_NUPTAK.o\
	$(OD)/PT_OPGROW.o\
	$(OD)/PT_OPHARV.o\
	$(OD)/PT_PHASEI.o\
	$(OD)/PT_PHENOL.o\
	$(OD)/PT_ROOTGR.o\
	$(OD)/PT_THTIME.o\
	$(OD)/P_Uptake.o\
	$(OD)/RETC_VG.o\
	$(OD)/RI_Calcshk.o\
	$(OD)/RICE.o\
	$(OD)/RI_Grosub.o\
	$(OD)/RI_Ipcrop.o\
	$(OD)/RI_Nfacto.o\
	$(OD)/RI_Nuptak.o\
	$(OD)/RI_Opgrow.o\
	$(OD)/RI_Opharv.o\
	$(OD)/RI_Phenol.o\
	$(OD)/RI_Rootgr.o\
	$(OD)/RI_Tillsub.o\
	$(OD)/RI_Transpl_g.o\
	$(OD)/RI_Transpl_p.o\
	$(OD)/RNOFF.o\
	$(OD)/ROOTS.o\
	$(OD)/RootSoilVol.o\
	$(OD)/RStages.o\
	$(OD)/RunList.o\
	$(OD)/SAL_Stemp.o\
	$(OD)/SATFLO.o\
	$(OD)/SC_CanesimCanopy.o\
	$(OD)/SC_Canop3.o\
	$(OD)/SC_CNG_mods.o\
	$(OD)/SC_OUTPUT.o\
	$(OD)/SC_PARTIT.o\
	$(OD)/SC_PHENOL.o\
	$(OD)/SC_PHOTOS.o\
	$(OD)/SC_Poplt3.o\
	$(OD)/SC_ROOTG.o\
	$(OD)/SENESADD_C.o\
	$(OD)/SG_CERES.o\
	$(OD)/SG_ROOTGR.o\
	$(OD)/SLigCeres.o\
	$(OD)/SoilCBal_C.o\
	$(OD)/SOILEV.o\
	$(OD)/SoilKi.o\
	$(OD)/SoilMixing.o\
	$(OD)/SoilNBalSum.o\
	$(OD)/SoilNiBal.o\
	$(OD)/SOILNI.o\
	$(OD)/SoilNi_init.o\
	$(OD)/SoilOrg_init.o\
	$(OD)/SoilPBalSum.o\
	$(OD)/soilpibal.o\
	$(OD)/SoilPi.o\
	$(OD)/SOMINIT_c.o\
	$(OD)/SOMLITPRINT_C.o\
	$(OD)/SPAM.o\
	$(OD)/SPSUBS.o\
	$(OD)/STEMP_EPIC.o\
	$(OD)/STEMP.o\
	$(OD)/SW_FreshWt.o\
	$(OD)/SW_GROSUB.o\
	$(OD)/TextureClass.o\
	$(OD)/Tillage.o\
	$(OD)/TillEvent.o\
	$(OD)/TR_Calcshk.o\
	$(OD)/TR_Grosub.o\
	$(OD)/TR_Ipcrop.o\
	$(OD)/TR_Nfacto.o\
	$(OD)/TR_Nuptak.o\
	$(OD)/TR_Opharv.o\
	$(OD)/TR_Phenol.o\
	$(OD)/TR_Rootgr.o\
	$(OD)/TR_Tillsub.o\
	$(OD)/TR_Transpl_g.o\
	$(OD)/TR_Transpl_p.o\
	$(OD)/TSOMLIT_C.o\
	$(OD)/WBAL.o\
	$(OD)/WBSUBS.o\
	$(OD)/weathr.o\
	$(OD)/WGEN.o\
	$(OD)/WTHMOD.o\
	$(OD)/OR_Opharv.o\
	$(OD)/SALUS_Roots.o\
	$(OD)/SALUS_Subs.o\
	$(OD)/addinf.o\
	$(OD)/addint.o\
	$(OD)/addrea.o\
	$(OD)/addref.o\
	$(OD)/addstf.o\
	$(OD)/addstr.o\
	$(OD)/ambusy.o\
	$(OD)/copfl2.o\
	$(OD)/decchk.o\
	$(OD)/decdou.o\
	$(OD)/decint.o\
	$(OD)/decrea.o\
	$(OD)/decrec.o\
	$(OD)/dectim.o\
	$(OD)/delfil.o\
	$(OD)/dtardp.o\
	$(OD)/dtdpar.o\
	$(OD)/dtdpst.o\
	$(OD)/dtfsecmp.o\
	$(OD)/dtfsedp.o\
	$(OD)/dtleap.o\
	$(OD)/dtsys.o\
	$(OD)/entcha.o\
	$(OD)/entdch.o\
	$(OD)/entddo.o\
	$(OD)/entdin.o\
	$(OD)/entdou.o\
	$(OD)/entdre.o\
	$(OD)/entdti.o\
	$(OD)/entdyn.o\
	$(OD)/enthlp.o\
	$(OD)/entint.o\
	$(OD)/entrea.o\
	$(OD)/enttim.o\
	$(OD)/entyno.o\
	$(OD)/extens.o\
	$(OD)/flexist.o\
	$(OD)/flname.o\
	$(OD)/fopens.o\
	$(OD)/getrec.o\
	$(OD)/getun2.o\
	$(OD)/getun.o\
	$(OD)/ifindi.o\
	$(OD)/istart.o\
	$(OD)/iunifl.o\
	$(OD)/lint.o\
	$(OD)/lowerc.o\
	$(OD)/messwrt.o\
	$(OD)/movavr.o\
	$(OD)/notnul.o\
	$(OD)/outar2.o\
	$(OD)/outcom.o\
	$(OD)/outplt.o\
	$(OD)/parsword.o\
	$(OD)/rchrsrc.o\
	$(OD)/rdacha.o\
	$(OD)/rdador.o\
	$(OD)/rdadou.o\
	$(OD)/rdainr.o\
	$(OD)/rdaint.o\
	$(OD)/rdalog.o\
	$(OD)/rdarea.o\
	$(OD)/rdarer.o\
	$(OD)/rdatim.o\
	$(OD)/rddata.o\
	$(OD)/rddtmp.o\
	$(OD)/rderr.o\
	$(OD)/rderri.o\
	$(OD)/rdfcha.o\
	$(OD)/rdfdor.o\
	$(OD)/rdfdou.o\
	$(OD)/rdfinr.o\
	$(OD)/rdfint.o\
	$(OD)/rdflog.o\
	$(OD)/rdfrea.o\
	$(OD)/rdfrer.o\
	$(OD)/rdfrom.o\
	$(OD)/rdftim.o\
	$(OD)/rdinar.o\
	$(OD)/rdindt.o\
	$(OD)/rdindx.o\
	$(OD)/rdinit.o\
	$(OD)/rdinlv.o\
	$(OD)/rdinne.o\
	$(OD)/rdinqr2.o\
	$(OD)/rdinqr3.o\
	$(OD)/rdinqr.o\
	$(OD)/rdlex.o\
	$(OD)/rdmcha.o\
	$(OD)/rdmdef.o\
	$(OD)/rdmdou.o\
	$(OD)/rdmint.o\
	$(OD)/rdmlog.o\
	$(OD)/rdmrea.o\
	$(OD)/rdmtim.o\
	$(OD)/rdpars.o\
	$(OD)/rdscha.o\
	$(OD)/rdsctb.o\
	$(OD)/rdsdor.o\
	$(OD)/rdsdou.o\
	$(OD)/rdsets.o\
	$(OD)/rdsinr.o\
	$(OD)/rdsint.o\
	$(OD)/rdslog.o\
	$(OD)/rdsrea.o\
	$(OD)/rdsrer.o\
	$(OD)/rdstim.o\
	$(OD)/rdtmp1.o\
	$(OD)/rdtmp2.o\
	$(OD)/reaand.o\
	$(OD)/reanor.o\
	$(OD)/remove.o\
	$(OD)/sfindg.o\
	$(OD)/sortch.o\
	$(OD)/sortin.o\
	$(OD)/str_copy.o\
	$(OD)/swpi4.o\
	$(OD)/ttuver.o\
	$(OD)/unifl.o\
	$(OD)/usedun.o\
	$(OD)/ver4_23.o\
	$(OD)/warning_OR.o\
	$(OD)/words.o\
	$(OD)/wracha.o\
	$(OD)/wradou.o\
	$(OD)/wraint.o\
	$(OD)/wralog.o\
	$(OD)/wrarea.o\
	$(OD)/wratim.o\
	$(OD)/wrinit.o\
	$(OD)/wrscha.o\
	$(OD)/wrsdou.o\
	$(OD)/wrsint.o\
	$(OD)/wrslog.o\
	$(OD)/wrsrea.o\
	$(OD)/wrstim.o\
	$(OD)/ASMDM.o\
	$(OD)/AUTHAR.o\
	$(OD)/AUTPLT.o\
	$(OD)/CANOPY.o\
	$(OD)/CE_RATIO_C.o\
	$(OD)/CHEMICAL.o\
	$(OD)/CO2VAL.o\
	$(OD)/CRSIMDEF.o\
	$(OD)/CSCAS.o\
	$(OD)/CSCER.o\
	$(OD)/CSCRP.o\
	$(OD)/CSDISEASE.o\
	$(OD)/CSP_CANOPY.o\
	$(OD)/CSP_CASUPRO.o\
	$(OD)/CSP_DEMAND.o\
	$(OD)/CSP_GROW_CANE.o\
	$(OD)/CSP_GROW.o\
	$(OD)/CSP_INCOMP.o\
	$(OD)/CSP_INCOMP_OUT.o\
	$(OD)/CSP_INPHENOL.o\
	$(OD)/CSP_IPDMND_OUT.o\
	$(OD)/CSP_IPPHENOL.o\
	$(OD)/CSP_IPPLNT.o\
	$(OD)/CSP_MOBIL.o\
	$(OD)/CSP_NUPTAK.o\
	$(OD)/CSP_OPGROW.o\
	$(OD)/CSP_OPHARV.o\
	$(OD)/CSP_PHENOL.o\
	$(OD)/CSP_RESPIR.o\
	$(OD)/CSP_ROOTS.o\
	$(OD)/CSP_SENES.o\
	$(OD)/CSP_VEGGR.o\
	$(OD)/CSREADS.o\
	$(OD)/CSUTS.o\
	$(OD)/DATES.o\
	$(OD)/DECRAT_C.o\
	$(OD)/DEMAND.o\
	$(OD)/EFLOW_C.o\
	$(OD)/EQUIL2.o\
	$(OD)/ERROR.o\
	$(OD)/FLOODI.o\
	$(OD)/Flood_Irrig.o\
	$(OD)/FREEZE.o\
	$(OD)/FreshWt.o\
	$(OD)/GROW.o\
	$(OD)/INCOMP.o\
	$(OD)/Info.o\
	$(OD)/INSOIL.o\
	$(OD)/INSTGE.o\
	$(OD)/INTRO.o\
	$(OD)/INVAR.o\
	$(OD)/IPCHEM.o\
	$(OD)/IPECO.o\
	$(OD)/IPENV.o\
	$(OD)/IPIBS.o\
	$(OD)/IPMAN.o\
	$(OD)/IPPARM.o\
	$(OD)/IPPEST.o\
	$(OD)/IPPLNT.o\
	$(OD)/IPPROG.o\
	$(OD)/IPSIM.o\
	$(OD)/IPSLIN.o\
	$(OD)/IPSOIL.o\
	$(OD)/IPSOIL_Inp.o\
	$(OD)/IPTILL.o\
	$(OD)/IPVAR.o\
	$(OD)/IRRIG.o\
	$(OD)/LAND.o\
	$(OD)/LINDM.o\
	$(OD)/LITDEC_C.o\
	$(OD)/LMATCH.o\
	$(OD)/ML_GROSUB.o\
	$(OD)/ML_NFACT.o\
	$(OD)/ml_NUPTAK.o\
	$(OD)/ML_opharv.o\
	$(OD)/ML_PHASEI.o\
	$(OD)/ML_PHENOL.o\
	$(OD)/ML_TILLSUB.o\
	$(OD)/MOBIL.o\
	$(OD)/MULCHEVAP.o\
	$(OD)/MULCHLAYER.o\
	$(OD)/MULCHWAT.o\
	$(OD)/MZ_CERES.o\
	$(OD)/MZ_IX_NUPTAK.o\
	$(OD)/MZ_IX_RESPIR.o\
	$(OD)/MZ_KUPTAK.o\
	$(OD)/MZ_NUPTAK.o\
	$(OD)/MZ_OPGROW.o\
	$(OD)/MZ_OPHARV.o\
	$(OD)/MZ_OPNIT.o\
	$(OD)/MZ_ROOTS.o\
	$(OD)/NCHECK_C.o\
	$(OD)/NCHECK_inorg.o\
	$(OD)/NCHECK_organic.o\
	$(OD)/NFIX.o\
	$(OD)/NFLUX.o\
	$(OD)/NUPTAK.o\
	$(OD)/OPETPHOT.o\
	$(OD)/OPFLOODN.o\
	$(OD)/OPGeneric.o\
	$(OD)/OPGEN.o\
	$(OD)/OPHARV.o\
	$(OD)/OPHEAD.o\
	$(OD)/OPMULCH.o\
	$(OD)/OPPEST.o\
	$(OD)/OPSOILNI.o\
	$(OD)/OPSOMLIT_C.o\
	$(OD)/OPSTRESS.o\
	$(OD)/OPTEMPXY2K.o\
	$(OD)/OPVIEW.o\
	$(OD)/OPWEATH.o\
	$(OD)/OXLAYER.o\
	$(OD)/PARTIT_C.o\
	$(OD)/PATH.o\
	$(OD)/P_CASUPRO.o\
	$(OD)/P_CERES.o\
	$(OD)/P_CGRO.o\
	$(OD)/PESTCP.o\
	$(OD)/PEST.o\
	$(OD)/PET.o\
	$(OD)/PHENOL.o\
	$(OD)/PHOTO.o\
	$(OD)/P_IPPLNT.o\
	$(OD)/PlantNBal.o\
	$(OD)/PODDET.o\
	$(OD)/PODS.o\
	$(OD)/PT_SUBSTOR.o\
	$(OD)/READS.o\
	$(OD)/RESPIR.o\
	$(OD)/RI_GNURSE.o\
	$(OD)/RI_KUPTAK.o\
	$(OD)/ROOTDM.o\
	$(OD)/ROOTWU.o\
	$(OD)/RPLACE_C.o\
	$(OD)/SC_CNGRO.o\
	$(OD)/SC_COEFFS.o\
	$(OD)/SC_OPHARV.o\
	$(OD)/SDCOMP.o\
	$(OD)/SECLI.o\
	$(OD)/SECROP.o\
	$(OD)/SEEDDM.o\
	$(OD)/SEFERT.o\
	$(OD)/SEFLD.o\
	$(OD)/SEFREQ.o\
	$(OD)/SEHARV.o\
	$(OD)/SEINIT.o\
	$(OD)/SEIRR.o\
	$(OD)/SENES.o\
	$(OD)/SENS.o\
	$(OD)/SEPEST.o\
	$(OD)/SEPLT.o\
	$(OD)/SERES.o\
	$(OD)/SESIM.o\
	$(OD)/SESOIL.o\
	$(OD)/SETIME.o\
	$(OD)/SEVAR.o\
	$(OD)/SEWTH.o\
	$(OD)/SG_GROSUB.o\
	$(OD)/SG_NFACT.o\
	$(OD)/sg_NUPTAK.o\
	$(OD)/SG_OPHARV.o\
	$(OD)/SG_PHASEI.o\
	$(OD)/SG_PHENOL.o\
	$(OD)/SoilCNPinit_C.o\
	$(OD)/SOILDYN.o\
	$(OD)/SOIL.o\
	$(OD)/SoilK_init.o\
	$(OD)/SoilNoBal_C.o\
	$(OD)/SoilNoBal.o\
	$(OD)/SoilNoPoBal.o\
	$(OD)/SoilOrg.o\
	$(OD)/SoilPi_init.o\
	$(OD)/SoilPoBal_C.o\
	$(OD)/SoilPoBal.o\
	$(OD)/SOLAR.o\
	$(OD)/SOMDEC_C.o\
	$(OD)/SOMFIX_C.o\
	$(OD)/TILEDRAIN.o\
	$(OD)/TRANS.o\
	$(OD)/TR_OPGROW.o\
	$(OD)/TR_SUBSTOR.o\
	$(OD)/UTILS.o\
	$(OD)/VEGDM.o\
	$(OD)/VEGGR.o\
	$(OD)/Warning.o\
	$(OD)/WATBAL.o\
	$(OD)/WEATHR_Inp.o\
	$(OD)/WTHSET.o\
	$(OD)/SALUS_OPGROW.o\
	$(OD)/SALUS_OPHARV.o\
	$(OD)/INSW.o\
	$(OD)/INTGRL.o\
	$(OD)/LIMIT.o\
	$(OD)/LINT2.o\
	$(OD)/TIMER2.o\
	$(OD)/G_Exper.o\
	$(OD)/G_Soil.o\
	$(OD)/Ncrop3.o\
	$(OD)/NightT.o\
	$(OD)/Nnostress2.o\
	$(OD)/OR_Opgrow.o\
	$(OD)/ORYZA1.o\
	$(OD)/ORYZA_Interface.o\
	$(OD)/quadpack.o\
	$(OD)/wnostress.o\
	$(OD)/WStress2.o\
	$(OD)/SALUS.o\
	$(OD)/SALUS_NPuptake.o\
	$(OD)/dtnow.o\
	$(OD)/fatalerr.o\
	$(OD)/fopengstandard.o\
	$(OD)/ifindc.o\
	$(OD)/messini.o\
	$(OD)/messinq.o\
	$(OD)/openlogf.o\
	$(OD)/outdat.o\
	$(OD)/outsel.o\
	$(OD)/recread.o\
	$(OD)/recreadi.o\
	$(OD)/recreadt.o\
	$(OD)/ttutil.o\
	$(OD)/ttutilprefs.o\
	$(OD)/upperc.o\
	$(OD)/DS1900.o\
	$(OD)/GETOBS.o\
	$(OD)/INDEXX.o\
	$(OD)/INQOBS.o\
	$(OD)/INTGR2.o\
	$(OD)/OBSINI.o\
	$(OD)/OBSSYS.o\
	$(OD)/OBSTRG.o\
	$(OD)/OPCF.o\
	$(OD)/OPINIT.o\
	$(OD)/OPNF.o\
	$(OD)/OPREAD.o\
	$(OD)/OPSC.o\
	$(OD)/OPSTOR.o\
	$(OD)/OPSYS.o\
	$(OD)/OPWRITE.o\
	$(OD)/TTHEAD.o\
	$(OD)/GPPARGET.o\
	$(OD)/GPPARSET.o\
	$(OD)/OR_PHENOL.o\
	$(OD)/OR_ROOTG.o\
	$(OD)/PARTITION.o\
	$(OD)/SASTRO.o\
	$(OD)/SGPC1.o\
	$(OD)/SGPC2.o\
	$(OD)/SGPCDT.o\
	$(OD)/SGPL.o\
	$(OD)/SRDPRF.o\
	$(OD)/SSKYC.o\
	$(OD)/SUBCBC.o\
	$(OD)/SUBCD2.o\
	$(OD)/SUBDD.o\
	$(OD)/SUBGRN.o\
	$(OD)/SUBLAI3.o\
	$(OD)/SVPS1.o\
	$(OD)/VARIABLE.o\

linux:  $(OD)/linux.o $(OBJS)
	@mkdir -p $(BD)
	$(FC) $(FLFLAGS) -o $(BD)/$(EFILE) $(OD)/linux.o $(OBJS)

w32: $(OD)/windows.o $(OBJS)
	@mkdir -p $(BD)
	$(FC) $(FLFLAGS) -o $(BD)/$(EFILE) $(OD)/windows.o $(OBJS)

w64: $(OD)/windows.o $(OBJS)
	@mkdir -p $(BD)
	$(FC) $(FLFLAGS) -o $(BD)/$(EFILE) $(OD)/windows.o $(OBJS) echo 
clean:
	@rm -f $(OD)/*.o $(OD)/*.mod $(OD)/*/*.o $(BD)/$(EFILE)

cleanw32:
	@rm -f $(OD)/*.o $(OD)/mod/*.mod $(OD)/*/*.o $(BD)/$(EFILE)

cleanw64:
	@rm -f $(OD)/*.o $(OD)/mod/*.mod $(OD)/*/*.o $(BD)/$(EFILE)

copy:
	@cp $(BD)/$(EFILE) ~/DSSAT46/


$(OD)/CENTURY.o: CENTURY.for $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o $(OD)/SoilMixing.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c CENTURY.for -o $@

$(OD)/CROPGRO.o: CROPGRO.for $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c CROPGRO.for -o $@

$(OD)/CSCAS_Interface.o: CSCAS_Interface.for $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c CSCAS_Interface.for -o $@

$(OD)/CSCERES_Interface.o: CSCERES_Interface.for $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c CSCERES_Interface.for -o $@

$(OD)/CSCRP_Interface.o: CSCRP_Interface.for $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c CSCRP_Interface.for -o $@

$(OD)/CSM.o: CSM.for $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o $(OD)/OPHEAD.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c CSM.for -o $@

$(OD)/CSP_HRes.o: CSP_HRes.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c CSP_HRes.for -o $@

$(OD)/CSP_PHOTO.o: CSP_PHOTO.for $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c CSP_PHOTO.for -o $@

$(OD)/ESR_SoilEvap.o: ESR_SoilEvap.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ESR_SoilEvap.for -o $@

$(OD)/ETPHOT.o: ETPHOT.for $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ETPHOT.for -o $@

$(OD)/ETPHR.o: ETPHR.for $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ETPHR.for -o $@

$(OD)/FCHEM.o: FCHEM.for $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c FCHEM.for -o $@

$(OD)/Fert_Place.o: Fert_Place.for $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c Fert_Place.for -o $@

$(OD)/Flood_Chem.o: Flood_Chem.for $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c Flood_Chem.for -o $@

$(OD)/forage.o: forage.for $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c forage.for -o $@

$(OD)/for_asmdm.o: for_asmdm.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c for_asmdm.for -o $@

$(OD)/for_canopy.o: for_canopy.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c for_canopy.for -o $@

$(OD)/for_ch2oref.o: for_ch2oref.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c for_ch2oref.for -o $@

$(OD)/for_demand.o: for_demand.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c for_demand.for -o $@

$(OD)/for_dormancy.o: for_dormancy.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c for_dormancy.for -o $@

$(OD)/for_freeze.o: for_freeze.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c for_freeze.for -o $@

$(OD)/for_grow.o: for_grow.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c for_grow.for -o $@

$(OD)/for_harv.o: for_harv.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c for_harv.for -o $@

$(OD)/for_hres_cgro.o: for_hres_cgro.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c for_hres_cgro.for -o $@

$(OD)/for_incomp.o: for_incomp.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c for_incomp.for -o $@

$(OD)/for_ipparm.o: for_ipparm.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c for_ipparm.for -o $@

$(OD)/for_ippest.o: for_ippest.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c for_ippest.for -o $@

$(OD)/for_ipphenol.o: for_ipphenol.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c for_ipphenol.for -o $@

$(OD)/for_ipplnt.o: for_ipplnt.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c for_ipplnt.for -o $@

$(OD)/for_ipprog.o: for_ipprog.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c for_ipprog.for -o $@

$(OD)/for_lindm.o: for_lindm.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c for_lindm.for -o $@

$(OD)/for_mobil.o: for_mobil.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c for_mobil.for -o $@

$(OD)/for_nfix.o: for_nfix.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c for_nfix.for -o $@

$(OD)/for_nuptak.o: for_nuptak.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c for_nuptak.for -o $@

$(OD)/for_opgrow.o: for_opgrow.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c for_opgrow.for -o $@

$(OD)/for_opharv.o: for_opharv.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c for_opharv.for -o $@

$(OD)/for_opmob.o: for_opmob.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c for_opmob.for -o $@

$(OD)/for_oppest.o: for_oppest.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c for_oppest.for -o $@

$(OD)/for_opview.o: for_opview.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c for_opview.for -o $@

$(OD)/for_pestcp.o: for_pestcp.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c for_pestcp.for -o $@

$(OD)/for_pest.o: for_pest.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c for_pest.for -o $@

$(OD)/for_phenol.o: for_phenol.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c for_phenol.for -o $@

$(OD)/for_photo.o: for_photo.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c for_photo.for -o $@

$(OD)/for_plantnbal.o: for_plantnbal.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c for_plantnbal.for -o $@

$(OD)/for_poddet.o: for_poddet.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c for_poddet.for -o $@

$(OD)/for_pods.o: for_pods.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c for_pods.for -o $@

$(OD)/for_respir.o: for_respir.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c for_respir.for -o $@

$(OD)/for_rootdm.o: for_rootdm.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c for_rootdm.for -o $@

$(OD)/for_roots.o: for_roots.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c for_roots.for -o $@

$(OD)/for_rstages.o: for_rstages.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c for_rstages.for -o $@

$(OD)/for_sdcomp.o: for_sdcomp.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c for_sdcomp.for -o $@

$(OD)/for_seeddm.o: for_seeddm.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c for_seeddm.for -o $@

$(OD)/for_senmob.o: for_senmob.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c for_senmob.for -o $@

$(OD)/for_vegdm.o: for_vegdm.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c for_vegdm.for -o $@

$(OD)/for_veggr.o: for_veggr.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c for_veggr.for -o $@

$(OD)/HMET.o: HMET.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c HMET.for -o $@

$(OD)/HResCeres.o: HResCeres.for $(OD)/ModuleDefs.o $(OD)/SLigCeres.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c HResCeres.for -o $@

$(OD)/HRes_CGRO.o: HRes_CGRO.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c HRes_CGRO.for -o $@

$(OD)/IMMOBLIMIT_C.o: IMMOBLIMIT_C.for $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c IMMOBLIMIT_C.for -o $@

$(OD)/INCORPOR_C.o: INCORPOR_C.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c INCORPOR_C.for -o $@

$(OD)/INFIL.o: INFIL.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c INFIL.for -o $@

$(OD)/input_sub.o: input_sub.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c input_sub.for -o $@

$(OD)/ipexp.o: ipexp.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ipexp.for -o $@

$(OD)/IPHedley_C.o: IPHedley_C.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c IPHedley_C.for -o $@

$(OD)/IPHedley_inorg.o: IPHedley_inorg.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c IPHedley_inorg.for -o $@

$(OD)/Ipphenol.o: Ipphenol.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c Ipphenol.for -o $@

$(OD)/IPWTH_alt.o: IPWTH_alt.for $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c IPWTH_alt.for -o $@

$(OD)/MgmtOps.o: MgmtOps.for $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o $(OD)/OPSUM.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c MgmtOps.for -o $@

$(OD)/ML_CERES.o: ML_CERES.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ML_CERES.for -o $@

$(OD)/ML_rootgr.o: ML_rootgr.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ML_rootgr.for -o $@

$(OD)/ModuleDefs.o: ModuleDefs.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ModuleDefs.for -o $@

$(OD)/MZ_GROSUB.o: MZ_GROSUB.for $(OD)/ModuleDefs.o $(OD)/SLigCeres.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c MZ_GROSUB.for -o $@

$(OD)/MZ_IX_GROSUB.o: MZ_IX_GROSUB.for $(OD)/ModuleDefs.o $(OD)/SLigCeres.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c MZ_IX_GROSUB.for -o $@

$(OD)/MZ_IX_KNUMBER.o: MZ_IX_KNUMBER.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c MZ_IX_KNUMBER.for -o $@

$(OD)/MZ_IX_LEAFAREA.o: MZ_IX_LEAFAREA.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c MZ_IX_LEAFAREA.for -o $@

$(OD)/MZ_IX_PHENOL.o: MZ_IX_PHENOL.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c MZ_IX_PHENOL.for -o $@

$(OD)/MZ_IX_PHOTSYNT.o: MZ_IX_PHOTSYNT.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c MZ_IX_PHOTSYNT.for -o $@

$(OD)/MZ_IX_PLANTG.o: MZ_IX_PLANTG.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c MZ_IX_PLANTG.for -o $@

$(OD)/MZ_IX_RADABS.o: MZ_IX_RADABS.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c MZ_IX_RADABS.for -o $@

$(OD)/MZ_NFACTO.o: MZ_NFACTO.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c MZ_NFACTO.for -o $@

$(OD)/MZ_PHENOL.o: MZ_PHENOL.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c MZ_PHENOL.for -o $@

$(OD)/OM_Place.o: OM_Place.for $(OD)/ModuleDefs.o $(OD)/IPSOIL.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c OM_Place.for -o $@

$(OD)/OpFlood.o: OpFlood.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c OpFlood.for -o $@

$(OD)/Opgrow.o: Opgrow.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c Opgrow.for -o $@

$(OD)/OpPlantP.o: OpPlantP.for $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c OpPlantP.for -o $@

$(OD)/OpSoilKi.o: OpSoilKi.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c OpSoilKi.for -o $@

$(OD)/OpSoilOrg.o: OpSoilOrg.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c OpSoilOrg.for -o $@

$(OD)/OpSoilPi.o: OpSoilPi.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c OpSoilPi.for -o $@

$(OD)/OpStemp.o: OpStemp.for $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c OpStemp.for -o $@

$(OD)/OPSUM.o: OPSUM.for $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c OPSUM.for -o $@

$(OD)/optempy2k.o: optempy2k.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c optempy2k.for -o $@

$(OD)/OPWBAL.o: OPWBAL.for $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c OPWBAL.for -o $@

$(OD)/Paddy_Mgmt.o: Paddy_Mgmt.for $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c Paddy_Mgmt.for -o $@

$(OD)/plant.o: plant.for $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c plant.for -o $@

$(OD)/p_plant.o: p_plant.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c p_plant.for -o $@

$(OD)/PPlantSubs.o: PPlantSubs.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c PPlantSubs.for -o $@

$(OD)/PT_GROSUB.o: PT_GROSUB.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c PT_GROSUB.for -o $@

$(OD)/PT_NFACTO.o: PT_NFACTO.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c PT_NFACTO.for -o $@

$(OD)/PT_NUPTAK.o: PT_NUPTAK.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c PT_NUPTAK.for -o $@

$(OD)/PT_OPGROW.o: PT_OPGROW.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c PT_OPGROW.for -o $@

$(OD)/PT_OPHARV.o: PT_OPHARV.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c PT_OPHARV.for -o $@

$(OD)/PT_PHASEI.o: PT_PHASEI.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c PT_PHASEI.for -o $@

$(OD)/PT_PHENOL.o: PT_PHENOL.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c PT_PHENOL.for -o $@

$(OD)/PT_ROOTGR.o: PT_ROOTGR.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c PT_ROOTGR.for -o $@

$(OD)/PT_THTIME.o: PT_THTIME.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c PT_THTIME.for -o $@

$(OD)/P_Uptake.o: P_Uptake.for $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c P_Uptake.for -o $@

$(OD)/RETC_VG.o: RETC_VG.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c RETC_VG.for -o $@

$(OD)/RI_Calcshk.o: RI_Calcshk.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c RI_Calcshk.for -o $@

$(OD)/RICE.o: RICE.for $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c RICE.for -o $@

$(OD)/RI_Grosub.o: RI_Grosub.for $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o $(OD)/SLigCeres.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c RI_Grosub.for -o $@

$(OD)/RI_Ipcrop.o: RI_Ipcrop.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c RI_Ipcrop.for -o $@

$(OD)/RI_Nfacto.o: RI_Nfacto.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c RI_Nfacto.for -o $@

$(OD)/RI_Nuptak.o: RI_Nuptak.for $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c RI_Nuptak.for -o $@

$(OD)/RI_Opgrow.o: RI_Opgrow.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c RI_Opgrow.for -o $@

$(OD)/RI_Opharv.o: RI_Opharv.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c RI_Opharv.for -o $@

$(OD)/RI_Phenol.o: RI_Phenol.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c RI_Phenol.for -o $@

$(OD)/RI_Rootgr.o: RI_Rootgr.for $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c RI_Rootgr.for -o $@

$(OD)/RI_Tillsub.o: RI_Tillsub.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c RI_Tillsub.for -o $@

$(OD)/RI_Transpl_g.o: RI_Transpl_g.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c RI_Transpl_g.for -o $@

$(OD)/RI_Transpl_p.o: RI_Transpl_p.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c RI_Transpl_p.for -o $@

$(OD)/RNOFF.o: RNOFF.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c RNOFF.for -o $@

$(OD)/ROOTS.o: ROOTS.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ROOTS.for -o $@

$(OD)/RootSoilVol.o: RootSoilVol.for $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c RootSoilVol.for -o $@

$(OD)/RStages.o: RStages.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c RStages.for -o $@

$(OD)/RunList.o: RunList.for $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c RunList.for -o $@

$(OD)/SAL_Stemp.o: SAL_Stemp.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SAL_Stemp.for -o $@

$(OD)/SATFLO.o: SATFLO.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SATFLO.for -o $@

$(OD)/SC_CanesimCanopy.o: SC_CanesimCanopy.for $(OD)/SC_CNG_mods.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SC_CanesimCanopy.for -o $@

$(OD)/SC_Canop3.o: SC_Canop3.for $(OD)/SC_CNG_mods.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SC_Canop3.for -o $@

$(OD)/SC_CNG_mods.o: SC_CNG_mods.for $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SC_CNG_mods.for -o $@

$(OD)/SC_OUTPUT.o: SC_OUTPUT.for $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o $(OD)/SC_CNG_mods.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SC_OUTPUT.for -o $@

$(OD)/SC_PARTIT.o: SC_PARTIT.for $(OD)/SC_CNG_mods.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SC_PARTIT.for -o $@

$(OD)/SC_PHENOL.o: SC_PHENOL.for $(OD)/ModuleDefs.o $(OD)/SC_CNG_mods.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SC_PHENOL.for -o $@

$(OD)/SC_PHOTOS.o: SC_PHOTOS.for $(OD)/SC_CNG_mods.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SC_PHOTOS.for -o $@

$(OD)/SC_Poplt3.o: SC_Poplt3.for $(OD)/SC_CNG_mods.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SC_Poplt3.for -o $@

$(OD)/SC_ROOTG.o: SC_ROOTG.for $(OD)/ModuleDefs.o $(OD)/SC_CNG_mods.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SC_ROOTG.for -o $@

$(OD)/SENESADD_C.o: SENESADD_C.for $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o $(OD)/IPSOIL.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SENESADD_C.for -o $@

$(OD)/SG_CERES.o: SG_CERES.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SG_CERES.for -o $@

$(OD)/SG_ROOTGR.o: SG_ROOTGR.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SG_ROOTGR.for -o $@

$(OD)/SLigCeres.o: SLigCeres.for $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SLigCeres.for -o $@

$(OD)/SoilCBal_C.o: SoilCBal_C.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SoilCBal_C.for -o $@

$(OD)/SOILEV.o: SOILEV.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SOILEV.for -o $@

$(OD)/SoilKi.o: SoilKi.for $(OD)/ModuleDefs.o $(OD)/SoilMixing.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SoilKi.for -o $@

$(OD)/SoilMixing.o: SoilMixing.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SoilMixing.for -o $@

$(OD)/SoilNBalSum.o: SoilNBalSum.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SoilNBalSum.for -o $@

$(OD)/SoilNiBal.o: SoilNiBal.for $(OD)/ModuleDefs.o $(OD)/SoilNBalSum.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SoilNiBal.for -o $@

$(OD)/SOILNI.o: SOILNI.for $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o $(OD)/SoilMixing.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SOILNI.for -o $@

$(OD)/SoilNi_init.o: SoilNi_init.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SoilNi_init.for -o $@

$(OD)/SoilOrg_init.o: SoilOrg_init.for $(OD)/ModuleDefs.o $(OD)/IPSOIL.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SoilOrg_init.for -o $@

$(OD)/SoilPBalSum.o: SoilPBalSum.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SoilPBalSum.for -o $@

$(OD)/soilpibal.o: soilpibal.for $(OD)/ModuleDefs.o $(OD)/SoilPBalSum.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c soilpibal.for -o $@

$(OD)/SoilPi.o: SoilPi.for $(OD)/ModuleDefs.o $(OD)/SoilMixing.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SoilPi.for -o $@

$(OD)/SOMINIT_c.o: SOMINIT_c.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SOMINIT_c.for -o $@

$(OD)/SOMLITPRINT_C.o: SOMLITPRINT_C.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SOMLITPRINT_C.for -o $@

$(OD)/SPAM.o: SPAM.for $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SPAM.for -o $@

$(OD)/SPSUBS.o: SPSUBS.for $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SPSUBS.for -o $@

$(OD)/STEMP_EPIC.o: STEMP_EPIC.for $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c STEMP_EPIC.for -o $@

$(OD)/STEMP.o: STEMP.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c STEMP.for -o $@

$(OD)/SW_FreshWt.o: SW_FreshWt.for $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SW_FreshWt.for -o $@

$(OD)/SW_GROSUB.o: SW_GROSUB.for $(OD)/ModuleDefs.o $(OD)/SLigCeres.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SW_GROSUB.for -o $@

$(OD)/TextureClass.o: TextureClass.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c TextureClass.for -o $@

$(OD)/Tillage.o: Tillage.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c Tillage.for -o $@

$(OD)/TillEvent.o: TillEvent.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c TillEvent.for -o $@

$(OD)/TR_Calcshk.o: TR_Calcshk.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c TR_Calcshk.for -o $@

$(OD)/TR_Grosub.o: TR_Grosub.for $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c TR_Grosub.for -o $@

$(OD)/TR_Ipcrop.o: TR_Ipcrop.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c TR_Ipcrop.for -o $@

$(OD)/TR_Nfacto.o: TR_Nfacto.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c TR_Nfacto.for -o $@

$(OD)/TR_Nuptak.o: TR_Nuptak.for $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c TR_Nuptak.for -o $@

$(OD)/TR_Opharv.o: TR_Opharv.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c TR_Opharv.for -o $@

$(OD)/TR_Phenol.o: TR_Phenol.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c TR_Phenol.for -o $@

$(OD)/TR_Rootgr.o: TR_Rootgr.for $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c TR_Rootgr.for -o $@

$(OD)/TR_Tillsub.o: TR_Tillsub.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c TR_Tillsub.for -o $@

$(OD)/TR_Transpl_g.o: TR_Transpl_g.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c TR_Transpl_g.for -o $@

$(OD)/TR_Transpl_p.o: TR_Transpl_p.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c TR_Transpl_p.for -o $@

$(OD)/TSOMLIT_C.o: TSOMLIT_C.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c TSOMLIT_C.for -o $@

$(OD)/WBAL.o: WBAL.for $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c WBAL.for -o $@

$(OD)/WBSUBS.o: WBSUBS.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c WBSUBS.for -o $@

$(OD)/weathr.o: weathr.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c weathr.for -o $@

$(OD)/WGEN.o: WGEN.for $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c WGEN.for -o $@

$(OD)/WTHMOD.o: WTHMOD.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c WTHMOD.for -o $@

$(OD)/OR_Opharv.o: ORYZA/OR_Opharv.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ORYZA/OR_Opharv.for -o $@

$(OD)/SALUS_Roots.o: SALUS/SALUS_Roots.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SALUS/SALUS_Roots.for -o $@

$(OD)/SALUS_Subs.o: SALUS/SALUS_Subs.for $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SALUS/SALUS_Subs.for -o $@

$(OD)/addinf.o: ttutil/addinf.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/addinf.for -o $@

$(OD)/addint.o: ttutil/addint.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/addint.for -o $@

$(OD)/addrea.o: ttutil/addrea.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/addrea.for -o $@

$(OD)/addref.o: ttutil/addref.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/addref.for -o $@

$(OD)/addstf.o: ttutil/addstf.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/addstf.for -o $@

$(OD)/addstr.o: ttutil/addstr.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/addstr.for -o $@

$(OD)/ambusy.o: ttutil/ambusy.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/ambusy.for -o $@

$(OD)/copfl2.o: ttutil/copfl2.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/copfl2.for -o $@

$(OD)/decchk.o: ttutil/decchk.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/decchk.for -o $@

$(OD)/decdou.o: ttutil/decdou.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/decdou.for -o $@

$(OD)/decint.o: ttutil/decint.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/decint.for -o $@

$(OD)/decrea.o: ttutil/decrea.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/decrea.for -o $@

$(OD)/decrec.o: ttutil/decrec.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/decrec.for -o $@

$(OD)/dectim.o: ttutil/dectim.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/dectim.for -o $@

$(OD)/delfil.o: ttutil/delfil.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/delfil.for -o $@

$(OD)/dtardp.o: ttutil/dtardp.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/dtardp.for -o $@

$(OD)/dtdpar.o: ttutil/dtdpar.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/dtdpar.for -o $@

$(OD)/dtdpst.o: ttutil/dtdpst.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/dtdpst.for -o $@

$(OD)/dtfsecmp.o: ttutil/dtfsecmp.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/dtfsecmp.for -o $@

$(OD)/dtfsedp.o: ttutil/dtfsedp.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/dtfsedp.for -o $@

$(OD)/dtleap.o: ttutil/dtleap.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/dtleap.for -o $@

$(OD)/dtsys.o: ttutil/dtsys.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/dtsys.for -o $@

$(OD)/entcha.o: ttutil/entcha.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/entcha.for -o $@

$(OD)/entdch.o: ttutil/entdch.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/entdch.for -o $@

$(OD)/entddo.o: ttutil/entddo.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/entddo.for -o $@

$(OD)/entdin.o: ttutil/entdin.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/entdin.for -o $@

$(OD)/entdou.o: ttutil/entdou.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/entdou.for -o $@

$(OD)/entdre.o: ttutil/entdre.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/entdre.for -o $@

$(OD)/entdti.o: ttutil/entdti.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/entdti.for -o $@

$(OD)/entdyn.o: ttutil/entdyn.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/entdyn.for -o $@

$(OD)/enthlp.o: ttutil/enthlp.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/enthlp.for -o $@

$(OD)/entint.o: ttutil/entint.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/entint.for -o $@

$(OD)/entrea.o: ttutil/entrea.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/entrea.for -o $@

$(OD)/enttim.o: ttutil/enttim.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/enttim.for -o $@

$(OD)/entyno.o: ttutil/entyno.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/entyno.for -o $@

$(OD)/extens.o: ttutil/extens.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/extens.for -o $@

$(OD)/flexist.o: ttutil/flexist.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/flexist.for -o $@

$(OD)/flname.o: ttutil/flname.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/flname.for -o $@

$(OD)/fopens.o: ttutil/fopens.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/fopens.for -o $@

$(OD)/getrec.o: ttutil/getrec.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/getrec.for -o $@

$(OD)/getun2.o: ttutil/getun2.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/getun2.for -o $@

$(OD)/getun.o: ttutil/getun.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/getun.for -o $@

$(OD)/ifindi.o: ttutil/ifindi.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/ifindi.for -o $@

$(OD)/istart.o: ttutil/istart.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/istart.for -o $@

$(OD)/iunifl.o: ttutil/iunifl.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/iunifl.for -o $@

$(OD)/lint.o: ttutil/lint.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/lint.for -o $@

$(OD)/lowerc.o: ttutil/lowerc.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/lowerc.for -o $@

$(OD)/messwrt.o: ttutil/messwrt.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/messwrt.for -o $@

$(OD)/movavr.o: ttutil/movavr.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/movavr.for -o $@

$(OD)/notnul.o: ttutil/notnul.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/notnul.for -o $@

$(OD)/outar2.o: ttutil/outar2.for $(OD)/outdat.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/outar2.for -o $@

$(OD)/outcom.o: ttutil/outcom.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/outcom.for -o $@

$(OD)/outplt.o: ttutil/outplt.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/outplt.for -o $@

$(OD)/parsword.o: ttutil/parsword.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/parsword.for -o $@

$(OD)/rchrsrc.o: ttutil/rchrsrc.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/rchrsrc.for -o $@

$(OD)/rdacha.o: ttutil/rdacha.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/rdacha.for -o $@

$(OD)/rdador.o: ttutil/rdador.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/rdador.for -o $@

$(OD)/rdadou.o: ttutil/rdadou.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/rdadou.for -o $@

$(OD)/rdainr.o: ttutil/rdainr.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/rdainr.for -o $@

$(OD)/rdaint.o: ttutil/rdaint.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/rdaint.for -o $@

$(OD)/rdalog.o: ttutil/rdalog.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/rdalog.for -o $@

$(OD)/rdarea.o: ttutil/rdarea.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/rdarea.for -o $@

$(OD)/rdarer.o: ttutil/rdarer.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/rdarer.for -o $@

$(OD)/rdatim.o: ttutil/rdatim.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/rdatim.for -o $@

$(OD)/rddata.o: ttutil/rddata.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/rddata.for -o $@

$(OD)/rddtmp.o: ttutil/rddtmp.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/rddtmp.for -o $@

$(OD)/rderr.o: ttutil/rderr.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/rderr.for -o $@

$(OD)/rderri.o: ttutil/rderri.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/rderri.for -o $@

$(OD)/rdfcha.o: ttutil/rdfcha.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/rdfcha.for -o $@

$(OD)/rdfdor.o: ttutil/rdfdor.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/rdfdor.for -o $@

$(OD)/rdfdou.o: ttutil/rdfdou.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/rdfdou.for -o $@

$(OD)/rdfinr.o: ttutil/rdfinr.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/rdfinr.for -o $@

$(OD)/rdfint.o: ttutil/rdfint.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/rdfint.for -o $@

$(OD)/rdflog.o: ttutil/rdflog.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/rdflog.for -o $@

$(OD)/rdfrea.o: ttutil/rdfrea.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/rdfrea.for -o $@

$(OD)/rdfrer.o: ttutil/rdfrer.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/rdfrer.for -o $@

$(OD)/rdfrom.o: ttutil/rdfrom.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/rdfrom.for -o $@

$(OD)/rdftim.o: ttutil/rdftim.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/rdftim.for -o $@

$(OD)/rdinar.o: ttutil/rdinar.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/rdinar.for -o $@

$(OD)/rdindt.o: ttutil/rdindt.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/rdindt.for -o $@

$(OD)/rdindx.o: ttutil/rdindx.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/rdindx.for -o $@

$(OD)/rdinit.o: ttutil/rdinit.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/rdinit.for -o $@

$(OD)/rdinlv.o: ttutil/rdinlv.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/rdinlv.for -o $@

$(OD)/rdinne.o: ttutil/rdinne.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/rdinne.for -o $@

$(OD)/rdinqr2.o: ttutil/rdinqr2.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/rdinqr2.for -o $@

$(OD)/rdinqr3.o: ttutil/rdinqr3.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/rdinqr3.for -o $@

$(OD)/rdinqr.o: ttutil/rdinqr.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/rdinqr.for -o $@

$(OD)/rdlex.o: ttutil/rdlex.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/rdlex.for -o $@

$(OD)/rdmcha.o: ttutil/rdmcha.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/rdmcha.for -o $@

$(OD)/rdmdef.o: ttutil/rdmdef.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/rdmdef.for -o $@

$(OD)/rdmdou.o: ttutil/rdmdou.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/rdmdou.for -o $@

$(OD)/rdmint.o: ttutil/rdmint.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/rdmint.for -o $@

$(OD)/rdmlog.o: ttutil/rdmlog.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/rdmlog.for -o $@

$(OD)/rdmrea.o: ttutil/rdmrea.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/rdmrea.for -o $@

$(OD)/rdmtim.o: ttutil/rdmtim.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/rdmtim.for -o $@

$(OD)/rdpars.o: ttutil/rdpars.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/rdpars.for -o $@

$(OD)/rdscha.o: ttutil/rdscha.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/rdscha.for -o $@

$(OD)/rdsctb.o: ttutil/rdsctb.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/rdsctb.for -o $@

$(OD)/rdsdor.o: ttutil/rdsdor.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/rdsdor.for -o $@

$(OD)/rdsdou.o: ttutil/rdsdou.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/rdsdou.for -o $@

$(OD)/rdsets.o: ttutil/rdsets.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/rdsets.for -o $@

$(OD)/rdsinr.o: ttutil/rdsinr.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/rdsinr.for -o $@

$(OD)/rdsint.o: ttutil/rdsint.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/rdsint.for -o $@

$(OD)/rdslog.o: ttutil/rdslog.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/rdslog.for -o $@

$(OD)/rdsrea.o: ttutil/rdsrea.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/rdsrea.for -o $@

$(OD)/rdsrer.o: ttutil/rdsrer.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/rdsrer.for -o $@

$(OD)/rdstim.o: ttutil/rdstim.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/rdstim.for -o $@

$(OD)/rdtmp1.o: ttutil/rdtmp1.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/rdtmp1.for -o $@

$(OD)/rdtmp2.o: ttutil/rdtmp2.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/rdtmp2.for -o $@

$(OD)/reaand.o: ttutil/reaand.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/reaand.for -o $@

$(OD)/reanor.o: ttutil/reanor.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/reanor.for -o $@

$(OD)/remove.o: ttutil/remove.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/remove.for -o $@

$(OD)/sfindg.o: ttutil/sfindg.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/sfindg.for -o $@

$(OD)/sortch.o: ttutil/sortch.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/sortch.for -o $@

$(OD)/sortin.o: ttutil/sortin.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/sortin.for -o $@

$(OD)/str_copy.o: ttutil/str_copy.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/str_copy.for -o $@

$(OD)/swpi4.o: ttutil/swpi4.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/swpi4.for -o $@

$(OD)/ttuver.o: ttutil/ttuver.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/ttuver.for -o $@

$(OD)/unifl.o: ttutil/unifl.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/unifl.for -o $@

$(OD)/usedun.o: ttutil/usedun.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/usedun.for -o $@

$(OD)/ver4_23.o: ttutil/ver4_23.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/ver4_23.for -o $@

$(OD)/warning_OR.o: ttutil/warning_OR.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/warning_OR.for -o $@

$(OD)/words.o: ttutil/words.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/words.for -o $@

$(OD)/wracha.o: ttutil/wracha.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/wracha.for -o $@

$(OD)/wradou.o: ttutil/wradou.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/wradou.for -o $@

$(OD)/wraint.o: ttutil/wraint.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/wraint.for -o $@

$(OD)/wralog.o: ttutil/wralog.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/wralog.for -o $@

$(OD)/wrarea.o: ttutil/wrarea.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/wrarea.for -o $@

$(OD)/wratim.o: ttutil/wratim.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/wratim.for -o $@

$(OD)/wrinit.o: ttutil/wrinit.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/wrinit.for -o $@

$(OD)/wrscha.o: ttutil/wrscha.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/wrscha.for -o $@

$(OD)/wrsdou.o: ttutil/wrsdou.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/wrsdou.for -o $@

$(OD)/wrsint.o: ttutil/wrsint.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/wrsint.for -o $@

$(OD)/wrslog.o: ttutil/wrslog.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/wrslog.for -o $@

$(OD)/wrsrea.o: ttutil/wrsrea.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/wrsrea.for -o $@

$(OD)/wrstim.o: ttutil/wrstim.for 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/wrstim.for -o $@

$(OD)/ASMDM.o: ASMDM.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ASMDM.FOR -o $@

$(OD)/AUTHAR.o: AUTHAR.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c AUTHAR.FOR -o $@

$(OD)/AUTPLT.o: AUTPLT.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c AUTPLT.FOR -o $@

$(OD)/CANOPY.o: CANOPY.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c CANOPY.FOR -o $@

$(OD)/CE_RATIO_C.o: CE_RATIO_C.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c CE_RATIO_C.FOR -o $@

$(OD)/CHEMICAL.o: CHEMICAL.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c CHEMICAL.FOR -o $@

$(OD)/CO2VAL.o: CO2VAL.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c CO2VAL.FOR -o $@

$(OD)/CRSIMDEF.o: CRSIMDEF.FOR 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c CRSIMDEF.FOR -o $@

$(OD)/CSCAS.o: CSCAS.FOR $(OD)/CRSIMDEF.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c CSCAS.FOR -o $@

$(OD)/CSCER.o: CSCER.FOR $(OD)/CRSIMDEF.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c CSCER.FOR -o $@

$(OD)/CSCRP.o: CSCRP.FOR $(OD)/CRSIMDEF.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c CSCRP.FOR -o $@

$(OD)/CSDISEASE.o: CSDISEASE.FOR 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c CSDISEASE.FOR -o $@

$(OD)/CSP_CANOPY.o: CSP_CANOPY.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c CSP_CANOPY.FOR -o $@

$(OD)/CSP_CASUPRO.o: CSP_CASUPRO.FOR $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c CSP_CASUPRO.FOR -o $@

$(OD)/CSP_DEMAND.o: CSP_DEMAND.FOR $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c CSP_DEMAND.FOR -o $@

$(OD)/CSP_GROW_CANE.o: CSP_GROW_CANE.FOR $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c CSP_GROW_CANE.FOR -o $@

$(OD)/CSP_GROW.o: CSP_GROW.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c CSP_GROW.FOR -o $@

$(OD)/CSP_INCOMP.o: CSP_INCOMP.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c CSP_INCOMP.FOR -o $@

$(OD)/CSP_INCOMP_OUT.o: CSP_INCOMP_OUT.FOR 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c CSP_INCOMP_OUT.FOR -o $@

$(OD)/CSP_INPHENOL.o: CSP_INPHENOL.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c CSP_INPHENOL.FOR -o $@

$(OD)/CSP_IPDMND_OUT.o: CSP_IPDMND_OUT.FOR 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c CSP_IPDMND_OUT.FOR -o $@

$(OD)/CSP_IPPHENOL.o: CSP_IPPHENOL.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c CSP_IPPHENOL.FOR -o $@

$(OD)/CSP_IPPLNT.o: CSP_IPPLNT.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c CSP_IPPLNT.FOR -o $@

$(OD)/CSP_MOBIL.o: CSP_MOBIL.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c CSP_MOBIL.FOR -o $@

$(OD)/CSP_NUPTAK.o: CSP_NUPTAK.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c CSP_NUPTAK.FOR -o $@

$(OD)/CSP_OPGROW.o: CSP_OPGROW.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c CSP_OPGROW.FOR -o $@

$(OD)/CSP_OPHARV.o: CSP_OPHARV.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c CSP_OPHARV.FOR -o $@

$(OD)/CSP_PHENOL.o: CSP_PHENOL.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c CSP_PHENOL.FOR -o $@

$(OD)/CSP_RESPIR.o: CSP_RESPIR.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c CSP_RESPIR.FOR -o $@

$(OD)/CSP_ROOTS.o: CSP_ROOTS.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c CSP_ROOTS.FOR -o $@

$(OD)/CSP_SENES.o: CSP_SENES.FOR $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c CSP_SENES.FOR -o $@

$(OD)/CSP_VEGGR.o: CSP_VEGGR.FOR $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c CSP_VEGGR.FOR -o $@

$(OD)/CSREADS.o: CSREADS.FOR $(OD)/CRSIMDEF.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c CSREADS.FOR -o $@

$(OD)/CSUTS.o: CSUTS.FOR $(OD)/CRSIMDEF.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c CSUTS.FOR -o $@

$(OD)/DATES.o: DATES.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c DATES.FOR -o $@

$(OD)/DECRAT_C.o: DECRAT_C.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c DECRAT_C.FOR -o $@

$(OD)/DEMAND.o: DEMAND.FOR $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c DEMAND.FOR -o $@

$(OD)/EFLOW_C.o: EFLOW_C.FOR 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c EFLOW_C.FOR -o $@

$(OD)/EQUIL2.o: EQUIL2.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c EQUIL2.FOR -o $@

$(OD)/ERROR.o: ERROR.FOR $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ERROR.FOR -o $@

$(OD)/FLOODI.o: FLOODI.FOR $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c FLOODI.FOR -o $@

$(OD)/Flood_Irrig.o: Flood_Irrig.FOR $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c Flood_Irrig.FOR -o $@

$(OD)/FREEZE.o: FREEZE.FOR 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c FREEZE.FOR -o $@

$(OD)/FreshWt.o: FreshWt.FOR $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c FreshWt.FOR -o $@

$(OD)/GROW.o: GROW.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c GROW.FOR -o $@

$(OD)/INCOMP.o: INCOMP.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c INCOMP.FOR -o $@

$(OD)/Info.o: Info.FOR $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o $(OD)/OPHEAD.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c Info.FOR -o $@

$(OD)/INSOIL.o: INSOIL.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c INSOIL.FOR -o $@

$(OD)/INSTGE.o: INSTGE.FOR 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c INSTGE.FOR -o $@

$(OD)/INTRO.o: INTRO.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c INTRO.FOR -o $@

$(OD)/INVAR.o: INVAR.FOR 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c INVAR.FOR -o $@

$(OD)/IPCHEM.o: IPCHEM.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c IPCHEM.FOR -o $@

$(OD)/IPECO.o: IPECO.FOR 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c IPECO.FOR -o $@

$(OD)/IPENV.o: IPENV.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c IPENV.FOR -o $@

$(OD)/IPIBS.o: IPIBS.FOR $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c IPIBS.FOR -o $@

$(OD)/IPMAN.o: IPMAN.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c IPMAN.FOR -o $@

$(OD)/IPPARM.o: IPPARM.FOR 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c IPPARM.FOR -o $@

$(OD)/IPPEST.o: IPPEST.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c IPPEST.FOR -o $@

$(OD)/IPPLNT.o: IPPLNT.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c IPPLNT.FOR -o $@

$(OD)/IPPROG.o: IPPROG.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c IPPROG.FOR -o $@

$(OD)/IPSIM.o: IPSIM.FOR $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c IPSIM.FOR -o $@

$(OD)/IPSLIN.o: IPSLIN.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c IPSLIN.FOR -o $@

$(OD)/IPSOIL.o: IPSOIL.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c IPSOIL.FOR -o $@

$(OD)/IPSOIL_Inp.o: IPSOIL_Inp.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c IPSOIL_Inp.FOR -o $@

$(OD)/IPTILL.o: IPTILL.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c IPTILL.FOR -o $@

$(OD)/IPVAR.o: IPVAR.FOR 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c IPVAR.FOR -o $@

$(OD)/IRRIG.o: IRRIG.FOR $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c IRRIG.FOR -o $@

$(OD)/LAND.o: LAND.FOR $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c LAND.FOR -o $@

$(OD)/LINDM.o: LINDM.FOR 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c LINDM.FOR -o $@

$(OD)/LITDEC_C.o: LITDEC_C.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c LITDEC_C.FOR -o $@

$(OD)/LMATCH.o: LMATCH.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c LMATCH.FOR -o $@

$(OD)/ML_GROSUB.o: ML_GROSUB.FOR $(OD)/ModuleDefs.o $(OD)/SLigCeres.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ML_GROSUB.FOR -o $@

$(OD)/ML_NFACT.o: ML_NFACT.FOR 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ML_NFACT.FOR -o $@

$(OD)/ml_NUPTAK.o: ml_NUPTAK.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ml_NUPTAK.FOR -o $@

$(OD)/ML_opharv.o: ML_opharv.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ML_opharv.FOR -o $@

$(OD)/ML_PHASEI.o: ML_PHASEI.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ML_PHASEI.FOR -o $@

$(OD)/ML_PHENOL.o: ML_PHENOL.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ML_PHENOL.FOR -o $@

$(OD)/ML_TILLSUB.o: ML_TILLSUB.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ML_TILLSUB.FOR -o $@

$(OD)/MOBIL.o: MOBIL.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c MOBIL.FOR -o $@

$(OD)/MULCHEVAP.o: MULCHEVAP.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c MULCHEVAP.FOR -o $@

$(OD)/MULCHLAYER.o: MULCHLAYER.FOR $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o $(OD)/IPSOIL.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c MULCHLAYER.FOR -o $@

$(OD)/MULCHWAT.o: MULCHWAT.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c MULCHWAT.FOR -o $@

$(OD)/MZ_CERES.o: MZ_CERES.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c MZ_CERES.FOR -o $@

$(OD)/MZ_IX_NUPTAK.o: MZ_IX_NUPTAK.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c MZ_IX_NUPTAK.FOR -o $@

$(OD)/MZ_IX_RESPIR.o: MZ_IX_RESPIR.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c MZ_IX_RESPIR.FOR -o $@

$(OD)/MZ_KUPTAK.o: MZ_KUPTAK.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c MZ_KUPTAK.FOR -o $@

$(OD)/MZ_NUPTAK.o: MZ_NUPTAK.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c MZ_NUPTAK.FOR -o $@

$(OD)/MZ_OPGROW.o: MZ_OPGROW.FOR $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c MZ_OPGROW.FOR -o $@

$(OD)/MZ_OPHARV.o: MZ_OPHARV.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c MZ_OPHARV.FOR -o $@

$(OD)/MZ_OPNIT.o: MZ_OPNIT.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c MZ_OPNIT.FOR -o $@

$(OD)/MZ_ROOTS.o: MZ_ROOTS.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c MZ_ROOTS.FOR -o $@

$(OD)/NCHECK_C.o: NCHECK_C.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c NCHECK_C.FOR -o $@

$(OD)/NCHECK_inorg.o: NCHECK_inorg.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c NCHECK_inorg.FOR -o $@

$(OD)/NCHECK_organic.o: NCHECK_organic.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c NCHECK_organic.FOR -o $@

$(OD)/NFIX.o: NFIX.FOR $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c NFIX.FOR -o $@

$(OD)/NFLUX.o: NFLUX.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c NFLUX.FOR -o $@

$(OD)/NUPTAK.o: NUPTAK.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c NUPTAK.FOR -o $@

$(OD)/OPETPHOT.o: OPETPHOT.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c OPETPHOT.FOR -o $@

$(OD)/OPFLOODN.o: OPFLOODN.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c OPFLOODN.FOR -o $@

$(OD)/OPGeneric.o: OPGeneric.FOR $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c OPGeneric.FOR -o $@

$(OD)/OPGEN.o: OPGEN.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c OPGEN.FOR -o $@

$(OD)/OPHARV.o: OPHARV.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c OPHARV.FOR -o $@

$(OD)/OPHEAD.o: OPHEAD.FOR $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c OPHEAD.FOR -o $@

$(OD)/OPMULCH.o: OPMULCH.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c OPMULCH.FOR -o $@

$(OD)/OPPEST.o: OPPEST.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c OPPEST.FOR -o $@

$(OD)/OPSOILNI.o: OPSOILNI.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c OPSOILNI.FOR -o $@

$(OD)/OPSOMLIT_C.o: OPSOMLIT_C.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c OPSOMLIT_C.FOR -o $@

$(OD)/OPSTRESS.o: OPSTRESS.FOR $(OD)/OPSUM.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c OPSTRESS.FOR -o $@

$(OD)/OPTEMPXY2K.o: OPTEMPXY2K.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c OPTEMPXY2K.FOR -o $@

$(OD)/OPVIEW.o: OPVIEW.FOR $(OD)/OPHEAD.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c OPVIEW.FOR -o $@

$(OD)/OPWEATH.o: OPWEATH.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c OPWEATH.FOR -o $@

$(OD)/OXLAYER.o: OXLAYER.FOR $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c OXLAYER.FOR -o $@

$(OD)/PARTIT_C.o: PARTIT_C.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c PARTIT_C.FOR -o $@

$(OD)/PATH.o: PATH.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c PATH.FOR -o $@

$(OD)/P_CASUPRO.o: P_CASUPRO.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c P_CASUPRO.FOR -o $@

$(OD)/P_CERES.o: P_CERES.FOR $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c P_CERES.FOR -o $@

$(OD)/P_CGRO.o: P_CGRO.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c P_CGRO.FOR -o $@

$(OD)/PESTCP.o: PESTCP.FOR $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c PESTCP.FOR -o $@

$(OD)/PEST.o: PEST.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c PEST.FOR -o $@

$(OD)/PET.o: PET.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c PET.FOR -o $@

$(OD)/PHENOL.o: PHENOL.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c PHENOL.FOR -o $@

$(OD)/PHOTO.o: PHOTO.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c PHOTO.FOR -o $@

$(OD)/P_IPPLNT.o: P_IPPLNT.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c P_IPPLNT.FOR -o $@

$(OD)/PlantNBal.o: PlantNBal.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c PlantNBal.FOR -o $@

$(OD)/PODDET.o: PODDET.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c PODDET.FOR -o $@

$(OD)/PODS.o: PODS.FOR $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c PODS.FOR -o $@

$(OD)/PT_SUBSTOR.o: PT_SUBSTOR.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c PT_SUBSTOR.FOR -o $@

$(OD)/READS.o: READS.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c READS.FOR -o $@

$(OD)/RESPIR.o: RESPIR.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c RESPIR.FOR -o $@

$(OD)/RI_GNURSE.o: RI_GNURSE.FOR 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c RI_GNURSE.FOR -o $@

$(OD)/RI_KUPTAK.o: RI_KUPTAK.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c RI_KUPTAK.FOR -o $@

$(OD)/ROOTDM.o: ROOTDM.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ROOTDM.FOR -o $@

$(OD)/ROOTWU.o: ROOTWU.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ROOTWU.FOR -o $@

$(OD)/RPLACE_C.o: RPLACE_C.FOR $(OD)/ModuleDefs.o $(OD)/IPSOIL.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c RPLACE_C.FOR -o $@

$(OD)/SC_CNGRO.o: SC_CNGRO.FOR $(OD)/ModuleDefs.o $(OD)/SC_CNG_mods.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SC_CNGRO.FOR -o $@

$(OD)/SC_COEFFS.o: SC_COEFFS.FOR $(OD)/ModuleDefs.o $(OD)/SC_CNG_mods.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SC_COEFFS.FOR -o $@

$(OD)/SC_OPHARV.o: SC_OPHARV.FOR $(OD)/ModuleDefs.o $(OD)/SC_CNG_mods.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SC_OPHARV.FOR -o $@

$(OD)/SDCOMP.o: SDCOMP.FOR 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SDCOMP.FOR -o $@

$(OD)/SECLI.o: SECLI.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SECLI.FOR -o $@

$(OD)/SECROP.o: SECROP.FOR 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SECROP.FOR -o $@

$(OD)/SEEDDM.o: SEEDDM.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SEEDDM.FOR -o $@

$(OD)/SEFERT.o: SEFERT.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SEFERT.FOR -o $@

$(OD)/SEFLD.o: SEFLD.FOR 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SEFLD.FOR -o $@

$(OD)/SEFREQ.o: SEFREQ.FOR 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SEFREQ.FOR -o $@

$(OD)/SEHARV.o: SEHARV.FOR 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SEHARV.FOR -o $@

$(OD)/SEINIT.o: SEINIT.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SEINIT.FOR -o $@

$(OD)/SEIRR.o: SEIRR.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SEIRR.FOR -o $@

$(OD)/SENES.o: SENES.FOR $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SENES.FOR -o $@

$(OD)/SENS.o: SENS.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SENS.FOR -o $@

$(OD)/SEPEST.o: SEPEST.FOR 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SEPEST.FOR -o $@

$(OD)/SEPLT.o: SEPLT.FOR 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SEPLT.FOR -o $@

$(OD)/SERES.o: SERES.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SERES.FOR -o $@

$(OD)/SESIM.o: SESIM.FOR 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SESIM.FOR -o $@

$(OD)/SESOIL.o: SESOIL.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SESOIL.FOR -o $@

$(OD)/SETIME.o: SETIME.FOR 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SETIME.FOR -o $@

$(OD)/SEVAR.o: SEVAR.FOR 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SEVAR.FOR -o $@

$(OD)/SEWTH.o: SEWTH.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SEWTH.FOR -o $@

$(OD)/SG_GROSUB.o: SG_GROSUB.FOR $(OD)/ModuleDefs.o $(OD)/SLigCeres.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SG_GROSUB.FOR -o $@

$(OD)/SG_NFACT.o: SG_NFACT.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SG_NFACT.FOR -o $@

$(OD)/sg_NUPTAK.o: sg_NUPTAK.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c sg_NUPTAK.FOR -o $@

$(OD)/SG_OPHARV.o: SG_OPHARV.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SG_OPHARV.FOR -o $@

$(OD)/SG_PHASEI.o: SG_PHASEI.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SG_PHASEI.FOR -o $@

$(OD)/SG_PHENOL.o: SG_PHENOL.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SG_PHENOL.FOR -o $@

$(OD)/SoilCNPinit_C.o: SoilCNPinit_C.FOR $(OD)/ModuleDefs.o $(OD)/IPSOIL.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SoilCNPinit_C.FOR -o $@

$(OD)/SOILDYN.o: SOILDYN.FOR $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SOILDYN.FOR -o $@

$(OD)/SOIL.o: SOIL.FOR $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SOIL.FOR -o $@

$(OD)/SoilK_init.o: SoilK_init.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SoilK_init.FOR -o $@

$(OD)/SoilNoBal_C.o: SoilNoBal_C.FOR $(OD)/ModuleDefs.o $(OD)/SoilNBalSum.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SoilNoBal_C.FOR -o $@

$(OD)/SoilNoBal.o: SoilNoBal.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SoilNoBal.FOR -o $@

$(OD)/SoilNoPoBal.o: SoilNoPoBal.FOR $(OD)/ModuleDefs.o $(OD)/SoilNBalSum.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SoilNoPoBal.FOR -o $@

$(OD)/SoilOrg.o: SoilOrg.FOR $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o $(OD)/IPSOIL.o $(OD)/ModuleDefs.o $(OD)/SoilMixing.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SoilOrg.FOR -o $@

$(OD)/SoilPi_init.o: SoilPi_init.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SoilPi_init.FOR -o $@

$(OD)/SoilPoBal_C.o: SoilPoBal_C.FOR $(OD)/ModuleDefs.o $(OD)/SoilPBalSum.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SoilPoBal_C.FOR -o $@

$(OD)/SoilPoBal.o: SoilPoBal.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SoilPoBal.FOR -o $@

$(OD)/SOLAR.o: SOLAR.FOR 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SOLAR.FOR -o $@

$(OD)/SOMDEC_C.o: SOMDEC_C.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SOMDEC_C.FOR -o $@

$(OD)/SOMFIX_C.o: SOMFIX_C.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SOMFIX_C.FOR -o $@

$(OD)/TILEDRAIN.o: TILEDRAIN.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c TILEDRAIN.FOR -o $@

$(OD)/TRANS.o: TRANS.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c TRANS.FOR -o $@

$(OD)/TR_OPGROW.o: TR_OPGROW.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c TR_OPGROW.FOR -o $@

$(OD)/TR_SUBSTOR.o: TR_SUBSTOR.FOR $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c TR_SUBSTOR.FOR -o $@

$(OD)/UTILS.o: UTILS.FOR $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c UTILS.FOR -o $@

$(OD)/VEGDM.o: VEGDM.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c VEGDM.FOR -o $@

$(OD)/VEGGR.o: VEGGR.FOR $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c VEGGR.FOR -o $@

$(OD)/Warning.o: Warning.FOR $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o $(OD)/OPHEAD.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c Warning.FOR -o $@

$(OD)/WATBAL.o: WATBAL.FOR $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c WATBAL.FOR -o $@

$(OD)/WEATHR_Inp.o: WEATHR_Inp.FOR 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c WEATHR_Inp.FOR -o $@

$(OD)/WTHSET.o: WTHSET.FOR 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c WTHSET.FOR -o $@

$(OD)/SALUS_OPGROW.o: SALUS/SALUS_OPGROW.FOR $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SALUS/SALUS_OPGROW.FOR -o $@

$(OD)/SALUS_OPHARV.o: SALUS/SALUS_OPHARV.FOR $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SALUS/SALUS_OPHARV.FOR -o $@

$(OD)/INSW.o: ttutil/INSW.FOR 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/INSW.FOR -o $@

$(OD)/INTGRL.o: ttutil/INTGRL.FOR 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/INTGRL.FOR -o $@

$(OD)/LIMIT.o: ttutil/LIMIT.FOR 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/LIMIT.FOR -o $@

$(OD)/LINT2.o: ttutil/LINT2.FOR 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/LINT2.FOR -o $@

$(OD)/TIMER2.o: ttutil/TIMER2.FOR 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/TIMER2.FOR -o $@

$(OD)/linux.o: linux.f90 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c linux.f90 -o $@

$(OD)/windows.o: windows.f90 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c windows.f90 -o $@

$(OD)/G_Exper.o: ORYZA/G_Exper.f90 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ORYZA/G_Exper.f90 -o $@

$(OD)/G_Soil.o: ORYZA/G_Soil.f90 $(OD)/VARIABLE.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ORYZA/G_Soil.f90 -o $@

$(OD)/Ncrop3.o: ORYZA/Ncrop3.f90 $(OD)/VARIABLE.o $(OD)/OR_ROOTG.o $(OD)/outdat.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ORYZA/Ncrop3.f90 -o $@

$(OD)/NightT.o: ORYZA/NightT.f90 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ORYZA/NightT.f90 -o $@

$(OD)/Nnostress2.o: ORYZA/Nnostress2.f90 $(OD)/VARIABLE.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ORYZA/Nnostress2.f90 -o $@

$(OD)/OR_Opgrow.o: ORYZA/OR_Opgrow.f90 $(OD)/ModuleDefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ORYZA/OR_Opgrow.f90 -o $@

$(OD)/ORYZA1.o: ORYZA/ORYZA1.f90 $(OD)/outdat.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ORYZA/ORYZA1.f90 -o $@

$(OD)/ORYZA_Interface.o: ORYZA/ORYZA_Interface.f90 $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o $(OD)/ModuleDefs.o $(OD)/VARIABLE.o $(OD)/OR_ROOTG.o $(OD)/IPSOIL.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ORYZA/ORYZA_Interface.f90 -o $@

$(OD)/quadpack.o: ORYZA/quadpack.f90 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ORYZA/quadpack.f90 -o $@

$(OD)/wnostress.o: ORYZA/wnostress.f90 $(OD)/VARIABLE.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ORYZA/wnostress.f90 -o $@

$(OD)/WStress2.o: ORYZA/WStress2.f90 $(OD)/OR_ROOTG.o $(OD)/VARIABLE.o $(OD)/outdat.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ORYZA/WStress2.f90 -o $@

$(OD)/SALUS.o: SALUS/SALUS.f90 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SALUS/SALUS.f90 -o $@

$(OD)/SALUS_NPuptake.o: SALUS/SALUS_NPuptake.f90 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c SALUS/SALUS_NPuptake.f90 -o $@

$(OD)/dtnow.o: ttutil/dtnow.f90 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/dtnow.f90 -o $@

$(OD)/fatalerr.o: ttutil/fatalerr.f90 $(OD)/ttutilprefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/fatalerr.f90 -o $@

$(OD)/fopengstandard.o: ttutil/fopengstandard.f90 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/fopengstandard.f90 -o $@

$(OD)/ifindc.o: ttutil/ifindc.f90 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/ifindc.f90 -o $@

$(OD)/messini.o: ttutil/messini.f90 $(OD)/ttutilprefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/messini.f90 -o $@

$(OD)/messinq.o: ttutil/messinq.f90 $(OD)/ttutilprefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/messinq.f90 -o $@

$(OD)/openlogf.o: ttutil/openlogf.f90 $(OD)/ttutilprefs.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/openlogf.f90 -o $@

$(OD)/outdat.o: ttutil/outdat.f90 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/outdat.f90 -o $@

$(OD)/outsel.o: ttutil/outsel.f90 $(OD)/outdat.o
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/outsel.f90 -o $@

$(OD)/recread.o: ttutil/recread.f90 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/recread.f90 -o $@

$(OD)/recreadi.o: ttutil/recreadi.f90 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/recreadi.f90 -o $@

$(OD)/recreadt.o: ttutil/recreadt.f90 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/recreadt.f90 -o $@

$(OD)/ttutil.o: ttutil/ttutil.f90 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/ttutil.f90 -o $@

$(OD)/ttutilprefs.o: ttutil/ttutilprefs.f90 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/ttutilprefs.f90 -o $@

$(OD)/upperc.o: ttutil/upperc.f90 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ttutil/upperc.f90 -o $@

$(OD)/DS1900.o: OP_OBS/DS1900.F90 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c OP_OBS/DS1900.F90 -o $@

$(OD)/GETOBS.o: OP_OBS/GETOBS.F90 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c OP_OBS/GETOBS.F90 -o $@

$(OD)/INDEXX.o: OP_OBS/INDEXX.F90 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c OP_OBS/INDEXX.F90 -o $@

$(OD)/INQOBS.o: OP_OBS/INQOBS.F90 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c OP_OBS/INQOBS.F90 -o $@

$(OD)/INTGR2.o: OP_OBS/INTGR2.F90 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c OP_OBS/INTGR2.F90 -o $@

$(OD)/OBSINI.o: OP_OBS/OBSINI.F90 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c OP_OBS/OBSINI.F90 -o $@

$(OD)/OBSSYS.o: OP_OBS/OBSSYS.F90 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c OP_OBS/OBSSYS.F90 -o $@

$(OD)/OBSTRG.o: OP_OBS/OBSTRG.F90 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c OP_OBS/OBSTRG.F90 -o $@

$(OD)/OPCF.o: OP_OBS/OPCF.F90 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c OP_OBS/OPCF.F90 -o $@

$(OD)/OPINIT.o: OP_OBS/OPINIT.F90 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c OP_OBS/OPINIT.F90 -o $@

$(OD)/OPNF.o: OP_OBS/OPNF.F90 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c OP_OBS/OPNF.F90 -o $@

$(OD)/OPREAD.o: OP_OBS/OPREAD.F90 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c OP_OBS/OPREAD.F90 -o $@

$(OD)/OPSC.o: OP_OBS/OPSC.F90 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c OP_OBS/OPSC.F90 -o $@

$(OD)/OPSTOR.o: OP_OBS/OPSTOR.F90 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c OP_OBS/OPSTOR.F90 -o $@

$(OD)/OPSYS.o: OP_OBS/OPSYS.F90 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c OP_OBS/OPSYS.F90 -o $@

$(OD)/OPWRITE.o: OP_OBS/OPWRITE.F90 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c OP_OBS/OPWRITE.F90 -o $@

$(OD)/TTHEAD.o: OP_OBS/TTHEAD.F90 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c OP_OBS/TTHEAD.F90 -o $@

$(OD)/GPPARGET.o: ORYZA/GPPARGET.F90 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ORYZA/GPPARGET.F90 -o $@

$(OD)/GPPARSET.o: ORYZA/GPPARSET.F90 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ORYZA/GPPARSET.F90 -o $@

$(OD)/OR_PHENOL.o: ORYZA/OR_PHENOL.F90 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ORYZA/OR_PHENOL.F90 -o $@

$(OD)/OR_ROOTG.o: ORYZA/OR_ROOTG.F90 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ORYZA/OR_ROOTG.F90 -o $@

$(OD)/PARTITION.o: ORYZA/PARTITION.F90 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ORYZA/PARTITION.F90 -o $@

$(OD)/SASTRO.o: ORYZA/SASTRO.F90 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ORYZA/SASTRO.F90 -o $@

$(OD)/SGPC1.o: ORYZA/SGPC1.F90 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ORYZA/SGPC1.F90 -o $@

$(OD)/SGPC2.o: ORYZA/SGPC2.F90 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ORYZA/SGPC2.F90 -o $@

$(OD)/SGPCDT.o: ORYZA/SGPCDT.F90 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ORYZA/SGPCDT.F90 -o $@

$(OD)/SGPL.o: ORYZA/SGPL.F90 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ORYZA/SGPL.F90 -o $@

$(OD)/SRDPRF.o: ORYZA/SRDPRF.F90 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ORYZA/SRDPRF.F90 -o $@

$(OD)/SSKYC.o: ORYZA/SSKYC.F90 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ORYZA/SSKYC.F90 -o $@

$(OD)/SUBCBC.o: ORYZA/SUBCBC.F90 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ORYZA/SUBCBC.F90 -o $@

$(OD)/SUBCD2.o: ORYZA/SUBCD2.F90 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ORYZA/SUBCD2.F90 -o $@

$(OD)/SUBDD.o: ORYZA/SUBDD.F90 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ORYZA/SUBDD.F90 -o $@

$(OD)/SUBGRN.o: ORYZA/SUBGRN.F90 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ORYZA/SUBGRN.F90 -o $@

$(OD)/SUBLAI3.o: ORYZA/SUBLAI3.F90 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ORYZA/SUBLAI3.F90 -o $@

$(OD)/SVPS1.o: ORYZA/SVPS1.F90 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ORYZA/SVPS1.F90 -o $@

$(OD)/VARIABLE.o: ORYZA/VARIABLE.F90 
	@mkdir -p $(dir $@)
	$(FC) $(INCLUDE) $(FCFLAGS) -c ORYZA/VARIABLE.F90 -o $@
# do not delete this line -- make depend depends on it.
