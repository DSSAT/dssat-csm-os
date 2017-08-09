!=======================================================================
!  MODULE CSMVersion
!  08/08/2017 WP Written
!=======================================================================

      MODULE CSMVersion
!     Contains defintion of CSM Version Number which are 
!     used throughout the model.
      SAVE

!=======================================================================

!     Global CSM Version Number
      TYPE VersionType
        INTEGER :: Major = 4
        INTEGER :: Minor = 6
        INTEGER :: Model = 5
        INTEGER :: Build = 7
      END TYPE VersionType
      TYPE (VersionType) Version
      CHARACTER(len=10) :: VBranch = '-develop  '
!     CHARACTER(len=10) :: VBranch = '-release  '

!     Version history:  
!       4.6.5.07 chp 07/26/2017 ModuleDefs.fpp (was .for), portability changes
!       4.6.5.06 chp 07/14/2017 Alfalfa added (KJB)
!       4.6.5.05 chp 07/13/2017 Z energy balance re-instated (BAK/KJB)
!                               New YCA cassava model added (PM/DA)
!       4.6.5.04 chp 07/12/2017 ET-based irrigation, minor fixes 
!       4.6.5.03 chp 07/08/2017 Cross-platform compatibility, 
!                    potato temperature sensitivity (R.Raymundo)
!       4.6.5.02 chp 07/06/2017 Y2K-2025, EXNAME in Summary.OUT, data updates
!       4.6.5.01 chp 05/10/2017 Workshop 2017 version. Remove SALUS. 
!       4.6.1.14 chp 05/09/2017 CSV output updates, minor sunflower changes, 
!                               remove auto forage variables
!       4.6.1.13 chp 05/05/2017 Forage model, cross-platform compatibility
!       4.6.1.12 chp 04/17/2017 Growth stage, supply-limited irrigation added
!       4.6.1.11 chp 04/07/2017 CSV format output, fix stage 2 rice longevity issue
!                               NWheat max N uptake from CUL file.
!       4.6.1.10 chp 12/15/2016 Bugfixes tillage, rice, SALUS
!       4.6.1.09 chp 11/18/2016 NWheat grain N bug fixed.
!                               Rice photosynth now uses SLPF.
!       4.6.1.08 chp 10/18/2016 CSCER compatibility with sequence, NWheat species file update
!                               Rice N uptake initialization, taro N uptake calculation 
!       4.6.1.07 chp 07/29/2016 Millet changes from KJB, Puddled field deactivates after 30 days dry
!       4.6.1.06 chp 07/21/2016 DSSAT soil temperature is default method, per GH.
!       4.6.1.05 chp 07/21/2016 EPIC soil temperature is default method.
!                               4-character Weather file in data directory recognized.
!                               SNOW variable correctly passed.
!                               Fatal error messaging improved.
!       4.6.1.04 chp 03/03/2016 Removed some screen messages in GROW and CSP_GROW. Remove make file. 
!       4.6.1.03 chp 08/28/2015 NWheat added 
!       4.6.1.02 chp 09/10/2015 Major changes to millet model (KJB) 
!       4.6.1.01 chp 08/28/2015 NWheat added 
!       4.6.1.00 GH  07/01/2015 DSSAT Version 4.6.1 Release
!       4.6.0.49 GH  06/19/2015 CERES-Rice drought stress issue
!       4.6.0.48 GH  06/18/2015 Harvest fix CERES & minor data file updates
!       4.6.0.47 GH  06/18/2015 Added MaxPest to ModuleDefs
!       4.6.0.46 chp 06/17/2015 Summary.OUT - fixed problems with environmental summary outputs
!       4.6.0.45 vsh 06/16/2015 added Tony's files; Added Taro files
!       4.6.0.44 chp 01/12/2015 CSCER Evaluate.OUT remove headers
!                               OPSUM improved handling of "-99" values
!                               Rice - Ceres & ORYZA - output N uptake
!       4.6.0.43 chp 01/12/2015 Portability - file exension consistency
!       4.6.0.42 chp 12/05/2014 DSSAT sprint -1st batch of changes
!                               Dates and headers, JDATE.CDE fix, portability fixes, 
!                               "F" forced auto-planting date option, minor changes
!       4.6.0.41 chp 11/04/2014 KThorp changes for portability
!       4.6.0.40 chp 09/19/2014 Minor changes
!       4.6.0.39 chp 07/25/2014 Allow daily input of CO2 in weather file (header CO2 or DCO2)
!                               Move PI and RAD to global constants
!       4.6.0.38 chp 07/17/2014 Fixed output switches
!       4.6.0.37 chp 06/05/2014 ORYZA code updated to ORYZA3 (Tao Li)
!       4.6.0.36 chp 05/07/2014 SALUS model updates, ESCP, EPCP added to Summary.OUT
!       4.6.0.35 chp 03/28/2014 Taro initialization fixed - RO
!       4.6.0.34 chp 03/28/2014 Minor changes Weather, CSCER, sorghum
!       4.6.0.33 chp 03/26/2014 Y2K crossover at 2020
!       4.6.0.32 chp 03/20/2014 Minor bug fixes, millet and auto-irrig
!       4.6.0.31 chp 03/10/2014 Sorghum P linkage
!       4.6.0.30 chp 02/21/2014 CSCER, CSCRP, CSCAS updates
!       4.6.0.29 chp 01/23/2014 Fixed bug in auto planting when IHARI = "R"
!       4.6.0.28 chp 01/16/2014 Suppress screen output for VBOSE=zero.
!       4.6.0.27 chp 01/12/2014 Added abiotic stresses to overview.out
!       4.6.0.26 chp 12/22/2013 Fixed issue with tillage routine for crop rotations
!       4.6.0.25 chp 12/03/2013 
!       4.6.0.24 chp 08/30/2013 Add in Tony's code from email 6/11/2013
!       4.6.0.23 chp 08/26/2013 EPIC Soil Temp added as option (METMP = "E")
!       4.6.0.22 chp 08/18/2013 Bugfix - nitrification units conversion
!       4.6.0.21 chp 05/25/2013 Fixed problem with crop-model compatibility check.
!       4.6.0.20 gh  04/27/2013 Cassava module added.
!       4.6.0.19 chp 04/19/2013 Salus generic crop model added.
!       4.6.0.18 chp 10/25/2012 Sugarcane CO2 response to photosynthesis.
!       4.6.0.17 chp 07/01/2012 Minor changes to match v4.5.2.1 release version.
!       4.6.0.16 chp 04/06/2012 Rollback CSCER and CSCRP. ORYZA minor changes.
!       4.6.0.15 chp 04/05/2012 Format changes for rice cultivar input
!                    potato & rice models OK, 
!                    need to rollback CSCER & CSCRP next build
!       4.6.0.14 chp 03/14/2012  
!                US  rice temperature responses, PHINT moved from SPE to CUL
!                GH  ecotype file for potato, RUE1 & RUE2
!                LAH revise CSCRP and CSCER
!       4.6.0.13 chp 03/13/2012 CHP / TL Add ORYZA rice model, synch w/ 4.5.1.27
!       4.6.0.12 chp 01/03/2012 JIL fix potato read stmts 
!       4.6.0.11 chp 12/15/2011 JIL remove P4 from potato 
!       4.6.0.10 chp 12/09/2011 Remove ksat estimation
!       4.6.0.9  chp 12/08/2011 All codes changed to 046
!       4.6.0.8  chp 11/17/2011 GFF version - equivalent to v4.5.1.22
!       4.6.0.7  chp 11/10/2011 Revert to old drainage routines.
!                               Denitrification rate for flooded field = 50% NO3/d
!                               Fixed discontinuity in potential soil evap routine
!       4.6.0.6  chp 10/29/2011 Modified CO2 effects to transpiration (SPAM, TRANS)
!       4.6.0.5  chp 09/22/2011 Drainage modifications JTR
!                               Enabled Canola
!                               CO2 response for potato
!       4.6.0.4  chp 08/30/2011 Sorghum changes GH, CSCER, CSCRP changes, LAH.
!       4.6.0.3  chp 08/30/2011 Added vapor pressure as optional weather input.
!       4.6.0.2  gh  06/29/2011 Sorghum cul file re-order.
!       4.6.0.1  chp 06/28/2011 v4.6
!                               Changes to CSCER, CSCRP, incl. spe, eco, cul formats
!       4.5.1.0  chp 10/10/2010 V4.5 Release version
!       4.0.2.0  chp 08/11/2005 Release
!       4.0.1.0  chp 01/28/2004 Release Version 

!======================================================================
      END MODULE CSMVersion
!======================================================================
