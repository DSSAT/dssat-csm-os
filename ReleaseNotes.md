# Release notes

v4.8.6.0, 2026-07-01

 **Updated Tools**
	  - The DSSAT Shell has been updated with all APPs for tools and utilties now visible from the main screen
	  - Various older programs were removed, including ICSim, the introductory crop simulation tool; and WeatherAnalogue.

 **New Tools**
    - GB2 - Replacement for GBuild for graphical visualization of crop model outcomes.
    - XB2 - Replacement for XBuild for creating crop management files (FileX); requires Java.

 **New Crop models**
    - Flax - CROPGRO
    - Hemp - CROPGRO
    - Rye  - CROPGRO

**New Functionality**
    - Stage-based environmental summaries in EnvSum.OUT and EnvSum.csv.
    - Cumulative precipitation output to Weather.OUT and weather CSV output files.
    - Plant nitrogen output variables for CROPGRO crops.
    - Winter cold- and drought-survival processes for perennial forage crops.
    - Support for automatic irrigation types E and T.
    - Warnings when water-table depth is specified in initial soil conditions.
    - Added water, nitrogen, carbon and mass-balance to support future model-integration developments.

**Plant model revisions**
    - CROPGRO-Flax
      - Added a new flax crop module, genotype files.
    - CROPGRO-Cotton
      - Updated cultivar, ecotype, and species files.
      - Added missing minimum and maximum values to ecotype parameters.
    - CROPGRO-Hemp
      - Updated cultivar and ecotype files to resolve cultivar-name conflicts.
    - CROPGRO-Forage
      - Added winter cold- and drought-survival responses.
      - Updated Bahia grass, Guinea grass, and alfalfa genotype files.
    - CROPGRO-Cassava
      - Added protection against division-by-zero errors.
    - CROPGRO-Bell Pepper
      - Corrected fruit dry-matter calculations.
    - CROPGRO
      - Added plant nitrogen output variables.
      - Add hemp and flax genotpye files
      - Updatedalfalfa, cotton, Bahia grass and Guinea grass genotype files.
    - CSCER and CSCRP
      - Added rye genotpye files
      - Updated FileA reading and path-handling procedures.
    - CSCRP, CSCAS and CSYCA
      - Initialization revisions and updates.

**Miscellaneous**
    - Corrected the monthly temperature-amplitude calculation in WEATHR.for.
    - Corrected automatic planting inputs to prevent simulation loops caused by invalid PFRST and PLAST values.
    - Corrected the surface-soil-layer calculation used in soil nitrogen immobilization processes.
    - Updated IMMOBLIMIT logic to allow immobilization when the available value is greater than zero.
    - Added protection against divide-by-zero errors in soils shallower than 20 cm.
    - Corrected a rare soil-water balance issue.
    - Added safeguards for cold-hardening calculations.
    - Improved diagnostics for missing tillage operations.
    - Corrected reporting in Overview.OUT, including Environmental and stress factors sections.
    - Corrected SoilWat.OUT, SomLitC.csv, and SoilDyn.OUT reporting.
    - Added cumulative precipitation to weather output files.
    - Corrected fresh-harvest weight, crop-stage, and emergence-date reporting.
    - Corrected by-product weight at harvest (BWAH) in MgmtEvent.OUT.
    - Removed trailing commas from Summary.csv and EnvSum.csv.
    - Improved output formatting for Linux and MacOS platforms.
    - Removed obsolete dummy files that could generate unwanted fort.* files.
    - Updated DATA.CDE, DETAIL.CDE and related variable definitions.
    - Corrected duplicate and missing data-code definitions.
    - Updated the minimum supported CMake version.
    - Addressed Intel Fortran deprecation messages.
    - Removed compiler warnings and improved source-code maintenance.
    - Improved input handling for tab characters in weather and soil-profile files.
    - Improved error messages for forage-survival parameters and weather-file formatting.
    - Updated the atmospheric CO2 data file with a revised format.
    - Corrected initilization issue affecting Sequence simulations.

 **Experimental Data**
    - Added experimental data files for six experiments for rye.
    - Added experimental data files for two experiments for flax.
    - Added experimental data files for four experiments for hemp.
    - Removed duplicate records from various weather files.
    - Updated the cultivar name in the crop management file to match the name shown in the cultivar file.
    - Updated the year in various measurement files to show either a two digit or four digit year.
    - Added example treatments for Automow for Brachiaria.
    - Corrected units for N2OEC in the measurement file for the QUKY crop rotation experiments.
    - Updated long-term weather data for Midville, Donalsonville, and Penfield (Athens), Georgia.
    - Add a four digit year to the phenological observations for wheat and barley.
    - Added example treatments for Automow for Bahia.
    - Added two new experiments from Brazil for soybean.
    - Removed "0" values for water table depth from the initial conditions section in the crop management file.
    - Removed tabs from several soil and weather files.
