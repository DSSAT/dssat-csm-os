# Release notes

v4.8.5.0, 2024-12-01 (previous release v4.8.2.0, 2023-08-03)

 **New Tools**
    - XB2 - Replacement for XBuild for creating crop management files; developed in Java
    - TSE - Time Series Estimator - Tool for estimating cultivar coefficients using time series observations

 **New Crop models**
    - Lentil - CROPGRO
    - Bahia - PRFRM
    - Bambara groundnut - CROPGRO
    - Amaranth - CROPGRO
     
**New Functionality**
   - FileA routine to read dates with 4-digit year.
   - Output for estimated net CO2 to GHG.OUT file.
   - Estimated net CO2 emissions output in summary.OUT.
   - Added economic yield output in the Overview.OUT and Summary.OUT files.
   - Multiple harvests are now output in MgmtEvent.OUT file.
   - The Perennial Forage Model cannow be used for crop rotations.
   - Update for Bahia grass using the PRFRM Model as the default crop module.
   - New daily soil output SoilWater.OUT file for LL, DUL, SAT, and BD for all soil layers.
   
**Plant model revisions**
   - SAMUCA
      - Protections for division by zero.
   - CERES-Wheat
      - Improved output format.
   - WHAPS
      - Updated read ecotype parameters format.
      - Fixed divisions by zero caused by cultivar calibration.
   - CROPSIM
      - Fixed divisions by zero caused by cultivar calibration.
   - CROPGRO-Cotton
      - Added lint yield.
   - CROPGRO-Strawberry
      - Improved FreshWt.OUT outputs.
       
**Miscellaneous**
   - Update Taro and CERES-Wheat cultivar file variable descriptions.
   - New README.md for dssat-csm-os developers.
   - Updated soil NO3 and NH4 variables in DATA.CDE.
   - General spelling improvements in \*.CUL files.
   - Added ASCE ET parameters to species files. 
   - Updated GHG starting condition for methane production.
   - Updated Soil properties error checking for DUL, SAT, LL, SRGF and SSKS.
   - Added MAXIMA, MINIMA and trait (P/G/N) for ecotype coefficients for CROPGRO crops.
   - Added protection for SOILDYN Soil Organic Matter changes in DUL.
   - CMake option to allow dynamic link.
   - Include Gencalc rules and Gencalc About in the Data\Default directory
   - Updated verbose output mode for SoilTemp.OUT.
   - Added protections for Weather variables when NaN found in \*.WTH file.
   - Updated encoding issues of \*.CDE files.
   - Corrected spelling of Tannier to Tanier
   
 **Experimental Data**
   - Change the default soil evaporation method to Ritchie for most crops
   - Added new example data files for climate change applications using data from Turkiye based on Gurkan et al. (2021): https://doi:10.1017/S0021859621000101
   - Added a new experimental data file for soybean from Brazil provided by Moura da Silva et al. (2022): https://doi.org/10.1016/j.agrformet.2022.109075