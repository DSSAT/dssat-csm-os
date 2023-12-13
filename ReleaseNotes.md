# Release notes

v4.8.2.0, 2023-08-03
(previous release v4.8.0.027, 2021-04-27)

- **New Crop models**
    - Carinata - CROPGRO
    - Guar - CROPGRO
    - Guinea grass - FORAGE
    - Strawberry - CROPGRO 
    
- **New Functionality**
    - Soil Temperature default method switched from Ritchie equation to average daily temperature (TAVG).
    - Automatic MOW for Forage crops.
    - Multi-Harvest for CROPGRO crops.
    - N20 csv output file.
    - Added Crop status column to Summary.OUT (status of crop at end of simulation).
    - Fertigation fertilizer placed at user-specified depth.
    - Output potential root water uptake in ET.OUT.
    - Input Soil alternative electron acceptors (SAEA) in soil analysis section of FileX, default value of 26.5 mol Ceq/m3
    - Fertilizers affect buffer for methane production.
    - Added model respose to user-specified water table inputs.
    - PEST Module and MOW file now accept dates in 2- and 4-digit year format.
    
- **Plant model revisions**
    - ALOHA
       - Rollback of PineApple model to version v4.7.5.26.
    - AROID
       - Calibrated cultivar coefficients for Taro with GLUE HPC.
    - CANEGRO
       - Removed hard-coded PORMIN.
       - Increased root length density based on literature.
       - Added plant response to soil Nitrogen.
    - CERES
       - Separated CERES main subroutine file into multiple files.
       - Fixed CERES-Maize issue with germination and emergence failure.
       - Calibrated cultivar coefficients for Barley, Wheat, Maize, Millet, Sorghum and Rice with GLUE HPC.
    - CROPGRO
       - Added time required for pod/fruit to reach harvest maturity (XMAGE) for crops that use fresh weight.
       - Added new Species parameters for Nitrogen mining, leaf growth and canopy.
       - Calibrated genetics for CROPGRO crops.
       - Updated genetics for fresh weight crops.
       - Improved handling of C and N mining for Multi-Harvest.
    - CROPSIM
       - Separated CROPSIM main subroutine file into multiple files.
       - Added protection to avoid zero division when calculating VRNSTAGE in CROPSIM model.
    - MANIHOT
       - Update genetics for Cassava.
    - NWHEAT
       - Simplified ozone equations for NWheat.
       - Removed FOZ2 and SFOZ2 parameters from NWheat Ecotype file.
    - SAMUCA
       - Fixed protections with CSM subroutines to simulate under extreme latitudes.
       - Updated Local variables description. 
       - Parameter simplification phyllochron = plastochron.
       - Soil temperature effect switched-off until SoilT improvements are done.
       - Recalibration of all cultivars using air temperature.
    - SUBSTOR
       - Added protection for leaf senescence factor due to temperature (SLFT).
       - Calibrated cultivar coefficients for Potato with GLUE HPC.
       
- **Miscellaneous**
    - Added protections for input extreme latitudes in FileX.
    - Updated and revised WARNING.OUT messages.
    - Checked for missing input codes for tillage, fertilzer, residue and chemical in FileX.
    - Reincorporated evapotranspiration parameters into the Species files.
    - Stopped model for undefined PET flag and update error checking for ASCE parameters.
    - Made ASCE ET robust for various weather file formats.
    - Removed static CMake install prefix for UNIX systems.
    - Updated start simulation date for 4-digit year to avoid stop the model.
    - Updated CO2 concentration to current August 4, 2023.
    - Reduced number of compile warnings.
    - Handled multiple fertilizers in one day correctly.
    - Removed WORK.OUT output.
    - Prevent negative NO3 and NH4 values in the FORAGE model.
    - Prevent total nitrogen uptake (TRNU) to avoid division by zero.
    - New Default directory under data for Simulation Control settings for Xbuild and XB2.
    - Weather variable definitions for Weatherman.
    - Updated error messages.
    - Updated variable definitions in DATA.CDE. 
    - BatchFiles in the Data directory. 
    - Updated CDE data files.
    - Replaced DATA.CDE with ASCII to UTF-8 file format.
    - Changed SweetCorn FreshWT header to match CROPGRO FreshWt header.
    - Removed unused Nitrogen Stress parameters read from plant input.
    - Cleaned up genotype files unused comments and updated files based on CROPGRO standard.
    - Added by-product harvested to MgmtEvent.OUT.
    

