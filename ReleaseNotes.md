# Release notes

v4.8.0.27, 2022-04-22
(previous release v4.7.5.8, 2019-04-18)

- **New Crop models**
    - Teff-Ceres (default teff model based on CERES-rice)
    - Teff-NWheat (alternate teff model based on NWheat)
    - Quinoa-CROPGRO
    - Chia-CROPGRO
    - Sugarcane-SAMUCA
    - Sunflower-OilCropSun (the CROPGRO sunflower model remains the default)
    
- **New Functionality**
    - Yield forecast application (Run mode = 'Y')
    - Methane emissions model
    - Greenhouse gas output file, GHG.OUT
    - Fertilizer characteristics are now specified in external file, FERCH048.SDA allowing user-specified fertilizer blends
    - ASCE Potential Evapotranspiration methods added for short and tall crop references
    - Soil Carbon balance (seasonal and daily options)

- **Plant model revisions**
    - MANIHOT (YCA) Cassava - Major revisions:
       - Include dry matter content dynamics in the storage roots allowing the reallocation of assimilates from the storage roots to aboveground growth. Dry matter              content is affected by temperature and soil water dynamics.
       - Modify cardinal temperatures for branching.
       - Updated photoperiod effect by branch level. 
       - Modify dynamics of leaf senescence due to water stress.
       - Hourly VPD effect in potential transpiration and photosynthesis.
       - Changes to the N dynamics with new concentrations in the different organs and organic matter composition (RESCH047.SDA file).
       - Modification in the cultivar and ecotype files: new parameters for dry matter content, leaf senescence, and photoperiod effect. 
       - Remove parameters no longer used in the species file. 
       - YCA Cassava genotype files version updated
    - ALOHA-Pineapple model - major revisions
    - NWheat APS model
       - Ozone effects added to NWheat model
       - Pest damage routines coupled to NWheat model
    - CROPGRO model
       - Added low-value corrections to leaf-level photosynthesis and energy balance routines.
       - Recalibration of many CROPGRO crops with revisions to leaf-level photosynthesis.    - Revisions to allow small seed crops allowed (chia, quinoa)
    - Perennial forage model
       - Add auto-mow feature

- **Miscellaneous**
    - Allow floating C:N ratio in CERES Soil Organic Matter Module
    - Fix initialization errors in soil temperature and soil water routines.
    - Default soil evaporation method is now Ritchie, EVAPO = "R"
    - Added "F" automatic planting option (force planting on last day of window)
    - Cultivar files now contain for GLUE parameter estimation tool
    - Summary.OUT file includes several additional variables including fresh weight, nitrogen mineralization, methane emissions, field location
    - Added 4 digits year for weather files
    - Support NASA format weather files
    - Improve platform portability
    

