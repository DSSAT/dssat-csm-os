# Release notes (in progress)

v4.8.0.27 2022-04-22
(previous release v4.7.5.8 2019-04-18)

1. **New Crop models**
    - Teff-Ceres
    - Teff-NWheat
    - Quinoa-CROPGRO
    - Chia-CROPGRO
    - Sugarcane-SAMUCA
    - Sunflower-OilCropSun

2. **Plant model revisions**
    - Major revisions to MANIHOT (YCA) Cassava model 
    - Ozone effects added to NWheat model
    - Pest damage routines coupled to NWheat model
    - Evapotranspiration protections for divisions by zero
    - Evapotranspiration new outputs for .csv files
    - Soil nitrogen outputs for urea
    - YCA Cassava genotype files version updated
    - Included flood and mulch evaporation in Summary output
    - Updated cultivar files for GLUE (Minima and Maxima)
    - Weather year, latitude, longitude and elevation in Summary output
    - Re-order Overview output
    - Soil Carbon balance

3. **Management**
    - Added "F" automatic planting option (force planting on last day of window)
    - Fertilizer characteristics specified in external file, FERCH048.SDA
    - New Forecast run modes

4. **Model function**
    - Accomodate long path names
    - Added 4 digits year for weather files
    - Fixed issues with long weather files
    - Prevent negative soil Nitrogen
    - Forecast application
    - Initialization issues
    - CROPGRO handle small seed crop
    - Updated .CDE files
    - Genotype files updated (Re-calibrated)
    - Added protections for several floating point calculations
    - Updated error messages (MODEL.ERR)
    - Added Methane model
    - Support NASA format weather files

Note: started at 13 Nov 2019 (beginning of OS repo) - April 17 2021


