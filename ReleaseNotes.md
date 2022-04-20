# Release notes (in progress)

v4.8.0.24 2022-03-11
(previous release v4.7.5.8 2019-04-18)

1. **New Crop models**
    - Ceres-Teff 
    - Quinoa
    - Samuca - sugarcane

2. **Plant model revisions**
    - Major revisions to MANIHOT (YCA) Cassava model:
       - Include dry matter content dynamics in the storage roots allowing the reallocation of assimilates from the storage roots to aboveground growth. Dry matter              content is affected by temperature and soil water dynamics.
       - Modify cardinal temperatures for branching.
       - Updated photoperiod effect by branch level. 
       - Modify dynamics of leaf senescence due to water stress.
       - Hourly VPD effect in potential transpiration and photosynthesis.
       - Changes to the N dynamics with new concentrations in the different organs and organic matter composition (RESCH047.SDA file).
       - Modification in the cultivar and ecotype files: new parameters for dry matter content, leaf senescence, and photoperiod effect. 
       - Remove parameters no longer used in the species file. 
 
    - Ozone effects added to NWheat model
    - Pest damage routines coupled to NWheat model

3. **Management**
    - added "F" automatic planting option (force planting on last day of window)
    - Fertilizer characteristics specified in external file, FERCH048.SDA

4. **Model function**
    - accomodate long path names
     

Note: started at 13 Nov 2019 (beginning of OS repo) ended at 21 Apr 2020

Still to do:
- need to add pre-os repo changes from v4.7.5.8 thru 4.7.5.13 (beginning of os repo)
- need to continue working forward in time from 21 Apr 2020 to present
- organize by general topic
- no need to mention minor changes



