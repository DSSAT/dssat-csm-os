---
# DSSAT/SAMUCA Release Notes:
## DSSAT Development Sprint Jan-2020
### Murilo Vianna (murilodsv@gmail.com)
---

The new version of SAMUCA model was incorporated into the DSSAT CSM.

Full description of the SAMUCA model can be found at:
- Standalone version: 	dx.doi.org/10.1590/S0103-90162014000100001
- Updated version:		Paper under review


---
## Crop Parameters:

Some parameters had to be adjusted to cope with the slightly different subroutines of DSSAT. 

As DSSAT consider the parameters SWCON1, SWCON2 and SWCON3 as constants, we increased the 
Specific Root Lengths parameters SRLMax and SRLMin at species files from 22 and 16 to 36 and 30 (m g-1), respectively,
to increase the RLD and, as a result, increase the root water uptake. Although this will make the model to overpredict
RLD (as tested for Laclau & Laclau, 2009), this was necessary to avoid changes into the CSM structure, specially at the
SPAM subroutine. If DSSAT turns to consider the parameters SWCON1, SWCON2 and SWCON3 as crop specific coefficients, 
the original parameters SRLMax and SRLMin should hold. Further, as the subroutine of soil temperature is slightly different
from that used in the simulation shell developed for SAMUCA v2, we increased the shoot elongation rate parameter from
0.04 cm Cday-1 to 0.068 cm Cday-1.

---

## Detailed Outputs:

Apart of PlantGro.OUT, user's of SAMUCA can get detailed outputs at phytomer level, canopy photosynthesis and crop abiotic stresses when the verbose option (VBOSE) is set to 'D'.

---

## Sample site:

A sample simulation for an experimental site at Piracicaba, Brazil, is provided in the file ESAL1401.SNX. Ancillary and biometric data collected throughout crop growth is also available for model testing and calibration. 
source: https://www.teses.usp.br/teses/disponiveis/11/11152/tde-01082018-150704/pt-br.php
