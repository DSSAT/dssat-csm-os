# DSSAT Biochar Extension

This extension adds a Biochar module to DSSAT to simulate the effects of biochar application on soil properties and nutrient dynamics.

## Features

1.  **Variable Application**: Supports multiple biochar applications over time.
2.  **Physical Effects**:
    *   Modifies Soil Bulk Density (BD).
    *   Modifies Soil Porosity (POROS).
    *   Modifies Water Retention Parameters (DUL, LL, SAT).
3.  **Chemical Effects**:
    *   Increases Cation Exchange Capacity (CEC) with aging.
    *   Modifies Soil pH (Liming effect).
    *   Increases Ammonium Adsorption Coefficient (ADCOEF).
4.  **Nutrient Release**:
    *   Simulates immediate release of K, P, and N from biochar ash.
    *   Adds these nutrients to the inorganic fertilizer pools (ADDSKi, ADDSPi, ADDSNH4).

## Input File: BIOCHAR.INP

The module reads from `BIOCHAR.INP` in the current directory.

### Format

```
*BIOCHAR APPLICATIONS
@DATE    AMOUNT  DEPTH   ASH    PH   CEC  C_PCT  TYPE
20200501 5000.0   20.0   5.0   9.0  20.0   75.0  WOOD
20210501 2000.0   20.0  10.0   8.0  15.0   60.0  MANURE
```

*   **DATE**: Application date (YYYYMMDD or YRDOY).
*   **AMOUNT**: Application rate (kg/ha).
*   **DEPTH**: Depth of incorporation (cm).
*   **ASH**: Ash content (%).
*   **PH**: pH of biochar.
*   **CEC**: Initial CEC of biochar (cmol/kg).
*   **C_PCT**: Carbon percentage (%).
*   **TYPE**: Description (up to 6 chars).

## Implementation Details

*   **Source Code**: `Soil/Biochar/Biochar_mod.f90`
*   **Integration**: `Soil/SOIL.for` calls `Biochar_Init` and `Biochar_Daily`.
*   **Build**: Added to `CMakeLists.txt`.

## Compilation

Re-run cmake and make to include the new module.
```bash
cd build
cmake ..
make
```
