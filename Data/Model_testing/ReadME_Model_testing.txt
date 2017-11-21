ReadMe_Model_testing.txt

2017-07-14, DSSAT Sprint @ IFDC

A perennial challenge in crop model development is to ensure that changes in code do not have unintended effects on other parts of the model software. While comparisons of "normal" scenarios as simulated by baseline and modified model can detect many problems, experience with DSSAT has been that subtle problems are overlooked until a model is tested under unusual circumstances.
With DSSAT we have established a set of the File-Xs that specify unusual conditions such as sowing out of normal cropping seasons or applying unrealistically high levels of nitrogen. While essentially a type of sensitivity analysis, 
This folder contains subfolders corresponding to different tests. In most cases one to four crops are considered.
For each test, three files are provided:
1. The file-X, typically named with four letters for a single factor (e.g., TEMP for temperature) or two letters each for two factors (NIIR for nitrogen inputs x irrigation levels).
2. The corresponding batch file that lists the treatments (e.g., NITM01MZ for nitrogen by temperature with maize as the crop).
3. A *.BAT that will run the simulations and copy the output to the subfolder Output_files provide. As the files are copied, the name of the source File-X is appended.
Note that the batch file and *.BAT files need to be edited for different model versions.
We welcome suggestions for additional types of comparison testing.  
