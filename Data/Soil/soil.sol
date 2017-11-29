*SOILS: General DSSAT Soil Input File
! DSSAT v4.7; 09/01/2017
!
! Standard Soil Profiles
!
! The following generic information was developed by A.J. Gijsman:
!
! - BD was estimated as BD = 100 / (SOM% / 0.224 + (100 - SOM%) / mineral BD)  
!   (Adams, 1973; Rawls and Brakensiek, 1985).
! - LL and DUL are according to Saxton et al., 1986.
! - SAT was taken as a fraction of porosity (Dalgliesh and Foale, 1998):
!   0.93 for soil classes S, SL and LS; 0.95 for soil classes L, SIL, SI,
!   SCL and SC; and 0.97 for soil classes C, CL, SIC and SICL.
!   For this, porosity was estimated as: POR = 1 - BD / APD (in which APD
!   is the adjusted particle density, i.e. corrected for SOM; Baumer and Rice, 1988).
! - The ranges of LL and DUL values were calculated by stepping through the
!   complete texture triangle in steps of 1% sand, 1% silt and 1% clay (>5000 
!   combinations), but with the texture limitations that Saxton set for his method
!   taken into consideration. For SAT, these limitations do not hold, as this was
!   based on POR and not on Saxton. See Gijsman et al., 2002.
! - The root growth distribution function SRGF was was calculated as:
!   SRGF = 1 * EXP(-0.02 * LAYER_CENTER); SRGF was set 1 for LAYER_BOTTOM <= 15.
!
! SOIL CLASS       BD                LL               DUL               SAT
! ========== =============     =============     =============     =============
!   C        1.129 - 1.512     0.220 - 0.346     0.330 - 0.467     0.413 - 0.488
!   CL       1.243 - 1.502     0.156 - 0.218     0.282 - 0.374     0.417 - 0.512
!   L        1.245 - 1.483     0.083 - 0.156     0.222 - 0.312     0.415 - 0.501
!   LS       1.353 - 1.629     0.059 - 0.110     0.137 - 0.185     0.355 - 0.416
!   S        1.446 - 1.574     0.055 - 0.085     0.123 - 0.158     0.374 - 0.400
!   SC       1.501 - 1.593     0.195 - 0.294     0.276 - 0.389     0.376 - 0.409
!   SCL      1.475 - 1.636     0.132 - 0.191     0.213 - 0.304     0.360 - 0.418
!   SI       0.978 - 1.464     0.096 - 0.099     0.299 - 0.307     0.442 - 0.488
!   SIC      1.307 - 1.446     0.224 - 0.326     0.379 - 0.456     0.455 - 0.489
!   SICL     1.248 - 1.464     0.155 - 0.219     0.324 - 0.392     0.448 - 0.511
!   SIL      0.968 - 1.464     0.082 - 0.152     0.240 - 0.333     0.439 - 0.547
!   SL       1.142 - 1.647     0.066 - 0.133     0.164 - 0.243     0.348 - 0.499
!
!======================================================================================================
! Start of Generic soil profiles
!======================================================================================================
!
! The 12 Generic soils for SOIL.SOL, as estimated by Arjan Gijsman:
!
! - LL, DUL are according to the Nearest Neighbor method (Jagtap et al, 2004)
! - Ksat at -99 
! - BD according to Gijsman et al (2002)
! - SAT based on the APSRU manual (Dalgliesh and Foale, 1998); i.e. 93-97% of porosity
!   depending on the soil type) in which porosity is according to Baumer and Rice (1988).
!
! References
! Adams W.A. 1973. The effect of organic matter on the bulk and true densities of some
!   uncultivated podzolic soils. J. Soil Science 24, 10-17.
! Baumer O.W. and Rice J.W. 1988. Methods to predict soil input data for DRAINMOD. 
!   Am. Soc. Agr. Eng. Paper 88-2564
! Dalgliesh, N.P., and M.A. Foale. 1998. Soil Matters � monitoring soil water and nitrogen
!   in dryland farming. CSIRO, Agricultural Production Systems Research Unit, 
!   Toowoomba, Queensland, Australia. 122 pp.
! Gijsman A.J., Jagtap S.S., Jones J.W. 2002. Wading through a swamp of complete confusion: 
!   how to choose a method for estimating soil water retention parameters for crop models. 
!   European Journal of Agronomy, 18: 75-105.
! Jagtap S.S., Lal U., Jones J.W., Gijsman A.J., Ritchie J.T. 2004. A dynamic nearest-neighbor
!   method for estimating soil water parameters. Transactions of ASAE 47: 1437-1444
! Rawls W.J. and Brakensiek D.L. 1985. Prediction of soil water properties for hydrologic
!   modeling. In: Jones, E.B. and Ward, T.J. (Eds.), Proc. Symp. Watershed Management
!   in the Eighties. April 30-May 1, 1985, Denver, CO. Am. Soc. Civil Eng., 
!   New York, NY. pp.293-299.
! Saxton K.E., Rawls W.J., Romberger J.S., Papendick R.I. 1986. Estimating generalized soil-water
!   characteristics from texture. Soil Sci. Soc. Am. J. 50, 1031-1036
!
!======================================================================================================

*IB00000001  IBSNAT      SIC     210 DEFAULT - DEEP SILTY CLAY
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Generic     Generic           -99    -99  Generic
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.11   6.0  0.30  85.0  1.00  1.00 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.228 0.385 0.481 1.000   -99  1.30  1.75  50.0  45.0   0.0 0.170   6.5   -99   -99   -99 
    15   -99 0.228 0.385 0.481 1.000   -99  1.30  1.75  50.0  45.0   0.0 0.170   6.5   -99   -99   -99 
    30   -99 0.249 0.406 0.482 0.638   -99  1.30  1.60  50.0  45.0   0.0 0.170   6.5   -99   -99   -99 
    45   -99 0.249 0.406 0.465 0.472   -99  1.35  1.45  50.0  45.0   0.0 0.140   6.5   -99   -99   -99 
    60   -99 0.249 0.406 0.465 0.350   -99  1.35  1.45  50.0  45.0   0.0 0.140   6.5   -99   -99   -99 
    90   -99 0.308 0.456 0.468 0.223   -99  1.35  1.10  50.0  45.0   0.0 0.110   6.5   -99   -99   -99 
   120   -99 0.207 0.341 0.452 0.122   -99  1.40  0.65  50.0  45.0   0.0 0.060   6.5   -99   -99   -99 
   150   -99 0.243 0.365 0.455 0.067   -99  1.40  0.30  50.0  45.0   0.0 0.030   6.5   -99   -99   -99 
   180   -99 0.259 0.361 0.457 0.037   -99  1.40  0.10  50.0  45.0   0.0 0.010   6.5   -99   -99   -99 
   210   -99 0.259 0.361 0.457 0.020   -99  1.40  0.01  50.0  45.0   0.0 0.000   6.5   -99   -99   -99 

*IB00000002  IBSNAT      SIC     150 DEFAULT - MEDIUM SILTY CLAY
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Generic     Generic           -99    -99  Generic
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.11   6.0  0.20  87.0  1.00  1.00 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.228 0.385 0.463 1.000   -99  1.35  1.74  50.0  45.0   0.0 0.170   6.5   -99   -99   -99 
    15   -99 0.228 0.385 0.463 1.000   -99  1.35  1.74  50.0  45.0   0.0 0.170   6.5   -99   -99   -99 
    30   -99 0.228 0.385 0.459 0.638   -99  1.36  1.66  50.0  45.0   0.0 0.170   6.5   -99   -99   -99 
    45   -99 0.249 0.406 0.461 0.472   -99  1.36  1.45  50.0  45.0   0.0 0.140   6.5   -99   -99   -99 
    60   -99 0.249 0.406 0.461 0.350   -99  1.36  1.45  50.0  45.0   0.0 0.140   6.5   -99   -99   -99 
    90   -99 0.308 0.449 0.460 0.223   -99  1.37  1.09  50.0  45.0   0.0 0.110   6.5   -99   -99   -99 
   120   -99 0.207 0.341 0.460 0.122   -99  1.38  0.65  50.0  45.0   0.0 0.060   6.5   -99   -99   -99 
   150   -99 0.256 0.373 0.463 0.067   -99  1.38  0.29  50.0  45.0   0.0 0.030   6.5   -99   -99   -99 

*IB00000003  IBSNAT      SIC      60 DEFAULT - SHALLOW SILTY CLAY
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Generic     Generic           -99    -99  Generic
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.11   6.0  0.10  89.0  1.00  1.00 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.228 0.385 0.463 1.000   -99  1.35  1.74  50.0  45.0   0.0 0.170   6.5   -99   -99   -99 
    15   -99 0.228 0.385 0.463 1.000   -99  1.35  1.74  50.0  45.0   0.0 0.170   6.5   -99   -99   -99 
    30   -99 0.228 0.385 0.459 0.638   -99  1.36  1.66  50.0  45.0   0.0 0.170   6.5   -99   -99   -99 
    45   -99 0.249 0.406 0.461 0.472   -99  1.36  1.45  50.0  45.0   0.0 0.140   6.5   -99   -99   -99 
    60   -99 0.249 0.406 0.461 0.350   -99  1.36  1.45  50.0  45.0   0.0 0.140   6.5   -99   -99   -99 

*IB00000004  IBSNAT      SIL     210 DEFAULT - DEEP SILTY LOAM
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Generic     Generic           -99    -99  Generic
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.12   6.0  0.40  77.0  1.00  1.00 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.110 0.227 0.450 1.000   -99  1.37  1.16  10.0  60.0   0.0 0.120   6.5   -99   -99   -99 
    15   -99 0.110 0.227 0.450 1.000   -99  1.37  1.16  10.0  60.0   0.0 0.120   6.5   -99   -99   -99 
    30   -99 0.103 0.201 0.451 0.638   -99  1.37  1.10  10.0  60.0   0.0 0.110   6.5   -99   -99   -99 
    45   -99 0.099 0.193 0.452 0.472   -99  1.37  0.97  10.0  60.0   0.0 0.100   6.5   -99   -99   -99 
    60   -99 0.099 0.193 0.452 0.350   -99  1.37  0.97  10.0  60.0   0.0 0.100   6.5   -99   -99   -99 
    90   -99 0.088 0.173 0.450 0.223   -99  1.38  0.72  10.0  60.0   0.0 0.070   6.5   -99   -99   -99 
   120   -99 0.079 0.165 0.452 0.122   -99  1.38  0.43  10.0  60.0   0.0 0.040   6.5   -99   -99   -99 
   150   -99 0.086 0.178 0.450 0.067   -99  1.39  0.20  10.0  60.0   0.0 0.020   6.5   -99   -99   -99 
   190   -99 0.072 0.174 0.451 0.033   -99  1.39  0.06  10.0  60.0   0.0 0.010   6.5   -99   -99   -99 
   210   -99 0.072 0.174 0.452 0.018   -99  1.39  0.01  10.0  60.0   0.0 0.000   6.5   -99   -99   -99 

*IB00000005  IBSNAT      SIL     150 DEFAULT - MEDIUM SILTY LOAM
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Generic     Generic           -99    -99  Generic
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.12   6.0  0.30  79.0  1.00  1.00 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.110 0.227 0.450 1.000   -99  1.37  1.16  10.0  60.0   0.0 0.120   6.5   -99   -99   -99 
    15   -99 0.110 0.227 0.450 1.000   -99  1.37  1.16  10.0  60.0   0.0 0.120   6.5   -99   -99   -99 
    30   -99 0.103 0.201 0.451 0.638   -99  1.37  1.10  10.0  60.0   0.0 0.110   6.5   -99   -99   -99 
    45   -99 0.099 0.193 0.452 0.472   -99  1.37  0.97  10.0  60.0   0.0 0.100   6.5   -99   -99   -99 
    60   -99 0.099 0.193 0.452 0.350   -99  1.37  0.97  10.0  60.0   0.0 0.100   6.5   -99   -99   -99 
    90   -99 0.088 0.173 0.450 0.223   -99  1.38  0.72  10.0  60.0   0.0 0.070   6.5   -99   -99   -99 
   120   -99 0.079 0.165 0.452 0.122   -99  1.38  0.43  10.0  60.0   0.0 0.040   6.5   -99   -99   -99 
   150   -99 0.086 0.178 0.450 0.067   -99  1.39  0.20  10.0  60.0   0.0 0.020   6.5   -99   -99   -99 

*IB00000006  IBSNAT      SIL      60 DEFAULT - SHALLOW SILTY LOAM
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Generic     Generic           -99    -99  Generic
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.12   6.0  0.20  81.0  1.00  1.00 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.110 0.227 0.450 1.000   -99  1.37  1.16  10.0  60.0   0.0 0.120   6.5   -99   -99   -99 
    15   -99 0.110 0.227 0.450 1.000   -99  1.37  1.16  10.0  60.0   0.0 0.120   6.5   -99   -99   -99 
    30   -99 0.103 0.201 0.451 0.638   -99  1.37  1.10  10.0  60.0   0.0 0.110   6.5   -99   -99   -99 
    45   -99 0.099 0.193 0.452 0.472   -99  1.37  0.97  10.0  60.0   0.0 0.100   6.5   -99   -99   -99 
    60   -99 0.099 0.193 0.452 0.350   -99  1.37  0.97  10.0  60.0   0.0 0.100   6.5   -99   -99   -99 

*IB00000007  IBSNAT      SL      210 DEFAULT - DEEP SANDY LOAM
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Generic     Generic           -99    -99  Generic
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.13   6.0  0.50  68.0  1.00  1.00 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.052 0.176 0.359 1.000   -99  1.61  0.70  10.0  30.0   0.0 0.070   6.5   -99   -99   -99 
    15   -99 0.052 0.176 0.359 1.000   -99  1.61  0.70  10.0  30.0   0.0 0.070   6.5   -99   -99   -99 
    30   -99 0.052 0.176 0.359 0.638   -99  1.61  0.66  10.0  30.0   0.0 0.070   6.5   -99   -99   -99 
    45   -99 0.073 0.192 0.360 0.472   -99  1.61  0.58  10.0  30.0   0.0 0.060   6.5   -99   -99   -99 
    60   -99 0.073 0.192 0.360 0.350   -99  1.61  0.58  10.0  30.0   0.0 0.060   6.5   -99   -99   -99 
    90   -99 0.128 0.232 0.361 0.223   -99  1.61  0.43  10.0  30.0   0.0 0.040   6.5   -99   -99   -99 
   120   -99 0.143 0.243 0.359 0.122   -99  1.62  0.26  10.0  30.0   0.0 0.030   6.5   -99   -99   -99 
   150   -99 0.138 0.243 0.360 0.067   -99  1.62  0.12  10.0  30.0   0.0 0.010   6.5   -99   -99   -99 
   180   -99 0.138 0.244 0.361 0.037   -99  1.62  0.04  10.0  30.0   0.0 0.000   6.5   -99   -99   -99 
   210   -99 0.138 0.244 0.361 0.020   -99  1.62  0.01  10.0  30.0   0.0 0.000   6.5   -99   -99   -99 

*IB00000008  IBSNAT      SL      150 DEFAULT - MEDIUM SANDY LOAM
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Generic     Generic           -99    -99  Generic
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.13   6.0  0.50  70.0  1.00  1.00 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.052 0.176 0.359 1.000   -99  1.61  0.70  10.0  30.0   0.0 0.070   6.5   -99   -99   -99 
    15   -99 0.052 0.176 0.359 1.000   -99  1.61  0.70  10.0  30.0   0.0 0.070   6.5   -99   -99   -99 
    30   -99 0.052 0.176 0.359 0.638   -99  1.61  0.66  10.0  30.0   0.0 0.070   6.5   -99   -99   -99 
    45   -99 0.073 0.192 0.360 0.472   -99  1.61  0.58  10.0  30.0   0.0 0.060   6.5   -99   -99   -99 
    60   -99 0.073 0.192 0.360 0.350   -99  1.61  0.58  10.0  30.0   0.0 0.060   6.5   -99   -99   -99 
    90   -99 0.128 0.232 0.361 0.223   -99  1.61  0.43  10.0  30.0   0.0 0.040   6.5   -99   -99   -99 
   120   -99 0.143 0.243 0.359 0.122   -99  1.62  0.26  10.0  30.0   0.0 0.030   6.5   -99   -99   -99 
   150   -99 0.138 0.243 0.360 0.067   -99  1.62  0.12  10.0  30.0   0.0 0.010   6.5   -99   -99   -99 

*IB00000009  IBSNAT      SL       60 DEFAULT - SHALLOW SANDY LOAM
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Generic     Generic           -99    -99  Generic
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.13   6.0  0.40  74.0  1.00  1.00 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.052 0.176 0.359 1.000   -99  1.61  0.70  10.0  30.0   0.0 0.070   6.5   -99   -99   -99 
    15   -99 0.052 0.176 0.359 1.000   -99  1.61  0.70  10.0  30.0   0.0 0.070   6.5   -99   -99   -99 
    30   -99 0.052 0.176 0.359 0.638   -99  1.61  0.66  10.0  30.0   0.0 0.070   6.5   -99   -99   -99 
    45   -99 0.073 0.192 0.360 0.472   -99  1.61  0.58  10.0  30.0   0.0 0.060   6.5   -99   -99   -99 
    60   -99 0.073 0.192 0.360 0.350   -99  1.61  0.58  10.0  30.0   0.0 0.060   6.5   -99   -99   -99 

*IB00000010  IBSNAT      S       210 DEFAULT - DEEP SAND
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Generic     Generic           -99    -99  Generic
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.15   4.0  0.60  65.0  1.00  1.00 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.025 0.096 0.345 1.000   -99  1.66  0.29   5.0   5.0   0.0 0.030   6.5   -99   -99   -99 
    15   -99 0.025 0.096 0.345 1.000   -99  1.66  0.29   5.0   5.0   0.0 0.030   6.5   -99   -99   -99 
    30   -99 0.023 0.097 0.345 0.638   -99  1.66  0.28   5.0   5.0   0.0 0.030   6.5   -99   -99   -99 
    45   -99 0.023 0.097 0.345 0.472   -99  1.66  0.24   5.0   5.0   0.0 0.020   6.5   -99   -99   -99 
    60   -99 0.023 0.097 0.345 0.350   -99  1.66  0.24   5.0   5.0   0.0 0.020   6.5   -99   -99   -99 
    90   -99 0.018 0.091 0.346 0.223   -99  1.66  0.18   5.0   5.0   0.0 0.020   6.5   -99   -99   -99 
   120   -99 0.020 0.095 0.346 0.122   -99  1.66  0.11   5.0   5.0   0.0 0.010   6.5   -99   -99   -99 
   150   -99 0.021 0.091 0.347 0.067   -99  1.66  0.05   5.0   5.0   0.0 0.000   6.5   -99   -99   -99 
   180   -99 0.021 0.091 0.347 0.037   -99  1.66  0.01   5.0   5.0   0.0 0.000   6.5   -99   -99   -99 
   210   -99 0.021 0.091 0.347 0.020   -99  1.66  0.00   5.0   5.0   0.0 0.000   6.5   -99   -99   -99 

*IB00000011  IBSNAT      S       150 DEFAULT - MEDIUM SAND
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Generic     Generic           -99    -99  Generic
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.15   4.0  0.50  70.0  1.00  1.00 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.025 0.096 0.345 1.000   -99  1.66  0.29   5.0   5.0   0.0 0.030   6.5   -99   -99   -99 
    15   -99 0.025 0.096 0.345 1.000   -99  1.66  0.29   5.0   5.0   0.0 0.030   6.5   -99   -99   -99 
    30   -99 0.023 0.097 0.345 0.638   -99  1.66  0.28   5.0   5.0   0.0 0.030   6.5   -99   -99   -99 
    45   -99 0.023 0.097 0.345 0.472   -99  1.66  0.24   5.0   5.0   0.0 0.020   6.5   -99   -99   -99 
    60   -99 0.023 0.097 0.345 0.350   -99  1.66  0.24   5.0   5.0   0.0 0.020   6.5   -99   -99   -99 
    90   -99 0.018 0.091 0.346 0.223   -99  1.66  0.18   5.0   5.0   0.0 0.020   6.5   -99   -99   -99 
   120   -99 0.020 0.095 0.346 0.122   -99  1.66  0.11   5.0   5.0   0.0 0.010   6.5   -99   -99   -99 
   150   -99 0.021 0.091 0.347 0.067   -99  1.66  0.05   5.0   5.0   0.0 0.000   6.5   -99   -99   -99 

*IB00000012  IBSNAT      S        60 DEFAULT - SHALLOW SAND
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Generic     Generic           -99    -99  Generic
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.15   4.0  0.40  75.0  1.00  1.00 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.025 0.096 0.345 1.000   -99  1.66  0.29   5.0   5.0   0.0 0.030   6.5   -99   -99   -99 
    15   -99 0.025 0.096 0.345 1.000   -99  1.66  0.29   5.0   5.0   0.0 0.030   6.5   -99   -99   -99 
    30   -99 0.023 0.097 0.345 0.638   -99  1.66  0.28   5.0   5.0   0.0 0.030   6.5   -99   -99   -99 
    45   -99 0.023 0.097 0.345 0.472   -99  1.66  0.24   5.0   5.0   0.0 0.020   6.5   -99   -99   -99 
    60   -99 0.023 0.097 0.345 0.350   -99  1.66  0.24   5.0   5.0   0.0 0.020   6.5   -99   -99   -99 
!======================================================================================================
! End of Generic soil profiles
!======================================================================================================

*IBSB910015  SCS         -99     180 Millhopper Fine Sand
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Gainesville USA            29.630 -82.370 Loamy,silic,hyperth Gross. Paleudults
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.18   5.0  0.50  66.0  1.00  0.92 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.023 0.086 0.230 1.000  7.40  1.36  0.90   0.9  11.8   -99   -99   5.3   -99   -99   -99 
    15   -99 0.023 0.086 0.230 1.000  7.40  1.40  0.69   0.9  11.8   -99   -99   5.4   -99   -99   -99 
    30   -99 0.023 0.086 0.230 0.498 15.80  1.46  0.28   4.6   6.4   -99   -99   5.7   -99   -99   -99 
    45   -99 0.023 0.086 0.230 0.294 28.00  1.46  0.20   5.8   5.4   -99   -99   5.8   -99   -99   -99 
    60   -99 0.023 0.086 0.230 0.294 28.00  1.47  0.20   5.8   5.4   -99   -99   5.8   -99   -99   -99 
    90   -99 0.021 0.076 0.230 0.380 27.60  1.43  0.09   9.6   4.2   -99   -99   5.9   -99   -99   -99 
   120   -99 0.020 0.076 0.230 0.133 17.50  1.48  0.03   9.6   4.2   -99   -99   5.9   -99   -99   -99 
   150   -99 0.027 0.130 0.230 0.062  0.30  1.57  0.03   8.3   3.6   -99   -99   5.9   -99   -99   -99 
   180   -99 0.070 0.258 0.360 0.031  0.10  1.79  0.03   8.3   3.6   -99   -99   5.9   -99   -99   -99 

*IBSB910009  IBSNAT      -99     136 Norfolk Sandy Clay Loam (
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 -99         USA               -99    -99  Fine loamy,silic.,therm. Typic Paleudults
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.14   3.0  0.23  60.0  1.00  0.95 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.042 0.169 0.392 1.000   -99  0.00  0.00   -99   -99   -99   -99   -99   -99   -99   -99 
    15   -99 0.042 0.169 0.392 1.000   -99  0.00  0.00   -99   -99   -99   -99   -99   -99   -99   -99 
    25   -99 0.042 0.169 0.392 0.779   -99  0.00  0.00   -99   -99   -99   -99   -99   -99   -99   -99 
    33   -99 0.044 0.177 0.358 0.349   -99  0.00  0.00   -99   -99   -99   -99   -99   -99   -99   -99 
    46   -99 0.056 0.165 0.396 0.209   -99  0.00  0.00   -99   -99   -99   -99   -99   -99   -99   -99 
    61   -99 0.150 0.291 0.377 0.070   -99  0.00  0.00   -99   -99   -99   -99   -99   -99   -99   -99 
    76   -99 0.150 0.291 0.377 0.070   -99  0.00  0.00   -99   -99   -99   -99   -99   -99   -99   -99 
   106   -99 0.150 0.291 0.377 0.017   -99  0.00  0.00   -99   -99   -99   -99   -99   -99   -99   -99 
   136   -99 0.150 0.291 0.377 0.000   -99  0.00  0.00   -99   -99   -99   -99   -99   -99   -99   -99 

*IBSB910017  SCS         -99     203 Orangeburg Sandy Loam
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Quincy      USA            30.600 -86.400 Loamy,silic,hyperth Gross. Paleud(17)
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.13   9.0  0.27  84.0  1.00  0.97 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.125 0.198 0.294 1.000   -99  1.49  1.73   -99   -99   -99   -99   -99   -99   -99   -99 
    15   -99 0.125 0.198 0.294 0.874   -99  1.49  1.73   -99   -99   -99   -99   -99   -99   -99   -99 
    25   -99 0.125 0.198 0.294 0.874   -99  1.49  1.73   -99   -99   -99   -99   -99   -99   -99   -99 
    34   -99 0.117 0.226 0.323 0.351   -99  1.41  0.40   -99   -99   -99   -99   -99   -99   -99   -99 
    43   -99 0.117 0.226 0.323 0.351   -99  1.41  0.40   -99   -99   -99   -99   -99   -99   -99   -99 
    53   -99 0.138 0.250 0.332 0.310   -99  1.44  0.20   -99   -99   -99   -99   -99   -99   -99   -99 
    64   -99 0.138 0.250 0.332 0.310   -99  1.44  0.20   -99   -99   -99   -99   -99   -99   -99   -99 
   102   -99 0.167 0.281 0.331 0.302   -99  1.57  0.14   -99   -99   -99   -99   -99   -99   -99   -99 
   145   -99 0.182 0.291 0.334 0.077   -99  1.59  0.16   -99   -99   -99   -99   -99   -99   -99   -99 
   175   -99 0.162 0.272 0.320 0.036   -99  1.61  0.09   -99   -99   -99   -99   -99   -99   -99   -99 
   203   -99 0.154 0.263 0.319 0.006   -99  1.58  0.03   -99   -99   -99   -99   -99   -99   -99   -99 

*IBSB910026  SCS         -99     180 Ida Silt Loam
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Castana     USA            42.200 -93.700 Ida Silt Loam
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.12   6.0  0.30  60.0  1.00  1.00 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.135 0.290 0.485 1.000   -99  0.00  0.00   -99   -99   -99   -99   -99   -99   -99   -99 
    15   -99 0.135 0.290 0.485 1.000   -99  0.00  0.00   -99   -99   -99   -99   -99   -99   -99   -99 
    30   -99 0.135 0.290 0.485 0.175   -99  0.00  0.00   -99   -99   -99   -99   -99   -99   -99   -99 
    45   -99 0.106 0.228 0.514 0.138   -99  0.00  0.00   -99   -99   -99   -99   -99   -99   -99   -99 
    60   -99 0.106 0.228 0.514 0.138   -99  0.00  0.00   -99   -99   -99   -99   -99   -99   -99   -99 
    90   -99 0.105 0.254 0.517 0.188   -99  0.00  0.00   -99   -99   -99   -99   -99   -99   -99   -99 
   120   -99 0.133 0.290 0.507 0.250   -99  0.00  0.00   -99   -99   -99   -99   -99   -99   -99   -99 
   150   -99 0.108 0.283 0.505 0.213   -99  0.00  0.00   -99   -99   -99   -99   -99   -99   -99   -99 
   180   -99 0.108 0.291 0.542 0.100   -99  0.00  0.00   -99   -99   -99   -99   -99   -99   -99   -99 

*IBSB910027  Castana,mKJ SIL     225 Ida Silt Loam
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 -99         -99               -99    -99  Ida Silt Loam
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.12   9.8  0.55  60.0  1.00  1.00 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.098 0.260 0.485 1.000 14.50  1.29  0.00  14.1  51.9   -99   -99   8.1   1.0   -99   -99 
    15   -99 0.098 0.260 0.485 1.000 14.50  1.29  0.00  14.1  51.9   -99   -99   8.1   1.0   -99   -99 
    45   -99 0.098 0.248 0.500 0.257 16.00  1.24  0.00  11.5  53.3   -99   -99   8.1   1.0   -99   -99 
    75   -99 0.093 0.230 0.516 0.263 23.10  1.22  0.00  10.2  57.7   -99   -99   8.3   1.0   -99   -99 
   105   -99 0.095 0.235 0.512 0.300 23.10  1.23  0.00  13.5  50.9   -99   -99   8.3   1.0   -99   -99 
   135   -99 0.098 0.235 0.506 0.300 16.00  1.23  0.00   8.4  55.4   -99   -99   8.4   1.0   -99   -99 
   165   -99 0.088 0.223 0.524 0.300  9.70  1.24  0.00  10.6  51.0   -99   -99   8.4   1.0   -99   -99 
   195   -99 0.080 0.220 0.551 0.300  8.20  1.25  0.00  10.3  53.1   -99   -99   8.3   1.0   -99   -99 
   225   -99 0.083 0.223 0.547 0.300  8.70  1.25  0.00   9.6  52.8   -99   -99   8.3   0.2   -99   -99 
!SOIL FROM MIGUEL CALMON, MODIFIED BY KJB, SLIGHTLY LOWER LL, NEW WRL

*IBSB910055  Troup SCS   -99     190 TROUP FINE SAND
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Jackson CO  USA               -99    -99  TROUP, SCS JACKSON CO, GROSSARENIC PALEUDULT, L,
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.13   6.2  0.60  76.0  1.00  0.92 SA001 SA001 SA001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.033 0.105 0.314 1.000   -99  1.56  0.64   -99   -99   -99   -99   5.8   -99   -99   -99 
    15   -99 0.033 0.105 0.314 1.000   -99  1.56  0.64   -99   -99   -99   -99   5.8   -99   -99   -99 
    30   -99 0.031 0.099 0.313 0.927   -99  1.56  0.19   -99   -99   -99   -99   5.9   -99   -99   -99 
    45   -99 0.031 0.099 0.313 0.229   -99  1.56  0.19   -99   -99   -99   -99   5.9   -99   -99   -99 
    60   -99 0.031 0.099 0.313 0.060   -99  1.56  0.19   -99   -99   -99   -99   5.9   -99   -99   -99 
    90   -99 0.029 0.092 0.315 0.016   -99  1.60  0.05   -99   -99   -99   -99   5.4   -99   -99   -99 
   120   -99 0.029 0.092 0.315 0.011   -99  1.60  0.05   -99   -99   -99   -99   5.4   -99   -99   -99 
   145   -99 0.030 0.096 0.316 0.006   -99  1.60  0.04   -99   -99   -99   -99   5.1   -99   -99   -99 
   167   -99 0.047 0.151 0.357 0.000   -99  1.56  0.10   -99   -99   -99   -99   5.2   -99   -99   -99 
   190   -99 0.047 0.151 0.357 0.000   -99  1.56  0.10   -99   -99   -99   -99   5.2   -99   -99   -99 

*IBPN910015  SCS         -99     180 Millhopper Fine Sand
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Gainesville USA            29.630 -82.370 Loamy,silic,hyperth Gross. Paleudults
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.18   5.0  0.50  66.0  1.00  0.92 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.023 0.086 0.230 1.000  7.40  1.36  0.90   0.9  11.8   -99   -99   -99   -99   -99   -99 
    15   -99 0.023 0.086 0.230 1.000  7.40  1.40  0.69   0.9  11.8   -99   -99   -99   -99   -99   -99 
    30   -99 0.023 0.086 0.230 0.550 15.80  1.46  0.28   4.6   6.4   -99   -99   -99   -99   -99   -99 
    45   -99 0.023 0.086 0.230 0.320 28.00  1.46  0.20   5.8   5.4   -99   -99   -99   -99   -99   -99 
    60   -99 0.023 0.086 0.230 0.320 28.00  1.47  0.20   5.8   5.4   -99   -99   -99   -99   -99   -99 
    90   -99 0.021 0.076 0.230 0.380 27.60  1.43  0.09   9.6   4.2   -99   -99   -99   -99   -99   -99 
   120   -99 0.020 0.076 0.230 0.400 17.50  1.48  0.03   9.6   4.2   -99   -99   -99   -99   -99   -99 
   150   -99 0.027 0.130 0.230 0.300  0.30  1.57  0.03   8.3   3.6   -99   -99   -99   -99   -99   -99 
   180   -99 0.070 0.258 0.360 0.200  0.10  1.79  0.03   8.3   3.6   -99   -99   -99   -99   -99   -99 

*IBPN910016  Gainesville -99     180 Lake Fine Sand
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 -99         -99               -99    -99  Hyperthermic, coated Typic Quartzipsamments
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.18   5.0  0.50  66.0  1.00  0.92 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.020 0.089 0.230 1.000   -99  0.00  0.90   -99   -99   -99   -99   -99   -99   -99   -99 
    15   -99 0.020 0.089 0.230 1.000   -99  0.00  0.69   -99   -99   -99   -99   -99   -99   -99   -99 
    30   -99 0.019 0.068 0.230 0.550   -99  0.00  0.28   -99   -99   -99   -99   -99   -99   -99   -99 
    45   -99 0.026 0.075 0.230 0.320   -99  0.00  0.28   -99   -99   -99   -99   -99   -99   -99   -99 
    60   -99 0.026 0.075 0.230 0.320   -99  0.00  0.20   -99   -99   -99   -99   -99   -99   -99   -99 
    90   -99 0.025 0.073 0.230 0.380   -99  0.00  0.09   -99   -99   -99   -99   -99   -99   -99   -99 
   120   -99 0.022 0.069 0.230 0.400   -99  0.00  0.03   -99   -99   -99   -99   -99   -99   -99   -99 
   150   -99 0.023 0.072 0.230 0.300   -99  0.00  0.03   -99   -99   -99   -99   -99   -99   -99   -99 
   180   -99 0.035 0.085 0.230 0.200   -99  0.00  0.03   -99   -99   -99   -99   -99   -99   -99   -99 

*IBPN910024  Marianna, F -99     229 Norfolk Sandy Loam
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Marianna,FL USA               -99    -99  F-loamy,silic,thermic Typ Paleudults
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.18   6.0  0.10  77.0  1.00  0.92 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.061 0.145 0.312 1.000   -99  1.38  1.29   -99   -99   -99   -99   5.5   -99   -99   -99 
    10   -99 0.061 0.145 0.312 1.000   -99  1.38  1.29   -99   -99   -99   -99   5.5   -99   -99   -99 
    20   -99 0.050 0.141 0.302 0.775   -99  1.42  0.47   -99   -99   -99   -99   5.5   -99   -99   -99 
    38   -99 0.056 0.165 0.270 0.448   -99  1.52  0.28   -99   -99   -99   -99   5.5   -99   -99   -99 
    58   -99 0.198 0.304 0.359 0.300   -99  1.48  0.25   -99   -99   -99   -99   5.1   -99   -99   -99 
    79   -99 0.198 0.304 0.359 0.300   -99  1.48  0.25   -99   -99   -99   -99   5.1   -99   -99   -99 
    95   -99 0.197 0.305 0.335 0.100   -99  1.64  0.12   -99   -99   -99   -99   5.1   -99   -99   -99 
   112   -99 0.197 0.305 0.335 0.100   -99  1.64  0.12   -99   -99   -99   -99   5.1   -99   -99   -99 
   129   -99 0.184 0.292 0.332 0.100   -99  1.61  0.06   -99   -99   -99   -99   5.0   -99   -99   -99 
   147   -99 0.184 0.292 0.332 0.100   -99  1.61  0.06   -99   -99   -99   -99   5.0   -99   -99   -99 
   173   -99 0.210 0.318 0.339 0.020   -99  1.67  0.05   -99   -99   -99   -99   5.0   -99   -99   -99 
   201   -99 0.227 0.335 0.350 0.000   -99  1.66  0.06   -99   -99   -99   -99   4.9   -99   -99   -99 
   229   -99 0.227 0.335 0.350 0.000   -99  1.66  0.06   -99   -99   -99   -99   4.9   -99   -99   -99 

*IBPN910025  NRCS-USDA   L       173 Norfolk Sandy Loam
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Mobile      US             30.920 -88.034 Unclassified
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
    BN  0.13   6.0  0.60  61.0  1.00  0.92 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    25    Ap 0.101 0.249 0.498 1.000  1.32  1.24  1.16   8.6  41.5   -99   -99   6.0   -99   7.9   -99 
    66   Bt1 0.173 0.303 0.439 0.403  0.23  1.42  0.30  27.8  36.9   -99   -99   6.5   -99   8.6   -99 
    91   Bt2 0.203 0.329 0.444 0.208  0.23  1.41  0.13  34.7  35.3   -99   -99   4.7   -99  12.1   -99 
   127   Bt3 0.212 0.333 0.437 0.113  0.23  1.43  0.07  36.8  33.0   -99   -99   4.9   -99  13.9   -99 
   142   Bt4 0.214 0.339 0.444 0.068  0.23  1.41  0.13  36.8  34.1   -99   -99   4.9   -99  13.6   -99 
   173   Bt5 0.205 0.316 0.422 0.043  0.23  1.47  0.07  35.5  28.1   -99   -99   4.7   -99  12.8   -99 

*IBPN910040  GreenAcres, -99     180 Millhopper Fine Sand
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 -99         -99               -99    -99  Loamy,silic,hyperth Gross. Paleudults
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.18   5.0  0.50  66.0  1.00  0.90 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.023 0.086 0.230 1.000  7.40  1.36  0.90   -99   -99   -99   -99   -99   -99   -99   -99 
    15   -99 0.023 0.086 0.230 1.000  7.40  1.40  0.69   -99   -99   -99   -99   -99   -99   -99   -99 
    30   -99 0.023 0.086 0.230 0.550 15.80  1.46  0.28   -99   -99   -99   -99   -99   -99   -99   -99 
    45   -99 0.023 0.086 0.230 0.320 28.00  1.46  0.28   -99   -99   -99   -99   -99   -99   -99   -99 
    60   -99 0.023 0.086 0.230 0.320 28.00  1.47  0.20   -99   -99   -99   -99   -99   -99   -99   -99 
    90   -99 0.021 0.076 0.230 0.380 27.60  1.43  0.09   -99   -99   -99   -99   -99   -99   -99   -99 
   120   -99 0.020 0.076 0.230 0.400 17.50  1.48  0.03   -99   -99   -99   -99   -99   -99   -99   -99 
   150   -99 0.027 0.130 0.230 0.300  0.30  1.57  0.03   -99   -99   -99   -99   -99   -99   -99   -99 
   180   -99 0.070 0.258 0.360 0.200  0.10  1.79  0.03   -99   -99   -99   -99   -99   -99   -99   -99 

*IBMZ910013  IBSNAT      -99     110 Waipio
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Waipio,HI   USA               -99    -99  Clayey, kaolinitic, isohyperth, Tropeptic Eutrusto
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.14   5.0  0.60  60.0  1.00  1.00 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.220 0.350 0.550 1.000   -99  1.00  2.27   -99   -99   -99   -99   6.3   -99   -99   -99 
    15   -99 0.230 0.350 0.550 1.000   -99  1.00  2.27   -99   -99   -99   -99   6.3   -99   -99   -99 
    30   -99 0.240 0.350 0.550 0.800   -99  1.05  1.10   -99   -99   -99   -99   5.8   -99   -99   -99 
    50   -99 0.250 0.370 0.480 0.400   -99  1.17  1.41   -99   -99   -99   -99   5.8   -99   -99   -99 
    70   -99 0.260 0.380 0.460 0.200   -99  1.22  0.59   -99   -99   -99   -99   6.0   -99   -99   -99 
    90   -99 0.250 0.380 0.460 0.050   -99  1.22  0.36   -99   -99   -99   -99   6.0   -99   -99   -99 
   110   -99 0.260 0.400 0.480 0.020   -99  1.17  0.27   -99   -99   -99   -99   6.0   -99   -99   -99 
!WDB Original DSSAT35

*IBMZ913514  Gainesville -99     180 Millhopper Fine Sand
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Gainesville USA            29.630 -82.370 Loamy,silic,hyperth Arenic Paleudult
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.18   2.0  0.65  60.0  1.00  0.80 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.026 0.096 0.230 1.000   -99  1.30  2.00   -99   -99   -99   -99   -99   -99  20.0   -99 
    15   -99 0.025 0.086 0.230 1.000   -99  1.30  1.00   -99   -99   -99   -99   -99   -99   -99   -99 
    30   -99 0.025 0.086 0.230 0.800   -99  1.40  1.00   -99   -99   -99   -99   -99   -99   -99   -99 
    60   -99 0.025 0.086 0.230 0.200   -99  1.40  0.50   -99   -99   -99   -99   -99   -99   -99   -99 
    90   -99 0.028 0.090 0.230 0.100   -99  1.45  0.10   -99   -99   -99   -99   -99   -99   -99   -99 
   120   -99 0.028 0.090 0.230 0.050   -99  1.45  0.10   -99   -99   -99   -99   -99   -99   -99   -99 
   150   -99 0.029 0.130 0.230 0.002   -99  1.45  0.04   -99   -99   -99   -99   -99   -99   -99   -99 
   180   -99 0.070 0.258 0.360 0.000   -99  1.20  0.24   -99   -99   -99   -99   -99   -99   -99   -99 
!WDB Modified for DSSAT40

*IBMZ910014  Gainesville -99     180 Millhopper Fine Sand
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Gainesville USA            29.630 -82.370 Loamy,silic,hyperth Arenic Paleudult
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.18   2.0  0.65  60.0  1.00  0.92 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.026 0.096 0.230 1.000   -99  1.30  2.00   -99   -99   -99   -99   -99   -99  20.0   -99 
    15   -99 0.025 0.086 0.230 1.000   -99  1.30  1.00   -99   -99   -99   -99   -99   -99   -99   -99 
    30   -99 0.025 0.086 0.230 0.700   -99  1.40  1.00   -99   -99   -99   -99   -99   -99   -99   -99 
    60   -99 0.025 0.086 0.230 0.300   -99  1.40  0.50   -99   -99   -99   -99   -99   -99   -99   -99 
    90   -99 0.028 0.090 0.230 0.050   -99  1.45  0.10   -99   -99   -99   -99   -99   -99   -99   -99 
   120   -99 0.028 0.090 0.230 0.030   -99  1.45  0.10   -99   -99   -99   -99   -99   -99   -99   -99 
   150   -99 0.029 0.130 0.230 0.002   -99  1.45  0.04   -99   -99   -99   -99   -99   -99   -99   -99 
   180   -99 0.070 0.258 0.360 0.000   -99  1.20  0.24   -99   -99   -99   -99   -99   -99   -99   -99 
!GH For Maize Envirotron experiment (SLPF=0.72) 

*IBMZ910114  Gainesville -99     180 Millhopper Fine Sand
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Gainesville USA            29.630 -82.370 Loamy,silic,hyperth Arenic Paleudult
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.18   2.0  0.65  60.0  1.00  0.72 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.026 0.096 0.230 1.000   -99  1.30  2.00   -99   -99   -99   -99   -99   -99  20.0   -99 
    15   -99 0.025 0.086 0.230 1.000   -99  1.30  1.00   -99   -99   -99   -99   -99   -99   -99   -99 
    30   -99 0.025 0.086 0.230 0.700   -99  1.40  1.00   -99   -99   -99   -99   -99   -99   -99   -99 
    60   -99 0.025 0.086 0.230 0.300   -99  1.40  0.50   -99   -99   -99   -99   -99   -99   -99   -99 
    90   -99 0.028 0.090 0.230 0.050   -99  1.45  0.10   -99   -99   -99   -99   -99   -99   -99   -99 
   120   -99 0.028 0.090 0.230 0.030   -99  1.45  0.10   -99   -99   -99   -99   -99   -99   -99   -99 
   150   -99 0.029 0.130 0.230 0.002   -99  1.45  0.04   -99   -99   -99   -99   -99   -99   -99   -99 
   180   -99 0.070 0.258 0.360 0.000   -99  1.20  0.24   -99   -99   -99   -99   -99   -99   -99   -99 

*IBMZ910023  IBSNAT      -99     151 Norfolk Loamy Sand
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Florence,SC USA               -99    -99  Norfolk Loamy Sand
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.14   5.0  0.60  60.0  1.00  1.00 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    10   -99 0.075 0.210 0.250 1.000   -99  1.55  0.30   -99   -99   -99   -99   -99   -99   -99   -99 
    20   -99 0.075 0.210 0.250 1.000   -99  1.55  0.30   -99   -99   -99   -99   -99   -99   -99   -99 
    41   -99 0.100 0.240 0.290 0.800   -99  1.67  0.17   -99   -99   -99   -99   -99   -99   -99   -99 
    71   -99 0.210 0.310 0.350 0.400   -99  1.54  0.01   -99   -99   -99   -99   -99   -99   -99   -99 
   101   -99 0.210 0.320 0.360 0.100   -99  1.54  0.01   -99   -99   -99   -99   -99   -99   -99   -99 
   126   -99 0.180 0.280 0.320 0.100   -99  1.68  0.01   -99   -99   -99   -99   -99   -99   -99   -99 
   151   -99 0.180 0.280 0.320 0.100   -99  1.74  0.01   -99   -99   -99   -99   -99   -99   -99   -99 

*IBMZ910032  87EBDRL     -99     120 Clayey, Oxidic, Isothermic Haplustox
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 -99         -99               -99    -99  QUINTANA
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.13   7.1  0.50  76.0  1.00  1.00 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    15   -99 0.165 0.338 0.417 1.000   -99  1.00  1.16   -99   -99   -99   -99   5.0   -99   -99   -99 
    30   -99 0.219 0.339 0.378 0.697   -99  0.94  0.43   -99   -99   -99   -99   5.0   -99   -99   -99 
    45   -99 0.219 0.320 0.396 0.303   -99  0.90  0.31   -99   -99   -99   -99   5.0   -99   -99   -99 
    60   -99 0.200 0.310 0.417 0.515   -99  0.89  0.18   -99   -99   -99   -99   5.0   -99   -99   -99 
    75   -99 0.195 0.303 0.436 0.242   -99  0.93  0.24   -99   -99   -99   -99   5.0   -99   -99   -99 
    90   -99 0.195 0.305 0.436 0.182   -99  0.97  0.24   -99   -99   -99   -99   5.0   -99   -99   -99 
   105   -99 0.200 0.309 0.436 0.061   -99  0.89  0.27   -99   -99   -99   -99   5.0   -99   -99   -99 
   120   -99 0.205 0.309 0.436 0.030   -99  0.90  0.20   -99   -99   -99   -99   5.0   -99   -99   -99 

*IBWH980018  SCS         -99     180 Haynie
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Manhattan   USA               -99    -99  Coarse-silty, mixed,calcareous,mesic Typ Udifluven
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.14   5.0  0.60  60.0  1.00  1.00 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    15   -99 0.072 0.225 0.275 1.000   -99  1.15  0.61   -99   -99   -99   -99   -99   -99   -99   -99 
    30   -99 0.070 0.240 0.290 0.700   -99  1.16  0.61   -99   -99   -99   -99   -99   -99   -99   -99 
    60   -99 0.040 0.154 0.194 0.200   -99  1.21  0.59   -99   -99   -99   -99   -99   -99   -99   -99 
    90   -99 0.032 0.091 0.141 0.050   -99  1.23  0.29   -99   -99   -99   -99   -99   -99   -99   -99 
   120   -99 0.032 0.087 0.137 0.030   -99  1.31  0.24   -99   -99   -99   -99   -99   -99   -99   -99 
   150   -99 0.032 0.087 0.137 0.010   -99  1.31  0.20   -99   -99   -99   -99   -99   -99   -99   -99 
   180   -99 0.032 0.087 0.137 0.010   -99  1.31  0.20   -99   -99   -99   -99   -99   -99   -99   -99 

*IBWH980019  Swift       -99     150 Wood Mountain Loam
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Swift_Curr  Canada            -99    -99  Orthic Brown Chernozem
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.12   8.0  0.50  60.0  1.00  1.00 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.096 0.230 0.440 1.000   -99  0.00  1.10   -99   -99   -99   -99   -99   -99   -99   -99 
    15   -99 0.096 0.230 0.440 1.000   -99  0.00  1.10   -99   -99   -99   -99   -99   -99   -99   -99 
    30   -99 0.112 0.250 0.440 1.000   -99  0.00  0.61   -99   -99   -99   -99   -99   -99   -99   -99 
    45   -99 0.094 0.220 0.440 0.800   -99  0.00  0.61   -99   -99   -99   -99   -99   -99   -99   -99 
    60   -99 0.103 0.220 0.440 0.700   -99  0.00  0.59   -99   -99   -99   -99   -99   -99   -99   -99 
    75   -99 0.103 0.220 0.440 0.600   -99  0.00  0.15   -99   -99   -99   -99   -99   -99   -99   -99 
    90   -99 0.102 0.250 0.440 0.400   -99  0.00  0.10   -99   -99   -99   -99   -99   -99   -99   -99 
   120   -99 0.102 0.250 0.440 0.300   -99  0.00  0.10   -99   -99   -99   -99   -99   -99   -99   -99 
   150   -99 0.102 0.250 0.440 0.200   -99  0.00  0.10   -99   -99   -99   -99   -99   -99   -99   -99 

*IBWH980020  IBSNAT      -99     155 Rothamsted
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Rothamsted  England           -99    -99  Rothamsted
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.14   6.0  0.50  60.0  1.00  1.00 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    10   -99 0.110 0.280 0.330 1.000   -99  1.10  1.16   -99   -99   -99   -99   -99   -99   -99   -99 
    25   -99 0.150 0.320 0.420 0.900   -99  1.20  1.00   -99   -99   -99   -99   -99   -99   -99   -99 
    45   -99 0.220 0.370 0.420 0.700   -99  1.25  0.68   -99   -99   -99   -99   -99   -99   -99   -99 
    65   -99 0.220 0.370 0.420 0.500   -99  1.25  0.26   -99   -99   -99   -99   -99   -99   -99   -99 
    95   -99 0.220 0.370 0.420 0.200   -99  1.25  0.25   -99   -99   -99   -99   -99   -99   -99   -99 
   125   -99 0.220 0.370 0.420 0.100   -99  1.25  0.20   -99   -99   -99   -99   -99   -99   -99   -99 
   155   -99 0.220 0.370 0.420 0.050   -99  1.25  0.20   -99   -99   -99   -99   -99   -99   -99   -99 

*IBBN910015  SCS         -99     180 Millhopper Fine Sand
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Gainesville USA            29.630 -82.370 Loamy,silic,hyperth Gross. Paleudults
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.18   5.0  0.50  66.0  1.00  0.92 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.023 0.086 0.230 1.000  7.40  1.47  0.90   0.9  11.8   -99   -99   5.3   -99   -99   -99 
    15   -99 0.023 0.086 0.230 1.000  7.40  1.47  0.69   0.9  11.8   -99   -99   5.4   -99   -99   -99 
    30   -99 0.023 0.086 0.230 0.900 15.80  1.41  0.28   4.6   6.4   -99   -99   5.7   -99   -99   -99 
    45   -99 0.023 0.086 0.230 0.700 28.00  1.43  0.20   5.8   5.4   -99   -99   5.8   -99   -99   -99 
    60   -99 0.023 0.086 0.230 0.500 28.00  1.43  0.20   5.8   5.4   -99   -99   5.8   -99   -99   -99 
    90   -99 0.021 0.076 0.230 0.200 27.60  1.52  0.09   9.6   4.2   -99   -99   5.9   -99   -99   -99 
   120   -99 0.020 0.076 0.230 0.100 17.50  1.52  0.03   9.6   4.2   -99   -99   5.9   -99   -99   -99 
   150   -99 0.027 0.130 0.230 0.050  0.30  1.46  0.03   8.3   3.6   -99   -99   5.9   -99   -99   -99 
   180   -99 0.070 0.258 0.360 0.000  0.10  1.46  0.03   8.3   3.6   -99   -99   5.9   -99   -99   -99 

*IBBN910016  SCS         -99     180 Millhoper Fine Sand, W-2, Kendrick clay closer to surface
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Gainesville USA            29.630 -82.370 Loamy,silic,hyperth Gross. Paleudults
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.18   5.0  0.50  66.0  1.00  0.90 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.023 0.086 0.230 1.000  7.40  1.47  0.90   0.9  11.8   -99   -99   5.3   -99   -99   -99 
    15   -99 0.023 0.086 0.230 1.000  7.40  1.47  0.69   0.9  11.8   -99   -99   5.4   -99   -99   -99 
    30   -99 0.023 0.086 0.230 0.900 15.80  1.41  0.28   4.6   6.4   -99   -99   5.7   -99   -99   -99 
    45   -99 0.023 0.086 0.230 0.700 28.00  1.43  0.20   5.8   5.4   -99   -99   5.8   -99   -99   -99 
    60   -99 0.023 0.086 0.230 0.500 28.00  1.43  0.20   5.8   5.4   -99   -99   5.8   -99   -99   -99 
    90   -99 0.027 0.130 0.230 0.200 27.60  1.52  0.09   9.6   4.2   -99   -99   5.9   -99   -99   -99 
   120   -99 0.027 0.130 0.230 0.100 17.50  1.52  0.03   9.6   4.2   -99   -99   5.9   -99   -99   -99 
   150   -99 0.027 0.130 0.230 0.050  0.30  1.46  0.03   8.3   3.6   -99   -99   5.9   -99   -99   -99 
   180   -99 0.070 0.258 0.360 0.000  0.10  1.46  0.03   8.3   3.6   -99   -99   5.9   -99   -99   -99    
  
*IBBN910030  CIAT        -99     209 CIAT-Plot M3
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Palmira(M3) Colombia        3.480  73.370 Fine-silty,mixed,isohyperth.Aquic Hapludoll
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.09  11.0  0.40  84.0  1.00  1.00 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.204 0.340 0.392 1.000   -99  1.45  2.19   -99   -99   -99   -99   6.9   -99   -99   -99 
    15   -99 0.204 0.340 0.392 1.000   -99  1.45  2.19   -99   -99   -99   -99   6.9   -99   -99   -99 
    25   -99 0.209 0.345 0.390 0.750   -99  1.45  1.21   -99   -99   -99   -99   7.2   -99   -99   -99 
    35   -99 0.209 0.345 0.390 0.500   -99  1.45  1.21   -99   -99   -99   -99   7.2   -99   -99   -99 
    50   -99 0.198 0.335 0.390 0.350   -99  1.49  0.53   -99   -99   -99   -99   8.0   -99   -99   -99 
    65   -99 0.185 0.323 0.395 0.200   -99  1.58  0.20   -99   -99   -99   -99   8.2   -99   -99   -99 
    80   -99 0.185 0.323 0.395 0.150   -99  1.58  0.20   -99   -99   -99   -99   8.2   -99   -99   -99 
    99   -99 0.201 0.328 0.408 0.100   -99  1.54  0.10   -99   -99   -99   -99   8.1   -99   -99   -99 
   122   -99 0.198 0.325 0.410 0.050   -99  1.58  0.09   -99   -99   -99   -99   8.2   -99   -99   -99 
   137   -99 0.159 0.288 0.399 0.000   -99  1.50  0.09   -99   -99   -99   -99   8.3   -99   -99   -99 
   159   -99 0.110 0.242 0.402 0.000   -99  1.69  0.10   -99   -99   -99   -99   8.3   -99   -99   -99 
   184   -99 0.047 0.177 0.351 0.000   -99  1.59  0.08   -99   -99   -99   -99   8.0   -99   -99   -99 
   209   -99 0.050 0.193 0.410 0.000   -99  1.45  0.12   -99   -99   -99   -99   8.5   -99   -99   -99 

*IBBN910038  ICTA        -99     150 San_Fernando,Quezada
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 S_Fernando  Guatemala         -99    -99  LOAMY-SKELETAL,MIXED,ISOHYPERTHERMIC TYPIC USTIFLU
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.13  11.3  0.40  80.0  1.00  0.80 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.191 0.299 0.369 1.000   -99  1.27  2.25   -99   -99   -99   -99   7.0   -99   -99   -99 
    15   -99 0.191 0.299 0.369 1.000   -99  1.27  2.25   -99   -99   -99   -99   7.0   -99   -99   -99 
    45   -99 0.160 0.273 0.352 0.750   -99  1.11  0.87   -99   -99   -99   -99   6.7   -99   -99   -99 
    72   -99 0.244 0.361 0.376 0.500   -99  1.30  1.20   -99   -99   -99   -99   6.7   -99   -99   -99 
    95   -99 0.170 0.285 0.355 0.350   -99  1.13  0.67   -99   -99   -99   -99   6.9   -99   -99   -99 
   120   -99 0.095 0.207 0.332 0.200   -99  1.02  0.36   -99   -99   -99   -99   7.0   -99   -99   -99 
   150   -99 0.163 0.266 0.363 0.150   -99  1.28  0.60   -99   -99   -99   -99   7.1   -99   -99   -99 

*GAPN930001  SCS         SL      178 Faceville
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Plains,GA   USA            32.000 -84.330 CLAYEY, KAOLINITIC THERMIC, TYPIC PALEUDULTS
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
    BN  0.13   9.3  0.60  76.0  1.00  1.00 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    18    AP 0.092 0.189 0.327 1.000   -99  1.45  0.97  16.1  12.8  22.0   -99   6.5   -99   2.8   -99 
    28    B1 0.129 0.238 0.335 0.750   -99  1.61  0.54  22.0  11.6   6.0   -99   5.7   -99   3.1   -99 
    61    B2 0.180 0.293 0.347 0.750   -99  1.52  0.24  33.1  14.9   3.0   -99   5.9   -99   3.9   -99 
    76    B2 0.192 0.307 0.351 0.750   -99  1.48  0.27  35.6  17.7   2.0   -99   5.7   -99   4.4   -99 
   114    B2 0.245 0.364 0.379 0.350   -99  1.39  0.15  47.6  24.9   2.0   -99   5.1   -99   6.4   -99 
   152    B2 0.283 0.400 0.415 0.200   -99  1.38  0.19  55.6  18.9   0.0   -99   4.6   -99  15.0   -99 
   178    B3 0.336 0.455 0.470 0.150   -99  1.26  0.11  67.6  21.2   0.0   -99   4.6   -99  36.8   -99 

*IBRI910001  IBSNAT      -99      50 Andaqueptic Haplaquoll
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 -99         -99               -99    -99  UNKNOWN
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.13   7.5  0.00  87.0  1.00  1.00 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.289 0.415 0.700 1.000   -99  0.75  2.45   -99   -99   -99   -99   6.2   -99  40.0   -99 
    20   -99 0.289 0.415 0.650 0.850   -99  0.90  2.45   -99   -99   -99   -99   6.2   -99   -99   -99 
    35   -99 0.289 0.415 0.600 0.200   -99  0.90  1.45   -99   -99   -99   -99   6.4   -99   -99   -99 
    50   -99 0.289 0.415 0.600 0.050   -99  0.88  1.45   -99   -99   -99   -99   6.4   -99   -99   -99 

*IBRI910002  IBSNAT      -99      50 Vertic Tropaquept
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 -99         -99               -99    -99  UNKNOWN
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.13   7.5  0.00  87.0  1.50  1.00 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.258 0.389 0.650 1.000   -99  0.85  1.30   -99   -99   -99   -99   -99   -99  40.0   -99 
    20   -99 0.258 0.389 0.600 0.850   -99  0.90  1.20   -99   -99   -99   -99   -99   -99   -99   -99 
    35   -99 0.267 0.396 0.550 0.200   -99  0.90  0.65   -99   -99   -99   -99   -99   -99   -99   -99 
    50   -99 0.267 0.396 0.550 0.050   -99  0.90  0.50   -99   -99   -99   -99   -99   -99   -99   -99 

*IBRI910023  IRRI        -99     135 Upland Soil
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 -99         -99               -99    -99  FINE, MIXED,ISOHYPERTHERMIC, ANDAQUEPTIC HAPLAQUOL
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.13  12.0  0.60  67.0  0.90  1.00 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.280 0.397 0.412 1.000   -99  1.00  2.45   -99   -99   -99   -99   6.0   -99  25.0   -99 
    15   -99 0.280 0.397 0.412 1.000   -99  1.00  2.45   -99   -99   -99   -99   6.0   -99   -99   -99 
    29   -99 0.275 0.392 0.407 1.000   -99  1.00  1.48   -99   -99   -99   -99   6.5   -99   -99   -99 
    38   -99 0.198 0.264 0.412 0.200   -99  0.93  0.52   -99   -99   -99   -99   7.0   -99   -99   -99 
    47   -99 0.198 0.264 0.412 0.200   -99  0.93  0.52   -99   -99   -99   -99   7.0   -99   -99   -99 
    58   -99 0.174 0.235 0.373 0.100   -99  0.78  0.31   -99   -99   -99   -99   6.9   -99   -99   -99 
    69   -99 0.174 0.235 0.373 0.050   -99  0.78  0.31   -99   -99   -99   -99   6.9   -99   -99   -99 
    96   -99 0.152 0.213 0.366 0.000   -99  0.74  0.25   -99   -99   -99   -99   7.0   -99   -99   -99 
   123   -99 0.152 0.213 0.366 0.000   -99  0.74  0.25   -99   -99   -99   -99   7.0   -99   -99   -99 
   135   -99 0.172 0.238 0.364 0.000   -99  0.55  0.06   -99   -99   -99   -99   7.0   -99   -99   -99 

*IBRI910024  IBSNAT      -99      51 Suphan Lowland
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 -99         -99               -99    -99  Fine, Mixed, Non-acid, Isohyper., Aeric Tropaquept
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.10   7.5  0.00  60.0  1.00  1.00 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.245 0.332 0.398 1.000   -99  1.41  1.81   -99   -99   -99   -99   -99   -99   5.0   -99 
     8   -99 0.245 0.332 0.398 1.000   -99  1.41  1.81   -99   -99   -99   -99   -99   -99   -99   -99 
    19   -99 0.210 0.341 0.402 0.361   -99  1.49  0.79   -99   -99   -99   -99   -99   -99   -99   -99 
    28   -99 0.242 0.369 0.404 0.200   -99  1.39  0.54   -99   -99   -99   -99   -99   -99   -99   -99 
    38   -99 0.242 0.369 0.404 0.100   -99  1.39  0.54   -99   -99   -99   -99   -99   -99   -99   -99 
    51   -99 0.218 0.344 0.388 0.100   -99  1.34  0.32   -99   -99   -99   -99   -99   -99   -99   -99 

*IBWM860001  AWAII       CL      120 Kawaihapai Gravelly Clay Loam
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 -99         USA            -9.000  -9.000 FINE,MIXED,ISOHYPERHERMIC CUMULIC HAPLUSTOLLS
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
 BROWN  0.13  12.0  0.60  90.0  1.00  1.00 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    20   Ap1 0.211 0.327 0.366 1.000   -99  1.30  1.00  33.0  33.0   -99   -99   6.7   -99   -99   -99 
    35   Ap2 0.173 0.289 0.370 1.000   -99  1.30  0.90  25.0  36.0   -99   -99   6.6   -99   -99   -99 
    50    C1 0.170 0.274 0.369 1.000   -99  1.30  0.20  26.0   6.0   -99   -99   7.1   -99   -99   -99 
    60    C2 0.177 0.277 0.440 1.000   -99  1.20  0.60  26.0   6.0   -99   -99   6.7   -99   -99   -99 
    95    C3 0.169 0.276 0.488 1.000   -99  1.10  0.30  26.0   6.0   -99   -99   6.8   -99   -99   -99 
   120    C4 0.178 0.279 0.535 1.000   -99  1.00  0.80  26.0   6.0   -99   -99   7.1   -99   -99   -99 

*CCPA000030  Veltkamp    -99     209 Silty Clay Loam
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Palmira     Colombia        3.520  76.350 Fine-silty, mixed, isohyperth. Aquic Hapludoll
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.09  11.0  0.40  84.0  1.00  1.00 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.204 0.340 0.392 1.000   -99  1.45  2.19   -99   -99   -99   -99   6.9   -99   -99   -99 
    15   -99 0.204 0.340 0.392 1.000   -99  1.45  2.19   -99   -99   -99   -99   6.9   -99   -99   -99 
    25   -99 0.209 0.345 0.390 0.750   -99  1.45  1.21   -99   -99   -99   -99   7.2   -99   -99   -99 
    35   -99 0.209 0.345 0.390 0.500   -99  1.45  1.21   -99   -99   -99   -99   7.2   -99   -99   -99 
    50   -99 0.198 0.335 0.390 0.350   -99  1.49  0.53   -99   -99   -99   -99   8.0   -99   -99   -99 
    65   -99 0.185 0.323 0.395 0.200   -99  1.58  0.20   -99   -99   -99   -99   8.2   -99   -99   -99 
    80   -99 0.185 0.323 0.395 0.150   -99  1.58  0.20   -99   -99   -99   -99   8.2   -99   -99   -99 
    99   -99 0.201 0.328 0.408 0.100   -99  1.54  0.10   -99   -99   -99   -99   8.1   -99   -99   -99 
   122   -99 0.198 0.325 0.410 0.050   -99  1.58  0.09   -99   -99   -99   -99   8.2   -99   -99   -99 
   137   -99 0.159 0.288 0.399 0.000   -99  1.50  0.09   -99   -99   -99   -99   8.3   -99   -99   -99 
   159   -99 0.110 0.242 0.402 0.000   -99  1.69  0.10   -99   -99   -99   -99   8.3   -99   -99   -99 
   184   -99 0.047 0.177 0.351 0.000   -99  1.59  0.08   -99   -99   -99   -99   8.0   -99   -99   -99 
   209   -99 0.050 0.193 0.410 0.000   -99  1.45  0.12   -99   -99   -99   -99   8.5   -99   -99   -99 

*CCQU000033  Connor      -99     260 Clay
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Quilichao   Colombia        3.100  76.510 Fine,Kaolinitic,Isohyperth. Oxic Dystropept
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.09   6.8  0.60  76.0  1.00  1.00 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.351 0.470 0.485 1.000   -99  1.17  2.90   -99   -99   -99   -99   -99   -99   -99   -99 
    15   -99 0.354 0.473 0.488 1.000   -99  1.21  2.91   -99   -99   -99   -99   -99   -99   -99   -99 
    28   -99 0.374 0.491 0.506 0.750   -99  1.19  2.45   -99   -99   -99   -99   -99   -99   -99   -99 
    44   -99 0.395 0.510 0.525 0.750   -99  1.13  1.48   -99   -99   -99   -99   -99   -99   -99   -99 
    65   -99 0.415 0.527 0.542 0.500   -99  1.07  0.76   -99   -99   -99   -99   -99   -99   -99   -99 
    96   -99 0.427 0.536 0.551 0.500   -99  1.08  0.36   -99   -99   -99   -99   -99   -99   -99   -99 
   122   -99 0.437 0.549 0.564 0.250   -99  1.01  0.23   -99   -99   -99   -99   -99   -99   -99   -99 
   150   -99 0.441 0.553 0.568 0.250   -99  1.00  0.15   -99   -99   -99   -99   -99   -99   -99   -99 
   178   -99 0.428 0.541 0.556 0.125   -99  0.99  0.14   -99   -99   -99   -99   -99   -99   -99   -99 
   196   -99 0.371 0.488 0.503 0.125   -99  1.10  0.08   -99   -99   -99   -99   -99   -99   -99   -99 
   230   -99 0.371 0.488 0.503 0.050   -99  1.10  0.08   -99   -99   -99   -99   -99   -99   -99   -99 
   260   -99 0.371 0.488 0.503 0.050   -99  1.10  0.08   -99   -99   -99   -99   -99   -99   -99   -99 

*IBML910001  IBSNAT      -99     210 Niger Sandy Soil
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 -99         Niger             -99    -99  NIGER SANDY SOIL
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.14  10.0  0.80  76.0  0.35  0.70 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.031 0.097 0.320 1.000   -99  1.61  0.50   -99   -99   -99   -99   5.5   -99   -99   -99 
    15   -99 0.031 0.097 0.320 1.000   -99  1.61  0.50   -99   -99   -99   -99   5.5   -99   -99   -99 
    30   -99 0.037 0.119 0.331 0.800   -99  1.66  0.50   -99   -99   -99   -99   5.5   -99   -99   -99 
    45   -99 0.037 0.117 0.332 0.600   -99  1.54  0.50   -99   -99   -99   -99   5.5   -99   -99   -99 
    60   -99 0.037 0.117 0.332 0.400   -99  1.54  0.50   -99   -99   -99   -99   5.5   -99   -99   -99 
    90   -99 0.040 0.126 0.331 0.010   -99  1.57  0.50   -99   -99   -99   -99   5.5   -99   -99   -99 
   120   -99 0.037 0.117 0.331 0.010   -99  1.55  0.50   -99   -99   -99   -99   5.5   -99   -99   -99 
   150   -99 0.037 0.119 0.330 0.010   -99  1.64  0.50   -99   -99   -99   -99   5.5   -99   -99   -99 
   180   -99 0.037 0.119 0.331 0.010   -99  1.58  0.50   -99   -99   -99   -99   5.5   -99   -99   -99 
   210   -99 0.034 0.108 0.324 0.010   -99  1.60  0.50   -99   -99   -99   -99   5.5   -99   -99   -99 

*IBML910083  IBSNAT      -99     156 Medium Alfisol
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 -99         -99               -99    -99  MEDIUM ALFISOL
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.13   6.0  0.50  60.0  1.00  1.00 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    22   -99 0.060 0.180 0.240 1.000   -99  1.51  0.70   -99   -99   -99   -99   6.5   -99   -99   -99 
    52   -99 0.085 0.195 0.255 0.700   -99  1.51  0.66   -99   -99   -99   -99   6.5   -99   -99   -99 
    82   -99 0.150 0.210 0.270 0.300   -99  1.51  0.58   -99   -99   -99   -99   6.5   -99   -99   -99 
   112   -99 0.160 0.200 0.260 0.100   -99  1.51  0.43   -99   -99   -99   -99   6.5   -99   -99   -99 
   127   -99 0.173 0.200 0.260 0.020   -99  1.52  0.26   -99   -99   -99   -99   6.5   -99   -99   -99 
   156   -99 0.193 0.200 0.260 0.010   -99  1.52  0.12   -99   -99   -99   -99   6.5   -99   -99   -99 

*IBSG910010  IBSNAT      -99     210 Miller Clay Loam
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 College STN USA               -99    -99  MILLER CLAY LOAM
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.13   7.0  0.28  78.0  1.00  0.00 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    10   -99 0.140 0.270 0.330 1.000   -99   -99   -99   -99   -99   -99   -99   -99   -99   -99   -99 
    30   -99 0.140 0.270 0.330 0.700   -99   -99   -99   -99   -99   -99   -99   -99   -99   -99   -99 
    60   -99 0.140 0.270 0.330 0.500   -99   -99   -99   -99   -99   -99   -99   -99   -99   -99   -99 
    90   -99 0.140 0.270 0.330 0.300   -99   -99   -99   -99   -99   -99   -99   -99   -99   -99   -99 
   120   -99 0.140 0.270 0.330 0.100   -99   -99   -99   -99   -99   -99   -99   -99   -99   -99   -99 
   150   -99 0.140 0.270 0.330 0.050   -99   -99   -99   -99   -99   -99   -99   -99   -99   -99   -99 
   180   -99 0.140 0.270 0.330 0.050   -99   -99   -99   -99   -99   -99   -99   -99   -99   -99   -99 
   210   -99 0.140 0.270 0.330 0.025   -99   -99   -99   -99   -99   -99   -99   -99   -99   -99   -99 

*IBSG910011  IBSNAT      -99     130 Weakly self-mulching grey medium to heavy clay
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 -99         -99               -99    -99  HEAVY CLAY
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.13   6.0  0.50  60.0  1.00  0.80  IB001 IB001 IB001
!   -99  0.13   6.0  0.50  60.0  0.00  0.90 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.154 0.231 0.414 1.000   -99  1.10  2.00   -99   -99   -99   -99   -99   -99   -99   -99 
    20   -99 0.153 0.228 0.290 0.900   -99  1.30  1.00   -99   -99   -99   -99   -99   -99   -99   -99 
    30   -99 0.164 0.231 0.273 0.800   -99  1.40  0.90   -99   -99   -99   -99   -99   -99   -99   -99 
    40   -99 0.166 0.228 0.277 0.700   -99  1.50  0.80   -99   -99   -99   -99   -99   -99   -99   -99 
    50   -99 0.164 0.241 0.296 0.600   -99  1.50  0.60   -99   -99   -99   -99   -99   -99   -99   -99 
    70   -99 0.169 0.236 0.285 0.500   -99  1.50  0.50   -99   -99   -99   -99   -99   -99   -99   -99 
    90   -99 0.172 0.241 0.284 0.400   -99  1.50  0.30   -99   -99   -99   -99   -99   -99   -99   -99 
   110   -99 0.180 0.256 0.308 0.200   -99  1.50  0.30   -99   -99   -99   -99   -99   -99   -99   -99 
   130   -99 0.180 0.256 0.308 0.050   -99  1.60  0.20   -99   -99   -99   -99   -99   -99   -99   -99 

*IBSG910085  IBSNAT      -99     172 Patencheru
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Patancheru  India             -99    -99  ALFISOL Udic Rhodustalf, Patencheru Series
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.13   6.0  0.30  80.0  1.00  1.00 IB001 IB001 IB001
!   -99  0.13   6.0  0.30  80.0  1.00  1.10 IB001 IB001 IB001   GH/2011 
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    10   -99 0.080 0.220 0.310 1.000   -99  1.61  0.85   -99   -99   -99   -99   -99   -99   -99   -99 
    22   -99 0.090 0.220 0.310 0.900   -99  1.61  0.55   -99   -99   -99   -99   -99   -99   -99   -99 
    52   -99 0.125 0.245 0.315 0.700   -99  1.62  0.51   -99   -99   -99   -99   -99   -99   -99   -99 
    82   -99 0.150 0.230 0.290 0.500   -99  1.64  0.63   -99   -99   -99   -99   -99   -99   -99   -99 
   112   -99 0.160 0.240 0.310 0.100   -99  1.64  0.10   -99   -99   -99   -99   -99   -99   -99   -99 
   142   -99 0.173 0.240 0.310 0.050   -99  1.64  0.10   -99   -99   -99   -99   -99   -99   -99   -99 
   172   -99 0.193 0.250 0.320 0.030   -99  1.64  0.10   -99   -99   -99   -99   -99   -99   -99   -99 

*IBSG910086  IBSNAT      -99     202 Kasareddipalli
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 -99         -99               -99    -99  VERTISOL,Typic Pellustert ,Kasareddipalli Series
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.11   6.0  0.25  85.0  1.00  1.00 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    10   -99 0.270 0.407 0.467 1.000   -99  1.35  0.96   -99   -99   -99   -99   -99   -99   -99   -99 
    22   -99 0.270 0.407 0.467 0.900   -99  1.35  0.96   -99   -99   -99   -99   -99   -99   -99   -99 
    52   -99 0.270 0.436 0.500 0.700   -99  1.36  0.69   -99   -99   -99   -99   -99   -99   -99   -99 
    82   -99 0.290 0.440 0.500 0.300   -99  1.36  0.68   -99   -99   -99   -99   -99   -99   -99   -99 
   112   -99 0.307 0.440 0.500 0.100   -99  1.36  0.60   -99   -99   -99   -99   -99   -99   -99   -99 
   142   -99 0.323 0.440 0.500 0.020   -99  1.36  0.60   -99   -99   -99   -99   -99   -99   -99   -99 
   172   -99 0.362 0.440 0.490 0.010   -99  1.36  0.60   -99   -99   -99   -99   -99   -99   -99   -99 
   202   -99 0.362 0.430 0.490 0.010   -99  1.36  0.60   -99   -99   -99   -99   -99   -99   -99   -99 

*IBSG910092  IBSNAT      -99      53 Patencheru
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 -99         -99               -99    -99  ALFISOL Udic Rhodustalf, Patencheru Series
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.13   6.0  0.50  60.0  1.00  1.00 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    10   -99 0.060 0.180 0.240 1.000   -99  1.61  0.55   -99   -99   -99   -99   -99   -99   -99   -99 
    22   -99 0.060 0.180 0.240 0.900   -99  1.61  0.55   -99   -99   -99   -99   -99   -99   -99   -99 
    37   -99 0.085 0.195 0.255 0.700   -99  1.62  0.51   -99   -99   -99   -99   -99   -99   -99   -99 
    53   -99 0.150 0.210 0.270 0.300   -99  1.64  0.63   -99   -99   -99   -99   -99   -99   -99   -99 

*IBSG910093  IBSNAT      -99     135 Tindall Clay Loam
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 -99         -99               -99    -99  TINDALL CLAY LOAM, TIPPERA SERIES
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.13   6.0  0.40  74.0  1.00  1.00 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.190 0.289 0.306 1.000   -99  1.57  1.00   -99   -99   -99   -99   6.9   -99   -99   -99 
    15   -99 0.218 0.289 0.306 0.800   -99  1.57  0.52   -99   -99   -99   -99   6.9   -99   -99   -99 
    35   -99 0.232 0.328 0.338 0.400   -99  1.46  0.63   -99   -99   -99   -99   6.9   -99   -99   -99 
    55   -99 0.235 0.340 0.361 0.050   -99  1.40  0.52   -99   -99   -99   -99   6.4   -99   -99   -99 
    75   -99 0.233 0.329 0.369 0.005   -99  1.38  0.40   -99   -99   -99   -99   6.5   -99   -99   -99 
    95   -99 0.234 0.326 0.376 0.002   -99  1.39  0.10   -99   -99   -99   -99   6.5   -99   -99   -99 
   115   -99 0.239 0.341 0.384 0.001   -99  1.42  0.10   -99   -99   -99   -99   6.5   -99   -99   -99 
   135   -99 0.251 0.343 0.393 0.001   -99  1.47  0.10   -99   -99   -99   -99   6.5   -99   -99   -99 

*IBSG910096  IBSNAT      -99     100 Medium Black Calcarious Soil
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Pune        India             -99    -99  VERTISOL
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.13  11.6  0.20  85.0  1.00  1.00 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.202 0.345 0.443 1.000   -99  0.92  0.51   -99   -99   -99   -99   8.4   -99   -99   -99 
    10   -99 0.202 0.345 0.443 0.905   -99  0.92  0.51   -99   -99   -99   -99   8.4   -99   -99   -99 
    25   -99 0.204 0.346 0.433 0.705   -99  1.01  0.50   -99   -99   -99   -99   8.4   -99   -99   -99 
    40   -99 0.210 0.359 0.415 0.522   -99  1.08  0.50   -99   -99   -99   -99   8.4   -99   -99   -99 
    55   -99 0.210 0.411 0.420 0.387   -99  1.07  0.41   -99   -99   -99   -99   8.4   -99   -99   -99 
    70   -99 0.191 0.425 0.433 0.287   -99  1.10  0.39   -99   -99   -99   -99   8.4   -99   -99   -99 
    85   -99 0.191 0.363 0.401 0.212   -99  1.13  0.39   -99   -99   -99   -99   8.4   -99   -99   -99 
   100   -99 0.191 0.363 0.401 0.212   -99  1.13  0.39   -99   -99   -99   -99   8.4   -99   -99   -99 

*IBBA980008  IBSNAT      -99     105 Typic Calciorthid
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Breda       Syria          35.209  40.000 Typic Calciorthid
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.14   6.0  0.50  72.0  1.00  1.00 SA001 SA001 SA001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.170 0.290 0.460 1.000   -99  1.10  0.63   -99   -99   -99   -99   -99   -99   -99   -99 
    15   -99 0.190 0.310 0.460 1.000   -99  1.10  0.63   -99   -99   -99   -99   -99   -99   -99   -99 
    30   -99 0.190 0.310 0.460 1.000   -99  1.20  0.50   -99   -99   -99   -99   -99   -99   -99   -99 
    45   -99 0.220 0.330 0.460 0.750   -99  1.20  0.30   -99   -99   -99   -99   -99   -99   -99   -99 
    60   -99 0.220 0.330 0.460 0.650   -99  1.20  0.25   -99   -99   -99   -99   -99   -99   -99   -99 
    75   -99 0.220 0.330 0.460 0.450   -99  1.20  0.19   -99   -99   -99   -99   -99   -99   -99   -99 
    90   -99 0.220 0.330 0.460 0.350   -99  1.20  0.13   -99   -99   -99   -99   -99   -99   -99   -99 
   105   -99 0.220 0.330 0.460 0.100   -99  1.20  0.13   -99   -99   -99   -99   -99   -99   -99   -99 

*IBBA980060  IBSNAT      -99     180 Bozeman
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Bozeman     USA               -99    -99  Typic Haploboroll
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.12  12.0  0.60  60.0  1.00  1.00 SA001 SA001 SA001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    10   -99 0.280 0.410 0.490 1.000   -99  1.30  1.16   -99   -99   -99   -99   -99   -99   -99   -99 
    30   -99 0.280 0.410 0.490 0.700   -99  1.30  1.00   -99   -99   -99   -99   -99   -99   -99   -99 
    60   -99 0.280 0.410 0.490 0.300   -99  1.30  0.91   -99   -99   -99   -99   -99   -99   -99   -99 
    90   -99 0.280 0.410 0.490 0.100   -99  1.30  0.61   -99   -99   -99   -99   -99   -99   -99   -99 
   120   -99 0.280 0.410 0.490 0.050   -99  1.30  0.59   -99   -99   -99   -99   -99   -99   -99   -99 
   150   -99 0.280 0.410 0.490 0.025   -99  1.30  0.29   -99   -99   -99   -99   -99   -99   -99   -99 
   180   -99 0.280 0.410 0.490 0.005   -99  1.30  0.24   -99   -99   -99   -99   -99   -99   -99   -99 

*IBBA910009  IBSNAT      -99     180 Palexerollic Chromoxerert
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Jindiress   Syria             -99    -99  Palexerollic Chromoxerert
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.09   5.0  0.30  60.0  1.00  1.00 SA001 SA001 SA001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.230 0.360 0.440 0.800   -99  1.30  0.65   -99   -99   -99   -99   -99   -99   -99   -99 
    15   -99 0.230 0.360 0.440 1.000   -99  1.30  0.64   -99   -99   -99   -99   -99   -99   -99   -99 
    30   -99 0.210 0.340 0.420 0.850   -99  1.30  0.50   -99   -99   -99   -99   -99   -99   -99   -99 
    45   -99 0.210 0.340 0.420 0.720   -99  1.30  0.43   -99   -99   -99   -99   -99   -99   -99   -99 
    60   -99 0.210 0.340 0.420 0.550   -99  1.30  0.37   -99   -99   -99   -99   -99   -99   -99   -99 
    75   -99 0.210 0.340 0.420 0.400   -99  1.30  0.35   -99   -99   -99   -99   -99   -99   -99   -99 
    90   -99 0.220 0.350 0.430 0.280   -99  1.30  0.33   -99   -99   -99   -99   -99   -99   -99   -99 
   105   -99 0.220 0.350 0.430 0.150   -99  1.30  0.33   -99   -99   -99   -99   -99   -99   -99   -99 
   120   -99 0.220 0.350 0.430 0.100   -99  1.30  0.28   -99   -99   -99   -99   -99   -99   -99   -99 
   135   -99 0.220 0.350 0.430 0.060   -99  1.30  0.10   -99   -99   -99   -99   -99   -99   -99   -99 
   150   -99 0.220 0.350 0.430 0.040   -99  1.30  0.10   -99   -99   -99   -99   -99   -99   -99   -99 
   165   -99 0.220 0.350 0.430 0.020   -99  1.30  0.10   -99   -99   -99   -99   -99   -99   -99   -99 
   180   -99 0.220 0.350 0.430 0.005   -99  1.30  0.10   -99   -99   -99   -99   -99   -99   -99   -99 

*IBBA910014  IBSNAT      -99     165 Tel Hadya
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Tel_Hadya   Syria             -99    -99  unknown
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.07   6.0  0.20  60.0  1.00  1.00 SA001 SA001 SA001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.170 0.300 0.380 0.925   -99  1.30  0.42   -99   -99   -99   -99   -99   -99   -99   -99 
    15   -99 0.190 0.320 0.400 1.000   -99  1.30  0.39   -99   -99   -99   -99   -99   -99   -99   -99 
    30   -99 0.190 0.320 0.400 0.850   -99  1.30  0.38   -99   -99   -99   -99   -99   -99   -99   -99 
    45   -99 0.200 0.330 0.410 0.630   -99  1.30  0.33   -99   -99   -99   -99   -99   -99   -99   -99 
    60   -99 0.200 0.330 0.410 0.548   -99  1.30  0.29   -99   -99   -99   -99   -99   -99   -99   -99 
    75   -99 0.190 0.320 0.400 0.358   -99  1.30  0.27   -99   -99   -99   -99   -99   -99   -99   -99 
    90   -99 0.190 0.320 0.400 0.238   -99  1.30  0.25   -99   -99   -99   -99   -99   -99   -99   -99 
   105   -99 0.190 0.320 0.400 0.180   -99  1.30  0.20   -99   -99   -99   -99   -99   -99   -99   -99 
   120   -99 0.190 0.320 0.400 0.100   -99  1.30  0.10   -99   -99   -99   -99   -99   -99   -99   -99 
   135   -99 0.170 0.300 0.380 0.050   -99  1.30  0.05   -99   -99   -99   -99   -99   -99   -99   -99 
   150   -99 0.170 0.300 0.380 0.025   -99  1.30  0.05   -99   -99   -99   -99   -99   -99   -99   -99 
   165   -99 0.170 0.300 0.380 0.010   -99  1.30  0.05   -99   -99   -99   -99   -99   -99   -99   -99 

*IBBA910018  IBSNAT      -99     150 Khanasser
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Khanasser   Syria             -99    -99  unknown
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.07   6.0  0.20  60.0  1.00  1.00 SA001 SA001 SA001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.200 0.330 0.350 0.925   -99  1.30  0.90   -99   -99   -99   -99   -99   -99   -99   -99 
    15   -99 0.200 0.330 0.350 0.780   -99  1.30  0.80   -99   -99   -99   -99   -99   -99   -99   -99 
    30   -99 0.200 0.330 0.350 0.650   -99  1.30  0.60   -99   -99   -99   -99   -99   -99   -99   -99 
    45   -99 0.200 0.330 0.350 0.530   -99  1.30  0.50   -99   -99   -99   -99   -99   -99   -99   -99 
    60   -99 0.200 0.330 0.350 0.448   -99  1.30  0.40   -99   -99   -99   -99   -99   -99   -99   -99 
    75   -99 0.200 0.330 0.350 0.258   -99  1.30  0.40   -99   -99   -99   -99   -99   -99   -99   -99 
    90   -99 0.200 0.330 0.350 0.138   -99  1.30  0.20   -99   -99   -99   -99   -99   -99   -99   -99 
   105   -99 0.200 0.330 0.350 0.080   -99  1.30  0.20   -99   -99   -99   -99   -99   -99   -99   -99 
   120   -99 0.200 0.330 0.350 0.045   -99  1.30  0.10   -99   -99   -99   -99   -99   -99   -99   -99 
   135   -99 0.200 0.330 0.350 0.015   -99  1.30  0.10   -99   -99   -99   -99   -99   -99   -99   -99 
   150   -99 0.200 0.330 0.350 0.015   -99  1.30  0.10   -99   -99   -99   -99   -99   -99   -99   -99 

*IBBA910041  IBSNAT      -99     160 Rothhamsted
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Rothamsted  England           -99    -99  unknown
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.14   6.0  0.50  60.0  1.00  1.00 SA001 SA001 SA001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    15   -99 0.130 0.280 0.330 1.000   -99  1.20  1.16   -99   -99   -99   -99   -99   -99   -99   -99 
    30   -99 0.180 0.320 0.420 0.900   -99  1.30  1.00   -99   -99   -99   -99   -99   -99   -99   -99 
    50   -99 0.250 0.370 0.420 0.700   -99  1.40  0.68   -99   -99   -99   -99   -99   -99   -99   -99 
    70   -99 0.250 0.370 0.420 0.500   -99  1.40  0.26   -99   -99   -99   -99   -99   -99   -99   -99 
   100   -99 0.250 0.370 0.420 0.200   -99  1.50  0.25   -99   -99   -99   -99   -99   -99   -99   -99 
   130   -99 0.250 0.370 0.420 0.100   -99  1.50  0.20   -99   -99   -99   -99   -99   -99   -99   -99 
   160   -99 0.250 0.370 0.420 0.050   -99  1.60  0.20   -99   -99   -99   -99   -99   -99   -99   -99 

*IBBA910061  IBSNAT      -99     195 The Murrays
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 The_Murrays Scotland          -99    -99  unknown
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.12   5.0  0.30  35.0  1.00  1.00 SA001 SA001 SA001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    15   -99 0.190 0.320 0.400 0.925   -99  1.30  1.00   -99   -99   -99   -99   -99   -99   -99   -99 
    45   -99 0.190 0.320 0.400 0.500   -99  1.30  1.00   -99   -99   -99   -99   -99   -99   -99   -99 
    75   -99 0.190 0.320 0.400 0.200   -99  1.30  0.90   -99   -99   -99   -99   -99   -99   -99   -99 
   105   -99 0.190 0.320 0.400 0.075   -99  1.30  0.75   -99   -99   -99   -99   -99   -99   -99   -99 
   135   -99 0.190 0.320 0.400 0.038   -99  1.30  0.40   -99   -99   -99   -99   -99   -99   -99   -99 
   165   -99 0.150 0.280 0.360 0.018   -99  1.30  0.20   -99   -99   -99   -99   -99   -99   -99   -99 
   195   -99 0.140 0.270 0.350 0.008   -99  1.30  0.10   -99   -99   -99   -99   -99   -99   -99   -99 

*IBPT910002  IBSNAT      -99      65 Sandy Clay (AUST.1970 P1, SALE)
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 -99         -99               -99    -99  UNKNOWN
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.14   5.0  0.60  60.0  1.00  1.00 SA001 SA001 SA001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    10   -99 0.180 0.300 0.380 0.750   -99  1.20  2.50   -99   -99   -99   -99   -99   -99   -99   -99 
    25   -99 0.180 0.300 0.380 1.000   -99  1.20  2.00   -99   -99   -99   -99   -99   -99   -99   -99 
    40   -99 0.180 0.300 0.380 0.500   -99  1.20  1.50   -99   -99   -99   -99   -99   -99   -99   -99 
    65   -99 0.180 0.300 0.380 0.150   -99  1.20  1.00   -99   -99   -99   -99   -99   -99   -99   -99 

*IBPT910005  IBSNAT      -99      65 Sandy Loam (Abedeen Idaho, 1978)
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Aberdeen    USA               -99    -99  UNKNOWN
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.14   5.0  0.60  60.0  1.00  1.00 SA001 SA001 SA001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    10   -99 0.110 0.250 0.330 0.800   -99  1.20  2.50   -99   -99   -99   -99   -99   -99   -99   -99 
    25   -99 0.110 0.250 0.330 1.000   -99  1.20  2.00   -99   -99   -99   -99   -99   -99   -99   -99 
    40   -99 0.110 0.250 0.330 0.500   -99  1.20  1.50   -99   -99   -99   -99   -99   -99   -99   -99 
    65   -99 0.110 0.250 0.330 0.050   -99  1.20  1.00   -99   -99   -99   -99   -99   -99   -99   -99 

*IBPT910006  IBSNAT      -99      65 MONTCALM MICHIGAN  McBRIDE SANDY LOAM
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 -99         -99               -99    -99  UNKNOWN (  6)
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.14   5.0  0.60  60.0  1.00  1.00 SA001 SA001 SA001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    10   -99 0.080 0.220 0.260 0.800   -99  1.15  2.50   -99   -99   -99   -99   -99   -99   -99   -99 
    25   -99 0.080 0.220 0.260 1.000   -99  1.20  2.00   -99   -99   -99   -99   -99   -99   -99   -99 
    40   -99 0.080 0.220 0.260 0.500   -99  1.25  1.50   -99   -99   -99   -99   -99   -99   -99   -99 
    65   -99 0.080 0.220 0.260 0.050   -99  1.25  1.00   -99   -99   -99   -99   -99   -99   -99   -99 

*UFPT930001  SCS         S       203 Eaugallie fine sand
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 BRADENTON   U.S.A.         27.300  82.300 SANDY, SILICEOUS, HYPERTHERMIC, ALFIC, HAPLAQUOD
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
     G  0.13  12.7  0.05  87.0  1.00  1.00 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    18   -99 0.033 0.166 0.540 1.000 18.40  1.22  1.00   0.6   2.9   0.0 1.500   6.0   -99   4.0   -99 
    36   -99 0.022 0.111 0.380 0.300 15.80  1.51  0.70   0.6   2.9   0.0 1.500   5.0   -99   2.5   -99 
    74   -99 0.016 0.081 0.331 0.100 21.10  1.62  0.14   0.6   1.9   0.0 0.500   4.8   -99   0.2   -99 
    81   -99 0.035 0.248 0.385 0.050 22.40  1.60  1.19   3.7   4.8   0.0 3.500   4.5   -99   8.1   -99 
   119   -99 0.013 0.159 0.355 0.000 12.40  1.66  0.60   1.5   2.6   0.0 1.000   4.8   -99   2.8   -99 
   173   -99 0.028 0.252 0.366 0.000  7.10  1.66  0.53   2.4   2.9   0.0 0.000   4.9   -99   4.0   -99 
   190   -99 0.070 0.260 0.310 0.000  0.50  1.79  0.21  13.0   3.0   0.0 0.000   4.8   -99   5.7   -99 
   203   -99 0.134 0.283 0.308 0.000  0.10  1.87  0.14  21.8   2.1   0.0 0.000   4.2   -99   6.7   -99 

*UFWH940002  SCS         SL      157 Crowley fine sandy loam
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 EAGLE LA,TX U.S.A.         29.600 -96.300 FINE, MONTMORILLONITIC, THERMIC TYPIC ALBAQUALFS
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
    BN  0.13   8.8  0.40  87.0  1.00  1.00 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    28    Ap 0.080 0.204 0.337 1.000   -99  1.60  1.00  10.0  30.0   0.0   -99   6.0   -99   -99   -99 
    38   A2g 0.080 0.204 0.337 0.670   -99  1.60  0.65  10.0  30.0   0.0   -99   5.6   -99   -99   -99 
    56  B21t 0.280 0.398 0.413 0.540   -99  1.52  0.60  55.0  20.0   0.0   -99   5.8   -99   -99   -99 
    97  B22g 0.223 0.335 0.350 0.370   -99  1.57  0.40  42.0   8.0   0.0   -99   6.0   -99   -99   -99 
   132    B3 0.223 0.335 0.350 0.250   -99  1.57  0.25  42.0   8.0   0.0   -99   7.2   -99   -99   -99 
   157     C 0.223 0.335 0.350 0.210   -99  1.57  0.15  42.0   8.0   0.0   -99   7.2   -99   -99   -99 

*UFWH940003  SCS         SIL     151 Captina silt loam
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 FAYETTEV,AR U.S.A.         36.100 -94.200 FINE-SILTY, MIXED, MESIC TYPIC FRAGIULDULTS
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
    BN  0.13   9.1  0.20  84.0  1.00  1.00 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    20    Ap 0.097 0.235 0.420 1.000  6.00  1.34  1.50  14.0  65.0   3.0  10.0   5.0   5.0   -99   -99 
    30  B21t 0.097 0.235 0.420 0.800  6.00  1.44  1.10  14.0  65.0   3.0  10.0   5.0   5.0   -99   -99 
    50  B22t 0.184 0.314 0.410 0.300  6.00  1.40  0.80  34.0  47.0   3.0  10.0   5.0   4.0   -99   -99 
    65   Bx1 0.184 0.314 0.410 0.200  2.00  1.40  0.60  34.0  47.0   3.0  10.0   5.0   4.0   -99   -99 
   136   Bx2 0.184 0.314 0.410 0.150  1.50  1.40  0.30  34.0  47.0   3.0  10.0   5.0   4.0   -99   -99 
   151     C 0.341 0.457 0.472 0.050  1.50  1.65  0.10  70.0  20.0   3.0  10.0   4.0   4.0   -99   -99 

*UFWH940004  SCS         LS      172 Eustis loamy sand
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 AMERICUS,GA U.S.A.         30.950 -87.100 SANDY, SILICEOUS, THERMIC PSAMMENTIC PALEUDULTS
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
    BN  0.13   8.5  0.53  64.0  1.00  1.00 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    18    Ap 0.055 0.177 0.302 1.000 33.00  1.48  1.00   6.0  15.0   0.0   -99   4.8   -99   -99   -99 
    51   A21 0.055 0.177 0.302 0.850 33.00  1.50  0.50   6.0  15.0   0.0   -99   4.8   -99   -99   -99 
    66   A22 0.055 0.177 0.302 0.400 33.00  1.50  0.30   6.0  15.0   0.0   -99   4.8   -99   -99   -99 
    91    B1 0.054 0.172 0.313 0.300 33.00  1.50  0.30  10.0  10.0   0.0   -99   4.8   -99   -99   -99 
   147  B21t 0.054 0.172 0.313 0.200 33.00  1.50  0.20  10.0  10.0   0.0   -99   4.8   -99   -99   -99 
   172  B22t 0.054 0.172 0.313 0.150 33.00  1.50  0.10  10.0  10.0   0.0   -99   4.8   -99   -99   -99 

*UFWH940005  SCS         SL      189 Greenville sandy loam
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 THORSBY,AL  U.S.A.         32.900 -86.700 CLAYEY, KAOLINITIC, THERMIC RHODIC PALEUDULTS
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
    BN  0.13   9.0  0.60  76.0  1.00  1.00 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    13    Ap 0.087 0.203 0.332 0.900 10.00  1.62  0.70  12.0  25.0   5.0   -99   5.0   -99   -99   -99 
    28    B1 0.219 0.329 0.345 0.600  3.50  1.57  0.60  42.0   8.0   3.0   -99   5.0   -99   -99   -99 
   139  B21t 0.299 0.413 0.428 0.300  3.50  1.53  0.30  60.0  15.0   2.0   -99   5.0   -99   -99   -99 
   189  B22t 0.299 0.413 0.428 0.200  3.50  1.53  0.15  60.0  15.0   2.0   -99   5.0   -99   -99   -99 

*UFWH940010  SCS         SL      150 Red Bay sandy loam
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 JAY,FL      U.S.A.         30.950 -87.100 FINE-LAOMY, SILICEOUS, THERMIC RHODIC PALEUDULTS
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
    BN  0.13   9.2  0.60  84.0  1.00  1.00 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    20    Ap 0.103 0.218 0.326 1.000   -99  1.63  0.89  15.0  15.5   0.0   -99   5.3   4.6   -99   -99 
    35    B1 0.125 0.239 0.333 0.600   -99  1.61  0.32  20.0  12.9   0.0   -99   5.3   4.4   -99   -99 
   128  B21t 0.147 0.261 0.337 0.100   -99  1.60  0.04  25.0  11.3   0.0   -99   5.5   4.8   -99   -99 
   150  B22t 0.125 0.236 0.332 0.020   -99  1.61  0.02  20.0   7.1   0.0   -99   5.6   5.2   -99   -99 

*IBSC910014  SASA        -99     300 Wartland
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 LAMERCY     RSA           -29.630  30.000 Loamy,silic,hyperth Arenic Paleudult
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.18   9.0  1.00  82.0  1.00  0.80 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.083 0.265 0.387 1.000  0.80  1.30  2.00   -99   -99   -99 0.000   7.0   -99   -99   -99 
    15   -99 0.083 0.265 0.329 0.820  0.80  1.61  1.00   -99   -99   -99 0.000   7.0   -99   -99   -99 
    30   -99 0.112 0.247 0.316 0.640  0.80  1.63  1.00   -99   -99   -99 0.000   7.0   -99   -99   -99 
    45   -99 0.141 0.238 0.319 0.470  0.80  1.59  0.50   -99   -99   -99 0.000   7.0   -99   -99   -99 
    60   -99 0.142 0.248 0.345 0.350  0.70  1.48  0.50   -99   -99   -99 0.000   7.0   -99   -99   -99 
    75   -99 0.152 0.268 0.359 0.260  0.60  1.46  0.50   -99   -99   -99 0.000   7.0   -99   -99   -99 
    90   -99 0.231 0.339 0.390 0.190  0.50  1.48  0.10   -99   -99   -99 0.000   7.0   -99   -99   -99 
   105   -99 0.317 0.359 0.385 0.140  0.50  1.56  0.10   -99   -99   -99 0.000   7.0   -99   -99   -99 
   120   -99 0.356 0.385 0.391 0.100  0.50  1.60  0.04   -99   -99   -99 0.000   7.0   -99   -99   -99 
   300   -99 0.367 0.415 0.418 0.015  0.01  1.56  0.24   -99   -99   -99 0.000   7.0   -99   -99   -99 

*IBSU910045              -99     300 Granja Sandy Loam
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Cordoba     Spain             -99    -99  Granja sandy loam. Cordoba, Spain
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.18   6.0  0.50  77.0  1.00  1.00 SA001 SA001 SA001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    10   -99   .08   .24   .34     1   -99   1.6     0   -99   -99   -99   -99   -99   -99   -99   -99
    30   -99   .08   .24   .34     1   -99   1.6     0   -99   -99   -99   -99   -99   -99   -99   -99
    60   -99   .08   .24   .36  .411   -99   1.6     0   -99   -99   -99   -99   -99   -99   -99   -99
    90   -99   .08   .24   .38  .223   -99   1.6     0   -99   -99   -99   -99   -99   -99   -99   -99
   120   -99   .08   .24   .37    .1   -99   1.6     0   -99   -99   -99   -99   -99   -99   -99   -99
   150   -99   .08   .24   .38    .1   -99   1.6     0   -99   -99   -99   -99   -99   -99   -99   -99
   180   -99   .08   .24   .35    .1   -99   1.6     0   -99   -99   -99   -99   -99   -99   -99   -99
   210   -99   .08   .24   .36    .1   -99   1.6     0   -99   -99   -99   -99   -99   -99   -99   -99
   240   -99   .08   .24   .35   .01   -99   1.6     0   -99   -99   -99   -99   -99   -99   -99   -99
   270   -99   .08   .24   .34  .001   -99   1.6     0   -99   -99   -99   -99   -99   -99   -99   -99
   300   -99   .08   .24   .34  .001   -99   1.6     0   -99   -99   -99   -99   -99   -99   -99   -99

*IBWH910999  Swift       -99     150 Wood Mountain Loam
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Swift_Curr  Canada            -99    -99  Orthic Brown Chernozem
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.12   8.0  0.50  60.0  1.00  1.00 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.096 0.230 0.250 1.000   -99  0.00  1.10   -99   -99   -99   -99   -99   -99   -99   -99 
    15   -99 0.096 0.230 0.250 0.800   -99  0.00  1.10   -99   -99   -99   -99   -99   -99   -99   -99 
    30   -99 0.112 0.250 0.260 0.700   -99  0.00  0.61   -99   -99   -99   -99   -99   -99   -99   -99 
    45   -99 0.094 0.220 0.230 0.500   -99  0.00  0.61   -99   -99   -99   -99   -99   -99   -99   -99 
    60   -99 0.103 0.220 0.230 0.250   -99  0.00  0.59   -99   -99   -99   -99   -99   -99   -99   -99 
    75   -99 0.103 0.220 0.230 0.150   -99  0.00  0.15   -99   -99   -99   -99   -99   -99   -99   -99 
    90   -99 0.102 0.220 0.250 0.080   -99  0.00  0.10   -99   -99   -99   -99   -99   -99   -99   -99 
   120   -99 0.102 0.220 0.250 0.050   -99  0.00  0.10   -99   -99   -99   -99   -99   -99   -99   -99 
   150   -99 0.102 0.220 0.250 0.050   -99  0.00  0.10   -99   -99   -99   -99   -99   -99   -99   -99 

*IBCH910008  IBSNAT      -99     180 VERY FINE MONTMORILLONITIC ISOHYPERTHERMIC
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Patancheru  India             -99    -99  Vertisol
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.13   6.0  0.70  95.0  1.00  0.80 SA001 SA001 SA001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.265 0.360 0.395 1.000   -99  1.50  0.73   -99   -99   -99   -99   8.8   -99   -99   -99 
    20   -99 0.200 0.337 0.395 1.000   -99  1.50  0.73   -99   -99   -99   -99   8.8   -99   -99   -99 
    30   -99 0.159 0.309 0.407 1.000   -99  1.51  0.54   -99   -99   -99   -99   9.2   -99   -99   -99 
    40   -99 0.154 0.310 0.407 0.900   -99  1.51  0.54   -99   -99   -99   -99   9.2   -99   -99   -99 
    50   -99 0.161 0.307 0.416 0.600   -99  1.37  0.47   -99   -99   -99   -99   9.4   -99   -99   -99 
    60   -99 0.155 0.300 0.416 0.600   -99  1.37  0.47   -99   -99   -99   -99   9.4   -99   -99   -99 
    75   -99 0.165 0.292 0.424 0.350   -99  1.38  0.39   -99   -99   -99   -99   9.4   -99   -99   -99 
    90   -99 0.165 0.262 0.424 0.350   -99  1.38  0.39   -99   -99   -99   -99   9.4   -99   -99   -99 
   110   -99 0.167 0.225 0.451 0.150   -99  1.40  0.28   -99   -99   -99   -99   9.4   -99   -99   -99 
   130   -99 0.160 0.222 0.451 0.150   -99  1.40  0.28   -99   -99   -99   -99   9.4   -99   -99   -99 
   155   -99 0.160 0.222 0.443 0.050   -99  1.40  0.25   -99   -99   -99   -99   9.4   -99   -99   -99 
   180   -99 0.165 0.222 0.443 0.050   -99  1.40  0.25   -99   -99   -99   -99   9.4   -99   -99   -99 

*IBCH910010  IBSNAT      -99      97 Kasireddipalli Series
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Patancheru  India             -99    -99  Vertisol, Typic Pellustert
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.11   6.0  0.70  84.5  1.00  0.90 SA001 SA001 SA001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    10   -99 0.140 0.420 0.470 1.000   -99  1.35  0.96   -99   -99   -99   -99   8.8   -99   -99   -99 
    22   -99 0.230 0.420 0.470 1.000   -99  1.35  0.96   -99   -99   -99   -99   8.8   -99   -99   -99 
    37   -99 0.220 0.400 0.460 0.700   -99  1.36  0.69   -99   -99   -99   -99   9.2   -99   -99   -99 
    52   -99 0.210 0.370 0.460 0.500   -99  1.36  0.68   -99   -99   -99   -99   9.2   -99   -99   -99 
    67   -99 0.210 0.365 0.460 0.250   -99  1.36  0.60   -99   -99   -99   -99   9.4   -99   -99   -99 
    82   -99 0.210 0.347 0.460 0.125   -99  1.36  0.60   -99   -99   -99   -99   9.4   -99   -99   -99 
    97   -99 0.200 0.300 0.460 0.060   -99  1.36  0.60   -99   -99   -99   -99   9.4   -99   -99   -99 

*UFBR950001  SCS         S       203 Eaugallie fine sand (IMPERVIOUS LAYER AT 120 CM)
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 BRADENTON   U.S.A.         27.300  82.300 SANDY, SILICEOUS, HYPERTHERMIC, ALFIC, HAPLAQUOD
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
     G  0.13  12.7  0.05  87.0  1.00  0.97 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    18   -99 0.022 0.133 0.423 1.000 18.40  1.22  1.02   0.8   1.1   0.0 0.090   6.0   -99   3.9   -99 
    36   -99 0.022 0.111 0.430 0.300 15.80  1.51  0.70   0.6   2.9   0.0 0.060   5.0   -99   2.6   -99 
    74   -99 0.022 0.111 0.430 0.010 21.10  1.62  0.14   0.6   1.9   0.0 0.010   4.8   -99   0.2   -99 
    81   -99 0.035 0.248 0.396 0.000 22.40  1.60  1.20   3.7   4.8   0.0 0.100   4.5   -99   8.1   -99 
   119   -99 0.013 0.159 0.374 0.000  0.00  1.66  0.60   1.5   2.6   0.0 0.050   4.8   -99   2.8   -99 
   173   -99 0.028 0.252 0.374 0.000  7.10  1.66  0.53   2.4   2.9   0.0 0.040   4.9   -99   4.1   -99 
   190   -99 0.070 0.260 0.324 0.000  0.50  1.79  0.21  13.0   3.0   0.0 0.020   4.8   -99   5.7   -99 
   203   -99 0.134 0.283 0.294 0.000  0.10  1.87  0.14  21.8   2.1   0.0 0.010   4.2  99.0   6.7   -99 

*IA00940001  SCS         SIC     175 LHK
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 LUSHNJE     ALBANIA        40.000  19.000 FINR
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
    BN  0.13   7.7  0.20  76.0  1.00  1.00 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    19    AP 0.277 0.407 0.422 1.000   -99  1.31  1.34  54.2  43.6   0.0 0.170   8.2   -99  34.6   -99 
    42    B1 0.282 0.411 0.426 0.200   -99  1.47  1.18  55.3  42.9   0.0 0.160   8.3   -99  34.5   -99 
    68    B2 0.279 0.408 0.423 0.200   -99  1.39  1.14  54.6  43.1   0.0 0.160   8.3   -99  38.6   -99 
    97    B3 0.252 0.385 0.400 0.150   -99  1.38  0.85  48.6  50.1   0.0 0.130   8.3   -99  36.0   -99 
   128    B4 0.245 0.375 0.397 0.100   -99  1.36  0.80  47.0  43.4   0.0 0.120   8.2   -99  34.1   -99 
   153    3C 0.155 0.286 0.391 0.100   -99  1.51  0.42  26.8  46.1   0.0 0.060   8.6   -99  19.8   -99 
   175    CG 0.220 0.350 0.403 0.100   -99  1.47  0.67  41.4  45.0   0.0 0.100   8.7   -99  30.4   -99 

*GA00620001  SCS         SL      191 LLOYD SERIES - SPALDING COUNTY, GA
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 GRIFFIN     USA            33.300 -84.300 RED YELLOW PODZOLIC
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
     R  0.14   9.0  0.60  76.0  1.00  0.92 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    18     A 0.092 0.209 0.325 1.000   -99  1.40  0.38  12.5  19.6   0.0   -99   5.5   -99   -99   -99 
    28     B 0.158 0.275 0.344 0.750   -99  1.40  0.12  27.5  17.4   0.0   -99   5.0   -99   -99   -99 
   127     B 0.269 0.392 0.409 0.500   -99  1.40  0.05  52.5  29.4   0.0   -99   5.0   -99   -99   -99 
   191     C 0.269 0.392 0.409 0.350   -99  1.40  0.05  52.5  29.4   0.0   -99   5.0   -99   -99   -99 

*CCBN910030  CIAT        -99     209 M2N
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Palmira(M2) Colombia        3.480 -73.370 Fine-silty,mixed,isohyperth.Aquic Hapludoll ( 30)
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.09  11.0  0.40  84.0  1.00  0.81 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.204 0.340 0.392 1.000   -99  1.45  2.19   -99   -99   -99   -99   6.9   -99   -99   -99 
    15   -99 0.204 0.340 0.392 1.000   -99  1.45  2.19   -99   -99   -99   -99   6.9   -99   -99   -99 
    25   -99 0.209 0.345 0.390 0.750   -99  1.45  1.21   -99   -99   -99   -99   7.2   -99   -99   -99 
    35   -99 0.209 0.345 0.390 0.500   -99  1.45  1.21   -99   -99   -99   -99   7.2   -99   -99   -99 
    50   -99 0.198 0.335 0.390 0.350   -99  1.49  0.53   -99   -99   -99   -99   8.0   -99   -99   -99 
    65   -99 0.185 0.323 0.395 0.200   -99  1.58  0.20   -99   -99   -99   -99   8.2   -99   -99   -99 
    80   -99 0.185 0.323 0.395 0.150   -99  1.58  0.20   -99   -99   -99   -99   8.2   -99   -99   -99 
    99   -99 0.201 0.328 0.408 0.100   -99  1.54  0.10   -99   -99   -99   -99   8.1   -99   -99   -99 
   122   -99 0.198 0.325 0.410 0.050   -99  1.58  0.09   -99   -99   -99   -99   8.2   -99   -99   -99 
   137   -99 0.159 0.288 0.399 0.000   -99  1.50  0.09   -99   -99   -99   -99   8.3   -99   -99   -99 
   159   -99 0.110 0.242 0.402 0.000   -99  1.69  0.10   -99   -99   -99   -99   8.3   -99   -99   -99 
   184   -99 0.047 0.177 0.351 0.000   -99  1.59  0.08   -99   -99   -99   -99   8.0   -99   -99   -99 
   209   -99 0.050 0.193 0.410 0.000   -99  1.45  0.12   -99   -99   -99   -99   8.5   -99   -99   -99 
! 26 Feb. 1994 J White made this profile to represent poorer field where
! P. Sexton's trial in 1990 was run.  Source was IBBN910030

*CCBN880060  SCS         SIL     200 UNKNOWN
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 POPAYAN     COLOMBIA        2.420 -76.580 MEDIAL, ISOTHERMIC TYPIC DYSTRANDEPT (INCEPTISOL)
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
    BK  0.09   8.7  0.60  76.0  1.00  0.90 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5    AP 0.075 0.215 0.406 0.500   -99  0.58 16.80   9.0  61.6   0.0 1.250   5.7   -99   -99   -99 
    13    AP 0.075 0.215 0.406 0.500   -99  0.58 16.80   9.0  61.6   0.0 1.250   5.7   -99   -99   -99 
    30     A 0.039 0.175 0.371 0.200   -99  0.55 15.80   0.9  53.0   0.0 0.940   4.9   -99   -99   -99 
    39    BA 0.050 0.164 0.302 0.100   -99  0.52  6.87   0.0  18.5   0.0 0.580   5.1   -99   -99   -99 
    66    BT 0.043 0.139 0.302 0.100   -99  0.48  3.80   0.0  14.0   0.0 0.360   5.3   -99   -99   -99 
    82    BT 0.041 0.130 0.302 0.100   -99  0.51  2.19   0.0  12.4   0.0 0.000   5.3   -99   -99   -99 
   102    BT 0.044 0.140 0.302 0.100   -99  0.62  1.88   0.0  14.2   0.0 0.000   5.3   -99   -99   -99 
   124    BT 0.040 0.128 0.302 0.100   -99  0.64  1.51   0.0  12.0   0.0 0.000   5.2   -99   -99   -99 
   137    BT 0.045 0.145 0.302 0.000   -99  0.51  1.66   0.0  15.2   0.0 0.000   5.2   -99   -99   -99 
   176    BT 0.037 0.117 0.302 0.000   -99  0.48  1.31   0.0  10.0   0.0 0.000   5.2   -99   -99   -99 
   200    BT 0.033 0.104 0.302 0.000   -99  0.40  1.65   0.0   7.7   0.0 0.000   5.6   -99   -99   -99 

*CCPA970001  SCS         SICL    145 CIAT
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 CIAT        COLOMBIA        3.500  76.350 Unknown
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
    BK  0.10  12.7  0.30  70.0  1.00  0.97 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    10   -99 0.216 0.357 0.460 1.000   -99  1.35  1.74  27.7  53.1   0.0   -99   7.3   -99  32.1   -99 
    38   -99 0.216 0.357 0.460 1.000   -99  1.40  1.57  27.7  53.1   0.0   -99   7.3   -99  32.1   -99 
    57   -99 0.168 0.306 0.460 0.600   -99  1.50  0.35  34.4  47.6   0.0   -99   8.2   -99  27.9   -99 
    81   -99 0.144 0.277 0.440 0.300   -99  1.50  0.17  23.1  56.1   0.0   -99   8.2   -99  23.1   -99 
    95   -99 0.140 0.272 0.450 0.100   -99  1.60  0.17  28.7  49.1   0.0   -99   8.5   -99  22.5   -99 
   110   -99 0.103 0.191 0.490 0.000   -99  1.60  0.29  21.9  54.2   0.0   -99   8.6   -99  14.3   -99 
   128   -99 0.035 0.177 0.450 0.000   -99  1.60  0.17   5.0  63.8   0.0   -99   8.9   -99   6.5   -99 
   145   -99 0.034 0.183 0.530 0.000   -99  1.60  0.17  43.0  49.7   0.0   -99   8.7   -99   6.1   -99 

*CI00960001  SCS         CL      150 El Batan
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 EL BATANG9  Mexico         19.310 -98.500 El Batan
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
    BN  0.13  10.5  0.40  76.0  1.00  1.00 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    15     A 0.171 0.290 0.359 0.750  1.50  1.50  0.80  31.0  26.0   3.0 0.120   6.2   -99  14.0   -99 
    30   -99 0.201 0.318 0.358 0.500  0.70  1.50  0.70  38.0  22.0   3.0 0.070   6.3   -99  15.5   -99 
    45   -99 0.178 0.296 0.359 0.350  1.30  1.60  0.90  33.0  25.0   4.0 0.100   6.3   -99  15.0   -99 
    60   -99 0.164 0.274 0.341 0.350  1.00  1.60  0.30  30.0  10.0   5.0   -99   6.0   -99   -99   -99 
    75   -99 0.058 0.186 0.325 0.350   -99  1.60  0.20  15.0   9.0   6.0   -99   6.0   -99   -99   -99 
    90   -99 0.048 0.154 0.338 0.200   -99  1.70  0.20  10.0   8.0   7.0   -99   6.0   -99   -99   -99 
   150   -99 0.039 0.125 0.327 0.150   -99  1.70  0.15   7.0   6.0  10.0   -99   6.0   -99   -99   -99 

*CI00970002  SCS         SICL    130 SUELO COLORADA
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 SANTA ROSA2 MEXICO         18.300 -95.100 HUMIC HAPLUDULTS
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
    YR  0.14  10.7  0.80  80.0  1.00  1.00 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    20    AP 0.185 0.315 0.406 0.750   -99  1.38  2.00  34.0  50.0   3.0   -99   5.4   -99  41.0   -99 
    37    A1 0.200 0.325 0.406 0.500   -99  1.38  1.60  36.0  48.0   2.0   -99   5.6   -99  45.0   -99 
    86    B1 0.238 0.365 0.395 0.150   -99  1.42  0.70  47.0  45.0   5.0   -99   5.5   -99  65.0   -99 
   130    B2 0.182 0.314 0.399 0.000   -99  1.41  0.22  34.0  55.0   5.0   -99   4.8   -99  70.0   -99 

*CBAN840001  XXXX   SACL SCL     120 Sandy-Clay Loam EL NUS: CENICAFE/CORPOICA
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 EL NUS COLO MBIA            6.480   3.000 Inceptisols.
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.13  11.5  0.65  75.0  1.00  1.00   -99   -99   -99
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.190 0.330 0.430 0.950  0.68  1.36  1.40  31.0  23.0   -99 0.140   4.8   -99   7.5   -99 
    10   -99 0.190 0.340 0.430 0.860  0.68  1.36  1.26  31.0  23.0   -99 0.140   4.8   -99   7.5   -99 
    15   -99 0.190 0.330 0.430 0.780  0.68  1.36  1.15  31.0  23.0   -99 0.140   4.8   -99   7.5   -99 
    20   -99 0.190 0.320 0.430 0.700  0.68  1.38  1.03  31.0  23.0   -99 0.140   4.8   -99   7.5   -99 
    25   -99 0.190 0.320 0.430 0.630  0.68  1.40  0.93  31.0  16.0   -99 0.060   5.3   -99   5.8   -99 
    30   -99 0.190 0.310 0.430 0.580  0.66  1.41  0.85  31.0  16.0   -99 0.060   5.3   -99   5.8   -99 
    35   -99 0.190 0.330 0.430 0.520  0.52  1.42  0.76  31.0  16.0   -99 0.060   5.3   -99   5.8   -99 
    40   -99 0.190 0.330 0.430 0.470  0.52  1.43  0.70  31.0  16.0   -99 0.060   5.3   -99   5.8   -99 
    70   -99 0.190 0.320 0.400 0.330  0.52  1.44  0.50  31.0  16.0   -99 0.160   5.3   -99   5.8   -99 
   120   -99 0.190 0.320 0.400 0.150  0.52  1.48  0.22  31.0  16.0   -99 0.100   5.3   -99   5.8   -99 

*CCCA800001  CL       CL SIL     120 ULTISOL CARIMAGUA-CIAT
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 CARIMAGUA   COLOMBIA        4.610  71.310 CLAY
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.13  11.5  0.65  75.0  1.00  1.00   -99   -99   -99
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.190 0.350 0.430 0.950   -99  1.30  1.60  12.0  50.0   -99 0.160   4.1   -99   5.6   -99 
    10   -99 0.240 0.400 0.430 0.860   -99  1.42  1.45  12.0  50.0   -99 0.160   4.1   -99   5.6   -99 
    15   -99 0.240 0.400 0.430 0.780   -99  1.43  1.31  12.0  50.0   -99 0.160   4.1   -99   5.6   -99 
    20   -99 0.240 0.400 0.430 0.700   -99  1.43  1.18  12.0  50.0   -99 0.160   4.1   -99   5.6   -99 
    25   -99 0.240 0.400 0.430 0.630   -99  1.43  1.06  12.0  48.0   -99 0.130   4.0   -99   3.4   -99 
    30   -99 0.240 0.390 0.430 0.580   -99  1.45  0.98  12.0  48.0   -99 0.130   4.0   -99   3.4   -99 
    35   -99 0.240 0.390 0.430 0.520   -99  1.46  0.88  12.0  48.0   -99 0.130   4.0   -99   3.4   -99 
    40   -99 0.240 0.390 0.430 0.470   -99  1.46  0.80  12.0  48.0   -99 0.130   4.0   -99   3.4   -99 
    70   -99 0.230 0.390 0.430 0.330   -99  1.48  0.56  12.0  48.0   -99 0.700   4.0   -99   3.4   -99 
   120   -99 0.230 0.390 0.430 0.150   -99  1.50  0.25  12.0  48.0   -99 0.500   4.0   -99   3.4   -99 

*CCQU790001  SCS         C        60 QUILICHAO
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 QUILICHAO   COLOMBIA        3.100  76.510 Fine, Kaolinitic, Isohyperth. oxic
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
    BN  0.13   6.7  0.18  76.0  1.00  1.00 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    20   -99 0.351 0.468 0.483 1.000   -99  0.96  5.10  71.0  18.0   -99 0.440   4.1   -99   2.2   -99 
    40   -99 0.356 0.470 0.485 0.700   -99  1.02  3.77  72.0  13.0   -99 0.320   3.9   -99   1.7   -99 
    60   -99 0.366 0.480 0.487 0.400   -99  1.12  2.77  74.0  13.0   -99 0.220   3.9   -99   1.3   -99 

*CCYU820001  XXXX   LOSA SL      120 ULTISOL, TYPIC PALEUDULT AT YURIMAGUAS-CIAT
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 YURIMAGUAS  PERU           -5.930   3.000 Loamy sand.
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.15  11.5  0.65  75.0  1.00  1.00   -99   -99   -99
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.060 0.190 0.440 0.950  0.00  1.30  1.20   6.0  21.0   -99 0.120   4.4   -99   3.5   -99 
    10   -99 0.060 0.210 0.440 0.860  0.00  1.52  1.07   6.0  21.0   -99 0.120   4.4   -99   3.5   -99 
    15   -99 0.060 0.200 0.440 0.780  0.00  1.54  0.99   6.0  21.0   -99 0.120   4.4   -99   3.5   -99 
    20   -99 0.060 0.210 0.440 0.700  0.00  1.55  0.88   6.0  21.0   -99 0.120   4.4   -99   3.5   -99 
    25   -99 0.060 0.200 0.410 0.630  0.00  1.55  0.80  16.0  24.0   -99 0.120   4.6   -99   3.7   -99 
    30   -99 0.060 0.200 0.410 0.580  0.00  1.57  0.73  16.0  24.0   -99 0.120   4.6   -99   3.7   -99 
    35   -99 0.060 0.200 0.410 0.520  0.00  1.58  0.65  16.0  24.0   -99 0.120   4.6   -99   3.7   -99 
    40   -99 0.060 0.200 0.410 0.470  0.00  1.59  0.60  16.0  24.0   -99 0.120   4.6   -99   3.7   -99 
    70   -99 0.060 0.200 0.410 0.330  0.00  1.62  0.42  16.0  24.0   -99 0.080   4.6   -99   3.7   -99 
   120   -99 0.059 0.200 0.410 0.150  0.00  1.63  0.19  16.0  24.0   -99 0.050   4.6   -99   3.7   -99 

*CNCH820001  XXXX   SALO L       120 SANDY LOAM AT LA ROMELIA-CENICAFE
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 LA ROMELIA  COLOMBIA        4.960  75.700 Typic Dystrandept.
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.13  11.5  0.65  75.0  0.80  1.00   -99   -99   -99
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.090 0.260 0.620 0.950   -99  0.70  3.60  19.0  31.0   -99 0.360   5.1   -99   3.8   -99 
    10   -99 0.130 0.360 0.620 0.860   -99  1.31  3.26  19.0  31.0   -99 0.360   5.1   -99   3.8   -99 
    15   -99 0.130 0.360 0.620 0.780   -99  1.33  2.95  19.0  31.0   -99 0.360   5.1   -99   3.8   -99 
    20   -99 0.130 0.360 0.620 0.700   -99  1.34  2.65  19.0  31.0   -99 0.360   5.1   -99   3.8   -99 
    25   -99 0.130 0.350 0.430 0.630   -99  1.37  2.39  19.0  31.0   -99 0.360   5.1   -99   -99   -99 
    30   -99 0.120 0.300 0.430 0.580   -99  1.37  2.20  19.0  31.0   -99 0.360   5.1   -99   -99   -99 
    35   -99 0.140 0.320 0.430 0.520   -99  1.39  1.97  19.0  31.0   -99 0.360   5.1   -99   -99   -99 
    40   -99 0.140 0.300 0.430 0.470   -99  1.40  1.78  19.0  31.0   -99 0.360   5.1   -99   -99   -99 
    70   -99 0.130 0.280 0.400 0.330   -99  1.49  1.26  19.0  31.0   -99 0.200   5.1   -99   -99   -99 
   120   -99 0.130 0.270 0.400 0.150   -99  1.55  0.57  19.0  31.0   -99 0.100   5.1   -99   -99   -99 

*SAUR900001  XXXX   SACL SCL     120 SANDY CLAY LOAM AT URRAO-SECR.AGRIC. ANTIOQUIA
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 URRAO       COLOMBIA        4.610  71.310 Inseptisols
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.13  11.5  0.65  75.0  1.00  1.00   -99   -99   -99
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.190 0.320 0.410 0.950  0.68  1.36  1.56  30.0  23.0   -99 0.150   4.5   -99   7.5   -99 
    10   -99 0.190 0.320 0.380 0.860  0.68  1.45  1.41  30.0  23.0   -99 0.150   4.5   -99   7.5   -99 
    15   -99 0.190 0.320 0.380 0.780  0.68  1.46  1.28  30.0  23.0   -99 0.150   4.5   -99   7.5   -99 
    20   -99 0.270 0.380 0.390 0.700  0.68  1.47  1.15  30.0  23.0   -99 0.150   4.5   -99   7.5   -99 
    25   -99 0.270 0.380 0.400 0.630  0.68  1.42  1.03  49.0  25.0   -99 0.150   4.5   -99   5.8   -99 
    30   -99 0.270 0.380 0.390 0.580  0.66  1.44  0.95  49.0  25.0   -99 0.060   4.5   -99   5.8   -99 
    35   -99 0.270 0.380 0.430 0.520  0.52  1.45  0.85  49.0  25.0   -99 0.060   4.5   -99   5.8   -99 
    40   -99 0.270 0.380 0.430 0.470  0.52  1.45  0.77  49.0  25.0   -99 0.060   4.5   -99   5.8   -99 
    70   -99 0.270 0.390 0.400 0.330  0.52  1.47  0.54  49.0  25.0   -99 0.050   4.5   -99   5.8   -99 
   120   -99 0.270 0.370 0.370 0.150  0.52  1.50  0.24  49.0  25.0   -99 0.020   4.5   -99   5.8   -99 

*IB00740011  SCS         CL      137 MAKIKI
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 HI021 (4)   US              0.000   0.000 ANDIC USTIC HUMITROPEPT, FINE, MIXED, ISOHYPERTHER
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
    BN  0.13  11.0  0.60  87.0  1.00  1.00 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5     A 0.189 0.306 0.383 0.600   -99  1.00  4.47  38.0  37.0  13.0   -99   5.3   -99   -99   -99 
    21     A 0.189 0.306 0.383 0.600   -99  1.00  4.47  38.0  37.0  13.0   -99   5.3   -99   -99   -99 
    51     A 0.189 0.306 0.383 0.600   -99  1.00  4.47  38.0  37.0  13.0   -99   5.3   -99   -99   -99 
    76     B 0.189 0.306 0.383 0.280   -99  1.00  0.00  38.0  37.0  13.0   -99   5.8   -99   -99   -99 
   137     B 0.145 0.234 0.383 0.120   -99  0.85  0.00  38.0  37.0  43.0   -99   5.8   -99   -99   -99 

*IB00830002  SCS         SIL     150 Unknown (Kula Ag Park)
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 KULA AGPARK US             20.780 156.376 TORROXIC HAPLUSTOLL, FINE, KAOLINITIC, ISOHYPERTHE
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
    BN  0.13  10.0  0.60  91.0  1.00  1.00 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    10    AP 0.150 0.283 0.400 0.750   -99  1.29  1.96  25.6  50.8   0.0   -99   7.0   6.3  26.4   -99 
    29    BW 0.132 0.267 0.400 1.000   -99  1.08  1.29  21.7  52.4   0.0   -99   7.1   6.4  24.5   -99 
    48    BW 0.137 0.263 0.393 0.750   -99  1.12  0.95  24.2  48.0   8.0   -99   7.2   6.4  20.0   -99 
    82    BW 0.146 0.277 0.392 1.000   -99  1.17  0.78  24.7  47.3   0.0   -99   7.3   6.7  16.9   -99 
   122    BW 0.144 0.286 0.395 0.500   -99  1.14  0.53  24.3  67.7   0.0   -99   7.3   6.7  15.7   -99 
   150    BW 0.184 0.321 0.398 0.200   -99  1.30  0.42  33.4  56.6   0.0   -99   7.4   6.6  13.4   -99 

*IB00830003  SCS         L       160 Unknown (Pulehu Experimental Station)
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 PULEHU EXFM US             20.781 156.333 TORROXIC HAPLUSTOLL, FINE, KAOLINITIC, ISOHYPERTHE
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
    BN  0.13  10.1  0.60  94.0  1.00  1.00 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    31    AP 0.142 0.266 0.396 1.000   -99  0.95  3.98  26.0  48.6  11.0 0.390   6.1   5.3  38.9   -99 
    46    BW 0.178 0.309 0.408 1.000   -99  1.01  2.05  32.6  49.9   3.0 0.220   6.6   5.9  24.5   -99 
    58    BW 0.210 0.334 0.402 1.000   -99  1.08  1.01  41.6  45.4   9.0 0.090   6.1   5.4  15.5   -99 
    88    BW 0.162 0.262 0.402 1.000   -99  1.11  0.87  40.2  46.8  38.0   -99   5.6   4.8  13.9   -99 
   160    BC 0.098 0.179 0.410 0.500   -99  0.74  0.92  28.3  52.7  56.0   -99   5.8   4.7  18.2   -99 

*IB00720001  SCS         CL       51 Unknown (Mauka Campus)
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 MAUKA CAMPU US             21.310 157.840 -99
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
    BN  0.13  10.4  0.60  76.0  1.00  1.00 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    25    AP 0.154 0.266 0.365 1.000   -99  1.37  3.61  30.0  30.0  15.0   -99   6.7   -99   -99   -99 
    51    AP 0.154 0.266 0.365 1.000   -99  1.32  3.53  30.0  30.0  15.0   -99   6.8   -99   -99   -99 

*IBPI910013  IBSNAT      -99     110 Waipio
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Waipio,HI   USA               -99    -99  Clayey, kaolinitic, isohyperth, Tropeptic Eutrusto
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.14   5.0  0.60  60.0  1.00  1.00 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.220 0.350 0.550 1.000   -99  1.00  2.27   -99   -99   -99   -99   6.3   -99   -99   -99 
    15   -99 0.230 0.350 0.550 1.000   -99  1.00  2.27   -99   -99   -99   -99   6.3   -99   -99   -99 
    30   -99 0.240 0.350 0.550 0.800   -99  1.05  1.10   -99   -99   -99   -99   5.8   -99   -99   -99 
    50   -99 0.250 0.370 0.480 0.400   -99  1.17  1.41   -99   -99   -99   -99   5.8   -99   -99   -99 
    70   -99 0.260 0.380 0.460 0.200   -99  1.22  0.59   -99   -99   -99   -99   6.0   -99   -99   -99 
    90   -99 0.250 0.380 0.460 0.050   -99  1.22  0.36   -99   -99   -99   -99   6.0   -99   -99   -99 
   110   -99 0.260 0.400 0.480 0.020   -99  1.17  0.27   -99   -99   -99   -99   6.0   -99   -99   -99 

*IBAR910030  SCS         -99     137 Clay
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 -99         -99               -99    -99  CLAYEY, KAOLINITIC, ISOHYPERTHERMIC TROPEPTIC HAPL
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.13   6.9  0.60  76.0  1.00  1.00 SA001 SA001 SA001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.266 0.380 0.412 1.000   -99  1.39  2.01   -99   -99   -99   -99   4.8   -99  20.0   -99 
    20   -99 0.266 0.380 0.412 0.900   -99  1.39  2.01   -99   -99   -99   -99   4.8   -99  20.0   -99 
    35   -99 0.320 0.422 0.440 0.750   -99  1.33  0.72   -99   -99   -99   -99   6.1   -99  20.0   -99 
    50   -99 0.320 0.422 0.440 0.500   -99  1.33  0.72   -99   -99   -99   -99   6.1   -99  20.0   -99 
    65   -99 0.319 0.421 0.436 0.200   -99  1.41  0.56   -99   -99   -99   -99   6.4   -99  20.0   -99 
    80   -99 0.319 0.421 0.436 0.100   -99  1.41  0.56   -99   -99   -99   -99   6.4   -99  20.0   -99 
    99   -99 0.325 0.425 0.463 0.050   -99  1.26  0.43   -99   -99   -99   -99   6.6   -99  20.0   -99 
   118   -99 0.314 0.415 0.433 0.005   -99  1.50  0.23   -99   -99   -99   -99   6.7   -99  20.0   -99 
   137   -99 0.314 0.415 0.433 0.000   -99  1.50  0.23   -99   -99   -99   -99   6.7   -99  20.0   -99 

*IBAR910034  S70Ha-7-1   -99      66 IBAR910034
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 -99         -99               -99    -99  VERTIC HAPLUSTOLLS, VERY-FINE, KAOLINITIC, ISOHYPE
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.13  11.7  0.60  87.0  0.80  1.00 SA001 SA001 SA001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.226 0.348 0.396 1.000   -99  1.18  1.98   -99   -99   -99   -99   6.1   -99  20.0   -99 
    18   -99 0.226 0.348 0.396 0.800   -99  1.18  1.98   -99   -99   -99   -99   6.1   -99  20.0   -99 
    28   -99 0.233 0.359 0.403 0.500   -99  1.22  1.90   -99   -99   -99   -99   6.2   -99  20.0   -99 
    38   -99 0.233 0.359 0.403 0.050   -99  1.22  1.90   -99   -99   -99   -99   6.2   -99  20.0   -99 
    52   -99 0.223 0.347 0.400 0.005   -99  1.10  0.80   -99   -99   -99   -99   6.4   -99  20.0   -99 
    66   -99 0.223 0.347 0.400 0.000   -99  1.10  0.80   -99   -99   -99   -99   6.4   -99  20.0   -99 

*IBAR910035  83P0882     -99     153 IBAR910035
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 -99         -99               -99    -99  CLAYEY, OXIDIC, ISOTHERMIC HUMOXIC TROPOHUMULT (35
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.13  10.1  0.60  85.0  1.00  1.00 SA001 SA001 SA001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.155 0.298 0.419 1.000   -99  1.17  3.58   -99   -99   -99   -99   5.1   -99  20.0   -99 
    16   -99 0.155 0.298 0.419 0.800   -99  1.17  3.58   -99   -99   -99   -99   5.1   -99  20.0   -99 
    30   -99 0.141 0.280 0.413 0.500   -99  1.33  2.22   -99   -99   -99   -99   4.8   -99  20.0   -99 
    40   -99 0.109 0.247 0.426 0.250   -99  1.65  1.30   -99   -99   -99   -99   4.5   -99  20.0   -99 
    51   -99 0.109 0.247 0.426 0.200   -99  1.65  1.30   -99   -99   -99   -99   4.5   -99  20.0   -99 
    62   -99 0.104 0.222 0.388 0.200   -99  1.61  0.98   -99   -99   -99   -99   4.7   -99  20.0   -99 
    74   -99 0.104 0.222 0.388 0.100   -99  1.61  0.98   -99   -99   -99   -99   4.7   -99  20.0   -99 
    97   -99 0.067 0.158 0.361 0.010   -99  1.68  0.88   -99   -99   -99   -99   4.7   -99  20.0   -99 
   123   -99 0.026 0.085 0.354 0.000   -99  1.70  3.14   -99   -99   -99   -99   4.9   -99  20.0   -99 
   153   -99 0.034 0.111 0.354 0.000   -99  1.70  1.75   -99   -99   -99   -99   5.1   -99  20.0   -99 

*IBAR910043  87P294      -99     227 IBAR910043
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 -99         -99               -99    -99  VERY-FINE, KAOLINITIC, ISOHYPERTHERMIC XANTHIC KAN
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.13   6.9  0.60  76.0  0.80  1.00 SA001 SA001 SA001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.305 0.407 0.422 0.500   -99  1.39  2.01   -99   -99   -99   -99   6.0   -99   -99   -99 
    10   -99 0.305 0.407 0.422 0.500   -99  1.39  2.01   -99   -99   -99   -99   6.0   -99   -99   -99 
    22   -99 0.305 0.407 0.422 0.500   -99  1.39  2.01   -99   -99   -99   -99   6.0   -99   -99   -99 
    35   -99 0.389 0.498 0.513 0.500   -99  1.33  0.72   -99   -99   -99   -99   6.5   -99   -99   -99 
    48   -99 0.389 0.498 0.513 0.500   -99  1.33  0.72   -99   -99   -99   -99   6.5   -99   -99   -99 
    63   -99 0.386 0.494 0.509 0.200   -99  1.41  0.56   -99   -99   -99   -99   6.5   -99   -99   -99 
    78   -99 0.386 0.494 0.509 0.200   -99  1.41  0.56   -99   -99   -99   -99   6.5   -99   -99   -99 
    97   -99 0.394 0.502 0.517 0.200   -99  1.26  0.43   -99   -99   -99   -99   6.5   -99   -99   -99 
   116   -99 0.383 0.493 0.508 0.200   -99  1.50  0.23   -99   -99   -99   -99   6.5   -99   -99   -99 
   135   -99 0.383 0.493 0.508 0.200   -99  1.50  0.23   -99   -99   -99   -99   6.5   -99   -99   -99 
   167   -99 0.350 0.463 0.478 0.200   -99  1.52  0.18   -99   -99   -99   -99   6.5   -99   -99   -99 
   197   -99 0.372 0.480 0.495 0.200   -99  1.65  0.12   -99   -99   -99   -99   6.5   -99   -99   -99 
   227   -99 0.372 0.480 0.495 0.200   -99  1.65  0.12   -99   -99   -99   -99   6.5   -99   -99   -99 

*UFON780006  S25-2-(1-7) S       203 Pomona Fine Sand
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Ona REC     USA            27.420 -81.920 Ultic Haplaquods, sandy, siliceous, hyperthermic
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
    BL  0.09   5.9  0.60  30.0  1.00  0.92 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     8    A1 0.027 0.110 0.471 1.000 21.00  1.32  1.02   0.6   5.5   0.0   -99   5.2   -99   8.1   -99 
    25   A21 0.021 0.089 0.389 1.000 10.90  1.56  0.27   0.5   3.4   0.0   -99   4.9   -99   0.3   -99 
    68   A22 0.023 0.091 0.347 1.000 13.80  1.68  0.11   0.4   2.3   0.0   -99   4.9   -99   0.0   -99 
    89   B2H 0.020 0.087 0.307 0.800  3.20  1.78  0.62   1.6   5.4   0.0   -99   4.5   -99   5.3   -99 
   117    B3 0.025 0.097 0.313 0.800  4.70  1.77  0.35   1.1   2.9   0.0   -99   4.7   -99   2.3   -99 
   145   A'2 0.025 0.097 0.320 0.600  2.20  1.75  0.28   2.6   2.9   0.0   -99   5.0   -99   2.5   -99 
   203  B'TG 0.175 0.261 0.336 0.600  2.20  1.75  0.13  16.6   3.3   0.0   -99   5.0   -99  11.0   -99 

*IBSB910032  Sioux       -99     182 Sioux Loam
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 -99         -99               -99    -99  Sioux Loam
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.11   9.0  0.50  77.0  1.00  1.00 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     7   -99 0.265 0.467 0.653 1.000   -99  1.20  0.00   -99   -99   -99   -99   -99   -99   -99   -99 
    19   -99 0.270 0.444 0.580 1.000   -99  1.20  0.00   -99   -99   -99   -99   -99   -99   -99   -99 
    32   -99 0.270 0.444 0.580 1.000   -99  1.20  0.00   -99   -99   -99   -99   -99   -99   -99   -99 
    47   -99 0.270 0.444 0.580 0.500   -99  1.20  0.00   -99   -99   -99   -99   -99   -99   -99   -99 
    62   -99 0.270 0.444 0.580 0.500   -99  1.20  0.00   -99   -99   -99   -99   -99   -99   -99   -99 
    92   -99 0.202 0.333 0.435 0.250   -99  1.20  0.00   -99   -99   -99   -99   -99   -99   -99   -99 
   122   -99 0.202 0.333 0.435 0.100   -99  1.20  0.00   -99   -99   -99   -99   -99   -99   -99   -99 
   152   -99 0.202 0.333 0.435 0.000   -99  1.20  0.00   -99   -99   -99   -99   -99   -99   -99   -99 
   182   -99 0.202 0.333 0.435 0.000   -99  1.20  0.00   -99   -99   -99   -99   -99   -99   -99   -99 

*IBSB910049  Nicollet mo -99     202 NICOLLET CLAY LOAM
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 -99         -99               -99    -99  NICOLLET CLAY LOAM (FINE-LOAMY,MIXED,MESIC AQUIC H
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.09  10.6  0.60  75.0  1.00  1.00 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.153 0.283 0.340 1.000   -99  1.37  0.00   -99   -99   -99   -99   7.0   -99   -99   -99 
    17   -99 0.143 0.283 0.340 1.000   -99  1.37  0.00   -99   -99   -99   -99   7.0   -99   -99   -99 
    29   -99 0.220 0.330 0.390 0.800   -99  1.37  0.00   -99   -99   -99   -99   7.0   -99   -99   -99 
    43   -99 0.205 0.340 0.400 0.800   -99  1.37  0.00   -99   -99   -99   -99   7.0   -99   -99   -99 
    55   -99 0.205 0.340 0.400 0.800   -99  1.35  0.00   -99   -99   -99   -99   7.0   -99   -99   -99 
    67   -99 0.205 0.340 0.400 0.700   -99  1.35  0.00   -99   -99   -99   -99   7.0   -99   -99   -99 
    79   -99 0.205 0.330 0.390 0.700   -99  1.35  0.00   -99   -99   -99   -99   7.0   -99   -99   -99 
    94   -99 0.190 0.320 0.380 0.600   -99  1.35  0.00   -99   -99   -99   -99   7.0   -99   -99   -99 
   123   -99 0.160 0.300 0.355 0.600   -99  1.40  0.00   -99   -99   -99   -99   7.0   -99   -99   -99 
   152   -99 0.140 0.300 0.355 0.500   -99  1.40  0.00   -99   -99   -99   -99   7.0   -99   -99   -99 
   177   -99 0.140 0.300 0.355 0.500   -99  1.40  0.00   -99   -99   -99   -99   7.0   -99   -99   -99 
   202   -99 0.140 0.300 0.355 0.500   -99  1.40  0.00   -99   -99   -99   -99   7.0   -99   -99   -99 

*IUBF970110  wine2   IA  L       210 Ritchie 6/12/97  Loam
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 -99         -99             2.000     -99 unknown 
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.09   7.0  0.55  70.0  0.50  1.00 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.183 0.360 0.465 1.000 79.00  1.31  2.03  26.0  36.0   -99   -99   6.4   -99   -99   -99 
    15   -99 0.183 0.360 0.438 1.000 79.00  1.39  2.03  26.0  36.0   -99   -99   6.4   -99   -99   -99 
    30   -99 0.183 0.360 0.451 1.000 79.00  1.35  2.03  27.0  37.0   -99   -99   6.4   -99   -99   -99 
    45   -99 0.183 0.360 0.458 1.000 79.00  1.33  2.03  27.0  37.0   -99   -99   6.4   -99   -99   -99 
    60   -99 0.183 0.360 0.451 1.000 79.00  1.35  0.44  27.0  37.0   -99   -99   6.7   -99   -99   -99 
    75   -99 0.183 0.360 0.458 1.000 79.00  1.33  0.44  29.0  36.0   -99   -99   6.7   -99   -99   -99 
    90   -99 0.183 0.360 0.458 1.000 79.00  1.33  0.44  29.0  36.0   -99   -99   6.7   -99   -99   -99 
   105   -99 0.163 0.330 0.434 0.850 79.00  1.40  0.15  20.0  28.0   -99   -99   7.9   -99   -99   -99 
   120   -99 0.163 0.330 0.410 0.700 79.00  1.47  0.15  20.0  28.0   -99   -99   7.9   -99   -99   -99 
   135   -99 0.163 0.330 0.396 0.550 79.00  1.51  0.15  18.0  31.0   -99   -99   7.9   -99   -99   -99 
   150   -99 0.163 0.330 0.403 0.400 79.00  1.49  0.15  18.0  31.0   -99   -99   7.9   -99   -99   -99 
   165   -99 0.163 0.330 0.403 0.300 79.00  1.49  0.15  18.0  31.0   -99   -99   7.9   -99   -99   -99 
   180   -99 0.163 0.330 0.403 0.200 79.00  1.49  0.15  18.0  31.0   -99   -99   7.9   -99   -99   -99 
   195   -99 0.163 0.330 0.403 0.100 79.00  1.49  0.15  18.0  31.0   -99   -99   7.9   -99   -99   -99 
   210   -99 0.163 0.330 0.403 0.000 79.00  1.49  0.15  18.0  31.0   -99   -99   7.9   -99   -99   -99 
! tiledrain at 100 cm

*IUBF970211  SOREN-  IA  L       210 Ritchie 6/12/97  Loam
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 -99         -99             2.000     -99 unknown 
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.09   7.0  0.55  60.0  0.50  0.92 IB001 IB001 IB001
!   -99  0.09   7.0  0.55  60.0  0.50  1.00 IB001 IB001 IB001, KJB - 5/31/09 decreased .99 to .92
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.163 0.340 0.465 1.000 79.00  1.31  2.03  26.0  36.0   -99   -99   6.4   -99   -99   -99 
    15   -99 0.163 0.340 0.438 1.000 79.00  1.39  2.03  26.0  36.0   -99   -99   6.4   -99   -99   -99 
    30   -99 0.173 0.350 0.451 1.000 79.00  1.35  2.03  27.0  37.0   -99   -99   6.4   -99   -99   -99 
    45   -99 0.193 0.350 0.458 1.000 79.00  1.33  2.03  27.0  37.0   -99   -99   6.4   -99   -99   -99 
    60   -99 0.193 0.350 0.451 1.000 79.00  1.35  0.44  27.0  37.0   -99   -99   6.7   -99   -99   -99 
    75   -99 0.173 0.350 0.458 1.000 79.00  1.33  0.44  29.0  36.0   -99   -99   6.7   -99   -99   -99 
    90   -99 0.173 0.350 0.458 1.000 79.00  1.33  0.44  29.0  36.0   -99   -99   6.7   -99   -99   -99 
   105   -99 0.163 0.330 0.434 0.850 79.00  1.40  0.15  20.0  28.0   -99   -99   7.9   -99   -99   -99 
   120   -99 0.163 0.330 0.410 0.700 79.00  1.47  0.15  20.0  28.0   -99   -99   7.9   -99   -99   -99 
   135   -99 0.163 0.330 0.396 0.550 79.00  1.51  0.15  18.0  31.0   -99   -99   7.9   -99   -99   -99 
   150   -99 0.163 0.330 0.403 0.400 79.00  1.49  0.15  18.0  31.0   -99   -99   7.9   -99   -99   -99 
   165   -99 0.163 0.330 0.403 0.300 79.00  1.49  0.15  18.0  31.0   -99   -99   7.9   -99   -99   -99 
   180   -99 0.163 0.330 0.403 0.200 79.00  1.49  0.15  18.0  31.0   -99   -99   7.9   -99   -99   -99 
   195   -99 0.163 0.330 0.403 0.100 79.00  1.49  0.15  18.0  31.0   -99   -99   7.9   -99   -99   -99 
   210   -99 0.163 0.330 0.403 0.000 79.00  1.49  0.15  18.0  31.0   -99   -99   7.9   -99   -99   -99 

*LUGO930001  SAU CAL SOY SCL     150 FINCA DE PRACTICAS EPS CALICATA 1 (LUGO) Plus 0.01 water
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 LUGO        SPAIN          43.040  -3.300 TYPIC HAPLUMREPT
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
    YR  0.14   9.7  0.60  80.0  1.00  1.00 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    15    A1 0.121 0.265 0.348 1.000 21.50  0.95  2.83  21.0  26.0   9.9 4.880   6.5   5.3  15.4   -99 
    30    A2 0.125 0.274 0.350 0.750 28.10  1.12  2.34  21.0  27.0   4.8 4.030   6.4   5.0  11.4   -99 
    50     B 0.111 0.260 0.354 0.500 50.30  1.26  0.56  18.0  32.0   6.9 0.960   6.0   4.0   6.6   -99 
    65    BC 0.101 0.252 0.344 0.350 31.00  1.31  0.48  15.0  29.0   2.2 0.830   5.2   3.7   5.9   -99 
   150     C 0.085 0.232 0.321 0.200 12.90  1.35  0.41  11.0  19.0   0.2 0.720   4.9   3.3   5.0   -99 

*IBSB910046  Wooster     -99     153 WOOSTER SILT LOAM
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 -99         -99               -99    -99  WOOSTER SILT LOAM (FINE-LOAMY,MIXED MESIC TYPIC FR
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.13   8.8  0.40  84.0  1.00  1.00 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.042 0.222 0.427 0.100   -99  1.40  2.50   -99   -99   -99   -99   6.0   -99   -99   -99 
    15   -99 0.042 0.222 0.427 0.750   -99  1.40  2.50   -99   -99   -99   -99   6.0   -99   -99   -99 
    25   -99 0.042 0.222 0.427 0.750   -99  1.40  0.00   -99   -99   -99   -99   5.5   -99   -99   -99 
    33   -99 0.083 0.223 0.427 0.500   -99  1.50  0.00   -99   -99   -99   -99   5.5   -99   -99   -99 
    41   -99 0.083 0.223 0.427 0.500   -99  1.50  0.00   -99   -99   -99   -99   5.5   -99   -99   -99 
    54   -99 0.112 0.252 0.372 0.350   -99  1.50  0.00   -99   -99   -99   -99   5.5   -99   -99   -99 
    62   -99 0.112 0.252 0.372 0.350   -99  1.50  0.00   -99   -99   -99   -99   5.5   -99   -99   -99 
    70   -99 0.112 0.252 0.372 0.350   -99  1.50  0.00   -99   -99   -99   -99   5.5   -99   -99   -99 
    98   -99 0.116 0.196 0.315 0.200   -99  1.65  0.00   -99   -99   -99   -99   5.6   -99   -99   -99 
   123   -99 0.116 0.196 0.315 0.200   -99  1.65  0.00   -99   -99   -99   -99   5.5   -99   -99   -99 
   148   -99 0.116 0.196 0.315 0.150   -99  1.65  0.00   -99   -99   -99   -99   5.5   -99   -99   -99 
   153   -99 0.116 0.196 0.315 0.150   -99  1.53  0.00   -99   -99   -99   -99   6.0   -99   -99   -99 

*IBSB910064              -99     205 Deep Silt Loam
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 -99         -99               -99    -99  DEEP SILT LOAM
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.12   6.0  0.40  77.0  1.00  1.00 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    10   -99 0.106 0.262 0.362 1.000   -99  1.37  1.16   -99   -99   -99   -99   6.5   -99   -99   -99 
    25   -99 0.106 0.262 0.362 0.819   -99  1.37  1.10   -99   -99   -99   -99   6.5   -99   -99   -99 
    40   -99 0.107 0.262 0.362 0.607   -99  1.37  0.97   -99   -99   -99   -99   6.5   -99   -99   -99 
    55   -99 0.107 0.262 0.362 0.607   -99  1.37  0.97   -99   -99   -99   -99   6.5   -99   -99   -99 
    85   -99 0.108 0.261 0.361 0.368   -99  1.38  0.72   -99   -99   -99   -99   6.5   -99   -99   -99 
   115   -99 0.110 0.260 0.360 0.202   -99  1.38  0.43   -99   -99   -99   -99   6.5   -99   -99   -99 
   145   -99 0.111 0.259 0.359 0.111   -99  1.39  0.20   -99   -99   -99   -99   6.5   -99   -99   -99 
   175   -99 0.112 0.258 0.358 0.061   -99  1.39  0.06   -99   -99   -99   -99   6.5   -99   -99   -99 
   205   -99 0.112 0.258 0.358 0.033   -99  1.39  0.01   -99   -99   -99   -99   6.5   -99   -99   -99 

*ISUV950008  AEES, AMES  -99     152 CLARION LOAM
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 -99         -99            42.000     -99  TYPIC HAPLUDOLL, FINE-LOAMY, MIXED, MESIC ( 31)
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.09   9.8  0.40  84.0  0.50  1.00 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.110 0.300 0.361 1.000  3.30  1.45  2.03  21.0   -99   -99   -99   6.4   -99   -99   -99 
    18   -99 0.110 0.300 0.361 1.000  3.30  1.45  2.03  21.0   -99   -99   -99   6.4   -99   -99   -99 
    31   -99 0.110 0.300 0.361 1.000  3.30  1.45  2.03  21.0   -99   -99   -99   6.4   -99   -99   -99 
    46   -99 0.110 0.300 0.361 1.000  3.30  1.45  2.03  27.0   -99   -99   -99   6.4   -99   -99   -99 
    56   -99 0.129 0.310 0.371 1.000  3.30  1.60  0.44  27.0   -99   -99   -99   6.7   -99   -99   -99 
    66   -99 0.129 0.310 0.371 1.000  3.30  1.60  0.44  27.0   -99   -99   -99   6.7   -99   -99   -99 
    91   -99 0.129 0.310 0.371 0.500  3.30  1.60  0.44  27.0   -99   -99   -99   6.7   -99   -99   -99 
   111   -99 0.107 0.229 0.369 0.500  3.30  1.60  0.15  17.0   -99   -99   -99   7.9   -99   -99   -99 
   132   -99 0.107 0.229 0.369 0.500  3.30  1.60  0.15  17.0   -99   -99   -99   7.9   -99   -99   -99 
   152   -99 0.107 0.229 0.369 0.500  3.30  1.60  0.15  17.0   -99   -99   -99   7.9   -99   -99   -99 

*MSKB890006  IBSNAT      L       120 KALAMAZOO LOAM
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Kalamazoo   MI, U.S.A.     41.700 -85.500 Typic Hapludalf
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
  10YR  0.13   9.0  0.15  80.0  1.00  1.00 SA001 SA001 SA001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    10    Ap 0.137 0.270 0.380 1.000   -99  1.60  1.10  19.0  38.0   -99 0.130   6.5   -99  10.0   -99 
    22    Ap 0.137 0.270 0.380 1.000   -99  1.60  0.90  19.0  38.0   -99 0.130   6.5   -99  10.0   -99 
    31     E 0.137 0.270 0.380 1.000   -99  1.60  0.70  22.0  47.0   -99 0.130   6.5   -99  10.0   -99 
    41   Btu 0.165 0.298 0.343 0.900   -99  1.60  0.30  23.0  44.0   -99 0.050   6.5   -99  10.0   -99 
    51   Btm 0.165 0.300 0.410 0.700   -99  1.60  0.22  25.0  19.0   -99 0.040   6.0   -99  10.0   -99 
    61   Btl 0.137 0.270 0.380 0.800   -99  1.60  0.10  21.0  17.0   -99 0.040   6.0   -99  10.0   -99 
    75  Bt2u 0.137 0.270 0.380 0.400   -99  1.60  0.05  19.0  12.0   -99 0.020   6.0   -99  10.0   -99 
    89  Bt2l 0.060 0.162 0.270 0.100   -99  1.60  0.02   7.0   4.0   -99 0.010   6.0   -99   3.0   -99 
   102  Bt3u 0.060 0.162 0.270 0.000   -99  1.60  0.02   7.0   5.0   -99 0.010   6.0   -99   3.0   -99 
   120  Bt3u 0.060 0.162 0.270 0.000   -99  1.60  0.02   7.0   5.0   -99 0.010   6.0   -99   3.0   -99 
   
*IBMZ910214  Gainesville S      180. Millhopper Fine Sand                              
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Gainesville USA            29.630 -82.370 Loamy,silic,hyperth Arenic Paleudult              
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
 -99    0.18   2.0  0.65   60.  1.00  0.92 IB001 IB001 IB001 -99  
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    5.       0.026 0.096 0.345 1.000 7.400  1.66  0.67   1.7   0.9   0.0 -99.0  7.00 -99.0 20.00 -99.0
   15.       0.025 0.105 0.345 1.000 7.400  1.66  0.67   1.7   0.9   0.0 -99.0  7.00 -99.0 -99.0 -99.0
   30.       0.075 0.120 0.345 0.700  14.8  1.66  0.17   2.4   2.6   0.0 -99.0  7.00 -99.0 -99.0 -99.0
   45.       0.025 0.086 0.345 0.300 3.700  1.66  0.17   2.4   2.6   0.0 -99.0  7.00 -99.0 -99.0 -99.0
   60.       0.025 0.072 0.345 0.300 3.700  1.66  0.17   2.4   2.6   0.0 -99.0  7.00 -99.0 -99.0 -99.0
   90.       0.028 0.072 0.345 0.100 3.700  1.66  0.17   2.4   2.6   0.0 -99.0  7.00 -99.0 -99.0 -99.0
  120.       0.028 0.080 0.345 0.100 0.100  1.66  0.18   7.7   3.1   0.0 -99.0  7.00 -99.0 -99.0 -99.0
  150.       0.029 0.090 0.345 0.050 0.100  1.66  0.15   7.7   3.1   0.0 -99.0  7.00 -99.0 -99.0 -99.0
  180.       0.029 0.090 0.345 0.050 0.100  1.66  0.10   7.7   3.1   0.0 -99.0  7.00 -99.0 -99.0 -99.0   

*UFBG760002  SCS         S        71 Lauderhill Muck
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 EREC        USA            26.400  80.400 euic, hyperthermic Lithic Haplosaprist
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
    BL  0.09   6.0  0.25  61.0  1.00  1.00 IB001 SA001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    20   Oap 0.150 0.250 0.500 1.000 21.00  0.35 45.00   5.0   5.0   0.0 3.500   6.2   -99 211.0   -99 
    45   Oa2 0.150 0.250 0.500 0.522 21.00  0.35 46.00   5.0   5.0   0.0 3.500   6.3   -99 210.0   -99 
    60   Oa3 0.150 0.250 0.500 0.330 21.00  0.35 40.00   5.0   5.0   0.0 3.500   6.6   -99 141.0   -99 
    71   -99 0.026 0.058 0.402 0.010  0.00  1.53  0.00   0.0   0.0  80.0   -99   8.4   -99   -99   -99 
@  SLB  SLPX  SLPT  SLPO CACO3  SLAL  SLFE  SLMN  SLBS  SLPA  SLPB  SLKE  SLMG  SLNA  SLSU  SLEC  SLCA
    20  96.0 210.0 187.0  0.27  2.30  2.30   -99   0.1   -99   -99  0.08  0.13  0.00 13.30   -99  0.27 
    45  56.0 183.0 155.0  0.12  2.01  2.30   -99   -99   -99   -99  0.03  0.05  0.00 15.30   -99  0.12 
    60  40.0 150.3 100.0  0.11  1.25  2.30   -99   -99   -99   -99  0.03  0.04  0.00 14.10   -99  0.11 
    71   0.1   0.1   0.1  0.09  0.09  0.90  0.09   0.1  0.09  0.09  0.09  0.09  0.00 90.09  0.09  0.10 

*IBGB910015  SCS-Desire  -99     180 Millhopper Fine Sand
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Gainesville USA            29.630 -82.370 Loamy,silic,hyperth Gross. Paleudults
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.18   5.0  0.50  66.0  1.00  0.92 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.023 0.086 0.230 1.000  7.40  1.47  0.90   0.9  11.8   -99   -99   5.3   -99   -99   -99 
    15   -99 0.023 0.086 0.230 1.000  7.40  1.47  0.69   0.9  11.8   -99   -99   5.4   -99   -99   -99 
    30   -99 0.023 0.086 0.230 0.900 15.80  1.41  0.28   4.6   6.4   -99   -99   5.7   -99   -99   -99 
    45   -99 0.023 0.086 0.230 0.700 28.00  1.43  0.20   5.8   5.4   -99   -99   5.8   -99   -99   -99 
    60   -99 0.023 0.086 0.230 0.500 28.00  1.43  0.20   5.8   5.4   -99   -99   5.8   -99   -99   -99 
    90   -99 0.021 0.076 0.230 0.200 27.60  1.52  0.09   9.6   4.2   -99   -99   5.9   -99   -99   -99 
   120   -99 0.020 0.076 0.230 0.100 17.50  1.52  0.03   9.6   4.2   -99   -99   5.9   -99   -99   -99 
   150   -99 0.027 0.130 0.230 0.050  0.30  1.46  0.03   8.3   3.6   -99   -99   5.9   -99   -99   -99 
   180   -99 0.070 0.258 0.360 0.000  0.10  1.46  0.03   8.3   3.6   -99   -99   5.9   -99   -99   -99 

*UFGA010700  SCS         S       180 Candler
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Gainesville USA            29.630 -82.370 Hyperthermic uncoated
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
     Y  0.17   6.0  0.85  64.0  1.00  0.95 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.045 0.091 0.412 1.000 21.00  1.49  0.48   1.3   1.3   -99   -99   5.5   -99   -99   -99 
    15   -99 0.045 0.091 0.412 1.000 21.00  1.49  0.48   1.3   1.3   -99   -99   5.5   -99   -99   -99 
    30   -99 0.037 0.076 0.404 0.638 21.00  1.52  0.18   1.3   1.3   -99   -99   5.3   -99   -99   -99 
    45   -99 0.034 0.071 0.415 0.472 21.00  1.49  0.09   1.3   1.3   -99   -99   5.1   -99   -99   -99 
    60   -99 0.034 0.071 0.415 0.350 21.00  1.49  0.09   1.3   1.3   -99   -99   5.1   -99   -99   -99 
    90   -99 0.043 0.081 0.412 0.223 21.00  1.50  0.09   3.0   1.2   -99   -99   5.2   -99   -99   -99 
   120   -99 0.042 0.078 0.412 0.122 21.00  1.50  0.02   3.0   1.2   -99   -99   5.3   -99   -99   -99 
   150   -99 0.047 0.084 0.416 0.067 21.00  1.49  0.02   4.0   1.5   -99   -99   5.2   -99   -99   -99 
   180   -99 0.047 0.084 0.416 0.037 21.00  1.49  0.02   4.0   1.5   -99   -99   5.2   -99   -99   -99 

*UFGA017000  SCS         S       180 Tavares
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Gainesville USA            29.630 -82.370 Sand uncoated
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
     Y  0.17   6.0  0.40  64.0  1.00  0.95 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.072 0.142 0.364 1.000 21.00  1.60  1.41   1.5   1.5   -99   -99   4.2   -99   -99   -99 
    15   -99 0.072 0.142 0.364 1.000 21.00  1.60  1.41   1.5   1.5   -99   -99   4.2   -99   -99   -99 
    30   -99 0.051 0.102 0.371 0.638 21.00  1.60  0.66   1.5   1.2   -99   -99   4.3   -99   -99   -99 
    45   -99 0.044 0.091 0.373 0.472 21.00  1.60  0.44   1.3   1.5   -99   -99   4.3   -99   -99   -99 
    60   -99 0.045 0.092 0.347 0.350 21.00  1.67  0.44   1.5   1.5   -99   -99   4.2   -99   -99   -99 
    90   -99 0.038 0.078 0.350 0.223 21.00  1.67  0.18   1.5   1.5   -99   -99   4.2   -99   -99   -99 
   120   -99 0.046 0.086 0.364 0.122 21.00  1.63  0.18   3.0   1.2   -99   -99   4.3   -99   -99   -99 
   150   -99 0.048 0.087 0.365 0.067 21.00  1.63  0.08   4.0   1.5   -99   -99   4.3   -99   -99   -99 
   180   -99 0.052 0.091 0.358 0.037 21.00  1.65  0.05   5.0   1.5   -99   -99   4.4   -99   -99   -99 

*UFGA950002  Alachua county       203  Millhopper Fine Sand (Compacted layer) 
@SITE        COUNTRY          LAT     LONG SCS FAMILY 
-99         -99              -99      -99 Loamy, siliceous hyperthermic Grossarenic Paleudults 
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE 
  -99  0.18   5.0  0.50  66.0  1.00  0.92 IB001 IB001 IB001 
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC 
   15   -99 0.024 0.161 0.487 1.000   9.3  1.36  .73    2.1   6.3     0   .08   5.2   -99   5.4   -99 
   33   -99 0.016 0.104 0.407 0.300  24.4  1.57  .34    2.3   4.1     0   .03   5.4   -99   2.9   -99 
   89   -99 0.011 0.113 0.408 0.000  .001  1.57  .19    2.4   3.3     0   .02   5.2   -99   1.9   -99 
  147   -99 0.021 0.080 0.385 0.050  32.9  1.63  .07    1.9   3.3     9   .01   5.3   -99   1.1   -99 
  157   -99 0.022 0.101 0.404 0.000  19.8  1.58  .07    4.1   3.9     0   .01   5.2   -99   2.0   -99 
  175   -99 0.070 0.196 0.374 0.000   4.0  1.66  .10   16.4   6.1     0   .01   4.9   -99   6.0   -99 
  203   -99 0.105 0.248 0.351 0.000   0.9  1.72  .12   28.6   8.6     0   .01   5.0   -99  10.4   -99 

*UFGA950003  Alachua cou S       203 Millhopper Fine Sand (Mulched)
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 99          -99               -99    -99  oamy, siliceous hyperthermic Grossarenic Paleudult
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.18   5.0  0.50  66.0  1.00  0.92  B001  B001  B001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     2   -99 0.024 0.161 0.487 1.000  0.01  1.36  0.73   2.1   6.3   0.0 0.080   5.2   -99   5.4   -99 
    15   -99 0.024 0.161 0.487 1.000  9.30  1.36  0.73   2.1   6.3   0.0 0.080   5.2   -99   5.4   -99 
    33   -99 0.016 0.104 0.407 0.300 24.40  1.55  0.34   2.3   4.1   0.0 0.030   5.4   -99   2.9   -99 
    89   -99 0.011 0.113 0.408 0.100 31.90  1.57  0.19   2.4   3.3   0.0 0.020   5.2   -99   1.9   -99 
   147   -99 0.021 0.080 0.385 0.050 32.90  1.63  0.07   1.9   3.3   9.0 0.010   5.3   -99   1.1   -99 
   157   -99 0.022 0.101 0.404 0.000 19.80  1.58  0.07   4.1   3.9   0.0 0.010   5.2   -99   2.0   -99 
   175   -99 0.070 0.196 0.374 0.000  4.00  1.66  0.10  16.4   6.1   0.0 0.010   4.9   -99   6.0   -99 
   203   -99 0.105 0.248 0.351 0.000  0.90  1.72  0.12  28.6   8.6   0.0 0.010   5.0   -99  10.4   -99 

*IBTM910017  SCS         -99     203 Orangeburg Sandy Loam
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Quincy      USA            30.600 -86.400 Loamy,silic,hyperth Gross. Paleud(17)
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.13   9.0  0.27  84.0  1.00  0.96 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.125 0.198 0.294 1.000   -99  1.49  1.73   -99   -99   -99   -99   -99   -99   -99   -99 
    15   -99 0.125 0.198 0.294 0.874   -99  1.49  1.73   -99   -99   -99   -99   -99   -99   -99   -99 
    25   -99 0.125 0.198 0.294 0.874   -99  1.49  1.73   -99   -99   -99   -99   -99   -99   -99   -99 
    34   -99 0.117 0.226 0.323 0.351   -99  1.41  0.40   -99   -99   -99   -99   -99   -99   -99   -99 
    43   -99 0.117 0.226 0.323 0.351   -99  1.41  0.40   -99   -99   -99   -99   -99   -99   -99   -99 
    53   -99 0.138 0.250 0.332 0.310   -99  1.44  0.20   -99   -99   -99   -99   -99   -99   -99   -99 
    64   -99 0.138 0.250 0.332 0.310   -99  1.44  0.20   -99   -99   -99   -99   -99   -99   -99   -99 
   102   -99 0.167 0.281 0.331 0.302   -99  1.57  0.14   -99   -99   -99   -99   -99   -99   -99   -99 
   145   -99 0.182 0.291 0.334 0.077   -99  1.59  0.16   -99   -99   -99   -99   -99   -99   -99   -99 
   175   -99 0.162 0.272 0.320 0.036   -99  1.61  0.09   -99   -99   -99   -99   -99   -99   -99   -99 
   203   -99 0.154 0.263 0.319 0.006   -99  1.58  0.03   -99   -99   -99   -99   -99   -99   -99   -99 

*IBPP910015  SCS         -99     180 Millhopper Fine Sand
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Gainesville USA            29.630 -82.370 Loamy,silic,hyperth Gross. Paleudults
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99  0.18   5.0  0.50  66.0  1.00  0.92 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99 0.023 0.086 0.230 1.000  7.40  1.36  0.90   -99   -99   -99   -99   5.3   -99   -99   -99
    15   -99 0.023 0.086 0.230 1.000  7.40  1.40  0.69   -99   -99   -99   -99   5.4   -99   -99   -99
    30   -99 0.023 0.086 0.230 0.498 15.80  1.46  0.28   -99   -99   -99   -99   5.7   -99   -99   -99
    60   -99 0.023 0.086 0.230 0.294 28.00  1.47  0.20   -99   -99   -99   -99   5.8   -99   -99   -99
    90   -99 0.021 0.076 0.230 0.380 27.60  1.43  0.09   -99   -99   -99   -99   5.9   -99   -99   -99
   120   -99 0.020 0.076 0.230 0.066 17.50  1.48  0.03   -99   -99   -99   -99   5.9   -99   -99   -99
   150   -99 0.027 0.130 0.230 0.031  0.30  1.57  0.03   -99   -99   -99   -99   5.9   -99   -99   -99
   180   -99 0.070 0.258 0.360 0.015  0.10  1.79  0.03   -99   -99   -99   -99   5.9   -99   -99   -99

*IN00020001  SCS         CL      150 BLACK SOIL, JNKVV, INDORE,MP
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 JNKVV       INDIA         22.400   75.500 TYPIC HAPLUSTERT, FINE, MONTMORILLONITIC
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
 BN     0.11   6.0  0.50    70  1.00  0.88 IB001 IB001 IB001
@  SLB SLMH   SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC
     5 AP    0.177 0.400 0.602  1.00 -99.0  1.36  0.60   -99   -99   -99   -99   8.0 -99.0   -99
    15 AP    0.177 0.400 0.602  1.00 -99.0  1.36  0.60   -99   -99   -99   -99   8.0 -99.0   -99
    30 AP    0.177 0.400 0.602  1.00 -99.0  1.36  0.60   -99   -99   -99   -99   8.0 -99.0   -99
    60 A     0.177 0.390 0.603  1.00 -99.0  1.36  0.64   -99   -99   -99   -99   8.1 -99.0   -99
    90 B     0.177 0.390 0.603  1.00 -99.0  1.36  0.64   -99   -99   -99   -99   8.1 -99.0   -99
   120 BS    0.177 0.390 0.602  1.00 -99.0  1.36  0.60   -99   -99   -99   -99   8.1 -99.0   -99
   150 BS    0.177 0.390 0.603  1.00 -99.0  1.36  0.60   -99   -99   -99   -99   8.1 -99.0   -99
   160 BS    0.177 0.390 0.603  1.00 -99.0  1.36  0.60   -99   -99   -99   -99   8.1 -99.0   -99   

*AUTA810001  CSIRO       CL      200 Red sodosol
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Tatura      Australia       36.27  145.14 Red brown earth
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
     R   .13     6    .1    80     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    20     1   .23  .455  .505     1   .23   1.3  1.03    35    30   -99   -99   8.2   -99   -99   -99
    40     2   .24   .49   .54  .549   .23   1.3   .86    35    30   -99   -99   8.5   -99   -99   -99
    60     3   .24   .49   .54  .368   .23  1.29   .77    35    30   -99   -99   8.6   -99   -99   -99
    80     4   .25    .5   .55  .247   .23  1.31   .65    35    30   -99   -99   8.6   -99   -99   -99
   100     5   .27   .42   .47  .165   .23  1.35   .54    35    30   -99   -99   7.4   -99   -99   -99
   120     6   .31  .385  .435  .111   .23  1.36   .48    35    30   -99   -99   5.3   -99   -99   -99
   140     7   .38   .43   .48  .074   .23  1.36   .43    35    30   -99   -99     5   -99   -99   -99
   160     8   .38  .405  .455   .05   .23  1.36   .43    35    30   -99   -99     5   -99   -99   -99
   180     9   .38  .405  .455  .033   .23  1.36   .43    35    30   -99   -99     5   -99   -99   -99
   200    10   .38   .38   .43  .022   .23  1.36   .43    35    30   -99   -99     5   -99   -99   -99