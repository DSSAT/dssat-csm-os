*SOILS: General DSSAT Soil Input File
! DSSAT v4.6; 07/01/2013
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
! Dalgliesh, N.P., and M.A. Foale. 1998. Soil Matters – monitoring soil water and nitrogen
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

*IB00000001  IBSNAT      SIC      210  DEFAULT - DEEP SILTY CLAY
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Generic     Generic           -99     -99 Generic
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .11     6    .3    85     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99  .228  .385  .481     1   -99   1.3  1.75    50    45     0   .17   6.5   -99   -99   -99
    15   -99  .228  .385  .481     1   -99   1.3  1.75    50    45     0   .17   6.5   -99   -99   -99
    30   -99  .249  .406  .482  .638   -99   1.3   1.6    50    45     0   .17   6.5   -99   -99   -99
    45   -99  .249  .406  .465  .472   -99  1.35  1.45    50    45     0   .14   6.5   -99   -99   -99
    60   -99  .249  .406  .465   .35   -99  1.35  1.45    50    45     0   .14   6.5   -99   -99   -99
    90   -99  .308  .456  .468  .223   -99  1.35   1.1    50    45     0   .11   6.5   -99   -99   -99
   120   -99  .207  .341  .452  .122   -99   1.4   .65    50    45     0   .06   6.5   -99   -99   -99
   150   -99  .243  .365  .455  .067   -99   1.4    .3    50    45     0   .03   6.5   -99   -99   -99
   180   -99  .259  .361  .457  .037   -99   1.4    .1    50    45     0   .01   6.5   -99   -99   -99
   210   -99  .259  .361  .457   .02   -99   1.4   .01    50    45     0     0   6.5   -99   -99   -99

*IB00000002  IBSNAT      SIC      150  DEFAULT - MEDIUM SILTY CLAY
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Generic     Generic           -99     -99 Generic
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .11     6    .2    87     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99  .228  .385  .463     1   -99  1.35  1.74    50    45     0   .17   6.5   -99   -99   -99
    15   -99  .228  .385  .463     1   -99  1.35  1.74    50    45     0   .17   6.5   -99   -99   -99
    30   -99  .228  .385  .459  .638   -99  1.36  1.66    50    45     0   .17   6.5   -99   -99   -99
    45   -99  .249  .406  .461  .472   -99  1.36  1.45    50    45     0   .14   6.5   -99   -99   -99
    60   -99  .249  .406  .461   .35   -99  1.36  1.45    50    45     0   .14   6.5   -99   -99   -99
    90   -99  .308  .449   .46  .223   -99  1.37  1.09    50    45     0   .11   6.5   -99   -99   -99
   120   -99  .207  .341   .46  .122   -99  1.38   .65    50    45     0   .06   6.5   -99   -99   -99
   150   -99  .256  .373  .463  .067   -99  1.38   .29    50    45     0   .03   6.5   -99   -99   -99

*IB00000003  IBSNAT      SIC       60  DEFAULT - SHALLOW SILTY CLAY
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Generic     Generic           -99     -99 Generic
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .11     6    .1    89     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99  .228  .385  .463     1   -99  1.35  1.74    50    45     0   .17   6.5   -99   -99   -99
    15   -99  .228  .385  .463     1   -99  1.35  1.74    50    45     0   .17   6.5   -99   -99   -99
    30   -99  .228  .385  .459  .638   -99  1.36  1.66    50    45     0   .17   6.5   -99   -99   -99
    45   -99  .249  .406  .461  .472   -99  1.36  1.45    50    45     0   .14   6.5   -99   -99   -99
    60   -99  .249  .406  .461   .35   -99  1.36  1.45    50    45     0   .14   6.5   -99   -99   -99

*IB00000004  IBSNAT      SIL      210  DEFAULT - DEEP SILTY LOAM
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Generic     Generic           -99     -99 Generic
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .12     6    .4    77     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99   .11  .227   .45     1   -99  1.37  1.16    10    60     0   .12   6.5   -99   -99   -99
    15   -99   .11  .227   .45     1   -99  1.37  1.16    10    60     0   .12   6.5   -99   -99   -99
    30   -99  .103  .201  .451  .638   -99  1.37   1.1    10    60     0   .11   6.5   -99   -99   -99
    45   -99  .099  .193  .452  .472   -99  1.37   .97    10    60     0    .1   6.5   -99   -99   -99
    60   -99  .099  .193  .452   .35   -99  1.37   .97    10    60     0    .1   6.5   -99   -99   -99
    90   -99  .088  .173   .45  .223   -99  1.38   .72    10    60     0   .07   6.5   -99   -99   -99
   120   -99  .079  .165  .452  .122   -99  1.38   .43    10    60     0   .04   6.5   -99   -99   -99
   150   -99  .086  .178   .45  .067   -99  1.39    .2    10    60     0   .02   6.5   -99   -99   -99
   190   -99  .072  .174  .451  .033   -99  1.39   .06    10    60     0   .01   6.5   -99   -99   -99
   210   -99  .072  .174  .452  .018   -99  1.39   .01    10    60     0     0   6.5   -99   -99   -99

*IB00000005  IBSNAT      SIL      150  DEFAULT - MEDIUM SILTY LOAM
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Generic     Generic           -99     -99 Generic
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .12     6    .3    79     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99   .11  .227   .45     1   -99  1.37  1.16    10    60     0   .12   6.5   -99   -99   -99
    15   -99   .11  .227   .45     1   -99  1.37  1.16    10    60     0   .12   6.5   -99   -99   -99
    30   -99  .103  .201  .451  .638   -99  1.37   1.1    10    60     0   .11   6.5   -99   -99   -99
    45   -99  .099  .193  .452  .472   -99  1.37   .97    10    60     0    .1   6.5   -99   -99   -99
    60   -99  .099  .193  .452   .35   -99  1.37   .97    10    60     0    .1   6.5   -99   -99   -99
    90   -99  .088  .173   .45  .223   -99  1.38   .72    10    60     0   .07   6.5   -99   -99   -99
   120   -99  .079  .165  .452  .122   -99  1.38   .43    10    60     0   .04   6.5   -99   -99   -99
   150   -99  .086  .178   .45  .067   -99  1.39    .2    10    60     0   .02   6.5   -99   -99   -99

*IB00000006  IBSNAT      SIL       60  DEFAULT - SHALLOW SILTY LOAM
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Generic     Generic           -99     -99 Generic
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .12     6    .2    81     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99   .11  .227   .45     1   -99  1.37  1.16    10    60     0   .12   6.5   -99   -99   -99
    15   -99   .11  .227   .45     1   -99  1.37  1.16    10    60     0   .12   6.5   -99   -99   -99
    30   -99  .103  .201  .451  .638   -99  1.37   1.1    10    60     0   .11   6.5   -99   -99   -99
    45   -99  .099  .193  .452  .472   -99  1.37   .97    10    60     0    .1   6.5   -99   -99   -99
    60   -99  .099  .193  .452   .35   -99  1.37   .97    10    60     0    .1   6.5   -99   -99   -99

*IB00000007  IBSNAT      SL       210  DEFAULT - DEEP SANDY LOAM
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Generic     Generic           -99     -99 Generic
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .13     6    .5    68     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99  .052  .176  .359     1   -99  1.61    .7    10    30     0   .07   6.5   -99   -99   -99
    15   -99  .052  .176  .359     1   -99  1.61    .7    10    30     0   .07   6.5   -99   -99   -99
    30   -99  .052  .176  .359  .638   -99  1.61   .66    10    30     0   .07   6.5   -99   -99   -99
    45   -99  .073  .192   .36  .472   -99  1.61   .58    10    30     0   .06   6.5   -99   -99   -99
    60   -99  .073  .192   .36   .35   -99  1.61   .58    10    30     0   .06   6.5   -99   -99   -99
    90   -99  .128  .232  .361  .223   -99  1.61   .43    10    30     0   .04   6.5   -99   -99   -99
   120   -99  .143  .243  .359  .122   -99  1.62   .26    10    30     0   .03   6.5   -99   -99   -99
   150   -99  .138  .243   .36  .067   -99  1.62   .12    10    30     0   .01   6.5   -99   -99   -99
   180   -99  .138  .244  .361  .037   -99  1.62   .04    10    30     0     0   6.5   -99   -99   -99
   210   -99  .138  .244  .361   .02   -99  1.62   .01    10    30     0     0   6.5   -99   -99   -99

*IB00000008  IBSNAT      SL       150  DEFAULT - MEDIUM SANDY LOAM
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Generic     Generic           -99     -99 Generic
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .13     6    .5    70     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99  .052  .176  .359     1   -99  1.61    .7    10    30     0   .07   6.5   -99   -99   -99
    15   -99  .052  .176  .359     1   -99  1.61    .7    10    30     0   .07   6.5   -99   -99   -99
    30   -99  .052  .176  .359  .638   -99  1.61   .66    10    30     0   .07   6.5   -99   -99   -99
    45   -99  .073  .192   .36  .472   -99  1.61   .58    10    30     0   .06   6.5   -99   -99   -99
    60   -99  .073  .192   .36   .35   -99  1.61   .58    10    30     0   .06   6.5   -99   -99   -99
    90   -99  .128  .232  .361  .223   -99  1.61   .43    10    30     0   .04   6.5   -99   -99   -99
   120   -99  .143  .243  .359  .122   -99  1.62   .26    10    30     0   .03   6.5   -99   -99   -99
   150   -99  .138  .243   .36  .067   -99  1.62   .12    10    30     0   .01   6.5   -99   -99   -99

*IB00000009  IBSNAT      SL        60  DEFAULT - SHALLOW SANDY LOAM
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Generic     Generic           -99     -99 Generic
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .13     6    .4    74     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99  .052  .176  .359     1   -99  1.61    .7    10    30     0   .07   6.5   -99   -99   -99
    15   -99  .052  .176  .359     1   -99  1.61    .7    10    30     0   .07   6.5   -99   -99   -99
    30   -99  .052  .176  .359  .638   -99  1.61   .66    10    30     0   .07   6.5   -99   -99   -99
    45   -99  .073  .192   .36  .472   -99  1.61   .58    10    30     0   .06   6.5   -99   -99   -99
    60   -99  .073  .192   .36   .35   -99  1.61   .58    10    30     0   .06   6.5   -99   -99   -99

*IB00000010  IBSNAT      S        210  DEFAULT - DEEP SAND
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Generic     Generic           -99     -99 Generic
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .15     4    .6    65     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99  .025  .096  .345     1   -99  1.66   .29     5     5     0   .03   6.5   -99   -99   -99
    15   -99  .025  .096  .345     1   -99  1.66   .29     5     5     0   .03   6.5   -99   -99   -99
    30   -99  .023  .097  .345  .638   -99  1.66   .28     5     5     0   .03   6.5   -99   -99   -99
    45   -99  .023  .097  .345  .472   -99  1.66   .24     5     5     0   .02   6.5   -99   -99   -99
    60   -99  .023  .097  .345   .35   -99  1.66   .24     5     5     0   .02   6.5   -99   -99   -99
    90   -99  .018  .091  .346  .223   -99  1.66   .18     5     5     0   .02   6.5   -99   -99   -99
   120   -99   .02  .095  .346  .122   -99  1.66   .11     5     5     0   .01   6.5   -99   -99   -99
   150   -99  .021  .091  .347  .067   -99  1.66   .05     5     5     0     0   6.5   -99   -99   -99
   180   -99  .021  .091  .347  .037   -99  1.66   .01     5     5     0     0   6.5   -99   -99   -99
   210   -99  .021  .091  .347   .02   -99  1.66     0     5     5     0     0   6.5   -99   -99   -99

*IB00000011  IBSNAT         S     150  DEFAULT - MEDIUM SAND
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Generic     Generic           -99     -99 Generic
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .15     4    .6    70     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99  .025  .096  .345     1   -99  1.66   .29     5     5     0   .03   6.5   -99   -99   -99
    15   -99  .025  .096  .345     1   -99  1.66   .29     5     5     0   .03   6.5   -99   -99   -99
    30   -99  .023  .097  .345  .638   -99  1.66   .28     5     5     0   .03   6.5   -99   -99   -99
    45   -99  .023  .097  .345  .472   -99  1.66   .24     5     5     0   .02   6.5   -99   -99   -99
    60   -99  .023  .097  .345   .35   -99  1.66   .24     5     5     0   .02   6.5   -99   -99   -99
    90   -99  .018  .091  .346  .223   -99  1.66   .18     5     5     0   .02   6.5   -99   -99   -99
   120   -99   .02  .095  .346  .122   -99  1.66   .11     5     5     0   .01   6.5   -99   -99   -99
   150   -99  .021  .091  .347  .067   -99  1.66   .05     5     5     0     0   6.5   -99   -99   -99

*IB00000012  IBSNAT      S         60  DEFAULT - SHALLOW SAND
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Generic     Generic           -99     -99 Generic
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .15     4    .4    75     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99  .025  .096  .345     1   -99  1.66   .29     5     5     0   .03   6.5   -99   -99   -99
    15   -99  .025  .096  .345     1   -99  1.66   .29     5     5     0   .03   6.5   -99   -99   -99
    30   -99  .023  .097  .345  .638   -99  1.66   .28     5     5     0   .03   6.5   -99   -99   -99
    45   -99  .023  .097  .345  .472   -99  1.66   .24     5     5     0   .02   6.5   -99   -99   -99
    60   -99  .023  .097  .345   .35   -99  1.66   .24     5     5     0   .02   6.5   -99   -99   -99
!======================================================================================================
! End of Generic soil profiles
!======================================================================================================

*IBSB910015  SCS         S        180  Millhopper Fine Sand
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Gainesville USA             29.63  -82.37 Loamy,silic,hyperth Gross. Paleudults
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .18     5    .5    66     1   .92 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99  .023  .086   .23     1   7.4  1.36    .9    .9  11.8   -99   -99   5.3   -99   -99   -99
    15   -99  .023  .086   .23     1   7.4   1.4   .69    .9  11.8   -99   -99   5.4   -99   -99   -99
    30   -99  .023  .086   .23  .498  15.8  1.46   .28   4.6   6.4   -99   -99   5.7   -99   -99   -99
    45   -99  .023  .086   .23  .294    28  1.46    .2   5.8   5.4   -99   -99   5.8   -99   -99   -99
    60   -99  .023  .086   .23  .294    28  1.47    .2   5.8   5.4   -99   -99   5.8   -99   -99   -99
    90   -99  .021  .076   .23   .38  27.6  1.43   .09   9.6   4.2   -99   -99   5.9   -99   -99   -99
   120   -99   .02  .076   .23  .133  17.5  1.48   .03   9.6   4.2   -99   -99   5.9   -99   -99   -99
   150   -99  .027   .13   .23  .062    .3  1.57   .03   8.3   3.6   -99   -99   5.9   -99   -99   -99
   180   -99   .07  .258   .36  .031    .1  1.79   .03   8.3   3.6   -99   -99   5.9   -99   -99   -99

*IBSG910015  SCS         S        180  Millhopper Fine Sand
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Gainesville USA             29.63  -82.37 Loamy,silic,hyperth Gross. Paleudults
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .18     5    .5    66     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99  .023  .086   .23     1   7.4  1.36    .9    .9  11.8   -99   -99   5.3   -99   -99   -99
    15   -99  .023  .086   .23     1   7.4   1.4   .69    .9  11.8   -99   -99   5.4   -99   -99   -99
    30   -99  .023  .086   .23  .498  15.8  1.46   .28   4.6   6.4   -99   -99   5.7   -99   -99   -99
    45   -99  .023  .086   .23  .294    28  1.46    .2   5.8   5.4   -99   -99   5.8   -99   -99   -99
    60   -99  .023  .086   .23  .294    28  1.47    .2   5.8   5.4   -99   -99   5.8   -99   -99   -99
    90   -99  .021  .076   .23   .38  27.6  1.43   .09   9.6   4.2   -99   -99   5.9   -99   -99   -99
   120   -99   .02  .076   .23  .133  17.5  1.48   .03   9.6   4.2   -99   -99   5.9   -99   -99   -99
   150   -99  .027   .13   .23  .062    .3  1.57   .03   8.3   3.6   -99   -99   5.9   -99   -99   -99
   180   -99   .07  .258   .36  .031    .1  1.79   .03   8.3   3.6   -99   -99   5.9   -99   -99   -99

*IBSB910009  IBSNAT      -99      136  Norfolk Sandy Clay Loam (
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 -99         USA               -99     -99 Fine loamy,silic.,therm. Typic Paleudults
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .14     3   .23    60     1   .95 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99  .042  .169  .392     1   -99     0     0   -99   -99   -99   -99   -99   -99   -99   -99
    15   -99  .042  .169  .392     1   -99     0     0   -99   -99   -99   -99   -99   -99   -99   -99
    25   -99  .042  .169  .392  .779   -99     0     0   -99   -99   -99   -99   -99   -99   -99   -99
    33   -99  .044  .177  .358  .349   -99     0     0   -99   -99   -99   -99   -99   -99   -99   -99
    46   -99  .056  .165  .396  .209   -99     0     0   -99   -99   -99   -99   -99   -99   -99   -99
    61   -99   .15  .291  .377   .07   -99     0     0   -99   -99   -99   -99   -99   -99   -99   -99
    76   -99   .15  .291  .377   .07   -99     0     0   -99   -99   -99   -99   -99   -99   -99   -99
   106   -99   .15  .291  .377  .017   -99     0     0   -99   -99   -99   -99   -99   -99   -99   -99
   136   -99   .15  .291  .377     0   -99     0     0   -99   -99   -99   -99   -99   -99   -99   -99

*IBSB910017  SCS         -99      203  Orangeburg Sandy Loam
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Quincy      USA              30.6   -86.4 Loamy,silic,hyperth Gross. Paleud(17)
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .13     9   .27    84     1   .97 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99  .125  .198  .294     1   -99  1.49  1.73   -99   -99   -99   -99   -99   -99   -99   -99
    15   -99  .125  .198  .294  .874   -99  1.49  1.73   -99   -99   -99   -99   -99   -99   -99   -99
    25   -99  .125  .198  .294  .874   -99  1.49  1.73   -99   -99   -99   -99   -99   -99   -99   -99
    34   -99  .117  .226  .323  .351   -99  1.41    .4   -99   -99   -99   -99   -99   -99   -99   -99
    43   -99  .117  .226  .323  .351   -99  1.41    .4   -99   -99   -99   -99   -99   -99   -99   -99
    53   -99  .138   .25  .332   .31   -99  1.44    .2   -99   -99   -99   -99   -99   -99   -99   -99
    64   -99  .138   .25  .332   .31   -99  1.44    .2   -99   -99   -99   -99   -99   -99   -99   -99
   102   -99  .167  .281  .331  .302   -99  1.57   .14   -99   -99   -99   -99   -99   -99   -99   -99
   145   -99  .182  .291  .334  .077   -99  1.59   .16   -99   -99   -99   -99   -99   -99   -99   -99
   175   -99  .162  .272   .32  .036   -99  1.61   .09   -99   -99   -99   -99   -99   -99   -99   -99
   203   -99  .154  .263  .319  .006   -99  1.58   .03   -99   -99   -99   -99   -99   -99   -99   -99

*IBSB910026  SCS         -99      180  Ida Silt Loam
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Castana     USA              42.2   -93.7 Ida Silt Loam
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .12     6    .3    60     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99  .135   .29  .485     1   -99     0     0   -99   -99   -99   -99   -99   -99   -99   -99
    15   -99  .135   .29  .485     1   -99     0     0   -99   -99   -99   -99   -99   -99   -99   -99
    30   -99  .135   .29  .485  .175   -99     0     0   -99   -99   -99   -99   -99   -99   -99   -99
    45   -99  .106  .228  .514  .138   -99     0     0   -99   -99   -99   -99   -99   -99   -99   -99
    60   -99  .106  .228  .514  .138   -99     0     0   -99   -99   -99   -99   -99   -99   -99   -99
    90   -99  .105  .254  .517  .188   -99     0     0   -99   -99   -99   -99   -99   -99   -99   -99
   120   -99  .133   .29  .507   .25   -99     0     0   -99   -99   -99   -99   -99   -99   -99   -99
   150   -99  .108  .283  .505  .213   -99     0     0   -99   -99   -99   -99   -99   -99   -99   -99
   180   -99  .108  .291  .542    .1   -99     0     0   -99   -99   -99   -99   -99   -99   -99   -99

*IBSB910027  Castana,mKJ SIL      225  Ida Silt Loam
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 -99         -99               -99     -99 Ida Silt Loam
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .12   9.8   .55    60     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99  .098   .26  .485     1  14.5  1.29     0  14.1  51.9   -99   -99   8.1     1   -99   -99
    15   -99  .098   .26  .485     1  14.5  1.29     0  14.1  51.9   -99   -99   8.1     1   -99   -99
    45   -99  .098  .248    .5  .257    16  1.24     0  11.5  53.3   -99   -99   8.1     1   -99   -99
    75   -99  .093   .23  .516  .263  23.1  1.22     0  10.2  57.7   -99   -99   8.3     1   -99   -99
   105   -99  .095  .235  .512    .3  23.1  1.23     0  13.5  50.9   -99   -99   8.3     1   -99   -99
   135   -99  .098  .235  .506    .3    16  1.23     0   8.4  55.4   -99   -99   8.4     1   -99   -99
   165   -99  .088  .223  .524    .3   9.7  1.24     0  10.6    51   -99   -99   8.4     1   -99   -99
   195   -99   .08   .22  .551    .3   8.2  1.25     0  10.3  53.1   -99   -99   8.3     1   -99   -99
   225   -99  .083  .223  .547    .3   8.7  1.25     0   9.6  52.8   -99   -99   8.3    .2   -99   -99
!SOIL FROM MIGUEL CALMON, MODIFIED BY KJB, SLIGHTLY LOWER LL, NEW WRL

*IBSB910055  Troup SCS   -99      190  TROUP FINE SAND
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Jackson CO  USA               -99     -99 TROUP, SCS JACKSON CO, GROSSARENIC PALEUDULT, L,
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .13   6.2    .6    76     1   .92 SA001 SA001 SA001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99  .033  .105  .314     1   -99  1.56   .64   -99   -99   -99   -99   5.8   -99   -99   -99
    15   -99  .033  .105  .314     1   -99  1.56   .64   -99   -99   -99   -99   5.8   -99   -99   -99
    30   -99  .031  .099  .313  .927   -99  1.56   .19   -99   -99   -99   -99   5.9   -99   -99   -99
    45   -99  .031  .099  .313  .229   -99  1.56   .19   -99   -99   -99   -99   5.9   -99   -99   -99
    60   -99  .031  .099  .313   .06   -99  1.56   .19   -99   -99   -99   -99   5.9   -99   -99   -99
    90   -99  .029  .092  .315  .016   -99   1.6   .05   -99   -99   -99   -99   5.4   -99   -99   -99
   120   -99  .029  .092  .315  .011   -99   1.6   .05   -99   -99   -99   -99   5.4   -99   -99   -99
   145   -99   .03  .096  .316  .006   -99   1.6   .04   -99   -99   -99   -99   5.1   -99   -99   -99
   167   -99  .047  .151  .357     0   -99  1.56    .1   -99   -99   -99   -99   5.2   -99   -99   -99
   190   -99  .047  .151  .357     0   -99  1.56    .1   -99   -99   -99   -99   5.2   -99   -99   -99

*IBPN910015  SCS         S        180  Millhopper Fine Sand
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Gainesville USA             29.63  -82.37 Loamy,silic,hyperth Gross. Paleudults
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .18     5    .5    66     1   .92 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99  .023  .086   .23     1   7.4  1.36    .9    .9  11.8   -99   -99   -99   -99   -99   -99
    15   -99  .023  .086   .23     1   7.4   1.4   .69    .9  11.8   -99   -99   -99   -99   -99   -99
    30   -99  .023  .086   .23   .55  15.8  1.46   .28   4.6   6.4   -99   -99   -99   -99   -99   -99
    45   -99  .023  .086   .23   .32    28  1.46    .2   5.8   5.4   -99   -99   -99   -99   -99   -99
    60   -99  .023  .086   .23   .32    28  1.47    .2   5.8   5.4   -99   -99   -99   -99   -99   -99
    90   -99  .021  .076   .23   .38  27.6  1.43   .09   9.6   4.2   -99   -99   -99   -99   -99   -99
   120   -99   .02  .076   .23    .4  17.5  1.48   .03   9.6   4.2   -99   -99   -99   -99   -99   -99
   150   -99  .027   .13   .23    .3    .3  1.57   .03   8.3   3.6   -99   -99   -99   -99   -99   -99
   180   -99   .07  .258   .36    .2    .1  1.79   .03   8.3   3.6   -99   -99   -99   -99   -99   -99

*IBPN910016  Gainesville -99      180  Lake Fine Sand
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 -99         -99               -99     -99 Hyperthermic, coated Typic Quartzipsamments
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .18     5    .5    66     1   .92 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99   .02  .089   .23     1   -99     0    .9   -99   -99   -99   -99   -99   -99   -99   -99
    15   -99   .02  .089   .23     1   -99     0   .69   -99   -99   -99   -99   -99   -99   -99   -99
    30   -99  .019  .068   .23   .55   -99     0   .28   -99   -99   -99   -99   -99   -99   -99   -99
    45   -99  .026  .075   .23   .32   -99     0   .28   -99   -99   -99   -99   -99   -99   -99   -99
    60   -99  .026  .075   .23   .32   -99     0    .2   -99   -99   -99   -99   -99   -99   -99   -99
    90   -99  .025  .073   .23   .38   -99     0   .09   -99   -99   -99   -99   -99   -99   -99   -99
   120   -99  .022  .069   .23    .4   -99     0   .03   -99   -99   -99   -99   -99   -99   -99   -99
   150   -99  .023  .072   .23    .3   -99     0   .03   -99   -99   -99   -99   -99   -99   -99   -99
   180   -99  .035  .085   .23    .2   -99     0   .03   -99   -99   -99   -99   -99   -99   -99   -99

*IBPN910024  Marianna, F -99      229  Norfolk Sandy Loam
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Marianna,FL USA               -99     -99 F-loamy,silic,thermic Typ Paleudults
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .18     6    .1    77     1   .92 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99  .061  .145  .312     1   -99  1.38  1.29   -99   -99   -99   -99   5.5   -99   -99   -99
    10   -99  .061  .145  .312     1   -99  1.38  1.29   -99   -99   -99   -99   5.5   -99   -99   -99
    20   -99   .05  .141  .302  .775   -99  1.42   .47   -99   -99   -99   -99   5.5   -99   -99   -99
    38   -99  .056  .165   .27  .448   -99  1.52   .28   -99   -99   -99   -99   5.5   -99   -99   -99
    58   -99  .198  .304  .359    .3   -99  1.48   .25   -99   -99   -99   -99   5.1   -99   -99   -99
    79   -99  .198  .304  .359    .3   -99  1.48   .25   -99   -99   -99   -99   5.1   -99   -99   -99
    95   -99  .197  .305  .335    .1   -99  1.64   .12   -99   -99   -99   -99   5.1   -99   -99   -99
   112   -99  .197  .305  .335    .1   -99  1.64   .12   -99   -99   -99   -99   5.1   -99   -99   -99
   129   -99  .184  .292  .332    .1   -99  1.61   .06   -99   -99   -99   -99     5   -99   -99   -99
   147   -99  .184  .292  .332    .1   -99  1.61   .06   -99   -99   -99   -99     5   -99   -99   -99
   173   -99   .21  .318  .339   .02   -99  1.67   .05   -99   -99   -99   -99     5   -99   -99   -99
   201   -99  .227  .335   .35     0   -99  1.66   .06   -99   -99   -99   -99   4.9   -99   -99   -99
   229   -99  .227  .335   .35     0   -99  1.66   .06   -99   -99   -99   -99   4.9   -99   -99   -99

*IBPN910025  NRCS-USDA   L        173  Norfolk Sandy Loam
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Mobile      US              30.92 -88.034 Unclassified
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
    BN   .13     6    .6    61     1   .92 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    25    Ap  .101  .249  .498     1  1.32  1.24  1.16   8.6  41.5   -99   -99     6   -99   7.9   -99
    66   Bt1  .173  .303  .439  .403   .23  1.42    .3  27.8  36.9   -99   -99   6.5   -99   8.6   -99
    91   Bt2  .203  .329  .444  .208   .23  1.41   .13  34.7  35.3   -99   -99   4.7   -99  12.1   -99
   127   Bt3  .212  .333  .437  .113   .23  1.43   .07  36.8    33   -99   -99   4.9   -99  13.9   -99
   142   Bt4  .214  .339  .444  .068   .23  1.41   .13  36.8  34.1   -99   -99   4.9   -99  13.6   -99
   173   Bt5  .205  .316  .422  .043   .23  1.47   .07  35.5  28.1   -99   -99   4.7   -99  12.8   -99

*IBPN910040  GreenAcres, -99      180  Millhopper Fine Sand
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 -99         -99               -99     -99 Loamy,silic,hyperth Gross. Paleudults
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .18     5    .5    66     1    .9 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99  .023  .086   .23     1   7.4  1.36    .9   -99   -99   -99   -99   -99   -99   -99   -99
    15   -99  .023  .086   .23     1   7.4   1.4   .69   -99   -99   -99   -99   -99   -99   -99   -99
    30   -99  .023  .086   .23   .55  15.8  1.46   .28   -99   -99   -99   -99   -99   -99   -99   -99
    45   -99  .023  .086   .23   .32    28  1.46   .28   -99   -99   -99   -99   -99   -99   -99   -99
    60   -99  .023  .086   .23   .32    28  1.47    .2   -99   -99   -99   -99   -99   -99   -99   -99
    90   -99  .021  .076   .23   .38  27.6  1.43   .09   -99   -99   -99   -99   -99   -99   -99   -99
   120   -99   .02  .076   .23    .4  17.5  1.48   .03   -99   -99   -99   -99   -99   -99   -99   -99
   150   -99  .027   .13   .23    .3    .3  1.57   .03   -99   -99   -99   -99   -99   -99   -99   -99
   180   -99   .07  .258   .36    .2    .1  1.79   .03   -99   -99   -99   -99   -99   -99   -99   -99

*IBMZ910013  IBSNAT      -99      110  Waipio
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Waipio,HI   USA               -99     -99 Clayey, kaolinitic, isohyperth, Tropeptic Eutrusto
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .14     5    .6    60     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99   .22   .35   .55     1   -99     1  2.27   -99   -99   -99   -99   6.3   -99   -99   -99
    15   -99   .23   .35   .55     1   -99     1  2.27   -99   -99   -99   -99   6.3   -99   -99   -99
    30   -99   .24   .35   .55    .8   -99  1.05   1.1   -99   -99   -99   -99   5.8   -99   -99   -99
    50   -99   .25   .37   .48    .4   -99  1.17  1.41   -99   -99   -99   -99   5.8   -99   -99   -99
    70   -99   .26   .38   .46    .2   -99  1.22   .59   -99   -99   -99   -99     6   -99   -99   -99
    90   -99   .25   .38   .46   .05   -99  1.22   .36   -99   -99   -99   -99     6   -99   -99   -99
   110   -99   .26    .4   .48   .02   -99  1.17   .27   -99   -99   -99   -99     6   -99   -99   -99
!WDB Original DSSAT35

*IBMZ913514  Gainesville -99      180  Millhopper Fine Sand
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Gainesville USA             29.63  -82.37 Loamy,silic,hyperth Arenic Paleudult
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .18     2   .65    60     1    .8 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99  .026  .096   .23     1   -99   1.3     2   -99   -99   -99   -99   -99   -99    20   -99
    15   -99  .025  .086   .23     1   -99   1.3     1   -99   -99   -99   -99   -99   -99   -99   -99
    30   -99  .025  .086   .23    .8   -99   1.4     1   -99   -99   -99   -99   -99   -99   -99   -99
    60   -99  .025  .086   .23    .2   -99   1.4    .5   -99   -99   -99   -99   -99   -99   -99   -99
    90   -99  .028   .09   .23    .1   -99  1.45    .1   -99   -99   -99   -99   -99   -99   -99   -99
   120   -99  .028   .09   .23   .05   -99  1.45    .1   -99   -99   -99   -99   -99   -99   -99   -99
   150   -99  .029   .13   .23  .002   -99  1.45   .04   -99   -99   -99   -99   -99   -99   -99   -99
   180   -99   .07  .258   .36     0   -99   1.2   .24   -99   -99   -99   -99   -99   -99   -99   -99
!WDB Modified for DSSAT40

*IBMZ910014  Gainesville -99      180  Millhopper Fine Sand
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Gainesville USA             29.63  -82.37 Loamy,silic,hyperth Arenic Paleudult
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .18     2   .65    60     1   .92 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99  .026  .096   .23     1   -99   1.3     2   -99   -99   -99   -99   -99   -99    20   -99
    15   -99  .025  .086   .23     1   -99   1.3     1   -99   -99   -99   -99   -99   -99   -99   -99
    30   -99  .025  .086   .23    .7   -99   1.4     1   -99   -99   -99   -99   -99   -99   -99   -99
    60   -99  .025  .086   .23    .3   -99   1.4    .5   -99   -99   -99   -99   -99   -99   -99   -99
    90   -99  .028   .09   .23   .05   -99  1.45    .1   -99   -99   -99   -99   -99   -99   -99   -99
   120   -99  .028   .09   .23   .03   -99  1.45    .1   -99   -99   -99   -99   -99   -99   -99   -99
   150   -99  .029   .13   .23  .002   -99  1.45   .04   -99   -99   -99   -99   -99   -99   -99   -99
   180   -99   .07  .258   .36     0   -99   1.2   .24   -99   -99   -99   -99   -99   -99   -99   -99
!GH For Maize Envirotron experiment (SLPF=0.72) 

*IBMZ910114  Gainesville -99      180  Millhopper Fine Sand
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Gainesville USA             29.63  -82.37 Loamy,silic,hyperth Arenic Paleudult
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .18     2   .65    60     1   .72 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99  .026  .096   .23     1   -99   1.3     2   -99   -99   -99   -99   -99   -99    20   -99
    15   -99  .025  .086   .23     1   -99   1.3     1   -99   -99   -99   -99   -99   -99   -99   -99
    30   -99  .025  .086   .23    .7   -99   1.4     1   -99   -99   -99   -99   -99   -99   -99   -99
    60   -99  .025  .086   .23    .3   -99   1.4    .5   -99   -99   -99   -99   -99   -99   -99   -99
    90   -99  .028   .09   .23   .05   -99  1.45    .1   -99   -99   -99   -99   -99   -99   -99   -99
   120   -99  .028   .09   .23   .03   -99  1.45    .1   -99   -99   -99   -99   -99   -99   -99   -99
   150   -99  .029   .13   .23  .002   -99  1.45   .04   -99   -99   -99   -99   -99   -99   -99   -99
   180   -99   .07  .258   .36     0   -99   1.2   .24   -99   -99   -99   -99   -99   -99   -99   -99

*IBMZ910023  IBSNAT      -99      151  Norfolk Loamy Sand
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Florence,SC USA               -99     -99 Norfolk Loamy Sand
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .14     5    .6    60     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    10   -99  .075   .21   .25     1   -99  1.55    .3   -99   -99   -99   -99   -99   -99   -99   -99
    20   -99  .075   .21   .25     1   -99  1.55    .3   -99   -99   -99   -99   -99   -99   -99   -99
    41   -99    .1   .24   .29    .8   -99  1.67   .17   -99   -99   -99   -99   -99   -99   -99   -99
    71   -99   .21   .31   .35    .4   -99  1.54   .01   -99   -99   -99   -99   -99   -99   -99   -99
   101   -99   .21   .32   .36    .1   -99  1.54   .01   -99   -99   -99   -99   -99   -99   -99   -99
   126   -99   .18   .28   .32    .1   -99  1.68   .01   -99   -99   -99   -99   -99   -99   -99   -99
   151   -99   .18   .28   .32    .1   -99  1.74   .01   -99   -99   -99   -99   -99   -99   -99   -99

*IBMZ910032  87EBDRL     -99      120  Clayey, Oxidic, Isothermic Haplustox
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 -99         -99               -99     -99 QUINTANA
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .13   7.1    .5    76     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    15   -99  .165  .338  .417     1   -99     1  1.16   -99   -99   -99   -99     5   -99   -99   -99
    30   -99  .219  .339  .378  .697   -99   .94   .43   -99   -99   -99   -99     5   -99   -99   -99
    45   -99  .219   .32  .396  .303   -99    .9   .31   -99   -99   -99   -99     5   -99   -99   -99
    60   -99    .2   .31  .417  .515   -99   .89   .18   -99   -99   -99   -99     5   -99   -99   -99
    75   -99  .195  .303  .436  .242   -99   .93   .24   -99   -99   -99   -99     5   -99   -99   -99
    90   -99  .195  .305  .436  .182   -99   .97   .24   -99   -99   -99   -99     5   -99   -99   -99
   105   -99    .2  .309  .436  .061   -99   .89   .27   -99   -99   -99   -99     5   -99   -99   -99
   120   -99  .205  .309  .436   .03   -99    .9    .2   -99   -99   -99   -99     5   -99   -99   -99

*IBWH980018  SCS         -99      180  Haynie
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Manhattan   USA               -99     -99 Coarse-silty, mixed,calcareous,mesic Typ Udifluven
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .14     5    .6    60     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    15   -99  .072  .225  .275     1   -99  1.15   .61   -99   -99   -99   -99   -99   -99   -99   -99
    30   -99   .07   .24   .29    .7   -99  1.16   .61   -99   -99   -99   -99   -99   -99   -99   -99
    60   -99   .04  .154  .194    .2   -99  1.21   .59   -99   -99   -99   -99   -99   -99   -99   -99
    90   -99  .032  .091  .141   .05   -99  1.23   .29   -99   -99   -99   -99   -99   -99   -99   -99
   120   -99  .032  .087  .137   .03   -99  1.31   .24   -99   -99   -99   -99   -99   -99   -99   -99
   150   -99  .032  .087  .137   .01   -99  1.31    .2   -99   -99   -99   -99   -99   -99   -99   -99
   180   -99  .032  .087  .137   .01   -99  1.31    .2   -99   -99   -99   -99   -99   -99   -99   -99

*IBWH980019  Swift       -99      150  Wood Mountain Loam
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Swift_Curr  Canada            -99     -99 Orthic Brown Chernozem
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .12     8    .5    60     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99  .096   .23   .44     1   -99     0   1.1   -99   -99   -99   -99   -99   -99   -99   -99
    15   -99  .096   .23   .44     1   -99     0   1.1   -99   -99   -99   -99   -99   -99   -99   -99
    30   -99  .112   .25   .44     1   -99     0   .61   -99   -99   -99   -99   -99   -99   -99   -99
    45   -99  .094   .22   .44    .8   -99     0   .61   -99   -99   -99   -99   -99   -99   -99   -99
    60   -99  .103   .22   .44    .7   -99     0   .59   -99   -99   -99   -99   -99   -99   -99   -99
    75   -99  .103   .22   .44    .6   -99     0   .15   -99   -99   -99   -99   -99   -99   -99   -99
    90   -99  .102   .25   .44    .4   -99     0    .1   -99   -99   -99   -99   -99   -99   -99   -99
   120   -99  .102   .25   .44    .3   -99     0    .1   -99   -99   -99   -99   -99   -99   -99   -99
   150   -99  .102   .25   .44    .2   -99     0    .1   -99   -99   -99   -99   -99   -99   -99   -99

*IBWH980020  IBSNAT      -99      155  Rothamsted
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Rothamsted  England           -99     -99 Rothamsted
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .14     6    .5    60     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    10   -99   .11   .28   .33     1   -99   1.1  1.16   -99   -99   -99   -99   -99   -99   -99   -99
    25   -99   .15   .32   .42    .9   -99   1.2     1   -99   -99   -99   -99   -99   -99   -99   -99
    45   -99   .22   .37   .42    .7   -99  1.25   .68   -99   -99   -99   -99   -99   -99   -99   -99
    65   -99   .22   .37   .42    .5   -99  1.25   .26   -99   -99   -99   -99   -99   -99   -99   -99
    95   -99   .22   .37   .42    .2   -99  1.25   .25   -99   -99   -99   -99   -99   -99   -99   -99
   125   -99   .22   .37   .42    .1   -99  1.25    .2   -99   -99   -99   -99   -99   -99   -99   -99
   155   -99   .22   .37   .42   .05   -99  1.25    .2   -99   -99   -99   -99   -99   -99   -99   -99

*IBBN910015  SCS         S        180  Millhopper Fine Sand
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Gainesville USA             29.63  -82.37 Loamy,silic,hyperth Gross. Paleudults
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .18     5    .5    66     1   .92 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99  .023  .086   .23     1   7.4  1.47    .9    .9  11.8   -99   -99   5.3   -99   -99   -99
    15   -99  .023  .086   .23     1   7.4  1.47   .69    .9  11.8   -99   -99   5.4   -99   -99   -99
    30   -99  .023  .086   .23    .9  15.8  1.41   .28   4.6   6.4   -99   -99   5.7   -99   -99   -99
    45   -99  .023  .086   .23    .7    28  1.43    .2   5.8   5.4   -99   -99   5.8   -99   -99   -99
    60   -99  .023  .086   .23    .5    28  1.43    .2   5.8   5.4   -99   -99   5.8   -99   -99   -99
    90   -99  .021  .076   .23    .2  27.6  1.52   .09   9.6   4.2   -99   -99   5.9   -99   -99   -99
   120   -99   .02  .076   .23    .1  17.5  1.52   .03   9.6   4.2   -99   -99   5.9   -99   -99   -99
   150   -99  .027   .13   .23   .05    .3  1.46   .03   8.3   3.6   -99   -99   5.9   -99   -99   -99
   180   -99   .07  .258   .36     0    .1  1.46   .03   8.3   3.6   -99   -99   5.9   -99   -99   -99

*IBBN910016  SCS         S        180  Millhoper Fine Sand, W-2, Kendrick clay closer to
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Gainesville USA             29.63  -82.37 Loamy,silic,hyperth Gross. Paleudults
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .18     5    .5    66     1    .9 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99  .023  .086   .23     1   7.4  1.47    .9    .9  11.8   -99   -99   5.3   -99   -99   -99
    15   -99  .023  .086   .23     1   7.4  1.47   .69    .9  11.8   -99   -99   5.4   -99   -99   -99
    30   -99  .023  .086   .23    .9  15.8  1.41   .28   4.6   6.4   -99   -99   5.7   -99   -99   -99
    45   -99  .023  .086   .23    .7    28  1.43    .2   5.8   5.4   -99   -99   5.8   -99   -99   -99
    60   -99  .023  .086   .23    .5    28  1.43    .2   5.8   5.4   -99   -99   5.8   -99   -99   -99
    90   -99  .027   .13   .23    .2  27.6  1.52   .09   9.6   4.2   -99   -99   5.9   -99   -99   -99
   120   -99  .027   .13   .23    .1  17.5  1.52   .03   9.6   4.2   -99   -99   5.9   -99   -99   -99
   150   -99  .027   .13   .23   .05    .3  1.46   .03   8.3   3.6   -99   -99   5.9   -99   -99   -99
   180   -99   .07  .258   .36     0    .1  1.46   .03   8.3   3.6   -99   -99   5.9   -99   -99   -99

*IBBN910030  CIAT        -99      209  CIAT-Plot M3
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Palmira(M3) Colombia         3.48   73.37 Fine-silty,mixed,isohyperth.Aquic Hapludoll
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .09    11    .4    84     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99  .204   .34  .392     1   -99  1.45  2.19   -99   -99   -99   -99   6.9   -99   -99   -99
    15   -99  .204   .34  .392     1   -99  1.45  2.19   -99   -99   -99   -99   6.9   -99   -99   -99
    25   -99  .209  .345   .39   .75   -99  1.45  1.21   -99   -99   -99   -99   7.2   -99   -99   -99
    35   -99  .209  .345   .39    .5   -99  1.45  1.21   -99   -99   -99   -99   7.2   -99   -99   -99
    50   -99  .198  .335   .39   .35   -99  1.49   .53   -99   -99   -99   -99     8   -99   -99   -99
    65   -99  .185  .323  .395    .2   -99  1.58    .2   -99   -99   -99   -99   8.2   -99   -99   -99
    80   -99  .185  .323  .395   .15   -99  1.58    .2   -99   -99   -99   -99   8.2   -99   -99   -99
    99   -99  .201  .328  .408    .1   -99  1.54    .1   -99   -99   -99   -99   8.1   -99   -99   -99
   122   -99  .198  .325   .41   .05   -99  1.58   .09   -99   -99   -99   -99   8.2   -99   -99   -99
   137   -99  .159  .288  .399     0   -99   1.5   .09   -99   -99   -99   -99   8.3   -99   -99   -99
   159   -99   .11  .242  .402     0   -99  1.69    .1   -99   -99   -99   -99   8.3   -99   -99   -99
   184   -99  .047  .177  .351     0   -99  1.59   .08   -99   -99   -99   -99     8   -99   -99   -99
   209   -99   .05  .193   .41     0   -99  1.45   .12   -99   -99   -99   -99   8.5   -99   -99   -99

*IBBN910038  ICTA        -99      150  San_Fernando,Quezada
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 S_Fernando  Guatemala         -99     -99 LOAMY-SKELETAL,MIXED,ISOHYPERTHERMIC TYPIC USTIFLU
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .13  11.3    .4    80     1    .8 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99  .191  .299  .369     1   -99  1.27  2.25   -99   -99   -99   -99     7   -99   -99   -99
    15   -99  .191  .299  .369     1   -99  1.27  2.25   -99   -99   -99   -99     7   -99   -99   -99
    45   -99   .16  .273  .352   .75   -99  1.11   .87   -99   -99   -99   -99   6.7   -99   -99   -99
    72   -99  .244  .361  .376    .5   -99   1.3   1.2   -99   -99   -99   -99   6.7   -99   -99   -99
    95   -99   .17  .285  .355   .35   -99  1.13   .67   -99   -99   -99   -99   6.9   -99   -99   -99
   120   -99  .095  .207  .332    .2   -99  1.02   .36   -99   -99   -99   -99     7   -99   -99   -99
   150   -99  .163  .266  .363   .15   -99  1.28    .6   -99   -99   -99   -99   7.1   -99   -99   -99

*GAPN930001  SCS         SL       178  Faceville
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Plains,GA   USA                32  -84.33 CLAYEY, KAOLINITIC THERMIC, TYPIC PALEUDULTS
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
    BN   .13   9.3    .6    76     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    18    AP  .092  .189  .327     1   -99  1.45   .97  16.1  12.8    22   -99   6.5   -99   2.8   -99
    28    B1  .129  .238  .335   .75   -99  1.61   .54    22  11.6     6   -99   5.7   -99   3.1   -99
    61    B2   .18  .293  .347   .75   -99  1.52   .24  33.1  14.9     3   -99   5.9   -99   3.9   -99
    76    B2  .192  .307  .351   .75   -99  1.48   .27  35.6  17.7     2   -99   5.7   -99   4.4   -99
   114    B2  .245  .364  .379   .35   -99  1.39   .15  47.6  24.9     2   -99   5.1   -99   6.4   -99
   152    B2  .283    .4  .415    .2   -99  1.38   .19  55.6  18.9     0   -99   4.6   -99    15   -99
   178    B3  .336  .455   .47   .15   -99  1.26   .11  67.6  21.2     0   -99   4.6   -99  36.8   -99

*IBRI910001  IBSNAT      -99       50  Andaqueptic Haplaquoll
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 -99         -99               -99     -99 UNKNOWN
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .13   7.5     0    87     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99  .289  .415    .7     1   -99   .75  2.45   -99   -99   -99   -99   6.2   -99    40   -99
    20   -99  .289  .415   .65   .85   -99    .9  2.45   -99   -99   -99   -99   6.2   -99   -99   -99
    35   -99  .289  .415    .6    .2   -99    .9  1.45   -99   -99   -99   -99   6.4   -99   -99   -99
    50   -99  .289  .415    .6   .05   -99   .88  1.45   -99   -99   -99   -99   6.4   -99   -99   -99

*IBRI910002  IBSNAT      -99       50  Vertic Tropaquept
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 -99         -99               -99     -99 UNKNOWN
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .13   7.5     0    87   1.5     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99  .258  .389   .65     1   -99   .85   1.3   -99   -99   -99   -99   -99   -99    40   -99
    20   -99  .258  .389    .6   .85   -99    .9   1.2   -99   -99   -99   -99   -99   -99   -99   -99
    35   -99  .267  .396   .55    .2   -99    .9   .65   -99   -99   -99   -99   -99   -99   -99   -99
    50   -99  .267  .396   .55   .05   -99    .9    .5   -99   -99   -99   -99   -99   -99   -99   -99

*IBRI910023  IRRI        -99      135  Upland Soil
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 -99         -99               -99     -99 FINE, MIXED,ISOHYPERTHERMIC, ANDAQUEPTIC HAPLAQUOL
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .13    12    .6    67    .9     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99   .28  .397  .412     1   -99     1  2.45   -99   -99   -99   -99     6   -99    25   -99
    15   -99   .28  .397  .412     1   -99     1  2.45   -99   -99   -99   -99     6   -99   -99   -99
    29   -99  .275  .392  .407     1   -99     1  1.48   -99   -99   -99   -99   6.5   -99   -99   -99
    38   -99  .198  .264  .412    .2   -99   .93   .52   -99   -99   -99   -99     7   -99   -99   -99
    47   -99  .198  .264  .412    .2   -99   .93   .52   -99   -99   -99   -99     7   -99   -99   -99
    58   -99  .174  .235  .373    .1   -99   .78   .31   -99   -99   -99   -99   6.9   -99   -99   -99
    69   -99  .174  .235  .373   .05   -99   .78   .31   -99   -99   -99   -99   6.9   -99   -99   -99
    96   -99  .152  .213  .366     0   -99   .74   .25   -99   -99   -99   -99     7   -99   -99   -99
   123   -99  .152  .213  .366     0   -99   .74   .25   -99   -99   -99   -99     7   -99   -99   -99
   135   -99  .172  .238  .364     0   -99   .55   .06   -99   -99   -99   -99     7   -99   -99   -99

*IBRI910024  IBSNAT      -99       51  Suphan Lowland
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 -99         -99               -99     -99 Fine, Mixed, Non-acid, Isohyper., Aeric Tropaquept
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99    .1   7.5     0    60     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99  .245  .332  .398     1   -99  1.41  1.81   -99   -99   -99   -99   -99   -99     5   -99
     8   -99  .245  .332  .398     1   -99  1.41  1.81   -99   -99   -99   -99   -99   -99   -99   -99
    19   -99   .21  .341  .402  .361   -99  1.49   .79   -99   -99   -99   -99   -99   -99   -99   -99
    28   -99  .242  .369  .404    .2   -99  1.39   .54   -99   -99   -99   -99   -99   -99   -99   -99
    38   -99  .242  .369  .404    .1   -99  1.39   .54   -99   -99   -99   -99   -99   -99   -99   -99
    51   -99  .218  .344  .388    .1   -99  1.34   .32   -99   -99   -99   -99   -99   -99   -99   -99

*IBWM860001  AWAII       CL       120  Kawaihapai Gravelly Clay Loam
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 -99         USA                -9      -9 FINE,MIXED,ISOHYPERHERMIC CUMULIC HAPLUSTOLLS
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
 BROWN   .13    12    .6    90     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    20   Ap1  .211  .327  .366     1   -99   1.3     1    33    33   -99   -99   6.7   -99   -99   -99
    35   Ap2  .173  .289   .37     1   -99   1.3    .9    25    36   -99   -99   6.6   -99   -99   -99
    50    C1   .17  .274  .369     1   -99   1.3    .2    26     6   -99   -99   7.1   -99   -99   -99
    60    C2  .177  .277   .44     1   -99   1.2    .6    26     6   -99   -99   6.7   -99   -99   -99
    95    C3  .169  .276  .488     1   -99   1.1    .3    26     6   -99   -99   6.8   -99   -99   -99
   120    C4  .178  .279  .535     1   -99     1    .8    26     6   -99   -99   7.1   -99   -99   -99

*CCPA000030  Veltkamp    -99      209  Silty Clay Loam
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Palmira     Colombia         3.52   76.35 Fine-silty, mixed, isohyperth. Aquic Hapludoll
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .09    11    .4    84     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99  .204   .34  .392     1   -99  1.45  2.19   -99   -99   -99   -99   6.9   -99   -99   -99
    15   -99  .204   .34  .392     1   -99  1.45  2.19   -99   -99   -99   -99   6.9   -99   -99   -99
    25   -99  .209  .345   .39   .75   -99  1.45  1.21   -99   -99   -99   -99   7.2   -99   -99   -99
    35   -99  .209  .345   .39    .5   -99  1.45  1.21   -99   -99   -99   -99   7.2   -99   -99   -99
    50   -99  .198  .335   .39   .35   -99  1.49   .53   -99   -99   -99   -99     8   -99   -99   -99
    65   -99  .185  .323  .395    .2   -99  1.58    .2   -99   -99   -99   -99   8.2   -99   -99   -99
    80   -99  .185  .323  .395   .15   -99  1.58    .2   -99   -99   -99   -99   8.2   -99   -99   -99
    99   -99  .201  .328  .408    .1   -99  1.54    .1   -99   -99   -99   -99   8.1   -99   -99   -99
   122   -99  .198  .325   .41   .05   -99  1.58   .09   -99   -99   -99   -99   8.2   -99   -99   -99
   137   -99  .159  .288  .399     0   -99   1.5   .09   -99   -99   -99   -99   8.3   -99   -99   -99
   159   -99   .11  .242  .402     0   -99  1.69    .1   -99   -99   -99   -99   8.3   -99   -99   -99
   184   -99  .047  .177  .351     0   -99  1.59   .08   -99   -99   -99   -99     8   -99   -99   -99
   209   -99   .05  .193   .41     0   -99  1.45   .12   -99   -99   -99   -99   8.5   -99   -99   -99

*CCQU000033  Connor      -99      260  Clay
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Quilichao   Colombia          3.1   76.51 Fine,Kaolinitic,Isohyperth. Oxic Dystropept
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .09   6.8    .6    76     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99  .351   .47  .485     1   -99  1.17   2.9   -99   -99   -99   -99   -99   -99   -99   -99
    15   -99  .354  .473  .488     1   -99  1.21  2.91   -99   -99   -99   -99   -99   -99   -99   -99
    28   -99  .374  .491  .506   .75   -99  1.19  2.45   -99   -99   -99   -99   -99   -99   -99   -99
    44   -99  .395   .51  .525   .75   -99  1.13  1.48   -99   -99   -99   -99   -99   -99   -99   -99
    65   -99  .415  .527  .542    .5   -99  1.07   .76   -99   -99   -99   -99   -99   -99   -99   -99
    96   -99  .427  .536  .551    .5   -99  1.08   .36   -99   -99   -99   -99   -99   -99   -99   -99
   122   -99  .437  .549  .564   .25   -99  1.01   .23   -99   -99   -99   -99   -99   -99   -99   -99
   150   -99  .441  .553  .568   .25   -99     1   .15   -99   -99   -99   -99   -99   -99   -99   -99
   178   -99  .428  .541  .556  .125   -99   .99   .14   -99   -99   -99   -99   -99   -99   -99   -99
   196   -99  .371  .488  .503  .125   -99   1.1   .08   -99   -99   -99   -99   -99   -99   -99   -99
   230   -99  .371  .488  .503   .05   -99   1.1   .08   -99   -99   -99   -99   -99   -99   -99   -99
   260   -99  .371  .488  .503   .05   -99   1.1   .08   -99   -99   -99   -99   -99   -99   -99   -99

*IBML910001  IBSNAT      -99      210  Niger Sandy Soil
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 -99         Niger             -99     -99 NIGER SANDY SOIL
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .14    10    .8    76   .35    .7 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99  .031  .097   .32     1   -99  1.61    .5   -99   -99   -99   -99   5.5   -99   -99   -99
    15   -99  .031  .097   .32     1   -99  1.61    .5   -99   -99   -99   -99   5.5   -99   -99   -99
    30   -99  .037  .119  .331    .8   -99  1.66    .5   -99   -99   -99   -99   5.5   -99   -99   -99
    45   -99  .037  .117  .332    .6   -99  1.54    .5   -99   -99   -99   -99   5.5   -99   -99   -99
    60   -99  .037  .117  .332    .4   -99  1.54    .5   -99   -99   -99   -99   5.5   -99   -99   -99
    90   -99   .04  .126  .331   .01   -99  1.57    .5   -99   -99   -99   -99   5.5   -99   -99   -99
   120   -99  .037  .117  .331   .01   -99  1.55    .5   -99   -99   -99   -99   5.5   -99   -99   -99
   150   -99  .037  .119   .33   .01   -99  1.64    .5   -99   -99   -99   -99   5.5   -99   -99   -99
   180   -99  .037  .119  .331   .01   -99  1.58    .5   -99   -99   -99   -99   5.5   -99   -99   -99
   210   -99  .034  .108  .324   .01   -99   1.6    .5   -99   -99   -99   -99   5.5   -99   -99   -99

*IBML910083  IBSNAT      -99      156  Medium Alfisol
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 -99         -99               -99     -99 MEDIUM ALFISOL
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .13     6    .5    60     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    22   -99   .06   .18   .24     1   -99  1.51    .7   -99   -99   -99   -99   6.5   -99   -99   -99
    52   -99  .085  .195  .255    .7   -99  1.51   .66   -99   -99   -99   -99   6.5   -99   -99   -99
    82   -99   .15   .21   .27    .3   -99  1.51   .58   -99   -99   -99   -99   6.5   -99   -99   -99
   112   -99   .16    .2   .26    .1   -99  1.51   .43   -99   -99   -99   -99   6.5   -99   -99   -99
   127   -99  .173    .2   .26   .02   -99  1.52   .26   -99   -99   -99   -99   6.5   -99   -99   -99
   156   -99  .193    .2   .26   .01   -99  1.52   .12   -99   -99   -99   -99   6.5   -99   -99   -99

*IBSG910010  IBSNAT      -99      210  Miller Clay Loam
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 College STN USA               -99     -99 MILLER CLAY LOAM
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .13     7   .28    78     1     0 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    10   -99   .14   .27   .33     1   -99   -99   -99   -99   -99   -99   -99   -99   -99   -99   -99
    30   -99   .14   .27   .33    .7   -99   -99   -99   -99   -99   -99   -99   -99   -99   -99   -99
    60   -99   .14   .27   .33    .5   -99   -99   -99   -99   -99   -99   -99   -99   -99   -99   -99
    90   -99   .14   .27   .33    .3   -99   -99   -99   -99   -99   -99   -99   -99   -99   -99   -99
   120   -99   .14   .27   .33    .1   -99   -99   -99   -99   -99   -99   -99   -99   -99   -99   -99
   150   -99   .14   .27   .33   .05   -99   -99   -99   -99   -99   -99   -99   -99   -99   -99   -99
   180   -99   .14   .27   .33   .05   -99   -99   -99   -99   -99   -99   -99   -99   -99   -99   -99
   210   -99   .14   .27   .33  .025   -99   -99   -99   -99   -99   -99   -99   -99   -99   -99   -99

*IBSG910011  IBSNAT      -99      130  Weakly self-mulching grey medium to heavy clay
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 -99         -99               -99     -99 HEAVY CLAY
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .13     6    .5    60     1    .8  IB00  IB00  IB00
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99  .154  .231  .414     1   -99   1.1     2   -99   -99   -99   -99   -99   -99   -99   -99
    20   -99  .153  .228   .29    .9   -99   1.3     1   -99   -99   -99   -99   -99   -99   -99   -99
    30   -99  .164  .231  .273    .8   -99   1.4    .9   -99   -99   -99   -99   -99   -99   -99   -99
    40   -99  .166  .228  .277    .7   -99   1.5    .8   -99   -99   -99   -99   -99   -99   -99   -99
    50   -99  .164  .241  .296    .6   -99   1.5    .6   -99   -99   -99   -99   -99   -99   -99   -99
    70   -99  .169  .236  .285    .5   -99   1.5    .5   -99   -99   -99   -99   -99   -99   -99   -99
    90   -99  .172  .241  .284    .4   -99   1.5    .3   -99   -99   -99   -99   -99   -99   -99   -99
   110   -99   .18  .256  .308    .2   -99   1.5    .3   -99   -99   -99   -99   -99   -99   -99   -99
   130   -99   .18  .256  .308   .05   -99   1.6    .2   -99   -99   -99   -99   -99   -99   -99   -99
!   -99  0.13   6.0  0.50  60.0  0.00  0.90 IB001 IB001 IB001

*IBSG910085  IBSNAT      -99      172  Patencheru
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Patancheru  India             -99     -99 ALFISOL Udic Rhodustalf, Patencheru Series
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .13     6    .3    80     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    10   -99   .08   .22   .31     1   -99  1.61   .85   -99   -99   -99   -99   -99   -99   -99   -99
    22   -99   .09   .22   .31    .9   -99  1.61   .55   -99   -99   -99   -99   -99   -99   -99   -99
    52   -99  .125  .245  .315    .7   -99  1.62   .51   -99   -99   -99   -99   -99   -99   -99   -99
    82   -99   .15   .23   .29    .5   -99  1.64   .63   -99   -99   -99   -99   -99   -99   -99   -99
   112   -99   .16   .24   .31    .1   -99  1.64    .1   -99   -99   -99   -99   -99   -99   -99   -99
   142   -99  .173   .24   .31   .05   -99  1.64    .1   -99   -99   -99   -99   -99   -99   -99   -99
   172   -99  .193   .25   .32   .03   -99  1.64    .1   -99   -99   -99   -99   -99   -99   -99   -99
!   -99  0.13   6.0  0.30  80.0  1.00  1.10 IB001 IB001 IB001   GH/2011 

*IBSG910086  IBSNAT      -99      202  Kasareddipalli
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 -99         -99               -99     -99 VERTISOL,Typic Pellustert ,Kasareddipalli Series
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .11     6   .25    85     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    10   -99   .27  .407  .467     1   -99  1.35   .96   -99   -99   -99   -99   -99   -99   -99   -99
    22   -99   .27  .407  .467    .9   -99  1.35   .96   -99   -99   -99   -99   -99   -99   -99   -99
    52   -99   .27  .436    .5    .7   -99  1.36   .69   -99   -99   -99   -99   -99   -99   -99   -99
    82   -99   .29   .44    .5    .3   -99  1.36   .68   -99   -99   -99   -99   -99   -99   -99   -99
   112   -99  .307   .44    .5    .1   -99  1.36    .6   -99   -99   -99   -99   -99   -99   -99   -99
   142   -99  .323   .44    .5   .02   -99  1.36    .6   -99   -99   -99   -99   -99   -99   -99   -99
   172   -99  .362   .44   .49   .01   -99  1.36    .6   -99   -99   -99   -99   -99   -99   -99   -99
   202   -99  .362   .43   .49   .01   -99  1.36    .6   -99   -99   -99   -99   -99   -99   -99   -99

*IBSG910092  IBSNAT      -99       53  Patencheru
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 -99         -99               -99     -99 ALFISOL Udic Rhodustalf, Patencheru Series
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .13     6    .5    60     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    10   -99   .06   .18   .24     1   -99  1.61   .55   -99   -99   -99   -99   -99   -99   -99   -99
    22   -99   .06   .18   .24    .9   -99  1.61   .55   -99   -99   -99   -99   -99   -99   -99   -99
    37   -99  .085  .195  .255    .7   -99  1.62   .51   -99   -99   -99   -99   -99   -99   -99   -99
    53   -99   .15   .21   .27    .3   -99  1.64   .63   -99   -99   -99   -99   -99   -99   -99   -99

*IBSG910093  IBSNAT      -99      135  Tindall Clay Loam
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 -99         -99               -99     -99 TINDALL CLAY LOAM, TIPPERA SERIES
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .13     6    .4    74     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99   .19  .289  .306     1   -99  1.57     1   -99   -99   -99   -99   6.9   -99   -99   -99
    15   -99  .218  .289  .306    .8   -99  1.57   .52   -99   -99   -99   -99   6.9   -99   -99   -99
    35   -99  .232  .328  .338    .4   -99  1.46   .63   -99   -99   -99   -99   6.9   -99   -99   -99
    55   -99  .235   .34  .361   .05   -99   1.4   .52   -99   -99   -99   -99   6.4   -99   -99   -99
    75   -99  .233  .329  .369  .005   -99  1.38    .4   -99   -99   -99   -99   6.5   -99   -99   -99
    95   -99  .234  .326  .376  .002   -99  1.39    .1   -99   -99   -99   -99   6.5   -99   -99   -99
   115   -99  .239  .341  .384  .001   -99  1.42    .1   -99   -99   -99   -99   6.5   -99   -99   -99
   135   -99  .251  .343  .393  .001   -99  1.47    .1   -99   -99   -99   -99   6.5   -99   -99   -99

*IBSG910096  IBSNAT      -99      100  Medium Black Calcarious Soil
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Pune        India             -99     -99 VERTISOL
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .13  11.6    .2    85     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99  .202  .345  .443     1   -99   .92   .51   -99   -99   -99   -99   8.4   -99   -99   -99
    10   -99  .202  .345  .443  .905   -99   .92   .51   -99   -99   -99   -99   8.4   -99   -99   -99
    25   -99  .204  .346  .433  .705   -99  1.01    .5   -99   -99   -99   -99   8.4   -99   -99   -99
    40   -99   .21  .359  .415  .522   -99  1.08    .5   -99   -99   -99   -99   8.4   -99   -99   -99
    55   -99   .21  .411   .42  .387   -99  1.07   .41   -99   -99   -99   -99   8.4   -99   -99   -99
    70   -99  .191  .425  .433  .287   -99   1.1   .39   -99   -99   -99   -99   8.4   -99   -99   -99
    85   -99  .191  .363  .401  .212   -99  1.13   .39   -99   -99   -99   -99   8.4   -99   -99   -99
   100   -99  .191  .412  .401  .212   -99  1.13   .39   -99   -99   -99   -99   8.4   -99   -99   -99

*IBBA980008  IBSNAT      -99      105  Typic Calciorthid
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Breda       Syria          35.209      40 Typic Calciorthid
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .14     6    .5    72     1     1 SA001 SA001 SA001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99   .17   .29   .46     1   -99   1.1   .63   -99   -99   -99   -99   -99   -99   -99   -99
    15   -99   .19   .31   .46     1   -99   1.1   .63   -99   -99   -99   -99   -99   -99   -99   -99
    30   -99   .19   .31   .46     1   -99   1.2    .5   -99   -99   -99   -99   -99   -99   -99   -99
    45   -99   .22   .33   .46   .75   -99   1.2    .3   -99   -99   -99   -99   -99   -99   -99   -99
    60   -99   .22   .33   .46   .65   -99   1.2   .25   -99   -99   -99   -99   -99   -99   -99   -99
    75   -99   .22   .33   .46   .45   -99   1.2   .19   -99   -99   -99   -99   -99   -99   -99   -99
    90   -99   .22   .33   .46   .35   -99   1.2   .13   -99   -99   -99   -99   -99   -99   -99   -99
   105   -99   .22   .33   .46    .1   -99   1.2   .13   -99   -99   -99   -99   -99   -99   -99   -99

*IBBA980060  IBSNAT      -99      180  Bozeman
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Bozeman     USA               -99     -99 Typic Haploboroll
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .12    12    .6    60     1     1 SA001 SA001 SA001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    10   -99   .28   .41   .49     1   -99   1.3  1.16   -99   -99   -99   -99   -99   -99   -99   -99
    30   -99   .28   .41   .49    .7   -99   1.3     1   -99   -99   -99   -99   -99   -99   -99   -99
    60   -99   .28   .41   .49    .3   -99   1.3   .91   -99   -99   -99   -99   -99   -99   -99   -99
    90   -99   .28   .41   .49    .1   -99   1.3   .61   -99   -99   -99   -99   -99   -99   -99   -99
   120   -99   .28   .41   .49   .05   -99   1.3   .59   -99   -99   -99   -99   -99   -99   -99   -99
   150   -99   .28   .41   .49  .025   -99   1.3   .29   -99   -99   -99   -99   -99   -99   -99   -99
   180   -99   .28   .41   .49  .005   -99   1.3   .24   -99   -99   -99   -99   -99   -99   -99   -99

*IBBA910009  IBSNAT      -99      180  Palexerollic Chromoxerert
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Jindiress   Syria             -99     -99 Palexerollic Chromoxerert
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .09     5    .3    60     1     1 SA001 SA001 SA001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99   .23   .36   .44    .8   -99   1.3   .65   -99   -99   -99   -99   -99   -99   -99   -99
    15   -99   .23   .36   .44     1   -99   1.3   .64   -99   -99   -99   -99   -99   -99   -99   -99
    30   -99   .21   .34   .42   .85   -99   1.3    .5   -99   -99   -99   -99   -99   -99   -99   -99
    45   -99   .21   .34   .42   .72   -99   1.3   .43   -99   -99   -99   -99   -99   -99   -99   -99
    60   -99   .21   .34   .42   .55   -99   1.3   .37   -99   -99   -99   -99   -99   -99   -99   -99
    75   -99   .21   .34   .42    .4   -99   1.3   .35   -99   -99   -99   -99   -99   -99   -99   -99
    90   -99   .22   .35   .43   .28   -99   1.3   .33   -99   -99   -99   -99   -99   -99   -99   -99
   105   -99   .22   .35   .43   .15   -99   1.3   .33   -99   -99   -99   -99   -99   -99   -99   -99
   120   -99   .22   .35   .43    .1   -99   1.3   .28   -99   -99   -99   -99   -99   -99   -99   -99
   135   -99   .22   .35   .43   .06   -99   1.3    .1   -99   -99   -99   -99   -99   -99   -99   -99
   150   -99   .22   .35   .43   .04   -99   1.3    .1   -99   -99   -99   -99   -99   -99   -99   -99
   165   -99   .22   .35   .43   .02   -99   1.3    .1   -99   -99   -99   -99   -99   -99   -99   -99
   180   -99   .22   .35   .43  .005   -99   1.3    .1   -99   -99   -99   -99   -99   -99   -99   -99

*IBBA910014  IBSNAT      -99      165  Tel Hadya
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Tel_Hadya   Syria             -99     -99 unknown
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .07     6    .2    60     1     1 SA001 SA001 SA001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99   .17    .3   .38  .925   -99   1.3   .42   -99   -99   -99   -99   -99   -99   -99   -99
    15   -99   .19   .32    .4     1   -99   1.3   .39   -99   -99   -99   -99   -99   -99   -99   -99
    30   -99   .19   .32    .4   .85   -99   1.3   .38   -99   -99   -99   -99   -99   -99   -99   -99
    45   -99    .2   .33   .41   .63   -99   1.3   .33   -99   -99   -99   -99   -99   -99   -99   -99
    60   -99    .2   .33   .41  .548   -99   1.3   .29   -99   -99   -99   -99   -99   -99   -99   -99
    75   -99   .19   .32    .4  .358   -99   1.3   .27   -99   -99   -99   -99   -99   -99   -99   -99
    90   -99   .19   .32    .4  .238   -99   1.3   .25   -99   -99   -99   -99   -99   -99   -99   -99
   105   -99   .19   .32    .4   .18   -99   1.3    .2   -99   -99   -99   -99   -99   -99   -99   -99
   120   -99   .19   .32    .4    .1   -99   1.3    .1   -99   -99   -99   -99   -99   -99   -99   -99
   135   -99   .17    .3   .38   .05   -99   1.3   .05   -99   -99   -99   -99   -99   -99   -99   -99
   150   -99   .17    .3   .38  .025   -99   1.3   .05   -99   -99   -99   -99   -99   -99   -99   -99
   165   -99   .17    .3   .38   .01   -99   1.3   .05   -99   -99   -99   -99   -99   -99   -99   -99

*IBBA910018  IBSNAT      -99      150  Khanasser
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Khanasser   Syria             -99     -99 unknown
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .07     6    .2    60     1     1 SA001 SA001 SA001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99    .2   .33   .35  .925   -99   1.3    .9   -99   -99   -99   -99   -99   -99   -99   -99
    15   -99    .2   .33   .35   .78   -99   1.3    .8   -99   -99   -99   -99   -99   -99   -99   -99
    30   -99    .2   .33   .35   .65   -99   1.3    .6   -99   -99   -99   -99   -99   -99   -99   -99
    45   -99    .2   .33   .35   .53   -99   1.3    .5   -99   -99   -99   -99   -99   -99   -99   -99
    60   -99    .2   .33   .35  .448   -99   1.3    .4   -99   -99   -99   -99   -99   -99   -99   -99
    75   -99    .2   .33   .35  .258   -99   1.3    .4   -99   -99   -99   -99   -99   -99   -99   -99
    90   -99    .2   .33   .35  .138   -99   1.3    .2   -99   -99   -99   -99   -99   -99   -99   -99
   105   -99    .2   .33   .35   .08   -99   1.3    .2   -99   -99   -99   -99   -99   -99   -99   -99
   120   -99    .2   .33   .35  .045   -99   1.3    .1   -99   -99   -99   -99   -99   -99   -99   -99
   135   -99    .2   .33   .35  .015   -99   1.3    .1   -99   -99   -99   -99   -99   -99   -99   -99
   150   -99    .2   .33   .35  .015   -99   1.3    .1   -99   -99   -99   -99   -99   -99   -99   -99

*IBBA910041  IBSNAT      -99      160  Rothhamsted
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Rothamsted  England           -99     -99 unknown
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .14     6    .5    60     1     1 SA001 SA001 SA001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    15   -99   .13   .28   .33     1   -99   1.2  1.16   -99   -99   -99   -99   -99   -99   -99   -99
    30   -99   .18   .32   .42    .9   -99   1.3     1   -99   -99   -99   -99   -99   -99   -99   -99
    50   -99   .25   .37   .42    .7   -99   1.4   .68   -99   -99   -99   -99   -99   -99   -99   -99
    70   -99   .25   .37   .42    .5   -99   1.4   .26   -99   -99   -99   -99   -99   -99   -99   -99
   100   -99   .25   .37   .42    .2   -99   1.5   .25   -99   -99   -99   -99   -99   -99   -99   -99
   130   -99   .25   .37   .42    .1   -99   1.5    .2   -99   -99   -99   -99   -99   -99   -99   -99
   160   -99   .25   .37   .42   .05   -99   1.6    .2   -99   -99   -99   -99   -99   -99   -99   -99

*IBBA910061  IBSNAT      -99      195  The Murrays
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 The_Murrays Scotland          -99     -99 unknown
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .12     5    .3    35     1     1 SA001 SA001 SA001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    15   -99   .19   .32    .4  .925   -99   1.3     1   -99   -99   -99   -99   -99   -99   -99   -99
    45   -99   .19   .32    .4    .5   -99   1.3     1   -99   -99   -99   -99   -99   -99   -99   -99
    75   -99   .19   .32    .4    .2   -99   1.3    .9   -99   -99   -99   -99   -99   -99   -99   -99
   105   -99   .19   .32    .4  .075   -99   1.3   .75   -99   -99   -99   -99   -99   -99   -99   -99
   135   -99   .19   .32    .4  .038   -99   1.3    .4   -99   -99   -99   -99   -99   -99   -99   -99
   165   -99   .15   .28   .36  .018   -99   1.3    .2   -99   -99   -99   -99   -99   -99   -99   -99
   195   -99   .14   .27   .35  .008   -99   1.3    .1   -99   -99   -99   -99   -99   -99   -99   -99

*IBPT910002  IBSNAT      -99       65  Sandy Clay (AUST.1970 P1, SALE)
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 -99         -99               -99     -99 UNKNOWN
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .14     5    .6    60     1     1 SA001 SA001 SA001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    10   -99   .18    .3   .38   .75   -99   1.2   2.5   -99   -99   -99   -99   -99   -99   -99   -99
    25   -99   .18    .3   .38     1   -99   1.2     2   -99   -99   -99   -99   -99   -99   -99   -99
    40   -99   .18    .3   .38    .5   -99   1.2   1.5   -99   -99   -99   -99   -99   -99   -99   -99
    65   -99   .18    .3   .38   .15   -99   1.2     1   -99   -99   -99   -99   -99   -99   -99   -99

*IBPT910005  IBSNAT      -99       65  Sandy Loam (Abedeen Idaho, 1978)
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Aberdeen    USA               -99     -99 UNKNOWN
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .14     5    .6    60     1     1 SA001 SA001 SA001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    10   -99   .11   .25   .33    .8   -99   1.2   2.5   -99   -99   -99   -99   -99   -99   -99   -99
    25   -99   .11   .25   .33     1   -99   1.2     2   -99   -99   -99   -99   -99   -99   -99   -99
    40   -99   .11   .25   .33    .5   -99   1.2   1.5   -99   -99   -99   -99   -99   -99   -99   -99
    65   -99   .11   .25   .33   .05   -99   1.2     1   -99   -99   -99   -99   -99   -99   -99   -99

*IBPT910006  IBSNAT      -99       65  MONTCALM MICHIGAN  McBRIDE SANDY LOAM
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 -99         -99               -99     -99 UNKNOWN (  6)
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .14     5    .6    60     1     1 SA001 SA001 SA001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    10   -99   .08   .22   .26    .8   -99  1.15   2.5   -99   -99   -99   -99   -99   -99   -99   -99
    25   -99   .08   .22   .26     1   -99   1.2     2   -99   -99   -99   -99   -99   -99   -99   -99
    40   -99   .08   .22   .26    .5   -99  1.25   1.5   -99   -99   -99   -99   -99   -99   -99   -99
    65   -99   .08   .22   .26   .05   -99  1.25     1   -99   -99   -99   -99   -99   -99   -99   -99

*UFPT930001  SCS         S        203  Eaugallie fine sand
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 BRADENTON   U.S.A.           27.3    82.3 SANDY, SILICEOUS, HYPERTHERMIC, ALFIC, HAPLAQUOD
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
     G   .13  12.7   .05    87     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    18   -99  .033  .166   .54     1  18.4  1.22     1    .6   2.9     0   1.5     6   -99     4   -99
    36   -99  .022  .111   .38    .3  15.8  1.51    .7    .6   2.9     0   1.5     5   -99   2.5   -99
    74   -99  .016  .081  .331    .1  21.1  1.62   .14    .6   1.9     0    .5   4.8   -99    .2   -99
    81   -99  .035  .248  .385   .05  22.4   1.6  1.19   3.7   4.8     0   3.5   4.5   -99   8.1   -99
   119   -99  .013  .159  .355     0  12.4  1.66    .6   1.5   2.6     0     1   4.8   -99   2.8   -99
   173   -99  .028  .252  .366     0   7.1  1.66   .53   2.4   2.9     0     0   4.9   -99     4   -99
   190   -99   .07   .26   .31     0    .5  1.79   .21    13     3     0     0   4.8   -99   5.7   -99
   203   -99  .134  .283  .308     0    .1  1.87   .14  21.8   2.1     0     0   4.2   -99   6.7   -99

*UFWH940002  SCS         SL       157  Crowley fine sandy loam
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 EAGLE LA,TX U.S.A.           29.6   -96.3 FINE, MONTMORILLONITIC, THERMIC TYPIC ALBAQUALFS
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
    BN   .13   8.8    .4    87     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    28    Ap   .08  .204  .337     1   -99   1.6     1    10    30     0   -99     6   -99   -99   -99
    38   A2g   .08  .204  .337   .67   -99   1.6   .65    10    30     0   -99   5.6   -99   -99   -99
    56  B21t   .28  .398  .413   .54   -99  1.52    .6    55    20     0   -99   5.8   -99   -99   -99
    97  B22g  .223  .335   .35   .37   -99  1.57    .4    42     8     0   -99     6   -99   -99   -99
   132    B3  .223  .335   .35   .25   -99  1.57   .25    42     8     0   -99   7.2   -99   -99   -99
   157     C  .223  .335   .35   .21   -99  1.57   .15    42     8     0   -99   7.2   -99   -99   -99

*UFWH940003  SCS         SIL      151  Captina silt loam
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 FAYETTEV,AR U.S.A.           36.1   -94.2 FINE-SILTY, MIXED, MESIC TYPIC FRAGIULDULTS
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
    BN   .13   9.1    .2    84     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    20    Ap  .097  .235   .42     1     6  1.34   1.5    14    65     3    10     5     5   -99   -99
    30  B21t  .097  .235   .42    .8     6  1.44   1.1    14    65     3    10     5     5   -99   -99
    50  B22t  .184  .314   .41    .3     6   1.4    .8    34    47     3    10     5     4   -99   -99
    65   Bx1  .184  .314   .41    .2     2   1.4    .6    34    47     3    10     5     4   -99   -99
   136   Bx2  .184  .314   .41   .15   1.5   1.4    .3    34    47     3    10     5     4   -99   -99
   151     C  .341  .457  .472   .05   1.5  1.65    .1    70    20     3    10     4     4   -99   -99

*UFWH940004  SCS         LS       172  Eustis loamy sand
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 AMERICUS,GA U.S.A.          30.95   -87.1 SANDY, SILICEOUS, THERMIC PSAMMENTIC PALEUDULTS
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
    BN   .13   8.5   .53    64     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    18    Ap  .055  .177  .302     1    33  1.48     1     6    15     0   -99   4.8   -99   -99   -99
    51   A21  .055  .177  .302   .85    33   1.5    .5     6    15     0   -99   4.8   -99   -99   -99
    66   A22  .055  .177  .302    .4    33   1.5    .3     6    15     0   -99   4.8   -99   -99   -99
    91    B1  .054  .172  .313    .3    33   1.5    .3    10    10     0   -99   4.8   -99   -99   -99
   147  B21t  .054  .172  .313    .2    33   1.5    .2    10    10     0   -99   4.8   -99   -99   -99
   172  B22t  .054  .172  .313   .15    33   1.5    .1    10    10     0   -99   4.8   -99   -99   -99

*UFWH940005  SCS         SL       189  Greenville sandy loam
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 THORSBY,AL  U.S.A.           32.9   -86.7 CLAYEY, KAOLINITIC, THERMIC RHODIC PALEUDULTS
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
    BN   .13     9    .6    76     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    13    Ap  .087  .203  .332    .9    10  1.62    .7    12    25     5   -99     5   -99   -99   -99
    28    B1  .219  .329  .345    .6   3.5  1.57    .6    42     8     3   -99     5   -99   -99   -99
   139  B21t  .299  .413  .428    .3   3.5  1.53    .3    60    15     2   -99     5   -99   -99   -99
   189  B22t  .299  .413  .428    .2   3.5  1.53   .15    60    15     2   -99     5   -99   -99   -99

*UFWH940010  SCS         SL       150  Red Bay sandy loam
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 JAY,FL      U.S.A.          30.95   -87.1 FINE-LAOMY, SILICEOUS, THERMIC RHODIC PALEUDULTS
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
    BN   .13   9.2    .6    84     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    20    Ap  .103  .218  .326     1   -99  1.63   .89    15  15.5     0   -99   5.3   4.6   -99   -99
    35    B1  .125  .239  .333    .6   -99  1.61   .32    20  12.9     0   -99   5.3   4.4   -99   -99
   128  B21t  .147  .261  .337    .1   -99   1.6   .04    25  11.3     0   -99   5.5   4.8   -99   -99
   150  B22t  .125  .236  .332   .02   -99  1.61   .02    20   7.1     0   -99   5.6   5.2   -99   -99

*IBSC910014  SASA        -99      300  Wartland
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 LAMERCY     RSA            -29.63      30 Loamy,silic,hyperth Arenic Paleudult
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .18     9     1    82     1    .8 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99  .083  .265  .387     1    .8   1.3     2   -99   -99   -99     0     7   -99   -99   -99
    15   -99  .083  .265  .329   .82    .8  1.61     1   -99   -99   -99     0     7   -99   -99   -99
    30   -99  .112  .247  .316   .64    .8  1.63     1   -99   -99   -99     0     7   -99   -99   -99
    45   -99  .141  .238  .319   .47    .8  1.59    .5   -99   -99   -99     0     7   -99   -99   -99
    60   -99  .142  .248  .345   .35    .7  1.48    .5   -99   -99   -99     0     7   -99   -99   -99
    75   -99  .152  .268  .359   .26    .6  1.46    .5   -99   -99   -99     0     7   -99   -99   -99
    90   -99  .231  .339   .39   .19    .5  1.48    .1   -99   -99   -99     0     7   -99   -99   -99
   105   -99  .317  .359  .385   .14    .5  1.56    .1   -99   -99   -99     0     7   -99   -99   -99
   120   -99  .356  .385  .391    .1    .5   1.6   .04   -99   -99   -99     0     7   -99   -99   -99
   300   -99  .367  .415  .418  .015   .01  1.56   .24   -99   -99   -99     0     7   -99   -99   -99

*IBSU910045              -99      300  Granja Sandy Loam
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Cordoba     Spain             -99     -99 Granja sandy loam. Cordoba, Spain
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .18     6    .5    77     1     1 SA001 SA001 SA001
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
!*IBSU910045              -99     300 Granja Sandy Loam
!@SITE        COUNTRY          LAT     LONG SCS FAMILY
! Cordoba     Spain             -99    -99  Granja sandy loam. Cordoba, Spain
!@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
!   -99  0.18   6.0  0.50  77.0  1.00  1.00 SA001 SA001 SA001
!@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
!    10   -99 0.080 0.240 0.340 1.000   -99  1.60  0.00   -99   -99   -99   -99   -99   -99   -99   -99 
!    30   -99 0.080 0.240 0.340 1.000   -99  1.60  0.00   -99   -99   -99   -99   -99   -99   -99   -99 
!    60   -99 0.080 0.240 0.360 0.100   -99  1.60  0.00   -99   -99   -99   -99   -99   -99   -99   -99 
!    90   -99 0.080 0.240 0.380 0.100   -99  1.60  0.00   -99   -99   -99   -99   -99   -99   -99   -99 
!   120   -99 0.080 0.240 0.370 0.100   -99  1.60  0.00   -99   -99   -99   -99   -99   -99   -99   -99 
!   150   -99 0.080 0.240 0.380 0.100   -99  1.60  0.00   -99   -99   -99   -99   -99   -99   -99   -99 
!   180   -99 0.080 0.240 0.350 0.100   -99  1.60  0.00   -99   -99   -99   -99   -99   -99   -99   -99 
!   210   -99 0.080 0.240 0.360 0.100   -99  1.60  0.00   -99   -99   -99   -99   -99   -99   -99   -99 
!   240   -99 0.080 0.240 0.350 0.010   -99  1.60  0.00   -99   -99   -99   -99   -99   -99   -99   -99 
!   270   -99 0.080 0.240 0.340 0.001   -99  1.60  0.00   -99   -99   -99   -99   -99   -99   -99   -99 
!   300   -99 0.080 0.240 0.340 0.001   -99  1.60  0.00   -99   -99   -99   -99   -99   -99   -99   -99 

*IBWH910999  Swift       -99      150  Wood Mountain Loam
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Swift_Curr  Canada            -99     -99 Orthic Brown Chernozem
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .12     8    .5    60     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99  .096   .23   .25     1   -99     0   1.1   -99   -99   -99   -99   -99   -99   -99   -99
    15   -99  .096   .23   .25    .8   -99     0   1.1   -99   -99   -99   -99   -99   -99   -99   -99
    30   -99  .112   .25   .26    .7   -99     0   .61   -99   -99   -99   -99   -99   -99   -99   -99
    45   -99  .094   .22   .23    .5   -99     0   .61   -99   -99   -99   -99   -99   -99   -99   -99
    60   -99  .103   .22   .23   .25   -99     0   .59   -99   -99   -99   -99   -99   -99   -99   -99
    75   -99  .103   .22   .23   .15   -99     0   .15   -99   -99   -99   -99   -99   -99   -99   -99
    90   -99  .102   .22   .25   .08   -99     0    .1   -99   -99   -99   -99   -99   -99   -99   -99
   120   -99  .102   .22   .25   .05   -99     0    .1   -99   -99   -99   -99   -99   -99   -99   -99
   150   -99  .102   .22   .25   .05   -99     0    .1   -99   -99   -99   -99   -99   -99   -99   -99

*IBCH910008  IBSNAT      -99      180  VERY FINE MONTMORILLONITIC ISOHYPERTHERMIC
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Patancheru  India             -99     -99 Vertisol
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .13     6    .7    95     1    .8 SA001 SA001 SA001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99  .265   .36  .395     1   -99   1.5   .73   -99   -99   -99   -99   8.8   -99   -99   -99
    20   -99    .2  .337  .395     1   -99   1.5   .73   -99   -99   -99   -99   8.8   -99   -99   -99
    30   -99  .159  .309  .407     1   -99  1.51   .54   -99   -99   -99   -99   9.2   -99   -99   -99
    40   -99  .154   .31  .407    .9   -99  1.51   .54   -99   -99   -99   -99   9.2   -99   -99   -99
    50   -99  .161  .307  .416    .6   -99  1.37   .47   -99   -99   -99   -99   9.4   -99   -99   -99
    60   -99  .155    .3  .416    .6   -99  1.37   .47   -99   -99   -99   -99   9.4   -99   -99   -99
    75   -99  .165  .292  .424   .35   -99  1.38   .39   -99   -99   -99   -99   9.4   -99   -99   -99
    90   -99  .165  .262  .424   .35   -99  1.38   .39   -99   -99   -99   -99   9.4   -99   -99   -99
   110   -99  .167  .225  .451   .15   -99   1.4   .28   -99   -99   -99   -99   9.4   -99   -99   -99
   130   -99   .16  .222  .451   .15   -99   1.4   .28   -99   -99   -99   -99   9.4   -99   -99   -99
   155   -99   .16  .222  .443   .05   -99   1.4   .25   -99   -99   -99   -99   9.4   -99   -99   -99
   180   -99  .165  .222  .443   .05   -99   1.4   .25   -99   -99   -99   -99   9.4   -99   -99   -99

*IBCH910010  IBSNAT      -99       97  Kasireddipalli Series
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Patancheru  India             -99     -99 Vertisol, Typic Pellustert
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .11     6    .7  84.5     1    .9 SA001 SA001 SA001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    10   -99   .14   .42   .47     1   -99  1.35   .96   -99   -99   -99   -99   8.8   -99   -99   -99
    22   -99   .23   .42   .47     1   -99  1.35   .96   -99   -99   -99   -99   8.8   -99   -99   -99
    37   -99   .22    .4   .46    .7   -99  1.36   .69   -99   -99   -99   -99   9.2   -99   -99   -99
    52   -99   .21   .37   .46    .5   -99  1.36   .68   -99   -99   -99   -99   9.2   -99   -99   -99
    67   -99   .21  .365   .46   .25   -99  1.36    .6   -99   -99   -99   -99   9.4   -99   -99   -99
    82   -99   .21  .347   .46  .125   -99  1.36    .6   -99   -99   -99   -99   9.4   -99   -99   -99
    97   -99    .2    .3   .46   .06   -99  1.36    .6   -99   -99   -99   -99   9.4   -99   -99   -99

*UFBR950001  SCS         S        203  Eaugallie fine sand (IMPERVIOUS LAYER AT 120 CM)
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 BRADENTON   U.S.A.           27.3    82.3 SANDY, SILICEOUS, HYPERTHERMIC, ALFIC, HAPLAQUOD
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
     G   .13  12.7   .05    87     1   .97 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    18   -99  .022  .133  .423     1  18.4  1.22  1.02    .8   1.1     0   .09     6   -99   3.9   -99
    36   -99  .022  .111   .43    .3  15.8  1.51    .7    .6   2.9     0   .06     5   -99   2.6   -99
    74   -99  .022  .111   .43   .01  21.1  1.62   .14    .6   1.9     0   .01   4.8   -99    .2   -99
    81   -99  .035  .248  .396     0  22.4   1.6   1.2   3.7   4.8     0    .1   4.5   -99   8.1   -99
   119   -99  .013  .159  .374     0     0  1.66    .6   1.5   2.6     0   .05   4.8   -99   2.8   -99
   173   -99  .028  .252  .374     0   7.1  1.66   .53   2.4   2.9     0   .04   4.9   -99   4.1   -99
   190   -99   .07   .26  .324     0    .5  1.79   .21    13     3     0   .02   4.8   -99   5.7   -99
   203   -99  .134  .283  .294     0    .1  1.87   .14  21.8   2.1     0   .01   4.2    99   6.7   -99

*IA00940001  SCS         SIC      175  LHK
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 LUSHNJE     ALBANIA            40      19 FINR
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
    BN   .13   7.7    .2    76     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    19    AP  .277  .407  .422     1   -99  1.31  1.34  54.2  43.6     0   .17   8.2   -99  34.6   -99
    42    B1  .282  .411  .426    .2   -99  1.47  1.18  55.3  42.9     0   .16   8.3   -99  34.5   -99
    68    B2  .279  .408  .423    .2   -99  1.39  1.14  54.6  43.1     0   .16   8.3   -99  38.6   -99
    97    B3  .252  .385    .4   .15   -99  1.38   .85  48.6  50.1     0   .13   8.3   -99    36   -99
   128    B4  .245  .375  .397    .1   -99  1.36    .8    47  43.4     0   .12   8.2   -99  34.1   -99
   153    3C  .155  .286  .391    .1   -99  1.51   .42  26.8  46.1     0   .06   8.6   -99  19.8   -99
   175    CG   .22   .35  .403    .1   -99  1.47   .67  41.4    45     0    .1   8.7   -99  30.4   -99

*GA00620001  SCS         SL       191  LLOYD SERIES - SPALDING COUNTY, GA
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 GRIFFIN     USA              33.3   -84.3 RED YELLOW PODZOLIC
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
     R   .14     9    .6    76     1   .92 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    18     A  .092  .209  .325     1   -99   1.4   .38  12.5  19.6     0   -99   5.5   -99   -99   -99
    28     B  .158  .275  .344   .75   -99   1.4   .12  27.5  17.4     0   -99     5   -99   -99   -99
   127     B  .269  .392  .409    .5   -99   1.4   .05  52.5  29.4     0   -99     5   -99   -99   -99
   191     C  .269  .392  .409   .35   -99   1.4   .05  52.5  29.4     0   -99     5   -99   -99   -99

*CCBN910030  CIAT        -99      209  M2N
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Palmira(M2) Colombia         3.48  -73.37 Fine-silty,mixed,isohyperth.Aquic Hapludoll ( 30)
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .09    11    .4    84     1   .81 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99  .204   .34  .392     1   -99  1.45  2.19   -99   -99   -99   -99   6.9   -99   -99   -99
    15   -99  .204   .34  .392     1   -99  1.45  2.19   -99   -99   -99   -99   6.9   -99   -99   -99
    25   -99  .209  .345   .39   .75   -99  1.45  1.21   -99   -99   -99   -99   7.2   -99   -99   -99
    35   -99  .209  .345   .39    .5   -99  1.45  1.21   -99   -99   -99   -99   7.2   -99   -99   -99
    50   -99  .198  .335   .39   .35   -99  1.49   .53   -99   -99   -99   -99     8   -99   -99   -99
    65   -99  .185  .323  .395    .2   -99  1.58    .2   -99   -99   -99   -99   8.2   -99   -99   -99
    80   -99  .185  .323  .395   .15   -99  1.58    .2   -99   -99   -99   -99   8.2   -99   -99   -99
    99   -99  .201  .328  .408    .1   -99  1.54    .1   -99   -99   -99   -99   8.1   -99   -99   -99
   122   -99  .198  .325   .41   .05   -99  1.58   .09   -99   -99   -99   -99   8.2   -99   -99   -99
   137   -99  .159  .288  .399     0   -99   1.5   .09   -99   -99   -99   -99   8.3   -99   -99   -99
   159   -99   .11  .242  .402     0   -99  1.69    .1   -99   -99   -99   -99   8.3   -99   -99   -99
   184   -99  .047  .177  .351     0   -99  1.59   .08   -99   -99   -99   -99     8   -99   -99   -99
   209   -99   .05  .193   .41     0   -99  1.45   .12   -99   -99   -99   -99   8.5   -99   -99   -99
! 26 Feb. 1994 J White made this profile to represent poorer field where
! P. Sexton's trial in 1990 was run.  Source was IBBN910030

*CCBN880060  SCS         SIL      200  UNKNOWN
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 POPAYAN     COLOMBIA         2.42  -76.58 MEDIAL, ISOTHERMIC TYPIC DYSTRANDEPT (INCEPTISOL)
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
    BK   .09   8.7    .6    76     1    .9 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5    AP  .075  .215  .406    .5   -99   .58  16.8     9  61.6     0  1.25   5.7   -99   -99   -99
    13    AP  .075  .215  .406    .5   -99   .58  16.8     9  61.6     0  1.25   5.7   -99   -99   -99
    30     A  .039  .175  .371    .2   -99   .55  15.8    .9    53     0   .94   4.9   -99   -99   -99
    39    BA   .05  .164  .302    .1   -99   .52  6.87     0  18.5     0   .58   5.1   -99   -99   -99
    66    BT  .043  .139  .302    .1   -99   .48   3.8     0    14     0   .36   5.3   -99   -99   -99
    82    BT  .041   .13  .302    .1   -99   .51  2.19     0  12.4     0     0   5.3   -99   -99   -99
   102    BT  .044   .14  .302    .1   -99   .62  1.88     0  14.2     0     0   5.3   -99   -99   -99
   124    BT   .04  .128  .302    .1   -99   .64  1.51     0    12     0     0   5.2   -99   -99   -99
   137    BT  .045  .145  .302     0   -99   .51  1.66     0  15.2     0     0   5.2   -99   -99   -99
   176    BT  .037  .117  .302     0   -99   .48  1.31     0    10     0     0   5.2   -99   -99   -99
   200    BT  .033  .104  .302     0   -99    .4  1.65     0   7.7     0     0   5.6   -99   -99   -99

*CCPA970001  SCS         SICL     145  CIAT
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 CIAT        COLOMBIA          3.5   76.35 Unknown
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
    BK    .1  12.7    .3    70     1   .97 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    10   -99  .216  .357   .46     1   -99  1.35  1.74  27.7  53.1     0   -99   7.3   -99  32.1   -99
    38   -99  .216  .357   .46     1   -99   1.4  1.57  27.7  53.1     0   -99   7.3   -99  32.1   -99
    57   -99  .168  .306   .46    .6   -99   1.5   .35  34.4  47.6     0   -99   8.2   -99  27.9   -99
    81   -99  .144  .277   .44    .3   -99   1.5   .17  23.1  56.1     0   -99   8.2   -99  23.1   -99
    95   -99   .14  .272   .45    .1   -99   1.6   .17  28.7  49.1     0   -99   8.5   -99  22.5   -99
   110   -99  .103  .191   .49     0   -99   1.6   .29  21.9  54.2     0   -99   8.6   -99  14.3   -99
   128   -99  .035  .177   .45     0   -99   1.6   .17     5  63.8     0   -99   8.9   -99   6.5   -99
   145   -99  .034  .183   .53     0   -99   1.6   .17    43  49.7     0   -99   8.7   -99   6.1   -99

*CI00960001  SCS         CL       150  El Batan
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 EL BATANG9  Mexico          19.31   -98.5 El Batan
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
    BN   .13  10.5    .4    76     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    15     A  .171   .29  .359   .75   1.5   1.5    .8    31    26     3   .12   6.2   -99    14   -99
    30   -99  .201  .318  .358    .5    .7   1.5    .7    38    22     3   .07   6.3   -99  15.5   -99
    45   -99  .178  .296  .359   .35   1.3   1.6    .9    33    25     4    .1   6.3   -99    15   -99
    60   -99  .164  .274  .341   .35     1   1.6    .3    30    10     5   -99     6   -99   -99   -99
    75   -99  .058  .186  .325   .35   -99   1.6    .2    15     9     6   -99     6   -99   -99   -99
    90   -99  .048  .154  .338    .2   -99   1.7    .2    10     8     7   -99     6   -99   -99   -99
   150   -99  .039  .125  .327   .15   -99   1.7   .15     7     6    10   -99     6   -99   -99   -99

*CI00970002  SCS         SICL     130  SUELO COLORADA
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 SANTA ROSA2 MEXICO           18.3   -95.1 HUMIC HAPLUDULTS
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
    YR   .14  10.7    .8    80     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    20    AP  .185  .315  .406   .75   -99  1.38     2    34    50     3   -99   5.4   -99    41   -99
    37    A1    .2  .325  .406    .5   -99  1.38   1.6    36    48     2   -99   5.6   -99    45   -99
    86    B1  .238  .365  .395   .15   -99  1.42    .7    47    45     5   -99   5.5   -99    65   -99
   130    B2  .182  .314  .399     0   -99  1.41   .22    34    55     5   -99   4.8   -99    70   -99

*CBAN840001  XXXX   SACL SCL      120  Sandy-Clay Loam EL NUS: CENICAFE/CORPOICA
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 EL NUS COLO MBIA             6.48       3 Inceptisols.
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .13  11.5   .65    75     1     1   -99   -99   -99
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99   .19   .33   .43   .95   .68  1.36   1.4    31    23   -99   .14   4.8   -99   7.5   -99
    10   -99   .19   .34   .43   .86   .68  1.36  1.26    31    23   -99   .14   4.8   -99   7.5   -99
    15   -99   .19   .33   .43   .78   .68  1.36  1.15    31    23   -99   .14   4.8   -99   7.5   -99
    20   -99   .19   .32   .43    .7   .68  1.38  1.03    31    23   -99   .14   4.8   -99   7.5   -99
    25   -99   .19   .32   .43   .63   .68   1.4   .93    31    16   -99   .06   5.3   -99   5.8   -99
    30   -99   .19   .31   .43   .58   .66  1.41   .85    31    16   -99   .06   5.3   -99   5.8   -99
    35   -99   .19   .33   .43   .52   .52  1.42   .76    31    16   -99   .06   5.3   -99   5.8   -99
    40   -99   .19   .33   .43   .47   .52  1.43    .7    31    16   -99   .06   5.3   -99   5.8   -99
    70   -99   .19   .32    .4   .33   .52  1.44    .5    31    16   -99   .16   5.3   -99   5.8   -99
   120   -99   .19   .32    .4   .15   .52  1.48   .22    31    16   -99    .1   5.3   -99   5.8   -99

*CCCA800001  CL       CL SIL      120  ULTISOL CARIMAGUA-CIAT
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 CARIMAGUA   COLOMBIA         4.61   71.31 CLAY
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .13  11.5   .65    75     1     1   -99   -99   -99
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99   .19   .35   .43   .95   -99   1.3   1.6    12    50   -99   .16   4.1   -99   5.6   -99
    10   -99   .24    .4   .43   .86   -99  1.42  1.45    12    50   -99   .16   4.1   -99   5.6   -99
    15   -99   .24    .4   .43   .78   -99  1.43  1.31    12    50   -99   .16   4.1   -99   5.6   -99
    20   -99   .24    .4   .43    .7   -99  1.43  1.18    12    50   -99   .16   4.1   -99   5.6   -99
    25   -99   .24    .4   .43   .63   -99  1.43  1.06    12    48   -99   .13     4   -99   3.4   -99
    30   -99   .24   .39   .43   .58   -99  1.45   .98    12    48   -99   .13     4   -99   3.4   -99
    35   -99   .24   .39   .43   .52   -99  1.46   .88    12    48   -99   .13     4   -99   3.4   -99
    40   -99   .24   .39   .43   .47   -99  1.46    .8    12    48   -99   .13     4   -99   3.4   -99
    70   -99   .23   .39   .43   .33   -99  1.48   .56    12    48   -99    .7     4   -99   3.4   -99
   120   -99   .23   .39   .43   .15   -99   1.5   .25    12    48   -99    .5     4   -99   3.4   -99

*CCQU790001  SCS         C         60  QUILICHAO
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 QUILICHAO   COLOMBIA          3.1   76.51 Fine, Kaolinitic, Isohyperth. oxic
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
    BN   .13   6.7   .18    76     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    20   -99  .351  .468  .483     1   -99   .96   5.1    71    18   -99   .44   4.1   -99   2.2   -99
    40   -99  .356   .47  .485    .7   -99  1.02  3.77    72    13   -99   .32   3.9   -99   1.7   -99
    60   -99  .366   .48  .487    .4   -99  1.12  2.77    74    13   -99   .22   3.9   -99   1.3   -99

*CCYU820001  XXXX   LOSA SL       120  ULTISOL, TYPIC PALEUDULT AT YURIMAGUAS-CIAT
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 YURIMAGUAS  PERU            -5.93       3 Loamy sand.
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .15  11.5   .65    75     1     1   -99   -99   -99
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99   .06   .19   .44   .95     0   1.3   1.2     6    21   -99   .12   4.4   -99   3.5   -99
    10   -99   .06   .21   .44   .86     0  1.52  1.07     6    21   -99   .12   4.4   -99   3.5   -99
    15   -99   .06    .2   .44   .78     0  1.54   .99     6    21   -99   .12   4.4   -99   3.5   -99
    20   -99   .06   .21   .44    .7     0  1.55   .88     6    21   -99   .12   4.4   -99   3.5   -99
    25   -99   .06    .2   .41   .63     0  1.55    .8    16    24   -99   .12   4.6   -99   3.7   -99
    30   -99   .06    .2   .41   .58     0  1.57   .73    16    24   -99   .12   4.6   -99   3.7   -99
    35   -99   .06    .2   .41   .52     0  1.58   .65    16    24   -99   .12   4.6   -99   3.7   -99
    40   -99   .06    .2   .41   .47     0  1.59    .6    16    24   -99   .12   4.6   -99   3.7   -99
    70   -99   .06    .2   .41   .33     0  1.62   .42    16    24   -99   .08   4.6   -99   3.7   -99
   120   -99  .059    .2   .41   .15     0  1.63   .19    16    24   -99   .05   4.6   -99   3.7   -99

*CNCH820001  XXXX   SALO L        120  SANDY LOAM AT LA ROMELIA-CENICAFE
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 LA ROMELIA  COLOMBIA         4.96    75.7 Typic Dystrandept.
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .13  11.5   .65    75    .8     1   -99   -99   -99
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99   .09   .26   .62   .95   -99    .7   3.6    19    31   -99   .36   5.1   -99   3.8   -99
    10   -99   .13   .36   .62   .86   -99  1.31  3.26    19    31   -99   .36   5.1   -99   3.8   -99
    15   -99   .13   .36   .62   .78   -99  1.33  2.95    19    31   -99   .36   5.1   -99   3.8   -99
    20   -99   .13   .36   .62    .7   -99  1.34  2.65    19    31   -99   .36   5.1   -99   3.8   -99
    25   -99   .13   .35   .43   .63   -99  1.37  2.39    19    31   -99   .36   5.1   -99   -99   -99
    30   -99   .12    .3   .43   .58   -99  1.37   2.2    19    31   -99   .36   5.1   -99   -99   -99
    35   -99   .14   .32   .43   .52   -99  1.39  1.97    19    31   -99   .36   5.1   -99   -99   -99
    40   -99   .14    .3   .43   .47   -99   1.4  1.78    19    31   -99   .36   5.1   -99   -99   -99
    70   -99   .13   .28    .4   .33   -99  1.49  1.26    19    31   -99    .2   5.1   -99   -99   -99
   120   -99   .13   .27    .4   .15   -99  1.55   .57    19    31   -99    .1   5.1   -99   -99   -99

*SAUR900001  XXXX   SACL SCL      120  SANDY CLAY LOAM AT URRAO-SECR.AGRIC. ANTIOQUIA
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 URRAO       COLOMBIA         4.61   71.31 Inseptisols
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .13  11.5   .65    75     1     1   -99   -99   -99
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99   .19   .32   .41   .95   .68  1.36  1.56    30    23   -99   .15   4.5   -99   7.5   -99
    10   -99   .19   .32   .38   .86   .68  1.45  1.41    30    23   -99   .15   4.5   -99   7.5   -99
    15   -99   .19   .32   .38   .78   .68  1.46  1.28    30    23   -99   .15   4.5   -99   7.5   -99
    20   -99   .27   .38   .39    .7   .68  1.47  1.15    30    23   -99   .15   4.5   -99   7.5   -99
    25   -99   .27   .38    .4   .63   .68  1.42  1.03    49    25   -99   .15   4.5   -99   5.8   -99
    30   -99   .27   .38   .39   .58   .66  1.44   .95    49    25   -99   .06   4.5   -99   5.8   -99
    35   -99   .27   .38   .43   .52   .52  1.45   .85    49    25   -99   .06   4.5   -99   5.8   -99
    40   -99   .27   .38   .43   .47   .52  1.45   .77    49    25   -99   .06   4.5   -99   5.8   -99
    70   -99   .27   .39    .4   .33   .52  1.47   .54    49    25   -99   .05   4.5   -99   5.8   -99
   120   -99   .27   .37   .37   .15   .52   1.5   .24    49    25   -99   .02   4.5   -99   5.8   -99

*IB00740011  SCS         CL       137  MAKIKI
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 HI021 (4)   US                -99       0 ANDIC USTIC HUMITROPEPT, FINE, MIXED, ISOHYPERTHER
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
    BN   .13    11    .6    87     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5     A  .189  .306  .383    .6   -99     1  4.47    38    37    13   -99   5.3   -99   -99   -99
    21     A  .189  .306  .383    .6   -99     1  4.47    38    37    13   -99   5.3   -99   -99   -99
    51     A  .189  .306  .383    .6   -99     1  4.47    38    37    13   -99   5.3   -99   -99   -99
    76     B  .189  .306  .383   .28   -99     1     0    38    37    13   -99   5.8   -99   -99   -99
   137     B  .145  .234  .383   .12   -99   .85     0    38    37    43   -99   5.8   -99   -99   -99

*IB00830002  SCS         SIL      150  Unknown (Kula Ag Park)
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 KULA AGPARK US              20.78 156.376 TORROXIC HAPLUSTOLL, FINE, KAOLINITIC, ISOHYPERTHE
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
    BN   .13    10    .6    91     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    10    AP   .15  .283    .4   .75   -99  1.29  1.96  25.6  50.8     0   -99     7   6.3  26.4   -99
    29    BW  .132  .267    .4     1   -99  1.08  1.29  21.7  52.4     0   -99   7.1   6.4  24.5   -99
    48    BW  .137  .263  .393   .75   -99  1.12   .95  24.2    48     8   -99   7.2   6.4    20   -99
    82    BW  .146  .277  .392     1   -99  1.17   .78  24.7  47.3     0   -99   7.3   6.7  16.9   -99
   122    BW  .144  .286  .395    .5   -99  1.14   .53  24.3  67.7     0   -99   7.3   6.7  15.7   -99
   150    BW  .184  .321  .398    .2   -99   1.3   .42  33.4  56.6     0   -99   7.4   6.6  13.4   -99

*IB00830003  SCS         L        160  Unknown (Pulehu Experimental Station)
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 PULEHU EXFM US             20.781 156.333 TORROXIC HAPLUSTOLL, FINE, KAOLINITIC, ISOHYPERTHE
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
    BN   .13  10.1    .6    94     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    31    AP  .142  .266  .396     1   -99   .95  3.98    26  48.6    11   .39   6.1   5.3  38.9   -99
    46    BW  .178  .309  .408     1   -99  1.01  2.05  32.6  49.9     3   .22   6.6   5.9  24.5   -99
    58    BW   .21  .334  .402     1   -99  1.08  1.01  41.6  45.4     9   .09   6.1   5.4  15.5   -99
    88    BW  .162  .262  .402     1   -99  1.11   .87  40.2  46.8    38   -99   5.6   4.8  13.9   -99
   160    BC  .098  .179   .41    .5   -99   .74   .92  28.3  52.7    56   -99   5.8   4.7  18.2   -99

*IB00720001  SCS         CL        51  Unknown (Mauka Campus)
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 MAUKA CAMPU US              21.31  157.84 -99
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
    BN   .13  10.4    .6    76     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    25    AP  .154  .266  .365     1   -99  1.37  3.61    30    30    15   -99   6.7   -99   -99   -99
    51    AP  .154  .266  .365     1   -99  1.32  3.53    30    30    15   -99   6.8   -99   -99   -99

*IBPI910013  IBSNAT      -99      110  Waipio
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Waipio,HI   USA               -99     -99 Clayey, kaolinitic, isohyperth, Tropeptic Eutrusto
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .14     5    .6    60     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99   .22   .35   .55     1   -99     1  2.27   -99   -99   -99   -99   6.3   -99   -99   -99
    15   -99   .23   .35   .55     1   -99     1  2.27   -99   -99   -99   -99   6.3   -99   -99   -99
    30   -99   .24   .35   .55    .8   -99  1.05   1.1   -99   -99   -99   -99   5.8   -99   -99   -99
    50   -99   .25   .37   .48    .4   -99  1.17  1.41   -99   -99   -99   -99   5.8   -99   -99   -99
    70   -99   .26   .38   .46    .2   -99  1.22   .59   -99   -99   -99   -99     6   -99   -99   -99
    90   -99   .25   .38   .46   .05   -99  1.22   .36   -99   -99   -99   -99     6   -99   -99   -99
   110   -99   .26    .4   .48   .02   -99  1.17   .27   -99   -99   -99   -99     6   -99   -99   -99

*IBAR910030  SCS         -99      137  87p294
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 -99         -99               -99     -99 CLAYEY, KAOLINITIC, ISOHYPERTHERMIC TROPEPTIC HAPL
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .13   6.9    .6    76     1     1 SA001 SA001 SA001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99  .266   .38  .412     1   -99  1.39  2.01   -99   -99   -99   -99   4.8   -99    20   -99
    20   -99  .266   .38  .412    .9   -99  1.39  2.01   -99   -99   -99   -99   4.8   -99    20   -99
    35   -99   .32  .422   .44   .75   -99  1.33   .72   -99   -99   -99   -99   6.1   -99    20   -99
    50   -99   .32  .422   .44    .5   -99  1.33   .72   -99   -99   -99   -99   6.1   -99    20   -99
    65   -99  .319  .421  .436    .2   -99  1.41   .56   -99   -99   -99   -99   6.4   -99    20   -99
    80   -99  .319  .421  .436    .1   -99  1.41   .56   -99   -99   -99   -99   6.4   -99    20   -99
    99   -99  .325  .425  .463   .05   -99  1.26   .43   -99   -99   -99   -99   6.6   -99    20   -99
   118   -99  .314  .415  .433  .005   -99   1.5   .23   -99   -99   -99   -99   6.7   -99    20   -99
   137   -99  .314  .415  .433     0   -99   1.5   .23   -99   -99   -99   -99   6.7   -99    20   -99

*IBAR910034  S70Ha-7-1   -99       66  IBAR910034
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 -99         -99               -99     -99 VERTIC HAPLUSTOLLS, VERY-FINE, KAOLINITIC, ISOHYPE
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .13  11.7    .6    87    .8     1 SA001 SA001 SA001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99  .226  .348  .396     1   -99  1.18  1.98   -99   -99   -99   -99   6.1   -99    20   -99
    18   -99  .226  .348  .396    .8   -99  1.18  1.98   -99   -99   -99   -99   6.1   -99    20   -99
    28   -99  .233  .359  .403    .5   -99  1.22   1.9   -99   -99   -99   -99   6.2   -99    20   -99
    38   -99  .233  .359  .403   .05   -99  1.22   1.9   -99   -99   -99   -99   6.2   -99    20   -99
    52   -99  .223  .347    .4  .005   -99   1.1    .8   -99   -99   -99   -99   6.4   -99    20   -99
    66   -99  .223  .347    .4     0   -99   1.1    .8   -99   -99   -99   -99   6.4   -99    20   -99

*IBAR910035  83P0882     -99      153  IBAR910035
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 -99         -99               -99     -99 CLAYEY, OXIDIC, ISOTHERMIC HUMOXIC TROPOHUMULT (35
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .13  10.1    .6    85     1     1 SA001 SA001 SA001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99  .155  .298  .419     1   -99  1.17  3.58   -99   -99   -99   -99   5.1   -99    20   -99
    16   -99  .155  .298  .419    .8   -99  1.17  3.58   -99   -99   -99   -99   5.1   -99    20   -99
    30   -99  .141   .28  .413    .5   -99  1.33  2.22   -99   -99   -99   -99   4.8   -99    20   -99
    40   -99  .109  .247  .426   .25   -99  1.65   1.3   -99   -99   -99   -99   4.5   -99    20   -99
    51   -99  .109  .247  .426    .2   -99  1.65   1.3   -99   -99   -99   -99   4.5   -99    20   -99
    62   -99  .104  .222  .388    .2   -99  1.61   .98   -99   -99   -99   -99   4.7   -99    20   -99
    74   -99  .104  .222  .388    .1   -99  1.61   .98   -99   -99   -99   -99   4.7   -99    20   -99
    97   -99  .067  .158  .361   .01   -99  1.68   .88   -99   -99   -99   -99   4.7   -99    20   -99
   123   -99  .026  .085  .354     0   -99   1.7  3.14   -99   -99   -99   -99   4.9   -99    20   -99
   153   -99  .034  .111  .354     0   -99   1.7  1.75   -99   -99   -99   -99   5.1   -99    20   -99

*IBAR910043  87P294      -99      227  IBAR910043
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 -99         -99               -99     -99 VERY-FINE, KAOLINITIC, ISOHYPERTHERMIC XANTHIC KAN
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .13   6.9    .6    76    .8     1 SA001 SA001 SA001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99  .305  .407  .422    .5   -99  1.39  2.01   -99   -99   -99   -99     6   -99   -99   -99
    10   -99  .305  .407  .422    .5   -99  1.39  2.01   -99   -99   -99   -99     6   -99   -99   -99
    22   -99  .305  .407  .422    .5   -99  1.39  2.01   -99   -99   -99   -99     6   -99   -99   -99
    35   -99  .389  .498  .513    .5   -99  1.33   .72   -99   -99   -99   -99   6.5   -99   -99   -99
    48   -99  .389  .498  .513    .5   -99  1.33   .72   -99   -99   -99   -99   6.5   -99   -99   -99
    63   -99  .386  .494  .509    .2   -99  1.41   .56   -99   -99   -99   -99   6.5   -99   -99   -99
    78   -99  .386  .494  .509    .2   -99  1.41   .56   -99   -99   -99   -99   6.5   -99   -99   -99
    97   -99  .394  .502  .517    .2   -99  1.26   .43   -99   -99   -99   -99   6.5   -99   -99   -99
   116   -99  .383  .493  .508    .2   -99   1.5   .23   -99   -99   -99   -99   6.5   -99   -99   -99
   135   -99  .383  .493  .508    .2   -99   1.5   .23   -99   -99   -99   -99   6.5   -99   -99   -99
   167   -99   .35  .463  .478    .2   -99  1.52   .18   -99   -99   -99   -99   6.5   -99   -99   -99
   197   -99  .372   .48  .495    .2   -99  1.65   .12   -99   -99   -99   -99   6.5   -99   -99   -99
   227   -99  .372   .48  .495    .2   -99  1.65   .12   -99   -99   -99   -99   6.5   -99   -99   -99

*UFON780006  S25-2-(1-7) S        203  Pomona Fine Sand
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Ona REC     USA             27.42  -81.92 Ultic Haplaquods, sandy, siliceous, hyperthermic
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
    BL   .09   5.9    .6    30     1   .92 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     8    A1  .027   .11  .471     1    21  1.32  1.02    .6   5.5     0   -99   5.2   -99   8.1   -99
    25   A21  .021  .089  .389     1  10.9  1.56   .27    .5   3.4     0   -99   4.9   -99    .3   -99
    68   A22  .023  .091  .347     1  13.8  1.68   .11    .4   2.3     0   -99   4.9   -99     0   -99
    89   B2H   .02  .087  .307    .8   3.2  1.78   .62   1.6   5.4     0   -99   4.5   -99   5.3   -99
   117    B3  .025  .097  .313    .8   4.7  1.77   .35   1.1   2.9     0   -99   4.7   -99   2.3   -99
   145   A'2  .025  .097   .32    .6   2.2  1.75   .28   2.6   2.9     0   -99     5   -99   2.5   -99
   203  B'TG  .175  .261  .336    .6   2.2  1.75   .13  16.6   3.3     0   -99     5   -99    11   -99

*IBSB910032  Sioux       -99      182  Sioux Loam
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 -99         -99               -99     -99 Sioux Loam
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .11     9    .5    77     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     7   -99  .265  .467  .653     1   -99   1.2     0   -99   -99   -99   -99   -99   -99   -99   -99
    19   -99   .27  .444   .58     1   -99   1.2     0   -99   -99   -99   -99   -99   -99   -99   -99
    32   -99   .27  .444   .58     1   -99   1.2     0   -99   -99   -99   -99   -99   -99   -99   -99
    47   -99   .27  .444   .58    .5   -99   1.2     0   -99   -99   -99   -99   -99   -99   -99   -99
    62   -99   .27  .444   .58    .5   -99   1.2     0   -99   -99   -99   -99   -99   -99   -99   -99
    92   -99  .202  .333  .435   .25   -99   1.2     0   -99   -99   -99   -99   -99   -99   -99   -99
   122   -99  .202  .333  .435    .1   -99   1.2     0   -99   -99   -99   -99   -99   -99   -99   -99
   152   -99  .202  .333  .435     0   -99   1.2     0   -99   -99   -99   -99   -99   -99   -99   -99
   182   -99  .202  .333  .435     0   -99   1.2     0   -99   -99   -99   -99   -99   -99   -99   -99

*IBSB910049  Nicollet mo -99      202  NICOLLET CLAY LOAM
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 -99         -99               -99     -99 NICOLLET CLAY LOAM (FINE-LOAMY,MIXED,MESIC AQUIC H
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .09  10.6    .6    75     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99  .153  .283   .34     1   -99  1.37     0   -99   -99   -99   -99     7   -99   -99   -99
    17   -99  .143  .283   .34     1   -99  1.37     0   -99   -99   -99   -99     7   -99   -99   -99
    29   -99   .22   .33   .39    .8   -99  1.37     0   -99   -99   -99   -99     7   -99   -99   -99
    43   -99  .205   .34    .4    .8   -99  1.37     0   -99   -99   -99   -99     7   -99   -99   -99
    55   -99  .205   .34    .4    .8   -99  1.35     0   -99   -99   -99   -99     7   -99   -99   -99
    67   -99  .205   .34    .4    .7   -99  1.35     0   -99   -99   -99   -99     7   -99   -99   -99
    79   -99  .205   .33   .39    .7   -99  1.35     0   -99   -99   -99   -99     7   -99   -99   -99
    94   -99   .19   .32   .38    .6   -99  1.35     0   -99   -99   -99   -99     7   -99   -99   -99
   123   -99   .16    .3  .355    .6   -99   1.4     0   -99   -99   -99   -99     7   -99   -99   -99
   152   -99   .14    .3  .355    .5   -99   1.4     0   -99   -99   -99   -99     7   -99   -99   -99
   177   -99   .14    .3  .355    .5   -99   1.4     0   -99   -99   -99   -99     7   -99   -99   -99
   202   -99   .14    .3  .355    .5   -99   1.4     0   -99   -99   -99   -99     7   -99   -99   -99

*IUBF970110  wine2   IA  L        210  Ritchie 6/12/97  Loam
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 -99         -99               -99     -99 
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .09     7   .55    70    .5     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99  .183   .36  .465     1    79  1.31  2.03    26    36   -99   -99   6.4   -99   -99   -99
    15   -99  .183   .36  .438     1    79  1.39  2.03    26    36   -99   -99   6.4   -99   -99   -99
    30   -99  .183   .36  .451     1    79  1.35  2.03    27    37   -99   -99   6.4   -99   -99   -99
    45   -99  .183   .36  .458     1    79  1.33  2.03    27    37   -99   -99   6.4   -99   -99   -99
    60   -99  .183   .36  .451     1    79  1.35   .44    27    37   -99   -99   6.7   -99   -99   -99
    75   -99  .183   .36  .458     1    79  1.33   .44    29    36   -99   -99   6.7   -99   -99   -99
    90   -99  .183   .36  .458     1    79  1.33   .44    29    36   -99   -99   6.7   -99   -99   -99
   105   -99  .163   .33  .434   .85    79   1.4   .15    20    28   -99   -99   7.9   -99   -99   -99
   120   -99  .163   .33   .41    .7    79  1.47   .15    20    28   -99   -99   7.9   -99   -99   -99
   135   -99  .163   .33  .396   .55    79  1.51   .15    18    31   -99   -99   7.9   -99   -99   -99
   150   -99  .163   .33  .403    .4    79  1.49   .15    18    31   -99   -99   7.9   -99   -99   -99
   165   -99  .163   .33  .403    .3    79  1.49   .15    18    31   -99   -99   7.9   -99   -99   -99
   180   -99  .163   .33  .403    .2    79  1.49   .15    18    31   -99   -99   7.9   -99   -99   -99
   195   -99  .163   .33  .403    .1    79  1.49   .15    18    31   -99   -99   7.9   -99   -99   -99
   210   -99  .163   .33  .403     0    79  1.49   .15    18    31   -99   -99   7.9   -99   -99   -99
! tiledrain at 100 cm

*IUBF970211  SOREN-  IA  L        210  Ritchie 6/12/97  Loam
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 -99         -99               -99     -99 
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .09     7   .55    60    .5   .92 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99  .163   .34  .465     1    79  1.31  2.03    26    36   -99   -99   6.4   -99   -99   -99
    15   -99  .163   .34  .438     1    79  1.39  2.03    26    36   -99   -99   6.4   -99   -99   -99
    30   -99  .173   .35  .451     1    79  1.35  2.03    27    37   -99   -99   6.4   -99   -99   -99
    45   -99  .193   .35  .458     1    79  1.33  2.03    27    37   -99   -99   6.4   -99   -99   -99
    60   -99  .193   .35  .451     1    79  1.35   .44    27    37   -99   -99   6.7   -99   -99   -99
    75   -99  .173   .35  .458     1    79  1.33   .44    29    36   -99   -99   6.7   -99   -99   -99
    90   -99  .173   .35  .458     1    79  1.33   .44    29    36   -99   -99   6.7   -99   -99   -99
   105   -99  .163   .33  .434   .85    79   1.4   .15    20    28   -99   -99   7.9   -99   -99   -99
   120   -99  .163   .33   .41    .7    79  1.47   .15    20    28   -99   -99   7.9   -99   -99   -99
   135   -99  .163   .33  .396   .55    79  1.51   .15    18    31   -99   -99   7.9   -99   -99   -99
   150   -99  .163   .33  .403    .4    79  1.49   .15    18    31   -99   -99   7.9   -99   -99   -99
   165   -99  .163   .33  .403    .3    79  1.49   .15    18    31   -99   -99   7.9   -99   -99   -99
   180   -99  .163   .33  .403    .2    79  1.49   .15    18    31   -99   -99   7.9   -99   -99   -99
   195   -99  .163   .33  .403    .1    79  1.49   .15    18    31   -99   -99   7.9   -99   -99   -99
   210   -99  .163   .33  .403     0    79  1.49   .15    18    31   -99   -99   7.9   -99   -99   -99
!   -99  0.09   7.0  0.55  60.0  0.50  1.00 IB001 IB001 IB001, KJB - 5/31/09 decreased .99 to .92

*LUGO930001  SAU CAL SOY SCL      150  FINCA DE PRACTICAS EPS CALICATA 1 (LUGO) Plus 0.01
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 LUGO        SPAIN           43.04    -3.3 TYPIC HAPLUMREPT
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
    YR   .14   9.7    .6    80     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    15    A1  .121  .265  .348     1  21.5   .95  2.83    21    26   9.9  4.88   6.5   5.3  15.4   -99
    30    A2  .125  .274   .35   .75  28.1  1.12  2.34    21    27   4.8  4.03   6.4     5  11.4   -99
    50     B  .111   .26  .354    .5  50.3  1.26   .56    18    32   6.9   .96     6     4   6.6   -99
    65    BC  .101  .252  .344   .35    31  1.31   .48    15    29   2.2   .83   5.2   3.7   5.9   -99
   150     C  .085  .232  .321    .2  12.9  1.35   .41    11    19    .2   .72   4.9   3.3     5   -99

*IBSB910046  Wooster     -99      153  WOOSTER SILT LOAM
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 -99         -99               -99     -99 WOOSTER SILT LOAM (FINE-LOAMY,MIXED MESIC TYPIC FR
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .13   8.8    .4    84     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99  .042  .222  .427    .1   -99   1.4   2.5   -99   -99   -99   -99     6   -99   -99   -99
    15   -99  .042  .222  .427   .75   -99   1.4   2.5   -99   -99   -99   -99     6   -99   -99   -99
    25   -99  .042  .222  .427   .75   -99   1.4     0   -99   -99   -99   -99   5.5   -99   -99   -99
    33   -99  .083  .223  .427    .5   -99   1.5     0   -99   -99   -99   -99   5.5   -99   -99   -99
    41   -99  .083  .223  .427    .5   -99   1.5     0   -99   -99   -99   -99   5.5   -99   -99   -99
    54   -99  .112  .252  .372   .35   -99   1.5     0   -99   -99   -99   -99   5.5   -99   -99   -99
    62   -99  .112  .252  .372   .35   -99   1.5     0   -99   -99   -99   -99   5.5   -99   -99   -99
    70   -99  .112  .252  .372   .35   -99   1.5     0   -99   -99   -99   -99   5.5   -99   -99   -99
    98   -99  .116  .196  .315    .2   -99  1.65     0   -99   -99   -99   -99   5.6   -99   -99   -99
   123   -99  .116  .196  .315    .2   -99  1.65     0   -99   -99   -99   -99   5.5   -99   -99   -99
   148   -99  .116  .196  .315   .15   -99  1.65     0   -99   -99   -99   -99   5.5   -99   -99   -99
   153   -99  .116  .196  .315   .15   -99  1.53     0   -99   -99   -99   -99     6   -99   -99   -99

*IBSB910064              -99      205  Deep Silt Loam
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 -99         -99               -99     -99 DEEP SILT LOAM
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .12     6    .4    77     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    10   -99  .106  .262  .362     1   -99  1.37  1.16   -99   -99   -99   -99   6.5   -99   -99   -99
    25   -99  .106  .262  .362  .819   -99  1.37   1.1   -99   -99   -99   -99   6.5   -99   -99   -99
    40   -99  .107  .262  .362  .607   -99  1.37   .97   -99   -99   -99   -99   6.5   -99   -99   -99
    55   -99  .107  .262  .362  .607   -99  1.37   .97   -99   -99   -99   -99   6.5   -99   -99   -99
    85   -99  .108  .261  .361  .368   -99  1.38   .72   -99   -99   -99   -99   6.5   -99   -99   -99
   115   -99   .11   .26   .36  .202   -99  1.38   .43   -99   -99   -99   -99   6.5   -99   -99   -99
   145   -99  .111  .259  .359  .111   -99  1.39    .2   -99   -99   -99   -99   6.5   -99   -99   -99
   175   -99  .112  .258  .358  .061   -99  1.39   .06   -99   -99   -99   -99   6.5   -99   -99   -99
   205   -99  .112  .258  .358  .033   -99  1.39   .01   -99   -99   -99   -99   6.5   -99   -99   -99

*ISUV950008  AEES, AMES  -99      152  CLARION LOAM
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 -99         -99                42     -99 TYPIC HAPLUDOLL, FINE-LOAMY, MIXED, MESIC ( 31)
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .09   9.8    .4    84    .5     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99   .11    .3  .361     1   3.3  1.45  2.03    21   -99   -99   -99   6.4   -99   -99   -99
    18   -99   .11    .3  .361     1   3.3  1.45  2.03    21   -99   -99   -99   6.4   -99   -99   -99
    31   -99   .11    .3  .361     1   3.3  1.45  2.03    21   -99   -99   -99   6.4   -99   -99   -99
    46   -99   .11    .3  .361     1   3.3  1.45  2.03    27   -99   -99   -99   6.4   -99   -99   -99
    56   -99  .129   .31  .371     1   3.3   1.6   .44    27   -99   -99   -99   6.7   -99   -99   -99
    66   -99  .129   .31  .371     1   3.3   1.6   .44    27   -99   -99   -99   6.7   -99   -99   -99
    91   -99  .129   .31  .371    .5   3.3   1.6   .44    27   -99   -99   -99   6.7   -99   -99   -99
   111   -99  .107  .229  .369    .5   3.3   1.6   .15    17   -99   -99   -99   7.9   -99   -99   -99
   132   -99  .107  .229  .369    .5   3.3   1.6   .15    17   -99   -99   -99   7.9   -99   -99   -99
   152   -99  .107  .229  .369    .5   3.3   1.6   .15    17   -99   -99   -99   7.9   -99   -99   -99

*MSKB890006  IBSNAT      L        120  KALAMAZOO LOAM
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Kalamazoo   MI, U.S.A.       41.7   -85.5 Typic Hapludalf
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
  10YR   .13     9   .15    80     1     1 SA001 SA001 SA001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    10    Ap  .137   .27   .38     1   -99   1.6   1.1    19    38   -99   .13   6.5   -99    10   -99
    22    Ap  .137   .27   .38     1   -99   1.6    .9    19    38   -99   .13   6.5   -99    10   -99
    31     E  .137   .27   .38     1   -99   1.6    .7    22    47   -99   .13   6.5   -99    10   -99
    41   Btu  .165  .298  .343    .9   -99   1.6    .3    23    44   -99   .05   6.5   -99    10   -99
    51   Btm  .165    .3   .41    .7   -99   1.6   .22    25    19   -99   .04     6   -99    10   -99
    61   Btl  .137   .27   .38    .8   -99   1.6    .1    21    17   -99   .04     6   -99    10   -99
    75  Bt2u  .137   .27   .38    .4   -99   1.6   .05    19    12   -99   .02     6   -99    10   -99
    89  Bt2l   .06  .162   .27    .1   -99   1.6   .02     7     4   -99   .01     6   -99     3   -99
   102  Bt3u   .06  .162   .27     0   -99   1.6   .02     7     5   -99   .01     6   -99     3   -99
   120  Bt3u   .06  .162   .27     0   -99   1.6   .02     7     5   -99   .01     6   -99     3   -99

*IBMZ910214  Gainesville S        180  Millhopper Fine Sand
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Gainesville USA             29.63  -82.37 Loamy,silic,hyperth Arenic Paleudult
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .18     2   .65    60     1   .92 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99  .026  .096  .345     1   7.4  1.66   .67   1.7    .9     0   -99     7   -99    20   -99
    15   -99  .025  .105  .345     1   7.4  1.66   .67   1.7    .9     0   -99     7   -99   -99   -99
    30   -99  .075   .12  .345    .7  14.8  1.66   .17   2.4   2.6     0   -99     7   -99   -99   -99
    45   -99  .025  .086  .345    .3   3.7  1.66   .17   2.4   2.6     0   -99     7   -99   -99   -99
    60   -99  .025  .072  .345    .3   3.7  1.66   .17   2.4   2.6     0   -99     7   -99   -99   -99
    90   -99  .028  .072  .345    .1   3.7  1.66   .17   2.4   2.6     0   -99     7   -99   -99   -99
   120   -99  .028   .08  .345    .1    .1  1.66   .18   7.7   3.1     0   -99     7   -99   -99   -99
   150   -99  .029   .09  .345   .05    .1  1.66   .15   7.7   3.1     0   -99     7   -99   -99   -99
   180   -99  .029   .09  .345   .05    .1  1.66    .1   7.7   3.1     0   -99     7   -99   -99   -99

*UFBG760002  SCS         S         71  Lauderhill Muck
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 EREC        USA              26.4    80.4 euic, hyperthermic Lithic Haplosaprist
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
    BL   .09     6   .25    61     1     1 IB001 SA001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    20   Oap   .15   .25    .5     1    21   .35    45     5     5     0   3.5   6.2   -99   211   -99
    45   Oa2   .15   .25    .5  .522    21   .35    46     5     5     0   3.5   6.3   -99   210   -99
    60   Oa3   .15   .25    .5   .33    21   .35    40     5     5     0   3.5   6.6   -99   141   -99
    71   -99  .026  .058  .402   .01     0  1.53     0     0     0    80   -99   8.4   -99   -99   -99
@  SLB  SLPX  SLPT  SLPO CACO3  SLAL  SLFE  SLMN  SLBS  SLPA  SLPB  SLKE  SLMG  SLNA  SLSU  SLEC  SLCA
    20    96   210   187   .27   2.3   2.3   -99    .1   -99   -99   .08   .13     0  13.3   -99   .27
    45    56   183   155   .12  2.01   2.3   -99   -99   -99   -99   .03   .05     0  15.3   -99   .12
    60    40 150.3   100   .11  1.25   2.3   -99   -99   -99   -99   .03   .04     0  14.1   -99   .11
    71    .1    .1    .1   .09   .09    .9   .09    .1   .09   .09   .09   .09     0 90.09   .09    .1

*IBGB910015  SCS-Desire  S        180  Millhopper Fine Sand
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Gainesville USA             29.63  -82.37 Loamy,silic,hyperth Gross. Paleudults
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .18     5    .5    66     1   .92 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99  .023  .086   .23     1   7.4  1.47    .9    .9  11.8   -99   -99   5.3   -99   -99   -99
    15   -99  .023  .086   .23     1   7.4  1.47   .69    .9  11.8   -99   -99   5.4   -99   -99   -99
    30   -99  .023  .086   .23    .9  15.8  1.41   .28   4.6   6.4   -99   -99   5.7   -99   -99   -99
    45   -99  .023  .086   .23    .7    28  1.43    .2   5.8   5.4   -99   -99   5.8   -99   -99   -99
    60   -99  .023  .086   .23    .5    28  1.43    .2   5.8   5.4   -99   -99   5.8   -99   -99   -99
    90   -99  .021  .076   .23    .2  27.6  1.52   .09   9.6   4.2   -99   -99   5.9   -99   -99   -99
   120   -99   .02  .076   .23    .1  17.5  1.52   .03   9.6   4.2   -99   -99   5.9   -99   -99   -99
   150   -99  .027   .13   .23   .05    .3  1.46   .03   8.3   3.6   -99   -99   5.9   -99   -99   -99
   180   -99   .07  .258   .36     0    .1  1.46   .03   8.3   3.6   -99   -99   5.9   -99   -99   -99

*UFGA010700  SCS         S        180  Candler
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Gainesville USA             29.63  -82.37 Hyperthermic uncoated
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
     Y   .17     6   .85    64     1   .95 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99  .045  .091  .412     1    21  1.49   .48   1.3   1.3   -99   -99   5.5   -99   -99   -99
    15   -99  .045  .091  .412     1    21  1.49   .48   1.3   1.3   -99   -99   5.5   -99   -99   -99
    30   -99  .037  .076  .404  .638    21  1.52   .18   1.3   1.3   -99   -99   5.3   -99   -99   -99
    45   -99  .034  .071  .415  .472    21  1.49   .09   1.3   1.3   -99   -99   5.1   -99   -99   -99
    60   -99  .034  .071  .415   .35    21  1.49   .09   1.3   1.3   -99   -99   5.1   -99   -99   -99
    90   -99  .043  .081  .412  .223    21   1.5   .09     3   1.2   -99   -99   5.2   -99   -99   -99
   120   -99  .042  .078  .412  .122    21   1.5   .02     3   1.2   -99   -99   5.3   -99   -99   -99
   150   -99  .047  .084  .416  .067    21  1.49   .02     4   1.5   -99   -99   5.2   -99   -99   -99
   180   -99  .047  .084  .416  .037    21  1.49   .02     4   1.5   -99   -99   5.2   -99   -99   -99

*UFGA017000  SCS         S        180  Tavares
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Gainesville USA             29.63  -82.37 Sand uncoated
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
     Y   .17     6    .4    64     1   .95 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99  .072  .142  .364     1    21   1.6  1.41   1.5   1.5   -99   -99   4.2   -99   -99   -99
    15   -99  .072  .142  .364     1    21   1.6  1.41   1.5   1.5   -99   -99   4.2   -99   -99   -99
    30   -99  .051  .102  .371  .638    21   1.6   .66   1.5   1.2   -99   -99   4.3   -99   -99   -99
    45   -99  .044  .091  .373  .472    21   1.6   .44   1.3   1.5   -99   -99   4.3   -99   -99   -99
    60   -99  .045  .092  .347   .35    21  1.67   .44   1.5   1.5   -99   -99   4.2   -99   -99   -99
    90   -99  .038  .078   .35  .223    21  1.67   .18   1.5   1.5   -99   -99   4.2   -99   -99   -99
   120   -99  .046  .086  .364  .122    21  1.63   .18     3   1.2   -99   -99   4.3   -99   -99   -99
   150   -99  .048  .087  .365  .067    21  1.63   .08     4   1.5   -99   -99   4.3   -99   -99   -99
   180   -99  .052  .091  .358  .037    21  1.65   .05     5   1.5   -99   -99   4.4   -99   -99   -99

*UFGA950002  Alachua cou S        203  Millhopper Fine Sand (Compacted layer)
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 99          -99               -99     -99 oamy, siliceous hyperthermic Grossarenic Paleudult
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .18     5    .5    66     1   .92  B001  B001  B001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    15   -99  .024  .161  .487     1   9.3  1.36   .73   2.1   6.3     0   .08   5.2   -99   5.4   -99
    33   -99  .016  .104  .407    .3  24.4  1.57   .34   2.3   4.1     0   .03   5.4   -99   2.9   -99
    89   -99  .011  .113  .408     0  .001  1.57   .19   2.4   3.3     0   .02   5.2   -99   1.9   -99
   147   -99  .021   .08  .385   .05  32.9  1.63   .07   1.9   3.3     9   .01   5.3   -99   1.1   -99
   157   -99  .022  .101  .404     0  19.8  1.58   .07   4.1   3.9     0   .01   5.2   -99     2   -99
   175   -99   .07  .196  .374     0     4  1.66    .1  16.4   6.1     0   .01   4.9   -99     6   -99
   203   -99  .105  .248  .351     0    .9  1.72   .12  28.6   8.6     0   .01     5   -99  10.4   -99

*UFGA950003  Alachua cou S        203  Millhopper Fine Sand (Mulched)
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 99          -99               -99     -99 oamy, siliceous hyperthermic Grossarenic Paleudult
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .18     5    .5    66     1   .92  B001  B001  B001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     2   -99  .024  .161  .487     1   .01  1.36   .73   2.1   6.3     0   .08   5.2   -99   5.4   -99
    15   -99  .024  .161  .487     1   9.3  1.36   .73   2.1   6.3     0   .08   5.2   -99   5.4   -99
    33   -99  .016  .104  .407    .3  24.4  1.55   .34   2.3   4.1     0   .03   5.4   -99   2.9   -99
    89   -99  .011  .113  .408    .1  31.9  1.57   .19   2.4   3.3     0   .02   5.2   -99   1.9   -99
   147   -99  .021   .08  .385   .05  32.9  1.63   .07   1.9   3.3     9   .01   5.3   -99   1.1   -99
   157   -99  .022  .101  .404     0  19.8  1.58   .07   4.1   3.9     0   .01   5.2   -99     2   -99
   175   -99   .07  .196  .374     0     4  1.66    .1  16.4   6.1     0   .01   4.9   -99     6   -99
   203   -99  .105  .248  .351     0    .9  1.72   .12  28.6   8.6     0   .01     5   -99  10.4   -99

*IBTM910017  SCS         -99      203  Orangeburg Sandy Loam
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Quincy      USA              30.6   -86.4 Loamy,silic,hyperth Gross. Paleud(17)
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .13     9   .27    84     1   .96 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99  .125  .198  .294     1   -99  1.49  1.73   -99   -99   -99   -99   -99   -99   -99   -99
    15   -99  .125  .198  .294  .874   -99  1.49  1.73   -99   -99   -99   -99   -99   -99   -99   -99
    25   -99  .125  .198  .294  .874   -99  1.49  1.73   -99   -99   -99   -99   -99   -99   -99   -99
    34   -99  .117  .226  .323  .351   -99  1.41    .4   -99   -99   -99   -99   -99   -99   -99   -99
    43   -99  .117  .226  .323  .351   -99  1.41    .4   -99   -99   -99   -99   -99   -99   -99   -99
    53   -99  .138   .25  .332   .31   -99  1.44    .2   -99   -99   -99   -99   -99   -99   -99   -99
    64   -99  .138   .25  .332   .31   -99  1.44    .2   -99   -99   -99   -99   -99   -99   -99   -99
   102   -99  .167  .281  .331  .302   -99  1.57   .14   -99   -99   -99   -99   -99   -99   -99   -99
   145   -99  .182  .291  .334  .077   -99  1.59   .16   -99   -99   -99   -99   -99   -99   -99   -99
   175   -99  .162  .272   .32  .036   -99  1.61   .09   -99   -99   -99   -99   -99   -99   -99   -99
   203   -99  .154  .263  .319  .006   -99  1.58   .03   -99   -99   -99   -99   -99   -99   -99   -99

*IBPP910015  SCS         -99      180  Millhopper Fine Sand
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Gainesville USA             29.63  -82.37 Loamy,silic,hyperth Gross. Paleudults
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .18     5    .5    66     1   .92 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5   -99  .023  .086   .23     1   7.4  1.36    .9   -99   -99   -99   -99   5.3   -99   -99   -99
    15   -99  .023  .086   .23     1   7.4   1.4   .69   -99   -99   -99   -99   5.4   -99   -99   -99
    30   -99  .023  .086   .23  .498  15.8  1.46   .28   -99   -99   -99   -99   5.7   -99   -99   -99
    60   -99  .023  .086   .23  .294    28  1.47    .2   -99   -99   -99   -99   5.8   -99   -99   -99
    90   -99  .021  .076   .23   .38  27.6  1.43   .09   -99   -99   -99   -99   5.9   -99   -99   -99
   120   -99   .02  .076   .23  .066  17.5  1.48   .03   -99   -99   -99   -99   5.9   -99   -99   -99
   150   -99  .027   .13   .23  .031    .3  1.57   .03   -99   -99   -99   -99   5.9   -99   -99   -99
   180   -99   .07  .258   .36  .015    .1  1.79   .03   -99   -99   -99   -99   5.9   -99   -99   -99

*IN00020001  SCS         -99      160  BLACK SOIL, JNKVV, INDORE,MP
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 JNKVV       INDIA            22.4    75.5 TYPIC HAPLUSTERT, FINE, MONTMORILLONITIC
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
    BN   .11     6    .5    70     1   .88 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     5    AP  .177    .4  .602     1   -99  1.36    .6   -99   -99   -99   -99     8   -99   -99   -99
    15    AP  .177    .4  .602     1   -99  1.36    .6   -99   -99   -99   -99     8   -99   -99   -99
    30    AP  .177    .4  .602     1   -99  1.36    .6   -99   -99   -99   -99     8   -99   -99   -99
    60     A  .177   .39  .603     1   -99  1.36   .64   -99   -99   -99   -99   8.1   -99   -99   -99
    90     B  .177   .39  .603     1   -99  1.36   .64   -99   -99   -99   -99   8.1   -99   -99   -99
   120    BS  .177   .39  .602     1   -99  1.36    .6   -99   -99   -99   -99   8.1   -99   -99   -99
   150    BS  .177   .39  .603     1   -99  1.36    .6   -99   -99   -99   -99   8.1   -99   -99   -99
   160    BS  .177   .39  .603     1   -99  1.36    .6   -99   -99   -99   -99   8.1   -99   -99   -99

*AUNR810111  Literature  CL       180  Vertisol
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Narrabri    Australia       30.15  149.41 Vertisol
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .13     6    .5    73     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     0     1   .23    .5   .55     1   .23  1.42   -99    35    30   -99   -99   -99   -99   -99   -99
    20     2   .24  .465  .515     1   .23  1.42   -99    35    30   -99   -99   -99   -99   -99   -99
    40     3   .24  .445  .495  .549   .23  1.42   -99    35    30   -99   -99   -99   -99   -99   -99
    60     4   .25  .425  .475  .368   .23  1.42   -99    35    30   -99   -99   -99   -99   -99   -99
    80     5   .27   .41   .46  .247   .23  1.42   -99    35    30   -99   -99   -99   -99   -99   -99
   100     6   .31  .385  .435  .165   .23  1.42   -99    35    30   -99   -99   -99   -99   -99   -99
   120     7   .38  .455  .505  .111   .23  1.42   -99    35    30   -99   -99   -99   -99   -99   -99
   140     8   .38   .43   .48  .074   .23  1.42   -99    35    30   -99   -99   -99   -99   -99   -99
   160     9   .38   .41   .46   .05   .23  1.42   -99    35    30   -99   -99   -99   -99   -99   -99
   180    10   .38  .395  .445  .033   .23  1.42   -99    35    30   -99   -99   -99   -99   -99   -99

*AUUC810111  Literature  CL       180  Red sodosol
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Tatura      Australia       36.27  145.14 Clayey soil
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .13     6    .4    80     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     0     1   .23  .455  .505     1   .23   1.3  1.03    35    30   -99   -99   8.2   -99   -99   -99
    20     2   .24   .49   .54     1   .23   1.3   .86    35    30   -99   -99   8.5   -99   -99   -99
    40     3   .24   .49   .54  .549   .23  1.29   .77    35    30   -99   -99   8.6   -99   -99   -99
    60     4   .25    .5   .55  .368   .23  1.31   .65    35    30   -99   -99   8.6   -99   -99   -99
    80     5   .27   .42   .47  .247   .23  1.35   .54    35    30   -99   -99   7.4   -99   -99   -99
   100     6   .31  .385  .435  .165   .23  1.36   .48    35    30   -99   -99   5.3   -99   -99   -99
   120     7   .38   .43   .48  .111   .23  1.36   .43    35    30   -99   -99     5   -99   -99   -99
   140     8   .38  .405  .455  .074   .23  1.36   .43    35    30   -99   -99     5   -99   -99   -99
   160     9   .38  .405  .455   .05   .23  1.36   .43    35    30   -99   -99     5   -99   -99   -99
   180    10   .38   .38   .43  .033   .23  1.36   .43    35    30   -99   -99     5   -99   -99   -99

*AUBL730001  Literature  CL       180  Alluvial soil
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Biloela     Australia       24.24  150.31 Light textured soil
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .13     6    .4    80     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     0     1   .23   .43   .48     1   .23   1.3  1.03    35    30   -99   -99   8.2   -99   -99   -99
    20     2   .24  .428  .478     1   .23   1.3   .86    35    30   -99   -99   8.5   -99   -99   -99
    40     3   .24  .428  .478  .549   .23  1.29   .77    35    30   -99   -99   8.6   -99   -99   -99
    60     4   .25  .438  .488  .368   .23  1.31   .65    35    30   -99   -99   8.6   -99   -99   -99
    80     5   .27  .458  .507  .247   .23  1.35   .54    35    30   -99   -99   7.4   -99   -99   -99
   100     6   .31  .498  .548  .165   .23  1.36   .48    35    30   -99   -99   5.3   -99   -99   -99
   120     7   .38  .567  .618  .111   .23  1.36   .43    35    30   -99   -99     5   -99   -99   -99
   140     8   .38   .38   .43  .074   .23  1.36   .43    35    30   -99   -99     5   -99   -99   -99
   160     9   .38   .38   .43   .05   .23  1.36   .43    35    30   -99   -99     5   -99   -99   -99
   180    10   .38   .38   .43  .033   .23  1.36   .43    35    30   -99   -99     5   -99   -99   -99

*XXLL741111  Literature  CL       180  Alluvial light textured soil
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Biloela     Australia       24.24  150.31 Light textured soil
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .13     6    .4    80     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     0     1   .23   .43   .48     1   .23   1.3  1.03    35    30   -99   -99   8.2   -99   -99   -99
    20     2   .24   .39   .44     1   .23   1.3   .86    35    30   -99   -99   8.5   -99   -99   -99
    40     3   .24   .39   .44  .549   .23  1.29   .77    35    30   -99   -99   8.6   -99   -99   -99
    60     4   .25    .4   .45  .368   .23  1.31   .65    35    30   -99   -99   8.6   -99   -99   -99
    80     5   .27   .42   .47  .247   .23  1.35   .54    35    30   -99   -99   7.4   -99   -99   -99
   100     6   .31   .46   .51  .165   .23  1.36   .48    35    30   -99   -99   5.3   -99   -99   -99
   120     7   .38   .53   .58  .111   .23  1.36   .43    35    30   -99   -99     5   -99   -99   -99
   140     8   .38   .38   .43  .074   .23  1.36   .43    35    30   -99   -99     5   -99   -99   -99
   160     9   .38   .38   .43   .05   .23  1.36   .43    35    30   -99   -99     5   -99   -99   -99
   180    10   .38   .38   .43  .033   .23  1.36   .43    35    30   -99   -99     5   -99   -99   -99

*AUBL720111  Scot's file CL       180  Alluvial set I
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Bileola     Australia       24.42  150.31 light textured soil
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .13     6    .6    80     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     0     1   .23   .43   .48     1   .23   1.3  1.03    35    30   -99   -99   8.2   -99   -99   -99
    20     2   .24  .428  .478     1   .23   1.3   .86    35    30   -99   -99   8.5   -99   -99   -99
    40     3   .24  .428  .478  .549   .23  1.29   .77    35    30   -99   -99   8.6   -99   -99   -99
    60     4   .25  .438  .488  .368   .23  1.31   .65    35    30   -99   -99   8.6   -99   -99   -99
    80     5   .27  .458  .507  .247   .23  1.35   .54    35    30   -99   -99   7.4   -99   -99   -99
   100     6   .31   .46   .51  .165   .23  1.36   .48    35    30   -99   -99   5.3   -99   -99   -99
   120     7   .38   .53   .58  .111   .23  1.36   .43    35    30   -99   -99     5   -99   -99   -99
   140     8   .38   .38   .43  .074   .23  1.36   .43    35    30   -99   -99     5   -99   -99   -99
   160     9   .38   .38   .43   .05   .23  1.36   .43    35    30   -99   -99     5   -99   -99   -99
   180    10   .38   .38   .43  .033   .23  1.36   .43    35    30   -99   -99     5   -99   -99   -99

*AUNR810112  Literature  CL       180  Alluvial soil
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Bileola     Australia       24.42  150.31 Light textured soil
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
   -99   .13     6    .6    80     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
     0     1   .23   .43   .48     1   .23   1.3  1.03    35    30   -99   -99   8.2   -99   -99   -99
    20     2   .24  .428  .478     1   .23   1.3   .86    35    30   -99   -99   8.5   -99   -99   -99
    40     3   .24  .428  .478  .549   .23  1.29   .77    35    30   -99   -99   8.6   -99   -99   -99
    60     4   .25  .438  .488  .368   .23  1.31   .65    35    30   -99   -99   8.6   -99   -99   -99
    80     5   .27  .458  .507  .247   .23  1.35   .54    35    30   -99   -99   7.4   -99   -99   -99
   100     6   .31   .46   .51  .165   .23  1.36   .48    35    30   -99   -99   5.3   -99   -99   -99
   120     7   .38   .53   .58  .111   .23  1.36   .43    35    30   -99   -99     5   -99   -99   -99
   140     8   .38   .38   .43  .074   .23  1.36   .43    35    30   -99   -99     5   -99   -99   -99
   160     9   .38   .38   .43   .05   .23  1.36   .43    35    30   -99   -99     5   -99   -99   -99
   180    10   .38   .38   .43  .033   .23  1.36   .43    35    30   -99   -99     5   -99   -99   -99

*AFSF990001  Excercise   SCL      150  Alluvium,sandstone
@SITE        COUNTRY          LAT     LONG SCS FAMILY
 Umariya     Africa           23.3   80.45 Fine,montmorillonitic
@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
    BN   .13     6    .4    73     1     1 IB001 IB001 IB001
@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
    11    Ap  .175  .271  .415     1   .43  1.47   .97  24.5  13.2   -99   -99   6.7   -99  18.5   -99
    31    B1  .226  .332  .421  .657   .12  1.46   .66  36.4  18.4   -99   -99   6.4   -99  19.3   -99
    50   B12  .256  .348  .408  .445   .12   1.5    .5  43.3  11.1   -99   -99   6.4   -99  24.5   -99
    75    B2  .278  .374  .423  .287   .06  1.46    .4  48.2  12.9   -99   -99   6.4   -99  31.3   -99
   100   B31  .371  .534  .522  .174   .06  1.14     3  52.6  13.6   -99   -99   5.7   -99  33.5   -99
   125   B32  .317  .411  .443  .105   .06  1.41   .25  56.9  10.7   -99   -99   5.6   -99  36.4   -99
   150   B33  .306  .402   .44  .064   .06  1.42    .1  55.4  14.3   -99   -99   5.7   -99  39.5   -99
