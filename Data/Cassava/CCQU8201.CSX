*EXP.DETAILS: CCQU8201CS 1982 WATER SUPPLY * TWO PLANTING AGE

*GENERAL
@PEOPLE
PORTO, M.C.M. COCK,J.H.
@ADDRESS
CIAT,A.A.6713,CALI,COLOMBIA
@SITE
SANTANDER DE QUILICHAO,COLOMBIA  3.10;-76.51;990;CW
@NOTES
Water stress at 3 and 6 months old plants PhD Thesis Porto Physiological Mechanisms of Drought Tolerance in Cassava. 1983, 115 p.

*TREATMENTS                        -------------FACTOR LEVELS------------
@N R O C TNAME.................... CU FL SA IC MP MI MF MR MC MT ME MH SM
 1 1 0 0 3 months MCol-1684Control  1  1  0  0  1  0  1  0  0  0  0  1  1
 2 1 0 0 6 months MCol-1684Control  1  1  0  0  2  0  2  0  0  0  0  1  2
 3 1 0 0 3 months MCol-1684Stress   1  1  0  0  1  0  1  0  0  0  1  1  3
 4 1 0 0 6 months MCol-1684Stress   1  1  0  0  2  0  2  0  0  0  1  1  4

*CULTIVARS
@C CR INGENO CNAME
 1 CS UC0006 MCol-1684

*FIELDS
@L ID_FIELD WSTA....  FLSA  FLOB  FLDT  FLDD  FLDS  FLST SLTX  SLDP  ID_SOIL    FLNAME
 1 CCQU0001 CCQU         0     0     1     0     0    0F CLLO   260  CCQU000033 -99
@L ...........XCRD ...........YCRD .....ELEV .............AREA .SLEN .FLWR .SLAS FLHST FHDUR
 1               0               0         0                 0     0     0     0   -99   -99

*INITIAL CONDITIONS
@C   PCR ICDAT  ICRT  ICND  ICRN  ICRE  ICWD ICRES ICREN ICREP ICRIP ICRID ICNAME
 1    BN 82237   -99     0     1     1   -99     0     0     0   100    15 -99
@C  ICBL  SH2O  SNH4  SNO3
 1     5   .47   -99   -99
 1    15  .473   -99   -99
 1    28  .491   -99   -99
 1    44   .51   -99   -99
 1    65  .527   -99   -99
 1    96  .536   -99   -99
 1   122  .549   -99   -99
 1   150  .553   -99   -99
 1   178  .541   -99   -99
 1   196  .488   -99   -99
@C   PCR ICDAT  ICRT  ICND  ICRN  ICRE  ICWD ICRES ICREN ICREP ICRIP ICRID ICNAME
 2    BN 82135   -99     0     1     1   -99     0     0     0   100    15 -99
@C  ICBL  SH2O  SNH4  SNO3
 2     5   .47   -99   -99
 2    15  .473   -99   -99
 2    28  .491   -99   -99
 2    44   .51   -99   -99
 2    65  .527   -99   -99
 2    96  .536   -99   -99
 2   122  .549   -99   -99
 2   150  .553   -99   -99
 2   178  .541   -99   -99
 2   196  .488   -99   -99

*PLANTING DETAILS
@P PDATE EDATE  PPOP  PPOE  PLME  PLDS  PLRS  PLRD  PLDP  PLWT  PAGE  PENV  PLPH  SPRL                        PLNAME
 1 82257   -99  1.25  1.25     V     R   100    90    15   350   -99   -99   -99    20                        -99
 2 82165   -99  1.25  1.25     V     R   100    90    15   350   -99   -99   -99    20                        -99

*FERTILIZERS (INORGANIC)
@F FDATE  FMCD  FACD  FDEP  FAMN  FAMP  FAMK  FAMC  FAMO  FOCD FERNAME
 1 82255 FE001 AP001    20   100     0     0     0     0   -99 -99
 1 82255 FE013 AP001    20     0   200     0     0     0   -99 -99
 1 82255 FE017 AP001    20     0     0   100     0     0   -99 -99
 2 82163 FE001 AP001    20   100     0     0     0     0   -99 -99
 2 82163 FE013 AP001    20     0   200     0     0     0   -99 -99
 2 82163 FE017 AP001    20     0     0   100     0     0   -99 -99

*ENVIRONMENT MODIFICATIONS
@E ODATE EDAY  ERAD  EMAX  EMIN  ERAIN ECO2  EDEW  EWIND ENVNAME  
 1 83003 A   0 A   0 A   0 A   0 M0.00 A   0 A   0 A   0 
 1 83084 A   0 A   0 A   0 A   0 M1.00 A   0 A   0 A   0 

*HARVEST DETAILS
@H HDATE  HSTG  HCOM HSIZE   HPC  HBPC HNAME
 1 83083     6     H     A   100   -99 

*SIMULATION CONTROLS
@N GENERAL     NYERS NREPS START SDATE RSEED SNAME.................... SMODEL
 1 GE              1     1     S 82237  2150 3 months Control          
@N OPTIONS     WATER NITRO SYMBI PHOSP POTAS DISES  CHEM  TILL   CO2
 1 OP              Y     Y     N     N     N     N     N     Y     M
@N METHODS     WTHER INCON LIGHT EVAPO INFIL PHOTO HYDRO NSWIT MESOM MESEV MESOL
 1 ME              M     M     E     R     S     R     R     1     G     S     2
@N MANAGEMENT  PLANT IRRIG FERTI RESID HARVS
 1 MA              R     A     R     R     R
@N OUTPUTS     FNAME OVVEW SUMRY FROPT GROUT CAOUT WAOUT NIOUT MIOUT DIOUT VBOSE CHOUT OPOUT
 1 OU              N     Y     Y     2     Y     N     Y     Y     N     N     A     N     N

@  AUTOMATIC MANAGEMENT
@N PLANTING    PFRST PLAST PH2OL PH2OU PH2OD PSTMX PSTMN
 1 PL          82237 82277    40   100    30    40    10
@N IRRIGATION  IMDEP ITHRL ITHRU IROFF IMETH IRAMT IREFF
 1 IR             30    50   100 GS000 IR001    10     1
@N NITROGEN    NMDEP NMTHR NAMNT NCODE NAOFF
 1 NI             30    50    25 IB001 IB001
@N RESIDUES    RIPCN RTIME RIDEP
 1 RE            100     1    20
@N HARVEST     HFRST HLAST HPCNP HPCNR
 1 HA              0 83083   100     0

@N GENERAL     NYERS NREPS START SDATE RSEED SNAME.................... SMODEL
 2 GE              1     1     S 82135  2150 6 months Control          
@N OPTIONS     WATER NITRO SYMBI PHOSP POTAS DISES  CHEM  TILL   CO2
 2 OP              Y     Y     N     N     N     N     N     Y     M
@N METHODS     WTHER INCON LIGHT EVAPO INFIL PHOTO HYDRO NSWIT MESOM MESEV MESOL
 2 ME              M     M     E     R     S     R     R     1     G     S     2
@N MANAGEMENT  PLANT IRRIG FERTI RESID HARVS
 2 MA              R     A     R     R     R
@N OUTPUTS     FNAME OVVEW SUMRY FROPT GROUT CAOUT WAOUT NIOUT MIOUT DIOUT VBOSE CHOUT OPOUT
 2 OU              N     Y     Y     2     Y     N     Y     Y     N     N     A     N     N

@  AUTOMATIC MANAGEMENT
@N PLANTING    PFRST PLAST PH2OL PH2OU PH2OD PSTMX PSTMN
 2 PL          82135 82275    40   100    30    40    10
@N IRRIGATION  IMDEP ITHRL ITHRU IROFF IMETH IRAMT IREFF
 2 IR             30    50   100 GS000 IR001    10     1
@N NITROGEN    NMDEP NMTHR NAMNT NCODE NAOFF
 2 NI             30    50    25 IB001 IB001
@N RESIDUES    RIPCN RTIME RIDEP
 2 RE            100     1    20
@N HARVEST     HFRST HLAST HPCNP HPCNR
 2 HA              0 83083   100     0

@N GENERAL     NYERS NREPS START SDATE RSEED SNAME.................... SMODEL
 3 GE              1     1     S 82237  2150 3 months Stress           
@N OPTIONS     WATER NITRO SYMBI PHOSP POTAS DISES  CHEM  TILL   CO2
 3 OP              Y     N     N     N     N     N     N     Y     M
@N METHODS     WTHER INCON LIGHT EVAPO INFIL PHOTO HYDRO NSWIT MESOM MESEV MESOL
 3 ME              M     M     E     R     S     R     R     1     G     S     2
@N MANAGEMENT  PLANT IRRIG FERTI RESID HARVS
 3 MA              R     N     R     R     R
@N OUTPUTS     FNAME OVVEW SUMRY FROPT GROUT CAOUT WAOUT NIOUT MIOUT DIOUT VBOSE CHOUT OPOUT
 3 OU              N     Y     Y     2     Y     N     Y     Y     N     N     A     N     N

@  AUTOMATIC MANAGEMENT
@N PLANTING    PFRST PLAST PH2OL PH2OU PH2OD PSTMX PSTMN
 3 PL          82237 82277    40   100    30    40    10
@N IRRIGATION  IMDEP ITHRL ITHRU IROFF IMETH IRAMT IREFF
 3 IR             30    50   100 IB001 IBI01    10     1
@N NITROGEN    NMDEP NMTHR NAMNT NCODE NAOFF
 3 NI             30    50    25 IB001 IB001
@N RESIDUES    RIPCN RTIME RIDEP
 3 RE            100     1    20
@N HARVEST     HFRST HLAST HPCNP HPCNR
 3 HA              0 83083   100     0

@N GENERAL     NYERS NREPS START SDATE RSEED SNAME.................... SMODEL
 4 GE              1     1     S 82135  2150 6 months Stress           
@N OPTIONS     WATER NITRO SYMBI PHOSP POTAS DISES  CHEM  TILL   CO2
 4 OP              Y     N     N     N     N     N     N     Y     M
@N METHODS     WTHER INCON LIGHT EVAPO INFIL PHOTO HYDRO NSWIT MESOM MESEV MESOL
 4 ME              M     M     E     R     S     R     R     1     G     S     2
@N MANAGEMENT  PLANT IRRIG FERTI RESID HARVS
 4 MA              R     N     R     R     R
@N OUTPUTS     FNAME OVVEW SUMRY FROPT GROUT CAOUT WAOUT NIOUT MIOUT DIOUT VBOSE CHOUT OPOUT
 4 OU              N     Y     Y     2     Y     N     Y     Y     N     N     A     N     N

@  AUTOMATIC MANAGEMENT
@N PLANTING    PFRST PLAST PH2OL PH2OU PH2OD PSTMX PSTMN
 4 PL          82135 82275    40   100    30    40    10
@N IRRIGATION  IMDEP ITHRL ITHRU IROFF IMETH IRAMT IREFF
 4 IR             30    50   100 IB001 IBI01    10     1
@N NITROGEN    NMDEP NMTHR NAMNT NCODE NAOFF
 4 NI             30    50    25 IB001 IB001
@N RESIDUES    RIPCN RTIME RIDEP
 4 RE            100     1    20
@N HARVEST     HFRST HLAST HPCNP HPCNR
 4 HA              0 83083   100     0



