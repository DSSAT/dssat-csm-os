!=======================================================================
!  MODULE Module_YCA_Formats
!  22/10/2014 MJF Written
!=======================================================================

    MODULE YCA_Formats_m
        !   Contains format strings to replace numbered FORMAT statements
        !   that are used across subroutines in output from the CIATCAS
        !   submodel. Each is the original format statement number preceded
        !   by FMT.
        
        !USE ModuleDefs
        
        IMPLICIT NONE

        SAVE
        
        CHARACTER(LEN=128) :: FMT203 = '(" MODEL            ",A8)'
        CHARACTER(LEN=256) :: FMT206 = '(/, "*MAIN GROWTH AND DEVELOPMENT VARIABLES",//,"@",5X,' // &
            '"VARIABLE",T42,"SIMULATED   MEASURED",/,6X,"--------",T42,"---------   --------")'
        CHARACTER(LEN=128) :: FMT208 = '(" PLANTING         ",A3,I3,I8,2X,I4," plants/m2 in ",I3," cm rows")'
        CHARACTER(LEN=128) :: FMT209 = '(" TEMPERATURES C   Tmax (max):",F5.1," (mnth av):",F5.1," Tmin (min):",' // &
            'F5.1," (mnth av):",F5.1)'
        CHARACTER(LEN=128) :: FMT270 = '(/, 110("-"))'
        CHARACTER(LEN=256) :: FMT290 = '(6X, "Germination  (dap)          ",6X,I7,  4X,I7,  /,6X,' // & 
            '"Emergence    (dap)          ", 6X,I7,  4X,I7    )'
        CHARACTER(LEN=128) :: FMT291 = '(6X,A13, "(dap)          "         ,6X,I7  ,4X,I7    )'
        CHARACTER(LEN=256) :: FMT298 = '("  HWAH  HWUH","  H#AH  H#GH  LAIX  L#SH BR#AH","  CWAH  VWAH  HIAH  RWAH",' // &
            '"  HN%H  TNAH", "  CNAH  HNAH","  HINH PLPOP","  NICH"," SRADA TMAXA TMINA  PRCP")'
        CHARACTER(LEN=256) :: FMT300 = '(/,10X,A," YIELD : ",I8," kg/ha    [Dry weight] ",/)'
        CHARACTER(LEN=512) :: FMT305 = '(6X, "AboveGround (kg dm/ha)      ",6X,I7,  4X,I7,  /,6X,' //&
            '"Roots+seed residue (kg dm/ha)",5X,I7, 4X,I7,/, 6X, "Senesced (kg dm/ha)         ",6X,I7,  4X,I7,  /,' //  &
            '6X, "Product (kg dm/ha)          ",6X, I7, 4X,I7, /,6X, "AboveGroundVegetative (kg dm/ha)  ",' // &
            'I7,4X,I7,/,6X, "HarvestIndex (ratio)        ",6X,F7.2,4X,F7.2,/,6X, "Reserves (kg dm/ha)         ",6X,I7,4X,I7)'
        CHARACTER(LEN=128) :: FMT306 = '(6X,"Removed canopy (kg dm/ha)   ",7X,I6,5X,I6)'
        CHARACTER(LEN=960) :: FMT307 = '(6X,"Product unit wt (g dm)      ",7X,A6,5X,A6,/,6X,"Product number (/m2)        ",' // &
            '6X,I7,  4X,I7,  /,6X, "Product number (/shoot)     ",6X,F7.1,4X,F7.1,/,6X, "Maximum leaf area index     ",' // &
            '6X,F7.1,4X,F7.1,/,6X, "Final leaf number (one axis)",6X,F7.1,4X,F7.1,/,6X, "Assimilated N (kg/ha)       ",' // &
            '6X,F7.1,4X,F7.1,/,6X, "AboveGround N (kg/ha)       ",6X,F7.1,4X,F7.1,/ 6X, "Root N (kg/ha)              ",' // &
            '6X,F7.1,4X,F7.1,/,6X, "Senesced N (kg/ha)          ",6X,F7.1,4X,F7.1,/,6X, "Product N (kg/ha)           ",' // &
            '6X,F7.1,4X,F7.1,/,6X, "AboveGroundVegetative N (kg/ha)  ",F8.1,4X,F7.1,/,6X, "N HarvestIndex (ratio)      ",' // &
            '6X,F7.2,4X,F7.2,/,6X, "Product N (% dm)            ",6X,F7.1,4X,F7.1,/,6X, "AboveGroundVegetative N (% dm)    ",' // &
            'F7.1,4X,F7.1)'  
        CHARACTER(LEN=128) :: FMT400 = '(I6,1X,A10,I4,I3,1X,A25,I4,I6,I2,I2,I2,4X,A2,I6,I6)'
        CHARACTER(LEN=128) :: FMT401 = '(F6.1,I6,I6,I6,I6,I6,I6,F6.1,I6,I6,I6,A6,I6,I6,F6.1,I6,I6,I6,I6,I6,2A6,3I6)'
        CHARACTER(LEN=128) :: FMT409 = '(I6,A6,I6,I6,A6,F6.1,18F6.1,I6,I6,A6,I6,A6,I6,I6,I6,A6,F6.1,I6,3F6.1,I6)' !LPM 28MAR15 To change the format of plantres.OUT
        CHARACTER(LEN=128) :: FMT410 = '(I6,1X,A10,I4,I3,1X,A25,I4,I6,I2,I2,I2,1X,A2,I6)'
        CHARACTER(LEN=128) :: FMT411 = '(I6,A6,I6,I6,A6,F6.1,F6.1,I6,I6,A6,I6,A6,I6,I6,I6,A6,F6.1,I6,3F6.1,I6)'   !LPM 28MAR15 To change the format of plantrem.OUT
        CHARACTER(LEN=128) :: FMT499 = '("   RUN EXCODE    TRNO RN"," TNAME..................",".. REP  RUNI S O C CR  HWAM")'
        CHARACTER(LEN=640) :: FMT500 = '(/, "*ENVIRONMENTAL AND STRESS FACTORS",//,' // &
            '"|-------Branching tier-------|-------------Environment--------","------|----------------Stress---------",' // &
            '"--------|",/,30X,"|--------Average-------|---Cumulative--|         (0=Min, 1=","Max Stress)         |",/,' // &
            '25X,"Time  Temp  Temp Solar Photop         Evapo |----Water---|-","-Nitrogen--|--Phosphorus-|",/,' // &
            '25X,"Span   Max   Min   Rad  [day]   Rain  Trans  Photo",9X,"Pho","to         Photo",/,' // &
            '25X,"days    °C    °C MJ/m2     hr     mm     mm  synth Growth  ","synth Growth  synth Growth",/,110("-"))'
        CHARACTER(LEN=256) :: FMT501 = '(I5,I4,2I6,F6.1,A6,F6.1,F6.3,F6.2,F6.1,A6,F6.3,A6,4I6,3I6,F6.3,I6,2A6,F6.2,' // & ! issue 50
            !'I6,F6.1,2F6.2,F6.1,F6.1, F6.2,2F6.2,2F6.2,F6.1,2F6.2,F6.2)'   !LPM 19MAY2015 to delete PTF as output
            'F6.2,F6.1,1F6.2,F6.1,F6.1, F6.2,2F6.2,2F6.2,F6.1,2F6.2,F6.2)'
        !CHARACTER(LEN=128) :: FMT502 = '(I5,I4,2I6,F6.1,A6,F6.2,A6,F6.2,A6,A6,F6.1,2A6,I6,A6,2F6.2,F6.3,10F6.2)' !LPM 19MAY2015 to delete PTF as output
        CHARACTER(LEN=128) :: FMT502 = '(I5,I4,2I6,F6.1,A6,F6.2,A6,F6.2,A6,A6,F6.1,2A6,2F6.2,F6.3,9F6.2)' ! issue 50
        CHARACTER(LEN=512) :: FMT503 = '(I5,I4,2I6,F6.1,A6,F6.1,F6.1,2F6.2,4F6.1,A6,F6.2,2A6,3F6.3,3F6.3,3F6.3,F6.2,' //&
            'F6.2,F6.2,F6.1,F6.1)'
        !LPM 21jul2021 Add more space for HWAD and FHWAD
        CHARACTER(LEN= 78) :: FMT504 = '(I5,I4,2I6,F6.1,A6,F6.1,2I8,F6.3,3F6.2)'
        CHARACTER(LEN=128) :: FMT507 = '(I5,I4,2I6,2F6.1,A6,F6.1,F6.2,5F6.2,3F6.2,2F6.2,F6.1,F6.2,F6.1,3F6.2)'
        CHARACTER(LEN=128) :: FMT600 = '(1X,A10,A3,A10,I5,3F6.1,F7.2,2F7.1,4F7.3,2F7.2)'
        CHARACTER(LEN=128) :: FMT610 = '(1X,A10,13X,I5,3F6.1,F7.2,2I7,6F7.3)'
        CHARACTER(LEN=512) :: FMT896 = '("@  RUN"," EXCODE     ","  TRNO RN","    CR","    EDAP   EDAPE","    MDAP   MDAPE",' // &
            '"    HWAH   HWAHE","    HWUM   HWUME","    H#AM   H#AME","    H#GM   H#GME","    LAIX   LAIXE", ' // &
            '"    L#SM   L#SME","    CWAM   CWAME","    VWAM   VWAME","    HIAM   HIAME","    HN%M   HN%ME", ' // &
            '"    CNAM   CNAME","    HNAM   HNAME")'
        CHARACTER(LEN=128) :: FMT993 = '(A14,A10,"  ",A25,2X,A8,/)'
        CHARACTER(LEN=128) :: FMT994 = '("@RUN EXCODE      TRNO RN CR EDAPS EDAPM")'
        CHARACTER(LEN=128) :: FMT995 = '(A14,2X,A10,A23,2X,A8/)'
        CHARACTER(LEN=128) :: FMT996 = '(/,"*ERRORS(A)",/)'
        CHARACTER(LEN=960) :: FMT1200 = '(110("-"), ///,"*RESOURCE PRODUCTIVITY",//," Growing season length:", I4,' // &
            '" days ",//," Precipitation during growth season",T42,F7.1," mm[rain]",/,'// &
            '"   Dry Matter Productivity",T42,F7.2," kg[DM]/m3[rain]",T75,"=",F7.1," kg[DM]/ha per mm[rain]",/,' // &
            '"   Yield Productivity",T42,F7.2," kg[Sroot yield]/m3[rain]",T75,"=",F7.1," kg[yield]/ha per mm[rain]",//,' // &
            '" Evapotranspiration during growth season",T42,F7.1," mm[ET]",/,' // &
            '"   Dry Matter Productivity",T42,F7.2," kg[DM]/m3[ET]",T75,"=",F7.1," kg[DM]/ha per mm[ET]",/,' // &
            '"   Yield Productivity",T42,F7.2," kg[Sroot yield]/m3[ET]",T75,"=",F7.1," kg[yield]/ha per mm[ET]",//,' // &
            '" Transpiration during growth season",T42,F7.1," mm[EP]",/,"   Dry Matter Productivity",T42,F7.2,' // &
            '" kg[DM]/m3[EP]",T75,"=",F7.1," kg[DM]/ha per mm[EP]",/,"   Yield Productivity",T42,F7.2,' // &
            '" kg[Sroot yield]/m3[EP]",T75,"=",F7.1," kg[yield]/ha per mm[EP]")'
        CHARACTER(LEN=512) :: FMT1210 = '(/," Irrigation during growing season",T42,F7.1," mm[irrig]",/,' // &
            '"   Dry Matter Productivity",T42,F7.2," kg[DM]/m3[irrig]",T75,"=",F7.1," kg[DM]/ha per mm[irrig]",/,' // &
            '"   Yield Productivity",T42,F7.2," kg[Sroot yield]/m3[irrig]",T75,"=",F7.1," kg[yield]/ha per mm[irrig]")'
        CHARACTER(LEN=512) :: FMT1220 = '(/," N applied during growing season",T42,F7.1," kg[N applied]/ha"/,' // &
            '"   Dry Matter Productivity",T42,F7.1," kg[DM]/kg[N applied]",/,"   Yield Productivity",T42,F7.1,' // &
            '" kg[yield]/kg[N applied]")'
        CHARACTER(LEN=512) :: FMT1230 = '(/," N uptake during growing season",T42,F7.1," kg[N uptake]/ha"/,' // &
            '"   Dry Matter Productivity",T42,F7.1," kg[DM]/kg[N uptake]", /, "   Yield Productivity",T42,F7.1,' // &
            '" kg[yield]/kg[N uptake]")'
        CHARACTER(LEN=128) :: FMT1502 = '(A180)'
        CHARACTER(LEN=256) :: FMT2061 = '(/,"@",5X,"VARIABLE",T42,"SIMULATED   MEASURED",/,6X,"--------",' // &
            'T42,"---------   --------")'
        CHARACTER(LEN=128) :: FMT2089 = '(" POTENTIAL ET cm  Crop model:", F5.1,"  Penman:    ",    F9.1,"  Penman-M:  ", F7.1)'
        CHARACTER(LEN=128) :: FMT2090 = '(" SOIL WATER cm    Initial: ",   F7.1,"  Precipitation: ",F5.1,"  Irrigation: ",F6.1)'
        CHARACTER(LEN=128) :: FMT2091 = '("                  Runoff: ",    F8.1,"  Drainage: "    ,F10.1,"  Final:    ",  F8.1)'
        CHARACTER(LEN=128) :: FMT2093 = '(" SOIL N kg/ha     Initial:",    F8.1,"  Applied:",      F12.1,"  Final:      ",F6.1)'
        CHARACTER(LEN=128) :: FMT2094 = '("                  Denitrified:",F4.1,"  Leached:",      F12.1,"  Net from OM:",F6.1)'
        CHARACTER(LEN=128) :: FMT2095 = '(" CROP N kg/ha     Total:  ",    F8.1,"  Product ",      F12.1,"  Leaf+stem:  ",F6.1)'
        CHARACTER(LEN=128) :: FMT2096 = '("                  Leaf loss: ", F5.1,"  Root loss:  ",   F8.1)'
        CHARACTER(LEN=128) :: FMT2097 = '("                  Priestley: ", F5.1,"  E.budget:  ",    F9.1)'
        CHARACTER(LEN=128) :: FMT2098 = '("                  SOM1 decay:",   I5,"  SOM2 decay:   ",   I6,"  SOM3 decay:",I7)'
        CHARACTER(LEN=128) :: FMT2099 = '("                  OM Fixation:",F4.1,"  Fresh OM decay:",F5.1,"  SOM decay:",F8.1)'

        CHARACTER(LEN=384) :: FMT2201 = '("@YEAR DOY   DAS   DAP TMEAN","  GSTD  L#SD"," PARID PARUD  AWAD",' // &
            '"  LAID  SAID  CAID","  TWAD SDWAD  RWAD  CWAD  LWAD  SWAD  HWAD  HIAD"," RSWAD SNWLD SNWSD",' // &
            !'"  RS%D","  H#AD  HWUD", "  S#AD  SLAD  RDPD  PTFD","  SWXD WAVRD"," WUPRD  WFPD  WFGD","  NFPD  ",' // & !LPM 20MAY2015 to delete PTFD
            '"  RS%D","  S#AD  SLAD  RDPD","  SWXD WAVRD"," WUPRD  WFPD  WFGD","  NFPD  ",' // & ! issue 50
            '"NFGD NUPRD  TFPD  TFGD", " DYLFD","      ","      ")'
        !LPM 21jul2021 Add more space for HWAD and FHWAD
        CHARACTER(LEN=90) :: FMT2202 = '("@YEAR DOY   DAS   DAP TMEAN","  GSTD  L#SD","    HWAD   FHWAD PDMCD  HIAD  TFGD  WFGD")'
        CHARACTER(LEN=256) :: FMT2205 = '("@YEAR DOY   DAS   DAP TMEAN TCDIF  GSTD","    DU DYLFD  TFPD","  WFPD  NFPD ",' // &
            '"CO2FD RSFPD  TFGD  WFGD  NFGD"," WAVRD WUPRD  SWXD  EOPD","  SNXD LN%RD SN%RD RN%RD            ")'
        CHARACTER(LEN=256) :: FMT2215 = '("!........DATES.......  ...TEMP... STAGE "," ...PHENOLOGY.... ",' // & 
            '" ....PHOTOSYNTHESIS.... ", " .....GROWTH..... ","H2O STRESS DETERMINANTS"," .N STRESS DETERMINANTS.")'
        CHARACTER(LEN=256) :: FMT2251 = '("@YEAR DOY   DAS   DAP TMEAN  GSTD  RSTD"," LAIPD LAISD  LAID  CHTD SDWAD ",' // &
            '"SNWLD SNWSD"," SHRTD  RDPD","  RL1D  RL2D  RL3D  RL4D  RL5D  RL6D  RL7D  RL8D",' // & ! issue 50
            '"  RL9D RL10D")'
        !LPM 18apr2021 Change NUAD to NUAC in header as suggested by CHP
        CHARACTER(LEN=256) :: FMT2252 = '("@YEAR DOY   DAS   DAP TMEAN  GSTD  NUAC", "  TNAD SDNAD  RNAD  CNAD  LNAD",' // &
            '"  SNAD  HNAD  HIND RSNAD SNN0D SNN1D","  RN%D  LN%D  SN%D  HN%D SDN%D  VN%D"," LN%RD SN%RD RN%RD  ",' // &
            '"VCN%  VMN% NUPRD NDEMD NFLF2")'
        CHARACTER(LEN=128) :: FMT2996 = '(/,"*ERRORS(T):",A69,/)'
        CHARACTER(LEN=128) :: FMT7104 = '("*RUN ",A5,A10," ",I1,",",I1," C",I1," ",A40,"  ")'
        CHARACTER(LEN=128) :: FMT7105 = '("*RUN ",A5,A10," ",I2,",",I1," C",I1," ",A40," ")'
        CHARACTER(LEN=128) :: FMT7106 = '("*RUN ",A5,A10," ",I3,",",I1," C",I1," ",A40)'
        CHARACTER(LEN=128) :: FMT7107 = '("*RUN ",A5,": ",A10," ",I1," ",A40,"  ")'
        CHARACTER(LEN=128) :: FMT7108 = '("*RUN ",A5,": "A10," ",I2,",",I1," ",A40," ")'
        CHARACTER(LEN=128) :: FMT7109 = '("*RUN ",A5,": "A10," ",I3,",",I1," ",A40)'
        CHARACTER(LEN=128) :: FMT7778 = '(A180)'
        CHARACTER(LEN=128) :: FMT7779 = '(A255)'
        CHARACTER(LEN=128) :: FMT8401 = '(I6,1X,A10,1X,I6,I3,4X,A2,6I8,F8.3,3I8,F8.1,I8,F8.1,I8,F8.1,5I8,F8.2,I8,F8.1,5I8)'
        CHARACTER(LEN=128) :: FMT9588 = '(/," ...... DATE ....... GROWTH STAGE    BIOMASS   LEAF       CROP N      STRESS")'
        CHARACTER(LEN=128) :: FMT9589 = '(//,"*SIMULATED CROP AND SOIL STATUS AT MAIN DEVELOPMENT STAGES")'
        CHARACTER(LEN=128) :: FMT9600 = '(" YEARDOY DOM MON DAP ............... kg/ha  AREA NUMBER kg/ha    %   H2O     N")'
        CHARACTER(LEN=128) :: FMT99511 = '("*RESPONSES(M):",A10,"  ",A8)'

    END MODULE YCA_Formats_m