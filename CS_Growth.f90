!***************************************************************************************************************************
! This is the code from the section (DYNAMIC.EQ.RATE) lines 4218 - 5516 of the original CSCAS code. the names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement. the variables are described in CSCAS.
!
! This subroutine calculates germination timing, daylength and development units, reserves and grazing (?), PAR interception, 
! rate factors, senescence, assimilation and its partitioning, growth of storage roots, leaves, stems and crowns, reserves 
! and plant height, and soil water.
!
!     TO DO: Divide SUBROUTINE GROWTH into several subroutines to account for (at least) senescence, assimilation, and 
!       partitioning.
!
!***************************************************************************************************************************

    SUBROUTINE CS_Growth ( &
        BD          , BRSTAGE     , CO2         , DAYL        , DLAYR       , DUL         , EOP         , ISWDIS      , &
        ISWNIT      , ISWWAT      , KCAN        , LL          , NFP         , NH4LEFT     , NLAYR       , NO3LEFT     , &
        PARIP       , PARIPA      , RLV         , SAT         , SENCALG     , SENLALG     , SENNALG     , SHF         , &
        SLPF        , SRAD        , SW          , TRWUP       , UNH4        , UNO3        , WEATHER       &                 ! MF 15SE14 Removed <TDEW        , TMAX        , TMIN        ,> (in Module_CSCAS_Vars_List)
        )
    
        USE ModuleDefs
        !USE CRSIMDEF                                                                MF 15SE14 Declared in ModuleDefs
        USE Module_CSCAS_Vars_List
    
        IMPLICIT NONE
        
        TYPE (WeatherType) WEATHER
    
        CHARACTER(LEN=1) ISWDIS      , ISWNIT      , ISWWAT      
        INTEGER NLAYR       
        REAL    BD(NL)      , BRSTAGE     , CO2         , DAYL        , DLAYR(NL)   , DUL(NL)     , EOP         , KCAN        
        REAL    LL(NL)      , NFP         , NH4LEFT(NL) , NO3LEFT(NL) , PARIP       , PARIPA      , RLV(NL)     , SAT(NL)     
        REAL    SENCALG(0:NL)             , SENLALG(0:NL)             , SENNALG(0:NL)             , SHF(NL)     , SLPF        
        REAL    SRAD        , SW(NL)      , TRWUP       , UNH4(NL)    , UNO3(NL)                                             ! MF 15SE14 removed <, TDEW        , TMAX        , TMIN > (In Module_CSCas_Vars_List)           

    
        INTEGER CSIDLAYR                                                                     ! Integer function call.
        REAL    CSYVAL      , CSVPSAT     , TFAC4       , YVALXY                             ! Real function calls
      
 
        !-----------------------------------------------------------------------
        !           Determine when in day germination and emergence occurred
        !-----------------------------------------------------------------------

        ! Germination
        IF (GEUCUM.LT.PEGD.AND.GEUCUM+TTGEM*WFGE.LT.PEGD) THEN
            GERMFR = 0.0
        ELSEIF (GEUCUM.LE.PEGD.AND.GEUCUM+TTGEM*WFGE.GE.PEGD) THEN
            GERMFR = 1.0 - (PEGD-GEUCUM)/(TTGEM*WFGE)
        ELSEIF (GEUCUM.GT.PEGD) THEN
            GERMFR = 1.0
        ENDIF

        ! Emergence
        IF (GEUCUM.LT.PEGD+PECM*SDEPTHU.AND.GEUCUM+TTGEM*WFGE.LE.PEGD+PECM*SDEPTHU) THEN
            EMRGFR = 0.0
        ELSEIF (GEUCUM.LE.PEGD+PECM*SDEPTHU.AND.GEUCUM+TTGEM*WFGE.GT.PEGD+PECM*SDEPTHU) THEN
            EMRGFR = 1.0 - (PEGD+PECM*SDEPTHU-GEUCUM)/(TTGEM*WFGE)
        IF (EMFLAG.NE.'Y') THEN
            WRITE(FNUMWRK,*)' ' 
            WRITE(FNUMWRK,'(A18,I8)')' Emergence on day ',yeardoy 
            EMFLAG = 'Y'
        ENDIF
        LNUMSG = 1     ! LAH NEW
        ELSEIF (GEUCUM.GT.PEGD+PECM*SDEPTHU) THEN
            EMRGFR = 1.0
        ENDIF
     
        !-----------------------------------------------------------------------
        !           Calculate daylength factors for development
        !-----------------------------------------------------------------------

        DF = 1.0
        DFNEXT = 1.0
        ! To ensure correct sensitivity on emergence day
        IF (BRSTAGE.LE.0.0) THEN
            BRSTAGETMP = 1.0
        ELSE
            BRSTAGETMP = BRSTAGE
        ENDIF
        IF (PPSEN.EQ.'SL') THEN      ! Short day response,linear 
            DF = 1.0 - DAYLS(INT(BRSTAGETMP))/1000.*(PPTHR-DAYL)
            IF (BRSTAGETMP.LT.FLOAT(MSTG)) THEN
                DFNEXT = 1.-DAYLS(INT(BRSTAGETMP+1))/1000.*(PPTHR-DAYL)
            ELSE
                DFNEXT = DF
            ENDIF 
        ELSEIF (PPSEN.EQ.'LQ') THEN  ! Long day response,quadratic
            DF = AMAX1(0.0,AMIN1(1.0,1.0-(DAYLS(INT(BRSTAGETMP))/10000.*(PPTHR-DAYL)**PPEXP)))
            IF (BRSTAGETMP.LT.10.0) DFNEXT = AMAX1(0.0,AMIN1(1.0,1.0-(DAYLS(INT(BRSTAGETMP+1.0))/10000.*(PPTHR-DAYL)**PPEXP)))
            Tfdf = AMAX1(0.0,1.0-AMAX1(0.0,(TMEAN-10.0)/10.0))
            Tfdf = 1.0  ! LAH No temperature effect on DF ! 
            DF = DF + (1.0-DF)*(1.0-TFDF)
            DFNEXT = DFNEXT + (1.0-DFNEXT)*(1.0-TFDF)
        ENDIF

        ! Set daylength factor for output (Is dfpe before emergence)
        IF (EMRGFR.GE.1.0) THEN
            DFOUT = DF
        ELSE
            DFOUT = DFPE
        ENDIF 

        !-----------------------------------------------------------------------
        !           Calculate development units
        !-----------------------------------------------------------------------

        DU = 0.0
        DUPHASE = 0.0
        DUPNEXT = 0.0
        ! To avoid exceeding the array sizes
        IF (BRSTAGETMP.LT.10.0) THEN
            DUNEED = PSTART(INT(BRSTAGETMP+1.0))-CUMDU
            IF (DUNEED.GE.TT*(DFPE*(GERMFR-EMRGFR)+DF*EMRGFR))THEN
                DUPHASE = TT*(DFPE*(GERMFR-EMRGFR)+DF*EMRGFR)
                TIMENEED = 1.0
                DUPNEXT = 0.0
            ELSE  
                DUPHASE = DUNEED
                TIMENEED = DUNEED/(TT*(DFPE*(GERMFR-EMRGFR)+DF*EMRGFR))
                DUPNEXT = TTNEXT*(1.0-TIMENEED)*DFNEXT
            ENDIF
        ELSE
        ENDIF
            
        DU = DUPHASE+DUPNEXT
            
        !-----------------------------------------------------------------------
        !           Set seed reserve use for root growth and update av.reserves
        !-----------------------------------------------------------------------

        IF (GERMFR.GT.0.0.OR.GESTAGE.GE.0.5) THEN
            SEEDRSAVR = AMIN1(SEEDRS,SEEDRSI/SDDUR*(TT/STDAY)*GERMFR)
        ELSE
            SEEDRSAVR = 0.0
        ENDIF
        ! Seed reserves available
        SEEDRSAV = SEEDRSAV-SEEDRSAVR

        !-----------------------------------------------------------------------
        !           Determine if today has a harvest instruction
        !-----------------------------------------------------------------------

        DO I = 1, 20
            IF (HYEARDOY(I).EQ.YEARDOY) THEN
                HANUM = I
                WRITE(fnumwrk,*) ' '
                WRITE(fnumwrk,'(A20,i2,A12,A1,A6,i8)')' Harvest instruction',hanum,'  Operation ',hop(i),'  Day ',yeardoy
                CALL CSUCASE(HOP(I)) 
                IF (hop(i).EQ.'F') YEARDOYHARF = YEARDOY 
            ENDIF
        END DO

        !-----------------------------------------------------------------------
        !           Determine amounts removed by grazing,etc.   
        !-----------------------------------------------------------------------

        IF (HANUM.GT.0) THEN
            IF (HOP(HANUM).EQ.'G'.AND.CWAD.GT.0.0.AND.CWAD.GT.CWAN(HANUM)) THEN
                HAWAD = AMIN1((CWAD-CWAN(HANUM)),HAMT(HANUM))
                HAWAD = AMAX1(0.0,HAWAD)
                HAFR = AMAX1(0.0,HAWAD/CWAD)
            ELSE   
                HAWAD = 0.0
                HAFR = 0.0
            ENDIF
        ENDIF
              
        IF (HAFR.GT.0.0)WRITE(fnumwrk,'(A23,3F6.1)')' HARVEST  FR,CWAN,CWAD ',HAFR,CWAN(HANUM),CWAD

        ! For grazing 
        lwph = lfwt * hafr
        laph = lapd * hafr
        swph = stwt * hafr
        rswph = rswt * hafr
        lnph = leafn * hafr
        snph = stemn * hafr
        rsnph = rsn * hafr

        !-----------------------------------------------------------------------
        !           Check for or calculate PAR interception at start of day
        !-----------------------------------------------------------------------

        PARI = 0.0
        PARI1 = (1.0 - EXP((-KCAN)*LAI))
        IF (PARIP.GT.0.0) THEN
            ! From competition model
            IF (ISWDIS(LENDIS:LENDIS).NE.'N') THEN
                PARI = PARIPA/100.0
            ELSE
                PARI = PARIP/100.0
            ENDIF
        ELSE
            PARI = PARI1! LAH For row crops may need to change 
            ! In original Ceres maize, kcan is calculated as:
            ! 1.5 - 0.768*((rowspc*0.01)**2*pltpop)**0.1
            ! eg. 1.5 - 0.768*((75*0.01)**2*6.0)**0.1  =  0.63
        ENDIF

        !-----------------------------------------------------------------------
        !           Calculate adjustment to yesterday's C assimilation
        !-----------------------------------------------------------------------

        ! End of day interception = today's starting interception
        IF (MEPHO.EQ.'M') CARBOEND = CARBOTMPM * PARI/PLTPOP
        IF (MEPHO.EQ.'I') CARBOEND = CARBOTMPI * PARI/PLTPOP
        IF (MEPHO.EQ.'R') CARBOEND = CARBOTMPR * PARI/PLTPOP

        CARBOADJ = (CARBOEND-CARBOBEG)/2.0*EMRGFRPREV
        ! But note, no adjustment if leaf kill
        PARMJIADJ = PARMJFAC*SRADPREV*(PARI-PARIPREV)/2.0*EMRGFR

        !-----------------------------------------------------------------------
        !           Calculate process rate factors
        !-----------------------------------------------------------------------

        ! Water
        ! No water stress after emergence on day that emerges
        WFG = 1.0
        WFP = 1.0
        IF (ISWWAT.NE.'N') THEN
            IF (EOP.GT.0.0) THEN
                WUPR = TRWUP/(EOP*0.1)
                IF (WFGU-WFGL.GT.0.0) WFG = AMAX1(0.0,AMIN1(1.0,(WUPR-WFGL)/(WFGU-WFGL)))
                IF (WFPU-WFPL.GT.0.0) WFP = AMAX1(0.0,AMIN1(1.0,(WUPR-WFPL)/(WFPU-WFPL)))
            ENDIF
            IF (ISWWATEARLY.EQ.'N') THEN
                WFG = 1.0
                WFP = 1.0
            ENDIF
        ENDIF

        ! Nitrogen
        ! WARNING No N stress after emergence on day that emerges
        IF (ISWNIT.NE.'N') THEN
            IF (LFWT.GT.1.0E-5) THEN
                !NFG =AMIN1(1.0,AMAX1(0.0,(LANC-LNCGL)/(LNCGU-LNCGL)))
                LNCGL = LNCM + NFGL * (LNCX-LNCM)
                LNCGU = LNCM + NFGU * (LNCX-LNCM)
                IF (LNCGU - LNCGL > 1.E-6) THEN
                    NFG =AMIN1(1.0,AMAX1(0.0,(LANC-LNCGL)/(LNCGU-LNCGL)))
                ELSE
                    NFG = 1.0 
                ENDIF
                LNCPL = LNCM + NFPL * (LNCX-LNCM)
                LNCPU = LNCM + NFPU * (LNCX-LNCM)
                IF (LNCPU - LNCPL > 1.E-6) THEN
                    NFP =AMIN1(1.0,AMAX1(0.0,(LANC-LNCPL)/(LNCPU-LNCPL)))
                ELSE
                    NFP = 1.0 
                ENDIF
            ELSE
                NFG = 1.0
                NFP = 1.0  
            ENDIF
        ELSE  
            NFG = 1.0
            NFP = 1.0  
        ENDIF

        ! If N stress switched off early in cycle. 
        IF (ISWNITEARLY.EQ.'N') THEN
            NFG = 1.0
            NFP = 1.0  
        ENDIF

        ! Reserves
        IF (RSFPU.GT.0.0.AND.RSFPU.GT.0.0) THEN
            RSFP = 1.-AMIN1(1.,AMAX1(0.,(RSCD-RSFPL)/(RSFPU-RSFPL)))
        ELSE
            RSFP = 1.0
        ENDIF

        ! Temperature
        ! LAH No cold night effect.
        ! Maybe,one cold night --> reduced phs next day!!
        ! May want to introduce:
        ! IF (TMEAN20.LT.0.0) TFG = 0.0
        ! IF (TMEAN20.LT.0.0) TFP = 0.0
        Tfp = TFAC4(trphs,tmean,TTOUT)
        Tfg = TFAC4(trlfg,tmean,TTOUT)
        IF (CFLTFG.EQ.'N') TFG = 1.0

        !! Vapour pressure                                                           ! Code for VPD reponse moved to CS_Photo for hourly response.
        !VPDFP = 1.0
        !IF (PHTV.GT.0.0) THEN
        !    IF (TDEW.LE.-98.0) TDEW = TMIN
        !    VPD = CSVPSAT(tmax) - CSVPSAT(TDEW)    ! Pa 
        !    IF (VPD/1000.0.GT.PHTV) VPDFP = AMAX1(0.0,1.0+PHSV*(VPD/1000.0-PHTV))
        !ENDIF


        ! CO2 factor using look-up table
        CO2FP = YVALXY(CO2RF,CO2F,CO2)
        ! Co2 factor using CROPGRO formula
        ! CO2EX Exponent for CO2-PHS relationship (0.05)  
        ! COCCC CO2 compensation concentration (80 vpm)
        ! CO2FP = 
        !   PARFC*((1.-EXP(-CO2EX*CO2))-(1.-EXP(-CO2EX*CO2COMPC)))

        !  LAH Notes from original cassava model                                                          
        !  IF (TEMPM .LT. SenCritTemp) THEN
        !     Life(I) = Life(I)-SenTempFac*(SenCritTemp-TEMPM)
        !  ENDIF
        !  IF (CumLAI .GT. SenCritLai) THEN
        !     Life(I) = Life(I)-SenLaiFac*(CumLAI-SenCritLai)
        !  ENDIF
        !  IF (Life(I) .LT. 0.0) Life(I) = 0.0
        !  LSCL   0.4  Leaf senescence,critical LAI
        !  LSCT  18.0  Leaf senescence,critical temperature (C)
        !  LSSL  0.05  Leaf senescence,sensitivity to LAI
        !  LSST   0.3  Leaf senescence,sensitivity to temp (fr/d)

        !-----------------------------------------------------------------------
        !           Calculate leaf number at end of day;adjust PHINT if needed
        !-----------------------------------------------------------------------
                                               
        LAGEG = 0.0
        LNUMG = 0.0
        ! Reduce PHINT with development stage (LAH Sept 2012)
        PHINT = 1.0/((1.0/PHINTS)*(1.0-PHINTFAC*DSTAGE))
        !LNUMEND = LNUM + (TT*EMRGFR)/PHINT !LPM 17MY14  FLN and LNUMEND are not used, deleted 
        !
        !! Restrict to maximum
        !LNUMEND = AMIN1(FLOAT(LNUMX),LNUMEND)
        !IF(FLN.GT.0.0) LNUMEND = AMIN1(FLN,LNUMEND)
        !!LNUMG = LNUMEND - LNUM
        LNUMG = (TT*EMRGFR)/PHINT

        !-----------------------------------------------------------------------
        !           Calculate senescence of leaves,stems,etc..
        !-----------------------------------------------------------------------

        ! LAH Notes from original cassava model. May need to take
        ! into account procedure to calculate leaf senescence. 
        ! Leaves are assumed to have a variety-specific maximum 
        ! life, which can be influenced by temperature and shading
        ! by leaves above. Water stress is assumed not to have any
        ! effect on leaf life (Cock, pers. comm.). However, on 
        ! release of stress leaves drop off and are replaced by a 
        ! flush of new leaves. This is not yet built into the 
        ! model.

        PLASP = 0.0
        PLASI = 0.0
        PLASL = 0.0
        PLASS = 0.0

        ! Leaf senescence - phyllochron or real time driven
        LAPSTMP = 0.0
        DO L = 1,LNUMSG
            IF (LAGETT(L)+TTLFLIFE*EMRGFR.LE.LLIFATT+LLIFGTT) EXIT
            IF (LAP(L)-LAPS(L).GT.0.0) THEN
                LAPSTMP = AMIN1((LAP(L)-LAPS(L)),LAP(L)/LLIFSTT*AMIN1((LAGETT(L)+(TTLFLIFE*EMRGFR)-(LLIFGTT+LLIFATT)), &
                    (TTLFLIFE*EMRGFR)))
                LAPS(L) = LAPS(L) + LAPSTMP
                PLASP = PLASP + LAPSTMP
            ENDIF
        ENDDO

        ! Leaf senescence - injury        ! LAH  To add later?
        !PLASI = PLA*(LSENI/100.0)*DU/STDAY  ! May need injury loss

        ! Leaf senescence - water or N stress
        ! LAH Need to accelerated senescence rather than lose leaf
        PLASW = 0.0
        PLASN = 0.0
        IF (ISWWAT.NE.'N') THEN
            IF (PLA-SENLA.GT.0.0.AND.WUPR.LT.WFSU) PLASW = AMAX1(0.0,AMIN1((PLA-SENLA)-PLAS,(PLA-SENLA)*LLOSA))
        ENDIF
        IF (ISWNIT.NE.'N') THEN
            LNCSEN = LNCM + NFSU * (LNCX-LNCM)
            IF (PLA-SENLA.GT.0.0.AND.LANC.LT.LNCSEN) PLASN = AMAX1(0.0,AMIN1((PLA-SENLA)-PLAS,(PLA-SENLA)*LLOSA))
        ENDIF
        ! LAH TMP
        PLASW = 0.0
        PLASN = 0.0
        PLASS = PLASW + PLASN    ! Loss because of stress
              
        ! Leaf senescence - low light at base of canopy
        ! NB. Just senesces any leaf below critical light fr 
        PLASL = 0.0
        IF (LAI.GT.LAIXX) THEN
            PLASL = (LAI-LAIXX) / (PLTPOP*0.0001)
            ! LAH Eliminated! Replaced by accelerated senescence
            PLASL = 0.0
        ENDIF
            
        ! Leaf senescence - overall
        PLAS =  PLASP + PLASI + PLASS + PLASL
        ! Overall check to restrict senescence to what available
        PLAS = AMAX1(0.0,AMIN1(PLAS,PLA-SENLA))

        !-----------------------------------------------------------------------
        !           Calculate C and N made available through senescence
        !-----------------------------------------------------------------------

        SENLFG = 0.0
        SENLFGRS = 0.0
        SENNLFG = 0.0
        SENNLFGRS = 0.0
        IF (PLA-SENLA.GT.0.0) THEN
        ! LAH New algorithms 03/04/13
        SENLFG = AMIN1(LFWT*LWLOS,(AMAX1(0.0,(LFWT*(PLAS/(PLA-SENLA))*LWLOS))))
        SENLFGRS = AMIN1(LFWT*(1.0-LWLOS),(AMAX1(0.0,(LFWT*(PLAS/(PLA-SENLA))*(1.0-LWLOS)))))
        ENDIF
  
        IF (ISWNIT.NE.'N') THEN
            ! NB. N loss has a big effect if low N
            ! Assumes that all reserve N in leaves
            IF (LFWT.GT.0.0) LANCRS = (LEAFN+RSN) / LFWT
            SENNLFG = AMIN1(LEAFN,(SENLFG+SENLFGRS)*LNCM)
            SENNLFGRS = AMIN1(LEAFN-SENNLFG,(SENLFG+SENLFGRS)*(LANC-LNCM))
        ELSE
            SENNLFG = 0.0
            SENNLFGRS = 0.0
        ENDIF

        !-----------------------------------------------------------------------
        !           Calculate overall senescence loss from tops
        !-----------------------------------------------------------------------

        SENFR = 1.0
        SENTOPLITTERG = 0.0
        SENTOPLITTERG = SENLFG*SENFR

        !-----------------------------------------------------------------------
        !           Calculate C assimilation at beginning of day
        !-----------------------------------------------------------------------

        CALL CS_Photo ( &
            CO2         , NFP         , SLPF        , SRAD        , WEATHER     &                      ! MF 15SE14 Removed <, TAIRHR      , TDEW        , TMAX        , TMIN> (In Module_CSCAS_Vars_list)
            )


        !-----------------------------------------------------------------------
        !           Partitioning of C to above ground and roots (minimum) 
        !-----------------------------------------------------------------------

        PTF = PTFMN+(PTFMX-PTFMN)*DSTAGE     
        ! Partition adjustment for stress effects
        PTF = AMIN1(PTFMX,PTF-PTFA*(1.0-AMIN1(WFG,NFG)))
        CARBOR = AMAX1(0.0,(CARBOBEG+CARBOADJ))*(1.0-PTF)
        CARBOT = AMAX1(0.0,(CARBOBEG+CARBOADJ)) - CARBOR

        ! Stem fraction or ratio to leaf whilst leaf still growing
        ! (If a constant STFR is entered,swfrx is set=stfr earlier)
        ! Increases linearly between specified limits
        SWFR = CSYVAL (LNUM,SWFRNL,SWFRN,SWFRXL,SWFRX)

        ! Crown fraction 
        GROCRFR = 0.0
        ! Increases linearly from start to end of growth cycle
        GROCRFR = CRFR * DSTAGE

        !-----------------------------------------------------------------------
        !           Storage root basic growth and number determination
        !-----------------------------------------------------------------------

        GROSR = 0.0
        IF(CUMDU+DU.LT.DUSRI)THEN
            SRDAYFR = 0.0
        ELSEIF(CUMDU.LT.DUSRI.AND.CUMDU+DU.GE.DUSRI)THEN
            SRDAYFR = (DUSRI-CUMDU)/DU
        ELSEIF(CUMDU.GT.DUSRI)THEN
            SRDAYFR = 1.0
        ENDIF
        GROSR = SRFR*CARBOT*SRDAYFR
            
        IF(CUMDU.GE.DUSRI.AND.SRNOPD.LE.0.0) THEN
            SRNOPD = INT(SRNOW*((LFWT+STWT+CRWT+RSWT)))
        ENDIF
                     
        !-----------------------------------------------------------------------
        !           Specific leaf area
        !-----------------------------------------------------------------------

        IF (LAWTR.GT.0.0.AND.LAWTS.GT.0.0.AND.LAWTS.GT.TMEAN) THEN
            TFLAW = 1.0+LAWTR*(TMEAN-LAWTS)
        ELSE
            TFLAW = 1.0
        ENDIF
        IF (LAWWR.GT.0.0.AND.WFG.LT.1.0) THEN
            WFLAW = 1.0+LAWWR*(WFG-1.0)
        ELSE
            WFLAW = 1.0
        ENDIF

        ! Position effect on standard SLA
        IF (LNUMSG.GT.0) THEN
            LAWL(1) = AMAX1(LAWS*LAWFF,LAWS+(LAWS*LAWCF)*(LNUMSG-1))
            ! Temperature and water stress effects on SLA at position
            LAWL(1) = AMAX1(LAWL(1)*LAWMNFR,LAWL(1)*TFLAW*WFLAW)
        ELSE  
            LAWL(1) = LAWS
        ENDIF 

        !-----------------------------------------------------------------------
        !           Leaf growth
        !-----------------------------------------------------------------------

        GROLF = 0.0
        GROLFADJ = 0.0
        GROLFP = 0.0
        GROLSRS = 0.0
        GROLS = 0.0
        GROLSA = 0.0
        GROLSP = 0.0
        GROLSRT = 0.0
        GROLSSEN = 0.0
        GROLSRTN = 0.0
        GROLSSD = 0.0
        LAGEG = 0.0
        PLAGS2 = 0.0
        SHLAG2 = 0.0
        SHLAGB2 = 0.0
        SHLAGB3 = 0.0
        SHLAGB4 = 0.0
            
        ! BRANCH NUMBER            
        ! Old method (1 fork number throughout)
        ! BRNUMST = AMAX1(1.0,BRNUMFX**(INT(brstage)-1))
        ! New method (fork number specified for each forking point)
        ! First calculate new BRSTAGE as temporary variable
        ! (LAH Check whether can move brstage calc up here! 
        ! (If do this, brstage in brfx below must be reduced by 1))
        IF (PDL(INT(BRSTAGE)).GT.0.0) THEN
            TVR1 = FLOAT(INT(BRSTAGE)) + (LNUM-LNUMTOSTG(INT(BRSTAGE)))/PDL(INT(BRSTAGE))
        ELSE
            TVR1 = FLOAT(INT(BRSTAGE))
        ENDIF
        IF (INT(TVR1).GT.INT(BRSTAGEPREV)) THEN
            IF (BRSTAGE.EQ.0.0) THEN
                BRNUMST = 1 
            ELSEIF (BRSTAGE.GT.0.0) THEN
                BRNUMST = BRNUMST*BRFX(INT(BRSTAGE))
            ENDIF
        ENDIF

        ! Potential leaf size for next growing leaf - main shoot 
        LNUMNEED = FLOAT(INT(LNUM+1)) - LNUM
        IF (ABS(LNUMNEED).LE.1.0E-6) LNUMNEED = 0.0
        IF (LNUMSG+1.LE.INT(LAXNO)) THEN
            LAPOTX(LNUMSG+1) = AMIN1(LAXS, LA1S + LNUMSG*((LAXS-LA1S)/(LAXNO-1)))
        ELSEIF (LNUMSG+1.GT.INT(LAXNO).AND.LNUMSG+1.LE.INT(LAXN2)) THEN
            LAPOTX(LNUMSG+1) = LAXS
        ELSE
            LAPOTX(LNUMSG+1) = AMAX1(LAFS, LAXS - ((LNUMSG+1)-LAXN2)*((LAXS-LAFS)/(LAFND-LAXN2)))
        ENDIF
            
        ! LAH Sept 2012 Eliminate fork # effect on leaf size 
        ! Adjust for fork#/shoot
        !IF (BRNUMST.GE.1)LAPOTX(LNUMSG+1)=LAPOTX(LNUMSG+1)/BRNUMST
        ! Keep track of 'forking';reduce potential>forking
        !IF (LNUMSG.GT.1.AND.BRNUMPT.GT.BRNUMSTPREV) THEN
        !  LAPOTX(LNUMSG+1) = LAPOTX(LNUMSG+1)/LAFF
        !  LNUMFORK = LNUMSG
        !ENDIF 
            
        ! Leaf area increase:growing leaves on 1 axis,main shoot
        SHLAG2(1) = 0.0
        DO L = MAX(1,LNUMSG-1-INT((LLIFG/PHINTS))),LNUMSG+1
            ! Basic leaf growth calculated on thermal time base. 
            ! Basic response (cm2/d) same as for development. 
            TTNEED = AMAX1(0.0,LLIFG-LAGETT(L))
            LATLPREV(1,L) = LATL(1,L)
            LATLPOT(1,L)=LAPOTX(L)*((LAGETT(L)+TTLFLIFE*EMRGFR)/LLIFG)
            IF (LATLPOT(1,L).LT.0.0) LATLPOT(1,L) = 0.0
            IF (LATLPOT(1,L).GT.LAPOTX(L)) LATLPOT(1,L) = LAPOTX(L)
            LATL(1,l) = LATL(1,L) + (LATLPOT(1,L)-LATLPREV(1,L))
            LATL2(1,l) = LATL2(1,L) + (LATLPOT(1,L)-LATLPREV(1,L))* AMIN1(WFG,NFG)*TFG
            SHLAG2(1) = SHLAG2(1) + (LATLPOT(1,L)-LATLPREV(1,L))* AMIN1(WFG,NFG)*TFG
            ! The 2 at the end of the names indicates that 2 groups 
            ! of stresses have been taken into account
            ! Stress factors for individual leaves
            WFLF(L) = AMIN1(1.0,WFLF(L)+WFG*(LATLPOT(1,L)-LATLPREV(1,L))/LAPOTX(L))
            NFLF(L) = AMIN1(1.0,NFLF(L)+NFG*(LATLPOT(1,L)-LATLPREV(1,L))/LAPOTX(L))
            NFLFP(L) = AMIN1(1.0,NFLFP(L)+NFP*(LATLPOT(1,L)-LATLPREV(1,L))/LAPOTX(L))
            TFGLF(L) = AMIN1(1.0,TFGLF(L)+TFG*(LATLPOT(1,L)-LATLPREV(1,L))/LAPOTX(L))
            TFDLF(L) = AMIN1(1.0,TFDLF(L)+TFD*(LATLPOT(1,L)-LATLPREV(1,L))/LAPOTX(L))
            ! New LEAF
            IF (L.EQ.LNUMSG.AND.LNUMG.GT.LNUMNEED) THEN
                LAGL(1,L+1) = LAPOTX(L+1) * (TTLFLIFE*EMRGFR) * (((LNUMG-LNUMNEED)/LNUMG)/LLIFG) 
                LATL(1,L+1) = LATL(1,L+1) + LAGL(1,L+1)
                LATL2(1,L+1) = LATL2(1,L+1) + LAGL(1,L+1) * AMIN1(WFG,NFG)*TFG
                SHLAG2(1) = SHLAG2(1) + LAGL(1,L+1) * AMIN1(WFG,NFG)*TFG
                LBIRTHDAP(L+1) = DAP  
                ! Stress factors for individual leaves
                WFLF(L+1) = AMIN1(1.0,WFLF(L+1)+WFG*LATL(1,L+1)/LAPOTX(L+1))
                NFLF(L+1) = AMIN1(1.0,NFLF(L+1)+NFG*LATL(1,L+1)/LAPOTX(L+1))
                NFLFP(L+1) = AMIN1(1.0,NFLFP(L+1)+NFP*LATL(1,L+1)/LAPOTX(L+1))
                TFGLF(L+1) = AMIN1(1.0,TFGLF(L+1)+TFG*LATL(1,L+1)/LAPOTX(L+1))
                TFDLF(L+1) = AMIN1(1.0,TFDLF(L+1)+TFD*LATL(1,L+1)/LAPOTX(L+1))
            ENDIF
        ENDDO
 
        ! Leaf area increase:growing leaves on 1 axis,all shoots
        PLAGS2 = SHLAG2(1) ! To initialize before adding over shoots
        DO L = 2,INT(SHNUM+2) ! L is shoot cohort,main=cohort 1
            IF (SHNUM-FLOAT(L-1).GT.0.0) THEN
                PLAGS2 = PLAGS2+SHLAG2(1)*SHGR(L) * AMAX1(0.,AMIN1(FLOAT(L),SHNUM)-FLOAT(L-1))
                SHLAG2(L) = SHLAG2(1)*SHGR(L) * AMAX1(0.,AMIN1(FLOAT(L),SHNUM)-FLOAT(L-1))
            ENDIF
        ENDDO

        ! Leaf area increase:growing leaves on all axes,all shoots
        PLAGSB2 = PLAGS2*BRNUMST
        SHLAGB2(1) = SHLAG2(1)*BRNUMST
        DO L = 2,INT(SHNUM+2) ! L is shoot cohort,main= 1
            SHLAGB2(L) =  SHLAG2(L)*BRNUMST
        ENDDO
            
        ! Potential leaf weight increase.
        IF (LAWL(1).GT.0.0) GROLFP = (PLAGSB2/LAWL(1)) / (1.0-LPEFR)    

        ! Potential leaf+stem weight increase.
        IF (SWFR.GT.0.0.AND.SWFR.LT.1.0) THEN
            GROLSP = GROLFP * (1.0 + SWFR/(1.0-SWFR))
        ELSE
            GROLSP = GROLFP
        ENDIF

        IF (GROLSP.GT.0.0) THEN
            ! Leaf+stem weight increase from assimilates
            GROLSA = AMAX1(0.,AMIN1(GROLSP,CARBOT-GROSR))

            ! Leaf+stem weight increase from senescence 
            IF (GROLSA.LT.GROLSP) THEN

                GROLSSEN = AMIN1(GROLSP-GROLSA,SENLFGRS)
            ENDIF
            
            IF (GROLSA+GROLSSEN.LT.GROLSP) THEN
                ! Leaf+stem weight increase from seed reserves
                ! LAH May need to restrict seed use.To use by roots?
                GROLSSD = AMIN1((GROLSP-GROLSA-GROLSSEN),SEEDRSAV)
                SEEDRSAV = SEEDRSAV - GROLSSD
                IF ( LAI.LE.0.0.AND.GROLSSD.LE.0.0.AND.SEEDRSAV.LE.0.0.AND.ESTABLISHED.NE.'Y') THEN
                    CFLFAIL = 'Y'
                    WRITE (Message(1),'(A41)') 'No seed reserves to initiate leaf growth '
                    WRITE (Message(2),'(A33,F8.3,F6.1)') '  Initial seed reserves,seedrate ',seedrsi,sdrate
                    WRITE (Message(3),'(A33,F8.3,F6.1)') '  Reserves %,plant population    ',sdrsf,pltpop 
                    CALL WARNING(3,'CSCAS',MESSAGE)
                ENDIF
            ENDIF
            ! Leaf+stem weight increase from plant reserves
            IF (GROLSA+GROLSSD+GROLSSEN.LT.GROLSP) THEN
                GROLSRS =  AMIN1(RSWT*RSUSE,GROLSP-GROLSA-GROLSSD-GROLSSEN)
            ENDIF
            ! Leaf+stem weight increase from roots (after drought)
            GROLSRT = 0.0
            GROLSRTN = 0.0
            IF ((GROLSA+GROLSSD+GROLSRS+GROLSSEN).LT.GROLSP.AND.SHRTD.LT.1.0.AND.RTUFR.GT.0.0.AND.ESTABLISHED.EQ.'Y') THEN
                GROLSRT = AMIN1(RTWT*RTUFR,(GROLSP-GROLSA-GROLSSD-GROLSSEN-GROLSRS))
                IF (ISWNIT.NE.'N') THEN
                    GROLSRTN = GROLSRT * RANC
                ELSE
                    GROLSRTN = 0.0
                ENDIF
                WRITE(Message(1),'(A16,A12,F3.1,A8,F7.4,A7,F7.4,A9,F7.4)') &
                    'Roots -> leaves ',' Shoot/root ',shrtd,' Grolsp ',grolsp,' Grols ',grols,' Grolsrt ',grolsrt
                CALL WARNING(1,'CSCAS',MESSAGE)
            ENDIF
            ! Leaf+stem weight increase from all sources
            GROLS = GROLSA + GROLSSEN + GROLSSD + GROLSRS+GROLSRT
            ! Leaf weight increase from all sources
            IF ((GROLSP).GT.0.0) THEN
                GROLF = GROLS * GROLFP/GROLSP
            ELSE  
                GROLF = 0.0
            ENDIF
            ! Check if enough assimilates to maintain SLA within limits
            AREAPOSSIBLE = GROLF*(1.0-LPEFR)*(LAWL(1)*(1.0+LAWFF))
    
            ! If not enough assim.set assimilate factor
            IF (PLAGSB2.GT.AREAPOSSIBLE.AND.PLAGSB2.GT.0.0)THEN
                AFLF(0) = AREAPOSSIBLE/PLAGSB2
            ELSE  
                AFLF(0) = 1.0
            ENDIF
            IF (CFLAFLF.EQ.'N') AFLF(0) = 1.0
            ! Area and assimilate factors for each leaf
            DO L = MAX(1,LNUMSG-1-INT((LLIFG/PHINTS))),LNUMSG+1 
                IF (LNUMSG.LT.LNUMX) THEN
                    LATL3(1,L)= LATL2(1,L) * AFLF(0)
                    AFLF(L) = AMIN1(1.0,AFLF(L) + AMAX1(0.0,AFLF(0)) * (LATLPOT(1,L)-LATLPREV(1,L))/LAPOTX(L))
                    IF (CFLAFLF.EQ.'N') AFLF(L) = 1.0
                ENDIF  
            ENDDO
            PLAGSB3 = PLAGSB2 * AFLF(0)
            SHLAGB3(1) = SHLAGB2(1) * AFLF(0)
            SHLAGB3(2) = SHLAGB2(2) * AFLF(0)
            SHLAGB3(3) = SHLAGB2(3) * AFLF(0)
    
        ENDIF
            
        !-----------------------------------------------------------------------
        !           Stem and crown growth                                     
        !-----------------------------------------------------------------------

        GROCR = 0.0
        GROSTCRP = 0.0
        GROST = 0.0
        GROSTCR = 0.0
        STAIG = 0.0
        STAIS = 0.0
        ! Potential stem weight increase.
        IF (SWFR.LT.1.0) THEN
            GROSTCRP = GROLFP * SWFR/(1.0-SWFR)
            GROSTCRPSTORE = AMAX1(GROLFP,GROSTCRPSTORE)
        ELSE  
            GROSTCRP = GROSTCRPSTORE
            ! LAH May need to change GROSTCRP as progress
        ENDIF
            
        IF (GROLFP+GROSTCRP.GT.0.0) GROSTCR = GROLS * GROSTCRP/(GROLFP+GROSTCRP) * (1.0-RSFRS)
        ! LAH RSFRS is the fraction of stem growth to reserves
        ! May need to have this change as stem growth proceeds
     
        ! Crown (Planting stick) .. in balance with stem
        GROCR = GROSTCR * GROCRFR
        GROST = GROSTCR * (1.0-GROCRFR)
                          
        !-----------------------------------------------------------------------
        !           Root growth                                     
        !-----------------------------------------------------------------------

        RTWTG = (CARBOR+SEEDRSAVR)*(1.0-RRESP) 
        RTRESP = RTWTG*RRESP/(1.0-RRESP)   

        !-----------------------------------------------------------------------
        !           N Uptake  -  and growth adjustments if inadequate N
        !-----------------------------------------------------------------------

        ! Set adjusted values to unadjusted 
        ! For when no adjustment necessary, or when not simulating N
        GROCRADJ = GROCR
        GROLFADJ = GROLF
        GROSTADJ = GROST
        PLAGSB4 = PLAGSB3
        RSSRWTGLFADJ = 0.0
        RTRESPADJ = RTRESP   
        RTWTGADJ = RTWTG
        SHLAGB4 = SHLAGB3

        IF (ISWNIT.NE.'N') THEN
    
            ANDEM = 0.0
            RNDEM = 0.0
            LNDEM = 0.0
            SNDEM = 0.0
            SRNDEM = 0.0
            RNDEMG = 0.0
            LNDEMG = 0.0
            SNDEMG = 0.0
            SRNDEMG = 0.0
            RNDEMTU = 0.0
            LNDEMTU = 0.0
            SNDEMTU = 0.0
            SRNDEMTU = 0.0
            SEEDNUSE = 0.0
            SEEDNUSE2 = 0.0
            RSNUSED = 0.0
    
            SNO3PROFILE = 0.0
            SNH4PROFILE = 0.0
            SNO3ROOTZONE = 0.0
            SNH4ROOTZONE = 0.0
            TRLV = 0.0
            DO L = 1, NLAYR
                TRLV = TRLV + RLV(L)
                FAC(L) = 10.0/(BD(L)*DLAYR(L))
                SNO3(L) = NO3LEFT(L) / FAC(L)
                SNH4(L) = NH4LEFT(L) / FAC(L)
                SNO3PROFILE = SNO3PROFILE + SNO3(L)
                SNH4PROFILE = SNH4PROFILE + SNH4(L)
                IF (RLV(L).GT.0.0) THEN
                    SNO3ROOTZONE = SNO3ROOTZONE + SNO3(L)
                    SNH4ROOTZONE = SNH4ROOTZONE + SNH4(L)
                ENDIF
            END DO
    
            LNDEM = GROLF*LNCX + (LFWT-SENLFG-SENLFGRS)*AMAX1(0.0,NTUPF*(LNCX-LANC)) - GROLSRTN
            SNDEM = AMAX1(0.0,GROST+GROCR)*SNCX + (STWT+CRWT)*AMAX1(0.0,NTUPF*(SNCX-SANC))
            RNDEM = RTWTG*RNCX + (RTWT-SENRTG-GROLSRT)*AMAX1(0.0,NTUPF*(RNCX-RANC))
            SRNDEM = (GROSR+SRWTGRS)*(SRNPCS/100.0) + SRWT*AMAX1(0.0,NTUPF*((SRNPCS/100.0)-SRANC)) 
    
    
            ! Seed use if no roots
            ! N use same % of initial as for CH20,if needed.
            IF (RTWT.LE.0.0) THEN
                SEEDNUSE = AMAX1(0.0, AMIN1(SEEDN,LNDEM+SNDEM+RNDEM,SEEDNI/SDDUR*(TT/STDAY)))
            ELSE
                ! Some use of seed (0.5 need) even if may not be needed
                SEEDNUSE = AMAX1(0.0,AMIN1(SEEDN, 0.5*(LNDEM+SNDEM+RNDEM),SEEDNI/SDDUR*(TT/STDAY)))
            ENDIF
    
            ! Reserves used before uptake
            RSNUSED = AMIN1(LNDEM+SNDEM+RNDEM+SRNDEM-SEEDNUSE,RSN)
    
            ! N uptake needed 
            ANDEM = AMAX1(0.0,PLTPOP*10.0* (LNDEM+SNDEM+RNDEM+SRNDEM-SEEDNUSE-RSNUSED))
            ! Uptake
    
            ! Original from CSM with some 'modification'.  
            ! RNUMX = RTNO3,RTNH4 = N uptake/root length (mgN/cm,.006)
            ! RNO3U,RNH4U  = Nitrogen uptake (kg N/ha)
            ! RNUMX = 0.006    
            WFNU = 1.0
            NUPAP = 0.0
            RNO3U = 0.0
            RNH4U = 0.0
            TVR1 = -0.08  ! Could be AMAX1(0.01,(0.20-NconcU*0.004))
            DO L=1,NLAYR
                IF (RLV(L) .GT. 0.0) THEN
                    NLAYRROOT = L
                    ! N concentration effects
                    FNH4 = 1.0-EXP(TVR1*NH4CF * NH4LEFT(L))
                    FNO3 = 1.0-EXP(TVR1*NO3CF * NO3LEFT(L))
                    IF (NO3LEFT(L) .LE. NO3MN) FNO3 = 0.0  
                    IF (FNO3 .GT. 1.0)  FNO3 = 1.0
                    IF (NH4LEFT(L) .LE. NH4MN) FNH4 = 0.0  
                    IF (FNH4 .GT. 1.0)  FNH4 = 1.0
                    ! Water effects
                    IF (SW(L) .LE. DUL(L)) THEN
                        WFNU = (SW(L) - LL(L)) / (DUL(L) - LL(L)) 
                    ELSE
                        WFNU = 1.0-(SW(L)-DUL(L))/(SAT(L)-DUL(L))
                        WFNU = 1.0 ! Wet soil effect not implemented
                    ENDIF
                    IF (WFNU.LT.0.0) WFNU = 0.0
                    ! LAH Note that WFNU squared
                    TVR2 = AMAX1(0.0,1.0 - H2OCF*(1.0-(WFNU*WFNU)))
                    RFAC = RLV(L) * TVR2 * DLAYR(L) * 100.0
                    RNO3U(L) = RFAC * FNO3 * RTNO3
                    RNH4U(L) = RFAC * FNH4 * RTNH4
                    RNO3U(L) = MAX(0.0,RNO3U(L))
                    RNH4U(L) = MAX(0.0,RNH4U(L))
                    NUPAP = NUPAP + RNO3U(L) + RNH4U(L) !kg[N]/ha
                ENDIF
            ENDDO
    
            ! Ratio (NUPRATIO) to indicate N supply for output
            IF (ANDEM.GT.0) THEN
                NUPRATIO = NUPAP/ANDEM
            ELSE
                IF (NUPAP.GT.0.0) THEN
                    NUPRATIO = 10.0
                ELSE  
                    NUPRATIO = 0.0
                ENDIF  
            ENDIF
            ! Factor (NUF) to reduce N uptake to level of demand
            NUF = 1.0
            IF (NUPAP.GT.0.0) THEN
                NUF = AMIN1(1.0,ANDEM/NUPAP)
            ENDIF 
    
            ! Actual N uptake by layer roots based on demand (kg/ha)
            UNO3 = 0.0
            UNH4 = 0.0
            NUPD = 0.0
            NUPAD = 0.0
            DO L = 1, NLAYRROOT
                UNO3(L) = RNO3U(L)*NUF
                UNH4(L) = RNH4U(L)*NUF
                IF (FAC(L).LE.0.0) THEN
                    XMIN = 0.0
                ELSE  
                    XMIN = NO3MN/FAC(L) 
                ENDIF  
                UNO3(L) = MAX(0.0,MIN (UNO3(L),SNO3(L)-XMIN))
                IF (FAC(L).LE.0.0) THEN
                    XMIN = 0.0
                ELSE  
                    XMIN = NH4MN/FAC(L) 
                ENDIF  
                XMIN = NH4MN/FAC(L) 
                UNH4(L) = MAX(0.0,MIN (UNH4(L),SNH4(L)-XMIN))
                NUPAD = NUPAD + UNO3(L) + UNH4(L)                  
            END DO
            IF (PLTPOP > 1.E-6) THEN
                NUPD = NUPAD/(PLTPOP*10.0)
            ELSE
                NUPD = 0.
            ENDIF
    
            SEEDNUSE2 = 0.0
            ! Seed use after using reserves and uptake
            IF (RTWT.GT.0.0.AND.ISWNIT.NE.'N') THEN
                SEEDNUSE2 = AMAX1(0.0,AMIN1(SEEDN-SEEDNUSE,LNDEM+SNDEM+RNDEM-RSNUSED-SEEDNUSE-NUPD,SEEDNI/SDDUR*(TT/STDAY)))
            ELSE
                SEEDNUSE2 = 0.0
            ENDIF
    
            ! Distribute N to leaves,stem,root,and storage root
            LNUSE = 0.0
            SNUSE = 0.0
            RNUSE = 0.0
            SRNUSE = 0.0
            NULEFT = SEEDNUSE+SEEDNUSE2+RSNUSED+NUPD
    
            ! For supplying minimum
            NDEMMN = GROLF*LNCM+RTWTG*RNCM+(GROST+GROCR)*SNCM+GROSR*(SRNPCS/100.0)*0.5
            LNUSE(1) = (GROLF*LNCM)*AMIN1(1.0,NULEFT/NDEMMN)
            RNUSE(1) = (RTWTG*RNCM)*AMIN1(1.0,NULEFT/NDEMMN)
            SNUSE(1) = ((GROST+GROCR)*SNCM)*AMIN1(1.0,NULEFT/NDEMMN)
            SRNUSE(1) = (GROSR*(SRNPCS/100.0)*0.5)*AMIN1(1.0,NULEFT/NDEMMN)
    
            ! Reduce stem,crown,root growth if N < supply minimum
            IF (NDEMMN.GT.NULEFT) THEN
                GROSTADJ = GROST*AMIN1(1.0,NULEFT/NDEMMN)
                GROCRADJ = GROCR*AMIN1(1.0,NULEFT/NDEMMN)
                RTWTGADJ = RTWTG*AMIN1(1.0,NULEFT/NDEMMN)
                RTRESPADJ = RTWTGADJ*RRESP/(1.0-RRESP)   
            ELSE
                GROSTADJ = GROST
                GROCRADJ = GROCR
                RTWTGADJ = RTWTG
                RTRESPADJ = RTRESP   
            ENDIF
    
            NULEFT = NULEFT - LNUSE(1)-RNUSE(1)-SNUSE(1)-SRNUSE(1)
    
            ! 5.For leaf growth to standard N (N to leaves first)
            LNUSE(2) = AMIN1(NULEFT,(GROLF*LNCX)-LNUSE(1))
            !Could use the NLLG parameter but may need to adjust 
            !the photosynthesis-leaf N response parameters, or
            !the standard PARUE  
            !LNUSE(2) = AMIN1(NULEFT,(GROLF*LNCX*NLLG)-LNUSE(1))
            NULEFT = NULEFT - LNUSE(2)
    
            ! 6.For distribution of remaining N to st,rt,storage root
            NDEM2 = SNDEM-SNUSE(1)+RNDEM-RNUSE(1)+SRNDEM-SRNUSE(1)
            IF (NDEM2.GT.0.0)THEN
                SNUSE(2) = (SNDEM-SNUSE(1)) * AMIN1(1.0,NULEFT/NDEM2)
                RNUSE(2) = (RNDEM-RNUSE(1)) * AMIN1(1.0,NULEFT/NDEM2)
                SRNUSE(2) = (SRNDEM-SRNUSE(1))*AMIN1(1.0,NULEFT/NDEM2)
                NULEFT = NULEFT - SNUSE(2) - RNUSE(2) - SRNUSE(2)
                IF (NULEFT.GT.0.0) THEN
                    LNUSE(3) = NULEFT
                ELSE
                    LNUSE(3) = 0.0
                ENDIF
            ELSE
                LNUSE(3) = 0.0
                SNUSE(2) = 0.0
                RNUSE(2) = 0.0
                SRNUSE(2) = 0.0
            ENDIF  
    
            LNUSE(0) = LNUSE(1) + LNUSE(2) + LNUSE(3)
            SNUSE(0) = SNUSE(1) + SNUSE(2)
            RNUSE(0) = RNUSE(1) + RNUSE(2) 
            SRNUSE(0) = SRNUSE(1) + SRNUSE(2) 
    
            ! N Pools available for re-mobilization
            NUSEFAC = NLABPC/100.0
            NPOOLR = AMAX1 (0.0,((RTWT-SENRTG)*(RANC-RNCM)*NUSEFAC))
            NPOOLL = AMAX1 (0.0,((LFWT-SENLFG-SENLFGRS)*(LANC-LNCM)*NUSEFAC))
            NPOOLS = AMAX1 (0.0,((STWT+CRWT)*(SANC-SNCM)*NUSEFAC))
    
            ! Check N and reduce leaf growth if not enough N  
            IF (ABS(NULEFT).LE.1.0E-5) THEN   ! Inadequate N
                IF (NLLG.GT.0.0.AND.LNCX.GT.0.0) THEN 
                    IF ((LNUSE(1)+LNUSE(2))/GROLF.LT.(LNCX*NLLG)) THEN 
                        GROLFADJ = (LNUSE(1)+LNUSE(2))/(LNCX*NLLG)
                    ELSE  
                        GROLFADJ = GROLF
                    ENDIF  
                ENDIF
                RSSRWTGLFADJ = GROLF - GROLFADJ
        
                AREAPOSSIBLEN =GROLFADJ*(1.0-LPEFR)*(LAWL(1)*(1.0+LAWFF))
        
                ! If not enough N set N factor
                IF (PLAGSB3.GT.AREAPOSSIBLEN.AND.PLAGSB3.GT.0.0)THEN
                    NFLF2(0) = AREAPOSSIBLEN/PLAGSB3
                ELSE  
                    NFLF2(0) = 1.0
                ENDIF
        
                ! Area and assimilate factors for each leaf
                DO L = MAX(1,LNUMSG-1-INT((LLIFG/PHINTS))),LNUMSG+1 
                    IF (LNUMSG.LT.LNUMX) THEN
                        LATL4(1,L)= LATL3(1,L) * NFLF2(0)            
                        NFLF2(L) = AMIN1(1.0,NFLF2(L) + AMAX1(0.0,NFLF2(0)) * (LATLPOT(1,L)-LATLPREV(1,L))/LAPOTX(L))
                    ENDIF  
                ENDDO
        
                PLAGSB4 = PLAGSB3 * NFLF2(0)
                SHLAGB4(1) = SHLAGB3(1) * NFLF2(0)
                SHLAGB4(2) = SHLAGB3(2) * NFLF2(0)
                SHLAGB4(3) = SHLAGB3(3) * NFLF2(0)
        
            ELSE   ! Adequate N 
        
                NFLF2(0) = 1.0
                DO L = MAX(1,LNUMSG-1-INT((LLIFG/PHINTS))),LNUMSG+1 
                    IF (LNUMSG.LT.LNUMX) THEN
                        LATL4(1,L)= LATL3(1,L) * NFLF2(0)            
                        NFLF2(L) = AMIN1(1.0,NFLF2(L) + AMAX1(0.0,NFLF2(0)) * (LATLPOT(1,L)-LATLPREV(1,L))/LAPOTX(L))
                    ENDIF  
                ENDDO
                
            ENDIF
    
        ELSE     ! ISWNIT = N   
    
            LATL4 = LATL3
            NFLF2 = 1.0            
    
        ENDIF    ! End of N uptake and growth adjustmenets

        !-----------------------------------------------------------------------
        !           Reserves growth
        !-----------------------------------------------------------------------

        GRORS = CARBOT+GROLSSD+GROLSRT+SENLFGRS-GROLFADJ-GROSTADJ-GROCRADJ-GROSR
        IF(GRORS.LT.0.0.AND.GRORS.GT.-1.0E-07) GRORS = 0.0

        ! Reserves to STORAGE ROOT if conc too great (overflow!)
        SRWTGRS = 0.0
        ! Determine potential new concentration
        IF (LFWT+GROLFADJ+STWT+CRWT+GROSTADJ+GROCRADJ.GT.0.0) TVR1 = (RSWT+GRORS)/((LFWT+GROLFADJ-SENLFG-SENLFGRS)+ &
        (STWT+GROSTADJ+CRWT+GROCRADJ)+(RSWT+GRORS))
        IF(TVR1.LT.0.0.AND.TVR1.GT.-1.0E-07) TVR1 = 0.0
        IF (TVR1.GT.RSPCO/100.0) THEN   ! If potential>standard 
            TVR2 = RSWT+GRORS             ! What rswt could be
            TVR3 =   ((RSPCO/100.0)*(LFWT+GROLFADJ-SENLFG-SENLFGRS+STWT+CRWT+GROSTADJ+GROCRADJ))/(1.0-(RSPCO/100.0))! What rswt should be 
            SRWTGRS = (TVR2 - TVR3) 
            ! Determine FINAL new concentration
            IF (LFWT+GROLFADJ+STWT+CRWT+GROSTADJ+GROCRADJ.GT.0.0) TVR5 = (RSWT+GRORS-SRWTGRS)/((LFWT+GROLFADJ-SENLFG-SENLFGRS)+ &
                (STWT+GROSTADJ+CRWT+GROCRADJ)+(RSWT+GRORS-SRWTGRS))
        ENDIF

        !-----------------------------------------------------------------------
        !           Height growth
        !-----------------------------------------------------------------------

        CANHTG = 0.0
        CANHTG = SERX*DU

        !-----------------------------------------------------------------------
        !           Root depth and length growth, and distribution
        !-----------------------------------------------------------------------

        RTWTGL = 0.0
        RTWTSL = 0.0
        RTWTUL = 0.0
        RTNSL = 0.0

        IF (GERMFR.GT.0.0.OR.GESTAGE.GE.0.5) THEN
    
            ! Establish water factor for root depth growth
            IF (ISWWAT.NE.'N') THEN
                LRTIP = CSIDLAYR (NLAYR, DLAYR, RTDEP) ! Root tip layer
                IF (LRTIP.GT.1) THEN
                    SWPRTIP = SWP(LRTIP)
                ELSE
                    SWPRTIP = AMIN1(SWP(2),(SWP(2)-((DLAYR(1)-RTDEP)/DLAYR(1))*(SWP(2)-SWP(1))))
                ENDIF
                WFRG = 1.0
                IF (WFRTG.GT.0.0)WFRG = AMAX1(0.0,AMIN1(1.0,(SWPRTIP/WFRTG)))
            ELSE
                WFRG = 1.0
            ENDIF
    
            ! Root depth growth
            RTDEPG = 0.0
            IF (ISWWAT.NE.'N') THEN
                ! LAH Note reduced effect of SHF, AND no acceleration
                RTDEPG = TT*RDGS/STDAY*GERMFR* SQRT(AMAX1(0.3,SHF(LRTIP))) * WFRG
            ELSE
                RTDEPG = TT*RDGS/STDAY*GERMFR
            ENDIF
            L = 0
            CUMDEP = 0.0
            RTDEPTMP = RTDEP+RTDEPG
            DO WHILE ((CUMDEP.LE.RTDEPTMP) .AND. (L.LT.NLAYR))
                L = L + 1
                CUMDEP = CUMDEP + DLAYR(L)
                ! LAH Limit on WFRG. 0 WFRG (when 1 layer) -> 0 TRLDF.
                IF (ISWWAT.NE.'N'.AND.WFRTG.GT.0.0) THEN
                    WFRG = AMIN1(1.0,AMAX1(0.1,SWP(L)/WFRTG))
                ELSE
                    WFRG = 1.0
                ENDIF
                IF (ISWNIT.NE.'N'.AND.NCRG.GT.0.0) THEN
                    NFRG = AMIN1(1.0,AMAX1(0.1,(NO3LEFT(L)+NH4LEFT(L))/NCRG))
                ELSE
                    NFRG = 1.0
                ENDIF 
                ! LAH Tried to use AMAX1 here because layer may have 
                ! lots H20,no N,or inverse, and therefore need roots
                ! But with KSAS8101,AMAX1 lowered yield. Return to AMIN1
                !RLDF(L) = AMAX1(WFRG,NFRG)*SHF(L)*DLAYR(L)
                RLDF(L) = AMIN1(WFRG,NFRG)*SHF(L)*DLAYR(L)
            END DO
            IF (L.GT.0.AND.CUMDEP.GT.RTDEPTMP) RLDF(L) = RLDF(L)*(1.0-((CUMDEP-RTDEPTMP)/DLAYR(L)))
            NLAYRROOT = L
            ! Root senescence
            SENRTG = 0.0
            DO L = 1, NLAYRROOT
                RTWTSL(L) = RTWTL(L)*(RSEN/100.0)*TT/STDAY 
                ! LAH Temperature effect above is not from soil temp
                IF (RTWT.GT.0.0) RTWTUL(L) = RTWTL(L)*GROLSRT/RTWT
                SENRTG = SENRTG + RTWTSL(L)
                IF (ISWNIT.NE.'N') THEN
                    RTNSL(L) = RTWTSL(L)*RANC
                ELSE
                    RTNSL(L) = 0.0
                ENDIF  
            ENDDO
    
            ! Root weight growth by layer
            TRLDF = 0.0
            DO  L = 1, NLAYRROOT
                TRLDF = TRLDF + RLDF(L)
            END DO
            IF (TRLDF.GT.0.0) THEN
                DO  L = 1, NLAYRROOT
                    RTWTGL(L) = (RLDF(L)/TRLDF)*(RTWTGADJ)
                END DO
            ENDIF
        ENDIF

        !-----------------------------------------------------------------------
        !           Water in profile and rootzone
        !-----------------------------------------------------------------------
            
        AH2OPROFILE = 0.0
        H2OPROFILE = 0.0
        AH2OROOTZONE = 0.0
        H2OROOTZONE = 0.0
        DO L = 1, NLAYR
            AH2OPROFILE = AH2OPROFILE+((SW(L)-LL(L))*DLAYR(L))*10.
            H2OPROFILE = H2OPROFILE + SW(L)*DLAYR(L)*10.0
            IF (RLV(L).GT.0.0) THEN
                AH2OROOTZONE=AH2OROOTZONE+((SW(L)-LL(L))*DLAYR(L))*10.
                H2OROOTZONE = H2OROOTZONE+SW(L)*DLAYR(L)*10.
            ENDIF
        END DO

        !-----------------------------------------------------------------------
        !           Rate variables expressed on an area basis
        !-----------------------------------------------------------------------

        ! C assimilation
        ! Senesced material added to litter or soil
        SENWALG = 0.0
        SENNALG = 0.0
        SENCALG = 0.0
        SENLALG = 0.0
        SENWAGS = 0.0
        SENCAGS = 0.0
        SENLAGS = 0.0
        SENNAGS = 0.0
        SENWALG(0) = SENTOPLITTERG * PLTPOP*10.0
        SENCALG(0) = SENWALG(0) * 0.4 
        SENLALG(0) = (SENLFG*LLIGP/100) * PLTPOP*10.0
        SENNALG(0) = SENNLFG * SENFR * PLTPOP*10.0
        ! Root senescence
        DO L = 1, NLAYR
            SENWALG(L) = RTWTSL(L) * PLTPOP*10.0
            SENNALG(L) = RTNSL(L) * PLTPOP*10.0
            SENCALG(L) = SENWALG(L) * 0.4
            SENLALG(L) = SENWALG(L) * RLIGP/100.0
            SENWAGS = SENWAGS + SENWALG(L)
            SENCAGS = SENCAGS + SENCALG(L)
            SENLAGS = SENLAGS + SENLALG(L)
            SENNAGS = SENNAGS + SENNALG(L)
        ENDDO

        IF (ESTABLISHED.NE.'Y'.AND.SHRTD.GT.2.0) ESTABLISHED = 'Y'
    END SUBROUTINE CS_GROWTH
