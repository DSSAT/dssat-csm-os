!***************************************************************************************************************************
! This is the code from the section (DYNAMIC == INTEGR) lines 6730 - 6900 of the original CSCAS code. The names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement and are declared here. The variables 
! that are not arguments are declared in module YCA_First_Trans_m. Unless identified as by MF, all comments are those of 
! the original CSCAS.FOR code.
!
! Subroutine YCA_Out_Work outputs Work details (A OUTPUTS ). 
!***************************************************************************************************************************
    
    SUBROUTINE YCA_Out_Work ( & 
        BRSTAGE     , CN          , CO2         , DOY         , EO          , IDETL       , IRRAMT      , NFP         , &
        RAIN        , WINDSP      , YEAR        &
        )
        
        USE YCA_First_Trans_m
        USE YCA_Formats_m
        USE YCA_Control_Leaf
        USE YCA_Control_Plant
     
        IMPLICIT NONE 
     
        INTEGER :: CN          , DOY         , YEAR       

        REAL    :: BRSTAGE     , CO2         , EO          , IRRAMT      , NFP         , RAIN        , WINDSP      

        CHARACTER(LEN=1)  :: IDETL       
        
        !---------------------------------------------------------------------------------------------------------------
        !         IDETL = A (Outputs Work details)
        !---------------------------------------------------------------------------------------------------------------     
            
        IF (IDETL == 'A') THEN
            WRITE(fnumwrk,*)' '
            WRITE(fnumwrk,'(A25,I16,I7,I7)')' Year,day,DAP            ',YEAR,DOY,DAP
            WRITE(fnumwrk,'(A34,2F7.3)')' Rainfall,Irrigation mm           ',rain,irramt
            WRITE(fnumwrk,'(A34,2F7.3)')' Tmean,Tcan oC                    ',tmean,tcan
            WRITE(fnumwrk,'(A34,2F7.3)')' Tcan-Tmean Today and average oC  ',tcan-tmean,tdifav
            WRITE(fnumwrk,'(A34,F7.1)')' Windspeed m/s                    ',windsp     
            WRITE(fnumwrk,'(A34,2F7.3,2F7.1)')' Brstage,Lnum. Beginning,end of day',brstageprev,brstage,lnumprev,lnum
            IF (plantGreenLeafArea() < 9999.9) THEN
                WRITE(fnumwrk,'(A34,F7.1,F7.1)')' Laminae area end day /m2,/plant  ',lai,plantGreenLeafArea()
            ELSE
                WRITE(fnumwrk,'(A34,F7.1,I7)')' Laminae area end day /m2,/plant  ',lai,NINT(plantGreenLeafArea())
            ENDIF
            WRITE(fnumwrk,'(A25,I1,A7,2F7.3)')' PARI,competition model,C',CN,' 1-crop',PARI,PARI1
            IF (Rlf > 0.0) THEN
                WRITE(fnumwrk,'(A34,2F7.1,2F7.1)')' Ratm,Rcrop,Rcrop*Rco2/R,*H2o     ',ratm,rcrop,rcrop*rlfc/rlf, &
                    rcrop*rlfc/rlf*(1.0-(1.0-wfp))
            ELSE
                WRITE(fnumwrk,'(A34,2F7.1)')' Ratm,Rcrop                       ',ratm,rcrop                         
            ENDIF
            IF (FILEIOT /= 'XFL') THEN
                IF (IDETL == 'D'.OR.IDETL == 'A') THEN
                    IF (meevp == 'R')THEN
                        WRITE(fnumwrk,'(A50)')' Model (CSM) pot.evap.method: Priestley-Taylor R  '
                    ELSEIF (meevp == 'P')THEN
                        WRITE(fnumwrk,'(A51)')' Model (CSM) pot.evap.method: FAO Penman (FAO-24) P'
                    ELSEIF (meevp == 'F')THEN
                        WRITE(fnumwrk,'(A50,A10)')' Model (CSM) pot.evap.method: FAO Penman-Monteith ', '(FAO-56) F'
                    ELSEIF (meevp == 'D')THEN 
                        WRITE(fnumwrk,'(A53,A10)')' Model (CSM) pot.evap.method: Dynamic Penman-Monteith'
                    ENDIF
                    !MEEVP CSM Model routines
                    !   F  PETPEN  FAO Penman-Monteith (FAO-56) potential evapotranspiration 
                    !                with KC = 1.0
                    !   R  PETPT   Calculates Priestly-Taylor potential evapotranspiration
                    !                (default method)
                    !   D  PETDYN  Dynamic Penman-Monteith, pot. evapotranspiration, with
                    !                dynamic input of LAI, crop height effects on Ra and Rs
                    !   P  PETPNO  FAO Penman (FAO-24) potential evapotranspiration 
                    !   M  PETMEY  "Standard reference evaporation calculation for inland 
                    !                south eastern Australia" By Wayne Meyer 1993
                    WRITE(fnumwrk,'(A34,4F7.3,F8.3)')' EO  P-T,Pen,M-Pen,Ebud,Model     ',eopt,eopen,eompen,eoebud,eo
                    WRITE(fnumwrk,'(A34,4F7.3,F8.3)')' EOCrp                            ',eopt,eopen,eompcrp,eoebudcrp,eo
                    WRITE(fnumwrk,'(A34,4F7.3,F8.3)')' EOCrpCo2                         ',eopt,eopen,eompcrpco2, &
                        eoebudcrpco2,eo
                    WRITE(fnumwrk,'(A34,4F7.3,F8.3)')' EOCrpCo2H2o                         ',eopt,eopen,eompcrpco2h2o, &
                        eoebudcrpco2h2o,eo
                    IF (WFP < 1.0.AND.WFP > 0.0)WRITE(fnumwrk,'(A41,F4.2,A8,F4.1,A8,F4.1)') &
                        ' NB.Water stress effect operative. WFP = ',wfp,' TCAN = ',tcan,' TAIR = ',tmean
                    WRITE(fnumwrk,'(A34,4F7.1,F8.1)')' EOC P-T,Pen,M-P,Ebud,Model       ',eoptc,eopenc,eompenc,eoebudc,eoc
                    WRITE(fnumwrk,'(A34,4F7.1,F8.1)')' EOCrpC                           ',eoptc,eopenc,eompcrpc,eoebudcrpc,eoc
                    WRITE(fnumwrk,'(A34,4F7.1,F8.1)')' EOCrpCo2C                        ',eoptc,eopenc,eompcrpco2c, &
                        eoebudcrpco2c,eoc
                    WRITE(fnumwrk,'(A34,4F7.1,F8.1)')' EOCrpCo2h2oC                     ',eoptc,eopenc,eompcrpco2h2oc, &
                        eoebudcrpco2h2oc,eoc
                ENDIF
            ENDIF
            IF (EYEARDOY <= YEARDOY) THEN
                WRITE(fnumwrk,'(A,2F7.3)')     ' Pot.pl./Pot.soil evap; /Pot.pl330',epsratio,tratio
                WRITE(fnumwrk,'(A,F7.3)')      ' Quantum requirement              ',photqr
                WRITE(fnumwrk,'(A,2F7.1)')     ' CO2                              ',co2
                WRITE(fnumwrk,'(A,F7.3,6F7.3)')' Phs facs Co2,Temp,H2o,N,Rsvs,Vpd ',co2fp,tfp,wfp,nfp,rsfp,vpdfp
                WRITE(fnumwrk,'(A,3F7.3)')     ' Phs. Rue,Rue+Co2i,Resistances    ',carbobegr*pltpop,carbobegi*pltpop, &
                    carbobegm*pltpop
                WRITE(fnumwrk,'(A,3F7.2)')     ' CH2O Start,end,remobilized       ',carbobeg*pltpop*10.,carboend*plantPopulation(), &
                    senlfgrs*plantPopulation()
                !IF(CUMDU <= DUSRI.AND.CUMDU+DU > DUSRI.AND.SRDAYFR > 0.0)THEN !LPM 05JUN2015 DUSRI is not used
                !    WRITE(fnumwrk,'(A, I7)')   ' STORAGE ROOT INITIATION (no/pl)  ',srnopd
                !    WRITE(fnumwrk,'(A,2F7.1)') ' Canopy wt at end of day (kg/ha)  ',(vegetativeCanopyWeight())*plantPopulation()
                !    WRITE(fnumwrk,'(A,F7.1)')  ' Storage root fraction            ',srfr
                !ENDIF
                IF (SRWTGRS+SRWTGRS > 0.0) WRITE(FNUMWRK,'(A)')' Surplus assimilates sent to storage roots      '
                IF (LRTIP == 1) WRITE(fnumwrk,'(A)')' Root tip in layer 1 '
                WRITE(FNUMWRK,'(A,2F7.2)')     ' N demand,shortage (kg/ha)        ',andem,AMAX1(0.0,andem-nupap)
                ! Folowing detailed outputs can be printed if tvi1=1
                tvi1 = 0
                IF (tvi1.eq.1) THEN
                    IF (ANDEM <= 0.0) THEN
                        WRITE(FNUMWRK,'(A)')' N demand at zero! Components of demand/use:' 
                    ELSE
                        WRITE(FNUMWRK,'(A)')' N demand above zero! Components of demand/use:' 
                    ENDIF  
                    WRITE(FNUMWRK,*)' Leaves            ',lndem*plantPopulation()
                    WRITE(FNUMWRK,*)' Stem              ',sndem*plantPopulation()
                    WRITE(FNUMWRK,*)' Roots             ',rndem*plantPopulation()
                    WRITE(FNUMWRK,*)' Storage root      ',srndem*plantPopulation()
                    WRITE(FNUMWRK,*)' Seed use          ',(seednuse+seednuse2)*plantPopulation()
                    WRITE(FNUMWRK,*)' Reserves use      ',rsnused*plantPopulation()
                    IF (ANDEM > 0.0.AND.NUPAP < ANDEM) WRITE(fnumwrk,'(A)')'  N uptake insufficient to meet demand'
                    IF (ANDEM > 10.0) WRITE(fnumwrk,'(A,F4.1,A,F4.1)')' N demand (',ANDEM, &
                        ') very high! Uptake = ',nuf*andem
                ENDIF 
            ENDIF ! End EYEARDOY <= YEARDOY
        ENDIF ! End detailed WORK writes  IDDETL = 'A'   

    END SUBROUTINE YCA_Out_Work