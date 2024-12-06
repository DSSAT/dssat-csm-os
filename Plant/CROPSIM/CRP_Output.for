!**********************************************************************
! Subroutine CRP_Output takes all the code from original CSCRP code 
! lines 9225 - 12628.
!**********************************************************************

      SUBROUTINE CRP_Output (GSTAGE, LAI, CANHT, CN, DAYLT, DOY,
     &     DYNAMIC, EOP, IDETG, IDETL, IDETO, IDETS,
     &     ISWNIT, ISWWAT, KCAN, MESOM , NFP, NLAYR, ON, REP,
     &     RLV, RN, RNMODE, RUN, RUNI, SN, SRAD, STGYEARDOY, TN,
     &     TNIMBSOM, TOMINSOM1, UNH4, UNO3, YEAR)

! 2023-04-13 TF removed unused variables in argument list
!     IRRAMT, CO2, EO, RAIN, WINDSP

      USE ModuleDefs
      USE CRP_First_Trans_m
  
      IMPLICIT NONE
      EXTERNAL YR_DOY, GETLUN, SUMVALS, HEADER, TVILENT, TVICOLNM, 
     &    TL10FROMI, LTRIM, CSTIMDIF, CSOPLINE, CALENDAR, DAPCALC, 
     &    LTRIM2, AREADR, AREADI, CSYDOY, GETSTRI, CSCLEAR5, GETSTR, 
     &    GETSTRR, WARNING, CSUCASE
      SAVE
    
      INTEGER CN, DOY, DYNAMIC, NLAYR, ON, REP, RN          
      INTEGER RUN, RUNI, SN, STGYEARDOY(20), TN, YEAR
      INTEGER CSTIMDIF, DLAYR(NL), TVILENT, CSYDOY, DAPCALC
      INTEGER TVICOLNM
      REAL GSTAGE, LAI, CANHT, EOP !IRRAMT, CO2, EO
      REAL KCAN, NFP, RLV(NL), SRAD, TNIMBSOM, TOMINSOM1 !RAIN
      REAL UNH4(NL), UNO3(NL), DAYLT, BD(NL) !WINDSP, RTWTSGE
      REAL NH4LEFT(NL), NO3LEFT(NL)
      CHARACTER(LEN=1) IDETG, IDETL, IDETO, IDETS, ISWNIT, ISWWAT      
      CHARACTER(LEN=1) MESOM, RNMODE 
      CHARACTER(LEN=10) :: TL10FROMI
         
!         Simulated outputs only
!          IDETG (GROUT in controls (Y,N))  Plant growth outputs
!           Y->Work_details+Plantgro+Plantgr2+Plantgrf
!              +PlantN(If N switched on)
!           FROUT->#=number of days between outputs
!          IDETS (SUMRY in controls (Y,N)) Summary outputs
!           Y->Summary+Plantsum+Work(Harvest)                        
!        
!         Simulated+Measured outputs
!          IDETO (OVVEW in controls (Y,E,N)) Overview outputs
!           Y->Overview+Evaluate(+Measured if IDETG=Y)
!           E->Evaluate only
!          IDETL (VBOSE in controls (0,N,Y,D,A))
!           Y->Leaves+Phases+Measured                 
!           D->+Phenols+Phenolm+Plantres+Plantrem
!           A->Errora+Errors+Errort+Full Reads
!           0,A are meta switches:
!             0 switches everything to N apart from IDETS,which given a Y,
!               and IDETO,which given an E when RNMODE is not N (seasonal)
!             A switches ALL outputs on  

        ! If model failure so that cycle not completed
        IF (DYNAMIC.EQ.SEASEND .AND. SEASENDOUT.NE.'Y') THEN
          laix = -99.0
          cwahc = -99.0
          nupac = -99.0
          hwam = -99.0
          hiam = -99.0
          sennatc = -99.0
          gfdur = -99
        ENDIF

        DAS = MAX(0,CSTIMDIF(YEARSIM,YEARDOY))

        SLAOUT = -99.0
        !OUTCHOICE = 3  ! Set earlier when writing to work.out
        ! Note possibilities. To change must recompile.
        IF (OUTCHOICE.EQ.1) THEN
          ! 1. Include reserves, stem wt includes sheath
          LLWADOUT = LLWAD+LLRSWAD
          STWADOUT = STWAD+STRSWAD + LSHWAD+LSHRSWAD
          CHWADOUT = CHWAD+CHRSWAD
          IF (LFWT.GT.1.0E-6) 
     &     SLAOUT=(PLA-SENLA-LAPHC)/(LFWT*(1.0-LSHFR)+LLRSWT)
        ELSEIF (OUTCHOICE.EQ.2) THEN
          ! 2. No reserves, stem wt includes sheath
          LLWADOUT = LLWAD
          STWADOUT = STWAD + LSHWAD
          CHWADOUT = CHWAD
          IF (LFWT.GT.1.0E-6)SLAOUT=(PLA-SENLA-LAPHC)/(LFWT*(1.0-LSHFR))
        ELSEIF (OUTCHOICE.EQ.3) THEN
          ! 3. No reserves, stem wt does not includes sheath
          LLWADOUT = LLWAD
          STWADOUT = STWAD
          CHWADOUT = CHWAD
          IF (LFWT.GT.1.0E-6)SLAOUT=(PLA-SENLA-LAPHC)/(LFWT*(1.0-LSHFR))
          ! Stem area (NOT including leaf sheath)
          SAID = STAI
        ENDIF  
        IF (SLA.LE.0.0) SLAOUT = -99.0
        
        CALL Csopline(sentoplitterac,(sentoplittera))
        CALL Csopline(senrootc,(senroota))
        CALL Csopline(laic,lai)
        CALL Csopline(caic,caid)
        CALL Csopline(hindc,hind)
        CALL Csopline(hwudc,hwud)
        CALL Csopline(sdwadc,sdwad)
        CALL Csopline(gstagec,gstage)
        
        ! Calculate Pari to equate to updated LAI
        PARIOUT = (1.0 - EXP((-KCAN)*LAI))

!-----------------------------------------------------------------------
!       TIME SEQUENCE OUTPUTS (WorkPlantgro,gr2,grf)
!-----------------------------------------------------------------------

        IF (  (MOD(DAS,FROPADJ).EQ.0.AND.YEARDOY.GE.PLYEARDOY)
     &   .OR. (YEARDOY.EQ.PLYEARDOY)
     &   .OR. (YEARDOY.EQ.STGYEARDOY(1))
     &   .OR. (YEARDOY.EQ.STGYEARDOY(HSTG))
     &   .OR. (YEARDOY.EQ.STGYEARDOY(11))) THEN

!-----------------------------------------------------------------------
!         IDETL = A OUTPUTS (Work details)
!-----------------------------------------------------------------------     

          IF (IDETL.EQ.'A') THEN
!            WRITE(fnumwrk,*)' '
!            WRITE(fnumwrk,'(A25,I16,I7,I7)')
!     &       ' Year,day,DAP            ',YEAR,DOY,DAP
!            WRITE(fnumwrk,'(A34,2F7.3)')
!     &       ' Rainfall,Irrigation mm           ',rain,irramt
!            WRITE(fnumwrk,'(A34,2F7.3)')
!     &       ' Tmean,Tcan oC                    ',tmean,tcan
!            WRITE(fnumwrk,'(A34,2F7.3)')
!     &       ' Tcan-Tmean Today and average oC  ',tcan-tmean,tdifav
!            WRITE(fnumwrk,'(A34,F7.1)')
!     &       ' Windspeed m/s                    ',windsp     
!            WRITE(fnumwrk,'(A34,2F7.3,2F7.1)')
!     &       ' Rstage,Lnum. Beginning,end of day',
!     &       rstagep,rstage,lnumprev,lnum
            IF (CUMDU.GE.LGPHASEDU(1).AND.CUMDU.LT.LGPHASEDU(2)) THEN
!              WRITE(fnumwrk,'(A36,F5.1,F7.1)')
!     &         ' Phyllochron interval. Std.,actual  ',phints,phint
            ENDIF 
            IF (PLA-SENLA-LAPHC.LT.9999.9) THEN
!              WRITE(fnumwrk,'(A34,F7.1,F7.1)')
!     &        ' Laminae area end day /m2,/plant  ',lai,pla-senla-laphc
            ELSE
!              WRITE(fnumwrk,'(A34,F7.1,I7)')
!     &        ' Laminae area end day /m2,/plant  ',
!     &        lai,NINT(pla-senla-laphc)
            ENDIF
!            WRITE(fnumwrk,'(A25,I1,A8,2F7.3)')
!     &       ' PARI,competition model,C',CN,' 1-crop ',PARI,PARI1
            IF (Rlf.GT.0.0) THEN
!              WRITE(fnumwrk,'(A34,2F7.1,2F7.1)')
!     &         ' Ratm,Rcrop,Rcrop*Rco2/R,*H2o     ',
!     &        ratm,rcrop,rcrop*rlfc/rlf,rcrop*rlfc/rlf*(1.0-(1.0-wfp))
            ELSE
!              WRITE(fnumwrk,'(A34,2F7.1)')
!     &         ' Ratm,Rcrop                       ',
!     &        ratm,rcrop                         
            ENDIF
!            IF (FILEIOT.NE.'XFL') THEN
!             IF (IDETL.EQ.'D'.OR.IDETL.EQ.'A') THEN
!               IF (meevp.EQ.'R')THEN
!                 WRITE(fnumwrk,'(A50)')
!     &         ' Model (CSM) pot.evap.method: Priestley-Taylor R  '
!               ELSEIF (meevp.EQ.'P')THEN
!                 WRITE(fnumwrk,'(A51)')
!     &         ' Model (CSM) pot.evap.method: FAO Penman (FAO-24) P'
!               ELSEIF (meevp.EQ.'F')THEN
!                 WRITE(fnumwrk,'(A50,A10)')
!     &         ' Model (CSM) pot.evap.method: FAO Penman-Monteith ', 
!     &         '(FAO-56) F'
!               ELSEIF (meevp.EQ.'D')THEN
!                 WRITE(fnumwrk,'(A53,A10)')
!     &           ' Model (CSM) pot.evap.method: Dynamic Penman-Monteith'
!               ENDIF
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
!              WRITE(fnumwrk,'(A34,4F7.3,F8.3)')
!     &         ' EO  P-T,Pen,M-Pen,Ebud,Model     ',
!     &         eopt,eopen,eompen,eoebud,eo
!              WRITE(fnumwrk,'(A34,4F7.3,F8.3)')
!     &         ' EOCrp                            ',
!     &         eopt,eopen,eompcrp,eoebudcrp,eo
!              WRITE(fnumwrk,'(A34,4F7.3,F8.3)')
!     &         ' EOCrpCo2                         ',
!     &         eopt,eopen,eompcrpco2,eoebudcrpco2,eo
!              WRITE(fnumwrk,'(A34,4F7.3,F8.3)')
!     &         ' EOCrpCo2H2o                         ',
!     &         eopt,eopen,eompcrpco2h2o,eoebudcrpco2h2o,eo
!              IF (WFP.LT.1.0.AND.WFP.GT.0.0) 
!     &         WRITE(fnumwrk,'(A41,F4.2,A8,F4.1,A8,F4.1)')
!     &         ' NB.Water stress effect operative. WFP = ',wfp,
!     &         ' TCAN = ',tcan,' TAIR = ',tmean
!              WRITE(fnumwrk,'(A34,4F7.1,F8.1)')
!     &         ' EOC P-T,Pen,M-P,Ebud,Model       ',
!     &         eoptc,eopenc,eompenc,eoebudc,eoc
!              WRITE(fnumwrk,'(A34,4F7.1,F8.1)')
!     &         ' EOCrpC                           ',
!     &         eoptc,eopenc,eompcrpc,eoebudcrpc,eoc
!              WRITE(fnumwrk,'(A34,4F7.1,F8.1)')
!     &         ' EOCrpCo2C                        ',
!     &         eoptc,eopenc,eompcrpco2c,eoebudcrpco2c,eoc
!              WRITE(fnumwrk,'(A34,4F7.1,F8.1)')
!     &         ' EOCrpCo2h2oC                     ',
!     &         eoptc,eopenc,eompcrpco2h2oc,eoebudcrpco2h2oc,eoc
!             ENDIF
!            ENDIF
            IF (EYEARDOY.LE.YEARDOY) THEN
!              WRITE(fnumwrk,'(A34,2F7.3)')
!     &         ' Pot.pl./Pot.soil evap; /Pot.pl330',epsratio,tratio
!              WRITE(fnumwrk,'(A34,F7.3)')
!     &         ' Quantum requirement              ',photqr
!              WRITE(fnumwrk,'(A34,2F7.1)')
!     &         ' CO2,Estimated internal CO2 vpm   ',co2,co2intppm
!              WRITE(fnumwrk,'(A34,F7.3,6F7.3)')
!     &         ' Phs facs Co2,Temp,H2o,N,Rsvs,Vpd ',
!     &         co2fp,tfp,wfp,nfp,rsfp,vpdfp
!              WRITE(fnumwrk,'(A34,3F7.3)')
!     &         ' Phs. Rue,Rue+Co2i,Resistances    ',
!     &         carbobegr*pltpop,carbobegi*pltpop,carbobegm*pltpop
!              WRITE(fnumwrk,'(A34,3F7.2)')
!     &         ' CH2O Start,end,remobilized       ',
!     &         carbobeg*pltpop*10.,
!     &         carboend*pltpop*10.0,senlfgrs*pltpop*10.0
!              IF (RTWTGRS.GT.0.0) WRITE(FNUMWRK,'(A34)')
!     &         ' Surplus assimilates sent to roots'
!              IF (LRTIP.EQ.1) WRITE(fnumwrk,'(A21)')
!     &         ' Root tip in layer 1 '
!              WRITE(FNUMWRK,'(A34,3F7.2)')
!     &         ' N demand,uptake,shortage (kg/ha) ',
!     &         andem,nupap,AMAX1(0.0,andem-nupap)
              ! Folowing detailed outputs printed if CFLNOUTPUTS=Y
              IF (CFLNOUTPUTS.EQ.'Y') THEN
!                IF (ANDEM.LE.0.0) THEN
!                  WRITE(FNUMWRK,'(A44)')
!     &              ' N demand at zero! Components of demand/use:' 
!                ELSE
!                  WRITE(FNUMWRK,'(A47)')
!     &              ' N demand above zero! Components of demand/use:' 
!                ENDIF  
!                WRITE(FNUMWRK,*)
!     &            ' Leaves            ',lndem*pltpop*10.0
!                WRITE(FNUMWRK,*)
!     &            ' Stem              ',sndem*pltpop*10.0
!                WRITE(FNUMWRK,*)
!     &            ' Roots             ',rndem*pltpop*10.0
!                WRITE(FNUMWRK,*)
!     &            ' Grain             ',grainndem*pltpop*10.0
!                WRITE(FNUMWRK,'(A47)')
!     &            ' N supplied to grain:                          ' 
!                WRITE(FNUMWRK,*)
!     &            ' Grainngrs,grainngu',
!     &            grainngrs*pltpop*10.0,grainngu*pltpop*10.0
!                WRITE(FNUMWRK,*)
!     &            ' Grainngr,pool     ',grainngr*pltpop*10.0,
!     &             npoolr*pltpop*10.0
!                WRITE(FNUMWRK,*)
!     &            ' Grainngs,pool     ',grainngs*pltpop*10.0,
!     &             npools*pltpop*10.0
!                WRITE(FNUMWRK,*)
!     &            ' Grainngl,,pool    ',grainngl*pltpop*10.0,
!     &             npooll*pltpop*10.0
!                WRITE(FNUMWRK,'(A47)')
!     &            ' N useages:                                    ' 
!                WRITE(FNUMWRK,*)
!     &            ' Seed use          ',seednuse*pltpop*10.0
!                WRITE(FNUMWRK,*)
!     &            ' Reserves use      ',rsnused*pltpop*10.0
!                WRITE(FNUMWRK,*)
!     &            ' Leaf N use        ',grainngl*pltpop*10.0
!                WRITE(FNUMWRK,*)
!     &            ' Stem N use        ',grainngs*pltpop*10.0
!                WRITE(FNUMWRK,*)
!     &            ' Root N use        ',grainngr*pltpop*10.0
!                WRITE(FNUMWRK,'(A47)')
!     &            ' N contents:                                   ' 
                ! Soil N
                SOILN = 0.0
                DO I = 1, NLAYR
                  SOILN = SOILN + NO3LEFT(I)/(10.0/(BD(I)*(DLAYR(I))))
     &                      + NH4LEFT(I)/(10.0/(BD(I)*(DLAYR(I))))
                ENDDO
!                WRITE(FNUMWRK,'(A20,F7.1)')
!     &            '  Soil inorganic N  ',soiln
                IF (lanc.GT.0.0.AND.lncm.GT.0.0) THEN
                  tvr1 = leafn*pltpop*10.0*(lanc-lncm)/lanc       
                ELSE
                  tvr1 = 0.0   
                ENDIF
!                WRITE(FNUMWRK,'(A20,2F7.1)')
!     &            '  Leaf N,total,>min ',leafn*pltpop*10.0,tvr1
                IF (sanc.GT.0.0.AND.sncm.GT.0.0) THEN
                  tvr1 = stemn*pltpop*10.0*(sanc-sncm)/sanc       
                ELSE
                  tvr1 = 0.0   
                ENDIF
!                WRITE(FNUMWRK,'(A20,2F7.1)')
!     &            '  Stem N,total,>min ',stemn*pltpop*10.0,tvr1
                IF (ranc.GT.0.0.AND.rncm.GT.0.0) THEN
                  tvr1 = rootn*pltpop*10.0*(ranc-rncm)/ranc       
                ELSE
                  tvr1 = 0.0   
                ENDIF
!                WRITE(FNUMWRK,'(A20,2F7.1)')
!     &            '  Root N,total,>min ',rootn*pltpop*10.0,tvr1
!                WRITE(FNUMWRK,'(A20,F7.1)')
!     &            '  Grain N           ',grainn*pltpop*10.0  
              ENDIF
!              IF (CCOUNTV.EQ.1) WRITE (fnumwrk,'(A35,I4)')
!     &         ' Maximum leaf number reached on day',DOY
            ENDIF ! End EYEARDOY.LE.YEARDOY
          ENDIF ! End detailed WORK writes  IDDETL = 'A'   


!-----------------------------------------------------------------------
!         IDETG NE N OUTPUTS (Plantgro,gr2,grf,n)
!-----------------------------------------------------------------------     

          IF ((IDETG.NE.'N'.AND.IDETL.NE.'0').OR.IDETL.EQ.'A') THEN
          
            ! PlantGro
            IF (YEARDOY.EQ.PLYEARDOY) THEN
              OPEN (UNIT = NOUTPG, FILE = OUTPG,POSITION = 'APPEND')
              IF (FILEIOT(1:2).EQ.'DS') THEN
                CALL HEADER(2, NOUTPG, RUN)
              ELSE
                WRITE (NOUTPG,'(/,A79,/)') OUTHED
                WRITE (NOUTPG,103) MODEL
                WRITE (NOUTPG,1031) MODNAME
                WRITE (NOUTPG,104)
     &           EXCODE(1:8),' ',EXCODE(9:10),'  ',ENAME(1:47)
                WRITE (NOUTPG,102) TN,TNAME
                WRITE (NOUTPG,107) CROP,VARNO,VRNAME
                CALL Calendar (year,doy,dom,month)
                WRITE(NOUTPG,108)
     &           month,dom,plyeardoy,NINT(pltpopp),NINT(rowspc)
  103           FORMAT (' MODEL            ',A8)
 1031           FORMAT (' MODULE           ',A8)
  104           FORMAT (' EXPERIMENT       ',A8,A1,A2,A2,A47)
  102           FORMAT (' TREATMENT',I3,'     ',A25)
  107           FORMAT (' GENOTYPE         ',A2,A6,'  ',A16)
  108           FORMAT (' PLANTING         ',A3,I3,I8,2X,I4,
     &           ' plants/m2 in ',I3,' cm rows',/)
              ENDIF
              YEARDOY = YEAR*1000 + DOY
              WRITE (NOUTPG,2201)
 2201         FORMAT ('@YEAR DOY   DAS   DAP TMEAN TKILL',
     A               '  GSTD  L#SD',
     B               ' PARID PARUD  AWAD',
     C               '  LAID  SAID  CAID',
     D               '  TWAD SDWAD  RWAD  CWAD  LWAD  SWAD  GWAD  HIAD',
     E               ' CHWAD  EWAD RSWAD SNWPD SNWLD SNWSD',
     F               '  RS%D',
     G               '  H#AD  HWUD',
     I               '  T#AD  SLAD  RDPD  PTFD',
     J               '  SWXD WAVRD',
     K               ' WUPRD  WFTD  WFPD  WFGD',
     L               '  NFTD  NFPD  NFGD NUPRD',
     M               '  TFPD  TFGD',
     N               ' VRNFD DYLFD',
     O               '      ',
     P               '      ')
            ENDIF  ! End Plantgro header writes
            WRITE (NOUTPG,501)
     A      YEAR,DOY,DAS,DAP,TMEAN,TKILL,GSTAGEC,LNUM,
     B      PARIOUT,PARIUE,AMIN1(999.9,CARBOBEG*PLTPOP*10.0),
     &      LAIC,SAID,CAIC,
     C      NINT(TWAD),SDWADC,NINT(RWAD),NINT(CWAD),
     D      NINT(LLWADOUT),NINT(STWADOUT),NINT(HWAD),HIAD,
     E      NINT(CHWADOUT),NINT(EWAD),NINT(RSWAD),
     &      NINT(SENTOPRETAINEDA),SENTOPLITTERAC,SENROOTC,
     F      RSCD*100.0,NINT(HNUMAD),HWUDC,
     G      NINT(TNUMAD),NINT(SLAOUT),RTDEP/100.0,PTF,H2OA,
     H      AMIN1(99.9,WAVR),AMIN1(15.0,WUPR),
     I      1.0-WFT,1.0-WFP,1.0-WFG,
     J      1.0-NFT,1.0-NFP,1.0-NFG,AMIN1(2.0,NUPRATIO),
     K      1.0-TFP,1.0-TFG,1.0-VF,1.0-DFOUT
  501       FORMAT(
     A      I5,I4,2I6,F6.1,F6.1,A6,F6.1,
     B      F6.3,F6.2,F6.1,
     &      A6,F6.3,A6,
     C      I6,A6,2I6,
     C      3I6,F6.3,
     C      3I6,
     C      I6,2A6,
     D      F6.1,I6,A6,
     E      2I6,2F6.2,F6.1,
     F      F6.1,F6.2,
     G      3F6.2,
     H      3F6.2,F6.1,
     I      2F6.2,
     J      2F6.2)
            ! End Plantgro writes
            
            ! PlantGroReductionFactors
            IF (YEARDOY.GT.PLYEARDOY) THEN
              TCDIF = TCAN - TMEAN
            ELSE  
              TCDIF = -99
            ENDIF
            IF (YEARDOY.EQ.PLYEARDOY) THEN
              OPEN (UNIT = NOUTPGF, FILE = OUTPGF,POSITION = 'APPEND')
              IF (FILEIOT(1:2).EQ.'DS') THEN
                CALL HEADER(2, NOUTPGF, RUN)
              ELSE
                WRITE (NOUTPGF,'(/,A79,/)') OUTHED
                WRITE (NOUTPGF,103) MODEL
                WRITE (NOUTPGF,1031) MODNAME
                WRITE (NOUTPGF,104)
     &           EXCODE(1:8),' ',EXCODE(9:10),'  ',ENAME(1:47)
                WRITE (NOUTPGF,102) TN,TNAME
                WRITE (NOUTPGF,107) CROP,VARNO,VRNAME
                CALL Calendar (year,doy,dom,month)
                WRITE(NOUTPGF,108)
     &           month,dom,plyeardoy,NINT(pltpopp),NINT(rowspc)
              ENDIF
              WRITE (NOUTPGF,2215)
 2215         FORMAT ('!........DATES.......  ...TEMP... STAGE ',
     N               ' ...PHENOLOGY.... ',
     1               ' .......PHOTOSYNTHESIS....... ', 
     M               ' .....GROWTH.....  ..TILLERS. ',
     2               'WATER STRESS DETERMINANTS',
     2               ' N STRESS DETERMINANTS        ')
              WRITE (NOUTPGF,2205)
 2205         FORMAT ('@YEAR DOY   DAS   DAP TMEAN TCDIF  GSTD',
     N               '    DU VRNFD DYLFD',
     1               '  TFPD  WFPD  NFPD CO2FD RSFPD', 
     M               '  TFGD  WFGD  NFGD  WFTD  NFTD',
     &               ' WAVRD WUPRD  SWXD  EOPD',
     &               '  SNXD LN%RD SN%RD RN%RD            ')
            ENDIF  ! End Plantgro header writes
            WRITE (NOUTPGF,507)
     A      YEAR,DOY,DAS,DAP,TMEAN,TCDIF,GSTAGEC,
     B      DU,1.0-VF,1.0-DFOUT,
     C      1.0-TFP,1.0-WFP,1.0-NFP,1.0-CO2FP,1.0-RSFP,
     D      1.0-TFG,1.0-WFG,1.0-NFG,1.0-WFT,1.0-NFT,
     H      AMIN1(99.9,WAVR),AMIN1(15.0,WUPR),H2OA,EOP,
     I      SNO3PROFILE+SNH4PROFILE,LNCR,SNCR,RNCR
  507       FORMAT(
     a      I5,I4,2I6,2F6.1,A6,
     b      F6.1,2F6.2,
     c      5F6.2,
     d      5F6.2,
     e      2F6.2,F6.1,F6.2,
     F      F6.1,3F6.2)
            ! End Plantgro reduction factor writes
            
            ! PlantGr2
            IF (YEARDOY.EQ.PLYEARDOY) THEN
              OPEN (UNIT = NOUTPG2, FILE = OUTPG2, STATUS='UNKNOWN',
     &        POSITION = 'APPEND')
              IF (FILEIOT(1:2).EQ.'DS') THEN
                CALL HEADER(2, NOUTPG2, RUN)
              ELSE
                WRITE (NOUTPG2,'(/,A79,/)') OUTHED
                WRITE (NOUTPG2,103) MODEL
                WRITE (NOUTPG2,1031) MODNAME
                WRITE (NOUTPG2,104)
     &           EXCODE(1:8),' ',EXCODE(9:10),'  ',ENAME(1:47)
                WRITE (NOUTPG2,102) TN,TNAME
                WRITE (NOUTPG2,107) CROP,VARNO,VRNAME
                WRITE(NOUTPG2,108)
     &           month,dom,plyeardoy,NINT(pltpopp),NINT(rowspc)
              ENDIF 
              WRITE (NOUTPG2,2251)
 2251         FORMAT ('@YEAR DOY   DAS   DAP TMEAN  GSTD  RSTD',
     A          ' LAIPD LAISD  LAID  CHTD SDWAD SNWLD SNWSD',
     a          '  H#AD  HWUD',
     B          ' SHRTD  PTFD  RDPD',
     C          '  RL1D  RL2D  RL3D  RL4D  RL5D  RL6D',
     D          '  RL7D  RL8D  RL9D RL10D')
            ENDIF   ! Plantgr2 header writes
            LAIPROD = PLA*PLTPOP*0.0001
            CALL Csopline(laiprodc,laiprod)
            CALL Csopline(canhtc,canht)
            L = MAX(1,LNUMSG-INT(LLIFG))
            WRITE (NOUTPG2,502)
     A       YEAR,DOY,DAS,DAP,TMEAN,GSTAGEC,RSTAGE,
     B       LAIPRODC,SENLA*PLTPOP*0.0001,LAIC,CANHTC,SDWAD,
     &       SENTOPLITTERAC,SENROOTC,
     &       NINT(HNUMAD),HWUDC,
     D       SHRTD,PTF,RTDEP/100.0,(RLV(I),I=1,10)
  502       FORMAT(
     A       I5,I4,2I6,F6.1,A6,F6.2,
     B       A6,F6.2,A6,A6,F6.1,2A6,I6,A6,
     D       2F6.2,F6.3,10F6.2)
            ! End PlantGr2 writes

            ! PlantN
            IF (ISWNIT.NE.'N') THEN
              IF (YEARDOY.EQ.PLYEARDOY) THEN
                OPEN (UNIT = NOUTPN, FILE = OUTPN, STATUS='UNKNOWN',
     &          POSITION = 'APPEND')
                IF (FILEIOT(1:2).EQ.'DS') THEN
                  CALL HEADER(2, NOUTPN, RUN)
                ELSE
                  WRITE (NOUTPN,'(/,A79,/)') OUTHED
                  WRITE (NOUTPN,103) MODEL
                  WRITE (NOUTPN,1031) MODNAME
                  WRITE (NOUTPN,104)
     &             EXCODE(1:8),' ',EXCODE(9:10),'  ',ENAME(1:47)
                  WRITE (NOUTPN,102) TN,TNAME
                  WRITE (NOUTPN,107) CROP,VARNO,VRNAME
                  WRITE (NOUTPN,108)
     &             month,dom,plyeardoy,NINT(pltpopp),NINT(rowspc)
                ENDIF 
                WRITE (NOUTPN,2252)
!               2021-02-15 chp Change NUAD to NUAC in header.
 2252           FORMAT ('@YEAR DOY   DAS   DAP TMEAN  GSTD  NUAC',
     A           '  TNAD SDNAD  RNAD  CNAD  LNAD  SNAD  HNAD  HIND',
     F           ' RSNAD SNNPD SNN0D SNN1D',
     B           '  RN%D  LN%D  SN%D  HN%D SDN%D  VN%D',
     C           ' LN%RD SN%RD RN%RD  VCN%  VMN% NUPRD',
     D           ' NDEMD')
              ENDIF  ! Plantn header writes
              CALL Csopline(senn0c,sennal(0))
              CALL Csopline(sennsc,sennas)
              WRITE (NOUTPN,503)
     A         YEAR,DOY,DAS,DAP,TMEAN,GSTAGEC,NUPAC,
     B         TNAD,SDNAD,RNAD,CNAD,LLNAD,SNAD,HNAD,HINDC,
     H         RSNAD,DEADNAD,SENN0C,SENNSC,
     C         RANC*100.0,LANC*100.0,SANCOUT*100.0,
     D         AMIN1(9.9,HNC*100.0),SDNC*100.0,AMIN1(9.9,VANC*100.0),
     E         LNCR,SNCR,RNCR,
     &         VCNC*100.0,VMNC*100.0,
     F         AMIN1(2.,NUPRATIO),ANDEM
  503          FORMAT(
     1         I5,I4,2I6,F6.1,A6,F6.1,
     2         F6.1,2F6.2,4F6.1,A6,
     3         2F6.2,2A6,
     4         3F6.3,
     5         3F6.3,
     6         3F6.3,
     2         F6.1,F6.2,
     8         F6.2,F6.1)
            ENDIF  ! ISWNIT  Plantn writes

          ELSE ! (IDETG.NE.'N'.AND.IDETL.NE.'0').OR.IDETL.EQ.'A'

            IF (IDETGNUM.LE.0) THEN
              OPEN (UNIT=FNUMTMP, FILE=OUTPG, STATUS = 'UNKNOWN')
              CLOSE (UNIT=FNUMTMP, STATUS = 'DELETE')
              OPEN (UNIT=FNUMTMP, FILE=OUTPG2, STATUS = 'UNKNOWN')
              CLOSE (UNIT=FNUMTMP, STATUS = 'DELETE')
              OPEN (UNIT=FNUMTMP, FILE=OUTPGF, STATUS = 'UNKNOWN')
              CLOSE (UNIT=FNUMTMP, STATUS = 'DELETE')
              OPEN (UNIT=FNUMTMP, FILE=OUTPN, STATUS = 'UNKNOWN')
              CLOSE (UNIT=FNUMTMP, STATUS = 'DELETE')
              IDETGNUM = IDETGNUM + 1 
            ENDIF  

          ENDIF ! End ((IDETG.NE.'N'.AND.IDETL.NE.'0').OR.IDETL.EQ.'A'

        ELSEIF(YEARDOY.LT.PLYEARDOY.AND.(MOD(DAS,FROPADJ)).EQ.0.AND.
!    &   IPLTI.EQ.'A') THEN
     &   (IPLTI.EQ.'A' .OR. IPLTI.EQ.'F')) THEN
     
          ! Automatic planting
          !WRITE (fnumwrk,*) 'Yeardoy ',yeardoy
          !WRITE (fnumwrk,*) 'Water thresholds ',swpltl,swplth
          !WRITE (fnumwrk,*) 'Water ',avgsw
          !WRITE (fnumwrk,*) 'Temperature thresholds ',pttn,ptx
          !WRITE (fnumwrk,*) 'Temperature ',tsdep

        ENDIF  ! End time-course outputs (appropriate day, etc.)
               ! (MOD(DAS,FROPADJ).EQ.0.AND.YEARDOY.GE.PLYEARDOY),etc..
               

!***********************************************************************
        IF (STGYEARDOY(11).EQ.YEARDOY .OR.
     &      DYNAMIC.EQ.SEASEND) THEN         ! If harvest/failure day
!***********************************************************************

!-----------------------------------------------------------------------
!         IDETO OUTPUTS AND NECESSARY DATA INPUTS (Evaluate & Overview)
!-----------------------------------------------------------------------
          
          IF (IDETO.NE.'N'.OR.IDETL.EQ.'0') THEN

            tiernum = 0
          
            adatm = -99
            adapm = -99
            cnaam = -99
            cnamm = -99
            cnpcmm = -99
            cwaam = -99
            cwamm = -99
            deadwamm = -99.0
            edatm = -99
            edapm = -99
            gdapm = -99
            tildapm = -99
            sgedapm = -99
            aedapm = -99
            gfdapm = -99
            gdatm = -99
            gnamm = -99
            hnamm = -99
            hnpcmm = -99
            hiamm = -99
            hinmm = -99
            gnoamm = -99
            gnogmm = -99
            hnumamm = -99
            gnpcmm = -99
            hwahm = -99
            hwamm = -99
            hyamm = -99
            hwumm = -99
            laixm = -99
            lnumsmm = -99
            mdapm = -99
            mdatm = -99
            nupacm = -99
            rnamm = -99
            rswamm = -99
            rscmm = -99
            rwamm = -99
            sennatcm = -99
            senwacmm = -99
            shrtmm = -99
            tnumamm = -99
            tnumpmm = -99
            psdatm = -99
            ssdatm = -99
            vnamm = -99
            vnpcmm = -99
            vwamm = -99
            laixt = -99.0
            valuer = -99.0
            ! Variables from time-course file
            LAIXT = -99.0 
            TNUMT = -99.0 
            LNUMT = -99.0 
            CWADT = -99.0 
            HWADT = -99.0 
            HIADT = -99.0 
            HWUT = -99.0 
            HNUMAT = -99.0 
            HNUMET = -99.0 
            ADATT = -99 
            MDATT = -99 

!            WRITE(Fnumwrk,*)' '
!            WRITE(Fnumwrk,*)' '
!            WRITE(Fnumwrk,'(A45)')
!     &        ' FINISHED SIMULATION. PREPARING FINAL OUTPUTS'
                      
            ! Reading A-file
            CALL LTRIM2 (FILEIO,filenew)
            FILELEN = TVILENT(FILENEW)
            FILELEN = MAX(FILELEN-12, 0) 

            IF (TVILENT(FILEADIR).GT.3) THEN
              IF (FILEADIR(TVILENT(FILEADIR):TVILENT(FILEADIR)).NE.
     &         SLASH) THEN
                FILEA = FILEADIR(1:TVILENT(FILEADIR))//
     &           SLASH//EXCODE(1:8)//'.'//EXCODE(9:10)//'A'
              ELSE
                FILEA = FILEADIR(1:TVILENT(FILEADIR))//
     &           EXCODE(1:8)//'.'//EXCODE(9:10)//'A'
              ENDIF
            ELSE
              FILEA = FILENEW(1:FILELEN-12)//EXCODE(1:8)//'.'//
     &        EXCODE(9:10)//'A'
            ENDIF       
            FEXISTA = .FALSE.
            INQUIRE (FILE = FILEA,EXIST = FEXISTA)
            IF (.not.FEXISTA) THEN
              WRITE (Message(1),'(A23,A50)')
     &         'Could not find A-file: ',filea(1:50)
              WRITE (Message(2),'(A23,A50)')
     &         'Experiment file:       ',fileio(1:50)
              CALL WARNING(2,'CSCRP',MESSAGE)
!              WRITE(Fnumwrk,*)' '
!              WRITE (Fnumwrk,'(A24,A50)')
!     &         ' Could not find A-file: ',filea(1:50)
!              WRITE (Fnumwrk,'(A24,A50)')
!     &         ' Experiment file:       ',fileio(1:50)
              OPEN (UNIT=FNUMTMP, FILE=FILEA, STATUS = 'UNKNOWN')
              CLOSE (UNIT=FNUMTMP, STATUS = 'DELETE')
            ELSE
!              WRITE(Fnumwrk,*)' '
!              WRITE (Fnumwrk,'(A15,A50)')
!     &         ' Found A-file: ',filea(1:50)
              ! Yield at maturity  
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'HWAM',hwamm)
              IF (hwamm.GT.0.0.AND.HWAMM.LT.50.0) HWAMM = HWAMM*1000.0
              IF (hwamm.LE.0.0)THEN
                CALL AREADR (FILEA,TN,RN,SN,ON,CN,'GWAM',gwamm)
                IF (GWAMM.GT.0.0) THEN
                  IF (gwamm.GT.0.0.AND.GWAMM.LT.50.0) GWAMM=GWAMM*1000.0
                  HWAMM = GWAMM
                ENDIF  
              ENDIF  
              IF (HWAMM.LE.0.0) THEN
                CALL AREADR (FILEA,TN,RN,SN,ON,CN,'HYAM',hyamm)
                IF (hyamm.LE.0.0) THEN
                  CALL AREADR (FILEA,TN,RN,SN,ON,CN,'GYAM',hyamm)
                ENDIF  
                IF (hyamm.GT.0.0.AND.HYAMM.LT.50.0) HYAMM = HYAMM*1000.0
              ENDIF
              
              ! Yield at harvest
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'GWAH',gwahm)
             
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'HWUM',hwumm)
              IF (hwumm.le.0.0)
     &         CALL AREADR (FILEA,TN,RN,SN,ON,CN,'GWUM',gwumm)
          
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'LAIX',laixm)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'CWAM',cwamm)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'BWAH',vwamm)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'CWAA',cwaam)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'T#AM',tnumamm)
              IF (tnumamm.le.0.0)
     &         CALL AREADR (FILEA,TN,RN,SN,ON,CN,'TNOAM',tnumamm)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'H#AM',hnumamm)
              IF (hnumamm.le.0.0)
     &         CALL AREADR (FILEA,TN,RN,SN,ON,CN,'HNOAM',hnumamm)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'H#SM',hnumgmm)
              IF (hnumgmm.le.0.0)
     &         CALL AREADR (FILEA,TN,RN,SN,ON,CN,'HNOSM',hnumgmm)
              IF (hnumgmm.le.0.0)
     &         CALL AREADR (FILEA,TN,RN,SN,ON,CN,'H#UM',hnumgmm)
              IF (hnumgmm.le.0.0)
     &         CALL AREADR (FILEA,TN,RN,SN,ON,CN,'HNOUM',hnumgmm)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'L#SM',lnumsmm)
              IF (lnumsmm.le.0.0)
     &         CALL AREADR (FILEA,TN,RN,SN,ON,CN,'LNOSM',lnumsmm)
          
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'CNAM',cnamm)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'VNAM',vnamm)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'CNAA',cnaam)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'HNAM',hnamm)
              IF (hnamm.LE.0.0)
     &         CALL AREADR (FILEA,TN,RN,SN,ON,CN,'GNAM',gnamm)
              IF (HNAMM.LE.0.0) HNAMM = GNAMM
              IF (HNAMM.LE.0.0) HNAMM = -99   
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'CN%M',cnpcmm)
              IF (cnpcmm.le.0.0)
     &         CALL AREADR (FILEA,TN,RN,SN,ON,CN,'CNPCM',cnpcmm)
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'HN%M',hnpcmm)
              IF (hnpcmm.le.0.0)
     &         CALL AREADR (FILEA,TN,RN,SN,ON,CN,'HNPCM',hnpcmm)
              IF (hnpcmm.le.0.0) THEN
                CALL AREADR (FILEA,TN,RN,SN,ON,CN,'GN%M',gnpcmm)
                IF (gnpcmm.le.0.0)
     &           CALL AREADR (FILEA,TN,RN,SN,ON,CN,'GNPCM',gnpcmm)
              ENDIF
              IF (HNPCMM.LE.0.0) HNPCMM = GNPCMM
              IF (HNPCMM.LE.0.0) HNPCMM = -99   
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'VN%M',vnpcmm)
              IF (vnpcmm.le.0.0)
     &         CALL AREADR (FILEA,TN,RN,SN,ON,CN,'VNPCM',vnpcmm)
          
              CALL AREADR (FILEA,TN,RN,SN,ON,CN,'HIAM',hiamm)
              IF (HIAMM.GE.1.0) HIAMM = HIAMM/100.0
          
              CALL AREADI (FILEA,TN,RN,SN,ON,CN,'EDAT',edatm)
              CALL AREADI (FILEA,TN,RN,SN,ON,CN,'GDAT',gdatm)
          
              IF (HWUMM.LE.0.0) HWUMM = GWUMM
              IF (HNAMM.LE.0.0) HNAMM = GNAMM
          
              DO L = 1,PSNUM              
               CALL AREADI (FILEA,TN,RN,SN,ON,CN,psabv(l),psdatm(l))
               CALL LTRIM(PSABV(L)) 
               IF (PSABV(L).EQ.'TSAT')
     &           CALL AREADI (FILEA,TN,RN,SN,ON,CN,'TSDAT',psdatm(l))
               IF (PSDATM(L).GT.0.0.AND.PSDATM(L).LT.1000) THEN
                 CALL AREADI (FILEA,TN,RN,SN,ON,CN,'YEAR',yearm)
                 IF (YEARM.GT.0.0) PSDATM = CSYDOY(YEARM,PSDATM(L))
               ENDIF
               IF (psdatm(l).gt.0) then
                psdapm(l) = Dapcalc(psdatm(l),plyear,plday)
               ELSE
                psdapm(l) = -99
               ENDIF 
              ENDDO
              DO L = 1,SSNUM
               CALL AREADI (FILEA,TN,RN,SN,ON,CN,ssabv(l),ssdatm(l))
               CALL LTRIM(SSABV(L)) 
               IF (SSABV(L).EQ.'DRAT')
     &           CALL AREADI (FILEA,TN,RN,SN,ON,CN,'DRDAT',ssdatm(l))
               IF (SSDATM(L).GT.0.0.AND.SSDATM(L).LT.1000) THEN
                 CALL AREADI (FILEA,TN,RN,SN,ON,CN,'YEAR',yearm)
                 IF (YEARM.GT.0.0) SSDATM = CSYDOY(YEARM,SSDATM(L))
               ENDIF
               ssdapm(l) = Dapcalc(ssdatm(l),plyear,plday)
              ENDDO
            ENDIF ! File-A exists
          
            ! Reading T-file to complement A-data and writing MEASURED
            IF (IDETG.NE.'N'.OR.IDETL.EQ.'A') THEN 
              STARNUMO = STARNUMO + 1 ! Number of datasets in Simop file
              IF (TVILENT(FILEADIR).GT.3) THEN
                IF (FILEADIR(TVILENT(FILEADIR):TVILENT(FILEADIR)).NE.
     &           SLASH) THEN
                  FILET = FILEADIR(1:TVILENT(FILEADIR))//
     &             SLASH//EXCODE(1:8)//'.'//EXCODE(9:10)//'T'
                ELSE
                  FILET = FILEADIR(1:TVILENT(FILEADIR))//
     &             EXCODE(1:8)//'.'//EXCODE(9:10)//'T'
                ENDIF
              ELSE
                FILET = FILENEW(1:FILELEN-12)//EXCODE(1:8)//'.'//
     &          EXCODE(9:10)//'T'
              ENDIF       
              FEXISTT  = .FALSE.
              INQUIRE (FILE = FILET,EXIST = FEXISTT)
              IF (.not.FEXISTT) THEN
                WRITE (Message(1),'(A23,A50)')
     &          'Could not find T-file: ',filet(1:50)
                CALL WARNING(1,'CSCRP',MESSAGE)
!                WRITE(Fnumwrk,*)' '
!                WRITE (Fnumwrk,'(A24,A50)')
!     &          ' Could not find T-file: ',filet(1:50)
              ELSE
!                WRITE(Fnumwrk,*)' '
!                WRITE (Fnumwrk,'(A15,A50)')
!     &          ' Found T-file: ',filet(1:50)
                TLINENUM = 0
                OPEN (UNIT=FNUMT,FILE=FILET)
                OPEN (UNIT=FNUMMEAS,FILE=FNAMEMEAS,POSITION='APPEND')
                COLNUM = 1
                L1 = 0
                DO
                  READ(FNUMT,'(A180)',END = 5555)LINET
                  TLINENUM = TLINENUM + 1  ! Used to check if file empty
                  L1 = 0
                  L2 = 0
                  ! First IF to jump over comments and blanks
                  IF (LEN(LINET).GT.0.AND.LINET(1:1).NE.'!') THEN
                    IF (LINET(1:7).EQ.'*DATA(T' .OR.
     &               LINET(1:7).EQ.'*EXP.DA' .OR.
     &               LINET(1:7).EQ.'*EXP. D' .OR.
     &               LINET(1:7).EQ.'*TIME_C' .OR.
     &               LINET(1:7).EQ.'$EXPERI') THEN
                      TNCHAR = TL10FROMI(TN)
                      LENLINE = TVILENT(LINET)
                      IF(LINET(1:7).EQ.'*EXP.DA'.OR.
     &                   LINET(1:7).EQ.'*EXP. D'.OR.
     &                   LINET(1:7).EQ.'$EXPERI')THEN
                        GROUP = 'A'
                        DO L = 1,30
                          IF (LINET(L:L+1).EQ.': ') L1 = L+2
                          IF (LINET(L:L).EQ.':'.AND.
     &                        LINET(L+1:L+1).NE.' ')
     &                      L1 = L+1
                          IF (L1.GT.0.AND.L.GT.L1+9.AND.
     &                                    LINET(L:L).NE.' ') THEN
                            L2 = L ! Start of group information in tfile
                            EXIT
                          ENDIF
                        ENDDO
                        LENTNAME = MIN(15,TVILENT(TNAME))
                        LENGROUP = MIN(L2+14,LENLINE)
                        IF (TVILENT(TNCHAR).EQ.1) THEN
                          LINESTAR = LINET(L1:L1+9)//' '//
     &                    TNCHAR(1:1)//' '//TNAME(1:LENTNAME)
                        ELSEIF (TVILENT(TNCHAR).EQ.2) THEN
                          LINESTAR = LINET(L1:L1+9)//' '//
     &                     TNCHAR(1:2)//' '//TNAME(1:LENTNAME)
                        ELSEIF (TVILENT(TNCHAR).EQ.3) THEN
                          LINESTAR = LINET(L1:L1+9)//' '//
     &                     TNCHAR(1:3)//' '//TNAME(1:LENTNAME)
                        ENDIF
                        LENLINESTAR = TVILENT(LINESTAR)
                      ENDIF
                    ELSEIF (LINET(1:1).EQ.'@') THEN
                      DO L = 1,TVILENT(LINET)
                        IF (LINET(L:L+2).EQ.' GW') LINET(L:L+2) = ' HW'
                      END DO
                      DATECOL = Tvicolnm(linet,'DATE')
                      YEARCOL = Tvicolnm(linet,'YEAR')
                      DOYCOL = Tvicolnm(linet,'DOY')
                      IF (DOYCOL.LE.0) DOYCOL = Tvicolnm(linet,'DAY')
                      RPCOL = Tvicolnm(linet,'RP')
                      LAIDCOL = Tvicolnm(linet,'LAID')
                      TNUMCOL = Tvicolnm(linet,'T#AD')
                      LNUMCOL = Tvicolnm(linet,'L#SD')
                      CWADCOL = Tvicolnm(linet,'CWAD')
                      HWADCOL = Tvicolnm(linet,'HWAD')
                      HIADCOL = Tvicolnm(linet,'HIAD')
                      !HWTUCOL = Tvicolnm(linet,'HWNOD')
                      HWTUCOL = Tvicolnm(linet,'HWUD')
                      HNUMACOL = Tvicolnm(linet,'H#AD')
                      HNUMECOL = Tvicolnm(linet,'H#ED')
                      GSTDCOL = Tvicolnm(linet,'GSTD')
                      LENLINE = TVILENT(LINET)
                      LINET(LENLINE+1:LENLINE+20)='   DAP   DAS YEARDOY'
                      LINET(1:1) = '@'
                      TIERNUM = TIERNUM + 1
                      IF (TIERNUM.LT.10) THEN
                        WRITE(TIERNUMC,'(I1)') TIERNUM
                      ELSE
                        WRITE(TIERNUMC,'(I2)') TIERNUM
                      ENDIF
                      LINESTAR2 = '*TIER('//TIERNUMC//'):'//
     &                 LINESTAR(1:LENLINESTAR)//LINET(14:LENLINE)
                      IF (IDETG.NE.'N') THEN 
                        WRITE (FNUMMEAS,*) ' '
                        WRITE (FNUMMEAS,'(A80)') LINESTAR2(1:80)
                        WRITE (FNUMMEAS,*) ' '
                        WRITE (FNUMMEAS,'(A180)') LINET(1:180)
                      ENDIF  
                      STARNUMM = STARNUMM + 1              ! # datasets
                    ELSE
                      CALL Getstri (LINET,COLNUM,VALUEI)
                      IF (VALUEI.EQ.TN) THEN
                        IF (DATECOL.GT.0.OR.DOYCOL.GT.0) THEN
                          IF (DATECOL.GT.0) THEN
                            CALL Getstri (LINET,DATECOL,DATE)
                            ! July 2013 ... to get full yeardoy
                            IF (DATE.GT.40001) THEN   ! After 1940
                              DATE = 1900000 + DATE 
                            ELSE  
                              DATE = 2000000 + DATE 
                            ENDIF
                          ELSEIF (DATECOL.LE.0) THEN
                            CALL Getstri (LINET,DOYCOL,DOY)
                            CALL Getstri (LINET,YEARCOL,YEAR)
                            IF (YEAR.GT.2000) YEAR = YEAR-2000
                            IF (YEAR.GT.1900) YEAR = YEAR-1900
                            DATE = YEAR*1000+DOY
                          ENDIF
                          DAP = MAX(0,CSTIMDIF(PLYEARDOY,DATE))
                          DAS = MAX(0,CSTIMDIF(YEARSIM,DATE))
                          DAPCHAR = TL10FROMI(DAP)
                          IF (TVILENT(DAPCHAR).EQ.1) THEN
                            DAPWRITE = '     '//DAPCHAR(1:1)
                          ELSEIF (TVILENT(DAPCHAR).EQ.2) THEN
                            DAPWRITE = '    '//DAPCHAR(1:2)
                          ELSEIF (TVILENT(DAPCHAR).EQ.3) THEN
                            DAPWRITE = '   '//DAPCHAR(1:3)
                          ENDIF
                          LENLINE = TVILENT(LINET)
                          LINET(LENLINE+1:LENLINE+6) = DAPWRITE(1:6)
                          DAPCHAR = TL10FROMI(DAS)
                          IF (TVILENT(DAPCHAR).EQ.1) THEN
                            DAPWRITE = '     '//DAPCHAR(1:1)
                          ELSEIF (TVILENT(DAPCHAR).EQ.2) THEN
                            DAPWRITE = '    '//DAPCHAR(1:2)
                          ELSEIF (TVILENT(DAPCHAR).EQ.3) THEN
                            DAPWRITE = '   '//DAPCHAR(1:3)
                          ENDIF
                          LENLINE = TVILENT(LINET)
                          LINET(LENLINE+1:LENLINE+6) = DAPWRITE(1:6)
                          DAPCHAR = TL10FROMI(DATE)
                          IF (TVILENT(DAPCHAR).EQ.7) THEN
                            DATEWRITE = ' '//DAPCHAR(1:7)
                          ENDIF
                          LENLINE = TVILENT(LINET)
                          LINET(LENLINE+1:LENLINE+8) = DATEWRITE(1:8)
                        ENDIF
                        CALL Getstri (LINET,RPCOL,VALUEI)
                        IF (IDETG.NE.'N') THEN 
                          IF (VALUEI.LE.0) 
     &                       WRITE (FNUMMEAS,'(A180)') LINET
                        ENDIF  
                      
                        ! T-FILE STUFF FOR OUTPUT OF INDIVIDUAL VARS
                        ! Below is to pick up variables for output files
                        IF (IDETL.EQ.'A') THEN
                         IF (GROUP.EQ.'A') THEN
                          !WRITE(fnumwrk,*)' Picking vars from t-file'
                          CALL Getstrr (LINET,LAIDCOL,VALUER)
                          IF (VALUER.GT.LAIXT) LAIXT = VALUER
                          CALL Getstrr (LINET,TNUMCOL,VALUER)
                          IF (VALUER.GT.0.0) TNUMT = VALUER
                          CALL Getstrr (LINET,LNUMCOL,VALUER)
                          IF (VALUER.GT.LNUMT) LNUMT = VALUER
                          CALL Getstrr (LINET,CWADCOL,VALUER)
                          IF (VALUER.GT.0.0) CWADT = VALUER
                          CALL Getstrr (LINET,HWADCOL,VALUER)
                          IF (VALUER.GT.0.0) HWADT = VALUER
                          CALL Getstrr (LINET,HIADCOL,VALUER)
                          IF (VALUER.GT.0.0) HIADT = VALUER
                          IF (HIADT.GE.1.0) HIADT = HIADT/100.0
                          CALL Getstrr (LINET,HWTUCOL,VALUER)
                          IF (VALUER.GT.0.0) HWUT = VALUER
                          CALL Getstrr (LINET,HNUMACOL,VALUER)
                          IF (VALUER.GT.0.0) HNUMAT = VALUER
                          CALL Getstrr (LINET,HNUMECOL,VALUER)
                          IF (VALUER.GT.0.0) HNUMET = VALUER
                          CALL Getstrr (LINET,GSTDCOL,VALUER)
                          IF (VALUER.GT.FLOAT(ASTG*10).AND.ADATT.LE.0)
     &                      ADATT = DATE
                          IF (VALUER.GT.FLOAT(MSTG*10).AND.MDATT.LE.0)
     &                      MDATT = DATE
                          ! To indicate that t data present
                          tdatanum = 1
                         ENDIF ! End picking variables from t for a     
                        ENDIF ! End of details flag 
                      ENDIF ! End correct treatment 
                    ENDIF ! End particular data lines
                  ENDIF ! End valid (ie.non comment) line
                ENDDO
 5555           CONTINUE
                ! If T-file was empty
                IF (TLINENUM.LT.4) THEN
                  tdatanum = 0
                  WRITE (Message(1),'(A23,A50)')
     &             'T-file was empty '
                  CALL WARNING(1,'CSCRP',MESSAGE)
!                  WRITE(Fnumwrk,*)' '
!                  WRITE (Fnumwrk,'(A24,A50)')
!     &             ' T-file was empty '
                ENDIF
                CLOSE(FNUMT)
                CLOSE(FNUMMEAS)
              ENDIF ! End t-file reads,measured.out writes
              
              IF (IDETL.EQ.'A') THEN
                ! Use T-data if A-data missing (whem output=all)
                IF (FEXISTT) THEN
                  IF (HWAMM.LE.0.0) THEN
                    IF (HWADT.GT.0.0) THEN
                      HWAMM = HWADT
                      WRITE(Message(1),'(A30)')
     &                 'Time-course data used for HWAM'
                      CALL WARNING(1,'CSCRP',MESSAGE)
!                      WRITE(Fnumwrk,*)' '
!                      WRITE(Fnumwrk,'(A32)')
!     &                 '  Time-course data used for HWAM'
                    ENDIF
                  ELSE
                    IF (HWADT.GT.0.0) THEN
                      IF (ABS(100.0*ABS(HWAMM-HWADT)/HWAMM).GT.0.0) THEN
                      WRITE(Message(1),'(A48,F8.2)')
     &                'Pc difference between final,time-course yields ='
     &                ,100.0*ABS(HWAMM-HWADT)/HWAMM
                      WRITE(Message(2),'(A20,I6)')
     &                'Final yield         ',NINT(HWAMM)
                      WRITE(Message(3),'(A20,I6)')
     &                'Time-course yield   ',NINT(HWADT)
                      CALL WARNING(3,'CSCRP',MESSAGE)
!                      WRITE(Fnumwrk,*)' '
!                      WRITE(Fnumwrk,'(A48,F8.2)')
!     &                ' % difference between final,time-course yields ='
!     &                ,100.0*ABS(HWAMM-HWADT)/HWAMM
!                      WRITE(Fnumwrk,'(A21,I6)')
!     &                ' Final yield         ',NINT(HWAMM)
!                      WRITE(Fnumwrk,'(A21,I6)')
!     &                ' Time-course yield   ',NINT(HWADT)
                      ENDIF
                    ENDIF
                  ENDIF
                  IF (CWAMM.LE.0.0) THEN
                    IF (CWADT.GT.0.0) THEN
                      CWAMM = CWADT
                      WRITE(Message(1),'(A31)')
     &                 'Time-course data used for CWAMM'
                      CALL WARNING(1,'CSCRP',MESSAGE)
!                      WRITE(Fnumwrk,*)' '
!                      WRITE(Fnumwrk,'(A33)')
!     &                 '  Time-course data used for CWAMM'
                    ENDIF
                  ELSE
                    IF (CWADT.GT.0.0) THEN
                     IF (ABS(100.0*ABS(CWAMM-CWADT)/CWAMM).GT.0.0) THEN
                      WRITE(Message(1),'(A48,F8.2)')
     &                'Pc difference between final,time-course canopy ='
     &                ,100.0*ABS(CWAMM-CWADT)/CWAMM
                      WRITE(Message(2),'(A19,I6)')
     &                'Final canopy       ',NINT(CWAMM)
                      WRITE(Message(3),'(A19,I6)')
     &                'Time-course canopy ',NINT(CWADT)
                      CALL WARNING(3,'CSCRP',MESSAGE)
!                      WRITE(Fnumwrk,*)' '
!                      WRITE(Fnumwrk,'(A49,F8.2)')
!     &                ' % difference between final,time-course canopy ='
!     &                ,100.0*ABS(CWAMM-CWADT)/CWAMM
!                      WRITE(Fnumwrk,'(A20,I6)')
!     &                ' Final canopy       ',NINT(CWAMM)
!                      WRITE(Fnumwrk,'(A20,I6)')
!     &                ' Time-course canopy ',NINT(CWADT)
                     ENDIF
                    ENDIF
                  ENDIF
                  IF (LAIXM.LE.0.0.AND.LAIXT.GT.0.0) THEN
                    LAIXM = LAIXT
                    WRITE(Message(1),'(A31)')
     &               'Time-course data used for LAIXM'
                    CALL WARNING(1,'CSCRP',MESSAGE)
!                    WRITE(Fnumwrk,*)' '
!                    WRITE(Fnumwrk,'(A32)')
!     &               ' Time-course data used for LAIXM'
                  ENDIF
                  IF (LNUMSMM.LE.0.0.AND.LNUMSMM.GT.0.0) THEN
                    LNUMSMM = LNUMT
                    WRITE(Message(1),'(A33)')
     &               'Time-course data used for LNUMSMM'
                    CALL WARNING(1,'CSCRP',MESSAGE)
!                    WRITE(Fnumwrk,*)' '
!                    WRITE(Fnumwrk,'(A34)')
!     &               ' Time-course data used for LNUMSMM'
                  ENDIF
                  IF (TNUMAMM.LE.0.0.AND.TNUMT.GT.0.0) THEN
                    TNUMAMM = TNUMT
                    WRITE(Message(1),'(A33)')
     &               'Time-course data used for TNUMAMM'
                    CALL WARNING(1,'CSCRP',MESSAGE)
!                    WRITE(Fnumwrk,*)' '
!                    WRITE(Fnumwrk,'(A34)')
!     &               ' Time-course data used for TNUMAMM'
                  ENDIF
                  IF (HIAMM.LE.0.0.AND.HIADT.GT.0.0) THEN
                    HIAMM = HIADT
                    WRITE(Message(1),'(A31)')
     &               'Time-course data used for HIAMM'
                    CALL WARNING(1,'CSCRP',MESSAGE)
!                    WRITE(Fnumwrk,*)' '
!                    WRITE(Fnumwrk,'(A32)')
!     &               ' Time-course data used for HIAMM'
                  ENDIF
                  IF (HWUMM.LE.0.0.AND.HWUT.GT.0.0) THEN
                    HWUMM = HWUT
                    WRITE(Message(1),'(A31)')
     &               'Time-course data used for HWUMM'
                    CALL WARNING(1,'CSCRP',MESSAGE)
!                    WRITE(Fnumwrk,*)' '
!                    WRITE(Fnumwrk,'(A32)')
!     &               ' Time-course data used for HWUMM'
                  ENDIF
                  IF (HNUMAMM.LE.0.0.AND.HNUMAT.GT.0.0) THEN
                    HNUMAMM = HNUMAT
                    WRITE(Message(1),'(A31)')
     &               'Time-course data used for H#AT'
                    CALL WARNING(1,'CSCRP',MESSAGE)
!                    WRITE(Fnumwrk,*)' '
!                    WRITE(Fnumwrk,'(A32)')
!     &               ' Time-course data used for H#AT'
                  ENDIF
                  IF (HNUMGMM.LE.0.0.AND.HNUMET.GT.0.0) THEN
                    HNUMGMM = HNUMET
                    WRITE(Message(1),'(A32)')
     &               'Time-course data used for H#GMM'
                    CALL WARNING(1,'CSCRP',MESSAGE)
!                    WRITE(Fnumwrk,*)' '
!                    WRITE(Fnumwrk,'(A33)')
!     &               ' Time-course data used for H#GMM'
                  ENDIF
                ENDIF
                DO L = 1,PSNUM
                  IF (PSABV(L).EQ.'ADAT'.AND.PSDATM(L).LE.0.0) THEN
                    IF (ADATT.GT.0) THEN
                      PSDATM(L) = INT(ADATT)
                      WRITE(Message(1),'(A31)')
     &                 'Time-course data used for ADATM'
                      CALL WARNING(1,'CSCRP',MESSAGE)
!                      WRITE(Fnumwrk,*)' '
!                      WRITE(Fnumwrk,'(A32)')
!     &                 ' Time-course data used for ADATM'
                    ENDIF  
                  ENDIF
                  IF (PSABV(L).EQ.'MDAT'.AND.PSDATM(L).LE.0.0) THEN
                    IF (MDATT.GT.0) THEN
                      PSDATM(L) = INT(MDATT)
                      WRITE(Message(1),'(A31)')
     &                 'Time-course data used for MDATM'
                      CALL WARNING(1,'CSCRP',MESSAGE)
!                      WRITE(Fnumwrk,*)' '
!                      WRITE(Fnumwrk,'(A32)')
!     &                 ' Time-course data used for MDATM'
                    ENDIF  
                  ENDIF
                ENDDO
              ENDIF ! END OF USE T-DATA TO FILL IN FOR MISSING A-DATA
              
            ELSE  ! For IDETG.NE.'N'.OR.IDETL.EQ.'A' 
              ! No call for measured.out! Delete old files.
              OPEN (UNIT=FNUMTMP,FILE=FNAMEMEAS,STATUS = 'UNKNOWN')
              CLOSE (UNIT=FNUMTMP, STATUS = 'DELETE')
                
            ENDIF ! End A-file reads,T-file reads,Measured writes,T->A
                 
            ! Check data and calculate equivalents,if needed
            
            ! Emergence and maturity dates 
            IF (edatm.LE.0) edatm = edatmx ! If no Afile data,use Xfile
            IF (mdatm.LE.0) mdatm = psdatm(mstg)
            
            ! Product wt at maturity
            IF (hwahm.GT.0.AND.hwamm.LE.0) hwamm = hwahm/(hpcf/100.0)
            
            ! Product wt at harvest
            IF (hwamm.GT.0.AND.hwahm.LE.0) hwahm = hwamm*(hpcf/100.0)
            
            ! Canopy wt at maturity
            IF (vwamm.GT.0.AND.hwamm.GT.0) cwamm = vwamm+hwamm
            
            ! Vegetative wt at maturity
            IF (hwamm.GT.0.AND.cwamm.GT.0) vwamm = cwamm-hwamm
            
            ! Harvest index at maturity
            IF (hiamm.LE.0.0) THEN
              IF (cwamm.GT.0.AND.hwamm.GT.0) THEN
                hiamm = hwamm/cwamm
              ENDIF  
            ELSE
              IF (cwamm.GT.0.AND.hwamm.GT.0) THEN
                hiammtmp = hwamm/cwamm
                IF (hiammtmp/hiam.GT.1.1 .OR. hiammtmp/hiam.LT.0.9) THEN
!                  IF (ABS(hiammtmp-hiamm)/hiamm.GT.0.05) THEN
!                    WRITE (fnumwrk,*) 'Reported HI not consistent',
!     &               ' with yield and total weight data  '
!                    WRITE (fnumwrk,*) ' Reported HI   ',hiamm
!                    WRITE (fnumwrk,*) ' Calculated HI ',hiammtmp
!                    WRITE (fnumwrk,*) ' Will use reported value '
!                  ENDIF
                ENDIF
              ENDIF
            ENDIF
            
            ! Product unit wt at maturity
            IF (hwumm.GT.1.0) hwumm = hwumm/1000.0 ! mg->g
            IF (hwumm.LE.0.AND.hnumamm.GT.0) THEN
              IF (hwamm.GT.0.0) hwumm=hwamm*0.1/hnumamm  ! kg->g
            ELSE
              IF (hwamm.gt.0.0.AND.hnumamm.GT.0.0) THEN
                hwumyld = hwamm*0.1/hnumamm
!                IF (ABS(hwumyld-hwumm)/hwumm.GT.0.05) THEN
!                  WRITE (fnumwrk,*)' '
!                  WRITE (fnumwrk,'(A14)')' MEASURED DATA'
!                  WRITE (fnumwrk,'(A36,A33)')
!     &              ' Reported product wt.not consistent',
!     &              ' with yield and product # data   '
!                  WRITE (fnumwrk,*) ' Reported wt   ',hwumm
!                  WRITE (fnumwrk,*) ' Calculated wt ',hwumyld
!                  WRITE (fnumwrk,*) '   Yield       ',hwamm
!                  WRITE (fnumwrk,*) '   Kernel no   ',hnumamm
!                  WRITE (fnumwrk,*) ' Will use reported value '
!                ENDIF
              ENDIF
            ENDIF
            
            ! Product number at maturity
            IF (HNUMAMM.LE..0.AND.HNUMGMM.GT..0.AND.TNUMAMM.GT..0) THEN
              HNUMAMM = HNUMGMM * TNUMAMM
              WRITE(Message(1),'(A39)')
     &         'ShootNo * product/shoot used for H#AMM'
              CALL WARNING(1,'CSCRP',MESSAGE)
!              WRITE(Fnumwrk,*)' '
!              WRITE(Fnumwrk,'(A40)')
!     &         ' ShootNo * product/shoot used for H#AMM'
            ENDIF
            IF (HNUMAMM.GT.0.0) THEN
              IF (PLTPOPP.GT.0) THEN 
                HNUMPMM = HNUMAMM/PLTPOPP
              ELSE
                HNUMPMM = -99.0
              ENDIF  
            ELSE
              HNUMPMM = -99.0
            ENDIF
            IF (HNUMGMM.LE.0.AND.TNUMAMM.GT.0.AND.HNUMAMM.GT.0) THEN
              HNUMGMM = HNUMAMM/TNUMAMM
              WRITE(Message(1),'(A38)')
     &         'ProductNo/area/ShootNo used for H#GMM '
              CALL WARNING(1,'CSCRP',MESSAGE)
!              WRITE(Fnumwrk,*)' '
!              WRITE(Fnumwrk,'(A39)')
!     &         ' ProductNo/area/ShootNo used for H#GMM '
            ENDIF
            
            ! Tiller number at maturity
            IF (tnumamm.LE.0.AND.hnumamm.GT.0.AND.hnumgmm.GT.0)
     &         tnumamm = hnumamm/hnumgmm
            IF (pltpopp.GT.0..AND.tnumamm.GT.0.) tnumpmm=tnumamm/pltpopp
            
            ! Shoot/root ratio at maturity
            IF (rwamm.GT.0.0) shrtmm = cwamm/rwamm
            
            ! Reserves concentration at maturity
            IF (vwamm+rwamm.GT.0.AND.rswamm.GT.0.0)
     &       rscmm = rswamm/(vwamm+rwamm)
            
            ! Canopy N at maturity
            IF (vnamm.GT.0.AND.gnamm.GT.0.AND.cnamm.LE.0)
     &        cnamm = vnamm + gnamm
            
            ! Total N at maturity
            IF (CNAMM.GT.0.0.AND.RNAMM.GT.0.0) THEN
              tnamm = cnamm+rnamm
            ELSE
              tnamm = -99.0
            ENDIF
            
            ! Vegetative N at maturity
            IF (vnamm.LE.0) THEN
             IF (hnamm.GE.0.AND.cnamm.GT.0) vnamm=cnamm-hnamm
            ENDIF
            
            ! Product N harvest index at maturity
            IF (cnamm.GT.0.AND.hnamm.GT.0) hinmm=hnamm/cnamm
            
            ! Vegetative N concentration at maturity
            IF (vnpcmm.LE.0) THEN
             IF (vwamm.GT.0.AND.vnamm.GT.0) vnpcmm = (vnamm/vwamm)*100
            ENDIF
            
            ! Product N concentration at maturity
            IF (hnpcmm.LE.0) THEN
             IF (hwamm.GT.0.AND.hnamm.GT.0) hnpcmm = (hnamm/hwamm)*100
            ENDIF
            
            ! Leaf N concentration at maturity
            IF (cnpcmm.LE.0.AND.cnamm.GT.0.AND.cwamm.GT.0.0)
     &        cnpcmm = cnamm/cwamm
            
            ! Express dates as days after planting
            edapm = -99
            edapm = Dapcalc(edatm,plyear,plday)
            IF (edapm.GT.200) THEN
              WRITE (Message(1),'(A31,A31,A11)')
     &         'Measured emergence over 200DAP ',
     &         'Maybe reported before planting.',
     &         'Check files'
              CALL WARNING(1,'CSCRP',MESSAGE)
!              WRITE(Fnumwrk,*)' '
!              WRITE (Fnumwrk,'(A32,A31,A11)')
!     &         ' Measured emergence over 200DAP ',
!     &         'Maybe reported before planting.',
!     &         'Check files'
            ENDIF
            gdapm = Dapcalc(gdatm,plyear,plday)
            adapm = Dapcalc(adatm,plyear,plday)
            IF (mdapm.LE.0) mdapm = Dapcalc(mdatm,plyear,plday)

            ! Check that -99 not multiplied or divided 
            IF (hnumgm.LT.0.0) hnumgm = -99
            IF (hnumam.LT.0.0) hnumam = -99
            IF (hnumgmm.LT.0.0) hnumgmm = -99
            IF (hnumamm.LT.0.0) hnumamm = -99
            
            ! Put N variables to -99 if N switched off
            IF (ISWNIT.EQ.'N') THEN
              hnpcm = -99
              vnpcm = -99
              cnam = -99
              hnam = -99
!             hinm = -99
              sdnap = -99
              rnam = -99
              nupac = -99
            ENDIF  

            ! Calculate N% without dead matter        
            IF((LFWT+STWT+RSWT).GT.0.0) THEN       
              VNPCM = 100.0*(LEAFN+STEMN+RSN)/(LFWT+STWT+RSWT)
            ELSE
              VNPCM = 0.0
            ENDIF
C-GH 1/20/2022 For ISWNI set to N
            IF (ISWNIT.EQ.'N') THEN
               hinm = -99
            ELSE
              IF(GRAINN+LEAFN+STEMN+RSN .GT. 0.0) THEN
                HINM = GRAINN/(GRAINN+LEAFN+STEMN+RSN)
              ELSE
                HINM = 0.0
              ENDIF 
            ENDIF       
           
            ! Create character equivalents for outputing
            CALL Csopline(hwumchar,hwum)
            CALL Csopline(hwummchar,AMAX1(-99.0,hwumm))
            CALL Csopline(hiamchar,AMAX1(-99.0,hiam))
            CALL Csopline(hiammchar,AMAX1(-99.0,hiamm))
            CALL Csopline(hinmchar,AMAX1(-99.0,hinm))
            CALL Csopline(hinmmchar,AMAX1(-99.0,hinmm))
            CALL Csopline(hnpcmchar,AMAX1(-99.0,hnpcm))
            CALL Csopline(hnpcmmchar,AMAX1(-99.0,hnpcmm))
            CALL Csopline(vnpcmchar,AMAX1(-99.0,vnpcm))
            CALL Csopline(vnpcmmchar,AMAX1(-99.0,vnpcmm))
            CALL Csopline(laixchar,AMAX1(-99.0,laix))
            CALL Csopline(laixmchar,AMAX1(-99.0,laixm))
            
            ! Evaluate
            EVHEADER = ' '
            EVHEADER(1:14) = '*EVALUATION : '
            IF (RUN.EQ.1.OR.(EXCODE.NE.EXCODEPREV.AND.EVALOUT.GT.1))THEN
              IF (RUN.EQ.1) THEN
                EVALOUT = 0
                EVHEADNM = 0
                EVHEADNMMAX = 1
              ENDIF
              IF (EXCODE.NE.EXCODEPREV) THEN
                EVHEADNM = EVHEADNM + 1
                OPEN (UNIT=FNUMEVAL,FILE=FNAMEEVAL,POSITION='APPEND')
                IF (EVHEADNM.LE.EVHEADNMMAX.AND.EVHEADNMMAX.GE.1) THEN
                  LENENAME = TVILENT(ENAME)
                  WRITE (FNUMEVAL,*) ' '
                  WRITE (FNUMEVAL,993) 
     &             EVHEADER,EXCODE,ENAME(1:25),MODNAME
  993             FORMAT (A14,A10,'  ',A25,2X,A8,/)
                ELSE
                  IF (EVHEADNMMAX.GT.1) THEN
                    WRITE (FNUMEVAL,*) ' '
                    WRITE (FNUMEVAL,1995) EVHEADER,MODNAME, 
     &               'ALL REMAIN','ING EXPERIMENTS        '
                  ELSEIF (EVHEADNM.LE.EVHEADNMMAX) THEN
                    WRITE (FNUMEVAL,*) ' '
                    WRITE (FNUMEVAL,1995) EVHEADER,MODNAME,
     &               'ALL EXPERI','MENTS                  '
 1995               FORMAT (A14,2X,A10,A23,2X,A8/)
                  ENDIF 
                ENDIF
              ENDIF
              IF (EVHEADNM.LE.EVHEADNMMAX) THEN
                WRITE (FNUMEVAL,994,ADVANCE='NO')
  994           FORMAT ('@RUN EXCODE      TRNO RN CR EDAPS EDAPM')
                DO L = 1,KEYSTX
                  IF (KEYSS(L).GT.0) THEN 
                    IF (SSABVO(KEYSS(L))(1:1).NE.' ') 
     &               WRITE (FNUMEVAL,'(A1)',ADVANCE='NO') ' '
                     WRITE (FNUMEVAL,'(A5,A1)',ADVANCE='NO') 
     &                      SSABVO(KEYSS(L)),'S'
                    IF (SSABVO(KEYSS(L))(1:1).NE.' ') 
     &               WRITE (FNUMEVAL,'(A1)',ADVANCE='NO') ' '
                     WRITE (FNUMEVAL,'(A5,A1)',ADVANCE='NO') 
     &                      SSABVO(KEYSS(L)),'M'
                  ENDIF
                ENDDO
                DO L = 1,KEYSTX
                  IF (KEYPS(L).GT.0) THEN
                    IF (PSABVO(KEYPS(L))(1:1).NE.' ') 
     &               WRITE (FNUMEVAL,'(A1)',ADVANCE='NO') ' '
                    WRITE (FNUMEVAL,'(A5,A1)',ADVANCE='NO') 
     &                      PSABVO(KEYPS(L)),'S'
                    IF (PSABVO(KEYPS(L))(1:1).NE.' ') 
     &               WRITE (FNUMEVAL,'(A1)',ADVANCE='NO') ' '
                    WRITE (FNUMEVAL,'(A5,A1)',ADVANCE='NO') 
     &                      PSABVO(KEYPS(L)),'M'
                  ENDIF 
                ENDDO
                WRITE (FNUMEVAL,9942)
 9942           FORMAT (
     &          ' HWAMS HWAMM HWUMS HWUMM',
     &          ' H#AMS H#AMM',
     &          ' LAIXS LAIXM L#SMS L#SMM',
     &          ' T#AMS T#AMM CWAMS CWAMM VWAMS VWAMM',
     &          ' HIAMS HIAMM HN%MS HN%MM VN%MS VN%MM',
     &          ' CNAMS CNAMM HNAMS HNAMM VNAMS VNAMM HINMS HINMM',
     &          ' CWACS CWACM ')
                CLOSE(FNUMEVAL)
              ENDIF  
            ENDIF  ! End Evaluate header writes
            IF (EXCODE.NE.EXCODEPREV) EVALOUT = 0
            EVALOUT = EVALOUT + 1
            OPEN (UNIT = FNUMEVAL,FILE = FNAMEEVAL,POSITION = 'APPEND')
            WRITE (FNUMEVAL,8404,ADVANCE='NO') RUN,EXCODE,TN,RN,CROP,
     &          edap,edapm
 8404       FORMAT (I4,1X,A10,I6,I3,1X,A2,2I6)
            DO L = 1,KEYSTX
              IF (KEYSS(L).GT.0) THEN
                !SSDAP(2) = DRDAP  !If need to check formula calculation
                IF (SSABVO(KEYSS(L))(1:1).NE.' ') 
     &            WRITE (FNUMEVAL,'(A1)',ADVANCE='NO') ' '
                WRITE (FNUMEVAL,'(I6)',ADVANCE='NO') SSDAP(KEYSS(L))
                IF (SSABVO(KEYSS(L))(1:1).NE.' ') 
     &            WRITE (FNUMEVAL,'(A1)',ADVANCE='NO') ' '
                WRITE (FNUMEVAL,'(I6)',ADVANCE='NO') SSDAPM(KEYSS(L))
              ENDIF
            ENDDO
            DO L = 1,KEYSTX
              IF (KEYPS(L).GT.0) THEN 
                IF (PSABVO(KEYPS(L))(1:1).NE.' ') 
     &            WRITE (FNUMEVAL,'(A1)',ADVANCE='NO') ' '
                WRITE (FNUMEVAL,'(I6)',ADVANCE='NO') PSDAP(KEYPS(L))
                IF (PSABVO(KEYPS(L))(1:1).NE.' ') 
     &            WRITE (FNUMEVAL,'(A1)',ADVANCE='NO') ' '
                WRITE (FNUMEVAL,'(I6)',ADVANCE='NO') PSDAPM(KEYPS(L))
              ENDIF
            ENDDO
            WRITE (FNUMEVAL,8406)
     F       NINT(hwam),NINT(hwamm),
     G       hwumchar,hwummchar,
     &       NINT(hnumam),NINT(hnumamm),  
     H       laix,laixm,lnumsm,lnumsmm,
     I       NINT(tnumam),NINT(tnumamm),
     J       NINT(cwam),NINT(cwamm),
     J       NINT(vwam),NINT(vwamm),
     L       hiamchar,hiammchar,
     M       hnpcmchar,hnpcmmchar,vnpcmchar,vnpcmmchar,
     &       NINT(cnam),NINT(cnamm),NINT(hnam),NINT(hnamm),
     &       NINT(vnam),NINT(vnamm),
     &       hinmchar,hinmmchar,
     N       NINT(cwahc),NINT(cwahcm)
 8406       FORMAT (
     A      I6,I6,
     G      A6,A6,I6,I6,
     H      F6.1,F6.1,F6.1,F6.1,
     I      I6,I6,
     J      I6,I6,
     J      I6,I6,
     L      A6,A6,
     M      A6,A6,A6,A6,
     &      I6,I6,I6,I6,I6,I6,
     &      A6,A6,
     &      I6,I6)
            Close(FNUMEVAL)
            ! End of Evaluation.Out writes

            ! Overview
            IF (IDETO.NE.'E') THEN  ! No Overview if only need Evaluate
              IF (FILEIOT(1:2).EQ.'DS') THEN
                IF (RUN.EQ.1 .AND. RUNI.EQ.1) THEN
                  OPEN (UNIT = FNUMOV, FILE = FNAMEOV)
                  WRITE(FNUMOV,'("*SIMULATION OVERVIEW FILE")')
                ELSE
                  INQUIRE (FILE = FNAMEOV, EXIST = FEXIST)
                  IF (FEXIST) THEN
                    OPEN (UNIT = FNUMOV, FILE = FNAMEOV, 
     &              POSITION = 'APPEND')
                  ELSE
                    OPEN (UNIT = FNUMOV, FILE = FNAMEOV, STATUS = 'NEW')
                    WRITE(FNUMOV,'("*SIMULATION OVERVIEW FILE")')
                  ENDIF
                ENDIF
                WRITE (FNUMOV,*) ' '
                CALL HEADER(1, FNUMOV, RUN)
              ELSE
                OPEN (UNIT = FNUMOV, FILE=FNAMEOV, POSITION='APPEND')
                WRITE (FNUMOV,'(/,A79,/)') OUTHED
                WRITE (FNUMOV,203) MODEL
  203           FORMAT (' MODEL            ',A8)
                IF (ISWNIT.EQ.'N') THEN
                  WRITE (FNUMOV,210) iswwat, iswnit
  210             FORMAT (' MODEL SWITCHES   ','Water: ',A1,
     &            '  Nitrogen: ',A1)
                ELSE
                  WRITE (FNUMOV,211) iswwat, iswnit, mesom
  211             FORMAT (' MODEL SWITCHES   ','Water: ',A1,
     &            '  Nitrogen: ',A1,' (OM decay: ',A1,')')
                ENDIF
                WRITE (FNUMOV,2031) MODNAME
 2031           FORMAT (' MODULE           ',A8)
                WRITE (FNUMOV,2032) MEPHS
 2032           FORMAT (' MODULE SWITCHES  ','Photosynthesis: ',A1)
                ! P=PARU effic,I=P+internal CO2,R=resistances(Monteith)
                WRITE (FNUMOV,2034) FILENEW
 2034           FORMAT (' FILE             ',A60)
                WRITE (FNUMOV,204)
     &           EXCODE(1:8),' ',EXCODE(9:10),'  ',ENAME(1:47)
  204           FORMAT (' EXPERIMENT       ',A8,A1,A2,A2,A47)
                WRITE (FNUMOV,202) TN, TNAME
  202           FORMAT (' TREATMENT',I3,'     ',A25)
                WRITE (FNUMOV,207) CROP,VARNO,VRNAME
  207           FORMAT (' GENOTYPE         ',A2,A6,'  ',A16)
                WRITE(FNUMOV,*) ' '
                CALL Calendar (plyear,plday,dom,month)
                WRITE (FNUMOV,208)month,dom,plyeardoy,NINT(pltpop),
     &           NINT(rowspc)
  208           FORMAT(' PLANTING         ',A3,I3,I8,2X,I4,' plants/m2 '
     &          ,'in ',I3,' cm rows')
!                CALL CSYR_DOY(EYEARDOY,YEAR,DOY)
                CALL YR_DOY(EYEARDOY,YEAR,DOY)
                CALL Calendar(year,doy,dom,month)
                WRITE(FNUMOV,109) month,dom,eyeardoy                  
  109           FORMAT (' EMERGENCE        ',A3,I3,I8)
                WRITE(FNUMOV,*) ' '
                WRITE (FNUMOV,209) tmaxx,tmaxm,tminn,tminm              
  209           FORMAT (' TEMPERATURES C   ','Tmax (max):',F5.1,
     &           ' (mnth av):',F5.1,
     &           ' Tmin (min):',F5.1,
     &           ' (mnth av):',F5.1)
                IF (ISWNIT.NE.'N') THEN
                  WRITE(fnumov,2095)cnad+rnad+hnad,hnad,vnad
 2095             FORMAT (' CROP N kg/ha     ','Total:  ',F8.1,
     &             '  Product ',F12.1,
     &             '  Leaf+stem:  ',F6.1)
                  WRITE(fnumov,2096)sennal(0),sennas            
 2096             FORMAT ('                  ','Leaf loss: ',F5.1,
     &             '  Root loss:  ',F8.1)
                  WRITE(fnumov,2093)soilni,amtnit,soilnf
 2093             FORMAT (' SOIL N kg/ha     ','Initial:',F8.1,
     &             '  Applied:',F12.1,
     &             '  Final:      ',F6.1)
                  WRITE(fnumov,2094)tnoxc,tlchc,
     &              tominfomc+tominsomc-tnimbsom  
 2094             FORMAT ('                  ',
     &             'Denitrified:',F4.1,
     &             '  Leached:',F12.1,
     &             '  Net from OM:',F6.1)
                  WRITE(fnumov,2099)tnimbsom,tominfomc,tominsomc   
 2099             FORMAT ('                  ',
     &             'OM Fixation:',F4.1,
     &             '  Fresh OM decay:',F5.1,
     &             '  SOM decay:',F8.1)
                  IF (tominsom1.GT.0.0)
     &             WRITE(fnumov,2098)NINT(tominsom1c),NINT(tominsom2c),
     &             NINT(tominsom3c)
 2098              FORMAT ('                  ',
     &             'SOM1 decay:',I5,
     &             '  SOM2 decay:   ',I6,
     &             '  SOM3 decay:',I7)
                ENDIF  
                IF (ISWWAT.NE.'N') THEN
                  WRITE(fnumov,2090)isoilh2o,rainc/10.0,irramtc/10.0
 2090             FORMAT (' SOIL WATER cm    ','Initial: ',F7.1,
     &             '  Precipitation: ',F5.1,
     &             '  Irrigation: ',F6.1)
                  WRITE(fnumov,2091)runoffc/10.0,drainc/10.0,fsoilh2o
 2091             FORMAT ('                  ','Runoff: ',F8.1,
     &             '  Drainage: ',F10.1,
     &             '  Final:    ',F8.1)
                  WRITE(fnumov,2089)eoc/10.0,eopenc/10.0,eompenc/10.0
 2089             FORMAT (' POTENTIAL ET cm  ','Crop model:',F5.1,
     &             '  Penman:    ',F9.1,
     &             '  Penman-M:  ',F7.1)
                  WRITE(fnumov,2097)eoptc/10.0,eoebudc/10.0
 2097             FORMAT ('                  ','Priestley: ',F5.1,
     &             '  E.budget:  ',F9.1)
                ENDIF
              ENDIF 
              ! End of Overview header writes
              WRITE(FNUMOV,9589)
              WRITE(fnumov,*)' '
              WRITE(fnumov,'(A11,I4,A3,A60)')
     &         ' RUN NO.   ',RUN,'  ',ENAME
              IF (DYNAMIC.EQ.SEASEND) THEN
                WRITE(fnumov,*)' '
                WRITE(fnumov,'(A50,A25)')
     &          ' NB. RUN TERMINATED PREMATURELY (PROBABLY BECAUSE ',
     &          'OF MISSING WEATHER DATA) '
              ENDIF
              WRITE(fnumov,9588)
              WRITE(fnumov,9600)
              DO L = 1, PSNUM
                CALL Csopline(laic,laistg(l))
                IF (STGYEARDOY(L).LT.9999999.AND.
     &              L.NE.10.AND.L.NE.11) THEN
!                  CALL CSYR_DOY(STGYEARDOY(L),YEAR,DOY)
                  CALL YR_DOY(STGYEARDOY(L),YEAR,DOY)
                  CALL Calendar(year,doy,dom,month)
                  CNCTMP = 0.0
                  IF (CWADSTG(L).GT.0.0)
     &             CNCTMP = CNADSTG(L)/CWADSTG(L)*100
                  WRITE (FNUMOV,'(I8,I4,1X,A3,I4,1X,I1,1X,A13,I6,A6,
     &            F6.1,I6,F6.2,F6.2,F6.2)')
     &            STGYEARDOY(L),DOM,MONTH,
     &            Dapcalc(stgyeardoy(L),(plyeardoy/1000),plday),
     &            l,psname(l),
     &            NINT(CWADSTG(L)),LAIC,LNUMSTG(L),
     &            NINT(CNADSTG(L)),CNCTMP,
     &            1.0-WFPPAV(L-1),1.0-NFPPAV(L-1)
                ENDIF
              ENDDO
              ! For harvest at specified date
              IF (YEARDOYHARF.EQ.YEARDOY) THEN
                CALL Csopline(laic,lai)
!                  CALL CSYR_DOY(YEARDOYHARF,YEAR,DOY)
                  CALL YR_DOY(YEARDOYHARF,YEAR,DOY)
                  CALL Calendar(year,doy,dom,month)
                  CNCTMP = 0.0
                  IF (CWAD.GT.0.0)CNCTMP = CNAD/CWAD*100
                  WRITE (FNUMOV,'(I8,I4,1X,A3,I4,1X,I1,1X,A13,I6,A6,
     &            F6.1,I6,F6.2,F6.2,F6.2)')
     &            YEARDOY,DOM,MONTH,
     &            Dapcalc(yeardoy,(plyeardoy/1000),plday),
     &            l,'Harvest      ',
     &            NINT(CWAD),LAIC,LNUM,
     &            NINT(CNAD),CNCTMP,
     &            1.0-WFPCAV,1.0-NFPCAV
              ENDIF 
              IF (RUN.EQ.1 .AND. RUNI.EQ.1) THEN
               WRITE(fnumov,*)' '
               WRITE(fnumov,*)
     &          'BIOMASS  = Above-ground dry weight (kg/ha)'
               WRITE(fnumov,*)'LEAF AREA  = Leaf area index (m2/m2)'
               WRITE(fnumov,*)
     &          'LEAF NUMBER  = Leaf number produced on main axis'
               WRITE(fnumov,*)'CROP N  = Above-ground N (kg/ha)'
               WRITE(fnumov,*)
     &          'CROP N%  = Above-ground N concentration (%)'
               WRITE(fnumov,*)
     &         'H2O STRESS = Photosynthesis stress,',
     &         ' prior to stage (0-1,0=none)'
               WRITE(fnumov,*)
     &         'N STRESS = Photosynthesis stress,',
     &         ' prior to stage(0-1,0=none)'
              ENDIF
              WRITE(fnumov,*)' '
              WRITE (FNUMOV,206)
              WRITE (FNUMOV,290) MAX(-99,gdap),MAX(-99,gdapm),
     A         MAX(-99,edap),MAX(-99,edapm)
              DO L = 1,KEYSTX
                IF (KEYPS(L).GT.0) THEN
                  IF (psdap(keyps(l)).LT.-1) EXIT
                  WRITE (FNUMOV,291)
     &             psname(KEYPS(L)),PSDap(KEYPS(L)),PSDapm(KEYPS(L))
                ENDIF                 
              ENDDO
              ! For harvest at specified date
              IF (YEARDOYHARF.EQ.YEARDOY) THEN
                tvi1 = Dapcalc(yeardoy,(plyeardoy/1000),plday)
                WRITE (FNUMOV,291)psname(mstg),tvi1,tvi1
              ENDIF
              WRITE (FNUMOV,305)
     H         NINT(cwam),NINT(cwamm),
     I         MAX(-99,NINT(rwam+sdwam)),NINT(rwamm),
     J         NINT(senwacm),NINT(senwacmm),
     L         NINT(hwam),NINT(hwamm),
     M         NINT(vwam),NINT(vwamm),
     N         hiam,hiamm,
     O         NINT(rswam),NINT(rswamm)
              IF (lwphc+swphc.GT.0.0) WRITE (FNUMOV,3051)
     &         NINT(cwahc),NINT(cwahcm),
     &         NINT(spnumhc*pltpop),MAX(-99,NINT(spnumhcm*pltpop))
              WRITE (FNUMOV,3052)
     A         hwumchar,hwummchar,
     B         NINT(hnumam),NINT(hnumamm),
     C         hnumgm,hnumgmm,
     D         NINT(tnumam),NINT(tnumamm),
     E         laix,laixm,
     F         lnumsm,lnumsmm,
     G         nupac,nupacm,
     H         cnam,cnamm,
     I         rnam,rnamm,
     J         sennatc,sennatcm,
     K         hnam,hnamm,
     L         vnam,vnamm,
     M         hinm,hinmm,
     N         hnpcm,hnpcmm,
     O         vnpcm,vnpcmm
              IF (cwaa.GT.0.0) WRITE (FNUMOV,3053)
     P         NINT(cwaa),NINT(cwaam),
     Q         cnaa,cnaam,
     R         NINT(lfwaa),NINT(lfwaam),
     S         NINT(stwaa),NINT(stwaam),
     T         NINT(rswaa),NINT(rswaam)
  290         FORMAT (                                                
     &           6X, 'Germination  (dap)          ',6X,I7,  4X,I7,  /,
     A           6X, 'Emergence    (dap)          ',6X,I7,  4X,I7    )
  291         FORMAT(                                                
     &           6X,A13, '(dap)          '         ,6X,I7  ,4X,I7    )
  305         FORMAT(                                                
     &           6X, 'AboveGround (kg dm/ha)      ',6X,I7,  4X,I7,  /,
     I           6X, 'Roots+seed residue (kg dm/ha)',5X,I7, 4X,I7,  /,
     J           6X, 'Senesced (kg dm/ha)         ',6X,I7,  4X,I7,  /,
     L           6X, 'Product (kg dm/ha)          ',6X,I7,  4X,I7,  /,
     K          6X, 'AboveGroundVegetative (kg dm/ha)  ',I7,4X,I7,  /,
     N           6X, 'HarvestIndex (ratio)        ',6X,F7.2,4X,F7.2,/,
     O           6X, 'Reserves (kg dm/ha)         ',6X,I7,  4X,I7)
 3051         FORMAT(                                               
     &           6X, 'Removed canopy (kg dm/ha)   ',7X,I6,  5X,I6,/,
     C           6X, 'Removed spikes (no/m2)      ',6X,I7  ,4X,I7  )  
 3052         FORMAT(                                                
     &           6X, 'Product unit wt (g dm)      ',7X,A6,  5X,A6,  /,
     A           6X, 'Product number (/m2)        ',6X,I7,  4X,I7,  /,
     B           6X, 'Product number (/shoot)     ',6X,F7.1,4X,F7.1,/,
     C           6X, 'Final shoot number (no/m2)  ',6X,I7  ,4X,I7  ,/,
     D           6X, 'Maximum leaf area index     ',6X,F7.1,4X,F7.1,/,
     E           6X, 'Final leaf number (one axis)',6X,F7.1,4X,F7.1,/,
     F           6X, 'Assimilated N (kg/ha)       ',6X,F7.1,4X,F7.1,/,
     G           6X, 'AboveGround N (kg/ha)       ',6X,F7.1,4X,F7.1,/,
     H           6X, 'Root N (kg/ha)              ',6X,F7.1,4X,F7.1,/,
     I           6X, 'Senesced N (kg/ha)          ',6X,F7.1,4X,F7.1,/,
     J           6X, 'Product N (kg/ha)           ',6X,F7.1,4X,F7.1,/,
     K         6X, 'AboveGroundVegetative N (kg/ha)  ',F8.1,4X,F7.1,/,
     L           6X, 'N HarvestIndex (ratio)      ',6X,F7.2,4X,F7.2,/,
     M           6X, 'Product N (% dm)            ',6X,F7.1,4X,F7.1,/,
     N         6X, 'AboveGroundVegetative N (% dm)    ',F7.1,4X,F7.1)
 3053         FORMAT(
     O           6X, 'Straw wt,anthesis (kg dm/ha)',6X,I7,  4X,I7  ,/,
     P           6X, 'Straw N,anthesis (kg/ha)    ',6X,F7.1,4X,F7.1,/,
     Q           6X, 'Leaf wt,anthesis (kg dm/ha) ',6X,I7  ,4X,I7  ,/,
     R           6X, 'Stem wt,anthesis (kg dm/ha) ',6X,I7  ,4X,I7  ,/,
     S           6X, 'Res. wt,anthesis (kg dm/ha) ',6X,I7  ,4X,I7  ,/)
              WRITE(fnumov,500)
              PFPPAV = -99.0
              PFGPAV = -99.0
              DO tvI1 = 1,mstg
                IF (pdays(tvi1).GT.0) THEN  
                WRITE(fnumov,600) psname(tvi1),dash,psname(tvi1+1), 
     &          pdays(tvI1),tmaxpav(tvI1),tminpav(tvI1),sradpav(tvI1),
     &          daylpav(tvI1),rainpc(tvI1),etpc(tvI1),1.-wfppav(tvi1),
     &          1.0-wfgpav(tvi1), 1.0-nfppav(tvi1), 1.0-nfgpav(tvi1), 
     &          pfppav(tvi1), pfgpav(tvi1)
  600           FORMAT(1X,A10,A3,A10,I5,3F6.1,F7.2,2F7.1,4F7.3,2F7.2)
  610           FORMAT(1X,A10,13X,I5,3F6.1,F7.2,2I7,6F7.3)
                ENDIF
              ENDDO
              IF (pdays(mstg).GT.0.OR.yeardoyharf.EQ.yeardoy) THEN 
              WRITE(fnumov,*) ' '
              pfpcav = -99.0
              pfgcav = -99.0 
              IF (pdays(mstg).GT.0.) THEN 
                WRITE(fnumov,600) psname(1),dash,psname(mstg), 
     &           cdays, tmaxcav, tmincav, sradcav,
     &           daylcav, raincc, etcc, 1.0-wfpcav, 
     &           1.0-wfgcav, 1.0-nfpcav, 1.0-nfgcav,
     &           pfpcav, pfgcav
              ELSE  
                WRITE(fnumov,600) psname(1),dash,'Harvest', 
     &           cdays, tmaxcav, tmincav, sradcav,
     &           daylcav, raincc, etcc, 1.0-wfpcav, 
     &           1.0-wfgcav, 1.0-nfpcav, 1.0-nfgcav,
     &           pfpcav, pfgcav
              ENDIF 
              ! Resource productivity calculations
              DMP_Rain = -99.
              GrP_Rain = -99.
              DMP_ET = -99.
              GrP_ET = -99.
              DMP_EP = -99.
              GrP_EP = -99.
              DMP_Irr = -99.    
              GrP_Irr = -99.
              DMP_NApp = -99.
              GrP_NApp = -99.
              DMP_NUpt = -99.
              GrP_NUpt = -99.
              IF (RAINCC > 1.E-3) THEN
               DMP_Rain = CWAM / RAINCC 
               GrP_Rain = HWAM  / RAINCC
              ENDIF
              IF (ETCC > 1.E-3) THEN
               DMP_ET = CWAM / ETCC 
               GrP_ET = HWAM  / ETCC 
              ENDIF
              IF (EPCC > 1.E-3) THEN
               DMP_EP = CWAM / EPCC 
               GrP_EP = HWAM  / EPCC 
              ENDIF
              IF (IRRAMTC > 1.E-3) THEN
                DMP_Irr = CWAM / IRRAMTC 
                GrP_Irr = HWAM  / IRRAMTC
              ENDIF
              IF (ISWNIT.NE.'N') THEN
                IF (Amtnit > 1.E-3) THEN
                  DMP_NApp = CWAM / Amtnit
                  GrP_NApp = HWAM  / Amtnit
                ENDIF
                IF (NUPAC > 1.E-3) THEN
                  DMP_NUpt = CWAM / NUPAC
                  GrP_NUpt = HWAM  / NUPAC
                ENDIF
              ENDIF ! ISWNIT NE 'N'
              WRITE (FNUMOV, 1200) CDAYS, 
     &         RAINCC, DMP_Rain*0.1, DMP_Rain, GrP_Rain*0.1, GrP_Rain,
     &         ETCC,  DMP_ET*0.1,   DMP_ET,   GrP_ET*0.1,   GrP_ET, 
     &         EPCC,  DMP_EP*0.1,   DMP_EP,   GrP_EP*0.1,   GrP_EP
              IF (IRRAMTC > 1.E-3) THEN
                WRITE(FNUMOV, 1210) 
     &            IRRAMTC, DMP_Irr*0.1, DMP_Irr, GrP_Irr*0.1, GrP_Irr
              ENDIF  
              IF (ISWNIT.NE.'N') THEN
                IF (Amtnit > 1.E-3) THEN
                  WRITE(FNUMOV, 1220) Amtnit, DMP_NApp, GrP_NApp 
                ENDIF
                IF (NUPAC > 1.E-3) THEN
                  WRITE(FNUMOV, 1230) NUPAC, DMP_NUpt,GrP_NUpt
                ENDIF
              ENDIF ! ISWNIT NE 'N'
              WRITE(FNUMOV,270)
              IF (CROP.EQ.'WH') THEN 
                WRITE(FNUMOV,300) 'WHEAT', NINT(HWAM),GMPCH
              ELSEIF (CROP.EQ.'BA') THEN 
                WRITE(FNUMOV,300) 'BARLEY', NINT(HWAM),GMPCH
              ENDIF  
              WRITE(FNUMOV,'(110("*"))')
              CLOSE(FNUMOV)  ! Overview.out
              ENDIF 
              ! Basic info.to Work.out when calling for Overview
              !  Summary of various environmental aspects
!              WRITE(fnumwrk,*) ' '
!              WRITE(fnumwrk,'(A16,A10,I3)')
!     &          ' CONDITIONS FOR ',
!     &            excode,tn
!              WRITE(fnumwrk,*) ' '
!              WRITE (fnumwrk,209) tmaxx,tmaxm,tminn,tminm              
!              IF (ISWNIT.NE.'N') THEN
!                WRITE(fnumwrk,2095)cnad+rnad+hnad,hnad,vnad
!                WRITE(fnumwrk,2096)sennal(0),sennas            
!                WRITE(fnumwrk,2093)soilni,amtnit,soilnf
!                WRITE(fnumwrk,2094)
!     &            tnoxc,tlchc,tominsomc+tominfomc-tnimbsom
!                WRITE(fnumwrk,2099)tnimbsom,tominfomc,tominsomc   
!                IF (tominsom1.GT.0.0)
!     &            WRITE(fnumwrk,2098)NINT(tominsom1c),NINT(tominsom2c),
!     &             NINT(tominsom3c)
!                IF (FILEIOT.EQ.'DS4'.AND.IDETL.EQ.'D'.OR.
!     &             FILEIOT.EQ.'DS4'.AND.IDETL.EQ.'A'.OR.
!     &             FILEIOT.NE.'DS4') THEN                  
!                  WRITE(fnumwrk,2090)isoilh2o,rainc/10.0,irramtc/10.0
!                  WRITE(fnumwrk,2091)runoffc/10.0,drainc/10.0,fsoilh2o
!                  WRITE(fnumwrk,2089)eoc/10.0,eopenc/10.0,eompenc/10.0
!                  WRITE(fnumwrk,2097)eoptc/10.0,eoebudc/10.0
!                ENDIF
!                IF (FAPPNUM.GT.0) THEN
!                  WRITE (fnumwrk,*) ' '
!                  WRITE (fnumwrk,'(A18,A10,I3)')
!     &              ' N FERTILIZER FOR ',excode,tn
!                  DO L = 1,FAPPNUM
!                     WRITE (fnumwrk,'(A80)') FAPPLINE(L)
!                  ENDDO
!                ENDIF
!                WRITE(FNUMWRK,*) ' '
!                WRITE(FNUMWRK,'(A45)')
!     &            ' INORGANIC N (kg/ha) LEFT IN SOIL AT MATURITY'
!                WRITE(FNUMWRK,'(A28,2F6.1)')
!     &           '  NO3 and NH4 N in PROFILE: ',
!     &           SNO3PROFILE,SNH4PROFILE
!                WRITE(FNUMWRK,'(A28,2F6.1)')
!     &           '  NO3 and NH4 N in ROOTZONE:',
!     &           SNO3ROOTZONE,SNH4ROOTZONE
!              ENDIF   ! End Iswnit NE N
!              WRITE(FNUMWRK,*) ' '
!              WRITE(FNUMWRK,'(A34)')
!     &          ' H2O (mm) LEFT IN SOIL AT MATURITY'
!              WRITE(FNUMWRK,'(A36,2F6.1)')
!     &         '  H2O and AVAILABLE H2O in PROFILE: ',
!     &         H2OPROFILE,AH2OPROFILE
!              WRITE(FNUMWRK,'(A36,2F6.1)')
!     &         '  H2O and AVAILABLE H2O in ROOTZONE:',
!     &         H2OROOTZONE,AH2OROOTZONE
!              WRITE (fnumwrk,*) ' '
!              WRITE (fnumwrk,'(A32,A10,I3)')
!     &         ' CRITICAL PERIOD CONDITIONS FOR ',excode,tn
!              WRITE (fnumwrk,'(A40,F6.1)')
!     &         '  Temperature mean,germination         ',TMEANG
!              WRITE (fnumwrk,'(A40,F6.1)')
!     &         '  Temperature mean,germ-emergence      ',TMEANE
!              WRITE (fnumwrk,'(A40,F6.1)')
!     &         '  Temperature mean,first 20 days       ',TMEAN20P
!              IF (TMEAN20ANTH.GT.0.0) WRITE (fnumwrk,'(A40,F6.1)')
!     &         '  Temperature mean,20d around anthesis ',TMEAN20ANTH
!              IF (CUMDU.GE.MSTG-1) THEN 
!               WRITE (fnumwrk,'(A40,3F6.1)')
!     &         '  Temperature mean,max,min,grain fill  ',
!     &         TMEANPAV(MSTG-1),TMAXPAV(MSTG-1),TMINPAV(MSTG-1)
!               WRITE (fnumwrk,'(A40,F6.1)')
!     &         '  Temperature max.reached,grain fill   ',
!     &         TMAXGFILL
!              ENDIF
!              IF (SRAD20ANTH.GT.0.0) THEN
!                WRITE (fnumwrk,*)' '
!                WRITE (fnumwrk,'(A38,F6.1)')
!     &           '  Solar radn. mean,20d around anthesis ',SRAD20ANTH
!              ENDIF
!              WRITE (fnumwrk,'(A38,F6.1)')
!     &         '  Stress fac. mean,20d before grain set',STRESS20GS
              
            ELSE   ! For Overview
            
              OPEN (UNIT=FNUMOV, FILE=FNAMEOV, STATUS = 'UNKNOWN')
              CLOSE (UNIT=FNUMOV, STATUS = 'DELETE')
              
            ENDIF  ! For Overview  (IDETO.NE.'E')                    
          
          ELSE ! For Evaluate,Overview  IDETL.EQ.'0'.OR.IDETO.NE.'N'
          
            OPEN (UNIT=FNUMMEAS, FILE=FNAMEMEAS, STATUS = 'UNKNOWN')
            CLOSE (UNIT=FNUMMEAS, STATUS = 'DELETE')
            OPEN (UNIT=FNUMEVAL, FILE=FNAMEEVAL, STATUS = 'UNKNOWN')
            CLOSE (UNIT=FNUMEVAL, STATUS = 'DELETE')
            OPEN (UNIT=FNUMOV, FILE=FNAMEOV, STATUS = 'UNKNOWN')
            CLOSE (UNIT=FNUMOV, STATUS = 'DELETE')
          
          ENDIF  ! End Ideto outputs (Evaluate,Overview)'

!-----------------------------------------------------------------------
!         IDETS OUTPUTS (Plantsum)
!-----------------------------------------------------------------------
           
          IF ((IDETS.NE.'N'.AND.IDETL.NE.'0').OR.IDETL.EQ.'A') THEN
          
            ! PLANT SUMMARY (SIMULATED)'
            IF (CROP.NE.CROPPREV.OR.RUN.EQ.1) THEN
              OPEN (UNIT=fnumpsum,FILE=FNAMEPSUM,POSITION='APPEND')
              WRITE (FNUMPSUM,9953)
 9953         FORMAT (/,'*SUMMARY')
              WRITE (FNUMPSUM,99,ADVANCE='NO')
   99         FORMAT ('@  RUN EXCODE    TRNO RN',
     X         ' TNAME....................',
     A        ' REP  RUNI S O C    CR PYEAR  PDAT  GDAP  EDAP')
              DO L = 1,KEYSTX
                IF (KEYPS(L).GT.0) THEN
                  WRITE (FNUMPSUM,'(A6)',ADVANCE='NO') PSABVO(KEYPS(L))
                  !IF (PSABVO(KEYPS(L)).EQ.'TSDAP') THEN
                  ! WRITE (FNUMPSUM,'(A6)',ADVANCE='NO') '  DAYL'
                  !ENDIF
                ENDIF
              ENDDO
              WRITE (FNUMPSUM,299)
  299         FORMAT (
     B        '   FLN FLDAP HYEAR  HDAY SDWAP',
     C        ' CWAHC  CWAM PARUE  HWAM  HWAH  VWAM  HWUM  H#AM  H#UM',
     D        ' SDNAP  CNAM  HNAM  RNAM  TNAM  NUCM  HN%M  VN%M TMAXG',
     E        ' SRADA  TAVA CWADG TILWG D1INI D2INI D3INI ')
              CLOSE(fnumpsum)  
            ENDIF  ! End of Plantsum.Out headers
            OPEN (UNIT=fnumpsum,FILE=FNAMEPSUM,POSITION='APPEND')
            WRITE (fnumpsum,400,ADVANCE='NO') run,excode,tn,rn,tname,
     A        rep,runi,sn,on,cn,crop,
     B        plyear,plday,gdap,edap 
  400       FORMAT (I6,1X,A10,I4,I3,1X,A25,
     A       I4,I6,I2,I2,I2,4X,A2,
     B       I6,I6,I6,I6)
            DO L = 1,KEYSTX
              IF (KEYPS(L).GT.0) THEN
                WRITE (FNUMPSUM,'(I6)',ADVANCE='NO') PSDAP(KEYPS(L))
                IF (PSABVO(KEYPS(L)).EQ.'TSDAP') THEN
                  WRITE (FNUMPSUM,'(F6.1)',ADVANCE='NO') DAYLST(L)
                ENDIF  
              ENDIF
            ENDDO
            WRITE (fnumpsum,401)FLN, FLDAP,
     &        hayear,hadoy,
     C        NINT(sdrate),NINT(cwahc),
     D        NINT(cwam),pariued,NINT(hwam),
     E        NINT(hwam*hpcf/100.0),NINT(vwam),
     F        hwumchar,NINT(hnumam),NINT(hnumgm),
     G        sdnap,NINT(cnam),NINT(hnam),NINT(rnam),
     H        NINT(AMAX1(-99.0,cnam+rnam)),NINT(nupac),
     I        hnpcmchar,vnpcmchar,tmaxgfill,sradcav,tmeancav,
     &        NINT(cwadgs),tilwtgs,
     J        didoy(1),didoy(2),didoy(3)
  401       FORMAT (
     &       F6.1,I6, 
     B       I6,I6,
     C       I6,I6,
     D       I6,F6.1,I6,
     E       I6,I6,
     F       A6,I6,I6,
     G       F6.1,I6,I6,I6,
     H       I6,I6,
     I       2A6,3F6.1,
     &       I6,F6.3,
     J       3I6)
            CLOSE(fnumpsum)  
          ELSE  
            OPEN (UNIT=FNUMPSUM,FILE=FNAMEPSUM,STATUS='UNKNOWN')
            CLOSE (UNIT=FNUMPSUM, STATUS = 'DELETE')
          ENDIF
          ! End IDETS Outputs (Plantsum.Out)          

!-----------------------------------------------------------------------
!         IDETL = Y or D OUTPUTS (Leaves,Phases)
!-----------------------------------------------------------------------

          IF (IDETL.EQ.'Y'.OR.IDETL.EQ.'D'.OR.IDETL.EQ.'A') THEN
          
            ! LEAVES.OUT
            OPEN(UNIT=FNUMLVS,FILE=FNAMELEAVES,POSITION='APPEND')
            WRITE (FNUMLVS,'(/,A79,/)') OUTHED
            WRITE (FNUMLVS,'(A14,F6.1)') '! LEAF NUMBER ',LNUM
            IF (LAFSWITCH.GT.0.0) THEN
              WRITE(FNUMLVS,'(A42,F6.2)')
     &         '! LEAF NUMBER WHEN INCREASE FACTOR CHANGED',lafswitch
              !Taken out whilst use simpler calculations at change-over 
              !LAPOTX(INT(LAFSWITCH+1)) = LAPOTXCHANGE
              WRITE(FNUMLVS,'(A35,F6.2)')
     &         '! AREA OF LEAF WHEN FACTOR CHANGED  ',lapotxchange
            ENDIF     
            WRITE (FNUMLVS,'(/,A42,A30)')
     &       '@ LNUM AREAP AREA1 AREAT AREAS  T#PL  T#AL',
     &       '  WFLF  NFLF  AFLF  TFLF DAYSG'
            DO I = 1, INT(LNUM+0.99)
              CALL Csopline(lapotxc,lapotx(i))
              CALL Csopline(latlc,latl(1,i))
              CALL Csopline(lapc,lap(i))
              CALL Csopline(lapsc,laps(i))
              ! Adjust for growth period
              WFLF(I) = WFLF(I)/AMIN1(1.0,(LAGEP(I)/LLIFG))
              WFLFP(I) = WFLFP(I)/AMIN1(1.0,(LAGEP(I)/LLIFG))
              NFLF(I) = NFLF(I)/AMIN1(1.0,(LAGEP(I)/LLIFG))
              NFLFP(I) = NFLFP(I)/AMIN1(1.0,(LAGEP(I)/LLIFG))
              TFLF(I) = TFLF(I)/AMIN1(1.0,(LAGEP(I)/LLIFG))
              AFLF(I) = AFLF(I)/AMIN1(1.0,(LAGEP(I)/LLIFG))
              WRITE (fnumlvs,'(I6,4A6,F6.1,I6,4F6.1,F6.1)')
     &          I,LAPOTXC,
     &          LATLC,LAPC,LAPSC,
     &          TNUML(I),NINT(TNUML(I)*PLTPOP),1.0-WFLF(I),
     &          1.0-NFLF(I),1.0-AFLF(I),1.0-TFLF(I),DGLF(I)
            ENDDO
            IF (RUN.EQ.1) THEN
              WRITE(fnumlvs,*)' '
              WRITE(fnumlvs,'(A36)')
     &          '! LNUM = Number of leaf on one axis '
              WRITE(fnumlvs,'(A52)')
     &          '! AREAP = Potential area of leaf on main axis (cm2) '
              WRITE(fnumlvs,'(A41,A16)')
     &          '! AREA1 = Area of youngest mature leaf on', 
     &          ' main axis (cm2)'
              WRITE(fnumlvs,'(A42,A16)')
     &          '! AREAT = Area of cohort of leaves at leaf',
     &          ' position (cm2) '
              WRITE(fnumlvs,'(A43,A35)')
     &          '! AREAS = Senesced area of cohort of leaves',
     &          ' at maturity at leaf position (cm2)'
              WRITE(fnumlvs,'(A45)')
     &          '! T#PL = Tiller number/plant at leaf position'
              WRITE(fnumlvs,'(A48)')
     &          '! T#AL = Tiller number/area(m2) at leaf position'
              WRITE(fnumlvs,'(A38,A17)')
     &          '! WFLF  = Water stress factor for leaf',
     &          ' (0-1,1=0 stress)'
              WRITE(fnumlvs,'(A48,A17)')
     &          '! WFLFP = Water stress factor for photosynthesis',
     &          ' (0-1,1=0 stress)'
              WRITE(fnumlvs,'(A51)')
     &          '! NFLF  = N stress factor for leaf (0-1,1=0 stress)'
              WRITE(fnumlvs,'(A44,A17)')
     &          '! NFLFP = N stress factor for photosynthesis',
     &          ' (0-1,1=0 stress)'
              WRITE(fnumlvs,'(A36,A24)')
     &          '! AFLF  = Assimilate factor for leaf',
     &          ' (0-1,1=0 no limitation)'
              WRITE(fnumlvs,'(A37,A24)')
     &          '! TFLF  = Temperature factor for leaf',
     &          ' (0-1,1=0 no limitation)'
              WRITE(fnumlvs,'(A37)')
     &          '! DAYSG = Number of days of growth   '
            ENDIF
            CLOSE (FNUMLVS)
            ! End of Leaves.out
          
            ! Phase conditions (Simulated;PHASES.OUT)
            OPEN(UNIT=FNUMPHA,FILE=FNAMEPHASES,POSITION='APPEND')
            WRITE (FNUMPHA,'(/,A79,/)') OUTHED
            WRITE (fnumpha,'(A45,A24,A12)')
     &       '@PH PDAYS SRADA  TMXA  TMNA  PREA  TWLA  CO2A',
     &       '  WFPA  WFGA  NFPA  NFGA',
     &       ' PHASE_END  '
            DO L=1,PSNUM
              IF (STGYEARDOY(L).LT.9999999.AND.
     &         L.NE.0.AND.L.NE.10.AND.L.NE.11)
     &         WRITE (fnumpha,'(I3,I6,3F6.1,2F6.2,I6,4F6.2,1X,A13)')
     &         L,pdays(L),sradpav(L),tmaxpav(L),tminpav(L),
     &         rainpav(L),daylpav(L),NINT(co2pav(L)),
     &         1.0-wfppav(L),1.0-wfgpav(L),
     &         1.0-nfppav(L),1.0-nfgpav(L),
     &         psname(MIN(L+1,PSX))
            ENDDO
            CLOSE (FNUMPHA)
              
          ELSE
          
            OPEN (UNIT=FNUMLVS,FILE=FNAMELEAVES,STATUS='UNKNOWN')
            CLOSE (UNIT=FNUMLVS, STATUS = 'DELETE')
            OPEN (UNIT=FNUMPHA,FILE=FNAMEPHASES,STATUS = 'UNKNOWN')
            CLOSE (UNIT=FNUMPHA, STATUS = 'DELETE')
            
          ENDIF
          ! End of Leaves and Phases writes
          
          ! If have not read measured data cannot produce A summaries
          IF (IDETL.EQ.'D'.AND.IDETO.EQ.'N') THEN
            WRITE(Message(1),'(A35)')
     &       'IDETL flag called for detail files.'
            WRITE(Message(2),'(A31,A31)')
     &       'But IDETO flag set at N so that',
     &       'measured data not read.        '
            WRITE(Message(3),'(A45)')
     &       'Therefore,could not write detailed summaries.'
            CALL WARNING(3,'CSCRP',MESSAGE)
!            WRITE(Fnumwrk,*)' '
!            WRITE(Fnumwrk,'(A36)')
!     &       ' IDETL flag called for detail files.'
!            WRITE(Fnumwrk,'(A32,A31)')
!     &       ' But IDETO flag set at N so that',
!     &       'measured data not read.        '
!            WRITE(Fnumwrk,'(A46)')
!     &       ' Therefore,could not write detailed summaries.'
          ENDIF
          
!-----------------------------------------------------------------------
!         IDETL = D OUTPUTS (Work details;Phenols,m;Plantres,m)
!-----------------------------------------------------------------------
           
          IF ((IDETL.EQ.'D'.AND.IDETO.NE.'N').OR.IDETL.EQ.'A') THEN
                  
            ! WORK
!            WRITE(fnumwrk,*) ' '
!            WRITE(fnumwrk,'(A26,A10,I3)')' HARVEST/FAILURE DATA FOR ',
!     &       excode,tn
!            WRITE(fnumwrk,*)' '
!            IF (DYNAMIC.EQ.SEASEND .AND. SEASENDOUT.NE.'Y') THEN
!              WRITE(fnumwrk,*)  ' Program terminated      ',YEARDOY
!            ELSE 
!              WRITE(fnumwrk,*)  ' Harvest reached         ',YEARDOY
!            ENDIF  
!            WRITE (fnumwrk,*)' '
!            WRITE (fnumwrk,'(A54,F5.1,F4.1)')
!     &       '  Overall PAR use efficientcy(incident,intercepted) = ',
!     &       paruec,pariued
!            WRITE(fnumwrk,*) ' '
!            WRITE(fnumwrk,'(A27,F11.2)')'  Harvest product (kg/ha)  ',
!     &       HWAM
!            WRITE(fnumwrk,'(A27,F11.2)')'  Product/Total wt (HI)    ',
!     &       HIAM
!            IF (GNOWTS.GT.0.0.AND.CWAM.GT.0.0) THEN
!              WRITE(fnumwrk,'(A28,F10.2)')
!     &         '  Grain #/(Total wt)        ',HNUMAM/(CWAM*.1)
!              WRITE(fnumwrk,'(A28,F10.2)')
!     &         '  Grain #/(Total product)   ',HNUMAM/((CWAM-HWAM)*.1)
!              WRITE(fnumwrk,*) ' '
!              WRITE(fnumwrk,'(A28,F10.2,A1)')
!     &        '  (Grain #/Total standard   ',GNOWTS,')'
!            ENDIF
!            IF (GNOWTS.GT.0.0) THEN
!             WRITE (fnumwrk,*)' '
!             IF (gnopm.GT.0.0) WRITE (fnumwrk,'(A22,F7.1)')
!     &        '  Grain weight mg     ',GRWT/GNOPM*1000.0
!             WRITE (fnumwrk,'(A22,F7.1)')
!     &        '  Grain weight coeff  ',gwta
!             IF (carbolim.GT.0.OR.Nlimit.GT.0.OR.Tlimit.GT.0) THEN
!               WRITE (fnumwrk,'(A38)')
!     &          '  Grain growth limited by some factor!'
!               WRITE(fnumwrk,'(A24,I5)')
!     &          '   Days of Ch2o limit   ',carbolim
!               WRITE(fnumwrk,'(A24,I5)')
!     &          '   Days of N limit      ',nlimit
!               WRITE(fnumwrk,'(A24,I5)')
!     &          '   Days of temp limit   ',tlimit
!             ENDIF
!             WRITE(fnumwrk,'(A24,I5)')
!     &        '  Days of linear fill   ',pdays(MSTG-1)
!             IF (grwt.GT.0.0) WRITE (fnumwrk,'(A24,F7.1)')
!     &        '  Grain nitrogen %       ',grainn/grwt*100.0
!             WRITE (fnumwrk,'(A24,F7.1)')
!     &        '  Minimum nitrogen %     ',gnpcmn
!             WRITE (fnumwrk,'(A24,F7.1)')
!     &        '  Standard nitrogen %   ',gnpcs
!            ENDIF
!            WRITE(fnumwrk,*) ' '
!            WRITE(fnumwrk,'(A26,A10,I3)')
!     &       ' CH2O BALANCE (kg/ha) FOR ',excode,tn
!            WRITE(fnumwrk,'(A27,3F11.1)')
!     &       '  SEED+FIXED (1) Seed,fixed',
!     &       (SEEDRSI+SDCOAT+CARBOC)*PLTPOP*10.0,
!     &       (SEEDRSI+SDCOAT)*PLTPOP*10.0,CARBOC*PLTPOP*10.0
             TVR1 = (SEEDRSI+SDCOAT+CARBOC)*PLTPOP*10.0
!            WRITE(fnumwrk,'(A27,3F11.1)')
!     &       '  RESPIRED (2)  Tops,root  ',RESPC*PLTPOP*10.0,
!     &       RESPTC*PLTPOP*10.0,RESPRC*PLTPOP*10.0
             TVR2 = RESPC*PLTPOP*10.0
!            WRITE(fnumwrk,'(A27,3F11.1)')
!     &       '  SENESCED (3)  Tops,root  ',
!     &       (SENTOPLITTER+SENROOT)*PLTPOP*10.0,
!     &       SENTOPLITTER*PLTPOP*10.0,SENROOT*PLTPOP*10.0
             TVR3 = (SENTOPLITTER+SENROOT)*PLTPOP*10.0
!            WRITE(fnumwrk,'(A27,3F11.1)')
!     &       '  LIVE+DEAD (4) Live,dead  ',
!     &       (SEEDRS+SDCOAT+RTWT+LFWT+STWT+CHWT+GRWT+RSWT+
!     &        SENTOPRETAINED)*PLTPOP*10.0,
!     &       (SEEDRS+SDCOAT+RTWT+LFWT+STWT+CHWT+GRWT+RSWT)
!     &       *PLTPOP*10.0,
!     &        SENTOPRETAINED*PLTPOP*10.0
             TVR4 = (SEEDRS+SDCOAT+RTWT+LFWT+STWT+CHWT+GRWT+RSWT+
     &        SENTOPRETAINED)*PLTPOP*10.0
!            WRITE(fnumwrk,'(A27,3F11.1)')
!     &       '  PLANT+SEED_RESIDUE Pl,sd ',
!     &       (SEEDRS+SDCOAT+RTWT+LFWT+STWT+CHWT+GRWT+RSWT)
!     &       *PLTPOP*10.0,
!     &       (RTWT+LFWT+STWT+CHWT+GRWT+RSWT)*PLTPOP*10.0,
!     &       (SEEDRS+SDCOAT)*PLTPOP*10.0
!            WRITE(fnumwrk,'(A27,2F11.1)')
!     &       '  RESERVES (5)  Post-mat   ',RSWT*PLTPOP*10.0,
!     &       RSWTPM*PLTPOP*10.0
             TVR5 = RSWT*PLTPOP*10.0
!            WRITE(fnumwrk,'(A29, F9.1)')
!     &       '  HARVESTED DURING CYCLE (6) ',
!     &       (LWPHC+SWPHC+RSWPHC+GWPHC+DWRPHC)*PLTPOP*10.0
             TVR6 = (LWPHC+SWPHC+RSWPHC+GWPHC+DWRPHC)*PLTPOP*10.0
!            WRITE(fnumwrk,'(A27, F11.2)')
!     &       '  BALANCE (1-(2+3+4+6))    ',TVR1 -(TVR2+TVR3+TVR4+TVR6)
!            IF (ABS(TVR1-(TVR2+TVR3+TVR4+TVR6)).GT.0.05)
!     &      WRITE(fnumwrk,'(A29,A10,A1,I2)')
!     &       '   PROBLEM WITH CH2O BALANCE ',EXCODE,' ',TN
!            IF (GNOWTS.GT.0.0.AND.lfwtsge.GT.0.0) THEN
!              WRITE (fnumwrk,*) ' '
!              WRITE (fnumwrk,'(A42,A10,I3)')
!     &         ' CH2O BALANCE FROM END OF STEM GROWTH FOR ',excode,tn
!              WRITE (fnumwrk,'(A53)')
!     &         '  NB.Balance assumes that no dead matter is shed     '
!              WRITE (fnumwrk,'(A22,F7.1)')'  Above ground at SGE ',
!     &         (lfwtsge+stwtsge+rswtsge+grwtsge+deadwtsge)*pltpop*10.0
!              WRITE (fnumwrk,'(A22,F7.1)')'  Leaves at SGE       ',
!     &         (lfwtsge)*pltpop*10.0
!              WRITE (fnumwrk,'(A22,F7.1)')'  Stems at SGE        ',
!     &         (stwtsge)*pltpop*10.0
!              WRITE (fnumwrk,'(A22,F7.1)')'  Reserves at SGE     ',
!     &         (rswtsge)*pltpop*10.0
!              WRITE (fnumwrk,'(A22,F7.1)')'  Grain at SGE        ',
!     &         (grwtsge)*pltpop*10.0
!              WRITE (fnumwrk,'(A22,F7.1)')'  Dead at SGE         ',
!     &         (deadwtsge)*pltpop*10.0
!              WRITE (fnumwrk,'(A22,F7.1)')'  Roots at SGE        ',
!     &         (rtwtsge)*pltpop*10.0
!              WRITE (fnumwrk,'(A22,F7.1)')'  Above ground at end ',
!     &         (lfwt+stwt+rswt+grwt+SENTOPRETAINED)*pltpop*10.0
!              WRITE (fnumwrk,'(A22,F7.1)')'  Leaves at end       ',
!     &         (lfwt)*pltpop*10.0
!              WRITE (fnumwrk,'(A22,F7.1)')'  Stems at end        ',
!     &         (stwt)*pltpop*10.0
!              WRITE (fnumwrk,'(A22,F7.1)')'  Reserves at end     ',
!     &         (rswt)*pltpop*10.0
!              WRITE (fnumwrk,'(A22,F7.1)')'  Grain at end        ',
!     &         (grwt)*pltpop*10.0
!              WRITE (fnumwrk,'(A22,F7.1)')'  Dead at end         ',
!     &         (SENTOPRETAINED)*pltpop*10.0
!              WRITE (fnumwrk,'(A22,F7.1)')'  Roots at end        ',
!     &         (rtwt)*pltpop*10.0
!              WRITE (fnumwrk,'(A22,F7.1)')'  Assimilation > SGE  ',
!     &         carbogf*pltpop*10.0
!              WRITE (fnumwrk,'(A22,F7.1)')'  Respiration > SGE   ',
!     &         (respgf)*pltpop*10.0
!              WRITE (fnumwrk,'(A22,F7.1)')'  Senesced > SGE      ',
!     &         sengf*pltpop*10.0
!              WRITE (fnumwrk,'(A22,F7.1)')'  Leaves > SGE        ',
!     &         (lfwt-lfwtsge)*pltpop*10.0
!              WRITE (fnumwrk,'(A22,F7.1)')'  Stems > SGE         ',
!     &         (stwt-stwtsge)*pltpop*10.0
!              WRITE (fnumwrk,'(A22,F7.1)')'  Reserves > SGE      ',
!     &         (rswt-rswtsge)*pltpop*10.0
!              WRITE (fnumwrk,'(A22,F7.1)')'  Grain > SGE         ',
!     &         (grwt-grwtsge)*pltpop*10.0
!              WRITE (fnumwrk,'(A22,F7.1)')'  Dead > SGE          ',
!     &         (SENTOPRETAINED-deadwtsge)*pltpop*10.0
!              WRITE (fnumwrk,'(A22,F7.1)')'  Roots > SGE         ',
!     &         (rtwt-rtwtsge)*pltpop*10.0
!              WRITE (fnumwrk,'(A22,F7.1)')'  Total > SGE         ',
!     &         ((lfwt+stwt+rswt+grwt+SENTOPRETAINED+rtwt)*pltpop*10.0) -
!     &         ((lfwtsge+stwtsge+rswtsge+grwtsge+deadwtsge+rtwtsge)
!     &         *pltpop*10.0)
!              WRITE (fnumwrk,'(A22,F7.1)')'  Assim-R-S-Diff > SGE',
!     &        ((carbogf-respgf)*pltpop*10.0) - sengf*pltpop*10.0 -
!     &        (((lfwt+stwt+rswt+grwt+SENTOPRETAINED+rtwt)*pltpop*10.0) -
!     &        ((lfwtsge+stwtsge+rswtsge+grwtsge+deadwtsge+rtwtsge)
!     &        *pltpop*10.0))
!            ENDIF

!            WRITE (fnumwrk,*) ' '
!            WRITE (fnumwrk,'(A21,A10,I3)')
!     &       ' RESERVES STATUS FOR ',excode,tn
!            WRITE (fnumwrk,'(A22,F7.2)')'  Reserves coeff      ',
!     &         RSPCA
!            WRITE (fnumwrk,'(A22,F7.1)')'  Kg/ha at anthesis   ',
!     &       RSWTA*PLTPOP*10.0
!            IF (rswta+stwta+lfwta.GT.0) WRITE (fnumwrk,'(A22,F7.1)')
!     &      '  % above ground      ',rswta/(rswta+stwta+lfwta)*100.0
!            IF (stwta.GT.0) WRITE (fnumwrk,'(A22,F7.1)')
!     &      '  % stem+reserves     ',rswta/(rswta+stwta)*100.0
!            WRITE (fnumwrk,'(A22,F7.1)')'  Kg/ha at grain set  ',
!     &      RSWTAE*PLTPOP*10.0
!            IF (rswtae+stwtae+lfwtae.GT.0) 
!     &       WRITE(fnumwrk,'(A22,F7.1)') '  % above ground      ',
!     &       rswtae/(rswtae+stwtae+lfwtae)*100.
!            WRITE (fnumwrk,'(A22,F7.1)')'  Kg/ha at end stem   ',
!     &      RSWTSGE*PLTPOP*10.0
!            IF ((rswtsge+stwtsge+lfwtsge).GT.0)
!     &      WRITE(fnumwrk,'(A22,F7.1)') '  % above ground      ',
!     &      rswtsge/(rswtsge+stwtsge+lfwtsge)*100.
!            IF (stwtsge.GT.0) WRITE (fnumwrk,'(A22,F7.1)')
!     &        '  Pc stem+reserves    ',rswtsge/(rswtsge+stwtsge)*100.0
!            WRITE (fnumwrk,'(A22,F7.1)')
!     &       '  Kg/ha at maximum    ',RSWTX*PLTPOP*10.0
!            WRITE (fnumwrk,'(A22,F7.1)')
!     &       '  % above ground      ',RSCX*100.
!            WRITE (fnumwrk,'(A22,F7.1)')
!     &       '  Kg/ha at maturity   ',RSWAD
!            IF (lfwt+stwt+rswt.GT.0) WRITE (fnumwrk,'(A22,F7.1)')
!     &       '  % above ground      ',rswt/(lfwt+stwt+rswt)*100.0
!            WRITE (fnumwrk,*) ' '
!            WRITE (fnumwrk,'(A34,A10,I3)')
!     &       ' SEED USE (KG/HA or PER CENT) FOR ',excode,tn
!            WRITE (fnumwrk,'(A22,F7.3)')'  Initial reserves    ',
!     &       seedrsi*pltpop*10.0
!            WRITE (fnumwrk,'(A22,F7.3)')'  Use for tops        ',
!     &       seeduset*pltpop*10.0
!            WRITE (fnumwrk,'(A22,F7.3)')'  Use for roots       ',
!     &       seeduser*pltpop*10.0
!            WRITE (fnumwrk,'(A22,F7.3)')'  Total use           ',
!     &       (seeduset+seeduser)*pltpop*10.0
!            IF (seeduser+seeduset.GT.0.0)
!     &       WRITE (fnumwrk,'(A22,F7.3)')'  Percent to tops     ',
!     &        seeduset/(seeduset+seeduser)*100.0
!            WRITE(fnumwrk,*)' '
!            WRITE (fnumwrk,'(A35,A10,I3)')
!     &       ' DEAD MATTER AND ROOTS (KG/HA) FOR ',excode,tn
!            WRITE(fnumwrk,'(A32,F8.1)')
!     &       '  DEAD MATERIAL LEFT ON SURFACE  ',SENTOPLITTERA
!            WRITE(fnumwrk,'(A32,F8.1)')
!     &       '  DEAD MATERIAL LEFT IN SOIL     ',SENROOTA
!            WRITE(fnumwrk,'(A32,F8.1)')
!     &       '  ROOT WEIGHT AT HARVEST/FAILURE ',RWAD
!            WRITE (fnumwrk,*) ' '
!            WRITE (fnumwrk,'(A20,A10,I3)')
!     &       ' ROOTS BY LAYER FOR ',excode,tn
!            WRITE (fnumwrk,'(A19)')
!     &       '  LAYER  RTWT   RLV'
!            DO L=1,NLAYR
!              IF (RTWTAL(L).GT.0.0) WRITE (fnumwrk,'(I6,F7.1,F6.2)')
!     &        L,RTWTAL(L),RLV(L)
!            ENDDO
!            IF (RTSLXDATE.GT.0) THEN
!              WRITE(fnumwrk,'(A30,I7)')
!     &         '  FINAL SOIL LAYER REACHED ON ',RTSLXDATE
!              WRITE(fnumwrk,'(A15,I7,A1)')
!     &         '  (MATURITY ON ',YEARDOY,')'
!            ELSE  
!             WRITE(fnumwrk,*)' FINAL SOIL LAYER NOT REACHED '
!            ENDIF
!            WRITE (fnumwrk,*) ' '
!            WRITE (fnumwrk,'(A40)')
!     &       ' PRINCIPAL AND SECONDARY STAGES         '
!            WRITE (fnumwrk,'(A40)')
!     &       '  STAGE NAME   DAYS > PLANTING          '
!            WRITE (fnumwrk,'(A15,F7.1)')
!     &       '   Germination ',gdapfr
!            WRITE (fnumwrk,'(A15,F7.1)')
!     &       '   Emergence   ',edapfr
!            IF (CROP.EQ.'WH'.OR.CROP.EQ.'BA') THEN
!              WRITE (fnumwrk,'(A15,I7)')
!     &        '   Tillering   ',tildap
!            ENDIF 
            DO L = 2,PSNUM
              CALL CSUCASE (PSNAME(L))
              IF (PSNAME(L)(1:3).EQ.'HAR'.AND.PSDAPFR(l).LE.0.0)
     &         psdapfr(l) = psdapfr(mstg)
              IF (PSNAME(L)(1:3).EQ.'END'.AND.PSDAPFR(l).LE.0.0)
     &         psdapfr(l) = psdapfr(mstg)
!              IF (PSNAME(L)(1:3).NE.'FAI') THEN
!                IF (psdapfr(l).GT.0) WRITE (FNUMWRK,'(A3,A13,F6.1)')
!     &            '   ',psname(l),psdapfr(l)
!              ELSE
!                IF (CFLFAIL.EQ.'Y'.AND.psdapfr(l).GT.0)
!     &           WRITE (FNUMWRK,'(A3,A13,F6.1)')
!     &             '   ',psname(l),psdapfr(l)
!              ENDIF
              IF (TVILENT(PSNAME(L)).LT.5) EXIT
            ENDDO
!            IF (SSNUM.GT.0) WRITE (FNUMWRK,*) ' '
            DO L = 1,SSNUM
!             IF (ssdapfr(l).GT.0.0)
!     &        WRITE(FNUMWRK,'(A3,A13,F6.1)')'   ',ssname(l),ssdapfr(l)
              IF (TVILENT(SSNAME(L)).LT.5) EXIT
            ENDDO
!            WRITE (fnumwrk,*) ' '
!            WRITE (fnumwrk,'(A28,A10,I3)')
!     &       ' STRESS FACTOR AVERAGES FOR ',excode,tn
!            WRITE (fnumwrk,'(A55)')
!     &       '  PHASE  H2O(PS)   H2O(GR)   N(PS)     N(GR)  PHASE_END'
!            DO L=1,PSNUM
!              IF (STGYEARDOY(L).LT.9999999.AND.
!     &         L.NE.1.AND.L.NE.10.AND.L.NE.11)
!     &         WRITE (fnumwrk,'(I6,F8.2,3F10.2,2X,A13)')
!     &         l,1.0-wfppav(l),1.0-wfgpav(l),
!     &         1.0-nfppav(l),1.0-nfgpav(l),psname(MIN(L+1,PSX))
!            ENDDO
!            WRITE (fnumwrk,'(A42)')
!     &       '  NB 0.0 = minimum ; 1.0 = maximum stress.'
            ! LAH  Must change from daily leaf cohorts to bigger unit
            ! Too much to output if daily
            !WRITE (fnumwrk,*)' '
            !WRITE (fnumwrk,'(A23)') ' COHORT   AREA    AREAS'
            !DO L = 1, LCNUM
            !  WRITE (fnumwrk,'(I7,2F8.3)') L,LCOA(L),LCOAS(L)
            !ENDDO
!            WRITE (fnumwrk,*) ' '
!            WRITE (fnumwrk,'(A18,A10,I3)')
!     &       ' TILLER SIZES FOR ',excode,tn
!            WRITE (fnumwrk,'(A25,F6.1)')
!     &       '   MAXIMUM TILLER NUMBER ',tnumx
!            WRITE (fnumwrk,'(A31)') '   TILL   BIRTH   AREAP   AREAS'
!            DO I = 1,INT(TNUMX)
!              WRITE (fnumwrk,'(I7,3I8)')
!     &          I,NINT(TILBIRTHL(I)),NINT(TLA(I)),NINT(TLAS(I))
!            ENDDO
            IF (INT(TNUMX-FLOAT(INT(TNUMX))).GT.0) THEN
              I = INT(TNUMX) + 1
!              WRITE (fnumwrk,'(I7,3I8)')
!     &          I,NINT(TILBIRTHL(I)),NINT(TLA(I)),NINT(TLAS(I))
            ENDIF
            
            IF (ISWNIT.NE.'N') THEN
!            WRITE (fnumwrk,*) ' '
!            WRITE (fnumwrk,'(A25,A10,I3)')' N BALANCE (kg/ha) FOR ',
!     &       excode,tn
!            WRITE (fnumwrk,'(A34,F8.2,2F11.2)')
!     &       '   N UPTAKE + SEED (1)            ', 
!     &       (NUPC+SEEDNI)*PLTPOP*10.0,NUPC*PLTPOP*10.,SEEDNI*PLTPOP*10.
             TVR1 = (NUPC+SEEDNI)*PLTPOP*10.0  
!            WRITE (fnumwrk,'(A33,F9.2,2F11.2)')
!     &       '   TOTAL N SENESCED (2) Tops,Root',
!     &       (SENNL(0)+SENNS)*PLTPOP*10.0,SENNL(0)*PLTPOP*10.0,
!     &       SENNS*PLTPOP*10.0
             TVR2 = (SENNL(0)+SENNS)*PLTPOP*10.0 
!            WRITE (fnumwrk,'(A34,F8.2)')
!     &       '   N IN DEAD MATTER               ', PLTPOP*10.0*DEADN
!            WRITE (fnumwrk,'(A34,F8.2)')
!     &       '   TOTAL N IN PLANT (3)           ',PLTPOP*10.0*
!     &       (ROOTN+LEAFN+STEMN+RSN+GRAINN+SEEDN+DEADN)
             TVR3 = (ROOTN+LEAFN+STEMN+RSN+GRAINN+SEEDN+DEADN)*
     &              PLTPOP*10.0         
!            WRITE (fnumwrk,'(A33, F9.2)')
!     &       '   HARVESTED DURING CYCLE (4)    ',PLTPOP*10.0*
!     &       LNPHC+SNPHC+RSNPHC+GNPHC
             TVR4 = (LNPHC+SNPHC+RSNPHC+GNPHC)* PLTPOP*10.0             
!            WRITE (fnumwrk,'(A34,F8.3)')
!     &       '   BALANCE (1-(2+3+4))            ',TVR1-TVR2-TVR3-TVR4
!            IF (TVR1-(TVR2+TVR3+TVR4).GT.0.005)
!     &      WRITE(fnumwrk,'(A26,A10,A1,I2)')
!     &       '   PROBLEM WITH N BALANCE ',EXCODE,' ',TN
            ENDIF
            ! End of Detailed WORK writes
    
            ! Phenology (Simulated;PHENOLS.OUT)
            INQUIRE (FILE = FNAMEPHENOLS,EXIST = FFLAG)
            OPEN(UNIT=FNUMPHES,FILE=FNAMEPHENOLS,POSITION='APPEND')
            IF (CROP.NE.CROPPREV.OR.RUN.EQ.1.OR.(.NOT.(FFLAG))) THEN
              WRITE (FNUMPHES,'(/,A14,A10)')
     &         '*PHENOLOGY(S):',EXCODE
              WRITE (FNUMPHES,'(A16,A24)',ADVANCE='NO') 
     &         '@ EXCODE    TRNO',' PYEAR  PDAT  GDAP  EDAP'
              DO L = 1,KEYSTX
                IF (KEYPS(L).GT.0)THEN
                  WRITE (FNUMPHES,'(A6)',ADVANCE='NO') PSABVO(KEYPS(L))
                ENDIF
              ENDDO
              DO L = 1,KEYSTX
                IF (KEYSS(L).GT.0) WRITE (FNUMPHES,'(A6)',ADVANCE='NO')
     &           SSABVO(KEYSS(L))
              ENDDO
              WRITE (fnumphes,'(/,A10,I6,2I6,2F6.1)',ADVANCE='NO')
     &        EXCODE,TN,PLYEAR,PLDAY,gdapfr,edapfr
            ELSE  ! End Phenology simulated header writes
              WRITE (fnumphes,'(A10,I6,2I6,2F6.1)',ADVANCE='NO')
     &        EXCODE,TN,PLYEAR,PLDAY,gdapfr,edapfr
            ENDIF
            DO L = 1,KEYSTX
              IF (KEYPS(L).GT.0) WRITE (FNUMPHES,'(I6)',ADVANCE='NO')
     &         PSDAP(KEYPS(L))
            ENDDO
            DO L = 1,KEYSTX
             IF (KEYSS(L).GT.0) WRITE (FNUMPHES,'(I6)',ADVANCE='NO')
     &         SSDAP(KEYSS(L))
            ENDDO
            CLOSE (FNUMPHES)
            ! End Phenology simulated writes              
            
            ! Phenology (Measured;PHENOLM.OUT)
            IF (TDATANUM.LE.0 .AND. .NOT.FEXISTA) THEN
!              WRITE (fnumwrk,*)' '
!              WRITE (fnumwrk,*)
!     &          ' No data so cannot write PHENOLOGY (MEASURED)'
              OPEN (UNIT=FNUMPHEM,FILE=FNAMEPHENOLM,STATUS ='UNKNOWN')
              CLOSE (UNIT=FNUMPHEM, STATUS = 'DELETE')
            ELSE
              INQUIRE (FILE = FNAMEPHENOLM,EXIST = FFLAG)
              OPEN(UNIT=FNUMPHEM,FILE=FNAMEPHENOLM,POSITION='APPEND')
              IF (CROP.NE.CROPPREV.OR.RUN.EQ.1.OR.(.NOT.(FFLAG))) THEN
                WRITE (FNUMPHEM,'(/,A14,A10)')
     &            '*PHENOLOGY(M):',EXCODE
                WRITE (FNUMPHEM,'(A16,A24)',ADVANCE='NO') 
     &            '@EXCODE     TRNO',' PYEAR  PDAT  GDAP  EDAP'
                DO L = 1,KEYSTX
                  IF (KEYPS(L).GT.0) THEN
                    WRITE (FNUMPHEM,'(A6)',ADVANCE='NO')PSABVO(KEYPS(L))
                  ENDIF 
                ENDDO
                DO L = 1,KEYSTX
                  IF (KEYSS(L).GT.0) WRITE(FNUMPHEM,'(A6)',ADVANCE='NO')
     &             SSABVO(KEYSS(L))
                ENDDO
                WRITE (FNUMPHEM,'(/,A10,I6,2I6,2F6.1)',ADVANCE='NO')
     &           EXCODE,TN,PLYEAR,PLDAY,gdapfr,edapfr
              ELSE ! End Phenology measured header writes
                WRITE (FNUMPHEM,'(A10,I6,2I6,2F6.1)',ADVANCE='NO')
     &           EXCODE,TN,PLYEAR,PLDAY,gdapfr,edapfr
              ENDIF
              DO L = 1,KEYSTX
                IF (KEYPS(L).GT.0) WRITE (FNUMPHEM,'(I6)',ADVANCE='NO')
     &           PSDAPM(KEYPS(L))
              ENDDO
              DO L = 1,KEYSTX
                IF (KEYSS(L).GT.0) WRITE (FNUMPHEM,'(I6)',ADVANCE='NO')
     &           SSDAPM(KEYSS(L))
              ENDDO
              CLOSE (FNUMPHEM)
            ENDIF  
            ! End Phenology (Measured)

            ! Plant responses (Simulated)'
            ! Set temporary planting date for overlapping year end
            IF (RUNCRP.EQ.1) PLDAYTMP = -99
            IF (PLDAY.LT.PLDAYTMP) THEN
              IF (VARNO.EQ.VARNOPREV) THEN
                PLDAYTMP = PLDAY + 365
              ELSE
                PLDAYTMP = PLDAY
              ENDIF
            ELSE
              PLDAYTMP = PLDAY
            ENDIF
            PLDAYTMP = PLDAY
            IF (EXCODE.NE.EXCODEPREV.OR.TNAME(1:1).EQ.'*') THEN
              OPEN (UNIT=FNUMPRES,FILE=FNAMEPRES,POSITION='APPEND')
              WRITE (FNUMPRES,*) ' '
              WRITE (TLINETMP,9951) EXCODE,MODNAME
 9951         FORMAT ('*RESPONSES(S):',A10,'  ',A8)
              IF (TNAME(1:1).EQ.'*') THEN
                WRITE (FNUMPRES,'(A180)') TLINETMP
              ELSE
                WRITE (FNUMPRES,'(A180)') TLINETMP
              ENDIF
              WRITE (FNUMPRES,97,ADVANCE='NO')
   97         FORMAT ('@  RUN',
     A        ' EXCODE   ',
     B        ' TRNO RN    CR',
     D        '  PDAT  EDAP')
              DO L = 1,KEYSTX
                IF (KEYPS(L).GT.0) THEN
                  WRITE (FNUMPRES,'(A6)',ADVANCE='NO') PSABVO(KEYPS(L))
                ENDIF  
              ENDDO
              WRITE (FNUMPRES,297)
  297         FORMAT (
     G        '  HWAM  HWUM',
     H        '  H#AM  H#GM  LAIX  L#SM  T#AM',
     I        '  CWAM  VWAM  HIAM  RWAM',
     J        '  HN%M  TNAM',
     K        '  CNAM  HNAM',
     L        '  HINM PLPOP',
     F        '  NICM WFPAV NFPAV',
     M        ' SRADA TMEAN TMAXA TMINA  PRCP')
            ELSE
              OPEN (UNIT=FNUMPRES,FILE=FNAMEPRES,POSITION='APPEND')
            ENDIF  ! End Responses simulated header writes
            WRITE (FNUMPRES,7401,ADVANCE='NO') RUN,EXCODE,TN,RN,CROP,
     A       PLDAYTMP,EDAPFR
            DO L = 1,KEYSTX
              IF (KEYPS(L).GT.0) WRITE (FNUMPRES,'(I6)',ADVANCE='NO')
     &        PSDAP(KEYPS(L))
            ENDDO
            WRITE (fnumpres,409)
     D       NINT(hwam),hwumchar,
     E       NINT(hnumam),NINT(hnumgm),
     F       laixchar,lnumsm,NINT(tnumam),
     G       NINT(cwam),NINT(vwam),
     H       hiamchar,
     I       NINT(rwam),
     J       hnpcmchar,
     K       NINT(AMAX1(-99.0,cnam+rnam)),NINT(cnam),NINT(gnam),
     L       hinmchar,pltpop,
     C       NINT(amtnit),
     M       1.0-wfpcav,1.0-nfpcav,
!    M       sradcav,tmeancav,tmaxcav,tmincav,   ! For crop cycle
     M       sradpav(1),tmeanpav(1),tmaxpav(1),tminpav(1), !For phase ?
     N       NINT(raincc)            
 7401       FORMAT (I6,1X,A10,I4,I3,4X,A2,     !Run,excode,tn,rn,crop
     A       I6,                               !Pldaytmp
     B       F6.1)                             !Edapfr
  409       FORMAT (
     D       I6,A6,                            !gwam,hwum
     E       I6,I6,                            !g#am,g#gm
     F       A6,F6.1,I6,                       !laix,lnumsm,tnumam
     G       I6,I6,                            !Cwam,vwam
     H       A6,                               !hiam
     I       I6,                               !N(rwam)
     J       A6,                               !hnpcm
     K       I6,I6,I6,                         !N(cnam+rnam),(cn),(gn)
     L       A6,F6.1,                          !hinm,pltpop,tnumpm
     C       I6,                               !Amtnit
     M       2F6.1,                            !wfpcav,nfpcav
     M       4F6.1,                            !RAD,TX,TN
     N       I6)                               !RN   
            CLOSE(FNUMPRES)
            ! End Responses simulated writes
            
            ! Plant responses (Measured)
            IF (TDATANUM.LE.0 .AND. .NOT.FEXISTA) THEN
!              WRITE (fnumwrk,*)' '
!              WRITE (fnumwrk,*)
!     &         ' No data so cannot write PLANT RESPONSES (MEASURED)'
              OPEN (UNIT = FNUMTMP,FILE = FNAMEPREM,STATUS='UNKNOWN')
              CLOSE (UNIT=FNUMTMP, STATUS = 'DELETE')
            ELSE
              IF (EXCODE.NE.EXCODEPREV.OR.TNAME(1:1).EQ.'*') THEN
                OPEN (UNIT=FNUMPREM,FILE=FNAMEPREM,POSITION='APPEND')
                WRITE (FNUMPREM,*) ' '
                WRITE (TLINETMP,99511) EXCODE,MODNAME
99511           FORMAT ('*RESPONSES(M):',A10,'  ',A8)
                IF (TNAME(1:1).EQ.'*') THEN
                  WRITE (FNUMPREM,'(A180)') TLINETMP
                ELSE
                  WRITE (FNUMPREM,'(A180)') TLINETMP
                ENDIF
                WRITE (FNUMPREM,97,ADVANCE='NO')
                DO L = 1,KEYSTX
                  IF (KEYPS(L).GT.0) THEN
                   WRITE (FNUMPREM,'(A6)',ADVANCE='NO') PSABVO(KEYPS(L))
                  ENDIF 
                ENDDO
                WRITE (FNUMPREM,298)
  298         FORMAT (
     G        '  HWAM  HWUM',
     H        '  H#AM  H#GM  LAIX  L#SM  T#AM',
     I        '  CWAM  VWAM  HIAM  RWAM',
     J        '  HN%M  TNAM',
     K        '  CNAM  HNAM',
     L        '  HINM PLPOP',
     F        '  NICM WFPAV NFPAV',
     M        ' SRADA TMEAN TMAXA TMINA  PRCP')
              ELSE
                OPEN (UNIT=FNUMPREM,FILE=FNAMEPREM,POSITION='APPEND')
              ENDIF ! End Responses measured header writes
              WRITE (FNUMPREM,7401,ADVANCE='NO') RUN,EXCODE,TN,RN,CROP,
     A         PLDAYTMP,FLOAT(MAX(-99,edapm))
              DO L = 1,KEYSTX
                IF (KEYPS(L).GT.0) WRITE (FNUMPREM,'(I6)',ADVANCE='NO')
     &           PSDAPM(KEYPS(L))
              ENDDO
              WRITE (FNUMPREM,409)
     F         NINT(hwamm),hwummchar,
     G         NINT(hnumamm),NINT(hnumgmm),
     H         laixmchar,lnumsmm,NINT(tnumamm), 
     I         NINT(cwamm),NINT(vwamm),hiammchar,
     J         NINT(rwamm),
     K         hnpcmmchar,NINT(tnamm),NINT(cnamm),NINT(hnamm),
     L         hinmmchar,pltpopp,
     F         NINT(amtnit),
     M         1.0-wfpcav,1.0-nfpcav,
!    M         sradcav,tmeancav,tmaxcav,tmincav,               ! Cycle
     M         sradpav(1),tmeanpav(1),tmaxpav(1),tminpav(1),   ! Phase ?
     N         NINT(raincc)            
              CLOSE(FNUMPREM)
            ENDIF  
            ! End Responses (Measured)
              
          ELSE  ! IDETL = 'D'

            OPEN (UNIT=FNUMPHES,FILE=FNAMEPHENOLS,STATUS = 'UNKNOWN')
            CLOSE (UNIT=FNUMPHES, STATUS = 'DELETE')
            OPEN (UNIT=FNUMPHEM,FILE=FNAMEPHENOLM,STATUS = 'UNKNOWN')
            CLOSE (UNIT=FNUMPHEM, STATUS = 'DELETE')
            OPEN (UNIT = FNUMPRES,FILE = FNAMEPRES,STATUS='UNKNOWN')
            CLOSE (UNIT=FNUMPRES, STATUS = 'DELETE')
            OPEN (UNIT = FNUMPREM,FILE = FNAMEPREM,STATUS='UNKNOWN')
            CLOSE (UNIT=FNUMPREM, STATUS = 'DELETE')
          
          ENDIF ! IDETL = 'D'

!-----------------------------------------------------------------------
!         IDETL = A OUTPUTS (Errora,Errort,Errors)
!-----------------------------------------------------------------------

          IF (IDETL.EQ.'A') THEN     ! Write some error outputs
          
            ! Find intermediate stage dates
            DO L = MSTG-1,1,-1
              IF (psdapm(l).GT.0.0) THEN
                psidapm = psdapm(l)
                EXIT
              ENDIF
            ENDDO
            IF (L.EQ.0) L = INT((FLOAT(MSTG)/2.0)+1)
            IF (L.GT.0) THEN
              PSIDAP = PSDAP(L)
            ELSE
!              WRITE (fnumwrk,*)' '
!              WRITE (fnumwrk,*)' Problem in finding intermediate stage '
!              WRITE (fnumwrk,*)'  Mature stage       = ',mstg          
!              WRITE (fnumwrk,*)'  Intermediate stage = ',l             
!              WRITE (fnumwrk,*)' '
            ENDIF
            
            ! Errors (A-data)
            IF (TDATANUM.LE.0 .AND. .NOT.FEXISTA) THEN
!              WRITE (fnumwrk,*)' '
!              WRITE (fnumwrk,*)' No data so cannot write PLANTERA'
              OPEN (UNIT=FNUMTMP,FILE=FNAMEERA,STATUS='UNKNOWN')
              CLOSE (UNIT=FNUMTMP, STATUS = 'DELETE')
            ELSE ! If data availabe
              IF (edapm.GT.0) THEN
               emdaterr = 100.0*(Edap-Edapm)/edapm
              ELSE
               emdaterr = -99
              Endif
              IF (adapm.GT.0) THEN
               adaterr = 100.0*(adap-adapm)/adapm
              ELSE
               adaterr = -99
              Endif
              IF (psidapm.GT.0) THEN
               psidaterr = 100.0*(psidap-psidapm)/psidapm
              ELSE
               psidaterr = -99
              Endif
              IF (mdatm.GT.0) THEN
               mdaterr = 100.0*(mdap-mdapm)/mdapm
              ELSE
               mdaterr = -99
              Endif
              IF (hwahm.GT.0.AND.hwam.GT.0.AND.hpcf.GT.0) THEN
               hwaherr = 100.*(hwam*hpcf/100.-hwahm)/(hwahm*hpcf/100.)
               IF (hwaherr.GT.99999.0) hwaherr = 99999.0
               IF (hwaherr.LT.-9999.0) hwaherr = -9999.0
              ELSE
               hwaherr = -99
              ENDIF
              IF (hwumm.GT.0.AND.hwum.GT.0) THEN
               hwumerr = 100.0*(hwum-hwumm)/hwumm
              ELSE
               hwumerr = -99
              ENDIF
              IF (hnumamm.GT.0.AND.hnumam.GT.0) THEN
               hnumaerr = 100.0*(hnumam-hnumamm)/(hnumamm)
              ELSE
               hnumaerr = -99
              ENDIF
              IF (hnumgmm.GT.0.AND.hnumgm.GT.0) THEN
               hnumgerr = 100.0*((hnumgm-hnumgmm)/hnumgmm)
              ELSE
               hnumgerr = -99
              ENDIF
              IF (laixm.GT.0.AND.laix.GT.0) THEN
               laixerr = 100.0*((laix-laixm)/laixm)
              ELSE
               laixerr = -99
              ENDIF
              IF (lnumsmm.GT.0.AND.lnumsm.GT.0) THEN
               lnumserr = 100.0*((lnumsm-lnumsmm)/lnumsmm)
              ELSE
               lnumserr = -99
              ENDIF
              IF (tnumamm.GT.0.AND.tnumam.GT.0) THEN
               tnumaerr = 100.0*((tnumam-tnumamm)/tnumamm)
              ELSE
               tnumaerr = -99
              Endif
              IF (cwamm.GT.0.AND.cwam.GT.0) THEN
               cwamerr = 100.0*(cwam-cwamm)/cwamm
              ELSE
               cwamerr = -99
              Endif
              IF (vwamm.GT.0.AND.vwam.GT.0) THEN
               vwamerr = 100.0*(vwam-vwamm)/vwamm
              ELSE
               vwamerr = -99
              Endif
              IF (hiamm.GT.0.AND.hiam.GT.0) THEN
               hiamerr = 100.0*(hiam-hiamm)/hiamm
              ELSE
               hiamerr = -99
              Endif
              IF (hnpcmm.GT.0.AND.hnpcm.GT.0) THEN
               hnpcmerr = 100.0*(hnpcm-hnpcmm)/hnpcmm
              ELSE
               hnpcmerr = -99
              Endif
              IF (cnamm.GT.0.AND.cnam.GT.0) THEN
               cnamerr = 100.0*(cnam-cnamm)/cnamm
              ELSE
               cnamerr = -99
              Endif
              IF (gnamm.GT.0.AND.gnam.GT.0) THEN
               hnamerr = 100.0*(hnam-hnamm)/hnamm
              ELSE
               hnamerr = -99
              Endif
              IF (RUN.EQ.1) THEN
                OPEN (UNIT=FNUMERA,FILE=FNAMEERA,POSITION='APPEND')
                WRITE (FNUMERA,996)
  996           FORMAT (/,'*ERRORS(A)',/)
                WRITE (FNUMERA,896)
  896           FORMAT ('@  RUN',
     A          ' EXCODE     ',
     B          '  TRNO RN',
     C          '    CR',
     D          '    EDAP   EDAPE',
     E          '    ADAP   ADAPE',
     F          '    MDAP   MDAPE',
     G          '    HWAH   HWAHE',
     H          '    HWUM   HWUME',
     I          '    H#AM   H#AME',
     J          '    H#GM   H#GME',
     K          '    LAIX   LAIXE',
     L          '    L#SM   L#SME',
     M          '    S#AM   S#AME',
     N          '    CWAM   CWAME',
     O          '    VWAM   VWAME',
     P          '    HIAM   HIAME',
     Q          '    HN%M   HN%ME',
     R          '    CNAM   CNAME',
     S          '    HNAM   HNAME')
                CLOSE(FNUMERA)
              ENDIF  ! End ErrorA header writes
              OPEN (UNIT = FNUMERA,FILE = FNAMEERA,POSITION = 'APPEND')
              WRITE (FNUMERA,8401) RUN,EXCODE,TN,RN,CROP,
     A         Edap,emdaterr,
     B         adap,adaterr,
     C         mdap,mdaterr,
     D         NINT(hwam),NINT(hwaherr),
     E         hwum,NINT(hwumerr),
     F         NINT(hnumam),NINT(hnumaerr),
     G         hnumgm,NINT(hnumgerr),
     H         laix,NINT(laixerr),
     I         lnumsm,NINT(lnumserr),
     J         NINT(tnumam),NINT(tnumaerr),
     K         NINT(cwam),NINT(cwamerr),
     L         NINT(vwam),NINT(vwamerr),
     M         hiam,NINT(hiamerr),
     N         hnpcm,NINT(hnpcmerr),
     O         NINT(cnam),NINT(cnamerr),
     P         NINT(hnam),NINT(hnamerr)
 8401         FORMAT (I6,1X,A10,1X,I6,I3,4X,A2,
     A         I8,  I8,
     B         I8,  I8,
     C         I8,  I8,
     D         I8,  I8,
     E         F8.3,I8,
     F         I8,  I8,
     G         F8.1,I8,
     H         F8.1,I8,
     I         F8.1,I8,
     J         I8  ,I8,
     K         I8,  I8,
     L         I8,  I8,
     M         F8.2,I8,
     N         F8.1,I8,
     O         I8,  I8,
     P         I8,  I8)
              CLOSE(FNUMERA)
            ENDIF ! End ErrorA writes (If data available)
          
            ! Errors (T)
            IF (.NOT.FEXISTT .OR. FROPADJ.GT.1 .OR. IDETG.EQ.'N') THEN
!              WRITE (fnumwrk,*) ' '
              IF (FROPADJ.GT.1) THEN
!                WRITE (fnumwrk,*) ' Cannot write PLANT ERRORS (T).',
!     &          ' Frequency of output > 1 day'
              ELSE  
!                WRITE (fnumwrk,*)
!     &          ' No data so cannot write PLANT ERRORS (T)'
              ENDIF      
              OPEN (UNIT=FNUMTMP,FILE=FNAMEERT,STATUS='UNKNOWN')
              CLOSE (UNIT=FNUMTMP, STATUS = 'DELETE')
            ELSE
              INQUIRE (FILE = OUTPG,OPENED = FOPEN)
              IF (FOPEN) CLOSE (NOUTPG)
              STARNUM = 0
              OPEN (UNIT=FNUMT,FILE='Measured.out',STATUS='UNKNOWN')
              DO WHILE (TLINET(1:1).NE.'@')
                TLINET = ' '
                READ (FNUMT,1502,END=1600,ERR=1600) TLINET
 1502           FORMAT(A180)
                IF (TLINET(1:1).EQ.'*') STARNUM = STARNUM + 1
                IF (TLINET(1:1).EQ.'@') THEN
                  IF (STARNUM.NE.STARNUMM) THEN
                    TLINET = ' '
                    READ (FNUMT,1502,END=1600,ERR=1600) TLINET
                  ENDIF
                ENDIF
              ENDDO
              tlinet(1:1) = ' '
              STARNUM = 0
              OPEN (UNIT=NOUTPG,FILE=OUTPG,STATUS='UNKNOWN')
              DO WHILE (TLINEGRO(1:1).NE.'@')
                TLINEGRO = ' '
                READ (NOUTPG,'(A254)') TLINEGRO
                IF (TLINEGRO(1:4).EQ.'*RUN') STARNUM = STARNUM + 1
                IF (TLINEGRO(1:1).EQ.'@') THEN
                  IF (STARNUM.NE.STARNUMO) THEN
                    TLINEGRO = ' '
                    READ (NOUTPG,'(A254)') TLINEGRO
                  ENDIF
                ENDIF
              ENDDO
              tlinegro(1:1) = ' '
              ! Find headers from Measured file
              DO L = 1,20
                CALL Getstr(tlinet,l,thead(l))
                IF (THEAD(L)(1:3).EQ.'-99') EXIT
                IF (THEAD(L)(1:3).EQ.'DAP') tfdapcol = l
              ENDDO
              TFCOLNUM = L-1
              IF (TFCOLNUM.LE.0) THEN
!                WRITE (FNUMWRK,*) 'No columns found in T-file '
                GO TO 7777
              ENDIF
              ! Make new header line
              TLINETMP = ' '
              TLINETMP(1:1) = '@'
              DO L = 1, TFCOLNUM
                TLPOS = (L-1)*6+1
                IF (THEAD(L).EQ.'TRNO'.OR.THEAD(L).EQ.'YEAR'.OR.
     &            THEAD(L).EQ.'DATE') THEN
                  TLINETMP(TLPOS+2:TLPOS+5)=THEAD(L)(1:4)
                ELSEIF(THEAD(L).EQ.'DOY'.OR.THEAD(L).EQ.'DAP' .OR.
     &            THEAD(L).EQ.'DAS'.OR.THEAD(L).EQ.'DAY') THEN
                  TLINETMP(TLPOS+3:TLPOS+5)=THEAD(L)(1:3)
                ELSE
                  WRITE (TCHAR,'(I6)') NINT(ERRORVAL*100.0)
                  TLINETMP(TLPOS+1:TLPOS+4) = THEAD(L)(1:4)
                  TLINETMP(TLPOS+5:TLPOS+5) = 'E'
                ENDIF
              ENDDO
              ! Find corresponding columns in PlantGro.OUT
              DO L = 1,TFCOLNUM
                pgrocol(l) = Tvicolnm(tlinegro,thead(l))
              ENDDO
              OPEN (UNIT=FNUMERT,FILE=FNAMEERT,POSITION='APPEND')
              WRITE (FNUMERT,2996) OUTHED(12:79)
 2996         FORMAT (/,'*ERRORS(T):',A69,/)
              tlinet(1:1) = '@'
              WRITE (FNUMERT,'(A180)') TLINETMP
              ! Read data lines, match dates, calculate errors, write
              DO L1 = 1,200
                TLINET = ' '
                READ (FNUMT,7778,ERR=7777,END=7777) TLINET
 7778           FORMAT(A180)
                IF (TLINET(1:1).EQ.'*') GO TO 7777
                IF (TLINET(1:6).EQ.'      ') GO TO 7776
                CALL Getstri(tlinet,tfdapcol,tfdap)
                IF (TFDAP.LE.0.0) THEN
                  GO TO 7777
                ENDIF
                DO WHILE (tfdap.NE.pgdap)
                  TLINEGRO = ' '
                  READ (NOUTPG,7779,ERR=7777,END=7777) TLINEGRO
                  CALL Getstri(tlinegro,pgrocol(tfdapcol),pgdap)
                  IF (PGDAP.LT.0) THEN
!                    WRITE (FNUMWRK,*) 'DAP in Plantgro file < 0 '
                    GO TO 7777
                  ENDIF
                ENDDO
 7779           FORMAT(A255)
                TLINETMP = ' '
                DO L = 1, TFCOLNUM
                  CALL Getstrr(tlinet,l,tfval)
                  CALL Getstrr(tlinegro,pgrocol(l),pgval)
                  ERRORVAL = 0.0
                  IF(TFVAL.GT.0.0.AND.PGVAL.GT.-99.AND.
     &             PGVAL.NE.0.0)THEN
                    ERRORVAL = 100.0 * (PGVAL - TFVAL) / TFVAL
                  ELSE
                    ERRORVAL = -99.0
                  ENDIF
                  IF (THEAD(L).EQ.'TRNO'.OR.THEAD(L).EQ.'YEAR' .OR.
     &              THEAD(L).EQ.'DOY'.OR.THEAD(L).EQ.'DAP' .OR.
     &              THEAD(L).EQ.'DAY' .OR.
     &              THEAD(L).EQ.'DAS'.OR.THEAD(L).EQ.'DATE') THEN
                    CALL Getstri(tlinet,l,tvi1)
                    WRITE (TCHAR,'(I6)') TVI1
                  ELSE
                    WRITE (TCHAR,'(I6)') NINT(ERRORVAL)
                  ENDIF
                  TLPOS = (L-1)*6+1
                  TLINETMP(TLPOS:TLPOS+5)=TCHAR
                ENDDO
                WRITE (FNUMERT,'(A180)') TLINETMP
 7776           CONTINUE
              ENDDO
 7777         CONTINUE
              GO TO 1601
 1600         CONTINUE
!              WRITE(fnumwrk,*)'End of file reading Measured.out'
!              WRITE(fnumwrk,*)
!     &         'Starnum and starnumm were: ',starnum,starnumm
 1601         CONTINUE
              CLOSE (FNUMERT)
              CLOSE (FNUMT)
              CLOSE (NOUTPG)
              ! Re-open file if open at start of work here
              IF (FOPEN) 
     &        OPEN (UNIT=NOUTPG,FILE=OUTPG,POSITION='APPEND')
            ENDIF  ! .NOT.FEXISTT .OR. FROPADJ.GT.1 .OR. IDETG.EQ.'N'
            ! End of ErrorT writes
          
          ELSE ! No ERROR files called for ... must be deleted          
          
            OPEN (UNIT=FNUMTMP,FILE=FNAMEERA,STATUS='UNKNOWN')
            CLOSE (UNIT=FNUMTMP, STATUS = 'DELETE')
            OPEN (UNIT=FNUMTMP,FILE=FNAMEERT,STATUS='UNKNOWN')
            CLOSE (UNIT=FNUMTMP, STATUS = 'DELETE')
          
          ENDIF ! End of Error writes  IDETL.EQ.'A'

!-----------------------------------------------------------------------
!         IDETD (DISPLAY) OUTPUTS IF WORKING IN CROPSIM SHELL
!-----------------------------------------------------------------------

          ! Screen writes
          ! LAH April 2010 Must check this for Cropsimstand-alone
          IF (IDETD.EQ.'S') THEN
            IF (OUTCOUNT.LE.0) THEN
              CALL CSCLEAR5
              WRITE(*,*)' SIMULATION SUMMARY'
              WRITE(*,*)' '
              WRITE (*,499)
  499         FORMAT ('   RUN EXCODE    TRNO RN',
     X         ' TNAME..................',
     X         '.. REP  RUNI S O C CR  HWAM')
            ENDIF
            IF (OUTCOUNT .EQ. 25) THEN
              OUTCOUNT = 1
            ELSE
              OUTCOUNT = OUTCOUNT + 1
            ENDIF
            WRITE (*,410) run,excode,tn,rn,tname(1:25),
     X      rep,runi,sn,on,cn,crop,NINT(hwam)
  410       FORMAT (I6,1X,A10,I4,I3,1X,A25,
     X      I4,I6,I2,I2,I2,1X,A2,I6)
          ELSEIF (IDETD.EQ.'M') THEN
            ! Simulation and measured data
            CALL CSCLEAR5
            WRITE(*,'(A20,A10,I3)')' STAGES SUMMARY FOR ',EXCODE,TN
            WRITE(*,*)' '
            WRITE(*,9600)
            DO L = 1, PSNUM
              CALL Csopline(laic,laistg(l))
              IF (STGYEARDOY(L).LT.9999999.AND.
     &         L.NE.10.AND.L.NE.11) THEN
!                CALL CSYR_DOY(STGYEARDOY(L),YEAR,DOY)
                CALL YR_DOY(STGYEARDOY(L),YEAR,DOY)
                CALL Calendar(year,doy,dom,month)
                CNCTMP = 0.0
                IF (CWADSTG(L).GT.0.) 
     &            CNCTMP = CNADSTG(L)/CWADSTG(L)*100
                WRITE (*,'(I8,I4,1X,A3,I4,1X,I1,1X,A13,I6,A6,
     &           F6.1,I6,F6.2,F6.2,F6.2)')
     &           STGYEARDOY(L),DOM,MONTH,
     &           Dapcalc(stgyeardoy(L),plyear,plday),L,PSNAME(L),
     &           NINT(CWADSTG(L)),LAIC,LNUMSTG(L),
     &           NINT(CNADSTG(L)),CNCTMP,1.0-WFPPAV(L),1.0-NFPPAV(L)
              ENDIF
            ENDDO
            WRITE(*,*)' '
            WRITE(*,*)' Press ENTER to continue'
            PAUSE ' '
            CALL CSCLEAR5
            WRITE(*,'(A36,A10,I3)')
     &      ' SIMULATED-MEASURED COMPARISONS FOR ',EXCODE,TN
            WRITE(*,*)' '
            WRITE (*,206)
            WRITE (*,290) MAX(-99,gdap),MAX(-99,gdapm),
     A      MAX(-99,edap),MAX(-99,edapm)
            DO L = 1,KEYSTX
              IF (KEYPS(L).GT.0) WRITE (*,291)
     &        psname(KEYPS(L)),PSDap(KEYPS(L)),PSDapm(KEYPS(L))
            ENDDO
            WRITE (*,305)
     x       NINT(cwam),NINT(cwamm),
     x       NINT(rwam+sdwam),NINT(rwamm),
     x       NINT(senwacm),NINT(senwacmm),
     x       NINT(hwam),NINT(hwamm),
     x       NINT(vwam),NINT(vwamm),
     x       hiam,hiamm,
     x       NINT(rswam),NINT(rswamm)
          ENDIF ! End IDETD.EQ.'S' 

!-----------------------------------------------------------------------
!         Screen writes for Dssat sensitivity mode
!-----------------------------------------------------------------------
            
          IF (FILEIOT(1:3).EQ.'DS4' .AND. CN.EQ.1
     &                              .AND. RNMODE.EQ.'E') THEN         
            CALL CSCLEAR5
            WRITE(*,9589)
            WRITE(*,*) ' '
            WRITE(*,9588)
            WRITE(*,9600)
            DO L = 1, PSNUM
              CALL Csopline(laic,laistg(l))
              IF (STGYEARDOY(L).LT.9999999.AND.
     &            L.NE.10.AND.L.NE.11) THEN
!                CALL CSYR_DOY(STGYEARDOY(L),YEAR,DOY)
                CALL YR_DOY(STGYEARDOY(L),YEAR,DOY)
                CALL Calendar(year,doy,dom,month)
                CNCTMP = 0.0
                IF (CWADSTG(L).GT.0.0)
     &           CNCTMP = CNADSTG(L)/CWADSTG(L)*100
                WRITE (*,'(I8,I4,1X,A3,I4,1X,I1,1X,A13,I6,A6,
     &          F6.1,I6,F6.2,F6.2,F6.2)')
     &          STGYEARDOY(L),DOM,MONTH,
     &          Dapcalc(stgyeardoy(L),(plyeardoy/1000),plday),
     &          l,psname(l),
     &          NINT(CWADSTG(L)),LAIC,LNUMSTG(L),
     &          NINT(CNADSTG(L)),CNCTMP,1.0-WFPPAV(L),1.0-NFPPAV(L)
              ENDIF
            ENDDO
            ! For harvest at specified date
            IF (YEARDOYHARF.EQ.YEARDOY) THEN
              CALL Csopline(laic,lai)
!                CALL CSYR_DOY(YEARDOYHARF,YEAR,DOY)
                CALL YR_DOY(YEARDOYHARF,YEAR,DOY)
                CALL Calendar(year,doy,dom,month)
                CNCTMP = 0.0
                IF (CWAD.GT.0.0)CNCTMP = CNAD/CWAD*100
                WRITE (*,'(I8,I4,1X,A3,I4,1X,I1,1X,A13,I6,A6,
     &          F6.1,I6,F6.2,F6.2,F6.2)')
     &          YEARDOY,DOM,MONTH,
     &          Dapcalc(yeardoy,(plyeardoy/1000),plday),
     &          l,'Harvest      ',
     &          NINT(CWAD),LAIC,LNUM,
     &          NINT(CNAD),CNCTMP,1.0-WFPCAV,1.0-NFPCAV
            ENDIF 
          
            WRITE(*,*)' '
            WRITE(*,*)' Press ENTER to continue'
            PAUSE ' '
            CALL CSCLEAR5
          
            !WRITE (*,206)
            WRITE(*,'(A36,A10,I3)')
     &      ' SIMULATED-MEASURED COMPARISONS FOR ',EXCODE,TN
            WRITE(*,*)' '
            WRITE (*,206)
            WRITE (*,290) MAX(-99,gdap),MAX(-99,gdapm),
     A       MAX(-99,edap),MAX(-99,edapm)
            DO L = 1,KEYSTX
              IF (KEYPS(L).GT.0) WRITE (*,291)
     &         psname(KEYPS(L)),PSDap(KEYPS(L)),PSDapm(KEYPS(L))
            ENDDO
            WRITE (*,305)
     H       NINT(cwam),NINT(cwamm),
     I       MAX(-99,NINT(rwam+sdwam)),NINT(rwamm),
     J       NINT(senwacm),NINT(senwacmm),
     L       NINT(hwam),NINT(hwamm),
     M       NINT(vwam),NINT(vwamm),
     N       hiam,hiamm,
     O       NINT(rswam),NINT(rswamm)
            IF (lwphc+swphc.GT.0.0) WRITE (*,3051)
     &       NINT(cwahc),NINT(cwahcm),
     &       NINT(spnumhc*pltpop),MAX(-99,NINT(spnumhcm*pltpop))
          
            WRITE(*,*)' '
            WRITE(*,*)' Press ENTER to continue'
            PAUSE ' '
            CALL CSCLEAR5
          
            WRITE (*,2061)
2061        FORMAT(
     &       /,   
     &       "@",5X,"VARIABLE",T42,"SIMULATED   MEASURED",/,  
     &         6X,"--------",T42,"---------   --------")  
             WRITE (*,3052)
     A       hwumchar,hwummchar,
     B       NINT(hnumam),NINT(hnumamm),
     C       hnumgm,hnumgmm,
     D       NINT(tnumam),NINT(tnumamm),
     E       laix,laixm,
     F       lnumsm,lnumsmm,
     G       nupac,nupacm,
     H       cnam,cnamm,
     I       rnam,rnamm,
     J       sennatc,sennatcm,
     K       hnam,hnamm,
     L       vnam,vnamm,
     M       hinm,hinmm,
     N       hnpcm,hnpcmm,
     O       vnpcm,vnpcmm
          
            WRITE(*,*)' '
            WRITE(*,*)' Press ENTER to continue'
            PAUSE ' '
            CALL CSCLEAR5
            CALL CSCLEAR5
            CALL CSCLEAR5
            CALL CSCLEAR5
          
          ENDIF ! END OF SCREEN WRITES FOR DSSAT SENSITIVITY MODE
        
!-----------------------------------------------------------------------
!         Store variables for sending to CSM summary output routines
!-----------------------------------------------------------------------
        
          ! Store summary labels and values in arrays to send to
          ! CSM OPSUM routine for printing.  Integers are temporarily
          ! saved as real numbers for placement in real array.
          
          IF (IDETO.EQ.'E'.OR.IDETO.EQ.'N') THEN
              ! Resource productivity calculations 
              ! (Not done earlier because no Overview called for)
              DMP_Rain = -99.
              GrP_Rain = -99.
              DMP_ET = -99.
              GrP_ET = -99.
              DMP_EP = -99.
              GrP_EP = -99.
              DMP_Irr = -99.    
              GrP_Irr = -99.
              DMP_NApp = -99.
              GrP_NApp = -99.
              DMP_NUpt = -99.
              GrP_NUpt = -99.
              IF (RAINCC > 1.E-3) THEN
               DMP_Rain = CWAM / RAINCC 
               GrP_Rain = HWAM  / RAINCC
              ENDIF
              IF (ETCC > 1.E-3) THEN
               DMP_ET = CWAM / ETCC 
               GrP_ET = HWAM  / ETCC 
              ENDIF
              IF (EPCC > 1.E-3) THEN
               DMP_EP = CWAM / EPCC 
               GrP_EP = HWAM  / EPCC 
              ENDIF
              IF (IRRAMTC > 1.E-3) THEN
                DMP_Irr = CWAM / IRRAMTC 
                GrP_Irr = HWAM  / IRRAMTC
              ENDIF
              IF (ISWNIT.NE.'N') THEN
                IF (Amtnit > 1.E-3) THEN
                  DMP_NApp = CWAM / Amtnit
                  GrP_NApp = HWAM  / Amtnit
                ENDIF
                IF (NUPAC > 1.E-3) THEN
                  DMP_NUpt = CWAM / NUPAC
                  GrP_NUpt = HWAM  / NUPAC
                ENDIF
              ENDIF ! ISWNIT NE 'N'
!              WRITE (FNUMWRK, 1200) CDAYS, 
!     &         RAINCC, DMP_Rain*0.1, DMP_Rain, GrP_Rain*0.1, GrP_Rain,
!     &         ETCC,  DMP_ET*0.1,   DMP_ET,   GrP_ET*0.1,   GrP_ET, 
!     &         EPCC,  DMP_EP*0.1,   DMP_EP,   GrP_EP*0.1,   GrP_EP
              IF (IRRAMTC > 1.E-3) THEN
!                WRITE(FNUMWRK, 1210) 
!     &            IRRAMTC, DMP_Irr*0.1, DMP_Irr, GrP_Irr*0.1, GrP_Irr
              ENDIF  
!              IF (ISWNIT.NE.'N') THEN
!                IF (Amtnit > 1.E-3) THEN
!                  WRITE(FNUMWRK, 1220) Amtnit, DMP_NApp, GrP_NApp 
!                ENDIF
!                IF (NUPAC > 1.E-3) THEN
!                  WRITE(FNUMWRK, 1230) NUPAC, DMP_NUpt,GrP_NUpt
!                ENDIF
!              ENDIF ! ISWNIT NE 'N'
          ENDIF
          
          LABEL(1) = 'ADAT'; VALUE(1) = FLOAT(adat)
          IF (stgyeardoy(mstg).LT.9999999) THEN
            LABEL(2) = 'MDAT'; VALUE(2) = FLOAT(stgyeardoy(mstg))
          ELSE
            LABEL(2) = 'MDAT'; VALUE(2) = -99.0
          ENDIF
          LABEL(3) = 'DWAP'; VALUE(3) = sdrate
          LABEL(4) = 'CWAM'; VALUE(4) = cwam
          LABEL(5) = 'HWAM'; VALUE(5) = hwam
          LABEL(6) = 'HWAH '; VALUE(6) = hwam * hpcf/100.0
          LABEL(7) = 'BWAH '; VALUE(7) = vwam * hbpcf/100.0
          LABEL(8) = 'HWUM'; VALUE(8) = hwum
          LABEL(9) = 'H#AM'; VALUE(9) = hnumam
          LABEL(10) = 'H#UM'; VALUE(10) = hnumgm
          LABEL(11) = 'NUCM'; VALUE(11) = nupac
          LABEL(12) = 'CNAM'; VALUE(12) = cnam
          LABEL(13) = 'GNAM'; VALUE(13) = gnam
          LABEL(14) = 'PWAM'; VALUE(14) = hwam+chwt*pltpop*10.0
          LABEL(15) = 'LAIX'; VALUE(15) = laix
          LABEL(16) = 'HIAM'; VALUE(16) = hiam
            
          LABEL(17) = 'DMPPM'; VALUE(17) = DMP_Rain 
          LABEL(18) = 'DMPEM'; VALUE(18) = DMP_ET                     
          LABEL(19) = 'DMPTM'; VALUE(19) = DMP_EP                     
          LABEL(20) = 'DMPIM'; VALUE(20) = DMP_Irr
          LABEL(21) = 'DPNAM'; VALUE(21) = DMP_NApp
          LABEL(22) = 'DPNUM'; VALUE(22) = DMP_NUpt
           
          LABEL(23) = 'YPPM ' ; VALUE(23) = GrP_Rain                  
          LABEL(24) = 'YPEM ' ; VALUE(24) = GrP_ET                   
          LABEL(25) = 'YPTM ' ; VALUE(25) = GrP_EP                    
          LABEL(26) = 'YPIM ' ; VALUE(26) = GrP_Irr
          LABEL(27) = 'YPNAM' ; VALUE(27) = GrP_NApp
          LABEL(28) = 'YPNUM' ; VALUE(28) = GrP_NUpt
           
          LABEL(29) = 'EDAT ' ; VALUE(29) = FLOAT(EYEARDOY)     
          
          LABEL(30) = 'NDCH ' ; VALUE(30) = FLOAT(CDAYS) 
          LABEL(31) = 'TMINA' ; VALUE(31) = TMINCAV       
          LABEL(32) = 'TMAXA' ; VALUE(32) = TMAXCAV       
          LABEL(33) = 'SRADA' ; VALUE(33) = SRADCAV       
          LABEL(34) = 'DAYLA' ; VALUE(34) = DAYLCAV       
          LABEL(35) = 'CO2A ' ; VALUE(35) = CO2CAV        
          LABEL(36) = 'PRCP ' ; VALUE(36) = RAINCC       
          LABEL(37) = 'ETCP ' ; VALUE(37) = ETCC      
           
          IF (FILEIOT(1:2).EQ.'DS') CALL SUMVALS (SUMNUM, LABEL, VALUE)
         
!-----------------------------------------------------------------------
!         Re-initialize
!-----------------------------------------------------------------------
         
          ! Need to re-initialize following because of automatic
          ! fertilization routines in DSSAT
          NFG = 1.0
          NFP = 1.0
          NFT = 1.0
          WFG = 1.0
          WFP = 1.0
          WFT = 1.0
          
          UNO3 = 0.0
          UNH4 = 0.0
          
          !RUNCRP = RUNCRP + 1
          
!          WRITE (fnumwrk,*) ' '
!          WRITE (fnumwrk,'(A50)')
!     &     ' END OF RUN. WILL BEGIN NEW CYCLE IF CALLED FOR.  '
!          IF (IDETL.NE.'N') WRITE (fnumwrk,*) ' '
          SEASENDOUT = 'Y'
          
        ENDIF ! End STGYEARDOY(11).EQ.YEARDOY WITHIN DYNAMIC = OUTPUT

!-----------------------------------------------------------------------
!       Store variables for possible use next day/step
!-----------------------------------------------------------------------

        CNADPREV = CNAD
        IF (CN.LE.1) CROPPREV = CROP
        IF (RNMODE.NE.'G') CUDIRFLPREV = CUDIRFLE
        CWADPREV = CWAD
        DAYLPREV = DAYLT
        ECDIRFLPREV = ECDIRFLE
        ECONOPREV = ECONO
        EMRGFRPREV = EMRGFR
        GESTAGEPREV = GESTAGE
        LAIPREV = LAI
        LNUMPREV = LNUM
        PARIPREV = PARI
        PLYEARDOYPREV = PLYEARDOY
        SPDIRFLPREV = SPDIRFLE
        SRADPREV = SRAD
        SWFRPREV = SWFR
        TNUMPREV = TNUM
        VARNOPREV = VARNO

 9589 FORMAT
     & (//,'*SIMULATED CROP AND SOIL STATUS AT MAIN DEVELOPMENT STAGES')
 9588 FORMAT(
     &/,' ...... DATE ....... GROWTH STAGE    BIOMASS   LEAF  
     &     CROP N      STRESS')     
 9600 FORMAT(' YEARDOY DOM MON DAP ................ kg/ha AREA NUMBER
     &  kg/ha   %   H2O    N')
  206       FORMAT(
     &    /,"*MAIN GROWTH AND DEVELOPMENT VARIABLES",//,   
     &       "@",5X,"VARIABLE",T42,"SIMULATED   MEASURED",/,  
     &           6X,"--------",T42,"---------   --------")  
 500  FORMAT(/,'*ENVIRONMENTAL AND STRESS FACTORS',//,
     &' |-----Development Phase------|-------------Environment--------',
     &'------|----------------Stress-----------------|',/,
     &30X,'|--------Average-------|---Cumulative--|         (0=Min, 1=',
     &'Max Stress)         |',/,
     &25X,'Time  Temp  Temp Solar Photop         Evapo |----Water---|-',
     &'-Nitrogen--|--Phosphorus-|',/,
     &25X,'Span   Max   Min   Rad  [day]   Rain  Trans  Photo',9X,'Pho',
     &'to         Photo',/,
     &25X,'days    °C    °C MJ/m2     hr     mm     mm  synth Growth ',
     &' synth Growth  synth Growth',/,110('-'))
  270 FORMAT(/,'------------------------------------------------------',
     &'--------------------------------------------------------')
  300 FORMAT(/,10X,A," YIELD : ",I8," kg/ha    [",F4.1,"%Moisture] ",/)
 1200     FORMAT(
     &'------------------------------------------------------',
     &'--------------------------------------------------------',
     &///,'*RESOURCE PRODUCTIVITY',
     &//,' Growing season length:', I4,' days ',
     &//,' Precipitation during growth season',T42,F7.1,' mm[rain]',
     & /,'   Dry Matter Productivity',T42,F7.2,' kg[DM]/m3[rain]',
     &                           T75,'=',F7.1,' kg[DM]/ha per mm[rain]',
     & /,'   Yield Productivity',T42,F7.2,' kg[grain yield]/m3[rain]',
     &                       T75,'=',F7.1,' kg[yield]/ha per mm[rain]',
     &//,' Evapotranspiration during growth season',T42,F7.1,' mm[ET]',
     & /,'   Dry Matter Productivity',T42,F7.2,' kg[DM]/m3[ET]',
     &                            T75,'=',F7.1,' kg[DM]/ha per mm[ET]',
     & /,'   Yield Productivity',T42,F7.2,' kg[grain yield]/m3[ET]',
     &                       T75,'=',F7.1,' kg[yield]/ha per mm[ET]',
     &//,' Transpiration during growth season',T42,F7.1,' mm[EP]',
     & /,'   Dry Matter Productivity',T42,F7.2,' kg[DM]/m3[EP]',
     &                            T75,'=',F7.1,' kg[DM]/ha per mm[EP]',
     & /,'   Yield Productivity',T42,F7.2,' kg[grain yield]/m3[EP]',
     &                       T75,'=',F7.1,' kg[yield]/ha per mm[EP]')

 1210 FORMAT(
     & /,' Irrigation during growing season',T42,F7.1,' mm[irrig]',
     & /,'   Dry Matter Productivity',T42,F7.2,' kg[DM]/m3[irrig]',
     &                       T75,'=',F7.1,' kg[DM]/ha per mm[irrig]',
     & /,'   Yield Productivity',T42,F7.2,' kg[grain yield]/m3[irrig]',
     &                       T75,'=',F7.1,' kg[yield]/ha per mm[irrig]')

 1220 FORMAT(
     & /,' N applied during growing season',T42,F7.1,' kg[N applied]/ha'
     & /,'   Dry Matter Productivity',T42,F7.1,' kg[DM]/kg[N applied]',
     & /,'   Yield Productivity',T42,F7.1,' kg[yield]/kg[N applied]')

 1230 FORMAT(
     & /,' N uptake during growing season',T42,F7.1,' kg[N uptake]/ha'
     & /,'   Dry Matter Productivity',T42,F7.1,' kg[DM]/kg[N uptake]',
     & /,'   Yield Productivity',T42,F7.1,' kg[yield]/kg[N uptake]')

      END SUBROUTINE CRP_Output
