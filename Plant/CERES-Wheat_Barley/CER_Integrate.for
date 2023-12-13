!***********************************************************************
! This is the code from the section (DYNAMIC == INTEGR) 
! lines 5010 - 6129 of the original CSCER code.
!***********************************************************************

      SUBROUTINE CER_Integrate (LAI, CANHT, CO2,
     &     DAYLT, DEPMAX, DLAYR, DOY, EOP, EP, ET, KCAN,
     &     HARVFRAC, ISWWAT, LL, NFP, NLAYR,
     &     RAIN, RESCALG, RESLGALG, RESNALG, RLV,
     &     RESWALG, RESWAL, RESNAL, RESLGAL,
     &     SRAD, STGDOY, SW, TMAX, TMIN,
     &     YEAR)

! 2023-01-25 CHP removed unused variables from argument list
!     BD

        USE ModuleDefs
        USE CER_First_Trans_m
        IMPLICIT NONE
        EXTERNAL Cslayers, YVAL1, CALENDAR

        INTEGER DOY, NLAYR, STGDOY(20), YEAR  
        
        REAL CANHT, CO2, HARVFRAC(2) ! BD(20), 
        REAL EOP, KCAN, LL(NL), NFP, RLV(NL) !EO, ES, 
        REAL SRAD, SW(NL), TMAX, TMIN, LAI
        REAL DLAYR(NL), DEPMAX, DAYLT, EP, ET, RAIN
        REAL RESNALG(0:NL), RESCALG(0:NL), RESLGALG(0:NL) 
        REAL RESWALG(0:NL), RESWAL(0:NL), RESNAL(0:NL), RESLGAL(0:NL)
        REAL YVAL1
      
        CHARACTER(LEN=1) ISWWAT 
        

        IF (YEARDOY.GE.YEARPLT) THEN

          ! Dry weights
          ! LAH No growth/development on planting day
          IF (YEARDOY.GT.YEARPLT) THEN   

            SENRSC = SENRSC + SENRS
            CARBOC = CARBOC + CARBO
            RESPC = RESPC + RTRESP
            LFWT = LFWT + GROLF - SENLFG - SENLFGRS
            IF (LFWT.LT.1.0E-12) THEN
!              IF (LFWT.LT.0.0) 
!     &          WRITE(fnumwrk,*)'Leaf weight less than 0! ',LFWT
              LFWT = 0.0
            ENDIF
            STWT = STWT + GROST - SENSTG - GROGRST
            IF (STWT.LT.1.0E-06) THEN
!              IF (STWT.LT.0.0) 
!     &         WRITE(fnumwrk,*)'Stem weight less than 0! ',STWT
              STWT = 0.0
            ENDIF
            GRWT = GRWT + GROGR - GROGRADJ
            RSWT = RSWT + GRORS + GRORSGR + GROGRADJ + SENLFGRS - 
     &             SENRS - RTWTGRS
            
            ! Reserves distribution 
            ! Max concentration in leaves increases through life cycle.
            LLRSWT = AMIN1(RSWT,
     &       LFWT*(1.0-LSHFR)*RSCLX*CUMDU/(Pd(1)+pd(2)+pd(3)+pd(4)))
            LSHRSWT = AMIN1(RSWT-LLRSWT,
     &       LFWT*LSHFR*RSCLX*CUMDU/(Pd(1)+pd(2)+pd(3)+pd(4)))
            IF (STWT.GT.0.0) THEN
!-GH        IF (STWT+CHWT.GT.0.0) THEN
              STRSWT = (RSWT-LLRSWT-LSHRSWT)*(STWT-CHWT)/STWT
              CHRSWT = (RSWT-LLRSWT-LSHRSWT)*CHWT/STWT
            ELSE
              STRSWT = (RSWT-LLRSWT-LSHRSWT)
              CHRSWT = 0.0
            ENDIF
            IF (XSTAGE.GE.LRETS) THEN
              DEADWT = DEADWT + SENLFG + SENSTG
            ELSE
              SENWL(0) = SENWL(0) + (SENLFG+SENSTG)
              SENCL(0) = SENCL(0) + (SENLFG+SENSTG) * 0.4
              SENLGL(0) = SENLGL(0)+(SENLFG*LLIGP/100+SENSTG*SLIGP/100)
            ENDIF
            RTWT = 0.0
            DO L = 1, NLAYR
              RTWTL(L) = RTWTL(L) + RTWTGL(L) - RTWTSL(L)
              SENWL(L) = SENWL(L) + RTWTSL(L)
              SENCL(L) = SENCL(L) + RTWTSL(L) * 0.4
              SENLGL(L) = SENLGL(L) + RTWTSL(L) * RLIGP/100.0
              ! Totals
              RTWT = RTWT + RTWTL(L)
              SENWS = SENWS + RTWTSL(L)
              SENCS = SENCS + RTWTSL(L) * 0.4
              SENLGS = SENLGS + RTWTSL(L) * RLIGP/100.0
            END DO
          ENDIF 
          SEEDRS = AMAX1(0.0,SEEDRS+GRORSSD-RTWTGS)
          SEEDRSAV = SEEDRS

          IF (ISTAGE.GE.6) RSWTPM = RSWTPM + GRORSPM

          ! Chaff. Calculated for output. 
          ! STWT includes chaff, but STWAD excludes chaff.
          IF (XSTAGE.GT.CHSTG) THEN
            IF (GROST.GT.0.0) CHWT = CHWT + GROST*CHFR
          ENDIF

          IF (GRNUM.GT.0.0) THEN
            GWUD = GRWT/GRNUM
          ELSE
            GWUD = 0.0
          ENDIF
            
          GWGD = GWUD*1000.0

          HIAD = 0.0
          SHRTD = 0.0
          IF ((LFWT+STWT+GRWT+RSWT+DEADWT).GT.0.0)
     &     HIAD = GRWT/(LFWT+STWT+GRWT+RSWT+DEADWT)
          IF (RTWT.GT.0.0)
     &     SHRTD = (LFWT+STWT+GRWT+RSWT+DEADWT) / RTWT

          ! Reserve concentration and factor
          RSCD = 0.0
          IF (LFWT+STWT.GT.0.0)
     &     RSCD = RSWT/(LFWT+STWT+RSWT)

          ! Radiation use efficiency
          PARUED = 0.0
          PARADCUM = PARADCUM + PARAD
          IF (PARAD*PARI.GT.0.0) THEN
            PARUED = CARBO*PLTPOP/(PARAD*PARI)
            PARADICUM = PARADICUM + PARAD*PARI
          ENDIF  

          ! Height
          CANHT = CANHT + CANHTG

          ! Leaf areas
          PLA = PLA + PLAGT(1) + PLAGT(2)
          SENLA = SENLA + PLAS + PLASS + PLASC + PLAST
          IF (XSTAGE.GE.LRETS) THEN
            SENLARETAINED = SENLARETAINED
     &                        + (PLAS+PLASS+PLASC+PLAST)
          ELSE
            SENLALITTER = SENLALITTER + (PLAS+PLASS+PLASC+PLAST)
          ENDIF
          IF (LNUMSG.GT.0) THEN
            LAP(LNUMSG) = LAP(LNUMSG) + PLAGT(1)
            LATL(1,LNUMSG) = LATL(1,LNUMSG)+PLAG(1)
            IF (PLAG(2).GT.0.0) THEN
              IF (LNUMSG.LT.LNUMX) THEN
                LAP(LNUMSG+1) = LAP(LNUMSG+1) + PLAGT(2)
                LATL(1,LNUMSG+1) = LATL(1,LNUMSG+1)+PLAG(2)
              ELSEIF (LNUMSG.GE.LNUMX) THEN
                LAP(LNUMSG) = LAP(LNUMSG) + PLAGT(2)
                LATL(1,LNUMSG) = LATL(1,LNUMSG)+PLAG(2)
              ENDIF
            ENDIF
          ENDIF

          IF (ISTAGE.GT.0) GPLA(ISTAGE) = AMAX1(0.0,PLA-SENLA)
          LAI = AMAX1 (0.0,(PLA-SENLA)*PLTPOP*0.0001)
          LAIX = AMAX1(LAIX,LAI)

          PLASTMP = PLAS + PLASS + PLASC + PLAST
          ! Distribute senesced leaf over leaf positions
          IF (LNUMSG.GT.0 .AND. PLASTMP.GT.0) THEN
            DO L = 1, LNUMSG
              IF (LAP(L)-LAPS(L).GT.PLASTMP) THEN
                LAPS(L) = LAPS(L) + PLASTMP
                PLASTMP = 0.0
              ELSE
                PLASTMP = PLASTMP - (LAP(L)-LAPS(L))
                LAPS(L) = LAP(L)
              ENDIF
              IF (PLASTMP.LE.0.0) EXIT
            ENDDO
          ENDIF

          IF (fileiot(1:2).NE.'DS') THEN
          IF (LNUMSG.GT.0) CALL Cslayers
     X     (chtpc,clapc,               ! Canopy characteristics
     X     pltpop,lai,canht,           ! Canopy aspects
     X     lnumsg,lap,lap(lnumsg),     ! Leaf cohort number and size
     X     LAIL)                       ! Leaf area indices by layer
          ENDIF

          ! PAR interception
          IF (PARIP.LT.0.0.AND.LAI.GT.0.0) THEN
            PARI = (1.0 - EXP(-KCAN*(LAI+AWNAI)))
            !WRITE(fnumwrk,'(A28,F5.3)')
     X      ! '  PARI from one-crop model: ',PARI
            ! For maize, kcan is calculated as:
            ! 1.5 - 0.768*((rowspc*0.01)**2*pltpop)**0.1
            ! eg. 1.5 - 0.768*((75*0.01)**2*6.0)**0.1  =  0.63
          ELSE
            PARI = 0.0
          ENDIF

          ! Specific leaf area
          SLA = -99.0
          IF (LFWT.GT.0) SLA = (PLA-SENLA) / (LFWT*(1.0-LSHFR))
          ! Warning if SLA too low
          IF (SLA.LT.0.0.AND.SLA.GT.-90.0) THEN
!            WRITE(fnumwrk,'(A21,F8.3,A12)')
!     X       '  SLA below zero at: ',sla,' Reset to 0'
            SLA = -99.0
          ENDIF

          ! Leaf sheath area
          IF (RSTAGE.LE.4.0) THEN
            LSHAI = (LFWT*LSHFR*LSHAWS)*PLTPOP*0.0001
          ELSE
            ! 2.0 senesces per day
            LSHAI =LSHAI * (1.0-(2.0/100.0)*TT/20.0)  
           ENDIF  

          ! Stem area 
          SAID = AMAX1 (0.0,(STWT*SAWS*PLTPOP*0.0001))

          ! Tillers (Limited to maximum of 20)
          TNUM = AMIN1(20.0,AMAX1(1.0,TNUM+TNUMG-TNUMD-TNUMLOSS))
          IF (LNUMSG.GT.0) TNUML(LNUMSG) = TNUM

          ! Plants
          PLTPOP = PLTPOP - PLTLOSS   

          IF (PLTPOP < 1.E-5) THEN
!            WRITE(fnumwrk,'(I5,1X,I3.3,
!     &        " PLTPOP is zero or negative. Set to zero.",/)') YEAR, DOY
            PLTPOP = AMAX1(PLTPOP, 1.E-5)
          ENDIF

          ! Root depth and length
          IF (SDEPTH.GT.0.0 .AND.RTDEP.LE.0.0) RTDEP = SDEPTH
          RTDEP = AMIN1 (RTDEP+RTDEPG,DEPMAX)
          
          DO L = 1, NLAYR
            RLV(L)=RTWTL(L)*RLWR*PLTPOP/DLAYR(L)
            IF (L.EQ.NLAYR.AND.RLV(L).GT.0.0)THEN
              IF (RTSLXDATE.LE.0) RTSLXDATE = YEARDOY
            ENDIF
          END DO
          ! Originally: RLV(L)=RTWTL(L)*(RLWR/0.6)*PLTPOP/DLAYR(L)
          ! 0.6 above kept to keep similarity with Ceres,
          ! in which a value of 0.98 for RLWR was applied to assimilate
          ! going to root. This was multiplied by a factor of 0.6 to 
          ! account for root repiration to get actual root dry weight 
          ! increase.

          ! Vernalization.  NB. Starts at germination
          CUMVD = AMAX1(0.0,CUMVD+TFV-VDLOST)
          IF (ISTAGE.GE.7 .OR. ISTAGE.LE.1) THEN
            IF (P1V.GT.0.0) THEN
              IF (p1v.GT.0.0) THEN
                VRNSTAGE =AMAX1(0.,AMIN1(1.,CUMVD/p1v))
              ELSE
                VRNSTAGE = 1.0
              ENDIF
              !VF = AMAX1(0.,AMAX1(0.,AMIN1(1.,CUMVD/P1V)))
              ! BELOW FROM CSCRP
              VF = AMAX1(0.,(1.0-VEFF) + VEFF*VRNSTAGE)
            ELSE
              VF = 1.0   
            ENDIF
          ELSEIF (ISTAGE.GT.1 .AND. ISTAGE.LT.7) THEN
              VF = 1.0
          ENDIF

          ! Cold hardiness
          HARDAYS = AMAX1(HARDAYS+TFH-HARDILOS,0.0)
          HARDI = AMIN1(1.0,HARDAYS/HDUR)
          TKILL = LT50S + (LT50H-LT50S)*HARDI

          ! Nitrogen
          NUPC = NUPC + NUPD
          LEAFN = LEAFN + DLEAFN + SEEDNT
     &          - GRAINNGL - SENNLFG - SENNLFGRS
          IF (LEAFN.LT.1.0E-10) LEAFN = 0.0
          STEMN = STEMN + DSTEMN
     &          - GRAINNGS - SENNSTG - SENNSTGRS
          IF (STEMN.LT.1.0E-10) STEMN = 0.0
          ROOTNS = 0.0
          DO L = 1, NLAYR
            SENNL(L) = SENNL(L) + RTNSL(L)
            ROOTNS = ROOTNS + RTNSL(L)
            SENNS = SENNS + RTNSL(L)
          END DO
          ROOTN = ROOTN + DROOTN + SEEDNR - GRAINNGR - ROOTNS
          SEEDN = SEEDN - SEEDNR - SEEDNT
          IF (SEEDN.LT.1.0E-6) SEEDN = 0.0
          GRAINN = GRAINN + GRAINNG + GRAINNGL + GRAINNGS + GRAINNGR
     &           + RSNUSEG
          RSN = RSN - RSNUSEG - RSNUSER - RSNUSET
     &        + SENNLFGRS + SENNSTGRS
          IF (XSTAGE.GE.LRETS) THEN
            DEADN = DEADN + SENNLFG + SENNSTG
          ELSE
            SENNL(0) = SENNL(0) + (SENNLFG+SENNSTG)
          ENDIF

          ! Harvest index for N
          HIND = 0.0
          IF ((LEAFN+STEMN+GRAINN+RSN+DEADN).GT.0.0)
     &     HIND = GRAINN/(LEAFN+STEMN+GRAINN+RSN+DEADN)

          ! Variables expressed per unit ground area:living plant
          CARBOAC = CARBOC*PLTPOP*10.0
          RESPAC = RESPC*PLTPOP*10.0

          CWAD = AMAX1(0.0,(LFWT+STWT+GRWT+RSWT+DEADWT)*PLTPOP*10.0)
          DWAD = AMAX1(0.0,DEADWT*PLTPOP*10.0)
          GWAD = AMAX1(0.0,GRWT*PLTPOP*10.0)
          LLWAD = AMAX1(0.0,LFWT*(1.0-LSHFR)*10.0*PLTPOP)
          LSHWAD = AMAX1(0.0,LFWT*LSHFR*10.0*PLTPOP)
          CHWAD = AMAX1(0.0,CHWT*PLTPOP*10.0)
          ! NB.No reserves in chaff
          RSWAD = AMAX1(0.0,RSWT*PLTPOP*10.0)
          RSWADPM = AMAX1(0.0,RSWTPM*PLTPOP*10.0)
          RWAD = AMAX1(0.0,RTWT*PLTPOP*10.0)
          SDWAD = AMAX1(0.0,(SEEDRS+SDCOAT)*10.0*PLTPOP)
          STWAD = AMAX1(0.0,(STWT-CHWT)*10.0*PLTPOP)
          ! NB. Stem weigh here excludes chaff wt.
          STLSRWAD = AMAX1(0.0,(STWT-CHWT+LFWT*LSHFR+RSWT)*10.0*PLTPOP)
          LLRSWAD = AMAX1(0.0,LLRSWT*PLTPOP*10.0)
          LSHRSWAD = AMAX1(0.0,LSHRSWT*PLTPOP*10.0)
          STRSWAD = AMAX1(0.0,STRSWT*PLTPOP*10.0)
          CHRSWAD = AMAX1(0.0,CHRSWT*PLTPOP*10.0)

          SENWAS = SENWS*10.0*PLTPOP
          SENCAS = SENCS*10.0*PLTPOP
          SENLGAS = SENLGS*10.0*PLTPOP
          SENWAL(0) = SENWL(0)*PLTPOP*10.0
          SENCAL(0) = SENCL(0)*PLTPOP*10.0
          SENLGAL(0) = SENLGL(0)*PLTPOP*10.0
          DO L =1,NLAYR
            RTWTAL(L) = RTWTL(L)*PLTPOP*10.0
            SENWAL(L) = SENWL(L)*PLTPOP*10.0
            SENCAL(L) = SENCL(L)*PLTPOP*10.0
            SENLGAL(L) = SENLGL(L)*PLTPOP*10.0
          ENDDO

          TWAD = (SEEDRS+SDCOAT+RTWT+LFWT+STWT+GRWT+RSWT+DEADWT)
     &         * PLTPOP*10.0
          VWAD = (LFWT+STWT+RSWT+DEADWT)*PLTPOP * 10.0
          EWAD = (GRWT+CHWT)*PLTPOP * 10.0

          IF (GRNUM.GT.0.0) THEN 
            GRNUMAD = GRNUM*PLTPOP
          ELSE
            GRNUMAD = 0.0
          ENDIF  
          TNUMAD = TNUM*PLTPOP

          NUAD = NUPC*PLTPOP*10.0
          CNAD = (LEAFN+STEMN+GRAINN+RSN+DEADN)*PLTPOP*10.0
          DNAD = DEADN*PLTPOP*10.0
          GNAD = GRAINN*PLTPOP*10.0
          LLNAD = LEAFN*(1.0-LSHFR)*PLTPOP*10.0
          RNAD = ROOTN*PLTPOP*10.0
          RSNAD = RSN*PLTPOP*10.0
          SDNAD = SEEDN*PLTPOP*10.0
          SNAD = STEMN*PLTPOP*10.0
          TNAD = (ROOTN+LEAFN+STEMN+RSN+GRAINN+SEEDN+DEADN)*PLTPOP*10.0
          VNAD = (LEAFN+STEMN+RSN+DEADN)*PLTPOP*10.0

          SENNAS = SENNS*10.0*PLTPOP
          SENNAL(0) = SENNL(0)*PLTPOP*10.0
          DO L =1,NLAYR
            SENNAL(L) = SENNL(L)*PLTPOP*10.0
          ENDDO

          ! STAGES:Reproductive development (Rstages)
          CUMDU = CUMDU + DU
          IF (GESTAGE.GE.1.0) CUMTU = CUMTU + TT

          IF (CUMDU.LT.PTH(0) .AND. PD(0) > 0.) THEN
            RSTAGE = CUMDU/PD(0)
          ELSE
            DO L = 6,1,-1
              IF (CUMDU.GE.PTH(L-1)) THEN
                RSTAGE = FLOAT(L) + (CUMDU-PTH(L-1))/PD(L)
                ! Following is necessary because xstages non-sequential
                RSTAGE = AMIN1(6.9,RSTAGE)
                EXIT
              ENDIF
            ENDDO
          ENDIF
          IF (CROP.EQ.'MZ'.AND.PDADJ.LE.-99.0.AND.RSTAGE.GT.2.0) THEN
            PDADJ = (CUMTU-TT-PD(0))/(CUMDU-DU-PD(0))
!            WRITE(fnumwrk,'(A26,F6.1)')
!     &       ' Phase adjustment         ',(PDADJ-1.0)*PD(2)
!            WRITE(fnumwrk,'(A24)')'   PHASE OLD_END NEW_END'
            DO L = 2,10
              PTHOLD = PTH(L)
              PTH(L) = PTH(L) + AMAX1(0.0,PDADJ-1.0)*PD(2)
!              WRITE(fnumwrk,'(I8,2F8.1)')L,PTHOLD,PTH(L)
            ENDDO
          ENDIF

          ! STAGES:Germination and emergence (Gstages)
          ! NB 0.5 factor used to equate to Zadoks)
          IF (ISTAGE.GT.7) CUMGEU = CUMGEU + GEU
          IF (CUMGEU.LT.PEGD) THEN
            GESTAGE = AMIN1(1.0,CUMGEU/PEGD*0.5)
          ELSE
            GESTAGE = AMIN1(1.0,0.5+0.5*(CUMGEU-PEGD)/(PEMRG*SDEPTH))
          ENDIF

          ! STAGES:Leaf numbers
          IF (LNUMSG.GT.0 .AND. ISTAGE.LE.2) THEN
            ! If new leaf to be produced
            IF ((TT/PHINT).GT.(FLOAT(LNUMSG)-LNUMSD)) THEN 
              ! If new leaf will be in 3rd phint phase
              IF (LNUMSD.LE.(PHINTL(1)+PHINTL(2)).AND.
     &          LNUMSD+(TT/PHINT).GT.(PHINTL(1)+PHINTL(2))) THEN
                  TTTMP = TT - PHINT*(FLOAT(LNUMSG)-LNUMSD)
                  LNUMSD = LNUMSD+(TT-TTTMP)/PHINT+
     &             TTTMP/(PHINTS*PHINTF(3))
              ! If new leaf will be in 2nd phint phase
              ELSEIF (LNUMSD.LE.PHINTL(1).AND.
     &          LNUMSD+(TT/PHINT).GT.PHINTL(1)) THEN
                  TTTMP = TT - PHINT*(FLOAT(LNUMSG)-LNUMSD)
                  LNUMSD = LNUMSD+(TT-TTTMP)/PHINT+
     &             TTTMP/PHINTS
              ELSE
              ! New leaf in current phint phase
              LNUMSD = AMIN1(FLOAT(LNUMX-1)+0.9,LNUMSD+TT/PHINT)
              ENDIF
              LNUMSD = AMIN1(FLOAT(LNUMX-1)+0.9,LNUMSD)
            ! NO new leaf  
            ELSE
              LNUMSD = AMIN1(FLOAT(LNUMX-1)+0.9,LNUMSD+TT/PHINT)
            ENDIF
          ENDIF
          IF (LNUMSD.GE.FLOAT(LNUMX-1)+0.9) THEN
 !           IF (CCOUNTV.EQ.0) WRITE (fnumwrk,'(A35,I4)')
 !    &       ' Maximum leaf number reached on day',DOY
            CCOUNTV = CCOUNTV + 1
            IF (CCOUNTV.EQ.50) THEN
!              WRITE (fnumwrk,'(A47,/,A44,/A26)')
!     &         ' 50 days after maximum leaf number! Presumably ',
!     &         ' vernalization requirement could not be met!',
!     &         ' Will assume crop failure.'
              CFLFAIL = 'Y'
            ENDIF
          ENDIF
          LNUMSG = INT(LNUMSD)+1
          IF (LNUMSG.EQ.100) THEN
!           WRITE (fnumwrk,'(A47,/,A47,/,A47,/,A26)')
!     &      ' Maximum leaf number reached! Presumably       ',
!     &      ' vernalization requirement could not be met,   ',
!     &      ' or photoperiod too short.                     ',
!     &      ' Will assume crop failure.'
            CFLFAIL = 'Y'
          ENDIF
          LCNUM = INT(LNUMSD)+1

          ! STAGES:Overall development (Istages)      Zadoks  Rstages
          ! 8 - Sowing date                             00      0.0
          ! 9 - Germination                             05      1.0
          ! 1 - Emergence                               10      1.0
          ! 2 - End spikelet production (=t spikelet)   ??      2.0
          ! 3 - End leaf growth                         40      3.0
          ! 4 - End spike growth                        50      4.0
          ! 5 - End lag phase of grain growth           80      5.0
          ! 6 - End grain fill (Physiological maturity) 90      6.0
          ! 7 - Harvest maturity or harvest             92      6.9
          ! More precisely translated as:
          !  Xstage Zstage
          !      .1  1.00
          !     1.0
          !     2.0
          !     2.5 31.00  Jointing after tsp at 2.0+PD2(1)/PD(2)
          !     3.0 40.00
          !     4.0 57.00
          !     5.0 71.37
          !     5.5 80.68
          !     6.0 90.00
          ! Possible new rstages:
          !  Rstnew?               Xstage Zstage
          !       0 Wetted up         1.0     0.0
          !     1.0 End juvenile
          !     2.0 Double ridges
          !     3.0 Terminal spikelet 2.0
          !         Jointing          2.?    31.0
          !     4.0 Last leaf         3.0    39.0
          !     5.0 Spike emergence          50.0
          !         End spike growth  4.0
          !     6.0 Start anthesis           60.0
          !     7.0 End anthesis             70.0
          !     8.0 End lag           5.0    71.4
          !         End milk          5.5    80.7
          !     9.0 End grain fill    6.0    90.0
          !    10.0 Harvest           6.9    92.0

          IF (ISTAGE.EQ.7) THEN                       ! Pre-planting
            ISTAGE = 8
            XSTAGE = 8.0
          ELSEIF (ISTAGE.EQ.8) THEN                   ! Planted
            XSTAGE = FLOAT(ISTAGE) + GESTAGE*2.0
            IF(GESTAGE.GE.0.5) THEN
              ISTAGE = 9
              XSTAGE = FLOAT(ISTAGE) + (GESTAGE-0.5)*2.0
            ENDIF
          ELSEIF (ISTAGE.EQ.9) THEN                   ! Germination
            XSTAGE = FLOAT(ISTAGE) + (GESTAGE-0.5)*2.0
            IF(GESTAGE.GE.1.0) THEN
              ISTAGE = 1
              XSTAGE = 1.0
            ENDIF
          ELSE                                        ! Emergence on
            ISTAGE = INT(RSTAGE)
            XSTAGE = AMIN1(6.9,RSTAGE)                ! < 7 (=pre-plant)
          ENDIF
          ! Secondary stages
          SSTAGE = AMAX1(0.0,AMIN1(1.0,(XSTAGE-AINT(XSTAGE))))

          ! STAGES:Overall development (Zadoks)
          ! 01=begining of seed imbibition (assumed to be at planting)
          ! 05=germination (assumed to be when the radicle emerged)
          ! 09=coleoptile thru soil surface
          ! 10=first leaf emerged from the coleoptile (= emergence)
          ! 11=first leaf fully expanded --> 1n=nth leaf fully expanded
          ! 20=first tiller appeared on some plants --> 2n=nth tiller
          ! 30=f(reproductive stage)

          IF (XSTAGE.GE.8.0 .AND. XSTAGE.LE.9.0) THEN
            ZSTAGE =  ((XSTAGE-8.0)/2.0)*10.0
          ELSEIF (XSTAGE.GT.9.0) THEN
            ZSTAGE = (0.5+((XSTAGE-9.0)/2.0))*10.0
          ELSEIF (XSTAGE.GE.0.0 .AND. XSTAGE.LE.(2.0+pd2fr(1))) THEN
            IF (TNUM.LT.2.0) THEN
              ZSTAGE = AMIN1(20.0,10.0 + LNUMSD)
            ELSE
              ZSTAGE = AMIN1(30.0,20.0 + (TNUM-1.0))
            ENDIF
            IF (ZSTAGE.LT.ZSTAGEP) ZSTAGE = ZSTAGEP
            ZSTAGEP = ZSTAGE
          ELSEIF (XSTAGE.GT.(2.0+pd2fr(1)) .AND. XSTAGE.LE.3.0) THEN
            ZSTAGE = 30.0 + 10.0*(XSTAGE-(2.0+pd2fr(1)))/(1.0-pd2fr(1))
          ELSEIF (XSTAGE.GT.3.0 .AND. XSTAGE.LE.4.0) THEN
            ZSTAGE = 40.0 + 10.0*(XSTAGE-3.0)
          ELSEIF (XSTAGE.GT.4.0 .AND. XSTAGE.LE.5.0) THEN
            IF (XSTAGE.LT.ASTAGE) THEN
              ZSTAGE = 50.0 + 10.0*((XSTAGE-4.0)/(ASTAGE-4.0))
            ELSEIF (XSTAGE.GE.ASTAGE.AND.XSTAGE.LT.ASTAGEND) THEN
              ZSTAGE = 60.0 + 10.0*((XSTAGE-ASTAGE)/(ASTAGEND-ASTAGE))
            ELSE
              ZSTAGE = 70.0 + 10.0*((XSTAGE-ASTAGEND)/(5.0-ASTAGEND))
            ENDIF
          ELSEIF (XSTAGE.GT.5.0 .AND. XSTAGE.LE.6.0) THEN
            ZSTAGE = 80.0 + 10.0*(XSTAGE-5.0)
          ELSEIF (XSTAGE.GT.6.0 .AND. XSTAGE.LE.7.0) THEN
            ZSTAGE = 90.0 + 10.0*(XSTAGE-6.0)
          ENDIF
          
          GO TO 9345   ! Jump over WDBachelor-CERES calculations
          ! Following were introduced for comparative purposes only
          ! Original CRITICAL AND MINIMA N concentrations.
          ! Below from w_nfacto as reworked by WDB
          ! Conversions from XSTAGE to ZADOKS growth stage
          ! From CROPSIM-CERES
          IF (XSTAGE.GE.8.0 .AND. XSTAGE.LE.9.0) THEN
            ZSTAGE =  ((XSTAGE-8.0)/2.0)
          ELSEIF (XSTAGE.GT.9.0) THEN
            ZSTAGE = (0.5+((XSTAGE-9.0)/2.0))
          ENDIF
          ! From CERES 9WDB)
          IF (XSTAGE .LE. 2.0) THEN
              ZSTAGE = XSTAGE
          ENDIF
          IF (XSTAGE .GT. 2.0 .AND. XSTAGE .LE. 3.0) THEN
              ZSTAGE = 2.00 + 2.0000*(XSTAGE-2.0)
          ENDIF
          IF (XSTAGE .GT. 3.0 .AND. XSTAGE .LE. 4.0) THEN
              ZSTAGE = 4.00 + 1.7000*(XSTAGE-3.0)
          ENDIF
          IF (XSTAGE .GT. 4.0 .AND. XSTAGE .LE. 4.4) THEN
              ZSTAGE = 5.70 + 0.8000*(XSTAGE-4.0)
          ENDIF
          IF (XSTAGE .GT. 4.4 .AND. XSTAGE .LE. 6.0) THEN
              ZSTAGE = 6.02 + 1.8625*(XSTAGE-4.4)
          ENDIF
          YSTAGE = XSTAGE
          ZS2    = ZSTAGE*ZSTAGE
          ZS3    = ZS2*ZSTAGE
          ZS4    = ZS3*ZSTAGE
          IF (P1V .GE. 0.03) THEN
             TCNP = -5.0112400-6.350677*ZSTAGE+14.9578400*SQRT(ZSTAGE)
     1              +0.2238197*ZS2
           ELSE
             TCNP =  7.4531813-1.7907829*ZSTAGE+0.6092849*SQRT(ZSTAGE)
     1              +0.0933967*ZS2
          ENDIF
          IF (ZSTAGE .GT. 6.0) THEN
             TCNP = TCNP - (ZSTAGE-6.0)*0.140
          ENDIF
          TCNP  = TCNP/100.0
          TMNC  = (2.97-0.455*XSTAGE)/100.0
          RCNP  = (2.10-0.14*SQRT(ZSTAGE))*0.01 ! RCNC in original Ceres
          RMNP  = 0.75 * RCNP
          ZSTAGE = ZSTAGE * 10.0
9345      CONTINUE     

          ! Stage dates and characteristics
          ! NB. Characeristics are at end of phase
          IF (ISTAGE.NE.ISTAGEP.AND.ISTAGEP.GT.0) THEN
            STGDOY(ISTAGEP) = YEARDOY
            CWADSTG(ISTAGEP) = CWAD
            LAISTG(ISTAGEP) = LAI
            LNUMSTG(ISTAGEP) = LNUMSD
            CNADSTG(ISTAGEP) = CNAD
          ENDIF
          
          ! Double ridge factors by calibration from LAMS experiment
          drf1 = 1.9
          drf2 = 0.058
          drf3 = 3.3
          !DRSTAGE = AMAX1(1.1,drf1-drf2*(LNUMSD-drf3))
          ! Changed to same as in CCSRP
          DRSTAGE = 1.6
          IF (DRDAT.EQ.-99 .AND. RSTAGE.GE.DRSTAGE) THEN
            DRDAT = YEARDOY
!            WRITE(fnumwrk,*)' '
!            WRITE(fnumwrk,*)'Double ridges. Stage,Leaf#: ',
!     &       DRSTAGE,LNUMSD
             ! NB. Experimental. DR occurs at later apical stage when
             !     leaf # less, earlier when leaf # greater (ie.when
             !     early planting of winter type).
          ENDIF
          IF (TSDAT.EQ.-99 .AND. RSTAGE.GE.2.00) THEN
            TSDAT = YEARDOY
            LNUMTS = LNUMSD
            ! Final leaf# algorithm. LAH Coefficients need to be in file
            FLN = LNUMSD + (2.8 + 0.1 * LNUMTS)
            TVR1 = MOD(FLN,(FLOAT(INT(FLN))))
            IF (TVR1.LT.0.5) THEN
              FLN = FLOAT(INT(FLN))-0.001
            ELSE
              FLN = FLOAT(INT(FLN))+0.999
            ENDIF
            PD2ADJ = ((FLN-LNUMTS)) * PHINTS
!            WRITE(fnumwrk,*)' '  
!            WRITE(fnumwrk,'(A25,I12)')' Terminal spikelet       ',tsdat
!            WRITE(fnumwrk,*)' Terminal spilelet leaf #       ',LNUMTS  
!            WRITE(fnumwrk,*)' Final leaf # (Aitken formula)  ',FLN      
!            WRITE(fnumwrk,*)' P2 Durations Input,From Aitken ',
!     &       PD(2),PD2ADJ
            IF (PD(2).LE.0.0) THEN
              PD(2) = PD2ADJ
              DO L = 2,10
                PTH(L) = PTH(L-1) + PD(L)
              ENDDO
!              WRITE(fnumwrk,*)' AITKEN FORMULA USED TO CALCULATE P2  '
            ENDIF 
          ENDIF  
          IF (JDAT.EQ.-99 .AND. RSTAGE.GE.2.0+PD2(1)/PD(2)) THEN
            JDAT = YEARDOY
!            WRITE (fnumwrk,*) ' '
!            WRITE (fnumwrk,'(A25,I12)') ' Jointing:               ',jdat
          ENDIF
          IF (LLDAT.EQ.-99 .AND. RSTAGE.GE.3.0) THEN
            LLDAT = YEARDOY
            FLNMODEL = LNUMSD
!            WRITE (fnumwrk,'(A25,I12)')' Last leaf emergence:    ',lldat
          ENDIF
          IF (IEDAT.EQ.-99 .AND. RSTAGE.GE.4.0) THEN
            IEDAT = YEARDOY
!            WRITE (fnumwrk,'(A25,I12)')' Inflorescence emergence:',iedat
          ENDIF
          IF (ADAT.LE.0.0 .AND. RSTAGE.GE.4.0+PD4(1)/PD(4)) THEN
            ADAT = YEARDOY
!            WRITE (fnumwrk,'(A25,I12)') ' Anthesis:               ',adat
            RSWAA = RSWAD
            RSCA = RSCD
            CWAA = CWAD
            CNAA = CNAD
            LNPCA = LANC*100.0
            ADATEND = -99
          ENDIF
          IF (ADATEND.LE.0.0 .AND.
     &      RSTAGE.GE.4.0+(PD4(1)+PD4(2))/PD(4)) THEN
            ADATEND = YEARDOY
            TMEAN20A = TMEAN20
            SRAD20A = SRAD20
            STRESS20A = STRESS20
            GRNUM = (LFWT+STWT+RSWT)*G1CWT
!            WRITE (fnumwrk,'(A25,I12)')
!     &       ' End of anthesis:        ',adatend
!            WRITE (fnumwrk,'(A27,F7.1)')
!     &       ' Prior 20d mean temperature   ',tmean20a
!            WRITE (fnumwrk,'(A27,F7.1)')
!     &       ' Prior 20d mean stress factor ',stress20a
!            WRITE (fnumwrk,'(A27)')'  NB. 1.0 = 0 stress          '
!            WRITE (fnumwrk,*)'Grain #/m2,Nfg ',GRNUM*PLTPOP,NFG
!            IF ((GRNUM*PLTPOP).LT.100.0) THEN
!              WRITE (fnumwrk,*)'Crop failure - few grains set!'
!            ENDIF
          ENDIF
          IF (RSTAGE.GT.ASTAGEND) THEN
            LAGSTAGE = AMAX1(0.0,
     &       AMIN1(1.0,(RSTAGE-ASTAGEND)/(5.0-ASTAGEND)))
          ENDIF

          ! Average nitrogen and water stress, and environmental vars
          ! LAH Changed from 6 to 7 JUNE 2008
          IF (ISTAGEP.GT.0 .AND. ISTAGEP.LE.7) THEN
            NFPC = NFPC + NFP
            NFGC = NFGC + NFG
            WFPC = WFPC + WFP
            WFGC = WFGC + WFG
            ICSDUR = ICSDUR + 1
            tmaxsump = tmaxsump + tmax
            tminsump = tminsump + tmin
            sradsum = sradsum + srad
            daylsum = daylsum + daylt
            co2sum = co2sum + co2
            rainsum = rainsum + rain
            etsum = etsum + et
            epsum = epsum + ep
            ICSDUR0 = ICSDUR0 + 1
            NFPC0 = NFPC0 + NFP
            NFGC0 = NFGC0 + NFG
            WFPC0 = WFPC0 + WFP
            WFGC0 = WFGC0 + WFG
            tmaxsump0 = tmaxsump0 + tmax
            tminsump0 = tminsump0 + tmin
            sradsum0 = sradsum0 + srad
            daylsum0 = daylsum0 + daylt
            co2sum0 = co2sum0 + co2
            rainsum0 = rainsum0 + rain
            etsum0 = etsum0 + et
            epsum0 = epsum0 + ep
            IF (ICSDUR.GT.0) THEN
              NFPAV(ISTAGEP) = NFPC / ICSDUR
              NFGAV(ISTAGEP) = NFGC / ICSDUR
              WFPAV(ISTAGEP) = WFPC / ICSDUR
              WFGAV(ISTAGEP) = WFGC / ICSDUR
              DAYSC(ISTAGEP) = ICSDUR
              tmaxav(ISTAGEP) = tmaxsump / ICSDUR
              tminav(ISTAGEP) = tminsump / ICSDUR 
              sradav(ISTAGEP) = sradsum / ICSDUR
              daylav(ISTAGEP) = daylsum / ICSDUR
              co2av(ISTAGEP) = co2sum / ICSDUR
              raincp(ISTAGEP) = rainsum 
              etc(ISTAGEP)   = etsum 
              epc(ISTAGEP)   = epsum 
              DAYSC(0) = ICSDUR0
              NFPAV(0) = NFPC0 / ICSDUR0
              NFGAV(0) = NFGC0 / ICSDUR0
              WFPAV(0) = WFPC0 / ICSDUR0
              WFGAV(0) = WFGC0 /ICSDUR0
              tmaxav(0) = tmaxsump0 / ICSDUR0
              tminav(0) = tminsump0 / ICSDUR0 
              sradav(0) = sradsum0 / ICSDUR0
              daylav(0) = daylsum0 / ICSDUR0
              co2av(0) = co2sum0 / ICSDUR0
              raincp(0) = rainsum0 
              etc(0)   = etsum0 
              epc(0)   = epsum0 
            ENDIF
            IF (ISTAGE.NE.ISTAGEP) THEN
              NFPAV(ISTAGE) = 1.0
              NFGAV(ISTAGE) = 1.0
              WFPAV(ISTAGE) = 1.0
              WFGAV(ISTAGE) = 1.0
              tmaxav(ISTAGE) = -99.0
              tminav(ISTAGE) = -99.0 
              sradav(ISTAGE) = -99.0
              daylav(ISTAGE) = -99.0
              co2av(ISTAGE) = -99.0
              etc(ISTAGE)   = 0.0
              epc(ISTAGE)   = 0.0
              raincp(ISTAGE) = -99.0
              NFPC = 0.0
              NFGC = 0.0
              WFPC = 0.0
              WFGC = 0.0
              ICSDUR = 0
              tmaxsump = 0.0 
              tminsump = 0.0 
              sradsum = 0.0 
              daylsum = 0.0 
              co2sum = 0.0 
              rainsum = 0.0 
              etsum = 0.0 
              epsum = 0.0 
              IF (ISTAGE.EQ.6) THEN
                NFPAV(ISTAGE) = NFP
                NFGAV(ISTAGE) = NFG
                WFPAV(ISTAGE) = WFP
                WFGAV(ISTAGE) = WFG
                tmaxav(ISTAGE) = TMAX
                tminav(ISTAGE) = TMIN 
                sradav(ISTAGE) = SRAD
                daylav(ISTAGE) = DAYLT
                co2av(ISTAGE) = co2
                etc(ISTAGE)   = 0
                epc(ISTAGE)   = 0
                raincp(ISTAGE) = 0
              ENDIF
            ENDIF
          ENDIF

          ! Phyllochron intervals
          IF (CROP.EQ.'BA'.AND.ISTAGE.NE.ISTAGEP.AND.ISTAGE.EQ.1) THEN
            tvr1 = 77.5 - 232.6*(DAYLT-DAYLTP)
!            WRITE(FNUMWRK,*)' '
!            WRITE(FNUMWRK,*)
!     &       ' PHINT calculated from daylength change: ',tvr1
!            WRITE(FNUMWRK,*)
!     &       ' PHINT being used:                       ',phints 
          ENDIF
          IF (LNUMSG.GT.0) THEN
            IF (LNUMSD.LT.PHINTL(1)) THEN
              PHINT = PHINTS*PHINTF(1)
            ELSEIF (LNUMSD.GE.PHINTL(1)+PHINTL(2)) THEN
              PHINT = PHINTS*PHINTF(3)
            ELSE
              PHINT = PHINTS
            ENDIF  
          ENDIF

          ! Critical and minimum N concentrations
          IF (ISTAGE.LT.7) THEN
            LCNC = YVAL1(LCNCS,'0','9',XSTAGE)
            SCNC = YVAL1(SCNCS,'0','9',XSTAGE)
            RCNC = YVAL1(RCNCS,'0','9',XSTAGE)
            LMNC = YVAL1(LMNCS,'0','9',XSTAGE)
            SMNC = YVAL1(SMNCS,'0','9',XSTAGE)
            RMNC = YVAL1(RMNCS,'0','9',XSTAGE)
          ELSE
            RCNC = RCNCS(0) + (RCNCS(1)-RCNCS(0))*((XSTAGE-8.0)/2.0)
            RMNC = RMNCS(0) + (RCNCS(1)-RCNCS(0))*((XSTAGE-8.0)/2.0)
          ENDIF

          ! N concentrations and adjustments
          ! (Adjustements to account for changes in criticals)
          RANC = 0.0
          LANC = 0.0
          SANC = 0.0
          VANC = 0.0
          IF (RTWT.GT.0.0) RANC = ROOTN / RTWT
          IF (LFWT.GT.0.0) LANC = LEAFN / LFWT
          IF (STWT.GT.0.0) SANC = STEMN / STWT
          IF (VWAD.GT.0.0) VANC = VNAD/VWAD
          RSNGR = AMAX1(0.0,RTWT*(RANC-RCNC))
          RSNGL = AMAX1(0.0,LFWT*(LANC-LCNC))
          RSNGS = AMAX1(0.0,STWT*(SANC-SCNC))
          RSN = RSN + RSNGR + RSNGL + RSNGS
          ROOTN = ROOTN - RSNGR
          LEAFN = LEAFN - RSNGL
          STEMN = STEMN - RSNGS
          IF (RTWT.GT.0.0) RANC = ROOTN/RTWT
          IF (LFWT.GT.0) LANC = LEAFN/LFWT
          IF (STWT.GT.0.0) SANC = STEMN/STWT
          IF (VWAD.GT.0.0) VANC = VNAD/VWAD
          IF (LANC.LT.0.0) THEN
!            WRITE(fnumwrk,*)'LANC below 0 with value of ',LANC
!            WRITE(fnumwrk,*)'LEAFN,LFWT had values of   ',LEAFN,LFWT
            LANC = AMAX1(0.0,LANC)
          ENDIF
          IF (LFWT+STWT.GT.0.0) VCNC = 
     &     (LCNC*AMAX1(0.0,LFWT)+SCNC*AMAX1(0.0,STWT))/
     &     (AMAX1(0.0,LFWT)+AMAX1(0.0,STWT))
          IF (LFWT+STWT.GT.0.0) VMNC = 
     &     (LMNC*AMAX1(0.0,LFWT)+SMNC*AMAX1(0.0,STWT))/
     &     (AMAX1(0.0,LFWT)+AMAX1(0.0,STWT))

          CANANC = 0.0
          SDNC = 0.0
          GRAINANC = 0.0
          IF ((LFWT+STWT+GRWT+RSWT+DEADWT).GT.0.0)
     &     CANANC = (LEAFN+STEMN+GRAINN+RSN+DEADN)/
     &      (LFWT+STWT+GRWT+RSWT+DEADWT)
          IF (SEEDRS.GT.0.0) SDNC = SEEDN/(SEEDRS+SDCOAT)
          IF (GRWT.GT.0) GRAINANC = GRAINN/GRWT

          LCNF = 0.0
          SCNF = 0.0
          RCNF = 0.0
          IF (LCNC.GT.0.0) LCNF = LANC/LCNC
          IF (LCNF.GT.1.0001 .OR. LCNF.LT.0.0) THEN
!            WRITE(fnumwrk,*)'LCNF out of limits with value of ',LCNF
            LCNF = AMAX1(0.0,AMIN1(1.0,LCNF))
          ENDIF
          IF (SCNC.GT.0.0.AND.STWT.GT.1.0E-10) SCNF = SANC/SCNC
          IF (RCNC.GT.0.0.AND.RTWT.GT.0.0) RCNF = RANC/RCNC

          ! Harvesting conditions
          IF (IHARI.EQ.'A' .AND. ISTAGE.EQ.6) THEN
            ! Here need to check out if possible to harvest.
            IF (YEARDOY.GE.HFIRST) THEN
              IF (SW(1).GE.SWPLTL.AND.SW(1).LE.SWPLTH) YEARHARF=YEARDOY
            ENDIF
            ! Check if past earliest date; check if not past latest date
            ! Check soil water
            ! If conditions met set YEARHARF = YEARDOY
            ! (Change YEARHARF to more something more appropriate)
          ENDIF

          ! Harvesting or failure
          IF (DAP.GE.90 .AND. ISTAGE.EQ.8) THEN
            CFLFAIL = 'Y'
!            WRITE (FNUMWRK,*)'No germination within 90 days of sowing!'
          ENDIF
          
          IF (IHARI.NE.'A'.AND.DAPM.GE.90) THEN
            CFLFAIL = 'Y'
!            WRITE (FNUMWRK,*)'90 days after physiological maturity!'
!            WRITE (FNUMWRK,*)'Harvesting triggered!'
          ENDIF
          
          IF (IHARI.NE.'A'.AND.ISTAGE.GE.4.AND.ISTAGE.LT.7) THEN
            IF (TT20.NE.-99.0.AND.TT20.LE.0.0) THEN
              CFLFAIL = 'Y'
!              WRITE (FNUMWRK,*)'20day thermal time mean = 0!'
!              WRITE (FNUMWRK,*)'Harvesting triggered!'
            ENDIF
          ENDIF

          IF (CFLFAIL.EQ.'Y' .OR.
     &     IHARI.EQ.'R'.AND.YEARHARF.GT.-99.AND.YEARHARF.EQ.YEARDOY .OR.
     &     IHARI.EQ.'D'.AND.YEARHARF.GT.-99.AND.YEARHARF.EQ.DAP .OR.
     &     IHARI.EQ.'G'.AND.YEARHARF.GT.-99.AND.YEARHARF.LE.XSTAGE .OR.
     &     IHARI.EQ.'A'.AND.YEARHARF.GT.-99.AND.YEARHARF.EQ.YEARDOY .OR.
     &     IHARI.EQ.'M'.AND.XSTAGE.GE.6.0.AND.XSTAGE.LT.7.0 .OR.
     &     YEARHARF.LE.-99 .AND. XSTAGE.GE.6.9.AND.XSTAGE.LT.7.0) THEN
            IF (CFLFAIL.EQ.'Y') THEN
              STGDOY(10) = YEARDOY
              IF (STGDOY(9).EQ.9999999) STGDOY(9) = -99
              IF (STGDOY(8).EQ.9999999) STGDOY(8) = -99
              IF (STGDOY(5).EQ.9999999) STGDOY(5) = -99
              IF (STGDOY(4).EQ.9999999) STGDOY(4) = -99
              IF (STGDOY(3).EQ.9999999) STGDOY(3) = -99
              IF (STGDOY(2).EQ.9999999) STGDOY(2) = -99
              IF (STGDOY(1).EQ.9999999) STGDOY(1) = -99
              WFPAV(6) = WFPAV(ISTAGE)
              NFPAV(6) = NFPAV(ISTAGE)
            ENDIF
            IF (STGDOY(10).EQ.9999999) STGDOY(10) = -99
            STGDOY(6) = YEARDOY
            STGDOY(11) = YEARDOY
            CWADSTG(6) = CWAD
            LAISTG(6) = LAI
            LNUMSTG(6) = LNUMSD
            CNADSTG(6) = CNAD
            
            ! If running CSM use harvfrac to handle automatic mngement
            ! LAH May 2009 Was NE  CHP to check
            IF (FILEIOT .EQ. 'DS4') THEN
              hpc = harvfrac(1)*100.0   ! Harvest %
              hbpc = harvfrac(2)*100.0
            ENDIF  
 
!            WRITE(fnumwrk,*)' '
!            WRITE(fnumwrk,*)'HARVEST REACHED ',YEARDOY
            
            
            PARIUEM = -99.0
            IF (PARADCUM.GT.0.0.AND.PARADICUM.GT.0.0) THEN
!              WRITE (fnumwrk,*)' '
!              WRITE (fnumwrk,'(A53,F5.1,F4.1)')
!     &         ' OVERALL PAR USE EFFICIENCY (INCIDENT,INTERCEPTED) = ',
!     &         CWAD/PARADCUM/10.0, CWAD/PARADICUM/10.0 
               PARIUEM = CWAD/PARADICUM/10.0
            ENDIF  
                  
!            WRITE(FNUMWRK,*) ' '
!            WRITE(FNUMWRK,*) 'INORGANIC NO3 AND NH4 (kg/ha)'
!            WRITE(FNUMWRK,*) ' PROFILE:  '
!            WRITE(FNUMWRK,'(A15,2F6.1)')
!     &        '  SEASON START:',SNO3PROFILEI,SNH4PROFILEI
!            WRITE(FNUMWRK,'(A15,2F6.1)')
!     &        '  SEASON END:  ',SNO3PROFILE,SNH4PROFILE
!            WRITE(FNUMWRK,*) ' ROOTZONE: '
!            WRITE(FNUMWRK,'(A15,2F6.1)')
!     &        '  SEASON END:  ',SNO3ROOTZONE,SNH4ROOTZONE
 
!            WRITE(FNUMWRK,*) ' '
!            WRITE(FNUMWRK,*) 'TOTAL AND AVAILABLE WATER (mm) '
!            WRITE(FNUMWRK,*) ' PROFILE:  '
!            WRITE(FNUMWRK,'(A15,2F6.1)')
!     &        '  SEASON START:',H2OPROFILEI,AH2OPROFILEI
!            WRITE(FNUMWRK,'(A15,2F6.1)') 
!     &       '  SEASON END:  ',H2OPROFILE,AH2OPROFILE
!            WRITE(FNUMWRK,*) ' ROOTZONE: '
!            WRITE(FNUMWRK,'(A15,2F6.1)')
!     &        '  SEASON END:  ',H2OROOTZONE,AH2OROOTZONE
            
            ! Reset crop stage
            ISTAGE = 7
            XSTAGE = 7.0
          ENDIF

          IF(IHARI.EQ.'R'.AND.YRHARF.GT.-99.AND.YEARDOY.LT.YEARHARF)THEN
            IF (XSTAGE.GT.6.9 .AND. YRHARFF .NE. 'Y') THEN
              ! This loop is necessary because of non-sequential staging
              IF (XSTAGE.LT.7.0) THEN
!                WRITE(fnumwrk,*)
!     &           'WAITING FOR HARVEST! YEARDOY,YRHAR ',YEARDOY,YRHARF
                YRHARFF = 'Y'
              ENDIF
            ENDIF
          ENDIF

          ! After harvest residues
          IF (STGDOY(11).EQ.YEARDOY) THEN
            ! Surface
            RESWALG(0) = VWAD*(1.0-HBPC/100.0) + GWAD*(1.0-HPC/100.0)
            RESNALG(0) = (LEAFN+STEMN+DEADN)*PLTPOP*10.*(1.-HBPC/100.)
     &                 + GNAD*(1.0-HPC/100.0)
            RESCALG(0) = RESWALG(0) * 0.4
            RESLGALG(0) = LLWAD*LLIGP/100.0*(1.0-HBPC/100.0)
     &                  + LSHWAD*SLIGP/100.0*(1.0-HBPC/100.0)
     &                  + STWAD*SLIGP/100.0*(1.0-HBPC/100.0)
     &                  + GWAD*GLIGP/100.0*(1.0-HPC/100.0)
            ! Soil
            DO L = 1, NLAYR
              RESWALG(L) = RTWTL(L)*PLTPOP*10.0
              RESNALG(L) = RTWTL(L)*PLTPOP*10.0 * RANC
              RESCALG(L) = RTWTL(L)*PLTPOP*10.0 * 0.4
              RESLGALG(L) = RTWTL(L)*PLTPOP*10.0 * RLIGP/100.0
            ENDDO

            ! Surface
            RESWAL(0) = RESWAL(0) + RESWALG(0)
            RESNAL(0) = RESNAL(0) + RESNALG(0)
            RESCAL(0) = RESCAL(0) + RESCALG(0)
            RESLGAL(0) = RESLGAL(0) + RESLGALG(0)
            ! Soil
            DO L = 1, NLAYR
              RESWAL(L) = RESWAL(L) + RESWALG(L)
              RESNAL(L) = RESNAL(L) + RESNALG(L)
              RESCAL(L) = RESCAL(L) + RESCALG(L)
              RESLGAL(L) = RESLGAL(L) + RESLGALG(L)
            ENDDO
          ENDIF

          ! Weather summary variables
          CUMTT = CUMTT + TT
          TMAXX = AMAX1(TMAXX,TMAX)
          TMINN = AMIN1(TMINN,TMIN)
          CO2MAX = AMAX1(CO2MAX,CO2)
          RAINC = RAINC + RAIN
          IF (ADAT.LT.0) RAINCA = RAINCA + RAIN
          SRADC = SRADC + SRAD
          IF (XSTAGE.GE.5.0 .AND. XSTAGE.LT.6.0) THEN
            GFTSUM = GFTSUM + TMEAN
            GFDSUM = GFDSUM + 1
            GFTMEAN = GFTSUM/GFDSUM
          ENDIF
          IF (XSTAGE.GE.5.7 .AND. XSTAGE.LT.6.0) THEN
            GMTSUM = GMTSUM + TMEAN
            GMDSUM = GMDSUM + 1
            GMTMEAN = GMTSUM/GMDSUM
          ENDIF
          IF (XSTAGE.GE.8.0 .AND. XSTAGE.LT.10.0) THEN
            GETSUM = GETSUM + TMEAN
            GEDSUM = GEDSUM + 1
            GETMEAN = GETSUM/GEDSUM
          ENDIF
          CALL Calendar (year,doy,dom,month)
          IF (DOM.GT.1) THEN
            TMAXSUM = TMAXSUM + TMAX
            TMINSUM = TMINSUM + TMIN
            DAYSUM = DAYSUM + 1.0
          ELSE
            IF (DAYSUM.GT.0) THEN
              IF (TMAXM.LT.TMAXSUM/DAYSUM) TMAXM=TMAXSUM/DAYSUM
              IF (TMINM.GT.TMINSUM/DAYSUM) TMINM=TMINSUM/DAYSUM
            ENDIF
              TMAXSUM = TMAX
              TMINSUM = TMIN
              DAYSUM =  1
          ENDIF

          ! N fertilizer applications
          IF (NFERT.GT.0.AND.IFERI.EQ.'R'.AND.YEARDOY.EQ.YEARPLT) THEN
            DO I = 1, NFERT
              IF (FDAY(I).GT.YEARDOY) EXIT
              IF (FDAY(I).LE.-99) EXIT
              AMTNIT = AMTNIT + ANFER(I)
            END DO
!            IF (FILEIOT.EQ.'XFL') WRITE(fnumwrk,*)' '
!            WRITE(fnumwrk,'(A24,I4,A6)')
!     &       ' Fertilizer N pre-plant ',NINT(amtnit),' kg/ha'
          ENDIF
          IF (NFERT.GT.0.AND.IFERI.EQ.'R'.AND.YEARDOY.GT.YEARPLT) THEN
            DO I = 1, NFERT
              IF (FDAY(I).GT.YEARDOY) EXIT
              IF (FDAY(I).EQ.YEARDOY) THEN
                AMTNIT = AMTNIT + ANFER(I)
!                WRITE(fnumwrk,'(A14,I4,A10,I9,A13,I4,A6)')
!     &          ' Fertilizer N ',NINT(anfer(i)),' kg/ha on ',
!     &          YEARDOY,'     To date ',NINT(amtnit),' kg/ha'
              ENDIF
            END DO
          ENDIF

          ! Adjustment of kernel growth rate
          ! Originally set temperature response here
          IF (ISTAGE.EQ.5.AND.ISTAGEP.EQ.4) THEN
!            WRITE(fnumwrk,*)'Start of linear kernel growth    '
!            WRITE(fnumwrk,*)' Original kernel growth rate (G2) ',g2
!           chp handle zero divide
            IF (GRNUM .GT. 1.E-6) THEN
              G2 = (G2KWT-(GRWT/GRNUM)*1000.0) / (PD(5)*(6.0-XSTAGE))
            ELSE
              G2 = (G2KWT) / (PD(5)*(6.0-XSTAGE))
            ENDIF
!            WRITE(fnumwrk,*)' Adjusted kernel growth rate (G2) ',g2
!            WRITE(fnumwrk,*)' (Adjustment because growing at lag rate',
!     &      ' for overlap into linear filling period)'
          ENDIF

          ! Stored variables (For use next day or step)
          ISTAGEP = ISTAGE
          ZSTAGEP = ZSTAGE
          RSTAGEP = RSTAGE
          DAYLTP = DAYLT

          ! Soil water aspects
          BLAYER = 0.0
          H2OA = 0.0
          IF (ISWWAT.EQ.'Y') THEN
            DO L = 1, NLAYR
              DLAYRTMP(L) = DLAYR(L)
              BLAYER = BLAYER + DLAYR(L)
              IF (RTDEP.GT.0.0.AND.RTDEP.LT.BLAYER) THEN
                DLAYRTMP(L) = RTDEP-(BLAYER-DLAYR(L))
                IF (DLAYRTMP(L).LE.0.0) EXIT
              ENDIF
              H2OA = H2OA + 10.0*AMAX1(0.0,(SW(L)-LL(L))*DLAYRTMP(L))
            ENDDO
            IF (EOP.GT.0.0) THEN
              WAVR = H2OA/EOP
            ELSE
              WAVR = 99.9
            ENDIF
          ENDIF
 
          H2OPROFILE = 0.0
          H2OROOTZONE = 0.0
          AH2OPROFILE = 0.0
          AH2OROOTZONE = 0.0
          DO L = 1, NLAYR
            AH2OPROFILE = AH2OPROFILE + ((SW(L)-LL(L))*DLAYR(L))*10.0
            H2OPROFILE = H2OPROFILE + SW(L)*DLAYR(L)*10.0
            IF (RLV(L).GT.0.0) THEN
              AH2OROOTZONE = AH2OROOTZONE + ((SW(L)-LL(L))*DLAYR(L))*10.
              H2OROOTZONE = H2OROOTZONE + SW(L)*DLAYR(L)*10.
            ENDIF
          END DO

        ENDIF


      END SUBROUTINE CER_Integrate