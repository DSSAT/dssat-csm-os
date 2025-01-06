!***********************************************************************
! This is the code from the section (DYNAMIC == INTEGR) 
! lines 7649 - 9221 of the original CSCRP code.
!***********************************************************************

      SUBROUTINE CRP_Integrate (ALBEDOS, BD, GSTAGE, LAI, CANHT, CO2,
     &     DAYLT, DEPMAX, DLAYR, DOY, DRAIN, EOP, EP, ET, FERNIT,
     &     IRRAMT, ISWNIT, ISWWAT, LL, NFP, NH4LEFT, NLAYR,
     &     NO3LEFT, RAIN, RESCALG, RESLGALG, RESNALG, RLV, RUNOFF,
     &     SRAD, STGYEARDOY, SW, TLCHD, TMAX, TMIN, TNIMBSOM,
     &     TNOXD, TOMINFOM, TOMINSOM, TOMINSOM1, TOMINSOM2,
     &     TOMINSOM3, YEAR)
    
      USE ModuleDefs
      USE CRP_First_Trans_m
      IMPLICIT NONE
      EXTERNAL WARNING, CSTIMDIF, Calendar

      INTEGER DOY, NLAYR, STGYEARDOY(20), YEAR                  
      INTEGER CSTIMDIF  !CSIDLAYR,                  
      REAL ALBEDOS, BD(NL), GSTAGE, CANHT, CO2  !, CLOUDS
      REAL EOP, LL(NL), NFP, NH4LEFT(NL)  !DUL(NL), EO, ES, KCAN, kep, 
      REAL NO3LEFT(NL), RLV(NL)   !PARIP, PARIPA, SAT(NL),    
!     REAL SENCALG(0:NL), SENLALG(0:NL), SENNALG(0:NL), SHF(NL), SLPF
      REAL SRAD, SW(NL), TMAX, TMIN !, ST(NL), TAIRHR(24), TDEW, TRWUP
      REAL LAI !UH2O(NL), UNH4(NL), UNO3(NL), WINDSP, 
      REAL DLAYR(NL), DEPMAX, DAYLT, DRAIN, EP, ET, FERNIT, IRRAMT
      REAL RAIN, RTWTSGE, RUNOFF, TLCHD, TNIMBSOM, TNOXD, TOMINFOM
      REAL TOMINSOM, TOMINSOM1, TOMINSOM2, TOMINSOM3, VPENDFR
      REAL RESNALG(0:NL), RESCALG(0:NL), RESLGALG(0:NL) 
      
!     REAL CSVPSAT, CSYVAL, TFAC4
!     REAL YVALXY
      CHARACTER(LEN=1) ISWNIT, ISWWAT !, ISWDIS, RNMODE  

!=======================================================================
        IF (YEARDOY.GE.PLYEARDOY) THEN
!=======================================================================

!-----------------------------------------------------------------------
!         Update ages
!-----------------------------------------------------------------------

          ! Leaves that present at beginning of day
          DO L = 1,LNUMSG
            LAGEDU(L) = LAGEDU(L) + DULF
            LAGEP(L) = LAGEP(L) + LNUMG
          ENDDO
          ! New leavea   
          IF (LNUMG.GT.0.0) THEN
            IF (LNUMSG.LT.LNUMX) THEN
              LAGEP(LNUMSG+1)=LAGEP(LNUMSG+1)+AMAX1(0.0,LNUMG-LNUMNEED)
              LAGEDU(LNUMSG+1) = LAGEDU(LNUMSG+1)+
     &         DULF*AMAX1(0.0,LNUMG-LNUMNEED)/LNUMG
            ENDIF  
          ENDIF

!-----------------------------------------------------------------------
!         Update dry weights
!-----------------------------------------------------------------------

          ! Dry weights
          ! LAH No growth/development on planting day
          IF (YEARDOY.GE.PLYEARDOY) THEN   

            ! Assimilation and respiration
            CARBOC = CARBOC + AMAX1(0.0,(CARBOBEG+CARBOADJ))
            RESPRC = RESPRC + RTRESP
            RESPTC = 0.0  ! Respiration tops - not yet used
            RESPC = RESPRC + RESPTC
            ! Variables for balancing during grain fill
            IF (CUMDU.GE.SGPHASEDU(2).AND.CUMDU.LT.PSTART(MSTG)) THEN
              RESPGF = RESPGF + RTRESP
              CARBOGF = CARBOGF + AMAX1(0.0,CARBOBEG+CARBOADJ)
            ENDIF

            LFWT = LFWT + GROLF - SENLFG - SENLFGRS - LWPH
            LWPHC = LWPHC +  LWPH

            IF (LFWT.LT.-1.0E-8) THEN
              WRITE(Message(1),'(A35,F4.1,A14)')
     &         'Leaf weight less than 0! Weight of ',lfwt,
     &         ' reset to zero'
              CALL WARNING(1,'CSCRP',MESSAGE)
!              WRITE(Fnumwrk,*)' '
!              WRITE(Fnumwrk,'(A36,F4.1,A14)')
!     &         ' Leaf weight less than 0! Weight of ',lfwt,
!     &         ' reset to zero'
              LFWT = 0.0
            ENDIF
            RSWT = RSWT + 
     &             GRORS + GRORSGR - SENRS - RSWPH - RTWTGRS
            RSWPHC = RSWPHC +  RSWPH
            ! Reserves distribution 
            ! Max concentration in leaves increases through life cycle.
            IF (PSTART(MSTG).GT.0.0) LLRSWT = AMIN1(RSWT,
     &       LFWT*(1.0-LSHFR)*(RSPCLX/100.0)*CUMDU/PSTART(MSTG))
            IF (PSTART(MSTG).GT.0.0) LSHRSWT = AMIN1(RSWT-LLRSWT,
     &       LFWT*LSHFR*(RSPCLX/100.0)*CUMDU/PSTART(MSTG))
            IF (STWT+CHWT.GT.0.0) THEN
              STRSWT = (RSWT-LLRSWT-LSHRSWT)*STWT/(STWT+CHWT)
              CHRSWT = (RSWT-LLRSWT-LSHRSWT)*CHWT/(STWT+CHWT)
            ELSE
              STRSWT = (RSWT-LLRSWT-LSHRSWT)
              CHRSWT = 0.0
            ENDIF

            IF (RSWT.LT.0.0) THEN
              IF (ABS(RSWT).GT.1.0E-6) THEN
                WRITE(Message(1),'(A30,A11,F12.9)')
     &           'Reserves weight reset to zero.',
     &           'Weight was ',rswt
                CALL WARNING(1,'CSCRP',MESSAGE)
!                WRITE(Fnumwrk,*)' '
!                WRITE(Fnumwrk,'(A31,A11,F12.9)')
!     &           ' Reserves weight reset to zero.',
!     &           'Weight was ',rswt
                RSWT = 0.0
              ENDIF
            ENDIF

            RSWTX = AMAX1(RSWTX,RSWT)
            STWT = STWT + GROST - SENSTG - SENSTGRS - SWPH
            IF (STWT.LT.1.0E-06) THEN
!              IF (STWT.LT.0.0) 
!     &         WRITE(fnumwrk,*)'Stem weight less than 0! ',STWT
              STWT = 0.0
            ENDIF
            SWPHC = SWPHC +  SWPH
            STVWT = STVWT + STVWTG
            GRWT = GRWT + GROGR - GWPH
            GWPHC = GWPHC + GWPH
            CHWT = CHWT + GROCH 
            SENTOPLITTER = SENTOPLITTER + SENTOPLITTERG
            SENTOPRETAINED = 
     &        SENTOPRETAINED + (SENLFG+SENSTG+SENRS)*(1.0-SENFR) - DWRPH
            DWRPHC = DWRPHC + DWRPH
            SENCL(0) = SENCL(0) + SENTOPLITTERG*0.4
            SENLL(0) = SENLL(0)
     &       + (SENLFG*LLIGPC/100+SENSTG*SLIGPC/100)*(SENFR)
            RTWT = 0.0
            DO L = 1, NLAYR
              RTWTL(L) = RTWTL(L) + RTWTGL(L) - RTWTSL(L) - RTWTUL(L)
              SENWL(L) = SENWL(L) + RTWTSL(L)
              SENCL(L) = SENCL(L) + RTWTSL(L) * 0.4
              SENLL(L) = SENLL(L) + RTWTSL(L) * RLIGPC/100.0
              ! Totals
              RTWT = RTWT + RTWTL(L)
              SENROOT = SENROOT + RTWTSL(L)
              SENCS = SENCS + RTWTSL(L) * 0.4
              SENLS = SENLS + RTWTSL(L) * RLIGPC/100.0
            END DO
          ENDIF

          SEEDRS = AMAX1(0.0,SEEDRS-CARBOLSD-SEEDRSAVR)
          IF (CFLSDRSMSG.NE.'Y'.AND.SEEDRS.LE.0.0.AND.LNUM.LT.4.0) THEN
            CFLSDRSMSG = 'Y'
            IF (LAI.LE.0.0) THEN
              WRITE (Message(1),'(A41)')
     &          'No seed reserves to initiate leaf growth '
              WRITE (Message(2),'(A33,F8.3,F6.1)')
     &          '  Initial seed reserves,seedrate ',seedrsi,sdrate
              WRITE (Message(3),'(A33,F8.3,F6.1)')
     &          '  Reserves %,plant population    ',sdrspc,pltpop 
              CALL WARNING(3,'CSCRP',MESSAGE)
!              WRITE (Fnumwrk,*)' '
!              WRITE (Fnumwrk,'(A55,I8)')
!     &        ' WARNING  Before emergence but no seed reserves left on',
!     &          Yeardoy
!              WRITE (Fnumwrk,*)
!     &          ' Initial seed reserves,seedrate ',seedrsi,sdrate
!              WRITE (Fnumwrk,*)
!     &          ' Reserves %                     ',sdrspc    
!              WRITE (Fnumwrk,*)
!     &          'Should reduce the rate at which reserves are used.'    
!              WRITE (Fnumwrk,*)
!     &          '(This is done by increasing the period over which '
!              WRITE (Fnumwrk,*)
!     &          ' reserves are used [SDUR in the species file])'    
!              WRITE (Fnumwrk,'(A53)')
!     &          ' Program should stop but to allow the run to continue'
!              WRITE (Fnumwrk,'(A43)')
!     &          ' the initial seed reserves will be doubled.'
              SEEDRS =  SEEDRS + SEEDRSI
              !CFLFAIL = 'Y'  ! Taken out to allow run to continue
            ELSE
              WRITE(Message(1),'(A44,F3.1)')
     &        'Seed reserves all used but leaf number only ',lnum
              WRITE(Message(2),'(A56)')
     &        'For good establishment seed reserves should last to lf 4'
              WRITE(Message(3),'(A55)')
     &        'Maybe seeds too small or specific leaf area set too low'
              CALL WARNING(3,'CSCRP',MESSAGE)
!              WRITE(Fnumwrk,*)' '
!              WRITE(Fnumwrk,'(A36,I7,A9,F3.1)')
!     &        ' WARNING  Seed reserves all used on ',yeardoy,
!     &        ' Leaf# = ',Lnum
!              WRITE(Fnumwrk,'(A54)')
!     &        ' For good establishment reserves should last to leaf 4'
!              WRITE(Fnumwrk,'(A56)')
!     &        'Maybe seeds too small or specific leaf area set too low'
            ENDIF
          ENDIF


          SEEDUSE = SEEDUSE + CARBOLSD+SEEDRSAVR
          SEEDUSER = SEEDUSER + SEEDRSAVR
          SEEDUSET = SEEDUSET + CARBOLSD
          SEEDRSAV = SEEDRS

          RSWTPM = RSWTPM + GRORSPM
          SENGF = SENGF + SENRTGGF + SENTOPLITTERGGF

          IF (GNOPD.GT.0.0) GWUD = GRWT/GNOPD

          IF ((LFWT+STWT+GRWT+RSWT).GT.0.0) THEN
!            HIAD = GRWT/(LFWT+STWT+GRWT+RSWT+SENTOPRETAINED)
            HIAD = (GRWT/(1.0-GMPCH/100.0))
     &             / (LFWT+STWT+GRWT+RSWT+SENTOPRETAINED)
          ENDIF
          IF (RTWT.GT.0.0)
     &     SHRTD = (LFWT+STWT+GRWT+RSWT+SENTOPRETAINED) / RTWT

!-----------------------------------------------------------------------
!         Calculate reserve concentration
!-----------------------------------------------------------------------

          IF (LFWT+STWT+CHWT.GT.0.0) RSCD = RSWT/(LFWT+STWT+CHWT+RSWT)
          IF (RSCD.LT.0.0.AND.RSCD.GT.-1.0E-7) RSCD = 0.0
          RSCX = AMAX1(RSCX,RSCD)

!-----------------------------------------------------------------------
!         Update tiller leaf area (Must be done before PLA updated)
!-----------------------------------------------------------------------

          ! First for leaf senescence
          DO L = 1,INT(TNUM+1)
            IF (TNUM-FLOAT(L-1).GT.0.0) THEN
              IF (PLA-SENLA.GT.0.0) TLAS(L) = TLAS(L) +
     &             PLAS*(TLA(L)-TLAS(L))/(PLA-SENLA)
            ENDIF
          ENDDO

!-----------------------------------------------------------------------
!         Update produced leaf area
!-----------------------------------------------------------------------

          IF (DULF.GT.0.0) THEN
            DO L = 1,LNUMSG+1
              IF (LNUMSG.LT.LNUMX) THEN
                IF (PLAGTP(1)+PLAGTP(2).GT.0.0)
     &           LATL(1,L) = LATL(1,L)+PLAGLF(L)*
     &           ((PLAGT(1)+PLAGT(2))/(PLAGTP(1)+PLAGTP(2)))
              ENDIF
            ENDDO
            PLA = PLA + PLAGT(1) + PLAGT(2)
            PLAX = AMAX1(PLAX,PLA)
            LAP(LNUMSG) = LAP(LNUMSG) + PLAGT(1)
            IF (LNUMSG.LT.LNUMX)
     &       LAP(LNUMSG+1) = LAP(LNUMSG+1) + PLAGT(2)

            DO L = 1,INT(TNUM+1)
              IF (TNUM.GE.1.0.OR.TNUM-FLOAT(L-1).GT.0.0) THEN
                TLA(L) = TLA(L) + TLAG(L)
              ENDIF
            ENDDO

            IF (LCNUM.LT.LCNUMX) THEN
              IF (PLAGT(1).GT.0.0) THEN
                LCNUM = LCNUM+1
                LCOA(LCNUM) = PLAGT(1)
              ENDIF
              IF (LCNUM.LT.LCNUMX.AND.PLAGT(2).GT.0.0001) THEN
                LCNUM = LCNUM+1
                LCOA(LCNUM) = PLAGT(2)
              ELSE
                IF (LCNUM.GT.0)              
     &           LCOA(LCNUM) = LCOA(LCNUM) + PLAGT(2)
              ENDIF
            ELSE
              LCOA(LCNUM) = LCOA(LCNUM) + PLAGT(1) + PLAGT(2)
            ENDIF

          ENDIF

!-----------------------------------------------------------------------
!         Update senesced and harvested leaf area
!-----------------------------------------------------------------------

          SENLA = SENLA + PLAS
          SENLALITTER = SENLALITTER + PLAS * SENFR
          SENLARETAINED = SENLARETAINED + PLAS * (1.0-SENFR)
    	    ! Grazed leaf area
          LAPHC = LAPHC + LAPH
          ! Distribute senesced leaf over leaf positions and cohorts
          ! Leaf positions
          PLASTMP = PLAS - PLASP
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
            ! Cohorts
            PLASTMP2 = AMAX1(0.0,PLAS)
            DO L = 1, LCNUM
              IF (LCOA(L)-LCOAS(L).GT.PLASTMP2) THEN
                LCOAS(L) = LCOAS(L) + PLASTMP2
                PLASTMP2 = 0.0
              ELSE
                PLASTMP2 = PLASTMP2 - (LCOA(L)-LCOAS(L))
                LCOAS(L) = LCOA(L)
              ENDIF
              IF (PLASTMP2.LE.0.0) EXIT
            ENDDO
          ENDIF
          ! Distribute harvested leaf over leaf positions and cohorts
          ! Leaf positions
	    IF (LNUMSG.GT.0 .AND. LAPH.GT.0) THEN
            DO L = 1, LNUMSG
		      IF (LAP(L)-LAPS(L).GT.0.0)
     &	       LAPS(L) = LAPS(L) + (LAP(L)-LAPS(L)) * HAFR
		    ENDDO
            ! Cohorts
            DO L = 1, LCNUM
              IF (LCOA(L)-LCOAS(L).GT.0.0) THEN
                LCOAS(L) = LCOAS(L) + (LCOA(L)-LCOAS(L)) * HAFR
              ENDIF
            ENDDO
          ENDIF

!-----------------------------------------------------------------------
!         Update (green) leaf area                            
!-----------------------------------------------------------------------

          LAPD = AMAX1(0.0,(PLA-SENLA-LAPHC))
          LAI = AMAX1(0.0,(PLA-SENLA-LAPHC)*PLTPOP*0.0001)
          LAIX = AMAX1(LAIX,LAI)

!-----------------------------------------------------------------------
!         Update specific leaf area
!-----------------------------------------------------------------------

          SLA = -99.0
          IF (LFWT.GT.1.0E-6) SLA=(PLA-SENLA-LAPHC) / (LFWT*(1.0-LSHFR))
            
!-----------------------------------------------------------------------
!         Update leaf sheath,stem,and awn area
!-----------------------------------------------------------------------

          IF (TSSTG.LE.0.OR.LLSTG.LE.0) THEN
            LSHAI = (LFWT*LSHFR*LSHAV)*PLTPOP*0.0001
          ELSE
            IF (RSTAGE.LE.TSSTG) THEN
              LSHAI = (LFWT*LSHFR*LSHAV)*PLTPOP*0.0001
            ELSEIF (RSTAGE.GT.TSSTG.AND.RSTAGE.LT.LLSTG) THEN
              LSHAW = LSHAV+((LSHAR-LSHAV)*(RSTAGE-TSSTG))
              LSHAI = (LFWT*LSHFR*LSHAW)*PLTPOP*0.0001
            ELSE
              ! Use RSEN as temporary fix for sheath senescence
              LSHAI = LSHAI * (1.0-(RSEN/100.0)*TT/STDAY)  
            ENDIF  
          ENDIF  

          STAI = STAI + STAIG - STAIS

          IF (CUMDU.GT.PSTART(IESTG).AND.AWNS.GT.0.0)
     &     AWNAI = AWNAI + STAIG*AWNS/10.0

          SAID = STAI+AWNAI+LSHAI
          CAID = LAI + SAID

!-----------------------------------------------------------------------
!         Update height
!-----------------------------------------------------------------------

          CANHT = CANHT + CANHTG

!-----------------------------------------------------------------------
!         Update tiller numbers (Limited to a maximum of 20 per plant)
!-----------------------------------------------------------------------

          TNUM = AMIN1(TILNOX,AMAX1(1.0,TNUM+TNUMG-TNUMD-TNUMLOSS))
          SPNUMHC = SPNUMHC + SPNUMH  ! Harvested
          IF (LNUMSG.GT.0) TNUML(LNUMSG) = TNUM
          IF (TNUM-FLOAT(INT(TNUM)).GT.0.0.AND.
     &     TILBIRTHL(INT(TNUM+1)).LE.0.0) THEN
            IF (ABS(TNUM-TNUMPREV) > 1.E-6) THEN   
              TILBIRTHL(INT(TNUM)+1) = LNUMPREV
     &        + (FLOAT(INT(TNUM))-TNUMPREV)/(TNUM-TNUMPREV)
     &        * (LNUM-LNUMPREV)
            ELSE
              TILBIRTHL(INT(TNUM)+1) = LNUMPREV
            ENDIF
          ENDIF
          TNUMX = AMAX1(TNUMX,TNUM)

!-----------------------------------------------------------------------
!         Update plant number
!-----------------------------------------------------------------------

          PLTPOP = PLTPOP - PLTLOSS
          IF (PLTPOP.LE.0.0) THEN
            CFLFAIL = 'Y'
            Write (Message(1),'(A23)') 'Plant population < 0.0  '
            Write (Message(2),'(A12,F4.1)') 'Plant loss: ',pltloss      
            WRITE (Message(3),'(A20)') 'Crop failure assumed'
            CALL WARNING(3,'CSCRP',MESSAGE)
!            Write (Fnumwrk,*) ' '
!            Write (Fnumwrk,'(A23)') ' Plant population < 0.0 '
!            Write (Fnumwrk,'(A13,F6.1)') ' Plant loss: ',pltloss      
!            WRITE (Fnumwrk,'(A21)') ' Crop failure assumed'
          ENDIF

!-----------------------------------------------------------------------
!         Update root depth and length
!-----------------------------------------------------------------------

          IF (SDEPTH.GT.0.0 .AND.RTDEP.LE.0.0) RTDEP = SDEPTH
          RTDEP = AMIN1 (RTDEP+RTDEPG,DEPMAX)
          DO L = 1, NLAYR
            RLV(L)=RTWTL(L)*RLWR*PLTPOP/DLAYR(L)   ! cm/cm3
            IF (L.EQ.NLAYR.AND.RLV(L).GT.0.0)THEN
              IF (RTSLXDATE.LE.0.0) RTSLXDATE = YEARDOY
            ENDIF
          END DO

!-----------------------------------------------------------------------
!         Update nitrogen amounts
!-----------------------------------------------------------------------
          NUPC = NUPC + NUPD
          LEAFNEXCESS = 0.0
          IF (LANC.GT.LNCX)
     &      LEAFNEXCESS = (LFWT-SENLFG-SENLFGRS)*(LANC-LNCX)
         
          LEAFN = LEAFN + GROLFRTN + LNUSE(0)
     &          - STEMNGL - GRAINNGL - SENNLFG - SENNLFGRS - LNPH
     &          - LEAFNEXCESS
          LNPHC = LNPHC +  LNPH
          IF (LEAFN.LT.1.0E-10) LEAFN = 0.0
          STEMNEXCESS = 0.0
          IF (SANC.GT.SNCX)
     &      STEMNEXCESS = (STWT-SENSTG-SENSTGRS)*(SANC-SNCX)
          STEMN = STEMN + STEMNGL + SNUSE(0)
     &          - GRAINNGS - SENNSTG - SENNSTGRS - SNPH
     &          - STEMNEXCESS
          SNPHC = SNPHC +  SNPH
          IF (STEMN.LT.1.0E-10) STEMN = 0.0
          ROOTNS = 0.0
          SENNGS = 0.0
          DO L = 1, NLAYR
            SENNL(L) = SENNL(L) + RTNSL(L)
            ROOTNS = ROOTNS + RTNSL(L)
            SENNS = SENNS + RTNSL(L)
            SENNGS = SENNGS + RTNSL(L)
          END DO
          ! CHECK IF LANC > LNCX
          ROOTNEXCESS = 0.0
          IF (RANC.GT.RNCX .AND. PLTPOP .GT. 0.0)
     &      ROOTNEXCESS = (RTWT-(SENWALG(L)/(PLTPOP*10.0)))*(RANC-RNCX)
          ROOTN = ROOTN + (RNUSE(0)-GRAINNGR-ROOTNS-GROLFRTN)
     &                  - ROOTNEXCESS
          SEEDN = SEEDN - SEEDNUSE
          IF (SEEDN.LT.1.0E-6) SEEDN = 0.0
          GRAINN = GRAINN + GRAINNGU + GRAINNGL + GRAINNGS
     &      + GRAINNGR + GRAINNGRS - GNPH
          RSN = RSN - GRAINNGRS - RSNUSED
     &        - SENNRS - RSNPH + SENNLFGRS + SENNSTGRS
     &        + LEAFNEXCESS + STEMNEXCESS + ROOTNEXCESS
          RSNPHC = RSNPHC +  RSNPH
          IF (LRETSDU.GT.0.0.AND.LRETSDU.LT.CUMDU) THEN
            DEADN = DEADN + SENNLFG + SENNSTG
          ELSE
            SENNL(0) = SENNL(0) + SENNLFG + SENNSTG + SENNRS
          ENDIF

          ! Harvest index for N
          HIND = 0.0
          IF ((LEAFN+STEMN+GRAINN+RSN+DEADN).GT.0.0)
     &     HIND = GRAINN/(LEAFN+STEMN+GRAINN+RSN+DEADN)

!-----------------------------------------------------------------------
!         Update stages
!-----------------------------------------------------------------------

          ! STAGES:Germination and emergence (Gestages)
          ! NB 0.5 factor used to equate to Zadoks)
          GEUCUM = GEUCUM + TTGEM*WFGE
          IF (GEUCUM.LT.PEGD) THEN
            GESTAGE = AMIN1(1.0,GEUCUM/PEGD*0.5)
          ELSE
            IF (PEMRG*SDEPTHU > 1.E-6) THEN 
              GESTAGE = AMIN1(1.0,0.5+0.5*(GEUCUM-PEGD)/(PEMRG*SDEPTHU))
            ELSE
              GESTAGE = 1.0
            ENDIF    
          ENDIF
          
          ! Germination conditions  
          IF (GESTAGEPREV.LT.0.5) THEN
            TMEANGC = TMEANGC + TMEAN
            GEDAYSG = GEDAYSG + 1
            TMEANG = TMEANGC/GEDAYSG
          ENDIF  

          ! Germination to emergence conditions  
          IF (GESTAGE.LT.0.5) THEN
            GOTO 6666
          ELSEIF (GESTAGEPREV.LT.1.0) THEN
            TMEANEC = TMEANEC + TMEAN
            GEDAYSE = GEDAYSE + 1
            TMEANE = TMEANEC/GEDAYSE
          ENDIF

          ! STAGES:Overall development
          CUMDU = CUMDU + DU
          CUMTT = CUMTT + TT
          IF (PSTART(MSTG).GT.0.0) THEN
            DSTAGE = CUMDU/PSTART(MSTG)
          ENDIF 

          ! STAGES:Reproductive development (Rstages)
          RSTAGEP = RSTAGE
          IF (GESTAGE.GE.0.5) THEN
            DO L = HSTG,1,-1
              IF (CUMDU.GE.PSTART(L).AND.PD(L).GT.0.0) THEN
                RSTAGE = FLOAT(L) + (CUMDU-PSTART(L))/PD(L)
                ! Rstage cannot go above harvest stage 
                RSTAGE = AMIN1(FLOAT(HSTG),RSTAGE)
                EXIT
              ENDIF
            ENDDO
          ENDIF

          ! STAGES:Leaf development (Lstages)
          IF (GESTAGE.GE.1.0) THEN
            IF (EYEARDOY.LE.0) THEN
              LFGSDU = CUMDU - DU + TT*VF*PPFPE*(GERMFR-EMRGFR)
              LFGSCUMDU = TT*VF*DF*EMRGFR
            ELSE
              LFGSCUMDU = 
     &           AMAX1(0.0,AMIN1(LGPHASEDU(2)-LFGSDU,LFGSCUMDU+DU))
            ENDIF
            IF (LGPHASEDU(2).GT.0.0)
     &       LSTAGE = AMAX1(0.0,AMIN1(1.0,LFGSCUMDU/LGPHASEDU(2)))
            ENDIF

          ! STAGES:Leaf numbers
          LNUM = AMAX1(0.0,(AMIN1(FLOAT(LNUMX-1),(LNUM+LNUMG))))
          
          LNUMSG = INT(LNUM)+1
          IF (LNUM.GE.FLOAT(LNUMX-1)+0.9.AND.RSTAGE.LT.4.0) THEN
            IF (CCOUNTV.EQ.0) THEN
             WRITE (Message(1),'(A35)')
     &       'Maximum leaf number reached on day '
             CALL WARNING(1,'CSCRP',MESSAGE)
!             WRITE(Fnumwrk,*)' '
!             WRITE (Fnumwrk,'(A36,I8)')
!     &       ' Maximum leaf number reached on day ',yeardoy
            ENDIF
            CCOUNTV = CCOUNTV + 1
            IF (CCOUNTV.EQ.50.AND.VREQ.GT.0.0) THEN
              CFLFAIL = 'Y'
              WRITE (Message(1),'(A34)')
     &         '50 days after maximum leaf number '
              WRITE (Message(2),'(A54)')
     &         'Presumably vernalization requirement could not be met '
              WRITE (Message(3),'(A25)')
     &         'Will assume crop failure.'
              CALL WARNING(3,'CSCRP',MESSAGE)
!              WRITE (Fnumwrk,*)' '
!              WRITE (Fnumwrk,'(A34)')
!     &         '50 days after maximum leaf number '
!              WRITE (Fnumwrk,'(A54)')
!     &         'Presumably vernalization requirement could not be met '
!              WRITE (Fnumwrk,'(A25)')
!     &         'Will assume crop failure.'
            ENDIF
          ENDIF

          ! STAGES:Apical development - double ridges. 
          !  Factors by calibration from LAMS
          !  Only used for comparison with calc based on spe input
          drf1 = 1.9
          drf2 = 0.058
          drf3 = 3.3
          DRSTAGE = AMAX1(1.1,drf1-drf2*(LNUM-drf3))
          IF (DRDAT.EQ.-99 .AND. RSTAGE.GE.DRSTAGE) THEN
            DRDAT = YEARDOY
            DRDAP = MAX(0,CSTIMDIF(PLYEARDOY,YEARDOY))
!            WRITE(fnumwrk,*)' '
!            WRITE(fnumwrk,'(A37,3F7.2,I9)')
!     &       ' Double ridges. Rstage,Drtage,Leaf#: ',
!     &       RSTAGE,DRSTAGE,LNUM,YEARDOY
             ! NB. Experimental. DR occurs at later apical stage when
             !     leaf # less, earlier when leaf # greater (ie.when
             !     early planting of winter type).
          ENDIF
  
          ! STAGES:Stem development (Ststages)
          IF (CUMDU.GT.SGPHASEDU(1).AND.
     &        SGPHASEDU(2)-SGPHASEDU(1).GT.0.)THEN
            CUMDUS =AMAX1(.0,AMIN1(SGPHASEDU(2)-SGPHASEDU(1),CUMDUS+DU))
            STSTAGE = CUMDUS/(SGPHASEDU(2)-SGPHASEDU(1))
          ENDIF

          ! STAGES:Zadoks
          ! Zadoks (1974) codes
          ! CODE     DESCRIPTION                           RSTAGE
          ! 00-09    Germination                                .
          !  00=Dry seed at planting                            .
          !  01=begining of seed imbibition                     .
          !  05=germination (when radicle emerged)            0.0
          !  09=coleoptile thru soil surface                    .
          ! 10-19    Seedling growth                            .
          !  10=first leaf emerged from coleoptile              .
          !  11=first leaf fully expanded                       .
          !  1n=nth leaf fully expanded                         .
          ! 20-29    Tillering, no. of tillers + 20             .
          !  20=first tiller appeared on some plants            .
          !  2n=nth tiller                                      .
          !  21 Main shoot plus 1 tiller                        .
          !  22 Main shoot plus 2 tillers                       .
          ! 30-39    Stem elongation, no. nodes + 30            .
          !  30 Pseudo stem erection                          3.0
          !  31 1st node detectable. Jointing                 3.1
          !  32 2nd node detectable                             .
          !  37 Flag leaf just visible                          .
          !  39 Flag leaf ligule just visible                   .
          ! 40-49    Booting                                    .
          !  40 Flag sheath extending. Last leaf              4.0
          !  45 Boots swollen                                   .
          !  47 Flag sheath opening                             .
          !  49 First awns visible                              .
          ! 50-59    Inflorescence emergence                    .
          !  50 First spikelet just visible. Inflorescence    5.0
          !  59 Inflorescence emergence completed               .
          ! 60-69    Flowering                                  .
          !  60 Beginning of anthesis                         6.0
          !  65 Anthesis half way                               .
          !  69 Anthesis complete                             7.0
          ! 70-79    Milk development                           .
          !  70 Caryopsis water ripe                            .
          !  71 V.early milk                                    .
          !  73 Early milk                                      .
          !  75 Medium milk                                     .
          !  77 Late milk                                       .
          ! 80-89    Dough development                          .
          !  80 Milk -> dough                                 8.0
          !  81 V.early dough                                   .
          !  83 Early dough                                     .
          !  85 Soft dough (= end of grain fill?)               .
          !  87 Hard dought                                     .
          ! 90-99    Ripening                                   .
          !  90 Late hard dough (=end of grain fill?)         9.0
          !  91 Hard;difficult to divide by thumb-nail          .
          !     Binder ripe 16% h2o. Harvest                    .
          !     Chlorophyll of inflorescence largely lost       .
          !  92 Hard;can no longer be dented by thumb-nail   10.0
          !     Combine ripe < 16% h2o                          .
          !  93 Caryopsis loosening in daytime                  .
          !  93 = harvest maturity ?                            .

          IF (GESTAGE.LT.1.0) THEN
              ZSTAGE = GESTAGE * 10.0
          ELSEIF (GESTAGE.GE.1.0.AND.RSTAGE.LE.3.0) THEN
            IF (TNUM.LT.2.0) THEN
              ZSTAGE = AMIN1(20.0,10.0 + LNUM)
            ELSE
              ZSTAGE = AMIN1(30.0,20.0+(TNUM-1.0))
            ENDIF
          ELSEIF (RSTAGE.GT.3.0.AND.RSTAGE.LE.7.0) THEN
            ZSTAGE = 30.0 + 10.0*(RSTAGE-3.0)
            ! LAH Staging hereafter based on data from M.Fernandes:
            !  RSTAGE ZSTAGE
            !    70.6 69
            !    80.7 71
            !    82.8 75
            !    86.1 85
            !    88.1 91
            !ELSEIF (RSTAGE.GT.7.0.AND.RSTAGE.LE.8.0) THEN
            !  ZSTAGE = 70.0 + 2.0*(RSTAGE-7.0)
            !ELSEIF (RSTAGE.GT.8.0.AND.RSTAGE.LE.8.3) THEN
            !  ZSTAGE = 72.0 + 1.0*(RSTAGE-8.0)*10.0
            !ELSEIF (RSTAGE.GT.8.3.AND.RSTAGE.LE.8.6) THEN
            !  ZSTAGE = 75.0 + 3.3*(RSTAGE-8.3)*10.0
            !ELSEIF (RSTAGE.GT.8.6.AND.RSTAGE.LE.9.0) THEN
            !  ZSTAGE = 85.0 + 2.0*(RSTAGE-8.6)*10.0
            !ENDIF
            ! BUT taken out because not match with CSCER045
            ! Need to check when end of milk stage in RSTAGE terms.
          ELSEIF (RSTAGE.GT.7.0.AND.RSTAGE.LE.9.0) THEN
            ZSTAGE = 10.0 * RSTAGE          
          ENDIF

          GSTAGE = ZSTAGE

!-----------------------------------------------------------------------
!         Record stage dates and states
!-----------------------------------------------------------------------

          IF (INT(RSTAGE).GT.10.OR.
     &      INT(RSTAGE).LT.0.AND.GESTAGE.GT.0.5) THEN
            OPEN (UNIT = FNUMERR,FILE = 'ERROR.OUT')
            WRITE(fnumerr,*) ' '
            WRITE(fnumerr,*)
     &       'Rstage out of range allowed for phase thresholds '
            WRITE(fnumerr,*) 'Rstage was: ',rstage
            WRITE(fnumerr,*) 'Please contact model developer'
            WRITE(*,*)
     &       ' Rstage out of range allowed for phase thresholds '
            WRITE(*,*) ' Rstage was: ',rstage
            WRITE(*,*) ' Program will have to stop'
            PAUSE
            CLOSE (fnumerr)
            STOP ' '
          ENDIF
          IF (CUMDU-DU.LT.PSTART(INT(RSTAGE))) THEN
            STGYEARDOY(INT(RSTAGE)) = YEARDOY
            ! NB.Conditions are at start of phase
            IF (DU.GT.0.0) THEN
              CWADSTG(INT(RSTAGE)) = CWADPREV+DUNEED/DU*(CWAD-CWADPREV)
              LAISTG(INT(RSTAGE)) = LAIPREV + DUNEED/DU*(LAI-LAIPREV)
              LNUMSTG(INT(RSTAGE)) = LNUMPREV+DUNEED/DU*(LNUM-LNUMPREV)
              CNADSTG(INT(RSTAGE)) = CNADPREV+DUNEED/DU*(CNAD-CNADPREV)
            ENDIF
          ENDIF

          ! Primary stages
          IF (RSTAGEP.LT.0.0) RSTAGEP = 0.0
          L = INT(RSTAGEP) + 1
          IF (PSDAT(L).LE.0.AND.CUMDU.GE.PSTART(L)) THEN
            PSDAT(L) = YEARDOY
            IF (DU.GT.0.0) PSDAPFR(L)=(PSTART(L)-(CUMDU-DU))/DU
            PSDAPFR(L) = FLOAT(DAP) + PSDAPFR(L)
            PSDAP(L) = DAP
            ! If has jumped a phase 
            ! This was preventing setting ADAT in one case. Taken out. 
            !IF (RSTAGE-RSTAGEP.GT.1.0) THEN
            !  PSDAT(L+1) = YEARDOY
            !  PSDAP(L+1) = DAP
            !ENDIF
            IF (PSABV(L).EQ.'ADAT '.OR.PSABV(L).EQ.' ADAT') THEN
              ADAT = YEARDOY
              ADOY = DOY
              ADAP = DAP
              ADAYFR = TIMENEED
              ADAPFR = FLOAT(ADAP) + ADAYFR
              RSWTA = RSWT - (1.0-ADAYFR)*(GRORS+GRORSGR)
              STWTA = STWT-(1.0-ADAYFR)*(GROST-SENSTG)
              LFWTA = LFWT-(1.0-ADAYFR)*(GROLF-SENLFG-SENLFGRS)
              CWAA = (LFWTA+STWTA+RSWTA)*PLTPOP*10.0
              LFWAA = LFWTA*PLTPOP*10.0
              STWAA = STWTA*PLTPOP*10.0
              RSWAA = RSWTA*PLTPOP*10.0
              ! LAH  Below are not adjusted for adayfr
              CNAA = CNAD
              IF (CWAA.GT.0.0) CNPCA = CNAA/CWAA*100.0
            ENDIF
            IF (PSABV(L).EQ.'AEDAT') THEN
              ADATEND = YEARDOY
              AEDAPFR = FLOAT(DAP) + ADAYEFR
              RSWTAE = RSWT - (1.-ADAYEFR)*(GRORS+GRORSGR)
              LFWTAE = LFWT-(1.0-ADAYEFR)*(GROLF-SENLFG-SENLFGRS)
              STWTAE = STWT - (1.0-ADAYEFR)*(GROST-SENSTG)
              TMEAN20ANTH = TMEAN20
              SRAD20ANTH = SRAD20
              IF ((GNOPD*PLTPOP).LT.100.0) THEN
                WRITE (Message(1),'(A44)')
     &           'Very few grains set! Failure as a grain crop'
                WRITE (Message(2),'(A26,F6.1)')
     &           '  Plant population      = ',pltpop
                WRITE (Message(3),'(A26,F6.1)')
     &           '  Above ground  (kg/ha) = '
     &           ,(LFWT+STWT+RSWT)*pltpop*10.0
                WRITE (Message(4),'(A26,F6.1)')
     &           '  Grain number coeff    = ',gnowts
                WRITE (Message(5),'(A26,F6.1)')
     &           '  Leaf area index       = ',lai
                CALL WARNING(5,'CSCRP',MESSAGE)
                Message = ' '
!                WRITE(Fnumwrk,*)' '
!                WRITE (Fnumwrk,'(A45)')
!     &           ' Very few grains set! Failure as a grain crop'
!                WRITE (Fnumwrk,'(A27,F6.1)')
!     &           '   Plant population      = ',pltpop
!                WRITE (Fnumwrk,'(A27,F6.1)')
!     &           '   Above ground  (kg/ha) = '
!     &           ,(LFWT+STWT+RSWT)*pltpop*10.0
!                WRITE (Fnumwrk,'(A27,F6.1)')
!     &           '   Grain number coeff    = ',gnowts
!                WRITE (Fnumwrk,'(A27,F6.1)')
!     &           '   Leaf area index       = ',lai
              ENDIF
            ENDIF
            IF (PSABV(L).EQ.'MDAT '.OR.L.EQ.MSTG) THEN
              MDAT = YEARDOY
              MDOY = DOY
              MDAP = DAP
              MDAYFR = TIMENEED
              MDAPFR = FLOAT(MDAP) + MDAYFR
              GFDUR = (MDAPFR-GFDAPFR)
            ENDIF
            IF (PSABV(L).EQ.'GFDAT') THEN
              GFDAPFR = FLOAT(DAP) + TIMENEED
            ENDIF
          ENDIF

          ! Secondary stages
          DO L = 1,SSNUM
            IF (SSDAT(L).LE.0 .AND. CUMDU.GE.SSTH(L)) THEN
              SSDAT(L) = YEARDOY
              IF (DU.GT.0.0) SSDAYFR(L) = (SSTH(L)-(CUMDU-DU))/DU
              SSDAPFR(L) = FLOAT(DAP) + SSDAYFR(L)
              SSDAP(L) = DAP
            ENDIF
          ENDDO

          IF (STGEDAT.LE.0.AND.CUMDU.GE.SGPHASEDU(2)) THEN
            STGEDAT = YEARDOY
            IF (DU.GT.0.0) STGEFR = (SGPHASEDU(2)-(CUMDU-DU))/DU
            SGEDAPFR = FLOAT(DAP) + STGEFR
            ! Following for balancing only.Start at beginning of day
            STWTSGE = STWT
            LFWTSGE = LFWT
            DEADWTSGE = SENTOPRETAINED
            RTWTSGE = RTWT
            GRWTSGE = GRWT
            RSWTSGE = RSWT
          ENDIF

          IF (GYEARDOY.LE.0.0.AND.GERMFR.GT.0.0) THEN
            GYEARDOY = PLYEARDOY
            GDAP = DAP
            GDAYFR = 1.0 - GERMFR
            GDAPFR = FLOAT(DAP) + GDAYFR
          ENDIF

          IF (EYEARDOY.LE.0.0.AND.EMRGFR.GT.0.0) THEN
            EYEARDOY = YEARDOY
            EDAP = DAP
            EDAYFR = 1.0 - EMRGFR
            EDAPFR = FLOAT(DAP) + EDAYFR
            DAE = 0
            
!            WRITE(FNUMWRK,*)' '
!            WRITE(FNUMWRK,*)'EMERGENCE ',eyeardoy
!            IF (SEEDRSAV.GT.0.0) THEN
!            WRITE(FNUMWRK,*)'  Rtwtl(1)/seedrsi  ',rtwtl(1)/seedrsi
!            WRITE(FNUMWRK,*)'  Rtwtl(2)/seedrsi  ',rtwtl(2)/seedrsi
!            WRITE(FNUMWRK,*)'  Rtwtl(3)/seedrsi  ',rtwtl(3)/seedrsi
!            ENDIF
!            IF (SEEDNI.GT.0.0) THEN
!            WRITE(FNUMWRK,*)'  Rootn/seedni      ',rootn/seedni
!            ENDIF
!            WRITE(FNUMWRK,*)'  Rtdep            ',rtdep
!            WRITE(FNUMWRK,*)'  Rstage,Cumdu     ',rstage,cumdu

          ENDIF

          IF (TILDAT.LE.0.AND.TNUM.GT.1.0) THEN
            TILDAT = YEARDOY
            TILDAP = DAP
          ENDIF

          ! Vernalization  Process starts at germination,stops at VPEND
          IF (DU.GT.0.0) THEN
            VPENDFR = AMAX1(0.0,AMIN1(1.0,(VPENDDU-CUMDU)/DU))
          ELSE
            VPENDFR = 1.0
          ENDIF
          CUMVD = CUMVD+TFV*GERMFR*VPENDFR-VDLOS
          IF (VREQ.GT.0.0 .AND. (VREQ-VBASE) .GT. 0.0) THEN
            VRNSTAGE =AMAX1(0.,AMIN1(1.,(CUMVD-VBASE)/(VREQ-VBASE)))
          ELSE
            VRNSTAGE = 1.0
          ENDIF
          ! Vernalization  Effect starts at germination,stops at VEEND
          IF (CUMDU.LE.VEENDDU) THEN
            VF = AMAX1(0.,(1.0-VEFF) + VEFF*VRNSTAGE)
            IF (CUMDU+DU.LT.VEENDDU) THEN
              VFNEXT = VF
            ELSE
              VFNEXT = 1.0
            ENDIF
          ELSE
            VF = 1.0
            VFNEXT = 1.0
          ENDIF

          ! STAGES:Cold hardening
          ! NB. Hardening starts at germination,does not stop
          ! Hardening loss occurs throughout growth cycle
          HARDAYS = HARDAYS+TFH*GERMFR-HARDILOS
          IF (HDUR.GT.1.0) THEN
            HSTAGE = AMIN1(1.0,HARDAYS/HDUR)
          ELSE
            HSTAGE = 1.0
          ENDIF
          TKILL = TKUH + (TKFH-TKUH)*HSTAGE

!-----------------------------------------------------------------------
!         Calculate phase and crop cycle conditions
!-----------------------------------------------------------------------

          IF (WFG.LT.0.99999) WSDAYS = WSDAYS + 1
          IF (NFG.LT.0.99999) NSDAYS = NSDAYS + 1

          IF (GESTAGE.GT.0.1) THEN
            IF (CUMDU-DU.LT.PSTART(INT(RSTAGE))) THEN
              IF (INT(RSTAGE).EQ.8)
     &        TMAXGFILL = AMAX1(TMAX,TMAXGFILL) 
              TMAXPC = 0.0
              TMINPC = 0.0
              TMEANPC = 0.0
              SRADPC = 0.0
              DAYLPC = 0.0
              NFPPC = 0.0
              NFGPC = 0.0
              WFPPC = 0.0
              WFGPC = 0.0
              CO2PC = 0.0
            ENDIF
            TMAXPC = TMAXPC + TMAX
            TMINPC = TMINPC + TMIN
            TMEANPC = TMEANPC + TMEAN
            SRADPC = SRADPC + SRAD
            DAYLPC = DAYLPC + DAYLT
            TMAXCC = TMAXCC + TMAX
            TMINCC = TMINCC + TMIN
            TMEANCC = TMEANCC + TMEAN
            SRADCC = SRADCC + SRAD
            CO2CC = CO2CC + CO2
            DAYLCC = DAYLCC + DAYLT
            RAINCC = RAINCC + RAIN
            
            RAINPC(INT(RSTAGE)) = RAINPC(INT(RSTAGE)) + RAIN
            ETPC(INT(RSTAGE))   = ETPC(INT(RSTAGE)) + ET 
            EPPC(INT(RSTAGE))   = EPPC(INT(RSTAGE)) + EP 
            
            CO2PC = CO2PC + CO2
            NFPPC = NFPPC + NFP
            NFGPC = NFGPC + NFG
            WFPPC = WFPPC + WFP
            WFGPC = WFGPC + WFG
            NFPCC = NFPCC + NFP
            NFGCC = NFGCC + NFG
            WFPCC = WFPCC + WFP
            
            WFGCC = WFGCC + WFG
            ETCC   = ETCC + ET
            EPCC   = EPCC + EP 
            
            PDAYS(INT(RSTAGE)) = PDAYS(INT(RSTAGE)) + 1
            CDAYS = CDAYS + 1
            IF (PDAYS(INT(RSTAGE)).GT.0) THEN
              TMAXPAV(INT(RSTAGE)) = TMAXPC / PDAYS(INT(RSTAGE))
              TMINPAV(INT(RSTAGE)) = TMINPC / PDAYS(INT(RSTAGE))
              TMEANPAV(INT(RSTAGE)) = TMEANPC / PDAYS(INT(RSTAGE))
              SRADPAV(INT(RSTAGE)) = SRADPC / PDAYS(INT(RSTAGE))
              DAYLPAV(INT(RSTAGE)) = DAYLPC / PDAYS(INT(RSTAGE))
              DAYLST(INT(RSTAGE)) = DAYLT
              CO2PAV(INT(RSTAGE)) = CO2PC / PDAYS(INT(RSTAGE))
              RAINPAV(INT(RSTAGE)) = 
     &         RAINPC(INT(RSTAGE)) / PDAYS(INT(RSTAGE))
              NFPPAV(INT(RSTAGE)) = NFPPC / PDAYS(INT(RSTAGE))
              NFGPAV(INT(RSTAGE)) = NFGPC / PDAYS(INT(RSTAGE))
              WFPPAV(INT(RSTAGE)) = WFPPC / PDAYS(INT(RSTAGE))
              WFGPAV(INT(RSTAGE)) = WFGPC / PDAYS(INT(RSTAGE))
              ENDIF
            IF (CDAYS.GT.0) THEN              
              TMAXCAV = TMAXCC / CDAYS
              TMINCAV = TMINCC / CDAYS 
              TMEANCAV = TMEANCC / CDAYS
              SRADCAV = SRADCC / CDAYS
              DAYLCAV = DAYLCC / CDAYS
              CO2CAV = CO2CC / CDAYS
              NFPCAV = NFPCC / CDAYS
              NFGCAV = NFGCC / CDAYS
              WFPCAV = WFPCC / CDAYS
              WFGCAV = WFGCC / CDAYS
            ENDIF
          ENDIF

!-----------------------------------------------------------------------
!         Calculate nitrogen concentrations
!-----------------------------------------------------------------------

          IF (ISWNIT.NE.'N') THEN
            ! Critical and minimum N concentrations
              ! Change from lstage,ststage 05/10/11
              LNCX = LNCXS(0) + DSTAGE*(LNCXS(1)-LNCXS(0))
              SNCX = SNCXS(0) + DSTAGE*(SNCXS(1)-SNCXS(0))
              RNCX = RNCXS(0) + DSTAGE*(RNCXS(1)-RNCXS(0))
              LNCM = LNCMN(0) + DSTAGE*(LNCMN(1)-LNCMN(0))
              SNCM = SNCMN(0) + DSTAGE*(SNCMN(1)-SNCMN(0))
              RNCM = RNCMN(0) + DSTAGE*(RNCMN(1)-RNCMN(0))
              ! Original
              ! LNCX = LNCXS(0) + LSTAGE*(LNCXS(1)-LNCXS(0))
              ! SNCX = SNCXS(0) + STSTAGE*(SNCXS(1)-SNCXS(0))
              ! RNCX = RNCXS(0) + DSTAGE*(RNCXS(1)-RNCXS(0))
              ! LNCM = LNCMN(0) + LSTAGE*(LNCMN(1)-LNCMN(0))
              ! SNCM = SNCMN(0) + STSTAGE*(SNCMN(1)-SNCMN(0))
              ! RNCM = RNCMN(0) + DSTAGE*(RNCMN(1)-RNCMN(0))

            ! N concentrations
            RANC = 0.0
            LANC = 0.0
            SANC = 0.0
            VANC = 0.0
            VCNC = 0.0
            VMNC = 0.0
            IF (RTWT.GT.1.0E-5) RANC = ROOTN / RTWT
            IF (LFWT.GT.1.0E-5) LANC = LEAFN / LFWT
            IF (STWT.GT.1.0E-5) SANC = STEMN / STWT
            ! Originally included retained dead matter and reserves
            ! TF as reserves were removed from VANC calc.,
            ! the if statement was adjusted too to avoid fortran
            ! errors (08/05/2024)
            IF (((LFWT+STWT)*PLTPOP*10.0).GT.0.0)
     &       VANC = VNAD/((LFWT+STWT)*PLTPOP*10.0)
            IF (LANC.LT.0.0) THEN 
              WRITE(Message(1),'(A27,F4.1)')
     &         'LANC below 0 with value of ',LANC
              WRITE(Message(2),'(A27,2F5.1)')
     &         'LEAFN,LFWT had values of   ',LEAFN,LFWT
              CALL WARNING(2,'CSCRP',MESSAGE)
!              WRITE(Fnumwrk,*)' '
!              WRITE(Fnumwrk,'(A28,F4.1)')
!     &         ' LANC below 0 with value of ',LANC
!              WRITE(Fnumwrk,'(A28,2F5.1)')
!     &         ' LEAFN,LFWT had values of   ',LEAFN,LFWT
              LANC = AMAX1(0.0,LANC)
            ENDIF
            IF (LFWT+STWT.GT.0.0) VCNC = 
     &      (LNCX*AMAX1(0.0,LFWT)+SNCX*AMAX1(0.0,STWT))/
     &      (AMAX1(0.0,LFWT)+AMAX1(0.0,STWT))
            IF (LFWT+STWT.GT.0.0) VMNC = 
     &      (LNCM*AMAX1(0.0,LFWT)+SNCM*AMAX1(0.0,STWT))/
     &      (AMAX1(0.0,LFWT)+AMAX1(0.0,STWT))

            SDNC = 0.0
            GRAINANC = 0.0
            IF (SEEDRS.GT.0.0) SDNC = SEEDN/(SEEDRS+SDCOAT)
            IF (GRWT.GT.0.0) GRAINANC = GRAINN/GRWT
            LNCR = 0.0
            SNCR = 0.0
            RNCR = 0.0
            IF (LNCX.GT.0.0) LNCR = AMAX1(0.0,AMIN1(1.0,LANC/LNCX))
            IF (SNCX.GT.0.0) SNCR = AMAX1(0.0,AMIN1(1.0,SANC/SNCX))
            IF (RNCX.GT.0.0) RNCR = AMAX1(0.0,AMIN1(1.0,RANC/RNCX))
          ELSE
            LNCR = 1.0
            SNCR = 1.0
            RNCR = 1.0
          ENDIF
          

!-----------------------------------------------------------------------
!         Continuation point if not germinated
!-----------------------------------------------------------------------

 6666     CONTINUE    

!-----------------------------------------------------------------------
!         Determine if to harvest or fail
!-----------------------------------------------------------------------

          ! Harvesting conditions
          IF (IHARI.EQ.'A' .AND. CUMDU.GE.PSTART(MSTG)) THEN
            ! Here need to check out if possible to harvest.
            IF (YEARDOY.GE.HFIRST) THEN
              IF (SW(1).GE.SWPLTL.AND.SW(1).LE.SWPLTH) 
     &         YEARDOYHARF=YEARDOY
            ENDIF
            ! Check if past earliest date; check if not past latest date
            ! Check soil water
            ! If conditions met set YEARDOYHARF = YEARDOY
            ! (Change YEARDOYHARF to more something more appropriate)
          ENDIF

          ! Determine if crop failure
          IF (DAP.GE.150 .AND. GESTAGE.LT.1.0) THEN
            CFLFAIL = 'Y'
            WRITE (Message(1),'(A41)')
     &       ' No germination within 150 days of sowing '
             CALL WARNING(1,'CSCRP',MESSAGE)
!            WRITE (Fnumwrk,*) ' '
!            WRITE (Fnumwrk,'(A41)')
!     &       ' No germination within 150 days of sowing '
          ENDIF
          IF (IHARI.NE.'A'.AND.MDAT.GE.0.AND.DAP-MDAP.GE.90) THEN
            CFLFAIL = 'Y'
            WRITE (Message(1),'(A32)')'90 days after end of grain fill '
            WRITE (Message(2),'(A21)')'Harvesting triggered.'
            CALL WARNING(2,'CSCRP',MESSAGE)
!            WRITE (Fnumwrk,*) ' '
!            WRITE (Fnumwrk,'(A32)')'90 days after end of grain fill '
!            WRITE (Fnumwrk,'(A21)')'Harvesting triggered.'
          ENDIF
          IF (IHARI.NE.'A'.AND.CUMDU.GE.PSTART(MSTG-1)) THEN
            IF (TT20.LE.-98.0.AND.TT20.LE.0.0) THEN
              CFLFAIL = 'Y'
              WRITE (Message(1),'(A28)') '20day thermal time mean = 0 '
              CALL WARNING(1,'CSCRP',MESSAGE)
!              WRITE (Fnumwrk,*) ' '
!              WRITE (Fnumwrk,'(A28)') '20day thermal time mean = 0 '
           ENDIF
          ENDIF

          ! Determine if to harvest
          CFLHAR = 'N'
          IF (IHARI.EQ.'R'.AND.YEARDOYHARF.EQ.YEARDOY .OR.
     &     IHARI.EQ.'D'.AND.YEARDOYHARF.EQ.DAP .OR.
     &     IHARI.EQ.'G'.AND.YEARDOYHARF.LE.RSTAGE .OR.
     &     IHARI.EQ.'A'.AND.YEARDOYHARF.EQ.YEARDOY .OR.
     &     IHARI.EQ.'M'.AND.CUMDU.GE.PSTART(MSTG)) THEN
            CFLHAR = 'Y'
          ENDIF    

          IF(IHARI.EQ.'R'.AND.CFLHAR.EQ.'N')THEN
            IF (CUMDU.GT.PSTART(MSTG) .AND. CFLHARMSG .NE. 'Y') THEN
              CFLHARMSG = 'Y'
              WRITE(Message(1),'(A54,I7)')
     &        'Maturity reached but waiting for reported harvest on: ',
     &        YEARDOYHARF 
              CALL WARNING(1,'CSCRP',MESSAGE)
!              WRITE(Fnumwrk,*)' '
!              WRITE(Fnumwrk,'(A55,I7)')
!     &        ' Maturity reached but waiting for reported harvest on: ',
!     &        YEARDOYHARF 
            ENDIF
          ENDIF
     
          IF (CFLFAIL.EQ.'Y' .OR. CFLHAR.EQ.'Y') THEN
          
            IF (CFLFAIL.EQ.'Y'
     &            .AND. RSTAGE <= 12 .AND. RSTAGE > 0 ) THEN       
              STGYEARDOY(12) = YEARDOY
              TMAXPAV(12) = TMAXPAV(INT(RSTAGE))
              TMINPAV(12) = TMINPAV(INT(RSTAGE))
              SRADPAV(12) = SRADPAV(INT(RSTAGE))
              DAYLPAV(12) = DAYLPAV(INT(RSTAGE))
              RAINPAV(12) = RAINPAV(INT(RSTAGE))
              CO2PAV(12) = CO2PAV(INT(RSTAGE))
              NFPPAV(12) = NFPPAV(INT(RSTAGE))
              WFPPAV(12) = WFPPAV(INT(RSTAGE))
              WFGPAV(12) = WFGPAV(INT(RSTAGE))
              NFGPAV(12) = NFGPAV(INT(RSTAGE))
            ENDIF
            STGYEARDOY(10) = YEARDOY  ! Harvest
            STGYEARDOY(11) = YEARDOY  ! Crop End
            IF (HSTG.GT.0) PSDAPFR(HSTG) = FLOAT(DAP)
            IF (ECSTG.GT.0) PSDAPFR(ECSTG) = FLOAT(DAP)
            IF (MSTG.GT.0.AND.PSDAPFR(MSTG).LE.0.0)PSDAPFR(MSTG) = -99.0
            HADOY = DOY
            HAYEAR = YEAR
            CWADSTG(INT(10)) = CWAD
            LAISTG(INT(10)) = LAI
            LNUMSTG(INT(10)) = LNUM
            CNADSTG(INT(10)) = CNAD
            IF (MDAYFR.LT.0.0) THEN     ! Maturity not reached
              IF (CFLFAIL.EQ.'Y') THEN
                WRITE(Message(1),'(A26)')
     &           'Harvest/failure triggered '                 
                CALL WARNING(1,'CSCRP',MESSAGE)
!                WRITE(Fnumwrk,*)' '
!                WRITE(Fnumwrk,*)'Harvest/failure triggered on ',yeardoy
              ENDIF  
            ENDIF
          ENDIF

!-----------------------------------------------------------------------
!         Calculate season end soil conditions                              
!-----------------------------------------------------------------------

          SOILNF = 0.0
          FSOILH2O = 0.0
          DO I = 1, NLAYR
            SOILNF = SOILNF + NO3LEFT(I)/(10.0/(BD(I)*(DLAYR(I))))
     &                          + NH4LEFT(I)/(10.0/(BD(I)*(DLAYR(I))))
            FSOILH2O = FSOILH2O + SW(I)*DLAYR(I)
          ENDDO

!-----------------------------------------------------------------------
!         Calculate variables that are generally measured and presented
!-----------------------------------------------------------------------

          ! Here,reserves are included in leaf,stem,and chaff weights
          ! And weights are in kg/ha
          CWAD = (LFWT+STWT+CHWT+GRWT+RSWT+SENTOPRETAINED)*PLTPOP*10.0
          IF
     &    (LRETS.GT.0.AND.LRETS.LT.99999.AND.SENTOPRETAINED.GT.0.0) THEN
            SENTOPRETAINEDA = SENTOPRETAINED*PLTPOP*10.0
          ELSEIF (LRETS.LE.0.OR.LRETS.GE.99999) THEN
            SENTOPRETAINEDA = 0.0
          ENDIF
          GWAD = GRWT*PLTPOP*10.0
          LLWAD = LFWT*(1.0-LSHFR)*10.0*PLTPOP
          LSHWAD = LFWT*LSHFR*10.0*PLTPOP
          RWAD = RTWT*PLTPOP*10.0
          SDWAD = (SEEDRS+SDCOAT)*10.0*PLTPOP
          ! Leaf sheaths NOT included in stem here
          STWAD = STWT*10.0*PLTPOP
          CHWAD = CHWT*PLTPOP*10.0
          RSWAD = RSWT*PLTPOP*10.0
          RSWADPM = RSWTPM*PLTPOP*10.0
          LLRSWAD = LLRSWT*PLTPOP*10.0
          LSHRSWAD = LSHRSWT*PLTPOP*10.0
          STRSWAD = STRSWT*PLTPOP*10.0
          CHRSWAD = CHRSWT*PLTPOP*10.0

          ! Need to CHECK these
          SENROOTA = SENROOT*10.0*PLTPOP
          SENCAS = SENCS*10.0*PLTPOP
          SENLAS = SENLS*10.0*PLTPOP
          SENTOPLITTERA = SENTOPLITTER*PLTPOP*10.0
          DO L =1,NLAYR
            RTWTAL(L) = RTWTL(L)*PLTPOP*10.0
            SENWAL(L) = SENWL(L)*PLTPOP*10.0
          ENDDO

          TWAD = 
     &      (SEEDRS+SDCOAT+RTWT+LFWT+STWT+GRWT+RSWT+SENTOPRETAINED)
     &         * PLTPOP*10.0

          VWAD = (LFWT+STWT+RSWT+SENTOPRETAINED)*PLTPOP * 10.0
          EWAD = (GRWT+CHWT)*PLTPOP * 10.0

          GNOAD = GNOPD*PLTPOP
          TNUMAD = TNUM*PLTPOP

          IF (NUPAC.LT.0.0) THEN
            NUPAC = NUPAD
          ELSE 
            NUPAC = NUPAC+NUPAD
          ENDIF  
          CNAD = (LEAFN+STEMN+GRAINN+RSN+DEADN)*PLTPOP*10.0
          DEADNAD = DEADN*PLTPOP*10.0
          GNAD = GRAINN*PLTPOP*10.0
          LLNAD = LEAFN*(1.0-LSHFR)*PLTPOP*10.0
          RNAD = ROOTN*PLTPOP*10.0
          RSNAD = RSN*PLTPOP*10.0
          SDNAD = SEEDN*PLTPOP*10.0
          ! LAH Stem N does not include leaf sheaths
          SNAD = STEMN*PLTPOP*10.0
          ! SNAD = (STEMN+LEAFN*LSHFR)*PLTPOP*10.0
          TNAD = (LEAFN+STEMN+GRAINN+RSN+DEADN+SEEDN+ROOTN)*PLTPOP*10.0
          VNAD = (LEAFN+STEMN+RSN+DEADN)*PLTPOP*10.0
          
          ! LAH Note that no reserves included in sancout
          ! SANCOUT = SNAD/(STWAD+STRSWAD + LSHWAD+LSHRSWAD)
          ! LAH  IF ((STWAD + LSHWAD).GT.1.0E-5)
          ! LAH     SANCOUT = SNAD/(STWAD + LSHWAD)
          IF (STWAD.GT.1.0E-5)
     &     SANCOUT = SNAD/STWAD

          HWAD = GWAD
          HWUD = GWUD
          HNUMAD = GNOAD
          HNAD = GNAD
          HNC = GRAINANC

          SENNAS = SENNS*10.0*PLTPOP
          SENNAL(0) = SENNL(0)*PLTPOP*10.0
          SENNATC = SENNAL(0)+SENNAS
          DO L =1,NLAYR
            SENNAL(L) = SENNL(L)*PLTPOP*10.0
          ENDDO

          ! After harvest residues
          IF (STGYEARDOY(11).EQ.YEARDOY) THEN
            ! Surface
            RESWALG(0) = VWAD*(1.0-HBPCF/100.0) + GWAD*(1.0-HPCF/100.0)
            RESNALG(0) = (LEAFN+STEMN+DEADN)*PLTPOP*10.*(1.0-HBPCF/100.)
     &                 + GNAD*(1.0-HPCF/100.0)
            RESCALG(0) = RESWALG(0) * 0.4
            RESLGALG(0) = LLWAD*LLIGPC/100.0*(1.0-HBPCF/100.0)
     &                  + LSHWAD*SLIGPC/100.0*(1.0-HBPCF/100.0)
     &                  + STWAD*SLIGPC/100.0*(1.0-HBPCF/100.0)
     &                  + GWAD*GLIGPC/100.0*(1.0-HPCF/100.0)
            ! Soil
            DO L = 1, NLAYR
              RESWALG(L) = RTWTL(L)*PLTPOP*10.0
              RESNALG(L) = RTWTL(L)*PLTPOP*10.0 * RANC
              RESCALG(L) = RTWTL(L)*PLTPOP*10.0 * 0.4
              RESLGALG(L) = RTWTL(L)*PLTPOP*10.0 * RLIGPC/100.0
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

!-----------------------------------------------------------------------
!         Calculate weather and soil summary variables
!-----------------------------------------------------------------------

          ! Cumulatives
          TTCUM = TTCUM + TT
          RAINC = RAINC + RAIN
          DRAINC = DRAINC + DRAIN
          RUNOFFC = RUNOFFC + RUNOFF
          IRRAMTC = IRRAMTC + IRRAMT
          IF (ADAT.LT.0) RAINCA = RAINCA + RAIN
          SRADC = SRADC + SRAD
          PARMJC = PARMJC + PARMJFAC*SRAD
          PARMJIC = PARMJIC + PARMJFAC*SRAD*PARI + PARMJIADJ
          TOMINC = TOMINC + TOMIN
          TOFIXC = TOFIXC + TNIMBSOM
          TOMINFOMC = TOMINFOMC + TOMINFOM
          TOMINSOMC = TOMINSOMC + TOMINSOM
          IF (TOMINSOM1.GE.0.0) THEN
            TOMINSOM1C = TOMINSOM1C + TOMINSOM1
            TOMINSOM2C = TOMINSOM2C + TOMINSOM2
            TOMINSOM3C = TOMINSOM3C + TOMINSOM3
          ELSE
            TOMINSOM1C = -99.0
            TOMINSOM2C = -99.0
            TOMINSOM3C = -99.0
          ENDIF  
          TLCHC = TLCHC + TLCHD
          TNOXC = TNOXC + TNOXD

          ! Extremes
          TMAXX = AMAX1(TMAXX,TMAX)
          TMINN = AMIN1(TMINN,TMIN)
          CO2MAX = AMAX1(CO2MAX,CO2)

          ! Growing season means
          TMEANNUM = TMEANNUM + 1
          TMEANSUM = TMEANSUM + TMEAN

          ! 20-day means
          SRAD20S = 0.0
          TMEAN20S = 0.0
          STRESS20S = 0.0
          STRESS20NS = 0.0
          STRESS20WS = 0.0
          TT20S = 0.0
          DO L = 20,2,-1
            SRADD(L) = SRADD(L-1)
            SRAD20S = SRAD20S + SRADD(L)
            TMEAND(L) = TMEAND(L-1)
            TMEAN20S = TMEAN20S + TMEAND(L)
            STRESS(L) = STRESS(L-1)
            STRESSN(L) = STRESSN(L-1)
            STRESSW(L) = STRESSW(L-1)
            STRESS20S = STRESS20S + STRESS(L)
            STRESS20NS = STRESS20NS + STRESSN(L)
            STRESS20WS = STRESS20WS + STRESSW(L)
            TTD(L) = TTD(L-1)
            TT20S = TT20S + TTD(L)
            WUPRD(L) = WUPRD(L-1)
          ENDDO
          SRADD(1) = SRAD
          SRAD20S = SRAD20S + SRAD
          TMEAND(1) = TMEAN
          TMEAN20S = TMEAN20S + TMEAND(1)
          STRESS(1) = AMIN1(WFG,NFG)
          STRESSN(1) = NFG
          STRESSW(1) = WFG
          STRESS20S = STRESS20S + STRESS(1)
          STRESS20NS = STRESS20NS + STRESSN(1)
          STRESS20WS = STRESS20WS + STRESSW(1)
          TTD(1) = TT
          TT20S = TT20S + TTD(1)
          WUPRD(1) = AMAX1(0.0,AMIN1(10.0,WUPR))
          IF (TMEANNUM.GE.20) THEN
            IF (TMEANNUM.LE.20) TMEAN20P = TMEAN20S/20.0
            SRAD20 = SRAD20S/20.0
            TMEAN20 = TMEAN20S/20.0
            TT20 = TT20S/20.0
            STRESS20 = STRESS20S/20.0
            STRESS20N = STRESS20NS/20.0
            STRESS20W = STRESS20WS/20.0
          ELSE
            SRAD20 = 0.0
            TT20 = 0.0
            TMEAN20 = 0.0
            STRESS20 = 0.0
            STRESS20N = 0.0
            STRESS20N = 0.0
          ENDIF

          ! Monthly means
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

!-----------------------------------------------------------------------
!         Calculate PAR utilization efficiencies
!-----------------------------------------------------------------------

          IF (PARMJC.GT.0.0) PARUEC = AMAX1(0.0,
     &     (RTWT+LFWT+STWT+GRWT+RSWT+SENTOPRETAINED+SENTOPLITTER
     &     +SENROOT-SEEDUSE)
     &     * PLTPOP / PARMJC)
          IF (PARMJIC.GT.0.0) PARIUED = AMAX1(0.0,
     &     (RTWT+LFWT+STWT+GRWT+RSWT+SENTOPRETAINED+SENTOPLITTER
     &     +SENROOT-SEEDUSE)
     &     * PLTPOP / PARMJIC)

          IF (CARBOBEG.GT.0.0) THEN
            PARIUE = (CARBOBEG*PLTPOP)/(PARMJFAC*SRAD*PARI)
          ELSE  
            PARIUE = 0.0
          ENDIF

!-----------------------------------------------------------------------
!         Determine if nitrogen fertilizer applied
!-----------------------------------------------------------------------

          ! LAH Handled differently in stand-alone Cropsim. 
          ! Need to change Cropsim per se
          IF (FERNIT.GT.FERNITPREV) THEN
            FAPPNUM = FAPPNUM + 1
            AMTNIT = FERNIT
            WRITE(fappline(fappnum),'(A1,I4,A10,I7,A13,I4,A6)')
     &        ' ',NINT(FERNIT-FERNITPREV),' kg/ha on ',
     &        YEARDOY,'     To date ',NINT(amtnit),' kg/ha'
            FERNITPREV = FERNIT
          ENDIF

!-----------------------------------------------------------------------
!         Calculate water availability ratio
!-----------------------------------------------------------------------

          BASELAYER = 0.0
          H2OA = 0.0
          IF (ISWWAT.NE.'N') THEN
            DO L = 1, NLAYR
              DLAYRTMP(L) = DLAYR(L)
              BASELAYER = BASELAYER + DLAYR(L)
              IF (RTDEP.GT.0.0.AND.RTDEP.LT.BASELAYER) THEN
                DLAYRTMP(L) = RTDEP-(BASELAYER-DLAYR(L))
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

!-----------------------------------------------------------------------
!         Upgrade albedo
!-----------------------------------------------------------------------

          ! When running in CSM
          IF (FILEIOT.EQ.'DS4') THEN
            IF (LAI .LE. 0.0) THEN
              ALBEDO = ALBEDOS
            ELSE
              ALBEDO = 0.23-(0.23-ALBEDOS)*EXP(-0.75*LAI)
            ENDIF
          ELSE
            ALBEDO = ALBEDOS  
          ENDIF
          
!-----------------------------------------------------------------------
!         Compute weights,etc. at end crop
!-----------------------------------------------------------------------

          IF (STGYEARDOY(11).EQ.YEARDOY) THEN

            ! LAH No adjustment for fraction of day to maturity
            GRWTM = GRWT
            RSWTM = RSWT
            RTWTM = RTWT
            LFWTM = LFWT
            DEADWTM = SENTOPRETAINED
            STWTM = STWT
            RSWTM = RSWT

            LNUMSM = LNUM
            TNUMAM = TNUMAD
            GNOAM = GNOAD

            IF (GNOPD.GT.0.0) THEN
              GWUM = GRWTM/GNOPD
            ELSE
              GWUM = 0.0
            ENDIF
            IF (TNUMAM.GT.0.0) THEN
              GNOGM = GNOAM/TNUMAM
            ELSE
              GNOGM = 0.0
            ENDIF
            IF (PLTPOP.GT.0.0) THEN
              GNOPM = GNOAM/PLTPOP
              TNUMPM = TNUMAM/PLTPOP
            ELSE
              GNOPM = 0.0
              TNUMPM = 0.0
            ENDIF

            IF (LFWTM+STWTM.GT.0.0)
     &       RSCM = RSWTM/(LFWTM+STWTM)
            IF (RTWTM.GT.0.0)
     &       SHRTM = (LFWTM+STWTM+RSWTM+GRWTM+DEADWTM)/RTWTM

            CWAM = (LFWTM+STWTM+GRWTM+RSWTM+DEADWTM)*PLTPOP*10.0
            VWAM = (LFWTM+STWTM+RSWTM+DEADWTM)*PLTPOP * 10.0
            DEADWAM = DEADWTM*PLTPOP*10.0
            GWAM = GRWTM*PLTPOP*10.0
            
            ! For Grazing
            cwahc = (lwphc+swphc+rswphc+gwphc+dwrphc)*pltpop*10.0
            ! Adjustments for spikes that removed by grazing,etc..
            IF (TNUM.GT.0.0) THEN
              GWAM = GWAM * (TNUM-SPNUMHC)/TNUM
              GNOAM = GNOAM * (TNUM-SPNUMHC)/TNUM
            ENDIF  

            RWAM = RTWTM*PLTPOP*10.0
            SDWAM = (SEEDRS+SDCOAT)*PLTPOP*10.0

            IF (CWAM.GT.0.0) THEN
              HIAM = HIAD
              GNOWTM = GNOAM/(CWAM*0.1)
            ENDIF

            SENWACM = SENTOPLITTERA+SENROOTA

            RSWAM = RSWAD

            CNAM = CNAD
            GNAM = GNAD
            GNPCM = GRAINANC*100.0
            VNAM = VNAD
            VNPCM = VANC*100.0
            RNAM = RNAD

            HINM = HIND

            ! Set harvest product outputs
            HWAM = GWAM/(1.0-GMPCH/100.0)! To give yield at GMPCH 
            HNAM = GNAM
            HWUM = GWUM
            HNUMAM = GNOAM
            HNUMGM = GNOGM
            HNUMPM = GNOPM
            HNPCM = GNPCM

          ENDIF

!-----------------------------------------------------------------------
!       Set planting date if flag indicates relative to emergence date
!-----------------------------------------------------------------------
          
          IF (EDATMX.GT.0.AND.CFLPDATE.EQ.'E') THEN
            IF (GEUCUM.LT.PEGD) GEUCUM = 0.0 
            IF (EDATMX.EQ.YEARDOY) THEN
!              WRITE(fnumwrk,*)' '
!              WRITE(fnumwrk,*)
!     &       'Planting date set relative to measured emergence date on '
!     &       ,yeardoy
              GEUCUM = PEGD
              GERMFR = 1.0
              EMRGFR = 1.0
              Rtwtl(1) = seedrsi * 0.030
              Rtwtl(2) = seedrsi * 0.025
              Rtwtl(3) = seedrsi * 0.000
              Rootn = seedni* 0.04
              Rtdep = 7.5
              Cumdu = 20.0
              DAP = 10  ! Arbitrary
              GDAP = 4
              STGYEARDOY(1) = YEARDOY - 6
              DAE = 0
              EYEARDOY = YEARDOY
              EDAP = DAP
              EDAYFR = 0.0
              EDAPFR = FLOAT(DAP) + EDAYFR
              LNUMSG = 1
            ENDIF
          ENDIF

!=======================================================================
        ENDIF  ! End of after planted (integrate) section
!=======================================================================

      END SUBROUTINE CRP_Integrate