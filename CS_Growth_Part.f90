!***************************************************************************************************************************
! This is the code from the section (DYNAMIC.EQ.RATE) lines 4694 - 5033 of the original CSCAS code. The names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement and are declared here. The variables 
! that are not arguments are declared in Module_CSCAS_Vars_List. Unless identified as by MF, all comments are those of 
! the original CSCAS.FOR code.
!
! Subroutine CS_Growth_Part calculates assimilation partitioning, growth of storage roots, leaves, stems and crowns.
!***************************************************************************************************************************
    
    SUBROUTINE CS_Growth_Part ( &
        BRSTAGE     , ISWNIT      , NFP         &
        )
    
        USE ModuleDefs
        USE Module_CSCAS_Vars_List
    
        IMPLICIT NONE
        
        !TYPE (WeatherType) WEATHER    ! Defined in ModuleDefs
    
        CHARACTER(LEN=1) ISWNIT      
        REAL    BRSTAGE     , NFP         

        REAL    CSYVAL                                                                       ! Real function call

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
        IF (PDL(INT(BRSTAGE)).GT.0.0) THEN                                                          ! MSTG = KEYPSNUM
            TVR1 = FLOAT(INT(BRSTAGE)) + (LNUM-LNUMTOSTG(INT(BRSTAGE)))/PDL(INT(BRSTAGE))                              ! EQN 004
        ELSE
            TVR1 = FLOAT(INT(BRSTAGE))
        ENDIF
        IF (INT(TVR1).GT.INT(BRSTAGEPREV)) THEN
            IF (BRSTAGE.EQ.0.0) THEN
                BRNUMST = 1                                                                         ! BRNUMST          ! Branch number/shoot (>forking) # (Actually the total number of apices)
            ELSEIF (BRSTAGE.GT.0.0) THEN
                BRNUMST = BRNUMST*BRFX(INT(BRSTAGE))                                                ! BRFX(PSX)        ! EQN 005 ! # of branches at each fork # (This is where new branch is initiated)
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
        DO L = MAX(1,LNUMSG-(INT((LLIFG/PHINTS)+1))),LNUMSG+1                                       ! MF Why + 1? See LPM p. 63.
            ! Basic leaf growth calculated on thermal time base. 
            ! Basic response (cm2/d) same as for development. 
            TTNEED = AMAX1(0.0,LLIFG-LAGETT(L))
            LATLPREV(L) = LATL(L)
            LATLPOT(L)=LAPOTX(L)*((LAGETT(L)+TTLFLIFE*EMRGFR)/LLIFG)
            IF (LATLPOT(L).LT.0.0) LATLPOT(L) = 0.0
            IF (LATLPOT(L).GT.LAPOTX(L)) LATLPOT(L) = LAPOTX(L)
            LATL(l) = LATL(L) + (LATLPOT(L)-LATLPREV(L))
            LATL2(l) = LATL2(L) + (LATLPOT(L)-LATLPREV(L))* AMIN1(WFG,NFG)*TFG
            SHLAG2(1) = SHLAG2(1) + (LATLPOT(L)-LATLPREV(L))* AMIN1(WFG,NFG)*TFG
            ! The 2 at the end of the names indicates that 2 groups 
            ! of stresses have been taken into account
            ! Stress factors for individual leaves
            WFLF(L) = AMIN1(1.0,WFLF(L)+WFG*(LATLPOT(L)-LATLPREV(L))/LAPOTX(L))
            NFLF(L) = AMIN1(1.0,NFLF(L)+NFG*(LATLPOT(L)-LATLPREV(L))/LAPOTX(L))
            NFLFP(L) = AMIN1(1.0,NFLFP(L)+NFP*(LATLPOT(L)-LATLPREV(L))/LAPOTX(L))
            TFGLF(L) = AMIN1(1.0,TFGLF(L)+TFG*(LATLPOT(L)-LATLPREV(L))/LAPOTX(L))
            TFDLF(L) = AMIN1(1.0,TFDLF(L)+TFD*(LATLPOT(L)-LATLPREV(L))/LAPOTX(L))
            ! New LEAF
            IF (L.EQ.LNUMSG.AND.LNUMG.GT.LNUMNEED) THEN                                             ! This is where new leaf is initiated
                LAGL(L+1) = LAPOTX(L+1) * (TTLFLIFE*EMRGFR) * (((LNUMG-LNUMNEED)/LNUMG)/LLIFG)      ! LAGL(LNUMX)         ! Leaf area growth,shoot,lf pos  cm2/l
                LATL(L+1) = LATL(L+1) + LAGL(L+1)                                                   ! LATL(LNUMX)         ! Leaf area,shoot,lf#,potential  cm2/l   
                LATL2(L+1) = LATL2(L+1) + LAGL(L+1) * AMIN1(WFG,NFG)*TFG                            ! LATL2(LNUMX)        ! Leaf area,shoot,lf#,+h2o,n,tem cm2/l
                SHLAG2(1) = SHLAG2(1) + LAGL(L+1) * AMIN1(WFG,NFG)*TFG                              ! SHLAG2(25)          ! Shoot lf area gr,1 axis,H2oNt  cm2
                LBIRTHDAP(L+1) = DAP                                                                ! LBIRTHDAP(LCNUMX)   ! DAP on which leaf initiated #  
                ! Stress factors for individual leaves                       
                WFLF(L+1) = AMIN1(1.0,WFLF(L+1)+WFG*LATL(L+1)/LAPOTX(L+1))
                NFLF(L+1) = AMIN1(1.0,NFLF(L+1)+NFG*LATL(L+1)/LAPOTX(L+1))
                NFLFP(L+1) = AMIN1(1.0,NFLFP(L+1)+NFP*LATL(L+1)/LAPOTX(L+1))
                TFGLF(L+1) = AMIN1(1.0,TFGLF(L+1)+TFG*LATL(L+1)/LAPOTX(L+1))
                TFDLF(L+1) = AMIN1(1.0,TFDLF(L+1)+TFD*LATL(L+1)/LAPOTX(L+1))
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
                    LATL3(L)= LATL2(L) * AFLF(0)
                    AFLF(L) = AMIN1(1.0,AFLF(L) + AMAX1(0.0,AFLF(0)) * (LATLPOT(L)-LATLPREV(L))/LAPOTX(L))
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
        
    END SUBROUTINE CS_Growth_Part