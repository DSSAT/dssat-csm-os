!***************************************************************************************************************************
! This is the code from the section (DYNAMIC == RATE) lines 4538 - 4642 of the original CSCAS code. The names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement and are declared here. The variables 
! that are not arguments are declared in module YCA_First_Trans_m. Unless identified as by MF, all comments are those of 
! the original CSCAS.FOR code.
!
! Subroutine YCA_Growth_Senesce calculates senescence and remobilization.
!***************************************************************************************************************************
    SUBROUTINE YCA_Growth_Senesce ( &
        ISWNIT      , LAI       & 
        )

! 2023-01-25 chp removed unused variables
!       ISWWAT,     BRSTAGE      , 

        USE YCA_First_Trans_m
        USE YCA_Control_Leaf
    
        IMPLICIT NONE
        
        INTEGER :: BR                      ! Index for branch number/cohorts#          ! (From SeasInit)  
        INTEGER :: LF                      ! Loop counter leaves            #          !LPM 21MAR15 to add a leaf counter
        INTEGER :: Lcount                   ! counter for iterations in leafs (Lcount)
        INTEGER :: Bcount                  ! counters for iterations in branches (Bcount)
        CHARACTER(LEN=1) ISWNIT      !, ISWWAT
!       REAL BRSTAGE
        REAL LAI
    
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
        node%LAPSTMP = 0.0
        DO BR = 0, BRSTAGEINT                                                                                        !LPM 21MAR15
            DO LF = 1, LNUMSIMSTG(BR)         
                IF (.NOT. willLeafStillGrowingToday(node(BR,LF))) THEN                                                     !EQN 371 LPM28MAR15 Deleted LLIFGTT
                    IF (leafAreaLeftToSenesce(node(BR,LF)) > 0.0) THEN
                        node(BR,LF)%LAPSTMP = AMIN1((leafAreaLeftToSenesce(node(BR,LF))),(node(BR,LF)%LATL3T*(AMIN1((node(BR,LF)%LAGETT+(dailyGrowth())-(LLIFGTT+LLIFATT)), (dailyGrowth()))/LLIFSTT)))         !EQN 372
                        node(BR,LF)%LAPS = node(BR,LF)%LAPS + node(BR,LF)%LAPSTMP
                        PLASP = PLASP + node(BR,LF)%LAPSTMP                                                                                !EQN 370
                    ENDIF
                ENDIF
            ENDDO
        ENDDO

        ! Leaf senescence - injury        ! LAH  To add later?
        !PLASI = PLA*(LSENI/100.0)*DU/STDAY  ! May need injury loss

        ! Leaf senescence - water or N stress
        ! LAH Need to accelerated senescence rather than lose leaf
        PLASW = 0.0
        PLASN = 0.0
        !LPM 04FEB2021 Remove equations of accelerated senescence due to water stress (currently this effected is not implemented)
        !IF (ISWWAT /= 'N') THEN
        !    IF (plantLeafAreaLeftToSenesce() > 0.0.AND.WUPR < WFSU) PLASW = AMAX1(0.0,AMIN1((plantLeafAreaLeftToSenesce())-PLAS,(plantLeafAreaLeftToSenesce())*LLOSA))        !EQN 373
        !ENDIF
        !LPM 30NOV2020 Remove equations of accelerated senescence due to N (currently this effected is not implemented) 
        !Better to affect leaf appearance rate and leaf size
        !IF (ISWNIT /= 'N') THEN
        !    LNCSEN = node(0,0)%LNCM + NFSU * (node(0,0)%LNCX-node(0,0)%LNCM)                                                                         !EQN 374
        !    IF (plantLeafAreaLeftToSenesce() > 0.0.AND.LANC < LNCSEN) PLASN = AMAX1(0.0,AMIN1((plantLeafAreaLeftToSenesce())-PLAS,(plantLeafAreaLeftToSenesce())*LLOSA))
        !ENDIF
        ! LAH TMP
        PLASW = 0.0
        PLASN = 0.0
        PLASS = PLASW + PLASN    ! Loss because of stress
              
        ! LPM As a NOTE for further code: if PLASW and PLASN want to be added to the code, should be calculated by cohort
        
        !-----------------------------------------------------------------------
        !        LAI by Cohort
        !-----------------------------------------------------------------------
        node%LAIByCohort=0.0                               ! DA re-initializing LAIByCohort
        LAI=0.0                                              ! DA re-initializing LAI
        DO Bcount=0,BRSTAGEINT
            BR= BRSTAGEINT - Bcount                                                        ! DA 28OCT2016 to run the loop to the higher branch to the lowest
            DO Lcount=0,LNUMSIMSTG(BR)-1
                LF=LNUMSIMSTG(BR)-Lcount                                                ! DA to run the loop to the higher leaf to the lowest
                IF (isLeafAlive(node(BR,LF))) THEN                      ! DA if leave is alive

                    node(BR,LF)%LAIByCohort = LAI + (leafAreaLeftToSenesce(node(BR,LF)))*PLTPOP*0.0001             ! DA the LAI calculation is accumulative from the previous cohort LAI
                    LAI = node(BR,LF)%LAIByCohort                                                                     ! DA updating LAI
                    
                ENDIF
            ENDDO
        ENDDO
        
        ! Leaf senescence - low light at base of canopy
        ! NB. Just senesces any leaf below critical light fr 
        PLASL = 0.0
        !IF (LAI > LAIXX) THEN
        !    PLASL = (LAI-LAIXX) / (PLTPOP*0.0001)
        !    ! LAH Eliminated! Replaced by accelerated senescence
        !    PLASL = 0.0
        !ENDIF
            
        ! Leaf senescence - overall
        PLAS =  PLASP + PLASI + PLASS + PLASL                                                                          !EQN 369
        ! Overall check to restrict senescence to what available
        PLAS = AMAX1(0.0,AMIN1(PLAS,plantLeafAreaLeftToSenesce()))

        !-----------------------------------------------------------------------
        !           Calculate C and N made available through senescence
        !-----------------------------------------------------------------------

        SENLFG = 0.0
        SENLFGRS = 0.0
        SENNLFG = 0.0
        SENNLFGRS = 0.0
        node%SENNLFGRSN = 0.0
        IF (plantLeafAreaLeftToSenesce() > 0.0) THEN
        ! LAH New algorithms 03/04/13
        SENLFG = AMIN1(LFWT*LWLOS,(AMAX1(0.0,(LFWT*(PLAS/(plantLeafAreaLeftToSenesce()))*LWLOS))))                                        !EQN 375
        SENLFGRS = AMIN1(LFWT*(1.0-LWLOS),(AMAX1(0.0,(LFWT*(PLAS/(plantLeafAreaLeftToSenesce()))*(1.0-LWLOS)))))                          !EQN 376
        ENDIF
  
        IF (ISWNIT /= 'N') THEN
            ! NB. N loss has a big effect if low N
            ! Assumes that all reserve N in leaves
            IF (LFWT > 0.0) LANCRS = (LEAFN+RSN) / LFWT                                                               !EQN 377
            !SENNLFG = AMIN1(LEAFN,(SENLFG+SENLFGRS)*LNSC)                                                              !EQN 378
            DO BR = 0, BRSTAGEINT                                                                                        !LPM 21MAR15
                DO LF = 1, LNUMSIMSTG(BR)   
                    IF (plantLeafAreaLeftToSenesce() > 0.0 .AND. leafAreaLeftToSenesce(node(BR,LF)) > 0.0 .AND. .NOT. willLeafStillGrowingToday(node(BR,LF))) THEN
                        node(BR,LF)%SENNLFGN = AMAX1(0.0,(LFWT*(node(BR,LF)%LAPSTMP/plantLeafAreaLeftToSenesce()))*LNSC)
                        node(BR,LF)%SENNLFGRSN = AMAX1(0.0,(LFWT*(node(BR,LF)%LAPSTMP/plantLeafAreaLeftToSenesce()))*(node(BR,LF)%LANC-LNSC))                                 !EQN 379
                        SENNLFG = SENNLFG + node(BR,LF)%SENNLFGN
                        SENNLFGRS = SENNLFGRS + node(BR,LF)%SENNLFGRSN
                    ENDIF
                ENDDO
            ENDDO       
            SENNLFG = AMIN1(LEAFN,SENNLFG)
            SENNLFGRS = AMIN1(LEAFN-SENNLFG,SENNLFGRS)                                            
        ELSE
            SENNLFG = 0.0
            SENNLFGRS = 0.0
        ENDIF

        !-----------------------------------------------------------------------
        !           Calculate overall senescence loss from tops
        !-----------------------------------------------------------------------

        SENFR = 1.0
        SENTOPLITTERG = 0.0
        SENTOPLITTERG = SENLFG*SENFR                                                                                   !EQN 380
        
    END SUBROUTINE YCA_Growth_Senesce