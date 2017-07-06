!***************************************************************************************************************************
! This is the code from the section (DYNAMIC.EQ.INTEGR) lines 8387 - 8515 of the original CSCAS code. The names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement and are declared here. The variables 
! that are not arguments are declared in module YCA_First_Trans_m. Unless identified as by MF, all comments are those of 
! the original CSCAS.FOR code.
!
! Subroutine YCA_Out_LfTeir outputs details of leaves and tiers. 
!***************************************************************************************************************************
    
    SUBROUTINE YCA_Out_LfTier ( & 
        IDETL       , RUN         , STGYEARDOY  , BRSTAGE    &
        )
        
        !USE ModuleDefs                                         ! MF 31AU14 ADDED FOR ACCESS TO WEATHER
        USE YCA_First_Trans_m
        USE YCA_Formats_m
     
        IMPLICIT NONE 
     
        INTEGER :: RUN         , STGYEARDOY(0:19) 
        REAL BRSTAGE

        CHARACTER(LEN=1)  :: IDETL       
    
        !-----------------------------------------------------------------------------------------------------------
        !         Output leaves and tiers data (IDETL = Y or D)
        !-----------------------------------------------------------------------------------------------------------
        IF (IDETL.EQ.'Y'.OR.IDETL.EQ.'D'.OR.IDETL.EQ.'A') THEN
                    
            ! LEAVES.OUT
            OPEN(UNIT=FNUMLVS,FILE=FNAMELEAVES,POSITION='APPEND')
            WRITE (FNUMLVS,'(/,A79,/)') OUTHED
            WRITE (FNUMLVS,'(A14,F6.1)') '! LEAF NUMBER ',LNUM
            WRITE (FNUMLVS,'(/,A42,A36,A30)')'@ BRNUM LNUM AREAP AREA1 AREA2 AREAT AREAS', &                 ! DA 26JAN2017 issue #5 removed LATL2C and LATL4C
                '  WFLF  NFLF NFLF2  AFLF TFGLF TFDLF',' LLIFG LLIFA LLIFS LLIFE   DAP'
            DO BR = 0, BRSTAGE
                DO LF = 1, INT(LNUMSIMSTG(BR))  
                    CALL Csopline(lapotxc,plant(BR,LF)%lapotx)
                    CALL Csopline(latlc,AMAX1(0.0,plant(BR,LF)%LATL))
                    CALL Csopline(latl2c,AMAX1(0.0,plant(BR,LF)%LATL2))
                    CALL Csopline(latl3c,AMAX1(0.0,plant(BR,LF)%LATL3))
                    CALL Csopline(latl4c,AMAX1(0.0,plant(BR,LF)%LATL4))
                    CALL Csopline(lapc,plant(BR,LF)%lap)
                    CALL Csopline(lapsc,plant(BR,LF)%laps)
                    ! Adjust for growth period of non fully expanded leaves
                    IF (plant(BR,LF)%LAGETT<=LLIFGTT) THEN !LPM 24APR2016 Estimate when the leaves are growing   !MF 21AU16 ADDED DIMENSIONS TO LAGETT
                        plant(BR,LF)%WFLF = AMIN1(1.0,plant(BR,LF)%WFLF/AMIN1(1.0,(plant(BR,LF)%LAGETT/LLIFGTT)))
                        plant(BR,LF)%NFLF = AMIN1(1.0,plant(BR,LF)%NFLF/AMIN1(1.0,(plant(BR,LF)%LAGETT/LLIFGTT)))
                        plant(BR,LF)%NFLFP =AMIN1(1.0,plant(BR,LF)%NFLFP/AMIN1(1.0,(plant(BR,LF)%LAGETT/LLIFGTT)))
                        plant(BR,LF)%TFGLF =AMIN1(1.0,plant(BR,LF)%TFGLF/AMIN1(1.0,(plant(BR,LF)%LAGETT/LLIFGTT)))
                        plant(BR,LF)%AFLF = AMIN1(1.0,plant(BR,LF)%AFLF/AMIN1(1.0,(plant(BR,LF)%LAGETT/LLIFGTT)))
                    ENDIF
                    IF (plant(BR,LF)%LDEATHDAP == 0) THEN
                        plant%LDEATHDAP = -99
                    ENDIF
                    WRITE (fnumlvs,'(2I6,5A6,6F6.2,4F6.1,I6)')BR, LF,LAPOTXC,LATLC,LATL3C,LAPC,LAPSC,1.0-plant(BR,LF)%WFLF, &                          ! DA 26JAN2017 issue #5 removed LATL2C and LATL4C
                        1.0-plant(BR,LF)%NFLF,1.0-plant(BR,LF)%NFLF2,1.0-AMAX1(0.0,AMIN1(1.0,plant(BR,LF)%AFLF)),1.0-plant(BR,LF)%TFGLF,1.0-plant(BR,LF)%TFDLF,plant(BR,LF)%DGLF, &  
                        plant(BR,LF)%DALF,plant(BR,LF)%DSLF,plant(BR,LF)%DGLF+plant(BR,LF)%DALF+plant(BR,LF)%DSLF,plant(BR,LF)%LDEATHDAP
                ENDDO
            ENDDO
                
            IF (RUN.EQ.1) THEN
                WRITE(fnumlvs,*)' '
                WRITE(fnumlvs,'( A)')'! NB. Data are summed over all fork branches'
                WRITE(fnumlvs,*)' '
                WRITE(fnumlvs,'( A)')'! LNUM  = Number of leaf on one axis '
                WRITE(fnumlvs,'( A)')'! AREAP = Potential area of leaf (cm2) '
                WRITE(fnumlvs,'(2A)')'! AREA1 = Area of leaf ', 'without stress (cm2)'
                WRITE(fnumlvs,'(2A)')'! AREA2 = Area of leaf ', 'with stress (cm2)'
                WRITE(fnumlvs,'(2A)')'! AREAT = Area of cohort of leaves at leaf',' position (cm2) '
                WRITE(fnumlvs,'(2A)')'! AREAS = Senesced area of cohort of leaves', ' at harvest at leaf position (cm2) '
                WRITE(fnumlvs,'(2A)')'! WFLF  = Water stress factor for leaf',' (0-1,1=0 stress)'
                WRITE(fnumlvs,'( A)')'! NFLF  = N stress factor for leaf (0-1,1=0 stress)'
                WRITE(fnumlvs,'( A)')'! NFLF2 = N factor for area adjustment (0-1,1=0 stress)'
                ! WRITE(fnumlvs,'(2A)')'! NFLFP = N stress factor for photosynthesis',' (0-1,1=0 stress)'
                WRITE(fnumlvs,'(2A)')'! AFLF  = Assimilate factor for leaf',' (0-1,1=0 no limitation)'
                WRITE(fnumlvs,'(2A)')'! TFGLF = Temperature factor for leaf expansion ',' (0-1,1=0 no limitation)'
                WRITE(fnumlvs,'(2A)')'! TFDLF = Temperature factor for leaf development',' (0-1,1=0 no limitation)'
                WRITE(fnumlvs,'( A)')'! LLIFG = Number of days of leaf growing      '
                WRITE(fnumlvs,'( A)')'! LLIFA = Number of days of leaf fully active '
                WRITE(fnumlvs,'( A)')'! LLIFS = Number of days of leaf senescing    '
                WRITE(fnumlvs,'( A)')'! LLIFE = Total number of days of leaf longevity    '
                WRITE(fnumlvs,'( A)')'! DAP   = Leaf decease day after planting    '
            ENDIF
            CLOSE (FNUMLVS)
            ! End of Leaves.out
                    
            ! Branching tier conditions (Simulated; TIERS.OUT)
            OPEN(UNIT=FNUMPHA,FILE=FNAMEPHASES,POSITION='APPEND')
            WRITE (FNUMPHA,'(/,A79,/)') OUTHED
            WRITE (fnumpha,'(A42,A24,A12)')'@ TIER SRADA  TMXA  TMNA  PREA  TWLA  CO2A','  WFPA  WFGA  NFPA  NFGA', &
                '  TIER_END  '
            !DO L=1,MSTG-2       !LPM  07MAR15 MSTG TO PSX
            DO L=0,PSX-2
                IF (STGYEARDOY(L).LT.9999999.AND.L.NE.0.AND.L.NE.10.AND.L.NE.11) &
                    WRITE (fnumpha,'(I6,3F6.1,2F6.2,I6,4F6.2,1X,A13)')L,sradpav(L),tmaxpav(L),tminpav(L),rainpav(L), &
                    daylpav(L),NINT(co2pav(L)),1.0-wfppav(L),1.0-wfgpav(L),1.0-nfppav(L),1.0-nfgpav(L), &
                    psname(MIN(L+1,PSX))
            ENDDO
            !IF(yeardoyharf.EQ.yeardoy)THEN             !LPM  07MAR15 MSTG TO PSX
            !    WRITE (fnumpha,'(I6,3F6.1,2F6.2,I6,4F6.2,1X,A13)')mstg-1,sradpav(mstg-1),tmaxpav(mstg-1), &
            !        tminpav(mstg-1),rainpav(mstg-1),daylpav(mstg-1),NINT(co2pav(mstg-1)),1.0-wfppav(mstg-1), &
            !        1.0-wfgpav(mstg-1),1.0-nfppav(mstg-1),1.0-nfgpav(mstg-1),'Harvest      '
            !ELSE 
            !    WRITE (fnumpha,'(I6,3F6.1,2F6.2,I6,4F6.2,1X,A13)')mstg-1,sradpav(mstg-1),tmaxpav(mstg-1), &
            !        tminpav(mstg-1),rainpav(mstg-1),daylpav(mstg-1),NINT(co2pav(mstg-1)),1.0-wfppav(mstg-1), &
            !        1.0-wfgpav(mstg-1),1.0-nfppav(mstg-1),1.0-nfgpav(mstg-1),psname(mstg)
            !ENDIF
            IF(yeardoyharf.EQ.yeardoy)THEN
                WRITE (fnumpha,'(I6,3F6.1,2F6.2,I6,4F6.2,1X,A13)')psx-1,sradpav(psx-1),tmaxpav(psx-1), &
                    tminpav(psx-1),rainpav(psx-1),daylpav(psx-1),NINT(co2pav(psx-1)),1.0-wfppav(psx-1), &
                    1.0-wfgpav(psx-1),1.0-nfppav(psx-1),1.0-nfgpav(psx-1),'Harvest      '
            ELSE 
                WRITE (fnumpha,'(I6,3F6.1,2F6.2,I6,4F6.2,1X,A13)')psx-1,sradpav(psx-1),tmaxpav(psx-1), &
                    tminpav(psx-1),rainpav(psx-1),daylpav(psx-1),NINT(co2pav(psx-1)),1.0-wfppav(psx-1), &
                    1.0-wfgpav(psx-1),1.0-nfppav(psx-1),1.0-nfgpav(psx-1),psname(psx)
            ENDIF
            CLOSE (FNUMPHA)
                    
        ELSE
                    
            OPEN (UNIT=FNUMLVS,FILE=FNAMELEAVES,STATUS='UNKNOWN')
            CLOSE (UNIT=FNUMLVS, STATUS = 'DELETE')
            OPEN (UNIT=FNUMPHA,FILE=FNAMEPHASES,STATUS = 'UNKNOWN')
            CLOSE (UNIT=FNUMPHA, STATUS = 'DELETE')
        ENDIF
        ! End of Leaves and Tier writes
    END SUBROUTINE YCA_Out_LfTier
