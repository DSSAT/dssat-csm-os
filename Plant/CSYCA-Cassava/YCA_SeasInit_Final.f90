!**********************************************************************************************************************
! This is the code from the section (DYNAMIC == RUNINIT) ! Initialization, lines 3550 - 3941 of the original CSCAS code.
! The actual and dummy arguments are only for those variables that are dummy arguments for CSCAS. The type of all
! other variables are declared in the module YCA_First_Trans_m. The type of only the dummy arguments are declared here.
! The variables and their units are defined in CSCAS.
!
! SUBROUTINE YCA_SeasInit_Final sets up the output descriptors, checks controls and writes information to the overview and 
! work files
!**********************************************************************************************************************
    
    SUBROUTINE YCA_SeasInit_Final ( &
        ALBEDOS     , CLOUDS      , CN          , KCAN        , KEP         , ON          , &
        RN          , RUN         , RUNI        , RWUMX       , SN          , TAIRHR      , &
        TN           &
        )

! 2023-01-25 chp removed unused variables
!       IDETG       , ISWDIS      , ISWNIT      , ISWWAT      , RNMODE      , SLPF        , 

        USE YCA_Formats_m
        USE YCA_First_Trans_m
       
        IMPLICIT NONE
        EXTERNAL LTRIM, TVILENT, WARNING, XREADC
        
!       CHARACTER(LEN=1) IDETG       , ISWDIS      , ISWNIT      , ISWWAT      , RNMODE        
        INTEGER CN          , ON          , RN          , RUN         , RUNI        , SN          , TN         
        REAL    ALBEDOS     , CLOUDS      , KCAN        , KEP         , RWUMX       , TAIRHR(24)  ! SLPF        ,          
        INTEGER TVILENT                                                                       ! Integer function call.
        
        
!-----------------------------------------------------------------------
!       Create output descriptors
!-----------------------------------------------------------------------

        ! Run name
        IF (runname(1:6) == '      ' .OR.runname(1:3) == '-99') runname = tname
        
        ! Composite run variable
        IF (RUNI < 10) THEN
            WRITE (RUNRUNI,'(I3,A1,I1,A3)') RUN,',',RUNI,'   '
        ELSEIF (RUNI >= 10.AND.RUNI < 100) THEN
            WRITE (RUNRUNI,'(I3,A1,I2,A2)') RUN,',',RUNI,'  '
        ELSE
            WRITE (RUNRUNI,'(I3,A1,I3,A1)') RUN,',',RUNI,' '
        ENDIF
        IF (RUN < 10) THEN
            RUNRUNI(1:6) = RUNRUNI(3:8)
            RUNRUNI(7:8) = '  '
            ! Below is to give run number only for first run
            IF (RUNI <= 1) RUNRUNI(2:8) = '       '
        ELSEIF (RUN >= 10.AND.RUN < 100) THEN
            RUNRUNI(1:7) = RUNRUNI(2:8)
            RUNRUNI(8:8) = ' '
            ! Below is to give run number only for first run
            IF (RUNI <= 1) RUNRUNI(3:8) = '      '
        ENDIF
        
        ! Composite treatment+run name
        CALL LTRIM (RUNNAME)
        RUNNAME = TRIM(RUNNAME)
        CALL LTRIM (TNAME)
        TNAME = TRIM(TNAME)
        LENTNAME = MIN(15,TVILENT(TNAME))
        LENRNAME = MIN(15,TVILENT(RUNNAME))
        IF (LENRNAME > 5) THEN
            TRUNNAME = RUNNAME(1:LENRNAME)//' '//MODNAME
        ELSE
            TRUNNAME = TNAME(1:LENTNAME)//' '//MODNAME
        ENDIF
        IF (MEEXP == 'E') THEN
            CALL LTRIM (TRUNNAME)
            LENTNAME = TVILENT(TRUNNAME)
            TRUNNAME = TRUNNAME(1:LENTNAME)//' EXPERIMENTAL'
        ENDIF
        
        ! File header
        IF (CN > 1) THEN
            IF (TN < 10) THEN
                WRITE (OUTHED, FMT7104) RUNRUNI(1:5),EXCODE,TN,RN,CN,TRUNNAME
            ELSEIF (TN >= 10.AND.TN < 100) THEN
                WRITE (OUTHED, FMT7105) RUNRUNI,EXCODE,TN,RN,CN,TRUNNAME
            ELSEIF (TN >= 10 .AND. TN < 100) THEN
                WRITE (OUTHED, FMT7106) RUNRUNI,EXCODE,TN,RN,CN,TRUNNAME
            ENDIF
        ELSE
            IF (TN < 10) THEN
                WRITE (OUTHED, FMT7107) RUNRUNI(1:5),EXCODE,TN,TRUNNAME
            ELSEIF (TN >= 10.AND.TN < 100) THEN
                WRITE (OUTHED, FMT7108) RUNRUNI,EXCODE,TN,RN,TRUNNAME
            ELSEIF (TN >= 10 .AND. TN < 100) THEN
                WRITE (OUTHED, FMT7109) RUNRUNI,EXCODE,TN,RN,TRUNNAME
            ENDIF
        ENDIF
        
        !-----------------------------------------------------------------------
        !       Check controls
        !-----------------------------------------------------------------------
        
        ! Water and N uptake methods .. MEWNU 
        ! R=RLV+LL complex,W=RLV for h20,N=RLV for N,B=RLV for both
        IF (MEWNU /= 'R') THEN 
            IF (MEWNU /= 'W') THEN 
                IF (MEWNU /= 'N') THEN 
                    IF (MEWNU /= 'B') THEN 
                        MEWNU = 'R'
                    ENDIF        
                ENDIF        
            ENDIF        
        ENDIF        
        IF (MEPHO /='R' .AND. MEPHO /='V') THEN ! if photosynthesis method doesn't exists
                    WRITE(MESSAGE(1),'(A22,A1,A15)')'Photosynthesis method ',MEPHO,' not an option '
                    WRITE(MESSAGE(2),'(A42)')' changed to V (RUE with hourly VPD effect)'
                    CALL WARNING(2,'CSYCA',MESSAGE)
                    MEPHO = 'V'
        ENDIF
        !LPM 06FEB2021 Add a warning to recommend the use of MEEVP= H when MEPHO = V
        IF (MEPHO == 'V'.AND. MEEVP /='H') THEN 
                    WRITE(MESSAGE(1),'(A26,A1,A51)')'Evapotranspiration method ',MEEVP,' is not recommended when photosynthesis method is V.'
                    WRITE(MESSAGE(2),'(A66)')'Consider using method H (Priestley-Taylor with hourly VPD effect).'
                    CALL WARNING(2,'CSYCA',MESSAGE)
        ENDIF
        ! Other CSM codes are:
        !  C Canopy photosynthesis curve.
        !  L Leaf photosynthesis response curve
        IF (IHARI /= 'M') THEN
            IF (hnumber <= 0) THEN 
                WRITE(MESSAGE(1),'(A37,A13,A1)')'No harvest date set although planting','flag set to: ',IHARI
                MESSAGE(2)='Flag reset to M.'
                CALL WARNING(2,'CSYCA',MESSAGE)
                IHARI = 'M'                      
            ENDIF
        ENDIF        
        
        !-----------------------------------------------------------------------
        !       Set equivalences to avoid compile errors
        !-----------------------------------------------------------------------
        
        tvr1 = tairhr(1)
        ! When running in CSM
        IF (FILEIOT == 'DS4') THEN
            ALBEDO = ALBEDOS  ! Previously 0.2
            CLOUDS = 0.0
        ELSE
            ALBEDO = ALBEDOS  
        ENDIF
        
        !-----------------------------------------------------------------------
        !       Record starting values and files
        !-----------------------------------------------------------------------
        
        CNI = CN
        KCANI = KCAN
        ONI = ON
        RNI = RN
        RWUMXI = RWUMX
        SNI = SN
        TNI = TN
        KEPI = KEP
        
        !-----------------------------------------------------------------------
        !       Create and write warning messages re. input and output variables
        !-----------------------------------------------------------------------
                
        ! Control switch for OUTPUT file names
        CALL XREADC (FILEIO,TN,RN,SN,ON,CN,'FNAME',fname)
        
        ! Output choices
        ! 1=Reserves in laminae,stem,Plant. stick,SLA;stem includes petioles
        ! 2=No reserves;stem includes petioles
        ! 3=No reserves;stem does not include petioles
        OUTCHOICE = 3
        IF (OUTCHOICE == 2) THEN
            WRITE(MESSAGE(1),'(A36,A37)')'  NO reserve CH2O included in the wt',' of leaves,stems,and Plant. stick.          ' 
            CALL WARNING(1,'CSYCA',MESSAGE)
        ELSEIF (OUTCHOICE == 3) THEN
            WRITE(MESSAGE(1),'(A36,A37)')'  NO reserve CH2O included in the wt',' of leaves,stems,and Plant. stick.          ' 
            CALL WARNING(1,'CSYCA',MESSAGE)
        ENDIF  
                
    END SUBROUTINE YCA_SeasInit_Final
