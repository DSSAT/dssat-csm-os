!=======================================================================
!  Aloha_OpGrow, Subroutine, C.H.Porter
!  Generates daily output for Aloha-Pineapple model
!-----------------------------------------------------------------------
!  REVISION       HISTORY
!  03/28/2017 CHP Written
!=======================================================================
      SUBROUTINE Aloha_OpGrow (CONTROL, ISWITCH, &
        SWFAC, TURFAC, NSTRES, SATFAC, MDATE)

      USE Aloha_mod
      IMPLICIT  NONE
      SAVE

      INTEGER DYNAMIC, ERRNUM, NOUTDG, NOUTPN, RUN, ISTAGE, TIMDIF
      CHARACTER*1 IDETG, IDETL, IDETN, FMOPT, ISWNIT
      LOGICAL FEXIST
      REAL SWF_AV, TUR_AV, NST_AV, EXW_AV, PS1_AV , PS2_AV, KST_AV
      REAL SWFAC, TURFAC, NSTRES, SATFAC, PSTRES1, PSTRES2, KSTRES
      REAL LFWT, PLTPOP, SDWT, GRNWT, XLAI, LAI, GPSM, CRWNWT, GPP, TRNU, LEAFNO, LN
      REAL GM2KG, SHELPC, SHELLW, SDSIZE, HI, BIOMAS, VWAD, STMWT
      REAL RTWT, RTDEP, RLV(NL)
      REAL FRTWT, BASLFWT, SKWT
      REAL STOVN, GRAINN, STOVWT, ROOTN, WTNVEG, WTNGRN, PCNVEG, PCNGRN

      INTEGER I
      INTEGER DAP,YRPLT,YRDOY
      INTEGER RSTAGE,DAS
      INTEGER COUNT, FROP, MDATE, YEAR, DOY

      REAL    WTNUP
      REAL    SLA,PODWT,PODNO
      REAL    WTLF,SEEDNO,WTNLF,WTNST,WTNSD,WTNSH,WTNRT, WTNCAN
      REAL    PCNL,PCNST,PCNRT,PCNSH,VSTAGE,CANHT,CANWH

      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH

      DYNAMIC = CONTROL % DYNAMIC
      RUN     = CONTROL % RUN
      FROP    = CONTROL % FROP

      IDETG = ISWITCH % IDETG
      IDETN = ISWITCH % IDETN
      IDETL = ISWITCH % IDETL
      FMOPT = ISWITCH % FMOPT
      ISWNIT= ISWITCH % ISWNIT


!***********************************************************************
!***********************************************************************
!     Run initialization - run once per simulation
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
!-----------------------------------------------------------------------
        CALL GETLUN('PlantGro.OUT', NOUTDG)
        CALL GETLUN('PlantN.OUT'  , NOUTPN)

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
        IF (IDETG .EQ. 'Y') THEN
!         Initialize daily growth output file
          INQUIRE (FILE = 'PlantGro.OUT', EXIST = FEXIST)
          IF (FEXIST) THEN
            OPEN (UNIT = NOUTDG, FILE = 'PlantGro.OUT', STATUS = 'OLD', IOSTAT = ERRNUM, POSITION = 'APPEND')
          ELSE
            OPEN (UNIT = NOUTDG, FILE = 'PlantGro.OUT', STATUS = 'NEW', IOSTAT = ERRNUM)
            WRITE(NOUTDG,'("*Daily plant growth output file")')
          ENDIF
          
          !Write headers
          CALL HEADER(SEASINIT, NOUTDG, RUN)

          WRITE (NOUTDG,2192) '!YR    Days   Leaf  Grow                      Dry  ' // &
                'Weight                           Pod      Phot. Grow       Leaf  ' // &
                'Shell  Spec    Canopy          Root  ³                Root Length' // &
                'Density                     ³'
          WRITE (NOUTDG,2192) '! and  after  Num  Stage  LAI   Leaf  Stem Fruit' // &
            ' Root Basal Crown  Suck   HI   Wgt.   No.    Water     Nit.   Nit ' // &
            '  -ing  Leaf  Hght Brdth       Depth  ³                cm3/cm3  of' // &
            ' soil                        ³'
          WRITE (NOUTDG,2192) '!  DOY plant                    ³<---------------' // &
             ' kg/Ha -------------->³       Kg/Ha        ³<Stress (0-1)>³    %  ' // &
             '   %   Area    m     m           m   ³<---------------------------' // &
             '--------------------------->³'
          WRITE (NOUTDG,2192) '@DATE   CDAY  L#SD  GSTD  LAID  LWAD  SWAD  FWAD ' // &
             ' RWAD  BWAD  CRAD  SUGD  HIAD  EWAD  E#AD  WSPD  WSGD  NSTD  LN%D ' // &
             ' SH%D  SLAD  CHTD  CWID  EWSD  RDPD  RL1D  RL2D  RL3D  RL4D  RL5D ' // &
             ' RL6D  RL7D  RL8D  RL9D  RL10'
  2192    FORMAT (A210)
        ENDIF

!-----------------------------------------------------------------------
!       Initialize daily plant nitrogen output file
        IF (IDETN .EQ. 'Y') THEN
          INQUIRE (FILE = 'PlantN.OUT', EXIST = FEXIST)
          IF (FEXIST) THEN
            OPEN (UNIT = NOUTPN, FILE = 'PlantN.OUT', STATUS = 'OLD',IOSTAT = ERRNUM, POSITION = 'APPEND')
          ELSE
            OPEN (UNIT = NOUTPN, FILE = 'PlantN.OUT', STATUS = 'NEW',IOSTAT = ERRNUM)
            WRITE(NOUTPN,'("*Daily plant N output file")')
          ENDIF
          
          !Write headers
          CALL HEADER(SEASINIT, NOUTPN, RUN)

          WRITE (NOUTPN,2240) '!            <--------Plant N (kg/ha) ---------> <---------- Plant N (%) ---------->'
          WRITE (NOUTPN,2240) '!            Uptak  Crop Grain   Veg  Leaf  Stem Grain   Veg  Leaf  Stem Shell  Root'
          WRITE (NOUTPN,2240) '@DATE    DAP  NUPC  CNAD  GNAD  VNAD  LNAD  SNAD  GN%D  VN%D  LN%D  SN%D  SHND  RN%D'
  2240    FORMAT (A252)
        ENDIF
!-----------------------------------------------------------------------
        !CUMSENSURF  = 0.0
        !CUMSENSOIL  = 0.0
        !CUMSENSURFN = 0.0
        !CUMSENSOILN = 0.0 
        SWF_AV = 0.0
        TUR_AV = 0.0
        NST_AV = 0.0
        EXW_AV = 0.0
        PS1_AV = 0.0
        PS2_AV = 0.0
        KST_AV = 0.0
        COUNT = 0

        WTNUP   = 0.0
        PSTRES1 = 1.0
        PSTRES2 = 1.0
        KSTRES  = 1.0

!***********************************************************************
!***********************************************************************
!     DAILY OUTPUT
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. OUTPUT) THEN
!-----------------------------------------------------------------------
        IF (YRDOY .LT. YRPLT .OR. YRPLT .LT. 0) RETURN

!       Compute average stress factors since last printout
        SWF_AV = SWF_AV + (1.0 - SWFAC)
        TUR_AV = TUR_AV + (1.0 - TURFAC)
        NST_AV = NST_AV + (1.0 - NSTRES)
        EXW_AV = EXW_AV + SATFAC
        PS1_AV = PS1_AV + (1.0 - PSTRES1)
        PS2_AV = PS2_AV + (1.0 - PSTRES2)
        KST_AV = KST_AV + (1.0 - KSTRES)
        COUNT = COUNT + 1

!!       Accumulate senesced matter for surface and soil.
!        SENSURFT = SENESCE % ResWt(0)
!        CUMSENSURF  = CUMSENSURF  + SENESCE % ResWt(0)
!        CUMSENSURFN = CUMSENSURFN + SENESCE % ResE(0,1) 
!      
!        SENSOILT = 0.0
!        DO L = 1, NLAYR
!          SENSOILT    = SENSOILT    + SENESCE % ResWt(L)
!          CUMSENSOIL  = CUMSENSOIL  + SENESCE % ResWt(L)
!          CUMSENSOILN = CUMSENSOILN + SENESCE % ResE(L,1)
!        ENDDO

!-----------------------------------------------------------------------
!       Do we print today?
!-----------------------------------------------------------------------
        IF ((MOD(DAS,FROP) .EQ. 0)    &    !Daily output every FROP days,
          .OR. (YRDOY .EQ. YRPLT)     &    !on planting date, and
          .OR. (YRDOY .EQ. MDATE)) THEN    !at harvest maturity 

          DAP = MAX(0,TIMDIF(YRPLT,YRDOY))
          IF (DAP > DAS) DAP = 0
          CALL YR_DOY(YRDOY, YEAR, DOY) 

!         Compute average stress factors since last printout
          IF (COUNT > 0) THEN
            SWF_AV = SWF_AV / COUNT
            TUR_AV = TUR_AV / COUNT
            NST_AV = NST_AV / COUNT
            EXW_AV = EXW_AV / COUNT
            PS1_AV = PS1_AV / COUNT
            PS2_AV = PS2_AV / COUNT
            KST_AV = KST_AV / COUNT
            COUNT = 0
          ENDIF

!-----------------------------------------------------------------------
!         PlantGro.OUT
!-----------------------------------------------------------------------
          IF (IDETG == 'Y') THEN
            WTLF   = LFWT   * PLTPOP
            SDWT   = GRNWT  * PLTPOP
            XLAI   = LAI
            IF (WTLF .GT. 0.0) THEN
               SLA  = LAI * 10000 / WTLF
             ELSE
               SLA = 0.0
            ENDIF
            SEEDNO = GPSM
            PODWT  = CRWNWT
            IF (GPP .GT. 0.0) THEN
               PODNO = SEEDNO/GPP
             ELSE
               PODNO = 0.0
            ENDIF
            
            WTNUP = WTNUP + TRNU * PLTPOP
            
            LEAFNO = LN
            VSTAGE = REAL (LEAFNO)
            IF (LEAFNO .GT. 0.0) THEN
               RSTAGE = REAL(ISTAGE)
             ELSE
               RSTAGE = 0.0
            ENDIF
            SDWT = GRNWT
            
!           GM2KG converts gm/plant to kg/ha
            GM2KG  = PLTPOP * 10.0
            SHELPC = 0.0
            IF (PODWT .GT. 0.1) THEN
               SHELPC = SDWT*100.0/PODWT
            ENDIF
            SHELLW = PODWT - SDWT
            SDSIZE = 0.0
            IF (SEEDNO .GT. 0.0) THEN
               SDSIZE = SDWT*PLTPOP/SEEDNO*1000.0
            ENDIF
            HI     = 0.0
            IF (BIOMAS .GT. 0.0 .AND. SDWT .GE. 0.0) THEN
               HI = SDWT*PLTPOP/BIOMAS
            ENDIF
            
            VWAD = NINT(WTLF*10. + STMWT*10.)

            IF (FMOPT /= 'C') THEN   ! VSH
              WRITE (NOUTDG,400) YRDOY,DAP,VSTAGE,RSTAGE,XLAI,      &
                NINT(WTLF*10.0),NINT(STMWT*GM2KG),NINT(FRTWT*GM2KG),           &
                NINT(RTWT*GM2KG),NINT(BASLFWT*10.0),NINT(CRWNWT*GM2KG),        &
                NINT(SKWT*GM2KG),HI,                                           &
                NINT(PODWT*GM2KG),NINT(PODNO),(1.0-SWFAC),(1.0-TURFAC),        &
                (1.0-NSTRES),PCNL,SHELPC,SLA,CANHT,CANWH,SATFAC,               &
                (RTDEP/100),(RLV(I),I=1,10)
!DATE   CDAY  L#SD  GSTD  LAID  LWAD  SWAD  FWAD  RWAD  BWAD  CRAD  SUGD  HIAD  EWAD  E#AD  WSPD  WSGD  NSTD  LN%D  SH%D  SLAD  CHTD  CWID  EWSD  RDPD  RL1D  RL2D  RL3D  RL4D  RL5D  RL6D  RL7D  RL8D  RL9D  RL10
 400          FORMAT (2(1X,I5),1X,F5.1,1X,I5,1X,F5.2,7(1X,I5),           &
                1X,F5.3,2(1X,I5),3(1X,F5.3),2(1X,F5.2),1X,F5.1,          &
                2(1X,F5.2),1X,F5.3,11(1X,F5.2))

!-------------------------------------------------------------------------
!           VSH CSV output corresponding to PlantGro.OUT
            ELSE   ! VSH
              !CALL CsvOut(EXPNAME,CONTROL%RUN, CONTROL%TRTNUM,CONTROL%ROTNUM,  &
              !  CONTROL%REPNO, YEAR, DOY, DAS, DAP, VSTAGE, RSTAGE, XLAI,               &
              !  WTLF, STMWT, SDWT, RTWT, VWAD, TOPWT, SEEDNO, SDSIZE, HI, PODWT,        &
              !  PODNO, SWF_AV, TUR_AV, NST_AV, PS1_AV, PS2_AV, KST_AV, EXW_AV,          &
              !  PCNLP, SHELPC, HIP, PODWTD, SLAP, CANHT, CANWH,                         &
              !  DWNOD, RTDEP, N_LYR, RLV, CUMSENSURF, CUMSENSOIL,                       &
              !  vCsvline, vpCsvline, vlngth)
              !
              !CALL Linklst(vCsvline)
            ENDIF
          ENDIF     !Print PlantGro report

!-----------------------------------------------------------------------
!         PlantN.OUT
!-----------------------------------------------------------------------
          IF (IDETN .EQ. 'Y' .AND. ISWNIT .EQ. 'Y') THEN
            WTNCAN = (STOVN + GRAINN) * PLTPOP
            IF ((LFWT+STMWT) .GT. 0.0) THEN
               WTNLF = STOVN * (LFWT  / STOVWT) * PLTPOP
               WTNST = STOVN * (STMWT / (LFWT + STMWT)) * PLTPOP
             ELSE
               WTNLF = 0.0
               WTNST = 0.0
            ENDIF
            WTNSD = GRAINN * PLTPOP
            WTNRT = ROOTN * PLTPOP        ! Is this right?
            WTNSH = 0.0

            IF (LFWT .GT. 0.0) THEN
               PCNL = WTNLF /( LFWT * PLTPOP) * 100.0
             ELSE
               PCNL = 0.0
            ENDIF
            IF (STMWT .GT. 0.0) THEN
               PCNST = WTNST/(STMWT * PLTPOP) * 100.0
             ELSE
               PCNST = 0.0
            ENDIF
            IF (RTWT .GT. 0.0) THEN
               PCNRT = ROOTN/RTWT * 100.0
             ELSE
               PCNRT = 0.0
            ENDIF

            WTNVEG  = (WTNLF + WTNST)
            WTNGRN  = (WTNSH + WTNSD)
            IF ((WTLF+STMWT) .GT. 0.0) THEN
               PCNVEG = (WTNLF+WTNST)/(WTLF+(STMWT*PLTPOP))*100.0
             ELSE
               PCNVEG = 0.0
            ENDIF
            IF (SDWT .GT. 0.0) THEN
               PCNGRN = WTNSD/SDWT*100
             ELSE
               PCNGRN = 0.0
            ENDIF

!-----------------------------------------------------------------------

            IF (FMOPT /= 'C') THEN       ! VSH
              WRITE (NOUTPN,300) YRDOY,DAP,(WTNUP*10.0),                   &
                     (WTNCAN*10.0),(WTNSD*10.0),(WTNVEG*10.0),(WTNLF*10.0),(WTNST*10.0),      &
                     PCNGRN,PCNVEG,PCNL,PCNST,PCNSH,PCNRT
            !DATE    DAP  NUPC  CNAD  GNAD  VNAD  LNAD  SNAD  GN%D  VN%D  LN%D  SN%D  SHND  RN%D
  300         FORMAT (2(1X,I5),1X,F5.1,1X,I5,1X,F5.2,7(1X,I5),1X,F5.3,     &
                1X,F5.3,2(1X,I5),3(1X,F5.3),2(1X,F5.2),1X,F5.1,            &
                2(1X,F5.2),1X,F5.3,11(1X,F5.2))
 

!---------  --------------------------------------------------------------
!           CSV output corresponding to PlantN.OUT
            !     VSH
            ELSE
              !CALL CsvOutPlNCrGro(EXPNAME, CONTROL%RUN, CONTROL%TRTNUM,     &
              !  CONTROL%ROTNUM, CONTROL%REPNO, YEAR, DOY, DAS, DAP,         &
              !  WTNCAN, WTNSD, WTNVEG, PCNSDP, PCNVEG, WTNFX, WTNUP,        &
              !  WTNLF, WTNST, PCNLP, PCNSTP, PCNSHP, PCNRTP, NFIXN,         &
              !  CUMSENSURFN, CUMSENSOILN,                                   &
              !  vCsvlinePlNCrGro, vpCsvlinePlNCrGro, vlngthPlNCrGro)
              !
              !CALL LinklstPlNCrGro(vCsvlinePlNCrGro)
              !
            ENDIF
          ENDIF !Print plant N report
        ENDIF   !Print today
!-----------------------------------------------------------------------

!       Set average stress factors since last printout back to zero
        SWF_AV = 0.0
        TUR_AV = 0.0
        NST_AV = 0.0
        EXW_AV = 0.0
        PS1_AV = 0.0
        PS2_AV = 0.0
        KST_AV = 0.0


!-----------------------------------------------------------------------

      CALL OPVIEW ()

      RETURN


!***********************************************************************
!***********************************************************************
!     Seasonal Output 
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. SEASEND) THEN
!-----------------------------------------------------------------------
        IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN   ! VSH
          !Close daily output files.
          CLOSE (NOUTDG)
          CLOSE (NOUTPN)
        END IF   ! VSH

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
!-----------------------------------------------------------------------
!     FORMAT Strings
!----------------------------------------------------------------------
 !200  FORMAT (2(1X,I5),3(1X,F5.1),2(1X,F5.2),1X,I5,4(1X,F5.1),1X,I5)
 !300  FORMAT (2(1X,I5),3(1X,F5.1),2(1X,F5.2),1X,I5,4(1X,F5.1),1X,I5,
 !    &        2(1X,F5.1),4(1X,F5.2),23(1X,F5.1))
 !
 !
 !
 !100  FORMAT (/,'*RUN ',I3,8X,': ',A25,/,
 !    &       1X,'MODEL',10X,':'1X,A8,' - ',A10,/,
 !    &       1X,'EXPERIMENT',5X,':',1X,A8,1X,A2,4X,A47,/,
 !    &       1X,'TREATMENT',1X,I2, 3X,':',1X,A25,/)
 !2190 FORMAT (A80)
 !2196 FORMAT (4(A5,I1))
 !2197 FORMAT (10(A5,I1))
 !2090 FORMAT (A240)
 !2340 FORMAT (A252)




!***********************************************************************
      RETURN
      END SUBROUTINE Aloha_OpGrow
!======================================================================
