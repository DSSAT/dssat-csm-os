!=======================================================================
!  Aloha_OpGrow, Subroutine, C.H.Porter
!  Generates daily output for Aloha-Pineapple model
!-----------------------------------------------------------------------
!  REVISION       HISTORY
!  03/28/2017 CHP Written
!=======================================================================
      SUBROUTINE Aloha_OpGrow (CONTROL, ISWITCH,          &
        BASLFWT, BIOMAS, CRWNWT, EYEWT, FLRWT, FRTWT,     &
        FRUITS, GPP, GPSM, ISTAGE, LAI, LFWT, LN, MDATE,  &
        NSTRES, PLTPOP, RLV, ROOTN,  RTDEP, RTWT, SKWT,   &
        STMWT, STOVN, STOVWT, SWFAC, TURFAC, WTNCAN,      &
        WTNGRN, WTNUP, YRPLT,                             &
        VNAM, VWATM, CNAM)    !Output for Overview.OUT 

      USE Aloha_mod
      IMPLICIT  NONE
      EXTERNAL GETLUN, HEADER, TIMDIF, YR_DOY
      SAVE

      INTEGER DYNAMIC, ERRNUM, NOUTDG, NOUTPN, RUN, ISTAGE, TIMDIF
      CHARACTER*1 IDETG, IDETL, IDETN, FMOPT, ISWNIT
      LOGICAL FEXIST
      REAL SWF_AV, TUR_AV, NST_AV, EXW_AV, PS1_AV , PS2_AV, KST_AV
      REAL SWFAC, TURFAC, NSTRES, PSTRES1, PSTRES2, KSTRES
      REAL LFWT, PLTPOP, SDWT, XLAI, LAI, CRWNWT, LN   
      REAL GM2KG, HI, BIOMAS, VWAD, STMWT
      REAL RTWT, RTDEP, RLV(NL)
      REAL BASLFWT, SKWT, TOPWT 
      REAL STOVN, GRAINN, STOVWT, ROOTN, WTNVEG, WTNGRN, PCNVEG, PCNGRN
      REAL VNAM, VWATM, CNAM

      INTEGER I, LEAFNO
      INTEGER DAP,YRPLT,YRDOY
      INTEGER DAS
      INTEGER COUNT, FROP, MDATE, YEAR, DOY

      REAL    WTNUP, SLA
      REAL    WTLF,WTNLF,WTNST,WTNSD,WTNSH,WTNRT, WTNCAN
      REAL    PCNL,PCNST,PCNRT,VSTAGE 

      REAL    LWAD, SWAD, CRAD, BWAD, SUGD, RWAD, FWAD, EYWAD, EYEWT, FLWAD
      REAL    FRUITS, FRTWT, FLRWT, GPP, GPSM

      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH

      DYNAMIC = CONTROL % DYNAMIC
      RUN     = CONTROL % RUN
      FROP    = CONTROL % FROP
      YRDOY   = CONTROL % YRDOY
      DAS     = CONTROL % DAS

      IDETG = ISWITCH % IDETG
      IDETN = ISWITCH % IDETN
      IDETL = ISWITCH % IDETL
      FMOPT = ISWITCH % FMOPT
      ISWNIT= ISWITCH % ISWNIT

      PLTPOP    = PLANTING % PLTPOP

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

          WRITE (NOUTDG,'(A,/,A,/,A,/,A)') & 
 '!                       Leaf   Grow        <--------------------------- Dry  Weight --------------------------->   Harv <--- Eye ---> <-- Stress (0-1) -->   Leaf   Spec   Root  <--------------- Root Length Density ------------------------------>', &
 '!                        Num  Stage    LAI   Tops    Veg   Leaf   Stem Flower  Fruit  Crown  Basal   Suck   Root  Index   Wgt.    No.      Water      Nitr   Nitr   Leaf  Depth  <---------------   cm3/cm3  of soil  ------------------------------>', &
 '!                                          <------------------------------ kg/Ha ------------------------------>         kg/ha          Phot   Grow             %   Area      m  <------------------------------------------------------------------>', &
 '@YEAR DOY   DAS   DAP   L#SD   GSTD   LAID   CWAD   VWAD   LWAD   SWAD  FLWAD   FWAD   CRAD   BWAD   SUGD   RWAD   HIAD  EYWAD  EY#AD   WSPD   WSGD   NSTD   LN%D   SLAD   RDPD   RL1D   RL2D   RL3D   RL4D   RL5D   RL6D   RL7D   RL8D   RL9D   RL10'

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

          WRITE (NOUTPN,'(A)') '!                     <-----------Plant N (kg/ha) ------------> <------ Plant N (%) ------>'
          WRITE (NOUTPN,'(A)') '!                     Uptake   Crop    Veg   Leaf   Stem   Root    Veg   Leaf   Stem   Root'
          WRITE (NOUTPN,'(A)') '@YEAR DOY   DAS   DAP   NUPC   CNAD   VNAD   LNAD   SNAD   RNAD   VN%D   LN%D   SN%D   RN%D'
        ENDIF
!-----------------------------------------------------------------------
!        CUMSENSURF  = 0.0
!        CUMSENSOIL  = 0.0
!        CUMSENSURFN = 0.0
!        CUMSENSOILN = 0.0 

        SWF_AV = 0.0
        TUR_AV = 0.0
        NST_AV = 0.0
        EXW_AV = 0.0
        PS1_AV = 0.0
        PS2_AV = 0.0
        KST_AV = 0.0
        COUNT = 0

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
!       EXW_AV = EXW_AV + SATFAC
        PS1_AV = PS1_AV + (1.0 - PSTRES1)
        PS2_AV = PS2_AV + (1.0 - PSTRES2)
        KST_AV = KST_AV + (1.0 - KSTRES)
        COUNT = COUNT + 1

!       Accumulate senesced matter for surface and soil.
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

            LEAFNO = LN
            VSTAGE = REAL (LEAFNO)

!           GM2KG converts gm/plant to kg/ha
            GM2KG  = PLTPOP * 10.0
            
            TOPWT  = BIOMAS * 10.    !topwt in kg/ha, biomas in g/m2

            WTLF = LFWT * PLTPOP      !leaf, g/m2
            LWAD = LFWT * GM2KG       !leaf, kg/ha
            SWAD = STMWT* GM2KG       !stem, kg/ha
            VWAD = LWAD + SWAD        !veg,  kg/ha

            BWAD = BASLFWT * GM2KG    !basal, kg/ha
            SUGD = SKWT    * GM2KG    !sucker,kg/ha
            RWAD = RTWT    * GM2KG    !roots, kg/ha

            IF (FRUITS < 1.E-6) THEN
!             At stage 5, flower weight becomes fruit and crown
!             Some mass is lost because fruits/m2 < plants/m2
              FLWAD= FLRWT * GM2KG    !flower,kg/ha
              FWAD = 0.0
              CRAD = 0.0
            ELSE
!             Fruit and crown weights are calculated using FRUITS/m2, not PLTPOP/m2
              FWAD = FRTWT * FRUITS * 10. !fruit, kg/ha
              CRAD = CRWNWT* FRUITS * 10. !crown, kg/ha
              FLWAD = 0.0
            ENDIF

            IF (TOPWT .GT. 0.0) THEN
              HI = FWAD / TOPWT
            ELSE
              HI = 0.0
            ENDIF

            IF (GPP > 1.E-6) THEN
              EYEWT = FRTWT / GPP         !eye weight, g/eye
              GPSM  = GPP * FRUITS        !# eyes/m2
              EYWAD = EYEWT * GPSM * 10.  !eye weight, kg/ha  
        ! this makes eye weight exactly equal to fruit weight. Hmmmm.  
            ELSE
              EYEWT = 0.0
              EYWAD = 0.0
            ENDIF        
            
            XLAI   = LAI
            IF (WTLF .GT. 0.0) THEN
               SLA  = LAI * 10000 / WTLF
            ELSE
               SLA = 0.0
            ENDIF

!           SEEDNO = GPSM
!           PODWT  = CRWNWT
!           IF (GPP .GT. 0.0) THEN
!              PODNO = SEEDNO/GPP
!           ELSE
!              PODNO = 0.0
!           ENDIF

            IF ((LFWT+STMWT) .GT. 0.0) THEN
              WTNLF = STOVN * (LFWT  / STOVWT) * PLTPOP
              WTNST = STOVN * (STMWT / (LFWT + STMWT)) * PLTPOP
            ELSE
              WTNLF = 0.0
              WTNST = 0.0
            ENDIF

            IF (LFWT .GT. 0.0) THEN
              PCNL = WTNLF / (LFWT * PLTPOP) * 100.0
            ELSE
              PCNL = 0.0
            ENDIF

            IF (FMOPT /= 'C') THEN   ! VSH
              WRITE (NOUTDG,400) YEAR, DOY, DAS, DAP, VSTAGE, ISTAGE, XLAI,         &
                NINT(TOPWT),  NINT(VWAD), NINT(LWAD), NINT(SWAD), NINT(FLWAD),      &
                NINT(FWAD), NINT(CRAD), NINT(BWAD), NINT(SUGD), NINT(RWAD), HI,     &
                NINT(EYWAD), NINT(GPSM), (1.0-SWFAC), (1.0-TURFAC), (1.0-NSTRES),   &
                PCNL, SLA, (RTDEP/100.), (RLV(I),I=1,10)       

!YEAR DOY   DAS   DAP VSTAGE ISTAGE   XLAI  TOPWT   VWAD   LWAD   SWAD  FLWAD   FWAD   CRAD   BWAD   SUGD   RWAD    HI   EYWAD  GPSM   SWFAC TURFAC NSTRES   PCNL    SLA  RTDEP   RLV(I),I=1,10
!YEAR DOY   DAS   DAP   L#SD   GSTD   LAID   CWAD   VWAD   LWAD   SWAD  FLWAD   FWAD   CRAD   BWAD   SUGD   RWAD   HIAD  EYWAD  EY#AD   WSPD   WSGD   NSTD   LN%D   SLAD   RDPD   RL1D   RL2D   RL3D   RL4D   RL5D   RL6D   RL7D   RL8D   RL9D   RL10
!1989 166     7     0    0.0      8   0.22    595    384    315     68      0      0      0    208      0      0  0.000      0      0  0.000  0.000  0.000   0.00   71.3   0.05   0.00   0.00   0.00   0.00   0.00   0.00   0.00   0.00   0.00   0.00
!1989 167     8     1    0.0      9   0.22    595    384    315     68      0      0      0    208      0      0  0.000      0      0  0.000  0.000  0.000   0.00   71.3   0.05   0.00   0.00   0.00   0.00   0.00   0.00   0.00   0.00   0.00   0.00


  400          FORMAT (1X,I4,1X,I3.3,2I6,1X,F6.1,1X,I6,1X,F6.2,   &
                  10(1X,I6),1X,F6.3,           &    
                  2(1X,I6), 3(1X,F6.3),       &
                  1X,F6.2, 1X,F6.1, F7.2, 10(1X,F6.2))        
                  

!-------------------------------------------------------------------------
!           VSH CSV output corresponding to PlantGro.OUT
            ELSE   ! VSH
              !CALL CsvOut(EXPNAME,CONTROL%RUN, CONTROL%TRTNUM,CONTROL%ROTNUM,  &
              !  CONTROL%REPNO, YEAR, DOY, DAS, DAP, VSTAGE, ISTAGE, XLAI,               &
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

            WTNSD = GRAINN * PLTPOP
            WTNRT = ROOTN * PLTPOP        ! Is this right?
            WTNSH = 0.0
            WTNCAN = (STOVN + GRAINN) * PLTPOP
            WTNVEG  = (WTNLF + WTNST)
            WTNGRN  = (WTNSH + WTNSD)

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
              WRITE (NOUTPN,300) YEAR, DOY, DAS, DAP,                   &
                     (WTNUP*10.0), (WTNCAN*10.0), (WTNVEG*10.0), (WTNLF*10.0), (WTNST*10.0), (WTNRT*10),     &
                     PCNVEG, PCNL, PCNST, PCNRT
            !DATE    DAP  NUPC  CNAD  GNAD  VNAD  LNAD  SNAD  GN%D  VN%D  LN%D  SN%D  SHND  RN%D
  300         FORMAT (1X,I4, 1X,I3.3, 2I6, 6(1X,F6.1), 4(1X,F6.2))

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
!       Save values at maturity for Overview.OUT
        IF (YRDOY == MDATE) THEN
          VWATM = VWAD / 1000.  !Veg wt at maturity, t/ha
          CNAM  = WTNCAN * 10.0 !Canopy N at maturity, kg/ha
          VNAM  = WTNVEG * 10.0 !Veg N at maturity, kg/ha
        ENDIF

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
 200  FORMAT (2(1X,I5),3(1X,F5.1),2(1X,F5.2),1X,I5,4(1X,F5.1),1X,I5)
 2190 FORMAT (A80)
 2196 FORMAT (4(A5,I1))
 2197 FORMAT (10(A5,I1))
 2090 FORMAT (A240)
 2340 FORMAT (A252)

!***********************************************************************
      RETURN
      END SUBROUTINE Aloha_OpGrow
!======================================================================
