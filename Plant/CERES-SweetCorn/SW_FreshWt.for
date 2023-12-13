!======================================================================
!  SW_FreshWt, Subroutine
!
!  Sweet corn fresh weight 
!----------------------------------------------------------------------
!  Revision history
!  04/21/2008 CHP Moved this code from SW_GROSUB
!  06/29/2023  FO Updated FreshWT.OUT header for SweetCorn   
!----------------------------------------------------------------------
!
!  Called : SW_GROSUB
!----------------------------------------------------------------------

      SUBROUTINE SW_FreshWt (ISWITCH, ISWFWT, 
     &    CUMDTTEG, EARS, EARWT, ISTAGE, MDATE, SLPF, 
     &    STGDOY, SUMDTT, SWFAC, NSTRES, YRPLT)

      USE ModuleDefs
      USE ModuleData
      IMPLICIT  NONE
      EXTERNAL GETLUN, HEADER, TIMDIF, YR_DOY
      SAVE
!----------------------------------------------------------------------
!                         Variable Declaration
!----------------------------------------------------------------------

      REAL        EARS1_2
      REAL        FWYLD1_2
      REAL        CUMDTTEG      
      INTEGER     DOY         
      INTEGER     DYNAMIC     
      REAL        EARDMC
      REAL        EARFWT
      REAL        EARMKT
      REAL        EARS        
      REAL        EARWT       
      CHARACTER*6 ERRKEY 
      CHARACTER*11, PARAMETER :: FWFile = "FreshWt.OUT"
      PARAMETER   (ERRKEY='MZ_GRO')   
      REAL        EARSFCY
      REAL        FWYLDFCY
      REAL        FWYIELD
      INTEGER     ISTAGE  
      CHARACTER*1 ISWFWT  
      INTEGER     MDATE
      REAL        MKTFWYLD
      INTEGER     NOUTPF
      REAL        NSTRES      
      REAL        SLPF
      INTEGER     STGDOY(20) 
      REAL        SUMDTT      
      REAL        SWFAC       
      REAL        XFWYLDFCY
      INTEGER     YRDOY    

      TYPE (SwitchType)  ISWITCH
      LOGICAL FEXIST
      INTEGER DAP, DAS, ERRNUM, FROP, TIMDIF, YEAR, YRPLT

      TYPE (ControlType) CONTROL
  
      IF (ISWFWT == 'N' .OR. ISWITCH % IDETG == 'N') RETURN

      CALL GET (CONTROL)
      DYNAMIC = CONTROL%DYNAMIC

!----------------------------------------------------------------------
!                     DYNAMIC = SEASINIT
!----------------------------------------------------------------------

      IF(DYNAMIC == SEASINIT) THEN

          CALL GETLUN(FWFile,  NOUTPF)

          INQUIRE (FILE = FWFILE, EXIST = FEXIST)
          IF (FEXIST) THEN
            OPEN (UNIT = NOUTPF, FILE = FWFILE, STATUS='OLD',
     &        IOSTAT=ERRNUM, POSITION='APPEND')
          ELSE
            OPEN (UNIT = NOUTPF, FILE = FWFILE, STATUS='NEW',
     &        IOSTAT = ERRNUM)
              WRITE(NOUTPF,'("*Fresh Weight Output File")')
          ENDIF

          CALL HEADER(SEASINIT, NOUTPF, CONTROL%RUN)
                   
          WRITE (NOUTPF, 1000)
          WRITE (NOUTPF, 1001)

 1000 FORMAT ('!',22X,'(0-1)  g/ear g/ear  <---- kg/ha ---> < ear/ha >')
 1001 FORMAT ('@YEAR DOY   DAS   DAP',
     &               '  PDMCD  AFPWD ADPWD    TOFPW    MFWAD    NFWAD')

          EARS1_2= 0.0
          FWYLD1_2=0.0
          EARDMC = 0.0
          EARFWT = 0.0
          EARMKT = 0.0
          EARSFCY= 0.0
          FWYLDFCY=0.0
          FWYIELD= 0.0
          MKTFWYLD=0.0
          XFWYLDFCY = 0.0

!-----------------------------------------------------------------------
!                     DYNAMIC = INTEGR
!-----------------------------------------------------------------------
      ELSEIF(DYNAMIC.EQ.INTEGR) THEN   
   
          IF (ISTAGE .EQ. 3) THEN

! JIL 04/03/2006 Calculate ear fresh weight
              IF (CUMDTTEG .GT. 0.001) THEN
                 EARDMC = 0.05 + 0.0002 * CUMDTTEG ! Fraction (0.05-0.1)
                 EARFWT = EARWT / EARDMC           ! g/ear
              ENDIF

!      --------------------------------------------------------------------
!         ISTAGE = 4 (Silking to beginning of effective grain filling period)
!      --------------------------------------------------------------------

          ELSEIF (ISTAGE .EQ. 4) THEN

! JIL 04/03/2006 Calculate ear fresh weight
              EARDMC = 0.1 + 0.0002*SUMDTT
              EARFWT = EARWT / EARDMC                  ! g/ear

          !-------------------------------------------------------------
          !   ISTAGE = 5 Effective Grain Filling Period
          !-------------------------------------------------------------

          ELSEIF (ISTAGE .EQ. 5) THEN

! JIL 04/03/2006 Calculate ear fresh weight
              EARDMC = 0.1 + 0.0002*SUMDTT
              EARFWT = EARWT / EARDMC                  ! g/ear


          ENDIF    


!----------------------------------------------------------------------
!   The following code is executed each day regardless of ISTAGE value
!----------------------------------------------------------------------


          !------------------------------------------------------------
          !             MARKET FRESH WEIGHT AND EAR QUALITY
          !------------------------------------------------------------

!     04/04/2006 JIL, Calculating Market fresh weight and ear numbers

          FWYIELD  = EARFWT * EARS * 10.0  ! Total FW yield (kg/ha)
          MKTFWYLD = AMAX1(0.0,(0.9872*FWYIELD-1453.8)*SLPF
     &               *AMIN1(SWFAC,NSTRES)) ! Marketable FW yield (kg/ha)

!            Max fraction of Fancy FW in Marketable FW
          IF (MKTFWYLD .GT. 0.0) THEN
             XFWYLDFCY = AMAX1(0.0,0.919 * (1.0-EXP(-0.00012*MKTFWYLD)))   
             FWYLDFCY = XFWYLDFCY * MKTFWYLD
             EARSFCY  = 4.053 * FWYLDFCY
          ELSE
             FWYLDFCY = 0.0
             EARSFCY  = 0.0
          ENDIF

          FWYLD1_2 = MKTFWYLD - FWYLDFCY ! US1+US2 ears FW yield (kg/ha)
          EARS1_2  = 7.0375 * FWYLD1_2   ! US1+US2 ear number (ear/ha)
          EARMKT = EARSFCY + EARS1_2     ! Marketable ear number(ear/ha)

!----------------------------------------------------------------------
!                     DYNAMIC = OUTPUT
!----------------------------------------------------------------------
      
      ELSEIF(DYNAMIC.EQ.OUTPUT) THEN


!     04/04/2006 JIL, Output file for sweet corn fresh mass simulation

      IF (ISTAGE .GE. 3) THEN

        FROP = CONTROL % FROP
        DAS  = CONTROL % DAS
        YRDOY= CONTROL % YRDOY
        DAP  = TIMDIF(YRPLT, YRDOY)

        IF ((MOD(DAS,FROP) .EQ. 0)    !Daily output every FROP days,
     &      .OR. (YRDOY .EQ. STGDOY(3))     !on tassel init date, and
     &      .OR. (YRDOY .EQ. MDATE)) THEN   !at harvest maturity 

          CALL YR_DOY(YRDOY, YEAR, DOY)

          WRITE (NOUTPF,1100) YEAR, DOY, DAS, DAP, EARDMC, EARFWT, 
     &            EARWT, FWYIELD, MKTFWYLD, EARMKT
 1100 FORMAT (1X,I4,1X,I3.3,2(1X,I5),F7.3,F7.1,F6.1,3(F9.1))

        ENDIF
      ENDIF


!----------------------------------------------------------------------
!                     DYNAMIC = SEASEND
!----------------------------------------------------------------------

      ELSEIF(DYNAMIC.EQ.SEASEND) THEN

      CLOSE (NOUTPF)
      ENDIF       !Endif for DYNAMIC LOOP

      RETURN


      END SUBROUTINE SW_FreshWt


