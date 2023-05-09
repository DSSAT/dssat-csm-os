c     Climate change study summary output
c     ----------------------------------------
c     MJ & SK, Jun 2015.  Work done for SASRI project 08RE14.
c     This module generates a single line of output per
c     season, relevant to climate change studies,
c     specifically the requirements of 08RE14.
c     ---------------------------------------
c     Variables to output:
c      - Aerial dry mass (t/ha)
c      - Stalk dry mass (t/ha)
c      - Sucrose mass (t/ha)
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!     FO 05-09-2023 Commented out CMDMD and RDMD because of need to
!                   remove output files.
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE SC_CCOUT(CONTROL,
     &  BADMD, SMDMD, SUCMD, IRRAMT, RAIN,   ! Canegro inputs
     &  SWDF1, TMIN, LGDMD, LDDMD, SMFMD,
     &  PAR, Fi, EP, ES)
     
c     ***************************************************************      
c     Instruct compiler to use module definitions:
c     DSSAT
      USE ModuleDefs
c     Canegro
      USE CNG_ModuleDefs      

      IMPLICIT NONE
      EXTERNAL GETLUN
      SAVE

c     Control variable:
      TYPE (ControlType), INTENT(IN) :: CONTROL        

c     Bio-physical variables to output in summary file
c     CMDMD is cellulose DM.
c     LGDMD, LDDMD are green tops DM and trash DM
c     RDMD is root dry mass
      REAL, INTENT(IN) :: BADMD, SMDMD, SUCMD, IRRAMT, RAIN,
     &  SWDF1, TMIN, LGDMD, LDDMD, PAR, Fi, EP, ES,
     &  SMFMD
!      REAL, INTENT(IN) :: CMDMD, RDMD

c     Local state variables
c     :::::::::::::::::::::
c     Total seasonal cumulative irrigation (mm)
      REAL IRR_TOT
c     Total seasonal cumulative rainfall (mm)
      REAL RAIN_TOT
c     Severe stress days (count of days with SWDF1 < 0.05)
      INTEGER STRESS_DAYS
c     Frost days (count of days with TMIN < -1.5 °C)
      INTEGER FROST_DAYS
c     Fibre mass calcd here as STKDM - SUCDM
      REAL FIBDM
c     Total SWDF1 (for calculating average)
      REAL SWDF1_TOT
c     Cumulative PAR interception
      REAL IPARC
c     Cumulative transpiration + evaporation
      REAL EPC, ETC
c     Recalculate Bio-Physical variables to output
      REAL cBADMD, cSMDMD, cSMFMD
c     Average Fi
      REAL AvgFi, FiTOT
c     Days to 80% canopy cover
      INTEGER DAYS80Fi

      INTEGER COUNTER

c     The file unit number for the summary output file.
!      INTEGER CCOUT
c     Filename
!      CHARACTER*20 OFILE
c     Does the file exist, is this the first run?:
!      LOGICAL FILE_EXISTS !, FIRST
c     Error status:
!      INTEGER ERRNUM

c     ===============================================================
c     RUN INITIALISATION
c     ===============================================================
c     Code to be called at the beginning of the simulation run
c     ::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      IF (Control%DYNAMIC .EQ. RUNINIT) THEN
c     Nothing for now...

c     ===============================================================
c     RUN INITIALISATION
c     ===============================================================
c     Code to be called at the beginning of each simulated crop
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      ELSEIF (Control%DYNAMIC .EQ. SEASINIT) THEN
c     Open the output file
c     Write a file header

c     Open growth aspects output file:
c     ::::::::::::::::::::::::::::::::
c       Set file name:
!        OFILE = 'CCSUMMRY.OUT'
!
!c       Get file unit number:
!        CALL GETLUN('CCSUMMRY', CCOUT)
!
!c       Check that the file exists:
!        FILE_EXISTS = .FALSE.
!        INQUIRE(FILE=OFILE, EXIST=FILE_EXISTS)
!
!c       Open the file
!        IF (FILE_EXISTS) THEN
!c         In append mode if the file already exists
!          OPEN (UNIT=CCOUT, FILE=OFILE, STATUS='OLD',
!     &      IOSTAT=ERRNUM, POSITION='APPEND')
!        ELSE
!c         A new file if not existing
!          OPEN (UNIT=CCOUT, FILE=OFILE, STATUS='NEW',
!     &      IOSTAT = ERRNUM)
!          WRITE(CCOUT,'("*CLIMATE CHANGE SUMMARY OUTPUT FILE")')
!          WRITE(CCOUT,'("! IRRC - Cumulative irrigation (mm)")')
!          WRITE(CCOUT,'("! PRCM - Cumulative rainfall (mm)")')
!          WRITE(CCOUT,'("! FDAYS - # frost days (TMIN < 1.5°C)")')
!          WRITE(CCOUT,'("! SDAYS - # stress days (SWDF1 < 0.05)")')
!          WRITE(CCOUT,'("! D80Fi - # days to 80% canopy cover")')
!          WRITE(CCOUT,'("! SMFMH - Fresh cane yield WM (t/ha)")')
!          WRITE(CCOUT,'("! BADMH - Aerial DM (t/ha)")')
!          WRITE(CCOUT,'("! SMDMH - Stalk DM (t/ha)")')
!          WRITE(CCOUT,'("! SUCMH - Sucrose mass (t/ha)")')
!          WRITE(CCOUT,'("! FBDMH - Total fibre DM (t/ha)")')
!          WRITE(CCOUT,'("! CMDMH - Cellulosic DM (t/ha)")')
!          WRITE(CCOUT,'("! LGDMH - Green tops DM (t/ha)")')
!          WRITE(CCOUT,'("! LDDMH - Trash DM (t/ha)")')
!          WRITE(CCOUT,'("! RDMH  - Roots DM (t/ha)")')
!          WRITE(CCOUT,'("! WSPH  - avg. SWDF1 water stress")')
!          WRITE(CCOUT,'("! AvgFi - avg. frac. int. of PAR")')
!          WRITE(CCOUT,'("! IPARC - Intercepted PAR (MJ/m2)")')
!          WRITE(CCOUT,'("! EPCM  - Cumulative transpiration (mm)")')
!          WRITE(CCOUT,'("! ETCM  - Cumulative evapo-transp. (mm)")')
!          WRITE(CCOUT,'("! BADMRUE - Aerial dry mass [PA]RUE (g/MJ)")')
!          WRITE(CCOUT,'("! BADMWUE - Aerl. DM WUE (t/100 mm ET)")')
!          WRITE(CCOUT,'("! SMDMWUE - Stlk. DM WUE (t/100 mm ET)")')
!          WRITE(CCOUT,'("! SMFMWUE - St Fresh Mass WUE (t/100 mm ET)")')
!          WRITE(CCOUT,'("")')
!c         Write column headings
!          WRITE(CCOUT, '(A8, 1H , A4, 1H , A5, 1H , 23(A8, 1H ))') 
!     &      '@EXPCODE', 'TRNO', 'RUNNO', 'IRRC', 'PRCM','FDAYS',
!     &      'SDAYS','D80Fi','SMFMH','BADMH', 'SMDMH', 'SUCMH', 'FBDMH', 
!     &      'CMDMH', 'LGDMH', 'LDDMH', 'RDMH', 'WSPH', 'AVGFI','IPARC',
!     &       'EPCM','ETCM', 'BADMRUE', 'BADMWUE', 'SMDMWUE', 'SMFMWUE'
!        ENDIF
!
c       Initialised total irrigation to 0.0
        IRR_TOT = 0.0
        RAIN_TOT = 0.0
        FROST_DAYS = 0
        STRESS_DAYS = 0
        FIBDM = 0.0
        SWDF1_TOT = 0.0
        COUNTER = 0
        IPARC = 0.0
        EPC = 0.0
        ETC = 0.0
        FiTOT = 0.0
        DAYS80Fi = -99


c     ===============================================================
c     INTEGRATION
c     ===============================================================
      ELSEIF (Control%DYNAMIC .EQ. INTEGR) THEN
c       Any calculations necessary.
c       Total irrigation
        IRR_TOT = IRR_TOT + IRRAMT
c       Total rainfall
        RAIN_TOT = RAIN_TOT + RAIN
c       Severe stress days (count of days with SWDF1 < 0.05)
        IF (SWDF1 .LT. 0.05) THEN 
          STRESS_DAYS = STRESS_DAYS + 1
        ENDIF
c       Frost days (count of days with TMIN < -1.5 °C)
        IF (TMIN .LT. -1.5) THEN 
          FROST_DAYS = FROST_DAYS + 1
        ENDIF

c       Accumulated SWDF1
        SWDF1_TOT = SWDF1_TOT + SWDF1
c       Counter
        COUNTER = COUNTER + 1

c       Cumulative intercepted radiation 
        IPARC = IPARC + (PAR*Fi)

c       Cumulative transpiration and evap
        EPC = EPC + EP
        ETC = ETC + EP + ES

c       Average Fi
        FiTOT = FiTOT + Fi
c       Days to 80% canopy
        IF ((Fi .GT. 0.80) .AND. (DAYS80Fi .LT. 0.0)) THEN
          DAYS80Fi = COUNTER
        ENDIF 

c     ===============================================================
c     END OF SEASON
c     ===============================================================
      ELSEIF (Control%DYNAMIC .EQ. SEASEND) THEN
c       Total fibre DM is stalk mass minus sucrose mass, plus
c         green leaf DM and trash DM
        FIBDM = SMDMD - SUCMD + LGDMD + LDDMD
c       Averaage Fi
        AvgFi = FiTOT / (COUNTER * 1.0)
c       Recalculate Bio-physical variables to output
        IF(ETC .GT. 0.0) THEN
          cBADMD = BADMD / ETC*100.0
          cSMDMD = SMDMD / ETC*100.0
          cSMFMD = SMFMD / ETC*100.0
        ELSE
          cBADMD = -99.0          
          cSMDMD = -99.0        
          cSMFMD = -99.0          
        ENDIF
                        
c       Write summary data to file.
!          WRITE(CCOUT, '(A8, 1H , I4, 1H , I5, 1H , ' //
!          ! Irrigation and rainfall
!     &      '2(F8.0, 1H ), '//
!          ! Frost and stress days, days to 80% Fi
!     &      '3(I8, 1H ), ' // 
!          ! Biomass variables
!     &      '9(F8.1, 1H ), ' // 
!          ! SDWF1, AvgFi, IPARC, EPC, ETC
!     &      '2(F8.2, 1H ), 3(F8.0, 1H ),' // 
!          ! RUE and WUE
!     &      '4(F8.3, 1H ))') 
!     &      Control%FILEX, Control%TRTNUM, Control%RUN, 
!     &      IRR_TOT, RAIN_TOT, 
!     &      FROST_DAYS, STRESS_DAYS, DAYS80Fi, SMFMD,
!     &      BADMD, SMDMD, SUCMD, FIBDM, CMDMD, LGDMD, LDDMD, RDMD,
!     &      SWDF1_TOT / (1.0*COUNTER), AvgFi, IPARC, EPC, ETC,
!     &      BADMD*100.0 / IPARC, cBADMD, cSMDMD, cSMFMD
!             
!c       Close the output file
!        CLOSE(UNIT=CCOUT)
!
c     End of DYNAMIC conditional statement
      ENDIF
      
      END  
c     ===============================================================   