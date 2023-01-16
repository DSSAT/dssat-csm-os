C=======================================================================
C  OpFloodN, Subroutine
C
C  Generates output for simulated data
C=======================================================================

      SUBROUTINE OpFloodN (CONTROL, ISWITCH, 
     &    ALGACT, ALI, AMLOSS, BD1, EF, FLDH3C, FLDH4, 
     &    FLDH4C, FLDN3C, FLDU, FLOOD, FNI, FPH, FTI, 
     &    FUHYDR, OXRNTRF, YRDOY)

      USE ModuleDefs
      IMPLICIT  NONE
      EXTERNAL GETLUN, HEADER, YR_DOY
      SAVE

      CHARACTER*1  IDETN, RNMODE, ISWNIT
      CHARACTER*116 FLOHEAD(4)
      INTEGER YRDOY, DAS, DOY, DYNAMIC, NOUTDF, ERRNUM, RUN, YEAR
      INTEGER FROP, REPNO
      REAL FNI,ALI,FTI,ALGACT,
     &         FPH,FLDU,FLDH4,FLDH4C,FLDH3C,FLDN3C,FUHYDR,AMLOSS,
     &         OXRNTRF,EF,BD1,FLOOD

      LOGICAL FEXIST, FIRST

!-----------------------------------------------------------------------
      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH

      IDETN   = ISWITCH % IDETN
      ISWNIT  = ISWITCH % ISWNIT

      IF (IDETN .EQ. 'N' .OR. ISWNIT .EQ. 'N') RETURN

      DAS     = CONTROL % DAS
      DYNAMIC = CONTROL % DYNAMIC
      RUN     = CONTROL % RUN
      RNMODE  = CONTROL % RNMODE
      REPNO   = CONTROL % REPNO
      FROP    = CONTROL % FROP

!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN

       FLOHEAD(1) = 
     &'! YR       Days  <--  Floodwater Indices -->   Fl' //
     &'oodwater    Floodwater Conc   Urea   NH3 Nitrf Flood Surfc Flood'

       FLOHEAD(2) = 
     &'!  and     After Nitr Light  Temp Algae   pH   Ur' //
     &'ea   Nh4    Nh4   Nh3    No3  Hydr Volat  Rate Evapn   BD   Dept'

       FLOHEAD(3) = 
     &'!     DOY   Sim  <------ 0 to 1 ------>   pH   <k' //
     &'g N/ha->   <-- mg N / L --->  <--kg N/ha /d-->   mm   g/cc   mm '

       FLOHEAD(4) = 
     &'@YEAR DOY   DAS  FLNI  FALI  FLTI  FALG  FLPH  FL' //
     &'UR  FL4N   FL4C  FL3C   FL3N  FUHY  AMLS  OXRN  EFAD  FLBD  FLOD'

      CALL GETLUN('FLDN', NOUTDF)

!     Initialize daily growth output file
        INQUIRE (FILE = "FloodN.OUT", EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT = NOUTDF, FILE = "FloodN.OUT", STATUS = 'OLD',
     &      IOSTAT = ERRNUM, POSITION = 'APPEND')
          FIRST = .FALSE.  
        ELSE
          OPEN (UNIT = NOUTDF, FILE = "FloodN.OUT", STATUS = 'NEW',
     &      IOSTAT = ERRNUM)
          WRITE(NOUTDF,'("*FLOOD N OUTPUT FILE")')
          FIRST = .TRUE.  
        ENDIF

        !Write headers
        IF (RNMODE .NE. 'Q' .OR. RUN .EQ. 1 .OR. FIRST) THEN

          !For first run of a sequenced run, use replicate
          ! number instead of run number in header.
          IF (RNMODE .EQ. 'Q') THEN
            CALL HEADER(SEASINIT, NOUTDF, REPNO)
          ELSE
            CALL HEADER(SEASINIT, NOUTDF, RUN)
          ENDIF

          WRITE (NOUTDF,2440) FLOHEAD(1)
          WRITE (NOUTDF,2440) FLOHEAD(2)
          WRITE (NOUTDF,2440) FLOHEAD(3)
          WRITE (NOUTDF,2440) FLOHEAD(4)
 2440     FORMAT (A116)
        ENDIF

!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT) THEN

      IF (MOD(DAS, FROP) .EQ. 0 .OR. DAS == 1) THEN
        CALL YR_DOY(YRDOY, YEAR, DOY) 

        WRITE (NOUTDF,400) YEAR, DOY, DAS, FNI,ALI,FTI,ALGACT,
     &         FPH,FLDU,FLDH4,FLDH4C,FLDH3C,FLDN3C,FUHYDR,AMLOSS,
     &         OXRNTRF,EF,BD1,FLOOD
 400    FORMAT (1X,I4,1X,I3.3,1X,I5,4(1X,F5.2),1X,F5.2,2(1X,F5.1),
     &         F7.2,1X,F5.2,1X,F6.2,1X,F5.3,1X,F5.2,1(1X,F5.3),
     &         2(1X,F5.2),1X,F5.1)
      ENDIF

!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASEND) THEN
        CLOSE (NOUTDF)

!***********************************************************************
      ENDIF
      RETURN
      END SUBROUTINE OpFloodN

