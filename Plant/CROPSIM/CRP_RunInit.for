!***********************************************************************
! This is the code from the section (DYNAMIC == RUNINIT) 
! lines 1781 - 2158 of the original CSCRP code.
!***********************************************************************

      SUBROUTINE CRP_RunInit (CN, DOY, FILEIOIN, FROP, IDETL, ISWNIT,
     &     ON, RN, RNMODE, RUN, SN, TN, YEAR)
      
      USE ModuleDefs
      USE CRP_First_Trans_m 
      IMPLICIT NONE
      EXTERNAL UCASE, TVILENT, GETLUN, TL10FROMI, XREADC, XREADT
      SAVE

      INTEGER CN, DOY, FROP, ON, RN, RUN, SN, TN
      INTEGER NLAYR, DLAYR(NL)
      INTEGER YEAR
      !INTEGER VERSIONCSCRP
      INTEGER TVILENT          ! Integer function call

      CHARACTER(LEN=1)   IDETL, ISWNIT, RNMODE !, ISWWAT     
      CHARACTER (LEN=250) FILEIOIN  
      CHARACTER(LEN=10)  TL10FROMI      
        
       IF (RUNCRP.LE.0) THEN          ! First time through

          MODNAME(1:8) = 'CSCRP048'
          !VERSIONCSCRP = 010115       ! TF 010115 Changed from VERSION, 
          !                            !  conflict with ModuleDefs 
          GENFLCHK(3:15) = 'CRP048.20200721'
          ! Control flags/switches
          CFLPDATE = 'P'      ! P=at planting;I=at first irrigation;
                              ! E=relative to emergence
          CFLNOUTPUTS = 'N'   ! Y=special N outputs written
          ISWWATCROP = 'Y'    ! E=no water stress effect on crop growth

!-----------------------------------------------------------------------
!         Set parameters (Most should be placed in input files!)
!-----------------------------------------------------------------------

          ! Physical constants
          MJPERE = 220.0*1.0E-3  ! MJ per Einstein at 540 nm
          PARMJFAC = 0.5         ! PAR in SRAD (fr)

          ! Model standard parameters
          STDAY = 20.0      ! TT in standard day
          STEPNUM = 1       ! Step number per day set to 1

!-----------------------------------------------------------------------
!         Determine base of soil layers
!-----------------------------------------------------------------------

          BLAYR = 0.0
          BLAYR(1) = DLAYR(1)
          DO L = 2,NLAYR
            BLAYR(L) = BLAYR(L-1) + DLAYR(L)
          ENDDO  
          
!-----------------------------------------------------------------------
!         Read command line arguments for model name and path (Cropsim)
!-----------------------------------------------------------------------

          arg = ' '
          tvi2 = 0
          tvi3 = 0
          tvi4 = 0
          ! Following not good for all platforms.Changed for portability
          ! CALL GETARG (0,arg,arglen)
          CALL GETARG(0,arg)
          arglen = len_trim(arg)
          DO tvi1 = 1,arglen
            IF (arg(tvi1:tvi1).EQ.Slash) tvi2=tvi1
            IF (arg(tvi1:tvi1).EQ.'.') tvi3=tvi1
            IF (arg(tvi1:tvi1).EQ.' ' .AND. tvi4.EQ.0) tvi4=tvi1
          ENDDO
          IF (TVI3.EQ.0 .AND. TVI4.GT.0) THEN
            tvi3 = tvi4
          ELSEIF (TVI3.EQ.0 .AND. TVI4.EQ.0) THEN
            tvi3 = arglen+1
          ENDIF
          MODEL = ARG(TVI2+1:TVI3-1)
          CALL UCASE(MODEL)

!-----------------------------------------------------------------------
!         Set configuration file name (Cropsim)
!-----------------------------------------------------------------------

          IF (FILEIOT(1:2).NE.'DS') THEN
            CFGDFILE = ' '
            IF (TVI2.GT.1) THEN
              CFGDFILE = ARG(1:TVI2)//'CROPSIM.CFG'
            ELSE
              CFGDFILE(1:12) = 'CROPSIM.CFG '
            ENDIF
          ENDIF

!-----------------------------------------------------------------------
!         Determine input file type (Dssat or X-file) and check if there
!-----------------------------------------------------------------------

          TVI1 = TVILENT(FILEIOIN)
          IF (FILEIOIN(TVI1-2:TVI1).EQ.'INP') THEN
            FILEIOIN(TVI1:TVI1) = 'H'
            FILEIOT = 'DS4'
          ELSE
            FILEIOT = 'XFL'
          ENDIF
          FILEIO = ' '
          FILEIO(1:TVI1) = FILEIOIN(1:TVI1)
          INQUIRE (FILE = FILEIO,EXIST = FFLAG)
          IF (.NOT.(FFLAG)) THEN
            CALL GETLUN ('ERROR.OUT',FNUMERR)
            OPEN (UNIT = FNUMERR,FILE = 'ERROR.OUT')
            WRITE(fnumerr,*) ' '
            WRITE(fnumerr,*) 'Input file not found!     '
            WRITE(fnumerr,*) 'File sought was:          '
            WRITE(fnumerr,*) Fileio(1:78)
            WRITE(fnumerr,*) 'Please check'
            WRITE(*,*) ' Input file not found!     '
            WRITE(*,*) 'File sought was:          '
            WRITE(*,*) Fileio(1:78)
            WRITE(*,*) ' Program will have to stop'
            CLOSE (FNUMERR)
            STOP ' '
          ENDIF

!-----------------------------------------------------------------------
!         Create output file extensions (For different components)
!-----------------------------------------------------------------------

          CNCHAR = ' '
          CNCHAR2 = '  '
          IF (CN.EQ.1.OR.CN.EQ.0) THEN
            OUT = 'OUT'
            CNCHAR2= '1 '
          ELSE
            CNCHAR = TL10FROMI(CN)
            OUT = 'OU'//CNCHAR(1:1)
            CNCHAR2(1:1) = CNCHAR(1:1)
          ENDIF

!-----------------------------------------------------------------------
!         Set output flags to agree with run modes and control switches
!-----------------------------------------------------------------------

          IF (FILEIOT.EQ.'XFL') THEN
            IF (RNMODE.EQ.'I'.OR.RNMODE.EQ.'E'.OR.RNMODE.EQ.'A') THEN
              IDETD = 'M'
            ELSEIF (RNMODE.EQ.'B'.OR.RNMODE.EQ.'N'.OR.RNMODE.EQ.'Q'
     &               .OR.RNMODE.EQ.'Y')THEN
              IDETD = 'S'
            ENDIF  
          ELSE
            IDETD = 'N'
          ENDIF
          FROPADJ = FROP
          IF (RNMODE.EQ.'T') FROPADJ = 1
          IF (IDETL.EQ.'D'.OR.IDETL.EQ.'A') FROPADJ = 1

!-----------------------------------------------------------------------
!         Set file names and determine file unit numbers
!-----------------------------------------------------------------------

          ! DATA FILES
          CALL GETLUN ('FILET',FNUMT)

          ! WORK,ERROR,AND TEMPORARY FILES
!          CALL GETLUN ('WORK.OUT',FNUMWRK)
          CALL GETLUN ('ERROR.OUT',FNUMERR)
          CALL GETLUN ('FNAMETMP',FNUMTMP)

          ! IDETG FILES
          ! Check if need to change file names
          CALL XREADC (FILEIO,TN,RN,SN,ON,CN,'FNAME',fname)
          IF (FNAME.EQ.'Y') THEN   ! File name change required.
            CALL XREADT (FILEIO,TN,RN,SN,ON,CN,'EXPER',excode)
            !NB. Renaming of Plantgro and Plantn handled by CSM
            OUTPG = 'PlantGro.'//OUT
            OUTPN = 'PlantN.'//OUT
            OUTPG2 = EXCODE(1:8)//'.OP2'
            OUTPGF = EXCODE(1:8)//'.OPF'
          ELSE  
            OUTPG = 'PlantGro.'//OUT
            OUTPN = 'PlantN.'//OUT
            OUTPG2 = 'PlantGr2.'//OUT
            OUTPGF = 'PlantGrf.'//OUT
          ENDIF
          CALL GETLUN (OUTPG,NOUTPG)
          CALL GETLUN (OUTPG2,NOUTPG2)
          CALL GETLUN (OUTPGF,NOUTPGF)
          CALL GETLUN (OUTPN,NOUTPN)

          ! IDETO FILES
          ! NB. Renaming of Overview and Evaluate handled by CSM
          ! TF - Updated OVERVIEW.OUT name to avoid issues
          ! with case sensitive systems (07/27/2021) 
          FNAMEOV = 'OVERVIEW.'//out
          FNAMEEVAL = 'Evaluate.'//out
          FNAMEMEAS = 'Measured.'//out
          CALL GETLUN (FNAMEEVAL,fnumeval)
          CALL GETLUN (FNAMEOV,fnumov)
          CALL GETLUN (FNAMEMEAS,fnummeas)

          ! IDETS FILES
          FNAMEPSUM(1:12)   = 'Plantsum.'//OUT
          CALL GETLUN (FNAMEPSUM,  fnumpsum)

          ! RESPONSE FILES
          FNAMEPRES(1:12)   = 'Plantres.'//out
          FNAMEPREM(1:12) = 'Plantrem.'//out
          CALL GETLUN (FNAMEPRES,fnumpres)
          CALL GETLUN (FNAMEPREM,fnumprem)

          ! LEAVES FILES
          FNAMELEAVES(1:10) = 'Leaves.'//OUT
          CALL GETLUN (FNAMELEAVES,fnumlvs)

          ! PHENOL FILES
          FNAMEPHASES(1:10) = 'Phases.'//out
          FNAMEPHENOLS(1:11) = 'Phenols.'//out
          FNAMEPHENOLM(1:11) = 'Phenolm.'//out
          CALL GETLUN (FNAMEPHASES,fnumpha)
          CALL GETLUN (FNAMEPHENOLS,fnumphes)
          CALL GETLUN (FNAMEPHENOLM,fnumphem)

          ! ERROR FILES
          FNAMEERA(1:12) = 'Plantera.'//out
          FNAMEERT(1:12) = 'Plantert.'//out
          CALL GETLUN (FNAMEERT,fnumert)
          CALL GETLUN (FNAMEERA,fnumera)

          ! LAH March 2010 Metadata taken out. Check later if needed
          ! METADATA FILES
          !CALL GETLUN ('META',FNUMMETA)

!-----------------------------------------------------------------------
!         Open and write main headers to output files
!-----------------------------------------------------------------------

          IF (RUN.EQ.1) THEN
            ! IDETG FILES
            OPEN (UNIT = NOUTPG, FILE = OUTPG)
            WRITE (NOUTPG,'(A27)')
     &      '$GROWTH ASPECTS OUTPUT FILE'
            CLOSE (NOUTPG)
            OPEN (UNIT = NOUTPG2, FILE = OUTPG2)
            WRITE (NOUTPG2,'(A38)')
     &      '$GROWTH ASPECTS SECONDARY OUTPUTS FILE'
            CLOSE (NOUTPG2)
            OPEN (UNIT = NOUTPGF, FILE = OUTPGF)
            WRITE (NOUTPGF,'(A27)')
     &      '$GROWTH FACTOR OUTPUTS FILE'
            CLOSE (NOUTPGF)
            IF (ISWNIT.NE.'N') THEN
              OPEN (UNIT = NOUTPN, FILE = OUTPN)
              WRITE (NOUTPN,'(A35)')
     &        '$PLANT NITROGEN ASPECTS OUTPUT FILE'
              CLOSE (NOUTPN)
            ELSE  
              INQUIRE (FILE = OUTPN,EXIST = FEXIST)
              IF (FEXIST) THEN
                OPEN (UNIT = NOUTPN, FILE = OUTPN, STATUS='UNKNOWN',
     &          POSITION = 'APPEND')
                CLOSE (UNIT=NOUTPN, STATUS = 'DELETE')
              ENDIF  
            ENDIF  

            ! IDETO FILES
            OPEN (UNIT = FNUMOV, FILE = FNAMEOV)
            WRITE(FNUMOV,'(A20)') '$SIMULATION_OVERVIEW'
            CLOSE(FNUMOV)
            OPEN (UNIT = FNUMEVAL, FILE = FNAMEEVAL)
            WRITE(FNUMEVAL,'(A17)') '$PLANT_EVALUATION'
            CLOSE(FNUMEVAL)
            OPEN (UNIT = FNUMMEAS,FILE = FNAMEMEAS)
            WRITE (FNUMMEAS,'(A22)') '$TIME_COURSE(MEASURED)'
            CLOSE(FNUMMEAS)
            
            ! IDETS FILES
            OPEN (UNIT = FNUMPSUM,FILE = FNAMEPSUM)
            WRITE (FNUMPSUM,'(A27)') '$PLANT_SUMMARY             '
            CLOSE(FNUMPSUM)
            
             OPEN(UNIT=FNUMLVS,FILE=FNAMELEAVES)
             WRITE (FNUMLVS,'(A11)') '$LEAF_SIZES'
             CLOSE(FNUMLVS)
            
             OPEN(UNIT=FNUMPHA,FILE=FNAMEPHASES)
             WRITE (FNUMPHA,'(A17)') '$PHASE_CONDITIONS'
             CLOSE(FNUMPHA)
             OPEN(UNIT=FNUMPHES,FILE=FNAMEPHENOLS)
             WRITE (FNUMPHES,'(A27)') '$PHENOLOGY_DATES(SIMULATED)'
             CLOSE(FNUMPHES)
             OPEN(UNIT=FNUMPHEM,FILE=FNAMEPHENOLM)
             WRITE (FNUMPHEM,'(A27)') '$PHENOLOGY_DATES(MEASURED) '
             CLOSE(FNUMPHEM)
            
             OPEN (UNIT = FNUMPRES,FILE = FNAMEPRES,STATUS = 'UNKNOWN')
             WRITE (FNUMPRES,'(A27)') '$PLANT_RESPONSES(SIMULATED)'
             CLOSE(FNUMPRES)
             OPEN (UNIT = FNUMPREM,FILE = FNAMEPREM,STATUS = 'UNKNOWN')
             WRITE (FNUMPREM,'(A26)') '$PLANT_RESPONSES(MEASURED)'
             CLOSE(FNUMPREM)

            ! ERROR FILES
             INQUIRE (FILE = FNAMEERA,EXIST = FFLAG)
             OPEN (UNIT = FNUMERA,FILE = FNAMEERA,STATUS = 'UNKNOWN')
             WRITE (FNUMERA,'(A27)') '$ERRORS   As % of measured '
             CLOSE(FNUMERA)
             OPEN (UNIT = FNUMERT,FILE = FNAMEERT,STATUS = 'UNKNOWN')
             WRITE (FNUMERT,'(A20)') '$ERRORS(TIME_COURSE)'
             WRITE (FNUMERT,*)' '
             WRITE (FNUMERT,'(A25)')'! Errors as % of measured'
             CLOSE(FNUMERT)

            ! Initialize 'previous' variables
            CROPPREV = '  '
            VARNOPREV = ' '
            CUDIRFLPREV = ' '
            ECONOPREV = ' '
            ECDIRFLPREV = ' '
            SPDIRFLPREV = ' '
          ENDIF

        ENDIF ! End of first time through stuff

!-----------------------------------------------------------------------
!       Open or close and re-open work and reads files  
!-----------------------------------------------------------------------

!        INQUIRE (FILE = 'WORK.OUT',OPENED = FOPEN)
        ! Normally is closed at the end of the program.
!        IF (.NOT.FOPEN) THEN
!          IF (RUN.EQ.1) THEN
!            OPEN (UNIT = FNUMWRK,FILE = 'WORK.OUT')
!            WRITE(FNUMWRK,*) 'CSCRP  Cropsim Cereal Crop Module '
!          ELSE
!            OPEN (UNIT = FNUMWRK,FILE = 'WORK.OUT',POSITION='APPEND',
!     &            ACTION = 'READWRITE')
!            WRITE(fnumwrk,*) ' '
!            WRITE(fnumwrk,*) 'CSCRP  Cropsim Cereal Crop Module '
            ! If do not wish to accumulate for all runs
            !IF (IDETL.EQ.'0'.OR.IDETL.EQ.'Y'.OR.IDETL.EQ.'N') THEN
            !  CLOSE (FNUMWRK)
            !  OPEN (UNIT = FNUMWRK,FILE = 'WORK.OUT')
            !  WRITE(fnumwrk,*) ' '
            !  WRITE(fnumwrk,*) 'CSCRP  Cropsim Cereal Crop Module '
            !ENDIF  
!            IF (IDETL.EQ.'A') THEN
!              CLOSE (FNUMWRK, STATUS = 'DELETE')
!              OPEN (UNIT = FNUMWRK,FILE = 'WORK.OUT')
!              WRITE(fnumwrk,*) ' '
!              WRITE(fnumwrk,*) 'CSCRP  Cropsim Cereal Crop Module '
!            ENDIF  
! FO/LPM/GH/CHP - 12-04-2020 - READS.out file removed from CSM output.            
!            IF (FNUMREA.LE.0) CALL Getlun('READS.OUT',fnumrea)
            ! Close and re-open Reads file
!            CLOSE (FNUMREA, STATUS = 'DELETE')
!            OPEN (UNIT = FNUMREA,FILE = 'READS.OUT', STATUS = 'NEW',
!     &            ACTION = 'READWRITE')
!            WRITE(fnumrea,*)' '
!            WRITE(fnumrea,*)
!     &      ' File closed and re-opened to avoid generating huge file'
!          ENDIF
!        ELSE  ! File is open .. not closed at end of run!        
!          IF (IDETL.EQ.'0'.OR.IDETL.EQ.'Y'.OR.IDETL.EQ.'N') THEN
            ! IDETL is headed VBOSE
            ! Close and re-open Work file. 
!            CLOSE (FNUMWRK, STATUS = 'DELETE')
!            OPEN (UNIT = FNUMWRK,FILE = 'WORK.OUT', STATUS = 'NEW',
!     &            ACTION = 'READWRITE')
!            WRITE(fnumwrk,*) ' '
!            WRITE(fnumwrk,*) 'CSCRP  Cropsim Cereal Crop Module '
! FO/LPM/GH/CHP - 12-04-2020 - READS.out file removed from CSM output.            
!            CALL Getlun('READS.OUT',fnumrea)
            ! Close and re-open Reads file
!            CLOSE (FNUMREA, STATUS = 'DELETE')
!            OPEN (UNIT = FNUMREA,FILE = 'READS.OUT', STATUS = 'NEW',
!     &            ACTION = 'READWRITE')
!            WRITE(fnumrea,*)' '
!            WRITE(fnumrea,*)
!     &      ' File closed and re-opened to avoid generating huge file'
!          ELSE  
!            WRITE(fnumwrk,*) ' '
!            WRITE(fnumwrk,*) 'CSCRP  Cropsim Cereal Crop Module '
!          ENDIF
!        ENDIF  
          
!-----------------------------------------------------------------------
!       Record/set starting information
!-----------------------------------------------------------------------

        YEARSIM = YEAR*1000 + DOY
        SEASENDOUT = 'N'  ! Season end outputs flag     

!-----------------------------------------------------------------------
!       Increment counter for re-initialized runs within module
!-----------------------------------------------------------------------

        RUNCRP = RUNCRP + 1
      END SUBROUTINE CRP_RunInit
