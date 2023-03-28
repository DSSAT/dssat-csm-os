!***********************************************************************
!  SOMFIX_C, subroutine for CENTURY-based SOM/residue module of DSSAT.
!
!  Purpose: This subroutine reads the input data from the SOMFX???.SDA
!           file.
!
!  Revision history:
!  ........   Parton et al.  Written for CENTURY model.
!  01/01/1999 AJG  Revised and linked to DSSAT.
!  03/26/2003 GH   Modifed file name
!  08/12/2003 CHP  Added I/O error checking
!  11/17/2003 AJG  Renamed CHECK_C to CHECKFIX_C
!  09/06/2004 AJG  Changed .NE. into /= etc.
!  11/12/2004 AJG Caught many errors in the P code and got it functioning
!                 for the first time, but calibration still needed.
!  09/11/2006 AJG Changed input for C:P ratios.
!  10/25/2007 CHP/GH Changed name of SOMFIX file to SOMFX045.SDA
!  04/30/2008 CHP Path for SDA files set in DSSATPRO file
!  --------------------------------------------------------------------
!  Called: SOILNI_C
!  Calls : CHECKFIX_C
!***********************************************************************

      SUBROUTINE SOMFIX_C (CONTROL, 
     &  CEDAM, CES1, CES1M, CES1T, CES1X, CES2,           !Output
     &  CES21I, CES21M, CES21S, CES21T, CES21X, CES23,    !Output
     &  CES23LM, CES23LX,                                 !Output
     &  CES23M, CES23T, CES23X, CES2LI,                   !Output
     &  CES2LM, CES2LS, CES2LX, CES3, CES3M, CES3T,       !Output
     &  CES3X, CESTR, CO2MET, CO2S1, CO2S1I, CO2S1S,      !Output
     &  CO2S2, CO2S3, CO2STR, CULMETQ, CULS1Q,            !Output
     &  CULS2Q, CULS3Q, CULSTRQ, DECMET, DECS1,           !Output
     &  DECS2, DECS3, DECSTR, FRDAE, FRMETI, FRMETS,      !Output
     &  LIGSTR, RESDAX, S1S3I, S1S3S, S2S3I, S2S3S,       !Output
     &  TXS1I, TXS1S)                                     !Output

!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         !which contain control information, soil
                         !parameters, hourly weather data.
      IMPLICIT  NONE
      EXTERNAL PATH, ERROR, GETLUN, CHECKFIX_C
      SAVE

      CHARACTER*1, PARAMETER :: BLANK = ' '
      CHARACTER*6,  PARAMETER :: ERRKEY  = 'SOMFIX'
      CHARACTER*12 NAMEF
      CHARACTER*14 NAME
      CHARACTER*30 FILEIO
      CHARACTER*80 PATHSD
      CHARACTER*92 SOMPF
      CHARACTER*12 SOMFILE 

      INTEGER ERRNUM, I, LNUM, LUN, LUNIO, PFLAG

      LOGICAL FEXIST

      REAL CO2S1I, CO2S1S, CO2S2, CO2S3, CULMETQ,
     &  CULS1Q, CULS2Q, CULS3Q, CULSTRQ, FRMETI, FRMETS,
     &  RESDAX, S1S3I, S1S3S, S2S3I, S2S3S, TXS1I, TXS1S

      REAL CEDAM(3), CESTR(3), CO2MET(0:1),
     &  CO2S1(0:NL), DECMET(0:1), DECS1(0:1),
     &  DECS2(1), DECS3(1), DECSTR(0:1),
     &  FRDAE(3), LIGSTR(0:1)

      REAL CES1(0:NL,3), CES1M(0:1,3), CES1T(0:1,3), CES1X(0:1,3),
     &  CES2(1:NL,3), CES21I(0:0,3), CES21M(0:1,3), CES21S(0:1,3),
     &  CES21T(1,3), CES21X(0:1,3), CES23(1:NL,3),
     &  CES23LM(0:1,3), CES23LX(0:1,3),
     &  CES23M(0:1,3), CES23T(1:1,3), CES23X(0:1,3),
     &  CES2LI(0:1,3), CES2LM(0:1,3), CES2LS(0:1,3), CES2LX(0:1,3),
     &  CES3(1:NL,3), CES3M(1,3), CES3T(1,3), CES3X(1,3), CO2STR(0:1,2)

!     Constructed variables defined in ModuleDefs.
      TYPE (ControlType) CONTROL

!      IF (N_ELEMS <= 0) RETURN

!     Transfer values from constructed data types into local variables.
      FILEIO  = CONTROL % FILEIO
      LUNIO   = CONTROL % LUNIO

!     ------------------------------------------------------------------
        SOMFILE = 'SOMFX' // ModelVerTxt // '.SDA'

        SOMPF = SOMFILE
        INQUIRE (FILE = SOMPF, EXIST = FEXIST)
        IF (.NOT. FEXIST) THEN
          CALL PATH('STD',CONTROL%DSSATP,PATHSD,PFLAG,NAMEF)
          SOMPF = TRIM(PATHSD) // SOMFILE
        ENDIF

        INQUIRE(FILE = SOMPF, EXIST = FEXIST)
        IF (.NOT. FEXIST) THEN
          CALL ERROR(ERRKEY, 1, SOMFILE, 0)
        ENDIF

        CALL GETLUN('FINPUT', LUN)
        OPEN (UNIT = LUN, FILE = SOMPF, STATUS = 'OLD', IOSTAT=ERRNUM)
      IF (ERRNUM /= 0) CALL ERROR(ERRKEY,ERRNUM,SOMFILE,0)
        LNUM = 1

!       Skip the blank lines and text lines in the SOMFIX file.
        DO I = 1, 14
          READ (LUN,*,IOSTAT=ERRNUM)
        IF (ERRNUM /= 0) CALL ERROR (ERRKEY, ERRNUM, FILEIO, LNUM)
          LNUM = LNUM + 1
        END DO

!       Start reading the data from the SOMFIX file, checking 
!       every time whether the correct variable or parameter has been
!       read.
        READ (LUN,*,IOSTAT=ERRNUM) NAME, CEDAM(1)
        CALL CHECKFIX_C (NAME, 'CEDAM(1)    ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CEDAM(2)
        CALL CHECKFIX_C (NAME, 'CEDAM(2)    ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CEDAM(3)
        CALL CHECKFIX_C (NAME, 'CEDAM(3)    ', ERRNUM, LNUM)

!       The variables CES1, CES2 an CES3, as given in the SOMFIX
!       file are only used to initialize the model. These variables
!       should be site-specific, but because very few users will have
!       these data available for their site, they are included here as
!       fixed parameters (but with the option of changing them in
!       SOMFIX file).
        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES1(0,1)
        CALL CHECKFIX_C (NAME, 'CES1(0,1)   ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES1(0,2)
        CALL CHECKFIX_C (NAME, 'CES1(0,2)   ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES1(0,3)
        CALL CHECKFIX_C (NAME, 'CES1(0,3)   ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES1(1,1)
        CALL CHECKFIX_C (NAME, 'CES1(1,1)   ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES1(1,2)
        CALL CHECKFIX_C (NAME, 'CES1(1,2)   ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES1(1,3)
        CALL CHECKFIX_C (NAME, 'CES1(1,3)   ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES1M(0,1)
        CALL CHECKFIX_C (NAME, 'CES1M(0,1)  ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES1M(0,2)
        CALL CHECKFIX_C (NAME, 'CES1M(0,2)  ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES1M(0,3)
        CALL CHECKFIX_C (NAME, 'CES1M(0,3)  ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES1M(1,1)
        CALL CHECKFIX_C (NAME, 'CES1M(1,1)  ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES1M(1,2)
        CALL CHECKFIX_C (NAME, 'CES1M(1,2)  ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES1M(1,3)
        CALL CHECKFIX_C (NAME, 'CES1M(1,3)  ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES1T(0,1)
        CALL CHECKFIX_C (NAME, 'CES1T(0,1)  ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES1T(0,2)
        CALL CHECKFIX_C (NAME, 'CES1T(0,2)  ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES1T(0,3)
        CALL CHECKFIX_C (NAME, 'CES1T(0,3)  ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES1T(1,1)
        CALL CHECKFIX_C (NAME, 'CES1T(1,1)  ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES1T(1,2)
        CALL CHECKFIX_C (NAME, 'CES1T(1,2)  ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES1T(1,3)
        CALL CHECKFIX_C (NAME, 'CES1T(1,3)  ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES1X(0,1)
        CALL CHECKFIX_C (NAME, 'CES1X(0,1)  ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES1X(0,2)
        CALL CHECKFIX_C (NAME, 'CES1X(0,2)  ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES1X(0,3)
        CALL CHECKFIX_C (NAME, 'CES1X(0,3)  ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES1X(1,1)
        CALL CHECKFIX_C (NAME, 'CES1X(1,1)  ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES1X(1,2)
        CALL CHECKFIX_C (NAME, 'CES1X(1,2)  ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES1X(1,3)
        CALL CHECKFIX_C (NAME, 'CES1X(1,3)  ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES2(1,1)
        CALL CHECKFIX_C (NAME, 'CES2(1,1)   ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES2(1,2)
        CALL CHECKFIX_C (NAME, 'CES2(1,2)   ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES2(1,3)
        CALL CHECKFIX_C (NAME, 'CES2(1,3)   ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES21I(0,1)
        CALL CHECKFIX_C (NAME, 'CES21I(0,1) ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES21I(0,2)
        CALL CHECKFIX_C (NAME, 'CES21I(0,2) ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES21I(0,3)
        CALL CHECKFIX_C (NAME, 'CES21I(0,3) ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES21M(0,1)
        CALL CHECKFIX_C (NAME, 'CES21M(0,1) ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES21M(0,2)
        CALL CHECKFIX_C (NAME, 'CES21M(0,2) ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES21M(0,3)
        CALL CHECKFIX_C (NAME, 'CES21M(0,3) ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES21M(1,1)
        CALL CHECKFIX_C (NAME, 'CES21M(1,1) ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES21M(1,2)
        CALL CHECKFIX_C (NAME, 'CES21M(1,2) ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES21M(1,3)
        CALL CHECKFIX_C (NAME, 'CES21M(1,3) ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES21S(0,1)
        CALL CHECKFIX_C (NAME, 'CES21S(0,1) ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES21S(0,2)
        CALL CHECKFIX_C (NAME, 'CES21S(0,2) ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES21S(0,3)
        CALL CHECKFIX_C (NAME, 'CES21S(0,3) ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES21T(1,1)
        CALL CHECKFIX_C (NAME, 'CES21T(1,1) ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES21T(1,2)
        CALL CHECKFIX_C (NAME, 'CES21T(1,2) ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES21T(1,3)
        CALL CHECKFIX_C (NAME, 'CES21T(1,3) ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES21X(0,1)
        CALL CHECKFIX_C (NAME, 'CES21X(0,1) ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES21X(0,2)
        CALL CHECKFIX_C (NAME, 'CES21X(0,2) ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES21X(0,3)
        CALL CHECKFIX_C (NAME, 'CES21X(0,3) ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES21X(1,1)
        CALL CHECKFIX_C (NAME, 'CES21X(1,1) ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES21X(1,2)
        CALL CHECKFIX_C (NAME, 'CES21X(1,2) ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES21X(1,3)
        CALL CHECKFIX_C (NAME, 'CES21X(1,3) ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES2LI(0,1)
        CALL CHECKFIX_C (NAME, 'CES2LI(0,1) ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES2LI(0,2)
        CALL CHECKFIX_C (NAME, 'CES2LI(0,2) ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES2LI(0,3)
        CALL CHECKFIX_C (NAME, 'CES2LI(0,3) ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES2LI(1,1)
        CALL CHECKFIX_C (NAME, 'CES2LI(1,1) ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES2LI(1,2)
        CALL CHECKFIX_C (NAME, 'CES2LI(1,2) ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES2LI(1,3)
        CALL CHECKFIX_C (NAME, 'CES2LI(1,3) ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES2LM(0,1)
        CALL CHECKFIX_C (NAME, 'CES2LM(0,1) ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES2LM(0,2)
        CALL CHECKFIX_C (NAME, 'CES2LM(0,2) ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES2LM(0,3)
        CALL CHECKFIX_C (NAME, 'CES2LM(0,3) ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES2LM(1,1)
        CALL CHECKFIX_C (NAME, 'CES2LM(1,1) ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES2LM(1,2)
        CALL CHECKFIX_C (NAME, 'CES2LM(1,2) ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES2LM(1,3)
        CALL CHECKFIX_C (NAME, 'CES2LM(1,3) ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES2LS(0,1)
        CALL CHECKFIX_C (NAME, 'CES2LS(0,1) ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES2LS(0,2)
        CALL CHECKFIX_C (NAME, 'CES2LS(0,2) ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES2LS(0,3)
        CALL CHECKFIX_C (NAME, 'CES2LS(0,3) ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES2LS(1,1)
        CALL CHECKFIX_C (NAME, 'CES2LS(1,1) ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES2LS(1,2)
        CALL CHECKFIX_C (NAME, 'CES2LS(1,2) ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES2LS(1,3)
        CALL CHECKFIX_C (NAME, 'CES2LS(1,3) ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES2LX(0,1)
        CALL CHECKFIX_C (NAME, 'CES2LX(0,1) ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES2LX(0,2)
        CALL CHECKFIX_C (NAME, 'CES2LX(0,2) ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES2LX(0,3)
        CALL CHECKFIX_C (NAME, 'CES2LX(0,3) ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES2LX(1,1)
        CALL CHECKFIX_C (NAME, 'CES2LX(1,1) ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES2LX(1,2)
        CALL CHECKFIX_C (NAME, 'CES2LX(1,2) ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES2LX(1,3)
        CALL CHECKFIX_C (NAME, 'CES2LX(1,3) ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES3(1,1)
        CALL CHECKFIX_C (NAME, 'CES3(1,1)   ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES3(1,2)
        CALL CHECKFIX_C (NAME, 'CES3(1,2)   ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES3(1,3)
        CALL CHECKFIX_C (NAME, 'CES3(1,3)   ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES3M(1,1)
        CALL CHECKFIX_C (NAME, 'CES3M(1,1)  ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES3M(1,2)
        CALL CHECKFIX_C (NAME, 'CES3M(1,2)  ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES3M(1,3)
        CALL CHECKFIX_C (NAME, 'CES3M(1,3)  ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES3T(1,1)
        CALL CHECKFIX_C (NAME, 'CES3T(1,1)  ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES3T(1,2)
        CALL CHECKFIX_C (NAME, 'CES3T(1,2)  ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES3T(1,3)
        CALL CHECKFIX_C (NAME, 'CES3T(1,3)  ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES3X(1,1)
        CALL CHECKFIX_C (NAME, 'CES3X(1,1)  ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES3X(1,2)
        CALL CHECKFIX_C (NAME, 'CES3X(1,2)  ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CES3X(1,3)
        CALL CHECKFIX_C (NAME, 'CES3X(1,3)  ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CESTR(1)
        CALL CHECKFIX_C (NAME, 'CESTR(1)    ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CESTR(2)
        CALL CHECKFIX_C (NAME, 'CESTR(2)    ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CESTR(3)
        CALL CHECKFIX_C (NAME, 'CESTR(3)    ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CO2MET(0)
        CALL CHECKFIX_C (NAME, 'CO2MET(0)   ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CO2MET(1)
        CALL CHECKFIX_C (NAME, 'CO2MET(1)   ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CO2S1(0)
        CALL CHECKFIX_C (NAME, 'CO2S1(0)    ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CO2S1I
        CALL CHECKFIX_C (NAME, 'CO2S1I      ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CO2S1S
        CALL CHECKFIX_C (NAME, 'CO2S1S      ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CO2S2
        CALL CHECKFIX_C (NAME, 'CO2S2       ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CO2S3
        CALL CHECKFIX_C (NAME, 'CO2S3       ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CO2STR(0,1)
        CALL CHECKFIX_C (NAME, 'CO2STR(0,1) ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CO2STR(1,1)
        CALL CHECKFIX_C (NAME, 'CO2STR(1,1) ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CO2STR(0,2)
        CALL CHECKFIX_C (NAME, 'CO2STR(0,2) ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CO2STR(1,2)
        CALL CHECKFIX_C (NAME, 'CO2STR(1,2) ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CULMETQ
        CALL CHECKFIX_C (NAME, 'CULMETQ     ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CULS1Q
        CALL CHECKFIX_C (NAME, 'CULS1Q      ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CULS2Q
        CALL CHECKFIX_C (NAME, 'CULS2Q      ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CULS3Q
        CALL CHECKFIX_C (NAME, 'CULS3Q      ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, CULSTRQ
        CALL CHECKFIX_C (NAME, 'CULSTRQ     ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, DECMET(0)
        CALL CHECKFIX_C (NAME, 'DECMET(0)   ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, DECMET(1)
        CALL CHECKFIX_C (NAME, 'DECMET(1)   ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, DECS1(0)
        CALL CHECKFIX_C (NAME, 'DECS1(0)    ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, DECS1(1)
        CALL CHECKFIX_C (NAME, 'DECS1(1)    ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, DECS2(1)
        CALL CHECKFIX_C (NAME, 'DECS2(1)    ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, DECS3(1)
        CALL CHECKFIX_C (NAME, 'DECS3(1)    ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, DECSTR(0)
        CALL CHECKFIX_C (NAME, 'DECSTR(0)   ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, DECSTR(1)
        CALL CHECKFIX_C (NAME, 'DECSTR(1)   ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, FRDAE(1)
        CALL CHECKFIX_C (NAME, 'FRDAE(1)    ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, FRDAE(2)
        CALL CHECKFIX_C (NAME, 'FRDAE(2)    ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, FRDAE(3)
        CALL CHECKFIX_C (NAME, 'FRDAE(3)    ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, FRMETI
        CALL CHECKFIX_C (NAME, 'FRMETI      ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, FRMETS
        CALL CHECKFIX_C (NAME, 'FRMETS      ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, LIGSTR(0)
        CALL CHECKFIX_C (NAME, 'LIGSTR(0)   ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, LIGSTR(1)
        CALL CHECKFIX_C (NAME, 'LIGSTR(1)   ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, RESDAX
        CALL CHECKFIX_C (NAME, 'RESDAX      ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, S1S3I
        CALL CHECKFIX_C (NAME, 'S1S3I       ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, S1S3S
        CALL CHECKFIX_C (NAME, 'S1S3S       ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, S2S3I
        CALL CHECKFIX_C (NAME, 'S2S3I       ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, S2S3S
        CALL CHECKFIX_C (NAME, 'S2S3S       ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, TXS1I
        CALL CHECKFIX_C (NAME, 'TXS1I       ', ERRNUM, LNUM)

        READ (LUN,*,IOSTAT=ERRNUM) NAME, TXS1S
        CALL CHECKFIX_C (NAME, 'TXS1S       ', ERRNUM, LNUM)

!     ----------------------------------------------------------------
!     P MODULE.
!     ----------------------------------------------------------------
!      IF (N_ELEMS <= 1) RETURN

      READ (LUN,*)

      READ (LUN,*) NAME, CES23(1,2)
      CALL CHECKFIX_C (NAME, 'CES23(1,2)  ', ERRNUM, LNUM)

      READ (LUN,*) NAME, CES23LM(0,2)
      CALL CHECKFIX_C (NAME, 'CES23LM(0,2)', ERRNUM, LNUM)

      READ (LUN,*) NAME, CES23LM(1,2)
      CALL CHECKFIX_C (NAME, 'CES23LM(1,2)', ERRNUM, LNUM)

      READ (LUN,*) NAME, CES23LX(0,2)
      CALL CHECKFIX_C (NAME, 'CES23LX(0,2)', ERRNUM, LNUM)

      READ (LUN,*) NAME, CES23LX(1,2)
      CALL CHECKFIX_C (NAME, 'CES23LX(1,2)', ERRNUM, LNUM)

      READ (LUN,*)

      READ (LUN,*) NAME, CES23M(0,2)
      CALL CHECKFIX_C (NAME, 'CES23M(0,2) ', ERRNUM, LNUM)

      READ (LUN,*) NAME, CES23M(1,2)
      CALL CHECKFIX_C (NAME, 'CES23M(1,2) ', ERRNUM, LNUM)

      READ (LUN,*) NAME, CES23T(1,2)
      CALL CHECKFIX_C (NAME, 'CES23T(1,2) ', ERRNUM, LNUM)

      READ (LUN,*) NAME, CES23X(0,2)
      CALL CHECKFIX_C (NAME, 'CES23X(0,2) ', ERRNUM, LNUM)

      READ (LUN,*) NAME, CES23X(1,2)
      CALL CHECKFIX_C (NAME, 'CES23X(1,2) ', ERRNUM, LNUM)

        CLOSE (LUN)

!     ------------------------------------------------------------------
      RETURN
      END SUBROUTINE SOMFIX_C


!***********************************************************************
!***********************************************************************
!  CHECKFIX_C, subroutine for CENTURY-based SOM/residue module of DSSAT.
!
!  Purpose: This subroutine checks whether the data read in the
!           SOMFIX and TEMPIN subroutines are correctly read.
!
!  Revision history:
!  ........ Parton et al.  Written for CENTURY model.
!  01/01/1999 AJG  Revised and linked to DSSAT.
!  08/12/2003 CHP  Send error message to Warning.out file
!  11/17/2003 AJG  Renamed CHECK_C to CHECKFIX_C
!
!  Called: SOMFIX
!  Calls : --
!***********************************************************************

      SUBROUTINE CHECKFIX_C (NAME, EXPECT, ERRNUM, LNUM)


!     ------------------------------------------------------------------
      USE ModuleDefs
      IMPLICIT  NONE
      EXTERNAL ERROR, WARNING
!     ------------------------------------------------------------------
!      CHARACTER*6  ROUTIN
      CHARACTER*6, PARAMETER :: ERRKEY = 'SOMFIX'
      CHARACTER*12 EXPECT, NAME
      CHARACTER*12 SOMFILE 
      CHARACTER*78  MSG(4)
      INTEGER ERRNUM, LNUM
      INTEGER LEN

      SOMFILE = 'SOMFX' // ModelVerTxt // '.SDA'

      IF (ERRNUM /= 0) CALL ERROR(ERRKEY,ERRNUM,SOMFILE,LNUM)
      LNUM = LNUM + 1

!     chp 08/26/2004
!     When NAME and EXPECT come into this routine, the trailing
!     spaces in EXPECT cause the variables to be unequal.  Even
!     after using the TRIM function, they are still unequal.  
!     This fixes the problem.
      LEN = LEN_TRIM(NAME)    
      IF (NAME(1:LEN) /= EXPECT(1:LEN)) THEN
!     IF (NAME /= EXPECT) THEN
        WRITE(MSG(1),11) SOMFILE
        WRITE(MSG(2),12) 
        WRITE(MSG(3),13) EXPECT, NAME
        WRITE(MSG(4),14) 
   11   FORMAT(' File: ',A12)
   12   FORMAT(' There was an error when reading the input data.')
   13   FORMAT(' Instead of ',A11,' the program encountered ',A11,'.')
   14   FORMAT(' Program will stop.')
        CALL WARNING(4, ERRKEY, MSG)

        WRITE(*,'(/,A78)') MSG(1)
        WRITE(*,'(A78)')   MSG(2)
        WRITE(*,'(A78)')   MSG(3)
        WRITE(*,'(A78,/)') MSG(4)

        STOP
      ENDIF

!     ------------------------------------------------------------------
      RETURN
      END SUBROUTINE CHECKFIX_C

!***********************************************************************
! CHECKFIX_C variables:
!
! EXPECT      Variable / parameter that was expected when checking the 
!             input data read by SOMFIX_C
! NAME        Variable / parameter that was read when checking the input
!             data read by SOMFIX_C
! ROUTIN      File from which data were read when checking the input data
!             read by SOMFIX_C.
!***********************************************************************


