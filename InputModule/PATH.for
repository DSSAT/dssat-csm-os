C=======================================================================
C  PATH, Subroutine
C
C  Program to read the PATH from DSSATPRO file
C-----------------------------------------------------------------------
C  Revision history
C
C  01/01/1991 GH  Written
C  05/28/1993 PWW Header revision and minor changes
C  09/17/2007 JIL Added codes for IXIM maize model
C  08/09/2012 GH  Add codes for CSCAS cassava model
C  04/16/2013 CHP/KAD Added codes for SALUS model
!  05/09/2013 CHP/FR/JZW Added N-wheat module
C  06/03/2015 LPM Added codes for CSYCA CIAT cassava model
C  06/18/2015 GH  Add error code for configuration file issues
C  09/26/2017 WP  Add DSSAT_HOME environment variable configuration
C-----------------------------------------------------------------------
C  INPUT  : PROCOD,PFLAG
C
C  LOCAL  : BLANK ERRKEY FILEPR LINE LUNPR ERRNUM PATHL I K FEXIST
C
C  OUTPUT : PATHC,NAMEF
C-----------------------------------------------------------------------
C  Called : SECLI SEWTH IPEXP
C
C  Calls  : ERROR
C=======================================================================

      SUBROUTINE PATH (PROCOD,DSSATP,PATHC,PFLAG,NAMEF)

      USE ModuleDefs
      IMPLICIT     NONE
      EXTERNAL ERROR, GETLUN, GET_NEXT_STRING

      CHARACTER*1  BLANK
      CHARACTER*3  PROCOD
      CHARACTER*6  ERRKEY
      CHARACTER*12 NAMEF
      CHARACTER*102 DSSATP
      CHARACTER*80 LINE,PATHC

      INTEGER      LUNPR,ERRNUM,PATHL,I,K,PFLAG
      LOGICAL      FEXIST

!     PARAMETER (LUNPR  = 15)
      PARAMETER (ERRKEY = 'PATH  ')
      PARAMETER (BLANK  = ' ')

      PATHC = BLANK
      CALL GETLUN('DSPRO',LUNPR)
      INQUIRE (FILE = DSSATP,EXIST = FEXIST)
      IF (FEXIST) THEN
         OPEN (LUNPR,FILE=DSSATP,STATUS = 'OLD',IOSTAT=ERRNUM)
      ENDIF
      IF (.NOT. FEXIST .OR. ERRNUM .NE. 0) RETURN

      DO I = 1, 500
         READ (LUNPR,'(A80)',IOSTAT=ERRNUM) LINE
         IF (LINE(1:3) .EQ. PROCOD) THEN
!            PATHC  = LINE(5:6) // LINE(8:80)
            call get_next_string(line,4,pathc)
            if(index(line,trim(pathc))<7)then
               call get_next_string(line,7,pathc)
               pathc = trim(adjustl(line(5:6)))//trim(pathc)
            end if

C-SUN       PATHC  = LINE(8:80)
            PATHL  = INDEX (PATHC,BLANK)
            IF (PATHL .EQ. 1) THEN
                 CALL ERROR (ERRKEY,3,DSSATP,I)
            ENDIF
            IF (PATHC(PATHL-1:PATHL-1) /= SLASH) THEN
              WRITE (PATHC(PATHL:PATHL),'(A1)') SLASH
            ELSE
              PATHL = PATHL - 1
            ENDIF
            NAMEF  = PATHC (PATHL+1:PATHL+13)
            IF (PATHL .LT. 80) THEN
               DO K = (PATHL+1),80
                  IF (PATHC(K:K) .NE. BLANK) THEN
                     PATHC(K:K) = BLANK
                  ENDIF
               END DO
            ENDIF
            CLOSE (LUNPR)
            RETURN
         ENDIF
      END DO

      IF (PFLAG .EQ. 1) THEN
         WRITE(*,600) PROCOD, DSSATP
 600     FORMAT(' CODE ',A3,' not found in file ',A)
         CALL ERROR (ERRKEY,1,DSSATP,I)
      ENDIF

      CLOSE (LUNPR)
      END SUBROUTINE PATH

C=======================================================================
C  PATHD, Subroutine
C
C  Program to read the PATH of DSSATPRO file
C    Assume in current directory first.
C    If not in current directory, DSSATPRO file is stored with the EXEs
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written                                       G.H         8-10-93
C  2  Modified by
C  3. Header revision and minor changes             P.W.W.      5-28-93
! 07/17/2008 CHP DSSATPRO name specified in ModuleDefs
C-----------------------------------------------------------------------
C  INPUT  : INPUTX
C
C  LOCAL  : BLANK ERRKEY FILEPR LINE LUNPR ERRNUM PATHL I K FEXIST
C
C  OUTPUT : DSSATP
C-----------------------------------------------------------------------
C  Called : INPUT
C
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  FEXIST :
C=======================================================================

      SUBROUTINE PATHD (DSSATP,INPUTX,IP)

      USE ModuleDefs
      IMPLICIT     NONE
      EXTERNAL ERROR

!     CHARACTER*12 DSSATF
      CHARACTER*102 DSSATP
      CHARACTER*120 INPUTX
      CHARACTER(len=255) :: DSSAT_HOME

      INTEGER      I
      INTEGER    IP

      LOGICAL      FEXIST

!     DSSATP(1:12) = DSSATF
      DSSATP(1:12) = DSSATPRO
      INQUIRE (FILE = DSSATP,EXIST = FEXIST)
      IF (.NOT. FEXIST .AND. IP .GT. 12) THEN
         DO I = IP, 0, -1
           IF (INPUTX(I:I) .EQ. SLASH .OR. INPUTX(I:I) .EQ. "/")GO TO 10
         END DO
   10    CONTINUE
!        DSSATP(1:I+12) = INPUTX(1:I) // DSSATF
         DSSATP(1:I+12) = INPUTX(1:I) // DSSATPRO
      ENDIF

      INQUIRE (FILE = DSSATP,EXIST = FEXIST)
      IF (.NOT. FEXIST) THEN
        CALL get_environment_variable("DSSAT_HOME", DSSAT_HOME)
        IF(TRIM(DSSAT_HOME) .NE. '') THEN
            STDPATH = TRIM(DSSAT_HOME)
        ENDIF
        DSSATP = trim(STDPATH) // TRIM(DSSATPRO)
      ENDIF

      INQUIRE (FILE = DSSATP,EXIST = FEXIST)
      IF (.NOT. FEXIST) THEN
        CALL ERROR ('PATH  ',2,DSSATP,0)
      ENDIF

      RETURN
      END SUBROUTINE PATHD

C=======================================================================
C  MODEL_NAME, Subroutine
C
C  Program to read the Module name from DSSATPRO file
C-----------------------------------------------------------------------
C  Revision history
C
C  02/21/2006 GH  Written
!  10/25/2006 CHP Added CRMODEL, the model name from FILEX which will
!                   override MODEL from DSSATPRO, if valid.
!  09/17/2007 JIL Added codes for IXIM maize model
!  04/17/2013 CHP Added exception for crop-model matching for
!                   SALUS generic crop model
C-----------------------------------------------------------------------
C  INPUT  : PROCOD,PFLAG
C
C  LOCAL  : BLANK ERRKEY FILEPR LINE LUNPR ERRNUM PATHL I K FEXIST
C
C  OUTPUT : PATHC,NAMEF
C-----------------------------------------------------------------------
C  Called : SECLI SEWTH IPEXP
C
C  Calls  : ERROR
C=======================================================================

      SUBROUTINE MODEL_NAME (CROP, DSSATP, CRMODEL, MODEL)

      USE ModuleDefs
      IMPLICIT     NONE
      EXTERNAL ERROR, WARNING, IGNORE, UPCASE, LENSTRING, 
     &  GET_NEXT_STRING, VALIDMODEL

      CHARACTER*1   UPCASE
      CHARACTER*2   CROP
      CHARACTER*3   PROCOD
      CHARACTER*6   ERRKEY
      CHARACTER*8   MODEL, CRMODEL
      CHARACTER*78  MSG(4)
      CHARACTER*80  LINE
      CHARACTER*102 DSSATP
!     CHARACTER*120 PATHX

      INTEGER      EXE_POS,LUNPR,LINPR,ERRNUM,ISECT,I,J   !, IPX
      INTEGER      ValidModel, LENSTRING

      LOGICAL FEXIST

      PARAMETER (LUNPR = 25)
      PARAMETER (ERRKEY = 'MODELN')
!      PARAMETER (EXE_STRING = 'EXE')

      IF (LENSTRING(CRMODEL) > 0) THEN
        DO I = 1,8
          CRMODEL(I:I)= UPCASE(CRMODEL(I:I))
        ENDDO
!       CHP 5/22/2008 Check for model version being integer value
!       Pick off name of executable, if necessary.
        DO I = 6,8
          J = ICHAR(CRMODEL(I:I))
          IF (J < 48 .OR. J > 57) THEN
!            Fill in model version from executable name
             CRMODEL(6:8) = ModelVerTxt
            EXIT
          ENDIF
        ENDDO
        ERRNUM = ValidModel(CROP, CRMODEL)
        IF (ERRNUM == 0) THEN
          MODEL = CRMODEL
          RETURN
        ELSE
          MSG(1) = "Invalid model specified."
          WRITE(MSG(2),'("Crop: ",A2,"; Model: ",A8)') CROP, CRMODEL
          MSG(3) = "Use default model from DSSATPRO."
          CALL WARNING(3,ERRKEY,MSG)
        ENDIF
      ELSE
        ERRNUM = 8
      ENDIF
!     If no model in FILEX, or invalid, use DSSATPRO default model

      INQUIRE (FILE = DSSATP,EXIST = FEXIST)
      IF (FEXIST) THEN
         OPEN (LUNPR,FILE=TRIM(DSSATP),STATUS = 'OLD',IOSTAT=ERRNUM)
         IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,1,DSSATP,0)
      ELSE
         CALL ERROR (ERRKEY,2,DSSATP,0)
      ENDIF

      PROCOD = 'M' // CROP

      REWIND(LUNPR)
      LINPR = 0

 100  CONTINUE
      CALL IGNORE (LUNPR, LINPR, ISECT,LINE)
      IF (ISECT .EQ. 0) THEN
         MSG(1) = "Code not found in DSSATPRO file."
         MSG(2) = "File: " // DSSATP(1:72)
         WRITE(MSG(3),'("Code = ",A3)') PROCOD
         MSG(4) = "Program will end."
         CALL WARNING(4,ERRKEY,MSG)
         CALL ERROR (ERRKEY,3,DSSATP,LINPR)
      ELSEIF (LINE(1:3) .NE. PROCOD) THEN
         GO TO 100
      ELSE
         EXE_POS = INDEX(LINE,'DSCSM')
         if(exe_pos==0) exe_pos = index(line,'dscsm')
!         MODEL = LINE((EXE_POS+4):(EXE_POS+11))
         call get_next_string(line,8,model)
         exe_pos = index(line,model)
         call get_next_string(line,exe_pos,model)
         DO I = 1,5
           MODEL(I:I)= UPCASE(MODEL(I:I))
         ENDDO
      ENDIF

      CLOSE (LUNPR)

!     Check MODEL for validity.
      ERRNUM = ValidModel(CROP, MODEL)
      IF (ERRNUM /= 0) THEN
        MSG(1) = "No valid model found in DSSATPRO for this crop."
        WRITE(MSG(2),'("Crop: ",A2)') CROP
        MSG(3) = "Simulation will stop."
        CALL WARNING(3,ERRKEY,MSG)
        CALL ERROR (ERRKEY,ERRNUM,DSSATP,0)
      ENDIF

      RETURN
      END SUBROUTINE MODEL_NAME


!======================================================================
      FUNCTION ValidModel(CROP, MODEL)

      IMPLICIT NONE
      EXTERNAL ReadCropModels
      SAVE

      INTEGER ValidModel
      CHARACTER*2, INTENT(IN) :: CROP
      CHARACTER*8, INTENT(IN) :: MODEL

      INTEGER, PARAMETER :: MaxNum = 100
      INTEGER I
      CHARACTER*2 CropName(MaxNum)
      CHARACTER*5 ModelName(MaxNum)
!      CHARACTER*78 MSG(2)

      LOGICAL FIRST
      DATA FIRST /.TRUE./

      ValidModel = 0      !0 indicates no error

!     Blank model = error 6
      IF (MODEL(1:3) .EQ. '   ') THEN
        ValidModel = 6
        RETURN
      ENDIF

!!    Check for valid crop modeling approaches
!      IF ((INDEX(MODEL(3:5),'CER') .EQ. 0) .AND.   !CERES
!!    &    (INDEX(MODEL(3:5),'ORZ') .EQ. 0) .AND.   !ORYZA
!     &    (INDEX(MODEL(3:5),'IXM') .EQ. 0) .AND.   !IXIM
!     &    (INDEX(MODEL(3:5),'GRO') .EQ. 0) .AND.   !CROPGRO
!     &    (INDEX(MODEL(3:5),'FRM') .EQ. 0) .AND.   !FORAGE
!     &    (INDEX(MODEL(3:5),'CSM') .EQ. 0) .AND.   !CROPSIM (Cereal)
!     &    (INDEX(MODEL(3:5),'CAS') .EQ. 0) .AND.   !CSCAS (Cassava)
!     &    (INDEX(MODEL(3:5),'YCA') .EQ. 0) .AND.   !CSYCA (CIAT-Cassava)
!     &    (INDEX(MODEL(3:5),'SIM') .EQ. 0) .AND.   !CROPSIM (Cassava)
!     &    (INDEX(MODEL(3:5),'SUB') .EQ. 0) .AND.   !SUBSTOR
!     &    (INDEX(MODEL(3:5),'CAN') .EQ. 0) .AND.   !CANEGRO
!     &    (INDEX(MODEL(3:5),'CSP') .EQ. 0) .AND.   !CASUPRO
!     &    (INDEX(MODEL(3:5),'SAM') .EQ. 0) .AND.   !SAMUCA
!     &    (INDEX(MODEL(3:5),'ALO') .EQ. 0) .AND.   !ALOHA
!     &    (INDEX(MODEL(3:5),'ARO') .EQ. 0) .AND.   !AROIDS
!     &    (INDEX(MODEL(3:5),'CRP') .EQ. 0) .AND.   !CropSim cassava
!     &    (INDEX(MODEL(3:5),'APS') .EQ. 0) .AND.   !APSIM N-wheat
!     &    (INDEX(MODEL(3:5),'OIL') .EQ. 0) .AND.   !OILCROP
!     &    (INDEX(MODEL(3:5),'LUS') .EQ. 0)         !SALUS
!     &    ) THEN
!!       Invalid model = error 4
!        ValidModel = 4
!        RETURN
!      ENDIF
!
!!     Check for valid crop modules
!      IF ((INDEX(MODEL(1:5),'CSCER') .EQ. 0) .AND. !Wheat and Barley
!     &    (INDEX(MODEL(1:5),'CSCRP') .EQ. 0) .AND. !Wheat and barley
!     &    (INDEX(MODEL(1:5),'CSCAS') .EQ. 0) .AND. !Cassava
!     &    (INDEX(MODEL(1:5),'CSYCA') .EQ. 0) .AND. !Cassava CIAT
!     &    (INDEX(MODEL(1:5),'WHAPS') .EQ. 0) .AND. !APSIM N-wheat
!     &    (INDEX(MODEL(1:5),'TFAPS') .EQ. 0) .AND. !Tef based on N-wheat
!     &    (INDEX(MODEL(1:5),'CRGRO') .EQ. 0) .AND. !CROPGRO (All
!!                         grain legumes, grasses, vegetables and cotton
!     &    (INDEX(MODEL(1:5),'PRFRM') .EQ. 0) .AND. !FORAGE
!     &    (INDEX(MODEL(1:5),'MZCER') .EQ. 0) .AND. !Maize CERES
!     &    (INDEX(MODEL(1:5),'MZIXM') .EQ. 0) .AND. !Maize IXIM
!     &    (INDEX(MODEL(1:5),'MLCER') .EQ. 0) .AND. !Millet
!     &    (INDEX(MODEL(1:5),'PIALO') .EQ. 0) .AND. !Aloha Pineapple
!     &    (INDEX(MODEL(1:5),'PTSUB') .EQ. 0) .AND. !Potato
!     &    (INDEX(MODEL(1:5),'RICER') .EQ. 0) .AND. !CERES-Rice
!!    &    (INDEX(MODEL(1:5),'RIORZ') .EQ. 0) .AND. !ORYZA-Rice
!     &    (INDEX(MODEL(1:5),'SGCER') .EQ. 0) .AND. !Sorghum
!     &    (INDEX(MODEL(1:5),'SCCSP') .EQ. 0) .AND. !Sugarcane CASUPRO
!     &    (INDEX(MODEL(1:5),'SCCAN') .EQ. 0) .AND. !Sugarcane CaneGro
!     &    (INDEX(MODEL(1:5),'SCSAM') .EQ. 0) .AND. !Sugarcane SAMUCA
!     &    (INDEX(MODEL(1:5),'BSCER') .EQ. 0) .AND. !Sugarbeet VSH
!     &    (INDEX(MODEL(1:5),'SWCER') .EQ. 0) .AND. !Sweet corn
!     &    (INDEX(MODEL(1:5),'TNARO') .EQ. 0) .AND. !Tanier
!     &    (INDEX(MODEL(1:5),'TRARO') .EQ. 0)       !Taro
!!    &    (INDEX(MODEL(1:5),'TRARO') .EQ. 0) .AND. !Taro
!!    &    (INDEX(MODEL(1:5),'SUOIL') .EQ. 0)       !Oilcrop
!     &    ) THEN
!!       Invalid module name = error 5
!        ValidModel = 5
!        RETURN
!      ENDIF

!     Check that crop and model are compatible based on codes in
!     Simulation.CDE
      IF (FIRST) THEN
        CALL ReadCropModels(MaxNum, ModelName, CropName)
        FIRST = .FALSE.
      ENDIF

!!     Generic SALUS model can be used for any crop,
!!       identified in cultivar data
!      IF (MODEL(1:5) == 'SALUS') THEN
!        ValidModel = 0
!        RETURN
!      ENDIF
!
      ValidModel = 7  !Assume crop does NOT match model
      DO I = 1, MaxNum
        IF (CropName(I) == CROP .AND. ModelName(I) == MODEL(1:5)) THEN
!         Model matches crop -- return with no error
          ValidModel = 0
          RETURN
        ENDIF
      ENDDO

!      IF (ValidModel /= 0) THEN
!        WRITE(MSG(1),100) CROP, MODEL
!  100   FORMAT("Crop ",A2," can not be simulated with model ",A5)
!        MSG(2)="Check SIMULATION.CDE file for valid crop models."
!        CALL WARNING(2,"MODCHECK",MSG)
!      ENDIF

      RETURN
      END FUNCTION ValidModel
!======================================================================
!  MODEL and CROP should be modified when model versions change
!     or when a crop specific model is created.

!  MODEL   CROP  Description
!  BSCER    BS   CERES-Sugarbeet
!  CRGRO    BM   CROPGRO-Bermuda
!  CRGRO    BH   CROPGRO-Bahia
!  CRGRO    BN   CROPGRO-Dry bean
!  CRGRO    BR   CROPGRO-Brachiaria
!  CRGRO    CB   CROPGRO-Cabbage
!  CRGRO    CH   CROPGRO-Chickpea
!  CRGRO    CI   CROPGRO-Chia
!  CRGRO    CN   CROPGRO-Canola
!  CRGRO    CO   CROPGRO-Cotton
!  CRGRO    CP   CROPGRO-Cowpea
!  CRGRO    FA   CROPGRO-Fallow
!  CRGRO    FB   CROPGRO-Faba bean
!  CRGRO    GB   CROPGRO-Grean bean
!  CRGRO    LT   CROPGRO-Lentil
!  CRGRO    NP   CROPGRO-Napier grass
!  CRGRO    PN   CROPGRO-Peanut
!  CRGRO    PP   CROPGRO-Pigeonpea
!  CRGRO    PR   CROPGRO-Bellpepper
!  CRGRO    QU   CROPGRO-Quinoa
!  CRGRO    SF   CROPGRO-Safflower
!  CRGRO    SB   CROPGRO-Soybean
!  CRGRO    SU   CROPGRO-Sunflower
!  CRGRO    TM   CROPGRO-Tomato
!  CRGRO    VB   CROPGRO-Velvetbean
!  CSCER    BA   CROPSIM-CERES-Barley
!  CSCER    WH   CROPSIM-CERES-Wheat
!  CSCRP    BA   CSCRP-Barley
!  CSCAS    CS   CSCAS-Cassava
!  CSYCA    CS   MANIHOT-Cassava
!  CSCRP    WH   CSCRP-Wheat
!  MLCER    ML   CERES-Millet
!  MZCER    MZ   CERES-Maize
!  MZIXM    MZ   IXIM-Maize
!  PTSUB    PT   SUBSTOR-Potato
!  RICER    RI   CERES-Rice
!  SCCAN    SC   CANEGRO-Sugarcane
!  SCCSP    SC   CASUPRO-Sugarcane
!  SCSAM    SC   SAMUCA-Sugarcane
!  SGCER    SG   CERES-Sorghum
!  SWCER    SW   CERES-Sweetcorn
!  PIALO    PI   ALOHA-Pineapple
!  TRARO    TR   AROIDS-Taro
!  TNARO    TN   AROIDS-Tanier
!  TFAPS    TF   NWHEAT-Teff
!  WHAPS    WH   NWHEAT-Wheat
!  PRFRM    BM   FORAGE-Bermudagrass
!  PRFRM    BR   FORAGE-Brachiaria
!  PRFRM    BH   FORAGE-Bahiagrass
!  PRFRM    AL   FORAGE-Alfalfa
!  SUOIL    SU   OILCROP-Sunflower
!  CRGRO    SR   CRGRO-Strawberry
!  CRGRO    SQ   CRGRO-WinterSquash
!  CRGRO    CI   CRGRO-Chia
!-----------------------------------------------------------------------
