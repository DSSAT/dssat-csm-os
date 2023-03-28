!=======================================================================
!  SenLig_Ceres, Subroutine
!-----------------------------------------------------------------------
!  Determines lignin content of senesced material for Ceres crops.
!     Maize, millet, sorghum, rice, wheat(?).
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  03/30/2006 CHP Written 
!  01/11/2007 CHP Changed GETPUT calls to GET and PUT
!=======================================================================

      SUBROUTINE SenLig_Ceres(
     &    PLIGLF, PLIGST, PLIGRT, PLIGSH, PLIGSD)         !Output

      USE ModuleDefs 
      USE ModuleData
      IMPLICIT NONE
      EXTERNAL GETLUN, FIND, IGNORE, INFO, ERROR
      SAVE

!     Output variables
      REAL,INTENT(OUT),OPTIONAL:: PLIGLF, PLIGST, PLIGRT, PLIGSH, PLIGSD

!     Local Variables
      CHARACTER*2  CROP, PREV_CROP
      CHARACTER*6  SECTION, LABEL
      CHARACTER*6, PARAMETER :: ERRKEY='SENLIG'
      CHARACTER*12 FILES
      CHARACTER*30 FILEIO
      CHARACTER*78 MSG(10), MissingCrops
      CHARACTER*80 PATHSR, C80
      CHARACTER*92 FILECC
      INTEGER ERR, FOUND, ISECT, LNUM, LUNCRP, LUNIO    !, NMSG
      REAL VALUE

      REAL PLIGLF_default, PLIGLF_file
      REAL PLIGST_default, PLIGST_file
      REAL PLIGRT_default, PLIGRT_file
      REAL PLIGSH_default, PLIGSH_file
      REAL PLIGSD_default, PLIGSD_file

      TYPE (ControlType) CONTROL

      LOGICAL Missing
      
      DATA PREV_CROP /'xx'/

      DATA PLIGLF_default /0.070/
      DATA PLIGST_default /0.070/
      DATA PLIGRT_default /0.070/
      DATA PLIGSH_default /0.280/
      DATA PLIGSD_default /0.020/

      DATA MissingCrops /"                                              
     &                                "/

      CALL GET(CONTROL)
      CROP = CONTROL % CROP
      IF (CROP == 'FA') RETURN

!----------------------------------------------------------------------
!     If this is the first call to the routine for this crop, then need
!     to read values from species file. 
      IF (CROP .NE. PREV_CROP) THEN

        PLIGLF_File = -99.
        PLIGST_File = -99.
        PLIGRT_File = -99.
        PLIGSH_File = -99.
        PLIGSD_File = -99.

        LUNIO   = CONTROL % LUNIO
        FILEIO  = CONTROL % FILEIO
!       Get name of species file from FILEIO
        CALL GETLUN('FILEIO', LUNIO)
        OPEN (LUNIO, FILE = FILEIO,STATUS = 'OLD',IOSTAT=ERR)  
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,0)
        READ(LUNIO,50,IOSTAT=ERR) FILES, PATHSR; LNUM = 7
   50   FORMAT(//////,15X,A12,1X,A80)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
        CLOSE (LUNIO)

!       -----------------------------------------------------------------
!       Get lignin fractions from species file
        FILECC =  TRIM(PATHSR) // FILES
        CALL GETLUN('FILEC', LUNCRP)
        OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,0)

!-----------------------------------------------------------------
!     Format of species file (minus "!" column 1):
!*PLANT COMPOSITION VALUES
!  PLIGLF  0.070     !Leaf lignin fraction
!  PLIGST  0.070     !Stem lignin fraction 
!  PLIGRT  0.070     !Root lignin fraction 
!  PLIGSH  0.280     !Shell lignin fraction 
!  PLIGSD  0.020     !Seed lignin fraction 
!Could also contain:
!  PLIGTB  0.050     !Tuber lignin fraction
!  PLIGNO  0.050     !Nodule lignin fraction
! etc.
!-----------------------------------------------------------------

        SECTION = '*PLANT'
        CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
        IF (FOUND /= 0) THEN
          DO WHILE (.TRUE.)
            CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
            IF (ISECT /= 1) EXIT
            READ(C80,'(2X,A6,F8.0)',IOSTAT=ERR) LABEL, VALUE
            IF (ERR == 0 .and. VALUE > 0.) THEN
              SELECT CASE(LABEL)
                CASE ("PLIGLF"); PLIGLF_File = Value
                CASE ("PLIGST"); PLIGST_File = Value
                CASE ("PLIGRT"); PLIGRT_File = Value
                CASE ("PLIGSH"); PLIGSH_File = Value
                CASE ("PLIGSD"); PLIGSD_File = Value
              END SELECT
            ENDIF
          ENDDO    !Loop thru lines in section
        ENDIF    !Found section

        PREV_CROP = CROP
      ENDIF   !End of CROP=PREV_CROP clause
      CLOSE(LUNCRP)

!     -----------------------------------------------------------------
!     Assign requested values
!      NMSG = 1
      MISSING = .FALSE.
      IF (PRESENT(PLIGLF)) THEN 
        IF (PLIGLF_file > 0.0) THEN
          PLIGLF = PLIGLF_file
!          NMSG=NMSG+1; WRITE(MSG(NMSG),'("PLIGLF = ",F6.3)') PLIGLF
        ELSE    
          PLIGLF = PLIGLF_default
          Missing = .TRUE.
        ENDIF
      ENDIF
      IF (PRESENT(PLIGST)) THEN 
        IF (PLIGST_file > 0.0) THEN
          PLIGST = PLIGST_file
!          NMSG=NMSG+1; WRITE(MSG(NMSG),'("PLIGST = ",F6.3)') PLIGST
        ELSE    
          PLIGST = PLIGST_default
          Missing = .TRUE.
        ENDIF
      ENDIF
      IF (PRESENT(PLIGRT)) THEN 
        IF (PLIGRT_file > 0.0) THEN
          PLIGRT = PLIGRT_file
!          NMSG=NMSG+1; WRITE(MSG(NMSG),'("PLIGRT = ",F6.3)') PLIGRT
        ELSE    
          PLIGRT = PLIGRT_default
          Missing = .TRUE.
        ENDIF
      ENDIF
      IF (PRESENT(PLIGSH)) THEN 
        IF (PLIGSH_file > 0.0) THEN
          PLIGSH = PLIGSH_file
!          NMSG=NMSG+1; WRITE(MSG(NMSG),'("PLIGSH = ",F6.3)') PLIGSH
        ELSE    
          PLIGSH = PLIGSH_default
          Missing = .TRUE.
        ENDIF
      ENDIF
      IF (PRESENT(PLIGSD)) THEN 
        IF (PLIGSD_file > 0.0) THEN
          PLIGSD = PLIGSD_file
!          NMSG=NMSG+1; WRITE(MSG(NMSG),'("PLIGSD = ",F6.3)') PLIGSD
        ELSE    
          PLIGSD = PLIGSD_default
          Missing = .TRUE.
        ENDIF
      ENDIF

!      IF (NMSG > 1) THEN
!        WRITE(MSG(1),'("CROP: ",A2)') CROP
!        CALL INFO(NMSG,ERRKEY,MSG)
!      ENDIF
!
      IF (MISSING) THEN
        IF (INDEX(MissingCrops,CROP) < 1) THEN
!         Write message only once per crop.
          MissingCrops = CROP // Trim(MissingCrops)
          MSG(1)="Default values used for plant lignin fractions."
          CALL INFO(1,ERRKEY,MSG)
        ENDIF
      ENDIF

!     -----------------------------------------------------------------
      RETURN
      END SUBROUTINE SenLig_Ceres

!=======================================================================
      MODULE Interface_SenLig_Ceres
!     Interface needed for dummy arguments with SenLig_Ceres
      INTERFACE
        SUBROUTINE SenLig_Ceres(
     &    PLIGLF, PLIGST, PLIGRT, PLIGSH, PLIGSD)         !Output
          USE ModuleDefs; IMPLICIT NONE
          REAL,INTENT(OUT),OPTIONAL:: PLIGLF,PLIGST,PLIGRT,PLIGSH,PLIGSD
        END SUBROUTINE SenLig_Ceres
      END INTERFACE
      END MODULE Interface_SenLig_Ceres
!=======================================================================
