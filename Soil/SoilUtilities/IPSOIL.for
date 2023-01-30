C=======================================================================
C  IPSOIL, Subroutine
C
C  Read soil parameters from RESCH???.SDA
C-----------------------------------------------------------------------
C  Revision history
C  06/15/1994 PWW Written
C  02/07/1993 PWW Header revision and minor changes
C  02/07/1993 PWW Added switch common block, restructured
C  06/09/1999 CHP Modular format
C  03/16/2000 GH  Incorporated in CROPGRO
C                 Note, file name should be dynamically created based on
C                 model name
C  03/26/2003 GH  Modified file name and location (SOL directory)
C  08/12/2003 CHP Added I/O error checking
C  08/22/2003 CHP Changed to read file once only and save arrays of data.
C  03/07/2006 CHP Added AM, RCP. Made output arguments optional
!  04/13/2006 CHP Revised format for input file.  Most arguments are 
!                 no longer needed, but defaults will be provided 
!                 rather than values read from file.  Note that the 
!                 values which were previously included in the file
!                 were the same for each residue, so this will not
!                 change results from previous runs.
!  06/28/2007 CHP Added AM, WATFAC to SOILN045.SOL file
!  10/25/2007 CHP/GH Changed name of residue characteristics file to
!                     RESCH045.SDA 
!  04/30/2008 CHP Changed units for SCN, SCP, RCN, RCP to %
!  04/30/2008 CHP Path for SDA files set in DSSATPRO file
!  06/25/2008 CHP Changed units for AM from ha/kg to cm2/g
!                               WATFAC from mm-ha/kg to kg[H2O]/kg[DM]
!                 Added EXTFAC - Light extinction coef. for mulch
!                 Check for file version number, use defaults if wrong 
C-----------------------------------------------------------------------
C  Called : CROPGRO
C=======================================================================

      SUBROUTINE IPSOIL (CONTROL, RESTYPE, CROP,          !Input
     &    AM, DMINR, DSNC, EXTFAC, PRCEL, PRCHO,          !Output
     &    PRLIG, PSLIG, RCN, RCP, RDCEL, RDCHO, RDLIG,    !Output
     &    SCN, SCP, WATFAC)                               !Output

C-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.

      IMPLICIT     NONE
      EXTERNAL PATH, GETLUN, FIND2, IGNORE, WARNING
      SAVE

      TYPE (ControlType), INTENT(IN) :: CONTROL
      CHARACTER(len=5), INTENT(IN), OPTIONAL :: RESTYPE
      CHARACTER(len=2), INTENT(IN), OPTIONAL :: CROP

      REAL, INTENT(OUT), OPTIONAL :: PRLIG, PSLIG, RCN, RCP, SCN, SCP
      REAL, INTENT(OUT), OPTIONAL :: AM, DMINR, DSNC, EXTFAC, PRCEL
      REAL, INTENT(OUT), OPTIONAL :: PRCHO, RDCEL, RDCHO, RDLIG, WATFAC

      CHARACTER(len=1), PARAMETER :: BLANK = ' '
      CHARACTER(len=6), PARAMETER :: ERRKEY = 'IPSOIL'
      CHARACTER(len=3)   ModelVersionDecimal
      CHARACTER(len=9)   VERSION_TXT
      CHARACTER(len=12)  FILESS, NAMEF
      CHARACTER(len=30)  FILEIO
      CHARACTER(len=78)  MSG(4)
      CHARACTER(len=80)  PATHSD
      CHARACTER(len=92)  SOILNF
      CHARACTER(len=180) CHAR

      INTEGER FOUND, ERR, ISECT, LUNSOL, LNUM
      INTEGER I, J, JJ, LUNIO, NRES, PFLAG
      INTEGER, PARAMETER :: NR=50    !Maximum # of residue types
      REAL REDUCE_FRAC

!     Keep default values for legacy variables
      REAL ADMINR, ADSNC, APRCEL, APRCHO
      REAL ARDCEL, ARDCHO, ARDLIG

!     Saved values from file
      CHARACTER*2, DIMENSION(NR) :: ACROP             
      CHARACTER*5, DIMENSION(NR) :: ARESTYPE          
      REAL, DIMENSION(NR) :: AAM, AEXTFAC, ARCN, ARCP, ASCN, ASCP
      REAL, DIMENSION(NR) :: APRLIG, APSLIG, AWATFAC  

      LOGICAL FIRST, FEXIST, EROR

      DATA FIRST /.TRUE./

C-----------------------------------------------------------------------
!     On first call to routine, read file and save values in arrays for
!       subsequent retrieval.
      IF (FIRST) THEN
        FIRST = .FALSE.
        EROR = .FALSE.

!       Assign default values
        ARESTYPE='RE001'
        AAM    = 32.0     !Area covered / unit dry mass of res (cm2/g)
        AWATFAC= 3.8      !Sat. water content of res. (kg[H2O]/kg[DM])
        AEXTFAC= 0.80     !Light extinction coef. for mulch
        APSLIG = 0.10     !Proportion of lignin is surface res. (%)
        ASCN   = 1.0      !N content of init surface residue (%)
        ASCP   = 0.1      !P content of init surface residue (%)  
        APRLIG = 0.10     !Proportion of lignin in root residue (%)
        ARCN   = 1.0      !N content of init root residue (%)
        ARCP   = 0.02     !P content of init root residue (%)

        ADMINR = 8.3E-05
        ADSNC  = 20.0
        ARDCHO = 0.2
        ARDCEL = 0.05
        ARDLIG = 0.0095
        APRCHO = 0.20  !PRCHO, PRCEL, and PRLIG are proportions of
        APRCEL = 0.70  !carbohydrate, cellulose and lignin in residue
        APRLIG = 0.10  !and must sum to 1.0.

        FILEIO  = CONTROL % FILEIO
        LUNIO   = CONTROL % LUNIO

        FILESS = 'RESCH' // ModelVerTxt // '.SDA'
        WRITE(ModelVersionDecimal,'(I1,".",I1)')
     &     Version%Major, Version%Minor

        SOILNF = FILESS
        INQUIRE (FILE = SOILNF, EXIST = FEXIST)
        IF (.NOT. FEXIST) THEN
          CALL PATH('STD',CONTROL%DSSATP,PATHSD,PFLAG,NAMEF)
          SOILNF = TRIM(PATHSD) // FILESS
        ENDIF

        INQUIRE (FILE=SOILNF, EXIST=FEXIST)
        IF (FEXIST) THEN
C-----------------------------------------------------------------------
C       Open the RESCH???.SDA file 
C-----------------------------------------------------------------------
          CALL GETLUN('FILESS', LUNSOL)
          OPEN (LUNSOL,FILE = SOILNF, STATUS = 'OLD', IOSTAT=ERR)
          IF (ERR .EQ. 0) THEN

            CALL FIND2(LUNSOL,'@VERSION',LNUM,FOUND)
            IF (FOUND == 0) THEN 
!             Version not found in file
              EROR = .TRUE.
              ERR = 50
              GOTO 500
            ELSE
!             Read version number
              CALL IGNORE(LUNSOL,LNUM,ISECT,CHAR)
              READ(CHAR,'(A9)',IOSTAT=ERR) VERSION_TXT
              IF (INDEX(VERSION_TXT,ModelVersionDecimal) < 1 .OR. 
     &                ERR /= 0) THEN 
!               Version not found in file, or wrong version
                EROR = .TRUE.
                ERR = 50
                GOTO 500
              ENDIF
            ENDIF
C-----------------------------------------------------------------------
C     Read Soil Parameters from RESCH???.SDA
C-----------------------------------------------------------------------
            I = 0; ISECT = 1
!           EOF not portable. CHP 7/24/2007
!           ReadLoop: DO WHILE (.NOT. EOF (LUNSOL))
            ReadLoop: DO WHILE (ISECT > 0)
              CALL IGNORE(LUNSOL,LNUM,ISECT,CHAR)
              IF (ISECT == 0) EXIT ReadLoop
              IF (CHAR(1:1) .EQ. '*' .OR. CHAR(1:1) .EQ. '@') THEN
                CYCLE ReadLoop
              ENDIF
              I = I + 1
              READ(CHAR,'(1X,A5,1X,A2,3F8.0,2(F8.0,2F6.0))',IOSTAT=ERR)
     &            ARESTYPE(I), ACROP(I), AAM(I), AWATFAC(I), AEXTFAC(I),
     &            APSLIG(I), ASCN(I), ASCP(I),  
     &            APRLIG(I), ARCN(I), ARCP(I)
              IF (ERR .NE. 0) THEN 
                I = I - 1
                EROR = .TRUE.
              ENDIF
            ENDDO ReadLoop
          ELSE
            !File could not be opened
            EROR = .TRUE.
            ERR = 20
          ENDIF
        ELSE
          !File does not exist
          EROR = .TRUE.
          ERR = 40
        ENDIF

        NRES = I

        CONTINUE
        CLOSE (LUNSOL)

C-----------------------------------------------------------------------
  500   CONTINUE
!       Use default values if problems with reading file
        IF (EROR) THEN
          SELECT CASE(ERR)
            CASE (20)
              MSG(1) = 'Error opening file: ' 
            CASE (40)
              MSG(1) = 'File does not exist: '
            CASE (50)
              MSG(1) = 'Incorrect version for file:'
            CASE DEFAULT
              MSG(1) = 'Error reading file: '
          END SELECT
          MSG(2) = SOILNF(1:78)
          MSG(3) = 'Default residue characteristics will be used.'
          CALL WARNING(3,ERRKEY,MSG)
        ENDIF     
      ENDIF       !End of FIRST block

C-----------------------------------------------------------------------
!     Assign values independant of restype (legacy values)
      IF (PRESENT(DMINR)) DMINR = ADMINR
      IF (PRESENT(DSNC )) DSNC  = ADSNC 
      IF (PRESENT(RDCHO)) RDCHO = ARDCHO
      IF (PRESENT(RDCEL)) RDCEL = ARDCEL
      IF (PRESENT(RDLIG)) RDLIG = ARDLIG
      IF (PRESENT(PRCHO)) PRCHO = APRCHO
      IF (PRESENT(PRCEL)) PRCEL = APRCEL

!     Get appropriate residue or crop type, if included
      IF (PRESENT(RESTYPE) .OR. PRESENT(CROP)) THEN
        RLoop: DO J = 1, NR
          IF (PRESENT(RESTYPE)) THEN
            IF (RESTYPE == '     ') THEN
              JJ = 1
              EXIT RLoop
            ENDIF
            IF (RESTYPE .EQ. ARESTYPE(J)) THEN
              JJ = J
              EXIT RLoop    
            ENDIF
          ELSEIF (PRESENT(CROP)) THEN
            IF (CROP == '  ') THEN
              JJ = 1
              EXIT RLoop
            ENDIF
            IF (CROP .EQ. ACROP(J)) THEN
              JJ = J
              EXIT RLoop
            ENDIF
          ENDIF
        ENDDO RLoop

        IF (J > NR) THEN
          JJ = 1
          IF (PRESENT(RESTYPE)) THEN
            WRITE(MSG(1),"('Residue type: ',A5,' not valid.')") RESTYPE
            MSG(2) = 'Default residue characteristics will be used.'
            CALL WARNING(2,ERRKEY,MSG)
          ENDIF
        ENDIF

      ELSE !Default to residue type 1
        JJ = 1
      ENDIF

      IF (PRESENT(AM    )) THEN
        AM    = AAM(JJ)
!       Check for incorrect units
        IF (AM < 0.01) THEN
          AM = AM * 1.E5
          MSG(1) = "Units of mulch aerial coverage appear to be wrong."
          WRITE(MSG(2),'(A,F8.2,A)') "Value changed to ",AM," cm2/g"
          MSG(3) = "May need to update file: "
          MSG(4) = SOILNF(1:78)
          CALL WARNING(4,ERRKEY,MSG)
        ENDIF
      ENDIF

      IF (PRESENT(WATFAC)) THEN
        WATFAC= AWATFAC(JJ)
!       Check for incorrect units
        IF (AM < 0.001) THEN
          WATFAC = WATFAC * 1.E4
          MSG(1) ="Units of mulch saturation factor appear to be wrong."
          WRITE(MSG(2),'(A,F8.3,A)')
     &      "Value changed to ",WATFAC," kg[H2O]/KG[DM]"
          MSG(3) = "May need to update file: "
          MSG(4) = SOILNF(1:78)
          CALL WARNING(4,ERRKEY,MSG)
        ENDIF
      ENDIF

      IF (PRESENT(EXTFAC)) EXTFAC= AEXTFAC(JJ)
      IF (PRESENT(PSLIG )) PSLIG = APSLIG(JJ)
      IF (PRESENT(SCN   )) SCN   = ASCN  (JJ)
      IF (PRESENT(SCP   )) SCP   = ASCP  (JJ)

!     PRLIG must sum with PRCHO and PRCEL to 1.0
      IF (PRESENT(PRLIG )) THEN
        PRLIG = APRLIG(JJ)
        IF (PRLIG .LT. 0) THEN
          PRLIG = APSLIG(JJ)
        ENDIF
        IF (PRESENT(PRCHO) .AND. PRESENT(PRCEL)) THEN
          REDUCE_FRAC = (1.0 - PRLIG) / (PRCHO + PRCEL) 
          PRCHO = PRCHO * REDUCE_FRAC
          PRCEL = PRCEL * REDUCE_FRAC
        ENDIF
      ENDIF

      IF (PRESENT(RCN)) THEN
        RCN = ARCN(JJ)
        IF (RCN .LT. 0) THEN
          RCN = ASCN(JJ)
        ENDIF
      ENDIF
      IF (PRESENT(RCP)) THEN
        RCP = ARCP(JJ)
        IF (RCP .LT. 0) THEN
          RCP = ASCP(JJ)
        ENDIF
      ENDIF

C-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE IPSOIL

C=======================================================================

! IPSOIL and SCREATE Variables
!
! ACRO(I) List of variables names used in RESCH???.SDA file which contains 
!           coefficients needed for simulating the decomposition of soil 
!           organic matter and organic matter 
! CHAR    Contains the contents of last record read 
! DMINR   Maximum decomposition rate constant of stable organic matter
!           (d-1)
! DSNC    Depth to which C and N are integrated across all soil layers for 
!           output in CARBON.OUT (cm)
! ERR     Error code for file operation 
! FILESS  Path plus filename for species file (*.spe) 
! I       Loop counter 
! J       Loop counter 
! LUNSOL  Logical unit number for RESCH???.SDA 
! PRCEL   Cellulose fraction of the residue (fraction)
! PRCHO   Carbohydrate fraction of the residue (fraction)
! PRLIG   Lignin fraction of the residue (fraction)
! RCN     N content of initial root residue (%)
! RCP     P content of initial root residue (%)
! SCN     N content of initial shoot residue (%)
! SCP     P content of initial shoot residue (%)
! RDCEL   Maximum decomposition rate of cellulose (fraction / day)
! RDCHO   Maximum decomposition rate of carbohydrates (fraction / day)
! RDLIG   Maximum decomposition rate of lignin (fraction / day)
! RESTYPE Residue application method as sent from calling routine
! T(I)    Text array with explanatory text of the RESCH???.SDA file 
!=======================================================================

!=======================================================================
      MODULE Interface_IpSoil
!     Interface needed for optional arguments with IPSOIL
      INTERFACE
        SUBROUTINE IPSOIL (CONTROL, RESTYPE, CROP,        !Input
     &    AM, DMINR, DSNC, EXTFAC, PRCEL, PRCHO,          !Output
     &    PRLIG, PSLIG, RCN, RCP, RDCEL, RDCHO, RDLIG,    !Output
     &    SCN, SCP, WATFAC)                               !Output
          USE ModuleDefs
          TYPE (ControlType), INTENT(IN) :: CONTROL
          CHARACTER(len=5), INTENT(IN), OPTIONAL :: RESTYPE
          CHARACTER(len=2), INTENT(IN), OPTIONAL :: CROP
          REAL, INTENT(OUT), OPTIONAL :: AM, DMINR, DSNC, EXTFAC 
          REAL, INTENT(OUT), OPTIONAL :: PRCEL, PRCHO, PRLIG, PSLIG 
          REAL, INTENT(OUT), OPTIONAL :: RCN, RCP, RDCEL, RDCHO, RDLIG
          REAL, INTENT(OUT), OPTIONAL :: SCN, SCP, WATFAC
        END SUBROUTINE
      END INTERFACE
      END MODULE Interface_IpSoil

!=======================================================================
