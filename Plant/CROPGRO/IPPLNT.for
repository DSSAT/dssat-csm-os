C=======================================================================
C  IPPLNT, Subroutine, C.H. Porter
C-----------------------------------------------------------------------
C  Reads variables from crop or species specific data file
C
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  09/21/1998 CHP Split IPCROP into IPCROP and IPPLNT
C  05/10/1999 GH  Incoporated in CROPGRO
C  03/16/2000 GH  Further adaptation for modular system
C  09/21/2001 CHP Read KCAN, PORMIN, RWUEP1 and RWUMX here for export
C                   to WATBAL.
C  08/12/2003 CHP Added I/O error checking
C  06/30/2006 CHP/CDM Added KC_SLOPE to SPE file and KC_ECO to ECO file.
C                 Added warning for use of default ecotype.
!  09/11/2008 KJB, CHP Added 5 species parameters affecting N stress
C  01/18/2018 KRT Added functionality for ASCE dual Kc ET routines
C-----------------------------------------------------------------------
!  Called:      PLANT
!  Calls:       FIND, ERROR, IGNORE
C=======================================================================
      SUBROUTINE IPPLNT(CONTROL, ISWITCH, 
     &  CADPR1, CMOBMX, CROP, DETACH, ECONO,              !Output
     &  EORATIO, FILECC, FILEGC, FRCNOD, FREEZ1, FREEZ2,  !Output
     &  KCAN, KC_SLOPE, KEP, NOUTDO, PCARSH, PCH2O,       !Output
     &  PLIPSH, PLIGSD, PLIGSH, PMINSD, PMINSH, POASD,    !Output
     &  POASH, PORMIN, PROLFI, PRORTI, PROSHI, PROSTI,    !Output
     &  R30C2, RCH2O, RES30C, RFIXN, RLIG, RLIP, RMIN,    !Output
     &  RNH4C, RNO3C, ROA, RPRO, RWUEP1, RWUMX, TTFIX)    !Output
!     &  NSTR_FAC, NSTR_EXP, NRAT_FAC, EXCS_FAC, EXCS_EXP) !Output

C-----------------------------------------------------------------------

      USE ModuleDefs
      USE ModuleData
      IMPLICIT NONE

!-----------------------------------------------------------------------
      CHARACTER*1  BLANK, UPCASE, DETACH, MEEVP
      PARAMETER (BLANK  = ' ')

      CHARACTER*2 CROP
      CHARACTER*6 SECTION, ECONO, ECOTYP
      CHARACTER*6  ERRKEY
      PARAMETER (ERRKEY = 'IPPLNT')

      CHARACTER*12 FILEC, FILEE
      CHARACTER*30 FILEIO
      CHARACTER*78 MSG(6)
      CHARACTER*80 PATHCR, CHAR, PATHEC
      CHARACTER*92 FILECC, FILEGC
      CHARACTER*255 C255

      INTEGER LUNCRP, LUNECO, NOUTDO, LUNIO
      INTEGER PATHL, FOUND, ERR, LINC, LNUM, ISECT

      REAL
     &  CADPR1, CMOBMX, FRCNOD, FREEZ1, FREEZ2,
     &  PCARSH, PCH2O, PLIPSH, PLIGSD,
     &  PLIGSH, PMINSD, PMINSH, POASD, POASH,
     &  PROLFI, PRORTI, PROSHI, PROSTI, R30C2,
     &  RCH2O, RES30C, RFIXN, RLIG, RLIP, RMIN,
     &  RNH4C, RNO3C, ROA, RPRO, TTFIX

!     Species-dependant variables exported to SPAM or WATBAL:
      REAL EORATIO, KCAN, KEP, PORMIN, RWUMX, RWUEP1
      REAL KCAN_ECO, KC_SLOPE
      REAL SKC, KCBMIN, KCBMAX

!     Species parameters for N stress  9/11/2008
!     REAL NSTR_FAC, NSTR_EXP, NRAT_FAC, EXCS_FAC, EXCS_EXP

!     The variable "CONTROL" is of constructed type "ControlType" as 
!     defined in ModuleDefs.for, and contains the following variables.
!     The components are copied into local variables for use here.
      TYPE (ControlType) CONTROL
      TYPE (SwitchType) ISWITCH
      FILEIO = CONTROL % FILEIO
      LUNIO  = CONTROL % LUNIO
      MEEVP = ISWITCH % MEEVP

!-----------------------------------------------------------------------
!       Read data from FILEIO for use in PLANT module
!-----------------------------------------------------------------------
      OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,0)
!-----------------------------------------------------------------------
      READ(LUNIO,50,IOSTAT=ERR) FILEC, PATHCR ; LNUM = 7
   50 FORMAT(6(/),15X,A12,1X,A80)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)

      READ(LUNIO,51,IOSTAT=ERR) FILEE, PATHEC; LNUM = LNUM + 1
   51 FORMAT(15X,A12,1X,A80)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)

!-----------------------------------------------------------------------
!    Read Experiment Details, Treatments, and Cultivars Sections
!-----------------------------------------------------------------------
      SECTION = '*EXP.D'
      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILEIO, LNUM)
      ELSE
        READ(LUNIO,'(////,3X,A2)',IOSTAT=ERR) CROP; LNUM = LNUM + 5
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
      ENDIF

!-----------------------------------------------------------------------
      IF (CROP .NE. 'FA') THEN
!-----------------------------------------------------------------------
!    Read Cultivar Section
!-----------------------------------------------------------------------
        SECTION = '*CULTI'
        CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
        IF (FOUND .EQ. 0) THEN
          CALL ERROR(SECTION, 42, FILEIO, LNUM)
        ELSE
          READ(LUNIO,'(24X,A6)',IOSTAT=ERR) ECONO ; LNUM = LNUM + 1
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
        ENDIF

      ENDIF
      
      CLOSE (LUNIO)

!-----------------------------------------------------------------------
      IF (CROP .NE. 'FA') THEN
!-----------------------------------------------------------------------
! READ CROP PARAMETERS FROM FILEC
!-----------------------------------------------------------------------
        LNUM = 0
        PATHL  = INDEX(PATHCR,BLANK)
        IF (PATHL .LE. 1) THEN
          FILECC = FILEC
        ELSE
          FILECC = PATHCR(1:(PATHL-1)) // FILEC
        ENDIF
        CALL GETLUN('FILEC', LUNCRP)
        OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,0)
        LNUM = 0
C-----------------------------------------------------------------------
C READ PHOTOSYNTHESIS PARAMETERS *******************
C-----------------------------------------------------------------------
        SECTION = '!*PHOT'
        CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
        IF (FOUND .EQ. 0) THEN
          CALL ERROR(SECTION, 42, FILECC, LNUM)
        ELSE
          CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
          IF (ISECT .EQ. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
          READ(CHAR,'(12X,F6.0)',IOSTAT=ERR) KCAN
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
          !KC_SLOPE optional, default value 0.1
          READ(CHAR,'(18X,F6.0)',IOSTAT=ERR) KC_SLOPE
          IF (ERR .NE. 0 .OR. KC_SLOPE .LT. 1.E-6) KC_SLOPE = 0.1
        ENDIF

C-----------------------------------------------------------------------
C READ RESPIRATION PARAMETERS **********************
C-----------------------------------------------------------------------
        SECTION = '!*RESP'
        CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
        IF (FOUND .EQ. 0) THEN
          CALL ERROR(SECTION, 42, FILECC, LNUM)
        ELSE
          CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
          READ(CHAR,'(G12.0,F6.1)',IOSTAT=ERR) RES30C, R30C2
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
  
          CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
          READ(CHAR,'(4F6.0)',IOSTAT=ERR) RNO3C, RNH4C, RPRO, RFIXN
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
  
          CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
          READ(CHAR,'(6F6.0)',IOSTAT=ERR)
     &            RCH2O, RLIP, RLIG, ROA, RMIN, PCH2O
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
        ENDIF
  
C-----------------------------------------------------------------------
C READ PLANT COMPOSITION PARAMETERS
C-----------------------------------------------------------------------
        SECTION = '!*PLAN'
        CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
        IF (FOUND .EQ. 0) THEN
          CALL ERROR(SECTION, 42, FILECC, LNUM)
        ELSE
          CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
          READ(CHAR,'(F6.0,12X,F6.0)',IOSTAT=ERR) PROLFI, PROSTI
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
  
          CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
          READ(CHAR,'(F6.0,12X,F6.0)',IOSTAT=ERR) PRORTI, PROSHI
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

          CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
          CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
          READ(CHAR,'(18X,F6.0)',IOSTAT=ERR) PCARSH
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
  
          CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
          READ(CHAR,'(18X,F6.0)',IOSTAT=ERR) PLIPSH
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
  
          CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
          READ(CHAR,'(18X,2F6.0)',IOSTAT=ERR) PLIGSH, PLIGSD
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

          CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
          READ(CHAR,'(18X,2F6.0)',IOSTAT=ERR) POASH, POASD
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
  
          CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
          READ(CHAR,'(18X,2F6.0)',IOSTAT=ERR) PMINSH, PMINSD
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
        ENDIF

C-----------------------------------------------------------------------
C READ CARBON AND NITROGEN MINING PARAMETERS
C-----------------------------------------------------------------------
        SECTION = '!*CARB'
        CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
        IF (FOUND .EQ. 0) THEN
          CALL ERROR(SECTION, 42, FILECC, LNUM)
        ELSE
          CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
          READ(CHAR,'(F6.0,6X,F6.0)',IOSTAT=ERR) CMOBMX, CADPR1
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
        ENDIF

C-----------------------------------------------------------------------
C READ NITROGEN FIXATION PARAMATERS
C-----------------------------------------------------------------------
        SECTION = '!*NITR'
        CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
        IF (FOUND .EQ. 0) THEN
          CALL ERROR(SECTION, 42, FILECC, LNUM)
        ELSE
          CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
          READ(CHAR,'(18X,F6.0)',IOSTAT=ERR) TTFIX
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
        ENDIF
  
C-----------------------------------------------------------------------
C  
C     ***** READ PARTITIONING PARAMETERS *****************
C
C-----------------------------------------------------------------------
        SECTION = '!*VEGE'
        CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
        IF (FOUND .EQ. 0) THEN
          CALL ERROR(SECTION, 42, FILECC, LNUM)
        ELSE
          CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
          CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
          CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
          CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
          READ(CHAR,'(30X,F6.0)',IOSTAT=ERR) FRCNOD
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
        ENDIF

C-----------------------------------------------------------------------
C
C     ***** READ SENESCENCE PARAMETERS ******************
C       This is found in the second heading that begins with '!*LEAF'
C-----------------------------------------------------------------------
        SECTION = '!*LEAF'
        CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
        IF (FOUND .EQ. 0) THEN
          CALL ERROR(SECTION, 42, FILECC, LNUM)
        ENDIF
  
        CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
        IF (FOUND .EQ. 0) THEN
          CALL ERROR(SECTION, 42, FILECC, LNUM)
        ELSE
          CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
          READ(CHAR,'(18X,2F6.0)',IOSTAT=ERR) FREEZ1, FREEZ2
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
        ENDIF

!-----------------------------------------------------------------------
C         Read ROOT parameters
!-----------------------------------------------------------------------
        SECTION = '!*ROOT'
        CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
        IF (FOUND .EQ. 0) THEN
            CALL ERROR(SECTION, 42, FILECC, LNUM)
        ELSE
          CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
            IF (ISECT .EQ. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
            READ(CHAR,'(30X,2F6.0)',IOSTAT=ERR) RWUEP1, RWUMX
            IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
  
          CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
          CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
            IF (ISECT .EQ. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
            READ(CHAR,'(12X,F6.0)',IOSTAT=ERR) PORMIN
            IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
        ENDIF

C-----------------------------------------------------------------------
C
C     ***** READ POD DETACHMENT PARAMETERS *****
C
C-----------------------------------------------------------------------
        SECTION = '!*POD '
        CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
        IF (FOUND .EQ. 0) THEN
          CALL ERROR(SECTION, 42, FILECC, LNUM)
        ELSE
          CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
          READ(CHAR,'(5X,A1)',IOSTAT=ERR) DETACH
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
          DETACH = UPCASE(DETACH)
        ENDIF
  
C-----------------------------------------------------------------------
C
C     ***** READ EVAPOTRANSPIRATION PARAMETERS *****
C
C-----------------------------------------------------------------------
        SECTION = '!*EVAP'
        CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
        IF (FOUND .EQ. 0) THEN
          CALL ERROR(SECTION, 42, FILECC, LNUM)
        ELSE
          CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
          IF (MEEVP .EQ. 'A' .OR. MEEVP .EQ. 'G') THEN !ASCE dual Kc ET
            READ(CHAR,'(5F6.0)',IOSTAT=ERR)KEP,EORATIO,SKC,KCBMIN,KCBMAX
            SKC    = MAX(0.50,MIN(1.0,SKC))
            KCBMIN = MAX(0.00,MIN(1.1,KCBMIN))
            KCBMAX = MAX(0.25,MIN(1.5,KCBMAX))
            CALL PUT('SPAM', 'SKC', SKC)
            CALL PUT('SPAM', 'KCBMIN', KCBMIN)
            CALL PUT('SPAM', 'KCBMAX', KCBMAX)
          ELSE
            READ(CHAR,'(2F6.0)',IOSTAT=ERR) KEP, EORATIO
          ENDIF
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
        ENDIF

C-----------------------------------------------------------------------
C
C     ***** READ NITROGEN STRESS PARAMETERS *****
C
C-----------------------------------------------------------------------
!        REWIND (LUNCRP)
!        CALL FIND2(LUNCRP, '*NITROGEN STRESS', LNUM, FOUND)
!        IF (FOUND .EQ. 0) THEN
!!         Use default values
!          NSTR_FAC = 0.70
!          NSTR_EXP = 1.00
!          NRAT_FAC = 1.00
!          EXCS_FAC = 0.20
!          EXCS_EXP = 0.50
!          MSG(1) = "Default nitrogen stress values will be used."
!        ELSE
!          CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
!          READ(CHAR,'(2F6.0)',IOSTAT=ERR) NSTR_FAC, NSTR_EXP
!          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
!          CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
!          READ(CHAR,'(F6.0)',IOSTAT=ERR) NRAT_FAC
!          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
!          CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
!          READ(CHAR,'(2F6.0)',IOSTAT=ERR) EXCS_FAC, EXCS_EXP
!          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
!          MSG(1) = "Nitrogen stress values read from species file."
!        ENDIF
!
!        WRITE(MSG(2),'(A,F6.2)') "NSTRES factor   = ", NSTR_FAC
!        WRITE(MSG(3),'(A,F6.2)') "NSTRES exponent = ", NSTR_EXP
!        WRITE(MSG(4),'(A,F6.2)') "NRATIO factor   = ", NRAT_FAC
!        WRITE(MSG(5),'(A,F6.2)') "EXCESS factor   = ", EXCS_FAC
!        WRITE(MSG(6),'(A,F6.2)') "EXCESS exponent = ", EXCS_EXP
!        CALL INFO(6,ERRKEY,MSG)

        CLOSE (LUNCRP)

C-----------------------------------------------------------------------
C    Read Ecotype Parameter File
C-----------------------------------------------------------------------
!    Set file plus pathname for ecotype parameter file
!-----------------------------------------------------------------------
        PATHL  = INDEX(PATHEC,BLANK)
        IF (PATHL .LE. 1) THEN
          FILEGC = FILEE
        ELSE
          FILEGC = PATHEC(1:(PATHL-1)) // FILEE
        ENDIF

!       Get ecotype Kcan, if present.  
!       If not here, use value read from species file.
        LUNECO = LUNCRP
        OPEN (LUNECO,FILE = FILEGC,STATUS = 'OLD',IOSTAT=ERR)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEGC,0)
        ECOTYP = '      '
        LNUM = 0
        DO WHILE (ECOTYP .NE. ECONO)
          CALL IGNORE(LUNECO, LNUM, ISECT, C255)
          IF ((ISECT .EQ. 1) .AND. (C255(1:1) .NE. ' ') .AND.
     &          (C255(1:1) .NE. '*')) THEN
            READ (C255,'(A6,139X,F6.0)',IOSTAT=ERR) ECOTYP, KCAN_ECO
            IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEGC,LNUM)
            IF (ECOTYP .EQ. ECONO) EXIT

          ELSE IF (ISECT .EQ. 0) THEN
            IF (ECONO .EQ. 'DFAULT') THEN
              MSG(1)='No default ecotype found in file: '
              WRITE(MSG(2),'(2X,A)') FILEGC(1:76)
              MSG(3)='Program will halt.'
              CALL WARNING(3, ERRKEY, MSG)
              CALL ERROR(ERRKEY,35,FILEGC,LNUM)
            ENDIF

!           Write message to WARNING.OUT file that default ecotype 
!             will be used.
            WRITE(MSG(1),5000) ECONO
            WRITE(MSG(2),'(2X,A)') FILEGC(1:76)
            WRITE(MSG(3),5001) 
 5000       FORMAT('Ecotype ',A6,' not found in file: ')
 5001       FORMAT('Default ecotype parameters will be used.')
            CALL WARNING(3, ERRKEY, MSG)

            ECONO = 'DFAULT'
            REWIND(LUNECO)
            LNUM = 0
          ENDIF
        ENDDO

        CLOSE (LUNECO)

        IF (KCAN_ECO .GT. 1.E-6) THEN
          KCAN = KCAN_ECO
        ENDIF

!-----------------------------------------------------------------------
      ENDIF
!-----------------------------------------------------------------------
!     Assign unit number for overview.out file
      CALL GETLUN('OUTO', NOUTDO)

      RETURN
      END SUBROUTINE IPPLNT
!=======================================================================

!-----------------------------------------------------------------------
! Variable definitions
!-----------------------------------------------------------------------
! CADPR1   Maximum fraction of stem growth after flowering that can be 
!            allocated to carbohydrate storage just before a full seed load 
!            is set. ( fraction)
! CMOBMX   Maximum C pool mobilization rate (g[CH2O] / m2 / d)
! CROP     Crop identification code 
! DETACH   Switch to determine if pod detachment will be simulated (Y or N)
! ECONO    Ecotype code - used to match ECOTYP in .ECO file 
! ENAME    Experiment description 
! ERR      Error code for file operation 
! EXPER    Experiment code (prefix of input files) 
! FILEC    Filename for SPE file (e.g., SBGRO980.SPE) 
! FILECC   Path plus filename for species file (*.spe) 
! FILEE    Filename for ECO file (e.g., SBGRO980.ECO) 
! FILEGC   Pathname plus filename for ECO file 
! FILEIO   Filename for INP file (e.g., IBSNAT35.INP) 
! FOUND    Indicator that good data was read from file by subroutine FIND 
!            (0 - End-of-file encountered, 1 - NAME was found) 
! FRCNOD   Fraction of new root dry matter allocation that is diverted to 
!            nodule growth 
! FREEZ1   Temperature below which plant loses all leaves, but development 
!            continues (�C)
! FREEZ2   Temperature below which plant growth stops completely. (�C)
! ISECT    Data record code (0 - End of file encountered, 1 - Found a good 
!            line to read, 2 - End of Section in file encountered, denoted 
!            by * in column 1
! ISWDIS   Pest damage simulation switch (Y or N) 
! KCBMAX   Maximum basal crop coefficient for ASCE dual Kc ET method
! KCBMIN   Minimum basal crop coefficient for ASCE dual Kc ET method
! LUNCRP   Logical unit number for FILEC (*.spe file) 
! MODEL    Name of CROPGRO executable file 
! NL       maximum number of soil layers = 20 
! PATHCR   Pathname for SPE file or FILEE. 
! PATHEC   Pathname for FILEC 
! PATHL    Number of characters in path name (path plus filename for FILEC)
!            
! PCARSH   Proportion of shell tissue that is carbohydrate (fraction)
! PCH2O    Respiration loss due to storage/mobilization of CH2O
!            (g[CH2O] / g[CH2O])
! PLIGSD   Proportion of seed tissue that is lignin (fraction)
! PLIGSH   Proportion of shell tissue that is lignin (fraction)
! PLIPSH   Proportion of shell tissue that is lipid (fraction)
! PMINSD   Proportion of seed tissue that is mineral (fraction)
! PMINSH   Proportion of shell tissue that is mineral (fraction)
! POASD    Proportion of seed tissue that is organic acid (fraction)
! POASH    Proportion of shell tissue that is organic acid (fraction)
! PROLFI   Maximum protein composition in leaves during growth with 
!            luxurious supply of N (g[protein] / g[leaf tissue])
! PRORTI   Maximum protein composition in roots during growth with 
!            luxurious supply of N (g[protein] / g[root])
! PROSHI   Maximum protein composition in shells during growth with 
!            luxurious supply of N ( g[protein] / g[shell tissue])
! PROSTI   Maximum protein composition in stems during growth with 
!            luxurious supply of N (g[protein] / g[stem])
! R30C2    Respiration coefficient that depends on total plant mass, value 
!            at 30C (g[CH2O] used / g[CH2O] fixed / hr)
! RCH2O    Respiration required for synthesizing CH2O structure
!            (g[CH2O] / g[tissue])
! RES30C   Respiration coefficient that depends on gross photosynthesis, 
!            value at 30C (g CH2O/g DW/hr)
! RFIXN    CH2O required for biological N fixation (g[CH2O] / g[protein])
! RLIG     Respiration required for synthesizing lignin structure
!            (g[CH2O] / g[lignin])
! RLIP     Respiration required for synthesizing lipid structure
!            (g[CH2O] / g[lipid])
! RMIN     Respiration required for synthesizing mineral structure
!            (g[CH2O] / g[mineral])
! RNH4C    CH2O required for protein synthesis when source of N is ammonium 
!            uptake (g[CH2O] / g[protein])
! RNO3C    Respiration required for reducing NO3 to protein
!            (g[CH2O] / g[protein])
! ROA      Respiration required for synthesizing organic acids
!            (g[CH2O] / g[product])
! RPRO     Respiration required for re-synthesizing protein from mobilized 
!            N (g[CH2O] / g[protein])
! SKC      Shaping coefficient for ASCE dual Kc ET approach
! TTFIX    Physiological days delay in nodule initiation
!            (photo-thermal days / day)
!-----------------------------------------------------------------------
!      END SUBROUTINE IPPLNT
!=======================================================================
