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
C  06/30/2003 SJR Added READ statements for storage organs
C  07/05/2003 SJR Added READ statements for freeze parameters for storage
C-----------------------------------------------------------------------
!  Called:      CROPGRO
!  Calls:       FIND, ERROR, IGNORE
C=======================================================================
      SUBROUTINE FOR_IPPLNT(CONTROL, 
     &  CADPR1, CMOBMX, CROP, DETACH, ECONO, EORATIO,     !Output
     &  FILECC, FILEGC, FRCNOD, FREEZ1, FREEZ2, KCAN, KEP,!Output
     &  NOUTDO, PCARSH, PCH2O, PLIPSH, PLIGSD, PLIGSH,    !Output
     &  PMINSD, PMINSH, POASD, POASH, PORMIN, PROLFI,     !Output
     &  PRORTI, PROSHI, PROSTI, R30C2, RCH2O, RES30C,     !Output
     &  RFIXN, RLIG, RLIP, RMIN, RNH4C, RNO3C, ROA,       !Output
     &  RPRO, RWUEP1, RWUMX, TTFIX,                       !Output
     &  CADPV, PROSRI, STRSRFL, STRLYR1,                  !Output
     &  LFMRC, mft, MRSWITCH, RTMRC, SDMRC,SHELMRC,       !Output
     &  STMMRC, STRMRC, TRST, TRSTYP, TRSWITCH)           !Output
C-----------------------------------------------------------------------

      USE ModuleDefs     !Definitions of constructed variable types, 
        ! which contain control information, soil
        ! parameters, hourly weather data.
      IMPLICIT NONE
      EXTERNAL FIND, ERROR, GETLUN, IGNORE, UPCASE
      SAVE

!-----------------------------------------------------------------------
      CHARACTER*1  BLANK, UPCASE, DETACH
      PARAMETER (BLANK  = ' ')

      CHARACTER*2 CROP

      CHARACTER*6 SECTION, ECONO
      CHARACTER*6  ERRKEY
      PARAMETER (ERRKEY = 'IPPLNT')

      CHARACTER*12 FILEC, FILEE
      CHARACTER*30 FILEIO
      CHARACTER*80 PATHCR, CHAR, PATHEC
      CHARACTER*92 FILECC, FILEGC

      INTEGER LUNCRP, LUNIO, NOUTDO
      INTEGER PATHL, FOUND, ERR, LINC, ISECT  !, II

      REAL
     &  CADPR1, CMOBMX, FRCNOD, FREEZ1, FREEZ2,
     &  PCARSH, PCH2O, PLIPSH, PLIGSD,
     &  PLIGSH, PMINSD, PMINSH, POASD, POASH,
     &  PROLFI, PRORTI, PROSHI, PROSTI, R30C2,
     &  RCH2O, RES30C, RFIXN, RLIG, RLIP, RMIN,
     &  RNH4C, RNO3C, ROA, RPRO, TTFIX
       
      REAL PROSRI, STRSRFL, STRLYR1

      CHARACTER*1 MRSWITCH, TRSWITCH
      CHARACTER*3 TRSTYP
      REAL LFMRC, mft, RTMRC, SDMRC, SHELMRC, STMMRC,
     &   STRMRC, TRST(4)
      REAL CADPV

!     Species-dependant variables exported to SPAM or WATBAL:
      REAL EORATIO, KCAN, KEP, PORMIN, RWUMX, RWUEP1

!     The variable "CONTROL" is of constructed type "ControlType" as 
!     defined in ModuleDefs.for, and contains the following variables.
!     The components are copied into local variables for use here.
      TYPE (ControlType) CONTROL
      FILEIO = CONTROL % FILEIO
      LUNIO  = CONTROL % LUNIO

!-----------------------------------------------------------------------
!       Read data from FILEIO for use in PLANT module
!-----------------------------------------------------------------------
      OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,0)
!-----------------------------------------------------------------------
      READ(LUNIO,50) FILEC, PATHCR, FILEE, PATHEC
   50 FORMAT(/////,2(/,15X,A12,1X,A80))

!-----------------------------------------------------------------------
!    Read Experiment Details, Treatments, and Cultivars Sections
!-----------------------------------------------------------------------
      SECTION = '*EXP.D'
      CALL FIND(LUNIO, SECTION, LINC, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILEIO, LINC)
      ELSE
        READ(LUNIO,'(////,3X,A2)') CROP
      ENDIF

!-----------------------------------------------------------------------
      IF (CROP .NE. 'FA') THEN
!-----------------------------------------------------------------------
!    Read Cultivar Section
!-----------------------------------------------------------------------
        SECTION = '*CULTI'
        CALL FIND(LUNIO, SECTION, LINC, FOUND)
        IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILEIO, LINC)
        ELSE
        READ(LUNIO,'(24X,A6)') ECONO
        ENDIF

      ENDIF
      
      CLOSE (LUNIO)

!-----------------------------------------------------------------------
      IF (CROP .NE. 'FA') THEN
!-----------------------------------------------------------------------
!    Set file plus pathname for ecotype parameter file
!-----------------------------------------------------------------------
        PATHL  = INDEX(PATHEC,BLANK)
        IF (PATHL .LE. 1) THEN
        FILEGC = FILEE
        ELSE
        FILEGC = PATHEC(1:(PATHL-1)) // FILEE
        ENDIF

!-----------------------------------------------------------------------
! READ CROP PARAMETERS FROM FILEC
!-----------------------------------------------------------------------
        LINC = 0
        PATHL  = INDEX(PATHCR,BLANK)
        IF (PATHL .LE. 1) THEN
        FILECC = FILEC
        ELSE
        FILECC = PATHCR(1:(PATHL-1)) // FILEC
        ENDIF
        CALL GETLUN('FILEC', LUNCRP)
        OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,0)

C-----------------------------------------------------------------------
C READ PHOTOSYNTHESIS PARAMETERS *******************
C-----------------------------------------------------------------------
        LINC = 1
        SECTION = '!*PHOT'
        CALL FIND(LUNCRP, SECTION, LINC, FOUND)
        IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILECC, LINC)
        ELSE
        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        IF (ISECT .EQ. 0) CALL ERROR(ERRKEY,ERR,FILECC,LINC)
        READ(CHAR,'(12X,F6.2)',IOSTAT=ERR) KCAN
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LINC)
        ENDIF

C-----------------------------------------------------------------------
C READ RESPIRATION PARAMETERS **********************
C-----------------------------------------------------------------------
        SECTION = '!*RESP'
        CALL FIND(LUNCRP, SECTION, LINC, FOUND)
        IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILECC, LINC)
        ELSE
        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        READ(CHAR,'(G12.0,F6.1)',IOSTAT=ERR) RES30C, R30C2
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LINC)
  
        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        READ(CHAR,'(4F6.0)',IOSTAT=ERR) RNO3C, RNH4C, RPRO, RFIXN
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LINC)
  
        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        READ(CHAR,'(6F6.0)',IOSTAT=ERR) 
     &    RCH2O, RLIP, RLIG, ROA, RMIN, PCH2O
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LINC)

        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        READ(CHAR,'(2(5X,A1))',IOSTAT=ERR) MRSWITCH, TRSWITCH
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LINC)

        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        READ(CHAR,'(3G12.0)',IOSTAT=ERR)
     &    LFMRC, STMMRC, RTMRC 
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LINC)

        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        READ(CHAR,'(3G12.0)',IOSTAT=ERR)
     &    STRMRC, SHELMRC, SDMRC
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LINC)

        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        READ(CHAR,'(4F6.0,3X,A3,F6.0)',IOSTAT=ERR)
     &    TRST(1), TRST(2), TRST(3), TRST(4), TRSTYP, mft
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LINC)

        ENDIF
  
C-----------------------------------------------------------------------
C READ PLANT COMPOSITION PARAMETERS
C-----------------------------------------------------------------------
        SECTION = '!*PLAN'
        CALL FIND(LUNCRP, SECTION, LINC, FOUND)
        IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILECC, LINC)
        ELSE
        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        READ(CHAR,'(F6.0,12X,F6.0)',IOSTAT=ERR) PROLFI, PROSTI
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LINC)
  
        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        READ(CHAR,'(F6.0,12X,F6.0)',IOSTAT=ERR) PRORTI, PROSHI
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LINC)

        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        READ(CHAR,'(18X,F6.0)',IOSTAT=ERR) PCARSH
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LINC)
  
        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        READ(CHAR,'(18X,F6.0)',IOSTAT=ERR) PLIPSH
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LINC)
  
        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        READ(CHAR,'(18X,2F6.0)',IOSTAT=ERR) PLIGSH, PLIGSD
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LINC)

        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        READ(CHAR,'(18X,2F6.0)',IOSTAT=ERR) POASH, POASD
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LINC)
  
        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        READ(CHAR,'(18X,2F6.0)',IOSTAT=ERR) PMINSH, PMINSD
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LINC)

        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        READ(CHAR,'(F6.0)',IOSTAT=ERR) PROSRI


        ENDIF

C-----------------------------------------------------------------------
C READ CARBON AND NITROGEN MINING PARAMETERS
C-----------------------------------------------------------------------
        SECTION = '!*CARB'
        CALL FIND(LUNCRP, SECTION, LINC, FOUND)
        IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILECC, LINC)
        ELSE
        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        READ(CHAR,'(F6.0,6X,F6.0)',IOSTAT=ERR) CMOBMX, CADPR1
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LINC)
 !       ENDIF

        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)

        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)


        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        READ(CHAR,'(30X,F6.0)',IOSTAT=ERR) CADPV
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LINC)
        ENDIF



C-----------------------------------------------------------------------
C READ NITROGEN FIXATION PARAMATERS
C-----------------------------------------------------------------------
        SECTION = '!*NITR'
        CALL FIND(LUNCRP, SECTION, LINC, FOUND)
        IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILECC, LINC)
        ELSE
        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        READ(CHAR,'(18X,F6.0)',IOSTAT=ERR) TTFIX
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LINC)
        ENDIF
  
C-----------------------------------------------------------------------
C  
C     ***** READ PARTITIONING PARAMETERS *****************
C
C-----------------------------------------------------------------------
        SECTION = '!*VEGE'
        CALL FIND(LUNCRP, SECTION, LINC, FOUND)
        IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILECC, LINC)
        ELSE
        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        READ(CHAR,'(30X,F6.0)',IOSTAT=ERR) FRCNOD
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LINC)
        ENDIF

C-----------------------------------------------------------------------
C
C     ***** READ SENESCENCE PARAMETERS ******************
C       This is found in the second heading that begins with '!*LEAF'
C-----------------------------------------------------------------------
        SECTION = '!*LEAF'
        CALL FIND(LUNCRP, SECTION, LINC, FOUND)
        IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILECC, LINC)
        ENDIF
  
        CALL FIND(LUNCRP, SECTION, LINC, FOUND)
        IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILECC, LINC)
        ELSE
        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        READ(CHAR,'(18X,2F6.0)',IOSTAT=ERR) FREEZ1, FREEZ2
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LINC)
        ENDIF

!-----------------------------------------------------------------------
C         Read ROOT parameters
!-----------------------------------------------------------------------
        SECTION = '!*ROOT'
        CALL FIND(LUNCRP, SECTION, LINC, FOUND)
        IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILECC, LINC)
        ELSE
        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        IF (ISECT .EQ. 0) CALL ERROR(ERRKEY,ERR,FILECC,LINC)
        READ(CHAR,'(30X,2F6.0)',IOSTAT=ERR) RWUEP1, RWUMX
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LINC)
  
        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        IF (ISECT .EQ. 0) CALL ERROR(ERRKEY,ERR,FILECC,LINC)
        READ(CHAR,'(12X,F6.0)',IOSTAT=ERR) PORMIN
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LINC)
        ENDIF

C-----------------------------------------------------------------------
C
C     ***** READ POD DETACHMENT PARAMETERS *****
C
C-----------------------------------------------------------------------
        SECTION = '!*POD '
        CALL FIND(LUNCRP, SECTION, LINC, FOUND)
        IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILECC, LINC)
        ELSE
        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        READ(CHAR,'(5X,A1)',IOSTAT=ERR) DETACH
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LINC)
        DETACH = UPCASE(DETACH)
        ENDIF
  
C-----------------------------------------------------------------------
C
C     ***** READ EVAPOTRANSPIRATION PARAMETERS *****
C
C-----------------------------------------------------------------------
        SECTION = '!*EVAP'
        CALL FIND(LUNCRP, SECTION, LINC, FOUND)
        IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILECC, LINC)
        ELSE
        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        READ(CHAR,'(2F6.0)',IOSTAT=ERR) KEP, EORATIO
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LINC)
        ENDIF

C-----------------------------------------------------------------------
C
C     ***** READ STORAGE ORGAN PARTITIONING PARAMETERS *****
C
C-----------------------------------------------------------------------


        SECTION = '!*STOR'
        CALL FIND(LUNCRP, SECTION, LINC, FOUND)
        IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILECC, LINC)
        ELSE
        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        READ(CHAR,'(2F6.0)',IOSTAT=ERR) STRSRFL, STRLYR1
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LINC)
        ENDIF
 
        CLOSE (LUNCRP)
!-----------------------------------------------------------------------
      ENDIF
!-----------------------------------------------------------------------
!     Assign unit number for overview.out file
      CALL GETLUN('OUTO', NOUTDO)

      RETURN
      END ! SUBROUTINE IPPLNT
!=======================================================================

!-----------------------------------------------------------------------
! Variable definitions
!-----------------------------------------------------------------------
! CADPR1   Maximum fraction of stem growth after flowering that can be 
!            allocated to carbohydrate storage just before a full seed load 
!            is set. ( fraction)
! CADPV    Maximum fraction of PGAVL for vegetative growth that can be 
!            allocated to mobile carbohydrate storage under non-stress   
!            conditions during vegetative growth stages. ( fraction)
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
!            continues (°C)
! FREEZ2   Temperature below which plant growth stops completely. (°C)
! ISECT    Data record code (0 - End of file encountered, 1 - Found a good 
!            line to read, 2 - End of Section in file encountered, denoted 
!            by * in column 1
! ISWDIS   Pest damage simulation switch (Y or N) 
! ISWWAT   Water simulation control switch (Y or N) 
! LFMRC   Maintenance respiration cost for leaves (g[CH2O] / g leaf protein / hr)
! LUNCRP   Logical unit number for FILEC (*.spe file) 
! mft             Multiplier for maintenance respiration temperature factor
! MODEL    Name of CROPGRO executable file 
! MRSWITCH Parameter to select the method of calculating portion of 
!            Maintenance Respiration (MR) associated with plant mass.
!            If MRSWITCH="P" then calculate MR based on prgan protein content 
!            If MRSWITCH="M"or otherwise, calculate the mass associated portion  
!            of MR based on plant mass (original CROPGRO approach).
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
! PROSRI   Maximum protein composition in storage during growth with 
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
! RTMRC   Maintenance respiration cost for roots (g[CH2O] / g root protein / hr)
! SDMRC   Maintenance respiration cost for the portion of seed subject to 
!            maintenance respiration (g[CH2O] / g seed protein / hr)
! SHELMRC Maintenance respiration cost for shell (g[CH2O] / g shell protein / hr)
! STMMRC  Maintenance respiration cost for stem (g[CH2O] / g stem protein / hr)
! STRLYR1  Initial proportion of storage organ dry mass in soil layer 1
! STRMRC  Maintenance respiration cost for storage 
!            (g[CH2O] / g storage protein / hr)
! STRSRFL  Initial proportion of storage organ dry mass on soil surface
! TRST(4)       Maintenance respiration temperature response parameters define
!             the shape of maintenance respiration temperature response
! TRSTYP       Type of CURV function to describe maintenance respiration 
!             temperature response
! TRSWITCH Type of maintenance respiration temperature response to use
!             "M" uses McCree 1974 equation from CROPGRO, any other value
!             invokes the CURV function for a custom response
! TTFIX    Physiological days delay in nodule initiation
!            (photo-thermal days / day)
!-----------------------------------------------------------------------
!      END SUBROUTINE IPPLNT
!=======================================================================
