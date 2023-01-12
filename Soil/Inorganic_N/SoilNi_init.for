C***********************************************************************
C  SoilNi_init, Subroutine
C
C  Purpose: Do soil N initialization.
C  This was modified from SOILNI when NTRANS was split into organic and 
C  inorganic sections.  SOILNI was split into SOILNI_inorganic and 
C  SOILNI_organic.
C-----------------------------------------------------------------------
C  REVISION HISTORY 
C  02/08/1993 PWW Header revision and minor changes.
C  02/20/1996 GH  Written.
C  02/26/1998 WTB Fixed HUMC/HUMN calculations.
C  06/09/1999 AJG Completely revised the soil N and SOM module, and made
C               a new SOM module based on the CENTURY model.
C               Also changed variable names:
C         OLD         NEW       
C         -------     -------   
C         TFY         TFNITY          
C         WRESR       ICRT, TRTRES
C  06/21/1999 CHP Modular format
C  03/16/2000 GH  Incorporated in CROPGRO
C  06/11/2002 GH  Modified for Y2K
C  08/12/2003 CHP Added I/O error checking
C                 No re-initialization done for sequenced runs
!  01/14/2005 CHP/UPS Split NTRANS into separate organic and 
!                  inorganic routines.
C-----------------------------------------------------------------------
C  Called : SOILN_inorg
C  Calls  : ERROR, FIND
C=======================================================================

      SUBROUTINE SoilNi_init(CONTROL, ISWNIT,
     &    SOILPROP, ST,                                   !Input
     &    NH4, NO3, SNH4, SNO3, TFNITY, UPPM, UREA)       !Output
      
!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT  NONE
      EXTERNAL ERROR, FIND
      SAVE

      CHARACTER*1 RNMODE, ISWNIT
      CHARACTER*6 ERRKEY, SECTION, DUMMY
      CHARACTER*30 FILEIO
      PARAMETER (ERRKEY = 'SOILNI')

      INTEGER ERRNUM, FOUND, L, LNUM
      INTEGER LUNIO, NLAYR
      INTEGER RUN

      REAL KG2PPM(NL)
      REAL NH4(NL), NO3(NL)
      REAL SNH4(NL)
      REAL SNO3(NL), ST(NL), TFNITY(NL) 
      REAL UREA(NL), UPPM(NL)

!-----------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.

!     Constructed variables are defined in ModuleDefs.
      TYPE (ControlType) CONTROL
      TYPE (SoilType)    SOILPROP

!     Transfer values from constructed data types into local variables.
      FILEIO  = CONTROL % FILEIO
      LUNIO   = CONTROL % LUNIO
      RNMODE  = CONTROL % RNMODE
      RUN     = CONTROL % RUN

      KG2PPM = SOILPROP % KG2PPM    
      NLAYR  = SOILPROP % NLAYR  

!***********************************************************************
      IF (RUN .EQ. 1 .OR. INDEX('QF',RNMODE) .LE. 0) THEN
        IF (INDEX(ISWNIT,'N') < 1) THEN
!         --------------------------------------------------------------
!         Open the FILEIO input file.
          OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT = ERRNUM)
          IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY, ERRNUM, FILEIO, 0)
!         --------------------------------------------------------------
!         Find and Read INITIAL CONDITIONS Section.
!         --------------------------------------------------------------
          SECTION = '*INITI'
          CALL FIND (LUNIO, SECTION, LNUM, FOUND)
          IF (FOUND .EQ. 0) CALL ERROR (SECTION, 42, FILEIO, LNUM)
        
!         Read line before layer data 
          READ (LUNIO, '(A6)', IOSTAT = ERRNUM) DUMMY
          LNUM = LNUM + 1
          IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY, ERRNUM, FILEIO, LNUM)
        
          DO L = 1, NLAYR
            LNUM = LNUM + 1
            READ(LUNIO, 100, IOSTAT=ERRNUM) NH4(L),NO3(L)
100         FORMAT (14X, 2 (1X, F5.1))
            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY, ERRNUM, FILEIO, LNUM)
          ENDDO
          CLOSE (LUNIO)
        ELSE
!         Set some default concentrations, needed for ORYZA even when no N simulated
          NO3 = 0.1
          NH4 = 0.1
          UPPM = 0.0
        ENDIF

!       !Non-sequenced runs
        DO L = 1, NLAYR
!         --------------------------------------------------------------
!         Calculate yesterday's soil temperature factor. When
!         calculating the nitrification, TFNITY will be compared with
!         today's soil temperature factor, and the maximum will apply.
          IF (ST(L) .LT. 5.0) THEN
            TFNITY(L) = 0.0
          ELSE
            TFNITY(L) = 0.0009766 * ST(L) * ST(L)
          ENDIF

!         --------------------------------------------------------------
!         Initialize soil mineral nitrogen and urea.
!         --------------------------------------------------------------
!         Limit NO3 and NH4 to >=0.01.
!          NO3(L) = AMAX1 (NO3(L), 0.01)
!          NH4(L) = AMAX1 (NH4(L), 0.01)
          NO3(L) = AMAX1 (NO3(L), 1.E-6)
          NH4(L) = AMAX1 (NH4(L), 1.E-6)

!         Convert the N concentrations to kg[N] / ha per soil layer.
          SNO3(L) = NO3(L) / KG2PPM(L)
          SNH4(L) = NH4(L) / KG2PPM(L)

!         Initialize urea.
          UREA(L) = 0.0
        END DO   !End of soil layer loop.
      ENDIF  !End of RUN if-construct

!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE SoilNi_init

!=======================================================================
! SOILNI Variables
!
! ERRKEY     Subroutine name for error file 
! ERRNUM     Error number for input 
! FILEIO     Filename for input file (e.g., IBSNAT35.INP) 
! FOUND      Indicator that good data was read from file by subroutine FIND 
!              (0 - End-of-file encountered, 1 - NAME was found) 
! KG2PPM(L)  Conversion factor to switch from kg [N] / ha to µg [N] / g 
!              [soil] for soil layer L 
! LNUM       Current line number of input file 
! LUNIO      Logical unit number for FILEIO 
! NH4(L)     Ammonium N in soil layer L (µg[N] / g[soil])
! NL         Maximum number of soil layers = 20 
! NLAYR      Actual number of soil layers 
! NO3(L)     Nitrate in soil layer L (µg[N] / g[soil])
! SECTION    Section name in input file 
! SNH4(L)    Total extractable ammonium N in soil layer L (kg [N] / ha)
! SNO3(L)    Total extractable nitrate N in soil layer L (kg [N] / ha)
! ST(L)      Soil temperature in soil layer L (°C)
! SW(L)      Volumetric soil water content in layer L
!              (cm3 [water] / cm3 [soil])
! TFNITY(L)  Yesterday’s soil temperature factor for nitrification (range 
!              0-1) 
! UREA(L)    Amount of urea in soil layer L (kg [N] / ha)
!=======================================================================
