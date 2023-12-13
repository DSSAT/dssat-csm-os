C=======================================================================
C  CSP_NUPTAK, Subroutine for CASUPRO sugarcane model, based on 
C  NUPTAK, Subroutine (adapted from CERES)
C  Determines N uptake 
C-----------------------------------------------------------------------
C  Revision history
C  09/01/89 JWJ,GH Written
C  03/01/93 WTB Modified.
C  01/20/97 GH  Modified.
C  07/10/98 CHP modified for modular format.
C  05/11/98 GH  Incorporated in CROPGRO
C  11/08/01 O.H. Daza modified for the sugarcane model
C  08/27/2003 FSR Incorporated in CASUPRO for DSSAT 4.0
C-----------------------------------------------------------------------
C  Called from:  CASUPRO
C  Calls:        ERROR, FIND, IGNORE
C=======================================================================
!OHD - This subroutine is esentially equal to NUPTAK except for the 
!      variable NDMSDR that was discarded from the input list and other 
!      statements that were modified below

      SUBROUTINE CSP_NUPTAK(DYNAMIC,
     &    DLAYR, DUL, KG2PPM, FILECC, LL, NDMTOT,         !Input
     &    NH4, NLAYR, NO3, RLV, SAT, SW,                  !Input
     &    TRNH4U, TRNO3U, TRNU, UNH4, UNO3)               !Output

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      EXTERNAL ERROR, GETLUN, FIND, IGNORE
      SAVE

      CHARACTER*10 ERRKEY
      PARAMETER (ERRKEY = 'CSP_NUPTAK')
      CHARACTER*6 SECTION
      CHARACTER*80 CHAR
      CHARACTER*92 FILECC

      INTEGER LUNCRP, ERR, LNUM, ISECT, FOUND
      INTEGER L, NLAYR, DYNAMIC !, WLUN
      
      REAL NUF, XMIN
      REAL DLAYR(NL), LL(NL), DUL(NL), SAT(NL), SW(NL), RLV(NL)
      REAL SNO3(NL), SNH4(NL), KG2PPM(NL), NO3(NL), NH4(NL)
      REAL RNO3U(NL), RNH4U(NL), UNO3(NL), UNH4(NL)
      REAL TRNO3U, TRNH4U, TRNU
      REAL NDMTOT,         ANDEM, FNH4, FNO3, SMDFR, RFAC
      REAL RTNO3, RTNH4, MXNH4U, MXNO3U

!      INTEGER OpenStatus

!***********************************************************************
!***********************************************************************
!     Run Initialization - Called once per simulation
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
!-----------------------------------------------------------------------
!     ***** READ ROOT GROWTH PARAMETERS *****************
!-----------------------------------------------------------------------
!     Read in values from input file, which were previously input
!       in Subroutine IPCROP.
!-----------------------------------------------------------------------
      CALL GETLUN('FILEC', LUNCRP)      
      OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,0)

!-----------------------------------------------------------------------
!     Subroutine FIND finds appropriate SECTION in a file by
!     searching for the specified 6-character string at beginning
!     of each line.
!-----------------------------------------------------------------------
      SECTION = '*#ROOT'
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
		CALL ERROR(SECTION, 42, FILECC, LNUM)
	ELSE		
		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)

		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		READ(CHAR,'(13X,F6.2)',IOSTAT=ERR) RTNO3 
 		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

		CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
		READ(CHAR,'(13X,F6.2)',IOSTAT=ERR) RTNH4
		IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

      ENDIF

      CLOSE (LUNCRP)
!***********************************************************************
!OHD - Echoes input data

! Open file to write results from NUPTAK_SC
!      CALL GETLUN('WORK.OUT',WLUN)
!      OPEN(UNIT = WLUN, FILE = "WORK.OUT", STATUS = "UNKNOWN",
!     &   ACTION = "WRITE", POSITION = "APPEND", IOSTAT = OpenStatus)

!      WRITE(WLUN,'(1X, "")')   !chp for portability
!      WRITE(WLUN,'(1X, "")')
!      WRITE(WLUN,'(1X,"RESULTS FROM NUPTAK_SC.for")')
!      WRITE(WLUN,'(1X,"--------------------------")')
!      WRITE(WLUN,'(1X, "")')
!      WRITE(WLUN,'(1X,"*** FILECC : ",A80)') FILECC
!      WRITE(WLUN,'(1X, "")')
!      WRITE(WLUN,'(1X,"SECTION: ",A6)') SECTION
!	WRITE(WLUN,'(1X,"RTNO3  : ",F6.3)') RTNO3
!      WRITE(WLUN,'(1X,"RTNH4  : ",F6.3)') RTNH4
!      WRITE(WLUN,'(1X, "")')
!      WRITE(WLUN,'(1X,"END RESULTS FROM NUPTAK_SC.for")')

!      CLOSE (WLUN)

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      TRNO3U = 0.0              !
      TRNH4U = 0.0              !Moved from INPLNT
      TRNU   = 0.0              !

      DO L = 1, NLAYR
        UNH4(L)  = 0.0
        UNO3(L)  = 0.0
      ENDDO

!***********************************************************************
!***********************************************************************
!     DAILY RATE/INTEGRATION
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------
C   Initialize variables
C-----------------------------------------------------------------------
      TRNU   = 0.0
      TRNO3U = 0.0
      TRNH4U = 0.0
      NUF    = 0.0
      XMIN   = 0.0
      DO L=1,NLAYR
        RNO3U(L) = 0.0
        RNH4U(L) = 0.0
        UNH4(L)  = 0.0
        UNO3(L)  = 0.0
        !KG2PPM(L) = 10. / (BD(L) * DLAYR(L))
        SNO3(L) = NO3(L) / KG2PPM(L)
        SNH4(L) = NH4(L) / KG2PPM(L)
      ENDDO
C-----------------------------------------------------------------------
C   Determine crop N demand (kg N/ha), after subtracting mobilized N
!OHD - Changes for CASUPRO in this section
C-----------------------------------------------------------------------
!      ANDEM = (NDMTOT - NDMSDR) * 10.0

! ANDEM    Total crop N demand (g [N] / m2 - d) SHOULD BE (kg [N] / ha - d)
! NDMTOT   Total N demand (g [N] / m2 - d)

	ANDEM = NDMTOT * 10.0  ! What is the factor 10? CHECK IT OUT!
      IF (ANDEM .GT. 0.0) THEN
C-----------------------------------------------------------------------
C   Calculate potential N uptake in soil layers with roots
C-----------------------------------------------------------------------
        DO L=1,NLAYR
          IF (RLV(L) .GT. 0.0) THEN
            FNH4 = 1.0 - EXP(-0.08 * NH4(L))
            FNO3 = 1.0 - EXP(-0.08 * NO3(L))
            IF (FNO3 .LT. 0.04) FNO3 = 0.0  
            IF (FNO3 .GT. 1.0)  FNO3 = 1.0
            IF (FNH4 .LT. 0.04) FNH4 = 0.0  
            IF (FNH4 .GT. 1.0)  FNH4 = 1.0

            SMDFR = (SW(L) - LL(L)) / (DUL(L) - LL(L))
            IF (SMDFR .LT. 0.0) THEN
              SMDFR = 0.0
            ENDIF

            IF (SW(L) .GT. DUL(L)) THEN
              SMDFR = 1.0 - (SW(L) - DUL(L)) / (SAT(L) - DUL(L))
            ENDIF
            RFAC = RLV(L) * SMDFR * SMDFR * DLAYR(L) * 100.0
C-----------------------------------------------------------------------
C  RLV = Rootlength density (cm/cm3); SMDFR = relative drought factor
C  DLAYR = Layer depth (cm)
C  RTNO3 + RTNH4 = Nitrogen uptake / root length (mg N/cm)
C  RNO3U + RNH4  = Nitrogen uptake (kg N/ha)
C-----------------------------------------------------------------------
            RNO3U(L) = RFAC * FNO3 * RTNO3
            RNH4U(L) = RFAC * FNH4 * RTNH4
            RNO3U(L) = MAX(0.0,RNO3U(L))
            RNH4U(L) = MAX(0.0,RNH4U(L))
            TRNU = TRNU + RNO3U(L) + RNH4U(L) !kg[N]/ha
          ENDIF
        ENDDO
C-----------------------------------------------------------------------
C   Calculate N uptake in soil layers with roots based on demand (kg/ha)
C-----------------------------------------------------------------------
        IF (ANDEM .GT. TRNU) THEN
          ANDEM = TRNU
        ENDIF

        IF (TRNU .GT. 0.0) THEN
          NUF = ANDEM / TRNU
          DO L=1,NLAYR
            IF (RLV(L) .GT. 0.0) THEN
              UNO3(L) = RNO3U(L) * NUF
              UNH4(L) = RNH4U(L) * NUF
              XMIN    = 0.25 / KG2PPM(L)
              MXNO3U  = MAX(0.0,(SNO3(L) - XMIN))
              IF (UNO3(L) .GT. MXNO3U) THEN
                UNO3(L) = MXNO3U
              ENDIF
              XMIN = 0.5 / KG2PPM(L)
              MXNH4U  = MAX(0.0,(SNH4(L) - XMIN))
              IF (UNH4(L) .GT. MXNH4U) UNH4(L) = MXNH4U
              TRNO3U  = TRNO3U + UNO3(L)
              TRNH4U  = TRNH4U + UNH4(L)
            ENDIF
          ENDDO
C-----------------------------------------------------------------------
C   Convert uptake to g/m^2
C-----------------------------------------------------------------------
          TRNO3U = TRNO3U / 10.0
          TRNH4U = TRNH4U / 10.0
          TRNU   = TRNO3U + TRNH4U
C-----------------------------------------------------------------------
        ENDIF
      ENDIF

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END ! SUBROUTINE CSP_NUPTAK
C=======================================================================

!-----------------------------------------------------------------------
!       Variable definitions
!-----------------------------------------------------------------------
! ANDEM    Total crop N demand (kg[N]/ha)
! CHAR     Contains the contents of last record read 
! DLAYR(L) Soil thickness in layer L (cm)
! DUL(L)   Volumetric soil water content at Drained Upper Limit in soil 
!            layer L (cm3 [H2O] /cm3 [soil])
! DYNAMIC  Module control variable; =RUNINIT, SEASINIT, RATE, EMERG, 
!            INTEGR, OUTPUT, or FINAL 
! ERR      Error code for file operation 
! ERRKEY   Subroutine name for error file 
! FILECC   Path plus filename for species file (*.spe) 
! FNH4     Potential NH4 availability factor 
! FNO3     Potential NO3 availability factor 
! KG2PPM(L) Conversion factor to switch from kg [N] / ha to ug [N] / g 
!            [soil] for soil layer L 
! LL(L)    Volumetric soil water content in soil layer L at lower limit
!            ( cm3/cm3)
! LUNCRP   Logical unit number for FILEC (*.spe file) 
! MXNH4U   Maximum NH4 uptake from soil (kg N/ha)
! MXNO3U   Maximum NO3 uptake from soil (kg N/ha)
! NDMSDR   Amount of Mobilized N which can be used for seed growth
!            (g[N] / m2 / d)
! NDMTOT   Total N demand (g[N] / m2 / d)
! NH4(L)   Ammonium N in soil layer L (µg[N] / g[soil])
! NL       maximum number of soil layers = 20 
! NLAYR    Number of soil layers 
! NO3(L)   Nitrate in soil layer L (µg[N] / g[soil])
! NUF      N uptake fraction (ratio of demand to N uptake), <= 1.0 
! RFAC     Nitrogen uptake conversion factor ((kg N/ha) / (mg N / cm root))
! RLV(L)   Root length density for soil layer L ((cm root / cm3 soil))
! RNH4U(L) Ammonium uptake (kg N/ha)
! RNO3U(L) Nitrate uptake (kg N/ha)
! RTNH4    Ammonium uptake per unit root length (mg N / cm)
! RTNO3    Nitrate uptake per unit root length (mg N / cm)
! SAT(L)   Volumetric soil water content in layer L at saturation
!            (cm3 [water] / cm3 [soil])
! SMDFR    Relative drought factor 
! SNH4(L)  Total extractable ammonium N in soil layer L (kg [N] / ha)
! SNO3(L)  Total extractable nitrate N in soil layer L (kg [N] / ha)
! SW(L)    Volumetric soil water content in layer L
!            (cm3 [water] / cm3 [soil])
! TRNH4U   Total N uptake in ammonium form in a day (g[N] / m2 / d)
! TRNO3U   Total N uptake in nitrate form in a day (g[N] / m2 / d)
! TRNU     Total N uptake in a day (kg[N] / ha / d)
! UNH4     Uptake of NH4 from soil (interim value) (kg N/ha)
! UNO3     Uptake of NO3 from soil (interim value) (kg N/ha)
! XMIN     Amount of NH4 that cannot be immobilized but stays behind in 
!            soil as NH4; Also, Amount of NO3 that cannot denitrify but 
!            stays behind in the soil as NO3 (kg [N] / ha)
!-----------------------------------------------------------------------
!       END SUBROUTINE CSP_NUPTAK
!=======================================================================
