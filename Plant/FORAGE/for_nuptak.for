C=======================================================================
C  FOR_NUPTAK, Subroutine
C  Determines N uptake (adapted from CERES)
C-----------------------------------------------------------------------
C  Revision history
C  09/01/89 JWJ,GH Written
C  03/01/93 WTB Modified.
C  01/20/97 GH  Modified.
C  07/10/98 CHP modified for modular format.
C  05/11/98 GH  Incorporated in CROPGRO
C  10/17/03 SJR Added code to prevent cost of N uptake and reduction
C                    from exceeding PGAVL
C-----------------------------------------------------------------------
C  Called from:  PLANT
C  Calls:        ERROR, FIND, IGNORE
C=======================================================================

      SUBROUTINE FOR_NUPTAK(
     &  BD, DLAYR, DUL, FILECC, LL, NDMSDR, NDMTOT, NH4,  !Input
     &  NO3, NLAYR, PGAVL, RLV, RNH4C, RNO3C, SAT, SW,    !Input
     &  NUPNH4, NUPNO3, TRNH4U, TRNO3U, TRNU, TSNMOB,     !Output
     &  UNH4, UNO3,                                       !Output
     &  DYNAMIC)                                          !Control

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
        ! which contain control information, soil
        ! parameters, hourly weather data.
      IMPLICIT NONE
      EXTERNAL GETLUN, FIND, ERROR, IGNORE
      SAVE

      CHARACTER*6 ERRKEY
      PARAMETER (ERRKEY = 'NUPTAK')
      CHARACTER*6 SECTION
      CHARACTER*80 CHAR
      CHARACTER*92 FILECC

      INTEGER I, LUNCRP, ERR, LNUM, ISECT, FOUND
      INTEGER L, NLAYR, DYNAMIC

      REAL NUF, XMIN
      REAL BD(NL), DLAYR(NL), LL(NL), DUL(NL), SAT(NL), SW(NL), RLV(NL)
      REAL SNO3(NL), SNH4(NL), FAC(NL), NO3(NL), NH4(NL)
      REAL RNO3U(NL), RNH4U(NL), UNO3(NL), UNH4(NL)
      REAL TRNO3U, TRNH4U, TRNU
      REAL NDMTOT, NDMSDR, ANDEM, FNH4, FNO3, SMDFR, RFAC
      REAL RTNO3, RTNH4, MXNH4U, MXNO3U
      REAL PGAVL, PRSPNH4, PRSPNO3, PTRNH4U, PTRNO3U, RNH4C, RNO3C
      REAL NUPNH4(NL), NUPNO3(NL)
      REAL TSNMOB
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
!    Find and Read Photosynthesis Section
!-----------------------------------------------------------------------
!     Subroutine FIND finds appropriate SECTION in a file by
!     searching for the specified 6-character string at beginning
!     of each line.
!-----------------------------------------------------------------------
      LNUM = 1
      SECTION = '!*ROOT'
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILECC, LNUM)
      ELSE
        DO I = 1, 3
        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        IF (ISECT .EQ. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
        ENDDO
        READ(CHAR,'(2F6.0)') RTNO3, RTNH4
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
      ENDIF

      CLOSE (LUNCRP)

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
      PTRNO3U  = 0.0
      PTRNH4U  = 0.0


      DO L=1,NLAYR
        RNO3U(L) = 0.0
        RNH4U(L) = 0.0
        UNH4(L)  = 0.0
        UNO3(L)  = 0.0
        FAC(L) = 10. / (BD(L) * DLAYR(L))
        SNO3(L) = NO3(L) / FAC(L)
        SNH4(L) = NH4(L) / FAC(L)
      ENDDO
C-----------------------------------------------------------------------
C   Determine crop N demand (kg N/ha), after subtracting mobilized N
C-----------------------------------------------------------------------
      ANDEM = (NDMTOT - MAX (TSNMOB,NDMSDR)) * 10.0
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

        SMDFR = 2.0*((SW(L) - LL(L)) / (DUL(L) - LL(L)))
        IF (SMDFR .LT. 0.0) THEN
        SMDFR = 0.0
        ENDIF

        IF (SW(L) .GT. DUL(L)) THEN
        SMDFR = 2.0*(1.0 - (SW(L) - DUL(L)) / (SAT(L) - DUL(L)))
        ENDIF
        SMDFR    = AMAX1 (SMDFR,0.0)
        SMDFR    = AMIN1 (SMDFR,1.0)

        RFAC = RLV(L) * SMDFR * SMDFR * DLAYR(L) * 100.0
C-----------------------------------------------------------------------          
!C-----------------------------------------------------------------------
!C   Calculate potential N uptake in soil layers with roots
!C-----------------------------------------------------------------------
!        DO L=1,NLAYR
!        IF (RLV(L) .GT. 0.0) THEN
!        FNH4 = 1.0 - EXP(-0.08 * NH4(L))
!        FNO3 = 1.0 - EXP(-0.08 * NO3(L))
!        IF (FNO3 .LT. 0.04) FNO3 = 0.0  
!        IF (FNO3 .GT. 1.0)  FNO3 = 1.0
!        IF (FNH4 .LT. 0.04) FNH4 = 0.0  
!        IF (FNH4 .GT. 1.0)  FNH4 = 1.0
!        
!        SMDFR = 2.0*((SW(L) - LL(L)) / (DUL(L) - LL(L)))
!        IF (SMDFR .LT. 0.0) THEN
!        SMDFR = 0.0
!        ENDIF
!        
!        IF (SW(L) .GT. DUL(L)) THEN
!        SMDFR = 2.0*(1.0 - (SW(L) - DUL(L)) / (SAT(L) - DUL(L)))
!        ENDIF
!        SMDFR    = AMAX1 (SMDFR,0.0)
!        SMDFR    = AMIN1 (SMDFR,1.0)
!        
!        RFAC = RLV(L) * SMDFR * DLAYR(L) * 100.0
!         ENDIF
!      END DO
!            
!            
!C-----------------------------------------------------------------------
C  RLV = Rootlength density (cm/cm3);SMDFR = relative drought factor
C  DLAYR = Layer depth (cm)
C  RTNO3 + RTNH4 = Nitrogen uptake / root length (mg N/cm)
C  RNO3U + RNH4  = Nitrogen uptake (kg N/ha)
C-----------------------------------------------------------------------
        RNO3U(L) = RFAC * FNO3 * RTNO3
        RNH4U(L) = RFAC * FNH4 * RTNH4
C-----------------------------------------------------------------------
C      11/2/05 SJR Copied code limiting potential N uptake from the 
C            "actual" uptake code below.  This prevents the situation where
C            TRNU islarger than ANDEM, so TRNU is limited to TRNU but ends 
C            up short of ANDEM because one or more layers is limited by 
C            MXNO3U or MXNH4U.
C-----------------------------------------------------------------------


        XMIN    = 0.25 / FAC(L)
        MXNO3U  = MAX(0.0,(SNO3(L) - XMIN))
        IF (RNO3U(L) .GT. MXNO3U) THEN
        RNO3U(L) = MXNO3U
        ENDIF
        XMIN = 0.5 / FAC(L)
        MXNH4U  = MAX(0.0,(SNH4(L) - XMIN))
        IF (RNH4U(L) .GT. MXNH4U) RNH4U(L) = MXNH4U
        
        RNO3U(L) = MAX(0.0,RNO3U(L))
        RNH4U(L) = MAX(0.0,RNH4U(L))
        TRNU = TRNU + RNO3U(L) + RNH4U(L) !kg[N]/ha
C-----------------------------------------------------------------------
C     SJR 10/17/03 - Calculate potential total NO3 and NH4 uptake
C-----------------------------------------------------------------------
        PTRNO3U  = PTRNO3U + RNO3U(L)
        PTRNH4U  = PTRNH4U + RNH4U(L)

        ENDIF
        ENDDO
C-----------------------------------------------------------------------
C   Calculate N uptake in soil layers with roots based on demand (kg/ha)
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
C     SJR 10/17/03 - Calculate cost of uptake and reduction of CP 
C      from potential NO3 and NH4
C-----------------------------------------------------------------------

        PRSPNO3 = (PTRNO3U/10)/0.16 * RNO3C  
        PRSPNH4 = (PTRNH4U/10)/0.16 * RNH4C  

C-----------------------------------------------------------------------
C     SJR 10/17/03 - Check that cost of uptake and reduction of CP 
C      from TRNU does not exceed PGAVL
C-----------------------------------------------------------------------

        IF (PGAVL .LT. (PRSPNO3 + PRSPNH4)) THEN
        TRNU = (PGAVL / (PRSPNO3+PRSPNH4)) * TRNU
        ENDIF


        IF (ANDEM .GT. TRNU) THEN
        ANDEM = TRNU
        ENDIF
!        IF (TRNU .EQ. 0.0) GO TO 600
        IF (TRNU .GT. 0.0) THEN
C-----------------------------------------------------------------------
C      10/31/05 SJR Replace TRNU with (PRSPNO3 + PRSPNH4) to prevent 
C            excessive N-uptake when PGAVL limits TRNU to less than
C            (PRSPNO3 + PRSPNH4), hence yielding a false high proportion
C             of available N to be taken up - contributes to NLEAK.
C-----------------------------------------------------------------------
!          NUF = ANDEM / TRNU
        NUF = ANDEM / (PTRNO3U + PTRNH4U)
        DO L=1,NLAYR
        IF (RLV(L) .GT. 0.0) THEN
        UNO3(L) = RNO3U(L) * NUF
        UNH4(L) = RNH4U(L) * NUF
        XMIN    = 0.25 / FAC(L)
        MXNO3U  = MAX(0.0,(SNO3(L) - XMIN))
        IF (UNO3(L) .GT. MXNO3U) THEN
        UNO3(L) = MXNO3U
        ENDIF
        XMIN = 0.5 / FAC(L)
        MXNH4U  = MAX(0.0,(SNH4(L) - XMIN))
        IF (UNH4(L) .GT. MXNH4U) UNH4(L) = MXNH4U
        TRNO3U  = TRNO3U + UNO3(L)
        TRNH4U  = TRNH4U + UNH4(L)
        ENDIF
        ENDDO

C-----------------------------------------------------------------------
C      Calculate proportion of TRNU coming from NO3 and NH4 in each layer
C      for use in "returning" NLEAK
C-----------------------------------------------------------------------
        DO L=1,NLAYR
        IF (TRNO3U .GT. 0.0) THEN
        NUPNO3(L) = UNO3(L) / TRNO3U
        ENDIF

        IF (TRNH4U .GT. 0.0) THEN
        NUPNH4(L) = UNH4(L) / TRNH4U
!        NUPNH4(L) = UNH4(L) / TRNO3U !kjb fixed 7/12/2017
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
      END ! SUBROUTINE FOR_NUPTAK
C=======================================================================

!-----------------------------------------------------------------------
!       Variable definitions
!-----------------------------------------------------------------------
! ANDEM    Total crop N demand (g[N] / m2 / d)
! CHAR     Contains the contents of last record read 
! DLAYR(L) Soil Depth in layer L (cm)
! DUL(L)   Volumetric soil water content at Drained Upper Limit in soil 
!            layer L (cm3 [H2O] /cm3 [soil])
! DYNAMIC  Module control variable; =RUNINIT, SEASINIT, RATE, EMERG, 
!            INTEGR, OUTPUT, or SEASEND 
! ERR      Error code for file operation 
! ERRKEY   Subroutine name for error file 
! FAC(L)   Conversion factor to switch from kg [N] / ha to ug [N] / g 
!            [soil] for soil layer L 
! FILECC   Path plus filename for species file (*.spe) 
! FNH4     Potential NH4 availability factor 
! FNO3     Potential NO3 availability factor 
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
! NUPNO3(L)      Proportion of TRNU from a soil layer that is nitrate
! NUPNH4(L)      Proportion of TRNU from a soil layer that is ammonium
! PGAVL    Total available CH2O available for growth & respiration
!           (g[CH2O] / m2)
! PRSPNO3       Respiration cost to fix and reduce all potentially available NO3 
!             g CH2O/M2 to CP
! PRSPNH4       Respiration cost to fix and reduce all potentially available NH4 
!             g CH2O/M2 to CP
! PTRNO3U  Total potential NO3 uptake
!             (kg [N] / ha)
! PTRNH4U  Total potential NH4 uptake
!             (kg [N] / ha)
! RFAC     Nitrogen uptake conversion factor ((kg N/ha) / (mg N / cm root))
! RLV(L)   Root length density for soil layer L ((cm root / cm3 soil))
! RNH4C     CH2O required for protein synthesis when source of N is 
!             ammonium uptake (g[CH2O] / g[protein])
! RNH4U(L) Ammonium uptake (kg N/ha)
! RNO3C     Respiration required for reducing NO3 to protein
!             (g[CH2O] / g[protein])
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
! TRNU     Total N uptake in a day (g[N] / m2 / d)
! UNH4     Uptake of NH4 from soil (interim value) (kg N/ha)
! UNO3     Uptake of NO3 from soil (interim value) (kg N/ha)
! XMIN     Amount of NH4 that cannot be immobilized but stays behind in 
!            soil as NH4; Also, Amount of NO3 that cannot denitrify but 
!            stays behind in the soil as NO3 (kg [N] / ha)
!-----------------------------------------------------------------------
!       END SUBROUTINE FOR_NUPTAK
!=======================================================================
