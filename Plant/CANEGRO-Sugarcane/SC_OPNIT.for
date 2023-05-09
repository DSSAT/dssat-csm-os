!=======================================================================
!  SC_OPNIT, Subroutine
!-----------------------------------------------------------------------
!  Generates output file for daily plant nitrogen variables
!     This routine is used for sugarcane (CANEGRO).
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  01/01/1990     Written
!  09/21/1998 CHP Split off from OPDAY.for file
!  05/11/1999 GH  Incorporated in CROPGRO
!  12/18/2001 WDB Revised for modular CERES
!  08/20/2002 GH  Modified for Y2K
!  04/21/2023 HBD Copied/Taken from MZ_OPNIT.for
!                 Adapted for CANEGRO N implemented by MvdL
!                 Commented csv-related codes
!-----------------------------------------------------------------------
!  Called by: SC_NITRO
!  Calls:     None
!=======================================================================
      SUBROUTINE SC_OPNIT(CONTROL, ISWITCH, 
     &    YRPLT, NLAYR, SENESCE,
!     &    WTNCAN,WTNSD,WTNVEG,PCNGRN,PCNVEG,
!     &    WTNUP,WTNLF,WTNST,PCNL,PCNST,PCNRT
! Exporting what MvdL used to export for his 2011 paper
!     &    Control%YRDOY,
     &    MASSES,
     &    N_POOL,
!     &    N_ALLOC,
     &    N_STRESS,
!     &    PSV_UPTAKE, ! FO has commented it in SC_NITRO
!     &    N_SENESCE, ! check if this is = to SENESCE % ResE(n,1) (HBD)
     &    N_CONC,
!     &    TOT_N_DEMAND, POT_N_SUPPLY, is it necessary to be exported? (HBD)
!     &    TUNO3, ! FO has commented it in SC_NITRO; check necessity here (HBD)
     &    ABVGRND_N_MASS, 
!     &    TOT_N_POOL, !FO commented it out variable not used
     &    ACC_UPTAKE) 

!-----------------------------------------------------------------------
      USE ModuleDefs     
      USE CsvOutput   
      ! HBD: had to leave it here because of statements using 'FMOPT')

!     Define CANEGRO composite variables:
      USE CNG_ModuleDefs
!     Use the composite variable defn:
      USE N_TYPES

      IMPLICIT NONE
      EXTERNAL GETLUN, HEADER, TIMDIF, YR_DOY
      SAVE

      INTEGER DAS, DYNAMIC, MDATE, NOUTDN, YRSIM

      CHARACTER*1  IDETG, IDETN, RNMODE
      CHARACTER*6, PARAMETER :: ERRKEY = 'SC_OPN' !TODO
      CHARACTER*12 OUTPN

      INTEGER DAP, DOY, ERRNUM, FROP, L, NLAYR, RUN
      INTEGER TIMDIF, YEAR, YRDOY, YRPLT

!      REAL PCNL
!      REAL WTNCAN,WTNSD,WTNVEG,PCNGRN,PCNVEG
!      REAL WTNUP,WTNLF,WTNST,PCNST,PCNRT     
      REAL CUMSENSURFN, CUMSENSOILN   !cumul. senes. N soil and surface

      LOGICAL FEXIST, FIRST

!     The variable "CONTROL" is of type "ControlType".
      TYPE (ControlType) CONTROL

!     The variable "ISWITCH" is of type "SwitchType".
      TYPE (SwitchType) ISWITCH
!      TYPE (SOILTYPE) SoilProp
      TYPE (ResidueType) SENESCE
!      TYPE (N_PARAM_TYPE) N_PARAMS(NUM_COMPS)
      
!     CANEGRO N variables
!     MASSES: Dry masses of each plant component, 
!     stored in array form to facilitate
!     processing in loops
!     Units:  t/ha
      REAL MASSES(NUM_COMPS)
!     N_POOL: An array of Nitrogen masses for each plant
!     component.  This is updated in the integration
!     section each day.
!     Units:  kg/ha
      REAL N_POOL(NUM_COMPS)
      !REAL TOP_N
!     Allocation of N to each plant component today (kg/day)
      !REAL N_ALLOC(NUM_COMPS)
      !REAL TOT_N_ALLOC
!     N_STRESS: An array of N-stresses for each plant 
!     component.  These are used to reduce
!     various processes (photosynthesis, mainly)
!     Units:  None
      REAL N_STRESS !TODO check if this is enough (HBD)
!     Passive N-uptake (mass flow and remobilised N)
!      REAL PSV_UPTAKE ! FO has commented it in SC_NITRO
!     N_SENESCE: mass of Nitrogen remobilised from senesced
!     leaves (trash).  This is distributed together with N_UPTAKE
!	(Going to add this to the storage pool)
      !REAL N_SENESCE
!     N_CONC: An array of N-concentration for each plant
!     component.
!     Units:  kg/kg(/ha)
      REAL N_CONC(NUM_COMPS)
!     TOT_N_DEMAND: Total N-demand, kg/ha/day:
      !REAL TOT_N_DEMAND
!     Potential NO3 and NH4 uptake
      !REAL POT_N_SUPPLY
!     FO has commented TUNO3 in SC_NITRO
      !REAL TUNO3 !, TUNH4
      REAL ABVGRND_N_MASS 
      REAL ACC_UPTAKE
!      REAL TOT_N_POOL

      IDETG   = ISWITCH % IDETG
      IDETN   = ISWITCH % IDETN
      IF (IDETG == 'N' .OR. IDETN == 'N') RETURN

      DAS     = CONTROL % DAS
      DYNAMIC = CONTROL % DYNAMIC
      FROP    = CONTROL % FROP
      RUN     = CONTROL % RUN
      RNMODE  = CONTROL % RNMODE
      YRDOY   = CONTROL % YRDOY
      YRSIM   = CONTROL % YRSIM
      FMOPT   = ISWITCH % FMOPT   ! VSH

!-----------------------------------------------------------------------
      IF(DYNAMIC .EQ. RUNINIT) THEN

          OUTPN = 'PlantN.OUT'
          CALL GETLUN('OUTPN', NOUTDN)

      

!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. SEASINIT) THEN
            IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN    ! VSH
      !     Initialize daily plant nitrogen output file
            INQUIRE (FILE = OUTPN, EXIST = FEXIST)
            IF (FEXIST) THEN
              OPEN (UNIT = NOUTDN, FILE = OUTPN, STATUS = 'OLD',
     &        IOSTAT = ERRNUM, POSITION = 'APPEND')
              FIRST = .FALSE.  
            ELSE
              OPEN (UNIT = NOUTDN, FILE = OUTPN, STATUS = 'NEW',
     &        IOSTAT = ERRNUM)
              WRITE(NOUTDN,'("*PLANT N OUTPUT FILE")')
              FIRST = .TRUE.  
            ENDIF

            CALL HEADER(SEASINIT, NOUTDN, RUN)
            WRITE (NOUTDN,230)
  230       FORMAT('@YEAR DOY   DAS   DAP',
     &        '   CNAD',
     &        '   SNAD   LNAD   RNAD',!   VNAD',
!     &        '  LDNAD', ! total N
     &        '   NUPC',
     &        '   SN%D   LN%D   RN%D',!   VN%D
!     &        '  LDN%D')!, ! total Nconc
     &        '   NSTD')
!     &        '  SNN0C   SNN1C')
            END IF
            
            !cumul. senes. N soil and surface
            CUMSENSURFN = 0.0
            CUMSENSOILN = 0.0   

!***********************************************************************
!***********************************************************************
!     DAILY OUTPUT
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. RATE) THEN
!         Calculate cumulative N senesced
          CUMSENSURFN = CUMSENSURFN + SENESCE % ResE(0,1) 
          DO L = 1, NLAYR
            CUMSENSOILN = CUMSENSOILN + SENESCE % ResE(L,1)
          ENDDO

c         Re-Calculate N-concentrations for each component for outputs:
!         If ratoon has a root system, it will keep outputing its [N]
!         print*,'before: ',N_CONC(STALKS)*100
c         ::::::::::::::::::::::::::::::::::::::::::::::
c         Conc = N_pool/mass, if mass < 0
            IF (MASSES(TOPS) < 0.00001) THEN
                N_CONC(TOPS) = 0.00001   
		ENDIF
		IF (MASSES(STALKS) < 0.00001) THEN
                N_CONC(STALKS) = 0.00001
		ENDIF
		IF (MASSES(ROOTS) < 0.00001) THEN
                N_CONC(ROOTS) = 0.00001
		ENDIF
!         print*,'after: ',N_CONC(STALKS)*100
      !-----------------------------------------------------------------
      !   CHECK FOR OUTPUT FREQUENCY
      !-----------------------------------------------------------------
        IF (YRDOY .GE. YRPLT .AND. YRPLT .GT. 0)
     &          THEN

          DAP = MAX(0, TIMDIF(YRPLT, YRDOY))

          IF ((MOD(DAS,FROP) .EQ. 0)     !Daily output every FROP days,
     &      .OR. (YRDOY .EQ. YRPLT)         !on planting date, and
     &      .OR. (YRDOY .EQ. MDATE)) THEN   !at harvest maturity 

            CALL YR_DOY(YRDOY, YEAR, DOY)
            
            IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN ! VSH

              WRITE (NOUTDN,300) YEAR, DOY, DAS, DAP,
     &         ABVGRND_N_MASS,
     &         N_POOL(STALKS), N_POOL(TOPS), N_POOL(ROOTS),
!     &         N_POOL(DEADLF), !add total N
     &         ACC_UPTAKE,
     &         N_CONC(STALKS)*100, N_CONC(TOPS)*100, N_CONC(ROOTS)*100,
!add abg Nconc  !add total Nconc
!     &         N_CONC(DEADLF)*100!,
!              HBD Apr 2023 move to plantgro.out in future
     &         1.0-N_STRESS 
!     &         CUMSENSURFN, CUMSENSOILN

  300         FORMAT (1X,I4,1X,I3.3,2(1X,I5),
     &         4(1X,F6.1),
     &         1(1X,F6.1),
     &         4(1X,F6.1))!,
!     &         1(1X,F6.2), 2F8.2)
            END IF    
          ENDIF
        ENDIF
      

!***********************************************************************
!***********************************************************************
!     SEASEND
!***********************************************************************
      ELSE IF ((DYNAMIC .EQ. SEASEND)
     & .AND. (FMOPT == 'A' .OR. FMOPT == ' ')) THEN
!-----------------------------------------------------------------------
        CLOSE (NOUTDN)

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE SC_OPNIT
!=======================================================================