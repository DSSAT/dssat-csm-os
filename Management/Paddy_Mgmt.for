C=======================================================================
C  Paddy_Mgmt.for, Subroutine, J.T. Ritchie and U. Singh
C  Calculates water balance components.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/01/1989 JR  Written
!  02/26/2002 CHP Removed parts from RITCHIEF that have to do only with
!                 flooded conditions.  These are flood management and 
!                 now reside in the Management Operations module.
C=======================================================================

      SUBROUTINE PADDY_MGMT (CONTROL, ISWITCH,
     &    IRRAMT, RAIN,                                   !Input
     &    FLOOD, FLOODWAT, FLOODN)                        !Output 

!-----------------------------------------------------------------------
      USE ModuleDefs
      USE FloodModule
      IMPLICIT  NONE
      EXTERNAL INCYD, OPFLOOD
      SAVE

      INTEGER DYNAMIC, INCYD, NDAT, NDRY, NBUND
      INTEGER YRDRY, YRWET, YRDOY

      REAL ABUND, FL_DEFICIT, CEF, EF, FLOOD, FRUNOFF, IRRAMT
      REAL RAIN, RUNOFF, INFILT
      REAL TOTBUNDRO

      LOGICAL PUDDLED

!-----------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.
      TYPE (ControlType) CONTROL
      TYPE (SwitchType) ISWITCH
      TYPE (FloodWatType) FLOODWAT
      TYPE (FloodNType) FloodN

!     Transfer values from constructed data types into local variables.
      DYNAMIC = CONTROL % DYNAMIC
      YRDOY   = CONTROL % YRDOY

      NDAT = FLOODN % NDAT    !from rice phenology

      ABUND  = FLOODWAT % ABUND !mm
      EF     = FLOODWAT % EF
      INFILT = FLOODWAT % INFILT
      RUNOFF = FLOODWAT % RUNOFF
      PUDDLED= FLOODWAT % PUDDLED
      NBUND  = FLOODWAT % NBUND

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
!     Every season set flood variables back to zero (even for sequenced)
      CEF       = 0.0 !Cumulative flood evaporation
      EF        = 0.0
      FLOOD     = 0.0 !Flood depth
      FRUNOFF   = 0.0
      TOTBUNDRO = 0.0 !Cumulative flow over bund
      YRDRY = -99     !Day on which dry conditions exist after flooded.
      YRWET = -99     !Day on which flooded conditions exist after dry.
      NDRY  = 0       !Number of dry days since transplant (?)

      IF (NBUND .GT. 0) THEN
        CALL OpFlood(CONTROL, ISWITCH, 
     &     ABUND, EF, FLOOD, FRUNOFF, INFILT, IRRAMT, RAIN, RUNOFF)
      ENDIF

      FLOODWAT % CEF       = CEF
      FLOODWAT % EF        = EF
      FLOODWAT % FLOOD     = FLOOD      !mm
      FLOODWAT % FRUNOFF   = FRUNOFF
      FLOODWAT % TOTBUNDRO = TOTBUNDRO
      FLOODWAT % YRDRY     = YRDRY
      FLOODWAT % YRWET     = YRWET

!***********************************************************************
!***********************************************************************
!     Rate calculations
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!-----------------------------------------------------------------------
!     PUDDLED calcs. from old POROSITY routine.  Note:  this section not
!         called yet.
      IF (FLOOD .GT. 0.0 .AND. NDAT .LE. 1) THEN
         NDRY = 0
      ENDIF

      IF (FLOOD .LE. 0.0 .AND. NDAT .GT. 0) THEN
         NDRY = NDRY + 1
      ENDIF

      IF (PUDDLED .AND. NDRY .GT. 12) THEN
         PUDDLED = .FALSE.
!        Note: change PLOWPAN here?
      ENDIF

      FLOODWAT % PUDDLED = PUDDLED

!***********************************************************************
!***********************************************************************
!     DAILY INTEGRATION
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------
!     Started out flooded at beginning of day
      IF (FLOOD .GT. 0.0) THEN
        FLOOD = FLOOD + RAIN + IRRAMT - INFILT - EF

        IF (FLOOD .LE. 0.001) THEN
          !Deficit of flood water - set flood to zero.
          !Evaporation will be reduced in SPAM.
          FL_DEFICIT = -FLOOD
          FLOOD   = 0.0
          EF = MAX(0.0, EF - FL_DEFICIT)

          !Trigger for drying processes - increment by 1 day so it 
          !  will work in tomorrow's rate section of soil processes.
          YRDRY = INCYD(YRDOY,1)  
        ENDIF
        
!     Started out dry at beginning of day
      ELSE
        FLOOD = FLOOD + RAIN + IRRAMT - INFILT
        FLOOD = MAX (0.0, FLOOD)

!       See if flood conditions now exist.
        IF (FLOOD .GT. 0.001) THEN
          !Trigger for new flood event - increment by 1 day so it will 
          !  work in tomorrow's rate section of soil processes.
          YRWET = INCYD(YRDOY,1)  
        ELSE
          FLOOD = 0.0
        ENDIF
      ENDIF

      IF (FLOOD .GT. ABUND) THEN
        !Surplus of flood water - flow over bund
        FRUNOFF = FLOOD - ABUND
        FLOOD   = ABUND
      ELSE
        FRUNOFF = 0.0
      ENDIF

      TOTBUNDRO = TOTBUNDRO + FRUNOFF
      CEF       = CEF       + EF

      FLOODWAT % EF         = EF
      FLOODWAT % FLOOD      = FLOOD
      FLOODWAT % YRDRY      = YRDRY
      FLOODWAT % YRWET      = YRWET
      FLOODWAT % FRUNOFF    = FRUNOFF
      FLOODWAT % TOTBUNDRO  = TOTBUNDRO
      !FLOODWAT % FL_DEFICIT = FL_DEFICIT
      FLOODWAT % CEF        = CEF

!***********************************************************************
!***********************************************************************
!     OUTPUT/SEASEND - Daily & Seasonal output
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT .OR. DYNAMIC .EQ. SEASEND) THEN
C-----------------------------------------------------------------------
      IF (NBUND .GT. 0) THEN
        CALL OpFlood(CONTROL, ISWITCH, 
     &     ABUND, EF, FLOOD, FRUNOFF, INFILT, IRRAMT, RAIN, RUNOFF)
      ENDIF

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!-----------------------------------------------------------------------

      RETURN
      END SUBROUTINE PADDY_MGMT
C=======================================================================

