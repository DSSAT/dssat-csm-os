!=======================================================================
!  OR_OPGROW, Subroutine
!
!  Generates output for simulated data
!-----------------------------------------------------------------------
!  Revision history
!  05/26/2011 chp adapted for ORYZA2000
!=======================================================================

      SUBROUTINE OR_OPGROW (CONTROL, ISWITCH, SOILPROP,  &
         CPEW, DVS, HU, LAI, LDSTRS, LESTRS, LRSTRS,     &
         NFLV, NGR, RNSTRS, PCEW, RDCL,               &
         WAGT, WLVD, WLVG, WRR, WRT, WSO, WST, YRPLT, ZRT, &
         NACR, ANRT, ANLV, ANSO, ANST, ANLD, SNN1C)

!-----------------------------------------------------------------------
      USE ModuleDefs
      IMPLICIT  NONE
      SAVE

      CHARACTER*8   CHAR8
      CHARACTER*30  LayerText
      CHARACTER*220 GROHEAD(4), NITHEAD

      INTEGER DAP, DAS, YRDOY, YRPLT

      CHARACTER*1 IDETG, ISWNIT
      CHARACTER*12 OUTG, OUTPN
      INTEGER DYNAMIC, NOUTDG, FROP, I, J, L, NLAYR
      INTEGER MDATE, NOUTPN, RUN, ERRNUM, TIMDIF, YEAR, DOY

      REAL CPEW, DVS, HU, LAI, LDSTRS, LESTRS, LRSTRS,     &
         NFLV, RNSTRS, PCEW,                                &
         WAGT, WLVD, WLVG, WRR, WRT, WSO, WST, ZRT 
      REAL, DIMENSION(10) :: RDCL

      REAL NGR, HIAD, HWUD, SLAD, SEEDNO, LeafNPct
      REAL CUMSENSURF, CUMSENSOIL 
      REAL NACR, ANRT, ANLV, ANSO, ANST, ANLD, VNAD, CNAD, SNN1C
      Real RNpctD, VNpctD, SNpctD, LNpctD

      LOGICAL FEXIST, FIRST

      TYPE (ResidueType) SENESCE
      TYPE (SoilType) SOILPROP

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      TYPE (ControlType) CONTROL
      TYPE (SwitchType) ISWITCH

      DAS     = CONTROL % DAS
      DYNAMIC = CONTROL % DYNAMIC
      YRDOY   = CONTROL % YRDOY

!***********************************************************************
!***********************************************************************
!     Seasonal Initialization - Called once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
!     Transfer values from constructed data types into local variables.
      IDETG  = ISWITCH % IDETG
      IF (IDETG .NE. 'Y') RETURN
      ISWNIT = ISWITCH % ISWNIT
      FROP    = CONTROL % FROP
      RUN     = CONTROL % RUN

      IF (IDETG .EQ. 'Y') THEN
        OUTG  = 'PlantGro.OUT'
        CALL GETLUN('OUTG',  NOUTDG)
        OUTPN  = 'PlantN.OUT  '
        CALL GETLUN('OUTPN', NOUTPN)

!       Text describing soil layer depths
        LayerText = "                              "
        DO L = 1, MIN(5,NLAYR)
          SELECT CASE (L)
          CASE (1:4);CHAR8 = SoilProp % LayerText(L)
!         LayerText(11) is for layers 5 thru NLAYR
          CASE (5);  CHAR8 = SoilProp % LayerText(11)
          END SELECT
          I = (L-1)*6+1
          J = L*6
          LayerText(I:J) = CHAR8(3:8)
        ENDDO

      GROHEAD(1) = '!YR        Days  Days        Leaf  <------------- Dry Weight -------------> Grain Grain        <--------- Stress (0-1) --------->  Leaf   Spec  Root<Senesced Mat>   Phot'                                                   
      GROHEAD(2) = '!  and    after after  Grow  Area  <---------------- kg/Ha --------------->   per    wt  Harv  <--------- Water ---------->         Nit   Leaf Depth <--(kg/ha)-->  Therm'                                                   
      GROHEAD(3) = '!     DOY start plant Stage Index  Leaf  Leaf  Stem Grain  Root Panic  Crop    m2     g  Indx  Phot  Grow Trans  Roll Death  Nitr    %    Area    m    Surf   Soil   Days'                                                   
      GROHEAD(4) = '@YEAR DOY   DAS   DAP  GSTD  LAID  LWAD  LDAD  SWAD  GWAD  RWAD  EWAD  CWAD  G#AD  HWUD  HIAD  WSPD  WSGD  WFTD  WFRD  WFDD  NSTD  LN%D   SLAD  RDPD  SNW0C  SNW1C   DTTD'                                                   

!!-----------------------------------------------------------------------
      NITHEAD = '@YEAR DOY   DAS   DAP  CNAD  VNAD  VN%D  NUPC  LNAD  SNAD  LN%D  SN%D  RN%D  SNN0C  SNN1C'

! Comparing DSSAT PlantN.out, remove GNAD, GN%D
!-----------------------------------------------------------------------
!       Initialize daily growth output file
        INQUIRE (FILE = OUTG, EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT = NOUTDG, FILE = OUTG, STATUS = 'OLD',IOSTAT = ERRNUM, POSITION = 'APPEND')
          FIRST = .FALSE.  
        ELSE
          OPEN (UNIT = NOUTDG, FILE = OUTG, STATUS = 'NEW',IOSTAT = ERRNUM)
          WRITE(NOUTDG,'("*GROWTH ASPECTS OUTPUT FILE")')
          FIRST = .TRUE.  
        ENDIF

        !Write headers
        CALL HEADER(SEASINIT, NOUTDG, RUN)

!       Variable heading for GROWTH.OUT
        WRITE (NOUTDG,2192) GROHEAD(1)
        WRITE (NOUTDG,2192) GROHEAD(2)
        WRITE (NOUTDG,2192) GROHEAD(3)
        WRITE (NOUTDG,2192) GROHEAD(4)
 2192   FORMAT (A220)

!-----------------------------------------------------------------------
!     Initialize daily plant nitrogen output file
        IF (ISWNIT .EQ. 'Y') THEN
          INQUIRE (FILE = OUTPN, EXIST = FEXIST)
          IF (FEXIST) THEN
            OPEN (UNIT = NOUTPN, FILE = OUTPN, STATUS = 'OLD',  & 
              IOSTAT = ERRNUM, POSITION = 'APPEND')
            FIRST = .FALSE.  
          ELSE
            OPEN (UNIT = NOUTPN, FILE = OUTPN, STATUS = 'NEW',  &
              IOSTAT = ERRNUM)
            WRITE(NOUTPN,'("*PLANT N OUTPUT FILE")')
            FIRST = .TRUE.  
          ENDIF

          !Write headers
          CALL HEADER(SEASINIT, NOUTPN, RUN)
          WRITE (NOUTPN,2240) NITHEAD
 2240     FORMAT (A140)
        ENDIF
      ENDIF

!-----------------------------------------------------------------------

      CUMSENSURF  = 0.0
      CUMSENSOIL  = 0.0

!      WTNUP = 0.0
!      WTNST    = 0.0 
!      WTNSD    = 0.0 
!      WTNSH    = 0.0 
!      WTNRT    = 0.0 

!      CUMSENSURFN = 0.0
!      CUMSENSOILN = 0.0   

!***********************************************************************
!***********************************************************************
!     Daily OUTPUT
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT) THEN
      IF (IDETG .NE. 'Y') RETURN
      IF (YRDOY .LT. YRPLT .AND. YRPLT .GT. 0) RETURN

!     Accumulate senesced matter for surface and soil.
      DO L = 1, 10
        SENESCE % ResWt(L) = RDCL(L)
        CUMSENSOIL  = CUMSENSOIL  + RDCL(L)
      ENDDO

!-----------------------------------------------------------------------
!     Check for output frequency
      IF ((MOD(DAS,FROP) .EQ. 0)    &      !Daily output every FROP days,
       .OR. (YRDOY .EQ. YRPLT)      &      !on planting date, and
       .OR. (YRDOY .EQ. MDATE)) THEN       !at harvest maturity 

!-----------------------------------------------------------------------
!        The following generates output for PlantGro.OUT
!-----------------------------------------------------------------------
!        LFWT = WTLF / PLTPOP  !do we need this?

        SEEDNO  = NGR / 1.E4    !#/m2 

        IF (NGR > 0.0) THEN
          HWUD = WRR / NGR * 1.E3  !g/grain
        ELSE
          HWUD = 0.0
        ENDIF

        IF (WAGT > 0.0 .AND. WRR > 0.0) THEN
          HIAD = WRR / WAGT
        ELSE
          HIAD = 0.0
        ENDIF 

        IF (WLVG > 1.E-6) THEN
          SLAD = LAI / WLVG * 1.E5  !cm2/g
        ELSE
          SLAD = 0.0
        ENDIF

        DAP = MAX(0,TIMDIF(YRPLT,YRDOY))
        IF (DAP > DAS) DAP = 0
        CALL YR_DOY(YRDOY, YEAR, DOY)

        LeafNPct = NFLV * LAI  / WLVG * 1000.

        WRITE (NOUTDG,400) YEAR, DOY, DAS, DAP,                    &
          DVS, LAI, NINT(WLVG), NINT(WLVD), NINT(WST), NINT(WRR),  &
          NINT(WRT), NINT(WSO), NINT(WAGT), NINT(SEEDNO), HWUD, HIAD, &
          1.0 - PCEW, 1.0 - LESTRS, 1.0 - CPEW, 1.0 - LRSTRS, 1.0 - LDSTRS, 1.0 - RNSTRS,               &
          LeafNPct, SLAD, ZRT, NINT(WLVD), NINT(CUMSENSOIL), HU                        
 400    FORMAT (1X,I4, 1X,I3.3, 2(1X,I5), &
             1X,F5.3, 1X,F5.2, 4(1X,I5),  &
             4(1X,I5), 1X,F5.1, F6.3,     &
             6F6.3,                       &
             F6.2, F7.1, F6.2, 2I7, F7.2)

!!-----------------------------------------------------------------------
!!        The following generates output for file PlantN.OUT
!!-----------------------------------------------------------------------
        if (WRT .GT. 0.0) RNpctD =100. * ANRT / WRT
        if (WLVG .GT. 0.0) LNpctD = 100. * ANLV / WLVG ! in % where WLVG is Dry weight of green leaves
        CNAD = ANSO + ANLV + ANST + ANLD ! Storage N + leaves N + stems N + dead lieves N
        VNAD = ANST + ANLV
        if ((WSO + WLVG) .GT. 0.0) VNpctD = 100.*VNAD / (WSO + WLVG)
        if (WST .GT. 0.0) SNpctD = 100. * ANST / WST
!
        IF (ISWNIT .EQ. 'Y') THEN

          WRITE (NOUTPN,310)YEAR, DOY, DAS, DAP,             &
          ! CNAD  VNAD   VN%D   NUPC  LNAD  SNAD  
            CNAD, VNAD, VNpctD, NACR, ANLV, ANST,   &
          ! LN%D    SN%D          RN%D    SNN0C  SNN1C
            LNpctD, SNpctD,       RNpctD,  ANLD, SNN1C  
!
!!       ADDED RNAD (= NRT KG/HA)
!
  310     FORMAT (1X,I4,1X,I3.3,2(1X,I5),  &
             3(1X,F5.1),2(1X,F5.2),1X,F5.1,  &
             2(1X,F5.1),1X,F5.2,2(1X,F6.2))
        ENDIF

      ENDIF

!***********************************************************************
!***********************************************************************
!     Seasonal OUTPUT
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASEND) THEN
!***********************************************************************
        CLOSE (NOUTDG)
!        CLOSE (NOUTPN)

!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE OR_OPGROW
!=======================================================================
!=======================================================================
!=====================================================================
!     OR_OPGROW VARIABLE DEFINITIONS:
!-----------------------------------------------------------------------
! ANRT    Nitrogen in root  in kg N/ha
!         Equivalent to DSSAT RN%D in %
! ANLV    Nitrogen in leaves in kg N/ha
!         Equivalent to DSSAT LNAD in kg/ha
! ANLD    N in dead leaf (kg[N]/ha) (only for W limited)
!         Equivalent to DSSAT SNN0C (Cumulative senesced N to surface (kg[N]/ha) )
! ANSO    N in storage + grain (where grain not used here) in kg[N]/ha
! ANST    Nitrogen in stems in kg N/ha (it is not calculate in interface)
!         Equivalent to DSSAT SNAD
! CNAD    DSSAT Crop(Tops, or biomass) N in kg/ha
! LNpctD  DSSAT Leaf N concentration in %
! NACR    Actual N uptake by crop in kg N/ha/d
!         Equivalent to DSSAT NUPC in kg/ha
! RNpctD  DSSAT Root N concentration in %
! SNpctD  DSSAT steam N concentration in %
! SNN1C   Cumulative Senes Soil N  in kg/ha
! VNAD    DSSAT Veg (stem+leaf) N in kg/ha
! VNpctD  DSSAT Veg (stem+leaf) N concentration in %
! WLVD    Dead leaf biomass in kg/ha
! WLVG    Dry weight of green leaves (or total leaf biomass) in kg/ha
! WRT     Total root dry weight in kg/ha
! WSO     Dry weight of storage in kg/ha
! WST     Dry weight of stems in kg/ha
!-----------------------------------------------------------------------
!     END SUBROUTINE OR_OPGROW
!============================================= ==========================
