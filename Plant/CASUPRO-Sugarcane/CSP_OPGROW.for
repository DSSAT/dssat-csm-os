C=======================================================================
C  CSP_OPGROW, Subroutine for CASUPRO sugarcane model, based on 
C  OPGROW, Subroutine, G. Hoogenboom, J.W. Jones
C-----------------------------------------------------------------------
C  Generates output file for daily growth variables
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  01/01/1990 GH  Written
C  09/21/1998 CHP Split off from OPDAY.for file
C  05/11/1999 GH  Incorporated in CROPGRO
C  06/19/2001 GH  Modified output format
C  11/11/2001 O.H. Daza modified for sugarcane
C  08/20/2002 GH  Modified for Y2K
C  07/08/2003 CHP Changed senescence output.
C  08/27/2003 FSR Incorporated in CASUPRO for DSSAT 4.0
C  07/26/2004 CHP Removed variables which were not being used
!  07/24/2007 CHP Removed stalk printout temporarily.  GBuild can't
!                     handle this many columns.  Maybe split to 
!                     multiple files?
!  10/20/2008 FSR Redesigned PlantGro.out to match CaneGro output file
!                 former PlantGro.out will be PlantKg.out 
C-----------------------------------------------------------------------
C  Called by: CASUPRO
C  Calls:     None
!=======================================================================
      SUBROUTINE CSP_OPGROW(CONTROL, ISWITCH,                     !Input
     &  BRDMD, CADLF, CADST, CANHT, CANWH, CMINEA,                !Input
     &  CountStalk, DLAYR, EOS, GROWTH, GRWRES,                   !Input
     &  Kill, LeafArea, LeafArealost,LeafNum, LFWTHa,             !Input
     &  LFWT, LFWTHalost, LSWTlost, LSWT, LSWTHa,                 !Input
     &  LSWTHalost, LFWTlost, LITOTAL,                            !Input
     &  MAINR, MDATE, NFIXN, NLAYR, NSTRES, PSTRES1, PSTRES2,     !Input
     &  PCLSD,PCCSD, PCNL, PCNRT, PCNSD, PCNSH, PCNST,            !Input
     &  PGT, RHOL, RHOS, RLV, RTDEP,                              !Input
     &  RTWTHa, SATFAC, SENESCE, SLAREF, Smax, StalkPopul,        !Input
     &  STKFWTHa, STKFWT, STKWT, STKWTHa,                         !Input
     &  STKWTHalost, SumTTD, SumTTG, SWFAC, SUWT, SUWTHa,         !Input
     &  TGRO, TGROAV, TOTWT, TURFAC, WTNCAN, WTNLF, WTNST,        !Input
     &  WTNSD, WTNUP, WTNFX, XLAI, XLAIlost, YRPLT, PhenoStage,   !Input
     &  BADMD)                                                   !Output
!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      EXTERNAL GETLUN, HEADER, TIMDIF, YR_DOY
      SAVE
!-----------------------------------------------------------------------
      CHARACTER*1  IDETG, ISWPHO
      CHARACTER*2  CROP
      CHARACTER*10, PARAMETER :: ERRKEY = 'CSP_OPGROW'
!     CHARACTER*8  FNAME
      CHARACTER*10 SKIP_STR, VLEN_STR, WIDTH_STR
      CHARACTER*12 OUTG, OUTPC, OUTPN, OUTKG
      CHARACTER*15 GROHEAD(4, 50)
      CHARACTER*15 GRO_OUT(50)
!     CHARACTER*20 FILEIO, OFILE
      CHARACTER*100 G_FMT_STR, D_FMT_STR ! Strings for outputting 
	                                   !  comments and daily values 
      CHARACTER*1024 FMT_STR, T_FMT_STR  ! Runtime format strings

      INTEGER CountStalk, DAP, DAS, DOY, DYNAMIC, ERRNUM, FROP, I, J, L
      INTEGER MDATE, NLAYR, NOUTDG, NOUTKG, NOUTPC, NOUTPN, NUM_OVARS  
      INTEGER PhenoStage, OpenStatus, RUN, SKIP, Smax, Stalk, TIMDIF   
	INTEGER VAR_WIDTH, VLEN, YEAR, YRDOY, YRPLT, YRSIM, PSLUN

	INTEGER TLEN, FLEN, ENDI ! String lengths

      INTEGER, DIMENSION(1:NumOfStalks):: Kill

      REAL BADMD, BRDMD, CADLF, CADST, CANHT, CANWH, CMINEA 
      REAL EOS, GROWTH, GRWRES, HID, HIF, LITOTAL  !, LAITD
      REAL MAINR, NSTRES, PCLSD, PCCSD, PCNL, PGT  
      REAL RHOL, RHOS, RTDEP  
      REAL SLAREF
      REAL SATFAC, SMDMD, SMFMD, SUID, SumTTD, SumTTG !, SUDMD, SUFMD
      REAL SWFAC, TGROAV, TOTWT, TRLV, TURFAC, XLAI, XLAIlost 
	    !TOPWT, VSTAGE 

      REAL PCCSDP, PCLSDP, PCNLP, PCNRTP, PCNSDP
      REAL PCNSHP, PCNSTP, RHOLP, RHOSP, SLAP

      REAL DLAYR(NL), RLV(NL)
      REAL TGRO(TS)

      REAL WTNCAN,WTNLF,WTNST,WTNSD,WTNUP,WTNFX
      REAL WTNVEG,PCNVEG,NFIXN
      REAL PCNST,PCNRT,PCNSH,PCNSD

      REAL CUMSENSURF,  CUMSENSOIL  !cumul. senesced matter, soil & surf
      REAL CUMSENSURFN, CUMSENSOILN !cumul. senes. N soil and surface

      REAL, DIMENSION(0:NumOfDays) ::LFWTHa, LSWTHa, LFWTHalost,
     &             LSWTHalost, RTWTHa, StalkPopul, STKFWTHa, 
     &             STKWTHa, STKWTHalost, SUWTHa

      REAL, DIMENSION(0:NumOfDays,NumOfStalks) ::LeafArea,LeafArealost, 
     &               LeafNum, LFWT, LFWTlost, LSWTlost, LSWT, 
     &               STKFWT, STKWT, SUWT
	 
      LOGICAL FEXIST, FIRST

!     Added for P module
      REAL PStres1, PStres2
!-----------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.

!     The variable "CONTROL" is of type "ControlType".
      TYPE (ControlType) CONTROL
!     TYPE (SoilType) SOILPROP         !CHP not actually used yet

!     The variable "ISWITCH" is of type "SwitchType".
      TYPE (SwitchType) ISWITCH
      TYPE (ResidueType) SENESCE

!----------------------------------------------------------------------
c             DATA (for headings) 
!----------------------------------------------------------------------
c      Headings for output variables:
c      ::::::::::::::::::::::::::::::
c      Date:
           DATA GROHEAD(1, 1) /'!DATE'/
           DATA GROHEAD(2, 1) /'! '/
           DATA GROHEAD(3, 1) /'!YEAR'/
           DATA GROHEAD(4, 1) /'@YEAR'/

c      Day of year:
           DATA GROHEAD(1, 2) /'Day '/
           DATA GROHEAD(2, 2) /'of'/
           DATA GROHEAD(3, 2) /'year'/
           DATA GROHEAD(4, 2) /'DOY'/

c      Days after 'simulation start' (I think)
           DATA GROHEAD(1, 3) /'Days'/
           DATA GROHEAD(2, 3) /'after'/
           DATA GROHEAD(3, 3) /'sim.start'/
           DATA GROHEAD(4, 3) /'DAS'/

c      Days after 'planting' (/harvest?)
           DATA GROHEAD(1, 4) /'Days'/
           DATA GROHEAD(2, 4) /'after'/
           DATA GROHEAD(3, 4) /'plant'/
           DATA GROHEAD(4, 4) /'DAP'/

c      Number of leaf tips 
           DATA GROHEAD(1, 5) /'Primary'/
           DATA GROHEAD(2, 5) /'Leaf-tip'/
           DATA GROHEAD(3, 5) /'number'/
           DATA GROHEAD(4, 5) /'L#S1D'/

c      Growth stage
           DATA GROHEAD(1, 6) /'Growth'/
           DATA GROHEAD(2, 6) /'stage'/
           DATA GROHEAD(3, 6) /'number'/
           DATA GROHEAD(4, 6) /'GSTD'/

c      Leaf area index
           DATA GROHEAD(1, 7) /'Leaf'/
           DATA GROHEAD(2, 7) /'area'/
           DATA GROHEAD(3, 7) /'index'/
           DATA GROHEAD(4, 7) /'LAIGD'/

c      Dry mass of green leaves + meristem
           DATA GROHEAD(1, 8) /'Green'/
           DATA GROHEAD(2, 8) /'leaf'/
           DATA GROHEAD(3, 8) /'DM (t/ha)'/
           DATA GROHEAD(4, 8) /'LGDMD'/

c      Dry mass of green leaf sheaths
           DATA GROHEAD(1, 9) /'Green'/
           DATA GROHEAD(2, 9) /'leaf sheath'/
           DATA GROHEAD(3, 9) /'DM (t/ha)'/
           DATA GROHEAD(4, 9) /'SGDMD'/

c      Stalk fresh mass
           DATA GROHEAD(1, 10) /'Stalk'/
           DATA GROHEAD(2, 10) /'fresh'/
           DATA GROHEAD(3, 10) /'mass (t/ha)'/
           DATA GROHEAD(4, 10) /'SMFMD'/

c      Stalk dry mass
           DATA GROHEAD(1, 11) /'Stalk dry'/
           DATA GROHEAD(2, 11) /'mass with'/
           DATA GROHEAD(3, 11) /'sucro(t/ha)'/
           DATA GROHEAD(4, 11) /'SMDMD'/

c      Sucrose dry mass
           DATA GROHEAD(1, 12) /'Sucrose'/
           DATA GROHEAD(2, 12) /'mass'/
           DATA GROHEAD(3, 12) /'(t/ha)'/
           DATA GROHEAD(4, 12) /'SUCMD'/

c      Root dry mass
           DATA GROHEAD(1, 13) /'Root'/
           DATA GROHEAD(2, 13) /'Dry mass'/
           DATA GROHEAD(3, 13) /'t/ha'/
           DATA GROHEAD(4, 13) /'RDMD'/

c      Aerial (above-ground) biomass
           DATA GROHEAD(1, 14) /'Aboveground'/
           DATA GROHEAD(2, 14) /'live + dead'/
           DATA GROHEAD(3, 14) /'DM (t/ha)'/
           DATA GROHEAD(4, 14) /'BADMD'/

c     Stalks per ha	     
           DATA GROHEAD(1, 15) /'Stalks'/
           DATA GROHEAD(2, 15) /'per'/
           DATA GROHEAD(3, 15) /'m2'/
           DATA GROHEAD(4, 15) /'T#AD'/

c     Stalks per plant	     
           DATA GROHEAD(1, 16) /'Stalks'/
           DATA GROHEAD(2, 16) /'per'/
           DATA GROHEAD(3, 16) /'Plant'/
           DATA GROHEAD(4, 16) /'T#PD'/

c     Harvest index (t sucrose / t above ground biomass)
           DATA GROHEAD(1, 17) /'Harvest'/
           DATA GROHEAD(2, 17) /'Index'/
           DATA GROHEAD(3, 17) /'HI (t/t)'/
           DATA GROHEAD(4, 17) /'SUID'/

c     Trash dry mass
           DATA GROHEAD(1, 18) /'Trash-dead'/
           DATA GROHEAD(2, 18) /'leaf+sheath'/
           DATA GROHEAD(3, 18) /'DM (t/ha)'/
           DATA GROHEAD(4, 18) /'LDDMD'/

c     Total (living + dead) leaf area index
           DATA GROHEAD(1, 19) /'live'/
           DATA GROHEAD(2, 19) /'+ dead leaf'/
           DATA GROHEAD(3, 19) /'area index'/
           DATA GROHEAD(4, 19) /'LAITD'/

c     Water stress affecting photosynthesis
           DATA GROHEAD(1, 20) /'Phot.'/
           DATA GROHEAD(2, 20) /'water'/
           DATA GROHEAD(3, 20) /'Stress(0-1)'/
           DATA GROHEAD(4, 20) /'WSPD'/

c     Water stress affecting growth
           DATA GROHEAD(1, 21) /'Growth'/
           DATA GROHEAD(2, 21) /'Water'/
           DATA GROHEAD(3, 21) /'stress'/
           DATA GROHEAD(4, 21) /'WSGD'/

c     Senesced tiller (stalk) dry mass
           DATA GROHEAD(1, 22) /'Dead'/
           DATA GROHEAD(2, 22) /'Stalk'/
           DATA GROHEAD(3, 22) /'DM (t/ha)'/
           DATA GROHEAD(4, 22) /'SDDMD'/


c     Stalk height (metres)  
           DATA GROHEAD(1, 23) /'Stalk'/
           DATA GROHEAD(2, 23) /'Height'/
           DATA GROHEAD(3, 23) /'m'/
           DATA GROHEAD(4, 23) /'SHTD'/

c     Root depth (m)
           DATA GROHEAD(1, 24) /'Root'/
           DATA GROHEAD(2, 24) /'Depth'/
           DATA GROHEAD(3, 24) /'m'/
           DATA GROHEAD(4, 24) /'RDPD'/

c     Root length density (cm3/cm3)
c     Layer 1
           DATA GROHEAD(1, 25) /'Root_Len.'/
           DATA GROHEAD(2, 25) /'density(1)'/
           DATA GROHEAD(3, 25) /'cm3/cm3'/
           DATA GROHEAD(4, 25) /'RL1D'/

c     Root length density (cm3/cm3)
c     Layer 2
           DATA GROHEAD(1, 26) /'Root_Len.'/
           DATA GROHEAD(2, 26) /'density(2)'/
           DATA GROHEAD(3, 26) /'cm/cm3'/
           DATA GROHEAD(4, 26) /'RL2D'/
c     Root length density (cm3/cm3)
c     Layer 3
           DATA GROHEAD(1, 27) /'Root_Len.'/
           DATA GROHEAD(2, 27) /'density(3)'/
           DATA GROHEAD(3, 27) /'cm/cm3'/
           DATA GROHEAD(4, 27) /'RL3D'/
c     Root length density (cm3/cm3)
c     Layer 4
           DATA GROHEAD(1, 28) /'Root_Len.'/
           DATA GROHEAD(2, 28) /'density(4)'/
           DATA GROHEAD(3, 28) /'cm/cm3'/
           DATA GROHEAD(4, 28) /'RL4D'/
c     Root length density (cm3/cm3)
c     Layer 5
           DATA GROHEAD(1, 29) /'Root_Len.'/
           DATA GROHEAD(2, 29) /'density(5)'/
           DATA GROHEAD(3, 29) /'cm/cm3'/
           DATA GROHEAD(4, 29) /'RL5D'/
c     Root length density (cm3/cm3)
c     Layer 6
           DATA GROHEAD(1, 30) /'Root_Len.'/
           DATA GROHEAD(2, 30) /'density(6)'/
           DATA GROHEAD(3, 30) /'cm/cm3'/
           DATA GROHEAD(4, 30) /'RL6D'/
c     Root length density (cm3/cm3)
c     Layer 7
           DATA GROHEAD(1, 31) /'Root_Len.'/
           DATA GROHEAD(2, 31) /'density(7)'/
           DATA GROHEAD(3, 31) /'cm/cm3'/
           DATA GROHEAD(4, 31) /'RL7D'/
c     Root length density (cm3/cm3)
c     Layer 8
           DATA GROHEAD(1, 32) /'Root_Len.'/
           DATA GROHEAD(2, 32) /'density(8)'/
           DATA GROHEAD(3, 32) /'cm/cm3'/
           DATA GROHEAD(4, 32) /'RL8D'/
c     Root length density (cm3/cm3)
c     Layer 9
           DATA GROHEAD(1, 33) /'Root_Len.'/
           DATA GROHEAD(2, 33) /'density(9)'/
           DATA GROHEAD(3, 33) /'cm/cm3'/
           DATA GROHEAD(4, 33) /'RL9D'/
c     Root length density (cm3/cm3)
c     Layer 10
           DATA GROHEAD(1, 34) /'Root_Len.'/
           DATA GROHEAD(2, 34) /'density(10)'/
           DATA GROHEAD(3, 34) /'cm/cm3'/
           DATA GROHEAD(4, 34) /'RL10D'/

c     Potential soil evaporation (mm/day)
c     This is in here because ET.OUT does not output it...
           DATA GROHEAD(1, 35) /'Potential'/
           DATA GROHEAD(2, 35) /'soil'/
           DATA GROHEAD(3, 35) /'evap(mm)'/
           DATA GROHEAD(4, 35) /'EOSA'/

c     Total root length density
c     This is a fairly meaningless variable that
c     assisted with model comparison
           DATA GROHEAD(1, 36) /'Total'/
           DATA GROHEAD(2, 36) /'Root'/
           DATA GROHEAD(3, 36) /'Length'/
           DATA GROHEAD(4, 36) /'RTLD'/

c     Percent light interception
c     0-100%		     
           DATA GROHEAD(1, 37) /'Percent'/
           DATA GROHEAD(2, 37) /'light'/
           DATA GROHEAD(3, 37) /'interceptn'/
           DATA GROHEAD(4, 37) /'LI%D'/
			     
c     Specific leaf area
           DATA GROHEAD(1, 38) /'Spec'/
           DATA GROHEAD(2, 38) /'Leaf'/
           DATA GROHEAD(3, 38) /'Area'/
           DATA GROHEAD(4, 38) /'SLAD'/

c     Gross photosynthetic rate
c     t/ha/day
           DATA GROHEAD(1, 39) /'Gross'/
           DATA GROHEAD(2, 39) /'photos'/
           DATA GROHEAD(3, 39) /'rate(t/ha/d)'/
           DATA GROHEAD(4, 39) /'PGRD'/

c     Biomass accumulation rate (change per day)
c     t/ha/day		     
           DATA GROHEAD(1, 40) /'Biomass'/
           DATA GROHEAD(2, 40) /'accum.'/
           DATA GROHEAD(3, 40) /'(t/ha/d)'/
           DATA GROHEAD(4, 40) /'BRDMD'/
			     
c     Water-logging stress   
           DATA GROHEAD(1, 41) /'Excess'/
           DATA GROHEAD(2, 41) /'Soil H2O'/
           DATA GROHEAD(3, 41) /'Stress(0-1)'/
           DATA GROHEAD(4, 41) /'EWSD'/
			    
c     Cumulative thermal time (emergence and new leaves)
           DATA GROHEAD(1, 42) /'Heat units'/
           DATA GROHEAD(2, 42) /'Emrg & Leaf'/
           DATA GROHEAD(3, 42) /'deg C.day'/
           DATA GROHEAD(4, 42) /'STTD'/

c     Cumulative thermal time (new tillers)
           DATA GROHEAD(1, 43) /'Heat units'/
           DATA GROHEAD(2, 43) /'New Tillers'/
           DATA GROHEAD(3, 43) /'deg C.day'/
           DATA GROHEAD(4, 43) /'STTG'/

c     Nitrogen stress
           DATA GROHEAD(1, 44) /'Nitrogen'/
           DATA GROHEAD(2, 44) /''/
           DATA GROHEAD(3, 44) /'Stress(0-1)'/
           DATA GROHEAD(4, 44) /'NSTD'/

c     Sucrose % of dry stalk mass
           DATA GROHEAD(1, 45) /'Sucrose %'/
           DATA GROHEAD(2, 45) /'dry mass'/
           DATA GROHEAD(3, 45) /'t/t * 100'/
           DATA GROHEAD(4, 45) /'SU%DMD'/

c     Sucrose % of wet/fresh stalk mass
           DATA GROHEAD(1, 46) /'Sucrose %'/
           DATA GROHEAD(2, 46) /'wet mass'/
           DATA GROHEAD(3, 46) /'t/t * 100'/
           DATA GROHEAD(4, 46) /'SU%FMD'/

c     P stress affecting growth (PSTRES1)
           DATA GROHEAD(1, 47) /'Growth'/
           DATA GROHEAD(2, 47) /'Phosphorus'/
           DATA GROHEAD(3, 47) /'Stress(0-1)'/
           DATA GROHEAD(4, 47) /'PS1D'/

c     P stress affecting photosynthesis (PSTRES2)
           DATA GROHEAD(1, 48) /'Phot.'/
           DATA GROHEAD(2, 48) /'Phosphorus'/
           DATA GROHEAD(3, 48) /'Stress(0-1)'/
           DATA GROHEAD(4, 48) /'PS2D'/

c     ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c          Number of output variables (important for output!)
           DATA NUM_OVARS /48/

c          Width of output columns:
           DATA VAR_WIDTH /12/
c     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

!     No output for fallow crop
      CROP    = CONTROL % CROP
      IDETG   = ISWITCH % IDETG
      IF (CROP .EQ. 'FA' .OR. IDETG .EQ. 'N') RETURN

!     Transfer values from constructed data types into local variables.
      DAS     = CONTROL % DAS
      DYNAMIC = CONTROL % DYNAMIC
      FROP    = CONTROL % FROP
      RUN     = CONTROL % RUN
      YRDOY   = CONTROL % YRDOY
      YRSIM   = CONTROL % YRSIM

      ISWPHO = ISWITCH % ISWPHO
!***********************************************************************
!***********************************************************************
!     Run initialization - run once per simulation
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
!-----------------------------------------------------------------------
!      IF (IDETG .EQ. 'Y') THEN
        OUTG  = 'PlantGro.OUT'
        CALL GETLUN('OUTG',  NOUTDG)

        OUTPN  = 'PlantN.OUT  '
        CALL GETLUN('OUTPN', NOUTPN)

        OUTPC  = 'PlantC.OUT  '
        CALL GETLUN('OUTPC', NOUTPC)
        
        OUTKG  = 'PlantKg.OUT '
        CALL GETLUN('OUTKG', NOUTKG)
!      ENDIF

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
!       Initialize daily growth output file (tons, CaneGro format)
        INQUIRE (FILE = OUTG, EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT = NOUTDG, FILE = OUTG, STATUS = 'OLD',
     &      IOSTAT = ERRNUM, POSITION = 'APPEND')
          FIRST = .FALSE.
        ELSE
          OPEN (UNIT = NOUTDG, FILE = OUTG, STATUS = 'NEW',
     &      IOSTAT = ERRNUM)
          WRITE(NOUTDG,'("*GROWTH ASPECTS OUTPUT FILE")')
          FIRST = .TRUE.
        ENDIF

        !Write headers
        CALL HEADER(SEASINIT, NOUTDG, RUN)

          FMT_STR = '(A5, 1X, A3, '

          DO J=3, NUM_OVARS
c             Get length of variable name
              VLEN = LEN_TRIM(GROHEAD(4, J))
c             How many spaces need to be skipped?
              SKIP = VAR_WIDTH - VLEN
c             Create strings of these:
              WRITE(SKIP_STR, '(I3)') SKIP
              WRITE(VLEN_STR, '(I3)') VLEN
c              WRITE(*, '(A)') SKIP_STR, VLEN_STR

c             Add to format statement:
              IF (J .EQ. NUM_OVARS) THEN
c                 If this is format info for the last variable:
                  T_FMT_STR = TRIM(SKIP_STR) // 'X,'
     &                      // 'A' // TRIM(VLEN_STR)
     &                      // ')'
              ELSE
c                 For any variable
                  T_FMT_STR = TRIM(SKIP_STR) // 'X,'
     &                      // 'A' // TRIM(VLEN_STR)
     &                      // ','
              ENDIF

              TLEN = LEN_TRIM(T_FMT_STR) + 1
              FLEN = LEN_TRIM(FMT_STR) + 1
              ENDI = FLEN + TLEN - 1

c              WRITE(*, '(A)') T_FMT_STR
c              WRITE(*, '(3(I3, 2H, ))') TLEN, FLEN, ENDI
              WRITE(FMT_STR(FLEN:ENDI), '(A)') T_FMT_STR(1:TLEN)
              
          ENDDO


c         Output the format string

c         Format string for general headings:
          WRITE(WIDTH_STR, '(I3)') VAR_WIDTH-1
          G_FMT_STR = '(A5,1X,A3,1X,50(1H|, A' 
     &                 // TRIM(WIDTH_STR) // '))'


c         Loop through each row of GROHEAD, outputting
c         each column value per row
          DO I=1, 4
              DO J=1,NUM_OVARS    
                  GRO_OUT(J) = GROHEAD(I, J)  
              ENDDO
               IF (I .LT. 4) THEN
                  WRITE (NOUTDG,FMT=G_FMT_STR) 
     &                       GRO_OUT(1:NUM_OVARS)
               ELSE
c                   Write right-aligned column headings
                    WRITE (NOUTDG,FMT=FMT_STR) 
     &                       GRO_OUT(1:NUM_OVARS)
               ENDIF 
          ENDDO

c         Output format string for daily values
          D_FMT_STR = '(1X, I4, 1X, I3, 4(1X, I' 
     &                // TRIM(WIDTH_STR) // 
     &                '), 95(1X, F' // 
     &                TRIM(WIDTH_STR) // '.3))'

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!       Initialize daily growth output file (kg, DSSAT format)
        INQUIRE (FILE = OUTKG, EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT = NOUTKG, FILE = OUTKG, STATUS = 'OLD',
     &      IOSTAT = ERRNUM, POSITION = 'APPEND')
          FIRST = .FALSE.
        ELSE
          OPEN (UNIT = NOUTKG, FILE = OUTKG, STATUS = 'NEW',
     &      IOSTAT = ERRNUM)
          WRITE(NOUTKG,'("*GROWTH ASPECTS OUTPUT FILE, Kg")')
          FIRST = .TRUE.
        ENDIF

        !Write headers
        CALL HEADER(SEASINIT, NOUTKG, RUN)
	
        IF (ISWPHO .EQ. 'N') THEN ! Phosphorus stress is off.

        WRITE (NOUTKG,200,ADVANCE="NO")
!        WRITE (NOUTKG,200)
  200   FORMAT('!           |<-Days->|         Leaf',30X,'|< T/Ha>',
     &   '|<---------- kg/Ha ----------->|',10X,'|<-kg/Ha->|',
     &   ' |<----  Stress (0-1) ---->|',8X,'SLAREF   <--Canopy->  ',    
     &   ' Root  |',
     &   '<---------------------- cm[roots]/cm3[soil] ',
     &   '---------------------->| <-- kg/ha--> |',
     &    /'!             after   Growth    Num  |<---LAI-->|',
     &    ' |<Stalks per>| |<-------- Dry Weight --------->|',
     &    '  Fresh Fresh  |  Senesced     Phot   Grow   Leaf  Exces' ,
     &   '   Leaf   Leaf   High   Wide  Depth  |',67X,'|   Senes Dry  |',
     &    /'!',11X,'Sim Plant  Stage  Stk#1   live   dead  Plant     M2',
     &    '  Aerial  Leaf  Stalk  Sucro   Root  Yield Sucro%   Leaf',
     &    '  Stalk    H2O    H2O      N    H2O    N %  cm2/g',
     &    '      m      m      m  |',23X,
     &    ' Root Length Density',24X,'|  Surf   Soil | ',
     &    'Leaf weight each stalk (g[dry matter live leaves])')

        DO I = 1, ((Smax*7)-53) 
	    WRITE (NOUTKG,'("-")',ADVANCE="NO")
	  ENDDO 
	  
	  WRITE (NOUTKG,'(">| Stalk weight each", 
     &        " (g[dry matter live stalks])")',ADVANCE="NO")  

        DO I = 1, ((Smax*7)-48) 
	    WRITE (NOUTKG,'("-")',ADVANCE="NO")
	  ENDDO 
	  
	  WRITE (NOUTKG,'(">| Sucrose weight each stalk (g)")',
     &	  ADVANCE="NO")  

        DO I = 1, ((Smax*7)-32) 
	    WRITE (NOUTKG,'("-")',ADVANCE="NO")
	  ENDDO  
	  
	  WRITE (NOUTKG,'(">| Nodes on each stalk",
     &	    " (live and senesced leaves)")',ADVANCE="NO") 

        DO I = 1, ((Smax*7)-49) 
	    WRITE (NOUTKG,'("-")',ADVANCE="NO")
	  ENDDO  
	  
	  WRITE (NOUTKG,'(">| Leaf area on each stalk",
     &	" (cm2 [live leaves])")',  ADVANCE="NO") 

        DO I = 1, ((Smax*7)-46) 
	    WRITE (NOUTKG,'("-")',ADVANCE="NO")
	  ENDDO  
	  
	  WRITE (NOUTKG,'(">|")',ADVANCE="NO")


        WRITE (NOUTKG,210,ADVANCE="NO")

  210   FORMAT(//'@YEAR DOY   DAS   DAP',
     &         '   GSTD  L#S1D  LAIGD  DLAID',   
     &         '   T#PD   T#AD  BADMD   LWAD',   
     &         '   SWAD   SUAD',
     &         '   RWAD   HWAH',
     &         ' SU%FMD   LDAD   SDAD',
     &         '   WSPD   WSGD   NSTD',
     &         '   EWSD   LN%D   SLAD',
     &         '   CHTD   CWID', 
     &         '   RDPD   RL1D   RL2D   RL3D   RL4D',
     &         '   RL5D   RL6D   RL7D   RL8D   RL9D  RL10D',
     &         '  SNW0C  SNW1C  ')
        ELSE ! Phosphorus stress is on.

        WRITE (NOUTKG,220,ADVANCE="NO")

  220   FORMAT('!           |<-Days->|         Leaf',30X,'|< T/Ha>',
     &   '|<---------- kg/Ha ----------->|',10X,'|<-kg/Ha->|',
     &  ' |<------------ Stress (0-1)------------>|',8X,'SLAREF',
     &  '   <--Canopy->   Root  |',
     &  '<---------------------- cm[roots]/cm3[soil] ----------------',
     &  '------>| <-- kg/ha--> |',
     &    /,'!             after   Growth    Num  |<---LAI-->|',
     &    ' |<Stalks per>| |<-------- Dry Weight --------->|',
     &    '  Fresh Fresh  |',
     &    '  Senesced     Phot   Grow   Leaf  Parti   Phot',
     &    '  Exces   Leaf   Leaf   High   Wide  Depth  ',
     &    '|',67X,'|   Senes Dry  |',
     &  /,'!',11X,'Sim Plant  Stage  Stk#1   live   dead  Plant     M2',
     &    '   Aerial Leaf  Stalk  Sucro   Root  Yield Sucro%   Leaf',
     &    '  Stalk    H2O    H2O      N      P      P    H2O    N % ',
     &    ' cm2/g      m      m      m',
     &    '  |',24X,'Root Length Density',24X,'|  Surf   Soil | ',
     &    'Leaf weight each stalk (g[dry matter live leaves])')

        DO I = 1, ((Smax*7)-53) 
	    WRITE (NOUTKG,'("-")',ADVANCE="NO")
	  ENDDO 
	  
	  WRITE (NOUTKG,'(">| Stalk weight each", 
     &        " (g[dry matter live stalks])")',ADVANCE="NO")  

        DO I = 1, ((Smax*7)-48) 
	    WRITE (NOUTKG,'("-")',ADVANCE="NO")
	  ENDDO 
	  
	  WRITE (NOUTKG,'(">| Sucrose weight each stalk (g)")',
     &	  ADVANCE="NO")  

        DO I = 1, ((Smax*7)-32) 
	    WRITE (NOUTKG,'("-")',ADVANCE="NO")
	  ENDDO  
	  
	  WRITE (NOUTKG,'(">| Nodes on each stalk",
     &	    " (live and senesced leaves)")',ADVANCE="NO") 

        DO I = 1, ((Smax*7)-49)  
	    WRITE (NOUTKG,'("-")',ADVANCE="NO")
	  ENDDO  
	  
	  WRITE (NOUTKG,'(">| Leaf area on each stalk",
     &	" (cm2 [live leaves])")',  ADVANCE="NO") 

        DO I = 1, ((Smax*7)-46) 
	    WRITE (NOUTKG,'("-")',ADVANCE="NO")
	  ENDDO  
	  
	  WRITE (NOUTKG,'(">|")')

          WRITE (NOUTKG,230,ADVANCE="NO")  ! Begin here FSR

  230     FORMAT(//'@YEAR DOY   DAS   DAP',
     &         '   GSTD  L#S1D  LAIGD  DLAID',   
     &         '   T#PD   T#AD  BADMD   LWAD',   
     &         '   SWAD   SUAD',
     &         '   RWAD   HWAH',
     &         ' SU%FMD   LDAD   SDAD',
     &         '   WSPD   WSGD   NSTD',   
     &         '   PS1D   PS2D', 
     &         '   EWSD   LN%D   SLAD',
     &         '   CHTD   CWID', 
     &         '   RDPD   RL1D   RL2D   RL3D   RL4D',
     &         '   RL5D   RL6D   RL7D   RL8D   RL9D  RL10D',
     &         '  SNW0C  SNW1C  ')
        ENDIF  ! (ISWPHO .EQ. 'N')

!    Header line IF statements to keep column headings same length
!      by inserting underscore where stalk number < 10.  FSR     

      IF (Smax <= 9) THEN	
	  DO Stalk = 1,Smax
        WRITE(NOUTKG,'(2X"LWD_",I1)',ADVANCE="NO") Stalk
      END DO
	ELSE
	  DO Stalk = 1,9
        WRITE(NOUTKG,'(2X"LWD_",I1)',ADVANCE="NO") Stalk
      END DO
	  DO Stalk = 10,Smax
        WRITE(NOUTKG,'(2X"LWD",I2)',ADVANCE="NO") Stalk
        END DO
	ENDIF
      
      IF (Smax <= 9) THEN	
	  DO Stalk = 1,Smax
        WRITE(NOUTKG,'(2X"SWD_",I1)',ADVANCE="NO") Stalk
      END DO
	ELSE
	  DO Stalk = 1,9
        WRITE(NOUTKG,'(2X"SWD_",I1)',ADVANCE="NO") Stalk
      END DO
	  DO Stalk = 10,Smax
        WRITE(NOUTKG,'(2X"SWD",I2)',ADVANCE="NO") Stalk
        END DO
	ENDIF
      
      IF (Smax <= 9) THEN	
	  DO Stalk = 1,Smax
        WRITE(NOUTKG,'(2X"SUW_",I1)',ADVANCE="NO") Stalk
      END DO
	ELSE
	  DO Stalk = 1,9
        WRITE(NOUTKG,'(2X"SUW_",I1)',ADVANCE="NO") Stalk
      END DO
	  DO Stalk = 10,Smax
        WRITE(NOUTKG,'(2X"SUW",I2)',ADVANCE="NO") Stalk
        END DO
	ENDIF
    
      IF (Smax <= 9) THEN	
	  DO Stalk = 1,Smax
        WRITE(NOUTKG,'(3X"LN_",I1)',ADVANCE="NO") Stalk
      END DO
	ELSE
	  DO Stalk = 1,9
        WRITE(NOUTKG,'(3X"LN_",I1)',ADVANCE="NO") Stalk
      END DO
	  DO Stalk = 10,Smax
        WRITE(NOUTKG,'(3X"LN",I2)',ADVANCE="NO") Stalk
        END DO
	ENDIF
    
      IF (Smax <= 9) THEN	
	  DO Stalk = 1,Smax
        WRITE(NOUTKG,'(2X"LAD_",I1)',ADVANCE="NO") Stalk
      END DO
	ELSE
	  DO Stalk = 1,9
        WRITE(NOUTKG,'(2X"LAD_",I1)',ADVANCE="NO") Stalk
      END DO
	  DO Stalk = 10,Smax
        WRITE(NOUTKG,'(2X"LAD",I2)',ADVANCE="NO") Stalk
        END DO
	ENDIF 
    
      WRITE (NOUTKG,'(/)')  ! skip a line before data output
!-----------------------------------------------------------------------

!       Initialize daily plant nitrogen output file
        INQUIRE (FILE = OUTPN, EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT = NOUTPN, FILE = OUTPN, STATUS = 'OLD',
     &      IOSTAT = ERRNUM, POSITION = 'APPEND')
          FIRST = .FALSE.
        ELSE
          OPEN (UNIT = NOUTPN, FILE = OUTPN, STATUS = 'NEW',
     &      IOSTAT = ERRNUM)
          WRITE(NOUTPN,'("*PLANT N OUTPUT FILE")')
          FIRST = .TRUE.
        ENDIF

        !Write headers
        CALL HEADER(SEASINIT, NOUTPN, RUN)

        WRITE (NOUTPN,240)
  240   FORMAT('@YEAR DOY   DAS   DAP',
     &      '    CNAD    GNAD    VNAD    GN%D    VN%D     NFXC    NUPC',
     &      '    LNAD    SNAD    LN%D    SN%D    SHND',
     &      '   RN%D   NFXD   SNN0C   SNN1C')

!-----------------------------------------------------------------------
!       Initialize daily plant carbon output file
        INQUIRE (FILE = OUTPC, EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT = NOUTPC, FILE = OUTPC, STATUS = 'OLD',
     &      IOSTAT = ERRNUM, POSITION = 'APPEND')
          FIRST = .FALSE.
        ELSE
          OPEN (UNIT = NOUTPC, FILE = OUTPC, STATUS = 'NEW',
     &      IOSTAT = ERRNUM)
          WRITE(NOUTPC,'("*PLANT C OUTPUT FILE")')
          FIRST = .TRUE.
        ENDIF

        !Write headers
        CALL HEADER(SEASINIT, NOUTPC, RUN)

        WRITE (NOUTPC,250)
  250   FORMAT('@YEAR DOY   DAS   DAP   TWAD    PHAD',
     &      '    CMAD    CGRD    GRAD    MRAD    CHAD   CL%D   CS%D',
     &      '   TGNN   TGAV    GN%D    GL%D    GC%D')
!-----------------------------------------------------------------------
! Open file to write results for comparison to field experiment data

!CHP    OPEN(UNIT = PSLUN, FILE = "PRIMARY_STALK.OUT", STATUS = "UNKNOWN", 
        CALL GETLUN('PSTALK',PSLUN)
        OPEN(UNIT = PSLUN, FILE = "PrimaryStalk.OUT",STATUS = "UNKNOWN",
     &     ACTION = "WRITE", POSITION = "REWIND", IOSTAT = OpenStatus)

        WRITE(PSLUN,'("*PRIMARY STALK OUTPUT FILE")')

        !Write headers
        CALL HEADER(SEASINIT, PSLUN, RUN)

        WRITE (PSLUN,260,ADVANCE="NO")
  260   FORMAT('!     |<----Days--->|  |<Cuml TU>|',
     &   '       |<Leaf & Sheath Dry Weight (g on stalk)> |',22X,
     &   '|<--Primary Stalk Weight (g)-->|',
     & /,'!             after    |  °C-day |  Leaf |     Live     |',
     &   '    Senesced   |  Total  | <--Leaf Area  cm2--> ',
     &   '|<-----Dry matter----->| Fresh |', 
     & /,'!',11X,'Sim Plant  TB(1) TB(2) Numbr |  Leaf  Sheath|   Leaf',
     &   '  Sheath|         |  Live    Dead   Total| Stalk Sucrose',
     &   ' Stk+Suc| stalk |')

        WRITE(PSLUN,'(/"@YEAR DOY   DAS   DAP  STTD  STTG  L#S1D
     &   LWD_1   LSW_1   DLW_1   DLS_1   TLW_1   LAD_1   DLA_1   TLA_1
     &   SWD_1   SUW_1   STD_1   SWF_1")')
!-----------------------------------------------------------------------

        CUMSENSURF  = 0.
        CUMSENSOIL  = 0.
        CUMSENSURFN = 0.
        CUMSENSOILN = 0.
	  BRDMD       = 0.

!***********************************************************************
!***********************************************************************
!     DAILY OUTPUT 
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. OUTPUT) THEN
C-----------------------------------------------------------------------
C   CHECK FOR OUTPUT FREQUENCY
C-----------------------------------------------------------------------
      IF (YRDOY .LT. YRPLT .OR. YRPLT .LT. 0) RETURN

!     Accumulate senesced matter for surface and soil.
      CUMSENSURF  = CUMSENSURF  + SENESCE % ResWt(0) 
      CUMSENSURFN = CUMSENSURFN + SENESCE % ResE(0,1) 
	TRLV = 0.0
      DO L = 1, NLAYR
        CUMSENSOIL  = CUMSENSOIL  + SENESCE % ResWt(L)
        CUMSENSOILN = CUMSENSOILN + SENESCE % ResE(L,1)
	  TRLV = TRLV + RLV(L) * DLAYR(L)
      ENDDO

      IF ((MOD(DAS,FROP) .EQ. 0)          !Daily output every FROP days,
     &  .OR. (YRDOY .EQ. YRPLT)           !on planting date, and
     &  .OR. (YRDOY .EQ. MDATE)) THEN     !at harvest maturity 

!       Print 
        DAP = MAX(0,TIMDIF(YRPLT,YRDOY))
        CALL YR_DOY(YRDOY, YEAR, DOY) 

        IF (IDETG .EQ. 'Y') THEN

!          IF (DAS .GE. NVEG0) THEN
            RHOLP  = RHOL * 100.
            RHOSP  = RHOS * 100.
            SLAP   = SLAREF
            PCNLP  = PCNL
            PCNSTP = PCNST
            PCNRTP = PCNRT

!Hold off on this "IF" statement until GBuild can handle -99 values
!          ELSE
!            RHOLP  = -99.
!            RHOSP  = -99.
!            SLAP   = -99.
!            PCNLP  = -99.
!            PCNSTP = -99.
!            PCNRTP = -99.
!          ENDIF

!          IF (SDWT .GT. 0.0001) THEN  ! (for -99 values)
            PCNSDP = PCNSD
            PCLSDP = PCLSD
            PCCSDP = PCCSD
            PCNSHP = PCNSH

		 BADMD = LFWTHa(DAS) +LSWTHa(DAS) +STKWTHa(DAS) +SUWTHa(DAS) 
     &		   + LFWTHalost(DAS) + LSWTHalost(DAS) + STKWTHalost(DAS)
		 ! Above ground biomass includes senesced leaves and stalks

	SMDMD = (SUWTHa(DAS)+STKWTHa(DAS)) ! Millable stalk DM (kg)
 	SMFMD = STKFWTHa(DAS)              ! Fresh millable stalk (kg)

        IF (SUWTHa(DAS) .GE. 1E-4) THEN

          HID = SUWTHa(DAS)/SMDMD  !Harvest ratio: Sucrose / Millable DM
          HIF = SUWTHa(DAS)/SMFMD  !Harvest ratio: Sucrose / Millable FM
		SUID = SUWTHa(DAS) / BADMD ! sucrose/aerial biomass DM

        ELSE 
          HID = 0.
          HIF = 0.
		SUID = 0.
        
	  ENDIF

! Continue here calculating derived variables as needed for output: 
            
          IF (ISWPHO .EQ. 'N') THEN
            WRITE (NOUTKG,310,ADVANCE="NO")YEAR, DOY, DAS, DAP, 
     &        PhenoStage, INT(LeafNum(DAS,1)), XLAI, XLAIlost, 
     &        CountStalk, StalkPopul(DAS), 
     &        BADMD/1000,
     &        NINT(LFWTHa(DAS)), NINT(STKWTHa(DAS)), NINT(SUWTHa(DAS)),    
     &        NINT(RTWTHa(DAS)), NINT(SMFMD),
     &        HIF*100, NINT(LFWTHalost(DAS)), NINT(STKWTHalost(DAS)), 
!                    1X,F6.1, 2(1X,I6),
     &        (1.-SWFAC), (1.-TURFAC),(1.-NSTRES), !3(1X,F6.3),
     &        SATFAC, PCNLP, NINT(SLAREF), 
     &        CANHT, CANWH,   
     &        (RTDEP/100.), (RLV(I),I=1,10),  
     &        NINT(CUMSENSURF), NINT(CUMSENSOIL)
  310       FORMAT (/1X,I4,1X,I3,2(1X,I5),
     &        1X,I6,1X,I6,1X,F6.2,1X,F6.2,
     &        1X,I6,1X,F6.1,1X,F6.2,1X,I6, !12
     &        2(1X,I6),
     &        2(1X,I6),
     &        1X,F6.1,2(1X,I6),   !19
     &        3(1X,F6.3),
     &        1X,F6.3, 1X,F6.1,1X,I6,
     &        2(1X, F6.2),
     &        11(1X,F6.2),
     &        2(1X,I6),2X)  
          
		ELSE
	
            WRITE (NOUTKG,312,ADVANCE="NO")YEAR, DOY, DAS, DAP, 
     &        PhenoStage, INT(LeafNum(DAS,1)), XLAI, XLAIlost, 
     &        CountStalk, StalkPopul(DAS), 
!     &        (LFWTHa(DAS)+STKWTHa(DAS)+SUWTHa(DAS))/1000,
     &		BADMD /1000, 
     &		NINT(LFWTHa(DAS)), 
     &        NINT(STKWTHa(DAS)), NINT(SUWTHa(DAS)),  
     &        NINT(RTWTHa(DAS)), NINT(SMFMD),
     &        HIF*100, NINT(LFWTHalost(DAS)), NINT(STKWTHalost(DAS)), 
     &        (1.-SWFAC), (1.-TURFAC),(1.-NSTRES), 
     &        (1.-PSTRES1), (1.-PSTRES2), 
     &        SATFAC, PCNLP, NINT(SLAREF), 
     &        CANHT, CANWH,   
     &        (RTDEP/100.), (RLV(I),I=1,10),  
     &        NINT(CUMSENSURF), NINT(CUMSENSOIL)
 312       FORMAT (/1X,I4,1X,I3,2(1X,I5),
     &        1X,I6,     !PhenoStage
     &	    1X,I6,
     &        1X,F6.2,   !XLAI
     &        1X,F6.2,   !XLAIlost
     &        1X,I6,     !CountStalk
     &        1X,F6.1,   !StalkPopul(DAS)
     &        1X,F6.2,   !Total above ground
     &        1X,I6,           
     &        2(1X,I6),      
     &        2(1X,I6),
     &        1X,F6.1,   !HI*100
     &        2(1X,I6),
     &        2(1X,F6.3),
     &        3(1X,F6.3),
     &        1X,F6.3, 
     &        1X,F6.1,
     &        1X,I6,          
     &        2(1X, F6.2),
     &        11(1X,F6.2),
     &        2(1X,I6),2X)

          ENDIF
!  Restored long output since this is not longer a DSSAT "standard" output. FSR
  	
      DO Stalk = 1, Smax
	  IF (Kill(Stalk) > -1) THEN 
        WRITE(NOUTKG,'(1X,F6.2)', ADVANCE="NO") LFWT(DAS,Stalk)
        ELSE
	    WRITE(NOUTKG,'(7X)', ADVANCE="NO") 
	  END IF
      END DO

      DO Stalk = 1, Smax
	  IF (Kill(Stalk) > -1) THEN 
        WRITE(NOUTKG,'(1X,I6)', ADVANCE="NO") NINT(STKWT(DAS,Stalk))
        ELSE
	    WRITE(NOUTKG,'(7X)', ADVANCE="NO") 
	  END IF
      END DO

      DO Stalk = 1, Smax
	  IF (Kill(Stalk) > -1) THEN 
        WRITE(NOUTKG,'(1X,I6)', ADVANCE="NO") NINT(SUWT(DAS,Stalk))
        ELSE
	    WRITE(NOUTKG,'(7X)', ADVANCE="NO") 
	  END IF
      END DO

      DO Stalk = 1, Smax
	  IF (Kill(Stalk) > -1) THEN 
        WRITE(NOUTKG,'(3X,I4)', ADVANCE="NO") INT(LeafNum(DAS,Stalk))
        ELSE
	    WRITE(NOUTKG,'(7X)', ADVANCE="NO") 
	  END IF
      END DO

      DO Stalk = 1, Smax
	  IF (Kill(Stalk) > -1) THEN 
        WRITE(NOUTKG,'(1X,I6)', ADVANCE="NO") NINT(LeafArea(DAS,Stalk))
        ELSE
	    WRITE(NOUTKG,'(7X)', ADVANCE="NO") 
	  END IF
      END DO

C-----------------------------------------------------------------------
          WTNVEG  = (WTNLF + WTNST)
      
          IF ((LFWTHa(DAS)+STKWTHa(DAS)).GT. 0.0001) THEN
            PCNVEG = (WTNLF+WTNST)/(LFWTHa(DAS)+STKWTHa(DAS))*10.
          ELSE
            !PCNVEG = -99.    !Wait for GBuild fix for -99's
            PCNVEG = 0.
          ENDIF

          WRITE (NOUTPN,410) YEAR, DOY, DAS, DAP, (WTNCAN*10), 
     &       (WTNSD*10), (WTNVEG*10), PCNSDP, PCNVEG, (WTNFX*10),
     &       (WTNUP*10), (WTNLF*10), (WTNST*10), PCNLP, PCNSTP,
     &       PCNSHP, PCNRTP, NFIXN*10, CUMSENSURFN, CUMSENSOILN
  410     FORMAT(1X,I4,1X,I3.3,2(1X,I5),3(1X,F7.1),2(1X,F7.2),1X,
     &       2(1X,F7.1),2(1X,F7.1),2(1X,F7.2),1X,F7.1,2(1X,F6.1),
     &       2(1X,F7.2))

C-----------------------------------------------------------------------
          WRITE (NOUTPC,510) YEAR, DOY, DAS, DAP,
     &        NINT(TOTWT*10), PGT, CMINEA, GROWTH,
     &        GRWRES, MAINR, (CADLF + CADST), RHOLP, RHOSP,
     &        TGRO(TS/2), TGROAV, PCNSDP, PCLSDP, PCCSDP
C            changed from 12 to TS/2 on 9Jan17 by Bruce Kimball          
  510     FORMAT(1X,I4,1X,I3.3,2(1X,I5),1X,I6,6(1X,F7.2),2(1X,F6.1),
     &        2(1X,F6.1),3(1X,F7.2))
C-----------------------------------------------------------------------

		WRITE(PSLUN,'(1X,I4,1X,I3,4(1X,I5),1X,I6,12(1X,I7))'), 
     &       YEAR, DOY, DAS, DAP, NINT(SumTTD), NINT(SumTTG),
     &       INT(LeafNum(DAS, 1)), NINT(LFWT(DAS,1)), 
     &       NINT(LSWT(DAS,1)), NINT(LFWTlost(DAS,1)),
     &       NINT(LSWTlost(DAS,1)), 
     &    NINT(LFWT(DAS,1)+LFWTlost(DAS,1)+LSWT(DAS,1)+LSWTlost(DAS,1)),
     &       NINT(LeafArea(DAS,1)), NINT(LeafArealost(DAS,1)),
     &       NINT(LeafArea(DAS,1) + LeafArealost(DAS,1)),
     &       NINT(STKWT(DAS,1)), NINT(SUWT(DAS,1)),
     &       NINT(STKWT(DAS,1) + SUWT(DAS,1)),
     &       NINT(STKFWT(DAS,1))
 
        ENDIF   
!***********************************************************************
C Write PlantGro.Out file

        WRITE(NOUTDG,FMT=D_FMT_STR) YEAR, DOY, DAS, DAP, 
     &       INT(LeafNum(DAS,1)),PhenoStage, XLAI, LFWTHa(DAS)/1000, 
  
     &       LSWTHa(DAS)/1000, SMFMD/1000, SMDMD/1000,  
     &       SUWTHa(DAS)/1000, RTWTHa(DAS)/1000, BADMD/1000,                 
     &       StalkPopul(DAS), REAL(CountStalk), SUID,  
     &	   (LFWTHalost(DAS)+LSWTHalost(DAS))/1000, XLAI+XLAIlost,             
     &       (1.-SWFAC), (1.-TURFAC), STKWTHalost(DAS)/1000, CANHT, 
     &       RTDEP/100, (RLV(I),I=1,10), EOS, TRLV, LITOTAL*100,
     &       SLAREF, PGT/100, BRDMD, SATFAC, SumTTD, SumTTG,  
     &       (1.-NSTRES), HID*100, HIF*100, (1.-PSTRES1), (1.-PSTRES2)
!***********************************************************************
	  
	      
      ENDIF
C-----------------------------------------------------------------------

!***********************************************************************
!***********************************************************************
!     Seasonal Output 
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. SEASEND) THEN
C-----------------------------------------------------------------------
        !Close daily output files.
        CLOSE (NOUTDG)
	  CLOSE (NOUTKG)
        CLOSE (NOUTPN)
        CLOSE (NOUTPC)

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END ! SUBROUTINE CSP_OPGROW
!=======================================================================


!=======================================================================
!       Variable definitions for CSP_OPGROW
!-----------------------------------------------------------------------
! BADMD   Aerial dry biomass: stalk + leaf(live & dead) + sucrose (t/ha)
! BRDMD   Biomass accumulation rate (kg/ha/day)
! CHTD    Canopy height (m)
! CANWH   Canopy width normal to row (m)
! CROP    Crop identification code 
! CROPD   Name of crop 
! DAP     Number of days after planting (d)
! DYNAMIC Module control variable; =RUNINIT, SEASINIT, RATE, EMERG, INTEGR, 
!           OUTPUT, or FINAL 
! ENAME   Experiment description 
! EOS     Potential soil evaporation per day (mm/day)
! EXPER   Experiment code (prefix of input files) 
! HI      Harvest index (t sucrose / t above ground biomass)
! HID     Dry Harvest ratio:   Sucrose / Harvestable Cane Dry Mass                                            ! 
! HIF     Fresh Harvest ratio: Sucrose / Harvestable Cane Fresh Mass
! LAIGD   Green leaf area index (m2/m2)
! LAITD   Total (living + dead) LAI
! LDDMD   Trash (residue) dry mass (t/ha)
! LDGFD   Extent of lodging
! LFWTHa  Dry mass of leaf tissue including C and N (kg[leaf] / ha[ground])
! LGDMD   Green leaf canopy + meristem dry mass(t/ha)['green tops']
! LI%D    Canopy light interception (%)
! LITOTAL Total light interception fraction, calculated by summing LI of all stalks 
! MODEL   Name of CROPGRO executable file 
! MULTI   Current seasonal simulation (=1 for first or only simulation, 
!           =NYRS for last simulation 
! NL      maximum number of soil layers = 20 
! NOUTDG  Unit number for growth output file 
! NSTRES  Nitrogen stress factor (1=no stress, 0=max stress) 
! OUTG    PlantGro output file name (matches CaneGro PlantGro.Out)  
! OUTKG   PlantKg output file name (similar to other DSSAT crop output: kgs, no tons)
! PCNL    Percentage of N in leaf tissue (100 g[N] / g[leaf])
! PGRD    Gross photosynthesis rate (t/ha/day)
! RDMD    Root dry mass, header in PlantGro.Out (t/ha)   
! RLV(L)  Root length density for soil layer L ((cm root / cm3 soil))
! RSTAGE  Number of RSTAGES which have occurred. 
! RTDEP   Root depth (cm)
! RTLD    Total root length per cm2 (cm/cm2)  
! RTWTHa  Dry mass of root tissue, including C and N (g[root] / ha[ground])
! SATFAC  Root length weighted soil water excess stress factor ( 0 = no 
!           stress; 1 = saturated stress ) 
! SDSIZE  Average mass of seeds (mg / seed)
! SDWT    Dry mass of seed tissue, including C and N (g[seed] / m2[ground])
! SEEDNO  Total number of seeds (#/m2)
! SMDMD   Millable stalk dry mass (t/ha)
! SHELLW  Shell mass (g[shell] / m2)
! SHELPC  Percentage of pod mass that is seeds (g[seed]/g[pods] * 100%)
! SHTD    Stalk height, millable to breaking point (m) 
! SHTVD   Stalk height, to top visible dew dewlap (m) 
! SKIP    Number of whitespaces that need to be skipped
! SLAREF  Specific leaf area for new leaves by Ecotype (cm2[leaf] / g[leaf DM])
! SMDMD   Stalk (millable; i.e., stalk + sugars) dry mass (t/ha)
! SMFMD   Stalk fresh massStalk (millable) fresh mass (t/ha)  
! SSTKH   Stalk (structure, no sucrose) dry weight at harvest (t/ha) 
! STKWTHa Dry mass of stalk tissue, including C and N (g[stem] / ha[ground)
! SUCMD   Sucrose dry mass (t/ha)
! SUDMD   Sucrose content (%) of millable dry mass 
! SUFMD   Sucrose content (%) of millable fresh mass
! SUID    Sucrose harvest index (sucrose/aerial biomass DM)
! SWFAC   Effect of soil-water stress on photosynthesis, 1.0=no stress, 
!           0.0=max stress 
! TITLET  Description of treatment for this simulation 
! TOPWT   Total weight of above-ground portion of crop, including pods
!           (g[tissue] / m2)
! TURFAC  Water stress factor for expansion (0 - 1) 
! VAR_WIDTH  Width of columns in PlantGro.Out 
! VSTAGE  Number of nodes on main stem of plant 
! WTCO    Cumulative losses of plant tissue (g[tissue] / m2)
! VLEN    Length of variable name string (excluding 
!         leading & trailing whitespace) 
! WSTD    Water stress affecting tiller population   
! WTLO    Cumulative leaf losses (g[leaf] / m2)
! WTSO    Cumulative stem losses (g[stem] / m2)
! XLAI    Leaf area (one side) per unit of ground area
!           (m2[leaf] / m2[ground])
! YRDOY   Current day of simulation (YYDDD)
! YRPLT   Planting date (YYDDD)
!***********************************************************************
!       END SUBROUTINE CSP_OPGROW
!=======================================================================

