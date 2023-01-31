C=======================================================================
C  TEFF_OPGROW, Subroutine
C
C  Generates output for simulated data
C-----------------------------------------------------------------------
C  Revision history
C
C  02/08/1993 PWW Header revision and minor changes
C  02/08/1993 PWW Added switch block, etc.
C  08/29/2002 CHP/MUS Converted to modular format for inclusion in CSM.
C  07/08/2003 CHP Added senescence output to conform to other plant routines.
C  11/22/2004 CHP Changed output file names from *.out to *.OUT 
C                   for case-sensitive OS's.
!  02/13/2015 chp N-uptake from TEFF_Nuptak.for routine instead of calculated
!                 here as tops N
C  12/12/2019 MB/US Copyed from Rice model and modified for Teff 
C  03/29/2021 MB/WP Addapted to Teff based on CERES-Rice
C=======================================================================

      SUBROUTINE TEFF_OPGROW (CONTROL, ISWITCH, SOILPROP,
     &    BIOMAS, GPP, GPSM, GRAINN, GRNWT, ISTAGE, LAI,  
     &    LEAFNO, LFWT, MDATE, NLAYR, NSTRES, PANWT, PLANTS,
     &    PLTPOP, RLV, ROOTN, RTDEP, RTWT, SENESCE,       
     &    STMWT, STOVN, SWFAC, TILNO, CumNUptake, TURFAC, YRPLT,      
     &    CANHT, KSTRES, DTT)                                  

C-----------------------------------------------------------------------
      USE ModuleDefs
      USE CsvOutput   ! VSH,chp
      IMPLICIT  NONE
      EXTERNAL GETLUN, HEADER, TIMDIF, YR_DOY
      SAVE

      CHARACTER*8   CHAR8
      CHARACTER*30  LayerText
      CHARACTER*220 GROHEAD(4), NITHEAD

      INTEGER LUNOV,YRPLT,DAP,DAS
      INTEGER RSTAGE,ISTAGE,YRDOY

      CHARACTER*1 IDETG, ISWNIT
      CHARACTER*12 OUTG, OUTPN
      INTEGER DYNAMIC, NOUTDG, FROP, LEAFNO, I, J, L, NLAYR
      INTEGER MDATE, NOUTPN, RUN, ERRNUM, TIMDIF, YEAR, DOY
      REAL BIOMAS, RTWT, LFWT, GRNWT, HI, GM2KG
      REAL LAI, GPSM, PANWT, GPP, PCNGRN, PCNSH, PCNVEG
      REAL PLTPOP, PLANTS, STOVN, GRAINN, ROOTN
      REAL TURFAC, RTDEP, STMWT,  RLV(NL), SWFAC, NSTRES  !, RLV5(5)
      REAL TILNO, CumNUptake, SATFAC, SDSIZE, SHELPC
      REAL WTNGRN, WTNVEG

      REAL    WTLF,SDWT,XLAI,SEEDNO   !,PODWT
      REAL    NFIXN,PCNL,PCNST,PCNRT
      REAL    TOTWT
      REAL    VSTAGE, DTT
      REAL    WTNUP,WTNCAN,WTNLF,WTNST,WTNSD,WTNSH,WTNFX,WTNRT

      REAL CUMSENSURF, CUMSENSOIL, CUMSENSURFN, CUMSENSOILN  

C
C     Growth variables
C
      REAL    CANHT,CANWH,SLA,KSTRES
      PARAMETER (LUNOV  =  6)

      LOGICAL FEXIST, FIRST

      TYPE (ResidueType) SENESCE
      TYPE (SoilType) SOILPROP

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
!     The variable "CONTROL" is of type "ControlType".
      TYPE (ControlType) CONTROL

!     The variable "ISWITCH" is of type "SwitchType".
      TYPE (SwitchType) ISWITCH

!     Transfer values from constructed data types into local variables.
      IDETG  = ISWITCH % IDETG
      IF (IDETG .NE. 'Y') RETURN
      ISWNIT = ISWITCH % ISWNIT

      DAS     = CONTROL % DAS
      DYNAMIC = CONTROL % DYNAMIC
      FROP    = CONTROL % FROP
      RUN     = CONTROL % RUN
      YRDOY   = CONTROL % YRDOY

      FMOPT   = ISWITCH % FMOPT   ! VSH, CHP

!***********************************************************************
!***********************************************************************
!     Seasonal Initialization - Called once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      IF (IDETG .EQ. 'Y') THEN
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

        IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN
          OUTG  = 'PlantGro.OUT'
          CALL GETLUN('OUTG',  NOUTDG)

      GROHEAD(1) =
     &  '!YR        Days  Days              Leaf  <--------- Dry Wei' //
     &  'ght -----------> Grain Kernl              <------ Stress (0' //
     &  '-1) ------>   Leaf  Shell  Spec  <-Canopy->  Root  <-- Root' //
     &  ' Length Density ---><Senesced Mat>   Phot'   

      GROHEAD(2) =
     &  '!  and    after after  Leaf  Grow  Area  <------------ kg/H' //
     &  'a ------------->   per  wght  Harv Tillr  <--- Water ----> ' //
     &  '               Nit   -ing  Leaf  Hght Brdth Depth  <---- cm' //
     &  '3/cm3 of soil -----> <--(kg/ha)-->  Therm'
  
      WRITE(GROHEAD(3),'(A,A,A,A,A)') 
     &  '!     DOY start plant   Num Stage Index  Leaf  Stem Grain  ',
     &  'Root Panic  Crop    m2    mg  Indx   No.  Phot  Grow  Excs ',
     &  ' Nitr Potas     %      %   Area    m     m     m ',
     &  LayerText,'   Surf   Soil   Days'  

      GROHEAD(4) =
     &  '@YEAR DOY   DAS   DAP  L#SD  GSTD  LAID  LWAD  SWAD  GWAD' //
     &  '  RWAD  EWAD  CWAD  G#AD  GWGD  HIAD  T#AD  WSPD  WSGD' //
     &  '  EWSD  NSTD  KSTD   LN%D   SH%D  SLAD  CHTD  CWID  RDPD' //
     &  '  RL1D  RL2D  RL3D  RL4D  RL5D  SNW0C  SNW1C   DTTD' 

!-----------------------------------------------------------------------
!         Initialize daily growth output file
        INQUIRE (FILE = OUTG, EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT = NOUTDG, FILE = OUTG, STATUS = 'OLD',
     &        IOSTAT = ERRNUM, POSITION = 'APPEND')
          FIRST = .FALSE.  
        ELSE
          OPEN (UNIT = NOUTDG, FILE = OUTG, STATUS = 'NEW',
     &        IOSTAT = ERRNUM)
          WRITE(NOUTDG,'("*GROWTH ASPECTS OUTPUT FILE")')
          FIRST = .TRUE.  
        ENDIF

        !Write headers
        CALL HEADER(SEASINIT, NOUTDG, RUN)

!         Variable heading for GROWTH.OUT
        WRITE (NOUTDG,2192) GROHEAD(1)
        WRITE (NOUTDG,2192) GROHEAD(2)
        WRITE (NOUTDG,2192) GROHEAD(3)
        WRITE (NOUTDG,2192) GROHEAD(4)
 2192     FORMAT (A220)

        ENDIF
!-----------------------------------------------------------------------
!       Initialize daily plant nitrogen output file
        IF (ISWNIT .EQ. 'Y') THEN
C-----------------------------------------------------------------------
          OUTPN  = 'PlantN.OUT  '
          CALL GETLUN('OUTPN', NOUTPN)
          NITHEAD = '@YEAR DOY   DAS   DAP' //
     &      '  CNAD  GNAD  VNAD  GN%D  VN%D  NUPC  LNAD' //
     &      '  SNAD  LN%D  SN%D  SHND  RN%D  SNN0C  SNN1C'
        
          INQUIRE (FILE = OUTPN, EXIST = FEXIST)
          IF (FEXIST) THEN
            OPEN (UNIT = NOUTPN, FILE = OUTPN, STATUS = 'OLD',
     &        IOSTAT = ERRNUM, POSITION = 'APPEND')
            FIRST = .FALSE.  
          ELSE
            OPEN (UNIT = NOUTPN, FILE = OUTPN, STATUS = 'NEW',
     &        IOSTAT = ERRNUM)
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
      SEEDNO = 0.0
      GPP   = 0.0
      WTNUP = 0.0
!     YIELD = 0.0
      CANHT = 0.0
      CANWH = 0.0

      WTNLF    = 0.0 
      WTNST    = 0.0 
      WTNSD    = 0.0 
      WTNSH    = 0.0 
      WTNRT    = 0.0 

      PCNSH = 0.0
      SATFAC = 0.0

      CUMSENSURF  = 0.0
      CUMSENSOIL  = 0.0
      CUMSENSURFN = 0.0
      CUMSENSOILN = 0.0   

!***********************************************************************
!***********************************************************************
!     Daily OUTPUT
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT) THEN
      IF (YRDOY .LT. YRPLT .AND. YRPLT .GT. 0) RETURN

!     Accumulate senesced matter for surface and soil.
      CUMSENSURF  = CUMSENSURF  + SENESCE % ResWt(0) 
      CUMSENSURFN = CUMSENSURFN + SENESCE % ResE(0,1) 
      DO L = 1, NLAYR
        CUMSENSOIL  = CUMSENSOIL  + SENESCE % ResWt(L)
        CUMSENSOILN = CUMSENSOILN + SENESCE % ResE(L,1)
      ENDDO

!-----------------------------------------------------------------------
C     Check for output frequency
      IF ((MOD(DAS,FROP) .EQ. 0)          !Daily output every FROP days,
     &  .OR. (YRDOY .EQ. YRPLT)           !on planting date, and
     &  .OR. (YRDOY .EQ. MDATE)) THEN     !at harvest maturity 

C-----------------------------------------------------------------------
C        The following generates output for PlantGro.OUT
C-----------------------------------------------------------------------
!        PLTPOP = PLANTS                                   ! DG
        TOTWT  = (BIOMAS+RTWT)*PLANTS*10.0
        WTLF   = LFWT   * PLTPOP
        SDWT   = PANWT
        XLAI   = LAI

        IF (WTLF .GT. 0.0) THEN
           SLA = LAI * 10000.0 / WTLF
         ELSE
           SLA = 0.0
        ENDIF

        SEEDNO = GPSM
!        PODWT  = PANWT
!        IF (GPP .GT. 0.0) THEN
!           PODNO = SEEDNO/GPP
!         ELSE
!           PODNO = 0.0
!        ENDIF

        VSTAGE = REAL(LEAFNO)
        IF (LEAFNO .GT. 0) THEN
          RSTAGE = ISTAGE
        ELSE
          RSTAGE = 0
        ENDIF

        IF (PANWT .GT. 0.1) THEN
          SHELPC = GRNWT*100.0/PANWT
        ELSE
          SHELPC = 0.0
        ENDIF

!        SHELLW = PODWT - SDWT
        IF (SEEDNO .GT. 0.0) THEN
          SDSIZE = SDWT*PLTPOP/SEEDNO*1000.0
        ELSE
          SDSIZE = 0.0
        ENDIF

        IF (BIOMAS .GT. 0.0 .AND. GRNWT .GE. 0.0) THEN
          HI = GRNWT / BIOMAS
        ELSE
          HI = 0.0
        ENDIF

        GM2KG  = PLTPOP * 10.0

        DAP = MAX(0,TIMDIF(YRPLT,YRDOY))
        IF (DAP > DAS) DAP = 0
        CALL YR_DOY(YRDOY, YEAR, DOY) 

C-----------------------------------------------------------------------
C        The following generates output for file PlantN.OUT
C-----------------------------------------------------------------------
        IF (ISWNIT .EQ. 'Y') THEN
          WTNCAN = (STOVN + GRAINN) * PLTPOP
          IF ((LFWT+STMWT) .GT. 0.0) THEN
             WTNLF = STOVN * (LFWT  / (LFWT + STMWT)) * PLTPOP
             WTNST = STOVN * (STMWT / (LFWT + STMWT)) * PLTPOP
           ELSE
             WTNLF = 0.0
             WTNST = 0.0
          ENDIF
          WTNSD = GRAINN * PLTPOP
          WTNRT = ROOTN * PLTPOP        ! Is this right?
          WTNSH = 0.0
!         No longer used for output of Nuptake
          WTNUP = (STOVN+GRAINN)*PLANTS   
          WTNFX = 0.0
          NFIXN = 0.0

          IF (LFWT .GT. 0.0) THEN
             PCNL = WTNLF/(LFWT * PLTPOP) * 100.0
           ELSE
             PCNL = 0.0
          ENDIF
          IF (STMWT .GT. 0.0) THEN
             PCNST = WTNST/(STMWT * PLTPOP) * 100.0
           ELSE
             PCNST = 0.0
          ENDIF
          IF (RTWT .GT. 0) THEN
             PCNRT = ROOTN/RTWT * 100.0
           ELSE
             PCNRT = 0.0
          ENDIF

          WTNVEG  = (WTNLF + WTNST)
          WTNGRN  = (WTNSH + WTNSD)
          IF ((WTLF+STMWT).GT. 0.0) THEN
             PCNVEG = (WTNLF+WTNST)/(WTLF+(STMWT * PLTPOP))*100
           ELSE
             PCNVEG = 0.0
          ENDIF
          IF (SDWT.GT. 0.001) THEN
             PCNGRN = WTNSD / (SDWT * PLTPOP) * 100.0
           ELSE
             PCNGRN = 0.0
          ENDIF

          WRITE (NOUTPN,310)YEAR, DOY, DAS, DAP,
     &      (WTNCAN*10.0), (WTNSD*10.0), (WTNVEG*10.0), PCNGRN, PCNVEG,
!    &      (WTNUP*10.0), (WTNLF*10.0), (WTNST*10.0), PCNL,
     &      CumNUptake, (WTNLF*10.0), (WTNST*10.0), PCNL,
     &      PCNST, PCNSH, PCNRT, CUMSENSURFN, CUMSENSOILN
 
  310       FORMAT (1X,I4,1X,I3.3,2(1X,I5),
     &        3(1X,F5.1),2(1X,F5.2),1X,F5.1,
     &        2(1X,F5.1),4(1X,F5.2),2(1X,F6.2))
        ENDIF

        IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN   ! VSH, CHP

        WRITE (NOUTDG,400)YEAR, DOY, DAS, DAP,VSTAGE,RSTAGE,XLAI,
     &       NINT(WTLF*10),NINT(STMWT*GM2KG),NINT(GRNWT*GM2KG),
     &       NINT(RTWT*GM2KG),NINT(PANWT*GM2KG),NINT(BIOMAS*GM2KG),
     &       NINT(SEEDNO),SDSIZE,HI,NINT((TILNO+1.)*PLTPOP),(1.0-SWFAC),
     &       (1.0-TURFAC),SATFAC,(1.0-NSTRES),(1.0-KSTRES),PCNL,SHELPC,
     &       SLA,CANHT,CANWH,(RTDEP/100),(RLV(I),I=1,5),
     &       NINT(CUMSENSURF), NINT(CUMSENSOIL), DTT
 400      FORMAT (1X,I4,1X,I3.3,2(1X,I5),
     &        1X,F5.1,1X,I5,1X,F5.2,7(1X,I5),
     &        1X,F5.1,1X,F5.3,1X,I5,5(1X,F5.3),2(1X,F6.2),1X,F5.1,
     &        2(1X,F5.2),6(1X,F5.2)  !)
     &        ,2(1X,I6),F7.2 ) 

!     chp 2021-08-23
!     error #6633 The type of the actual argument differs
!     from the type of the dummy argument [DTT]
!     I don't want to debug this right now, so I'm commenting it out.
 !!      VSH CSV output corresponding to PlantGro.OUT
 !       ELSEIF (FMOPT == 'C') THEN 
 !         DO L = 1, 5
 !           RLV5(L) = RLV(L)
 !         ENDDO    
 !         CALL CsvOut_RICER(EXPNAME,CONTROL%RUN,CONTROL%TRTNUM, 
 !    &       CONTROL%ROTNUM,CONTROL%REPNO, YEAR, DOY, DAS, DAP, 
 !    &       VSTAGE,RSTAGE,XLAI,
 !    &       WTLF*10,STMWT*GM2KG,GRNWT*GM2KG,
 !    &       RTWT*GM2KG,PANWT*GM2KG,(BIOMAS*GM2KG),
 !    &       SEEDNO,SDSIZE,HI,((TILNO+1.)*PLTPOP),(1.0-SWFAC),
 !    &       (1.0-TURFAC),SATFAC,(1.0-NSTRES),(1.0-KSTRES),PCNL,SHELPC,
 !    &       SLA,CANHT,CANWH,(RTDEP/100), RLV5,
 !    &       CUMSENSURF, CUMSENSOIL, DTT,
 !    &       vCsvlineRICER, vpCsvlineRICER, vlngthRICER) 
 !         CALL LinklstRICER(vCsvlineRICER)
        ENDIF
      ENDIF

!***********************************************************************
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASEND) THEN
!***********************************************************************
        CLOSE (NOUTDG)
        CLOSE (NOUTPN)

!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE TEFF_OPGROW

C=======================================================================

!------------------------------------------------------------------------------------------------------------------------------------------
!                         DEFINITIONS
!-----------------------------------------------------------------------------------------------------------------------------------------
!BIOMAS      Above ground biomass, g/m2
!CARBO     	 Daily biomass production, g/plant/day
!DOY		     Day of year
!DTT         Growing degree days today, C
!DYNAMIC     Main control variable to tell each module which section of code to run
!GPP   		   Grain number per plant, grains/plant
!GPSM    	   Grain numbers, grains/m2
!GRAINN   	 Grain nitrogen content, g N/plant
!GRNWT	  	 Grain weight, g/plant
!I         	 Counter
!ISTAGE 		 Growth stage (integer)
!ISWNIT  	   Nitrogen balance switch (Y/N) 
!L  		     Index counter
!LAI   		   Leaf area index, m2/m2
!LEAFNO		   Number of oldest leaf per plant (same as XN)
!LFWT		     Leaf weight, g/plant
!MDATE 		   Year and day of year of maturity
!NLAYR 		   Number of soil layer
!NSTRES 	   Nitrogen stress factor affecting growth (0-1)
!OUTPUT   	 Program control variable to output state and rate variables to output file (value=5)
!PCNVEG	     Percent nitrogen in vegetative tissue (leaf and stem), kg N/ha
!PCNGRN      Percent nitrogen in grain,%
!PLTPOP 	   Plant population, plants/m2
!PODWT		   Dry mass of seeds plus shells, including C and N (g[pods] / m2[ground])
!RLV (L) 	   Root length density for soil layer L, cm root/cm3 soil 
!ROOTN  	   Root nitrogen content, g N/plant
!RTDEP 		   Root depth (cm)
!RTWT     	 Root weight, g/plant
!SDWT      	 Seed weight, g/m2
!STMWT       Stem weight, g/plant
!STOVN 		   Nitrogen content in stover, g N/plant
!SWFAC  	   Soil water stress effect on growth (0-1), 1 is no stress, 0 is full
!TURFAC		   Soil water stress effect on expansion (0-1), 1 is no 
!            stress, 0 is full stress
!WTLF  		   Dry mass of leaf tissue including C and N (g[leaf] / m2[ground])
!XN   		   Number of oldest expanding leaf
!YR_DOY  	   Year and day of year
!YRPLT    	 Planting date (YYDDD)                             