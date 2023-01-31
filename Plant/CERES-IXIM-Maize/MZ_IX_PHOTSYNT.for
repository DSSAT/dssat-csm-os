C=======================================================================
C  PHOTSYNT, Subroutine, J.I. Lizaso
C  Computes canopy daily gross photosynthesis (g biomass/m2 d) by 
C  integrating each leaf contribution over hourly time steps in the day.
C  Adapted from ETPHOT by N.B. Pickering
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  02/07/2003 JIL Written
C  09/12/2007 JIL Modified and adapted for IXIM model
C-----------------------------------------------------------------------
C  Called from: GROWTH
C  Calls:       RADABS,PLANTG,RESPIR
C=======================================================================
	SUBROUTINE MZ_IX_PHOTSYNT (DYNAMIC,                     !Control
     &      AMTRH,AZIR,DUL,GDDAE,GLA,GREAR,GRLF,                !Input
     &      FILES,GRGRN, GRRT, GRSTM, PCNL, PCNST, PCNRT,       !Input
     &      PATHSR,PCNSD, LAP, LFL, LFN, PLTPOP, Z2STAGE,       !Input
     &      ROWSPC,SALB,SW,YX,WEATHER,WTMAIN,TURFAC,            !Input
     &      PG,MAINR,CVF,PLIGLF,PLIGRT)                         !Output

      USE ModuleDefs

      IMPLICIT NONE
      EXTERNAL MZ_IX_PLANTG, MZ_IX_RADABS, MZ_IX_RESPIR
      EXTERNAL ERROR, FIND, GETLUN, IGNORE
      SAVE

      REAL        AMPLT      
      REAL        AMTRH(TS)      
      REAL        AS         
      REAL        ASMAX
      REAL        AZIR       
      REAL        AZZON(TS)  
      REAL        BETA(TS)   
      REAL        BETN       
      CHARACTER*80 C80      
      REAL        CANH       
      REAL        CANHT      
      REAL        CANS       
      REAL        CANW       
      REAL        CANWH      
      REAL        CVF        
      REAL        CVX        
      LOGICAL     DAYTIM
      REAL        DUL        
      INTEGER	  DYNAMIC      
      INTEGER     ERR 
      CHARACTER*6     ERRKEY                
!      CHARACTER*12    FILEC     
      CHARACTER*92    FILECC
      CHARACTER*12    FILES      
      INTEGER     FOUND        
      REAL        FRDIFP(TS) 
      REAL        FROLL      
      REAL        GDDAE      
      REAL        GLA(50)    
      REAL        GREAR      
      REAL        GRGRN      
      REAL        GRLF       
      REAL        GRRT       
      REAL        GRSTM      
      INTEGER     H
      REAL        HLAI       
      REAL        HS         
      INTEGER     I
      REAL        INSP       
      INTEGER     ISECT
      REAL        LAP(50)    
      REAL        LFL(50)    
      REAL        LFLN       
      REAL        LFLP       
      INTEGER     LFN
      LOGICAL     LIGHT
      INTEGER     LNUM                
      INTEGER     LUNCRP      
      REAL        MAINR      
      REAL        MULT       
      REAL        PALB       
      REAL        PALBD      
      REAL        PANG       
      REAL        PARHR(TS)  
      REAL        PARSH(50) 
      REAL        PARSUN(50)
      CHARACTER*80    PATHSR      
      REAL        PCAREA     
      REAL        PCARLF     
      REAL        PCARRT     
      REAL        PCARSD     
      REAL        PCARST     
      REAL        PCNL       
      REAL        PCNRT      
      REAL        PCNSD      
      REAL        PCNST      
      REAL        PG         
      REAL        PGDAY      
      REAL        PGHR       
!      REAL        PI         
      REAL        PLAISH(50)
      REAL        PLAISL(50)
      REAL        PLIGEA     
      REAL        PLIGLF     
      REAL        PLIGRT     
      REAL        PLIGSD     
      REAL        PLIGST     
      REAL        PLIPEA     
      REAL        PLIPLF     
      REAL        PLIPRT     
      REAL        PLIPSD     
      REAL        PLIPST     
      REAL        PLTPOP     
      REAL        PMINEA     
      REAL        PMINLF     
      REAL        PMINRT     
      REAL        PMINSD     
      REAL        PMINST     
      REAL        POAEA      
      REAL        POALF      
      REAL        POART      
      REAL        POASD      
      REAL        POAST      
      REAL        PPROEA     
      REAL        PPROLF     
      REAL        PPRORT     
      REAL        PPROSD     
      REAL        PPROST     
      REAL        R30C2      
!      REAL        RAD        
      REAL        RES30C     
      REAL        ROWSPC     
      REAL        RSPC       
      REAL        SALB       
      REAL        SCVP       
      CHARACTER*6     SECTION       
      REAL        SNDN       
      REAL        SNUP       
      REAL        SW         
      REAL        TAIRHR(TS) 
      REAL        TINCR      
      REAL        TRSFAC     
      REAL        TURFAC     
      REAL        WTMAIN     
      REAL        XC         
      REAL        YX(50)     
      REAL        Z2STAGE    

	PARAMETER (TINCR=24.0/TS)
!	PARAMETER (PI=3.14159, RAD=PI/180.0, PANG=64.0)
	PARAMETER (PANG=64.0)
	PARAMETER (CANS=4.5, SCVP=0.2)
	PARAMETER (INSP=4.514, AS=288.615, CVX=-75.774)

	TYPE (WeatherType) WEATHER

!     Transfer values from constructed data types into local variables.
      AZZON = WEATHER % AZZON
      BETA = WEATHER % BETA
      FRDIFP = WEATHER % FRDIFP
      PARHR = WEATHER % PARHR
	SNDN = WEATHER % SNDN
	SNUP = WEATHER % SNUP
	TAIRHR = WEATHER % TAIRHR

!----------------------------------------------------------------------
!                     DYNAMIC = RUNINIT
!----------------------------------------------------------------------

      IF(DYNAMIC.EQ.RUNINIT.OR.DYNAMIC.EQ.SEASINIT) THEN

!     ****************************************************************
!                     READ SPECIES FILE
!     ****************************************************************
      FILECC =  TRIM(PATHSR) // FILES
      CALL GETLUN('FILEC', LUNCRP)
      OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,0)

      SECTION = '*PHOTO'
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)                
        READ(C80,'(8X,F6.2)',IOSTAT=ERR) ASMAX
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(8X,F6.2)',IOSTAT=ERR) XC
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(8X,F6.2)',IOSTAT=ERR) CANH
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
        
      ENDIF
      REWIND(LUNCRP)

      SECTION = '*RESPI'
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(1X,F8.6,F7.4)',IOSTAT=ERR) RES30C, R30C2
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

      ENDIF
      REWIND(LUNCRP)

      SECTION = '*PLANT'
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(5F6.3)',IOSTAT=ERR) PCARLF,PCARST,PCARRT,
     &                                       PCAREA,PCARSD
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(5F6.3)',IOSTAT=ERR) PPROLF,PPROST,PPRORT,
     &                                       PPROEA,PPROSD
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(5F6.3)',IOSTAT=ERR) PLIPLF,PLIPST,PLIPRT,
     &                                       PLIPEA,PLIPSD
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(5F6.3)',IOSTAT=ERR) PLIGLF,PLIGST,PLIGRT,
     &                                       PLIGEA,PLIGSD
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(5F6.3)',IOSTAT=ERR) POALF, POAST, POART, 
     &                                       POAEA, POASD
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(5F6.3)',IOSTAT=ERR) PMINLF,PMINST,PMINRT,
     &                                       PMINEA,PMINSD
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

      ENDIF
      REWIND(LUNCRP)

      CLOSE (LUNCRP)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      AMPLT         = 0.0
      BETN          = 0.0
      CANHT         = 0.0
      CANW          = 0.0
      CANWH         = 0.0
      CVF           = 0.0
      DAYTIM        = .FALSE.
      FROLL         = 0.0
      H             = 0
      HLAI          = 0.0
      HS            = 0.0
      I             = 0
      LFLN          = 0.0
      LFLP          = 0.0
      LIGHT         = .TRUE.
      MAINR         = 0.0
      MULT          = 0.0
      PALB          = 0.0
      PALBD         = 0.0
      PG            = 0.0
      PGDAY         = 0.0
      PGHR          = 0.0
      RSPC          = 0.0
      SNDN          = 0.0
      SNUP          = 0.0
      TRSFAC        = 0.0
      
!      chp 2/23/2009 added array index for tairhr -- I know this is wrong, but
!        what does it need to be?  Call subroutine in a loop from 1 to 24?
        CALL MZ_IX_PLANTG(DYNAMIC,                              !Control
     &      ASMAX, GDDAE, GLA, PLAISL, PLAISH, LAP, LFL, LFN,   !Input
     &      LIGHT, PARSH, PARSUN, TAIRHR(1), YX,                !Input
     &      PGHR)                                               !Output
	  
	  CALL MZ_IX_RESPIR(DYNAMIC,                              !Control
     &  PG, R30C2, RES30C, TAIRHR, WTMAIN,                        !Input
     &  PCARLF,PCARST,PCARRT,PCAREA,PCARSD,PPROLF,PPROST,PPRORT,  !Input
     &  PPROEA,PPROSD,PLIPLF,PLIPRT,PLIPST,PLIPEA,PLIPSD,PLIGLF,  !Input
     &  PLIGST,PLIGRT,PLIGEA,PLIGSD,POALF,POAST,POART,POAEA,POASD,!Input
     &  PMINLF,PMINST,PMINRT,PMINEA,PMINSD,                       !Input
     &  GRLF,GRRT,GRSTM,GREAR,GRGRN,                              !Input
     &  MAINR, CVF)                                              !Output
	  
!-----------------------------------------------------------------------
!                     DYNAMIC = INTEGR
!-----------------------------------------------------------------------
      ELSEIF(DYNAMIC.EQ.INTEGR) THEN   

C ** JIL Making protein content in tissues a function of N content

	  IF (PCNL .GT. 0.0) THEN
	    PPROLF = PCNL * 6.25 / 100.0
	    PCARLF = 1.0 - (PPROLF+PLIPLF+PLIGLF+POALF+PMINLF)
	  ENDIF

	  IF (PCNST .GT. 0.0) THEN
	    PPROST = PCNST * 6.25 / 100.0
	    PCARST = 1.0 - (PPROST+PLIPST+PLIGST+POAST+PMINST)
	  ENDIF

	  IF (PCNRT .GT. 0.0) THEN
	    PPRORT = PCNRT * 6.25 / 100.0
	    PCARRT = 1.0 - (PPRORT+PLIPRT+PLIGRT+POART+PMINRT)
	  ENDIF

	  IF (PCNSD .GT. 0.0) THEN
	    PPROSD = PCNSD * 6.25 / 100.0
	    PCARSD = 1.0 - (PPROSD+PLIPSD+PLIGSD+POASD+PMINSD)
	  ENDIF

C ** JIL Calculate canopy growth and update LAI using updated green leaf area

        RSPC = ROWSPC / 100.0
	  IF (Z2STAGE .LE. 1.0) THEN
	     CANHT = 1.85*CANH/(1+EXP(-CANS*(Z2STAGE-0.95)))
	  ENDIF

	  HLAI = 0.0
	  DO I=1,LFN
	    IF (GLA(I) .LT. 0.0) GLA(I)=0.0
	    HLAI = HLAI + GLA(I)*PLTPOP*0.0001
	    IF (LAP(I) .GE. 0.8*YX(I)) THEN
	       LFLN=(((INSP*GLA(I)+AS)-((INSP*GLA(I)+AS)**2.0-
     &		   (4.0*INSP*GLA(I)*AS*CVX))**0.5)/(2.0*CVX))
		   LFLP = LFLN * COS(PANG*RAD)
		   CANW = LFLP *2.0/100.0
	       CANWH = MIN(MAX(CANWH,CANW),RSPC)
          ENDIF
	  END DO
        IF (CANHT.GT.0.0 .AND. CANHT.LT.0.01) CANHT = 0.01
        IF (CANHT.GT.0.01 .AND. CANWH.LT.0.01) CANWH = 0.01

C ** JIL Calculate between plant spacing (m)

      IF (RSPC.GT.0.0 .AND. PLTPOP.GT.0.0) THEN
        BETN = 1.0 / (RSPC*PLTPOP)
      ELSE
        BETN = 0.0
      ENDIF

C ** JIL Calculate plant albedo to PAR as a function of surface SW

	PALB = 0.6 * SALB
	IF (SW .LT. DUL) THEN
	   PALBD = PALB * 1.25
	   PALB = PALBD - (PALBD-PALB)/DUL * SW
	ENDIF

C ** JIL Begining hourly loop

	  TRSFAC = 0.0
	  PGDAY = 0.0
	  LIGHT = .TRUE.
        DO H=1,TS

C         Calculate effect of leaf rolling

	    MULT  = 5.0 - 4.0*TURFAC
	    AMPLT = (25.0+(100.0-25.0)*EXP(-3.5*(1.0-AMTRH(H))))*MULT
	    FROLL = AMIN1(TURFAC+(REAL(H)-14.0)**2.0/AMPLT,1.0)
C	    FROLL = 1.0                 ! Remove the comment to remove leaf rolling JIL

C         Calculate real and solar time.

          HS = REAL(H) * TINCR
          IF (HS.GT.SNUP .AND. HS.LT.SNDN) THEN
            DAYTIM = .TRUE.
          ELSE
            DAYTIM = .FALSE.
          ENDIF

C         Calculate hourly radiation absorption by canopy/soil.

          CALL MZ_IX_RADABS(
     &      AZIR, AZZON(H), BETA(H), BETN, CANHT, CANWH,          !Input
     &      DAYTIM, FRDIFP(H), FROLL, GLA, H, LFN, PALB,          !Input
     &      PARHR(H), PLTPOP, RSPC, SCVP, HLAI, XC,               !Input
     &      PARSH, PARSUN, PLAISH, PLAISL)                       !Output

C         Calculate instantaneous gross assimilation

          CALL MZ_IX_PLANTG(DYNAMIC,                            !Control
     &      ASMAX, GDDAE, GLA, PLAISL, PLAISH, LAP, LFL, LFN,     !Input
     &      LIGHT, PARSH, PARSUN, TAIRHR(H), YX,                  !Input
     &      PGHR)                                                !Output

C         Integrate instantaneous canopy photoynthesis (µmol CO2/m2 s)
C         to get daily values (g CO2/m2 d)

          PGDAY = PGDAY + TINCR*PGHR*44.0*0.0036

        END DO

C ** JIL Ending hourly loop
C ** JIL Calculating daily gross assimilation (g CH2O/m2 d).

        PG = PGDAY*30.0/44.0

C ** JIL Calculating maintenance and growth respiration

	  CALL MZ_IX_RESPIR(DYNAMIC,                              !Control
     &  PG, R30C2, RES30C, TAIRHR, WTMAIN,                        !Input
     &  PCARLF,PCARST,PCARRT,PCAREA,PCARSD,PPROLF,PPROST,PPRORT,  !Input
     &  PPROEA,PPROSD,PLIPLF,PLIPRT,PLIPST,PLIPEA,PLIPSD,PLIGLF,  !Input
     &  PLIGST,PLIGRT,PLIGEA,PLIGSD,POALF,POAST,POART,POAEA,POASD,!Input
     &  PMINLF,PMINST,PMINRT,PMINEA,PMINSD,                       !Input
     &  GRLF,GRRT,GRSTM,GREAR,GRGRN,                              !Input
     &  MAINR, CVF)                                              !Output

	  IF(MAINR .GT. PG) THEN
	    MAINR = PG
	  ENDIF
      
      ENDIF       !Endif for DYNAMIC LOOP

	RETURN

	END
