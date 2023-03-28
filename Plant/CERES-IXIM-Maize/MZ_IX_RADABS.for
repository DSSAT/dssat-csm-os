C=======================================================================
C  RADABS, Subroutine, J.I. Lizaso
C  Calculates hourly canopy absorption of PAR (J/m2 s) by shaded and 
C  sunlit leaf area
C  Adapted from RADABS by N.B. Pickering
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  02/07/2003 JIL Written
C  09/12/2007 JIL Adapted for IXIM model
C-----------------------------------------------------------------------
C  Called from: PHOTSYNT
C  Calls:       SHADOW,LFEXTN,CANABS
C=======================================================================
      SUBROUTINE MZ_IX_RADABS(
     &  AZIR, AZZON, BETA, BETN, CANHT, CANWH, DAYTIM,         !Input
     &  FRDIFP, FROLL, GLA, H, LFN, PALB, PARH, PLTPOP,        !Input
     &  ROWSPC, SCVP, HLAI, XC,                                !Input
     &  PARSH, PARSUN, PLAISH, PLAISL)                         !Output

      IMPLICIT NONE
      EXTERNAL MZ_IX_SHADOW, MZ_IX_LFEXTN, MZ_IX_CANABS
      SAVE

      REAL     AZIR
      REAL     AZZON
      REAL     BETA
      REAL     BETN
      REAL     CANHT
      REAL     CANWH
      LOGICAL  DAYTIM
      REAL     FRACSH     
      REAL     FRDIFP     
      REAL     FROLL      
      REAL     FRSHV      
      REAL     GLA(50)    
      INTEGER  H
      REAL     HLAI       
      INTEGER  I
      REAL     KDIFBL     
      REAL     KDIRBL     
      REAL     KDRBLV     
      REAL     LAISH      
      REAL     LAISHV     
      REAL     LAISL      
      REAL     LAISLV     
      INTEGER  LFN
      REAL     PALB       
      REAL     PARH      
      REAL     PARSH(50) 
      REAL     PARSL      
      REAL     PARSS      
      REAL     PARSUN(50)
      REAL     PCABSP     
      REAL     PCABSR     
      REAL     PCINTP     
      REAL     PCINTR     
      REAL     PCREFP     
      REAL     PCREFR     
      REAL     PLAISH(50)
      REAL     PLAISL(50)
      REAL     PLTPOP     
      REAL     ROWSPC     
      REAL     SCVP       
      REAL     XC         

C     Initialize.
      IF (H .EQ. 1) THEN     !Daily initialization
        FRSHV  = 0.0
        LAISHV = 0.0
        LAISLV = 0.0
      ENDIF
      PCABSP = 0.0           !Hourly initialization
      PCINTP = 0.0
      PCREFP = 0.0
      PARSL = 0.0
      PARSS = 0.0
      PCABSR = 0.0
      PCINTR = 0.0
      PCREFR = 0.0
	DO I=1,50
	   PARSUN(I) = 0.0
	   PARSH(I) = 0.0
	END DO

      IF (HLAI .GT. 0.000) THEN

C       Calculate fraction shaded and LAI's for vertical sun position.

        IF (H .EQ. 1) THEN

          CALL MZ_IX_SHADOW(
     &      AZIR, AZZON, 90.0, BETN, CANHT, CANWH, ROWSPC,     !Input
     &      FRSHV)                                             !Output

          CALL MZ_IX_LFEXTN(
     &      90.0, FROLL, FRSHV, H, HLAI, XC,                   !Input
     &      KDIFBL, KDRBLV, LAISHV, LAISLV)                    !Output
        ENDIF

        IF (DAYTIM) THEN

C         Calculate fraction shaded, leaf extinction and LAI's.

          CALL MZ_IX_SHADOW(
     &      AZIR, AZZON, BETA, BETN, CANHT, CANWH, ROWSPC,     !Input
     &      FRACSH)                                            !Output
          CALL MZ_IX_LFEXTN(
     &      BETA, FROLL, FRACSH, H, HLAI, XC,                  !Input
     &      KDIFBL, KDIRBL, LAISH, LAISL)                      !Output

C         Calculate PAR absorbed by canopy during day.

          CALL MZ_IX_CANABS(
     &      PALB, BETA, BETN, CANHT, CANWH, FRACSH, FROLL,     !Input
     &      GLA, LFN, PLTPOP, FRDIFP, KDIFBL, KDIRBL,          !Input
     &      PARH, ROWSPC, SCVP,                                !Input
     &      PCABSP, PCINTP, PCREFP, PLAISH, PLAISL,            !Output
     &      PARSH, PARSUN)                                     !Output

        ELSE

C         Night time with canopy.

          FRACSH = FRSHV
          KDIRBL = KDRBLV
          LAISH = LAISHV
          LAISL = LAISLV
        ENDIF
      ELSE

C       Bare soil (day or night).

        PCABSP = (1.0-PALB) * 100.0
        PCREFP = PALB * 100.0
        PARSS = (1.0-PALB) * PARH
      ENDIF

	RETURN
      END !SUBROUTINE RADABS

C=======================================================================
C  SHADOW, Subroutine, N.B. Pickering, J.W. Jones
C  Calculates fraction shaded for sun and row geometry using ellipses.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  03/14/1991 NBP Written
C  11/15/1991 NBP Modified
C  11/23/1993 NBP Included more error checks and limits
C  09/12/2007 JIL Modified and adapted for IXIM model
C-----------------------------------------------------------------------
C  Called from: RADABS
C  Calls:       
C=======================================================================

      SUBROUTINE MZ_IX_SHADOW(
     &  AZIR, AZZON, BETA, BETN, CANHT, CANWH, ROWSPC,       !Input
     &  FRACSH)                                              !Output

      IMPLICIT NONE
      SAVE      

      REAL     A
      REAL     B
      REAL     C1
      REAL     C2
      REAL     C3
      REAL     C4
      REAL     AZIMD
      REAL     AZIR
      REAL     AZZON
      REAL     BETA
      REAL     BETN
      REAL     CANHT
      REAL     CANWH
      REAL     ETA
      REAL     FRACSH
      REAL     GAMMA
      REAL     PI
      REAL     RAD
      REAL     RBETA
      REAL     ROWSPC
      REAL     SHADE
      REAL     SHLEN
      REAL     SHPERP
      REAL     STOUCH
      REAL     ZERO
      
      PARAMETER (PI=3.14159, RAD=PI/180.0, ZERO=1.0E-6)

C     Initialization

      A       = 0.0
      B       = 0.0
      C1      = 0.0
      C2      = 0.0
      C3      = 0.0
      C4      = 0.0
      AZIMD   = 0.0
      ETA     = 0.0
      FRACSH  = 0.0
      GAMMA   = 0.0
      RBETA   = 0.0
      SHADE   = 0.0
      SHLEN   = 0.0
      SHPERP  = 0.0
      STOUCH  = 0.0
      
C     Set fraction shaded to 0.0 for zero width or height.

      IF (CANWH .LE. ZERO .OR. CANHT .LE. ZERO) THEN
        FRACSH = 0.0

C     Set fraction shaded to 1.0 for full cover.

      ELSE IF (CANWH .GE. ROWSPC) THEN
        FRACSH = 1.0

C     Calculate fraction shaded.

      ELSE

C       Adjust BETA if sun exactly overhead or at horizon.  Calculate
C       acute positive angle between sun and row azimuths. Initialize
C       other constants.

        RBETA = MIN(MAX(BETA*RAD,1.0E-6),PI/2.0-1.0E-6)
        AZIMD = ABS(AZZON-AZIR)*RAD
        IF (AZIMD .GT. PI/2.0) AZIMD = PI - AZIMD
        A = (CANWH/CANHT)**2
        GAMMA = ATAN(A*TAN(RBETA))
        C1 = A*(TAN(RBETA))**2
        C2 = (A*TAN(RBETA))**2

C       Calculate shadow length assuming elliptical plant.

        SHLEN = CANHT * COS(RBETA-GAMMA) / SIN(RBETA) *
     &    SQRT((1.0+C2)/(1.0+C1))
        B = (SHLEN/CANWH)**2
        C3 = B*(TAN(AZIMD))**2
        C4 = (B*TAN(AZIMD))**2
        STOUCH = SHLEN / (COS(AZIMD) * SQRT(1.+C3))

C       CALCULATE FRACTION SHADED.

C       Sun parallel to row.  Shadow limited to BETN.

        IF (AZIMD .LE. ZERO) THEN
          SHLEN = MIN(SHLEN,BETN)
          SHADE = 0.25 * PI * SHLEN * CANWH

C       Sun not parallel to row.

        ELSE

C         Calculate perpendicular shadow length.

          AZIMD = MAX(AZIMD,1.0E-6)
          ETA = ATAN(1.0/(B*TAN(AZIMD)))
          SHPERP = CANWH*SIN(AZIMD+ETA)*SQRT((1.0+C4)/(1.0+C3))

C         Hedgerow (plant shadows overlap).

          IF (STOUCH .GE. BETN) THEN

C           Shadow length is perpendicular and limited to ROWSPC.

            SHLEN = MIN(SHPERP,ROWSPC)
            SHADE = SHLEN * BETN

C         Individual plants.

          ELSE

C           Limit shadow length to within one ROWSPC.

            IF (SHPERP .GT. ROWSPC) SHLEN = SHLEN * ROWSPC/SHPERP
            SHADE = 0.25 * PI * SHLEN * CANWH

          ENDIF
        ENDIF

        FRACSH = MIN(SHADE/(ROWSPC*BETN),1.0)

      ENDIF

      FRACSH = MIN(MAX(FRACSH,1.0E-6),1.0)

      END !SUBROUTINE MZ_IX_SHADOW

C=======================================================================
C  LFEXTN, Subroutine, N.B. Pickering, K.J. Boote
C  Computes leaf extinction coefficients based on leaf angle distribution
C  (Goudriaan, 1988) and sunlit and shaded leaf area indices.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  ??/??/???? KJB Written
C  05/14/1991 NBP Removed COMMON and reorganized.
C  08/31/2004 JIL Adapted for Maize photosynthesis
C  09/12/2007 JIL Modified and adapted for IXIM model
C-----------------------------------------------------------------------
C  Called from: RADABS
C  Calls:       
C=======================================================================

      SUBROUTINE MZ_IX_LFEXTN(
     &  BETA, FROLL, FRACSH, H, HLAI, XC,                    !Input
     &  KDIFBL, KDIRBL, LAISH, LAISL)                        !Output

      IMPLICIT NONE
      SAVE      
      
      REAL     BETA
      REAL     EXPMIN
      REAL     EXPONENT
      REAL     FRACSH
      INTEGER  H
      INTEGER  I
      REAL     KDIRBL
      REAL     KDIFBL
      REAL     LAISH
      REAL     FROLL
      REAL     LAISL
      REAL     PI
      REAL     RAD
      REAL     HLAI
      REAL     BT
      REAL     KDIR
      REAL     TAUDIFF
      REAL     TAUDIR
      REAL     XC
      
      PARAMETER (PI = 3.14159, RAD = PI/180.0, EXPMIN=-40.0)

C     Calculate black leaf extinction coefficients 
C     for diffuse and direct radiation
C     Campbell (1986); Campbell (1990); Campbell and Norman (1998)
C     Initialize

      IF (H .EQ. 1) THEN     !Daily initialization
        BT       = 0.0
        KDIFBL   = 0.0
        KDIR     = 0.0
        TAUDIFF  = 0.0
        TAUDIR   = 0.0
      ENDIF

      I        = 1           !Hourly initialization
      KDIRBL   = 0.0
      LAISH    = 0.0
      LAISL    = 0.0
      
C     Calculate KDIFBL once a day assuming Uniform OverCast sky (UOC)

	IF (H .EQ. 1) THEN
         TAUDIFF = 0.0
	   DO I = 1, 89, 2
	      BT = REAL(I)
            KDIR = ((XC**2.+1./(TAN(BT*RAD))**2.)**0.5)/
     &              (XC+1.744*(XC+1.182)**(-0.733))

!           Underflows  chp 8/5/2009
            EXPONENT = -KDIR*HLAI*FROLL
            IF (EXPONENT > EXPMIN) THEN
	        TAUDIR = EXP(-KDIR*HLAI*FROLL)
            ELSE
              TAUDIR = 0.0
            ENDIF

	      TAUDIFF = TAUDIFF + (TAUDIR*SIN(BT*RAD)*COS(BT*RAD))
	   ENDDO
	   TAUDIFF = 2.0*TAUDIFF*2.0*RAD
	   KDIFBL = -LOG(TAUDIFF)/(HLAI*FROLL)
	ENDIF

C     Calculate KDIRBL hourly as a function of solar elevation 

      KDIRBL = ((XC**2.+1./(TAN(BETA*RAD))**2.)**0.5)/
     &          (XC+1.744*(XC+1.182)**(-0.733))

C     Calculate sunlit and shaded leaf area indices.

      EXPONENT = -KDIRBL*HLAI*FROLL/FRACSH
      IF (EXPONENT > -40) THEN
        LAISL = (FRACSH/KDIRBL) * (1.0-EXP(-KDIRBL*HLAI*FROLL/FRACSH))
      ELSE
        LAISL = FRACSH/KDIRBL
      ENDIF

	IF (HLAI*FROLL .GT. 0.02) THEN
         LAISL = MAX(LAISL,0.02)
	  ELSE
	   LAISL = HLAI*FROLL
	ENDIF
      LAISH = HLAI*FROLL - LAISL

      END !SUBROUTINE MZ_IX_LFEXTN

C=======================================================================
C  CANABS, Subroutine, K.J. Boote, N.B. Pickering
C  Computes radiation absorbed by soil, sunlit and shaded leaves.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  ??/??/?? KJB Written
C  05/14/91 NBP Removed COMMON and reorganized.
C  08/31/04 JIL Adapted to photosynthesis in maize
C  09/12/2007 JIL Modified and adapted for IXIM model
C  06/21/2013 JIL Fixing an error in the absorption of reflected light
C-----------------------------------------------------------------------
C  Called from: RADABS
C  Calls:       
C=======================================================================

      SUBROUTINE MZ_IX_CANABS(
     &  ALBEDO, BETA, BETN, CANHT, CANWH, FRACSH, FROLL,     !Input
     &  GLA, LFN, PLTPOP, FRDIF, KDIFBL, KDIRBL, PARH,       !Input
     &  ROWSPC, SCVR,                                        !Input
     &  PCTABS, PCTINT, PCTREF, PLAISH, PLAISL,              !Output
     &  RADSH, RADSUN)                                       !Output 

      IMPLICIT NONE
      SAVE      

      REAL      ADDF
      REAL      ADDFSH    
      REAL      ADDFSL    
      REAL      ADDR      
      REAL      ADDRSL    
      REAL      ADIF      
      REAL      ADIFCAN
      REAL      ADIFSH    
      REAL      ADIFSL    
      REAL      ADIR      
      REAL      ADIRCAN
      REAL      ADIRSL    
      REAL      ALBEDO    
      REAL      AREF      
      REAL      AREFSH    
      REAL      AREFSL    
      REAL      ATOT      
      REAL      BETA      
      REAL      BETN      
      REAL      CANHT     
      REAL      CANWH     
      REAL      CUMASH    
      REAL      CUMASUN   
      REAL      DELWP     
      REAL      DELWR     
      REAL      DIFP      
      REAL      DIFPR     
      REAL      DIFR 
      REAL      EXPMIN
      REAL      EXPONENT     
      REAL      FRACSH    
      REAL      FRDIF     
      REAL      FROLL     
      REAL      GLA(50)   
      REAL      INCSOI    
      REAL      INTCAN    
      REAL      KDIFBL    
      REAL      KDIRBL    
      INTEGER   L
      INTEGER   LFN
      REAL      PARH     
      REAL      PATHP     
      REAL      PATHR     
      REAL      PCTABS    
      REAL      PCTINT    
      REAL      PCTREF    
      REAL      PI        
      REAL      PLAISH(50)
      REAL      PLAISL(50)
      REAL      PLTPOP    
      REAL      RAD       
      REAL      RADDIF    
      REAL      RADDIR    
      REAL      RADSH(50) 
      REAL      RADSS     
      REAL      RADSUN(50)
      REAL      RADTOT    
      REAL      RDIFSL    
      REAL      REFDF     
      REAL      REFDIF    
      REAL      REFDIR    
      REAL      REFDR     
      REAL      REFH      
      REAL      REFSOI    
      REAL      REFTOT    
      REAL      ROWSPC    
      REAL      SCVR      
      REAL      SINB      
      REAL      SQV       
      REAL      UCUMASH   
      REAL      UCUMASUN  
      REAL      UYLAISH   
      REAL      UYLAISL   
      REAL      YHLAI     
      REAL      YHLAICAN
      REAL      YLAICANSH
      REAL      YLAICANSL
      REAL      YLAISH    
      REAL      YLAISL    

      PARAMETER (PI=3.14159, RAD=PI/180.0, EXPMIN=-40.0)

C     Initialization.
      ADDF      = 0.0
      ADDFSH    = 0.0 
      ADDFSL    = 0.0 
      ADDR      = 0.0 
      ADDRSL    = 0.0 
      ADIF      = 0.0 
      ADIFCAN   = 0.0
      ADIFSH    = 0.0 
      ADIFSL    = 0.0 
      ADIR      = 0.0 
      ADIRCAN   = 0.0
      ADIRSL    = 0.0 
      AREF      = 0.0 
      AREFSH    = 0.0 
      AREFSL    = 0.0 
      ATOT      = 0.0 
      CUMASH    = 0.0 
      CUMASUN   = 0.0 
      DELWP     = 0.0 
      DELWR     = 0.0 
      DIFP      = 0.0 
      DIFPR     = 0.0 
      DIFR      = 0.0 
      INCSOI    = 0.0 
      INTCAN    = 0.0 
      L         = 1
      PATHP     = 0.0 
      PATHR     = 0.0 
      PCTABS    = 0.0 
      PCTINT    = 0.0 
      PCTREF    = 0.0 
      RADDIF    = 0.0 
      RADDIR    = 0.0 
      RADSS     = 0.0 
      RADTOT    = 0.0 
      RDIFSL    = 0.0 
      REFDF     = 0.0 
      REFDIF    = 0.0 
      REFDIR    = 0.0 
      REFDR     = 0.0 
      REFH      = 0.0 
      REFSOI    = 0.0 
      REFTOT    = 0.0 
      UCUMASH   = 0.0 
      UCUMASUN  = 0.0 
      UYLAISH   = 0.0 
      UYLAISL   = 0.0 
      YHLAI     = 0.0 
      YHLAICAN  = 0.0
      YLAICANSH = 0.0
      YLAICANSL = 0.0
      YLAISH    = 0.0 
      YLAISL    = 0.0 

	DO L=1,50
        PLAISH(L) = 0.0
        PLAISL(L) = 0.0
        RADSH(L)  = 0.0
        RADSUN(L) = 0.0
      ENDDO

      SQV = SQRT(1.0-SCVR)
      SINB = SIN(BETA*RAD)

C     Compute reflection coefficients for direct and diffuse light.

      REFH = (1.0-SQV) / (1.0+SQV)
      REFDR = 2.0*KDIRBL/(1.0+KDIRBL) * REFH
      REFDF = REFH

C     Split total radiation into direct and diffuse components.

      RADDIF = FRDIF * PARH
      RADDIR = PARH - RADDIF

C     Diffuse skylight is absorbed over an effective area equal to the
C     canopy height plus width for an isolated row.  For interfering rows,
C     Eqns. 2.130 and 2.128 from Goudriaan (1977) are used.  Concept
C     extended for both between plants (P) and rows (R).

      IF (CANWH .LT. BETN) THEN
        PATHP = BETN - CANWH
        DELWP = PATHP + CANHT - SQRT(PATHP**2+CANHT**2)
        DIFP = MIN((CANWH+DELWP)/BETN,1.0)
      ELSE
        DIFP = 1.0
      ENDIF
      IF (CANWH .LT. ROWSPC) THEN
        PATHR = ROWSPC - CANWH
        DELWR = PATHR + CANHT - SQRT(PATHR**2+CANHT**2)
        DIFR = MIN((CANWH+DELWR)/ROWSPC,1.0)
      ELSE
        DIFR = 1.0
      ENDIF

      DIFPR = MIN(MAX(DIFP*DIFR,1.0E-6),1.0)

C     Computing whole canopy absorbed direct and diffuse light for later
C     calculation of absorbed reflected light

      YHLAICAN = SUM(GLA(1:LFN))*FROLL*PLTPOP*0.0001
      EXPONENT = -KDIRBL*YHLAICAN/FRACSH
      IF (EXPONENT > EXPMIN) THEN
        YLAICANSL=(FRACSH/KDIRBL)*(1.0-EXP(-KDIRBL*YHLAICAN/FRACSH))
      ELSE
        YLAICANSL = FRACSH/KDIRBL
      ENDIF
      YLAICANSH = YHLAICAN - YLAICANSL

      EXPONENT = -KDIRBL*SQV*YHLAICAN/FRACSH
      IF (EXPONENT .GT. EXPMIN) THEN
        ADIRCAN = FRACSH * (1.0-REFDR) * RADDIR *
     &            (1.0-EXP(-KDIRBL*SQV*YHLAICAN/FRACSH))
      ELSE
        ADIRCAN = FRACSH * (1.0-REFDR) * RADDIR
      ENDIF
         
      EXPONENT = -KDIFBL*SQV*YHLAICAN/DIFPR
      IF (EXPONENT .GT. EXPMIN) THEN
        ADIFCAN = DIFPR * (1.0-REFDF) * RADDIF *
     &            (1.0-EXP(-KDIFBL*SQV*YHLAICAN/DIFPR))
      ELSE
        ADIFCAN = DIFPR * (1.0-REFDF) * RADDIF
      ENDIF

C ** JIL Beginning per-leaf loop
C     Direct radiation absorbed in shaded zone by considering the direct
C     (ADDR) and diffuse/scattered (ADDF) components of the direct beam.

	DO L=LFN,1,-1
	  IF(GLA(L) .GT. 0.0) THEN
	   YHLAI = YHLAI+GLA(L)*FROLL*PLTPOP*0.0001

         EXPONENT = -KDIRBL*YHLAI/FRACSH
         IF (EXPONENT > EXPMIN) THEN
           YLAISL=(FRACSH/KDIRBL) * (1.0-EXP(-KDIRBL*YHLAI/FRACSH))
         ELSE
           YLAISL = FRACSH/KDIRBL
         ENDIF

         YLAISH = YHLAI - YLAISL

         EXPONENT = -KDIRBL*SQV*YHLAI/FRACSH
         IF (EXPONENT > EXPMIN) THEN
           ADIR = FRACSH * (1.0-REFDR) * RADDIR *
     &          (1.0-EXP(-KDIRBL*SQV*YHLAI/FRACSH))
         ELSE
           ADIR = FRACSH * (1.0-REFDR) * RADDIR
         ENDIF

         EXPONENT = -KDIRBL*YHLAI/FRACSH
         IF (EXPONENT > EXPMIN) THEN
           ADDR = FRACSH * (1.0-SCVR) * RADDIR *
     &          (1.0-EXP(-KDIRBL*YHLAI/FRACSH))
         ELSE
           ADDR = FRACSH * (1.0-SCVR) * RADDIR
         ENDIF

         ADDF = ADIR - ADDR
         EXPONENT = -KDIRBL*SQV*YLAISL/FRACSH
         IF (EXPONENT > EXPMIN) THEN
           ADIRSL = FRACSH * (1.0-REFDR) * RADDIR *
     &          (1.0-EXP(-KDIRBL*SQV*YLAISL/FRACSH))
         ELSE
           ADIRSL = FRACSH * (1.0-REFDR) * RADDIR
         ENDIF

         EXPONENT = -KDIRBL*YLAISL/FRACSH
         IF (EXPONENT > EXPMIN) THEN
           ADDRSL = FRACSH * (1.0-SCVR) * RADDIR *
     &          (1.0-EXP(-KDIRBL*YLAISL/FRACSH))
         ELSE
           ADDRSL = FRACSH * (1.0-SCVR) * RADDIR
         ENDIF

         ADDFSL = ADIRSL - ADDRSL
         ADDFSH = ADDF - ADDFSL

C     Diffuse skylight is absorbed over an effective area equal to the
C     canopy height plus width for an isolated row.  For interfering rows,
C     Eqns. 2.130 and 2.128 from Goudriaan (1977) are used.  Concept
C     extended for both between plants (P) and rows (R).

!         IF (CANWH .LT. BETN) THEN
!           PATHP = BETN - CANWH
!           DELWP = PATHP + CANHT - SQRT(PATHP**2+CANHT**2)
!           DIFP = MIN((CANWH+DELWP)/BETN,1.0)
!         ELSE
!           DIFP = 1.0
!         ENDIF
!         IF (CANWH .LT. ROWSPC) THEN
!           PATHR = ROWSPC - CANWH
!           DELWR = PATHR + CANHT - SQRT(PATHR**2+CANHT**2)
!           DIFR = MIN((CANWH+DELWR)/ROWSPC,1.0)
!         ELSE
!           DIFR = 1.0
!         ENDIF

!         DIFPR = MIN(MAX(DIFP*DIFR,1.0E-6),1.0)
         EXPONENT = -KDIFBL*SQV*YHLAI/DIFPR
         IF (EXPONENT > EXPMIN) THEN
           ADIF = DIFPR * (1.0-REFDF) * RADDIF *
     &     (1.0-EXP(-KDIFBL*SQV*YHLAI/DIFPR))
         ELSE
           ADIF = DIFPR * (1.0-REFDF) * RADDIF
         ENDIF

         EXPONENT = -KDIFBL*SQV*YLAISL/DIFPR
         IF (EXPONENT > EXPMIN) THEN
           ADIFSL = DIFPR * (1.0-REFDF) * RADDIF *
     &     (1.0-EXP(-KDIFBL*SQV*YLAISL/DIFPR))
         ELSE
           ADIFSL = DIFPR * (1.0-REFDF) * RADDIF
         ENDIF

         ADIFSH = ADIF - ADIFSL

C     Light reflected from the soil assumed to be isotropic and diffuse.
C     Absorption handled in the same manner as diffuse skylight.
C     The flux is assumed to go bottom-up.

         REFDIR = FRACSH * REFDR * RADDIR
         REFDIF = DIFPR * REFDF * RADDIF
         INTCAN = REFDIR + REFDIF + ADIR + ADIF
         INCSOI = PARH - INTCAN
         REFSOI = ALBEDO * INCSOI

         EXPONENT = -KDIFBL*SQV*YHLAI/DIFPR
         IF (EXPONENT > EXPMIN) THEN
           AREF = DIFPR * (1.0-REFDF) * REFSOI *
     &     (1.0-EXP(-KDIFBL*SQV*YHLAI/DIFPR))
         ELSE
           AREF = DIFPR * (1.0-REFDF) * REFSOI
         ENDIF

         EXPONENT = -KDIFBL*SQV*YLAISH/DIFPR
         IF (EXPONENT > EXPMIN) THEN
           AREFSH = DIFPR * (1.0-REFDF) * REFSOI *
     &     (1.0-EXP(-KDIFBL*SQV*YLAISH/DIFPR))
         ELSE
           AREFSH = DIFPR * (1.0-REFDF) * REFSOI
         ENDIF

         AREFSL = AREF - AREFSH
         ATOT = ADIR + ADIF + AREF
         REFTOT = REFDIR + REFDIF + REFSOI - AREF

C     Calculate cumulative light absorbed down the conopy by sunlit and shaded LAI

         RADSS = INCSOI * (1.0-ALBEDO)
         RDIFSL = (ADDFSL+ADIFSL+AREFSL)

         CUMASUN = RDIFSL + (1.-SCVR)*RADDIR*KDIRBL*YLAISL
         CUMASH = ADDFSH+ADIFSH+AREFSH

C     Determine per-leaf sunlit and shaded LAI and sunlit and shaded PAR

	   PLAISL(L) = YLAISL - UYLAISL
	   PLAISH(L) = YLAISH - UYLAISH

	   IF (PLAISL(L) .EQ. 0.0) THEN
	      RADSUN(L) = 0.0
	     ELSE
	      RADSUN(L) = (CUMASUN-UCUMASUN)/PLAISL(L)
	   ENDIF
	   IF (PLAISH(L) .EQ. 0.0) THEN
	      RADSH(L) = 0.0
	     ELSE
	      RADSH(L) = (CUMASH-UCUMASH)/PLAISH(L)
	   ENDIF

	   UYLAISH = YLAISH
	   UYLAISL = YLAISL
	   UCUMASUN = CUMASUN
	   UCUMASH = CUMASH
	  ENDIF
	END DO

C ** JIL End of per-leaf loop

C     Set radiation array and calculate ratios of components.

      IF (PARH .GT. 0.0) THEN
        PCTINT = 100.0 * INTCAN / PARH
        PCTABS = 100.0 * ATOT / PARH
        PCTREF = 100.0 * REFTOT / PARH
      ELSE
        PCTINT = 0.0
        PCTABS = 0.0
        PCTREF = 0.0
      ENDIF

C     Energy balance check (RADTOT=PARH).

      RADTOT = ATOT + REFTOT + RADSS

      END !SUBROUTINE MZ_IX_CANABS
