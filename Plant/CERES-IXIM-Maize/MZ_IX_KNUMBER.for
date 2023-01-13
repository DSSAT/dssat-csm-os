C==========================================================================
C  KNUMBER Subroutine
C
C  Simulates kernel numbers and prolificacy/barrenness 
C--------------------------------------------------------------------------
C  Revision history
C
C  1. Written                                        JIL   Jul 2001
C  2. Calibrated for new PAR,k,Leaf area,ears/plant  JIL   Nov 2001
C  3. Modified to add synchrony                      JIL   Jul 2003
C  4. Added Subs for source/sink-limited KSet        JIL   Sep 2004
C  5. Simplified and adapted for CSM                 JIL   Jul 2005
C  6. Modified source-limited KSet for IXIM model    JIL   Sep 2007
C--------------------------------------------------------------------------
C  Called : GROWTH
C==========================================================================	
	SUBROUTINE MZ_IX_KNUMBER (DYNAMIC,                  !Control
     &      FILES,G2,ICSDUR,IPAR,ISTAGE,PATHSR,P3,        !Inputs
     &      PLTPOP,SHCARB,SUMDTT,                         !Inputs
     &      BSGDD,EARS,GPP,GPSM)                          !Outputs

      USE ModuleDefs

	IMPLICIT NONE
      EXTERNAL ERROR, FIND, GETLUN, IGNORE
      SAVE	

      REAL       ASGDD
C      REAL       ASYMP
      REAL       BSGDD    
      CHARACTER*80    C80        
C      REAL       CRV1
C      REAL       CRV2
      REAL       CUMCARB  
      REAL       CUMIPAR  
      REAL       CUMSTRESS
      INTEGER    DYNAMIC
      REAL       EAR1     
      REAL       EAR2     
      REAL       EARS     
      INTEGER   ERR       
      CHARACTER*6     ERRKEY                
!     CHARACTER*12    FILEC     
      CHARACTER*92    FILECC
      CHARACTER*12    FILES      
      INTEGER    FOUND              
      REAL       G2       
      REAL       GPP      
      REAL       GPP1     
      REAL       GPP2     
      REAL       GPSM     
      REAL       GRSTRESS 
      INTEGER    ICSDUR
C      REAL       INT2
      REAL       IPAR     
      INTEGER    ISECT            
      INTEGER    ISTAGE
      REAL       KNCARB   
      INTEGER    KNDUR
      REAL       KNIPAR   
      INTEGER    LNUM                      
      INTEGER    LUNCRP            
!     REAL       NSTRES   
      REAL       P3       
      CHARACTER*80    PATHSR            
      INTEGER    PLF
      REAL       PLTPOP   
      REAL       PP4
C      REAL       PRLF      
      CHARACTER*6     SECTION         
      REAL       SHCARB   
C      REAL       SLP2
      REAL       SUMDTT   
!     REAL       SWFAC    
C      REAL       THRS1
C      REAL       THRS2

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

      SECTION = '*KERNE'
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(F6.1,2X,F6.1)',IOSTAT=ERR) ASGDD,BSGDD
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

      ENDIF
      REWIND(LUNCRP)

      CLOSE (LUNCRP)

C** JIL Need to input prolificacy code, PLF: 0 (non-prolific), 1 (prolific)
        PLF       = 0
        CUMCARB   = 0.0
        CUMIPAR	  = 0.0
        CUMSTRESS = 0.0
        EAR1	  = 0.0
        EAR2	  = 0.0
        EARS      = 0.0
        GPP       = 0.0
        GPP1      = 0.0
        GPP2      = 0.0
        GPSM      = 0.0
        GRSTRESS  = 0.0
        KNCARB    = 0.0
        KNDUR     = 0	 
        KNIPAR	  = 0.0
        PP4       = 0.0

C** JIL Prolificacy code, PRLF: 1.0 (prolific); 0.0 (non-prolific)
C       Read from ECO file in MZ_IX_PHENOL 

C        THRS1 = 1.436 - 0.001 * G2
C        CRV1  = 0.413 + 2451.577 * EXP(-0.015*G2)
C        ASYMP = 1.8 * G2
C           SLP2 = 5.76 - 2.643*PRLF
C           INT2 = AMAX1(1.18-1.474*PRLF, 0.0)
C        THRS2 = INT2 + SLP2*THRS1
C        CRV2  = 0.9 - 0.5 * PRLF

!-----------------------------------------------------------------------
!                     DYNAMIC = INTEGR
!-----------------------------------------------------------------------

      ELSEIF(DYNAMIC.EQ.INTEGR) THEN   

C ** Estimating average supply of assimilates around flowering

        PP4 = P3 - BSGDD	
        IF (ISTAGE .EQ. 3 .AND. SUMDTT .GE. PP4) THEN
          CUMCARB = CUMCARB + SHCARB
          CUMIPAR = CUMIPAR + IPAR
          KNDUR = KNDUR +1
        ELSEIF (ISTAGE .EQ. 4 .AND. SUMDTT .LE. ASGDD) THEN	
          CUMCARB = CUMCARB + SHCARB
          CUMIPAR = CUMIPAR + IPAR
          KNDUR = KNDUR +1
        ELSEIF (ISTAGE .EQ. 4 .AND. SUMDTT .GT. ASGDD) THEN
          IF (KNIPAR .EQ. 0.0) THEN
	      KNCARB   = CUMCARB   / REAL(KNDUR)
            KNIPAR   = CUMIPAR   / REAL(KNDUR)
          ENDIF
        ELSEIF (ISTAGE .EQ. 5 .AND. ICSDUR .EQ. 1) THEN

C ** Estimating barrenness and prolificacy

C	    GPP1 = G2*(1.-EXP(-0.846*(KNCARB-1.19)))      ! Andrade et al., 1999
	    GPP1 = G2*(1.-EXP(-0.3473*(KNCARB-0.0)))
          GPP2 = 1.8*G2*(1.-EXP(-0.895*(KNCARB-3.0)))
          EAR1 = 1.0-EXP(-1.00*(KNCARB-0.0))
          EAR2 = 2.0*(1.0-EXP(-0.875*(KNCARB-2.8)))
	    IF (PLF .EQ. 0) THEN
	      GPP2 = 0.0
	      EAR2 = 0.0
	    ENDIF

          GPP = AMAX1(GPP1,GPP2,0.0)

	  ENDIF

C ** Calculating outputs

        EARS = PLTPOP * AMAX1(EAR1,EAR2,0.0)      ! Average ears/m2
        GPSM = PLTPOP * GPP                       ! Average KN/m2

      ENDIF       !Endif for DYNAMIC LOOP

	RETURN
	
	END
