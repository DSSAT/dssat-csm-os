C=======================================================================
C  PLANTG, Subroutine, J.I. Lizaso
C  Calculates instantaneous plant photosynthesis (µmol CO2/m2 s) of 
C  shaded and sunlit leaf area on a per-leaf basis and integrates for
C  the whole canopy
C  Adapted from CANOPG by K.J. Boote, J.W. Jones, G. Hoogenboom
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  02/07/2003 JIL Written
C  09/12/2007 JIL Adapted for IXIM model
C-----------------------------------------------------------------------
C  Called from: PHOTSYNT
C  Calls:       PSPARAM,PSLEAF
C=======================================================================
      SUBROUTINE MZ_IX_PLANTG(DYNAMIC,                          !Control
     &      ASMAX, GDDAE, GLA, PLAISL, PLAISH, LAP, LFL, LFN,   !Input
     &      LIGHT, PARSH, PARSUN, TAIRH, YX,                    !Input
     &      PGHR)                                               !Output

      USE ModuleDefs

      IMPLICIT  NONE
      EXTERNAL MZ_IX_PSPARAM, MZ_IX_PSLEAF
      SAVE

      REAL     ASMAX
      REAL     ASSAT(50)  
      REAL     CVXTY(50)  
      INTEGER  DYNAMIC
      REAL     GDDAE      
      REAL     GLA(50)    
      INTEGER  I
      REAL     INTSLP(50) 
      REAL     LAP(50)    
      REAL     LFL(50)    
      INTEGER  LFN
      LOGICAL  LIGHT
      REAL     PARSH(50) 
      REAL     PARSUN(50)
      REAL     PGHR       
      REAL     PGSH(50)  
      REAL     PGSL(50)  
      REAL     PGSUM      
      REAL     PGSUN      
      REAL     PLAISH(50)
      REAL     PLAISL(50)
      REAL     TAIRH     
      REAL     YX(50)     

!----------------------------------------------------------------------
!                     DYNAMIC = RUNINIT
!----------------------------------------------------------------------

      IF(DYNAMIC.EQ.RUNINIT.OR.DYNAMIC.EQ.SEASINIT) THEN

C     Initialize.
        I          = 1
        PGHR       = 0.0
        PGSUM      = 0.0
        PGSUN      = 0.0

	  DO I=1,50
          PGSH(I)    = 0.0
          PGSL(I)    = 0.0
        ENDDO

        CALL MZ_IX_PSPARAM(DYNAMIC,                             !Control
     &    ASMAX, GDDAE, GLA, LAP, LFL, LFN, LIGHT, TAIRH, YX,   !Input
     &    ASSAT, CVXTY, INTSLP)                                 !Output 

!-----------------------------------------------------------------------
!                     DYNAMIC = INTEGR
!-----------------------------------------------------------------------
      ELSEIF(DYNAMIC.EQ.INTEGR) THEN   

C
C ** JIL Calculate light response curve parameters per leaf 
C        as a function of leaf age and hourly air temperature


        CALL MZ_IX_PSPARAM(DYNAMIC,                             !Control
     &    ASMAX, GDDAE, GLA, LAP, LFL, LFN, LIGHT, TAIRH, YX,   !Input
     &    ASSAT, CVXTY, INTSLP)                                 !Output 

C     Calculate per-leaf assimilation 

	  PGHR = 0.0
	  DO I=1,LFN
	    IF(GLA(I) .GT. 0.05*YX(I)) THEN
            PGSUM = 0.0

C     Compute photosynthesis for sunlit leaves

	      CALL MZ_IX_PSLEAF(
     &        PARSUN(I), ASSAT(I), INTSLP(I), CVXTY(I),          !Input
     &        PGSUN)                                             !Output
            PGSL(I) = PGSUN

C     Compute photosynthesis for shaded leaves

            CALL MZ_IX_PSLEAF(
     &        PARSH(I), ASSAT(I), INTSLP(I), CVXTY(I),           !Input
     &        PGSH(I))                                           !Output

C     Compute instantaneous canopy gross photosynthesis (µmol CO2/m2 s).

            PGHR = PGHR + PGSL(I)*PLAISL(I) + PGSH(I)*PLAISH(I)
	    ENDIF
	  END DO

      ENDIF       !Endif for DYNAMIC LOOP

	RETURN

      END !SUBROUTINE MZ_IX_PLANTG

C=======================================================================
C  PSPARAM, Subroutine, J.I. Lizaso
C  Calculates parameters of light response curve per leaf as affected by
C  leaf age and hourly air temperature
C  Adapted from PGLFEQ by K.J. Boote, N.B. Pickering
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  02/07/2003 JIL Written
C  09/12/2007 JIL Adapted for IXIM model
C-----------------------------------------------------------------------
C  Called from: PLANTG
C  Calls:       
C=======================================================================
      SUBROUTINE MZ_IX_PSPARAM(DYNAMIC,                       !Control
     &  ASMAX, GDDAE, GLA, LAP, LFL, LFN, LIGHT, TAIRH, YX,   !Input
     &  ASSAT, CVXTY, INTSLP)                                 !Output 

      USE ModuleDefs

      IMPLICIT NONE
      SAVE      
      
      REAL     ASF
      REAL     AK        
      REAL     APK(50)   
      REAL     ASAT(50)  
      REAL     ASMAX     
      REAL     ASSAT(50) 
      REAL     AXX       
      REAL     AXZ       
      REAL     AYZ       
      REAL     CK        
      REAL     CVTY(50)  
      REAL     CVXTY(50) 
      REAL     CX        
      REAL     CXF       
      REAL     CXZ       
      INTEGER  DYNAMIC
      REAL     EXPONENT
      REAL     GDDAE     
      REAL     GLA(50)   
      INTEGER  I
      REAL     INSL(50)  
      REAL     INTSLP(50)
      REAL     ISF       
      REAL     LAP(50)   
      REAL     LFL(50)   
      INTEGER  LFN
      LOGICAL  LIGHT
      REAL     TAIRH    
      REAL     XX        
      REAL     YX(50)        

!----------------------------------------------------------------------
!                     DYNAMIC = RUNINIT
!----------------------------------------------------------------------

      IF(DYNAMIC.EQ.RUNINIT.OR.DYNAMIC.EQ.SEASINIT) THEN

C     Initialize
        ASF       = 0.0
        AK        = 0.0
        AXX       = 0.0
        AXZ       = 0.0
        AYZ       = 0.0
        CK        = 0.0
        CX        = 0.0
        CXF       = 0.0
        CXZ       = 0.0
        I         = 1
        ISF       = 0.0
        XX        = 0.0

	  DO I=1,LFN
	    APK(I)    = 0.0
	    ASAT(I)   = 0.0
          ASSAT(I)  = 0.0
          CVTY(I)   = 0.0
          CVXTY(I)  = 0.0
          INSL(I)   = 0.0
          INTSLP(I) = 0.0
        ENDDO

C ** JIL Parameters are calculated once a day and updated hourly
C        with changing hour temperature  

!-----------------------------------------------------------------------
!                     DYNAMIC = INTEGR
!-----------------------------------------------------------------------
      ELSEIF(DYNAMIC.EQ.INTEGR) THEN   

C ** JIL Calculating light response curve parameters for each leaf

	  IF (LIGHT) THEN
	  LIGHT = .FALSE.
	  DO I=1, LFN
	    ASAT(I) = 0.0
	    CVTY(I) = 0.0
	    INSL(I) = 0.0
	    IF(GLA(I) .GT. 0.0) THEN
        IF(YX(I) .GT. 0.0) THEN
	        XX = LAP(I)/YX(I)
        ELSE
          XX = 0.0
        ENDIF
	      IF(XX .LT. 0.99) THEN    ! Expanding leaf
	        AYZ = 0.66
	        AXX = 0.34
	        AK = 10.0
	        AXZ = 0.5
	        CX = 0.95
	        CK = 3.55
	        CXZ = 0.186
	        INSL(I) = 0.06
	      ELSE                     ! Leaf maturing after complete expansion
	        IF(APK(I) .LT. 1.0) THEN
		        APK(I) = GDDAE
	        ENDIF
!            CHP 11/27/2007 CHECK FOR LFL(I) = 0.
              IF (LFL(I) > 1.E-10) THEN
	          XX = (GDDAE-APK(I))/LFL(I)
              ELSE
	          XX = 1.0
              ENDIF

	        AYZ = 0.18
	        AXX = 0.85
	        AK = -7.0
	        AXZ = 0.47
	        CX = 0.95
	        CK = -16.7
	        CXZ = 0.88
              EXPONENT = 8.*(XX-0.75)
              IF (EXPONENT < -40.) THEN
                INSL(I) = 0.06
              ELSEIF (EXPONENT > 40.) THEN
                INSL(I) = 0.04
              ELSE
	          INSL(I) = 0.04+(0.02/(1.+EXP(EXPONENT)))
              ENDIF
            ENDIF

!	      ASAT(I) = ASMAX*(AYZ+(AXX/(1.0+EXP(-AK*(XX-AXZ)))))
            EXPONENT = -AK*(XX-AXZ)
            IF (EXPONENT < -40.) THEN
              ASAT(I) = ASMAX * (AYZ + AXX)
            ELSEIF (EXPONENT > 40.) THEN
              ASAT(I) = ASMAX * AYZ
            ELSE
	        ASAT(I) = ASMAX*(AYZ+(AXX/(1.0+EXP(EXPONENT))))
            ENDIF

!	      CVTY(I) = CX/(1.0+EXP(-CK*(XX-CXZ)))
            EXPONENT = -CK*(XX-CXZ)
            IF (EXPONENT < -40.) THEN
              CVTY(I) = CX
            ELSEIF (EXPONENT > 40.) THEN
              CVTY(I) = 0.0
            ELSE
	        CVTY(I) = CX/(1.0+EXP(EXPONENT))
            ENDIF

		    IF(APK(I) .GT. 1.0 .AND. XX .LT. 0.25) THEN
	        CX = 0.9
	        CK = 0.2
	        CVTY(I)=CX+(CK*XX)
	      ENDIF
	    ENDIF
	  END DO

	  ENDIF

C ** JIL Effect of T on light curve parameters 
C        Curve parameters are defined at 30 C and scaled using hourly air temp

	  ASF = 0.0886-0.00867*TAIRH+0.00284*TAIRH**2.0
     &       -0.0000507*TAIRH**3.0
	  ISF = 0.6783+0.0291*TAIRH-0.000756*TAIRH**2.0
     &       +0.00000513*TAIRH**3.0
	  CXF = 1.0108-0.0005*TAIRH-0.00001*TAIRH*2.0
     &       +0.0000005*TAIRH**3.0

	  DO I=1, LFN
	    ASSAT(I) = ASAT(I)*ASF*1.0
	    INTSLP(I) = INSL(I)*ISF*1.0	
	    CVXTY(I) = CVTY(I)*CXF*1.0
	  END DO

      ENDIF       !Endif for DYNAMIC LOOP

	RETURN

	END	     

C=======================================================================
C  PSLEAF, Subroutine, J.I. Lizaso
C  Calculates gross photosynthesis (µmol CO2/m2 s) per unit leaf area as
C  a function of instantaneous PAR (µmol/m2 s)
C  Adapted from PGLEAF by K.J.Boote, J.W.Jones, G.Hoogenboom
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  02/07/03 JIL Written
C  09/12/2007 JIL Adapted for IXIM model
C-----------------------------------------------------------------------
C  Called from: PLANTG
C  Calls:       
C=======================================================================
      SUBROUTINE MZ_IX_PSLEAF(
     &  PARLF, AST, ISP, CVT,                             !Input
     &  PGLF)                                             !Output

      IMPLICIT NONE
      SAVE      
      
      REAL     A
      REAL     B
      REAL     C
      REAL     AST
      REAL     CVT
      REAL     ISP
      REAL     PARLF 
      REAL     PGLF

C     Initialize

      A     = 0.0
      B     = 0.0
      C     = 0.0
      PGLF  = 0.0

C ** JIL Using a non-rectangular hyperbolae (Thornley and Johnson, 1990)
C ** JIL PGLF is average leaf gross assimilation (umol/m2 s)

	A = CVT
	B = (ISP*PARLF) + AST
	C = ISP * PARLF * AST

 !JIL 09/17/09
      IF (AST .GT. 0.0 .AND. A .GT. 0.0) THEN
         PGLF = (B - SQRT(B**2.-4.*A*C)) / (2.*A)
       ELSE
         PGLF = 0.0
      ENDIF

	RETURN

	END
