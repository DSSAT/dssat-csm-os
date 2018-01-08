C======================================================================
C  BS_NFACTO, Subroutine
C
C  Determines N deficit in Sugarbeet
C----------------------------------------------------------------------
C  Revision history
C  xx/xx/19xx     Written
                  
C----------------------------------------------------------------------
C
C  Called : BS_GROSUB
C
C  Calls  : None
C----------------------------------------------------------------------

      SUBROUTINE BS_NFACTO(DYNAMIC,               !Control
     %    TANC,TCNP,TMNC,                         !Inputs
     %    AGEFAC,NDEF3,NFAC,NSTRES)               !Outputs
      
      USE ModuleDefs
      IMPLICIT NONE
      SAVE
C----------------------------------------------------------------------
C                     Variable Declaration
C----------------------------------------------------------------------
      REAL        AGEFAC      
      REAL        NDEF3       
      REAL        NFAC 
      REAL        NSTRES      
      REAL        TANC        
      REAL        TCNP        
      REAL        TMNC           
      INTEGER     DYNAMIC     
C     ----------------------------------------------------------------


      IF(DYNAMIC.EQ.RUNINIT) THEN
          NSTRES = 1.0    
          AGEFAC = 1.0    
          NDEF3 = 1.0     
          NFAC = 1.0      

      ELSEIF(DYNAMIC.EQ.SEASINIT) THEN
          NSTRES = 1.0
          AGEFAC = 1.0
          NDEF3 = 1.0     
          NFAC = 1.0  

      ELSEIF(DYNAMIC.EQ.INTEGR) THEN


! If the actual nitrogen content in the above ground biomass (TANC) 
! decreases below a critical level (TCNP), then compute N stress 
! based on proportion nitrogen below critical level (TCNP-TANC) and 
! total nitrogen between the critical level and minimum 
! level (TCNP-TMNC). 

          NFAC   = 1.0 - (TCNP-TANC)/(TCNP-TMNC)
!          NFAC   = AMIN1 (NFAC,1.0)

!         2/25/2005 chp   
!          NFAC   = AMAX1 (NFAC,0.001)

!Make nitrogen stress factor less sensitive during ISTAGE 3 and 4. 
!JIL: This is the same as moving TCNP down during XSTAGE 1.5~5.5 ??
!     which will move TCNP closer to the original Jones (1983) function
!Note however that when NFAC=1, this transformation makes it 0.94, 
!indicating that there will always be some N stress during this period.
! 10/02.2007 JIL Fixing this problem
!          IF (ISTAGE .EQ. 3 .OR. ISTAGE .EQ. 4) THEN          !
!           IF (NFAC .LE. 0.93) THEN                           !
!              NFAC = 1.0 - 1.80*EXP(-3.5*NFAC)                !
!           ENDIF                                              !
!!              NFAC = 1.082 - 1.82*EXP(-3.1*NFAC)             !
!          ENDIF                                               !

          NFAC   = AMIN1 (NFAC,1.0)
          NFAC   = AMAX1 (NFAC,0.001)

          !Compute nitrogen stress factor for reducing leaf expansion
          AGEFAC = 1.0
          AGEFAC = NFAC
          AGEFAC = AMIN1 (AGEFAC,1.0)

          !Make grain per plant calculation less sensitive to N stress
          NDEF3  = 1.0
          IF (NFAC .LT. 0.8) THEN
              NDEF3 = 0.2 + NFAC
          ENDIF
          NDEF3  = AMIN1 (NDEF3, 1.0)


          !Compute nitrogen stress factor for screen output only?
          !Note that NSTRES is not used to modify any processes
          NSTRES = 1.0
          NSTRES = NFAC*1.2 + 0.2
          IF (NFAC .GT. 0.5) THEN
              NSTRES = NFAC*0.4 + 0.6
          ENDIF
      ENDIF

 
      RETURN

      END SUBROUTINE BS_NFACTO


C----------------------------------------------------------------------
C                         DEFINITIONS
C----------------------------------------------------------------------
! AGEFAC      !Nitrogen stress factor affecting expansion (0-1)
! ISTAGE      !Plant growth staqe
! NDEF3       !Nitrogen stress factor affecting grains per plant (0-1)
! NFAC        !Nitrogen stress factor based on actual and critical 
!              nitrogen content in vegetative tisue
! NSTRES      !Nitrogen stress factor affecting growth (0-1)
! TANC        !Nitrogen content in above ground biomass, decimal
! TCNP        !Critical nitrogen concentration in tops, g N/g dry wt.
! TMNC        !Plant top minimum N concentration g N/g dry matter
! DYNAMIC     !Modular control variable
! RUNINIT     !DYNAMIC control variable
! SEASINIT    !DYNAMIC control variable
! RATE        !DYNAMIC control variable
! INTEGR      !DYNAMIC control variable
! OUTPUT      !DYNAMIC control variable
! SEASEND     !DYNAMIC control variable
C----------------------------------------------------------------------

