C======================================================================
C  SU_NFACTO, Subroutine
C
C  Determines N deficit in Sunflower
C----------------------------------------------------------------------
C  Revision history
C  xx/xx/19xx     Written
C  02/08/1993 PWW Header revision and minor changes              
C  03/29/2001 WDB Converted to modular form                      
C  12/01/2001 WDB Further modular conversion  
!  02/25/2005 CHP Check for NFAC < 0.                   
C----------------------------------------------------------------------
C
C  Called : SU_GROSUB
C
C  Calls  : None
C----------------------------------------------------------------------

      SUBROUTINE SU_NFACTO(DYNAMIC,               !Control
     %    TANC,TCNP,TMNC,xlanc,xlcnp,xlmnc,       !Inputs
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
      REAL        xlanc,xlcnp,xlmnc     
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
        IF (TCNP-TMNC .GT. 0.0) THEN
          NFAC   = 1.0 - (TCNP-TANC)/(TCNP-TMNC)  
        ELSE
          NFAC   = 0.0
        ENDIF
c       write(*,*)'nfacto 62',tcnp,tanc,tmnc,NFAC
          NFAC   = AMIN1 (NFAC,1.0)
          NFAC   = AMAX1 (NFAC,0.001)

          !Compute nitrogen stress factor for reducing leaf expansion

          AGEFAC = NFAC
          AGEFAC = AMIN1 (AGEFAC,1.0)

          !Make grain per plant calculation less sensitive to N stress
        NDEF3  = 1.0
        IF (NFAC .LT. 0.8) THEN
          NDEF3 = AMIN1 (0.2 + NFAC,1.0)
        ENDIF
          


          !Compute nitrogen stress factor for PHOTOSYNTHESIS
          
        NSTRES =  1.0 - (XLCNP-XLANC)/(XLCNP-XLMNC)
        IF (NSTRES .GT. 0.5)  THEN
          NSTRES = NSTRES * 0.4  + 0.6
        ELSE
          IF (NSTRES .GT. 0.0) THEN
            NSTRES = NSTRES * 1.2  + 0.2
          ELSE
            NSTRES = 0.2
          ENDIF
        ENDIF
        NSTRES = AMIN1 (NSTRES,1.0)
      ENDIF 
 
      RETURN

      END SUBROUTINE SU_NFACTO


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

