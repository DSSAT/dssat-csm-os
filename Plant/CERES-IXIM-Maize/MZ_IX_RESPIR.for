C=======================================================================
C  RESPIR, Subroutine, K.J.Boote, J.W.Jones, G. Hoogenboom
C  Calculates daily maintainence and growth respiration.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/01/1989 KJB Written.
C  10/15/1992 NBP Made into subroutine.
C  09/15/1998 CHP Modified for modular format
C  05/11/1999 GH  Incorporated in CROPGRO
C  03/20/2003 JIL Modified and added growth respiration
C  09/12/2007 JIL Adapted for IXIM model
C-----------------------------------------------------------------------
C  Called from:   PHOTSYNT
C  Calls:         
C=======================================================================
      SUBROUTINE MZ_IX_RESPIR(DYNAMIC,                          !Control
     &  PG, R30C2, RES30C, TAIRHR, WTMAIN,                        !Input
     &  PCARLF,PCARST,PCARRT,PCAREA,PCARSD,PPROLF,PPROST,PPRORT,  !Input
     &  PPROEA,PPROSD,PLIPLF,PLIPRT,PLIPST,PLIPEA,PLIPSD,PLIGLF,  !Input
     &  PLIGST,PLIGRT,PLIGEA,PLIGSD,POALF,POAST,POART,POAEA,POASD,!Input
     &  PMINLF,PMINST,PMINRT,PMINEA,PMINSD,                       !Input
     &  GRLF,GRRT,GRSTM,GREAR,GRGRN,                              !Input
     &  MAINR, CVF)                                              !Output

!-----------------------------------------------------------------------
      USE ModuleDefs

      IMPLICIT NONE
      SAVE      

      REAL        CVF       
      INTEGER     DYNAMIC
      REAL        GREAR     
      REAL        GRGRN     
      REAL        GRLF      
      REAL        GRRT      
      REAL        GRSTM     
      INTEGER     H
      REAL        MAINR     
      REAL        PCAREA    
      REAL        PCARLF    
      REAL        PCARRT    
      REAL        PCARSD    
      REAL        PCARST    
      REAL        PG        
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
      REAL        RCEA      
      REAL        RCGR      
      REAL        RCLF      
      REAL        RCRT      
      REAL        RCST      
      REAL        RES30C    
      REAL        RO        
      REAL        RP
      REAL        SCLTS 
C                     SCLTS added on 4 July 2017 by Bruce Kimball
      REAL        TAIRHR(24)
      REAL        TRSFAC    
      REAL        WTMAIN    

!----------------------------------------------------------------------
!                     DYNAMIC = RUNINIT
!----------------------------------------------------------------------

      IF(DYNAMIC.EQ.RUNINIT.OR.DYNAMIC.EQ.SEASINIT) THEN
        CVF     = 0.0  
        H       = 1
        MAINR   = 0.0 
        RCEA    = 0.0  
        RCGR    = 0.0  
        RCLF    = 0.0  
        RCRT    = 0.0  
        RCST    = 0.0  
        RO      = 0.0  
        RP      = 0.0  
        TRSFAC  = 0.0  

!-----------------------------------------------------------------------
!                     DYNAMIC = INTEGR
!-----------------------------------------------------------------------
      ELSEIF(DYNAMIC.EQ.INTEGR) THEN   

C-----------------------------------------------------------------------
C     Temperature effect on maintenance respiration (McCree, 1974)
C-----------------------------------------------------------------------
        TRSFAC = 0.0
        SCLTS = 24./TS
        DO H = 1,TS
C         TRSFAC = TRSFAC + 0.044+0.0019*TAIRHR(H)+0.001*TAIRHR(H)**2
      TRSFAC = TRSFAC +(0.044+0.0019*TAIRHR(H)+0.001*TAIRHR(H)**2)*SCLTS
C     scaling to number of time steps 24/TS added by Bruce Kimball on 4 July 2017
        ENDDO
	  TRSFAC = TRSFAC*1.0
C-----------------------------------------------------------------------
C     Convert maintainence respiration to actual temperature. RES30C is
C     the g CH2O/g DW hr used in maintenance respiration at 30 C.
C     R30C2 is the g of CH2O/g CH2O hr at 30 C. MAINR is g CH2O/m2 d 
C-----------------------------------------------------------------------
        RO = RES30C * TRSFAC
        RP = R30C2 * TRSFAC
        MAINR = RO*WTMAIN + RP*PG
C-----------------------------------------------------------------------
C     Calculate growth respiration according to Penning de Vries and van
C     Laar (1982) and Boote et al (1998)
C-----------------------------------------------------------------------

C     Calculate growth respiratory cost per organ (g CH2O / g DM tissue)
C     according to Table 11, p 61 (Penning de Vries et al, 1989)

        RCLF = PLIPLF*3.189 + PLIGLF*2.231 + POALF*0.954 + PMINLF*0.12
     &         + PCARLF*1.275 + PPROLF*1.887
	  RCST = PLIPST*3.189 + PLIGST*2.231 + POAST*0.954 + PMINST*0.12
     &         + PCARST*1.275 + PPROST*1.887
	  RCRT = PLIPRT*3.189 + PLIGRT*2.231 + POART*0.954 + PMINRT*0.12
     &         + PCARRT*1.275 + PPRORT*1.887
	  RCEA = PLIPEA*3.189 + PLIGEA*2.231 + POAEA*0.954 + PMINEA*0.12
     &         + PCAREA*1.275 + PPROEA*1.887
        RCGR = PLIPSD*3.189 + PLIGSD*2.231 + POASD*0.954 + PMINSD*0.12
     &         + PCARSD*1.275 + PPROSD*1.887

C     Calculate weighted conversion factor for the whole plant
C     CVF (g CH2O / g DM) 

	  IF(GRLF+GRSTM+GRRT+GREAR+GRGRN .EQ. 0.0) THEN
	     CVF = 0.0
	  ELSE
	     CVF = (GRLF*RCLF + GRSTM*RCST + GRRT*RCRT + GREAR*RCEA
     &          + GRGRN*RCGR) / (GRLF+GRSTM+GRRT+GREAR+GRGRN)
	  ENDIF
      ENDIF       !Endif for DYNAMIC LOOP
	  
!-----------------------------------------------------------------------
      RETURN
      END ! SUBROUTINE RESPIR
!-----------------------------------------------------------------------
!     RESPIR variables:
!-----------------------------------------------------------------------
! MAINR   Maintenance respiration (g[CH2O] / m2 / d)
! PG      Daily gross photosynthesis (g[CH2O] / m2 / d)
! R30C2   Respiration coefficient that depends on total plant mass, value 
!           at 30C (g[CH2O] used / g[CH2O] fixed / hr)
! RES30C  Respiration coefficient that depends on gross photosynthesis, 
!           value at 30C (g CH2O/g DW/hr)
! RO      Respiration coefficient that depends on total plant mass
!           (g[CH2O] / g[tissue])
! RP      proportion of the day's photosynthesis which is respired in the 
!           maintenance process 
! TGRO(I) Hourly air temperature (°C)
! TRSFAC  Temperature effect on maintenance respiration 
! TS      Number of intermediate time steps (=24) 
! WTMAIN  Mass of tissue assumed to require maintenance (g[tissue] / m2)
!-----------------------------------------------------------------------
!     END SUBROUTINE RESPIR
!-----------------------------------------------------------------------
