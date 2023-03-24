C=======================================================================
C  RESPIR, Subroutine, K.J.Boote, J.W.Jones, G. Hoogenboom
C  Calculates maintainence respiration and net photosythate
C  available.  PGAVL is the net photosynthesis after maintenance
C  respiration is subtracted from gross photosynthesis.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/01/89 KJB Written.
C  10/15/92 NBP Made into subroutine.
C  09/15/98 CHP Modified for modular format
C  05/11/99 GH  Incorporated in CROPGRO
C-----------------------------------------------------------------------
C  Called from:   CROPGRO
C  Calls:         None
C=======================================================================
      SUBROUTINE FOR_RESPIR(
     &    PG, R30C2, RES30C, TGRO, WTMAIN,                !Input
     &    RO, RP,                                         !Input/Output
     &    MAINR,                                          !Output
     &    LFMRC, mft, MRSWITCH, RTMRC, SDMRC, SHELMRC,    !Input
     &    STMMRC, STRMRC, TRSWITCH, TRST, TRSTYP, WTNLF,  !Input
     &      WTNRT, WTNSD, WTNSH, WTNSR, WTNST)            !Input
     
!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
!     TS defined in ModuleDefs.for

      IMPLICIT NONE
      EXTERNAL CURV
      SAVE

      INTEGER H
      REAL MAINR, PG, R30C2, RES30C, RO, RP, TRSFAC, WTMAIN
      REAL TGRO(TS)
      CHARACTER*1 MRSWITCH, TRSWITCH
      CHARACTER*3 TRSTYP
      REAL LFMRC, mft, RTMRC, SDMRC, SHELMRC, STMMRC, STRMRC, 
     &       TRST(4), WTNLF, WTNRT, WTNSD, WTNSH, WTNST, WTNSR
      REAL CURV

C-----------------------------------------------------------------------
C     Temperature effect on maintenance respiration 
C      When TRSWITCH="M", use McCree, 1974 quadratic equation
C      When TRSWITCH="U" use CURV Function to define temperature response
C      of maintenance respiration
C      TRSTYP="VOP" uses variable order polynomial equation from 
C      Thornley, 1998 where 
C      TRST(1) = Base temperature, response=0.0
C      TRST(4) = Maximum Temperature, response=0.0
C      TRST(2) = Reference temperature, response=1.0
C      TRST(3) = qft, variable controlling order of the polynomial
C      TRST(3)=1 for quadratic, 2 for cubic, 3 for quartic, etc.
C      TRST(3) does not have to be an integer, is continuously variable
C      mft is a multiplier
C-----------------------------------------------------------------------
      TRSFAC = 0.0
      IF (TRSWITCH .EQ. "M" .OR. TRSWITCH .EQ. "m")THEN
            DO H = 1,24
                TRSFAC = TRSFAC + 0.044+0.0019*TGRO(H)+0.001*TGRO(H)**2
            ENDDO
      ELSE
                  DO H = 1,24
                TRSFAC=TRSFAC+mft*CURV(TRSTYP,TRST(1),TRST(2),TRST(3),
     &                        TRST(4),TGRO(H))
            ENDDO
      ENDIF
C-----------------------------------------------------------------------
C     Convert maintainence respiration to actual temperature. RES30C is
C     the g CH2O/g DW/hr used in maintenance respiration at 30 C.
C-----------------------------------------------------------------------
C      01/17/05 SJR Convert from single maintenance respiration cost for all organs
C                  to cost per unit crude protein for each organ
C                  Estimate cost from Penning de Vries cost of 0.24g glucose/g protein 
C                  then multiply by turnover rate/hour.  Use much lower turnover rate 
C                  for storage organ than leaf.
C      Used MRSWITCH to allow user to revert to original MR code.  
C      IF MRSWITCH=0 then use original code, else use scheme correlating 
C      maintenance respiration with organ protein content and turnover.  
C      NOTE: still includes PG rate effect.
C-----------------------------------------------------------------------
      IF (MRSWITCH .EQ. "P") THEN
       RO = WTNLF*6.25*LFMRC+WTNST*6.25*STMMRC+WTNRT*6.25*RTMRC+
     &       WTNSR*6.25*STRMRC+
     &       MIN((WTNSH*6.25*SHELMRC),(WTNSD*6.25*SDMRC))
      RO = RO*TRSFAC
      ELSE
      RO = RES30C * TRSFAC
      ENDIF
      RP = R30C2 * TRSFAC
C      RP = R30C2 * 24

      IF (MRSWITCH .EQ. "P") THEN
      MAINR = RO + RP*PG
      ELSE
      MAINR = RO*WTMAIN + RP*PG
      ENDIF
!-----------------------------------------------------------------------
      RETURN
      END ! SUBROUTINE RESPIR
!-----------------------------------------------------------------------
!     RESPIR variables:
!-----------------------------------------------------------------------
! LFMRC   Maintenance respiration cost for leaves (g[CH2O] / g leaf protein / hr)
! MAINR   Maintenance respiration (g[CH2O] / m2 / d)
! mft            Multiplier for maintenance respiration temperature factor
! MRSWITCH Parameter to select the method of calculating portion of 
!            Maintenance Respiration (MR) associated with plant mass.
!            If MRSWITCH="P" then calculate MR based on organ protein content 
!            If MRSWITCH="M"or other, calculate the mass associated portion  
!            of MR based on plant mass (original CROPGRO approach).
! PG      Daily gross photosynthesis (g[CH2O] / m2 / d)
! R30C2   Respiration coefficient that depends on total plant mass, value 
!           at 30C (g[CH2O] used / g[CH2O] fixed / hr)
! RES30C  Respiration coefficient that depends on gross photosynthesis, 
!           value at 30C (g CH2O/g DW/hr)
! RO      Respiration coefficient that depends on total plant mass
!           (g[CH2O] / g[tissue])
! RP      proportion of the day's photosynthesis which is respired in the 
!           maintenance process 
! RTMRC   Maintenance respiration cost for roots (g[CH2O] / g root protein / hr)
! SDMRC   Maintenance respiration cost for the portion of seed subject to 
!            maintenance respiration (g[CH2O] / g seed protein / hr)
! SHELMRC Maintenance respiration cost for shell (g[CH2O] / g shell protein / hr)
! STMMRC  Maintenance respiration cost for stem (g[CH2O] / g stem protein / hr)
! STRMRC  Maintenance respiration cost for storage 
!            (g[CH2O] / g storage protein / hr)
! TGRO(I) Hourly air temperature (°C)
! TRSFAC  Temperature effect on maintenance respiration 
! TRST(4)       Maintenance respiration temperature response parameters define
!             the shape of maintenance respiration temperature response
! TRSTYP       Type of CURV function to describe maintenance respiration 
!             temperature response
! TRSWITCH Type of maintenance respiration temperature response to use
!             "M" uses McCree 1974 equation from CROPGRO, any other value
!             invokes the CURV function for a custom response
! TS      Number of intermediate time steps (=24) 
! WTMAIN  Mass of tissue assumed to require maintenance (g[tissue] / m2)
! WTNLF    Mass of N in leaves (g[leaf N] / m2[ground])
! WTNRT    Mass of N in roots (g[root N] / m2[ground])
! WTNSD    Mass of N in seeds (g[N] / m2[ground])
! WTNSH    Mass of N in shells (g[N] / m2[ground])
! WTNSR    Mass of N in storage organ (g[storage N] / m2[ground])
! WTNST    Mass of N in stems (g[stem N] / m2[ground])
!-----------------------------------------------------------------------
!     END SUBROUTINE RESPIR
!-----------------------------------------------------------------------
