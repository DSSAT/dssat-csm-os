C=======================================================================
C  RESPIR, Subroutine, K.J.Boote, J.W.Jones, G. Hoogenboom
C  Calculates maintainence respiration and net photosythate
C  available.  PGAVL is the net photosynthesis after maintenance
C  respiration is subtracted from gross photosynthesis.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/01/1989 KJB Written.
C  10/15/1992 NBP Made into subroutine.
C  09/15/1998 CHP Modified for modular format
C  05/11/1999 GH  Incorporated in CROPGRO
C-----------------------------------------------------------------------
C  Called from:   PLANT
C  Calls:         None
C=======================================================================
      SUBROUTINE RESPIR(
     &    PG, R30C2, RES30C, TGRO, WTMAIN,                !Input
     &    RO, RP,                                         !Input/Output
     &    MAINR)                                          !Output

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
!     TS defined in ModuleDefs.for

      IMPLICIT NONE

      INTEGER H
      REAL MAINR, PG, R30C2, RES30C, RO, RP, TRSFAC, WTMAIN, SCLTS
C     SCLTS added on 4 July 2017 by Bruce Kimball
      REAL TGRO(TS)

C-----------------------------------------------------------------------
C     Temperature effect on maintenance respiration (McCree, 1974)
C-----------------------------------------------------------------------
      TRSFAC = 0.0
      SCLTS = 24./TS
      DO H = 1,TS
C        TRSFAC = TRSFAC + 0.044+0.0019*TGRO(H)+0.001*TGRO(H)**2
         TRSFAC = TRSFAC + (0.044+0.0019*TGRO(H)+0.001*TGRO(H)**2)*SCLTS
C         scaling factor of 24/TS added on 4July2017 by Bruce Kimball
      ENDDO
C 24 changed to TS on 3 July 2017 by Bruce Kimball
C This equation look really suspicious because TRSFAC very 
C dependent on number of times through the loop!
      
C-----------------------------------------------------------------------
C     Convert maintainence respiration to actual temperature. RES30C is
C     the g CH2O/g DW/hr used in maintenance respiration at 30 C.
C-----------------------------------------------------------------------
      RO = RES30C * TRSFAC
      RP = R30C2 * TRSFAC
      MAINR = RO*WTMAIN + RP*PG

!-----------------------------------------------------------------------
      RETURN
      END ! SUBROUTINE RESPIR
!-----------------------------------------------------------------------
!     RESPIR variables:
!-----------------------------------------------------------------------
! MAINR   Maintenance respiration (g[CH2O] / m2 / d)
! PG      Daily gross photosynthesis (g[CH2O] / m2 / d)
! R30C2   Respiration coefficient that depends on gross photosynthesis, 
!           value at 30C (g[CH2O] used / g[CH2O] fixed / hr)
! RES30C  Respiration coefficient that depends on total plant mass,
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
