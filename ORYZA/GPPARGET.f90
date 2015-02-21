! user subroutine !!!!!!!!

! xGAI     - Total leaf area index
! xGAID    - Leaf area index above point of calculation
! xAmaxIn  - Uncorrected amax
! xEffIn   - Uncorrected efficiency
! xAmaxOut - Corrected amax
! xEffOut  - Corrected efficiency

      subroutine GPParGet (xGAI,xGAID,xAmaxIn,xEffIn,xAmaxOut,xEffOut)
	   use gp
      implicit none

!     formal parameters
      real xGAI, xGAID, xAmaxIn, xEffIn, xAmaxOut, xEffOut

!     local variables
      real AmaxCO2,SLNI
      real Amax
      real tmpr1

!     include common block
!      real cCO2, cKNF, cNFLV, cREDFT
!      common /gp_common/ cCO2, cKNF, cNFLV, cREDFT

      SAVE         !TAOLI

!     avoid compiler warnings on unused variables
      tmpr1 = xAmaxin
      
      AmaxCO2 = 49.57/34.26*(1.-exp (-0.208*(cCO2-60.)/49.57))
      AmaxCO2 = max (0.,AmaxCO2)

      if (xGAI.GT.0.01.AND.cKNF.GT.0.) then
         SLNI = cNFLV*xGAI*cKNF*exp (-cKNF*xGAID)/(1.-exp (-cKNF*xGAI))
      else
         SLNI = cNFLV
      end if
 
!-----Calculate actual photosynthesis from SLN, CO2 and temperature
!     calculation of AMAX according to Van Keulen & Seligman (1987):
!     AMAX = 32.4 * (SLNI-0.2) * REDFT * CO2AMX

      if (SLNI.GE.0.5) then
         Amax = 9.5+(cAMaxSLN*SLNI)*cREDFT*AmaxCO2  !TaoLi, 25May 2011
      else
!         Amax = max (0.,68.33*(SLNI-0.2)*cREDFT*AmaxCO2)
         Amax = max (0.,68.33*(SLNI-cMinSLN)*cREDFT*AmaxCO2) !TaoLi, 25May 2011
      end if

      xAmaxOut = Amax
      xEffOut  = xEffIn

      return
      end
