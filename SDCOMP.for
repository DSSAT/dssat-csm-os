C=======================================================================
C  SDCOMP, Subroutine, Ernie Piper, K.J. Boote, G. Hoogenboom
C-----------------------------------------------------------------------
C  Calculates seed composition for oil and protein
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  09/01/1994 EP  Written.
C  08/22/1995 GH  Included in CROPGRO.
C  09/15/1998 CHP Modified for modular format
C  05/10/1999 GH  Incorporated in CROPGRO
C-----------------------------------------------------------------------
C  Called by:  DEMAND
C  Calls:      None
C=======================================================================

      SUBROUTINE SDCOMP(
     &  CARMIN, LIPOPT, LIPTB, PLIGSD, PMINSD, POASD,     !Input
     &  RCH2O, RLIG, RLIP, RMIN, RNO3C, ROA, SDLIP,       !Input
     &  SDPRO, SLOSUM, TAVG,                              !Input
     &  AGRSD1, AGRSD2, FNINSD, POTCAR, POTLIP)           !Output
!-----------------------------------------------------------------------
      IMPLICIT NONE
      SAVE
!-----------------------------------------------------------------------

      REAL LIPTEM, LIPOPT, LIPTB, GENSUM
      REAL POTPRO, CARMIN, SLOSUM, POTLIP, POTCAR
      REAL TAVG, SUMTEM, PSUMSD, TOTAL
      REAL AGRSD1, AGRSD2
      REAL PMINSD, POASD, PLIGSD, SDLIP, SDPRO, FNINSD
      REAL RNO3C, RLIG, RMIN, RCH2O, RLIP, ROA

!***********************************************************************
!***********************************************************************
C     The quadratic plateau for predicting percentage lipids
C-----------------------------------------------------------------------
      IF (TAVG .GE. LIPOPT) THEN
         LIPTEM= 1.0
      ELSEIF ((TAVG .LT. LIPOPT) .AND. (TAVG .GT. LIPTB)) THEN
         LIPTEM = 1.0 - ((LIPOPT - TAVG) / (LIPOPT - LIPTB))**2
      ELSE
         LIPTEM= 0.0
      ENDIF
      POTLIP = SDLIP * LIPTEM

C-----------------------------------------------------------------------
C     Determination of protein percentage
C-----------------------------------------------------------------------
      GENSUM = (SDPRO*100.0) + 
     &  (SDLIP*100.0) * (1.0 - (((LIPOPT - 25.) / (LIPOPT - LIPTB))**2))
      SUMTEM = 1.0 + SLOSUM * (TAVG - 25.0)
      PSUMSD = GENSUM * SUMTEM / 100.0
      POTPRO = PSUMSD - POTLIP

C-----------------------------------------------------------------------
C     Determination of carbohydrate percentage
C-----------------------------------------------------------------------
      POTCAR = 1.0 - POTLIP - POTPRO
      IF (POTCAR .LT. CARMIN) THEN
        POTCAR =  CARMIN
      ENDIF

      TOTAL  = POTLIP + POTPRO + POTCAR
!      IF (TOTAL .NE. 1.0) THEN
      IF (ABS(TOTAL) - 1.0 .GT. 0.0005) THEN
         POTPRO = POTPRO / TOTAL
         POTLIP = POTLIP / TOTAL
         POTCAR = POTCAR / TOTAL
!        Note:  POTCAR will fall below CARMIN again, if adusted.
!        Should only POTPRO and POTLIP be adusted here? -chp
!        Check logic - GH
!        Check PODDETACH - GH
         TOTAL  = POTLIP + POTPRO + POTCAR
      ENDIF

      POTCAR = POTCAR - PMINSD - POASD - PLIGSD
      AGRSD1 = PMINSD*RMIN + PLIGSD*RLIG + POASD*ROA +
     &    POTLIP*RLIP + POTCAR*RCH2O
      AGRSD2 = AGRSD1 + RNO3C*POTPRO
      FNINSD = POTPRO / 6.25

!***********************************************************************
      RETURN
!-----------------------------------------------------------------------
      END !SUBROUTINE SDCOMP
!-----------------------------------------------------------------------
! AGRSD1 CH2O requirement for seed growth, excluding cost for protein 
!          content (g[CH2O] / g[seed])
! AGRSD2 CH2O requirement for seed growth, including cost for protein 
!          content (g[CH2O] / g[seed])
! CARMIN Minimum carbohydrate fraction 
! FNINSD Maximum fraction of N for growing seed tissue based on temperature
!          (g[N] / g[seed])
! GENSUM Protein plus lipid composition (%)
! LIPOPT Temperature above which lipid composition is at a maximum (°C)
! LIPTB  Temperature below which lipid composition is zero (°C)
! LIPTEM Factor to reduce lipid composition based on temperature (0-1). 
!          Normalized quadratic plateau function. 
! PLIGSD Proportion of seed tissue that is lignin (fraction)
! PMINSD Proportion of seed tissue that is mineral (fraction)
! POASD  Proportion of seed tissue that is organic acid (fraction)
! POTCAR Potential carbohydrate composition of seed based on temperature
!          (fraction)
! POTLIP Potential lipid composition of seed based on temperature
!          (fraction)
! POTPRO Potential protein composition of seed based on temperature
!          (fraction)
! PSUMSD Potential protein plus lipid composition (fraction)
! RCH2O  Respiration required for synthesizing CH2O structure
!          (g[CH2O] / g[tissue])
! RLIG   Respiration required for synthesizing lignin structure
!          (g[CH2O] / g[lignin])
! RLIP   Respiration required for synthesizing lipid structure
!          (g[CH2O] / g[lipid])
! RMIN   Respiration required for synthesizing mineral structure
!          (g[CH2O] / g[mineral])
! RNO3C  Respiration required for reducing NO3 to protein
!          (g[CH2O] / g[protein])
! ROA    Respiration required for synthesizing organic acids
!          (g[CH2O] / g[product])
! SDLIP  Maximum lipid composition in seed (fraction)
! SDPRO  Seed protein fraction at 25oC (g[protein] / g[seed])
! SLOSUM Slope of temperature vs. SUMTEM line (1/oC)
! SUMTEM Factor which affects protein composition based on average 
!          temperature. 
! TAVG   Average daily temperature (°C)
! TOTAL  Check for total composition equal to one. 
!-----------------------------------------------------------------------
!     END SUBROUTINE SDCOMP
!-----------------------------------------------------------------------

