
!     SUBROUTINE PGS(SWFACP,STRESS_PHO,STRESS_K,STRESS_N,CHUPIBASE,PAR,LAI,PG,RESP,DIAC,TMN,W,CCEFF,CCMAX,k,PHTMAX,CCMP,PARMAX)
      SUBROUTINE PGS(SWFACP,STRESS_PHO,STRESS_K,STRESS_N,PAR,LAI,PG,RESP,DIAC,TMN,W,CCEFF,CCMAX,k,PHTMAX,CCMP,PARMAX)

!*****************************************************************************
!*     Subroutine PGS
!*     Calculates the canopy gross photosysntesis rate (PG)
!*     Input: SWFACP,PAR,LAI,DIAC,TMN,W,CCEFF,CCMAX,k,PHTMAX,PARMAX,PTSMAX)
!*     Output: PG,RESP
!*     FABIO MARIN
!******************************************************************************      

      IMPLICIT NONE 
      REAL PAR,LAI,DIac,RESP,LI,PG,E_FAC,PRATIO,STRESS_PHO,STRESS_K,STRESS_N
      REAL SWFACP,W,AGEFACTOR,PSTRES2  !SWFACE,ROWSPC,
      REAL CCK,CCEFF,CCMAX,k,PHTMAX,PARMAX,PTSMAX
      REAL A0,CCMP,CO2,CT,TMN  !,CHUPIBASE

!Original Equation - Some of the varaibles were taken out because the lack of relationship with
!Sugarcane or the lack of time to develop it in time for this version
      
      CO2 = 380.      
      
!-----------------------------------------------------------------------
!     Calculate maximum photosynthesis as function of PAR, g CH2O/m2
!-----------------------------------------------------------------------
      PAR = PAR * 3.969354729                                   !The multiplier for PAR is to convert 
      PTSMAX = PHTMAX * (1.0 - EXP(-(1.0 / PARMAX) * (PAR)))    !PAR (MJ/m2.d) to (moles/m2.d). 
                                                                !See Sheet c:\canemodel\Photosynthesis_Cropgro.xls
!-----------------------------------------------------------------------
!     Calculate reduction in photosynthesis due to incomplete canopy.
!-----------------------------------------------------------------------
      LI    = 1. - EXP(-k * LAI) ! Replacing LIalt in the PG's equation.
! FM - The code should include the effect of N stress. It should be included in second phase of the model.      
      
!-----------------------------------------------------------------------
!     Adjust canopy photosynthesis for CO2 concentration assuming a
!     reference value of CO2 of 330 ppmv. Thus, it computes daily gross canopy 
!     photosynthesis (g[CH2O]/m2-d). 
!-----------------------------------------------------------------------
      CCK = CCEFF / CCMAX !CCK is QE in Alagarswamy et al. - it means "quantum efficiency of the leaf"
      A0 = -CCMAX * (1. - EXP(-CCK * CCMP)) ! CCMAX is the AMax in the same paper. It means the Maximum A
      PRATIO = A0 + CCMAX * (1. - EXP(-CCK * CO2))
 
!Compute daily gross photosynthesis (g CH2O/m2 leaf/d)
      PSTRES2 = 1.0 ! it'll be kept equal 1 untill Phosphorus algorithm had been finished
!      AGEFACTOR = EXP(-0.000401*(DIAC-(CHUPIBASE+2000))) ! it'll be kept equal 1 untill N Stress algorithm had been finished
      AGEFACTOR = EXP(-0.000401*(DIAC-(2000))) ! Changed to reduce the photosynthesis rates after a while
      AGEFACTOR = MIN(1.,AGEFACTOR)

	!		AGEFACTOR = EXP(-0.000401*(DIAC-CHUPIBASE)) ! Factor is an age reduction factor for dPER, based on N. G. Inman-Bamber et al. 2008 Australian Journal of Agricultural Research, Fig.3
    !        AGEFACTOR = MIN(1.,AGEFACTOR)

!E_FAC - Effect of N and/or P stress on canopy photosynthesis (0-1). KEEP 1 UNTIL ADVANCES
      IF (SWFACP < .99) THEN
        E_FAC = AGEFACTOR * SWFACP *  STRESS_N * STRESS_K * STRESS_PHO !Changed in 11/5/2010 to reduce the photosynthesis rates after a while
      ELSE
        E_FAC = 1. !AGEFACTOR 
      ENDIF

!COMPUTING GROSS PHOTOSYNTHESIS without stress during the first days
    PG = PTSMAX * LI * E_FAC * PRATIO   !  (g m-2 leaf d-1)
    PG = PG / 1000 ! (converting to kg m-2 leaf d-1)

! COMPUTING RESPIRATION from original equation of McCree (1974).
! It was assumed the same coefficients from Grain Sorghum do compute daily Photosynthesis
	  CT = (0.044 + 0.0019 * TMN + 0.001 * (TMN**2))   *.0108 ! EQUATION 5 FROM MCCREE (1974). .0054 is the
      !Coefficient for Sorghum. They are giving an response similar to that measure by Bull et al, apud Singels et al. (2005)
	  
      !convert from ton ha-1 to kg m-2
      w = w / 10.
      
      RESP = 0.14 * PG + CT * (W)    !(kg m-2 leaf d-1)

      RETURN
    END SUBROUTINE PGS
    