subroutine GPParSet (xCO2, xKNF, xNFLV, xREDFT, xAmaxSLN, xMinSLN)
use gp
implicit none
real xCO2, xKNF, xNFLV, xREDFT, xAmaxSLN, xMinSLN

!real cCO2, cKNF, cNFLV, cREDFT
!common /gp_common/ cCO2, cKNF, cNFLV, cREDFT

! store variables in common block
cCO2   = xCO2
cKNF   = xKNF
cNFLV  = xNFLV
cREDFT = xREDFT
cAMaxSLN = xAmaxSLN
cMinSLN = xMinSLN
return
end
