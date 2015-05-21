Module CS_Cultivar_Coeffs_m
    
    REAL    :: LNUM_slope            ! Slope of the leaf number relationship 
    REAL    :: Tb_cul_leaf_size      ! Base temperature for leaf size increase 
    DATA LNUM_slope /0.8/            ! LPM 14MAY15 just for testing proposals is here
    DATA Tb_cul_leaf_size /12.0/     ! LPM 28feb15 it would be a cultivar coefficient (for testing)
    REAL    :: nod_cul               ! Node weight of the stem for BR = 0 (first stem of the shoot before branching) at harvest (it should be a fix amount of thermal time) 
    DATA nod_cul /12.0/              ! LPM 11APR15 it would be a cultivar coefficient (for testing)
    REAL    :: nod_length            ! LPM 20MAY2015 mean internode length (cm) for BR =0 and lignified 
    DATA nod_length /2.0/            ! LPM 20MAY2015 it would be a cultivar coefficient (for testing) 
End Module CS_Cultivar_Coeffs_m