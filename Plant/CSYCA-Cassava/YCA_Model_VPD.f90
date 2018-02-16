! @author mylesjfisher
    Module YCA_Model_VPD
    
    USE ModuleDefs
    !USE CS_First_Trans_m
    
    SAVE
    
    REAL    :: VPDFPHR(TS)                ! Hourly VPD factor, 0-1
    REAL    :: MNVPDFPHR                  ! Daily mean VPD factor, 0-1MNVPDFPHR
    REAL    :: VPDStartHr    , VPDMaxHr   ! VPD start hour, VPD max hour
    REAL    :: CARBOTMPRNoVPD             ! CH2O  with standare RUE and RUE with hourly VPD
    REAL    :: ET0(TS)                    ! Hourly reference transpiration
    REAL    :: ET0DAY                     ! Daily integral of reference transpiration

End Module YCA_Model_VPD