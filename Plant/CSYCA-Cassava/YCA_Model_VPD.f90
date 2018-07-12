! @author mylesjfisher
    Module YCA_Model_VPD
    
    USE ModuleDefs
    
    SAVE
    
    REAL    :: VPDFPHR(TS)                ! Hourly VPD factor, 0-1
    REAL    :: MNVPDFPHR                  ! Daily mean VPD factor, 0-1MNVPDFPHR
    REAL    :: VPDStartHr    , VPDMaxHr   ! VPD start hour, VPD max hour
    REAL    :: ET0(TS)                    ! Hourly reference transpiration
    REAL    :: ET0DAY                     ! Daily integral of reference transpiration

End Module YCA_Model_VPD