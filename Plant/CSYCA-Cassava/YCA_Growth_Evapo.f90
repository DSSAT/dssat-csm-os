!***************************************************************************************************************************
! This is the code from the section (DYNAMIC == RATE) lines 4075 - 4215 of the original CSCAS code. The names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement and are declared here. The variables 
! that are not arguments are declared in module YCA_First_Trans_m. Unless identified as by MF, all comments are those of 
! the original CSCAS.FOR code.
!
! Subroutine YCA_Growth_Evapo calculates the water and thermal conditions and water factor for initiation of growth of the 
! planting material (called germination).
!***************************************************************************************************************************

    SUBROUTINE YCA_Growth_Evapo ( & 
        BRSTAGE     , CO2         , DLAYR       , DUL         , EO          ,     &
        ISWWAT      , LL          , NLAYR       , SW          , YEAR        , ST)

! 2023-01-25 chp removed unused variables
!       ALBEDOS     , CLOUDS      , EOP         , ES          , KEP         , RLV         , 
!       SAT         , SRAD        , TAIRHR      , TDEW        , TMAX        , TMIN        , 
!       TRWUP       , UH2O        , WINDSP      , LAI         
!       WEATHER     , SOILPROP    , CONTROL

        USE ModuleDefs
        USE YCA_First_Trans_m
        USE YCA_Control_Environment
   
        IMPLICIT NONE
        EXTERNAL CSIDLAYR, TFAC4
        
!        TYPE (ControlType), intent (in) :: CONTROL    ! Defined in ModuleDefs
!        TYPE (WeatherType), intent (in) :: WEATHER    ! Defined in ModuleDefs
!        TYPE (SoilType), intent (in) ::   SOILPROP   ! Defined in ModuleDefs                                                            ! MF Defined in ModuleDefs

        INTEGER NLAYR       , YEAR         
        INTEGER CSIDLAYR                                                                      ! Integer function call.
        
!       REAL    ALBEDOS     , BRSTAGE     , CLOUDS      , CO2         , DLAYR(NL)   , DUL(NL)     , EO          , EOP         
        REAL    BRSTAGE     , CO2         , DLAYR(NL)   , DUL(NL)     , EO          
!       REAL    ES          , KEP         , LL(NL)      , RLV(NL)     , SAT(NL)     , SRAD        
        REAL    LL(NL)      
!       REAL    SW(NL)      , TAIRHR(24)  , TDEW        , TMAX        , TMIN        , TRWUP       , UH2O(NL)    , WINDSP      
        REAL    SW(NL)      
        REAL    ST(NL)                                  !LPM20MAR2016 To consider ST for germination
        REAL    TFAC4    !CSVPSAT     ,                  ! Real function call.  !LPM 19SEP2017 Added tfac5
!       REAL    LAI
        
        CHARACTER(LEN=1) ISWWAT      !IDETG       , 
        
          IF (PLYEAR <= 0) PLYEAR = YEAR
!-----------------------------------------------------------------------
!         Calculate potential plant evaporation,water uptake if neeeded
!-----------------------------------------------------------------------

          ! EO is brought into the module. The following calculations
          ! (apart from the root water uptake module) are for 
          ! comparative purposes only. The root water uptake module 
          ! is not necessary when running in CSM, but is necessary for
          ! CROPSIM.

          ! Co2 effect on stomatal resistances. General for C3 crops 
          RLF  = 9.72 + 0.0757 * 330.0 + 10.0
          RLFC = 9.72 + 0.0757 *  CO2  + 10.0
          YEARDOY = YEARDOY                                                 ! MF For WORK.OUT
                                                         
          
          ! Cumulative potential ET as used
          IF (EO > 0.0) EOC = EOC + EO
          
          ! Cumulative canopy-air temperature difference
          TDIFSUM = TDIFSUM+(TCAN-TMEAN)
          TDIFNUM = TDIFNUM + 1
          TDIFAV = TDIFSUM/TDIFNUM

!!-----------------------------------------------------------------------
!!         Calculate thermal time                       !LPM 21MAR2016 Moved after estimation of water factors to consider ST(LSEED)
!!-----------------------------------------------------------------------
!
!          Tfd = TFAC4(trdv1,tmean,TT)
!          IF (brstage+1.0 < 10.0) &
!           Tfdnext = TFAC4(trdv2,tmean,TTNEXT)
!          IF (trgem(3) > 0.0) THEN
!            Tfgem = TFAC4(trgem,ST(LSEED),TTGEM)
!            !Tfgem = TFAC4(trgem,tmean,TTGEM)  !LPM 20MAR2016 To modify 
!          ELSE
!            Ttgem = tt
!          ENDIF    
!          !IF (Cfllflife == 'D') THEN 
!          !  ! Leaf life read-in as days (eg.7 phyllochrons->7 days)
!          !  Ttlflife = Phints   
!          !ELSE  
!          !  !Tflflife = TFAC4(trdv1,tmean,TTlflife) 
!            Tflflife = TFAC4(trdv3,tmean,TTlflife)                         ! LPM 18MAR15 modified trdv1 to trdv3 to consider the cardinal temperatures for leaf development
!            Tflfsize = TFAC4(trdv4,tmean,TTlfsize)                         ! LPM 18MAR15 modified trdv1 to trdv4 to consider different optimum temperature for leaf size
!          !ENDIF  
!  
!-----------------------------------------------------------------------
!         Calculate soil water 'status' (Used as a sw 'potential')
!-----------------------------------------------------------------------

          DO L = 1,NLAYR
            SWP(L) = &
             AMIN1(1.0,AMAX1(.0,((SW(L)-LL(L))/(DUL(L)-LL(L)))))
          ENDDO
          
!-----------------------------------------------------------------------
!         Calculate water factor for germination
!-----------------------------------------------------------------------

          IF (LSEED < 0) LSEED = CSIDLAYR (NLAYR, DLAYR, SDEPTH)
          WFGE = 1.0
          IF (ISWWAT /= 'N') THEN
              !IF (GESTAGE < 1.0) THEN  !LPM 21MAR2015 to estimate WFGE for germination and emergence
              IF (EMRGFR < 1.0) THEN
              !IF (LSEED < 0) LSEED = CSIDLAYR (NLAYR, DLAYR, SDEPTH) !LPM 21MAR2015 To estimate LSEED when ISWWAT == N (linking ST and germination)
                  !IF (LSEED > 1) THEN
                  !  SWPSD = SWP(LSEED)
                  !ELSE
                  ! SWP(0) = AMIN1(1.,AMAX1(.0,(SWP(1)-0.5*(SWP(2)-SWP(1)))))
                  ! SWPSD = SWP(0) + (SDEPTH/DLAYR(1))*(SWP(2)-SWP(0))
                  !ENDIF
                  IF (WFGEM > 0.0) &
                   WFGE = AMAX1(0.0,AMIN1(1.0,(SWP(LSEED)/WFGEM)))
            ENDIF
          ENDIF
          IF (ISWWATCROP == 'N') WFGE = 1.0

!-----------------------------------------------------------------------
!         Calculate thermal time
!-----------------------------------------------------------------------

          Tfd = TFAC4(trdv1,tmean,TT)
          IF (brstage+1.0 < PSX) & !LPM 23JUL19 set as limit PSX instead of 10
           Tfdnext = TFAC4(trdv2,tmean,TTNEXT)
          IF (trgem(3) > 0.0) THEN
            Tfgem = TFAC4(trgem,ST(LSEED),TTGEM)
            !Tfgem = TFAC4(trgem,tmean,TTGEM)  !LPM 20MAR2016 To modify the TT for germination using ST 
          ELSE
            Ttgem = tt
          ENDIF    
          !IF (Cfllflife == 'D') THEN 
          !  ! Leaf life read-in as days (eg.7 phyllochrons->7 days)
          !  Ttlflife = Phints   
          !ELSE  
          !  !Tflflife = TFAC4(trdv1,tmean,TTlflife) 
          !LPM 09OCT2019 Remove TFLfgrowth because it is the same than TFG  
          !Tflfgrowth = calculateTemperatureFactor(trdv3,tmean,TTlfgrowth)                         ! LPM 18MAR15 modified trdv1 to trdv3 to consider the cardinal temperatures for leaf development
            Tflfsize = TFAC4(trdv4,tmean,TTlfsize)                         ! LPM 18MAR15 modified trdv1 to trdv4 to consider different optimum temperature for leaf size
            Tflflife = calculateTemperatureFactor(trlfl,tmean,TTlflife)              !LPM 14SEP2017 Added new cardinal temperatures for leaf life and other for leaf growth (trdv3 or trlfg)
            Tfdmc = calculateTemperatureFactor(trdmc,tmean,TTdmc)
            !ENDIF  
          
        
            
          
      END SUBROUTINE YCA_Growth_Evapo
