    subroutine SC_OPGROW_SAM_DETAILED (CONTROL, CaneCrop,YRPLT)

    !     Define DSSAT composite variables:
    USE ModuleDefs
    USE ModuleData

    !     SAMUCA's composite variables:
    USE SAM_ModuleDefs

    IMPLICIT NONE
    EXTERNAL GETLUN, rm_file, open_file, HEADER, YR_DOY
    SAVE

    !     Declare composite variables:
    TYPE (ControlType)  CONTROL     ! DSSAT's control
    Type (SwitchType)   ISWITCH     ! DSSAT's switches
    TYPE (CaneSamuca)   CaneCrop    ! Samuca's Composite variables

    !     DSSAT inputs:
    INTEGER         NOUTDG_det      ! The file unit number for the phytomers growth output file.
    INTEGER         NOUTDG_pho      ! The file unit number for the layered canopy output file.
    INTEGER         NOUTDG_str      ! The file unit number for the crop stresses output file.
    INTEGER         RUN             ! local run number
!   INTEGER         ERRNUM          ! Error status
    INTEGER         DYNAMIC         ! Dynamic
!   LOGICAL         FILE_EXISTS     ! Does the file exist, is this the first run?
    CHARACTER*1     IDETG           ! Print Output flag
    CHARACTER*20    FILEIO          ! local ?
    CHARACTER*21    OFILE_det       ! Filename detailed (phytomers)
    CHARACTER*21    OFILE_pho       ! Filename photosynthesis (layered canopy)
    CHARACTER*21    OFILE_str       ! Filename crop stresses
    CHARACTER*50    header_char     ! Filename crop stresses

!   real            SW(NL)                  ! Soil water content
!   real            watdmd                  ! water demand (TRWUP/Transp)
    integer         glai
    integer         ghour
    integer         phy

    !--- Time control
    INTEGER DAP, DAS, FROP, YRDOY, YRPLT, YEAR, DOY
!   INTEGER TIMDIF

    !--- Output setup
!   INTEGER I, J        ! Varible counters
    integer NUM_OVARS   ! Number of output variables
    integer VAR_WIDTH   ! Width of output columns

    !--- Number of variables and collumn widht
    parameter(NUM_OVARS = 45, VAR_WIDTH = 12)

!   CHARACTER*15 GROHEAD(4, NUM_OVARS)
!   CHARACTER*15 GRO_OUT(NUM_OVARS)

!   CHARACTER*10 SKIP_STR, VLEN_STR, WIDTH_STR  ! String equivalents of VLEN and SKIP
!   CHARACTER*1024 FMT_STR, T_FMT_STR           ! Runtime format statement:
    
    !--- Set files name:
    parameter(OFILE_det = 'PlantGr_Det.OUT')    ! Must be exactly the same as in OUTPUT.CDE
    parameter(OFILE_pho = 'Canopy_Phot.OUT')    ! Must be exactly the same as in OUTPUT.CDE
    parameter(OFILE_str = 'Crop_Stress.OUT')    ! Must be exactly the same as in OUTPUT.CDE

    !--- Get dynamic
    DYNAMIC = CONTROL%DYNAMIC

    !--- Get RUN
    RUN     = CONTROL%RUN

    !--- Get fileio
    FILEIO  = CONTROL%FILEIO

    !--- Head to dynamic
    IF (DYNAMIC.EQ.RUNINIT) THEN

        !---------------!
        !--- RUNINIT ---!
        !---------------!
        
        !--- Get file unit number:
        CALL GETLUN('SAMPHY', NOUTDG_det)
        CALL GETLUN('SAMCPH', NOUTDG_pho)
        CALL GETLUN('SAMSTR', NOUTDG_str)
        
        !--- Delete files if they exists        
        call rm_file(NOUTDG_det, OFILE_det)
        call rm_file(NOUTDG_pho, OFILE_pho)
        call rm_file(NOUTDG_str, OFILE_str)

    ELSEIF (DYNAMIC.EQ.SEASINIT) THEN

        !----------------!
        !--- SEASINIT ---!
        !----------------!

        CALL GET(ISWITCH)
        IDETG = ISWITCH % IDETG
        IF (IDETG .NE. 'Y') RETURN

        !----------------------!
        !--- Create Outputs ---!
        !----------------------!

        !--- Get file unit number:
        CALL GETLUN('SAMPHY', NOUTDG_det)
        CALL GETLUN('SAMCPH', NOUTDG_pho)
        CALL GETLUN('SAMSTR', NOUTDG_str)

        header_char = '("*DETAILED GROWTH ASPECTS OUTPUT FILE")'
        call open_file(NOUTDG_det, OFILE_det, header_char)
        
        header_char = '("*LAYERED CANOPY PHOTOSYNTHESIS")'
        call open_file(NOUTDG_pho, OFILE_pho, header_char)
        
        header_char = '("*ABIOTIC CROP STRESSES")'
        call open_file(NOUTDG_str, OFILE_str, header_char)

        !--- Output a header (treatment / run info, etc)
        CALL HEADER(SEASINIT, NOUTDG_det, RUN)
        CALL HEADER(SEASINIT, NOUTDG_pho, RUN)
        CALL HEADER(SEASINIT, NOUTDG_str, RUN)

        !--- write photosynthesis header
        write(NOUTDG_det, 10)
        write(NOUTDG_det, 11)

        !--- Detailed Photos Header
10      format('Variables Below:')
11      format('season, pltype, year, doy, das, dap, phy, above_ground_internode, above_ground_leaf, leaf_alive, phytomer_attribute_name, phytomer_attribute_units, phytomer_attribute_value')
        
        !--- write photosynthesis header
        write(NOUTDG_pho, 20)
        write(NOUTDG_pho, 21)
        write(NOUTDG_pho, 22)
        
        !--- Detailed Photos Header
20      format('Variables and Units Below:')
21      format('season, pltype, year,   doy,   das,   dap, gaus_hour, gaus_lai,   lai, frac_li,             amax, eff, gaus_hour_abs, gaus_lai_abs,  qleaf_layer,      a_layer, dir_par, dif_par, total_par')
22      format('   dml,    dml, yyyy, 0-366, 0-inf, 0-inf,       0-5,      0-5, m2/m2,     0-1, micromol m-2 s-1,   -,          hour,          lai, micromol m-2 s-1, micromol m-2 s-1,   W m-2,   W m-2,     W m-2')
        
        !--- write photosynthesis header
        write(NOUTDG_str, 30)
        write(NOUTDG_str, 31)
        write(NOUTDG_str, 32)
        
        !--- Detailed Photos Header
30      format('Variables and Units Below:')
31      format('season, pltype, year,   doy,   das,   dap, watdmd, swfacp, swface, tmn, tempfac_pho, tempfac_per, co2, pho_fac_co2, diacem, agefactor_amax, agefactor_per, sug_it_BG, amaxfbfac,      dtg,          per')
32      format('   dml,    dml, yyyy, 0-366, 0-inf, 0-inf,    0-1,    0-1,    0-1,  oC,         0-1,         0-1, ppm,         0-1, oCdays,            0-1,           0-1,    ton/ha,       0-1,    g m-2,     mm day-1')
        
        !--- Frenquency of outputs
        FROP   = CONTROL%FROP

    ELSEIF(DYNAMIC.EQ.RATE) THEN

        !------------!
        !--- RATE ---!
        !------------!

    ELSEIF(DYNAMIC.EQ.INTEGR) THEN

        !-------------------!
        !--- INTEGRATION ---!
        !-------------------!

    ELSEIF(DYNAMIC.EQ.OUTPUT) THEN

        !--------------------!
        !--- WRITE OUTPUT ---!
        !--------------------!

        !--- Return if no outputs should be printed
        IF (IDETG .NE. 'Y') RETURN

        !--- Print daily output:
        YRDOY  = CONTROL%YRDOY

        IF (YRDOY .GE. YRPLT) THEN

            !--- Get time controls
            DAP = CaneCrop % dap
            DAS = CONTROL % DAS
            CALL YR_DOY(YRDOY, YEAR, DOY)

            !--------------------------------------------------
            !  Write output based on user specified frequency
            !--------------------------------------------------
            IF ((MOD(DAS,FROP) .EQ. 0) .OR. (YRDOY .EQ. YRPLT)) THEN     ! Daily output every FROP days on planting date

                !------------------------!
                !--- Phytomer Profile ---!
                !------------------------!

                do phy = 1, CaneCrop % n_ph
                    write(NOUTDG_det,113) CaneCrop % seqnow, ',', CaneCrop % pltype, ',', year, ',', doy, ',', das, ',', dap, ',', phy, ',', CaneCrop % fl_it_AG(phy), ',', CaneCrop % fl_lf_AG(phy), ',', CaneCrop % fl_lf_alive(phy), ',', 'Leaf Age'                   , ',', 'Cdays'   , ',', CaneCrop % phprof(phy,1)
                    write(NOUTDG_det,113) CaneCrop % seqnow, ',', CaneCrop % pltype, ',', year, ',', doy, ',', das, ',', dap, ',', phy, ',', CaneCrop % fl_it_AG(phy), ',', CaneCrop % fl_lf_AG(phy), ',', CaneCrop % fl_lf_alive(phy), ',', 'Leaf Total DW'              , ',', 'g'       , ',', CaneCrop % phprof(phy,6)
                    write(NOUTDG_det,113) CaneCrop % seqnow, ',', CaneCrop % pltype, ',', year, ',', doy, ',', das, ',', dap, ',', phy, ',', CaneCrop % fl_it_AG(phy), ',', CaneCrop % fl_lf_AG(phy), ',', CaneCrop % fl_lf_alive(phy), ',', 'Leaf Area'                  , ',', 'cm2'     , ',', CaneCrop % phprof(phy,5)
                    write(NOUTDG_det,113) CaneCrop % seqnow, ',', CaneCrop % pltype, ',', year, ',', doy, ',', das, ',', dap, ',', phy, ',', CaneCrop % fl_it_AG(phy), ',', CaneCrop % fl_lf_AG(phy), ',', CaneCrop % fl_lf_alive(phy), ',', 'Internode Age'              , ',', 'Cdays'   , ',', CaneCrop % phprof(phy,58)
                    write(NOUTDG_det,113) CaneCrop % seqnow, ',', CaneCrop % pltype, ',', year, ',', doy, ',', das, ',', dap, ',', phy, ',', CaneCrop % fl_it_AG(phy), ',', CaneCrop % fl_lf_AG(phy), ',', CaneCrop % fl_lf_alive(phy), ',', 'Internode Total DW'         , ',', 'g'       , ',', CaneCrop % phprof(phy,50)
                    write(NOUTDG_det,113) CaneCrop % seqnow, ',', CaneCrop % pltype, ',', year, ',', doy, ',', das, ',', dap, ',', phy, ',', CaneCrop % fl_it_AG(phy), ',', CaneCrop % fl_lf_AG(phy), ',', CaneCrop % fl_lf_alive(phy), ',', 'Internode Structural DW'    , ',', 'g'       , ',', CaneCrop % phprof(phy,51)
                    write(NOUTDG_det,113) CaneCrop % seqnow, ',', CaneCrop % pltype, ',', year, ',', doy, ',', das, ',', dap, ',', phy, ',', CaneCrop % fl_it_AG(phy), ',', CaneCrop % fl_lf_AG(phy), ',', CaneCrop % fl_lf_alive(phy), ',', 'Internode Total Sugars DW'  , ',', 'g'       , ',', CaneCrop % phprof(phy,52)
                    write(NOUTDG_det,113) CaneCrop % seqnow, ',', CaneCrop % pltype, ',', year, ',', doy, ',', das, ',', dap, ',', phy, ',', CaneCrop % fl_it_AG(phy), ',', CaneCrop % fl_lf_AG(phy), ',', CaneCrop % fl_lf_alive(phy), ',', 'Internode Sucrose DW'       , ',', 'g'       , ',', CaneCrop % phprof(phy,53)
                    write(NOUTDG_det,113) CaneCrop % seqnow, ',', CaneCrop % pltype, ',', year, ',', doy, ',', das, ',', dap, ',', phy, ',', CaneCrop % fl_it_AG(phy), ',', CaneCrop % fl_lf_AG(phy), ',', CaneCrop % fl_lf_alive(phy), ',', 'Internode Hexose DW'        , ',', 'g'       , ',', CaneCrop % phprof(phy,54)
                    write(NOUTDG_det,113) CaneCrop % seqnow, ',', CaneCrop % pltype, ',', year, ',', doy, ',', das, ',', dap, ',', phy, ',', CaneCrop % fl_it_AG(phy), ',', CaneCrop % fl_lf_AG(phy), ',', CaneCrop % fl_lf_alive(phy), ',', 'Internode Length'           , ',', 'mm'      , ',', CaneCrop % phprof(phy,16)
                    write(NOUTDG_det,113) CaneCrop % seqnow, ',', CaneCrop % pltype, ',', year, ',', doy, ',', das, ',', dap, ',', phy, ',', CaneCrop % fl_it_AG(phy), ',', CaneCrop % fl_lf_AG(phy), ',', CaneCrop % fl_lf_alive(phy), ',', 'Phytomer Age'               , ',', 'Cdays'   , ',', CaneCrop % phprof(phy,12)
                    write(NOUTDG_det,113) CaneCrop % seqnow, ',', CaneCrop % pltype, ',', year, ',', doy, ',', das, ',', dap, ',', phy, ',', CaneCrop % fl_it_AG(phy), ',', CaneCrop % fl_lf_AG(phy), ',', CaneCrop % fl_lf_alive(phy), ',', 'Internode Fiber Fraction'   , ',', 'Cdays'   , ',', CaneCrop % phprof(phy,17)
                    write(NOUTDG_det,113) CaneCrop % seqnow, ',', CaneCrop % pltype, ',', year, ',', doy, ',', das, ',', dap, ',', phy, ',', CaneCrop % fl_it_AG(phy), ',', CaneCrop % fl_lf_AG(phy), ',', CaneCrop % fl_lf_alive(phy), ',', 'Internode Sugars Fraction'  , ',', 'Cdays'   , ',', CaneCrop % phprof(phy,18)
                    write(NOUTDG_det,113) CaneCrop % seqnow, ',', CaneCrop % pltype, ',', year, ',', doy, ',', das, ',', dap, ',', phy, ',', CaneCrop % fl_it_AG(phy), ',', CaneCrop % fl_lf_AG(phy), ',', CaneCrop % fl_lf_alive(phy), ',', 'Internode Sucrose Fraction' , ',', 'Cdays'   , ',', CaneCrop % phprof(phy,19)
                    write(NOUTDG_det,113) CaneCrop % seqnow, ',', CaneCrop % pltype, ',', year, ',', doy, ',', das, ',', dap, ',', phy, ',', CaneCrop % fl_it_AG(phy), ',', CaneCrop % fl_lf_AG(phy), ',', CaneCrop % fl_lf_alive(phy), ',', 'Internode Hexose Fraction'  , ',', 'Cdays'   , ',', CaneCrop % phprof(phy,20)
                    write(NOUTDG_det,113) CaneCrop % seqnow, ',', CaneCrop % pltype, ',', year, ',', doy, ',', das, ',', dap, ',', phy, ',', CaneCrop % fl_it_AG(phy), ',', CaneCrop % fl_lf_AG(phy), ',', CaneCrop % fl_lf_alive(phy), ',', 'Pfac Struc'                 , ',', '0-1'     , ',', CaneCrop % phprof(phy,15)
                enddo
113 format(i2,a1,a6,a1,i4,a1,i3,a1,i4,a1,i4,a1,i3,a1,l1,a1,l1,a1,l1,a1,a25,a1,a5,a1,f12.4)

                !-------------------------------!
                !--- Detailed Photosynthesis ---!
                !-------------------------------!
                if(CaneCrop % flemerged)then

                    do glai = 1 ,5
                        do ghour = 1, 3
                            write(NOUTDG_pho,111)                          &
                                CaneCrop % seqnow                   , ',', &
                                CaneCrop % pltype                   , ',', &
                                year                                , ',', &
                                doy                                 , ',', &
                                das                                 , ',', &
                                dap                                 , ',', &
                                ghour                               , ',', &
                                glai                                , ',', &
                                CaneCrop % lai_ass                  , ',', &
                                CaneCrop % frac_li_pho              , ',', &
                                CaneCrop % amax_out                 , ',', &
                                CaneCrop % eff_out                  , ',', &
                                CaneCrop % Acanopy(ghour+1,1)       , ',', &
                                CaneCrop % Acanopy(1,glai+1)        , ',', &
                                CaneCrop % Qleaf(ghour+1,glai+1)    , ',', &
                                CaneCrop % Acanopy(ghour+1,glai+1)  , ',', &
                                CaneCrop % incpar(ghour,2)          , ',', &
                                CaneCrop % incpar(ghour,3)          , ',', &
                                CaneCrop % incpar(ghour,4)
                        enddo
                    enddo
                endif

111             format(     i2,         a1,     &   ! seqnow
                a6,         a1,     &   ! pltype
                i4,         a1,     &   ! year
                i3,         a1,     &   ! doy
                i4,         a1,     &   ! das
                i4,         a1,     &   ! dap
                i2,         a1,     &   ! ghour                 [hour of day]
                i2,         a1,     &   ! glai                  [canopy layer]
                f20.5,      a1,     &   ! lai                   [m2/m2]
                f20.5,      a1,     &   ! frac light absorbed   [0-1]
                f20.5,      a1,     &   ! amax_out              [micromol m-2 s-1]
                f20.5,      a1,     &   ! eff_out               [micromol(CO2) micromol(photon)-1]
                f20.5,      a1,     &   ! Acanopy(ghour)        [hour]
                f20.5,      a1,     &   ! Acanopy(glai)         [m2/m2]
                f20.5,      a1,     &   ! Qleaf(ghour,glai)     [micromol/m2/s]
                f20.5,      a1,     &   ! Acanopy(ghour,glai)   [micromol/m2/s]
                f20.5,      a1,     &   ! incpar(ghour,2)       [direct PAR - W/m2]
                f20.5,      a1,     &   ! incpar(ghour,3)       [difuse PAR - W/m2]
                f20.5)                  ! incpar(ghour,4)       [total PAR - W/m2]
                
                !---------------------!
                !--- Crop Stresses ---!
                !---------------------!
                
                write(NOUTDG_str, 145)                 & 
                    CaneCrop % seqnow           , ',', &
                    CaneCrop % pltype           , ',', &
                    year                        , ',', &
                    doy                         , ',', &
                    das                         , ',', &
                    dap                         , ',', &
                    CaneCrop % watdmd           , ',', &
                    CaneCrop % swfacp           , ',', &
                    CaneCrop % swface           , ',', & 
                    CaneCrop % tmn              , ',', & 
                    CaneCrop % tempfac_pho      , ',', &
                    CaneCrop % tempfac_per      , ',', &
                    CaneCrop % co2              , ',', &
                    CaneCrop % pho_fac_co2      , ',', &
                    CaneCrop % diacem           , ',', &
                    CaneCrop % agefactor_amax   , ',', &
                    CaneCrop % agefactor_per    , ',', &
                    CaneCrop % sug_it_BG        , ',', &
                    CaneCrop % amaxfbfac        , ',', &
                    CaneCrop % dtg*(1.e6/1.e4)  , ',', & 
                    CaneCrop % per
                
145 format(i2,a1,a6,a1,i4,a1,i3,a1,i4,a1,i4,30(a1,f20.5))  

            ENDIF
        ENDIF

    ELSEIF(DYNAMIC.EQ.SEASEND) THEN

        !--------------------!
        !--- CLOSE OUTPUT ---!
        !--------------------!

        IF (IDETG .NE. 'Y') RETURN
        CLOSE(UNIT=NOUTDG_det)
        CLOSE(UNIT=NOUTDG_pho)
        CLOSE(UNIT=NOUTDG_str)

    ENDIF

    end subroutine SC_OPGROW_SAM_DETAILED
