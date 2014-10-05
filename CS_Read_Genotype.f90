!**********************************************************************************************************************
! This is the code from the section (DYNAMIC.EQ.RUNINIT) ! Initialization, lines 2653 - 3099 of the original CSCAS code.
! The actual and dummy arguments are only for those variables that are dummy arguments for CSCAS. The type of all
! other variables are declared in the MODULE Module_CSCAS_Vars. The type of only the dummy arguments are declared here.
! The variables and their units are defined in CSCAS.
!
! SUBROUTINE CS_Read_Genotype reads the data from the genotype (species, ecotype and cultivar) files.
!
!**********************************************************************************************************************

    SUBROUTINE CS_Read_Genotype( &
        CN          , KCAN        , ON          , RN          , RNMODE      , RWUMX       , RWUPM       , SN          , &
        TN           &
        )

        !USE CRSIMDEF                                                                MF 15SE14 Declared in ModuleDefs
        USE Module_CSCAS_Vars_List
        
        IMPLICIT NONE
        
        CHARACTER(LEN=1) RNMODE 
        INTEGER CN          , ON          , RN          , SN          , TN          
        REAL    KCAN        , RWUMX       , RWUPM       

        !-----------------------------------------------------------------------
        !       Read genotype (cultivar,ecotype,species) 
        !-----------------------------------------------------------------------
        
        ! Cultivar coefficients initialized
        pdl = 0.0
        
        ! Ecotype coefficients re-set
        rspco = -99
        canhts = -99
        dayls = -99
        srnpcs = -99
        srprs = -99
        srprs = -99
        lseni = -99
        dusri = -99
        parue = -99
        paru2 = -99
        la1s = -99
        laxs = -99
        laws = -99
        rdgs = -99
        nfgu = -99
        nfgl = -99
        no3cf = -99
        h2ocf = -99
        rtno3 = -99
        rtnh4 = -99

        ! Species coefficients re-set
        pd = -99
        dusri = -99 
        llifa = -99 
        srfr = -99 
        rdgs = -99 
        laxs = -99 
        rlwr = -99 
        nfgl = -99 
        nfgu = -99 
        nfpu = -99 
        nfpl = -99 
        kcan = -99 
        lawcf = -99 
        lawmnfr = -99
        lawtr = -99
        lawts = -99
        lawmnfr = -99
        dfpe = -99 
        ppexp = -99 
        nh4mn = -99
        no3mn = -99
        rsfrs = -99

        IF (FILEIOT(1:2).EQ.'DS') THEN
            IF (RNMODE.NE.'T') CALL FVCHECK(CUDIRFLE,GENFLCHK)
            
            CALL XREADC (FILEIO,TN,RN,SN,ON,CN,'ECO#',econo)
            CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PPS1',dayls(1))
            CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PPS2',dayls(2))
            CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PPS3',dayls(3))
            CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PPS4',dayls(4))
            CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PPS5',dayls(5))
            CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PPS6',dayls(6))
            CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PPS7',dayls(7))
            CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PPS8',dayls(8))
            CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PPS9',dayls(9))
            CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PPEXP',ppexp) ! Trial
            CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PPFPE',dfpe)
            CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PHINT',phints)
            CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P1',pd(1))
            CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P2',pd(2))
            CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P3',pd(3))
            CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P4',pd(4))
            CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P5',pd(5))
            CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P6',pd(6))
            CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P7',pd(7))
            CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P8',pd(8))
            CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'P9',pd(9))
            CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'B01ND',pdl(1))
            CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'B12ND',pdl(2))
            CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'B23ND',pdl(3))
            CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'B34ND',pdl(4))
            CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'B45ND',pdl(5))
            CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'B56ND',pdl(6))
            CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'B67ND',pdl(7))
            CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'B78ND',pdl(8))
            CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'B89ND',pdl(9))
            CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'LLIFA',llifa)
            CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'STFR',swfrs)
            CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'SR#WT',srnow)
            CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'SRFR',srfr)
            CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'LAXS',laxs)
            CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'LAXND',laxno)
            CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'LAXN2',laxn2)
            CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'LAFS',lafs)
            CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'LAFND',lafnd)
            CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'SLAS',laws)
            CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'NFPU',nfpu)
            CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'NFPL',nfpl)
            CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'NFGU',nfgu)
            CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'NFGL',nfgl)
            CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'RDGS',rdgs)
            CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'RLWR',rlwr)
            CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PARUE',parue)
            CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'PARU2',paru2)
            CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'LA1S',la1s)
            ! New (Nov 2011) N uptake variables
            CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'NUPNF',no3cf)
            CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'NUPWF',h2ocf)
            CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'RTNUP',rtno3)
            CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'RTNH4',rtnh4)
            CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'NO3MN',no3mn)
            CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'NH4MN',nh4mn)
            CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'HMPC',hmpc)
            CALL XREADR (FILEIO,TN,RN,SN,ON,CN,'LPEFR',lpefr)
        ELSE
            IF (RNMODE.NE.'T') CALL FVCHECK(CUDIRFLE,GENFLCHK)
            CALL CUREADC (CUDIRFLE,VARNO,'ECO#',econo)
            CALL CUREADR (CUDIRFLE,VARNO,'PPS1',dayls(1))
            CALL CUREADR (CUDIRFLE,VARNO,'PPS2',dayls(2))
            CALL CUREADR (CUDIRFLE,VARNO,'PPS3',dayls(3))
            CALL CUREADR (CUDIRFLE,VARNO,'PPS4',dayls(4))
            CALL CUREADR (CUDIRFLE,VARNO,'PPS5',dayls(5))
            CALL CUREADR (CUDIRFLE,VARNO,'PPS6',dayls(6))
            CALL CUREADR (CUDIRFLE,VARNO,'PPS7',dayls(7))
            CALL CUREADR (CUDIRFLE,VARNO,'PPS8',dayls(8))
            CALL CUREADR (CUDIRFLE,VARNO,'PPS9',dayls(9))
            CALL CUREADR (CUDIRFLE,VARNO,'PPEXP',ppexp)! Trial
            CALL CUREADR (CUDIRFLE,VARNO,'PPFPE',dfpe)
            CALL CUREADR (CUDIRFLE,VARNO,'P1',pd(1))
            CALL CUREADR (CUDIRFLE,VARNO,'P2',pd(2))
            CALL CUREADR (CUDIRFLE,VARNO,'P3',pd(3))
            CALL CUREADR (CUDIRFLE,VARNO,'P4',pd(4))
            CALL CUREADR (CUDIRFLE,VARNO,'P5',pd(5))
            CALL CUREADR (CUDIRFLE,VARNO,'P6',pd(6))
            CALL CUREADR (CUDIRFLE,VARNO,'P7',pd(7))
            CALL CUREADR (CUDIRFLE,VARNO,'P8',pd(8))
            CALL CUREADR (CUDIRFLE,VARNO,'P9',pd(9))
            CALL CUREADR (CUDIRFLE,VARNO,'B01ND',pdl(1))
            CALL CUREADR (CUDIRFLE,VARNO,'B12ND',pdl(2))
            CALL CUREADR (CUDIRFLE,VARNO,'B23ND',pdl(3))
            CALL CUREADR (CUDIRFLE,VARNO,'B34ND',pdl(4))
            CALL CUREADR (CUDIRFLE,VARNO,'B45ND',pdl(5))
            CALL CUREADR (CUDIRFLE,VARNO,'B56ND',pdl(6))
            CALL CUREADR (CUDIRFLE,VARNO,'B67ND',pdl(7))
            CALL CUREADR (CUDIRFLE,VARNO,'B78ND',pdl(8))
            CALL CUREADR (CUDIRFLE,VARNO,'B89ND',pdl(9))
            CALL CUREADR (CUDIRFLE,VARNO,'PHINT',phints)
            CALL CUREADR (CUDIRFLE,VARNO,'LLIFA',llifa)
            CALL CUREADR (CUDIRFLE,VARNO,'STFR',swfrs)
            CALL CUREADR (CUDIRFLE,VARNO,'SR#WT',srnow)
            CALL CUREADR (CUDIRFLE,VARNO,'SRFR',srfr)
            CALL CUREADR (CUDIRFLE,VARNO,'LAXS',laxs)
            CALL CUREADR (CUDIRFLE,VARNO,'LAXND',laxno)
            CALL CUREADR (CUDIRFLE,VARNO,'LAXN2',laxn2)
            CALL CUREADR (CUDIRFLE,VARNO,'LAFS',lafs)
            CALL CUREADR (CUDIRFLE,VARNO,'LAFND',lafnd)
            CALL CUREADR (CUDIRFLE,VARNO,'SLAS',laws)
            CALL CUREADR (CUDIRFLE,VARNO,'NFPU',nfpu)
            CALL CUREADR (CUDIRFLE,VARNO,'NFPL',nfpl)
            CALL CUREADR (CUDIRFLE,VARNO,'NFGU',nfgu)
            CALL CUREADR (CUDIRFLE,VARNO,'NFGL',nfgl)
            CALL CUREADR (CUDIRFLE,VARNO,'RDGS',rdgs)
            CALL CUREADR (CUDIRFLE,VARNO,'RLWR',rlwr)
            CALL CUREADR (CUDIRFLE,VARNO,'PARUE',parue)
            CALL CUREADR (CUDIRFLE,VARNO,'PARU2',paru2)
            CALL CUREADR (CUDIRFLE,VARNO,'LA1S',la1s)
            CALL CUREADR (CUDIRFLE,VARNO,'LPEFR',lpefr)
            ! New (Nov 2011) N uptake variables
            CALL CUREADR (CUDIRFLE,VARNO,'NUPNF',no3cf)
            CALL CUREADR (CUDIRFLE,VARNO,'NUPWF',h2ocf)
            CALL CUREADR (CUDIRFLE,VARNO,'RTNUP',rtno3)
            CALL CUREADR (CUDIRFLE,VARNO,'RTNH4',rtnh4)
            CALL CUREADR (CUDIRFLE,VARNO,'NO3MN',no3mn)
            CALL CUREADR (CUDIRFLE,VARNO,'NH4MN',nh4mn)
            CALL CUREADR (CUDIRFLE,VARNO,'HMPC',hmpc)
        ENDIF     ! End Cultivar reads
        
        !-----------------------------------------------------------------------
        !       Read ecotype information
        !-----------------------------------------------------------------------
        
        CALL FVCHECK(ECDIRFLE,GENFLCHK)
        CALL ECREADR (ECDIRFLE,ECONO,'HTSTD',canhts)
        CALL ECREADR (ECDIRFLE,ECONO,'SRN%S',srnpcs)
        IF (SRNPCS.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'SRP%S',srprs)
        IF (SRPRS.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'SRPRS',srprs)
        CALL ECREADR (ECDIRFLE,ECONO,'LSENI',lseni)
        CALL ECREADR (ECDIRFLE,ECONO,'DUSRI',dusri)
        ! LAH Following set up to allow for change in stem fraction
        ! Currently not used ... just one stem fraction (STFR)
        CALL ECREADR (ECDIRFLE,ECONO,'SWFRX',swfrx)
        CALL ECREADR (ECDIRFLE,ECONO,'SWFRN',swfrn)
        CALL ECREADR (ECDIRFLE,ECONO,'SWFNL',swfrnl)
        CALL ECREADR (ECDIRFLE,ECONO,'SWFXL',swfrxl)
        CALL ECREADR (ECDIRFLE,ECONO,'SLACF',lawcf)
        CALL ECREADR (ECDIRFLE,ECONO,'KCAN',kcan)
        CALL ECREADR (ECDIRFLE,ECONO,'BR1FX',brfx(1))
        CALL ECREADR (ECDIRFLE,ECONO,'BR2FX',brfx(2))
        CALL ECREADR (ECDIRFLE,ECONO,'BR3FX',brfx(3))
        CALL ECREADR (ECDIRFLE,ECONO,'BR4FX',brfx(4))
        CALL ECREADR (ECDIRFLE,ECONO,'BR5FX',brfx(5))
        CALL ECREADR (ECDIRFLE,ECONO,'BR6FX',brfx(6))
        ! Following may have been (temporarily) in the CUL file
        ! Radiation use efficiency
        IF (PARUE.LE.0.0) CALL ECREADR (ECDIRFLE,ECONO,'PARUE',parue)
        IF (PARU2.LT.-89.0) CALL ECREADR (ECDIRFLE,ECONO,'PARU2',paru2)
        ! Leaf area
        IF (LA1S.LE.0.0) CALL ECREADR (ECDIRFLE,ECONO,'LA1S',la1s)
        IF (LAXS.LE.0.0) CALL ECREADR (ECDIRFLE,ECONO,'LAXS',laxs)
        IF (LAWS.LE.0.0) CALL ECREADR (ECDIRFLE,ECONO,'SLAS',laws)
        ! Roots
        IF (RDGS.LE.0.0) CALL ECREADR (ECDIRFLE,ECONO,'RDGS',rdgs)
        ! Reduction factors
        IF (NFGU.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'NFGU',nfgu)
        IF (NFGL.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'NFGL',nfgl)
        ! N uptake
        IF (NO3CF.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'NUPNF',no3cf)
        IF (H2OCF.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'NUPWF',h2ocf)
        IF (RTNO3.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'RTNUP',rtno3)
        IF (RTNH4.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'RTNH4',rtnh4)
        IF (NO3MN.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'NO3MN',no3mn)
        IF (NH4MN.LT.0.0) CALL ECREADR (ECDIRFLE,ECONO,'NH4MN',nh4mn)
        
        !-----------------------------------------------------------------------
        !       Read species information
        !-----------------------------------------------------------------------
        
        CALL FVCHECK(SPDIRFLE,GENFLCHK)
        CALL SPREADR (SPDIRFLE,'CRFR' ,crfr)
        CALL SPREADR (SPDIRFLE,'CO2CC',co2compc) 
        CALL SPREADR (SPDIRFLE,'CO2EX',co2ex)
        CALL SPREADR (SPDIRFLE,'HDUR' ,hdur)
        CALL SPREADR (SPDIRFLE,'SLAFF',lawff)
        CALL SPREADR (SPDIRFLE,'SLATR',lawtr)
        CALL SPREADR (SPDIRFLE,'SLATS',lawts)
        CALL SPREADR (SPDIRFLE,'SLAWR',lawwr)
        CALL SPREADR (SPDIRFLE,'LLIFG',llifg)
        CALL SPREADR (SPDIRFLE,'LLIFS',llifs)
        CALL SPREADR (SPDIRFLE,'LLIFX',llifx)
        CALL SPREADR (SPDIRFLE,'LLIG%',lligp)
        CALL SPREADR (SPDIRFLE,'LLOSA',llosa)
        CALL SPREADR (SPDIRFLE,'LWLOS',lwlos)
        CALL SPREADR (SPDIRFLE,'NFSU' ,nfsu)
        CALL SPREADR (SPDIRFLE,'LPEAW',lpeaw)
        CALL SPREADR (SPDIRFLE,'NCRG',ncrg)
        CALL SPREADR (SPDIRFLE,'NTUPF',ntupf)
        CALL SPREADR (SPDIRFLE,'PARIX',parix)
        CALL SPREADR (SPDIRFLE,'LAIXX',laixx)
        CALL SPREADR (SPDIRFLE,'PARFC',parfc)
        CALL SPREADR (SPDIRFLE,'PEMRG',pecm)
        CALL SPREADR (SPDIRFLE,'PGERM',pgerm)
        CALL SPREADR (SPDIRFLE,'PHSV' ,phsv)
        CALL SPREADR (SPDIRFLE,'PHNTF',phintfac)
        CALL SPREADR (SPDIRFLE,'PHTV' ,phtv)
        CALL SPREADR (SPDIRFLE,'PPTHR',ppthr)
        CALL SPREADR (SPDIRFLE,'PTFA' ,ptfa)
        CALL SPREADR (SPDIRFLE,'PTFMN',ptfmn)
        CALL SPREADR (SPDIRFLE,'PTFMX',ptfmx)
        CALL SPREADR (SPDIRFLE,'RATM' ,ratm)
        CALL SPREADR (SPDIRFLE,'RCROP',rcrop)
        CALL SPREADR (SPDIRFLE,'RDGAF',rdgaf)
        CALL SPREADR (SPDIRFLE,'RWULF',rlfwu)
        CALL SPREADR (SPDIRFLE,'RLIG%',rligp)
        CALL SPREADR (SPDIRFLE,'RRESP',rresp)
        CALL SPREADR (SPDIRFLE,'RS%LX',rsclx)
        CALL SPREADR (SPDIRFLE,'RS%O' ,rspco)
        CALL SPREADR (SPDIRFLE,'RSEN' ,rsen)
        IF (RSEN.LT.0.0) CALL SPREADR (SPDIRFLE,'RSEN%' ,rsen)
        CALL SPREADR (SPDIRFLE,'RSFPL',rsfpl)
        CALL SPREADR (SPDIRFLE,'RSFPU',rsfpu)
        CALL SPREADR (SPDIRFLE,'RSUSE',rsuse)
        CALL SPREADR (SPDIRFLE,'RTUFR',rtufr)
        CALL SPREADR (SPDIRFLE,'RWUMX',rwumx)
        CALL SPREADR (SPDIRFLE,'RWUPM',rwupm)
        CALL SPREADR (SPDIRFLE,'SAWS' ,saws)
        CALL SPREADR (SPDIRFLE,'SDDUR',sddur)
        CALL SPREADR (SPDIRFLE,'SDN%',sdnpci)
        CALL SPREADR (SPDIRFLE,'SDRS%',sdrsf)
        CALL SPREADR (SPDIRFLE,'SDWT' ,sdsz)
        CALL SPREADR (SPDIRFLE,'SLIG%',sligp)
        CALL SPREADR (SPDIRFLE,'SGRO2',shgr(2))
        CALL SPREADR (SPDIRFLE,'TPAR' ,tpar)
        CALL SPREADR (SPDIRFLE,'TSRAD',tsrad)
        CALL SPREADR (SPDIRFLE,'WFEU' ,wfeu)
        CALL SPREADR (SPDIRFLE,'WFGEU',wfgem)
        CALL SPREADR (SPDIRFLE,'WFGU' ,wfgu)
        CALL SPREADR (SPDIRFLE,'WFGL' ,wfgl)
        CALL SPREADR (SPDIRFLE,'WFPU' ,wfpu)
        CALL SPREADR (SPDIRFLE,'WFPL' ,wfpl)
        CALL SPREADR (SPDIRFLE,'WFRGU',wfrtg)
        CALL SPREADR (SPDIRFLE,'WFSU' ,wfsu)
        CALL SPREADR (SPDIRFLE,'NLAB%',nlabpc)
        ! LAH Following set up to allow for change in stem fraction
        ! Currently not used ... just one stem fraction (STFR)
        IF (SWFRN.LE.0.0) CALL SPREADR (SPDIRFLE,'SWFRN',swfrn)
        IF (SWFRNL.LE.0.0) CALL SPREADR (SPDIRFLE,'SWFRNL',swfrnl)
        IF (SWFRXL.LE.0.0) CALL SPREADR (SPDIRFLE,'SWFXL',swfrxl)
        IF (SWFRX.LE.0.0) CALL SPREADR (SPDIRFLE,'SWFRX',swfrx)
        ! Following may be temporarily in ECO or CUL file
        IF (PD(9).LE.0.0) CALL SPREADR (SPDIRFLE,'P9',pd(9))
        IF (DUSRI.LT.0.0) CALL SPREADR (SPDIRFLE,'DUSRI',dusri)
        IF (LLIFA.LE.0.0) CALL SPREADR (SPDIRFLE,'LLIFA',llifa)
        IF (SRFR.LE.0.0) CALL SPREADR (SPDIRFLE,'SRFR',srfr)
        IF (RDGS.LE.0.0) CALL SPREADR (SPDIRFLE,'RDGS',rdgs)
        IF (LAXS.LE.0.0) CALL SPREADR (SPDIRFLE,'LAXS',laxs)
        IF (RLWR.LE.0.0) CALL SPREADR (SPDIRFLE,'RLWR',rlwr)
        IF (NFGL.LT.0.0) CALL SPREADR (SPDIRFLE,'NFGL',nfgl)
        IF (NLLG.LE.0.0) CALL SPREADR (SPDIRFLE,'NLLG',nllg)
        IF (NFGU.LE.0.0) CALL SPREADR (SPDIRFLE,'NFGU',nfgu)
        IF (NFPU.LE.0.0) CALL SPREADR (SPDIRFLE,'NFPU',nfpu)
        IF (NFPL.LE.0.0) CALL SPREADR (SPDIRFLE,'NFPL',nfpl)
        IF (KCAN.LE.0.0) CALL SPREADR (SPDIRFLE,'KCAN',kcan)
        IF (LAWCF.LE.0.0) CALL SPREADR (SPDIRFLE,'SLACF',lawcf)
        IF (LAWMNFR.LE.0.0) CALL SPREADR (SPDIRFLE,'SLAMN',lawmnfr)
        IF (DFPE.LT.0.0) CALL SPREADR (SPDIRFLE,'PPFPE',dfpe)
        IF (PPEXP.LT.0.0) CALL SPREADR (SPDIRFLE,'PPEXP',ppexp)
        ! N uptake 
        IF (NO3CF.LT.0.0) CALL SPREADR (SPDIRFLE,'NUPNF',no3cf)
        IF (H2OCF.LT.0.0) CALL SPREADR (SPDIRFLE,'NUPWF',h2ocf)
        IF (RTNO3.LT.0.0) CALL SPREADR (SPDIRFLE,'RTNUP',rtno3)
        IF (RTNH4.LT.0.0) CALL SPREADR (SPDIRFLE,'RTNH4',rtnh4)
        IF (NO3MN.LT.0.0) CALL SPREADR (SPDIRFLE,'NO3MN',no3mn)
        IF (NH4MN.LT.0.0) CALL SPREADR (SPDIRFLE,'NH4MN',nh4mn)
        IF (RSFRS.LT.0.0) CALL SPREADR (SPDIRFLE,'RSFRS',rsfrs)

        CALL SPREADC (SPDIRFLE,'HPROD',hprod)
        CALL SPREADC (SPDIRFLE,'PPSEN',ppsen)
        CALL SPREADRA (SPDIRFLE,'LN%S','2',lnpcs)
        CALL SPREADRA (SPDIRFLE,'RN%S','2',rnpcs)
        CALL SPREADRA (SPDIRFLE,'SN%S','2',snpcs)
        CALL SPREADRA (SPDIRFLE,'LN%MN','2',lnpcmn)
        CALL SPREADRA (SPDIRFLE,'RN%MN','2',rnpcmn)
        CALL SPREADRA (SPDIRFLE,'SN%MN','2',snpcmn)
        CALL SPREADRA (SPDIRFLE,'CHT%','10',chtpc)
        CALL SPREADRA (SPDIRFLE,'CLA%','10',clapc)
        CALL SPREADRA (SPDIRFLE,'CO2RF','10',co2rf)
        CALL SPREADRA (SPDIRFLE,'CO2F','10',co2f)

        ! Temperature responses
        CALL SPREADRA (SPDIRFLE,'TRDV1','4',trdv1)
        IF (trdv1(1).LT.-98.0) THEN
            OPEN (UNIT = FNUMERR,FILE = 'ERROR.OUT')
            WRITE(fnumerr,*) ' '
            WRITE(fnumerr,*) ' No temp response data for development'
            WRITE(fnumerr,*) ' Please check'
            WRITE(*,*) ' No temperature response data for development'
            WRITE(*,*) ' Program will have to stop'
            CLOSE (fnumerr)
            STOP ' '
        ENDIF
        
        CALL SPREADRA (SPDIRFLE,'TRDV2','4',trdv2)
        IF (trdv2(1).LT.-98.0) TRDV2 = TRDV1
        
        CALL SPREADRA (SPDIRFLE,'TRGEM','4',trgem)
        CALL SPREADRA (SPDIRFLE,'TRLFG','4',trlfg)
        CALL SPREADRA (SPDIRFLE,'TRPHS','4',trphs)
        IF (diffacr(1).LT.0.0) CALL SPREADRA (SPDIRFLE,'DIFFR','3',diffacr)
        CALL SPREADCA (SPDIRFLE,'PSNAME','20',psname)
        CALL SPREADCA (SPDIRFLE,'PSABV','20',psabv)
        CALL SPREADCA (SPDIRFLE,'PSTYP','20',pstyp)
        
    END SUBROUTINE CS_Read_Genotype