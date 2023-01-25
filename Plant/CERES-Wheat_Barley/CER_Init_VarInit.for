!***********************************************************************
! This is the code from the section (DYNAMIC == RUNINIT)
! Subroutine CER_Init_VarInit initializes state and rate variables.
!***********************************************************************
    
      SUBROUTINE CER_Init_VarInit (LAI, CANHT, DEWDUR,
     &   NFP, RESCALG, RESLGALG, RESNALG, RLV,
     &   STGDOY, TRWUP, UH2O)

! 2023-01-25 CHP removed unused variables from argument list
!     SENCALG, SENNALG, UNH4ALG, UNO3ALG
        
        USE ModuleDefs
        USE CER_First_Trans_m

        IMPLICIT     NONE
        EXTERNAL clear_CER_First_Trans_m
        
        INTEGER STGDOY(20), ADAT10            
        
        REAL LAI, CANHT, DEWDUR, NFP, KCAN !, HARVFRAC(2)  
        REAL RESCALG(0:20), RESLGALG(0:20) 
        REAL RESNALG(0:20), RLV(20)  !, SENCALG(0:20)
        REAL TRWUP, UH2O(20)  !SENNALG(0:20), UNH4ALG(20), UNO3ALG(20),
        REAL RESLGAL(0:20), RESNAL(0:20), RESWAL(0:20), RESWALG(0:20)
        REAL SNOW
        
        
        !---------------------------------------------------------------
        !       Reinitializing plant variables
        !---------------------------------------------------------------       
        CALL clear_CER_First_Trans_m()
        !---------------------------------------------------------------
        !       Initialize both state and rate variables                   
        !---------------------------------------------------------------
        
        ISTAGE = 7   ! Pre-planting
        XSTAGE = 7.0 ! Pre-planting
        YEARPLT = 9999999
        YEARPLTP = 9999999
        stgdoy = 9999999
        
        adat10 = -99
        canht = 0.0
        canhtg = 0.0
        dewdur = -99.0
        lai = 0.0
        nfp = 1.0
        pari = 0.0
        parip = -99.0
        rescalg = 0.0
        reslgal = 0.0
        reslgalg = 0.0
        resnal = 0.0
        resnalg = 0.0
        reswal = 0.0
        reswalg = 0.0
        rlv = 0.0
        snow = 0.0
        trwup = 0.0
        uh2o = 0.0

      
        ! Ecotype coefficients re-set
        PD = -99
        PD2FR = -99 
        PD(2) = -99 
        PD(3) = -99 
        PD(4) = -99
        PD4FR = -99
        paruv = -99
        parur = -99
        lapot = -99
        laws = -99
        lafv = -99
        lafr = -99
        veff = -99
        grnmn = -99
        grns = -99
        nfpu = -99
        nfpl = -99
        nfgu = -99
        nfgl = -99
        rdgs1 = -99
        rtno3 = -99
        rtnh4 = -99
        ppfpe = -99
        P4SGE = -99
        canhts = -99
        awns = -99
        kcan = -99
        tvr1 = -99
        rspcs = -99
        ti1lf = -99
        wfpu = -99
        wfgu = -99
        wfpgf = -99
        lt50h = -99
        rdgs2 = -99
        tifac = -99
        tilpe = -99
        tilds = -99
        tilde = -99
        tildf = -99
        lsens = -99
        lsene = -99
        phintf = -99
        phintl = -99
        
        ! Species re-set
        PD4FR = -99
        ppfpe = -99
        lsens = -99
        lsene = -99
        lwlos = -99
        tildf = -99
        lt50s = -99
        tklf = -99
        hdur = -99
        pd2fr = -99
        p4sge = -99
        nfgu = -99
        nfgl = -99
        nfpu = -99
        nfpl = -99
        wfpu = -99
        wfpu = -99
        wfpgf = -99
        tvr1 = -99
        nftu = -99
        nftl = -99
        wfsu = -99
        nfsu = -99
        wfnuu = -99
        rlfnu = -99
        ncnu = -99
        xnfs = -99
        rtno3 = -99
        rtnh4 = -99


      END SUBROUTINE CER_Init_VarInit
