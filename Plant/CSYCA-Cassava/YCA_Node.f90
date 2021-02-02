Module YCA_Node !Module of environment
    type Node_type
        
        ! TODO DA 21MAR2017
        ! [ ] rename variables to give more semantic
        ! [ ] detect variables that can be functions instead
        ! [ ] generate getters and setters
        ! [ ] create the node and stem objects and move the respective variables 
        REAL    :: AFLF                         ! CH2O factor for leaf,average   #          ! (From SeasInit) !LPM 23MAR15 to have a value by canopy  and leaf level 
        REAL    :: DALF                         ! Days during which leaf active  d          ! (From SeasInit)  !LPM 28MAR15 Adjusted to consider two dimensions 
        REAL    :: DGLF                         ! Days during which leaf growing #          ! (From SeasInit) !LPM 28MAR15 Adjusted to consider two dimensions    
        REAL    :: DSLF                         ! Days during which leaf senesng #          ! (From SeasInit)  !LPM 28MAR15 Adjusted to consider two dimensions 
        REAL    :: LAGETT                       ! Leaf age after leaf emergence  C.d        ! (From SeasInit) !LPM 25MAR15 Adjusted to consider two dimensions    
        REAL    :: LAGL                         ! Leaf area growth,shoot,lf pos  cm2/l      ! (From SeasInit) !LPM 25MAR15 Adjusted to consider two dimensions  
        REAL    :: LAGL3                        ! Leaf area growth,shoot,lf+assim cm2/l     ! (From SeasInit) !LPM 15NOV15 Added to save leaf area by cohort
        REAL    :: LAGL3T                       ! Leaf area by cohort lf+assim   cm2/cohort ! (From SeasInit) !LPM 15NOV15 Added to save leaf area by cohort
        REAL    :: LAGLT                        ! Leaf area growth by cohort     cm2/cohort ! (From SeasInit) !LPM 25OCT15 added to keep the leaf area by cohort
        REAL    :: LAIByCohort                  ! Leaf area index by cohort      #          !                 !DA  28OCT2016 Added to save eaf area index from cohort data   
        REAL    :: LANC                         ! Leaf actual N concentration    #
        REAL    :: LAPOTX                       ! Leaf area potentials,maxima    cm2/l      ! (From SeasInit)  
        REAL    :: LAPOTX2                      ! Leaf area potentials,max/day   cm2/l      ! (From SeasInit) !LPM 24APR2016 To estimate a modified daily leaf area potential modified by temperature 
        REAL    :: LAPP                         ! Leaf area diseased,leaf posn   cm2/p      ! (From SeasInit) !LPM 28MAR15 Adjusted to consider two dimensions currently it is not used    
        REAL    :: LAPS                         ! Leaf area senesced,leaf posn   cm2/p      ! (From SeasInit) !LPM 28MAR15 Adjusted to consider two dimensions   
        REAL    :: LAPSTMP                      ! Leaf area senesced,temporary   cm2/p      ! (From Growth)
        REAL    :: LATL                         ! Leaf area,shoot,lf#,potential  cm2/l      ! (From SeasInit)  
        REAL    :: LATL2                        ! Leaf area,shoot,lf#,+h2o,n,tem cm2/l      ! (From SeasInit)  
        REAL    :: LATL2T                       ! Leaf area by cohort +h2o,n,tem cm2/cohort ! (From SeasInit)!LPM 15NOV15 Added to have the leaf area by cohort
        REAL    :: LATL3                        ! Leaf area,shoot,lf#,+assim.    cm2/l      ! (From SeasInit)  
        REAL    :: LATL3T                       ! Leaf area by cohort lf#,+assim cm2/cohort ! (From SeasInit)!LPM 15NOV15 Added to have the leaf area by cohort +assim
        REAL    :: LATL4                        ! Leaf area,shoot,lf#,+assim.+N  cm2/l      ! (From SeasInit)  
        REAL    :: LATLPOT                      ! Leaf area,shoot,leaf,pot       cm2/l      ! (From Growth)    
        REAL    :: LATLPREV                     ! Leaf area,shoot,leaf,prev.     cm2/l      ! (From Growth)    
        REAL    :: LEAFNEXCESSN                 ! Leaf N > critical/node         g/p   
        REAL    :: LEAFNN                       ! Leaf N/node                    g/p        !
        INTEGER :: LBIRTHDAP                    ! DAP on which leaf initiated    #          ! (From Growth)   !LPM 25MAR15 Adjusted to consider two dimensions  
        REAL    :: LCNC                         ! Leaf critical max N conc/node  #/n        !
        REAL    :: LCNM                         ! Leaf critical min N conc/node   #/n
        INTEGER :: LDEATHDAP                    ! DAP on which leaf 100% dead    #          ! (From Integrate) 
        REAL    :: LNCM                         ! Leaf N conc,minimum            fr         ! (From SeasInit)
        REAL    :: LNCR                         ! Leaf N relative to maximum     #          ! (From SeasInit)
        REAL    :: LNCX                         ! Leaf N conc,maximum            fr         !
        REAL    :: LNDEMN                       ! Leaves demand for N by node    g/n/p      ! 
        REAL    :: NDDAE                        ! DAE when a new node appears     ! DA 13DIC2016
        REAL    :: NDEMLMN                      ! N demand for growth/leaf min   g/p        !
        REAL    :: NDEMSMN                      ! N demand for growth/node min   g/p        !LPM 25MAY2015 addet to consider stem N by node cohort
        REAL    :: NFLF                         ! N factor for leaf,average      #          ! (From SeasInit) !LPM 23MAR15 Adjusted to consider two dimensions  
        REAL    :: NFLF2                        ! N factor for leaf area adj     #          ! (From SeasInit) !LPM 23MAR15 Adjusted to consider two dimensions 
        REAL    :: NFLFP                        ! N factor phs leaf,average      #          ! (From SeasInit) !LPM 25MAR15 Adjusted to consider two dimensions  
        REAL    :: NODEWT                       ! Node wt  by cohort             g/p        ! LPM 11APR15
        REAL    :: NODEWTG                      ! Node wt growth by cohort       g/d/p      ! LPM 02MAR15 
        REAL    :: NODEWTGB                     ! Leaf wt growth     g/d/leaf   ! DA 16DIC16 
        REAL    :: NPOOLLN                      ! Leaf N pool by node            g/p        !
        REAL    :: NPOOLSN                      ! Stem N pool by node            g/p        ! LPM 25MAY2015 Added to consider different N concentration by node 
        REAL    :: SANC                         ! Stem N concentration           #          ! (From SeasInit) !LPM 25MAY2015 change the dimensions to include values by node
        REAL    :: SCNC                         ! Stem critical max N conc/node  #/n        !LPM 25MAY2015 Added to estimate the value by cohort
        REAL    :: SCNM                         ! Stem critical min N conc/node  #/n       !LPM 25MAY2015 Added to estimate the value by cohort
        REAL    :: SENNLFGN                     ! Senesced N from leaves/node    g/p
        REAL    :: SENNLFGRSN                   ! Senesced N from leaves/node,rs g/p
        REAL    :: SNCM                         ! Stem N conc,minimum            fr         ! (From SeasInit)  
        REAL    :: SNCR                         ! Stem N relative to maximum     #          ! (From SeasInit) !LPM 25MAY2015 Modified to include cohorts   
        REAL    :: SNCX                         ! Stem N conc,maximum            fr         ! (From SeasInit) !LPM 23MAY2015 Modified to include cohorts  
        REAL    :: SNDEMN                       ! Stem demand for N by node      g/n/p        !
        REAL    :: SNPHN                        ! Stem N harvested by node       g/n/p        !
        REAL    :: STEMNN                       ! Stem N by cohort               g/n/p      ! !LPM 23MAY2015 added to consider N concentration by node  
        REAL    :: STEMNEXCESSN                 ! Stem N > critical by node      g/n/p      ! !LPM 23MAY2015 added to consider N concentration by node
        REAL    :: TFDLF                        ! Temp factor,dev for leaf,av    #          ! (From SeasInit) !LPM 25MAR15 Adjusted to consider two dimensions  
        REAL    :: TFGLF                        ! Temp factor,exp for leaf,av    #          ! (From SeasInit)  !LPM 25MAR15 Adjusted to consider two dimensions  
        REAL    :: WFLF                         ! H2O factor for leaf,average    #          ! (From SeasInit) !LPM 23MAR15 Change to consider two dimensions
        !TYPE    :: leafStatus                         ! Status of the leaf
            
    contains

    
    end Type Node_type
    
    !ENUM, BIND(C) :: Status
    !    ENUMERATOR :: EMERGING = 0, ACTIVE = 1, SENESCING=2, FALL=3
    !END ENUM
    
    ! interface to reference the leaf constructor
    interface Node_type
        module procedure Node_type_constructor
    end interface Node_type
    
    contains
    
    ! constructor for the leaf type
    type (Node_type) function Node_type_constructor()
        implicit none
        !real, intent (in) :: AreaGrowth
        Node_type_constructor%aflf = 0.0
        Node_type_constructor%dalf = 0.0
        Node_type_constructor%dglf = 0.0
        Node_type_constructor%dslf = 0.0
        Node_type_constructor%lagett = 0.0
        Node_type_constructor%lagl = 0.0
        Node_type_constructor%lagl3 = 0.0
        Node_type_constructor%lagl3t = 0.0
        Node_type_constructor%laglt = 0.0
        Node_type_constructor%laibycohort = 0.0
        Node_type_constructor%lanc = 0.0
        Node_type_constructor%lapp = 0.0
        Node_type_constructor%LAPOTX = 0.0
        Node_type_constructor%LAPOTX2 = 0.0
        Node_type_constructor%laps = 0.0
        Node_type_constructor%latl = 0.0
        Node_type_constructor%latl2 = 0.0 !LPM 15NOV15 added to save leaf area by cohort
        Node_type_constructor%latl2t = 0.0 !LPM 15NOV15 added to save leaf area by cohort
        Node_type_constructor%latl3 = 0.0   !LPM 15NOV15 added to save leaf area by cohort
        Node_type_constructor%latl3t = 0.0 !LPM 15NOV15 added to save leaf area by cohort (considering assimilates restriction)
        Node_type_constructor%latl4 = 0.0 !LPM 15NOV15 added to save leaf area by cohort
        Node_type_constructor%latlpot = 0.0
        Node_type_constructor%LBIRTHDAP = 0
        Node_type_constructor%lcnc = 0.0
        Node_type_constructor%lcnm = 0.0
        Node_type_constructor%LDEATHDAP = 0
        Node_type_constructor%leafnexcessn = 0.0
        Node_type_constructor%leafnn = 0.0
        Node_type_constructor%lncm = 0.0
        Node_type_constructor%lncr = 0.0
        Node_type_constructor%lncx = 0.0
        Node_type_constructor%LNDEMN = 0.0
        Node_type_constructor%nddae = 0.0
        Node_type_constructor%NDEMLMN = 0.0
        Node_type_constructor%NDEMSMN = 0.0
        Node_type_constructor%nflf = 0.0
        Node_type_constructor%nflf2 = 0.0
        Node_type_constructor%nflfp = 1.0
        Node_type_constructor%NODEWTG = 0.0
        Node_type_constructor%NODEWTGB = 0.0 !LPM 11APR15 New variables of node growth
        Node_type_constructor%NODEWT = 0.0 !LPM 11APR15 New variables of node growth
        Node_type_constructor%sanc = 0.0
        Node_type_constructor%scnc = 0.0
        Node_type_constructor%scnm = 0.0
        Node_type_constructor%SENNLFGN = 0.0
        Node_type_constructor%SENNLFGRSN = 0.0
        Node_type_constructor%sncm = 0.0
        Node_type_constructor%sncr = 0.0
        Node_type_constructor%sndemn = 0.0
        Node_type_constructor%snphn = 0.0
        Node_type_constructor%stemnn = 0.0 !LPM 23MAY2015 Added to keep nitrogen content by node
        Node_type_constructor%STEMNEXCESSN = 0.0
        Node_type_constructor%tfdlf = 0.0
        Node_type_constructor%tfglf = 0.0
        Node_type_constructor%wflf = 0.0
        !Node_type_constructor%leadStatus = 0
        
        
    end function Node_type_constructor
    
    
END Module YCA_Node  
    