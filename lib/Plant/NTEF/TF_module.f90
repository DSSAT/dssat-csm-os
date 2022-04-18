!=======================================================================

      MODULE TF_module
!     Contains defintion of derived data types and constants which are 
!     used throughout the model.
      SAVE

!     05/09/2013 the names emergence and mature conflict with variable names in the SALUS module
!     01/11/2018 KEP converted WH_ sub-routines to TF_.
!     Need to create a separate N-wheat module for these parameters (remove from Module_Defs)
      INTEGER, PARAMETER ::                                             &
         !WHAPS wheat model, NWheat crop development stages:            
        emergence  = 1,    & ! Emergence to End of Juvenile              
        endjuv     = 2,    & ! End of Juvenile to End of Vegetative growth   
        endveg     = 3,    & ! End of Vegetative Growth to End of Ear growth 
        endear     = 4,    & ! End of Ear Growth to Start of Grain Filling   
        grnfil     = 5,    & ! Start of Grain Filling to Maturity            
        mature     = 6,    & ! Maturity                                  
        fallow     = 7,    & ! No crop present to Sowing                     
        sowing     = 8,    & ! Sowing to Germination                         
        germ       = 9,    & ! Germination to Emergence                      
        root_part  = 1,    & !                                               
        leaf_part  = 2,    & !                                               
        lfsheath_part = 3, & ! Plant parts, written to avoid conflict     
        stem_part  = 4,    & ! with same part name in other DSSAT models     
        grain_part = 5,    & !                                               
        seed_part  = 6,    & !                                               
        photo_nw  = 1,     & ! For designating swdef water-stress array      
        cellxp = 2,        & ! For designating swdef water-stress array      
        tiller_nw = 3,     & ! For designating swdef water-stress array
        mxpart =  6          !Maximum number of plant parts (FSR Nwheat)

!======================================================================
      END MODULE TF_module
!======================================================================
