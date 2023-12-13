C=======================================================================
C  TEFF_TRNSPL_GROSUB, Subroutine
C
C  Allows for transplanting shock and estimates new values for
C  P1, BIOMAS, RTWT and PLA
C-----------------------------------------------------------------------
C  Revision history
C
C  08/07/1993 PWW Header revision and minor changes
C  08/29/2002 CHP/MUS Converted to modular format for inclusion in CSM.
C                     Split TRANSPL into TEFF_TRNSPL_GROSUB and TEFF_TRNSPL_PHENOL
C  12/12/2019 MB/US Copyed from Rice model and modified for Teff 
C  03/29/2021 MB/WP Addapted to Teff based on CERES-Rice
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  FIELD  : Switch to indicate field simulation of all processes but when
C           false then growth without stress as in seedbed
C=======================================================================

      SUBROUTINE TEFF_TRNSPL_GROSUB (DYNAMIC,  
     &    ATEMP, CDTT_TP, FIELD, ISWNIT, ITRANS, NPPH,    !Input
     &    P1T, PANWT, PHINT, PLANTS, SDTT_TP, SDWTPL,     !Input
     &    TAGE, TBASE, TMAX, TPLANTS, XST_TP,             !Input
     &    AGEFAC, BIOMAS, CUMPH, ISTAGE, LAI, LFWT, MLFWT,!Output
     &    MPLA, MSTMWT, NDEF3, NFAC, NSTRES, RCNP, ROOTN, !Output
     &    RTWT, SEEDRV, STMWT, STOVN, STOVWT, TANC,       !Output
     &    TCNP, TLFWT, TMNC, TPLA, TRLOS)                 !Output

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT  NONE
      EXTERNAL TEFF_NFACTO
      SAVE

      CHARACTER*1 ISWNIT
      CHARACTER*6 ERRKEY
      PARAMETER (ERRKEY = 'IPPLNT')

      INTEGER DYNAMIC, ISTAGE, ITRANS, TAGEFAC

      REAL AGEFAC, ATEMP, BIOMAS, CDTT_TP, CUMPH, LAI, LFWT
      REAL MLFWT, MPLA, MSTMWT, NDEF3, NFAC, NPPH, NSTRES
      REAL P_AGE, P1T, PANWT, PHINT, PLANTS
      REAL RCNP, ROOTN, RREDUCE, RTWT
      REAL SD2, SDTT_TP, SEEDRV, STMWT, STOVN
      REAL STOVWT, SDWTPL
      REAL TAGE, TANC, TBASE, TBIOMS, TCNP, TLFWT
      REAL TMAX, TMNC, TPLA, TPLANTS, TREDUCE, TRLOS
      REAL XST_TP, YPLANTN

      LOGICAL FIELD

C-----------------------------------------------------------------------
      IF (ITRANS .EQ. 1 .OR. ITRANS .EQ. 4) RETURN

      TAGEFAC = 0

      ! Cummulative thermal time
      IF (ITRANS .EQ. 3) THEN
         ! Estimate phenological age of transplant
         P_AGE = TAGE*(ATEMP-TBASE)
         IF (P_AGE .LT. 240.0 .OR. TAGE .LE. 14.0) THEN
            TAGEFAC = 1
         ENDIF

         CUMPH  = SDTT_TP / PHINT 

         ! Initialize Growth Parameters
         SD2    = SDTT_TP*SDTT_TP*0.001
         MPLA = 150.0*(0.039867-0.0002482*SDTT_TP+0.00241*SD2)  

!         RTWT = (1.290-0.008*SUMDTT+0.1095*SD2)/SPLANTS      
         RTWT = (1.290 - 0.008 * SDTT_TP + 0.1095 * SD2) / PLANTS      
         TBIOMS  = SDWTPL    !FROM IPFLOD
         IF (TBIOMS .LE. 0.0) THEN
            TBIOMS = RTWT*1.25
         ELSE
            ! Convert from g to mg
            TBIOMS = TBIOMS/1000.0
            RTWT   = TBIOMS
            MPLA   = TBIOMS/0.0045
         ENDIF

         MSTMWT = 0.1*TBIOMS
         MLFWT  = 0.9*TBIOMS
         BIOMAS = TBIOMS
         STOVWT = TBIOMS
         TBIOMS = 0.0
         MPLA   = AMIN1 (MPLA,MLFWT/0.0045)

         IF (ISWNIT .EQ. 'Y') THEN
            CALL TEFF_NFACTO(DYNAMIC, FIELD, XST_TP, 
     &    AGEFAC, NDEF3, NFAC, NSTRES, RCNP, TANC, TCNP, TMNC)  !Output

            YPLANTN = ROOTN + STOVN
            STOVN   = TCNP*BIOMAS*0.5   ! N def common in seedlings
            ROOTN   = RCNP*RTWT
         ENDIF

      ENDIF

      PLANTS = TPLANTS*NPPH  

  ! Min. delay is 20 C.  Reduce biomass by 10% and root weight by 50%,
  ! unless seedling past juvenile stage.
      IF (TMAX .GE. 15 .AND. TMAX .LT. 28) THEN
         TREDUCE = 0.90
         RREDUCE = 0.75
      ELSE
         TREDUCE = 0.80
         RREDUCE = 0.55
      ENDIF

      IF (P1T .LT. CDTT_TP) THEN
         TREDUCE = AMAX1 (0.4,TREDUCE*P1T/CDTT_TP)
      ENDIF

      IF (TAGEFAC .EQ. 1) THEN
         TREDUCE = 0.9
         RREDUCE = 1.0
      ENDIF

      SEEDRV = 0.0
      BIOMAS = BIOMAS * TREDUCE
      MPLA   = MPLA   * TREDUCE
      RTWT   = RTWT   * TREDUCE * RREDUCE
      MLFWT  = MLFWT  * TREDUCE
      MSTMWT = MSTMWT * TREDUCE
!      PLTWT  = BIOMAS + RTWT !NOT USED

      IF (ISTAGE .GT. 1) THEN
         TPLA  = TPLA *TREDUCE
         TLFWT = TLFWT*TREDUCE
      ENDIF

      LAI    = (MPLA+TPLA)*PLANTS*0.0001
      LFWT   = MLFWT + TLFWT
      STMWT  = MSTMWT
      STOVWT = LFWT+STMWT+PANWT

      TRLOS    = 0.0     !FROM INPLNT
      IF (ISWNIT .EQ. 'Y') THEN
         IF (ITRANS .EQ. 2) THEN
            YPLANTN = ROOTN + STOVN
         ENDIF
         STOVN = TCNP*STOVWT*0.9
         ROOTN = RCNP*RTWT*0.25
         TRLOS = PLANTS*10.0*(YPLANTN-ROOTN-STOVN)
      ENDIF

      RETURN
      END SUBROUTINE TEFF_TRNSPL_GROSUB
