C=======================================================================
C  TR_TRNSPL_GROSUB, Subroutine
C
C  Allows for transplanting shock and estimates new values for
C  P1, BIOMAS, RTWT and PLA
C-----------------------------------------------------------------------
C  Revision history
C
C  08/07/1993 PWW Header revision and minor changes
C  08/29/2002 CHP/MUS Converted to modular format for inclusion in CSM.
C                     Split TRANSPL into TRNSPL_GROSUB and TRNSPL_PHENOL
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  FIELD  : Switch to indicate field simulation of all processes but when
C           false then growth without stress as in seedbed
C=======================================================================

      SUBROUTINE TR_TRNSPL_GROSUB (DYNAMIC, SOILPROP,  
     &    CDTT_TP, FIELD, ISWWAT, ISWNIT, ITRANS, P1T,    !Input
     &    PHINT, PLANTS, SDTT_TP, SDWTPL, TPLANTS, XST_TP,!Input
     &    AGEFAC, BIOMAS, CUMPH, ISTAGE, LAI, MLFWT,      !Output
     &    MPLA, MPETWT, NDEF3, NFAC, NSTRES, RCNP, ROOTN, !Output
     &    RTDEP, RTWT, SDEPTH, SEEDRV, STOVN, STOVWT,     !Output
     &    TANC, TCNP, TLFWT, TMNC, TPLA, TRLOS, TREDUCE,  !Output
     &    MCORMWT)                                        !Output

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT  NONE
      EXTERNAL TR_NFACTO
      SAVE

      CHARACTER*1 ISWNIT, ISWWAT
      !CHARACTER*6 ERRKEY
      !PARAMETER (ERRKEY = 'IPPLNT')

      INTEGER DYNAMIC, ISTAGE, ITRANS, NLAYR, I !, TAGEFAC

      REAL AGEFAC, BIOMAS, CDTT_TP, CUMPH, LAI  !, ATEMP
      REAL MCORMWT, MLFWT, MPLA, MPETWT, NDEF3, NFAC
	REAL NSTRES
      REAL P1T, PHINT, PLANTS    !, P1
      REAL RCNP, ROOTN, RTDEP, RTWT
      REAL SD2, SDTT_TP, SDEPTH, SEEDRV, STOVN
      REAL SDWTPL, STOVWT
      REAL TANC, TBIOMS, TCNP, TLFWT !, TAGE, TBASE
      REAL TMNC, TPLA, TPLANTS, TREDUCE, TRLOS
      REAL XST_TP, YPLANTN
      REAL, DIMENSION(NL) :: RLV, DLAYR
      LOGICAL FIELD
	!     The variable "SOILPROP" is of type "SoilType".
      TYPE (SoilType) SOILPROP

!     Transfer values from constructed data types into local variables.
      DLAYR = SOILPROP % DLAYR
      NLAYR = SOILPROP % NLAYR

C-----------------------------------------------------------------------
      IF (ITRANS .EQ. 1 .OR. ITRANS .EQ. 4) RETURN

      !TAGEFAC = 0

      ! Cummulative thermal time
      IF (ITRANS .EQ. 3) THEN
         ! Estimate phenological age of transplant
         !P1 = TAGE*(ATEMP-TBASE)
         !IF (P1 .LT. 240.0 .OR. TAGE .LE. 14.0) THEN
         !   TAGEFAC = 1
         !ENDIF

         CUMPH  = SDTT_TP / PHINT 

         ! Initialize Growth Parameters
         SD2    = SDTT_TP*SDTT_TP*0.001
         MPLA = 120.0*(0.032867-0.0002482*SDTT_TP+0.000741*SD2)
         RTWT = 1.250-0.008*SDTT_TP+0.0495*SD2
         TBIOMS  = SDWTPL    !FROM IPFLOD
         IF (TBIOMS .LE. 0.0) THEN
            TBIOMS = 5.5*RTWT
          ELSE
            RTWT   = TBIOMS/5.5
         ENDIF
         IF (MPLA .GT. TBIOMS/0.0045) THEN
             MPLA = TBIOMS/0.0045
         ENDIF
         MLFWT   = TBIOMS*0.65
         MCORMWT = TBIOMS*0.10
         MPETWT  = TBIOMS - MLFWT - MCORMWT
         BIOMAS  = TBIOMS
         TBIOMS  = 0.
         SEEDRV  = 0.0
         IF (ISWNIT .EQ. 'Y') THEN
c        Call for TR_NFACTO included Mar 26, 2014
            CALL TR_NFACTO(DYNAMIC, FIELD, XST_TP, 
     &      AGEFAC, NDEF3, NFAC, NSTRES, RCNP, TANC, TCNP, TMNC) 
            YPLANTN = ROOTN + STOVN
            STOVN   = TCNP  * BIOMAS*0.5   ! N DEF COMMON IN SEEDLINGS
            ROOTN   = RCNP  * RTWT
	write(*,*) 'in transpl_g after call tr_nfacto nfac=',nfac
         ENDIF
         !
         ! Estimate RLV from root weight
         !
         IF (ISWWAT .EQ. 'Y') THEN
            RTDEP  = AMIN1(10.0,SDEPTH)
            RLV(1) = AMAX1((RTWT*PLANTS*0.5),0.1)
            IF (RTDEP .GT. (DLAYR(1)+DLAYR(2))) THEN
               RLV(2) = RLV(1)*0.9
            ENDIF
         ENDIF
      ENDIF
      !

      PLANTS = TPLANTS
      ! Min. delay is 20 C
      !
      ! Reduce biomass by 10% and root weight by 50%, 
	! unless seedling pastjuvenile stage
      !
      TREDUCE = 0.8
      IF (P1T .LT. CDTT_TP) THEN
         TREDUCE = AMAX1(0.4,TREDUCE*P1T/CDTT_TP)
      ENDIF
      !
      ! ITRANS2 assumes nonlimting growth, use 0.8 to reduce growth
      !
      IF (ITRANS .EQ. 2) THEN
         RTDEP = 0.5*RTDEP
      ENDIF
      !  RTDEP   = AMAX1 (RTDEP,SDEPTH)
      BIOMAS  = BIOMAS * TREDUCE
      MPLA    = MPLA   * TREDUCE
      RTWT    = RTWT   * 0.55 * TREDUCE
      MLFWT   = MLFWT  * TREDUCE
      MPETWT  = MPETWT * TREDUCE
      MCORMWT = MCORMWT* TREDUCE
      RLV(1)  = RLV(1) * TREDUCE
      RLV(2)  = RLV(2) * TREDUCE
      IF (ISTAGE .GT. 1) THEN
         TPLA  = TPLA  * TREDUCE
         TLFWT = TLFWT * TREDUCE
      ENDIF
      LAI    = (MPLA+TPLA)*PLANTS*0.0001
      STOVWT = BIOMAS
      !
      ! Estimate bias root distribution to surface
      !
      RLV(1) = RLV(1) + 0.75*RLV(2)
      RLV(2) = 0.25*RLV(2)
      IF (NLAYR .GT. 2) THEN
         DO I = 3, NLAYR
            RLV(I) = 0.0
         END DO
      ENDIF
      IF (ISWNIT .NE. 'N') THEN
         IF (ITRANS .EQ. 2) THEN
            YPLANTN = ROOTN + STOVN
         ENDIF
         STOVN = TCNP * STOVWT*0.9
         ROOTN = RCNP * RTWT*0.25
         TRLOS = PLANTS*10.0*(YPLANTN-ROOTN-STOVN)
      ENDIF

      RETURN
      END SUBROUTINE TR_TRNSPL_GROSUB
