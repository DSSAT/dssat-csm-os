C=======================================================================
C  ML_TILLSUB, Subroutine
C
C  Determines tiller growth in Millet
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes             P.W.W.      2-8-93
C  4. Converted to modular format                   W.D.B.      7-31-02
C-----------------------------------------------------------------------
C  INPUT  : None
C
C  LOCAL  : B,C
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : MGROSUB
C
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  B      : Temporary variable
C  C      : Temporary variable
C=======================================================================

      SUBROUTINE ML_TILLSUB (DYNAMIC,TURFAC,AGEFAC,
     &G1,CUMDTT,TPLAG,TLAG1,TGROLF,TGROSTM,TCARBO,
     &CARBO,SUMDTT,DTT,TEMF, ISTAGE)

      USE ModuleDefs
   
      IMPLICIT NONE
      SAVE

      INTEGER DYNAMIC

      REAL     TLAG2,TURFAC,AGEFAC
c-------------------------------------------------------
c      Variables that were in common blocks
c      and should be passed as arguments
c-------------------------------------------------------
      REAL G1,CUMDTT,TPLAG,TLAG1,TGROLF,TGROSTM,TCARBO
      REAL CARBO,SUMDTT,DTT,TEMF
      INTEGER ISTAGE
c-------------------------------------------------------
      IF(DYNAMIC.EQ.RUNINIT.OR.DYNAMIC.EQ.SEASINIT) THEN
      TPLAG = 0.0
      TGROLF = 0.0
      TGROSTM = 0.0
      TCARBO = 0.0
      TLAG1 = 0.0
      TLAG2 = 0.0
      
      ENDIF    
    
      IF(DYNAMIC.EQ.INTEGR) THEN
      IF (ISTAGE .LE. 2) THEN
         TLAG2   = G1*(32.0*EXP(0.00827*(CUMDTT-125.))-28.0)
         TPLAG   = (TLAG2-TLAG1)*AMIN1(TURFAC,TEMF,AGEFAC)
         TGROLF  = TPLAG*.0040
         TGROSTM = TGROLF*0.3
         TCARBO  = TGROLF+TGROSTM
         IF (TCARBO .GT. 0.35*CARBO) THEN
            TCARBO  = 0.35*CARBO
            TGROLF  = TCARBO*(TGROLF/(TGROLF+TGROSTM))
            TGROSTM = TCARBO-TGROLF
            TPLAG   = TGROLF/0.0040
         ENDIF
         IF (TCARBO .LT. CARBO*0.20) THEN
            TCARBO  = CARBO*0.20

!           chp 3/18/2005 fixed zero divide
!           TGROLF  = TCARBO*(TGROLF/TGROLF+TGROSTM)
            IF (TGROLF+TGROSTM > 1.E-8) THEN  
              TGROLF = TCARBO * TGROLF / (TGROLF + TGROSTM)
            ELSE
              TGROLF = TCARBO / 2.0  
            ENDIF

            TGROLF  = AMIN1 (TGROLF,TCARBO)
            TGROSTM = TCARBO-TGROLF
            TPLAG   = TGROLF/.0040
         END IF
       ELSEIF (ISTAGE .EQ. 3) THEN
         TGROLF     = TCARBO*(0.60-0.00022*SUMDTT)     ! 0.60
         TPLAG      = TGROLF/.0045
         TGROSTM    = TCARBO-TGROLF
       ELSEIF (ISTAGE .EQ. 4) THEN
         TGROSTM    = 0.15*DTT
         TGROSTM    = AMIN1 (TGROSTM,TCARBO)
         TGROLF     = TCARBO-TGROSTM
         TPLAG      = TGROLF/0.0065
      END IF

      TLAG1 = TLAG2
      ENDIF

      RETURN
      END SUBROUTINE ML_TILLSUB
