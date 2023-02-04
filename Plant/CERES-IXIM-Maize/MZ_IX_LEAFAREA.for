C=======================================================================
C  MZ_IX_LEAFAREA, Subroutine
C
C  Simulates leaf area expansion and leaf senescence
C  Reference: Lizaso et al., 2003. Field Crops Research 80:1-17
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written                                       JIL        Aug 2001
C  2. Included SLA and C demand for leaf growth     JIL        Aug 2006
C  3. Modified and adapted for IXIM model           JIL        Sep 2007
C-----------------------------------------------------------------------
C  Called : GROSUB
C
C  Calls  : 
C=======================================================================	
      SUBROUTINE MZ_IX_LEAFAREA (DYNAMIC,                       !Control
     &   AX,LX,GDDAE,GROLF,ISTAGE,LFN,PAR,PHINT,PLTPOP,PATHSR,  !Input
     &   AGEFAC,FILES,SATFAC,PStres2,TURFAC,TEMPM,TLNO,XSTAGE,  !Input
     &   NSTRES,SWFAC,PLAE,                                     !Input
     &   GLA,LA,LAD,LAP,LATOT,LFL,LFWTD,LNEXP,PGROLF,PLAG,SEN,  !Output
     &   SENLA,YX)                                              !Output

      USE ModuleDefs
      IMPLICIT NONE
      EXTERNAL GETLUN, FIND, ERROR, IGNORE
      SAVE

      REAL      A3
      REAL   	A4
      REAL      AGEFAC
      REAL      AK
      REAL      AX
      CHARACTER*80 C80            
      REAL      CRLL
      INTEGER	DYNAMIC
      INTEGER   ERR       
      CHARACTER*6     ERRKEY 
      REAL      EXPONENT               
      CHARACTER*92    FILECC
      CHARACTER*12    FILES      
      INTEGER     FOUND              
      REAL      GDDAE
      REAL      GLA(50)
      REAL      GR
      REAL      GROLF
      INTEGER	I      
      INTEGER	IGDD   
      REAL      II
      INTEGER   ISECT      
      INTEGER   ISTAGE 
      INTEGER   J      
      INTEGER   JGDD   
      REAL      JJ
      INTEGER   KGDD   
      REAL      KK(50)
      REAL      LA(50)
      REAL      LAD(50)
      REAL      LAGR
      REAL      LAP(50)
      REAL      LATOT
      REAL      LEAFTT(50)
      REAL      LFCN(50)
      REAL      LFL(50)
      REAL      LFLGV
      INTEGER   LFN  
      INTEGER   LFNOLD
      REAL      LFWT(50)
      REAL      LFWTD(50)
      REAL      LLX
      REAL      LM
      INTEGER   LNEXP  
      INTEGER   LNUM                      
      INTEGER   LUNCRP            
      REAL      LX
      REAL      NSTRES
      REAL      PAR
      CHARACTER*80    PATHSR      
      REAL      PGROLF
      REAL      PHINT
      REAL      PLAE
      REAL      PLAG
      REAL      PLTPOP
      REAL      POPINT
      REAL      POPSLP
      REAL      PRED
      REAL      PStres2
      REAL      RLL
      REAL      SATFAC
      CHARACTER*6     SECTION             
      REAL      SEN(50)
      REAL      SENGR
      REAL      SGR
      REAL      SWFAC
      REAL      LSLA(50)
      REAL      SLAINT
      REAL      SLAL
      REAL      SLAMN
      REAL      SLAMX
      REAL      SENLA
      REAL      SLASL
      REAL      SLASLP
      REAL      SLAX
      REAL      SRED
      REAL      TEMPM
      REAL      TLNO
      REAL      TRED
      REAL      TURFAC
      REAL      WK
      REAL      WLL
      REAL      XCK
      REAL      XSTAGE
      REAL      XZERO(50)
      REAL      XZEROS(50)
      REAL      YGDDAE
      REAL      YK
      REAL      YLL
      REAL      YX(50)
      REAL      YX1

C	DOUBLE PRECISION AX,LX

C ** JIL Inputs AX,LX read from cultivar file in MZ_IX_GROSUB

!----------------------------------------------------------------------
!                     DYNAMIC = RUNINIT
!----------------------------------------------------------------------

      IF(DYNAMIC.EQ.RUNINIT.OR.DYNAMIC.EQ.SEASINIT) THEN

!     ****************************************************************
!                     READ SPECIES FILE
!     ****************************************************************
      FILECC =  TRIM(PATHSR) // FILES
      CALL GETLUN('FILEC', LUNCRP)
      OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,0)

      SECTION = '*LEAF '
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(3F8.3)',IOSTAT=ERR) A3,A4,AK
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(2F8.3)',IOSTAT=ERR) YK,YLL
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(3F8.3)',IOSTAT=ERR) SLAX,SLAMX,SLAMN
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

      ENDIF
      REWIND(LUNCRP)

      CLOSE (LUNCRP)

      CRLL      = 0.0
      GR        = 0.0
      I         = 0
      IGDD      = 0
      II        = 0.0
      J         = 0
      JGDD      = 0
      JJ        = 0.0
      KGDD      = 0
      LAGR      = 0.0
      LATOT     = 0.0
      LFLGV     = 0.0
      LFNOLD    = 50
      LLX       = 0.0
      LM        = 0.0
      LNEXP     = 0
      PGROLF    = 0.0
      PLAG      = 0.0
      POPINT    = 0.0
      POPSLP    = 0.0
      PRED      = 0.0
      RLL       = 0.0
      SENGR     = 0.0
      SGR       = 0.0
      SLASL     = 9.0 - 0.275*PLTPOP
      SLAINT    = 0.0
      SLAL      = 0.0
      SENLA     = 0.0
      SLASLP    = 0.0
      SRED      = 1.0
      TRED      = 1.0
      WK        = 0.0
      WLL       = 0.0
      XCK       = 1.0
      YGDDAE    = 1.0
      YX1       = 0.0

      DO I = 1,50
          KK(I)     = 0.0
          GLA(I)    = 0.0
          LA(I)     = 0.0
          LAD(I)    = 0.0
          LAP(I)    = 0.0
          LEAFTT(I) = 0.0
          LFCN(I)   = 0.0
          LFL(I)    = 0.0
          LFWT(I)   = 0.0
          LFWTD(I)  = 0.0
          LSLA(I)   = 0.0
          SEN(I)    = 0.0
          XZERO(I)  = 0.0
          XZEROS(I) = 0.0
          YX(I)     = 0.0
      ENDDO
      LA(1) = PLAE
      
!-----------------------------------------------------------------------
!                     DYNAMIC = INTEGR
!-----------------------------------------------------------------------
      ELSEIF(DYNAMIC.EQ.INTEGR) THEN   

C *****************************************************************
C **                                                             **
C **                    LEAF EXPANSION SECTION                   **
C **                                                             **
C *****************************************************************

C ** Initializing variables

      IF (TLNO .NE. 0.0 .AND. LFNOLD .EQ. 50) THEN	
         LFNOLD = LFN	
         LFN = NINT(TLNO)
      ENDIF

      LM = 0.67 * LFN         !From Birch et al, 1998
      WK = LFN / 8.18
      IGDD = NINT(YGDDAE)
      JGDD = NINT(GDDAE)

      IF (JGDD .EQ. 0) THEN
        JGDD = 1
        CRLL = 0.0            !Initialization
      ENDIF

C ** Calculating leaf area expansion parameters

      IF (ISTAGE .LE. 3) THEN

C ** Temperature effect on leaf expansion
C    Calculated from Salah and Tardieu, 1996. J of Exp Bot 47:1689-1698

        IF (TEMPM .GT. 32.0) THEN
           TRED = 4.8 - 0.118*TEMPM
        ELSE
           TRED = 1.15*(1.0-EXP(-0.2*(TEMPM-9.8)))	     
        ENDIF
        TRED = AMIN1(TRED,1.0)
        TRED = AMAX1(TRED,1.E-8)   !JIL 03/03/2010

C ** Reducing leaf expansion due to water, oxygen, nitrogen or phosphorus stresses
C    Included PStres2 here
        SRED = AMIN1(TURFAC,(1-SATFAC),AGEFAC,PStres2)
        SRED = AMAX1(SRED,1.E-8)   !JIL 03/03/2010        

        LATOT = 0.0
        PLAG = 0.0

        DO I=1,LFN

          II=REAL(I)
          YX(I)=1.016*AX*EXP((A3*((II-LM)/(LM-1))**2)
     &          +(A4*((II-LM)/(LM-1))**3))

C ** Reducing potential leaf size due to population density

          IF (PLTPOP .LE. 5.0) THEN
            IF (II .GT. LM) THEN
              PRED=1.0+0.115
     &                *EXP(-0.5*(((II/REAL(LFN))-0.876)/0.06)**2.0)
            ELSE
              PRED = 1.0
            ENDIF
          ELSE
            IF (II .LT. LM) THEN
              POPINT = 0.968+0.0005*PLTPOP
              POPSLP = -0.044+0.013*PLTPOP
            ELSE
              POPINT = 0.772+0.047*PLTPOP-0.0009*PLTPOP**2
              POPSLP = -0.4044+0.098*PLTPOP-0.0016*PLTPOP**2
            ENDIF

            PRED = POPINT-POPSLP*(II/REAL(LFN))
            PRED = AMIN1(PRED,1.0)
          ENDIF
          YX(I) = YX(I)*PRED
         
          KK(I) = YK+(AK*EXP(-((II-XCK)**2.0)/(2.0*WK**2.0)))
     &            + 0.031/(1.+EXP(-2.141*(II-LFN+0.5)))

          IF (I .LE. 2) THEN
           XZERO(I) = (2.0*(II - 1.0)) * 10.0   !  0, 20
          ELSE
           XZERO(I) = ((II-2.0) * PHINT + 8.0) + (2.197/KK(I))
          ENDIF

C ** Calculating individual leaf growth rate

         IF (LAP(I) .LT. YX(I)) THEN       !Leaf expanding
            LAGR = 0.0
            DO J=IGDD,JGDD
              JJ=REAL(J)
 
!             CHP 11/27/2007 getting overflows here -- put in a check
!             chp 8/5/2009 check low numbers, too.
              EXPONENT = -KK(I)*(JJ-XZERO(I))
              IF (EXPONENT > -40. .AND. EXPONENT < 40.) THEN
                GR = (YX(I)*KK(I))*((EXP(EXPONENT))/
     &             (1.0+EXP(EXPONENT))**2)
              ELSE
                GR = 0.0
              ENDIF

              IF (I .EQ. 1 .AND. J .EQ. 1) THEN
                GR = 0.6*YX(I)
              ENDIF
            LAGR = LAGR + GR
            END DO

            IF ((LAP(I)+LAGR) .GT. YX(I)) THEN
              LAGR = YX(I)-LAP(I)
            ENDIF

          ELSE                              !Leaf reached full size
            LAGR = 0.0
          ENDIF

C ** Calculating individual LA including stress & temperature and accumulating

          IF (I .LE. 5) THEN
            LAD(I)=LAGR*SRED
          ELSE
            LAD(I)=LAGR*AMIN1(SRED,TRED)
          ENDIF

          LA(I) = LA(I) + LAD(I)
          PLAG = PLAG + LAD(I)
          LAP(I) = LAP(I) + LAGR
          LATOT = LATOT + LA(I)

          IF (ISTAGE.LT.3.AND.LAP(I).EQ.YX(I).AND.LNEXP.LT.I) THEN
            LNEXP = I    !Leaf number that has completed expansion
          ENDIF

        END DO

        IF (JGDD .EQ. 1) THEN
          GROLF = PLAG / SLAX
        ENDIF

C ** Calculating individual leaf demand for biomass and nitrogen

C ** Effect of light on SLA
C    Calculated from Warrington and Norton, 1991. J Amer Soc Hort Sci 116:544-551.

        SLAL = 0.44+(1.9-0.46)*EXP(-0.165*PAR)
        PGROLF = 0.0
        DO I=1,LFN

C ** Effect of leaf position on SLA at today's light intensity
C    Calculated from Thiagarajah and Hunt, 1982. Can J Bot 60:1647-1652.
          LSLA(I) = SLAL * (SLAMN+(SLAMX-SLAMN) * 
     &             EXP(-SLASL*(REAL(I)/REAL(LFN))))
          LSLA(I) = AMIN1(LSLA(I),SLAX)

C ** Effect of mean temperature on SLA
C    Calculated from Thiagarajah and Hunt, 1982. Can J Bot 60:1647-1652.
          IF (TEMPM .LT. 32.0) THEN
             SLAINT = (0.0003*TEMPM**2.0)-(0.0095*TEMPM)+1.0696
             SLASLP = 0.0222*TEMPM-0.8313

             LSLA(I) = LSLA(I)*(SLAINT+SLASLP*(REAL(I)/REAL(LFN)))
          ENDIF

C ** Individual leaf demand for biomass
!	    IF (LAD(I) .GT. 0.0) THEN
          IF (LAD(I) .GT. 1.E-9) THEN  !chp 08/10/09
            LFWTD(I) = LAD(I) / LSLA(I)    !Leaf biomass demand,g/leaf d

C	      LFND    = LFND + LFWTD(I)*0.05 !Leaf N demand, g N/plant
                                         !New leaf tissue target: 5% [N]
            LFWT(I) = LFWT(I)+LFWTD(I)   !For checking purposes only
          ELSE
            LFWTD(I) = 0.0
          ENDIF

C ** Potential leaf tissue growth, g/pl, used in NUPTAK for leaf N demand
          IF (I .LE. 5) THEN 
            PGROLF = PGROLF+LFWTD(I)/SRED
          ELSE
            PGROLF = PGROLF+LFWTD(I)/AMIN1(SRED,TRED)
          ENDIF

        ENDDO

      ELSE
        PLAG   = 0.0
        PGROLF = 0.0
      ENDIF

C *****************************************************************
C **                                                             **
C **                    LEAF LONGEVITY SECTION                   **
C **                                                             **
C *****************************************************************

      LLX = 3.6 + (0.52*LFN)
      WLL = 0.35 * LFN
C
C ** JIL Accelerated senescence due to stresses
C
      IF (XSTAGE .LE. 3.0) THEN
        RLL=(1.0-AMIN1(SWFAC,(1-SATFAC),NSTRES,PStres2))*0.7
       ELSEIF (XSTAGE .LE. 4.5) THEN
        RLL=(1.0-AMIN1(AMAX1(SWFAC,0.5),(1-SATFAC),NSTRES,PStres2))*0.5
       ELSEIF (XSTAGE .LE. 5.5) THEN
        RLL=(1.0-AMIN1(AMAX1(SWFAC,0.5),(1-SATFAC),NSTRES,PStres2))*0.25
       ELSE
        RLL=(1.0-AMIN1(AMAX1(SWFAC,0.5),(1-SATFAC),NSTRES,PStres2))*0.01
      ENDIF

      DO I=1,LFN
         II=REAL(I)

C ** Initializing longevity of each leaf
         LFLGV = YLL + LX*EXP(-((II-LLX)**2.0)/(2.0*WLL**2.0))
         IF (LFL(I) .EQ. 0.0 .AND. SEN(1) .EQ. 0.0) THEN
            LFL(I) = LFLGV
         ENDIF
         IF (LFL(I) .EQ. 0.0 .AND. LFNOLD .LT. 50) THEN
            LFL(I) = LFLGV
         ENDIF

C ** Under stress reduce longevity
         IF (GDDAE .GT. XZERO(I)) THEN
         LFL(I)=LFL(I)-YLL*RLL
           LFL(I)=AMAX1(LFL(I),0.0)
         ENDIF

      END DO

C ** Longer longevity for remaining green leaf area after stress
!	IF (RLL .GT. 0.0) THEN
      IF (RLL .GT. 0.1) THEN
         CRLL = CRLL+RLL
        ELSEIF (CRLL .GT. 0.0) THEN
         IF (ISTAGE .GT. 3) THEN
           DO I=1,LFN
             IF (GLA(I) .GT. 0.0) THEN
                LFL(I) = LFL(I)+YLL*CRLL*0.6
             ENDIF
           ENDDO
           CRLL = 0.0
         ENDIF
 !	   CRLL = 0.0
      ENDIF
C
C *****************************************************************
C **                                                             **
C **                    LEAF SENESCENCE SECTION                  **
C **                                                             **
C *****************************************************************
C
C ** Calculating phenological leaf senescence
      SENLA = 0.0
      DO I=1,LFN
         YX1 = LA(I)
         XZEROS(I) = XZERO(I) + LFL(I)
         KGDD = IGDD - NINT(YLL*RLL)

         IF (SEN(I) .LT. LA(I)) THEN
           SENGR = 0.0
           DO J=KGDD,JGDD
             JJ=REAL(J)
!            CHP 11/27/2007 getting overflows here -- put in a check
             EXPONENT = -KK(I)*(JJ-XZEROS(I))
             IF (EXPONENT > -40. .AND. EXPONENT < 40.) THEN
               SGR = (YX1*KK(I))*((EXP(EXPONENT))/
     &             (1.0+EXP(EXPONENT))**2.)
             ELSE
               SGR = 0.0
             ENDIF

           SENGR = SENGR + SGR
           END DO
           IF ((SEN(I)+SENGR) .GT. LA(I)) THEN
             SENGR = LA(I)-SEN(I)
           ENDIF
         ELSE
           SENGR = 0.0
         ENDIF

         SEN(I) = SEN(I) + SENGR
         SENLA = SENLA + SEN(I)
         GLA(I) = LA(I) - SEN(I)

      END DO

      IF (SENLA .GT. LATOT) THEN
        SENLA = LATOT
      ENDIF

      YGDDAE = GDDAE
      
      ENDIF       !Endif for DYNAMIC LOOP

      RETURN

      END

