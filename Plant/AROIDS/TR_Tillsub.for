C=======================================================================
C  TR_TILLSUB, Subroutine
C
C  Determines tillering
C-----------------------------------------------------------------------
C  Revision history
C
C  08/07/1993 PWW Header revision and minor changes
C  08/29/2002 CHP/MUS Converted to modular format for inclusion in CSM.
C=======================================================================

      SUBROUTINE TR_TILLSUB (DYNAMIC, ISTAGE, 
     &    BGRAD, BGRAD2, BINT, BINT2, 
     &    DTT, FLOOD, G2, G3, KSHADE, LEAFT, LAI,         !Input
     &	NSTRES, P3, P4, P5, PETGR2, PETGR5, RGCORMT, 
     &    RTR, SLAT1, SLAT2, SLAT5, SRAD, SUCINT, SUCX1, 
     &    SUCX2, SUCX3, SUMDTT, TCARBC1, TCARBC2, TCARBC3, 
     &    TCARBL2, TCARBL5, TCARBO, TCLRAT1, TCMAT, 
     &    TCORMWT, TGROCOM, TILNO, TLFWT, TMIN, TMAX,     !Output
     &    TPETWT,  TPLA, TURFAC, XN)                      !Output

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      SAVE

      INTEGER DYNAMIC, ISTAGE, LEAFT

      REAL DTT, FLOOD, G2, G3, GM, P3, P4, P5
      REAL SUCINT, SUCX1, SUCX2, SUCX3, BINT, BINT2, BGRAD, BGRAD2
      REAL KSHADE, SLAT1, SLAT2, SLAT5
      REAL TCARBC1, TCARBC2, TCARBC3, TCARBL2, TCARBL5
      REAL TCLRAT1, PETGR2, PETGR5, RGCORMT, TCMAT 
!      REAL AGEFAC            
      REAL LAI, TMIN, TMAX, NSTRES, RTR, SUMDTT, SRAD, TCARBO 
      REAL TURFAC, XN      !TEMF,                               
      REAL TILNO, TLFWT, TPETWT, TCORMWT, TPLA                

      REAL TGROCOM, TGROLF, TGROPET, TGROTOP, TPLAG  

      REAL DPTILNO,DTILNO,ECRAT,PTILNO,YPTILNO
      REAL SWTIL, TRSM, TILNSTR

	REAL B, SUCTMF, SUCLF, TCARBL1 
   
C     LOCAL  : SLFN,SLFT,SLFW,SWTIL,PTILNO,TSTAGE,RTR,TRSM,TILST
C              ECRAT,DPTILNO,DTILNO,SLW,SENTIL,TILS,B,GM
C

	! Default values for variables ... now read in from SPE files
      !
      ! SUCINT  = 1.25    SUCX1   = 0.750   SUCX2   = 0.0375 
	! SUCX3  = 0.0
      ! TCARBC1 = 0.80    BINT    = 0.50    BGRAD   = 0.0005  
	! KSHADE = 0.65
      ! RESPF   = 0.25    TCARBC2 = 0.85    BINT2   = 0.60    
	! BGRAD2 = 0.00015
      ! TCARBL5 = 0.50    LEAFT   = 8       SLAT1   = 0.00350 
	! TCLRAT1= 0.20
	! TCARBL2 = 0.20    SLAT2   = 0.00350 PETGR2  = 1.45   
	!  PETGR5 = 1.15
	! TCARBC3 = 0.85    SLAT5   = 0.00350 RGCORMT = 0.03   
	!  TCMAT  = 0.25       

!***********************************************************************
!***********************************************************************
!     Seasonal Initialization - Called once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
!     Initialization - from inplt:
      TILNO    = 0.0
      YPTILNO  = 0.0 
      TPLA     = 0.5
      PTILNO   = 0.0

!***********************************************************************
!***********************************************************************
!     Daily rate / integration calculations
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------
      SWTIL    = 1.0
      TRSM     = 1.0
      IF (FLOOD .LE. 0.0) THEN
         SWTIL = 1.5*EXP(TURFAC-1.0)-0.5
      ENDIF
      SWTIL   = AMAX1 (SWTIL,0.0)
      TILNSTR = 1.0
      IF (NSTRES .LT. 1.0) THEN
         TILNSTR = 0.85*NSTRES**2
      ENDIF
      IF (XN .LE. LEAFT+1.) THEN
         YPTILNO = 0.0
         PTILNO  = 0.0
         DTILNO  = 0.0
         TILNO   = 0.0
       ELSE
         IF (ISTAGE .LE. 5) THEN        
            YPTILNO = AMAX1 (PTILNO,0.0)
            GM      = AMAX1 (G3,1.0)
            SUCLF   = LEAFT + 1.
            PTILNO  = G3*(SUCINT+SUCX1*G3*(XN-SUCLF)-
     &                SUCX2*(GM*(XN-SUCLF))**2+
     &                SUCX3*(G3*(XN-SUCLF))**3)    
            IF (DTT .LE. 0.0) THEN
               DTT = 1.0
            ENDIF
            RTR     = SRAD/DTT
            TRSM    = EXP(-KSHADE*LAI)          
            TRSM    = AMAX1 (TRSM,0.45)
            ECRAT   = AMIN1(RTR*TRSM,1.0)*TILNSTR
            SUCTMF  = 1.0-0.015*((0.25*TMIN+0.75*TMAX)-28.)**2
            SUCTMF  = AMAX1 (SUCTMF,0.0)
            SUCTMF  = AMIN1 (SUCTMF,1.0)
	      IF (ISTAGE .EQ. 5 .AND. G2 .GT. 1.0) THEN
	         PTILNO = PTILNO/G2   !   LESS TILLERS WHEN PETIOLES LARGE
            ENDIF
            DPTILNO = AMAX1((PTILNO-YPTILNO),0.0)
            DTILNO  = DPTILNO*ECRAT*SUCTMF*AMIN1(TILNSTR,SWTIL)
            TILNO   = TILNO + DTILNO
            TILNO   = AMAX1 (TILNO,0.0)
         ENDIF
      ENDIF

      TGROLF  = 0.0
      TGROPET = 0.0
	TPLAG   = 0.0
      IF (ISTAGE .NE. 5 ) RETURN
      SELECT CASE (ISTAGE)
          CASE (1)
            B       = BINT+BGRAD*SUMDTT      
            IF (B .GT. TCARBC1) THEN
                B = TCARBC1
            ENDIF
            TCARBL1 = TCARBC1*TCLRAT1  !0.2
            TPLAG   = B*TCARBO*TCARBL1/SLAT1 !*AMIN1(TURFAC,NSTRES,TEMF)
            TGROCOM = B*TCARBO
            TGROLF  = TPLAG*SLAT1 !/AMIN1(AGEFAC,TURFAC)
            TGROPET = TCARBO - TGROLF - TGROCOM
            TGROTOP = TCARBO - TGROCOM
	!write(*,*) 'tgp tgl rat',tgropet,tgrolf,tgropet/tgrolf
	!pause
          CASE (2,3,4)
            B       = BINT2  + BGRAD2*SUMDTT/(P3+P4)
	!WRITE(*,*) 'B INT GRAD ',B,BINT2,BGRAD2
            IF (B .GT. TCARBC2) THEN
                B = TCARBC2
            ENDIF
            TPLAG   = TCARBO*TCARBL2/SLAT2 !*AMIN1(TURFAC,AGEFAC,TEMF)
            !TGROCOM = B*TCARBO
            TGROLF  = TPLAG*SLAT2
            TGROPET = TGROLF * PETGR2*G2
            TGROTOP = TGROLF + TGROPET
            IF (TGROTOP .GT. TCARBO) THEN
	         TGROPET = TGROPET/TGROTOP*TCARBO
	         TGROLF  = TCARBO-TGROPET
	         TGROTOP = TCARBO
            ENDIF
            TGROCOM = TCARBO - TGROLF - TGROPET
	!WRITE(*,*)'TPT TCM TLF',TGROPET/TCARBO,TGROCOM/TCARBO,
      !&TGROLF/TCARBO,TGROLF/TPLAG
      !WRITE (*,*) 'tpg tCOM TLF RAT3 B=',Tgropet,TGROCOM,TGROLF,B,
      !&tgropet/tCARBO,TGROCOM/TCARBO,tgrolf/tCARBO
      END SELECT
      
      IF (ISTAGE .EQ. 5) THEN
          TGROCOM = RGCORMT*(1.0 + TCMAT*SUMDTT/P5)*
     &              DTT    !*AMIN1(SWFAC,NSTRES,TEMF)     !*RMAT
	! WRITE(*,*)' TCoM TCaB RAT',TGROCOM,TCARBO,
      !& TGROCOM/TCARBO
          IF (TGROCOM .LT. 0.0) THEN
             TGROCOM = 0.0
          ENDIF
          IF (TGROCOM .GT. TCARBO) THEN
             TGROCOM = TCARBO*AMIN1(TCARBC3/G3,0.70)
          ENDIF
          TGROTOP = TCARBO - TGROCOM
          TPLAG   = TGROTOP*TCARBL5*G3/SLAT5 
		!*AMIN1(TURFAC,NSTRES,TEMF)    
          TGROLF  = TPLAG * SLAT5
          TGROPET = TGROLF * PETGR5 * G2

          IF (TGROTOP .GT. TGROPET+TGROLF) THEN
             TGROCOM  = TCARBO - TGROLF - TGROPET
          ENDIF
	!WRITE (*,*)'TgC TC3 0.70',TGROCOM/TCARBO,TCARBC3/G3
      !&tgropet/tgrolf,slat5,tgrolf/tplag,TGROCOM/TCARBO,TGROCOM
      ENDIF

      TLFWT   = TLFWT   + TGROLF
      TPETWT  = TPETWT  + TGROPET
      TCORMWT = TCORMWT + TGROCOM
	TPLA    = TPLA    + TPLAG
      !WRITE(*,*)'TGTOP TGP+L tc chk til wt SW',TGROTOP,TGROLF+TGROPET
      !&,tcarbo,tgrolf+tgropet+tgrocom,tilno,(tlfwt+tpetwt+tcormwt)/
      !&tilno,SWTIL
 
!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
      RETURN
      END SUBROUTINE TR_TILLSUB
