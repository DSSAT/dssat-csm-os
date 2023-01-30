C=======================================================================
C  EQUIL2, Subroutine, U. Singh
C  Determines diffusive fluxes between the floodwater and surface soil
C  layer for each of ammonium, nitrate and urea
!  Also used for oxidation layer calculations for non-flooded conditions.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C             US  Written
C  03/29/2002 CHP modular format
!  01/12/2009 CHP fixed bug introduced with correction of underflow errors
!  04/27/2021 FO/GH Added protection for division by zero for VOLOX and VOLSW
C=======================================================================

      SUBROUTINE EQUIL2 (
     &    BD1, SURCEC, DLAYR, FLOOD, IHDAY, ISI,          !Input
     &    OXLT, OXMIN3, OXMIN4, SW1, YRDOY, YRDRY,        !Input
     &    IBP, FSPEC, OXSPEC, SSPEC)                      !I/O 

      USE ModuleDefs
      IMPLICIT NONE
      EXTERNAL AMTHERM
      SAVE

      INTEGER  ISI, YRDOY, YRDRY, IHDAY, IBP
      REAL SPPM,OXSPEC,AQDC,DIFFN,OXC,FSPEC,FLOODC,SSPEC,PART,FAC1
      REAL XAMT,SAMT,SMIN,OXMIN,FIS,VOLSW,VOLOX,VOLFLD
      REAL DELX,OXDIFF,YSSPEC,YSMIN,YOXSPEC,OXSPP,OXSC,BPS,OXMINC
      REAL SOILC,BPOXL,SSPP,EQUILC,POTFLUX,EXDFAC,DFAC,DELC,DE
      REAL DLAYR(NL)
      REAL OXFAC, DLAYR1, FLOOD, OXLT, BP, OXMIN3, OXMIN4
      REAL SW1, BD1, SURCEC

      ! The 10-5 is included in calculation below
      ! VOLFLD in of water 1 ha field in litres where FLOOD is in mm.
      !
      DLAYR1 = DLAYR(1)
      FAC1  = 1.0 / (BD1 * 1.E-01 * DLAYR1)
      OXFAC = 1.0 / (BD1 * OXLT * 1.0E-01)
      OXC  = OXSPEC * OXFAC
      SPPM = SSPEC * FAC1

!     For non-flooded conditions, reduce layer 1 thickness by 
!         oxidation layer thickness.
      IF (FLOOD .LE. 1.E-6) THEN
         DLAYR1 = DLAYR(1) - OXLT
         FAC1 = 1.0 / (BD1 * 1.E-01 * DLAYR1)
         IF (SPPM .GT. OXC) THEN
            OXC    = SPPM
            OXSPEC = OXC / OXFAC
         ENDIF
      ENDIF
      PART = DLAYR1 / DLAYR(1)
      !
      ! ISI : Species indicator  1 = urea, 2 = nitrate, 3 = ammonium
      !
      SELECT CASE (ISI)
        CASE (1)
          !Urea
          XAMT  = 0.0
          SAMT  = 0.0
          SMIN  = 0.0
          OXMIN = 0.0
          FIS   = 0.9
          BP    = 1.0
          AQDC  = 1.8

        CASE (2)
          !Nitrate
          XAMT  = OXMIN3
          SAMT  = 0.0
          BP    = 1.0
          SMIN  = 0.0
          OXMIN = OXMIN3
          FIS   = 0.9
          AQDC  = 1.8

        CASE (3)
          !Ammonium
          XAMT  = OXMIN4
          SAMT  = 0.0 !0.01
          SMIN  = 0.0  !0.01/PART
          OXMIN = OXMIN4
          FIS   = 1.0
          AQDC  = 0.8
      END SELECT
      !
      ! VOLSW in 1 ha soil in litres
      !
      VOLSW = DLAYR1 * SW1 * 1.E5
      VOLOX = OXLT   * SW1 * 1.E5

!---------------------------------------------------------------------
!     FLOODED CONDITIONS
!---------------------------------------------------------------------
      IF (FLOOD .GT. 1.E-6) THEN
         VOLFLD = 1.E4 * FLOOD
         !
         ! FLOODC (mg/l)= amt(kg/ha)/VOLFLD(l)*1.E6(kg to mg)
         !
         FLOODC = FSPEC / FLOOD * 100.0
         DELX   = DLAYR1 * 0.5
         OXDIFF = 0.0
         IF (YRDOY .EQ. YRDRY .AND. OXC .GT. SPPM) THEN
            YSSPEC  = SSPEC
            YSMIN   = SMIN
            YOXSPEC = OXSPEC
            SSPEC   = OXSPEC
            OXC     = OXSPEC*OXFAC
            SPPM    = OXC
            SMIN    = OXMIN
            VOLSW   = VOLOX
            DELX    = OXLT*0.5
            OXDIFF  = 1.0
         ENDIF    
   
!---------------------------------------------------------------------
!     NON-FLOODED CONDITIONS
!---------------------------------------------------------------------
       ELSE
         !
         ! Set up OX layer concentrations
         !
         IF (OXSPEC .LT. 1.E-10) THEN
            OXSPEC = 0.0
         ENDIF
         IF ((SSPEC-OXSPEC) .LT. SMIN) THEN
            OXSPEC = SSPEC - SMIN
            SSPEC  = SMIN
            OXSPEC = AMAX1 (OXSPEC,OXMIN)
          ELSE
            SSPEC  = SSPEC - OXSPEC
         ENDIF
         OXC   = OXSPEC * OXFAC
         SPPM  = SSPEC  * FAC1
         OXSPP = (OXSPEC-OXMIN)*FIS
!FO 04/27/2021 - Added protection for division by zero for VOLOX              
         IF(VOLOX .GT. 0.0) THEN
           OXSC  = OXSPP/VOLOX*1.E6
         ELSE
           OXSC = 0.0
         ENDIF
!         EQUIN = OXSPEC + SSPEC
         IF (OXSPEC .LT. 1.E-6) THEN
            OXSPP = 0.0
         ENDIF
      ENDIF

      BPS = 1.0
      IF (ISI .EQ. 3) THEN
         !Ammonium
         OXMINC = 0.0  !  0.001 * FAC1
         CALL AMTHERM (SPPM,SOILC,BD1,SURCEC,1,BPS,OXMINC, IBP)

         IF (FLOOD .LE. 1.E-6) THEN
            CALL AMTHERM (OXC,OXSC,BD1,SURCEC,2,BPOXL,OXMINC, IBP)
            BPS = AMIN1 (BPOXL,BPS)
         ENDIF
      ENDIF
      !
      ! Calculate concentrations
      ! Conc (mg/l) = amt(kg/ha)/vol(l)*1.E6 (kg to mg)
      !
      SSPP = (SSPEC-SMIN)*FIS
      IF (SSPP .LT. 1.E-6) THEN
         SSPP = 0.0
      ENDIF

      IF (ISI .EQ. 3) THEN
         !Ammonium
         IF (FLOOD .LE. 1.E-6) THEN
            OXSPP = OXSC*VOLOX*1.E-6
         ENDIF
         SSPP = SOILC*VOLSW*1.E-6
         IF (SSPP .LT. 1.E-6) THEN
            SSPP = 0.0
         ENDIF
      ELSE
!FO 04/27/2021 - Added protection for division by zero for VOLSW        
         IF(VOLSW .GT. 0.0) THEN
           SOILC = SSPP/VOLSW*1.E6
         ELSE
           SOILC = 0.0
         ENDIF
      ENDIF

      SOILC = AMAX1 (SOILC,0.0)

      IF (OXSPP .LT. 1.E-6) THEN
          OXSPP = 0.0
      ENDIF
      !
      ! If POTFLUX is positive flow is from soil to Floodwater
      !
      IF (FLOOD .GT. 1.E-6) THEN
         EQUILC  = (FSPEC + SSPP) / (VOLFLD + VOLSW) * 1.E6
         POTFLUX = (EQUILC*VOLFLD*1.E-6) - FSPEC

         IF (POTFLUX .GE. 1.E-10) THEN
            POTFLUX = 0.0
          ELSE
            EXDFAC  = 2.25*POTFLUX
            EXDFAC  = AMAX1 (EXDFAC,-40.0)
            DFAC    = 1-EXP(-(0.025+EXP(EXDFAC))*SURCEC)
            DIFFN   = (POTFLUX/FLOAT(IHDAY))*DFAC
            IF ((FSPEC + DIFFN) .LT. 1.E-10) THEN
               DIFFN  = -FSPEC
            ENDIF
            IF (OXDIFF .EQ. 1.0) THEN
               SSPEC  = YSSPEC
               SMIN   = YSMIN
               OXDIFF = 0.0
            ENDIF
            GOTO 100
         ENDIF
      ENDIF
      !
      ! Diffusion distance is half thickness of top layer (usu 5 cm)
      ! Assume no resistance to diffusion once ion is in floodwater
      !
      IF (FLOOD .GT. 1.E-6) THEN
         !
         ! Concentration gradient
         !
         DELC = SOILC - FLOODC
       ELSE
         DELX = DLAYR1 * 0.5 - OXLT * 0.5
         DELC = SOILC - OXSC
      ENDIF
      !
      ! Calculate effective diffusion coefficient from soil to floodwat
      ! diffsn coeff AQDC is 1.E-5 cm2/s
      !
      DE = AQDC * 1.E-5 * SQRT(SW1) * SW1 / BPS
      !
      ! DE = AQDC(ISI)*1.E-5*SQRT(0.90*po(1))*Sat(1)/BPS
      ! (=360)=( s to hr)*1.E-6(mg to kg)/[1.E3(lit to cm3)*
      ! 1.E-8 (cm2 to ha)]
      !
!     CHP prevent underflow errors
      IF (ABS(DELC) > 1.E-10 .AND. ABS(DE) > 1.E-10) THEN
        DIFFN = DELC/DELX*DE*360.0*24.0/FLOAT(IHDAY)
      ELSE
        DIFFN = 0.0
      ENDIF

      IF (DIFFN .LT. 1.E-10) THEN
         !
         ! From oxlayer to bulk soil
         !
         IF ((DIFFN + OXSPEC - OXMIN) .LT. 1.E-6) THEN
            DIFFN = OXMIN - OXSPEC
         ENDIF
       ELSE
         !
! From bulk soil to floodwater or bulk soil to oxlayer or oxl to fldw
         !
         IF (DIFFN .GT. SSPEC-SMIN) THEN
            DIFFN = SSPEC - SMIN
         ENDIF
      ENDIF
      !
      ! Ensure Mass Balance is preserved
      ! Floodwater to soil when diffusion is negative
      !
100   SSPEC = SSPEC - DIFFN

      IF ((SMIN-SSPEC) .GT. 1.E-10) THEN
         SSPEC = SMIN
      ENDIF

      IF (FLOOD .GT. 1.E-6) THEN
         !Flooded
         FSPEC = FSPEC + DIFFN
         !
         ! Update concentrations
         !
         FLOODC = FSPEC / FLOOD * 100.0
         IF (OXDIFF .EQ. 1.0) THEN
            !
            ! From floodwater to oxlayr
            !
            OXSPEC = SSPEC
            OXC    = OXSPEC * OXFAC
            SSPEC  = YSSPEC - YOXSPEC + OXSPEC
            OXDIFF = 0.0
         ENDIF
         SPPM  = SSPEC   * FAC1

       ELSE
         !Non-flooded
         OXSPEC = OXSPEC + DIFFN
         IF ((OXMIN-OXSPEC) .GT. 1.E-10) THEN
            OXSPEC = OXMIN
         ENDIF
         OXC    = OXSPEC * OXFAC
         SSPEC  = SSPEC  + OXSPEC
         SPPM   = SSPEC  * FAC1
      ENDIF

      RETURN
      END SUBROUTINE EQUIL2


C=======================================================================
C  AMTHERM, Subroutine, U. Singh
C  Determines Ammonium Adsorption/Desorption Isotherm
C-----------------------------------------------------------------------
C  REVISION HISTORY
C             US  Written
C  03/29/2002 CHP modular format
C=======================================================================

      SUBROUTINE AMTHERM (SPPM,SOILC,PBD,SURCEC,KFLAG,BPOX,SMINC, IBP)

      IMPLICIT NONE

      INTEGER  KFLAG, IBP
      REAL     SPPM,SURCEC,PBD,BPOX,SMINC,SOILC,EFFC,B,BP1,C,SOLN
      REAL     BP,A,ALNSOL

C     LN[SOLN] = LN(A)+B*LN[SOIL]
C     With [SOLN] in mol*10^6/ml and [soil] in mol*10^6/cc soil
C     B    = 2.5 with SURCEC of 30
C     EFFC = AMIN1 (3.6,(0.0775*SURCEC))
C
      EFFC = AMIN1 (4.0,0.225*SURCEC**0.65)
      B    = 4.1 - EFFC
C
C     B    = 3.8 - 0.045*SURCEC
C
      B    = AMAX1 (B,0.001)
C
C     Convert mg/l to mol*10^6/cc soil
C     SPPM(ug/g)/14*10^6(ug/mol)*BD(g/cc)*10^6
C
      BP1 = 30.0*(1.0-EXP(-0.065*SURCEC))
      C   = (SPPM-SMINC)/14.0*PBD
      IF (C .GT. -0.1 .AND. C .LT. 1.E-6) THEN
         C    = 0.0
         SPPM = SMINC
      ENDIF
      IF (C .LT. 0.00001 .AND. C. GT. 1.E-6) THEN
         C = 0.0
      ENDIF
!     IF (C .EQ. 0.0) THEN
      IF (ABS(C) < 1.E-6) THEN
         SOLN  = 0.0
         BP    = BP1 + 1.0
         SOILC = 0.0
         IF (KFLAG .EQ. 2) THEN
            BPOX = BP
            RETURN
         ENDIF
       ELSE
         A      = 1.83                               ! A = 0.83
         ALNSOL = B*ALOG(C)-A
         SOLN   = EXP(ALNSOL)
C
C        SOLN (mol*10^6/ml)*14000(mg/mol)/10^6*1000(ml/l)
C
         SOILC  = SOLN*14.0
         SOILC  = AMIN1 (SOILC,SPPM-SMINC)
         BP     = C/SOLN
         BP     = AMAX1 (BP,1.0)
         BPOX   = BP
         IF (KFLAG .EQ. 2) THEN
            RETURN
         ENDIF
      ENDIF

!     IBP is initialized to 0 for new flood event, limiting
!       BP to a maximum of BP1.  When BP first falls below
!       BP1, IBP is set to 1 and BP is no longer limited until
!       the next flood event.
      IF (BP .LT. BP1) THEN
         IBP = 1
      ENDIF
      IF (IBP .EQ. 0) THEN
         BP = AMIN1(BP,BP1)
      ENDIF

      RETURN
      END SUBROUTINE AMTHERM

C=======================================================================

