C=======================================================================
C  INSOIL, Subroutine
C
C  Determines soil initialization
C-----------------------------------------------------------------------
C  Revision history
C
C  01/01/1993 GH  Written
C  05/28/1996 PWW Header revision and minor changes
C  04/01/1996 GH  Add residue/initial conditions
C  07/16/2002 CHP Increased number of applications to 200 (NAPPL)
C
C-----------------------------------------------------------------------
C  INPUT  : ISWWAT,ISWNIT,AINO3,ANO3,AINH4,ANH4,TNMIN,SWINIT,TSWINI,NLAYR,
C           DUL,LL,ESW,DLAYR,SAT,SW,TLL,TDUL,TSAT,TPESW,CUMDEP,PESW,TSW,BD,
C           INO3,INH4,TSOC,OC,PH
C
C  LOCAL  :
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : INPUT
C
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE INSOIL (ISWWAT,ISWNIT,AINO3,ANO3,AINH4,ANH4,TNMIN,
     &        SWINIT,TSWINI,NLAYR,DUL,LL,ESW,DLAYR,SAT,SW,TLL,TDUL,
     &        TSAT,TPESW,CUMDEP,PESW,TSW,BD,INO3,INH4,TSOC,OC,PH,
!    &        RESN,RESP,RESIDUE,RINP,DEPRES,ICRES,ICREN,ICREP,ICRIP,
!    &        ICRID,NARES,YRSIM,RESAMT,RESDAY,SLTX,SLTXS,TOTN)
!     2019-02-21 CHP got rid of unused variables
     &        SLTX,SLTXS,TOTN)

      USE ModuleDefs
      IMPLICIT  NONE

      CHARACTER*1 ISWWAT,ISWNIT
      CHARACTER*5 SLTX,SLTXS

      INTEGER   L,NLAYR   !,NARES,YRSIM,RESDAY(NAPPL)
      INTEGER   I,J,K

      REAL      AINO3,AINH4,TNMIN,ANO3,ANH4
      REAL      TSWINI,SWINIT(NL),OC(NL),PH(NL),TOTN(NL)
      REAL      DUL(NL),LL(NL),ESW(NL),DLAYR(NL),SAT(NL),SW(NL)
      REAL      TLL,TDUL,TSAT,TPESW,CUMDEP,PESW,TSW,TSOC,HUM(NL)
      REAL      KG2PPM(NL),BD(NL),SNO3(NL),SNH4(NL),INO3(NL),INH4(NL)
!      REAL      RESN(NAPPL),RESP(NAPPL),RESIDUE(NAPPL),RINP(NAPPL)
!      REAL      DEPRES(NAPPL)
!      REAL      ICRES,ICREN,ICREP,ICRIP,ICRID,RESAMT


      PESW   = 0.0
      CUMDEP = 0.0
      TSW    = 0.0
      TSWINI = 0.0
      TPESW  = 0.0
      TDUL   = 0.0
      TLL    = 0.0
      TSAT   = 0.0
      AINO3  = 0.0
      AINH4  = 0.0
      ANO3   = 0.0
      ANH4   = 0.0
      CUMDEP = 0.0
      TNMIN  = 0.0
      TSOC   = 0.0

      IF (ISWWAT .NE. 'Y') THEN
       ! SWINIT = 0.0
!       JZW changed due to nwheats_germn in NWheat request 
        SWINIT = DUL 
        INH4 = 0.0
        INO3 = 0.0
        RETURN
      ENDIF

      DO L = 1, NLAYR
         IF (SWINIT(L) .LE. 0) THEN
            SWINIT(L) = DUL(L)
         ENDIF
         ESW(L) = DUL(L) - LL(L)
         SW(L)  = SWINIT(L)
         CUMDEP = CUMDEP + DLAYR(L)
         TSWINI = TSWINI + SWINIT(L)*DLAYR(L)
         TPESW  = TPESW  + ESW(L)   *DLAYR(L)
         TLL    = TLL    + LL(L)    *DLAYR(L)
         TDUL   = TDUL   + DUL(L)   *DLAYR(L)
         TSAT   = TSAT   + SAT(L)   *DLAYR(L)
         IF (BD(L) .LE. 0.0) THEN
             BD(L) = 0.0
         ENDIF
         IF (PH(L) .LE. 0.0) THEN
             PH(L) = 0.0
         ENDIF
         IF (OC(L) .LE. 0.0) THEN
             OC(L) = -99.0
         ENDIF
         IF (TOTN(L) .LT. -9.0) THEN
             TOTN(L) = -9.0                   ! Keeps from format error
         ENDIF
         IF (INO3(L) .LE. 0.0) THEN
             INO3(L) = 0.0
         ENDIF
         IF (INH4(L) .LE. 0.0) THEN
             INH4(L) = 0.0
         ENDIF
         IF (ISWNIT .EQ. 'Y') THEN
            IF (BD(L) .LE. 0.0) THEN
                BD(L) = 1.2
            ENDIF
            IF (PH(L) .LE. 0.0) THEN
                PH(L) = 7.0
            ENDIF
            KG2PPM(L) = 1.0/(BD(L) * 0.1 * DLAYR(L))
            SNO3(L) = INO3(L) / KG2PPM(L)
            SNH4(L) = INH4(L) / KG2PPM(L)
            AINO3   = AINO3   + SNO3(L)
            AINH4   = AINH4   + SNH4(L)
            IF (OC(L) > 1.E-6) THEN
              HUM(L)  = OC(L)*1000.0 * BD(L)*DLAYR(L)
            ELSE
              HUM(L) = 0.0
            ENDIF
            TSOC    = TSOC    + HUM(L)
         ENDIF
      END DO

      TSW  = TSWINI
      PESW = MAX (0.0,TSW - TLL)

!-----------------------------------------------------------------------
!     chp 11/24/2009 - remove this section.  Adding applied residue to
!     initial conditions removes users selection of  residue type.
!     First application may be manure or something besides previous crop.
!     Keep first application as a residue applicaton, not initial conditions.
!C-----------------------------------------------------------------------
!C    Move residue information from residue variables to initial condtions
!C    if there is only one application.
!C    Fix to accomodate changes from DSSAT v3.0 to DSSAT v3.5.
!C-----------------------------------------------------------------------
!      IF (NARES .EQ. 1 .AND. RESDAY(1) .EQ. YRSIM .AND. ICRES .LE. 0.00)
!     & THEN
!         ICRES      = RESIDUE(1)
!         RESIDUE(1) = 0.0
!         ICREN      = RESN(1)
!         RESN(1)    = 0.0
!         ICREP      = RESP(1)
!         RESP(1)    = 0.0
!         ICRIP      = RINP(1)
!         RINP(1)    = 0.0
!         ICRID      = DEPRES(1)
!         DEPRES(1)  = 0.0
!         RESAMT     = RESAMT - ICRES
!         NARES      = 0
!      ENDIF
C-----------------------------------------------------------------------

      J = 0
      K = 0
      DO L = 1, 4
         I = INDEX(SLTX(L:L+1),' ')
         IF ((I .EQ. 0) .AND. (J .EQ. 0)) THEN
           SLTX = SLTX(L:5)
           J = L
         ENDIF
         I = INDEX(SLTXS(L:L+1),' ')
         IF ((I .EQ. 0) .AND. (K .EQ. 0)) THEN
           SLTXS = SLTXS(L:5)
           K = L
         ENDIF
      END DO

!     CHP removed at LAH's request 7/28/2006
!      IF (SLTX(1:2) .EQ. '-9') THEN
!         SLTX(1:5) = '     '
!      ENDIF
!      IF (SLTXS(1:2) .EQ. '-9') THEN
!         SLTXS(1:5) = '     '
!      ENDIF
!
!      IF (SLTX(1:1) .EQ. ' ' .AND. SLTXS(1:1) .NE. ' ') THEN
!         SLTX(1:5) = SLTXS
!      ELSE IF (SLTXS(1:1) .EQ. ' ' .AND. SLTX(1:1) .NE. ' ') THEN
!         SLTXS(1:5) = SLTX
!      ENDIF

!     Replace with:
      IF ((SLTX(1:2) .EQ. '-9' .OR. SLTX(1:1) .EQ. ' ')
     &     .AND. SLTXS(1:1) .NE. ' ') THEN
         SLTX(1:5) = SLTXS
      ELSEIF (SLTXS(1:1) .EQ. ' ' .AND.
     &    (SLTX(1:1) .NE. ' ' .AND. SLTX(1:2) .NE. '-9')) THEN
         SLTXS(1:5) = SLTX
      ENDIF

      RETURN
      END SUBROUTINE INSOIL
