C=======================================================================
C  IPSLIN, Subroutine
C
C  Reads soil initial conditions from FILEX file
C  Read initial values of rapidly changing soil variables,
C  and convert values to standard soil depths
C-----------------------------------------------------------------------
C  Revision history
C
C  06/21/1991 JWJ Written
C  05/28/1993 PWW Header revision and minor changes
C  12/01/1993 WTB Modifed to read soil test P
C  08/19/2002 GH  Modified for Y2K
C  02/07/2007 GH  Add path to FileX
C  05/07/2020 FO  Added new Y4K subroutine call to convert YRDOY
C-----------------------------------------------------------------------
C  INPUT  : FILEX,LNIC,NLAYR,DUL,SWINIT,PEDON,SLNO
C
C  LOCAL  : LN,NLAYRI,NLAYR,DS,DLAYRI,LINEXP,ISECT,CHARTEST,LUNEXP
C
C  OUTPUT : YRIC,PRCROP,WRESR,WRESND,EFINOC,EFNFIX,INO3,INH4,SWINIT
C-----------------------------------------------------------------------
C  Called : SESOIL INPUT
C
C  Calls  : FIND IGNORE ERROR
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE IPSLIN (FILEX,FILEX_P,LNIC,NLAYR,DUL,YRIC,PRCROP,WRESR,
     &        WRESND,EFINOC,EFNFIX,PEDON,SLNO,DS,SWINIT,INH4,INO3,
     &        ISWITCH,ICWD,ICRES,ICREN,ICREP,ICRIP,ICRID) !,YRSIM) 
!     &        SOM1I, SOM2I, SOM3I)

!     2023-01-26 chp removed unused variables in argument list:
!       YRSIM

      USE ModuleDefs
      IMPLICIT     NONE
      EXTERNAL ERROR, FIND, IGNORE, Y4K_DOY, LMATCH

      CHARACTER*2  PRCROP
      CHARACTER*6  ERRKEY,FINDCH
      CHARACTER*10 PEDON,SLNO
      CHARACTER*12 FILEX
      CHARACTER*80 CHARTEST
	CHARACTER*92 FILEX_P

      INTEGER      L,LN,LUNEXP,NLAYRI,NLAYR,LINEXP,ISECT,LNIC, !,FWY1P,
     &             YRIC,ERRNUM,IFIND   !,YRSIM, YRICYEAR, YR, DOY
      REAL         DS(NL),DLAYRI(NL),SWINIT(NL)
      REAL         DUL(NL),WRESR,WRESND,EFINOC,EFNFIX,INO3(NL),INH4(NL)
      REAL         ICWD,ICRES,ICREN,ICREP,ICRIP,ICRID !, TOTSOM
!      REAL, DIMENSION(NL) :: SOM1I, SOM2I, SOM3I

      TYPE (SwitchType) ISWITCH

      PARAMETER   (LUNEXP = 16)
      PARAMETER   (ERRKEY = 'IPSLIN')

      FINDCH = '*INITI'
C
C     Set default initial conditions in case they are missing
C
      YRIC = 0
      PRCROP = '  '
      WRESR  = 0.0
C-GH  WRESR  = 1.0
      WRESND = 0.0
      EFINOC = 1.0
      EFNFIX = 1.0
      ICWD   = -99
      ICRES  = 0.0
      ICREN  = 0.0
      ICREP  = 0.0
      ICRIP  = 100.0
      ICRID  = 0.0
C-GH  ICRID  = 15.0
C
      SWINIT = -99.
      INO3   = -99.
      INH4   = -99.

      IF (PEDON .NE. SLNO) RETURN
      IF (LNIC  .LE. 0)    RETURN

      OPEN (LUNEXP,FILE = FILEX_P,STATUS = 'OLD',IOSTAT=ERRNUM)
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,0)

      CALL FIND (LUNEXP,FINDCH,LINEXP,IFIND)
      IF (IFIND .EQ. 0) CALL ERROR (ERRKEY,1,FILEX,LINEXP)
 50   CONTINUE
      CALL IGNORE (LUNEXP,LINEXP,ISECT,CHARTEST)

      IF (ISECT .EQ. 1) THEN
          READ (CHARTEST,55,IOSTAT=ERRNUM) LN
          IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
          IF (LN .NE. LNIC) GO TO 50
          READ (CHARTEST,55,IOSTAT=ERRNUM) LN,PRCROP,YRIC,WRESR,
     &       WRESND,EFINOC,EFNFIX,ICWD,ICRES,ICREN,ICREP,ICRIP,ICRID

          IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
       ELSE
         CALL ERROR (ERRKEY,2,FILEX,LINEXP)
      ENDIF
      
C  FO - 05/07/2020 Add new Y4K subroutine call to convert YRDOY
      !CALL  Y2K_DOY (YRIC)
      CALL  Y4K_DOY (YRIC,FILEX,LINEXP,ERRKEY,3)
!      IF (YRIC .LT. YRSIM) THEN
!        !FO - Initial Condition raise an error if is lower than YRSIM.
!        CALL ERROR (ERRKEY,3,FILEX,LINEXP)
!      ENDIF
      
      IF (ISWITCH%ISWNIT .EQ. 'Y') THEN
         WRESR = MAX(WRESR,0.0)
C-GH     IF (WRESR  .LT. 1.0) WRESR  = 1.0
         IF (WRESND .LT. 0.0) WRESND = 0.0
         IF (EFINOC .LT. 0.0) EFINOC = 1.0
         IF (EFNFIX .LT. 0.0) EFNFIX = 1.0
!        ICWD  = MAX(ICWD,0.0)
C-PW     ICRES = MAX(ICRES,10.0)
         ICRES = MAX(ICRES,0.0)
         ICREN = MAX(ICREN,0.0)
         ICREP = MAX(ICREP,0.0)
         ICRID = MAX(ICRID,0.0)
C-GH     ICRID = MAX(ICRID,15.0)
         IF (ICRIP  .LT. 0.0) ICRIP  = 100.0
      ENDIF

      NLAYRI = 1
C
C     Read layer information for the correct IC level number
C
 70   CONTINUE
      CALL IGNORE (LUNEXP,LINEXP,ISECT,CHARTEST)

      IF (ISECT .EQ. 1) THEN
         READ (CHARTEST,60,IOSTAT=ERRNUM) LN
         IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
         IF (LN .NE. LNIC) GO TO 70
         READ (CHARTEST,60,IOSTAT=ERRNUM) LN,DLAYRI(NLAYRI),
     &             SWINIT(NLAYRI),INH4(NLAYRI),INO3(NLAYRI)
         IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)

         NLAYRI = NLAYRI + 1
         GO TO 70
      ENDIF

      CLOSE (LUNEXP)
      NLAYRI = NLAYRI - 1

!     Match layers first!
      CALL LMATCH (NLAYRI,DLAYRI,SWINIT,NLAYR,DS)
      CALL LMATCH (NLAYRI,DLAYRI,INH4,  NLAYR,DS)
      CALL LMATCH (NLAYRI,DLAYRI,INO3,  NLAYR,DS)

!     Then check for bounds
      DO L = 1, NLAYR
        IF (ISWITCH%ISWWAT .NE. 'N') THEN
           IF (SWINIT(L) .GT. 0.75) THEN
              CALL ERROR (ERRKEY,10,FILEX,LINEXP)
           ENDIF
           IF (SWINIT(L) .LT. 0.00) THEN
               SWINIT(L) = DUL(L)
           ENDIF
        ENDIF

        IF (ISWITCH%ISWNIT .EQ. 'Y') THEN
           IF (INH4(L) .LT.   0.0) THEN
              INH4(L) = 0.01
           ENDIF
           IF (INH4(L) .GT. 100.0) THEN
              CALL ERROR (ERRKEY,11,FILEX,LINEXP)
           ENDIF
           IF (INO3(L) .LT.   0.0) THEN
              INO3(L) = 0.01
           ENDIF
           IF (INO3(L) .GT. 100.0) THEN
              CALL ERROR (ERRKEY,12,FILEX,LINEXP)
           ENDIF
        ENDIF
      ENDDO


!      CALL LMATCH (NLAYRI,DLAYRI,SOM1I, NLAYR,DS)
!      CALL LMATCH (NLAYRI,DLAYRI,SOM2I, NLAYR,DS)
!      CALL LMATCH (NLAYRI,DLAYRI,SOM3I, NLAYR,DS)

      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

 55   FORMAT (I3,3X,A2,1X,I5,10(1X,F5.0))
 60   FORMAT (I3,F5.0,3(1X,F5.0))
 65   FORMAT (26X,3F6.0)

      END SUBROUTINE IPSLIN

C=======================================================================
C  IPSLAN, Subroutine
C
C  Reads soil analysis data from FILEX file
C  Read initial values of soil variables, such as fertility,
C  that change at a medium rate, adjust soil depths to standard
C  Created 01-JUL-91, J. Jones to read new IBSNAT formats
C-----------------------------------------------------------------------
C  Revision history
C
C 07/01/1991 JWJ Written
C 11/18/1992 WDB Modifed to read soil test P
C 05/28/1993 PWW Header revision and minor changes
! 03/31/2005 CHP Return method of P extraction (SMPX) if any P values
!                (SAPX) are read.
C 02/07/2007 GH  Add path to File_X
C 05/07/2020 FO  Add new Y4K subroutine call to convert YRDOY
C-----------------------------------------------------------------------
C  INPUT  : FILEX,LNSA,PEDON,SLNO,BD,OC,PH
C
C  LOCAL  : LUNEXP,LN,NLAYRI,NLAYR,DS,SABL,LINEXP,ISECT,CHARTEST,
C           SADM,SAOC,SANI,SAPHW,SAPHB,SAPX,SAKE,
C           SADAT,SMDM,SMOC,SMNI,SMHW,SMHB,SMPX,SMKE
C
C  OUTPUT : BD,OC,PH,EXTP
C-----------------------------------------------------------------------
C  Called : SESOIL INPUT
C
C  Calls  : FIND IGNORE ERROR
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE IPSLAN (FILEX, FILEX_P,LNSA, BD, DS, EXK, EXTP, OC,
     &            PEDON, PH, PHKCL, SLNO, SMHB, SMKE, SMPX, TOTN, 
     &            SASC, SAEA, NLAYR)    !, YRSIM)

      USE ModuleDefs
      IMPLICIT     NONE
      EXTERNAL ERROR, FIND, IGNORE, Y4K_DOY, LMATCH

      CHARACTER*5  SMHB, SMKE, SMPX, SMHBtemp, SMKEtemp, SMPXtemp
      CHARACTER*6  ERRKEY, FINDCH
      CHARACTER*10 SLNO, PEDON
      CHARACTER*12 FILEX
      CHARACTER*80 CHARTEST*80
	CHARACTER*92 FILEX_P

      INTEGER      LN,LUNEXP,NLAYRI,NLAYR,LINEXP,ISECT,LNSA
      INTEGER      ERRNUM,SADAT,IFIND,L   !,YRSIM

      REAL         SABL(NL),SADM(NL),SAOC(NL),SANI(NL),SAPHW(NL)
      REAL         SAPX(NL),SAKE(NL),BD(NL),OC(NL),DS(NL),SAPHB(NL)
      REAL         PH(NL),PHKCL(NL),EXK(NL), EXTP(NL),TOTN(NL), SASC(NL)

!     chp 2023-01-24 for methane model initialization
      REAL         SAEA(NL) !Soil Alternative Electron Acceptors 

      PARAMETER   (LUNEXP = 16)
      PARAMETER   (ERRKEY = 'IPSLAN')

                   FINDCH = '*SOIL '

      IF (LNSA  .LE. 0)    RETURN
      IF (PEDON .NE. SLNO) RETURN

      SADM = -99.
      SAOC = -99.
      SANI = -99.
      SAPHW = -99.
      SAPHB = -99.
      SAPX = -99.
      SASC = -99.
      SAEA = -99.

      OPEN (LUNEXP,FILE = FILEX_P,STATUS = 'OLD',IOSTAT=ERRNUM)
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,0)

      CALL FIND (LUNEXP,FINDCH,LINEXP,IFIND)
      IF (IFIND .EQ. 0) CALL ERROR (ERRKEY,1,FILEX,LINEXP)

 50   CONTINUE
      CALL IGNORE (LUNEXP,LINEXP,ISECT,CHARTEST)
      IF (ISECT .EQ. 1) THEN
         READ (CHARTEST,55,IOSTAT=ERRNUM) LN
         IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
         IF (LN .NE. LNSA) GO TO 50

!chp 3/31/05 READ (CHARTEST,55,IOSTAT=ERRNUM) LN,SADAT,SMHB,SMPX,SMKE
         READ (CHARTEST,55,IOSTAT=ERRNUM) LN, SADAT, 
     &        SMHBtemp, SMPXtemp, SMKEtemp
         IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
       ELSE
         CALL ERROR (ERRKEY,2,FILEX,LINEXP)
      ENDIF
      
C  FO - 05/07/2020 Add new Y4K subroutine call to convert YRDOY
      !CALL Y2K_DOY (SADAT)
      CALL Y4K_DOY (SADAT,FILEX,LINEXP,ERRKEY,3)
      
C
C     Read layer information for the correct soil analysis number
C
      NLAYRI = 1
 70   CONTINUE
      CALL IGNORE (LUNEXP,LINEXP,ISECT,CHARTEST)

      IF (ISECT .EQ. 1) THEN
         READ (CHARTEST,60,IOSTAT=ERRNUM) LN
         IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
         IF (LN .NE. LNSA) GO TO 70

         READ (CHARTEST,60,IOSTAT=ERRNUM) LN,SABL(NLAYRI),
     &         SADM (NLAYRI),SAOC(NLAYRI),SANI(NLAYRI),SAPHW(NLAYRI),
     &         SAPHB(NLAYRI),SAPX(NLAYRI),SAKE(NLAYRI)
         IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)

!        chp added 4/21/2008 - stable organic C in %
         READ (CHARTEST,'(51X,F6.0)',IOSTAT=ERRNUM) SASC(NLAYRI) 
         IF (ERRNUM .NE. 0) SASC(NLAYRI) = -99.

!        chp added 2023-01-24 - soil alternative electron acceptors (mol Ceq/m3)
         READ (CHARTEST,'(57X,F6.0)',IOSTAT=ERRNUM) SAEA(NLAYRI) 
         IF (ERRNUM .NE. 0) SAEA(NLAYRI) = -99.

         IF (SADM(NLAYRI) .GT. 10.0) THEN
            CALL ERROR (ERRKEY,10,FILEX,LINEXP)
         ENDIF
         IF (SAOC(NLAYRI) .GT. 100.0) THEN
            CALL ERROR (ERRKEY,11,FILEX,LINEXP)
         ENDIF
         IF (SANI(NLAYRI) .GT. 10.0) THEN
            CALL ERROR (ERRKEY,12,FILEX,LINEXP)
         ENDIF
         NLAYRI = NLAYRI + 1
         GO TO 70
      ENDIF

      CLOSE (LUNEXP)
      NLAYRI = NLAYRI - 1

C     Use soil analysis values for NLAYR layers for which data were
C          in the experimental field; for other layers the values from
C          the soil profile will be retained and used. Only replace
C          those values if data were available (i.e., not < 0.)
      IF (SADM(1) .GT.  0.0 .AND. NLAYRI > 0) THEN
        CALL LMATCH (NLAYRI, SABL, SADM,  NLAYR, DS)
        DO L = 1, NLAYR
          IF (SADM(L) .GT. 0.0) BD(L) = SADM(L)
        ENDDO
      ENDIF

      IF (SAOC(1) .GT.  -1.E-6 .AND. NLAYRI > 0) THEN
        CALL LMATCH (NLAYRI, SABL, SAOC,  NLAYR, DS)
        DO L = 1, NLAYR
!         OC in %, SAOC in % (despite what v3.5-Vol2 says)
          IF (SAOC(L) .GT. -1.E-6) OC(L) = SAOC(L)
        ENDDO
      ENDIF

      IF (SANI(1) .GT.  -1.E-6 .AND. NLAYRI > 0) THEN
        CALL LMATCH (NLAYRI, SABL, SANI,  NLAYR, DS)
        DO L = 1, NLAYR
!         TOTN in %, SANI in % (despite what v3.5-Vol2 says)
          IF (SANI(L) .GT. -1.E-6) TOTN(L) = SANI(L)
        ENDDO
      ENDIF

      IF (SAPHW(1) .GT.  0.0 .AND. NLAYRI > 0) THEN
        CALL LMATCH (NLAYRI, SABL, SAPHW, NLAYR, DS)
        DO L = 1, NLAYR
          IF (SAPHW(L).GT. 0.0) PH(L) = SAPHW(L)
        ENDDO
      ENDIF

      IF (SAPHB(1) .GT.  0.0 .AND. NLAYRI > 0) THEN
        CALL LMATCH (NLAYRI, SABL, SAPHB, NLAYR, DS)
        SMHB = SMHBtemp
        DO L = 1, NLAYR
          IF (SAPHB(L).GT. 0.0) PHKCL(L) = SAPHB(L)
        ENDDO
      ENDIF

      IF (SAPX(1) .GT.  -1.E-6 .AND. NLAYRI > 0) THEN
        CALL LMATCH (NLAYRI, SABL, SAPX,  NLAYR, DS)
        SMPX = SMPXtemp    !CHP 3/31/2005
        DO L = 1, NLAYR
          IF (SAPX(L) .GT. -1.E-6) THEN
            EXTP(L) = SAPX(L)
          ENDIF
        ENDDO
      ENDIF

      IF (SAKE(1) .GT.  -1.E-6 .AND. NLAYRI > 0) THEN
        CALL LMATCH (NLAYRI, SABL, SAKE,  NLAYR, DS)
        SMKE = SMKEtemp    !CHP 3/31/2005
        DO L = 1, NLAYR
          IF (SAKE(L) .GT. -1.E-6) EXK(L) = SAKE(L)
        ENDDO
      ENDIF

!     Stable organic C has no counterpart in the soil profile data
      IF (SASC(1) .GT.  -1.E-6 .AND. NLAYRI > 0) THEN
        CALL LMATCH (NLAYRI, SABL, SASC,  NLAYR, DS)
      ENDIF

!     SAEA has no counterpart in the soil profile data
      IF (SAEA(1) .GT.  -1.E-6 .AND. NLAYRI > 0) THEN
        CALL LMATCH (NLAYRI, SABL, SAEA,  NLAYR, DS)
      ENDIF

      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

 55   FORMAT (I3,I5,3(1X,A5))
 60   FORMAT (I3,F5.0,8(1X,F5.0))

      END SUBROUTINE IPSLAN

!
