C=======================================================================
C  SESOIL, Subroutine
C
C  Determines soil sensitivity analysis options
C-----------------------------------------------------------------------
C  Revision history
C
C  01/01/1990 GH  Written
C  05/28/1993 PWW Header revision and minor changes
!  03/31/2005 CHP Added SMPX to call to IPSLAN
C-----------------------------------------------------------------------
C  INPUT  : FILES,FILEX,NSENS,RNMODE,SLNF,SLPF,U SWCON,CN2,SALB,DEPMAX,
C           SLDESC,PEDON,SLNO,SLTX,LL,DUL,SAT,SHF,SWCN,BD,OC,PH,DLAYR,
C           NLAYR,DS,LNIC,LNSA,YRIC,PRCROP,WRESR,WRESND,EFINOC,EFNFIX,
C           PATHSL,SWINIT,INO3,INH4,ISWWAT
C
C  LOCAL  :
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : SENS
C
C  Calls  : CLEAR IPSOIL_Inp SEINIT CLEAR SEDLYR IPSLIN IPSLAN SEPROF SESURF
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE SESOIL (FILES,FILEX,FILEX_P,NSENS,RNMODE,SLNF,SLPF,U,
     &     SWCON,CN2,SALB,DEPMAX,PEDON,SLNO,SLTX,LL,DUL,SAT,WR,BD,OC,PH,
     &     DLAYR,NLAYR,DS,LNIC,LNSA,YRIC,PRCROP,WRESR,WRESND,EFINOC,
     &     EFNFIX,PATHSL,SWINIT,INO3,INH4,EXTP,
     &     ICWD,ICRES,ICREN,ICREP,ICRIP,ICRID,SWCN,ADCOEF,TOTN,  !YRSIM,
     &     SMPX, EXK, PHKCL, SMHB, SMKE, ISWITCH)

      USE ModuleDefs
      IMPLICIT     NONE
      EXTERNAL CLEAR, ERROR, IPSLAN, IPSLIN, IPSOIL_INP, SEDLYR, 
     &  SEINIT, SEPROF, SESURF

      CHARACTER*1  RNMODE,BLANK,SWSPRF   !,ISWPHO
      CHARACTER*2  PRCROP
      CHARACTER*5  SLTX, SMPX, SMHB, SMKE
      CHARACTER*6  ERRKEY
      CHARACTER*10 PEDON,SLNO
      CHARACTER*12 FILES,FILEX
      CHARACTER*80 PATHSL
      CHARACTER*92 FILESS,FILEX_P

      INTEGER      MENU,NSENS,NLAYR,NLOOP,LNIC,LNSA,YRIC,PATHL,I !,YRSIM
      LOGICAL      FEXIST

      REAL         LL(NL),DUL(NL),SAT(NL),BD(NL),WR(NL),PH(NL)
      REAL         OC(NL),SLPF,U,SLNF,SWCON,CN2,SALB,DEPMAX,DLAYR(NL)
      REAL         DS(NL),WRESR,WRESND,EFINOC,EFNFIX,ADCOEF(NL),TOTN(NL)
      REAL         SWINIT(NL),INO3(NL),INH4(NL),EXTP(NL),SWCN(NL)
      REAL         EXK(NL), PHKCL(NL), SASC(NL), SAEA(NL)
      REAL         ICWD,ICRES,ICREN,ICREP,ICRIP,ICRID
!      REAL, DIMENSION(NL) :: SOM1I, SOM2I, SOM3I

      TYPE (SwitchType) ISWITCH

      PARAMETER (ERRKEY = 'SESOIL')
      PARAMETER (BLANK  = ' ')

      NLOOP  = 0
      FEXIST = .TRUE.

  100 CONTINUE
      NLOOP = NLOOP + 1
      IF (NLOOP .GT. 25) CALL ERROR (ERRKEY,1,' ',0)

      IF (INDEX('IE',RNMODE) .GT. 0)  THEN
         CALL CLEAR
         WRITE (*, 200) PEDON,SLTX,FILES,PATHSL
         IF (.NOT. FEXIST) THEN
            WRITE (*,250) FILES
          ELSE
            IF (FILES .NE. 'SOIL.SOL') WRITE (*,260) FILES
         ENDIF
         WRITE (*,290)
      ENDIF
C
C     Get menu choice
C
      READ (5,'(I2)',ERR = 100) MENU
C
C     Branch to menu choice
C
      IF (MENU .EQ. 0) THEN
          RETURN
      ELSE IF (MENU .EQ. 1) THEN
          CALL IPSOIL_Inp (RNMODE,FILES,PATHSL,NSENS,ISWITCH)
          SWINIT(1) = DUL(1)
          DO I = 2, NLAYR
             SWINIT(I) = DUL(I)
             IF (INO3(I) .EQ. 0.0) THEN
                 INO3(I) = INO3(I-1)
             ENDIF
             IF (INH4(I) .EQ. 0.0) THEN
                 INH4(I) = INH4(I-1)
             ENDIF
          END DO
          SWSPRF = 'Y'
          CALL SEINIT (RNMODE,NLAYR,SWINIT,PRCROP,WRESR,WRESND,DLAYR,
     &                 SLPF,DS,SWSPRF,INO3,INH4,ICWD,ICRES,ICREN,
     &                   ICREP,ICRIP,ICRID)
          SWSPRF = 'N'
      ELSE IF (MENU .EQ. 2) THEN
          IF (FILES(1:4) .EQ. 'SOIL') THEN
             WRITE (FILES(1:8),'(A2,A6)') SLNO(1:2),'.SOL  '
           ELSE IF (FILES(1:2) .EQ. SLNO(1:2)) THEN
             WRITE (FILES(1:8),'(A8)') 'SOIL.SOL'
          ENDIF
          PATHL  = INDEX (PATHSL,BLANK)
          IF (PATHL .LE. 1) THEN
             FILESS = FILES
           ELSE
             FILESS = PATHSL (1:(PATHL-1)) // FILES
          ENDIF
          INQUIRE (FILE = FILESS, EXIST = FEXIST)
          IF (FEXIST) THEN
             NSENS = 0
             CALL IPSOIL_Inp(RNMODE,FILES,PATHSL,NSENS,ISWITCH)
             NSENS = 1
           ELSE
             PEDON = '-9'
             SLTX  = '-9'
          ENDIF
      ELSE IF (MENU .EQ. 3) THEN
          IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,800) PATHSL
          READ (5,'(A80)') PATHSL
          PATHL  = INDEX (PATHSL,BLANK)
          IF (PATHL .LE. 1) THEN
             FILESS = FILES
           ELSE
             IF (PATHL .GT. 1) THEN
                IF (PATHSL((PATHL-1):(PATHL-1)) .NE. SLASH) THEN
                   WRITE (PATHSL(PATHL:PATHL),'(A1)') SLASH
                   PATHL = PATHL+1
                ENDIF
             ENDIF
             FILESS = PATHSL (1:(PATHL-1)) // FILES
          ENDIF
          INQUIRE (FILE = FILESS, EXIST = FEXIST)
          IF (FEXIST) THEN
             NSENS = 0
             CALL IPSOIL_Inp (RNMODE,FILES,PATHSL,NSENS,ISWITCH)
             NSENS = 1
           ELSE
             PEDON = '-9'
             SLTX  = '-9'
          ENDIF
      ELSE IF (MENU .EQ. 4) THEN
          CALL SEDLYR (DLAYR,NLAYR,RNMODE,DS,DEPMAX)
          NSENS = 0
          CALL IPSOIL_Inp (RNMODE,FILES,PATHSL,NSENS,ISWITCH)
          NSENS = 1
          CALL IPSLIN (FILEX,FILEX_P,LNIC,NLAYR,DUL,YRIC,PRCROP,WRESR,
     &	     WRESND,EFINOC,EFNFIX,PEDON,SLNO,DS,SWINIT,INH4,INO3,
     &         ISWITCH,ICWD,ICRES,ICREN,ICREP,ICRIP,ICRID)    !,YRSIM)
!     &         SOM1I, SOM2I, SOM3I)
          CALL IPSLAN (FILEX, FILEX_P,LNSA, BD, DS, EXK, EXTP, OC,
     &            PEDON, PH, PHKCL, SLNO, SMHB, SMKE, SMPX, TOTN, 
     &            SASC, SAEA, NLAYR)    !, YRSIM)
      ELSE IF (MENU .EQ. 5) THEN
          CALL SEPROF (DLAYR,NLAYR,RNMODE,DS,DUL,LL,SAT,BD,WR,OC,PH,
     &         SWINIT,SWCN,ADCOEF,TOTN)
      ELSE IF (MENU .EQ. 6) THEN
          CALL SESURF (RNMODE,U,CN2,SWCON,SALB,SLNF,SLPF)
      ENDIF

      GO TO 100

C-----------------------------------------------------------------------
C     Format strings
C-----------------------------------------------------------------------

  200 FORMAT(//,9X,'SOIL PROFILE SELECTION AND MODIFICATION',/,
     &          9X,'=======================================',//,
     & 5X,' 0. Return to Main Menu ',//,
     & 5X,' 1. Soil Profile Selection ............[ ',A10,' - ',A5,/,
     & 5X,' 2. Soil Profile File .................[ ',A12,/,
     & 5X,' 3. Soil Profile Path .................[ ',A30,/,
     & 5X,' 4. Soil Profile Layer Thickness ......| ',/,
     & 5X,' 5. Soil Profile Parameters ...........| ',/,
     & 5X,' 6. Soil Surface Parameters ...........| ',/)
  250 FORMAT (/,9X,'File ',A12,' does not exist.',/,
     &          9X,'Please select another file.')
  260 FORMAT (9X,'The NON-STANDARD Soil File ',A12,' is selected.',/,
     &        9X,'You might have to select another soil profile ',
     &           '[Option 1].')
  290 FORMAT (//,9X,'SELECTION ? [ Default = 0 ] ===> ',$)
  800 FORMAT (//,9X,'Please enter a soil path name using the following',
     &         /,9X,'format :       [ ',A50,' ] ',
     &         /,9X,'SELECTION ? ===> ',$)

      END SUBROUTINE SESOIL

C=======================================================================
C  SEPROF, Subroutine
C
C  Determines soil profile modification
C-----------------------------------------------------------------------
C  Revision history
C
C  01/01/1991 GH  Written
C  05/28/1993 PWW Header revision and minor changes
C-----------------------------------------------------------------------
C  INPUT  : DLAYR,NLAYR,RNMODE,DS,DUL,LL,SAT,BD,SHF,OC,PH,SWINIT
C
C  LOCAL  : ERRKEY,DESUN,DESMOD,NLOOP,MENU
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : SESOIL
C
C  Calls  : ERROR CLEAR SEPLYR
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE SEPROF (DLAYR,NLAYR,RNMODE,DS,DUL,LL,SAT,BD,WR,OC,PH,
     &                   SWINIT,SWCN,ADCOEF,TOTN)

      USE ModuleDefs
      IMPLICIT     NONE
      EXTERNAL CLEAR, ERROR, SEPLYR

      CHARACTER*1  RNMODE
      CHARACTER*6  ERRKEY
      CHARACTER*10 DESUN
      CHARACTER*30 DESMOD

      INTEGER      NLOOP,MENU,NLAYR
      REAL         DLAYR(NL),DS(NL),DUL(NL),LL(NL),SAT(NL),ADCOEF(NL)
      REAL         BD(NL),WR(NL),OC(NL),PH(NL),SWINIT(NL),SWCN(NL)
      REAL         TOTN(NL)

      PARAMETER (ERRKEY = 'SEPROF')
      NLOOP  = 0

  100 CONTINUE
      NLOOP = NLOOP + 1
      IF (NLOOP .GT. 25) CALL ERROR (ERRKEY,1,' ',0)

      IF (INDEX('IE',RNMODE) .GT. 0)  THEN
         CALL CLEAR
         WRITE (*,200)
         WRITE (*,290)
      ENDIF
C
C     Get menu choice
C
      READ (5,'(I2)',ERR = 100) MENU
C
C     Branch to menu choice
C
      IF (MENU .EQ. 0) THEN
          RETURN
      ELSE IF (MENU .EQ. 1) THEN
          DESUN  = 'cm3/cm3   '
          DESMOD = 'DRAINED UPPER LIMIT          '
          CALL SEPLYR (DLAYR,NLAYR,RNMODE,DS,DUL,DESMOD,DESUN)
      ELSE IF (MENU .EQ. 2) THEN
          DESUN  = 'cm3/cm3   '
          DESMOD = 'LOWER LIMIT                  '
          CALL SEPLYR (DLAYR,NLAYR,RNMODE,DS,LL,DESMOD,DESUN)
      ELSE IF (MENU .EQ. 3) THEN
          DESUN  = 'cm3/cm3   '
          DESMOD = 'SATURATED WATER CONTENT      '
          CALL SEPLYR (DLAYR,NLAYR,RNMODE,DS,SAT,DESMOD,DESUN)
      ELSE IF (MENU .EQ. 4) THEN
          DESUN  = 'cm3/cm3   '
          DESMOD = 'INITIAL SOIL WATER CONTENT   '
          CALL SEPLYR (DLAYR,NLAYR,RNMODE,DS,SWINIT,DESMOD,DESUN)
      ELSE IF (MENU .EQ. 5) THEN
          DESUN  = 'gram/cm3  '
          DESMOD = 'BULK DENSITY                 '
          CALL SEPLYR (DLAYR,NLAYR,RNMODE,DS,BD,DESMOD,DESUN)
      ELSE IF (MENU .EQ. 6) THEN
          DESUN  = '          '
          DESMOD = 'pH                           '
          CALL SEPLYR (DLAYR,NLAYR,RNMODE,DS,PH,DESMOD,DESUN)
      ELSE IF (MENU .EQ. 7) THEN
          DESUN  = '%         '
          DESMOD = 'SOIL ORGANIC CARBON          '
          CALL SEPLYR (DLAYR,NLAYR,RNMODE,DS,OC,DESMOD,DESUN)
      ELSE IF (MENU .EQ. 8) THEN
          DESUN  = 'rel. [0-1]'
          DESMOD = 'ROOTING CHARACTERISTICS      '
          CALL SEPLYR (DLAYR,NLAYR,RNMODE,DS,WR,DESMOD,DESUN)
      ELSE IF (MENU .EQ. 9) THEN
          DESUN  = 'cm/hr     '
          DESMOD = 'SAT. HYDRAULIC CONDUCTIVITY  '
          CALL SEPLYR (DLAYR,NLAYR,RNMODE,DS,SWCN,DESMOD,DESUN)
      ELSE IF (MENU .EQ. 10) THEN
          DESUN  = '          '
          DESMOD = 'ADSORPTION COEFFICIENT       '
          CALL SEPLYR (DLAYR,NLAYR,RNMODE,DS,ADCOEF,DESMOD,DESUN)
      ELSE IF (MENU .EQ. 11) THEN
          DESUN  = '%         '
          DESMOD = 'TOTAL SOIL NITROGEN          '
          CALL SEPLYR (DLAYR,NLAYR,RNMODE,DS,TOTN,DESMOD,DESUN)
      ENDIF

      GO TO 100

C-----------------------------------------------------------------------
C     Format strings
C-----------------------------------------------------------------------

  200 FORMAT(//,9X,'SOIL PROFILE MODIFICATION',/,
     &          9X,'=========================',//,
     &      5X,' 0. Return to Main Menu ',//,
     &      5X,' 1. Drained Upper Limit ...............| ',/,
     &      5X,' 2. Lower Limit .......................| ',/,
     &      5X,' 3. Saturated Water Content ...........| ',/,
     &      5X,' 4. Initial Soil Water Content ........| ',/,
     &      5X,' 5. Bulk Density ......................| ',/,
     &      5X,' 6. pH ................................| ',/,
     &      5X,' 7. Soil Organic Carbon ...............| ',/,
     &      5X,' 8. Rooting Characteristics ...........| ',/,
     &      5X,' 9. Saturated Hydraulic Conductivity...| ',/,
     &      5X,'10. Adsorption Coefficient ............| ',/,
     &      5X,'11. Total Soil Nitrogen ...............| ',/)
  290 FORMAT (//,9X,'SELECTION ? [ Default = 0 ] ===> ',$)

      END SUBROUTINE SEPROF

C=======================================================================
C  SEDLYR, Subroutine
C
C  Determines soil profile modification
C-----------------------------------------------------------------------
C  Revision history
C
C  01/01/1991 GH  Written
C  05/28/1993 PWW Header revision and minor changes
C-----------------------------------------------------------------------
C  INPUT  : DLAYR,NLAYR,RNMODE,DS,DEPMAX
C
C  LOCAL  : LINE,ERRKEY,NLOOP,I,DL,SL,FLAG,DEPMAX
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : SESOIL
C
C  Calls  : ERROR CLEAR VERIFY
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE SEDLYR (DLAYR,NLAYR,RNMODE,DS,DEPMAX)

      USE ModuleDefs
      IMPLICIT    NONE
      EXTERNAL CLEAR, ERROR, VERIFY

      CHARACTER*1 RNMODE,LINE(80)
      CHARACTER*6 ERRKEY

      INTEGER     NLOOP,I,NLAYR
      REAL        DLAYR(NL),DL,SL,FLAG,DS(NL),DEPMAX

      PARAMETER (ERRKEY = 'SEDLYR')

      NLOOP    = 0
      DLAYR(1) = DS(1)
      DO I = 2, NL
         DLAYR(I) = DS(I) - DS(I-1)
      END DO

  100 CONTINUE
      NLOOP = NLOOP + 1
      IF (NLOOP .GT. 25) CALL ERROR (ERRKEY,1,' ',0)

      IF (INDEX('IE',RNMODE) .GT. 0)  THEN
         CALL CLEAR
         WRITE (*,200)
         DO I = 1, NLAYR
            WRITE (*,400) I,DS(I)-DLAYR(I),DS(I),DLAYR(I)
         END DO
         LINE(1) = ' '
         WRITE (*,500)
      ENDIF

      READ (5,600) LINE
      CALL VERIFY (LINE,SL,FLAG)

      IF (SL .LE. 0.0) THEN
         RETURN
       ELSE IF ((FLAG .GT. 0) .OR. (SL .GT. (I-1))) THEN
         WRITE (*,700) (I-1)
         GO TO 100
       ELSE IF (SL .NE. NINT(SL)) THEN
         WRITE (*,800)
         GO TO 100
       ELSE IF (SL .GT. 0.0) THEN
         I = NINT(SL)
         LINE(1) = ' '
         IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,900) I,DLAYR(I)
         READ (5,600) LINE
         CALL VERIFY (LINE,DL,FLAG)

         IF (DL .LE. 0.0) THEN
            DL = DLAYR(I)
          ELSE IF ((FLAG .GT. 0) .OR. (DL .GT. 50.0)) THEN
            WRITE (*,1000)
            GO TO 100
          ELSE IF (DL .GT. 0.0) THEN
            DLAYR(I) = DL
            DS(1) = DLAYR(1)
            IF (NLAYR .GT. 1) THEN
               DO I = 2, NL
                  DS(I) = DS(I-1) + DLAYR(I)
                  IF (DS(I) .GE. DEPMAX) GO TO 1010
               END DO
 1010          CONTINUE
               NLAYR = I
            ENDIF
         ENDIF
      ENDIF

      GO TO 100

C-----------------------------------------------------------------------
C     Format strings
C-----------------------------------------------------------------------

  200 FORMAT (//,9X,'LAYER DEPTH MODIFICATION',/,
     &           9X,'========================',//,
     &    5X,' 0)','  RETURN TO THE MAIN MENU',/)
  400 FORMAT (4X,I3,')',2X,F6.2,' - ',F6.2,'........]',2X,F6.2,' cm')
  500 FORMAT (//,9X,'SELECTION ? [ Default = 0 ] ===> ',$)
  600 FORMAT (80A1)
  700 FORMAT (10X,'ERROR! Layer Selection must be between 1 and ',I3,/)
  800 FORMAT (10X,'ERROR! Layer Selection must be an INTEGER value',/)
  900 FORMAT (/,9X,'CURRENT DEPTH FOR LAYER ',I2,' ===>',1X,F6.2,' cm',
     &        /,9X,'NEW DEPTH ?                    --->',1X,' ',$)
 1000 FORMAT (10X,'ERROR! Please reenter layer depth',/)

      END SUBROUTINE SEDLYR

C=======================================================================
C  SEPLYR, Subroutine
C
C  Determines layer selection - soil profile modification
C-----------------------------------------------------------------------
C  Revision history
C
C  01/01/1991 GH  Written
C  05/28/1993 PWW Header revision and minor changes
C-----------------------------------------------------------------------
C  INPUT  : DLAYR,NLAYR,RNMODE,DS,DESPAR,DESMOD,DESUN
C
C  LOCAL  : LINE,ERRKEY,NLOOP,I,DL,SL,FLAG
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : SEPROF
C
C  Calls  : ERROR CLEAR VERIFY
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE SEPLYR (DLAYR,NLAYR,RNMODE,DS,DESPAR,DESMOD,DESUN)

      USE ModuleDefs
      IMPLICIT     NONE
      EXTERNAL CLEAR, ERROR, VERIFY

      CHARACTER*1  RNMODE,LINE(80)
      CHARACTER*6  ERRKEY
      CHARACTER*10 DESUN
      CHARACTER*30 DESMOD

      INTEGER      NLOOP,I,NLAYR
      REAL         DLAYR(NL),DL,SL,FLAG,DS(NL),DESPAR(NL)

      PARAMETER (ERRKEY = 'SEPLYR')
      NLOOP  = 0

  100 CONTINUE
      NLOOP = NLOOP + 1
      IF (NLOOP .GT. 25) CALL ERROR (ERRKEY,1,' ',0)

      IF (INDEX('IE',RNMODE) .GT. 0)  THEN
         CALL CLEAR
         WRITE (*,200) DESMOD
         DO I = 1, NLAYR
            WRITE (*,400) I,DS(I)-DLAYR(I),DS(I),DESPAR(I),DESUN
         END DO
         LINE(1) = ' '
         WRITE (*,500)
      ENDIF

      READ (5, 600) LINE
      CALL VERIFY (LINE,SL,FLAG)

      IF (SL .LE. 0.0) THEN
         RETURN
       ELSE  IF ((FLAG .GT. 0.0) .OR. (SL .GT. (I-1))) THEN
         WRITE (*,700) (I-1)
         GO TO 100
       ELSE IF (SL .NE. NINT(SL)) THEN
         WRITE (*,800)
         GO TO 100
       ELSE IF (SL .GT. 0.0) THEN
         I = NINT(SL)
         LINE(1) = ' '
         IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,900) I,DESPAR(I),DESUN
         READ (5, 600) LINE
         CALL VERIFY (LINE,DL,FLAG)
         IF (DL .LT. 0.0) THEN
            GO TO 100
          ELSE IF (DESPAR(I) .EQ. 0.0 .AND. DL .GT. 0) THEN
            DESPAR(I) = DL
          ELSE IF ((FLAG .GT. 0)
     &      .OR. (DL .GT. (1000*DESPAR(I)) .AND. DESPAR(I) .GT. 0.0)
     &      .OR. (DL .LT.(0.001*DESPAR(I)) .AND. DL .NE. 0.000)) THEN
            WRITE (*,1000)
            GO TO 100
          ELSE IF (DL .GE. 0.0) THEN
            DESPAR(I) = DL
         ENDIF
      ENDIF

      GO TO 100

C-----------------------------------------------------------------------
C     Format strings
C-----------------------------------------------------------------------

  200 FORMAT (//,9X,A30,/,9X,30('='),//,
     &           5X,' 0)','  RETURN TO THE MAIN MENU',/)
  400 FORMAT (4X,I3,')',2X,F4.0,' - ',F4.0,' cm ........]',2X,F10.4,
     &        2X,A10)
  500 FORMAT (//,9X,'SELECTION ? [ Default = 0 ] ===> ',$)
  600 FORMAT (80A1)
  700 FORMAT (10X,'ERROR! Layer Selection must be between 1 and ',I3,/)
  800 FORMAT (10X,'ERROR! Layer Selection must be an INTEGER value',/)
  900 FORMAT (/,9X,'CURRENT VALUE FOR LAYER ',I2,' ===>',1X,F10.4,
     & 1X,A10,/,9X,'NEW VALUE ?',16X,'--->',5X,' ',$)
 1000 FORMAT (10X,'ERROR! Please reenter value',/)

      END SUBROUTINE SEPLYR

C=======================================================================
C  SESURF, Subroutine
C
C  Determines soil surface parameter modification
C-----------------------------------------------------------------------
C  Revision history
C
C  01/01/1991 GH  Written
C  05/28/1993 PWW Header revision and minor changes
C-----------------------------------------------------------------------
C  INPUT  : RNMODE,U,CN2,SWCON,SALB,SLNF,SLPF
C
C  LOCAL  : LINE,ERRKEY,NLOOP,MENU,FLAG,EFF
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : SESOIL
C
C  Calls  : ERROR CLEAR VERIFY
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE SESURF (RNMODE,U,CN2,SWCON,SALB,SLNF,SLPF)

      IMPLICIT    NONE
      EXTERNAL CLEAR, ERROR, VERIFY

      CHARACTER*1 RNMODE,LINE(80)
      CHARACTER*6 ERRKEY

      INTEGER     NLOOP,MENU
      REAL        FLAG,EFF,U,CN2,SWCON,SALB,SLNF,SLPF

      PARAMETER (ERRKEY = 'SESURF')

      NLOOP = 0

  100 CONTINUE
      NLOOP = NLOOP + 1
      IF (NLOOP .GT. 25) CALL ERROR (ERRKEY,1,' ',0)

      IF (INDEX('IE',RNMODE) .GT. 0) THEN
         CALL CLEAR
         WRITE (*,3900) SALB,U,CN2,SWCON,SLNF,SLPF
         WRITE (*,4050)
      ENDIF
C
C     Get menu choice
C
      READ (5,'(I2)',ERR = 100) MENU
C
C     Branch to menu choice
C
      IF (MENU .EQ. 0) THEN
          RETURN
      ELSE IF (MENU .EQ. 1) THEN
          IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,4500) SALB
          READ (5,5200) LINE
          CALL VERIFY (LINE,EFF,FLAG)
          IF (EFF .GT. 0.0 .AND. EFF .LE. 1.0 .AND. FLAG .LE. 0) THEN
             SALB = EFF
          ENDIF
      ELSE IF (MENU .EQ. 2) THEN
          IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,4700) U
          READ (5,5200) LINE
          CALL VERIFY (LINE,EFF,FLAG)
          IF (EFF .GT. 0.0 .AND. EFF .LE. 100.0 .AND. FLAG .LE. 0) THEN
             U = EFF
          ENDIF
      ELSE IF (MENU .EQ. 3) THEN
          IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,4800) CN2
          READ (5,5200) LINE
          CALL VERIFY (LINE,EFF,FLAG)
          IF (EFF .GT. 0.0 .AND. EFF .LE. 250.0 .AND. FLAG .LE. 0) THEN
             CN2 = EFF
          ENDIF
      ELSE IF (MENU .EQ. 4) THEN
          IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,4900) SWCON
          READ (5,5200) LINE
          CALL VERIFY (LINE,EFF,FLAG)
          IF (EFF .GT. 0.0 .AND. EFF .LE. 100.0 .AND. FLAG .LE. 0) THEN
             SWCON = EFF
          ENDIF
      ELSE IF (MENU .EQ. 5) THEN
          IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,5000) SLNF
          READ (5,5200) LINE
          CALL VERIFY (LINE,EFF,FLAG)
          IF (EFF .GT. 0.0 .AND. EFF .LE. 1.0 .AND. FLAG .LE. 0) THEN
             SLNF = EFF
          ENDIF
      ELSE IF (MENU .EQ. 6) THEN
          IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,5100) SLPF
          READ (5,5200) LINE
          CALL VERIFY (LINE,EFF,FLAG)
          IF (EFF .GT. 0.0 .AND. EFF .LE. 2.0 .AND. FLAG .LE. 0) THEN
             SLPF = EFF
          ENDIF
      ENDIF

      GO TO 100

C-----------------------------------------------------------------------
C     Format strings
C-----------------------------------------------------------------------

 3900 FORMAT (9X,'SOIL SURFACE PARAMETERS',/,
     &        9X,'=======================',//,
     &    5X,' 0. Return to Previous Menu ',//
     &    5X,' 1. Soil Albedo ...................] ',F6.3,/,
     &    5X,' 2. Evaporation Limit .............] ',F6.3,/,
     &    5X,' 3. Runoff Curve Number ...........] ',F6.3,/,
     &    5X,' 4. Drainage Rate .................] ',F6.3,/,
     &    5X,' 5. Mineralization Factor .........] ',F6.3,/,
     &    5X,' 6. Growth Reduction/Fertility Fac.] ',F6.3,/)
 4050 FORMAT (//,9X,'SELECTION (#) ? [ Default = 0 ] ---> ',$)
 4500 FORMAT (//,9X,'SOIL ALBEDO                 ===>',1X,F6.3,/,
     &           9X,'NEW SOIL ALBEDO ?           --->  ',$)
 4700 FORMAT (//,9X,'EVAPORATION LIMIT           ===>',1X,F6.3,/,
     &           9X,'NEW EVAPORATION LIMIT ?     --->  ',$)
 4800 FORMAT (//,9X,'RUNOFF CURVE NUMBER         ===>',1X,F6.3,/,
     &           9X,'NEW RUNOFF CURVE NUMBER ?   --->  ',$)
 4900 FORMAT (//,9X,'DRAINAGE RATE               ===>',1X,F6.3,/,
     &           9X,'NEW DRAINAGE RATE ?         --->  ',$)
 5000 FORMAT (//,9X,'MINERALIZATION FACTOR       ===>',1X,F6.3,/,
     &           9X,'NEW MINERALIZATION FACTOR ? --->  ',$)
 5100 FORMAT (//,9X,'FERTILITY FACTOR            ===>',1X,F6.3,/,
     &           9X,'[RELATIVE - VALUE BETWEEN 0 and 1 ]',/,
     &           9X,'NEW FERTILITY FACTOR ?      --->  ',$)
 5200 FORMAT (80A1)

      END SUBROUTINE SESURF
