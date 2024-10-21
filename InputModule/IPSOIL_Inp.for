C=======================================================================
C  IPSOIL_Inp, Subroutine
C
C  Soil selection
C-----------------------------------------------------------------------
C  Revision history
C
C  06/21/1991 JWJ 
C  05/28/1993 PWW Header revision and minor changes
C  12/01/1993 WTB Modified for soil P
C  12/12/2000 GH  Modified format
C  07/01/2003 GH  Add error checking codes for surface values
!  04/07/2005 CHP Added EXCA, exchangable calcium (cmol/kg) 
!  06/13/2005 CHP Free format read for soils parameters.  Format is 
!                 defined by location of headers.
!  08/23/2005 CHP Fixed some error checking for soil water parameters.
!  05/18/2006 CHP Modifications for input subroutine module
!  09/01/2011 CHP Added Van Genuchten parameters in optional 3rd tier of data
!  07/19/2024  FO Updated Soil prop. error checking conditions.
C-----------------------------------------------------------------------
C  INPUT  : RNMODE,FILES,PATHSL,ISWWAT
C
C  LOCAL  :
C
C  OUTPUT : NSENS
C-----------------------------------------------------------------------
C  Called : SESOIL INPUT
C
C  Calls  : FIND IGNORE ERROR
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE IPSOIL_Inp (RNMODE,FILES,PATHSL,NSENS,ISWITCH)

      USE ModuleDefs
      IMPLICIT NONE
      EXTERNAL ERROR, WARNING, IGNORE2, IGNORE, CLEAR, UPCASE, VERIFY, 
     &  PARSE_HEADERS, LYRSET, LYRSET3, LYRSET2, LMATCH

      INCLUDE 'COMSOI.blk'

      CHARACTER*1   LINE(80),RNMODE,BLANK,ANS,UPCASE
      CHARACTER*5   MH(NL)
      CHARACTER*6   ERRKEY
      CHARACTER*12  FILES
      CHARACTER*78  MSG(25)
      CHARACTER*80  PATHSL
      CHARACTER*92  FILESS
      CHARACTER*255 C255

      INTEGER I,J,P1,NLAYRI,LINSOL,ISECT
      INTEGER NSENS,NLSOIL,NLOOP,ERR,LUNSL,PATHL, LINSOL_1

!     05/27/2004 CHP Added these variables to COMSOI.blk
!      REAL    PTERMA(NL),PTERMB(NL),EXK(NL),EXMG(NL),EXNA(NL),EXTS(NL)
!      REAL    SLEC(NL),ZLYR(NL),ZZLYR(NL)
      REAL    ZLYR(NL)
      REAL    FLAG,SL,SLDP

!     Up to MAXCOL headers per line, up to 10 characters long each
      INTEGER, PARAMETER :: MAXCOL = 50
      CHARACTER*15  HEADER(MAXCOL)
!     COL keeps beginning and ending column for each header
      INTEGER COL(MAXCOL,2), C1, C2, COUNT, L

      TYPE (SwitchType) ISWITCH

      PARAMETER (ERRKEY = 'IPSOIL')
      PARAMETER (LUNSL  = 12)
      PARAMETER (BLANK = ' ')

      NLSOIL = 0
!-----------------------------------------------------------------------
!     No soil file read - default conditions
      IF (SLNO(1:3) .EQ. '   ' .OR. SLNO(1:3) .EQ. '-99' .OR.
     &    SLNO(8:10) .EQ. '-99' .OR. SLNO(8:10) .EQ. '   ') THEN
         SLPF  = 1.0
         NLAYR = 1

!     Read soil file
       ELSE
         LINSOL = 0
         PATHL  = INDEX(PATHSL,BLANK)
         IF (PATHL .LE. 1) THEN
            FILESS = FILES
          ELSE
            FILESS = PATHSL(1:(PATHL-1)) // FILES
         ENDIF
         OPEN (LUNSL, FILE = FILESS,STATUS = 'OLD',IOSTAT=ERR)
         IF (ERR .NE. 0) THEN
            CALL ERROR(ERRKEY,ERR,FILES,0)
         END IF

!-----------------------------------------------------------------------
!     Sensitivity Analysis - soil selection
         IF (NSENS .EQ. 1 ) THEN
            NLOOP  = 0
            NLSOIL = 0
            DO I = 1, 80
               LINE(I) = ' '
            END DO
            I  = 1
            IF (INDEX('IE',RNMODE) .GT. 0) THEN
               CALL CLEAR
               WRITE (*, 5130)
            ENDIF
 10         CONTINUE
            CALL IGNORE(LUNSL, LINSOL, ISECT, C255)
            IF ( ISECT .EQ. 0 ) GO TO 111
            IF (C255(1:1) .NE. '*') GO TO 10
            IF (C255(2:5) .EQ. 'SOIL') GO TO 10

            READ (C255,5030,IOSTAT=ERR) PEDON,SLSOUR,SLTXS,
     &             SLDP,SLDESC
            IF (ERR .NE. 0) THEN
               CALL ERROR (ERRKEY,ERR,FILES,LINSOL)
            ENDIF

            DO P1 = 1, 10
              PEDON(P1:P1) = UPCASE(PEDON(P1:P1))
              SLNO(P1:P1)  = UPCASE(SLNO(P1:P1))
            END DO
            IF (INDEX('IE',RNMODE) .GT. 0) WRITE(*, 5140) I,SLDESC,PEDON
            IF (PEDON  .EQ. SLNO) NLSOIL = I
C
C           Write out pause statement every 15 lines
C
            IF ((MOD(I,15) .EQ. 0).AND.(INDEX('IE',RNMODE) .GT. 0)) THEN
               WRITE (*,5000)
               READ  (5,'(A1)') ANS
            END IF
            I  = I + 1

            GOTO 10
 111        CONTINUE

            NLOOP = NLOOP + 1
            IF (NLOOP .GT. 25) THEN
               CALL ERROR (ERRKEY,1,FILES,LINSOL)
            ENDIF
            LINE(1) = ' '

            IF ((INDEX('IE',RNMODE) .GT. 0) .AND. NLSOIL .EQ. 0) THEN
               WRITE (*,950) SLNO, FILESS
            ENDIF
            IF (INDEX('IE',RNMODE) .GT. 0) WRITE(*, 5160) NLSOIL
            READ (5,'(80A1)') LINE
            CALL VERIFY (LINE,SL,FLAG)

            IF (SL .LE. 0) THEN
               SL  = NLSOIL
             ELSEIF ((FLAG .GT. 0) .OR. (SL .GT. (I-1))) THEN
               WRITE (*,5101) (I-1)
               GO TO 111
             ELSEIF (SL .NE. NINT(SL)) THEN
               WRITE (*,5102)
               GO TO 111
             ELSEIF (SL .GT. 0.) THEN
               NLSOIL = NINT(SL)
             ELSE
               CALL ERROR(ERRKEY,3,FILES,LINSOL)
            ENDIF

            REWIND (LUNSL)
         ENDIF     !End of sensitivity selection

!-----------------------------------------------------------------------
!     Set default values for soil parameters
      SLTXS  = '-99  '
      SLSOUR = '-99        '
      SLDESC = '-99                                              '
      TAXON  = '-99                                              '
      SSITE  = '-99        '
      SCOUNT = '-99        '
      SCOM   = '-99  '
      SMHB   = '-99  '
      SMPX   = '-99  '
      SMKE   = '-99  '
      SGRP   = '-99  '
      
      SLAT   = -99.
      SLONG  = -99.
      SLDP   = -99.
      SALB   = -99.
      SLNF   = -99.
      SLPF   = -99.
      CLAY   = -99.
      SILT   = -99.
      STONES = -99.
      OC     = -99.
      PH     = -99.
      BD     = -99.
      LL     = -99.
      DUL    = -99.
      SAT    = -99.
      SWCN   = -99.
      PHKCL  = -99.
      CEC    = -99.

      U      = -99.
      SWCON  = -99.
      CN2    = -99.
      SHF    = -99.
      TOTN   = -99.
      ADCOEF = 0.
      
!     2ND TIER
      EXTP   = -99.
      TOTP   = -99.
      ORGP   = -99.
      CACO   = -99.
      EXTAL  = -99.
      EXTFE  = -99.
      EXTMN  = -99.
      TOTBAS = -99.
      PTERMA = -99.
      PTERMB = -99.
      EXK    = -99.
      EXMG   = -99.
      EXNA   = -99.
      EXTS   = -99.
      SLEC   = -99.
      EXCA   = -99.

      alphaVG= -99.
      mVG = -99.
      nVG = -99.
      WCR = -99.

!     Stable organic C and SAEA read from soil analysis section only.
!     Output with 2nd tier soil data to INP file.
      SASC   = -99.
      SAEA   = -99.

!-----------------------------------------------------------------------
!     Find correct soil within soil file
         I = 0
 5024    CONTINUE
         I = I + 1
 5025    CONTINUE
         CALL IGNORE (LUNSL,LINSOL,ISECT,C255)
         IF ( ISECT .EQ. 0 ) THEN     !end of file
            CLOSE(LUNSL)
            IF (FILES(1:4) .EQ. 'SOIL') THEN
               FILES(1:8) = SLNO(1:2)//'.SOL  '
               PATHL  = INDEX(PATHSL,BLANK)
               IF (PATHL .LE. 1) THEN
                 FILESS = FILES
               ELSE
                 FILESS = PATHSL(1:(PATHL-1)) // FILES
               ENDIF
               OPEN (LUNSL, FILE = FILESS,STATUS = 'OLD',IOSTAT=ERR)
               IF (ERR .NE. 0) THEN
                  CALL ERROR(ERRKEY,ERR,FILES,0)
               ENDIF
               LINSOL = 0
               GO TO 5025
            ENDIF
!            IF (ISWITCH%ISWWAT .EQ. 'N') THEN
!              SLPF  = 1.0
!              NLAYR = 1
!              RETURN
!            ELSE
              MSG(1) = "Soil profile missing from file."
              WRITE(MSG(2),'("Soil ID: ",A)') SLNO
              WRITE(MSG(3),'("File: ",A)') FILESS(1:72)
              CALL WARNING(3,ERRKEY,MSG)
              CALL ERROR (ERRKEY,4,FILES,LINSOL)
!            ENDIF
         ENDIF
         IF (C255(1:1) .NE. '*') GO TO 5025
         IF (C255(2:5) .EQ. 'SOIL') GO TO 5025
         READ (C255,5030,IOSTAT=ERR) PEDON,SLSOUR,SLTXS,SLDP,SLDESC

!        chp 5/23/2013 - ignore error in file if this is not the profile of interest
         IF (ERR .NE. 0) THEN
           WRITE(MSG(1),'("Error in soil file on line ",I6)') LINSOL
           MSG(2) = 
     &         "Model will continue to search for a valid soil profile."
           CALL WARNING(2,ERRKEY,MSG)
         ENDIF

         IF (PEDON .NE. SLNO .AND. NSENS .EQ. 0) GO TO 5024
         IF (I .LT. NLSOIL .AND. NSENS .EQ. 1) GO TO 5024

!        If there was an error reading the profile and this is the profile of interest, then
!        call an error and stop model run.
         IF (ERR .NE. 0) CALL ERROR (ERRKEY,ERR,FILES,LINSOL)
         DO P1 = 1, 10
           PEDON(P1:P1) = UPCASE(PEDON(P1:P1))
           SLNO(P1:P1)  = UPCASE(SLNO(P1:P1))
         END DO

!-----------------------------------------------------------------------
!        Found correct soil
!-----------------------------------------------------------------------
!        Read general information in fixed format
         CALL IGNORE2 (LUNSL, LINSOL, ISECT, C255)
         IF (ISECT .EQ. 3) THEN
           CALL IGNORE (LUNSL,LINSOL,ISECT,C255)
           IF (ISECT .NE. 1) CALL ERROR (ERRKEY,4,FILES,LINSOL)
           READ (C255,5035,IOSTAT=ERR) SSITE,SCOUNT,SLAT,SLONG,TAXON
           IF (ERR .NE. 0) CALL ERROR (ERRKEY,ERR,FILES,LINSOL)
         ENDIF

!        Initialize
         HEADER = '-99'
         NLAYR = 0

!        Loop thru soils file
C-KRT*******************************************************************
C-KRT g95 compiler does not have an intrinsic EOF function
C-KRT I believe the EOF check is redundant anyway - KRT (8/29/2008)
C-KRT    GoodSoil: DO WHILE (.NOT. EOF(LUNSL))
         GoodSoil: DO WHILE (.true.)
C-KRT*******************************************************************

           CALL IGNORE2 (LUNSL, LINSOL, ISECT, C255)
           SELECT CASE(ISECT)

!          ----------------------------------------------
           CASE(0,2)  !End of file, End of Section
             IF (NLAYRI == 0) THEN
               IF (ISECT == 0) CALL ERROR(ERRKEY,4,FILES,LINSOL)
               IF (ISECT == 2) CALL ERROR(ERRKEY,3,FILES,LINSOL)
             ENDIF
             EXIT GoodSoil

!          ----------------------------------------------
           CASE(3)  !Header line found ('@' in column 1)
             CALL PARSE_HEADERS(C255, MAXCOL, HEADER, COUNT, COL)
             IF (COUNT .LT. 1) CALL ERROR (ERRKEY,10,FILES,LINSOL)
             L = 0

!          ----------------------------------------------
           CASE(1)  !Data line found
             IF (INDEX(HEADER(1),'-99') > 0) THEN
               CALL ERROR(ERRKEY,3,FILES,LINSOL)
             ENDIF

!            Read columns of data
             IF (TRIM(HEADER(1)) .NE. 'SLB') THEN
               DO I = 1, COUNT
                 C1 = COL(I,1)
                 C2 = COL(I,2)
                 SELECT CASE (TRIM(HEADER(I)))

                 CASE('SCOM'); READ(C255(C1:C2),*,IOSTAT=ERR) SCOM
                 CASE('SALB'); READ(C255(C1:C2),*,IOSTAT=ERR) SALB
                 CASE('SLU1'); READ(C255(C1:C2),*,IOSTAT=ERR) U
                 CASE('SLDR'); READ(C255(C1:C2),*,IOSTAT=ERR) SWCON
                 CASE('SLRO'); READ(C255(C1:C2),*,IOSTAT=ERR) CN2
                 CASE('SLNF'); READ(C255(C1:C2),*,IOSTAT=ERR) SLNF
                 CASE('SLPF'); READ(C255(C1:C2),*,IOSTAT=ERR) SLPF
                 CASE('SMHB'); READ(C255(C1:C2),*,IOSTAT=ERR) SMHB
                 CASE('SMPX'); READ(C255(C1:C2),*,IOSTAT=ERR) SMPX
                 CASE('SMKE'); READ(C255(C1:C2),*,IOSTAT=ERR) SMKE
                 CASE('SGRP'); READ(C255(C1:C2),*,IOSTAT=ERR) SGRP

!                 CASE('SOILLAT'); READ(C255(C1:C2),*,IOSTAT=ERR) SLAT
!                 CASE('SOILLONG');READ(C255(C1:C2),*,IOSTAT=ERR) SLONG
!                 CASE('SLTX');    READ(C255(C1:C2),*,IOSTAT=ERR) SLTXS
!                 CASE('SLDP');    READ(C255(C1:C2),*,IOSTAT=ERR) SLDP
!                 CASE('SOIL_SCS')
!                   READ(C255(C1:C2),'(A50)',IOSTAT=ERR) TAXON
!                   TAXON = ADJUSTL(TAXON)
!
!                 CASE('SLDN');    READ(C255(C1:C2),*,IOSTAT=ERR) SLDN
!                 CASE('ETDR');    READ(C255(C1:C2),*,IOSTAT=ERR) ETDR

                 END SELECT
                 IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILES,LINSOL)
               ENDDO

             ELSE
               !Soil layer data
                L = L + 1
!               LyrLoop: DO L = 1, NL + 1
                 IF (L .GT. NL) CALL ERROR (ERRKEY,2,FILES,LINSOL)
                 IF (L == 1) LINSOL_1 = LINSOL
!                 CALL IGNORE (LUNSL,LINSOL,ISECT,C255)
!                 IF (C255(1:1) .EQ. '*' .OR. ISECT .NE. 1) EXIT GoodSoil

                 !First header of layer data must be layer depth
                 C1 = COL(1,1)
                 C2 = COL(1,2)
                 READ(C255(C1:C2),*,IOSTAT=ERR) ZLYR(L)
                 IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILES,LINSOL)

                 !Read subsequent columns of data
                 ColLoop: DO I = 2, COUNT  
                   C1 = COL(I,1)
                   C2 = COL(I,2)
                   SELECT CASE (TRIM(HEADER(I)))

                   CASE('SLMH'); READ(C255(C1:C2),*,IOSTAT=ERR)    MH(L)
                   CASE('SLLL'); READ(C255(C1:C2),*,IOSTAT=ERR)    LL(L)
                   CASE('SDUL'); READ(C255(C1:C2),*,IOSTAT=ERR)   DUL(L)
                   CASE('SSAT'); READ(C255(C1:C2),*,IOSTAT=ERR)   SAT(L)
                   CASE('SRGF'); READ(C255(C1:C2),*,IOSTAT=ERR)   SHF(L)
                   CASE('SSKS'); READ(C255(C1:C2),*,IOSTAT=ERR)  SWCN(L)
                   CASE('SBDM'); READ(C255(C1:C2),*,IOSTAT=ERR)    BD(L)
                   CASE('SLOC'); READ(C255(C1:C2),*,IOSTAT=ERR)    OC(L)
                   CASE('SLCL'); READ(C255(C1:C2),*,IOSTAT=ERR)  CLAY(L)
                   CASE('SLSI'); READ(C255(C1:C2),*,IOSTAT=ERR)  SILT(L)
                   CASE('SLCF'); READ(C255(C1:C2),*,IOSTAT=ERR)STONES(L)
                   CASE('SLNI'); READ(C255(C1:C2),*,IOSTAT=ERR)  TOTN(L)
                   CASE('SLHW'); READ(C255(C1:C2),*,IOSTAT=ERR)    PH(L)
                   CASE('SLHB'); READ(C255(C1:C2),*,IOSTAT=ERR) PHKCL(L)
                   CASE('SCEC'); READ(C255(C1:C2),*,IOSTAT=ERR)   CEC(L)
                   CASE('SADC') 
                      READ(C255(C1:C2),*,IOSTAT=ERR) ADCOEF(L)
                      IF (ERR .NE. 0) THEN        
                        ADCOEF = -99.; ERR = 0
                      ENDIF

!                  04/20/2005 CHP added 2nd tier data
                   CASE('SLPX'); READ(C255(C1:C2),*,IOSTAT=ERR)  EXTP(L)
                   CASE('SLPT'); READ(C255(C1:C2),*,IOSTAT=ERR)  TOTP(L)
                   CASE('SLPO'); READ(C255(C1:C2),*,IOSTAT=ERR)  ORGP(L)
                   CASE('CACO3');READ(C255(C1:C2),*,IOSTAT=ERR)  CACO(L)
                   CASE('SLAL'); READ(C255(C1:C2),*,IOSTAT=ERR) EXTAL(L)
                   CASE('SLFE'); READ(C255(C1:C2),*,IOSTAT=ERR) EXTFE(L)
                   CASE('SLMN'); READ(C255(C1:C2),*,IOSTAT=ERR) EXTMN(L)
                   CASE('SLBS'); READ(C255(C1:C2),*,IOSTAT=ERR)TOTBAS(L)
                   CASE('SLPA'); READ(C255(C1:C2),*,IOSTAT=ERR)PTERMA(L)
                   CASE('SLPB'); READ(C255(C1:C2),*,IOSTAT=ERR)PTERMB(L)
                   CASE('SLKE'); READ(C255(C1:C2),*,IOSTAT=ERR)   EXK(L)
                   CASE('SLMG'); READ(C255(C1:C2),*,IOSTAT=ERR)  EXMG(L)
                   CASE('SLNA'); READ(C255(C1:C2),*,IOSTAT=ERR)  EXNA(L)
                   CASE('SLSU'); READ(C255(C1:C2),*,IOSTAT=ERR)  EXTS(L)
                   CASE('SLEC'); READ(C255(C1:C2),*,IOSTAT=ERR)  SLEC(L)
                   CASE('SLCA'); READ(C255(C1:C2),*,IOSTAT=ERR)  EXCA(L)

!                  09/01/2011 CHP added 3rd tier data
!                  Optional Van Genuchten parameters
                   CASE('ALFVG')
                                READ(C255(C1:C2),*,IOSTAT=ERR)alphaVG(L)
                   CASE('MVG'); READ(C255(C1:C2),*,IOSTAT=ERR)  mVG(L)
                   CASE('NVG'); READ(C255(C1:C2),*,IOSTAT=ERR)  nVG(L)
                   CASE('WCRES');READ(C255(C1:C2),*,IOSTAT=ERR) WCR(L)

                   END SELECT

                   !IF (ERR .NE. 0) THEN
                   !  CALL ERROR(ERRKEY,ERR,FILES,LINSOL)
         !ENDIF
                 ENDDO ColLoop

!                !Check for validity of layer data
!                IF (SAT(L) .LT. DUL(L)) CALL ERROR(ERRKEY,7,FILES,LINSOL)
!                IF (DUL(L) .LT. LL(L))  CALL ERROR(ERRKEY,8,FILES,LINSOL)
!                IF (SAT(L) - DUL(L) .LT. 0.01) SAT(L) = DUL(L) + 0.01
!                IF (DUL(L) - LL(L)  .LT. 0.01) LL(L)  = DUL(L) - 0.01
!               ENDDO LyrLoop
               NLAYRI = L
                                                                        
!              CHP 05/21/2004
!              Fix for deep layer soils -- set bottom of 20th layer to 
!              depth of profile
               IF (NLAYRI == NL) THEN
                 DS(NL) = MAX(DS(NL), ZLYR(NLAYRI))
               ENDIF
                                 
             ENDIF    !Soil layer data
           END SELECT !ISECT
         ENDDO GoodSoil

         SLNO = PEDON
         CLOSE (LUNSL)

!        Check validity of soil values 
	   IF (SWCON .LT. 0.0) CALL ERROR (ERRKEY,10,FILES,LINSOL)
	   IF (CN2 .LE. 0.0)   CALL ERROR (ERRKEY,11,FILES,LINSOL)
         IF (SALB .LE. 0.0) THEN
!           SALB = 0.13
!           MSG(1) = "Soil albedo not specified. " //
!     &        "Default value of 0.13 will be used."
!           CALL WARNING(1,ERRKEY,MSG)
            CALL ERROR (ERRKEY,12,FILES,LINSOL)
         ENDIF

         IF (SLNF > 1.0 .OR. SLNF < 0.0) THEN
            SLNF = 1.0
            IF (ISWITCH%ISWNIT .NE. 'N') THEN
               MSG(1) = "Invalid value for SLNF in soil file."
               MSG(2) = "Value changed to 1.0."
              CALL WARNING(2,ERRKEY,MSG)
            ENDIF
         ENDIF

         IF (ISWITCH%ISWWAT .NE. 'N') THEN
           ERR = 0
           DO J = 1, NLAYRI
             IF ((DUL(J) - SAT(J)) .GT. 0.0) THEN
                CALL ERROR (ERRKEY,7,FILES,LINSOL_1+J-1)
              ENDIF
              IF ((LL(J) - DUL(J)) .GT. 0.0) THEN
                 CALL ERROR (ERRKEY,8,FILES,LINSOL_1+J-1)
              ENDIF
              IF (DUL(J) .LT. 0.0) THEN
                 CALL ERROR (ERRKEY,13,FILES,LINSOL_1+J-1)
              ENDIF
              IF (ABS(SAT(J) - DUL(J)) .LE. 0.0) THEN
                 SAT(J) = DUL(J) + 0.01
              ENDIF
              IF (ABS(DUL(J) -  LL(J)) .LE. 0.0) THEN
                 LL(J) = DUL(J) - 0.01
              ENDIF  
              IF (SHF(J) .LT. 0.0) THEN
                 WRITE(MSG(1),'(A,A72)') 'File: ',FILESS
                 
                 WRITE(MSG(2),'(A,I4,2X,A,I2)') 
     &                'Line number:',LINSOL_1+J-1, 'Soil layer: ',J
                 MSG(3) = 'Root growth factor is missing.  '
                 MSG(4) = 'Model requires value between 0 and 1.'
                 MSG(5) = 'Program will stop.'
                 CALL WARNING(5,ERRKEY,MSG)
                 CALL ERROR(ERRKEY,14,FILES,LINSOL_1+J-1)
              ENDIF

             IF (SWCN(J) < 0.0) THEN
               SWCN(J) = -99.
               ERR = ERR + 1
             ENDIF
           ENDDO

           IF (ERR > 0) THEN
             MSG(1) = "Saturated hydraulic conductivity equal to " // 
     &          "zero for one or more soil layers."
             MSG(2) = "Data will be treated as missing."
             CALL WARNING(2,ERRKEY,MSG)
           ENDIF
         ENDIF

         SELECT CASE (ISWITCH % MESOL)
         CASE('1'); CALL LYRSET (NLAYRI, ZLYR, NLAYR, DS, DLAYR, DEPMAX)
         CASE('3'); CALL LYRSET3(NLAYRI, ZLYR, DS, NLAYR, DLAYR, DEPMAX)
         CASE DEFAULT
                    CALL LYRSET2(NLAYRI, ZLYR, DS, NLAYR, DLAYR, DEPMAX)
         END SELECT

!        Check for top layer too thick (could happen for MESOL='3')
         IF (DLAYR(1) > 5) THEN
           MSG(1) = "Soil layer 1 thicker than 5 cm."
           MSG(2) = "Soil water algorithms may become unstable."
           CALL WARNING(2,ERRKEY,MSG)
         ENDIF

!        CHP 10/2/2009 No evidence of instabilities with thin layers - remove message
!         DO L = 1, NLAYR
!           IF (DLAYR(L) < 5.) THEN
!             WRITE(MSG(1),
!     &     '("Soil layer",I3,": thickness =",F4.1," cm")') L, DLAYR(L)
!             MSG(2)="Thin layers may result in instabilities."
!             MSG(3)="See INFO.OUT for more detail on soil layers."
!             CALL WARNING(3,ERRKEY,MSG)
!           ENDIF
!         ENDDO

!!        Print soil layer data to INFO.OUT
!         WRITE(MSG(1),'(A,A)') "Soil layer distribution method ", 
!     &            ISWITCH%MESOL
!         MSG(2) = "            Thick-"
!         MSG(3) = "      Depth  ness"
!         MSG(4) = "   L   (cm)   (cm)"
!         DO L = 1, NLAYR
!           WRITE(MSG(L+4),'(I4,2F7.0)') L, DS(L), DLAYR(L)
!         ENDDO
!         CALL INFO(NLAYR+4, "LYRSET", MSG)
!
!        First tier soils data
         CALL LMATCH (NLAYRI, ZLYR, LL,    NLAYR, DS)
         CALL LMATCH (NLAYRI, ZLYR, DUL,   NLAYR, DS)
         CALL LMATCH (NLAYRI, ZLYR, SAT,   NLAYR, DS)
         CALL LMATCH (NLAYRI, ZLYR, SHF,   NLAYR, DS)
         CALL LMATCH (NLAYRI, ZLYR, SWCN,  NLAYR, DS)
         CALL LMATCH (NLAYRI, ZLYR, BD,    NLAYR, DS)
         CALL LMATCH (NLAYRI, ZLYR, OC,    NLAYR, DS)
         CALL LMATCH (NLAYRI, ZLYR, CLAY,  NLAYR, DS)
         CALL LMATCH (NLAYRI, ZLYR, SILT,  NLAYR, DS)
         CALL LMATCH (NLAYRI, ZLYR, STONES,NLAYR, DS)
         CALL LMATCH (NLAYRI, ZLYR, TOTN,  NLAYR, DS)
         CALL LMATCH (NLAYRI, ZLYR, PH,    NLAYR, DS)
         CALL LMATCH (NLAYRI, ZLYR, PHKCL, NLAYR, DS)
         CALL LMATCH (NLAYRI, ZLYR, CEC,   NLAYR, DS)
         CALL LMATCH (NLAYRI, ZLYR, ADCOEF,NLAYR, DS)

!        Second tier soils data
         CALL LMATCH (NLAYRI, ZLYR, EXTP,  NLAYR, DS)
         CALL LMATCH (NLAYRI, ZLYR, TOTP  ,NLAYR, DS)
         CALL LMATCH (NLAYRI, ZLYR, ORGP,  NLAYR, DS)
         CALL LMATCH (NLAYRI, ZLYR, CACO,  NLAYR, DS)
         CALL LMATCH (NLAYRI, ZLYR, EXTAL, NLAYR, DS)
         CALL LMATCH (NLAYRI, ZLYR, EXTFE, NLAYR, DS)
         CALL LMATCH (NLAYRI, ZLYR, EXTMN, NLAYR, DS)
         CALL LMATCH (NLAYRI, ZLYR, TOTBAS,NLAYR, DS)
         CALL LMATCH (NLAYRI, ZLYR, PTERMA,NLAYR, DS)
         CALL LMATCH (NLAYRI, ZLYR, PTERMB,NLAYR, DS)
         CALL LMATCH (NLAYRI, ZLYR, EXK,   NLAYR, DS)
         CALL LMATCH (NLAYRI, ZLYR, EXMG,  NLAYR, DS)
         CALL LMATCH (NLAYRI, ZLYR, EXNA,  NLAYR, DS)
         CALL LMATCH (NLAYRI, ZLYR, EXTS,  NLAYR, DS)
         CALL LMATCH (NLAYRI, ZLYR, SLEC,  NLAYR, DS)
         CALL LMATCH (NLAYRI, ZLYR, EXCA,  NLAYR, DS)

         CALL LMATCH (NLAYRI, ZLYR, alphaVG,NLAYR, DS)
         CALL LMATCH (NLAYRI, ZLYR, mVG,  NLAYR, DS)
         CALL LMATCH (NLAYRI, ZLYR, nVG,  NLAYR, DS)
         CALL LMATCH (NLAYRI, ZLYR, WCR,  NLAYR, DS)
     
      ENDIF

      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

!  950 FORMAT (/,5X,' SOIL ',A10,' is not in the SOIL file SOIL.SOL !')
  950 FORMAT (/,5X,' SOIL ',A10,' is not in the SOIL file:',/,A92)
 5000 FORMAT (/, '  More.... press < ENTER > key ')
 5030 FORMAT (1X,A10, 2X, A11,1X,A5,1X,F5.0,1X,A50)
 5035 FORMAT (2(1X,A11),2(1X,F8.3),1X,A50)
! 5040 FORMAT (1X,A5,6(1X,F5.0),4(1X,A5))
! 5040 FORMAT (1X,A5,6(1X,F5.0),4(1X,A5),1X,F5.0)
! 5040 FORMAT (1X,A5,6(1X,F5.0),3(1X,A5),F6.0) !McNair's 5/13/2005
! 5080 FORMAT (1X,F5.0,1X,A5,29(1X,F5.0))
! 5090 FORMAT (1X,F5.0,18(1X,F5.0))
 5101 FORMAT (10X,'ERROR! Soil Selection must be between 1 and ',I3,/)
 5102 FORMAT (10X,'ERROR! Soil Selection must be an INTEGER value',/)
 5130 FORMAT (T25, 'SOILS IN THE DATA BASE', /,T3, 'REF', T25,
     &        22('='),/,T3, 'NO.', T7, 'TAXONOMY NAME', T67,
     &        'PEDON NUMBER', /T2, 4('-'), 1X, 50('-'), T67, 12('-'))
 5140 FORMAT (I4,') ',A50, T67, A10)
 5160 FORMAT (/,6X,'SELECTED SOIL TYPE ===>',1X,I3,
     &        /,6X,'NEW SELECTION ?    --->',2X,' ',$)

      END SUBROUTINE IPSOIL_Inp
C=======================================================================
! IPSOIL_Inp Variables
! NLAYR total # of layers in the converted soil layers
! NLAYRI total # of layers in original soil file
! ZLYR   Depth of bottom of original soil file
!=======================================================================