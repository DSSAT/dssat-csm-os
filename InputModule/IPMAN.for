C=======================================================================
C  IPIRR, Subroutine
C
C  Determines irrigation application for a simulation
C-----------------------------------------------------------------------
C  Revision history
C
C  01/01/1993 GH  Written
C  05/28/1993 PWW Header revision and minor changes
C  04/16/2002 GH  Modified logic for reading planting date
C  06/19/2002 GH  Modified for Y2K
C  08/23/2002 GH  Expanded array for irrigation applications to NAPPL
C  05/07/2020 FO  Added new Y4K subroutine call to convert YRDOY
C-----------------------------------------------------------------------
C  INPUT  : LUNEXP,FILEX,LNIR,YRSIM,ISWWAT,NIRR,EFFIRX,DSOILX,THETCX
C           IEPTX,IOFFX,IAMEX,NAPW,TOTAPW,AIRAMX,IDLAPL,IRRCOD,AMT
C
C  LOCAL  : IDLAPL,ISECT,LINEXP,IFIND,LN,J,ERRNUM,AMT,ERRKEY,IRRCOD
C           CHARTEST
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : IPEXP
C
C  Calls  : FIND ERROR IGNORE
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE IPIRR (LUNEXP,FILEX,LNIR,YRSIM,ISWWAT,
     &           NIRR,EFFIRX,DSOILX,THETCX,IEPTX,IOFFX,IAMEX,LNSIM,
     &           NAPW,TOTAPW,AIRAMX,IDLAPL,IRRCOD,AMT,IIRV,IIRRI)

      USE ModuleDefs
      IMPLICIT NONE
      EXTERNAL ERROR, FIND, IGNORE, HFIND, IGNORE2, Y4K_DOY

      INTEGER      LNIR,NIRR,LUNEXP,IDLAPL(NAPPL),ISECT,LINEXP,LNSIM
      INTEGER      YRSIM,IFIND,LN,J,ERRNUM,NAPW,IIRV(NAPPL),IRRCD

      REAL         AMT(NAPPL),DSOILX,THETCX,IEPTX,EFFIRX,TOTAPW,AIRAMX

      CHARACTER*1  ISWWAT,IIRRI
      CHARACTER*5  IRRCOD(NAPPL),IOFFX,IAMEX
      CHARACTER*6  FINDCH,ERRKEY
      CHARACTER*12 FILEX
      CHARACTER*80 CHARTEST

      INTEGER I, PERM, PERMDOY

      PARAMETER (ERRKEY='IPIRR ')

                 FINDCH='*IRRIG'
C
C     Set default values in case section or data are missing from file EXP
C
      EFFIRX = 1.00
      NIRR   = 0
      NAPW   = 0
      TOTAPW = 0.0
      THETCX = 0.0
      DSOILX = 0.0
      AIRAMX = 0.0
      IOFFX  = 'GS000'
      IAMEX  = 'IR001'

      DO J = 1, NAPPL
         IDLAPL(J) = 0
         AMT(J)    = 0.0
         IRRCOD(J) = '     '
      END DO

      IF (LNIR .GT. 0) THEN
         IF (ISWWAT .EQ. 'N' .AND. LNSIM .EQ. 0) THEN
            IIRRI  = 'R'
            ISWWAT = 'Y'
         ENDIF
         NIRR = 1
         CALL FIND (LUNEXP,FINDCH,LINEXP,IFIND)
         IF (IFIND .EQ. 0) CALL ERROR (ERRKEY,1,FILEX,LINEXP)
 50      CALL IGNORE (LUNEXP,LINEXP,ISECT,CHARTEST)
         IF (ISECT .EQ. 1) THEN
            READ (CHARTEST,55,IOSTAT=ERRNUM) LN
            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
            IF (LN .NE. LNIR) GO TO 50
            READ (CHARTEST,55,IOSTAT=ERRNUM) LN,EFFIRX,DSOILX,
     &                        THETCX,IEPTX,IOFFX,IAMEX,AIRAMX
            IF (AIRAMX .LT. 0.0) AIRAMX = 0.0
            IF (EFFIRX .LE. 0.0) EFFIRX = 1.0
            IF (DSOILX .LE. 0.0) DSOILX = 30.0
            IF (THETCX .LE. 0.0) THETCX = 75.0
            IF (IOFFX(1:3) .EQ. '-99' .OR. IOFFX(3:5) .EQ. '-99')
     &          IOFFX = 'GS000'
            IF (IAMEX(1:3) .EQ. '-99' .OR. IAMEX(3:5) .EQ. '-99')
     &          IAMEX = 'IR001'
            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
          ELSE
            CALL ERROR (ERRKEY,2,FILEX,LINEXP)
         ENDIF
C
C        Read different IRR amounts for the selected IRR level
C
 70      CONTINUE 
         PERM = 0
         CALL IGNORE (LUNEXP,LINEXP,ISECT,CHARTEST)
         IF (ISECT .EQ. 1) THEN
            READ (CHARTEST,60,IOSTAT=ERRNUM) LN
            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
            IF (LN .GT. LNIR) GO TO 120
            READ (CHARTEST,60,IOSTAT=ERRNUM) LN,IDLAPL(NIRR),
     &                        IRRCOD(NIRR),AMT(NIRR),IIRV(NIRR)
            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
            IF ((IDLAPL(NIRR) .LT.  0) .OR.
     &         (IIRRI .EQ. 'R' .AND. MOD(IDLAPL(NIRR),1000) .GT. 366))
     &         CALL ERROR (ERRKEY,10,FILEX,LINEXP)
C  FO - 05/07/2020 Add new Y4K subroutine call to convert YRDOY
            !IF (IIRRI .NE. 'D') CALL Y2K_DOY (IDLAPL(NIRR))
            IF (IIRRI .NE. 'D') THEN
              CALL Y4K_DOY (IDLAPL(NIRR),FILEX,LINEXP,ERRKEY,3)
            ENDIF
            IF (IIRRI .EQ. 'R' .AND. IDLAPL(NIRR) .LT. YRSIM) 
     &          CALL ERROR (ERRKEY,3,FILEX,LINEXP)
     
            IF (IIRRI .EQ. 'D' .AND. IDLAPL(NIRR) .LT. 0) GO TO 70

!           chp 04/18/2013 remove this requirement. For puddling event, 
!               irrigation amount is not a required field.
!            IF ((AMT(NIRR) .LT. -1.0) .OR. (AMT(NIRR) .GT. 99999.)) 
!     &         CALL ERROR (ERRKEY,11,FILEX,LINEXP)

            READ (IRRCOD(NIRR)(3:5),'(I3)',IOSTAT=ERRNUM) IRRCD
            IF (IRRCD .LT. 1 .OR. IRRCD .GT. 11 .OR.  !CHP changed to 11
     &          ERRNUM .NE. 0) CALL ERROR (ERRKEY,12,FILEX,LINEXP)

            !CHP start ***********************************************
            !Check for old rice irrigation codes and convert to new.
            !For bunding event (IRRCOD=9), permanent and continuous 
            !  flood elevations are indicated by 7 and 8 in 4th column.
            !  Irrigation events for this date are read as depths of 
            !  permanent flood.
            !CSM treats permanent and continuous flooding events
            !  the same.
            !IIRV is no longer used.  However, since old files will have
            !  this code, read and convert IRRCD where necessary.  
            IF ((IRRCD .EQ. 3 .OR. IRRCD .EQ. 9) .AND. 
     &            IIRV(NIRR) .GT. 0) THEN
              SELECT CASE (IIRV(NIRR))
              CASE (6);       PERM = 1 
              CASE (7);       PERM = 2
              CASE (8);       PERM = 3
              CASE DEFAULT;   PERM = 0
              END SELECT

            !Check previous irrigation events that have already been 
            !  read in for this date.  Some of the input files have 
            !  the irrigation record before the bund record.
            !When PERM > 0, change irrigation events to type 6 (PERM =1)
            !  or type 11 (PERM = 2 or 3).
              IF (PERM > 0 .AND. NIRR .GT. 1) THEN
                PERMDOY = IDLAPL(NIRR)

                !Work back from current record 
                DO I = NIRR-1, 1, -1
                  IF (IDLAPL(I) .EQ. PERMDOY .AND.
     &                IRRCOD(I)(3:5) .NE. '007' .AND.
     &                IRRCOD(I)(3:5) .NE. '008' .AND.
     &                IRRCOD(I)(3:5) .NE. '009' .AND.
     &                IRRCOD(I)(3:5) .NE. '010') THEN 

                    !Found irrigation event corresponding to today's
                    !  bund record.  Change the irrigation code.
                    SELECT CASE (PERM)
                    CASE (1)   
                      WRITE (IRRCOD(I), '(A5)') IRRCOD(I)(1:2) // '006'
                    CASE (2,3)
                      WRITE (IRRCOD(I), '(A5)') IRRCOD(I)(1:2) // '011'
                    END SELECT
                  ENDIF   !End of previous record found block
                ENDDO     !End of loop thru previous records
              ENDIF       !End of PERM > 0 block
            ENDIF         !End of IRRCOD = 9 (old-style bund record) 
            !CHP end *************************************************

            IF ((IRRCOD(NIRR)(3:5)) .NE. '007' .AND.
     &          (IRRCOD(NIRR)(3:5)) .NE. '008' .AND.
     &          (IRRCOD(NIRR)(3:5)) .NE. '009' .AND.
     &          (IRRCOD(NIRR)(3:5)) .NE. '010') THEN 
                NAPW = NAPW + 1
                IF (AMT(NAPW) .GT. 0.0) THEN
                  TOTAPW = TOTAPW + AMT(NAPW)
                ENDIF

            !CHP start ***********************************************
            !When PERM > 0, change irrigation events to type 6 (PERM =1)
            !  or type 11 (PERM = 2 or 3).
              IF (PERM > 0) THEN
                SELECT CASE (PERM)
                CASE (1)
                  WRITE(IRRCOD(NIRR), '(A5)') IRRCOD(NIRR)(1:2) // '006'
                CASE (2,3)
                  WRITE(IRRCOD(NIRR), '(A5)') IRRCOD(NIRR)(1:2) // '011'
                END SELECT
              ENDIF
            ENDIF
            !CHP end *************************************************

            NIRR = NIRR + 1
            IF (NIRR .GT. NAPPL) GO TO 120
          ELSE
            GO TO 120
         ENDIF
         GO TO 70
      ENDIF

 120  CONTINUE
      REWIND (LUNEXP)
      NIRR = MAX((NIRR - 1),0)

      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

 55   FORMAT (I3,F5.0,3(1X,F5.0),2(1X,A5),1X,F5.0)
 60   FORMAT (I3,I5,1X,A5,1X,F5.0,4X,I2)
      END SUBROUTINE IPIRR
!=======================================================================
!     IPIRR VARIABLE DEFINITIONS:
!-----------------------------------------------------------------------
!IDLAPL(I) Irrigation or water table dates read from input file.  
!IIRRI    Irrigation switch R=on reported dates, D=as reported, days after 
!            planting, A=automatic, when required., F=automatic w/ fixed 
!            amt, P=as reported thru last reported day then automatic, W=as 
!            reported thru last reported day then fixed amount, N=not 
!            irrigated 
!IIRV is no longer used. It is water table depth for IR007, otherwise it is?
!IRRCOD    Irrigation Codes: IR001: Furrow in mm; IR006: Flood depth in mm; IR009 Bund height in mm
!ISWWAT   Water simulation control switch (Y or N). It is read from X file simulation water
!LNSIM    Simulation treatment setting in X file
!NIRR # of irrigation events reading
!
!-----------------------------------------------------------------------
!     END SUBROUTINE IPIRR
!=======================================================================


C=======================================================================
C  IPRES, Subroutine
C
C  Determines residue application for a simulation
C-----------------------------------------------------------------------
C  Revision history
C
C  01/01/1993 GH  Written
C  05/28/1993 PWW Header revision and minor changes
C  06/19/2002 GH  Modified for Y2K
C  05/28/1993 PWW Header revision and minor changes
C  08/23/2002 GH  Expanded array for organic material applications to NAPPL
C  02/03/2005 GH  Corrected error checking for missing levels
C  05/07/2020 FO  Added new Y4K subroutine call to convert YRDOY
C-----------------------------------------------------------------------
C  INPUT  : LUNEXP,FILEX,LNRES,RESDAY,RESCOD,RESIDUE,RINP,DEPRES,
C           RESN,RESP,RESK,NARES,RESAMT,ISWNIT,YRSIM,ISWPHO,ISWPOT
C
C  LOCAL  : ERRKEY,CHARTEST,ISECT,LINEXP,ERRNUM,J,IFIND,LN
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : IPEXP
C
C  Calls  : FIND IGNORE ERROR
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE IPRES (LUNEXP,FILEX,LNRES,RESDAY,RESCOD,RESIDUE,
     &     RINP,DEPRES,RESN,RESP,RESK,NARES,RESAMT,ISWNIT,YRSIM,
     &     ISWPHO,ISWPOT,IRESI,ISWWAT,RMET,LNSIM)

      USE ModuleDefs
      IMPLICIT     NONE
      EXTERNAL ERROR, FIND, IGNORE, WARNING, Y4K_DOY

      CHARACTER*1  ISWNIT,ISWPHO,ISWPOT,IRESI,ISWWAT
      CHARACTER*5  RESCOD(NAPPL),RMET(NAPPL)
      CHARACTER*6  ERRKEY,FINDCH
      CHARACTER*12 FILEX
      CHARACTER*78 MSG(3)
      CHARACTER*80 CHARTEST

      INTEGER      LNRES,LUNEXP,ISECT,LINEXP,RESDAY(NAPPL),NRESAP
      INTEGER      ERRNUM,J,IFIND,LN,NARES,YRSIM,LNSIM,IRESCD

      REAL         RESN(NAPPL),RESP(NAPPL),RESK(NAPPL),RESIDUE(NAPPL),
     &             RINP(NAPPL),DEPRES(NAPPL),RESAMT

      PARAMETER   (ERRKEY ='IPRES ')

                   FINDCH ='*RESID'

      NRESAP = 0
      NARES  = 0
      RESAMT = 0.0

      DO J = 1, NAPPL
         RESCOD(J)  = '     '
         RMET(J)    = '     '
         RESDAY(J)  = 0
         RESIDUE(J) = 0.0
!         RINP(J)    = 100.0
         RINP(J)    = 0.0
         DEPRES(J)  = 0.0
         RESN(J)    = 0.0
         RESP(J)    = 0.0
         RESK(J)    = 0.0
      END DO

      IF (LNRES .GT. 0) THEN
         IF (ISWNIT .EQ. 'N' .AND. LNSIM .EQ. 0) THEN
           ISWNIT = 'Y'
         ENDIF
         IF (ISWNIT .EQ. 'Y' .AND. LNSIM .EQ. 0) THEN
           IRESI  = 'R'
         ENDIF
         IF (ISWWAT .EQ. 'N' .AND. LNSIM .EQ. 0) THEN
           ISWWAT = 'Y'
         ENDIF
         NRESAP = 1
         CALL FIND (LUNEXP,FINDCH,LINEXP,IFIND)
         IF (IFIND .EQ. 0) CALL ERROR (ERRKEY,1,FILEX,LINEXP)
 50      CALL IGNORE (LUNEXP,LINEXP,ISECT,CHARTEST)

         IF (ISECT .EQ. 1) THEN
            READ (CHARTEST,60,IOSTAT=ERRNUM) LN
            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
            IF (LN .NE. LNRES) GO TO 50
C
C           Read different residue types and amounts
C
            READ (CHARTEST,60,IOSTAT=ERRNUM) LN,RESDAY(NRESAP),
     &             RESCOD(NRESAP),RESIDUE(NRESAP),RESN(NRESAP),
     &             RESP  (NRESAP),RESK   (NRESAP),RINP(NRESAP),
     &             DEPRES(NRESAP),RMET(NRESAP)
            RESN(NRESAP)    = MAX (RESN(NRESAP),0.00)
            RESP(NRESAP)    = MAX (RESP(NRESAP),0.00)
            RESK(NRESAP)    = MAX (RESK(NRESAP),0.00)
C
C           RESIDUE(NRESAP) = MAX (RESIDUE(NRESAP),1000.0)
C
C           Set minimum lower than 1000 KG .. PWW
C
C-PW        RESIDUE(NRESAP) = MAX (RESIDUE(NRESAP),10.0)

C-GH 7/25/2022 Remove MAX statement; error checking below
C-GH        RESIDUE(NRESAP) = MAX (RESIDUE(NRESAP),0.0)
            
            
            READ (RESCOD(NRESAP)(3:5),'(I3)',IOSTAT=ERRNUM) IRESCD
            IF (IRESCD .LT. 1 .OR. IRESCD .GE. 999 .OR.
     &          ERRNUM .NE. 0) THEN
               CALL ERROR (ERRKEY,4,FILEX,LINEXP)
            ENDIF
                      
C-GH 7/25/2022 Check for missing residue code        
            IF ((RESDAY(NRESAP) .LT. 0) .OR.
     &          (IRESI .EQ. 'R' .AND. MOD(RESDAY(NRESAP),1000)
     &           .GT. 366)) THEN
               CALL ERROR (ERRKEY,11,FILEX,LINEXP)
            ENDIF
            IF (IRESI .EQ. 'R') THEN
C  FO - 05/07/2020 Add new Y4K subroutine call to convert YRDOY
              !CALL Y2K_DOY (RESDAY(NRESAP))
              CALL Y4K_DOY (RESDAY(NRESAP),FILEX,LINEXP,ERRKEY,3)
            ENDIF
            IF (IRESI .EQ. 'R' .AND. RESDAY(NRESAP) .LT. YRSIM) THEN
                CALL ERROR (ERRKEY,3,FILEX,LINEXP)
            ENDIF
            
            IF (RESIDUE(NRESAP) .LT. 0.0 .OR. RESIDUE(NRESAP)
     &           .GT. 99999.) THEN
               CALL ERROR (ERRKEY,11,FILEX,LINEXP)
            ENDIF
            IF ((RESN(NRESAP) .LT. 0) .OR.
     &          (RESN(NRESAP) .GT. 99999.)) THEN
               CALL ERROR (ERRKEY,12,FILEX,LINEXP)
            ENDIF
            IF (ISWPHO .EQ. 'Y' .OR. ISWPHO .EQ. 'H') THEN !CHP 3/29/05
               IF ((RESP(NRESAP) .LT.  0) .OR.
     &             (RESP(NRESAP) .GT. 99999.)) THEN
                  CALL ERROR (ERRKEY,13,FILEX,LINEXP)
               ENDIF
            ENDIF
            IF (ISWPOT .EQ. 'Y') THEN
               IF ((RESK(NRESAP) .LT.  0) .OR.
     &             (RESK(NRESAP) .GT. 99999.)) THEN
                  CALL ERROR (ERRKEY,13,FILEX,LINEXP)
               ENDIF
            ENDIF

            IF (RINP(NRESAP) < 0.0) THEN
              IF (DEPRES(NRESAP) > 0.0) THEN
                RINP(NRESAP) = 100.0
                DEPRES(NRESAP) = MAX (DEPRES (NRESAP),15.0)
                WRITE(MSG(1),'("Residue application ",I3)') NRESAP 
                WRITE(MSG(2),'(A,A)')"Incorporation percentage not ", 
     &              "specified, and incorporation depth > 0." 
                WRITE(MSG(3),'(A,A,F5.1,A)')"Applied residues will be ",
     &              "incorporated to a depth of ", DEPRES(NRESAP)," cm."
                CALL WARNING(3, ERRKEY, MSG)
              ELSE
                RINP(NRESAP) = 0.0
                WRITE(MSG(1),'("Residue application ",I3)') NRESAP 
                MSG(2) = "Neither residue incorporation percentage " // 
     &              "nor depth specified. Applied residues" 
                MSG(3) = "will remain on surface until a tillage " // 
     &              "event is specified."
                CALL WARNING(3, ERRKEY, MSG)
              ENDIF
            ENDIF
            IF (RINP(NRESAP) > 0.0 .AND. DEPRES(NRESAP) < 15.0) THEN
              DEPRES(NRESAP) = MAX (DEPRES (NRESAP),15.0)
              WRITE(MSG(1),'("Residue application ",I3)') NRESAP 
              WRITE(MSG(2),'(A,A,F5.1,A)')"Applied residues will be ",
     &              "incorporated to a depth of ", DEPRES(NRESAP)," cm."
              CALL WARNING(2, ERRKEY, MSG)
            ENDIF
            RESAMT = RESAMT + RESIDUE(NRESAP)
            NARES  = NARES  + 1
            NRESAP = NRESAP + 1
            IF (NRESAP .GT. NAPPL) GO TO 120
          ELSE
            IF (NRESAP .EQ. 1) THEN
              CALL ERROR (ERRKEY,2,FILEX,LINEXP)
            ENDIF
            GO TO 120
         ENDIF
         GO TO 50
      ENDIF

 120  REWIND (LUNEXP)
      NRESAP = MAX((NRESAP-1),0)

      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

 60   FORMAT (I3,I5,1X,A5,1X,F5.0,3(1X,F5.0),2(1X,F5.0),1X,A5)

      END SUBROUTINE IPRES

C=======================================================================
C  IPFERT, Subroutine
C
C  Subroutine to read in fertilizer applications during season
C  To read *FERTILIZER section in the V3.5 input files
C-----------------------------------------------------------------------
C  Revision history
C
C  05/08/1991 JWW Written for DSSAT v3 format
C  05/28/1993 PWW Header revision and minor changes
C  08/23/2002 GH  Expanded array for fertilizer applications to NAPPL
C  02/03/2005 GH  Corrected error checking for missing levels
C  05/07/2020 FO  Added new Y4K subroutine call to convert YRDOY
C-----------------------------------------------------------------------
C  INPUT  : LUNEXP,FILEX,LNFER,YRSIM,ISWNIT
C
C  LOCAL  : ISECT,LINEXP,LN,ERRKEY,CHARTEST,ERRNUM,J,IFIND
C
C  OUTPUT : NFERT,FDAY,IFTYPE,FERCOD,DFERT,ANFER,APFER,AKFER
C           ACFER,AOFER,FOCOD,ANFER
C-----------------------------------------------------------------------
C  Called : IPEXP
C
C  Calls  : FIND,IGNORE
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE IPFERT (LUNEXP,FILEX,LNFER,YRSIM,ISWNIT,ISWPHO,ISWPOT,
     &     NFERT,FDAY,IFTYPE,FERCOD,DFERT,ANFER,APFER,AKFER,ACFER,
     &     AOFER,FOCOD,TOTNAP,IFERI,ISWWAT,LNSIM)

      USE ModuleDefs
      IMPLICIT     NONE
      EXTERNAL ERROR, FIND, IGNORE, Y4K_DOY

      CHARACTER*1  ISWNIT,ISWPHO,ISWPOT,IFERI,ISWWAT
      CHARACTER*5  FERCOD(NAPPL),FOCOD(NAPPL),IFTYPE(NAPPL)
      CHARACTER*6  ERRKEY,FINDCH
      CHARACTER*12 FILEX
      CHARACTER*80 CHARTEST

      INTEGER      LUNEXP,LNFER,YRSIM,NFERT,FDAY(NAPPL),IFFTYP,IFFCOD
      INTEGER      ISECT,LINEXP,ERRNUM,J,IFIND,LN,LNSIM
      REAL         DFERT(NAPPL),ANFER(NAPPL),APFER(NAPPL),AKFER(NAPPL)
      REAL         ACFER(NAPPL),AOFER(NAPPL),TOTNAP

      PARAMETER   (ERRKEY ='IPFERT')

                   FINDCH ='*FERTI'

      NFERT  = 0
      TOTNAP = 0.0

      DO J = 1, NAPPL
         DFERT(J) = 0.0
         ANFER(J) = 0.0
         APFER(J) = 0.0
         AKFER(J) = 0.0
         ACFER(J) = 0.0
         AOFER(J) = 0.0
      END DO

      IF (LNFER .GT. 0) THEN
         IF (ISWNIT .EQ. 'N' .AND. LNSIM .EQ. 0) THEN
            ISWNIT = 'Y'
            IFERI  = 'R'
         ENDIF
         IF (ISWWAT .EQ. 'N' .AND. LNSIM .EQ. 0) THEN
           ISWWAT = 'Y'
         ENDIF
         LINEXP = 0
         NFERT  = 1
         CALL FIND (LUNEXP,FINDCH,LINEXP,IFIND)
         IF (IFIND .EQ. 0) CALL ERROR (ERRKEY,1,FILEX,LINEXP)
 50      CALL IGNORE (LUNEXP,LINEXP,ISECT,CHARTEST)
         IF (ISECT .EQ. 1) THEN
            READ (CHARTEST,60,IOSTAT=ERRNUM) LN
            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
            IF (LN .NE. LNFER) GO TO 50
C
C           Read different FERT amounts for the selected FERT level
C
            READ (CHARTEST,60,IOSTAT=ERRNUM) LN,FDAY(NFERT),
     &           IFTYPE(NFERT),FERCOD(NFERT),DFERT(NFERT),ANFER(NFERT),
     &           APFER (NFERT),AKFER (NFERT),ACFER(NFERT),AOFER(NFERT),
     &           FOCOD (NFERT)

            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
            IF ((FDAY(NFERT) .LT. 0) .OR.
     &         (IFERI .EQ. 'R' .AND. MOD(FDAY(NFERT),1000) .GT. 366))
     &         THEN
               CALL ERROR (ERRKEY,10,FILEX,LINEXP)
            ENDIF
            IF (IFERI .EQ. 'R') THEN
C  FO - 05/07/2020 Add new Y4K subroutine call to convert YRDOY
              !CALL Y2K_DOY(FDAY(NFERT))
              CALL Y4K_DOY(FDAY(NFERT),FILEX,LINEXP,ERRKEY,3)
            ENDIF
            IF (IFERI .EQ. 'R' .AND. FDAY(NFERT) .LT. YRSIM)  THEN
               CALL ERROR (ERRKEY,3,FILEX,LINEXP)
            ENDIF
            IF ((DFERT(NFERT) .LT. 0) .OR.
     &         (DFERT(NFERT) .GT. 99999.)) THEN
               CALL ERROR (ERRKEY,11,FILEX,LINEXP)
            ENDIF
            IF ((ANFER(NFERT) .LT. 0) .OR.
     &         (ANFER(NFERT) .GT. 99999.)) THEN
               CALL ERROR (ERRKEY,12,FILEX,LINEXP)
            ENDIF
            READ (IFTYPE(NFERT)(3:5),'(I3)',IOSTAT=ERRNUM) IFFTYP
            IF (IFFTYP .LT. 1 .OR. IFFTYP .GE. 999 .OR.
     &          ERRNUM .NE. 0) THEN
               CALL ERROR (ERRKEY,14,FILEX,LINEXP)
            ENDIF
            READ (FERCOD(NFERT)(3:5),'(I3)',IOSTAT=ERRNUM) IFFCOD
!CHP 2/1/11 IF (IFFCOD .LT. 1 .OR. IFFCOD .GT. 18 .OR.
            IF (IFFCOD .LT. 1 .OR. IFFCOD .GT. 20 .OR.
     &          ERRNUM .NE. 0) THEN
               WRITE(FERCOD(NFERT)(3:5),'(A3)') '001'
            ENDIF
            IF (ISWPHO .EQ. 'Y' .OR. ISWPHO .EQ. 'H') THEN !CHP 3/29/05
               IF ((APFER(NFERT) .LT. 0) .OR.
     &             (APFER(NFERT) .GT. 99999.)) THEN
!CHP 5/16/08     CALL ERROR (ERRKEY,13,FILEX,LINEXP)
                 APFER(NFERT) = 0.0
               ENDIF
            ENDIF
            IF (ISWPOT .EQ. 'Y') THEN !CHP 3/29/05
               IF ((AKFER(NFERT) .LT. 0) .OR.
     &             (AKFER(NFERT) .GT. 99999.)) THEN
!CHP 5/16/08     CALL ERROR (ERRKEY,13,FILEX,LINEXP)
                 AKFER(NFERT) = 0.0
               ENDIF
            ENDIF
            TOTNAP = TOTNAP + ANFER(NFERT)
            NFERT  = NFERT  + 1
            IF (NFERT .GT. NAPPL) GO TO 120
          ELSE
            IF (NFERT .EQ. 1) THEN
              CALL ERROR (ERRKEY,2,FILEX,LINEXP)
            ENDIF
            GO TO 120
         ENDIF
         GO TO 50
      ENDIF

 120  REWIND (LUNEXP)
      NFERT = MAX((NFERT - 1),0)

      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

 60   FORMAT (I3,I5,1X,A5,1X,A5,6(1X,F5.0),1X,A5)

      END SUBROUTINE IPFERT

C=======================================================================
C  IPHAR, Subroutine
C
C  Subroutine to read in harvest management
C 
C-----------------------------------------------------------------------
C  Revision history
C
C  05/08/1991 JWW Written for DSSAT v3 format
C  05/28/1993 PWW Header revision and minor changes
C  06/09/2002 GH  Modified for Y2K
C  02/03/2005 GH  Corrected error checking for missing levels
C  05/07/2020 FO  Added new Y4K subroutine call to convert YRDOY
C  04/01/2021 FO/VSH Update harvest array size for MultiHarvest
C-----------------------------------------------------------------------
C  INPUT  : LUNEXP,FILEX,LNHAR,YEAR
C
C  LOCAL  : LN,ERRKEY,CHARTEST,ERRNUM,J,IFIND
C
C  OUTPUT : HDATE,HSTG,HCOM,HSIZ,HPC,NHAR
C-----------------------------------------------------------------------
C  Called : IPEXP
C
C  Calls  : FIND,IGNORE
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE IPHAR (LUNEXP,FILEX,LNHAR,HDATE,HSTG,HCOM,HSIZ,HPC,
     &                  NHAR,IHARI,YRSIM,CROP,HBPC)  !,FREQ,CUHT)
!NEW FORAGE VARIABLES (DIEGO-2/14/2017)

      USE ModuleDefs
      IMPLICIT     NONE
      EXTERNAL ERROR, FIND, IGNORE, Y4K_DOY

      CHARACTER*1  IHARI
      CHARACTER*2  CROP
      CHARACTER*5  HSTG(NAPPL),HCOM(NAPPL),HSIZ(NAPPL)
      CHARACTER*6  ERRKEY,FINDCH
      CHARACTER*12 FILEX
      CHARACTER*80 CHARTEST

      INTEGER      LNHAR,LUNEXP,ISECT,LINEXP,HDATE(NAPPL),NHAR
      INTEGER      ERRNUM,J,IFIND,LN,YRSIM
!     INTEGER      HYR, HDAY

      REAL         HPC(NAPPL),HBPC(NAPPL)
!     REAL FREQ,CUHT !NEW FORAGE VARIABLES (DIEGO-2/14/2017)

      PARAMETER   (ERRKEY='IPHAR ')

                   FINDCH='*HARVE'

      NHAR  = 0

      DO J = 1, NHAR         
         HSTG(J)  = '     '
         HCOM(J)  = '     '
         HSIZ(J)  = '     '
         HPC(J)   = 100.0
         HDATE(J) = -99
      END DO

      IF (LNHAR .EQ. 0) GO TO 120
      NHAR = 1
      CALL FIND (LUNEXP,FINDCH,LINEXP,IFIND)
      IF (IFIND .EQ. 0) CALL ERROR (ERRKEY,1,FILEX,LINEXP)

 50   CALL IGNORE (LUNEXP,LINEXP,ISECT,CHARTEST)

      IF (ISECT .EQ. 1) THEN
         READ (CHARTEST,60,IOSTAT=ERRNUM) LN
         IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
         IF (LN .NE. LNHAR) GO TO 50
C
C        Read several lines of harvest details
C
         READ (CHARTEST,60,IOSTAT=ERRNUM) LN,HDATE(NHAR),HSTG(NHAR),
     &                  HCOM(NHAR),HSIZ(NHAR),HPC(NHAR),HBPC(NHAR)    !,
!CHP - Auto-harvest not yet implemented. Probably should go in Sim Controls, anyway.
!     &                  FREQ, CUHT !New variables for forages (Diego-2/14/2017)
         IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
         IF ((HDATE(NHAR) .LT.  0) .OR.
     &       (IHARI .EQ. 'R' .AND. MOD(HDATE(NHAR),1000) .GT. 366).OR.
     &       (IHARI .EQ. 'W' .AND. MOD(HDATE(NHAR),1000) .GT. 366).OR.
     &       (IHARI .EQ. 'X' .AND. MOD(HDATE(NHAR),1000) .GT. 366).OR.
     &       (IHARI .EQ. 'Y' .AND. MOD(HDATE(NHAR),1000) .GT. 366).OR.
     &       (IHARI .EQ. 'Z' .AND. MOD(HDATE(NHAR),1000) .GT. 366))
     &       THEN
             CALL ERROR (ERRKEY,10,FILEX,LINEXP)
         ENDIF
         IF (IHARI .EQ. 'R' .OR. IHARI .EQ. 'W' .OR.
     &   IHARI .EQ. 'X' .OR. IHARI .EQ. 'Y' .OR. IHARI .EQ. 'Z') THEN
C  FO - 05/07/2020 Add new Y4K subroutine call to convert YRDOY
           !CALL Y2K_DOY(HDATE(NHAR))
           CALL Y4K_DOY(HDATE(NHAR),FILEX,LINEXP,ERRKEY,6)
         ENDIF
         IF (IHARI .EQ. 'R' .AND. HDATE(NHAR) .LT. YRSIM) GO TO 50
         IF (IHARI .EQ. 'W' .AND. HDATE(NHAR) .LT. YRSIM) GO TO 50
         IF (IHARI .EQ. 'X' .AND. HDATE(NHAR) .LT. YRSIM) GO TO 50
         IF (IHARI .EQ. 'Y' .AND. HDATE(NHAR) .LT. YRSIM) GO TO 50
         IF (IHARI .EQ. 'Z' .AND. HDATE(NHAR) .LT. YRSIM) GO TO 50

!        Harvested product defaults to 100%
         IF (HPC(NHAR) .LT. -1.E-4) THEN
             HPC(NHAR) = 100.0
         ENDIF
!        Harvested by-product defaults to 0%
         IF (HBPC(NHAR) .LT. 1.E-4) THEN
             HBPC(NHAR) = 0.0
         ENDIF
         IF (HSTG(NHAR) .EQ. '     ') THEN
           HSTG(NHAR) = '  -99'
         ENDIF
         IF (HCOM(NHAR) .EQ. '     ') THEN
           HCOM(NHAR) = '  -99'
         ENDIF
         IF (HSIZ(NHAR) .EQ. '     ') THEN
           HSIZ(NHAR) = '  -99'
         ENDIF
         NHAR = NHAR + 1

         IF (NHAR .GE. NAPPL) GO TO 120
         
       ELSE
         IF (NHAR .EQ. 1) THEN
           CALL ERROR (ERRKEY,2,FILEX,LINEXP)
         ENDIF
         GO TO 120
      ENDIF

      GO TO 50
 120  REWIND (LUNEXP)

      IF ((INDEX('CSPT',CROP)) .GT. 0) THEN
        IF (HDATE(1) .LT. 0) THEN
           CALL ERROR (ERRKEY,13,FILEX,LINEXP)
        ENDIF
        IF (IHARI .EQ. 'A') THEN
           CALL ERROR (ERRKEY,14,FILEX,LINEXP)
        ENDIF
      ENDIF

      NHAR = MAX (0,NHAR-1)
      IF (LNHAR .EQ. 0 .AND. IHARI .NE. 'M' .AND. IHARI .NE. 'A') THEN
         CALL ERROR (ERRKEY,1,FILEX,LINEXP)
      ENDIF
      IF (IHARI .EQ. 'G' .AND. HSTG(1) .EQ. '     ') THEN
         CALL ERROR (ERRKEY,3,FILEX,LINEXP)
      ENDIF
      IF (IHARI .EQ. 'R' .AND. HDATE(1) .EQ. 0) THEN
         CALL ERROR (ERRKEY,4,FILEX,LINEXP)
      ENDIF
      IF (IHARI .EQ. 'W' .AND. HDATE(1) .EQ. 0) THEN
         CALL ERROR (ERRKEY,4,FILEX,LINEXP)
      ENDIF
      IF (IHARI .EQ. 'X' .AND. HDATE(1) .EQ. 0) THEN
         CALL ERROR (ERRKEY,4,FILEX,LINEXP)
      ENDIF
      IF (IHARI .EQ. 'Y' .AND. HDATE(1) .EQ. 0) THEN
         CALL ERROR (ERRKEY,4,FILEX,LINEXP)
      ENDIF
      IF (IHARI .EQ. 'Z' .AND. HDATE(1) .EQ. 0) THEN
         CALL ERROR (ERRKEY,4,FILEX,LINEXP)
      ENDIF
      IF (IHARI .EQ. 'D' .AND. HDATE(1) .EQ. 0) THEN
         CALL ERROR (ERRKEY,5,FILEX,LINEXP)
      ENDIF
!      WRITE(7000,'(2F5.0)') FREQ,CUHT
      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

!     editted to read forage variables (Diego-2/14/2017)
 60   FORMAT (I3,I5,3(1X,A5),2(1X,F5.0),6X,F5.0,F5.0) 

      END SUBROUTINE IPHAR

C=======================================================================
C  IPCUL, Subroutine
C
C  Reads parameters related cultivar selection from FILEX file
C  Reads new IBSNAT formats
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written, P. Wilkens                           P.W.W       4-12-93
C  2  Modified by
C  3. Header revision and minor changes             P.W.W       5-28-93
C-----------------------------------------------------------------------
C  INPUT  : LUNEXP,FILEX,LNCU
C
C  LOCAL  : LN,CHARTEST,ERRKEY,LINEXP,ISECT,IFIND,ERRNUM
C
C  OUTPUT : CROP,VARNO,VRNAME
C-----------------------------------------------------------------------
C  Called : IPEXP
C
C  Calls  : FIND IGNORE ERROR
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE IPCUL (LUNEXP,FILEX,LNCU,CROP,VARNO)

      IMPLICIT     NONE
      EXTERNAL ERROR, FIND, IGNORE, UPCASE

      INTEGER      LNCU,LUNEXP,ISECT,LINEXP
      INTEGER      IFIND,LN,ERRNUM

      CHARACTER*1  UPCASE
      CHARACTER*2  CROP
      CHARACTER*6  ERRKEY,VARNO,FINDCH
      CHARACTER*12 FILEX
      CHARACTER*16 CNAME
      CHARACTER*80 CHARTEST

      PARAMETER (ERRKEY='IPCUL ')

                 FINDCH='*CULTI'

      IF (LNCU .GT. 0) THEN
         CALL FIND (LUNEXP,FINDCH,LINEXP,IFIND)
         IF (IFIND .EQ. 0) CALL ERROR (ERRKEY,1,FILEX,LINEXP)
 50      CALL IGNORE (LUNEXP,LINEXP,ISECT,CHARTEST)
         IF (ISECT .EQ. 1) THEN
            READ (CHARTEST,55,IOSTAT=ERRNUM) LN
            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
            IF (LN .NE. LNCU) GO TO 50
            IF (LN .GT. LNCU) GO TO 120
            READ (CHARTEST,55,IOSTAT=ERRNUM) LNCU,CROP,VARNO,CNAME
            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
          ELSE
            CALL ERROR(ERRKEY,2,FILEX,LINEXP)
         ENDIF
      ENDIF

      CROP(1:1) = UPCASE(CROP(1:1))
      CROP(2:2) = UPCASE(CROP(2:2))

 120  CONTINUE
      REWIND (LUNEXP)
      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

 55   FORMAT (I3,A2,1X,A6,1X,A16)

      END SUBROUTINE IPCUL
