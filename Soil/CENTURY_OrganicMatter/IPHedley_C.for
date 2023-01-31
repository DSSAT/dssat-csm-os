!=======================================================================
!  IPHedley_C
!
!  Reads soil P Hedley fractionation data from FILEH.
!  This optional section of data can be supplied when phosphorus is 
!     being simulated (ISWPHO = 'H').

!-----------------------------------------------------------------------
!  REVISION HISTORY
!  09/19/2003 AJG Brought into modular format and linked to the 
!                 CENTURY-based SOM/residue module.
!  12/17/2003 AJG Renamed all the CH_ and CHEM_ variables to P_ variables.
!  02/12/2004 AJG Renamed the subroutine from CHEM_INI to P_INI.
!  08/30/2004 AJG Added default values for Hedley fractions.
!  03/25/2005 CHP Separated IPHedley into separate subroutine.
!-----------------------------------------------------------------------
!  Called by: SoilP_inorg
!  Calls: None    
!=======================================================================

      SUBROUTINE IPHedley_C (CONTROL, NLAYR,              !Input    
     &  P_FUMIG, P_RESIDUAL, P_TOTAL, Po_BICARB,          !Output
     &  Po_NaOH, Po_NaOHsonic, Po_HClhot, UseHedley)      !Output

!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         !which contain control information, soil
                         !parameters, hourly weather data.
      IMPLICIT  NONE
      EXTERNAL FIND, IGNORE2, PARSE_HEADERS, LMATCH, WARNING, ERROR
!     ------------------------------------------------------------------
      CHARACTER*6,  PARAMETER :: ERRKEY = 'IPHEDL'
      CHARACTER*6   FINDCH
      CHARACTER*12  FILEX, FILEH
      CHARACTER*78  MSG(10)
      CHARACTER*255 C255

      INTEGER ERR, FOUND, ISECT, I, L, LNUM, LUNIO, NLAYRI, NLAYR 

!     Hedley fractionation data
      REAL, DIMENSION(NL) :: DS, DLAYRI,
     &  P_FUMIG, P_RESIDUAL, P_TOTAL, 
     &  Po_BICARB, Po_HClhot, Po_NAOH, Po_NaOHsonic

!     Up to MAXCOL headers per line, up to 10 characters long each
      INTEGER, PARAMETER :: MAXCOL = 25
      CHARACTER*15  HEADER(MAXCOL)
!     COL keeps beginning and ending column for each header
      INTEGER COL(MAXCOL,2), C1, C2, COUNT

      LOGICAL UseHedley !, EOF

      TYPE (ControlType) CONTROL
      FILEX = CONTROL % FILEX
      LUNIO = CONTROL % LUNIO

!-----------------------------------------------------------------------
!     Determine name of FILEH from FILEX
      FILEH = FILEX(1:11) // "H"

!     Set default initial conditions in case they are missing
      P_FUMIG      = -99.
      P_RESIDUAL   = -99.
      P_TOTAL      = -99.
      Po_BICARB    = -99.
      Po_HClhot    = -99.
      Po_NaOH      = -99.
      Po_NaOHsonic = -99.
      UseHedley    = .True.

      OPEN (LUNIO, FILE = FILEH, STATUS = 'OLD', IOSTAT=ERR)
      IF (ERR .NE. 0) GOTO 5000

      FINDCH = '*HEDLE'
      CALL FIND (LUNIO,FINDCH,LNUM,FOUND)
      IF (FOUND .EQ. 0) GOTO 5000

      ReadLoop: DO WHILE (.TRUE.)    !.NOT. EOF(LUNIO)
        CALL IGNORE2 (LUNIO,LNUM,ISECT,C255)
        IF (ISECT .EQ. 0) THEN
          EXIT
        ELSEIF (ISECT .EQ. 3) THEN
!         Found header line -- parse headers
          CALL PARSE_HEADERS(C255, MAXCOL, HEADER, COUNT, COL)
          IF (COUNT .LT. 1) GOTO 5000

        ELSEIF (ISECT .EQ. 1) THEN
!         Found data line 
!         Loop thru layers
!         First layer -- data record has already been read.
          LyrLoop: DO L = 1, NL
            IF (L /= 1) THEN
              CALL IGNORE2 (LUNIO,LNUM,ISECT,C255)
              IF (ISECT .NE. 1) EXIT ReadLoop
            ENDIF

            C1 = COL(1,1)
            C2 = COL(1,2)
            READ(C255(C1:C2),*,IOSTAT=ERR) DLAYRI(L)
            IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEX,LNUM)

!            Read subsequent columns of data
            ColLoop: DO I = 1, COUNT  
              C1 = COL(I,1)
              C2 = COL(I,2)
              SELECT CASE (TRIM(HEADER(I)))
              CASE('PFUMI');READ(C255(C1:C2),*,IOSTAT=ERR)    P_FUMIG(L)
              CASE('PRESI');READ(C255(C1:C2),*,IOSTAT=ERR) P_RESIDUAL(L)
              CASE('PTOTL');READ(C255(C1:C2),*,IOSTAT=ERR)    P_TOTAL(L)
              CASE('POBIC');READ(C255(C1:C2),*,IOSTAT=ERR)  Po_BICARB(L)
              CASE('PONAO');READ(C255(C1:C2),*,IOSTAT=ERR)    Po_NaOH(L)
              CASE('PONAS')
                          READ(C255(C1:C2),*,IOSTAT=ERR) Po_NaOHsonic(L)
              CASE('POHCH');READ(C255(C1:C2),*,IOSTAT=ERR)  Po_HClhot(L)
              END SELECT

              IF (ERR .NE. 0) THEN
                GOTO 5000
              ENDIF
            ENDDO ColLoop
          ENDDO LyrLoop
        ENDIF
      ENDDO ReadLoop
      NLAYRI = L - 1

      CLOSE (LUNIO)

!     Match soil layer depths to DS
      CALL LMATCH (NLAYRI, DLAYRI, P_FUMIG    , NLAYR, DS)
      CALL LMATCH (NLAYRI, DLAYRI, P_RESIDUAL , NLAYR, DS)
      CALL LMATCH (NLAYRI, DLAYRI, P_TOTAL    , NLAYR, DS)
      CALL LMATCH (NLAYRI, DLAYRI, Po_BICARB  , NLAYR, DS)
      CALL LMATCH (NLAYRI, DLAYRI, Po_NaOH    , NLAYR, DS)
      CALL LMATCH (NLAYRI, DLAYRI, Po_NaOHsonic,NLAYR, DS)
      CALL LMATCH (NLAYRI, DLAYRI, Po_HClhot  , NLAYR, DS)

!-----------------------------------------------------------------------
!     Check Hedley data 
      UseHedley = .True.
      DO L = 1, NLAYR
!       Need to have either P_FUMIG or P_TOTAL to be able to use
!         Hedley data.
        IF (P_FUMIG(L) < -1.E-6 .AND. P_TOTAL(L) < -1.E-6) THEN
          GOTO 5000
        ENDIF
      ENDDO

      RETURN
!-----------------------------------------------------------------------
!     Error handling
 5000 CONTINUE
      UseHedley = .False.
      MSG(1) = 'Error in Hedley fractionation data file, ' // FILEH // 
     &              '.'
      MSG(2) = 'Use default P initialization methods.'
      CALL WARNING(2, ERRKEY, MSG)

!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE IPHedley_C
!=======================================================================


!**********************************************************************
!  PROBABLY WON'T USE THIS SUBROUTINE AT ALL.  EITHER YOU HAVE GOOD 
!    HEDLEY DATA OR YOU DON'T
!**********************************************************************
!=======================================================================
!  Hedinit_org
!
!  Estimate Hedley P data and inorganic P data if only total P is known.
!
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  03/29/2005 CHP Written based in info in P_INI.
!-----------------------------------------------------------------------
!  Called by: SoilP_inorg
!  Calls: None    
!=======================================================================
      SUBROUTINE Hedinit_org (L, TOTP,                    !Input
     &    P_RESIDUAL, P_TOTAL, Pi_BICARB, Pi_HCl,         !I/O
     &    Pi_HClhot, Pi_NaOH, Pi_NaOHsonic, Pi_RESIN,     !I/O
     &    Po_BICARB, Po_HClhot, Po_NaOH, Po_NaOHsonic)    !I/O

!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         !which contain control information, soil
                         !parameters, hourly weather data.
      IMPLICIT  NONE
      EXTERNAL ERROR
!     ------------------------------------------------------------------
      CHARACTER*6, PARAMETER :: ERRKEY = 'IPHEDL'
      INTEGER L

      REAL, DIMENSION(NL) :: P_RESIDUAL, P_TOTAL, TOTP,  
     &  Pi_BICARB, Pi_HCl, Pi_HClhot, Pi_NaOH, Pi_NaOHsonic, Pi_RESIN, 
     &  Po_BICARB, Po_HClhot, Po_NAOH, Po_NaOHsonic

!     ---------------------------------------------------------------------
!     If there are missing Hedley data, estimate the values from published
!       median values for tropical soils. 

!     Check for P_TOTAL missing -- 1st set to TOTP from 2nd tier soils  
!     then if still missing call error routine
      IF (P_TOTAL(L) < 1.E-6) THEN                              !13
        P_TOTAL(L) = TOTP(L)       !both in ppm

!       Can't have total P missing -- call error routine
!       Need to put new message in MODEL.ERR file for this  <<------chp
        IF (P_TOTAL(L) < 1.E-6) THEN
          CALL ERROR(ERRKEY,15,ERRKEY,0)
        ENDIF
      ENDIF

!     Estimate Hedley fractions based on the total P and reduce the
!     estimated amount from residual P (to keep the P balance OK).
!     These estimates are based on published values for tropical
!     soils (model of Daroub et al. 2003).
!     The original estimates were replaced by estimates from the
!     literature and own work by Else Bünemann.
      IF (Pi_RESIN(L) < 1.E-6) THEN
        Pi_RESIN(L)   = P_TOTAL(L) * 3.2 / 100.   !original: 0.4   !2
        P_RESIDUAL(L) = AMAX1 (0.0, P_RESIDUAL(L) - Pi_RESIN(L))
      ENDIF

      IF (Pi_BICARB(L) < 1.E-6) THEN
        Pi_BICARB(L)  = P_TOTAL(L) * 4.5 / 100.   !original: 3.3   !3
        P_RESIDUAL(L) = AMAX1 (0.0, P_RESIDUAL(L) - Pi_BICARB(L))
      ENDIF

      IF (Po_BICARB(L) < 1.E-6) THEN
        Po_BICARB(L)  = P_TOTAL(L) * 4.9 / 100.   !original: 2.1   !8
        P_RESIDUAL(L) = AMAX1 (0.0, P_RESIDUAL(L) - Po_BICARB(L))
      ENDIF

      IF (Pi_NaOH(L) < 1.E-6) THEN
        Pi_NaOH(L)    = P_TOTAL(L) * 13.0 / 100.  !original: 11.4  !4
        P_RESIDUAL(L) = AMAX1 (0.0, P_RESIDUAL(L) - Pi_NaOH(L))
      ENDIF

      IF (Po_NaOH(L) < 1.E-6) THEN
        Po_NaOH(L)    = P_TOTAL(L) * 19.6 / 100.  !original: 6.9   !9
        P_RESIDUAL(L) = AMAX1 (0.0, P_RESIDUAL(L) - Po_NaOH(L))
      ENDIF

      IF (Pi_NaOHsonic(L) < 1.E-6) THEN
        Pi_NaOHsonic(L) = P_TOTAL(L) * 21.7/ 100. !original: 1.9   !5
        P_RESIDUAL(L) = AMAX1 (0.0, P_RESIDUAL(L) - Pi_NaOHsonic(L))
      ENDIF

      IF (Po_NaOHsonic(L) < 1.E-6) THEN
        Po_NaOHsonic(L) = P_TOTAL(L) * 4.5 / 100. !original: 4.8   !10
        P_RESIDUAL(L) = AMAX1 (0.0, P_RESIDUAL(L) - Po_NaOHsonic(L))
      ENDIF

      IF (Pi_HCl(L) < 1.E-6) THEN
        Pi_HCl(L)     = P_TOTAL(L) * 8.2 / 100.   !original: 0.7   !6
        P_RESIDUAL(L) = AMAX1 (0.0, P_RESIDUAL(L) - Pi_HCl(L))
      ENDIF

      IF (Pi_HClhot(L) < 1.E-6) THEN
        Pi_HClhot(L)  = P_TOTAL(L) * 18.7 / 100.  !original: --    !7
!       P_RESIDUAL(L) = AMAX1 (0.0, P_RESIDUAL(L) - Pi_HClhot(L))
      ENDIF

      IF (Po_HClhot(L) < 1.E-6) THEN
        Po_HClhot(L)  = P_TOTAL(L) * 5.7 / 100.   !original: --    !7
!       P_RESIDUAL(L) = AMAX1 (0.0, P_RESIDUAL(L) - Po_HClhot(L))
      ENDIF

      IF (P_RESIDUAL(L) < 1.E-6) THEN                              !12
        P_RESIDUAL(L) = P_TOTAL(L) - Pi_RESIN(L) - Pi_BICARB(L) - 
     &    Po_BICARB(L) - Pi_NaOH(L) - Po_NaOH(L) - 
     &    Pi_NaOHsonic(L) - Po_NaOHsonic(L) - Pi_HCl(L) - 
     &    Pi_HClhot(L) - Po_HClhot(L)

!       If all Hedley fractions are estimated from P_TOTAL, then no more
!       than 31.5% of P_TOTAL is distributed, so P_RESIDUE cannot be <0.
!       If it yet happens (e.g. with some measured and some calculated
!       fractions), set P_RESIDUE to zero.
        P_RESIDUAL(L) = AMAX1(P_RESIDUAL(L), 0.)
      ENDIF

!-----------------------------------------------------------------------

      RETURN
      END SUBROUTINE Hedinit_org

!=======================================================================
