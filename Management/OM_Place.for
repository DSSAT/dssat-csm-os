!**********************************************************************
!  OM_Place, Subroutine
!
!  Purpose: Deals with organic matter applications.
!
!  Revision   History
!  11/21/1995 PWW Written.
!  06/08/1999 CHP Modified for modular format
!  06/09/1999 AJG Completely revised the soil N and SOM module, and made
!               a new SOM module based on the CENTURY model.
!  06/09/1999 AJG Completely revised the soil N and SOM module, and made
!                 a new SOM module based on the CENTURY model.
!                 Also changed variable names:
!                 OLD      NEW                OLD      NEW
!                 ------   ------             ------   ------ 
!                 AMTRES   CUMRES             RESCOD   RESTYP
!                 DEPRES   RESDEP             RESDEP   RESDEPTH 
!                 METRES   RESTYPE            RESNIT   RESECONC
!                 RESAMT   TOPRES             RINP     RIP
!                 RESAPP   RESSOL, RESSRF     RMET     RESMET
!  03/17/2000 GH  Incorporated in CROPGRO
!  07/01/2000 GH  Changed variable names RESDEP to RESDEPTH;
!                 DEPRES to RESDEP
!  04/20/2002 GH  Adjusted for crop rotations
!  06/11/2002 GH  Modified for Y2K
!  11/11/2002 AJG Added DLAYR to the PARTIT_C parameter string.
!  03/12/2003 CHP Changed senescence variable to composite (SENESCE)
!                   as defined in ModuleDefs.for
!  05/20/2003 CHP Added variable MULCHMASS, which is the surface residue
!               used in soil evaporation and soil temperature routines
!               (was called RESLEFT) and MULCHN (was RESLEFTN).
!               Also added MULCHCOVER, the fractional coverage of residue.
!  08/12/2003 CHP Added I/O error checking
!  09/29/2003 AJG Reorganized for incorporating the P module. Combined
!                 with subroutine CHEM_APP. Split off FERTILIZERTYPE_C
!                 as a separate subroutine.
!  10/28/2004 CHP Added fix to allow multiple applications in one day.
!  11/14/2003 CHP Added call to WARNING for high residue N concentrataions.
!  12/17/2003 AJG Renamed all the CH_ and CHEM_ variables to P_ variables.
!  08/05/2004 AJG Corrected format 400 from 0.04 to 4.0
!  01/22/2005 CHP Merged RPLACE and RPLACE_C into OM_Place and moved to 
!                     management module.
!  01/22/2005 CHP Added phosphorus content to residue
!  02/22/2005 CHP Move senescence and harvest residue out of this routine.  This should
!                 be just for applied residue.
!  04/30/2008 CHP Changed units for SCN, SCP, RCN, RCP to %



!               Note comments in code about previous crop code, etc.
!               Develop mechanism to handle surface residue   
!
!     Note:  TFOM should include initial root mass, but doesn't seem to.
!         Need to check this out.     chp 12/9/2003
!
!-----------------------------------------------------------------------
!  Called : MgmtOps
!  Calls  : ERROR, FIND, YR_DOY
!=======================================================================
!     TO DO: 
!     1. Need to add IRESI = "F" option
!=======================================================================

      SUBROUTINE OM_Place (CONTROL, ISWITCH, 
     &    DLAYR, NLAYR, YRPLT,                    !Input
     &    OMAData)                                !Output
!-----------------------------------------------------------------------
      USE ModuleDefs  
      USE Interface_IpSoil
      IMPLICIT  NONE
      EXTERNAL ERROR, FIND, TIMDIF, WARNING, YR_DOY
      SAVE

      CHARACTER*1 IRESI, ISWNIT, ISWPHO, RNMODE
      CHARACTER*5 RESTYPE  
      CHARACTER*5 RESTYP(NAPPL), RESMET(NAPPL)
      CHARACTER*6 ERRKEY, SECTION
      CHARACTER*30 FILEIO
      CHARACTER*78 MSG(10)
      CHARACTER*90 CHAR
      PARAMETER (ERRKEY = 'RPLACE')

      INTEGER, PARAMETER :: SRFC = 0  !Surface layer

      INTEGER DAP, DYNAMIC, ERRNUM, FOUND, I, IDATE,
     &  L, LINC, LNUM, LUNIO, MULTI, 
     &  NLAYR, NRESAP, NRESDL, RUN, TIMDIF, YR,
     &  YRDIF, YRDOY, YRPLT, YRSIM

      INTEGER RESDAY(NAPPL)

      REAL DEPTH, DRESMG, FR, HOLD, SCN, SCP
!      REAL PRCEL, PRCHO, PRLIG
      REAL RESDEPTH, RESNIT, ResPho, RESSOL, RESSRF
      REAL RESSRFN, RESSOLN, RESSRFP, RESSOLP
      REAL ResLign, ResSolLig, ResSrfLig

      REAL DLAYR(NL)

!     Read from FILEX:
      REAL RESDEP(NAPPL), RESIDUE(NAPPL), RESN(NAPPL)
      REAL RESP(NAPPL), RIP(NAPPL), RESLIGNIN(NAPPL)

!     Organic matter applied today:
      INTEGER RESDAT, NAPRes
      REAL ResMixPerc   !Percent mixing rate for SOM module

      REAL, DIMENSION(0:NL) :: ResWt        !kg[dry matter]/ha/d
      REAL, DIMENSION(0:NL,NELEM) :: ResE   !kg[E]/ha/d (E=N, P, S,..)
      REAL, DIMENSION(0:NL) :: ResCar       !kg[carbohydrates]/ha/d
      REAL, DIMENSION(0:NL) :: ResCel       !kg[cellulose]/ha/d
      REAL, DIMENSION(0:NL) :: ResLig       !kg[lignin]/ha/d
      REAL  CumResWt                        !cumul. kg[dry matter]/ha
      REAL, DIMENSION(NELEM) :: CumResE     !cumulative kg[E]/ha

      LOGICAL P_msg, N_msg  !, K_msg
      DATA N_msg /.FALSE./
      DATA P_msg /.FALSE./
!     DATA K_msg /.FALSE./

!     Constructed variables are defined in ModuleDefs.
      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH
      TYPE (OrgMatAppType) OMAData

      DYNAMIC = CONTROL % DYNAMIC
      YRDOY   = CONTROL % YRDOY
      IRESI   = ISWITCH % IRESI
      ISWNIT  = ISWITCH % ISWNIT
      ISWPHO  = ISWITCH % ISWPHO

!-----------------------------------------------------------------------
!     Residue types                                                    
!     1 = Crop residue                                                 
!     2 = Green manure                                                 
!     3 = Barnyard manure                                              
!     4 = Liquid manure                                                
!                                                                     
!     Residue Application (IRESI)                                             
!     A = Auto residue for multiyear                                   
!     N = No residue                                                   
!     R = On reported dates                                            
!     D = As reported, in DAP                                          
!     F = Auto, with fixed amounts 

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC == SEASINIT) THEN
!     ------------------------------------------------------------------
      FILEIO  = CONTROL % FILEIO
      LUNIO   = CONTROL % LUNIO
      RUN     = CONTROL % RUN
      MULTI   = CONTROL % MULTI
      RNMODE  = CONTROL % RNMODE
      YRDIF   = CONTROL % YRDIF
      YRSIM   = CONTROL % YRSIM

      RESLIGNIN = 0.0

!     ------------------------------------------------------------------
!     Read RPLACE data from FILEIO
!     ------------------------------------------------------------------
      OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT = ERRNUM)
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY, ERRNUM, FILEIO, 0)
      LNUM =0

!     ------------------------------------------------------------------
!     Find AUTOMATIC MANAGEMENT Section
!     ------------------------------------------------------------------
      SECTION = '!AUTOM'
      CALL FIND (LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
!     If the residue section can't be found, call an error, or else
!     read the input data.
      IF (FOUND == 0) THEN
        CALL ERROR (SECTION, 42, FILEIO, LNUM)
      ELSE
!       Read the number of days after planting that residues are
!       applied (with automatic application) and the residue
!       application depth.
        READ (LUNIO,'(///,20X,I6,F6.0)', IOSTAT = ERRNUM) 
     &        NRESDL, DRESMG
        LNUM = LNUM + 4
        IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY, ERRNUM, FILEIO, LNUM)
      ENDIF

!     ------------------------------------------------------------------
!     Find RESIDUE Section
!     ------------------------------------------------------------------
!     Only read the residue section if residues are going to be
!     applied.
      IF (IRESI .NE. 'N') THEN
        SECTION = '*RESID'

!       Find the line number from where to start reading.
        CALL FIND (LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC

!       If the residue section can't be found, call an error, or else
!       read the input data.
        IF (FOUND == 0) THEN
          CALL ERROR (SECTION, 42, FILEIO, LNUM)
        ELSE
!         Initialize the number of residue applications to be done.
          NRESAP = 0
          DO I = 1, NAPPL
!           Read the residue application parameters.
            READ (LUNIO,'(3X,I7,1X,A90)',ERR=87,END=87) RESDAY(I), CHAR
            LNUM = LNUM + 1

            READ (CHAR, '(A5,3F6.0,6X,2F6.0,1X,A5)', IOSTAT = ERRNUM) 
     &        RESTYP(I), RESIDUE(I), RESN(I), RESP(I), 
     &        RIP(I), RESDEP(I), RESMET(I)
            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY, ERRNUM, FILEIO, LNUM)

            IF (RIP(I) .LT. 0.001) THEN
              RIP(I) = 0.0
            ENDIF

!           If the residue code does not start with 'RE'.
            IF (RESTYP(I)(1:2) .NE. 'RE') RESTYP(I)(1:2) = 'RE'
            NRESAP = NRESAP + 1
          ENDDO

!         Continue here after jumping out of the DO loop with an error
!         (thus the end of the residue section was reached).
87        CONTINUE
        ENDIF   !End of IF block on FOUND=0.

        DO I = 1, NRESAP
!         Get default values if lignin content not specified.
          IF (RESLIGNIN(I) < 1.E-4) THEN
            CALL IPSOIL(CONTROL, RESTYPE=RESTYP(I),PSLIG=RESLIGNIN(I))
          ENDIF

!         Get default values if N not specified.
          IF (RESN(I) .LT. 1.E-3) THEN
            CALL IPSOIL(CONTROL, RESTYPE=RESTYP(I), SCN=SCN)
            RESN(I) = SCN !%
            IF (.NOT. N_msg .AND. ISWNIT == 'Y') THEN
              MSG(1) = "Default N concentration used for one or " //
     &             "more residue applications"
              CALL WARNING(1,ERRKEY,MSG)
              N_msg = .TRUE.
            ENDIF
          ENDIF

!         Get default values if P not specified.
          IF (RESP(I) .LT. 1.E-3) THEN
            CALL IPSOIL(CONTROL, RESTYPE=RESTYP(I), SCP=SCP)
            RESP(I) = SCP !%
            IF (.NOT. P_msg .AND. ISWPHO .NE. 'N') THEN
              MSG(1) = "Default P concentration used for one or " //
     &             "more residue applications"
              CALL WARNING(1,ERRKEY,MSG)
              P_msg = .TRUE.
            ENDIF
          ENDIF
        ENDDO
      ENDIF   !End of IF block on IRESI.

!     Close the input file.
      CLOSE (LUNIO)

!     Check for invalid residue option.
      IF (INDEX('ARDN',IRESI) .EQ. 0) THEN
        WRITE(MSG(1),400) IRESI
        WRITE(MSG(2),410) 
        CALL WARNING(2, ERRKEY, MSG)
      ENDIF

  400 FORMAT(
     &    ' Warning: The residue option, "',A1,'" is not currently ')
  410 FORMAT(' supported.  No residue applications were added.')

!     ------------------------------------------------------------------
!     Initialize the number of residue applications done.
      NAPRes  = 0 

!     Initialize the cumulative residue applications to zero. 
      CumResWt    = 0.0
      CumResE(N)  = 0.0
      CumResE(P)  = 0.0

!     ------------------------------------------------------------------
      IF (RUN .NE. 1 .AND. INDEX('QF',RNMODE) .GT. 0) THEN
!       Increment day of year and adjust all date values for sequenced
!       runs (Moved from DATECS). Do this only if the number of
!       residue applications to be done is > 0, and the residue
!       application method is 'As reported', and the residue-application
!       day is before the start day of the new season.
        IF (NRESAP .GT. 0 .AND. RESDAY(1) .LT. YRSIM .AND. 
     &      IRESI .NE. 'D') THEN
          DO I = 1, NRESAP
            CALL YR_DOY(RESDAY(I),YR,IDATE)
            RESDAY(I) = (YR + YRDIF) * 1000 + IDATE
          ENDDO
        ENDIF
      ENDIF   

C-----------------------------------------------------------------------
      IF (MULTI .GT. 1 .AND. NRESAP .GT. 0 .AND. IRESI .NE. 'D') THEN
C-----------------------------------------------------------------------        
!       Each season of a seasonal run, the YR number is increased by
!       one. The residue application dates thus have to be updated.
C-----------------------------------------------------------------------
        DO I = 1, NRESAP
          CALL YR_DOY (RESDAY(I), YR, IDATE)
          RESDAY(I) = (YR + MULTI - 1) * 1000 + IDATE
        ENDDO
      ENDIF

!***********************************************************************
!***********************************************************************
!     DAILY RATE CALCULATIONS 
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE .OR. DYNAMIC .EQ. SEASEND) THEN
!     ------------------------------------------------------------------
!     Set back to zero every time the rate section is entered.
      ResWt  = 0.0
      ResE   = 0.0
      ResE   = 0.0
      ResCar = 0.0
      ResCel = 0.0
      ResLig = 0.0

      RESSOL  = 0.0
      RESSOLN = 0.0
      RESSOLP = 0.0
      ResSolLig = 0.0

      RESSRF  = 0.0
      RESSRFN = 0.0
      RESSRFP = 0.0
      ResSrfLig = 0.0

!     ----------------------------------------------------------------
      IF (NRESAP .GT. 0) THEN
        DAP = MAX(0, TIMDIF(YRPLT,YRDOY))
        DO I = 1, NRESAP

              !Residue application on a specified date.
          IF ((IRESI .EQ. 'R' .AND. YRDOY .EQ. RESDAY(I)) .OR.
              !Residue application on a specified day after planting.
     &        (IRESI .EQ. 'D' .AND. RESDAY(I) .NE. 0 .AND. 
     &                                          DAP .EQ. RESDAY(I)) .OR.
              !Residue application on planting day
     &        (IRESI .EQ. 'D' .AND. RESDAY(I) .EQ. 0 .AND. 
     &                                            YRDOY .EQ. YRPLT) .OR.
              !Automatic multiyear residue application.
     &        (IRESI .EQ. 'A' .AND. DAP .EQ. NRESDL)) THEN

            !Apply residue today
            !-------------------
!           Increase number of residue applications done.
            NAPRes  = NAPRes + 1
            ResDat = YRDOY

!           If the residue depth is zero, the incorporation percentage
!           should also be zero.
            SELECT CASE(IRESI)
            CASE ('R', 'D')
              IF (RESDEP(I) .LT. 0.001) RIP(I) = 0.
            CASE ('A')
              IF (DRESMG .LT. 0.001) RIP(I) = 0.
            END SELECT

!           Divide the residue over soil and surface.
            RESSOL  = RESSOL + RESIDUE(I) * RIP(I) / 100.
            RESSRF  = RESSRF + RESIDUE(I) * (1.0 - RIP(I) / 100.)

!           Additional N added: 
            RESNIT  = RESN(I) / 100. * RESIDUE(I)    !kg[N]/ha
            RESSOLN = RESSOLN + RESNIT * RIP(I) / 100.
            RESSRFN = RESSRFN + RESNIT * (1.0 - RIP(I) / 100.)

!           Additional P added: 
            ResPho  = RESP(I) / 100. * RESIDUE(I)    !kg[N]/ha
            RESSOLP = RESSOLP + ResPho * RIP(I) / 100.
            RESSRFP = RESSRFP + ResPho * (1.0 - RIP(I) / 100.)

!           Additional lignin added: 
            ResLign   = RESLIGNIN(I) * RESIDUE(I)
            ResSolLig = ResSolLig + ResLign * RIP(I) / 100.
            ResSrfLig = ResSrfLig + ResLign * (1.0 - RIP(I) / 100.)

!           Set the residue incorporation depth.
            RESDEPTH  = RESDEP(I)

!           Set the type of residue.
            RESTYPE = RESTYP(I)
            !READ(RESTYPE,'(3X,I2)') OMATYP

!         If it is not yet a residue application day, jump out of loop 
          ELSEIF ((IRESI .EQ. 'R' .AND. RESDAY(I) .GT. YRDOY) .OR.
     &            (IRESI .EQ. 'D' .AND. RESDAY(I) .GT. DAP)) THEN
            EXIT 
          ENDIF   !End of IF block on YRDOY and RESDAY
        END DO   !End of loop on NRESAP.
      ENDIF
!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!-----------------------------------------------------------------------

!***********************************************************************
!***********************************************************************
!   PERFORM ADDITIONAL CALCULATIONS FOR INITIALIZATION AND RATE SECTIONS
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT .OR. DYNAMIC .EQ. RATE .OR. 
     &        DYNAMIC .EQ. SEASEND) THEN
!-----------------------------------------------------------------------
!     C and N contribution of the applied residue.
!     ----------------------------------------------------------------
!     Surface residues
      ResWt(0)  = RESSRF
      ResE(0,N) = RESSRFN
      ResE(0,P) = RESSRFP
      ResLig(0) = ResSrfLig

!     Soil-deposited residues
      IF (RESSOL .GT. 0.001 .AND. RESDEPTH .GT. 0.001) THEN 
!       Set the starting depth for counting the soil layers to zero.
        DEPTH = 0.0
        DO L = 1, NLAYR
          HOLD  = DEPTH               !Depth to top of layer
          DEPTH = DEPTH + DLAYR(L)    !Depth to bottom of layer
          IF (RESDEPTH .LE. DEPTH) THEN
!           FR = fraction of the residue added to this layer
            FR = (RESDEPTH - HOLD) / RESDEPTH
          ELSE
            FR = DLAYR(L) / RESDEPTH
          ENDIF   !End of IF block on RESDEPTH =< DEPTH.

!!         Add the residue to the carbohydrate, cellulose and lignin
!!         pools of the layer.
!          ADD = RESSOL * FR
          RESWT(L) = RESSOL * FR

!         Add the residue to the rate variables.
          ResE(L,N) = ResE(L,N) + RESSOLN * FR        !kg[N]/ha
          ResE(L,P) = ResE(L,P) + RESSOLP * FR        !kg[P]/ha
          ResLig(L) = ResLig(L) + ResSolLig * FR      !lignin
!          ResCar(L) = ResCar(L) + ADD * PRCHO         !carbohydrate
!          ResCel(L) = ResCel(L) + ADD * PRCEL         !cellulose

!         If there are no more soil layers over which to distribute
!         the residue, jump out of the DO loop. 
          IF (DEPTH >= RESDEPTH) EXIT
        ENDDO   !End of loop on soil layers.
      ELSE
        RESDEPTH = 0.0
        ResMixPerc = 0.0
      ENDIF   !End of IF block on RESSOL and RESDEPTH.

      CumResWt = CumResWt + RESSOL + RESSRF
      CumResE(N)  = CumResE(N) + RESSOLN + RESSRFN
      CumResE(P)  = CumResE(P) + RESSOLP + RESSRFP

!***********************************************************************
!***********************************************************************
!     END OF SECOND DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!-----------------------------------------------------------------------
      OMAData % NAPRes     = NAPRes
      OMAData % RESDAT     = RESDAT
      OMAData % ResDepth   = RESDEPTH
      OMAData % ResTYPE    = RESTYPE
      OMAData % ResMixPerc = ResMixPerc

      OMAData % ResWt = ResWt
      OMAData % ResE  = ResE
      OMAData % ResLig= ResLig

      OMAData % CumResWt   = CumResWt
      OMAData % CumResE(N) = CumResE(N)
      OMAData % CumResE(P) = CumResE(P)

!======================================================================
!     Organic Matter Application data
!      TYPE OrgMatAppType
!        INTEGER NAPRes, ResDat, ResDepth
!        CHARACTER (len=5) RESTYPE
!        REAL ResMixPerc   !Percent mixing rate for SOM module
!
!        REAL, DIMENSION(0:NL) :: ResWt        !kg[dry matter]/ha/d
!        REAL, DIMENSION(0:NL) :: ResLig       !kg[lignin]/ha/d
!        REAL, DIMENSION(0:NL,NELEM) :: ResE   !kg[E]/ha/d (E=N, P, ..)
!        REAL  CumResWt                        !cumul. kg[dry matter]/ha
!        REAL, DIMENSION(NELEM) :: CumResE     !cumulative kg[E]/ha
!      END TYPE OrgMatAppType
!======================================================================

  300 RETURN
      END SUBROUTINE OM_Place

