!***********************************************************************
!  PARTIT_C, Subroutine for CENTURY-based SOM/residue module of DSSAT.
!
!  Purpose: This subroutine calculates the amount of carbon,
!           nutrients and lignin in newly added residues that goes
!           to the structural residue pool, and the amount that goes
!           to the metabolic residue pool.
!
!  Revision history:
!  ........       Parton et al.  Written for CENTURY model.
!  01/01/1999 AJG Revised and linked to DSSAT.
!  11/11/2002 AJG Corrected RESDAX for layer thickness.
!  03/26/2003 GH  Modified file name
!  01/19/2006 CHP Added checks to prevent negative N
!
!  Called: RPLACE_C, SENESADD_C, SoilCNPinit_C
!  Calls : 
!***********************************************************************
      SUBROUTINE PARTIT_C (
     &  AMINRL, CEDAM, CESTR, DLAYR, FRDAE,               !Input
     &  FRLRES, FRMETI, FRMETS, LAYER, N_ELEMS,           !Input
     &  RESC, RESCE, RESDAX,                              !Input

     &  DLTLIGC, DLTMETABC, DLTMETABE, DLTSTRUCC,         !Input/Output
     &  DLTSTRUCE, IMMOB, RESE,                           !Input/Output

     &  ADDMETABEFLAG, FRMETFLAG)                         !Output

!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         !which contain control information, soil
                         !parameters, hourly weather data.
!     NL defined in ModuleDefs.for

      IMPLICIT  NONE
      SAVE
!     ------------------------------------------------------------------
      LOGICAL ADDMETABEFLAG, FRMETFLAG

      INTEGER IEL, L, LAYER, N_ELEMS, AMFCOUNT
!      INTEGER, PARAMETER :: N = 1, P = 2
      INTEGER, PARAMETER :: SRFC = 0
      DATA AMFCOUNT /0/
      
      REAL ADDLIGC, ADDMETABC, ADDMETABE, ADDSTRUCC, ADDSTRUCE,
     &  DIRABS, FRMET, FRMETI, FRMETS, FRNRES, LNRES, 
     &  RESDAX

      REAL CEDAM(3), CESTR(3), DLAYR(NL), DLTLIGC(0:NL),
     &  DLTMETABC(0:NL), DLTSTRUCC(0:NL),
     &  FRDAE(3), FRLRES(0:NL), RESC(0:NL)

      REAL AMINRL(NL,3), DLTMETABE(0:NL,3),
     &  DLTSTRUCE(0:NL,3), IMMOB(0:NL,NELEM), RESCE(0:NL,3),
     &  RESE(0:NL,3)

!     ----------------------------------------------------------------
!     Copy LAYER back to L.
      L = LAYER

!     If no new residue has been applied, go back.
      IF (RESC(L) < 0.001) RETURN

      DO IEL = 1, NELEM
        IF (RESCE(L,IEL) > 0.0) THEN
!         Calculate the amount of E in the newly added residue.
          RESE(L,IEL) = RESC(L) / RESCE(L,IEL)
        ELSE
!         09/02/2005 CHP This was causing an imbalance when a residue 
!         with zero N was placed.  
!          RESE(L,IEL) = RESC(L) / 40.0  
          RESE(L,IEL) = 0.0 
        ENDIF
      ENDDO !IEL=N_ELEMS

!     ----------------------------------------------------------------
!     Direct absorption of N by the residue
!     CHP 2/3/06
!     This is not generic enough to use for P.  Need some equivalent
!     computation of DIRABS for P???
!     ----------------------------------------------------------------
!     For the SRFC layer there is no direct absorption as the contact
!     of the litter with the soil is limited.
      IF (L /= SRFC .AND. N_ELEMS > 0) THEN

!       NB: The original CENTURY model only deals with a 20-cm-thick 
!       soil layer, while DSSAT has many layers of variable thickness. 
!       The parameter RESDAX (a 'fixed' parameter) is in kg[N]/ha
!       and the comparison it is used for depends on the layer
!       thickness. It is therefore recalculated to a 1-cm-thick layer.

!       Calculate the direct absorption of E by the newly added
!       residue. This equals the fraction of E available for
!       absorption (FRDAE) times the amount of mineral E in the soil
!       layer (AMINRL times a reduction factor that depends on
!       the amount of residue present (RESC/RESDAX).
        DIRABS = FRDAE(N) * AMINRL(L,N) / DLAYR(L) * 
     &      AMIN1 ((RESC(L) / DLAYR(L)) / (RESDAX / 20.), 1.)

!       Calculate the C/E ratio after the direct absorption.
        IF (RESE(L,N) + DIRABS > 1.E-5) THEN
          RESCE(L,N) = RESC(L) / (RESE(L,N) + DIRABS)
        ELSE
          RESCE(L,N) = 0.
        ENDIF

!       Limit the RESCE after direct absorption so that the C/E ratio
!       of the residue does not become lower than CEDAM. The original
!       residue may have already a C/E ratio < CEDAM (eg. with high-N
!       legume residues). In that case take the original C/E as
!       critical limit.
        IF (RESE(L,N) > 1.E-5) THEN
          RESCE(L,N) = AMIN1 (RESC(L) / RESE(L,N),
     &      AMAX1 (CEDAM(N), RESCE(L,N)))
        ELSE
          RESCE(L,N) = AMAX1 (CEDAM(N), RESCE(L,N))
        ENDIF

!       Do the direct absorption.
        DIRABS = RESC(L) / RESCE(L,N) - RESE(L,N)

!       DIRABS should be >= 0 and =< AMINRL
        DIRABS = AMIN1 (DIRABS, AMINRL(L,N))
        DIRABS = AMAX1 (DIRABS, 0.)

!       Add the E from direct absorption to the E pool of the newly
!       added residue
        RESE(L,N) = RESE(L,N) + DIRABS

!       Get the cumulative direct absorption as if it were immobilization.
        IMMOB(L,N) = IMMOB(L,N) + DIRABS
      ENDIF   !End of IF block on L=SRFC

!     ------------------------------------------------------------------
!     Partition the residue into structural and metabolic.
!     ------------------------------------------------------------------
!     Calculate the N fraction of the newly added residue.
      IF (N_ELEMS > 0) THEN
        FRNRES = RESE(L,N) / (RESC(L) * 2.5)
      ELSE
        FRNRES = 0.
      ENDIF

!     Calculate the lignin/nitrogen ratio of newly added residue.
      IF (FRNRES > 0.0001) THEN
        LNRES = FRLRES(L) / FRNRES
      ELSE 
        LNRES = 0.0
      ENDIF

!     The fraction of the newly added residue that goes to metabolic
!     depends on the L/N ratio; the rest goes to structural.
      FRMET = FRMETI - FRMETS * LNRES

!     Make sure that the fraction of the newly added residue that goes
!     to structural is not less than its lignin fraction (thus: at
!     least all the lignin has to go to structural; the rest may go to
!     metabolic).
      IF ((1. - FRMET) < FRLRES(L)) FRMET = (1. - FRLRES(L))

!     Error check. Set a flag for printing a warning in NCHECK.
      IF (FRMET < 0.) THEN
        FRMETFLAG = .TRUE.
        FRMET = 0.20
      ELSEIF (FRMET > 1.) THEN
        FRMETFLAG = .TRUE.
        FRMET = 1.0
      ENDIF

!     Make sure at least 20% of the newly added residue goes to
!     metabolic.
      IF (FRMET < 0.20) FRMET = 0.20

!     Calculate the amount of C added to the metabolic pool.
      ADDMETABC = RESC(L) * FRMET
      IF (ADDMETABC < 0.) ADDMETABC = 0.
      DLTMETABC(L) = DLTMETABC(L) + ADDMETABC

!     Calculate the amount of C added to the structural pool.
      ADDSTRUCC = RESC(L) - ADDMETABC
      IF (ADDSTRUCC < 0.) ADDSTRUCC = 0.
      DLTSTRUCC(L) = DLTSTRUCC(L) + ADDSTRUCC

!     Calculate the amount of lignin C in the newly added residue.
      ADDLIGC = FRLRES(L) * RESC(L)
      DLTLIGC(L) = DLTLIGC(L) + ADDLIGC

!     Partition the newly added residue E over the structural and
!     metabolic pools. The C/E ratio of the structural pool is a
!     "fixed" parameter (from the SOMFX???.SDA file).
      DO IEL = 1, N_ELEMS
        ADDSTRUCE = ADDSTRUCC / CESTR(IEL)
        ADDMETABE = RESE(L,IEL) - ADDSTRUCE

!       With residues that have a very low E concentration, the
!       structural residue still gets the C/E ratio as set in
!       SOMFX???.SDA. This may result in ADDSTRUCE being greater than
!       RESE, and thus negative ADDMETABE. Set a flag for signaling this
!       and printing a warning in NCHECK.
        IF (ADDMETABE < -1.E-6) THEN
          IF (AMFCOUNT < 1) ADDMETABEFLAG = .TRUE.
          AMFCOUNT = AMFCOUNT + 1
          ADDMETABE = 0.0
          ADDSTRUCE = RESE(L,IEL)
        ENDIF

        DLTSTRUCE(L,IEL) = DLTSTRUCE(L,IEL) + ADDSTRUCE
        DLTMETABE(L,IEL) = DLTMETABE(L,IEL) + ADDMETABE
      END DO   !End of IEL loop.

!     Set back to zero. 
      RESC(L)   = 0.
      ADDMETABC = 0.
      ADDSTRUCC = 0.
      DO IEL = 1, N_ELEMS
        RESE(L,IEL) = 0.
        ADDMETABE   = 0.
        ADDSTRUCE   = 0. 
      END DO

!     ------------------------------------------------------------------
      RETURN
      END SUBROUTINE PARTIT_C


