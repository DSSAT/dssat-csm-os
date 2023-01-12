!***********************************************************************
!  SOMLITPRINT_C, for CENTURY-based SOM/residue module of DSSAT.
!
!  Purpose: Prints daily output of SOM and litter variables in a
!           layer-by-layer structure (easy to read, but hard to import
!           in a spreadsheet; for that, see OPSOMLIT).
!
!  Revision history:
!  06/09/1999 AJG Written
!  01/01/2000 AJG Integrated the CENTURY-based and CERES-based SOM
!                  modules with CHP's modular structure.
!  11/05/2002 AJG Adapted for output from sequential runs.
!  01/30/2004 AJG Made P output option
!  08/30/2004 AJG Corrected some layout irregularities.
!
!  Called: CENTURY
!  Calls:  ERROR, GETLUN
!***********************************************************************
      SUBROUTINE SOMLITPRINT_C (CONTROL,
     &  DLAYR, LITC, LITE, METABC, METABE, N_ELEMS,       !Input
     &  NLAYR, SOM1C, SOM1E, SOM2C, SOM2E, SOM23E,        !Input
     &  SOM3C, SOM3E, SSOMC, SSOME, STRUCC, STRUCE,       !Input
     &  TLITC, TLITE, TMETABC, TMETABE, TSOM1C,           !Input
     &  TSOM1E, TSOM2C, TSOM2E, TSOM23E, TSOM3C,          !Input
     &  TSOM3E, TSOMC, TSOME, TSTRUCC, TSTRUCE)           !Input

!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         !which contain control information, soil
                         !parameters, hourly weather data.
      IMPLICIT  NONE
      EXTERNAL ERROR, YR_DOY, GETLUN
      SAVE
!     ------------------------------------------------------------------
!     The variable FIRSTTIME controls that it only prints the SOM 
!     initialization with a call from SOILNI_C on the first day and not 
!     again with a call from CENTURY, which does the litter initialization.
      LOGICAL FIRSTTIME

      CHARACTER*1 RNMODE
      CHARACTER*11 ERRKEY
      PARAMETER (ERRKEY = 'SOMLITPRINT')

      INTEGER DYNAMIC, ERRNUM, L, LUN, N_ELEMS, NLAYR, RUN, YRDOY
      INTEGER YEAR, DOY
      INTEGER, PARAMETER :: SRFC = 0    !, SOIL = 1, N = 1, P = 2

      REAL TLITC, TMETABC, TSOM1C, TSOM2C, TSOM3C, TSOMC, TSTRUCC

      REAL TMETABE(NELEM), TSOM2E(NELEM), TSOM23E(NELEM),
     &  TSOM3E(NELEM), TSOME(NELEM), TSTRUCE(NELEM)

      REAL TSOM1E(NELEM), TLITE(NELEM)
 
      REAL DLAYR(NL), LITC(0:NL), METABC(0:NL), SOM1C(0:NL), SOM2C(NL),
     &  SOM3C(NL), SSOMC(0:NL), STRUCC(0:NL)

      REAL LITE(0:NL,3), METABE(0:NL,3), SOM1E(0:NL,3), SOM2E(NL,3),
     &  SOM23E(NL,3), SOM3E(NL,3), SSOME(0:NL,3), STRUCE(0:NL,3)

      DATA FIRSTTIME /.TRUE./

      LOGICAL FEXIST

!     Constructed variables are defined in ModuleDefs.
      TYPE (ControlType) CONTROL

!     Transfer values from constructed data types into local variables.
      DYNAMIC = CONTROL % DYNAMIC
      RNMODE  = CONTROL % RNMODE
      RUN     = CONTROL % RUN
      YRDOY   = CONTROL % YRDOY
      CALL YR_DOY(YRDOY, YEAR, DOY)

!***********************************************************************
!***********************************************************************
!     INITIALIZATION
!***********************************************************************
      IF (DYNAMIC == SEASINIT) THEN
!     ------------------------------------------------------------------
!     Open output file.
      CALL GETLUN('SLDET', LUN)
      INQUIRE (FILE = 'SOMLIT1.OUT', EXIST = FEXIST)
      IF (FEXIST) THEN
        OPEN (UNIT = LUN, FILE = 'SOMLIT1.OUT', STATUS = 'OLD',
     &    IOSTAT = ERRNUM, POSITION = 'APPEND')
      ELSE
        OPEN (UNIT = LUN, FILE = 'SOMLIT1.OUT', STATUS = 'NEW',
     &    IOSTAT = ERRNUM)
      ENDIF

!     If the file can't be found, call an error.
      IF (ERRNUM /= 0) CALL ERROR (ERRKEY, ERRNUM, 'SOMLITPRINT', 0)

!     ----------------------------------------------------------------
!     SOM INITIALIZATION OUTPUT; LITTER WILL BE DONE BELOW.
!     ----------------------------------------------------------------
      IF (RUN == 1 .OR. INDEX ('QF',RNMODE) .LE. 0) THEN

!       Write output on SOM initialization.
        IF (FIRSTTIME) THEN
          WRITE (LUN,'(101(''*''))')
          WRITE (LUN,'(A)') 'SOM initialization.'
          WRITE (LUN,'(101(''*''))')

          DO L = 0, NLAYR
!           ----------------------------------------------------------
!           Surface layer; only C+N.
!           ----------------------------------------------------------
            IF (L == SRFC .AND. N_ELEMS == 1) THEN
!               Write headers of the various pools.
                WRITE (LUN,100) 'L', 'DLAYR', 'SSOMC', 'SOM1C',
     &            'SOM2C', 'SOM3C', 'LITC', 'STRUCC', 'METABC',
     &            'SSOME(N)', 'SOM1E(N)', 'SOM2E(N)', 'SOM3E(N)',
     &            'LITE(N)', 'STRUCE(N)', 'METABE(N)'

100             FORMAT (A2, 4X, A5, 2X, 14(A9, 2X))

!               Write SOM pool sizes (litter is still zero).
                WRITE (LUN,200) L, SOM1C(SRFC), LITC(SRFC), 
     &            STRUCC(SRFC), METABC(SRFC), SOM1E(SRFC,N),
     &            LITE(SRFC,N), STRUCE(SRFC,N), METABE(SRFC,N)

200             FORMAT (I2, 20X, F11.2, 22X, 3F11.2, 11X, F11.2, 22X,
     &          3F11.2)

!           ----------------------------------------------------------
!           Surface layer; C+N+P.
!           ----------------------------------------------------------
            ELSEIF (L == SRFC .AND. N_ELEMS == 2) THEN
!             Write headers of the various pools.
              WRITE (LUN,110) 'L', 'DLAYR', 'SSOMC', 'SOM1C',
     &          'SOM2C', 'SOM3C', 'LITC', 'STRUCC', 'METABC',
     &          'SSOME(N)', 'SOM1E(N)', 'SOM2E(N)', 'SOM3E(N)',
     &          'LITE(N)', 'STRUCE(N)', 'METABE(N)',
     &          'SSOME(P)', 'SOM1E(P)', 'SOM23E(P)',
     &          'LITE(P)', 'STRUCE(P)', 'METABE(P)'
  
110           FORMAT (A2, 4X, A5, 2X, 20(A9, 2X))
    
!             Write SOM pool sizes (litter is still zero).
              WRITE (LUN,210) L, SOM1C(SRFC), LITC(SRFC), 
     &          STRUCC(SRFC), METABC(SRFC), SOM1E(SRFC,N),
     &          LITE(SRFC,N), STRUCE(SRFC,N), METABE(SRFC,N),
     &          SOM1E(SRFC,P), LITE(SRFC,P),
     &          STRUCE(SRFC,P), METABE(SRFC,P)

210           FORMAT (I2, 20X, F11.2, 22X, 3F11.2, 11X, F11.2, 22X,
     &          3F11.2, 11X, F11.2, 11X, 3F11.2)

!           ----------------------------------------------------------
!           Soil layer; only C+N.
!           ----------------------------------------------------------
!           Soil layers; only C+N.
            ELSEIF (L /= SRFC .AND. N_ELEMS == 1) THEN
!             Write SOM and litter pool sizes (litter is still zero).
              WRITE (LUN,300) L, DLAYR(L), SSOMC(L), SOM1C(L),
     &          SOM2C(L), SOM3C(L), LITC(L), STRUCC(L), METABC(L),
     &          SSOME(L,N), SOM1E(L,N), SOM2E(L,N), SOM3E(L,N),
     &          LITE(L,N), STRUCE(L,N), METABE(L,N)
  
300           FORMAT (I2, F9.0, 14(F11.2))
  
!             Write pool size of total soil profile.
              IF (L == NLAYR) WRITE (LUN,400) TSOMC, TSOM1C, TSOM2C,
     &          TSOM3C, TLITC, TSTRUCC, TMETABC, TSOME(N), TSOM1E(N),
     &          TSOM2E(N), TSOM3E(N), TLITE(N), TSTRUCE(N),TMETABE(N)
  
400           FORMAT ('Soil', 7X, 14(F11.2))
  
!           ----------------------------------------------------------
!           Soil layer; C+N+P.
!           ----------------------------------------------------------
!           Soil layers; only C+N.
            ELSEIF (L /= SRFC .AND. N_ELEMS == 2) THEN
!             Write SOM and litter pool sizes (litter is still zero).
              WRITE (LUN,310) L, DLAYR(L), SSOMC(L), SOM1C(L),
     &          SOM2C(L), SOM3C(L), LITC(L), STRUCC(L), METABC(L),
     &          SSOME(L,N), SOM1E(L,N), SOM2E(L,N), SOM3E(L,N),
     &          LITE(L,N), STRUCE(L,N), METABE(L,N), 
     &          SSOME(L,P), SOM1E(L,P), SOM23E(L,P),
     &          LITE(L,P), STRUCE(L,P), METABE(L,P)

310           FORMAT (I2, F9.0, 20(F11.2))

!             Write pool size of total soil profile.
              IF (L == NLAYR) WRITE (LUN,410) TSOMC, TSOM1C, TSOM2C,
     &          TSOM3C, TLITC, TSTRUCC, TMETABC, TSOME(N), TSOM1E(N),
     &          TSOM2E(N), TSOM3E(N), TLITE(N), TSTRUCE(N),
     &          TMETABE(N), TSOME(P), TSOM1E(P), TSOM23E(P),
     &          TLITE(P), TSTRUCE(P),TMETABE(P)

410           FORMAT ('Soil', 7X, 20(F11.2))

            ENDIF   !End of IF block on L=SRFC ans N_ELEMS=1.
          ENDDO   !End of DO loop on L.

          FIRSTTIME = .FALSE.

!       --------------------------------------------------------------
!       LITTER INITIALIZATION OUTPUT; SOM ALREADY DONE ABOVE.
!       --------------------------------------------------------------
        ELSE   !IF .NOT. FIRSTTIME
          WRITE (LUN,'(/, 101(''*''))')
!         Write header of output on litter initialization.
          WRITE (LUN,500)
500       FORMAT ('Litter initialization ',
     &      '(for a sequential run, harvest residues of the previous ',
     &      'season are added here).')

          WRITE (LUN,600)
600       FORMAT ('If the litter has been incorporated, then SOM1 of',
     &      ' layer 0 will also be incorporated. *')
          WRITE (LUN,'(101(''*''))')

!         Set FIRSTTIME back to true, so that with a seasonal run the
!         output will start from the beginning.
!         FIRSTTIME = .TRUE.

          DO L = 0, NLAYR
!           ----------------------------------------------------------
!           Surface layer; only C+N.
!           ----------------------------------------------------------
            IF (L == SRFC .AND. N_ELEMS == 1) THEN
!             Write headers of the various pools.
              WRITE (LUN,100) 'L', 'DLAYR', 'SSOMC', 'SOM1C',
     &          'SOM2C', 'SOM3C', 'LITC', 'STRUCC', 'METABC',
     &          'SSOME(N)', 'SOM1E(N)', 'SOM2E(N)', 'SOM3E(N)',
     &          'LITE(N)', 'STRUCE(N)', 'METABE(N)'

!             Write SOM and litter pool sizes.
              WRITE (LUN,200) L, SOM1C(SRFC), LITC(SRFC),
     &          STRUCC(SRFC),  METABC(SRFC), SOM1E(SRFC,N),
     &          LITE(SRFC,N), STRUCE(SRFC,N), METABE(SRFC,N)

              IF (L == NLAYR) WRITE (LUN,410) TSOMC, TSOM1C, TSOM2C,
     &          TSOM3C, TLITC, TSTRUCC, TMETABC, TSOME(N), TSOM1E(N),
     &          TSOM2E(N), TSOM3E(N), TLITE(N), TSTRUCE(N),
     &          TMETABE(N), TSOME(P), TSOM1E(P), TSOM23E(P),
     &          TLITE(P), TSTRUCE(P), TMETABE(P)

!           ----------------------------------------------------------
!           Surface layer; C+N+P.
!           ----------------------------------------------------------
            ELSEIF (L == SRFC .AND. N_ELEMS == 2) THEN
!             Write headers of the various pools.
              WRITE (LUN, 110) 'L', 'DLAYR', 'SSOMC', 'SOM1C',
     &          'SOM2C', 'SOM3C', 'LITC', 'STRUCC', 'METABC',
     &          'SSOME(N)', 'SOM1E(N)', 'SOM2E(N)', 'SOM3E(N)',
     &          'LITE(N)', 'STRUCE(N)', 'METABE(N)',
     &          'SSOME(P)', 'SOM1E(P)', 'SOM23E(P)',
     &          'LITE(P)', 'STRUCE(P)', 'METABE(P)'

!             Write SOM and litter pool sizes.
              WRITE (LUN, 210) L, SOM1C(SRFC), LITC(SRFC),
     &          STRUCC(SRFC),  METABC(SRFC), SOM1E(SRFC,N),
     &          LITE(SRFC,N), STRUCE(SRFC,N), METABE(SRFC,N),
     &          SOM1E(SRFC,P), LITE(SRFC,P),
     &          STRUCE(SRFC,P), METABE(SRFC,P)

!           ----------------------------------------------------------
!           Soil layer; only C+N.
!           ----------------------------------------------------------
            ELSEIF (L /= SRFC .AND. N_ELEMS == 1) THEN
!             Write SOM and litter pool sizes.
              WRITE (LUN,300) L, DLAYR(L), SSOMC(L), SOM1C(L),
     &          SOM2C(L), SOM3C(L), LITC(L), STRUCC(L), METABC(L),
     &          SSOME(L,N), SOM1E(L,N), SOM2E(L,N), SOM3E(L,N),
     &          LITE(L,N), STRUCE(L,N), METABE(L,N)

!             Write pool size of total soil profile.
              IF (L == NLAYR) WRITE (LUN,400) TSOMC, TSOM1C, TSOM2C,
     &          TSOM3C, TLITC, TSTRUCC, TMETABC, TSOME(N), TSOM1E(N),
     &          TSOM2E(N), TSOM3E(N), TLITE(N), TSTRUCE(N),
     &          TMETABE(N)

!           ----------------------------------------------------------
!           Soil layer; C+N+P.
!           ----------------------------------------------------------
            ELSEIF (L /= SRFC .AND. N_ELEMS == 2) THEN
!             Write SOM and litter pool sizes.
              WRITE (LUN,310) L, DLAYR(L), SSOMC(L), SOM1C(L),
     &          SOM2C(L), SOM3C(L), LITC(L), STRUCC(L), METABC(L),
     &          SSOME(L,N), SOM1E(L,N), SOM2E(L,N), SOM3E(L,N),
     &          LITE(L,N), STRUCE(L,N), METABE(L,N), 
     &          SSOME(L,P), SOM1E(L,P), SOM23E(L,P),
     &          LITE(L,P), STRUCE(L,P), METABE(L,P)

!             Write pool size of total soil profile.
              IF (L == NLAYR) THEN
                WRITE (LUN,410) TSOMC, TSOM1C, TSOM2C, TSOM3C,
     &            TLITC, TSTRUCC, TMETABC, TSOME(N), TSOM1E(N),
     &            TSOM2E(N), TSOM3E(N), TLITE(N), TSTRUCE(N), 
     &            TMETABE(N), TSOME(P), TSOM1E(P), TSOM23E(P),
     &            TLITE(P), TSTRUCE(P), TMETABE(P)
              ENDIF   !End of IF block on L=NLAYR
            ENDIF   !End of IF block on L=SRFC and N_ELEMS=1.
          ENDDO   !End of DO loop on L.
        ENDIF   !End of IF block on FIRSTTIME
      ENDIF   !End of IF block on RUN=1 and INDEX(QF, RNMODE)

!***********************************************************************
!     OUTPUT (Daily, not just initialization!)
!***********************************************************************
      ELSEIF (DYNAMIC == OUTPUT) THEN
!     ------------------------------------------------------------------
!     Write header on daily output.
      WRITE (LUN,'(/, A, I4,1X,I3.3)') 'YEAR DOY = ', YEAR, DOY

      DO L = 0, NLAYR
!       --------------------------------------------------------------
!       Surface layer; only C+N.
!       --------------------------------------------------------------
        IF (L == SRFC .AND. N_ELEMS == 1) THEN
!         Write headers of the various pools.
          WRITE (LUN,100) 'L', 'DLAYR', 'SSOMC', 'SOM1C',
     &      'SOM2C', 'SOM3C', 'LITC', 'STRUCC', 'METABC',
     &      'SSOME(N)', 'SOM1E(N)', 'SOM2E(N)', 'SOM3E(N)',
     &      'LITE(N)', 'STRUCE(N)', 'METABE(N)'

!         Write SOM and litter pool sizes.
          WRITE (LUN,200) L, SOM1C(SRFC), LITC(SRFC), STRUCC(SRFC), 
     &      METABC(SRFC), SOM1E(SRFC,N), LITE(SRFC,N),
     &      STRUCE(SRFC,N), METABE(SRFC,N)

!       --------------------------------------------------------------
!       Surface layer; C+N+P.
!       --------------------------------------------------------------
        ELSEIF (L == SRFC .AND. N_ELEMS == 2) THEN
!         Write headers of the various pools.
          WRITE (LUN,110) 'L', 'DLAYR', 'SSOMC', 'SOM1C',
     &      'SOM2C', 'SOM3C', 'LITC', 'STRUCC', 'METABC',
     &      'SSOME(N)', 'SOM1E(N)', 'SOM2E(N)', 'SOM3E(N)',
     &      'LITE(N)', 'STRUCE(N)', 'METABE(N)',
     &      'SSOME(P)', 'SOM1E(P)', 'SOM23E(P)',
     &      'LITE(P)', 'STRUCE(P)', 'METABE(P)'

!         Write SOM and litter pool sizes.
          WRITE (LUN,210) L, SOM1C(SRFC), LITC(SRFC), STRUCC(SRFC), 
     &      METABC(SRFC), SOM1E(SRFC,N), LITE(SRFC,N),
     &      STRUCE(SRFC,N), METABE(SRFC,N), SOM1E(SRFC,P),
     &      LITE(SRFC,P), STRUCE(SRFC,P), METABE(SRFC,P)

!       --------------------------------------------------------------
!       Soil layer; only C+N.
!       --------------------------------------------------------------
        ELSEIF (L /= SRFC .AND. N_ELEMS == 1) THEN
!         Write SOM and litter pool sizes.
          WRITE (LUN,300) L, DLAYR(L), SSOMC(L), SOM1C(L),
     &      SOM2C(L), SOM3C(L), LITC(L), STRUCC(L), METABC(L),
     &      SSOME(L,N), SOM1E(L,N), SOM2E(L,N), SOM3E(L,N),
     &      LITE(L,N), STRUCE(L,N), METABE(L,N)
  
          IF (L == NLAYR) WRITE (LUN,400) TSOMC, TSOM1C, TSOM2C,
     &      TSOM3C, TLITC, TSTRUCC, TMETABC, TSOME(N), TSOM1E(N),
     &      TSOM2E(N), TSOM3E(N), TLITE(N), TSTRUCE(N),TMETABE(N)
  
!       --------------------------------------------------------------
!       Soil layer; C+N+P.
!       --------------------------------------------------------------
        ELSEIF (L /= SRFC .AND. N_ELEMS == 2) THEN
!         Write SOM and litter pool sizes.
          WRITE (LUN,310) L, DLAYR(L), SSOMC(L), SOM1C(L),
     &      SOM2C(L), SOM3C(L), LITC(L), STRUCC(L), METABC(L),
     &      SSOME(L,N), SOM1E(L,N), SOM2E(L,N), SOM3E(L,N),
     &      LITE(L,N), STRUCE(L,N), METABE(L,N), 
     &      SSOME(L,P), SOM1E(L,P), SOM23E(L,P),
     &      LITE(L,P), STRUCE(L,P), METABE(L,P)
  
          IF (L == NLAYR) WRITE (LUN,410) TSOMC, TSOM1C, TSOM2C,
     &      TSOM3C, TLITC, TSTRUCC, TMETABC, TSOME(N), TSOM1E(N),
     &      TSOM2E(N), TSOM3E(N), TLITE(N), TSTRUCE(N), TMETABE(N),
     &      TSOME(P), TSOM1E(P), TSOM23E(P), TLITE(P), 
     &      TSTRUCE(P),TMETABE(P)
        ENDIF   !End of IF block on L=SRFC ans N_ELEMS=1.
      END DO 

!***********************************************************************
!***********************************************************************
!     End of season
!***********************************************************************
      ELSEIF (DYNAMIC == SEASEND) THEN
!     ------------------------------------------------------------------
        CLOSE (LUN)

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF

!     ------------------------------------------------------------------
      RETURN
      END SUBROUTINE SOMLITPRINT_C

