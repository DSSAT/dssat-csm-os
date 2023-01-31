C= CSP_IPDMND_OUT =======================================================
C
C  CSP_IPDMND_OUT, Subroutine, O.H. Daza
C-----------------------------------------------------------------------
C  Writes variables read from crop or species specific data file.
!  Follows the same structure as IPDMND_SC for output.
C
C-----------------------------------------------------------------------
!  11/14/2001 O.H. Daza wrote for the sugarcane model
C  07/26/2004 CHP Removed variables which were not being used
C-----------------------------------------------------------------------
!  Called:      IPDMND_SC.for
C=======================================================================

      SUBROUTINE CSP_IPDMND_OUT(
     &  ECONO, FILECC, FILEIO,                                  !Input
     &  FINREF, FRLFF, FRLFMX, FRSTMF, NMOBMX, NVSMOB, PROLFF,  !Input
     &  PROLFI, PRORTF, PRORTI, PROSTF, PROSTI, PROSUF, PROSUI, !Input 
     &  RCH2O, RLIG, RLIP, RMIN, RNO3C, RPRO, ROA, SLAMAX,      !Input
     &  SLAMIN, SLAPAR, SLAREF, SLAVAR, SIZELF, SIZREF, TURSLA, !Input
     &  VSSINK, XLEAF, XSLATM, XVGROW, YLEAF, YSLATM, YSTEM,    !Input
     &  YVREF, XFRRT, YFRRT, XFRSU, YFRSU, GAMMA)               !Input

!     &  ECONO, FILECC, FILEGC, FILEIO,                          !Input
!-----------------------------------------------------------------------
      IMPLICIT NONE
      EXTERNAL GETLUN
!-----------------------------------------------------------------------
!       CHARACTER*3   TYPSDT
      CHARACTER*6   ERRKEY
      PARAMETER (ERRKEY = 'DEMAND')
      CHARACTER*6   SECTION
      CHARACTER*6   ECONO ! , ECOTYP
      CHARACTER*30  FILEIO
!       CHARACTER*80  C80
      CHARACTER*92  FILECC    ! , FILEGC
!       CHARACTER*255 C255

!       INTEGER LUNCRP, LUNIO, LUNECO, ERR, LNUM, FOUND, ISECT
C      PARAMETER (LUNIO = 21)
      INTEGER II !, WLUN

      REAL FINREF, FRLFF, FRLFMX, FRSTMF, NMOBMX, NVSMOB,
     &  PROLFF, PROLFI, PRORTF, PRORTI, PROSTF, PROSTI, PROSUI, PROSUF,
     &  RCH2O, RLIG, RLIP, RMIN, RNO3C, RPRO, ROA,
     &  SIZELF, SIZREF, SLAMAX, SLAMIN, SLAPAR, SLAREF, SLAVAR, !SLOSUM,
     &  TURSLA, VSSINK

      REAL XVGROW(6), YVREF(6)
      REAL XSLATM(10), YSLATM(10)
      REAL XLEAF(25), YLEAF(25), YSTEM(25)

! New
      INTEGER OpenStatus
      REAL GAMMA, XFRRT(4), YFRRT(4), XFRSU(4), YFRSU(4)

! Open file to write results from CSP_IPDMND

!      CALL GETLUN('WORK.OUT',WLUN)
!      OPEN(UNIT = WLUN, FILE = "WORK.OUT", STATUS = "UNKNOWN",
!     &   ACTION = "WRITE", POSITION = "APPEND", IOSTAT = OpenStatus)

!      WRITE(WLUN,'(1X, "")')   !chp for portability
!      WRITE(WLUN,'(1X, "")')
!      WRITE(WLUN,'(1X,"RESULTS FROM IPDMND_SC.for")')
!      WRITE(WLUN,'(1X,"--------------------------")')
!      WRITE(WLUN,'(1X, "")')

!-----------------------------------------------------------------------
!      OPEN (LUNIO, FILE = FILEIO, STATUS = 'UNKNOWN', IOSTAT=ERR)
!      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,0)
!-----------------------------------------------------------------------
C    Find and Read Field Section from FILEIO - previously read in IPIBS
!       Look for the second section header beginning with '*CULTI'
C-----------------------------------------------------------------------
!      LNUM = 1
      SECTION = '*CULTI'
!      CALL FIND(LUNIO, SECTION, LNUM, FOUND)
!      IF (FOUND .EQ. 0) THEN
!        CALL ERROR(ERRKEY, 1, FILEIO, LNUM)
!      ENDIF
!      CALL FIND(LUNIO, SECTION, LNUM, FOUND)
!      IF (FOUND .EQ. 0) THEN
!        CALL ERROR(ERRKEY, 1, FILEIO, LNUM)
!      ELSE
!        READ(LUNIO,'(24X,A6,48X,2F6.0)') ECONO, SLAVAR, SIZELF
!      ENDIF

!      CLOSE (LUNIO)

!      WRITE(WLUN,'(1X,"*** FILEIO : ",A12)') FILEIO
!      WRITE(WLUN,'(1X, "")')
!      WRITE(WLUN,'(1X,"SECTION: ",A6)') SECTION
!      WRITE(WLUN,'(1X,"ECONO  : ",A6)') ECONO
!      WRITE(WLUN,'(1X,"SLAVAR : ",F6.1)') SLAVAR 
!      WRITE(WLUN,'(1X,"SIZELF : ",F6.1)') SIZELF
      
!-----------------------------------------------------------------------
!     Read in values from input file, which were previously input
!       in Subroutine IPCROP.
!-----------------------------------------------------------------------
!      LUNCRP = 10
!-----------------------------------------------------------------------
!      OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD', IOSTAT=ERR)
!      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,0)
!-----------------------------------------------------------------------
!    Find and Read Respiration Section
!-----------------------------------------------------------------------
!      LNUM = 1
      SECTION = '!*RESP'
!      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
!      IF (FOUND .EQ. 0) THEN
!        CALL ERROR(ERRKEY, 1, FILECC, LNUM)
!      ELSE
!        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
!        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
!        READ(C80,'(F6.0)',IOSTAT=ERR) RNO3C
!        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

!        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
!        READ(C80,'(5F6.0)',IOSTAT=ERR) RCH2O,RLIP,RLIG,ROA,RMIN
!        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
!      ENDIF

!      WRITE(WLUN,'(1X, "")')
!      WRITE(WLUN,'(1X,"*** FILECC : ",A80)') FILECC
!      WRITE(WLUN,'(1X, "")')
!      WRITE(WLUN,'(1X,"SECTION: ",A6)') SECTION
!      WRITE(WLUN,'(1X,"RNO3C  : ",G12.4)') RNO3C
!      WRITE(WLUN,'(1X,"RPRO   : ",G12.3)') RPRO
!      WRITE(WLUN,'(1X,"RCH2O  : ",F6.3)') RCH2O
!      WRITE(WLUN,'(1X,"RLIP   : ",F6.3)') RLIP
!      WRITE(WLUN,'(1X,"RLIG   : ",F6.3)') RLIG
!      WRITE(WLUN,'(1X,"ROA    : ",F6.3)') ROA
!      WRITE(WLUN,'(1X,"RMIN   : ",F6.2)') RMIN

!-----------------------------------------------------------------------
!    Find and Read Plant Composition Section
!-----------------------------------------------------------------------
      SECTION = '!*PLAN'
!      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
!      IF (FOUND .EQ. 0) THEN
!        CALL ERROR(ERRKEY, 1, FILECC, LNUM)
!      ELSE
!        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
!        READ(C80,'(F6.0,6X,2F6.0,6X,F6.0)', IOSTAT=ERR)
!     &          PROLFI, PROLFF, PROSTI, PROSTF
!        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

!        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
!        READ(C80,'(F6.0,6X,F6.0)', IOSTAT=ERR) PRORTI, PRORTF
!        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

!        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
!        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
!        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
!        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
!        READ(C80,'(24X,F6.0)',IOSTAT=ERR) PLIGSD
!        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

!        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
!        READ(C80,'(24X,F6.0)',IOSTAT=ERR) POASD
!        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

!        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
!        READ(C80,'(24X,F6.0)',IOSTAT=ERR) PMINSD
!        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
!      ENDIF

!      WRITE(WLUN,'(1X, "")')
!      WRITE(WLUN,'(1X,"SECTION: ",A6)') SECTION
!      WRITE(WLUN,'(1X,"PROLFI : ",F6.3)') PROLFI
!      WRITE(WLUN,'(1X,"PROLFF : ",F6.3)') PROLFF
!      WRITE(WLUN,'(1X,"PROSTI : ",F6.3)') PROSTI
!      WRITE(WLUN,'(1X,"PROSTF : ",F6.3)') PROSTF
!      WRITE(WLUN,'(1X,"PRORTI : ",F6.3)') PRORTI
!      WRITE(WLUN,'(1X,"PRORTF : ",F6.3)') PRORTF
!      WRITE(WLUN,'(1X,"PROSUI : ",F6.3)') PROSUI
!      WRITE(WLUN,'(1X,"PROSUF : ",F6.3)') PROSUF

!-----------------------------------------------------------------------
!    Find and Read Seed Composition Section
!-----------------------------------------------------------------------
!      SECTION = '!*SEED'
!      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
!      IF (FOUND .EQ. 0) THEN
!        CALL ERROR(ERRKEY, 1, FILECC, LNUM)
!      ELSE
!        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
!        READ(C80,'(4F6.0)',IOSTAT=ERR) LIPTB, LIPOPT, SLOSUM, CARMIN
!        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
!          SLOSUM = SLOSUM / 100.0
!      ENDIF

!-----------------------------------------------------------------------
!    Find and Read Carbon and Nitrogen Mining Section
!-----------------------------------------------------------------------
!      SECTION = '!*CARB'
!      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
!      IF (FOUND .EQ. 0) THEN
!        CALL ERROR(ERRKEY, 1, FILECC, LNUM)
!      ELSE
!        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
!        READ(C80,'(18X,3F6.0)',IOSTAT=ERR) NMOBMX, NVSMOB, NRCVR
!        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
!      ENDIF

      SECTION = '!*CARB'
!      WRITE(WLUN,'(1X, "")')
!      WRITE(WLUN,'(1X,"SECTION: ",A6)') SECTION
!      WRITE(WLUN,'(1X,"NMOBMX : ",F6.3)') NMOBMX
!      WRITE(WLUN,'(1X,"NVSMOB : ",F6.2)') NVSMOB

!-----------------------------------------------------------------------
!    Find and Read Vegetative Partitioning Section
!-----------------------------------------------------------------------
      SECTION = '!*VEGE'
!      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
!      IF (FOUND .EQ. 0) THEN
!        CALL ERROR(ERRKEY, 1, FILECC, LNUM)
!      ELSE
!        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
!        READ(C80,'(8F6.0)',IOSTAT=ERR)(XLEAF(II),II=1,8)
!        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

!        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
!        READ(C80,'(8F6.0)',IOSTAT=ERR)(YLEAF(II),II=1,8)
!        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

!        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
!        READ(C80,'(8F6.0)',IOSTAT=ERR)(YSTEM(II),II=1,8)
!        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

!        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
!        READ(C80,'(12X,2F6.0)',IOSTAT=ERR) FRSTMF, FRLFF
!        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

!        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
!        READ(C80,'(F6.0)',IOSTAT=ERR) FRLFMX
!        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
!      ENDIF

!      WRITE(WLUN,'(1X, "")')
!      WRITE(WLUN,'(1X,"SECTION: ",A6)') SECTION
!      WRITE(WLUN,'(1X,"I      : ",8I6)') (II,II=1,8)
!      WRITE(WLUN,'(1X,"XLEAF  : ",8F6.1)') (XLEAF(II),II=1,8)
!      WRITE(WLUN,'(1X,"YLEAF  : ",8F6.2)') (YLEAF(II),II=1,8)
!      WRITE(WLUN,'(1X,"YSTEM  : ",8F6.2)') (YSTEM(II),II=1,8)
!      WRITE(WLUN,'(1X, "")')
!      WRITE(WLUN,'(1X,"FRSTMF : ", F6.2)') FRSTMF
!      WRITE(WLUN,'(1X,"FRLFF  : ", F6.2)') FRLFF
!      WRITE(WLUN,'(1X,"FRLFMX : ", F6.2)') FRLFMX

!-----------------------------------------------------------------------
!    Find and Read Leaf Growth Section
!-----------------------------------------------------------------------
      SECTION = '!*LEAF'
!      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
!      IF (FOUND .EQ. 0) THEN
!        CALL ERROR(ERRKEY, 1, FILECC, LNUM)
!      ELSE
!        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
!        READ(C80,'(4F6.0)', IOSTAT=ERR) FINREF, SLAREF, SIZREF, VSSINK
!        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

!        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
!        READ(C80,'(4F6.0)', IOSTAT=ERR) SLAMAX, SLAMIN, SLAPAR, TURSLA
!        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

!        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
!        READ(C80,'(6F6.0)', IOSTAT=ERR)(XVGROW(II),II=1,6)
!        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

!        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
!        READ(C80,'(6F6.0)', IOSTAT=ERR)(YVREF(II),II=1,6)
!        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

!        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
!        READ(C80,'(5F6.0)', IOSTAT=ERR)(XSLATM(II),II = 1,5)
!        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

!        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
!        READ(C80,'(5F6.0)', IOSTAT=ERR)(YSLATM(II),II = 1,5)
!        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
!      ENDIF

!      WRITE(WLUN,'(1X, "")')
!      WRITE(WLUN,'(1X,"SECTION: ",A6)') SECTION
!      WRITE(WLUN,'(1X,"FINREF : ",F6.1)') FINREF
!      WRITE(WLUN,'(1X,"SLAREF : ",F6.1)') SLAREF
!      WRITE(WLUN,'(1X,"SIZREF : ",F6.1)') SIZREF
!      WRITE(WLUN,'(1X,"VSSINK : ",F6.1)') VSSINK
!      WRITE(WLUN,'(1X,"SLAMAX : ",F6.1)') SLAMAX
!      WRITE(WLUN,'(1X,"SLAMIN : ",F6.1)') SLAMIN
!      WRITE(WLUN,'(1X,"SLAPAR : ",F6.3)') SLAPAR
!      WRITE(WLUN,'(1X,"TURSLA : ",F6.2)') TURSLA
!      WRITE(WLUN,'(1X, "")')
!      WRITE(WLUN,'(1X,"I      : ",6I6)') (II,II=1,6)
!      WRITE(WLUN,'(1X,"XVGROW : ",6F6.1)') (XVGROW(II),II=1,6)
!      WRITE(WLUN,'(1X,"YVREF  : ",6F6.1)') (YVREF(II),II=1,6)
!      WRITE(WLUN,'(1X, "")')
!      WRITE(WLUN,'(1X,"I      : ",6I6)') (II,II=1,5)
!      WRITE(WLUN,'(1X,"XSLATM : ",5F6.1)') (XSLATM(II),II = 1,5)
!      WRITE(WLUN,'(1X,"YSLATM : ",5F6.2)') (YSLATM(II),II = 1,5)

      SECTION = '!*ROOT'
!      WRITE(WLUN,'(1X, "")')
!      WRITE(WLUN,'(1X,"SECTION: ",A6)') SECTION
!      WRITE(WLUN,'(1X,"I      : ",4I6)') (II,II=1,4)
!      WRITE(WLUN,'(1X,"XFRRT  : ",4F6.2)') (XFRRT(II),II=1,4)
!      WRITE(WLUN,'(1X,"YFRRT  : ",4F6.2)') (YFRRT(II),II=1,4)

      SECTION = '!*SUGA'
!      WRITE(WLUN,'(1X, "")')
!      WRITE(WLUN,'(1X,"SECTION: ",A6)') SECTION
!      WRITE(WLUN,'(1X,"I      : ",4I6)') (II,II=1,4)
!      WRITE(WLUN,'(1X,"XFRSU  : ",4F6.2)') (XFRSU(II),II=1,4)
!      WRITE(WLUN,'(1X,"YFRSU  : ",4F6.2)') (YFRSU(II),II=1,4)
!      WRITE(WLUN,'(1X, "")')
!      WRITE(WLUN,'(1X,"GAMMA  : ",F6.2)') GAMMA
!-----------------------------------------------------------------------
!    Find and Read Seed and Shell Growth Section
!-----------------------------------------------------------------------
!      SECTION = '!*SEED'
!      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
!      IF (FOUND .EQ. 0) THEN
!        CALL ERROR(ERRKEY, 1, FILECC, LNUM)
!      ELSE
!        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
!        READ(C80,'(6X,F6.0)',IOSTAT=ERR) SRMAX
!        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

!        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
!        READ(C80,'(6X,2F6.0)',IOSTAT=ERR) XFRMAX, SHLAG
!        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

!        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
!        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
!        READ(C80,'(4(1X,F5.2),3X,A3)',IOSTAT=ERR)
!     &          (FNSDT(II),II=1,4), TYPSDT
!        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

!        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
!        READ(C80,'(6F6.0)',IOSTAT=ERR)(XXFTEM(II),II = 1,6)
!        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

!        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
!        READ(C80,'(6F6.0)',IOSTAT=ERR)(YXFTEM(II),II = 1,6)
!        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

!        DO I=1,5
!            CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
!          ENDDO
!        READ(C80,'(4F6.0)',IOSTAT=ERR)(XTRFAC(II),II = 1,4)
!        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

!        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
!        READ(C80,'(4F6.0)',IOSTAT=ERR)(YTRFAC(II),II = 1,4)
!        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
!      ENDIF
!-----------------------------------------------------------------------
!      CLOSE(LUNCRP)

C-----------------------------------------------------------------------
C    Read Ecotype Parameter File
C-----------------------------------------------------------------------
!      LUNECO = 10
!-----------------------------------------------------------------------
!      OPEN (LUNECO,FILE = FILEGC,STATUS = 'OLD',IOSTAT=ERR)

!      ISECT = 2
!      DO I=1,200
!        CALL IGNORE(LUNECO, LNUM, ISECT, C255)
!        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEGC,0)
!        IF ((ISECT .EQ. 1) .AND. (C255(1:1) .NE. ' ') .AND.
!     &        (C255(1:1) .NE. '*')) THEN
!          READ (C255,'(A6,66X,F6.0,30X,3F6.0)',IOSTAT=ERR)
!     &        ECOTYP, LNGSH, THRESH, SDPRO, SDLIP
!          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEGC,LNUM)
!          IF (ECOTYP .EQ. ECONO) THEN
!            EXIT
!          ENDIF
!        ELSE IF (ISECT .EQ. 0) THEN
!          IF (ECONO .EQ. 'DFAULT') CALL ERROR(ERRKEY,3,FILEGC,LNUM)
!          ECONO = 'DFAULT'
!          REWIND(LUNECO)
!        ENDIF
!      ENDDO

!      CLOSE (LUNECO)

!      WRITE(WLUN,'(1X, "")')
!      WRITE(WLUN,'(1X,"END RESULTS FROM IPDMND_SC_OUT.for")')

!      CLOSE (WLUN)

!-----------------------------------------------------------------------
!      RETURN
!-----------------------------------------------------------------------
      END  ! SUBROUTINE IPDMND_SC_OUT
!=======================================================================
!       Variable definitions for DEMAND and IPDMND
!-----------------------------------------------------------------------
! AGRLF     Mass of CH2O required for new leaf growth (g[CH2O] / g[leaf])
! AGRRT     Mass of CH2O required for new root growth (g[CH2O] / g[root])
! AGRSTM    Mass of CH2O required for new stem growth (g[CH2O] / g[stem])
! AGRVG     Mass of CH2O required for vegetative tissue growth including 
!             stoichiometry and respiration (g[CH2O] / g[tissue])
! AGRVG2    Total mass of CH2O required for vegetative tissue growth
!             (g[CH2O] / g[tissue])
! CDMTOT    Total CH2O demand (g[CH2O] / m2 / d)
! CDMVEG    Carbon demand for vegetative growth (g[CH2O] / m2 / d)
! CNOLD     Available CH2O after reproductive growth (g[CH2O] / m2 / d)
! CROP      Crop identification code 
! DAS       Days after start of simulation (days)
! DRPP      Photoperiod days which occur in a real day
!             (photoperiod days / day)
! ECONO     Ecotype code - used to match ECOTYP in .ECO file 
! ECOTYP    Ecotype code for this simulation 
! F         Specific leaf area of new leaf tissue growth, including N
!             (cm2[leaf] / g[leaf])
! FFVEG     Specific leaf area of new leaf tissue growth (interim value)
!             (cm2[leaf] / g[leaf])
! FILECC    Path plus filename for species file (*.spe) 
! FILEGC    Pathname plus filename for ECO file 
! FILEIO    Filename for INP file (e.g., IBSNAT35.INP) 
! FINREF    Specific leaf area (SLA) of leaves of standard crop cultivar 
!             when plants emerge (cm2[leaf] / g[leaf])
! FNINL     Maximum fraction of N for growing leaf tissue (g[N] / g[leaf])
! FNINR     Maximum fraction of N for growing root tissue (g[N] / g[root])
! FNINS     Maximum fraction of N for growing stem tissue (g[N] / g[stem])
! FRLF      Fraction of vegetative tissue growth that goes to leaves on a 
!             day (g[leaf] / g[veg])
! FRLFF     Fraction of daily increase in vegetative weight which goes to 
!             leaves after the day on which the maximum number of V-stages 
!             occurs (NDVSTG). (g[leaf] / g[veg])
! FRLFMX    Maximum leaf partitioning (g[leaf] / g[veg])
! FRRT      Fraction of vegetative tissue growth that goes to roots on a 
!             day (g[root] / g[veg])
! FRSTM     Fraction of vegetative tissue growth that goes to stems on a 
!             day (g[stem] / g[veg])
! FRSTMF    Fraction of daily dry weight increase in vegetative plant parts 
!             which goes to stems after the day on which the maximum number 
!             of V-stages occurs (NDVSTG). (g[stem] / g[veg])
! FVEG      Specific leaf area prior to computing effects of temperature, 
!             PAR, water stress (cm2[leaf] / g[leaf])
! GAINNW    Leaf area added (prior to VSSINK) (cm2[leaf] / m2[ground])
! GAINWT    Leaf weight added (prior to VSSINK and after NDLEAF)
!             (g[leaf] / m2[ground])
! GROMAX    Maximum leaf area which can be added per plant between 
!             emergence and day of simulation as a function of V-stage on 
!             day of simulation (cm2[leaf] / plant)
! GROYES    Maximum leaf area which could have been added per plant between 
!             emergence and yesterday as a function of V-stage
!             (cm2[leaf] / plant)
! LUNCRP    Logical unit number for FILEC (*.spe file) 
! LUNECO    Logical unit number for FILEE (*.eco file) 
! LUNIO     Logical unit number for FILEIO 
! NDMNEW    Total N demand for new growth (g[N] / m2 / d)
! NDMOLD    N demand for old tissue (g[N] / m2 / d)
! NDMTOT    Total N demand (g[N] / m2 / d)
! NDMVEG    N required for vegetative growth if all PGAVL is used as 
!             computed (g[N] / m2 / d)
! NMINEP    Potential N mobilization from storage (g[N] / m2 / d)
! NMOBMX    Maximum fraction of N which can be mobilized in a day 
! NMOBR     Stage-dependent potential N mining rate expressed as a 
!             fraction of the maximum rate (NMOBMX)
! NSTRES    Nitrogen stress factor (1=no stress, 0=max stress) 
! NVEG0     Day of emergence (days)
! NVSMOB    Relative rate of N mining during vegetative stage to that in 
!             reproductive stage 
! NVSTL     N content in leaves (fraction)
! NVSTR     N content in roots (fraction)
! NVSTS     N content in stems (fraction)
! PAR       Daily photosynthetically active radiation or photon flux 
!             density (moles[quanta]/m2-d)
! PARSLA    Effect of PAR on specific leaf area 
! PCNL      Percentage of N in leaf tissue (100 g[N] / g[leaf])
! PCNRT     Percent N in root tissue (100 g[N] / g[root])
! PCNST     Percent N in stem tissue (100 g[N] / g[stem])
! PGAVL     Total available CH2O available for growth & respiration
!             (g[CH2O] / m2)
! PLTPOP    Plant population (# plants / m2)
! PROLFF    Minimum leaf protein composition after N mining
!             ( g[protein] / g[leaf])
! PROLFI    Maximum protein composition in leaves during growth with 
!             luxurious supply of N (g[protein] / g[leaf tissue])
! PRORTF    Minimum root protein composition after N mining
!             ( g[protein] / g[root])
! PRORTI    Maximum protein composition in roots during growth with 
!             luxurious supply of N (g[protein] / g[root])
! PROSTF    Minimum stem protein composition after N mining
!             (g[protein] / g[stem])
! PROSTI    Maximum protein composition in stems during growth with 
!             luxurious supply of N (g[protein] / g[stem])
! RCH2O     Respiration required for synthesizing CH2O structure
!             (g[CH2O] / g[tissue])
! RLIG      Respiration required for synthesizing lignin structure
!             (g[CH2O] / g[lignin])
! RLIP      Respiration required for synthesizing lipid structure
!             (g[CH2O] / g[lipid])
! RMIN      Respiration required for synthesizing mineral structure
!             (g[CH2O] / g[mineral])
! RNO3C     Respiration required for reducing NO3 to protein
!             (g[CH2O] / g[protein])
! ROA       Respiration required for synthesizing organic acids
!             (g[CH2O] / g[product])
! RPROAV    Respiration required for protein synthesis, average based on 
!             sources of N (g[CH2O] / g[protein])
! RTWT      Dry mass of root tissue, including C and N
!             (g[root] / m2[ground])
! SIZELF    The size of a normal upper node leaf (nodes 8 - 10) used to 
!             adjust leaf area expansion during sink-limited phase of 
!             vegetative growth, i.e., prior to VSSINK nodes on the main stem
!             (cm2/leaf)
! SIZRAT    Ratio of upper node normal leaf size for given variety to that 
!             for standard cultivar, used to adjust table of maximum leaf 
!             area vs. V-stage 
! SIZREF    The size of a normal upper node  leaf (nodes 8 - 10) of 
!             standard cultivar. (cm2 / leaf)
! SLAMAX    The maximum specific leaf area (SLA) for new leaves when grown 
!             under low (nearly zero) radiation but optimum water and 
!             temperature for the standard cultivar. (cm2 / g)
! SLAMIN    The minimum specific leaf area (SLA) for new leaves when grown 
!             under infinitely high radiation, optimum water and 
!             temperature for the standard cultivar. (cm2 / g)
! SLAMN     Minimum specific leaf area for new leaves when grown under high 
!             radiation and optimum water and temperature conditions (cm2 / g)
! SLAMX     Maximum specific leaf area for new leaves when grown under low 
!             radiation, but optimum water and temperature conditions
!             (cm2 / g)
! SLAPAR    Coefficient in exponential equation to reduce SLA as PAR 
!             increases (leaf curvature) 
! SLAREF    Specific leaf area (SLA) for new leaves during peak vegetative 
!             growth for the standard cultivar. (cm2/g)
! SLAVAR    Specific leaf area (SLA) for new leaves during peak vegetative 
!             growth for cultivar I, modified by environmental factor (cm2/g)
! STMWT     Dry mass of stem tissue, including C and N
!             (g[stem] / m2[ground)
! SUMTEM    Factor which affects protein composition based on average 
!             temperature. 
! TAVG      Average daily temperature (°C)
! TDUMX     Photo-thermal time that occurs in a real day based on early 
!             reproductive development temperature function
!             (photo-thermal days / day)
! TPHFAC    Reduction in specific leaf area due to daytime temperature 
!             being less than optimal (0-1) 
! TURADD    Water stress factor (TURFAC) effect on reproductive growth and 
!             pod addition.  Stress is defined to INCREASE growth and 
!             addition. 
! TURFAC    Water stress factor for expansion (0 - 1) 
! TURFSL    Factor which applies water stress to specific leaf area of new 
!             leaf tissue growth 
! TURSLA    Water stress effects on leaf area expansion 
! VSSINK    Vegetative stage beyond which sink-limited leaf area expansion 
!             can no longer limit photosynthesis or leaf area growth. 
! VSTAGE    Number of nodes on main stem of plant 
! WCRLF     Mass of CH2O reserves in leaves (g[leaf CH2O] / m2[ground])
! WCRRT     Mass of CH2O reserves in roots (g[root CH2O] / m2[ground])
! WCRST     Mass of CH2O reserves in stems (g[stem CH2O] / m2[ground])
! WNRLF     N available for mobilization from leaves above lower limit of 
!             mining (g[N] / m2)
! WNRRT     N available for mobilization from roots above lower limit of 
!             mining (g[N] / m2)
! WNRST     N available for mobilization from stems above lower limit of 
!             mining (g[N] / m2)
! WTLF      Dry mass of leaf tissue including C and N
!             (g[leaf] / m2[ground])
! XLEAF(I)  V-stage at which partitioning to leaves is YLEAF(I).
!             (leaf nodes)
! XSLATM(I) Temperature values for function that reduces specific leaf area 
!             (SLA) (°C)
! XVGROW(I) V-stage at which maximum leaf area growth per plant since 
!             emergence is YVGROW(I). (# leaf nodes)
! XX        Difference between partitioning fraction to stems at beginning 
!             bloom (R1) and at the day on which the maximum number of 
!             V-stages occurs (NDLEAF) 
! YLEAF(I)  Partitioning fraction to leaves at V-stage XLEAF(I)
!             ( g[leaf] / g[veg. plant])
! YRDOY     Current day of simulation (YYDDD)
! YRSIM     Start of simulation date (YYDDD)
! YSLATM(I) Array which describes the effect of temperature on specific 
!             leaf area 
! YSTEM(I)  Partitioning factor for stem growth at V-stage XSTEM(I)
!             (g[stem] / g[veg. plant])
! YVGROW(I) Maximum leaf area grown per plant at V-stage XVGROW(I)
!             (cm2 / plant)
! YVREF(I)  Maximum leaf area grown per plant at V-stage XVGROW(I), for 
!             reference cultivar. (cm2 / plant)
!-----------------------------------------------------------------------
!       END SUBROUTINE DEMAND
!=======================================================================
