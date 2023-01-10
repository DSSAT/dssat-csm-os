C=======================================================================
C  OPGEN, Subroutine
C
C  Generates output for simulated data
C-----------------------------------------------------------------------
C  Revision history
C
C  01/01/1990 GH  Written
C  05/28/1999 PWW Header revision and minor changes 
C  04/02/1996 GH  Added new output files for flooding and chemical apps
C  12/11/2000 GH  Modified write statemento fit 80 character limit
C  03/05/2002 GH  Modified for CSM modeling system
C  03/11/2005 GH  Modified format for P2R for millet and sorghum
!  12/14/2005 CHP/PST Added new sorghum cultivar coefficients (optional)
C-----------------------------------------------------------------------
C  INPUT  : CUMDEP,TPESW,VRNAME,AINO3,AINH4,TLL,TDUL,TSAT,TSWINI,WTHADJ,
C           CO2,ECONAM,RUN,MODEL,CROP,CROPD,TITLER,ECONO,VARTY,ESW,
C           SWINIT,INO3,INH4,TSOC
C
C  LOCAL  :
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : INPUT
C
C  Calls  : CLEAR OPHEAD OPSOIL
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE OPGEN (CUMDEP,TPESW,VRNAME,AINO3,AINH4,TLL,TDUL,TSAT,
     &     TSWINI,RUN,MODEL,CROP,CROPD,TITLET,ECONO,VARTY,
     &     ESW,SWINIT,INO3,INH4,TSOC,WTHSTR,NYRS, RNMODE, 
     &     CONTROL, ISWITCH, UseSimCtr, ATLINE, PATHEX)

      USE ModuleDefs
      IMPLICIT NONE
      EXTERNAL CLEAR, OPHEAD, OPSOIL

      INCLUDE 'COMSOI.blk'
      INCLUDE 'COMSWI.blk'

      CHARACTER*  1 ANS, RNMODE
      CHARACTER*  2 CROP
      CHARACTER*  6 VARTY,ECONO
      CHARACTER*  8 MODEL
      CHARACTER* 16 CROPD, VRNAME
      CHARACTER* 25 TITLET
      CHARACTER* 80 PATHEX
      CHARACTER*120 WTHSTR
      CHARACTER*1000 ATLINE

      INTEGER NYRS,RUN
      INTEGER LUNOV,LUNOUT

      REAL    AINO3,AINH4
      REAL    SWINIT(NL),TSWINI,INO3(NL),INH4(NL)
      REAL    CUMDEP,TPESW,ESW(NL)
      REAL    TLL,TDUL,TSAT,TSOC

      LOGICAL UseSimCtr

      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH

      PARAMETER (LUNOUT = 30, LUNOV = 80)

C-----------------------------------------------------------------------
C     Generate header file to be used by model output routines.
C-----------------------------------------------------------------------
      CALL OPHEAD(SEASINIT,LUNOV, CUMDEP,TPESW,VRNAME,AINO3,AINH4,
     &     ECONO,RUN,MODEL,TITLET,WTHSTR, RNMODE,
     &     CONTROL, ISWITCH, UseSimCtr, PATHEX)

C-----------------------------------------------------------------------
C     Generate a summary output for the screen
C-----------------------------------------------------------------------
       IF (RNMODE .EQ. 'I' .AND. NYRS .LE. 1) THEN
          CALL CLEAR
!         write header to console
          WRITE (6,50)                                    
          CALL OPHEAD (SEASINIT,6,CUMDEP,TPESW,VRNAME,AINO3,AINH4, 
     &     ECONO,RUN,MODEL,TITLET,WTHSTR, RNMODE,
     &     CONTROL, ISWITCH, UseSimCtr, PATHEX)
       ENDIF

      CALL OPSOIL (LL,DUL,SAT,
     &     DLAYR,SWINIT,DS,NLAYR,ESW,SHF,BD,PH,INO3,INH4,OC,
     &     TLL,TDUL,TSAT,TPESW,TSWINI,AINO3,AINH4,TSOC,
     &     SWCON,U,SALB,CN2,CROPD,VRNAME,VARTY,SLPF,
     &     ECONO,SLNF,CROP, RNMODE, RUN, CONTROL % MODEL, ISWITCH
     &      , ATLINE)
      IF (RNMODE .EQ. 'I' .AND.  NYRS .LE. 1) THEN
        WRITE (*,2900)
        READ (5,'(1A1)') ANS
      ENDIF

!      WRITE(LUNOV,'(//)')
!      CLOSE (LUNOV)
      RETURN

C-----------------------------------------------------------------------
C     FORMAT Strings
C-----------------------------------------------------------------------

  50  FORMAT ('*SIMULATION OVERVIEW')
 2900 FORMAT (1X,'Please press < ENTER > key to continue ',$)

      END SUBROUTINE OPGEN

C=======================================================================
C  OPSOIL, Subroutine
C
C  Generates output for soil data
C-----------------------------------------------------------------------
C  Revision history
!  09/25/2007 CHP Moved OPSOIL to OPHEAD.for to generate headers
C=======================================================================
