C=======================================================================
C  CSP_VEGGR, Subroutine for CASUPRO sugarcane model, based on 
C  VEGGR, Subroutine, J. W. Jones, K. J. Boote, G. Hoogenboom
C  Calculates vegetative partitioning as a function of V stage,
C  and limits growth prior to VSINK.
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  01/01/1989     Written.
C  04/24/1994 NBP Changed TAIRHR to TGRO.
C  09/26/1995 KJB Added shift in partitioning from stem to leaf during
C                 and especially after drought stress.
C  01/19/1996 KBJ & JWJ  Idea that we should allow shift in partitioning
C                 from stem to leaf under cold stress and possibly low
C                 light stress in similar manner.  Non-optimum temp
C                 causes more DM to leaf in SPAR experiment.  What
C                 about root:shoot?  Do these later.
C  02/15/1996 KJB Allow excess (=PGLEFT) to influence PG on next day
C  06/18/1998 CHP Modified for modular format
C  05/10/1999 GH  Incorporated in CROPGRO
C  06/21/2001 GH  Modified seasonal initialization
C  11/09/2001 O.H. Daza modified for sugarcane model
C  08/12/2003 CHP Added I/O error checking
C  07/26/2004 CHP Removed variables which were not being used
C-----------------------------------------------------------------------
C  Called by: CASUPRO
C  Calls:     CANOPY (this is left for future reference)
C             ERROR, FIND, IGNORE
C========================================================================

      SUBROUTINE CSP_VEGGR(DYNAMIC,
     &    AGRLF, AGRRT, AGRSTM, AGRSU, CMINEP, CSAVEV,    !Input
     &    DTX, FILECC, FNINL, FNINR, FNINS, FNINSU, NAVL, !Input
     &    NDMNEW, NDMOLD, NMINEA, PAR, PCH2O, PG, PGAVL,  !Input
     &    ROWSPC, STMWT, TRNU, TURFAC, VSTAGE, WCRLF,     !Input
     &    WCRRT, WCRST, WCRSU, WTLF, XLAI, YRDOY, YRSIM,  !Input
     &    AGRVG, FRLF, FRRT, FRSTM, FRSU,                 !Input/Output
     &    CADLF, CADST, CADSU, CMINEA, CRUSLF, CRUSRT,    !Output
     &    CRUSST, CRUSSU, EXCESS, NADLF, NADRT, NADST,    !Output
     &    NADSU, NGRLF, NGRRT, NGRST, NGRSU, NSTRES,      !Output
     &    TNLEAK, WLDOTN, WRDOTN, WSDOTN, WSUDOTN)        !Output

! include WCRSU, CRUSSU, 
!-----------------------------------------------------------------------
      USE ModuleDefs 
      USE ModuleData

      IMPLICIT NONE
      EXTERNAL GETLUN, FIND, ERROR, IGNORE, PRNT_VEGGR_SC
      SAVE

      CHARACTER*6  ERRKEY
      PARAMETER   (ERRKEY = 'VEGGR')

      CHARACTER*6  SECTION    !, ECONO
      CHARACTER*80 C80
      CHARACTER*92 FILECC     !, FILEGC

      INTEGER DYNAMIC   !, TIMDIF
      INTEGER YRDOY, YRSIM, DAS   !, YREMRG
      INTEGER I, LUNCRP, ERR, LINC, LNUM, ISECT, FOUND

      REAL AGRLF, AGRRT, AGRSTM, CMINEP, CMOBMX
      REAL DTX, FNINL, FNINR, FNINS
      REAL NAVL, NDMNEW, NDMOLD, PAR, PCH2O, PG
      REAL PROLFI, PRORTI, PROSTI, ROWSPC
      REAL STMWT, TURFAC, WCRLF, WCRRT, WCRST
      REAL WTLF, XLAI

      REAL AGRVG, CADLF, CADST, CMINEA
      REAL CRUSLF, CRUSRT, CRUSST, CUMTUR
      REAL EXCESS, FRLF, FRRT, FRSTM, NADLF, NADRT, NADST
      REAL NGRLF, NGRRT, NGRST, NSTRES, PGAVL
      REAL TNLEAK, VSTAGE, WLDOTN, WRDOTN, WSDOTN

      REAL ATOP, CADSTF, FNINLG, FNINRG, FNINSG
      REAL PROLFG, PRORTG, PROSTG
      REAL NRATIO, NGRVEG, NADRAT, NLEFT
      REAL NGRVGG, NGRLFG, NGRSTG, NGRRTG
      REAL PROLFT, PROSTT, PRORTT
      REAL VGRDEM, SUPPN, PGLEFT, LSTR, CSAVEV
      REAL NLEAK, NMINEA, TRNU

!      REAL TGRO(TS)

! LIST OF NEW VARIABLES ADDED TO ENABLE SIMULATION OF SUGARS IN THE 
! SUGARCANE MODEL

!	INTEGER OpenStatus !, WLUN

	REAL DXR
      REAL AGRSU, CADSU, CRUSSU, FNINSU, FRSU, NADSU, NGRSU, NGRSUG 
	REAL PROSUG, PROSUI, PROSUT, WCRSU, WSUDOTN
      REAL RSDN, ANDMVEG, EXNIT

	LOGICAL SUBTITLE

      TYPE (ControlType) CONTROL

!***********************************************************************
!***********************************************************************
!     Run Initialization - Called once per simulation
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
!-----------------------------------------------------------------------
!     Read in values from input file, which were previously input
!       in Subroutine IPCROP.
!-----------------------------------------------------------------------
      CALL GETLUN('FILEC', LUNCRP)
      OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,0)
      LNUM = 0
!-----------------------------------------------------------------------
!    Find and Read Plant Composition Section
!-----------------------------------------------------------------------
!     Subroutine FIND finds appropriate SECTION in a file by
!     searching for the specified 6-character string at beginning
!     of each line.
!-----------------------------------------------------------------------
      SECTION = '!*PLAN'
      CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(2F6.0,6X,2F6.0)',IOSTAT=ERR)
     &          PROLFI, PROLFG, PROSTI, PROSTG
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(2F6.0,6X,2F6.0)',IOSTAT=ERR) 
     &       PRORTI, PRORTG, PROSUI, PROSUG
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
      ENDIF
!-----------------------------------------------------------------------
!    Find and Read Plant Composition Section
!-----------------------------------------------------------------------
      SECTION = '!*CARB'
      CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(2F6.0)',IOSTAT=ERR) CMOBMX, CADSTF
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
      ENDIF
!-----------------------------------------------------------------------
!    Find and Read Partitioning Section
!-----------------------------------------------------------------------
      SECTION = '!*VEGE'
      CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILECC, LNUM)
      ELSE
          DO I=1,4
            ISECT = 2
            DO WHILE (ISECT .NE. 1)
            CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
            ENDDO
          ENDDO
        READ(C80,'(24X,F6.0)',IOSTAT=ERR) ATOP
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
      ENDIF
      
      CLOSE (LUNCRP)

!***********************************************************************
! Echoes input data

! Open file to write results from CSP_VEGGR

!      CALL GETLUN('WORK.OUT',WLUN)
!      OPEN(UNIT = WLUN, FILE = "WORK.OUT", STATUS = "UNKNOWN",
!     &   ACTION = "WRITE", POSITION = "APPEND", IOSTAT = OpenStatus)

!      WRITE(WLUN,*)
!      WRITE(WLUN,*)
!      WRITE(WLUN,'(1X,"RESULTS FROM VEGGR_SC.for")')
!      WRITE(WLUN,'(1X,"-------------------------")')
!      WRITE(WLUN,*)
!      WRITE(WLUN,'(1X,"*** FILECC : ",A80)') FILECC

      SECTION = '!*PLAN'
!      WRITE(WLUN,*)
!      WRITE(WLUN,'(1X,"SECTION: ",A6)') SECTION
!      WRITE(WLUN,'(1X,"PROLFI : ",F6.3)') PROLFI
!      WRITE(WLUN,'(1X,"PROLFG : ",F6.3)') PROLFG
!      WRITE(WLUN,'(1X,"PROSTI : ",F6.3)') PROSTI
!      WRITE(WLUN,'(1X,"PROSTG : ",F6.3)') PROSTG
!      WRITE(WLUN,'(1X,"PRORTI : ",F6.3)') PRORTI
!      WRITE(WLUN,'(1X,"PRORTG : ",F6.3)') PRORTG
!      WRITE(WLUN,'(1X,"PROSUI : ",F6.3)') PROSUI
!      WRITE(WLUN,'(1X,"PROSUG : ",F6.3)') PROSUG

      SECTION = '!*CARB'
!      WRITE(WLUN,*)
!      WRITE(WLUN,'(1X,"SECTION: ",A6)') SECTION
!      WRITE(WLUN,'(1X,"CMOBMX : ",F6.3)') CMOBMX
!      WRITE(WLUN,'(1X,"CADSTF : ",F6.3)') CADSTF

      SECTION = '!*VEGE'
!      WRITE(WLUN,*)
!      WRITE(WLUN,'(1X,"SECTION: ",A6)') SECTION
!      WRITE(WLUN,'(1X,"ATOP   : ",F6.3)') ATOP

!      WRITE(WLUN,*)
!      WRITE(WLUN,'(1X,"END RESULTS FROM VEGGR_SC.for")')

!      CLOSE (WLUN)

!-----------------------------------------------------------------------
!    Call CANOPY for input
!-----------------------------------------------------------------------
!     CALL CANOPY(
!    &  ECONO, FILECC, FILEGC, PAR, ROWSPC,               !Input
!    &  RVSTGE, TGRO, TURFAC, VSTAGE, XLAI,               !Input
!    &  CANHT, CANWH,                                     !Output
!    &  RUNINIT)                                          !Control

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      CADLF  = 0.0  
      CADST  = 0.0
      CMINEA = 0.0        
      CRUSLF = 0.0  
      CRUSRT = 0.0  
      CRUSST = 0.0
	CRUSSU = 0.0  !Sugars
      CUMTUR = 1.0        
      EXCESS = 1.0	  
      FNINLG = 0.0  
      FNINRG = 0.0  
      FNINSG = 0.0  

      FNINSU  = 0.0  ! Sugars. Check this out again
      NADLF  = 0.0  
      NADRT  = 0.0  
      NADST  = 0.0  
      NADSU  = 0.0  !Sugars
      NGRLF  = 0.0  
      NGRRT  = 0.0  
      NGRST  = 0.0  
      NGRSU  = 0.0  !Sugars

      NSTRES = 1.0
      PGLEFT = 0.0
      SUPPN  = 0.0
      TNLEAK = 0.0 
      VGRDEM = 0.0

      WLDOTN = 0.0  
      WRDOTN = 0.0  
      WSDOTN = 0.0  
      WSUDOTN = 0.0  !Sugars

!      CALL CANOPY(
!     &  ECONO, FILECC, FILEGC, PAR, ROWSPC,               !Input
!     &  RVSTGE, TGRO, TURFAC, VSTAGE, XLAI,               !Input
!     &  CANHT, CANWH,                                     !Output
!     &  RUNINIT)                                          !Control

!***********************************************************************
!***********************************************************************
!     EMERGENCE CALCULATIONS - Performed once per season upon emergence
!         or transplanting of plants
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. EMERG) THEN
!-----------------------------------------------------------------------
      FNINLG = PROLFG * 0.16   
      FNINRG = PRORTG * 0.16   
      FNINSG = PROSTG * 0.16   
      CUMTUR = 1.0 

! Sugars
	FNINSU = PROSUG * 0.16

!      CALL CANOPY(
!     &  ECONO, FILECC, FILEGC, PAR, ROWSPC,               !Input
!     &  RVSTGE, TGRO, TURFAC, VSTAGE, XLAI,               !Input
!     &  CANHT, CANWH,                                     !Output
!     &  EMERG)                                            !Control

!***********************************************************************
!***********************************************************************
!     DAILY RATE/INTEGRATION
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------
!     DAS   = MAX(0,TIMDIF(YRSIM,YRDOY))
      CALL GET(CONTROL)
      DAS = CONTROL % DAS
!-----------------------------------------------------------------------
C     Partitioning is modified by water stress and nitrogen stress
C-----------------------------------------------------------------------
      SUPPN = TRNU + NMINEA
      IF (SUPPN .LT. 0.70 * NDMNEW .AND. NDMNEW .GT. 0.) THEN
        NSTRES = MIN(1.0,SUPPN/(NDMNEW * 0.70))
      ELSE
        NSTRES = 1.0  ! N supply > N demand
      ENDIF

! N available = N supply
! If N available > N demand
!   Yes: growth takes place as calculated in Demand_SC and NSTRES=1
!   No : growth takes place but limited by N supply.
!        Then, reduce growth of everything but Sugars

! Recompute growth and fractions according to N supply
! RSDN      Ratio of supply demand for Nitrogen
! ANDMVEG   Actual nitrogen demand for vegetative growth (g [N] / m2 - d)

      IF (SUPPN > 0 .AND. NDMNEW > 0) THEN !Added for debugging FSR
!     IF (SUPPN > 0) THEN   
        RSDN = SUPPN / (0.70 * NDMNEW)
        IF (RSDN >= 1.0) THEN   !Excess of Nitrogen
          EXNIT   = SUPPN - (0.70 * NDMNEW)
	    ANDMVEG = 0.70 * NDMNEW  !Updates nitrogen actually used
        ELSE                    !Deficit of Nitrogen
	    FRLF  = RSDN * FRLF   !Reduce growth of leaves
	    FRSTM = RSDN * FRSTM  !Reduce growth of stalks
	    FRRT  = RSDN * FRRT   !Reduce growth of roots
                                !Do not reduce accumulation of sugars
	    ANDMVEG = SUPPN       !Updates nitrogen actually used
        END IF
	END IF

!-Comment: We need a function to account for water and nitrogen strss in roots
!Dec 11      FRRT = ATOP * (1.0 - (MIN(TURFAC, NSTRES))) * (1.0 - FRRT) + FRRT
C-----------------------------------------------------------------------
C     Cumulative turgor factor that remembers veg drought stress
C     to shift partitioning between leaf and stem toward leaf,
C     especially after drought is released.
C     Sort of 20-day rolling average
C-----------------------------------------------------------------------
      CUMTUR = 0.95*CUMTUR + 0.05*TURFAC
C-----------------------------------------------------------------------
C     0.6 IS A SCALAR, COULD BE LESS, was once 0.8 and 0.7
C     0.7 appears to be too much for peanut, but not for soybean.
C-----------------------------------------------------------------------
! Include the part on Sugars from here on
! Original
!      FRLF  = (1.0 + 0.6*(1.0-CUMTUR))*(1.-FRRT)*FRLF/(FRLF + FRSTM)
!      FRLF = MIN(FRLF, 0.90*(1. - FRRT))
!      FRSTM = 1.0 - FRRT - FRLF

! New for sugars
!Dec 11      FRLF  = (1.0 + 0.6 * (1.0 - CUMTUR)) * (1. - FRRT) * FRLF / 
!Dec 11     &        (FRLF + FRSTM + FRSU)
!Dec 11      FRLF = MIN(FRLF, 0.90 * (1. - FRRT))

!Dec 11      FRSTM = (1.0 + 0.6 * (1.0 - CUMTUR)) * (1. - FRRT) * FRSTM / 
!Dec 11     &        (FRLF + FRSTM + FRSU)
!Dec 11      FRSTM = MIN(FRSTM, 0.90 * (1. - FRRT))

!      FRSTM = 1.0 - FRRT - FRLF - FRSU

!      FRSU = 1.0 - FRLF - FRSTM - FRRT

!Dec 11      FRSU = MAX(0.0, 1.0 - FRLF - FRSTM - FRRT)

C-----------------------------------------------------------------------
C     To prevent negative partitioning to root and limit leaf plus
C     stem to a maximum of 98 % of the vegetative partitioning
C-----------------------------------------------------------------------
!Dec 11      FRLF = MIN(FRLF, FRLF * 0.98 / (MAX(0.001, FRLF + FRSTM + FRSU)))
!Dec 11      FRSTM = MIN(FRSTM,FRSTM*0.98 / (MAX(0.001, FRLF + FRSTM + FRSU)))

! Sugars
!Dec 11      FRSU = MIN(FRSU, FRSU * 0.98 / (MAX(0.001, FRLF + FRSTM + FRSU)))

!Dec 11      FRRT  = 1.0 - FRLF - FRSTM - FRSU

C-----------------------------------------------------------------------
C     Calculate weighted PHI + GR = 1/E = AGRVG for veg. growth
C-----------------------------------------------------------------------
! AGRVG   Mass of CH2O required for vegetative tissue growth including 
!           stoichiometry and respiration (g [CH2O] / g [tissue])

      FRSU = MAX(0.0, 1.0 - FRLF - FRSTM - FRRT)

      AGRVG = AGRLF * FRLF + AGRRT * FRRT + AGRSTM * FRSTM + 
     &        AGRSU * FRSU

C-----------------------------------------------------------------------
C     Calculate New Growth Rate of Leaves, Stems, Roots and Sugars
C-----------------------------------------------------------------------
! VGRDEM  Vegetative growth demand (g [vegetative tissue] / m2 - d)
! WLDOTN  Dry weight growth rate of new leaf tissue including N but not C 
!           reserves (g [leaf] / m2 [ground] - d)
! WRDOTN  Dry weight growth rate of new root tissue including N but not C 
!           reserves (g [root] / m2 [ground] - d)
! WSDOTN  Dry weight growth rate of new stem tissue including N but not C 
!           reserves (g [stem] / m2 [ground] - d)
! WSUDOTN Dry weight growth rate of new sugars including N but not C 
!           reserves (g [sugars] / m2 [ground] - d)

      VGRDEM  = PGAVL / AGRVG

      WLDOTN  = FRLF  * VGRDEM
      WSDOTN  = FRSTM * VGRDEM
      WRDOTN  = FRRT  * VGRDEM
      WSUDOTN = FRSU  * VGRDEM  !Sugars

! Note: Include sugars accumulation in a pool based on the amount of 
! carbohydrate available after all the processes of growth and 
! respiration.

! Note: Include the carbon balance daily


C-----------------------------------------------------------------------
C     Compute maximum N required for tissue growth
C-----------------------------------------------------------------------
! NGRLF   Maximum N demand for leaf growth (g [leaf N] / m2 [ground] - d)
! NGRRT   Maximum N demand for root growth (g [root N] / m2 [ground] - d)
! NGRST   Maximum N demand for stem growth (g [stem N] / m2 [ground] - d)
! NGRSU   Maximum N demand for sugars growth (g [sugars N] / m2 [ground] - d)
! NGRVEG  Maximum N demand for vegetative tissue growth
!           (g [leaf N] / m2 [ground] - d)

      NGRLF = WLDOTN  * FNINL
      NGRST = WSDOTN  * FNINS
      NGRRT = WRDOTN  * FNINR
      NGRSU = WSUDOTN * FNINSU  !Sugars

      NGRVEG = NGRLF + NGRST + NGRRT + NGRSU
C-----------------------------------------------------------------------
C     Compute minimum N required for tissue growth
C-----------------------------------------------------------------------
! NGRLFG  Minimum N requirement for leaf growth
!           (g [leaf N] / m2 [ground] - d)
! NGRRTG  Minimum N requirement for root growth
!           (g [leaf N] / m2 [ground] - d)
! NGRSTG  Minimum N requirement for stem growth
!           (g [leaf N] / m2 [ground] - d)
! NGRSUG  Minimum N requirement for sugars accumulation 
!           (g [sugars N] / m2 [ground] - d)
! NGRVGG  Minimum N requirement for vegetative tissue growth
!           (g [leaf N] / m2 [ground] - d)

      NGRLFG = WLDOTN  * FNINLG
      NGRSTG = WSDOTN  * FNINSG
      NGRRTG = WRDOTN  * FNINRG
      NGRSUG = WSUDOTN * FNINSU  !Sugars

      NGRVGG = NGRLFG + NGRSTG + NGRRTG + NGRSUG

      NRATIO = 1.0

      IF (NAVL .LT. NGRVGG) THEN
C-----------------------------------------------------------------------
C     Compute ratio for reducing leaf growth to prevent N conc of
C       new tissue from being below the minimum for growth
C-----------------------------------------------------------------------
         IF (NGRVGG .GT. 0.0) THEN
            NRATIO = NAVL / NGRVGG

            WLDOTN  = WLDOTN  * NRATIO
            WSDOTN  = WSDOTN  * NRATIO
            WRDOTN  = WRDOTN  * NRATIO
            WSUDOTN = WSUDOTN * NRATIO  !Sugars

            NGRLF = NGRLFG * NRATIO
            NGRST = NGRSTG * NRATIO
            NGRRT = NGRRTG * NRATIO
            NGRSU = NGRSUG * NRATIO  !Sugars

C-----------------------------------------------------------------------
C     Adjust conversion costs to account for composition of tissue at
C       lower N concentration
C-----------------------------------------------------------------------
            AGRVG = 
     &       AGRLF * FRLF * (1.0 - (PROLFG - PROLFI)/(1.0 - PROLFI)) + 
     &       AGRRT * FRRT * (1.0 - (PRORTG - PRORTI)/(1.0 - PRORTI)) + 
     &       AGRSTM * FRSTM * (1.0 - (PROSTG - PROSTI)/(1.0 - PROSTI)) + 
!Sugars
     &       AGRSU * FRSU * (1.0 - (PROSUG - PROSUI)/(1.0 - PROSUI))
         ENDIF
      ELSE
C-----------------------------------------------------------------------
C     NAVL is between lower and maximum N limit; in this case,
C       leaf expansion occurs as normal, but N concentration is reduced
C-----------------------------------------------------------------------
         IF (NGRVEG .GT. 0.0 .AND. NAVL .LT. NGRVEG) THEN
            NGRLF = MIN(NAVL * NGRLF / NGRVEG, NGRLF)
            NGRST = MIN(NAVL * NGRST / NGRVEG, NGRST)
            NGRRT = MIN(NAVL * NGRRT / NGRVEG, NGRRT)
            NGRSU = MIN(NAVL * NGRSU / NGRVEG, NGRSU)  !Sugars
         ENDIF
C-----------------------------------------------------------------------
C     Compute protein fraction of new vegetative tissue growth
C-----------------------------------------------------------------------
! PROLFT  Protein fraction of new leaf growth (g [protein] / g [leaf tissue])
! PRORTT  Protein fraction of new root growth (g [protein] / g [root])
! PROSTT  Protein fraction of new stalk growth (g [protein] / g [stalk])
! PROSUT  Protein fraction of new sugars accumulation (g [protein] / g [sugar])

         IF (WLDOTN .GT. 0.0) THEN
            PROLFT = NGRLF * (100. / 16.) / WLDOTN
         ELSE
            PROLFT = 0.0
         ENDIF

         IF (WSDOTN .GT. 0.0) THEN
            PROSTT = NGRST * (100. / 16.) / WSDOTN
         ELSE
            PROSTT = 0.0
         ENDIF

         IF (WRDOTN .GT. 0.0) THEN
            PRORTT = NGRRT * (100. / 16.) / WRDOTN
         ELSE
            PRORTT = 0.0
         ENDIF

         IF (WSUDOTN .GT. 0.0) THEN  !Sugars
            PROSUT = NGRSU * (100. / 16.) / WSUDOTN
         ELSE
            PROSUT = 0.0
         ENDIF
C-----------------------------------------------------------------------
C     Recompute respiration costs if expansion occurs at low N-conc.,
C       allow N dilution during growth of leaves, stems, and roots
C-----------------------------------------------------------------------
! AGRVG   Mass of CH2O required for vegetative tissue growth including 
!           stoichiometry and respiration (g [CH2O] / g [tissue])

         AGRVG = 
     &    AGRLF * FRLF * (1.0 - (PROLFT - PROLFI) / (1.0 - PROLFI)) + 
     &	AGRRT * FRRT * (1.0 - (PRORTT - PRORTI) / (1.0 - PRORTI)) + 
     &	AGRSTM * FRSTM * (1.0 - (PROSTT - PROSTI) / (1.0 - PROSTI)) +
!Sugars
     &	AGRSU * FRSU * (1.0 - (PROSUT - PROSUI) / (1.0 - PROSUI))
      ENDIF
C-----------------------------------------------------------------------
C     Compute C and N remaining to add to reserves
C-----------------------------------------------------------------------
! PGLEFT  Excess PG after today's tissue growth (g [CH2O] / m2)

      PGLEFT = MAX(0.0,PGAVL - ((WLDOTN + WSDOTN + WRDOTN + WSUDOTN) *
     &	         AGRVG))
C-----------------------------------------------------------------------
C     Scales to 1.0 if PGLEFT is small fraction, and to 0.2 if large
C     fraction.  Used 0.04, so minor PGLEFT has no effect.  Used square
C     root.  Creates almost no effect if PGLEFT/PG is small, but goes to
C     0.2 as PGLEFT/PG  approaches 1.0.  0.04 could be parameterized as
C     kickoff point.  Upper cutoff is the value 1.04.  Limit of 1.04 -
C     1.00 forces relationship to stop at 0.04, gives 0.2 of normal PG.
C     value 1.04 -0.04 also can not be greater than 1.0 or we get
C     stimulation of photosynthesis and the sq root works differently.
C-----------------------------------------------------------------------
! EXCESS  Factor based on excess PG used to affect tomorrow's PG 
!           calculation 

      IF (PG .GT. 0.0001 .AND. PGLEFT .GT. 0.00001) THEN
         EXCESS = (1.20 - MIN(1.0, MAX(PGLEFT / PG, 0.20)) ) ** 0.5
      ELSE
         EXCESS = 1.00
      ENDIF

! CADLF   Mass of CH2O added to leaf reserves after growth
!           (g [CH2O] / m2 - d)
! CADST   Mass of CH2O added to stems (g[CH2O] / m2 - d)
! CMINEA  Actual carbon mined from vegetative tissue (g [CH2O] / m2 - d)
! CRUSLF  C mobilized from leaf tissue in a day (g [CH2O] / m2 - d)
! CRUSRT  C mobilized from root tissue in a day (g [CH2O] / m2 - d)
! CRUSSH  C mobilized from shell tissue in a day (g [CH2O] / m2 - d)
! CRUSST  C mobilized from stem tissue in a day (g [CH2O] / m2 - d)
! CRUSSU  C mobilized from sugars storage in a day (g [CH2O] / m2 - d)

      CADST  = 0.0
      CADLF  = 0.0
      CMINEA = 0.0
      CRUSLF = 0.0
      CRUSST = 0.0
      CRUSRT = 0.0
      CRUSSU = 0.0  !Sugars
C-----------------------------------------------------------------------
C    Calculate increase in remobilizable C due to N shortage and
C      add to Carbon Pool. Distribute to Leaves and Stems.
C-----------------------------------------------------------------------
C    Want half as much accumulation in stem in veg phase
C-----------------------------------------------------------------------
!      IF (DAS .LT. NR1) THEN
!         LSTR = (1.-0.6*CADSTF)/(0.6*CADSTF)
!      ELSE

! CADSTF  Proportion of CH2O reserves that are added to stalks (fraction)
! CMINEP  Potential CH2O mobilization from storage (g [CH2O] / m2 - d)
! LSTR    Ratio of excess C to be added to leaves in a day relative to the 
!           amount to be stored in stalks
! PCH2O   Respiration loss due to storage/mobilization of CH2O
!           (g [CH2O] / g [CH2O])

         LSTR = (1.- CADSTF) / CADSTF

!      ENDIF

      IF (STMWT + WTLF .GT. 0.0) THEN
         LSTR = LSTR * WTLF / (STMWT + WTLF * LSTR)
      ENDIF

      IF (PGLEFT .GE. CMINEP) THEN
        CADLF = (PGLEFT - CMINEP) / PCH2O * LSTR
        CADST = (PGLEFT - CMINEP) * (1. - LSTR) / PCH2O
      ELSE
C-----------------------------------------------------------------------
C    Calculate actual C used (CMINEA) , compute how much is taken
C    from LF, ST, RT, and SU, which may be less than orig calc of CMINEP
C
C    8/26/97 KJB  DTX IN PLACE OF 1 TO SLOW IT DOWN A BIT AT ALL TIMES
C    AND TO BE SENSITIVE TO TEMPERATURE PRIOR TO R5 STAGE, BUT
C    STILL WANT THE SPEED-UP CAUSED BY THE "+ DXR57" FEATURE AFTER R5.
C
C-----------------------------------------------------------------------
! CMOBMX  Maximum C pool mobilization rate (g [CH2O] / m2 - d)
! CRUSLF  C mobilized from leaf tissue in a day (g [CH2O] / m2 - d)
! CRUSRT  C mobilized from root tissue in a day (g [CH2O] / m2 - d)
! CRUSST  C mobilized from stem tissue in a day (g [CH2O] / m2 - d)
! CRUSSU  C mobilized from sugars storage in a day (g [CH2O] / m2 - d)
! CSAVEV  Fraction of PG for VEG that is stored as CH2O 
! DTX     Thermal time that occurs in a real day based on vegetative 
!           development temperature function (thermal days / day)
! WCRLF   Mass of CH2O reserves in leaves (g [leaf CH2O] / m2 [ground])
! WCRRT   Mass of CH2O reserves in roots (g [root CH2O] / m2 [ground])
! WCRST   Mass of CH2O reserves in stems (g [stem CH2O] / m2 [ground])
! WCRSU   Mass of CH2O reserves in sugars (g [sugars CH2O] / m2 [ground])

! new variable added to replace DXR57 as it does not exist for SC        
	  DXR = 1.0  

        IF (CMINEP .GT. 0) THEN
          CMINEA = CMINEP - PGLEFT

!          CRUSLF = CMINEA / CMINEP * CMOBMX * WCRLF * (DTX + DXR57)
!          CRUSST = CMINEA / CMINEP * CMOBMX * WCRST * (DTX + DXR57)
!          CRUSRT = CMINEA / CMINEP * CMOBMX * WCRRT * (DTX + DXR57)
!          CRUSSH = CMINEA / CMINEP * CMOBMX * WCRSH * (DTX + DXR57)

! Modified for sugarcane. However there are questions!!!.
          CRUSLF = CMINEA / CMINEP * CMOBMX * WCRLF * DTX
          CRUSST = CMINEA / CMINEP * CMOBMX * WCRST * DTX
          CRUSRT = CMINEA / CMINEP * CMOBMX * WCRRT * DTX
          CRUSSU = CMINEA / CMINEP * CMOBMX * WCRSU * DTX
        ENDIF
      ENDIF

!      CADLF = CADLF + CSAVEV / PCH2O * LSTR
!      CADST = CADST + CSAVEV * (1. - LSTR) / PCH2O

C-----------------------------------------------------------------------
C    Calculate increase in remobilizable N due to a C shortage,
C      add to nitrogen pool
C-----------------------------------------------------------------------
! NADRAT  Total nitrogen added to vegetative N reserves (g [N] / m2 - d)
! NLEAK   Nitrogen leak (g [N] / m2 - d)
! NLEFT   Nitrogen left after vegetative demands are met (g [N] / m2 - d)

! Thsi part is not useful for sugarcane. However it will be better 
! to use a N pool. NDMOLD = 0 as it is not being used. 
! See subroutine DEMAND_SC

      NLEFT = MAX(0.0, NAVL - (NGRLF + NGRST + NGRRT + NGRSU))
      IF (NLEFT .GT. 0.0) THEN
         IF (NLEFT .GT. NDMOLD) THEN
            NLEAK  = NLEFT  - NDMOLD
            TNLEAK = TNLEAK + NLEAK
            NLEFT  = NLEFT  - NLEAK
         ELSE
            NLEAK = 0.0
         ENDIF

         NADRAT = NLEFT / (FRLF * FNINL + FRSTM * FNINS + FRRT * FNINR +
     &                     FRSU * FNINSU)

         NADLF  = NADRAT * FRLF  * FNINL
         NADST  = NADRAT * FRSTM * FNINS
         NADRT  = NADRAT * FRRT  * FNINR
         NADSU  = NADRAT * FRSU  * FNINSU  !Sugars
      ELSE
         NADRAT = 0.0
         NADST  = 0.0
         NADLF  = 0.0
         NADRT  = 0.0
         NADSU  = 0.0  ! Sugars
      ENDIF

      IF (DYNAMIC == 4) THEN
        SUBTITLE = .TRUE.
      END IF
C-----------------------------------------------------------------------
C     Subroutine CANOPY calculates height and width of the canopy as a
C     function of VSTAGE, air temperature, drought stress (TURFAC),
C     daylenght and radiation (PAR).
C-----------------------------------------------------------------------
!      CALL CANOPY(
!     &  ECONO, FILECC, FILEGC, PAR, ROWSPC,             !Input
!     &  RVSTGE, TGRO, TURFAC, VSTAGE, XLAI,             !Input
!     &  CANHT, CANWH,                                   !Output
!     &  INTEGR)                                         !Control

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF

C-----------------------------------------------------------------------
      SELECT CASE (DYNAMIC)
        CASE (1)
	    SUBTITLE = .TRUE.

	  CASE (2)
          SUBTITLE = .TRUE.
	
        CASE (3)	  
          SUBTITLE = .TRUE.

	  CASE (4)
	    IF (SUBTITLE) THEN        !for portability
            SUBTITLE = .FALSE.
          ELSE
            SUBTITLE = .TRUE. 
          END IF
      END SELECT

      CALL PRNT_VEGGR_SC(
     &  DYNAMIC, SUBTITLE, 
     &  YRDOY, YRSIM,                         !Input
     &  AGRLF, AGRRT, AGRSTM, AGRSU,          !Input
     &  CMINEP, CSAVEV,                       !Input
     &  DTX,                                  !Input
     &  FNINL, FNINR, FNINS, FNINSU,          !Input
     &  NAVL, NDMNEW, NDMOLD, NMINEA,         !Input
     &  PAR, PCH2O, PG, PGAVL,                !Input
     &  ROWSPC,                               !Input
     &  STMWT, TRNU, TURFAC, VSTAGE,          !Input
     &  WCRLF, WCRRT, WCRST,  WCRSU,          !Input
     &  WTLF, XLAI,                           !Input

     &  AGRVG, FRLF, FRRT, FRSTM, FRSU,       !Input/Output

     &  CADLF, CADST, CADSU,                  !Output
     &  CMINEA,                               !Output
     &  CRUSLF, CRUSRT, CRUSST, CRUSSU,       !Output 
     &  EXCESS, NADLF, NADRT, NADST, NADSU,   !Output 
     &  NGRLF, NGRRT, NGRST, NGRSU, NSTRES,   !Output
     &  TNLEAK,                               !Output 
     &  WLDOTN, WRDOTN, WSDOTN)               !Output

!***********************************************************************
      RETURN
C-----------------------------------------------------------------------
      END ! SUBROUTINE CSP_VEGGR
C-----------------------------------------------------------------------

      SUBROUTINE PRNT_VEGGR_SC(
     &  DYNAMIC, SUBTITLE, 
     &  YRDOY, YRSIM,                         !Input
     &  AGRLF, AGRRT, AGRSTM, AGRSU,          !Input
     &  CMINEP, CSAVEV,                       !Input
     &  DTX,                                  !Input
     &  FNINL, FNINR, FNINS, FNINSU,          !Input
     &  NAVL, NDMNEW, NDMOLD, NMINEA,         !Input
     &  PAR, PCH2O, PG, PGAVL,                !Input
     &  ROWSPC,                               !Input
     &  STMWT, TRNU, TURFAC, VSTAGE,          !Input
     &  WCRLF, WCRRT, WCRST,  WCRSU,          !Input
     &  WTLF, XLAI,                           !Input

     &  AGRVG, FRLF, FRRT, FRSTM, FRSU,       !Input/Output

     &  CADLF, CADST, CADSU,                  !Output
     &  CMINEA,                               !Output
     &  CRUSLF, CRUSRT, CRUSST, CRUSSU,       !Output 
     &  EXCESS, NADLF, NADRT, NADST, NADSU,   !Output 
     &  NGRLF, NGRRT, NGRST, NGRSU, NSTRES,   !Output
     &  TNLEAK,                               !Output 
     &  WLDOTN, WRDOTN, WSDOTN)               !Output 

      IMPLICIT NONE
     
      INTEGER DYNAMIC, OpenStatus
      INTEGER YRDOY, YRSIM

      REAL AGRLF, AGRRT, AGRSTM, AGRSU
      REAL CMINEP, CSAVEV
      REAL DTX
      REAL FNINL, FNINR, FNINS, FNINSU
      REAL NAVL, NDMNEW, NDMOLD, NMINEA
      REAL PAR, PCH2O, PG, PGAVL
      REAL ROWSPC
      REAL STMWT, TRNU, TURFAC, VSTAGE
      REAL WCRLF, WCRRT, WCRST,  WCRSU
      REAL WTLF, XLAI

      REAL AGRVG, FRLF, FRRT, FRSTM, FRSU        !Input/Output

      REAL CADLF, CADST, CADSU
      REAL CMINEA
      REAL CRUSLF, CRUSRT, CRUSST, CRUSSU
      REAL EXCESS, NADLF, NADRT, NADST, NADSU
      REAL NGRLF, NGRRT, NGRST, NGRSU, NSTRES
      REAL TNLEAK
      REAL WLDOTN, WRDOTN, WSDOTN

	LOGICAL SUBTITLE


      SELECT CASE (DYNAMIC)
        CASE (1)

      OPEN(UNIT = 600, FILE = "TestVeggr_SC.out", STATUS = "UNKNOWN", 
     & ACTION = "WRITE", POSITION = "REWIND", IOSTAT = OpenStatus)

      WRITE(600,'(1X,"RESULTS FROM VEGGR_SC.for")')
      WRITE(600,*)
      WRITE(600,'(1X,"  YRDOY   YRSIM")', ADVANCE="NO") 
      WRITE(600,'(1X,"AGRLF AGRRT AGRSTM AGRSU")', ADVANCE="NO") 
      WRITE(600,'(1X,"CMINEP CSAVEV")', ADVANCE="NO")
      WRITE(600,'(1X,"  DTX")', ADVANCE="NO") 
      WRITE(600,'(1X,"FNINL FNINR FNINS FNINSU")', ADVANCE="NO") 
      WRITE(600,'(1X," NAVL NDMNEW NDMOLD NMINEA")', ADVANCE="NO") 
      WRITE(600,'(1X,"  PAR PCH2O    PG PGAVL")', ADVANCE="NO")
      WRITE(600,'(1X,"ROWSPC")', ADVANCE="NO") 
      WRITE(600,'(1X,"  STMWT  TRNU TURFAC VSTAGE")', ADVANCE="NO")
      WRITE(600,'(1X,"WCRLF WCRRT WCRST WCRSU")', ADVANCE="NO")
      WRITE(600,'(1X,"   WTLF  XLAI")', ADVANCE="NO") 

      WRITE(600,'(1X,"AGRVG  FRLF  FRRT FRSTM  FRSU")', ADVANCE="NO") 

      WRITE(600,'(1X,"CADLF CADST CADSU")', ADVANCE="NO") 
      WRITE(600,'(1X,"CMINEA")', ADVANCE="NO") 
      WRITE(600,'(1X,"CRUSLF CRUSRT CRUSST CRUSSU")', ADVANCE="NO") 
      WRITE(600,'(1X,"EXCESS NADLF NADRT NADST NADSU")', 
     &	  ADVANCE="NO") 
      WRITE(600,'(1X,"NGRLF NGRRT NGRST NGRSU NSTRES")', 
     &      ADVANCE="NO")
      WRITE(600,'(1X,"TNLEAK")', ADVANCE="NO") 
      WRITE(600,'(1X,"WLDOTN WRDOTN WSDOTN")') 

	  CASE (2)
          WRITE(600,'(1X,"SEASINIT")')
	
        CASE (3)	  
          WRITE(600,'(1X,"EMERG")')

	  CASE (4)
	    IF (SUBTITLE) THEN
		    WRITE(600,'(1X,"INTEGR")')
            SUBTITLE = .FALSE.
          ELSE
            SUBTITLE = .TRUE. 
          END IF
      END SELECT

      IF (SUBTITLE) THEN
        WRITE(600,'(2(1X,I7))', ADVANCE = "NO") YRDOY, YRSIM
        WRITE(600,'(2(1X,F5.2),1X,F6.2,1X,F5.2)', ADVANCE = "NO") 
     &        AGRLF, AGRRT, AGRSTM, AGRSU
        WRITE(600,'(2(1X,F6.2))', ADVANCE = "NO") 
     &        CMINEP, CSAVEV
        WRITE(600,'(1X,F5.2)', ADVANCE = "NO") 
     &        DTX
        WRITE(600,'(3(1X,F5.2),1X,F6.2)', ADVANCE = "NO") 
     &        FNINL, FNINR, FNINS, FNINSU
        WRITE(600,'(1X,F5.2,3(1X,F6.2))', ADVANCE = "NO") 
     &        NAVL, NDMNEW, NDMOLD, NMINEA
        WRITE(600,'(4(1X,F5.2))', ADVANCE = "NO") 
     &        PAR, PCH2O, PG, PGAVL
        WRITE(600,'(1X,F6.2)', ADVANCE = "NO") 
     &        ROWSPC
        WRITE(600,'(1X,F7.2,1X,F5.2,2(1X,F6.2))', ADVANCE = "NO") 
     &        STMWT, TRNU, TURFAC, VSTAGE
        WRITE(600,'(4(1X,F5.2))', ADVANCE = "NO") 
     &        WCRLF, WCRRT, WCRST, WCRSU
        WRITE(600,'(1X,F7.2,1X,F5.2)', ADVANCE = "NO") 
     &        WTLF, XLAI
        WRITE(600,'(5(1X,F5.2))', ADVANCE = "NO") 
     &        AGRVG, FRLF, FRRT, FRSTM, FRSU       !Input/Output
        WRITE(600,'(3(1X,F5.2))', ADVANCE = "NO") 
     &        CADLF, CADST, CADSU
        WRITE(600,'(1X,F5.2)', ADVANCE = "NO")       
     &        CMINEA
        WRITE(600,'(4(1X,F6.2))', ADVANCE = "NO") 
     &        CRUSLF, CRUSRT, CRUSST, CRUSSU
        WRITE(600,'(1X,F6.2,5(1X,F5.2))', ADVANCE = "NO")       
     &        EXCESS, NADLF, NADRT, NADST, NADSU
        WRITE(600,'(4(1X,F5.2),1X,F6.2)', ADVANCE = "NO") 
     &        NGRLF, NGRRT, NGRST, NGRSU, NSTRES
        WRITE(600,'(1X,F6.2)', ADVANCE = "NO") 
     &        TNLEAK
        WRITE(600,'(3(1X,F6.2))') 
     &        WLDOTN, WRDOTN, WSDOTN
      END IF  

	RETURN

	END

C-----------------------------------------------------------------------
! AGRLF   Mass of CH2O required for new leaf growth (g [CH2O] / g [leaf])
! AGRRT   Mass of CH2O required for new root growth (g [CH2O] / g [root])
! AGRSTM  Mass of CH2O required for new stem growth (g [CH2O] / g [stem])
! AGRVG   Mass of CH2O required for vegetative tissue growth including 
!           stoichiometry and respiration (g [CH2O] / g [tissue])
! ATOP    Maximum fraction change in partitioning from top growth to roots 
!           if severe water or nitrogen stresses occur. 
! CADLF   Mass of CH2O added to leaf reserves after growth
!           (g [CH2O] / m2 / d)
! CADST   Mass of CH2O added to stems (g [CH2O] / m2 / d)
! CADSTF  Proportion of CH2O reserves that are added to stems (fraction)
! CANHT   Canopy height (m)
! CANWH   Canopy width normal to row (m)
! CMINEA  Actual carbon mined from vegetative tissue (g [CH2O] / m2 - d)
! CMINEP  Potential CH2O mobilization from storage (g [CH2O] / m2 - d)
! CMOBMX  Maximum C pool mobilization rate (g [CH2O] / m2 - d)
! CRUSLF  C mobilized from leaf tissue in a day (g [CH2O] / m2 - d)
! CRUSRT  C mobilized from root tissue in a day (g [CH2O] / m2 - d)
! CRUSSH  C mobilized from shell tissue in a day (g [CH2O] / m2 - d)
! CRUSST  C mobilized from stem tissue in a day (g [CH2O] / m2 - d)
! CSAVEV  Fraction of PG for VEG that is stored as CH2O 
! CUMTUR  Cumulative turgor factor - 20 day water stress average 
! DAS     Days after start of simulation (days)
! DTX     Thermal time that occurs in a real day based on vegetative 
!           development temperature function (thermal days / day)
! ECONO   Ecotype code - used to match ECOTYP in .ECO file 
! ERR     Error code for file operation 
! ERRKEY  Subroutine name for error file 
! EXCESS  Factor based on excess PG used to affect tomorrow's PG 
!           calculation 
! FILECC  Path plus filename for species file (*.spe) 
! FILEGC  Pathname plus filename for ECO file 
! FNINL   Maximum fraction of N for growing leaf tissue (g [N] / g [leaf])
! FNINLG  Minimum fraction of N for growing leaf tissue (g [N] / g [leaf])
! FNINR   Maximum fraction of N for growing root tissue (g [N] / g [root])
! FNINRG  Minimum fraction of N for growing root tissue (g [N] / g [root])
! FNINS   Maximum fraction of N for growing stem tissue (g [N] / g [stem])
! FNINSG  Minimum fraction of N for growing stem tissue (g [N] / g [stem])
! FOUND   Indicator that good data was read from file by subroutine FIND (0 
!           - End-of-file encountered, 1 - NAME was found) 
! FRLF    Fraction of vegetative tissue growth that goes to leaves on a day
!           (g [leaf] / g [veg])
! FRRT    Fraction of vegetative tissue growth that goes to roots on a day
!           (g [root] / g [veg])
! FRSTM   Fraction of vegetative tissue growth that goes to stems on a day
!           (g [stem] / g [veg])
! ISECT   Data record code (0 - End of file encountered, 1 - Found a good 
!           line to read, 2 - End of Section in file encountered, denoted 
!           by * in column 1
! LNUM    Current line number of input file 
! LSTR    Ratio of excess C to be added to leaves in a day relative to the 
!           amount to be stored in stems 
! LUNCRP  Logical unit number for FILEC (*.spe file) 
! NADLF   N added to leaf N reserves (g [N] / m2 / d)
! NADRAT  Total nitrogen added to vegetative N reserves (g [N] / m2 - d)
! NADRT   N added to root N reserves (g [N] / m2 / d)
! NADST   N added to stem N reserves (g [N] / m2 / d)
! NAVL    Total mass of nitrogen available for growth (g [N] / m2 / d)
! NDMNEW  Total N demand for new growth (g [N] / m2 / d)
! NDMOLD  N demand for old tissue (g [N] / m2 / d)
! NFIXN   Amount of N fixed during the day (g [N] / m2 / d)
! NGRLF   Maximum N demand for leaf growth (g [leaf N] / m2[ground] / d)
! NGRLFG  Minimum N requirement for leaf growth
!           (g [leaf N] / m2[ground] / d)
! NGRRT   Maximum N demand for root growth (g [root N] / m2[ground] / d)
! NGRRTG  Minimum N requirement for root growth
!           (g [leaf N] / m2[ground] / d)
! NGRST   Maximum N demand for stem growth (g [stem N] / m2[ground] / d)
! NGRSTG  Minimum N requirement for stem growth
!           (g [leaf N] / m2[ground] / d)
! NGRVEG  Maximum N demand for vegetative tissue growth
!           (g [leaf N] / m2[ground] / d)
! NGRVGG  Minimum N requirement for vegetative tissue growth
!           (g [leaf N] / m2[ground] / d)
! NLEAK   Nitrogen leak (g [N] / m2 - d)
! NLEFT   Nitrogen left after vegetative demands are met (g [N] / m2 - d)
! NMINEA  Actual Nitrogen mined from existing tissue (g [N] / m2 / d)
! NRATIO  Factor to reduce tissue growth based on low available nitrogen 
! NSTRES  Nitrogen stress factor (1=no stress, 0=max stress) 
! PAR     Daily photosynthetically active radiation or photon flux density
!           (moles[quanta]/m2-d)
! PCH2O   Respiration loss due to storage/mobilization of CH2O
!           (g [CH2O] / g [CH2O])
! PG      Daily gross photosynthesis (g [CH2O] / m2 / d)
! PGAVL   Total available CH2O available for growth & respiration
!           (g [CH2O] / m2)
! PGLEFT  Excess PG after today's tissue growth (g [CH2O] / m2)
! PROLFG  Normal growth protein composition in leaves during growth
!           (g [protein] / g [leaf tissue])
! PROLFI  Maximum protein composition in leaves during growth with 
!           luxurious supply of N (g [protein] / g [leaf tissue])
! PROLFT  Protein fraction of new leaf growth (g [protein] / g [leaf tissue])
! PRORTG  Normal growth protein composition in roots during growth
!           (g [protein] / g [root])
! PRORTI  Maximum protein composition in roots during growth with luxurious 
!           supply of N (g [protein] / g [root])
! PRORTT  Protein fraction of new root growth (g [protein] / g [root])
! PROSTG  Normal growth protein composition in stems during growth
!           (g [protein] / g [stem])
! PROSTI  Maximum protein composition in stems during growth with luxurious 
!           supply of N (g [protein] / g [stem])
! PROSTT  Protein fraction of new root growth (g [protein] / g [stem])
! ROWSPC  Row spacing (m)
! RVSTGE  Rate of VSTAGE change (nodes/day)
! SECTION Section name in input file 
! STMWT   Dry mass of stem tissue, including C and N (g [stem] / m2[ground)
! SUPPN   Total supply of N (g [N] / m2 / d)
! TGRO(I) Hourly air temperature (°C)
! TIMDIF  Integer function which calculates the number of days between two 
!           Julian dates (da)
! TNLEAK  Total nitrogen leak (g [N] / m2 / d)
! TRNU    Total N uptake in a day (g [N] / m2 / d)
! TS      Number of intermediate time steps (=24) 
! TURFAC  Water stress factor for expansion (0 - 1) 
! VGRDEM  Vegetative growth demand (g [vegetative tissue] / m2-d)
! VSTAGE  Number of nodes on main stem of plant 
! WCRLF   Mass of CH2O reserves in leaves (g [leaf CH2O] / m2[ground])
! WCRRT   Mass of CH2O reserves in roots (g [root CH2O] / m2[ground])
! WCRSH   Mass of CH2O reserves in shells (g [shell CH2O] / m2[ground])
! WCRST   Mass of CH2O reserves in stems (g [stem CH2O] / m2[ground])
! WLDOTN  Dry weight growth rate of new leaf tissue including N but not C 
!           reserves (g [leaf] / m2[ground]-d)
! WRDOTN  Dry weight growth rate of new root tissue including N but not C 
!           reserves (g [root] / m2[ground]-d)
! WSDOTN  Dry weight growth rate of new stem tissue including N but not C 
!           reserves (g [stem] / m2[ground]-d)
! WTLF    Dry mass of leaf tissue including C and N (g [leaf] / m2[ground])
! XLAI    Leaf area (one side) per unit of ground area
!           (m2[leaf] / m2[ground])
! YRDOY   Current day of simulation (YYDDD)
! YRSIM   Start of simulation date (YYDDD)
!
!-----------------------------------------------------------------------
! LIST OF VARIABLES ADDED TO ENABLE SIMULATION OF SUGARS IN THE 
! SUGARCANE MODEL
!-----------------------------------------------------------------------
! AGRSU    Mass of CH2O required for new sugars growth (g [CH2O] / g [sugar])
! ALPHSU   Fraction of new sugars growth that is mobile C (fraction)
! CPFSU    Respiration requirement for net sugars growth
!            (g [CH20] / g [tissue]) +++
! CRUSSU   C mobilized from sugars storage in a day (g [CH2O] / m2 - d)
! CSUW     Cumulative sugar accumulation growth (g [sugar] / m2)
! NADSU    N added to sugars N reserves (g [N] / m2 - d)
! NGRSU    Maximum N demand for sugars growth (g [sugars N] / m2 [ground] - d)
! NGRSUG   Minimum N requirement for sugars accumulation 
!            (g [sugars N] / m2 [ground] - d)
! NRUSSU   N actually mobilized from sugars in a day (g [N] / m2 - d)
! NSUDOT   Net N addition for sugars (g [N] / m2 [ground] - d)
! NSUOFF   N loss from sugars in a day (g [N]/m2 - d)
! NSUALL   N added to sugars today (g [N] / m2 - d)
! PCNSU    Percentage of N in sugars (100 g [N] / g [sugars]) +++
! PCARSU   Proportion of sugars that is carbohydrate (fraction)
! PLIGSU   Proportion of sugars that is lignin (fraction)
! PLIPSU   Proportion of sugars that is lipid (fraction)
! PMINSU   Proportion of sugars that is mineral (fraction)
! POASU    Proportion of sugars that is organic acid (fraction)
! PROSUF   Minimum sugars protein composition after N mining
!            (g [protein] / g [sugars])
! PROSUG   Normal growth protein composition in sugars during growth
!            (g [protein] / g [sugar])
! PROSUI   Maximum protein composition in sugars during growth with 
!            luxurious supply of N (g [protein] / g [sugars])
! PROSUT   Protein fraction of new sugars growth (g [protein] / g [sugar])
! RHOSU    Fraction of sugars which is carbohydrate (g [CH20] / g[sugars])
! SUWT     Dry mass of sugars, including C and N
!            (g [sugars] / m2 [ground])
! WCRSU    Mass of CH2O reserves in sugars (g [sugars CH2O] / m2 [ground])
! WNRSU    N available for mobilization from sugars above lower limit of 
!            mining (g [N] / m2)
! WRCSUDT  Net C addition for sugars (g [CH2O] / m2 - d)
! WSUDOT   Net sugars growth rate (g [sugars] / m2 - d)
! WSUDOTN  Dry weight growth rate of new sugars including N but not C 
!            reserves (g [sugars] / m2 [ground] - d)
! WSUI     Initial weight of sugars (g [leaf] / m2)
! WTNSU    Mass of N in sugars (g [sugars N] / m2 [ground])
! WTNSUA   Cumulative N added to sugars (g [N] / m2 - d)
! WTNSUO   Cumulative N loss from sugars (g [N] / m2)
! WTSUO    Cumulative sugar losses (g [sugars] / m2)
!-----------------------------------------------------------------------
!     END SUBROUTINE CSP_VEGGR
!-----------------------------------------------------------------------
