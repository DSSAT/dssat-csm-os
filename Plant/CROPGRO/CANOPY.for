C=======================================================================
C  CANOPY, Subroutine, G. Hoogenboom, K.J. Boote, J.W. Jones
C  Calculates canopy height and canopy width as a function of V-Stage,
C  air temperature, drought stress, daylength, and radiation.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  05/01/1989 Written.
C  04/24/1994 NBP Changed TAIRHR to TGRO.  Edited indentation.
C  01/19/1996 KJB Include PAR effect on expansion.
C  07/15/1998 CHP Modified for modular format
C  05/15/1999 GH  Incorporated into CROPGRO
C  01/22/2003 KJB Add checks for minimum canopy height and width.
C  08/12/2003 CHP Revised I/O error checking
C  06/30/2004 CHP/CDM Added KC_SLOPE to SPE file and KC_ECO to ECO file.
C                 Added optional KCAN to ECO file.
C-----------------------------------------------------------------------
C  Called : VEGGR
C  Calls  : ERROR, FIND, IGNORE
C========================================================================

      SUBROUTINE CANOPY(DYNAMIC, 
     &    ECONO, FILECC, FILEGC, KCAN, PAR, ROWSPC,       !Input
     &    RVSTGE, TGRO, TURFAC, VSTAGE, XLAI, NSTRES,     !Input
     &    CANHT, CANWH)                                   !Output

C-----------------------------------------------------------------------
      USE ModuleDefs
      IMPLICIT NONE
      EXTERNAL GETLUN, FIND, ERROR, IGNORE, TABEX
      SAVE

      CHARACTER*6 ERRKEY
      PARAMETER (ERRKEY = 'CANOPY')

      CHARACTER*6   SECTION
      CHARACTER*6   ECOTYP, ECONO
      CHARACTER*92  FILECC, FILEGC
      CHARACTER*255 C255

      INTEGER I, II, LUNCRP, LUNECO, ERR, LINC, LNUM, ISECT
      INTEGER DYNAMIC
      INTEGER FOUND

      REAL PAR, ROWSPC, RVSTGE, TURFAC, VSTAGE
      REAL CANHT, CANWH, XLAI
      REAL KCAN, RHGHT, RWIDTH
      REAL HWTEM, RCANHT, RCANWH, PARNOD, HPAR, WPAR
      REAL TABEX
      REAL XHWPAR(10), XHWTEM(10), YHWPAR(10), YHWTEM(10)
      REAL XVSHT(15), YVSHT(15), YVSWH(15)
      REAL TGRO(TS)
      
! Fo Cotton-Nitrogen
      REAL NHGT,HNHGT,NSTRES,CUMNHT

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
!    Find and Read Photosynthesis Section
!-----------------------------------------------------------------------
!     Subroutine FIND finds appropriate SECTION in a file by
!     searching for the specified 6-character string at beginning
!     of each line.
!-----------------------------------------------------------------------
!CHP 7/30/2004 - Get KCAN from main routine.
!                May be overriden by value in ECOTYPE file.
!      SECTION = '!*PHOT'
!      CALL FIND(LUNCRP, SECTION, LINC, FOUND); LNUM = LNUM + LINC
!      IF (FOUND .EQ. 0) THEN
!        CALL ERROR(SECTION, 42, FILECC, LNUM)
!      ELSE
!        ISECT = 2
!        DO WHILE (ISECT .NE. 1)
!          CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
!          IF (ISECT .EQ. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
!        ENDDO
!        READ(C255,'(12X,F6.0)',IOSTAT=ERR) KCAN
!        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
!      ENDIF
!-----------------------------------------------------------------------
C     ***** READ CANOPY HEIGHT & WIDTH PARAMETERS ******************
C-----------------------------------------------------------------------
      SECTION = '!*CANO'
      CALL FIND(LUNCRP, SECTION, LINC, FOUND); LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
        READ(C255,'(10F6.0)',IOSTAT=ERR)(XVSHT(II),II = 1,10)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
        READ(C255,'(10F6.0)',IOSTAT=ERR)(YVSHT(II),II = 1,10)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
        READ(C255,'(10F6.0)',IOSTAT=ERR)(YVSWH(II),II = 1,10)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
        READ(C255,'(5F6.0)',IOSTAT=ERR)(XHWTEM(II),II = 1,5)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
        READ(C255,'(5F6.0)',IOSTAT=ERR)(YHWTEM(II),II = 1,5)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
        READ(C255,'(8F6.0)',IOSTAT=ERR)(XHWPAR(II),II = 1,8)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
        READ(C255,'(8F6.0)',IOSTAT=ERR)(YHWPAR(II),II = 1,8)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
        
        CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
        READ(C255,'(F6.0)',IOSTAT=ERR) NHGT
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
      ENDIF

      CLOSE (LUNCRP)

C-----------------------------------------------------------------------
C    Read Ecotype Parameter File
C-----------------------------------------------------------------------
      CALL GETLUN('FILEE', LUNECO)
      OPEN (LUNECO,FILE = FILEGC,STATUS = 'OLD',IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEGC,0)
      ECOTYP = '      '
      LNUM = 0
      DO WHILE (ECOTYP .NE. ECONO)
        CALL IGNORE(LUNECO, LNUM, ISECT, C255)
          IF ((ISECT .EQ. 1) .AND. (C255(1:1) .NE. ' ') .AND.
     &        (C255(1:1) .NE. '*')) THEN
          READ (C255,'(A6,90X,2(1X,F5.0))',IOSTAT=ERR)
     &        ECOTYP, RWIDTH, RHGHT
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEGC,LNUM)
          IF (ECOTYP .EQ. ECONO) THEN
              EXIT
          ENDIF

        ELSE IF (ISECT .EQ. 0) THEN
          IF (ECONO .EQ. 'DFAULT') CALL ERROR(ERRKEY,35,FILEGC,LNUM)
          ECONO = 'DFAULT'
          REWIND(LUNECO)
          LNUM = 0
        ENDIF
      ENDDO

      CLOSE (LUNECO)

      CANHT = 0.0
      CANWH = 0.0

!***********************************************************************
!***********************************************************************
!     SEASONAL INITIALIZATION 
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      CANHT = 0.0
      CANWH = 0.0
      
      HNHGT = 1.0
      CUMNHT= 1.0

!***********************************************************************
!***********************************************************************
!     EMERGENCE CALCULATIONS - Performed once per season upon emergence
!         or transplanting of plants
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. EMERG) THEN
!-----------------------------------------------------------------------
        CANHT  = TABEX(YVSHT,XVSHT,VSTAGE,10)       
        CANWH  = TABEX(YVSWH,XVSHT,VSTAGE,10)       

!***********************************************************************
!***********************************************************************
!     DAILY RATE/INTEGRATION
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------
C     Calculate effect of temperature on canopy expansion, HWTEM
C-----------------------------------------------------------------------
      HWTEM = 0.0
      DO I = 1, TS
        HWTEM = HWTEM + TABEX(YHWTEM,XHWTEM,TGRO(I),5)
      ENDDO
      HWTEM = HWTEM /TS
C       24 changed to TS on 5 July 2017 by Bruce Kimball

C-----------------------------------------------------------------------
C     Calculate effect of day's PAR on canopy expansion, HPAR.
C     ASSUME THAT UPPER 30% OF CANOPY SHADES THE GROWING POINT
C     WPAR IS EFFECT ON WIDTH.  SHADE DOES NOT MAKE MUCH WIDER. LOWER K?
C-----------------------------------------------------------------------
C     IF (XLAI .GT. 0.1) THEN
C        PARNOD = PAR * EXP(-KCAN*0.3*(XLAI-0.1))
C     ELSE
C        PARNOD = PAR
C     ENDIF
C-----------------------------------------------------------------------
      PARNOD = PAR * EXP(-KCAN*(0.3*XLAI))
      HPAR = TABEX(YHWPAR,XHWPAR,PARNOD,8)
      WPAR = TABEX(YHWPAR,XHWPAR,PAR,8)
C-----------------------------------------------------------------------
!     Nitogen effect on canopy height and width by KJB
C-----------------------------------------------------------------------
      IF(NHGT .GT. 1.4)THEN                     !To limit NGHT to 1.4
          NHGT = 1.4 
      ENDIF
      HNHGT = MAX(0.1, (1.0 - (1.0 - NSTRES)*NHGT))
      CUMNHT = 0.75*CUMNHT + 0.25*HNHGT
C-----------------------------------------------------------------------
C     Calculate rate of increase in canopy height and update height, CANHT
!     KJB - Added CUMNHT to RCANWH calculation
C-----------------------------------------------------------------------
      RCANHT= RVSTGE * TABEX(YVSHT,XVSHT,VSTAGE,10) * HWTEM *
     &  TURFAC * HPAR * RHGHT * CUMNHT
      CANHT = CANHT + RCANHT

!     Set minimum Canopy height based on lookup function
      CANHT = MAX(CANHT, TABEX(YVSHT,XVSHT, 0.0, 10))

C-----------------------------------------------------------------------
C     Calculate rate of increase in canopy width and update width, CANWH
C     RWIDTH,RHGHT are used to normalize other crops to the values in tables
C     Values of RHGHT and RWIDTH = 1.00 are for Florunner peanut variety
C     1/22/03 KJB - Don't allow reduction in vstage to reduce canopy
C       width.
!     KJB - Added CUMNHT to RCANWH calculation
!-----------------------------------------------------------------------
      RCANWH= MAX(0.0,RVSTGE) * TABEX(YVSWH,XVSHT,VSTAGE,10) * HWTEM *
     &  TURFAC * WPAR * RWIDTH * CUMNHT
      CANWH = CANWH + RCANWH 

!     Set minimum Canopy width based on lookup function
      CANWH = MAX(CANWH, TABEX(YVSWH, XVSHT, 0.0, 10))  
      CANWH = MIN(CANWH,ROWSPC)

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END ! SUBROUTINE CANOPY
!=======================================================================
! CANOPY Definitions:  updated 25 Feb 2004
!-----------------------------------------------------------------
! C255      255-character record read from file 
! CANHT     Canopy height (m)
! CANWH     Canopy width normal to row (m)
! ECONO     Ecotype code - used to match ECOTYP in .ECO file 
! ECOTYP    Ecotype code for this simulation 
! ERR       Error code for file operation 
! FILECC    Path plus filename for species file (*.spe) 
! FILEGC    Pathname plus filename for ECO file 
! FOUND     Indicator that good data was read from file by subroutine FIND 
!             (0 - End-of-file encountered, 1 - NAME was found) 
! HPAR      Effect of day's PAR on canopy expansion 
! HWTEM     Effect of temperature on canopy expansion 
! ISECT     Indicator of completion of IGNORE routine: 0 - End of file 
!             encountered, 1 - Found a good line to read, 2 - End of 
!             Section in file encountered denoted by * in column 1. 
! KCAN      Canopy light extinction coefficient for daily PAR, for 
!             equidistant plant spacing, modified when in-row and between 
!             row spacing are not equal 
! LINC      Line number of input file 
! LNUM      Current line number of input file 
! LUNCRP    Logical unit number for FILEC (*.spe file) 
! LUNECO    Logical unit number for FILEE (*.eco file) 
! PAR       Daily photosynthetically active radiation or photon flux 
!             density (moles[quanta]/m2-d)
! PARNOD    Effective PAR at growth point (moles[quanta]/m2-d)
! RCANHT    Rate of increase in canopy height (m/d)
! RCANWH    Rate of increase in canopy width (m/d)
! RHGHT     Relative height of this ecotype in comparison to the standard 
!             height per node (YVSHT) defined in the species file (*.SPE) 
! ROWSPC    Row spacing (m)
! RVSTGE    Rate of VSTAGE change (nodes/day)
! RWIDTH    Relative width of this ecotype in comparison to the standard 
!             width per node (YVSWH) defined in the species file (*.SPE) (m)
! SECTION   Section name in input file 
! TGRO(I)   Hourly canopy temperature (°C)
! TURFAC    Water stress factor for expansion (0 - 1) 
! VSTAGE    Number of nodes on main stem of plant (nodes)
! WPAR      Effect of PAR on canopy width 
! XHWPAR(I) PAR values for table look-up for modifying height and width 
!             growth rate, particularly to allow etiliolation at low PAR 
!             values (mol/day)
! XHWTEM    Temperatures in a table look-up function for modifying height 
!             and width growth rates (°C)
! XLAI      Leaf area (one side) per unit of ground area
!            (m2[leaf] / m2[ground])
! XVSHT     Node number on main stem for use in computing height and width 
!             growth rates 
! YHWPAR(I) Relative increase in height and width growth rates with low PAR 
!             as given in XHWPAR 
! YHWTEM(I) Relative (0-1) expansion in height and width with temperatures 
!             given in XHWTEM 
! YVSHT     Length of internode (m) Vs position on the main stem defined by 
!             XVSHT (m/node)
! YVSWH     Increase in canopy width per node developed on the main stem
!            (m/node)
!***********************************************************************
!      END SUBROUTINE CANOPY
!=======================================================================

