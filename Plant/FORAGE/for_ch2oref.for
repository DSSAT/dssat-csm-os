C=======================================================================
C  FOR_CH2OREF Subroutine 5/31/05 SJR 
C  Allow refilling of mobile CH2O during non-stressed, vegetative growth.
C----------------------------------------------------------------------
C  Called by: CROPGRO
C  Calls    : None
C=======================================================================
        SUBROUTINE FOR_CH2OREF(CONTROL,
     &  ALPHL, ALPHR, ALPHS, ALPHSR, CADPV, CRREF,        !INPUT
     &  LFSCMOB, LRREF, PG, PRREF, RTSCMOB,               !INPUT
     &  RTWT, SLDOT, SRDOT, SRSCMOB,                      !INPUT
     &  SSDOT, SSRDOT, STMWT, STRWT, STSCMOB,             !INPUT
     &  TYPCREF, TYPLREF, TYPPREF, WCRLF, WCRRT,          !INPUT
     &  WCRSR, WCRST,                                     !INPUT
     &  WTLF, XLAI,                                       !INPUT
     &  CDEBIT, CADVG, PGAVL,                             !INPUT/OUTPUT
     &  LFCDEBT, RTCDEBT, SRCDEBT, STCDEBT )              !OUTPUT

! 2023-01-18 CHP removed unused variables from argument list:
! ALPHSH, LFSNMOB, RTSNMOB, SHELWT, SRSNMOB, STSNMOB, WCRSH,

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
        ! which contain control information, soil
        ! parameters, hourly weather data.
      IMPLICIT NONE
      EXTERNAL FIND, ERROR, GETLUN, IGNORE, CURV
      SAVE
      CHARACTER*3 TYPCREF, TYPLREF, TYPPREF 
      CHARACTER*1  BLANK
      CHARACTER*6  ERRKEY, SECTION
      CHARACTER*12 FILEC, FILEE
      CHARACTER*30 FILEIO
      CHARACTER*80 PATHCR,CHAR, PATHEC
      CHARACTER*80  C80
      CHARACTER*92 FILECC

      INTEGER LUNIO, LNUM, FOUND
      INTEGER DYNAMIC, II, PATHL, LUNCRP, ERR, LINC, ISECT

      REAL ALPHL, ALPHR, ALPHS, ALPHSR, CADPV, CADVG,  !ALPHSH, 
     &   PG, PGAVL, PGLFMX, PHTMAX, RTWT, STMWT,  !SHELWT,    
     &   STRWT, WCRLF, WCRRT, WCRSR, WCRST, WTLF, XLAI  !WCRSH, 

      REAL CRREF(4), LRREF(4), PRREF(4)
      REAL CFILL, LFILL, PFILL, CSTATUS, CDEBIT, LFCCAP, 
     &   PGSTATUS, PCHOLFF, PCHORTF, PCHOSRF, PCHOSTF,
     &   RTCCAP, SRCCAP, STCCAP
      REAL LFMCCAP, RTMCCAP, SRMCCAP, STMCCAP
      REAL LFSCMOB, RTSCMOB, !LFSNMOB, RTSNMOB, 
     &   SRSCMOB, STSCMOB   !SRSNMOB, , STSNMOB

      REAL LFCDEBT, RTCDEBT, SRCDEBT, STCDEBT
      REAL SLDOT, SRDOT, SSDOT, SSRDOT

      REAL CURV, LMXSTD, PGREF

      PARAMETER (BLANK  = ' ')
      PARAMETER (ERRKEY = 'CH2OREF')

!     The variable "CONTROL" is of constructed type "ControlType" as 
!     defined in ModuleDefs.for, and contains the following variables.
!     The components are copied into local variables for use here.
      TYPE (ControlType) CONTROL
      DYNAMIC = CONTROL % DYNAMIC
      FILEIO = CONTROL % FILEIO
      LUNIO  = CONTROL % LUNIO
C***********************************************************************
C     Run Initialization - Called once per simulation
C***********************************************************************

      IF (DYNAMIC .EQ. RUNINIT) THEN

C-----------------------------------------------------------------------
C     Initialize Refill variables
C-----------------------------------------------------------------------
      CDEBIT = 0.0
      LFCDEBT = 0.0
      STCDEBT = 0.0
      RTCDEBT = 0.0
      SRCDEBT = 0.0
      CADVG = 0.0


!-----------------------------------------------------------------------
!       Read data from FILEIO for use in PLANT module
!-----------------------------------------------------------------------
      OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,0)
!-----------------------------------------------------------------------
      READ(LUNIO,50) FILEC, PATHCR, FILEE, PATHEC
   50 FORMAT(/////,2(/,15X,A12,1X,A80))

!!-----------------------------------------------------------------------
!C    Read Soil Section - this caused an error for a crop rotation because
!       there is no soil section after first rotation.
!!-----------------------------------------------------------------------
!      SECTION = '*SOIL'
!      CALL FIND(LUNIO, SECTION, LNUM, FOUND)
!      IF (FOUND .EQ. 0) THEN
!        CALL ERROR(ERRKEY, 1, FILEIO, LINC)
!       ENDIF
!-----------------------------------------------------------------------
C    Read Cultivars Section (need 2nd occurance!)
!-----------------------------------------------------------------------
      SECTION = '*CULTI'
      CALL FIND(LUNIO, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILEIO, LINC)
      ENDIF
      CALL FIND(LUNIO, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILEIO, LINC)
      ELSE
        READ(LUNIO,'(66X,6X,F6.0)') LMXSTD
      ENDIF

C-----------------------------------------------------------------------
      CLOSE (LUNIO)

C-----------------------------------------------------------------------
      LINC = 0
      PATHL  = INDEX(PATHCR,BLANK)
      IF (PATHL .LE. 1) THEN
        FILECC = FILEC
      ELSE
        FILECC = PATHCR(1:(PATHL-1)) // FILEC
      ENDIF
      CALL GETLUN('FILEC', LUNCRP)
      OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,0)

C-----------------------------------------------------------------------
C READ PHOTOSYNTHESIS PARAMETERS *******************
C-----------------------------------------------------------------------
        LINC = 1
        SECTION = '!*PHOT'
        CALL FIND(LUNCRP, SECTION, LINC, FOUND)
        IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILECC, LINC)
        ELSE
        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        IF (ISECT .EQ. 0) CALL ERROR(ERRKEY,ERR,FILECC,LINC)
        READ(CHAR,'(6X,F6.2)',IOSTAT=ERR) PHTMAX
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LINC)
      
        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)

        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)

        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)

        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        CALL IGNORE(LUNCRP,LINC,ISECT,CHAR)
        READ(CHAR,'(24X,F6.0)',IOSTAT=ERR) PGREF
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LINC)
        ENDIF
!-----------------------------------------------------------------------
!    Find and Read Plant Composition Section
!-----------------------------------------------------------------------

        SECTION = '!*PLAN'
        CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
        IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILECC, LNUM)
        ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
  
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
  
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
  
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
  
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
  
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
  
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
  
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(4F6.0)',IOSTAT=ERR)
     &    PCHOLFF, PCHOSTF, PCHORTF, PCHOSRF
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)


        ENDIF



!-----------------------------------------------------------------------
!    Find and Read Carbon and Nitrogen Mining Section
!-----------------------------------------------------------------------
      SECTION = '!*CARB'
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(4(1X,F5.2),3X,A3)',IOSTAT=ERR)
     &    (CRREF(II),II=1,4), TYPCREF
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(4(1X,F5.2),3X,A3)',IOSTAT=ERR)
     &    (LRREF(II),II=1,4), TYPLREF
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(4(1X,F5.2),3X,A3)',IOSTAT=ERR)
     &    (PRREF(II),II=1,4), TYPPREF
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)


      ENDIF
C-----------------------------------------------------------------------
      CLOSE (LUNCRP)
C***********************************************************************
C     Daily Rate Calculations 
C***********************************************************************
      

      ELSE IF (DYNAMIC .EQ. RATE) THEN

C***********************************************************************
C     Daily Integration 
C***********************************************************************

      ELSE IF (DYNAMIC .EQ. INTEGR) THEN
      CDEBIT = 0.0
      LFCDEBT = 0.0
      STCDEBT = 0.0
      RTCDEBT = 0.0
      SRCDEBT = 0.0
      CADVG = 0.0



C-----------------------------------------------------------------------
!     Calculate current mobile CH2O status and deficit
C-----------------------------------------------------------------------
C      02/01/06 SJR Modified calculations of CSTATUS and xxCCAP for CH2O 
C      mobilization and loss from natural senescence
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C      Max CH2O capacity for leaves is ALPHL * today's leaf mass without CH2O
C      Today's leaf mass without CH2O is:
C      WTLF minus today's DM lost to natural senescence minus today's CH2O content
C      DM lost to natural senescence is SLMDOT corrected for N and CH2O 
C      mobilized prior to abscission (LFSNMOB and LFSCMOB).
C      Leaf CH2O content is WCRLF adjusted for CH2O lost with SLMDOT (SLMDOT*PCHOLFF)
C      CH2O REFILL capacity is the Max CH2O capacity described above minus 
C      today's CH2O content.
C-----------------------------------------------------------------------

      LFMCCAP = ALPHL * (WTLF - SLDOT - WCRLF)

      STMCCAP = ALPHS * (STMWT - SSDOT - WCRST)

      RTMCCAP = ALPHR * (RTWT - SRDOT - WCRRT)

      SRMCCAP = ALPHSR * (STRWT  - SSRDOT - WCRSR)
!-------------------------------------------------------------------
!  PDA 6/3/2010 Corrected math so that ALPHX is actually the maximum
!-------------------------------------------------------------------
!      LFMCCAP = (ALPHL/(1-ALPHL)) * (WTLF - SLDOT - WCRLF)

!      STMCCAP = (ALPHS/(1-ALPHS)) * (STMWT - SSDOT - WCRST)

!      RTMCCAP = (ALPHR/(1-ALPHR)) * (RTWT - SRDOT - WCRRT)

!      SRMCCAP = (ALPHSR/(1-ALPHSR)) * (STRWT  - SSRDOT - WCRSR)
!-------------------------------------------------------------------

!      SRMCCAP = ALPHSR * (STRWT  - SSRMDOT + SRSCMOB + SRSNMOB / 0.16 
!               &                         - WCRSR - SSRMDOT * PCHOSRF)
C-----------------------------------------------------------------------
C      C Status is current CH2O concentration as a proportion of Max CH2O capacity
C-----------------------------------------------------------------------
      CSTATUS = WCRLF + WCRRT + WCRSR + WCRST
      if(CSTATUS .gt. 0.0)then
         if(WTLF .gt. 0.0)then
            CSTATUS = CSTATUS - SLDOT*WCRLF/WTLF + LFSCMOB
         end if
         if(STMWT .gt. 0.0)then
            CSTATUS = CSTATUS - SSDOT*WCRST/STMWT + STSCMOB
         end if
         if(RTWT .gt. 0.0)then
            CSTATUS = CSTATUS - SRDOT*WCRRT/RTWT + RTSCMOB
         end if
         if(STRWT .gt. 0.0)then
            CSTATUS = CSTATUS - SSRDOT*WCRSR/STRWT + SRSCMOB
         end if
      end if
      if(CSTATUS .le. 0.0 .or.
     &   (LFMCCAP + STMCCAP + RTMCCAP + SRMCCAP) .le. 0.0)then
         CSTATUS = 0.0
      else
         CSTATUS = CSTATUS / (LFMCCAP + STMCCAP + RTMCCAP + SRMCCAP)
      end if
      
C-----------------------------------------------------------------------
C      CH2O REFILL capacity is the Max CH2O capacity described above minus 
C      today's CH2O content.
C-----------------------------------------------------------------------

      if(WTLF .gt. 0.0)then
         LFCCAP = LFMCCAP - WCRLF - (SLDOT * WCRLF/WTLF - LFSCMOB)
         IF (LFCCAP .LE. 0.0) LFCCAP = 0.0
      else
         LFCCAP = 0.0
      end if


      if(STMWT .gt. 0.0)then
         STCCAP = STMCCAP - WCRST - (SSDOT * WCRST/STMWT - STSCMOB)
         IF (STCCAP .LE. 0.0) STCCAP = 0.0
      else
         STCCAP = 0.0
      end if

      if(RTWT .gt. 0.0)then
         RTCCAP = RTMCCAP - WCRRT - (SRDOT * WCRRT/RTWT - RTSCMOB)
         IF (RTCCAP .LE. 0.0) RTCCAP = 0.0
      else
         RTCCAP = 0.0
      end if

      if(STRWT .gt. 0.0)then
         SRCCAP = SRMCCAP - WCRSR - (SSRDOT * WCRSR/STRWT - SRSCMOB)
         IF (SRCCAP .LE. 0.0) SRCCAP = 0.0
      else
         SRCCAP = 0.0
      end if


!      CDEBIT = (ALPHL * WTLF + ALPHR * RTWT + ALPHS * STMWT + 
!     &          ALPHSR *STRWT) - (WCRLF + WCRRT + WCRSR + WCRST)
      CDEBIT = LFCCAP + STCCAP + RTCCAP + SRCCAP
      CDEBIT = MAX(CDEBIT, 0.0)


      IF (CDEBIT .GT. 0.0) THEN
         LFCDEBT = LFCCAP / CDEBIT
         IF (LFCDEBT .LE. 0.0) LFCDEBT = 0.0


         STCDEBT = STCCAP / CDEBIT
         IF (STCDEBT .LE. 0.0) STCDEBT = 0.0


         RTCDEBT = RTCCAP / CDEBIT
         IF (RTCDEBT .LE. 0.0) RTCDEBT = 0.0


         SRCDEBT = SRCCAP / CDEBIT
         IF (SRCDEBT .LE. 0.0) SRCDEBT = 0.0


      ELSE 
        LFCDEBT = 0.0
        STCDEBT = 0.0
        RTCDEBT = 0.0
        SRCDEBT = 1.0
      ENDIF


C-----------------------------------------------------------------------
!     Calculate mobile CH2O refill rate modifiers
C-----------------------------------------------------------------------
      PGLFMX = (1. - EXP(-1.6 * LMXSTD)) / (1. - EXP(-1.6 * PGREF))
      PGSTATUS = PG / PHTMAX * PGLFMX

!      PGSTATUS = PG / PHTMAX * PGLFMX

      CFILL = CURV(TYPCREF,CRREF(1),CRREF(2),CRREF(3),
     &    CRREF(4),CSTATUS)
      LFILL = CURV(TYPLREF,LRREF(1),LRREF(2),LRREF(3),
     &    LRREF(4),XLAI)


      PFILL = CURV(TYPPREF,PRREF(1),PRREF(2),PRREF(3),
     &    PRREF(4),PGSTATUS)
      
C-----------------------------------------------------------------------
!     Calculate amount of PGAVL allocated to mobile CH2O status refill
C-----------------------------------------------------------------------

      CADVG = CADPV * CFILL * LFILL * PFILL * MIN(PG,PGAVL)
      CADVG = MIN(CADVG, CDEBIT)
      

!***********************************************************************
!     OUTPUT
!***********************************************************************

      ELSE IF (DYNAMIC .EQ. OUTPUT) THEN

!***********************************************************************
!     SEASEND
!***********************************************************************
 
      ELSE IF (DYNAMIC .EQ. SEASEND) THEN

      END IF
      Return
      End !SUBROUTINE FOR_CH2OREF

! CADPV    Maximum fraction of PGAVL for vegetative growth that can be 
!            allocated to mobile carbohydrate storage under non-stress   
!            conditions during vegetative growth stages. ( fraction)
! CADVG        CH2O to be reserved today for CH2O refill of vegetative 
!                  organs g [CH2O] m-2 d-1
! CDEBIT        Mobile CH2O deficit relative to mobile CH2O content 
!                  if plant was all "new" tissue
! CFILL        CH2O status effect on routine mobile CH2O refill rate 
! CRREF(4)  CURV response of routine refill of mobile CH2O during 
!                  vegetaive stages to current mobile CH2O status
! CSTATUS        Mobile CH2O content relative to mobile CH2O content 
!                  if plant were all "new" tissue
! LFCCAP    Leaf mobile CH2O deficit relative to mobile CH2O content 
!                  if leaf was all "new" tissue
! LFCDEBT        Proportion of total plant CH2O "deficit" attributed to leaves
! LFILL        LAI effect on routine mobile CH2O refill rate 
! LFMCCAP        Maximum CH2O content of leaves if tissues are at ALPHL concentration of CH2O
! LRREF(4)  CURV response of routine refill of mobile CH2O during 
!                  vegetaive stages to current LAI
! PCH2OLFF Proportion of mobile CH2O remaining in senesced leaf tissue
!             g [CH2O] / g [senesced leaf DM]
! PCH2ORTF Proportion of mobile CH2O remaining in senesced root tissue
!             g [CH2O] / g [senesced root DM]
! PCH2OSRF Proportion of mobile CH2O remaining in senesced STOR tissue
!             g [CH2O] / g [senesced STOR DM]
! PCH2OSTF Proportion of mobile CH2O remaining in senesced stem tissue
!             g [CH2O] / g [senesced leaf DM]
! PFILL        Canopy PG effect on routine mobile CH2O refill rate 
! PHTMAX   Maximum amount of CH20 which can be produced if 
!            photosynthetically active radiation (PAR) is very high (3 
!            times PARMAX) and all other factors are optimal (g[CH2O]/m2-d)
! PRREF(4)  CURV response of routine refill of mobile CH2O during 
!                  vegetaive stages to today's canopy PG rate
! RTCCAP    Root mobile CH2O deficit relative to mobile CH2O content 
!                  if root was all "new" tissue
! RTCDEBT        Proportion of total plant CH2O "deficit" attributed to roots
! RTMCCAP        Maximum CH2O content of roots if tissues are at ALPHR concentration of CH2O
! SLMDOT    Defoliation due to daily leaf senescence that is lost at PROLFF, 
!              hence, some fraction of the N content is subject to mobilization (g/m2/day)
! SRCCAP    STOR mobile CH2O deficit relative to mobile CH2O content 
!                  if STOR was all "new" tissue
! SRCDEBT        Proportion of total plant CH2O "deficit" attributed to STOR
! SRMCCAP        Maximum CH2O content of STOR if tissues are at ALPHSR concentration of CH2O
! SRMDOT    Daily root senescence that is lost at PRORTF, 
!              hence, some fraction of the N content is subject to mobilization (g/m2/day)
! SSMDOT    Daily petiole senescence that is lost at PROSTF, 
!              hence, some fraction of the N content is subject to mobilization (g/m2/day)
! SSRMDOT   Daily STOR senescence that is lost at PROSRF, 
!              hence, some fraction of the N content is subject to mobilization (g/m2/day)
! STCCAP    Stem mobile CH2O deficit relative to mobile CH2O content 
!                  if stem was all "new" tissue
! STCDEBT        Proportion of total plant CH2O "deficit" attributed to stem
! STMCCAP        Maximum CH2O content of Stems if tissues are at ALPHS concentration of CH2O
! TYPCREF        Shape of CURV response for refilling mobile CH2O to CH2O status
! TYPLREF        Shape of CURV response for refilling mobile CH2O to LAI
! TYPPREF        Shape of CURV response for refilling mobile CH2O to canopy PG rate
