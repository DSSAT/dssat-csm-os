C=======================================================================
C  FOR_INCOMP  Subroutine
C     This subroutine initializes parameters for composition of tissues
C     which vary with genotype at the beginning of each run.
C----------------------------------------------------------------------
C  REVISION HISTORY
C  03/31/91 JWW Separated old INPHEN into INPHEN, INVEG, INCOMP
C  04/01/91 GH  Adapted for CROPGRO
C  09/18/98 CHP Moved to PLANT module and added input statements
C  05/10/99 GH  Incorporated in CROPGRO
C  06/22/03 SJR Added storage organ for Forage model
C-----------------------------------------------------------------------
C  Called : CROPGRO
C  Calls  : ERROR, FIND, IGNORE
C=======================================================================

      SUBROUTINE FOR_INCOMP(
     &  ECONO, FILECC, FILEGC, FRLF, FRRT,                      !Input
     &  FRSTM,                                                  !Input
     &  AGRLF, AGRNOD, AGRRT, AGRSD1, AGRSD2,                   !Output
     &  AGRSH1, AGRSH2, AGRSTM, AGRVG, AGRVG2,                  !Output
     &  SDPROR,                                                 !Output
     &  AGRSTR, FRSTR,                                          !Output      
     &  DYNAMIC)

C-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
        ! which contain control information, soil
        ! parameters, hourly weather data.
      IMPLICIT NONE
      EXTERNAL GETLUN, FIND, ERROR, IGNORE
      SAVE

      CHARACTER*6 ERRKEY
      PARAMETER (ERRKEY = 'INCOMP')
      CHARACTER*6 SECTION, ECONO, ECOTYP
      CHARACTER*80 C80
      CHARACTER*92 FILECC, FILEGC
      CHARACTER*255 C255

      INTEGER LUNCRP, LUNECO
      INTEGER DYNAMIC, ERR, LNUM, FOUND, ISECT
      INTEGER I

      REAL  AGRLF , AGRNOD, AGRRT , AGRSD1, AGRSD2, AGRSH1,
     &  AGRSH2, AGRSTM, AGRVG , AGRVG2,
     &  FRLF  , FRRT  , FRSTM,
     &  PCARLF, PCARNO, PCARRT, PCARSD, PCARSH, PCARST,
     &  PLIGLF, PLIGNO, PLIGRT, PLIGSD, PLIGSH, PLIGST,
     &  PLIPLF, PLIPNO, PLIPRT, PLIPSH, PLIPST,
     &  PMINLF, PMINNO, PMINRT, PMINSD, PMINSH, PMINST,
     &  POALF , POANO , POART , POASD , POASH , POAST ,
     &    PROLFI, PRORTI, PROSHI, PROSTI,
     &  RCH2O , RLIG  , RLIP  , RMIN  , RNO3C , ROA   ,
     &  SDLIP, SDPRO , SDPROR, SDPROS

      REAL PROSRI, PLIPSR, PLIGSR, POASR, PMINSR, PCARSR, 
     &    AGRSTR, FRSTR
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
!-----------------------------------------------------------------------
!    Find and Read Respiration Section
!-----------------------------------------------------------------------
!     Subroutine FIND finds appropriate SECTION in a file by
!     searching for the specified 6-character string at beginning
!     of each line.
!-----------------------------------------------------------------------
      LNUM = 1
      SECTION = '!*RESP'
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(F6.0)',IOSTAT=ERR) RNO3C
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(5F6.0)',IOSTAT=ERR) RCH2O, RLIP, RLIG, ROA, RMIN
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
      ENDIF

      SECTION = '!*PLAN'
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
!        READ(C80,'(F6.0,6X,2F6.0)',IOSTAT=ERR) PROLFI, PROLFF, PROSTI
        READ(C80,'(F6.0,12X,F6.0)',IOSTAT=ERR) PROLFI, PROSTI
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(F6.0,12X,F6.0)',IOSTAT=ERR) PRORTI, PROSHI
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(F6.0)',IOSTAT=ERR) SDPROS
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(6F6.0)',IOSTAT=ERR)
     &    PCARLF, PCARST, PCARRT, PCARSH, PCARSD, PCARNO
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(5F6.0)',IOSTAT=ERR)
     &    PLIPLF, PLIPST, PLIPRT, PLIPSH, PLIPNO
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(6F6.0)',IOSTAT=ERR)
     &    PLIGLF, PLIGST, PLIGRT, PLIGSH, PLIGSD, PLIGNO
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(6F6.0)',IOSTAT=ERR)
     &    POALF, POAST, POART, POASH, POASD, POANO
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(6F6.0)',IOSTAT=ERR)
     &    PMINLF, PMINST, PMINRT, PMINSH, PMINSD, PMINNO
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
!      ENDIF

C-----------------------------------------------------------------------
C    Read New Storage Organ Composition Parameters
C-----------------------------------------------------------------------

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(F6.0)',IOSTAT=ERR)
     &    PROSRI
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
!      ENDIF

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(5F6.0)',IOSTAT=ERR)
     &    PCARSR, PLIPSR, PLIGSR, POASR, PMINSR
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
      ENDIF



      CLOSE (LUNCRP)

C-----------------------------------------------------------------------
C    Read Ecotype Parameter File
C-----------------------------------------------------------------------
      CALL GETLUN('FILEE', LUNECO)
      OPEN (LUNECO,FILE = FILEGC,STATUS = 'OLD',IOSTAT=ERR)

      ISECT = 2
      DO I=1,1000
        CALL IGNORE(LUNECO, LNUM, ISECT, C255)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEGC,0)
        IF ((ISECT .EQ. 1) .AND. (C255(1:1) .NE. ' ') .AND.
     &    (C255(1:1) .NE. '*')) THEN
        READ (C255,'(A6,108X,2F6.0)',IOSTAT=ERR)
     &    ECOTYP, SDPRO, SDLIP
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEGC,LNUM)
        IF (ECOTYP .EQ. ECONO) THEN
        EXIT
        ENDIF
        ELSE IF (ISECT .EQ. 0) THEN
        IF (ECONO .EQ. 'DFAULT') CALL ERROR(ERRKEY,3,FILEGC,LNUM)
        ECONO = 'DFAULT'
        REWIND(LUNECO)
        ENDIF
      ENDDO

      CLOSE (LUNECO)

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
C-----------------------------------------------------------------------
C     COMPUTE RESPIRATION COEFFICIENTS BASED ON PLANT COMPOSITION

C-----------------------------------------------------------------------
C
      AGRLF  = PLIPLF*RLIP + PLIGLF*RLIG + POALF*ROA
     &    + PMINLF*RMIN + PCARLF*RCH2O
      AGRSTM = PLIPST*RLIP + PLIGST*RLIG + POAST*ROA
     &    + PMINST*RMIN + PCARST*RCH2O
      AGRRT  =  PLIPRT*RLIP + PLIGRT*RLIG + POART*ROA
     &    + PMINRT*RMIN + PCARRT*RCH2O
      AGRNOD =  PLIPNO*RLIP + PLIGNO*RLIG + POANO*ROA
     &    + PMINNO*RMIN + PCARNO*RCH2O

!-----------------------------------------------------------------------
!     AGRSTR storage growth cost, AGRSR2 include protein component of  
!     growth cost
!-----------------------------------------------------------------------

      AGRSTR = PLIPSR*RLIP + PLIGSR*RLIG + POASR*ROA
     &    + PMINSR*RMIN + PCARSR*RCH2O

!-----------------------------------------------------------------------
!     AGRVG2, AGRSH2, AGRSD2 include protein component of vegetative 
!     growth cost
!-----------------------------------------------------------------------
      AGRVG  = AGRLF * FRLF + AGRRT * FRRT + AGRSTM * FRSTM 
     &    + AGRSTR * FRSTR
        
      AGRVG2 = AGRVG + (FRLF*PROLFI+FRRT*PRORTI+FRSTM*PROSTI
     &    +FRSTR*PROSRI)*RNO3C

!-----------------------------------------------------------------------
      AGRSH1 =  PLIPSH*RLIP + PLIGSH*RLIG + POASH*ROA
     &    + PMINSH*RMIN + PCARSH*RCH2O
      AGRSH2 =  AGRSH1 + PROSHI*RNO3C 

!-----------------------------------------------------------------------
      SDPROR = (SDPRO - SDPROS) / ( SDLIP + PCARSD )
      AGRSD1 = PMINSD*RMIN + PLIGSD*RLIG + POASD*ROA
     &    + (SDLIP*RLIP + PCARSD*RCH2O)*(1. - SDPROR)
      AGRSD2 = AGRSD1 + SDPRO*RNO3C 

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!-----------------------------------------------------------------------
      RETURN
      END ! SUBROUTINE FOR_INCOMP
!=======================================================================

!-----------------------------------------------------------------------
!       Variable definitions
!-----------------------------------------------------------------------
! AGRLF   Mass of CH2O required for new leaf growth 
! AGRNOD  CH2O requirement for nodule growth 
! AGRRT   Mass of CH2O required for new root growth 
! AGRSD1  CH2O requirement for seed growth, excluding cost for protein 
!           content 
! AGRSD2  CH2O requirement for seed growth, including cost for protein 
!           content 
! AGRSH1  CH2O required for shell growth, excluding cost for protein 
!           content 
! AGRSH2  CH2O requirement for shell growth, including cost for protein 
!           content 
! AGRSTR  Mass of CH2O required for new storage tissue growth 
! AGRSTM  Mass of CH2O required for new stem growth 
! AGRVG   Mass of CH2O required for vegetative tissue growth including 
!           stoichiometry and respiration 
! AGRVG2  Total mass of CH2O required for vegetative tissue growth 
! DYNAMIC Module control variable; =RUNINIT, SEASINIT, RATE, EMERG, INTEGR, 
!           OUTPUT, or SEASEND 
! ECONO   Ecotype code - used to match ECOTYP in .ECO file
!           ","IPDMND, IPGROW, IPIBS, IPPHENOL, PODS, IPPLNT
! ECOTYP  Ecotype code for this simulation "
! ERR     Error code for file operation 
! FILECC  Path plus filename for species file (*.spe) 
! FILEGC  Pathname plus filename for ECO file "
! FOUND   Indicator that good data was read from file by subroutine FIND (0 
!           - End-of-file encountered, 1 - NAME was found) 
! FRLF    Fraction of vegetative tissue growth that goes to leaves on a day
!           
! FRRT    Fraction of vegetative tissue growth that goes to roots on a day 
! FRSTR   Fraction of vegetative tissue growth that goes to storage on a day 
! FRSTM   Fraction of vegetative tissue growth that goes to stems on a day 
! ISECT   Data record code (0 - End of file encountered, 1 - Found a good 
!           line to read, 2 - End of Section in file encountered, denoted 
!           by * in column 1
! LNUM    Current line number of input file 
! LUNCRP  Logical unit number for FILEC (*.spe file) 
! LUNECO  Logical unit number for FILEE (*.eco file) "
! PCARLF  Proportion of leaf tissue that is carbohydrate
!           (fraction)","IPGROW, INCOMP
! PCARNO  Proportion of nodule tissue that is carbohydrate
!           (fraction)","IPGROW, INCOMP
! PCARRT  Proportion of root tissue that is carbohydrate
!           (fraction)","IPGROW, INCOMP
! PCARSD  Proportion of seed tissue that is carbohydrate
!           (g[CH2O] / g[seed])","IPGROW, INCOMP, IPPLNT
! PCARSH  Proportion of shell tissue that is carbohydrate
!           (fraction)","IPGROW, INCOMP, IPPLNT
! PCARSR  Proportion of storage tissue that is carbohydrate
!           (fraction)","IPGROW, INCOMP
! PCARST  Proportion of stem tissue that is carbohydrate
!           (fraction)","IPGROW, INCOMP
! PLIGLF  Proportion of leaf tissue that is lignin
!           (fraction)","IPGROW, INCOMP
! PLIGNO  Proportion of nodule tissue that is lignin
!           (fraction)","IPGROW, INCOMP
! PLIGRT  Proportion of root tissue that is lignin
!           (fraction)","IPGROW, INCOMP
! PLIGSD  Proportion of seed tissue that is lignin
!           (fraction)","IPPLNT, IPDMND, PODCOMP, IPGROW, INC
! PLIGSH  Proportion of shell tissue that is lignin
!           (fraction)","IPPLNT, IPGROW, INCOMP
! PLIGSR  Proportion of storage tissue that is lignin
!           (fraction)","IPGROW, INCOMP
! PLIGST  Proportion of stem tissue that is lignin
!           (fraction)","IPGROW, INCOMP
! PLIPLF  Proportion of leaf tissue that is lipid
!           (fraction)","IPGROW, INCOMP
! PLIPNO  Proportion of nodule tissue that is lipid
!           (fraction)","IPGROW, INCOMP
! PLIPRT  Proportion of root tissue that is lipid
!           (fraction)","IPGROW, INCOMP
! PLIPSH  Proportion of shell tissue that is lipid
!           (fraction)","IPPLNT, IPGROW, INCOMP
! PLIPSR  Proportion of storage tissue that is lipid
!           (fraction)","IPGROW, INCOMP
! PLIPST  Proportion of stem tissue that is lipid
!           (fraction)","IPGROW, INCOMP
! PMINLF  Proportion of leaf tissue that is mineral
!           (fraction)","IPGROW, INCOMP
! PMINNO  Proportion of nodule tissue that is mineral
!           (fraction)","IPGROW, INCOMP
! PMINRT  Proportion of root tissue that is mineral
!           (fraction)","IPGROW, INCOMP
! PMINSD  Proportion of seed tissue that is mineral
!           (fraction)","IPPLNT, IPDMND, PODCOMP, IPGROW, INC
! PMINSH  Proportion of shell tissue that is mineral
!           (fraction)","IPPLNT, IPGROW, INCOMP
! PMINSR  Proportion of storage tissue that is mineral
!           (fraction)","IPGROW, INCOMP
! PMINST  Proportion of stem tissue that is mineral
!           (fraction)","IPGROW, INCOMP
! POALF   Proportion of leaf tissue that is organic acid
!           (fraction)","IPGROW, INCOMP
! POANO   Proportion of nodule tissue that is organic acid
!           (fraction)","IPGROW, INCOMP
! POART   Proportion of root tissue that is organic acid
!           (fraction)","IPGROW, INCOMP
! POASD   Proportion of seed tissue that is organic acid
!           (fraction)","IPPLNT, IPDMND, PODCOMP, IPGROW, INC
! POASH   Proportion of shell tissue that is organic acid
!           (fraction)","IPPLNT, IPGROW, INCOMP
! POASR   Proportion of storage tissue that is organic acid
!           (fraction)","IPGROW, INCOMP
! POAST   Proportion of stem tissue that is organic acid
!           (fraction)","IPGROW, INCOMP
! PROLFI  Maximum protein composition in leaves during growth with 
!           luxurious supply of N
!           (g[protein] / g[leaf tissue])","IPPLNT, IPDMND, I
! PRORTI  Maximum protein composition in roots during growth with luxurious 
!           supply of N (g[protein] / g[root])","IPPLNT, IPDMND, IPGROW, 
! PROSHI  Maximum protein composition in shells during growth with 
!           luxurious supply of N
!           ( g[protein] / g[shell tissue])","PODS, IPPLNT, I
! PROSTF  Minimum stem protein composition after N mining
!           (g[protein] / g[stem])","IPGROW, INCOMP, IPDMND
! PROSRI  Maximum protein composition in storage during growth with luxurious 
!           supply of N (g[protein] / g[stem])","IPPLNT, IPDMND, IPGROW, 
! PROSTI  Maximum protein composition in stems during growth with luxurious 
!           supply of N (g[protein] / g[stem])","IPPLNT, IPDMND, IPGROW, 
! RCH2O   Respiration required for synthesizing CH2O structure
!           (g[CH2O] / g[tissue])","IPDMND, PODCOMP, IPPLNT, 
! RLIG    Respiration required for synthesizing lignin structure
!           (g[CH2O] / g[lignin])","IPDMND, PODCOMP, IPPLNT, 
! RLIP    Respiration required for synthesizing lipid structure
!           (g[CH2O] / g[lipid])","IPDMND, PODCOMP, IPPLNT, I
! RMIN    Respiration required for synthesizing mineral structure
!           (g[CH2O] / g[mineral])","IPPLNT, IPDMND, PODCOMP
! RNO3C   Respiration required for reducing NO3 to protein
!           (g[CH2O] / g[protein])","IPDMND, IPPLNT, INCOMP
! ROA     Respiration required for synthesizing organic acids
!           (g[CH2O] / g[product])","IPDMND, PODCOMP, IPPLNT
! SDLIP   Maximum lipid composition in seed
!           (fraction)","IPDMND, IPGROW, INCOMP
! SDPRO   Seed protein fraction at 25oC
!           (g[protein] / g[seed])","IPDMND, IPGROW, INCOMP
! SDPROR  Ratio to adjust lipid and carbohydrate proportions when seed 
!           protein differs from protein composition of standard cultivar 
!           (SDPROS) 
! SDPROS  Seed protein fraction of standard cultivar at 25oC
!           (g[protein] / g[seed])","INCOMP
!-----------------------------------------------------------------------
!      END SUBROUTINE FOR_INCOMP
!=======================================================================
