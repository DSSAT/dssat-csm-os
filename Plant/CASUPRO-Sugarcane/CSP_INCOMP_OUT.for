C======================================================================
C  CSP_INCOMP_OUT  Subroutine
C     This subroutine outputs values of variables from the 
C     CSP_INCOMP subroutine
C----------------------------------------------------------------------
C  REVISION HISTORY
C  11/09/01 O.H. Daza created for the CASUPRO sugarcane model
C  08/25/03 F.S. Royce updated for DSSAT 4.0
C  07/26/2004 CHP Removed variables which were not being used
C-----------------------------------------------------------------------
C  Called : CSP_INCOMP
C  Calls  : ERROR, FIND, IGNORE
C=======================================================================

      SUBROUTINE CSP_INCOMP_OUT( 
     &  FILECC, RNO3C, RCH2O,	RLIP, RLIG,	ROA, RMIN, PROLFI,    !Input
     &  PROSTI, PRORTI, PROSUI, PCARLF, PCARST, PCARRT, PCARSU, !Input
     &  PLIPLF, PLIPST, PLIPRT, PLIPSU, PLIGLF, PLIGST, PLIGRT, !Input
     &  PLIGSU, POALF, POAST, POART, POASU, PMINLF, PMINST,     !Input
     &  PMINRT, PMINSU)                                         !Input

!-----------------------------------------------------------------------
      IMPLICIT NONE
      EXTERNAL INFO
      SAVE
!-----------------------------------------------------------------------

      CHARACTER*6 ERRKEY
      PARAMETER (ERRKEY = 'INCOMP')
      CHARACTER*6 SECTION ! , ECONO, ECOTYP
!       CHARACTER*80 C80
      CHARACTER*78 MSG(10)
      CHARACTER*92 FILECC ! , FILEGC
!       CHARACTER*255 C255

!       INTEGER DYNAMIC, RUNINIT, SEASINIT
!       PARAMETER (RUNINIT = 1, SEASINIT = 2)
!       INTEGER LUNCRP, LUNECO
!       INTEGER ERR, LNUM, FOUND, ISECT
!       INTEGER I

      REAL  ! AGRLF , AGRRT , AGRSTM, AGRVG , AGRVG2,
     &      ! FRLF  , FRRT  , FRSTM,
     &      PCARLF, PCARRT, PCARST, PCARSU,
     &      PLIGLF, PLIGRT, PLIGST, PLIGSU,
     &      PLIPLF, PLIPRT, PLIPST, PLIPSU,
     &      PMINLF, PMINRT, PMINST, PMINSU,
     &      POALF , POART , POAST , POASU,
     &      PROLFI, PRORTI, PROSTI, PROSUI,
     &      RCH2O , RLIG  , RLIP  , RMIN  , RNO3C , ROA

!      INTEGER OpenStatus


! Open file to write results from CSP_IPDMND

!      OPEN(UNIT = 2000, FILE = "CSP01TEST.OUT", STATUS = "UNKNOWN",
!     &   ACTION = "WRITE", POSITION = "APPEND", IOSTAT = OpenStatus)
!
!      WRITE(2000,'')
!      WRITE(2000,'')
      WRITE(MSG(1),'("RESULTS FROM INCOMP_SC.for")')


      SECTION = '!*RESP'
! 

      WRITE(MSG(2),'("FILECC : ",A69)') FILECC(1:69)
      WRITE(MSG(3),'("SECTION: ",A6)') SECTION
      WRITE(MSG(4),'("RNO3C  : ",G12.4)') RNO3C
      WRITE(MSG(5),'("RCH2O  : ",F6.3)') RCH2O
	WRITE(MSG(6),'("RLIP   : ",F6.3)') RLIP
	WRITE(MSG(7),'("RLIG   : ",F6.3)') RLIG
	WRITE(MSG(8),'("ROA    : ",F6.3)') ROA
	WRITE(MSG(9),'("RMIN   : ",F6.2)') RMIN
      CALL INFO(9,ERRKEY,MSG)

      SECTION = '!*PLAN'

      WRITE(MSG(1),'("RESULTS FROM INCOMP_SC.for (continued)")')
      WRITE(MSG(2),'("SECTION: ",A6)') SECTION
      WRITE(MSG(3),'("PROLFI : ",F6.3)') PROLFI
      WRITE(MSG(4),'("PROSTI : ",F6.3)') PROSTI
      WRITE(MSG(5),'("PRORTI : ",F6.3)') PRORTI
      WRITE(MSG(6),'("PROUII : ",F6.3)') PROSUI

      WRITE(MSG(7),'("PCARLF : ",F6.3)') PCARLF
      WRITE(MSG(8),'("PCARST : ",F6.3)') PCARST
      WRITE(MSG(9),'("PCARRT : ",F6.3)') PCARRT
      WRITE(MSG(10),'("PCARSU : ",F6.3)') PCARSU
      CALL INFO(10,ERRKEY,MSG)

      WRITE(MSG(1),'("RESULTS FROM INCOMP_SC.for (continued)")')
      WRITE(MSG(2),'("SECTION: ",A6)') SECTION
      WRITE(MSG(3),'("PLIPLF : ",F6.3)') PLIPLF
      WRITE(MSG(4),'("PLIPST : ",F6.3)') PLIPST
      WRITE(MSG(5),'("PLIPRT : ",F6.3)') PLIPRT
      WRITE(MSG(6),'("PLIPSU : ",F6.3)') PLIPSU

      WRITE(MSG(7),'("PLIGLF : ",F6.3)') PLIGLF
      WRITE(MSG(8),'("PLIGST : ",F6.3)') PLIGST
      WRITE(MSG(9),'("PLIGRT : ",F6.3)') PLIGRT
      WRITE(MSG(10),'("PLIGSU : ",F6.3)') PLIGSU
      CALL INFO(10,ERRKEY,MSG)

      WRITE(MSG(1),'("RESULTS FROM INCOMP_SC.for (continued)")')
      WRITE(MSG(2),'("SECTION: ",A6)') SECTION
      WRITE(MSG(3),'("POALF  : ",F6.3)') POALF
      WRITE(MSG(4),'("POAST  : ",F6.3)') POAST
      WRITE(MSG(5),'("POART  : ",F6.3)') POART
      WRITE(MSG(6),'("POASU  : ",F6.3)') POASU

      WRITE(MSG(7),'("PMINLF : ",F6.3)') PMINLF
      WRITE(MSG(8),'("PMINST : ",F6.3)') PMINST
      WRITE(MSG(9),'("PMINRT : ",F6.3)') PMINRT
      WRITE(MSG(10),'("PMINSU : ",F6.3)') PMINSU
      CALL INFO(10,ERRKEY,MSG)

!      WRITE(MSG(2),'')
!      WRITE(MSG(2),'("END RESULTS FROM INCOMP_SC.for")')

!      CLOSE (2000)

      RETURN
      END ! SUBROUTINE INCOMP_SC_OUT
!=======================================================================

!-----------------------------------------------------------------------
!       Variable definitions
!-----------------------------------------------------------------------
! AGRLF   Mass of CH2O required for new leaf growth 
! AGRRT   Mass of CH2O required for new root growth 
! AGRSTM  Mass of CH2O required for new stem growth 
! AGRVG   Mass of CH2O required for vegetative tissue growth including 
!           stoichiometry and respiration 
! AGRVG2  Total mass of CH2O required for vegetative tissue growth 
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
! FRSTM   Fraction of vegetative tissue growth that goes to stems on a day 
! ISECT   Data record code (0 - End of file encountered, 1 - Found a good 
!           line to read, 2 - End of Section in file encountered, denoted 
!           by * in column 1
! LNUM    Current line number of input file 
! LUNCRP  Logical unit number for FILEC (*.spe file) 
! LUNECO  Logical unit number for FILEE (*.eco file) "
! PCARLF  Proportion of leaf tissue that is carbohydrate
!           (fraction)","IPGROW, INCOMP
! PCARRT  Proportion of root tissue that is carbohydrate
!           (fraction)","IPGROW, INCOMP
! PCARST  Proportion of stem tissue that is carbohydrate
!           (fraction)","IPGROW, INCOMP
! PLIGLF  Proportion of leaf tissue that is lignin
!           (fraction)","IPGROW, INCOMP
! PLIGRT  Proportion of root tissue that is lignin
!           (fraction)","IPGROW, INCOMP
! PLIGST  Proportion of stem tissue that is lignin
!           (fraction)","IPGROW, INCOMP
! PLIPLF  Proportion of leaf tissue that is lipid
!           (fraction)","IPGROW, INCOMP
! PLIPRT  Proportion of root tissue that is lipid
!           (fraction)","IPGROW, INCOMP
! PLIPST  Proportion of stem tissue that is lipid
!           (fraction)","IPGROW, INCOMP
! PMINLF  Proportion of leaf tissue that is mineral
!           (fraction)","IPGROW, INCOMP
! PMINRT  Proportion of root tissue that is mineral
!           (fraction)","IPGROW, INCOMP
! PMINST  Proportion of stem tissue that is mineral
!           (fraction)","IPGROW, INCOMP
! POALF   Proportion of leaf tissue that is organic acid
!           (fraction)","IPGROW, INCOMP
! POART   Proportion of root tissue that is organic acid
!           (fraction)","IPGROW, INCOMP
! POAST   Proportion of stem tissue that is organic acid
!           (fraction)","IPGROW, INCOMP
! PROLFF  Minimum leaf protein composition after N mining
!           ( g[protein] / g[leaf])
! PROLFI  Maximum protein composition in leaves during growth with 
!           luxurious supply of N
!           (g[protein] / g[leaf tissue])","IPPLNT, IPDMND, I
! PRORTI  Maximum protein composition in roots during growth with luxurious 
!           supply of N (g[protein] / g[root])","IPPLNT, IPDMND, IPGROW, 
! PROSTF  Minimum stem protein composition after N mining
!           (g[protein] / g[stem])","IPGROW, INCOMP, IPDMND
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
!-----------------------------------------------------------------------
!      END SUBROUTINE INCOMP_SC
!=======================================================================
