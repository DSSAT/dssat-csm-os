MODULE MultiHar
!
!
!
IMPLICIT NONE

   INTEGER       :: NHAR1
   INTEGER, DIMENSION(:), ALLOCATABLE :: HDATE1
   INTEGER       :: HARV
   INTEGER       :: HARV_AH = 0            ! trigger true or false for harvesting
   INTEGER, SAVE :: iHARV   = 1
   
! FO xmpage was moved pods and read by ecotype
!   REAL    :: xmpage = 20.0 !6 seems to be better? depend on experiment - not sure
   
   REAL    :: RTDSD 
   REAL    :: RTDSH
   !for FreshWt.for output
   REAL    :: HRPN, AvgRFPW, AvgRDPW

   !new format - changed Integer to Real. Otherwise error later on
   REAL    :: RTFPW
   REAL    :: RTDPW  
   REAL    :: RPODNO 
   REAL    :: RSEEDNO
   INTEGER :: NPP0
   !end

   !for FreshWt.for output
   REAL :: HRVD   ! dry weight of harvested fruit
   REAL :: HRVF   ! harvest fresh weight of mature fruit 
   REAL :: DIFR
   REAL :: RUDPW
   REAL :: CHRVD   ! Cumulative dry weight of harvested fruit 
   REAL :: CHRVF   ! Cumulative fresh weight of harvested fruit 
     
END MODULE MultiHar