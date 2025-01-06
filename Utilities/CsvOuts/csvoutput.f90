Module CsvOutput
!
!
Use Linklist

Implicit None
Save

Character(Len=8) :: expname
Character(Len=1) :: fmopt
Integer :: maxnlayers = 1

Character(:), allocatable, Target :: vCsvline, vCsvlineSW, vCsvlineTemp
Character (:), Pointer :: vpCsvline, vpCsvlineSW,vpCsvlineTemp
Integer :: vlngth, vlngthSW, vlngthTemp
!------------------------------------------------------------------------------
! for cscer
Character(:), allocatable, Target :: vCsvlineCsCer
Character (:), Pointer :: vpCsvlineCsCer
Integer :: vlngthCsCer
!------------------------------------------------------------------------------
! for ET.OUT
Character(:), allocatable, Target :: vCsvlineET
Character (:), Pointer :: vpCsvlineET
Integer :: vlngthET 
!------------------------------------------------------------------------------
! for MZCER
Character(:), allocatable, Target :: vCsvlineMZCER
Character (:), Pointer :: vpCsvlineMZCER
Integer :: vlngthMZCER
!------------------------------------------------------------------------------
! for RICER
Character(:), allocatable, Target :: vCsvlineRICER
Character (:), Pointer :: vpCsvlineRICER
Integer :: vlngthRICER
!------------------------------------------------------------------------------
! for SUOIL
Character(:), allocatable, Target :: vCsvlineSUOIL
Character (:), Pointer :: vpCsvlineSUOIL
Integer :: vlngthSUOIL
!------------------------------------------------------------------------------
! for PlNSUOIL
Character(:), allocatable, Target :: vCsvlinePlNSUOIL
Character (:), Pointer :: vpCsvlinePlNSUOIL
Integer :: vlngthPlNSUOIL
!------------------------------------------------------------------------------
! for PlNCrGro
Character(:), allocatable, Target :: vCsvlinePlNCrGro
Character (:), Pointer :: vpCsvlinePlNCrGro
Integer :: vlngthPlNCrGro
!------------------------------------------------------------------------------
! for PlNCrCer
Character(:), allocatable, Target :: vCsvlinePlNCsCer
Character (:), Pointer :: vpCsvlinePlNCsCer
Integer :: vlngthPlNCsCer
!------------------------------------------------------------------------------
! for SoilNi
Character(:), allocatable, Target :: vCsvlineSoilNi
Character (:), Pointer :: vpCsvlineSoilNi
Integer :: vlngthSoilNi
!------------------------------------------------------------------------------
! for PlNMzCer
Character(:), allocatable, Target :: vCsvlinePlNMzCer
Character (:), Pointer :: vpCsvlinePlNMzCer
Integer :: vlngthPlNMzCer
!!------------------------------------------------------------------------------
!! for PlNRICer
!Character(:), allocatable, Target :: vCsvlinePlNRICer
!Character (:), Pointer :: vpCsvlinePlNRICer
!Integer :: vlngthPlNRICer
!------------------------------------------------------------------------------
! for weather.out
Character(:), allocatable, Target :: vCsvlineWth
Character (:), Pointer :: vpCsvlineWth
Integer :: vlngthWth
!--------------------------------------------------------------------
! for PlantGr2.out
Character(:), allocatable, Target :: vCsvlinePlGr2
Character (:), Pointer :: vpCsvlinePlGr2
Integer :: vlngthPlGr2
!--------------------------------------------------------------------
! for PlantGrf.out
Character(:), allocatable, Target :: vCsvlinePlGrf
Character (:), Pointer :: vpCsvlinePlGrf
Integer :: vlngthPlGrf
!--------------------------------------------------------------------
! for Evaluate.out
Character(:), allocatable, Target :: vCsvlineEvalCsCer
Character (:), Pointer :: vpCsvlineEvalCsCer
Integer :: vlngthEvalCsCer
!------------------------------------------------------------------------------
! for Evaluate.out from Opsum
Character(:), allocatable, Target :: vCsvlineEvOpsum
Character (:), Pointer :: vpCsvlineEvOpsum
Integer :: vlngthEvOpsum
!------------------------------------------------------------------------------
! for summary.out 
Character(:), allocatable, Target :: vCsvlineSumOpsum
Character (:), Pointer :: vpCsvlineSumOpsum
Integer :: vlngthSumOpsum
!------------------------------------------------------------------------------
! for PlCCrGro
Character(:), allocatable, Target :: vCsvlinePlCCrGro
Character (:), Pointer :: vpCsvlinePlCCrGro
Integer :: vlngthPlCCrGro
!------------------------------------------------------------------------------
! for SoilOrg
Character(:), allocatable, Target :: vCsvlineSoilOrg
Character (:), Pointer :: vpCsvlineSoilOrg
Integer :: vlngthSoilOrg
!------------------------------------------------------------------------------
! for ETPhot
Character(:), allocatable, Target :: vCsvlineETPhot
Character (:), Pointer :: vpCsvlineETPhot
Integer :: vlngthETPhot
!------------------------------------------------------------------------------
! for Mulch
Character(:), allocatable, Target :: vCsvlineMulch
Character (:), Pointer :: vpCsvlineMulch
Integer :: vlngthMulch
!------------------------------------------------------------------------------
! for PlantP
Character(:), allocatable, Target :: vCsvlinePlantP
Character (:), Pointer :: vpCsvlinePlantP
Integer :: vlngthPlantP
!------------------------------------------------------------------------------
! for SoilPi
Character(:), allocatable, Target :: vCsvlineSoilPi
Character (:), Pointer :: vpCsvlineSoilPi
Integer :: vlngthSoilPi
!------------------------------------------------------------------------------
! for MLCER
Character(:), allocatable, Target :: vCsvlineMLCER
Character (:), Pointer :: vpCsvlineMLCER
Integer :: vlngthMLCER
!------------------------------------------------------------------------------
! for PlantGro PRFRM
Character(:), allocatable, Target :: vCsvlinePlGroPrFrm
Character (:), Pointer :: vpCsvlinePlGroPrFrm
Integer :: vlngthPlGroPrFrm
!------------------------------------------------------------------------------
! for Plant N Forage
Character(:), allocatable, Target :: vCsvlinePlNPrFrm
Character (:), Pointer :: vpCsvlinePlNPrFrm
Integer :: vlngthPlNPrFrm
!------------------------------------------------------------------------------
! for Plant C Forage
Character(:), allocatable, Target :: vCsvlinePlCPrFrm
Character (:), Pointer :: vpCsvlinePlCPrFrm
Integer :: vlngthPlCPrFrm
!------------------------------------------------------------------------------
! for dormancy Forage
Character(:), allocatable, Target :: vCsvlineDormPrFrm
Character (:), Pointer :: vpCsvlineDormPrFrm
Integer :: vlngthDormPrFrm
!------------------------------------------------------------------------------
! for storage Forage
Character(:), allocatable, Target :: vCsvlineStorPrFrm
Character (:), Pointer :: vpCsvlineStorPrFrm
Integer :: vlngthStorPrFrm
!------------------------------------------------------------------------------
! for SOM N Century
Character(:), allocatable, Target :: vCsvlineSomN
Character (:), Pointer :: vpCsvlineSomN
Integer :: vlngthSomN
!------------------------------------------------------------------------------
! for SOM C Century
Character(:), allocatable, Target :: vCsvlineSomC
Character (:), Pointer :: vpCsvlineSomC
Integer :: vlngthSomC
!------------------------------------------------------------------------------
! for N2O.csv
Character(:), allocatable, Target :: vCsvlineN2O
Character (:), Pointer :: vpCsvlineN2O
Integer :: vlngthN2O
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
! Generic subroutine CsvOut
! 
Interface CsvOut
   Module Procedure CsvOut_cscer, &     
                    CsvOut_crgro
!                    CsvOut_mzcer
End Interface CsvOut
!------------------------------------------------------------------------------    

Contains

!------------------------------------------------------------------------------    
! Sub for plantgro.csv output CSCER
Subroutine CsvOut_cscer(EXCODE, RUN, TN, RN, SN, ON, REP, CN, YEAR, DOY, &
   DAS, DAP, TMEAN, TKILL, ZSTAGE, LNUMSD, PARIOUT, PARUED, CARBOA, LAI, &
   SAIDOUT, TWAD, SDWAD, RWAD, CWAD, LLWADOUT, STWADOUT, GWAD, HIAD, CHWADOUT,&
   EWAD, RSWAD, DWAD, SENW0C, SENWSC, RSCD, GRNUMAD, HWUDC, TNUMAD, SLAOUT, &
   RTDEP, PTF, H2OA, WAVR, WUPR, WFT, WFP, WFG, NFT, NFP, NFG, NUPR, TFP, TFG,& 
   VF, DF, Csvline, pCsvline, lngth)
         
   ! Input vars
   Character(10),Intent(in):: EXCODE     ! Experiment code/name           text
!   Character(8),Intent(in) :: RUNRUNI    ! Run+internal run number        text
   Integer, Intent(IN) :: RUN           ! run number
   Integer,Intent(in) :: TN, RN, SN, ON, REP, CN, YEAR, DOY, DAS, DAP        
   Real,Intent(in) :: TMEAN, TKILL, ZSTAGE, LNUMSD, PARIOUT, PARUED, CARBOA, &
      LAI, SAIDOUT, TWAD, SDWAD, RWAD, CWAD, LLWADOUT, STWADOUT, GWAD, HIAD, &       
      CHWADOUT, EWAD, RSWAD, DWAD, RSCD, GRNUMAD, TNUMAD, SLAOUT, RTDEP, PTF, &     
      H2OA, WAVR, WUPR, WFT, WFP, WFG, NFT, NFP, NFG, NUPR, TFP, TFG, VF, DF  

   CHARACTER(6),Intent(in) :: SENW0C, SENWSC, HWUDC

   ! Temp vars
!  integer :: length
   ! Recalculated vars
   REAL :: cCARBOA1,cRSCD1,cRTDEP1, cWAVR1, cWUPR1, cWFT1, cWFP1, cWFG1, &     
           cNFT1, cNFP1, cNFG1, cNUPR1, cTFP1, cTFG1, cVF1, cDF1, LAISAI
   Integer :: iLLWADOUT, iTWAD, iSDWAD, iRWAD, iCWAD, iSTWADOUT, iGWAD, & 
              iCHWADOUT, iEWAD, iRSWAD, iDWAD, iGRNUMAD, iTNUMAD, iSLAOUT
   
   Character(:), allocatable, Target, Intent(Out) :: Csvline
   Character(:), Pointer, Intent(Out) :: pCsvline
   Integer, Intent(Out) :: lngth
   Integer :: size
   Character(Len=1000) :: tmp 
   ! End of vars

   ! Recalculation
   cCARBOA1 = AMIN1(999.9,CARBOA)
   LAISAI = LAI + SAIDOUT 
   iTWAD = NINT(TWAD)
   iSDWAD = NINT(SDWAD)
   iRWAD = NINT(RWAD)
   iCWAD = NINT(CWAD)
   iLLWADOUT = NINT(LLWADOUT)
   iSTWADOUT = NINT(STWADOUT)
   iGWAD = NINT(GWAD)
   iCHWADOUT = NINT(CHWADOUT)
   iEWAD = NINT(EWAD)
   iRSWAD = NINT(RSWAD)
   iDWAD = NINT(DWAD)
   cRSCD1 = RSCD * 100.0
   iGRNUMAD = NINT(GRNUMAD)
   iTNUMAD = NINT(TNUMAD)
   iSLAOUT = NINT(SLAOUT)
   cRTDEP1 = RTDEP / 100.0
   cWAVR1= AMIN1(99.9,WAVR)
   cWUPR1 = AMIN1(15.0,WUPR)
   cWFT1 = 1.0 - WFT
   cWFP1 = 1.0 - WFP
   cWFG1 = 1.0 - WFG
   cNFT1 = 1.0 - NFT
   cNFP1 = 1.0 - NFP
   cNFG1 = 1.0 - NFG
   cNUPR1 = AMIN1(2.0,NUPR)
   cTFP1 = 1.0 - TFP
   cTFG1 = 1.0 - TFG
   cVF1 = 1.0 - VF
   cDF1 = 1.0 - DF 
     
   
!  Unformated outputs  
   Write(tmp,'(56(g0,","),g0)') RUN, EXCODE, TN, RN, SN, ON, REP, CN, YEAR, &
      DOY, DAS, DAP, TMEAN, TKILL, ZSTAGE, LNUMSD, PARIOUT, PARUED, cCARBOA1, LAI,& 
      SAIDOUT, LAISAI, iTWAD, iSDWAD, iRWAD, iCWAD, iLLWADOUT, iSTWADOUT, iGWAD, &
      HIAD, iCHWADOUT, iEWAD, iRSWAD, iDWAD, SENW0C, SENWSC, cRSCD1, iGRNUMAD, & 
      HWUDC, iTNUMAD, iSLAOUT, cRTDEP1, PTF, H2OA, cWAVR1, cWUPR1, cWFT1, cWFP1, &
      cWFG1, cNFT1, cNFP1, cNFG1, cNUPR1, cTFP1, cTFG1, cVF1, cDF1

   lngth = Len(Trim(Adjustl(tmp)))
   size = lngth
   Allocate(Character(Len = size)::Csvline)
   Csvline = Trim(Adjustl(tmp))
   pCsvline => Csvline
   Return
end Subroutine CsvOut_cscer
!---------------------------------------------------------------------------------   
! Sub for plantgro.csv output CRGRO
Subroutine CsvOut_crgro(EXCODE, RUN, TN, ROTNUM, REPNO, YEAR, DOY, DAS, DAP, &
   VSTAGE, RSTAGE, XLAI, WTLF, STMWT, SDWT, LINTW, LINTP, &
   RTWT, VWAD, TOPWT, SEEDNO, SDSIZE, HI, &
   PODWT, PODNO, SWF_AV, TUR_AV, NST_AV, PS1_AV, PS2_AV, KST_AV, EXW_AV, PCNLP, & 
   SHELPC, HIP, PODWTD, SLAP, CANHT, CANWH, DWNOD, RTDEP, N_LYR, RLV, CUMSENSURF, & 
   CUMSENSOIL, Csvline, pCsvline, lngth) 

!  Input vars
   Character(8),Intent(IN):: EXCODE    
   Integer, Intent(IN) :: RUN, TN, ROTNUM, REPNO, YEAR, DOY, DAS, DAP      
!   INTEGER,Intent(in)      :: SN         ! Sequence number,crop rotation  #
!   INTEGER,Intent(in)      :: ON         ! Option number (sequence runs)  #
!   INTEGER,Intent(in)      :: CN         ! Crop component (multicrop)     #
   Real,Intent(IN) :: VSTAGE, XLAI, WTLF, STMWT, SDWT, RTWT, TOPWT, SEEDNO     
   Real,Intent(in) :: SDSIZE, HI, PODWT, PODNO, SWF_AV, TUR_AV, NST_AV, PS1_AV
   Real,Intent(IN) :: PS2_AV, KST_AV, EXW_AV, PCNLP, SHELPC, HIP, PODWTD, SLAP
   Real,Intent(IN) :: CANHT, CANWH, DWNOD, RTDEP, CUMSENSURF, CUMSENSOIL
   Real,Intent(IN) :: LINTW, LINTP     
   Integer,Intent(IN) :: RSTAGE, VWAD

   Integer,Intent(IN) :: N_LYR
   Real, Dimension(N_LYR), Intent(IN) :: RLV 
  
!  Recalculated vars
   Integer :: cWTLF1, cSTMWT1, cSDWT1, cRTWT1, cTOPWT1, cSEEDNO1, cPODWT1
   Integer :: cPODNO1, cPODWTD1, cPodSum, cCUMSENSURF1, cCUMSENSOIL1,cLINTW
   Real :: cDWNOD1, cRTDEP1
  
   Integer :: i, size
  
   Character(:), allocatable, Target, Intent(Out) :: Csvline
   Character(:), Pointer, Intent(Out) :: pCsvline
   Integer, Intent(Out) :: lngth
   Character(Len = 900) :: tmp 
   Character(Len = 200) :: tmp1  
   Character(Len = 20)  :: fmt   
!  End of vars
          
!  Recalculation
   cWTLF1 = NINT(WTLF * 10.0)
   cSTMWT1 = NINT(STMWT * 10.0)
   cSDWT1 = NINT(SDWT * 10.0)
   cLINTW = NINT(LINTW * 10.0)
   cRTWT1 = NINT(RTWT * 10.0)
   cTOPWT1 = NINT(TOPWT * 10.0)
   cSEEDNO1 = NINT(SEEDNO)
   cPODWT1 = NINT(PODWT * 10.0)
   cPODNO1 = NINT(PODNO)
   cPODWTD1 = NINT(PODWTD * 10.0)
   cPodSum = NINT((PODWTD + PODWT) * 10.0)
   cDWNOD1 = DWNOD * 10.0
   cRTDEP1 = RTDEP / 100.0
   cCUMSENSURF1 = NINT(CUMSENSURF)  
   cCUMSENSOIL1 = NINT(CUMSENSOIL) 
   
   ! Unformatted string output
   Write(tmp,'(44(g0,","))') RUN, EXCODE, TN, ROTNUM, REPNO, YEAR, DOY, DAS,DAP,&
      VSTAGE, RSTAGE, XLAI, cWTLF1, cSTMWT1, cSDWT1, cLINTW, LINTP, &
      cRTWT1, VWAD, cTOPWT1, &
      cSEEDNO1, SDSIZE, HI, cPODWT1, cPODNO1, SWF_AV, TUR_AV, NST_AV, PS1_AV, &
      PS2_AV, KST_AV, EXW_AV, PCNLP, SHELPC, HIP, cPODWTD1, cPodSum, SLAP, &
      CANHT, CANWH, cDWNOD1, cRTDEP1, cCUMSENSURF1, cCUMSENSOIL1 
   
   Write(fmt,'(I2)') N_LYR - 1
   fmt = '('//Trim(Adjustl(fmt))//'(g0,","),g0)'
   fmt = Trim(Adjustl(fmt))
   
   Write(tmp1,fmt) (RLV(i), i = 1, N_LYR)  
   
     
   tmp = Trim(Adjustl(tmp)) // Trim(Adjustl(tmp1))
   
   lngth = Len(Trim(Adjustl(tmp)))
   size = lngth
   Allocate(Character(Len = size)::Csvline)
   Csvline = Trim(Adjustl(tmp))
   pCsvline => Csvline
   
   return
end Subroutine CsvOut_crgro
!---------------------------------------------------------------------------------
!  Sub for soilwat.csv output
Subroutine CsvOutSW_crgro(EXCODE, RUN, TN, ROTNUM,  REPNO, YEAR, DOY, DAS, TSW, &
   PESW, TRUNOF, TDRAIN, CRAIN, NAP, TOTIR, AVWTD, MULCHWAT, TDFD, TDFC, RUNOFF, &
   N_LYR, SW, Csvline, pCsvline, lngth) 
    
!  Input vars
   Character(8),Intent(IN):: EXCODE    
   Integer, Intent(IN) :: RUN, TN, ROTNUM, REPNO, YEAR, DOY, DAS, NAP            
!        INTEGER,Intent(in)      :: SN         ! Sequence number,crop rotation  #
!        INTEGER,Intent(in)      :: ON         ! Option number (sequence runs)  #
!        INTEGER,Intent(in)      :: CN         ! Crop component (multicrop)     #

   Real,Intent(IN) :: TSW, PESW, TRUNOF, TDRAIN, CRAIN, TOTIR, AVWTD, MULCHWAT
   Real,Intent(IN) :: TDFD, TDFC, RUNOFF
   Integer,Intent(IN) :: N_LYR
   Real, Dimension(N_LYR), Intent(IN) :: SW
  
!  Integer :: length      
!  Recalculated vars
   Integer :: cTSW1, cPESW1, cTRUNOF1, cTDRAIN1, cCRAIN1, cTOTIR1, cAVWTD1
!  Integer :: cWTLF1, cSTMWT1, cSDWT1, cRTWT1, cTOPWT1, cSEEDNO1, cPODWT1
!  Integer :: cPODNO1, cPODWTD1, cPodSum, cCUMSENSURF1, cCUMSENSOIL1 
!  Real :: cDWNOD11, cRTDEP1
   Integer :: i, size
  
   Character(:), allocatable, Target, Intent(Out) :: Csvline
   Character(:), Pointer, Intent(Out) :: pCsvline
   Integer, Intent(Out) :: lngth
   Character(Len=450) :: tmp
   Character(Len=170) :: tmp1   
   Character(Len=20) :: fmt     
!  End of vars
          
!  Recalculation
   cTSW1 = NINT(TSW)
   cPESW1 = NINT(PESW * 10.0)
   cTRUNOF1= NINT(TRUNOF)
   cTDRAIN1 = NINT(TDRAIN)
   cCRAIN1 = NINT(CRAIN)
   cTOTIR1 = NINT(TOTIR) 
   cAVWTD1 = NINT(AVWTD)
         
   Write(tmp,'(20(g0,","))') RUN, EXCODE, TN, ROTNUM, REPNO, YEAR, DOY, &
   DAS, cTSW1, cPESW1, cTRUNOF1, cTDRAIN1, cCRAIN1, NAP, cTOTIR1, &
   cAVWTD1, MULCHWAT, TDFD, TDFC, RUNOFF
   
   Write(fmt,'(I2)') N_LYR - 1  
   fmt = '('//Trim(Adjustl(fmt))//'(g0,","),g0)'
   fmt = Trim(Adjustl(fmt))
   
   Write(tmp1,fmt) (SW(i), i = 1, N_LYR)  
   
   tmp = Trim(Adjustl(tmp)) // Trim(Adjustl(tmp1)) 
   
   lngth = Len(Trim(Adjustl(tmp)))
   size = lngth
   Allocate(Character(Len = size)::Csvline)
   Csvline = Trim(Adjustl(tmp))
   pCsvline => Csvline
   Return
end Subroutine CsvOutSW_crgro
!---------------------------------------------------------------------------------
!  for soiltemp.csv output 
Subroutine CsvOutTemp_crgro(EXCODE, RUN, TN, ROTNUM,  REPNO, YEAR, DOY, DAS, &
   SRFTEMP, N_LYR, ST, Csvline, pCsvline, lngth) 
    
!  Input vars
   Character(8),Intent(IN) :: EXCODE     
   Integer, Intent(IN) :: RUN, TN, ROTNUM, REPNO, YEAR, DOY, DAS     
!        INTEGER,Intent(in)      :: SN         ! Sequence number,crop rotation  #
!        INTEGER,Intent(in)      :: ON         ! Option number (sequence runs)  #
!        INTEGER,Intent(in)      :: CN         ! Crop component (multicrop)     #
   Real,Intent(IN)         :: SRFTEMP    
   Integer,Intent(IN)      :: N_LYR
   Real, Dimension(N_LYR), Intent(IN) :: ST
  
!  Integer :: length      
!  Recalculated vars 
!  Integer :: cWTLF1, cSTMWT1, cSDWT1, cRTWT1, cTOPWT1, cSEEDNO1, cPODWT1
!  Integer :: cPODNO1, cPODWTD1, cPodSum, cCUMSENSURF1, cCUMSENSOIL1 
!  Real :: cDWNOD1, cRTDEP1
  
   Integer :: i, size
  
   Character(:), allocatable, Target, Intent(Out) :: Csvline
   Character(:), Pointer, Intent(Out) :: pCsvline
   Integer, Intent(Out) :: lngth
   Character(Len=270) :: tmp 
   Character(Len=200) :: tmp1 
   Character(Len=20) :: fmt    
   
   Write(tmp,'(9(g0,","))') RUN, EXCODE, TN, ROTNUM, REPNO, YEAR, DOY, &
      DAS, SRFTEMP
   
   Write(fmt,'(I2)') N_LYR - 1  
   fmt = '('//Trim(Adjustl(fmt))//'(g0,","),g0)'
   fmt = Trim(Adjustl(fmt))
   
   Write(tmp1,fmt) (ST(i), i = 1, N_LYR) 
     
   tmp = Trim(Adjustl(tmp)) // Trim(Adjustl(tmp1)) 
   
   lngth = Len(Trim(Adjustl(tmp)))
   size = lngth
   Allocate(Character(Len = size)::Csvline)
   Csvline = Trim(Adjustl(tmp))
   pCsvline => Csvline
   Return
end Subroutine CsvOutTemp_crgro
!------------------------------------------------------------------------------
! Sub for et.csv output
Subroutine CsvOutET(EXCODE, RUN, TN, ROTNUM,  REPNO, YEAR, DOY, DAS, AVSRAD, &
   AVTMX, AVTMN, REFA, EOAA, EOPA, EOSA, ETAA, EPAA, ESAA, EFAA, EMAA, CEO, &
   CET, CEP, CES, CEF, CEM, KCAA, KBSA, KEAA, N_LYR, ES_LYR, AVRWU, AVRWUP, Csvline, &
   pCsvline, lngth)

!  Input vars
   Character(8),Intent(IN):: EXCODE    
   Integer, Intent(IN) :: RUN, TN, ROTNUM, REPNO, YEAR, DOY, DAS         
!        INTEGER,Intent(in)      :: SN         ! Sequence number,crop rotation  #
!        INTEGER,Intent(in)      :: ON         ! Option number (sequence runs)  #
!        INTEGER,Intent(in)      :: CN         ! Crop component (multicrop)     #
   REAL,Intent(IN) :: AVSRAD, AVTMX, AVTMN, EOAA, EOPA, EOSA, ETAA, EPAA, ESAA 
   REAL,Intent(IN) :: EFAA, EMAA, CEO, CET, CEP, CES, CEF, CEM, AVRWU, AVRWUP
   REAL,Intent(IN) :: REFA, KCAA, KBSA, KEAA
   INTEGER,Intent(IN) :: N_LYR
   REAL, Dimension(N_LYR), Intent(IN) :: ES_LYR 
  
   Integer :: i, size   
   Real :: ES10
   
   Character(:), allocatable, Target, Intent(Out) :: Csvline
   Character(:), Pointer, Intent(Out) :: pCsvline
   Integer, Intent(Out) :: lngth
  
   Character(Len=550) :: tmp 
   Character(Len=200) :: tmp1  
   Character(Len=20) :: fmt  
   Character(Len=20) :: cES10   
!  End of vars
              
   Write(tmp,'(50(g0,","))') RUN, EXCODE, TN, ROTNUM, REPNO, YEAR, DOY, DAS, &
       AVSRAD, AVTMX, AVTMN, REFA, EOAA, EOPA, EOSA, ETAA, EPAA, ESAA, EFAA, &
       EMAA, CEO, CET, CEP, CES, CEF, CEM, KCAA, KBSA, KEAA, AVRWU, AVRWUP
   
   If (N_LYR < 11) Then
   Write(fmt,'(I2)') N_LYR - 1  
   fmt = '('//Trim(Adjustl(fmt))//'(g0,","),g0)'
   fmt = Trim(Adjustl(fmt))
   
     Write(tmp1,fmt) (ES_LYR(i), i = 1, N_LYR)        
   Else
     ES10 = 0.0
     DO i = 10, N_LYR
       ES10 = ES10 + ES_LYR(i)
     ENDDO
     Write(fmt,'(I2)') N_LYR - (N_LYR-10) - 1 
     fmt = '('//Trim(Adjustl(fmt))//'(g0,","),g0)'
     fmt = Trim(Adjustl(fmt))
   
     Write(tmp1,fmt) (ES_LYR(i), i = 1, 9)
     Write(cES10, '(g0)') ES10
     tmp1 = Trim(Adjustl(tmp1)) // Trim(Adjustl(cES10)) 
   End IF
   
   tmp = Trim(Adjustl(tmp)) // Trim(Adjustl(tmp1)) 
   
   lngth = Len(Trim(Adjustl(tmp)))
   size = lngth
   Allocate(Character(Len=size)::Csvline)
   Csvline = Trim(Adjustl(tmp))
   pCsvline => Csvline
   Return
end Subroutine CsvOutET
!------------------------------------------------------------------------------
! Sub for csv output MZCER PlantGro.csv
Subroutine CsvOut_mzcer(EXCODE, RUN, TN, ROTNUM,  REPNO, YEAR, DOY, DAS, DAP, &
   VSTAGE, RSTAGE, XLAI, WTLF, STMWTO, SDWT, RTWT, PLTPOP, VWAD, TOPWT,SEEDNO,& 
   SDSIZE, HI, PODWT, PODNO, SWF_AV, TUR_AV, NST_AV, EXW_AV, PS1_AV, PS2_AV, &
   KST_AV, PCNL, SHELPC, HIP, PODWTD, SLA, CANHT, CANWH, RTDEP, N_LYR, RLV, &
   WTCO, WTLO, WTSO, CUMSENSURF, CUMSENSOIL, DTT, Csvline, pCsvline, lngth) 

!  Input vars
   Character(8),Intent(IN):: EXCODE    
   Integer,Intent(IN) :: RUN, TN, ROTNUM, REPNO, YEAR, DOY, DAS, DAP                     
!        INTEGER,Intent(in)      :: SN      ! Sequence number,crop rotation  #
!        INTEGER,Intent(in)      :: ON      ! Option number (sequence runs)  #
!        INTEGER,Intent(in)      :: CN      ! Crop component (multicrop)     #
   Real,Intent(IN) :: VSTAGE, XLAI, WTLF, STMWTO, SDWT, RTWT, PLTPOP, TOPWT  
   Integer,Intent(IN) :: RSTAGE, VWAD                            
   Real,Intent(IN) :: SEEDNO, SDSIZE, HI, PODWT, PODNO, SWF_AV, TUR_AV, NST_AV     
   Real,Intent(IN) :: EXW_AV, PS1_AV, PS2_AV, KST_AV, PCNL, SHELPC, HIP, PODWTD
   Real,Intent(IN) :: SLA, CANHT, CANWH, RTDEP, WTCO, WTLO, WTSO
   Integer,Intent(IN) :: N_LYR
   Real, Dimension(N_LYR),Intent(IN) :: RLV 
   Real :: CUMSENSURF,  CUMSENSOIL, DTT
     
!  Recalculated vars
   Integer :: cWTLF1, cSTMWT1, cSDWT1, cRTWT1, cTOPWT1, cSEEDNO1, cPODWT1
   Integer :: cPODNO1, cPODWTD1, cPodSum, cCUMSENSURF1, cCUMSENSOIL1 
   Real :: cRTDEP1  !cDWNOD1, 
   Integer :: cWTCO1, cWTLO1, cWTSO1
  
   Integer :: i
  
   Character(:), allocatable, Target, Intent(Out) :: Csvline
   Character(:), Pointer, Intent(Out) :: pCsvline
   Integer, Intent(Out) :: lngth
  
   Character(Len=800) :: tmp
   Character(Len=300) :: tmp1
   Character(Len=20) :: fmt      
!  End of vars
          
!  Recalculation
   cWTLF1 = NINT(WTLF*10.)
   cSTMWT1 = NINT(STMWTO*10.)
   cSDWT1 = NINT(SDWT*10.)
   cRTWT1 = NINT(RTWT*10.*PLTPOP)
   cTOPWT1 = NINT(TOPWT*10.)
   cSEEDNO1 = NINT(SEEDNO)
   cPODWT1 = NINT(PODWT*10.)
   cPODNO1 = NINT(PODNO)
   cPODWTD1 = NINT(PODWTD*10.)
   cPodSum = NINT((PODWTD+PODWT)*10.)
   cRTDEP1 = RTDEP/100.
   cWTCO1 = NINT(WTCO*10.)
   cWTLO1 = NINT(WTLO*10.)
   cWTSO1 = NINT(WTSO*10.)
   cCUMSENSURF1 = NINT(CUMSENSURF)  
   cCUMSENSOIL1 = NINT(CUMSENSOIL) 

   Write(tmp,'(45(g0,","))')RUN, EXCODE, TN, ROTNUM, REPNO, YEAR, DOY, DAS,DAP,& 
      VSTAGE, RSTAGE, XLAI, cWTLF1, cSTMWT1, cSDWT1, cRTWT1, VWAD, cTOPWT1, &
      cSEEDNO1, SDSIZE, HI, cPODWT1, cPODNO1, SWF_AV, TUR_AV, NST_AV, EXW_AV, &
      PS1_AV, PS2_AV, KST_AV, PCNL, SHELPC, HIP, cPODWTD1, cPodSum, SLA, &
      CANHT, CANWH, cRTDEP1, cWTCO1, cWTLO1, cWTSO1,cCUMSENSURF1,cCUMSENSOIL1,DTT  
    
   Write(fmt,'(I2)') N_LYR - 1   
   fmt = '('//trim(adjustl(fmt))//'(g0,","),g0)'
   fmt=trim(adjustl(fmt))
   
   Write(tmp1,fmt) (RLV(i), i = 1, N_LYR)
      
   tmp = trim(tmp) // trim(adjustl(tmp1))  
   
   lngth = Len(Trim(tmp))
   Allocate(Character(Len=Len(Trim(tmp)))::Csvline)
   Csvline = Trim(tmp)
   pCsvline => Csvline
   
   return
end Subroutine CsvOut_mzcer
!------------------------------------------------------------------------------
! Sub for csv output RICER PlantGro.csv
Subroutine CsvOut_RIcer(EXCODE, RUN, TN, ROTNUM,  REPNO, YEAR, DOY, DAS, DAP, &
   VSTAGE, RSTAGE, XLAI,                           &
   cWTLF, cSTMWT, cSDWT,                        &
   cRTWT, cPANWT, cTOPWT,                       &
   SEEDNO, SDSIZE, HI, cTILNO, SWF_AV,            &
   TUR_AV, SAT_AV, NST_AV, KST_AV, PCNL, SHELPC,   &
   SLA, CANHT, CANWH, cRTDEP, RLV,                &
   CUMSENSURF, CUMSENSOIL, DTTC,                    &
   Csvline, pCsvline, lngth)

!  Input vars
   Character(8),Intent(IN):: EXCODE    
   Integer,Intent(IN) :: RUN, TN, ROTNUM, REPNO, YEAR, DOY, DAS, DAP                     
!        INTEGER,Intent(in)      :: SN      ! Sequence number,crop rotation  #
!        INTEGER,Intent(in)      :: ON      ! Option number (sequence runs)  #
!        INTEGER,Intent(in)      :: CN      ! Crop component (multicrop)     #
   Real,Intent(IN) :: VSTAGE, XLAI, cWTLF, cSTMWT, cSDWT, cRTWT, cPANWT, cTOPWT  
   Integer,Intent(IN) :: RSTAGE, DTTC                            
   Real,Intent(IN) :: SEEDNO, SDSIZE, HI, cTILNO, SWF_AV, TUR_AV, SAT_AV, NST_AV     
   Real,Intent(IN) :: KST_AV, PCNL, SHELPC
   Real,Intent(IN) :: SLA, CANHT, CANWH, cRTDEP 
!  Integer,Intent(IN) :: N_LYR
   Real, Dimension(5),Intent(IN) :: RLV 
   Real :: CUMSENSURF,  CUMSENSOIL
     
!  Recalculated vars
   Integer :: cWTLF1, cSTMWT1, cSDWT1, cRTWT1, cTOPWT1, cSEEDNO1, cPANWT1 !, cPODWT1
   Integer :: cCUMSENSURF1, cCUMSENSOIL1 !cPODNO1, cPODWTD1, cPodSum, 
!  Real :: cDWNOD1, cRTDEP1
   Integer :: cTILNO1   !cWTCO1, cWTLO1, cWTSO1, 
  
   Integer :: i
  
   Character(:), allocatable, Target, Intent(Out) :: Csvline
   Character(:), Pointer, Intent(Out) :: pCsvline
   Integer, Intent(Out) :: lngth
  
   Character(Len=800) :: tmp
   Character(Len=300) :: tmp1
   Character(Len=20) :: fmt      
!  End of vars
          
!  Recalculation
   cWTLF1  = NINT(cWTLF)
   cSTMWT1 = NINT(cSTMWT)
   cSDWT1  = NINT(cSDWT)
   cRTWT1  = NINT(cRTWT)
   cPANWT1 = NINT(cPANWT)
   cTOPWT1 = NINT(cTOPWT)
   cSEEDNO1= NINT(SEEDNO)
   cTILNO1 = NINT(cTILNO)
!   cPODWT1 = NINT(PODWT*10.)
!   cPODNO1 = NINT(PODNO)
!   cPODWTD1 = NINT(PODWTD*10.)
!   cPodSum = NINT((PODWTD+PODWT)*10.)
!   cRTDEP1 = RTDEP/100.
!   cWTCO1 = NINT(WTCO*10.)
!   cWTLO1 = NINT(WTLO*10.)
!   cWTSO1 = NINT(WTSO*10.)
   cCUMSENSURF1 = NINT(CUMSENSURF)  
   cCUMSENSOIL1 = NINT(CUMSENSOIL) 

   Write(tmp,'(45(g0,","))') RUN, EXCODE, TN, ROTNUM, REPNO, YEAR, DOY, DAS, DAP,& 
      VSTAGE, RSTAGE, XLAI,                           &
      cWTLF1, cSTMWT1, cSDWT1,                        &
      cRTWT1, cPANWT1, cTOPWT1,                       &
      cSEEDNO1, SDSIZE, HI, cTILNO1, SWF_AV,          &
      TUR_AV, SAT_AV, NST_AV, KST_AV, PCNL, SHELPC,   &
      SLA, CANHT, CANWH, cRTDEP, RLV,                 &
      cCUMSENSURF1, cCUMSENSOIL1, DTTC

   Write(fmt,'(I2)') 5 - 1   
   fmt = '('//trim(adjustl(fmt))//'(g0,","),g0)'
   fmt=trim(adjustl(fmt))
   
   Write(tmp1,fmt) (RLV(i), i = 1, 5)
      
   tmp = trim(tmp) // trim(adjustl(tmp1))  
   
   lngth = Len(Trim(tmp))
   Allocate(Character(Len=Len(Trim(tmp)))::Csvline)
   Csvline = Trim(tmp)
   pCsvline => Csvline
   
   return
end Subroutine CsvOut_RIcer
!------------------------------------------------------------------------------
! Sub for csv output SUOIL PlantGro.csv
Subroutine CsvOut_SUOIL(EXCODE, RUN, TN, ROTNUM,  REPNO, YEAR, DOY, DAS, DAP, &
   VSTAGE, RSTAGE, XLAI, WTLF, STMWTO, SDWT, RTWT, PLTPOP, VWAD, TOPWT,SEEDNO,& 
   SDSIZE, HI, OILWT, OILPC, SWF_AV, TUR_AV, NST_AV, EXW_AV, PS1_AV, PS2_AV, &
   KST_AV, PCNL, SHELPC, HIO, SLA, CANHT, CANWH, RTDEP, N_LYR, RLV, &
   WTCO, WTLO, WTSO, CUMSENSURF, CUMSENSOIL, DTT, Csvline, pCsvline, lngth) 

!  Input vars
   Character(8),Intent(IN):: EXCODE    
   Integer,Intent(IN) :: RUN, TN, ROTNUM, REPNO, YEAR, DOY, DAS, DAP                     
!        INTEGER,Intent(in)      :: SN      ! Sequence number,crop rotation  #
!        INTEGER,Intent(in)      :: ON      ! Option number (sequence runs)  #
!        INTEGER,Intent(in)      :: CN      ! Crop component (multicrop)     #
  Real,Intent(IN) :: VSTAGE, XLAI, WTLF, STMWTO, SDWT, RTWT, PLTPOP, TOPWT  
   Integer,Intent(IN) :: RSTAGE, VWAD                            
   Real,Intent(IN) :: SEEDNO, SDSIZE, HI, OILWT, OILPC, SWF_AV, TUR_AV, NST_AV     
   Real,Intent(IN) :: EXW_AV, PS1_AV, PS2_AV, KST_AV, PCNL, SHELPC, HIO
   Real,Intent(IN) :: SLA, CANHT, CANWH, RTDEP, WTCO, WTLO, WTSO
   Integer,Intent(IN) :: N_LYR
   Real, Dimension(N_LYR),Intent(IN) :: RLV 
   Real :: CUMSENSURF,  CUMSENSOIL, DTT
     
!  Recalculated vars
   Integer :: cWTLF1, cSTMWT1, cSDWT1, cRTWT1, cTOPWT1, cSEEDNO1  !, cPODWT1
   Integer :: cCUMSENSURF1, cCUMSENSOIL1 ! cPODNO1, cPODWTD1, cPodSum, 
   Real :: cRTDEP1,cOILWT1,COILPC   !cDWNOD1, 
   Integer :: cWTCO1, cWTLO1, cWTSO1

  Integer :: i
  
   Character(:), allocatable, Target, Intent(Out) :: Csvline
   Character(:), Pointer, Intent(Out) :: pCsvline
   Integer, Intent(Out) :: lngth
  
   Character(Len=800) :: tmp
   Character(Len=300) :: tmp1
   Character(Len=20) :: fmt      
!  End of vars 
   cWTLF1 = NINT(WTLF*10.)
   cSTMWT1 = NINT(STMWTO*10.)
   cSDWT1 = NINT(SDWT*10.)
   cRTWT1 = NINT(RTWT*10.*PLTPOP)
   cTOPWT1 = NINT(TOPWT*10.)
   cSEEDNO1 = NINT(SEEDNO)
   cOILWT1 = NINT(OILWT*10.)
   cOILPC = NINT(OILPC)
   cRTDEP1 = RTDEP/100.
   cWTCO1 = NINT(WTCO*10.)
   cWTLO1 = NINT(WTLO*10.)
   cWTSO1 = NINT(WTSO*10.)
   cCUMSENSURF1 = NINT(CUMSENSURF)  
   cCUMSENSOIL1 = NINT(CUMSENSOIL) 

   Write(tmp,'(45(g0,","))')RUN, EXCODE, TN, ROTNUM, REPNO, YEAR, DOY, DAS,DAP,& 
      VSTAGE, RSTAGE, XLAI, cWTLF1, cSTMWT1, cSDWT1, cRTWT1, VWAD, cTOPWT1, &
      cSEEDNO1, SDSIZE, HI, cOILWT1, cOILPC, SWF_AV, TUR_AV, NST_AV, EXW_AV, &
      PS1_AV, PS2_AV, KST_AV, PCNL, SHELPC, HIO, SLA, &
      CANHT, CANWH, cRTDEP1, cWTCO1, cWTLO1, cWTSO1,cCUMSENSURF1,cCUMSENSOIL1,DTT  
    
   Write(fmt,'(I2)') N_LYR - 1   
   fmt = '('//trim(adjustl(fmt))//'(g0,","),g0)'
   fmt=trim(adjustl(fmt))
   
   Write(tmp1,fmt) (RLV(i), i = 1, N_LYR)

     tmp = trim(tmp) // trim(adjustl(tmp1))  
   
   lngth = Len(Trim(tmp))
   Allocate(Character(Len=Len(Trim(tmp)))::Csvline)
   Csvline = Trim(tmp)
   pCsvline => Csvline
   return

end Subroutine CsvOut_SUOIL
!------------------------------------------------------------------------------
! Sub for plantn.csv output
Subroutine CsvOutPlNSUOIL(EXCODE, RUNRUNI, TN, ROTNUM, REPNO, YEAR, DOY, DAS, &
   DAP, WTNCAN, WTNSD, WTNVEG, PCNGRN, PCNVEG, WTNUP, WTNLF, WTNST, PCNL, &
   PCNST, PCNRT, CUMSENSURFN, CUMSENSOILN, Csvline, pCsvline, lngth) 
    
!  Input vars
   Character(8),Intent(IN):: EXCODE    
   Integer,Intent(in) :: RUNRUNI, TN, ROTNUM, REPNO, YEAR, DOY, DAS, DAP       
!  Integer, Intent(IN) :: RUN        ! run number        
!  INTEGER,Intent(in)  :: SN         ! Sequence number,crop rotation  #
!  INTEGER,Intent(in)  :: ON         ! Option number (sequence runs)  #
!  INTEGER,Intent(in)  :: CN         ! Crop component (multicrop)     #
   REAL,Intent(IN) :: WTNCAN, WTNSD, WTNVEG, PCNGRN, PCNVEG, WTNUP, WTNLF
   REAL,Intent(IN) :: WTNST, PCNL, PCNST, PCNRT, CUMSENSURFN, CUMSENSOILN
  
   Character(:), allocatable, Target, Intent(Out) :: Csvline
   Character(:), Pointer, Intent(Out) :: pCsvline
   Integer, Intent(Out) :: lngth
   Integer :: size
   Character(Len=350) :: tmp      
!  End of vars
  
!  Recalculated vars
   Real :: cWTNCAN1, cWTNSD1, cWTNVEG1, cWTNUP1, cWTNLF1, cWTNST1  
  
   cWTNCAN1 = WTNCAN * 10.0
   cWTNSD1  = WTNSD  * 10.0
   cWTNVEG1 = WTNVEG * 10.0
   cWTNUP1  = WTNUP  * 10.0
   cWTNLF1  = WTNLF  * 10.0
   cWTNST1  = WTNST  * 10.0
             
   Write(tmp,'(21(g0,","),g0)') RUNRUNI, EXCODE, TN, ROTNUM, REPNO, YEAR, DOY, &
      DAS, DAP, cWTNCAN1, cWTNSD1, cWTNVEG1, PCNGRN, PCNVEG, cWTNUP1, &
      cWTNLF1, cWTNST1, PCNL, PCNST, PCNRT, CUMSENSURFN, CUMSENSOILN  
   
   lngth = Len(Trim(Adjustl(tmp)))
   size = lngth
   Allocate(Character(Len = size)::Csvline)
   Csvline = Trim(Adjustl(tmp))
   pCsvline => Csvline
   Return
end Subroutine CsvOutPlNSUOIL
!------------------------------------------------------------------------------
! Sub for plantn.csv output
Subroutine CsvOutPlNCrGro(EXCODE, RUN, TN, ROTNUM, REPNO, YEAR, DOY, DAS, DAP,&
   WTNCAN, WTNSD, WTNVEG, PCNSDP, PCNVEG, WTNFX, WTNUP, WTNLF, WTNST, PCNLP, &
   PCNSTP, PCNSHP, PCNRTP, NFIXN, CUMSENSURFN, CUMSENSOILN, &
   Csvline, pCsvline, lngth) 
    
!  Input vars
   Character(8),Intent(IN):: EXCODE    
   Integer, Intent(IN) :: RUN, TN, ROTNUM, REPNO, YEAR, DOY, DAS, DAP           
!   INTEGER,Intent(in)      :: SN         ! Sequence number,crop rotation  #
!   INTEGER,Intent(in)      :: ON         ! Option number (sequence runs)  #
!   INTEGER,Intent(in)      :: CN         ! Crop component (multicrop)     #
   Real,Intent(IN) :: WTNCAN, WTNSD, WTNVEG, PCNSDP, PCNVEG, WTNFX, WTNUP
   REAL,Intent(IN) :: WTNLF, WTNST, PCNLP, PCNSTP, PCNSHP, PCNRTP, NFIXN
   REAL,Intent(IN) :: CUMSENSURFN, CUMSENSOILN 
  
   Character(:), allocatable, Target, Intent(Out) :: Csvline
   Character(:), Pointer, Intent(Out) :: pCsvline
   Integer, Intent(Out) :: lngth
   Integer :: size
   Character(Len=400) :: tmp      
!  End of vars
  
!  Recalculated vars
   Real :: cWTNCAN1, cWTNSD1, cWTNVEG1, cWTNFX1, cWTNUP1, cWTNLF1, cWTNST1  
   Real :: cNFIXN1
  
   cWTNCAN1 = WTNCAN * 10.0
   cWTNSD1  = WTNSD  * 10.0
   cWTNVEG1 = WTNVEG * 10.0
   cWTNFX1  = WTNFX  * 10.0
   cWTNUP1  = WTNUP  * 10.0
   cWTNLF1  = WTNLF  * 10.0
   cWTNST1  = WTNST  * 10.0
   cNFIXN1  = NFIXN  * 10.0
  
!  Unofmatted   
   Write(tmp,'(24(g0,","),g0)') RUN, EXCODE, TN, ROTNUM, REPNO, YEAR, DOY, DAS, &
      DAP, cWTNCAN1, cWTNSD1, cWTNVEG1, PCNSDP, PCNVEG, cWTNFX1, cWTNUP1, cWTNLF1,&
      cWTNST1, PCNLP, PCNSTP, PCNSHP, PCNRTP, cNFIXN1, CUMSENSURFN, CUMSENSOILN
   
   lngth = Len(Trim(Adjustl(tmp)))
   size = lngth
   Allocate(Character(Len = size)::Csvline)
   Csvline = Trim(Adjustl(tmp))
   pCsvline => Csvline
   Return
end Subroutine CsvOutPlNCrGro
!------------------------------------------------------------------------------
! Sub for plantn.csv output
Subroutine CsvOutPlNCsCer(EXCODE, RUN, TN, ROTNUM,  REPNO, YEAR, DOY, DAS,&
   DAP, TMEAN, ZSTAGE, NUAD, TNAD, SDNAD, RNAD, CNAD, LLNAD, SNAD, GNAD, HIND,&
   RSNAD, DNAD, SENN0C, SENNSC, RANC, LANC, SANC, GRAINANC, SDNC, VANC, LCNF, &
   SCNF, RCNF, VCNC, VMNC, NUPR, ANDEM, Csvline, pCsvline, lngth) 
    
!  Input vars
   CHARACTER(10),Intent(IN):: EXCODE    ! Experiment code/name           text
!   CHARACTER(8),Intent(in) :: RUNRUNI    ! Run+internal run number        text 
  Integer, Intent(IN) :: RUN           ! run number        
   INTEGER,Intent(IN)  :: TN, ROTNUM, REPNO, YEAR, DOY, DAS, DAP          ! 
!    INTEGER,Intent(in)      :: SN         ! Sequence number,crop rotation  #
!    INTEGER,Intent(in)      :: ON         ! Option number (sequence runs)  #
!    INTEGER,Intent(in)      :: CN         ! Crop component (multicrop)     #
   REAL,Intent(IN) :: TMEAN, ZSTAGE, NUAD, TNAD, SDNAD, RNAD, CNAD, LLNAD, SNAD 
   REAL,Intent(IN) :: GNAD, HIND, RSNAD, DNAD, RANC, LANC, SANC, GRAINANC
   REAL,Intent(IN) :: SDNC, VANC, LCNF, SCNF, RCNF, VCNC, VMNC, NUPR, ANDEM
  
   Character(6),Intent(IN) :: SENN0C, SENNSC
  
   Character(:), allocatable, Target, Intent(Out) :: Csvline
   Character(:), Pointer, Intent(Out) :: pCsvline
   Integer, Intent(Out) :: lngth
   Character(Len=600) :: tmp 
   Integer :: size     
!  End of vars
  
!  Recalculated vars
   Real :: cRANC1, cLANC1, cSANC1, cGRAINANC1, cSDNC1, cVANC1, cVCNC1, cVMNC1, cNUPR1
  
   cRANC1 = RANC * 100.0 
   cLANC1 = LANC * 100.0 
   cSANC1 = SANC * 100.0
   cGRAINANC1 = GRAINANC * 100.0
   cSDNC1 = SDNC * 100.0 
   cVANC1 = VANC * 100.0 
   cVCNC1 = VCNC * 100.0 
   cVMNC1 = VMNC * 100.0
   cNUPR1 = AMIN1(2.0,NUPR)
             
   Write(tmp,'(36(g0,","),g0)') RUN, EXCODE, TN, ROTNUM, REPNO, YEAR, DOY, DAS, DAP,&
   TMEAN, ZSTAGE, NUAD, TNAD, SDNAD, RNAD, CNAD, LLNAD, SNAD, GNAD, HIND, RSNAD, DNAD,&
   SENN0C, SENNSC, cRANC1, cLANC1, cSANC1, cGRAINANC1, cSDNC1, cVANC1, LCNF, SCNF, RCNF,&
   cVCNC1, cVMNC1, cNUPR1, ANDEM
   
   lngth = Len(Trim(Adjustl(tmp)))
   size = lngth
   Allocate(Character(Len=size)::Csvline)
   Csvline = Trim(Adjustl(tmp))
   pCsvline => Csvline
   Return
end Subroutine CsvOutPlNCsCer
!------------------------------------------------------------------------------
! Sub for soilni.csv output
Subroutine CsvOutSoilNi(EXCODE, RUN, TN, ROTNUM, REPNO, YEAR, DOY, DAS, N, &
   AMTFER, NAPFER, TLCH, TNH4NO3, TNO3, TNH4, N_LYR, NO3, NH4, CMINERN, &
   CNITRIFY, TNOX, CIMMOBN, TOTAML, CNETMINRN, CNUPTAKE, &  
   Csvline, pCsvline, lngth) 
    
!  Input vars
   Character(8),Intent(IN):: EXCODE    
   Integer,Intent(in) :: RUN, TN, ROTNUM, REPNO, YEAR, DOY, DAS, N  
!  Integer,Intent(IN) :: RUN           ! run number        
!  Integer,Intent(in) :: SN         ! Sequence number,crop rotation  #
!  Integer,Intent(in) :: ON         ! Option number (sequence runs)  #
!  INTEGER,Intent(in) :: CN         ! Crop component (multicrop)     #
   Real,Dimension(N),Intent(IN)      :: AMTFER  
   Integer,Dimension(N),Intent(IN)   :: NAPFER
   Real,Intent(IN) :: TLCH, TNH4NO3, TNO3, TNH4
   Integer,Intent(IN) :: N_LYR 
   Real,Dimension(N_LYR),Intent(IN) :: NO3
   Real,Dimension(N_LYR),Intent(IN) :: NH4   
   Real,Intent(IN) :: CMINERN, CNITRIFY, TNOX, CIMMOBN, TOTAML 
   Real,Intent(IN) :: CNETMINRN, CNUPTAKE  
  
   Character(:), allocatable, Target, Intent(Out) :: Csvline
   Character(:), Pointer, Intent(Out) :: pCsvline
   Integer, Intent(Out) :: lngth
   Character(Len=700) :: tmp
   Character(Len=300) :: tmp1
   Character(Len=250) :: tmp2, tmp3, tmp4
   Character(Len=20) :: fmt
  
   Integer :: i, size      
!  End of vars
  
!  Recalculated vars
   Integer :: cAMTFER1, cNAPFER1  
  
   cAMTFER1 = NINT(AMTFER(N)) 
   cNAPFER1 = NAPFER(N)
              
   Write(tmp1,'(14(g0,","))') RUN, EXCODE, TN, ROTNUM, REPNO, YEAR, DOY, &
      DAS, cAMTFER1, cNAPFER1, TLCH, TNH4NO3, TNO3, TNH4
   
   If (N_LYR >= 10) Then
      fmt = '(10(g0,","))'
      Write(tmp2,fmt) (NO3(i), i = 1, 10)
      Write(tmp3,fmt) (NH4(i), i = 1, 10)
   Else
      Write(fmt,'(I1)') N_LYR   
      fmt = '('//Trim(Adjustl(fmt))//'(g0,","))'
      fmt = Trim(Adjustl(fmt))
      Write(tmp2,fmt) (NO3(i), i = 1, N_LYR)
      Write(tmp3,fmt) (NH4(i), i = 1, N_LYR)
      
      Do i = 1, 10 - N_LYR
         tmp2 = Trim(Adjustl(tmp2)) // '-99,'
         tmp3 = Trim(Adjustl(tmp3)) // '-99,'
      End Do
   End If
    
   Write(tmp4,'(6(g0,","),g0)') CMINERN, CNITRIFY, TNOX, CIMMOBN, TOTAML, &
      CNETMINRN, CNUPTAKE     
   
   tmp = Trim(Adjustl(tmp1)) // Trim(Adjustl(tmp2)) // &
         Trim(Adjustl(tmp3)) // Trim(Adjustl(tmp4))
   
   lngth = Len(Trim(Adjustl(tmp)))
   size = lngth
   Allocate(Character(Len = size)::Csvline)
   Csvline = Trim(Adjustl(tmp))
   pCsvline => Csvline
   Return
end Subroutine CsvOutSoilNi
!------------------------------------------------------------------------------
! Sub for plantn.csv output
Subroutine CsvOutPlNMzCer(EXCODE, RUNRUNI, TN, ROTNUM, REPNO, YEAR, DOY, DAS, &
   DAP, WTNCAN, WTNSD, WTNVEG, PCNGRN, PCNVEG, WTNUP, WTNLF, WTNST, PCNL, &
   PCNST, PCNRT, CUMSENSURFN, CUMSENSOILN, Csvline, pCsvline, lngth) 
    
!  Input vars
   Character(8),Intent(IN):: EXCODE    
   Integer,Intent(in) :: RUNRUNI, TN, ROTNUM, REPNO, YEAR, DOY, DAS, DAP       
!  Integer, Intent(IN) :: RUN        ! run number        
!  INTEGER,Intent(in)  :: SN         ! Sequence number,crop rotation  #
!  INTEGER,Intent(in)  :: ON         ! Option number (sequence runs)  #
!  INTEGER,Intent(in)  :: CN         ! Crop component (multicrop)     #
   REAL,Intent(IN) :: WTNCAN, WTNSD, WTNVEG, PCNGRN, PCNVEG, WTNUP, WTNLF
   REAL,Intent(IN) :: WTNST, PCNL, PCNST, PCNRT, CUMSENSURFN, CUMSENSOILN
  
   Character(:), allocatable, Target, Intent(Out) :: Csvline
   Character(:), Pointer, Intent(Out) :: pCsvline
   Integer, Intent(Out) :: lngth
   Integer :: size
   Character(Len=350) :: tmp      
!  End of vars
  
!  Recalculated vars
   Real :: cWTNCAN1, cWTNSD1, cWTNVEG1, cWTNUP1, cWTNLF1, cWTNST1  
  
   cWTNCAN1 = WTNCAN * 10.0
   cWTNSD1  = WTNSD  * 10.0
   cWTNVEG1 = WTNVEG * 10.0
   cWTNUP1  = WTNUP  * 10.0
   cWTNLF1  = WTNLF  * 10.0
   cWTNST1  = WTNST  * 10.0
             
   Write(tmp,'(21(g0,","),g0)') RUNRUNI, EXCODE, TN, ROTNUM, REPNO, YEAR, DOY, &
      DAS, DAP, cWTNCAN1, cWTNSD1, cWTNVEG1, PCNGRN, PCNVEG, cWTNUP1, &
      cWTNLF1, cWTNST1, PCNL, PCNST, PCNRT, CUMSENSURFN, CUMSENSOILN  
   
   lngth = Len(Trim(Adjustl(tmp)))
   size = lngth
   Allocate(Character(Len = size)::Csvline)
   Csvline = Trim(Adjustl(tmp))
   pCsvline => Csvline
   Return
end Subroutine CsvOutPlNMzCer
!------------------------------------------------------------------------------
! Sub for weather.csv output
Subroutine CsvOutWth(EXCODE, RUN, TN, ROTNUM, REPNO, YEAR, DOY, DAS, RAIN, &
   DAYL, TWILEN, SRAD, PAR, CLOUDS, TMAX, TMIN, TAVG, TDAY, TDEW, TGROAV, &
   TGRODY, WINDSP, CO2, VPDF, vpd_transp, Csvline, pCsvline, lngth) 
    
!  Input vars
   Character(8),Intent(IN):: EXCODE    
   Integer,Intent(in) :: RUN, TN, ROTNUM, REPNO, YEAR, DOY, DAS        
!  INTEGER,Intent(in)      :: SN         ! Sequence number,crop rotation  #
!  INTEGER,Intent(in)      :: ON         ! Option number (sequence runs)  #
!  INTEGER,Intent(in)      :: CN         ! Crop component (multicrop)     #  
   REAL,Intent(IN) :: RAIN, DAYL, TWILEN, SRAD, PAR, CLOUDS, TMAX, TMIN, TAVG
   REAL,Intent(IN) :: TDAY, TDEW, TGROAV, TGRODY, WINDSP, CO2, VPDF, vpd_transp
  
   Character(:), allocatable, Target, Intent(Out) :: Csvline
   Character(:), Pointer, Intent(Out) :: pCsvline
   Integer, Intent(Out) :: lngth
   Integer :: size
   Character(Len=400) :: tmp      
!  End of vars

   Write(tmp,'(24(g0,","),g0)') RUN, EXCODE, TN, ROTNUM, REPNO, YEAR, DOY, &
     DAS, RAIN, DAYL, TWILEN, SRAD, PAR, CLOUDS, TMAX, TMIN, TAVG, TDAY, TDEW, &
     TGROAV, TGRODY, WINDSP, CO2, VPDF, vpd_transp
      
   lngth = Len(Trim(Adjustl(tmp)))
   size = lngth
   Allocate(Character(Len = size)::Csvline)
   Csvline = Trim(Adjustl(tmp))
   pCsvline => Csvline
   Return
end Subroutine CsvOutWth
!----------------------------------------------------------------------------------
! Sub for output PlantGr2.csv
Subroutine CsvOutPlGr2(EXCODE, RUN, TN, RN, SN, ON, REP, CN, YEAR, DOY, DAS, &
   DAP, TMEAN, GSTAGEC, RSTAGE, LAIPRODC, SENLA, PLTPOP, LAIC, CANHTC, SDWAD, &
   SENW0C, SENWSC, GRNUMAD, HWUDC, SHRTD, PTF, RTDEP, N_LYR, RLV, &
   Csvline, pCsvline, lngth)
         
!  Input vars
   CHARACTER(10),Intent(in):: EXCODE     ! Experiment code/name           
!   CHARACTER(8),Intent(in) :: RUNRUNI    ! Run+internal run number 
   Integer, Intent(IN) :: RUN           ! run number       
   INTEGER,Intent(in) :: TN, RN, SN, ON, REP, CN, YEAR, DOY, DAS, DAP  
   REAL,Intent(in) :: TMEAN, RSTAGE, SENLA, PLTPOP, SDWAD, GRNUMAD, SHRTD, PTF, RTDEP          
   CHARACTER(Len=6),Intent(in) :: GSTAGEC, LAIPRODC, LAIC, CANHTC, SENW0C, SENWSC, HWUDC  
   Integer,Intent(in) :: N_LYR
   REAL, Dimension(20), Intent(in) :: RLV  ! Root length volume by layer    /cm2
   
!  Temp vars
!  integer :: length
!  Recalculated vars
   REAL :: cLAISD, cRTDEP1
   Integer :: cGRNUMAD1
   
   Character(:), allocatable, Target, Intent(Out) :: Csvline
   Character(:), Pointer, Intent(Out) :: pCsvline
   Integer, Intent(Out) :: lngth
   Character(Len=750) :: tmp
   Character(Len=400) :: tmp1
   Character(Len=20) :: fmt
   Integer :: i, size, nl
!  End of vars

!  Recalculation
   cLAISD = SENLA * PLTPOP * 0.0001
   cGRNUMAD1 = NINT(GRNUMAD)
   cRTDEP1 = RTDEP/100.0
   
   Write(tmp,'(27(g0,","))') RUN, EXCODE(1:8), TN, RN, SN, ON, REP, &
      CN, YEAR, DOY, DAS, DAP, TMEAN, GSTAGEC, RSTAGE, LAIPRODC, cLAISD, LAIC, &
      CANHTC, SDWAD, SENW0C, SENWSC, cGRNUMAD1, HWUDC, SHRTD, PTF, cRTDEP1

   nl = MIN(10, N_LYR)   ! limit output to 10 as in CSCER
   
   Write(fmt,'(I2)') nl - 1 
   fmt = '('//Trim(Adjustl(fmt))//'(g0,","),g0)'
   fmt = Trim(Adjustl(fmt))
   
   Write(tmp1,fmt) (RLV(i), i = 1, nl)            
   
   tmp = Trim(Adjustl(tmp)) // Trim(Adjustl(tmp1)) 
   
   lngth = Len(Trim(Adjustl(tmp)))
   size = lngth
   Allocate(Character(Len=size)::Csvline)
   Csvline = Trim(Adjustl(tmp))
   pCsvline => Csvline
   Return
end Subroutine CsvOutPlGr2
!------------------------------------------------------------------------------------ 
! Sub for output PlantGrf.csv
Subroutine CsvOutPlGrf(EXCODE, RUN, TN, RN, SN, ON, REP, CN, YEAR, DOY, DAS, &
   DAP, TMEAN, ZSTAGE, DU, VF, DF, TFGEM, WFGE, TFP, WFP, NFP, CO2FP, RSFP, TFG, &
   WFG, NFG, WFT, NFT, WAVR, WUPR, H2OA, EOP, SNH4PROFILE, SNO3PROFILE, LCNF, SCNF,&
   RCNF, Csvline, pCsvline, lngth)
    
!  Input vars
   CHARACTER(10),Intent(in):: EXCODE     ! Experiment code/name           text
!   CHARACTER(8),Intent(in) :: RUNRUNI    ! Run+internal run number        text
   Integer, Intent(IN) :: RUN           ! run number
   INTEGER,Intent(in) :: TN, RN, SN, ON, REP, CN, YEAR, DOY, DAS, DAP        
   REAL,Intent(in) :: TMEAN, ZSTAGE, DU, VF, DF, TFGEM, WFGE, TFP, WFP, NFP, CO2FP           
   REAL,Intent(in) :: RSFP, TFG, WFG, NFG, WFT, NFT, WAVR, WUPR, H2OA, EOP, SNH4PROFILE   
   REAL,Intent(in) :: SNO3PROFILE, LCNF, SCNF, RCNF          
   
!  Temp vars
!  Integer :: length
!  Recalculated vars
   REAL :: cVF1, cDF1, cTFGEM1, cWFGE1, cTFP1, cWFP1, cNFP1, cCO2FP1
   REAL :: cRSFP1, cTFG1, cWFG1, cNFG1, cWFT1, cNFT1, cWAVR1, cWUPR1, cPROFILE    
   
   Character(:), allocatable, Target, Intent(Out) :: Csvline
   Character(:), Pointer, Intent(Out) :: pCsvline
   Integer, Intent(Out) :: lngth
   Character(Len=600) :: tmp 
   Integer :: size
!  End of vars

!  Recalculation
   cVF1 = 1.0 - VF
   cDF1 = 1.0 - DF
   cTFGEM1 = 1.0 - TFGEM
   cWFGE1 = 1.0 - WFGE
   cTFP1 = 1.0 - TFP
   cWFP1 = 1.0 - WFP
   cNFP1 = 1.0 - NFP
   cCO2FP1 = 1.0 - CO2FP
   cRSFP1 = 1.0 - RSFP
   cTFG1 = 1.0 - TFG
   cWFG1 = 1.0 - WFG 
   cNFG1 = 1.0 - NFG
   cWFT1 = 1.0 - WFT
   cNFT1 = 1.0 - NFT
   cWAVR1 = AMIN1(99.9,WAVR)
   cWUPR1 = AMIN1(15.0,WUPR)
   cPROFILE = SNH4PROFILE + SNO3PROFILE
   
   Write(tmp,'(36(g0,","),g0)') RUN, EXCODE(1:8), TN, RN, SN, ON, REP,&
      CN, YEAR, DOY, DAS, DAP, TMEAN, ZSTAGE, DU, cVF1, cDF1, cTFGEM1, cWFGE1, &
      cTFP1, cWFP1, cNFP1, cCO2FP1, cRSFP1, cTFG1, cWFG1, cNFG1, cWFT1, cNFT1, &
      cWAVR1, cWUPR1, H2OA, EOP, cPROFILE, LCNF, SCNF, RCNF
   
   lngth = Len(Trim(Adjustl(tmp)))
   size = lngth
   Allocate(Character(Len=size)::Csvline)
   Csvline = Trim(Adjustl(tmp))
   pCsvline => Csvline
   Return
end Subroutine CsvOutPlGrf
!------------------------------------------------------------------------------------
! Sub for csv output for CSCER
Subroutine CsvOutEvalCsCer(EXCODE, RUN, TN, ROTNUM,  REPNO, CR, Edap, Edapm, &
   Drdap, Drdapm, Tsdap, Tsdapm, Adap, Adapm, Mdap, Mdapm, Gwam, Gwamm, Gwumc, &
   Gwummc, Hnumam, Hnumamm, Hnumgm, Hnumgmm, Laix, Laixm, Lnumsm, Lnumsmm, Tnumam, &
   Tnumamm, Cwam, Cwamm, Vwam, Vwamm, Hiamc, Hiammc, Gnpcm, Gnpcmm, Vnpcmc, Vnpcmmc,&
   Cnam, Cnamm, Gnam, Gnamm, Hinmc, Hinmmc, Csvline, pCsvline, lngth) 
    
!  Input vars
   CHARACTER(10),Intent(IN):: EXCODE    ! Experiment code/name           text
   
!   CHARACTER(8),Intent(in) :: RUNRUNI    ! Run+internal run number        text 
   Integer, Intent(IN) :: RUN           ! run number        
   
   INTEGER,Intent(IN)  :: TN, ROTNUM, REPNO, Edap, Edapm, Drdap, Drdapm, Tsdap                             
!  INTEGER,Intent(in)      :: SN         ! Sequence number,crop rotation  #
!  INTEGER,Intent(in)      :: ON         ! Option number (sequence runs)  #
!  INTEGER,Intent(in)      :: CN         ! Crop component (multicrop)     #
   INTEGER,Intent(IN) :: Tsdapm, Adap, Adapm, Mdap, Mdapm
   REAL,Intent(IN) :: Gwam, Gwamm, Hnumam, Hnumamm, Hnumgm, Hnumgmm, Laix, Laixm
   REAL,Intent(IN) :: Lnumsm, Lnumsmm, Tnumam, Tnumamm, Cwam, Cwamm, Vwam, Vwamm
   REAL,Intent(IN) :: Gnpcm, Gnpcmm, Cnam, Cnamm, Gnam, Gnamm
   CHARACTER(2),Intent(in) :: CR
   CHARACTER(6),Intent(in) :: Gwumc, Gwummc, Hiamc, Hiammc, Vnpcmc, Vnpcmmc
   CHARACTER(6),Intent(in) :: Hinmc, Hinmmc
  
   Character(:), allocatable, Target, Intent(Out) :: Csvline
   Character(:), Pointer, Intent(Out) :: pCsvline
   Integer, Intent(Out) :: lngth
   Character(Len=750) :: tmp  
   Integer :: size    
!  End of vars
  
!  Recalculated vars
   Integer :: cGwam1, cGwamm1, cHnumam1, cHnumamm1, cTnumam1, cTnumamm1
   Integer :: cCwam1, cCwamm1, cVwam1, cVwamm1, cCnam1, cCnamm1 
   Integer :: cGnam1, cGnamm1  
  
   cGwam1 = NINT(Gwam)
   cGwamm1 = NINT(Gwamm)
   cHnumam1 = NINT(Hnumam)
   cHnumamm1 = NINT(Hnumamm)
   cTnumam1 = NINT(Tnumam)
   cTnumamm1 = NINT(Tnumamm)
   cCwam1 = NINT(Cwam)
   cCwamm1 = NINT(Cwamm)
   cVwam1 = NINT(Vwam)
   cVwamm1 = NINT(Vwamm)
   cCnam1 = NINT(Cnam)
   cCnamm1 = NINT(Cnamm)
   cGnam1 = NINT(Gnam)
   cGnamm1 = NINT(Gnamm)
             
   Write(tmp,'(45(g0,","),g0)') RUN, EXCODE, TN, ROTNUM, REPNO, CR, Edap, &
      Edapm, Drdap, Drdapm, Tsdap, Tsdapm, Adap, Adapm, Mdap, Mdapm, cGwam1, &
      cGwamm1, Gwumc, Gwummc, cHnumam1, cHnumamm1, Hnumgm, Hnumgmm, Laix, &
      Laixm, Lnumsm, Lnumsmm, cTnumam1, cTnumamm1, cCwam1, cCwamm1, cVwam1, &
      cVwamm1, Hiamc, Hiammc, Gnpcm, Gnpcmm, Vnpcmc, Vnpcmmc, cCnam1, cCnamm1, &
      cGnam1, cGnamm1, Hinmc, Hinmmc
      
   lngth = Len(Trim(Adjustl(tmp)))
   size = lngth
   Allocate(Character(Len=size)::Csvline)
   Csvline = Trim(Adjustl(tmp))
   pCsvline => Csvline
   Return
end Subroutine CsvOutEvalCsCer
! ----------------------------------------------------------------------------------
! Sub for evaluate.csv output
Subroutine CsvOutEvOpsum(EXCODE, RUNRUNI, CG, TN, ROTNUM, CR, Simulated, Measured, &
   ICOUNT, Csvline, pCsvline, lngth) 
    
!  Input vars
   Character(8),Intent(IN):: EXCODE    
   Integer,Intent(in) :: RUNRUNI, TN, ROTNUM, ICOUNT   
!  Integer, Intent(IN) :: RUN        ! run number        
!  Integer,Intent(in)  :: SN         ! Sequence number,crop rotation  #
!  Integer,Intent(in)  :: ON         ! Option number (sequence runs)  #
!  Integer,Intent(IN)  :: REPNO      ! Number of run repetitions      #
!  Integer,Intent(in)  :: CN         ! Crop component (multicrop)     #
   Character(Len=2),Intent(in) :: CR
   Character(Len=8),Dimension(ICOUNT) :: Measured, Simulated
   Character(Len=2),Intent(in) :: CG
  
   Integer :: i, size
  
   Character(:), allocatable, Target, Intent(Out) :: Csvline
   Character(:), Pointer, Intent(Out) :: pCsvline
   Integer, Intent(Out) :: lngth
  
   Character(Len=1000) :: tmp 
   Character(Len=100) :: tmp1  
   Character (Len=20) :: fmt

!  chp 2023-01-05 If no observed data, don't try to print anything
   if (ICOUNT == 0) RETURN

   Write(tmp1,'(6(g0,","))') RUNRUNI,  EXCODE, CG, TN, ROTNUM, CR      
    
   Write(fmt,'(I3)') 2*ICOUNT-1   
   fmt = '('//Trim(Adjustl(fmt))//'(g0,","),g0)'
   fmt = Trim(Adjustl(fmt))
   
   Write(tmp,fmt) (Simulated(i), Measured(i), i = 1, ICOUNT)
   
   
   tmp = Trim(Adjustl(tmp1)) // Trim(Adjustl(tmp)) 

   lngth = Len(Trim(Adjustl(tmp)))
   size = lngth
   Allocate(Character(Len = size)::Csvline)
   Csvline = Trim(Adjustl(tmp))
   pCsvline => Csvline
   Return
end Subroutine CsvOutEvOpsum
!------------------------------------------------------------------------------
! Sub for csv output for summary.out
! 3/15/2018 CHP changed CRPNO to REPNO
! 06/11/2021 FO Added LATI, LONG, ELEV and HYEAR to Summary.csv
!Subroutine CsvOutSumOpsum(RUN, TRTNUM, ROTNO, ROTOPT, CRPNO, CROP, MODEL, &
Subroutine CsvOutSumOpsum(RUN, TRTNUM, ROTNO, ROTOPT, REPNO, CROP, MODEL, &
   EXNAME, TITLET, FLDNAM, WSTAT, WYEAR, SLNO, LATI, LONG, ELEV, &
   YRSIM, YRPLT, EDAT, ADAT, MDAT, YRDOY, HYEAR, DWAP, &
   !CWAM, HWAM, HWAH, BWAH, PWAM, HWUM, HNUMUM, HIAM, LAIX, HNUMAM, IRNUM, IRCM,&
   CWAM, HWAM, HWAH, BWAH, PWAM, HWUM, HNUMUM, HIAM, LAIX, EYLDH, HNUMAM, &
   FCWAM, FHWAM, HWAHF, FBWAH, FPWAM, IRNUM, IRCM, &
   PRCM, ETCM, EPCM, ESCM, ROCM, DRCM, SWXM, NINUMM, NICM, NFXM, NUCM, NLCM, &
   NIAM, NMINC, CNAM, GNAM, N2OEM, PINUMM, PICM, PUPC, SPAM, KINUMM, KICM, KUPC, SKAM, RECM, &
   ONTAM, ONAM, OPTAM, OPAM, OCTAM, OCAM, CO2EM, CH4EM, DMPPM, DMPEM, DMPTM, DMPIM, YPPM, &
   YPEM, YPTM, YPIM, DPNAM, DPNUM, YPNAM, YPNUM, NDCH, TMAXA, TMINA, SRADA, &
   DAYLA, CO2A, PRCP, ETCP, ESCP, EPCP, CRST, Csvline, pCsvline, lngth) 
      
!  Input vars
   Integer, Intent(IN) :: RUN, TRTNUM, ROTNO, ROTOPT, REPNO, YRSIM, YRPLT  
   Character(Len=2), Intent(IN) :: CROP  
   Character(Len=8), Intent(IN) :: MODEL, FLDNAM, WSTAT, EXNAME
   Character(Len=9), Intent(IN) :: ELEV
   Character(Len=10), Intent(IN) :: SLNO 
   Character(Len=15), Intent(IN) :: LATI,LONG
   Character(Len=25), Intent(IN) :: TITLET 
   Integer, Intent(IN) :: EDAT, ADAT, MDAT, YRDOY, DWAP, CWAM, HWAM, PWAM
   Integer, Intent(IN) :: FCWAM, FHWAM, FPWAM 
   Real, Intent (IN) :: HWAH, BWAH, HWAHF, FBWAH, EYLDH
  
   Real :: HWUM, HNUMUM, HIAM, LAIX, DMPPM, DMPEM, DMPTM, DMPIM, YPPM, YPEM
   Integer :: HNUMAM, IRNUM, IRCM, PRCM, ETCM, EPCM, ESCM, ROCM, DRCM, SWXM 
   Integer :: NINUMM, NICM, NFXM, NUCM, NLCM, NIAM, NMINC, CNAM, GNAM, PINUMM
   Integer :: PICM, PUPC, SPAM, KINUMM, KICM, KUPC, SKAM, RECM, ONTAM 
   Integer :: ONAM, OPTAM, OPAM, OCTAM, OCAM, NDCH, WYEAR, CRST, HYEAR  
   Real :: YPTM, YPIM, DPNAM, DPNUM, YPNAM, YPNUM,  TMAXA, TMINA, SRADA
   Real :: DAYLA, CO2A, PRCP, ETCP, ESCP, EPCP
   Real :: N2OEM, CH4EM, CO2EM
   
   Character(:), allocatable, Target, Intent(Out) :: Csvline
   Character(:), Pointer, Intent(Out) :: pCsvline
   Integer, Intent(Out) :: lngth
   Integer :: size
   Character(Len=1300) :: tmp      
!  End of vars
  
!  Recalculated vars
   Integer :: cHWAH1, cBWAH1, cHWAHF1, cFBWAH1
   Character(Len=25) :: TITLET1   
  
   cHWAH1 = NINT(HWAH)
   cBWAH1 = NINT(BWAH * 10.0)
   cHWAHF1 = NINT(HWAHF)
   cFBWAH1 = NINT(FBWAH)
   TITLET1 = Trim(AdjustL(CommaDash(TITLET)))
           
   Write(tmp,'(1500(g0,","),g0)') RUN, TRTNUM, ROTNO, ROTOPT, REPNO, CROP, MODEL, &
   EXNAME, TITLET1, FLDNAM, WSTAT, WYEAR, SLNO, LATI, LONG, ELEV, &
   YRSIM, YRPLT, EDAT, ADAT, MDAT, YRDOY, HYEAR, DWAP, &
   !CWAM, HWAM, cHWAH1, cBWAH1, PWAM, HWUM, HNUMAM, HNUMUM, HIAM, LAIX, IRNUM, &
   CWAM, HWAM, cHWAH1, cBWAH1, PWAM, HWUM, HNUMAM, HNUMUM, HIAM, LAIX, EYLDH, &
   FCWAM, FHWAM, cHWAHF1, cFBWAH1, FPWAM, IRNUM, &
   IRCM, PRCM, ETCM, EPCM, ESCM, ROCM, DRCM, SWXM, NINUMM, NICM, NFXM, NUCM, &
   NLCM, NIAM, NMINC, CNAM, GNAM, N2OEM, PINUMM, PICM, PUPC, SPAM, KINUMM, KICM, KUPC, SKAM,&
   RECM, ONTAM, ONAM, OPTAM, OPAM, OCTAM, OCAM, CO2EM, CH4EM, DMPPM, DMPEM, DMPTM, DMPIM, &
   YPPM, YPEM, YPTM, YPIM, DPNAM, DPNUM, YPNAM, YPNUM, NDCH, TMAXA, TMINA, &
   SRADA, DAYLA, CO2A, PRCP, ETCP, ESCP, EPCP, CRST
   
   lngth = Len(Trim(Adjustl(tmp)))
   size = lngth
   Allocate(Character(Len = size)::Csvline)
   Csvline = Trim(Adjustl(tmp))
   pCsvline => Csvline
   Return
end Subroutine CsvOutSumOpsum
!---------------------------------------------------------------------------------

! Sub for plantc.csv output Plant C CropGro
Subroutine CsvOutPlCCrGro(EXCODE, RUN, TN, ROTNUM, REPNO, YEAR, DOY, DAS, DAP, &
   TOTWT, PG, CMINEA, GROWTH, GRWRES, MAINR, CADLF, CADST, RHOLP, RHOSP, TGRO, &
   TGROAV, PCNSDP, PCLSDP, PCCSDP, TS, Csvline, pCsvline, lngth) 
    
!  Input vars
   Character(8),Intent(IN):: EXCODE    
   Integer, Intent(IN) :: RUN, TN, ROTNUM, REPNO, YEAR, DOY, DAS, DAP, TS           
!   INTEGER,Intent(in)      :: SN         ! Sequence number,crop rotation  #
!   INTEGER,Intent(in)      :: ON         ! Option number (sequence runs)  #
!   INTEGER,Intent(in)      :: CN         ! Crop component (multicrop)     #
   REAL,Intent(IN) :: TOTWT, PG, CMINEA, GROWTH, GRWRES, MAINR, CADLF, CADST
   REAL,Intent(IN) :: RHOLP, RHOSP, TGROAV, PCNSDP, PCLSDP, PCCSDP  
   REAL,Dimension(TS),Intent(IN)      :: TGRO
    
   Character(:), allocatable, Target, Intent(Out) :: Csvline
   Character(:), Pointer, Intent(Out) :: pCsvline
   Integer, Intent(Out) :: lngth
   Integer :: size
   Character(Len=350) :: tmp      
!  End of vars
  
!  Recalculated vars
   Integer :: TOTWT1 
   Real :: cCADLF1
  
   TOTWT1= NINT(TOTWT * 10.0)
   cCADLF1 = CADLF + CADST  
            
   Write(tmp,'(22(g0,","),g0)') RUN, EXCODE, TN, ROTNUM, REPNO, YEAR, DOY, DAS, &
     DAP, TOTWT1, PG, CMINEA, GROWTH, GRWRES, MAINR, cCADLF1, RHOLP, RHOSP, &
     TGRO(12), TGROAV, PCNSDP, PCLSDP, PCCSDP
   
  lngth = Len(Trim(Adjustl(tmp)))
  size = lngth
  Allocate(Character(Len = size)::Csvline)
  Csvline = Trim(Adjustl(tmp))
  pCsvline => Csvline
  Return
end Subroutine CsvOutPlCCrGro
!---------------------------------------------------------------------------------
! Sub for csv output for SoilOrg.OUT
Subroutine CsvOutSoilOrg1(EXCODE, RUN, TN, ROTNUM,  REPNO, YEAR, DOY, DAS,CumRes,&
   SCDD, SOCD, SomLitC, TSOMC, TLITC, CumResE, SNDD, SOND, SomLitE, TSOME, TLITE,&
   SPDD, SOPD, Csvline, pCsvline, lngth) 
  
   INTEGER, PARAMETER :: &
     NL       = 20, &   !Maximum number of soil layers 
     N = 1,         &   !Nitrogen  
     NELEM    = 3,  &   !Number of elements modeled (currently N & P)
     P = 2              !Phosphorus
  
!   Input vars
    Character(8),Intent(IN):: EXCODE    
    Integer, Intent(IN) :: RUN, TN, ROTNUM, REPNO, YEAR, DOY, DAS           
!  INTEGER,Intent(in)      :: SN         ! Sequence number,crop rotation  #
!  INTEGER,Intent(in)      :: ON         ! Option number (sequence runs)  #
!  INTEGER,Intent(in)      :: CN         ! Crop component (multicrop)     #
!  INTEGER,Intent(IN)      :: DAP        ! Number of days after planting (d) # 
   Real,Intent(IN) :: CumRes, SCDD, SOCD, TSOMC, TLITC, SNDD, SOND, SPDD, SOPD  
   REAL,Dimension(0:NL),Intent(IN) :: SomLitC
   REAL,Dimension(NELEM),Intent(IN) :: CumResE
   REAL,Dimension(0:NL,NELEM),Intent(IN):: SomLitE
   REAL,Dimension(NELEM),Intent(IN)      :: TSOME
   REAL,Dimension(NELEM),Intent(IN)      :: TLITE
    
   Character(:), allocatable, Target, Intent(Out) :: Csvline
   Character(:), Pointer, Intent(Out) :: pCsvline
   Integer, Intent(Out) :: lngth
   Integer :: size
   Character(Len=450) :: tmp      
!  End of vars
  
!  Recalculated vars
   Integer :: CumRes1, SCDD1, SOCD1, SomLitC1, Var1, TSOMC1  
   Real :: Var2, Var3 
  
   CumRes1 = NINT(CumRes) 
   SCDD1 = NINT(SCDD) 
   SOCD1 = NINT(SOCD)
   SomLitC1 = NINT(SomLitC(0))
   Var1 = NINT(TSOMC+TLITC)
   TSOMC1 = NINT(TSOMC)
   Var2 = TSOME(N) + TLITE(N)
   Var3 = TSOME(P) + TLITE(P)
             
   Write(tmp,'(28(g0,","),g0)') RUN, EXCODE, TN, ROTNUM, REPNO, YEAR, DOY, DAS, &
      CumRes1, SCDD1, SOCD1, SomLitC1, Var1, TSOMC1, TLITC, CumResE(N), SNDD, &
      SOND, SomLitE(0,N), Var2, TSOME(N), TLITE(N), CumResE(P), SPDD, SOPD,  &
      SomLitE(0,P), Var3, TSOME(P), TLITE(P)
   
   lngth = Len(Trim(Adjustl(tmp)))
   size = lngth
   Allocate(Character(Len = size)::Csvline)
   Csvline = Trim(Adjustl(tmp))
   pCsvline => Csvline
   Return
end Subroutine CsvOutSoilOrg1
!---------------------------------------------------------------------------------
! Sub for csv output for SoilOrg.OUT
Subroutine CsvOutSoilOrg2(EXCODE, RUN, TN, ROTNUM,  REPNO, YEAR, DOY, DAS, &
   CumRes, SCDD, SOCD, SomLitC, TSOMC, TLITC, CumResE, SNDD, SOND, SomLitE, &
   TSOME, TLITE, Csvline, pCsvline, lngth) 
  
   INTEGER, PARAMETER :: &
      NL       = 20, &   !Maximum number of soil layers 
      N = 1,         &   !Nitrogen  
      NELEM    = 3,  &   !Number of elements modeled (currently N & P)
      P = 2              !Phosphorus
  
!  Input vars
   Character(8),Intent(IN):: EXCODE    
   Integer, Intent(IN) :: RUN, TN, ROTNUM, REPNO, YEAR, DOY, DAS          
!  INTEGER,Intent(in)      :: SN         ! Sequence number,crop rotation  #
!  INTEGER,Intent(in)      :: ON         ! Option number (sequence runs)  #
!  INTEGER,Intent(in)      :: CN         ! Crop component (multicrop)     #
!  INTEGER,Intent(IN)      :: DAP        ! Number of days after planting (d) # 
   Real,Intent(IN) :: CumRes, SCDD, SOCD, TSOMC, TLITC, SNDD, SOND  
   Real,Dimension(0:NL),Intent(IN) :: SomLitC
   Real,Dimension(NELEM),Intent(IN) :: CumResE
   Real,Dimension(0:NL,NELEM),Intent(IN):: SomLitE
   Real,Dimension(NELEM),Intent(IN)      :: TSOME
   Real,Dimension(NELEM),Intent(IN)      :: TLITE
    
   Character(:), allocatable, Target, Intent(Out) :: Csvline
   Character(:), Pointer, Intent(Out) :: pCsvline
   Integer, Intent(Out) :: lngth
   Integer :: size
   Character(Len=350) :: tmp      
!  End of vars
  
!  Recalculated vars
   Integer :: CumRes1, SCDD1, SOCD1, SomLitC1, Var1, TSOMC1  
   Real :: Var2 !, Var3 
  
   CumRes1 = NINT(CumRes) 
   SCDD1 = NINT(SCDD) 
   SOCD1 = NINT(SOCD)
   SomLitC1 = NINT(SomLitC(0))
   Var1 = NINT(TSOMC+TLITC)
   TSOMC1 = NINT(TSOMC)
   Var2 = TSOME(N) + TLITE(N)
             
   Write(tmp,'(21(g0,","),g0)') RUN, EXCODE, TN, ROTNUM, REPNO, YEAR, DOY, &
      DAS, CumRes1, SCDD1, SOCD1, SomLitC1, Var1, TSOMC1, TLITC, CumResE(N), &
      SNDD, SOND, SomLitE(0,N), Var2, TSOME(N), TLITE(N)
   
   lngth = Len(Trim(Adjustl(tmp)))
   size = lngth
   Allocate(Character(Len = size)::Csvline)
   Csvline = Trim(Adjustl(tmp))
   pCsvline => Csvline
   Return
end Subroutine CsvOutSoilOrg2
!---------------------------------------------------------------------------------
! Sub for csv output for ETPhot.OUT
Subroutine CsvOutETPhot(EXCODE, RUN, TN, ROTNUM,  REPNO, YEAR, DOY, DAS, PCINPD, &
   PG, PGNOON, PCINPN, SLWSLN, SLWSHN, PNLSLN, PNLSHN, LMXSLN,LMXSHN,TGRO,TGROAV,&  
   Csvline, pCsvline, lngth) 
  
  ! Input vars
  Character(8),Intent(IN):: EXCODE    
  Integer, Intent(IN) :: RUN, TN, ROTNUM, REPNO, YEAR, DOY, DAS         
!        INTEGER,Intent(in)      :: SN         ! Sequence number,crop rotation  #
!        INTEGER,Intent(in)      :: ON         ! Option number (sequence runs)  #
!        INTEGER,Intent(in)      :: CN         ! Crop component (multicrop)     # 
  Real,Intent(IN) :: PCINPD, PG, PGNOON, PCINPN, SLWSLN, SLWSHN, PNLSLN, PNLSHN
  Real,Intent(IN) :: LMXSLN, LMXSHN, TGROAV
  Real,Dimension(24),Intent(IN) :: TGRO
    
  Character(:), allocatable, Target, Intent(Out) :: Csvline
  Character(:), Pointer, Intent(Out) :: pCsvline
  Integer, Intent(Out) :: lngth
  Integer :: size
  Character(Len=350) :: tmp      
  ! End of vars

  Write(tmp,'(19(g0,","),g0)') RUN, EXCODE, TN, ROTNUM, REPNO, YEAR, DOY, DAS, &
     PCINPD, PG, PGNOON, PCINPN, SLWSLN, SLWSHN, PNLSLN, PNLSHN, LMXSLN, LMXSHN, &
     TGRO(12), TGROAV
   
    lngth = Len(Trim(Adjustl(tmp)))
    size = lngth
    Allocate(Character(Len=size)::Csvline)
    Csvline = Trim(Adjustl(tmp))
   pCsvline => Csvline
    Return
end Subroutine CsvOutETPhot
!---------------------------------------------------------------------------------
! Sub for Mulch.csv
Subroutine CsvOutMulch(EXCODE, RUN, TN, ROTNUM,  REPNO, YEAR, DOY, DAS, &
   MULCHCOVER, MULCHTHICK, MULCHMASS, MULCHWAT, Csvline, pCsvline, lngth) 
  
!  Input vars
   Character(8),Intent(IN):: EXCODE    
   Integer, Intent(IN) :: RUN, TN, ROTNUM, REPNO, YEAR, DOY, DAS           
!    Integer,Intent(in)      :: SN         ! Sequence number,crop rotation  #
!    Integer,Intent(in)      :: ON         ! Option number (sequence runs)  #
!    Integer,Intent(in)      :: CN         ! Crop component (multicrop)     #
!    Integer,Intent(IN)      :: DAP        ! Number of days after planting (d) #
  
   Real,Intent(IN) :: MULCHCOVER, MULCHTHICK, MULCHMASS, MULCHWAT
    
   Character(:), allocatable, Target, Intent(Out) :: Csvline
   Character(:), Pointer, Intent(Out) :: pCsvline
   Integer, Intent(Out) :: lngth
   Integer :: size
   Character(Len=200) :: tmp      
!  End of vars
   
   Integer :: MULCHMASS1
   
   MULCHMASS1 = NINT(MULCHMASS)
   
   Write(tmp,'(11(g0,","),g0)') RUN, EXCODE, TN, ROTNUM, REPNO, YEAR, DOY, &
   DAS, MULCHCOVER, MULCHTHICK, MULCHMASS1, MULCHWAT
     
   lngth = Len(Trim(Adjustl(tmp)))
   size = lngth
   Allocate(Character(Len = size)::Csvline)
   Csvline = Trim(Adjustl(tmp))
   pCsvline => Csvline
   Return
end Subroutine CsvOutMulch
!---------------------------------------------------------------------------------
! Sub for csv output for ETPhot.OUT
Subroutine CsvOutPlantP(EXCODE, RUN, TN, ROTNUM,  REPNO, YEAR, DOY, DAS, DAP, &
   PConc_Shut_opt, PConc_Root_opt, PConc_Shel_opt, PConc_Seed_opt, PConc_Shut_min, & 
   PConc_Root_min, PConc_Shel_min, PConc_Seed_min, PConc_Shut, PConc_Root, &
   PConc_Shel, PConc_Seed, PConc_Plant, PShut_kg, PRoot_kg, PShel_kg, PSeed_kg, &
   PPlant_kg, PS1_AV, PS2_AV, PUptakeProf, PUptake_Cum, CumSenSurfP, CumSenSoilP, & 
   PhFrac1, PhFrac2, Shut_kg, Root_kg, Shel_kg, Seed_kg, PSTRESS_RATIO, N2P, PTotDem, &
   Csvline, pCsvline, lngth) 
     
!  Input vars
  CHARACTER(8),Intent(IN):: EXCODE    ! Experiment code/name           
   Integer, Intent(IN) :: RUN, TN, ROTNUM, REPNO, YEAR, DOY, DAS, DAP         
!        INTEGER,Intent(in)      :: SN         ! Sequence number,crop rotation  #
!        INTEGER,Intent(in)      :: ON         ! Option number (sequence runs)  #
!        INTEGER,Intent(in)      :: CN         ! Crop component (multicrop)     # 
   Real,Intent(IN) :: PConc_Shut_opt, PConc_Root_opt, PConc_Shel_opt, PConc_Seed_opt, &
      PConc_Shut_min, PConc_Root_min, PConc_Shel_min, PConc_Seed_min, PConc_Shut,     & 
      PConc_Root, PConc_Shel, PConc_Seed, PConc_Plant, PShut_kg, PRoot_kg, PShel_kg,  &
      PSeed_kg, PPlant_kg, PS1_AV, PS2_AV, PUptakeProf, PUptake_Cum, CumSenSurfP,     & 
      CumSenSoilP, PhFrac1, PhFrac2, Shut_kg, Root_kg, Shel_kg, Seed_kg, PSTRESS_RATIO,&
      N2P, PTotDem
      
   Character(:), allocatable, Target, Intent(Out) :: Csvline
   Character(:), Pointer, Intent(Out) :: pCsvline
   Integer, Intent(Out) :: lngth
   Character(Len=650) :: tmp 
   Integer :: size     
!  End of vars
   
!  Temps vars
   Real :: PConc_Shut_opt1, PConc_Root_opt1, PConc_Shel_opt1, PConc_Seed_opt1, & 
      PConc_Shut_min1, PConc_Root_min1, PConc_Shel_min1, PConc_Seed_min1,      &
      PConc_Shut1, PConc_Root1, PConc_Shel1, PConc_Seed1, PConc_Plant1
   Integer :: Shut_kg1, Root_kg1, Shel_kg1, Seed_kg1
   
   PConc_Shut_opt1 = PConc_Shut_opt * 100.
   PConc_Root_opt1 = PConc_Root_opt * 100.
   PConc_Shel_opt1 = PConc_Shel_opt * 100.
   PConc_Seed_opt1 = PConc_Seed_opt * 100.
   PConc_Shut_min1 = PConc_Shut_min * 100.
   PConc_Root_min1 = PConc_Root_min * 100.
   PConc_Shel_min1 = PConc_Shel_min * 100.
   PConc_Seed_min1 = PConc_Seed_min * 100.
   PConc_Shut1 = PConc_Shut * 100.
   PConc_Root1 = PConc_Root * 100.
   PConc_Shel1 = PConc_Shel * 100.
   PConc_Seed1 = PConc_Seed * 100.
   PConc_Plant1 = PConc_Plant * 100.
   Shut_kg1 = NINT(Shut_kg)
   Root_kg1 = NINT(Root_kg)
   Shel_kg1 = NINT(Shel_kg)
   Seed_kg1 = NINT(Seed_kg)
   
   Write(tmp,'(41(g0,","),g0)') RUN, EXCODE, TN, ROTNUM, REPNO, YEAR, DOY, DAS, DAP, &
      PConc_Shut_opt1, PConc_Root_opt1, PConc_Shel_opt1, PConc_Seed_opt1, &
      PConc_Shut_min1, PConc_Root_min1, PConc_Shel_min1, PConc_Seed_min1, &
      PConc_Shut1, PConc_Root1, PConc_Shel1, PConc_Seed1, PConc_Plant1, PShut_kg,&
      PRoot_kg, PShel_kg, PSeed_kg, PPlant_kg, PS1_AV, PS2_AV, PUptakeProf, &
      PUptake_Cum, CumSenSurfP, CumSenSoilP, PhFrac1, PhFrac2, Shut_kg1, Root_kg1,&
      Shel_kg1, Seed_kg1, PSTRESS_RATIO, N2P, PTotDem
   
   lngth = Len(Trim(Adjustl(tmp)))
   size = lngth
   Allocate(Character(Len=size)::Csvline)
   Csvline = Trim(Adjustl(tmp))
   pCsvline => Csvline
   Return
end Subroutine CsvOutPlantP
!---------------------------------------------------------------------------------
! Sub for csv output for ETPhot.OUT
Subroutine CsvOutSoilPi(EXCODE, RUN, TN, ROTNUM,  REPNO, YEAR, DOY, DAS, &
   SPiTotProf, SPiAvlProf, SPiSolProf, SPiLabProf, SPiActProf, SPiStaProf, & 
   CumFertP, CMinerP, CImmobP, CumUptakeP, SPi_AVAIL, PUptake, PiLabile, &      
   Csvline, pCsvline, lngth) 
  
!  Input vars
   CHARACTER(8),Intent(IN):: EXCODE    ! Experiment code/name           
   Integer, Intent(IN) :: RUN, TN, ROTNUM, REPNO, YEAR, DOY, DAS         
!    INTEGER,Intent(in)      :: SN         ! Sequence number,crop rotation  #
!    INTEGER,Intent(in)      :: ON         ! Option number (sequence runs)  #
!    INTEGER,Intent(in)      :: CN         ! Crop component (multicrop)     #
!    INTEGER,Intent(IN)      :: DAP        ! Number of days after planting (d) #
  
   REAL,Intent(IN) :: SPiTotProf, SPiAvlProf, SPiSolProf, SPiLabProf, SPiActProf, &
      SPiStaProf, CumFertP, CMinerP, CImmobP, CumUptakeP
  
   Integer, Parameter :: NL = 20
   REAL, DIMENSION(NL) :: SPi_AVAIL, PUptake, PiLabile
    
   Character(:), allocatable, Target, Intent(Out) :: Csvline
   Character(:), Pointer, Intent(Out) :: pCsvline
   Integer, Intent(Out) :: lngth
   Character(Len=500) :: tmp
   Integer :: size      
!  End of vars
                
   Write(tmp,'(32(g0,","),g0)') RUN, EXCODE, TN, ROTNUM, REPNO, YEAR, DOY, &
      DAS, SPiTotProf, SPiAvlProf, SPiSolProf, SPiLabProf, SPiActProf, SPiStaProf, &
      CumFertP, CMinerP, CImmobP, CumUptakeP, SPi_AVAIL(1), SPi_AVAIL(2), &
      SPi_AVAIL(3), SPi_AVAIL(4), SPi_AVAIL(5), PUptake(1), PUptake(2), PUptake(3), & 
      PUptake(4), PUptake(5), PiLabile(1), PiLabile(2), PiLabile(3), PiLabile(4), &
      PiLabile(5)
    
   lngth = Len(Trim(Adjustl(tmp)))
   size = lngth
   Allocate(Character(Len=size)::Csvline)
   Csvline = Trim(Adjustl(tmp))
   pCsvline => Csvline
   Return
end Subroutine CsvOutSoilPi
!---------------------------------------------------------------------------------
! Sub for csv output MLCER PlantGro.csv
Subroutine CsvOut_mlcer(EXCODE, RUN, TN, ROTNUM,  REPNO, YEAR, DOY, DAS, DAP, &
   VSTAGE, RSTAGE, XLAI, WTLF, STMWTO, SDWT, RTWT, PLTPOP, VWAD, TOPWT,SEEDNO,& 
   SDSIZE, HI, PODWT, PODNO, SWF_AV, TUR_AV, NST_AV, EXW_AV, PS1_AV, PS2_AV, &
   KST_AV, PCNL, SHELPC, HIP, PODWTD, SLA, CANHT, CANWH, RTDEP, & 
   MLAG1, TLAG1, MPLAG, TPLAG, MPLA, TPLA, PLA, MLFWT, TLFWT, &
   MSTMWT, TSTMWT, &
   N_LYR, RLV, &
   WTCO, WTLO, WTSO, CUMSENSURF, CUMSENSOIL, DTT, Csvline, pCsvline, lngth) 

!  Input vars
   Character(8),Intent(IN):: EXCODE    
   Integer,Intent(IN) :: RUN, TN, ROTNUM, REPNO, YEAR, DOY, DAS, DAP                     
!        INTEGER,Intent(in)      :: SN      ! Sequence number,crop rotation  #
!        INTEGER,Intent(in)      :: ON      ! Option number (sequence runs)  #
!        INTEGER,Intent(in)      :: CN      ! Crop component (multicrop)     #
   Real,Intent(IN) :: VSTAGE, XLAI, WTLF, STMWTO, SDWT, RTWT, PLTPOP, TOPWT  
   Integer,Intent(IN) :: RSTAGE, VWAD                            
   Real,Intent(IN) :: SEEDNO, SDSIZE, HI, PODWT, PODNO, SWF_AV, TUR_AV, NST_AV     
   Real,Intent(IN) :: EXW_AV, PS1_AV, PS2_AV, KST_AV, PCNL, SHELPC, HIP, PODWTD
   Real,Intent(IN) :: SLA, CANHT, CANWH, RTDEP, WTCO, WTLO, WTSO
   Real,Intent(IN) :: MLAG1, TLAG1, MPLAG, TPLAG, MPLA, TPLA, PLA, MLFWT, TLFWT
   Real,Intent(IN) :: MSTMWT, TSTMWT 
   Integer,Intent(IN) :: N_LYR
   Real, Dimension(N_LYR),Intent(IN) :: RLV 
   Real :: CUMSENSURF,  CUMSENSOIL, DTT
     
!  Recalculated vars
   Integer :: cWTLF1, cSTMWT1, cSDWT1, cRTWT1, cTOPWT1, cSEEDNO1, cPODWT1
   Integer :: cPODNO1, cPODWTD1, cPodSum, cCUMSENSURF1, cCUMSENSOIL1 
   Real :: cRTDEP1, MPLA1, TPLA1, PLA1, AMLWT, ATLWT !cDWNOD1, 
   Integer :: cWTCO1, cWTLO1, cWTSO1
  
   Integer :: i
  
   Character(:), allocatable, Target, Intent(Out) :: Csvline
   Character(:), Pointer, Intent(Out) :: pCsvline
   Integer, Intent(Out) :: lngth
  
   Character(Len=1200) :: tmp
   Character(Len=300) :: tmp1
   Character(Len=20) :: fmt      
!  End of vars
          
!  Recalculation
   cWTLF1 = NINT(WTLF*10.)
   cSTMWT1 = NINT(STMWTO*10.)
   cSDWT1 = NINT(SDWT*10.)
   cRTWT1 = NINT(RTWT*10.*PLTPOP)
   cTOPWT1 = NINT(TOPWT*10.)
   cSEEDNO1 = NINT(SEEDNO)
   cPODWT1 = NINT(PODWT*10.)
   cPODNO1 = NINT(PODNO)
   cPODWTD1 = NINT(PODWTD*10.)
   cPodSum = NINT((PODWTD+PODWT)*10.)
   cRTDEP1 = RTDEP/100.
   MPLA1 = MPLA * 0.0001 * PLTPOP
   TPLA1 = TPLA * 0.0001 * PLTPOP
   PLA1 = PLA * 0.0001 * PLTPOP
   AMLWT = MLFWT * PLTPOP * 10. 
   ATLWT = TLFWT * PLTPOP * 10.
   cWTCO1 = NINT(WTCO*10.)
   cWTLO1 = NINT(WTLO*10.)
   cWTSO1 = NINT(WTSO*10.)
   cCUMSENSURF1 = NINT(CUMSENSURF)  
   cCUMSENSOIL1 = NINT(CUMSENSOIL) 

   Write(tmp,'(58(g0,","))')RUN, EXCODE, TN, ROTNUM, REPNO, YEAR, DOY, DAS,DAP,& 
      VSTAGE, RSTAGE, XLAI, cWTLF1, cSTMWT1, cSDWT1, cRTWT1, VWAD, cTOPWT1, &
      cSEEDNO1, SDSIZE, HI, cPODWT1, cPODNO1, SWF_AV, TUR_AV, NST_AV, EXW_AV, &
      PS1_AV, PS2_AV, KST_AV, PCNL, SHELPC, HIP, cPODWTD1, cPodSum, SLA, &
      CANHT, CANWH, cRTDEP1, & 
      MLAG1, TLAG1, MPLAG, TPLAG, MPLA1, TPLA1, PLA1, MLFWT, TLFWT, MSTMWT, TSTMWT, &
      AMLWT, ATLWT, &     
      cWTCO1, cWTLO1, cWTSO1,cCUMSENSURF1,cCUMSENSOIL1,DTT  
    
   Write(fmt,'(I2)') N_LYR - 1   
   fmt = '('//trim(adjustl(fmt))//'(g0,","),g0)'
   fmt=trim(adjustl(fmt))
   
   Write(tmp1,fmt) (RLV(i), i = 1, N_LYR)
      
   tmp = trim(tmp) // trim(adjustl(tmp1))  
   
   lngth = Len(Trim(tmp))
   Allocate(Character(Len=Len(Trim(tmp)))::Csvline)
   Csvline = Trim(tmp)
   pCsvline => Csvline
   
   return
   end Subroutine CsvOut_mlcer
!------------------------------------------------------------------------------
   
! Sub for csv output PRFRM PlantGro.csv
Subroutine CsvOut_PlGroPrFrm(EXCODE, RUN, TN, ROTNUM,  REPNO, YEAR, DOY, DAS, DAP, &
   VSTAGE, RSTAGE, XLAI, WTLF, STMWT, STRWT, PSRSRFL, PSRLYR1, SDWT, RTWT, TOPWT, & 
   SEEDNO, SDSIZE, HI, PODWT, PODNO, SWFAC, TURFAC, NSTRES, SATFAC, PCNL, SHELPC, &  
   HIP, PODWTD, SLA, CANHT, CANWH, DWNOD, RTDEP, WTCO, WTLO, WTSO, WTSRO, FHWAH, &  
   FHLPH, PELF, DWTCO, DWTLO, DWTSO, fhpctn, N_LYR, RLV, Csvline, pCsvline, lngth) 
     
!  Input vars
   Character(8),Intent(IN):: EXCODE    
   Integer,Intent(IN) :: RUN, TN, ROTNUM, REPNO, YEAR, DOY, DAS, DAP                     
!        INTEGER,Intent(in)      :: SN      ! Sequence number,crop rotation  #
!        INTEGER,Intent(in)      :: ON      ! Option number (sequence runs)  #
!        INTEGER,Intent(in)      :: CN      ! Crop component (multicrop)     #
   Real,Intent(IN) :: VSTAGE, XLAI, WTLF, STMWT, STRWT, PSRSRFL, PSRLYR1, SDWT  
   Real,Intent(IN) :: RTWT, TOPWT, SEEDNO, SDSIZE, HI, PODWT, PODNO, SWFAC
   Real,Intent(IN) :: TURFAC, NSTRES, SATFAC, PCNL, SHELPC, HIP, DWNOD, RTDEP, WTSRO  
   Real,Intent(IN) :: CANHT, CANWH, fhpctn, SLA, PODWTD, WTCO, WTLO, WTSO
   Real,Intent(IN) :: FHWAH, FHLPH, PELF, DWTCO, DWTLO, DWTSO
   Integer,Intent(IN) :: RSTAGE 
   Integer,Intent(IN) :: N_LYR
   Real, Dimension(N_LYR),Intent(IN) :: RLV 
   
!  Recalculated vars
   Integer :: cWTLF1, cSTMWT1, cSTRWT1, cSDWT1, cRTWT1, cTOPWT1, cSEEDNO1, cPODWT1
   Integer :: cPODNO1, cPODWTD1, cPodSum, WTSRO1, FHWAH1, DWTCO1, DWTLO1, DWTSO1, CANHT1
   Integer :: cWTLO1, cWTSO1, cWTCO1 
   Real :: DWNOD1, NSTRES1, RTDEP1, SWFAC1, TURFAC1, fhpctn1
  
   Integer :: i
  
   Character(:), allocatable, Target, Intent(Out) :: Csvline
   Character(:), Pointer, Intent(Out) :: pCsvline
   Integer, Intent(Out) :: lngth
  
   Character(Len=1200) :: tmp
   Character(Len=300) :: tmp1
   Character(Len=20) :: fmt      
!  End of vars
          
!  Recalculation
   cWTLF1 = NINT(WTLF*10.)
   cSTMWT1 = NINT(STMWT*10.)
   cSTRWT1 = NINT(STRWT*10.)
   cSDWT1 = NINT(SDWT*10.)
   cRTWT1 = NINT(RTWT*10.)
   cTOPWT1 = NINT(TOPWT*10.)
   cSEEDNO1 = NINT(SEEDNO)
   cPODWT1 = NINT(PODWT*10.)
   cPODNO1 = NINT(PODNO)
   SWFAC1 = 1. - SWFAC
   TURFAC1 = 1. - TURFAC
   NSTRES1 = 1. - NSTRES
   cPODWTD1 = NINT(PODWTD*10.)
   cPodSum = NINT((PODWTD+PODWT)*10.)
   DWNOD1 = DWNOD * 10. 
   RTDEP1 = RTDEP / 100.
   cWTCO1 = NINT(WTCO*10.)
   cWTLO1 = NINT(WTLO*10.)
   cWTSO1 = NINT(WTSO*10.)
   WTSRO1 = NINT(WTSRO*10.)
   FHWAH1 = NINT(FHWAH*10.)
   DWTCO1 = NINT(DWTCO*10.)
   DWTLO1 = NINT(DWTLO*10.)
   DWTSO1 = NINT(DWTSO*10.)
   CANHT1 = NINT(CANHT*100.)
   fhpctn1 = fhpctn * 6.25
   
   Write(tmp,'(52(g0,","))')RUN, EXCODE, TN, ROTNUM, REPNO, YEAR, DOY, DAS, DAP,& 
      VSTAGE, RSTAGE, XLAI, cWTLF1, cSTMWT1, cSTRWT1, PSRSRFL, PSRLYR1, cSDWT1, &
      cRTWT1, cTOPWT1, cSEEDNO1, SDSIZE, HI, cPODWT1, cPODNO1, SWFAC1, TURFAC1, & 
      NSTRES1, SATFAC, PCNL, SHELPC, HIP, cPODWTD1, cPodSum, SLA, CANHT, &
      CANWH, DWNOD1, RTDEP1, cWTCO1, cWTLO1, cWTSO1, WTSRO1, FHWAH1, FHLPH, PELF, &
      DWTCO1, DWTLO1, DWTSO1, CANHT1, fhpctn1  
      
   Write(fmt,'(I2)') N_LYR - 1   
   fmt = '('//trim(adjustl(fmt))//'(g0,","),g0)'
   fmt=trim(adjustl(fmt))
   
   Write(tmp1,fmt) (RLV(i), i = 1, N_LYR)
      
   tmp = trim(tmp) // trim(adjustl(tmp1))  
   
   lngth = Len(Trim(tmp))
   Allocate(Character(Len=Len(Trim(tmp)))::Csvline)
   Csvline = Trim(tmp)
   pCsvline => Csvline
   
   return
   end Subroutine CsvOut_PlGroPrFrm
!------------------------------------------------------------------------------
! Sub for plantn.csv output for PRFRM
Subroutine CsvOutPlNPrFrm(EXCODE, RUN, TN, ROTNUM, REPNO, YEAR, DOY, DAS, DAP,&
   WTNCAN, WTNSD, WTNVEG, PCNSD, PCNVEG, WTNFX, WTNUP, WTNLF, WTNST, WTNSR, &
   PCNL, PCNST, PCNSR, PCNSH, PCNRT, NFIXN, &
   Csvline, pCsvline, lngth) 
    
!  Input vars
   Character(8),Intent(IN):: EXCODE    
   Integer, Intent(IN) :: RUN, TN, ROTNUM, REPNO, YEAR, DOY, DAS, DAP           
!   INTEGER,Intent(in)      :: SN         ! Sequence number,crop rotation  #
!   INTEGER,Intent(in)      :: ON         ! Option number (sequence runs)  #
!   INTEGER,Intent(in)      :: CN         ! Crop component (multicrop)     #
   Real,Intent(IN) :: WTNCAN, WTNSD, WTNVEG, PCNSD, PCNVEG, WTNFX, WTNUP
   REAL,Intent(IN) :: WTNLF, WTNST, WTNSR, PCNL, PCNST, PCNSR, PCNSH, PCNRT 
   REAL,Intent(IN) :: NFIXN 
  
   Character(:), allocatable, Target, Intent(Out) :: Csvline
   Character(:), Pointer, Intent(Out) :: pCsvline
   Integer, Intent(Out) :: lngth
   Integer :: size
   Character(Len=400) :: tmp      
!  End of vars
  
!  Recalculated vars
   Real :: cWTNCAN1, cWTNSD1, cWTNVEG1, cWTNFX1, cWTNUP1, cWTNLF1, cWTNST1  
   Real :: cWTNSR1, cNFIXN1
  
   cWTNCAN1 = WTNCAN * 10.0
   cWTNSD1  = WTNSD  * 10.0
   cWTNVEG1 = WTNVEG * 10.0
   cWTNFX1  = WTNFX  * 10.0
   cWTNUP1  = WTNUP  * 10.0
   cWTNLF1  = WTNLF  * 10.0
   cWTNST1  = WTNST  * 10.0
   cWTNSR1  = WTNSR  * 10.0
   cNFIXN1  = NFIXN  * 10.0
  
!  Unofmatted   
   Write(tmp,'(24(g0,","),g0)') RUN, EXCODE, TN, ROTNUM, REPNO, YEAR, DOY, DAS, &
      DAP, cWTNCAN1, cWTNSD1, cWTNVEG1, PCNSD, PCNVEG, cWTNFX1, cWTNUP1, cWTNLF1,&
      cWTNST1, cWTNSR1, PCNL, PCNST, PCNSR, PCNSH, PCNRT, cNFIXN1
   
   lngth = Len(Trim(Adjustl(tmp)))
   size = lngth
   Allocate(Character(Len = size)::Csvline)
   Csvline = Trim(Adjustl(tmp))
   pCsvline => Csvline
   Return
   end Subroutine CsvOutPlNPrFrm
!---------------------------------------------------------------------------------
! Sub for plantc.csv output Plant C PRFRM
Subroutine CsvOutPlCPrFrm(EXCODE, RUN, TN, ROTNUM, REPNO, YEAR, DOY, DAS, DAP, &
   TOTWT, PG, CMINEA, GROWTH, GRWRES, MAINR, CADLF, CADST, CADSR, RHOL, RHOS, &
   RHOSR, RHOR, TGRO, TGROAV, PCNSD, PCLSD, PCCSD, TS, Csvline, pCsvline, lngth) 
    
!  Input vars
   Character(8),Intent(IN):: EXCODE    
   Integer, Intent(IN) :: RUN, TN, ROTNUM, REPNO, YEAR, DOY, DAS, DAP, TS           
!   INTEGER,Intent(in)      :: SN         ! Sequence number,crop rotation  #
!   INTEGER,Intent(in)      :: ON         ! Option number (sequence runs)  #
!   INTEGER,Intent(in)      :: CN         ! Crop component (multicrop)     #
   REAL,Intent(IN) :: TOTWT, PG, CMINEA, GROWTH, GRWRES, MAINR, CADLF, CADST
   REAL,Intent(IN) :: CADSR, RHOL, RHOS, RHOSR, RHOR, TGROAV, PCNSD, PCLSD, PCCSD  
   REAL,Dimension(TS),Intent(IN)      :: TGRO
    
   Character(:), allocatable, Target, Intent(Out) :: Csvline
   Character(:), Pointer, Intent(Out) :: pCsvline
   Integer, Intent(Out) :: lngth
   Integer :: size
   Character(Len=400) :: tmp      
!  End of vars
  
!  Recalculated vars
   Integer :: TOTWT1 
   Real :: cCADLF1, RHOL1, RHOS1, RHOSR1, RHOR1
  
   TOTWT1= NINT(TOTWT * 10.0)
   cCADLF1 = CADLF + CADST
   RHOL1 = RHOL * 100.
   RHOS1 = RHOS * 100.
   RHOSR1 = RHOSR * 100.
   RHOR1 = RHOR * 100.
            
   Write(tmp,'(25(g0,","),g0)') RUN, EXCODE, TN, ROTNUM, REPNO, YEAR, DOY, DAS, &
     DAP, TOTWT1, PG, CMINEA, GROWTH, GRWRES, MAINR, cCADLF1, CADSR, RHOL1, RHOS1, &
     RHOSR1, RHOR1, TGRO(12), TGROAV, PCNSD, PCLSD, PCCSD
   
  lngth = Len(Trim(Adjustl(tmp)))
  size = lngth
  Allocate(Character(Len = size)::Csvline)
  Csvline = Trim(Adjustl(tmp))
  pCsvline => Csvline
  Return
   end Subroutine CsvOutPlCPrFrm
!---------------------------------------------------------------------------------
! Sub for plantc.csv output Plant C PRFRM
Subroutine CsvOutDormPrFrm(EXCODE, RUN, TN, ROTNUM, REPNO, YEAR, DOY, DAS, DAP, &
   DRMST, PPGFAC, PPMFAC, PPTFAC, SRFTEMP, ST, FREEZ2, FRLF, FRSTM, FRSTR, FRRT, &
   TS, Csvline, pCsvline, lngth) 
    
!  Input vars
   Character(8),Intent(IN):: EXCODE    
   Integer, Intent(IN) :: RUN, TN, ROTNUM, REPNO, YEAR, DOY, DAS, DAP, TS           
!   INTEGER,Intent(in)      :: SN         ! Sequence number,crop rotation  #
!   INTEGER,Intent(in)      :: ON         ! Option number (sequence runs)  #
!   INTEGER,Intent(in)      :: CN         ! Crop component (multicrop)     #
   Character(6),Intent(IN):: DRMST
   REAL,Intent(IN) :: PPGFAC, PPMFAC, PPTFAC, SRFTEMP, FREEZ2, FRLF, FRSTM 
   REAL,Intent(IN) :: FRSTR, FRRT  
   REAL,Dimension(TS),Intent(IN)      :: ST
    
   Character(:), allocatable, Target, Intent(Out) :: Csvline
   Character(:), Pointer, Intent(Out) :: pCsvline
   Integer, Intent(Out) :: lngth
   Integer :: size
   Character(Len=400) :: tmp      
!  End of vars
            
   Write(tmp,'(19(g0,","),g0)') RUN, EXCODE, TN, ROTNUM, REPNO, YEAR, DOY, DAS, &
     DAP, DRMST, PPGFAC, PPMFAC, PPTFAC, SRFTEMP, ST(1), FREEZ2, FRLF, FRSTM, FRSTR, FRRT
   
  lngth = Len(Trim(Adjustl(tmp)))
  size = lngth
  Allocate(Character(Len = size)::Csvline)
  Csvline = Trim(Adjustl(tmp))
  pCsvline => Csvline
  Return
   end Subroutine CsvOutDormPrFrm
!---------------------------------------------------------------------------------
! Sub for storage.csv output PRFRM
Subroutine CsvOutStorPrFrm(EXCODE, RUN, TN, ROTNUM, REPNO, YEAR, DOY, DAS, DAP, &
   AGRSTR, CADSR, CMOBSR, CPFSTR, CRUSSR, CSRFRZ, CSRW, CSTRM, DSTOR, FNINSR, & 
   FNINSRG, FRSTR, FRSTRM, NADSR, NGRSR, NGRSRG, NMOBSR, NRUSSR, NSRALL, NSRDOT,&
   NSROFF, NVSTSR, PCNSR, PCSTRD, PROSRT, PSRSRFD, PSRLYRD, PSRSRFL, PSRLYR1, &
   RHOSR, SRDAM, SRSRFD, SRLYRD, SSRDOT, SSRNDOT, STRWT, TPSRSRFL, TPSRLYR1, &
   WCRSR, WNRSR, WRCSRDT, WSRDOT, WSRDOTN, WSRFDOT, WSRI, WSRIDOT, WTNSR, WTNSRA,&
   WTNSRO, WTSRO, XSTR, Csvline, pCsvline, lngth) 
    
!  Input vars
   Character(8),Intent(IN):: EXCODE    
   Integer, Intent(IN) :: RUN, TN, ROTNUM, REPNO, YEAR, DOY, DAS, DAP           
!   INTEGER,Intent(in)      :: SN         ! Sequence number,crop rotation  #
!   INTEGER,Intent(in)      :: ON         ! Option number (sequence runs)  #
!   INTEGER,Intent(in)      :: CN         ! Crop component (multicrop)     #
   REAL,Intent(IN) :: AGRSTR, CADSR, CMOBSR, CPFSTR, CRUSSR, CSRFRZ, CSRW, CSTRM, &       
      DSTOR, FNINSR, FNINSRG, FRSTR, FRSTRM, NADSR, NGRSR, NGRSRG, NMOBSR, NRUSSR, &
      NSRALL, NSRDOT, NSROFF, NVSTSR, PCNSR, PCSTRD, PROSRT, PSRSRFD, PSRLYRD, &
      PSRSRFL, PSRLYR1, RHOSR, SRDAM, SRSRFD, SRLYRD, SSRDOT, SSRNDOT, STRWT, &
      TPSRSRFL, TPSRLYR1, WCRSR, WNRSR, WRCSRDT, WSRDOT, WSRDOTN, WSRFDOT, WSRI, &
      WSRIDOT, WTNSR, WTNSRA, WTNSRO, WTSRO, XSTR
       
   Character(:), allocatable, Target, Intent(Out) :: Csvline
   Character(:), Pointer, Intent(Out) :: pCsvline
   Integer, Intent(Out) :: lngth
   Integer :: size
   Character(Len=1000) :: tmp      
!  End of vars
  
!  Recalculated vars
   Integer :: CSRW1, STRWT1, WTNSRA1, WTSRO1 
  
   CSRW1= NINT(CSRW)
   STRWT1 = NINT(STRWT * 10.)
   WTNSRA1 = NINT(WTNSRA * 10.)
   WTSRO1 = NINT(WTSRO * 10.)
   
   Write(tmp,'(59(g0,","),g0)') RUN, EXCODE, TN, ROTNUM, REPNO, YEAR, DOY, DAS, &
     DAP, AGRSTR, CADSR, CMOBSR, CPFSTR, CRUSSR, CSRFRZ, CSRW1, CSTRM, DSTOR, &
     FNINSR, FNINSRG, FRSTR, FRSTRM, NADSR, NGRSR, NGRSRG, NMOBSR, NRUSSR, &
     NSRALL, NSRDOT, NSROFF, NVSTSR, PCNSR, PCSTRD, PROSRT, PSRSRFD, PSRLYRD, &
     PSRSRFL, PSRLYR1, RHOSR, SRDAM, SRSRFD, SRLYRD, SSRDOT, SSRNDOT, STRWT1, &
     TPSRSRFL, TPSRLYR1, WCRSR, WNRSR, WRCSRDT, WSRDOT, WSRDOTN, WSRFDOT, WSRI, &
     WSRIDOT, WTNSR, WTNSRA1, WTNSRO, WTSRO1, XSTR   
   
   
  lngth = Len(Trim(Adjustl(tmp)))
  size = lngth
  Allocate(Character(Len = size)::Csvline)
  Csvline = Trim(Adjustl(tmp))
  pCsvline => Csvline
  Return
   end Subroutine CsvOutStorPrFrm
!---------------------------------------------------------------------------------
! Sub for somlitn.csv output Parton Soil Organic Matter output
Subroutine CsvOutSomN(EXCODE, RUN, TRN, ROTNUM, REPNO, YEAR, DOY, DAS, &
   SON_20CM, SON_20CM_P, SON_40CM, SON_40CM_P, TNTD, TN0D, TNSD, TN, SOM1E, &
   TSOM1E, S1N, TSOM2E, S2N, TSOM3E, S3N, LITE, TLITE, LIN, METABE, TMETABE,  &
   MEN, STRUCE, TSTRUCE, STN, CUMRESE, NLR, N, NELEM, Csvline, pCsvline, lngth) 
    
!  Input vars
   Character(8),Intent(IN):: EXCODE    
   Integer, Intent(IN) :: RUN, TRN, ROTNUM, REPNO, YEAR, DOY, DAS, NLR, NELEM, N           
!   INTEGER,Intent(in)      :: SN         ! Sequence number,crop rotation  #
!   INTEGER,Intent(in)      :: ON         ! Option number (sequence runs)  #
!   INTEGER,Intent(in)      :: CN         ! Crop component (multicrop)     #
   REAL,Intent(IN) :: SON_20CM, SON_20CM_P, SON_40CM, SON_40CM_P, TNTD, TN0D, TNSD
   
   REAL,Dimension(NLR),Intent(IN) :: TN, S1N, S2N, S3N, LIN, MEN, STN 
   REAL,Intent(IN) :: SOM1E(0:NLR,3), LITE(0:NLR,3), METABE(0:NLR,3), STRUCE(0:NLR,3) 
   REAL,Dimension(NELEM),Intent(IN) :: TSOM1E, TSOM2E, TSOM3E, TMETABE, TSTRUCE, &
       TLITE, CUMRESE
   
   Character(:), allocatable, Target, Intent(Out) :: Csvline
   Character(:), Pointer, Intent(Out) :: pCsvline
   Integer, Intent(Out) :: lngth
   Integer :: size
   Character(Len=1000) :: tmp      
!  End of vars
  
!  Recalculated vars
   Integer :: k, SON_20CM1, SON_40CM1, TNTD1, TN0D1, TNSD1
   Integer, Dimension(NLR) :: TN1, S1N1, S2N1, S3N1
   Integer :: SOM1E1(0:NLR,3)
   Integer,Dimension(NELEM) :: TSOM1E1, TSOM2E1, TSOM3E1, CUMRESE1 
   
   SON_20CM1 = NINT(SON_20CM)
   SON_40CM1 = NINT(SON_40CM)
   TNTD1 = NINT(TNTD) 
   TN0D1 = NINT(TN0D)
   TNSD1 = NINT(TNSD)
   TN1 = NINT(TN)
   SOM1E1 = NINT(SOM1E)
   TSOM1E1 = NINT(TSOM1E)
   S1N1 = NINT(S1N)
   TSOM2E1 = NINT(TSOM2E)
   S2N1 = NINT(S2N)
   TSOM3E1 = NINT(TSOM3E)
   S3N1 = NINT(S3N)
   CUMRESE1 = NINT(CUMRESE)
   
   Write(tmp,'(60(g0,","),g0)') RUN, EXCODE, TRN, ROTNUM, REPNO, YEAR, DOY, DAS, &
     SON_20CM1, SON_20CM_P, SON_40CM1, SON_40CM_P, TNTD1, TN0D1, TNSD1, &
     ((TN1(k)), k=1, 5), SOM1E1(0,N), TSOM1E1(N), ((S1N1(k)), k=1, 5), TSOM2E1(N), & 
     ((S2N1(k)), k=1, 5), TSOM3E1(N), ((S3N1(k)), k=1, 5), LITE(0,N), TLITE(N), &
     ((LIN(k)), k=1, 5), METABE(0,N), TMETABE(N), ((MEN(k)), k=1, 5), STRUCE(0,N), &
     TSTRUCE(N), ((STN(k)), k=1, 5), CUMRESE1(N)
   
  lngth = Len(Trim(Adjustl(tmp)))
  size = lngth
  Allocate(Character(Len = size)::Csvline)
  Csvline = Trim(Adjustl(tmp))
  pCsvline => Csvline
  Return
end Subroutine CsvOutSomN
!------------------------------------------------------------------------------
! Sub for N2O.out
Subroutine CsvOutN2O(EXCODE, RUN, TN, ROTNUM, REPNO, YEAR, DOY, DAS, &
   CN2O_emitted, CN2_emitted, CNO_emitted, &
   CNOX, CNITRIFY, CN2Odenit, CN2Onitrif, CN2, CNOflux, &
   N2O_emitted, N2_emitted, NO_emitted, & 
   TNOXD, TNITRIFY, TN2OdenitD, &
   TN2OnitrifD, TN2D, TNOfluxD, &
   DENITRIF, NITRIF, &
   N2Oflux, N2flux, &
   NOflux, N_LYR, &
   Csvline, pCsvline, lngth) 
    
!  Input vars
   Character(8),Intent(IN):: EXCODE    
   Integer,Intent(in) :: RUN, TN, ROTNUM, REPNO, YEAR, DOY, DAS 
   INTEGER N_LYR
!          Cumul      Daily     Layer         
   REAL CNOX,      TNOXD,    DENITRIF(N_LYR)  !Denitrification
   REAL CNITRIFY,            NITRIF(N_LYR)
   INTEGER         TNITRIFY
   REAL CN2,       TN2D,     N2flux(N_LYR)    !N2
   REAL                      N2Oflux(N_LYR)   
   REAL CNOflux,   TNOfluxD, NOflux(N_LYR)    !NO total flux
   REAL CN2Odenit, TN2OdenitD !, N2Odenit(N_LYR)   
   REAL CN2Onitrif, TN2OnitrifD !, N2ONitrif(N_LYR)     
   REAL N2O_emitted, N2_emitted, CN2O_emitted, CN2_emitted  
   REAL NO_emitted, CNO_emitted     
  
!  Csv control vars  
   Character(:), allocatable, Target, Intent(Out) :: Csvline
   Character(:), Pointer, Intent(Out) :: pCsvline
   Integer, Intent(Out) :: lngth
   Character(Len=1000) :: tmp
   Character(Len=750) :: tmp1, tmp2
   Character(Len=20) :: fmt
  
   Integer :: i, size      
!  End of vars
            
   WRITE(tmp1,'(26(g0,","))') RUN,EXCODE,TN,ROTNUM,REPNO,YEAR,DOY,DAS, &
      CN2O_emitted, CN2_emitted, CNO_emitted, &
      CNOX, CNITRIFY, CN2Odenit, CN2Onitrif, CN2, CNOflux, &
      N2O_emitted, N2_emitted, NO_emitted, & 
      TNOXD, TNITRIFY, TN2OdenitD, &
      TN2OnitrifD, TN2D, TNOfluxD
      
    IF (N_LYR < 10) THEN
      WRITE(fmt,'(I2)') 5 * N_LYR - 1
      fmt = '('//trim(adjustl(fmt))//'(g0,","),g0)'
      WRITE (tmp2,fmt)                  &
           (DENITRIF(i), i = 1, N_LYR), &
           (NITRIF(i),   i = 1, N_LYR), &
           (N2Oflux(i),  i = 1, N_LYR), &
           (N2flux(i),   i = 1, N_LYR), &
           (NOflux(i),   i = 1, N_LYR) 
    ELSE
      WRITE (tmp2,'(49(g0,","),g0)')    &                
           (DENITRIF(i), i = 1, 10),    &
           (NITRIF(i),   i = 1, 10),    &
           (N2Oflux(i),  i = 1, 10),    &
           (N2flux(i),   i = 1, 10),    &
           (NOflux(i),   i = 1, 10)
    ENDIF
      
   tmp = Trim(Adjustl(tmp1)) // Trim(Adjustl(tmp2))
   
   lngth = Len(Trim(Adjustl(tmp)))
   size = lngth
   Allocate(Character(Len = size)::Csvline)
   Csvline = Trim(Adjustl(tmp))
   pCsvline => Csvline
   Return
end Subroutine CsvOutN2O
!---------------------------------------------------------------------------------
! Sub for somlitC.csv output Parton Soil Organic Matter output
! 09/02/2022 FO - Updated array size REAL due to array bound mismatch
Subroutine CsvOutSomC(EXCODE, RUN, TRN, ROTNUM, REPNO, YEAR, DOY, DAS,     &
      SOC_20CM, SOC_20CM_P, SOC_40CM, SOC_40CM_P,                          &
      SLC_20CM, SLC_20CM_P, SLC_40CM, SLC_40CM_P,                          &
      TCTD, TC0D, TCSD, TC,                                                &
      SOM1C, TSOM1C, S1C, TSOM2C, S2C, TSOM3C, S3C, LITC, TLITC, LIT,      &
      METABC, TMETABC, MET, STRUCC, TSTRUCC, STR, CUMRESC,                 &
      ACCCO2, NLR, Csvline, pCsvline, lngth)

!  Input vars
   Character(8),Intent(IN):: EXCODE    
   Integer, Intent(IN) :: RUN, TRN, ROTNUM, REPNO, YEAR, DOY, DAS, NLR
!   INTEGER,Intent(in)      :: SN         ! Sequence number,crop rotation  #
!   INTEGER,Intent(in)      :: ON         ! Option number (sequence runs)  #
!   INTEGER,Intent(in)      :: CN         ! Crop component (multicrop)     #
   REAL,Intent(IN) :: SOC_20CM, SOC_20CM_P, SOC_40CM, SOC_40CM_P
   REAL,Intent(IN) :: SLC_20CM, SLC_20CM_P, SLC_40CM, SLC_40CM_P
   REAL,Intent(IN) :: TCTD, TC0D, TCSD
   
   REAL,Dimension(5),Intent(IN) :: TC, S1C, S2C, S3C, LIT, MET, STR 
   REAL,Intent(IN) :: SOM1C(0:NLR), LITC(0:NLR), METABC(0:NLR), STRUCC(0:NLR) 
   REAL,Intent(IN) :: TSOM1C, TSOM2C, TSOM3C, TMETABC, TSTRUCC, &
       TLITC, CUMRESC, ACCCO2(0:1)
   
   Character(:), allocatable, Target, Intent(Out) :: Csvline
   Character(:), Pointer, Intent(Out) :: pCsvline
   Integer, Intent(Out) :: lngth
   Integer :: size
   Character(Len=1000) :: tmp      
!  End of vars
  
!  Recalculated vars
   Integer :: k, SOC_20CM1, SOC_40CM1, SLC_20CM1, SLC_40CM1, TCTD1, TC0D1, TCSD1
   Integer, Dimension(5) :: TC1, S1C1, S2C1, S3C1, LIT1, MET1, STR1
   Integer :: SOM1C1(0:NLR), LITC1(0:NLR), METABC1(0:NLR), STRUCC1(0:NLR) 
   Integer :: TSOM1C1, TSOM2C1, TSOM3C1, CUMRESC1 
   Integer :: TMETABC1, TSTRUCC1, TLITC1, ACCCO21(0:1)

   SOC_20CM1 = NINT(SOC_20CM)
   SOC_40CM1 = NINT(SOC_40CM)
   SLC_20CM1 = NINT(SLC_20CM)
   SLC_40CM1 = NINT(SLC_40CM)
   TCTD1 = NINT(TCTD) 
   TC0D1 = NINT(TC0D)
   TCSD1 = NINT(TCSD)
   TC1 = NINT(TC)
   SOM1C1 = NINT(SOM1C)
   TSOM1C1 = NINT(TSOM1C)
   S1C1 = NINT(S1C)
   TSOM2C1 = NINT(TSOM2C)
   S2C1 = NINT(S2C)
   TSOM3C1 = NINT(TSOM3C)
   S3C1 = NINT(S3C)

   LITC1    = NINT(LITC)
   TLITC1   = NINT(TLITC)
   LIT1     = NINT(LIT)
   METABC1  = NINT(METABC)
   TMETABC1 = NINT(TMETABC)
   MET1     = NINT(MET)    
   STRUCC1  = NINT(STRUCC) 
   TSTRUCC1 = NINT(TSTRUCC)
   STR1     = NINT(STR)    
   ACCCO21  = NINT(ACCCO2) 
   CUMRESC1 = NINT(CUMRESC)

   Write(tmp,'(67(g0,","),g0)') RUN, EXCODE, TRN, ROTNUM, REPNO, YEAR, DOY, DAS, &
     SOC_20CM1, SOC_20CM_P, SOC_40CM1, SOC_40CM_P,                               &
     SLC_20CM1, SLC_20CM_P, SLC_40CM1, SLC_40CM_P,                               &
     TCTD1, TC0D1, TCSD1, &
     ((TC1(k)), k=1, 5), SOM1C1(0), TSOM1C1, ((S1C1(k)), k=1, 5),                & 
     TSOM2C1, ((S2C1(k)), k=1, 5), TSOM3C1, ((S3C1(k)), k=1, 5), LITC1(0), TLITC1, &
     ((LIT1(k)), k=1, 5), METABC1(0), TMETABC1, ((MET(k)), k=1, 5), STRUCC1(0),  &
     TSTRUCC, ((STR(k)), k=1, 5), CUMRESC1, ACCCO21(0), ACCCO21(1)
   
  lngth = Len(Trim(Adjustl(tmp)))
  size = lngth
  Allocate(Character(Len = size)::Csvline)
  Csvline = Trim(Adjustl(tmp))
  pCsvline => Csvline
  Return
end Subroutine CsvOutSomC
!---------------------------------------------------------------------------------
Subroutine CsvOutputs(CropModel, numelem, nlayers)

    Character(Len=5) :: CropModel
    Integer :: numelem, nlayers 

! CSV outputs corresponding to *.OUT
         Select case (CropModel)
             Case('CRGRO')
                 Call ListtofilePlantgrCrGro(nlayers) ! plantgro.csv
                 Call ListtofilePlNCrGro              ! plantn.csv
                 Call ListtofilePlCCrGro              ! plantc.csv
                 Call ListtofileEvOpsum               ! evaluate.csv            
             Case('CSCER')
                 Call ListtofilePlantGrCsCer          ! plantgro.csv
                 Call ListtofilePlNCsCer              ! plantn.csv
                 Call ListtofilePlGr2                 ! plantgr2.csv
                 Call ListtofilePlGrf                 ! plantgrf.csv
                 Call ListtofileEvalCsCer             ! evaluate.csv  
             Case('MZCER','SGCER')
                 Call ListtofileMZCER(nlayers)        ! plantgro.csv
                 Call ListtofilePlNMzCer              ! plantn.csv
                 Call ListtofileEvOpsum               ! evaluate.csv
             Case('RICER')
                 Call ListtofileRICER(nlayers)        ! plantgro.csv
!                Call ListtofilePlNMzCer              ! plantn.csv
!                Call ListtofileEvOpsum               ! evaluate.csv
             Case('MLCER')
                 Call ListtofileMLCER(nlayers)        ! plantgro.csv
                 Call ListtofilePlNMzCer              ! plantn.csv
                 Call ListtofileEvOpsum               ! evaluate.csv
             Case('PRFRM')
                 Call ListtofilePlGroPrFrm(nlayers)   ! plantgro.csv
                 Call ListtofilePlNPrFrm              ! plantn.csv
                 Call ListtofilePlCPrFrm              ! plantc.csv
                 Call ListtofileDormPrFrm             ! dormancy.csv
                 Call ListtofileStorPrFrm             ! storage.csv
             Case('SUOIL')
                 Call ListtofileSUOIL(nlayers)        ! plantgro.csv
                 Call ListtofilePlNSUOIL              ! plantn.csv
                 Call ListtofileEvOpsum               ! evaluate.csv 
         End Select

         Call ListtofileSW(nlayers)         ! SoilWat.csv
         Call ListtofileTemp(nlayers)       ! SoilTemp.csv
         Call ListtofileET(nlayers)         ! et.csv
         Call ListtoFileSoilNi()            ! SoilNi.csv
         call ListtoFileWth                 ! weather.csv
         Call ListtofileSumOpsum            ! summary.csv
         Call ListtofileSoilOrg(numelem)    ! SoilOrg.csv
         Call ListtofileETPhot              ! ETPhot.csv
         Call ListtofileMulch               ! Mulch.csv
         Call ListtofilePlantP              ! PlantP.csv
         Call ListtofileSoilPi              ! SoilPi.csv
         Call ListtofileSomN                ! somlitn.csv
         Call ListtofileSomC                ! somlitc.csv
         Call ListtofileN2O(nlayers)        ! N2O.csv
         
         Return
End Subroutine CsvOutputs
!------------------------------------------------------------------------------
! In varibale length string replace comma(,) to dash(-)
   Function CommaDash(string)
      Character(Len=25) :: CommaDash
      Character(Len=25), Intent(IN) :: string 
      Character(len=Len(string)) :: temp
      Integer :: i

      temp = string

      Do i=1, Len(string)

         If (temp(i:i)==',') temp(i:i) = '-'

      End DO
    
      CommaDash = temp
      Return         
   End Function CommaDash
!------------------------------------------------------------------------------

End Module CsvOutput

!------------------------------------------------------------------------------
