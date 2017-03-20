Module CsvGeneric
!
!
Implicit None

! Generic function AnyToStr
! gets Integer, Real(with format) or String as arguments
! and converts into string
Interface AnyToStr
   Module Procedure itoa, &     
                    rtoa, &
                    atoa
End Interface AnyToStr

Contains
!------------------------------------------------------------------------------  
    ! Interface procedures start
    ! Integer to String
    Function itoa(i) result(res)
       Implicit None
       Character(:),Allocatable :: res
       Integer,Intent(in) :: i
       Character(range(i)+2) :: tmp
        
       Write(tmp,'(i0)') i
       res = trim(tmp)
    End Function

    ! Real to String using format 'F0.n'
    ! where n is number of decimals
    Function rtoa(r,fmt) result(res)
       Implicit None
       Character(:),Allocatable :: res
       Real,Intent(in) :: r
       Character(Len=*), Intent(in) :: fmt   ! output format for real number   
       Character(range(r)+4) :: tmp
        
       Write(tmp,fmt) r
       res = trim(tmp)
    End Function
    
    ! String to string without leading and trailing spaces
    Function atoa(a) result(res)
       Implicit None
       Character(:),Allocatable :: res
       Character(Len=*), Intent(in) :: a 
       Character(len(a)) :: tmp
        
       Write(tmp,'(A)') a
       Allocate(character(LEN=Len(tmp)) :: res)
       res = adjustl(tmp)
       res = trim(res)
       res='"'//res//'"'
    End Function 
    ! Interface procedures end
    
!------------------------------------------------------------------------------  
    Subroutine CsvFileHeader
       Character(12) :: fn
       Character(:),Allocatable :: Header 
       Integer :: nf, ErrNum, length
      
  length= Len('RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,DAP,L#SD,GSTD,LAID,' &
  //'LWAD,SWAD,GWAD,RWAD,VWAD,CWAD,G#AD,GWGD,HIAD,PWAD,P#AD,WSPD,WSGD,NSTD,' &
  //'PST1A,PST2A,KSTD,EWSD,LN%D,SH%D,HIPD,PWDD,PWTD,SLAD,CHTD,CWID,NWAD,RDPD,'&
  //'RL1D,RL2D,RL3D,RL4D,RL5D,RL6D,RL7D,RL8D,RL9D,SNW0C,SNW1C')

       Allocate(character(LEN=length) :: Header)

  Header = 'RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,DAP,L#SD,GSTD,LAID,' &
  //'LWAD,SWAD,GWAD,RWAD,VWAD,CWAD,G#AD,GWGD,HIAD,PWAD,P#AD,WSPD,WSGD,NSTD,' &
  //'PST1A,PST2A,KSTD,EWSD,LN%D,SH%D,HIPD,PWDD,PWTD,SLAD,CHTD,CWID,NWAD,RDPD,'&
  //'RL1D,RL2D,RL3D,RL4D,RL5D,RL6D,RL7D,RL8D,RL9D,SNW0C,SNW1C'
   
       fn = 'plantgro.csv'      
       Open (UNIT = nf, FILE = fn, FORM='FORMATTED', STATUS = 'NEW',  &
          Action='Write', IOSTAT = ErrNum)
        
       Write(nf,'(A)')Header
       Deallocate(Header)
       Close(nf)   
            
       Return
    End Subroutine CsvFileHeader
!------------------------------------------------------------------------------      
    Subroutine CsvFileHeaderSoilWat
       Character(12) :: fn
       Character(:),Allocatable :: Header 
       Integer :: nf, ErrNum, length
      
  length= Len('RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,SWTD,SWXD,ROFC,DRNC,' &
  //'PREC,IR#C,IRRC,DTWT,MWTD,TDFD,TDFC,ROFD,SW1D,SW2D,SW3D,SW4D,SW5D,SW6D,' &
  //'SW7D,SW8D,SW9D')

       Allocate(character(LEN=length) :: Header)

  Header = 'RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,SWTD,SWXD,ROFC,DRNC,' &
  //'PREC,IR#C,IRRC,DTWT,MWTD,TDFD,TDFC,ROFD,SW1D,SW2D,SW3D,SW4D,SW5D,SW6D,' &
  //'SW7D,SW8D,SW9D' 
   
       fn = 'soilwat.csv'      
       Open (UNIT = nf, FILE = fn, FORM='FORMATTED', STATUS = 'NEW', &
          Action='Write', IOSTAT = ErrNum)
        
       Write(nf,'(A)')Header
       Deallocate(Header)
       Close(nf)   
            
       Return
    End Subroutine CsvFileHeaderSoilWat
!------------------------------------------------------------------------------        
    Subroutine CsvFileHeaderSoilTemp
       Character(12) :: fn
       Character(:),Allocatable :: Header 
       Integer :: nf, ErrNum, length
      
  length= Len('RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,TS0D,TS1D,TS2D,TS3D,' &
  //'TS4D,TS5D,TS6D,TS7D,TS8D,TS9D')
  
       Allocate(character(LEN=length) :: Header)

  Header = 'RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,TS0D,TS1D,TS2D,TS3D,' &
  //'TS4D,TS5D,TS6D,TS7D,TS8D,TS9D' 
   
       fn = 'soiltemp.csv'      
       Open (UNIT = nf, FILE = fn, FORM='FORMATTED', STATUS = 'NEW', &
          Action='Write', IOSTAT = ErrNum)
        
       Write(nf,'(A)')Header
       Deallocate(Header)
       Close(nf)   
            
       Return
    End Subroutine CsvFileHeaderSoilTemp
!------------------------------------------------------------------------------            
    Subroutine CsvFileHeaderCsCer
       Character(12) :: fn
       Character(:),Allocatable :: Header 
       Integer :: nf, ErrNum, length
            
  length= Len('RUN,EXP,TR,RN,SN,ON,REP,CN,YEAR,DOY,DAS,DAP,TMEAN,TKILL,GSTD,' &
    //'L#SD,PARID,PARUD,AWAD,LAID,SAID,CAID,TWAD,SDWAD,RWAD,CWAD,LWAD,SWAD,' &
    //'HWAD,HIAD,CHWAD,EWAD,RSWAD,SNWPD,SNWLD,SNWSD,RS%D,H#AD,HWUD,T#AD,SLAD,' &
    //'RDPD,PTFD,SWXD,WAVRD,WUPRD,WFTD,WFPD,WFGD,NFTD,NFPD,NFGD,NUPRD,TFPD,' &
    //'TFGD,VRNFD,DYLFD')
  
       Allocate(character(LEN=length) :: Header)

  Header = 'RUN,EXP,TR,RN,SN,ON,REP,CN,YEAR,DOY,DAS,DAP,TMEAN,TKILL,GSTD,' &
    //'L#SD,PARID,PARUD,AWAD,LAID,SAID,CAID,TWAD,SDWAD,RWAD,CWAD,LWAD,SWAD,' &
    //'HWAD,HIAD,CHWAD,EWAD,RSWAD,SNWPD,SNWLD,SNWSD,RS%D,H#AD,HWUD,T#AD,SLAD,' &
    //'RDPD,PTFD,SWXD,WAVRD,WUPRD,WFTD,WFPD,WFGD,NFTD,NFPD,NFGD,NUPRD,TFPD,' &
    //'TFGD,VRNFD,DYLFD' 
   
       fn = 'plantgro.csv'      
       Open (UNIT = nf, FILE = fn, FORM='FORMATTED', STATUS = 'NEW', &
          Action='Write', IOSTAT = ErrNum)
        
       Write(nf,'(A)')Header
       Deallocate(Header)
       Close(nf)   
            
       Return
    End Subroutine CsvFileHeaderCsCer
!------------------------------------------------------------------------------            
    Subroutine CsvFileHeaderET
       Character(12) :: fn
       Character(:),Allocatable :: Header 
       Integer :: nf, ErrNum, length   
     
  length= Len('RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,SRAA,TMAXA,TMINA,' &
  //'EOAA,EOPA,EOSA,ETAA,EPAA,ESAA,EFAA,EMAA,EOAC,ETAC,EPAC,ESAC,EFAC,' &
  //'EMAC,ES1D,ES2D,ES3D,ES4D,ES5D,ES6D,ES7D,ES8D,ES9D')
  

       Allocate(character(LEN=length) :: Header)

  Header = 'RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,SRAA,TMAXA,TMINA,' &
  //'EOAA,EOPA,EOSA,ETAA,EPAA,ESAA,EFAA,EMAA,EOAC,ETAC,EPAC,ESAC,EFAC,' &
  //'EMAC,ES1D,ES2D,ES3D,ES4D,ES5D,ES6D,ES7D,ES8D,ES9D' 
   
       fn = 'et.csv'      
       Open (UNIT = nf, FILE = fn, FORM='FORMATTED', STATUS = 'NEW', &
          Action='Write', IOSTAT = ErrNum)
        
       Write(nf,'(A)')Header
       Deallocate(Header)
       Close(nf)   
            
       Return
    End Subroutine CsvFileHeaderET
    
!------------------------------------------------------------------------------            
    Subroutine CsvFileHeaderMZCER
       Character(12) :: fn
       Character(:),Allocatable :: Header 
       Integer :: nf, ErrNum, length   
     
  length= Len('RUN,EXP,TR,RN,REP,YEAR,DOY,DAS,DAP,L#SD,GSTD,LAID,LWAD,SWAD,' &
  //'GWAD,RWAD,VWAD,CWAD,G#AD,GWGD,HIAD,PWAD,P#AD,WSPD,WSGD,NSTD,EWSD,PST1A,' &
  //'PST2A,KSTD,LN%D,SH%D,HIPD,PWDD,PWTD,SLAD,CHTD,CWID,RDPD,RL1D,RL2D,RL3D,'&   
  //'RL4D,RL5D,RL6D,RL7D,RL8D,RL9D,CDAD,LDAD,SDAD,SNW0C,SNW1C,DTTD')

       Allocate(character(LEN=length) :: Header)

  Header = 'RUN,EXP,TR,RN,REP,YEAR,DOY,DAS,DAP,L#SD,GSTD,LAID,LWAD,SWAD,' &
  //'GWAD,RWAD,VWAD,CWAD,G#AD,GWGD,HIAD,PWAD,P#AD,WSPD,WSGD,NSTD,EWSD,PST1A,' &
  //'PST2A,KSTD,LN%D,SH%D,HIPD,PWDD,PWTD,SLAD,CHTD,CWID,RDPD,RL1D,RL2D,RL3D,'&   
  //'RL4D,RL5D,RL6D,RL7D,RL8D,RL9D,CDAD,LDAD,SDAD,SNW0C,SNW1C,DTTD' 
   
       fn = 'plantgro.csv'      
       Open (UNIT = nf, FILE = fn, FORM='FORMATTED', STATUS = 'NEW', &
          Action='Write', IOSTAT = ErrNum)
        
       Write(nf,'(A)')Header
       Deallocate(Header)
       Close(nf)   
            
       Return
    End Subroutine CsvFileHeaderMZCER
!------------------------------------------------------------------------------            
    Subroutine CsvHeadPlNCrGro
       Character(12) :: fn
       Character(:),Allocatable :: Header 
       Integer :: nf, ErrNum, length   
     
  length= Len('RUN,EXP,TR,RN,REP,YEAR,DOY,DAS,DAP,CNAD,GNAD,VNAD,GN%D,' &
  //'VN%D,NFXC,NUPC,LNAD,SNAD,LN%D,SN%D,SHND,RN%D,NFXD,SNN0C,SNN1C')
  
       Allocate(character(LEN=length) :: Header)

  Header = 'RUN,EXP,TR,RN,REP,YEAR,DOY,DAS,DAP,CNAD,GNAD,VNAD,GN%D,' &
  //'VN%D,NFXC,NUPC,LNAD,SNAD,LN%D,SN%D,SHND,RN%D,NFXD,SNN0C,SNN1C' 
   
       fn = 'plantn.csv'      
       Open (UNIT = nf, FILE = fn, FORM='FORMATTED', STATUS = 'NEW', &
          IOSTAT = ErrNum)
        
       Write(nf,'(A)')Header
       Deallocate(Header)
       Close(nf)   
            
       Return
    End Subroutine CsvHeadPlNCrGro
!------------------------------------------------------------------------------  
    Subroutine CsvHeadPlNCsCer
      Character(12) :: fn
      Character(:),Allocatable :: Header 
      Integer :: nf, ErrNum, length   
     
  length= Len('RUN,EXP,TR,RN,REP,YEAR,DOY,DAS,DAP,TMEAN,GSTD,NUAD,TNAD,SDNAD,' &
  //'RNAD,CNAD,LNAD,SNAD,HNAD,HIND,RSNAD,SNNPD,SNN0D,SNN1D,RN%D,LN%D,SN%D,' &
  //'HN%D,SDN%D,VN%D,LN%RD,SN%RD,RN%RD,VCN%,VMN%,NUPRD,NDEMD')

      Allocate(character(LEN=length) :: Header)

  Header = 'RUN,EXP,TR,RN,REP,YEAR,DOY,DAS,DAP,TMEAN,GSTD,NUAD,TNAD,SDNAD,' &
  //'RNAD,CNAD,LNAD,SNAD,HNAD,HIND,RSNAD,SNNPD,SNN0D,SNN1D,RN%D,LN%D,SN%D,' &
  //'HN%D,SDN%D,VN%D,LN%RD,SN%RD,RN%RD,VCN%,VMN%,NUPRD,NDEMD' 
   
      fn = 'plantn.csv'      
      Open (UNIT = nf, FILE = fn, FORM='FORMATTED', STATUS = 'NEW', &
        Action='Write', IOSTAT = ErrNum)
        
      Write(nf,'(A)')Header
      Deallocate(Header)
      Close(nf)   
            
      Return
    End Subroutine CsvHeadPlNCsCer
!------------------------------------------------------------------------------    
    Subroutine CsvHeadSoilNi
      Character(12) :: fn
      Character(:),Allocatable :: Header 
      Integer :: nf, ErrNum, length   
     
  length= Len('RUN,EXP,TR,RN,REP,YEAR,DOY,DAS,NAPC,NI#M,NLCC,NIAD,NITD,NHTD,' &
  //'NI1D,NI2D,NI3D,NI4D,NI5D,NI6D,NI7D,NI8D,NI9D,NH1D,NH2D,NH3D,NH4D,NH5D,' &
  //'NH6D,NH7D,NH8D,NH9D,NMNC,NITC,NDNC,NIMC,AMLC,NNMNC,NUCM')

      Allocate(character(LEN=length) :: Header)

  Header = 'RUN,EXP,TR,RN,REP,YEAR,DOY,DAS,NAPC,NI#M,NLCC,NIAD,NITD,NHTD,' &
  //'NI1D,NI2D,NI3D,NI4D,NI5D,NI6D,NI7D,NI8D,NI9D,NH1D,NH2D,NH3D,NH4D,NH5D,' &
  //'NH6D,NH7D,NH8D,NH9D,NMNC,NITC,NDNC,NIMC,AMLC,NNMNC,NUCM' 
   
      fn = 'soilni.csv'      
      Open (UNIT = nf, FILE = fn, FORM='FORMATTED', STATUS = 'NEW', &
        Action='Write', IOSTAT = ErrNum)
        
      Write(nf,'(A)')Header
      Deallocate(Header)
      Close(nf)   
            
      Return
    End Subroutine CsvHeadSoilNi
 !------------------------------------------------------------------------------            
   Subroutine CsvHeadPlantN
      Character(12) :: fn
      Character(:),Allocatable :: Header 
      Integer :: nf, ErrNum, length   
     
  length= Len('RUN,EXP,TR,RN,REP,YEAR,DOY,DAS,DAP,CNAD,GNAD,VNAD,GN%D,VN%D,' &
  //'NUPC,LNAD,SNAD,LN%D,SN%D,RN%D,SNN0C,SNN1C') 

      Allocate(character(LEN=length) :: Header)

  Header = 'RUN,EXP,TR,RN,REP,YEAR,DOY,DAS,DAP,CNAD,GNAD,VNAD,GN%D,VN%D,' &
  //'NUPC,LNAD,SNAD,LN%D,SN%D,RN%D,SNN0C,SNN1C' 
   
      fn = 'plantn.csv'      
      Open (UNIT = nf, FILE = fn, FORM='FORMATTED', STATUS = 'NEW', &
        Action='Write', IOSTAT = ErrNum)
        
      Write(nf,'(A)')Header
      Deallocate(Header)
      Close(nf)   
            
      Return
   End Subroutine CsvHeadPlantN
!------------------------------------------------------------------------------            
   Subroutine CsvHeadWth
      Character(12) :: fn
      Character(:),Allocatable :: Header 
      Integer :: nf, ErrNum, length   
     
  length= Len('RUN,EXP,TR,RN,REP,YEAR,DOY,DAS,PRED,DAYLD,TWLD,SRAD,' &
  //'PARD,CLDD,TMXD,TMND,TAVD,TDYD,TDWD,TGAD,TGRD,WDSD,CO2D') 

      Allocate(character(LEN=length) :: Header)

  Header = 'RUN,EXP,TR,RN,REP,YEAR,DOY,DAS,PRED,DAYLD,TWLD,SRAD,' &
  //'PARD,CLDD,TMXD,TMND,TAVD,TDYD,TDWD,TGAD,TGRD,WDSD,CO2D' 
   
      fn = 'weather.csv'      
      Open (UNIT = nf, FILE = fn, FORM='FORMATTED', STATUS = 'NEW', &
         Action='Write', IOSTAT = ErrNum)
        
      Write(nf,'(A)')Header
      Deallocate(Header)
      Close(nf)   
            
      Return
   End Subroutine CsvHeadWth
 !-----------------------------------------------------------------------------  
   Subroutine CsvHeadPlantGr2
      Character(12) :: fn
      Character(:),Allocatable :: Header 
      Integer :: nf, ErrNum, length   
     
  length= Len('RUN,EXP,TR,RN,REP,SN,ON,CN,YEAR,DOY,DAS,DAP,TMEAN,GSTD,RSTD,' &
  //'LAIPD,LAISD,LAID,CHTD,SDWAD,SNWLD,SNWSD,H#AD,HWUD,SHRTD,PTFD,RDPD,RL1D,' &
  //'RL2D,RL3D,RL4D,RL5D,RL6D,RL7D,RL8D,RL9D,RL10D') 

      Allocate(character(LEN=length) :: Header)

  Header = 'RUN,EXP,TR,RN,REP,SN,ON,CN,YEAR,DOY,DAS,DAP,TMEAN,GSTD,RSTD,' &
  //'LAIPD,LAISD,LAID,CHTD,SDWAD,SNWLD,SNWSD,H#AD,HWUD,SHRTD,PTFD,RDPD,RL1D,' &
  //'RL2D,RL3D,RL4D,RL5D,RL6D,RL7D,RL8D,RL9D,RL10D' 
   
      fn = 'plantgr2.csv'      
      Open (UNIT = nf, FILE = fn, FORM='FORMATTED', STATUS = 'NEW',  &
         Action='Write', IOSTAT = ErrNum)
        
      Write(nf,'(A)')Header
      Deallocate(Header)
      Close(nf)   
            
      Return
   End Subroutine CsvHeadPlantGr2
!------------------------------------------------------------------------------ 
   Subroutine CsvHeadPlGrf
      Character(12) :: fn
      Character(:),Allocatable :: Header 
      Integer :: nf, ErrNum, length   
     
  length= Len('RUN,EXP,TR,RN,REP,SN,ON,CN,YEAR,DOY,DAS,DAP,TMEAN,GSTD,DU,' &
  //'VRNFD,DYLFD,TFGEM,WFGE,TFPD,WFPD,NFPD,CO2FD,RSFPD,TFGD,WFGD,NFGD,WFTD,' &
  //'NFTD,WAVRD,WUPRD,SWXD,EOPD,SNXD,LN%RD,SN%RD,RN%RD')
  
      Allocate(character(LEN=length) :: Header)

  Header = 'RUN,EXP,TR,RN,REP,SN,ON,CN,YEAR,DOY,DAS,DAP,TMEAN,GSTD,DU,' &
  //'VRNFD,DYLFD,TFGEM,WFGE,TFPD,WFPD,NFPD,CO2FD,RSFPD,TFGD,WFGD,NFGD,WFTD,' &
  //'NFTD,WAVRD,WUPRD,SWXD,EOPD,SNXD,LN%RD,SN%RD,RN%RD' 
   
      fn = 'plantgrf.csv'      
      Open (UNIT = nf, FILE = fn, FORM='FORMATTED', STATUS = 'NEW', &
         Action='Write', IOSTAT = ErrNum)
        
      Write(nf,'(A)')Header
      Deallocate(Header)
      Close(nf)   
            
      Return
   End Subroutine CsvHeadPlGrf
!------------------------------------------------------------------------------
   Subroutine CsvHeadEvalCsCer
      Character(12) :: fn
      Character(:),Allocatable :: Header 
      Integer :: nf, ErrNum, length   
     
  length= Len('RUN,EXCODE,TRNO,RN,REP,CR,EDAPS,EDAPM,DRAPS,DRAPM,TSAPS,TSAPM,' &
  //'ADAPS,ADAPM,MDAPS,MDAPM,HWAMS,HWAMM,HWUMS,HWUMM,H#AMS,H#AMM,H#GMS,H#GMM,' &
  //'LAIXS,LAIXM,L#SMS,L#SMM,T#AMS,T#AMM,CWAMS,CWAMM,VWAMS,VWAMM,HIAMS,HIAMM,' &
  //'HN%MS,HN%MM,VN%MS,VN%MM,CNAMS,CNAMM,HNAMS,HNAMM,HINMS,HINMM' )

      Allocate(character(LEN=length) :: Header)

  Header = 'RUN,EXCODE,TRNO,RN,REP,CR,EDAPS,EDAPM,DRAPS,DRAPM,TSAPS,TSAPM,' &
  //'ADAPS,ADAPM,MDAPS,MDAPM,HWAMS,HWAMM,HWUMS,HWUMM,H#AMS,H#AMM,H#GMS,H#GMM,' &
  //'LAIXS,LAIXM,L#SMS,L#SMM,T#AMS,T#AMM,CWAMS,CWAMM,VWAMS,VWAMM,HIAMS,HIAMM,' &
  //'HN%MS,HN%MM,VN%MS,VN%MM,CNAMS,CNAMM,HNAMS,HNAMM,HINMS,HINMM' 
   
      fn = 'evaluate.csv'      
      Open (UNIT = nf, FILE = fn, FORM='FORMATTED', STATUS = 'NEW', &
         Action='Write', IOSTAT = ErrNum)
        
      Write(nf,'(A)')Header
      Deallocate(Header)
      Close(nf)   
            
      Return
   End Subroutine CsvHeadEvalCsCer
!------------------------------------------------------------------------------ 
   Subroutine CsvHeadEvOpsum(cICOUNT,cOLAP)
      Integer :: i, cICOUNT
      CHARACTER(Len=6),  DIMENSION(cICOUNT) :: cOLAP   
      
      Character(12) :: fn
      Character(:),Allocatable :: Header 
      Integer :: nf, ErrNum, length      
      Character(Len=700) :: tmp1 
      
      length= Len('RUN,EXCODE,CG,TN,RN,CR')

      Allocate(character(LEN=length) :: Header)

      Header = 'RUN,EXCODE,CG,TN,RN,CR' 
   
      tmp1 = trim(adjustl(cOLAP(1))) //'S'// ',' // trim(adjustl(cOLAP(1))) //'M'
      do i = 2, cICOUNT
         tmp1 = trim(tmp1) // ',' // trim(adjustl(cOLAP(i))) // 'S' // ',' // &
            trim(adjustl(cOLAP(i))) // 'M'
      end do            

      tmp1 = Header // ',' // trim(adjustl(tmp1))
   
      fn = 'evaluate.csv'      
      Open (UNIT = nf, FILE = fn, FORM='FORMATTED', STATUS = 'NEW', &
         Action='Write', IOSTAT = ErrNum)
        
      Write(nf,'(A)')tmp1
      Deallocate(Header)
      Close(nf)   
            
      Return
   End Subroutine CsvHeadEvOpsum
!------------------------------------------------------------------------------ 
   Subroutine CsvHeadSumOpsum
      Character(12) :: fn
      Character(:),Allocatable :: Header 
      Integer :: nf, ErrNum, length   
     
  length= Len('RUNNO,TRNO,R#,O#,C#,CR,MODEL,TNAM,FNAM,WSTA,SOIL_ID,' &
  // 'SDAT,PDAT,EDAT,ADAT,MDAT,HDAT,DWAP,CWAM,HWAM,HWAH,BWAH,PWAM,HWUM,' &
  // 'H#AM,H#UM,HIAM,LAIX,IR#M,IRCM,PRCM,ETCM,EPCM,ESCM,ROCM,DRCM,SWXM,' &
  // 'NI#M,NICM,NFXM,NUCM,NLCM,NIAM,CNAM,GNAM,PI#M,PICM,PUPC,SPAM,KI#M,' &
  // 'KICM,KUPC,SKAM,RECM,ONTAM,ONAM,OPTAM,OPAM,OCTAM,OCAM,DMPPM,DMPEM,' &
  // 'DMPTM,DMPIM,YPPM,YPEM,YPTM,YPIM,DPNAM,DPNUM,YPNAM,YPNUM,NDCH,TMAXA,' &
  // 'TMINA,SRADA,DAYLA,CO2A,PRCP,ETCP,ESCP,EPCP')

       Allocate(character(LEN=length) :: Header)

  Header = 'RUNNO,TRNO,R#,O#,C#,CR,MODEL,TNAM,FNAM,WSTA,SOIL_ID,' &
  // 'SDAT,PDAT,EDAT,ADAT,MDAT,HDAT,DWAP,CWAM,HWAM,HWAH,BWAH,PWAM,HWUM,' &
  // 'H#AM,H#UM,HIAM,LAIX,IR#M,IRCM,PRCM,ETCM,EPCM,ESCM,ROCM,DRCM,SWXM,' &
  // 'NI#M,NICM,NFXM,NUCM,NLCM,NIAM,CNAM,GNAM,PI#M,PICM,PUPC,SPAM,KI#M,' &
  // 'KICM,KUPC,SKAM,RECM,ONTAM,ONAM,OPTAM,OPAM,OCTAM,OCAM,DMPPM,DMPEM,' &
  // 'DMPTM,DMPIM,YPPM,YPEM,YPTM,YPIM,DPNAM,DPNUM,YPNAM,YPNUM,NDCH,TMAXA,' &
  // 'TMINA,SRADA,DAYLA,CO2A,PRCP,ETCP,ESCP,EPCP' 
   
       fn = 'summary.csv'      
       Open (UNIT = nf, FILE = fn, FORM='FORMATTED', STATUS = 'NEW', &
          Action='Write', IOSTAT = ErrNum)
        
       Write(nf,'(A)')Header
       Deallocate(Header)
       Close(nf)   
            
       Return
    End Subroutine CsvHeadSumOpsum
!------------------------------------------------------------------------------ 

   Subroutine CsvHeadCrgroPlantC
      Character(12) :: fn
      Character(:),Allocatable :: Header 
      Integer :: nf, ErrNum, length   
     
  length= Len('RUN,EXP,TR,RN,REP,YEAR,DOY,DAS,DAP,TWAD,PHAD,CMAD,CGRD,' &
  //'GRAD,MRAD,CHAD,CL%D,CS%D,TGNN,TGAV,GN%D,GL%D,GC%D') 

      Allocate(character(LEN=length) :: Header)

  Header = 'RUN,EXP,TR,RN,REP,YEAR,DOY,DAS,DAP,TWAD,PHAD,CMAD,CGRD,' &
  //'GRAD,MRAD,CHAD,CL%D,CS%D,TGNN,TGAV,GN%D,GL%D,GC%D' 
   
      fn = 'plantc.csv'      
      Open (UNIT = nf, FILE = fn, FORM='FORMATTED', STATUS = 'NEW', &
         Action='Write', IOSTAT = ErrNum)
        
      Write(nf,'(A)')Header
      Deallocate(Header)
      Close(nf)   
            
      Return
   End Subroutine CsvHeadCrgroPlantC
!------------------------------------------------------------------------------ 
   Subroutine CsvHeadSoilOrg(N_ELEMS)
      Character(12) :: fn
      Character(:),Allocatable :: Header 
      
      Integer :: N_ELEMS       ! Number of elements: 1 = N, 2 = N+P, 3 = N+P+S (-)
      Integer :: nf, ErrNum, length   
  
      If (N_ELEMS > 1) Then 
         length= Len('RUN,EXP,TR,RN,REP,YEAR,DOY,DAS,OMAC,SCDD,SOCD,' &
         //'SC0D,SCTD,SOMCT,LCTD,ONAC,SNDD,SOND,SN0D,SNTD,' &
         //'SOMNT,LNTD,OPAC,SPDD,SOPD,SP0D,SPTD,SOMPT,LPTD') 

         Allocate(character(LEN=length) :: Header)

         Header = 'RUN,EXP,TR,RN,REP,YEAR,DOY,DAS,OMAC,SCDD,SOCD,' &
         //'SC0D,SCTD,SOMCT,LCTD,ONAC,SNDD,SOND,SN0D,SNTD,' &
         //'SOMNT,LNTD,OPAC,SPDD,SOPD,SP0D,SPTD,SOMPT,LPTD' 
      Else
         length= Len('RUN,EXP,TR,RN,REP,YEAR,DOY,DAS,' &
         //'OMAC,SCDD,SOCD,SC0D,SCTD,SOMCT,LCTD,ONAC,SNDD,' &
         //'SOND,SN0D,SNTD,SOMNT,LNTD')

         Allocate(character(LEN=length) :: Header)

         Header = 'RUN,EXP,TR,RN,REP,YEAR,DOY,DAS,' &
         //'OMAC,SCDD,SOCD,SC0D,SCTD,SOMCT,LCTD,ONAC,SNDD,' &
         //'SOND,SN0D,SNTD,SOMNT,LNTD' 
      End If 
  
      fn = 'soilorg.csv'      
      Open (UNIT = nf, FILE = fn, FORM='FORMATTED', STATUS = 'NEW', Action='Write', &
        IOSTAT = ErrNum)
     
      Write(nf,'(A)')Header
      Deallocate(Header)
      Close(nf)   
         
      Return
   End Subroutine CsvHeadSoilOrg
!------------------------------------------------------------------------------ 
   Subroutine CsvHeadETPhot
      Character(12) :: fn
      Character(:),Allocatable :: Header 
      
      Integer :: nf, ErrNum, length   
  
     length= Len('RUN,EXP,TR,RN,REP,YEAR,DOY,DAS,' &
     //'LI%D,PHAD,PHAN,LI%N,SLLN,SLHN,N%LN,N%HN,LMLN,LMHN,TGNN,TGAV') 

      Allocate(character(LEN=length) :: Header)

     Header = 'RUN,EXP,TR,RN,REP,YEAR,DOY,DAS,' &
     //'LI%D,PHAD,PHAN,LI%N,SLLN,SLHN,N%LN,N%HN,LMLN,LMHN,TGNN,TGAV'    
  
      fn = 'etphot.csv'      
      Open (UNIT = nf, FILE = fn, FORM='FORMATTED', STATUS = 'NEW', Action='Write', &
         IOSTAT = ErrNum)
     
      Write(nf,'(A)')Header
      Deallocate(Header)
      Close(nf)   
         
      Return
   End Subroutine CsvHeadETPhot
!------------------------------------------------------------------------------ 
   Subroutine CsvHeadMulch
      Character(12) :: fn
      Character(:),Allocatable :: Header 
      
      Integer :: nf, ErrNum, length   
  
      length= Len('RUN,EXP,TR,RN,REP,YEAR,DOY,DAS,MCFD,MDEPD,MWAD,MWTD') 

      Allocate(character(LEN=length) :: Header)

      Header = 'RUN,EXP,TR,RN,REP,YEAR,DOY,DAS,MCFD,MDEPD,MWAD,MWTD'    
  
      fn = 'mulch.csv'      
      Open (UNIT = nf, FILE = fn, FORM='FORMATTED', STATUS = 'NEW', Action='Write', &
         IOSTAT = ErrNum)
     
      Write(nf,'(A)')Header
      Deallocate(Header)
      Close(nf)   
         
      Return
   End Subroutine CsvHeadMulch
!------------------------------------------------------------------------------ 
   Subroutine CsvHeadPlantP
      Character(12) :: fn
      Character(:),Allocatable :: Header 
      
      Integer :: nf, ErrNum, length   
  
     length= Len('RUN,EXP,TR,RN,REP,YEAR,DOY,DAS,DAP,' &
     //'PSHOD,PRTOD,PSLOD,PSDOD,PSHMD,PRTMD,PSLMD,PSDMD,SHPPD,RTPPD,SLPPD,' &
     //'SDPPD,PLPPD,SHPAD,RTPAD,SLPAD,SDPAD,PLPAD,PST1A,PST2A,PUPD,PUPC,' &
     //'SNP0C,SNP1C,PHFR1,PHFR2,SHWAD,RWAD,SHAD,GWAD,PSTRAT,NTOPD,PTDD')
     
      Allocate(character(LEN=length) :: Header) 

     Header = 'RUN,EXP,TR,RN,REP,YEAR,DOY,DAS,DAP' &
     //'PSHOD,PRTOD,PSLOD,PSDOD,PSHMD,PRTMD,PSLMD,PSDMD,SHPPD,RTPPD,SLPPD,' &
     //'SDPPD,PLPPD,SHPAD,RTPAD,SLPAD,SDPAD,PLPAD,PST1A,PST2A,PUPD,PUPC,' &
     //'SNP0C,SNP1C,PHFR1,PHFR2,SHWAD,RWAD,SHAD,GWAD,PSTRAT,NTOPD,PTDD'  
  
      fn = 'plantp.csv'      
      Open (UNIT = nf, FILE = fn, FORM='FORMATTED', STATUS = 'NEW', Action='Write', &
         IOSTAT = ErrNum)
     
      Write(nf,'(A)')Header
      Deallocate(Header)
      Close(nf)   
         
      Return
   End Subroutine CsvHeadPlantP
!------------------------------------------------------------------------------ 
   Subroutine CsvHeadSoilPi
      Character(12) :: fn
      Character(:),Allocatable :: Header 
      
      Integer :: nf, ErrNum, length   
  
     length= Len('RUN,EXP,TR,RN,REP,YEAR,DOY,DAS,' &
     //'PIAD,PAVLD,PSOLD,PLABD,PACTD,PSTAD,PAPC,PMNC,PIMC,' &
     //'PUPC,PAV1D,PAV2D,PAV3D,PAV4D,PAV5D,PUP1D,PUP2D,PUP3D,' &
     //'PUP4D,PUP5D,PLAB1,PLAB2,PLAB3,PLAB4,PLAB5') 

      Allocate(character(LEN=length) :: Header)

     Header = 'RUN,EXP,TR,RN,REP,YEAR,DOY,DAS,' &
     //'PIAD,PAVLD,PSOLD,PLABD,PACTD,PSTAD,PAPC,PMNC,PIMC,' &
     //'PUPC,PAV1D,PAV2D,PAV3D,PAV4D,PAV5D,PUP1D,PUP2D,PUP3D,' &
     //'PUP4D,PUP5D,PLAB1,PLAB2,PLAB3,PLAB4,PLAB5'    
  
      fn = 'soilpi.csv'      
      Open (UNIT = nf, FILE = fn, FORM='FORMATTED', STATUS = 'NEW', Action='Write', &
         IOSTAT = ErrNum)
     
      Write(nf,'(A)')Header
      Deallocate(Header)
      Close(nf)   
         
      Return
   End Subroutine CsvHeadSoilPi
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

End Module CsvGeneric
!------------------------------------------------------------------------------            
