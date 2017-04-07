Module Linklist
!
!
Implicit None
Save
    Character(Len=1) :: switchcsv
   
! Data dictionary: declare variable types & definitions 
!------------------------------------------------------------------------------

    Type :: lin_value
       Character(:), Allocatable :: pcline
       Type (lin_value), Pointer :: p
    End Type

    Type (lin_value), Pointer :: head      ! Pointer to head of list
    Type (lin_value), Pointer :: tail      ! Pointer to tail of list
    Type (lin_value), Pointer :: ptr       ! Temporary Pointer
    
    Integer :: istat                       ! Status: 0 for success
!------------------------------------------------------------------------------

    Type :: lin_valuesw
       Character(:), Allocatable :: pclinesw
       Type (lin_valuesw), Pointer :: psw
    End Type

    Type (lin_valuesw), Pointer :: headsw     
    Type (lin_valuesw), Pointer :: tailsw     
    Type (lin_valuesw), Pointer :: ptrsw      
    
    Integer :: istatsw                        
!------------------------------------------------------------------------------
    
    Type :: lin_valuetemp
       Character(:), Allocatable :: pclinetemp
       Type (lin_valuetemp), Pointer :: ptemp
    End Type

    Type (lin_valuetemp), Pointer :: headtemp      
    Type (lin_valuetemp), Pointer :: tailtemp      
    Type (lin_valuetemp), Pointer :: ptrtemp       
    
    Integer :: istattemp                           
!------------------------------------------------------------------------------
    
!   for cscer
    Type :: lin_valuecscer
       Character(:), Allocatable :: pclinecscer
       Type (lin_valuecscer), Pointer :: pcscer
    End Type

    Type (lin_valuecscer), Pointer :: headcscer      
    Type (lin_valuecscer), Pointer :: tailcscer      
    Type (lin_valuecscer), Pointer :: ptrcscer       
    
    Integer :: istatcscer                            
!------------------------------------------------------------------------------
   
!   for ET
    Type :: lin_valueET
       Character(:), Allocatable :: pclineET
       Type (lin_valueET), Pointer :: pET
    End Type

    Type (lin_valueET), Pointer :: headET      
    Type (lin_valueET), Pointer :: tailET      
    Type (lin_valueET), Pointer :: ptrET       
    
    Integer :: istatET                         
!------------------------------------------------------------------------------

!   for MZCER
    Type :: lin_valueMZCER
       Character(:), Allocatable :: pclineMZCER
       Type (lin_valueMZCER), Pointer :: pMZCER
    End Type

    Type (lin_valueMZCER), Pointer :: headMZCER      
    Type (lin_valueMZCER), Pointer :: tailMZCER      
    Type (lin_valueMZCER), Pointer :: ptrMZCER       
    
    Integer :: istatMZCER                            
!------------------------------------------------------------------------------

!   for PlNCrGro
    Type :: lin_valuePlNCrGro
       Character(:), Allocatable :: pclinePlNCrGro
       Type (lin_valuePlNCrGro), Pointer :: pPlNCrGro
    End Type

    Type (lin_valuePlNCrGro), Pointer :: headPlNCrGro    
    Type (lin_valuePlNCrGro), Pointer :: tailPlNCrGro    
    Type (lin_valuePlNCrGro), Pointer :: ptrPlNCrGro     
    
    Integer :: istatPlNCrGro                             
!------------------------------------------------------------------------------

!   for PlNCsCer
    Type :: lin_valuePlNCsCer
       Character(:), Allocatable :: pclinePlNCsCer
       Type (lin_valuePlNCsCer), Pointer :: pPlNCsCer
    End Type

    Type (lin_valuePlNCsCer), Pointer :: headPlNCsCer   
    Type (lin_valuePlNCsCer), Pointer :: tailPlNCsCer   
    Type (lin_valuePlNCsCer), Pointer :: ptrPlNCsCer    
    
    Integer :: istatPlNCsCer                            
!------------------------------------------------------------------------------

!   for SoilNi
    Type :: lin_valueSoilNi
       Character(:), Allocatable :: pclineSoilNi
       Type (lin_valueSoilNi), Pointer :: pSoilNi
    End Type

    Type (lin_valueSoilNi), Pointer :: headSoilNi      
    Type (lin_valueSoilNi), Pointer :: tailSoilNi      
    Type (lin_valueSoilNi), Pointer :: ptrSoilNi       
    
    Integer :: istatSoilNi                             
!------------------------------------------------------------------------------

!   for PlNMzCer
    Type :: lin_valuePlNMzCer
       Character(:), Allocatable :: pclinePlNMzCer
       Type (lin_valuePlNMzCer), Pointer :: pPlNMzCer
    End Type

    Type (lin_valuePlNMzCer), Pointer :: headPlNMzCer   
    Type (lin_valuePlNMzCer), Pointer :: tailPlNMzCer   
    Type (lin_valuePlNMzCer), Pointer :: ptrPlNMzCer    
    
    Integer :: istatPlNMzCer                            
!------------------------------------------------------------------------------

!   for PlNMzCer
    Type :: lin_valueWth
       Character(:), Allocatable :: pclineWth
       Type (lin_valueWth), Pointer :: pWth
    End Type

    Type (lin_valueWth), Pointer :: headWth      
    Type (lin_valueWth), Pointer :: tailWth      
    Type (lin_valueWth), Pointer :: ptrWth       
    
    Integer :: istatWth                          
!------------------------------------------------------------------------------
!  for PlantGr2
    Type :: lin_valuePlGr2
       Character(:), Allocatable :: pclinePlGr2
       Type (lin_valuePlGr2), Pointer :: pPlGr2
    End Type

    Type (lin_valuePlGr2), Pointer :: headPlGr2    
    Type (lin_valuePlGr2), Pointer :: tailPlGr2    
    Type (lin_valuePlGr2), Pointer :: ptrPlGr2     
    
    Integer :: istatPlGr2                          
!------------------------------------------------------------------------------         
!   for PlantGrf.OUT
    Type :: lin_valuePlGrf
       Character(:), Allocatable :: pclinePlGrf
       Type (lin_valuePlGrf), Pointer :: pPlGrf
    End Type

    Type (lin_valuePlGrf), Pointer :: headPlGrf    
    Type (lin_valuePlGrf), Pointer :: tailPlGrf    
    Type (lin_valuePlGrf), Pointer :: ptrPlGrf     
    
    Integer :: istatPlGrf                          
!------------------------------------------------------------------------------  
!   for Evaluate.OUT
    Type :: lin_valueEvalCsCer
       Character(:), Allocatable :: pclineEvalCsCer
       Type (lin_valueEvalCsCer), Pointer :: pEvalCsCer
    End Type

    Type (lin_valueEvalCsCer), Pointer :: headEvalCsCer  
    Type (lin_valueEvalCsCer), Pointer :: tailEvalCsCer  
    Type (lin_valueEvalCsCer), Pointer :: ptrEvalCsCer   
    
    Integer :: istatEvalCsCer                            
!------------------------------------------------------------------------------ 
!   for evaluate.OUT
    Type :: lin_valueEvOpsum
       Character(:), Allocatable :: pclineEvOpsum
       Type (lin_valueEvOpsum), Pointer :: pEvOpsum
    End Type

    Type (lin_valueEvOpsum), Pointer :: headEvOpsum    
    Type (lin_valueEvOpsum), Pointer :: tailEvOpsum    
    Type (lin_valueEvOpsum), Pointer :: ptrEvOpsum     
    
    Integer :: istatEvOpsum                            
!------------------------------------------------------------------------------  
!   for evaluate.OUT
    Type :: lin_valueSumOpsum
       Character(:), Allocatable :: pclineSumOpsum
       Type (lin_valueSumOpsum), Pointer :: pSumOpsum
    End Type

    Type (lin_valueSumOpsum), Pointer :: headSumOpsum    
    Type (lin_valueSumOpsum), Pointer :: tailSumOpsum    
    Type (lin_valueSumOpsum), Pointer :: ptrSumOpsum     
    
    Integer :: istatSumOpsum                             
!------------------------------------------------------------------------------ 
    Type :: lin_valuePlCCrGro
       Character(:), Allocatable :: pclinePlCCrGro
       Type (lin_valuePlCCrGro), Pointer :: pPlCCrGro
    End Type

    Type (lin_valuePlCCrGro), Pointer :: headPlCCrGro    
    Type (lin_valuePlCCrGro), Pointer :: tailPlCCrGro    
    Type (lin_valuePlCCrGro), Pointer :: ptrPlCCrGro     
    
    Integer :: istatPlCCrGro                             
!--------------------------------------------------------------------------------------
Type :: lin_valueSoilOrg
       Character(:), Allocatable :: pclineSoilOrg
       Type (lin_valueSoilOrg), Pointer :: pSoilOrg
    End Type

    Type (lin_valueSoilOrg), Pointer :: headSoilOrg      
    Type (lin_valueSoilOrg), Pointer :: tailSoilOrg      
    Type (lin_valueSoilOrg), Pointer :: ptrSoilOrg       
    
    Integer :: istatSoilOrg                              
!--------------------------------------------------------------------------------------
Type :: lin_valueETPhot
       Character(:), Allocatable :: pclineETPhot
       Type (lin_valueETPhot), Pointer :: pETPhot
    End Type

    Type (lin_valueETPhot), Pointer :: headETPhot      
    Type (lin_valueETPhot), Pointer :: tailETPhot      
    Type (lin_valueETPhot), Pointer :: ptrETPhot       
    
    Integer :: istatETPhot                             
!--------------------------------------------------------------------------------------
Type :: lin_valueMulch
       Character(:), Allocatable :: pclineMulch
       Type (lin_valueMulch), Pointer :: pMulch
    End Type

    Type (lin_valueMulch), Pointer :: headMulch      
    Type (lin_valueMulch), Pointer :: tailMulch      
    Type (lin_valueMulch), Pointer :: ptrMulch       
    
    Integer :: istatMulch                            
!--------------------------------------------------------------------------------------
Type :: lin_valuePlantP
       Character(:), Allocatable :: pclinePlantP
       Type (lin_valuePlantP), Pointer :: pPlantP
    End Type

    Type (lin_valuePlantP), Pointer :: headPlantP      
    Type (lin_valuePlantP), Pointer :: tailPlantP      
    Type (lin_valuePlantP), Pointer :: ptrPlantP       
    
    Integer :: istatPlantP                             
!--------------------------------------------------------------------------------------
Type :: lin_valueSoilPi
       Character(:), Allocatable :: pclineSoilPi
       Type (lin_valueSoilPi), Pointer :: pSoilPi
    End Type

    Type (lin_valueSoilPi), Pointer :: headSoilPi      
    Type (lin_valueSoilPi), Pointer :: tailSoilPi      
    Type (lin_valueSoilPi), Pointer :: ptrSoilPi       
    
    Integer :: istatSoilPi                             
!--------------------------------------------------------------------------------------
               
Contains

!------------------------------------------------------------------------------

 Subroutine Linklst(ptxtline)

    Character(:), Allocatable :: ptxtline  ! Temporary variable
        
    If(.Not. Associated(head)) Then     ! No values in list
      Allocate(head, Stat=istat)        ! Allocate new value
      If(istat==0) Then                 ! Check status of Allocate
        tail => head                    ! Tail pts to new value
        Nullify(tail%p)                 ! Nullify p in new value
        tail%pcline = ptxtline          ! Store 
      Else
        ! Error message
      End If
    Else
      Allocate(tail%p, Stat=istat)      ! Allocate new value
      If(istat==0) Then                 ! Check status of Allocate
        tail=> tail%p                   ! Tail pts to new value
        Nullify(tail%p)                 ! Nullify p in new value
        tail%pcline = ptxtline          ! Store 
      Else
      ! Error message
      End If
    End If

 End Subroutine Linklst
!------------------------------------------------------------------------------

 Subroutine LinklstSW(ptxtlinesw)

    Character(:), Allocatable :: ptxtlinesw            
        
    If(.Not. Associated(headsw)) Then     
      Allocate(headsw, Stat=istatsw)      
      If(istatsw==0) Then                 
        tailsw => headsw                  
        Nullify(tailsw%psw)               
        tailsw%pclinesw = ptxtlinesw      
      Else
        ! Error message
      End If
    Else
      Allocate(tailsw%psw, Stat=istatsw)  
      If(istatsw==0) Then                 
        tailsw=> tailsw%psw               
        Nullify(tailsw%psw)               
        tailsw%pclinesw = ptxtlinesw       
      Else
      ! Error message
      End If
    End If

 End Subroutine LinklstSW
!------------------------------------------------------------------------------

 Subroutine LinklstTemp(ptxtlinetemp)

    Character(:), Allocatable :: ptxtlinetemp            
        
    If(.Not. Associated(headtemp)) Then     
      Allocate(headtemp, Stat=istattemp)    
      If(istattemp==0) Then                 
        tailtemp => headtemp                
        Nullify(tailtemp%ptemp)             
        tailtemp%pclinetemp = ptxtlinetemp   
      Else
        ! Error message
      End If
    Else
      Allocate(tailtemp%ptemp, Stat=istattemp)      
      If(istattemp==0) Then                         
        tailtemp=> tailtemp%ptemp                   
        Nullify(tailtemp%ptemp)                     
        tailtemp%pclinetemp = ptxtlinetemp           
      Else
      ! Error message
      End If
    End If

 End Subroutine LinklstTemp
!------------------------------------------------------------------------------

 Subroutine LinklstCsCer(ptxtlineCsCer)

    Character(:), Allocatable :: ptxtlineCsCer            
        
    If(.Not. Associated(headCsCer)) Then          
      Allocate(headCsCer, Stat=istatCsCer)        
      If(istatCsCer==0) Then                      
        tailCsCer => headCsCer                    
        Nullify(tailCsCer%pCsCer)                 
        tailCsCer%pclineCsCer = ptxtlineCsCer     
      Else
        ! Error message
      End If
    Else
      Allocate(tailCsCer%pCsCer, Stat=istatCsCer)      
      If(istatCsCer==0) Then                           
        tailCsCer=> tailCsCer%pCsCer                   
        Nullify(tailCsCer%pCsCer)                      
        tailCsCer%pclineCsCer = ptxtlineCsCer           
      Else
      ! Error message
      End If
    End If

 End Subroutine LinklstCsCer
!------------------------------------------------------------------------------

 Subroutine LinklstET(ptxtlineET)

    Character(:), Allocatable :: ptxtlineET            
        
    If(.Not. Associated(headET)) Then       
      Allocate(headET, Stat=istatET)        
      If(istatET==0) Then                   
        tailET => headET                    
        Nullify(tailET%pET)                 
        tailET%pclineET = ptxtlineET        
      Else
        ! Error message
      End If
    Else
      Allocate(tailET%pET, Stat=istatET)      
      If(istatET==0) Then                     
        tailET=> tailET%pET                  
        Nullify(tailET%pET)                   
        tailET%pclineET = ptxtlineET          
      Else
      ! Error message
      End If
    End If

 End Subroutine LinklstET
!------------------------------------------------------------------------------

 Subroutine LinklstMZCER(ptxtlineMZCER)

    Character(:), Allocatable :: ptxtlineMZCER            
        
    If(.Not. Associated(headMZCER)) Then          
      Allocate(headMZCER, Stat=istatMZCER)        
      If(istatMZCER==0) Then                      
        tailMZCER => headMZCER                    
        Nullify(tailMZCER%pMZCER)                 
        tailMZCER%pclineMZCER = ptxtlineMZCER     
      Else
        ! Error message
      End If
    Else
      Allocate(tailMZCER%pMZCER, Stat=istatMZCER)      
      If(istatMZCER==0) Then                           
        tailMZCER=> tailMZCER%pMZCER                   
        Nullify(tailMZCER%pMZCER)                      
        tailMZCER%pclineMZCER = ptxtlineMZCER          
      Else
      ! Error message
      End If
    End If

 End Subroutine LinklstMZCER
!------------------------------------------------------------------------------

 Subroutine LinklstPlNCrGro(ptxtlinePlNCrGro)

    Character(:), Allocatable :: ptxtlinePlNCrGro            
        
    If(.Not. Associated(headPlNCrGro)) Then             
      Allocate(headPlNCrGro, Stat=istatPlNCrGro)        
      If(istatPlNCrGro==0) Then                         
        tailPlNCrGro => headPlNCrGro                    
        Nullify(tailPlNCrGro%pPlNCrGro)                 
        tailPlNCrGro%pclinePlNCrGro = ptxtlinePlNCrGro  
      Else
        ! Error message
      End If
    Else
      Allocate(tailPlNCrGro%pPlNCrGro, Stat=istatPlNCrGro)      
      If(istatPlNCrGro==0) Then                                 
        tailPlNCrGro=> tailPlNCrGro%pPlNCrGro                   
        Nullify(tailPlNCrGro%pPlNCrGro)                         
        tailPlNCrGro%pclinePlNCrGro = ptxtlinePlNCrGro          
      Else
      ! Error message
      End If
    End If

 End Subroutine LinklstPlNCrGro
!------------------------------------------------------------------------------

 Subroutine LinklstPlNCsCer(ptxtlinePlNCsCer)

    Character(:), Allocatable :: ptxtlinePlNCsCER            
        
    If(.Not. Associated(headPlNCsCER)) Then             
      Allocate(headPlNCsCER, Stat=istatPlNCsCER)        
      If(istatPlNCsCER==0) Then                         
        tailPlNCsCER => headPlNCsCER                    
        Nullify(tailPlNCsCER%pPlNCsCER)                 
        tailPlNCsCER%pclinePlNCsCER = ptxtlinePlNCsCER  
      Else
        ! Error message
      End If
    Else
      Allocate(tailPlNCsCER%pPlNCsCER, Stat=istatPlNCsCER)      
      If(istatPlNCsCER==0) Then                                 
        tailPlNCsCER=> tailPlNCsCER%pPlNCsCER                   
        Nullify(tailPlNCsCER%pPlNCsCER)                         
        tailPlNCsCER%pclinePlNCsCER = ptxtlinePlNCsCER          
      Else
      ! Error message
      End If
    End If

 End Subroutine LinklstPlNCsCER
!------------------------------------------------------------------------------

 Subroutine LinklstSoilNi(ptxtlineSoilNi)

    Character(:), Allocatable :: ptxtlineSoilNi            
        
    If(.Not. Associated(headSoilNi)) Then           
      Allocate(headSoilNi, Stat=istatSoilNi)        
      If(istatSoilNi==0) Then                       
        tailSoilNi => headSoilNi                    
        Nullify(tailSoilNi%pSoilNi)                 
        tailSoilNi%pclineSoilNi = ptxtlineSoilNi    
      Else
        ! Error message
      End If
    Else
      Allocate(tailSoilNi%pSoilNi, Stat=istatSoilNi)      
      If(istatSoilNi==0) Then                             
        tailSoilNi=> tailSoilNi%pSoilNi                   
        Nullify(tailSoilNi%pSoilNi)                       
        tailSoilNi%pclineSoilNi = ptxtlineSoilNi           
      Else
      ! Error message
      End If
    End If

 End Subroutine LinklstSoilNi
!------------------------------------------------------------------------------

 Subroutine LinklstPlNMzCer(ptxtlinePlNMzCer)

    Character(:), Allocatable :: ptxtlinePlNMzCER            
        
    If(.Not. Associated(headPlNMzCER)) Then             
      Allocate(headPlNMzCER, Stat=istatPlNMzCER)        
      If(istatPlNMzCER==0) Then                         
        tailPlNMzCER => headPlNMzCER                    
        Nullify(tailPlNMzCER%pPlNMzCER)                 
        tailPlNMzCER%pclinePlNMzCER = ptxtlinePlNMzCER  
      Else
        ! Error message
      End If
    Else
      Allocate(tailPlNMzCER%pPlNMzCER, Stat=istatPlNMzCER)      
      If(istatPlNMzCER==0) Then                                 
        tailPlNMzCER=> tailPlNMzCER%pPlNMzCER                   
        Nullify(tailPlNMzCER%pPlNMzCER)                         
        tailPlNMzCER%pclinePlNMzCER = ptxtlinePlNMzCER           
      Else
      ! Error message
      End If
    End If

 End Subroutine LinklstPlNMzCER
!------------------------------------------------------------------------------

 Subroutine LinklstWth(ptxtlineWth)

    Character(:), Allocatable :: ptxtlineWth            
        
    If(.Not. Associated(headWth)) Then        
      Allocate(headWth, Stat=istatWth)        
      If(istatWth==0) Then                    
        tailWth => headWth                    
        Nullify(tailWth%pWth)                 
        tailWth%pclineWth = ptxtlineWth       
      Else
        ! Error message
      End If
    Else
      Allocate(tailWth%pWth, Stat=istatWth)      
      If(istatWth==0) Then                       
        tailWth=> tailWth%pWth                   
        Nullify(tailWth%pWth)                    
        tailWth%pclineWth = ptxtlineWth          
      Else
      ! Error message
      End If
    End If

 End Subroutine LinklstWth
!------------------------------------------------------------------------------
 Subroutine LinklstPlGr2(ptxtlinePlGr2)

    Character(:), Allocatable :: ptxtlinePlGr2         
        
    If(.Not. Associated(headPlGr2)) Then          
      Allocate(headPlGr2, Stat=istatPlGr2)        
      If(istatPlGr2==0) Then                      
        tailPlGr2 => headPlGr2                    
        Nullify(tailPlGr2%pPlGr2)                 
        tailPlGr2%pclinePlGr2 = ptxtlinePlGr2     
      Else
        ! Error message
      End If
    Else
      Allocate(tailPlGr2%pPlGr2, Stat=istatPlGr2) 
      If(istatPlGr2==0) Then                      
        tailPlGr2=> tailPlGr2%pPlGr2              
        Nullify(tailPlGr2%pPlGr2)                 
        tailPlGr2%pclinePlGr2 = ptxtlinePlGr2     
      Else
      ! Error message
      End If
    End If

 End Subroutine LinklstPlGr2
!------------------------------------------------------------------------------
 Subroutine LinklstPlGrf(ptxtlinePlGrf)

    Character(:), Allocatable :: ptxtlinePlGrf         
        
    If(.Not. Associated(headPlGrf)) Then          
      Allocate(headPlGrf, Stat=istatPlGrf)        
      If(istatPlGrf==0) Then                      
        tailPlGrf => headPlGrf                    
        Nullify(tailPlGrf%pPlGrf)                 
        tailPlGrf%pclinePlGrf = ptxtlinePlGrf     
      Else
        ! Error message
      End If
    Else
      Allocate(tailPlGrf%pPlGrf, Stat=istatPlGrf) 
      If(istatPlGrf==0) Then                      
        tailPlGrf=> tailPlGrf%pPlGrf              
        Nullify(tailPlGrf%pPlGrf)                 
        tailPlGrf%pclinePlGrf = ptxtlinePlGrf     
      Else
      ! Error message
      End If
    End If

 End Subroutine LinklstPlGrf
!------------------------------------------------------------------------------
 Subroutine LinklstEvalCsCer(ptxtlineEvalCsCer)

    Character(:), Allocatable :: ptxtlineEvalCsCer          
        
    If(.Not. Associated(headEvalCsCer)) Then              
      Allocate(headEvalCsCer, Stat=istatEvalCsCer)        
      If(istatEvalCsCer==0) Then                          
        tailEvalCsCer => headEvalCsCer                    
        Nullify(tailEvalCsCer%pEvalCsCer)                 
        tailEvalCsCer%pclineEvalCsCer = ptxtlineEvalCsCer 
      Else
        ! Error message
      End If
    Else
      Allocate(tailEvalCsCer%pEvalCsCer, Stat=istatEvalCsCer) 
      If(istatEvalCsCer==0) Then                              
        tailEvalCsCer=> tailEvalCsCer%pEvalCsCer              
        Nullify(tailEvalCsCer%pEvalCsCer)                     
        tailEvalCsCer%pclineEvalCsCer = ptxtlineEvalCsCer     
      Else
      ! Error message
      End If
    End If

 End Subroutine LinklstEvalCsCer
!------------------------------------------------------------------------------
 Subroutine LinklstEvOpsum(ptxtlineEvOpsum)

    Character(:), Allocatable :: ptxtlineEvOpsum            
        
    If(.Not. Associated(headEvOpsum)) Then            
      Allocate(headEvOpsum, Stat=istatEvOpsum)        
      If(istatEvOpsum==0) Then                        
        tailEvOpsum => headEvOpsum                    
        Nullify(tailEvOpsum%pEvOpsum)                 
        tailEvOpsum%pclineEvOpsum = ptxtlineEvOpsum   
      Else
        ! Error message
      End If
    Else
      Allocate(tailEvOpsum%pEvOpsum, Stat=istatEvOpsum)      
      If(istatEvOpsum==0) Then                               
        tailEvOpsum=> tailEvOpsum%pEvOpsum                   
        Nullify(tailEvOpsum%pEvOpsum)                        
        tailEvOpsum%pclineEvOpsum = ptxtlineEvOpsum          
      Else
      ! Error message
      End If
    End If

 End Subroutine LinklstEvOpsum
!------------------------------------------------------------------------------
 Subroutine LinklstSumOpsum(ptxtlineSumOpsum)

    Character(:), Allocatable :: ptxtlineSumOpsum            
        
    If(.Not. Associated(headSumOpsum)) Then             
      Allocate(headSumOpsum, Stat=istatSumOpsum)        
      If(istatSumOpsum==0) Then                         
        tailSumOpsum => headSumOpsum                    
        Nullify(tailSumOpsum%pSumOpsum)                 
        tailSumOpsum%pclineSumOpsum = ptxtlineSumOpsum  
      Else
        ! Error message
      End If
    Else
      Allocate(tailSumOpsum%pSumOpsum, Stat=istatSumOpsum)   
      If(istatSumOpsum==0) Then                              
        tailSumOpsum=> tailSumOpsum%pSumOpsum                
        Nullify(tailSumOpsum%pSumOpsum)                      
        tailSumOpsum%pclineSumOpsum = ptxtlineSumOpsum       
      Else
      ! Error message
      End If
    End If

 End Subroutine LinklstSumOpsum
!------------------------------------------------------------------------------
   Subroutine Listtofile
      Integer          :: nf       ! Number for growth output file  #
      Character(Len=12):: fn       ! Growth output file code  
      
      fn = 'plantgro.csv'
      Call GETLUN (fn,nf)
   
      OPEN (UNIT = nf,FILE = fn,FORM='FORMATTED', POSITION='APPEND', Action='Write')

      ! write out the data
      ptr => head
      Do
        If(.Not. Associated(ptr)) Exit           ! Pointer valid?
        Write(nf,'(A)') ptr % pcline             ! Yes: Write value
        ptr => ptr % p                           ! Get next pointer
      End Do

      Nullify(ptr, head, tail)
      Close(nf)
  End Subroutine Listtofile
!------------------------------------------------------------------------------

   Subroutine ListtofileSW
      Integer          :: nf       
      Character(Len=12):: fn        
      
      fn = 'soilwat.csv'
      Call GETLUN (fn,nf)
   
      OPEN (UNIT = nf,FILE = fn,FORM='FORMATTED', POSITION='APPEND', Action='Write')

      ptrsw => headsw
      Do
        If(.Not. Associated(ptrsw)) Exit             
        Write(nf,'(A)') ptrsw % pclinesw             
        ptrsw => ptrsw % psw                         
      End Do

      Nullify(ptrsw, headsw, tailsw)
      Close(nf)
   End Subroutine ListtofileSW
!------------------------------------------------------------------------------

   Subroutine ListtofileTemp
      Integer          :: nf       
      Character(Len=12):: fn         
      
      fn = 'soiltemp.csv'
      Call GETLUN (fn,nf)
   
      OPEN (UNIT = nf,FILE = fn,FORM='FORMATTED', POSITION='APPEND', Action='Write')

      ptrtemp => headtemp
      Do
        If(.Not. Associated(ptrtemp)) Exit                 
        Write(nf,'(A)') ptrtemp % pclinetemp               
        ptrtemp => ptrtemp % ptemp                         
      End Do

      Nullify(ptrtemp, headtemp, tailtemp)
      Close(nf)
   End Subroutine ListtofileTemp
!------------------------------------------------------------------------------

   Subroutine ListtofileCsCer
      Integer          :: nf       
      Character(Len=12):: fn         
      
      fn = 'plantgro.csv'
      Call GETLUN (fn,nf)
   
      OPEN (UNIT = nf,FILE = fn,FORM='FORMATTED', POSITION='APPEND', Action='Write')

      ptrCsCer => headCsCer
      Do
        If(.Not. Associated(ptrCsCer)) Exit                
        Write(nf,'(A)') ptrCsCer % pclineCsCer             
        ptrCsCer => ptrCsCer % pCsCer                      
      End Do

      Nullify(ptrCsCer, headCsCer, tailCsCer)
      Close(nf)
   End Subroutine ListtofileCsCer
!------------------------------------------------------------------------------

   Subroutine ListtofileET
      Integer          :: nf       
      Character(Len=12):: fn         
      
      fn = 'ET.csv'
      Call GETLUN (fn,nf)
   
      OPEN (UNIT = nf,FILE = fn,FORM='FORMATTED', POSITION='APPEND', Action='Write')

      ptrET => headET
      Do
        If(.Not. Associated(ptrET)) Exit             
        Write(nf,'(A)') ptrET % pclineET             
        ptrET => ptrET % pET                         
      End Do

      Nullify(ptrET, headET, tailET)
      Close(nf)
  End Subroutine ListtofileET
!------------------------------------------------------------------------------

  Subroutine ListtofileMZCER
      Integer          :: nf       
      Character(Len=12):: fn         
      
      fn = 'plantgro.csv'
      Call GETLUN (fn,nf)
   
      OPEN (UNIT = nf,FILE = fn,FORM='FORMATTED', POSITION='APPEND', Action='Write')

      ptrMZCER => headMZCER
      Do
        If(.Not. Associated(ptrMZCER)) Exit                
        Write(nf,'(A)') ptrMZCER % pclineMZCER            
        ptrMZCER => ptrMZCER % pMZCER                      
      End Do

      Nullify(ptrMZCER, headMZCER, tailMZCER)
      Close(nf)
  End Subroutine ListtofileMZCER
!------------------------------------------------------------------------------

  Subroutine ListtofilePlNCrGro
      Integer          :: nf       
      Character(Len=12):: fn         
      
      fn = 'plantn.csv'
      Call GETLUN (fn,nf)
   
      OPEN (UNIT = nf,FILE = fn,FORM='FORMATTED', POSITION='APPEND', Action='Write')

      ptrPlNCrGro => headPlNCrGro
      Do
        If(.Not. Associated(ptrPlNCrGro)) Exit          
        Write(nf,'(A)') ptrPlNCrGro % pclinePlNCrGro    
        ptrPlNCrGro => ptrPlNCrGro % pPlNCrGro          
      End Do

      Nullify(ptrPlNCrGro, headPlNCrGro, tailPlNCrGro)
      Close(nf)
  End Subroutine ListtofilePlNCrGro
!------------------------------------------------------------------------------

   Subroutine ListtofilePlNCsCer
      Integer          :: nf       
      Character(Len=12):: fn         
      
      fn = 'plantn.csv'
      Call GETLUN (fn,nf)
   
      OPEN (UNIT = nf,FILE = fn,FORM='FORMATTED', POSITION='APPEND', Action='Write')

      ptrPlNCsCer => headPlNCsCer
      Do
        If(.Not. Associated(ptrPlNCsCer)) Exit              
        Write(nf,'(A)') ptrPlNCsCer % pclinePlNCsCer        
        ptrPlNCsCer => ptrPlNCsCer % pPlNCsCer              
      End Do

      Nullify(ptrPlNCsCer, headPlNCsCer, tailPlNCsCer)
      Close(nf)
   End Subroutine ListtofilePlNCsCer
!------------------------------------------------------------------------------

   Subroutine ListtofileSoilNi
      Integer          :: nf       
      Character(Len=12):: fn         
      
      fn = 'soilni.csv'
      Call GETLUN (fn,nf)
   
      OPEN (UNIT = nf,FILE = fn,FORM='FORMATTED', POSITION='APPEND', Action='Write')

      ptrSoilNi => headSoilNi
      Do
        If(.Not. Associated(ptrSoilNi)) Exit                
        Write(nf,'(A)') ptrSoilNi % pclineSoilNi            
        ptrSoilNi => ptrSoilNi % pSoilNi                    
      End Do

      Nullify(ptrSoilNi, headSoilNi, tailSoilNi)
      Close(nf)
   End Subroutine ListtofileSoilNi
!------------------------------------------------------------------------------

   Subroutine ListtofilePlNMzCer
      Integer          :: nf       
      Character(Len=12):: fn         
      
      fn = 'plantn.csv'
      Call GETLUN (fn,nf)
   
      OPEN (UNIT = nf,FILE = fn,FORM='FORMATTED', POSITION='APPEND', Action='Write')

      ptrPlNMzCer => headPlNMzCer
      Do
        If(.Not. Associated(ptrPlNMzCer)) Exit                
        Write(nf,'(A)') ptrPlNMzCer % pclinePlNMzCer          
        ptrPlNMzCer => ptrPlNMzCer % pPlNMzCer                
      End Do

      Nullify(ptrPlNMzCer, headPlNMzCer, tailPlNMzCer)
      Close(nf)
   End Subroutine ListtofilePlNMzCer
!------------------------------------------------------------------------------

   Subroutine ListtofileWth
      Integer          :: nf       
      Character(Len=12):: fn         
      
      fn = 'weather.csv'
      Call GETLUN (fn,nf)
   
      OPEN (UNIT = nf,FILE = fn,FORM='FORMATTED', POSITION='APPEND', Action='Write')

      ptrWth => headWth
      Do
        If(.Not. Associated(ptrWth)) Exit              
        Write(nf,'(A)') ptrWth % pclineWth             
        ptrWth => ptrWth % pWth                       
      End Do

      Nullify(ptrWth, headWth, tailWth)
      Close(nf)
   End Subroutine ListtofileWth
!------------------------------------------------------------------------------
   Subroutine ListtoFilePlGr2
      Integer          :: nf       
      Character(Len=12):: fn         
      
      fn = 'plantgr2.csv'
      Call GETLUN (fn,nf)
   
      OPEN (UNIT = nf,FILE = fn,FORM='FORMATTED', POSITION='APPEND', Action='Write')

      ptrPlGr2 => headPlGr2
      Do
        If(.Not. Associated(ptrPlGr2)) Exit          
        Write(nf,'(A)') ptrPlGr2 % pclinePlGr2       
        ptrPlGr2 => ptrPlGr2 % pPlGr2                
      End Do

      Nullify(ptrPlGr2, headPlGr2, tailPlGr2)
      Close(nf)
   End Subroutine ListtofilePlGr2
!------------------------------------------------------------------------------
   Subroutine ListtoFilePlGrf
      Integer          :: nf       
      Character(Len=12):: fn         
      
      fn = 'plantgrf.csv'
      Call GETLUN (fn,nf)
   
      OPEN (UNIT = nf,FILE = fn,FORM='FORMATTED', POSITION='APPEND', Action='Write')

      ptrPlGrf => headPlGrf
      Do
        If(.Not. Associated(ptrPlGrf)) Exit          
        Write(nf,'(A)') ptrPlGrf % pclinePlGrf       
        ptrPlGrf => ptrPlGrf % pPlGrf                
      End Do

      Nullify(ptrPlGrf, headPlGrf, tailPlGrf)
      Close(nf)
   End Subroutine ListtofilePlGrf
!------------------------------------------------------------------------------
   Subroutine ListtofileEvalCsCer
      Integer          :: nf       
      Character(Len=12):: fn        
      
      fn = 'evaluate.csv'
      Call GETLUN (fn,nf)
   
      OPEN (UNIT = nf,FILE = fn,FORM='FORMATTED', POSITION='APPEND', Action='Write')

      ptrEvalCsCer => headEvalCsCer
      Do
        If(.Not. Associated(ptrEvalCsCer)) Exit                
        Write(nf,'(A)') ptrEvalCsCer % pclineEvalCsCer         
        ptrEvalCsCer => ptrEvalCsCer % pEvalCsCer              !
      End Do

      Nullify(ptrEvalCsCer, headEvalCsCer, tailEvalCsCer)
      Close(nf)
   End Subroutine ListtofileEvalCsCer
!------------------------------------------------------------------------------
   Subroutine ListtofileEvOpsum
      Integer          :: nf       
      Character(Len=12):: fn         
      
      fn = 'evaluate.csv'
      Call GETLUN (fn,nf)
   
      OPEN (UNIT = nf,FILE = fn,FORM='FORMATTED', POSITION='APPEND', Action='Write')

      ptrEvOpsum => headEvOpsum
      Do
        If(.Not. Associated(ptrEvOpsum)) Exit                
        Write(nf,'(A)') ptrEvOpsum % pclineEvOpsum           
        ptrEvOpsum => ptrEvOpsum % pEvOpsum                  
      End Do

      Nullify(ptrEvOpsum, headEvOpsum, tailEvOpsum)
      Close(nf)
   End Subroutine ListtofileEvOpsum
!------------------------------------------------------------------------------
   Subroutine ListtofileSumOpsum
      Integer          :: nf       
      Character(Len=12):: fn         
      
      fn = 'summary.csv'
      Call GETLUN (fn,nf)
   
      OPEN (UNIT = nf,FILE = fn,FORM='FORMATTED', POSITION='APPEND', Action='Write')

      ptrSumOpsum => headSumOpsum
      Do
        If(.Not. Associated(ptrSumOpsum)) Exit                 
        Write(nf,'(A)') ptrSumOpsum % pclineSumOpsum           
        ptrSumOpsum => ptrSumOpsum % pSumOpsum                 
      End Do

      Nullify(ptrSumOpsum, headSumOpsum, tailSumOpsum)
      Close(nf)
   End Subroutine ListtofileSumOpsum
!------------------------------------------------------------------------------

   Subroutine ListtofilePlCCrGro
      Integer          :: nf       
      Character(Len=12):: fn         
      
      fn = 'plantc.csv'
      Call GETLUN (fn,nf)
   
      OPEN (UNIT = nf,FILE = fn,FORM='FORMATTED', POSITION='APPEND', Action='Write')

      ptrPlCCrGro => headPlCCrGro
      Do
        If(.Not. Associated(ptrPlCCrGro)) Exit          
        Write(nf,'(A)') ptrPlCCrGro % pclinePlCCrGro    
        ptrPlCCrGro => ptrPlCCrGro % pPlCCrGro          
      End Do

      Nullify(ptrPlCCrGro, headPlCCrGro, tailPlCCrGro)
      Close(nf)
   End Subroutine ListtofilePlCCrGro
!------------------------------------------------------------------------------

 Subroutine LinklstPlCCrGro(ptxtlinePlCCrGro)
    Character(:), Allocatable :: ptxtlinePlCCrGro            
        
    If(.Not. Associated(headPlCCrGro)) Then             
      Allocate(headPlCCrGro, Stat=istatPlCCrGro)        
      If(istatPlCCrGro==0) Then                         
        tailPlCCrGro => headPlCCrGro                    
        Nullify(tailPlCCrGro%pPlCCrGro)                 
        tailPlCCrGro%pclinePlCCrGro = ptxtlinePlCCrGro  
      Else
        ! Error message
      End If
    Else
      Allocate(tailPlCCrGro%pPlCCrGro, Stat=istatPlCCrGro)  
      If(istatPlCCrGro==0) Then                             
        tailPlCCrGro=> tailPlCCrGro%pPlCCrGro               
        Nullify(tailPlCCrGro%pPlCCrGro)                     
        tailPlCCrGro%pclinePlCCrGro = ptxtlinePlCCrGro      
      Else
      ! Error message
      End If
    End If

 End Subroutine LinklstPlCCrGro
!------------------------------------------------------------------------------
 Subroutine LinklstSoilOrg(ptxtlineSoilOrg)
    Character(:), Allocatable :: ptxtlineSoilOrg            
        
    If(.Not. Associated(headSoilOrg)) Then            
      Allocate(headSoilOrg, Stat=istatSoilOrg)        
      If(istatSoilOrg==0) Then                        
        tailSoilOrg => headSoilOrg                    
        Nullify(tailSoilOrg%pSoilOrg)                 
        tailSoilOrg%pclineSoilOrg = ptxtlineSoilOrg   
      Else
        ! Error message
      End If
    Else
      Allocate(tailSoilOrg%pSoilOrg, Stat=istatSoilOrg)      
      If(istatSoilOrg==0) Then                               
        tailSoilOrg=> tailSoilOrg%pSoilOrg                   
        Nullify(tailSoilOrg%pSoilOrg)                        
        tailSoilOrg%pclineSoilOrg = ptxtlineSoilOrg          
      Else
      ! Error message
      End If
    End If

 End Subroutine LinklstSoilOrg
!------------------------------------------------------------------------------
   Subroutine ListtofileSoilOrg
      Integer          :: nf       
      Character(Len=12):: fn         
      
      fn = 'soilorg.csv'
      Call GETLUN (fn,nf)
   
      OPEN (UNIT = nf,FILE = fn,FORM='FORMATTED', POSITION='APPEND', Action='Write')

      ptrSoilOrg => headSoilOrg
      Do
        If(.Not. Associated(ptrSoilOrg)) Exit          
        Write(nf,'(A)') ptrSoilOrg % pclineSoilOrg     
        ptrSoilOrg => ptrSoilOrg % pSoilOrg            
      End Do

      Nullify(ptrSoilOrg, headSoilOrg, tailSoilOrg)
      Close(nf)
   End Subroutine ListtofileSoilOrg
!------------------------------------------------------------------------------
 Subroutine LinklstETPhot(ptxtlineETPhot)
    Character(:), Allocatable :: ptxtlineETPhot           
        
    If(.Not. Associated(headETPhot)) Then           
      Allocate(headETPhot, Stat=istatETPhot)        
      If(istatETPhot==0) Then                       
        tailETPhot => headETPhot                    
        Nullify(tailETPhot%pETPhot)                 
        tailETPhot%pclineETPhot = ptxtlineETPhot    
      Else
        ! Error message
      End If
    Else
      Allocate(tailETPhot%pETPhot, Stat=istatETPhot)   
      If(istatETPhot==0) Then                          
        tailETPhot=> tailETPhot%pETPhot                
        Nullify(tailETPhot%pETPhot)                    
        tailETPhot%pclineETPhot = ptxtlineETPhot       
      Else
      ! Error message
      End If
    End If

 End Subroutine LinklstETPhot
!------------------------------------------------------------------------------
   Subroutine ListtofileETPhot
      Integer          :: nf       
      Character(Len=12):: fn         
      
      fn = 'etphot.csv'
      Call GETLUN (fn,nf)
   
      OPEN (UNIT = nf,FILE = fn,FORM='FORMATTED', POSITION='APPEND', Action='Write')

      ptrETPhot => headETPhot
      Do
        If(.Not. Associated(ptrETPhot)) Exit          
        Write(nf,'(A)') ptrETPhot % pclineETPhot      
        ptrETPhot => ptrETPhot % pETPhot              
      End Do

      Nullify(ptrETPhot, headETPhot, tailETPhot)
      Close(nf)
   End Subroutine ListtofileETPhot
!------------------------------------------------------------------------------
 Subroutine LinklstMulch(ptxtlineMulch)
    Character(:), Allocatable :: ptxtlineMulch            
        
    If(.Not. Associated(headMulch)) Then          
      Allocate(headMulch, Stat=istatMulch)        
      If(istatMulch==0) Then                      
        tailMulch => headMulch                    
        Nullify(tailMulch%pMulch)                 
        tailMulch%pclineMulch = ptxtlineMulch     
      Else
        ! Error message
      End If
    Else
      Allocate(tailMulch%pMulch, Stat=istatMulch)      
      If(istatMulch==0) Then                           
        tailMulch=> tailMulch%pMulch                   
        Nullify(tailMulch%pMulch)                      
        tailMulch%pclineMulch = ptxtlineMulch          
      Else
      ! Error message
      End If
    End If

 End Subroutine LinklstMulch
!------------------------------------------------------------------------------
  Subroutine ListtofileMulch
      Integer          :: nf       
      Character(Len=12):: fn         
      
      fn = 'mulch.csv'
      Call GETLUN (fn,nf)
   
      OPEN (UNIT = nf,FILE = fn,FORM='FORMATTED', POSITION='APPEND', Action='Write')

      ptrMulch => headMulch
      Do
        If(.Not. Associated(ptrMulch)) Exit          
        Write(nf,'(A)') ptrMulch % pclineMulch       
        ptrMulch => ptrMulch % pMulch                
      End Do

      Nullify(ptrMulch, headMulch, tailMulch)
      Close(nf)
  End Subroutine ListtofileMulch
!------------------------------------------------------------------------------
 Subroutine LinklstPlantP(ptxtlinePlantP)
    Character(:), Allocatable :: ptxtlinePlantP            
        
    If(.Not. Associated(headPlantP)) Then           
      Allocate(headPlantP, Stat=istatPlantP)        
      If(istatPlantP==0) Then                       
        tailPlantP => headPlantP                    
        Nullify(tailPlantP%pPlantP)                 
        tailPlantP%pclinePlantP = ptxtlinePlantP     
      Else
        ! Error message
      End If
    Else
      Allocate(tailPlantP%pPlantP, Stat=istatPlantP)  
      If(istatPlantP==0) Then                         
        tailPlantP=> tailPlantP%pPlantP               
        Nullify(tailPlantP%pPlantP)                   
        tailPlantP%pclinePlantP = ptxtlinePlantP      
      Else
      ! Error message
      End If
    End If

 End Subroutine LinklstPlantP
!------------------------------------------------------------------------------
 Subroutine ListtofilePlantP
    Integer          :: nf       
    Character(Len=12):: fn       

    fn = 'plantp.csv'
    Call GETLUN (fn,nf)

    OPEN (UNIT = nf,FILE = fn,FORM='FORMATTED', POSITION='APPEND', Action='Write')

    ptrPlantP => headPlantP
    Do
    If(.Not. Associated(ptrPlantP)) Exit          
    Write(nf,'(A)') ptrPlantP % pclinePlantP      
    ptrPlantP => ptrPlantP % pPlantP              
    End Do

    Nullify(ptrPlantP, headPlantP, tailPlantP)
    Close(nf)
 End Subroutine ListtofilePlantP
!------------------------------------------------------------------------------
 Subroutine LinklstSoilPi(ptxtlineSoilPi)
    Character(:), Allocatable :: ptxtlineSoilPi           
        
    If(.Not. Associated(headSoilPi)) Then           
      Allocate(headSoilPi, Stat=istatSoilPi)        
      If(istatSoilPi==0) Then                       
        tailSoilPi => headSoilPi                    
        Nullify(tailSoilPi%pSoilPi)                 
        tailSoilPi%pclineSoilPi = ptxtlineSoilPi    
      Else
        ! Error message
      End If
    Else
      Allocate(tailSoilPi%pSoilPi, Stat=istatSoilPi)   
      If(istatSoilPi==0) Then                          
        tailSoilPi=> tailSoilPi%pSoilPi                
        Nullify(tailSoilPi%pSoilPi)                    
        tailSoilPi%pclineSoilPi = ptxtlineSoilPi       
      Else
      ! Error message
      End If
    End If

 End Subroutine LinklstSoilPi
!------------------------------------------------------------------------------
 Subroutine ListtofileSoilPi
    Integer          :: nf       
    Character(Len=12):: fn         

    fn = 'soilpi.csv'
    Call GETLUN (fn,nf)

    OPEN (UNIT = nf,FILE = fn,FORM='FORMATTED', POSITION='APPEND', Action='Write')

    ptrSoilPi => headSoilPi
    Do
    If(.Not. Associated(ptrSoilPi)) Exit          
    Write(nf,'(A)') ptrSoilPi % pclineSoilPi      
    ptrSoilPi => ptrSoilPi % pSoilPi              
    End Do

    Nullify(ptrSoilPi, headSoilPi, tailSoilPi)
    Close(nf)
 End Subroutine ListtofileSoilPi
!------------------------------------------------------------------------------

End Module Linklist