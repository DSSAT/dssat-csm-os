Module Linklist
!
!
Implicit None
Save
 
Integer :: csvICOUNT
Character(Len=6),  Dimension(40) :: csvOLAP    !Labels

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

!   for RICER
    Type :: lin_valueRICER
       Character(:), Allocatable :: pclineRICER
       Type (lin_valueRICER), Pointer :: pRICER
    End Type

    Type (lin_valueRICER), Pointer :: headRICER      
    Type (lin_valueRICER), Pointer :: tailRICER      
    Type (lin_valueRICER), Pointer :: ptrRICER       
    
    Integer :: istatRICER                            

!------------------------------------------------------------------------------
!   for MLCER
    Type :: lin_valueMLCER
       Character(:), Allocatable :: pclineMLCER
       Type (lin_valueMLCER), Pointer :: pMLCER
    End Type

    Type (lin_valueMLCER), Pointer :: headMLCER      
    Type (lin_valueMLCER), Pointer :: tailMLCER      
    Type (lin_valueMLCER), Pointer :: ptrMLCER       
    
    Integer :: istatMLCER                            
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

!   for SUOIL
    Type :: lin_valueSUOIL
       Character(:), Allocatable :: pclineSUOIL
       Type (lin_valueSUOIL), Pointer :: pSUOIL
    End Type

    Type (lin_valueSUOIL), Pointer :: headSUOIL      
    Type (lin_valueSUOIL), Pointer :: tailSUOIL      
    Type (lin_valueSUOIL), Pointer :: ptrSUOIL       
    
    Integer :: istatSUOIL

!------------------------------------------------------------------------------

!   for PlNSUOIL
    Type :: lin_valuePlNSUOIL
       Character(:), Allocatable :: pclinePlNSUOIL
       Type (lin_valuePlNSUOIL), Pointer :: pPlNSUOIL
    End Type

    Type (lin_valuePlNSUOIL), Pointer :: headPlNSUOIL   
    Type (lin_valuePlNSUOIL), Pointer :: tailPlNSUOIL   
    Type (lin_valuePlNSUOIL), Pointer :: ptrPlNSUOIL    
    
    Integer :: istatPlNSUOIL                            
                                     
!!------------------------------------------------------------------------------
!
!!   for PlNRICer
!    Type :: lin_valuePlNRICer
!       Character(:), Allocatable :: pclinePlNRICer
!       Type (lin_valuePlNRICer), Pointer :: pPlNRICer
!    End Type
!
!    Type (lin_valuePlNRICer), Pointer :: headPlNRICer   
!    Type (lin_valuePlNRICer), Pointer :: tailPlNRICer   
!    Type (lin_valuePlNRICer), Pointer :: ptrPlNRICer    
!    
!    Integer :: istatPlNRICer                            
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
!   for PlantGro PRFRM
    Type :: lin_valuePlGroPrFrm
       Character(:), Allocatable :: pclinePlGroPrFrm
       Type (lin_valuePlGroPrFrm), Pointer :: pPlGroPrFrm
    End Type

    Type (lin_valuePlGroPrFrm), Pointer :: headPlGroPrFrm      
    Type (lin_valuePlGroPrFrm), Pointer :: tailPlGroPrFrm      
    Type (lin_valuePlGroPrFrm), Pointer :: ptrPlGroPrFrm       
    
    Integer :: istatPlGroPrFrm 
!------------------------------------------------------------------------------
!   for PlNPrFrm
    Type :: lin_valuePlNPrFrm
       Character(:), Allocatable :: pclinePlNPrFrm
       Type (lin_valuePlNPrFrm), Pointer :: pPlNPrFrm
    End Type

    Type (lin_valuePlNPrFrm), Pointer :: headPlNPrFrm    
    Type (lin_valuePlNPrFrm), Pointer :: tailPlNPrFrm    
    Type (lin_valuePlNPrFrm), Pointer :: ptrPlNPrFrm     
    
    Integer :: istatPlNPrFrm                             
!-------------------------------------------------------------------------------------- 
!   for PlCPrFrm
    Type :: lin_valuePlCPrFrm
       Character(:), Allocatable :: pclinePlCPrFrm
       Type (lin_valuePlCPrFrm), Pointer :: pPlCPrFrm
    End Type

    Type (lin_valuePlCPrFrm), Pointer :: headPlCPrFrm    
    Type (lin_valuePlCPrFrm), Pointer :: tailPlCPrFrm    
    Type (lin_valuePlCPrFrm), Pointer :: ptrPlCPrFrm     
    
    Integer :: istatPlCPrFrm  
!-------------------------------------------------------------------------------------- 
!   for DormPrFrm
    Type :: lin_valueDormPrFrm
       Character(:), Allocatable :: pclineDormPrFrm
       Type (lin_valueDormPrFrm), Pointer :: pDormPrFrm
    End Type

    Type (lin_valueDormPrFrm), Pointer :: headDormPrFrm    
    Type (lin_valueDormPrFrm), Pointer :: tailDormPrFrm    
    Type (lin_valueDormPrFrm), Pointer :: ptrDormPrFrm     
    
    Integer :: istatDormPrFrm 
!--------------------------------------------------------------------------------------
!   for StorPrFrm
    Type :: lin_valueStorPrFrm
       Character(:), Allocatable :: pclineStorPrFrm
       Type (lin_valueStorPrFrm), Pointer :: pStorPrFrm
    End Type

    Type (lin_valueStorPrFrm), Pointer :: headStorPrFrm    
    Type (lin_valueStorPrFrm), Pointer :: tailStorPrFrm    
    Type (lin_valueStorPrFrm), Pointer :: ptrStorPrFrm     
    
    Integer :: istatStorPrFrm 
!--------------------------------------------------------------------------------------
!   for SOM N 
    Type :: lin_valueSomN
       Character(:), Allocatable :: pclineSomN
       Type (lin_valueSomN), Pointer :: pSomN
    End Type

    Type (lin_valueSomN), Pointer :: headSomN    
    Type (lin_valueSomN), Pointer :: tailSomN    
    Type (lin_valueSomN), Pointer :: ptrSomN     
    
    Integer :: istatSomN 
!--------------------------------------------------------------------------------------
!   for SOM C 
    Type :: lin_valueSomC
       Character(:), Allocatable :: pclineSomC
       Type (lin_valueSomC), Pointer :: pSomC
    End Type

    Type (lin_valueSomC), Pointer :: headSomC    
    Type (lin_valueSomC), Pointer :: tailSomC    
    Type (lin_valueSomC), Pointer :: ptrSomC     
    
    Integer :: istatSomC 

!--------------------------------------------------------------------------------------
!   for N2O.csv
    Type :: lin_valueN2O
       Character(:), Allocatable :: pclineN2O
       Type (lin_valueN2O), Pointer :: pN2O
    End Type

    Type (lin_valueN2O), Pointer :: headN2O
    Type (lin_valueN2O), Pointer :: tailN2O    
    Type (lin_valueN2O), Pointer :: ptrN2O     
    
    Integer :: istatN2O

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

 Subroutine LinklstRICER(ptxtlineRICER)

    Character(:), Allocatable :: ptxtlineRICER            
        
    If(.Not. Associated(headRICER)) Then          
      Allocate(headRICER, Stat=istatRICER)        
      If(istatRICER==0) Then                      
        tailRICER => headRICER                    
        Nullify(tailRICER%pRICER)                 
        tailRICER%pclineRICER = ptxtlineRICER     
      Else
        ! Error message
      End If
    Else
      Allocate(tailRICER%pRICER, Stat=istatRICER)      
      If(istatRICER==0) Then                           
        tailRICER=> tailRICER%pRICER                   
        Nullify(tailRICER%pRICER)                      
        tailRICER%pclineRICER = ptxtlineRICER          
      Else
      ! Error message
      End If
    End If

 End Subroutine LinklstRICER
!------------------------------------------------------------------------------
 Subroutine LinklstMLCER(ptxtlineMLCER)

    Character(:), Allocatable :: ptxtlineMLCER            
        
    If(.Not. Associated(headMLCER)) Then          
      Allocate(headMLCER, Stat=istatMLCER)        
      If(istatMLCER==0) Then                      
        tailMLCER => headMLCER                    
        Nullify(tailMLCER%pMLCER)                 
        tailMLCER%pclineMLCER = ptxtlineMLCER     
      Else
        ! Error message
      End If
    Else
      Allocate(tailMLCER%pMLCER, Stat=istatMLCER)      
      If(istatMLCER==0) Then                           
        tailMLCER=> tailMLCER%pMLCER                   
        Nullify(tailMLCER%pMLCER)                      
        tailMLCER%pclineMLCER = ptxtlineMlCER          
      Else
      ! Error message
      End If
    End If

 End Subroutine LinklstMLCER
!------------------------------------------------------------------------------
 Subroutine LinklstSUOIL(ptxtlineSUOIL)

    Character(:), Allocatable :: ptxtlineSUOIL            
        
    If(.Not. Associated(headSUOIL)) Then          
      Allocate(headSUOIL, Stat=istatSUOIL)        
      If(istatSUOIL==0) Then                      
        tailSUOIL => headSUOIL                    
        Nullify(tailSUOIL%pSUOIL)                 
        tailSUOIL%pclineSUOIL = ptxtlineSUOIL
      Else
        ! Error message
      End If

    Else
      Allocate(tailSUOIL%pSUOIL, Stat=istatSUOIL)      
      If(istatSUOIL==0) Then                           
        tailSUOIL=> tailSUOIL%pSUOIL                   
        Nullify(tailSUOIL%pSUOIL)                      
        tailSUOIL%pclineSUOIL = ptxtlineSUOIL                
      Else
      ! Error message
      End If
    End If
    
End Subroutine LinklstSUOIL     
!------------------------------------------------------------------------------
 Subroutine LinklstPlNSUOIL(ptxtlinePlNSUOIL)

    Character(:), Allocatable :: ptxtlinePlNSUOIL            
        
    If(.Not. Associated(headPlNSUOIL)) Then             
      Allocate(headPlNSUOIL, Stat=istatPlNSUOIL)        
      If(istatPlNSUOIL==0) Then                         
        tailPlNSUOIL => headPlNSUOIL                    
        Nullify(tailPlNSUOIL%pPlNSUOIL)                 
        tailPlNSUOIL%pclinePlNSUOIL = ptxtlinePlNSUOIL  
      Else
        ! Error message
      End If
    Else
      Allocate(tailPlNSUOIL%pPlNSUOIL, Stat=istatPlNSUOIL)      
      If(istatPlNSUOIL==0) Then                                 
        tailPlNSUOIL=> tailPlNSUOIL%pPlNSUOIL                   
        Nullify(tailPlNSUOIL%pPlNSUOIL)                         
        tailPlNSUOIL%pclinePlNSUOIL = ptxtlinePlNSUOIL           
      Else
      ! Error message
      End If
    End If

 End Subroutine LinklstPlNSUOIL
 
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
!!------------------------------------------------------------------------------
!
! Subroutine LinklstPlNRICer(ptxtlinePlNRICer)
!
!    Character(:), Allocatable :: ptxtlinePlNRICER            
!        
!    If(.Not. Associated(headPlNRICER)) Then             
!      Allocate(headPlNRICER, Stat=istatPlNRICER)        
!      If(istatPlNRICER==0) Then                         
!        tailPlNRICER => headPlNRICER                    
!        Nullify(tailPlNRICER%pPlNRICER)                 
!        tailPlNRICER%pclinePlNRICER = ptxtlinePlNRICER  
!      Else
!        ! Error message
!      End If
!    Else
!      Allocate(tailPlNRICER%pPlNRICER, Stat=istatPlNRICER)      
!      If(istatPlNRICER==0) Then                                 
!        tailPlNRICER=> tailPlNRICER%pPlNRICER                   
!        Nullify(tailPlNRICER%pPlNRICER)                         
!        tailPlNRICER%pclinePlNRICER = ptxtlinePlNRICER           
!      Else
!      ! Error message
!      End If
!    End If
!
! End Subroutine LinklstPlNRICER
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
   Subroutine ListtofilePlantgrCrGro(nlayers)
      EXTERNAL GETLUN
      Integer          :: nf       ! Number for growth output file  #
      Character(Len=12):: fn       ! Growth output file code  
      Character(Len=14) :: fmt
      Character(Len=2) :: numtoch1, numtoch2 
      Character(Len=220) :: tmp
      Character(:),Allocatable :: Header 
      Integer :: ErrNum, length, nlayers, i, nl
      
      If(.Not. Associated(head)) Return

      nl = MIN(10, MAX(4,nlayers))
  
      Write(numtoch1,'(I2)') nl - 1  
       
      fmt = '('//Trim(Adjustl(numtoch1))//'(A2,I1,A2))'
      fmt = Trim(Adjustl(fmt))
   
      Write (tmp,fmt) ("RL",i,"D,",i=1,nl - 1)
      tmp = Trim(Adjustl(tmp)) 
      Write(numtoch2,'(I2)') nl  
      tmp = Trim(Adjustl(tmp)) // "RL" // Trim(Adjustl(numtoch2)) // "D" 
       
  length= Len('RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,DAP,L#SD,GSTD,LAID,' &
  //'LWAD,SWAD,GWAD,LIWAM,LINTP,' &
  //'RWAD,VWAD,CWAD,G#AD,GWGD,HIAD,PWAD,P#AD,WSPD,WSGD,NSTD,' &
  //'PST1A,PST2A,KSTD,EWSD,LN%D,SH%D,HIPD,PWDD,PWTD,SLAD,CHTD,CWID,NWAD,RDPD,')&
  + Len('SNW0C,SNW1C,') + Len(Trim(Adjustl(tmp)))

      Allocate(character(LEN=length) :: Header)

  Header = 'RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,DAP,L#SD,GSTD,LAID,' &
  //'LWAD,SWAD,GWAD,LIWAM,LINTP,' &
  //'RWAD,VWAD,CWAD,G#AD,GWGD,HIAD,PWAD,P#AD,WSPD,WSGD,NSTD,' &
  //'PST1A,PST2A,KSTD,EWSD,LN%D,SH%D,HIPD,PWDD,PWTD,SLAD,CHTD,CWID,NWAD,RDPD,'&
  // 'SNW0C,SNW1C,' // Trim(Adjustl(tmp))
         
      fn = 'plantgro.csv'
      Call GETLUN (fn,nf)
  
      Open (UNIT = nf, FILE = fn, FORM='FORMATTED', STATUS = 'REPLACE',  &
          Action='Write', IOSTAT = ErrNum)
        
      Write(nf,'(A)')Header
      Deallocate(Header)

      ! write out the data
      ptr => head
      Do
        If(.Not. Associated(ptr)) Exit           ! Pointer valid?
        Write(nf,'(A)') ptr % pcline             ! Yes: Write value
        ptr => ptr % p                           ! Get next pointer
      End Do

      Nullify(ptr, head, tail)
      Close(nf)
  End Subroutine ListtofilePlantgrCrGro
!------------------------------------------------------------------------------

   Subroutine ListtofileSW(nlayers)
      EXTERNAL GETLUN
      Integer          :: nf, ErrNum, length, nlayers, i, nl       
      Character(Len=12):: fn
      Character(Len=14) :: fmt
      Character(Len=2) :: numtoch1, numtoch2 
      Character(Len=220) :: tmp
      Character(:),Allocatable :: Header 
      
      If(.Not. Associated(headsw)) Return
      
      nl = MIN(10, MAX(4,nlayers))
  
      Write(numtoch1,'(I2)') nl - 1  
       
      fmt = '('//Trim(Adjustl(numtoch1))//'(A2,I1,A2))'
      fmt = Trim(Adjustl(fmt))
   
      Write (tmp,fmt) ("SW",i,"D,",i=1,nl - 1)
      tmp = Trim(Adjustl(tmp)) 
      Write(numtoch2,'(I2)') nl  
      tmp = Trim(Adjustl(tmp)) // "SW" // Trim(Adjustl(numtoch2)) // "D"  
       
  length= Len('RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,SWTD,SWXD,ROFC,DRNC,' &
  //'PREC,IR#C,IRRC,DTWT,MWTD,TDFD,TDFC,ROFD,') + Len(Trim(Adjustl(tmp)))

      Allocate(character(LEN=length) :: Header)

  Header = 'RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,SWTD,SWXD,ROFC,DRNC,' &
  //'PREC,IR#C,IRRC,DTWT,MWTD,TDFD,TDFC,ROFD,' // Trim(Adjustl(tmp)) 
  
      fn = 'soilwat.csv'
      Call GETLUN (fn,nf)
   
      Open (UNIT = nf, FILE = fn, FORM='FORMATTED', STATUS = 'REPLACE', &
          Action='Write', IOSTAT = ErrNum)
        
      Write(nf,'(A)')Header
      Deallocate(Header)
       
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

   Subroutine ListtofileTemp(nlayers)
      EXTERNAL GETLUN
      Integer          :: nf, ErrNum, length, nlayers, i, nl       
      Character(Len=12):: fn         
      Character(Len=14) :: fmt
      Character(Len=2) :: numtoch1, numtoch2 
      Character(Len=220) :: tmp
      Character(:),Allocatable :: Header 
      
      If(.Not. Associated(headtemp)) Return
      
      nl = MIN(10, MAX(4,nlayers))
  
       Write(numtoch1,'(I2)') nl - 1  
       
       fmt = '('//Trim(Adjustl(numtoch1))//'(A2,I1,A2))'
       fmt = Trim(Adjustl(fmt))
   
       Write (tmp,fmt) ("TS",i,"D,",i=1,nl - 1)
       tmp = Trim(Adjustl(tmp)) 
       Write(numtoch2,'(I2)') nl  
       tmp = Trim(Adjustl(tmp)) // "TS" // Trim(Adjustl(numtoch2)) // "D" 
       
  length= Len('RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,TS0D,') + &
          Len(Trim(Adjustl(tmp)))
  
       Allocate(character(LEN=length) :: Header)

  Header = 'RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,TS0D,' // & 
            Trim(Adjustl(tmp))
             
      fn = 'soiltemp.csv'
      Call GETLUN (fn,nf)
   
      Open (UNIT = nf, FILE = fn, FORM='FORMATTED', STATUS = 'REPLACE', &
          Action='Write', IOSTAT = ErrNum)
        
      Write(nf,'(A)')Header
      Deallocate(Header)

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

   Subroutine ListtofilePlantGrCsCer
      EXTERNAL GETLUN
      Integer          :: nf, ErrNum, length       
      Character(Len=12):: fn 
      Character(:),Allocatable :: Header        
      
      If(.Not. Associated(headCsCer)) Return

  length= Len('RUN,EXP,TRTNUM,ROTNUM,REPNO,ON,REP,CN,YEAR,DOY,DAS,DAP,' &
    //'TMEAN,TKILL,GSTD,' &
    //'L#SD,PARID,PARUD,AWAD,LAID,SAID,CAID,TWAD,SDWAD,RWAD,CWAD,LWAD,SWAD,' &
    //'HWAD,HIAD,CHWAD,EWAD,RSWAD,SNWPD,SNWLD,SNWSD,RS%D,H#AD,HWUD,T#AD,SLAD,' &
    //'RDPD,PTFD,SWXD,WAVRD,WUPRD,WFTD,WFPD,WFGD,NFTD,NFPD,NFGD,NUPRD,TFPD,' &
    //'TFGD,VRNFD,DYLFD')
  
       Allocate(character(LEN=length) :: Header)

  Header = 'RUN,EXP,TRTNUM,ROTNUM,REPNO,ON,REP,CN,YEAR,DOY,DAS,DAP,' &
    //'TMEAN,TKILL,GSTD,' &
    //'L#SD,PARID,PARUD,AWAD,LAID,SAID,CAID,TWAD,SDWAD,RWAD,CWAD,LWAD,SWAD,' &
    //'HWAD,HIAD,CHWAD,EWAD,RSWAD,SNWPD,SNWLD,SNWSD,RS%D,H#AD,HWUD,T#AD,SLAD,' &
    //'RDPD,PTFD,SWXD,WAVRD,WUPRD,WFTD,WFPD,WFGD,NFTD,NFPD,NFGD,NUPRD,TFPD,' &
    //'TFGD,VRNFD,DYLFD' 
          
      fn = 'plantgro.csv'
      Call GETLUN (fn,nf)
   
      Open (UNIT = nf, FILE = fn, FORM='FORMATTED', STATUS = 'REPLACE', &
          Action='Write', IOSTAT = ErrNum)
        
      Write(nf,'(A)')Header
      Deallocate(Header)
       
      ptrCsCer => headCsCer
      Do
        If(.Not. Associated(ptrCsCer)) Exit                
        Write(nf,'(A)') ptrCsCer % pclineCsCer             
        ptrCsCer => ptrCsCer % pCsCer                      
      End Do

      Nullify(ptrCsCer, headCsCer, tailCsCer)
      Close(nf)
   End Subroutine ListtofilePlantGrCsCer
!------------------------------------------------------------------------------

   Subroutine ListtofileET (nlayers)
      EXTERNAL GETLUN
      Integer          :: nf, ErrNum, length, nlayers, i, nl        
      Character(Len=12):: fn         
      Character(Len=14) :: fmt
      Character(Len=2) :: numtoch1, numtoch2 
      Character(Len=220) :: tmp
      Character(:),Allocatable :: Header 
       
      If(.Not. Associated(headET)) Return
      
!      nl = MIN(10, MAX(4,nlayers))
      nl = nlayers
      If (nl < 11) then
          Write(numtoch1,'(I2)') nl - 1  
           
          fmt = '('//Trim(Adjustl(numtoch1))//'(A2,I1,A2))'
          fmt = Trim(Adjustl(fmt))
       
          Write (tmp,fmt) ("ES",i,"D,",i=1,nl - 1)
          tmp = Trim(Adjustl(tmp)) 
          Write(numtoch2,'(I2)') nl  
          tmp = Trim(Adjustl(tmp)) // "ES" // Trim(Adjustl(numtoch2)) // "D" 
      Else
          fmt = '(9(A2,I1,A2))'
          Write (tmp,fmt) ("ES",i,"D,",i=1,9)
          tmp = Trim(Adjustl(tmp)) 
          tmp = Trim(Adjustl(tmp)) // "ES10D" 
      End If    
     
  length= Len('RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,SRAA,TMAXA,TMINA,' &
  //'REFA,EOAA,EOPA,EOSA,ETAA,EPAA,ESAA,EFAA,EMAA,EOAC,ETAC,EPAC,ESAC,EFAC,' &
  //'EMAC,KCAA,KBSA,KEAA,TRWUD,TWUPD,') + Len(Trim(Adjustl(tmp)))
  
       Allocate(character(LEN=length) :: Header)

  Header = 'RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,SRAA,TMAXA,TMINA,' &
  //'REFA,EOAA,EOPA,EOSA,ETAA,EPAA,ESAA,EFAA,EMAA,EOAC,ETAC,EPAC,ESAC,EFAC,' &
  //'EMAC,KCAA,KBSA,KEAA,TRWUD,TWUPD,' // Trim(Adjustl(tmp)) 
  
      fn = 'et.csv'
      Call GETLUN (fn,nf)
   
      Open (UNIT = nf, FILE = fn, FORM='FORMATTED', STATUS = 'REPLACE', &
          Action='Write', IOSTAT = ErrNum)
        
      Write(nf,'(A)')Header
      Deallocate(Header)
       
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

  Subroutine ListtofileMZCER(nlayers)
      EXTERNAL GETLUN
      Integer          :: nf, ErrNum, length, nlayers, i, nl       
      Character(Len=12):: fn 
      Character(Len=14) :: fmt
      Character(Len=2) :: numtoch1, numtoch2 
      Character(Len=100) :: tmp
      Character(:),Allocatable :: Header 
      
      If(.Not. Associated(headMZCER)) Return

      nl = MIN(10, MAX(4,nlayers))
  
      Write(numtoch1,'(I2)') nl - 1  
       
      fmt = '('//Trim(Adjustl(numtoch1))//'(A2,I1,A2))'
      fmt = Trim(Adjustl(fmt))
   
      Write (tmp,fmt) ("RL",i,"D,",i=1,nl - 1)
      tmp = Trim(Adjustl(tmp)) 
      Write(numtoch2,'(I2)') nl  
      tmp = Trim(Adjustl(tmp)) // "RL" // Trim(Adjustl(numtoch2)) // "D" 
       
  length= Len('RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,DAP,L#SD,GSTD,LAID,LWAD,SWAD,' &
  //'GWAD,RWAD,VWAD,CWAD,G#AD,GWGD,HIAD,PWAD,P#AD,WSPD,WSGD,NSTD,EWSD,PST1A,' &
  //'PST2A,KSTD,LN%D,SH%D,HIPD,PWDD,PWTD,SLAD,CHTD,CWID,RDPD,'&   
  //'CDAD,LDAD,SDAD,SNW0C,SNW1C,DTTD,')+ Len(Trim(Adjustl(tmp)))

      Allocate(character(LEN=length) :: Header)

  Header = 'RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,DAP,L#SD,GSTD,LAID,LWAD,SWAD,' &
  //'GWAD,RWAD,VWAD,CWAD,G#AD,GWGD,HIAD,PWAD,P#AD,WSPD,WSGD,NSTD,EWSD,PST1A,' &
  //'PST2A,KSTD,LN%D,SH%D,HIPD,PWDD,PWTD,SLAD,CHTD,CWID,RDPD,'&   
  //'CDAD,LDAD,SDAD,SNW0C,SNW1C,DTTD,' // Trim(Adjustl(tmp)) 
        
      fn = 'plantgro.csv'
      Call GETLUN (fn,nf)
   
      Open (UNIT = nf, FILE = fn, FORM='FORMATTED', STATUS = 'REPLACE', &
          Action='Write', IOSTAT = ErrNum)
        
      Write(nf,'(A)')Header
      Deallocate(Header)

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

  Subroutine ListtofileRICER(nlayers)
      EXTERNAL GETLUN
      Integer          :: nf, ErrNum, length, nlayers, i, nl       
      Character(Len=12):: fn 
      Character(Len=14) :: fmt
      Character(Len=2) :: numtoch1, numtoch2 
      Character(Len=100) :: tmp
      Character(:),Allocatable :: Header 
      
      If(.Not. Associated(headRICER)) Return

      nl = MIN(10, MAX(4,nlayers))
  
      Write(numtoch1,'(I2)') nl - 1  
       
      fmt = '('//Trim(Adjustl(numtoch1))//'(A2,I1,A2))'
      fmt = Trim(Adjustl(fmt))
   
      Write (tmp,fmt) ("RL",i,"D,",i=1,nl - 1)
      tmp = Trim(Adjustl(tmp)) 
      Write(numtoch2,'(I2)') nl  
      tmp = Trim(Adjustl(tmp)) // "RL" // Trim(Adjustl(numtoch2)) // "D" 
       
  length= Len('RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,DAP,L#SD,GSTD,LAID,LWAD,SWAD,' &
  //'GWAD,RWAD,EWAD,CWAD,G#AD,GWGD,HIAD,T#AD,WSPD,WSGD,EWSD,NSTD,KSTD,LN%D,' &
  //'SH%D,SLAD,CHTD,CWID,RDPD,'&   
  //'RL1D,RL2D,RL3D,RL4D,RL5D,SNW0C,SNW1C,DTTC,')+ Len(Trim(Adjustl(tmp)))

      Allocate(character(LEN=length) :: Header)

  Header = 'RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,DAP,L#SD,GSTD,LAID,LWAD,SWAD,' &
  //'GWAD,RWAD,EWAD,CWAD,G#AD,GWGD,HIAD,T#AD,WSPD,WSGD,EWSD,NSTD,KSTD,LN%D,' &
  //'SH%D,SLAD,CHTD,CWID,RDPD,'&   
  //'RL1D,RL2D,RL3D,RL4D,RL5D,SNW0C,SNW1C,DTTC,' // Trim(Adjustl(tmp)) 
        
      fn = 'plantgro.csv'
      Call GETLUN (fn,nf)
   
      Open (UNIT = nf, FILE = fn, FORM='FORMATTED', STATUS = 'REPLACE', &
          Action='Write', IOSTAT = ErrNum)
        
      Write(nf,'(A)')Header
      Deallocate(Header)

      ptrRICER => headRICER
      Do
        If(.Not. Associated(ptrRICER)) Exit                
        Write(nf,'(A)') ptrRICER % pclineRICER            
        ptrRICER => ptrRICER % pRICER                      
      End Do

      Nullify(ptrRICER, headRICER, tailRICER)
      Close(nf)
  End Subroutine ListtofileRICER
!------------------------------------------------------------------------------

  Subroutine ListtofileMLCER(nlayers)
      EXTERNAL GETLUN
      Integer          :: nf, ErrNum, length, nlayers, i, nl       
      Character(Len=12):: fn 
      Character(Len=14) :: fmt
      Character(Len=2) :: numtoch1, numtoch2 
      Character(Len=100) :: tmp
      Character(:),Allocatable :: Header 
      
      If(.Not. Associated(headMLCER)) Return

      nl = MIN(10, MAX(4,nlayers))
  
      Write(numtoch1,'(I2)') nl - 1  
       
      fmt = '('//Trim(Adjustl(numtoch1))//'(A2,I1,A2))'
      fmt = Trim(Adjustl(fmt))
   
      Write (tmp,fmt) ("RL",i,"D,",i=1,nl - 1)
      tmp = Trim(Adjustl(tmp)) 
      Write(numtoch2,'(I2)') nl  
      tmp = Trim(Adjustl(tmp)) // "RL" // Trim(Adjustl(numtoch2)) // "D" 
       
  length= Len('RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,DAP,L#SD,GSTD,LAID,LWAD,SWAD,' &
  //'GWAD,RWAD,VWAD,CWAD,G#AD,GWGD,HIAD,PWAD,P#AD,WSPD,WSGD,NSTD,EWSD,PST1A,' &
  //'PST2A,KSTD,LN%D,SH%D,HIPD,PWDD,PWTD,SLAD,CHTD,CWID,RDPD,'&
  //'MLAG1,TLAG1,MPLAG,TPLAG,MPLA,TPLA,PLA,MLFWT,TLFWT,MSTMWT,TSTMWT,AMLWT,ATLWT,' &   
  //'CDAD,LDAD,SDAD,SNW0C,SNW1C,DTTD,')+ Len(Trim(Adjustl(tmp)))

      Allocate(character(LEN=length) :: Header)

  Header = 'RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,DAP,L#SD,GSTD,LAID,LWAD,SWAD,' &
  //'GWAD,RWAD,VWAD,CWAD,G#AD,GWGD,HIAD,PWAD,P#AD,WSPD,WSGD,NSTD,EWSD,PST1A,' &
  //'PST2A,KSTD,LN%D,SH%D,HIPD,PWDD,PWTD,SLAD,CHTD,CWID,RDPD,'&
  //'MLAG1,TLAG1,MPLAG,TPLAG,MPLA,TPLA,PLA,MLFWT,TLFWT,MSTMWT,TSTMWT,AMLWT,ATLWT,'&    
  //'CDAD,LDAD,SDAD,SNW0C,SNW1C,DTTD,' // Trim(Adjustl(tmp)) 
        
      fn = 'plantgro.csv'
      Call GETLUN (fn,nf)
   
      Open (UNIT = nf, FILE = fn, FORM='FORMATTED', STATUS = 'REPLACE', &
          Action='Write', IOSTAT = ErrNum)
        
      Write(nf,'(A)')Header
      Deallocate(Header)

      ptrMLCER => headMLCER
      Do
        If(.Not. Associated(ptrMLCER)) Exit                
        Write(nf,'(A)') ptrMLCER % pclineMLCER            
        ptrMLCER => ptrMLCER % pMLCER                      
      End Do

      Nullify(ptrMLCER, headMLCER, tailMLCER)
      Close(nf)
  End Subroutine ListtofileMLCER
!-----------------------------------------------------------------------  
  Subroutine ListtofileSUOIL(nlayers)
      EXTERNAL GETLUN
      Integer          :: nf, ErrNum, length, nlayers, i, nl       
      Character(Len=12):: fn 
      Character(Len=14) :: fmt
      Character(Len=2) :: numtoch1, numtoch2 
      Character(Len=100) :: tmp
      Character(:),Allocatable :: Header 
      
      If(.Not. Associated(headSUOIL)) Return

      nl = MIN(10, MAX(4,nlayers))
  
      Write(numtoch1,'(I2)') nl - 1  
       
      fmt = '('//Trim(Adjustl(numtoch1))//'(A2,I1,A2))'
      fmt = Trim(Adjustl(fmt))
   
      Write (tmp,fmt) ("RL",i,"D,",i=1,nl - 1)
      tmp = Trim(Adjustl(tmp)) 
      Write(numtoch2,'(I2)') nl  
      tmp = Trim(Adjustl(tmp)) // "RL" // Trim(Adjustl(numtoch2)) // "D" 
       
  length= Len('RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,DAP,L#SD,GSTD,LAID,LWAD,SWAD,' &
  //'GWAD,RWAD,VWAD,CWAD,G#AD,GWGD,HIAD,PWAD,P#AD,WSPD,WSGD,NSTD,EWSD,PST1A,' &
  //'PST2A,KSTD,LN%D,SH%D,HIPD,PWDD,PWTD,SLAD,CHTD,CWID,RDPD,'&   
  //'CDAD,LDAD,SDAD,SNW0C,SNW1C,DTTD,')+ Len(Trim(Adjustl(tmp)))

      Allocate(character(LEN=length) :: Header)

  Header = 'RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,DAP,L#SD,GSTD,LAID,LWAD,SWAD,' &
  //'GWAD,RWAD,VWAD,CWAD,G#AD,GWGD,HIAD,PWAD,P#AD,WSPD,WSGD,NSTD,EWSD,PST1A,' &
  //'PST2A,KSTD,LN%D,SH%D,HIPD,PWDD,PWTD,SLAD,CHTD,CWID,RDPD,'&   
  //'CDAD,LDAD,SDAD,SNW0C,SNW1C,DTTD,' // Trim(Adjustl(tmp)) 
        
      fn = 'plantgro.csv'
      Call GETLUN (fn,nf)
   
      Open (UNIT = nf, FILE = fn, FORM='FORMATTED', STATUS = 'REPLACE', &
          Action='Write', IOSTAT = ErrNum)
        
      Write(nf,'(A)')Header
      Deallocate(Header)

      ptrSUOIL => headSUOIL
      Do
        If(.Not. Associated(ptrSUOIL)) Exit                
        Write(nf,'(A)') ptrSUOIL % pclineSUOIL            
        ptrSUOIL => ptrSUOIL % pSUOIL                      
      End Do

      Nullify(ptrSUOIL, headSUOIL, tailSUOIL)
      Close(nf)
  End Subroutine ListtofileSUOIL
!------------------------------------------------------------------------------
   Subroutine ListtofilePlNSUOIL
      EXTERNAL GETLUN
      Integer          :: nf, ErrNum, length        
      Character(Len=12):: fn 
      Character(:),Allocatable :: Header         
  
      If(.Not. Associated(headPlNSUOIL)) Return
      
      length= Len('RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,DAP,CNAD,GNAD,VNAD,GN%D,VN%D,' &
  //'NUPC,LNAD,SNAD,LN%D,SN%D,RN%D,SNN0C,SNN1C') 

      Allocate(character(LEN=length) :: Header)

  Header = 'RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,DAP,CNAD,GNAD,VNAD,GN%D,VN%D,' &
  //'NUPC,LNAD,SNAD,LN%D,SN%D,RN%D,SNN0C,SNN1C' 
  
      fn = 'plantn.csv'
      Call GETLUN (fn,nf)
   
      Open (UNIT = nf, FILE = fn, FORM='FORMATTED', STATUS = 'REPLACE', &
        Action='Write', IOSTAT = ErrNum)
        
      Write(nf,'(A)')Header
      Deallocate(Header)
      
      ptrPlNSUOIL => headPlNSUOIL
      Do
        If(.Not. Associated(ptrPlNSUOIL)) Exit                
        Write(nf,'(A)') ptrPlNSUOIL % pclinePlNSUOIL          
        ptrPlNSUOIL => ptrPlNSUOIL % pPlNSUOIL                
      End Do

      Nullify(ptrPlNSUOIL, headPlNSUOIL, tailPlNSUOIL)
      Close(nf)
   End Subroutine ListtofilePlNSUOIL      
!------------------------------------------------------------------------------
  Subroutine ListtofilePlNCrGro
      EXTERNAL GETLUN
      Integer          :: nf, ErrNum, length       
      Character(Len=12):: fn
      Character(:),Allocatable :: Header         
      
      If(.Not. Associated(headPlNCrGro)) Return
      
      length= Len('RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,DAP,CNAD,GNAD,VNAD,GN%D,' &
  //'VN%D,NFXC,NUPC,LNAD,SNAD,LN%D,SN%D,SHND,RN%D,NFXD,SNN0C,SNN1C')
  
      Allocate(character(LEN=length) :: Header)

  Header = 'RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,DAP,CNAD,GNAD,VNAD,GN%D,' &
  //'VN%D,NFXC,NUPC,LNAD,SNAD,LN%D,SN%D,SHND,RN%D,NFXD,SNN0C,SNN1C' 
  
      fn = 'plantn.csv'
      Call GETLUN (fn,nf)

      Open (UNIT = nf, FILE = fn, FORM='FORMATTED', STATUS = 'REPLACE', &
          IOSTAT = ErrNum)
        
      Write(nf,'(A)')Header
      Deallocate(Header)    

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
      EXTERNAL GETLUN
      Integer          :: nf, ErrNum, length       
      Character(Len=12):: fn
      Character(:),Allocatable :: Header         
      
      If(.Not. Associated(headPlNCsCer)) Return
      
        length= Len('RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,DAP,TMEAN,GSTD,NUAC,TNAD,SDNAD,' &
  //'RNAD,CNAD,LNAD,SNAD,HNAD,HIND,RSNAD,SNNPD,SNN0D,SNN1D,RN%D,LN%D,SN%D,' &
  //'HN%D,SDN%D,VN%D,LN%RD,SN%RD,RN%RD,VCN%,VMN%,NUPRD,NDEMD')

      Allocate(character(LEN=length) :: Header)

  Header = 'RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,DAP,TMEAN,GSTD,NUAC,TNAD,SDNAD,' &
  //'RNAD,CNAD,LNAD,SNAD,HNAD,HIND,RSNAD,SNNPD,SNN0D,SNN1D,RN%D,LN%D,SN%D,' &
  //'HN%D,SDN%D,VN%D,LN%RD,SN%RD,RN%RD,VCN%,VMN%,NUPRD,NDEMD'
  
      fn = 'plantn.csv'
      Call GETLUN (fn,nf)
   
      Open (UNIT = nf, FILE = fn, FORM='FORMATTED', STATUS = 'REPLACE', &
        Action='Write', IOSTAT = ErrNum)
        
      Write(nf,'(A)')Header
      Deallocate(Header)
      
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
   Subroutine ListtofileSoilNi()  !(nlayers)
      EXTERNAL GETLUN
      Integer          :: nf, ErrNum, length, i, nl   !, nlayers    
      Character(Len=12):: fn
      Character(Len=14) :: fmt
      Character(Len=2) :: numtoch1, numtoch2 
      Character(Len=100) :: tmp, tmp1
      Character(:),Allocatable :: Header 
      
      If(.Not. Associated(headSoilNi)) Return

!      nl = MIN(10, MAX(4,nlayers))
      nl = 10   ! always for 10 layers
      
      Write(numtoch1,'(I2)') nl - 1  
       
      fmt = '('//Trim(Adjustl(numtoch1))//'(A2,I1,A2))'
      fmt = Trim(Adjustl(fmt))
   
      Write (tmp,fmt) ("NI",i,"D,",i=1,nl - 1)
      tmp = Trim(Adjustl(tmp)) 
      Write(numtoch2,'(I2)') nl  
      tmp = Trim(Adjustl(tmp)) // "NI" // Trim(Adjustl(numtoch2)) // "D," 
      
      Write (tmp1,fmt) ("NH",i,"D,",i=1,nl - 1)
      tmp1 = Trim(Adjustl(tmp1)) 
      Write(numtoch2,'(I2)') nl  
      tmp1 = Trim(Adjustl(tmp1)) // "NH" // Trim(Adjustl(numtoch2)) // "D,"
      
     
  length= Len('RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,NAPC,NI#M,NLCC,NIAD,NITD,NHTD,') + &
          Len(Trim(Adjustl(tmp)))  + Len(Trim(Adjustl(tmp1))) + &
          Len('NMNC,NITC,NDNC,NIMC,AMLC,NNMNC,NUCM')

      Allocate(character(LEN=length) :: Header)

  Header = 'RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,NAPC,NI#M,NLCC,NIAD,NITD,NHTD,' &
  // Trim(Adjustl(tmp)) // Trim(Adjustl(tmp1)) &
  // 'NMNC,NITC,NDNC,NIMC,AMLC,NNMNC,NUCM' 
        
      fn = 'soilni.csv'
      Call GETLUN (fn,nf)
   
      Open (UNIT = nf, FILE = fn, FORM='FORMATTED', STATUS = 'REPLACE', &
        Action='Write', IOSTAT = ErrNum)
        
      Write(nf,'(A)')Header
      Deallocate(Header)
      
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
      EXTERNAL GETLUN
      Integer          :: nf, ErrNum, length        
      Character(Len=12):: fn 
      Character(:),Allocatable :: Header         
  
      If(.Not. Associated(headPlNMzCer)) Return
      
      length= Len('RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,DAP,CNAD,GNAD,VNAD,GN%D,VN%D,' &
  //'NUPC,LNAD,SNAD,LN%D,SN%D,RN%D,SNN0C,SNN1C') 

      Allocate(character(LEN=length) :: Header)

  Header = 'RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,DAP,CNAD,GNAD,VNAD,GN%D,VN%D,' &
  //'NUPC,LNAD,SNAD,LN%D,SN%D,RN%D,SNN0C,SNN1C' 
  
      fn = 'plantn.csv'
      Call GETLUN (fn,nf)
   
      Open (UNIT = nf, FILE = fn, FORM='FORMATTED', STATUS = 'REPLACE', &
        Action='Write', IOSTAT = ErrNum)
        
      Write(nf,'(A)')Header
      Deallocate(Header)
      
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
      EXTERNAL GETLUN
      Integer          :: nf, ErrNum, length       
      Character(Len=12):: fn         
      Character(:),Allocatable :: Header
      
      If(.Not. Associated(headWth)) Return
      
  length= Len('RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,PRED,DAYLD,TWLD,SRAD,' &
  //'PARD,CLDD,TMXD,TMND,TAVD,TDYD,TDWD,TGAD,TGRD,WDSD,CO2D,VPDF,VPD') 

      Allocate(character(LEN=length) :: Header)

  Header = 'RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,PRED,DAYLD,TWLD,SRAD,' &
  //'PARD,CLDD,TMXD,TMND,TAVD,TDYD,TDWD,TGAD,TGRD,WDSD,CO2D,VPDF,VPD' 
  
      fn = 'weather.csv'
      Call GETLUN (fn,nf)
   
      Open (UNIT = nf, FILE = fn, FORM='FORMATTED', STATUS = 'REPLACE', &
         Action='Write', IOSTAT = ErrNum)
        
      Write(nf,'(A)')Header
      Deallocate(Header)

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
      EXTERNAL GETLUN
      Integer          :: nf, ErrNum, length       
      Character(Len=12):: fn 
      Character(:),Allocatable :: Header 
        
      If(.Not. Associated(headPlGr2)) Return
  
  length= Len('RUN,EXP,TRTNUM,ROTNUM,REPNO,SN,ON,CN,YEAR,DOY,DAS,DAP,TMEAN,GSTD,RSTD,' &
  //'LAIPD,LAISD,LAID,CHTD,SDWAD,SNWLD,SNWSD,H#AD,HWUD,SHRTD,PTFD,RDPD,RL1D,' &
  //'RL2D,RL3D,RL4D,RL5D,RL6D,RL7D,RL8D,RL9D,RL10D') 

      Allocate(character(LEN=length) :: Header)

  Header = 'RUN,EXP,TRTNUM,ROTNUM,REPNO,SN,ON,CN,YEAR,DOY,DAS,DAP,TMEAN,GSTD,RSTD,' &
  //'LAIPD,LAISD,LAID,CHTD,SDWAD,SNWLD,SNWSD,H#AD,HWUD,SHRTD,PTFD,RDPD,RL1D,' &
  //'RL2D,RL3D,RL4D,RL5D,RL6D,RL7D,RL8D,RL9D,RL10D' 
        
      fn = 'plantgr2.csv'
      Call GETLUN (fn,nf)
   
      Open (UNIT = nf, FILE = fn, FORM='FORMATTED', STATUS = 'REPLACE',  &
         Action='Write', IOSTAT = ErrNum)
        
      Write(nf,'(A)')Header
      Deallocate(Header)
      
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
      EXTERNAL GETLUN
      Integer          :: nf, ErrNum, length       
      Character(Len=12):: fn 
      Character(:),Allocatable :: Header 
      
      If(.Not. Associated(headPlGrf)) Return  
      
  length= Len('RUN,EXP,TRTNUM,ROTNUM,REPNO,SN,ON,CN,YEAR,DOY,DAS,DAP,TMEAN,GSTD,DU,' &
  //'VRNFD,DYLFD,TFGEM,WFGE,TFPD,WFPD,NFPD,CO2FD,RSFPD,TFGD,WFGD,NFGD,WFTD,' &
  //'NFTD,WAVRD,WUPRD,SWXD,EOPD,SNXD,LN%RD,SN%RD,RN%RD')
  
      Allocate(character(LEN=length) :: Header)

  Header = 'RUN,EXP,TRTNUM,ROTNUM,REPNO,SN,ON,CN,YEAR,DOY,DAS,DAP,TMEAN,GSTD,DU,' &
  //'VRNFD,DYLFD,TFGEM,WFGE,TFPD,WFPD,NFPD,CO2FD,RSFPD,TFGD,WFGD,NFGD,WFTD,' &
  //'NFTD,WAVRD,WUPRD,SWXD,EOPD,SNXD,LN%RD,SN%RD,RN%RD' 
        
      fn = 'plantgrf.csv'
      Call GETLUN (fn,nf)
   
      Open (UNIT = nf, FILE = fn, FORM='FORMATTED', STATUS = 'REPLACE', &
         Action='Write', IOSTAT = ErrNum)
        
      Write(nf,'(A)')Header
      Deallocate(Header)
      
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
      EXTERNAL GETLUN
      Integer          :: nf, ErrNum, length       
      Character(Len=12):: fn 
      Character(:),Allocatable :: Header        
      
      If(.Not. Associated(headEvalCsCer)) Return
      
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
      Call GETLUN (fn,nf)
   
      Open (UNIT = nf, FILE = fn, FORM='FORMATTED', STATUS = 'REPLACE', &
         Action='Write', IOSTAT = ErrNum)
        
      Write(nf,'(A)')Header
      Deallocate(Header)
      
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
      EXTERNAL GETLUN
      Integer          :: nf, i, ErrNum, length       
      Character(Len=12):: fn         
      Character(:),Allocatable :: Header 
      Character(Len=700) :: tmp1 
      
      If(.Not. Associated(headEvOpsum)) Return
      
      length= Len('RUN,EXCODE,CG,TN,RN,CR')

      Allocate(character(LEN=length) :: Header)

      Header = 'RUN,EXCODE,CG,TN,RN,CR' 
   
      tmp1 = trim(adjustl(csvOLAP(1))) //'S'// ',' // trim(adjustl(csvOLAP(1))) //'M'
      do i = 2, csvICOUNT
         tmp1 = trim(tmp1) // ',' // trim(adjustl(csvOLAP(i))) // 'S' // ',' // &
            trim(adjustl(csvOLAP(i))) // 'M'
      end do            

      tmp1 = Header // ',' // trim(adjustl(tmp1))
      
      fn = 'evaluate.csv'
      Call GETLUN (fn,nf)
   
      Open (UNIT = nf, FILE = fn, FORM='FORMATTED', STATUS = 'REPLACE', &
         Action='Write', IOSTAT = ErrNum)
        
      Write(nf,'(A)')tmp1
      Deallocate(Header)
      
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
      EXTERNAL GETLUN
      Integer          :: nf, ErrNum, length       
      Character(Len=12):: fn  
      Character(:),Allocatable :: Header        
      
      If(.Not. Associated(headSumOpsum)) Return
      
!  length= Len('RUNNO,TRNO,R#,O#,C#,CR,MODEL,EXNAME,TNAM,FNAM,WSTA,SOIL_ID,' &
   length= Len('RUNNO,TRNO,R#,O#,P#,CR,MODEL,EXNAME,TNAM,'& 
  // 'FNAM,WSTA,WYEAR,SOIL_ID,LAT,LONG,ELEV,' &
  // 'SDAT,PDAT,EDAT,ADAT,MDAT,HDAT,HYEAR,DWAP,CWAM,HWAM,HWAH,BWAH,PWAM,HWUM,' &
  // 'H#AM,H#UM,HIAM,LAIX,EYLDH,FCWAM,FHWAM,HWAHF,FBWAH,FPWAM,IR#M,IRCM,PRCM,ETCM,EPCM,ESCM,ROCM,DRCM,SWXM,' &
  // 'NI#M,NICM,NFXM,NUCM,NLCM,NIAM,NMINC,CNAM,GNAM,N2OEM,PI#M,PICM,PUPC,SPAM,KI#M,' &
  // 'KICM,KUPC,SKAM,RECM,ONTAM,ONAM,OPTAM,OPAM,OCTAM,OCAM,CO2EM,CH4EM,DMPPM,DMPEM,' &
  // 'DMPTM,DMPIM,YPPM,YPEM,YPTM,YPIM,DPNAM,DPNUM,YPNAM,YPNUM,NDCH,TMAXA,' &
  // 'TMINA,SRADA,DAYLA,CO2A,PRCP,ETCP,ESCP,EPCP,CRST')

      Allocate(character(LEN=length) :: Header)

! Header = 'RUNNO,TRNO,R#,O#,C#,CR,MODEL,EXNAME,TNAM,FNAM,WSTA,SOIL_ID,' &
  Header = 'RUNNO,TRNO,R#,O#,P#,CR,MODEL,EXNAME,TNAM,'& 
  // 'FNAM,WSTA,WYEAR,SOIL_ID,LAT,LONG,ELEV,' &
  // 'SDAT,PDAT,EDAT,ADAT,MDAT,HDAT,HYEAR,DWAP,CWAM,HWAM,HWAH,BWAH,PWAM,HWUM,' &
  // 'H#AM,H#UM,HIAM,LAIX,EYLDH,FCWAM,FHWAM,HWAHF,FBWAH,FPWAM,IR#M,IRCM,PRCM,ETCM,EPCM,ESCM,ROCM,DRCM,SWXM,' &
  // 'NI#M,NICM,NFXM,NUCM,NLCM,NIAM,NMINC,CNAM,GNAM,N2OEM,PI#M,PICM,PUPC,SPAM,KI#M,' &
  // 'KICM,KUPC,SKAM,RECM,ONTAM,ONAM,OPTAM,OPAM,OCTAM,OCAM,CO2EM,CH4EM,DMPPM,DMPEM,' &
  // 'DMPTM,DMPIM,YPPM,YPEM,YPTM,YPIM,DPNAM,DPNUM,YPNAM,YPNUM,NDCH,TMAXA,' &
  // 'TMINA,SRADA,DAYLA,CO2A,PRCP,ETCP,ESCP,EPCP,CRST'      
      
      fn = 'summary.csv'
      Call GETLUN (fn,nf)
   
      Open (UNIT = nf, FILE = fn, FORM='FORMATTED', STATUS = 'REPLACE', &
          Action='Write', IOSTAT = ErrNum)
        
      Write(nf,'(A)')Header
      Deallocate(Header)
       
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
      EXTERNAL GETLUN
      Integer          :: nf, ErrNum, length       
      Character(Len=12):: fn 
      Character(:),Allocatable :: Header         
      
      If(.Not. Associated(headPlCCrGro)) Return
      
      length= Len('RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,DAP,TWAD,PHAD,CMAD,CGRD,' &
  //'GRAD,MRAD,CHAD,CL%D,CS%D,TGNN,TGAV,GN%D,GL%D,GC%D') 

      Allocate(character(LEN=length) :: Header)

  Header = 'RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,DAP,TWAD,PHAD,CMAD,CGRD,' &
  //'GRAD,MRAD,CHAD,CL%D,CS%D,TGNN,TGAV,GN%D,GL%D,GC%D' 
  
      fn = 'plantc.csv'
      Call GETLUN (fn,nf)
  
      Open (UNIT = nf, FILE = fn, FORM='FORMATTED', STATUS = 'REPLACE', &
         Action='Write', IOSTAT = ErrNum)
        
      Write(nf,'(A)')Header
      Deallocate(Header)

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
   Subroutine ListtofileSoilOrg(N_ELEMS)
      EXTERNAL GETLUN
      Integer          :: nf, ErrNum, length       
      Character(Len=12):: fn  
      Character(:),Allocatable :: Header        
      
      Integer :: N_ELEMS       ! Number of elements: 1 = N, 2 = N+P, 3 = N+P+S (-)

      If(.Not. Associated(headSoilOrg)) Return
      
      If (N_ELEMS > 1) Then 
         length= Len('RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,OMAC,SCDD,SOCD,' &
         //'SC0D,SCTD,SOMCT,LCTD,ONAC,SNDD,SOND,SN0D,SNTD,' &
         //'SOMNT,LNTD,OPAC,SPDD,SOPD,SP0D,SPTD,SOMPT,LPTD') 

         Allocate(character(LEN=length) :: Header)

         Header = 'RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,OMAC,SCDD,SOCD,' &
         //'SC0D,SCTD,SOMCT,LCTD,ONAC,SNDD,SOND,SN0D,SNTD,' &
         //'SOMNT,LNTD,OPAC,SPDD,SOPD,SP0D,SPTD,SOMPT,LPTD' 
      Else
         length= Len('RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,' &
         //'OMAC,SCDD,SOCD,SC0D,SCTD,SOMCT,LCTD,ONAC,SNDD,' &
         //'SOND,SN0D,SNTD,SOMNT,LNTD')

         Allocate(character(LEN=length) :: Header)

         Header = 'RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,' &
         //'OMAC,SCDD,SOCD,SC0D,SCTD,SOMCT,LCTD,ONAC,SNDD,' &
         //'SOND,SN0D,SNTD,SOMNT,LNTD' 
      End If 
      
      fn = 'soilorg.csv'
      Call GETLUN (fn,nf)
      
      Open (UNIT = nf, FILE = fn, FORM='FORMATTED', STATUS = 'REPLACE', Action='Write', &
        IOSTAT = ErrNum)
     
      Write(nf,'(A)')Header
      Deallocate(Header)

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
      EXTERNAL GETLUN
      Integer          :: nf, ErrNum, length       
      Character(Len=12):: fn  
      Character(:),Allocatable :: Header 
            
      If (.NOT. Associated(headETPhot)) Return
      
      length= Len('RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,' &
     //'LI%D,PHAD,PHAN,LI%N,SLLN,SLHN,N%LN,N%HN,LMLN,LMHN,TGNN,TGAV') 

      Allocate(character(LEN=length) :: Header)

      Header = 'RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,' &
     //'LI%D,PHAD,PHAN,LI%N,SLLN,SLHN,N%LN,N%HN,LMLN,LMHN,TGNN,TGAV' 
      
      fn = 'etphot.csv'
      Call GETLUN (fn,nf)
   
      Open (UNIT = nf, FILE = fn, FORM='FORMATTED', STATUS = 'REPLACE', Action='Write', &
         IOSTAT = ErrNum)
      
      Write(nf,'(A)')Header
      Deallocate(Header)
      
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
      EXTERNAL GETLUN
      Integer          :: nf, ErrNum, length       
      Character(Len=12):: fn
      Character(:),Allocatable :: Header          
      
      If(.Not. Associated(headMulch)) Return
      
      length= Len('RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,MCFD,MDEPD,MWAD,MWTD') 

      Allocate(character(LEN=length) :: Header)

      Header = 'RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,MCFD,MDEPD,MWAD,MWTD' 
      
      fn = 'mulch.csv'
      Call GETLUN (fn,nf)
   
      Open (UNIT = nf, FILE = fn, FORM='FORMATTED', STATUS = 'REPLACE', Action='Write', &
         IOSTAT = ErrNum)
     
      Write(nf,'(A)')Header
      Deallocate(Header)
      
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
    EXTERNAL GETLUN
    Integer          :: nf, ErrNum, length       
    Character(Len=12):: fn 
    Character(:),Allocatable :: Header       
    
    If (.NOT. Associated(headPlantP)) Return
    
    length= Len('RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,DAP,' &
     //'PSHOD,PRTOD,PSLOD,PSDOD,PSHMD,PRTMD,PSLMD,PSDMD,SHPPD,RTPPD,SLPPD,' &
     //'SDPPD,PLPPD,SHPAD,RTPAD,SLPAD,SDPAD,PPAD,PST1A,PST2A,PUPD,PUPC,' &
     //'SNP0C,SNP1C,PHFR1,PHFR2,SHWAD,RWAD,SHAD,GWAD,PSTRAT,NTOPD,PTDD')
     
      Allocate(character(LEN=length) :: Header) 

     Header = 'RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,DAP,' &
     //'PSHOD,PRTOD,PSLOD,PSDOD,PSHMD,PRTMD,PSLMD,PSDMD,SHPPD,RTPPD,SLPPD,' &
     //'SDPPD,PLPPD,SHPAD,RTPAD,SLPAD,SDPAD,PPAD,PST1A,PST2A,PUPD,PUPC,' &
     //'SNP0C,SNP1C,PHFR1,PHFR2,SHWAD,RWAD,SHAD,GWAD,PSTRAT,NTOPD,PTDD'
     
    fn = 'plantp.csv'
    Call GETLUN (fn,nf)

    Open (UNIT = nf, FILE = fn, FORM='FORMATTED', STATUS = 'REPLACE', Action='Write', &
         IOSTAT = ErrNum)
     
    Write(nf,'(A)')Header
    Deallocate(Header)

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
    EXTERNAL GETLUN
    Integer          :: nf, ErrNum, length       
    Character(Len=12):: fn 
    Character(:),Allocatable :: Header        
    
    If (.NOT. Associated(headSoilPi)) Return
    
    length= Len('RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,' &
     //'PIAD,PAVLD,PSOLD,PLABD,PACTD,PSTAD,PAPC,PMNC,PIMC,' &
     //'PUPC,PAV1D,PAV2D,PAV3D,PAV4D,PAV5D,PUP1D,PUP2D,PUP3D,' &
     //'PUP4D,PUP5D,PLAB1,PLAB2,PLAB3,PLAB4,PLAB5') 

      Allocate(character(LEN=length) :: Header)

    Header = 'RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,' &
     //'PIAD,PAVLD,PSOLD,PLABD,PACTD,PSTAD,PAPC,PMNC,PIMC,' &
     //'PUPC,PAV1D,PAV2D,PAV3D,PAV4D,PAV5D,PUP1D,PUP2D,PUP3D,' &
     //'PUP4D,PUP5D,PLAB1,PLAB2,PLAB3,PLAB4,PLAB5'
     
    fn = 'soilpi.csv'
    Call GETLUN (fn,nf)

    Open (UNIT = nf, FILE = fn, FORM='FORMATTED', STATUS = 'REPLACE', Action='Write', &
         IOSTAT = ErrNum)
     
    Write(nf,'(A)')Header
    Deallocate(Header)

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
Subroutine LinklstPlGroPrFrm(ptxtlinePlGroPrFrm)
    Character(:), Allocatable :: ptxtlinePlGroPrFrm           
        
    If(.Not. Associated(headPlGroPrFrm)) Then           
      Allocate(headPlGroPrFrm, Stat=istatPlGroPrFrm)        
      If(istatPlGroPrFrm==0) Then                       
        tailPlGroPrFrm => headPlGroPrFrm                    
        Nullify(tailPlGroPrFrm%pPlGroPrFrm)                 
        tailPlGroPrFrm%pclinePlGroPrFrm = ptxtlinePlGroPrFrm    
      Else
        ! Error message
      End If
    Else
      Allocate(tailPlGroPrFrm%pPlGroPrFrm, Stat=istatPlGroPrFrm)   
      If(istatPlGroPrFrm==0) Then                          
        tailPlGroPrFrm=> tailPlGroPrFrm%pPlGroPrFrm                
        Nullify(tailPlGroPrFrm%pPlGroPrFrm)                    
        tailPlGroPrFrm%pclinePlGroPrFrm = ptxtlinePlGroPrFrm       
      Else
      ! Error message
      End If
    End If

End Subroutine LinklstPlGroPrFrm
!------------------------------------------------------------------------------ 
   Subroutine ListtofilePlGroPrFrm(nlayers)
      EXTERNAL GETLUN
      Integer          :: nf       ! Number for growth output file  #
      Character(Len=12):: fn       ! Growth output file code  
      Character(Len=14) :: fmt
      Character(Len=2) :: numtoch1, numtoch2 
      Character(Len=220) :: tmp
      Character(:),Allocatable :: Header 
      Integer :: ErrNum, length, nlayers, i, nl
      
      If(.Not. Associated(headPlGroPrFrm)) Return

      nl = MIN(10, MAX(4,nlayers))
  
      Write(numtoch1,'(I2)') nl - 1  
       
      fmt = '('//Trim(Adjustl(numtoch1))//'(A2,I1,A2))'
      fmt = Trim(Adjustl(fmt))
   
      Write (tmp,fmt) ("RL",i,"D,",i=1,nl - 1)
      tmp = Trim(Adjustl(tmp)) 
      Write(numtoch2,'(I2)') nl  
      tmp = Trim(Adjustl(tmp)) // "RL" // Trim(Adjustl(numtoch2)) // "D" 
       
  length= Len('RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,DAP,L#SD,GSTD,LAID,' &
  //'LWAD,SWAD,QWAD,QS%D,Q1%D,GWAD,RWAD,CWAD,G#AD,GWGD,HIAD,PWAD,P#AD,WSPD,' &
  //'WSGD,NSTD,EWSD,LN%D,SH%D,HIPD,PWDD,PWTD,SLAD,CHTD,CWID,NWAD,RDPD,' &
  //'CDAD,LDAD,SDAD,QDAD,HERB,FHL%,LF%D,DWTCO,DWTLO,DWTSO,CHTCM,CPROT,') &
  + Len(Trim(Adjustl(tmp)))
  
      Allocate(character(LEN=length) :: Header)

  Header = 'RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,DAP,L#SD,GSTD,LAID,' &
  //'LWAD,SWAD,QWAD,QS%D,Q1%D,GWAD,RWAD,CWAD,G#AD,GWGD,HIAD,PWAD,P#AD,WSPD,' &
  //'WSGD,NSTD,EWSD,LN%D,SH%D,HIPD,PWDD,PWTD,SLAD,CHTD,CWID,NWAD,RDPD,' &
  //'CDAD,LDAD,SDAD,QDAD,HERB,FHL%,LF%D,DWTCO,DWTLO,DWTSO,CHTCM,CPROT,' &
  // Trim(Adjustl(tmp))
         
      fn = 'plantgro.csv'
      Call GETLUN (fn,nf)
  
      Open (UNIT = nf, FILE = fn, FORM='FORMATTED', STATUS = 'REPLACE',  &
          Action='Write', IOSTAT = ErrNum)
        
      Write(nf,'(A)')Header
      Deallocate(Header)

      ! write out the data
      ptrPlGroPrFrm => headPlGroPrFrm
      Do
        If(.Not. Associated(ptrPlGroPrFrm)) Exit           ! Pointer valid?
        Write(nf,'(A)') ptrPlGroPrFrm % pclinePlGroPrFrm   ! Yes: Write value
        ptrPlGroPrFrm => ptrPlGroPrFrm % pPlGroPrFrm       ! Get next pointer
      End Do

      Nullify(ptrPlGroPrFrm, headPlGroPrFrm, tailPlGroPrFrm)
      Close(nf)
   End Subroutine ListtofilePlGroPrFrm 
!------------------------------------------------------------------------------ 
  Subroutine ListtofilePlNPrFrm
      EXTERNAL GETLUN
      Integer          :: nf, ErrNum, length       
      Character(Len=12):: fn
      Character(:),Allocatable :: Header         
      
      If(.Not. Associated(headPlNPrFrm)) Return
      
      length= Len('RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,DAP,' &
  //'CNAD,GNAD,VNAD,GN%D,' &
  //'VN%D,NFXC,NUPC,LNAD,SNAD,QNAD,LN%D,SN%D,QN%D,SHND,RN%D,NFXD')
  
      Allocate(character(LEN=length) :: Header)

  Header = 'RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,DAP,' &
  //'CNAD,GNAD,VNAD,GN%D,' &
  //'VN%D,NFXC,NUPC,LNAD,SNAD,QNAD,LN%D,SN%D,QN%D,SHND,RN%D,NFXD' 
  
      fn = 'plantn.csv'
      Call GETLUN (fn,nf)

      Open (UNIT = nf, FILE = fn, FORM='FORMATTED', STATUS = 'REPLACE', &
          IOSTAT = ErrNum)
        
      Write(nf,'(A)')Header
      Deallocate(Header)    

      ptrPlNPrFrm => headPlNPrFrm
      Do
        If(.Not. Associated(ptrPlNPrFrm)) Exit          
        Write(nf,'(A)') ptrPlNPrFrm % pclinePlNPrFrm    
        ptrPlNPrFrm => ptrPlNPrFrm % pPlNPrFrm          
      End Do

      Nullify(ptrPlNPrFrm, headPlNPrFrm, tailPlNPrFrm)
      Close(nf)
  End Subroutine ListtofilePlNPrFrm
!------------------------------------------------------------------------------ 
Subroutine LinklstPlNPrFrm(ptxtlinePlNPrFrm)

    Character(:), Allocatable :: ptxtlinePlNPrFrm            
        
    If(.Not. Associated(headPlNPrFrm)) Then             
      Allocate(headPlNPrFrm, Stat=istatPlNPrFrm)        
      If(istatPlNPrFrm==0) Then                         
        tailPlNPrFrm => headPlNPrFrm                    
        Nullify(tailPlNPrFrm%pPlNPrFrm)                 
        tailPlNPrFrm%pclinePlNPrFrm = ptxtlinePlNPrFrm  
      Else
        ! Error message
      End If
    Else
      Allocate(tailPlNPrFrm%pPlNPrFrm, Stat=istatPlNPrFrm)      
      If(istatPlNPrFrm==0) Then                                 
        tailPlNPrFrm=> tailPlNPrFrm%pPlNPrFrm                   
        Nullify(tailPlNPrFrm%pPlNPrFrm)                         
        tailPlNPrFrm%pclinePlNPrFrm = ptxtlinePlNPrFrm          
      Else
      ! Error message
      End If
    End If

End Subroutine LinklstPlNPrFrm
!------------------------------------------------------------------------------
  Subroutine ListtofilePlCPrFrm
      EXTERNAL GETLUN
      Integer          :: nf, ErrNum, length       
      Character(Len=12):: fn
      Character(:),Allocatable :: Header         
      
      If(.Not. Associated(headPlCPrFrm)) Return
      
      length= Len('RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,DAP,TWAD,PHAD,' &
  //'CMAD,CGRD,GRAD,MRAD,CHAD,QHAD,CL%D,CS%D,QC%D,CR%D,TGNN,TGAV,'&
  //'GN%D,GL%D,GC%D')
  
      Allocate(character(LEN=length) :: Header)

      Header = 'RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,DAP,TWAD,PHAD,' &
  //'CMAD,CGRD,GRAD,MRAD,CHAD,QHAD,CL%D,CS%D,QC%D,CR%D,TGNN,TGAV,'&
  //'GN%D,GL%D,GC%D' 
  
      fn = 'plantc.csv'
      Call GETLUN (fn,nf)

      Open (UNIT = nf, FILE = fn, FORM='FORMATTED', STATUS = 'REPLACE', &
          IOSTAT = ErrNum)
        
      Write(nf,'(A)')Header
      Deallocate(Header)    

      ptrPlCPrFrm => headPlCPrFrm
      Do
        If(.Not. Associated(ptrPlCPrFrm)) Exit          
        Write(nf,'(A)') ptrPlCPrFrm % pclinePlCPrFrm    
        ptrPlCPrFrm => ptrPlCPrFrm % pPlCPrFrm          
      End Do

      Nullify(ptrPlCPrFrm, headPlCPrFrm, tailPlCPrFrm)
      Close(nf)
  End Subroutine ListtofilePlCPrFrm
!------------------------------------------------------------------------------
Subroutine LinklstPlCPrFrm(ptxtlinePlCPrFrm)

    Character(:), Allocatable :: ptxtlinePlCPrFrm            
        
    If(.Not. Associated(headPlCPrFrm)) Then             
      Allocate(headPlCPrFrm, Stat=istatPlCPrFrm)        
      If(istatPlCPrFrm==0) Then                         
        tailPlCPrFrm => headPlCPrFrm                    
        Nullify(tailPlCPrFrm%pPlCPrFrm)                 
        tailPlCPrFrm%pclinePlCPrFrm = ptxtlinePlCPrFrm  
      Else
        ! Error message
      End If
    Else
      Allocate(tailPlCPrFrm%pPlCPrFrm, Stat=istatPlCPrFrm)      
      If(istatPlCPrFrm==0) Then                                 
        tailPlCPrFrm=> tailPlCPrFrm%pPlCPrFrm                   
        Nullify(tailPlCPrFrm%pPlCPrFrm)                         
        tailPlCPrFrm%pclinePlCPrFrm = ptxtlinePlCPrFrm          
      Else
      ! Error message
      End If
    End If

End Subroutine LinklstPlCPrFrm
!------------------------------------------------------------------------------
  Subroutine ListtofileDormPrFrm
      EXTERNAL GETLUN
      Integer          :: nf, ErrNum, length       
      Character(Len=12):: fn
      Character(:),Allocatable :: Header         
      
      If(.Not. Associated(headDormPrFrm)) Return
      
      length= Len('RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,DAP,' &
  //'QDSD,PPGF,PPMF,PPTF,TSRD,TS1D,FRZ2,LV%D,SV%D,QV%D,RV%D')
  
      Allocate(character(LEN=length) :: Header)

      Header = 'RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,DAP,' &
  //'QDSD,PPGF,PPMF,PPTF,TSRD,TS1D,FRZ2,LV%D,SV%D,QV%D,RV%D' 
  
      fn = 'dormancy.csv'
      Call GETLUN (fn,nf)

      Open (UNIT = nf, FILE = fn, FORM='FORMATTED', STATUS = 'REPLACE', &
          IOSTAT = ErrNum)
        
      Write(nf,'(A)')Header
      Deallocate(Header)    

      ptrDormPrFrm => headDormPrFrm
      Do
        If(.Not. Associated(ptrDormPrFrm)) Exit          
        Write(nf,'(A)') ptrDormPrFrm % pclineDormPrFrm    
        ptrDormPrFrm => ptrDormPrFrm % pDormPrFrm          
      End Do

      Nullify(ptrDormPrFrm, headDormPrFrm, tailDormPrFrm)
      Close(nf)
  End Subroutine ListtofileDormPrFrm
!------------------------------------------------------------------------------
Subroutine LinklstDormPrFrm(ptxtlineDormPrFrm)

    Character(:), Allocatable :: ptxtlineDormPrFrm            
        
    If(.Not. Associated(headDormPrFrm)) Then             
      Allocate(headDormPrFrm, Stat=istatDormPrFrm)        
      If(istatDormPrFrm==0) Then                         
        tailDormPrFrm => headDormPrFrm                    
        Nullify(tailDormPrFrm%pDormPrFrm)                 
        tailDormPrFrm%pclineDormPrFrm = ptxtlineDormPrFrm  
      Else
        ! Error message
      End If
    Else
      Allocate(tailDormPrFrm%pDormPrFrm, Stat=istatDormPrFrm)      
      If(istatDormPrFrm==0) Then                                 
        tailDormPrFrm=> tailDormPrFrm%pDormPrFrm                   
        Nullify(tailDormPrFrm%pDormPrFrm)                         
        tailDormPrFrm%pclineDormPrFrm = ptxtlineDormPrFrm          
      Else
      ! Error message
      End If
    End If

End Subroutine LinklstDormPrFrm
!------------------------------------------------------------------------------
  Subroutine ListtofileStorPrFrm
      EXTERNAL GETLUN
      Integer          :: nf, ErrNum, length       
      Character(Len=12):: fn
      Character(:),Allocatable :: Header         
      
      If(.Not. Associated(headStorPrFrm)) Return
      
      length= Len('RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,DAP,' &
  //'QCQD,QHAD,QC%M,QRAD,QMAD,QCFD,QCAD,QCDD,QDTD,QN%X,'&
  //'QN%I,QV%D,QV%T,QNAA,QNRX,QNRN,QNAR,QNAM,QNAG,QNAN,'&
  //'QNAL,QN%N,QN%D,QW%C,QP%W,QL%S,QL%1,QS%D,Q1%D,QC%D,'&
  //'QCAM,QFDS,QFD1,QEAD,QEWD,QWAD,QT%S,QT%1,QCRD,QNMD,'&
  //'QCAG,QWNG,QWND,QFAD,QWAI,QMAM,QNAD,QNAC,QNLC,QDAD,XSTR')
  
      Allocate(character(LEN=length) :: Header)

      Header = 'RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,DAP,' &
  //'QCQD,QHAD,QC%M,QRAD,QMAD,QCFD,QCAD,QCDD,QDTD,QN%X,'&
  //'QN%I,QV%D,QV%T,QNAA,QNRX,QNRN,QNAR,QNAM,QNAG,QNAN,'&
  //'QNAL,QN%N,QN%D,QW%C,QP%W,QL%S,QL%1,QS%D,Q1%D,QC%D,'&
  //'QCAM,QFDS,QFD1,QEAD,QEWD,QWAD,QT%S,QT%1,QCRD,QNMD,'&
  //'QCAG,QWNG,QWND,QFAD,QWAI,QMAM,QNAD,QNAC,QNLC,QDAD,XSTR' 
  
      fn = 'storage.csv'
      Call GETLUN (fn,nf)

      Open (UNIT = nf, FILE = fn, FORM='FORMATTED', STATUS = 'REPLACE', &
          IOSTAT = ErrNum)
        
      Write(nf,'(A)')Header
      Deallocate(Header)    

      ptrStorPrFrm => headStorPrFrm
      Do
        If(.Not. Associated(ptrStorPrFrm)) Exit          
        Write(nf,'(A)') ptrStorPrFrm % pclineStorPrFrm    
        ptrStorPrFrm => ptrStorPrFrm % pStorPrFrm          
      End Do

      Nullify(ptrStorPrFrm, headStorPrFrm, tailStorPrFrm)
      Close(nf)
  End Subroutine ListtofileStorPrFrm
!------------------------------------------------------------------------------
Subroutine LinklstStorPrFrm(ptxtlineStorPrFrm)

    Character(:), Allocatable :: ptxtlineStorPrFrm            
        
    If(.Not. Associated(headStorPrFrm)) Then             
      Allocate(headStorPrFrm, Stat=istatStorPrFrm)        
      If(istatStorPrFrm==0) Then                         
        tailStorPrFrm => headStorPrFrm                    
        Nullify(tailStorPrFrm%pStorPrFrm)                 
        tailStorPrFrm%pclineStorPrFrm = ptxtlineStorPrFrm  
      Else
        ! Error message
      End If
    Else
      Allocate(tailStorPrFrm%pStorPrFrm, Stat=istatStorPrFrm)      
      If(istatStorPrFrm==0) Then                                 
        tailStorPrFrm=> tailStorPrFrm%pStorPrFrm                   
        Nullify(tailStorPrFrm%pStorPrFrm)                         
        tailStorPrFrm%pclineStorPrFrm = ptxtlineStorPrFrm          
      Else
      ! Error message
      End If
    End If

End Subroutine LinklstStorPrFrm
!------------------------------------------------------------------------------
  Subroutine ListtofileSomN
      EXTERNAL GETLUN
      Integer          :: nf, ErrNum, length       
      Character(Len=12):: fn
      Character(:),Allocatable :: Header         
      
      If(.Not. Associated(headSomN)) Return
      
      length= Len('RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,' &
  //'SNS20D,SN%20D,SNS40D,SN%40D,SOND,SN0D,SNTD,SN1D,SN2D,'&
  //'SN3D,SN4D,SN5D,S1N0D,S1NTD,S1N1D,S1N2D,S1N3D,S1N4D,' &
  //'S1N5D,S2NTD,S2N1D,S2N2D,S2N3D,S2N4D,S2N5D,S3NTD,S3N1D,'&
  //'S3N2D,S3N3D,S3N4D,S3N5D,LN0D,LNTD,LN1D,LN2D,LN3D,LN4D,' &
  //'LN5D,MEN0D,MENTD,MEN1D,MEN2D,MEN3D,MEN4D,MEN5D,STN0D,' &
  //'STNTD,STN1D,STN2D,STN3D,STN4D,STN5D,RESNC')
  
      Allocate(character(LEN=length) :: Header)

      Header = 'RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,' &
  //'SNS20D,SN%20D,SNS40D,SN%40D,SOND,SN0D,SNTD,SN1D,SN2D,'&
  //'SN3D,SN4D,SN5D,S1N0D,S1NTD,S1N1D,S1N2D,S1N3D,S1N4D,' &
  //'S1N5D,S2NTD,S2N1D,S2N2D,S2N3D,S2N4D,S2N5D,S3NTD,S3N1D,'&
  //'S3N2D,S3N3D,S3N4D,S3N5D,LN0D,LNTD,LN1D,LN2D,LN3D,LN4D,' &
  //'LN5D,MEN0D,MENTD,MEN1D,MEN2D,MEN3D,MEN4D,MEN5D,STN0D,' &
  //'STNTD,STN1D,STN2D,STN3D,STN4D,STN5D,RESNC' 
  
      fn = 'somlitn.csv'
      Call GETLUN (fn,nf)

      Open (UNIT = nf, FILE = fn, FORM='FORMATTED', STATUS = 'REPLACE', &
          IOSTAT = ErrNum)
        
      Write(nf,'(A)')Header
      Deallocate(Header)    

      ptrSomN => headSomN
      Do
        If(.Not. Associated(ptrSomN)) Exit          
        Write(nf,'(A)') ptrSomN % pclineSomN    
        ptrSomN => ptrSomN % pSomN          
      End Do

      Nullify(ptrSomN, headSomN, tailSomN)
      Close(nf)
  End Subroutine ListtofileSomN
!------------------------------------------------------------------------------
Subroutine LinklstSomN(ptxtlineSomN)

    Character(:), Allocatable :: ptxtlineSomN            
        
    If(.Not. Associated(headSomN)) Then             
      Allocate(headSomN, Stat=istatSomN)        
      If(istatSomN==0) Then                         
        tailSomN => headSomN                    
        Nullify(tailSomN%pSomN)                 
        tailSomN%pclineSomN = ptxtlineSomN  
      Else
        ! Error message
      End If
    Else
      Allocate(tailSomN%pSomN, Stat=istatSomN)      
      If(istatSomN==0) Then                                 
        tailSomN=> tailSomN%pSomN                   
        Nullify(tailSomN%pSomN)                         
        tailSomN%pclineSomN = ptxtlineSomN          
      Else
      ! Error message
      End If
    End If

End Subroutine LinklstSomN

!------------------------------------------------------------------------------
  Subroutine ListtofileSomC
      EXTERNAL GETLUN
      Integer          :: nf, ErrNum, length       
      Character(Len=12):: fn
      Character(:),Allocatable :: Header         
      
      If(.Not. Associated(headSomC)) Return
      
      length= Len('RUN,EXP,TR,RN,REP,YEAR,DOY,DAS,'   &
       //'SCS20D,C%20D,SCS40D,SC%40D'                 &
       //'SLC20D,L%20D,SLC40D,SL%40D,SOCD'            &
       //'SC0D,SCTD,SC1D,SC2D,SC3D,SC4D,SC5D'         &
       //'S1C0D,S1CTD,S1C1D,S1C2D,S1C3D,S1C4D,S1C5D'  &
       //',S2CTD,S2C1D,S2C2D,S2C3D,S2C4D,S2C5D'       &
       //',S3CTD,S3C1D,S3C2D,S3C3D,S3C4D,S3C5D'       &
       //'LC0D,LCTD,LC1D,LC2D,LC3D,LC4D,LC5D'         &
       //'MEC0D,MECTD,MEC1D,MEC2D,MEC3D,MEC4D,MEC5D'  &
       //'STC0D,STCTD,STC1D,STC2D,STC3D,STC4D,STC5D'  &
       //'RESC,CO20C,CO2SC')                          

      Allocate(character(LEN=length) :: Header)

      Header = 'RUN,EXP,TR,RN,REP,YEAR,DOY,DAS,'      &
       //'SCS20D,C%20D,SCS40D,SC%40D'                 &
       //'SLC20D,L%20D,SLC40D,SL%40D,SOCD'            &
       //'SC0D,SCTD,SC1D,SC2D,SC3D,SC4D,SC5D'         &
       //'S1C0D,S1CTD,S1C1D,S1C2D,S1C3D,S1C4D,S1C5D'  &
       //',S2CTD,S2C1D,S2C2D,S2C3D,S2C4D,S2C5D'       &
       //',S3CTD,S3C1D,S3C2D,S3C3D,S3C4D,S3C5D'       &
       //'LC0D,LCTD,LC1D,LC2D,LC3D,LC4D,LC5D'         &
       //'MEC0D,MECTD,MEC1D,MEC2D,MEC3D,MEC4D,MEC5D'  &
       //'STC0D,STCTD,STC1D,STC2D,STC3D,STC4D,STC5D'  &
       //'RESC,CO20C,CO2SC'                           
  
      fn = 'somlitc.csv'
      Call GETLUN (fn,nf)

      Open (UNIT = nf, FILE = fn, FORM='FORMATTED', STATUS = 'REPLACE', &
          IOSTAT = ErrNum)
        
      Write(nf,'(A)')Header
      Deallocate(Header)    

      ptrSomC => headSomC
      Do
        If(.Not. Associated(ptrSomC)) Exit          
        Write(nf,'(A)') ptrSomC % pclineSomC    
        ptrSomC => ptrSomC % pSomC          
      End Do

      Nullify(ptrSomC, headSomC, tailSomC)
      Close(nf)
  End Subroutine ListtofileSomC

!------------------------------------------------------------------------------
Subroutine LinklstSomC(ptxtlineSomC)

    Character(:), Allocatable :: ptxtlineSomC            
        
    If(.Not. Associated(headSomC)) Then             
      Allocate(headSomC, Stat=istatSomC)        
      If(istatSomC==0) Then                         
        tailSomC => headSomC                    
        Nullify(tailSomC%pSomC)                 
        tailSomC%pclineSomC = ptxtlineSomC  
      Else
        ! Error message
      End If
    Else
      Allocate(tailSomC%pSomC, Stat=istatSomC)      
      If(istatSomC==0) Then                                 
        tailSomC=> tailSomC%pSomC                   
        Nullify(tailSomC%pSomC)                         
        tailSomC%pclineSomC = ptxtlineSomC          
      Else
      ! Error message
      End If
    End If

End Subroutine LinklstSomC

!------------------------------------------------------------------------------
Subroutine ListtofileN2O(nlayers)
      EXTERNAL GETLUN
      Integer          :: nf       
      Character(Len=12):: fn  
      Character(Len=30):: fmt  
      Character(Len=1000) :: tmp
      Character(:),Allocatable :: Header 
      Integer :: ErrNum, length, nlayers, i
      
      If(.Not. Associated(headN2O)) Return
      
      IF (nlayers < 10) THEN
        WRITE(fmt,'(I2)') 5 * nlayers
        fmt = '('//trim(adjustl(fmt))//'(",",g0,I1,g0))'
        WRITE (tmp,fmt)                     &
             ('NDN',i,'D',i=1,nlayers),     &
             ('NIT',i,'D',i=1,nlayers),     &
             ('N2O',i,'D',i=1,nlayers),     & 
             ('N2F',i,'D',i=1,nlayers),     &
             ('NOF',i,'D',i=1,nlayers)     
      ELSE
        fmt = '(5(9(",",A,I1,A),",",A5))'
        WRITE (tmp,fmt)                     &                
             ('NDN',i,'D',i=1,9),'NDN10',   &
             ('NIT',i,'D',i=1,9),'NIT10',   &
             ('N2O',i,'D',i=1,9),'N2O10',   &
             ('N2F',i,'D',i=1,9),'N2F10',   &
             ('NOF',i,'D',i=1,9),'NOF10'   
      ENDIF
      
      tmp = Trim(Adjustl(tmp)) 
      
    length= Len('RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,' &
      //'N2OEC,N2EC,NOEC,NDNC,' &
      //'NITC,N2ODC,N2ONC,N2FLC,NOFLC,' &
      //'N2OED,N2ED,NOED,NDND,' &
      //'NITRD,N2ODD,N2OND,N2FLD,NOFLD') &
      + Len(Trim(Adjustl(tmp)))
    
        Allocate(character(LEN=length) :: Header)

    Header = 'RUN,EXP,TRTNUM,ROTNUM,REPNO,YEAR,DOY,DAS,' &
      //'N2OEC,N2EC,NOEC,NDNC,' &
      //'NITC,N2ODC,N2ONC,N2FLC,NOFLC,' &
      //'N2OED,N2ED,NOED,NDND,' &
      //'NITRD,N2ODD,N2OND,N2FLD,NOFLD' &
      // Trim(Adjustl(tmp))
  
      fn = 'n2o.csv'
      Call GETLUN (fn,nf)

      Open (UNIT = nf, FILE = fn, FORM='FORMATTED', STATUS = 'REPLACE', &
          IOSTAT = ErrNum)
        
      Write(nf,'(A)')Header
      Deallocate(Header)    

      ptrN2O => headN2O
      Do
        If(.Not. Associated(ptrN2O)) Exit          
        Write(nf,'(A)') ptrN2O % pclineN2O
        ptrN2O => ptrN2O % pN2O
      End Do

      Nullify(ptrN2O, headN2O, tailN2O)
      Close(nf)
End Subroutine ListtofileN2O

!------------------------------------------------------------------------------
Subroutine LinklstN2O(ptxtlineN2O)

    Character(:), Allocatable :: ptxtlineN2O
        
    If(.Not. Associated(headN2O)) Then             
      Allocate(headN2O, Stat=istatN2O)        
      If(istatN2O==0) Then                         
        tailN2O => headN2O
        Nullify(tailN2O%pN2O)                 
        tailN2O%pclineN2O = ptxtlineN2O  
      Else
        ! Error message
      End If
    Else
      Allocate(tailN2O%pN2O, Stat=istatN2O)   
      If(istatN2O==0) Then                              
        tailN2O=> tailN2O%pN2O                
        Nullify(tailN2O%pN2O)                      
        tailN2O%pclineN2O = ptxtlineN2O       
      Else
      ! Error message
      End If
    End If

 End Subroutine LinklstN2O
!------------------------------------------------------------------------------
End Module Linklist
