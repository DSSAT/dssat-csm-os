C ***********************************************************************
C METHMOD.FOR
C Routines to calculate methane and oxygen fluxes	in flooded soils.
C Original model developed by Jon Arah in Turbo Pascal.
C Translated into Fortran for use with CERES-Rice by Robin Matthews, April 1998
C ***********************************************************************
c
c Description of variables:
c
c h :depth step (m)
c steps : number of layers in the profile 
c
c Following are (51) arrays for each depth:
c   z       : depth m 
c   epsilon : air-filled porosity m3 m-3 
c   lamda   : root transmissivity s-1 
c   theta   : volumetric moisture fraction m3 m-3
c   L       : leaching rate m3 m-3 s-1
c
c Following are (4) arrays for each substance:
c   alpha   : solubility constant (aqueous/gaseous)	(mol/m3 aq : mol/m3 g)
c   solub   : solubility at 25°C (mol m-3)  
c   rate    : reaction rate mol m-2 s-1
c
c Following are (4,4) arrays for interactions between each substance:
c   eta     : inhibition efficiency sub1 on redn of sub2 
c   k       : Michaelis constant sub1 with sub2 mol m-3 
c
c Following are (4,51) arrays for each substance at each depth:
c   DD : diffusion constant m2 s-1 
c   LL : root transport factor s-1
c   VV : reaction potential mol m-3 s-1
c   yy : concentration mol m-3
c
C ***********************************************************************
C Module to define constants used by subroutines.
C ***********************************************************************
      MODULE MethaneConstants
        REAL :: large=1.e6
        REAL :: small=1.e-6
        REAL solub(4),alpha(4),ya0(4),ys0(4),Da(4),Dw(4)
        REAL eta(4,4),k(4,4),ebullk
        INTEGER om,o2,bb,ch4,steps,niterations
        
        DATA om,o2,bb,ch4/1,2,3,4/
        DATA steps,niterations/51,100/
!       ebullition coefficient (1/d)
        DATA ebullk/86400./
!       solubility of substances in water (mol/m3 at 25°C)
        DATA solub/0.0,1.23,0.0,1.31/
!       solubility constants (aqueous / gaseous)
        DATA alpha/0., 0.03, 1.e9, 0.03/ 
!       surface gas-phase concentration (mol/m3) o2, b, ch4
        DATA ya0/0., 7.76, -1.e-3, 7.5e-5/  
!       gaseous diffusion constants (m2/s) o2, b, ch4
!       (figures for O2 from Jones, 1983. Plants & Microclimate)
        DATA Da/0., 2.02e-5, 0.0, 1.06e-5/  
!       aqueous diffusion constants (m2/s) o2, b, ch4 
!       (figures for O2 from Jones, 1983. Plants & Microclimate)
        DATA Dw/0., 2.00e-9, 1.5e-9, 1.49e-9/  
!       inhibition factor, o2 on ch4 production (see text)
        DATA eta/0.,0.,0.,0.,
     &             0.,0.,0.,0.,
     &             0.,100.,0.,0.,
     &             0.,400.,200.,0./
!       Michaelis constants (mol/m3) for respn, O2ox, CH4ox
        DATA k/0.00,0.22,1.00,0.00,
     &           0.00,0.00,0.00,0.44,
     &           0.00,0.00,0.00,0.00,
     &           0.00,0.33,0.00,0.00/
      END MODULE MethaneConstants
C ***********************************************************************

C ***********************************************************************
C Module containing array variable declarations
C ***********************************************************************
      MODULE MethaneVariables
        TYPE TSubstance
          SEQUENCE
          INTEGER ID
          REAL Diffusion, Leaching, Production, Consumption, RootFluxIn,
     &           RootFluxOut, Ebullition, Storage, StorageFlux
        END TYPE
        TYPE (TSubstance) oxy, meth
        
        INTEGER Iterations1
        REAL z(51),epsilon(51),lamda(51),theta(51),L(51),rho(51)
        REAL DD(4,51),LL(4,51),VV(4,51),yy(4,51),inityy(4,51)
        REAL h,Lz,lamda_rho,difference1,TSubstrate
        LOGICAL FirstTime
        DATA oxy%ID,meth%ID/2,4/
        
!        CHP - need to reset for every run
!        DATA FirstTime/.TRUE./

      END MODULE MethaneVariables

C ***********************************************************************
C Subroutine to initialise variables.
C ***********************************************************************
      SUBROUTINE Setup(nlayrs)
      USE MethaneConstants
      USE MethaneVariables
      IMPLICIT NONE
      EXTERNAL interpolate_steps
      INTEGER nlayrs,i,s
      REAL t(51)
      
      DO s=o2,ch4
        IF(ya0(s).GT.0.) THEN
          ys0(s) = alpha(s) * ya0(s)
        ELSE
          ys0(s) = -ya0(s)
          ya0(s) = 0.0
        ENDIF
      ENDDO

      h = (z (nlayrs) - z (1)) / (steps - 1)
      CALL interpolate_steps (nlayrs, epsilon, h, z)
      CALL interpolate_steps (nlayrs, theta, h, z)
      CALL interpolate_steps (nlayrs, rho, h, z)
      CALL interpolate_steps (nlayrs, lamda, h, z)
      DO i=1,steps
        t(i) = VV(om,i)
      ENDDO
      CALL interpolate_steps (nlayrs, t,h,z)
      DO i=1,steps
        VV(om,i) = t(i)
      ENDDO
      DO i=1,steps
        t(i) = VV(o2,i)
      ENDDO
      CALL interpolate_steps (nlayrs, t,h,z)
      DO i=1,steps
        VV(o2,i) = t(i)
      ENDDO
c	DO i=1,steps
c	  t(i) = VV(bb,i)
c	ENDDO
c      CALL interpolate_steps (nlayrs, t,h,z)
      DO i=1,steps
        VV(bb,i) = 0.0 ! t(i)
      ENDDO
      DO i=1,steps
        t(i) = VV(ch4,i)
      ENDDO
      CALL interpolate_steps (nlayrs, t,h,z)
      DO i=1,steps
        VV(ch4,i) = t(i)
      ENDDO
      DO i=1,steps
        z (i) = z (1) + (i-1) * h
        if (theta(i).gt.0.0) then
          L(i) = Lz * theta(steps) / theta(i)
        else
          L(i) = 0.0
        endif
c	  lamda(i) = rho(i) * lamda_rho
        DO s=o2,ch4
          if (FirstTime) then
              yy (s, i) = ya0(s) * epsilon(i) + ys0(s) * theta(i)
          endif
          inityy(s,i) = yy(s,i)
          DD (s, i) = Da(s) * epsilon(i) + Dw(s) * theta(i)
          LL (s, i) = Da (s) * lamda (i)
        ENDDO
      ENDDO

      FirstTime = .false.

      RETURN
      END

C ***********************************************************************
C Subroutine to calculate the 'steady-state' fluxes.
C ***********************************************************************
      SUBROUTINE SteadyState 
      
      USE MethaneConstants
      USE MethaneVariables
      IMPLICIT NONE
      EXTERNAL store, Pseudo, compare, CalculateTotals
      INTEGER s,i
      REAL oldyy(4,51)
      REAL old_difference
      LOGICAL finished
      
      difference1 = large
      finished = .FALSE.
      iterations1 = 0
      DO WHILE (.NOT.finished)
        old_difference = difference1
        iterations1 = iterations1 + 1
c        write(29,'(i4,a5,51f6.2)') Iterations1,'  O2 ',(yy(o2,i),i=1,51)
c        write(29,'(i4,a5,51f6.3)') Iterations1,' CH4 ',(yy(ch4,i),i=1,51)
        CALL store(yy,oldyy)           
        DO s=o2,ch4                       
          CALL Pseudo(s,yy)                  
        ENDDO                         			 
        CALL compare (yy,oldyy,difference1)
c	write(29,'(f15.10)') difference1

c        finished = ((difference1.LE.0.01).OR.(iterations1.GE.100))
        finished = ((difference1.GE.old_difference)
     &                      .OR.(difference1.LE.small*steps))
        if (difference1.GE.old_difference) then
          CALL store(oldyy,yy)
          difference1 = old_difference
        endif

c	write(29,'(i4,a14,f8.3)') iterations1,' Difference = ',difference1

      ENDDO                            
    
      TSubstrate = 0.0
      DO i=1,steps
        TSubstrate = TSubStrate + VV(om,i) * h
      ENDDO
      CALL CalculateTotals(oxy)
      CALL CalculateTotals(meth)

C      write(29,'(i6,f10.5)') Iterations1,difference1

      RETURN
      END
C ***********************************************************************

C ***********************************************************************
C Solves for non-linear consumption etc by repeated application of 
C Newton-Raphson approximation to an initial guess yy              
C ***********************************************************************
      SUBROUTINE Pseudo (s,w)
      USE MethaneConstants
      USE MethaneVariables
      IMPLICIT NONE
      EXTERNAL replace, NewtonRaphson, add, assess, squash, average
      INTEGER s,iterations2   !,i
      REAL w(4,51),oldw(4,51)
      REAL dv(51)
      REAL olderror,error
      LOGICAL finished
      
      error = large
      finished = .FALSE.
      iterations2 = 0
      DO WHILE (.NOT. finished)
        olderror = error
        CALL replace (s,w,oldw)
        CALL NewtonRaphson (s,w,dv)
        CALL add (s,dv,w)
        CALL squash(s,w)
        CALL assess (s,oldw,w,error)
        IF (error.GT.olderror/2.) THEN
          CALL average(s,w,oldw,w)
          CALL assess (s,oldw,w,error)
        ENDIF
        iterations2 = iterations2 + 1
c       finished = ((error.LE.0.01).OR.(iterations2.ge.100))
        finished = ((error.GE.olderror).OR.(error.LE.small))
        if (error.GE.olderror) then
          CALL replace(s,oldw,w)
          error = olderror
        endif
      ENDDO

      RETURN
      END
C ***********************************************************************

C ***********************************************************************
C Procedure to calculate total fluxes from the system.
C ***********************************************************************
      SUBROUTINE CalculateTotals(sub)
      USE MethaneConstants
      USE MethaneVariables
      IMPLICIT NONE
      EXTERNAL YS, OO, PP, QQ, RR, SS
      TYPE (TSubstance) sub
      INTEGER i,s
      REAL OO,PP,QQ,RR,SS,ys
      
      s = sub%ID
      sub%Leaching = L(steps)	* ys(ch4,steps,yy)
      sub%RootFluxIn = 0.0
      sub%Production = 0.0
      sub%Consumption = 0.0
      sub%RootFluxOut = 0.0
      sub%Ebullition = 0.0
      sub%Storage = 0.0
      sub%StorageFlux = 0.0
      DO i=1,steps
        sub%RootFluxIn = sub%RootFluxIn + OO(s,i,yy) * h
        sub%Production = sub%Production + PP(s,i,yy) * h
        sub%Consumption = sub%Consumption + QQ(s,i,yy) * h
        sub%RootFluxOut = sub%RootFluxOut + RR(s,i,yy) * h
        sub%Ebullition = sub%Ebullition + SS(s,i,yy) * h
        sub%Storage = sub%Storage + yy(s,i) * h
        sub%StorageFlux = sub%StorageFlux + (yy(s,i)-inityy(s,i)) 
     &       * h / (3600.*24.)
      ENDDO
      
      RETURN
      END
C ***********************************************************************

C ***********************************************************************
C Subroutine for outputting the results of the Arah methane model.
C ***********************************************************************
      SUBROUTINE Report(fn)
      USE MethaneConstants
      USE MethaneVariables
      IMPLICIT None
      EXTERNAL YA, YS, OO, PP, QQ, RR, SS
      
      INTEGER i,fn
      REAL OO,PP,QQ,RR,SS,ya,ys
      CHARACTER Header*256

      write(fn,*) 'Steady-state model output'
      write(fn,*)
      write(fn,'(1x,a27,f6.3,a7,f6.3)') 
     &   'Solubilities (mol/m3): O2: ',solub(o2),'  CH4: ',solub(ch4)
      write(fn,*)
      write(fn,*)	'Fluxes (umol/m2/s):'
      Header = '       Diffusion     Leached  RootFluxIn  Production Con
     &sumption RootFluxOut  Ebullition'
      write(fn,'(A256)') Header
      write(fn,'(a4,7f12.4)') '  O2',oxy%Diffusion*1.e6,
     &  oxy%Leaching*1.e6,oxy%RootFluxIn*1.e6,oxy%Production*1.e6,
     &  oxy%Consumption*1.e6,oxy%RootFluxOut*1.e6,oxy%Ebullition*1.e6
      write(fn,'(a4,7f12.4)') ' CH4',meth%Diffusion*1.e6,
     &  meth%Leaching*1.e6,meth%RootFluxIn*1.e6,meth%Production*1.e6,
     &  meth%Consumption*1.e6,meth%RootFluxOut*1.e6,meth%Ebullition*1.e6

      Header = 'depth theta epsil   lamda   DD-O2  DD-CH4   VV-OM   VV-O
     &2  VV-ch4   [O2]o      [CH4]o    [O2]       [CH4]      [CH4]a   
     & [CH4]s  OO-CH4  PP-CH4  QQ-CH4  RR-CH4  SS-CH4'

      write(fn,*)
      write(fn,'(1x,A256)') Header
      DO i=1,steps
         write(fn,'(3f6.3,6f8.4,2(f8.4,f12.8),2f12.5,5f8.4)') 
     &     z(i),theta(i),epsilon(i),lamda(i),
     &     DD(o2,i)*1.e9,DD(ch4,i)*1.e9,
     &     VV(om,i)*1.e6,VV(o2,i)*1.e6,VV(ch4,i)*1.e6,
     &     inityy(o2,i),inityy(ch4,i),
     &     yy(o2,i),yy(ch4,i),ya(ch4,i,yy),ys(ch4,i,yy),
     &     OO(ch4,i,yy)*1.e6,PP(ch4,i,yy)*1.e6,QQ(ch4,i,yy)*1.e6,
     &     RR(ch4,i,yy)*1.e6,SS(ch4,i,yy)*1.e6
      ENDDO
      write(fn,*)

      RETURN
      END

C ***********************************************************************
C Newton-Raphson algorithm for error vector dv
C ***********************************************************************
      SUBROUTINE NewtonRaphson (s,y,dv)
      USE MethaneConstants
      USE MethaneVariables
      IMPLICIT NONE
      EXTERNAL get_a, get_b, get_c, get_d, getNR_b, getNR_d, Thomas
      INTEGER s
      REAL y(4,51)
      REAL a(51),b(51),c(51),d(51),NR_b(51),NR_d(51),dv(51)

      CALL get_a (s,a)
      CALL get_b (s,y,b)
      CALL get_c (s,c)
      CALL get_d (s,y,d)
      CALL getNR_b (s,y,NR_b)
      CALL getNR_d (s,y,a,b,c,d,NR_d)

      CALL Thomas (a,NR_b,c,NR_d,dv)
    
      RETURN
      END
C ***********************************************************************

C ***********************************************************************
C The following 4 routines calculate the coefficients for Newton-Raphson 
C solution of nonlinear tridiagonal problem 
C ***********************************************************************

!-----------------------------------------------------------------
      SUBROUTINE get_a (s,a) !2023-01-26 chp removed unused "y" argument
      USE MethaneConstants
      USE MethaneVariables
      IMPLICIT NONE
      EXTERNAL dys_dy
      INTEGER s,i
      REAL a(51),dys_dy  !y(4,51),
      a (1) = 0.
      DO i = 2,steps-1
          a (i) = DD (s, i-1) + 4. * DD (s, i) - DD (s, i+1)
     &              + 2. * h * L(i) * dys_dy(s,i)
      ENDDO
        a (steps) = 8. * DD (s, steps)
      RETURN
      END

!-----------------------------------------------------------------
      SUBROUTINE get_b (s,y,b)
      USE MethaneConstants
      USE MethaneVariables
      IMPLICIT NONE
      EXTERNAL Q_y, R_y, S_y, dys_dy
      INTEGER i,s
      REAL y(4,51),b(51),R_y,S_y,Q_y,dys_dy
      b (1) = -1.
      DO i = 2,steps-1
        b (i) = - 8. * DD (s, i)
     &        - 2. * h * (L(i+1) - L(i-1)) * dys_dy(s,i) 
     &        - 4. * h*h * (Q_y (s,i,y)
     &                        + R_y (s,i) 
     &                        + S_y (s,i,y))
      ENDDO
      b (steps) = - 8. * DD (s, steps)
     &        - 2. * h * (L(steps) - L(steps-1)) * dys_dy(s,steps) 
     &        - 4. * h*h * (Q_y (s,steps,y)
     &                        + R_y (s,steps) 
     &                        + S_y (s,steps,y))
      RETURN
      END

!-----------------------------------------------------------------
      SUBROUTINE get_c (s,c) !2023-01-26 chp removed unused "y" argument
      USE MethaneConstants
      USE MethaneVariables
      IMPLICIT NONE
      EXTERNAL dys_dy
      INTEGER s,i
      REAL dys_dy,c(51)  !y(4,51),
      c (1) = 0.
      DO i = 2,steps-1
          c (i) = DD (s, i+1) + 4. * DD (s, i) - DD (s, i-1)
     &              - 2. * h * L(i) * dys_dy(s,i)
      ENDDO
      c (steps) = 0.
      RETURN
      END

!-----------------------------------------------------------------
      SUBROUTINE get_d(s,y,d)
      USE MethaneConstants
      USE MethaneVariables
      IMPLICIT None
      EXTERNAL OO, PP
      INTEGER i,s
      REAL y(4,51),OO,PP,d(51)
      d(1) = y(s,1)
      DO i=2,steps
          d(i) = 4. * h*h * (OO (s,i,y) + PP (s,i,y))
      ENDDO
      RETURN
      END

!-----------------------------------------------------------------
      SUBROUTINE getNR_b (s,y,NR_b)
      USE MethaneConstants
      USE MethaneVariables
      IMPLICIT NONE
      EXTERNAL dys_dy, dQ_dy, dR_dy, dS_dy
      INTEGER i,s
      REAL y(4,51),NR_b(51),dR_dy,dS_dy,dQ_dy,dys_dy
      REAL q1,r1,s1,d1
      NR_b (1) = -1.	  
      DO i = 2,steps-1
        d1 = dys_dy(s,i)
        q1 = dQ_dy (s,i,y)
        r1 = dR_dy (s,i)
        s1 = dS_dy (s,i,y)

        NR_b (i) = - 8. * DD (s, i)
     &          - 2. * h * (L(i+1) - L(i-1)) * dys_dy(s,i) 
     &          - 4. * h*h * (dQ_dy (s,i,y)
     &                        + dR_dy (s,i) 
     &                        + dS_dy (s,i,y))
      ENDDO
      NR_b (steps) = - 8. * DD (s, steps)
     &          - 2. * h * (L(steps) - L(steps-1)) * dys_dy(s,steps) 
     &          - 4. * h*h * (dQ_dy (s,steps,y)
     &                        + dR_dy (s,steps) 
     &                        + dS_dy (s,steps,y))
      RETURN
      END

!-----------------------------------------------------------------
      SUBROUTINE getNR_d (s,y,a,b,c,d,NR_d)
      USE MethaneConstants
      IMPLICIT NONE
      INTEGER i,s
      REAL y(4,51),a(51),b(51),c(51),d(51),NR_d(51)
      NR_d (1) = b(1)*y(s, 1) + c(1)*y(s, 2) + d(1) 
      DO i = 2,steps-1
        NR_d (i) = a (i) * y (s, i-1) 
     &             + b (i) * y (s, i) 
     &             + c (i) * y (s, i+1) 
     &             + d (i) 
      ENDDO
      NR_d (steps) = a (steps) * y (s, steps-1) 
     &             + b (steps) * y (s, steps) 
     &             + d (steps) 
      RETURN
      END

C ***********************************************************************
c Phase conversion routines
C ***********************************************************************

!-----------------------------------------------------------------
C Convert bulk to soluble concentration

      REAL function ys (s,i,y)
      USE MethaneConstants
      USE MethaneVariables
      IMPLICIT NONE
      INTEGER i,s
      REAL y(4,51)
      IF (alpha(s).GE.large) THEN
crbm - inserted to prevent division by zero
          IF (theta(i).gt.0.0) THEN
          ys = y(s,i) / theta (i)
        ELSE
          ys = 0.0
        ENDIF
      ELSE
        ys = alpha(s) * y(s,i) / (epsilon(i) + alpha(s)*theta(i))
      ENDIF
      RETURN
      END

!-----------------------------------------------------------------
      REAL function dys_dy (s,i)
      USE MethaneConstants
      USE MethaneVariables
      IMPLICIT NONE
      INTEGER s,i
      IF (alpha(s).GE.large) THEN
crbm - inserted to prevent division by zero
        IF (theta(i).gt.0.0) THEN
          dys_dy = 1. / theta(i)
        ELSE
          dys_dy = 0.0
        ENDIF
      ELSE
        dys_dy = alpha (s) / (epsilon (i) + alpha (s) * theta (i))
      ENDIF
      RETURN
      END

!-----------------------------------------------------------------
C Convert bulk to gaseous concentration

      REAL function ya (s,i,y)
      USE MethaneConstants
      USE MethaneVariables
      IMPLICIT NONE
      INTEGER s,i
      REAL y(4,51)
      IF (alpha(s).GE.large) THEN
        ya = 0.
      ELSE
        ya = y (s, i) / (epsilon (i) + theta (i) * alpha (s))
      ENDIF
      RETURN
      END

!-----------------------------------------------------------------
      REAL function dya_dy (s,i)
      USE MethaneConstants
      USE MethaneVariables
      IMPLICIT NONE
      INTEGER s,i
      IF (alpha(s).GE.large) THEN
        dya_dy = 0.
      ELSE
          dya_dy = 1. / (epsilon (i) + theta (i) * alpha (s))
      ENDIF
      RETURN
      END


C ***********************************************************************
C Concentration-dependent rate functions, coefficients and differentials
C ***********************************************************************

!-----------------------------------------------------------------
C Inhibition

      REAL function II (s,i,y)
      USE MethaneConstants
      USE MethaneVariables
      IMPLICIT NONE
      EXTERNAL ys
      INTEGER s,i,su
      REAL y(4,51),factor,ys
      factor = 1.
      DO su = o2,s-1
         factor = factor * 1. / (1. + eta (su, s) * ys (su,i,y))
      ENDDO
      II = factor
      RETURN
      END

!-----------------------------------------------------------------
c Apparent production from root transport 

      REAL function OO (s,i,y)
      USE MethaneVariables
      IMPLICIT NONE
      EXTERNAL ya
      INTEGER s,i
      REAL y(4,51),ya
      OO = LL (s, i) * ya (s, 1, y)
      RETURN
      END

!-----------------------------------------------------------------
c Production

      REAL function PP (s,i,y)
      USE MethaneConstants
      USE MethaneVariables
      IMPLICIT NONE
      EXTERNAL II
      INTEGER s,i
      REAL y(4,51),II
      SELECT CASE (s)
          CASE(1) 
          PP = 0.
        CASE(2)
            PP = 0.
        CASE(3)
            PP = 0.
        CASE(4)
            PP = II (ch4, i, y) * VV (om, i)
      ENDSELECT
      RETURN
      END

!-----------------------------------------------------------------
c Consumption 

      REAL function QQ (s,i,y)
      USE MethaneConstants
      USE MethaneVariables
      IMPLICIT NONE
      EXTERNAL ys
      INTEGER s,i
      REAL y(4,51),ys
          SELECT CASE (s)
          CASE(1)
              QQ = 0.
          CASE(2)
              QQ = VV (o2, i) * ys (o2, i, y) / (k (o2, om) 
     &                 + ys (o2,i,y))
     &                 + 2. * VV (ch4, i) * ys (o2, i, y) 
     &                    / (k (o2, ch4)+ ys (o2,i,y))
     &                 * ys (ch4, i, y) /
     &                 (k (ch4, o2) + ys (ch4,i,y))
          CASE(3)
              QQ = VV (bb,i) * ys (bb, i, y) / 
     &                (k (bb,om) + ys (bb, i, y))
            CASE(4)
            QQ = VV (ch4, i) * ys (o2, i, y) /
     &                 (k (o2, ch4) + ys (o2, i, y))
     &                 * ys (ch4,i,y) /
     &                 (k (ch4, o2) + ys (ch4,i,y))
          ENDSELECT  
      RETURN
      END

!-----------------------------------------------------------------
      REAL function Q_y (s,i,y)
      USE MethaneConstants
      USE MethaneVariables
      IMPLICIT NONE
      EXTERNAL dys_dy, ys
      INTEGER s,i
      REAL y(4,51),dys_dy,ys
      SELECT CASE (s)
        CASE(1)
          Q_y = 0.0
        CASE(2)
          Q_y = VV(o2,i) * dys_dy(o2,i) /
     &            (k(o2,om) + ys (o2, i, y)) + 
     &             2. * VV(ch4,i) * dys_dy(o2,i) /
     &            (k(o2,ch4) + ys(o2,i,y)) *
     &             ys(ch4,i,y) /
     &             (k(ch4,o2) + ys(ch4,i,y))
        CASE(3)
          Q_y = VV(bb,i) * dys_dy(bb,i) /
     &            (k(bb,om) + ys (bb, i, y))
        CASE(4)
          Q_y = VV(ch4,i) * ys(o2,i,y) /
     &            (k(o2,ch4) + ys(o2,i,y)) *
     &             dys_dy (ch4, i) /
     &            (k(ch4,o2) + ys(ch4,i,y))
      END SELECT
      RETURN
      END

!-----------------------------------------------------------------
      REAL function dQ_dy (s,i,y)
      USE MethaneConstants
      USE MethaneVariables
      IMPLICIT NONE
      EXTERNAL ys, dys_dy
      INTEGER s,i
      REAL y(4,51),dys_dy,ys
        SELECT CASE (s)
          CASE(1)
          dQ_dy = 0.
        CASE(2)
            dQ_dy = (VV (o2, i) * K (o2, om) /
     &                   ((K (o2, om) + ys (o2, i, y)) *
     &                   (K (o2, om) + ys (o2, i, y)) +
     &                   2. * VV (ch4, i) * ys (o2, i, y) /
     &                   (K (o2, ch4) + ys (o2, i, y)) *
     &                   K (ch4, o2) /
     &                   ((K (ch4, o2) + ys (ch4, i, y)) *
     &                   (K (ch4, o2) + ys (ch4, i, y))))) *
     &                   dys_dy (s, i)
        CASE(3)
          dQ_dy = VV (bb, i) * k (bb, om) /
     &                ((k (bb, om) + ys (bb, i, y)) *
     &                (k (bb, om) + ys (bb, i, y)))
          CASE(4)
          dQ_dy = (VV (ch4, i) * ys (o2, i, y) /
     &                       (k (o2, ch4) 
     &                         + ys (o2, i, y)) *
     &                       k (ch4, o2) /
     &                       ((k (ch4, o2) 
     &                         + ys (ch4, i, y)) *
     &                       (k (ch4, o2) 
     &                         + ys (ch4, i, y))))
     &                       * dys_dy (s,i)
        ENDSELECT 
      RETURN
      END

!-----------------------------------------------------------------
c Root-mediated transport

!-----------------------------------------------------------------
      REAL function RR (s,i,y)
      USE MethaneVariables
      IMPLICIT NONE
      EXTERNAL ya
      INTEGER s,i
      REAL y(4,51),ya
      RR = LL (s, i) * ya (s, i, y)
      RETURN
      END

!-----------------------------------------------------------------
!2023-01-26 chp removed unused "y" argument
      REAL function R_y (s,i) 
      USE MethaneVariables
      IMPLICIT NONE
      EXTERNAL dya_dy
      INTEGER s,i
      REAL dya_dy  !y(4,51),
        R_y = LL (s, i) * dya_dy (s, i)
      RETURN
      END

!-----------------------------------------------------------------
!2023-01-26 chp removed unused "y" argument
      REAL function dR_dy (s,i) 
      USE MethaneVariables
      IMPLICIT NONE
      EXTERNAL dya_dy
      INTEGER s,i
      REAL dya_dy  !y(4,51),
          dR_dy = LL (s, i) * dya_dy (s, i)
      RETURN
      END

!-----------------------------------------------------------------
c Ebullition

!-----------------------------------------------------------------
      REAL function SS (s,i,y)
      USE MethaneConstants
      USE MethaneVariables
      IMPLICIT NONE
      EXTERNAL ys
      INTEGER s,i
      REAL y(4,51),ys 
      if (ys(s,i,y).gt.solub(s)) then
        SS = (ys(s,i,y) - solub(s)) / ebullk
      else
        SS = 0.0
      endif
      RETURN
      END

!-----------------------------------------------------------------
      REAL function S_y (s,i,y)
      USE MethaneConstants
      USE MethaneVariables
      IMPLICIT NONE
      EXTERNAL ys, dys_dy
      INTEGER s,i
      REAL y(4,51),dys_dy,ys  
        if (ys(s,i,y).gt.solub(s)) then
        S_y = (ys(s,i,y) - solub(s)) / ebullk / y (s, i)
      else
        S_y = 0.0
      endif
      RETURN
      END

!-----------------------------------------------------------------
      REAL function dS_dy (s,i,y)
      USE MethaneConstants
      USE MethaneVariables
      IMPLICIT NONE
      EXTERNAL ys, dys_dy
      INTEGER s,i
      REAL y(4,51),dys_dy,ys 
        if (ys(s,i,y).gt.solub(s)) then
        dS_dy = dys_dy (s, i) / ebullk
      else
        dS_dy = 0.0
      endif
      RETURN
      END


C ***********************************************************************
C Following routines are general matrix handling procedures
C ***********************************************************************

!-----------------------------------------------------------------
      SUBROUTINE Replace(s,u,v)
      USE MethaneConstants
      IMPLICIT NONE
      INTEGER i,s
      REAL U(4,51),V(4,51)
      DO i = 1,steps
        v(s,i) = u(s,i)
      ENDDO
      RETURN
      END

!-----------------------------------------------------------------
      SUBROUTINE store(x,y)
      USE MethaneConstants
      IMPLICIT NONE
      EXTERNAL replace
      INTEGER s
      REAL X(4,51),Y(4,51)
        DO s=o2,ch4
          CALL replace (s, x, y)
        ENDDO
      RETURN
      END

!-----------------------------------------------------------------
      SUBROUTINE add(s,v,w)
      USE MethaneConstants
      IMPLICIT NONE
      INTEGER i,s
      REAL W(4,51),V(51)
        DO i = 1,steps
          w(s,i) = w(s,i) + v(i)
        ENDDO
      RETURN
      END

!-----------------------------------------------------------------
      SUBROUTINE assess (s,v,w,e)
      USE MethaneConstants
      IMPLICIT NONE
      INTEGER i,s
      REAL e,t1,t2,v(4,51),w(4,51)
      e = 0.
      t1 = 0.0
      t2 = 0.0
      DO i = 1,steps
        e = e + abs (v (s, i) - w (s, i))
        t1 = t1 + v (s, i)
        t2 = t2 + w (s, i)
      ENDDO
      if ((t1 + t2).gt.0.0) then
        e = e * 2. / (t1 + t2)
      endif
      RETURN
      END

!-----------------------------------------------------------------
      SUBROUTINE compare (x,y,e)
      USE MethaneConstants
      IMPLICIT NONE
      EXTERNAL assess
      INTEGER s,c
      REAL e,err,x(4,51),y(4,51)
      e = 0.0
      c = 0
      DO s = o2,ch4
        CALL assess (s, x, y, err)
        e = e + err
        c = c + 1
      ENDDO
      if (c.gt.0) then
        e = e / c
      endif
      RETURN
      END

!-----------------------------------------------------------------
      SUBROUTINE average(s,v,w,x)
      USE MethaneConstants
      IMPLICIT NONE
      INTEGER s,i
      REAL x(4,51),v(4,51),w(4,51)
      DO i = 1,steps
           x (s, i) = (v (s, i) + w (s, i)) / 2.
      ENDDO
      RETURN
      END

!-----------------------------------------------------------------
      SUBROUTINE squash(s,w)
      USE MethaneConstants
      IMPLICIT NONE
      INTEGER s,i
      REAL w(4,51)
      DO i=1,steps
        if (w(s,i) .LT. 0.) then 
          w(s,i) = 0.
        endif
      ENDDO
      DO i=2,steps-1
        IF ((w(s,i-1).GT.w(s,i)).AND.(w(s,i+1).GT.w(s,i))) THEN
          w(s,i) = (w(s,i-1) + 2.*w(s,i) + w(s,i+1))/4.
        ENDIF
      ENDDO
      RETURN
      END

C ***********************************************************************

C ***********************************************************************
C Thomas algorithm - solves tridiagonal matrices
      SUBROUTINE Thomas (a,b,c,d,e)
      USE MethaneConstants
      IMPLICIT NONE
      INTEGER i
      REAL a(51),b(51),c(51),d(51),mu(51),nu(51),e(51)
 
!     zero divide checks 2022-04-13 chp / gh
      DO i = 1,steps
        if (i==1) then
          mu(1) = b(1)
          nu(1) = -d(1)
        else
          mu (i) = b (i) - a (i) * c (i-1) / mu (i-1)
          nu (i) = -d (i) - a (i) * nu (i-1) / mu (i-1)
        endif

        if (abs(mu(i)) .LT. 1.E-30) then
          if (mu(i) .GT. 0.0) THEN
            mu(i) = 1.E-30
          else
            mu(i) = -1.E-30
          endif
        endif

      ENDDO

!     CHP 2022-02-07 Added logic to prevent zero-divide
!     e (steps) = nu (steps) / mu (steps)
      IF(abs(nu(steps)) < 1.E-30) THEN
        e(steps) = 0.0
      ELSEIF(abs(mu(steps)) > 1.E-30) THEN
        e (steps) = nu (steps) / mu (steps) !<-original eqn.
      ELSE
        e(steps) = e(steps-1)
        mu(steps) = mu(steps-1)
      ENDIF

      DO i = steps-1,1,-1
         e (i) = (nu (i) - c (i) * e (i+1)) / mu (i)
      ENDDO
    
      RETURN
      END
C ***********************************************************************


      SUBROUTINE interpolate_steps (j,v,h,z)
      USE MethaneConstants
      IMPLICIT NONE
      INTEGER j,upper,lower,a,i
      REAL v(51),oldv(51),h,z(51)
      
      DO i = 1,j
        oldv (i) = v (i)
      ENDDO
      DO i = 2,steps-1
        DO a = j,1,-1
          IF (z (a) .GE. z (1) + (i-1) * h) upper = a
        ENDDO
        DO a = 1,j
          IF (z (a) .LE. z (1) + (i-1) * h) lower = a
        ENDDO
        IF (upper .EQ. lower) then
           v (i) = oldv (upper)
        ELSE
           v (i) = oldv (lower) * (z (upper) - (z (1) + (i-1) * h)) 
     &	           / (z (upper) - z (lower)) +
     &                oldv (upper) * ((z (1) + (i-1) * h) - z (lower)) 
     &               / (z (upper) - z (lower))
        ENDIF
      ENDDO
      v (steps) = oldv (j)
      RETURN
      END



