!----------------------------------------------------------------------!
! SUBROUTINE NCROP                                                     !
! Authors:                                                             !
!          Version august, 503                                        !
! Date   : December 2001, Version: 1                                   !
! Modified: May 2009, TAOLI, present the interaction of NxW            ! 
!           Uptake involved massflow + diffusion                       ! 
! Purpose: This subroutine calculates the nitrogen dynamics in a rice  !
!          crop and the effects on growth and development              !
!                                                                      !
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      !
! name   type meaning                                     units  class !
! ----   ---- -------                                     -----  ----- !
! ITASK   I4  Task that subroutine should perform (-)               I  !
! IUNITD  I4  Unit that can be used for input files (-)             I  !
! IUNITL  I4  Unit number for log file messages (-)                 I  !
! FILEI1  C*  Name of file with crop data (-)                       I  !
! DELT    R4  Time step of integration (d)                          I  !
! TIME    R4  Time of simulation (d)                                I  !
! OUTPUT  R4  Flag to indicate if output should be done (-)         I  !
! TERMNL  L4  Flag to indicate if simulation is to stop (-)         I  !
! DVS     R4  Development stage of the crop (-)                     I  !
! LLV     R4  Loss rate of leaves caused by senescence (kg ha-1 d-1)I  !
! DLDR    R4  Loss rate of leaves caused by drought (kg ha-1 d-1)   I  !
! WLVG    R4  Dry weight of green leaves (kg ha-1)                  I  !
! WST     R4  Dry weight of stems (kg ha-1)                         I  !
! WSO     R4  Dry weight of storage organs (kg ha-1)                I  !
! GSO     R4  Growth rate of storage organs (kg ha-1 d-1)           I  !
! GST     R4  Growth rate of stems (kg ha-1 d-1)                    I  ! 
! GLV     R4  Growth rate of leaves (kg ha-1 d-1)                   I  !
! PLTR    R4  Intermediate variable for planting density (-)        I  !
! LAI     R4  Leaf area index (ha ha-1)                             I  !
! CROPSTA I4  Crop stage (-)                                        I  !
! TNSOIL  R4  Soil-N available for crop uptake (kg N ha-1 d-1)      I  !
! NACR    R4  Actual N uptake rate by the crop (kg ha-1 d-1)        O  !
! ANSO    R4  Nitrogen in storage (kg N/ha)                         O  !
! ANLV    R4  nitrogen in leaves (kg N/ha)                          O  !
! ANST    R4  Nitrogen in stems (kg N/ha)                           O  !
! ANLD    R4  Nitrogen in died leaves (kg N/ha)                     O  !
! ANRT    R4  Nitrogen in root (kg N/ha)                            O  !
! NFLV    R4  Nitrogen fraction in the leaves (g N m-2 leaf)        O  !
! NSLLV   R4  Stress factor for leaf death caused by N stress  (-)  O  !
! RNSTRS  R4  Decrease factor for RGRL caused by N stress (-)       O  !
!                                                                      !
! Subroutine called: SUBNBC                                            !
!                                                                      !
!----------------------------------------------------------------------*
      SUBROUTINE NCROP3 (ITASK, IUNITD, IUNITL, FILEI1, FILEI2, FILEIT, DELT, TIME, OUTPUT, &
                       TERMNL, DVS, LLV, DLDR, WLVG, WST, WSO, GSO, GST, GLV, GRT, &
                       PLTR, LAI, SLA, CROPSTA, TNSOIL, NACR, ANSO, ANLV, ANST, ANLD, &
                       ANRT, NFLV, NSLLV,NRT, RNSTRS)

      !USE CHART
      USE public_module		!VARIABLES
      use rootgrowth
      use Module_OutDat

        IMPLICIT NONE

!     Formal parameters
!     Input parameters
      INTEGER IUNITD, IUNITL, ITASK, SL, I
      CHARACTER (*) FILEI1, FILEI2, FILEIT

      LOGICAL OUTPUT, TERMNL, RDINQR

      REAL DELT, TIME
      REAL DVS, LLV, DLDR, WLVG, WST, WSO, PLTR, LAI, TNSOIL
      INTEGER CROPSTA

!     Ouput parameters
      REAL NACR, NFLV, NSLLV, RNSTRS, NFLV1

!     Local parameters
      INTEGER IMX
      PARAMETER (IMX=40)
      INTEGER ILNMAX, ILNMIN, ILNMNS, INSLLV, ILNFLV
      REAL RFNLV, RFNST, TCNTRF, ATNLV, ATNST, ATNRT, NUPP, NMAXUP
      REAL FNLV, FNST, FNSO, ANST, ANSO, ANCR, ANLV, ANCRPT
      REAL ANSTA, ANLVA, ANCRF, ANLD
      REAL NMAXL, NMINL, NMAXSO, NLDLV, NSHKLV, NSHKST, FNTRT
      REAL NALV, NAST, NASO, NALVS, NASTS, NASOS, NACRS, NTRTS
      REAL NDEMSX, NDEMC, NDEMS, NDEML, NDEMSN
      REAL ATN, NSO, NTSO, NTLV, NTST, NTRT, NLV, NSTAN, NST, NLVAN
      REAL NMINSO, GSO, GST, GLV, GRT, NFLVI, FNLVI, NFLVP
      REAL NMAXLT(IMX), NMINLT(IMX), NMINSOT(IMX), NSLLVT(IMX), NFLVTB(IMX)
      REAL NOTNUL, NCHCK, NBCHK, NSTRES, SLA
	  REAL RFNRT, RATIORL	!TAOLI 27 JULY 2009
	  REAL NMAXR, NMINR, FNRT		!TAOLI 27 JULY 2009
	  REAL NDEMRX, WROOTC, WROOTN, NART	!TAOLI 27 JULY 2009
	  REAL SMINNH4, SMINNO3, NSHKRT !TAOLI 27 JULY 2009N
	  REAL NARTS, NRT, ANRT	!TAOLI 27 JULY 2009
	  REAL RSNH4(10), RSNO3(10) !TAOLI 27 JULY 2009
	  real snh4x(15), sno3x(15), layt(15), bd(15), SDEP1, SDEP2, UpNCoeff  !TAOLI 27 JULY 2009
	  REAL tswc(15), SPNH4(15),SPNO3(15)  !TAOLI 27 JULY 2009
	  REAL NDSENS  !TAOLI,19MAR 2014, Nitrogen deficient sensitity to 
	               !primary production, 0.5= fair as default, 
	               !>0.5 tolerance, <0.5 sensitive
!	** Variables for using BioRice
	  REAL NUPTA, NUPTN	
	  
!     Functions
      REAL LINT2, INTGRL, LIMIT, INTGR2, TMPV1, TMPV2, TMPV3, TMPV4, TMPV5, TMPV6,tmpv7

      SAVE         !TAOLI

!-----Initialization
      IF (ITASK.EQ.1) THEN
!       Initialize variables
        NUPP  = 0.
        ANLV   = 0.
        ANSO   = 0.
        ANST   = 0.
		ANRT   = 0.
        ANLD   = 0.
        ANCR   = 0.
        ANLVA  = 0.
        ANSTA  = 0.
        ANCRF  = 0.
		FNRT   = 0.
        NALVS  = 0.
        NASTS  = 0.
        NASOS  = 0.
        NACRS  = 0.
		NRT    = 0.
		NARTS  = 0.
        NTRTS  = 0.
        NALV   = 0.
		NART   = 0.
        NAST   = 0.
        NASO   = 0.
        NACR   = 0.
        NLV    = 0.
        NST    = 0.
        NSO    = 0.
        NLDLV  = 0.
        NLVAN  = 0.
        NSTAN  = 0.
        NTRT   = 0.
        ANCRPT = 0.
        NBCHK  = 0.
        NCHCK  = 0.
		WROOTN = 0.
		WROOTC = 0.

        FNLV   = FNLVI
        FNST   = 0.5*FNLVI
        FNSO   = 0.
        NFLV   = NFLVI

        NSLLV  = 1.
        RNSTRS = 1.

!------ Reading input parameters (from crop  file)
        CALL RDINIT(IUNITD, IUNITL, FILEI1)
        CALL RDAREA('NMAXLT ',NMAXLT ,IMX,ILNMAX)
        CALL RDAREA('NMINLT ',NMINLT ,IMX,ILNMIN)
        CALL RDAREA('NMINSOT',NMINSOT,IMX, ILNMNS)
        CALL RDAREA('NSLLVT ',NSLLVT ,IMX,INSLLV)
        CALL RDAREA('NFLVTB',NFLVTB,IMX,ILNFLV)
        CALL RDSREA('NMAXSO', NMAXSO)
        CALL RDSREA('NMAXUP', NMAXUP)
        CALL RDSREA ('NFLVI', NFLVI)
        CALL RDSREA ('FNLVI', FNLVI)
        CALL RDSREA('RFNLV', RFNLV)
		IF(RDINQR('RCNL')) THEN
			CALL RDSREA('RCNL  ', RFNRT)			!TAOLI 27 JULY 2009
		ELSE
			RFNRT = RFNLV
			
		ENDIF
		IF(RDINQR('NDSENS')) THEN
			CALL RDSREA('NDSENS', NDSENS)			!TAOLI 18MAR 2014
		ELSE
			NDSENS = 0.5			
		ENDIF
		NDSENS = (NDSENS-0.5)*10.0
		NDSENS = 1.0-(1.0-2.73**(-NDSENS))/(1.0+2.73**(-NDSENS))
        CALL RDSREA('RFNST', RFNST)
        CALL RDSREA('TCNTRF', TCNTRF)
        CALL RDSREA('FNTRT', FNTRT)
        CLOSE (IUNITD)
		CALL RDINIT(IUNITD, IUNITL, FILEI2)
		IF(RDINQR('SMINNH4')) THEN
			CALL RDSREA('SMINNH4', SMINNH4)
		ELSE
			SMINNH4 = 5.  !2.5
		ENDIF
		IF(RDINQR('SMINNO3')) THEN
			CALL RDSREA('SMINNO3', SMINNO3)
		ELSE
			SMINNO3 = 2.5  !1.25
		ENDIF
		CLOSE(IUNITD)

        NMINSO  = LINT2('NMINSOT',NMINSOT,ILNMNS,ANCRF)
        NMAXL   = LINT2('NMAXLT',NMAXLT,ILNMAX,DVS)
        NMINL   = LINT2('NMINLT',NMINLT,ILNMIN,DVS)
        FNLV    = FNLVI
        NFLV    = NFLVI
		FNRT = FNLV * RFNRT/RFNLV	!TAOLI 27 JULY 2009


!=====Rate calculations
      ELSE IF (ITASK.EQ.2) THEN
!+	get public value for local use
		sl = pv%pnl; reffecd = pv%prootd
		do i = 1, sl
			layt(i) = pv%pdlayer(i)/1000.0;	snh4x(i) = pv%pnh4(i)  !ANH4X AND SNO3X ARE IN KG N/HA, PDLAYER IS IN mm, LAYT IN M
			sno3x(i) = pv%pno3(i);	bd(i) = pv%pbd(i)
			tswc(i) = pv%pswc(i)
		enddo

!------ Linear interpolation of parameter values
        NMINSO  = LINT2('NMINSOT',NMINSOT,ILNMNS,ANCRF)
        NMAXL   = LINT2('NMAXLT',NMAXLT,ILNMAX,DVS)
        NMINL   = LINT2('NMINLT',NMINLT,ILNMIN,DVS)
		RATIORL= 1.0  !RFNRT / RFNLV
		NMAXR = NMAXL* RATIORL      	!MAXIMUM N IN ROOT ON WEIGHT BASIS, TAOLI 27 JULY 2009
		NMINR = NMINL * RATIORL			!MINIMUM N IN ROOT ON WEIGHT BASIS, TAOLI 27 JULY 2009
!RCNL/RFNLV*LINT2('NMAXLT',NMAXLT,ILNMAX,DVS)
!====== Only calculations after in the main field
        IF (CROPSTA .GE. 4) THEN

!       Potential leaf N content (on LAI basis)
			NFLVP = LINT2('NFLVTB',NFLVTB,ILNFLV,DVS)

!========== Calculate (potential) N demand of crop organs
!           Maximum N demand of leaves
!            NDEML  = (NMAXL*(WLVG+GLV*DELT)-ANLV)/DELT            
            NDEML  = (NMAXL*(WLVG+GLV*DELT)-ANLV- &
                (LLV+DLDR)*max(0.0,ANLV/NOTNUL(WLVG)-RFNLV))/DELT  
                !Added by TAOLI 3Feb 2014, the second line is calculate 
                !the possible translocation from death leaves to living ones
            IF (NDEML .LT. 0.) NDEML = 0.
!           Maximum N demand of stems
            NDEMS  = (NMAXL*0.5*(WST+GST*DELT)-ANST)/DELT
            IF (NDEMS .LT. 0.) NDEMS = 0.
!           Maximum N demand of storage organs
            NDEMSX = NMAXSO*GSO !GSO in kg DM/ha/d
            IF (NDEMSX .LT. 0.) NDEMSX = 0.
!           Minimum nitrogen demand of storage organs 
            NDEMSN = NMINSO*GSO
            IF (NDEMSN .LT. 0.) NDEMSN = 0.
!-----------TAOLI 27 JULY 2009
			WROOTC = SUM(ROOTC(1:SL)) 
	        WROOTN=SUM(ROOTN(1:SL))			
			NDEMRX =MAX(0.0, (NMAXR * (WROOTC + GRT)- WROOTN)/DELT)
			
!-----------------------------------------------------------END THIS SECTION, 27 JULY 2009
						

!========== Calculate translocation of N from organs, in kg/ha/d
!           It is assumed that potential demand by storage organ is first met by translocation
!           No translocation before DVS = 0.95
            IF (DVS .LT. 0.95) THEN
              ATNLV = 0.
              ATNST = 0.
              ATN   = 0.
              NTSO = 0.
            ELSE
!             Maximum translocation amount from leaves and stems
              ATNLV = MAX(0., ANLV-WLVG*RFNLV)
              ATNST = MAX(0., ANST-WST*RFNST)
!             Maximum translocation amount from roots as fraction of that of shoot
              ATNRT = (ATNLV+ATNST)*FNTRT
!------------TAOLI, 27 JULY 2009
			  !ATNRT = MIN(ATNRT, (WROOTN + ANRT - WROOTC * NMINR))
			  ATNRT = MIN(ATNRT, (WROOTN  - WROOTC * NMINR))	

              ATN   = ATNLV+ATNST+ATNRT
!             Daily translocation is total pool divided by time constant
              NTSO = ATN/TCNTRF
!             Translocation is limited between minimum (NDEMSN) and maximum (NDEMSX)
              NTSO = LIMIT(NDEMSN,NDEMSX,NTSO)
            END IF

!---------- Actual N translocation rates from plant organs, in kg/ha/d
            NTLV  = NTSO*ATNLV/NOTNUL(ATN)
            NTST  = NTSO*ATNST/NOTNUL(ATN)
            NTRT  = NTSO*ATNRT/NOTNUL(ATN)
!           Sum total maximum uptake rates from leaves, stems and storage organs
            NDEMC  = (NDEML+NTLV)+(NDEMS+NTST)+(NDEMSX-NTSO) + (NDEMRX + NTRT)

!========== Calculate nitrogen uptake 
!------------TAOLI 27 JLUY 2009
             
            IF(WLVG.GT.0.0) THEN ! TAOLI 8 Sept 2012                
                UPNCOEFF = MAX(0.01,(ANLV/WLVG-0.9*nmaxl)/(NMAXL-NMINL))
            ELSE
                UPNCOEFF = 1.0
            END IF 
            
            UPNCOEFF = 1.0/UPNCOEFF
			!---Get sum of uptook water
			tmpv4 =sum(pv%PTrwl(1:SL))     
			TNSOIL = 0.0;TMPV3=0.0;SDEP1=0.0;SDEP2=0.0
		    DO I=1,SL
			    tmpv1 =0.0;tmpv2=0.0;tmpv3=0.0;SDEP2=SDEP2+LAYT(I)
			    tmpv5 = pv%pwcst(i)*layt(i)*1000.0  !TAOLI, 24June 2012
				tmpv6 = max(0.0, (tswc(i)-pv%pwcwp(i))*layt(i)*1000.0) 
			    IF(REFFECD.GT.SDEP2) THEN
				    tmpv1 =MAX(0.0, SNH4X(I)-SMINNH4*BD(I)*LAYT(I)*10.0/upncoeff )   
				    tmpv2 =MAX(0.0,SNO3X(I)-SMINNO3*BD(I)*LAYT(I)*10.0/upncoeff)
				    tmpv3 = tswc(i)*layt(i)*1000.0					    
				    IF((TMPV3.GT.0.0).AND.(TMPV4.GT.0.0)) THEN
					    SPNH4(I) = MIN(upncoeff*SNH4X(I) /tmpv3* (pv%PTrwl(i)+tmpv6/tmpv5/NDSENS),TMPV1)	
					    SPNO3(I)= MIN(upncoeff*SNO3X(I)/tmpv3* (pv%PTrwl(i)+tmpv6/tmpv5/NDSENS), tmpv2)	
					    !0.5/NDSENS, DONET TO NITROGEN DEFICIENCY TOLERANCE, TOLERANCE WILL INCREASE THE DIFFUSION UPTAKE, OTHER INVERSE
				    ELSE
					    SPNH4(I) = 0.0
					    SPNO3(I)= 0.0
				    ENDIF
				    TNSOIL =TNSOIL + SPNH4(I)
				    TNSOIL =TNSOIL + SPNO3(I)
			    ELSEIF((REFFECD.GT.SDEP1).AND.(REFFECD.LE.SDEP2)) THEN
				    tmpv1 =MAX(0.0, SNH4X(I)-SMINNH4*BD(I)*LAYT(I)*10.0/upncoeff)   
				    tmpv2 =MAX(0.0,SNO3X(I)-SMINNO3*BD(I)*LAYT(I)*10.0/upncoeff)   
				    tmpv3 = tswc(i)*1000.0*layt(i)
				    tmpv7 = (REFFECD-SDEP1)/LAYT(I)
				    IF((TMPV3.GT.0.0).AND.(TMPV4.GT.0.0)) THEN
					    SPNH4(I) = MIN(upncoeff*SNH4X(I) /tmpv3*(tmpv6/tmpv5*tmpv7/NDSENS+ pv%PTrwl(i)),TMPV1*tmpv7)
					    SPNO3(I)= MIN(upncoeff*SNO3X(I)/tmpv3*(tmpv6/tmpv5*tmpv7/NDSENS + pv%PTrwl(i)), tmpv2*tmpv7)
				    ELSE
					    SPNH4(I) = 0.0
					    SPNO3(I)=0.0
				    ENDIF	
				    TNSOIL =TNSOIL + SPNH4(I)
				    TNSOIL =TNSOIL + SPNO3(I)
			    ELSE
				    SPNH4(I) = 0.0
				    SPNO3(I) = 0.0
			    ENDIF
			    SDEP1=SDEP2	
		    ENDDO
		    
			!--------------END THIS SECTION, TAOLI 27 JULY 2009

            IF (CROPSTA .LT. 4) THEN
				NUPP=MAX(0.0,NDEMC)		!SUPPOSED THAT THERE IS NO NITROGEN STRESS IN SEED BED 
										!WHICH CORRESPOND TO NO WATER STRESS IN SEED BED 
			ELSE
                NUPP = MIN(NMAXUP, TNSOIL)
                IF (NUPP .LT. 0.) NUPP = 0.
			    NUPP=MAX(0.0,MIN(NDEMC,NUPP))
            END IF
!           Actual uptake per plant organ is minimum of availability and demand 
            NALV   = MAX(0.,MIN(NDEML+NTLV, NUPP*((NDEML+NTLV)/NOTNUL(NDEMC))))
            NAST   = MAX(0.,MIN(NDEMS+NTST, NUPP*((NDEMS+NTST)/NOTNUL(NDEMC))))
            NASO   = MAX(0.,MIN(NDEMSX-NTSO, NUPP*((NDEMSX-NTSO)/NOTNUL(NDEMC))))
			NART   = MAX(0.,MIN(NDEMRX+NTRT,NUPP*((NDEMRX+NTRT)/NOTNUL(NDEMC))))
!           Total uptake by crop from the soil
            NACR   = NALV+NAST+NASO + NART
			!ACTUAL DETECT RATE FOR EACH SOIL LAYER, TAOLI 27 JLUY 2009
			IF (CROPSTA .GE. 4) THEN
			    IF(NACR * DELT.EQ.TNSOIL) THEN !EACH LAYER GO TO THE MINIMUM CONTENT
				    DO I = 1, SL
					    RSNH4(I) = 0.0; RSNO3(I) = 0.0
					    RSNH4(I) = -SPNH4(I)
					    RSNO3(I) = -SPNO3(I)
					    snh4x(i) = snh4x(i) + rsnh4(i)
					    sno3x(i) = sno3x(i) + rsno3(i)
				    ENDDO
			    ELSE	
				    TMPV1 = NACR * DELT
				    IF(TNSOIL.GT.0.0) THEN
					    TMPV1 =TMPV1/TNSOIL
				    ELSE
					    TMPV1 = 0
				    ENDIF
				    DO I=1, SL
					    RSNH4(I) =  -TMPV1*SPNH4(I)
					    RSNO3(I) =  -TMPV1*SPNO3(I)
					    snh4x(i) = snh4x(i)+ rsnh4(i)
					    sno3x(i) = sno3x(i) + rsno3(i)					
				    ENDDO
				    tmpv2 = abs(sum(rsnh4) + sum(rsno3))
			    ENDIF
			END IF		
			
			tmpv1=NACR		!END THIS SECTION, TAOLI 27 JULY 2009
!========== Calculate net N flows to plant organs (daily rates)
!           Transplanting shock: remove N
            NSHKLV =ANLV*(1.-PLTR)
            NSHKST =ANST*(1.-PLTR)
			NSHKRT =ANRT*(1-PLTR)
!           Loss of N from leaves by leaf death
            NLDLV = (LLV+DLDR)*RFNLV
!NEW BB, aug 2003: with senescence and dying of leaves, the total leaf N
!           can go down. Yellow leaves initially have higher N content than RFNLV
            NLDLV  = MAX (NLDLV, ANLV-NMAXL*(WLVG+GLV-LLV-DLDR))
!           
!---------- Net flow to stems, leaves, AND ROOT
            NLV = NALV-NTLV-NLDLV-NSHKLV
            NST = NAST-NTST-NSHKST
			NRT = NART - NTRT - NSHKRT
!---------- Net N flow to storage organ
            NSO = NTSO+NASO
!---------- Net flow to stems and leaves before flowering 
            IF (DVS.LT. 1.) THEN
               NSTAN =NST
               NLVAN =NLV
            ELSE
               NSTAN = 0.
               NLVAN = 0.
            END IF

!====== End if statement for CROPSTA GT 0
        END IF

!        Output writing
         IF (OUTPUT) THEN
         !  CALL OUTDAT (2, 0, 'NDEML', NDEML)
         !  CALL OUTDAT (2, 0, 'NDEMC', NDEMC)
            CALL OUTDAT (2, 0, 'NSLLV', NSLLV)
            CALL OUTDAT (2, 0, 'RNSTRS', RNSTRS)
            CALL OUTDAT (2, 0, 'NUPP', NUPP)
           CALL OUTDAT (2, 0, 'ANCR', ANCR)
            CALL OUTDAT (2, 0, 'ANLV', ANLV)
          CALL OUTDAT (2, 0, 'ANLD', ANLD)
          CALL OUTDAT (2, 0, 'ANST', ANST)
           CALL OUTDAT (2, 0, 'ANSO', ANSO)
           CALL OUTDAT (2, 0, 'ANRT', ANRT)
           CALL OUTDAT (2, 0, 'NMAXL', NMAXL)
           CALL OUTDAT (2, 0, 'NMINL', NMINL)
           CALL OUTDAT (2, 0, 'FNLV', FNLV)
           CALL OUTDAT (2, 0, 'ROOTN', SUM(ROOTN))
           CALL OUTDAT (2, 0, 'ROOTC', SUM(ROOTC))           
           CALL OUTDAT (2, 0, 'SNH4', SUM(SNH4X(1:sl))) !changed for output whole profile nitrogen, TAOLI, 5Dec 2011
           CALL OUTDAT (2, 0, 'SNO3', sum(SNO3X(1:sl))) !changed for output whole profile nitrogen, TAOLI, 5Dec 2011
         END IF
!+		UPDATE SOIL MINERAL NITROGEN CONTENTS into public module values
		 DO I=1, SL
			pv%pnh4(i) = snh4x(i);	pv%pno3(i) = sno3x(i)
		 ENDDO 

!=====State updates/integration
      ELSE IF (ITASK.EQ.3) THEN

!------- N amount in plant organs
         ANSO  =INTGRL(ANSO,NSO, DELT)
         ANLV  =INTGRL(ANLV,NLV,DELT)
         ANST  =INTGRL(ANST,NST,DELT)
         ANLD  =INTGRL(ANLD,NLDLV,DELT)
		 ANRT  =INTGRL(ANRT,NRT, DELT)		!DOES NOT COUNT THE NITROGEN COMINT WITH NEW ROOT
         ANCR  =ANSO+ANLV+ANLD+ANST+ANRT
!--------SPLIT NITROGEN INTO LAYER ROOT NITROGEN ACCORDING TO ROOT CARBON CONTENT
			WROOTN=0.0; WROOTC = 0.0; TMPV1=NRT*DELT
		 DO I= 1, SL
			WROOTC = WROOTC + ROOTC(I)
		 ENDDO
		 IF(WROOTC.GT.0.0) THEN
			DO I =1 , SL
				WROOTN=WROOTN+ROOTN(I)
			ENDDO
		 ELSE
			ROOTN=0.0
		 ENDIF
		 TMPV3=ANCR+WROOTN

!------- N amount in plant organs before flowering
         ANLVA =INTGRL(ANLVA,NLVAN,DELT)
         ANSTA =INTGRL(ANSTA ,NSTAN,DELT)
         ANCRF =ANSTA+ANLVA

!------- Total N uptake from soil
         tmpv2=NALVS+NASTS+NASOS+NARTS
         NALVS = INTGRL(NALVS, NALV, DELT)
         NASTS = INTGRL(NASTS, NAST, DELT)
         NASOS = INTGRL(NASOS, NASO, DELT)
		 NARTS = INTGRL(NARTS, NART, DELT)
         NACRS = NALVS+NASTS+NASOS+NARTS
		 TMPV2=TMPV2+NACR
!------- Total N supply by translocation from roots
         NTRTS = INTGRL(NTRTS, NTRT, DELT)
		 		
!------- Nitrogen balance check
         NCHCK = ANCR - (NACRS)  
		 CALL SUBNBC3 (ANCR,NACRS,TIME,NBCHK,TERMNL)
!======= Calculate N contents and N stress factors (only if in main field)
         IF (CROPSTA .LT. 4) THEN
            FNLV   = FNLVI
            FNST   = 0.5*FNLVI
            FNSO   = 0.
            NFLV   = NFLVI
            NSLLV  = 1.
            RNSTRS = 1.
         ELSE
!---------- Fraction N in plant organs
            FNLV  =ANLV/NOTNUL(WLVG)
			!FNLV  =ANLVA/NOTNUL(WLVG)
            FNST  =ANST/NOTNUL(WST)
            FNSO  =ANSO/NOTNUL(WSO)
			FNRT  =ANRT/NOTNUL(WROOTC)
!           Leaf N content in g N/m2 leaf
            IF (LAI .EQ. 0.) THEN
              NFLV = NFLVI
            ELSE 
!              IF (LAI .LT.1. .AND. DVS .LT.1.) THEN
!                  NFLV = (FNLV/NMAXL)*NFLVP
!               ELSE
!                  NFLV = ANLV/(10.*LAI)
!               END IF
! Bouman, Feb 2004 This is a programming 'trick' to enable forcing of observed
!          NFLV as function of day-of-observation. Below the first observation
!          day, NFLV1 is used;l between first and last observation day, interpolated
!          observed values are used; after last observation day, NFLV1 is used again
!          Forcing is determined by the variable NFLV_FRC in the experiment data file.
            NFLV1 = MIN(NMAXL/(10.*SLA),FNLV/(10.*SLA))		!NFLV1 in g N/m2 leaf
            NFLV   = INTGR2(0., NFLV1, DELT, FILEIT, 'NFLV')
            END IF

!---------- Set N stress factor for leaf death
            ANCRPT =WLVG*NMAXL+WST*NMAXL*0.5+WSO*NMAXSO
            IF (ANCR .LE. 0.) THEN      !TAOLI, 8 MARCH 2012
              NSTRES = 2.
            ELSE
              NSTRES = ANCRPT/ANCR
            END IF
            IF (NSTRES .LT. 1.) NSTRES = 1.
            IF (NSTRES .GT. 2.) NSTRES = 2.
            IF(ISNaN(NSTRES)) then
                NSTRES = 2. 
            END IF           
            NSLLV = LINT2('NSLLVT', NSLLVT, INSLLV, NSTRES)

!---------- Set N stress factor for RGRL
            RNSTRS = MIN(1.0,MAX(0.0,(FNLV-0.9*NMAXL)/(NMAXL-0.9*NMAXL)))
            RNSTRS = (RNSTRS)**NDSENS  !TAOLI, 18MAR 2014
            IF (RNSTRS .GT. 1.) RNSTRS = 1.
            IF (RNSTRS .LT. 0.1) RNSTRS = 0.1		!LIMITED RNSTRS TO BE LARGER THAN 0.1 FOR RESIDUE EFFECTS
            
!======= End IF statement for main field
         END IF

!======= Termination of simulation when there is too little N in leaves
         IF (LAI .GT. 1. .AND. FNLV .LE. 0.5*NMINL) THEN
             WRITE(*,*), 'Leaf N < 0.5*MINIMUM; simulation stopped'
             CALL OUTCOM('Leaf N < 0.5*MINIMUM; simulation stopped')
             TERMNL = .TRUE.
         END IF

!-----Terminal output statements
      ELSE IF (ITASK.EQ.4) THEN
        IF(LEN_TRIM(PV%OPSTRING).GT.1) THEN 
            IF(INDEX(PV%OPSTRING,'ANCR').GT.0) CALL OPSTOR ('ANCR', INT(100.0*ANCR)/100.0)
!         CALL OPSTOR ('NACRS', NACRS)
!         CALL OPSTOR ('NTRTS', NTRTS) 
        ELSE
         CALL OPSTOR ('ANCR', INT(100.0*ANCR)/100.0)
!         CALL OPSTOR ('NACRS', NACRS)
!         CALL OPSTOR ('NTRTS', NTRTS)
        END IF
!     END ITASKS
      END IF

      RETURN
      END


!----------------------------------------------------------------------*
!  SUBROUTINE SUBNBC3                                                   *
!  Purpose: This subroutine checks the Crop Nitrogen Balance           *
!           and stops the simulation if the difference between         *
!           CHKIN and CHKFL exceeds 0.1 %                              *
!                                                                      *
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      *
! name   type meaning (unit)                                     class *
! ----   ---- ---------------                                    ----- *
! CHKIN   R4  Accumulated N in the crop (kg N ha-1)                 I  *
! CHKFL   R4  Sum of N supplied by soil and roots (kg N ha-1)       I  *
! TIME    R4  Time of simulation (d)                                T  *
! NBCHK   R4  Nitrogen balance check, relative value to the sums of    *
!             CHKIN and CHKFL (-)                                   O  *
! TERMNL  R4  Flag to indicate if simulation is to stop (-)         O  *
!                                                                      *
!  FILE usage : none                                                   *
!----------------------------------------------------------------------*
      SUBROUTINE SUBNBC3 (CHKIN,CKCFL,TIME,NBCHK,TERMNL)

      IMPLICIT NONE
!-----Formal parameters
      REAL    CHKIN,CKCFL,TIME,NBCHK
      LOGICAL TERMNL
      SAVE         ! TAOLI
 
      NBCHK = 2.0*(CHKIN-CKCFL)/(CHKIN+CKCFL+1.E-10)
 
      IF (ABS(NBCHK).GT.0.001) THEN
         WRITE (*,'(A,/,A,F8.3,2(A,F8.2),A, F6.1)') &
           '* * * Error in Nitrogen Balance, please check * * *', &
           ' NBCHK=',NBCHK,', CHKIN=',CHKIN,', CKCFL=',CKCFL,' at TIME=',TIME
         TERMNL = .TRUE.
      END IF
 
      RETURN
      END

