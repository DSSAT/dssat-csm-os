C***********************************************************************
C  PlantNBal, Subroutine
C 
C  Purpose: Provides output N balance for plant growth processes 
C     (file PlantN.bal).  Based on old NBAL.FOR, which combined
C     plant and soil N balances.
C
C  REVISION   HISTORY
C  01/01/1995 WTB Written.
C  06/09/1999 AJG Completely revised the soil N and SOM module, and made
C                 a new SOM module based on the CENTURY model.
C                 Also changed the following variable names:
C                  OLD       NEW                  OLD       NEW
C                 ------    ------               ------    ------ 
C                  ANH4      TNH4                 TSON      THUMN
C                  ANH4I     TNH4I                TIFON     TFON
C                  ANO3      TNO3                 TIFONI    TFONI
C                  ANO3I     TNO3I
C  01/01/2000 CHP/AJG Integrated the CENTURY-based and CERES-based SOM
C                 modules with CHP's modular structure.
C  03/16/2000 GH  Checked the new modular CROPGRO.
C  06/19/2001 GH  Modified output
C  03/07/2001 CHP Split plant and soil N balances.
C***********************************************************************
C      CALLED BY:  CROPGRO
C-----------------------------------------------------------------------
      SUBROUTINE FOR_PLANTNBAL(CONTROL, ISWITCH, 
     &    SEEDNI, TNLEAK, WTNFX, WTNLA, WTNLF, WTNLO,     !Input
     &    WTNNA, WTNNO, WTNNOD, WTNRA, WTNRO, WTNRT,      !Input
     &    WTNSA, WTNSD, WTNSDA, WTNSDO, WTNSH, WTNSHA,    !Input
     &    WTNSHO, WTNSO, WTNST, WTNUP,                    !Input
     &    WTNSR, WTNSRA, WTNSRO)                          !Input

C-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      EXTERNAL GETLUN, HEADER
      SAVE

      CHARACTER*1  IDETL, IDETN
      CHARACTER*13 PNBAL
      CHARACTER*30 FILEIO

      INTEGER DYNAMIC, ERRNUM, YRDOY
      INTEGER YRSIM, RUN, LUNPNC

      REAL SEEDNI, TNLEAK, 
     &  WTNALL, WTNFX, WTNHAR, WTNLA, WTNLF, WTNLO, WTNNA,
     &  WTNNO, WTNNOD, WTNOFF, WTNRA, WTNRO, WTNRT,
     &  WTNSA, WTNSD, WTNSDA, WTNSDO, WTNSH, WTNSHA, WTNSHO,
     &  WTNSO, WTNST, WTNUP

      REAL WTNSR, WTNSRA, WTNSRO

      LOGICAL FEXIST

!     The variable "CONTROL" is of type "ControlType".
      TYPE (ControlType) CONTROL

!     The variable "ISWITCH" is of type "SwitchType".
      TYPE (SwitchType) ISWITCH
!     ------------------------------------------------------------------
      IDETL   = ISWITCH % IDETL
      IDETN   = ISWITCH % IDETN
      IF (IDETL .EQ. 'N' .OR. IDETN .EQ. 'N') RETURN

      DYNAMIC = CONTROL % DYNAMIC
      FILEIO  = CONTROL % FILEIO
      RUN     = CONTROL % RUN
      YRDOY   = CONTROL % YRDOY
      YRSIM   = CONTROL % YRSIM

!     ------------------------------------------------------------------
      PNBAL = 'PlantNBal.OUT'
      CALL GETLUN('PNBAL', LUNPNC)
      INQUIRE (FILE = PNBAL, EXIST = FEXIST)
      IF (FEXIST) THEN
        OPEN (UNIT = LUNPNC, FILE = PNBAL, STATUS = 'OLD',
     &    IOSTAT = ERRNUM, ACCESS = 'APPEND')
      ELSE
        OPEN (UNIT = LUNPNC, FILE = PNBAL, STATUS = 'NEW',
     &    IOSTAT = ERRNUM)
        WRITE(LUNPNC,'("*PLANT N BALANCE")')
      ENDIF

!      CALL HEADER(RUNINIT, FILEIO, LUNPNC, RUN)
      CALL HEADER(RUNINIT, LUNPNC, RUN)
!     ------------------------------------------------------------------
!       Sum the N accumulated in all plant components.
        WTNALL = WTNLA + WTNSA + WTNRA + WTNSHA + WTNSDA + WTNNA+WTNSRA

!       Sum the N in all plant components at harvest.
        WTNHAR = WTNLF + WTNST + WTNRT + WTNSH + WTNSD + WTNNOD+WTNSR

!       Sum N in all plant components that senesced.
        WTNOFF = WTNLO + WTNSO + WTNRO + WTNSHO + WTNSDO + WTNNO+WTNSRO

!       Write output to PlantN.bal
        WRITE (LUNPNC,100)
  100   FORMAT (//,
     &    3X,'PLANT COMPONENT  HARVEST   SENESCED  TOTAL  BALANCE',/,
     &    3X,'---------------  -------- kg[N]/ha -------  -------')

        WRITE (LUNPNC,200) 
     &    WTNLF * 10.,  WTNLO * 10.,  WTNLA * 10.,    !Leaf
     &    WTNST * 10.,  WTNSO * 10.,  WTNSA * 10.,    !Stem
     &    WTNSR * 10.,  WTNSRO * 10., WTNSRA * 10.,   !Storage
     &    WTNSH * 10.,  WTNSHO * 10., WTNSHA * 10.,   !shell
     &    WTNSD * 10.,  WTNSDO * 10., WTNSDA * 10.,   !seed
     &    WTNRT * 10.,  WTNRO * 10.,  WTNRA * 10.,    !root
     &    WTNNOD * 10., WTNNO * 10.,  WTNNA * 10.,    !nodule
     &    WTNHAR * 10., WTNOFF * 10., WTNALL * 10.,   !total
     &    TNLEAK * 10., WTNALL * 10. + TNLEAK * 10.

  200   FORMAT (3X, 'Leaf N',   T19, 3F9.2, 
     &       /, 3X, 'Stem N',   T19, 3F9.2, 
     &       /, 3X, 'Storage N',   T19, 3F9.2, 
     &       /, 3X, 'Shell N',  T19, 3F9.2, 
     &       /, 3X, 'Seed N',   T19, 3F9.2, 
     &       /, 3X, 'Root N',   T19, 3F9.2, 
     &       /, 3x, 'Nodule N', T19, 3F9.2, 
     &       /, 3x, 'Total N',  T19, 3F9.2, 
     &       /, 3x, 'N leakage',T37, F9.2,
     &       /, 3x, 'TOTAL N',  T46, F9.2)

        WRITE (LUNPNC,300)
  300   FORMAT(//,
     &    '   N INPUTS TO SYSTEM                kg[N]/ha',/,
     &    '   ------------------                --------')

        WRITE (LUNPNC,400) SEEDNI * 10., WTNFX * 10., WTNUP * 10.,
     &    WTNUP * 10. + SEEDNI * 10. + WTNFX * 10. 

  400   FORMAT (3X, 'Seed N At Planting', T37, F9.2,
     &       /, 3X, 'N2 Fixed',           T37, F9.2,
     &       /, 3X, 'N Uptake from Soil', T37, F9.2,
     &       /, 3X, 'TOTAL N',            T46, F9.2,/)

        WRITE (LUNPNC,'(79("*"))')
        CLOSE (UNIT = LUNPNC)

!     ------------------------------------------------------------------
      RETURN
      END SUBROUTINE FOR_PLANTNBAL

!***********************************************************************
