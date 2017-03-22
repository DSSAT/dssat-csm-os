C=======================================================================
C  Aloha_IPCROP, Subroutine
C
C  Read crop parameters from SPECIES file
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written                                     P.W.W.      6-15-94
C  2. Header revision and minor changes           P.W.W.      2- 7-93
C  2. Added switch common block, restructured     P.W.W.      2- 7-93
C=======================================================================

      SUBROUTINE Aloha_IPCROP (FILEC,PATHCR)

      IMPLICIT    NONE

      !INCLUDE    'GEN2.BLK'
      !INCLUDE    'GEN3.BLK'
      !
      CHARACTER   CROP*2
      CHARACTER*4 ACRO(13)
      CHARACTER*1 BLANK*1,FILEC*12,PATHCR*80,CHAR*180,FILECC*92

      INTEGER     I,J,PATHL,LUNCRP,ERR,LINC

      PARAMETER (BLANK  = ' ')

      DATA ACRO /'P1GC','P6GC','CONV','P5GC','TBAS','CMFC',
     &           'FDMC','CO2X','CO2Y','RWEP','PORM','RWMX','RLWR'/
      !
      ! Default CO2 response of Pineapple
      !
      DATA CO2X /   0, 220, 330, 440, 550, 660, 770, 880, 990,9999/
      DATA CO2Y /0.00,0.81,1.00,1.03,1.06,1.10,1.13,1.16,1.18,1.25/
      !
      ! Defaults
      !
      P1      =     60
      P6      =     60
      CC      =   1.80
      P5      = 400.00
      TBASE1  =  16.00
      CMF     =   1.00
      FDMC    =   0.12
      RWUEP1  = 1.5
      PORMIN  = 0.02                              ! Minimum pore space
      RWUMX   = 0.03                              ! Max root water uptake
      RLWR    = 0.98                              ! Root length weight ratio

      LUNCRP = 10
      LINC   = 0
      PATHL  = INDEX (PATHCR,BLANK)
      IF (PATHL .LE. 1) THEN
        FILECC = FILEC
      ELSE
        FILECC = PATHCR(1:(PATHL-1)) // FILEC
      ENDIF
      OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)

C-----------------------------------------------------------------------
C     READ CROP PARAMETERS FROM FILEC
C-----------------------------------------------------------------------

      DO WHILE (.NOT. EOF (LUNCRP))
         READ (LUNCRP,'(A180)',IOSTAT=ERR) CHAR
         DO J = 1, 13
            IF (CHAR(10:13) .EQ. ACRO(J)) THEN
            SELECT CASE (J)
              CASE (1)
                READ (CHAR(16:20),'(I5)',  IOSTAT=ERR) P1
              CASE (2)
                READ (CHAR(16:20),'(I5)',  IOSTAT=ERR) P6
              CASE (3)
                READ (CHAR(16:20),'(F5.0)',IOSTAT=ERR) CC
              CASE (4)
                READ (CHAR(16:20),'(F5.0)',IOSTAT=ERR) P5
              CASE (5)
                READ (CHAR(16:20),'(F5.0)',IOSTAT=ERR) TBASE1
              CASE (6)
                READ (CHAR(16:20),'(F5.0)',IOSTAT=ERR) CMF
              CASE (7)
                READ (CHAR(16:20),'(F5.2)',IOSTAT=ERR) FDMC
              CASE (8)
                READ (CHAR(16:66),'(10F5.0)',IOSTAT=ERR)(CO2X(I),I=1,10)
              CASE (9)
                READ (CHAR(16:66),'(10F5.2)',IOSTAT=ERR)(CO2Y(I),I=1,10)
              CASE (10)
                READ (CHAR(16:21),'(F5.2)',IOSTAT=ERR) RWUEP1
              CASE (11)
                READ (CHAR(16:21),'(F5.2)',IOSTAT=ERR) PORMIN
              CASE (12)
                READ (CHAR(16:21),'(F5.2)',IOSTAT=ERR) RWUMX
              CASE (13)
                READ (CHAR(16:21),'(F5.2)',IOSTAT=ERR) RLWR
            END SELECT
            ENDIF
         END DO
      END DO

      CLOSE (LUNCRP)
      RETURN
      END Subroutine Aloha_IPCROP

