! DvK, 16-Dec-1999
!      this routine is very similar to the routine version 1.1 except that
!      this one considers it an error if there are no data available
!      to be written to the output file. This was there in previous versions
!      of opsys but taken out for a reason unknown to me now. This routine
!      does not contain the bug in cumulative frequency calculations that was
!      there in previous versions

      SUBROUTINE OPSYS (ITASK,IUNIT,VARNAM,VARVAL,VARMIN,VARMAX,XLAB, &
                        VAR2NM,VAR2MN,VAR2MX,VAR2LB,INCL)

      IMPLICIT NONE

!     formal parameters
      INTEGER       ITASK,IUNIT, INCL
      CHARACTER (*) VARNAM,VAR2NM
      REAL          VARVAL,VARMIN,VARMAX,XLAB,VAR2MN,VAR2MX,VAR2LB

!     local parameters

!     IMNV - Maximum number of variables
!     IMNP - Maximum number of points per variable
!     INCL - Number of classes to use for frequency distribution

      INTEGER        IMNV, IMNP, IMNCL
! BOUMAN: CHANGED 15-> 25 IMNV      PARAMETER     (IMNV=15,IMNP=1000, IMNCL=30)
      PARAMETER     (IMNV=25,IMNP=1000, IMNCL=30)
      REAL           VALA(IMNP,IMNV),VALMX(IMNV),VALMN(IMNV),HULP(IMNP)
      CHARACTER (11) LNAM, LNAM2, NAMA(IMNV)
      INTEGER        IWBEG(IMNV),IWEND(IMNV)

      CHARACTER (1000) LINE
      CHARACTER (80)   FILENM
      INTEGER          INP(IMNV),INDX(IMNP), ICL(IMNCL)
      INTEGER          INV,I1,I2,IFINDC,ITOLD,ISUM,IV,IV2,IM
      REAL             TMP1, TMP2, XTMP,YTMP,SUM
      REAL             RDIFF
      LOGICAL          EOF, NODATA, LTMP
      SAVE

      DATA ITOLD /0/, NODATA /.TRUE./

      IF (ITASK.EQ.1) THEN

!        initialize

!        reset sum, maximum and minimum
         DO I1=1,IMNV
            VALMX(I1) = 0.
            VALMN(I1) = 0.
            INP(I1)   = 0
            DO I2=1,IMNP
               VALA(I2,I1) = -99.
            END DO
         END DO
         INV = 0

         NODATA = .TRUE.

      ELSE IF (ITASK.EQ.2) THEN

!        store data

         IF (ITOLD.NE.1.AND.ITOLD.NE.2) &
            CALL FATALERR ('OPSYS','wrong ITASK')

         LNAM = VARNAM

         IV = IFINDC (NAMA,IMNV,1,INV,LNAM)

         IF (IV.EQ.0) THEN
!           name not in array, add
            INV = INV+1
            IF (INV.GT.IMNV) CALL FATALERR ('OPSYS','too many variables')
            NAMA(INV) = LNAM
            IV = INV
         END IF

         IF (INP(IV).LT.IMNP) THEN
!           add data to specific array
            INP(IV) = INP(IV)+1
            VALA(INP(IV),IV) = VARVAL
         ELSE
            CALL FATALERR ('OPSYS','too many data, increase IMNP')
         END IF

      ELSE IF (ITASK.EQ.3) THEN

!        write table to output file
         IF (INV.EQ.0) CALL FATALERR ('OPSYS','no data for output file')
         CALL FOPENG (IUNIT,'OP.DAT','NEW','FS',0,'DEL')

!         WRITE (IUNIT,'(A,T12,255A12)') 'RUNNUM',(NAMA(I1),I1=1,INV)
         WRITE (IUNIT,'(A,T12,255(1x,A12))') 'RUNNUM',(NAMA(I1),I1=1,INV) !TAOLI, 3JUNE 2011 
!        determine overall maximum number data
         IM = 0
         DO I1=1,INV
            IF (INP(I1).GT.IM) IM = INP(I1)
         END DO

         DO I1=1,IM
!            WRITE (IUNIT,'(I6,T12,255G12.5)') I1,(VALA(I1,I2),I2=1,INV)
            WRITE (IUNIT,'(I6,T12,255G12.5)') I1,(VALA(I1,I2),I2=1,INV) !TAOLI, 3JUNE 2011
         END DO

         CLOSE (IUNIT)

      ELSE IF (ITASK.EQ.4) THEN

!        read table from output file
         CALL FOPENG (IUNIT,'OP.DAT','OLD','FS',0,' ')

         DO I1=1,IMNV
            INP(I1) = 0
            DO I2=1,IMNP
               VALA(I2,I1) = -99.
            END DO
         END DO

         CALL GETREC (IUNIT,LINE,EOF)
         CALL WORDS (LINE,IMNV,' ,',IWBEG,IWEND,INV)

         INV = INV-1
         DO I1=1,INV
            NAMA(I1) = LINE(IWBEG(I1+1):IWEND(I1+1))
         END DO

         DO WHILE (.NOT.EOF)
            CALL GETREC (IUNIT,LINE,EOF)
            IF (.NOT.EOF) THEN
               CALL DECREC (LINE,INV+1,HULP)
               DO I1=1,INV
                  IF (HULP(I1+1).NE.-99.) THEN
                     INP(I1) = INP(I1)+1
                     VALA(INP(I1),I1) = HULP(I1+1)
                  END IF
               END DO
            END IF
         END DO
         CLOSE (IUNIT)
         NODATA = .FALSE.

      ELSE IF (ITASK.EQ.5) THEN

!        create cumulative frequency distribution

!        lookup name in array of names
         LNAM = VARNAM
         IV   = IFINDC (NAMA,IMNV,1,INV,LNAM)
         IF (IV.EQ.0) CALL FATALERR ('OPSYS','name not found')
         IF (INP(IV).EQ.0) CALL FATALERR ('OPSYS','no data for name')

!        create sorted index array based on values for variable
! chp 08 Dec 2011. I get this error - for now comment out this line
!   because DSSAT-ORYZA never gets here.  I can't compile this version.
! error #6633: The type of the actual argument differs from the type 
!  of the dummy argument.   [VALA]

!   This is the offending statement:
!         CALL INDEXX (INP(IV),VALA(1,IV),INDX)

!   It seems that VALA(1,IV) is a scalar - one element of array VALA.
!      SUBROUTINE INDEXX(N,ARRIN,INDX)
!      Integer, intent(in):: N   !added by TAOLI, 25Oct 2011
!      INTEGER, DIMENSION(N):: ARRIN,INDX  !modified by TAOLI, 25OCT 2011 from DIMENSION ARRIN(N), INDX(N)

         I1 = LEN_TRIM (VARNAM)
         FILENM = 'CF'//VARNAM(1:I1)//'.TTP'
         CALL TTHEAD (IUNIT,FILENM,'Cumulative frequency distribution', &
                      VARMIN,VARMAX,XLAB,XLAB,VARNAM, &
                      0.,1.,0.2,0.2,' ', &
                      0.,0.)
         WRITE (IUNIT,'(A,/,A,/)') '*','1 1 1'

         SUM = 0.
         DO I1=1,INP(IV)
            SUM = SUM+VALA(I1,IV)
         END DO

         TMP1 = 0.
         DO I1=1,INP(IV)
            IF (VALA(INDX(I1),IV).GE.VARMIN.AND. &
                VALA(INDX(I1),IV).LE.VARMAX) THEN
               TMP1 = TMP1+VALA(INDX(I1),IV)
               WRITE (IUNIT,*) VALA(INDX(I1),IV),TMP1/SUM
            ELSE
               WRITE (*,'(A,G12.5,A)') &
                ' Warning: the value ',VALA(INDX(I1),IV), &
                ' is outside the required bounds.'
            END IF
         END DO
         CLOSE (IUNIT)

      ELSE IF (ITASK.EQ.6) THEN

!        create normal frequency distribution

         LNAM = VARNAM
         IV   = IFINDC (NAMA,IMNV,1,INV,LNAM)
         IF (IV.EQ.0) CALL FATALERR ('OPSYS','name not found')
         IF (INP(IV).EQ.0) CALL FATALERR ('OPSYS','no data for name')
         IF (INCL.GT.IMNCL) CALL FATALERR ('OPSYS','too many classes')

         RDIFF  = (VARMAX-VARMIN)/INCL

         DO I1=1,INCL
            ICL(I1) = 0
         END DO

         ISUM = 0
         DO I1=1,INP(IV)
            LTMP = .FALSE.
            DO I2=1,INCL
               TMP1 = VARMIN+REAL(I2-1)*RDIFF
               TMP2 = VARMIN+REAL(I2)*RDIFF
               IF (VALA(I1,IV).GE.TMP1.AND.VALA(I1,IV).LT.TMP2) THEN
                  ICL(I2) = ICL(I2)+1
                  ISUM = ISUM+1
                  LTMP = .TRUE.
               END IF
            END DO
            IF (.NOT.LTMP) THEN
               WRITE (*,'(A,G12.5,A)') &
                ' Warning: the value ',VALA(I1,IV), &
                ' is outside the required categories.'
            END IF
         END DO

         I1 = LEN_TRIM (VARNAM)
         FILENM = 'NF'//VARNAM(1:I1)//'.TTP'
         CALL TTHEAD (IUNIT,FILENM,'Frequency distribution', &
                      VARMIN,VARMIN+RDIFF*INCL,RDIFF,RDIFF,VARNAM, &
                      0.,1.,0.2,0.2,' ', &
                      0.,0.)
         WRITE (IUNIT,'(A,/,A,/)') '*','1 1 0'

         DO I1=1,INCL
            XTMP = VARMIN+(I1-1)*RDIFF
            YTMP = REAL(ICL(I1))/REAL(INP(IV))
            WRITE (IUNIT,*) XTMP,0.
            WRITE (IUNIT,*) XTMP,YTMP
            WRITE (IUNIT,*) XTMP+RDIFF,YTMP
            WRITE (IUNIT,*) XTMP+RDIFF,0.
         END DO
         CLOSE (IUNIT)

      ELSE IF (ITASK.EQ.7) THEN

!        create scatter plot

         LNAM  = VARNAM
         LNAM2 = VAR2NM
         IV  = IFINDC (NAMA,IMNV,1,INV,LNAM)
         IV2 = IFINDC (NAMA,IMNV,1,INV,LNAM2)
         IF (IV.EQ.0.OR.IV2.EQ.0) &
            CALL FATALERR ('OPSYS','name not found')
         IF (INP(IV).EQ.0.OR.INP(IV).EQ.0) &
            CALL FATALERR ('OPSYS','no data for name')

         I1 = LEN_TRIM (VARNAM)
         FILENM = 'SC'//VARNAM(1:I1)//'.TTP'
         CALL TTHEAD (IUNIT,FILENM,'Scatter plot', &
                      VARMIN,VARMAX,XLAB,XLAB,VARNAM, &
                      VAR2MN,VAR2MX,VAR2LB,VAR2LB,VAR2NM, &
                      0.,0.)
         WRITE (IUNIT,'(A,/,A,/)') '*','1 0 1'
         I2 = MIN (INP(IV),INP(IV2))

         DO I1=1,I2
            WRITE (IUNIT,*) VALA(I1,IV),VALA(I1,IV2)
         END DO

         CLOSE (IUNIT)

      END IF

      ITOLD = ITASK

      RETURN
      END
