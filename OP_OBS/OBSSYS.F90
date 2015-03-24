      SUBROUTINE OBSSYS (ITASK,FILEIN,VARNAM,FRCREQ, &
                         THERE,FRC,OBS_DAY,VARVAL)
      IMPLICIT NONE

! remarks:
!            - extrapolatie buiten _obs range vindt niet plaats,
!            - _trg array wordt met een dummy dag verlengd zodat de
!              laatste _trg value nog een dag aktief is
!            - _trg array is alleen werkzaam binnen de gedefinieerde
!              datum range. Als de tijd daarbuiten komt wordt de _frc
!              waarde van nul aangenomen (geen forcering dus).
!            - belangrijk: _TRG = 1 en _TRG = 2 zijn w.b. betekenis
!              omgedraaid
!            - als forcing aanstaan en er zijn op die tijd geen
!              observaties wordt there=.false. teruggegeven
!              (dit heeft in intgr2 tot gevolg dat er gesimuleerd wordt)
!            - _FRC = 0 do not force
!              _FRC = 1 use triggering table
!              _FRC = 2 force during whole simulation
!              _TRG = 0 do not force
!              _TRG = 1 do point reset
!              _TRG = 2 force when applicable

!     Author : Daniel van Kraalingen, J.J.M. Riethoven
!              AB-DLO, PO Box 14, 6700 AA  Wageningen
!     Date   : May 1996
!     Version: 1.42

!     ITASK   - Task to be executed,                                  I
!               = 1, reset everything
!               = 2, get observed value, if available, and determine
!                    forcing status
!     FILEIN  - Filename to which variable name corresponds           I
!               (ITASK = 2 only)
!     VARNAM  - Variable name                                         I
!     FRCREQ  - Request forcing status                                I
!     THERE   - Flags presence of VARNAM_obs in FILEIN                O
!     FRC     - Flag indicates forcing of VARNAM                      O
!     OBS_DAY - Flag indicates simulation equals date of observ.      O
!     VARVAL  - Variable value                                        O

!     Formal parameters
      INTEGER       ITASK
      CHARACTER (*) FILEIN, VARNAM
      REAL          VARVAL
      LOGICAL       FRCREQ,THERE,FRC,OBS_DAY

!     Common area with MODELS routine of FSE
      REAL YEAR, DOY
      LOGICAL TERMNL
      INTEGER IUNITD, IUNITL
      COMMON /FSECM1/ YEAR, DOY, IUNITD, IUNITL, TERMNL

!     Declarations for RD routine data
      INTEGER        ILNDAT,INFDAT
      PARAMETER     (ILNDAT=400)
      CHARACTER (31) DATLIS(ILNDAT)

!     Storage of input files
!     ======================
!     MNINF  - Maximum number of input files
!     NINF   - Actual number of input files
!     AINF   - Character array with names of input files
!     INFBPT - Points to first variable name from input file
!              in array of variable names
!     INFEPT - Points to last variable name from input file
!              in array of variable names

      INTEGER        MNINF, NINF
      PARAMETER     (MNINF=10)
      CHARACTER (80) AINF(MNINF),INF
      INTEGER        INFBPT(MNINF),INFEPT(MNINF)

!     Storage of variable names
!     =========================
!     (length 27 because suffix is removed)
!     MNVAR  - Maximum number of relevant variable names
!     NVAR   - Actual number of variable names
!     AVAR   - Array with variable names
!     OBSBPT - Points to first value from variable_obs in
!              array of values
!     OBSEPT - Points to last value from variable_obs in
!              array of values
!     TRGBPT - Points to first value from variable_trg in
!              array of values
!     TRGEPT - Points to last value from variable_trg in
!              array of values
!     FRCVAL - Array with _frc values
!     TERVAL - Array with terminal observation values (_ter)
      INTEGER        MNVAR,NVAR
      PARAMETER     (MNVAR=100)
      CHARACTER (27) AVAR(MNVAR), TMP27
      INTEGER        OBSBPT(MNVAR),OBSEPT(MNVAR)
      INTEGER        TRGBPT(MNVAR),TRGEPT(MNVAR)
      INTEGER        FRCVAL(MNVAR)
      REAL           TERVAL(MNVAR)

!     Storage of values
!     =================
      INTEGER    MNVAL, NVAL
      PARAMETER (MNVAL=1000)
      REAL       AVAL(MNVAL)

!     Local variables
      INTEGER        IN, I1, I2, I3, I4, IL, ILFIL, IS, IFND,IFINDC
      INTEGER        PVAR
      REAL           LINT2, NDAYS, DS1900
      CHARACTER (31) TMP31
      LOGICAL        FND, FND2, MISS, INIT

      SAVE

      DATA INIT /.FALSE./

      IF (ITASK.EQ.1) THEN
!        reset everything
         NINF = 0
         NVAR = 0
         NVAL = 0

!        RESET FOR TEST PURPOSES ONLY
         DO I1=1,MNINF
            AINF(I1) = 'XXXXXXXXXX'
            INFBPT(I1) = -99
            INFEPT(I1) = -99
         END DO
         DO I1=1,MNVAR
            AVAR(I1) = 'XXXXXXX'
            OBSBPT(I1) = -99
            OBSEPT(I1) = -99
            TRGBPT(I1) = -99
            TRGEPT(I1) = -99
            FRCVAL(I1) = -99
            TERVAL(I1) = -99.
         END DO
         DO I1=1,MNVAL
            AVAL(I1) = -99.
         END DO

         INIT = .TRUE.

      ELSE IF (ITASK.EQ.2) THEN

         IF (.NOT.INIT) CALL FATALERR &
            ('OBSSYS','system never initialized')

         ILFIL = LEN_TRIM (FILEIN)

!        see if file is already processed
         INF = FILEIN
         IN  = IFINDC (AINF,MNINF,1,NINF,INF)

         IF (IN.EQ.0) THEN
!           filename not yet processed, store filename in buffer
            NINF = NINF+1
            IF (NINF.GT.MNINF) CALL FATALERR &
               ('OBSSYS','too many input files')
            AINF(NINF)   = INF
            INFBPT(NINF) = NVAR+1
            INFEPT(NINF) = 0

!           read the input file
            CALL RDINIT (IUNITD,IUNITL,INF)

!           start a loop to test the name of every variable from the
!           input file
            CALL RDINLV (.FALSE.,DATLIS,ILNDAT,INFDAT)

            DO I1=1,INFDAT
               TMP31 = DATLIS(I1)
               CALL UPPERC (TMP31)
               IL = LEN_TRIM (TMP31)
!              calculate the position of the character from where it is
!              four characters to the right until the end of the string
               IS = MAX (IL-4+1,1)

               IF (TMP31(IS:IL).EQ.'_OBS'.OR. &
                   TMP31(IS:IL).EQ.'_TER'.OR. &
                   TMP31(IS:IL).EQ.'_TRG'.OR. &
                   TMP31(IS:IL).EQ.'_FRC') THEN

!                 variable name has proper suffix

!                 assign part without suffix to temporary variable
                  TMP27 = ' '
                  TMP27 = TMP31(1:IS-1)

!                 check that this name is not yet read from other input
!                 files, this gives problems with reruns
                  FND = .FALSE.
                  IF (NINF.GT.1) THEN
                     I2  = IFINDC (AVAR,MNVAR,1,INFEPT(NINF-1),TMP27)
                     FND = I2.GT.0
                  END IF

!                 display warning if found
                  IF (FND) THEN
                     I2 = LEN_TRIM (TMP27)
                     WRITE (*,'(4A,/,2A)') &
                     ' WARNING from OBSSYS: Definitions for variable: ', &
                       TMP27(1:I2),' from datafile: ',FILEIN(1:ILFIL), &
                     ' already occurred in a previous datafile,', &
                     ' this will give problems with reruns'
                     WRITE (IUNITL,'(4A,/,2A)') &
                     ' WARNING from OBSSYS: Definitions for variable: ', &
                       TMP27(1:I2),' from datafile: ',FILEIN(1:ILFIL), &
                     ' already occurred in a previous datafile,', &
                     ' this will give problems with reruns'
                  END IF

                  FND   = .FALSE.
                  IF (INFEPT(NINF).GT.0) THEN
!                    a variable with a relevant suffix was already found
!                    in this input file, determine if this name with
!                    a different suffix was already parsed
                     I2  = IFINDC (AVAR,MNVAR,INFBPT(NINF),INFEPT(NINF),TMP27)
                     FND = I2.GT.0
                  END IF

                  IF (.NOT.FND) THEN
!                    name with this suffix was not yet parsed
                     NVAR = NVAR+1
                     IF (NVAR.GT.MNVAR) CALL FATALERR &
                        ('OBSSYS','too many variables')

!                    store name and reset pointers
                     INFEPT(NINF) = NVAR
                     AVAR(NVAR)   = TMP27
                     OBSBPT(NVAR) = 0
                     OBSEPT(NVAR) = 0
                     TRGBPT(NVAR) = 0
                     TRGEPT(NVAR) = 0
                     FRCVAL(NVAR) = -1
                     TERVAL(NVAR) = -99.
                     PVAR = NVAR
                  ELSE
                     PVAR = I2
                  END IF

                  MISS = .FALSE.

                  IF (TMP31(IS:IL).EQ.'_OBS'.OR. &
                      TMP31(IS:IL).EQ.'_TRG') THEN

!                    name ends with _OBS or _TRG suffix, we expect an
!                    array in the data file, read it
                     CALL RDAREA (DATLIS(I1),AVAL(NVAL+1),MNVAL-NVAL,IFND)

                     IF (IFND.EQ.1) THEN
                        IF (AVAL(NVAL+1).EQ.-99.) THEN
!                          missing value on file, don't store value
!                          leave pointers at zero
                           MISS = .TRUE.
                        ELSE
                           CALL FATALERR ('OBSSYS','illegal value')
                        END IF
                     ELSE IF (IFND.EQ.3) THEN
                        CALL FATALERR ('OBSSYS','only one point')
                     ELSE IF (MOD(IFND,3).NE.0) THEN
                        WRITE (*,*) IFND,DATLIS(I1)
                        CALL FATALERR ('OBSSYS','no multiple of three')
                     END IF
                  END IF

                  IF (.NOT.MISS.AND. &
                     (TMP31(IS:IL).EQ.'_OBS'.OR. &
                      TMP31(IS:IL).EQ.'_TRG')) THEN

!                    shift array to the left because the year,day values
!                    are converted to number of days since 1900, (this
!                    is a single value)
                     I3 = NVAL
                     DO I2=NVAL+1,NVAL+IFND,3
                        I3 = I3+1
                        AVAL(I3) = DS1900 (AVAL(I2),AVAL(I2+1))
                        I3 = I3+1
                        AVAL(I3) = AVAL(I2+2)
                     END DO

                     IF (TMP31(IS:IL).EQ.'_OBS') THEN
!                       update _OBS pointers
                        OBSBPT(PVAR) = NVAL+1
                        OBSEPT(PVAR) = I3
                     ELSE
!                       add a zero _trg value at the end of the table
!                       so that the last _trg value in the file is
!                       'active', however, only for one day

                        IF (I3+2.GT.MNVAL) CALL FATALERR ('OBSSYS', &
                            'value array full')
                        I3 = I3+2
                        AVAL(I3-1) = AVAL(I3-3)+1.
                        AVAL(I3)   = 0.

!                       update _TRG pointers
                        TRGBPT(PVAR) = NVAL+1
                        TRGEPT(PVAR) = I3

                        DO I2=TRGBPT(PVAR)+1,TRGEPT(PVAR),2
                           IF (AVAL(I2).LT.0..OR.AVAL(I2).GT.2.) THEN
                              CALL FATALERR ('OBSSYS', &
                              'illegal triggering value')
                           END IF
                        END DO

                     END IF
                     NVAL = I3

                  ELSE IF (TMP31(IS:IL).EQ.'_TER') THEN
!                    variable has been a terminal _OBS value

                     CALL RDSREA (DATLIS(I1),TERVAL(PVAR))

                  ELSE IF (TMP31(IS:IL).EQ.'_FRC') THEN
!                    variable must have been a _FRC variable type,
!                    get and store the single value

                     CALL RDSINT (DATLIS(I1),FRCVAL(PVAR))
                     IF (FRCVAL(PVAR).LT.0.OR.FRCVAL(PVAR).GT.2) THEN
                        CALL FATALERR ('OBSSYS','illegal force value')
                     END IF
                  END IF
               END IF
            END DO
            IN = NINF
            CLOSE (IUNITD)

!           error checks
            DO I1=INFBPT(IN),INFEPT(IN)
!              loop through every variable in the input file
               IF (FRCVAL(I1).GE.1.AND. &
                   OBSBPT(I1).EQ.0.AND. &
                   OBSEPT(I1).EQ.0) THEN
!                 triggering or forcing without observations
                  I2 = LEN_TRIM (AVAR(I1))
                  WRITE (*,'(4A,/,A)') &
                  ' ERROR in OBSSYS: Variable: ', AVAR(I1)(1:I2), &
                  ' from datafile: ',FILEIN(1:ILFIL), &
                  ' should be triggered but it has no observations'
                  CALL FATALERR ('OBSSYS',' ')
               END IF

               IF (FRCVAL(I1).EQ.1.AND. &
                   TRGBPT(I1).EQ.0.AND. &
                   TRGEPT(I1).EQ.0) THEN
!                 triggering without triggering table
                  I2 = LEN_TRIM (AVAR(I1))
                  WRITE (*,'(4A,/,A)') &
                  ' ERROR in OBSSYS: Variable: ', AVAR(I1)(1:I2), &
                  ' from datafile: ',FILEIN(1:ILFIL), &
                  ' should be triggered but it has no triggering table'
                  CALL FATALERR ('OBSSYS',' ')
               END IF

               IF (OBSBPT(I1).GT.0.AND.OBSEPT(I1).GT.0.AND. &
                   OBSEPT(I1)-OBSBPT(I1).GE.3) THEN
!                 valid set of observations was found, test data
                  I2 = LEN_TRIM (AVAR(I1))
                  TMP31 = AVAR(I1)(1:I2)//'_OBS'
                  VARVAL = LINT2 (TMP31,AVAL(OBSBPT(I1)), &
                           OBSEPT(I1)-OBSBPT(I1)+1,AVAL(OBSBPT(I1)))

               END IF
            END DO
         END IF

!        filename is processed when execution arrives here
!        try to get observed values, forcing and triggering information

         THERE   = .FALSE.
         FRC     = .FALSE.
         VARVAL  = -99.
         NDAYS   = DS1900 (YEAR,DOY)
         OBS_DAY = .FALSE.

         IF (INFBPT(IN).GT.0.AND.INFEPT(IN).GT.0) THEN
!           variable names with relevant suffixes were found in this
!           input file, see if the current variable name has been
!           defined in the input file

            TMP27 = VARNAM
            I2    = IFINDC (AVAR,MNVAR,INFBPT(IN),INFEPT(IN),TMP27)

            IF (I2.GT.0) THEN
!              requested variable found in input file

               IF (OBSBPT(I2).GT.0.AND.OBSEPT(I2).GT.0) THEN
!                 observations were supplied with this variable,
!                 get the observed value, only if within the date range
                  IF (AVAL(OBSBPT(I2)).LE.NDAYS.AND. &
                      AVAL(OBSEPT(I2)-1).GE.NDAYS) THEN

!                    current day is within defined dates in _obs table,

!                    see if the current date exactly matches an
!                    observation date

                     DO I1=OBSBPT(I2),OBSEPT(I2)-1,2
                        IF (AVAL(I1).EQ.NDAYS) OBS_DAY = .TRUE.
                     END DO
!                    carry out interpolation

                     VARVAL = LINT2 (VARNAM,AVAL(OBSBPT(I2)), &
                              OBSEPT(I2)-OBSBPT(I2)+1,NDAYS)

                     THERE = .TRUE.
!                     IF (TERMNL) WRITE (*,'(1X,3A)') &
!                       'WARNING from OBSSYS: ', &
!                       'time not yet outside observations of ',VARNAM
                  END IF

               ELSE IF (TERMNL.AND.TERVAL(I2).NE.-99.) THEN

!                 observations not defined, final value requested
                  THERE   = .TRUE.
                  VARVAL  = TERVAL(I2)
                  OBS_DAY = .TRUE.
               END IF

               IF (FRCVAL(I2).EQ.2.AND.FRCREQ.AND.THERE) THEN
!                 forcing was enabled
                  FRC = .TRUE.
               ELSE IF (FRCVAL(I2).EQ.1.AND.FRCREQ.AND.THERE) THEN
!                 triggering was enabled, search for the data
!                 that apply to the triggering date

                  FND = .FALSE.
                  IF (AVAL(TRGBPT(I2)).LE.NDAYS.AND. &
                      AVAL(TRGEPT(I2)-1).GE.NDAYS) THEN
!                    current day is within defined dates in _frc table,
!                    search for appropriate date in _frc table

                     I3  = TRGEPT(I2)-1

                     DO WHILE (I3.GE.TRGBPT(I2).AND..NOT.FND) 
                        IF (AVAL(I3).LE.NDAYS) THEN
                           FND = .TRUE.
                        ELSE
                           I3 = I3-2
                        END IF
                     END DO
                  END IF

                  IF (FND) THEN
!                    a triggering date was found

                     IF (I3.EQ.TRGEPT(I2)-1) THEN
!                       final date of triggering table is taken
                        WRITE (*,'(4A,/,A)') &
                           ' WARNING from OBSSYS: Variable: ', &
                             AVAR(I2)(1:I2), &
                           ' from datafile: ',FILEIN(1:ILFIL), &
                           ' Final date from triggering table is taken'
                        WRITE (IUNITL,'(4A,/,A)') &
                           ' WARNING from OBSSYS: Variable: ', &
                             AVAR(I2)(1:I2), &
                           ' from datafile: ',FILEIN(1:ILFIL), &
                           ' Final date from triggering table is taken'
                     END IF

                     IF (AVAL(I3+1).EQ.0.) THEN
!                       nothing
                        CONTINUE
                     ELSE IF (AVAL(I3+1).EQ.1.) THEN
!                       point reset
                        FRC = .TRUE.
                        AVAL(I3+1) = 0.

                        FND2 = .FALSE.
                        I4   = OBSBPT(I2)
                        DO WHILE (I4.LE.OBSEPT(I2).AND..NOT.FND2)
                           FND2 = AVAL(I3).EQ.AVAL(I4)
                           I4 = I4+1
                        END DO
                        IF (.NOT.FND2) THEN
                           WRITE (*,'(4A,/,A)') &
                           ' WARNING from OBSSYS: Variable: ', &
                             AVAR(I2)(1:I2), &
                           ' from datafile: ',FILEIN(1:ILFIL), &
                           ' Point reset value is interpolated'
                           WRITE (IUNITL,'(4A,/,A)') &
                           ' WARNING from OBSSYS: Variable: ', &
                             AVAR(I2)(1:I2), &
                           ' from datafile: ',FILEIN(1:ILFIL), &
                           ' Point reset value is interpolated'
                        END IF
                     ELSE IF (AVAL(I3+1).EQ.2.) THEN
!                       forcing
                        FRC = .TRUE.
                     ELSE
                        CALL FATALERR ('OBSSYS','illegal value')
                     END IF
                  END IF
               END IF
            END IF
         END IF
      ELSE
         CALL FATALERR ('OBSSYS','internal error, illegal task')
      END IF

      RETURN
      END
