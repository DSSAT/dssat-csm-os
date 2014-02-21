      SUBROUTINE SWPI4 (ITASK,NAME,IUNIT,DECL,IP,VAL)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      INTEGER ITASK,IUNIT,DECL,IP
      CHARACTER*(*) NAME

**    Local variables

*     Status array
*     MNA - Maximum number of names that can be swapped
*     ANAME - Array with names of swap arrays
*     AUNIT - Array with units for file i/o used for swap arrays
*     NREC  - Array with number of records per swap array
*     DECLA - Array with declared sizes of swap array

      INTEGER MNA
      PARAMETER (MNA=10)
      CHARACTER*62 ANAME(MNA),LNAME,EMPTY
      INTEGER AUNIT(MNA),NREC(MNA),DECLA(MNA)

*     Miscellaneous
      INTEGER I1,IN,IFINDC,RECN,TYPLEN
      CHARACTER MODNAM*6
      LOGICAL UNITOP

*     Data type specific
      INTEGER VAL,ZERO
      SAVE

*     Data type specific
      DATA ZERO /0/, TYPLEN /4/, MODNAM /'SWPI4'/

      DATA ANAME /MNA*' '/,EMPTY /' '/

C      write (*,*) itask,name,decl,ip

*     lookup name in name array
      LNAME = NAME
      IF (LNAME.EQ.' ') CALL FATALERR (MODNAM,'empty array name')
      IN    = IFINDC (ANAME,MNA,1,MNA,LNAME)

      IF (ITASK.EQ.1) THEN
*        store value

         IF (IN.EQ.0) THEN
*           array name unknown, set up for new name

            IF (DECL.LT.1) CALL FATALERR (MODNAM,
     &         'illegal declared length')
            IF (IUNIT.LT.10.OR.IUNIT.GT.99) CALL FATALERR
     &         (MODNAM,'illegal unit number')

            INQUIRE (UNIT=IUNIT,OPENED=UNITOP)
            IF (UNITOP) CALL FATALERR (MODNAM,'unit is already in use')

            IN = IFINDC (ANAME,MNA,1,MNA,EMPTY)
            IF (IN.EQ.0) CALL FATALERR
     &         (MODNAM,'internal name array full')

            ANAME(IN) = LNAME

            DECLA(IN) = DECL

            NREC(IN)  = 0
            AUNIT(IN) = IUNIT
            OPEN (AUNIT(IN),STATUS='SCRATCH',
     &            ACCESS='DIRECT',FORM='UNFORMATTED',RECL=TYPLEN)
         END IF

         IF (IP.LE.DECLA(IN)) CALL FATALERR (MODNAM,
     &      'pointer less than declared value')

         RECN = IP-DECLA(IN)

         IF (RECN.GT.NREC(IN)) THEN
            DO 10 I1=NREC(IN)+1,RECN-1
               WRITE (AUNIT(IN),REC=I1) ZERO
10          CONTINUE
            NREC(IN) = RECN
         END IF

         WRITE (AUNIT(IN),REC=RECN) VAL

      ELSE IF (ITASK.EQ.2) THEN

*        read value
         IF (IN.GT.0) THEN
            RECN = IP-DECLA(IN)
            IF (RECN.GE.1.AND.RECN.LE.NREC(IN)) THEN
*              array value has been stored on file
               READ (AUNIT(IN),REC=RECN) VAL
            ELSE IF (RECN.GT.NREC(IN)) THEN
*              read is beyond last record
               CALL WARNING_OR(MODNAM,'returning uninitialized element')
               VAL = ZERO
            ELSE IF (RECN.LT.1) THEN
*              error in calling swap routine
               CALL FATALERR (MODNAM,
     &         'array index below declared length')
            END IF
         ELSE
            CALL FATALERR (MODNAM,'unknown array name')
         END IF

      ELSE IF (ITASK.EQ.3) THEN

*        delete array, close and delete swap file, ignore arrays
*        not known to this routine
         IF (IN.GT.0) THEN
            ANAME(IN) = ' '
            CLOSE (AUNIT(IN))
            AUNIT(IN) = 0
         END IF
      ELSE
         CALL FATALERR (MODNAM,'illegal itask')
      END IF

      RETURN
      END
