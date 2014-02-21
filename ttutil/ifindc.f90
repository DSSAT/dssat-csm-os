FUNCTION IFINDC (NAMLIS,ILDEC,IST,IEND,NAME)
! finds NAME in character array NAMLIS, searching from IST to IEND

  IMPLICIT NONE
  INTEGER :: IFINDC

! formal
  INTEGER                            :: ILDEC, IST, IEND
  CHARACTER(LEN=*), DIMENSION(ILDEC) :: NAMLIS
  CHARACTER(LEN=*)                   :: NAME

! local
  INTEGER :: k, c, IM, NameLEN, STEP

! effective string sizes
  if (IST<1.or.IST>ILDEC.or.IEND<0.or.IEND>ILDEC) CALL FATALERR ('IFINDC','search outside array bounds')

  NameLEN = len_trim (NAME)

  if (IEND==0) then
!    special case used in some calling programs
     IM = 0

  else
!    direction of search
     STEP = 1
     if (IEND < IST) STEP = -1

     IM = 0
     search: do k=IST,IEND,STEP
        if (NameLEN /= len_trim(NAMLIS(k))) CYCLE search ! different length
        charComp: do c=1,NameLEN
           if (NAMLIS(k)(c:c) /= NAME(c:c)) CYCLE search ! different character
        end do charComp
!       all characters equal
        IM = k
        EXIT search
     end do search
  end if

  IFINDC = IM
Return
END FUNCTION IFINDC
