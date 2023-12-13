    subroutine rm_file(io, filename)
    
    !---------------------------------!
    !--- Delete existing file --------!
    !--- Murilo Vianna, Feb-2020 -----!
    !---------------------------------!
    !  REVISION HISTORY
    !  10/03/2020 FO Changed inquire units args
    
    implicit none

    integer         io
    integer         stat
    logical         file_exists

    character*21    filename

    !--- Check that the file exists (LBYL):
    file_exists = .false.
    inquire(unit=io, exist=file_exists)

    !--- Delete it if file exists
    if(file_exists)then
        open(unit=io, iostat=stat, file=trim(filename), status='old')
        if (stat == 0) close(1234, status='delete')
    endif

    return

    end subroutine rm_file
    
    
    subroutine open_file(io, filename, header)
        
    !---------------------------------!
    !--------- Open file -------------!
    !--- Murilo Vianna, Feb-2020 -----!
    !---------------------------------!
    !  REVISION HISTORY
    !  10/03/2020 FO Changed inquire units args
        
    implicit none

    integer         io
!   integer         stat
    integer         errnum
    logical         file_exists
    
    character*21    filename
    character*50    header

    !--- Check that the file exists (LBYL):
    file_exists = .false.
    inquire(unit=io, exist=file_exists)

    !--- Open the file
    IF (FILE_EXISTS) THEN
        !--- In append mode if the file already exists
        OPEN (UNIT=io, FILE=filename, STATUS='OLD',IOSTAT=ERRNUM, POSITION='APPEND')
    ELSE
        !--- A new file if not existing
        OPEN (UNIT=io, FILE=filename, STATUS='NEW',IOSTAT = ERRNUM)
        WRITE(io,trim(header))
    ENDIF    

    return

    end subroutine open_file
