!=======================================================================
! FInterface.f90, Felipe de Vargas, Willingthon Pavan, Fabio Oliveira
! Fortran getters and setters subroutines to calling functions in C++.
!-----------------------------------------------------------------------
! REVISION HISTORY
! 07/10/2016 FV Written.
! 09/01/2016 FV Create get's for read weather data.
! 09/25/2016 FV Create set's for write data in memory.
! 06/18/2017 FV Create new version of getters for weather data.
! 12/04/2017 FO Added a parameter in the functions for the new FlexibleIO data structure.
! 12/11/2017 FO Added variable to return error code in readweather subroutine.
! 02/27/2018 FO Restructured all get/set functions to link with the new CInterface.
! 08/10/2018 FO Added Read Weather interface
!========================================================================
module class_ioset
    type csm_io_type
    contains
        procedure :: getReal
        procedure :: getInteger
        procedure :: getChar
        procedure :: getRealIndex
        procedure :: getIntegerIndex
        procedure :: getCharIndex
        procedure :: getRealArray
        procedure :: getIntegerArray
        procedure :: getCharArray
        procedure :: getRealYrdoy
        procedure :: getIntegerYrdoy
        procedure :: getCharYrdoy
              
        generic :: get => getReal, getInteger, getChar, &
        getRealIndex, getIntegerIndex, getCharIndex, &
        getRealArray, getIntegerArray, getCharArray, &
        getRealYrdoy, getIntegerYrdoy, getCharYrdoy
      
        
        procedure :: setRealMemory
        procedure :: setIntegerMemory
        procedure :: setCharMemory
        procedure :: setRealIndexMemory
        procedure :: setIntegerIndexMemory
        procedure :: setCharIndexMemory
        procedure :: setRealYrdoyMemory
        procedure :: setIntegerYrdoyMemory
        procedure :: setCharYrdoyMemory

        generic :: set => setRealMemory, setIntegerMemory, setCharMemory, &
        setRealIndexMemory, setIntegerIndexMemory, setCharIndexMemory, &
        setRealYrdoyMemory, setIntegerYrdoyMemory, setCharYrdoyMemory


        procedure :: readfile
        
    end type csm_io_type

contains

    subroutine getReal(ioset, group, varname, value)
      
        implicit none
        class(csm_io_type) :: ioset
        character(len=*), intent(in) :: group
        character(len=*), intent(in) :: varname
        character(LEN(group)+1) :: groupstr
        character(LEN(varname)+1) :: varnamestr
        real, intent(out) :: value
      
        interface
            subroutine get_real(groupstr, varnamestr, value)bind(C, name = 'getReal')
                use, intrinsic :: iso_c_binding
                character(kind = c_char), dimension(*) :: groupstr
                character(kind = c_char), dimension(*) :: varnamestr
                real :: value
            end subroutine get_real
        end interface

        groupstr = group
        groupstr(LEN(groupstr):LEN(groupstr)) = CHAR(0)        
        varnamestr = varname
        varnamestr(LEN(varnamestr):LEN(varnamestr)) = CHAR(0)
        
        call get_real(groupstr, varnamestr, value)
        
    end subroutine getReal

    subroutine getInteger(ioset, group, varname, value)

        implicit none
        class(csm_io_type) :: ioset
        character(len=*), intent(in) :: group
        character(len=*), intent(in) :: varname
        character(LEN(group)+1) :: groupstr
        character(LEN(varname)+1) :: varnamestr
        integer, intent(out) :: value

        interface
            subroutine get_int(groupstr, varnamestr, value)bind(C, name = 'getInteger')
                use, intrinsic :: iso_c_binding
                character(kind = c_char), dimension(*) :: groupstr
                character(kind = c_char), dimension(*) :: varnamestr
                integer :: value
            end subroutine get_int
        end interface

        groupstr = group
        groupstr(LEN(groupstr):LEN(groupstr)) = CHAR(0)        
        varnamestr = varname
        varnamestr(LEN(varnamestr):LEN(varnamestr)) = CHAR(0)
        
        call get_int(groupstr, varnamestr, value)

    end subroutine getInteger

    subroutine getChar(ioset, group, varname, value)

        implicit none
        class(csm_io_type) :: ioset
        character(len=*), intent(in) :: group
        character(len=*), intent(in) :: varname
        character(LEN(group)+1) :: groupstr
        character(LEN(varname)+1) :: varnamestr
        character(len = *), intent(out) :: value


        interface
            subroutine get_char(groupstr, varnamestr, valuestr)bind(C, name = 'getChar')
                use, intrinsic :: iso_c_binding
                character(kind = c_char), dimension(*) :: groupstr
                character(kind = c_char), dimension(*) :: varnamestr
                character(kind = c_char), dimension(*) :: valuestr
            end subroutine get_char
        end interface

        groupstr = group
        groupstr(LEN(groupstr):LEN(groupstr)) = CHAR(0)        
        varnamestr = varname
        varnamestr(LEN(varnamestr):LEN(varnamestr)) = CHAR(0)
        value = CHAR(0)

        call get_char(groupstr, varnamestr, value)
        

    end subroutine getChar

    subroutine getRealIndex(ioset, group, varname, value, index)
      
        implicit none
        class(csm_io_type) :: ioset
        character(len=*), intent(in) :: group
        character(len=*), intent(in) :: varname
        character(LEN(group)+1) :: groupstr
        character(LEN(varname)+1) :: varnamestr
        real, intent(out) :: value
        integer, intent(in) :: index
    
        interface
            subroutine get_real(groupstr, varnamestr, value, index)bind(C, name = 'getRealIndex')
                use, intrinsic :: iso_c_binding
                character(kind = c_char), dimension(*) :: groupstr
                character(kind = c_char), dimension(*) :: varnamestr
                real :: value
                integer :: index
            end subroutine get_real
        end interface
        
        groupstr = group
        groupstr(LEN(groupstr):LEN(groupstr)) = CHAR(0)        
        varnamestr = varname
        varnamestr(LEN(varnamestr):LEN(varnamestr)) = CHAR(0)
    
        call get_real(groupstr, varnamestr, value, index)
    
    end subroutine getRealIndex
    
    subroutine getIntegerIndex(ioset, group, varname, value, index)
    
        implicit none
        class(csm_io_type) :: ioset
        character(len=*), intent(in) :: group
        character(len=*), intent(in) :: varname
        character(LEN(group)+1) :: groupstr
        character(LEN(varname)+1) :: varnamestr
        integer, intent(out) :: value
        integer, intent(in) :: index
    
        interface
            subroutine get_int_ind(groupstr, varnamestr, value, index)bind(C, name = 'getIntegerIndex')
                use, intrinsic :: iso_c_binding
                character(kind = c_char), dimension(*) :: groupstr
                character(kind = c_char), dimension(*) :: varnamestr
                integer :: value
                integer :: index
            end subroutine get_int_ind
        end interface
        
        groupstr = group
        groupstr(LEN(groupstr):LEN(groupstr)) = CHAR(0)        
        varnamestr = varname
        varnamestr(LEN(varnamestr):LEN(varnamestr)) = CHAR(0)
    
        call get_int_ind(groupstr, varnamestr, value, index)
    
    end subroutine getIntegerIndex
    
    subroutine getCharIndex(ioset, group, varname, value, index)
    
        implicit none
        class(csm_io_type) :: ioset
        character(len=*), intent(in) :: group
        character(len=*), intent(in) :: varname
        character(LEN(group)+1) :: groupstr
        character(LEN(varname)+1) :: varnamestr
        character(len=*), intent(out) :: value
        integer, intent(in) :: index
    
        interface
            subroutine get_char_ind(groupstr, varnamestr, valuestr, index)bind(C, name = 'getCharIndex')
                use, intrinsic :: iso_c_binding
                character(kind = c_char), dimension(*) :: groupstr
                character(kind = c_char), dimension(*) :: varnamestr
                character(kind = c_char), dimension(*) :: valuestr
                integer :: index
            end subroutine get_char_ind
        end interface
        
        groupstr = group
        groupstr(LEN(groupstr):LEN(groupstr)) = CHAR(0)
        varnamestr = varname
        varnamestr(LEN(varnamestr):LEN(varnamestr)) = CHAR(0)
        value = CHAR(0)
        
        call get_char_ind(groupstr, varnamestr, value, index)
    
    end subroutine getCharIndex
    
    subroutine getRealArray(ioset, group, varname, value, size)

        implicit none
        class(csm_io_type) :: ioset
        character(len=*), intent(in) :: group
        character(len=*), intent(in) :: varname
        character(LEN(group)+1) :: groupstr
        character(LEN(varname)+1) :: varnamestr
        real, dimension(*), intent(out) :: value
        character(len=*), intent(in) :: size
        character(LEN(size)+1) :: sizestr

        interface
            subroutine get_real_array(groupstr, varnamestr, value, sizestr)bind(C, name = 'getRealArray')
                use, intrinsic :: iso_c_binding
                character(kind = c_char), dimension(*) :: groupstr
                character(kind = c_char), dimension(*) :: varnamestr
                real, dimension(*) :: value
                character(kind = c_char), dimension(*) :: sizestr
            end subroutine get_real_array
        end interface
        
        groupstr = group
        groupstr(LEN(groupstr):LEN(groupstr)) = CHAR(0)        
        varnamestr = varname
        varnamestr(LEN(varnamestr):LEN(varnamestr)) = CHAR(0)
        sizestr = size
        sizestr(LEN(sizestr):LEN(sizestr)) = CHAR(0)

        call get_real_array(groupstr, varnamestr, value, sizestr)

    end subroutine getRealArray

    subroutine getIntegerArray(ioset, group, varname, value, size)

        implicit none
        class(csm_io_type) :: ioset
        character(len=*), intent(in) :: group
        character(len=*), intent(in) :: varname
        character(LEN(group)+1) :: groupstr
        character(LEN(varname)+1) :: varnamestr
        integer, dimension(:), intent(out) :: value
        character(len=*), intent(in) :: size
        character(LEN(size)+1) :: sizestr

        interface
            subroutine get_int_array(groupstr, varnamestr, value, sizestr)bind(C, name = 'getIntegerArray')
                use, intrinsic :: iso_c_binding
                character(kind = c_char), dimension(*) :: groupstr
                character(kind = c_char), dimension(*) :: varnamestr
                integer, dimension(*) :: value
                character(kind = c_char), dimension(*) :: sizestr
            end subroutine get_int_array
        end interface
        
        groupstr = group
        groupstr(LEN(groupstr):LEN(groupstr)) = CHAR(0)        
        varnamestr = varname
        varnamestr(LEN(varnamestr):LEN(varnamestr)) = CHAR(0)
        sizestr = size
        sizestr(LEN(sizestr):LEN(sizestr)) = CHAR(0)

        call get_int_array(groupstr, varnamestr, value, sizestr)

    end subroutine getIntegerArray

    subroutine getCharArray(ioset, group, varname, value, size)

        implicit none
        class(csm_io_type) :: ioset
        character(len=*), intent(in) :: group
        character(len=*), intent(in) :: varname
        character(LEN(group)+1) :: groupstr
        character(LEN(varname)+1) :: varnamestr
        character(len=*), dimension(:), intent(out) :: value
        character(len=*), intent(in) :: size
        character(LEN(size)+1) :: sizestr

        interface
            subroutine get_char_array(groupstr, varnamestr, value, sizestr)bind(C, name = 'getCharArray')
                use, intrinsic :: iso_c_binding
                character(kind = c_char), dimension(*) :: groupstr
                character(kind = c_char), dimension(*) :: varnamestr
                character(kind = c_char), dimension(*):: value
                character(kind = c_char), dimension(*) :: sizestr
            end subroutine get_char_array
        end interface
        
        groupstr = group
        groupstr(LEN(groupstr):LEN(groupstr)) = CHAR(0)        
        varnamestr = varname
        varnamestr(LEN(varnamestr):LEN(varnamestr)) = CHAR(0)
        sizestr = size
        sizestr(LEN(sizestr):LEN(sizestr)) = CHAR(0)

        call get_char_array(groupstr, varnamestr, value, sizestr)
        
    end subroutine getCharArray

    subroutine getRealYrdoy(ioset, group, yrdoy, varname, value)
      
        implicit none
        class(csm_io_type) :: ioset
        character(len=*), intent(in) :: group
        character(len=*), intent(in) :: varname
        character(LEN(group)+1) :: groupstr
        character(LEN(varname)+1) :: varnamestr
        integer, intent(in) :: yrdoy
        real, intent(out) :: value
        
        interface
          subroutine get_real_mem_yrdoy(groupstr, yrdoy, varnamestr, value)bind(C, name = 'getRealYrdoy')
            use, intrinsic :: iso_c_binding
            character(kind = c_char), dimension(*) :: groupstr
            integer :: yrdoy
            character(kind = c_char), dimension(*) :: varnamestr
            real :: value
          end subroutine get_real_mem_yrdoy
        end interface
        
        groupstr = group
        groupstr(LEN(groupstr):LEN(groupstr)) = CHAR(0)        
        varnamestr = varname
        varnamestr(LEN(varnamestr):LEN(varnamestr)) = CHAR(0)
        
        call get_real_mem_yrdoy(groupstr, yrdoy, varnamestr, value)
        
    end subroutine getRealYrdoy
    
    subroutine getIntegerYrdoy(ioset, group, yrdoy, varname, value)
    
        implicit none
        class(csm_io_type) :: ioset
        character(len=*), intent(in) :: group
        character(len=*), intent(in) :: varname
        character(LEN(group)+1) :: groupstr
        character(LEN(varname)+1) :: varnamestr
        integer, intent(in) :: yrdoy
        integer, intent(out) :: value
    
        interface
            subroutine get_int_mem_yrdoy(groupstr, yrdoy, varnamestr, value)bind(C, name = 'getIntegerYrdoy')
                use, intrinsic :: iso_c_binding
                character(kind = c_char), dimension(*) :: groupstr
                integer :: yrdoy
                character(kind = c_char), dimension(*) :: varnamestr
                integer :: value
            end subroutine get_int_mem_yrdoy
        end interface
        
        groupstr = group
        groupstr(LEN(groupstr):LEN(groupstr)) = CHAR(0)        
        varnamestr = varname
        varnamestr(LEN(varnamestr):LEN(varnamestr)) = CHAR(0)
    
        call get_int_mem_yrdoy(groupstr, yrdoy, varnamestr, value)
        
    end subroutine getIntegerYrdoy
    
    
    subroutine getCharYrdoy(ioset, group, yrdoy, varname, value)
    
        implicit none
        class(csm_io_type) :: ioset
        character(len=*), intent(in) :: group
        character(len=*), intent(in) :: varname
        character(LEN(group)+1) :: groupstr
        character(LEN(varname)+1) :: varnamestr
        integer, intent(in) :: yrdoy
        character(len=*), intent(out) :: value
        
        interface
            subroutine get_char_mem_yrdoy(groupstr, yrdoy, varnamestr, valuestr)bind(C, name = 'getCharYrdoy')
                use, intrinsic :: iso_c_binding
                character(kind = c_char), dimension(*) :: groupstr
                character(kind = c_char), dimension(*) :: varnamestr
                integer :: yrdoy
                character(kind = c_char), dimension(*) :: valuestr
            end subroutine get_char_mem_yrdoy
        end interface
        
        groupstr = group
        groupstr(LEN(groupstr):LEN(groupstr)) = CHAR(0)        
        varnamestr = varname
        varnamestr(LEN(varnamestr):LEN(varnamestr)) = CHAR(0)
        value = CHAR(0)
    
        call get_char_mem_yrdoy(groupstr, yrdoy, varnamestr, value)
        
    end subroutine getCharYrdoy



    subroutine setRealMemory(ioset, group, varname, value)

        implicit none
        class(csm_io_type) :: ioset
        character(len=*), intent(in) :: group
        character(len=*), intent(in) :: varname
        character(LEN(group)+1) :: groupstr
        character(LEN(varname)+1) :: varnamestr
        real, intent(in) :: value


        interface
            subroutine set_real_mem(groupstr, varnamestr, value)bind(C, name = 'setRealMemory')
                use, intrinsic :: iso_c_binding
                character(kind = c_char), dimension(*) :: groupstr
                character(kind = c_char), dimension(*) :: varnamestr
                real :: value
            end subroutine set_real_mem
        end interface

        groupstr = group
        groupstr(LEN(groupstr):LEN(groupstr)) = CHAR(0)        
        varnamestr = varname
        varnamestr(LEN(varnamestr):LEN(varnamestr)) = CHAR(0)

        call set_real_mem(groupstr, varnamestr, value)
        
    end subroutine setRealMemory

    subroutine setIntegerMemory(ioset, group, varname, value)
      
        implicit none
        class(csm_io_type) :: ioset
        character(len=*), intent(in) :: group
        character(len=*), intent(in) :: varname
        character(LEN(group)+1) :: groupstr
        character(LEN(varname)+1) :: varnamestr
        integer, intent(in) :: value
        
        interface
          subroutine set_int_mem(groupstr, varnamestr, value)bind(C, name = 'setIntegerMemory')
            use, intrinsic :: iso_c_binding
            character(kind = c_char), dimension(*) :: groupstr
            character(kind = c_char), dimension(*) :: varnamestr
            integer :: value
          end subroutine set_int_mem
        end interface
        
        groupstr = group
        groupstr(LEN(groupstr):LEN(groupstr)) = CHAR(0)        
        varnamestr = varname
        varnamestr(LEN(varnamestr):LEN(varnamestr)) = CHAR(0)
        
        call set_int_mem(groupstr, varnamestr, value)
      
    end subroutine setIntegerMemory
    
    subroutine setCharMemory(ioset, group, varname, value)

        implicit none
        class(csm_io_type) :: ioset
        character(len=*), intent(in) :: group
        character(len=*), intent(in) :: varname
        character(LEN(group)+1) :: groupstr
        character(LEN(varname)+1) :: varnamestr
        character(len=*), intent(in) :: value
        character(LEN(value)+1) :: valuestr
        
        interface
            subroutine set_char_mem(groupstr, varnamestr, valuestr)bind(C, name = 'setCharMemory')
                use, intrinsic :: iso_c_binding
                character(kind = c_char), dimension(*) :: groupstr
                character(kind = c_char), dimension(*) :: varnamestr
                character(kind = c_char), dimension(*) :: valuestr
            end subroutine set_char_mem
        end interface
        
        groupstr = group
        groupstr(LEN(groupstr):LEN(groupstr)) = CHAR(0)        
        varnamestr = varname
        varnamestr(LEN(varnamestr):LEN(varnamestr)) = CHAR(0)
        valuestr = value
        valuestr(LEN(valuestr):LEN(valuestr)) = CHAR(0)
        
        call set_char_mem(groupstr, varnamestr, valuestr)
        
    end subroutine setCharMemory

    subroutine setRealIndexMemory(ioset, group, varname, value, index)
      
        implicit none
        class(csm_io_type) :: ioset
        character(len=*), intent(in) :: group
        character(len=*), intent(in) :: varname
        character(LEN(group)+1) :: groupstr
        character(LEN(varname)+1) :: varnamestr
        real, intent(in) :: value
        integer, intent(in) :: index

        interface
            subroutine set_real_mem_ind(groupstr, varnamestr, value, index)bind(C, name = 'setRealIndexMemory')
                use, intrinsic :: iso_c_binding
                character(kind = c_char), dimension(*) :: groupstr
                character(kind = c_char), dimension(*) :: varnamestr
                real :: value
                integer :: index
            end subroutine set_real_mem_ind
        end interface

        groupstr = group
        groupstr(LEN(groupstr):LEN(groupstr)) = CHAR(0)        
        varnamestr = varname
        varnamestr(LEN(varnamestr):LEN(varnamestr)) = CHAR(0)
        
        call set_real_mem_ind(groupstr, varnamestr, value, index)
        
    end subroutine setRealIndexMemory

    subroutine setIntegerIndexMemory(ioset, group, varname, value, index)
      
        implicit none
        class(csm_io_type) :: ioset
        character(len=*), intent(in) :: group
        character(len=*), intent(in) :: varname
        character(LEN(group)+1) :: groupstr
        character(LEN(varname)+1) :: varnamestr
        integer, intent(in) :: value
        integer, intent(in) :: index
        
        interface
          subroutine set_int_mem_ind(groupstr, varnamestr, value, index)bind(C, name = 'setIntegerIndexMemory')
            use, intrinsic :: iso_c_binding
            character(kind = c_char), dimension(*) :: groupstr
            character(kind = c_char), dimension(*) :: varnamestr
            integer :: value
            integer index
          end subroutine set_int_mem_ind
        end interface
        
        groupstr = group
        groupstr(LEN(groupstr):LEN(groupstr)) = CHAR(0)        
        varnamestr = varname
        varnamestr(LEN(varnamestr):LEN(varnamestr)) = CHAR(0)
              
        call set_int_mem_ind(groupstr, varnamestr, value, index)
      
    end subroutine setIntegerIndexMemory
    
    subroutine setCharIndexMemory(ioset, group, varname, value, index)

        implicit none
        class(csm_io_type) :: ioset
        character(len=*), intent(in) :: group
        character(len=*), intent(in) :: varname
        character(LEN(group)+1) :: groupstr
        character(LEN(varname)+1) :: varnamestr
        character(len=*), intent(in) :: value
        character(LEN(value)+1) :: valuestr
        integer, intent(in) :: index
        
        interface
            subroutine set_char_mem_ind(groupstr, varnamestr, valuestr, index)bind(C, name = 'setCharIndexMemory')
                use, intrinsic :: iso_c_binding
                character(kind = c_char), dimension(*) :: groupstr
                character(kind = c_char), dimension(*) :: varnamestr
                character(kind = c_char), dimension(*) :: valuestr
                integer :: index
            end subroutine set_char_mem_ind
        end interface

        groupstr = group
        groupstr(LEN(groupstr):LEN(groupstr)) = CHAR(0)        
        varnamestr = varname
        varnamestr(LEN(varnamestr):LEN(varnamestr)) = CHAR(0)
        valuestr = value
        valuestr(LEN(valuestr):LEN(valuestr)) = CHAR(0)
        
        call set_char_mem_ind(groupstr, varnamestr, valuestr, index)
        
    end subroutine setCharIndexMemory

    subroutine setRealYrdoyMemory(ioset, group, yrdoy, varname, value)
      
        implicit none
        class(csm_io_type) :: ioset
        character(len=*), intent(in) :: group
        character(len=*), intent(in) :: varname
        character(LEN(group)+1) :: groupstr
        character(LEN(varname)+1) :: varnamestr
        integer, intent(in) :: yrdoy
        real, intent(in) :: value
        
        interface
          subroutine set_real_mem_yrdoy(groupstr, yrdoy, varnamestr, value)bind(C, name = 'setRealYrdoyMemory')
            use, intrinsic :: iso_c_binding
            character(kind = c_char), dimension(*) :: groupstr
            integer :: yrdoy
            character(kind = c_char), dimension(*) :: varnamestr
            real :: value
          end subroutine set_real_mem_yrdoy
        end interface
        
        groupstr = group
        groupstr(LEN(groupstr):LEN(groupstr)) = CHAR(0)        
        varnamestr = varname
        varnamestr(LEN(varnamestr):LEN(varnamestr)) = CHAR(0)
        
        call set_real_mem_yrdoy(groupstr, yrdoy, varname, value)
        
    end subroutine setRealYrdoyMemory
    
    subroutine setIntegerYrdoyMemory(ioset, group, yrdoy, varname, value)

        implicit none
        class(csm_io_type) :: ioset
        character(len=*), intent(in) :: group
        character(len=*), intent(in) :: varname
        character(LEN(group)+1) :: groupstr
        character(LEN(varname)+1) :: varnamestr
        integer, intent(in) :: yrdoy
        integer, intent(in) :: value

        interface
            subroutine set_int_mem_yrdoy(groupstr, yrdoy, varnamestr, value)bind(C, name = 'setIntegerYrdoyMemory')
                use, intrinsic :: iso_c_binding
                character(kind = c_char), dimension(*) :: groupstr
                integer :: yrdoy
                character(kind = c_char), dimension(*) :: varnamestr
                integer :: value
            end subroutine set_int_mem_yrdoy
        end interface
        
        groupstr = group
        groupstr(LEN(groupstr):LEN(groupstr)) = CHAR(0)        
        varnamestr = varname
        varnamestr(LEN(varnamestr):LEN(varnamestr)) = CHAR(0)

        call set_int_mem_yrdoy(groupstr, yrdoy, varnamestr, value)
        
    end subroutine setIntegerYrdoyMemory


    subroutine setCharYrdoyMemory(ioset, group, yrdoy, varname, value)

        implicit none
        class(csm_io_type) :: ioset
        character(len=*), intent(in) :: group
        character(len=*), intent(in) :: varname
        character(LEN(group)+1) :: groupstr
        character(LEN(varname)+1) :: varnamestr
        integer, intent(in) :: yrdoy
        character(len=*), intent(in) :: value
        character(LEN(value)+1) :: valuestr
        
        interface
            subroutine set_char_mem_yrdoy(groupstr, yrdoy, varnamestr, valuestr)bind(C, name = 'setCharYrdoyMemory')
                use, intrinsic :: iso_c_binding
                character(kind = c_char), dimension(*) :: groupstr
                character(kind = c_char), dimension(*) :: varnamestr
                integer :: yrdoy
                character(kind = c_char), dimension(*) :: valuestr
            end subroutine set_char_mem_yrdoy
        end interface
        
        groupstr = group
        groupstr(LEN(groupstr):LEN(groupstr)) = CHAR(0)        
        varnamestr = varname
        varnamestr(LEN(varnamestr):LEN(varnamestr)) = CHAR(0)
        valuestr = value
        valuestr(LEN(valuestr):LEN(valuestr)) = CHAR(0)

        call set_char_mem_yrdoy(groupstr, yrdoy, varnamestr, valuestr)
        
    end subroutine setCharYrdoyMemory


    subroutine readfile(ioset, group)
        use, intrinsic :: iso_c_binding
        class(csm_io_type) :: ioset
        character(len=*), intent(in) :: group
        character(LEN(group)+1) :: groupstr
    
        interface
            subroutine readinputfile(groupstr)bind(C, name = 'readinputfile')
                use, intrinsic :: iso_c_binding
                character(kind = c_char), dimension(*) :: groupstr
            end subroutine readinputfile
        end interface

        groupstr = group
        groupstr(LEN(groupstr):LEN(groupstr)) = CHAR(0)        

        call readinputfile(groupstr)
        
    end subroutine readfile
    
end module class_ioset  


module flexibleio

    use class_ioset

    implicit none

    type(csm_io_type) :: fio

end module flexibleio

subroutine READWEATHER(fileww, yrdoy, firstweatherday, lastweatherday, eof, lnum, nrecords, erryrdoy, errcode)
  
    use, intrinsic :: iso_c_binding
    character(len=*), intent(in) :: fileww
    character(LEN(fileww)+1) :: filewwstr
    integer, intent(in) :: yrdoy
    integer, intent(out) :: firstweatherday
    integer, intent(out) :: lastweatherday
    integer, intent(out) :: eof
    integer, intent(out) :: lnum
    integer, intent(out) :: nrecords
    character(len=*), intent(out) :: erryrdoy
    integer, intent(out) :: errcode

    interface
        subroutine readwthfile(filewwstr, yrdoy, firstweatherday, lastweatherday, eof, lnum, nrecords, erryrdoy, errcode)&
            bind(C, name = 'INPUTWEATHER')
            import :: c_char
            character(kind = c_char), dimension(*) :: filewwstr
            integer :: yrdoy
            integer :: firstweatherday
            integer :: lastweatherday
            integer :: eof
            integer :: lnum
            integer :: nrecords
            character(kind = c_char), dimension(*) :: erryrdoy
            integer :: errcode                
        end subroutine readwthfile
    end interface

    filewwstr = fileww
    filewwstr(LEN(filewwstr):LEN(filewwstr)) = CHAR(0)        
    erryrdoy = CHAR(0)        

    call readwthfile(filewwstr, yrdoy, firstweatherday, lastweatherday, eof, lnum, nrecords, erryrdoy, errcode)
    
end subroutine READWEATHER
