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
module flexibleio
    implicit none
    
    type csm_io_type
        private
            integer :: init
    contains
        procedure :: getFloat
        procedure :: getInteger
        procedure :: getString
        procedure :: getIndexFloat
        procedure :: getIndexInteger
        procedure :: getIndexString
        procedure :: getArrayFloat
        procedure :: getArrayInteger
        procedure :: getArrayString
        procedure :: getForKeyFloat
        procedure :: getForKeyInteger
        procedure :: getForKeyString
        procedure :: getFor2KeyFloat
        procedure :: getFor2KeyInteger
        procedure :: getFor2KeyString
              
        generic :: get => getFloat, getInteger, getString, &
        getIndexFloat, getIndexInteger, getIndexString, &
        getArrayFloat, getArrayInteger, getArrayString, &
        getForKeyFloat, getForKeyInteger, getForKeyString, &
        getFor2KeyFloat, getFor2KeyInteger, getFor2KeyString
      
        procedure :: setFloat
        procedure :: setInteger
        procedure :: setString
        procedure :: setIndexFloat
        procedure :: setIndexInteger
        procedure :: setIndexString
        procedure :: setForKeyFloat
        procedure :: setForKeyInteger
        procedure :: setForKeyString
        procedure :: setFor2KeyFloat
        procedure :: setFor2KeyInteger
        procedure :: setFor2KeyString

        generic :: set => setFloat, setInteger, setString, &
        setIndexFloat, setIndexInteger, setIndexString, &
        setForKeyFloat, setForKeyInteger, setForKeyString, &
        setFor2KeyFloat, setFor2KeyInteger, setFor2KeyString
        
    end type csm_io_type

    type(csm_io_type) :: fio
contains

    subroutine getFloat(this, group, varname, value)
      
        implicit none
        class(csm_io_type) :: this
        character(len=*), intent(in) :: group
        character(len=*), intent(in) :: varname
        character(LEN(group)+1) :: groupstr
        character(LEN(varname)+1) :: varnamestr
        real, intent(out) :: value
      
        interface
            subroutine get_Float(groupstr, varnamestr, value)bind(C, name = 'getFloat')
                use, intrinsic :: iso_c_binding
                character(kind = c_char), dimension(*) :: groupstr
                character(kind = c_char), dimension(*) :: varnamestr
                real :: value
            end subroutine get_Float
        end interface

        this%init = 1
        groupstr = group
        groupstr(LEN(groupstr):LEN(groupstr)) = CHAR(0)        
        varnamestr = varname
        varnamestr(LEN(varnamestr):LEN(varnamestr)) = CHAR(0)
        
        call get_Float(groupstr, varnamestr, value)
        
    end subroutine getFloat

    subroutine getInteger(this, group, varname, value)

        implicit none
        class(csm_io_type) :: this
        character(len=*), intent(in) :: group
        character(len=*), intent(in) :: varname
        character(LEN(group)+1) :: groupstr
        character(LEN(varname)+1) :: varnamestr
        integer, intent(out) :: value

        interface
            subroutine get_Integer(groupstr, varnamestr, value)bind(C, name = 'getInteger')
                use, intrinsic :: iso_c_binding
                character(kind = c_char), dimension(*) :: groupstr
                character(kind = c_char), dimension(*) :: varnamestr
                integer :: value
            end subroutine get_Integer
        end interface

        this%init = 1
        groupstr = group
        groupstr(LEN(groupstr):LEN(groupstr)) = CHAR(0)        
        varnamestr = varname
        varnamestr(LEN(varnamestr):LEN(varnamestr)) = CHAR(0)
        
        call get_Integer(groupstr, varnamestr, value)

    end subroutine getInteger

    subroutine getString(this, group, varname, value)

        implicit none
        class(csm_io_type) :: this
        character(len=*), intent(in) :: group
        character(len=*), intent(in) :: varname
        character(LEN(group)+1) :: groupstr
        character(LEN(varname)+1) :: varnamestr
        character(len = *), intent(out) :: value
        integer :: vsize


        interface
            subroutine get_String(groupstr, varnamestr, valuestr, vsize)bind(C, name = 'getString')
                use, intrinsic :: iso_c_binding
                character(kind = c_char), dimension(*) :: groupstr
                character(kind = c_char), dimension(*) :: varnamestr
                character(kind = c_char), dimension(*) :: valuestr
                integer :: vsize
            end subroutine get_String
        end interface

        this%init = 1
        groupstr = group
        groupstr(LEN(groupstr):LEN(groupstr)) = CHAR(0)        
        varnamestr = varname
        varnamestr(LEN(varnamestr):LEN(varnamestr)) = CHAR(0)
        value = CHAR(0)
        vsize = LEN(value)

        call get_String(groupstr, varnamestr, value, vsize)
        
    end subroutine getString

    subroutine getIndexFloat(this, group, varname, value, index)
      
        implicit none
        class(csm_io_type) :: this
        character(len=*), intent(in) :: group
        character(len=*), intent(in) :: varname
        character(LEN(group)+1) :: groupstr
        character(LEN(varname)+1) :: varnamestr
        real, intent(out) :: value
        integer, intent(in) :: index
    
        interface
            subroutine get_IndexFloat(groupstr, varnamestr, value, index)bind(C, name = 'getIndexFloat')
                use, intrinsic :: iso_c_binding
                character(kind = c_char), dimension(*) :: groupstr
                character(kind = c_char), dimension(*) :: varnamestr
                real :: value
                integer :: index
            end subroutine get_IndexFloat
        end interface
        
        this%init = 1
        groupstr = group
        groupstr(LEN(groupstr):LEN(groupstr)) = CHAR(0)        
        varnamestr = varname
        varnamestr(LEN(varnamestr):LEN(varnamestr)) = CHAR(0)
    
        call get_IndexFloat(groupstr, varnamestr, value, index)
    
    end subroutine getIndexFloat
    
    subroutine getIndexInteger(this, group, varname, value, index)
    
        implicit none
        class(csm_io_type) :: this
        character(len=*), intent(in) :: group
        character(len=*), intent(in) :: varname
        character(LEN(group)+1) :: groupstr
        character(LEN(varname)+1) :: varnamestr
        integer, intent(out) :: value
        integer, intent(in) :: index
    
        interface
            subroutine get_IndexInteger(groupstr, varnamestr, value, index)bind(C, name = 'getIndexInteger')
                use, intrinsic :: iso_c_binding
                character(kind = c_char), dimension(*) :: groupstr
                character(kind = c_char), dimension(*) :: varnamestr
                integer :: value
                integer :: index
            end subroutine get_IndexInteger
        end interface
        
        this%init = 1
        groupstr = group
        groupstr(LEN(groupstr):LEN(groupstr)) = CHAR(0)        
        varnamestr = varname
        varnamestr(LEN(varnamestr):LEN(varnamestr)) = CHAR(0)
    
        call get_IndexInteger(groupstr, varnamestr, value, index)
    
    end subroutine getIndexInteger
    
    subroutine getIndexString(this, group, varname, value, index)
    
        implicit none
        class(csm_io_type) :: this
        character(len=*), intent(in) :: group
        character(len=*), intent(in) :: varname
        character(LEN(group)+1) :: groupstr
        character(LEN(varname)+1) :: varnamestr
        character(len=*), intent(out) :: value
        integer :: vsize
        integer, intent(in) :: index
    
        interface
            subroutine get_IndexString(groupstr, varnamestr, valuestr, vsize, index)bind(C, name = 'getIndexString')
                use, intrinsic :: iso_c_binding
                character(kind = c_char), dimension(*) :: groupstr
                character(kind = c_char), dimension(*) :: varnamestr
                character(kind = c_char), dimension(*) :: valuestr
                integer :: vsize
                integer :: index
            end subroutine get_IndexString
        end interface
        
        this%init = 1
        groupstr = group
        groupstr(LEN(groupstr):LEN(groupstr)) = CHAR(0)
        varnamestr = varname
        varnamestr(LEN(varnamestr):LEN(varnamestr)) = CHAR(0)
        value = CHAR(0)
        vsize = LEN(value)
        
        call get_IndexString(groupstr, varnamestr, value, vsize, index)
    
    end subroutine getIndexString
    
    subroutine getArrayFloat(this, group, varname, value, size)

        implicit none
        class(csm_io_type) :: this
        character(len=*), intent(in) :: group
        character(len=*), intent(in) :: varname
        character(LEN(group)+1) :: groupstr
        character(LEN(varname)+1) :: varnamestr
        real, dimension(*), intent(out) :: value
        character(len=*), intent(in) :: size
        character(LEN(size)+1) :: sizestr

        interface
            subroutine get_ArrayFloat(groupstr, varnamestr, value, sizestr)bind(C, name = 'getArrayFloat')
                use, intrinsic :: iso_c_binding
                character(kind = c_char), dimension(*) :: groupstr
                character(kind = c_char), dimension(*) :: varnamestr
                real, dimension(*) :: value
                character(kind = c_char), dimension(*) :: sizestr
            end subroutine get_ArrayFloat
        end interface
        
        this%init = 1
        groupstr = group
        groupstr(LEN(groupstr):LEN(groupstr)) = CHAR(0)        
        varnamestr = varname
        varnamestr(LEN(varnamestr):LEN(varnamestr)) = CHAR(0)
        sizestr = size
        sizestr(LEN(sizestr):LEN(sizestr)) = CHAR(0)

        call get_ArrayFloat(groupstr, varnamestr, value, sizestr)

    end subroutine getArrayFloat

    subroutine getArrayInteger(this, group, varname, value, size)

        implicit none
        class(csm_io_type) :: this
        character(len=*), intent(in) :: group
        character(len=*), intent(in) :: varname
        character(LEN(group)+1) :: groupstr
        character(LEN(varname)+1) :: varnamestr
        integer, dimension(:), intent(out) :: value
        character(len=*), intent(in) :: size
        character(LEN(size)+1) :: sizestr

        interface
            subroutine get_ArrayInteger(groupstr, varnamestr, value, sizestr)bind(C, name = 'getArrayInteger')
                use, intrinsic :: iso_c_binding
                character(kind = c_char), dimension(*) :: groupstr
                character(kind = c_char), dimension(*) :: varnamestr
                integer, dimension(*) :: value
                character(kind = c_char), dimension(*) :: sizestr
            end subroutine get_ArrayInteger
        end interface
        
        this%init = 1
        groupstr = group
        groupstr(LEN(groupstr):LEN(groupstr)) = CHAR(0)        
        varnamestr = varname
        varnamestr(LEN(varnamestr):LEN(varnamestr)) = CHAR(0)
        sizestr = size
        sizestr(LEN(sizestr):LEN(sizestr)) = CHAR(0)

        call get_ArrayInteger(groupstr, varnamestr, value, sizestr)

    end subroutine getArrayInteger

    subroutine getArrayString(this, group, varname, value, size)

        implicit none
        class(csm_io_type) :: this
        character(len=*), intent(in) :: group
        character(len=*), intent(in) :: varname
        character(LEN(group)+1) :: groupstr
        character(LEN(varname)+1) :: varnamestr
        character(len=*), dimension(:), intent(out) :: value
        integer :: vsize
        character(len=*), intent(in) :: size
        character(LEN(size)+1) :: sizestr

        interface
            subroutine get_ArrayString(groupstr, varnamestr, value, vsize, sizestr)bind(C, name = 'getArrayString')
                use, intrinsic :: iso_c_binding
                character(kind = c_char), dimension(*) :: groupstr
                character(kind = c_char), dimension(*) :: varnamestr
                character(kind = c_char), dimension(*):: value
                integer :: vsize
                character(kind = c_char), dimension(*) :: sizestr
            end subroutine get_ArrayString
        end interface
        
        this%init = 1
        groupstr = group
        groupstr(LEN(groupstr):LEN(groupstr)) = CHAR(0)        
        varnamestr = varname
        varnamestr(LEN(varnamestr):LEN(varnamestr)) = CHAR(0)
        vsize = LEN(value)
        sizestr = size
        sizestr(LEN(sizestr):LEN(sizestr)) = CHAR(0)

        call get_ArrayString(groupstr, varnamestr, value, vsize, sizestr)
        
    end subroutine getArrayString

    subroutine getForKeyFloat(this, group, key, varname, value)
      
        implicit none
        class(csm_io_type) :: this
        character(len=*), intent(in) :: group
        character(len=*), intent(in) :: varname
        character(LEN(group)+1) :: groupstr
        character(LEN(varname)+1) :: varnamestr
        integer, intent(in) :: key
        real, intent(out) :: value
        
        interface
          subroutine get_ForKeyFloat(groupstr, key, varnamestr, value)bind(C, name = 'getForKeyFloat')
            use, intrinsic :: iso_c_binding
            character(kind = c_char), dimension(*) :: groupstr
            integer :: key
            character(kind = c_char), dimension(*) :: varnamestr
            real :: value
          end subroutine get_ForKeyFloat
        end interface
        
        this%init = 1
        groupstr = group
        groupstr(LEN(groupstr):LEN(groupstr)) = CHAR(0)        
        varnamestr = varname
        varnamestr(LEN(varnamestr):LEN(varnamestr)) = CHAR(0)
        
        call get_ForKeyFloat(groupstr, key, varnamestr, value)
        
    end subroutine getForKeyFloat
    
    subroutine getForKeyInteger(this, group, key, varname, value)
    
        implicit none
        class(csm_io_type) :: this
        character(len=*), intent(in) :: group
        character(len=*), intent(in) :: varname
        character(LEN(group)+1) :: groupstr
        character(LEN(varname)+1) :: varnamestr
        integer, intent(in) :: key
        integer, intent(out) :: value
    
        interface
            subroutine get_ForKeyInteger(groupstr, key, varnamestr, value)bind(C, name = 'getForKeyInteger')
                use, intrinsic :: iso_c_binding
                character(kind = c_char), dimension(*) :: groupstr
                integer :: key
                character(kind = c_char), dimension(*) :: varnamestr
                integer :: value
            end subroutine get_ForKeyInteger
        end interface
        
        this%init = 1
        groupstr = group
        groupstr(LEN(groupstr):LEN(groupstr)) = CHAR(0)        
        varnamestr = varname
        varnamestr(LEN(varnamestr):LEN(varnamestr)) = CHAR(0)
    
        call get_ForKeyInteger(groupstr, key, varnamestr, value)
        
    end subroutine getForKeyInteger
    
    
    subroutine getForKeyString(this, group, key, varname, value)
    
        implicit none
        class(csm_io_type) :: this
        character(len=*), intent(in) :: group
        character(len=*), intent(in) :: varname
        character(LEN(group)+1) :: groupstr
        character(LEN(varname)+1) :: varnamestr
        integer, intent(in) :: key
        character(len=*), intent(out) :: value
        integer :: vsize
        
        interface
            subroutine get_ForKeyString(groupstr, key, varnamestr, valuestr, vsize)bind(C, name = 'getForKeyString')
                use, intrinsic :: iso_c_binding
                character(kind = c_char), dimension(*) :: groupstr
                character(kind = c_char), dimension(*) :: varnamestr
                integer :: key
                character(kind = c_char), dimension(*) :: valuestr
                integer :: vsize
            end subroutine get_ForKeyString
        end interface
        
        this%init = 1
        groupstr = group
        groupstr(LEN(groupstr):LEN(groupstr)) = CHAR(0)        
        varnamestr = varname
        varnamestr(LEN(varnamestr):LEN(varnamestr)) = CHAR(0)
        value = CHAR(0)
        vsize = LEN(value)
    
        call get_ForKeyString(groupstr, key, varnamestr, value, vsize)
        
    end subroutine getForKeyString

    subroutine getFor2KeyFloat(this, group, key, key2, varname, value)
      
        implicit none
        class(csm_io_type) :: this
        character(len=*), intent(in) :: group
        character(len=*), intent(in) :: varname
        character(LEN(group)+1) :: groupstr
        character(LEN(varname)+1) :: varnamestr
        integer, intent(in) :: key
        integer, intent(in) :: key2
        real, intent(out) :: value
        
        interface
          subroutine get_For2KeyFloat(groupstr, key, key2, varnamestr, value)bind(C, name = 'getFor2KeyFloat')
            use, intrinsic :: iso_c_binding
            character(kind = c_char), dimension(*) :: groupstr
            integer :: key
            integer :: key2
            character(kind = c_char), dimension(*) :: varnamestr
            real :: value
          end subroutine get_For2KeyFloat
        end interface
        
        this%init = 1
        groupstr = group
        groupstr(LEN(groupstr):LEN(groupstr)) = CHAR(0)        
        varnamestr = varname
        varnamestr(LEN(varnamestr):LEN(varnamestr)) = CHAR(0)
        
        call get_For2KeyFloat(groupstr, key, key2, varnamestr, value)
        
    end subroutine getFor2KeyFloat
    
    subroutine getFor2KeyInteger(this, group, key, key2, varname, value)
    
        implicit none
        class(csm_io_type) :: this
        character(len=*), intent(in) :: group
        character(len=*), intent(in) :: varname
        character(LEN(group)+1) :: groupstr
        character(LEN(varname)+1) :: varnamestr
        integer, intent(in) :: key
        integer, intent(in) :: key2
        integer, intent(out) :: value
    
        interface
            subroutine get_For2KeyInteger(groupstr, key, key2, varnamestr, value)bind(C, name = 'getFor2KeyInteger')
                use, intrinsic :: iso_c_binding
                character(kind = c_char), dimension(*) :: groupstr
                integer :: key
                integer :: key2
                character(kind = c_char), dimension(*) :: varnamestr
                integer :: value
            end subroutine get_For2KeyInteger
        end interface
        
        this%init = 1
        groupstr = group
        groupstr(LEN(groupstr):LEN(groupstr)) = CHAR(0)        
        varnamestr = varname
        varnamestr(LEN(varnamestr):LEN(varnamestr)) = CHAR(0)
    
        call get_For2KeyInteger(groupstr, key, key2, varnamestr, value)
        
    end subroutine getFor2KeyInteger
    
    
    subroutine getFor2KeyString(this, group, key, key2, varname, value)
    
        implicit none
        class(csm_io_type) :: this
        character(len=*), intent(in) :: group
        character(len=*), intent(in) :: varname
        character(LEN(group)+1) :: groupstr
        character(LEN(varname)+1) :: varnamestr
        integer, intent(in) :: key
        integer, intent(in) :: key2
        character(len=*), intent(out) :: value
        integer :: vsize
        
        interface
            subroutine get_For2KeyString(groupstr, key, key2, varnamestr, valuestr, vsize)bind(C, name = 'getFor2KeyString')
                use, intrinsic :: iso_c_binding
                character(kind = c_char), dimension(*) :: groupstr
                character(kind = c_char), dimension(*) :: varnamestr
                integer :: key
                integer :: key2
                character(kind = c_char), dimension(*) :: valuestr
                integer :: vsize
            end subroutine get_For2KeyString
        end interface
        
        this%init = 1
        groupstr = group
        groupstr(LEN(groupstr):LEN(groupstr)) = CHAR(0)        
        varnamestr = varname
        varnamestr(LEN(varnamestr):LEN(varnamestr)) = CHAR(0)
        value = CHAR(0)
        vsize = LEN(value)
    
        call get_For2KeyString(groupstr, key, key2, varnamestr, value, vsize)
        
    end subroutine getFor2KeyString

    
    
    subroutine setFloat(this, group, varname, value)

        implicit none
        class(csm_io_type) :: this
        character(len=*), intent(in) :: group
        character(len=*), intent(in) :: varname
        character(LEN(group)+1) :: groupstr
        character(LEN(varname)+1) :: varnamestr
        real, intent(in) :: value


        interface
            subroutine set_Float(groupstr, varnamestr, value)bind(C, name = 'setFloat')
                use, intrinsic :: iso_c_binding
                character(kind = c_char), dimension(*) :: groupstr
                character(kind = c_char), dimension(*) :: varnamestr
                real :: value
            end subroutine set_Float
        end interface
        
        this%init = 1
        groupstr = group
        groupstr(LEN(groupstr):LEN(groupstr)) = CHAR(0)        
        varnamestr = varname
        varnamestr(LEN(varnamestr):LEN(varnamestr)) = CHAR(0)

        call set_Float(groupstr, varnamestr, value)
        
    end subroutine setFloat

    subroutine setInteger(this, group, varname, value)
      
        implicit none
        class(csm_io_type) :: this
        character(len=*), intent(in) :: group
        character(len=*), intent(in) :: varname
        character(LEN(group)+1) :: groupstr
        character(LEN(varname)+1) :: varnamestr
        integer, intent(in) :: value
        
        interface
          subroutine set_Integer(groupstr, varnamestr, value)bind(C, name = 'setInteger')
            use, intrinsic :: iso_c_binding
            character(kind = c_char), dimension(*) :: groupstr
            character(kind = c_char), dimension(*) :: varnamestr
            integer :: value
          end subroutine set_Integer
        end interface
        
        this%init = 1
        groupstr = group
        groupstr(LEN(groupstr):LEN(groupstr)) = CHAR(0)        
        varnamestr = varname
        varnamestr(LEN(varnamestr):LEN(varnamestr)) = CHAR(0)
        
        call set_Integer(groupstr, varnamestr, value)
      
    end subroutine setInteger
    
    subroutine setString(this, group, varname, value)

        implicit none
        class(csm_io_type) :: this
        character(len=*), intent(in) :: group
        character(len=*), intent(in) :: varname
        character(LEN(group)+1) :: groupstr
        character(LEN(varname)+1) :: varnamestr
        character(len=*), intent(in) :: value
        character(LEN(value)+1) :: valuestr
        
        interface
            subroutine set_String(groupstr, varnamestr, valuestr)bind(C, name = 'setString')
                use, intrinsic :: iso_c_binding
                character(kind = c_char), dimension(*) :: groupstr
                character(kind = c_char), dimension(*) :: varnamestr
                character(kind = c_char), dimension(*) :: valuestr
            end subroutine set_String
        end interface
        
        this%init = 1
        groupstr = group
        groupstr(LEN(groupstr):LEN(groupstr)) = CHAR(0)        
        varnamestr = varname
        varnamestr(LEN(varnamestr):LEN(varnamestr)) = CHAR(0)
        valuestr = value
        valuestr(LEN(valuestr):LEN(valuestr)) = CHAR(0)
        
        call set_String(groupstr, varnamestr, valuestr)
        
    end subroutine setString

    subroutine setIndexFloat(this, group, varname, value, index)
      
        implicit none
        class(csm_io_type) :: this
        character(len=*), intent(in) :: group
        character(len=*), intent(in) :: varname
        character(LEN(group)+1) :: groupstr
        character(LEN(varname)+1) :: varnamestr
        real, intent(in) :: value
        integer, intent(in) :: index

        interface
            subroutine set_IndexFloat(groupstr, varnamestr, value, index)bind(C, name = 'setIndexFloat')
                use, intrinsic :: iso_c_binding
                character(kind = c_char), dimension(*) :: groupstr
                character(kind = c_char), dimension(*) :: varnamestr
                real :: value
                integer :: index
            end subroutine set_IndexFloat
        end interface
            
        this%init = 1
        groupstr = group
        groupstr(LEN(groupstr):LEN(groupstr)) = CHAR(0)        
        varnamestr = varname
        varnamestr(LEN(varnamestr):LEN(varnamestr)) = CHAR(0)
        
        call set_IndexFloat(groupstr, varnamestr, value, index)
        
    end subroutine setIndexFloat

    subroutine setIndexInteger(this, group, varname, value, index)
      
        implicit none
        class(csm_io_type) :: this
        character(len=*), intent(in) :: group
        character(len=*), intent(in) :: varname
        character(LEN(group)+1) :: groupstr
        character(LEN(varname)+1) :: varnamestr
        integer, intent(in) :: value
        integer, intent(in) :: index
        
        interface
          subroutine set_IndexInteger(groupstr, varnamestr, value, index)bind(C, name = 'setIndexInteger')
            use, intrinsic :: iso_c_binding
            character(kind = c_char), dimension(*) :: groupstr
            character(kind = c_char), dimension(*) :: varnamestr
            integer :: value
            integer index
          end subroutine set_IndexInteger
        end interface
        
        this%init = 1
        groupstr = group
        groupstr(LEN(groupstr):LEN(groupstr)) = CHAR(0)        
        varnamestr = varname
        varnamestr(LEN(varnamestr):LEN(varnamestr)) = CHAR(0)
              
        call set_IndexInteger(groupstr, varnamestr, value, index)
      
    end subroutine setIndexInteger
    
    subroutine setIndexString(this, group, varname, value, index)

        implicit none
        class(csm_io_type) :: this
        character(len=*), intent(in) :: group
        character(len=*), intent(in) :: varname
        character(LEN(group)+1) :: groupstr
        character(LEN(varname)+1) :: varnamestr
        character(len=*), intent(in) :: value
        character(LEN(value)+1) :: valuestr
        integer, intent(in) :: index
        
        interface
            subroutine set_IndexString(groupstr, varnamestr, valuestr, index)bind(C, name = 'setIndexString')
                use, intrinsic :: iso_c_binding
                character(kind = c_char), dimension(*) :: groupstr
                character(kind = c_char), dimension(*) :: varnamestr
                character(kind = c_char), dimension(*) :: valuestr
                integer :: index
            end subroutine set_IndexString
        end interface
        
        this%init = 1
        groupstr = group
        groupstr(LEN(groupstr):LEN(groupstr)) = CHAR(0)        
        varnamestr = varname
        varnamestr(LEN(varnamestr):LEN(varnamestr)) = CHAR(0)
        valuestr = value
        valuestr(LEN(valuestr):LEN(valuestr)) = CHAR(0)
        
        call set_IndexString(groupstr, varnamestr, valuestr, index)
        
    end subroutine setIndexString

    subroutine setForKeyFloat(this, group, key, varname, value)
      
        implicit none
        class(csm_io_type) :: this
        character(len=*), intent(in) :: group
        character(len=*), intent(in) :: varname
        character(LEN(group)+1) :: groupstr
        character(LEN(varname)+1) :: varnamestr
        integer, intent(in) :: key
        real, intent(in) :: value
        
        interface
          subroutine set_ForKeyFloat(groupstr, key, varnamestr, value)bind(C, name = 'setForKeyFloat')
            use, intrinsic :: iso_c_binding
            character(kind = c_char), dimension(*) :: groupstr
            integer :: key
            character(kind = c_char), dimension(*) :: varnamestr
            real :: value
          end subroutine set_ForKeyFloat
        end interface
        
        this%init = 1
        groupstr = group
        groupstr(LEN(groupstr):LEN(groupstr)) = CHAR(0)        
        varnamestr = varname
        varnamestr(LEN(varnamestr):LEN(varnamestr)) = CHAR(0)
        
        call set_ForKeyFloat(groupstr, key, varname, value)
        
    end subroutine setForKeyFloat
    
    subroutine setForKeyInteger(this, group, key, varname, value)

        implicit none
        class(csm_io_type) :: this
        character(len=*), intent(in) :: group
        character(len=*), intent(in) :: varname
        character(LEN(group)+1) :: groupstr
        character(LEN(varname)+1) :: varnamestr
        integer, intent(in) :: key
        integer, intent(in) :: value

        interface
            subroutine set_ForKeyInteger(groupstr, key, varnamestr, value)bind(C, name = 'setForKeyInteger')
                use, intrinsic :: iso_c_binding
                character(kind = c_char), dimension(*) :: groupstr
                integer :: key
                character(kind = c_char), dimension(*) :: varnamestr
                integer :: value
            end subroutine set_ForKeyInteger
        end interface
        
        this%init = 1
        groupstr = group
        groupstr(LEN(groupstr):LEN(groupstr)) = CHAR(0)        
        varnamestr = varname
        varnamestr(LEN(varnamestr):LEN(varnamestr)) = CHAR(0)

        call set_ForKeyInteger(groupstr, key, varnamestr, value)
        
    end subroutine setForKeyInteger


    subroutine setForKeyString(this, group, key, varname, value)

        implicit none
        class(csm_io_type) :: this
        character(len=*), intent(in) :: group
        character(len=*), intent(in) :: varname
        character(LEN(group)+1) :: groupstr
        character(LEN(varname)+1) :: varnamestr
        integer, intent(in) :: key
        character(len=*), intent(in) :: value
        character(LEN(value)+1) :: valuestr
        
        interface
            subroutine set_ForKeyString(groupstr, key, varnamestr, valuestr)bind(C, name = 'setForKeyString')
                use, intrinsic :: iso_c_binding
                character(kind = c_char), dimension(*) :: groupstr
                character(kind = c_char), dimension(*) :: varnamestr
                integer :: key
                character(kind = c_char), dimension(*) :: valuestr
            end subroutine set_ForKeyString
        end interface
        
        this%init = 1
        groupstr = group
        groupstr(LEN(groupstr):LEN(groupstr)) = CHAR(0)        
        varnamestr = varname
        varnamestr(LEN(varnamestr):LEN(varnamestr)) = CHAR(0)
        valuestr = value
        valuestr(LEN(valuestr):LEN(valuestr)) = CHAR(0)

        call set_ForKeyString(groupstr, key, varnamestr, valuestr)
        
    end subroutine setForKeyString
    
    subroutine setFor2KeyFloat(this, group, key, key2, varname, value)
      
        implicit none
        class(csm_io_type) :: this
        character(len=*), intent(in) :: group
        character(len=*), intent(in) :: varname
        character(LEN(group)+1) :: groupstr
        character(LEN(varname)+1) :: varnamestr
        integer, intent(in) :: key
        integer, intent(in) :: key2
        real, intent(in) :: value
        
        interface
          subroutine set_For2KeyFloat(groupstr, key, key2, varnamestr, value)bind(C, name = 'setFor2KeyFloat')
            use, intrinsic :: iso_c_binding
            character(kind = c_char), dimension(*) :: groupstr
            integer :: key
            integer :: key2
            character(kind = c_char), dimension(*) :: varnamestr
            real :: value
          end subroutine set_For2KeyFloat
        end interface
        
        this%init = 1
        groupstr = group
        groupstr(LEN(groupstr):LEN(groupstr)) = CHAR(0)        
        varnamestr = varname
        varnamestr(LEN(varnamestr):LEN(varnamestr)) = CHAR(0)
        
        call set_For2KeyFloat(groupstr, key, key2, varname, value)
        
    end subroutine setFor2KeyFloat
    
    subroutine setFor2KeyInteger(this, group, key, key2, varname, value)

        implicit none
        class(csm_io_type) :: this
        character(len=*), intent(in) :: group
        character(len=*), intent(in) :: varname
        character(LEN(group)+1) :: groupstr
        character(LEN(varname)+1) :: varnamestr
        integer, intent(in) :: key
        integer, intent(in) :: key2
        integer, intent(in) :: value

        interface
            subroutine set_For2KeyInteger(groupstr, key, key2, varnamestr, value)bind(C, name = 'setFor2KeyInteger')
                use, intrinsic :: iso_c_binding
                character(kind = c_char), dimension(*) :: groupstr
                integer :: key
                integer :: key2
                character(kind = c_char), dimension(*) :: varnamestr
                integer :: value
            end subroutine set_For2KeyInteger
        end interface
        
        this%init = 1
        groupstr = group
        groupstr(LEN(groupstr):LEN(groupstr)) = CHAR(0)        
        varnamestr = varname
        varnamestr(LEN(varnamestr):LEN(varnamestr)) = CHAR(0)

        call set_For2KeyInteger(groupstr, key, key2, varnamestr, value)
        
    end subroutine setFor2KeyInteger


    subroutine setFor2KeyString(this, group, key, key2, varname, value)

        implicit none
        class(csm_io_type) :: this
        character(len=*), intent(in) :: group
        character(len=*), intent(in) :: varname
        character(LEN(group)+1) :: groupstr
        character(LEN(varname)+1) :: varnamestr
        integer, intent(in) :: key
        integer, intent(in) :: key2
        character(len=*), intent(in) :: value
        character(LEN(value)+1) :: valuestr
        
        interface
            subroutine set_For2KeyString(groupstr, key, key2, varnamestr, valuestr)bind(C, name = 'setFor2KeyString')
                use, intrinsic :: iso_c_binding
                character(kind = c_char), dimension(*) :: groupstr
                character(kind = c_char), dimension(*) :: varnamestr
                integer :: key
                integer :: key2
                character(kind = c_char), dimension(*) :: valuestr
            end subroutine set_For2KeyString
        end interface
        
        this%init = 1
        groupstr = group
        groupstr(LEN(groupstr):LEN(groupstr)) = CHAR(0)        
        varnamestr = varname
        varnamestr(LEN(varnamestr):LEN(varnamestr)) = CHAR(0)
        valuestr = value
        valuestr(LEN(valuestr):LEN(valuestr)) = CHAR(0)

        call set_For2KeyString(groupstr, key, key2, varnamestr, valuestr)
        
    end subroutine setFor2KeyString
    
    subroutine READ_WTH_Y2_4K(fileww, firstweatherdate, yrdoy, firstweatherday, lastweatherday, lnum, nrecords, mxrecords, errcode)

         implicit none
         character(len=*), intent(in) :: fileww
         character(LEN(fileww)+1) :: filewwstr
         integer, intent(in) :: firstweatherdate   
         integer, intent(in) :: yrdoy
         integer, intent(out) :: firstweatherday
         integer, intent(out) :: lastweatherday
         integer, intent(out) :: lnum
         integer, intent(out) :: nrecords
         integer, intent(in)  :: mxrecords
         integer, intent(out) :: errcode

    
         interface
             subroutine readwthfile(filewwstr, firstweatherdate, yrdoy, firstweatherday, lastweatherday, lnum, nrecords, mxrecords, errcode)&
                 bind(C, name = 'READ_WTH_Y2_4K')
                 use, intrinsic :: iso_c_binding
                 character(kind = c_char), dimension(*) :: filewwstr
                 integer :: firstweatherdate
                 integer :: yrdoy
                 integer :: firstweatherday
                 integer :: lastweatherday
                 integer :: lnum
                 integer :: nrecords
                 integer :: mxrecords
                 integer :: errcode                
             end subroutine readwthfile
         end interface
    
         filewwstr = fileww
         filewwstr(LEN(filewwstr):LEN(filewwstr)) = CHAR(0)     
    
         call readwthfile(filewwstr, firstweatherdate, yrdoy, firstweatherday, lastweatherday, lnum, nrecords, mxrecords, errcode)
        
    end subroutine READ_WTH_Y2_4K
     
    subroutine READ_WTH_HOURLY(fileww, yrdoy, firstweatherday, lastweatherday, lnum, nrecords, mxrecords, errcode)

        implicit none
        character(len=*), intent(in) :: fileww
        character(LEN(fileww)+1) :: filewwstr
        integer, intent(in) :: yrdoy
        integer, intent(out) :: firstweatherday
        integer, intent(out) :: lastweatherday
        integer, intent(out) :: lnum
        integer, intent(out) :: nrecords
        integer, intent(in)  :: mxrecords
        integer, intent(out) :: errcode

   
        interface
            subroutine readwthhr(filewwstr, yrdoy, firstweatherday, lastweatherday, lnum, nrecords, mxrecords, errcode)&
                bind(C, name = 'READ_WTH_HOURLY')
                use, intrinsic :: iso_c_binding
                character(kind = c_char), dimension(*) :: filewwstr
                integer :: yrdoy
                integer :: firstweatherday
                integer :: lastweatherday
                integer :: lnum
                integer :: nrecords
                integer :: mxrecords
                integer :: errcode                
            end subroutine readwthhr
        end interface
   
        filewwstr = fileww
        filewwstr(LEN(filewwstr):LEN(filewwstr)) = CHAR(0)    
   
        call readwthhr(filewwstr, yrdoy, firstweatherday, lastweatherday, lnum, nrecords, mxrecords, errcode)
       
    end subroutine READ_WTH_HOURLY
    
    subroutine READ_WTH_CSV(fileww, firstweatherdate, yrdoy, firstweatherday, lastweatherday, lnum, nrecords, mxrecords, errcode)

         implicit none
         character(len=*), intent(in) :: fileww
         character(LEN(fileww)+1) :: filewwstr
         integer, intent(in) :: firstweatherdate   
         integer, intent(in) :: yrdoy
         integer, intent(out) :: firstweatherday
         integer, intent(out) :: lastweatherday
         integer, intent(out) :: lnum
         integer, intent(out) :: nrecords
         integer, intent(in)  :: mxrecords
         integer, intent(out) :: errcode

    
         interface
             subroutine readwthcsv(filewwstr, firstweatherdate, yrdoy, firstweatherday, lastweatherday, lnum, nrecords, mxrecords, errcode)&
                 bind(C, name = 'READ_WTH_CSV')
                 use, intrinsic :: iso_c_binding
                 character(kind = c_char), dimension(*) :: filewwstr
                 integer :: firstweatherdate
                 integer :: yrdoy
                 integer :: firstweatherday
                 integer :: lastweatherday
                 integer :: lnum
                 integer :: nrecords
                 integer :: mxrecords
                 integer :: errcode                
             end subroutine readwthcsv
         end interface
    
         filewwstr = fileww
         filewwstr(LEN(filewwstr):LEN(filewwstr)) = CHAR(0)     
    
         call readwthcsv(filewwstr, firstweatherdate, yrdoy, firstweatherday, lastweatherday, lnum, nrecords, mxrecords, errcode)
        
    end subroutine READ_WTH_CSV
    
end module flexibleio  

