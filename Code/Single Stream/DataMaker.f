module IO

    implicit none
    public

contains
    subroutine WriteData(file, data)
        character(len=*), intent(in) :: file
        integer, dimension(:,:), allocatable, intent(in) :: data

        integer index
        integer unit
        integer status
        integer entry

        character(len=16) buffer

        open(newunit = unit, file = file, status = 'replace')
        write(unit, '(a,a,a,a,a)', iostat = status) 't,', 'tau,', 'a,', 'v,', 'V'

        do index = 1, size(data, 1)
            if (status /= 0) then
                print '(a)', 'Unable to write data file.', status
            end if

            do entry = 1, size(data, 2)
                write(buffer, '(i)') data(index, entry)

                write(unit, '(a\)', iostat = status) trim(adjustl(buffer))
                if (entry < size(data, 2)) then
                    write(unit, '(a\)', iostat = status) ','
                end if
            end do

            if (index < size(data, 1)) then
                write(unit, '(a)', iostat = status)
            end if
        end do

        close(unit)
    end subroutine
end module

program DataMaker

    use IO

    implicit none

    integer, parameter :: NUMBER_OF_OBJECTS = 11

    integer, parameter :: TIME_OF_ARRIVIAL_MAX      = NUMBER_OF_OBJECTS * 2
    integer, parameter :: PENALTY_PER_UNIT_MAX      = NUMBER_OF_OBJECTS * 2
    integer, parameter :: DURATION_OF_SERVICING_MAX = NUMBER_OF_OBJECTS
    integer, parameter :: VOLUME_OF_MATERIAL_MAX    = NUMBER_OF_OBJECTS * 6

    integer, dimension(:,:), allocatable :: data

    integer object
    real buffer

    allocate(data(NUMBER_OF_OBJECTS, 5))
    call random_seed()

    do object = 1, NUMBER_OF_OBJECTS
        ! the moment when the object arrives
        call random_number(buffer)
        data(object, 1) = int((TIME_OF_ARRIVIAL_MAX * buffer) + 1)

        ! the duration of servicing
        call random_number(buffer)
        data(object, 2) = int((DURATION_OF_SERVICING_MAX * buffer) + 1)

        ! the penalty per unit of time
        call random_number(buffer)
        data(object, 3) = int((PENALTY_PER_UNIT_MAX * buffer) + 1)

        ! the volume of material in the object
        call random_number(buffer)
        data(object, 4) = int((DURATION_OF_SERVICING_MAX * buffer) + 1)

        ! the total volume of the reservoir
        data(object, 5) = 0
    end do

    data(NUMBER_OF_OBJECTS, 5) = NUMBER_OF_OBJECTS * VOLUME_OF_MATERIAL_MAX

    call WriteData('data.csv', data)

    deallocate(data)
end
