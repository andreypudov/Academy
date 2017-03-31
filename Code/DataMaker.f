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
        write(unit, '(a,a,a,a)', iostat = status) 't,', 'a,', 'n,', 'tau'

        do index = 1, size(data, 1)
            if (status /= 0) then
                print '(a)', 'Unable to write data file.', status
            end if

            do entry = 1, 4
                write(buffer, '(i)') data(index, entry)

                write(unit, '(a\)', iostat = status) trim(adjustl(buffer))
                if (entry < 4) then
                    write(unit, '(a\)', iostat = status) ','
                end if
            end do

            write(unit, '(a)', iostat = status)
        end do

        close(unit)
    end subroutine
end module

program DataMaker

    use IO

    implicit none

    integer, parameter :: NUMBER_OF_PACKAGES = 5
    integer, parameter :: NUMBER_OF_OBJECTS  = (NUMBER_OF_PACKAGES ** 2) + 1

    integer, parameter :: TIME_OF_ARRIVIAL_MAX      = NUMBER_OF_OBJECTS * 2
    integer, parameter :: PENALTY_PER_UNIT_MAX      = NUMBER_OF_OBJECTS * 2
    integer, parameter :: DURATION_OF_SERVICING_MAX = NUMBER_OF_OBJECTS

    integer, parameter :: OBJECTS_PER_PACKAGE_MAX = ceiling(real(NUMBER_OF_OBJECTS) / NUMBER_OF_PACKAGES)

    integer, dimension(:,:), allocatable :: data

    integer package
    integer object
    integer total

    real buffer

    allocate(data(NUMBER_OF_PACKAGES, 4))
    call random_seed()

    total = 0

    do package = 1, NUMBER_OF_PACKAGES
        ! the moment when the object arrives
        call random_number(buffer)
        data(package, 1) = int((TIME_OF_ARRIVIAL_MAX * buffer) + 1)

        ! the penalty per unit of time
        call random_number(buffer)
        data(package, 2) = int((PENALTY_PER_UNIT_MAX * buffer) + 1)

        ! the number of objects in the stream
        call random_number(buffer)
        data(package, 3) = int((OBJECTS_PER_PACKAGE_MAX * buffer) + 1)

        ! the duration of servicing
        call random_number(buffer)
        data(package, 4) = int((DURATION_OF_SERVICING_MAX * buffer) + 1)

        total = total + data(package, 3)
    end do

    data(NUMBER_OF_PACKAGES, 3) = data(NUMBER_OF_PACKAGES, 3) + (NUMBER_OF_OBJECTS - total)

    call WriteData('data.csv', data)

    deallocate(data)
end
