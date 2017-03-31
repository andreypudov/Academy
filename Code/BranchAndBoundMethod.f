module Serial

    implicit none
    public

contains
    subroutine SerialSolution(data)
        integer, dimension(:,:), allocatable, intent(in) :: data

        call UpperBound(data)
        call LowerBound(data)
    end subroutine

    subroutine UpperBound(data)
        integer, dimension(:,:), allocatable, intent(in) :: data

        integer arrival
        integer begin
        integer end

        integer package
        integer object

        integer t   ! the moment when the object arrives
        integer a   ! the penalty per unit of time
        integer n   ! the number of objects in the stream
        integer tau ! the duration of servicing

        integer penalty
        integer estimate
        integer upper

        ! the time values for the first object
        end   = 0
        upper = 0

        print '(/5X,A,7X,A,9X,A)', 'Arrival', 'Begin', 'End'

        do package = 1, size(data, 1)
            t   = data(package, 1)
            a   = data(package, 2)
            n   = data(package, 3)
            tau = data(package, 4)

            do object = 1, n
                arrival = t
                begin   = end
                end     = begin + tau

                print '(I,I,I)', arrival, begin, end
            end do

            penalty  = end - arrival
            estimate = a * penalty
            upper    = upper + estimate

            print '(5X,A,I2,A,I3,A,I4)', 'a = ', a, ', d = ', penalty, ', estimate = ', estimate
        end do

        print '(/A,I)', 'Upper bound: ', upper
    end subroutine

    subroutine LowerBound(data)
        integer, dimension(:,:), allocatable, intent(in) :: data

        integer arrival
        integer begin
        integer end

        integer package

        integer t   ! the moment when the object arrives
        integer a   ! the penalty per unit of time
        integer n   ! the number of objects in the stream
        integer tau ! the duration of servicing

        integer estimate
        integer lower

        ! the time values for the first object
        end   = 0
        lower = 0

        print '(/5X,A,7X,A,9X,A)', 'Arrival', 'Begin', 'End'

        do package = 1, size(data, 1)
            t   = data(package, 1)
            a   = data(package, 2)
            !n   = data(package, 3)
            tau = data(package, 4)

            arrival = t
            begin   = end
            end     = arrival + begin
            if (package == 1) then
                end = n * tau
            end if

            print '(I,I,I)', arrival, begin, end

            estimate = a * tau
            lower    = lower + estimate

            print '(5X,A,I2,A,I3,A,I4)', 'a = ', a, ', d = ', tau, ', estimate = ', estimate
        end do

        print '(/A,I)', 'Lower bound: ', lower
    end subroutine
end module

module IO

    implicit none
    public

contains
    subroutine ReadData(file, data)
        character(len=*), intent(in) :: file
        integer, dimension(:,:), allocatable, intent(out) :: data

        integer unit
        integer status

        integer t   ! the moment when the object arrives
        integer a   ! the penalty per unit of time
        integer n   ! the number of objects in the stream
        integer tau ! the duration of servicing

        integer rows
        integer row

        rows = GetNumberOfLines(file)
        allocate(data(rows - 1, 5))

        open(newunit = unit, file = file, status = 'old', readonly)

        ! skip data header
        read(unit, *, iostat = status)
        print '(11x,a,11x,a,11x,a,10x,a)', 't', 'a', 'n', 'tau'
        row = 1

        do while ((status == 0) .and. (.not. eof(unit)) .and. (row <= rows))
            read(unit, '(I,I,I,I)', iostat = status) t, a, n, tau

            if (status == 0) then
                print '(I,I,I,I)', t, a, n, tau

                data(row, 1) = t
                data(row, 2) = a
                data(row, 3) = n
                data(row, 4) = tau
                data(row, 5) = 0    ! is serviced
            else
                print '(A)', 'Unable to read data file.', status
            end if

            row = row + 1
        end do

        close(unit)
    end subroutine

    function GetNumberOfLines(file) result(number)
        character(len=*), intent(in) :: file
        integer number

        integer unit
        integer status

        open(newunit = unit, file = file, status = 'old', readonly)
        number = 0

        do while ((status == 0) .and. .not. eof(unit))
            read(unit, *, iostat = status)

            if (status == 0) then
                number = number + 1
            else
                print '(a,i)', 'Unable to read data file.', status
            end if
        end do

        close(unit)
    end function
end module

program BranchAndBoundMethod

    use IO
    use Serial

    implicit none

    integer, dimension(:,:), allocatable :: data

    call ReadData('data.csv', data)

    call SerialSolution(data)

    deallocate(data)
end
