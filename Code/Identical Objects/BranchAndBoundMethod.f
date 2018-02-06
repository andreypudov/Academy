module Serial

    implicit none
    public

contains
    subroutine SerialSolution(data)
        integer, dimension(:,:), allocatable, intent(in) :: data

        integer, dimension(:), allocatable :: list

        call ConstructList(data, list)

        print *, UpperBound(data, list)
        print *, LowerBound(data, list)

        deallocate(list)
    end subroutine

    subroutine ConstructList(data, list)
        integer, dimension(:,:), intent(in) :: data
        integer, dimension(:), allocatable, intent(inout) :: list

        integer package
        integer object
        integer total
        integer index

        integer n ! the number of objects in the stream

        total = 0
        do package = 1, size(data, 2)
            n = data(3, package)

            total = total + n
        end do

        allocate(list(total))

        index = 1
        do package = 1, size(data, 2)
            n = data(3, package)

            do object = 1, n
                list(index) = package
                index = index + 1
            end do
        end do
    end subroutine

    function UpperBound(data, list) result(upper)
        integer, dimension(:,:), intent(in) :: data
        integer, dimension(:),   intent(in) :: list
        integer upper

        integer, dimension(size(data, 1)) :: delta

        integer arrival
        integer begin
        integer end

        integer package
        integer index

        ! the time values for the first object
        end   = 0
        upper = 0

        do index = 1, size(list)
            package = list(index)

            arrival = data(1, package)
            begin   = max(end, arrival)
            end     = begin + data(4, package)

            delta(package) = end - data(1, package)
        end do

        do index = 1, size(data, 2)
            upper = upper + (delta(index) * data(2, index))
        end do
    end function

    function LowerBound(data, list) result(lower)
        integer, dimension(:,:), allocatable, intent(in) :: data
        integer, dimension(:),   intent(in) :: list
        integer lower

        lower = -1
    end function
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
        allocate(data(4, rows - 1))

        open(newunit = unit, file = file, status = 'old', readonly)

        ! skip data header
        read(unit, *, iostat = status)
        print '(11x,a,11x,a,11x,A,10x,a)', 't', 'a', 'n', 'tau'
        row = 1

        do while ((status == 0) .and. (row <= rows))
            read(unit, '(I,I,I,I)', iostat = status) t, a, n, tau

            if (status == 0) then
                print '(i,i,i,i)', t, a, n, tau

                data(1, row) = t
                data(2, row) = a
                data(3, row) = n
                data(4, row) = tau
                data(5, row) = 0    ! is serviced
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

        do while (status == 0)
            read(unit, *, iostat = status)

            if (status == 0) then
                number = number + 1
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
