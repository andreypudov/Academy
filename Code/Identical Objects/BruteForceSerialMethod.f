module BruteForce

    implicit none
    public

contains
    subroutine BruteForceSolution(data)
        integer, dimension(:,:), allocatable, intent(in) :: data

        integer, dimension(:), allocatable :: list
        integer, dimension(:), allocatable :: minlist

        integer minimal
        integer upper

        call ConstructList(data, list)
        allocate(minlist(size(list)))

        minimal = huge(minimal)

        do
            upper = UpperBound(data, list)

            if (upper < minimal) then
                minimal = upper
                minlist = list
            end if

            if (Permutation(list) .eqv. .false.) then
                exit
            end if
        end do

        print '(x)'
        print '(i2\)', minlist
        print '(x,i)', minimal

        deallocate(list)
        deallocate(minlist)
    end subroutine

    function Permutation(list) result(exists)
        integer, dimension(:), intent(inout) :: list
        logical exists

        integer index
        integer jndex
        integer buffer

        index = size(list)
        do while ((index > 1) .and. (list(index - 1) >= list(index)))
            index = index - 1
        end do

        if (index == 1) then
            exists = .false.
            return
        end if

        jndex = size(list)
        do while (list(jndex) <= list(index - 1))
            jndex = jndex - 1
        end do

        buffer          = list(index - 1)
        list(index - 1) = list(jndex)
        list(jndex)     = buffer

        jndex = size(list)
        do while (index < jndex)
            buffer      = list(index)
            list(index) = list(jndex)
            list(jndex) = buffer

            index = index + 1
            jndex = jndex - 1
        end do

        exists = .true.
    end function

    subroutine ConstructList(data, list)
        integer, dimension(:,:), intent(in) :: data
        integer, dimension(:), allocatable, intent(inout) :: list

        integer package
        integer object
        integer total
        integer index

        integer n ! the number of objects in the stream

        total = 0
        do package = 1, size(data, 1)
            n = data(package, 3)

            total = total + n
        end do

        allocate(list(total))

        index = 1
        do package = 1, size(data, 1)
            n = data(package, 3)

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

            arrival = data(package, 1)
            begin   = max(end, arrival)
            end     = begin + data(package, 4)

            delta(package) = end - data(package, 1)
        end do

        do index = 1, size(data, 1)
            upper = upper + (delta(index) * data(index, 2))
        end do
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
        allocate(data(rows - 1, 5))

        open(newunit = unit, file = file, status = 'old', readonly)

        ! skip data header
        read(unit, *, iostat = status)
        print '(11x,a,11x,a,11x,A,10x,a)', 't', 'a', 'n', 'tau'
        row = 1

        do while ((status == 0) .and. (row <= rows))
            read(unit, '(I,I,I,I)', iostat = status) t, a, n, tau

            if (status == 0) then
                print '(i,i,i,i)', t, a, n, tau

                data(row, 1) = t
                data(row, 2) = a
                data(row, 3) = n
                data(row, 4) = tau
                data(row, 5) = 0    ! is serviced
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

program BruteForceMethod

    use IO
    use BruteForce

    implicit none

    integer, dimension(:,:), allocatable :: data

    real start
    real finish

    call cpu_time(start)

    call ReadData('data.csv', data)
    call BruteForceSolution(data)

    call cpu_time(finish)
    print '(a,f12.3,a)', 'Computation time:', finish - start, "s."

    deallocate(data)
end
