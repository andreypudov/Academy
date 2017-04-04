! ifort -free -parallel -qopenmp BruteForceParallelMethod.f -o BruteForceParallelMethod
! ./BruteForceParallelMethod

module BruteForceParallel

    use omp_lib

    implicit none
    public

contains
    subroutine BruteForceParallelSolution(data)
        integer, dimension(:,:), allocatable, intent(in) :: data

        integer, dimension(:,:), allocatable :: result
        integer, dimension(:),   allocatable :: list
        integer, dimension(:),   allocatable :: minlist

        integer minimal
        integer upper

        integer threads
        integer process

        integer(kind = selected_int_kind(10)) :: total
        integer(kind = selected_int_kind(10)) :: begin
        integer(kind = selected_int_kind(10)) :: end
        integer(kind = selected_int_kind(10)) :: step
        integer(kind = selected_int_kind(10)) :: index

        !$omp parallel default(private), shared(data, result)
        threads = omp_get_num_threads()
        process = omp_get_thread_num()

        call ConstructList(data, list)
        allocate(minlist(size(list)))

        !$omp master
        allocate(result(threads, size(list) + 1))

        if (size(list) >= 24) then
            print '(x)'
            print '(a)', 'The total number of objects is too large.'

            stop
        end if
        !$omp end master
        !$omp barrier

        minlist = list
        minimal = huge(minimal)

        total = factorial(size(list)) / product(factorial(data(3, 1:size(data, 2))))
        step  = ceiling(real(total) / threads)
        begin = (step * process) + 1
        end   = begin + step - 1

        index = 1
        do
            if (index == begin) then
                list = minlist
                exit
            end if

            if (Permutation(minlist) .eqv. .false.) then
                exit
            end if

            index = index + 1
        end do

        do index = begin, end
            upper = UpperBound(data, list)

            if (upper < minimal) then
                minimal = upper
                minlist = list
            end if

            if (Permutation(list) .eqv. .false.) then
                exit
            end if
        end do

        !$omp critical
        result(process + 1, 1) = minimal
        result(process + 1, 2:(size(list) + 1)) = minlist
        !$omp end critical

        deallocate(list)
        deallocate(minlist)

        !$omp end parallel

        minimal = result(1, 1)
        index   = 1

        do process = 2, size(result, 1)
            if (result(process, 1) < minimal) then
                minimal = result(process, 1)
                index = process
            end if
        end do

        print '(x)'
        print '(i2\)', result(index, 2:)
        print '(x,i)', minimal
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

        if (index <= 1) then
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

    pure elemental function factorial(limit) result(value)
        integer, intent(in) :: limit
        integer(kind = selected_int_kind(10)) :: value

        integer :: index

        value = 1
        do index = 2, limit
           value = index * value
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

program BruteForceParallelMethod

    use IO
    use BruteForceParallel

    implicit none

    integer, dimension(:,:), allocatable :: data

    real start
    real finish

    call cpu_time(start)

    call ReadData('data.csv', data)
    call BruteForceParallelSolution(data)

    call cpu_time(finish)
    print '(a,f12.3,a)', 'Computation time:', finish - start, "s."

    deallocate(data)
end
