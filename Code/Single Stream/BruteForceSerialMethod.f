module BruteForce

    implicit none
    public

contains
    subroutine BruteForceSolution(schedule, capacity)
        integer, dimension(:,:), intent(in) :: schedule
        integer, intent(in) :: capacity

        integer, dimension(:), allocatable :: sequence
        integer, dimension(:), allocatable :: minsequence

        integer penalty
        integer minimal

        call ConstructList(schedule, sequence)
        allocate(minsequence(size(sequence)))

        minsequence = 0
        minimal     = huge(minimal)

        do
            penalty = GetServicingPenalty(schedule, sequence, capacity)
            !print '(i3\)', sequence
            !print '(x,i)', penalty
            !exit
            if ((penalty < minimal) .and. (penalty /= -1)) then
                minimal     = penalty
                minsequence = sequence
            end if

            if (Permutation(sequence) .eqv. .false.) then
                exit
            end if
        end do

        print '(x)'
        print '(i3\)', minsequence
        print '(x,i)', minimal
        
        deallocate(sequence)
        deallocate(minsequence)
    end subroutine

    function GetServicingPenalty(schedule, sequence, capacity) result(penalty)
        integer, dimension(:,:), intent(in) :: schedule
        integer, dimension(:),   intent(in) :: sequence
        integer, intent(in) :: capacity

        integer object
        integer currentCapacity
        integer currentTime
        integer spentTime
        integer penalty

        integer t   ! the moment when the object arrives
        integer tau ! the duration of servicing
        integer a   ! the penalty per unit of time
        integer v   ! the volume of material in the object

        currentCapacity = 0
        currentTime     = schedule(sequence(1), 1)
        spentTime       = 0
        penalty         = 0
        tau             = 0

        do object = 1, size(sequence)
            currentTime = currentTime + tau

            t   = schedule(sequence(object), 1)
            tau = schedule(sequence(object), 2)
            a   = schedule(sequence(object), 3)
            v   = schedule(sequence(object), 4)

            if (currentTime < t) then
                currentTime = t
            end if

            currentCapacity = currentCapacity + v
            spentTime       = currentTime - t + tau
            penalty         = penalty + (spentTime * a)

            if ((currentCapacity < 0) .or. (currentCapacity > capacity)) then
                penalty = -1
                return
            end if
        end do
    end function

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

        integer index

        allocate(list(size(data, 1)))

        do index = 1, size(data, 1)
            list(index) = index
        end do
    end subroutine
end module

module IO

    implicit none
    public

contains
    subroutine ReadData(file, schedule, capacity)
        character(len=*), intent(in) :: file
        integer, dimension(:,:), allocatable, intent(out) :: schedule
        integer, intent(out) :: capacity

        integer unit
        integer status

        integer t   ! the moment when the object arrives
        integer tau ! the duration of servicing
        integer a   ! the penalty per unit of time
        integer v   ! the volume of material in the object
        integer Vt  ! the total volume of the reservoir

        integer rows
        integer row

        rows = GetNumberOfLines(file)
        allocate(schedule(rows - 1, 4))

        open(newunit = unit, file = file, status = 'old', readonly)

        ! skip data header
        read(unit, *, iostat = status)
        print '(11x,a,9x,a,11x,a,11x,a)', 't', 'tau', 'a', 'v'
        row = 1

        do while ((status == 0) .and. (row <= rows))
            read(unit, '(I,I,I,I,I)', iostat = status) t, tau, a, v, Vt

            if (status == 0) then
                print '(i,i,i,i)', t, tau, a, v

                schedule(row, 1) = t
                schedule(row, 2) = tau
                schedule(row, 3) = a
                schedule(row, 4) = v

                capacity = Vt
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

    integer, dimension(:,:), allocatable :: schedule
    integer capacity

    real start
    real finish

    call cpu_time(start)

    call ReadData('data.csv', schedule, capacity)
    call BruteForceSolution(schedule, capacity)

    call cpu_time(finish)
    print '(a,f12.3,a)', 'Computation time:', finish - start, "s."

    deallocate(schedule)
end