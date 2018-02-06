module Analytics

    implicit none
    public

contains
    subroutine AnalyticsSolution(schedule, capacity)
        integer, dimension(:,:), intent(in) :: schedule
        integer, intent(in) :: capacity

        integer, dimension(:,:), allocatable :: sequence

        integer penalty
        integer index
        integer jndex

        allocate(sequence(size(schedule, 1), 2))
        sequence(1:,1) = (/ (index, index = 1, size(schedule, 1)) /)
        
        ! sequence(1:,2) = schedule(1:,1) * schedule(1:,2) * schedule(1:,3)
        ! sequence(1:,2) = schedule(1:,2) * schedule(1:,3)
        ! sequence(1:,2) = schedule(1:,1) * schedule(1:,3)
        ! sequence(1:,2) = schedule(1:,1) ** schedule(1:,2)
        ! sequence(1:,2) = schedule(1:,1)
        sequence(1:,2) = schedule(1:,1) + schedule(1:,2)

        call SortSequence(sequence, 1, size(sequence, 1))
        !sequence(1:,1:) = sequence(size(sequence, 1):1:-1,1:)

        penalty = GetServicingPenalty(schedule, sequence(1:,1), capacity)

        print '(x)'
        print '(i3\)', sequence(1:,1)
        print '(x,i)', penalty
        
        deallocate(sequence)
    end subroutine
    
    recursive subroutine SortSequence(sequence, first, last)
        integer, dimension(:,:), intent(in out) :: sequence
        integer, intent(in) :: first
        integer, intent(in) :: last

        integer index
        integer jndex
        integer pivot
        integer buffer
        
        pivot = sequence((first + last) / 2, 2)
        index = first
        jndex = last
        
        do
            do while (sequence(index, 2) < pivot)
                index = index + 1
            end do
            
            do while (pivot < sequence(jndex, 2))
                jndex = jndex - 1
            end do
            
            if (index >= jndex) then
                exit
            end if
            
            buffer = sequence(index, 1)
            sequence(index, 1) = sequence(jndex, 1)
            sequence(jndex, 1) = buffer
            
            buffer = sequence(index, 2)
            sequence(index, 2) = sequence(jndex, 2)
            sequence(jndex, 2) = buffer
            
            index = index + 1
            jndex = jndex - 1
        end do
        
        if (first < index - 1) then
            call SortSequence(sequence, first, index - 1)
        end if
        
        if (jndex + 1 < last) then
            call SortSequence(sequence, jndex + 1, last)
        end if
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

            if (currentCapacity < 0) then
                currentCapacity = 0
            else if (currentCapacity > capacity) then
                penalty = -1
                return
            end if
        end do
    end function
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

program AnalyticsMethod

    use IO
    use Analytics

    implicit none

    integer, dimension(:,:), allocatable :: schedule
    integer capacity

    real start
    real finish

    call cpu_time(start)

    call ReadData('data.csv', schedule, capacity)
    call AnalyticsSolution(schedule, capacity)

    call cpu_time(finish)
    print '(a,f12.3,a)', 'Computation time:', finish - start, "s."

    deallocate(schedule)
end
