module List

    implicit none
    public

contains
    subroutine ListSolution(schedule, capacity)
        integer, dimension(:,:), intent(in) :: schedule
        integer, intent(in) :: capacity

        integer, dimension(:,:), allocatable :: incoming
        integer, dimension(:,:), allocatable :: outgoing

        integer, dimension(:), allocatable :: insolution
        integer, dimension(:), allocatable :: outsolution
        integer, dimension(:), allocatable :: solution

        integer currentCapacity

        integer object
        integer index
        integer ondex

        incoming = schedule(pack([(index, index = 1, size(schedule, 1))], &
                                 schedule(:, 4) >= 0), &
                            1:size(schedule, 2))
        outgoing = schedule(pack([(index, index = 1, size(schedule, 1))], &
                                 schedule(:, 4) < 0), &
                            1:size(schedule, 2))

        allocate(insolution(size(incoming, 1)))
        allocate(outsolution(size(outgoing, 1)))
        allocate(solution(size(incoming, 1) + size(outgoing, 1)))

        call ScheduleList(incoming, insolution)
        call ScheduleList(outgoing, outsolution)

        currentCapacity = 0
        object = 1
        index  = 1
        ondex  = 1

        do while ((index <= size(insolution)) .and. (ondex <= size(outsolution)))
            if (currentCapacity >= abs(outgoing(outsolution(ondex), 4))) then
                currentCapacity  = currentCapacity + outgoing(outsolution(ondex), 4)
                solution(object) = outgoing(outsolution(ondex), 5)
                ondex = ondex + 1
            else              
                currentCapacity  = currentCapacity + incoming(insolution(index), 4)
                solution(object) = incoming(insolution(index), 5)
                index = index + 1
            end if

            object = object + 1
        end do

        if (index <= size(insolution)) then
            solution(object:) = incoming(insolution(index:), 5)
        end if

        if (ondex <= size(outsolution)) then
            solution(object:) = outgoing(outsolution(ondex:), 5)
        end if

        print '(x)'
        print '(i3\)', solution
        print '(x,i)', GetServicingPenalty(schedule, solution, capacity)
        
        deallocate(solution)
        deallocate(insolution)
        deallocate(outsolution)
        deallocate(incoming)
        deallocate(outgoing)
    end subroutine

    subroutine ScheduleList(schedule, solution)
        integer, dimension(:,:), intent(in)  :: schedule
        integer, dimension(:),   intent(out) :: solution

        logical, dimension(:), allocatable :: available

        integer job
        integer index
        integer time     ! the moment when the object arrives
        integer duration ! the duration of servicing

        allocate(available(size(schedule, 1)))
        
        available = .true.
        index     = 1

        do while (any(available))
            ! job with largest release date
            job      = maxloc(schedule(1:,1), dim = 1, mask = available)
            time     = schedule(job, 1)
            duration = sum(schedule(:,2), mask = available)

            if (time <= duration) then
                ! job with lowest penalty / duration value
                job = minloc(real(schedule(1:, 3)) / schedule(1:, 2), dim = 1, mask = available)
            end if

            available(job)  = .false.
            solution(index) = job
            index = index + 1
        end do

        solution(1:) = solution(size(solution):1:-1)
        
        deallocate(available)
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
        allocate(schedule(rows - 1, 5))

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
                schedule(row, 5) = row

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

program ListMethod

    use IO
    use List

    implicit none

    integer, dimension(:,:), allocatable :: schedule
    integer capacity

    real start
    real finish

    call cpu_time(start)

    call ReadData('data.csv', schedule, capacity)
    call ListSolution(schedule, capacity)

    call cpu_time(finish)
    print '(a,f12.3,a)', 'Computation time:', finish - start, "s."

    deallocate(schedule)
end
