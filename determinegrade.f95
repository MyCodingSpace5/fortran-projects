program grades
    implicit none
    
    ! 1. ALL declarations must be at the absolute top
    integer :: num, i
    integer :: average = 0 
    ! Use 'allocatable' for arrays whose size is decided at runtime
    integer, allocatable, dimension(:) :: assignments
    
    write (*,*) "Enter # of assignments:"
    read (*,*) num
    
    ! Allocate the memory for the array now that we know 'num'
    allocate(assignments(num))
    
    ! 2. Loop from 1 to num (Fortran is 1-indexed)
    do i = 1, num
        write (*,*) "Enter grade for assignment ", i, ":"
        read (*,*) assignments(i)  ! Use the correct array name
    end do
    
    do i = 1, num
        average = average + assignments(i)
    end do
    
    average = average / num
    print *, "Your average is: ", average
    
    select case (average)
        case (90:)
            write (*,*) "Grade is: A"
        case (80:89)
            write (*,*) "Grade is: B"
        case (70:79)
            write (*,*) "Grade is: C"
        case (60:69)
            write (*,*) "Grade is: D"
        case default  ! Added 'case' keyword here
            write (*,*) "Grade is: F"
    end select
    
    ! Clean up your allocated memory at the end
    deallocate(assignments)

end program grades
