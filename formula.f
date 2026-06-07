program formula
        integer :: x, y
        read (*, *) x, y
        if(y == 0) then
           write (*, *) "Divide by zero"
        else 
            write (*, *) x / y
        end if
end program formula
