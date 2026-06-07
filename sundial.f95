program sundial
        integer :: n
        real, parameter :: pi=3.14159
        real :: b
        real :: e
        b = (2 * pi  * (real(n) - 81))/365
        e = 9.87 * sin(2 * b) - 7.53 * cos(b) - 1.5 * sin(b)
        write (*,*) e
        write (*,*) b
end program sundial
