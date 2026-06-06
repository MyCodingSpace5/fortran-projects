program fibbonaci
        implicit none
        integer :: a1, a2
        integer(kind=8) :: t1, t2, b1
        t1 = 0
        t2 = 1
        a2 = 10000
do a1 = 1, a2
        print *, t1
        b1 = t1 + t2
        t1 = t2
        t2 = b1
end do
end program fibbonaci
