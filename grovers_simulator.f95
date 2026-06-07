program grover_matmul_simulator
    implicit none
    integer, parameter :: dp = 8
    integer, parameter :: qubits = 8
    integer, parameter :: dim = 2**qubits      
    integer, parameter :: iterations = 12
    
    complex(dp), allocatable :: rho(:,:), tmp(:,:)
    complex(dp), allocatable :: oracle(:,:), diff(:,:)
    
    complex(dp) :: v(dim), s(dim)
    integer :: target_state, i, j, step
    real(dp) :: total_prob
    
    target_state = 42
    
    allocate(rho(dim, dim), tmp(dim, dim))
    allocate(oracle(dim, dim), diff(dim, dim))
    
    s = cmplx(1.0_dp / sqrt(real(dim, dp)), 0.0_dp, kind=dp)
    
    rho = spread(s, dim=2, ncopies=dim) * conjg(spread(s, dim=1, ncopies=dim))
    
    v = (0.0_dp, 0.0_dp)
    v(target_state) = (1.0_dp, 0.0_dp)
    
    oracle = (0.0_dp, 0.0_dp)
    ! Initializing to Identity
    do i = 1, dim
        oracle(i, i) = (1.0_dp, 0.0_dp) 
    end do
    ! Our oracle here: I - 2|v><v| 
    oracle = oracle - 2.0_dp * (spread(v, dim=2, ncopies=dim) * spread(v, dim=1, ncopies=dim))
    
    diff = 2.0_dp * (spread(s, dim=2, ncopies=dim) * spread(s, dim=1, ncopies=dim))
    do i = 1, dim
        diff(i, i) = diff(i, i) - (1.0_dp, 0.0_dp) 
    end do

    print *, "Running Grover's Algorithm using pure MATMUL..."
    
    do step = 1, iterations
        tmp = matmul(oracle, rho)
        rho = matmul(tmp, oracle)
        
        tmp = matmul(diff, rho)
        rho = matmul(tmp, diff)
    end do
    
    print *, "Final Probability of Target State (42):", real(rho(target_state, target_state))
    print *, "Final Probability of an Incorrect State (7):", real(rho(7, 7))
    
    deallocate(rho, tmp, oracle, diff)
end program grover_matmul_simulator
