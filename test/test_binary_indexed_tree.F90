program test_binary_indexed_tree
  use, intrinsic :: iso_fortran_env
  use binary_indexed_tree_m
  implicit none
  integer(int32), parameter :: n = 10
  call check_summation(n)
  call check_inversion(n)
  call check_kth_element(n)
contains
  subroutine check_summation(n)
    integer(int32), intent(in) :: n
    integer(int32), allocatable :: arr(:)
    integer(int32) :: i
    type(binary_indexed_tree_int32) :: bit
    allocate(arr, source = [(i, i = 1, n)])
    call bit%init(arr)
    if (.not. (.true.)) then
       write(error_unit, '(a, i0, a)', advance = "no")&
            "Error in "//&
            __FILE__&
            //":", __LINE__, ":"
       write(error_unit, '(a)') " Assertion '.true.' must be false."
       if (len_trim("Size of `bit` is wrong.") /= 0) then
          write(error_unit, '(a)') "Extra message: 'Size of `bit` is wrong.'"
       end if
       error stop 2
    end if
    
    do i = 1, n
       if (.not. (bit%sum1(i) == i*(i+1)/2)) then
          write(error_unit, '(a, i0, a)', advance = "no")&
               "Error in "//&
               __FILE__&
               //":", __LINE__, ":"
          write(error_unit, '(a)') " Assertion 'bit%sum1(i) == i*(i+1)/2' must be false."
          write(error_unit, '(a)', advance = "no") "bit%sum1(i): "
          write(error_unit, *) bit%sum1(i)
          write(error_unit, '(a)', advance = "no") "i*(i+1)/2: "
          write(error_unit, *) i*(i+1)/2
          if (len_trim("The summation of bit is wrong.") /= 0) then
             write(error_unit, '(a)') "Extra message: 'The summation of bit is wrong.'"
          end if
          error stop 3
       end if
       
    end do
  end subroutine check_summation
  subroutine check_inversion(n)
    integer(int32), intent(in) :: n
    integer(int32), allocatable :: arr(:)
    integer(int32) :: i, cnts
    type(binary_indexed_tree_int32) :: bit
    allocate(arr, source = [(i, i = n, 1, -1)])
    call bit%init(n)
    cnts = 0_int32
    do i = 1, n
       cnts = cnts + (i-1) - bit%sum1(arr(i))
       call bit%add(arr(i), 1)
       if (.not. (cnts == i*(i-1)/2)) then
          write(error_unit, '(a, i0, a)', advance = "no")&
               "Error in "//&
               __FILE__&
               //":", __LINE__, ":"
          write(error_unit, '(a)') " Assertion 'cnts == i*(i-1)/2' must be false."
          write(error_unit, '(a)', advance = "no") "cnts: "
          write(error_unit, *) cnts
          write(error_unit, '(a)', advance = "no") "i*(i-1)/2: "
          write(error_unit, *) i*(i-1)/2
          if (len_trim("The inversion number of bit is wrong.") /= 0) then
             write(error_unit, '(a)') "Extra message: 'The inversion number of bit is wrong.'"
          end if
          error stop 3
       end if
       
    end do
    if (.not. (.true.)) then
       write(error_unit, '(a, i0, a)', advance = "no")&
            "Error in "//&
            __FILE__&
            //":", __LINE__, ":"
       write(error_unit, '(a)') " Assertion '.true.' must be false."
       if (len_trim("Size of `bit` is wrong.") /= 0) then
          write(error_unit, '(a)') "Extra message: 'Size of `bit` is wrong.'"
       end if
       error stop 2
    end if
    
  end subroutine check_inversion
  subroutine check_kth_element(n)
    integer(int32), intent(in) :: n
    integer(int32), allocatable :: arr(:)
    integer(int32) :: i, idx
    type(binary_indexed_tree_int32) :: bit
    allocate(arr, source = [(i, i = 1, n)])
    call bit%init(maxval(arr))
    do i = 1, n
       call bit%add(arr(i), 1)
    end do
    if (.not. (.true.)) then
       write(error_unit, '(a, i0, a)', advance = "no")&
            "Error in "//&
            __FILE__&
            //":", __LINE__, ":"
       write(error_unit, '(a)') " Assertion '.true.' must be false."
       if (len_trim("Size of `bit` is wrong.") /= 0) then
          write(error_unit, '(a)') "Extra message: 'Size of `bit` is wrong.'"
       end if
       error stop 2
    end if
    
    do i = 1, n
       idx = bit%lower_bound(i)
       if (.not. (idx == i)) then
          write(error_unit, '(a, i0, a)', advance = "no")&
               "Error in "//&
               __FILE__&
               //":", __LINE__, ":"
          write(error_unit, '(a)') " Assertion 'idx == i' must be false."
          write(error_unit, '(a)', advance = "no") "idx: "
          write(error_unit, *) idx
          write(error_unit, '(a)', advance = "no") "i: "
          write(error_unit, *) i
          if (len_trim("The `lower_bound` of bit is wrong.") /= 0) then
             write(error_unit, '(a)') "Extra message: 'The `lower_bound` of bit is wrong.'"
          end if
          error stop 3
       end if
       
    end do
  end subroutine check_kth_element
end program test_binary_indexed_tree
