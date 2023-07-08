program test_unwrapped_vector
  use, intrinsic :: iso_fortran_env
  use unwrapped_vector_m
  implicit none
  integer(int32) :: i, j
  integer(int32) :: ierr
  integer(int32), parameter :: n = 10, low = 5, high = low+n-1
  type(unwrapped_vector_int32) :: v, v2
  store:do i = 1, n
     call v%push_back(i)
     if (.not. (v%arr_(i) == i)) then
        write(error_unit, '(a, i0, a)', advance = "no")&
             "Error in "//&
             __FILE__&
             //":", __LINE__, ":"
        write(error_unit, '(a)') " Assertion 'v%arr_(i) == i' must be false."
        if (len_trim("Stored value in `v%arr_(i)` is illegal in loop.") /= 0) then
           write(error_unit, '(a)') "Extra message: 'Stored value in `v%arr_(i)` is illegal in loop.'"
        end if
        error stop 10
     end if
     
  end do store
  test_lower_bound:do i = 0, v%size()+1
     j = v%lower_bound(i)
     if (.not. (j == max(1, i))) then
        write(error_unit, '(a, i0, a)', advance = "no")&
             "Error in "//&
             __FILE__&
             //":", __LINE__, ":"
        write(error_unit, '(a)') " Assertion 'j == max(1, i)' must be false."
        if (len_trim("Return value of `lower_bound` is illegal in loop.") /= 0) then
           write(error_unit, '(a)') "Extra message: 'Return value of `lower_bound` is illegal in loop.'"
        end if
        error stop 11
     end if
     
  end do test_lower_bound
  do i = 1, n
     j = v%pop_back()
  end do

  v2 = unwrapped_vector_int32(5)
  v2%arr_(:) = 1
  do i = 1, 5
     if (.not. (v2%arr_(i) == 1)) then
        write(error_unit, '(a, i0, a)', advance = "no")&
             "Error in "//&
             __FILE__&
             //":", __LINE__, ":"
        write(error_unit, '(a)') " Assertion 'v2%arr_(i) == 1' must be false."
        if (len_trim("Initialization by size of `v2` is illegal.") /= 0) then
           write(error_unit, '(a)') "Extra message: 'Initialization by size of `v2` is illegal.'"
        end if
        error stop 20
     end if
     
  end do
  v2 = unwrapped_vector_int32([(i, i = 1,5)])
  do i = 1, 5
     if (.not. (v2%arr_(i) == i)) then
        write(error_unit, '(a, i0, a)', advance = "no")&
             "Error in "//&
             __FILE__&
             //":", __LINE__, ":"
        write(error_unit, '(a)') " Assertion 'v2%arr_(i) == i' must be false."
        if (len_trim("Initialization by array of `v2` is illegal.") /= 0) then
           write(error_unit, '(a)') "Extra message: 'Initialization by array of `v2` is illegal.'"
        end if
        error stop 21
     end if
     
  end do
  v2 = unwrapped_vector_int32(size = 5, val = 2)
  do i = 1, 5
     if (.not. (v2%arr_(i) == 2)) then
        write(error_unit, '(a, i0, a)', advance = "no")&
             "Error in "//&
             __FILE__&
             //":", __LINE__, ":"
        write(error_unit, '(a)') " Assertion 'v2%arr_(i) == 2' must be false."
        if (len_trim("Initialization by init_val of `v2` is illegal.") /= 0) then
           write(error_unit, '(a)') "Extra message: 'Initialization by init_val of `v2` is illegal.'"
        end if
        error stop 22
     end if
     
  end do

  call v2%resize(0)
  do i = 1, 5
     call v2%push_back(i)
     if (.not. (v2%back() == i)) then
        write(error_unit, '(a, i0, a)', advance = "no")&
             "Error in "//&
             __FILE__&
             //":", __LINE__, ":"
        write(error_unit, '(a)') " Assertion 'v2%back() == i' must be false."
        if (len_trim("Resize or back for `v2` is illegal.") /= 0) then
           write(error_unit, '(a)') "Extra message: 'Resize or back for `v2` is illegal.'"
        end if
        error stop 23
     end if
     
  end do
end program test_unwrapped_vector
