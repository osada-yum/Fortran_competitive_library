program test_vector
  use, intrinsic :: iso_fortran_env
  use vector_m
  implicit none
  integer(int32) :: i, j
  integer(int32) :: ierr
  integer(int32), parameter :: n = 10, low = 5, high = low+n-1
  type(vector_int32) :: v, v2
  do i = 1, n
     call v%push_back(i)
  end do
  do i = 0, v%size()+1
     j = v%lower_bound(i)
     if (.not. (j == max(1, i))) then
        write(error_unit, '(a, i0, a)', advance = "no")&
             "Error in "//&
             __FILE__&
             //":", __LINE__, ":"
        write(error_unit, '(a)') " Assertion 'j == max(1, i)' must be true."
        if (len_trim("Return value of `lower_bound` is illegal in loop.") /= 0) then
           write(error_unit, '(a)') "Extra message: 'Return value of `lower_bound` is illegal in loop.'"
        end if
        error stop 11
     end if
     
  end do
  do i = 1, n
     if (.not. (v%at(i) == i)) then
        write(error_unit, '(a, i0, a)', advance = "no")&
             "Error in "//&
             __FILE__&
             //":", __LINE__, ":"
        write(error_unit, '(a)') " Assertion 'v%at(i) == i' must be true."
        if (len_trim("") /= 0) then
           write(error_unit, '(a)') "Extra message: ''"
        end if
        error stop 12
     end if
     
     call v%replace(i, -i)
     if (.not. (v%at(i) == -i)) then
        write(error_unit, '(a, i0, a)', advance = "no")&
             "Error in "//&
             __FILE__&
             //":", __LINE__, ":"
        write(error_unit, '(a)') " Assertion 'v%at(i) == -i' must be true."
        if (len_trim("") /= 0) then
           write(error_unit, '(a)') "Extra message: ''"
        end if
        error stop 13
     end if
     
  end do
  j = v%at(n+1, ierr)
  if (ierr == 0) then
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') " Assertion 'ierr == 0' must be false."
     if (len_trim("Return value of `at` is illegal.") /= 0) then
        write(error_unit, '(a)') "Extra message: 'Return value of `at` is illegal.'"
     end if
     error stop 14
  end if
  
  do i = 1, n
     j = v%pop_back()
  end do
  j = v%pop_back(ierr)
  if (ierr == 0) then
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') " Assertion 'ierr == 0' must be false."
     if (len_trim("Return value of `pop_back` is illegal.") /= 0) then
        write(error_unit, '(a)') "Extra message: 'Return value of `pop_back` is illegal.'"
     end if
     error stop 15
  end if
  

  call v2%init(low, high)
  do i = low, high
     call v2%push_back(i)
  end do
  do i = low-1, high+1
     j = v2%lower_bound(i)
     if (.not. (j == max(low, i))) then
        write(error_unit, '(a, i0, a)', advance = "no")&
             "Error in "//&
             __FILE__&
             //":", __LINE__, ":"
        write(error_unit, '(a)') " Assertion 'j == max(low, i)' must be true."
        if (len_trim("Return value of `lower_bound` is illegal in loop.") /= 0) then
           write(error_unit, '(a)') "Extra message: 'Return value of `lower_bound` is illegal in loop.'"
        end if
        error stop 21
     end if
     
  end do
  do i = low, high
     if (.not. (v2%at(i) == i)) then
        write(error_unit, '(a, i0, a)', advance = "no")&
             "Error in "//&
             __FILE__&
             //":", __LINE__, ":"
        write(error_unit, '(a)') " Assertion 'v2%at(i) == i' must be true."
        if (len_trim("") /= 0) then
           write(error_unit, '(a)') "Extra message: ''"
        end if
        error stop 22
     end if
     
     call v2%replace(i, -i)
     if (.not. (v2%at(i) == -i)) then
        write(error_unit, '(a, i0, a)', advance = "no")&
             "Error in "//&
             __FILE__&
             //":", __LINE__, ":"
        write(error_unit, '(a)') " Assertion 'v2%at(i) == -i' must be true."
        if (len_trim("") /= 0) then
           write(error_unit, '(a)') "Extra message: ''"
        end if
        error stop 23
     end if
     
  end do
  j = v2%at(high+1, ierr)
  if (ierr == 0) then
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') " Assertion 'ierr == 0' must be false."
     if (len_trim("Return value of `at` is illegal.") /= 0) then
        write(error_unit, '(a)') "Extra message: 'Return value of `at` is illegal.'"
     end if
     error stop 24
  end if
  
  do i = 1, n
     j = v2%pop_back()
  end do
  j = v2%pop_back(ierr)
  if (ierr == 0) then
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') " Assertion 'ierr == 0' must be false."
     if (len_trim("Return value of `pop_back` is illegal.") /= 0) then
        write(error_unit, '(a)') "Extra message: 'Return value of `pop_back` is illegal.'"
     end if
     error stop 25
  end if
  
end program test_vector
