program test_lower_bound
  use, intrinsic :: iso_fortran_env
  use lower_bound_m
  implicit none
  integer(int32) :: i, j
  integer(int32), parameter :: n = 10
  integer(int32) :: arr(n), arr2(1), dup_arr(n), allsame_arr(n)
  do i = 1, n
     arr(i) = i
  end do
  ! arr
  do i = 0, n
     j = lower_bound(arr, i)
     if (.not. (j == max(1, i))) then
        write(error_unit, '(a, i0, a)', advance = "no")&
             "Error in "//&
             __FILE__&
             //":", __LINE__, ":"
        write(error_unit, '(a)') " Assertion 'j == max(1, i)' must be false."
        if (len_trim("`lower_bound` does not work well...") /= 0) then
           write(error_unit, '(a)') "Extra message: '`lower_bound` does not work well...'"
        end if
        error stop 11
     end if
     
  end do
  j = lower_bound(arr, n+1)
  if (.not. (j == size(arr)+1)) then
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') " Assertion 'j == size(arr)+1' must be false."
     if (len_trim("`lower_bound` does not work well...") /= 0) then
        write(error_unit, '(a)') "Extra message: '`lower_bound` does not work well...'"
     end if
     error stop 12
  end if
  
  ! arr2
  arr2(1) = 7
  if (.not. (lower_bound(arr2, 6) == 1)) then
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') " Assertion 'lower_bound(arr2, 6) == 1' must be false."
     if (len_trim("`lower_bound` does not work well for one element array...") /= 0) then
        write(error_unit, '(a)') "Extra message: '`lower_bound` does not work well for one element array...'"
     end if
     error stop 13
  end if
  
  if (.not. (lower_bound(arr2, 7) == 1)) then
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') " Assertion 'lower_bound(arr2, 7) == 1' must be false."
     if (len_trim("`lower_bound` does not work well for one element array...") /= 0) then
        write(error_unit, '(a)') "Extra message: '`lower_bound` does not work well for one element array...'"
     end if
     error stop 14
  end if
  
  if (.not. (lower_bound(arr2, 8) == 2)) then
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') " Assertion 'lower_bound(arr2, 8) == 2' must be false."
     if (len_trim("`lower_bound` does not work well for one element array...") /= 0) then
        write(error_unit, '(a)') "Extra message: '`lower_bound` does not work well for one element array...'"
     end if
     error stop 15
  end if
  
  ! dup_arr
  dup_arr = [1, 1, 2, 3, 3, 3, 3, 5, 5, 5]
  if (.not. (lower_bound(dup_arr, 0) == 1)) then
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') " Assertion 'lower_bound(dup_arr, 0) == 1' must be false."
     if (len_trim("`lower_bound` does not work well for the array that has same values...") /= 0) then
        write(error_unit, '(a)') "Extra message: '`lower_bound` does not work well for the array that has same values...'"
     end if
     error stop 21
  end if
  
  if (.not. (lower_bound(dup_arr, 2) == 3)) then
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') " Assertion 'lower_bound(dup_arr, 2) == 3' must be false."
     if (len_trim("`lower_bound` does not work well for the array that has same values...") /= 0) then
        write(error_unit, '(a)') "Extra message: '`lower_bound` does not work well for the array that has same values...'"
     end if
     error stop 22
  end if
  
  if (.not. (lower_bound(dup_arr, 3) == 4)) then
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') " Assertion 'lower_bound(dup_arr, 3) == 4' must be false."
     if (len_trim("`lower_bound` does not work well for the array that has same values...") /= 0) then
        write(error_unit, '(a)') "Extra message: '`lower_bound` does not work well for the array that has same values...'"
     end if
     error stop 23
  end if
  
  if (.not. (lower_bound(dup_arr, 5) == 8)) then
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') " Assertion 'lower_bound(dup_arr, 5) == 8' must be false."
     if (len_trim("`lower_bound` does not work well for the array that has same values...") /= 0) then
        write(error_unit, '(a)') "Extra message: '`lower_bound` does not work well for the array that has same values...'"
     end if
     error stop 24
  end if
  
  if (.not. (lower_bound(dup_arr, 7) > size(dup_arr))) then
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') " Assertion 'lower_bound(dup_arr, 7) > size(dup_arr)' must be false."
     if (len_trim("`lower_bound` does not work well for the array that has same values...") /= 0) then
        write(error_unit, '(a)') "Extra message: '`lower_bound` does not work well for the array that has same values...'"
     end if
     error stop 25
  end if
  
  ! allsame_arr
  allsame_arr = [(1, i = 1, n)]
  if (.not. (lower_bound(allsame_arr, 0) == 1)) then
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') " Assertion 'lower_bound(allsame_arr, 0) == 1' must be false."
     if (len_trim("`lower_bound` does not work well for the array that has all same values...") /= 0) then
        write(error_unit, '(a)') "Extra message: '`lower_bound` does not work well for the array that has all same values...'"
     end if
     error stop 31
  end if
  
  if (.not. (lower_bound(allsame_arr, 1) == 1)) then
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') " Assertion 'lower_bound(allsame_arr, 1) == 1' must be false."
     if (len_trim("`lower_bound` does not work well for the array that has all same values...") /= 0) then
        write(error_unit, '(a)') "Extra message: '`lower_bound` does not work well for the array that has all same values...'"
     end if
     error stop 32
  end if
  
  if (.not. (lower_bound(allsame_arr, 2) > size(allsame_arr))) then
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') " Assertion 'lower_bound(allsame_arr, 2) > size(allsame_arr)' must be false."
     if (len_trim("`lower_bound` does not work well for the array that has all same values...") /= 0) then
        write(error_unit, '(a)') "Extra message: '`lower_bound` does not work well for the array that has all same values...'"
     end if
     error stop 33
  end if
  
end program test_lower_bound
