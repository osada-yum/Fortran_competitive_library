! [[file:../Implementations.org::comparable-test][comparable-test]]
program test_comparable
  use, intrinsic :: iso_fortran_env
  use comparable_m
  use comparable_test_m
  implicit none
  type(comp_int) :: a
  class(comparable), pointer :: b
  a = comp_int(39)
  allocate(b, source = comp_int(42))
  if (.not. (a < b)) then
     write(error_unit, '(a, i0, a)')&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') "Assertion 'a < b' must be true."
     if (len_trim("in main") /= 0) then
        write(error_unit, '(a)') "Extra message: 'in main'"
     end if
     error stop 1
  end if
  
  if (.not. (a <= b)) then
     write(error_unit, '(a, i0, a)')&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') "Assertion 'a <= b' must be true."
     if (len_trim("in main") /= 0) then
        write(error_unit, '(a)') "Extra message: 'in main'"
     end if
     error stop 2
  end if
  
  if (a >= b) then
     write(error_unit, '(a, i0, a)')&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') "Assertion 'a >= b' must be false."
     if (len_trim("in main") /= 0) then
        write(error_unit, '(a)') "Extra message: 'in main'"
     end if
     error stop 3
  end if
  
  if (a > b) then
     write(error_unit, '(a, i0, a)')&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') "Assertion 'a > b' must be false."
     if (len_trim("in main") /= 0) then
        write(error_unit, '(a)') "Extra message: 'in main'"
     end if
     error stop 4
  end if
  
  if (.not. (a < b)) then
     write(error_unit, '(a, i0, a)')&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') "Assertion 'a < b' must be true."
     if (len_trim("in main") /= 0) then
        write(error_unit, '(a)') "Extra message: 'in main'"
     end if
     error stop 5
  end if
  
  if (.not. (a <= a)) then
     write(error_unit, '(a, i0, a)')&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') "Assertion 'a <= a' must be true."
     if (len_trim("in main") /= 0) then
        write(error_unit, '(a)') "Extra message: 'in main'"
     end if
     error stop 6
  end if
  
  if (.not. (a == a)) then
     write(error_unit, '(a, i0, a)')&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') "Assertion 'a == a' must be true."
     if (len_trim("in main") /= 0) then
        write(error_unit, '(a)') "Extra message: 'in main'"
     end if
     error stop 7
  end if
  
  if (a /= a) then
     write(error_unit, '(a, i0, a)')&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') "Assertion 'a /= a' must be false."
     if (len_trim("in main") /= 0) then
        write(error_unit, '(a)') "Extra message: 'in main'"
     end if
     error stop 8
  end if
  
  if (b < a) then
     write(error_unit, '(a, i0, a)')&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') "Assertion 'b < a' must be false."
     if (len_trim("in main") /= 0) then
        write(error_unit, '(a)') "Extra message: 'in main'"
     end if
     error stop 9
  end if
  
  if (b <= a) then
     write(error_unit, '(a, i0, a)')&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') "Assertion 'b <= a' must be false."
     if (len_trim("in main") /= 0) then
        write(error_unit, '(a)') "Extra message: 'in main'"
     end if
     error stop 10
  end if
  
  if (.not. (b >= a)) then
     write(error_unit, '(a, i0, a)')&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') "Assertion 'b >= a' must be true."
     if (len_trim("in main") /= 0) then
        write(error_unit, '(a)') "Extra message: 'in main'"
     end if
     error stop 11
  end if
  
  if (.not. (b > a)) then
     write(error_unit, '(a, i0, a)')&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') "Assertion 'b > a' must be true."
     if (len_trim("in main") /= 0) then
        write(error_unit, '(a)') "Extra message: 'in main'"
     end if
     error stop 12
  end if
  
end program test_comparable
! comparable-test ends here
