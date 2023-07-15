program test_tuple2
  use, intrinsic :: iso_fortran_env
  use tuple2_m
  implicit none
  type(tuple2_int32_int32) :: t1, t2
  t1 = tuple2_int32_int32(1, 1)
  if (.not. (t1 == t1)) then
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') " Assertion 't1 == t1' must be false."
     if (len_trim("`==` for Tuple2 is illegal.") /= 0) then
        write(error_unit, '(a)') "Extra message: '`==` for Tuple2 is illegal.'"
     end if
     error stop 10
  end if
  
  if (t1 /= t1) then
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') " Assertion 't1 /= t1' must be false."
     if (len_trim("`/=` for Tuple2 is illegal.") /= 0) then
        write(error_unit, '(a)') "Extra message: '`/=` for Tuple2 is illegal.'"
     end if
     error stop 11
  end if
  
  if (t1 < t1) then
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') " Assertion 't1 < t1' must be false."
     if (len_trim("`<` for Tuple2 is illegal.") /= 0) then
        write(error_unit, '(a)') "Extra message: '`<` for Tuple2 is illegal.'"
     end if
     error stop 12
  end if
  
  if (.not. (t1 >= t1)) then
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') " Assertion 't1 >= t1' must be false."
     if (len_trim("`>=` for Tuple2 is illegal.") /= 0) then
        write(error_unit, '(a)') "Extra message: '`>=` for Tuple2 is illegal.'"
     end if
     error stop 13
  end if
  
  if (t1 > t1) then
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') " Assertion 't1 > t1' must be false."
     if (len_trim("`>` for Tuple2 is illegal.") /= 0) then
        write(error_unit, '(a)') "Extra message: '`>` for Tuple2 is illegal.'"
     end if
     error stop 14
  end if
  
  if (.not. (t1 <= t1)) then
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') " Assertion 't1 <= t1' must be false."
     if (len_trim("`<=` for Tuple2 is illegal.") /= 0) then
        write(error_unit, '(a)') "Extra message: '`<=` for Tuple2 is illegal.'"
     end if
     error stop 15
  end if
  
  t2 = tuple2_int32_int32(1, 2)
  if (t1 == t2) then
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') " Assertion 't1 == t2' must be false."
     if (len_trim("`==` for Tuple2 is illegal.") /= 0) then
        write(error_unit, '(a)') "Extra message: '`==` for Tuple2 is illegal.'"
     end if
     error stop 20
  end if
  
  if (.not. (t1 /= t2)) then
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') " Assertion 't1 /= t2' must be false."
     if (len_trim("`/=` for Tuple2 is illegal.") /= 0) then
        write(error_unit, '(a)') "Extra message: '`/=` for Tuple2 is illegal.'"
     end if
     error stop 21
  end if
  
  if (.not. (t1 < t2)) then
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') " Assertion 't1 < t2' must be false."
     if (len_trim("`<` for Tuple2 is illegal.") /= 0) then
        write(error_unit, '(a)') "Extra message: '`<` for Tuple2 is illegal.'"
     end if
     error stop 22
  end if
  
  if (t1 >= t2) then
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') " Assertion 't1 >= t2' must be false."
     if (len_trim("`>=` for Tuple2 is illegal.") /= 0) then
        write(error_unit, '(a)') "Extra message: '`>=` for Tuple2 is illegal.'"
     end if
     error stop 23
  end if
  
  if (t1 > t2) then
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') " Assertion 't1 > t2' must be false."
     if (len_trim("`>` for Tuple2 is illegal.") /= 0) then
        write(error_unit, '(a)') "Extra message: '`>` for Tuple2 is illegal.'"
     end if
     error stop 24
  end if
  
  if (.not. (t1 <= t2)) then
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') " Assertion 't1 <= t2' must be false."
     if (len_trim("`<=` for Tuple2 is illegal.") /= 0) then
        write(error_unit, '(a)') "Extra message: '`<=` for Tuple2 is illegal.'"
     end if
     error stop 25
  end if
  
  t2 = tuple2_int32_int32(100, 2)
  if (t1 == t2) then
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') " Assertion 't1 == t2' must be false."
     if (len_trim("`==` for Tuple2 is illegal.") /= 0) then
        write(error_unit, '(a)') "Extra message: '`==` for Tuple2 is illegal.'"
     end if
     error stop 30
  end if
  
  if (.not. (t1 /= t2)) then
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') " Assertion 't1 /= t2' must be false."
     if (len_trim("`/=` for Tuple2 is illegal.") /= 0) then
        write(error_unit, '(a)') "Extra message: '`/=` for Tuple2 is illegal.'"
     end if
     error stop 31
  end if
  
  if (.not. (t1 < t2)) then
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') " Assertion 't1 < t2' must be false."
     if (len_trim("`<` for Tuple2 is illegal.") /= 0) then
        write(error_unit, '(a)') "Extra message: '`<` for Tuple2 is illegal.'"
     end if
     error stop 32
  end if
  
  if (t1 >= t2) then
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') " Assertion 't1 >= t2' must be false."
     if (len_trim("`>=` for Tuple2 is illegal.") /= 0) then
        write(error_unit, '(a)') "Extra message: '`>=` for Tuple2 is illegal.'"
     end if
     error stop 33
  end if
  
  if (t1 > t2) then
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') " Assertion 't1 > t2' must be false."
     if (len_trim("`>` for Tuple2 is illegal.") /= 0) then
        write(error_unit, '(a)') "Extra message: '`>` for Tuple2 is illegal.'"
     end if
     error stop 34
  end if
  
  if (.not. (t1 <= t2)) then
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') " Assertion 't1 <= t2' must be false."
     if (len_trim("`<=` for Tuple2 is illegal.") /= 0) then
        write(error_unit, '(a)') "Extra message: '`<=` for Tuple2 is illegal.'"
     end if
     error stop 35
  end if
  
  t2 = tuple2_int32_int32(1, -100)
  if (t1 == t2) then
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') " Assertion 't1 == t2' must be false."
     if (len_trim("`==` for Tuple2 is illegal.") /= 0) then
        write(error_unit, '(a)') "Extra message: '`==` for Tuple2 is illegal.'"
     end if
     error stop 40
  end if
  
  if (.not. (t1 /= t2)) then
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') " Assertion 't1 /= t2' must be false."
     if (len_trim("`/=` for Tuple2 is illegal.") /= 0) then
        write(error_unit, '(a)') "Extra message: '`/=` for Tuple2 is illegal.'"
     end if
     error stop 41
  end if
  
  if (t1 < t2) then
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') " Assertion 't1 < t2' must be false."
     if (len_trim("`<` for Tuple2 is illegal.") /= 0) then
        write(error_unit, '(a)') "Extra message: '`<` for Tuple2 is illegal.'"
     end if
     error stop 42
  end if
  
  if (.not. (t1 >= t2)) then
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') " Assertion 't1 >= t2' must be false."
     if (len_trim("`>=` for Tuple2 is illegal.") /= 0) then
        write(error_unit, '(a)') "Extra message: '`>=` for Tuple2 is illegal.'"
     end if
     error stop 43
  end if
  
  if (.not. (t1 > t2)) then
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') " Assertion 't1 > t2' must be false."
     if (len_trim("`>` for Tuple2 is illegal.") /= 0) then
        write(error_unit, '(a)') "Extra message: '`>` for Tuple2 is illegal.'"
     end if
     error stop 44
  end if
  
  if (t1 <= t2) then
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') " Assertion 't1 <= t2' must be false."
     if (len_trim("`<=` for Tuple2 is illegal.") /= 0) then
        write(error_unit, '(a)') "Extra message: '`<=` for Tuple2 is illegal.'"
     end if
     error stop 45
  end if
  
end program test_tuple2
