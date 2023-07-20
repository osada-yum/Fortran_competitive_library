program test_priority_queue
  use, intrinsic :: iso_fortran_env
  use priority_queue_m
  implicit none
  integer(int32), parameter :: n = 20, arr(n) = [10, 1, 11, 2, 12, 3, 13, 4, 14, 5, 15, 6, 16, 7, 17, 8, 18, 9, 19, 20]
  integer(int32) :: i
  type(priority_queue_min_int32) :: pq_min
  type(priority_queue_max_int32) :: pq_max
  if (.not. (pq_min%empty())) then
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') " Assertion 'pq_min%empty()' must be false."
     if (len_trim("pq_min must be empty.") /= 0) then
        write(error_unit, '(a)') "Extra message: 'pq_min must be empty.'"
     end if
     error stop 1
  end if
  
  if (.not. (pq_max%empty())) then
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') " Assertion 'pq_max%empty()' must be false."
     if (len_trim("pq_max must be empty.") /= 0) then
        write(error_unit, '(a)') "Extra message: 'pq_max must be empty.'"
     end if
     error stop 2
  end if
  
  do i = 1, n
     call pq_min%push(arr(i))
     call pq_max%push(arr(i))
  end do
  if (.not. (pq_min%not_empty())) then
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') " Assertion 'pq_min%not_empty()' must be false."
     if (len_trim("pq_min must be not empty.") /= 0) then
        write(error_unit, '(a)') "Extra message: 'pq_min must be not empty.'"
     end if
     error stop 3
  end if
  
  if (.not. (pq_max%not_empty())) then
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') " Assertion 'pq_max%not_empty()' must be false."
     if (len_trim("pq_max must be not empty.") /= 0) then
        write(error_unit, '(a)') "Extra message: 'pq_max must be not empty.'"
     end if
     error stop 4
  end if
  
  if (.not. (n == pq_min%size())) then
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') " Assertion 'n == pq_min%size()' must be false."
     write(error_unit, '(a)', advance = "no") "n: "
     write(error_unit, *) n
     write(error_unit, '(a)', advance = "no") "pq_min%size(): "
     write(error_unit, *) pq_min%size()
     if (len_trim("The size of pq_min is illegal.") /= 0) then
        write(error_unit, '(a)') "Extra message: 'The size of pq_min is illegal.'"
     end if
     error stop 10
  end if
  
  if (.not. (n == pq_max%size())) then
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     write(error_unit, '(a)') " Assertion 'n == pq_max%size()' must be false."
     write(error_unit, '(a)', advance = "no") "n: "
     write(error_unit, *) n
     write(error_unit, '(a)', advance = "no") "pq_max%size(): "
     write(error_unit, *) pq_max%size()
     if (len_trim("The size of pq_max is illegal.") /= 0) then
        write(error_unit, '(a)') "Extra message: 'The size of pq_max is illegal.'"
     end if
     error stop 11
  end if
  
  do i = 1, n
     block
       integer(int32) :: val
       val = pq_min%pop()
       if (.not. (i == val)) then
          write(error_unit, '(a, i0, a)', advance = "no")&
               "Error in "//&
               __FILE__&
               //":", __LINE__, ":"
          write(error_unit, '(a)') " Assertion 'i == val' must be false."
          write(error_unit, '(a)', advance = "no") "i: "
          write(error_unit, *) i
          write(error_unit, '(a)', advance = "no") "val: "
          write(error_unit, *) val
          if (len_trim("The value of pq_min%pop() is illegal.") /= 0) then
             write(error_unit, '(a)') "Extra message: 'The value of pq_min%pop() is illegal.'"
          end if
          error stop 12
       end if
       
       val = pq_max%pop()
       if (.not. (n-i+1 == val)) then
          write(error_unit, '(a, i0, a)', advance = "no")&
               "Error in "//&
               __FILE__&
               //":", __LINE__, ":"
          write(error_unit, '(a)') " Assertion 'n-i+1 == val' must be false."
          write(error_unit, '(a)', advance = "no") "n-i+1: "
          write(error_unit, *) n-i+1
          write(error_unit, '(a)', advance = "no") "val: "
          write(error_unit, *) val
          if (len_trim("The value of pq_max%pop() is illegal.") /= 0) then
             write(error_unit, '(a)') "Extra message: 'The value of pq_max%pop() is illegal.'"
          end if
          error stop 13
       end if
       
     end block
  end do
end program test_priority_queue
