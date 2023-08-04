program test_tuple2_btree_m
  use, intrinsic :: iso_fortran_env
  use tuple2_m!, only: t2 => tuple2_int32_int32
  use tuple2_btree_m, only: bt_t2_to_int32 => btree_t2_i32_i32_to_int32, &
       bt_iter_t2_to_int32 => btree_node_iter_t2_i32_i32_to_int32
  implicit none
  integer(int32), parameter :: n = 1000
  call tuple2_btree_test_insertion_ascending(n)
contains
  subroutine tuple2_btree_test_insertion_ascending(n)
    integer(int32), intent(in) :: n
    type(bt_t2_to_int32) :: m
    type(bt_iter_t2_to_int32) :: iter
    type(tuple2_int32_int32) :: t
    integer(int32) :: i, j
    call m%init()
    do i = 1, n
       do j = 1, n
          call m%insert(tuple2_int32_int32(i, j), i+j)
       end do
    end do
    if (.not. (m%size() == int(n, int64)*n)) then
       write(error_unit, '(a, i0, a)', advance = "no")&
            "Error in "//&
            __FILE__&
            //":", __LINE__, ":"
       write(error_unit, '(a)') " Assertion 'm%size() == int(n, int64)*n' must be false."
       write(error_unit, '(a)', advance = "no") "m%size(): "
       write(error_unit, *) m%size()
       write(error_unit, '(a)', advance = "no") "int(n, int64)*n: "
       write(error_unit, *) int(n, int64)*n
       if (len_trim("Btree method `insert` are something wrong.") /= 0) then
          write(error_unit, '(a)') "Extra message: 'Btree method `insert` are something wrong.'"
       end if
       error stop 11
    end if
    
    call m%check_invariant()
    do i = 1, n
       do j = 1, n
          call m%remove(tuple2_int32_int32(i, j))
       end do
    end do
    if (.not. (m%size() == 0)) then
       write(error_unit, '(a, i0, a)', advance = "no")&
            "Error in "//&
            __FILE__&
            //":", __LINE__, ":"
       write(error_unit, '(a)') " Assertion 'm%size() == 0' must be false."
       write(error_unit, '(a)', advance = "no") "m%size(): "
       write(error_unit, *) m%size()
       write(error_unit, '(a)', advance = "no") "0: "
       write(error_unit, *) 0
       if (len_trim("Btree method `remove` are something wrong.") /= 0) then
          write(error_unit, '(a)') "Extra message: 'Btree method `remove` are something wrong.'"
       end if
       error stop 12
    end if
    
    do i = 1, n
       do j = 1, n
          call m%insert(tuple2_int32_int32(i, j), i+j)
       end do
    end do
    iter = m%minimum_iter()
    do while (iter%is_not_end())
       t = iter%key()
       ! write(output_unit, '(*(i0, 1x))') t%fst(), t%snd()
       if (.not. (t%fst()+t%snd() == iter%val())) then
          write(error_unit, '(a, i0, a)', advance = "no")&
               "Error in "//&
               __FILE__&
               //":", __LINE__, ":"
          write(error_unit, '(a)') " Assertion 't%fst()+t%snd() == iter%val()' must be false."
          write(error_unit, '(a)', advance = "no") "t%fst()+t%snd(): "
          write(error_unit, *) t%fst()+t%snd()
          write(error_unit, '(a)', advance = "no") "iter%val(): "
          write(error_unit, *) iter%val()
          if (len_trim("Btree iter is something wrong.") /= 0) then
             write(error_unit, '(a)') "Extra message: 'Btree iter is something wrong.'"
          end if
          error stop 12
       end if
       
       call iter%next()
    end do
    call m%check_invariant()
    if (.not. (m%size() == int(n, int64)*n)) then
       write(error_unit, '(a, i0, a)', advance = "no")&
            "Error in "//&
            __FILE__&
            //":", __LINE__, ":"
       write(error_unit, '(a)') " Assertion 'm%size() == int(n, int64)*n' must be false."
       write(error_unit, '(a)', advance = "no") "m%size(): "
       write(error_unit, *) m%size()
       write(error_unit, '(a)', advance = "no") "int(n, int64)*n: "
       write(error_unit, *) int(n, int64)*n
       if (len_trim("Btree method `insert` are something wrong.") /= 0) then
          write(error_unit, '(a)') "Extra message: 'Btree method `insert` are something wrong.'"
       end if
       error stop 13
    end if
    
    do i = n, 1, -1
       do j = n, 1, -1
          call m%remove(tuple2_int32_int32(i, j))
       end do
    end do
    if (.not. (m%size() == 0)) then
       write(error_unit, '(a, i0, a)', advance = "no")&
            "Error in "//&
            __FILE__&
            //":", __LINE__, ":"
       write(error_unit, '(a)') " Assertion 'm%size() == 0' must be false."
       write(error_unit, '(a)', advance = "no") "m%size(): "
       write(error_unit, *) m%size()
       write(error_unit, '(a)', advance = "no") "0: "
       write(error_unit, *) 0
       if (len_trim("Btree method `remove` are something wrong.") /= 0) then
          write(error_unit, '(a)') "Extra message: 'Btree method `remove` are something wrong.'"
       end if
       error stop 14
    end if
    
  end subroutine tuple2_btree_test_insertion_ascending
end program test_tuple2_btree_m
