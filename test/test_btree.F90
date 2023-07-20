program test_btree_m
  use, intrinsic :: iso_fortran_env
  use btree_m
  implicit none
  type(btree_int32_to_int32) :: m
  integer(int32), parameter :: n = 3*10**4, print_period = n/10
  call m%init()
  ! write(error_unit, '(a, *(i0, 1x))') "random insertion: s, h: ", m%size(), m%height()
  call check_insertion_random(n)
  ! write(error_unit, '(a, *(i0, 1x))') "ascending insertion: s, h: ", m%size(), m%height()
  call check_insertion_ascending(n)
  ! write(error_unit, '(a, *(i0, 1x))') "descending insertion: s, h: ", m%size(), m%height()
  call check_insertion_descending(n)
  ! write(error_unit, '(a, *(i0, 1x))') "all done: s, h: ", m%size(), m%height()
  call check_insertion_character100()
  ! write(error_unit, '(a)') "Success!"
contains
  subroutine check_insertion_random(n)
    integer(int32), intent(in) :: n
    integer(int32) :: seedsize
    integer(int32), allocatable :: seed(:)
    integer(int32), allocatable :: arr(:)
    logical, allocatable :: used(:)
    integer(int32) :: i
    real(real64) :: r
    allocate(arr(n))
    allocate(used(0:10*n-1), source = .false.)
    call random_seed(size = seedsize)
    allocate(seed(seedsize))
    ! call random_seed(get=seed)
    ! write(output_unit, '(*(i0, 1x))') seed(:)
    ! stop
    ! seed(:) = [1491111790, -1572383, 827114786, -2082456701, 1635664076, -1541841097, -1561155676, -20848911]
    ! seed(:) = [-200678384, 1172132809, 124451245, 104878683, 643972878, 359873178, 1075753119, -200657478, 554946086, 24937149&
    !      , -268657053, -197381607, 216039971, 1432749834, 990875914, -2110703970, 2126530906, 1473942311, 1489084070, -932897027&
    !      , -620123104, 1538555552, 715169866, -426413934, 1522025556, -432716, -882668547, -346073549, -1849722518, 111186990&
    !      , -50097180, 1633091563, 0]
    seed(:) = [(i, i = 1, seedsize)]
    call random_seed(put=seed)
    do i = 1, n
       do
          call random_number(r)
          arr(i) = floor(10*n*r)
          if (.not. used(arr(i))) then
             used(arr(i)) = .true.
             exit
          end if
       end do
       call m%insert(arr(i), i)
    end do
    call m%check_invariant()
    ! write(error_unit, '(a, *(i0, 1x))') "s, h: ", m%size(), m%height()
    do i = 1, n
       if (.not. (m%get(arr(i)) == i)) then
          write(error_unit, '(a, i0, a)', advance = "no")&
               "Error in "//&
               __FILE__&
               //":", __LINE__, ":"
          write(error_unit, '(a)') " Assertion 'm%get(arr(i)) == i' must be false."
          write(error_unit, '(a)', advance = "no") "m%get(arr(i)): "
          write(error_unit, *) m%get(arr(i))
          write(error_unit, '(a)', advance = "no") "i: "
          write(error_unit, *) i
          if (len_trim("Btree method `insert` or `get` are something wrong.") /= 0) then
             write(error_unit, '(a)') "Extra message: 'Btree method `insert` or `get` are something wrong.'"
          end if
          error stop 4
       end if
       
    end do
    do i = 1, n
       call m%remove(arr(i))
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
       error stop 5
    end if
    
  end subroutine check_insertion_random
  subroutine check_insertion_ascending(n)
    integer(int32), intent(in) :: n
    integer(int32) :: i
    do i = 1, n
       call m%insert(i, i)
    end do
    if (.not. (m%size() == n)) then
       write(error_unit, '(a, i0, a)', advance = "no")&
            "Error in "//&
            __FILE__&
            //":", __LINE__, ":"
       write(error_unit, '(a)') " Assertion 'm%size() == n' must be false."
       write(error_unit, '(a)', advance = "no") "m%size(): "
       write(error_unit, *) m%size()
       write(error_unit, '(a)', advance = "no") "n: "
       write(error_unit, *) n
       if (len_trim("Btree method `insert` are something wrong.") /= 0) then
          write(error_unit, '(a)') "Extra message: 'Btree method `insert` are something wrong.'"
       end if
       error stop 11
    end if
    
    call m%check_invariant()
    do i = 1, n
       call m%remove(i)
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
       call m%insert(i, i)
    end do
    call m%check_invariant()
    if (.not. (m%size() == n)) then
       write(error_unit, '(a, i0, a)', advance = "no")&
            "Error in "//&
            __FILE__&
            //":", __LINE__, ":"
       write(error_unit, '(a)') " Assertion 'm%size() == n' must be false."
       write(error_unit, '(a)', advance = "no") "m%size(): "
       write(error_unit, *) m%size()
       write(error_unit, '(a)', advance = "no") "n: "
       write(error_unit, *) n
       if (len_trim("Btree method `insert` are something wrong.") /= 0) then
          write(error_unit, '(a)') "Extra message: 'Btree method `insert` are something wrong.'"
       end if
       error stop 13
    end if
    
    do i = n, 1, -1
       call m%remove(i)
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
    
  end subroutine check_insertion_ascending
  subroutine check_insertion_descending(n)
    integer(int32), intent(in) :: n
    integer(int32) :: i
    do i = n, 1, -1
       call m%insert(i, i)
    end do
    call m%check_invariant()
    if (.not. (m%size() == n)) then
       write(error_unit, '(a, i0, a)', advance = "no")&
            "Error in "//&
            __FILE__&
            //":", __LINE__, ":"
       write(error_unit, '(a)') " Assertion 'm%size() == n' must be false."
       write(error_unit, '(a)', advance = "no") "m%size(): "
       write(error_unit, *) m%size()
       write(error_unit, '(a)', advance = "no") "n: "
       write(error_unit, *) n
       if (len_trim("Btree method `insert` are something wrong.") /= 0) then
          write(error_unit, '(a)') "Extra message: 'Btree method `insert` are something wrong.'"
       end if
       error stop 21
    end if
    
    do i = 1, n
       call m%remove(i)
    end do
    call m%check_invariant()
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
       error stop 22
    end if
    
    do i = n, 1, -1
       call m%insert(i, i)
    end do
    call m%check_invariant()
    if (.not. (m%size() == n)) then
       write(error_unit, '(a, i0, a)', advance = "no")&
            "Error in "//&
            __FILE__&
            //":", __LINE__, ":"
       write(error_unit, '(a)') " Assertion 'm%size() == n' must be false."
       write(error_unit, '(a)', advance = "no") "m%size(): "
       write(error_unit, *) m%size()
       write(error_unit, '(a)', advance = "no") "n: "
       write(error_unit, *) n
       if (len_trim("Btree method `insert` are something wrong.") /= 0) then
          write(error_unit, '(a)') "Extra message: 'Btree method `insert` are something wrong.'"
       end if
       error stop 23
    end if
    
    do i = n, 1, -1
       call m%remove(i)
    end do
    call m%check_invariant()
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
       error stop 24
    end if
    
  end subroutine check_insertion_descending
  subroutine check_insertion_character100()
    type(btree_character100_to_int32) :: m_c100
    character(len=10), parameter :: cs(*) = ["apple     ", "banana    ", "chocolate ", "donuts    ", "egg       ", "chocobanan"]
    character(len=100) :: c
    integer(int32) :: i
    call m_c100%init()
    do i = 1, size(cs)
       c = cs(i)
       call m_c100%insert(c, i)
    end do
    do i = 1, size(cs)
       c = cs(i)
       if (.not. (m_c100%get(c) == i)) then
          write(error_unit, '(a, i0, a)', advance = "no")&
               "Error in "//&
               __FILE__&
               //":", __LINE__, ":"
          write(error_unit, '(a)') " Assertion 'm_c100%get(c) == i' must be false."
          write(error_unit, '(a)', advance = "no") "m_c100%get(c): "
          write(error_unit, *) m_c100%get(c)
          write(error_unit, '(a)', advance = "no") "i: "
          write(error_unit, *) i
          if (len_trim("Btree method `insert` or `get` are something wrong.") /= 0) then
             write(error_unit, '(a)') "Extra message: 'Btree method `insert` or `get` are something wrong.'"
          end if
          error stop 40
       end if
       
    end do
  end subroutine check_insertion_character100
end program test_btree_m
