module lazy_segment_tree_m
  use, intrinsic :: iso_fortran_env
  implicit none
  private
  public :: segment_tree
  public :: monoid_op
  public :: plus_int32_op, min_int32_op, max_int32_op
  type :: lazy_segment_tree
     private
     integer(int32) :: arr_size_, tree_size_, depth_
     integer(int32), allocatable :: arr_(:)
     class(monoid_op), allocatable :: monoid
   contains
     procedure, pass :: init   => init_segment_tree
     procedure, pass :: dump   => dump_segment_tree
     procedure, pass :: update => update_segment_tree
     procedure, pass :: query => query_segment_tree
  end type segment_tree
  type, abstract :: monoid_op
     private
   contains
     procedure(identity_int32), nopass, deferred :: identity
     procedure(bin_op_int32)  , nopass, deferred :: bin_op
  end type monoid_op
  abstract interface
     pure integer(int32) function identity_int32() result(res)
       import int32
     end function identity_int32
     pure integer(int32) function bin_op_int32(x, y) result(res)
       import int32
       integer(int32), intent(in) :: x, y
     end function bin_op_int32
  end interface

  type, extends(monoid_op) :: plus_int32_op
     private
   contains
     procedure, nopass :: identity => identity_plus_int32_op
     procedure, nopass :: bin_op   => bin_op_plus_int32_op
  end type plus_int32_op
  type, extends(monoid_op) :: min_int32_op
     private
   contains
     procedure, nopass :: identity => identity_min_int32_op
     procedure, nopass :: bin_op   => bin_op_min_int32_op
  end type min_int32_op
  type, extends(monoid_op) :: max_int32_op
     private
   contains
     procedure, nopass :: identity => identity_max_int32_op
     procedure, nopass :: bin_op   => bin_op_max_int32_op
  end type max_int32_op
contains
  !> indices rage [1:2^a-1]
  subroutine init_segment_tree (this, arr_size, monoid)
    class(segment_tree), intent(inout) :: this
    integer(int32), intent(in) :: arr_size
    class(monoid_op), intent(in) :: monoid
    integer(int32) :: tree_size
    allocate(this%monoid, source = monoid)
    tree_size = 1
    this%depth_ = 1
    do
       if (tree_size > arr_size) exit
       tree_size = tree_size * 2
       this%depth_ = this%depth_ + 1
    end do
    this%arr_size_  = arr_size
    this%tree_size_ = tree_size
    allocate(this%arr_(2*tree_size - 1), source = this%monoid%identity())
  end subroutine init_segment_tree

  subroutine update_segment_tree (this, i, val)
    class(segment_tree), intent(inout) :: this
    integer(int32), intent(in) :: i, val
    integer(int32) :: x
    x = this%tree_size_ + i - 1
    this%arr_(x) = val
    do while (x > 1)
       x = x/2
       this%arr_(x) = this%monoid%bin_op(this%arr_(2*x), this%arr_(2*x+1))
    end do
  end subroutine update_segment_tree

  ! 閉区間[a, b]で操作.
  pure integer(int32) function query_segment_tree (this, a, b) result(query)
    class(segment_tree), intent(in) :: this
    integer(int32), intent(in) :: a, b
    integer(int32) :: p, r
    query = this%monoid%identity()
    if (a > b) return
    p = this%tree_size_ + a - 1
    r = this%tree_size_ + b - 1
    do while (p <= r)
       if (iand(p, b'1') == 1) then !> right child.
          query = this%monoid%bin_op(query, this%arr_(p))
          p = p + 1
       end if
       if (iand(r, b'1') == 0) then !> left child.
          query = this%monoid%bin_op(query, this%arr_(r))
          r = r - 1
       end if
       p = p/2
       r = r/2
    end do
  end function query_segment_tree

  subroutine dump_segment_tree(this)
    class(segment_tree), intent(in) :: this
    integer(int32) :: i
    do i = 1, this%depth_
       write(error_unit, '(*(i0, 1x))') i, this%arr_(2**(i-1):2**i-1)
    end do
  end subroutine dump_segment_tree

  pure integer(int32) function identity_plus_int32_op() result(res)
    res = 0_int32
  end function identity_plus_int32_op
  pure integer(int32) function bin_op_plus_int32_op(x, y) result(res)
    integer(int32), intent(in) :: x, y
    res = x + y
  end function bin_op_plus_int32_op
  pure integer(int32) function identity_min_int32_op() result(res)
    res = huge(0_int32)
  end function identity_min_int32_op
  pure integer(int32) function bin_op_min_int32_op(x, y) result(res)
    integer(int32), intent(in) :: x, y
    res = min(x, y)
  end function bin_op_min_int32_op
  pure integer(int32) function identity_max_int32_op() result(res)
    res = -huge(0_int32)-1
  end function identity_max_int32_op
  pure integer(int32) function bin_op_max_int32_op(x, y) result(res)
    integer(int32), intent(in) :: x, y
    res = max(x, y)
  end function bin_op_max_int32_op
end module lazy_segment_tree_m

program test_segree
  use, intrinsic :: iso_fortran_env
  use lazy_segment_tree_m
  implicit none
  integer(int32) :: i
  integer(int32) :: arr(8) = [100, 2, 30, 4, 500, 6, 7, -1]
  type(lazy_segment_tree) :: st
  class(monoid_op), allocatable :: add_int32, min_int32
  allocate(min_int32, source = min_int32_op())
  call st%init(size(arr), min_int32)
  do i = 1, size(arr)
     call st%dump()
     call st%update(i, arr(i))
  end do
  call st%dump()
  write(*,'("query [",i0,",",i0,"]: ",i0)') 1, 3, st%query(1, 3)
  write(*,'("query [",i0,",",i0,"]: ",i0)') 5, 7, st%query(5, 7)
  write(*,'("query [",i0,",",i0,"]: ",i0)') 4, 4, st%query(4, 4)
  write(*,'("query [",i0,",",i0,"]: ",i0)') 4, 1, st%query(4, 1)
end program test_segree
