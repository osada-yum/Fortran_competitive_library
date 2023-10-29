module lazy_segment_tree_m
  use, intrinsic :: iso_fortran_env
  implicit none
  private
  public :: lazy_segment_tree_int32
  public :: monoid_op_int32
  type :: lazy_segment_tree_int32
     private
     integer(int32) :: num_elems_, arr_size_, tree_size_, depth_, idx_range_left_, idx_range_right_
     integer(int32), allocatable :: arr_(:)
     , allocatable :: lazy_(:)
     logical, allocatable :: is_lazy_(:)
     class(monoid_op_int32), allocatable :: monoid_
   contains
     procedure, pass :: init_lazy_segment_tree_int32
     procedure, pass :: init_by_arr_lazy_segment_tree_int32
     generic :: init => &
          init_lazy_segment_tree_int32, &
          init_by_arr_lazy_segment_tree_int32
     procedure, pass :: dump => &
          dump_lazy_segment_tree_int32
     procedure, pass :: unsafe_set => &
          unsafe_set_lazy_segment_tree_int32
     procedure, pass :: unsafe_bottomup_update => &
          unsafe_bottomup_update_lazy_segment_tree_int32
     procedure, pass :: strict_propagate_all => &
          strict_propagate_all_lazy_segment_tree_int32
     procedure, pass, private :: strict_propagate_all_sub => &
          strict_propagate_all_sub_lazy_segment_tree_int32
     procedure, pass :: update => &
          update_lazy_segment_tree_int32
     procedure, pass, private :: update_sub => &
          update_sub_lazy_segment_tree_int32
     procedure, pass :: query => &
          query_lazy_segment_tree_int32
     procedure, pass, private :: eval_and_propagate => &
          eval_and_propagate_lazy_segment_tree_int32
  end type lazy_segment_tree_int32
  type, abstract :: monoid_op_int32
     private
   contains
     procedure(identity_int32) , nopass, deferred :: identity
     procedure(bin_op_int32)   , nopass, deferred :: bin_op
     procedure(mapping_int32)  , nopass, deferred :: mapping
     procedure(composite_int32), nopass, deferred :: composite
  end type monoid_op_int32
  abstract interface
     pure integer(int32) function identity_int32() result(res)
       import int32
     end function identity_int32
     pure integer(int32) function bin_op_int32(x, y) result(res)
       import int32
       integer(int32), intent(in) :: x, y
     end function bin_op_int32
     pure integer(int32) function mapping_int32(v, c, length) result(res)
       import int32
       integer(int32), intent(in) :: v
       , intent(in) :: c
       integer(4), intent(in) :: length
     end function mapping_int32
     pure integer(int32) function composite_int32(c_first, c_second) result(res)
       import int32
       , intent(in) :: c_first, c_second
     end function composite_int32
  end interface
  
  public :: sum_assign_int32_op, sum_add_int32_op, sum_mul_int32_op
  public :: min_assign_int32_op, min_add_int32_op
  public :: max_assign_int32_op, max_add_int32_op
  type, extends(monoid_op_int32) :: sum_assign_int32_op
     private
   contains
     procedure, nopass :: identity => identity_sum_int32_op
     procedure, nopass :: bin_op   => bin_op_sum_int32_op
     procedure, nopass :: mapping   => mapping_sum_assign_int32_op
     procedure, nopass :: composite => composite_sum_assign_int32_op
  end type sum_assign_int32_op
  type, extends(monoid_op_int32) :: sum_add_int32_op
     private
   contains
     procedure, nopass :: identity => identity_sum_int32_op
     procedure, nopass :: bin_op   => bin_op_sum_int32_op
     procedure, nopass :: mapping   => mapping_sum_add_int32_op
     procedure, nopass :: composite => composite_sum_add_int32_op
  end type sum_add_int32_op
  type, extends(monoid_op_int32) :: sum_mul_int32_op
     private
   contains
     procedure, nopass :: identity => identity_sum_int32_op
     procedure, nopass :: bin_op   => bin_op_sum_int32_op
     procedure, nopass :: mapping   => mapping_sum_mul_int32_op
     procedure, nopass :: composite => composite_sum_mul_int32_op
  end type sum_mul_int32_op
  
  type, extends(monoid_op_int32) :: min_assign_int32_op
     private
   contains
     procedure, nopass :: identity => identity_min_int32_op
     procedure, nopass :: bin_op   => bin_op_min_int32_op
     procedure, nopass :: mapping   => mapping_min_assign_int32_op
     procedure, nopass :: composite => composite_min_assign_int32_op
  end type min_assign_int32_op
  type, extends(monoid_op_int32) :: min_add_int32_op
     private
   contains
     procedure, nopass :: identity => identity_min_int32_op
     procedure, nopass :: bin_op   => bin_op_min_int32_op
     procedure, nopass :: mapping   => mapping_min_add_int32_op
     procedure, nopass :: composite => composite_min_add_int32_op
  end type min_add_int32_op
  
  type, extends(monoid_op_int32) :: max_assign_int32_op
     private
   contains
     procedure, nopass :: identity => identity_max_int32_op
     procedure, nopass :: bin_op   => bin_op_max_int32_op
     procedure, nopass :: mapping   => mapping_max_assign_int32_op
     procedure, nopass :: composite => composite_max_assign_int32_op
  end type max_assign_int32_op
  type, extends(monoid_op_int32) :: max_add_int32_op
     private
   contains
     procedure, nopass :: identity => identity_max_int32_op
     procedure, nopass :: bin_op   => bin_op_max_int32_op
     procedure, nopass :: mapping   => mapping_max_add_int32_op
     procedure, nopass :: composite => composite_max_add_int32_op
  end type max_add_int32_op
  
  public :: lazy_segment_tree_int64
  public :: monoid_op_int64
  type :: lazy_segment_tree_int64
     private
     integer(int32) :: num_elems_, arr_size_, tree_size_, depth_, idx_range_left_, idx_range_right_
     integer(int64), allocatable :: arr_(:)
     , allocatable :: lazy_(:)
     logical, allocatable :: is_lazy_(:)
     class(monoid_op_int64), allocatable :: monoid_
   contains
     procedure, pass :: init_lazy_segment_tree_int64
     procedure, pass :: init_by_arr_lazy_segment_tree_int64
     generic :: init => &
          init_lazy_segment_tree_int64, &
          init_by_arr_lazy_segment_tree_int64
     procedure, pass :: dump => &
          dump_lazy_segment_tree_int64
     procedure, pass :: unsafe_set => &
          unsafe_set_lazy_segment_tree_int64
     procedure, pass :: unsafe_bottomup_update => &
          unsafe_bottomup_update_lazy_segment_tree_int64
     procedure, pass :: strict_propagate_all => &
          strict_propagate_all_lazy_segment_tree_int64
     procedure, pass, private :: strict_propagate_all_sub => &
          strict_propagate_all_sub_lazy_segment_tree_int64
     procedure, pass :: update => &
          update_lazy_segment_tree_int64
     procedure, pass, private :: update_sub => &
          update_sub_lazy_segment_tree_int64
     procedure, pass :: query => &
          query_lazy_segment_tree_int64
     procedure, pass, private :: eval_and_propagate => &
          eval_and_propagate_lazy_segment_tree_int64
  end type lazy_segment_tree_int64
  type, abstract :: monoid_op_int64
     private
   contains
     procedure(identity_int64) , nopass, deferred :: identity
     procedure(bin_op_int64)   , nopass, deferred :: bin_op
     procedure(mapping_int64)  , nopass, deferred :: mapping
     procedure(composite_int64), nopass, deferred :: composite
  end type monoid_op_int64
  abstract interface
     pure integer(int64) function identity_int64() result(res)
       import int64
     end function identity_int64
     pure integer(int64) function bin_op_int64(x, y) result(res)
       import int64
       integer(int64), intent(in) :: x, y
     end function bin_op_int64
     pure integer(int64) function mapping_int64(v, c, length) result(res)
       import int64
       integer(int64), intent(in) :: v
       , intent(in) :: c
       integer(4), intent(in) :: length
     end function mapping_int64
     pure integer(int64) function composite_int64(c_first, c_second) result(res)
       import int64
       , intent(in) :: c_first, c_second
     end function composite_int64
  end interface
  
  public :: sum_assign_int64_op, sum_add_int64_op, sum_mul_int64_op
  public :: min_assign_int64_op, min_add_int64_op
  public :: max_assign_int64_op, max_add_int64_op
  type, extends(monoid_op_int64) :: sum_assign_int64_op
     private
   contains
     procedure, nopass :: identity => identity_sum_int64_op
     procedure, nopass :: bin_op   => bin_op_sum_int64_op
     procedure, nopass :: mapping   => mapping_sum_assign_int64_op
     procedure, nopass :: composite => composite_sum_assign_int64_op
  end type sum_assign_int64_op
  type, extends(monoid_op_int64) :: sum_add_int64_op
     private
   contains
     procedure, nopass :: identity => identity_sum_int64_op
     procedure, nopass :: bin_op   => bin_op_sum_int64_op
     procedure, nopass :: mapping   => mapping_sum_add_int64_op
     procedure, nopass :: composite => composite_sum_add_int64_op
  end type sum_add_int64_op
  type, extends(monoid_op_int64) :: sum_mul_int64_op
     private
   contains
     procedure, nopass :: identity => identity_sum_int64_op
     procedure, nopass :: bin_op   => bin_op_sum_int64_op
     procedure, nopass :: mapping   => mapping_sum_mul_int64_op
     procedure, nopass :: composite => composite_sum_mul_int64_op
  end type sum_mul_int64_op
  
  type, extends(monoid_op_int64) :: min_assign_int64_op
     private
   contains
     procedure, nopass :: identity => identity_min_int64_op
     procedure, nopass :: bin_op   => bin_op_min_int64_op
     procedure, nopass :: mapping   => mapping_min_assign_int64_op
     procedure, nopass :: composite => composite_min_assign_int64_op
  end type min_assign_int64_op
  type, extends(monoid_op_int64) :: min_add_int64_op
     private
   contains
     procedure, nopass :: identity => identity_min_int64_op
     procedure, nopass :: bin_op   => bin_op_min_int64_op
     procedure, nopass :: mapping   => mapping_min_add_int64_op
     procedure, nopass :: composite => composite_min_add_int64_op
  end type min_add_int64_op
  
  type, extends(monoid_op_int64) :: max_assign_int64_op
     private
   contains
     procedure, nopass :: identity => identity_max_int64_op
     procedure, nopass :: bin_op   => bin_op_max_int64_op
     procedure, nopass :: mapping   => mapping_max_assign_int64_op
     procedure, nopass :: composite => composite_max_assign_int64_op
  end type max_assign_int64_op
  type, extends(monoid_op_int64) :: max_add_int64_op
     private
   contains
     procedure, nopass :: identity => identity_max_int64_op
     procedure, nopass :: bin_op   => bin_op_max_int64_op
     procedure, nopass :: mapping   => mapping_max_add_int64_op
     procedure, nopass :: composite => composite_max_add_int64_op
  end type max_add_int64_op
  
contains
  !> indices rage [1:2^a-1]
  !> init_lazy_segment_tree_int32: Initialize lazy_segment_tree_int32 with  and monoid
  pure subroutine init_lazy_segment_tree_int32(this, num_elems, monoid)
    class(lazy_segment_tree_int32), intent(inout) :: this
    integer(int32), intent(in) :: num_elems
    class(monoid_op_int32), intent(in) :: monoid
    integer(int32) :: tree_size
    allocate(this%monoid_, source = monoid)
    tree_size = 1
    this%depth_ = 1
    do while (tree_size < num_elems)
       tree_size = tree_size * 2
       this%depth_ = this%depth_ + 1
    end do
    this%tree_size_ = tree_size
    this%arr_size_ = 2 * tree_size - 1
    this%num_elems_ = num_elems
    this%idx_range_left_  = 1
    this%idx_range_right_ = tree_size
    allocate(this%arr_(this%arr_size_), source = this%monoid_%identity())
    allocate(this%lazy_(this%arr_size_))
    allocate(this%is_lazy_(this%arr_size_), source = .false.)
  end subroutine init_lazy_segment_tree_int32
  !> init_by_arr_lazy_segment_tree_int32: Initialize lazy segment tree by array.
  pure subroutine init_by_arr_lazy_segment_tree_int32(this, arr, monoid)
    class(lazy_segment_tree_int32), intent(inout) :: this
    integer(int32), intent(in) :: arr(:)
    class(monoid_op_int32), intent(in) :: monoid
    integer(int32) :: i
    call this%init(size(arr), monoid)
    do i = 1, size(arr)
       this%arr_(i + this%tree_size_ - 1) = arr(i)
    end do
    call this%strict_propagate_all()
  end subroutine init_by_arr_lazy_segment_tree_int32
  
  !> unsafe_set_lazy_segment_tree_int32: Set value into the node of leaf of tree.
  pure subroutine unsafe_set_lazy_segment_tree_int32(this, idx, val)
    class(lazy_segment_tree_int32), intent(inout) :: this
    integer(int32), intent(in) :: idx
    integer(int32), intent(in) :: val
    this%arr_(this%tree_size_ + idx - 1) = val
  end subroutine unsafe_set_lazy_segment_tree_int32
  !> unsafe_set_lazy_segment_tree_int32: Update all node.
  pure subroutine unsafe_bottomup_update_lazy_segment_tree_int32(this)
    class(lazy_segment_tree_int32), intent(inout) :: this
    integer(int32) :: i
    do i = this%tree_size_ - 1, 1, -1
       this%arr_(i) = this%monoid_%bin_op(this%arr_(2 * i), this%arr_(2 * i + 1))
    end do
    this%is_lazy_(:) = .false.
  end subroutine unsafe_bottomup_update_lazy_segment_tree_int32
  
  !> strict_propagate_all_lazy_segment_tree_int32: Update all node.
  pure subroutine strict_propagate_all_lazy_segment_tree_int32(this)
    class(lazy_segment_tree_int32), intent(inout) :: this
    call this%strict_propagate_all_sub(1, this%idx_range_left_, this%idx_range_right_)
  end subroutine strict_propagate_all_lazy_segment_tree_int32
  pure recursive subroutine strict_propagate_all_sub_lazy_segment_tree_int32(this, idx, l, r)
    class(lazy_segment_tree_int32), intent(inout) :: this
    integer(int32), intent(in) :: idx, l, r
    call this%eval_and_propagate(idx, r - l + 1)
    if (l == r) return
    call this%strict_propagate_all_sub(2 * idx,                   l, (l + r) / 2)
    call this%strict_propagate_all_sub(2 * idx + 1, (l + r + 1) / 2, r)
    this%arr_(idx) = this%monoid_%bin_op(this%arr_(2 * idx), this%arr_(2 * idx + 1))
  end subroutine strict_propagate_all_sub_lazy_segment_tree_int32
  
  !> update_lazy_segment_tree_int32: Update tree by .
  !> [a, b]: Range to update.
  !> val: Value of update.
  pure subroutine update_lazy_segment_tree_int32(this, a, b, val)
    class(lazy_segment_tree_int32), intent(inout) :: this
    integer(int32), intent(in) :: a, b
    integer(int32), intent(in) :: val
    if (a > b) error stop "Illegal range of a > b."
    call this%update_sub(a, b, val, 1, this%idx_range_left_, this%idx_range_right_)
  end subroutine update_lazy_segment_tree_int32
  !> update_sub_lazy_segment_tree_int32: Update tree by .
  !> [a, b]: Range to update.
  !> val: Value of update.
  !> idx: Index of tree.
  !> [l, r]: Range of current node of tree.
  pure recursive subroutine update_sub_lazy_segment_tree_int32(this, a, b, val, idx, l, r)
    class(lazy_segment_tree_int32), intent(inout) :: this
    integer(int32), intent(in) :: a, b, idx, l, r
    integer(int32), intent(in) :: val
    ! write(error_unit, '(a, *(i0, 1x))') "update_sub: ", a, b, l, r, idx
    if (a <= l .and. r <= b) then !> [a, b] に [l, r]が内包.
       if (this%is_lazy_(idx)) then
          this%lazy_(idx) = this%monoid_%composite(this%lazy_(idx), val)
       else
          this%lazy_(idx) = val
          this%is_lazy_(idx) = .true.
       end if
       call this%eval_and_propagate(idx, r - l + 1)
       return
    end if
    call this%eval_and_propagate(idx, r - l + 1)
    if (r < a .or. b < l) return !> r < [a, b] or [a, b] < l
    !> 一部が [a, b]に入っている.
    call this%update_sub(a, b, val, 2 * idx,                   l, (l + r) / 2)
    call this%update_sub(a, b, val, 2 * idx + 1, (l + r + 1) / 2, r)
    this%arr_(idx) = this%monoid_%bin_op(this%arr_(2 * idx), this%arr_(2 * idx + 1))
  end subroutine update_sub_lazy_segment_tree_int32
  
  ! 閉区間[a, b]で操作.
  impure integer(int32) function query_lazy_segment_tree_int32 (this, a, b) result(query)
    class(lazy_segment_tree_int32), intent(inout) :: this
    integer(int32), intent(in) :: a, b
    if (a > b) error stop "Illegal range of a > b."
    query = query_sub(1, this%idx_range_left_, this%idx_range_right_)
  contains
    impure recursive integer(int32) function query_sub(idx, l, r) result(res)
      integer(int32), intent(in) :: idx, l, r
      integer(int32) :: val_l, val_r
      ! write(error_unit, '(a, *(i0, 1x))') "query_sub: ", a, b, l, r
      call this%eval_and_propagate(idx, r - l + 1)
      res = this%monoid_%identity()
      if (r < a .or. b < l) return !> r < [a, b] or [a, b] < l
      if (a <= l .and. r <= b) then !> [a, b] に [l, r]が内包.
         res = this%arr_(idx)
         return
      else !> l in [a, b] or r in [a, b]
         !> [l, r] -> [l, (l + r) / 2], [(l + r + 1) / 2, r]
         val_l = query_sub(2 * idx,                   l, (l + r) / 2)
         val_r = query_sub(2 * idx + 1, (l + r + 1) / 2, r)
         res = this%monoid_%bin_op(val_l, val_r)
      end if
    end function query_sub
  end function query_lazy_segment_tree_int32
  !> eval_and_propagate_lazy_segment_tree_int32: Treat lazy propagation.
  !> idx: Index of node of tree.
  !> length: Length of range of node.
  pure subroutine eval_and_propagate_lazy_segment_tree_int32(this, idx, length)
    class(lazy_segment_tree_int32), intent(inout) :: this
    integer(int32), intent(in) :: idx, length
    ! write(error_unit, '(a, L, *(1x, i0))') "eval_and_propagate: ", this%is_lazy_(idx) &
    !      , idx, this%arr_(idx), this%lazy_(idx), length, this%monoid_%mapping(this%arr_(idx), this%lazy_(idx), length)
    if (.not. this%is_lazy_(idx)) return
    this%arr_(idx) = this%monoid_%mapping(this%arr_(idx), this%lazy_(idx), length)
    this%is_lazy_(idx) = .false.
    if (2 * idx > this%arr_size_) return
    if (this%is_lazy_(2 * idx)) then
       this%lazy_(2 * idx) = this%monoid_%composite(this%lazy_(2 * idx), this%lazy_(idx))
    else
       this%lazy_(2 * idx) = this%lazy_(idx)
       this%is_lazy_(2 * idx) = .true.
    end if
    if (this%is_lazy_(2 * idx + 1)) then
       this%lazy_(2 * idx + 1) = this%monoid_%composite(this%lazy_(2 * idx + 1), this%lazy_(idx))
    else
       this%lazy_(2 * idx + 1) = this%lazy_(idx)
       this%is_lazy_(2 * idx + 1) = .true.
    end if
  end subroutine eval_and_propagate_lazy_segment_tree_int32
  
  subroutine dump_lazy_segment_tree_int32(this)
    class(lazy_segment_tree_int32), intent(in) :: this
    integer(int32) :: i
    write(error_unit, '(a)') "tree: "
    do i = 1, this%depth_
       write(error_unit, '(g0, ": ", *(g0, 1x))') i, this%arr_(2**(i-1):2**i-1)
    end do
    write(error_unit, '(a)') "lazy tree: "
    do i = 1, this%depth_
       write(error_unit, '(g0, ": ", *(g0, 1x))') i, this%lazy_(2**(i-1):2**i-1)
    end do
    write(error_unit, '(a)') "is_lazy?: "
    do i = 1, this%depth_
       write(error_unit, '(g0, ": ", *(L, 1x))') i, this%is_lazy_(2**(i-1):2**i-1)
    end do
  end subroutine dump_lazy_segment_tree_int32
  
  pure integer(int32) function identity_sum_int32_op() result(res)
    res = 0_int32
  end function identity_sum_int32_op
  pure integer(int32) function bin_op_sum_int32_op(x, y) result(res)
    integer(int32), intent(in) :: x, y
    res = x + y
  end function bin_op_sum_int32_op
  pure integer(int32) function mapping_sum_assign_int32_op(v, c, length) result(res)
    integer(int32), intent(in) :: v, c
    integer(4), intent(in) :: length
    res = c * length
  end function mapping_sum_assign_int32_op
  pure integer(int32) function composite_sum_assign_int32_op(c_first, c_second) result(res)
    integer(int32), intent(in) :: c_first, c_second
    res = c_second
  end function composite_sum_assign_int32_op
  pure integer(int32) function mapping_sum_add_int32_op(v, c, length) result(res)
    integer(int32), intent(in) :: v, c
    integer(4), intent(in) :: length
    res = v + c * length
  end function mapping_sum_add_int32_op
  pure integer(int32) function composite_sum_add_int32_op(c_first, c_second) result(res)
    integer(int32), intent(in) :: c_first, c_second
    res = c_first + c_second
  end function composite_sum_add_int32_op
  pure integer(int32) function mapping_sum_mul_int32_op(v, c, length) result(res)
    integer(int32), intent(in) :: v, c
    integer(4), intent(in) :: length
    res = v * c
  end function mapping_sum_mul_int32_op
  pure integer(int32) function composite_sum_mul_int32_op(c_first, c_second) result(res)
    integer(int32), intent(in) :: c_first, c_second
    res = c_first + c_second
  end function composite_sum_mul_int32_op
  
  pure integer(int32) function identity_min_int32_op() result(res)
    res = huge(0_int32)
  end function identity_min_int32_op
  pure integer(int32) function bin_op_min_int32_op(x, y) result(res)
    integer(int32), intent(in) :: x, y
    res = min(x, y)
  end function bin_op_min_int32_op
  pure integer(int32) function mapping_min_assign_int32_op(v, c, length) result(res)
    integer(int32), intent(in) :: v, c
    integer(4), intent(in) :: length
    res = c
  end function mapping_min_assign_int32_op
  pure integer(int32) function composite_min_assign_int32_op(c_first, c_second) result(res)
    integer(int32), intent(in) :: c_first, c_second
    res = c_second
  end function composite_min_assign_int32_op
  pure integer(int32) function mapping_min_add_int32_op(v, c, length) result(res)
    integer(int32), intent(in) :: v, c
    integer(4), intent(in) :: length
    res = v + c
  end function mapping_min_add_int32_op
  pure integer(int32) function composite_min_add_int32_op(c_first, c_second) result(res)
    integer(int32), intent(in) :: c_first, c_second
    res = c_first + c_second
  end function composite_min_add_int32_op
  
  pure integer(int32) function identity_max_int32_op() result(res)
    res = -huge(0_int32)-1
  end function identity_max_int32_op
  pure integer(int32) function bin_op_max_int32_op(x, y) result(res)
    integer(int32), intent(in) :: x, y
    res = max(x, y)
  end function bin_op_max_int32_op
  pure integer(int32) function mapping_max_assign_int32_op(v, c, length) result(res)
    integer(int32), intent(in) :: v, c
    integer(4), intent(in) :: length
    res = c
  end function mapping_max_assign_int32_op
  pure integer(int32) function composite_max_assign_int32_op(c_first, c_second) result(res)
    integer(int32), intent(in) :: c_first, c_second
    res = c_second
  end function composite_max_assign_int32_op
  pure integer(int32) function mapping_max_add_int32_op(v, c, length) result(res)
    integer(int32), intent(in) :: v, c
    integer(4), intent(in) :: length
    res = v + c
  end function mapping_max_add_int32_op
  pure integer(int32) function composite_max_add_int32_op(c_first, c_second) result(res)
    integer(int32), intent(in) :: c_first, c_second
    res = c_first + c_second
  end function composite_max_add_int32_op
  
  !> indices rage [1:2^a-1]
  !> init_lazy_segment_tree_int64: Initialize lazy_segment_tree_int64 with  and monoid
  pure subroutine init_lazy_segment_tree_int64(this, num_elems, monoid)
    class(lazy_segment_tree_int64), intent(inout) :: this
    integer(int32), intent(in) :: num_elems
    class(monoid_op_int64), intent(in) :: monoid
    integer(int32) :: tree_size
    allocate(this%monoid_, source = monoid)
    tree_size = 1
    this%depth_ = 1
    do while (tree_size < num_elems)
       tree_size = tree_size * 2
       this%depth_ = this%depth_ + 1
    end do
    this%tree_size_ = tree_size
    this%arr_size_ = 2 * tree_size - 1
    this%num_elems_ = num_elems
    this%idx_range_left_  = 1
    this%idx_range_right_ = tree_size
    allocate(this%arr_(this%arr_size_), source = this%monoid_%identity())
    allocate(this%lazy_(this%arr_size_))
    allocate(this%is_lazy_(this%arr_size_), source = .false.)
  end subroutine init_lazy_segment_tree_int64
  !> init_by_arr_lazy_segment_tree_int64: Initialize lazy segment tree by array.
  pure subroutine init_by_arr_lazy_segment_tree_int64(this, arr, monoid)
    class(lazy_segment_tree_int64), intent(inout) :: this
    integer(int64), intent(in) :: arr(:)
    class(monoid_op_int64), intent(in) :: monoid
    integer(int32) :: i
    call this%init(size(arr), monoid)
    do i = 1, size(arr)
       this%arr_(i + this%tree_size_ - 1) = arr(i)
    end do
    call this%strict_propagate_all()
  end subroutine init_by_arr_lazy_segment_tree_int64
  
  !> unsafe_set_lazy_segment_tree_int64: Set value into the node of leaf of tree.
  pure subroutine unsafe_set_lazy_segment_tree_int64(this, idx, val)
    class(lazy_segment_tree_int64), intent(inout) :: this
    integer(int32), intent(in) :: idx
    integer(int64), intent(in) :: val
    this%arr_(this%tree_size_ + idx - 1) = val
  end subroutine unsafe_set_lazy_segment_tree_int64
  !> unsafe_set_lazy_segment_tree_int64: Update all node.
  pure subroutine unsafe_bottomup_update_lazy_segment_tree_int64(this)
    class(lazy_segment_tree_int64), intent(inout) :: this
    integer(int32) :: i
    do i = this%tree_size_ - 1, 1, -1
       this%arr_(i) = this%monoid_%bin_op(this%arr_(2 * i), this%arr_(2 * i + 1))
    end do
    this%is_lazy_(:) = .false.
  end subroutine unsafe_bottomup_update_lazy_segment_tree_int64
  
  !> strict_propagate_all_lazy_segment_tree_int64: Update all node.
  pure subroutine strict_propagate_all_lazy_segment_tree_int64(this)
    class(lazy_segment_tree_int64), intent(inout) :: this
    call this%strict_propagate_all_sub(1, this%idx_range_left_, this%idx_range_right_)
  end subroutine strict_propagate_all_lazy_segment_tree_int64
  pure recursive subroutine strict_propagate_all_sub_lazy_segment_tree_int64(this, idx, l, r)
    class(lazy_segment_tree_int64), intent(inout) :: this
    integer(int32), intent(in) :: idx, l, r
    call this%eval_and_propagate(idx, r - l + 1)
    if (l == r) return
    call this%strict_propagate_all_sub(2 * idx,                   l, (l + r) / 2)
    call this%strict_propagate_all_sub(2 * idx + 1, (l + r + 1) / 2, r)
    this%arr_(idx) = this%monoid_%bin_op(this%arr_(2 * idx), this%arr_(2 * idx + 1))
  end subroutine strict_propagate_all_sub_lazy_segment_tree_int64
  
  !> update_lazy_segment_tree_int64: Update tree by .
  !> [a, b]: Range to update.
  !> val: Value of update.
  pure subroutine update_lazy_segment_tree_int64(this, a, b, val)
    class(lazy_segment_tree_int64), intent(inout) :: this
    integer(int32), intent(in) :: a, b
    integer(int64), intent(in) :: val
    if (a > b) error stop "Illegal range of a > b."
    call this%update_sub(a, b, val, 1, this%idx_range_left_, this%idx_range_right_)
  end subroutine update_lazy_segment_tree_int64
  !> update_sub_lazy_segment_tree_int64: Update tree by .
  !> [a, b]: Range to update.
  !> val: Value of update.
  !> idx: Index of tree.
  !> [l, r]: Range of current node of tree.
  pure recursive subroutine update_sub_lazy_segment_tree_int64(this, a, b, val, idx, l, r)
    class(lazy_segment_tree_int64), intent(inout) :: this
    integer(int32), intent(in) :: a, b, idx, l, r
    integer(int64), intent(in) :: val
    ! write(error_unit, '(a, *(i0, 1x))') "update_sub: ", a, b, l, r, idx
    if (a <= l .and. r <= b) then !> [a, b] に [l, r]が内包.
       if (this%is_lazy_(idx)) then
          this%lazy_(idx) = this%monoid_%composite(this%lazy_(idx), val)
       else
          this%lazy_(idx) = val
          this%is_lazy_(idx) = .true.
       end if
       call this%eval_and_propagate(idx, r - l + 1)
       return
    end if
    call this%eval_and_propagate(idx, r - l + 1)
    if (r < a .or. b < l) return !> r < [a, b] or [a, b] < l
    !> 一部が [a, b]に入っている.
    call this%update_sub(a, b, val, 2 * idx,                   l, (l + r) / 2)
    call this%update_sub(a, b, val, 2 * idx + 1, (l + r + 1) / 2, r)
    this%arr_(idx) = this%monoid_%bin_op(this%arr_(2 * idx), this%arr_(2 * idx + 1))
  end subroutine update_sub_lazy_segment_tree_int64
  
  ! 閉区間[a, b]で操作.
  impure integer(int64) function query_lazy_segment_tree_int64 (this, a, b) result(query)
    class(lazy_segment_tree_int64), intent(inout) :: this
    integer(int32), intent(in) :: a, b
    if (a > b) error stop "Illegal range of a > b."
    query = query_sub(1, this%idx_range_left_, this%idx_range_right_)
  contains
    impure recursive integer(int64) function query_sub(idx, l, r) result(res)
      integer(int32), intent(in) :: idx, l, r
      integer(int64) :: val_l, val_r
      ! write(error_unit, '(a, *(i0, 1x))') "query_sub: ", a, b, l, r
      call this%eval_and_propagate(idx, r - l + 1)
      res = this%monoid_%identity()
      if (r < a .or. b < l) return !> r < [a, b] or [a, b] < l
      if (a <= l .and. r <= b) then !> [a, b] に [l, r]が内包.
         res = this%arr_(idx)
         return
      else !> l in [a, b] or r in [a, b]
         !> [l, r] -> [l, (l + r) / 2], [(l + r + 1) / 2, r]
         val_l = query_sub(2 * idx,                   l, (l + r) / 2)
         val_r = query_sub(2 * idx + 1, (l + r + 1) / 2, r)
         res = this%monoid_%bin_op(val_l, val_r)
      end if
    end function query_sub
  end function query_lazy_segment_tree_int64
  !> eval_and_propagate_lazy_segment_tree_int64: Treat lazy propagation.
  !> idx: Index of node of tree.
  !> length: Length of range of node.
  pure subroutine eval_and_propagate_lazy_segment_tree_int64(this, idx, length)
    class(lazy_segment_tree_int64), intent(inout) :: this
    integer(int32), intent(in) :: idx, length
    ! write(error_unit, '(a, L, *(1x, i0))') "eval_and_propagate: ", this%is_lazy_(idx) &
    !      , idx, this%arr_(idx), this%lazy_(idx), length, this%monoid_%mapping(this%arr_(idx), this%lazy_(idx), length)
    if (.not. this%is_lazy_(idx)) return
    this%arr_(idx) = this%monoid_%mapping(this%arr_(idx), this%lazy_(idx), length)
    this%is_lazy_(idx) = .false.
    if (2 * idx > this%arr_size_) return
    if (this%is_lazy_(2 * idx)) then
       this%lazy_(2 * idx) = this%monoid_%composite(this%lazy_(2 * idx), this%lazy_(idx))
    else
       this%lazy_(2 * idx) = this%lazy_(idx)
       this%is_lazy_(2 * idx) = .true.
    end if
    if (this%is_lazy_(2 * idx + 1)) then
       this%lazy_(2 * idx + 1) = this%monoid_%composite(this%lazy_(2 * idx + 1), this%lazy_(idx))
    else
       this%lazy_(2 * idx + 1) = this%lazy_(idx)
       this%is_lazy_(2 * idx + 1) = .true.
    end if
  end subroutine eval_and_propagate_lazy_segment_tree_int64
  
  subroutine dump_lazy_segment_tree_int64(this)
    class(lazy_segment_tree_int64), intent(in) :: this
    integer(int32) :: i
    write(error_unit, '(a)') "tree: "
    do i = 1, this%depth_
       write(error_unit, '(g0, ": ", *(g0, 1x))') i, this%arr_(2**(i-1):2**i-1)
    end do
    write(error_unit, '(a)') "lazy tree: "
    do i = 1, this%depth_
       write(error_unit, '(g0, ": ", *(g0, 1x))') i, this%lazy_(2**(i-1):2**i-1)
    end do
    write(error_unit, '(a)') "is_lazy?: "
    do i = 1, this%depth_
       write(error_unit, '(g0, ": ", *(L, 1x))') i, this%is_lazy_(2**(i-1):2**i-1)
    end do
  end subroutine dump_lazy_segment_tree_int64
  
  pure integer(int64) function identity_sum_int64_op() result(res)
    res = 0_int64
  end function identity_sum_int64_op
  pure integer(int64) function bin_op_sum_int64_op(x, y) result(res)
    integer(int64), intent(in) :: x, y
    res = x + y
  end function bin_op_sum_int64_op
  pure integer(int64) function mapping_sum_assign_int64_op(v, c, length) result(res)
    integer(int64), intent(in) :: v, c
    integer(4), intent(in) :: length
    res = c * length
  end function mapping_sum_assign_int64_op
  pure integer(int64) function composite_sum_assign_int64_op(c_first, c_second) result(res)
    integer(int64), intent(in) :: c_first, c_second
    res = c_second
  end function composite_sum_assign_int64_op
  pure integer(int64) function mapping_sum_add_int64_op(v, c, length) result(res)
    integer(int64), intent(in) :: v, c
    integer(4), intent(in) :: length
    res = v + c * length
  end function mapping_sum_add_int64_op
  pure integer(int64) function composite_sum_add_int64_op(c_first, c_second) result(res)
    integer(int64), intent(in) :: c_first, c_second
    res = c_first + c_second
  end function composite_sum_add_int64_op
  pure integer(int64) function mapping_sum_mul_int64_op(v, c, length) result(res)
    integer(int64), intent(in) :: v, c
    integer(4), intent(in) :: length
    res = v * c
  end function mapping_sum_mul_int64_op
  pure integer(int64) function composite_sum_mul_int64_op(c_first, c_second) result(res)
    integer(int64), intent(in) :: c_first, c_second
    res = c_first + c_second
  end function composite_sum_mul_int64_op
  
  pure integer(int64) function identity_min_int64_op() result(res)
    res = huge(0_int64)
  end function identity_min_int64_op
  pure integer(int64) function bin_op_min_int64_op(x, y) result(res)
    integer(int64), intent(in) :: x, y
    res = min(x, y)
  end function bin_op_min_int64_op
  pure integer(int64) function mapping_min_assign_int64_op(v, c, length) result(res)
    integer(int64), intent(in) :: v, c
    integer(4), intent(in) :: length
    res = c
  end function mapping_min_assign_int64_op
  pure integer(int64) function composite_min_assign_int64_op(c_first, c_second) result(res)
    integer(int64), intent(in) :: c_first, c_second
    res = c_second
  end function composite_min_assign_int64_op
  pure integer(int64) function mapping_min_add_int64_op(v, c, length) result(res)
    integer(int64), intent(in) :: v, c
    integer(4), intent(in) :: length
    res = v + c
  end function mapping_min_add_int64_op
  pure integer(int64) function composite_min_add_int64_op(c_first, c_second) result(res)
    integer(int64), intent(in) :: c_first, c_second
    res = c_first + c_second
  end function composite_min_add_int64_op
  
  pure integer(int64) function identity_max_int64_op() result(res)
    res = -huge(0_int64)-1
  end function identity_max_int64_op
  pure integer(int64) function bin_op_max_int64_op(x, y) result(res)
    integer(int64), intent(in) :: x, y
    res = max(x, y)
  end function bin_op_max_int64_op
  pure integer(int64) function mapping_max_assign_int64_op(v, c, length) result(res)
    integer(int64), intent(in) :: v, c
    integer(4), intent(in) :: length
    res = c
  end function mapping_max_assign_int64_op
  pure integer(int64) function composite_max_assign_int64_op(c_first, c_second) result(res)
    integer(int64), intent(in) :: c_first, c_second
    res = c_second
  end function composite_max_assign_int64_op
  pure integer(int64) function mapping_max_add_int64_op(v, c, length) result(res)
    integer(int64), intent(in) :: v, c
    integer(4), intent(in) :: length
    res = v + c
  end function mapping_max_add_int64_op
  pure integer(int64) function composite_max_add_int64_op(c_first, c_second) result(res)
    integer(int64), intent(in) :: c_first, c_second
    res = c_first + c_second
  end function composite_max_add_int64_op
  
end module lazy_segment_tree_m
