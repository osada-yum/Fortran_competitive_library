module lazy_segment_tree_zero_one_sequence_m
  use, intrinsic :: iso_fortran_env
  implicit none
  private
  public :: zero_one_sequence
  type :: zero_one_sequence
     integer(int32) :: v_(7) = 0_int32
   contains
  end type zero_one_sequence
  public :: lazy_segment_tree_zero_one_sequence
  public :: monoid_op_zero_one_sequence
  type :: lazy_segment_tree_zero_one_sequence
     private
     integer(int32) :: num_elems_, arr_size_, tree_size_, depth_, idx_range_left_, idx_range_right_
     type(zero_one_sequence), allocatable :: arr_(:)
     , allocatable :: lazy_(:)
     logical, allocatable :: is_lazy_(:)
     class(monoid_op_zero_one_sequence), allocatable :: monoid_
   contains
     procedure, pass :: init_lazy_segment_tree_zero_one_sequence
     procedure, pass :: init_by_arr_lazy_segment_tree_zero_one_sequence
     generic :: init => &
          init_lazy_segment_tree_zero_one_sequence, &
          init_by_arr_lazy_segment_tree_zero_one_sequence
     procedure, pass :: dump => &
          dump_lazy_segment_tree_zero_one_sequence
     procedure, pass :: unsafe_set => &
          unsafe_set_lazy_segment_tree_zero_one_sequence
     procedure, pass :: unsafe_bottomup_update => &
          unsafe_bottomup_update_lazy_segment_tree_zero_one_sequence
     procedure, pass :: strict_propagate_all => &
          strict_propagate_all_lazy_segment_tree_zero_one_sequence
     procedure, pass, private :: strict_propagate_all_sub => &
          strict_propagate_all_sub_lazy_segment_tree_zero_one_sequence
     procedure, pass :: update => &
          update_lazy_segment_tree_zero_one_sequence
     procedure, pass, private :: update_sub => &
          update_sub_lazy_segment_tree_zero_one_sequence
     procedure, pass :: query => &
          query_lazy_segment_tree_zero_one_sequence
     procedure, pass, private :: eval_and_propagate => &
          eval_and_propagate_lazy_segment_tree_zero_one_sequence
  end type lazy_segment_tree_zero_one_sequence
  type, abstract :: monoid_op_zero_one_sequence
     private
   contains
     procedure(identity_zero_one_sequence) , nopass, deferred :: identity
     procedure(bin_op_zero_one_sequence)   , nopass, deferred :: bin_op
     procedure(mapping_zero_one_sequence)  , nopass, deferred :: mapping
     procedure(composite_zero_one_sequence), nopass, deferred :: composite
  end type monoid_op_zero_one_sequence
  abstract interface
     pure type(zero_one_sequence) function identity_zero_one_sequence() result(res)
       import zero_one_sequence
     end function identity_zero_one_sequence
     pure type(zero_one_sequence) function bin_op_zero_one_sequence(x, y) result(res)
       import zero_one_sequence
       type(zero_one_sequence), intent(in) :: x, y
     end function bin_op_zero_one_sequence
     pure type(zero_one_sequence) function mapping_zero_one_sequence(v, c, length) result(res)
       import zero_one_sequence
       type(zero_one_sequence), intent(in) :: v
       , intent(in) :: c
       integer(4), intent(in) :: length
     end function mapping_zero_one_sequence
     pure type(zero_one_sequence) function composite_zero_one_sequence(c_first, c_second) result(res)
       import zero_one_sequence
       , intent(in) :: c_first, c_second
     end function composite_zero_one_sequence
  end interface
  
contains
  !> indices rage [1:2^a-1]
  !> init_lazy_segment_tree_zero_one_sequence: Initialize lazy_segment_tree_zero_one_sequence with  and monoid
  pure subroutine init_lazy_segment_tree_zero_one_sequence(this, num_elems, monoid)
    class(lazy_segment_tree_zero_one_sequence), intent(inout) :: this
    integer(int32), intent(in) :: num_elems
    class(monoid_op_zero_one_sequence), intent(in) :: monoid
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
  end subroutine init_lazy_segment_tree_zero_one_sequence
  !> init_by_arr_lazy_segment_tree_zero_one_sequence: Initialize lazy segment tree by array.
  pure subroutine init_by_arr_lazy_segment_tree_zero_one_sequence(this, arr, monoid)
    class(lazy_segment_tree_zero_one_sequence), intent(inout) :: this
    type(zero_one_sequence), intent(in) :: arr(:)
    class(monoid_op_zero_one_sequence), intent(in) :: monoid
    integer(int32) :: i
    call this%init(size(arr), monoid)
    do i = 1, size(arr)
       this%arr_(i + this%tree_size_ - 1) = arr(i)
    end do
    call this%strict_propagate_all()
  end subroutine init_by_arr_lazy_segment_tree_zero_one_sequence
  
  !> unsafe_set_lazy_segment_tree_zero_one_sequence: Set value into the node of leaf of tree.
  pure subroutine unsafe_set_lazy_segment_tree_zero_one_sequence(this, idx, val)
    class(lazy_segment_tree_zero_one_sequence), intent(inout) :: this
    integer(int32), intent(in) :: idx
    type(zero_one_sequence), intent(in) :: val
    this%arr_(this%tree_size_ + idx - 1) = val
  end subroutine unsafe_set_lazy_segment_tree_zero_one_sequence
  !> unsafe_set_lazy_segment_tree_zero_one_sequence: Update all node.
  pure subroutine unsafe_bottomup_update_lazy_segment_tree_zero_one_sequence(this)
    class(lazy_segment_tree_zero_one_sequence), intent(inout) :: this
    integer(int32) :: i
    do i = this%tree_size_ - 1, 1, -1
       this%arr_(i) = this%monoid_%bin_op(this%arr_(2 * i), this%arr_(2 * i + 1))
    end do
    this%is_lazy_(:) = .false.
  end subroutine unsafe_bottomup_update_lazy_segment_tree_zero_one_sequence
  
  !> strict_propagate_all_lazy_segment_tree_zero_one_sequence: Update all node.
  pure subroutine strict_propagate_all_lazy_segment_tree_zero_one_sequence(this)
    class(lazy_segment_tree_zero_one_sequence), intent(inout) :: this
    call this%strict_propagate_all_sub(1, this%idx_range_left_, this%idx_range_right_)
  end subroutine strict_propagate_all_lazy_segment_tree_zero_one_sequence
  pure recursive subroutine strict_propagate_all_sub_lazy_segment_tree_zero_one_sequence(this, idx, l, r)
    class(lazy_segment_tree_zero_one_sequence), intent(inout) :: this
    integer(int32), intent(in) :: idx, l, r
    call this%eval_and_propagate(idx, r - l + 1)
    if (l == r) return
    call this%strict_propagate_all_sub(2 * idx,                   l, (l + r) / 2)
    call this%strict_propagate_all_sub(2 * idx + 1, (l + r + 1) / 2, r)
    this%arr_(idx) = this%monoid_%bin_op(this%arr_(2 * idx), this%arr_(2 * idx + 1))
  end subroutine strict_propagate_all_sub_lazy_segment_tree_zero_one_sequence
  
  !> update_lazy_segment_tree_zero_one_sequence: Update tree by .
  !> [a, b]: Range to update.
  !> val: Value of update.
  pure subroutine update_lazy_segment_tree_zero_one_sequence(this, a, b, val)
    class(lazy_segment_tree_zero_one_sequence), intent(inout) :: this
    integer(int32), intent(in) :: a, b
    type(zero_one_sequence), intent(in) :: val
    if (a > b) error stop "Illegal range of a > b."
    call this%update_sub(a, b, val, 1, this%idx_range_left_, this%idx_range_right_)
  end subroutine update_lazy_segment_tree_zero_one_sequence
  !> update_sub_lazy_segment_tree_zero_one_sequence: Update tree by .
  !> [a, b]: Range to update.
  !> val: Value of update.
  !> idx: Index of tree.
  !> [l, r]: Range of current node of tree.
  pure recursive subroutine update_sub_lazy_segment_tree_zero_one_sequence(this, a, b, val, idx, l, r)
    class(lazy_segment_tree_zero_one_sequence), intent(inout) :: this
    integer(int32), intent(in) :: a, b, idx, l, r
    type(zero_one_sequence), intent(in) :: val
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
  end subroutine update_sub_lazy_segment_tree_zero_one_sequence
  
  ! 閉区間[a, b]で操作.
  impure type(zero_one_sequence) function query_lazy_segment_tree_zero_one_sequence (this, a, b) result(query)
    class(lazy_segment_tree_zero_one_sequence), intent(inout) :: this
    integer(int32), intent(in) :: a, b
    if (a > b) error stop "Illegal range of a > b."
    query = query_sub(1, this%idx_range_left_, this%idx_range_right_)
  contains
    impure recursive type(zero_one_sequence) function query_sub(idx, l, r) result(res)
      integer(int32), intent(in) :: idx, l, r
      type(zero_one_sequence) :: val_l, val_r
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
  end function query_lazy_segment_tree_zero_one_sequence
  !> eval_and_propagate_lazy_segment_tree_zero_one_sequence: Treat lazy propagation.
  !> idx: Index of node of tree.
  !> length: Length of range of node.
  pure subroutine eval_and_propagate_lazy_segment_tree_zero_one_sequence(this, idx, length)
    class(lazy_segment_tree_zero_one_sequence), intent(inout) :: this
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
  end subroutine eval_and_propagate_lazy_segment_tree_zero_one_sequence
  
  subroutine dump_lazy_segment_tree_zero_one_sequence(this)
    class(lazy_segment_tree_zero_one_sequence), intent(in) :: this
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
  end subroutine dump_lazy_segment_tree_zero_one_sequence
  
end module lazy_segment_tree_zero_one_sequence_m
