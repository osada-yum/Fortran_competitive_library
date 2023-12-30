module tuple2_btree_m
  use, intrinsic :: iso_fortran_env
  use tuple2_m, only: &
       t2_i32_i32 => tuple2_int32_int32, &
       t2_i64_i64 => tuple2_int64_int64, &
       operator(==), operator(<), operator(<=), operator(>), operator(>=)
  implicit none
  private
  !> `t-1` must be the least number of elements in `btree_node` without root (minimum degree).
  integer(int32), parameter :: t = 6
  !> the number of internal node in `btree_node`.
  integer(int32), parameter :: inode = 2*t-1
  integer(int32), parameter :: iter_max_depth = 30
  !> pointer to btree_node_t2_i32_i32_to_int32.
  type :: btree_node_ptr_t2_i32_i32_to_int32
     type(btree_node_t2_i32_i32_to_int32), pointer :: p_ => null()
   contains
     procedure, pass :: size => size_btree_node_ptr_t2_i32_i32_to_int32
     procedure, pass :: is_leaf => is_leaf_btree_node_ptr_t2_i32_i32_to_int32
     procedure, pass :: get_iter => get_iter_btree_node_ptr_t2_i32_i32_to_int32
     procedure, pass :: split_child => split_child_btree_node_ptr_t2_i32_i32_to_int32
     procedure, pass :: insert => insert_btree_node_ptr_t2_i32_i32_to_int32
     procedure, pass :: remove     => remove_btree_node_ptr_t2_i32_i32_to_int32
     procedure, pass :: remove_key => remove_key_btree_node_ptr_t2_i32_i32_to_int32
     procedure, pass :: merge_children => merge_children_btree_node_ptr_t2_i32_i32_to_int32
     procedure, pass :: rotate_left  => rotate_left_btree_node_ptr_t2_i32_i32_to_int32
     procedure, pass :: rotate_right => rotate_right_btree_node_ptr_t2_i32_i32_to_int32
     procedure, pass :: shrink_left  => shrink_left_btree_node_ptr_t2_i32_i32_to_int32
     procedure, pass :: expand_right => expand_right_btree_node_ptr_t2_i32_i32_to_int32
     procedure, pass :: print => print_btree_node_ptr_t2_i32_i32_to_int32
     procedure, pass :: check_invariant => check_invariant_btree_node_ptr_t2_i32_i32_to_int32
  end type btree_node_ptr_t2_i32_i32_to_int32
  !> node of B-Tree.
  type :: btree_node_t2_i32_i32_to_int32
     integer(int32) :: nelem_ = 0
     type(t2_i32_i32) :: key_(inode)
     integer(int32) :: val_(inode)
     type(btree_node_ptr_t2_i32_i32_to_int32) :: children_(inode+1)
     logical :: is_leaf_ = .true.
  end type btree_node_t2_i32_i32_to_int32
  
  public :: btree_t2_i32_i32_to_int32
  !>  has pointer to root of B-Tree.
  type :: btree_t2_i32_i32_to_int32
     private
     type(btree_node_ptr_t2_i32_i32_to_int32) :: root_
     integer(int32) :: size_   = 0
     integer(int32) :: height_ = 0
   contains
     procedure, pass :: size   => size_btree_t2_i32_i32_to_int32
     procedure, pass :: height => height_btree_t2_i32_i32_to_int32
     procedure, pass :: init   => init_btree_t2_i32_i32_to_int32
     procedure, pass :: get      => get_btree_t2_i32_i32_to_int32
     procedure, pass :: get_iter => get_iter_btree_t2_i32_i32_to_int32
     procedure, pass :: contains => contains_btree_t2_i32_i32_to_int32
     procedure, pass :: insert => insert_btree_t2_i32_i32_to_int32
     procedure, pass :: remove => remove_btree_t2_i32_i32_to_int32
     procedure, pass :: minimum => minimum_btree_t2_i32_i32_to_int32
     procedure, pass :: maximum => maximum_btree_t2_i32_i32_to_int32
     procedure, pass :: minimum_iter => minimum_iter_btree_t2_i32_i32_to_int32
     procedure, pass :: maximum_iter => maximum_iter_btree_t2_i32_i32_to_int32
     ! procedure, pass :: lower_bound => lower_bound_btree_t2_i32_i32_to_int32
     ! procedure, pass :: upper_bound => upper_bound_btree_t2_i32_i32_to_int32
     procedure, pass :: print => print_btree_t2_i32_i32_to_int32
     procedure, pass :: check_invariant => check_invariant_btree_t2_i32_i32_to_int32
  end type btree_t2_i32_i32_to_int32
  public btree_node_iter_t2_i32_i32_to_int32
  type :: btree_node_iter_t2_i32_i32_to_int32
     private
     integer(int32) :: idx_ = -1 !> [1:iter%nptr_%size()], ノード内の節点を指す.
     type(btree_node_ptr_t2_i32_i32_to_int32) :: nptr_
     integer(int32) :: depth_ = 1
     integer(int32) :: indices_(iter_max_depth) !> [1:iter%nptr_%size()+1], 下ったポインタのインデックス.
     type(btree_node_ptr_t2_i32_i32_to_int32) :: parents_(iter_max_depth)
   contains
     procedure, pass :: key => key_btree_node_iter_t2_i32_i32_to_int32
     procedure, pass :: val => val_btree_node_iter_t2_i32_i32_to_int32
     procedure, pass :: mut_val => mut_val_btree_node_iter_t2_i32_i32_to_int32
     procedure, pass :: next => next_btree_node_iter_t2_i32_i32_to_int32
     procedure, pass :: prev => prev_btree_node_iter_t2_i32_i32_to_int32
     procedure, pass :: is_begin     => is_begin_btree_node_iter_t2_i32_i32_to_int32
     procedure, pass :: is_not_begin => is_not_begin_btree_node_iter_t2_i32_i32_to_int32
     procedure, pass :: is_end     => is_end_btree_node_iter_t2_i32_i32_to_int32
     procedure, pass :: is_not_end => is_not_end_btree_node_iter_t2_i32_i32_to_int32
     procedure, pass :: exist     => exist_btree_node_iter_t2_i32_i32_to_int32
     procedure, pass :: not_exist => not_exist_btree_node_iter_t2_i32_i32_to_int32
  end type btree_node_iter_t2_i32_i32_to_int32
  
  !> pointer to btree_node_t2_i64_i64_to_int64.
  type :: btree_node_ptr_t2_i64_i64_to_int64
     type(btree_node_t2_i64_i64_to_int64), pointer :: p_ => null()
   contains
     procedure, pass :: size => size_btree_node_ptr_t2_i64_i64_to_int64
     procedure, pass :: is_leaf => is_leaf_btree_node_ptr_t2_i64_i64_to_int64
     procedure, pass :: get_iter => get_iter_btree_node_ptr_t2_i64_i64_to_int64
     procedure, pass :: split_child => split_child_btree_node_ptr_t2_i64_i64_to_int64
     procedure, pass :: insert => insert_btree_node_ptr_t2_i64_i64_to_int64
     procedure, pass :: remove     => remove_btree_node_ptr_t2_i64_i64_to_int64
     procedure, pass :: remove_key => remove_key_btree_node_ptr_t2_i64_i64_to_int64
     procedure, pass :: merge_children => merge_children_btree_node_ptr_t2_i64_i64_to_int64
     procedure, pass :: rotate_left  => rotate_left_btree_node_ptr_t2_i64_i64_to_int64
     procedure, pass :: rotate_right => rotate_right_btree_node_ptr_t2_i64_i64_to_int64
     procedure, pass :: shrink_left  => shrink_left_btree_node_ptr_t2_i64_i64_to_int64
     procedure, pass :: expand_right => expand_right_btree_node_ptr_t2_i64_i64_to_int64
     procedure, pass :: print => print_btree_node_ptr_t2_i64_i64_to_int64
     procedure, pass :: check_invariant => check_invariant_btree_node_ptr_t2_i64_i64_to_int64
  end type btree_node_ptr_t2_i64_i64_to_int64
  !> node of B-Tree.
  type :: btree_node_t2_i64_i64_to_int64
     integer(int32) :: nelem_ = 0
     type(t2_i64_i64) :: key_(inode)
     integer(int64) :: val_(inode)
     type(btree_node_ptr_t2_i64_i64_to_int64) :: children_(inode+1)
     logical :: is_leaf_ = .true.
  end type btree_node_t2_i64_i64_to_int64
  
  public :: btree_t2_i64_i64_to_int64
  !>  has pointer to root of B-Tree.
  type :: btree_t2_i64_i64_to_int64
     private
     type(btree_node_ptr_t2_i64_i64_to_int64) :: root_
     integer(int32) :: size_   = 0
     integer(int32) :: height_ = 0
   contains
     procedure, pass :: size   => size_btree_t2_i64_i64_to_int64
     procedure, pass :: height => height_btree_t2_i64_i64_to_int64
     procedure, pass :: init   => init_btree_t2_i64_i64_to_int64
     procedure, pass :: get      => get_btree_t2_i64_i64_to_int64
     procedure, pass :: get_iter => get_iter_btree_t2_i64_i64_to_int64
     procedure, pass :: contains => contains_btree_t2_i64_i64_to_int64
     procedure, pass :: insert => insert_btree_t2_i64_i64_to_int64
     procedure, pass :: remove => remove_btree_t2_i64_i64_to_int64
     procedure, pass :: minimum => minimum_btree_t2_i64_i64_to_int64
     procedure, pass :: maximum => maximum_btree_t2_i64_i64_to_int64
     procedure, pass :: minimum_iter => minimum_iter_btree_t2_i64_i64_to_int64
     procedure, pass :: maximum_iter => maximum_iter_btree_t2_i64_i64_to_int64
     ! procedure, pass :: lower_bound => lower_bound_btree_t2_i64_i64_to_int64
     ! procedure, pass :: upper_bound => upper_bound_btree_t2_i64_i64_to_int64
     procedure, pass :: print => print_btree_t2_i64_i64_to_int64
     procedure, pass :: check_invariant => check_invariant_btree_t2_i64_i64_to_int64
  end type btree_t2_i64_i64_to_int64
  public btree_node_iter_t2_i64_i64_to_int64
  type :: btree_node_iter_t2_i64_i64_to_int64
     private
     integer(int32) :: idx_ = -1 !> [1:iter%nptr_%size()], ノード内の節点を指す.
     type(btree_node_ptr_t2_i64_i64_to_int64) :: nptr_
     integer(int32) :: depth_ = 1
     integer(int32) :: indices_(iter_max_depth) !> [1:iter%nptr_%size()+1], 下ったポインタのインデックス.
     type(btree_node_ptr_t2_i64_i64_to_int64) :: parents_(iter_max_depth)
   contains
     procedure, pass :: key => key_btree_node_iter_t2_i64_i64_to_int64
     procedure, pass :: val => val_btree_node_iter_t2_i64_i64_to_int64
     procedure, pass :: mut_val => mut_val_btree_node_iter_t2_i64_i64_to_int64
     procedure, pass :: next => next_btree_node_iter_t2_i64_i64_to_int64
     procedure, pass :: prev => prev_btree_node_iter_t2_i64_i64_to_int64
     procedure, pass :: is_begin     => is_begin_btree_node_iter_t2_i64_i64_to_int64
     procedure, pass :: is_not_begin => is_not_begin_btree_node_iter_t2_i64_i64_to_int64
     procedure, pass :: is_end     => is_end_btree_node_iter_t2_i64_i64_to_int64
     procedure, pass :: is_not_end => is_not_end_btree_node_iter_t2_i64_i64_to_int64
     procedure, pass :: exist     => exist_btree_node_iter_t2_i64_i64_to_int64
     procedure, pass :: not_exist => not_exist_btree_node_iter_t2_i64_i64_to_int64
  end type btree_node_iter_t2_i64_i64_to_int64
  
contains
  subroutine init_btree_t2_i32_i32_to_int32(this)
    class(btree_t2_i32_i32_to_int32), intent(inout) :: this
    type(btree_node_t2_i32_i32_to_int32), pointer :: x
    allocate(x)
    x%is_leaf_ = .true.
    x%nelem_ = 0
    this%size_   = 0
    this%height_ = 0
    this%root_%p_ => x
  end subroutine init_btree_t2_i32_i32_to_int32
  pure integer(int32) function size_btree_t2_i32_i32_to_int32(this) result(res)
    class(btree_t2_i32_i32_to_int32), intent(in) :: this
    res = this%size_
  end function size_btree_t2_i32_i32_to_int32
  pure integer(int32) function height_btree_t2_i32_i32_to_int32(this) result(res)
    class(btree_t2_i32_i32_to_int32), intent(in) :: this
    res = this%height_
  end function height_btree_t2_i32_i32_to_int32
  integer(int32) function get_btree_t2_i32_i32_to_int32(this, key) result(res)
    class(btree_t2_i32_i32_to_int32), intent(in) :: this
    type(t2_i32_i32), intent(in) :: key
    type(btree_node_iter_t2_i32_i32_to_int32) :: iter
    iter = this%root_%get_iter(key)
    if (iter%idx_ /= -1) then
       res = iter%nptr_%p_%val_(iter%idx_)
    else
    end if
  end function get_btree_t2_i32_i32_to_int32
  type(btree_node_iter_t2_i32_i32_to_int32) function get_iter_btree_t2_i32_i32_to_int32(this, key) result(res)
    class(btree_t2_i32_i32_to_int32), intent(in) :: this
    type(t2_i32_i32), intent(in) :: key
    res = this%root_%get_iter(key)
  end function get_iter_btree_t2_i32_i32_to_int32
  logical function contains_btree_t2_i32_i32_to_int32(this, key) result(res)
    class(btree_t2_i32_i32_to_int32), intent(in) :: this
    type(t2_i32_i32), intent(in) :: key
    type(btree_node_iter_t2_i32_i32_to_int32) :: iter
    iter = this%root_%get_iter(key)
    res = iter%idx_ /= -1
  end function contains_btree_t2_i32_i32_to_int32
  subroutine insert_btree_t2_i32_i32_to_int32(this, key, val)
    class(btree_t2_i32_i32_to_int32), intent(inout) :: this
    type(t2_i32_i32), intent(in) :: key
    integer(int32), intent(in) :: val
    type(btree_node_ptr_t2_i32_i32_to_int32) :: r
    type(btree_node_iter_t2_i32_i32_to_int32) :: iter
    r%p_ => this%root_%p_
    if (r%p_%nelem_ == 2*t - 1) then
       block
         type(btree_node_ptr_t2_i32_i32_to_int32) :: s
         allocate(s%p_)
         this%root_%p_ => s%p_
         s%p_%is_leaf_ = .false.
         s%p_%nelem_ = 0
         s%p_%children_(1)%p_ => r%p_
         call s%split_child(1)
         this%height_ = this%height_ + 1
         iter = s%insert(key, val)
       end block
    else
       iter = r%insert(key, val)
    end if
    if (iter%idx_ > 0) &
         this%size_ = this%size_ + 1
  end subroutine insert_btree_t2_i32_i32_to_int32
  subroutine remove_btree_t2_i32_i32_to_int32(this, key)
    class(btree_t2_i32_i32_to_int32), intent(inout) :: this
    type(t2_i32_i32), intent(in) :: key
    type(btree_node_ptr_t2_i32_i32_to_int32) :: tmp
    call this%root_%remove(key)
    if (this%root_%p_%nelem_ == 0 .and. (.not. this%root_%is_leaf())) then
       tmp%p_ => this%root_%p_
       this%root_%p_ => this%root_%p_%children_(1)%p_
       deallocate(tmp%p_)
       nullify(tmp%p_)
       this%height_ = this%height_ - 1
    end if
    this%size_ = this%size_ - 1
  end subroutine remove_btree_t2_i32_i32_to_int32
  !> minimum_btree_t2_i32_i32_to_int32: Return the minimum value.
  type(t2_i32_i32) function minimum_btree_t2_i32_i32_to_int32(this) result(res)
    class(btree_t2_i32_i32_to_int32), intent(in) :: this
    type(btree_node_iter_t2_i32_i32_to_int32) :: iter
    iter = this%minimum_iter()
    res = iter%key()
  end function minimum_btree_t2_i32_i32_to_int32
  !> minimum_iter_btree_t2_i32_i32_to_int32: Return the iterator to node that has minimum key.
  type(btree_node_iter_t2_i32_i32_to_int32) function minimum_iter_btree_t2_i32_i32_to_int32(this) result(res)
    class(btree_t2_i32_i32_to_int32), intent(in) :: this
    res%nptr_%p_ => this%root_%p_
    res%depth_ = 1
    res%idx_ = 0
    call res%next()
  end function minimum_iter_btree_t2_i32_i32_to_int32
  !> maximum_btree_t2_i32_i32_to_int32: Return the maximum value.
  type(t2_i32_i32) function maximum_btree_t2_i32_i32_to_int32(this) result(res)
    class(btree_t2_i32_i32_to_int32), intent(in) :: this
    type(btree_node_iter_t2_i32_i32_to_int32) :: iter
    iter = this%maximum_iter()
    res = iter%key()
  end function maximum_btree_t2_i32_i32_to_int32
  !> maximum_iter_btree_t2_i32_i32_to_int32: Return the iterator to node that has maximum key.
  type(btree_node_iter_t2_i32_i32_to_int32) function maximum_iter_btree_t2_i32_i32_to_int32(this) result(res)
    class(btree_t2_i32_i32_to_int32), intent(in) :: this
    res%nptr_%p_ => this%root_%p_
    res%depth_ = 1
    res%idx_ = res%nptr_%size() + 1
    call res%prev()
  end function maximum_iter_btree_t2_i32_i32_to_int32
  !> print_btree: Print whole node in B-tree for debug.
  !> For debug.
  subroutine print_btree_t2_i32_i32_to_int32(this, unit)
    class(btree_t2_i32_i32_to_int32), intent(in) :: this
    integer(int32), intent(in) :: unit
    if (associated(this%root_%p_)) &
         call this%root_%print(unit, 0)
  end subroutine print_btree_t2_i32_i32_to_int32
  !> check_invariant_btree_t2_i32_i32_to_int32: Check invariant for debug.
  !> invariant condition: The number of keys of each node in B-tree excluded root node must have at least  keys.
  !> The keys in left children is less than key of current node.
  !> The keys in right children is greater than key of current node.
  subroutine check_invariant_btree_t2_i32_i32_to_int32(this)
    class(btree_t2_i32_i32_to_int32), intent(in) :: this
    type(btree_node_iter_t2_i32_i32_to_int32) :: bt_iter
    type(t2_i32_i32) :: k, k_bef
    integer(int32) :: i
    if (this%size() == 0) return
    bt_iter = this%minimum_iter()
    k_bef = bt_iter%key()
    call bt_iter%next()
    do while (bt_iter%is_not_end())
       k = bt_iter%key()
       ! write(error_unit, *) k_bef, k
       if (k_bef >= k) then
          write(error_unit, '(a)') "Error: B-tree is not ordered."
          write(error_unit, '(a)') "Something wrong occurred in 'minimum_iter' or 'next'."
          error stop 5
       end if
       k_bef = k
       call bt_iter%next()
    end do
    bt_iter = this%maximum_iter()
    k_bef = bt_iter%key()
    call bt_iter%prev()
    do while (bt_iter%is_not_begin())
       k = bt_iter%key()
       ! write(error_unit, *) k_bef, k
       if (k_bef <= k) then
          write(error_unit, '(a)') "Error: B-tree is not ordered."
          write(error_unit, '(a)') "Something wrong occurred in 'maximum_iter' or 'prev'."
          error stop 6
       end if
       k_bef = k
       call bt_iter%prev()
    end do
    if (this%root_%is_leaf()) return
    do i = 1, this%root_%size() + 1
       call this%root_%p_%children_(i)%check_invariant()
    end do
  end subroutine check_invariant_btree_t2_i32_i32_to_int32
  pure integer(int32) function size_btree_node_ptr_t2_i32_i32_to_int32(this) result(res)
    class(btree_node_ptr_t2_i32_i32_to_int32), intent(in) :: this
    res = this%p_%nelem_
  end function size_btree_node_ptr_t2_i32_i32_to_int32
  pure logical function is_leaf_btree_node_ptr_t2_i32_i32_to_int32(this) result(res)
    class(btree_node_ptr_t2_i32_i32_to_int32), intent(in) :: this
    res = this%p_%is_leaf_
  end function is_leaf_btree_node_ptr_t2_i32_i32_to_int32
  type(btree_node_iter_t2_i32_i32_to_int32) function get_iter_btree_node_ptr_t2_i32_i32_to_int32(this, key) result(res)
    class(btree_node_ptr_t2_i32_i32_to_int32), intent(in) :: this
    type(t2_i32_i32), intent(in) :: key
    integer(int32) :: pos
    res%nptr_%p_ => this%p_
    if (res%nptr_%size() == 0) then
       res%idx_ = -1
       return
    end if
    res%depth_ = 1
    do !> search , which satisfied arr(pos) < key <= arr(pos+1), arr(0) == -infinity, arr(n+1) == +infinity
       pos = lower_bound(1, res%nptr_%size(), res%nptr_%p_%key_(1:res%nptr_%size()), key)
       ! write(error_unit, '(3(a, i0, 1x), *(i0, 1x))') "pos: ", pos, "key: ", key, "arr: ", res%nptr_%p_%key_(1:res%nptr_%size())
       !> key <= key_(pos)
       if (pos <= res%nptr_%size()) then
          if (res%nptr_%p_%key_(pos) == key) then !> key found.
             res%idx_ = pos
             return
          end if
       end if
       if (res%nptr_%is_leaf()) exit
       res%indices_(res%depth_) = pos
       res%parents_(res%depth_)%p_ => res%nptr_%p_
       res%nptr_%p_ => res%nptr_%p_%children_(pos)%p_
       res%depth_ = res%depth_ + 1
    end do
    !> not found.
    nullify(res%nptr_%p_)
    res%idx_ = -1
    return
  contains
    !> lower_bound: search , which satisfied arr(i) < key <= arr(i+1), arr(0) == -infinity.
    pure integer(int32) function lower_bound(lb, ub, arr, key) result(res)
      integer(int32), intent(in) :: lb, ub
      type(t2_i32_i32), intent(in) :: arr(lb:ub)
      type(t2_i32_i32), intent(in) :: key
      integer(int32) :: p, q, r
      p = lb
      r = ub
      if (key <= arr(p)) then
         res = p
      else if (arr(r) < key) then
         res = r + 1
      else !> arr(p) < key <= arr(r)
         ! invariant condition:
         ! key > arr(p) .and. key <= arr(r)
         binary_search: do while(p + 1 < r)
            q = (p+r) / 2
            if (arr(q) < key) then
               p = q
            else !> key <= arr(q)
               r = q
            end if
         end do binary_search
         res = r
      end if
    end function lower_bound
  end function get_iter_btree_node_ptr_t2_i32_i32_to_int32
  subroutine split_child_btree_node_ptr_t2_i32_i32_to_int32(this, idx)
    class(btree_node_ptr_t2_i32_i32_to_int32), intent(in) :: this
    integer(int32), intent(in) :: idx
    type(btree_node_ptr_t2_i32_i32_to_int32) :: y, z
    integer(int32) :: i
    allocate(z%p_)
    y%p_ => this%p_%children_(idx)%p_
    z%p_%is_leaf_ = y%p_%is_leaf_
    z%p_%nelem_ = t - 1
    do i = 1, t - 1
       z%p_%key_(i) = y%p_%key_(i+t)
       z%p_%val_(i) = y%p_%val_(i+t)
    end do
    if (.not. y%is_leaf()) then
       do i = 1, t
          z%p_%children_(i)%p_ => y%p_%children_(i+t)%p_
       end do
    end if
    y%p_%nelem_ = t - 1
    do i = this%size()+1, idx+1, -1
       this%p_%children_(i+1)%p_ => this%p_%children_(i)%p_
    end do
    this%p_%children_(idx+1)%p_ => z%p_
    do i = this%p_%nelem_, idx, -1
       this%p_%key_(i+1) = this%p_%key_(i)
       this%p_%val_(i+1) = this%p_%val_(i)
    end do
    this%p_%key_(idx) = y%p_%key_(t)
    this%p_%val_(idx) = y%p_%val_(t)
    this%p_%nelem_ = this%p_%nelem_ + 1
  end subroutine split_child_btree_node_ptr_t2_i32_i32_to_int32
  !> insert_btree_node_ptr_t2_i32_i32_to_int32: Insert (, ) into B-tree.
  type(btree_node_iter_t2_i32_i32_to_int32) function insert_btree_node_ptr_t2_i32_i32_to_int32(this, key, val) result(res)
    class(btree_node_ptr_t2_i32_i32_to_int32), intent(in) :: this
    type(t2_i32_i32), intent(in) :: key
    integer(int32), intent(in) :: val
    type(btree_node_ptr_t2_i32_i32_to_int32) :: x
    integer(int32) :: pos
    x%p_ => this%p_
    if (x%size() == 0) then
       x%p_%key_(1) = key
       x%p_%val_(1) = val
       x%p_%nelem_ = 1
       res%nptr_%p_ => x%p_
       res%idx_ = 1
       return
    end if
    ! write(error_unit, '(L)') x%p_%is_leaf_
    do while (.not. x%is_leaf())
       pos = lower_bound(1, x%size(), x%p_%key_(1:x%size()), key)
       if (x%p_%children_(pos)%size() == 2*t - 1) then
          call x%split_child(pos)
          if (key > x%p_%key_(pos)) pos = pos + 1
       end if
       if (pos <= x%size()) then
          if (key == x%p_%key_(pos)) then
             nullify(res%nptr_%p_)
             res%idx_ = -1
             return
          end if
       end if
       x%p_ => x%p_%children_(pos)%p_
    end do
    pos = lower_bound(1, x%size(), x%p_%key_(1:x%size()), key)
    if (pos <= x%size()) then !>  <= , where s == x%size().
       if (key == x%p_%key_(pos)) then !>  already exists in B-tree.
          nullify(res%nptr_%p_)
          res%idx_ = -1
          return
       else !> expand for insertion.
          call x%expand_right(pos)
       end if
    else !>  > , where s == x%size().
       x%p_%nelem_ = x%p_%nelem_ + 1
    end if
    ! write(error_unit, '(a, i0, 2(1x, i0))') "insert: ", pos+1, key, x%p_%key_(pos+1)
    x%p_%key_(pos) = key
    x%p_%val_(pos) = val
    res%nptr_%p_ => x%p_
    res%idx_ = pos
  contains
    !> lower_bound: search , which satisfied arr(i) < key <= arr(i+1), arr(0) == -infinity.
    pure integer(int32) function lower_bound(lb, ub, arr, key) result(res)
      integer(int32), intent(in) :: lb, ub
      type(t2_i32_i32), intent(in) :: arr(lb:ub)
      type(t2_i32_i32), intent(in) :: key
      integer(int32) :: p, q, r
      p = lb
      r = ub
      if (key <= arr(p)) then
         res = p
      else if (arr(r) < key) then
         res = r + 1
      else !> arr(p) < key <= arr(r)
         ! invariant condition:
         ! key > arr(p) .and. key <= arr(r)
         binary_search: do while(p + 1 < r)
            q = (p+r) / 2
            if (arr(q) < key) then
               p = q
            else !> key <= arr(q)
               r = q
            end if
         end do binary_search
         res = r
      end if
    end function lower_bound
  end function insert_btree_node_ptr_t2_i32_i32_to_int32
  !> remove_btree_node_ptr_t2_i32_i32_to_int32: Remove  from B-tree.
  !> invariant condition: the node  has at least  keys.
  recursive subroutine remove_btree_node_ptr_t2_i32_i32_to_int32(this, key)
    class(btree_node_ptr_t2_i32_i32_to_int32), intent(in) :: this
    type(t2_i32_i32), intent(in) :: key
    type(btree_node_ptr_t2_i32_i32_to_int32) :: x, c
    integer(int32) :: pos, s
    ! write(error_unit, '(a, i0)') "search: ", key
    x%p_ => this%p_
    pos = lower_bound(1, x%size(), x%p_%key_(1:x%size()), key)
    if (x%p_%key_(min(pos, x%size())) == key) then !>  exists in current internal node.
       call x%remove_key(key, pos)
       return
    end if
    !>  does not exist in current internal node.
    if (x%is_leaf()) then
       write(error_unit, '(a, i0, a)') "key: ", key, " is not found."
       call this%print(error_unit, 0)
       error stop 2
       return
    end if
    c%p_ => x%p_%children_(pos)%p_
    if (c%size() >= t) then
       ! write(error_unit, '(a)') "pattern 3, recursive remove"
       ! remove  recurrently.
       call c%remove(key)
       ! write(error_unit, '(a, i0)') "pattern 3 end: ", key
       return
    end if
    !> size of child has  keys.
    s = x%size()
    if (pos == s + 1) then
       if (x%p_%children_(s)%size() == t - 1) then
          !> x: _w      key (== x(s))
          !>      \    /           !>       c_to     c_from
          !> ------------------------------
          !> x: _w
          !>               !>         (c_to//key//c_from)
          call x%merge_children(s)
          call x%p_%children_(x%size() + 1)%remove(key)
          ! write(error_unit, '(a, i0)') "pattern 3b end: ", key
          return
       end if
    else !> pos: [1, s]
       if (x%p_%children_(pos+1)%size() == t - 1) then
          !>        key            _w
          !>      /     \        /            !> c_to         c_from        c3
          !> ------------------------------
          !>                    _w
          !>                   /           !> c(c_to//x//c_from)      c3
          call x%merge_children(pos)
          call x%p_%children_(pos)%remove(key)
          ! write(error_unit, '(a, i0)') "pattern 3b end: ", key
          return
       end if
    end if
    !> left or right child have n (>= t) keys.
    ! write(error_unit, '(a, i0)') "pattern 3a: ", key
    if (pos == s + 1) then
       ! write(error_unit, '(a, i0)') "pattern 3a-1: ", key
       !> x:            key
       !>             /          !> (c_from:v1)         c_to
       !> ------------------------------
       !> x:         v1
       !>          /         !>   c_from        (x:c_to)
       call x%rotate_right(s)
       call x%p_%children_(x%size() + 1)%remove(key)
    else !> pos: [1, s]
       ! write(error_unit, '(a, i0)') "pattern 3a-2: ", key
       !>      x
       !> c_to   (v1:c_from)
       !> ------------------------------
       !>          v1
       !> (c_to:x)    c_from
       call x%rotate_left(pos)
       call x%p_%children_(pos)%remove(key)
    end if
    ! write(error_unit, '(a, i0)') "pattern 3a end: ", key
    return
  contains
    !> lower_bound: search , which satisfied arr(i) < key <= arr(i+1), arr(0) == -infinity.
    pure integer(int32) function lower_bound(lb, ub, arr, key) result(res)
      integer(int32), intent(in) :: lb, ub
      type(t2_i32_i32), intent(in) :: arr(lb:ub)
      type(t2_i32_i32), intent(in) :: key
      integer(int32) :: p, q, r
      p = lb
      r = ub
      if (key <= arr(p)) then
         res = p
      else if (arr(r) < key) then
         res = r + 1
      else !> arr(p) < key <= arr(r)
         ! invariant condition:
         ! key > arr(p) .and. key <= arr(r)
         binary_search: do while(p + 1 < r)
            q = (p+r) / 2
            if (arr(q) < key) then
               p = q
            else !> key <= arr(q)
               r = q
            end if
         end do binary_search
         res = r
      end if
    end function lower_bound
  end subroutine remove_btree_node_ptr_t2_i32_i32_to_int32
  !> remove_key_btree_node_ptr_t2_i32_i32_to_int32: If some of current nodes have , call this.
  recursive subroutine remove_key_btree_node_ptr_t2_i32_i32_to_int32(x, key, pos)
    class(btree_node_ptr_t2_i32_i32_to_int32), intent(in) :: x
    type(t2_i32_i32), intent(in) :: key
    integer(int32), intent(in) :: pos
    !> pos: [1, x%size()].
    if (x%is_leaf()) then
       ! write(error_unit, '(a, i0)') "pattern 1: ", key
       call x%shrink_left(pos, pos)
       return
    end if
    !> x is not leaf.
    if (x%p_%children_(pos)%size() >= t) then
       !> Exchange previous  if left child has n (>= t) keys.
       ! write(error_unit, '(a)') "pattern 2a"
       !>   x             _y
       !>  /
       !> c           _c2
       !>       !>   (c':v1)
       !> ------------------------------
       !>   v1     _y
       !>  /
       !> c    _c2
       !>       !>   c'
       block
         type(t2_i32_i32) :: key_tmp
         integer(int32) :: val_tmp
         type(btree_node_ptr_t2_i32_i32_to_int32) :: prev
         prev%p_ => x%p_%children_(pos)%p_
         do while (.not. prev%is_leaf())
            prev%p_ => prev%p_%children_(prev%size()+1)%p_
         end do
         key_tmp = prev%p_%key_(prev%size())
         val_tmp = prev%p_%val_(prev%size())
         ! write(error_unit, '(a, *(i0, 1x))') "prev: ", key_tmp, key
         call x%remove(key_tmp)
         x%p_%key_(pos) = key_tmp
         x%p_%val_(pos) = val_tmp
         ! write(error_unit, '(a, 2(i0, 1x))') "pattern 2a end: ", key, key_tmp
         return
       end block
    else if (x%p_%children_(pos+1)%size() >= t) then !> right child has n (>= t) keys.
       ! write(error_unit, '(a)') "pattern 2b"
       !>     x         _y
       !>                !> _c1         c
       !>            /
       !>     (v1:c')
       !> ------------------------------
       !>     v1   _y
       !>            !> _c1    c
       !>       /
       !>     c'
       block
         type(t2_i32_i32) :: key_tmp
         integer(int32) :: val_tmp
         type(btree_node_ptr_t2_i32_i32_to_int32) :: next
         next%p_ => x%p_%children_(pos+1)%p_
         do while (.not. next%is_leaf())
            next%p_ => next%p_%children_(1)%p_
         end do
         key_tmp = next%p_%key_(1)
         val_tmp = next%p_%val_(1)
         ! write(error_unit, '(a, *(i0, 1x))') "next: ", key, key_tmp
         call x%remove(key_tmp)
         x%p_%key_(pos) = key_tmp
         x%p_%val_(pos) = val_tmp
         ! write(error_unit, '(a, 2(i0, 1x))') "pattern 2b end: ", key, key_tmp
         return
       end block
    else !> left and right children have  keys.
       ! write(error_unit, '(a)') "pattern 2c"
       !>   x                _y
       !> c   c2(deallocate)    _c3
       !> --------------------------------
       !>            _y
       !> (c//x//c2)    _c3
       call x%merge_children(pos)
       call x%p_%children_(pos)%remove(key)
       ! write(error_unit, '(a, i0)') "pattern 2c end: ", key
       return
    end if
  end subroutine remove_key_btree_node_ptr_t2_i32_i32_to_int32
  !> merge_btree_node_ptr_t2_i32_i32_to_int32: Merge left child, middle key and right child.
  !> Then shrink left and deallocate right child.
  !>      x                   _y
  !> left   right(deallocate)    _c
  !> --------------------------------
  !>                  _y
  !> (left//x//right)    _c
  subroutine merge_children_btree_node_ptr_t2_i32_i32_to_int32(x, pos)
    class(btree_node_ptr_t2_i32_i32_to_int32), intent(in) :: x
    integer(int32), intent(in) :: pos
    type(btree_node_ptr_t2_i32_i32_to_int32) :: left, right
    integer(int32) :: i
    left%p_  => x%p_%children_(pos)%p_
    right%p_ => x%p_%children_(pos+1)%p_
  
    left%p_%key_(t) = x%p_%key_(pos)
    left%p_%val_(t) = x%p_%val_(pos)
    left%p_%key_(t+1:2*t-1) = right%p_%key_(1:t-1)
    left%p_%val_(t+1:2*t-1) = right%p_%val_(1:t-1)
    do i = t+1, 2*t
       left%p_%children_(i)%p_ => right%p_%children_(i-t)%p_
    end do
    left%p_%nelem_ = 2*t - 1
    deallocate(right%p_)
    nullify(right%p_)
    call x%shrink_left(pos, pos+1) ! unlink right child.
  end subroutine merge_children_btree_node_ptr_t2_i32_i32_to_int32
  !> rotate_left_btree_node_ptr_t2_i32_i32_to_int32: Rotate keys.
  !> Increase the number of left node keys and decrease that of right node keys.
  !> The number of right node keys must have at least  keys.
  !> x:     key
  !>      /     !> left         right(v1:rest)
  !> ------------------------------
  !> x:           v1
  !>            /    !> (left:key)        rest
  subroutine rotate_left_btree_node_ptr_t2_i32_i32_to_int32(x, pos)
    class(btree_node_ptr_t2_i32_i32_to_int32), intent(in) :: x
    integer(int32), intent(in) :: pos
    type(btree_node_ptr_t2_i32_i32_to_int32) :: left, right
    integer(int32) :: ls
    left%p_  => x%p_%children_(pos)%p_
    right%p_ => x%p_%children_(pos+1)%p_
    ls = left%size() + 1
    left%p_%key_(ls) = x%p_%key_(pos)
    left%p_%val_(ls) = x%p_%val_(pos)
    left%p_%children_(ls+1)%p_ => right%p_%children_(1)%p_
    left%p_%nelem_ = ls
    x%p_%key_(pos) = right%p_%key_(1)
    x%p_%val_(pos) = right%p_%val_(1)
    call right%shrink_left(1, 1) !> right%size() -= 1
  end subroutine rotate_left_btree_node_ptr_t2_i32_i32_to_int32
  !> rotate_right_btree_node_ptr_t2_i32_i32_to_int32: Rotate keys.
  !> Increase the number of right node keys and decrease that of left node keys.
  !> The number of left node keys must have at least  keys.
  !> x:              key
  !>               /     !> left(init:v1)         right
  !> ------------------------------
  !> x:       v1
  !>        /    !>   init        (key:right)
  subroutine rotate_right_btree_node_ptr_t2_i32_i32_to_int32(x, pos)
    class(btree_node_ptr_t2_i32_i32_to_int32), intent(in) :: x
    integer(int32), intent(in) :: pos
    type(btree_node_ptr_t2_i32_i32_to_int32) :: left, right
    left%p_  => x%p_%children_(pos)%p_
    right%p_ => x%p_%children_(pos+1)%p_
    call right%expand_right(1) !> right%size() += 1
    right%p_%key_(1) = x%p_%key_(pos)
    right%p_%val_(1) = x%p_%val_(pos)
    right%p_%children_(1)%p_ => left%p_%children_(left%size() + 1)%p_
    x%p_%key_(pos) = left%p_%key_(left%size())
    x%p_%val_(pos) = left%p_%val_(left%size())
    call left%shrink_left(left%size(), left%size()+1) !> unlink right child.
  end subroutine rotate_right_btree_node_ptr_t2_i32_i32_to_int32
  !> shrink_left_btree_node_ptr_t2_i32_i32_to_int32: Remove the  and  from the  of  and shrink it.
  !> before: key(1), key(2), ... key(pos-1), key(pos), key(pos+1), ..., key(s)
  !> after : key(1), key(2), ... key(pos-1), key(pos+1), ..., key(s)
  !> before: child(1), child(2), ... child(pos_child-1), child(pos_child), child(pos_child+1), ..., child(s+1)
  !> after : child(1), child(2), ... child(pos_child-1),                   child(pos_child+1), ..., child(s+1)
  subroutine shrink_left_btree_node_ptr_t2_i32_i32_to_int32(this, pos, pos_child)
    class(btree_node_ptr_t2_i32_i32_to_int32), intent(in) :: this
    integer(int32), intent(in) :: pos, pos_child
    type(btree_node_ptr_t2_i32_i32_to_int32) :: x
    integer(int32) :: s
    integer(int32) :: i
    x%p_ => this%p_
    s = x%size()
    !> copy [pos+1, s] to [pos, s-1].
    !> delete  of array.
    x%p_%key_(pos:s-1) = x%p_%key_(pos+1:s)
    x%p_%val_(pos:s-1) = x%p_%val_(pos+1:s)
    x%p_%nelem_ = s - 1
    if (x%is_leaf()) return
    do i = pos_child, s
       x%p_%children_(i)%p_ => x%p_%children_(i+1)%p_
    end do
  end subroutine shrink_left_btree_node_ptr_t2_i32_i32_to_int32
  subroutine expand_right_btree_node_ptr_t2_i32_i32_to_int32(this, pos)
    class(btree_node_ptr_t2_i32_i32_to_int32), intent(in) :: this
    integer(int32), intent(in) :: pos
    type(btree_node_ptr_t2_i32_i32_to_int32) :: x
    integer(int32) :: s
    integer(int32) :: i
    x%p_ => this%p_
    s = x%size()
    !> copy [pos, s] to [pos+1, s+1].
    !>  of array is empty.
    x%p_%key_(pos+1:s+1) = x%p_%key_(pos:s)
    x%p_%val_(pos+1:s+1) = x%p_%val_(pos:s)
    x%p_%nelem_ = s + 1
    if (x%is_leaf()) return
    do i = s+1, pos, -1
       x%p_%children_(i+1)%p_ => x%p_%children_(i)%p_
    end do
  end subroutine expand_right_btree_node_ptr_t2_i32_i32_to_int32
  recursive subroutine print_btree_node_ptr_t2_i32_i32_to_int32(this, unit, depth)
    class(btree_node_ptr_t2_i32_i32_to_int32), intent(in) :: this
    integer(int32), intent(in) :: unit, depth
    type(btree_node_ptr_t2_i32_i32_to_int32) :: x
    integer(int32) :: i
    x%p_ => this%p_
    write(unit, *) repeat("|", min(1, depth))//repeat("-", depth), depth, ": ", x%p_%key_(1:x%size())
    if (x%is_leaf()) return
    do i = 1, x%p_%nelem_ + 1
       call x%p_%children_(i)%print(unit, depth + 1)
    end do
  end subroutine print_btree_node_ptr_t2_i32_i32_to_int32
  recursive subroutine check_invariant_btree_node_ptr_t2_i32_i32_to_int32(this)
    class(btree_node_ptr_t2_i32_i32_to_int32), intent(in) :: this
    integer(int32) :: i
    if (this%size() < t - 1) then
       write(error_unit, '(a)') "Error: invariant, node must have at least  keys."
       error stop 1
    end if
    if (this%is_leaf()) return
    do i = 1, this%size() + 1
       call this%p_%children_(i)%check_invariant()
    end do
  end subroutine check_invariant_btree_node_ptr_t2_i32_i32_to_int32
  impure type(t2_i32_i32) function key_btree_node_iter_t2_i32_i32_to_int32(this) result(res)
    class(btree_node_iter_t2_i32_i32_to_int32), intent(in) :: this
    res = this%nptr_%p_%key_(this%idx_)
  end function key_btree_node_iter_t2_i32_i32_to_int32
  impure integer(int32) function val_btree_node_iter_t2_i32_i32_to_int32(this) result(res)
    class(btree_node_iter_t2_i32_i32_to_int32), intent(in) :: this
    res = this%nptr_%p_%val_(this%idx_)
  end function val_btree_node_iter_t2_i32_i32_to_int32
  subroutine mut_val_btree_node_iter_t2_i32_i32_to_int32(this, val)
    class(btree_node_iter_t2_i32_i32_to_int32), intent(in) :: this
    integer(int32), intent(in) :: val
    type(btree_node_ptr_t2_i32_i32_to_int32) :: x
    x%p_ => this%nptr_%p_
    x%p_%val_(this%idx_) = val
  end subroutine mut_val_btree_node_iter_t2_i32_i32_to_int32
  subroutine next_btree_node_iter_t2_i32_i32_to_int32(this)
    class(btree_node_iter_t2_i32_i32_to_int32), intent(inout) :: this
    if (this%is_end()) then
       write(error_unit, '(a)') "Error in : exceed end of iterator."
       error stop 4
    end if
    if (this%nptr_%is_leaf()) then
       this%idx_ = this%idx_ + 1
       if (this%idx_ <= this%nptr_%size()) return
       !> this%idx_ == this%nptr_%size() + 1.
       do !> visit parent of current node if  exceeds the range of , where s == this%nptr_%size().
          if (this%depth_ == 1) return !> end of iterator if  and  is root of B-tree.
          this%depth_ = this%depth_ - 1
          this%nptr_%p_ => this%parents_(this%depth_)%p_
          this%idx_ = this%indices_(this%depth_)
          nullify(this%parents_(this%depth_)%p_)
          if (this%idx_ <= this%nptr_%size()) return !> this%idx_: [1:s], where s == this%nptr_%size().
       end do
    else !> visit right node and then visit the most left value.
       this%parents_(this%depth_)%p_ => this%nptr_%p_
       this%indices_(this%depth_) = this%idx_ + 1
       this%depth_ = this%depth_ + 1
       this%nptr_%p_ => this%nptr_%p_%children_(this%idx_ + 1)%p_
       do while (.not. this%nptr_%is_leaf())
          this%parents_(this%depth_)%p_ => this%nptr_%p_
          this%indices_(this%depth_) = 1
          this%depth_ = this%depth_ + 1
          this%nptr_%p_ => this%nptr_%p_%children_(1)%p_
       end do
       !> this%nptr_%is_leaf() is .true..
       this%idx_ = 1
    end if
  end subroutine next_btree_node_iter_t2_i32_i32_to_int32
  subroutine prev_btree_node_iter_t2_i32_i32_to_int32(this)
    class(btree_node_iter_t2_i32_i32_to_int32), intent(inout) :: this
    if (this%is_begin()) then !>  and  is root of B-tree.
       !> beginning of iterator.
       write(error_unit, '(a)') "Error in : beginning of iterator."
       error stop 4
    end if
    if (this%nptr_%is_leaf()) then
       this%idx_ = this%idx_ - 1
       if (this%idx_ >= 1) return
       !> this%idx_ == 0
       do !> visit parent of current node if  exceeds the range of , where s == this%nptr_%size().
          if (this%depth_ == 1) return !> beginning of iterator if  and  is root of B-tree.
          this%depth_ = this%depth_ - 1
          this%nptr_%p_ => this%parents_(this%depth_)%p_
          nullify(this%parents_(this%depth_)%p_)
          this%idx_ = this%indices_(this%depth_) - 1
          if (this%idx_ >= 1) return !> this%idx_: [1:s], where s == this%nptr_%size().
       end do
    else !> visit left node and then visit the most right value.
       this%parents_(this%depth_)%p_ => this%nptr_%p_
       this%indices_(this%depth_) = this%idx_
       this%depth_ = this%depth_ + 1
       this%nptr_%p_ => this%nptr_%p_%children_(this%idx_)%p_
       do while (.not. this%nptr_%is_leaf())
          this%parents_(this%depth_)%p_ => this%nptr_%p_
          this%indices_(this%depth_) = this%nptr_%size() + 1
          this%depth_ = this%depth_ + 1
          this%nptr_%p_ => this%nptr_%p_%children_(this%nptr_%size() + 1)%p_
       end do
       !> this%nptr_%is_leaf() is .true.
       this%idx_ = this%nptr_%size()
    end if
  end subroutine prev_btree_node_iter_t2_i32_i32_to_int32
  !> is_begin_btree_node_iter_t2_i32_i32_to_int32: return iter is begining of B-tree.
  logical function is_begin_btree_node_iter_t2_i32_i32_to_int32(this) result(res)
    class(btree_node_iter_t2_i32_i32_to_int32), intent(in) :: this
    res = this%depth_ == 1 .and. this%idx_ == 0
  end function is_begin_btree_node_iter_t2_i32_i32_to_int32
  !> is_not_begin_btree_node_iter_t2_i32_i32_to_int32: return iter is not begining of B-tree.
  logical function is_not_begin_btree_node_iter_t2_i32_i32_to_int32(this) result(res)
    class(btree_node_iter_t2_i32_i32_to_int32), intent(in) :: this
    res = .not. this%is_begin()
  end function is_not_begin_btree_node_iter_t2_i32_i32_to_int32
  !> is_end_btree_node_iter_t2_i32_i32_to_int32: return iter is end of B-tree.
  logical function is_end_btree_node_iter_t2_i32_i32_to_int32(this) result(res)
    class(btree_node_iter_t2_i32_i32_to_int32), intent(in) :: this
    res = this%depth_ == 1 .and. this%idx_ == this%nptr_%size() + 1
  end function is_end_btree_node_iter_t2_i32_i32_to_int32
  !> is_not_end_btree_node_iter_t2_i32_i32_to_int32: return iter is not end of B-tree.
  logical function is_not_end_btree_node_iter_t2_i32_i32_to_int32(this) result(res)
    class(btree_node_iter_t2_i32_i32_to_int32), intent(in) :: this
    res = .not. this%is_end()
  end function is_not_end_btree_node_iter_t2_i32_i32_to_int32
  logical function exist_btree_node_iter_t2_i32_i32_to_int32(this) result(res)
    class(btree_node_iter_t2_i32_i32_to_int32), intent(in) :: this
    res = this%idx_ /= -1
  end function exist_btree_node_iter_t2_i32_i32_to_int32
  logical function not_exist_btree_node_iter_t2_i32_i32_to_int32(this) result(res)
    class(btree_node_iter_t2_i32_i32_to_int32), intent(in) :: this
    res = .not. this%exist()
  end function not_exist_btree_node_iter_t2_i32_i32_to_int32
  
  subroutine init_btree_t2_i64_i64_to_int64(this)
    class(btree_t2_i64_i64_to_int64), intent(inout) :: this
    type(btree_node_t2_i64_i64_to_int64), pointer :: x
    allocate(x)
    x%is_leaf_ = .true.
    x%nelem_ = 0
    this%size_   = 0
    this%height_ = 0
    this%root_%p_ => x
  end subroutine init_btree_t2_i64_i64_to_int64
  pure integer(int32) function size_btree_t2_i64_i64_to_int64(this) result(res)
    class(btree_t2_i64_i64_to_int64), intent(in) :: this
    res = this%size_
  end function size_btree_t2_i64_i64_to_int64
  pure integer(int32) function height_btree_t2_i64_i64_to_int64(this) result(res)
    class(btree_t2_i64_i64_to_int64), intent(in) :: this
    res = this%height_
  end function height_btree_t2_i64_i64_to_int64
  integer(int64) function get_btree_t2_i64_i64_to_int64(this, key) result(res)
    class(btree_t2_i64_i64_to_int64), intent(in) :: this
    type(t2_i64_i64), intent(in) :: key
    type(btree_node_iter_t2_i64_i64_to_int64) :: iter
    iter = this%root_%get_iter(key)
    if (iter%idx_ /= -1) then
       res = iter%nptr_%p_%val_(iter%idx_)
    else
    end if
  end function get_btree_t2_i64_i64_to_int64
  type(btree_node_iter_t2_i64_i64_to_int64) function get_iter_btree_t2_i64_i64_to_int64(this, key) result(res)
    class(btree_t2_i64_i64_to_int64), intent(in) :: this
    type(t2_i64_i64), intent(in) :: key
    res = this%root_%get_iter(key)
  end function get_iter_btree_t2_i64_i64_to_int64
  logical function contains_btree_t2_i64_i64_to_int64(this, key) result(res)
    class(btree_t2_i64_i64_to_int64), intent(in) :: this
    type(t2_i64_i64), intent(in) :: key
    type(btree_node_iter_t2_i64_i64_to_int64) :: iter
    iter = this%root_%get_iter(key)
    res = iter%idx_ /= -1
  end function contains_btree_t2_i64_i64_to_int64
  subroutine insert_btree_t2_i64_i64_to_int64(this, key, val)
    class(btree_t2_i64_i64_to_int64), intent(inout) :: this
    type(t2_i64_i64), intent(in) :: key
    integer(int64), intent(in) :: val
    type(btree_node_ptr_t2_i64_i64_to_int64) :: r
    type(btree_node_iter_t2_i64_i64_to_int64) :: iter
    r%p_ => this%root_%p_
    if (r%p_%nelem_ == 2*t - 1) then
       block
         type(btree_node_ptr_t2_i64_i64_to_int64) :: s
         allocate(s%p_)
         this%root_%p_ => s%p_
         s%p_%is_leaf_ = .false.
         s%p_%nelem_ = 0
         s%p_%children_(1)%p_ => r%p_
         call s%split_child(1)
         this%height_ = this%height_ + 1
         iter = s%insert(key, val)
       end block
    else
       iter = r%insert(key, val)
    end if
    if (iter%idx_ > 0) &
         this%size_ = this%size_ + 1
  end subroutine insert_btree_t2_i64_i64_to_int64
  subroutine remove_btree_t2_i64_i64_to_int64(this, key)
    class(btree_t2_i64_i64_to_int64), intent(inout) :: this
    type(t2_i64_i64), intent(in) :: key
    type(btree_node_ptr_t2_i64_i64_to_int64) :: tmp
    call this%root_%remove(key)
    if (this%root_%p_%nelem_ == 0 .and. (.not. this%root_%is_leaf())) then
       tmp%p_ => this%root_%p_
       this%root_%p_ => this%root_%p_%children_(1)%p_
       deallocate(tmp%p_)
       nullify(tmp%p_)
       this%height_ = this%height_ - 1
    end if
    this%size_ = this%size_ - 1
  end subroutine remove_btree_t2_i64_i64_to_int64
  !> minimum_btree_t2_i64_i64_to_int64: Return the minimum value.
  type(t2_i64_i64) function minimum_btree_t2_i64_i64_to_int64(this) result(res)
    class(btree_t2_i64_i64_to_int64), intent(in) :: this
    type(btree_node_iter_t2_i64_i64_to_int64) :: iter
    iter = this%minimum_iter()
    res = iter%key()
  end function minimum_btree_t2_i64_i64_to_int64
  !> minimum_iter_btree_t2_i64_i64_to_int64: Return the iterator to node that has minimum key.
  type(btree_node_iter_t2_i64_i64_to_int64) function minimum_iter_btree_t2_i64_i64_to_int64(this) result(res)
    class(btree_t2_i64_i64_to_int64), intent(in) :: this
    res%nptr_%p_ => this%root_%p_
    res%depth_ = 1
    res%idx_ = 0
    call res%next()
  end function minimum_iter_btree_t2_i64_i64_to_int64
  !> maximum_btree_t2_i64_i64_to_int64: Return the maximum value.
  type(t2_i64_i64) function maximum_btree_t2_i64_i64_to_int64(this) result(res)
    class(btree_t2_i64_i64_to_int64), intent(in) :: this
    type(btree_node_iter_t2_i64_i64_to_int64) :: iter
    iter = this%maximum_iter()
    res = iter%key()
  end function maximum_btree_t2_i64_i64_to_int64
  !> maximum_iter_btree_t2_i64_i64_to_int64: Return the iterator to node that has maximum key.
  type(btree_node_iter_t2_i64_i64_to_int64) function maximum_iter_btree_t2_i64_i64_to_int64(this) result(res)
    class(btree_t2_i64_i64_to_int64), intent(in) :: this
    res%nptr_%p_ => this%root_%p_
    res%depth_ = 1
    res%idx_ = res%nptr_%size() + 1
    call res%prev()
  end function maximum_iter_btree_t2_i64_i64_to_int64
  !> print_btree: Print whole node in B-tree for debug.
  !> For debug.
  subroutine print_btree_t2_i64_i64_to_int64(this, unit)
    class(btree_t2_i64_i64_to_int64), intent(in) :: this
    integer(int32), intent(in) :: unit
    if (associated(this%root_%p_)) &
         call this%root_%print(unit, 0)
  end subroutine print_btree_t2_i64_i64_to_int64
  !> check_invariant_btree_t2_i64_i64_to_int64: Check invariant for debug.
  !> invariant condition: The number of keys of each node in B-tree excluded root node must have at least  keys.
  !> The keys in left children is less than key of current node.
  !> The keys in right children is greater than key of current node.
  subroutine check_invariant_btree_t2_i64_i64_to_int64(this)
    class(btree_t2_i64_i64_to_int64), intent(in) :: this
    type(btree_node_iter_t2_i64_i64_to_int64) :: bt_iter
    type(t2_i64_i64) :: k, k_bef
    integer(int32) :: i
    if (this%size() == 0) return
    bt_iter = this%minimum_iter()
    k_bef = bt_iter%key()
    call bt_iter%next()
    do while (bt_iter%is_not_end())
       k = bt_iter%key()
       ! write(error_unit, *) k_bef, k
       if (k_bef >= k) then
          write(error_unit, '(a)') "Error: B-tree is not ordered."
          write(error_unit, '(a)') "Something wrong occurred in 'minimum_iter' or 'next'."
          error stop 5
       end if
       k_bef = k
       call bt_iter%next()
    end do
    bt_iter = this%maximum_iter()
    k_bef = bt_iter%key()
    call bt_iter%prev()
    do while (bt_iter%is_not_begin())
       k = bt_iter%key()
       ! write(error_unit, *) k_bef, k
       if (k_bef <= k) then
          write(error_unit, '(a)') "Error: B-tree is not ordered."
          write(error_unit, '(a)') "Something wrong occurred in 'maximum_iter' or 'prev'."
          error stop 6
       end if
       k_bef = k
       call bt_iter%prev()
    end do
    if (this%root_%is_leaf()) return
    do i = 1, this%root_%size() + 1
       call this%root_%p_%children_(i)%check_invariant()
    end do
  end subroutine check_invariant_btree_t2_i64_i64_to_int64
  pure integer(int32) function size_btree_node_ptr_t2_i64_i64_to_int64(this) result(res)
    class(btree_node_ptr_t2_i64_i64_to_int64), intent(in) :: this
    res = this%p_%nelem_
  end function size_btree_node_ptr_t2_i64_i64_to_int64
  pure logical function is_leaf_btree_node_ptr_t2_i64_i64_to_int64(this) result(res)
    class(btree_node_ptr_t2_i64_i64_to_int64), intent(in) :: this
    res = this%p_%is_leaf_
  end function is_leaf_btree_node_ptr_t2_i64_i64_to_int64
  type(btree_node_iter_t2_i64_i64_to_int64) function get_iter_btree_node_ptr_t2_i64_i64_to_int64(this, key) result(res)
    class(btree_node_ptr_t2_i64_i64_to_int64), intent(in) :: this
    type(t2_i64_i64), intent(in) :: key
    integer(int32) :: pos
    res%nptr_%p_ => this%p_
    if (res%nptr_%size() == 0) then
       res%idx_ = -1
       return
    end if
    res%depth_ = 1
    do !> search , which satisfied arr(pos) < key <= arr(pos+1), arr(0) == -infinity, arr(n+1) == +infinity
       pos = lower_bound(1, res%nptr_%size(), res%nptr_%p_%key_(1:res%nptr_%size()), key)
       ! write(error_unit, '(3(a, i0, 1x), *(i0, 1x))') "pos: ", pos, "key: ", key, "arr: ", res%nptr_%p_%key_(1:res%nptr_%size())
       !> key <= key_(pos)
       if (pos <= res%nptr_%size()) then
          if (res%nptr_%p_%key_(pos) == key) then !> key found.
             res%idx_ = pos
             return
          end if
       end if
       if (res%nptr_%is_leaf()) exit
       res%indices_(res%depth_) = pos
       res%parents_(res%depth_)%p_ => res%nptr_%p_
       res%nptr_%p_ => res%nptr_%p_%children_(pos)%p_
       res%depth_ = res%depth_ + 1
    end do
    !> not found.
    nullify(res%nptr_%p_)
    res%idx_ = -1
    return
  contains
    !> lower_bound: search , which satisfied arr(i) < key <= arr(i+1), arr(0) == -infinity.
    pure integer(int32) function lower_bound(lb, ub, arr, key) result(res)
      integer(int32), intent(in) :: lb, ub
      type(t2_i64_i64), intent(in) :: arr(lb:ub)
      type(t2_i64_i64), intent(in) :: key
      integer(int32) :: p, q, r
      p = lb
      r = ub
      if (key <= arr(p)) then
         res = p
      else if (arr(r) < key) then
         res = r + 1
      else !> arr(p) < key <= arr(r)
         ! invariant condition:
         ! key > arr(p) .and. key <= arr(r)
         binary_search: do while(p + 1 < r)
            q = (p+r) / 2
            if (arr(q) < key) then
               p = q
            else !> key <= arr(q)
               r = q
            end if
         end do binary_search
         res = r
      end if
    end function lower_bound
  end function get_iter_btree_node_ptr_t2_i64_i64_to_int64
  subroutine split_child_btree_node_ptr_t2_i64_i64_to_int64(this, idx)
    class(btree_node_ptr_t2_i64_i64_to_int64), intent(in) :: this
    integer(int32), intent(in) :: idx
    type(btree_node_ptr_t2_i64_i64_to_int64) :: y, z
    integer(int32) :: i
    allocate(z%p_)
    y%p_ => this%p_%children_(idx)%p_
    z%p_%is_leaf_ = y%p_%is_leaf_
    z%p_%nelem_ = t - 1
    do i = 1, t - 1
       z%p_%key_(i) = y%p_%key_(i+t)
       z%p_%val_(i) = y%p_%val_(i+t)
    end do
    if (.not. y%is_leaf()) then
       do i = 1, t
          z%p_%children_(i)%p_ => y%p_%children_(i+t)%p_
       end do
    end if
    y%p_%nelem_ = t - 1
    do i = this%size()+1, idx+1, -1
       this%p_%children_(i+1)%p_ => this%p_%children_(i)%p_
    end do
    this%p_%children_(idx+1)%p_ => z%p_
    do i = this%p_%nelem_, idx, -1
       this%p_%key_(i+1) = this%p_%key_(i)
       this%p_%val_(i+1) = this%p_%val_(i)
    end do
    this%p_%key_(idx) = y%p_%key_(t)
    this%p_%val_(idx) = y%p_%val_(t)
    this%p_%nelem_ = this%p_%nelem_ + 1
  end subroutine split_child_btree_node_ptr_t2_i64_i64_to_int64
  !> insert_btree_node_ptr_t2_i64_i64_to_int64: Insert (, ) into B-tree.
  type(btree_node_iter_t2_i64_i64_to_int64) function insert_btree_node_ptr_t2_i64_i64_to_int64(this, key, val) result(res)
    class(btree_node_ptr_t2_i64_i64_to_int64), intent(in) :: this
    type(t2_i64_i64), intent(in) :: key
    integer(int64), intent(in) :: val
    type(btree_node_ptr_t2_i64_i64_to_int64) :: x
    integer(int32) :: pos
    x%p_ => this%p_
    if (x%size() == 0) then
       x%p_%key_(1) = key
       x%p_%val_(1) = val
       x%p_%nelem_ = 1
       res%nptr_%p_ => x%p_
       res%idx_ = 1
       return
    end if
    ! write(error_unit, '(L)') x%p_%is_leaf_
    do while (.not. x%is_leaf())
       pos = lower_bound(1, x%size(), x%p_%key_(1:x%size()), key)
       if (x%p_%children_(pos)%size() == 2*t - 1) then
          call x%split_child(pos)
          if (key > x%p_%key_(pos)) pos = pos + 1
       end if
       if (pos <= x%size()) then
          if (key == x%p_%key_(pos)) then
             nullify(res%nptr_%p_)
             res%idx_ = -1
             return
          end if
       end if
       x%p_ => x%p_%children_(pos)%p_
    end do
    pos = lower_bound(1, x%size(), x%p_%key_(1:x%size()), key)
    if (pos <= x%size()) then !>  <= , where s == x%size().
       if (key == x%p_%key_(pos)) then !>  already exists in B-tree.
          nullify(res%nptr_%p_)
          res%idx_ = -1
          return
       else !> expand for insertion.
          call x%expand_right(pos)
       end if
    else !>  > , where s == x%size().
       x%p_%nelem_ = x%p_%nelem_ + 1
    end if
    ! write(error_unit, '(a, i0, 2(1x, i0))') "insert: ", pos+1, key, x%p_%key_(pos+1)
    x%p_%key_(pos) = key
    x%p_%val_(pos) = val
    res%nptr_%p_ => x%p_
    res%idx_ = pos
  contains
    !> lower_bound: search , which satisfied arr(i) < key <= arr(i+1), arr(0) == -infinity.
    pure integer(int32) function lower_bound(lb, ub, arr, key) result(res)
      integer(int32), intent(in) :: lb, ub
      type(t2_i64_i64), intent(in) :: arr(lb:ub)
      type(t2_i64_i64), intent(in) :: key
      integer(int32) :: p, q, r
      p = lb
      r = ub
      if (key <= arr(p)) then
         res = p
      else if (arr(r) < key) then
         res = r + 1
      else !> arr(p) < key <= arr(r)
         ! invariant condition:
         ! key > arr(p) .and. key <= arr(r)
         binary_search: do while(p + 1 < r)
            q = (p+r) / 2
            if (arr(q) < key) then
               p = q
            else !> key <= arr(q)
               r = q
            end if
         end do binary_search
         res = r
      end if
    end function lower_bound
  end function insert_btree_node_ptr_t2_i64_i64_to_int64
  !> remove_btree_node_ptr_t2_i64_i64_to_int64: Remove  from B-tree.
  !> invariant condition: the node  has at least  keys.
  recursive subroutine remove_btree_node_ptr_t2_i64_i64_to_int64(this, key)
    class(btree_node_ptr_t2_i64_i64_to_int64), intent(in) :: this
    type(t2_i64_i64), intent(in) :: key
    type(btree_node_ptr_t2_i64_i64_to_int64) :: x, c
    integer(int32) :: pos, s
    ! write(error_unit, '(a, i0)') "search: ", key
    x%p_ => this%p_
    pos = lower_bound(1, x%size(), x%p_%key_(1:x%size()), key)
    if (x%p_%key_(min(pos, x%size())) == key) then !>  exists in current internal node.
       call x%remove_key(key, pos)
       return
    end if
    !>  does not exist in current internal node.
    if (x%is_leaf()) then
       write(error_unit, '(a, i0, a)') "key: ", key, " is not found."
       call this%print(error_unit, 0)
       error stop 2
       return
    end if
    c%p_ => x%p_%children_(pos)%p_
    if (c%size() >= t) then
       ! write(error_unit, '(a)') "pattern 3, recursive remove"
       ! remove  recurrently.
       call c%remove(key)
       ! write(error_unit, '(a, i0)') "pattern 3 end: ", key
       return
    end if
    !> size of child has  keys.
    s = x%size()
    if (pos == s + 1) then
       if (x%p_%children_(s)%size() == t - 1) then
          !> x: _w      key (== x(s))
          !>      \    /           !>       c_to     c_from
          !> ------------------------------
          !> x: _w
          !>               !>         (c_to//key//c_from)
          call x%merge_children(s)
          call x%p_%children_(x%size() + 1)%remove(key)
          ! write(error_unit, '(a, i0)') "pattern 3b end: ", key
          return
       end if
    else !> pos: [1, s]
       if (x%p_%children_(pos+1)%size() == t - 1) then
          !>        key            _w
          !>      /     \        /            !> c_to         c_from        c3
          !> ------------------------------
          !>                    _w
          !>                   /           !> c(c_to//x//c_from)      c3
          call x%merge_children(pos)
          call x%p_%children_(pos)%remove(key)
          ! write(error_unit, '(a, i0)') "pattern 3b end: ", key
          return
       end if
    end if
    !> left or right child have n (>= t) keys.
    ! write(error_unit, '(a, i0)') "pattern 3a: ", key
    if (pos == s + 1) then
       ! write(error_unit, '(a, i0)') "pattern 3a-1: ", key
       !> x:            key
       !>             /          !> (c_from:v1)         c_to
       !> ------------------------------
       !> x:         v1
       !>          /         !>   c_from        (x:c_to)
       call x%rotate_right(s)
       call x%p_%children_(x%size() + 1)%remove(key)
    else !> pos: [1, s]
       ! write(error_unit, '(a, i0)') "pattern 3a-2: ", key
       !>      x
       !> c_to   (v1:c_from)
       !> ------------------------------
       !>          v1
       !> (c_to:x)    c_from
       call x%rotate_left(pos)
       call x%p_%children_(pos)%remove(key)
    end if
    ! write(error_unit, '(a, i0)') "pattern 3a end: ", key
    return
  contains
    !> lower_bound: search , which satisfied arr(i) < key <= arr(i+1), arr(0) == -infinity.
    pure integer(int32) function lower_bound(lb, ub, arr, key) result(res)
      integer(int32), intent(in) :: lb, ub
      type(t2_i64_i64), intent(in) :: arr(lb:ub)
      type(t2_i64_i64), intent(in) :: key
      integer(int32) :: p, q, r
      p = lb
      r = ub
      if (key <= arr(p)) then
         res = p
      else if (arr(r) < key) then
         res = r + 1
      else !> arr(p) < key <= arr(r)
         ! invariant condition:
         ! key > arr(p) .and. key <= arr(r)
         binary_search: do while(p + 1 < r)
            q = (p+r) / 2
            if (arr(q) < key) then
               p = q
            else !> key <= arr(q)
               r = q
            end if
         end do binary_search
         res = r
      end if
    end function lower_bound
  end subroutine remove_btree_node_ptr_t2_i64_i64_to_int64
  !> remove_key_btree_node_ptr_t2_i64_i64_to_int64: If some of current nodes have , call this.
  recursive subroutine remove_key_btree_node_ptr_t2_i64_i64_to_int64(x, key, pos)
    class(btree_node_ptr_t2_i64_i64_to_int64), intent(in) :: x
    type(t2_i64_i64), intent(in) :: key
    integer(int32), intent(in) :: pos
    !> pos: [1, x%size()].
    if (x%is_leaf()) then
       ! write(error_unit, '(a, i0)') "pattern 1: ", key
       call x%shrink_left(pos, pos)
       return
    end if
    !> x is not leaf.
    if (x%p_%children_(pos)%size() >= t) then
       !> Exchange previous  if left child has n (>= t) keys.
       ! write(error_unit, '(a)') "pattern 2a"
       !>   x             _y
       !>  /
       !> c           _c2
       !>       !>   (c':v1)
       !> ------------------------------
       !>   v1     _y
       !>  /
       !> c    _c2
       !>       !>   c'
       block
         type(t2_i64_i64) :: key_tmp
         integer(int64) :: val_tmp
         type(btree_node_ptr_t2_i64_i64_to_int64) :: prev
         prev%p_ => x%p_%children_(pos)%p_
         do while (.not. prev%is_leaf())
            prev%p_ => prev%p_%children_(prev%size()+1)%p_
         end do
         key_tmp = prev%p_%key_(prev%size())
         val_tmp = prev%p_%val_(prev%size())
         ! write(error_unit, '(a, *(i0, 1x))') "prev: ", key_tmp, key
         call x%remove(key_tmp)
         x%p_%key_(pos) = key_tmp
         x%p_%val_(pos) = val_tmp
         ! write(error_unit, '(a, 2(i0, 1x))') "pattern 2a end: ", key, key_tmp
         return
       end block
    else if (x%p_%children_(pos+1)%size() >= t) then !> right child has n (>= t) keys.
       ! write(error_unit, '(a)') "pattern 2b"
       !>     x         _y
       !>                !> _c1         c
       !>            /
       !>     (v1:c')
       !> ------------------------------
       !>     v1   _y
       !>            !> _c1    c
       !>       /
       !>     c'
       block
         type(t2_i64_i64) :: key_tmp
         integer(int64) :: val_tmp
         type(btree_node_ptr_t2_i64_i64_to_int64) :: next
         next%p_ => x%p_%children_(pos+1)%p_
         do while (.not. next%is_leaf())
            next%p_ => next%p_%children_(1)%p_
         end do
         key_tmp = next%p_%key_(1)
         val_tmp = next%p_%val_(1)
         ! write(error_unit, '(a, *(i0, 1x))') "next: ", key, key_tmp
         call x%remove(key_tmp)
         x%p_%key_(pos) = key_tmp
         x%p_%val_(pos) = val_tmp
         ! write(error_unit, '(a, 2(i0, 1x))') "pattern 2b end: ", key, key_tmp
         return
       end block
    else !> left and right children have  keys.
       ! write(error_unit, '(a)') "pattern 2c"
       !>   x                _y
       !> c   c2(deallocate)    _c3
       !> --------------------------------
       !>            _y
       !> (c//x//c2)    _c3
       call x%merge_children(pos)
       call x%p_%children_(pos)%remove(key)
       ! write(error_unit, '(a, i0)') "pattern 2c end: ", key
       return
    end if
  end subroutine remove_key_btree_node_ptr_t2_i64_i64_to_int64
  !> merge_btree_node_ptr_t2_i64_i64_to_int64: Merge left child, middle key and right child.
  !> Then shrink left and deallocate right child.
  !>      x                   _y
  !> left   right(deallocate)    _c
  !> --------------------------------
  !>                  _y
  !> (left//x//right)    _c
  subroutine merge_children_btree_node_ptr_t2_i64_i64_to_int64(x, pos)
    class(btree_node_ptr_t2_i64_i64_to_int64), intent(in) :: x
    integer(int32), intent(in) :: pos
    type(btree_node_ptr_t2_i64_i64_to_int64) :: left, right
    integer(int32) :: i
    left%p_  => x%p_%children_(pos)%p_
    right%p_ => x%p_%children_(pos+1)%p_
  
    left%p_%key_(t) = x%p_%key_(pos)
    left%p_%val_(t) = x%p_%val_(pos)
    left%p_%key_(t+1:2*t-1) = right%p_%key_(1:t-1)
    left%p_%val_(t+1:2*t-1) = right%p_%val_(1:t-1)
    do i = t+1, 2*t
       left%p_%children_(i)%p_ => right%p_%children_(i-t)%p_
    end do
    left%p_%nelem_ = 2*t - 1
    deallocate(right%p_)
    nullify(right%p_)
    call x%shrink_left(pos, pos+1) ! unlink right child.
  end subroutine merge_children_btree_node_ptr_t2_i64_i64_to_int64
  !> rotate_left_btree_node_ptr_t2_i64_i64_to_int64: Rotate keys.
  !> Increase the number of left node keys and decrease that of right node keys.
  !> The number of right node keys must have at least  keys.
  !> x:     key
  !>      /     !> left         right(v1:rest)
  !> ------------------------------
  !> x:           v1
  !>            /    !> (left:key)        rest
  subroutine rotate_left_btree_node_ptr_t2_i64_i64_to_int64(x, pos)
    class(btree_node_ptr_t2_i64_i64_to_int64), intent(in) :: x
    integer(int32), intent(in) :: pos
    type(btree_node_ptr_t2_i64_i64_to_int64) :: left, right
    integer(int32) :: ls
    left%p_  => x%p_%children_(pos)%p_
    right%p_ => x%p_%children_(pos+1)%p_
    ls = left%size() + 1
    left%p_%key_(ls) = x%p_%key_(pos)
    left%p_%val_(ls) = x%p_%val_(pos)
    left%p_%children_(ls+1)%p_ => right%p_%children_(1)%p_
    left%p_%nelem_ = ls
    x%p_%key_(pos) = right%p_%key_(1)
    x%p_%val_(pos) = right%p_%val_(1)
    call right%shrink_left(1, 1) !> right%size() -= 1
  end subroutine rotate_left_btree_node_ptr_t2_i64_i64_to_int64
  !> rotate_right_btree_node_ptr_t2_i64_i64_to_int64: Rotate keys.
  !> Increase the number of right node keys and decrease that of left node keys.
  !> The number of left node keys must have at least  keys.
  !> x:              key
  !>               /     !> left(init:v1)         right
  !> ------------------------------
  !> x:       v1
  !>        /    !>   init        (key:right)
  subroutine rotate_right_btree_node_ptr_t2_i64_i64_to_int64(x, pos)
    class(btree_node_ptr_t2_i64_i64_to_int64), intent(in) :: x
    integer(int32), intent(in) :: pos
    type(btree_node_ptr_t2_i64_i64_to_int64) :: left, right
    left%p_  => x%p_%children_(pos)%p_
    right%p_ => x%p_%children_(pos+1)%p_
    call right%expand_right(1) !> right%size() += 1
    right%p_%key_(1) = x%p_%key_(pos)
    right%p_%val_(1) = x%p_%val_(pos)
    right%p_%children_(1)%p_ => left%p_%children_(left%size() + 1)%p_
    x%p_%key_(pos) = left%p_%key_(left%size())
    x%p_%val_(pos) = left%p_%val_(left%size())
    call left%shrink_left(left%size(), left%size()+1) !> unlink right child.
  end subroutine rotate_right_btree_node_ptr_t2_i64_i64_to_int64
  !> shrink_left_btree_node_ptr_t2_i64_i64_to_int64: Remove the  and  from the  of  and shrink it.
  !> before: key(1), key(2), ... key(pos-1), key(pos), key(pos+1), ..., key(s)
  !> after : key(1), key(2), ... key(pos-1), key(pos+1), ..., key(s)
  !> before: child(1), child(2), ... child(pos_child-1), child(pos_child), child(pos_child+1), ..., child(s+1)
  !> after : child(1), child(2), ... child(pos_child-1),                   child(pos_child+1), ..., child(s+1)
  subroutine shrink_left_btree_node_ptr_t2_i64_i64_to_int64(this, pos, pos_child)
    class(btree_node_ptr_t2_i64_i64_to_int64), intent(in) :: this
    integer(int32), intent(in) :: pos, pos_child
    type(btree_node_ptr_t2_i64_i64_to_int64) :: x
    integer(int32) :: s
    integer(int32) :: i
    x%p_ => this%p_
    s = x%size()
    !> copy [pos+1, s] to [pos, s-1].
    !> delete  of array.
    x%p_%key_(pos:s-1) = x%p_%key_(pos+1:s)
    x%p_%val_(pos:s-1) = x%p_%val_(pos+1:s)
    x%p_%nelem_ = s - 1
    if (x%is_leaf()) return
    do i = pos_child, s
       x%p_%children_(i)%p_ => x%p_%children_(i+1)%p_
    end do
  end subroutine shrink_left_btree_node_ptr_t2_i64_i64_to_int64
  subroutine expand_right_btree_node_ptr_t2_i64_i64_to_int64(this, pos)
    class(btree_node_ptr_t2_i64_i64_to_int64), intent(in) :: this
    integer(int32), intent(in) :: pos
    type(btree_node_ptr_t2_i64_i64_to_int64) :: x
    integer(int32) :: s
    integer(int32) :: i
    x%p_ => this%p_
    s = x%size()
    !> copy [pos, s] to [pos+1, s+1].
    !>  of array is empty.
    x%p_%key_(pos+1:s+1) = x%p_%key_(pos:s)
    x%p_%val_(pos+1:s+1) = x%p_%val_(pos:s)
    x%p_%nelem_ = s + 1
    if (x%is_leaf()) return
    do i = s+1, pos, -1
       x%p_%children_(i+1)%p_ => x%p_%children_(i)%p_
    end do
  end subroutine expand_right_btree_node_ptr_t2_i64_i64_to_int64
  recursive subroutine print_btree_node_ptr_t2_i64_i64_to_int64(this, unit, depth)
    class(btree_node_ptr_t2_i64_i64_to_int64), intent(in) :: this
    integer(int32), intent(in) :: unit, depth
    type(btree_node_ptr_t2_i64_i64_to_int64) :: x
    integer(int32) :: i
    x%p_ => this%p_
    write(unit, *) repeat("|", min(1, depth))//repeat("-", depth), depth, ": ", x%p_%key_(1:x%size())
    if (x%is_leaf()) return
    do i = 1, x%p_%nelem_ + 1
       call x%p_%children_(i)%print(unit, depth + 1)
    end do
  end subroutine print_btree_node_ptr_t2_i64_i64_to_int64
  recursive subroutine check_invariant_btree_node_ptr_t2_i64_i64_to_int64(this)
    class(btree_node_ptr_t2_i64_i64_to_int64), intent(in) :: this
    integer(int32) :: i
    if (this%size() < t - 1) then
       write(error_unit, '(a)') "Error: invariant, node must have at least  keys."
       error stop 1
    end if
    if (this%is_leaf()) return
    do i = 1, this%size() + 1
       call this%p_%children_(i)%check_invariant()
    end do
  end subroutine check_invariant_btree_node_ptr_t2_i64_i64_to_int64
  impure type(t2_i64_i64) function key_btree_node_iter_t2_i64_i64_to_int64(this) result(res)
    class(btree_node_iter_t2_i64_i64_to_int64), intent(in) :: this
    res = this%nptr_%p_%key_(this%idx_)
  end function key_btree_node_iter_t2_i64_i64_to_int64
  impure integer(int64) function val_btree_node_iter_t2_i64_i64_to_int64(this) result(res)
    class(btree_node_iter_t2_i64_i64_to_int64), intent(in) :: this
    res = this%nptr_%p_%val_(this%idx_)
  end function val_btree_node_iter_t2_i64_i64_to_int64
  subroutine mut_val_btree_node_iter_t2_i64_i64_to_int64(this, val)
    class(btree_node_iter_t2_i64_i64_to_int64), intent(in) :: this
    integer(int64), intent(in) :: val
    type(btree_node_ptr_t2_i64_i64_to_int64) :: x
    x%p_ => this%nptr_%p_
    x%p_%val_(this%idx_) = val
  end subroutine mut_val_btree_node_iter_t2_i64_i64_to_int64
  subroutine next_btree_node_iter_t2_i64_i64_to_int64(this)
    class(btree_node_iter_t2_i64_i64_to_int64), intent(inout) :: this
    if (this%is_end()) then
       write(error_unit, '(a)') "Error in : exceed end of iterator."
       error stop 4
    end if
    if (this%nptr_%is_leaf()) then
       this%idx_ = this%idx_ + 1
       if (this%idx_ <= this%nptr_%size()) return
       !> this%idx_ == this%nptr_%size() + 1.
       do !> visit parent of current node if  exceeds the range of , where s == this%nptr_%size().
          if (this%depth_ == 1) return !> end of iterator if  and  is root of B-tree.
          this%depth_ = this%depth_ - 1
          this%nptr_%p_ => this%parents_(this%depth_)%p_
          this%idx_ = this%indices_(this%depth_)
          nullify(this%parents_(this%depth_)%p_)
          if (this%idx_ <= this%nptr_%size()) return !> this%idx_: [1:s], where s == this%nptr_%size().
       end do
    else !> visit right node and then visit the most left value.
       this%parents_(this%depth_)%p_ => this%nptr_%p_
       this%indices_(this%depth_) = this%idx_ + 1
       this%depth_ = this%depth_ + 1
       this%nptr_%p_ => this%nptr_%p_%children_(this%idx_ + 1)%p_
       do while (.not. this%nptr_%is_leaf())
          this%parents_(this%depth_)%p_ => this%nptr_%p_
          this%indices_(this%depth_) = 1
          this%depth_ = this%depth_ + 1
          this%nptr_%p_ => this%nptr_%p_%children_(1)%p_
       end do
       !> this%nptr_%is_leaf() is .true..
       this%idx_ = 1
    end if
  end subroutine next_btree_node_iter_t2_i64_i64_to_int64
  subroutine prev_btree_node_iter_t2_i64_i64_to_int64(this)
    class(btree_node_iter_t2_i64_i64_to_int64), intent(inout) :: this
    if (this%is_begin()) then !>  and  is root of B-tree.
       !> beginning of iterator.
       write(error_unit, '(a)') "Error in : beginning of iterator."
       error stop 4
    end if
    if (this%nptr_%is_leaf()) then
       this%idx_ = this%idx_ - 1
       if (this%idx_ >= 1) return
       !> this%idx_ == 0
       do !> visit parent of current node if  exceeds the range of , where s == this%nptr_%size().
          if (this%depth_ == 1) return !> beginning of iterator if  and  is root of B-tree.
          this%depth_ = this%depth_ - 1
          this%nptr_%p_ => this%parents_(this%depth_)%p_
          nullify(this%parents_(this%depth_)%p_)
          this%idx_ = this%indices_(this%depth_) - 1
          if (this%idx_ >= 1) return !> this%idx_: [1:s], where s == this%nptr_%size().
       end do
    else !> visit left node and then visit the most right value.
       this%parents_(this%depth_)%p_ => this%nptr_%p_
       this%indices_(this%depth_) = this%idx_
       this%depth_ = this%depth_ + 1
       this%nptr_%p_ => this%nptr_%p_%children_(this%idx_)%p_
       do while (.not. this%nptr_%is_leaf())
          this%parents_(this%depth_)%p_ => this%nptr_%p_
          this%indices_(this%depth_) = this%nptr_%size() + 1
          this%depth_ = this%depth_ + 1
          this%nptr_%p_ => this%nptr_%p_%children_(this%nptr_%size() + 1)%p_
       end do
       !> this%nptr_%is_leaf() is .true.
       this%idx_ = this%nptr_%size()
    end if
  end subroutine prev_btree_node_iter_t2_i64_i64_to_int64
  !> is_begin_btree_node_iter_t2_i64_i64_to_int64: return iter is begining of B-tree.
  logical function is_begin_btree_node_iter_t2_i64_i64_to_int64(this) result(res)
    class(btree_node_iter_t2_i64_i64_to_int64), intent(in) :: this
    res = this%depth_ == 1 .and. this%idx_ == 0
  end function is_begin_btree_node_iter_t2_i64_i64_to_int64
  !> is_not_begin_btree_node_iter_t2_i64_i64_to_int64: return iter is not begining of B-tree.
  logical function is_not_begin_btree_node_iter_t2_i64_i64_to_int64(this) result(res)
    class(btree_node_iter_t2_i64_i64_to_int64), intent(in) :: this
    res = .not. this%is_begin()
  end function is_not_begin_btree_node_iter_t2_i64_i64_to_int64
  !> is_end_btree_node_iter_t2_i64_i64_to_int64: return iter is end of B-tree.
  logical function is_end_btree_node_iter_t2_i64_i64_to_int64(this) result(res)
    class(btree_node_iter_t2_i64_i64_to_int64), intent(in) :: this
    res = this%depth_ == 1 .and. this%idx_ == this%nptr_%size() + 1
  end function is_end_btree_node_iter_t2_i64_i64_to_int64
  !> is_not_end_btree_node_iter_t2_i64_i64_to_int64: return iter is not end of B-tree.
  logical function is_not_end_btree_node_iter_t2_i64_i64_to_int64(this) result(res)
    class(btree_node_iter_t2_i64_i64_to_int64), intent(in) :: this
    res = .not. this%is_end()
  end function is_not_end_btree_node_iter_t2_i64_i64_to_int64
  logical function exist_btree_node_iter_t2_i64_i64_to_int64(this) result(res)
    class(btree_node_iter_t2_i64_i64_to_int64), intent(in) :: this
    res = this%idx_ /= -1
  end function exist_btree_node_iter_t2_i64_i64_to_int64
  logical function not_exist_btree_node_iter_t2_i64_i64_to_int64(this) result(res)
    class(btree_node_iter_t2_i64_i64_to_int64), intent(in) :: this
    res = .not. this%exist()
  end function not_exist_btree_node_iter_t2_i64_i64_to_int64
  
end module tuple2_btree_m
