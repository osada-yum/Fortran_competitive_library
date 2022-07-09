module binary_tree_m
  use, intrinsic :: iso_fortran_env
  implicit none
  private
  public :: size
  public :: depth
  public :: binary_tree_int32
  type :: binary_tree_int32
     private
     integer(int32) :: num_elems_
     type(binary_tree_node_int32), pointer :: root_ => null()
   contains
     procedure, pass :: insert => insert_binary_tree_int32
     procedure, pass :: delete => delete_binary_tree_int32
     ! procedure, pass :: search => search_binary_tree_int32
     procedure, pass :: minimum => minimum_binary_tree_int32
     procedure, pass :: maximum => maximum_binary_tree_int32
     procedure, pass :: begin => begin_binary_tree_int32
     procedure, pass :: end   => end_binary_tree_int32
     procedure, pass :: to_array         => to_array_binary_tree_int32
     procedure, pass :: to_array_reverse => to_array_reverse_binary_tree_int32
     procedure, pass :: write_binary_tree_int32
     generic :: write(formatted) => write_binary_tree_int32
  end type binary_tree_int32
  public :: binary_tree_iterator_int32
  type :: binary_tree_iterator_int32
     private
     type(binary_tree_node_int32), pointer :: ptr_ => null()
   contains
     procedure, pass :: val => val_binary_tree_iterator_int32
     procedure, pass :: next => next_binary_tree_iterator_int32
     procedure, pass :: pred => pred_binary_tree_iterator_int32
     procedure, pass :: is_null => is_null_binary_tree_iterator_int32
     procedure, pass :: to => to_binary_tree_iterator_int32
  end type binary_tree_iterator_int32
  type :: binary_tree_node_int32
     private
     integer(int32) :: elem_
     type(binary_tree_node_int32), pointer :: p_     => null()
     type(binary_tree_node_int32), pointer :: left_  => null()
     type(binary_tree_node_int32), pointer :: right_ => null()
   contains
     procedure, pass :: minimum   => minimum_binary_tree_node_int32
     procedure, pass :: maximum   => maximum_binary_tree_node_int32
     procedure, pass :: successor   => successor_binary_tree_node_int32
     procedure, pass :: predecessor => predecessor_binary_tree_node_int32
  end type binary_tree_node_int32
  
  interface binary_tree_int32
     module procedure :: init_binary_tree_int32, init_binary_tree_int32_by_arr
  end interface binary_tree_int32
  interface binary_tree_node_int32
     module procedure :: init_binary_tree_node_int32
  end interface binary_tree_node_int32
  interface size
     module procedure :: size_binary_tree_int32
  end interface size
  interface depth
     module procedure :: depth_binary_tree_int32
  end interface depth
  
  public :: binary_tree_int64
  type :: binary_tree_int64
     private
     integer(int32) :: num_elems_
     type(binary_tree_node_int64), pointer :: root_ => null()
   contains
     procedure, pass :: insert => insert_binary_tree_int64
     procedure, pass :: delete => delete_binary_tree_int64
     ! procedure, pass :: search => search_binary_tree_int64
     procedure, pass :: minimum => minimum_binary_tree_int64
     procedure, pass :: maximum => maximum_binary_tree_int64
     procedure, pass :: begin => begin_binary_tree_int64
     procedure, pass :: end   => end_binary_tree_int64
     procedure, pass :: to_array         => to_array_binary_tree_int64
     procedure, pass :: to_array_reverse => to_array_reverse_binary_tree_int64
     procedure, pass :: write_binary_tree_int64
     generic :: write(formatted) => write_binary_tree_int64
  end type binary_tree_int64
  public :: binary_tree_iterator_int64
  type :: binary_tree_iterator_int64
     private
     type(binary_tree_node_int64), pointer :: ptr_ => null()
   contains
     procedure, pass :: val => val_binary_tree_iterator_int64
     procedure, pass :: next => next_binary_tree_iterator_int64
     procedure, pass :: pred => pred_binary_tree_iterator_int64
     procedure, pass :: is_null => is_null_binary_tree_iterator_int64
     procedure, pass :: to => to_binary_tree_iterator_int64
  end type binary_tree_iterator_int64
  type :: binary_tree_node_int64
     private
     integer(int64) :: elem_
     type(binary_tree_node_int64), pointer :: p_     => null()
     type(binary_tree_node_int64), pointer :: left_  => null()
     type(binary_tree_node_int64), pointer :: right_ => null()
   contains
     procedure, pass :: minimum   => minimum_binary_tree_node_int64
     procedure, pass :: maximum   => maximum_binary_tree_node_int64
     procedure, pass :: successor   => successor_binary_tree_node_int64
     procedure, pass :: predecessor => predecessor_binary_tree_node_int64
  end type binary_tree_node_int64
  
  interface binary_tree_int64
     module procedure :: init_binary_tree_int64, init_binary_tree_int64_by_arr
  end interface binary_tree_int64
  interface binary_tree_node_int64
     module procedure :: init_binary_tree_node_int64
  end interface binary_tree_node_int64
  interface size
     module procedure :: size_binary_tree_int64
  end interface size
  interface depth
     module procedure :: depth_binary_tree_int64
  end interface depth
  
contains
  impure type(binary_tree_int32) function init_binary_tree_int32() result(res)
    res%num_elems_ = 0
    res%root_ => null()
  end function init_binary_tree_int32
  impure type(binary_tree_int32) function init_binary_tree_int32_by_arr(arr) result(res)
    integer(int32), intent(in) :: arr(:)
    integer(int32) :: i, n
    res%num_elems_ = 0
    n = size(arr)
    do i = 1, n
       call res%insert(arr(i))
    end do
  end function init_binary_tree_int32_by_arr
  impure function init_binary_tree_node_int32(val) result(node)
    type(binary_tree_node_int32), pointer :: node
    integer(int32), intent(in) :: val
    allocate(node)
    node%elem_  = val
    node%p_     => null()
    node%left_  => null()
    node%right_ => null()
    return
  end function init_binary_tree_node_int32
  subroutine insert_binary_tree_int32(tree, val)
    class(binary_tree_int32), intent(inout) :: tree
    integer(int32), intent(in) :: val
    type(binary_tree_node_int32), pointer :: node, new_node, parent
    new_node => binary_tree_node_int32(val)
    parent => null()
    node => tree%root_
    if (.not. associated(node)) then
       tree%root_  => new_node
       new_node%p_ => null()
       tree%num_elems_ = 1
       return
    end if
    do
       if (val == node%elem_) then
          return
       else if (val < node%elem_) then
          parent => node
          node   => node%left_
          if (.not. associated(node)) then
             parent%left_ => new_node
             new_node%p_  => parent
             tree%num_elems_ = tree%num_elems_ + 1
             return
          end if
       else
          parent => node
          node   => node%right_
          if (.not. associated(node)) then
             parent%right_ => new_node
             new_node%p_   => parent
             tree%num_elems_ = tree%num_elems_ + 1
             return
          end if
       end if
    end do
  end subroutine insert_binary_tree_int32
  subroutine delete_binary_tree_int32(tree, val)
    class(binary_tree_int32), intent(inout) :: tree
    integer(int32), intent(in) :: val
    type(binary_tree_node_int32), pointer :: child, left, right
    if (.not. associated(tree%root_)) then
       return
    end if
    child => tree%root_
    do
       !> undefined...
    end do
  end subroutine delete_binary_tree_int32
  integer(int32) function minimum_binary_tree_int32(tree) result(mini)
    class(binary_tree_int32), intent(in) :: tree
    type(binary_tree_node_int32), pointer :: node, min_node
    if (.not. associated(tree%root_)) then
       write(error_unit, *) "Manipulation to empty tree is not allowed."
       return
    end if
    node => tree%root_
    min_node => node%minimum()
    mini = min_node%elem_
  end function minimum_binary_tree_int32
  integer(int32) function maximum_binary_tree_int32(tree) result(maxi)
    class(binary_tree_int32), intent(in) :: tree
    type(binary_tree_node_int32), pointer :: node, max_node
    if (.not. associated(tree%root_)) then
       write(error_unit, *) "Manipulation to empty tree is not allowed."
       return
    end if
    node => tree%root_
    max_node => node%maximum()
    maxi = max_node%elem_
  end function maximum_binary_tree_int32
  function begin_binary_tree_int32(tree) result(begin)
    class(binary_tree_int32), intent(in) :: tree
    type(binary_tree_node_int32), pointer :: begin
    if (.not. associated(tree%root_)) then
       write(error_unit, *) "Manipulation to empty tree is not allowed."
       return
    end if
    begin => tree%root_%minimum()
  end function begin_binary_tree_int32
  function end_binary_tree_int32(tree) result(end)
    class(binary_tree_int32), intent(in) :: tree
    type(binary_tree_node_int32), pointer :: end
    if (.not. associated(tree%root_)) then
       write(error_unit, *) "Manipulation to empty tree is not allowed."
       return
    end if
    end => tree%root_%maximum()
  end function end_binary_tree_int32
  subroutine to_array_binary_tree_int32(tree, arr)
    class(binary_tree_int32), intent(in) :: tree
    integer(int32), intent(out) :: arr(:)
    type(binary_tree_iterator_int32) :: iter
    type(binary_tree_node_int32), pointer :: beg_ptr
    integer(int32) :: i
    beg_ptr => tree%begin()
    call iter%to(beg_ptr)
    do i = 1, size(tree)
       if (iter%is_null()) exit
       arr(i) = iter%val()
       call iter%next()
    end do
  end subroutine to_array_binary_tree_int32
  subroutine to_array_reverse_binary_tree_int32(tree, arr)
    class(binary_tree_int32), intent(in) :: tree
    integer(int32), intent(out) :: arr(:)
    type(binary_tree_iterator_int32) :: iter
    type(binary_tree_node_int32), pointer :: end_ptr
    integer(int32) :: i
    end_ptr => tree%end()
    call iter%to(end_ptr)
    do i = 1, size(tree)
       if (iter%is_null()) exit
       arr(i) = iter%val()
       call iter%pred()
    end do
  end subroutine to_array_reverse_binary_tree_int32
  function minimum_binary_tree_node_int32(this) result(min_node)
    class(binary_tree_node_int32), target, intent(in) :: this
    type(binary_tree_node_int32) , pointer :: min_node
    min_node => this
    do
       if (.not. associated(min_node%left_)) return
       min_node => min_node%left_
    end do
  end function minimum_binary_tree_node_int32
  function maximum_binary_tree_node_int32(this) result(max_node)
    class(binary_tree_node_int32), target, intent(in) :: this
    type(binary_tree_node_int32) , pointer :: max_node
    max_node => this
    do
       if (.not. associated(max_node%right_)) return
       max_node => max_node%right_
    end do
  end function maximum_binary_tree_node_int32
  function successor_binary_tree_node_int32(this) result(succ)
    class(binary_tree_node_int32), target, intent(in) :: this
    type(binary_tree_node_int32) , pointer :: succ
    succ => this
    if (associated(succ%right_)) then ! succ has right.
       succ => succ%right_%minimum()
       return
    end if
    do
       if (.not. associated(succ%p_)) then
          succ => null()
          return
       else if (associated(succ, succ%p_%left_)) then
          succ => succ%p_
          return
       end if
       succ => succ%p_
    end do
  end function successor_binary_tree_node_int32
  function predecessor_binary_tree_node_int32(this) result(pred_node)
    class(binary_tree_node_int32), target, intent(in) :: this
    type(binary_tree_node_int32) , pointer :: node, pred_node
    node => this
    if (associated(node%left_)) then ! node has left.
       pred_node => node%left_%maximum()
       return
    end if
    do
       if (.not. associated(node%p_)) then
          pred_node => null()
          return
       else if (associated(node, node%p_%right_)) then
          pred_node => node%p_
          return
       end if
       node => node%p_
    end do
  end function predecessor_binary_tree_node_int32
  subroutine to_binary_tree_iterator_int32(iter, node_ptr)
    class(binary_tree_iterator_int32), intent(out) :: iter
    type(binary_tree_node_int32), pointer, intent(in) :: node_ptr
    iter%ptr_ => node_ptr
  end subroutine to_binary_tree_iterator_int32
  integer(int32) function val_binary_tree_iterator_int32(iter) result(res)
    class(binary_tree_iterator_int32), intent(in) :: iter
    res = iter%ptr_%elem_
  end function val_binary_tree_iterator_int32
  subroutine next_binary_tree_iterator_int32(iter)
    class(binary_tree_iterator_int32), intent(inout) :: iter
    if (associated(iter%ptr_)) iter%ptr_ => iter%ptr_%successor()
  end subroutine next_binary_tree_iterator_int32
  subroutine pred_binary_tree_iterator_int32(iter)
    class(binary_tree_iterator_int32), intent(inout) :: iter
    iter%ptr_ => iter%ptr_%predecessor()
  end subroutine pred_binary_tree_iterator_int32
  logical function is_null_binary_tree_iterator_int32(iter) result(is_null)
    class(binary_tree_iterator_int32), intent(in) :: iter
    is_null = .not. associated(iter%ptr_)
  end function is_null_binary_tree_iterator_int32
  pure integer(int32) function size_binary_tree_int32(tree) result(res)
    type(binary_tree_int32), intent(in) :: tree
    res = tree%num_elems_
  end function size_binary_tree_int32
  integer(int32) function depth_binary_tree_int32(tree) result(res)
    type(binary_tree_int32), intent(in) :: tree
    type(binary_tree_node_int32), pointer :: child
    child => tree%root_
    if (.not. associated(child)) then
       res = 0
    end if
    res = depth_binary_tree_node_int32(child)
  end function depth_binary_tree_int32
  pure recursive integer(int32) function depth_binary_tree_node_int32(node) result(res)
    type(binary_tree_node_int32), intent(in) :: node
    logical :: exist_left, exist_right
    exist_left  = associated(node%left_)
    exist_right = associated(node%right_)
    if (exist_left .and. exist_right) then
       res = 1 + max(depth_binary_tree_node_int32(node%left_)&
            , depth_binary_tree_node_int32(node%right_))
    else if (exist_left) then
       res = 1 + depth_binary_tree_node_int32(node%left_)
    else if (exist_right) then
       res = 1 + depth_binary_tree_node_int32(node%right_)
    else
       res = 1
    end if
  end function depth_binary_tree_node_int32
  subroutine write_binary_tree_int32(this, unit, iotype, v_list, iostat, iomsg)
    class(binary_tree_int32), intent(in) :: this
    type(binary_tree_iterator_int32) :: iter
    integer              , intent(in)    :: unit
    character(len=*)     , intent(in)    :: iotype
    integer              , intent(in)    :: v_list(:)
    integer              , intent(out)   :: iostat
    character(len=*)     , intent(inout) :: iomsg
    if (.not. associated(this%root_)) return
    write(unit, *, iostat=iostat, iomsg=iomsg) "| "
    call print_nodes_binary_tree_int32(this%root_, unit=unit, iostat=iostat, iomsg=iomsg)
    write(unit, *, iostat=iostat, iomsg=iomsg) "| "
  end subroutine write_binary_tree_int32
  
  recursive subroutine print_nodes_binary_tree_int32(node, unit, iostat, iomsg)
    type(binary_tree_node_int32), intent(in) :: node
    integer         , intent(in)    :: unit
    integer         , intent(out)   :: iostat
    character(len=*), intent(inout) :: iomsg
    if (associated(node%left_)) call print_nodes_binary_tree_int32(node%left_, unit=unit, iostat=iostat, iomsg=iomsg)
    write(unit, *, iostat=iostat, iomsg=iomsg) node%elem_
    if (associated(node%right_)) call print_nodes_binary_tree_int32(node%right_, unit=unit, iostat=iostat, iomsg=iomsg)
  end subroutine print_nodes_binary_tree_int32
  
  impure type(binary_tree_int64) function init_binary_tree_int64() result(res)
    res%num_elems_ = 0
    res%root_ => null()
  end function init_binary_tree_int64
  impure type(binary_tree_int64) function init_binary_tree_int64_by_arr(arr) result(res)
    integer(int64), intent(in) :: arr(:)
    integer(int32) :: i, n
    res%num_elems_ = 0
    n = size(arr)
    do i = 1, n
       call res%insert(arr(i))
    end do
  end function init_binary_tree_int64_by_arr
  impure function init_binary_tree_node_int64(val) result(node)
    type(binary_tree_node_int64), pointer :: node
    integer(int64), intent(in) :: val
    allocate(node)
    node%elem_  = val
    node%p_     => null()
    node%left_  => null()
    node%right_ => null()
    return
  end function init_binary_tree_node_int64
  subroutine insert_binary_tree_int64(tree, val)
    class(binary_tree_int64), intent(inout) :: tree
    integer(int64), intent(in) :: val
    type(binary_tree_node_int64), pointer :: node, new_node, parent
    new_node => binary_tree_node_int64(val)
    parent => null()
    node => tree%root_
    if (.not. associated(node)) then
       tree%root_  => new_node
       new_node%p_ => null()
       tree%num_elems_ = 1
       return
    end if
    do
       if (val == node%elem_) then
          return
       else if (val < node%elem_) then
          parent => node
          node   => node%left_
          if (.not. associated(node)) then
             parent%left_ => new_node
             new_node%p_  => parent
             tree%num_elems_ = tree%num_elems_ + 1
             return
          end if
       else
          parent => node
          node   => node%right_
          if (.not. associated(node)) then
             parent%right_ => new_node
             new_node%p_   => parent
             tree%num_elems_ = tree%num_elems_ + 1
             return
          end if
       end if
    end do
  end subroutine insert_binary_tree_int64
  subroutine delete_binary_tree_int64(tree, val)
    class(binary_tree_int64), intent(inout) :: tree
    integer(int64), intent(in) :: val
    type(binary_tree_node_int64), pointer :: child, left, right
    if (.not. associated(tree%root_)) then
       return
    end if
    child => tree%root_
    do
       !> undefined...
    end do
  end subroutine delete_binary_tree_int64
  integer(int64) function minimum_binary_tree_int64(tree) result(mini)
    class(binary_tree_int64), intent(in) :: tree
    type(binary_tree_node_int64), pointer :: node, min_node
    if (.not. associated(tree%root_)) then
       write(error_unit, *) "Manipulation to empty tree is not allowed."
       return
    end if
    node => tree%root_
    min_node => node%minimum()
    mini = min_node%elem_
  end function minimum_binary_tree_int64
  integer(int64) function maximum_binary_tree_int64(tree) result(maxi)
    class(binary_tree_int64), intent(in) :: tree
    type(binary_tree_node_int64), pointer :: node, max_node
    if (.not. associated(tree%root_)) then
       write(error_unit, *) "Manipulation to empty tree is not allowed."
       return
    end if
    node => tree%root_
    max_node => node%maximum()
    maxi = max_node%elem_
  end function maximum_binary_tree_int64
  function begin_binary_tree_int64(tree) result(begin)
    class(binary_tree_int64), intent(in) :: tree
    type(binary_tree_node_int64), pointer :: begin
    if (.not. associated(tree%root_)) then
       write(error_unit, *) "Manipulation to empty tree is not allowed."
       return
    end if
    begin => tree%root_%minimum()
  end function begin_binary_tree_int64
  function end_binary_tree_int64(tree) result(end)
    class(binary_tree_int64), intent(in) :: tree
    type(binary_tree_node_int64), pointer :: end
    if (.not. associated(tree%root_)) then
       write(error_unit, *) "Manipulation to empty tree is not allowed."
       return
    end if
    end => tree%root_%maximum()
  end function end_binary_tree_int64
  subroutine to_array_binary_tree_int64(tree, arr)
    class(binary_tree_int64), intent(in) :: tree
    integer(int64), intent(out) :: arr(:)
    type(binary_tree_iterator_int64) :: iter
    type(binary_tree_node_int64), pointer :: beg_ptr
    integer(int32) :: i
    beg_ptr => tree%begin()
    call iter%to(beg_ptr)
    do i = 1, size(tree)
       if (iter%is_null()) exit
       arr(i) = iter%val()
       call iter%next()
    end do
  end subroutine to_array_binary_tree_int64
  subroutine to_array_reverse_binary_tree_int64(tree, arr)
    class(binary_tree_int64), intent(in) :: tree
    integer(int64), intent(out) :: arr(:)
    type(binary_tree_iterator_int64) :: iter
    type(binary_tree_node_int64), pointer :: end_ptr
    integer(int32) :: i
    end_ptr => tree%end()
    call iter%to(end_ptr)
    do i = 1, size(tree)
       if (iter%is_null()) exit
       arr(i) = iter%val()
       call iter%pred()
    end do
  end subroutine to_array_reverse_binary_tree_int64
  function minimum_binary_tree_node_int64(this) result(min_node)
    class(binary_tree_node_int64), target, intent(in) :: this
    type(binary_tree_node_int64) , pointer :: min_node
    min_node => this
    do
       if (.not. associated(min_node%left_)) return
       min_node => min_node%left_
    end do
  end function minimum_binary_tree_node_int64
  function maximum_binary_tree_node_int64(this) result(max_node)
    class(binary_tree_node_int64), target, intent(in) :: this
    type(binary_tree_node_int64) , pointer :: max_node
    max_node => this
    do
       if (.not. associated(max_node%right_)) return
       max_node => max_node%right_
    end do
  end function maximum_binary_tree_node_int64
  function successor_binary_tree_node_int64(this) result(succ)
    class(binary_tree_node_int64), target, intent(in) :: this
    type(binary_tree_node_int64) , pointer :: succ
    succ => this
    if (associated(succ%right_)) then ! succ has right.
       succ => succ%right_%minimum()
       return
    end if
    do
       if (.not. associated(succ%p_)) then
          succ => null()
          return
       else if (associated(succ, succ%p_%left_)) then
          succ => succ%p_
          return
       end if
       succ => succ%p_
    end do
  end function successor_binary_tree_node_int64
  function predecessor_binary_tree_node_int64(this) result(pred_node)
    class(binary_tree_node_int64), target, intent(in) :: this
    type(binary_tree_node_int64) , pointer :: node, pred_node
    node => this
    if (associated(node%left_)) then ! node has left.
       pred_node => node%left_%maximum()
       return
    end if
    do
       if (.not. associated(node%p_)) then
          pred_node => null()
          return
       else if (associated(node, node%p_%right_)) then
          pred_node => node%p_
          return
       end if
       node => node%p_
    end do
  end function predecessor_binary_tree_node_int64
  subroutine to_binary_tree_iterator_int64(iter, node_ptr)
    class(binary_tree_iterator_int64), intent(out) :: iter
    type(binary_tree_node_int64), pointer, intent(in) :: node_ptr
    iter%ptr_ => node_ptr
  end subroutine to_binary_tree_iterator_int64
  integer(int64) function val_binary_tree_iterator_int64(iter) result(res)
    class(binary_tree_iterator_int64), intent(in) :: iter
    res = iter%ptr_%elem_
  end function val_binary_tree_iterator_int64
  subroutine next_binary_tree_iterator_int64(iter)
    class(binary_tree_iterator_int64), intent(inout) :: iter
    if (associated(iter%ptr_)) iter%ptr_ => iter%ptr_%successor()
  end subroutine next_binary_tree_iterator_int64
  subroutine pred_binary_tree_iterator_int64(iter)
    class(binary_tree_iterator_int64), intent(inout) :: iter
    iter%ptr_ => iter%ptr_%predecessor()
  end subroutine pred_binary_tree_iterator_int64
  logical function is_null_binary_tree_iterator_int64(iter) result(is_null)
    class(binary_tree_iterator_int64), intent(in) :: iter
    is_null = .not. associated(iter%ptr_)
  end function is_null_binary_tree_iterator_int64
  pure integer(int32) function size_binary_tree_int64(tree) result(res)
    type(binary_tree_int64), intent(in) :: tree
    res = tree%num_elems_
  end function size_binary_tree_int64
  integer(int32) function depth_binary_tree_int64(tree) result(res)
    type(binary_tree_int64), intent(in) :: tree
    type(binary_tree_node_int64), pointer :: child
    child => tree%root_
    if (.not. associated(child)) then
       res = 0
    end if
    res = depth_binary_tree_node_int64(child)
  end function depth_binary_tree_int64
  pure recursive integer(int32) function depth_binary_tree_node_int64(node) result(res)
    type(binary_tree_node_int64), intent(in) :: node
    logical :: exist_left, exist_right
    exist_left  = associated(node%left_)
    exist_right = associated(node%right_)
    if (exist_left .and. exist_right) then
       res = 1 + max(depth_binary_tree_node_int64(node%left_)&
            , depth_binary_tree_node_int64(node%right_))
    else if (exist_left) then
       res = 1 + depth_binary_tree_node_int64(node%left_)
    else if (exist_right) then
       res = 1 + depth_binary_tree_node_int64(node%right_)
    else
       res = 1
    end if
  end function depth_binary_tree_node_int64
  subroutine write_binary_tree_int64(this, unit, iotype, v_list, iostat, iomsg)
    class(binary_tree_int64), intent(in) :: this
    type(binary_tree_iterator_int64) :: iter
    integer              , intent(in)    :: unit
    character(len=*)     , intent(in)    :: iotype
    integer              , intent(in)    :: v_list(:)
    integer              , intent(out)   :: iostat
    character(len=*)     , intent(inout) :: iomsg
    if (.not. associated(this%root_)) return
    write(unit, *, iostat=iostat, iomsg=iomsg) "| "
    call print_nodes_binary_tree_int64(this%root_, unit=unit, iostat=iostat, iomsg=iomsg)
    write(unit, *, iostat=iostat, iomsg=iomsg) "| "
  end subroutine write_binary_tree_int64
  
  recursive subroutine print_nodes_binary_tree_int64(node, unit, iostat, iomsg)
    type(binary_tree_node_int64), intent(in) :: node
    integer         , intent(in)    :: unit
    integer         , intent(out)   :: iostat
    character(len=*), intent(inout) :: iomsg
    if (associated(node%left_)) call print_nodes_binary_tree_int64(node%left_, unit=unit, iostat=iostat, iomsg=iomsg)
    write(unit, *, iostat=iostat, iomsg=iomsg) node%elem_
    if (associated(node%right_)) call print_nodes_binary_tree_int64(node%right_, unit=unit, iostat=iostat, iomsg=iomsg)
  end subroutine print_nodes_binary_tree_int64
  
end module binary_tree_m
