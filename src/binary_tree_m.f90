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
     procedure, pass :: search => search_binary_tree_int32
     procedure, pass :: write_binary_tree_int32
     generic :: write(formatted) => write_binary_tree_int32
  end type binary_tree_int32
  type :: binary_tree_node_int32
     private
     integer(int32) :: elem_
     type(binary_tree_node_int32), pointer :: left_  => null()
     type(binary_tree_node_int32), pointer :: right_ => null()
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
     procedure, pass :: search => search_binary_tree_int64
     procedure, pass :: write_binary_tree_int64
     generic :: write(formatted) => write_binary_tree_int64
  end type binary_tree_int64
  type :: binary_tree_node_int64
     private
     integer(int64) :: elem_
     type(binary_tree_node_int64), pointer :: left_  => null()
     type(binary_tree_node_int64), pointer :: right_ => null()
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
    node%left_  => null()
    node%right_ => null()
    return
  end function init_binary_tree_node_int32
  subroutine insert_binary_tree_int32(tree, val)
    class(binary_tree_int32), intent(inout) :: tree
    integer(int32), intent(in) :: val
    type(binary_tree_node_int32), pointer :: child
    if (.not. associated(tree%root_)) then
       tree%root_ => binary_tree_node_int32(val)
       tree%num_elems_ = 1
       return
    end if
    child => tree%root_
    do
       if (val == child%elem_) then
          return
       else if (val < child%elem_) then
          if (.not. associated(child%left_)) then
             child%left_ => binary_tree_node_int32(val)
             tree%num_elems_ = tree%num_elems_ + 1
             return
          end if
          child => child%left_
       else
          if (.not. associated(child%right_)) then
             child%right_ => binary_tree_node_int32(val)
             tree%num_elems_ = tree%num_elems_ + 1
             return
          end if
          child => child%right_
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
  logical function search_binary_tree_int32(tree, val) result(found)
    class(binary_tree_int32), intent(in) :: tree
    integer(int32), intent(in) :: val
    !> undefined...
  end function search_binary_tree_int32
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
    node%left_  => null()
    node%right_ => null()
    return
  end function init_binary_tree_node_int64
  subroutine insert_binary_tree_int64(tree, val)
    class(binary_tree_int64), intent(inout) :: tree
    integer(int64), intent(in) :: val
    type(binary_tree_node_int64), pointer :: child
    if (.not. associated(tree%root_)) then
       tree%root_ => binary_tree_node_int64(val)
       tree%num_elems_ = 1
       return
    end if
    child => tree%root_
    do
       if (val == child%elem_) then
          return
       else if (val < child%elem_) then
          if (.not. associated(child%left_)) then
             child%left_ => binary_tree_node_int64(val)
             tree%num_elems_ = tree%num_elems_ + 1
             return
          end if
          child => child%left_
       else
          if (.not. associated(child%right_)) then
             child%right_ => binary_tree_node_int64(val)
             tree%num_elems_ = tree%num_elems_ + 1
             return
          end if
          child => child%right_
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
  logical function search_binary_tree_int64(tree, val) result(found)
    class(binary_tree_int64), intent(in) :: tree
    integer(int64), intent(in) :: val
    !> undefined...
  end function search_binary_tree_int64
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
