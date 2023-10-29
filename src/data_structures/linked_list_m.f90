module linked_list_m
  use, intrinsic :: iso_fortran_env
  implicit none
  private :: linked_list_int32
  type :: linked_list_int32
     private
     integer(int32) :: val_
     type(linked_list_int32), pointer :: next_ => null()
  end type linked_list_int32
  
  public :: linked_list_int32_head
  type :: linked_list_int32_head
     private
     type(linked_list_int32), pointer :: head_ => null()
   contains
     procedure, pass :: add    => add_linked_list_int32_head
     procedure, pass :: delete => delete_linked_list_int32_head
     procedure, pass :: search => search_linked_list_int32_head
  end type linked_list_int32_head
  
  interface linked_list_int32
     module procedure :: init_linked_list_int32
  end interface linked_list_int32
  interface linked_list_int32_head
     module procedure :: init_linked_list_int32_head
     module procedure :: init_linked_list_int32_head_by_array
  end interface linked_list_int32_head
  
  private :: linked_list_int64
  type :: linked_list_int64
     private
     integer(int64) :: val_
     type(linked_list_int64), pointer :: next_ => null()
  end type linked_list_int64
  
  public :: linked_list_int64_head
  type :: linked_list_int64_head
     private
     type(linked_list_int64), pointer :: head_ => null()
   contains
     procedure, pass :: add    => add_linked_list_int64_head
     procedure, pass :: delete => delete_linked_list_int64_head
     procedure, pass :: search => search_linked_list_int64_head
  end type linked_list_int64_head
  
  interface linked_list_int64
     module procedure :: init_linked_list_int64
  end interface linked_list_int64
  interface linked_list_int64_head
     module procedure :: init_linked_list_int64_head
     module procedure :: init_linked_list_int64_head_by_array
  end interface linked_list_int64_head
  
  private :: linked_list_real32
  type :: linked_list_real32
     private
     real(real32) :: val_
     type(linked_list_real32), pointer :: next_ => null()
  end type linked_list_real32
  
  public :: linked_list_real32_head
  type :: linked_list_real32_head
     private
     type(linked_list_real32), pointer :: head_ => null()
   contains
     procedure, pass :: add    => add_linked_list_real32_head
     procedure, pass :: delete => delete_linked_list_real32_head
     procedure, pass :: search => search_linked_list_real32_head
  end type linked_list_real32_head
  
  interface linked_list_real32
     module procedure :: init_linked_list_real32
  end interface linked_list_real32
  interface linked_list_real32_head
     module procedure :: init_linked_list_real32_head
     module procedure :: init_linked_list_real32_head_by_array
  end interface linked_list_real32_head
  
  private :: linked_list_real64
  type :: linked_list_real64
     private
     real(real64) :: val_
     type(linked_list_real64), pointer :: next_ => null()
  end type linked_list_real64
  
  public :: linked_list_real64_head
  type :: linked_list_real64_head
     private
     type(linked_list_real64), pointer :: head_ => null()
   contains
     procedure, pass :: add    => add_linked_list_real64_head
     procedure, pass :: delete => delete_linked_list_real64_head
     procedure, pass :: search => search_linked_list_real64_head
  end type linked_list_real64_head
  
  interface linked_list_real64
     module procedure :: init_linked_list_real64
  end interface linked_list_real64
  interface linked_list_real64_head
     module procedure :: init_linked_list_real64_head
     module procedure :: init_linked_list_real64_head_by_array
  end interface linked_list_real64_head
  
contains
  !> init_linked_list_int32: Initialize the linked_list_int32 by val.
  impure function init_linked_list_int32(val) result(lst)
    type(linked_list_int32), pointer :: lst
    integer(int32) :: val
    allocate(lst)
    lst%val_ = val
    return
  end function init_linked_list_int32
  !> init_linked_list_int32_head: Initialize the empty linked_list_int32_head.
  impure function init_linked_list_int32_head() result(lst_head)
    type(linked_list_int32_head) :: lst_head
    lst_head%head_ => null()
    return
  end function init_linked_list_int32_head
  !> init_linked_list_int32_head_by_array: Initialize the empty linked_list_int32_head by array.
  impure function init_linked_list_int32_head_by_array(arr) result(lst_head)
    type(linked_list_int32_head) :: lst_head
    integer(int32) :: arr(:)
    integer(int32) :: s, i
    s = size(arr)
    do i = s, 1, -1
       call lst_head%add(arr(i))
    end do
    return
  end function init_linked_list_int32_head_by_array
  !> add_linked_list_int32: Add val into head of linked list.
  subroutine add_linked_list_int32_head(lst_head, val)
    class(linked_list_int32_head), intent(inout) :: lst_head
    integer(int32), intent(in) :: val
    type(linked_list_int32), pointer :: lst_elem
    lst_elem => linked_list_int32(val)
    lst_elem%next_ => lst_head%head_
    lst_head%head_ => lst_elem
  end subroutine add_linked_list_int32_head
  !> delete_linked_list_int32: Delete val from element of linked list.
  !> Do nothing if lst does not elem val.
  subroutine delete_linked_list_int32_head(lst_head, val)
    class(linked_list_int32_head), intent(inout) :: lst_head
    integer(int32), intent(in) :: val
    type(linked_list_int32), pointer :: lst_elem, lst_del
    if (.not. associated(lst_head%head_)) return
    lst_elem => lst_head%head_
    if (lst_elem%val_ == val) then
       lst_head%head_ => lst_elem%next_
       deallocate(lst_elem)
       return
    end if
    do
       if (.not. associated(lst_elem%next_)) return
       if (lst_elem%next_%val_ == val) then
          lst_del => lst_elem%next_
          lst_elem%next_ => lst_elem%next_%next_
          deallocate(lst_del)
          return
       end if
    end do
  end subroutine delete_linked_list_int32_head
  !> search_linked_list_int32: Search val from element of linked list.
  !> Return .true. if success.
  logical function search_linked_list_int32_head(lst_head, val) result(find)
    class(linked_list_int32_head), intent(in) :: lst_head
    integer(int32), intent(in) :: val
    type(linked_list_int32), pointer :: lst_elem
    if (.not. associated(lst_head%head_)) return
    lst_elem => lst_head%head_
    find = .false.
    do
       if (.not. associated(lst_elem)) return
       if (lst_elem%val_ == val) then
          find = .true.
          return
       end if
       lst_elem => lst_elem%next_
    end do
  end function search_linked_list_int32_head
  
  !> init_linked_list_int64: Initialize the linked_list_int64 by val.
  impure function init_linked_list_int64(val) result(lst)
    type(linked_list_int64), pointer :: lst
    integer(int64) :: val
    allocate(lst)
    lst%val_ = val
    return
  end function init_linked_list_int64
  !> init_linked_list_int64_head: Initialize the empty linked_list_int64_head.
  impure function init_linked_list_int64_head() result(lst_head)
    type(linked_list_int64_head) :: lst_head
    lst_head%head_ => null()
    return
  end function init_linked_list_int64_head
  !> init_linked_list_int64_head_by_array: Initialize the empty linked_list_int64_head by array.
  impure function init_linked_list_int64_head_by_array(arr) result(lst_head)
    type(linked_list_int64_head) :: lst_head
    integer(int64) :: arr(:)
    integer(int32) :: s, i
    s = size(arr)
    do i = s, 1, -1
       call lst_head%add(arr(i))
    end do
    return
  end function init_linked_list_int64_head_by_array
  !> add_linked_list_int64: Add val into head of linked list.
  subroutine add_linked_list_int64_head(lst_head, val)
    class(linked_list_int64_head), intent(inout) :: lst_head
    integer(int64), intent(in) :: val
    type(linked_list_int64), pointer :: lst_elem
    lst_elem => linked_list_int64(val)
    lst_elem%next_ => lst_head%head_
    lst_head%head_ => lst_elem
  end subroutine add_linked_list_int64_head
  !> delete_linked_list_int64: Delete val from element of linked list.
  !> Do nothing if lst does not elem val.
  subroutine delete_linked_list_int64_head(lst_head, val)
    class(linked_list_int64_head), intent(inout) :: lst_head
    integer(int64), intent(in) :: val
    type(linked_list_int64), pointer :: lst_elem, lst_del
    if (.not. associated(lst_head%head_)) return
    lst_elem => lst_head%head_
    if (lst_elem%val_ == val) then
       lst_head%head_ => lst_elem%next_
       deallocate(lst_elem)
       return
    end if
    do
       if (.not. associated(lst_elem%next_)) return
       if (lst_elem%next_%val_ == val) then
          lst_del => lst_elem%next_
          lst_elem%next_ => lst_elem%next_%next_
          deallocate(lst_del)
          return
       end if
    end do
  end subroutine delete_linked_list_int64_head
  !> search_linked_list_int64: Search val from element of linked list.
  !> Return .true. if success.
  logical function search_linked_list_int64_head(lst_head, val) result(find)
    class(linked_list_int64_head), intent(in) :: lst_head
    integer(int64), intent(in) :: val
    type(linked_list_int64), pointer :: lst_elem
    if (.not. associated(lst_head%head_)) return
    lst_elem => lst_head%head_
    find = .false.
    do
       if (.not. associated(lst_elem)) return
       if (lst_elem%val_ == val) then
          find = .true.
          return
       end if
       lst_elem => lst_elem%next_
    end do
  end function search_linked_list_int64_head
  
  !> init_linked_list_real32: Initialize the linked_list_real32 by val.
  impure function init_linked_list_real32(val) result(lst)
    type(linked_list_real32), pointer :: lst
    real(real32) :: val
    allocate(lst)
    lst%val_ = val
    return
  end function init_linked_list_real32
  !> init_linked_list_real32_head: Initialize the empty linked_list_real32_head.
  impure function init_linked_list_real32_head() result(lst_head)
    type(linked_list_real32_head) :: lst_head
    lst_head%head_ => null()
    return
  end function init_linked_list_real32_head
  !> init_linked_list_real32_head_by_array: Initialize the empty linked_list_real32_head by array.
  impure function init_linked_list_real32_head_by_array(arr) result(lst_head)
    type(linked_list_real32_head) :: lst_head
    real(real32) :: arr(:)
    integer(int32) :: s, i
    s = size(arr)
    do i = s, 1, -1
       call lst_head%add(arr(i))
    end do
    return
  end function init_linked_list_real32_head_by_array
  !> add_linked_list_real32: Add val into head of linked list.
  subroutine add_linked_list_real32_head(lst_head, val)
    class(linked_list_real32_head), intent(inout) :: lst_head
    real(real32), intent(in) :: val
    type(linked_list_real32), pointer :: lst_elem
    lst_elem => linked_list_real32(val)
    lst_elem%next_ => lst_head%head_
    lst_head%head_ => lst_elem
  end subroutine add_linked_list_real32_head
  !> delete_linked_list_real32: Delete val from element of linked list.
  !> Do nothing if lst does not elem val.
  subroutine delete_linked_list_real32_head(lst_head, val)
    class(linked_list_real32_head), intent(inout) :: lst_head
    real(real32), intent(in) :: val
    type(linked_list_real32), pointer :: lst_elem, lst_del
    if (.not. associated(lst_head%head_)) return
    lst_elem => lst_head%head_
    if (lst_elem%val_ == val) then
       lst_head%head_ => lst_elem%next_
       deallocate(lst_elem)
       return
    end if
    do
       if (.not. associated(lst_elem%next_)) return
       if (lst_elem%next_%val_ == val) then
          lst_del => lst_elem%next_
          lst_elem%next_ => lst_elem%next_%next_
          deallocate(lst_del)
          return
       end if
    end do
  end subroutine delete_linked_list_real32_head
  !> search_linked_list_real32: Search val from element of linked list.
  !> Return .true. if success.
  logical function search_linked_list_real32_head(lst_head, val) result(find)
    class(linked_list_real32_head), intent(in) :: lst_head
    real(real32), intent(in) :: val
    type(linked_list_real32), pointer :: lst_elem
    if (.not. associated(lst_head%head_)) return
    lst_elem => lst_head%head_
    find = .false.
    do
       if (.not. associated(lst_elem)) return
       if (lst_elem%val_ == val) then
          find = .true.
          return
       end if
       lst_elem => lst_elem%next_
    end do
  end function search_linked_list_real32_head
  
  !> init_linked_list_real64: Initialize the linked_list_real64 by val.
  impure function init_linked_list_real64(val) result(lst)
    type(linked_list_real64), pointer :: lst
    real(real64) :: val
    allocate(lst)
    lst%val_ = val
    return
  end function init_linked_list_real64
  !> init_linked_list_real64_head: Initialize the empty linked_list_real64_head.
  impure function init_linked_list_real64_head() result(lst_head)
    type(linked_list_real64_head) :: lst_head
    lst_head%head_ => null()
    return
  end function init_linked_list_real64_head
  !> init_linked_list_real64_head_by_array: Initialize the empty linked_list_real64_head by array.
  impure function init_linked_list_real64_head_by_array(arr) result(lst_head)
    type(linked_list_real64_head) :: lst_head
    real(real64) :: arr(:)
    integer(int32) :: s, i
    s = size(arr)
    do i = s, 1, -1
       call lst_head%add(arr(i))
    end do
    return
  end function init_linked_list_real64_head_by_array
  !> add_linked_list_real64: Add val into head of linked list.
  subroutine add_linked_list_real64_head(lst_head, val)
    class(linked_list_real64_head), intent(inout) :: lst_head
    real(real64), intent(in) :: val
    type(linked_list_real64), pointer :: lst_elem
    lst_elem => linked_list_real64(val)
    lst_elem%next_ => lst_head%head_
    lst_head%head_ => lst_elem
  end subroutine add_linked_list_real64_head
  !> delete_linked_list_real64: Delete val from element of linked list.
  !> Do nothing if lst does not elem val.
  subroutine delete_linked_list_real64_head(lst_head, val)
    class(linked_list_real64_head), intent(inout) :: lst_head
    real(real64), intent(in) :: val
    type(linked_list_real64), pointer :: lst_elem, lst_del
    if (.not. associated(lst_head%head_)) return
    lst_elem => lst_head%head_
    if (lst_elem%val_ == val) then
       lst_head%head_ => lst_elem%next_
       deallocate(lst_elem)
       return
    end if
    do
       if (.not. associated(lst_elem%next_)) return
       if (lst_elem%next_%val_ == val) then
          lst_del => lst_elem%next_
          lst_elem%next_ => lst_elem%next_%next_
          deallocate(lst_del)
          return
       end if
    end do
  end subroutine delete_linked_list_real64_head
  !> search_linked_list_real64: Search val from element of linked list.
  !> Return .true. if success.
  logical function search_linked_list_real64_head(lst_head, val) result(find)
    class(linked_list_real64_head), intent(in) :: lst_head
    real(real64), intent(in) :: val
    type(linked_list_real64), pointer :: lst_elem
    if (.not. associated(lst_head%head_)) return
    lst_elem => lst_head%head_
    find = .false.
    do
       if (.not. associated(lst_elem)) return
       if (lst_elem%val_ == val) then
          find = .true.
          return
       end if
       lst_elem => lst_elem%next_
    end do
  end function search_linked_list_real64_head
  
end module linked_list_m
