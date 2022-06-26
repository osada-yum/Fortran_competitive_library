module hash_table_m
  use, intrinsic :: iso_fortran_env
  implicit none
  private
  integer, parameter :: max_elem = 701, small_m = 700, cardinal = 128
  public :: size

  type :: variable_char
     character(len=:), allocatable :: s
  end type variable_char
  public :: hash_table_int32
  type :: hash_table_int32
     private
     integer(int32) :: num_elems_
     type(variable_char), allocatable :: keys_(:)
     integer(int32), allocatable :: elems_(:)
     logical, allocatable :: vacancy_(:)
     logical, allocatable :: deleted_(:)
   contains
     procedure, pass :: insert => insert_hash_table_int32
     procedure, pass :: delete => delete_hash_table_int32
     procedure, pass :: search => search_hash_table_int32
     procedure, pass :: write_hash_table_int32
     generic :: write(formatted) => write_hash_table_int32
  end type hash_table_int32
  
  interface hash_table_int32
     module procedure :: init_hash_table_int32
  end interface hash_table_int32
  interface size
     module procedure :: size_hash_table_int32
  end interface
  
  public :: hash_table_int64
  type :: hash_table_int64
     private
     integer(int32) :: num_elems_
     type(variable_char), allocatable :: keys_(:)
     integer(int64), allocatable :: elems_(:)
     logical, allocatable :: vacancy_(:)
     logical, allocatable :: deleted_(:)
   contains
     procedure, pass :: insert => insert_hash_table_int64
     procedure, pass :: delete => delete_hash_table_int64
     procedure, pass :: search => search_hash_table_int64
     procedure, pass :: write_hash_table_int64
     generic :: write(formatted) => write_hash_table_int64
  end type hash_table_int64
  
  interface hash_table_int64
     module procedure :: init_hash_table_int64
  end interface hash_table_int64
  interface size
     module procedure :: size_hash_table_int64
  end interface
  
  public :: hash_table_real32
  type :: hash_table_real32
     private
     integer(int32) :: num_elems_
     type(variable_char), allocatable :: keys_(:)
     real(real32), allocatable :: elems_(:)
     logical, allocatable :: vacancy_(:)
     logical, allocatable :: deleted_(:)
   contains
     procedure, pass :: insert => insert_hash_table_real32
     procedure, pass :: delete => delete_hash_table_real32
     procedure, pass :: search => search_hash_table_real32
     procedure, pass :: write_hash_table_real32
     generic :: write(formatted) => write_hash_table_real32
  end type hash_table_real32
  
  interface hash_table_real32
     module procedure :: init_hash_table_real32
  end interface hash_table_real32
  interface size
     module procedure :: size_hash_table_real32
  end interface
  
  public :: hash_table_real64
  type :: hash_table_real64
     private
     integer(int32) :: num_elems_
     type(variable_char), allocatable :: keys_(:)
     real(real64), allocatable :: elems_(:)
     logical, allocatable :: vacancy_(:)
     logical, allocatable :: deleted_(:)
   contains
     procedure, pass :: insert => insert_hash_table_real64
     procedure, pass :: delete => delete_hash_table_real64
     procedure, pass :: search => search_hash_table_real64
     procedure, pass :: write_hash_table_real64
     generic :: write(formatted) => write_hash_table_real64
  end type hash_table_real64
  
  interface hash_table_real64
     module procedure :: init_hash_table_real64
  end interface hash_table_real64
  interface size
     module procedure :: size_hash_table_real64
  end interface
  
contains

  pure integer(int32) function hash1(key)
    character(len=*), intent(in) :: key
    integer(int32) :: h, i
    h = 0_int32
    do i = len(key), 1, -1
       h = mod(h * cardinal + ichar(key(i:i)), max_elem)
    end do
    hash1 = h
  end function hash1
  pure integer(int32) function hash2(key)
    character(len=*), intent(in) :: key
    integer(int32) :: h, i
    h = 0_int32
    do i = len(key), 1, -1
       h = mod(h * cardinal + ichar(key(i:i)), small_m)
    end do
    hash2 = h + 1
  end function hash2

  impure type(hash_table_int32) function init_hash_table_int32() result(res)
    res%num_elems_ = 0
    allocate(res%elems_(0:max_elem-1))
    allocate(res%keys_(0:max_elem-1))
    allocate(res%vacancy_(0:max_elem-1), source = .true.)
    allocate(res%deleted_(0:max_elem-1), source = .false.)
  end function init_hash_table_int32
  pure integer(int32) function size_hash_table_int32(ht) result(res)
    type(hash_table_int32), intent(in) :: ht
    res = ht%num_elems_
  end function size_hash_table_int32
  subroutine insert_hash_table_int32 (this, key, val, ierr)
    class(hash_table_int32), intent(inout) :: this
    character(len=*), intent(in) :: key
    integer(int32), intent(in) :: val
    integer(int32), optional, intent(out) :: ierr
    integer(int32) :: h1, h2, pos, i
    if (present(ierr)) ierr = 0
    h1 = hash1(key)
    h2 = hash2(key)
    pos = h1
    do i = 1, max_elem
       if (this%vacancy_(pos)) then
          this%keys_(pos)%s = key
          this%elems_(pos) = val
          this%vacancy_(pos) = .false.
          this%num_elems_ = this%num_elems_ + 1
          return
       else if (this%keys_(pos)%s == key) then
          this%elems_(pos) = val
          return
       end if
       pos = mod(pos + h2, max_elem)
    end do
    write(error_unit, '(a)') "Size limit: Hash table is too large."
    write(error_unit, '(a, i0)') __FILE__//": ", __LINE__
    if (present(ierr)) then
       ierr = 1
    else
       error stop 1
    end if
  end subroutine insert_hash_table_int32
  subroutine delete_hash_table_int32 (this, key, found)
    class(hash_table_int32), intent(inout) :: this
    character(len=*), intent(in) :: key
    logical, optional, intent(out) :: found
    integer(int32) :: h1, h2, pos, i
    h1 = hash1(key)
    h2 = hash2(key)
    pos = h1
    do i = 1, max_elem
       if (this%vacancy_(pos) .and. (.not. this%deleted_(pos))) exit
       if (this%keys_(pos)%s == key) then
          this%vacancy_(pos) = .true.
          this%deleted_(pos) = .true.
          this%num_elems_ = this%num_elems_ - 1
          if (present(found)) found = .true.
          return
       end if
       pos = mod(pos + h2, max_elem)
    end do
    if (present(found)) found = .false.
  end subroutine delete_hash_table_int32
  integer(int32) function search_hash_table_int32 (this, key, found) result(res)
    class(hash_table_int32), intent(in) :: this
    character(len=*), intent(in) :: key
    logical, optional, intent(out) :: found
    integer(int32) :: h1, h2, pos, i
    res = -1
    h1 = hash1(key)
    h2 = hash2(key)
    pos = h1
    do i = 1, max_elem
       if (this%vacancy_(pos) .and. (.not. this%deleted_(pos))) exit
       if (this%keys_(pos)%s == key) then
          res = this%elems_(pos)
          if (present(found)) found = .true.
          return
       end if
       pos = mod(pos + h2, max_elem)
    end do
    if (present(found)) found = .false.
  end function search_hash_table_int32
  subroutine write_hash_table_int32(this, unit, iotype, v_list, iostat, iomsg)
    class(hash_table_int32), intent(in)    :: this
    integer             , intent(in)    :: unit
    character(len=*)    , intent(in)    :: iotype
    integer             , intent(in)    :: v_list(:)
    integer             , intent(out)   :: iostat
    character(len=*)    , intent(inout) :: iomsg
    integer(int32) :: i
    do i = 0, max_elem-1
       if (.not. this%vacancy_(i)) then
          write(unit, fmt='(a, i0, a, g18.10)', advance = "No", iostat=iostat, iomsg=iomsg) &
               "|", i, ": ht["//this%keys_(i)%s//"] => ", this%elems_(i)
       end if
    end do
  end subroutine write_hash_table_int32
  
  impure type(hash_table_int64) function init_hash_table_int64() result(res)
    res%num_elems_ = 0
    allocate(res%elems_(0:max_elem-1))
    allocate(res%keys_(0:max_elem-1))
    allocate(res%vacancy_(0:max_elem-1), source = .true.)
    allocate(res%deleted_(0:max_elem-1), source = .false.)
  end function init_hash_table_int64
  pure integer(int32) function size_hash_table_int64(ht) result(res)
    type(hash_table_int64), intent(in) :: ht
    res = ht%num_elems_
  end function size_hash_table_int64
  subroutine insert_hash_table_int64 (this, key, val, ierr)
    class(hash_table_int64), intent(inout) :: this
    character(len=*), intent(in) :: key
    integer(int64), intent(in) :: val
    integer(int32), optional, intent(out) :: ierr
    integer(int32) :: h1, h2, pos, i
    if (present(ierr)) ierr = 0
    h1 = hash1(key)
    h2 = hash2(key)
    pos = h1
    do i = 1, max_elem
       if (this%vacancy_(pos)) then
          this%keys_(pos)%s = key
          this%elems_(pos) = val
          this%vacancy_(pos) = .false.
          this%num_elems_ = this%num_elems_ + 1
          return
       else if (this%keys_(pos)%s == key) then
          this%elems_(pos) = val
          return
       end if
       pos = mod(pos + h2, max_elem)
    end do
    write(error_unit, '(a)') "Size limit: Hash table is too large."
    write(error_unit, '(a, i0)') __FILE__//": ", __LINE__
    if (present(ierr)) then
       ierr = 1
    else
       error stop 1
    end if
  end subroutine insert_hash_table_int64
  subroutine delete_hash_table_int64 (this, key, found)
    class(hash_table_int64), intent(inout) :: this
    character(len=*), intent(in) :: key
    logical, optional, intent(out) :: found
    integer(int32) :: h1, h2, pos, i
    h1 = hash1(key)
    h2 = hash2(key)
    pos = h1
    do i = 1, max_elem
       if (this%vacancy_(pos) .and. (.not. this%deleted_(pos))) exit
       if (this%keys_(pos)%s == key) then
          this%vacancy_(pos) = .true.
          this%deleted_(pos) = .true.
          this%num_elems_ = this%num_elems_ - 1
          if (present(found)) found = .true.
          return
       end if
       pos = mod(pos + h2, max_elem)
    end do
    if (present(found)) found = .false.
  end subroutine delete_hash_table_int64
  integer(int64) function search_hash_table_int64 (this, key, found) result(res)
    class(hash_table_int64), intent(in) :: this
    character(len=*), intent(in) :: key
    logical, optional, intent(out) :: found
    integer(int32) :: h1, h2, pos, i
    res = -1
    h1 = hash1(key)
    h2 = hash2(key)
    pos = h1
    do i = 1, max_elem
       if (this%vacancy_(pos) .and. (.not. this%deleted_(pos))) exit
       if (this%keys_(pos)%s == key) then
          res = this%elems_(pos)
          if (present(found)) found = .true.
          return
       end if
       pos = mod(pos + h2, max_elem)
    end do
    if (present(found)) found = .false.
  end function search_hash_table_int64
  subroutine write_hash_table_int64(this, unit, iotype, v_list, iostat, iomsg)
    class(hash_table_int64), intent(in)    :: this
    integer             , intent(in)    :: unit
    character(len=*)    , intent(in)    :: iotype
    integer             , intent(in)    :: v_list(:)
    integer             , intent(out)   :: iostat
    character(len=*)    , intent(inout) :: iomsg
    integer(int32) :: i
    do i = 0, max_elem-1
       if (.not. this%vacancy_(i)) then
          write(unit, fmt='(a, i0, a, g18.10)', advance = "No", iostat=iostat, iomsg=iomsg) &
               "|", i, ": ht["//this%keys_(i)%s//"] => ", this%elems_(i)
       end if
    end do
  end subroutine write_hash_table_int64
  
  impure type(hash_table_real32) function init_hash_table_real32() result(res)
    res%num_elems_ = 0
    allocate(res%elems_(0:max_elem-1))
    allocate(res%keys_(0:max_elem-1))
    allocate(res%vacancy_(0:max_elem-1), source = .true.)
    allocate(res%deleted_(0:max_elem-1), source = .false.)
  end function init_hash_table_real32
  pure integer(int32) function size_hash_table_real32(ht) result(res)
    type(hash_table_real32), intent(in) :: ht
    res = ht%num_elems_
  end function size_hash_table_real32
  subroutine insert_hash_table_real32 (this, key, val, ierr)
    class(hash_table_real32), intent(inout) :: this
    character(len=*), intent(in) :: key
    real(real32), intent(in) :: val
    integer(int32), optional, intent(out) :: ierr
    integer(int32) :: h1, h2, pos, i
    if (present(ierr)) ierr = 0
    h1 = hash1(key)
    h2 = hash2(key)
    pos = h1
    do i = 1, max_elem
       if (this%vacancy_(pos)) then
          this%keys_(pos)%s = key
          this%elems_(pos) = val
          this%vacancy_(pos) = .false.
          this%num_elems_ = this%num_elems_ + 1
          return
       else if (this%keys_(pos)%s == key) then
          this%elems_(pos) = val
          return
       end if
       pos = mod(pos + h2, max_elem)
    end do
    write(error_unit, '(a)') "Size limit: Hash table is too large."
    write(error_unit, '(a, i0)') __FILE__//": ", __LINE__
    if (present(ierr)) then
       ierr = 1
    else
       error stop 1
    end if
  end subroutine insert_hash_table_real32
  subroutine delete_hash_table_real32 (this, key, found)
    class(hash_table_real32), intent(inout) :: this
    character(len=*), intent(in) :: key
    logical, optional, intent(out) :: found
    integer(int32) :: h1, h2, pos, i
    h1 = hash1(key)
    h2 = hash2(key)
    pos = h1
    do i = 1, max_elem
       if (this%vacancy_(pos) .and. (.not. this%deleted_(pos))) exit
       if (this%keys_(pos)%s == key) then
          this%vacancy_(pos) = .true.
          this%deleted_(pos) = .true.
          this%num_elems_ = this%num_elems_ - 1
          if (present(found)) found = .true.
          return
       end if
       pos = mod(pos + h2, max_elem)
    end do
    if (present(found)) found = .false.
  end subroutine delete_hash_table_real32
  real(real32) function search_hash_table_real32 (this, key, found) result(res)
    class(hash_table_real32), intent(in) :: this
    character(len=*), intent(in) :: key
    logical, optional, intent(out) :: found
    integer(int32) :: h1, h2, pos, i
    res = -1
    h1 = hash1(key)
    h2 = hash2(key)
    pos = h1
    do i = 1, max_elem
       if (this%vacancy_(pos) .and. (.not. this%deleted_(pos))) exit
       if (this%keys_(pos)%s == key) then
          res = this%elems_(pos)
          if (present(found)) found = .true.
          return
       end if
       pos = mod(pos + h2, max_elem)
    end do
    if (present(found)) found = .false.
  end function search_hash_table_real32
  subroutine write_hash_table_real32(this, unit, iotype, v_list, iostat, iomsg)
    class(hash_table_real32), intent(in)    :: this
    integer             , intent(in)    :: unit
    character(len=*)    , intent(in)    :: iotype
    integer             , intent(in)    :: v_list(:)
    integer             , intent(out)   :: iostat
    character(len=*)    , intent(inout) :: iomsg
    integer(int32) :: i
    do i = 0, max_elem-1
       if (.not. this%vacancy_(i)) then
          write(unit, fmt='(a, i0, a, g18.10)', advance = "No", iostat=iostat, iomsg=iomsg) &
               "|", i, ": ht["//this%keys_(i)%s//"] => ", this%elems_(i)
       end if
    end do
  end subroutine write_hash_table_real32
  
  impure type(hash_table_real64) function init_hash_table_real64() result(res)
    res%num_elems_ = 0
    allocate(res%elems_(0:max_elem-1))
    allocate(res%keys_(0:max_elem-1))
    allocate(res%vacancy_(0:max_elem-1), source = .true.)
    allocate(res%deleted_(0:max_elem-1), source = .false.)
  end function init_hash_table_real64
  pure integer(int32) function size_hash_table_real64(ht) result(res)
    type(hash_table_real64), intent(in) :: ht
    res = ht%num_elems_
  end function size_hash_table_real64
  subroutine insert_hash_table_real64 (this, key, val, ierr)
    class(hash_table_real64), intent(inout) :: this
    character(len=*), intent(in) :: key
    real(real64), intent(in) :: val
    integer(int32), optional, intent(out) :: ierr
    integer(int32) :: h1, h2, pos, i
    if (present(ierr)) ierr = 0
    h1 = hash1(key)
    h2 = hash2(key)
    pos = h1
    do i = 1, max_elem
       if (this%vacancy_(pos)) then
          this%keys_(pos)%s = key
          this%elems_(pos) = val
          this%vacancy_(pos) = .false.
          this%num_elems_ = this%num_elems_ + 1
          return
       else if (this%keys_(pos)%s == key) then
          this%elems_(pos) = val
          return
       end if
       pos = mod(pos + h2, max_elem)
    end do
    write(error_unit, '(a)') "Size limit: Hash table is too large."
    write(error_unit, '(a, i0)') __FILE__//": ", __LINE__
    if (present(ierr)) then
       ierr = 1
    else
       error stop 1
    end if
  end subroutine insert_hash_table_real64
  subroutine delete_hash_table_real64 (this, key, found)
    class(hash_table_real64), intent(inout) :: this
    character(len=*), intent(in) :: key
    logical, optional, intent(out) :: found
    integer(int32) :: h1, h2, pos, i
    h1 = hash1(key)
    h2 = hash2(key)
    pos = h1
    do i = 1, max_elem
       if (this%vacancy_(pos) .and. (.not. this%deleted_(pos))) exit
       if (this%keys_(pos)%s == key) then
          this%vacancy_(pos) = .true.
          this%deleted_(pos) = .true.
          this%num_elems_ = this%num_elems_ - 1
          if (present(found)) found = .true.
          return
       end if
       pos = mod(pos + h2, max_elem)
    end do
    if (present(found)) found = .false.
  end subroutine delete_hash_table_real64
  real(real64) function search_hash_table_real64 (this, key, found) result(res)
    class(hash_table_real64), intent(in) :: this
    character(len=*), intent(in) :: key
    logical, optional, intent(out) :: found
    integer(int32) :: h1, h2, pos, i
    res = -1
    h1 = hash1(key)
    h2 = hash2(key)
    pos = h1
    do i = 1, max_elem
       if (this%vacancy_(pos) .and. (.not. this%deleted_(pos))) exit
       if (this%keys_(pos)%s == key) then
          res = this%elems_(pos)
          if (present(found)) found = .true.
          return
       end if
       pos = mod(pos + h2, max_elem)
    end do
    if (present(found)) found = .false.
  end function search_hash_table_real64
  subroutine write_hash_table_real64(this, unit, iotype, v_list, iostat, iomsg)
    class(hash_table_real64), intent(in)    :: this
    integer             , intent(in)    :: unit
    character(len=*)    , intent(in)    :: iotype
    integer             , intent(in)    :: v_list(:)
    integer             , intent(out)   :: iostat
    character(len=*)    , intent(inout) :: iomsg
    integer(int32) :: i
    do i = 0, max_elem-1
       if (.not. this%vacancy_(i)) then
          write(unit, fmt='(a, i0, a, g18.10)', advance = "No", iostat=iostat, iomsg=iomsg) &
               "|", i, ": ht["//this%keys_(i)%s//"] => ", this%elems_(i)
       end if
    end do
  end subroutine write_hash_table_real64
  
end module hash_table_m
