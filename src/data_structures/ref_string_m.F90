module ref_string_m
  use, intrinsic :: iso_fortran_env
  implicit none
  private string_row
  type :: string_row
     integer(int32) :: ref_cnt_ = 0_int32
     integer(int32) :: size_ = 0_int32
     character(len=:), allocatable :: str_
   contains
     ! `ref_string` has the responsibility for deallocating `string_row`.
     ! final :: destroy_string_row
  end type string_row
  private to_string_row
  type :: ref_string
     private
     type(string_row), pointer :: ptr_ => null()
   contains
     procedure, pass :: to_chars => ref_string_to_chars
     procedure, pass :: assign_ref_string, assign_string_row, assign_chars
     generic :: assignment(=) => assign_string_row, assign_chars
     procedure, pass :: rc => rc_ref_string
     procedure, pass :: write_ref_string
     generic :: write(formatted) => write_ref_string
     procedure, pass :: read_ref_string
     generic :: read(formatted) => read_ref_string
     final :: destroy_ref_string
  end type ref_string
  private init_ref_string_empty
  interface ref_string
     module procedure :: init_ref_string_empty
  end interface ref_string
  interface assignment(=)
     module procedure :: assign_ref_string_to_ref_string, assign_ref_string_to_chars
  end interface assignment(=)
  private equal_ref_string, not_equal_ref_string
  private less_ref_string, less_equal_ref_string, greater_ref_string, greater_equal_ref_string
  interface len
     module procedure :: len_ref_string
  end interface len
  interface operator(==)
     module procedure :: equal_ref_string
  end interface operator(==)
  interface operator(/=)
     module procedure :: not_equal_ref_string
  end interface operator(/=)
  interface operator(<)
     module procedure :: less_ref_string
  end interface operator(<)
  interface operator(<=)
     module procedure :: less_equal_ref_string
  end interface operator(<=)
  interface operator(>)
     module procedure :: greater_ref_string
  end interface operator(>)
  interface operator(>=)
     module procedure :: greater_equal_ref_string
  end interface operator(>=)
contains
  !> to_string_row: convert `character(len=*)` to `string_row` and increment `ref_cnts_`.
  function to_string_row(chars) result(res)
    character(len=*), intent(in) :: chars
    type(string_row), pointer :: res
    allocate(res)
    allocate(res%str_, source = chars)
    res%size_ = len(chars)
    res%ref_cnt_ = 1_int32
  end function to_string_row
  !> ref_string_to_chars: convert `ref_string` to `character(len=*)`.
  function ref_string_to_chars(ref_str) result(res)
    class(ref_string), intent(in) :: ref_str
    character(len=:), allocatable :: res
    allocate(res, source = ref_str%ptr_%str_)
  end function ref_string_to_chars
  function init_ref_string_empty() result(res)
    type(ref_string) :: res
    nullify(res%ptr_)
  end function init_ref_string_empty

  subroutine destroy_string_row(str_row)
    type(string_row), intent(inout) :: str_row
    ! write(error_unit, *) "ですとろーい `string_row`: ", str_row%ref_cnt_, str_row%str_
    str_row%ref_cnt_ = str_row%ref_cnt_ - 1
    if (str_row%ref_cnt_ == 0) then
       ! write(error_unit, *) "ですとろーいど `string_row: `", str_row%str_, loc(str_row%str_)
       deallocate(str_row%str_)
    end if
  end subroutine destroy_string_row
  subroutine destroy_ref_string(ref_str)
    type(ref_string), intent(inout) :: ref_str
    if (.not. associated(ref_str%ptr_)) return
    ! write(error_unit, *) "ですとろーい `ref_string`: ", ref_str%rc(), ref_str
    call destroy_string_row(ref_str%ptr_)
    if (ref_str%rc() == 0) then
         ! write(error_unit, *) "ですとろーいど `ref_string`: ", loc(ref_str%ptr_), associated(ref_str%ptr_)
         deallocate(ref_str%ptr_)
    end if
    nullify(ref_str%ptr_)
  end subroutine destroy_ref_string

  integer(int32) function len_ref_string(ref_str) result(res)
    type(ref_string), intent(in) :: ref_str
    if (associated(ref_str%ptr_)) then
       res = 0_int32
    else
       res = ref_str%ptr_%size_
    end if
  end function len_ref_string
  subroutine assign_ref_string_to_ref_string(ref_str1, ref_str2)
    type(ref_string), intent(inout) :: ref_str1
    type(ref_string), intent(in) :: ref_str2
    call ref_str1%assign_ref_string(ref_str2)
  end subroutine assign_ref_string_to_ref_string
  subroutine assign_ref_string(this, ref_str)
    class(ref_string), intent(inout) :: this
    type(ref_string), intent(in) :: ref_str
    type(string_row), pointer :: tmp
    tmp => this%ptr_
    if (associated(ref_str%ptr_)) then
       this%ptr_ => ref_str%ptr_
       this%ptr_%ref_cnt_ = this%ptr_%ref_cnt_ + 1
    else
       nullify(this%ptr_)
    end if
    if (associated(tmp)) &
         call destroy_string_row(tmp)
  end subroutine assign_ref_string
  subroutine assign_string_row(this, str_row)
    class(ref_string), intent(inout) :: this
    type(string_row), pointer, intent(in) :: str_row
    type(string_row), pointer :: tmp
    tmp => this%ptr_
    this%ptr_ => str_row
    if (associated(tmp)) &
         call destroy_string_row(tmp)
  end subroutine assign_string_row
  subroutine assign_chars(this, chars)
    class(ref_string), intent(inout) :: this
    character(len=*), intent(in) :: chars
    this = to_string_row(chars)
  end subroutine assign_chars
  subroutine assign_ref_string_to_chars(chars, ref_str)
    character(len=*), intent(inout) :: chars
    type(ref_string), intent(in) :: ref_str
    if (.not. associated(ref_str%ptr_)) &
         return
    if (len(chars) < len(ref_str)) then
       chars(1:len(chars)) = ref_str%ptr_%str_(1:len(chars))
    else
       chars(1:len(ref_str)) = ref_str%ptr_%str_
    end if
  end subroutine assign_ref_string_to_chars
  pure integer(int32) function rc_ref_string(this) result(res)
    class(ref_string), intent(in) :: this
    if (associated(this%ptr_)) then
       res = this%ptr_%ref_cnt_
    else
       res = -1_int32
    end if
  end function rc_ref_string

  pure logical function equal_ref_string(lhs, rhs) result(res)
    type(ref_string), intent(in) :: lhs, rhs
    res = lhs%ptr_%str_ == rhs%ptr_%str_
  end function equal_ref_string
  pure logical function not_equal_ref_string(lhs, rhs) result(res)
    type(ref_string), intent(in) :: lhs, rhs
    res = .not. (lhs == rhs)
  end function not_equal_ref_string
  pure logical function less_ref_string(lhs, rhs) result(res)
    type(ref_string), intent(in) :: lhs, rhs
    res = lhs%ptr_%str_ < rhs%ptr_%str_
  end function less_ref_string
  pure logical function less_equal_ref_string(lhs, rhs) result(res)
    type(ref_string), intent(in) :: lhs, rhs
    res = lhs%ptr_%str_ <= rhs%ptr_%str_
  end function less_equal_ref_string
  pure logical function greater_ref_string(lhs, rhs) result(res)
    type(ref_string), intent(in) :: lhs, rhs
    res = .not. (lhs <= rhs)
  end function greater_ref_string
  pure logical function greater_equal_ref_string(lhs, rhs) result(res)
    type(ref_string), intent(in) :: lhs, rhs
    res = .not. (lhs < rhs)
  end function greater_equal_ref_string

  subroutine write_ref_string(this, unit, iotype, vlist, iostat, iomsg)
    class(ref_string), intent(in) :: this
    integer(int32), intent(in) :: unit
    character(len=*), intent(in) :: iotype
    integer(int32), intent(in) :: vlist(:)
    integer(int32), intent(out) :: iostat
    character(len=*), intent(inout) :: iomsg
    if (associated(this%ptr_)) then
       if (iotype == "LISTDIRECTED" .or. size(vlist) == 0) then
          write(unit, *, iostat = iostat, iomsg = iomsg) this%ptr_%str_
       else if (iotype(3:) == "ref_string" .or. iotype(3:) == "a") then
          block
            character(len=10) :: width
            character(len=:), allocatable :: fmt
            if (size(vlist) == 0) then
               allocate(fmt, source = '(a)')
            else
               write(width, '(i0)') vlist(1)
               allocate(fmt, source = '(a'//trim(width)//')')
            end if
            write(unit, fmt, iostat = iostat, iomsg = iomsg) this%ptr_%str_
          end block
       else
          ! iostat = -1
          ! iomsg = "The type `"//iotype//"` is not supported in ref_string."
          write(error_unit, '(a)') "The type `"//iotype//"` is not supported in ref_string."
          error stop 2
       end if
    else
       write(unit, *, iostat = iostat, iomsg = iomsg)
    end if
  end subroutine write_ref_string

  subroutine read_ref_string(this, unit, iotype, vlist, iostat, iomsg)
    class(ref_string), intent(inout) :: this
    integer(int32), intent(in) :: unit
    character(len=*), intent(in) :: iotype
    integer(int32), intent(in) :: vlist(:)
    integer(int32), intent(out) :: iostat
    character(len=*), intent(inout) :: iomsg
    type(string_row), pointer :: tmp
    tmp => this%ptr_
    if (associated(this%ptr_)) then
       if (iotype == "LISTDIRECTED" .or. size(vlist) == 0) then
          write(unit, *, iostat = iostat, iomsg = iomsg) this%ptr_%str_
       else if (iotype(3:) == "ref_string" .or. iotype(3:) == "a") then
          block
            character(len=10) :: width
            character(len=:), allocatable :: fmt
            if (size(vlist) == 0) then
               allocate(fmt, source = '(a)')
            else
               write(width, '(i0)') vlist(1)
               allocate(fmt, source = '(a'//trim(width)//')')
            end if
            write(unit, fmt, iostat = iostat, iomsg = iomsg) this%ptr_%str_
          end block
       else
          ! iostat = -1
          ! iomsg = "The type `"//iotype//"` is not supported in ref_string."
          write(error_unit, '(a)') "The type `"//iotype//"` is not supported in ref_string."
          error stop 2
       end if
    else
       write(unit, *, iostat = iostat, iomsg = iomsg)
    end if
    if (associated(tmp)) &
       call destroy_string_row(tmp)
  end subroutine read_ref_string
end module ref_string_m
