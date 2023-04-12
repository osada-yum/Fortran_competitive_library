module binary_indexed_tree_m
  use, intrinsic :: iso_fortran_env
  implicit none
  private
  public :: binary_indexed_tree_int32
  !> binary_indexed_tree_int32: can calculate the range sum O(log n).
  type :: binary_indexed_tree_int32
     private
     integer(int32), allocatable :: arr_(:)
     integer(int32) :: size_ = 0
   contains
     procedure, pass :: init_binary_indexed_tree_int32_by_size, init_binary_indexed_tree_int32_by_arr
     generic         :: init  => init_binary_indexed_tree_int32_by_size, init_binary_indexed_tree_int32_by_arr
     procedure, pass :: reset => reset_binary_indexed_tree_int32
     procedure, pass :: size  => size_binary_indexed_tree_int32
     procedure, pass :: add       => add_binary_indexed_tree_int32
     procedure, pass :: sum1      => sum1_binary_indexed_tree_int32
     procedure, pass :: sum_range => sum_range_binary_indexed_tree_int32
     final :: destroy_binary_indexed_tree_int32
  end type binary_indexed_tree_int32
  
  public :: binary_indexed_tree_int64
  !> binary_indexed_tree_int64: can calculate the range sum O(log n).
  type :: binary_indexed_tree_int64
     private
     integer(int64), allocatable :: arr_(:)
     integer(int32) :: size_ = 0
   contains
     procedure, pass :: init_binary_indexed_tree_int64_by_size, init_binary_indexed_tree_int64_by_arr
     generic         :: init  => init_binary_indexed_tree_int64_by_size, init_binary_indexed_tree_int64_by_arr
     procedure, pass :: reset => reset_binary_indexed_tree_int64
     procedure, pass :: size  => size_binary_indexed_tree_int64
     procedure, pass :: add       => add_binary_indexed_tree_int64
     procedure, pass :: sum1      => sum1_binary_indexed_tree_int64
     procedure, pass :: sum_range => sum_range_binary_indexed_tree_int64
     final :: destroy_binary_indexed_tree_int64
  end type binary_indexed_tree_int64
  
  public :: binary_indexed_tree_real32
  !> binary_indexed_tree_real32: can calculate the range sum O(log n).
  type :: binary_indexed_tree_real32
     private
     real(real32), allocatable :: arr_(:)
     integer(int32) :: size_ = 0
   contains
     procedure, pass :: init_binary_indexed_tree_real32_by_size, init_binary_indexed_tree_real32_by_arr
     generic         :: init  => init_binary_indexed_tree_real32_by_size, init_binary_indexed_tree_real32_by_arr
     procedure, pass :: reset => reset_binary_indexed_tree_real32
     procedure, pass :: size  => size_binary_indexed_tree_real32
     procedure, pass :: add       => add_binary_indexed_tree_real32
     procedure, pass :: sum1      => sum1_binary_indexed_tree_real32
     procedure, pass :: sum_range => sum_range_binary_indexed_tree_real32
     final :: destroy_binary_indexed_tree_real32
  end type binary_indexed_tree_real32
  
  public :: binary_indexed_tree_real64
  !> binary_indexed_tree_real64: can calculate the range sum O(log n).
  type :: binary_indexed_tree_real64
     private
     real(real64), allocatable :: arr_(:)
     integer(int32) :: size_ = 0
   contains
     procedure, pass :: init_binary_indexed_tree_real64_by_size, init_binary_indexed_tree_real64_by_arr
     generic         :: init  => init_binary_indexed_tree_real64_by_size, init_binary_indexed_tree_real64_by_arr
     procedure, pass :: reset => reset_binary_indexed_tree_real64
     procedure, pass :: size  => size_binary_indexed_tree_real64
     procedure, pass :: add       => add_binary_indexed_tree_real64
     procedure, pass :: sum1      => sum1_binary_indexed_tree_real64
     procedure, pass :: sum_range => sum_range_binary_indexed_tree_real64
     final :: destroy_binary_indexed_tree_real64
  end type binary_indexed_tree_real64
  
contains
  !> init_binary_indexed_tree_int32_by_size: Initialize the binary_indexed_tree_int32 by size.
  !> All elements of binary_indexed_tree_int32 is 0_int32.
  subroutine init_binary_indexed_tree_int32_by_size(this, n)
    class(binary_indexed_tree_int32), intent(inout) :: this
    integer(int32), intent(in) :: n
    !> Error exist if already allocated.
    if (allocated(this%arr_)) then
       write(error_unit, '(a, i0, a)', advance = "no")&
            "Error in "//&
            __FILE__&
            //":", __LINE__, ":"
         write(error_unit, '(a)')&
           "This binary_indexed_tree_int32 is already allocated."
         error stop 1
       
    end if
    allocate(this%arr_(n), source = 0_int32)
    this%size_ = n
  end subroutine init_binary_indexed_tree_int32_by_size
  
  !> init_binary_indexed_tree_int32_by_arr: Initialize the binary_indexed_tree_int32 by array.
  subroutine init_binary_indexed_tree_int32_by_arr(this, arr)
    class(binary_indexed_tree_int32), intent(inout) :: this
    integer(int32), intent(in) ::arr(:)
    integer(int32) :: i, arr_size
    arr_size = size(arr)
    call this%init(arr_size)
    do i = 1, arr_size
       call this%add(i, arr(i))
    end do
  end subroutine init_binary_indexed_tree_int32_by_arr
  !> reset_binary_indexed_tree_int32: Replace  with .
  subroutine reset_binary_indexed_tree_int32(this)
    class(binary_indexed_tree_int32), intent(inout) :: this
    if (allocated(this%arr_)) then
       this%arr_(:) = 0_int32
    end if
  end subroutine reset_binary_indexed_tree_int32
  !> size_binary_indexed_tree_int32: Return current size of the binary_indexed_tree_int32.
  pure integer(int32) function size_binary_indexed_tree_int32(this) result(res)
    class(binary_indexed_tree_int32), intent(in) :: this
    res = this%size_
  end function size_binary_indexed_tree_int32
  !> add_binary_indexed_tree_int32: Add the value  into the index  of .
  subroutine add_binary_indexed_tree_int32(this, idx, val)
    class(binary_indexed_tree_int32), intent(inout) :: this
    integer(int32), intent(in) :: idx
    integer(int32), intent(in) :: val
    integer(int32) :: i
    i = idx
    do
       if (i > this%size_) exit
       this%arr_(i) = this%arr_(i) + val
       i = i + iand(i, -i)
    end do
  end subroutine add_binary_indexed_tree_int32
  !> sum1_binary_indexed_tree_int32: Return the summation of .
  !> Return 0_int32 if r < 0.
  integer(int32) function sum1_binary_indexed_tree_int32(this, r) result(res)
    class(binary_indexed_tree_int32), intent(in) :: this
    integer(int32), intent(in) :: r
    integer(int32) :: i
    res = 0_int32
    i = r
    do
       if (i < 1) return
       res = res + this%arr_(i)
       i = i - iand(i, -i)
    end do
  end function sum1_binary_indexed_tree_int32
  !> sum_range_binary_indexed_tree_int32: Return the summation of 
  !> Returun 0_int32 if r < l.
  integer(int32) function sum_range_binary_indexed_tree_int32(this, l, r) result(res)
    class(binary_indexed_tree_int32), intent(in) :: this
    integer(int32), intent(in) :: l, r
    res = 0_int32
    if (r < l) return
    res = this%sum1(r) - this%sum1(l-1)
  end function sum_range_binary_indexed_tree_int32
  !> destroy_binary_indexed_tree_int32: Replace  with .
  subroutine destroy_binary_indexed_tree_int32(this)
    type(binary_indexed_tree_int32), intent(inout) :: this
    if (allocated(this%arr_)) then
       deallocate(this%arr_)
    end if
  end subroutine destroy_binary_indexed_tree_int32
  
  !> init_binary_indexed_tree_int64_by_size: Initialize the binary_indexed_tree_int64 by size.
  !> All elements of binary_indexed_tree_int64 is 0_int64.
  subroutine init_binary_indexed_tree_int64_by_size(this, n)
    class(binary_indexed_tree_int64), intent(inout) :: this
    integer(int32), intent(in) :: n
    !> Error exist if already allocated.
    if (allocated(this%arr_)) then
       write(error_unit, '(a, i0, a)', advance = "no")&
            "Error in "//&
            __FILE__&
            //":", __LINE__, ":"
         write(error_unit, '(a)')&
           "This binary_indexed_tree_int64 is already allocated."
         error stop 1
       
    end if
    allocate(this%arr_(n), source = 0_int64)
    this%size_ = n
  end subroutine init_binary_indexed_tree_int64_by_size
  
  !> init_binary_indexed_tree_int64_by_arr: Initialize the binary_indexed_tree_int64 by array.
  subroutine init_binary_indexed_tree_int64_by_arr(this, arr)
    class(binary_indexed_tree_int64), intent(inout) :: this
    integer(int64), intent(in) ::arr(:)
    integer(int32) :: i, arr_size
    arr_size = size(arr)
    call this%init(arr_size)
    do i = 1, arr_size
       call this%add(i, arr(i))
    end do
  end subroutine init_binary_indexed_tree_int64_by_arr
  !> reset_binary_indexed_tree_int64: Replace  with .
  subroutine reset_binary_indexed_tree_int64(this)
    class(binary_indexed_tree_int64), intent(inout) :: this
    if (allocated(this%arr_)) then
       this%arr_(:) = 0_int64
    end if
  end subroutine reset_binary_indexed_tree_int64
  !> size_binary_indexed_tree_int64: Return current size of the binary_indexed_tree_int64.
  pure integer(int32) function size_binary_indexed_tree_int64(this) result(res)
    class(binary_indexed_tree_int64), intent(in) :: this
    res = this%size_
  end function size_binary_indexed_tree_int64
  !> add_binary_indexed_tree_int64: Add the value  into the index  of .
  subroutine add_binary_indexed_tree_int64(this, idx, val)
    class(binary_indexed_tree_int64), intent(inout) :: this
    integer(int32), intent(in) :: idx
    integer(int64), intent(in) :: val
    integer(int32) :: i
    i = idx
    do
       if (i > this%size_) exit
       this%arr_(i) = this%arr_(i) + val
       i = i + iand(i, -i)
    end do
  end subroutine add_binary_indexed_tree_int64
  !> sum1_binary_indexed_tree_int64: Return the summation of .
  !> Return 0_int64 if r < 0.
  integer(int64) function sum1_binary_indexed_tree_int64(this, r) result(res)
    class(binary_indexed_tree_int64), intent(in) :: this
    integer(int32), intent(in) :: r
    integer(int32) :: i
    res = 0_int64
    i = r
    do
       if (i < 1) return
       res = res + this%arr_(i)
       i = i - iand(i, -i)
    end do
  end function sum1_binary_indexed_tree_int64
  !> sum_range_binary_indexed_tree_int64: Return the summation of 
  !> Returun 0_int64 if r < l.
  integer(int64) function sum_range_binary_indexed_tree_int64(this, l, r) result(res)
    class(binary_indexed_tree_int64), intent(in) :: this
    integer(int32), intent(in) :: l, r
    res = 0_int64
    if (r < l) return
    res = this%sum1(r) - this%sum1(l-1)
  end function sum_range_binary_indexed_tree_int64
  !> destroy_binary_indexed_tree_int64: Replace  with .
  subroutine destroy_binary_indexed_tree_int64(this)
    type(binary_indexed_tree_int64), intent(inout) :: this
    if (allocated(this%arr_)) then
       deallocate(this%arr_)
    end if
  end subroutine destroy_binary_indexed_tree_int64
  
  !> init_binary_indexed_tree_real32_by_size: Initialize the binary_indexed_tree_real32 by size.
  !> All elements of binary_indexed_tree_real32 is 0.0_real32.
  subroutine init_binary_indexed_tree_real32_by_size(this, n)
    class(binary_indexed_tree_real32), intent(inout) :: this
    integer(int32), intent(in) :: n
    !> Error exist if already allocated.
    if (allocated(this%arr_)) then
       write(error_unit, '(a, i0, a)', advance = "no")&
            "Error in "//&
            __FILE__&
            //":", __LINE__, ":"
         write(error_unit, '(a)')&
           "This binary_indexed_tree_real32 is already allocated."
         error stop 1
       
    end if
    allocate(this%arr_(n), source = 0.0_real32)
    this%size_ = n
  end subroutine init_binary_indexed_tree_real32_by_size
  
  !> init_binary_indexed_tree_real32_by_arr: Initialize the binary_indexed_tree_real32 by array.
  subroutine init_binary_indexed_tree_real32_by_arr(this, arr)
    class(binary_indexed_tree_real32), intent(inout) :: this
    real(real32), intent(in) ::arr(:)
    integer(int32) :: i, arr_size
    arr_size = size(arr)
    call this%init(arr_size)
    do i = 1, arr_size
       call this%add(i, arr(i))
    end do
  end subroutine init_binary_indexed_tree_real32_by_arr
  !> reset_binary_indexed_tree_real32: Replace  with .
  subroutine reset_binary_indexed_tree_real32(this)
    class(binary_indexed_tree_real32), intent(inout) :: this
    if (allocated(this%arr_)) then
       this%arr_(:) = 0.0_real32
    end if
  end subroutine reset_binary_indexed_tree_real32
  !> size_binary_indexed_tree_real32: Return current size of the binary_indexed_tree_real32.
  pure integer(int32) function size_binary_indexed_tree_real32(this) result(res)
    class(binary_indexed_tree_real32), intent(in) :: this
    res = this%size_
  end function size_binary_indexed_tree_real32
  !> add_binary_indexed_tree_real32: Add the value  into the index  of .
  subroutine add_binary_indexed_tree_real32(this, idx, val)
    class(binary_indexed_tree_real32), intent(inout) :: this
    integer(int32), intent(in) :: idx
    real(real32), intent(in) :: val
    integer(int32) :: i
    i = idx
    do
       if (i > this%size_) exit
       this%arr_(i) = this%arr_(i) + val
       i = i + iand(i, -i)
    end do
  end subroutine add_binary_indexed_tree_real32
  !> sum1_binary_indexed_tree_real32: Return the summation of .
  !> Return 0.0_real32 if r < 0.
  real(real32) function sum1_binary_indexed_tree_real32(this, r) result(res)
    class(binary_indexed_tree_real32), intent(in) :: this
    integer(int32), intent(in) :: r
    integer(int32) :: i
    res = 0.0_real32
    i = r
    do
       if (i < 1) return
       res = res + this%arr_(i)
       i = i - iand(i, -i)
    end do
  end function sum1_binary_indexed_tree_real32
  !> sum_range_binary_indexed_tree_real32: Return the summation of 
  !> Returun 0.0_real32 if r < l.
  real(real32) function sum_range_binary_indexed_tree_real32(this, l, r) result(res)
    class(binary_indexed_tree_real32), intent(in) :: this
    integer(int32), intent(in) :: l, r
    res = 0.0_real32
    if (r < l) return
    res = this%sum1(r) - this%sum1(l-1)
  end function sum_range_binary_indexed_tree_real32
  !> destroy_binary_indexed_tree_real32: Replace  with .
  subroutine destroy_binary_indexed_tree_real32(this)
    type(binary_indexed_tree_real32), intent(inout) :: this
    if (allocated(this%arr_)) then
       deallocate(this%arr_)
    end if
  end subroutine destroy_binary_indexed_tree_real32
  
  !> init_binary_indexed_tree_real64_by_size: Initialize the binary_indexed_tree_real64 by size.
  !> All elements of binary_indexed_tree_real64 is 0.0_real64.
  subroutine init_binary_indexed_tree_real64_by_size(this, n)
    class(binary_indexed_tree_real64), intent(inout) :: this
    integer(int32), intent(in) :: n
    !> Error exist if already allocated.
    if (allocated(this%arr_)) then
       write(error_unit, '(a, i0, a)', advance = "no")&
            "Error in "//&
            __FILE__&
            //":", __LINE__, ":"
         write(error_unit, '(a)')&
           "This binary_indexed_tree_real64 is already allocated."
         error stop 1
       
    end if
    allocate(this%arr_(n), source = 0.0_real64)
    this%size_ = n
  end subroutine init_binary_indexed_tree_real64_by_size
  
  !> init_binary_indexed_tree_real64_by_arr: Initialize the binary_indexed_tree_real64 by array.
  subroutine init_binary_indexed_tree_real64_by_arr(this, arr)
    class(binary_indexed_tree_real64), intent(inout) :: this
    real(real64), intent(in) ::arr(:)
    integer(int32) :: i, arr_size
    arr_size = size(arr)
    call this%init(arr_size)
    do i = 1, arr_size
       call this%add(i, arr(i))
    end do
  end subroutine init_binary_indexed_tree_real64_by_arr
  !> reset_binary_indexed_tree_real64: Replace  with .
  subroutine reset_binary_indexed_tree_real64(this)
    class(binary_indexed_tree_real64), intent(inout) :: this
    if (allocated(this%arr_)) then
       this%arr_(:) = 0.0_real64
    end if
  end subroutine reset_binary_indexed_tree_real64
  !> size_binary_indexed_tree_real64: Return current size of the binary_indexed_tree_real64.
  pure integer(int32) function size_binary_indexed_tree_real64(this) result(res)
    class(binary_indexed_tree_real64), intent(in) :: this
    res = this%size_
  end function size_binary_indexed_tree_real64
  !> add_binary_indexed_tree_real64: Add the value  into the index  of .
  subroutine add_binary_indexed_tree_real64(this, idx, val)
    class(binary_indexed_tree_real64), intent(inout) :: this
    integer(int32), intent(in) :: idx
    real(real64), intent(in) :: val
    integer(int32) :: i
    i = idx
    do
       if (i > this%size_) exit
       this%arr_(i) = this%arr_(i) + val
       i = i + iand(i, -i)
    end do
  end subroutine add_binary_indexed_tree_real64
  !> sum1_binary_indexed_tree_real64: Return the summation of .
  !> Return 0.0_real64 if r < 0.
  real(real64) function sum1_binary_indexed_tree_real64(this, r) result(res)
    class(binary_indexed_tree_real64), intent(in) :: this
    integer(int32), intent(in) :: r
    integer(int32) :: i
    res = 0.0_real64
    i = r
    do
       if (i < 1) return
       res = res + this%arr_(i)
       i = i - iand(i, -i)
    end do
  end function sum1_binary_indexed_tree_real64
  !> sum_range_binary_indexed_tree_real64: Return the summation of 
  !> Returun 0.0_real64 if r < l.
  real(real64) function sum_range_binary_indexed_tree_real64(this, l, r) result(res)
    class(binary_indexed_tree_real64), intent(in) :: this
    integer(int32), intent(in) :: l, r
    res = 0.0_real64
    if (r < l) return
    res = this%sum1(r) - this%sum1(l-1)
  end function sum_range_binary_indexed_tree_real64
  !> destroy_binary_indexed_tree_real64: Replace  with .
  subroutine destroy_binary_indexed_tree_real64(this)
    type(binary_indexed_tree_real64), intent(inout) :: this
    if (allocated(this%arr_)) then
       deallocate(this%arr_)
    end if
  end subroutine destroy_binary_indexed_tree_real64
  
end module binary_indexed_tree_m
