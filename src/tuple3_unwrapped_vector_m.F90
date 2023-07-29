module tuple3_unwrapped_vector_m
  use, intrinsic :: iso_fortran_env
  use tuple3_m
  implicit none
  public :: unwrapped_vector_tuple3_int32_int32_int32
  type :: unwrapped_vector_tuple3_int32_int32_int32
     private
     type(tuple3_int32_int32_int32), allocatable, public :: arr_(:)
     integer(int32) :: size_ = 0, capa_ = 0
   contains
     procedure, pass :: init      => init_unwrapped_vector_tuple3_int32_int32_int32
     procedure, pass :: push_back_unwrapped_vector_tuple3_int32_int32_int32, &
          push_back_array_unwrapped_vector_tuple3_int32_int32_int32
     generic         :: push_back => push_back_unwrapped_vector_tuple3_int32_int32_int32, &
          push_back_array_unwrapped_vector_tuple3_int32_int32_int32
     procedure, pass :: pop_back  => pop_back_unwrapped_vector_tuple3_int32_int32_int32
     procedure, pass :: back      => back_unwrapped_vector_tuple3_int32_int32_int32
     procedure, pass :: size      => size_unwrapped_vector_tuple3_int32_int32_int32
     procedure, pass :: resize    => resize_unwrapped_vector_tuple3_int32_int32_int32
     procedure, pass :: lower_bound => lower_bound_unwrapped_vector_tuple3_int32_int32_int32
  end type unwrapped_vector_tuple3_int32_int32_int32
  interface unwrapped_vector_tuple3_int32_int32_int32
     module procedure :: construct_unwrapped_vector_tuple3_int32_int32_int32_by_size, &
          construct_unwrapped_vector_tuple3_int32_int32_int32_by_arr, &
          construct_unwrapped_vector_tuple3_int32_int32_int32_by_init_val
  end interface unwrapped_vector_tuple3_int32_int32_int32
  
  public :: unwrapped_vector_tuple3_int64_int64_int64
  type :: unwrapped_vector_tuple3_int64_int64_int64
     private
     type(tuple3_int64_int64_int64), allocatable, public :: arr_(:)
     integer(int32) :: size_ = 0, capa_ = 0
   contains
     procedure, pass :: init      => init_unwrapped_vector_tuple3_int64_int64_int64
     procedure, pass :: push_back_unwrapped_vector_tuple3_int64_int64_int64, &
          push_back_array_unwrapped_vector_tuple3_int64_int64_int64
     generic         :: push_back => push_back_unwrapped_vector_tuple3_int64_int64_int64, &
          push_back_array_unwrapped_vector_tuple3_int64_int64_int64
     procedure, pass :: pop_back  => pop_back_unwrapped_vector_tuple3_int64_int64_int64
     procedure, pass :: back      => back_unwrapped_vector_tuple3_int64_int64_int64
     procedure, pass :: size      => size_unwrapped_vector_tuple3_int64_int64_int64
     procedure, pass :: resize    => resize_unwrapped_vector_tuple3_int64_int64_int64
     procedure, pass :: lower_bound => lower_bound_unwrapped_vector_tuple3_int64_int64_int64
  end type unwrapped_vector_tuple3_int64_int64_int64
  interface unwrapped_vector_tuple3_int64_int64_int64
     module procedure :: construct_unwrapped_vector_tuple3_int64_int64_int64_by_size, &
          construct_unwrapped_vector_tuple3_int64_int64_int64_by_arr, &
          construct_unwrapped_vector_tuple3_int64_int64_int64_by_init_val
  end interface unwrapped_vector_tuple3_int64_int64_int64
  
contains
  !> construct_unwrapped_vector_tuple3_int32_int32_int32_by_size: Construct unwrapped_vector_tuple3_int32_int32_int32 by the size, the initial values is unknown.
  impure function construct_unwrapped_vector_tuple3_int32_int32_int32_by_size(size) result(res)
    type(unwrapped_vector_tuple3_int32_int32_int32) :: res
    integer(int32), intent(in) :: size
    call res%init(size)
  end function construct_unwrapped_vector_tuple3_int32_int32_int32_by_size
  !> construct_unwrapped_vector_tuple3_int32_int32_int32_by_arr: Construct unwrapped_vector_tuple3_int32_int32_int32 by the array of type(tuple3_int32_int32_int32).
  impure function construct_unwrapped_vector_tuple3_int32_int32_int32_by_arr(arr) result(res)
    type(unwrapped_vector_tuple3_int32_int32_int32) :: res
    type(tuple3_int32_int32_int32), intent(in) :: arr(:)
    integer(int32) :: n
    n = size(arr)
    call res%init(n)
    res%arr_(1:n) = arr(1:n)
  end function construct_unwrapped_vector_tuple3_int32_int32_int32_by_arr
  !> construct_unwrapped_vector_tuple3_int32_int32_int32_by_init_val: Construct unwrapped_vector_tuple3_int32_int32_int32 by size and the initial values.
  impure function construct_unwrapped_vector_tuple3_int32_int32_int32_by_init_val(size, val) result(res)
    type(unwrapped_vector_tuple3_int32_int32_int32) :: res
    integer(int32), intent(in) :: size
    type(tuple3_int32_int32_int32), intent(in) :: val
    call res%init(size)
    res%arr_(1:size) = val
  end function construct_unwrapped_vector_tuple3_int32_int32_int32_by_init_val
  !> init_unwrapped_vector_tuple3_int32_int32_int32: Initialize the unwrapped_vector_tuple3_int32_int32_int32 by size.
  subroutine init_unwrapped_vector_tuple3_int32_int32_int32(this, n)
    class(unwrapped_vector_tuple3_int32_int32_int32), intent(inout) :: this
    integer(int32), intent(in) :: n
    if (.not. allocated(this%arr_)) then
       allocate(this%arr_(n))
       this%size_ = n
       this%capa_ = n
    end if
  end subroutine init_unwrapped_vector_tuple3_int32_int32_int32
  !> push_back_unwrapped_vector_tuple3_int32_int32_int32: Insert value to the tail of elements of the unwrapped_vector_tuple3_int32_int32_int32.
  subroutine push_back_unwrapped_vector_tuple3_int32_int32_int32(this, val)
    class(unwrapped_vector_tuple3_int32_int32_int32), intent(inout) :: this
    type(tuple3_int32_int32_int32), intent(in) :: val
    if (.not. allocated(this%arr_)) call this%resize(0)
    if (this%size_ == this%capa_) then
       call this%resize(2*this%capa_)
    end if
    this%size_ = this%size_ + 1
    this%arr_(this%size_) = val
  end subroutine push_back_unwrapped_vector_tuple3_int32_int32_int32
  !> push_back_array_unwrapped_vector_tuple3_int32_int32_int32: Insert elemeents of array to the tail of elements of the unwrapped_vector_tuple3_int32_int32_int32.
  subroutine push_back_array_unwrapped_vector_tuple3_int32_int32_int32(this, arr)
    class(unwrapped_vector_tuple3_int32_int32_int32), intent(inout) :: this
    type(tuple3_int32_int32_int32), intent(in) :: arr(:)
    integer(int32) :: s
    s = size(arr)
    if (.not. allocated(this%arr_)) call this%init(s)
    if (this%size_ + s > this%capa_) then
       call this%resize(this%size_ + s)
    end if
    this%arr_(this%size_+1:this%size_+s) = arr(:)
    this%size_ = this%size_ + s
  end subroutine push_back_array_unwrapped_vector_tuple3_int32_int32_int32
  !> pop_back_unwrapped_vector_tuple3_int32_int32_int32: Delete the value in the end of arr_(:) of the unwrapped_vector_tuple3_int32_int32_int32 and return it.
  type(tuple3_int32_int32_int32) function pop_back_unwrapped_vector_tuple3_int32_int32_int32(this)
    class(unwrapped_vector_tuple3_int32_int32_int32), intent(inout) :: this
    pop_back_unwrapped_vector_tuple3_int32_int32_int32 = this%arr_(this%size_)
    this%size_ = this%size_ - 1
  end function pop_back_unwrapped_vector_tuple3_int32_int32_int32
  !> back_unwrapped_vector_tuple3_int32_int32_int32: Delete the value in the end of arr_(:) of the unwrapped_vector_tuple3_int32_int32_int32 and return it.
  type(tuple3_int32_int32_int32) function back_unwrapped_vector_tuple3_int32_int32_int32(this)
    class(unwrapped_vector_tuple3_int32_int32_int32), intent(inout) :: this
    back_unwrapped_vector_tuple3_int32_int32_int32 = this%arr_(this%size_)
  end function back_unwrapped_vector_tuple3_int32_int32_int32
  !> size_vector_tuple3_int32_int32_int32: Return current size of the unwrapped_vector_tuple3_int32_int32_int32.
  pure integer(int32) function size_unwrapped_vector_tuple3_int32_int32_int32(this)
    class(unwrapped_vector_tuple3_int32_int32_int32), intent(in) :: this
    size_unwrapped_vector_tuple3_int32_int32_int32 = this%size_
  end function size_unwrapped_vector_tuple3_int32_int32_int32
  !> resize_unwrapped_vector_tuple3_int32_int32_int32: Shrink or expand arr_(:) of the unwrapped_vector_tuple3_int32_int32_int32.
  subroutine resize_unwrapped_vector_tuple3_int32_int32_int32(this, resize)
    class(unwrapped_vector_tuple3_int32_int32_int32), intent(inout) :: this
    integer(int32), intent(in) :: resize
    type(tuple3_int32_int32_int32), allocatable :: tmp(:)
    if (resize < 1) then
       this%size_ = 0
       allocate(tmp(1))
       call move_alloc(from = tmp, to = this%arr_)
       this%capa_ = 1
    else
       if (this%capa_ == resize) return
       allocate(tmp(resize))
       this%size_ = min(this%size_, resize)
       tmp(1:this%size_) = this%arr_(1:this%size_)
       call move_alloc(from = tmp, to = this%arr_)
       this%capa_ = resize
    end if
  end subroutine resize_unwrapped_vector_tuple3_int32_int32_int32
  !> lower_bound_vector_tuple3_int32_int32_int32: Return the minimum index that is higher than or equal to .
  integer(int32) function lower_bound_unwrapped_vector_tuple3_int32_int32_int32(this, val)
    class(unwrapped_vector_tuple3_int32_int32_int32), intent(in) :: this
    type(tuple3_int32_int32_int32), intent(in) :: val
    integer(int32) :: p, q, r
    p = 1
    r = this%size_
    if (this%arr_(r) < val) then
       lower_bound_unwrapped_vector_tuple3_int32_int32_int32 = r + 1
       return
    end if
    do
       q = (p+r)/2
       if (p + 1 > r) exit
       if (this%arr_(q) >= val) then
          r = q
       else
          p = q+1
       end if
    end do
    lower_bound_unwrapped_vector_tuple3_int32_int32_int32 = q
  end function lower_bound_unwrapped_vector_tuple3_int32_int32_int32
  
  !> construct_unwrapped_vector_tuple3_int64_int64_int64_by_size: Construct unwrapped_vector_tuple3_int64_int64_int64 by the size, the initial values is unknown.
  impure function construct_unwrapped_vector_tuple3_int64_int64_int64_by_size(size) result(res)
    type(unwrapped_vector_tuple3_int64_int64_int64) :: res
    integer(int32), intent(in) :: size
    call res%init(size)
  end function construct_unwrapped_vector_tuple3_int64_int64_int64_by_size
  !> construct_unwrapped_vector_tuple3_int64_int64_int64_by_arr: Construct unwrapped_vector_tuple3_int64_int64_int64 by the array of type(tuple3_int64_int64_int64).
  impure function construct_unwrapped_vector_tuple3_int64_int64_int64_by_arr(arr) result(res)
    type(unwrapped_vector_tuple3_int64_int64_int64) :: res
    type(tuple3_int64_int64_int64), intent(in) :: arr(:)
    integer(int32) :: n
    n = size(arr)
    call res%init(n)
    res%arr_(1:n) = arr(1:n)
  end function construct_unwrapped_vector_tuple3_int64_int64_int64_by_arr
  !> construct_unwrapped_vector_tuple3_int64_int64_int64_by_init_val: Construct unwrapped_vector_tuple3_int64_int64_int64 by size and the initial values.
  impure function construct_unwrapped_vector_tuple3_int64_int64_int64_by_init_val(size, val) result(res)
    type(unwrapped_vector_tuple3_int64_int64_int64) :: res
    integer(int32), intent(in) :: size
    type(tuple3_int64_int64_int64), intent(in) :: val
    call res%init(size)
    res%arr_(1:size) = val
  end function construct_unwrapped_vector_tuple3_int64_int64_int64_by_init_val
  !> init_unwrapped_vector_tuple3_int64_int64_int64: Initialize the unwrapped_vector_tuple3_int64_int64_int64 by size.
  subroutine init_unwrapped_vector_tuple3_int64_int64_int64(this, n)
    class(unwrapped_vector_tuple3_int64_int64_int64), intent(inout) :: this
    integer(int32), intent(in) :: n
    if (.not. allocated(this%arr_)) then
       allocate(this%arr_(n))
       this%size_ = n
       this%capa_ = n
    end if
  end subroutine init_unwrapped_vector_tuple3_int64_int64_int64
  !> push_back_unwrapped_vector_tuple3_int64_int64_int64: Insert value to the tail of elements of the unwrapped_vector_tuple3_int64_int64_int64.
  subroutine push_back_unwrapped_vector_tuple3_int64_int64_int64(this, val)
    class(unwrapped_vector_tuple3_int64_int64_int64), intent(inout) :: this
    type(tuple3_int64_int64_int64), intent(in) :: val
    if (.not. allocated(this%arr_)) call this%resize(0)
    if (this%size_ == this%capa_) then
       call this%resize(2*this%capa_)
    end if
    this%size_ = this%size_ + 1
    this%arr_(this%size_) = val
  end subroutine push_back_unwrapped_vector_tuple3_int64_int64_int64
  !> push_back_array_unwrapped_vector_tuple3_int64_int64_int64: Insert elemeents of array to the tail of elements of the unwrapped_vector_tuple3_int64_int64_int64.
  subroutine push_back_array_unwrapped_vector_tuple3_int64_int64_int64(this, arr)
    class(unwrapped_vector_tuple3_int64_int64_int64), intent(inout) :: this
    type(tuple3_int64_int64_int64), intent(in) :: arr(:)
    integer(int32) :: s
    s = size(arr)
    if (.not. allocated(this%arr_)) call this%init(s)
    if (this%size_ + s > this%capa_) then
       call this%resize(this%size_ + s)
    end if
    this%arr_(this%size_+1:this%size_+s) = arr(:)
    this%size_ = this%size_ + s
  end subroutine push_back_array_unwrapped_vector_tuple3_int64_int64_int64
  !> pop_back_unwrapped_vector_tuple3_int64_int64_int64: Delete the value in the end of arr_(:) of the unwrapped_vector_tuple3_int64_int64_int64 and return it.
  type(tuple3_int64_int64_int64) function pop_back_unwrapped_vector_tuple3_int64_int64_int64(this)
    class(unwrapped_vector_tuple3_int64_int64_int64), intent(inout) :: this
    pop_back_unwrapped_vector_tuple3_int64_int64_int64 = this%arr_(this%size_)
    this%size_ = this%size_ - 1
  end function pop_back_unwrapped_vector_tuple3_int64_int64_int64
  !> back_unwrapped_vector_tuple3_int64_int64_int64: Delete the value in the end of arr_(:) of the unwrapped_vector_tuple3_int64_int64_int64 and return it.
  type(tuple3_int64_int64_int64) function back_unwrapped_vector_tuple3_int64_int64_int64(this)
    class(unwrapped_vector_tuple3_int64_int64_int64), intent(inout) :: this
    back_unwrapped_vector_tuple3_int64_int64_int64 = this%arr_(this%size_)
  end function back_unwrapped_vector_tuple3_int64_int64_int64
  !> size_vector_tuple3_int64_int64_int64: Return current size of the unwrapped_vector_tuple3_int64_int64_int64.
  pure integer(int32) function size_unwrapped_vector_tuple3_int64_int64_int64(this)
    class(unwrapped_vector_tuple3_int64_int64_int64), intent(in) :: this
    size_unwrapped_vector_tuple3_int64_int64_int64 = this%size_
  end function size_unwrapped_vector_tuple3_int64_int64_int64
  !> resize_unwrapped_vector_tuple3_int64_int64_int64: Shrink or expand arr_(:) of the unwrapped_vector_tuple3_int64_int64_int64.
  subroutine resize_unwrapped_vector_tuple3_int64_int64_int64(this, resize)
    class(unwrapped_vector_tuple3_int64_int64_int64), intent(inout) :: this
    integer(int32), intent(in) :: resize
    type(tuple3_int64_int64_int64), allocatable :: tmp(:)
    if (resize < 1) then
       this%size_ = 0
       allocate(tmp(1))
       call move_alloc(from = tmp, to = this%arr_)
       this%capa_ = 1
    else
       if (this%capa_ == resize) return
       allocate(tmp(resize))
       this%size_ = min(this%size_, resize)
       tmp(1:this%size_) = this%arr_(1:this%size_)
       call move_alloc(from = tmp, to = this%arr_)
       this%capa_ = resize
    end if
  end subroutine resize_unwrapped_vector_tuple3_int64_int64_int64
  !> lower_bound_vector_tuple3_int64_int64_int64: Return the minimum index that is higher than or equal to .
  integer(int32) function lower_bound_unwrapped_vector_tuple3_int64_int64_int64(this, val)
    class(unwrapped_vector_tuple3_int64_int64_int64), intent(in) :: this
    type(tuple3_int64_int64_int64), intent(in) :: val
    integer(int32) :: p, q, r
    p = 1
    r = this%size_
    if (this%arr_(r) < val) then
       lower_bound_unwrapped_vector_tuple3_int64_int64_int64 = r + 1
       return
    end if
    do
       q = (p+r)/2
       if (p + 1 > r) exit
       if (this%arr_(q) >= val) then
          r = q
       else
          p = q+1
       end if
    end do
    lower_bound_unwrapped_vector_tuple3_int64_int64_int64 = q
  end function lower_bound_unwrapped_vector_tuple3_int64_int64_int64
  
end module tuple3_unwrapped_vector_m
