module unwrapped_vector_m
  use, intrinsic :: iso_fortran_env
  implicit none
  private
  public :: unwrapped_vector_int32
  type :: unwrapped_vector_int32
     private
     integer(int32), allocatable, public :: arr_(:)
     integer(int32) :: size_ = 0, capa_ = 0
   contains
     procedure, pass :: init      => init_unwrapped_vector_int32
     procedure, pass :: push_back_unwrapped_vector_int32, &
          push_back_array_unwrapped_vector_int32
     generic         :: push_back => push_back_unwrapped_vector_int32, &
          push_back_array_unwrapped_vector_int32
     procedure, pass :: pop_back  => pop_back_unwrapped_vector_int32
     procedure, pass :: back      => back_unwrapped_vector_int32
     procedure, pass :: size      => size_unwrapped_vector_int32
     procedure, pass :: resize    => resize_unwrapped_vector_int32
     procedure, pass :: lower_bound => lower_bound_unwrapped_vector_int32
  end type unwrapped_vector_int32
  interface unwrapped_vector_int32
     module procedure :: construct_unwrapped_vector_int32_by_size, &
          construct_unwrapped_vector_int32_by_arr, &
          construct_unwrapped_vector_int32_by_init_val
  end interface unwrapped_vector_int32
  
  public :: unwrapped_vector_int64
  type :: unwrapped_vector_int64
     private
     integer(int64), allocatable, public :: arr_(:)
     integer(int32) :: size_ = 0, capa_ = 0
   contains
     procedure, pass :: init      => init_unwrapped_vector_int64
     procedure, pass :: push_back_unwrapped_vector_int64, &
          push_back_array_unwrapped_vector_int64
     generic         :: push_back => push_back_unwrapped_vector_int64, &
          push_back_array_unwrapped_vector_int64
     procedure, pass :: pop_back  => pop_back_unwrapped_vector_int64
     procedure, pass :: back      => back_unwrapped_vector_int64
     procedure, pass :: size      => size_unwrapped_vector_int64
     procedure, pass :: resize    => resize_unwrapped_vector_int64
     procedure, pass :: lower_bound => lower_bound_unwrapped_vector_int64
  end type unwrapped_vector_int64
  interface unwrapped_vector_int64
     module procedure :: construct_unwrapped_vector_int64_by_size, &
          construct_unwrapped_vector_int64_by_arr, &
          construct_unwrapped_vector_int64_by_init_val
  end interface unwrapped_vector_int64
  
  public :: unwrapped_vector_real32
  type :: unwrapped_vector_real32
     private
     real(real32), allocatable, public :: arr_(:)
     integer(int32) :: size_ = 0, capa_ = 0
   contains
     procedure, pass :: init      => init_unwrapped_vector_real32
     procedure, pass :: push_back_unwrapped_vector_real32, &
          push_back_array_unwrapped_vector_real32
     generic         :: push_back => push_back_unwrapped_vector_real32, &
          push_back_array_unwrapped_vector_real32
     procedure, pass :: pop_back  => pop_back_unwrapped_vector_real32
     procedure, pass :: back      => back_unwrapped_vector_real32
     procedure, pass :: size      => size_unwrapped_vector_real32
     procedure, pass :: resize    => resize_unwrapped_vector_real32
     procedure, pass :: lower_bound => lower_bound_unwrapped_vector_real32
  end type unwrapped_vector_real32
  interface unwrapped_vector_real32
     module procedure :: construct_unwrapped_vector_real32_by_size, &
          construct_unwrapped_vector_real32_by_arr, &
          construct_unwrapped_vector_real32_by_init_val
  end interface unwrapped_vector_real32
  
  public :: unwrapped_vector_real64
  type :: unwrapped_vector_real64
     private
     real(real64), allocatable, public :: arr_(:)
     integer(int32) :: size_ = 0, capa_ = 0
   contains
     procedure, pass :: init      => init_unwrapped_vector_real64
     procedure, pass :: push_back_unwrapped_vector_real64, &
          push_back_array_unwrapped_vector_real64
     generic         :: push_back => push_back_unwrapped_vector_real64, &
          push_back_array_unwrapped_vector_real64
     procedure, pass :: pop_back  => pop_back_unwrapped_vector_real64
     procedure, pass :: back      => back_unwrapped_vector_real64
     procedure, pass :: size      => size_unwrapped_vector_real64
     procedure, pass :: resize    => resize_unwrapped_vector_real64
     procedure, pass :: lower_bound => lower_bound_unwrapped_vector_real64
  end type unwrapped_vector_real64
  interface unwrapped_vector_real64
     module procedure :: construct_unwrapped_vector_real64_by_size, &
          construct_unwrapped_vector_real64_by_arr, &
          construct_unwrapped_vector_real64_by_init_val
  end interface unwrapped_vector_real64
  
  public :: unwrapped_vector_character
  type :: unwrapped_vector_character
     private
     character, allocatable, public :: arr_(:)
     integer(int32) :: size_ = 0, capa_ = 0
   contains
     procedure, pass :: init      => init_unwrapped_vector_character
     procedure, pass :: push_back_unwrapped_vector_character, &
          push_back_array_unwrapped_vector_character
     generic         :: push_back => push_back_unwrapped_vector_character, &
          push_back_array_unwrapped_vector_character
     procedure, pass :: pop_back  => pop_back_unwrapped_vector_character
     procedure, pass :: back      => back_unwrapped_vector_character
     procedure, pass :: size      => size_unwrapped_vector_character
     procedure, pass :: resize    => resize_unwrapped_vector_character
     procedure, pass :: lower_bound => lower_bound_unwrapped_vector_character
  end type unwrapped_vector_character
  interface unwrapped_vector_character
     module procedure :: construct_unwrapped_vector_character_by_size, &
          construct_unwrapped_vector_character_by_arr, &
          construct_unwrapped_vector_character_by_init_val
  end interface unwrapped_vector_character
  
contains
  !> construct_unwrapped_vector_int32_by_size: Construct unwrapped_vector_int32 by the size, the initial values is unknown.
  impure function construct_unwrapped_vector_int32_by_size(size) result(res)
    type(unwrapped_vector_int32) :: res
    integer(int32), intent(in) :: size
    call res%init(size)
  end function construct_unwrapped_vector_int32_by_size
  !> construct_unwrapped_vector_int32_by_arr: Construct unwrapped_vector_int32 by the array of integer(int32).
  impure function construct_unwrapped_vector_int32_by_arr(arr) result(res)
    type(unwrapped_vector_int32) :: res
    integer(int32), intent(in) :: arr(:)
    integer(int32) :: n
    n = size(arr)
    call res%init(n)
    res%arr_(1:n) = arr(1:n)
  end function construct_unwrapped_vector_int32_by_arr
  !> construct_unwrapped_vector_int32_by_init_val: Construct unwrapped_vector_int32 by size and the initial values.
  impure function construct_unwrapped_vector_int32_by_init_val(size, val) result(res)
    type(unwrapped_vector_int32) :: res
    integer(int32), intent(in) :: size
    integer(int32), intent(in) :: val
    call res%init(size)
    res%arr_(1:size) = val
  end function construct_unwrapped_vector_int32_by_init_val
  !> init_unwrapped_vector_int32: Initialize the unwrapped_vector_int32 by size.
  subroutine init_unwrapped_vector_int32(this, n)
    class(unwrapped_vector_int32), intent(inout) :: this
    integer(int32), intent(in) :: n
    if (.not. allocated(this%arr_)) then
       allocate(this%arr_(n))
       this%size_ = n
       this%capa_ = n
    end if
  end subroutine init_unwrapped_vector_int32
  !> push_back_unwrapped_vector_int32: Insert value to the tail of elements of the unwrapped_vector_int32.
  subroutine push_back_unwrapped_vector_int32(this, val)
    class(unwrapped_vector_int32), intent(inout) :: this
    integer(int32), intent(in) :: val
    if (.not. allocated(this%arr_)) call this%resize(0)
    if (this%size_ == this%capa_) then
       call this%resize(2*this%capa_)
    end if
    this%size_ = this%size_ + 1
    this%arr_(this%size_) = val
  end subroutine push_back_unwrapped_vector_int32
  !> push_back_array_unwrapped_vector_int32: Insert elemeents of array to the tail of elements of the unwrapped_vector_int32.
  subroutine push_back_array_unwrapped_vector_int32(this, arr)
    class(unwrapped_vector_int32), intent(inout) :: this
    integer(int32), intent(in) :: arr(:)
    integer(int32) :: s
    s = size(arr)
    if (.not. allocated(this%arr_)) call this%init(s)
    if (this%size_ + s > this%capa_) then
       call this%resize(this%size_ + s)
    end if
    this%arr_(this%size_+1:this%size_+s) = arr(:)
    this%size_ = this%size_ + s
  end subroutine push_back_array_unwrapped_vector_int32
  !> pop_back_unwrapped_vector_int32: Delete the value in the end of arr_(:) of the unwrapped_vector_int32 and return it.
  integer(int32) function pop_back_unwrapped_vector_int32(this)
    class(unwrapped_vector_int32), intent(inout) :: this
    pop_back_unwrapped_vector_int32 = this%arr_(this%size_)
    this%size_ = this%size_ - 1
  end function pop_back_unwrapped_vector_int32
  !> back_unwrapped_vector_int32: Delete the value in the end of arr_(:) of the unwrapped_vector_int32 and return it.
  integer(int32) function back_unwrapped_vector_int32(this)
    class(unwrapped_vector_int32), intent(inout) :: this
    back_unwrapped_vector_int32 = this%arr_(this%size_)
  end function back_unwrapped_vector_int32
  !> size_vector_int32: Return current size of the unwrapped_vector_int32.
  pure integer(int32) function size_unwrapped_vector_int32(this)
    class(unwrapped_vector_int32), intent(in) :: this
    size_unwrapped_vector_int32 = this%size_
  end function size_unwrapped_vector_int32
  !> resize_unwrapped_vector_int32: Shrink or expand arr_(:) of the unwrapped_vector_int32.
  subroutine resize_unwrapped_vector_int32(this, resize)
    class(unwrapped_vector_int32), intent(inout) :: this
    integer(int32), intent(in) :: resize
    integer(int32), allocatable :: tmp(:)
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
  end subroutine resize_unwrapped_vector_int32
  !> lower_bound_vector_int32: Return the minimum index that is higher than or equal to .
  integer(int32) function lower_bound_unwrapped_vector_int32(this, val)
    class(unwrapped_vector_int32), intent(in) :: this
    integer(int32), intent(in) :: val
    integer(int32) :: p, q, r
    p = 1
    r = this%size_
    if (this%arr_(r) < val) then
       lower_bound_unwrapped_vector_int32 = r + 1
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
    lower_bound_unwrapped_vector_int32 = q
  end function lower_bound_unwrapped_vector_int32
  
  !> construct_unwrapped_vector_int64_by_size: Construct unwrapped_vector_int64 by the size, the initial values is unknown.
  impure function construct_unwrapped_vector_int64_by_size(size) result(res)
    type(unwrapped_vector_int64) :: res
    integer(int32), intent(in) :: size
    call res%init(size)
  end function construct_unwrapped_vector_int64_by_size
  !> construct_unwrapped_vector_int64_by_arr: Construct unwrapped_vector_int64 by the array of integer(int64).
  impure function construct_unwrapped_vector_int64_by_arr(arr) result(res)
    type(unwrapped_vector_int64) :: res
    integer(int64), intent(in) :: arr(:)
    integer(int32) :: n
    n = size(arr)
    call res%init(n)
    res%arr_(1:n) = arr(1:n)
  end function construct_unwrapped_vector_int64_by_arr
  !> construct_unwrapped_vector_int64_by_init_val: Construct unwrapped_vector_int64 by size and the initial values.
  impure function construct_unwrapped_vector_int64_by_init_val(size, val) result(res)
    type(unwrapped_vector_int64) :: res
    integer(int32), intent(in) :: size
    integer(int64), intent(in) :: val
    call res%init(size)
    res%arr_(1:size) = val
  end function construct_unwrapped_vector_int64_by_init_val
  !> init_unwrapped_vector_int64: Initialize the unwrapped_vector_int64 by size.
  subroutine init_unwrapped_vector_int64(this, n)
    class(unwrapped_vector_int64), intent(inout) :: this
    integer(int32), intent(in) :: n
    if (.not. allocated(this%arr_)) then
       allocate(this%arr_(n))
       this%size_ = n
       this%capa_ = n
    end if
  end subroutine init_unwrapped_vector_int64
  !> push_back_unwrapped_vector_int64: Insert value to the tail of elements of the unwrapped_vector_int64.
  subroutine push_back_unwrapped_vector_int64(this, val)
    class(unwrapped_vector_int64), intent(inout) :: this
    integer(int64), intent(in) :: val
    if (.not. allocated(this%arr_)) call this%resize(0)
    if (this%size_ == this%capa_) then
       call this%resize(2*this%capa_)
    end if
    this%size_ = this%size_ + 1
    this%arr_(this%size_) = val
  end subroutine push_back_unwrapped_vector_int64
  !> push_back_array_unwrapped_vector_int64: Insert elemeents of array to the tail of elements of the unwrapped_vector_int64.
  subroutine push_back_array_unwrapped_vector_int64(this, arr)
    class(unwrapped_vector_int64), intent(inout) :: this
    integer(int64), intent(in) :: arr(:)
    integer(int32) :: s
    s = size(arr)
    if (.not. allocated(this%arr_)) call this%init(s)
    if (this%size_ + s > this%capa_) then
       call this%resize(this%size_ + s)
    end if
    this%arr_(this%size_+1:this%size_+s) = arr(:)
    this%size_ = this%size_ + s
  end subroutine push_back_array_unwrapped_vector_int64
  !> pop_back_unwrapped_vector_int64: Delete the value in the end of arr_(:) of the unwrapped_vector_int64 and return it.
  integer(int64) function pop_back_unwrapped_vector_int64(this)
    class(unwrapped_vector_int64), intent(inout) :: this
    pop_back_unwrapped_vector_int64 = this%arr_(this%size_)
    this%size_ = this%size_ - 1
  end function pop_back_unwrapped_vector_int64
  !> back_unwrapped_vector_int64: Delete the value in the end of arr_(:) of the unwrapped_vector_int64 and return it.
  integer(int64) function back_unwrapped_vector_int64(this)
    class(unwrapped_vector_int64), intent(inout) :: this
    back_unwrapped_vector_int64 = this%arr_(this%size_)
  end function back_unwrapped_vector_int64
  !> size_vector_int64: Return current size of the unwrapped_vector_int64.
  pure integer(int32) function size_unwrapped_vector_int64(this)
    class(unwrapped_vector_int64), intent(in) :: this
    size_unwrapped_vector_int64 = this%size_
  end function size_unwrapped_vector_int64
  !> resize_unwrapped_vector_int64: Shrink or expand arr_(:) of the unwrapped_vector_int64.
  subroutine resize_unwrapped_vector_int64(this, resize)
    class(unwrapped_vector_int64), intent(inout) :: this
    integer(int32), intent(in) :: resize
    integer(int64), allocatable :: tmp(:)
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
  end subroutine resize_unwrapped_vector_int64
  !> lower_bound_vector_int64: Return the minimum index that is higher than or equal to .
  integer(int32) function lower_bound_unwrapped_vector_int64(this, val)
    class(unwrapped_vector_int64), intent(in) :: this
    integer(int64), intent(in) :: val
    integer(int32) :: p, q, r
    p = 1
    r = this%size_
    if (this%arr_(r) < val) then
       lower_bound_unwrapped_vector_int64 = r + 1
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
    lower_bound_unwrapped_vector_int64 = q
  end function lower_bound_unwrapped_vector_int64
  
  !> construct_unwrapped_vector_real32_by_size: Construct unwrapped_vector_real32 by the size, the initial values is unknown.
  impure function construct_unwrapped_vector_real32_by_size(size) result(res)
    type(unwrapped_vector_real32) :: res
    integer(int32), intent(in) :: size
    call res%init(size)
  end function construct_unwrapped_vector_real32_by_size
  !> construct_unwrapped_vector_real32_by_arr: Construct unwrapped_vector_real32 by the array of real(real32).
  impure function construct_unwrapped_vector_real32_by_arr(arr) result(res)
    type(unwrapped_vector_real32) :: res
    real(real32), intent(in) :: arr(:)
    integer(int32) :: n
    n = size(arr)
    call res%init(n)
    res%arr_(1:n) = arr(1:n)
  end function construct_unwrapped_vector_real32_by_arr
  !> construct_unwrapped_vector_real32_by_init_val: Construct unwrapped_vector_real32 by size and the initial values.
  impure function construct_unwrapped_vector_real32_by_init_val(size, val) result(res)
    type(unwrapped_vector_real32) :: res
    integer(int32), intent(in) :: size
    real(real32), intent(in) :: val
    call res%init(size)
    res%arr_(1:size) = val
  end function construct_unwrapped_vector_real32_by_init_val
  !> init_unwrapped_vector_real32: Initialize the unwrapped_vector_real32 by size.
  subroutine init_unwrapped_vector_real32(this, n)
    class(unwrapped_vector_real32), intent(inout) :: this
    integer(int32), intent(in) :: n
    if (.not. allocated(this%arr_)) then
       allocate(this%arr_(n))
       this%size_ = n
       this%capa_ = n
    end if
  end subroutine init_unwrapped_vector_real32
  !> push_back_unwrapped_vector_real32: Insert value to the tail of elements of the unwrapped_vector_real32.
  subroutine push_back_unwrapped_vector_real32(this, val)
    class(unwrapped_vector_real32), intent(inout) :: this
    real(real32), intent(in) :: val
    if (.not. allocated(this%arr_)) call this%resize(0)
    if (this%size_ == this%capa_) then
       call this%resize(2*this%capa_)
    end if
    this%size_ = this%size_ + 1
    this%arr_(this%size_) = val
  end subroutine push_back_unwrapped_vector_real32
  !> push_back_array_unwrapped_vector_real32: Insert elemeents of array to the tail of elements of the unwrapped_vector_real32.
  subroutine push_back_array_unwrapped_vector_real32(this, arr)
    class(unwrapped_vector_real32), intent(inout) :: this
    real(real32), intent(in) :: arr(:)
    integer(int32) :: s
    s = size(arr)
    if (.not. allocated(this%arr_)) call this%init(s)
    if (this%size_ + s > this%capa_) then
       call this%resize(this%size_ + s)
    end if
    this%arr_(this%size_+1:this%size_+s) = arr(:)
    this%size_ = this%size_ + s
  end subroutine push_back_array_unwrapped_vector_real32
  !> pop_back_unwrapped_vector_real32: Delete the value in the end of arr_(:) of the unwrapped_vector_real32 and return it.
  real(real32) function pop_back_unwrapped_vector_real32(this)
    class(unwrapped_vector_real32), intent(inout) :: this
    pop_back_unwrapped_vector_real32 = this%arr_(this%size_)
    this%size_ = this%size_ - 1
  end function pop_back_unwrapped_vector_real32
  !> back_unwrapped_vector_real32: Delete the value in the end of arr_(:) of the unwrapped_vector_real32 and return it.
  real(real32) function back_unwrapped_vector_real32(this)
    class(unwrapped_vector_real32), intent(inout) :: this
    back_unwrapped_vector_real32 = this%arr_(this%size_)
  end function back_unwrapped_vector_real32
  !> size_vector_real32: Return current size of the unwrapped_vector_real32.
  pure integer(int32) function size_unwrapped_vector_real32(this)
    class(unwrapped_vector_real32), intent(in) :: this
    size_unwrapped_vector_real32 = this%size_
  end function size_unwrapped_vector_real32
  !> resize_unwrapped_vector_real32: Shrink or expand arr_(:) of the unwrapped_vector_real32.
  subroutine resize_unwrapped_vector_real32(this, resize)
    class(unwrapped_vector_real32), intent(inout) :: this
    integer(int32), intent(in) :: resize
    real(real32), allocatable :: tmp(:)
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
  end subroutine resize_unwrapped_vector_real32
  !> lower_bound_vector_real32: Return the minimum index that is higher than or equal to .
  integer(int32) function lower_bound_unwrapped_vector_real32(this, val)
    class(unwrapped_vector_real32), intent(in) :: this
    real(real32), intent(in) :: val
    integer(int32) :: p, q, r
    p = 1
    r = this%size_
    if (this%arr_(r) < val) then
       lower_bound_unwrapped_vector_real32 = r + 1
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
    lower_bound_unwrapped_vector_real32 = q
  end function lower_bound_unwrapped_vector_real32
  
  !> construct_unwrapped_vector_real64_by_size: Construct unwrapped_vector_real64 by the size, the initial values is unknown.
  impure function construct_unwrapped_vector_real64_by_size(size) result(res)
    type(unwrapped_vector_real64) :: res
    integer(int32), intent(in) :: size
    call res%init(size)
  end function construct_unwrapped_vector_real64_by_size
  !> construct_unwrapped_vector_real64_by_arr: Construct unwrapped_vector_real64 by the array of real(real64).
  impure function construct_unwrapped_vector_real64_by_arr(arr) result(res)
    type(unwrapped_vector_real64) :: res
    real(real64), intent(in) :: arr(:)
    integer(int32) :: n
    n = size(arr)
    call res%init(n)
    res%arr_(1:n) = arr(1:n)
  end function construct_unwrapped_vector_real64_by_arr
  !> construct_unwrapped_vector_real64_by_init_val: Construct unwrapped_vector_real64 by size and the initial values.
  impure function construct_unwrapped_vector_real64_by_init_val(size, val) result(res)
    type(unwrapped_vector_real64) :: res
    integer(int32), intent(in) :: size
    real(real64), intent(in) :: val
    call res%init(size)
    res%arr_(1:size) = val
  end function construct_unwrapped_vector_real64_by_init_val
  !> init_unwrapped_vector_real64: Initialize the unwrapped_vector_real64 by size.
  subroutine init_unwrapped_vector_real64(this, n)
    class(unwrapped_vector_real64), intent(inout) :: this
    integer(int32), intent(in) :: n
    if (.not. allocated(this%arr_)) then
       allocate(this%arr_(n))
       this%size_ = n
       this%capa_ = n
    end if
  end subroutine init_unwrapped_vector_real64
  !> push_back_unwrapped_vector_real64: Insert value to the tail of elements of the unwrapped_vector_real64.
  subroutine push_back_unwrapped_vector_real64(this, val)
    class(unwrapped_vector_real64), intent(inout) :: this
    real(real64), intent(in) :: val
    if (.not. allocated(this%arr_)) call this%resize(0)
    if (this%size_ == this%capa_) then
       call this%resize(2*this%capa_)
    end if
    this%size_ = this%size_ + 1
    this%arr_(this%size_) = val
  end subroutine push_back_unwrapped_vector_real64
  !> push_back_array_unwrapped_vector_real64: Insert elemeents of array to the tail of elements of the unwrapped_vector_real64.
  subroutine push_back_array_unwrapped_vector_real64(this, arr)
    class(unwrapped_vector_real64), intent(inout) :: this
    real(real64), intent(in) :: arr(:)
    integer(int32) :: s
    s = size(arr)
    if (.not. allocated(this%arr_)) call this%init(s)
    if (this%size_ + s > this%capa_) then
       call this%resize(this%size_ + s)
    end if
    this%arr_(this%size_+1:this%size_+s) = arr(:)
    this%size_ = this%size_ + s
  end subroutine push_back_array_unwrapped_vector_real64
  !> pop_back_unwrapped_vector_real64: Delete the value in the end of arr_(:) of the unwrapped_vector_real64 and return it.
  real(real64) function pop_back_unwrapped_vector_real64(this)
    class(unwrapped_vector_real64), intent(inout) :: this
    pop_back_unwrapped_vector_real64 = this%arr_(this%size_)
    this%size_ = this%size_ - 1
  end function pop_back_unwrapped_vector_real64
  !> back_unwrapped_vector_real64: Delete the value in the end of arr_(:) of the unwrapped_vector_real64 and return it.
  real(real64) function back_unwrapped_vector_real64(this)
    class(unwrapped_vector_real64), intent(inout) :: this
    back_unwrapped_vector_real64 = this%arr_(this%size_)
  end function back_unwrapped_vector_real64
  !> size_vector_real64: Return current size of the unwrapped_vector_real64.
  pure integer(int32) function size_unwrapped_vector_real64(this)
    class(unwrapped_vector_real64), intent(in) :: this
    size_unwrapped_vector_real64 = this%size_
  end function size_unwrapped_vector_real64
  !> resize_unwrapped_vector_real64: Shrink or expand arr_(:) of the unwrapped_vector_real64.
  subroutine resize_unwrapped_vector_real64(this, resize)
    class(unwrapped_vector_real64), intent(inout) :: this
    integer(int32), intent(in) :: resize
    real(real64), allocatable :: tmp(:)
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
  end subroutine resize_unwrapped_vector_real64
  !> lower_bound_vector_real64: Return the minimum index that is higher than or equal to .
  integer(int32) function lower_bound_unwrapped_vector_real64(this, val)
    class(unwrapped_vector_real64), intent(in) :: this
    real(real64), intent(in) :: val
    integer(int32) :: p, q, r
    p = 1
    r = this%size_
    if (this%arr_(r) < val) then
       lower_bound_unwrapped_vector_real64 = r + 1
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
    lower_bound_unwrapped_vector_real64 = q
  end function lower_bound_unwrapped_vector_real64
  
  !> construct_unwrapped_vector_character_by_size: Construct unwrapped_vector_character by the size, the initial values is unknown.
  impure function construct_unwrapped_vector_character_by_size(size) result(res)
    type(unwrapped_vector_character) :: res
    integer(int32), intent(in) :: size
    call res%init(size)
  end function construct_unwrapped_vector_character_by_size
  !> construct_unwrapped_vector_character_by_arr: Construct unwrapped_vector_character by the array of character.
  impure function construct_unwrapped_vector_character_by_arr(arr) result(res)
    type(unwrapped_vector_character) :: res
    character, intent(in) :: arr(:)
    integer(int32) :: n
    n = size(arr)
    call res%init(n)
    res%arr_(1:n) = arr(1:n)
  end function construct_unwrapped_vector_character_by_arr
  !> construct_unwrapped_vector_character_by_init_val: Construct unwrapped_vector_character by size and the initial values.
  impure function construct_unwrapped_vector_character_by_init_val(size, val) result(res)
    type(unwrapped_vector_character) :: res
    integer(int32), intent(in) :: size
    character, intent(in) :: val
    call res%init(size)
    res%arr_(1:size) = val
  end function construct_unwrapped_vector_character_by_init_val
  !> init_unwrapped_vector_character: Initialize the unwrapped_vector_character by size.
  subroutine init_unwrapped_vector_character(this, n)
    class(unwrapped_vector_character), intent(inout) :: this
    integer(int32), intent(in) :: n
    if (.not. allocated(this%arr_)) then
       allocate(this%arr_(n))
       this%size_ = n
       this%capa_ = n
    end if
  end subroutine init_unwrapped_vector_character
  !> push_back_unwrapped_vector_character: Insert value to the tail of elements of the unwrapped_vector_character.
  subroutine push_back_unwrapped_vector_character(this, val)
    class(unwrapped_vector_character), intent(inout) :: this
    character, intent(in) :: val
    if (.not. allocated(this%arr_)) call this%resize(0)
    if (this%size_ == this%capa_) then
       call this%resize(2*this%capa_)
    end if
    this%size_ = this%size_ + 1
    this%arr_(this%size_) = val
  end subroutine push_back_unwrapped_vector_character
  !> push_back_array_unwrapped_vector_character: Insert elemeents of array to the tail of elements of the unwrapped_vector_character.
  subroutine push_back_array_unwrapped_vector_character(this, arr)
    class(unwrapped_vector_character), intent(inout) :: this
    character, intent(in) :: arr(:)
    integer(int32) :: s
    s = size(arr)
    if (.not. allocated(this%arr_)) call this%init(s)
    if (this%size_ + s > this%capa_) then
       call this%resize(this%size_ + s)
    end if
    this%arr_(this%size_+1:this%size_+s) = arr(:)
    this%size_ = this%size_ + s
  end subroutine push_back_array_unwrapped_vector_character
  !> pop_back_unwrapped_vector_character: Delete the value in the end of arr_(:) of the unwrapped_vector_character and return it.
  character function pop_back_unwrapped_vector_character(this)
    class(unwrapped_vector_character), intent(inout) :: this
    pop_back_unwrapped_vector_character = this%arr_(this%size_)
    this%size_ = this%size_ - 1
  end function pop_back_unwrapped_vector_character
  !> back_unwrapped_vector_character: Delete the value in the end of arr_(:) of the unwrapped_vector_character and return it.
  character function back_unwrapped_vector_character(this)
    class(unwrapped_vector_character), intent(inout) :: this
    back_unwrapped_vector_character = this%arr_(this%size_)
  end function back_unwrapped_vector_character
  !> size_vector_character: Return current size of the unwrapped_vector_character.
  pure integer(int32) function size_unwrapped_vector_character(this)
    class(unwrapped_vector_character), intent(in) :: this
    size_unwrapped_vector_character = this%size_
  end function size_unwrapped_vector_character
  !> resize_unwrapped_vector_character: Shrink or expand arr_(:) of the unwrapped_vector_character.
  subroutine resize_unwrapped_vector_character(this, resize)
    class(unwrapped_vector_character), intent(inout) :: this
    integer(int32), intent(in) :: resize
    character, allocatable :: tmp(:)
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
  end subroutine resize_unwrapped_vector_character
  !> lower_bound_vector_character: Return the minimum index that is higher than or equal to .
  integer(int32) function lower_bound_unwrapped_vector_character(this, val)
    class(unwrapped_vector_character), intent(in) :: this
    character, intent(in) :: val
    integer(int32) :: p, q, r
    p = 1
    r = this%size_
    if (this%arr_(r) < val) then
       lower_bound_unwrapped_vector_character = r + 1
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
    lower_bound_unwrapped_vector_character = q
  end function lower_bound_unwrapped_vector_character
  
end module unwrapped_vector_m
