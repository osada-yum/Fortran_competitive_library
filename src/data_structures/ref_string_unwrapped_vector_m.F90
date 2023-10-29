module ref_string_unwrapped_vector_m
  use, intrinsic :: iso_fortran_env
  use ref_string_m
  implicit none
  public :: unwrapped_vector_ref_string
  type :: unwrapped_vector_ref_string
     private
     type(ref_string), allocatable, public :: arr_(:)
     integer(int32) :: size_ = 0, capa_ = 0
   contains
     procedure, pass :: init      => init_unwrapped_vector_ref_string
     procedure, pass :: push_back_unwrapped_vector_ref_string, &
          push_back_array_unwrapped_vector_ref_string
     generic         :: push_back => push_back_unwrapped_vector_ref_string, &
          push_back_array_unwrapped_vector_ref_string
     procedure, pass :: pop_back  => pop_back_unwrapped_vector_ref_string
     procedure, pass :: back      => back_unwrapped_vector_ref_string
     procedure, pass :: size      => size_unwrapped_vector_ref_string
     procedure, pass :: resize    => resize_unwrapped_vector_ref_string
     procedure, pass :: lower_bound => lower_bound_unwrapped_vector_ref_string
  end type unwrapped_vector_ref_string
  interface unwrapped_vector_ref_string
     module procedure :: construct_unwrapped_vector_ref_string_by_size, &
          construct_unwrapped_vector_ref_string_by_arr, &
          construct_unwrapped_vector_ref_string_by_init_val
  end interface unwrapped_vector_ref_string
  
contains
  !> construct_unwrapped_vector_ref_string_by_size: Construct unwrapped_vector_ref_string by the size, the initial values is unknown.
  impure function construct_unwrapped_vector_ref_string_by_size(size) result(res)
    type(unwrapped_vector_ref_string) :: res
    integer(int32), intent(in) :: size
    call res%init(size)
  end function construct_unwrapped_vector_ref_string_by_size
  !> construct_unwrapped_vector_ref_string_by_arr: Construct unwrapped_vector_ref_string by the array of type(ref_string).
  impure function construct_unwrapped_vector_ref_string_by_arr(arr) result(res)
    type(unwrapped_vector_ref_string) :: res
    type(ref_string), intent(in) :: arr(:)
    integer(int32) :: n
    n = size(arr)
    call res%init(n)
    res%arr_(1:n) = arr(1:n)
  end function construct_unwrapped_vector_ref_string_by_arr
  !> construct_unwrapped_vector_ref_string_by_init_val: Construct unwrapped_vector_ref_string by size and the initial values.
  impure function construct_unwrapped_vector_ref_string_by_init_val(size, val) result(res)
    type(unwrapped_vector_ref_string) :: res
    integer(int32), intent(in) :: size
    type(ref_string), intent(in) :: val
    call res%init(size)
    res%arr_(1:size) = val
  end function construct_unwrapped_vector_ref_string_by_init_val
  !> init_unwrapped_vector_ref_string: Initialize the unwrapped_vector_ref_string by size.
  subroutine init_unwrapped_vector_ref_string(this, n)
    class(unwrapped_vector_ref_string), intent(inout) :: this
    integer(int32), intent(in) :: n
    if (.not. allocated(this%arr_)) then
       allocate(this%arr_(n))
       this%size_ = n
       this%capa_ = n
    end if
  end subroutine init_unwrapped_vector_ref_string
  !> push_back_unwrapped_vector_ref_string: Insert value to the tail of elements of the unwrapped_vector_ref_string.
  subroutine push_back_unwrapped_vector_ref_string(this, val)
    class(unwrapped_vector_ref_string), intent(inout) :: this
    type(ref_string), intent(in) :: val
    if (.not. allocated(this%arr_)) call this%resize(0)
    if (this%size_ == this%capa_) then
       call this%resize(2*this%capa_)
    end if
    this%size_ = this%size_ + 1
    this%arr_(this%size_) = val
  end subroutine push_back_unwrapped_vector_ref_string
  !> push_back_array_unwrapped_vector_ref_string: Insert elemeents of array to the tail of elements of the unwrapped_vector_ref_string.
  subroutine push_back_array_unwrapped_vector_ref_string(this, arr)
    class(unwrapped_vector_ref_string), intent(inout) :: this
    type(ref_string), intent(in) :: arr(:)
    integer(int32) :: s
    s = size(arr)
    if (.not. allocated(this%arr_)) call this%init(s)
    if (this%size_ + s > this%capa_) then
       call this%resize(this%size_ + s)
    end if
    this%arr_(this%size_+1:this%size_+s) = arr(:)
    this%size_ = this%size_ + s
  end subroutine push_back_array_unwrapped_vector_ref_string
  !> pop_back_unwrapped_vector_ref_string: Delete the value in the end of arr_(:) of the unwrapped_vector_ref_string and return it.
  type(ref_string) function pop_back_unwrapped_vector_ref_string(this)
    class(unwrapped_vector_ref_string), intent(inout) :: this
    pop_back_unwrapped_vector_ref_string = this%arr_(this%size_)
    this%size_ = this%size_ - 1
  end function pop_back_unwrapped_vector_ref_string
  !> back_unwrapped_vector_ref_string: Delete the value in the end of arr_(:) of the unwrapped_vector_ref_string and return it.
  type(ref_string) function back_unwrapped_vector_ref_string(this)
    class(unwrapped_vector_ref_string), intent(inout) :: this
    back_unwrapped_vector_ref_string = this%arr_(this%size_)
  end function back_unwrapped_vector_ref_string
  !> size_vector_ref_string: Return current size of the unwrapped_vector_ref_string.
  pure integer(int32) function size_unwrapped_vector_ref_string(this)
    class(unwrapped_vector_ref_string), intent(in) :: this
    size_unwrapped_vector_ref_string = this%size_
  end function size_unwrapped_vector_ref_string
  !> resize_unwrapped_vector_ref_string: Shrink or expand arr_(:) of the unwrapped_vector_ref_string.
  subroutine resize_unwrapped_vector_ref_string(this, resize)
    class(unwrapped_vector_ref_string), intent(inout) :: this
    integer(int32), intent(in) :: resize
    type(ref_string), allocatable :: tmp(:)
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
  end subroutine resize_unwrapped_vector_ref_string
  !> lower_bound_vector_ref_string: Return the minimum index that is higher than or equal to .
  integer(int32) function lower_bound_unwrapped_vector_ref_string(this, val)
    class(unwrapped_vector_ref_string), intent(in) :: this
    type(ref_string), intent(in) :: val
    integer(int32) :: p, q, r
    p = 1
    r = this%size_
    if (this%arr_(r) < val) then
       lower_bound_unwrapped_vector_ref_string = r + 1
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
    lower_bound_unwrapped_vector_ref_string = q
  end function lower_bound_unwrapped_vector_ref_string
  
end module ref_string_unwrapped_vector_m
