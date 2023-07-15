module queue_m
  use, intrinsic :: iso_fortran_env
  use unwrapped_vector_m
  implicit none
  private
  public :: queue
  type :: queue
     private
     integer(int32) :: head_, tail_
     type(unwrapped_vector_int32) :: q_
   contains
     procedure, pass :: init => init_queue
     procedure, pass :: push_back => push_back_queue
     procedure, pass :: pop_front => pop_front_queue
     procedure, pass :: size => size_front_queue
     procedure, pass :: empty => empty_front_queue
  end type queue
contains
  subroutine init_queue(this)
    class(queue), intent(inout) :: this
    this%head_ = 1
    this%tail_ = 0
  end subroutine init_queue
  subroutine push_back_queue(this, val)
    class(queue), intent(inout) :: this
    integer(int32), intent(in) :: val
    integer(int32) :: s
    if (this%head_ == this%q_%size()) then
       s = this%tail_ - (this%head_-1)
       this%q_%arr_(1:s) = eoshift(this%q_%arr_(:), shift = this%head_-1)
       this%tail_ = s
       this%head_ = 1
       call this%q_%resize(this%size())
    end if
    this%tail_ = this%tail_ + 1
    call this%q_%push_back(val)
  end subroutine push_back_queue
  integer(int32) function pop_front_queue(this) result(res)
    class(queue), intent(inout) :: this
    res = this%q_%arr_(this%head_)
    this%head_ = this%head_ + 1
  end function pop_front_queue
  pure integer(int32) function size_front_queue(this)
    class(queue), intent(in) :: this
    size_front_queue = this%tail_ - this%head_ + 1
  end function size_front_queue
  pure logical function empty_front_queue(this)
    class(queue), intent(in) :: this
    empty_front_queue = this%tail_ == this%head_ -1
  end function empty_front_queue
end module queue_m
