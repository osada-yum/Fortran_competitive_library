module priority_queue_m
  use, intrinsic :: iso_fortran_env
  implicit none
  private
  public :: priority_queue_min_int32
  type :: priority_queue_min_int32
     private
     integer(int32) :: size_ = 0_int32, capa_ = 0_int32
     integer(int32), allocatable :: arr_(:)
   contains
     procedure, pass :: push  => push_priority_queue_min_int32
     procedure, pass :: pop   => pop_priority_queue_min_int32
     procedure, pass :: front => front_priority_queue_min_int32
     procedure, pass :: size => size_priority_queue_min_int32
     ! procedure, pass :: dump => dump_priority_queue_min_int32
  end type priority_queue_min_int32
  public :: priority_queue_max_int32
  type :: priority_queue_max_int32
     private
     integer(int32) :: size_ = 0_int32, capa_ = 0_int32
     integer(int32), allocatable :: arr_(:)
   contains
     procedure, pass :: push  => push_priority_queue_max_int32
     procedure, pass :: pop   => pop_priority_queue_max_int32
     procedure, pass :: front => front_priority_queue_max_int32
     procedure, pass :: size => size_priority_queue_max_int32
     ! procedure, pass :: dump => dump_priority_queue_max_int32
  end type priority_queue_max_int32
  
  public :: priority_queue_min_int64
  type :: priority_queue_min_int64
     private
     integer(int32) :: size_ = 0_int32, capa_ = 0_int32
     integer(int64), allocatable :: arr_(:)
   contains
     procedure, pass :: push  => push_priority_queue_min_int64
     procedure, pass :: pop   => pop_priority_queue_min_int64
     procedure, pass :: front => front_priority_queue_min_int64
     procedure, pass :: size => size_priority_queue_min_int64
     ! procedure, pass :: dump => dump_priority_queue_min_int64
  end type priority_queue_min_int64
  public :: priority_queue_max_int64
  type :: priority_queue_max_int64
     private
     integer(int32) :: size_ = 0_int32, capa_ = 0_int32
     integer(int64), allocatable :: arr_(:)
   contains
     procedure, pass :: push  => push_priority_queue_max_int64
     procedure, pass :: pop   => pop_priority_queue_max_int64
     procedure, pass :: front => front_priority_queue_max_int64
     procedure, pass :: size => size_priority_queue_max_int64
     ! procedure, pass :: dump => dump_priority_queue_max_int64
  end type priority_queue_max_int64
  
  public :: priority_queue_min_real32
  type :: priority_queue_min_real32
     private
     integer(int32) :: size_ = 0_int32, capa_ = 0_int32
     real(real32), allocatable :: arr_(:)
   contains
     procedure, pass :: push  => push_priority_queue_min_real32
     procedure, pass :: pop   => pop_priority_queue_min_real32
     procedure, pass :: front => front_priority_queue_min_real32
     procedure, pass :: size => size_priority_queue_min_real32
     ! procedure, pass :: dump => dump_priority_queue_min_real32
  end type priority_queue_min_real32
  public :: priority_queue_max_real32
  type :: priority_queue_max_real32
     private
     integer(int32) :: size_ = 0_int32, capa_ = 0_int32
     real(real32), allocatable :: arr_(:)
   contains
     procedure, pass :: push  => push_priority_queue_max_real32
     procedure, pass :: pop   => pop_priority_queue_max_real32
     procedure, pass :: front => front_priority_queue_max_real32
     procedure, pass :: size => size_priority_queue_max_real32
     ! procedure, pass :: dump => dump_priority_queue_max_real32
  end type priority_queue_max_real32
  
  public :: priority_queue_min_real64
  type :: priority_queue_min_real64
     private
     integer(int32) :: size_ = 0_int32, capa_ = 0_int32
     real(real64), allocatable :: arr_(:)
   contains
     procedure, pass :: push  => push_priority_queue_min_real64
     procedure, pass :: pop   => pop_priority_queue_min_real64
     procedure, pass :: front => front_priority_queue_min_real64
     procedure, pass :: size => size_priority_queue_min_real64
     ! procedure, pass :: dump => dump_priority_queue_min_real64
  end type priority_queue_min_real64
  public :: priority_queue_max_real64
  type :: priority_queue_max_real64
     private
     integer(int32) :: size_ = 0_int32, capa_ = 0_int32
     real(real64), allocatable :: arr_(:)
   contains
     procedure, pass :: push  => push_priority_queue_max_real64
     procedure, pass :: pop   => pop_priority_queue_max_real64
     procedure, pass :: front => front_priority_queue_max_real64
     procedure, pass :: size => size_priority_queue_max_real64
     ! procedure, pass :: dump => dump_priority_queue_max_real64
  end type priority_queue_max_real64
  
  public :: priority_queue_min_character
  type :: priority_queue_min_character
     private
     integer(int32) :: size_ = 0_int32, capa_ = 0_int32
     character, allocatable :: arr_(:)
   contains
     procedure, pass :: push  => push_priority_queue_min_character
     procedure, pass :: pop   => pop_priority_queue_min_character
     procedure, pass :: front => front_priority_queue_min_character
     procedure, pass :: size => size_priority_queue_min_character
     ! procedure, pass :: dump => dump_priority_queue_min_character
  end type priority_queue_min_character
  public :: priority_queue_max_character
  type :: priority_queue_max_character
     private
     integer(int32) :: size_ = 0_int32, capa_ = 0_int32
     character, allocatable :: arr_(:)
   contains
     procedure, pass :: push  => push_priority_queue_max_character
     procedure, pass :: pop   => pop_priority_queue_max_character
     procedure, pass :: front => front_priority_queue_max_character
     procedure, pass :: size => size_priority_queue_max_character
     ! procedure, pass :: dump => dump_priority_queue_max_character
  end type priority_queue_max_character
  
contains
  !> push_priority_queue_min_int32: adds an element to the heap and reconstructs the heap by < order.
  subroutine push_priority_queue_min_int32(this, val)
    class(priority_queue_min_int32), intent(inout) :: this
    integer(int32), intent(in) :: val
    integer(int32) :: tmp
    integer(int32) :: i
    if (this%size_ == this%capa_) then
       if (this%capa_ == 0) then
          this%capa_ = 1
          allocate(this%arr_(1))
       else
          this%capa_ = 2*this%capa_
          block
            integer(int32), allocatable :: tmp_arr(:)
            allocate(tmp_arr(this%capa_))
            tmp_arr(1:this%size_) = this%arr_(1:this%size_)
            call move_alloc(from = tmp_arr, to = this%arr_)
          end block
       end if
    end if
    this%size_ = this%size_ + 1
    ! add  to heap.
    this%arr_(this%size_) = val
    i = this%size_
    tmp = val
    upheap:do ! reconstruct the heap by <.
       if (i == 1) then ! top of the heap
          this%arr_(1) = tmp
          exit
       else if (tmp < this%arr_(i/2)) then ! move the element up in the heap
          this%arr_(i) = this%arr_(i/2)
       else ! move the element up in the heap
          this%arr_(i) = tmp
          tmp = this%arr_(i/2)
       end if
       i = i / 2
    end do upheap
  end subroutine push_priority_queue_min_int32
  !> pop_priority_queue_min_int32: extracts the < element from the heap.
  integer(int32) function pop_priority_queue_min_int32(this) result(res)
    class(priority_queue_min_int32), intent(inout) :: this
    integer(int32) :: n, prev, next
    ! add  to heap.
    ! swap  and  and delete < element, .
    res = this%arr_(1)
    this%arr_(1) = this%arr_(this%size_)
    this%size_ = this%size_ - 1
    n = this%size_
    ! reconstruct the heap by moving the element  downwards.
    next = 1
    downheap:do ! reconstruct the heap by <.
       prev = next
       if (2*prev > n) exit
       if (this%arr_(2*prev)      < this%arr_(next)) &
            next = 2*prev
       if (2*prev+1 <= n) then
          if (this%arr_(2*prev+1) < this%arr_(next)) &
               next = 2*prev+1
       end if
       if (prev == next) exit ! arr(next) < arr(2*prev) .and. arr(next) < arr(2*prev+1)
       call swap(this%arr_(prev), this%arr_(next))
    end do downheap
  contains
    subroutine swap(x, y)
      integer(int32), intent(inout) :: x, y
      integer(int32) :: tmp
      tmp = x
      x   = y
      y   = tmp
    end subroutine swap
  end function pop_priority_queue_min_int32
  !> front_priority_queue_min_int32: returns the top of the element of the heap, which has either the minimum or maximum value depending on the type of heap.
  pure integer(int32) function front_priority_queue_min_int32(this) result(res)
    class(priority_queue_min_int32), intent(in) :: this
    res = this%arr_(1)
  end function front_priority_queue_min_int32
  !> size_priority_queue_min_int32: returns the size of the heap.
  pure integer(int32) function size_priority_queue_min_int32(this) result(res)
    class(priority_queue_min_int32), intent(in) :: this
    res = this%size_
  end function size_priority_queue_min_int32
  ! !> dump_priority_queue_min_int32: output the heap.
  ! subroutine dump_priority_queue_min_int32(this)
  !   class(priority_queue_min_int32), intent(in) :: this
  !   write(error_unit, '(*(g0, 1x))') this%arr_(1:this%size_)
  ! end subroutine dump_priority_queue_min_int32
  !> push_priority_queue_max_int32: adds an element to the heap and reconstructs the heap by > order.
  subroutine push_priority_queue_max_int32(this, val)
    class(priority_queue_max_int32), intent(inout) :: this
    integer(int32), intent(in) :: val
    integer(int32) :: tmp
    integer(int32) :: i
    if (this%size_ == this%capa_) then
       if (this%capa_ == 0) then
          this%capa_ = 1
          allocate(this%arr_(1))
       else
          this%capa_ = 2*this%capa_
          block
            integer(int32), allocatable :: tmp_arr(:)
            allocate(tmp_arr(this%capa_))
            tmp_arr(1:this%size_) = this%arr_(1:this%size_)
            call move_alloc(from = tmp_arr, to = this%arr_)
          end block
       end if
    end if
    this%size_ = this%size_ + 1
    ! add  to heap.
    this%arr_(this%size_) = val
    i = this%size_
    tmp = val
    upheap:do ! reconstruct the heap by >.
       if (i == 1) then ! top of the heap
          this%arr_(1) = tmp
          exit
       else if (tmp > this%arr_(i/2)) then ! move the element up in the heap
          this%arr_(i) = this%arr_(i/2)
       else ! move the element up in the heap
          this%arr_(i) = tmp
          tmp = this%arr_(i/2)
       end if
       i = i / 2
    end do upheap
  end subroutine push_priority_queue_max_int32
  !> pop_priority_queue_max_int32: extracts the > element from the heap.
  integer(int32) function pop_priority_queue_max_int32(this) result(res)
    class(priority_queue_max_int32), intent(inout) :: this
    integer(int32) :: n, prev, next
    ! add  to heap.
    ! swap  and  and delete > element, .
    res = this%arr_(1)
    this%arr_(1) = this%arr_(this%size_)
    this%size_ = this%size_ - 1
    n = this%size_
    ! reconstruct the heap by moving the element  downwards.
    next = 1
    downheap:do ! reconstruct the heap by >.
       prev = next
       if (2*prev > n) exit
       if (this%arr_(2*prev)      > this%arr_(next)) &
            next = 2*prev
       if (2*prev+1 <= n) then
          if (this%arr_(2*prev+1) > this%arr_(next)) &
               next = 2*prev+1
       end if
       if (prev == next) exit ! arr(next) < arr(2*prev) .and. arr(next) < arr(2*prev+1)
       call swap(this%arr_(prev), this%arr_(next))
    end do downheap
  contains
    subroutine swap(x, y)
      integer(int32), intent(inout) :: x, y
      integer(int32) :: tmp
      tmp = x
      x   = y
      y   = tmp
    end subroutine swap
  end function pop_priority_queue_max_int32
  !> front_priority_queue_max_int32: returns the top of the element of the heap, which has either the minimum or maximum value depending on the type of heap.
  pure integer(int32) function front_priority_queue_max_int32(this) result(res)
    class(priority_queue_max_int32), intent(in) :: this
    res = this%arr_(1)
  end function front_priority_queue_max_int32
  !> size_priority_queue_max_int32: returns the size of the heap.
  pure integer(int32) function size_priority_queue_max_int32(this) result(res)
    class(priority_queue_max_int32), intent(in) :: this
    res = this%size_
  end function size_priority_queue_max_int32
  ! !> dump_priority_queue_max_int32: output the heap.
  ! subroutine dump_priority_queue_max_int32(this)
  !   class(priority_queue_max_int32), intent(in) :: this
  !   write(error_unit, '(*(g0, 1x))') this%arr_(1:this%size_)
  ! end subroutine dump_priority_queue_max_int32
  
  !> push_priority_queue_min_int64: adds an element to the heap and reconstructs the heap by < order.
  subroutine push_priority_queue_min_int64(this, val)
    class(priority_queue_min_int64), intent(inout) :: this
    integer(int64), intent(in) :: val
    integer(int64) :: tmp
    integer(int32) :: i
    if (this%size_ == this%capa_) then
       if (this%capa_ == 0) then
          this%capa_ = 1
          allocate(this%arr_(1))
       else
          this%capa_ = 2*this%capa_
          block
            integer(int64), allocatable :: tmp_arr(:)
            allocate(tmp_arr(this%capa_))
            tmp_arr(1:this%size_) = this%arr_(1:this%size_)
            call move_alloc(from = tmp_arr, to = this%arr_)
          end block
       end if
    end if
    this%size_ = this%size_ + 1
    ! add  to heap.
    this%arr_(this%size_) = val
    i = this%size_
    tmp = val
    upheap:do ! reconstruct the heap by <.
       if (i == 1) then ! top of the heap
          this%arr_(1) = tmp
          exit
       else if (tmp < this%arr_(i/2)) then ! move the element up in the heap
          this%arr_(i) = this%arr_(i/2)
       else ! move the element up in the heap
          this%arr_(i) = tmp
          tmp = this%arr_(i/2)
       end if
       i = i / 2
    end do upheap
  end subroutine push_priority_queue_min_int64
  !> pop_priority_queue_min_int64: extracts the < element from the heap.
  integer(int64) function pop_priority_queue_min_int64(this) result(res)
    class(priority_queue_min_int64), intent(inout) :: this
    integer(int32) :: n, prev, next
    ! add  to heap.
    ! swap  and  and delete < element, .
    res = this%arr_(1)
    this%arr_(1) = this%arr_(this%size_)
    this%size_ = this%size_ - 1
    n = this%size_
    ! reconstruct the heap by moving the element  downwards.
    next = 1
    downheap:do ! reconstruct the heap by <.
       prev = next
       if (2*prev > n) exit
       if (this%arr_(2*prev)      < this%arr_(next)) &
            next = 2*prev
       if (2*prev+1 <= n) then
          if (this%arr_(2*prev+1) < this%arr_(next)) &
               next = 2*prev+1
       end if
       if (prev == next) exit ! arr(next) < arr(2*prev) .and. arr(next) < arr(2*prev+1)
       call swap(this%arr_(prev), this%arr_(next))
    end do downheap
  contains
    subroutine swap(x, y)
      integer(int64), intent(inout) :: x, y
      integer(int64) :: tmp
      tmp = x
      x   = y
      y   = tmp
    end subroutine swap
  end function pop_priority_queue_min_int64
  !> front_priority_queue_min_int64: returns the top of the element of the heap, which has either the minimum or maximum value depending on the type of heap.
  pure integer(int64) function front_priority_queue_min_int64(this) result(res)
    class(priority_queue_min_int64), intent(in) :: this
    res = this%arr_(1)
  end function front_priority_queue_min_int64
  !> size_priority_queue_min_int64: returns the size of the heap.
  pure integer(int32) function size_priority_queue_min_int64(this) result(res)
    class(priority_queue_min_int64), intent(in) :: this
    res = this%size_
  end function size_priority_queue_min_int64
  ! !> dump_priority_queue_min_int64: output the heap.
  ! subroutine dump_priority_queue_min_int64(this)
  !   class(priority_queue_min_int64), intent(in) :: this
  !   write(error_unit, '(*(g0, 1x))') this%arr_(1:this%size_)
  ! end subroutine dump_priority_queue_min_int64
  !> push_priority_queue_max_int64: adds an element to the heap and reconstructs the heap by > order.
  subroutine push_priority_queue_max_int64(this, val)
    class(priority_queue_max_int64), intent(inout) :: this
    integer(int64), intent(in) :: val
    integer(int64) :: tmp
    integer(int32) :: i
    if (this%size_ == this%capa_) then
       if (this%capa_ == 0) then
          this%capa_ = 1
          allocate(this%arr_(1))
       else
          this%capa_ = 2*this%capa_
          block
            integer(int64), allocatable :: tmp_arr(:)
            allocate(tmp_arr(this%capa_))
            tmp_arr(1:this%size_) = this%arr_(1:this%size_)
            call move_alloc(from = tmp_arr, to = this%arr_)
          end block
       end if
    end if
    this%size_ = this%size_ + 1
    ! add  to heap.
    this%arr_(this%size_) = val
    i = this%size_
    tmp = val
    upheap:do ! reconstruct the heap by >.
       if (i == 1) then ! top of the heap
          this%arr_(1) = tmp
          exit
       else if (tmp > this%arr_(i/2)) then ! move the element up in the heap
          this%arr_(i) = this%arr_(i/2)
       else ! move the element up in the heap
          this%arr_(i) = tmp
          tmp = this%arr_(i/2)
       end if
       i = i / 2
    end do upheap
  end subroutine push_priority_queue_max_int64
  !> pop_priority_queue_max_int64: extracts the > element from the heap.
  integer(int64) function pop_priority_queue_max_int64(this) result(res)
    class(priority_queue_max_int64), intent(inout) :: this
    integer(int32) :: n, prev, next
    ! add  to heap.
    ! swap  and  and delete > element, .
    res = this%arr_(1)
    this%arr_(1) = this%arr_(this%size_)
    this%size_ = this%size_ - 1
    n = this%size_
    ! reconstruct the heap by moving the element  downwards.
    next = 1
    downheap:do ! reconstruct the heap by >.
       prev = next
       if (2*prev > n) exit
       if (this%arr_(2*prev)      > this%arr_(next)) &
            next = 2*prev
       if (2*prev+1 <= n) then
          if (this%arr_(2*prev+1) > this%arr_(next)) &
               next = 2*prev+1
       end if
       if (prev == next) exit ! arr(next) < arr(2*prev) .and. arr(next) < arr(2*prev+1)
       call swap(this%arr_(prev), this%arr_(next))
    end do downheap
  contains
    subroutine swap(x, y)
      integer(int64), intent(inout) :: x, y
      integer(int64) :: tmp
      tmp = x
      x   = y
      y   = tmp
    end subroutine swap
  end function pop_priority_queue_max_int64
  !> front_priority_queue_max_int64: returns the top of the element of the heap, which has either the minimum or maximum value depending on the type of heap.
  pure integer(int64) function front_priority_queue_max_int64(this) result(res)
    class(priority_queue_max_int64), intent(in) :: this
    res = this%arr_(1)
  end function front_priority_queue_max_int64
  !> size_priority_queue_max_int64: returns the size of the heap.
  pure integer(int32) function size_priority_queue_max_int64(this) result(res)
    class(priority_queue_max_int64), intent(in) :: this
    res = this%size_
  end function size_priority_queue_max_int64
  ! !> dump_priority_queue_max_int64: output the heap.
  ! subroutine dump_priority_queue_max_int64(this)
  !   class(priority_queue_max_int64), intent(in) :: this
  !   write(error_unit, '(*(g0, 1x))') this%arr_(1:this%size_)
  ! end subroutine dump_priority_queue_max_int64
  
  !> push_priority_queue_min_real32: adds an element to the heap and reconstructs the heap by < order.
  subroutine push_priority_queue_min_real32(this, val)
    class(priority_queue_min_real32), intent(inout) :: this
    real(real32), intent(in) :: val
    real(real32) :: tmp
    integer(int32) :: i
    if (this%size_ == this%capa_) then
       if (this%capa_ == 0) then
          this%capa_ = 1
          allocate(this%arr_(1))
       else
          this%capa_ = 2*this%capa_
          block
            real(real32), allocatable :: tmp_arr(:)
            allocate(tmp_arr(this%capa_))
            tmp_arr(1:this%size_) = this%arr_(1:this%size_)
            call move_alloc(from = tmp_arr, to = this%arr_)
          end block
       end if
    end if
    this%size_ = this%size_ + 1
    ! add  to heap.
    this%arr_(this%size_) = val
    i = this%size_
    tmp = val
    upheap:do ! reconstruct the heap by <.
       if (i == 1) then ! top of the heap
          this%arr_(1) = tmp
          exit
       else if (tmp < this%arr_(i/2)) then ! move the element up in the heap
          this%arr_(i) = this%arr_(i/2)
       else ! move the element up in the heap
          this%arr_(i) = tmp
          tmp = this%arr_(i/2)
       end if
       i = i / 2
    end do upheap
  end subroutine push_priority_queue_min_real32
  !> pop_priority_queue_min_real32: extracts the < element from the heap.
  real(real32) function pop_priority_queue_min_real32(this) result(res)
    class(priority_queue_min_real32), intent(inout) :: this
    integer(int32) :: n, prev, next
    ! add  to heap.
    ! swap  and  and delete < element, .
    res = this%arr_(1)
    this%arr_(1) = this%arr_(this%size_)
    this%size_ = this%size_ - 1
    n = this%size_
    ! reconstruct the heap by moving the element  downwards.
    next = 1
    downheap:do ! reconstruct the heap by <.
       prev = next
       if (2*prev > n) exit
       if (this%arr_(2*prev)      < this%arr_(next)) &
            next = 2*prev
       if (2*prev+1 <= n) then
          if (this%arr_(2*prev+1) < this%arr_(next)) &
               next = 2*prev+1
       end if
       if (prev == next) exit ! arr(next) < arr(2*prev) .and. arr(next) < arr(2*prev+1)
       call swap(this%arr_(prev), this%arr_(next))
    end do downheap
  contains
    subroutine swap(x, y)
      real(real32), intent(inout) :: x, y
      real(real32) :: tmp
      tmp = x
      x   = y
      y   = tmp
    end subroutine swap
  end function pop_priority_queue_min_real32
  !> front_priority_queue_min_real32: returns the top of the element of the heap, which has either the minimum or maximum value depending on the type of heap.
  pure real(real32) function front_priority_queue_min_real32(this) result(res)
    class(priority_queue_min_real32), intent(in) :: this
    res = this%arr_(1)
  end function front_priority_queue_min_real32
  !> size_priority_queue_min_real32: returns the size of the heap.
  pure integer(int32) function size_priority_queue_min_real32(this) result(res)
    class(priority_queue_min_real32), intent(in) :: this
    res = this%size_
  end function size_priority_queue_min_real32
  ! !> dump_priority_queue_min_real32: output the heap.
  ! subroutine dump_priority_queue_min_real32(this)
  !   class(priority_queue_min_real32), intent(in) :: this
  !   write(error_unit, '(*(g0, 1x))') this%arr_(1:this%size_)
  ! end subroutine dump_priority_queue_min_real32
  !> push_priority_queue_max_real32: adds an element to the heap and reconstructs the heap by > order.
  subroutine push_priority_queue_max_real32(this, val)
    class(priority_queue_max_real32), intent(inout) :: this
    real(real32), intent(in) :: val
    real(real32) :: tmp
    integer(int32) :: i
    if (this%size_ == this%capa_) then
       if (this%capa_ == 0) then
          this%capa_ = 1
          allocate(this%arr_(1))
       else
          this%capa_ = 2*this%capa_
          block
            real(real32), allocatable :: tmp_arr(:)
            allocate(tmp_arr(this%capa_))
            tmp_arr(1:this%size_) = this%arr_(1:this%size_)
            call move_alloc(from = tmp_arr, to = this%arr_)
          end block
       end if
    end if
    this%size_ = this%size_ + 1
    ! add  to heap.
    this%arr_(this%size_) = val
    i = this%size_
    tmp = val
    upheap:do ! reconstruct the heap by >.
       if (i == 1) then ! top of the heap
          this%arr_(1) = tmp
          exit
       else if (tmp > this%arr_(i/2)) then ! move the element up in the heap
          this%arr_(i) = this%arr_(i/2)
       else ! move the element up in the heap
          this%arr_(i) = tmp
          tmp = this%arr_(i/2)
       end if
       i = i / 2
    end do upheap
  end subroutine push_priority_queue_max_real32
  !> pop_priority_queue_max_real32: extracts the > element from the heap.
  real(real32) function pop_priority_queue_max_real32(this) result(res)
    class(priority_queue_max_real32), intent(inout) :: this
    integer(int32) :: n, prev, next
    ! add  to heap.
    ! swap  and  and delete > element, .
    res = this%arr_(1)
    this%arr_(1) = this%arr_(this%size_)
    this%size_ = this%size_ - 1
    n = this%size_
    ! reconstruct the heap by moving the element  downwards.
    next = 1
    downheap:do ! reconstruct the heap by >.
       prev = next
       if (2*prev > n) exit
       if (this%arr_(2*prev)      > this%arr_(next)) &
            next = 2*prev
       if (2*prev+1 <= n) then
          if (this%arr_(2*prev+1) > this%arr_(next)) &
               next = 2*prev+1
       end if
       if (prev == next) exit ! arr(next) < arr(2*prev) .and. arr(next) < arr(2*prev+1)
       call swap(this%arr_(prev), this%arr_(next))
    end do downheap
  contains
    subroutine swap(x, y)
      real(real32), intent(inout) :: x, y
      real(real32) :: tmp
      tmp = x
      x   = y
      y   = tmp
    end subroutine swap
  end function pop_priority_queue_max_real32
  !> front_priority_queue_max_real32: returns the top of the element of the heap, which has either the minimum or maximum value depending on the type of heap.
  pure real(real32) function front_priority_queue_max_real32(this) result(res)
    class(priority_queue_max_real32), intent(in) :: this
    res = this%arr_(1)
  end function front_priority_queue_max_real32
  !> size_priority_queue_max_real32: returns the size of the heap.
  pure integer(int32) function size_priority_queue_max_real32(this) result(res)
    class(priority_queue_max_real32), intent(in) :: this
    res = this%size_
  end function size_priority_queue_max_real32
  ! !> dump_priority_queue_max_real32: output the heap.
  ! subroutine dump_priority_queue_max_real32(this)
  !   class(priority_queue_max_real32), intent(in) :: this
  !   write(error_unit, '(*(g0, 1x))') this%arr_(1:this%size_)
  ! end subroutine dump_priority_queue_max_real32
  
  !> push_priority_queue_min_real64: adds an element to the heap and reconstructs the heap by < order.
  subroutine push_priority_queue_min_real64(this, val)
    class(priority_queue_min_real64), intent(inout) :: this
    real(real64), intent(in) :: val
    real(real64) :: tmp
    integer(int32) :: i
    if (this%size_ == this%capa_) then
       if (this%capa_ == 0) then
          this%capa_ = 1
          allocate(this%arr_(1))
       else
          this%capa_ = 2*this%capa_
          block
            real(real64), allocatable :: tmp_arr(:)
            allocate(tmp_arr(this%capa_))
            tmp_arr(1:this%size_) = this%arr_(1:this%size_)
            call move_alloc(from = tmp_arr, to = this%arr_)
          end block
       end if
    end if
    this%size_ = this%size_ + 1
    ! add  to heap.
    this%arr_(this%size_) = val
    i = this%size_
    tmp = val
    upheap:do ! reconstruct the heap by <.
       if (i == 1) then ! top of the heap
          this%arr_(1) = tmp
          exit
       else if (tmp < this%arr_(i/2)) then ! move the element up in the heap
          this%arr_(i) = this%arr_(i/2)
       else ! move the element up in the heap
          this%arr_(i) = tmp
          tmp = this%arr_(i/2)
       end if
       i = i / 2
    end do upheap
  end subroutine push_priority_queue_min_real64
  !> pop_priority_queue_min_real64: extracts the < element from the heap.
  real(real64) function pop_priority_queue_min_real64(this) result(res)
    class(priority_queue_min_real64), intent(inout) :: this
    integer(int32) :: n, prev, next
    ! add  to heap.
    ! swap  and  and delete < element, .
    res = this%arr_(1)
    this%arr_(1) = this%arr_(this%size_)
    this%size_ = this%size_ - 1
    n = this%size_
    ! reconstruct the heap by moving the element  downwards.
    next = 1
    downheap:do ! reconstruct the heap by <.
       prev = next
       if (2*prev > n) exit
       if (this%arr_(2*prev)      < this%arr_(next)) &
            next = 2*prev
       if (2*prev+1 <= n) then
          if (this%arr_(2*prev+1) < this%arr_(next)) &
               next = 2*prev+1
       end if
       if (prev == next) exit ! arr(next) < arr(2*prev) .and. arr(next) < arr(2*prev+1)
       call swap(this%arr_(prev), this%arr_(next))
    end do downheap
  contains
    subroutine swap(x, y)
      real(real64), intent(inout) :: x, y
      real(real64) :: tmp
      tmp = x
      x   = y
      y   = tmp
    end subroutine swap
  end function pop_priority_queue_min_real64
  !> front_priority_queue_min_real64: returns the top of the element of the heap, which has either the minimum or maximum value depending on the type of heap.
  pure real(real64) function front_priority_queue_min_real64(this) result(res)
    class(priority_queue_min_real64), intent(in) :: this
    res = this%arr_(1)
  end function front_priority_queue_min_real64
  !> size_priority_queue_min_real64: returns the size of the heap.
  pure integer(int32) function size_priority_queue_min_real64(this) result(res)
    class(priority_queue_min_real64), intent(in) :: this
    res = this%size_
  end function size_priority_queue_min_real64
  ! !> dump_priority_queue_min_real64: output the heap.
  ! subroutine dump_priority_queue_min_real64(this)
  !   class(priority_queue_min_real64), intent(in) :: this
  !   write(error_unit, '(*(g0, 1x))') this%arr_(1:this%size_)
  ! end subroutine dump_priority_queue_min_real64
  !> push_priority_queue_max_real64: adds an element to the heap and reconstructs the heap by > order.
  subroutine push_priority_queue_max_real64(this, val)
    class(priority_queue_max_real64), intent(inout) :: this
    real(real64), intent(in) :: val
    real(real64) :: tmp
    integer(int32) :: i
    if (this%size_ == this%capa_) then
       if (this%capa_ == 0) then
          this%capa_ = 1
          allocate(this%arr_(1))
       else
          this%capa_ = 2*this%capa_
          block
            real(real64), allocatable :: tmp_arr(:)
            allocate(tmp_arr(this%capa_))
            tmp_arr(1:this%size_) = this%arr_(1:this%size_)
            call move_alloc(from = tmp_arr, to = this%arr_)
          end block
       end if
    end if
    this%size_ = this%size_ + 1
    ! add  to heap.
    this%arr_(this%size_) = val
    i = this%size_
    tmp = val
    upheap:do ! reconstruct the heap by >.
       if (i == 1) then ! top of the heap
          this%arr_(1) = tmp
          exit
       else if (tmp > this%arr_(i/2)) then ! move the element up in the heap
          this%arr_(i) = this%arr_(i/2)
       else ! move the element up in the heap
          this%arr_(i) = tmp
          tmp = this%arr_(i/2)
       end if
       i = i / 2
    end do upheap
  end subroutine push_priority_queue_max_real64
  !> pop_priority_queue_max_real64: extracts the > element from the heap.
  real(real64) function pop_priority_queue_max_real64(this) result(res)
    class(priority_queue_max_real64), intent(inout) :: this
    integer(int32) :: n, prev, next
    ! add  to heap.
    ! swap  and  and delete > element, .
    res = this%arr_(1)
    this%arr_(1) = this%arr_(this%size_)
    this%size_ = this%size_ - 1
    n = this%size_
    ! reconstruct the heap by moving the element  downwards.
    next = 1
    downheap:do ! reconstruct the heap by >.
       prev = next
       if (2*prev > n) exit
       if (this%arr_(2*prev)      > this%arr_(next)) &
            next = 2*prev
       if (2*prev+1 <= n) then
          if (this%arr_(2*prev+1) > this%arr_(next)) &
               next = 2*prev+1
       end if
       if (prev == next) exit ! arr(next) < arr(2*prev) .and. arr(next) < arr(2*prev+1)
       call swap(this%arr_(prev), this%arr_(next))
    end do downheap
  contains
    subroutine swap(x, y)
      real(real64), intent(inout) :: x, y
      real(real64) :: tmp
      tmp = x
      x   = y
      y   = tmp
    end subroutine swap
  end function pop_priority_queue_max_real64
  !> front_priority_queue_max_real64: returns the top of the element of the heap, which has either the minimum or maximum value depending on the type of heap.
  pure real(real64) function front_priority_queue_max_real64(this) result(res)
    class(priority_queue_max_real64), intent(in) :: this
    res = this%arr_(1)
  end function front_priority_queue_max_real64
  !> size_priority_queue_max_real64: returns the size of the heap.
  pure integer(int32) function size_priority_queue_max_real64(this) result(res)
    class(priority_queue_max_real64), intent(in) :: this
    res = this%size_
  end function size_priority_queue_max_real64
  ! !> dump_priority_queue_max_real64: output the heap.
  ! subroutine dump_priority_queue_max_real64(this)
  !   class(priority_queue_max_real64), intent(in) :: this
  !   write(error_unit, '(*(g0, 1x))') this%arr_(1:this%size_)
  ! end subroutine dump_priority_queue_max_real64
  
  !> push_priority_queue_min_character: adds an element to the heap and reconstructs the heap by < order.
  subroutine push_priority_queue_min_character(this, val)
    class(priority_queue_min_character), intent(inout) :: this
    character, intent(in) :: val
    character :: tmp
    integer(int32) :: i
    if (this%size_ == this%capa_) then
       if (this%capa_ == 0) then
          this%capa_ = 1
          allocate(this%arr_(1))
       else
          this%capa_ = 2*this%capa_
          block
            character, allocatable :: tmp_arr(:)
            allocate(tmp_arr(this%capa_))
            tmp_arr(1:this%size_) = this%arr_(1:this%size_)
            call move_alloc(from = tmp_arr, to = this%arr_)
          end block
       end if
    end if
    this%size_ = this%size_ + 1
    ! add  to heap.
    this%arr_(this%size_) = val
    i = this%size_
    tmp = val
    upheap:do ! reconstruct the heap by <.
       if (i == 1) then ! top of the heap
          this%arr_(1) = tmp
          exit
       else if (tmp < this%arr_(i/2)) then ! move the element up in the heap
          this%arr_(i) = this%arr_(i/2)
       else ! move the element up in the heap
          this%arr_(i) = tmp
          tmp = this%arr_(i/2)
       end if
       i = i / 2
    end do upheap
  end subroutine push_priority_queue_min_character
  !> pop_priority_queue_min_character: extracts the < element from the heap.
  character function pop_priority_queue_min_character(this) result(res)
    class(priority_queue_min_character), intent(inout) :: this
    integer(int32) :: n, prev, next
    ! add  to heap.
    ! swap  and  and delete < element, .
    res = this%arr_(1)
    this%arr_(1) = this%arr_(this%size_)
    this%size_ = this%size_ - 1
    n = this%size_
    ! reconstruct the heap by moving the element  downwards.
    next = 1
    downheap:do ! reconstruct the heap by <.
       prev = next
       if (2*prev > n) exit
       if (this%arr_(2*prev)      < this%arr_(next)) &
            next = 2*prev
       if (2*prev+1 <= n) then
          if (this%arr_(2*prev+1) < this%arr_(next)) &
               next = 2*prev+1
       end if
       if (prev == next) exit ! arr(next) < arr(2*prev) .and. arr(next) < arr(2*prev+1)
       call swap(this%arr_(prev), this%arr_(next))
    end do downheap
  contains
    subroutine swap(x, y)
      character, intent(inout) :: x, y
      character :: tmp
      tmp = x
      x   = y
      y   = tmp
    end subroutine swap
  end function pop_priority_queue_min_character
  !> front_priority_queue_min_character: returns the top of the element of the heap, which has either the minimum or maximum value depending on the type of heap.
  pure character function front_priority_queue_min_character(this) result(res)
    class(priority_queue_min_character), intent(in) :: this
    res = this%arr_(1)
  end function front_priority_queue_min_character
  !> size_priority_queue_min_character: returns the size of the heap.
  pure integer(int32) function size_priority_queue_min_character(this) result(res)
    class(priority_queue_min_character), intent(in) :: this
    res = this%size_
  end function size_priority_queue_min_character
  ! !> dump_priority_queue_min_character: output the heap.
  ! subroutine dump_priority_queue_min_character(this)
  !   class(priority_queue_min_character), intent(in) :: this
  !   write(error_unit, '(*(g0, 1x))') this%arr_(1:this%size_)
  ! end subroutine dump_priority_queue_min_character
  !> push_priority_queue_max_character: adds an element to the heap and reconstructs the heap by > order.
  subroutine push_priority_queue_max_character(this, val)
    class(priority_queue_max_character), intent(inout) :: this
    character, intent(in) :: val
    character :: tmp
    integer(int32) :: i
    if (this%size_ == this%capa_) then
       if (this%capa_ == 0) then
          this%capa_ = 1
          allocate(this%arr_(1))
       else
          this%capa_ = 2*this%capa_
          block
            character, allocatable :: tmp_arr(:)
            allocate(tmp_arr(this%capa_))
            tmp_arr(1:this%size_) = this%arr_(1:this%size_)
            call move_alloc(from = tmp_arr, to = this%arr_)
          end block
       end if
    end if
    this%size_ = this%size_ + 1
    ! add  to heap.
    this%arr_(this%size_) = val
    i = this%size_
    tmp = val
    upheap:do ! reconstruct the heap by >.
       if (i == 1) then ! top of the heap
          this%arr_(1) = tmp
          exit
       else if (tmp > this%arr_(i/2)) then ! move the element up in the heap
          this%arr_(i) = this%arr_(i/2)
       else ! move the element up in the heap
          this%arr_(i) = tmp
          tmp = this%arr_(i/2)
       end if
       i = i / 2
    end do upheap
  end subroutine push_priority_queue_max_character
  !> pop_priority_queue_max_character: extracts the > element from the heap.
  character function pop_priority_queue_max_character(this) result(res)
    class(priority_queue_max_character), intent(inout) :: this
    integer(int32) :: n, prev, next
    ! add  to heap.
    ! swap  and  and delete > element, .
    res = this%arr_(1)
    this%arr_(1) = this%arr_(this%size_)
    this%size_ = this%size_ - 1
    n = this%size_
    ! reconstruct the heap by moving the element  downwards.
    next = 1
    downheap:do ! reconstruct the heap by >.
       prev = next
       if (2*prev > n) exit
       if (this%arr_(2*prev)      > this%arr_(next)) &
            next = 2*prev
       if (2*prev+1 <= n) then
          if (this%arr_(2*prev+1) > this%arr_(next)) &
               next = 2*prev+1
       end if
       if (prev == next) exit ! arr(next) < arr(2*prev) .and. arr(next) < arr(2*prev+1)
       call swap(this%arr_(prev), this%arr_(next))
    end do downheap
  contains
    subroutine swap(x, y)
      character, intent(inout) :: x, y
      character :: tmp
      tmp = x
      x   = y
      y   = tmp
    end subroutine swap
  end function pop_priority_queue_max_character
  !> front_priority_queue_max_character: returns the top of the element of the heap, which has either the minimum or maximum value depending on the type of heap.
  pure character function front_priority_queue_max_character(this) result(res)
    class(priority_queue_max_character), intent(in) :: this
    res = this%arr_(1)
  end function front_priority_queue_max_character
  !> size_priority_queue_max_character: returns the size of the heap.
  pure integer(int32) function size_priority_queue_max_character(this) result(res)
    class(priority_queue_max_character), intent(in) :: this
    res = this%size_
  end function size_priority_queue_max_character
  ! !> dump_priority_queue_max_character: output the heap.
  ! subroutine dump_priority_queue_max_character(this)
  !   class(priority_queue_max_character), intent(in) :: this
  !   write(error_unit, '(*(g0, 1x))') this%arr_(1:this%size_)
  ! end subroutine dump_priority_queue_max_character
  
end module priority_queue_m
