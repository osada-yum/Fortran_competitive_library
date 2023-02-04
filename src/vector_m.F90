module vector_m
  use, intrinsic :: iso_fortran_env
  implicit none
  private
public :: vector_int32
type :: vector_int32
   private
   integer(int32), allocatable :: arr_(:)
   integer(int32) :: size_ = 0, capa_ = 0, lb_ = 0
 contains
   procedure, pass :: init_vector_int32, init_vector_range_int32
   generic         :: init      => init_vector_int32, init_vector_range_int32
   procedure, pass :: push_back => push_back_vector_int32
   procedure, pass :: pop_back  => pop_back_vector_int32
   procedure, pass :: size      => size_vector_int32
   procedure, pass :: lbound    => lbound_vector_int32
   procedure, pass :: ubound    => ubound_vector_int32
   procedure, pass :: resize_vector_int32, resize_vector_range_int32
   generic         :: resize    => resize_vector_int32, resize_vector_range_int32
   procedure, pass :: at        => at_vector_int32
   procedure, pass :: replace   => replace_vector_int32
   procedure, pass :: lower_bound => lower_bound_vector_int32
   ! procedure, pass :: make_iter => make_iter_vector_int32
end type vector_int32

! public :: iterator_vector_int32
! type :: iterator_vector_int32
!    private
!    type(vector_int32), pointer :: vec_ptr
!    integer(int32) :: iter_
!  contains
!    procedure, pass :: next  => next_iterator_vector_int32
!    procedure, pass :: prev  => prev_iterator_vector_int32
!    procedure, pass :: begin => begin_iterator_vector_int32
!    procedure, pass :: end   => end_iterator_vector_int32
!    procedure, pass :: val   => val_iterator_vector_int32
! end type vector_int32

public :: vector_int64
type :: vector_int64
   private
   integer(int64), allocatable :: arr_(:)
   integer(int32) :: size_ = 0, capa_ = 0, lb_ = 0
 contains
   procedure, pass :: init_vector_int64, init_vector_range_int64
   generic         :: init      => init_vector_int64, init_vector_range_int64
   procedure, pass :: push_back => push_back_vector_int64
   procedure, pass :: pop_back  => pop_back_vector_int64
   procedure, pass :: size      => size_vector_int64
   procedure, pass :: lbound    => lbound_vector_int64
   procedure, pass :: ubound    => ubound_vector_int64
   procedure, pass :: resize_vector_int64, resize_vector_range_int64
   generic         :: resize    => resize_vector_int64, resize_vector_range_int64
   procedure, pass :: at        => at_vector_int64
   procedure, pass :: replace   => replace_vector_int64
   procedure, pass :: lower_bound => lower_bound_vector_int64
   ! procedure, pass :: make_iter => make_iter_vector_int64
end type vector_int64

! public :: iterator_vector_int64
! type :: iterator_vector_int64
!    private
!    type(vector_int64), pointer :: vec_ptr
!    integer(int32) :: iter_
!  contains
!    procedure, pass :: next  => next_iterator_vector_int64
!    procedure, pass :: prev  => prev_iterator_vector_int64
!    procedure, pass :: begin => begin_iterator_vector_int64
!    procedure, pass :: end   => end_iterator_vector_int64
!    procedure, pass :: val   => val_iterator_vector_int64
! end type vector_int64

public :: vector_real32
type :: vector_real32
   private
   real(real32), allocatable :: arr_(:)
   integer(int32) :: size_ = 0, capa_ = 0, lb_ = 0
 contains
   procedure, pass :: init_vector_real32, init_vector_range_real32
   generic         :: init      => init_vector_real32, init_vector_range_real32
   procedure, pass :: push_back => push_back_vector_real32
   procedure, pass :: pop_back  => pop_back_vector_real32
   procedure, pass :: size      => size_vector_real32
   procedure, pass :: lbound    => lbound_vector_real32
   procedure, pass :: ubound    => ubound_vector_real32
   procedure, pass :: resize_vector_real32, resize_vector_range_real32
   generic         :: resize    => resize_vector_real32, resize_vector_range_real32
   procedure, pass :: at        => at_vector_real32
   procedure, pass :: replace   => replace_vector_real32
   procedure, pass :: lower_bound => lower_bound_vector_real32
   ! procedure, pass :: make_iter => make_iter_vector_real32
end type vector_real32

! public :: iterator_vector_real32
! type :: iterator_vector_real32
!    private
!    type(vector_real32), pointer :: vec_ptr
!    integer(int32) :: iter_
!  contains
!    procedure, pass :: next  => next_iterator_vector_real32
!    procedure, pass :: prev  => prev_iterator_vector_real32
!    procedure, pass :: begin => begin_iterator_vector_real32
!    procedure, pass :: end   => end_iterator_vector_real32
!    procedure, pass :: val   => val_iterator_vector_real32
! end type vector_real32

public :: vector_real64
type :: vector_real64
   private
   real(real64), allocatable :: arr_(:)
   integer(int32) :: size_ = 0, capa_ = 0, lb_ = 0
 contains
   procedure, pass :: init_vector_real64, init_vector_range_real64
   generic         :: init      => init_vector_real64, init_vector_range_real64
   procedure, pass :: push_back => push_back_vector_real64
   procedure, pass :: pop_back  => pop_back_vector_real64
   procedure, pass :: size      => size_vector_real64
   procedure, pass :: lbound    => lbound_vector_real64
   procedure, pass :: ubound    => ubound_vector_real64
   procedure, pass :: resize_vector_real64, resize_vector_range_real64
   generic         :: resize    => resize_vector_real64, resize_vector_range_real64
   procedure, pass :: at        => at_vector_real64
   procedure, pass :: replace   => replace_vector_real64
   procedure, pass :: lower_bound => lower_bound_vector_real64
   ! procedure, pass :: make_iter => make_iter_vector_real64
end type vector_real64

! public :: iterator_vector_real64
! type :: iterator_vector_real64
!    private
!    type(vector_real64), pointer :: vec_ptr
!    integer(int32) :: iter_
!  contains
!    procedure, pass :: next  => next_iterator_vector_real64
!    procedure, pass :: prev  => prev_iterator_vector_real64
!    procedure, pass :: begin => begin_iterator_vector_real64
!    procedure, pass :: end   => end_iterator_vector_real64
!    procedure, pass :: val   => val_iterator_vector_real64
! end type vector_real64

public :: vector_character
type :: vector_character
   private
   character, allocatable :: arr_(:)
   integer(int32) :: size_ = 0, capa_ = 0, lb_ = 0
 contains
   procedure, pass :: init_vector_character, init_vector_range_character
   generic         :: init      => init_vector_character, init_vector_range_character
   procedure, pass :: push_back => push_back_vector_character
   procedure, pass :: pop_back  => pop_back_vector_character
   procedure, pass :: size      => size_vector_character
   procedure, pass :: lbound    => lbound_vector_character
   procedure, pass :: ubound    => ubound_vector_character
   procedure, pass :: resize_vector_character, resize_vector_range_character
   generic         :: resize    => resize_vector_character, resize_vector_range_character
   procedure, pass :: at        => at_vector_character
   procedure, pass :: replace   => replace_vector_character
   procedure, pass :: lower_bound => lower_bound_vector_character
   ! procedure, pass :: make_iter => make_iter_vector_character
end type vector_character

! public :: iterator_vector_character
! type :: iterator_vector_character
!    private
!    type(vector_character), pointer :: vec_ptr
!    integer(int32) :: iter_
!  contains
!    procedure, pass :: next  => next_iterator_vector_character
!    procedure, pass :: prev  => prev_iterator_vector_character
!    procedure, pass :: begin => begin_iterator_vector_character
!    procedure, pass :: end   => end_iterator_vector_character
!    procedure, pass :: val   => val_iterator_vector_character
! end type vector_character

contains
!> init_vector_int32: Initialize the vector_int32 by size.
subroutine init_vector_int32(this, n)
  class(vector_int32), intent(inout) :: this
  integer(int32), intent(in) :: n
  if (.not. allocated(this%arr_)) then
     allocate(this%arr_(n))
     this%size_ = 0
     this%capa_ = n
     this%lb_   = 1
#ifdef DEBUG
  else
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
       write(error_unit, '(a)')&
         "Vector is already allocated..."
       error stop 1
     
#endif
  end if
end subroutine init_vector_int32

!> init_vector_range_int32: Initialize the vector_int32 by size.
subroutine init_vector_range_int32(this, lb, ub, ierr)
  class(vector_int32), intent(inout) :: this
  integer(int32), intent(in) :: lb, ub
  integer(int32), intent(out), optional :: ierr
  if (lb > ub) then
       if (present(ierr)) then
         ierr = 1
         return
       end if
     
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
       write(error_unit, '(a)')&
         "init_vector_range_int32:  must be larger than or equal to "
       error stop 2
     
  end if
  call this%init(ub-lb+1)
  this%lb_ = lb
  if (present(ierr)) ierr = 0
end subroutine init_vector_range_int32
!> push_back_vector_int32: Insert value to the tail of elements of the vector.
subroutine push_back_vector_int32(this, val)
  class(vector_int32), intent(inout) :: this
  integer(int32), intent(in) :: val
  if (.not. allocated(this%arr_)) call this%init(1)
  if (this%size_ == this%capa_) then
     call this%resize(2*this%capa_)
  end if
  this%size_ = this%size_ + 1
  this%arr_(this%size_) = val
end subroutine push_back_vector_int32
!> push_back_vector_int32: Delete the value in the end of arr_(:) of the vector and return it.
integer(int32) function pop_back_vector_int32(this, ierr)
  class(vector_int32), intent(inout) :: this
  integer(int32), intent(out), optional :: ierr
  if (this%size_ == 0) then
       if (present(ierr)) then
         ierr = 1
         return
       end if
     
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     
  end if
  pop_back_vector_int32 = this%arr_(this%size_)
  this%size_ = this%size_ - 1
  if (present(ierr)) ierr = 0
end function pop_back_vector_int32
!> size_vector_int32: Return current size of the vector.
pure integer(int32) function size_vector_int32(this)
  class(vector_int32), intent(in) :: this
  size_vector_int32 = this%size_
end function size_vector_int32
!> size_vector_int32: Return current lbound of the vector.
pure integer(int32) function lbound_vector_int32(this)
  class(vector_int32), intent(in) :: this
  lbound_vector_int32 = this%lb_
end function lbound_vector_int32
!> size_vector_int32: Return current ubonud of the vector.
pure integer(int32) function ubound_vector_int32(this)
  class(vector_int32), intent(in) :: this
  ubound_vector_int32 = this%lb_ + this%size_ - 1
end function ubound_vector_int32
!> resize_vector_int32: Shrink or expand arr_(:) of the vector.
subroutine resize_vector_int32(this, resize)
  class(vector_int32), intent(inout) :: this
  integer(int32), intent(in) :: resize
  integer(int32), allocatable :: tmp(:)
  if (this%capa_ == resize) return
  allocate(tmp(resize))
  this%size_ = min(this%size_, resize)
  tmp(1:this%size_) = this%arr_(1:this%size_)
  call move_alloc(from = tmp, to = this%arr_)
  this%capa_ = resize
end subroutine resize_vector_int32

!> resize_range_vector_int32: Shrink or expand arr_(:) of the vector by lb and ub.
subroutine resize_vector_range_int32(this, lb, ub, ierr)
  class(vector_int32), intent(inout) :: this
  integer(int32), intent(in) :: lb, ub
  integer(int32), intent(out), optional :: ierr
  if (ub > lb) then
       if (present(ierr)) then
         ierr = 1
         return
       end if
     
       write(error_unit, '(a, i0, a)', advance = "no")&
            "Error in "//&
            __FILE__&
            //":", __LINE__, ":"
         write(error_unit, '(a)')&
           "resize_range_vector_range_int32:  must be larger than or equal tox "
         error stop 2
       
  end if
  call this%resize(ub-lb+1)
  this%lb_ = lb
  if (present(ierr)) ierr = 0
end subroutine resize_vector_range_int32
!> at_vector_int32: Return the element that locate at  of the vector.
integer(int32) function at_vector_int32(this, i, ierr)
  class(vector_int32), intent(in) :: this
  integer(int32), intent(in) :: i
  integer(int32), intent(out), optional :: ierr
if (i < this%lbound() .or. i > this%ubound()) then
     if (present(ierr)) then
       ierr = i - this%lbound()
       return
     end if
   
#ifdef DEBUG
   write(error_unit, '(a, i0, a)', advance = "no")&
        "Error in "//&
        __FILE__&
        //":", __LINE__, ":"
   write(error_unit, '(a, *(i0, a))')&
        "Index ", i, " Out of bounds(", this%lbound(), ", ", this%ubound(), ")"
     error stop 1
   
#endif
end if
  at_vector_int32 = this%arr_(i - this%lb_ + 1)
  if (present(ierr)) ierr = 0
end function at_vector_int32
!> replace_vector_int32: Shrink or expand arr_(:) of the vector.
subroutine replace_vector_int32(this, i, val, ierr)
  class(vector_int32), intent(inout) :: this
  integer(int32), intent(in) :: i
  integer(int32), intent(out), optional :: ierr
  integer(int32), intent(in) :: val
if (i < this%lbound() .or. i > this%ubound()) then
     if (present(ierr)) then
       ierr = i - this%lbound()
       return
     end if
   
#ifdef DEBUG
   write(error_unit, '(a, i0, a)', advance = "no")&
        "Error in "//&
        __FILE__&
        //":", __LINE__, ":"
   write(error_unit, '(a, *(i0, a))')&
        "Index ", i, " Out of bounds(", this%lbound(), ", ", this%ubound(), ")"
     error stop 1
   
#endif
end if
  this%arr_(i - this%lb_ + 1) = val
  if (present(ierr)) ierr = 0
end subroutine replace_vector_int32
!> lower_bound_vector_int32: Return the minimum index that is higher than or equal to .
integer(int32) function lower_bound_vector_int32(this, val)
  class(vector_int32), intent(in) :: this
  integer(int32), intent(in) :: val
  integer(int32) :: p, q, r
  p = 1
  r = this%size_
  if (this%arr_(r) < val) then
     lower_bound_vector_int32 = r + 1 + (this%lb_ - 1)
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
  lower_bound_vector_int32 = q + (this%lb_ - 1)
end function lower_bound_vector_int32

!> init_vector_int64: Initialize the vector_int64 by size.
subroutine init_vector_int64(this, n)
  class(vector_int64), intent(inout) :: this
  integer(int32), intent(in) :: n
  if (.not. allocated(this%arr_)) then
     allocate(this%arr_(n))
     this%size_ = 0
     this%capa_ = n
     this%lb_   = 1
#ifdef DEBUG
  else
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
       write(error_unit, '(a)')&
         "Vector is already allocated..."
       error stop 1
     
#endif
  end if
end subroutine init_vector_int64

!> init_vector_range_int64: Initialize the vector_int64 by size.
subroutine init_vector_range_int64(this, lb, ub, ierr)
  class(vector_int64), intent(inout) :: this
  integer(int32), intent(in) :: lb, ub
  integer(int32), intent(out), optional :: ierr
  if (lb > ub) then
       if (present(ierr)) then
         ierr = 1
         return
       end if
     
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
       write(error_unit, '(a)')&
         "init_vector_range_int64:  must be larger than or equal to "
       error stop 2
     
  end if
  call this%init(ub-lb+1)
  this%lb_ = lb
  if (present(ierr)) ierr = 0
end subroutine init_vector_range_int64
!> push_back_vector_int64: Insert value to the tail of elements of the vector.
subroutine push_back_vector_int64(this, val)
  class(vector_int64), intent(inout) :: this
  integer(int64), intent(in) :: val
  if (.not. allocated(this%arr_)) call this%init(1)
  if (this%size_ == this%capa_) then
     call this%resize(2*this%capa_)
  end if
  this%size_ = this%size_ + 1
  this%arr_(this%size_) = val
end subroutine push_back_vector_int64
!> push_back_vector_int64: Delete the value in the end of arr_(:) of the vector and return it.
integer(int64) function pop_back_vector_int64(this, ierr)
  class(vector_int64), intent(inout) :: this
  integer(int32), intent(out), optional :: ierr
  if (this%size_ == 0) then
       if (present(ierr)) then
         ierr = 1
         return
       end if
     
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     
  end if
  pop_back_vector_int64 = this%arr_(this%size_)
  this%size_ = this%size_ - 1
  if (present(ierr)) ierr = 0
end function pop_back_vector_int64
!> size_vector_int64: Return current size of the vector.
pure integer(int32) function size_vector_int64(this)
  class(vector_int64), intent(in) :: this
  size_vector_int64 = this%size_
end function size_vector_int64
!> size_vector_int64: Return current lbound of the vector.
pure integer(int32) function lbound_vector_int64(this)
  class(vector_int64), intent(in) :: this
  lbound_vector_int64 = this%lb_
end function lbound_vector_int64
!> size_vector_int64: Return current ubonud of the vector.
pure integer(int32) function ubound_vector_int64(this)
  class(vector_int64), intent(in) :: this
  ubound_vector_int64 = this%lb_ + this%size_ - 1
end function ubound_vector_int64
!> resize_vector_int64: Shrink or expand arr_(:) of the vector.
subroutine resize_vector_int64(this, resize)
  class(vector_int64), intent(inout) :: this
  integer(int32), intent(in) :: resize
  integer(int64), allocatable :: tmp(:)
  if (this%capa_ == resize) return
  allocate(tmp(resize))
  this%size_ = min(this%size_, resize)
  tmp(1:this%size_) = this%arr_(1:this%size_)
  call move_alloc(from = tmp, to = this%arr_)
  this%capa_ = resize
end subroutine resize_vector_int64

!> resize_range_vector_int64: Shrink or expand arr_(:) of the vector by lb and ub.
subroutine resize_vector_range_int64(this, lb, ub, ierr)
  class(vector_int64), intent(inout) :: this
  integer(int32), intent(in) :: lb, ub
  integer(int32), intent(out), optional :: ierr
  if (ub > lb) then
       if (present(ierr)) then
         ierr = 1
         return
       end if
     
       write(error_unit, '(a, i0, a)', advance = "no")&
            "Error in "//&
            __FILE__&
            //":", __LINE__, ":"
         write(error_unit, '(a)')&
           "resize_range_vector_range_int64:  must be larger than or equal tox "
         error stop 2
       
  end if
  call this%resize(ub-lb+1)
  this%lb_ = lb
  if (present(ierr)) ierr = 0
end subroutine resize_vector_range_int64
!> at_vector_int64: Return the element that locate at  of the vector.
integer(int64) function at_vector_int64(this, i, ierr)
  class(vector_int64), intent(in) :: this
  integer(int32), intent(in) :: i
  integer(int32), intent(out), optional :: ierr
if (i < this%lbound() .or. i > this%ubound()) then
     if (present(ierr)) then
       ierr = i - this%lbound()
       return
     end if
   
#ifdef DEBUG
   write(error_unit, '(a, i0, a)', advance = "no")&
        "Error in "//&
        __FILE__&
        //":", __LINE__, ":"
   write(error_unit, '(a, *(i0, a))')&
        "Index ", i, " Out of bounds(", this%lbound(), ", ", this%ubound(), ")"
     error stop 1
   
#endif
end if
  at_vector_int64 = this%arr_(i - this%lb_ + 1)
  if (present(ierr)) ierr = 0
end function at_vector_int64
!> replace_vector_int64: Shrink or expand arr_(:) of the vector.
subroutine replace_vector_int64(this, i, val, ierr)
  class(vector_int64), intent(inout) :: this
  integer(int32), intent(in) :: i
  integer(int32), intent(out), optional :: ierr
  integer(int64), intent(in) :: val
if (i < this%lbound() .or. i > this%ubound()) then
     if (present(ierr)) then
       ierr = i - this%lbound()
       return
     end if
   
#ifdef DEBUG
   write(error_unit, '(a, i0, a)', advance = "no")&
        "Error in "//&
        __FILE__&
        //":", __LINE__, ":"
   write(error_unit, '(a, *(i0, a))')&
        "Index ", i, " Out of bounds(", this%lbound(), ", ", this%ubound(), ")"
     error stop 1
   
#endif
end if
  this%arr_(i - this%lb_ + 1) = val
  if (present(ierr)) ierr = 0
end subroutine replace_vector_int64
!> lower_bound_vector_int64: Return the minimum index that is higher than or equal to .
integer(int32) function lower_bound_vector_int64(this, val)
  class(vector_int64), intent(in) :: this
  integer(int64), intent(in) :: val
  integer(int32) :: p, q, r
  p = 1
  r = this%size_
  if (this%arr_(r) < val) then
     lower_bound_vector_int64 = r + 1 + (this%lb_ - 1)
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
  lower_bound_vector_int64 = q + (this%lb_ - 1)
end function lower_bound_vector_int64

!> init_vector_real32: Initialize the vector_real32 by size.
subroutine init_vector_real32(this, n)
  class(vector_real32), intent(inout) :: this
  integer(int32), intent(in) :: n
  if (.not. allocated(this%arr_)) then
     allocate(this%arr_(n))
     this%size_ = 0
     this%capa_ = n
     this%lb_   = 1
#ifdef DEBUG
  else
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
       write(error_unit, '(a)')&
         "Vector is already allocated..."
       error stop 1
     
#endif
  end if
end subroutine init_vector_real32

!> init_vector_range_real32: Initialize the vector_real32 by size.
subroutine init_vector_range_real32(this, lb, ub, ierr)
  class(vector_real32), intent(inout) :: this
  integer(int32), intent(in) :: lb, ub
  integer(int32), intent(out), optional :: ierr
  if (lb > ub) then
       if (present(ierr)) then
         ierr = 1
         return
       end if
     
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
       write(error_unit, '(a)')&
         "init_vector_range_real32:  must be larger than or equal to "
       error stop 2
     
  end if
  call this%init(ub-lb+1)
  this%lb_ = lb
  if (present(ierr)) ierr = 0
end subroutine init_vector_range_real32
!> push_back_vector_real32: Insert value to the tail of elements of the vector.
subroutine push_back_vector_real32(this, val)
  class(vector_real32), intent(inout) :: this
  real(real32), intent(in) :: val
  if (.not. allocated(this%arr_)) call this%init(1)
  if (this%size_ == this%capa_) then
     call this%resize(2*this%capa_)
  end if
  this%size_ = this%size_ + 1
  this%arr_(this%size_) = val
end subroutine push_back_vector_real32
!> push_back_vector_real32: Delete the value in the end of arr_(:) of the vector and return it.
real(real32) function pop_back_vector_real32(this, ierr)
  class(vector_real32), intent(inout) :: this
  integer(int32), intent(out), optional :: ierr
  if (this%size_ == 0) then
       if (present(ierr)) then
         ierr = 1
         return
       end if
     
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     
  end if
  pop_back_vector_real32 = this%arr_(this%size_)
  this%size_ = this%size_ - 1
  if (present(ierr)) ierr = 0
end function pop_back_vector_real32
!> size_vector_real32: Return current size of the vector.
pure integer(int32) function size_vector_real32(this)
  class(vector_real32), intent(in) :: this
  size_vector_real32 = this%size_
end function size_vector_real32
!> size_vector_real32: Return current lbound of the vector.
pure integer(int32) function lbound_vector_real32(this)
  class(vector_real32), intent(in) :: this
  lbound_vector_real32 = this%lb_
end function lbound_vector_real32
!> size_vector_real32: Return current ubonud of the vector.
pure integer(int32) function ubound_vector_real32(this)
  class(vector_real32), intent(in) :: this
  ubound_vector_real32 = this%lb_ + this%size_ - 1
end function ubound_vector_real32
!> resize_vector_real32: Shrink or expand arr_(:) of the vector.
subroutine resize_vector_real32(this, resize)
  class(vector_real32), intent(inout) :: this
  integer(int32), intent(in) :: resize
  real(real32), allocatable :: tmp(:)
  if (this%capa_ == resize) return
  allocate(tmp(resize))
  this%size_ = min(this%size_, resize)
  tmp(1:this%size_) = this%arr_(1:this%size_)
  call move_alloc(from = tmp, to = this%arr_)
  this%capa_ = resize
end subroutine resize_vector_real32

!> resize_range_vector_real32: Shrink or expand arr_(:) of the vector by lb and ub.
subroutine resize_vector_range_real32(this, lb, ub, ierr)
  class(vector_real32), intent(inout) :: this
  integer(int32), intent(in) :: lb, ub
  integer(int32), intent(out), optional :: ierr
  if (ub > lb) then
       if (present(ierr)) then
         ierr = 1
         return
       end if
     
       write(error_unit, '(a, i0, a)', advance = "no")&
            "Error in "//&
            __FILE__&
            //":", __LINE__, ":"
         write(error_unit, '(a)')&
           "resize_range_vector_range_real32:  must be larger than or equal tox "
         error stop 2
       
  end if
  call this%resize(ub-lb+1)
  this%lb_ = lb
  if (present(ierr)) ierr = 0
end subroutine resize_vector_range_real32
!> at_vector_real32: Return the element that locate at  of the vector.
real(real32) function at_vector_real32(this, i, ierr)
  class(vector_real32), intent(in) :: this
  integer(int32), intent(in) :: i
  integer(int32), intent(out), optional :: ierr
if (i < this%lbound() .or. i > this%ubound()) then
     if (present(ierr)) then
       ierr = i - this%lbound()
       return
     end if
   
#ifdef DEBUG
   write(error_unit, '(a, i0, a)', advance = "no")&
        "Error in "//&
        __FILE__&
        //":", __LINE__, ":"
   write(error_unit, '(a, *(i0, a))')&
        "Index ", i, " Out of bounds(", this%lbound(), ", ", this%ubound(), ")"
     error stop 1
   
#endif
end if
  at_vector_real32 = this%arr_(i - this%lb_ + 1)
  if (present(ierr)) ierr = 0
end function at_vector_real32
!> replace_vector_real32: Shrink or expand arr_(:) of the vector.
subroutine replace_vector_real32(this, i, val, ierr)
  class(vector_real32), intent(inout) :: this
  integer(int32), intent(in) :: i
  integer(int32), intent(out), optional :: ierr
  real(real32), intent(in) :: val
if (i < this%lbound() .or. i > this%ubound()) then
     if (present(ierr)) then
       ierr = i - this%lbound()
       return
     end if
   
#ifdef DEBUG
   write(error_unit, '(a, i0, a)', advance = "no")&
        "Error in "//&
        __FILE__&
        //":", __LINE__, ":"
   write(error_unit, '(a, *(i0, a))')&
        "Index ", i, " Out of bounds(", this%lbound(), ", ", this%ubound(), ")"
     error stop 1
   
#endif
end if
  this%arr_(i - this%lb_ + 1) = val
  if (present(ierr)) ierr = 0
end subroutine replace_vector_real32
!> lower_bound_vector_real32: Return the minimum index that is higher than or equal to .
integer(int32) function lower_bound_vector_real32(this, val)
  class(vector_real32), intent(in) :: this
  real(real32), intent(in) :: val
  integer(int32) :: p, q, r
  p = 1
  r = this%size_
  if (this%arr_(r) < val) then
     lower_bound_vector_real32 = r + 1 + (this%lb_ - 1)
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
  lower_bound_vector_real32 = q + (this%lb_ - 1)
end function lower_bound_vector_real32

!> init_vector_real64: Initialize the vector_real64 by size.
subroutine init_vector_real64(this, n)
  class(vector_real64), intent(inout) :: this
  integer(int32), intent(in) :: n
  if (.not. allocated(this%arr_)) then
     allocate(this%arr_(n))
     this%size_ = 0
     this%capa_ = n
     this%lb_   = 1
#ifdef DEBUG
  else
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
       write(error_unit, '(a)')&
         "Vector is already allocated..."
       error stop 1
     
#endif
  end if
end subroutine init_vector_real64

!> init_vector_range_real64: Initialize the vector_real64 by size.
subroutine init_vector_range_real64(this, lb, ub, ierr)
  class(vector_real64), intent(inout) :: this
  integer(int32), intent(in) :: lb, ub
  integer(int32), intent(out), optional :: ierr
  if (lb > ub) then
       if (present(ierr)) then
         ierr = 1
         return
       end if
     
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
       write(error_unit, '(a)')&
         "init_vector_range_real64:  must be larger than or equal to "
       error stop 2
     
  end if
  call this%init(ub-lb+1)
  this%lb_ = lb
  if (present(ierr)) ierr = 0
end subroutine init_vector_range_real64
!> push_back_vector_real64: Insert value to the tail of elements of the vector.
subroutine push_back_vector_real64(this, val)
  class(vector_real64), intent(inout) :: this
  real(real64), intent(in) :: val
  if (.not. allocated(this%arr_)) call this%init(1)
  if (this%size_ == this%capa_) then
     call this%resize(2*this%capa_)
  end if
  this%size_ = this%size_ + 1
  this%arr_(this%size_) = val
end subroutine push_back_vector_real64
!> push_back_vector_real64: Delete the value in the end of arr_(:) of the vector and return it.
real(real64) function pop_back_vector_real64(this, ierr)
  class(vector_real64), intent(inout) :: this
  integer(int32), intent(out), optional :: ierr
  if (this%size_ == 0) then
       if (present(ierr)) then
         ierr = 1
         return
       end if
     
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     
  end if
  pop_back_vector_real64 = this%arr_(this%size_)
  this%size_ = this%size_ - 1
  if (present(ierr)) ierr = 0
end function pop_back_vector_real64
!> size_vector_real64: Return current size of the vector.
pure integer(int32) function size_vector_real64(this)
  class(vector_real64), intent(in) :: this
  size_vector_real64 = this%size_
end function size_vector_real64
!> size_vector_real64: Return current lbound of the vector.
pure integer(int32) function lbound_vector_real64(this)
  class(vector_real64), intent(in) :: this
  lbound_vector_real64 = this%lb_
end function lbound_vector_real64
!> size_vector_real64: Return current ubonud of the vector.
pure integer(int32) function ubound_vector_real64(this)
  class(vector_real64), intent(in) :: this
  ubound_vector_real64 = this%lb_ + this%size_ - 1
end function ubound_vector_real64
!> resize_vector_real64: Shrink or expand arr_(:) of the vector.
subroutine resize_vector_real64(this, resize)
  class(vector_real64), intent(inout) :: this
  integer(int32), intent(in) :: resize
  real(real64), allocatable :: tmp(:)
  if (this%capa_ == resize) return
  allocate(tmp(resize))
  this%size_ = min(this%size_, resize)
  tmp(1:this%size_) = this%arr_(1:this%size_)
  call move_alloc(from = tmp, to = this%arr_)
  this%capa_ = resize
end subroutine resize_vector_real64

!> resize_range_vector_real64: Shrink or expand arr_(:) of the vector by lb and ub.
subroutine resize_vector_range_real64(this, lb, ub, ierr)
  class(vector_real64), intent(inout) :: this
  integer(int32), intent(in) :: lb, ub
  integer(int32), intent(out), optional :: ierr
  if (ub > lb) then
       if (present(ierr)) then
         ierr = 1
         return
       end if
     
       write(error_unit, '(a, i0, a)', advance = "no")&
            "Error in "//&
            __FILE__&
            //":", __LINE__, ":"
         write(error_unit, '(a)')&
           "resize_range_vector_range_real64:  must be larger than or equal tox "
         error stop 2
       
  end if
  call this%resize(ub-lb+1)
  this%lb_ = lb
  if (present(ierr)) ierr = 0
end subroutine resize_vector_range_real64
!> at_vector_real64: Return the element that locate at  of the vector.
real(real64) function at_vector_real64(this, i, ierr)
  class(vector_real64), intent(in) :: this
  integer(int32), intent(in) :: i
  integer(int32), intent(out), optional :: ierr
if (i < this%lbound() .or. i > this%ubound()) then
     if (present(ierr)) then
       ierr = i - this%lbound()
       return
     end if
   
#ifdef DEBUG
   write(error_unit, '(a, i0, a)', advance = "no")&
        "Error in "//&
        __FILE__&
        //":", __LINE__, ":"
   write(error_unit, '(a, *(i0, a))')&
        "Index ", i, " Out of bounds(", this%lbound(), ", ", this%ubound(), ")"
     error stop 1
   
#endif
end if
  at_vector_real64 = this%arr_(i - this%lb_ + 1)
  if (present(ierr)) ierr = 0
end function at_vector_real64
!> replace_vector_real64: Shrink or expand arr_(:) of the vector.
subroutine replace_vector_real64(this, i, val, ierr)
  class(vector_real64), intent(inout) :: this
  integer(int32), intent(in) :: i
  integer(int32), intent(out), optional :: ierr
  real(real64), intent(in) :: val
if (i < this%lbound() .or. i > this%ubound()) then
     if (present(ierr)) then
       ierr = i - this%lbound()
       return
     end if
   
#ifdef DEBUG
   write(error_unit, '(a, i0, a)', advance = "no")&
        "Error in "//&
        __FILE__&
        //":", __LINE__, ":"
   write(error_unit, '(a, *(i0, a))')&
        "Index ", i, " Out of bounds(", this%lbound(), ", ", this%ubound(), ")"
     error stop 1
   
#endif
end if
  this%arr_(i - this%lb_ + 1) = val
  if (present(ierr)) ierr = 0
end subroutine replace_vector_real64
!> lower_bound_vector_real64: Return the minimum index that is higher than or equal to .
integer(int32) function lower_bound_vector_real64(this, val)
  class(vector_real64), intent(in) :: this
  real(real64), intent(in) :: val
  integer(int32) :: p, q, r
  p = 1
  r = this%size_
  if (this%arr_(r) < val) then
     lower_bound_vector_real64 = r + 1 + (this%lb_ - 1)
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
  lower_bound_vector_real64 = q + (this%lb_ - 1)
end function lower_bound_vector_real64

!> init_vector_character: Initialize the vector_character by size.
subroutine init_vector_character(this, n)
  class(vector_character), intent(inout) :: this
  integer(int32), intent(in) :: n
  if (.not. allocated(this%arr_)) then
     allocate(this%arr_(n))
     this%size_ = 0
     this%capa_ = n
     this%lb_   = 1
#ifdef DEBUG
  else
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
       write(error_unit, '(a)')&
         "Vector is already allocated..."
       error stop 1
     
#endif
  end if
end subroutine init_vector_character

!> init_vector_range_character: Initialize the vector_character by size.
subroutine init_vector_range_character(this, lb, ub, ierr)
  class(vector_character), intent(inout) :: this
  integer(int32), intent(in) :: lb, ub
  integer(int32), intent(out), optional :: ierr
  if (lb > ub) then
       if (present(ierr)) then
         ierr = 1
         return
       end if
     
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
       write(error_unit, '(a)')&
         "init_vector_range_character:  must be larger than or equal to "
       error stop 2
     
  end if
  call this%init(ub-lb+1)
  this%lb_ = lb
  if (present(ierr)) ierr = 0
end subroutine init_vector_range_character
!> push_back_vector_character: Insert value to the tail of elements of the vector.
subroutine push_back_vector_character(this, val)
  class(vector_character), intent(inout) :: this
  character, intent(in) :: val
  if (.not. allocated(this%arr_)) call this%init(1)
  if (this%size_ == this%capa_) then
     call this%resize(2*this%capa_)
  end if
  this%size_ = this%size_ + 1
  this%arr_(this%size_) = val
end subroutine push_back_vector_character
!> push_back_vector_character: Delete the value in the end of arr_(:) of the vector and return it.
character function pop_back_vector_character(this, ierr)
  class(vector_character), intent(inout) :: this
  integer(int32), intent(out), optional :: ierr
  if (this%size_ == 0) then
       if (present(ierr)) then
         ierr = 1
         return
       end if
     
     write(error_unit, '(a, i0, a)', advance = "no")&
          "Error in "//&
          __FILE__&
          //":", __LINE__, ":"
     
  end if
  pop_back_vector_character = this%arr_(this%size_)
  this%size_ = this%size_ - 1
  if (present(ierr)) ierr = 0
end function pop_back_vector_character
!> size_vector_character: Return current size of the vector.
pure integer(int32) function size_vector_character(this)
  class(vector_character), intent(in) :: this
  size_vector_character = this%size_
end function size_vector_character
!> size_vector_character: Return current lbound of the vector.
pure integer(int32) function lbound_vector_character(this)
  class(vector_character), intent(in) :: this
  lbound_vector_character = this%lb_
end function lbound_vector_character
!> size_vector_character: Return current ubonud of the vector.
pure integer(int32) function ubound_vector_character(this)
  class(vector_character), intent(in) :: this
  ubound_vector_character = this%lb_ + this%size_ - 1
end function ubound_vector_character
!> resize_vector_character: Shrink or expand arr_(:) of the vector.
subroutine resize_vector_character(this, resize)
  class(vector_character), intent(inout) :: this
  integer(int32), intent(in) :: resize
  character, allocatable :: tmp(:)
  if (this%capa_ == resize) return
  allocate(tmp(resize))
  this%size_ = min(this%size_, resize)
  tmp(1:this%size_) = this%arr_(1:this%size_)
  call move_alloc(from = tmp, to = this%arr_)
  this%capa_ = resize
end subroutine resize_vector_character

!> resize_range_vector_character: Shrink or expand arr_(:) of the vector by lb and ub.
subroutine resize_vector_range_character(this, lb, ub, ierr)
  class(vector_character), intent(inout) :: this
  integer(int32), intent(in) :: lb, ub
  integer(int32), intent(out), optional :: ierr
  if (ub > lb) then
       if (present(ierr)) then
         ierr = 1
         return
       end if
     
       write(error_unit, '(a, i0, a)', advance = "no")&
            "Error in "//&
            __FILE__&
            //":", __LINE__, ":"
         write(error_unit, '(a)')&
           "resize_range_vector_range_character:  must be larger than or equal tox "
         error stop 2
       
  end if
  call this%resize(ub-lb+1)
  this%lb_ = lb
  if (present(ierr)) ierr = 0
end subroutine resize_vector_range_character
!> at_vector_character: Return the element that locate at  of the vector.
character function at_vector_character(this, i, ierr)
  class(vector_character), intent(in) :: this
  integer(int32), intent(in) :: i
  integer(int32), intent(out), optional :: ierr
if (i < this%lbound() .or. i > this%ubound()) then
     if (present(ierr)) then
       ierr = i - this%lbound()
       return
     end if
   
#ifdef DEBUG
   write(error_unit, '(a, i0, a)', advance = "no")&
        "Error in "//&
        __FILE__&
        //":", __LINE__, ":"
   write(error_unit, '(a, *(i0, a))')&
        "Index ", i, " Out of bounds(", this%lbound(), ", ", this%ubound(), ")"
     error stop 1
   
#endif
end if
  at_vector_character = this%arr_(i - this%lb_ + 1)
  if (present(ierr)) ierr = 0
end function at_vector_character
!> replace_vector_character: Shrink or expand arr_(:) of the vector.
subroutine replace_vector_character(this, i, val, ierr)
  class(vector_character), intent(inout) :: this
  integer(int32), intent(in) :: i
  integer(int32), intent(out), optional :: ierr
  character, intent(in) :: val
if (i < this%lbound() .or. i > this%ubound()) then
     if (present(ierr)) then
       ierr = i - this%lbound()
       return
     end if
   
#ifdef DEBUG
   write(error_unit, '(a, i0, a)', advance = "no")&
        "Error in "//&
        __FILE__&
        //":", __LINE__, ":"
   write(error_unit, '(a, *(i0, a))')&
        "Index ", i, " Out of bounds(", this%lbound(), ", ", this%ubound(), ")"
     error stop 1
   
#endif
end if
  this%arr_(i - this%lb_ + 1) = val
  if (present(ierr)) ierr = 0
end subroutine replace_vector_character
!> lower_bound_vector_character: Return the minimum index that is higher than or equal to .
integer(int32) function lower_bound_vector_character(this, val)
  class(vector_character), intent(in) :: this
  character, intent(in) :: val
  integer(int32) :: p, q, r
  p = 1
  r = this%size_
  if (this%arr_(r) < val) then
     lower_bound_vector_character = r + 1 + (this%lb_ - 1)
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
  lower_bound_vector_character = q + (this%lb_ - 1)
end function lower_bound_vector_character

end module vector_m
