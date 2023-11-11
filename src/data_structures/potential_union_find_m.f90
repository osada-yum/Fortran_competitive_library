module potential_union_find_m
  use, intrinsic :: iso_fortran_env
  implicit none
  type potential_union_find
     integer(int32), allocatable :: par_(:), size_(:)
     integer(int64), allocatable :: pot_(:)
   contains
     procedure, pass :: init  => init_uf
     procedure, pass :: union => union_uf
     procedure, pass :: root  => root_uf
     procedure, pass :: same  => same_uf
     procedure, pass :: potential  => potential_uf
     procedure, pass :: diff  => diff_uf
     procedure, pass :: size  => size_uf
  end type potential_union_find
contains
  subroutine init_uf(this, n)
    class(potential_union_find), intent(inout) :: this
    integer(int32)   , intent(in)    :: n
    integer(int32)                   :: i
    allocate(this%par_(n), this%size_(n), this%pot_(n))
    this%par_(:)  = [(i, i = 1, n)]
    this%size_(:) = 0
    this%pot_(:) = 0_int64
  end subroutine init_uf
  impure subroutine union_uf(this, i, j, d)
    class(potential_union_find), intent(inout) :: this
    integer(int32)   , intent(in)    :: i, j
    integer(int64)   , intent(in)    :: d
    integer(int32) :: x, y
    integer(int64) :: d_tmp
    d_tmp = d
    d_tmp = d_tmp + this%potential(i)
    d_tmp = d_tmp - this%potential(j)
    x = this%root(i)
    y = this%root(j)
    if (x == y) return
    if (this%size_(x) < this%size_(y)) then
       call swap(x, y)
       d_tmp = - d_tmp
    end if
    this%par_(y) = x
    if (this%size_(x) == this%size_(y)) &
         this%size_(x) = this%size_(x) + 1
    this%pot_(y) = d_tmp
  contains
    pure subroutine swap(x, y)
      integer(int32), intent(inout) :: x, y
      integer(int32) :: tmp
      tmp = x
      x = y
      y = tmp
    end subroutine swap
  end subroutine union_uf
  impure recursive integer(int32) function root_uf(this, i) result(res)
    class(potential_union_find), intent(inout) :: this
    integer(int32)   , intent(in) :: i
    integer(int32) :: r
    if (this%par_(i) == i) then
       res = i
       return
    end if
    r = this%root(this%par_(i))
    this%pot_(i) = this%pot_(i) + this%pot_(this%par_(i))
    this%par_(i) = r
    res = r
  end function root_uf
  impure integer(int64) function potential_uf(this, i) result(res)
    class(potential_union_find), intent(inout) :: this
    integer(int32)   , intent(in) :: i
    integer(int32) :: dummy
    dummy = this%root(i)
    res = this%pot_(i)
  end function potential_uf
  impure integer(int64) function diff_uf(this, i, j) result(res)
    class(potential_union_find), intent(inout) :: this
    integer(int32)   , intent(in) :: i, j
    res = this%potential(j) - this%potential(i)
  end function diff_uf
  impure logical function same_uf(this, i, j)
    class(potential_union_find), intent(inout) :: this
    integer(int32)   , intent(in) :: i, j
    same_uf = this%root(i) == this%root(j)
  end function same_uf
  impure integer(int32) function size_uf(this, i) result(res)
    class(potential_union_find), intent(inout) :: this
    integer(int32), intent(in) :: i
    integer(int32) :: root
    root = this%root(i)
    res = this%size_(root)
  end function size_uf
end module potential_union_find_m
