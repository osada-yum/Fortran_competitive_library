module merge_sort_m
  use, intrinsic :: iso_fortran_env
  implicit none
  private
  public :: merge_sort
  interface merge_sentinel
     module procedure :: merge_sentinel_int32, merge_sentinel_int64
     module procedure :: merge_sentinel_real32, merge_sentinel_real64
  end interface merge_sentinel
  interface merge
     module procedure :: merge_int32, merge_int64
     module procedure :: merge_real32, merge_real64
  end interface merge
  interface merge_sort
     module procedure :: merge_sort_int32, merge_sort_int64
     module procedure :: merge_sort_real32, merge_sort_real64
  end interface merge_sort
  interface merge_sort_sub
     module procedure :: merge_sort_sub_int32, merge_sort_sub_int64
     module procedure :: merge_sort_sub_real32, merge_sort_sub_real64
  end interface merge_sort_sub
contains

  subroutine merge_sentinel_int32(arr, p, q, r)
    integer(int32), intent(inout) :: arr(:)
    integer(int32)                :: Left(1:q-p+2), Right(1:r-q+1)
    !> merge_sentinel: Algorithm for merge_sort, set sentinel in end of Left and, Right.
    !> arguments:
    !> arr: array of some type, (out) arr(p:r) is sorted.
    !> p, q, r: integer, indices p is start, r is end, q = floor( (p+q)/2 ).
    !> variables:
    !> Left, Right: array of typeof(arr), sorted
    !> i, j, k: integer, loop counter.
    integer(int32), intent(in) :: p, q, r
    integer(int32)             :: i, j, k
    Left(1:q-p+1) = arr(p:q)
    Right(1:r-q)  = arr(q+1:r)
    Left(q-p+2)   = huge(arr(p))
    Right(r-q+1)  = huge(arr(p))
    i = 1
    j = 1
    do k = p, r
       if (Left(i) <= Right(j)) then
          arr(k) = Left(i)
          i = i + 1
       else
          arr(k) = Right(j)
          j = j + 1
       end if
    end do
  end subroutine merge_sentinel_int32
  subroutine merge_sentinel_int64(arr, p, q, r)
    integer(int64), intent(inout) :: arr(:)
    integer(int64)                :: Left(1:q-p+2), Right(1:r-q+1)
    !> merge_sentinel: Algorithm for merge_sort, set sentinel in end of Left and, Right.
    !> arguments:
    !> arr: array of some type, (out) arr(p:r) is sorted.
    !> p, q, r: integer, indices p is start, r is end, q = floor( (p+q)/2 ).
    !> variables:
    !> Left, Right: array of typeof(arr), sorted
    !> i, j, k: integer, loop counter.
    integer(int32), intent(in) :: p, q, r
    integer(int32)             :: i, j, k
    Left(1:q-p+1) = arr(p:q)
    Right(1:r-q)  = arr(q+1:r)
    Left(q-p+2)   = huge(arr(p))
    Right(r-q+1)  = huge(arr(p))
    i = 1
    j = 1
    do k = p, r
       if (Left(i) <= Right(j)) then
          arr(k) = Left(i)
          i = i + 1
       else
          arr(k) = Right(j)
          j = j + 1
       end if
    end do
  end subroutine merge_sentinel_int64
  subroutine merge_sentinel_real32(arr, p, q, r)
    real(real32), intent(inout) :: arr(:)
    real(real32)                :: Left(1:q-p+2), Right(1:r-q+1)
    !> merge_sentinel: Algorithm for merge_sort, set sentinel in end of Left and, Right.
    !> arguments:
    !> arr: array of some type, (out) arr(p:r) is sorted.
    !> p, q, r: integer, indices p is start, r is end, q = floor( (p+q)/2 ).
    !> variables:
    !> Left, Right: array of typeof(arr), sorted
    !> i, j, k: integer, loop counter.
    integer(int32), intent(in) :: p, q, r
    integer(int32)             :: i, j, k
    Left(1:q-p+1) = arr(p:q)
    Right(1:r-q)  = arr(q+1:r)
    Left(q-p+2)   = huge(arr(p))
    Right(r-q+1)  = huge(arr(p))
    i = 1
    j = 1
    do k = p, r
       if (Left(i) <= Right(j)) then
          arr(k) = Left(i)
          i = i + 1
       else
          arr(k) = Right(j)
          j = j + 1
       end if
    end do
  end subroutine merge_sentinel_real32
  subroutine merge_sentinel_real64(arr, p, q, r)
    real(real64), intent(inout) :: arr(:)
    real(real64)                :: Left(1:q-p+2), Right(1:r-q+1)
    !> merge_sentinel: Algorithm for merge_sort, set sentinel in end of Left and, Right.
    !> arguments:
    !> arr: array of some type, (out) arr(p:r) is sorted.
    !> p, q, r: integer, indices p is start, r is end, q = floor( (p+q)/2 ).
    !> variables:
    !> Left, Right: array of typeof(arr), sorted
    !> i, j, k: integer, loop counter.
    integer(int32), intent(in) :: p, q, r
    integer(int32)             :: i, j, k
    Left(1:q-p+1) = arr(p:q)
    Right(1:r-q)  = arr(q+1:r)
    Left(q-p+2)   = huge(arr(p))
    Right(r-q+1)  = huge(arr(p))
    i = 1
    j = 1
    do k = p, r
       if (Left(i) <= Right(j)) then
          arr(k) = Left(i)
          i = i + 1
       else
          arr(k) = Right(j)
          j = j + 1
       end if
    end do
  end subroutine merge_sentinel_real64

  subroutine merge_int32(arr, p, q, r)
    integer(int32), intent(inout) :: arr(:)
    integer(int32)                :: Left(1:q-p+1), Right(1:r-q)
    integer(int32), intent(in) :: p, q, r
    integer(int32)             :: l_max, r_max
    !> merge: Algorithm for merge_sort, check if Left or Right is end in each loop.
    !> arguments:
    !> arr: array of some type, (out) arr(p:r) is sorted.
    !> p, q, r: integer, indices p is start, r is end, q = floor( (p+q)/2 ).
    !> variables:
    !> Left, Right: array of typeof(arr), sorted
    !> l_max, r_max: integer, max index of Left or Right.
    l_max = q-p+1
    r_max = r-q
    block
      !> i, j, k: integer, loop counters.
      integer(int32) :: i, j, k
      Left(1:l_max)  = arr(p:q)
      Right(1:r_max) = arr(q+1:r)
      i = 1
      j = 1
      do k = p, r
         if (Left(i) <= Right(j)) then
            arr(k) = Left(i)
            i = i + 1
            if (i > l_max) then
               arr(k+1:r) = Right(j:)
               return
            end if
         else
            arr(k) = Right(j)
            j = j + 1
            if (j > r_max) then
               arr(k+1:r) = Left(i:)
               return
            end if
         end if
      end do
    end block
  end subroutine merge_int32
  subroutine merge_int64(arr, p, q, r)
    integer(int64), intent(inout) :: arr(:)
    integer(int64)                :: Left(1:q-p+1), Right(1:r-q)
    integer(int32), intent(in) :: p, q, r
    integer(int32)             :: l_max, r_max
    !> merge: Algorithm for merge_sort, check if Left or Right is end in each loop.
    !> arguments:
    !> arr: array of some type, (out) arr(p:r) is sorted.
    !> p, q, r: integer, indices p is start, r is end, q = floor( (p+q)/2 ).
    !> variables:
    !> Left, Right: array of typeof(arr), sorted
    !> l_max, r_max: integer, max index of Left or Right.
    l_max = q-p+1
    r_max = r-q
    block
      !> i, j, k: integer, loop counters.
      integer(int32) :: i, j, k
      Left(1:l_max)  = arr(p:q)
      Right(1:r_max) = arr(q+1:r)
      i = 1
      j = 1
      do k = p, r
         if (Left(i) <= Right(j)) then
            arr(k) = Left(i)
            i = i + 1
            if (i > l_max) then
               arr(k+1:r) = Right(j:)
               return
            end if
         else
            arr(k) = Right(j)
            j = j + 1
            if (j > r_max) then
               arr(k+1:r) = Left(i:)
               return
            end if
         end if
      end do
    end block
  end subroutine merge_int64
  subroutine merge_real32(arr, p, q, r)
    real(real32), intent(inout) :: arr(:)
    real(real32)                :: Left(1:q-p+1), Right(1:r-q)
    integer(int32), intent(in) :: p, q, r
    integer(int32)             :: l_max, r_max
    !> merge: Algorithm for merge_sort, check if Left or Right is end in each loop.
    !> arguments:
    !> arr: array of some type, (out) arr(p:r) is sorted.
    !> p, q, r: integer, indices p is start, r is end, q = floor( (p+q)/2 ).
    !> variables:
    !> Left, Right: array of typeof(arr), sorted
    !> l_max, r_max: integer, max index of Left or Right.
    l_max = q-p+1
    r_max = r-q
    block
      !> i, j, k: integer, loop counters.
      integer(int32) :: i, j, k
      Left(1:l_max)  = arr(p:q)
      Right(1:r_max) = arr(q+1:r)
      i = 1
      j = 1
      do k = p, r
         if (Left(i) <= Right(j)) then
            arr(k) = Left(i)
            i = i + 1
            if (i > l_max) then
               arr(k+1:r) = Right(j:)
               return
            end if
         else
            arr(k) = Right(j)
            j = j + 1
            if (j > r_max) then
               arr(k+1:r) = Left(i:)
               return
            end if
         end if
      end do
    end block
  end subroutine merge_real32
  subroutine merge_real64(arr, p, q, r)
    real(real64), intent(inout) :: arr(:)
    real(real64)                :: Left(1:q-p+1), Right(1:r-q)
    integer(int32), intent(in) :: p, q, r
    integer(int32)             :: l_max, r_max
    !> merge: Algorithm for merge_sort, check if Left or Right is end in each loop.
    !> arguments:
    !> arr: array of some type, (out) arr(p:r) is sorted.
    !> p, q, r: integer, indices p is start, r is end, q = floor( (p+q)/2 ).
    !> variables:
    !> Left, Right: array of typeof(arr), sorted
    !> l_max, r_max: integer, max index of Left or Right.
    l_max = q-p+1
    r_max = r-q
    block
      !> i, j, k: integer, loop counters.
      integer(int32) :: i, j, k
      Left(1:l_max)  = arr(p:q)
      Right(1:r_max) = arr(q+1:r)
      i = 1
      j = 1
      do k = p, r
         if (Left(i) <= Right(j)) then
            arr(k) = Left(i)
            i = i + 1
            if (i > l_max) then
               arr(k+1:r) = Right(j:)
               return
            end if
         else
            arr(k) = Right(j)
            j = j + 1
            if (j > r_max) then
               arr(k+1:r) = Left(i:)
               return
            end if
         end if
      end do
    end block
  end subroutine merge_real64

  recursive subroutine merge_sort_sub_int32(arr, p, r)
    integer(int32), intent(inout) :: arr(:)
    integer(int32), intent(in) :: p, r
    integer(int32)             :: q
    !> merge_sort_sub: Recursive function used by merge_sort.
    !> arguments:
    !> arr: array of some type.
    !> p, r: integer, p is start of arr, r is end of arr.
    !> variables:
    !> q: integer, q = floor( (p+q)/2 )
    if (p < r) then
       q = int((r+p)/2, int32)
       call merge_sort_sub(arr, p, q)
       call merge_sort_sub(arr, q+1, r)
       call merge(arr, p, q, r)
    end if
  end subroutine merge_sort_sub_int32
  recursive subroutine merge_sort_sub_int64(arr, p, r)
    integer(int64), intent(inout) :: arr(:)
    integer(int32), intent(in) :: p, r
    integer(int32)             :: q
    !> merge_sort_sub: Recursive function used by merge_sort.
    !> arguments:
    !> arr: array of some type.
    !> p, r: integer, p is start of arr, r is end of arr.
    !> variables:
    !> q: integer, q = floor( (p+q)/2 )
    if (p < r) then
       q = int((r+p)/2, int32)
       call merge_sort_sub(arr, p, q)
       call merge_sort_sub(arr, q+1, r)
       call merge(arr, p, q, r)
    end if
  end subroutine merge_sort_sub_int64
  recursive subroutine merge_sort_sub_real32(arr, p, r)
    real(real32), intent(inout) :: arr(:)
    integer(int32), intent(in) :: p, r
    integer(int32)             :: q
    !> merge_sort_sub: Recursive function used by merge_sort.
    !> arguments:
    !> arr: array of some type.
    !> p, r: integer, p is start of arr, r is end of arr.
    !> variables:
    !> q: integer, q = floor( (p+q)/2 )
    if (p < r) then
       q = int((r+p)/2, int32)
       call merge_sort_sub(arr, p, q)
       call merge_sort_sub(arr, q+1, r)
       call merge(arr, p, q, r)
    end if
  end subroutine merge_sort_sub_real32
  recursive subroutine merge_sort_sub_real64(arr, p, r)
    real(real64), intent(inout) :: arr(:)
    integer(int32), intent(in) :: p, r
    integer(int32)             :: q
    !> merge_sort_sub: Recursive function used by merge_sort.
    !> arguments:
    !> arr: array of some type.
    !> p, r: integer, p is start of arr, r is end of arr.
    !> variables:
    !> q: integer, q = floor( (p+q)/2 )
    if (p < r) then
       q = int((r+p)/2, int32)
       call merge_sort_sub(arr, p, q)
       call merge_sort_sub(arr, q+1, r)
       call merge(arr, p, q, r)
    end if
  end subroutine merge_sort_sub_real64

  subroutine merge_sort_int32(arr)
    integer(int32), intent(inout) :: arr(:)
    !> merge_sort: Sort arr(:) by sub function merge_sort_sub.
    !> arguments:
    !> arr: array of some type.
    call merge_sort_sub(arr, 1, size(arr))
  end subroutine merge_sort_int32
  subroutine merge_sort_int64(arr)
    integer(int64), intent(inout) :: arr(:)
    !> merge_sort: Sort arr(:) by sub function merge_sort_sub.
    !> arguments:
    !> arr: array of some type.
    call merge_sort_sub(arr, 1, size(arr))
  end subroutine merge_sort_int64
  subroutine merge_sort_real32(arr)
    real(real32), intent(inout) :: arr(:)
    !> merge_sort: Sort arr(:) by sub function merge_sort_sub.
    !> arguments:
    !> arr: array of some type.
    call merge_sort_sub(arr, 1, size(arr))
  end subroutine merge_sort_real32
  subroutine merge_sort_real64(arr)
    real(real64), intent(inout) :: arr(:)
    !> merge_sort: Sort arr(:) by sub function merge_sort_sub.
    !> arguments:
    !> arr: array of some type.
    call merge_sort_sub(arr, 1, size(arr))
  end subroutine merge_sort_real64

end module merge_sort_m
