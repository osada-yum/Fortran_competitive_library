module ref_string_merge_sort_m
  use, intrinsic :: iso_fortran_env
  use ref_string_m
  implicit none
  private
  public :: merge_sort, merge_sort_descending
  interface merge_sort
     module procedure :: merge_sort_ref_string
     module procedure :: merge_sort_with_key_ref_string
  end interface merge_sort
  interface merge_sort_descending
     module procedure :: merge_sort_ref_string_descending
     module procedure :: merge_sort_with_key_ref_string_descending
  end interface merge_sort_descending
  
contains
  !> merge_sort_ref_string: Sort arr(:) by sub function merge_sort_sub_ref_string.
  !> arguments:
  !> arr: array of some type.
  subroutine merge_sort_ref_string(arr)
    type(ref_string), intent(inout) :: arr(:)
    call merge_sort_sub_ref_string(arr, 1, size(arr))
  end subroutine merge_sort_ref_string
  !> merge: Algorithm for merge_sort, check if Left or Right is end in each loop.
  !> arguments:
  !> arr: array of some type, (out) arr(p:r) is sorted.
  !> p, q, r: integer, indices p is start, r is end, q = floor( (p+q)/2 ).
  !> variables:
  !> Left, Right: array of typeof(arr), sorted
  !> l_max, r_max: integer, max index of Left or Right.
  subroutine merge_ref_string(arr, p, q, r)
    type(ref_string), intent(inout) :: arr(:)
    integer(int32), intent(in) :: p, q, r
    type(ref_string)                :: Left(1:q-p+1), Right(1:r-q)
    integer(int32)             :: l_max, r_max
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
  end subroutine merge_ref_string
  !> merge_sort_sub: Recursive function used by merge_sort.
  !> arguments:
  !> arr: array of some type.
  !> p, r: integer, p is start of arr, r is end of arr.
  !> variables:
  !> q: integer, q = floor( (p+q)/2 )
  recursive subroutine merge_sort_sub_ref_string(arr, p, r)
    type(ref_string), intent(inout) :: arr(:)
    integer(int32), intent(in) :: p, r
    integer(int32)             :: q
    if (p < r) then
       q = (p+r)/2
       call merge_sort_sub_ref_string(arr, p, q)
       call merge_sort_sub_ref_string(arr, q+1, r)
       call merge_ref_string(arr, p, q, r)
    end if
  end subroutine merge_sort_sub_ref_string
  !> merge_sort_with_key_ref_string: Sort key(:) sub function merge_sort_sub_with_key_ref_string.
  !> arguments:
  !> key: array of some type.
  !> indices: array of some type.
  subroutine merge_sort_with_key_ref_string(key, indices)
    type(ref_string), intent(inout) :: key(:)
    integer(int32), intent(inout) :: indices(:)
    call merge_sort_sub_with_key_ref_string(key, indices, 1, size(key))
  end subroutine merge_sort_with_key_ref_string
  !> merge_with_key: Algorithm for merge_sort, check if Left or Right is end in each loop.
  !> arguments:
  !> indices: array of indices.
  !> key: array of some type, (out) key(p:r) is sorted.
  !> p, q, r: integer, indices p is start, r is end, q = floor( (p+q)/2 ).
  !> variables:
  !> Left, Right: array of typeof(indices), sorted
  !> l_max, r_max: integer, max index of Left or Right.
  subroutine merge_with_key_ref_string(key, indices, p, q, r)
    type(ref_string), intent(inout) :: key(:)
    integer(int32), intent(inout) :: indices(:)
    integer(int32), intent(in) :: p, q, r
    integer(int32) :: Left(1:q-p+1), Right(1:r-q)
    type(ref_string) :: Left_key(1:q-p+1), Right_key(1:r-q)
    integer(int32) :: l_max, r_max
    l_max = q-p+1
    r_max = r-q
    block
      !> i, j, k: integer, loop counters.
      integer(int32) :: i, j, k
      Left(1:l_max)  = indices(p:q)
      Right(1:r_max) = indices(q+1:r)
      Left_key(1:l_max)  = key(p:q)
      Right_key(1:r_max) = key(q+1:r)
      i = 1
      j = 1
      do k = p, r
         if (Left_key(i) <= Right_key(j)) then
            key(k) = Left_key(i)
            indices(k) = Left(i)
            i = i + 1
            if (i > l_max) then
               key(k+1:r) = Right_key(j:)
               indices(k+1:r) = Right(j:)
               return
            end if
         else
            key(k) = Right_key(j)
            indices(k) = Right(j)
            j = j + 1
            if (j > r_max) then
               key(k+1:r) = Left_key(i:)
               indices(k+1:r) = Left(i:)
               return
            end if
         end if
      end do
    end block
  end subroutine merge_with_key_ref_string
  !> merge_sort_sub_with_key: Recursive function used by merge_sort_with_key.
  !> arguments:
  !> indices: array of indices.
  !> key: array of some type.
  !> p, r: integer, p is start of arr, r is end of arr.
  !> variables:
  !> q: integer, q = floor( (p+q)/2 )
  recursive subroutine merge_sort_sub_with_key_ref_string(key, indices, p, r)
    type(ref_string), intent(inout) :: key(:)
    integer(int32), intent(inout) :: indices(:)
    integer(int32), intent(in) :: p, r
    integer(int32)             :: q
    if (p < r) then
       q = (p+r)/2
       call merge_sort_sub_with_key_ref_string(key, indices, p, q)
       call merge_sort_sub_with_key_ref_string(key, indices, q+1, r)
       call merge_with_key_ref_string(key, indices, p, q, r)
    end if
  end subroutine merge_sort_sub_with_key_ref_string
  !> merge_sort_ref_string_descending: Sort arr(:) by sub function merge_sort_sub_ref_string_descending.
  !> arguments:
  !> arr: array of some type.
  subroutine merge_sort_ref_string_descending(arr)
    type(ref_string), intent(inout) :: arr(:)
    call merge_sort_sub_ref_string_descending(arr, 1, size(arr))
  end subroutine merge_sort_ref_string_descending
  !> merge: Algorithm for merge_sort, check if Left or Right is end in each loop.
  !> arguments:
  !> arr: array of some type, (out) arr(p:r) is sorted.
  !> p, q, r: integer, indices p is start, r is end, q = floor( (p+q)/2 ).
  !> variables:
  !> Left, Right: array of typeof(arr), sorted
  !> l_max, r_max: integer, max index of Left or Right.
  subroutine merge_ref_string_descending(arr, p, q, r)
    type(ref_string), intent(inout) :: arr(:)
    integer(int32), intent(in) :: p, q, r
    type(ref_string)                :: Left(1:q-p+1), Right(1:r-q)
    integer(int32)             :: l_max, r_max
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
         if (Left(i) >= Right(j)) then
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
  end subroutine merge_ref_string_descending
  !> merge_sort_sub: Recursive function used by merge_sort.
  !> arguments:
  !> arr: array of some type.
  !> p, r: integer, p is start of arr, r is end of arr.
  !> variables:
  !> q: integer, q = floor( (p+q)/2 )
  recursive subroutine merge_sort_sub_ref_string_descending(arr, p, r)
    type(ref_string), intent(inout) :: arr(:)
    integer(int32), intent(in) :: p, r
    integer(int32)             :: q
    if (p < r) then
       q = (p+r)/2
       call merge_sort_sub_ref_string_descending(arr, p, q)
       call merge_sort_sub_ref_string_descending(arr, q+1, r)
       call merge_ref_string_descending(arr, p, q, r)
    end if
  end subroutine merge_sort_sub_ref_string_descending
  !> merge_sort_with_key_ref_string_descending: Sort key(:) sub function merge_sort_sub_with_key_ref_string_descending.
  !> arguments:
  !> key: array of some type.
  !> indices: array of some type.
  subroutine merge_sort_with_key_ref_string_descending(key, indices)
    type(ref_string), intent(inout) :: key(:)
    integer(int32), intent(inout) :: indices(:)
    call merge_sort_sub_with_key_ref_string_descending(key, indices, 1, size(key))
  end subroutine merge_sort_with_key_ref_string_descending
  !> merge_with_key: Algorithm for merge_sort, check if Left or Right is end in each loop.
  !> arguments:
  !> indices: array of indices.
  !> key: array of some type, (out) key(p:r) is sorted.
  !> p, q, r: integer, indices p is start, r is end, q = floor( (p+q)/2 ).
  !> variables:
  !> Left, Right: array of typeof(indices), sorted
  !> l_max, r_max: integer, max index of Left or Right.
  subroutine merge_with_key_ref_string_descending(key, indices, p, q, r)
    type(ref_string), intent(inout) :: key(:)
    integer(int32), intent(inout) :: indices(:)
    integer(int32), intent(in) :: p, q, r
    integer(int32) :: Left(1:q-p+1), Right(1:r-q)
    type(ref_string) :: Left_key(1:q-p+1), Right_key(1:r-q)
    integer(int32) :: l_max, r_max
    l_max = q-p+1
    r_max = r-q
    block
      !> i, j, k: integer, loop counters.
      integer(int32) :: i, j, k
      Left(1:l_max)  = indices(p:q)
      Right(1:r_max) = indices(q+1:r)
      Left_key(1:l_max)  = key(p:q)
      Right_key(1:r_max) = key(q+1:r)
      i = 1
      j = 1
      do k = p, r
         if (Left_key(i) >= Right_key(j)) then
            key(k) = Left_key(i)
            indices(k) = Left(i)
            i = i + 1
            if (i > l_max) then
               key(k+1:r) = Right_key(j:)
               indices(k+1:r) = Right(j:)
               return
            end if
         else
            key(k) = Right_key(j)
            indices(k) = Right(j)
            j = j + 1
            if (j > r_max) then
               key(k+1:r) = Left_key(i:)
               indices(k+1:r) = Left(i:)
               return
            end if
         end if
      end do
    end block
  end subroutine merge_with_key_ref_string_descending
  !> merge_sort_sub_with_key: Recursive function used by merge_sort_with_key.
  !> arguments:
  !> indices: array of indices.
  !> key: array of some type.
  !> p, r: integer, p is start of arr, r is end of arr.
  !> variables:
  !> q: integer, q = floor( (p+q)/2 )
  recursive subroutine merge_sort_sub_with_key_ref_string_descending(key, indices, p, r)
    type(ref_string), intent(inout) :: key(:)
    integer(int32), intent(inout) :: indices(:)
    integer(int32), intent(in) :: p, r
    integer(int32)             :: q
    if (p < r) then
       q = (p+r)/2
       call merge_sort_sub_with_key_ref_string_descending(key, indices, p, q)
       call merge_sort_sub_with_key_ref_string_descending(key, indices, q+1, r)
       call merge_with_key_ref_string_descending(key, indices, p, q, r)
    end if
  end subroutine merge_sort_sub_with_key_ref_string_descending
  
end module ref_string_merge_sort_m
