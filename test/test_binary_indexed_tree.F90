program test_binary_indexed_tree
  use, intrinsic :: iso_fortran_env
  use binary_indexed_tree_m
  implicit none
  integer(int32), parameter :: n = 10
  integer(int32) :: i
  integer(int32) :: arr(n)
  type(binary_indexed_tree_int32) :: bit
  arr(:) = [(i, i = 1, n)]
  call bit%init(arr)
  do i = 1, n
     
  end do
end program test_binary_indexed_tree
