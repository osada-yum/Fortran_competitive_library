program test_linked_list
  use, intrinsic :: iso_fortran_env
  use linked_list_m
  implicit none
  integer(int32) :: i
  type(linked_list_int32_head) :: lst_i32
  do i = 1, 10
     call lst_i32%add(i)
  end do
  print*, lst_i32%search(3)
  print*, lst_i32%search(-1)
end program test_linked_list
