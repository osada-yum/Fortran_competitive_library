program test_vector
  use, intrinsic :: iso_fortran_env
  use vector_m
  implicit none
  integer(int32) :: i, j
  integer(int32) :: ierr
  integer(int32), parameter :: n = 10
  type(vector_int32) :: v, v2
  call v2%init(2, 12)
  do i = 1, n
     call v%push_back(i)
     call v2%push_back(i)
  end do
  do i = 1, n
     if (v%at(i) /= i) then
        write(error_unit, '(a, i0, a)', advance = "no") &
             __FILE__&
             //": ", __LINE__, ": "
          error stop 20
        
     end if
     call v%replace(i, -i)
     if (v%at(i) /= -i) then
        write(error_unit, '(a, i0, a)', advance = "no") &
             __FILE__&
             //": ", __LINE__, ": "
          error stop 21
        
     end if
  end do
  j = v%at(n+1, ierr)
  if (ierr == 0) then
     write(error_unit, *) ierr
     write(error_unit, '(a, i0, a)', advance = "no") &
          __FILE__&
          //": ", __LINE__, ": "
       write(error_unit, '(a)')&
         "Return value of `at` is illegal."
       error stop 23
     
  end if
  do i = 1, n
     j = v%pop_back()
  end do
  j = v%pop_back(ierr)
  if (ierr == 0) then
     write(error_unit, '(a, i0, a)', advance = "no") &
          __FILE__&
          //": ", __LINE__, ": "
       write(error_unit, '(a)')&
         "Return value of `pop_back` is illegal."
       error stop 24
     
  end if
end program test_vector
