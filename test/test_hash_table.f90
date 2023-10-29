! [[file:../src/data_structures/hash_table.org::hash_table-test][hash_table-test]]
program test_hash_table
  use, intrinsic :: iso_fortran_env
  use hash_table_m
  implicit none
  integer(int32) :: v, i, j, k, ierr
  logical :: found
  character(len=:), allocatable :: s
  type(hash_table_int32) :: ht_i32, ht_i32_2
  ht_i32 = hash_table_int32()
  !> check empty character.
  call ht_i32%insert("", 0, ierr=ierr)
  v = ht_i32%search("", found=found)
  if (.not. found) then
     write(error_unit, *) "Empty string '' not found or not inserted..."
     error stop 2
  end if
  if (v /= 0) then
     write(error_unit, *) "Value of arr[''] must be 0"
     error stop 3
  end if
  !> check size
  !> insert 701 elements
  !> first, insert 10*10*7 elements
  do i = ichar("a"), ichar("a")+10-1
     do j = ichar("A"), ichar("A")+10-1
        do k = ichar(" "), ichar(" ")+7-1
           s = achar(i)//achar(j)//achar(k)
           call ht_i32%insert(s, 128**2*i+128*j+k, ierr)
        end do
     end do
  end do
  call ht_i32%insert("abcde", 0, ierr) ! size of hash table is maximum
  if (ierr == 0) then
     write(error_unit, *) "Insert in fully hash table must fail...", size(ht_i32)
     error stop 4
  end if
  call ht_i32%delete("aB$", found) ! delete elements in hash table.
  if (.not. found) then
     write(error_unit, *) "Delete failed...", size(ht_i32)
     error stop 5
  end if
  call ht_i32%insert("abcdef", 0, ierr) ! be able to insert
  if (ierr /= 0) then
     write(error_unit, *) "Delete or insert failed..."
     error stop 6
  end if
  !> insert 700 elements
  !> delete 700 elements
  !> first, insert 10*10*7 elements
  ht_i32_2 = hash_table_int32()
  do i = ichar("a"), ichar("a")+10-1
     do j = ichar("A"), ichar("A")+10-1
        do k = ichar(" "), ichar(" ")+7-1
           s = achar(i)//achar(j)//achar(k)
           call ht_i32_2%insert(s, 0, ierr)
           call ht_i32_2%delete(s, found=found)
           if (ierr /= 0) then
              write(error_unit, *) "Insert failed...", size(ht_i32_2)
              error stop 7
           end if
           if (.not. found) then
              write(error_unit, *) "Insert and Delete failed...", size(ht_i32_2)
              error stop 8
           end if
        end do
     end do
  end do
  if (size(ht_i32_2) /= 0) then
     write(error_unit, *) "Insert and delete failed...", size(ht_i32_2)
     error stop 9
  end if
  !> insert 700 elements
  !> delete 700 elements
  !> insert 700 elements
  !> first, insert 10*10*7 elements
  ht_i32_2 = hash_table_int32()
  do i = ichar("a"), ichar("a")+10-1
     do j = ichar("A"), ichar("A")+10-1
        do k = ichar(" "), ichar(" ")+7-1
           s = achar(i)//achar(j)//achar(k)
           call ht_i32_2%insert(s, 0)
           call ht_i32_2%delete(s, found=found)
           call ht_i32_2%insert(s, 0, ierr)
           if (ierr /= 0) then
              write(error_unit, *) "Insert failed...", size(ht_i32_2)
              error stop 10
           end if
           if (.not. found) then
              write(error_unit, *) "Delete failed...", size(ht_i32_2)
              error stop 11
           end if
        end do
     end do
  end do
  if (size(ht_i32_2) /= 700) then
     write(error_unit, *) "Insert and delete and insert failed...", size(ht_i32_2)
     error stop 12
  end if
end program test_hash_table
! hash_table-test ends here
