module algorithms
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, algorithms!"
  end subroutine say_hello
end module algorithms
