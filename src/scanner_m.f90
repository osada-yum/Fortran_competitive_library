module scanner_m
  use, intrinsic :: iso_fortran_env
  implicit none
  private
  public scanner
  integer(int32), parameter, public :: max_bufsize = ishft(1_int32, 20) ! ~ 10**6
  !> type(scanner): For reading token like Scanner class in Java.
  !> The default delimiter is space (" ").
  !> If delimiter is space (" "), regard `iostat_eor` as space.
  type :: scanner
     private
     integer(int32) :: pos_ = 0, unit_ = input_unit
     character :: delim_ = " "
     logical :: is_eof_ = .false.
     character(len=max_bufsize) :: buffer_
   contains
     !> setting function
     procedure, pass :: use_delim => use_delim_scanner
     !> read function
     procedure, pass, private :: read_until_delim => read_until_delim_scanner
     !> next functions
     procedure, pass :: next => next_scanner
     generic :: next_int => next_int8, &
          & next_int16, &
          & next_int32, &
          & next_int64
     procedure, pass :: next_int8 => next_int8_scanner
     procedure, pass :: next_int16 => next_int16_scanner
     procedure, pass :: next_int32 => next_int32_scanner
     procedure, pass :: next_int64 => next_int64_scanner
     generic :: next_real => next_real32, &
          & next_real64
     procedure, pass :: next_real32 => next_real32_scanner
     procedure, pass :: next_real64 => next_real64_scanner
     !> state functions
     procedure, pass :: is_eof => is_eof_scanner
  end type scanner
  interface scanner
     module procedure construct_scanner
  end interface scanner
contains
  !> construct_scanner: Constructor for `type(scanner)`.
  !> @param unit An unit of input device.
  !> @return A object of scanner type.
  pure type(scanner) function construct_scanner(unit) result(res)
    integer(int32), intent(in) :: unit
    if (unit == output_unit) &
         & error stop "Error: Scanner does not use `output_unit`."
    if (unit == error_unit) &
         & error stop "Error: Scanner does not use `error_unit`."
    res%unit_ = unit
  end function construct_scanner
  !> use_delim_scanner: Modify delimiter.
  !> @param this `this%delim_` changes.
  !> @param delimiter A character of delimiter.
  pure subroutine use_delim_scanner(this, delimiter)
    class(scanner), intent(inout) :: this
    character, intent(in) :: delimiter
    this%delim_ = delimiter
  end subroutine use_delim_scanner
  !> read_until_delim_scanner: Read from `this%unit_` until reaching `this%delim_`.
  !> @param this `this%buffer_(1:this%pos_)` represents token splited by `this%delim_`.
  impure subroutine read_until_delim_scanner(this)
    class(scanner), intent(inout) :: this
    character :: c
    integer(int32) :: iostat
    if (this%pos_ /= 0) return
    read_until_delim: do
       read (this%unit_, '(a1)', advance="no", iostat=iostat) c
       select case (iostat)
       case (iostat_eor)
          if (this%delim_ == " ") exit
          this%pos_ = this%pos_ + 1
          this%buffer_(this%pos_:this%pos_) = achar(10) !> newline code.
          cycle
       case (iostat_end)
          this%is_eof_ = .true.
          exit
       case (0)
          !> do nothing.
       case default
          write (error_unit, *) iostat
          error stop "Error: Unknown iostat"
       end select
       if (c == this%delim_) then
          if (this%pos_ /= 0) exit
          cycle !> The contiguous `this%delim_`.
       end if
       ! write(error_unit, *) c, this%pos_, iostat, iostat_eor, iostat_end
       this%pos_ = this%pos_ + 1
       this%buffer_(this%pos_:this%pos_) = c
    end do read_until_delim
  end subroutine read_until_delim_scanner
  !> next_scanner: Return token as `character(len=:)` through `res`.
  !> @param this this%pos_ reset 0.
  !> @param res A character of token splited by `this%delim_`.
  impure subroutine next_scanner(this, res)
    class(scanner), intent(inout) :: this
    character(len=:), intent(inout), allocatable :: res
    character(len=:), allocatable :: tmp
    call this%read_until_delim()
    allocate (tmp, source=this%buffer_(1:this%pos_))
    call move_alloc(from=tmp, to=res)
    this%pos_ = 0
  end subroutine next_scanner
  !> next_int8_scanner: Return token as `integer(int8)` through `res`.
  !> @param this this%pos_ reset 0.
  !> @param res An `integer(int8)` of token splited by `this%delim_`.
  impure subroutine next_int8_scanner(this, res)
    class(scanner), intent(inout) :: this
    integer(int8), intent(inout) :: res
    call this%read_until_delim()
    read (this%buffer_(1:this%pos_), *) res
    this%pos_ = 0
  end subroutine next_int8_scanner
  !> next_int16_scanner: Return token as `integer(int16)` through `res`.
  !> @param this this%pos_ reset 0.
  !> @param res An `integer(int16)` of token splited by `this%delim_`.
  impure subroutine next_int16_scanner(this, res)
    class(scanner), intent(inout) :: this
    integer(int16), intent(inout) :: res
    call this%read_until_delim()
    read (this%buffer_(1:this%pos_), *) res
    this%pos_ = 0
  end subroutine next_int16_scanner
  !> next_int32_scanner: Return token as `integer(int32)` through `res`.
  !> @param this this%pos_ reset 0.
  !> @param res An `integer(int32)` of token splited by `this%delim_`.
  impure subroutine next_int32_scanner(this, res)
    class(scanner), intent(inout) :: this
    integer(int32), intent(inout) :: res
    call this%read_until_delim()
    read (this%buffer_(1:this%pos_), *) res
    this%pos_ = 0
  end subroutine next_int32_scanner
  !> next_int64_scanner: Return token as `integer(int64)` through `res`.
  !> @param this this%pos_ reset 0.
  !> @param res An `integer(int64)` of token splited by `this%delim_`.
  impure subroutine next_int64_scanner(this, res)
    class(scanner), intent(inout) :: this
    integer(int64), intent(inout) :: res
    call this%read_until_delim()
    read (this%buffer_(1:this%pos_), *) res
    this%pos_ = 0
  end subroutine next_int64_scanner
  !> next_real32_scanner: Return token as `real(real32)` through `res`.
  !> @param this this%pos_ reset 0.
  !> @param res An `integer(real32)` of token splited by `this%delim_`.
  impure subroutine next_real32_scanner(this, res)
    class(scanner), intent(inout) :: this
    real(real32), intent(inout) :: res
    call this%read_until_delim()
    read (this%buffer_(1:this%pos_), *) res
    this%pos_ = 0
  end subroutine next_real32_scanner
  !> next_real64_scanner: Return token as `real(real64)` through `res`.
  !> @param this this%pos_ reset 0.
  !> @param res An `integer(real64)` of token splited by `this%delim_`.
  impure subroutine next_real64_scanner(this, res)
    class(scanner), intent(inout) :: this
    real(real64), intent(inout) :: res
    call this%read_until_delim()
    read (this%buffer_(1:this%pos_), *) res
    this%pos_ = 0
  end subroutine next_real64_scanner
  !> is_eof_scanner: Return is reaching EOF of input file or not.
  !> @param this Read one token.
  !> @return A logical value if reaching EOF or not.
  impure logical function is_eof_scanner(this) result(res)
    class(scanner), intent(inout) :: this
    call this%read_until_delim()
    res = this%is_eof_
  end function is_eof_scanner
end module scanner_m
