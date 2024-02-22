program test

    implicit none

    call dostuff

    print *, "Finishing"

contains

subroutine assert_eq(a, b)
    use symengine
    type(Basic) :: a, b
    if (a /= b) then
        print *, a%str(), " /= ", b%str()
        error stop "Not equal"
    end if
end subroutine

subroutine assert_matrix_eq(a, b)
    use symengine
    type(DenseMatrix) :: a, b
    if (.not. (a == b)) then
        print *, a%str(), " /= ", b%str()
        error stop "Not equal"
    end if
end subroutine

subroutine assert_str_eq(a, b)
    use symengine
    character(len=*), intent(in) :: a, b
    if (b /= a(1:len(a)-1)) then
        print *, a, " /= ", b
        error stop "Not equal"
    end if
end subroutine

subroutine assert(a)
    logical :: a
    if (.not. a) then
        error stop "Assertion failed"
    end if
end subroutine

subroutine dostuff()
    use symengine
    use iso_fortran_env, only: int64
    implicit none
    type(Basic) :: a, b, c, d
    type(DenseMatrix) :: M, N
    type(SetBasic) :: set

    a = SymInteger(12)
    b = Symbol('x')
    c = parse('x * 12')

    c = (a * b) * a
!    call assert_eq(a * b, c)
!    call assert_eq(b * 12, c)
!    call assert_eq(12 * b, c)
!    call assert_eq(b * 12_int64, c)
!    call assert_eq(12_int64 * b, c)
!
!    c = parse('x + 12')
!    call assert_eq(a + b, c)
!    call assert_eq(b + 12, c)
!    call assert_eq(12 + b, c)
!    call assert_eq(b + 12_int64, c)
!    call assert_eq(12_int64 + b, c)
end subroutine

end program
