subroutine assert_eq(a, b)
    use symengine
    type(Basic) :: a, b
    if (a /= b) then
        print *, a%str(), " /= ", b%str()
        error stop "Not equal"
    end if
end subroutine


subroutine dostuff()
    use symengine
    use iso_fortran_env, only: int64
    type(Basic) :: a, b, c, d

    a = SymInteger(12)
    b = Symbol('x')
    c = parse('x * 12')
    call assert_eq(a * b, c)
    call assert_eq(b * 12, c)
    call assert_eq(12 * b, c)
    call assert_eq(b * 12_int64, c)
    call assert_eq(12_int64 * b, c)

    c = parse('x + 12')
    call assert_eq(a + b, c)
    call assert_eq(b + 12, c)
    call assert_eq(12 + b, c)
    call assert_eq(b + 12_int64, c)
    call assert_eq(12_int64 + b, c)

    c = parse('12 - x')
    call assert_eq(a - b, c)
    call assert_eq(12 - b, c)
    call assert_eq(12_int64 - b, c)
    c = parse('x - 12')
    call assert_eq(b - 12, c)
    call assert_eq(b - 12_int64, c)

    c = parse('-x')
    call assert_eq(-b, c)

    c = parse('12 / x')
    call assert_eq(a / b, c)
    call assert_eq(12 / b, c)
    call assert_eq(12_int64 / b, c)
    c = parse('x / 12')
    call assert_eq(b / 12, c)
    call assert_eq(b / 12_int64, c)

    c = parse('12 ** x')
    call assert_eq(a ** b, c)
    call assert_eq(12 ** b, c)
    call assert_eq(12_int64 ** b, c)
    c = parse('x ** 12')
    call assert_eq(b ** 12, c)
    call assert_eq(b ** 12_int64, c)

    c = parse('sin(x)')
    call assert_eq(sin(b), c)

    c = parse('cos(x)')
    call assert_eq(cos(b), c)

    c = parse('sqrt(x)')
    call assert_eq(sqrt(b), c)

    a = Rational(1, 2)
    b = Rational(3, 4)
    c = Rational(3, 8)
    call assert_eq(a * b, c)

    a = RealDouble(1.0)
    b = RealDouble(2.0)
    c = RealDouble(3.0)
    call assert_eq(a + b, c)

    a = Symbol('x')
    b = parse('x * 2.0')
    call assert_eq(a * 2.0, b)
    call assert_eq(2.0 * a, b)
    call assert_eq(a * 2.0d0, b)
    call assert_eq(2.0d0 * a, b)

    a = Symbol('x')
    b = parse('x + 1.0')
    call assert_eq(a + 1.0, b)
    call assert_eq(1.0 + a, b)
    call assert_eq(a + 1.0d0, b)
    call assert_eq(1.0d0 + a, b)

    a = Symbol('x')
    b = parse('x - 1.0')
    call assert_eq(a - 1.0, b)
    call assert_eq(a - 1.0d0, b)
    b = parse('1.0 - x')
    call assert_eq(1.0 - a, b)
    call assert_eq(1.0d0 - a, b)

    a = Symbol('x')
    b = parse('x / 2.0')
    call assert_eq(a / 2.0, b)
    call assert_eq(a / 2.0d0, b)
    b = parse('2.0 / x')
    call assert_eq(2.0 / a, b)
    call assert_eq(2.0d0 / a, b)

    a = Symbol('x')
    b = parse('x ** 2.0')
    call assert_eq(a ** 2.0, b)
    call assert_eq(a ** 2.0d0, b)
    b = parse('2.0 ** x')
    call assert_eq(2.0 ** a, b)
    call assert_eq(2.0d0 ** a, b)

    a = RealDouble(1.0d0)
    b = RealDouble(2.0d0)
    c = RealDouble(3.0d0)
    call assert_eq(a + b, c)

    a = SymInteger(2)
    c = sqrt(a)
    c = c%evalf(53_8, 1)
    d = RealDouble(1.4142135623730951d0)
    call assert_eq(c, d)

    a = pi()
    b = parse("pi")
    call assert_eq(a, b)

    a = e()
    b = parse("e")
    call assert_eq(a, b)

    a = max([ptr(SymInteger(2)), ptr(SymInteger(3))])
    b = parse("3")
    call assert_eq(a, b)

    a = max([ptr(Symbol("x")), ptr(SymInteger(3))])
    b = parse("max(3, x)")
    call assert_eq(a, b)

    a = Symbol("x")
    b = SymInteger(2) * a
    c = b%subs(a, SymInteger(15))
    d = SymInteger(30)
    call assert_eq(c, d)
end subroutine



program test

    implicit none

    call dostuff

    print *, "Finishing"

end program
