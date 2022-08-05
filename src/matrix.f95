module class_Matrix
    implicit none
    private
    public :: Matrix, get_rows, get_cols, init, set, is_identity, sum, rsum, csum, fprint
    public :: get_row, get_col, get, copy, is_in, multiply, inner, det, is_square, inverse
    public :: is_empty, equals, purge, nullify, average

    type Matrix
        real :: r, c
        real, allocatable :: vals(:,:)
    end type Matrix

    interface Matrix 
        procedure :: init
    end interface Matrix

    contains 

    type(Matrix) function init(rows, cols)
        implicit none 

        real, intent(in) :: rows, cols

        init%r = rows
        init%c = cols
        allocate(init%vals(rows, cols))

        real :: i, j

        do i = 1, this%r, 1
            do j = 1, this%c, 1
                init%vals(i, j) = 0
            end do
        end do
    end function init

    function get_rows(this) result(rows)
        implicit none

        type(Matrix), intent(in) :: this
        real :: rows

        rows = this%r

        return
    end function get_rows

    function get_cols(this) result(cols)
        implicit none

        type(Matrix), intent(in) :: this
        real :: cols

        cols = this%c

        return
    end function get_cols

    function set(this, i, j, val) result(success)
        implicit none 

        type(Matrix), intent(in) :: this
        real :: i, j
        real :: val
        logical :: success

        this%vals(i, j) = val
        
        if this%vals(i, j) == val then
            success = .true.
        else
            success = .false.
        end if

        return
    end function set

    function is_identity(this) result(identity)
        implicit none

        type(Matrix), intent(in) :: this
        logical :: identity
        real :: i, j

        if (this%r /= this%c) then
            identity = .false.
            return
        end if

        do i = 1, this%r, 1
            do j = 1, this%c, 1
                if (this%vals(i, j) /= 1) then
                    identity = .false.
                    return
                end if
            end do
        end do

        do i = 1, this%r, 1
            do j = 1, this%c, 1
                if (i /= j) then
                    if (this%vals(i, j) /= 0) then 
                        identity = .false.
                        return
                    end if
                end if
            end do
        end do

        identity = .true.

        return
    end function is_identity 

    function sum(this) result(s)
        implicit none

        type(Matrix), intent(in) :: this
        real :: s, i, j

        s = 0

        do i = 1, this%r, 1
            do j = 1, this%c, 1
                s = s + this%vals(i, j)
            end do
        end do

        return
    end function sum

    function rsum(this, row) result(s)
        implicit none

        type(Matrix), intent(in) :: this
        real :: s, i, j, row

        s = 0
        i = row

        do j = 1, this%c, 1
            s = s + this%vals(i, j)
        end do

        return
    end function rsum

    function csum(this, col) result(s)
        implicit none

        type(Matrix), intent(in) :: this
        real :: s, i, j, col

        s = 0
        j = col

        do i = 1, this%r, 1
            s = s + this%vals(i, j)
        end do

        return
    end function csum

    subroutine fprint(this)
        implicit none 

        type(Matrix), intent(in) :: this
        real :: i

        do i = 1, this%r, 1
            print *, this%vals(i)
        end do
    end subroutine fprint

    function get_row(this, row) result(arr)
        implicit none

        type(Matrix), intent(in) :: this
        real :: row, j
        real, dimension(this%c) :: arr

        do j = 1, this%c, 1
            arr(j) = this%vals(row, j)
        end do

        return
    end function get_row

    function get_col(this, col) result(arr)
        implicit none

        type(Matrix), intent(in) :: this
        real :: col, i
        real, dimension(this%r) :: arr

        do i = 1, this%r, 1
            arr(i) = this%vals(i, col)
        end do

        return
    end function get_col

    function get(this, i, j) result(val)
        implicit none

        type(Matrix), intent(in) :: this
        real :: i, j, val

        val = this%vals(i, j)

        return
    end function get

    function copy(this) result(cloned)
        implicit none

        type(Matrix), intent(in) :: this
        type(Matrix) :: cloned
        real :: i, j

        cloned = Matrix(this%r, this%c)

        do i = 1, this%r, 1
            do j = 1, this%c, 1
                cloned%vals(i, j) = this%vals(i, j)
            end do
        end do

        return
    end function clone

    function is_in(this, val) result(located)
        implicit none

        type(Matrix), intent(in) :: this
        real :: val, i, j
        logical :: located

        do i = 1, this%r, 1
            do j = 1, this%c, 1
                if (this%vals(i, j) == val) then 
                    located = .true.
                    return
                end if
            end do
        end do

        located = .false.

        return
    end function is_in

    function multiply(this, b) result(m)
        implicit none

        type(Matrix), intent(in) :: this
        type(Matrix) :: b, m
        real :: i, j, k

        if (this%c /= b%r) then
            return
        end if

        m = Matrix(this%r, b%c)

        do i = 1, m%r, 1
            do j = 1, m%c, 1
                m%vals(i, j) = 0
                do k = 1, b%r, 1
                    m%vals(i, j) = m%vals(i, j) + (this%vals(i, k) * b%vals(k, j))
                end do
            end do
        end do

        return
    end function multiply

    function inner(this, r1, c1, r2, c2) result(m)
        implicit none

        type(Matrix), intent(in) :: this
        real :: r1, c1, r2, c2, i, j
        type(Matrix) :: m

        m = Matrix(r2 - r1, c2 - c1)

        do i = r1, r2, 1
            do j = c1, c2, 1
                m%vals(i - r1 + 1, j - c1 + 1) = this%vals(i, j)
            end do
        end do

        return
    end function inner

    function det(this) result(val)
        implicit none 

        type(Matrix), intent(in) :: this
        real :: val

        if (this%r /= this%c .or. (this%r /= 2 .and. this%r /= 3))) then
            return
        end if

        if this%r == 2 then
            val = this%vals(1, 1) * this%vals(2, 2) - (this%vals(1, 2) * this%vals(2, 1))
        else
            real :: i, j, k

            i = this%vals(1, 1) * ((this%vals(2, 2) * this%vals(3, 3)) - (this%vals(2, 3) * this%vals(3, 2)))
            j = this%vals(1, 2) * ((this%vals(2, 1) * this%vals(3, 3)) - (this%vals(2, 3) * this%vals(3, 1)))
            k = this%vals(1, 3) * ((this%vals(2, 1) * this%vals(3, 2)) - (this%vals(2, 2) * this%vals(3, 1)))
            val = i - j + k
        end if

        return
    end function det

    function is_square(this) result(l)
        implicit none

        type(Matrix), intent(in) :: this
        logical :: l

        if this%r == this%c then
            l = .true.
        else 
            l = .false.
        end if

        return
    end function is_square

    function inverse(this) result(inv)
        implicit none

        type(Matrix), intent(in) :: this
        type(Matrix) :: inv

        if (this%r /= this%c .or. this%r /= 2) then
            return
        end if

        inv = Matrix(2, 2)

        real :: val
        val = 1.0 / det(this)

        inv%vals(1, 1) = this%vals(2, 2) * val
        inv%vals(1, 2) = (-1) * this%vals(1, 2) * val
        inv%vals(2, 1) = (-1) * this%vals(2, 1) * val
        inv%vals(2, 2) = this%vals(1, 1) * val

        return
    end function inverse 

    function is_empty(this) result(l)
        implicit none

        type(Matrix), intent(in) :: this
        logical :: l
        real :: i, j

        do i = 1, this%r, 1
            do j = 1, this%c, 1
                if (this%vals(i, j) /= 0) then
                    l = .false.
                    return
                end if
            end do
        end do

        l = .true. 

        return
    end function is_empty

    function equals(this, other) result(eq)
        implicit none 

        type(Matrix), intent(in) :: this
        type(Matrix) :: other
        logical :: eq
        real :: i, j

        if (this%r /= other%r .or. this%c /= other%c) then
            eq = .false.
            return
        end if

        do i = 1, this%r, 1
            do j = 1, this%c, 1
                if (this%vals(i, j) /= other%vals(i, j)) then
                    eq = .false.
                    return
                end if
            end do
        end do

        eq = .true.

        return
    end function equals

    subroutine nullify(this)
        implicit none

        type(Matrix), intent(in) :: this
        real :: i, j

        do i = 1, this%r, 1
            do j = 1, this%c, 1
                this%vals(i, j) = 0
            end do
        end do
    end subroutine nullify

    subroutine purge(this)
        implicit none

        type(Matrix), intent(in) :: this

        deallocate(this%vals)
    end subroutine purge

    function average(this) result(avg)
        implicit none

        type(Matrix), intent(in) :: this
        real :: avg, i, j, n

        n = this%r * this%c
        avg = 0

        do i = 1, this%r, 1
            do j = 1, this%c, 1
                avg = avg + this%vals(i, j)
            end do
        end do

        avg = avg / n

        return
    end function average
end module class_Matrix