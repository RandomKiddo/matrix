module class_Matrix
    implicit none
    private
    public :: Matrix, get_rows, get_cols, init, set, is_identity, sum, rsum, csum, fprint
    public :: get_row, get_col, get, clone, is_in

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

    function clone(this) result(cloned)
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
end module class_Matrix