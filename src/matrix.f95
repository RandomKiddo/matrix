module class_Matrix
    implicit none
    private
    public :: Matrix, get_rows, get_cols, init, set, is_identity, sum, rsum, csum

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
end module class_Matrix