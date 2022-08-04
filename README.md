# Matrix 

[![License](https://img.shields.io/github/license/RandomKiddo/matrix?logo=github)](https://www.gnu.org/licenses/gpl-3.0.en.html)
[![Version](https://img.shields.io/badge/fortran-f95-blueviolet)](https://en.wikipedia.org/wiki/Fortran_95_language_features)

Matrix is a Fortran 95 module created to handle matrix functions in mathematics 

___

### Instantiating A Matrix

```fortran
program example
    use class_Matrix
    implicit none 

    type(Matrix) :: mat
    mat = Matrix(5, 5) ! 5x5 Matrix
    set(mat, 3, 3, 1) ! Sets Matrix[3, 3] to 1
    call fprint(mat) ! Subroutine to print matrix
end program example
```

___

[Back to Top](#matrix)

<sub>This page was last edited on 08.03.2022</sub>