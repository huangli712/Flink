## Introduction

It is a common module which defines some common used numerical or physical constants. We always need it.

As for how many constants are defined, please consult the source code (`src/m_constants.f90`).

## Usage

1. import module completely

```fortran
program test
    use constants
 
    implicit none

    real(dp) :: A
    A = one
end program
```

2. import module partially

```fortran
program test
    use constants, only : dp
    use constants, only : one
 
    implicit none

    real(dp) :: A
    A = one
end program
```
