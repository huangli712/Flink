## Introduction

It is a common module which defines some common used numerical or physical constants. They are as follows:

* for numerical precision:
    * `sp`
    * `dp`
* for file units
    * `mystd`
    * `myout`
    * `mytmp`
* for numerical constants
    * `pi`
    * `zero`
    * `one`
    * `two`
    * `half`
    * `eps6`
    * `eps8`
    * `epst`
    * `epss`
    * `czi`
    * `cone`
    * `czero`
* for physical constants
    * `ev2k`
    * `ry2e`
    * `ha2e` 

 We always need to import this module.

## Type

module

## Source

`src/m_constants.f90`

## Usage

(1) Import constants module completely.

```fortran
program test
    use constants

    implicit none

    real(dp) :: A
    A = one
end program
```

(2) Import constants module partially.

```fortran
program test
    use constants, only : dp
    use constants, only : one

    implicit none

    real(dp) :: A
    A = one
end program
```
