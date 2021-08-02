## Introduction

These subroutines are used to perform spline interpolation.

## Type

subroutines

## Source

`src/s_spline.f90`

## Usage

(1) Calculate 1-order derivates for a given function.

```fortran
subroutine s_spl_deriv1(...)
```

(2) Calculate 2-order derivates for a given function.

```fortran
subroutine s_spl_deriv2(...)
```

(3) Evaluate function value at a given point.

```fortran
function   s_spl_funct(...)
```
