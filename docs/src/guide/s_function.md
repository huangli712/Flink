## Introduction

Provide some subroutines to calculate the special functions.

## Type

subroutines

## Source

`src/s_function.f90`

## Usage

(1) Orthogonal polynomial basis.

```fortran
subroutine s_leg_basis(lemax, legrd, lmesh, rep_l)
subroutine s_che_basis(chmax, chgrd, cmesh, rep_c)
subroutine s_svd_basis(svmax, svgrd, smesh, rep_s, bose, beta)
```

`lemax` means maximum order for legendre orthogonal polynomial, `legrd` means number of mesh points for legendre orthogonal polynomial, `lmesh` means mesh for legendre orthogonal polynomial in [-1,1], `rep_l` saves legendre orthogonal polynomial defined on [-1,1].

`chmax` means maximum order for chebyshev orthogonal polynomial, `chgrd` means number of mesh points for chebyshev orthogonal polynomial, `cmesh` means mesh for chebyshev orthogonal polynomial in [-1,1], `rep_c` saves chebyshev orthogonal polynomial defined on [-1,1].

(2) Spheric Bessel function.

```fortran
subroutine s_sph_jl(lmax, x, jl)
```

(3) Bernstein polynomial.

```fortran
subroutine s_bezier(n, x, bern)
```

(4) Some helper functions for s\_svd\_basis().

```fortran
function s_safe_exp(x)
function s_f_kernel(tau, omega, beta)
function s_b_kernel(tau, omega, beta)
```

## Theory
