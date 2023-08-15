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

`svmax` means maximum order for svd orthogonal polynomial, `svgrd` means number of mesh points for svd orthogonal polynomial, `smesh` means mesh for svd orthogonal polynomial in [-1,1], `rep_s` saves svd orthogonal polynomial defined on [-1,1], `bose` determines whether the bosonic kernel is used, `beta` means inverse system temperature.

(2) Spheric Bessel function.

```fortran
subroutine s_sph_jl(lmax, x, jl)
```

It computes the spherical Bessel functions of the first kind, ``j_l(x)``, for argument ``x`` and ``l = 0, 1, \ldots, l_{max}``.

(3) Bernstein polynomial.

```fortran
subroutine s_bezier(n, x, bern)
```

`n` means the degree of the bernstein polynomials to be used. For any given ``n``, there is a set of ``n + 1`` bernstein polynomials, each of degree ``n``, which form a basis for polynomials on [0,1]. `x` means the evaluation point. `bern` saves the values of the ``n+1`` bernstein polynomials at ``x``.

(4) Some helper functions for `s\_svd\_basis()`.

```fortran
function s_safe_exp(x)
```

It is a safe exp call to avoid data overflow.

```fortran
function s_f_kernel(tau, omega, beta)
function s_b_kernel(tau, omega, beta)
```

They are used to calculate fermionic or bosonic kernel functions. `tau` means ``\tau``, `omega` means ``\omega``, `beta` means inverse system temperature ``\beta``.

## Theory

**Legendre orthogonal polynomial**

The legendre orthogonal polynomials obey the three term recurrence relation, known as Bonnet’s recursion formula:

```math
\begin{equation}
P_0(x) = 1
\end{equation}
```

```math
\begin{equation}
P_1(x) = x
\end{equation}
```

```math
\begin{equation}
(n+1) P_{n+1}(x) = (2n+1) P_n(x) - n P_{n-1}(x)
\end{equation}
```

**Chebyshev orthogonal polynomial**
**SVD orthogonal polynomial**
**Spheric Bessel function**
**Bernstein polynomial**
**Fermionic kernel**
**Bosonic kernel**
