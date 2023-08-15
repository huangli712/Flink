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

The Legendre orthogonal polynomials obey the three term recurrence relation, known as Bonnet’s recursion formula:

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

The Chebyshev orthogonal polynomials of the second kind can be defined by the following recurrence relation:

```math
\begin{equation}
U_0(x) = 1
\end{equation}
```

```math
\begin{equation}
U_1(x) = 2x
\end{equation}
```

```math
\begin{equation}
U_{n+1}(x) = 2xU_n(x) - U_{n-1}(x)
\end{equation}
```

**SVD orthogonal polynomial**

**Spheric Bessel function**

The following recursion relation

```math
\begin{equation}
j_{l+1}(x)=\frac{2l+1}{x}j_l(x)-j_{l-1}(x)
\end{equation}
```

is used either downwards for ``x < l`` or upwards for ``x \ge l``. For ``x \ll 1``, the following asymtotic form is used:

```math
\begin{equation}
j_l(x) \approx \frac{x^l}{(2l+1)!!}.
\end{equation}
```

This procedure is numerically stable and accurate to near this machine precision for ``l \le 50``.

**Bernstein polynomial**

     ! the bernstein polynomials are assumed to be based on [0,1].
     ! the formula reads:
     !
     !    B(N,I)(X) = [N!/(I!*(N-I)!)] * (1-X)**(N-I) * X**I
     !
     ! first values:
     !
     !    B(0,0)(X) = 1
     !    B(1,0)(X) =      1-X
     !    B(1,1)(X) =                X
     !    B(2,0)(X) =     (1-X)**2
     !    B(2,1)(X) = 2 * (1-X)    * X
     !    B(2,2)(X) =                X**2
     !    B(3,0)(X) =     (1-X)**3
     !    B(3,1)(X) = 3 * (1-X)**2 * X
     !    B(3,2)(X) = 3 * (1-X)    * X**2
     !    B(3,3)(X) =                X**3
     !    B(4,0)(X) =     (1-X)**4
     !    B(4,1)(X) = 4 * (1-X)**3 * X
     !    B(4,2)(X) = 6 * (1-X)**2 * X**2
     !    B(4,3)(X) = 4 * (1-X)    * X**3
     !    B(4,4)(X) =                X**4
     !
     ! special values:
     !
     !    B(N,I)(X) has a unique maximum value at X = I/N.
     !
     !    B(N,I)(X) has an I-fold zero at 0 and and N-I fold zero at 1.
     !
     !    B(N,I)(1/2) = C(N,K) / 2^N
     !
     !    for a fixed X and N, the polynomials add up to 1:
     !        sum ( 0 <= I <= N ) B(N,I)(X) = 1
     !

**Fermionic kernel**
**Bosonic kernel**
