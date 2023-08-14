## Introduction

In this module, two tetrahedron integration algorithms are implemented. They are:

* P. E. Blochl tetrahedron integration algorithm,
* Lambin-Vigneron tetrahedron integration algorithm.

Note that the former is used to calculate the DFT density of states, while the later is used to calculate the lattice Green's functions during the DFT + DMFT calculations. Besides, we also implement three smearing algorithm, which can be used to calculate the DFT density of states as well. They are:

* Gaussian smearing algorithm,
* Fermi-Dirac smearing algorithm,
* Marzari-Vanderbilt cold smearing algorithm.

This module depends on the `s_util.f90/s_qsorter()` subroutine.

## Type

module

## Source

`src/m_tetra.f90`

## Usage

(1) P. E. Blochl tetrahedron integration algorithm.

```fortran
call tetra_blochl_weight1(...)
```

or

```fortran
call tetra_blochl_weight2(...)
```

(2) Lambin-Vigneron tetrahedron integration algorithm.

```fortran
call tetra_lambin_weight(...)
```

(3) Gaussian smearing algorithm.

```fortran
call smearing_gauss_weight1(...)
```

or

```fortran
call smearing_gauss_weight2(...)
```

or

```fortran
call smearing_gauss_weight3(...)
```

(4) Fermi-Dirac smearing algorithm.

```fortran
call smearing_fermi_weight1(...)
```

or

```fortran
call smearing_fermi_weight2(...)
```

or

```fortran
call smearing_fermi_weight3(...)
```

(5) Marzari-Vanderbilt cold smearing algorithm.

```fortran
call smearing_marzari_weight1(...)
```

or

```fortran
call smearing_marzari_weight2(...)
```

or

```fortran
call smearing_marzari_weight3(...)
```

## Theory

### Analytical Tetrahedron Integration (Blochl's Algorithm)

**Number of states**

Here we list the expressions for the integrated density of states or number of states function ``n(\epsilon)`` from a given tetrahedron. The number of states function is used to determine the Fermi level. The expressions shown are similar to those given in previous papers. For the sake of simplicity we have omitted all band indices. The one-particle energies at the corners of the tetrahedron are ``\epsilon_1``, ``\epsilon_2``, ``\epsilon_3``, ``\epsilon_4``, which are ordered according to increasing values. ``\epsilon_{ij}`` is a shorthand notation for ``\epsilon_i - \epsilon_j``. ``V_T`` is the reciprocal-space volume of the tetrahedron, and ``V_G`` is the volume of the reciprocal unit cell.

```math
\begin{equation}
n(\epsilon) = 0
\end{equation}
```

for ``\epsilon < \epsilon_1``,

```math
\begin{equation}
n(\epsilon) = \frac{V_T}{V_G} \frac{(\epsilon - \epsilon_1)^3}{\epsilon_{21}\epsilon_{31}\epsilon_{41}}
\end{equation}
```

for ``\epsilon_1 < \epsilon < \epsilon_2``,

```math
\begin{equation}
n(\epsilon) = \frac{V_T}{V_G} 
\frac{1}{\epsilon_{31}\epsilon_{41}}
\left[
\epsilon^2_{21} +
3\epsilon_{21}(\epsilon-\epsilon_2) +
3(\epsilon - \epsilon_2)^2 -
\frac{\epsilon_{31} + \epsilon_{42}}{\epsilon_{32}\epsilon_{42}}(\epsilon - \epsilon_2)^3
\right]
\end{equation}
```

for ``\epsilon_2 < \epsilon < \epsilon_3``,

```math
\begin{equation}
n(\epsilon) = \frac{V_T}{V_G} 
\left(
1 - \frac{(\epsilon_4 - \epsilon)^3}{\epsilon_{41}\epsilon_{42}\epsilon_{43}}
\right)
\end{equation}
```

for ``\epsilon_3 < \epsilon < \epsilon_4``, and

```math
\begin{equation}
n(\epsilon) = \frac{V_T}{V_G}
\end{equation}
```

for ``\epsilon > \epsilon_4``.

**Integration weights**

Here we give the expressions that results in the integration weights ``w_{nj}``. By ``w_1``, ``w_2``, ``w_3``, and ``w_4`` we denote the contribution to the integration weights at the four corners of a tetrahedron, which are again ordered according to increasing one-particle energies. Note that the band index ``n`` is suppressed.

For a fully tetrahedron, i.e., ``\epsilon_F < \epsilon_1``, the contributions vanish:

```math
\begin{equation}
w_1 = w_2 = w_3 = w_4 = 0.
\end{equation}
```

### Smearning algorithm
