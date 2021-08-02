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
