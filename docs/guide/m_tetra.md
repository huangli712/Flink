!!
!!
!! Introduction
!! ============
!!
!! In this module, two tetrahedron integration algorithms are implemented.
!! They are:
!!     (1) P. E. Blochl tetrahedron integration algorithm,
!!     (2) Lambin-Vigneron tetrahedron integration algorithm.
!! Note that the former is used to calculate the DFT density of states,
!! while the later is used to calculate the lattice Green's functions
!! during the DFT + DMFT calculations.
!!
!! Besides, we also implement three smearing algorithm, which can be used
!! to calculate the DFT density of states. They are:
!!     (3) Gaussian smearing algorithm,
!!     (4) Fermi-Dirac smearing algorithm,
!!     (5) Marzari-Vanderbilt cold smearing algorithm.
!!
!! This module depends on s_util.f90/s_qsorter() subroutine.
!!
!! Usage
!! =====
!!
!! 1. P. E. Blochl tetrahedron integration algorithm
!! -------------------------------------------------
!!
!! call tetra_blochl_weight1(...)
!!
!! or
!!
!! call tetra_blochl_weight2(...)
!!
!! 2. Lambin-Vigneron tetrahedron integration algorithm
!! ----------------------------------------------------
!!
!! call tetra_lambin_weight(...)
!!
!! 3. Gaussian smearing algorithm
!! ------------------------------
!!
!! call smearing_gauss_weight1(...)
!!
!! or
!!
!! call smearing_gauss_weight2(...)
!!
!! or
!!
!! call smearing_gauss_weight3(...)
!!
!! 4. Fermi-Dirac smearing algorithm
!! ---------------------------------
!!
!! call smearing_fermi_weight1(...)
!!
!! or
!!
!! call smearing_fermi_weight2(...)
!!
!! or
!!
!! call smearing_fermi_weight3(...)
!!
!! 5. Marzari-Vanderbilt cold smearing algorithm
!! ---------------------------------------------
!!
!! call smearing_marzari_weight1(...)
!!
!! or
!!
!! call smearing_marzari_weight2(...)
!!
!! or
!!
!! call smearing_marzari_weight3(...)
!!
!!
