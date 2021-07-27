!!
!!
!! Introduction
!! ============
!!
!! 1. build constants (0) matrix
!! -----------------------------
!!
!! subroutine s_zeros_i(...)
!! subroutine s_zeros_d(...)
!! subroutine s_zeros_z(...)
!!
!! 2. build constants (1) matrix
!! -----------------------------
!!
!! subroutine s_ones_i(...)
!! subroutine s_ones_d(...)
!! subroutine s_ones_z(...)
!!
!! 3. build constants (any values) matrix
!! --------------------------------------
!!
!! subroutine s_any_i(...)
!! subroutine s_any_d(...)
!! subroutine s_any_z(...)
!!
!! 4. build diagonal matrix
!! ------------------------
!!
!! subroutine s_eye_i(...)
!! subroutine s_eye_d(...)
!! subroutine s_eye_z(...)
!!
!! 5. build identity matrix
!! ------------------------
!!
!! subroutine s_identity_i(...)
!! subroutine s_identity_d(...)
!! subroutine s_identity_z(...)
!!
!! 6. build diagonal matrix from vector
!! ------------------------------------
!!
!! subroutine s_diag_i(...)
!! subroutine s_diag_d(...)
!! subroutine s_diag_z(...)
!!
!! 7. calculate trace for matrix
!! -----------------------------
!!
!! subroutine s_trace_d(...)
!! subroutine s_trace_z(...)
!!
!! 8. calculate determinant for matrix
!! -----------------------------------
!!
!! subroutine s_det_d(...)
!! subroutine s_det_z(...)
!!
!! 9. calculate matrix inversion
!! -----------------------------
!!
!! subroutine s_inv_d(...)
!! subroutine s_inv_z(...)
!!
!! 10. general eigensystem problem
!! -------------------------------
!!
!! subroutine s_eig_dg(...)
!! subroutine s_eig_zg(...)
!! subroutine s_eigvals_dg(...)
!! subroutine s_eigvals_zg(...)
!!
!! 11. symmetric eigensystem problem
!! ---------------------------------
!!
!! subroutine s_eig_sy(...)
!! subroutine s_eig_he(...)
!! subroutine s_eigvals_sy(...)
!! subroutine s_eigvals_he(...)
!!
!! 12. linear equation solver
!! --------------------------
!!
!! subroutine s_solve_dg(...)
!! subroutine s_solve_zg(...)
!! subroutine s_solve_sy(...)
!! subroutine s_solve_he(...)
!!
!! 13. general singular value decomposition
!! ----------------------------------------
!!
!! subroutine s_svd_dg(...)
!! subroutine s_svd_zg(...)
!!
!! note: _i means integer version, _d real(dp) version, and _z complex(dp)
!! version. _dg means real(dp) general version, _zg complex(dp) general
!! version, _sy real(dp) symmetric version, _he complex(dp) Hermitian version.
!!
!!
