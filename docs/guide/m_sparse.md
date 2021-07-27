!!
!!
!! Introduction
!! ============
!!
!! In this module, we implement some basic sparse matrix algebra. Now it
!! supports double precision real and complex numbers.
!!
!! Usage
!! =====
!!
!! 1. import sparse support
!! ------------------------
!!
!! use sparse
!!
!! 2. convert normal matrix to sparse matrix
!! -----------------------------------------
!!
!! call sp_csr_to_dns(...)
!!
!!
!! 3. convert sparse matrix to normal matrix
!! -----------------------------------------
!!
!! call sp_dns_to_csr(...)
!!
!! 4. perform sparse matrix - vector multiplication
!! ------------------------------------------------
!!
!! call sp_csr_mv_vec(...)
!!
!! 5. perform sparse matrix - matrix multiplication
!! ------------------------------------------------
!!
!! call sp_csr_mm_csr(...)
!!
!! Specifically, if one of the matrix is diagonal matrix, then you can use
!!
!! call sp_dia_mm_csr(...)
!!
!! or
!!
!! call sp_csr_mm_dia(...)
!!
!!
