!!!-----------------------------------------------------------------------
!!! project : flink @ sakura
!!! program : s_zeros_i
!!!           s_zeros_d
!!!           s_zeros_z
!!!           s_ones_i
!!!           s_ones_d
!!!           s_ones_z
!!!           s_any_i
!!!           s_any_d
!!!           s_any_z
!!!           s_eye_i
!!!           s_eye_d
!!!           s_eye_z
!!!           s_identity_i
!!!           s_identity_d
!!!           s_identity_z
!!!           s_diag_i
!!!           s_diag_d
!!!           s_diag_z
!!!           s_trace_d
!!!           s_trace_z
!!!           s_det_d
!!!           s_det_z
!!!           s_inv_d
!!!           s_inv_z
!!!           s_eig_dg
!!!           s_eig_zg
!!!           s_eigvals_dg
!!!           s_eigvals_zg
!!!           s_eig_sy
!!!           s_eig_he
!!!           s_eigvals_sy
!!!           s_eigvals_he
!!!           s_solve_dg
!!!           s_solve_zg
!!!           s_solve_sy
!!!           s_solve_he
!!!           s_svd_dg
!!!           s_svd_zg
!!!           s_is_lower_triangular_d
!!!           s_is_lower_triangular_z
!!!           s_is_singular_d
!!!           s_is_singular_z
!!!           s_hilbert_d
!!!           s_inverse_hilbert_d
!!!           s_vandermonde_d
!!!           s_vandermonde_z
!!!           s_pascal_d
!!!           s_wilkinson_d
!!!           s_sparse_random_d
!!!           s_sparse_random_z
!!! source  : s_matrix.f90
!!! type    : subroutines
!!! author  : li huang (email:huangli@caep.cn)
!!! history : 07/10/2014 by li huang (created)
!!!           03/07/2026 by li huang (last modified)
!!! purpose : these subroutines are used to encapsulate some important and
!!!           frequently used linear algebra operations.
!!! status  : unstable
!!! comment :
!!!-----------------------------------------------------------------------

!!========================================================================
!!>>> matrix construction: build zeros/ones/any matrix                 <<<
!!========================================================================

!!
!! @sub s_zeros_i
!!
!! build an integer matrix with all elements are zero.
!!
  subroutine s_zeros_i(n, A)
     implicit none

!! external arguments
     ! size of matrix
     integer, intent(in)  :: n

     ! input/output matrix
     integer, intent(out) :: A(n,n)

!! [body

     A = 0

!! body]

     return
  end subroutine s_zeros_i

!!
!! @sub s_zeros_d
!!
!! build a real(dp) matrix with all elements are zero.
!!
  subroutine s_zeros_d(n, A)
     use constants, only : dp
     use constants, only : zero

     implicit none

!! external arguments
     ! size of matrix
     integer, intent(in)   :: n

     ! input/output matrix
     real(dp), intent(out) :: A(n,n)

!! [body

     A = zero

!! body]

     return
  end subroutine s_zeros_d

!!
!! @sub s_zeros_z
!!
!! build a complex(dp) matrix with all elements are zero.
!!
  subroutine s_zeros_z(n, A)
     use constants, only : dp
     use constants, only : czero

     implicit none

!! external arguments
     ! size of matrix
     integer, intent(in)      :: n

     ! input/output matrix
     complex(dp), intent(out) :: A(n,n)

!! [body

     A = czero

!! body]

     return
  end subroutine s_zeros_z

!!
!! @sub s_ones_i
!!
!! build an integer matrix with all elements are one.
!!
  subroutine s_ones_i(n, A)
     implicit none

!! external arguments
     ! size of matrix
     integer, intent(in)  :: n

     ! input/output matrix
     integer, intent(out) :: A(n,n)

!! [body

     A = 1

!! body]

     return
  end subroutine s_ones_i

!!
!! @sub s_ones_d
!!
!! build a real(dp) matrix with all elements are one.
!!
  subroutine s_ones_d(n, A)
     use constants, only : dp
     use constants, only : one

     implicit none

!! external arguments
     ! size of matrix
     integer, intent(in)   :: n

     ! input/output matrix
     real(dp), intent(out) :: A(n,n)

!! [body

     A = one

!! body]

     return
  end subroutine s_ones_d

!!
!! @sub s_ones_z
!!
!! build a complex(dp) matrix with all elements are one.
!!
  subroutine s_ones_z(n, A)
     use constants, only : dp
     use constants, only : cone

     implicit none

!! external arguments
     ! size of matrix
     integer, intent(in)      :: n

     ! input/output matrix
     complex(dp), intent(out) :: A(n,n)

!! [body

     A = cone

!! body]

     return
  end subroutine s_ones_z

!!
!! @sub s_any_i
!!
!! build an integer matrix with all elements are given by i.
!!
  subroutine s_any_i(n, i, A)
     implicit none

!! external arguments
     ! size of matrix
     integer, intent(in)  :: n

     ! value of matrix element
     integer, intent(in)  :: i

     ! input/output matrix
     integer, intent(out) :: A(n,n)

!! [body

     A = i

!! body]

     return
  end subroutine s_any_i

!!
!! @sub s_any_d
!!
!! build a real(dp) matrix with all elements are given by d.
!!
  subroutine s_any_d(n, d, A)
     use constants, only : dp

     implicit none

!! external arguments
     ! size of matrix
     integer, intent(in)   :: n

     ! value of matrix element
     real(dp), intent(in)  :: d

     ! input/output matrix
     real(dp), intent(out) :: A(n,n)

!! [body

     A = d

!! body]

     return
  end subroutine s_any_d

!!
!! @sub s_any_z
!!
!! build a complex(dp) matrix with all elements are given by z.
!!
  subroutine s_any_z(n, z, A)
     use constants, only : dp

     implicit none

!! external arguments
     ! size of matrix
     integer, intent(in)      :: n

     ! value of matrix element
     complex(dp), intent(in)  :: z

     ! input/output matrix
     complex(dp), intent(out) :: A(n,n)

!! [body

     A = z

!! body]

     return
  end subroutine s_any_z

!!========================================================================
!!>>> matrix construction: build diagonal matrix                       <<<
!!========================================================================

!!
!! @sub s_eye_i
!!
!! build integer matrix with ones on the diagonal and zeros elsewhere.
!!
  subroutine s_eye_i(n, k, A)
     implicit none

!! external arguments
     ! size of matrix
     integer, intent(in)  :: n

     ! index of the diagonal, here:
     ! 0 refers to the main diagonal;
     ! a positive value refers to an upper diagonal;
     ! and a negative value to a lower diagonal.
     integer, intent(in)  :: k

     ! input/output matrix
     integer, intent(out) :: A(n,n)

!! local variables
     ! loop index
     integer :: i

!! [body

     A = 0
     !
     do i=1,n
         if ( i - k < 1 .or. i - k > n ) CYCLE
         A(i,i-k) = 1
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_eye_i

!!
!! @sub s_eye_d
!!
!! build real(dp) matrix with ones on the diagonal and zeros elsewhere.
!!
  subroutine s_eye_d(n, k, A)
     use constants, only : dp
     use constants, only : zero, one

     implicit none

!! external arguments
     ! size of matrix
     integer, intent(in)   :: n

     ! index of the diagonal, here:
     ! 0 refers to the main diagonal;
     ! a positive value refers to an upper diagonal;
     ! and a negative value to a lower diagonal.
     integer, intent(in)   :: k

     ! input/output matrix
     real(dp), intent(out) :: A(n,n)

!! local variables
     ! loop index
     integer :: i

!! [body

     A = zero
     !
     do i=1,n
         if ( i - k < 1 .or. i - k > n ) CYCLE
         A(i,i-k) = one
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_eye_d

!!
!! @sub s_eye_z
!!
!! build complex(dp) matrix with ones on the diagonal and zeros elsewhere.
!!
  subroutine s_eye_z(n, k, A)
     use constants, only : dp
     use constants, only : czero, cone

     implicit none

!! external arguments
     ! size of matrix
     integer, intent(in)      :: n

     ! index of the diagonal, here:
     ! 0 refers to the main diagonal;
     ! a positive value refers to an upper diagonal;
     ! and a negative value to a lower diagonal.
     integer, intent(in)      :: k

     ! input/output matrix
     complex(dp), intent(out) :: A(n,n)

!! local variables
     ! loop index
     integer :: i

!! [body

     A = czero
     !
     do i=1,n
         if ( i - k < 1 .or. i - k > n ) CYCLE
         A(i,i-k) = cone
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_eye_z

!!
!! @sub s_identity_i
!!
!! build integer identity matrix.
!!
  subroutine s_identity_i(n, A)
     implicit none

!! external arguments
     ! size of matrix
     integer, intent(in)  :: n

     ! input/output matrix
     integer, intent(out) :: A(n,n)

!! local variables
     ! loop index
     integer :: i

!! [body

     A = 0
     !
     do i=1,n
         A(i,i) = 1
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_identity_i

!!
!! @sub s_identity_d
!!
!! build real(dp) identity matrix.
!!
  subroutine s_identity_d(n, A)
     use constants, only : dp
     use constants, only : zero, one

     implicit none

!! external arguments
     ! size of matrix
     integer, intent(in)   :: n

     ! input/output matrix
     real(dp), intent(out) :: A(n,n)

!! local variables
     ! loop index
     integer :: i

!! [body

     A = zero
     !
     do i=1,n
         A(i,i) = one
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_identity_d

!!
!! @sub s_identity_z
!!
!! build complex(dp) identity matrix.
!!
  subroutine s_identity_z(n, A)
     use constants, only : dp
     use constants, only : czero, cone

     implicit none

!! external arguments
     ! size of matrix
     integer, intent(in)      :: n

     ! input/output matrix
     complex(dp), intent(out) :: A(n,n)

!! local variables
     ! loop index
     integer :: i

!! [body

     A = czero
     !
     do i=1,n
         A(i,i) = cone
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_identity_z

!!
!! @sub s_diag_i
!!
!! build integer diagonal matrix from a vector.
!!
  subroutine s_diag_i(n, v, A)
     implicit none

!! external arguments
     ! size of matrix
     integer, intent(in)  :: n

     ! input integer vector
     integer, intent(in)  :: v(n)

     ! output integer diagonal matrix
     integer, intent(out) :: A(n,n)

!! local variables
     ! loop index
     integer :: i

!! [body

     A = 0
     !
     do i=1,n
         A(i,i) = v(i)
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_diag_i

!!
!! @sub s_diag_d
!!
!! build real(dp) diagonal matrix from a vector.
!!
  subroutine s_diag_d(n, v, A)
     use constants, only : dp
     use constants, only : zero

     implicit none

!! external arguments
     ! size of matrix
     integer, intent(in)   :: n

     ! input real(dp) vector
     real(dp), intent(in)  :: v(n)

     ! output real(dp) diagonal matrix
     real(dp), intent(out) :: A(n,n)

!! local variables
     ! loop index
     integer :: i

!! [body

     A = zero
     !
     do i=1,n
         A(i,i) = v(i)
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_diag_d

!!
!! @sub s_diag_z
!!
!! build complex(dp) diagonal matrix from a vector.
!!
  subroutine s_diag_z(n, v, A)
     use constants, only : dp
     use constants, only : czero

     implicit none

!! external arguments
     ! size of matrix
     integer, intent(in)      :: n

     ! input complex(dp) vector
     complex(dp), intent(in)  :: v(n)

     ! output complex(dp) diagonal matrix
     complex(dp), intent(out) :: A(n,n)

!! local variables
     ! loop index
     integer :: i

!! [body

     A = czero
     !
     do i=1,n
         A(i,i) = v(i)
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_diag_z

!!========================================================================
!!>>> matrix query: return matrix's trace or determinant               <<<
!!========================================================================

!!
!! @sub s_trace_d
!!
!! return trace for a real(dp) array.
!!
  subroutine s_trace_d(n, A, tr)
     use constants, only : dp
     use constants, only : zero

     implicit none

!! external arguments
     ! size of matrix
     integer, intent(in)   :: n

     ! output matrix's trace
     real(dp), intent(out) :: tr

     ! input real(dp) matrix
     real(dp), intent(in)  :: A(n,n)

!! local variables
     ! loop index
     integer :: i

!! [body

     tr = zero
     !
     do i=1,n
         tr = tr + A(i,i)
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_trace_d

!!
!! @sub s_trace_z
!!
!! return trace for a complex(dp) array.
!!
  subroutine s_trace_z(n, A, tr)
     use constants, only : dp
     use constants, only : czero

     implicit none

!! external arguments
     ! size of matrix
     integer, intent(in)      :: n

     ! output matrix's trace
     complex(dp), intent(out) :: tr

     ! input complex(dp) matrix
     complex(dp), intent(in)  :: A(n,n)

!! local variables
     ! loop index
     integer :: i

!! [body

     tr = czero
     !
     do i=1,n
         tr = tr + A(i,i)
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_trace_z

!!
!! @sub s_det_d
!!
!! calculate the determinant of a real(dp) matrix.
!!
  subroutine s_det_d(ndim, dmat, ddet)
     use constants, only : dp
     use constants, only : one, cone

     implicit none

!! external arguments
     ! dimension of dmat matrix
     integer, intent(in)     :: ndim

     ! determinant of dmat matrix
     real(dp), intent(out)   :: ddet

     ! object matrix.
     ! on entry, it contains the original matrix;
     ! on exit, it is destroyed and replaced with the L and U matrix.
     real(dp), intent(inout) :: dmat(ndim,ndim)

!! local variables
     ! loop index
     integer  :: i

     ! error flag
     integer  :: ierror

     ! size of working array work
     integer  :: lwork

     ! used to calculate determinant
     complex(dp) :: cres

     ! working arrays for lapack subroutines: dgetrf
     integer, allocatable  :: ipiv(:)

     ! working arrays for lapack subroutines: dgeev
     real(dp), allocatable :: work(:)

     ! real and imaginary parts of the computed eigenvalues
     real(dp), allocatable :: wi(:)
     real(dp), allocatable :: wr(:)

     ! left and right eigenvectors
     real(dp), allocatable :: vl(:,:)
     real(dp), allocatable :: vr(:,:)

     ! dummy arrays, used to save dmat
     real(dp), allocatable :: amat(:,:)

!! [body

     ! setup lwork
     lwork = 4 * ndim

     ! allocate memory
     allocate(ipiv(ndim),      stat=ierror)
     allocate(work(lwork),     stat=ierror)
     allocate(wi(ndim),        stat=ierror)
     allocate(wr(ndim),        stat=ierror)
     allocate(vl(ndim,ndim),   stat=ierror)
     allocate(vr(ndim,ndim),   stat=ierror)
     allocate(amat(ndim,ndim), stat=ierror)
     !
     if ( ierror /= 0 ) then
         call s_print_error('s_det_d','can not allocate enough memory')
     endif ! back if ( ierror /= 0 ) block

     ! copy dmat to amat at first
     amat = dmat

!-------------------------------------------------------------------------
! method A: preferred method
!-------------------------------------------------------------------------

     ! computes the LU factorization of a general m-by-n matrix.
     ! need lapack package (dgetrf subroutine).
     call DGETRF(ndim, ndim, dmat, ndim, ipiv, ierror)
     !
     if ( ierror /= 0 ) then
         call s_print_exception('s_det_d','error in lapack subroutine dgetrf')
     endif ! back if ( ierror /= 0 ) block

     ! calculate determinant
     ddet = one
     !
     do i=1,ndim
         if ( ipiv(i) == i ) then
             ddet = ddet * ( +dmat(i,i) )
         else
             ddet = ddet * ( -dmat(i,i) )
         endif ! back if ( ipiv(i) == i ) block
     enddo ! over i={1,ndim} loop

     ! everything is ok!
     if ( ierror == 0 ) RETURN

!-------------------------------------------------------------------------
! method B: as a backup
!-------------------------------------------------------------------------

     ! diagonalize amat to obtain its eigenvalues: wr and wi.
     call DGEEV('N', 'N', ndim, amat, ndim, wr, wi, vl, ndim, vr, &
         & ndim, work, lwork, ierror)
     !
     if ( ierror /= 0 ) then
         call s_print_error('s_det_d','error in lapack subroutine dgeev')
     endif ! back if ( ierror /= 0 ) block

     ! evaluate the final determinant
     cres = cone
     !
     do i=1,ndim
         cres = cres * dcmplx( wr(i), wi(i) )
     enddo ! over i={1,ndim} loop
     ddet = real(cres)

     ! deallocate memory
     if ( allocated(ipiv) ) deallocate(ipiv)
     if ( allocated(work) ) deallocate(work)
     if ( allocated(wi  ) ) deallocate(wi  )
     if ( allocated(wr  ) ) deallocate(wr  )
     if ( allocated(vl  ) ) deallocate(vl  )
     if ( allocated(vr  ) ) deallocate(vr  )
     if ( allocated(amat) ) deallocate(amat)

!! body]

     return
  end subroutine s_det_d

!!
!! @sub s_det_z
!!
!! calculate the determinant of a complex(dp) matrix.
!!
  subroutine s_det_z(ndim, zmat, zdet)
     use constants, only : dp
     use constants, only : cone

     implicit none

!! external arguments
     ! dimension of zmat matrix
     integer, intent(in)        :: ndim

     ! determinant of zmat matrix
     complex(dp), intent(out)   :: zdet

     ! object matrix.
     ! on entry, it contains the original matrix;
     ! on exit, it is destroyed and replaced with the L and U matrix.
     complex(dp), intent(inout) :: zmat(ndim,ndim)

!! local variables
     ! loop index
     integer :: i

     ! error flag
     integer :: ierror

     ! working arrays for lapack subroutines
     integer, allocatable :: ipiv(:)

!! [body

     ! allocate memory
     allocate(ipiv(ndim), stat=ierror)
     !
     if ( ierror /= 0 ) then
         call s_print_error('s_det_z','can not allocate enough memory')
     endif ! back if ( ierror /= 0 ) block

     ! computes the LU factorization of a general m-by-n matrix.
     ! need lapack package (zgetrf subroutine).
     call ZGETRF(ndim, ndim, zmat, ndim, ipiv, ierror)
     !
     if ( ierror /= 0 ) then
         call s_print_error('s_det_z','error in lapack subroutine zgetrf')
     endif ! back if ( ierror /= 0 ) block

     ! calculate determinant
     zdet = cone
     !
     do i=1,ndim
         if ( ipiv(i) == i ) then
             zdet = zdet * ( +zmat(i,i) )
         else
             zdet = zdet * ( -zmat(i,i) )
         endif ! back if ( ipiv(i) == i ) block
     enddo ! over i={1,ndim} loop

     ! deallocate memory
     if ( allocated(ipiv) ) deallocate(ipiv)

!! body]

     return
  end subroutine s_det_z

!!========================================================================
!!>>> matrix manipulation: calculate matrix's inversion                <<<
!!========================================================================

!!
!! @sub s_inv_d
!!
!! invert real(dp) matrix using lapack subroutines.
!!
  subroutine s_inv_d(ndim, dmat)
     use constants, only : dp

     implicit none

!! external arguments
     ! dimension of dmat matrix
     integer, intent(in)     :: ndim

     ! object matrix.
     ! on entry, it contains the original matrix;
     ! on exit, it is destroyed and replaced with the inversed matrix.
     real(dp), intent(inout) :: dmat(ndim,ndim)

!! local variables
     ! error flag
     integer :: ierror

     ! the length of the array work
     integer :: lwork

     ! working arrays for lapack subroutines
     integer, allocatable  :: ipiv(:)
     real(dp), allocatable :: work(:)

!! [body

     ! initialize lwork
     lwork = 4 * ndim

     ! allocate memory
     allocate(ipiv(ndim), stat=ierror)
     allocate(work(lwork), stat=ierror)
     !
     if ( ierror /= 0 ) then
         call s_print_error('s_inv_d','can not allocate enough memory')
     endif ! back if ( ierror /= 0 ) block

     ! computes the LU factorization of a general m-by-n matrix.
     ! need lapack package (dgetrf subroutine).
     call DGETRF(ndim, ndim, dmat, ndim, ipiv, ierror)
     !
     if ( ierror /= 0 ) then
         call s_print_error('s_inv_d','error in lapack subroutine dgetrf')
     endif ! back if ( ierror /= 0 ) block

     ! computes the inverse of an LU-factored general matrix,
     ! need lapack package (dgetri subroutine).
     call DGETRI(ndim, dmat, ndim, ipiv, work, lwork, ierror)
     !
     if ( ierror /= 0 ) then
         call s_print_error('s_inv_d','error in lapack subroutine dgetri')
     endif ! back if ( ierror /= 0 ) block

     ! deallocate memory
     if ( allocated(ipiv) ) deallocate(ipiv)
     if ( allocated(work) ) deallocate(work)

!! body]

     return
  end subroutine s_inv_d

!!
!! @sub s_inv_z
!!
!! invert complex(dp) matrix using lapack subroutines.
!!
  subroutine s_inv_z(ndim, zmat)
     use constants, only : dp

     implicit none

!! external arguments
     ! dimension of zmat matrix
     integer, intent(in)        :: ndim

     ! object matrix.
     ! on entry, it contains the original matrix;
     ! on exit, it is destroyed and replaced with the inversed matrix.
     complex(dp), intent(inout) :: zmat(ndim,ndim)

!! local variables
     ! error flag
     integer :: ierror

     ! the length of the array work
     integer :: lwork

     ! working arrays for lapack subroutines
     integer, allocatable     :: ipiv(:)
     complex(dp), allocatable :: work(:)

!! [body

     ! initialize lwork
     lwork = 4 * ndim

     ! allocate memory
     allocate(ipiv(ndim), stat=ierror)
     allocate(work(lwork), stat=ierror)
     !
     if ( ierror /= 0 ) then
         call s_print_error('s_inv_z','can not allocate enough memory')
     endif ! back if ( ierror /= 0 ) block

     ! computes the LU factorization of a general m-by-n matrix.
     ! need lapack package (zgetrf subroutine).
     call ZGETRF(ndim, ndim, zmat, ndim, ipiv, ierror)
     !
     if ( ierror /= 0 ) then
         call s_print_error('s_inv_z','error in lapack subroutine zgetrf')
     endif ! back if ( ierror /= 0 ) block

     ! computes the inverse of an LU-factored general matrix.
     ! need lapack package (zgetri subroutine).
     call ZGETRI(ndim, zmat, ndim, ipiv, work, lwork, ierror)
     !
     if ( ierror /= 0 ) then
         call s_print_error('s_inv_z','error in lapack subroutine zgetri')
     endif ! back if ( ierror /= 0 ) block

     ! deallocate memory
     if ( allocated(ipiv) ) deallocate(ipiv)
     if ( allocated(work) ) deallocate(work)

!! body]

     return
  end subroutine s_inv_z

!!========================================================================
!!>>> matrix manipulation: solve eigenvalues and eigenvectors problem  <<<
!!========================================================================

!!
!! @sub s_eig_dg
!!
!! try to diagonalize a general real(dp) matrix, return its eigenvalues
!! and eigenvectors.
!!
  subroutine s_eig_dg(ldim, ndim, amat, eval, evec)
     use constants, only : dp
     use constants, only : zero

     implicit none

!! external arguments
     ! leading dimension of matrix amat
     integer, intent(in)   :: ldim

     ! the order of the matrix amat
     integer, intent(in)   :: ndim

     ! original general real(dp) matrix to compute eigenvals
     ! and eigenvectors
     real(dp), intent(in)  :: amat(ldim,ndim)

     ! if info = 0, the eigenvalues in ascending order
     real(dp), intent(out) :: eval(ndim)

     ! if info = 0, orthonormal eigenvectors of the matrix
     real(dp), intent(out) :: evec(ldim,ndim)

!! local variables
     ! status flag
     integer :: istat

     ! return information from subroutine dgeev
     integer :: info

     ! the length of the array work.
     ! lwork >= max(1,4*ndim)
     integer :: lwork

     ! workspace array
     real(dp), allocatable :: work(:)

     ! auxiliary real(dp) matrix: real and imaginary parts of eigenvalues
     real(dp), allocatable :: wr(:)
     real(dp), allocatable :: wi(:)

     ! auxiliary real(dp) matrix: left and right eigenvectors
     real(dp), allocatable :: vr(:,:)
     real(dp), allocatable :: vl(:,:)

!! [body

     ! initialize lwork
     lwork = 4 * ndim

     ! allocate memory
     allocate(work(lwork),   stat=istat)
     allocate(wr(ndim),      stat=istat)
     allocate(wi(ndim),      stat=istat)
     allocate(vr(ndim,ndim), stat=istat)
     allocate(vl(ndim,ndim), stat=istat)
     !
     if ( istat /= 0 ) then
         call s_print_error('s_eig_dg','can not allocate enough memory')
     endif ! back if ( istat /= 0 ) block

     ! initialize output arrays
     eval = zero
     evec = amat

     ! call the computational subroutine: dgeev
     call DGEEV('N', 'V', ndim, evec, ldim, wr, wi, vl, ndim, vr, &
         & ndim, work, lwork, info)

     ! check the status
     if ( info /= 0 ) then
         call s_print_error('s_eig_dg','error in lapack subroutine dgeev')
     endif ! back if ( info /= 0 ) block

     ! copy eigenvalues and eigenvectors
     eval(1:ndim) = wr(1:ndim)
     evec(1:ndim,1:ndim) = vr(1:ndim,1:ndim)

     ! dealloate memory for workspace array
     if ( allocated(work) ) deallocate(work)
     if ( allocated(wr  ) ) deallocate(wr  )
     if ( allocated(wi  ) ) deallocate(wi  )
     if ( allocated(vr  ) ) deallocate(vr  )
     if ( allocated(vl  ) ) deallocate(vl  )

!! body]

     return
  end subroutine s_eig_dg

!!
!! @sub s_eig_zg
!!
!! try to diagonalize a general complex(dp) matrix and return its
!! eigenvalues and eigenvectors.
!!
  subroutine s_eig_zg(ldim, ndim, zmat, zeig, zvec)
     use constants, only : dp
     use constants, only : czero

     implicit none

!! external arguments
     ! leading dimension of matrix amat
     integer, intent(in)      :: ldim

     ! the order of the matrix amat
     integer, intent(in)      :: ndim

     ! original general complex(dp) matrix to compute eigenvals
     ! and eigenvectors
     complex(dp), intent(in)  :: zmat(ldim,ndim)

     ! if info = 0, the eigenvalues in ascending order
     complex(dp), intent(out) :: zeig(ndim)

     ! if info = 0, orthonormal eigenvectors of the matrix
     complex(dp), intent(out) :: zvec(ldim,ndim)

!! local variables
     ! status flag
     integer :: istat

     ! return information from subroutine zgeev
     integer :: info

     ! the length of the array work.
     ! lwork >= max(1,2*ndim)
     integer :: lwork

     ! workspace array
     complex(dp), allocatable :: work(:)

     ! auxiliary real(dp) matrix
     real(dp), allocatable    :: rwork(:)

     ! auxiliary complex(dp) matrix: left and right eigenvectors
     complex(dp), allocatable :: vr(:,:)
     complex(dp), allocatable :: vl(:,:)

!! [body

     ! initialize lwork
     lwork = 2 * ndim

     ! allocate memory
     allocate(work(lwork),   stat=istat)
     allocate(rwork(lwork),  stat=istat)
     allocate(vr(ndim,ndim), stat=istat)
     allocate(vl(ndim,ndim), stat=istat)
     !
     if ( istat /= 0 ) then
         call s_print_error('s_eig_zg','can not allocate enough memory')
     endif ! back if ( istat /= 0 ) block

     ! initialize output arrays
     zeig = czero
     zvec = zmat

     ! call the computational subroutine: zgeev
     call ZGEEV('N', 'V', ndim, zvec, ldim, zeig, vl, ndim, vr, &
         & ndim, work, lwork, rwork, info)

     ! check the status
     if ( info /= 0 ) then
         call s_print_error('s_eig_zg','error in lapack subroutine zgeev')
     endif ! back if ( info /= 0 ) block

     ! copy eigenvectors
     zvec = vr

     ! dealloate memory for workspace array
     if ( allocated(work ) )  deallocate(work )
     if ( allocated(rwork) )  deallocate(rwork)
     if ( allocated(vr   ) )  deallocate(vr   )
     if ( allocated(vl   ) )  deallocate(vl   )

!! body]

     return
  end subroutine s_eig_zg

!!
!! @sub s_eigvals_dg
!!
!! diagonalize a general real(dp) matrix and return its eigenvalues only.
!!
  subroutine s_eigvals_dg(ldim, ndim, amat, eval)
     use constants, only : dp
     use constants, only : zero

     implicit none

!! external arguments
     ! leading dimension of matrix amat
     integer, intent(in)   :: ldim

     ! the order of the matrix amat
     integer, intent(in)   :: ndim

     ! original general real(dp) matrix to compute eigenvals
     real(dp), intent(in)  :: amat(ldim,ndim)

     ! if info = 0, the eigenvalues in ascending order
     real(dp), intent(out) :: eval(ndim)

!! local variables
     ! status flag
     integer :: istat

     ! return information from subroutine dgeev
     integer :: info

     ! the length of the array work.
     ! lwork >= max(1,4*ndim)
     integer :: lwork

     ! workspace array, used to store amat
     real(dp), allocatable :: evec(:,:)

     ! workspace array
     real(dp), allocatable :: work(:)

     ! auxiliary real(dp) matrix: real and imaginary parts of eigenvalues
     real(dp), allocatable :: wr(:)
     real(dp), allocatable :: wi(:)

     ! auxiliary real(dp) matrix: left and right eigenvectors
     real(dp), allocatable :: vr(:,:)
     real(dp), allocatable :: vl(:,:)

!! [body

     ! initialize lwork
     lwork = 4 * ndim

     ! allocate memory
     allocate(evec(ldim,ndim), stat=istat)
     allocate(work(lwork),     stat=istat)
     allocate(wr(ndim),        stat=istat)
     allocate(wi(ndim),        stat=istat)
     allocate(vr(ndim,ndim),   stat=istat)
     allocate(vl(ndim,ndim),   stat=istat)
     !
     if ( istat /= 0 ) then
         call s_print_error('s_eigvals_dg','can not allocate enough memory')
     endif ! back if ( istat /= 0 ) block

     ! initialize output arrays
     eval = zero
     evec = amat

     ! call the computational subroutine: dgeev
     call DGEEV('N', 'N', ndim, evec, ldim, wr, wi, vl, ndim, vr, &
         & ndim, work, lwork, info)

     ! check the status
     if ( info /= 0 ) then
         call s_print_error('s_eigvals_dg','error in lapack subroutine dgeev')
     endif ! back if ( info /= 0 ) block

     ! copy eigenvalues
     eval(1:ndim) = wr(1:ndim)

     ! dealloate memory for workspace array
     if ( allocated(evec) ) deallocate(evec)
     if ( allocated(work) ) deallocate(work)
     if ( allocated(wr  ) ) deallocate(wr  )
     if ( allocated(wi  ) ) deallocate(wi  )
     if ( allocated(vr  ) ) deallocate(vr  )
     if ( allocated(vl  ) ) deallocate(vl  )

!! body]

     return
  end subroutine s_eigvals_dg

!!
!! @sub s_eigvals_zg
!!
!! diagonalize a general complex(dp) matrix and return its eigenvalues only.
!!
  subroutine s_eigvals_zg(ldim, ndim, zmat, zeig)
     use constants, only : dp
     use constants, only : czero

     implicit none

!! external arguments
     ! leading dimension of matrix amat
     integer, intent(in)      :: ldim

     ! the order of the matrix amat
     integer, intent(in)      :: ndim

     ! original general complex(dp) matrix to compute eigenvals
     complex(dp), intent(in)  :: zmat(ldim,ndim)

     ! if info = 0, the eigenvalues in ascending order
     complex(dp), intent(out) :: zeig(ndim)

!! local variables
     ! status flag
     integer :: istat

     ! return information from subroutine zgeev
     integer :: info

     ! the length of the array work.
     ! lwork >= max(1,2*ndim)
     integer :: lwork

     ! workspace array, used to store amat
     complex(dp), allocatable :: zvec(:,:)

     ! workspace array
     complex(dp), allocatable :: work(:)

     ! auxiliary real(dp) matrix
     real(dp), allocatable    :: rwork(:)

     ! auxiliary complex(dp) matrix: left and right eigenvectors
     complex(dp), allocatable :: vr(:,:)
     complex(dp), allocatable :: vl(:,:)

!! [body

     ! initialize lwork
     lwork = 2 * ndim

     ! allocate memory
     allocate(zvec(ldim,ndim), stat=istat)
     allocate(work(lwork),     stat=istat)
     allocate(rwork(lwork),    stat=istat)
     allocate(vr(ndim,ndim),   stat=istat)
     allocate(vl(ndim,ndim),   stat=istat)
     !
     if ( istat /= 0 ) then
         call s_print_error('s_eigvals_zg','can not allocate enough memory')
     endif ! back if ( istat /= 0 ) block

     ! initialize output arrays
     zeig = czero
     zvec = zmat

     ! call the computational subroutine: zgeev
     call ZGEEV('N', 'N', ndim, zvec, ldim, zeig, vl, ndim, vr, &
         & ndim, work, lwork, rwork, info)

     ! check the status
     if ( info /= 0 ) then
         call s_print_error('s_eigvals_zg','error in lapack subroutine zgeev')
     endif ! back if ( info /= 0 ) block

     ! dealloate memory for workspace array
     if ( allocated(zvec ) )  deallocate(zvec )
     if ( allocated(work ) )  deallocate(work )
     if ( allocated(rwork) )  deallocate(rwork)
     if ( allocated(vr   ) )  deallocate(vr   )
     if ( allocated(vl   ) )  deallocate(vl   )

     return
  end subroutine s_eigvals_zg

!!
!! @sub s_eig_sy
!!
!! computes all eigenvalues and eigenvectors of real symmetric matrix.
!!
  subroutine s_eig_sy(ldim, ndim, amat, eval, evec)
     use constants, only : dp
     use constants, only : zero

     implicit none

!! external arguments
     ! leading dimension of matrix amat
     integer, intent(in)   :: ldim

     ! the order of the matrix amat
     integer, intent(in)   :: ndim

     ! original real symmetric matrix to compute eigenvals and eigenvectors
     real(dp), intent(in)  :: amat(ldim,ndim)

     ! if info = 0, the eigenvalues in ascending order
     real(dp), intent(out) :: eval(ndim)

     ! if info = 0, orthonormal eigenvectors of the matrix
     real(dp), intent(out) :: evec(ldim,ndim)

!! local variables
     ! status flag
     integer :: istat

     ! return information from subroutine dysev
     integer :: info

     ! the length of the array work.
     ! lwork >= max(1,3*ndim-1)
     integer :: lwork

     ! workspace array
     real(dp), allocatable :: work(:)

!! [body

     ! initialize lwork
     lwork = 3 * ndim - 1

     ! allocate memory
     allocate(work(lwork), stat=istat)
     !
     if ( istat /= 0 ) then
         call s_print_error('s_eig_sy','can not allocate enough memory')
     endif ! back if ( istat /= 0 ) block

     ! initialize output arrays
     eval = zero
     evec = amat

     ! call the computational subroutine: dsyev
     call DSYEV('V', 'U', ndim, evec, ldim, eval, work, lwork, info)

     ! check the status
     if ( info /= 0 ) then
         call s_print_error('s_eig_sy','error in lapack subroutine dsyev')
     endif ! back if ( info /= 0 ) block

     ! dealloate memory for workspace array
     if ( allocated(work) ) deallocate(work)

!! body]

     return
  end subroutine s_eig_sy

!!
!! @sub s_eig_he
!!
!! computes all eigenvalues and eigenvectors of complex Hermitian matrix.
!!
  subroutine s_eig_he(ldim, ndim, amat, eval, evec)
     use constants, only : dp
     use constants, only : zero

     implicit none

!! external arguments
     ! leading dimension of matrix amat
     integer, intent(in)      :: ldim

     ! the order of the matrix amat
     integer, intent(in)      :: ndim

     ! original complex Hermitian matrix to compute eigenvals
     ! and eigenvectors
     complex(dp), intent(in)  :: amat(ldim,ndim)

     ! if info = 0, the eigenvalues in ascending order
     real(dp), intent(out)    :: eval(ndim)

     ! if info = 0, orthonormal eigenvectors of the matrix
     complex(dp), intent(out) :: evec(ldim,ndim)

!! local variables
     ! status flag
     integer :: istat

     ! return information from subroutine zheev
     integer :: info

     ! the length of the array work and rwork.
     ! lwork >= max(1,2*ndim-1), lrwork >= max(1,3*ndim-2)
     integer :: lwork
     integer :: lrwork

     ! workspace array
     real(dp), allocatable    :: rwork(:)
     complex(dp), allocatable :: work(:)

!! [body

     ! initialize lwork (lrwork)
     lwork = 2 * ndim - 1
     lrwork = 3 * ndim - 2

     ! allocate memory
     allocate(work(lwork),   stat=istat)
     allocate(rwork(lrwork), stat=istat)
     !
     if ( istat /= 0 ) then
         call s_print_error('s_eig_he','can not allocate enough memory')
     endif ! back if ( istat /= 0 ) block

     ! initialize output arrays
     eval = zero
     evec = amat

     ! call the computational subroutine: zheev
     call ZHEEV('V', 'U', ndim, evec, ldim, eval, work, lwork, rwork, info)

     ! check the status
     if ( info /= 0 ) then
         call s_print_error('s_eig_he','error in lapack subroutine zheev')
     endif ! back if ( info /= 0 ) block

     ! dealloate memory for workspace array
     if ( allocated(work ) ) deallocate(work )
     if ( allocated(rwork) ) deallocate(rwork)

!! body]

     return
  end subroutine s_eig_he

!!
!! @sub s_eigvals_sy
!!
!! computes all eigenvalues of real symmetric matrix.
!!
  subroutine s_eigvals_sy(ldim, ndim, amat, eval)
     use constants, only : dp
     use constants, only : zero

     implicit none

!! external arguments
     ! leading dimension of matrix amat
     integer, intent(in)   :: ldim

     ! the order of the matrix amat
     integer, intent(in)   :: ndim

     ! original real symmetric matrix to compute eigenvals
     real(dp), intent(in)  :: amat(ldim,ndim)

     ! if info = 0, the eigenvalues in ascending order
     real(dp), intent(out) :: eval(ndim)

!! local variables
     ! status flag
     integer :: istat

     ! return information from subroutine dysev
     integer :: info

     ! the length of the array work.
     ! lwork >= max(1,3*ndim-1)
     integer :: lwork

     ! workspace array
     real(dp), allocatable :: work(:)

     ! workspace array, used to store amat
     real(dp), allocatable :: evec(:,:)

!! [body

     ! initialize lwork
     lwork = 3 * ndim - 1

     ! allocate memory
     allocate(work(lwork),     stat=istat)
     allocate(evec(ldim,ndim), stat=istat)
     !
     if ( istat /= 0 ) then
         call s_print_error('s_eigvals_sy','can not allocate enough memory')
     endif ! back if ( istat /= 0 ) block

     ! initialize output arrays
     eval = zero
     evec = amat

     ! call the computational subroutine: dsyev
     call DSYEV('N', 'U', ndim, evec, ldim, eval, work, lwork, info)

     ! check the status
     if ( info /= 0 ) then
         call s_print_error('s_eigvals_sy','error in lapack subroutine dsyev')
     endif ! back if ( info /= 0 ) block

     ! dealloate memory for workspace array
     if ( allocated(work) ) deallocate(work)
     if ( allocated(evec) ) deallocate(evec)

!! body]

     return
  end subroutine s_eigvals_sy

!!
!! @sub s_eigvals_he
!!
!! computes all eigenvalues of complex Hermitian matrix.
!!
  subroutine s_eigvals_he(ldim, ndim, amat, eval)
     use constants, only : dp
     use constants, only : zero

     implicit none

!! external arguments
     ! leading dimension of matrix amat
     integer, intent(in)     :: ldim

     ! the order of the matrix amat
     integer, intent(in)     :: ndim

     ! original complex Hermitian matrix to compute eigenvals
     ! and eigenvectors
     complex(dp), intent(in) :: amat(ldim,ndim)

     ! if info = 0, the eigenvalues in ascending order
     real(dp), intent(out)   :: eval(ndim)

!! local variables
     ! status flag
     integer :: istat

     ! return information from subroutine zheev
     integer :: info

     ! the length of the array work and rwork.
     ! lwork >= max(1,2*ndim-1), lrwork >= max(1,3*ndim-2)
     integer :: lwork
     integer :: lrwork

     ! workspace array
     real(dp), allocatable    :: rwork(:)
     complex(dp), allocatable :: work(:)

     ! workspace array, used to store amat
     complex(dp), allocatable :: evec(:,:)

!! [body

     ! initialize lwork (lrwork)
     lwork = 2 * ndim - 1
     lrwork = 3 * ndim - 2

     ! allocate memory
     allocate(work(lwork),     stat=istat)
     allocate(rwork(lrwork),   stat=istat)
     allocate(evec(ldim,ndim), stat=istat)
     !
     if ( istat /= 0 ) then
         call s_print_error('s_eigvals_he','can not allocate enough memory')
     endif ! back if ( istat /= 0 ) block

     ! initialize output arrays
     eval = zero
     evec = amat

     ! call the computational subroutine: zheev
     call ZHEEV('N', 'U', ndim, evec, ldim, eval, work, lwork, rwork, info)

     ! check the status
     if ( info /= 0 ) then
         call s_print_error('s_eigvals_he','error in lapack subroutine zheev')
     endif ! back if ( info /= 0 ) block

     ! dealloate memory for workspace array
     if ( allocated(work ) ) deallocate(work )
     if ( allocated(rwork) ) deallocate(rwork)
     if ( allocated(evec ) ) deallocate(evec )

!! body]

     return
  end subroutine s_eigvals_he

!!========================================================================
!!>>> matrix manipulation: solve linear equations                      <<<
!!========================================================================

!!
!! @sub s_solve_dg
!!
!! solve linear system AX = B, real(dp) general version.
!!
  subroutine s_solve_dg(n, nrhs, A, B)
     use constants, only : dp

     implicit none

!! external arguments
     ! the number of linear equations
     integer, intent(in)     :: n

     ! the number of right-hand sides
     integer, intent(in)     :: nrhs

     ! on entry, it is a n-by-n coefficient matrix A;
     ! on exit, it is overwritten by the factors L and U from the
     ! factorization of A = PLU.
     real(dp), intent(inout) :: A(n,n)

     ! on entry, it is a n-by-nrhs matrix of right hand side matrix B;
     ! on exit, it is overwritten by the solution matrix X.
     real(dp), intent(inout) :: B(n,nrhs)

!! local variables
     ! status flag
     integer :: istat

     ! return information from subroutine dgesv
     integer :: info

     ! workspace array, its dimension is at least max(1,n)
     integer, allocatable :: ipiv(:)

!! [body

     ! allocate memory
     allocate(ipiv(n), stat=istat)
     !
     if ( istat /= 0 ) then
         call s_print_error('s_solve_dg','can not allocate enough memory')
     endif ! back if ( istat /= 0 ) block

     ! call the computational subroutine: dgesv
     call DGESV(n, nrhs, A, n, ipiv, B, n, info)

     ! check the status
     if ( info /= 0 ) then
         call s_print_error('s_solve_dg','error in lapack subroutine dgesv')
     endif ! back if ( info /= 0 ) block

     ! deallocate memory
     if ( allocated(ipiv) ) deallocate(ipiv)

!! body]

     return
  end subroutine s_solve_dg

!!
!! @sub s_solve_zg
!!
!! solve linear system AX = B, complex(dp) general version.
!!
  subroutine s_solve_zg(n, nrhs, A, B)
     use constants, only : dp

     implicit none

!! external arguments
     ! the number of linear equations
     integer, intent(in)        :: n

     ! the number of right-hand sides
     integer, intent(in)        :: nrhs

     ! on entry, it is a n-by-n coefficient matrix A;
     ! on exit, it is overwritten by the factors L and U from the
     ! factorization of A = PLU.
     complex(dp), intent(inout) :: A(n,n)

     ! on entry, it is a n-by-nrhs matrix of right hand side matrix B;
     ! on exit, it is overwritten by the solution matrix X.
     complex(dp), intent(inout) :: B(n,nrhs)

!! local variables
     ! status flag
     integer :: istat

     ! return information from subroutine zgesv
     integer :: info

     ! workspace array, its dimension is at least max(1,n)
     integer, allocatable :: ipiv(:)

!! [body

     ! allocate memory
     allocate(ipiv(n), stat=istat)
     !
     if ( istat /= 0 ) then
         call s_print_error('s_solve_zg','can not allocate enough memory')
     endif ! back if ( istat /= 0 ) block

     ! call the computational subroutine: zgesv
     call ZGESV(n, nrhs, A, n, ipiv, B, n, info)

     ! check the status
     if ( info /= 0 ) then
         call s_print_error('s_solve_zg','error in lapack subroutine zgesv')
     endif ! back if ( info /= 0 ) block

     ! deallocate memory
     if ( allocated(ipiv) ) deallocate(ipiv)

!! body]

     return
  end subroutine s_solve_zg

!!
!! @sub s_solve_sy
!!
!! solve linear system AX = B, real(dp) symmetric version.
!!
  subroutine s_solve_sy(n, nrhs, A, B)
     use constants, only : dp

     implicit none

!! external arguments
     ! the number of linear equations
     integer, intent(in)     :: n

     ! the number of right-hand sides
     integer, intent(in)     :: nrhs

     ! on entry, it is a n-by-n coefficient matrix A;
     ! on exit, it is overwritten by the factors L and U from the
     ! factorization of A = PLU.
     real(dp), intent(inout) :: A(n,n)

     ! on entry, it is a n-by-nrhs matrix of right hand side matrix B;
     ! on exit, it is overwritten by the solution matrix X.
     real(dp), intent(inout) :: B(n,nrhs)

!! local variables
     ! status flag
     integer :: istat

     ! return information from subroutine dsysv
     integer :: info

     ! workspace array, its dimension is at least max(1,n)
     integer, allocatable  :: ipiv(:)

     ! workspace array, its dimension is at least max(1, lwork)
     ! and lwork >= 1
     real(dp), allocatable :: work(:)

!! [body

     ! allocate memory
     allocate(ipiv(n), stat=istat)
     allocate(work(n), stat=istat)
     !
     if ( istat /= 0 ) then
         call s_print_error('s_solve_sy','can not allocate enough memory')
     endif ! back if ( istat /= 0 ) block

     ! call the computational subroutine: dsysv
     call DSYSV('U', n, nrhs, A, n, ipiv, B, n, work, n, info)

     ! check the status
     if ( info /= 0 ) then
         call s_print_error('s_solve_sy','error in lapack subroutine dsysv')
     endif ! back if ( info /= 0 ) block

     ! deallocate memory
     if ( allocated(ipiv) ) deallocate(ipiv)
     if ( allocated(work) ) deallocate(work)

!! body]

     return
  end subroutine s_solve_sy

!!
!! @sub s_solve_he
!!
!! solve linear system AX = B, complex(dp) Hermitian version.
!!
  subroutine s_solve_he(n, nrhs, A, B)
     use constants, only : dp

     implicit none

!! external arguments
     ! the number of linear equations
     integer, intent(in)        :: n

     ! the number of right-hand sides
     integer, intent(in)        :: nrhs

     ! on entry, it is a n-by-n coefficient matrix A;
     ! on exit, it is overwritten by the factors L and U from the
     ! factorization of A = PLU.
     complex(dp), intent(inout) :: A(n,n)

     ! on entry, it is a n-by-nrhs matrix of right hand side matrix B;
     ! on exit, it is overwritten by the solution matrix X.
     complex(dp), intent(inout) :: B(n,nrhs)

!! local variables
     ! status flag
     integer :: istat

     ! return information from subroutine zhesv
     integer :: info

     ! workspace array, its dimension is at least max(1,n)
     integer, allocatable     :: ipiv(:)

     ! workspace array, its dimension is at least max(1, lwork)
     ! and lwork >= 1
     complex(dp), allocatable :: work(:)

!! [body

     ! allocate memory
     allocate(ipiv(n), stat=istat)
     allocate(work(n), stat=istat)
     !
     if ( istat /= 0 ) then
         call s_print_error('s_solve_he','can not allocate enough memory')
     endif ! back if ( istat /= 0 ) block

     ! call the computational subroutine: zhesv
     call ZHESV('U', n, nrhs, A, n, ipiv, B, n, work, n, info)

     ! check the status
     if ( info /= 0 ) then
         call s_print_error('s_solve_he','error in lapack subroutine zhesv')
     endif ! back if ( info /= 0 ) block

     ! deallocate memory
     if ( allocated(ipiv) ) deallocate(ipiv)
     if ( allocated(work) ) deallocate(work)

!! body]

     return
  end subroutine s_solve_he

!!========================================================================
!!>>> matrix manipulation: singular values decomposition               <<<
!!========================================================================

!!
!! @sub s_svd_dg
!!
!! perform the singular values decomposition for a general real(dp) m-by-n
!! matrix A, where A = U * SIGMA * transpose(V), return its left vectors,
!! right vectors, and singular values.
!!
  subroutine s_svd_dg(m, n, min_mn, amat, umat, svec, vmat)
     use constants, only : dp

     implicit none

!! external arguments
     ! number of rows of A matrix
     integer, intent(in)     :: m

     ! number of columns of A matrix
     integer, intent(in)     :: n

     ! minimal value of m and n
     integer, intent(in)     :: min_mn

     ! A matrix
     real(dp), intent(inout) :: amat(m,n)

     ! left vectors of svd, U
     real(dp), intent(out)   :: umat(m,min_mn)

     ! singular values of svd, SIGMA
     real(dp), intent(out)   :: svec(min_mn)

     ! right vectors of svd, transpose(V)
     real(dp), intent(out)   :: vmat(min_mn,n)

!! local variables
     ! status flag
     integer :: istat

     ! return information from dgesvd
     integer :: info

     ! length of work array.
     ! lwork >= max(1, 3 * min_mn + max(m,n), 5 * min_mn)
     integer :: lwork

     ! workspace array
     real(dp), allocatable :: work(:)

!! [body

     ! initialize lwork
     lwork = max(1, 3 * min_mn + max(m,n), 5 * min_mn)

     ! allocate memory
     allocate(work(lwork), stat=istat)
     !
     if ( istat /= 0 ) then
         call s_print_error('s_svd_dg','can not allocate enough memory')
     endif ! back if ( istat /= 0 ) block

     ! call the computational subroutine: dgesvd
     call DGESVD('S', 'S', m, n, amat, m, svec, umat, m, vmat, &
         & min_mn, work, lwork, info)

     ! check the status
     if ( info /= 0 ) then
         call s_print_error('s_svd_dg','error in lapack subroutine dgesvd')
     endif ! back if ( info /= 0 ) block

     ! deallocate the memory for workspace array
     if ( allocated(work) ) deallocate(work)

!! body]

     return
  end subroutine s_svd_dg

!!
!! @sub s_svd_zg
!!
!! perform the singular values decomposition for a general complex(dp)
!! m-by-n matrix A, where A = U * SIGMA * conjugate-transpose(V), return
!! its left vectors, right vectors, and singular values.
!!
  subroutine s_svd_zg(m, n, min_mn, amat, umat, svec, vmat)
     use constants, only : dp

     implicit none

!! external arguments
     ! number of rows of A matrix
     integer, intent(in)        :: m

     ! number of columns of A matrix
     integer, intent(in)        :: n

     ! minimal value of m and n
     integer, intent(in)        :: min_mn

     ! A matrix
     complex(dp), intent(inout) :: amat(m,n)

     ! left vectors of svd, U
     complex(dp), intent(out)   :: umat(m,min_mn)

     ! singular values of svd, SIGMA
     real(dp), intent(out)      :: svec(min_mn)

     ! right vectors of svd, conjugate-transpose(V)
     complex(dp), intent(out)   :: vmat(min_mn,n)

!! local variables
     ! status flag
     integer :: istat

     ! return information from zgesvd
     integer :: info

     ! length of work array.
     ! lwork >= max(1, 2 * min_mn + max(m,n))
     integer :: lwork

     ! workspace arrays
     real(dp), allocatable :: rwork(:)
     complex(dp), allocatable :: work(:)

!! [body

     ! initialize lwork
     lwork = max(1, 2 * min_mn + max(m,n))

     ! allocate memory
     allocate(work(lwork),     stat=istat)
     allocate(rwork(5*min_mn), stat=istat)
     !
     if ( istat /= 0 ) then
         call s_print_error('s_svd_zg','can not allocate enough memory')
     endif ! back if ( istat /= 0 ) block

     ! call the computational subroutine: zgesvd
     call ZGESVD('S', 'S', m, n, amat, m, svec, umat, m, vmat, &
         & min_mn, work, lwork, rwork, info)

     ! check the status
     if ( info /= 0 ) then
         call s_print_error('s_svd_zg','error in lapack subroutine zgesvd')
     endif ! back if ( info /= 0 ) block

     ! deallocate the memory for workspace array
     if ( allocated(work ) ) deallocate(work )
     if ( allocated(rwork) ) deallocate(rwork)

!! body]

     return
  end subroutine s_svd_zg
 
!!========================================================================
!!>>> matrix properties: check if matrix is symmetric                  <<<
!!========================================================================

!!
!! @sub s_is_symmetric_d
!!
!! check if a real(dp) matrix is symmetric (A = A^T).
!!
  subroutine s_is_symmetric_d(n, A, is_symmetric, tol)
     use constants, only : dp
     use constants, only : eps8

     implicit none

!! external arguments
     ! size of matrix (must be square)
     integer, intent(in)   :: n

     ! input matrix
     real(dp), intent(in)  :: A(n,n)

     ! output: .true. if matrix is symmetric, .false. otherwise
     logical, intent(out) :: is_symmetric

     ! tolerance for floating point comparison (optional, default 1.0e-8)
     real(dp), intent(in), optional :: tol

 !! local variables
     ! loop indices
     integer :: i, j

     ! actual tolerance value
     real(dp) :: actual_tol

!! [body

     ! set tolerance (use default if not provided)
     if ( present(tol) ) then
         actual_tol = tol
     else
         actual_tol = eps8
     endif ! back if ( present(tol) ) block

     ! initialize
     is_symmetric = .true.

     ! compare upper triangular part with lower triangular part
     ! we only need to compare i > j (lower triangle) with (j,i) (upper triangle)
     outer_loop: do i=2,n
         inner_loop: do j=1,i-1
             ! check symmetry with tolerance
             if ( abs( A(i,j) - A(j,i) ) > actual_tol ) then
                 is_symmetric = .false.
                 exit outer_loop
             endif ! back if ( abs( A(i,j) - A(j,i) ) > actual_tol ) block
         enddo inner_loop ! over j={1,i-1} loop (inner_loop)
     enddo outer_loop ! over i={2,n} loop (outer_loop)

 !! body]

      return
   end subroutine s_is_symmetric_d

!!
!! @sub s_is_hermitian_z
!!
!! check if a complex(dp) matrix is Hermitian (A = A^H).
!!
  subroutine s_is_hermitian_z(n, A, is_hermitian, tol)
     use constants, only : dp
     use constants, only : eps8

     implicit none

!! external arguments
     ! size of matrix (must be square)
     integer, intent(in)        :: n

     ! input matrix
     complex(dp), intent(in)   :: A(n,n)

     ! output: .true. if matrix is Hermitian, .false. otherwise
     logical, intent(out)       :: is_hermitian

     ! tolerance for floating point comparison (optional, default 1.0e-8)
     real(dp), intent(in), optional :: tol

!! local variables
     ! loop indices
     integer :: i, j

     ! actual tolerance value
     real(dp) :: actual_tol

!! [body

     ! set tolerance (use default if not provided)
     if ( present(tol) ) then
         actual_tol = tol
     else
         actual_tol = eps8
     endif ! back if ( present(tol) ) block

     ! initialize
     is_hermitian = .true.

     ! compare upper triangular part with lower triangular part
     ! Hermitian condition: A(i,j) = conjg(A(j,i))
     ! we only need to compare i > j (lower triangle) with (j,i) (upper triangle)
     outer_loop: do i=2,n
         inner_loop: do j=1,i-1
             ! check Hermitian condition with tolerance
             if ( abs( A(i,j) - conjg( A(j,i) ) ) > actual_tol ) then
                 is_hermitian = .false.
                 exit outer_loop
             endif ! back if ( abs( A(i,j) - conj( A(j,i) ) ) > actual_tol ) block
         enddo inner_loop ! over j={1,i-1} loop (inner_loop)
     enddo outer_loop ! over i={2,n} loop (outer_loop)

!! body]

     return
  end subroutine s_is_hermitian_z

!!
!! @sub s_is_diagonal_d
!!
!! check if a real(dp) matrix is diagonal (all off-diagonal elements are zero).
!!
  subroutine s_is_diagonal_d(n, A, is_diagonal, tol)
     use constants, only : dp
     use constants, only : eps8

     implicit none

!! external arguments
     ! size of matrix (must be square)
     integer, intent(in)  :: n

     ! input matrix
     real(dp), intent(in) :: A(n,n)

     ! output: .true. if matrix is diagonal, .false. otherwise
     logical, intent(out) :: is_diagonal

     ! tolerance for floating point comparison (optional, default 1.0e-8)
     real(dp), intent(in), optional :: tol

!! local variables
     ! loop indices
     integer :: i, j

     ! actual tolerance value
     real(dp) :: actual_tol

!! [body

     ! set tolerance (use default if not provided)
     if ( present(tol) ) then
         actual_tol = tol
     else
         actual_tol = eps8
     endif ! back if ( present(tol) ) block

     ! initialize
     is_diagonal = .true.

     ! check all off-diagonal elements
     ! diagonal condition: A(i,j) = 0 for i /= j
     outer_loop: do i=1,n
         inner_loop: do j=1,n
             ! check if off-diagonal element is zero
             if ( i /= j .and. abs( A(i,j) ) > actual_tol ) then
                 is_diagonal = .false.
                 exit outer_loop
             endif ! back if ( i /= j .and. abs( A(i,j) ) > actual_tol ) block
         enddo inner_loop ! over j={1,n} loop (inner_loop)
     enddo outer_loop ! over i={1,n} loop (outer_loop)

!! body]

     return
  end subroutine s_is_diagonal_d

!!
!! @sub s_is_diagonal_z
!!
!! check if a complex(dp) matrix is diagonal (all off-diagonal elements are zero).
!!
  subroutine s_is_diagonal_z(n, A, is_diagonal, tol)
     use constants, only : dp
     use constants, only : eps8

     implicit none

!! external arguments
     ! size of matrix (must be square)
     integer, intent(in)     :: n

     ! input matrix
     complex(dp), intent(in) :: A(n,n)

     ! output: .true. if matrix is diagonal, .false. otherwise
     logical, intent(out)    :: is_diagonal

     ! tolerance for floating point comparison (optional, default 1.0e-8)
     real(dp), intent(in), optional :: tol

!! local variables
     ! loop indices
     integer :: i, j

     ! actual tolerance value
     real(dp) :: actual_tol

!! [body

     ! set tolerance (use default if not provided)
     if ( present(tol) ) then
         actual_tol = tol
     else
         actual_tol = eps8
     endif ! back if ( present(tol) ) block

     ! initialize
     is_diagonal = .true.

     ! check all off-diagonal elements
     ! diagonal condition: A(i,j) = 0 for i /= j
      outer_loop: do i=1,n
         inner_loop: do j=1,n
             ! check if off-diagonal element is zero
             if ( i /= j .and. abs( A(i,j) ) > actual_tol ) then
                 is_diagonal = .false.
                 exit outer_loop
             endif ! back if ( i /= j .and. abs( A(i,j) ) > actual_tol ) block
         enddo inner_loop ! over j={1,n} loop (inner_loop)
     enddo outer_loop ! over i={1,n} loop (outer_loop)

!! body]

     return
  end subroutine s_is_diagonal_z

!!
!! @sub s_is_tridiagonal_d
!!
!! check if a real(dp) matrix is tridiagonal (only main diagonal and
!! adjacent diagonals can be non-zero).
!!
  subroutine s_is_tridiagonal_d(n, A, is_tridiagonal, tol)
     use constants, only : dp
     use constants, only : eps8

     implicit none

!! external arguments
     ! size of matrix (must be square)
     integer, intent(in)  :: n

     ! input matrix
     real(dp), intent(in) :: A(n,n)

     ! output: .true. if matrix is tridiagonal, .false. otherwise
     logical, intent(out) :: is_tridiagonal

     ! tolerance for floating point comparison (optional, default 1.0e-8)
     real(dp), intent(in), optional :: tol

!! local variables
     ! loop indices
     integer  :: i, j

     ! actual tolerance value
     real(dp) :: actual_tol

!! [body

     ! set tolerance (use default if not provided)
     if ( present(tol) ) then
         actual_tol = tol
     else
         actual_tol = eps8
     endif ! back if ( present(tol) ) block

     ! initialize
     is_tridiagonal = .true.

     ! check all elements except main diagonal and adjacent diagonals
     ! tridiagonal condition: A(i,j) = 0 for |i-j| > 1
     outer_loop: do i=1,n
         inner_loop: do j=1,n
             ! check if element outside tridiagonal band is zero
             if ( abs( i - j ) > 1 .and. abs( A(i,j) ) > actual_tol ) then
                 is_tridiagonal = .false.
                 exit outer_loop
             endif ! back if ( abs( i - j ) > 1 .and. abs( A(i,j) ) > actual_tol ) block
         enddo inner_loop ! over j={1,n} loop (inner_loop)
     enddo outer_loop ! over i={1,n} loop (outer_loop)

!! body]

     return
  end subroutine s_is_tridiagonal_d

!!
!! @sub s_is_tridiagonal_z
!!
!! check if a complex(dp) matrix is tridiagonal (only main diagonal and
!! adjacent diagonals can be non-zero).
!!
  subroutine s_is_tridiagonal_z(n, A, is_tridiagonal, tol)
     use constants, only : dp
     use constants, only : eps8

     implicit none

!! external arguments
     ! size of matrix (must be square)
     integer, intent(in)     :: n

     ! input matrix
     complex(dp), intent(in) :: A(n,n)

     ! output: .true. if matrix is tridiagonal, .false. otherwise
     logical, intent(out)    :: is_tridiagonal

     ! tolerance for floating point comparison (optional, default 1.0e-8)
     real(dp), intent(in), optional :: tol

!! local variables
     ! loop indices
     integer  :: i, j

     ! actual tolerance value
     real(dp) :: actual_tol

!! [body

     ! set tolerance (use default if not provided)
     if ( present(tol) ) then
         actual_tol = tol
     else
         actual_tol = eps8
     endif ! back if ( present(tol) ) block

     ! initialize
     is_tridiagonal = .true.

     ! check all elements except main diagonal and adjacent diagonals
     ! tridiagonal condition: A(i,j) = 0 for |i-j| > 1
     outer_loop: do i=1,n
         inner_loop: do j=1,n
             ! check if element outside tridiagonal band is zero
             if ( abs( i - j ) > 1 .and. abs( A(i,j) ) > actual_tol ) then
                 is_tridiagonal = .false.
                 exit outer_loop
             endif ! back if ( abs( i - j ) > 1 .and. abs( A(i,j) ) > actual_tol ) block
         enddo inner_loop ! over j={1,n} loop (inner_loop)
     enddo outer_loop ! over i={1,n} loop (outer_loop)

!! body]

     return
  end subroutine s_is_tridiagonal_z

!!
!! @sub s_qr_d
!!
!! perform QR decomposition for a general real(dp) m-by-n matrix A,
!! where A = Q * R, return orthogonal matrix Q and upper triangular R.
!!
  subroutine s_qr_d(m, n, min_mn, amat, qmat, rmat)
     use constants, only : dp
     use constants, only : zero

     implicit none

!! external arguments
     ! number of rows of A matrix
     integer, intent(in)     :: m

     ! number of columns of A matrix
     integer, intent(in)     :: n

     ! minimal value of m and n
     integer, intent(in)     :: min_mn

     ! A matrix, on entry contains original matrix,
     ! on exit destroyed and contains Householder vectors
     real(dp), intent(inout) :: amat(m,n)

     ! orthogonal matrix Q from QR decomposition
     real(dp), intent(out)   :: qmat(m,min_mn)

     ! upper triangular matrix R from QR decomposition
     real(dp), intent(out)   :: rmat(min_mn,n)

!! local variables
     ! loop index
     integer :: i, j

     ! status flag
     integer :: istat

     ! return information from lapack subroutines
     integer :: info

     ! length of work array, lwork >= max(1,n)
     integer :: lwork

     ! workspace array for lapack subroutines
     real(dp), allocatable :: work(:)

     ! scalar factors for elementary reflectors
     real(dp), allocatable :: tau(:)

!! [body

     ! initialize lwork
     lwork = 4*n

     ! allocate memory
     allocate(work(lwork), stat=istat)
     allocate(tau(min_mn), stat=istat)
     !
     if ( istat /= 0 ) then
         call s_print_error('s_qr_d','can not allocate enough memory')
     endif ! back if ( istat /= 0 ) block

     ! compute QR factorization of general m-by-n matrix
     ! on exit, amat contains the elementary reflectors and tau contains scalars
     call DGEQRF(m, n, amat, m, tau, work, lwork, info)
     !
     if ( info /= 0 ) then
         call s_print_error('s_qr_d','error in lapack subroutine dgeqrf')
     endif ! back if ( info /= 0 ) block

     ! extract R matrix from amat (upper triangular part)
     rmat = zero
     rmat(1:min_mn,1:n) = amat(1:min_mn,1:n)
     !
     ! zero out lower triangular part of R
     do i=1,min_mn
         do j=i+1,min_mn
             rmat(j,i) = zero
         enddo ! over j={i+1,min_mn} loop
     enddo ! over i={1,min_mn} loop

     ! generate orthogonal matrix Q from elementary reflectors
     qmat = amat(1:m,1:min_mn)
     call DORGQR(m, min_mn, min_mn, qmat, m, tau, work, lwork, info)
     !
     if ( info /= 0 ) then
         call s_print_error('s_qr_d','error in lapack subroutine dorgqr')
     endif ! back if ( info /= 0 ) block

     ! deallocate memory for workspace arrays
     if ( allocated(work) ) deallocate(work)
     if ( allocated(tau) ) deallocate(tau)

!! body]

     return
  end subroutine s_qr_d

!!
!! @sub s_qr_z
!!
!! perform QR decomposition for a general complex(dp) m-by-n matrix A,
!! where A = Q * R, return unitary matrix Q and upper triangular R.
!!
  subroutine s_qr_z(m, n, min_mn, amat, qmat, rmat)
     use constants, only : dp
     use constants, only : czero

     implicit none

!! external arguments
     ! number of rows of A matrix
     integer, intent(in)        :: m

     ! number of columns of A matrix
     integer, intent(in)        :: n

     ! minimal value of m and n
     integer, intent(in)        :: min_mn

     ! A matrix, on entry contains original matrix,
     ! on exit destroyed and contains Householder vectors
     complex(dp), intent(inout) :: amat(m,n)

     ! unitary matrix Q from QR decomposition
     complex(dp), intent(out)   :: qmat(m,min_mn)

     ! upper triangular matrix R from QR decomposition
     complex(dp), intent(out)   :: rmat(min_mn,n)

!! local variables
     ! loop index
     integer :: i, j

     ! status flag
     integer :: istat

     ! return information from lapack subroutines
     integer :: info

     ! length of work array, lwork >= max(1,n)
     integer :: lwork

     ! workspace array for lapack subroutines
     complex(dp), allocatable :: work(:)

     ! scalar factors for elementary reflectors
     complex(dp), allocatable :: tau(:)

!! [body

     ! initialize lwork
     lwork = 4*n

     ! allocate memory
     allocate(work(lwork), stat=istat)
     allocate(tau(min_mn), stat=istat)
     !
     if ( istat /= 0 ) then
         call s_print_error('s_qr_z','can not allocate enough memory')
     endif ! back if ( istat /= 0 ) block

     ! compute QR factorization of general m-by-n matrix
     ! on exit, amat contains the elementary reflectors and tau contains scalars
     call ZGEQRF(m, n, amat, m, tau, work, lwork, info)
     !
     if ( info /= 0 ) then
         call s_print_error('s_qr_z','error in lapack subroutine zgeqrf')
     endif ! back if ( info /= 0 ) block

     ! extract R matrix from amat (upper triangular part)
     rmat = czero
     !
     ! copy upper triangular part of R from amat
     do i=1,min_mn
         do j=i,n
             rmat(i,j) = amat(i,j)
         enddo ! over j={i,n} loop
     enddo ! over i={1,min_mn} loop

     ! generate unitary matrix Q from elementary reflectors
     qmat = amat(1:m,1:min_mn)
     call ZUNGQR(m, min_mn, min_mn, qmat, m, tau, work, lwork, info)
     !
     if ( info /= 0 ) then
         call s_print_error('s_qr_z','error in lapack subroutine zungqr')
     endif ! back if ( info /= 0 ) block

     ! deallocate memory for workspace arrays
     if ( allocated(work) ) deallocate(work)
     if ( allocated(tau) ) deallocate(tau)

!! body]

     return
  end subroutine s_qr_z

!!
!! @sub s_cholesky_d
!!
!! perform Cholesky decomposition for a real(dp) symmetric positive-definite
!! matrix A, where A = L * L^T, return lower triangular matrix L.
!!
  subroutine s_cholesky_d(n, A, L)
     use constants, only : dp
     use constants, only : zero

     implicit none

!! external arguments
     ! dimension of matrix (must be square)
     integer, intent(in)     :: n

     ! on entry, symmetric positive-definite matrix A;
     ! on exit, lower triangular Cholesky factor L in lower triangle
     real(dp), intent(inout) :: A(n,n)

     ! lower triangular Cholesky factor L (output only)
     real(dp), intent(out)   :: L(n,n)

!! local variables
     ! loop index
     integer :: i, j

     ! error flag
     integer :: ierror

!! [body

     ! copy A to L
     L = A

     ! compute Cholesky factorization of symmetric positive-definite matrix
     ! on exit, lower triangle of L contains Cholesky factor
     call DPOTRF('L', n, L, n, ierror)
     !
     if ( ierror /= 0 ) then
         call s_print_error('s_cholesky_d','error in lapack subroutine dpotrf')
     endif ! back if ( ierror /= 0 ) block

     ! zero out upper triangular part of L
     do i=1,n
         do j=i+1,n
             L(i,j) = zero
         enddo ! over j={i+1,n} loop
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_cholesky_d

!!
!! @sub s_cholesky_z
!!
!! perform Cholesky decomposition for a complex(dp) Hermitian positive-definite
!! matrix A, where A = L * L^H, return lower triangular matrix L.
!!
  subroutine s_cholesky_z(n, A, L)
     use constants, only : dp
     use constants, only : czero

     implicit none

!! external arguments
     ! dimension of matrix (must be square)
     integer, intent(in)        :: n

     ! on entry, Hermitian positive-definite matrix A;
     ! on exit, lower triangular Cholesky factor L in lower triangle
     complex(dp), intent(inout) :: A(n,n)

     ! lower triangular Cholesky factor L (output only)
     complex(dp), intent(out)   :: L(n,n)

!! local variables
     ! loop index
     integer :: i, j

     ! error flag
     integer :: ierror

!! [body

     ! copy A to L
     L = A

     ! compute Cholesky factorization of Hermitian positive-definite matrix
     ! on exit, lower triangle of L contains Cholesky factor
     call ZPOTRF('L', n, L, n, ierror)
     !
     if ( ierror /= 0 ) then
         call s_print_error('s_cholesky_z','error in lapack subroutine zpotrf')
     endif ! back if ( ierror /= 0 ) block

     ! zero out upper triangular part of L
     do i=1,n
         do j=i+1,n
             L(i,j) = czero
         enddo ! over j={i+1,n} loop
     enddo ! over i={1,n} loop

!! body]

      return
  end subroutine s_cholesky_z

!!
!! @sub s_lu_d
!!
!! perform LU decomposition for a general real(dp) matrix A,
!! where A = P * L * U, return lower triangular L and upper triangular U,
!! and permutation vector P (represented by ipiv).
!!
  subroutine s_lu_d(n, A, L, U, ipiv)
     use constants, only : dp
     use constants, only : zero, one

     implicit none

!! external arguments
     ! dimension of matrix (must be square)
     integer, intent(in)     :: n

     ! on entry, original matrix A;
     ! on exit, destroyed and contains L and U factors
     real(dp), intent(inout) :: A(n,n)

     ! lower triangular matrix L (output only)
     real(dp), intent(out)   :: L(n,n)

     ! upper triangular matrix U (output only)
     real(dp), intent(out)   :: U(n,n)

     ! pivot indices representing permutation P (output)
     integer, intent(out)    :: ipiv(n)

!! local variables
     ! loop index
     integer :: i

     ! error flag
     integer :: ierror

!! [body

     ! perform LU factorization
     call DGETRF(n, n, A, n, ipiv, ierror)
     !
     if ( ierror /= 0 ) then
         call s_print_error('s_lu_d','error in lapack subroutine dgetrf')
     endif ! back if ( ierror /= 0 ) block

     ! extract L matrix (lower triangular, unit diagonal)
     L = zero
     do i=1,n
         L(i,1:i-1) = A(i,1:i-1)
         L(i,i) = one
     enddo ! over i={1,n} loop

     ! extract U matrix (upper triangular)
     U = zero
     do i=1,n
         U(i,i:n) = A(i,i:n)
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_lu_d

!!
!! @sub s_lu_z
!!
!! perform LU decomposition for a general complex(dp) matrix A,
!! where A = P * L * U, return lower triangular L and upper triangular U,
!! and permutation vector P (represented by ipiv).
!!
  subroutine s_lu_z(n, A, L, U, ipiv)
     use constants, only : dp
     use constants, only : czero, cone

     implicit none

!! external arguments
     ! dimension of matrix (must be square)
     integer, intent(in)        :: n

     ! on entry, original matrix A;
     ! on exit, destroyed and contains L and U factors
     complex(dp), intent(inout) :: A(n,n)

     ! lower triangular matrix L (output only)
     complex(dp), intent(out)   :: L(n,n)

     ! upper triangular matrix U (output only)
     complex(dp), intent(out)   :: U(n,n)

     ! pivot indices representing permutation P (output)
     integer, intent(out)       :: ipiv(n)

!! local variables
     ! loop index
     integer :: i

     ! error flag
     integer :: ierror

!! [body

     ! perform LU factorization
     call ZGETRF(n, n, A, n, ipiv, ierror)
     !
     if ( ierror /= 0 ) then
         call s_print_error('s_lu_z','error in lapack subroutine zgetrf')
     endif ! back if ( ierror /= 0 ) block

     ! extract L matrix (lower triangular, unit diagonal)
     L = czero
     do i=1,n
         L(i,1:i-1) = A(i,1:i-1)
         L(i,i) = cone
     enddo ! over i={1,n} loop

     ! extract U matrix (upper triangular)
     U = czero
     do i=1,n
         U(i,i:n) = A(i,i:n)
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_lu_z

!!
!! @sub s_schur_d
!!
!! perform Schur decomposition for a general real(dp) matrix A,
!! where A = Q * T * Q^T, return orthogonal matrix Q and
!! block upper triangular Schur form T.
!!
  subroutine s_schur_d(ldim, n, A, T, Q)
     use constants, only : dp

     implicit none

!! external arguments
     ! leading dimension of matrix A
     integer, intent(in)     :: ldim

     ! order of the matrix A
     integer, intent(in)     :: n

     ! on entry, original general real(dp) matrix A;
     ! on exit, contains Schur form T in upper triangle
     real(dp), intent(inout) :: A(ldim,n)

     ! Schur form T (block upper triangular)
     real(dp), intent(out)   :: T(ldim,n)

     ! orthogonal matrix Q from Schur decomposition
     real(dp), intent(out)   :: Q(ldim,n)

!! local variables
     ! status flag
     integer :: istat

     ! return information from lapack subroutine dgees
     integer :: info

     ! length of the array work
     ! lwork >= max(1,3*n)
     integer :: lwork

     ! dimension of stable invariant subspace (output from dgees)
     integer :: sdim

     ! workspace array
     real(dp), allocatable :: work(:)

     ! auxiliary real(dp) matrix: real and imaginary parts of eigenvalues
     real(dp), allocatable :: wr(:)
     real(dp), allocatable :: wi(:)

     ! auxiliary real(dp) matrix: Schur vectors
     real(dp), allocatable :: vs(:,:)

     ! boolean array for workspace
     logical, allocatable  :: bwork(:)

!! [body

     ! initialize lwork
     lwork = 3 * n

     ! allocate memory
     allocate(work(lwork), stat=istat)
     allocate(wr(n),       stat=istat)
     allocate(wi(n),       stat=istat)
     allocate(vs(ldim,n),  stat=istat)
     allocate(bwork(n),    stat=istat)
     !
     if ( istat /= 0 ) then
         call s_print_error('s_schur_d','can not allocate enough memory')
     endif ! back if ( istat /= 0 ) block

     ! initialize output arrays
     vs = A

     ! perform Schur decomposition: A = Q * T * Q^T
     ! on exit, vs contains orthogonal matrix Q,
     ! and A contains Schur form T
     call DGEES('V', 'N', 'N', n, A, ldim, sdim, wr, wi, vs, ldim, &
               & work, lwork, bwork, info)
     !
     if ( info /= 0 ) then
         call s_print_error('s_schur_d','error in lapack subroutine dgees')
     endif ! back if ( info /= 0 ) block

     ! copy Schur form T and orthogonal matrix Q
     T(1:ldim,1:n) = A(1:ldim,1:n)
     Q(1:ldim,1:n) = vs(1:ldim,1:n)

     ! deallocate memory for workspace arrays
     if ( allocated(work ) ) deallocate(work )
     if ( allocated(wr   ) ) deallocate(wr   )
     if ( allocated(wi   ) ) deallocate(wi   )
     if ( allocated(vs   ) ) deallocate(vs   )
     if ( allocated(bwork) ) deallocate(bwork)

!! body]

     return
  end subroutine s_schur_d

!!
!! @sub s_schur_z
!!
!! perform Schur decomposition for a general complex(dp) matrix A,
!! where A = Q * T * Q^H, return unitary matrix Q and
!! upper triangular Schur form T.
!!
  subroutine s_schur_z(ldim, n, A, T, Q)
     use constants, only : dp

     implicit none

!! external arguments
     ! leading dimension of matrix A
     integer, intent(in)        :: ldim

     ! order of the matrix A
     integer, intent(in)        :: n

     ! on entry, original general complex(dp) matrix A;
     ! on exit, contains Schur form T in upper triangle
     complex(dp), intent(inout) :: A(ldim,n)

     ! Schur form T (upper triangular)
     complex(dp), intent(out)   :: T(ldim,n)

     ! unitary matrix Q from Schur decomposition
     complex(dp), intent(out)   :: Q(ldim,n)

!! local variables
     ! status flag
     integer :: istat

     ! return information from lapack subroutine zgees
     integer :: info

     ! length of the array work
     ! lwork >= max(1,2*n)
     integer :: lwork

     ! dimension of stable invariant subspace (output from zgees)
     integer :: sdim

     ! workspace array
     complex(dp), allocatable :: work(:)

     ! auxiliary real(dp) matrix
     real(dp), allocatable    :: rwork(:)

     ! auxiliary complex(dp) matrix: eigenvalues
     complex(dp), allocatable :: w(:)

     ! auxiliary complex(dp) matrix: Schur vectors
     complex(dp), allocatable :: vs(:,:)

     ! boolean array for workspace
     logical, allocatable     :: bwork(:)

!! [body

     ! initialize lwork
     lwork = 2 * n

     ! allocate memory
     allocate(work(lwork), stat=istat)
     allocate(rwork(n),    stat=istat)
     allocate(w(n),        stat=istat)
     allocate(vs(ldim,n),  stat=istat)
     allocate(bwork(n),    stat=istat)
     !
     if ( istat /= 0 ) then
         call s_print_error('s_schur_z','can not allocate enough memory')
     endif ! back if ( istat /= 0 ) block

     ! initialize output arrays
     vs = A

     ! perform Schur decomposition: A = Q * T * Q^H
     ! on exit, vs contains unitary matrix Q,
     ! and A contains Schur form T
     call ZGEES('V', 'N', 'N', n, A, ldim, sdim, w, vs, ldim, &
               & work, lwork, rwork, bwork, info)
     !
     if ( info /= 0 ) then
         call s_print_error('s_schur_z','error in lapack subroutine zgees')
     endif ! back if ( info /= 0 ) block

     ! copy Schur form T and unitary matrix Q
     T(1:ldim,1:n) = A(1:ldim,1:n)
     Q(1:ldim,1:n) = vs(1:ldim,1:n)

     ! deallocate memory for workspace arrays
     if ( allocated(work ) ) deallocate(work )
     if ( allocated(rwork) ) deallocate(rwork)
     if ( allocated(w    ) ) deallocate(w    )
     if ( allocated(vs   ) ) deallocate(vs   )
     if ( allocated(bwork) ) deallocate(bwork)

!! body]

      return
  end subroutine s_schur_z

!!
!! @sub s_cond_d
!!
!! estimate the condition number of a real(dp) matrix A in the
!! 1-norm, infinity norm, or Frobenius norm using LAPACK.
!!
  subroutine s_cond_d(n, A, cond, norm_type)
     use constants, only : dp
     use constants, only : zero, one

     implicit none

!! external arguments
     ! dimension of matrix (must be square)
     integer, intent(in)   :: n

     ! input matrix
     real(dp), intent(in)  :: A(n,n)

     ! estimated condition number
     real(dp), intent(out) :: cond

     ! norm type: '1' for 1-norm, 'I' for infinity norm, 'F' for Frobenius
     character(len=1), intent(in), optional :: norm_type

!! local variables
     ! actual norm type (default: 1-norm)
     character(len=1) :: actual_norm

     ! error flag
     integer  :: ierror

     ! norm of A
     real(dp) :: anorm

     ! reciprocal of condition number
     real(dp) :: rcond

     ! workspace arrays for lapack subroutines
     integer, allocatable  :: ipiv(:)
     real(dp), allocatable :: work(:)
     real(dp), allocatable :: iwork(:)
     real(dp), allocatable :: A_lu(:,:)

!! [body

     ! set norm type (use '1' if not provided)
     if ( present(norm_type) ) then
         actual_norm = norm_type
     else
         actual_norm = '1'
     endif ! back if ( present(norm_type) ) block

     ! allocate memory
     allocate(ipiv(n),   stat=ierror)
     allocate(work(4*n), stat=ierror)
     allocate(iwork(n),  stat=ierror)
     allocate(A_lu(n,n), stat=ierror)
     !
     if ( ierror /= 0 ) then
         call s_print_error('s_cond_d','can not allocate enough memory')
     endif ! back if ( ierror /= 0 ) block

     ! copy A to A_lu for LU factorization
     A_lu = A

     ! compute LU factorization
     call DGETRF(n, n, A_lu, n, ipiv, ierror)
     !
     if ( ierror /= 0 ) then
         call s_print_error('s_cond_d','error in lapack subroutine dgetrf')
     endif ! back if ( ierror /= 0 ) block

     ! compute norm of original matrix A
     if ( actual_norm == '1' ) then
         ! 1-norm: maximum absolute column sum
         anorm = maxval(sum(abs(A), dim=1))
     elseif ( actual_norm == 'I' .or. actual_norm == 'i' ) then
         ! infinity norm: maximum absolute row sum
         anorm = maxval(sum(abs(A), dim=2))
     elseif ( actual_norm == 'F' .or. actual_norm == 'f' ) then
         ! Frobenius norm: sqrt(sum of squares)
         anorm = sqrt(sum(A**2))
     endif

     ! estimate reciprocal condition number
     call DGECON(actual_norm, n, A_lu, n, anorm, rcond, work, iwork, ierror)
     !
     if ( ierror /= 0 ) then
         call s_print_error('s_cond_d','error in lapack subroutine dgecon')
     endif ! back if ( ierror /= 0 ) block

     ! compute condition number
     if ( rcond > zero ) then
         cond = one / rcond
     else
         ! matrix is singular or near-singular
         cond = huge(one)
     endif

     ! deallocate memory
     if ( allocated(ipiv)  ) deallocate(ipiv)
     if ( allocated(work)  ) deallocate(work)
     if ( allocated(iwork) ) deallocate(iwork)
     if ( allocated(A_lu)  ) deallocate(A_lu)

!! body]

     return
  end subroutine s_cond_d

!!
!! @sub s_cond_z
!!
!! estimate the condition number of a complex(dp) matrix A in the
!! 1-norm, infinity norm, or Frobenius norm using LAPACK.
!!
  subroutine s_cond_z(n, A, cond, norm_type)
     use constants, only : dp
     use constants, only : zero, one

     implicit none

!! external arguments
     ! dimension of matrix (must be square)
     integer, intent(in)      :: n

     ! input matrix
     complex(dp), intent(in)  :: A(n,n)

     ! estimated condition number
     real(dp), intent(out)    :: cond

     ! norm type: '1' for 1-norm, 'I' for infinity norm, 'F' for Frobenius
     character(len=1), intent(in), optional :: norm_type

!! local variables
     ! actual norm type (default: 1-norm)
     character(len=1) :: actual_norm

     ! error flag
     integer  :: ierror

     ! norm of A
     real(dp) :: anorm

     ! reciprocal of condition number
     real(dp) :: rcond

     ! workspace arrays for lapack subroutines
     integer, allocatable     :: ipiv(:)
     complex(dp), allocatable :: work(:)
     real(dp), allocatable    :: rwork(:)
     complex(dp), allocatable :: A_lu(:,:)

!! [body

     ! set norm type (use '1' if not provided)
     if ( present(norm_type) ) then
         actual_norm = norm_type
     else
         actual_norm = '1'
     endif ! back if ( present(norm_type) ) block

     ! allocate memory
     allocate(ipiv(n),    stat=ierror)
     allocate(work(2*n),  stat=ierror)
     allocate(rwork(2*n), stat=ierror)
     allocate(A_lu(n,n),  stat=ierror)
     !
     if ( ierror /= 0 ) then
         call s_print_error('s_cond_z','can not allocate enough memory')
     endif ! back if ( ierror /= 0 ) block

     ! copy A to A_lu for LU factorization
     A_lu = A

     ! compute LU factorization
     call ZGETRF(n, n, A_lu, n, ipiv, ierror)
     !
     if ( ierror /= 0 ) then
         call s_print_error('s_cond_z','error in lapack subroutine zgetrf')
     endif ! back if ( ierror /= 0 ) block

     ! compute norm of original matrix A
     if ( actual_norm == '1' ) then
         ! 1-norm: maximum absolute column sum
         anorm = maxval(sum(abs(A), dim=1))
     elseif ( actual_norm == 'I' .or. actual_norm == 'i' ) then
         ! infinity norm: maximum absolute row sum
         anorm = maxval(sum(abs(A), dim=2))
     elseif ( actual_norm == 'F' .or. actual_norm == 'f' ) then
         ! Frobenius norm: sqrt(sum of squares)
         anorm = sqrt(real(sum(conjg(A)*A)))
     endif

     ! estimate reciprocal condition number
     call ZGECON(actual_norm, n, A_lu, n, anorm, rcond, work, rwork, ierror)
     !
     if ( ierror /= 0 ) then
         call s_print_error('s_cond_z','error in lapack subroutine zgecon')
     endif ! back if ( ierror /= 0 ) block

     ! compute condition number
     if ( rcond > zero ) then
         cond = one / rcond
     else
         ! matrix is singular or near-singular
         cond = huge(one)
     endif

     ! deallocate memory
     if ( allocated(ipiv)  ) deallocate(ipiv)
     if ( allocated(work)  ) deallocate(work)
     if ( allocated(rwork) ) deallocate(rwork)
     if ( allocated(A_lu)  ) deallocate(A_lu)

!! body]

     return
  end subroutine s_cond_z

!!
!! @sub s_rank_d
!!
!! estimate the rank of a real(dp) m-by-n matrix A using SVD.
!! returns the number of singular values greater than tolerance.
!!
  subroutine s_rank_d(m, n, A, rank, tol)
     use constants, only : dp
     use constants, only : eps8

     implicit none

!! external arguments
     ! number of rows of A matrix
     integer, intent(in)     :: m

     ! number of columns of A matrix
     integer, intent(in)     :: n

     ! A matrix (will be destroyed by SVD)
     real(dp), intent(inout) :: A(m,n)

     ! estimated rank of matrix A
     integer, intent(out)    :: rank

     ! tolerance for singular value comparison (optional, default: 1.0e-8)
     real(dp), intent(in), optional :: tol

!! local variables
     ! status flag for allocation
     integer  :: istat

     ! minimal value of m and n
     integer  :: min_mn

     ! actual tolerance value
     real(dp) :: actual_tol

     ! maximum singular value
     real(dp) :: max_sval

     ! tolerance threshold
     real(dp) :: thresh

     ! SVD output arrays
     real(dp), allocatable :: umat(:,:)
     real(dp), allocatable :: svec(:)
     real(dp), allocatable :: vmat(:,:)

!! [body

     ! compute minimal dimension
     min_mn = min(m, n)

     ! set tolerance (use default if not provided)
     if ( present(tol) ) then
         actual_tol = tol
     else
         actual_tol = eps8
     endif ! back if ( present(tol) ) block

     ! allocate arrays for SVD
     allocate(umat(m,min_mn), stat=istat)
     allocate(svec(min_mn),   stat=istat)
     allocate(vmat(min_mn,n), stat=istat)
     !
     if ( istat /= 0 ) then
         call s_print_error('s_rank_d','can not allocate enough memory')
     endif ! back if ( istat /= 0 ) block

     ! compute SVD decomposition
     call s_svd_dg(m, n, min_mn, A, umat, svec, vmat)

     ! find maximum singular value
     max_sval = maxval(svec)

     ! compute threshold: max_sval * tol * max(m,n)
     thresh = max_sval * actual_tol * real(max(m,n), dp)

     ! count singular values greater than threshold
     rank = count(svec > thresh)

     ! deallocate arrays
     if ( allocated(umat) ) deallocate(umat)
     if ( allocated(svec) ) deallocate(svec)
     if ( allocated(vmat) ) deallocate(vmat)

!! body]

     return
  end subroutine s_rank_d

!!
!! @sub s_rank_z
!!
!! estimate the rank of a complex(dp) m-by-n matrix A using SVD.
!! returns the number of singular values greater than tolerance.
!!
  subroutine s_rank_z(m, n, A, rank, tol)
     use constants, only : dp
     use constants, only : eps8

     implicit none

!! external arguments
     ! number of rows of A matrix
     integer, intent(in)        :: m

     ! number of columns of A matrix
     integer, intent(in)        :: n

     ! A matrix (will be destroyed by SVD)
     complex(dp), intent(inout) :: A(m,n)

     ! estimated rank of matrix A
     integer, intent(out)       :: rank

     ! tolerance for singular value comparison (optional, default: 1.0e-8)
     real(dp), intent(in), optional :: tol

!! local variables
     ! status flag for allocation
     integer  :: istat

     ! minimal value of m and n
     integer  :: min_mn

     ! actual tolerance value
     real(dp) :: actual_tol

     ! maximum singular value
     real(dp) :: max_sval

     ! tolerance threshold
     real(dp) :: thresh

     ! SVD output arrays
     complex(dp), allocatable :: umat(:,:)
     real(dp), allocatable    :: svec(:)
     complex(dp), allocatable :: vmat(:,:)

!! [body

     ! compute minimal dimension
     min_mn = min(m, n)

     ! set tolerance (use default if not provided)
     if ( present(tol) ) then
         actual_tol = tol
     else
         actual_tol = eps8
     endif ! back if ( present(tol) ) block

     ! allocate arrays for SVD
     allocate(umat(m,min_mn), stat=istat)
     allocate(svec(min_mn),   stat=istat)
     allocate(vmat(min_mn,n), stat=istat)
     !
     if ( istat /= 0 ) then
         call s_print_error('s_rank_z','can not allocate enough memory')
     endif ! back if ( istat /= 0 ) block

     ! compute SVD decomposition
     call s_svd_zg(m, n, min_mn, A, umat, svec, vmat)

     ! find maximum singular value
     max_sval = maxval(svec)

     ! compute threshold: max_sval * tol * max(m,n)
     thresh = max_sval * actual_tol * real(max(m,n), dp)

     ! count singular values greater than threshold
     rank = count(svec > thresh)

     ! deallocate arrays
     if ( allocated(umat) ) deallocate(umat)
     if ( allocated(svec) ) deallocate(svec)
     if ( allocated(vmat) ) deallocate(vmat)

!! body]

     return
  end subroutine s_rank_z

!!
!! @sub s_extract_submatrix_d
!!
!! extract a submatrix from a real(dp) matrix A_src.
!! extracts rows i_start:i_end and columns j_start:j_end.
!!
  subroutine s_extract_submatrix_d(m_src, n_src, A_src, &
                                  i_start, j_start, &
                                  m_sub, n_sub, A_sub)
     use constants, only : dp

     implicit none

!! external arguments
     ! number of rows of source matrix
     integer, intent(in)   :: m_src

     ! number of columns of source matrix
     integer, intent(in)   :: n_src

     ! source matrix
     real(dp), intent(in)  :: A_src(m_src,n_src)

     ! start row index (1-based)
     integer, intent(in)   :: i_start

     ! start column index (1-based)
     integer, intent(in)   :: j_start

     ! number of rows of submatrix
     integer, intent(in)   :: m_sub

     ! number of columns of submatrix
     integer, intent(in)   :: n_sub

     ! extracted submatrix
     real(dp), intent(out) :: A_sub(m_sub,n_sub)

!! [body

     ! check boundary conditions
     if ( i_start < 1 .or. i_start > m_src ) then
         call s_print_error('s_extract_submatrix_d','i_start is out of bounds')
     endif ! back if ( i_start < 1 .or. i_start > m_src ) block

     if ( j_start < 1 .or. j_start > n_src ) then
         call s_print_error('s_extract_submatrix_d','j_start is out of bounds')
     endif ! back if ( j_start < 1 .or. j_start > n_src ) block

     if ( i_start + m_sub - 1 > m_src ) then
         call s_print_error('s_extract_submatrix_d','submatrix rows exceed source matrix bounds')
     endif ! back if ( i_start + m_sub - 1 > m_src ) block

     if ( j_start + n_sub - 1 > n_src ) then
         call s_print_error('s_extract_submatrix_d','submatrix columns exceed source matrix bounds')
     endif ! back if ( j_start + n_sub - 1 > n_src ) block

     ! use array slicing for efficient copy
     A_sub = A_src(i_start:i_start+m_sub-1, j_start:j_start+n_sub-1)

!! body]

     return
  end subroutine s_extract_submatrix_d

!!
!! @sub s_extract_submatrix_z
!!
!! extract a submatrix from a complex(dp) matrix A_src.
!! extracts rows i_start:i_end and columns j_start:j_end.
!!
  subroutine s_extract_submatrix_z(m_src, n_src, A_src, &
                                  i_start, j_start, &
                                  m_sub, n_sub, A_sub)
     use constants, only : dp

     implicit none

!! external arguments
     ! number of rows of source matrix
     integer, intent(in)      :: m_src

     ! number of columns of source matrix
     integer, intent(in)      :: n_src

     ! source matrix
     complex(dp), intent(in)  :: A_src(m_src,n_src)

     ! start row index (1-based)
     integer, intent(in)      :: i_start

     ! start column index (1-based)
     integer, intent(in)      :: j_start

     ! number of rows of submatrix
     integer, intent(in)      :: m_sub

     ! number of columns of submatrix
     integer, intent(in)      :: n_sub

     ! extracted submatrix
     complex(dp), intent(out) :: A_sub(m_sub,n_sub)

!! [body

     ! check boundary conditions
     if ( i_start < 1 .or. i_start > m_src ) then
         call s_print_error('s_extract_submatrix_z','i_start is out of bounds')
     endif ! back if ( i_start < 1 .or. i_start > m_src ) block

     if ( j_start < 1 .or. j_start > n_src ) then
         call s_print_error('s_extract_submatrix_z','j_start is out of bounds')
     endif ! back if ( j_start < 1 .or. j_start > n_src ) block

     if ( i_start + m_sub - 1 > m_src ) then
         call s_print_error('s_extract_submatrix_z','submatrix rows exceed source matrix bounds')
     endif ! back if ( i_start + m_sub - 1 > m_src ) block

     if ( j_start + n_sub - 1 > n_src ) then
         call s_print_error('s_extract_submatrix_z','submatrix columns exceed source matrix bounds')
     endif ! back if ( j_start + n_sub - 1 > n_src ) block

     ! use array slicing for efficient copy
     A_sub = A_src(i_start:i_start+m_sub-1, j_start:j_start+n_sub-1)

!! body]

     return
  end subroutine s_extract_submatrix_z

!!
!! @sub s_concat_horiz_d
!!
!! concatenate two real(dp) matrices horizontally: C = [A | B].
!! matrices A and B must have the same number of rows.
!!
  subroutine s_concat_horiz_d(m_A, n_A, A, m_B, n_B, B, m_C, n_C, C)
     use constants, only : dp

     implicit none

!! external arguments
     ! number of rows of matrix A
     integer, intent(in)   :: m_A

     ! number of columns of matrix A
     integer, intent(in)   :: n_A

     ! first input matrix
     real(dp), intent(in)  :: A(m_A,n_A)

     ! number of rows of matrix B
     integer, intent(in)   :: m_B

     ! number of columns of matrix B
     integer, intent(in)   :: n_B

     ! second input matrix
     real(dp), intent(in)  :: B(m_B,n_B)

     ! number of rows of concatenated matrix C
     integer, intent(in)   :: m_C

     ! number of columns of concatenated matrix C
     integer, intent(in)   :: n_C

     ! concatenated matrix [A | B]
     real(dp), intent(out) :: C(m_C,n_C)

!! [body

     ! check boundary conditions
     if ( m_A /= m_B ) then
         call s_print_error('s_concat_horiz_d','matrices A and B must have the same number of rows')
     endif ! back if ( m_A /= m_B ) block

     if ( n_C /= n_A + n_B ) then
         call s_print_error('s_concat_horiz_d','n_C must equal n_A + n_B')
     endif ! back if ( n_C /= n_A + n_B ) block

     if ( m_C /= m_A ) then
         call s_print_error('s_concat_horiz_d','m_C must equal m_A')
     endif ! back if ( m_C /= m_A ) block

     ! use array slicing for efficient copy
     C(1:m_A, 1:n_A) = A(1:m_A, 1:n_A)
     C(1:m_B, n_A+1:n_A+n_B) = B(1:m_B, 1:n_B)

!! body]

     return
  end subroutine s_concat_horiz_d

!!
!! @sub s_concat_horiz_z
!!
!! concatenate two complex(dp) matrices horizontally: C = [A | B].
!! matrices A and B must have the same number of rows.
!!
  subroutine s_concat_horiz_z(m_A, n_A, A, m_B, n_B, B, m_C, n_C, C)
     use constants, only : dp

     implicit none

!! external arguments
     ! number of rows of matrix A
     integer, intent(in)      :: m_A

     ! number of columns of matrix A
     integer, intent(in)      :: n_A

     ! first input matrix
     complex(dp), intent(in)  :: A(m_A,n_A)

     ! number of rows of matrix B
     integer, intent(in)      :: m_B

     ! number of columns of matrix B
     integer, intent(in)      :: n_B

     ! second input matrix
     complex(dp), intent(in)  :: B(m_B,n_B)

     ! number of rows of concatenated matrix C
     integer, intent(in)      :: m_C

     ! number of columns of concatenated matrix C
     integer, intent(in)      :: n_C

     ! concatenated matrix [A | B]
     complex(dp), intent(out) :: C(m_C,n_C)

!! [body

     ! check boundary conditions
     if ( m_A /= m_B ) then
         call s_print_error('s_concat_horiz_z','matrices A and B must have the same number of rows')
     endif ! back if ( m_A /= m_B ) block

     if ( n_C /= n_A + n_B ) then
         call s_print_error('s_concat_horiz_z','n_C must equal n_A + n_B')
     endif ! back if ( n_C /= n_A + n_B ) block

     if ( m_C /= m_A ) then
         call s_print_error('s_concat_horiz_z','m_C must equal m_A')
     endif ! back if ( m_C /= m_A ) block

     ! use array slicing for efficient copy
     C(1:m_A, 1:n_A) = A(1:m_A, 1:n_A)
     C(1:m_B, n_A+1:n_A+n_B) = B(1:m_B, 1:n_B)

!! body]

     return
  end subroutine s_concat_horiz_z

!!
!! @sub s_concat_vert_d
!!
!! concatenate two real(dp) matrices vertically: C = [A; B].
!! matrices A and B must have the same number of columns.
!!
  subroutine s_concat_vert_d(m_A, n_A, A, m_B, n_B, B, m_C, n_C, C)
     use constants, only : dp

     implicit none

!! external arguments
     ! number of rows of matrix A
     integer, intent(in)   :: m_A

     ! number of columns of matrix A
     integer, intent(in)   :: n_A

     ! first input matrix
     real(dp), intent(in)  :: A(m_A,n_A)

     ! number of rows of matrix B
     integer, intent(in)   :: m_B

     ! number of columns of matrix B
     integer, intent(in)   :: n_B

     ! second input matrix
     real(dp), intent(in)  :: B(m_B,n_B)

     ! number of rows of concatenated matrix C
     integer, intent(in)   :: m_C

     ! number of columns of concatenated matrix C
     integer, intent(in)   :: n_C

     ! concatenated matrix [A; B]
     real(dp), intent(out) :: C(m_C,n_C)

!! [body

     ! check boundary conditions
     if ( n_A /= n_B ) then
         call s_print_error('s_concat_vert_d','matrices A and B must have the same number of columns')
     endif ! back if ( n_A /= n_B ) block

     if ( m_C /= m_A + m_B ) then
         call s_print_error('s_concat_vert_d','m_C must equal m_A + m_B')
     endif ! back if ( m_C /= m_A + m_B ) block

     if ( n_C /= n_A ) then
         call s_print_error('s_concat_vert_d','n_C must equal n_A')
     endif ! back if ( n_C /= n_A ) block

     ! use array slicing for efficient copy
     C(1:m_A, 1:n_A) = A(1:m_A, 1:n_A)
     C(m_A+1:m_A+m_B, 1:n_B) = B(1:m_B, 1:n_B)

!! body]

     return
  end subroutine s_concat_vert_d

!!
!! @sub s_concat_vert_z
!!
!! concatenate two complex(dp) matrices vertically: C = [A; B].
!! matrices A and B must have the same number of columns.
!!
  subroutine s_concat_vert_z(m_A, n_A, A, m_B, n_B, B, m_C, n_C, C)
     use constants, only : dp

     implicit none

!! external arguments
     ! number of rows of matrix A
     integer, intent(in)      :: m_A

     ! number of columns of matrix A
     integer, intent(in)      :: n_A

     ! first input matrix
     complex(dp), intent(in)  :: A(m_A,n_A)

     ! number of rows of matrix B
     integer, intent(in)      :: m_B

     ! number of columns of matrix B
     integer, intent(in)      :: n_B

     ! second input matrix
     complex(dp), intent(in)  :: B(m_B,n_B)

     ! number of rows of concatenated matrix C
     integer, intent(in)      :: m_C

     ! number of columns of concatenated matrix C
     integer, intent(in)      :: n_C

     ! concatenated matrix [A; B]
     complex(dp), intent(out) :: C(m_C,n_C)

!! [body

     ! check boundary conditions
     if ( n_A /= n_B ) then
         call s_print_error('s_concat_vert_z','matrices A and B must have the same number of columns')
     endif ! back if ( n_A /= n_B ) block

     if ( m_C /= m_A + m_B ) then
         call s_print_error('s_concat_vert_z','m_C must equal m_A + m_B')
     endif ! back if ( m_C /= m_A + m_B ) block

     if ( n_C /= n_A ) then
         call s_print_error('s_concat_vert_z','n_C must equal n_A')
     endif ! back if ( n_C /= n_A ) block

     ! use array slicing for efficient copy
     C(1:m_A, 1:n_A) = A(1:m_A, 1:n_A)
     C(m_A+1:m_A+m_B, 1:n_B) = B(1:m_B, 1:n_B)

!! body]

     return
  end subroutine s_concat_vert_z

!!
!! @sub s_geig_sy
!!
!! solve generalized eigenvalue problem A*x = lambda*B*x,
!! where A and B are real(dp) symmetric matrices.
!! returns eigenvalues and eigenvectors.
!!
  subroutine s_geig_sy(ldim, n, A, B, eval, evec)
     use constants, only : dp
     use constants, only : zero

     implicit none

!! external arguments
     ! leading dimension of matrices A and B
     integer, intent(in)   :: ldim

     ! the order of the matrices A and B
     integer, intent(in)   :: n

     ! original matrix A, symmetric
     real(dp), intent(in)  :: A(ldim,n)

     ! original matrix B, symmetric positive-definite
     real(dp), intent(in)  :: B(ldim,n)

     ! output: eigenvalues in ascending order
     real(dp), intent(out) :: eval(n)

     ! output: orthonormal eigenvectors
     real(dp), intent(out) :: evec(ldim,n)

!! local variables
     ! status flag
     integer :: istat

     ! return information from subroutine dsygv
     integer :: info

     ! length of the array work
     integer :: lwork

     ! workspace array
     real(dp), allocatable :: work(:)

     ! auxiliary matrix for LAPACK: will be destroyed
     real(dp), allocatable :: amat(:,:)
     real(dp), allocatable :: bmat(:,:)

!! [body

     ! initialize lwork: lwork >= max(1,3*n)
     lwork = 4 * n

     ! allocate memory
     allocate(work(lwork),  stat=istat)
     allocate(amat(ldim,n), stat=istat)
     allocate(bmat(ldim,n), stat=istat)
     !
     if ( istat /= 0 ) then
         call s_print_error('s_geig_sy','can not allocate enough memory')
     endif ! back if ( istat /= 0 ) block

     ! copy A and B to work matrices
     amat = A
     bmat = B

     ! initialize output arrays
     eval = zero
     evec = amat

     ! call LAPACK subroutine: dsygv
     ! ITYPE=1: solve A*x = lambda*B*x
     ! JOBZ='V': compute eigenvalues and eigenvectors
     ! UPLO='U': use upper triangle
     call DSYGV(1, 'V', 'U', n, amat, ldim, bmat, ldim, eval, work, lwork, info)

     ! check the status
     if ( info /= 0 ) then
         call s_print_error('s_geig_sy','error in lapack subroutine dsygv')
     endif ! back if ( info /= 0 ) block

     ! copy eigenvectors from amat to output
     evec = amat

     ! deallocate memory for workspace array
     if ( allocated(work) ) deallocate(work)
     if ( allocated(amat) ) deallocate(amat)
     if ( allocated(bmat) ) deallocate(bmat)

!! body]

     return
  end subroutine s_geig_sy

!!
!! @sub s_geig_he
!!
!! solve generalized eigenvalue problem A*x = lambda*B*x,
!! where A and B are complex(dp) Hermitian matrices.
!! returns eigenvalues and unitary eigenvectors.
!!
  subroutine s_geig_he(ldim, n, A, B, eval, evec)
     use constants, only : dp
     use constants, only : zero

     implicit none

!! external arguments
     ! leading dimension of matrices A and B
     integer, intent(in)      :: ldim

     ! order of the matrices A and B
     integer, intent(in)      :: n

     ! original matrix A, Hermitian
     complex(dp), intent(in)  :: A(ldim,n)

     ! original matrix B, Hermitian positive-definite
     complex(dp), intent(in)  :: B(ldim,n)

     ! output: eigenvalues in ascending order
     real(dp), intent(out)    :: eval(n)

     ! output: unitary eigenvectors
     complex(dp), intent(out) :: evec(ldim,n)

!! local variables
     ! status flag
     integer :: istat

     ! return information from subroutine zhegv
     integer :: info

     ! length of the array work
     integer :: lwork

     ! workspace array
     real(dp), allocatable    :: rwork(:)
     complex(dp), allocatable :: work(:)

     ! auxiliary matrices for LAPACK: will be destroyed
     complex(dp), allocatable :: amat(:,:)
     complex(dp), allocatable :: bmat(:,:)

!! [body

     ! initialize lwork: lwork >= max(1,2*n)
     lwork = 4 * n

     ! allocate memory
     allocate(rwork(3*n),   stat=istat)
     allocate(work(lwork),  stat=istat)
     allocate(amat(ldim,n), stat=istat)
     allocate(bmat(ldim,n), stat=istat)
     !
     if ( istat /= 0 ) then
         call s_print_error('s_geig_he','can not allocate enough memory')
     endif ! back if ( istat /= 0 ) block

     ! copy A and B to work matrices
     amat = A
     bmat = B

     ! initialize output arrays
     eval = zero
     evec = amat

     ! call LAPACK subroutine: zhegv
     ! ITYPE=1: solve A*x = lambda*B*x
     ! JOBZ='V': compute eigenvalues and eigenvectors
     ! UPLO='U': use upper triangle
     call ZHEGV(1, 'V', 'U', n, amat, ldim, bmat, ldim, eval, work, lwork, rwork, info)

     ! check the status
     if ( info /= 0 ) then
         call s_print_error('s_geig_he','error in lapack subroutine zhegv')
     endif ! back if ( info /= 0 ) block

     ! copy eigenvectors from amat to output
     evec = amat

     ! deallocate memory for workspace arrays
     if ( allocated(work ) ) deallocate(work )
     if ( allocated(rwork) ) deallocate(rwork)
     if ( allocated(amat) ) deallocate(amat)
     if ( allocated(bmat) ) deallocate(bmat)

!! body]

     return
  end subroutine s_geig_he

!!
!! @sub s_pinv_d
!!
!! compute Moore-Penrose pseudo-inverse of a general real(dp) m-by-n
!! matrix A using SVD decomposition, where pinv(A) = V * SIGMA^+ * U^T.
!!
  subroutine s_pinv_d(m, n, A, pinv, tol)
     use constants, only : dp
     use constants, only : zero, one, eps8

     implicit none

!! external arguments
     ! number of rows of A matrix
     integer, intent(in)   :: m

     ! number of columns of A matrix
     integer, intent(in)   :: n

     ! A matrix
     real(dp), intent(in)  :: A(m,n)

     ! pseudo-inverse matrix (n-by-m)
     real(dp), intent(out) :: pinv(n,m)

     ! tolerance for singular value cutoff (optional, default: 1.0e-12)
     real(dp), intent(in), optional :: tol

!! local variables
     ! status flag
     integer  :: istat

     ! minimal value of m and n
     integer  :: min_mn

     ! loop index
     integer  :: i

     ! actual tolerance value
     real(dp) :: actual_tol

     ! SVD output arrays
     real(dp), allocatable :: umat(:,:)
     real(dp), allocatable :: svec(:)
     real(dp), allocatable :: vmat(:,:)

     ! temporary matrices for pseudo-inverse computation
     real(dp), allocatable :: sigma_pinv(:,:)
     real(dp), allocatable :: temp1(:,:)

!! [body

     ! calculate min_mn
     min_mn = min(m, n)

     ! allocate arrays for SVD
     allocate(umat(m,min_mn), stat=istat)
     allocate(svec(min_mn), stat=istat)
     allocate(vmat(min_mn,n), stat=istat)
     allocate(sigma_pinv(min_mn,min_mn), stat=istat)
     allocate(temp1(min_mn,m), stat=istat)
     !
     if ( istat /= 0 ) then
         call s_print_error('s_pinv_d','can not allocate enough memory')
     endif ! back if ( istat /= 0 ) block

     ! set tolerance (use default if not provided)
     if ( present(tol) ) then
         actual_tol = tol
     else
         actual_tol = eps8
     endif ! back if ( present(tol) ) block

     ! compute SVD decomposition
     call s_svd_dg(m, n, min_mn, A, umat, svec, vmat)

     ! compute SIGMA^+: reciprocal of singular values with cutoff
     sigma_pinv = zero
     do i=1,min_mn
         if ( svec(i) > actual_tol ) then
             sigma_pinv(i,i) = one / svec(i)
         endif ! back if ( svec(i) > actual_tol ) block
     enddo ! over i={1,min_mn} loop

     ! compute pinv(A) = V * SIGMA^+ * U^T
     !
     ! temp1 = SIGMA^+ * U^T
     temp1 = matmul(sigma_pinv, transpose(umat))
     !
     ! pinv = V * temp1
     pinv = matmul(vmat, temp1)

     ! deallocate arrays
     if ( allocated(umat) ) deallocate(umat)
     if ( allocated(svec) ) deallocate(svec)
     if ( allocated(vmat) ) deallocate(vmat)
     if ( allocated(sigma_pinv) ) deallocate(sigma_pinv)
     if ( allocated(temp1) ) deallocate(temp1)

!! body]

     return
  end subroutine s_pinv_d

!!
!! @sub s_pinv_z
!!
!! compute Moore-Penrose pseudo-inverse of a general complex(dp) m-by-n
!! matrix A using SVD decomposition, where pinv(A) = V * SIGMA^+ * U^H.
!!
  subroutine s_pinv_z(m, n, A, pinv, tol)
     use constants, only : dp
     use constants, only : zero, one, eps8

     implicit none

!! external arguments
     ! number of rows of A matrix
     integer, intent(in)      :: m

     ! number of columns of A matrix
     integer, intent(in)      :: n

     ! A matrix
     complex(dp), intent(in)  :: A(m,n)

     ! pseudo-inverse matrix (n-by-m)
     complex(dp), intent(out) :: pinv(n,m)

     ! tolerance for singular value cutoff (optional, default: 1.0e-12)
     real(dp), intent(in), optional :: tol

!! local variables
     ! status flag
     integer  :: istat

     ! minimal value of m and n
     integer  :: min_mn

     ! loop index
     integer  :: i

     ! actual tolerance value
     real(dp) :: actual_tol

     ! SVD output arrays
     complex(dp), allocatable :: umat(:,:)
     real(dp), allocatable    :: svec(:)
     complex(dp), allocatable :: vmat(:,:)

     ! temporary matrices for pseudo-inverse computation
     real(dp), allocatable    :: sigma_pinv(:,:)
     complex(dp), allocatable :: temp1(:,:)

!! [body

     ! calculate min_mn
     min_mn = min(m, n)

     ! allocate arrays for SVD
     allocate(umat(m,min_mn), stat=istat)
     allocate(svec(min_mn), stat=istat)
     allocate(vmat(min_mn,n), stat=istat)
     allocate(sigma_pinv(min_mn,min_mn), stat=istat)
     allocate(temp1(min_mn,m), stat=istat)
     !
     if ( istat /= 0 ) then
         call s_print_error('s_pinv_z','can not allocate enough memory')
     endif ! back if ( istat /= 0 ) block

     ! set tolerance (use default if not provided)
     if ( present(tol) ) then
         actual_tol = tol
     else
         actual_tol = eps8
     endif ! back if ( present(tol) ) block

     ! compute SVD decomposition
     call s_svd_zg(m, n, min_mn, A, umat, svec, vmat)

     ! compute SIGMA^+: reciprocal of singular values with cutoff
     sigma_pinv = zero
     do i=1,min_mn
         if ( svec(i) > actual_tol ) then
             sigma_pinv(i,i) = one / svec(i)
         endif ! back if ( svec(i) > actual_tol ) block
     enddo ! over i={1,min_mn} loop

     ! compute pinv(A) = V * SIGMA^+ * U^H
     !
     ! temp1 = SIGMA^+ * U^H
     temp1 = matmul(sigma_pinv, conjg(transpose(umat)))
     !
     ! pinv = V * temp1
     pinv = matmul(vmat, temp1)

     ! deallocate arrays
     if ( allocated(umat) ) deallocate(umat)
     if ( allocated(svec) ) deallocate(svec)
     if ( allocated(vmat) ) deallocate(vmat)
     if ( allocated(sigma_pinv) ) deallocate(sigma_pinv)
     if ( allocated(temp1) ) deallocate(temp1)

!! body]

     return
  end subroutine s_pinv_z

!!
!! @sub s_is_skew_symmetric_d
!!
!! check if a real(dp) matrix is skew-symmetric (A = -A^T).
!!
  subroutine s_is_skew_symmetric_d(n, A, is_skew_symmetric, tol)
     use constants, only : dp
     use constants, only : eps8

     implicit none

!! external arguments
     ! size of matrix (must be square)
     integer, intent(in)  :: n

     ! input matrix
     real(dp), intent(in) :: A(n,n)

     ! output: .true. if matrix is skew-symmetric, .false. otherwise
     logical, intent(out) :: is_skew_symmetric

     ! tolerance for floating point comparison (optional, default 1.0e-8)
     real(dp), intent(in), optional :: tol

!! local variables
     ! loop indices
     integer  :: i, j

     ! actual tolerance value
     real(dp) :: actual_tol

!! [body

     ! set tolerance (use default if not provided)
     if ( present(tol) ) then
         actual_tol = tol
     else
         actual_tol = eps8
     endif ! back if ( present(tol) ) block

     ! initialize
     is_skew_symmetric = .true.

     ! compare upper triangular part with lower triangular part
     ! skew-symmetric condition: A(i,j) = -A(j,i)
     ! we only need to compare i > j (lower triangle) with (j,i) (upper triangle)
     outer_loop: do i=2,n
         inner_loop: do j=1,i-1
             ! check skew-symmetric condition with tolerance
             if ( abs( A(i,j) + A(j,i) ) > actual_tol ) then
                 is_skew_symmetric = .false.
                 exit outer_loop
             endif ! back if ( abs( A(i,j) + A(j,i) ) > actual_tol ) block
         enddo inner_loop ! over j={1,i-1} loop (inner_loop)
     enddo outer_loop ! over i={2,n} loop (outer_loop)

!! body]

     return
  end subroutine s_is_skew_symmetric_d

!!
!! @sub s_is_skew_hermitian_z
!!
!! check if a complex(dp) matrix is skew-Hermitian (A = -A^H).
!!
  subroutine s_is_skew_hermitian_z(n, A, is_skew_hermitian, tol)
     use constants, only : dp
     use constants, only : eps8

     implicit none

!! external arguments
     ! size of matrix (must be square)
     integer, intent(in)     :: n

     ! input matrix
     complex(dp), intent(in) :: A(n,n)

     ! output: .true. if matrix is skew-Hermitian, .false. otherwise
     logical, intent(out)    :: is_skew_hermitian

     ! tolerance for floating point comparison (optional, default 1.0e-8)
     real(dp), intent(in), optional :: tol

!! local variables
     ! loop indices
     integer  :: i, j

     ! actual tolerance value
     real(dp) :: actual_tol

!! [body

     ! set tolerance (use default if not provided)
     if ( present(tol) ) then
         actual_tol = tol
     else
         actual_tol = eps8
     endif ! back if ( present(tol) ) block

     ! initialize
     is_skew_hermitian = .true.

     ! compare upper triangular part with lower triangular part
     ! skew-Hermitian condition: A(i,j) = -conj(A(j,i))
     ! we only need to compare i > j (lower triangle) with (j,i) (upper triangle)
     outer_loop: do i=2,n
         inner_loop: do j=1,i-1
             ! check skew-Hermitian condition with tolerance
             if ( abs( A(i,j) + conjg( A(j,i) ) ) > actual_tol ) then
                 is_skew_hermitian = .false.
                 exit outer_loop
             endif ! back if ( abs( A(i,j) + conj( A(j,i) ) ) > actual_tol ) block
         enddo inner_loop ! over j={1,i-1} loop (inner_loop)
     enddo outer_loop ! over i={2,n} loop (outer_loop)

!! body]

     return
  end subroutine s_is_skew_hermitian_z

!!
!! @sub s_is_positive_definite_d
!!
!! check if a real(dp) symmetric matrix is positive-definite
!! using Cholesky decomposition (A = L * L^T).
!!
  subroutine s_is_positive_definite_d(n, A, is_positive_definite)
     use constants, only : dp

     implicit none

!! external arguments
     ! dimension of matrix (must be square)
     integer, intent(in)  :: n

     ! input matrix (must be symmetric)
     real(dp), intent(in) :: A(n,n)

     ! output: .true. if matrix is symmetric positive-definite, .false. otherwise
     logical, intent(out) :: is_positive_definite

!! local variables
     ! error flag
     integer :: ierror

     ! Cholesky factor (temporary)
     real(dp), allocatable :: L(:,:)

!! [body

     ! allocate temporary matrix for Cholesky factorization
     allocate(L(n,n), stat=ierror)
     !
     if ( ierror /= 0 ) then
         call s_print_error('s_is_positive_definite_d','can not allocate enough memory')
     endif ! back if ( ierror /= 0 ) block

     ! copy A to L
     L = A

     ! attempt Cholesky decomposition using LAPACK subroutine dpotrf
     ! on exit, lower triangle of L contains Cholesky factor
     call DPOTRF('L', n, L, n, ierror)
     !
     if ( ierror == 0 ) then
         ! Cholesky decomposition succeeded: matrix is positive-definite
         is_positive_definite = .true.
     else
         ! Cholesky decomposition failed: matrix is not positive-definite
         ! (either not symmetric, not positive-definite, or numerically singular)
         is_positive_definite = .false.
     endif ! back if ( ierror == 0 ) block

     ! deallocate temporary matrix
     if ( allocated(L) ) deallocate(L)

!! body]

     return
  end subroutine s_is_positive_definite_d

!!
!! @sub s_is_positive_definite_z
!!
!! check if a complex(dp) Hermitian matrix is positive-definite
!! using Cholesky decomposition (A = L * L^H).
!!
  subroutine s_is_positive_definite_z(n, A, is_positive_definite)
     use constants, only : dp

     implicit none

!! external arguments
     ! dimension of matrix (must be square)
     integer, intent(in)     :: n

     ! input matrix (must be Hermitian)
     complex(dp), intent(in) :: A(n,n)

     ! output: .true. if matrix is Hermitian positive-definite, .false. otherwise
     logical, intent(out)    :: is_positive_definite

!! local variables
     ! error flag
     integer :: ierror

     ! Cholesky factor (temporary)
     complex(dp), allocatable :: L(:,:)

!! [body

     ! allocate temporary matrix for Cholesky factorization
     allocate(L(n,n), stat=ierror)
     !
     if ( ierror /= 0 ) then
         call s_print_error('s_is_positive_definite_z','can not allocate enough memory')
     endif ! back if ( ierror /= 0 ) block

     ! copy A to L
     L = A

     ! attempt Cholesky decomposition using LAPACK subroutine zpotrf
     ! on exit, lower triangle of L contains Cholesky factor
     call ZPOTRF('L', n, L, n, ierror)
     !
     if ( ierror == 0 ) then
         ! Cholesky decomposition succeeded: matrix is positive-definite
         is_positive_definite = .true.
     else
         ! Cholesky decomposition failed: matrix is not positive-definite
         ! (either not Hermitian, not positive-definite, or numerically singular)
         is_positive_definite = .false.
     endif ! back if ( ierror == 0 ) block

     ! deallocate temporary matrix
     if ( allocated(L) ) deallocate(L)

!! body]

     return
  end subroutine s_is_positive_definite_z

!!
!! @sub s_is_positive_semidefinite_d
!!
!! check if a real(dp) symmetric matrix is positive-semidefinite.
!! all eigenvalues >= 0 (with tolerance).
!!
  subroutine s_is_positive_semidefinite_d(ldim, n, A, is_positive_semidefinite, tol)
     use constants, only : dp
     use constants, only : eps8

     implicit none

!! external arguments
     ! leading dimension of matrix A
     integer, intent(in)  :: ldim

     ! order of the matrix A
     integer, intent(in)  :: n

     ! input matrix (must be symmetric)
     real(dp), intent(in) :: A(ldim,n)

     ! output: .true. if matrix is positive-semidefinite, .false. otherwise
     logical, intent(out) :: is_positive_semidefinite

     ! tolerance for eigenvalue comparison (optional, default: 1.0e-8)
     real(dp), intent(in), optional :: tol

!! local variables
     ! loop index
     integer  :: i

     ! actual tolerance value
     real(dp) :: actual_tol

     ! eigenvalues
     real(dp), allocatable :: eval(:)

!! [body

     ! set tolerance (use default if not provided)
     if ( present(tol) ) then
         actual_tol = tol
     else
         actual_tol = eps8
     endif ! back if ( present(tol) ) block

     ! allocate eigenvalue array
     allocate(eval(n), stat=i)
     !
     if ( i /= 0 ) then
         call s_print_error('s_is_positive_semidefinite_d','can not allocate enough memory')
     endif ! back if ( i /= 0 ) block

     ! compute eigenvalues
     call s_eigvals_sy(ldim, n, A, eval)

     ! check if all eigenvalues >= -tol
     is_positive_semidefinite = .true.
     do i=1,n
         if ( eval(i) < -actual_tol ) then
             is_positive_semidefinite = .false.
             exit
         endif ! back if ( eval(i) < -actual_tol ) block
     enddo ! over i={1,n} loop

     ! deallocate eigenvalue array
     if ( allocated(eval) ) deallocate(eval)

!! body]

     return
  end subroutine s_is_positive_semidefinite_d

!!
!! @sub s_is_positive_semidefinite_z
!!
!! check if a complex(dp) Hermitian matrix is positive-semidefinite.
!! all eigenvalues >= 0 (with tolerance).
!!
  subroutine s_is_positive_semidefinite_z(ldim, n, A, is_positive_semidefinite, tol)
     use constants, only : dp
     use constants, only : eps8

     implicit none

!! external arguments
     ! leading dimension of matrix A
     integer, intent(in)     :: ldim

     ! order of the matrix A
     integer, intent(in)     :: n

     ! input matrix (must be Hermitian)
     complex(dp), intent(in) :: A(ldim,n)

     ! output: .true. if matrix is positive-semidefinite, .false. otherwise
     logical, intent(out)    :: is_positive_semidefinite

     ! tolerance for eigenvalue comparison (optional, default: 1.0e-8)
     real(dp), intent(in), optional :: tol

!! local variables
     ! loop index
     integer  :: i

     ! actual tolerance value
     real(dp) :: actual_tol

     ! eigenvalues
     real(dp), allocatable :: eval(:)

!! [body

     ! set tolerance (use default if not provided)
     if ( present(tol) ) then
         actual_tol = tol
     else
         actual_tol = eps8
     endif ! back if ( present(tol) ) block

     ! allocate eigenvalue array
     allocate(eval(n), stat=i)
     !
     if ( i /= 0 ) then
         call s_print_error('s_is_positive_semidefinite_z','can not allocate enough memory')
     endif ! back if ( i /= 0 ) block

     ! compute eigenvalues
     call s_eigvals_he(ldim, n, A, eval)

     ! check if all eigenvalues >= -tol
     is_positive_semidefinite = .true.
     do i=1,n
         if ( eval(i) < -actual_tol ) then
             is_positive_semidefinite = .false.
             exit
         endif ! back if ( eval(i) < -actual_tol ) block
     enddo ! over i={1,n} loop

     ! deallocate eigenvalue array
     if ( allocated(eval) ) deallocate(eval)

!! body]

     return
  end subroutine s_is_positive_semidefinite_z

!!
!! @sub s_is_upper_triangular_d
!!
!! check if a real(dp) matrix is upper triangular
!! (all elements below main diagonal are zero).
!!
  subroutine s_is_upper_triangular_d(n, A, is_upper_triangular, tol)
     use constants, only : dp
     use constants, only : eps8

     implicit none

!! external arguments
     ! size of matrix (must be square)
     integer, intent(in)  :: n

     ! input matrix
     real(dp), intent(in) :: A(n,n)

     ! output: .true. if matrix is upper triangular, .false. otherwise
     logical, intent(out) :: is_upper_triangular

     ! tolerance for floating point comparison (optional, default 1.0e-8)
     real(dp), intent(in), optional :: tol

!! local variables
     ! loop indices
     integer  :: i, j

     ! actual tolerance value
     real(dp) :: actual_tol

!! [body

     ! set tolerance (use default if not provided)
     if ( present(tol) ) then
         actual_tol = tol
     else
         actual_tol = eps8
     endif ! back if ( present(tol) ) block

     ! initialize
     is_upper_triangular = .true.

     ! check all elements below main diagonal
     ! upper triangular condition: A(i,j) = 0 for i > j
     outer_loop: do i=2,n
         inner_loop: do j=1,i-1
             ! check if element below diagonal is zero
             if ( abs( A(i,j) ) > actual_tol ) then
                 is_upper_triangular = .false.
                 exit outer_loop
             endif ! back if ( abs( A(i,j) ) > actual_tol ) block
         enddo inner_loop ! over j={1,i-1} loop (inner_loop)
     enddo outer_loop ! over i={2,n} loop (outer_loop)

!! body]

     return
  end subroutine s_is_upper_triangular_d

!!
!! @sub s_is_upper_triangular_z
!!
!! check if a complex(dp) matrix is upper triangular
!! (all elements below main diagonal are zero).
!!
  subroutine s_is_upper_triangular_z(n, A, is_upper_triangular, tol)
     use constants, only : dp
     use constants, only : eps8

     implicit none

!! external arguments
     ! size of matrix (must be square)
     integer, intent(in)     :: n

     ! input matrix
     complex(dp), intent(in) :: A(n,n)

     ! output: .true. if matrix is upper triangular, .false. otherwise
     logical, intent(out)    :: is_upper_triangular

     ! tolerance for floating point comparison (optional, default 1.0e-8)
     real(dp), intent(in), optional :: tol

!! local variables
     ! loop indices
     integer  :: i, j

     ! actual tolerance value
     real(dp) :: actual_tol

!! [body

     ! set tolerance (use default if not provided)
     if ( present(tol) ) then
         actual_tol = tol
     else
         actual_tol = eps8
     endif ! back if ( present(tol) ) block

     ! initialize
     is_upper_triangular = .true.

     ! check all elements below main diagonal
     ! upper triangular condition: A(i,j) = 0 for i > j
     outer_loop: do i=2,n
         inner_loop: do j=1,i-1
             ! check if element below diagonal is zero
             if ( abs( A(i,j) ) > actual_tol ) then
                 is_upper_triangular = .false.
                 exit outer_loop
             endif ! back if ( abs( A(i,j) ) > actual_tol ) block
         enddo inner_loop ! over j={1,i-1} loop (inner_loop)
     enddo outer_loop ! over i={2,n} loop (outer_loop)

 !! body]

      return
   end subroutine s_is_upper_triangular_z

!!
!! @sub s_is_lower_triangular_d
!!
!! check if a real(dp) matrix is lower triangular
!! (all elements above main diagonal are zero).
!!
  subroutine s_is_lower_triangular_d(n, A, is_lower_triangular, tol)
     use constants, only : dp
     use constants, only : eps8

     implicit none

!! external arguments
     ! size of matrix (must be square)
     integer, intent(in)  :: n

     ! input matrix
     real(dp), intent(in) :: A(n,n)

     ! output: .true. if matrix is lower triangular, .false. otherwise
     logical, intent(out) :: is_lower_triangular

     ! tolerance for floating point comparison (optional, default 1.0e-8)
     real(dp), intent(in), optional :: tol

!! local variables
     ! loop indices
     integer  :: i, j

     ! actual tolerance value
     real(dp) :: actual_tol

!! [body

     ! set tolerance (use default if not provided)
     if ( present(tol) ) then
         actual_tol = tol
     else
         actual_tol = eps8
     endif ! back if ( present(tol) ) block

     ! initialize
     is_lower_triangular = .true.

     ! check all elements above main diagonal
     ! lower triangular condition: A(i,j) = 0 for i < j
     outer_loop: do i=1,n-1
         inner_loop: do j=i+1,n
             ! check if element above diagonal is zero
             if ( abs( A(i,j) ) > actual_tol ) then
                 is_lower_triangular = .false.
                 exit outer_loop
             endif ! back if ( abs( A(i,j) ) > actual_tol ) block
         enddo inner_loop ! over j={i+1,n} loop (inner_loop)
     enddo outer_loop ! over i={1,n-1} loop (outer_loop)

!! body]

     return
  end subroutine s_is_lower_triangular_d

!!
!! @sub s_is_lower_triangular_z
!!
!! check if a complex(dp) matrix is lower triangular
!! (all elements above main diagonal are zero).
!!
  subroutine s_is_lower_triangular_z(n, A, is_lower_triangular, tol)
     use constants, only : dp
     use constants, only : eps8

     implicit none

!! external arguments
     ! size of matrix (must be square)
     integer, intent(in)     :: n

     ! input matrix
     complex(dp), intent(in) :: A(n,n)

     ! output: .true. if matrix is lower triangular, .false. otherwise
     logical, intent(out)    :: is_lower_triangular

     ! tolerance for floating point comparison (optional, default 1.0e-8)
     real(dp), intent(in), optional :: tol

!! local variables
     ! loop indices
     integer  :: i, j

     ! actual tolerance value
     real(dp) :: actual_tol

!! [body

     ! set tolerance (use default if not provided)
     if ( present(tol) ) then
         actual_tol = tol
     else
         actual_tol = eps8
     endif ! back if ( present(tol) ) block

     ! initialize
     is_lower_triangular = .true.

     ! check all elements above main diagonal
     ! lower triangular condition: A(i,j) = 0 for i < j
     outer_loop: do i=1,n-1
         inner_loop: do j=i+1,n
             ! check if element above diagonal is zero
             if ( abs( A(i,j) ) > actual_tol ) then
                 is_lower_triangular = .false.
                 exit outer_loop
             endif ! back if ( abs( A(i,j) ) > actual_tol ) block
         enddo inner_loop ! over j={i+1,n} loop (inner_loop)
     enddo outer_loop ! over i={1,n-1} loop (outer_loop)

!! body]

     return
  end subroutine s_is_lower_triangular_z

!!
!! @sub s_is_singular_d
!!
!! check if a real(dp) matrix is singular (determinant is zero).
!!
  subroutine s_is_singular_d(n, A, is_singular)
     use constants, only : dp

     implicit none

!! external arguments
     ! size of matrix (must be square)
     integer, intent(in)  :: n

     ! input matrix
     real(dp), intent(in) :: A(n,n)

     ! output: .true. if matrix is singular, .false. otherwise
     logical, intent(out) :: is_singular

!! local variables
     ! error flag
     integer :: ierror

     ! working array for LU decomposition
     real(dp), allocatable :: dmat(:,:)

     ! working arrays for lapack subroutines: dgetrf
     integer, allocatable  :: ipiv(:)

!! [body

     ! allocate memory
     allocate(dmat(n,n), stat=ierror)
     allocate(ipiv(n),   stat=ierror)
     !
     if ( ierror /= 0 ) then
         call s_print_error('s_is_singular_d','can not allocate enough memory')
     endif ! back if ( ierror /= 0 ) block

     ! copy input matrix to working array
     dmat = A

     ! computes the LU factorization of a general n-by-n matrix.
     ! need lapack package (dgetrf subroutine).
     ! if ierror > 0, the matrix is singular
     call DGETRF(n, n, dmat, n, ipiv, ierror)

     ! check if matrix is singular
     ! ierror > 0: U(i,i) is exactly zero. The factorization
     ! has been completed, but the factor U is exactly singular.
     if ( ierror > 0 ) then
         is_singular = .true.
     else
         is_singular = .false.
     endif ! back if ( ierror > 0 ) block

     ! deallocate memory
     if ( allocated(dmat) ) deallocate(dmat)
     if ( allocated(ipiv) ) deallocate(ipiv)

!! body]

     return
  end subroutine s_is_singular_d

!!
!! @sub s_is_singular_z
!!
!! check if a complex(dp) matrix is singular (determinant is zero).
!!
  subroutine s_is_singular_z(n, A, is_singular)
     use constants, only : dp

     implicit none

!! external arguments
     ! size of matrix (must be square)
     integer, intent(in)     :: n

     ! input matrix
     complex(dp), intent(in) :: A(n,n)

     ! output: .true. if matrix is singular, .false. otherwise
     logical, intent(out)    :: is_singular

!! local variables
     ! error flag
     integer :: ierror

     ! working array for LU decomposition
     complex(dp), allocatable :: zmat(:,:)

     ! working arrays for lapack subroutines: zgetrf
     integer, allocatable     :: ipiv(:)

!! [body

     ! allocate memory
     allocate(zmat(n,n), stat=ierror)
     allocate(ipiv(n),   stat=ierror)
     !
     if ( ierror /= 0 ) then
         call s_print_error('s_is_singular_z','can not allocate enough memory')
     endif ! back if ( ierror /= 0 ) block

     ! copy input matrix to working array
     zmat = A

     ! computes the LU factorization of a general n-by-n matrix.
     ! need lapack package (zgetrf subroutine).
     ! if ierror > 0, the matrix is singular
     call ZGETRF(n, n, zmat, n, ipiv, ierror)

     ! check if matrix is singular
     ! ierror > 0: U(i,i) is exactly zero. The factorization
     ! has been completed, but the factor U is exactly singular.
     if ( ierror > 0 ) then
         is_singular = .true.
     else
         is_singular = .false.
     endif ! back if ( ierror > 0 ) block

     ! deallocate memory
     if ( allocated(zmat) ) deallocate(zmat)
     if ( allocated(ipiv) ) deallocate(ipiv)

!! body]

     return
  end subroutine s_is_singular_z

!!
!! @sub s_is_orthogonal_d
!!
!! check if a real(dp) matrix is orthogonal (Q^T * Q = I).
!!
  subroutine s_is_orthogonal_d(n, Q, is_orthogonal, tol)
     use constants, only : dp
     use constants, only : eps8

     implicit none

!! external arguments
     ! size of matrix (must be square)
     integer, intent(in)  :: n

     ! input matrix to check
     real(dp), intent(in) :: Q(n,n)

     ! output: .true. if matrix is orthogonal, .false. otherwise
     logical, intent(out) :: is_orthogonal

     ! tolerance for floating point comparison (optional, default 1.0e-8)
     real(dp), intent(in), optional :: tol

!! local variables
     ! loop index
     integer  :: i

     ! actual tolerance value
     real(dp) :: actual_tol

     ! product matrix: Q^T * Q
     real(dp), allocatable :: product(:,:)

!! [body

     ! set tolerance (use default if not provided)
     if ( present(tol) ) then
         actual_tol = tol
     else
         actual_tol = eps8
     endif ! back if ( present(tol) ) block

     ! allocate matrix
     allocate(product(n,n), stat=i)
     !
     if ( i /= 0 ) then
         call s_print_error('s_is_orthogonal_d','can not allocate enough memory')
     endif ! back if ( i /= 0 ) block

     ! compute product = Q^T * Q using matmul
     product = matmul(transpose(Q), Q)

     ! check if product equals identity matrix within tolerance
     ! identity matrix has ones on diagonal, zeros elsewhere
     ! use maxval to check all elements at once
     is_orthogonal = ( maxval( abs( product - transpose(product) ) ) <= actual_tol )

     ! deallocate matrix
     if ( allocated(product) ) deallocate(product)

!! body]

     return
  end subroutine s_is_orthogonal_d

!!
!! @sub s_is_unitary_z
!!
!! check if a complex(dp) matrix is unitary (Q^H * Q = I).
!!
  subroutine s_is_unitary_z(n, Q, is_unitary, tol)
     use constants, only : dp
     use constants, only : eps8

     implicit none

!! external arguments
     ! size of matrix (must be square)
     integer, intent(in)     :: n

     ! input matrix to check
     complex(dp), intent(in) :: Q(n,n)

     ! output: .true. if matrix is unitary, .false. otherwise
     logical, intent(out)    :: is_unitary

     ! tolerance for floating point comparison (optional, default 1.0e-8)
     real(dp), intent(in), optional :: tol

!! local variables
     ! loop index
     integer  :: i

     ! actual tolerance value
     real(dp) :: actual_tol

     ! product matrix: Q^H * Q
     complex(dp), allocatable :: product(:,:)

!! [body

     ! set tolerance (use default if not provided)
     if ( present(tol) ) then
         actual_tol = tol
     else
         actual_tol = eps8
     endif ! back if ( present(tol) ) block

     ! allocate matrix
     allocate(product(n,n), stat=i)
     !
     if ( i /= 0 ) then
         call s_print_error('s_is_unitary_z','can not allocate enough memory')
     endif ! back if ( i /= 0 ) block

     ! compute product = Q^H * Q using matmul
     product = matmul(conjg(transpose(Q)), Q)

     ! check if product equals identity matrix within tolerance
     ! identity matrix has ones on diagonal, zeros elsewhere
     ! use maxval to check all elements at once
     is_unitary = ( maxval( abs( product - conjg(transpose(product)) ) ) <= actual_tol )

     ! deallocate matrix
     if ( allocated(product) ) deallocate(product)

!! body]

     return
  end subroutine s_is_unitary_z

!!
!! @sub s_is_normal_d
!!
!! check if a real(dp) matrix is normal (A^T * A = A * A^T).
!!
  subroutine s_is_normal_d(n, A, is_normal, tol)
     use constants, only : dp
     use constants, only : eps8

     implicit none

!! external arguments
     ! size of matrix (must be square)
     integer, intent(in)  :: n

     ! input matrix to check
     real(dp), intent(in) :: A(n,n)

     ! output: .true. if matrix is normal, .false. otherwise
     logical, intent(out) :: is_normal

     ! tolerance for floating point comparison (optional, default 1.0e-8)
     real(dp), intent(in), optional :: tol

!! local variables
     ! loop index
     integer  :: i

     ! actual tolerance value
     real(dp) :: actual_tol

     ! product matrices: A^T * A and A * A^T
     real(dp), allocatable :: left_product(:,:)
     real(dp), allocatable :: right_product(:,:)

!! [body

     ! set tolerance (use default if not provided)
     if ( present(tol) ) then
         actual_tol = tol
     else
         actual_tol = eps8
     endif ! back if ( present(tol) ) block

     ! allocate matrices
     allocate(left_product(n,n), stat=i)
     allocate(right_product(n,n), stat=i)
     !
     if ( i /= 0 ) then
         call s_print_error('s_is_normal_d','can not allocate enough memory')
     endif ! back if ( i /= 0 ) block

     ! initialize
     is_normal = .true.

     ! compute left product = A^T * A using matmul
     left_product = matmul(transpose(A), A)

     ! compute right product = A * A^T using matmul
     right_product = matmul(A, transpose(A))

     ! check if left_product equals right_product within tolerance
     is_normal = ( maxval( abs( left_product - right_product ) ) <= actual_tol )

     ! deallocate matrices
     if ( allocated(left_product) )  deallocate(left_product)
     if ( allocated(right_product) ) deallocate(right_product)

!! body]

     return
  end subroutine s_is_normal_d

!!
!! @sub s_is_normal_z
!!
!! check if a complex(dp) matrix is normal (A^H * A = A * A^H).
!!
  subroutine s_is_normal_z(n, A, is_normal, tol)
     use constants, only : dp
     use constants, only : eps8

     implicit none

!! external arguments
     ! size of matrix (must be square)
     integer, intent(in)     :: n

     ! input matrix to check
     complex(dp), intent(in) :: A(n,n)

     ! output: .true. if matrix is normal, .false. otherwise
     logical, intent(out)    :: is_normal

     ! tolerance for floating point comparison (optional, default 1.0e-8)
     real(dp), intent(in), optional :: tol

!! local variables
     ! loop index
     integer  :: i

     ! actual tolerance value
     real(dp) :: actual_tol

     ! product matrices: A^H * A and A * A^H
     complex(dp), allocatable :: left_product(:,:)
     complex(dp), allocatable :: right_product(:,:)

!! [body

     ! set tolerance (use default if not provided)
     if ( present(tol) ) then
         actual_tol = tol
     else
         actual_tol = eps8
     endif ! back if ( present(tol) ) block

     ! allocate matrices
     allocate(left_product(n,n), stat=i)
     allocate(right_product(n,n), stat=i)
     !
     if ( i /= 0 ) then
         call s_print_error('s_is_normal_z','can not allocate enough memory')
     endif ! back if ( i /= 0 ) block

     ! initialize
     is_normal = .true.

     ! compute left product = A^H * A using matmul
     left_product = matmul(conjg(transpose(A)), A)

     ! compute right product = A * A^H using matmul
     right_product = matmul(A, conjg(transpose(A)))

     ! check if left_product equals right_product within tolerance
     is_normal = ( maxval( abs( left_product - right_product ) ) <= actual_tol )

     ! deallocate matrices
     if ( allocated(left_product) )  deallocate(left_product)
     if ( allocated(right_product) ) deallocate(right_product)

!! body]

     return
  end subroutine s_is_normal_z

!!
!! @sub s_sparsity_ratio_i
!!
!! calculate sparsity ratio for an integer matrix (ratio of zero elements).
!!
  subroutine s_sparsity_ratio_i(n, A, ratio)
     use constants, only : dp
     use constants, only : zero

     implicit none

!! external arguments
     ! size of matrix (must be square)
     integer, intent(in)   :: n

     ! input matrix
     integer, intent(in)   :: A(n,n)

     ! output: sparsity ratio (0.0 to 1.0)
     real(dp), intent(out) :: ratio

!! local variables
     ! counter for zero elements
     integer :: zero_count

     ! total elements
     integer :: total

!! [body

     ! initialize
     total = n * n

     ! count zero elements using intrinsic function
     zero_count = count(A(1:n,1:n) == 0)

     ! compute sparsity ratio
     if ( total > 0 ) then
         ratio = real(zero_count, dp) / real(total, dp)
     else
         ratio = zero
     endif ! back if ( total > 0 ) block

!! body]

     return
  end subroutine s_sparsity_ratio_i

!!
!! @sub s_sparsity_ratio_d
!!
!! calculate sparsity ratio for a real(dp) matrix (ratio of zero elements).
!!
  subroutine s_sparsity_ratio_d(n, A, ratio)
     use constants, only : dp
     use constants, only : zero, eps8

     implicit none

!! external arguments
     ! size of matrix (must be square)
     integer, intent(in)   :: n

     ! input matrix
     real(dp), intent(in)  :: A(n,n)

     ! output: sparsity ratio (0.0 to 1.0)
     real(dp), intent(out) :: ratio

! local variables
     ! counter for zero elements
     integer :: zero_count

     ! total elements
     integer :: total

!! [body

     ! initialize
     total = n * n

     ! count zero elements using intrinsic function
     zero_count = count(abs(A(1:n,1:n)) < eps8)

     ! compute sparsity ratio
     if ( total > 0 ) then
         ratio = real(zero_count, dp) / real(total, dp)
     else
         ratio = zero
     endif ! back if ( total > 0 ) block

!! body]

     return
  end subroutine s_sparsity_ratio_d

!!
!! @sub s_sparsity_ratio_z
!!
!! calculate sparsity ratio for a complex(dp) matrix (ratio of zero elements).
!!
  subroutine s_sparsity_ratio_z(n, A, ratio)
     use constants, only : dp
     use constants, only : zero, eps8

     implicit none

!! external arguments
     ! size of matrix (must be square)
     integer, intent(in)     :: n

     ! input matrix
     complex(dp), intent(in) :: A(n,n)

     ! output: sparsity ratio (0.0 to 1.0)
     real(dp), intent(out)   :: ratio

!! local variables
     ! counter for zero elements
     integer :: zero_count

     ! total elements
     integer :: total

!! [body

     ! initialize
     total = n * n

     ! count zero elements using intrinsic function
     zero_count = count(abs(A(1:n,1:n)) < eps8)

     ! compute sparsity ratio
     if ( total > 0 ) then
         ratio = real(zero_count, dp) / real(total, dp)
     else
         ratio = zero
     endif ! back if ( total > 0 ) block

!! body]

     return
  end subroutine s_sparsity_ratio_z

!!
!! @sub s_upper_triangular_d
!!
!! construct an upper triangular matrix from a real(dp) matrix.
!! sets all elements below the main diagonal to zero.
!!
  subroutine s_upper_triangular_d(n, A)
     use constants, only : dp
     use constants, only : zero

     implicit none

!! external arguments
     ! size of matrix (must be square)
     integer, intent(in)     :: n

     ! input/output matrix
     real(dp), intent(inout) :: A(n,n)

!! local variables
     ! loop index
     integer :: i

!! [body

     ! set elements below main diagonal to zero
     do i=2,n
         A(i,1:i-1) = zero
     enddo ! over i={2,n} loop

!! body]

     return
  end subroutine s_upper_triangular_d

!!
!! @sub s_upper_triangular_z
!!
!! construct an upper triangular matrix from a complex(dp) matrix.
!! sets all elements below the main diagonal to zero.
!!
  subroutine s_upper_triangular_z(n, A)
     use constants, only : dp
     use constants, only : czero

     implicit none

!! external arguments
     ! size of matrix (must be square)
     integer, intent(in)        :: n

     ! input/output matrix
     complex(dp), intent(inout) :: A(n,n)

!! local variables
     ! loop index
     integer :: i

!! [body

     ! set elements below main diagonal to zero
     do i=2,n
         A(i,1:i-1) = czero
     enddo ! over i={2,n} loop

!! body]

     return
  end subroutine s_upper_triangular_z

!!
!! @sub s_lower_triangular_d
!!
!! construct a lower triangular matrix from a real(dp) matrix.
!! sets all elements above main diagonal to zero.
!!
  subroutine s_lower_triangular_d(n, A)
     use constants, only : dp
     use constants, only : zero

     implicit none

!! external arguments
     ! size of matrix (must be square)
     integer, intent(in)     :: n

     ! input/output matrix
     real(dp), intent(inout) :: A(n,n)

!! local variables
     ! loop index
     integer :: i

!! [body

     ! set elements above main diagonal to zero
     do i=1,n-1
         A(i,i+1:n) = zero
     enddo ! over i={1,n-1} loop

!! body]

     return
  end subroutine s_lower_triangular_d

!!
!! @sub s_lower_triangular_z
!!
!! construct a lower triangular matrix from a complex(dp) matrix.
!! sets all elements above main diagonal to zero.
!!
  subroutine s_lower_triangular_z(n, A)
     use constants, only : dp
     use constants, only : czero

     implicit none

!! external arguments
     ! size of matrix (must be square)
     integer, intent(in)        :: n

     ! input/output matrix
     complex(dp), intent(inout) :: A(n,n)

!! local variables
     ! loop index
     integer :: i

!! [body

     ! set elements above main diagonal to zero
     do i=1,n-1
         A(i,i+1:n) = czero
     enddo ! over i={1,n-1} loop

!! body]

     return
  end subroutine s_lower_triangular_z

!!
!! @sub s_tridiagonal_d
!!
!! construct a tridiagonal matrix from a real(dp) matrix.
!! sets all elements except main diagonal, super-diagonal,
!! and sub-diagonal to zero.
!!
  subroutine s_tridiagonal_d(n, A)
     use constants, only : dp
     use constants, only : zero

     implicit none

!! external arguments
     ! size of matrix (must be square)
     integer, intent(in)     :: n

     ! input/output matrix
     real(dp), intent(inout) :: A(n,n)

!! local variables
     ! loop index
     integer :: i

!! [body

     ! set elements with |i-j| > 1 to zero
     ! keep only main diagonal, super-diagonal, and sub-diagonal
     do i=1,n
         if ( i > 2 ) A(i,1:i-2) = zero
         if ( i+2 <= n ) A(i,i+2:n) = zero
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_tridiagonal_d

!!
!! @sub s_tridiagonal_z
!!
!! construct a tridiagonal matrix from a complex(dp) matrix.
!! sets all elements except main diagonal, super-diagonal,
!! and sub-diagonal to zero.
!!
  subroutine s_tridiagonal_z(n, A)
     use constants, only : dp
     use constants, only : czero

     implicit none

!! external arguments
     ! size of matrix (must be square)
     integer, intent(in)        :: n

     ! input/output matrix
     complex(dp), intent(inout) :: A(n,n)

!! local variables
     ! loop index
     integer :: i

!! [body

     ! set elements with |i-j| > 1 to zero
     ! keep only main diagonal, super-diagonal, and sub-diagonal
     do i=1,n
         if ( i > 2 ) A(i,1:i-2) = czero
         if ( i+2 <= n ) A(i,i+2:n) = czero
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_tridiagonal_z

!!
!! @sub s_hilbert_d
!!
!! build a Hilbert matrix.
!! Hilbert matrix elements: H(i,j) = 1 / (i + j - 1)
!!
  subroutine s_hilbert_d(n, A)
     use constants, only : dp
     use constants, only : one

     implicit none

!! external arguments
     ! size of matrix (must be square)
     integer, intent(in)   :: n

     ! output Hilbert matrix
     real(dp), intent(out) :: A(n,n)

!! local variables
     ! loop indices
     integer :: i, j

!! [body

     ! build Hilbert matrix: H(i,j) = 1/(i+j-1)
     do i=1,n
         do j=1,n
             A(i,j) = one / real(i + j - 1, dp)
         enddo ! over j={1,n} loop
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_hilbert_d

!!
!! @sub s_vandermonde_d
!!
!! build a real(dp) Vandermonde matrix.
!! Vandermonde matrix elements: V(i,j) = x_i^(j-1)
!!
  subroutine s_vandermonde_d(n, x, A)
     use constants, only : dp
     use constants, only : one

     implicit none

!! external arguments
     ! size of matrix (must be square)
     integer, intent(in)   :: n

     ! input vector of points
     real(dp), intent(in)  :: x(n)

     ! output Vandermonde matrix
     real(dp), intent(out) :: A(n,n)

!! local variables
     ! loop indices
     integer  :: i, j

     ! temporary value for power
     real(dp) :: val

!! [body

     ! build Vandermonde matrix: V(i,j) = x(i)^(j-1)
     do i=1,n
         A(i,1) = one
         val = one
         do j=2,n
             val = val * x(i)
             A(i,j) = val
         enddo ! over j={2,n} loop
     enddo ! over i={1,n} loop

!! body]

    return
  end subroutine s_vandermonde_d

!!
!! @sub s_vandermonde_z
!!
!! build a complex(dp) Vandermonde matrix.
!! Vandermonde matrix elements: V(i,j) = x_i^(j-1)
!!
  subroutine s_vandermonde_z(n, x, A)
     use constants, only : dp
     use constants, only : cone

     implicit none

!! external arguments
     ! size of matrix (must be square)
     integer, intent(in)      :: n

     ! input vector of points
     complex(dp), intent(in)  :: x(n)

     ! output Vandermonde matrix
     complex(dp), intent(out) :: A(n,n)

!! local variables
     ! loop indices
     integer :: i, j

     ! temporary value for power
     complex(dp) :: val

!! [body

     ! build Vandermonde matrix: V(i,j) = x(i)^(j-1)
     do i=1,n
         A(i,1) = cone
         val = cone
         do j=2,n
             val = val * x(i)
             A(i,j) = val
         enddo ! over j={2,n} loop
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_vandermonde_z

!!
!! @sub s_pascal_d
!!
!! build a real(dp) Pascal matrix.
!! Pascal matrix elements: P(i,j) = C(i+j-2, j-1) = C(i+j-2, i-1)
!!
  subroutine s_pascal_d(n, A)
     use constants, only : dp
     use constants, only : one

     implicit none

!! external arguments
     ! size of matrix (must be square)
     integer, intent(in)   :: n

     ! output Pascal matrix
     real(dp), intent(out) :: A(n,n)

!! local variables
     ! loop indices
     integer  :: i, j

!! [body

     ! build Pascal matrix using recurrence relation:
     ! P(i,j) = P(i-1,j) + P(i,j-1)
     ! with boundary conditions: P(1,j) = 1, P(i,1) = 1

     ! set first row and first column to 1
     do i=1,n
         A(i,1) = one
         A(1,i) = one
     enddo ! over i={1,n} loop

     ! fill the rest using recurrence relation
     do i=2,n
         do j=2,n
             A(i,j) = A(i-1,j) + A(i,j-1)
         enddo ! over j={2,n} loop
     enddo ! over i={2,n} loop

!! body]

     return
  end subroutine s_pascal_d

!!
!! @sub s_wilkinson_d
!!
!! build a real(dp) Wilkinson's W_{+} matrix.
!! Wilkinson matrix has main diagonal: W(i,i) = n/2 + 1 - i
!! and super/sub-diagonal elements equal to 1.
!!
  subroutine s_wilkinson_d(n, A)
     use constants, only : dp
     use constants, only : one, two, zero

     implicit none

!! external arguments
     ! size of matrix (must be square)
     integer, intent(in)   :: n

     ! output Wilkinson's W_{+} matrix
     real(dp), intent(out) :: A(n,n)

!! local variables
     ! loop index
     integer  :: i

     ! main diagonal value
     real(dp) :: diag_val

!! [body

     ! initialize matrix to zero
     A = zero

     ! set main diagonal: W(i,i) = n/2 + 1 - i
     do i=1,n
         diag_val = real(n, dp) / two + one - real(i, dp)
         A(i,i) = diag_val
     enddo ! over i={1,n} loop

     ! set super-diagonal and sub-diagonal to 1
     do i=1,n-1
         A(i,i+1) = one
         A(i+1,i) = one
     enddo ! over i={1,n-1} loop

!! body]

     return
  end subroutine s_wilkinson_d

!!
!! @sub s_sparse_random_d
!!
!! build a sparse random real(dp) matrix.
!! sparsity parameter controls fraction of non-zero elements (0-1).
!!
  subroutine s_sparse_random_d(n, sparsity, A)
     use constants, only : dp
     use constants, only : zero, one

     implicit none

!! external arguments
     ! size of matrix (must be square)
     integer, intent(in)   :: n

     ! sparsity level (1 = dense, 0 = sparse)
     real(dp), intent(in)  :: sparsity

     ! output sparse random matrix
     real(dp), intent(out) :: A(n,n)

!! local variables
     ! loop indices
     integer  :: i, j, k

     ! number of non-zero elements
     integer  :: nnz

     ! random values
     real(dp) :: r1, r2, r3

!! [body

     ! validate input parameters
     if ( sparsity < zero .or. sparsity > one ) then
         call s_print_error('s_sparse_random_d','sparsity must be [0, 1]')
     endif

     ! initialize matrix to zero
     A = zero

     ! calculate number of non-zero elements
     nnz = int(n * n * sparsity)

     ! fill matrix randomly
     do k=1,nnz
         call random_number(r1)
         call random_number(r2)
         call random_number(r3)
         i = min(int(r1 * real(n, dp)) + 1, n)
         j = min(int(r2 * real(n, dp)) + 1, n)
         A(i,j) = r3
     enddo ! over k={1,nnz} loop

!! body]

     return
  end subroutine s_sparse_random_d

!!
!! @sub s_sparse_random_z
!!
!! build a sparse random complex(dp) matrix.
!! sparsity parameter controls fraction of non-zero elements (0-1).
!!
  subroutine s_sparse_random_z(n, sparsity, A)
     use constants, only : dp
     use constants, only : zero, one
     use constants, only : czero

     implicit none

!! external arguments
     ! size of matrix (must be square)
     integer, intent(in)      :: n

     ! sparsity level (1 = dense, 0 = sparse)
     real(dp), intent(in)     :: sparsity

     ! output sparse random matrix
     complex(dp), intent(out) :: A(n,n)

!! local variables
     ! loop indices
     integer  :: i, j, k

     ! number of non-zero elements
     integer  :: nnz

     ! random values
     real(dp) :: r1, r2, r3, r4

!! [body

     ! validate input parameters
     if ( sparsity < zero .or. sparsity > one ) then
         call s_print_error('s_sparse_random_z','sparsity must be [0, 1]')
     endif

     ! initialize matrix to zero
     A = czero

     ! calculate number of non-zero elements
     nnz = int(n * n * sparsity)

     ! fill matrix randomly
     do k=1,nnz
         call random_number(r1)
         call random_number(r2)
         call random_number(r3)
         call random_number(r4)
         i = min(int(r1 * real(n, dp)) + 1, n)
         j = min(int(r2 * real(n, dp)) + 1, n)
         A(i,j) = dcmplx(r3, r4)
     enddo ! over k={1,nnz} loop

!! body]

     return
  end subroutine s_sparse_random_z

!!
!! @sub s_random_symmetric_d
!!
!! build a random symmetric real(dp) matrix.
!!
  subroutine s_random_symmetric_d(n, A)
     use constants, only : dp

     implicit none

!! external arguments
     ! size of matrix
     integer, intent(in)   :: n

     ! output random symmetric matrix
     real(dp), intent(out) :: A(n,n)

!! local variables
     ! loop indices
     integer  :: i, j

     ! random value
     real(dp) :: r

!! [body

     ! generate only lower triangular part including diagonal
     ! and simultaneously mirror to upper triangular part
     do i=1,n
         do j=1,i
             call random_number(r)
             A(i,j) = r
             ! mirror to upper triangle if not on diagonal
             if (i /= j) then
                 A(j,i) = A(i,j)
             endif
         enddo ! over j={1,i} loop
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_random_symmetric_d

!!
!! @sub s_random_hermitian_z
!!
!! build a random Hermitian complex(dp) matrix.
!!
  subroutine s_random_hermitian_z(n, A)
     use constants, only : dp
     use constants, only : two

     implicit none

!! external arguments
     ! size of matrix
     integer, intent(in)      :: n

     ! output random Hermitian matrix
     complex(dp), intent(out) :: A(n,n)

!! local variables
     ! loop indices
     integer  :: i, j

     ! random values
     real(dp) :: r1, r2

!! [body

     ! generate random complex matrix first
     do i=1,n
         do j=1,n
             call random_number(r1)
             call random_number(r2)
             A(i,j) = dcmplx(r1, r2)
         enddo ! over j={1,n} loop
     enddo ! over i={1,n} loop

     ! Hermitize the matrix: A(i,j) = conjg(A(j,i))
     do i=1,n
         do j=i+1,n
             A(i,j) = (A(i,j) + conjg(A(j,i))) / two
             A(j,i) = conjg(A(i,j))
         enddo ! over j={i+1,n} loop
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_random_hermitian_z

!!
!! @sub s_random_positive_definite_d
!!
!! build a random positive definite real(dp) matrix.
!!
  subroutine s_random_positive_definite_d(n, A)
     use constants, only : dp
     use constants, only : zero, one

     implicit none

!! external arguments
     ! size of matrix
     integer, intent(in)   :: n

     ! output random positive definite matrix
     real(dp), intent(out) :: A(n,n)

!! local variables
     ! loop indices
     integer  :: i, j

     ! random values
     real(dp) :: r

     ! random matrix B used to construct A = B * B^T
     real(dp), allocatable :: B(:,:)

!! [body

     ! allocate temporary matrix B
     allocate(B(n,n), stat=i)

     ! generate random matrix B
     do i=1,n
         do j=1,n
             call random_number(r)
             B(i,j) = real(r, dp)
         enddo ! over j={1,n} loop
     enddo ! over i={1,n} loop

     ! compute A = B * B^T to ensure positive definiteness
     A = matmul(B, transpose(B))

     ! add small positive value to diagonal to ensure strict positive definiteness
     do i=1,n
         A(i,i) = A(i,i) + one
     enddo ! over i={1,n} loop

     ! deallocate temporary matrix
     deallocate(B)

!! body]

     return
  end subroutine s_random_positive_definite_d

!!
!! @sub s_random_positive_definite_z
!!
!! build a random positive definite complex(dp) Hermitian matrix.
!!
  subroutine s_random_positive_definite_z(n, A)
     use constants, only : dp
     use constants, only : czero, cone

     implicit none

!! external arguments
     ! size of matrix
     integer, intent(in)      :: n

     ! output random positive definite Hermitian matrix
     complex(dp), intent(out) :: A(n,n)

!! local variables
     ! loop indices
     integer  :: i, j

     ! random values
     real(dp) :: r1, r2

     ! random matrix B used to construct A = B * B^H
     complex(dp), allocatable :: B(:,:)

!! [body

     ! allocate temporary matrix B
     allocate(B(n,n), stat=i)

     ! generate random complex matrix B
     do i=1,n
         do j=1,n
             call random_number(r1)
             call random_number(r2)
             B(i,j) = dcmplx(real(r1, dp), real(r2, dp))
         enddo ! over j={1,n} loop
     enddo ! over i={1,n} loop

     ! compute A = B * B^H to ensure positive definiteness
     A = matmul(B, conjg(transpose(B)))

     ! add small positive value to diagonal to ensure strict positive definiteness
     do i=1,n
         A(i,i) = A(i,i) + cone
     enddo ! over i={1,n} loop

     ! deallocate temporary matrix
     deallocate(B)

!! body]

     return
  end subroutine s_random_positive_definite_z
