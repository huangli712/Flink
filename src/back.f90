! CSR X DIA
     private :: csr_md_d ! real(dp) version
     private :: csr_md_z ! complex(dp) version

! DIA X CSR
     private :: csr_dm_d ! real(dp) version
     private :: csr_dm_z ! complex(dp) version

     public :: sp_csr_mm_dia
     interface sp_csr_mm_dia
         module procedure sp_matmul_amudia
         module procedure sp_matmul_amudia_z
     end interface sp_csr_mm_dia

     public :: sp_dia_mm_csr
     interface sp_dia_mm_csr
         module procedure sp_matmul_diamua
         module procedure sp_matmul_diamua_z
     end interface sp_dia_mm_csr

!!
!! @sub sp_matmul_amumat
!!
!! performs the matrix by matrix product C = A * B.
!!
  subroutine sp_matmul_amumat(nrow, ndim, ncol, nmax, a, ja, ia, b, jb, ib, c, jc, ic)
     implicit none

!! external arguments
     ! the row dimension of matrix A = row dimension of matrix C
     integer, intent(in)   :: nrow

     ! the column dimension of matrix A = row dimension of matrix B
     integer, intent(in)   :: ndim

     ! the column dimension of matrix B = column dimension of matrix C
     integer, intent(in)   :: ncol

     ! the length of the arrays c and jc.
     !
     ! this subroutine will stop if the result matrix C has a number of
     ! elements that exceeds nmax.
     integer, intent(in)   :: nmax

     ! a, ja, ia, matrix A in compressed sparse row format
     integer, intent(in)   :: ia(nrow+1)
     integer, intent(in)   :: ja(nmax)
     real(dp), intent(in)  :: a(nmax)

     ! b, jb, ib, matrix B in compressed sparse row format
     integer, intent(in)   :: ib(ndim+1)
     integer, intent(in)   :: jb(nmax)
     real(dp), intent(in)  :: b(nmax)

     ! c, jc, ic, resulting matrix C in compressed sparse row format
     integer, intent(out)  :: ic(nrow+1)
     integer, intent(out)  :: jc(nmax)
     real(dp), intent(out) :: c(nmax)

!! local variables
     ! loop index
     integer :: i, j, k

     ! loop index
     integer :: ka, kb

     ! dummy integer variables
     integer :: p, q

     ! integer work array of length equal to the number of columns in
     ! matrix B, which is an array that has nonzero value if the column
     ! index already exist, in which case the value is the index of
     ! that column.
     integer :: iw(ncol)

     ! dummy real(dp) variables, used to improve the ratio of floating
     ! point operations to memory accesses.
     real(dp) :: atmp, btmp

!! [body

     ! init work array
     iw = 0

     ! init C sparse matrix
     ic(1) = 1

     q = 0
     do i=1,nrow
         do ka=ia(i),ia(i+1)-1
             j = ja(ka)
             atmp = a(ka)
             do kb=ib(j),ib(j+1)-1
                 k = jb(kb)
                 btmp = b(kb)

                 p = iw(k)
                 if ( p == 0 ) then
                     q = q + 1
                     iw(k) = q
                     jc(q) = k
                     c(q) = atmp * btmp
                 else
                     c(p) = c(p) + atmp * btmp
                 endif ! back if ( p == 0 ) block
             enddo ! over kb={ib(j),ib(j+1)-1} loop
         enddo ! over ka={ia(i),ia(i+1)-1} loop

         ! done this row i, so set work array to zero again
         do k=ic(i),q
             iw( jc( k ) ) = 0
         enddo ! over k={ic(i),q} loop
         ic(i+1) = q + 1
     enddo ! over i={1,nrow} loop

     ! check the number of nonzero elements
     if ( q > nmax ) then
         write(mystd,'(a)') 'sparse: error in sp_matmul_amumat'
         STOP
     endif ! back if ( q > nmax ) block

!! body]

     return
  end subroutine sp_matmul_amumat

!!
!! @sub sp_matmul_amumat_z
!!
!! performs the matrix by matrix product C = A * B.
!!
  subroutine sp_matmul_amumat_z(nrow, ndim, ncol, nmax, sa, ja, ia, sb, jb, ib, sc, jc, ic)
     implicit none

!! external arguments
     ! the row dimension of matrix A = row dimension of matrix C
     integer, intent(in)      :: nrow

     ! the column dimension of matrix A = row dimension of matrix B
     integer, intent(in)      :: ndim

     ! the column dimension of matrix B = column dimension of matrix C
     integer, intent(in)      :: ncol

     ! the length of the arrays sc and jc.
     !
     ! this subroutine will stop if the result matrix C has a number of
     ! elements that exceeds nmax.
     integer, intent(in)      :: nmax

     ! sa, ja, ia, matrix A in compressed sparse row format
     integer, intent(in)      :: ia(nrow+1)
     integer, intent(in)      :: ja(nmax)
     complex(dp), intent(in)  :: sa(nmax)

     ! sb, jb, ib, matrix B in compressed sparse row format
     integer, intent(in)      :: ib(ndim+1)
     integer, intent(in)      :: jb(nmax)
     complex(dp), intent(in)  :: sb(nmax)

     ! sc, jc, ic, resulting matrix C in compressed sparse row format
     integer, intent(out)     :: ic(nrow+1)
     integer, intent(out)     :: jc(nmax)
     complex(dp), intent(out) :: sc(nmax)

!! local variables
     ! loop index
     integer :: i, j, k

     ! loop index
     integer :: ka, kb

     ! dummy integer variables
     integer :: p, q

     ! integer work array of length equal to the number of columns in
     ! matrix B, which is an array that has nonzero value if the column
     ! index already exist, in which case the value is the index of
     ! that column.
     integer :: iw(ncol)

     ! dummy complex(dp) variables, used to improve the ratio of
     ! floating point operations to memory accesses.
     complex(dp) :: atmp, btmp

!! [body

     ! init work array
     iw = 0

     ! init C sparse matrix
     ic(1) = 1

     q = 0
     do i=1,nrow
         do ka=ia(i),ia(i+1)-1
             j = ja(ka)
             atmp = sa(ka)
             do kb=ib(j),ib(j+1)-1
                 k = jb(kb)
                 btmp = sb(kb)

                 p = iw(k)
                 if ( p == 0 ) then
                     q = q + 1
                     iw(k) = q
                     jc(q) = k
                     sc(q) = atmp * btmp
                 else
                     sc(p) = sc(p) + atmp * btmp
                 endif ! back if ( p == 0 ) block
             enddo ! over kb={ib(j),ib(j+1)-1} loop
         enddo ! over ka={ia(i),ia(i+1)-1} loop

         ! done this row i, so set work array to zero again
         do k=ic(i),q
             iw( jc( k ) ) = 0
         enddo ! over k={ic(i),q} loop
         ic(i+1) = q + 1
     enddo ! over i={1,nrow} loop

     ! check the number of nonzero elements
     if ( q > nmax ) then
         write(mystd,'(a)') 'sparse: error in sp_matmul_amumat_z'
         STOP
     endif ! back if ( q > nmax ) block

!! body]

     return
  end subroutine sp_matmul_amumat_z

!!
!! @sub sp_matmul_amudia
!!
!! performs the matrix by matrix product B = A * Diag.
!!
  subroutine sp_matmul_amudia(nrow, nmax, a, ja, ia, diag, b, jb, ib)
     implicit none

!! external arguments
     ! the row dimension of dense matrix
     integer, intent(in)   :: nrow

     ! maximum number of nonzero elements allowed.
     ! this should be set to be the lengths of the arrays b and jb.
     integer, intent(in)   :: nmax

     ! a, ja, ia, matrix A in compressed sparse row format
     integer, intent(in)   :: ia(nrow+1)
     integer, intent(in)   :: ja(nmax)
     real(dp), intent(in)  :: a(nmax)

     ! diagonal matrix stored as a vector diag
     real(dp), intent(in)  :: diag(nrow)

     ! b, jb, ib, resulting matrix B in compressed sparse row format
     integer, intent(out)  :: ib(nrow+1)
     integer, intent(out)  :: jb(nmax)
     real(dp), intent(out) :: b(nmax)

!! local variables
     ! loop index
     integer :: i
     integer :: k

     ! loop index
     integer :: k1
     integer :: k2

!! [body

     ! init B sparse matrix
     b = 0.0_dp
     ib = 0
     jb = 0

     ! scale each element
     do i=1,nrow
         k1 = ia(i)
         k2 = ia(i+1) - 1
         do k=k1,k2
             b(k) = a(k) * diag( ja(k) )
         enddo ! over k={k1,k2} loop
     enddo ! over i={1,nrow} loop

     do i=1,nrow+1
         ib(i) = ia(i)
     enddo ! over i={1,nrow+1} loop

     do k=ia(1),ia(nrow+1)-1
         jb(k) = ja(k)
     enddo ! over k={ia(1),ia(nrow+1)-1} loop

!! body]

     return
  end subroutine sp_matmul_amudia

!!
!! @sub sp_matmul_amudia_z
!!
!! performs the matrix by matrix product B = A * Diag.
!!
  subroutine sp_matmul_amudia_z(nrow, nmax, sa, ja, ia, diag, sb, jb, ib)
     implicit none

!! external arguments
     ! the row dimension of dense matrix
     integer, intent(in)      :: nrow

     ! maximum number of nonzero elements allowed.
     ! this should be set to be the lengths of the arrays sb and jb.
     integer, intent(in)      :: nmax

     ! sa, ja, ia, matrix A in compressed sparse row format
     integer, intent(in)      :: ia(nrow+1)
     integer, intent(in)      :: ja(nmax)
     complex(dp), intent(in)  :: sa(nmax)

     ! diagonal matrix stored as a vector diag
     complex(dp), intent(in)  :: diag(nrow)

     ! sb, jb, ib, resulting matrix B in compressed sparse row format
     integer, intent(out)     :: ib(nrow+1)
     integer, intent(out)     :: jb(nmax)
     complex(dp), intent(out) :: sb(nmax)

!! local variables
     ! loop index
     integer :: i
     integer :: k

     ! loop index
     integer :: k1
     integer :: k2

!! [body

     ! init B sparse matrix
     sb = dcmplx(0.0_dp, 0.0_dp)
     ib = 0
     jb = 0

     ! scale each element
     do i=1,nrow
         k1 = ia(i)
         k2 = ia(i+1) - 1
         do k=k1,k2
             sb(k) = sa(k) * diag( ja(k) )
         enddo ! over k={k1,k2} loop
     enddo ! over i={1,nrow} loop

     do i=1,nrow+1
         ib(i) = ia(i)
     enddo ! over i={1,nrow+1} loop

     do k=ia(1),ia(nrow+1)-1
         jb(k) = ja(k)
     enddo ! over k={ia(1),ia(nrow+1)-1} loop

!! body]

     return
  end subroutine sp_matmul_amudia_z

!!
!! @sub sp_matmul_diamua
!!
!! performs the matrix by matrix product B = Diag * A.
!!
  subroutine sp_matmul_diamua(nrow, nmax, diag, a, ja, ia, b, jb, ib)
     implicit none

!! external arguments
     ! the row dimension of dense matrix
     integer, intent(in)   :: nrow

     ! maximum number of nonzero elements allowed.
     ! this should be set to be the lengths of the arrays b and jb.
     integer, intent(in)   :: nmax

     ! diagonal matrix stored as a vector diag
     real(dp), intent(in)  :: diag(nrow)

     ! a, ja, ia, matrix A in compressed sparse row format
     integer, intent(in)   :: ia(nrow+1)
     integer, intent(in)   :: ja(nmax)
     real(dp), intent(in)  :: a(nmax)

     ! b, jb, ib, resulting matrix B in compressed sparse row format
     integer, intent(out)  :: ib(nrow+1)
     integer, intent(out)  :: jb(nmax)
     real(dp), intent(out) :: b(nmax)

!! local variables
     ! loop index
     integer :: i
     integer :: k

     ! loop index
     integer :: k1
     integer :: k2

!! [body

     ! init B sparse matrix
     b = 0.0_dp
     ib = 0
     jb = 0

     ! normalize each row
     do i=1,nrow
         k1 = ia(i)
         k2 = ia(i+1) - 1
         do k=k1,k2
             b(k) = a(k) * diag(i)
         enddo ! over k={k1,k2} loop
     enddo ! over i={1,nrow} loop

     do i=1,nrow+1
         ib(i) = ia(i)
     enddo ! over i={1,nrow+1} loop

     do k=ia(1),ia(nrow+1)-1
         jb(k) = ja(k)
     enddo ! over k={ia(1),ia(nrow+1)-1} loop

!! body]

     return
  end subroutine sp_matmul_diamua

!!
!! @sub sp_matmul_diamua_z
!!
!! performs the matrix by matrix product B = Diag * A.
!!
  subroutine sp_matmul_diamua_z(nrow, nmax, diag, sa, ja, ia, sb, jb, ib)
     implicit none

!! external arguments
     ! the row dimension of dense matrix
     integer, intent(in)      :: nrow

     ! maximum number of nonzero elements allowed.
     ! this should be set to be the lengths of the arrays sb and jb.
     integer, intent(in)      :: nmax

     ! diagonal matrix stored as a vector diag
     complex(dp), intent(in)  :: diag(nrow)

     ! sa, ja, ia, matrix A in compressed sparse row format
     integer, intent(in)      :: ia(nrow+1)
     integer, intent(in)      :: ja(nmax)
     complex(dp), intent(in)  :: sa(nmax)

     ! sb, jb, ib, resulting matrix B in compressed sparse row format
     integer, intent(out)     :: ib(nrow+1)
     integer, intent(out)     :: jb(nmax)
     complex(dp), intent(out) :: sb(nmax)

!! local variables
     ! loop index
     integer :: i
     integer :: k

     ! loop index
     integer :: k1
     integer :: k2

!! [body

     ! init B sparse matrix
     sb = dcmplx(0.0_dp, 0.0_dp)
     ib = 0
     jb = 0

     ! normalize each row
     do i=1,nrow
         k1 = ia(i)
         k2 = ia(i+1) - 1
         do k=k1,k2
             sb(k) = sa(k) * diag(i)
         enddo ! over k={k1,k2} loop
     enddo ! over i={1,nrow} loop

     do i=1,nrow+1
         ib(i) = ia(i)
     enddo ! over i={1,nrow+1} loop

     do k=ia(1),ia(nrow+1)-1
         jb(k) = ja(k)
     enddo ! over k={ia(1),ia(nrow+1)-1} loop

!! body]

     return
  end subroutine sp_matmul_diamua_z
