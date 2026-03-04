!!!-----------------------------------------------------------------------
!!! project : flink @ sakura
!!! program : s_linspace
!!!           s_cumsum
!!!           s_cumprod
!!!           s_swap
!!!           s_mix
!!!           s_vecadd
!!!           s_dot
!!!           s_diff
!!!           s_stats
!!!           s_outer
!!!           s_cross
!!!           s_distance
!!!           s_unique
!!!           s_intersect
!!!           s_union
!!!           s_moment
!!!           s_skewness
!!!           s_kurtosis
!!!           s_reverse
!!!           s_clip
!!!           s_concat
!!!           s_norm1
!!!           s_norm2
!!!           s_norminf
!!!           s_shuffle
!!! source  : s_vector.f90
!!! type    : subroutines
!!! author  : li huang (email:huangli@caep.cn)
!!! history : 07/10/2014 by li huang (created)
!!!           03/02/2026 by li huang (last modified)
!!! purpose : these subroutines are designed for vectors or arrays. they
!!!           can be used to manipulate grid and mesh.
!!! status  : unstable
!!! comment :
!!!-----------------------------------------------------------------------

!!========================================================================
!!>>> mesh generation                                                  <<<
!!========================================================================

!!
!! @sub s_linspace_i
!!
!! create a linear mesh x in interval [xmin, xmax], integer version.
!!
  subroutine s_linspace_i(xmin, xmax, n, x)
     use constants, only : dp

     implicit none

!! external arguments
     ! left boundary
     integer, intent(in)  :: xmin

     ! right boundary
     integer, intent(in)  :: xmax

     ! size of array x
     integer, intent(in)  :: n

     ! output array, containing the linear mesh
     integer, intent(out) :: x(n)

!! local variables
     ! loop index
     integer :: i

!! [body

     do i=1,n
         x(i) = ( xmax - xmin ) * real(i - 1, dp) / real(n - 1, dp) + xmin
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_linspace_i

!!
!! @sub s_linspace_d
!!
!! create a linear mesh x in interval [xmin, xmax], real(dp) version.
!!
  subroutine s_linspace_d(xmin, xmax, n, x)
     use constants, only : dp

     implicit none

!! external arguments
     ! left boundary
     real(dp), intent(in)  :: xmin

     ! right boundary
     real(dp), intent(in)  :: xmax

     ! size of array x
     integer,  intent(in)  :: n

     ! output array, containing the linear mesh
     real(dp), intent(out) :: x(n)

!! local variables
     ! loop index
     integer :: i

!! [body

     do i=1,n
         x(i) = ( xmax - xmin ) * real(i - 1, dp) / real(n - 1, dp) + xmin
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_linspace_d

!!
!! @sub s_linspace_z
!!
!! create a linear mesh x in interval [xmin, xmax], complex(dp) version.
!!
  subroutine s_linspace_z(xmin, xmax, n, x)
     use constants, only : dp

     implicit none

!! external arguments
     ! left boundary
     complex(dp), intent(in)  :: xmin

     ! right boundary
     complex(dp), intent(in)  :: xmax

     ! size of array x
     integer,  intent(in)     :: n

     ! output array, containing the linear mesh
     complex(dp), intent(out) :: x(n)

!! local variables
     ! loop index
     integer :: i

!! [body

     do i=1,n
         x(i) = ( xmax - xmin ) * real(i - 1, dp) / real(n - 1, dp) + xmin
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_linspace_z

!!========================================================================
!!>>> sum operations                                                   <<<
!!========================================================================

!!
!! @sub s_cumsum_i
!!
!! return the cumulative sum of an integer array.
!!
  subroutine s_cumsum_i(n, v, vsum)
     implicit none

!! external arguments
     ! size of array v
     integer, intent(in)  :: n

     ! input integer array
     integer, intent(in)  :: v(n)

     ! cumulative sum of array v
     integer, intent(out) :: vsum(n)

!! local variables
     ! loop index
     integer :: i

!! [body

     vsum(1) = v(1)
     !
     do i=2,n
         vsum(i) = vsum(i-1) + v(i)
     enddo ! over i={2,n} loop

!! body]

     return
  end subroutine s_cumsum_i

!!
!! @sub s_cumsum_d
!!
!! return the cumulative sum of a real(dp) array.
!!
  subroutine s_cumsum_d(n, v, vsum)
     use constants, only : dp

     implicit none

!! external arguments
     ! size of array v
     integer, intent(in)   :: n

     ! input real(dp) array
     real(dp), intent(in)  :: v(n)

     ! cumulative sum of array v
     real(dp), intent(out) :: vsum(n)

!! local variables
     ! loop index
     integer :: i

!! [body

     vsum(1) = v(1)
     !
     do i=2,n
         vsum(i) = vsum(i-1) + v(i)
     enddo ! over i={2,n} loop

!! body]

     return
  end subroutine s_cumsum_d

!!
!! @sub s_cumsum_z
!!
!! return the cumulative sum of a complex(dp) array.
!!
  subroutine s_cumsum_z(n, v, vsum)
     use constants, only : dp

     implicit none

!! external arguments
     ! size of array v
     integer, intent(in)      :: n

     ! input complex(dp) array
     complex(dp), intent(in)  :: v(n)

     ! cumulative sum of array v
     complex(dp), intent(out) :: vsum(n)

!! local variables
     ! loop index
     integer :: i

!! [body

     vsum(1) = v(1)
     !
     do i=2,n
         vsum(i) = vsum(i-1) + v(i)
     enddo ! over i={2,n} loop

!! body]

     return
  end subroutine s_cumsum_z

!!========================================================================
!!>>> prod operations                                                  <<<
!!========================================================================

!!
!! @sub s_cumprod_i
!!
!! return the cumulative product of an integer array.
!!
  subroutine s_cumprod_i(n, v, vprod)
     implicit none

!! external arguments
     ! size of array v
     integer, intent(in)  :: n

     ! input integer array
     integer, intent(in)  :: v(n)

     ! cumulative product of array v
     integer, intent(out) :: vprod(n)

!! local variables
     ! loop index
     integer :: i

!! [body

     vprod(1) = v(1)
     !
     do i=2,n
         vprod(i) = vprod(i-1) * v(i)
     enddo ! over i={2,n} loop

!! body]

     return
  end subroutine s_cumprod_i

!!
!! @sub s_cumprod_d
!!
!! return the cumulative product of a real(dp) array.
!!
  subroutine s_cumprod_d(n, v, vprod)
     use constants, only : dp

     implicit none

!! external arguments
     ! size of array v
     integer, intent(in)   :: n

     ! input real(dp) array
     real(dp), intent(in)  :: v(n)

     ! cumulative product of array v
     real(dp), intent(out) :: vprod(n)

!! local variables
     ! loop index
     integer :: i

!! [body

     vprod(1) = v(1)
     !
     do i=2,n
         vprod(i) = vprod(i-1) * v(i)
     enddo ! over i={2,n} loop

!! body]

     return
  end subroutine s_cumprod_d

!!
!! @sub s_cumprod_z
!!
!! return the cumulative product of a complex(dp) array.
!!
  subroutine s_cumprod_z(n, v, vprod)
     use constants, only : dp

     implicit none

!! external arguments
     ! size of array v
     integer, intent(in)      :: n

     ! input complex(dp) array
     complex(dp), intent(in)  :: v(n)

     ! cumulative product of array v
     complex(dp), intent(out) :: vprod(n)

!! local variables
     ! loop index
     integer :: i

!! [body

     vprod(1) = v(1)
     !
     do i=2,n
         vprod(i) = vprod(i-1) * v(i)
     enddo ! over i={2,n} loop

!! body]

     return
  end subroutine s_cumprod_z

!!========================================================================
!!>>> swap operations                                                  <<<
!!========================================================================

!!
!! @sub s_swap_i
!!
!! exchange two integer vectors.
!!
  subroutine s_swap_i(n, ix, iy)
     implicit none

!! external arguments
     ! dimension of integer vector
     integer, intent(in)    :: n

     ! integer vector X
     integer, intent(inout) :: ix(n)

     ! integer vector Y
     integer, intent(inout) :: iy(n)

!! local variables
     ! dummy integer vector
     integer :: it(n)

!! [body

     it = ix
     ix = iy
     iy = it

!! body]

     return
  end subroutine s_swap_i

!!
!! @sub s_swap_d
!!
!! exchange two real(dp) vectors.
!!
  subroutine s_swap_d(n, dx, dy)
     use constants, only : dp

     implicit none

!! external arguments
     ! dimension of real(dp) vector
     integer, intent(in)     :: n

     ! real(dp) vector X
     real(dp), intent(inout) :: dx(n)

     ! real(dp) vector Y
     real(dp), intent(inout) :: dy(n)

!! local variables
     ! dummy real(dp) vector
     real(dp) :: dt(n)

!! [body

     dt = dx
     dx = dy
     dy = dt

!! body]

     return
  end subroutine s_swap_d

!!
!! @sub s_swap_z
!!
!! exchange two complex(dp) vectors.
!!
  subroutine s_swap_z(n, zx, zy)
     use constants, only : dp

     implicit none

!! external arguments
     ! dimension of complex(dp) vector
     integer, intent(in)        :: n

     ! complex(dp) vector X
     complex(dp), intent(inout) :: zx(n)

     ! complex(dp) vector Y
     complex(dp), intent(inout) :: zy(n)

!! local variables
     ! dummy complex(dp) vector
     complex(dp) :: zt(n)

!! [body

     zt = zx
     zx = zy
     zy = zt

!! body]

     return
  end subroutine s_swap_z

!!========================================================================
!!>>> mix operations                                                   <<<
!!========================================================================

!!
!! @sub s_mix_i
!!
!! linear mixing for two integer vectors.
!!
  subroutine s_mix_i(n, ix, iy, alpha)
     use constants, only : dp
     use constants, only : one

     implicit none

!! external arguments
     ! dimension of integer vector
     integer, intent(in)    :: n

     ! mixing parameter
     real(dp), intent(in)   :: alpha

     ! integer vector X
     integer, intent(in)    :: ix(n)

     ! integer vector Y
     integer, intent(inout) :: iy(n)

!! [body

     iy = int( real(ix) * (one - alpha) + real(iy) * alpha )

!! body]

     return
  end subroutine s_mix_i

!!
!! @sub s_mix_d
!!
!! linear mixing for two real(dp) vectors.
!!
  subroutine s_mix_d(n, dx, dy, alpha)
     use constants, only : dp
     use constants, only : one

     implicit none

!! external arguments
     ! dimension of real(dp) vector
     integer, intent(in)     :: n

     ! mixing parameter
     real(dp), intent(in)    :: alpha

     ! real(dp) vector X
     real(dp), intent(in)    :: dx(n)

     ! real(dp) vector Y
     real(dp), intent(inout) :: dy(n)

!! [body

     dy = dx * (one - alpha) + dy * alpha

!! body]

     return
  end subroutine s_mix_d

!!
!! @sub s_mix_z
!!
!! linear mixing for two complex(dp) vectors.
!!
  subroutine s_mix_z(n, zx, zy, alpha)
     use constants, only : dp
     use constants, only : one

     implicit none

!! external arguments
     ! dimension of complex(dp) vector
     integer, intent(in)        :: n

     ! mixing parameter
     real(dp), intent(in)       :: alpha

     ! complex(dp) vector X
     complex(dp), intent(in)    :: zx(n)

     ! complex(dp) vector Y
     complex(dp), intent(inout) :: zy(n)

!! [body

     zy = zx * (one - alpha) + zy * alpha

!! body]

     return
  end subroutine s_mix_z

!!========================================================================
!!>>> add operations                                                   <<<
!!========================================================================

!!
!! @sub s_vecadd_i
!!
!! add diagonal elements of a matrix to a vector, integer version.
!!
  subroutine s_vecadd_i(n, ix, iy, alpha)
     use constants, only : dp

     implicit none

!! external arguments
     ! dimension of integer vector
     integer, intent(in)    :: n

     ! prefactor
     real(dp), intent(in)   :: alpha

     ! integer vector X
     integer, intent(inout) :: ix(n)

     ! integer matrix Y
     integer, intent(in)    :: iy(n,n)

!! local variables
     ! loop index
     integer :: i

!! [body

     do i=1,n
         ix(i) = ix(i) + int( alpha * iy(i,i) )
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_vecadd_i

!!
!! @sub s_vecadd_d
!!
!! add diagonal elements of a matrix to a vector, real(dp) version.
!!
  subroutine s_vecadd_d(n, dx, dy, alpha)
     use constants, only : dp

     implicit none

!! external arguments
     ! dimension of real(dp) vector
     integer, intent(in)     :: n

     ! prefactor
     real(dp), intent(in)    :: alpha

     ! real(dp) vector X
     real(dp), intent(inout) :: dx(n)

     ! real(dp) matrix Y
     real(dp), intent(in)    :: dy(n,n)

!! local variables
     ! loop index
     integer :: i

!! [body

     do i=1,n
         dx(i) = dx(i) + alpha * dy(i,i)
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_vecadd_d

!!
!! @sub s_vecadd_z
!!
!! add diagonal elements of a matrix to a vector, complex(dp) version.
!!
  subroutine s_vecadd_z(n, zx, zy, alpha)
     use constants, only : dp

     implicit none

!! external arguments
     ! dimension of complex(dp) vector
     integer, intent(in)        :: n

     ! prefactor
     real(dp), intent(in)       :: alpha

     ! complex(dp) vector X
     complex(dp), intent(inout) :: zx(n)

     ! complex(dp) matrix Y
     complex(dp), intent(in)    :: zy(n,n)

!! local variables
     ! loop index
     integer :: i

!! [body

     do i=1,n
         zx(i) = zx(i) + alpha * zy(i,i)
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_vecadd_z

!!========================================================================
!!>>> dot operations                                                   <<<
!!========================================================================

!!
!! @sub s_dot_i
!!
!! compute dot product of two integer vectors.
!!
  subroutine s_dot_i(n, ix, iy, val)
     implicit none

!! external arguments
     ! dimension of integer vectors
     integer, intent(in)  :: n

     ! integer vector X
     integer, intent(in)  :: ix(n)

     ! integer vector Y
     integer, intent(in)  :: iy(n)

     ! dot product result
     integer, intent(out) :: val

!! local variables
     ! loop index
     integer :: i

!! [body

     if (n <= 0) then
         val = 0
         return
     endif
     !
     val = 0
     !
     do i=1,n
         val = val + ix(i) * iy(i)
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_dot_i

!!
!! @sub s_dot_d
!!
!! compute dot product of two real(dp) vectors.
!!
  subroutine s_dot_d(n, dx, dy, val)
     use constants, only : dp
     use constants, only : zero

     implicit none

!! external arguments
     ! dimension of real(dp) vectors
     integer, intent(in)   :: n

     ! real(dp) vector X
     real(dp), intent(in)  :: dx(n)

     ! real(dp) vector Y
     real(dp), intent(in)  :: dy(n)

     ! dot product result
     real(dp), intent(out) :: val

!! local variables
     ! loop index
     integer :: i

!! [body

     if (n <= 0) then
         val = zero
         return
     endif
     !
     val = zero
     !
     do i=1,n
         val = val + dx(i) * dy(i)
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_dot_d

!!
!! @sub s_dot_z
!!
!! compute dot product of two complex(dp) vectors.
!!
  subroutine s_dot_z(n, zx, zy, val)
     use constants, only : dp
     use constants, only : czero

     implicit none

!! external arguments
     ! dimension of complex(dp) vectors
     integer, intent(in)      :: n

     ! complex(dp) vector X
     complex(dp), intent(in)  :: zx(n)

     ! complex(dp) vector Y
     complex(dp), intent(in)  :: zy(n)

     ! dot product result
     complex(dp), intent(out) :: val

!! local variables
     ! loop index
     integer :: i

!! [body

     if (n <= 0) then
         val = czero
         return
     endif
     !
     val = czero
     !
     do i=1,n
         val = val + conjg(zx(i)) * zy(i)
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_dot_z

!!========================================================================
!!>>> diff operations                                                  <<<
!!========================================================================

!!
!! @sub s_diff_i
!!
!! compute finite differences of an integer vector.
!! result size is n-1: diff(i) = v(i+1) - v(i)
!!
  subroutine s_diff_i(n, iv, diff)
     implicit none

!! external arguments
     ! size of input array
     integer, intent(in)  :: n

     ! input integer array
     integer, intent(in)  :: iv(n)

     ! finite differences, size n-1
     integer, intent(out) :: diff(n-1)

!! local variables
     ! loop index
     integer :: i

!! [body
!!
     if (n < 2) return
     !
     do i=1,n-1
         diff(i) = iv(i+1) - iv(i)
     enddo ! over i={1,n-1} loop

!! body]

     return
  end subroutine s_diff_i

!!
!! @sub s_diff_d
!!
!! compute finite differences of a real(dp) vector.
!! result size is n-1: diff(i) = v(i+1) - v(i)
!!
  subroutine s_diff_d(n, dv, diff)
     use constants, only : dp

     implicit none

!! external arguments
     ! size of input array
     integer, intent(in)   :: n

     ! input real(dp) array
     real(dp), intent(in)  :: dv(n)

     ! finite differences, size n-1
     real(dp), intent(out) :: diff(n-1)

!! local variables
     ! loop index
     integer :: i

!! [body
!!
     if (n < 2) return
     !
     do i=1,n-1
         diff(i) = dv(i+1) - dv(i)
     enddo ! over i={1,n-1} loop

!! body]

     return
  end subroutine s_diff_d

!!
!! @sub s_diff_z
!!
!! compute finite differences of a complex(dp) vector.
!! result size is n-1: diff(i) = v(i+1) - v(i)
!!
  subroutine s_diff_z(n, zv, diff)
     use constants, only : dp

     implicit none

!! external arguments
     ! size of input array
     integer, intent(in)      :: n

     ! input complex(dp) array
     complex(dp), intent(in)  :: zv(n)

     ! finite differences, size n-1
     complex(dp), intent(out) :: diff(n-1)

!! local variables
     ! loop index
     integer :: i

!! [body
!!
     if (n < 2) return
     !
     do i=1,n-1
         diff(i) = zv(i+1) - zv(i)
     enddo ! over i={1,n-1} loop

!! body]

     return
  end subroutine s_diff_z

!!========================================================================
!!>>> statistics operation                                             <<<
!!========================================================================

!!
!! @sub s_stats_i
!!
!! compute mean and standard deviation of an integer vector.
!!
  subroutine s_stats_i(n, iv, mean, stddev)
     use constants, only : dp
     use constants, only : zero

     implicit none

!! external arguments
     ! size of array
     integer, intent(in)   :: n

     ! input integer array
     integer, intent(in)   :: iv(n)

     ! mean value
     real(dp), intent(out) :: mean

     ! standard deviation
     real(dp), intent(out) :: stddev

!! local variables
     ! loop index
     integer  :: i

     ! sum of squared deviations
     real(dp) :: sum_sq

     ! sum of values
     real(dp) :: sum_val

!! [body

     if (n <= 0) then
         mean = zero
         stddev = zero
         return
     endif

     ! compute mean
     sum_val = zero
     do i=1,n
         sum_val = sum_val + real(iv(i), dp)
     enddo
     mean = sum_val / real(n, dp)

     ! compute standard deviation
     if (n == 1) then
         stddev = zero
     else
         sum_sq = zero
         do i=1,n
             sum_sq = sum_sq + (real(iv(i), dp) - mean)**2
         enddo
         stddev = sqrt(sum_sq / real(n - 1, dp))
     endif

!! body]

     return
  end subroutine s_stats_i

!!
!! @sub s_stats_d
!!
!! compute mean and standard deviation of a real(dp) vector.
!!
  subroutine s_stats_d(n, dv, mean, stddev)
     use constants, only : dp
     use constants, only : zero

     implicit none

!! external arguments
     ! size of array
     integer, intent(in)   :: n

     ! input real(dp) array
     real(dp), intent(in)  :: dv(n)

     ! mean value
     real(dp), intent(out) :: mean

     ! standard deviation
     real(dp), intent(out) :: stddev

!! local variables
     ! loop index
     integer  :: i

     ! sum of squared deviations
     real(dp) :: sum_sq

     ! sum of values
     real(dp) :: sum_val

!! [body

     if (n <= 0) then
         mean = zero
         stddev = zero
         return
     endif

     ! compute mean
     sum_val = zero
     do i=1,n
         sum_val = sum_val + dv(i)
     enddo
     mean = sum_val / real(n, dp)

     ! compute standard deviation
     if (n == 1) then
         stddev = zero
     else
         sum_sq = zero
         do i=1,n
             sum_sq = sum_sq + (dv(i) - mean)**2
         enddo
         stddev = sqrt(sum_sq / real(n - 1, dp))
     endif

!! body]

     return
  end subroutine s_stats_d

!!
!! @sub s_stats_z
!!
!! compute mean and standard deviation of a complex(dp) vector.
!!
  subroutine s_stats_z(n, zv, mean, stddev)
     use constants, only : dp
     use constants, only : zero, czero

     implicit none

!! external arguments
     ! size of array
     integer, intent(in)      :: n

     ! input complex(dp) array
     complex(dp), intent(in)  :: zv(n)

     ! mean value
     complex(dp), intent(out) :: mean

     ! standard deviation
     real(dp), intent(out)    :: stddev

!! local variables
     ! loop index
     integer  :: i

     ! sum of squared deviations
     real(dp) :: sum_sq

     ! sum of values
     complex(dp) :: sum_val

!! [body

     if (n <= 0) then
         mean = czero
         stddev = zero
         return
     endif

     ! compute mean
     sum_val = czero
     do i=1,n
         sum_val = sum_val + zv(i)
     enddo
     mean = sum_val / real(n, dp)

     ! compute standard deviation (root mean square distance from mean)
     if (n == 1) then
         stddev = zero
     else
         sum_sq = zero
         do i=1,n
             sum_sq = sum_sq + abs(zv(i) - mean)**2
         enddo
         stddev = sqrt(sum_sq / real(n - 1, dp))
     endif

!! body]

     return
  end subroutine s_stats_z

!!========================================================================
!!>>> outer product operations                                         <<<
!!========================================================================

!!
!! @sub s_outer_i
!!
!! compute outer product of two integer vectors: a(i,j) = x(i) * y(j)
!!
  subroutine s_outer_i(n, m, ix, iy, ia)
     implicit none

!! external arguments
     ! size of input vector x
     integer, intent(in)  :: n

     ! size of input vector y
     integer, intent(in)  :: m

     ! input integer vector x
     integer, intent(in)  :: ix(n)

     ! input integer vector y
     integer, intent(in)  :: iy(m)

     ! output matrix: outer product a = x * y^T
     integer, intent(out) :: ia(n,m)

!! local variables
     ! loop indices
     integer :: i, j

!! [body

     if (n <= 0 .or. m <= 0) return
     !
     do i=1,n
         do j=1,m
             ia(i,j) = ix(i) * iy(j)
         enddo ! over j={1,m} loop
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_outer_i

!!
!! @sub s_outer_d
!!
!! compute outer product of two real(dp) vectors: a(i,j) = x(i) * y(j)
!!
  subroutine s_outer_d(n, m, dx, dy, da)
     use constants, only : dp

     implicit none

!! external arguments
     ! size of input vector x
     integer, intent(in)   :: n

     ! size of input vector y
     integer, intent(in)   :: m

     ! input real(dp) vector x
     real(dp), intent(in)  :: dx(n)

     ! input real(dp) vector y
     real(dp), intent(in)  :: dy(m)

     ! output matrix: outer product a = x * y^T
     real(dp), intent(out) :: da(n,m)

!! local variables
     ! loop indices
     integer :: i, j

!! [body

     if (n <= 0 .or. m <= 0) return
     !
     do i=1,n
         do j=1,m
             da(i,j) = dx(i) * dy(j)
         enddo ! over j={1,m} loop
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_outer_d

!!
!! @sub s_outer_z
!!
!! compute outer product of two complex(dp) vectors: a(i,j) = x(i) * y(j)
!!
  subroutine s_outer_z(n, m, zx, zy, za)
     use constants, only : dp

     implicit none

!! external arguments
     ! size of input vector x
     integer, intent(in)      :: n

     ! size of input vector y
     integer, intent(in)      :: m

     ! input complex(dp) vector x
     complex(dp), intent(in)  :: zx(n)

     ! input complex(dp) vector y
     complex(dp), intent(in)  :: zy(m)

     ! output matrix: outer product a = x * y^T
     complex(dp), intent(out) :: za(n,m)

!! local variables
     ! loop indices
     integer :: i, j

!! [body

     if (n <= 0 .or. m <= 0) return
     !
     do i=1,n
         do j=1,m
             za(i,j) = zx(i) * zy(j)
         enddo ! over j={1,m} loop
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_outer_z

!!========================================================================
!!>>> cross product operations                                         <<<
!!========================================================================

!!
!! @sub s_cross_i
!!
!! compute cross product of two 3d integer vectors.
!! c = a x b, where c(1) = a(2)*b(3) - a(3)*b(2)
!!                    c(2) = a(3)*b(1) - a(1)*b(3)
!!                    c(3) = a(1)*b(2) - a(2)*b(1)
!!
  subroutine s_cross_i(ix, iy, iz)
     implicit none

!! external arguments
     ! input 3d integer vector a
     integer, intent(in)  :: ix(3)

     ! input 3d integer vector b
     integer, intent(in)  :: iy(3)

     ! output 3d integer vector: cross product
     integer, intent(out) :: iz(3)

!! [body

     iz(1) = ix(2) * iy(3) - ix(3) * iy(2)
     iz(2) = ix(3) * iy(1) - ix(1) * iy(3)
     iz(3) = ix(1) * iy(2) - ix(2) * iy(1)

!! body]

     return
  end subroutine s_cross_i

!!
!! @sub s_cross_d
!!
!! compute cross product of two 3d real(dp) vectors.
!! c = a x b, where c(1) = a(2)*b(3) - a(3)*b(2)
!!                    c(2) = a(3)*b(1) - a(1)*b(3)
!!                    c(3) = a(1)*b(2) - a(2)*b(1)
!!
  subroutine s_cross_d(dx, dy, dz)
     use constants, only : dp

     implicit none

!! external arguments
     ! input 3d real(dp) vector a
     real(dp), intent(in)  :: dx(3)

     ! input 3d real(dp) vector b
     real(dp), intent(in)  :: dy(3)

     ! output 3d real(dp) vector: cross product
     real(dp), intent(out) :: dz(3)

!! [body

     dz(1) = dx(2) * dy(3) - dx(3) * dy(2)
     dz(2) = dx(3) * dy(1) - dx(1) * dy(3)
     dz(3) = dx(1) * dy(2) - dx(2) * dy(1)

!! body]

     return
  end subroutine s_cross_d

!!
!! @sub s_cross_z
!!
!! compute cross product of two 3d complex(dp) vectors.
!! c = a x b, where c(1) = a(2)*b(3) - a(3)*b(2)
!!                    c(2) = a(3)*b(1) - a(1)*b(3)
!!                    c(3) = a(1)*b(2) - a(2)*b(1)
!!
  subroutine s_cross_z(zx, zy, zz)
     use constants, only : dp

     implicit none

!! external arguments
     ! input 3d complex(dp) vector a
     complex(dp), intent(in)  :: zx(3)

     ! input 3d complex(dp) vector b
     complex(dp), intent(in)  :: zy(3)

     ! output 3d complex(dp) vector: cross product
     complex(dp), intent(out) :: zz(3)

!! [body

     zz(1) = zx(2) * zy(3) - zx(3) * zy(2)
     zz(2) = zx(3) * zy(1) - zx(1) * zy(3)
     zz(3) = zx(1) * zy(2) - zx(2) * zy(1)

!! body]

     return
  end subroutine s_cross_z

!!========================================================================
!!>>> distance operations                                              <<<
!!========================================================================

!!
!! @sub s_distance_i
!!
!! compute euclidean distance between two integer vectors.
!! dist = sqrt( sum_i (x(i) - y(i))^2 )
!!
  subroutine s_distance_i(n, ix, iy, dist)
     use constants, only : dp
     use constants, only : zero

     implicit none

!! external arguments
     ! size of vectors
     integer, intent(in)   :: n

     ! input integer vector x
     integer, intent(in)   :: ix(n)

     ! input integer vector y
     integer, intent(in)   :: iy(n)

     ! euclidean distance: ||x - y||
     real(dp), intent(out) :: dist

!! local variables
     ! loop index
     integer :: i

     ! squared differences sum
     real(dp) :: sum_sq

!! [body

     if (n <= 0) then
         dist = zero
         return
     endif
     !
     sum_sq = zero
     do i=1,n
         sum_sq = sum_sq + real(ix(i) - iy(i), dp)**2
     enddo ! over i={1,n} loop
     dist = sqrt(sum_sq)

!! body]

     return
  end subroutine s_distance_i

!!
!! @sub s_distance_d
!!
!! compute euclidean distance between two real(dp) vectors.
!! dist = sqrt( sum_i (x(i) - y(i))^2 )
!!
  subroutine s_distance_d(n, dx, dy, dist)
     use constants, only : dp
     use constants, only : zero

     implicit none

!! external arguments
     ! size of vectors
     integer, intent(in)   :: n

     ! input real(dp) vector x
     real(dp), intent(in)  :: dx(n)

     ! input real(dp) vector y
     real(dp), intent(in)  :: dy(n)

     ! euclidean distance: ||x - y||
     real(dp), intent(out) :: dist

!! local variables
     ! loop index
     integer :: i

     ! squared differences sum
     real(dp) :: sum_sq

!! [body

     if (n <= 0) then
         dist = zero
         return
     endif
     !
     sum_sq = zero
     do i=1,n
         sum_sq = sum_sq + (dx(i) - dy(i))**2
     enddo ! over i={1,n} loop
     dist = sqrt(sum_sq)

!! body]

     return
  end subroutine s_distance_d

!!
!! @sub s_distance_z
!!
!! compute euclidean distance between two complex(dp) vectors.
!! dist = sqrt( sum_i |x(i) - y(i)|^2 )
!!
  subroutine s_distance_z(n, zx, zy, dist)
     use constants, only : dp
     use constants, only : zero

     implicit none

!! external arguments
     ! size of vectors
     integer, intent(in)      :: n

     ! input complex(dp) vector x
     complex(dp), intent(in)  :: zx(n)

     ! input complex(dp) vector y
     complex(dp), intent(in)  :: zy(n)

     ! euclidean distance: ||x - y||
     real(dp), intent(out)    :: dist

!! local variables
     ! loop index
     integer :: i

     ! squared differences sum
     real(dp) :: sum_sq

!! [body

     if (n <= 0) then
         dist = zero
         return
     endif
     !
     sum_sq = zero
     do i=1,n
         sum_sq = sum_sq + abs(zx(i) - zy(i))**2
     enddo ! over i={1,n} loop
     dist = sqrt(sum_sq)

!! body]

     return
  end subroutine s_distance_z

!!========================================================================
!!>>> unique operations                                                <<<
!!========================================================================

!!
!! @sub s_unique_i
!!
!! return unique elements from an integer array, preserving order.
!! returns the number of unique elements in m.
!!
  subroutine s_unique_i(n, ix, m, iy)
     implicit none

!! external arguments
     ! size of input array
     integer, intent(in)  :: n

     ! input integer array
     integer, intent(in)  :: ix(n)

     ! number of unique elements (output)
     integer, intent(out) :: m

     ! unique elements (output, size n, but only m elements are valid)
     integer, intent(out) :: iy(n)

!! local variables
     ! loop indices
     integer :: i, j

     ! flag for uniqueness
     logical :: is_unique

!! [body

     if (n <= 0) then
         m = 0
         return
     endif
     !
     m = 0
     do i=1,n
         is_unique = .true.
         do j=1,m
             if (ix(i) == iy(j)) then
                 is_unique = .false.
                 exit
             endif
         enddo ! over j={1,m} loop
         if (is_unique) then
             m = m + 1
             iy(m) = ix(i)
         endif
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_unique_i

!!
!! @sub s_unique_d
!!
!! return unique elements from a real(dp) array, preserving order.
!! returns the number of unique elements in m.
!!
  subroutine s_unique_d(n, dx, m, dy)
     use constants, only : dp
     use constants, only : eps8

     implicit none

!! external arguments
     ! size of input array
     integer, intent(in)   :: n

     ! input real(dp) array
     real(dp), intent(in)  :: dx(n)

     ! number of unique elements (output)
     integer, intent(out)  :: m

     ! unique elements (output, size n, but only m elements are valid)
     real(dp), intent(out) :: dy(n)

!! local variables
     ! loop indices
     integer :: i, j

     ! flag for uniqueness
     logical :: is_unique

!! [body

     if (n <= 0) then
         m = 0
         return
     endif
     !
     m = 0
     do i=1,n
         is_unique = .true.
         do j=1,m
             if (abs(dx(i) - dy(j)) < eps8) then
                 is_unique = .false.
                 exit
             endif
         enddo ! over j={1,m} loop
         if (is_unique) then
             m = m + 1
             dy(m) = dx(i)
         endif
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_unique_d

!!
!! @sub s_unique_z
!!
!! return unique elements from a complex(dp) array, preserving order.
!! returns the number of unique elements in m.
!!
  subroutine s_unique_z(n, zx, m, zy)
     use constants, only : dp
     use constants, only : eps8

     implicit none

!! external arguments
     ! size of input array
     integer, intent(in)      :: n

     ! input complex(dp) array
     complex(dp), intent(in)  :: zx(n)

     ! number of unique elements (output)
     integer, intent(out)     :: m

     ! unique elements (output, size n, but only m elements are valid)
     complex(dp), intent(out) :: zy(n)

!! local variables
     ! loop indices
     integer :: i, j

     ! flag for uniqueness
     logical :: is_unique

!! [body

     if (n <= 0) then
         m = 0
         return
     endif
     !
     m = 0
     do i=1,n
         is_unique = .true.
         do j=1,m
             if (abs(zx(i) - zy(j)) < eps8) then
                 is_unique = .false.
                 exit
             endif
         enddo ! over j={1,m} loop
         if (is_unique) then
             m = m + 1
             zy(m) = zx(i)
         endif
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_unique_z

!!========================================================================
!!>>> intersect operations                                             <<<
!!========================================================================

!!
!! @sub s_intersect_i
!!
!! compute intersection of two integer vectors.
!! returns common elements in iz, with k elements total.
!! each element appears only once (unique).
!!
  subroutine s_intersect_i(n, ix, m, iy, k, iz)
     implicit none

!! external arguments
     ! size of first vector
     integer, intent(in)  :: n

     ! first input integer vector x
     integer, intent(in)  :: ix(n)

     ! size of second vector
     integer, intent(in)  :: m

     ! second input integer vector y
     integer, intent(in)  :: iy(m)

     ! number of intersection elements (output)
     integer, intent(out) :: k

     ! intersection elements (output, size n, but only k valid)
     integer, intent(out) :: iz(n)

!! local variables
     ! loop indices
     integer :: i, j, l

     ! flag for membership
     logical :: in_y
     logical :: already_added

!! [body

     if (n <= 0 .or. m <= 0) then
         k = 0
         return
     endif
     !
     k = 0
     do i=1,n
         ! check if ix(i) is in iy
         in_y = .false.
         do j=1,m
             if (ix(i) == iy(j)) then
                 in_y = .true.
                 exit
             endif
         enddo ! over j={1,m} loop
         !
         if (in_y) then
             ! check if already added to result
             already_added = .false.
             do l=1,k
                 if (ix(i) == iz(l)) then
                     already_added = .true.
                     exit
                 endif
             enddo ! over l={1,k} loop
             if (.not. already_added) then
                 k = k + 1
                 iz(k) = ix(i)
             endif
         endif
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_intersect_i

!!
!! @sub s_intersect_d
!!
!! compute intersection of two real(dp) vectors.
!! returns common elements in dz, with k elements total.
!! each element appears only once (unique).
!!
  subroutine s_intersect_d(n, dx, m, dy, k, dz)
     use constants, only : dp
     use constants, only : eps8

     implicit none

!! external arguments
     ! size of first vector
     integer, intent(in)   :: n

     ! first input real(dp) vector x
     real(dp), intent(in)  :: dx(n)

     ! size of second vector
     integer, intent(in)   :: m

     ! second input real(dp) vector y
     real(dp), intent(in)  :: dy(m)

     ! number of intersection elements (output)
     integer, intent(out)  :: k

     ! intersection elements (output, size n, but only k valid)
     real(dp), intent(out) :: dz(n)

!! local variables
     ! loop indices
     integer :: i, j, l

     ! flag for membership
     logical :: in_y
     logical :: already_added

!! [body

     if (n <= 0 .or. m <= 0) then
         k = 0
         return
     endif
     !
     k = 0
     do i=1,n
         ! check if dx(i) is in dy
         in_y = .false.
         do j=1,m
             if (abs(dx(i) - dy(j)) < eps8) then
                 in_y = .true.
                 exit
             endif
         enddo ! over j={1,m} loop
         !
         if (in_y) then
             ! check if already added to result
             already_added = .false.
             do l=1,k
                 if (abs(dx(i) - dz(l)) < eps8) then
                     already_added = .true.
                     exit
                 endif
             enddo ! over l={1,k} loop
             if (.not. already_added) then
                 k = k + 1
                 dz(k) = dx(i)
             endif
         endif
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_intersect_d

!!
!! @sub s_intersect_z
!!
!! compute intersection of two complex(dp) vectors.
!! returns common elements in zz, with k elements total.
!! each element appears only once (unique).
!!
  subroutine s_intersect_z(n, zx, m, zy, k, zz)
     use constants, only : dp
     use constants, only : eps8

     implicit none

!! external arguments
     ! size of first vector
     integer, intent(in)      :: n

     ! first input complex(dp) vector x
     complex(dp), intent(in)  :: zx(n)

     ! size of second vector
     integer, intent(in)      :: m

     ! second input complex(dp) vector y
     complex(dp), intent(in)  :: zy(m)

     ! number of intersection elements (output)
     integer, intent(out)     :: k

     ! intersection elements (output, size n, but only k valid)
     complex(dp), intent(out) :: zz(n)

!! local variables
     ! loop indices
     integer :: i, j, l

     ! flag for membership
     logical :: in_y
     logical :: already_added

!! [body

     if (n <= 0 .or. m <= 0) then
         k = 0
         return
     endif
     !
     k = 0
     do i=1,n
         ! check if zx(i) is in zy
         in_y = .false.
         do j=1,m
             if (abs(zx(i) - zy(j)) < eps8) then
                 in_y = .true.
                 exit
             endif
         enddo ! over j={1,m} loop
         !
         if (in_y) then
             ! check if already added to result
             already_added = .false.
             do l=1,k
                 if (abs(zx(i) - zz(l)) < eps8) then
                     already_added = .true.
                     exit
                 endif
             enddo ! over l={1,k} loop
             if (.not. already_added) then
                 k = k + 1
                 zz(k) = zx(i)
             endif
         endif
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_intersect_z

!!========================================================================
!!>>> union operations                                                 <<<
!!========================================================================

!!
!! @sub s_union_i
!!
!! compute union of two integer vectors.
!! returns all unique elements from both vectors in iz, with k elements total.
!! elements from first vector x appear first, then new elements from y.
!!
  subroutine s_union_i(n, ix, m, iy, k, iz)
     implicit none

!! external arguments
     ! size of first vector
     integer, intent(in)  :: n

     ! first input integer vector x
     integer, intent(in)  :: ix(n)

     ! size of second vector
     integer, intent(in)  :: m

     ! second input integer vector y
     integer, intent(in)  :: iy(m)

     ! number of union elements (output)
     integer, intent(out) :: k

     ! union elements (output, size n+m)
     integer, intent(out) :: iz(n+m)

!! local variables
     ! loop indices
     integer :: i, j

     ! flag for uniqueness
     logical :: is_unique

!! [body

     ! first, add all unique elements from x
     k = 0
     do i=1,n
         is_unique = .true.
         do j=1,k
             if (ix(i) == iz(j)) then
                 is_unique = .false.
                 exit
             endif
         enddo ! over j={1,k} loop
         if (is_unique) then
             k = k + 1
             iz(k) = ix(i)
         endif
     enddo ! over i={1,n} loop
     !
     ! then, add unique elements from y that are not in x
     do i=1,m
         is_unique = .true.
         do j=1,k
             if (iy(i) == iz(j)) then
                 is_unique = .false.
                 exit
             endif
         enddo ! over j={1,k} loop
         if (is_unique) then
             k = k + 1
             iz(k) = iy(i)
         endif
     enddo ! over i={1,m} loop

!! body]

     return
  end subroutine s_union_i

!!
!! @sub s_union_d
!!
!! compute union of two real(dp) vectors.
!! returns all unique elements from both vectors in dz, with k elements total.
!! elements from first vector x appear first, then new elements from y.
!!
  subroutine s_union_d(n, dx, m, dy, k, dz)
     use constants, only : dp
     use constants, only : eps8

     implicit none

!! external arguments
     ! size of first vector
     integer, intent(in)   :: n

     ! first input real(dp) vector x
     real(dp), intent(in)  :: dx(n)

     ! size of second vector
     integer, intent(in)   :: m

     ! second input real(dp) vector y
     real(dp), intent(in)  :: dy(m)

     ! number of union elements (output)
     integer, intent(out)  :: k

     ! union elements (output, size n+m)
     real(dp), intent(out) :: dz(n+m)

!! local variables
     ! loop indices
     integer :: i, j

     ! flag for uniqueness
     logical :: is_unique

!! [body

     ! first, add all unique elements from x
     k = 0
     do i=1,n
         is_unique = .true.
         do j=1,k
             if (abs(dx(i) - dz(j)) < eps8) then
                 is_unique = .false.
                 exit
             endif
         enddo ! over j={1,k} loop
         if (is_unique) then
             k = k + 1
             dz(k) = dx(i)
         endif
     enddo ! over i={1,n} loop
     !
     ! then, add unique elements from y that are not in x
     do i=1,m
         is_unique = .true.
         do j=1,k
             if (abs(dy(i) - dz(j)) < eps8) then
                 is_unique = .false.
                 exit
             endif
         enddo ! over j={1,k} loop
         if (is_unique) then
             k = k + 1
             dz(k) = dy(i)
         endif
     enddo ! over i={1,m} loop

!! body]

     return
  end subroutine s_union_d

!!
!! @sub s_union_z
!!
!! compute union of two complex(dp) vectors.
!! returns all unique elements from both vectors in zz, with k elements total.
!! elements from first vector x appear first, then new elements from y.
!!
  subroutine s_union_z(n, zx, m, zy, k, zz)
     use constants, only : dp
     use constants, only : eps8

     implicit none

!! external arguments
     ! size of first vector
     integer, intent(in)      :: n

     ! first input complex(dp) vector x
     complex(dp), intent(in)  :: zx(n)

     ! size of second vector
     integer, intent(in)      :: m

     ! second input complex(dp) vector y
     complex(dp), intent(in)  :: zy(m)

     ! number of union elements (output)
     integer, intent(out)     :: k

     ! union elements (output, size n+m)
     complex(dp), intent(out) :: zz(n+m)

!! local variables
     ! loop indices
     integer :: i, j

     ! flag for uniqueness
     logical :: is_unique

!! [body

     ! first, add all unique elements from x
     k = 0
     do i=1,n
         is_unique = .true.
         do j=1,k
             if (abs(zx(i) - zz(j)) < eps8) then
                 is_unique = .false.
                 exit
             endif
         enddo ! over j={1,k} loop
         if (is_unique) then
             k = k + 1
             zz(k) = zx(i)
         endif
     enddo ! over i={1,n} loop
     !
     ! then, add unique elements from y that are not in x
     do i=1,m
         is_unique = .true.
         do j=1,k
             if (abs(zy(i) - zz(j)) < eps8) then
                 is_unique = .false.
                 exit
             endif
         enddo ! over j={1,k} loop
         if (is_unique) then
             k = k + 1
             zz(k) = zy(i)
         endif
     enddo ! over i={1,m} loop

!! body]

     return
  end subroutine s_union_z

!!========================================================================
!!>>> moment operations                                                <<<
!!========================================================================

!!
!! @sub s_moment_i
!!
!! compute k-th moment about the mean of an integer vector.
!! moment = sum_i (x(i) - mean)^k / n
!!
  subroutine s_moment_i(n, iv, order, moment)
     use constants, only : dp
     use constants, only : zero

     implicit none

!! external arguments
     ! size of array
     integer, intent(in)   :: n

     ! input integer array
     integer, intent(in)   :: iv(n)

     ! order of moment (1=mean, 2=variance, 3=skewness related, etc.)
     integer, intent(in)   :: order

     ! k-th moment value
     real(dp), intent(out) :: moment

!! local variables
     ! loop index
     integer :: i

     ! mean value
     real(dp) :: mean

     ! sum of values
     real(dp) :: sum_val

!! [body

     if (n <= 0) then
         moment = zero
         return
     endif
     !
     ! compute mean
     sum_val = zero
     do i=1,n
         sum_val = sum_val + real(iv(i), dp)
     enddo
     mean = sum_val / real(n, dp)
     !
     ! compute k-th moment
     moment = zero
     do i=1,n
         moment = moment + (real(iv(i), dp) - mean)**order
     enddo
     moment = moment / real(n, dp)

!! body]

     return
  end subroutine s_moment_i

!!
!! @sub s_moment_d
!!
!! compute k-th moment about the mean of a real(dp) vector.
!! moment = sum_i (x(i) - mean)^k / n
!!
  subroutine s_moment_d(n, dv, order, moment)
     use constants, only : dp
     use constants, only : zero

     implicit none

!! external arguments
     ! size of array
     integer, intent(in)   :: n

     ! input real(dp) array
     real(dp), intent(in)  :: dv(n)

     ! order of moment (1=mean, 2=variance, 3=skewness related, etc.)
     integer, intent(in)   :: order

     ! k-th moment value
     real(dp), intent(out) :: moment

!! local variables
     ! loop index
     integer :: i

     ! mean value
     real(dp) :: mean

     ! sum of values
     real(dp) :: sum_val

!! [body

     if (n <= 0) then
         moment = zero
         return
     endif
     !
     ! compute mean
     sum_val = zero
     do i=1,n
         sum_val = sum_val + dv(i)
     enddo
     mean = sum_val / real(n, dp)
     !
     ! compute k-th moment
     moment = zero
     do i=1,n
         moment = moment + (dv(i) - mean)**order
     enddo
     moment = moment / real(n, dp)

!! body]

     return
  end subroutine s_moment_d

!!
!! @sub s_moment_z
!!
!! compute k-th moment about the mean of a complex(dp) vector.
!! moment = sum_i |z(i) - mean|^k / n
!!
  subroutine s_moment_z(n, zv, order, moment)
     use constants, only : dp
     use constants, only : zero, czero

     implicit none

!! external arguments
     ! size of array
     integer, intent(in)      :: n

     ! input complex(dp) array
     complex(dp), intent(in)  :: zv(n)

     ! order of moment (1=mean, 2=variance, 3=skewness related, etc.)
     integer, intent(in)      :: order

     ! k-th moment value
     real(dp), intent(out)    :: moment

!! local variables
     ! loop index
     integer :: i

     ! mean value
     complex(dp) :: mean

     ! sum of values
     complex(dp) :: sum_val

!! [body

     if (n <= 0) then
         moment = zero
         return
     endif
     !
     ! compute mean
     sum_val = czero
     do i=1,n
         sum_val = sum_val + zv(i)
     enddo
     mean = sum_val / real(n, dp)
     !
     ! compute k-th moment (based on magnitude)
     moment = zero
     do i=1,n
         moment = moment + abs(zv(i) - mean)**order
     enddo
     moment = moment / real(n, dp)

!! body]

     return
  end subroutine s_moment_z

!!========================================================================
!!>>> skewness operations                                              <<<
!!========================================================================

!!
!! @sub s_skewness_i
!!
!! compute skewness of an integer vector.
!! skewness = (1/n * sum_i (x(i) - mean)^3) / std^3
!!
  subroutine s_skewness_i(n, iv, skewness)
     use constants, only : dp
     use constants, only : zero

     implicit none

!! external arguments
     ! size of array
     integer, intent(in)   :: n

     ! input integer array
     integer, intent(in)   :: iv(n)

     ! skewness value
     real(dp), intent(out) :: skewness

!! local variables
     ! loop index
     integer :: i

     ! mean value
     real(dp) :: mean

     ! standard deviation
     real(dp) :: stddev

     ! sum of squared deviations
     real(dp) :: sum_sq

     ! sum of cubed deviations
     real(dp) :: sum_cu

     ! sum of values
     real(dp) :: sum_val

!! [body

     if (n <= 0) then
         skewness = zero
         return
     endif
     !
     ! compute mean
     sum_val = zero
     do i=1,n
         sum_val = sum_val + real(iv(i), dp)
     enddo
     mean = sum_val / real(n, dp)
     !
     ! compute standard deviation
     if (n == 1) then
         skewness = zero
         return
     endif
     !
     sum_sq = zero
     do i=1,n
         sum_sq = sum_sq + (real(iv(i), dp) - mean)**2
     enddo
     stddev = sqrt(sum_sq / real(n - 1, dp))
     !
     if (stddev == zero) then
         skewness = zero
         return
     endif
     !
     ! compute skewness
     sum_cu = zero
     do i=1,n
         sum_cu = sum_cu + (real(iv(i), dp) - mean)**3
     enddo
     skewness = (sum_cu / real(n, dp)) / (stddev**3)

!! body]

     return
  end subroutine s_skewness_i

!!
!! @sub s_skewness_d
!!
!! compute skewness of a real(dp) vector.
!! skewness = (1/n * sum_i (x(i) - mean)^3) / std^3
!!
  subroutine s_skewness_d(n, dv, skewness)
     use constants, only : dp
     use constants, only : zero

     implicit none

!! external arguments
     ! size of array
     integer, intent(in)   :: n

     ! input real(dp) array
     real(dp), intent(in)  :: dv(n)

     ! skewness value
     real(dp), intent(out) :: skewness

!! local variables
     ! loop index
     integer :: i

     ! mean value
     real(dp) :: mean

     ! standard deviation
     real(dp) :: stddev

     ! sum of squared deviations
     real(dp) :: sum_sq

     ! sum of cubed deviations
     real(dp) :: sum_cu

     ! sum of values
     real(dp) :: sum_val

!! [body

     if (n <= 0) then
         skewness = zero
         return
     endif
     !
     ! compute mean
     sum_val = zero
     do i=1,n
         sum_val = sum_val + dv(i)
     enddo
     mean = sum_val / real(n, dp)
     !
     ! compute standard deviation
     if (n == 1) then
         skewness = zero
         return
     endif
     !
     sum_sq = zero
     do i=1,n
         sum_sq = sum_sq + (dv(i) - mean)**2
     enddo
     stddev = sqrt(sum_sq / real(n - 1, dp))
     !
     if (stddev == zero) then
         skewness = zero
         return
     endif
     !
     ! compute skewness
     sum_cu = zero
     do i=1,n
         sum_cu = sum_cu + (dv(i) - mean)**3
     enddo
     skewness = (sum_cu / real(n, dp)) / (stddev**3)

!! body]

     return
  end subroutine s_skewness_d

!!
!! @sub s_skewness_z
!!
!! compute skewness of a complex(dp) vector.
!! skewness = (1/n * sum_i |z(i) - mean|^3) / std^3
!!
  subroutine s_skewness_z(n, zv, skewness)
     use constants, only : dp
     use constants, only : zero, czero

     implicit none

!! external arguments
     ! size of array
     integer, intent(in)      :: n

     ! input complex(dp) array
     complex(dp), intent(in)  :: zv(n)

     ! skewness value
     real(dp), intent(out)    :: skewness

!! local variables
     ! loop index
     integer :: i

     ! mean value
     complex(dp) :: mean

     ! standard deviation
     real(dp) :: stddev

     ! sum of squared deviations
     real(dp) :: sum_sq

     ! sum of cubed deviations
     real(dp) :: sum_cu

     ! sum of values
     complex(dp) :: sum_val

!! [body

     if (n <= 0) then
         skewness = zero
         return
     endif
     !
     ! compute mean
     sum_val = czero
     do i=1,n
         sum_val = sum_val + zv(i)
     enddo
     mean = sum_val / real(n, dp)
     !
     ! compute standard deviation (based on magnitude)
     if (n == 1) then
         skewness = zero
         return
     endif
     !
     sum_sq = zero
     do i=1,n
         sum_sq = sum_sq + abs(zv(i) - mean)**2
     enddo
     stddev = sqrt(sum_sq / real(n - 1, dp))
     !
     if (stddev == zero) then
         skewness = zero
         return
     endif
     !
     ! compute skewness (based on magnitude)
     sum_cu = zero
     do i=1,n
         sum_cu = sum_cu + abs(zv(i) - mean)**3
     enddo
     skewness = (sum_cu / real(n, dp)) / (stddev**3)

!! body]

     return
  end subroutine s_skewness_z

!!========================================================================
!!>>> kurtosis operations                                              <<<
!!========================================================================

!!
!! @sub s_kurtosis_i
!!
!! compute kurtosis of an integer vector.
!! kurtosis = (1/n * sum_i (x(i) - mean)^4) / std^4
!!
  subroutine s_kurtosis_i(n, iv, kurtosis)
     use constants, only : dp
     use constants, only : zero

     implicit none

!! external arguments
     ! size of array
     integer, intent(in)   :: n

     ! input integer array
     integer, intent(in)   :: iv(n)

     ! kurtosis value
     real(dp), intent(out) :: kurtosis

!! local variables
     ! loop index
     integer :: i

     ! mean value
     real(dp) :: mean

     ! standard deviation
     real(dp) :: stddev

     ! sum of squared deviations
     real(dp) :: sum_sq

     ! sum of fourth power deviations
     real(dp) :: sum_qu

     ! sum of values
     real(dp) :: sum_val

!! [body

     if (n <= 0) then
         kurtosis = zero
         return
     endif
     !
     ! compute mean
     sum_val = zero
     do i=1,n
         sum_val = sum_val + real(iv(i), dp)
     enddo
     mean = sum_val / real(n, dp)
     !
     ! compute standard deviation
     if (n == 1) then
         kurtosis = zero
         return
     endif
     !
     sum_sq = zero
     do i=1,n
         sum_sq = sum_sq + (real(iv(i), dp) - mean)**2
     enddo
     stddev = sqrt(sum_sq / real(n - 1, dp))
     !
     if (stddev == zero) then
         kurtosis = zero
         return
     endif
     !
     ! compute kurtosis
     sum_qu = zero
     do i=1,n
         sum_qu = sum_qu + (real(iv(i), dp) - mean)**4
     enddo
     kurtosis = (sum_qu / real(n, dp)) / (stddev**4)

!! body]

     return
  end subroutine s_kurtosis_i

!!
!! @sub s_kurtosis_d
!!
!! compute kurtosis of a real(dp) vector.
!! kurtosis = (1/n * sum_i (x(i) - mean)^4) / std^4
!!
  subroutine s_kurtosis_d(n, dv, kurtosis)
     use constants, only : dp
     use constants, only : zero

     implicit none

!! external arguments
     ! size of array
     integer, intent(in)   :: n

     ! input real(dp) array
     real(dp), intent(in)  :: dv(n)

     ! kurtosis value
     real(dp), intent(out) :: kurtosis

!! local variables
     ! loop index
     integer :: i

     ! mean value
     real(dp) :: mean

     ! standard deviation
     real(dp) :: stddev

     ! sum of squared deviations
     real(dp) :: sum_sq

     ! sum of fourth power deviations
     real(dp) :: sum_qu

     ! sum of values
     real(dp) :: sum_val

!! [body

     if (n <= 0) then
         kurtosis = zero
         return
     endif
     !
     ! compute mean
     sum_val = zero
     do i=1,n
         sum_val = sum_val + dv(i)
     enddo
     mean = sum_val / real(n, dp)
     !
     ! compute standard deviation
     if (n == 1) then
         kurtosis = zero
         return
     endif
     !
     sum_sq = zero
     do i=1,n
         sum_sq = sum_sq + (dv(i) - mean)**2
     enddo
     stddev = sqrt(sum_sq / real(n - 1, dp))
     !
     if (stddev == zero) then
         kurtosis = zero
         return
     endif
     !
     ! compute kurtosis
     sum_qu = zero
     do i=1,n
         sum_qu = sum_qu + (dv(i) - mean)**4
     enddo
     kurtosis = (sum_qu / real(n, dp)) / (stddev**4)

!! body]

     return
  end subroutine s_kurtosis_d

!!
!! @sub s_kurtosis_z
!!
!! compute kurtosis of a complex(dp) vector.
!! kurtosis = (1/n * sum_i |z(i) - mean|^4) / std^4
!!
  subroutine s_kurtosis_z(n, zv, kurtosis)
     use constants, only : dp
     use constants, only : zero, czero

     implicit none

!! external arguments
     ! size of array
     integer, intent(in)      :: n

     ! input complex(dp) array
     complex(dp), intent(in)  :: zv(n)

     ! kurtosis value
     real(dp), intent(out)    :: kurtosis

!! local variables
     ! loop index
     integer :: i

     ! mean value
     complex(dp) :: mean

     ! standard deviation
     real(dp) :: stddev

     ! sum of squared deviations
     real(dp) :: sum_sq

     ! sum of fourth power deviations
     real(dp) :: sum_qu

     ! sum of values
     complex(dp) :: sum_val

!! [body

     if (n <= 0) then
         kurtosis = zero
         return
     endif
     !
     ! compute mean
     sum_val = czero
     do i=1,n
         sum_val = sum_val + zv(i)
     enddo
     mean = sum_val / real(n, dp)
     !
     ! compute standard deviation (based on magnitude)
     if (n == 1) then
         kurtosis = zero
         return
     endif
     !
     sum_sq = zero
     do i=1,n
         sum_sq = sum_sq + abs(zv(i) - mean)**2
     enddo
     stddev = sqrt(sum_sq / real(n - 1, dp))
     !
     if (stddev == zero) then
         kurtosis = zero
         return
     endif
     !
     ! compute kurtosis (based on magnitude)
     sum_qu = zero
     do i=1,n
         sum_qu = sum_qu + abs(zv(i) - mean)**4
     enddo
     kurtosis = (sum_qu / real(n, dp)) / (stddev**4)

!! body]

     return
  end subroutine s_kurtosis_z

!!========================================================================
!!>>> reverse operations                                               <<<
!!========================================================================

!!
!! @sub s_reverse_i
!!
!! reverse an integer array: y(i) = x(n+1-i)
!!
  subroutine s_reverse_i(n, ix, iy)
     implicit none

!! external arguments
     ! size of array
     integer, intent(in)  :: n

     ! input integer array
     integer, intent(in)  :: ix(n)

     ! reversed integer array
     integer, intent(out) :: iy(n)

!! local variables
     ! loop index
     integer :: i

!! [body

     do i=1,n
         iy(i) = ix(n + 1 - i)
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_reverse_i

!!
!! @sub s_reverse_d
!!
!! reverse a real(dp) array: y(i) = x(n+1-i)
!!
  subroutine s_reverse_d(n, dx, dy)
     use constants, only : dp

     implicit none

!! external arguments
     ! size of array
     integer, intent(in)    :: n

     ! input real(dp) array
     real(dp), intent(in)   :: dx(n)

     ! reversed real(dp) array
     real(dp), intent(out)  :: dy(n)

!! local variables
     ! loop index
     integer :: i

!! [body

     do i=1,n
         dy(i) = dx(n + 1 - i)
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_reverse_d

!!
!! @sub s_reverse_z
!!
!! reverse a complex(dp) array: y(i) = x(n+1-i)
!!
  subroutine s_reverse_z(n, zx, zy)
     use constants, only : dp

     implicit none

!! external arguments
     ! size of array
     integer, intent(in)      :: n

     ! input complex(dp) array
     complex(dp), intent(in)  :: zx(n)

     ! reversed complex(dp) array
     complex(dp), intent(out) :: zy(n)

!! local variables
     ! loop index
     integer :: i

!! [body

     do i=1,n
         zy(i) = zx(n + 1 - i)
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_reverse_z

!!========================================================================
!!>>> clip operations                                                <<<
!!========================================================================

!!
!! @sub s_clip_i
!!
!! clip an integer array to range [vmin, vmax].
!! y(i) = max(min(x(i), vmax), vmin)
!!
  subroutine s_clip_i(n, ix, vmin, vmax, iy)
     implicit none

!! external arguments
     ! size of array
     integer, intent(in)  :: n

     ! input integer array
     integer, intent(in)  :: ix(n)

     ! minimum clip value
     integer, intent(in)  :: vmin

     ! maximum clip value
     integer, intent(in)  :: vmax

     ! clipped integer array
     integer, intent(out) :: iy(n)

!! local variables
     ! loop index
     integer :: i

!! [body

     do i=1,n
         if (ix(i) < vmin) then
             iy(i) = vmin
         elseif (ix(i) > vmax) then
             iy(i) = vmax
         else
             iy(i) = ix(i)
         endif
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_clip_i

!!
!! @sub s_clip_d
!!
!! clip a real(dp) array to range [vmin, vmax].
!! y(i) = max(min(x(i), vmax), vmin)
!!
  subroutine s_clip_d(n, dx, vmin, vmax, dy)
     use constants, only : dp

     implicit none

!! external arguments
     ! size of array
     integer, intent(in)    :: n

     ! input real(dp) array
     real(dp), intent(in)   :: dx(n)

     ! minimum clip value
     real(dp), intent(in)   :: vmin

     ! maximum clip value
     real(dp), intent(in)   :: vmax

     ! clipped real(dp) array
     real(dp), intent(out)  :: dy(n)

!! local variables
     ! loop index
     integer :: i

!! [body

     do i=1,n
         if (dx(i) < vmin) then
             dy(i) = vmin
         elseif (dx(i) > vmax) then
             dy(i) = vmax
         else
             dy(i) = dx(i)
         endif
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_clip_d

!!
!! @sub s_clip_z
!!
!! clip a complex(dp) array to range [vmin, vmax].
!! clip based on magnitude: |y(i)| = max(min(|x(i)|, vmax), vmin)
!! preserve original phase for complex values.
!!
  subroutine s_clip_z(n, zx, vmin, vmax, zy)
     use constants, only : dp

     implicit none

!! external arguments
     ! size of array
     integer, intent(in)      :: n

     ! input complex(dp) array
     complex(dp), intent(in)  :: zx(n)

     ! minimum clip value (magnitude)
     real(dp), intent(in)     :: vmin

     ! maximum clip value (magnitude)
     real(dp), intent(in)     :: vmax

     ! clipped complex(dp) array
     complex(dp), intent(out) :: zy(n)

!! local variables
     ! loop index
     integer :: i

     ! clipped magnitude
     real(dp) :: mag

     ! original phase
     real(dp) :: phase

!! [body

     do i=1,n
         mag = abs(zx(i))
         if (mag < vmin) then
             phase = atan2(aimag(zx(i)), real(zx(i)))
             zy(i) = cmplx(vmin * cos(phase), vmin * sin(phase), dp)
         elseif (mag > vmax) then
             phase = atan2(aimag(zx(i)), real(zx(i)))
             zy(i) = cmplx(vmax * cos(phase), vmax * sin(phase), dp)
         else
             zy(i) = zx(i)
         endif
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_clip_z


!!========================================================================
!!>>> concat operations                                                <<<
!!========================================================================

!!
!! @sub s_concat_i
!!
!! concatenate two integer vectors: z = [x, y]
!!
  subroutine s_concat_i(n, m, ix, iy, iz)
     implicit none

!! external arguments
     ! size of first vector
     integer, intent(in)  :: n

     ! size of second vector
     integer, intent(in)  :: m

     ! first integer vector
     integer, intent(in)  :: ix(n)

     ! second integer vector
     integer, intent(in)  :: iy(m)

     ! concatenated vector: [ix, iy]
     integer, intent(out) :: iz(n+m)

!! local variables
     ! loop index
     integer :: i

!! [body

     do i=1,n
         iz(i) = ix(i)
     enddo ! over i={1,n} loop
     !
     do i=1,m
         iz(n+i) = iy(i)
     enddo ! over i={1,m} loop

!! body]

     return
  end subroutine s_concat_i

!!
!! @sub s_concat_d
!!
!! concatenate two real(dp) vectors: z = [x, y]
!!
  subroutine s_concat_d(n, m, dx, dy, dz)
     use constants, only : dp

     implicit none

!! external arguments
     ! size of first vector
     integer, intent(in)     :: n

     ! size of second vector
     integer, intent(in)     :: m

     ! first real(dp) vector
     real(dp), intent(in)  :: dx(n)

     ! second real(dp) vector
     real(dp), intent(in)  :: dy(m)

     ! concatenated vector: [dx, dy]
     real(dp), intent(out) :: dz(n+m)

!! local variables
     ! loop index
     integer :: i

!! [body

     do i=1,n
         dz(i) = dx(i)
     enddo ! over i={1,n} loop
     !
     do i=1,m
         dz(n+i) = dy(i)
     enddo ! over i={1,m} loop

!! body]

     return
  end subroutine s_concat_d

!!
!! @sub s_concat_z
!!
!! concatenate two complex(dp) vectors: z = [x, y]
!!
  subroutine s_concat_z(n, m, zx, zy, zz)
     use constants, only : dp

     implicit none

!! external arguments
     ! size of first vector
     integer, intent(in)      :: n

     ! size of second vector
     integer, intent(in)      :: m

     ! first complex(dp) vector
     complex(dp), intent(in)  :: zx(n)

     ! second complex(dp) vector
     complex(dp), intent(in)  :: zy(m)

     ! concatenated vector: [zx, zy]
     complex(dp), intent(out) :: zz(n+m)

!! local variables
     ! loop index
     integer :: i

!! [body

     do i=1,n
         zz(i) = zx(i)
     enddo ! over i={1,n} loop
     !
     do i=1,m
         zz(n+i) = zy(i)
     enddo ! over i={1,m} loop

!! body]

     return
  end subroutine s_concat_z

!!========================================================================
!!>>> norm1 operations                                                 <<<
!!========================================================================

!!
!! @sub s_norm1_i
!!
!! compute L1 norm of an integer vector: ||x||_1 = sum |x(i)|
!!
  subroutine s_norm1_i(n, ix, norm)
     use constants, only : dp
     use constants, only : zero

     implicit none

!! external arguments
     ! size of vector
     integer, intent(in)   :: n

     ! input integer vector
     integer, intent(in)   :: ix(n)

     ! L1 norm: sum |x(i)|
     real(dp), intent(out) :: norm

!! local variables
     ! loop index
     integer :: i

     ! sum of absolute values
     real(dp) :: sum_abs

!! [body

     if (n <= 0) then
         norm = zero
         return
     endif
     !
     sum_abs = zero
     do i=1,n
         sum_abs = sum_abs + abs(real(ix(i), dp))
     enddo ! over i={1,n} loop
     norm = sum_abs

!! body]

     return
  end subroutine s_norm1_i

!!
!! @sub s_norm1_d
!!
!! compute L1 norm of a real(dp) vector: ||x||_1 = sum |x(i)|
!!
  subroutine s_norm1_d(n, dx, norm)
     use constants, only : dp
     use constants, only : zero

     implicit none

!! external arguments
     ! size of vector
     integer, intent(in)   :: n

     ! input real(dp) vector
     real(dp), intent(in)  :: dx(n)

     ! L1 norm: sum |x(i)|
     real(dp), intent(out) :: norm

!! local variables
     ! loop index
     integer :: i

     ! sum of absolute values
     real(dp) :: sum_abs

!! [body

     if (n <= 0) then
         norm = zero
         return
     endif
     !
     sum_abs = zero
     do i=1,n
         sum_abs = sum_abs + abs(dx(i))
     enddo ! over i={1,n} loop
     norm = sum_abs

!! body]

     return
  end subroutine s_norm1_d

!!
!! @sub s_norm1_z
!!
!! compute L1 norm of a complex(dp) vector: ||x||_1 = sum |x(i)|
!!
  subroutine s_norm1_z(n, zx, norm)
     use constants, only : dp
     use constants, only : zero

     implicit none

!! external arguments
     ! size of vector
     integer, intent(in)      :: n

     ! input complex(dp) vector
     complex(dp), intent(in)  :: zx(n)

     ! L1 norm: sum |x(i)|
     real(dp), intent(out)    :: norm

!! local variables
     ! loop index
     integer :: i

     ! sum of absolute values
     real(dp) :: sum_abs

!! [body

     if (n <= 0) then
         norm = zero
         return
     endif
     !
     sum_abs = zero
     do i=1,n
         sum_abs = sum_abs + abs(zx(i))
     enddo ! over i={1,n} loop
     norm = sum_abs

!! body]

     return
  end subroutine s_norm1_z

!!========================================================================
!!>>> norm2 operations                                                 <<<
!!========================================================================

!!
!! @sub s_norm2_i
!!
!! compute L2 norm of an integer vector: ||x||_2 = sqrt(sum x(i)^2)
!!
  subroutine s_norm2_i(n, ix, norm)
     use constants, only : dp
     use constants, only : zero

     implicit none

!! external arguments
     ! size of vector
     integer, intent(in)   :: n

     ! input integer vector
     integer, intent(in)   :: ix(n)

     ! L2 norm: sqrt(sum x(i)^2)
     real(dp), intent(out) :: norm

!! local variables
     ! loop index
     integer :: i

     ! sum of squared values
     real(dp) :: sum_sq

!! [body

     if (n <= 0) then
         norm = zero
         return
     endif
     !
     sum_sq = zero
     do i=1,n
         sum_sq = sum_sq + real(ix(i), dp)**2
     enddo ! over i={1,n} loop
     norm = sqrt(sum_sq)

!! body]

     return
  end subroutine s_norm2_i

!!
!! @sub s_norm2_d
!!
!! compute L2 norm of a real(dp) vector: ||x||_2 = sqrt(sum x(i)^2)
!!
  subroutine s_norm2_d(n, dx, norm)
     use constants, only : dp
     use constants, only : zero

     implicit none

!! external arguments
     ! size of vector
     integer, intent(in)   :: n

     ! input real(dp) vector
     real(dp), intent(in)  :: dx(n)

     ! L2 norm: sqrt(sum x(i)^2)
     real(dp), intent(out) :: norm

!! local variables
     ! loop index
     integer :: i

     ! sum of squared values
     real(dp) :: sum_sq

!! [body

     if (n <= 0) then
         norm = zero
         return
     endif
     !
     sum_sq = zero
     do i=1,n
         sum_sq = sum_sq + dx(i)**2
     enddo ! over i={1,n} loop
     norm = sqrt(sum_sq)

!! body]

     return
  end subroutine s_norm2_d

!!
!! @sub s_norm2_z
!!
!! compute L2 norm of a complex(dp) vector: ||x||_2 = sqrt(sum |z(i)|^2)
!!
  subroutine s_norm2_z(n, zx, norm)
     use constants, only : dp
     use constants, only : zero

     implicit none

!! external arguments
     ! size of vector
     integer, intent(in)      :: n

     ! input complex(dp) vector
     complex(dp), intent(in)  :: zx(n)

     ! L2 norm: sqrt(sum |z(i)|^2)
     real(dp), intent(out)    :: norm

!! local variables
     ! loop index
     integer :: i

     ! sum of squared absolute values
     real(dp) :: sum_sq

!! [body

     if (n <= 0) then
         norm = zero
         return
     endif
     !
     sum_sq = zero
     do i=1,n
         sum_sq = sum_sq + abs(zx(i))**2
     enddo ! over i={1,n} loop
     norm = sqrt(sum_sq)

!! body]

     return
  end subroutine s_norm2_z

!!========================================================================
!!>>> norminf operations                                               <<<
!!========================================================================

!!
!! @sub s_norminf_i
!!
!! compute L-infinity norm of an integer vector: ||x||_inf = max |x(i)|
!!
  subroutine s_norminf_i(n, ix, norm)
     use constants, only : dp
     use constants, only : zero

     implicit none

!! external arguments
     ! size of vector
     integer, intent(in)   :: n

     ! input integer vector
     integer, intent(in)   :: ix(n)

     ! L-infinity norm: max |x(i)|
     real(dp), intent(out) :: norm

!! local variables
     ! loop index
     integer :: i

     ! maximum absolute value
     real(dp) :: max_abs

!! [body

     if (n <= 0) then
         norm = zero
         return
     endif
     !
     max_abs = abs(real(ix(1), dp))
     do i=2,n
         max_abs = max(max_abs, abs(real(ix(i), dp)))
     enddo ! over i={2,n} loop
     norm = max_abs

!! body]

     return
  end subroutine s_norminf_i

!!
!! @sub s_norminf_d
!!
!! compute L-infinity norm of a real(dp) vector: ||x||_inf = max |x(i)|
!!
  subroutine s_norminf_d(n, dx, norm)
     use constants, only : dp
     use constants, only : zero

     implicit none

!! external arguments
     ! size of vector
     integer, intent(in)   :: n

     ! input real(dp) vector
     real(dp), intent(in)  :: dx(n)

     ! L-infinity norm: max |x(i)|
     real(dp), intent(out) :: norm

!! local variables
     ! loop index
     integer :: i

     ! maximum absolute value
     real(dp) :: max_abs

!! [body

     if (n <= 0) then
         norm = zero
         return
     endif
     !
     max_abs = abs(dx(1))
     do i=2,n
         max_abs = max(max_abs, abs(dx(i)))
     enddo ! over i={2,n} loop
     norm = max_abs

!! body]

     return
  end subroutine s_norminf_d

!!
!! @sub s_norminf_z
!!
!! compute L-infinity norm of a complex(dp) vector: ||x||_inf = max |x(i)|
!!
  subroutine s_norminf_z(n, zx, norm)
     use constants, only : dp
     use constants, only : zero

     implicit none

!! external arguments
     ! size of vector
     integer, intent(in)      :: n

     ! input complex(dp) vector
     complex(dp), intent(in)  :: zx(n)

     ! L-infinity norm: max |x(i)|
     real(dp), intent(out)    :: norm

!! local variables
     ! loop index
     integer :: i

     ! maximum absolute value
     real(dp) :: max_abs

!! [body

     if (n <= 0) then
         norm = zero
         return
     endif
     !
     max_abs = abs(zx(1))
     do i=2,n
         max_abs = max(max_abs, abs(zx(i)))
     enddo ! over i={2,n} loop
     norm = max_abs

!! body]

     return
  end subroutine s_norminf_z

!!========================================================================
!!>>> shuffle operations                                               <<<
!!========================================================================

!!
!! @sub s_shuffle_i
!!
!! shuffle an integer vector in-place using Fisher-Yates algorithm.
!!
  subroutine s_shuffle_i(n, ix)
     use constants, only : dp

     implicit none

!! external arguments
     ! size of vector
     integer, intent(in)    :: n

     ! integer vector to be shuffled (in-place)
     integer, intent(inout) :: ix(n)

!! local variables
     ! loop index
     integer :: i

     ! random index
     integer :: j

     ! temporary variable for swapping
     integer :: temp

     ! random number
     real(dp) :: r

!! [body

     if (n < 2) then
         return
     endif
     !
     ! Fisher-Yates shuffle algorithm
     do i=n,2,-1
         call random_number(r)
         j = int(r * real(i-1, dp)) + 1
         temp = ix(i)
         ix(i) = ix(j)
         ix(j) = temp
     enddo ! over i={n,2,-1} loop

!! body]

     return
  end subroutine s_shuffle_i

!!
!! @sub s_shuffle_d
!!
!! shuffle a real(dp) vector in-place using Fisher-Yates algorithm.
!!
  subroutine s_shuffle_d(n, dx)
     use constants, only : dp

     implicit none

!! external arguments
     ! size of vector
     integer, intent(in)     :: n

     ! real(dp) vector to be shuffled (in-place)
     real(dp), intent(inout) :: dx(n)

!! local variables
     ! loop index
     integer :: i

     ! random index
     integer :: j

     ! temporary variable for swapping
     real(dp) :: temp

     ! random number
     real(dp) :: r

!! [body

     if (n < 2) then
         return
     endif
     !
     ! Fisher-Yates shuffle algorithm
     do i=n,2,-1
         call random_number(r)
         j = int(r * real(i-1, dp)) + 1
         temp = dx(i)
         dx(i) = dx(j)
         dx(j) = temp
     enddo ! over i={n,2,-1} loop

!! body]

     return
  end subroutine s_shuffle_d

!!
!! @sub s_shuffle_z
!!
!! shuffle a complex(dp) vector in-place using Fisher-Yates algorithm.
!!
  subroutine s_shuffle_z(n, zx)
     use constants, only : dp

     implicit none

!! external arguments
     ! size of vector
     integer, intent(in)        :: n

     ! complex(dp) vector to be shuffled (in-place)
     complex(dp), intent(inout) :: zx(n)

!! local variables
     ! loop index
     integer :: i

     ! random index
     integer :: j

     ! temporary variable for swapping
     complex(dp) :: temp

     ! random number
     real(dp) :: r

!! [body

     if (n < 2) then
         return
     endif
     !
     ! Fisher-Yates shuffle algorithm
     do i=n,2,-1
         call random_number(r)
         j = int(r * real(i-1, dp)) + 1
         temp = zx(i)
         zx(i) = zx(j)
         zx(j) = temp
     enddo ! over i={n,2,-1} loop

!! body]

     return
  end subroutine s_shuffle_z

!!========================================================================
!!>>> pow operations                                                   <<<
!!========================================================================

!!
!! @sub s_pow_i
!!
!! compute power of an integer vector in-place: x = x^power
!!
  subroutine s_pow_i(n, ix, power)
     implicit none

!! external arguments
     ! size of vector
     integer, intent(in)    :: n

     ! integer vector to be modified (in-place)
     integer, intent(inout) :: ix(n)

     ! power exponent
     integer, intent(in)    :: power

!! local variables
     ! loop index
     integer :: i

!! [body

     if (n <= 0) then
         return
     endif
     !
     do i=1,n
         ix(i) = ix(i)**power
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_pow_i

!!
!! @sub s_pow_d
!!
!! compute power of a real(dp) vector in-place: x = x^power
!!
  subroutine s_pow_d(n, dx, power)
     use constants, only : dp

     implicit none

!! external arguments
     ! size of vector
     integer, intent(in)     :: n

     ! real(dp) vector to be modified (in-place)
     real(dp), intent(inout) :: dx(n)

     ! power exponent
     real(dp), intent(in)    :: power

!! local variables
     ! loop index
     integer :: i

!! [body

     if (n <= 0) then
         return
     endif
     !
     do i=1,n
         dx(i) = dx(i)**power
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_pow_d

!!
!! @sub s_pow_z
!!
!! compute power of a complex(dp) vector in-place: z = z^power
!!
  subroutine s_pow_z(n, zx, power)
     use constants, only : dp

     implicit none

!! external arguments
     ! size of vector
     integer, intent(in)        :: n

     ! complex(dp) vector to be modified (in-place)
     complex(dp), intent(inout) :: zx(n)

     ! power exponent
     real(dp), intent(in)       :: power

!! local variables
     ! loop index
     integer :: i

!! [body

     if (n <= 0) then
         return
     endif
     !
     do i=1,n
         zx(i) = zx(i)**power
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_pow_z

!!========================================================================
!!>>> square operations                                                <<<
!!========================================================================

!!
!! @sub s_square_i
!!
!! compute square of an integer vector in-place: x = x^2
!!
  subroutine s_square_i(n, ix)
     implicit none

!! external arguments
     ! size of vector
     integer, intent(in)    :: n

     ! integer vector to be modified (in-place)
     integer, intent(inout) :: ix(n)

!! local variables
     ! loop index
     integer :: i

!! [body

     if (n <= 0) then
         return
     endif
     !
     do i=1,n
         ix(i) = ix(i)**2
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_square_i

!!
!! @sub s_square_d
!!
!! compute square of a real(dp) vector in-place: x = x^2
!!
  subroutine s_square_d(n, dx)
     use constants, only : dp

     implicit none

!! external arguments
     ! size of vector
     integer, intent(in)     :: n

     ! real(dp) vector to be modified (in-place)
     real(dp), intent(inout) :: dx(n)

!! local variables
     ! loop index
     integer :: i

!! [body

     if (n <= 0) then
         return
     endif
     !
     do i=1,n
         dx(i) = dx(i)**2
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_square_d

!!
!! @sub s_square_z
!!
!! compute square of a complex(dp) vector in-place: z = z^2
!!
  subroutine s_square_z(n, zx)
     use constants, only : dp

     implicit none

!! external arguments
     ! size of vector
     integer, intent(in)      :: n

     ! complex(dp) vector to be modified (in-place)
     complex(dp), intent(inout) :: zx(n)

!! local variables
     ! loop index
     integer :: i

!! [body

     if (n <= 0) then
         return
     endif
     !
     do i=1,n
         zx(i) = zx(i)**2
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_square_z

!!========================================================================
!!>>> sqrt operations                                                  <<<
!!========================================================================

!!
!! @sub s_sqrt_i
!!
!! compute square root of an integer vector in-place: x = sqrt(x)
!!
  subroutine s_sqrt_i(n, ix)
     use constants, only : dp

     implicit none

!! external arguments
     ! size of vector
     integer, intent(in)    :: n

     ! integer vector to be modified (in-place)
     ! note: result is cast back to integer
     integer, intent(inout) :: ix(n)

!! local variables
     ! loop index
     integer :: i

!! [body

     if (n <= 0) then
         return
     endif
     !
     do i=1,n
         if (ix(i) >= 0) then
             ix(i) = int(sqrt(real(ix(i), dp)))
         else
             ix(i) = 0
         endif
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_sqrt_i

!!
!! @sub s_sqrt_d
!!
!! compute square root of a real(dp) vector in-place: x = sqrt(x)
!!
  subroutine s_sqrt_d(n, dx)
     use constants, only : dp
     use constants, only : zero

     implicit none

!! external arguments
     ! size of vector
     integer, intent(in)     :: n

     ! real(dp) vector to be modified (in-place)
     ! note: negative values are set to 0
     real(dp), intent(inout) :: dx(n)

!! local variables
     ! loop index
     integer :: i

!! [body

     if (n <= 0) then
         return
     endif
     !
     do i=1,n
         if (dx(i) >= 0.0_dp) then
             dx(i) = sqrt(dx(i))
         else
             dx(i) = zero
         endif
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_sqrt_d

!!
!! @sub s_sqrt_z
!!
!! compute square root of a complex(dp) vector in-place: z = sqrt(z)
!!
  subroutine s_sqrt_z(n, zx)
     use constants, only : dp

     implicit none

!! external arguments
     ! size of vector
     integer, intent(in)        :: n

     ! complex(dp) vector to be modified (in-place)
     complex(dp), intent(inout) :: zx(n)

!! local variables
     ! loop index
     integer :: i

!! [body

     if (n <= 0) then
         return
     endif
     !
     do i=1,n
         zx(i) = sqrt(zx(i))
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_sqrt_z

!!========================================================================
!!>>> exp operations                                                   <<<
!!========================================================================

!!
!! @sub s_exp_i
!!
!! compute exponential of an integer vector in-place: x = exp(x)
!!
  subroutine s_exp_i(n, ix)
     use constants, only : dp

     implicit none

!! external arguments
     ! size of vector
     integer, intent(in)    :: n

     ! integer vector to be modified (in-place)
     ! note: result is cast back to integer
     integer, intent(inout) :: ix(n)

!! local variables
     ! loop index
     integer :: i

!! [body

     if (n <= 0) then
         return
     endif
     !
     do i=1,n
         ix(i) = int(exp(real(ix(i), dp)))
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_exp_i

!!
!! @sub s_exp_d
!!
!! compute exponential of a real(dp) vector in-place: x = exp(x)
!!
  subroutine s_exp_d(n, dx)
     use constants, only : dp

     implicit none

!! external arguments
     ! size of vector
     integer, intent(in)     :: n

     ! real(dp) vector to be modified (in-place)
     real(dp), intent(inout) :: dx(n)

!! local variables
     ! loop index
     integer :: i

!! [body

     if (n <= 0) then
         return
     endif
     !
     do i=1,n
         dx(i) = exp(dx(i))
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_exp_d

!!
!! @sub s_exp_z
!!
!! compute exponential of a complex(dp) vector in-place: z = exp(z)
!!
  subroutine s_exp_z(n, zx)
     use constants, only : dp

     implicit none

!! external arguments
     ! size of vector
     integer, intent(in)        :: n

     ! complex(dp) vector to be modified (in-place)
     complex(dp), intent(inout) :: zx(n)

!! local variables
     ! loop index
     integer :: i

!! [body

     if (n <= 0) then
         return
     endif
     !
     do i=1,n
         zx(i) = exp(zx(i))
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_exp_z
