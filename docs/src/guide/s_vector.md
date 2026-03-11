It provides a lot of subroutines to manipulate vectors and implement vector operations.

## Type

subroutines

## Source

`src/s_vector.f90`

## Naming Convention

!!! note

    - `_i` means integer version
    - `_d` means real(dp) version
    - `_z` means complex(dp) version

## Usage

### Mesh Generation

```fortran
subroutine s_linspace_d(xmin, xmax, n, x)
subroutine s_linspace_z(xmin, xmax, n, x)
```

**Purpose:**

Create a linearly spaced vector with `n` points in the interval [`xmin`, `xmax`].

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `xmin` | `real(dp)`/`complex(dp)` | `in` | Starting value of the interval |
| `xmax` | `real(dp)`/`complex(dp)` | `in` | Ending value of the interval |
| `n` | `integer` | `in` | Number of points |
| `x` | `real(dp)`/`complex(dp)` | `out` | Output linearly spaced vector |

### Cumulative Operations

```fortran
subroutine s_cumsum_i(n, v, vsum)
subroutine s_cumsum_d(n, v, vsum)
subroutine s_cumsum_z(n, v, vsum)
```

**Purpose:**

Calculate the cumulative sum of a vector.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of vector |
| `v` | `integer`/`real(dp)`/`complex(dp)` | `in` | Input vector |
| `vsum` | `integer`/`real(dp)`/`complex(dp)` | `out` | Cumulative sum vector |

---

```fortran
subroutine s_cumprod_i(n, v, vprod)
subroutine s_cumprod_d(n, v, vprod)
subroutine s_cumprod_z(n, v, vprod)
```

**Purpose:**

Calculate the cumulative product of a vector.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of vector |
| `v` | `integer`/`real(dp)`/`complex(dp)` | `in` | Input vector |
| `vprod` | `integer`/`real(dp)`/`complex(dp)` | `out` | Cumulative product vector |

### Mixing and Add Operations

```fortran
subroutine s_mix_d(n, dx, dy, alpha)
subroutine s_mix_z(n, zx, zy, alpha)
```

**Purpose:**

Perform linear mixing of two vectors: `y = (1 - alpha) * x + alpha * y`.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of vectors |
| `dx`/`zx` | `real(dp)`/`complex(dp)` | `in` | First vector |
| `dy`/`zy` | `real(dp)`/`complex(dp)` | `inout` | Second vector; on exit, contains the mixed result |
| `alpha` | `real(dp)` | `in` | Mixing parameter (0 ≤ α ≤ 1) |

---

```fortran
subroutine s_vecadd_i(n, ix, iy, alpha)
subroutine s_vecadd_d(n, dx, dy, alpha)
subroutine s_vecadd_z(n, zx, zy, alpha)
```

**Purpose:**

Add diagonal elements of a matrix to a vector: `x = x + alpha * diag(y)`.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Dimension of vector and matrix (n-by-n) |
| `ix`/`dx`/`zx` | `integer`/`real(dp)`/`complex(dp)` | `inout` | Vector; on exit, contains `x + alpha * diag(y)` |
| `iy`/`dy`/`zy` | `integer`/`real(dp)`/`complex(dp)` | `in` | n-by-n matrix |
| `alpha` | `real(dp)` | `in` | Prefactor for the diagonal elements |

### Vector Products

```fortran
subroutine s_dot_i(n, ix, iy, val)
subroutine s_dot_d(n, dx, dy, val)
subroutine s_dot_z(n, zx, zy, val)
```

**Purpose:**

Calculate the dot product (inner product) of two vectors: `val = x · y`.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of vectors |
| `ix`/`dx`/`zx` | `integer`/`real(dp)`/`complex(dp)` | `in` | First vector |
| `iy`/`dy`/`zy` | `integer`/`real(dp)`/`complex(dp)` | `in` | Second vector |
| `val` | `integer`/`real(dp)`/`complex(dp)` | `out` | Dot product result |

---

```fortran
subroutine s_outer_i(n, m, ix, iy, ia)
subroutine s_outer_d(n, m, dx, dy, da)
subroutine s_outer_z(n, m, zx, zy, za)
```

**Purpose:**

Calculate the outer product of two vectors: `A = x ⊗ y` (A[i,j] = x[i] * y[j]).

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of vector `x` |
| `m` | `integer` | `in` | Size of vector `y` |
| `ix`/`dx`/`zx` | `integer`/`real(dp)`/`complex(dp)` | `in` | First vector (n elements) |
| `iy`/`dy`/`zy` | `integer`/`real(dp)`/`complex(dp)` | `in` | Second vector (m elements) |
| `ia`/`da`/`za` | `integer`/`real(dp)`/`complex(dp)` | `out` | Outer product matrix (n-by-m) |

---

```fortran
subroutine s_cross_i(ix, iy, iz)
subroutine s_cross_d(dx, dy, dz)
subroutine s_cross_z(zx, zy, zz)
```

**Purpose:**

Calculate the cross product of two 3D vectors: `z = x × y`.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `ix`/`dx`/`zx` | `integer`/`real(dp)`/`complex(dp)` | `in` | First 3D vector |
| `iy`/`dy`/`zy` | `integer`/`real(dp)`/`complex(dp)` | `in` | Second 3D vector |
| `iz`/`dz`/`zz` | `integer`/`real(dp)`/`complex(dp)` | `out` | Cross product result (3D vector) |

### Set Operations

```fortran
subroutine s_diff_i(n, iv, diff)
subroutine s_diff_d(n, dv, diff)
subroutine s_diff_z(n, zv, diff)
```

**Purpose:**

Calculate the differences between consecutive elements: `diff[i] = v[i+1] - v[i]`.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of input vector |
| `iv`/`dv`/`zv` | `integer`/`real(dp)`/`complex(dp)` | `in` | Input vector |
| `diff` | `integer`/`real(dp)`/`complex(dp)` | `out` | Difference vector (size n-1) |

---

```fortran
subroutine s_unique_i(n, ix, m, iy)
subroutine s_unique_d(n, dx, m, dy)
subroutine s_unique_z(n, zx, m, zy)
```

**Purpose:**

Extract unique elements from a vector, preserving original order.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of input vector |
| `ix`/`dx`/`zx` | `integer`/`real(dp)`/`complex(dp)` | `in` | Input vector |
| `m` | `integer` | `out` | Number of unique elements |
| `iy`/`dy`/`zy` | `integer`/`real(dp)`/`complex(dp)` | `out` | Unique elements (preserving order, size n but only first m valid) |

---

```fortran
subroutine s_intersect_i(n, ix, m, iy, k, iz)
subroutine s_intersect_d(n, dx, m, dy, k, dz)
subroutine s_intersect_z(n, zx, m, zy, k, zz)
```

**Purpose:**

Find the intersection of two vectors (elements common to both).

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of first vector |
| `ix`/`dx`/`zx` | `integer`/`real(dp)`/`complex(dp)` | `in` | First input vector |
| `m` | `integer` | `in` | Size of second vector |
| `iy`/`dy`/`zy` | `integer`/`real(dp)`/`complex(dp)` | `in` | Second input vector |
| `k` | `integer` | `out` | Number of elements in intersection |
| `iz`/`dz`/`zz` | `integer`/`real(dp)`/`complex(dp)` | `out` | Intersection elements (unique) |

---

```fortran
subroutine s_union_i(n, ix, m, iy, k, iz)
subroutine s_union_d(n, dx, m, dy, k, dz)
subroutine s_union_z(n, zx, m, zy, k, zz)
```

**Purpose:**

Find the union of two vectors (all unique elements from both).

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of first vector |
| `ix`/`dx`/`zx` | `integer`/`real(dp)`/`complex(dp)` | `in` | First input vector |
| `m` | `integer` | `in` | Size of second vector |
| `iy`/`dy`/`zy` | `integer`/`real(dp)`/`complex(dp)` | `in` | Second input vector |
| `k` | `integer` | `out` | Number of elements in union |
| `iz`/`dz`/`zz` | `integer`/`real(dp)`/`complex(dp)` | `out` | Union elements (unique, size n+m) |

### Statistics

```fortran
subroutine s_stats_i(n, iv, mean, stddev)
subroutine s_stats_d(n, dv, mean, stddev)
subroutine s_stats_z(n, zv, mean, stddev)
```

**Purpose:**

Calculate the mean and standard deviation of a vector.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of vector |
| `iv`/`dv`/`zv` | `integer`/`real(dp)`/`complex(dp)` | `in` | Input vector |
| `mean` | `real(dp)`/`complex(dp)` | `out` | Mean value |
| `stddev` | `real(dp)` | `out` | Standard deviation |

---

```fortran
subroutine s_moment_i(n, iv, order, moment)
subroutine s_moment_d(n, dv, order, moment)
subroutine s_moment_z(n, zv, order, moment)
```

**Purpose:**

Calculate the k-th central moment of a vector: `E[(X - μ)^k]`.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of vector |
| `iv`/`dv`/`zv` | `integer`/`real(dp)`/`complex(dp)` | `in` | Input vector |
| `order` | `integer` | `in` | Order of the moment |
| `moment` | `real(dp)` | `out` | k-th central moment |

---

```fortran
subroutine s_skewness_i(n, iv, skewness)
subroutine s_skewness_d(n, dv, skewness)
subroutine s_skewness_z(n, zv, skewness)
```

**Purpose:**

Calculate the skewness (asymmetry measure) of a distribution. Positive skew indicates right tail, negative indicates left tail.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of vector |
| `iv`/`dv`/`zv` | `integer`/`real(dp)`/`complex(dp)` | `in` | Input vector |
| `skewness` | `real(dp)` | `out` | Skewness value |

---

```fortran
subroutine s_kurtosis_i(n, iv, kurtosis)
subroutine s_kurtosis_d(n, dv, kurtosis)
subroutine s_kurtosis_z(n, zv, kurtosis)
```

**Purpose:**

Calculate the kurtosis (tailedness measure) of a distribution. Normal distribution has kurtosis = 3; higher values indicate heavier tails.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of vector |
| `iv`/`dv`/`zv` | `integer`/`real(dp)`/`complex(dp)` | `in` | Input vector |
| `kurtosis` | `real(dp)` | `out` | Kurtosis value |

### Distance and Norms

```fortran
subroutine s_distance_i(n, ix, iy, dist)
subroutine s_distance_d(n, dx, dy, dist)
subroutine s_distance_z(n, zx, zy, dist)
```

**Purpose:**

Calculate the Euclidean distance between two vectors: `dist = ||x - y||_2`.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of vectors |
| `ix`/`dx`/`zx` | `integer`/`real(dp)`/`complex(dp)` | `in` | First vector |
| `iy`/`dy`/`zy` | `integer`/`real(dp)`/`complex(dp)` | `in` | Second vector |
| `dist` | `real(dp)` | `out` | Euclidean distance |

---

```fortran
subroutine s_norm1_i(n, ix, norm)
subroutine s_norm1_d(n, dx, norm)
subroutine s_norm1_z(n, zx, norm)
```

**Purpose:**

Calculate the L1 norm (Manhattan norm) of a vector: `||v||_1 = Σ|v[i]|`.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of vector |
| `ix`/`dx`/`zx` | `integer`/`real(dp)`/`complex(dp)` | `in` | Input vector |
| `norm` | `real(dp)` | `out` | L1 norm value |

---

```fortran
subroutine s_norm2_i(n, ix, norm)
subroutine s_norm2_d(n, dx, norm)
subroutine s_norm2_z(n, zx, norm)
```

**Purpose:**

Calculate the L2 norm (Euclidean norm) of a vector: `||v||_2 = √(Σ|v[i]|²)`.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of vector |
| `ix`/`dx`/`zx` | `integer`/`real(dp)`/`complex(dp)` | `in` | Input vector |
| `norm` | `real(dp)` | `out` | L2 norm value |

---

```fortran
subroutine s_norminf_i(n, ix, norm)
subroutine s_norminf_d(n, dx, norm)
subroutine s_norminf_z(n, zx, norm)
```

**Purpose:**

Calculate the L∞ norm (maximum norm) of a vector: `||v||_∞ = max(|v[i]|)`.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of vector |
| `ix`/`dx`/`zx` | `integer`/`real(dp)`/`complex(dp)` | `in` | Input vector |
| `norm` | `real(dp)` | `out` | L∞ norm value |

### Mathematical Operations

```fortran
subroutine s_pow_i(n, ix, power)
subroutine s_pow_d(n, dx, power)
subroutine s_pow_z(n, zx, power)
```

**Purpose:**

Raise each element of a vector to a power (in-place): `x[i] = x[i]^power`.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of vector |
| `ix`/`dx`/`zx` | `integer`/`real(dp)`/`complex(dp)` | `inout` | Vector; on exit, contains powered elements |
| `power` | `real(dp)` | `in` | Exponent |

---

```fortran
subroutine s_square_i(n, ix)
subroutine s_square_d(n, dx)
subroutine s_square_z(n, zx)
```

**Purpose:**

Square each element of a vector (in-place): `x[i] = x[i]²`.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of vector |
| `ix`/`dx`/`zx` | `integer`/`real(dp)`/`complex(dp)` | `inout` | Vector; on exit, contains squared elements |

---

```fortran
subroutine s_sqrt_d(n, dx)
subroutine s_sqrt_z(n, zx)
```

**Purpose:**

Calculate the square root of each element (in-place): `x[i] = √x[i]`.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of vector |
| `dx`/`zx` | `real(dp)`/`complex(dp)` | `inout` | Vector; on exit, contains square roots |

---

```fortran
subroutine s_exp_d(n, dx)
subroutine s_exp_z(n, zx)
```

**Purpose:**

Calculate the exponential of each element (in-place): `x[i] = exp(x[i])`.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of vector |
| `dx`/`zx` | `real(dp)`/`complex(dp)` | `inout` | Vector; on exit, contains exponentials |

---

```fortran
subroutine s_log_d(n, dx)
subroutine s_log_z(n, zx)
```

**Purpose:**

Calculate the natural logarithm of each element (in-place): `x[i] = ln(x[i])`.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of vector |
| `dx`/`zx` | `real(dp)`/`complex(dp)` | `inout` | Vector; on exit, contains natural logarithms |

---

```fortran
subroutine s_log10_d(n, dx)
subroutine s_log10_z(n, zx)
```

**Purpose:**

Calculate the base-10 logarithm of each element (in-place): `x[i] = log₁₀(x[i])`.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of vector |
| `dx`/`zx` | `real(dp)`/`complex(dp)` | `inout` | Vector; on exit, contains base-10 logarithms |

### Trigonometric Functions

```fortran
subroutine s_sin_d(n, dx)
subroutine s_sin_z(n, zx)
```

**Purpose:**

Calculate the sine of each element (in-place): `x[i] = sin(x[i])`.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of vector |
| `dx`/`zx` | `real(dp)`/`complex(dp)` | `inout` | Vector (radians); on exit, contains sine values |

---

```fortran
subroutine s_cos_d(n, dx)
subroutine s_cos_z(n, zx)
```

**Purpose:**

Calculate the cosine of each element (in-place): `x[i] = cos(x[i])`.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of vector |
| `dx`/`zx` | `real(dp)`/`complex(dp)` | `inout` | Vector (radians); on exit, contains cosine values |

---

```fortran
subroutine s_tan_d(n, dx)
subroutine s_tan_z(n, zx)
```

**Purpose:**

Calculate the tangent of each element (in-place): `x[i] = tan(x[i])`.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of vector |
| `dx`/`zx` | `real(dp)`/`complex(dp)` | `inout` | Vector (radians); on exit, contains tangent values |

---

```fortran
subroutine s_sinh_d(n, dx)
subroutine s_sinh_z(n, zx)
```

**Purpose:**

Calculate the hyperbolic sine of each element (in-place): `x[i] = sinh(x[i])`.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of vector |
| `dx`/`zx` | `real(dp)`/`complex(dp)` | `inout` | Vector; on exit, contains hyperbolic sine values |

---

```fortran
subroutine s_cosh_d(n, dx)
subroutine s_cosh_z(n, zx)
```

**Purpose:**

Calculate the hyperbolic cosine of each element (in-place): `x[i] = cosh(x[i])`.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of vector |
| `dx`/`zx` | `real(dp)`/`complex(dp)` | `inout` | Vector; on exit, contains hyperbolic cosine values |

---

```fortran
subroutine s_tanh_d(n, dx)
subroutine s_tanh_z(n, zx)
```

**Purpose:**

Calculate the hyperbolic tangent of each element (in-place): `x[i] = tanh(x[i])`.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of vector |
| `dx`/`zx` | `real(dp)`/`complex(dp)` | `inout` | Vector; on exit, contains hyperbolic tangent values |

### Smoothing Operations

```fortran
subroutine s_moving_average_i(n, ix, window_size, iy)
subroutine s_moving_average_d(n, dx, window_size, dy)
subroutine s_moving_average_z(n, zx, window_size, zy)
```

**Purpose:**

Apply a simple moving average filter with uniform weights.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of input vector |
| `ix`/`dx`/`zx` | `integer`/`real(dp)`/`complex(dp)` | `in` | Input vector |
| `window_size` | `integer` | `in` | Window size for averaging (must be odd) |
| `iy`/`dy`/`zy` | `integer`/`real(dp)`/`complex(dp)` | `out` | Smoothed output vector |

---

```fortran
subroutine s_smooth_box_i(n, ix, window_size, iy)
subroutine s_smooth_box_d(n, dx, window_size, dy)
subroutine s_smooth_box_z(n, zx, window_size, zy)
```

**Purpose:**

Apply a box (rectangular) smoothing filter. Similar to moving average but with boundary handling.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of input vector |
| `ix`/`dx`/`zx` | `integer`/`real(dp)`/`complex(dp)` | `in` | Input vector |
| `window_size` | `integer` | `in` | Half-width of the box filter |
| `iy`/`dy`/`zy` | `integer`/`real(dp)`/`complex(dp)` | `out` | Smoothed output vector |

---

```fortran
subroutine s_smooth_gaussian_i(n, ix, sigma, iy)
subroutine s_smooth_gaussian_d(n, dx, sigma, dy)
subroutine s_smooth_gaussian_z(n, zx, sigma, zy)
```

**Purpose:**

Apply a Gaussian smoothing filter with specified standard deviation.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of input vector |
| `ix`/`dx`/`zx` | `integer`/`real(dp)`/`complex(dp)` | `in` | Input vector |
| `sigma` | `real(dp)` | `in` | Standard deviation of the Gaussian kernel |
| `iy`/`dy`/`zy` | `integer`/`real(dp)`/`complex(dp)` | `out` | Smoothed output vector |

### Manipulation Operations

```fortran
subroutine s_swap_i(n, ix, iy)
subroutine s_swap_d(n, dx, dy)
subroutine s_swap_z(n, zx, zy)
```

**Purpose:**

Exchange the contents of two vectors.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of vectors |
| `ix`/`dx`/`zx` | `integer`/`real(dp)`/`complex(dp)` | `inout` | First vector |
| `iy`/`dy`/`zy` | `integer`/`real(dp)`/`complex(dp)` | `inout` | Second vector |

---

```fortran
subroutine s_clip_i(n, ix, vmin, vmax, iy)
subroutine s_clip_d(n, dx, vmin, vmax, dy)
subroutine s_clip_z(n, zx, vmin, vmax, zy)
```

**Purpose:**

Clip (limit) the values in a vector to a specified range.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of vectors |
| `ix`/`dx`/`zx` | `integer`/`real(dp)`/`complex(dp)` | `in` | Input vector |
| `vmin` | `integer`/`real(dp)`/`complex(dp)` | `in` | Minimum value |
| `vmax` | `integer`/`real(dp)`/`complex(dp)` | `in` | Maximum value |
| `iy`/`dy`/`zy` | `integer`/`real(dp)`/`complex(dp)` | `out` | Clipped output vector |

---

```fortran
subroutine s_slice_i(n, ix, start_idx, count, iy)
subroutine s_slice_d(n, dx, start_idx, count, dy)
subroutine s_slice_z(n, zx, start_idx, count, zy)
```

**Purpose:**

Extract a contiguous slice from a vector starting at a given index.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of input vector |
| `ix`/`dx`/`zx` | `integer`/`real(dp)`/`complex(dp)` | `in` | Input vector |
| `start_idx` | `integer` | `in` | Starting index (1-based) |
| `count` | `integer` | `in` | Number of elements to extract |
| `iy`/`dy`/`zy` | `integer`/`real(dp)`/`complex(dp)` | `out` | Sliced output vector |

---

```fortran
subroutine s_take_i(n, ix, m, idx, iy)
subroutine s_take_d(n, dx, m, idx, dy)
subroutine s_take_z(n, zx, m, idx, zy)
```

**Purpose:**

Extract elements from a vector at specified indices.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of input vector |
| `ix`/`dx`/`zx` | `integer`/`real(dp)`/`complex(dp)` | `in` | Input vector |
| `m` | `integer` | `in` | Number of indices |
| `idx` | `integer` | `in` | Array of indices to extract (1-based) |
| `iy`/`dy`/`zy` | `integer`/`real(dp)`/`complex(dp)` | `out` | Output vector with selected elements |

---

```fortran
subroutine s_drop_i(n, ix, m, idx, iy)
subroutine s_drop_d(n, dx, m, idx, dy)
subroutine s_drop_z(n, zx, m, idx, zy)
```

**Purpose:**

Remove elements from a vector at specified indices.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of input vector |
| `ix`/`dx`/`zx` | `integer`/`real(dp)`/`complex(dp)` | `in` | Input vector |
| `m` | `integer` | `in` | Number of indices to drop |
| `idx` | `integer` | `in` | Array of indices to drop (1-based) |
| `iy`/`dy`/`zy` | `integer`/`real(dp)`/`complex(dp)` | `out` | Output vector with elements removed |

---

```fortran
subroutine s_shuffle_i(n, ix, seed)
subroutine s_shuffle_d(n, dx, seed)
subroutine s_shuffle_z(n, zx, seed)
```

**Purpose:**

Randomly shuffle the elements of a vector in place using Fisher-Yates algorithm.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of vector |
| `ix`/`dx`/`zx` | `integer`/`real(dp)`/`complex(dp)` | `inout` | Vector to shuffle (modified in place) |
| `seed` | `integer` | `in` | Random seed for reproducibility |

---

```fortran
subroutine s_reverse_i(n, ix, iy)
subroutine s_reverse_d(n, dx, dy)
subroutine s_reverse_z(n, zx, zy)
```

**Purpose:**

Reverse the order of elements in a vector.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of vectors |
| `ix`/`dx`/`zx` | `integer`/`real(dp)`/`complex(dp)` | `in` | Input vector |
| `iy`/`dy`/`zy` | `integer`/`real(dp)`/`complex(dp)` | `out` | Reversed output vector |

---

```fortran
subroutine s_concat_i(n, m, ix, iy, iz)
subroutine s_concat_d(n, m, dx, dy, dz)
subroutine s_concat_z(n, m, zx, zy, zz)
```

**Purpose:**

Concatenate two vectors: `z = [x, y]`.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of vector `a` |
| `a` | `integer`/`real(dp)`/`complex(dp)` | `in` | First input vector |
| `m` | `integer` | `in` | Size of vector `b` |
| `b` | `integer`/`real(dp)`/`complex(dp)` | `in` | Second input vector |
| `c` | `integer`/`real(dp)`/`complex(dp)` | `out` | Concatenated output vector (size n+m) |
