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
subroutine s_linspace_i(xmin, xmax, n, x)
subroutine s_linspace_d(xmin, xmax, n, x)
subroutine s_linspace_z(xmin, xmax, n, x)
```

**Purpose:**

Create a linearly spaced vector with `n` points in the interval [`xmin`, `xmax`].

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `xmin` | `integer`/`real(dp)`/`complex(dp)` | `in` | Starting value of the interval |
| `xmax` | `integer`/`real(dp)`/`complex(dp)` | `in` | Ending value of the interval |
| `n` | `integer` | `in` | Number of points |
| `x` | `integer`/`real(dp)`/`complex(dp)` | `out` | Output linearly spaced vector |

---

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
| `n` | `integer` | `in` | Size of vector `v` |
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
| `n` | `integer` | `in` | Size of vector `v` |
| `v` | `integer`/`real(dp)`/`complex(dp)` | `in` | Input vector |
| `vprod` | `integer`/`real(dp)`/`complex(dp)` | `out` | Cumulative product vector |

---

### Mixing and Add Operations

```fortran
subroutine s_mix_i(n, x, y, alpha)
subroutine s_mix_d(n, x, y, alpha)
subroutine s_mix_z(n, x, zy, alpha)
```

**Purpose:**

Perform linear mixing of two vectors: `x = (1 - alpha) * x + alpha * y`.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of vectors |
| `x` | `integer`/`real(dp)`/`complex(dp)` | `inout` | First vector; on exit, contains the mixed result |
| `y`/`zy` | `integer`/`real(dp)`/`complex(dp)` | `in` | Second vector |
| `alpha` | `real(dp)` | `in` | Mixing parameter (0 ≤ α ≤ 1) |

---

```fortran
subroutine s_vecadd_i(n, x, y, alpha)
subroutine s_vecadd_d(n, x, y, alpha)
subroutine s_vecadd_z(n, x, y, alpha)
```

**Purpose:**

Add diagonal elements of a matrix to a vector: `x = x + alpha * diag(y)`.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Dimension of vector `x` and matrix `y` (n-by-n) |
| `x` | `integer`/`real(dp)`/`complex(dp)` | `inout` | Vector; on exit, contains `x + alpha * diag(y)` |
| `y` | `integer`/`real(dp)`/`complex(dp)` | `in` | n-by-n matrix |
| `alpha` | `real(dp)` | `in` | Prefactor for the diagonal elements |

---

### Vector Products

```fortran
subroutine s_dot_i(n, x, y, d)
subroutine s_dot_d(n, x, y, d)
subroutine s_dot_z(n, x, y, d)
```

**Purpose:**

Calculate the dot product (inner product) of two vectors: `d = x · y`.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of vectors |
| `x` | `integer`/`real(dp)`/`complex(dp)` | `in` | First vector |
| `y` | `integer`/`real(dp)`/`complex(dp)` | `in` | Second vector |
| `d` | `integer`/`real(dp)`/`complex(dp)` | `out` | Dot product result |

---

```fortran
subroutine s_outer_i(m, n, x, y, A)
subroutine s_outer_d(m, n, x, y, A)
subroutine s_outer_z(m, n, x, y, A)
```

**Purpose:**

Calculate the outer product of two vectors: `A = x ⊗ y` (A[i,j] = x[i] * y[j]).

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `m` | `integer` | `in` | Size of vector `x` |
| `n` | `integer` | `in` | Size of vector `y` |
| `x` | `integer`/`real(dp)`/`complex(dp)` | `in` | First vector (m elements) |
| `y` | `integer`/`real(dp)`/`complex(dp)` | `in` | Second vector (n elements) |
| `A` | `integer`/`real(dp)`/`complex(dp)` | `out` | Outer product matrix (m-by-n) |

---

```fortran
subroutine s_cross_i(x, y, z)
subroutine s_cross_d(x, y, z)
subroutine s_cross_z(x, y, z)
```

**Purpose:**

Calculate the cross product of two 3D vectors: `z = x × y`.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `x` | `integer`/`real(dp)`/`complex(dp)` | `in` | First 3D vector |
| `y` | `integer`/`real(dp)`/`complex(dp)` | `in` | Second 3D vector |
| `z` | `integer`/`real(dp)`/`complex(dp)` | `out` | Cross product result (3D vector) |

---

### Set Operations

```fortran
subroutine s_diff_i(n, v, d)
subroutine s_diff_d(n, v, d)
subroutine s_diff_z(n, v, d)
```

**Purpose:**

Calculate the differences between consecutive elements: `d[i] = v[i+1] - v[i]`.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of input vector `v` |
| `v` | `integer`/`real(dp)`/`complex(dp)` | `in` | Input vector |
| `d` | `integer`/`real(dp)`/`complex(dp)` | `out` | Difference vector (size n-1) |

---

```fortran
subroutine s_unique_i(n, v, unique_v, n_unique)
subroutine s_unique_d(n, v, unique_v, n_unique)
subroutine s_unique_z(n, v, unique_v, n_unique)
```

**Purpose:**

Extract unique elements from a vector, sorted in ascending order.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of input vector `v` |
| `v` | `integer`/`real(dp)`/`complex(dp)` | `in` | Input vector |
| `unique_v` | `integer`/`real(dp)`/`complex(dp)` | `out` | Unique elements (sorted) |
| `n_unique` | `integer` | `out` | Number of unique elements |

---

```fortran
subroutine s_intersect_i(n, a, m, b, c, k)
subroutine s_intersect_d(n, a, m, b, c, k)
subroutine s_intersect_z(n, a, m, b, c, k)
```

**Purpose:**

Find the intersection of two vectors (elements common to both).

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of vector `a` |
| `a` | `integer`/`real(dp)`/`complex(dp)` | `in` | First input vector |
| `m` | `integer` | `in` | Size of vector `b` |
| `b` | `integer`/`real(dp)`/`complex(dp)` | `in` | Second input vector |
| `c` | `integer`/`real(dp)`/`complex(dp)` | `out` | Intersection elements (sorted) |
| `k` | `integer` | `out` | Number of elements in intersection |

---

```fortran
subroutine s_union_i(n, a, m, b, c, k)
subroutine s_union_d(n, a, m, b, c, k)
subroutine s_union_z(n, a, m, b, c, k)
```

**Purpose:**

Find the union of two vectors (all unique elements from both).

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of vector `a` |
| `a` | `integer`/`real(dp)`/`complex(dp)` | `in` | First input vector |
| `m` | `integer` | `in` | Size of vector `b` |
| `b` | `integer`/`real(dp)`/`complex(dp)` | `in` | Second input vector |
| `c` | `integer`/`real(dp)`/`complex(dp)` | `out` | Union elements (sorted) |
| `k` | `integer` | `out` | Number of elements in union |

---

### Statistics

```fortran
subroutine s_stats_i(n, v, mean, stddev)
subroutine s_stats_d(n, v, mean, stddev)
subroutine s_stats_z(n, v, mean, stddev)
```

**Purpose:**

Calculate the mean and standard deviation of a vector.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of vector `v` |
| `v` | `integer`/`real(dp)`/`complex(dp)` | `in` | Input vector |
| `mean` | `real(dp)`/`complex(dp)` | `out` | Mean value |
| `stddev` | `real(dp)` | `out` | Standard deviation |

---

```fortran
subroutine s_moment_i(n, v, k, moment)
subroutine s_moment_d(n, v, k, moment)
subroutine s_moment_z(n, v, k, moment)
```

**Purpose:**

Calculate the k-th central moment of a vector: `E[(X - μ)^k]`.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of vector `v` |
| `v` | `integer`/`real(dp)`/`complex(dp)` | `in` | Input vector |
| `k` | `integer` | `in` | Order of the moment |
| `moment` | `real(dp)` | `out` | k-th central moment |

---

```fortran
subroutine s_skewness_i(n, v, skew)
subroutine s_skewness_d(n, v, skew)
subroutine s_skewness_z(n, v, skew)
```

**Purpose:**

Calculate the skewness (asymmetry measure) of a distribution. Positive skew indicates right tail, negative indicates left tail.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of vector `v` |
| `v` | `integer`/`real(dp)`/`complex(dp)` | `in` | Input vector |
| `skew` | `real(dp)` | `out` | Skewness value |

---

```fortran
subroutine s_kurtosis_i(n, v, kurt)
subroutine s_kurtosis_d(n, v, kurt)
subroutine s_kurtosis_z(n, v, kurt)
```

**Purpose:**

Calculate the kurtosis (tailedness measure) of a distribution. Normal distribution has kurtosis = 3; higher values indicate heavier tails.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of vector `v` |
| `v` | `integer`/`real(dp)`/`complex(dp)` | `in` | Input vector |
| `kurt` | `real(dp)` | `out` | Kurtosis value |

---

### Distance and Norms

```fortran
subroutine s_distance_i(n, x, y, dist)
subroutine s_distance_d(n, x, y, dist)
subroutine s_distance_z(n, x, y, dist)
```

**Purpose:**

Calculate the Euclidean distance between two vectors: `dist = ||x - y||_2`.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of vectors |
| `x` | `integer`/`real(dp)`/`complex(dp)` | `in` | First vector |
| `y` | `integer`/`real(dp)`/`complex(dp)` | `in` | Second vector |
| `dist` | `real(dp)` | `out` | Euclidean distance |

---

```fortran
subroutine s_norm1_i(n, v, norm)
subroutine s_norm1_d(n, v, norm)
subroutine s_norm1_z(n, v, norm)
```

**Purpose:**

Calculate the L1 norm (Manhattan norm) of a vector: `||v||_1 = Σ|v[i]|`.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of vector `v` |
| `v` | `integer`/`real(dp)`/`complex(dp)` | `in` | Input vector |
| `norm` | `real(dp)` | `out` | L1 norm value |

---

```fortran
subroutine s_norm2_i(n, v, norm)
subroutine s_norm2_d(n, v, norm)
subroutine s_norm2_z(n, v, norm)
```

**Purpose:**

Calculate the L2 norm (Euclidean norm) of a vector: `||v||_2 = √(Σ|v[i]|²)`.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of vector `v` |
| `v` | `integer`/`real(dp)`/`complex(dp)` | `in` | Input vector |
| `norm` | `real(dp)` | `out` | L2 norm value |

---

```fortran
subroutine s_norminf_i(n, v, norm)
subroutine s_norminf_d(n, v, norm)
subroutine s_norminf_z(n, v, norm)
```

**Purpose:**

Calculate the L∞ norm (maximum norm) of a vector: `||v||_∞ = max(|v[i]|)`.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of vector `v` |
| `v` | `integer`/`real(dp)`/`complex(dp)` | `in` | Input vector |
| `norm` | `real(dp)` | `out` | L∞ norm value |

---

### Mathematical Operations

```fortran
subroutine s_pow_i(n, v, p, w)
subroutine s_pow_d(n, v, p, w)
subroutine s_pow_z(n, v, p, w)
```

**Purpose:**

Raise each element of a vector to a power: `w[i] = v[i]^p`.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of vectors |
| `v` | `integer`/`real(dp)`/`complex(dp)` | `in` | Input vector |
| `p` | `real(dp)` | `in` | Exponent |
| `w` | `integer`/`real(dp)`/`complex(dp)` | `out` | Output vector with powered elements |

---

```fortran
subroutine s_square_i(n, v, w)
subroutine s_square_d(n, v, w)
subroutine s_square_z(n, v, w)
```

**Purpose:**

Square each element of a vector: `w[i] = v[i]²`.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of vectors |
| `v` | `integer`/`real(dp)`/`complex(dp)` | `in` | Input vector |
| `w` | `integer`/`real(dp)`/`complex(dp)` | `out` | Output vector with squared elements |

---

```fortran
subroutine s_sqrt_d(n, v, w)
subroutine s_sqrt_z(n, v, w)
```

**Purpose:**

Calculate the square root of each element: `w[i] = √v[i]`.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of vectors |
| `v` | `real(dp)`/`complex(dp)` | `in` | Input vector |
| `w` | `real(dp)`/`complex(dp)` | `out` | Output vector with square roots |

---

```fortran
subroutine s_exp_d(n, v, w)
subroutine s_exp_z(n, v, w)
```

**Purpose:**

Calculate the exponential of each element: `w[i] = exp(v[i])`.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of vectors |
| `v` | `real(dp)`/`complex(dp)` | `in` | Input vector |
| `w` | `real(dp)`/`complex(dp)` | `out` | Output vector with exponentials |

---

```fortran
subroutine s_log_d(n, v, w)
subroutine s_log_z(n, v, w)
```

**Purpose:**

Calculate the natural logarithm of each element: `w[i] = ln(v[i])`.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of vectors |
| `v` | `real(dp)`/`complex(dp)` | `in` | Input vector |
| `w` | `real(dp)`/`complex(dp)` | `out` | Output vector with natural logarithms |

---

```fortran
subroutine s_log10_d(n, v, w)
subroutine s_log10_z(n, v, w)
```

**Purpose:**

Calculate the base-10 logarithm of each element: `w[i] = log₁₀(v[i])`.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of vectors |
| `v` | `real(dp)`/`complex(dp)` | `in` | Input vector |
| `w` | `real(dp)`/`complex(dp)` | `out` | Output vector with base-10 logarithms |

---

### Trigonometric Functions

```fortran
subroutine s_sin_d(n, v, w)
subroutine s_sin_z(n, v, w)
```

**Purpose:**

Calculate the sine of each element: `w[i] = sin(v[i])`.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of vectors |
| `v` | `real(dp)`/`complex(dp)` | `in` | Input vector (radians) |
| `w` | `real(dp)`/`complex(dp)` | `out` | Output vector with sine values |

---

```fortran
subroutine s_cos_d(n, v, w)
subroutine s_cos_z(n, v, w)
```

**Purpose:**

Calculate the cosine of each element: `w[i] = cos(v[i])`.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of vectors |
| `v` | `real(dp)`/`complex(dp)` | `in` | Input vector (radians) |
| `w` | `real(dp)`/`complex(dp)` | `out` | Output vector with cosine values |

---

```fortran
subroutine s_tan_d(n, v, w)
subroutine s_tan_z(n, v, w)
```

**Purpose:**

Calculate the tangent of each element: `w[i] = tan(v[i])`.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of vectors |
| `v` | `real(dp)`/`complex(dp)` | `in` | Input vector (radians) |
| `w` | `real(dp)`/`complex(dp)` | `out` | Output vector with tangent values |

---

```fortran
subroutine s_sinh_d(n, v, w)
subroutine s_sinh_z(n, v, w)
```

**Purpose:**

Calculate the hyperbolic sine of each element: `w[i] = sinh(v[i])`.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of vectors |
| `v` | `real(dp)`/`complex(dp)` | `in` | Input vector |
| `w` | `real(dp)`/`complex(dp)` | `out` | Output vector with hyperbolic sine values |

---

```fortran
subroutine s_cosh_d(n, v, w)
subroutine s_cosh_z(n, v, w)
```

**Purpose:**

Calculate the hyperbolic cosine of each element: `w[i] = cosh(v[i])`.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of vectors |
| `v` | `real(dp)`/`complex(dp)` | `in` | Input vector |
| `w` | `real(dp)`/`complex(dp)` | `out` | Output vector with hyperbolic cosine values |

---

```fortran
subroutine s_tanh_d(n, v, w)
subroutine s_tanh_z(n, v, w)
```

**Purpose:**

Calculate the hyperbolic tangent of each element: `w[i] = tanh(v[i])`.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of vectors |
| `v` | `real(dp)`/`complex(dp)` | `in` | Input vector |
| `w` | `real(dp)`/`complex(dp)` | `out` | Output vector with hyperbolic tangent values |

---

### Smoothing Operations

```fortran
subroutine s_moving_average_i(n, v, w, window)
subroutine s_moving_average_d(n, v, w, window)
subroutine s_moving_average_z(n, v, w, window)
```

**Purpose:**

Apply a simple moving average filter with uniform weights.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of input vector `v` |
| `v` | `integer`/`real(dp)`/`complex(dp)` | `in` | Input vector |
| `w` | `integer`/`real(dp)`/`complex(dp)` | `out` | Smoothed output vector |
| `window` | `integer` | `in` | Window size for averaging (must be odd) |

---

```fortran
subroutine s_smooth_box_i(n, v, w, window)
subroutine s_smooth_box_d(n, v, w, window)
subroutine s_smooth_box_z(n, v, w, window)
```

**Purpose:**

Apply a box (rectangular) smoothing filter. Similar to moving average but with boundary handling.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of input vector `v` |
| `v` | `integer`/`real(dp)`/`complex(dp)` | `in` | Input vector |
| `w` | `integer`/`real(dp)`/`complex(dp)` | `out` | Smoothed output vector |
| `window` | `integer` | `in` | Half-width of the box filter |

---

```fortran
subroutine s_smooth_gaussian_i(n, v, w, sigma)
subroutine s_smooth_gaussian_d(n, v, w, sigma)
subroutine s_smooth_gaussian_z(n, v, w, sigma)
```

**Purpose:**

Apply a Gaussian smoothing filter with specified standard deviation.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of input vector `v` |
| `v` | `integer`/`real(dp)`/`complex(dp)` | `in` | Input vector |
| `w` | `integer`/`real(dp)`/`complex(dp)` | `out` | Smoothed output vector |
| `sigma` | `real(dp)` | `in` | Standard deviation of the Gaussian kernel |

---

### Manipulation Operations

```fortran
subroutine s_swap_i(n, x, y)
subroutine s_swap_d(n, x, y)
subroutine s_swap_z(n, x, y)
```

**Purpose:**

Exchange the contents of two vectors.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of vectors |
| `x` | `integer`/`real(dp)`/`complex(dp)` | `inout` | First vector |
| `y` | `integer`/`real(dp)`/`complex(dp)` | `inout` | Second vector |

---

```fortran
subroutine s_clip_i(n, v, vmin, vmax, w)
subroutine s_clip_d(n, v, vmin, vmax, w)
subroutine s_clip_z(n, v, vmin, vmax, w)
```

**Purpose:**

Clip (limit) the values in a vector to a specified range.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of vectors |
| `v` | `integer`/`real(dp)`/`complex(dp)` | `in` | Input vector |
| `vmin` | `integer`/`real(dp)`/`complex(dp)` | `in` | Minimum value |
| `vmax` | `integer`/`real(dp)`/`complex(dp)` | `in` | Maximum value |
| `w` | `integer`/`real(dp)`/`complex(dp)` | `out` | Clipped output vector |

---

```fortran
subroutine s_slice_i(n, v, start, stride, end, w, m)
subroutine s_slice_d(n, v, start, stride, end, w, m)
subroutine s_slice_z(n, v, start, stride, end, w, m)
```

**Purpose:**

Extract a slice from a vector with specified start, stride, and end indices.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of input vector `v` |
| `v` | `integer`/`real(dp)`/`complex(dp)` | `in` | Input vector |
| `start` | `integer` | `in` | Starting index (1-based) |
| `stride` | `integer` | `in` | Step size between elements |
| `end` | `integer` | `in` | Ending index |
| `w` | `integer`/`real(dp)`/`complex(dp)` | `out` | Sliced output vector |
| `m` | `integer` | `out` | Size of output vector |

---

```fortran
subroutine s_take_i(n, v, indices, m, w)
subroutine s_take_d(n, v, indices, m, w)
subroutine s_take_z(n, v, indices, m, w)
```

**Purpose:**

Extract elements from a vector at specified indices.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of input vector `v` |
| `v` | `integer`/`real(dp)`/`complex(dp)` | `in` | Input vector |
| `indices` | `integer` | `in` | Array of indices to extract (1-based) |
| `m` | `integer` | `in` | Number of indices |
| `w` | `integer`/`real(dp)`/`complex(dp)` | `out` | Output vector with selected elements |

---

```fortran
subroutine s_drop_i(n, v, indices, m, w, k)
subroutine s_drop_d(n, v, indices, m, w, k)
subroutine s_drop_z(n, v, indices, m, w, k)
```

**Purpose:**

Remove elements from a vector at specified indices.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of input vector `v` |
| `v` | `integer`/`real(dp)`/`complex(dp)` | `in` | Input vector |
| `indices` | `integer` | `in` | Array of indices to drop (1-based) |
| `m` | `integer` | `in` | Number of indices to drop |
| `w` | `integer`/`real(dp)`/`complex(dp)` | `out` | Output vector with elements removed |
| `k` | `integer` | `out` | Size of output vector |

---

### Final Operations

```fortran
subroutine s_shuffle_i(n, v)
subroutine s_shuffle_d(n, v)
subroutine s_shuffle_z(n, v)
```

**Purpose:**

Randomly shuffle the elements of a vector in place using Fisher-Yates algorithm.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of vector `v` |
| `v` | `integer`/`real(dp)`/`complex(dp)` | `inout` | Vector to shuffle (modified in place) |

---

```fortran
subroutine s_reverse_i(n, v, w)
subroutine s_reverse_d(n, v, w)
subroutine s_reverse_z(n, v, w)
```

**Purpose:**

Reverse the order of elements in a vector.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of vectors |
| `v` | `integer`/`real(dp)`/`complex(dp)` | `in` | Input vector |
| `w` | `integer`/`real(dp)`/`complex(dp)` | `out` | Reversed output vector |

---

```fortran
subroutine s_concat_i(n, a, m, b, c)
subroutine s_concat_d(n, a, m, b, c)
subroutine s_concat_z(n, a, m, b, c)
```

**Purpose:**

Concatenate two vectors: `c = [a, b]`.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of vector `a` |
| `a` | `integer`/`real(dp)`/`complex(dp)` | `in` | First input vector |
| `m` | `integer` | `in` | Size of vector `b` |
| `b` | `integer`/`real(dp)`/`complex(dp)` | `in` | Second input vector |
| `c` | `integer`/`real(dp)`/`complex(dp)` | `out` | Concatenated output vector (size n+m) |
