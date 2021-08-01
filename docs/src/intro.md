## Welcome

*Flink* is a Fortran utility library. It provides some useful features with which you can develop your scientific computation codes quickly. Actually, almost all of my Fortran codes depend on the *Flink* library. They are

* [iQIST](https://github.com/huangli712/iqist)
* [Dyson](https://github.com/huangli712/dyson)
* And so on...

!!! note

    *Flink* obeys the Fortran 2003 standard. 

## Features

* Modules
    * Physical and numerical constants (**src/m_constants.f90**)
    * Linked list (**src/m_linkedlist.f90**)
    * Message passing interface (**src/m_mpi.f90**)
    * Sparse matrix (**src/m_sparse.f90**)
    * Configuration file (**src/m_parser.f90**)
    * Pseudo random number generator (**src/m_spring.f90**)
    * Stack (**src/m_stack.f90**)
    * Analytical tetrahedron method (**src/m_tetra.f90**)
    * Version information (**src/m_version.f90**)

* Subroutines
    * Warning and error messages (**src/s_error.f90**)
    * Fourier transformation (**src/s_fourier.f90**)
    * Special functions (**src/s_function.f90**)
    * Numerical integral (**src/s_integrator.f90**)
    * Matrix computation (**src/s_matrix.f90**)
    * Spline interpolation (**src/s_spline.f90**)
    * Some auxiliary subroutines, such as sorting algorithm (**src/s_util.f90**)
    * Vector computation (**src/s_vector.90**)

## Author

```text
Li Huang
Institute of Materials, China Academy of Engineering Physics, Sichuan Jiangyou, PRC
email: lihuang.dmft at gmail.com
```
