*Flink* is a fortran utility library. It provides some useful features with which you can develop your scientific computation codes quickly. Actually, almost all of my fortran codes depend on the *Flink* library. They are

* [iQIST](https://github.com/huangli712/iqist)
* [Dyson](https://github.com/huangli712/dyson)
* And so on...

## Features

* Modules
    * Physical and numerical constants (**src/m\_constants.f90**)
    * Linked list (**src/m\_linkedlist.f90**)
    * Message passing interface (**src/m\_mpi.f90**)
    * Sparse matrix (**src/m\_sparse.f90**)
    * Configuration file (**src/m\_parser.f90**)
    * Pseudo random number generator (**src/m\_spring.f90**)
    * Stack (**src/m\_stack.f90**)
    * Analytical tetrahedron method (**src/m\_tetra.f90**)
    * Version information (**src/m\_version.f90**)

* Subroutines
    * Warning and error messages (**src/s\_error.f90**)
    * Fourier transformation (**src/s\_fourier.f90**)
    * Special functions (**src/s\_function.f90**)
    * Numerical integral (**src/s\_integrator.f90**)
    * Matrix computation (**src/s\_matrix.f90**)
    * Spline interpolation (**src/s\_spline.f90**)
    * Some auxiliary subroutines, such as sorting algorithm (**src/s\_util.f90**)
    * Vector computation (**src/s\_vector.90**)

## Author

```text
Li Huang
Institute of Materials, China Academy of Engineering Physics, Sichuan Jiangyou, PRC
email: lihuang.dmft at gmail.com
```
