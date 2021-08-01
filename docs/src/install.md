As mentioned before, almost all of my fortran codes depend on the *Flink* library. So, if you want to try my codes, perhaps you have to compile and install *Flink* at first. Do not be nervous. It is quite easy.

At first, you have to download the latest source codes from the following repository:

> https://github.com/huangli712/flink

If every thing is OK, you will have a compressed file, such as `flink.zip` or `flink.tar.gz`. And then you need to decompress it with `unzip` or `tar`.

The third step is to compile *Flink*. Please execute the following three commands in your terminal.

```sh
$ cd flink/build
$ editor make.sys
$ make
```

Once the compilation is finished, we can find `libflink.a` and a lot of `*.mod` files under the `flink/src` directory. That is all.

In order to compile *Flink*, you have to ensure that a MPI-enabled fortran compiler (which is compatibe with fortran 2003 standard), and a numerical library which implements the BLAS and LAPACK interfaces (such as Intel MKL), are installed and configured correctly in your system. You can setup your compiling environment via editing the `build/make.sys` file.

See [make.sys](guide/make.md) for more details.
