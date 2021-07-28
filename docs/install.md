As mentioned before, almost all of my fortran codes depend on the *Flink* library. So, if you want to try my codes, perhaps you have to compile and install *Flink* at first. Do not be nervous. It is quite easy.

At first, you have to download the latest source codes from the following repository:

> https://github.com/huangli712/flink

If every thing is OK, you will have a compressed file, such as `flink.zip` or `flink.tar.gz`. And then you need to decompress it with `unzip` or `tar`.

The third step is to compile `Flink`. Please execute the following three commands in your terminal.

```sh
$ cd flink/build
$ editor make.sys
$ make
```

Once the compilation is finished, we can see `libflink.a` and a lot of `*.mod` files under the `flink/src` directory.