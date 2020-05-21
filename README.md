# mantler

A coded implementation for reconstructing heavy resonances in the antler decay topology using the singularity variable.

For C++ users, see [`MAT`](https://github.com/cbpark/MAT/).

## How to build

Internally, [GNU Scientific Library](https://www.gnu.org/software/gsl/) (GSL) is used to solve polynomial equations. In order to build this from source, make sure that `gsl-config` is in the `PATH`.

```
$ gsl-config --version
2.6
```

In Arch Linux, it can be installed by

```
$ sudo pacman -S gsl lapack
```

We recommend to use [`stack`](https://www.haskellstack.org) for building.

```
$ stack build
```

It may take some time to complete it in the first build, depending on the internet bandwidth and the machine specifications.

## How to use

See the [HEP.Kinematics.Antler](./src/HEP/Kinematics/Antler.hs) module and the example code, [mat.hs](./examples/mat.hs).

For a real-world application, see [mat-analysis](https://github.com/cbpark/mat-analysis).
