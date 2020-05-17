# mantler

A coded implementation for reconstructing heavy resonances in the antler decay topology using the singularity variable.

## Dependencies

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
