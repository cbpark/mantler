mantler
=======

[![Build Status](https://travis-ci.com/cbpark/mantler.svg?branch=master)](https://travis-ci.com/cbpark/mantler)

A coded implementation for reconstructing heavy resonances in the antler decay topology using the singularity variable. It has been used in [arXiv:2005.12297](https://arxiv.org/abs/2005.12297).

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

In Debian and Ubuntu,

```
$ sudo apt-get install libgsl-dev liblapack-dev
```

We recommend to use [`stack`](https://www.haskellstack.org) for building.

```
$ stack build
```

It may take some time to complete building in the first run, depending on the internet bandwidth and the machine specifications. If cloning [`hep-utilities`](https://github.com/cbpark/hep-utilities) fails, modify [`stack.yaml`](./stack.yaml):

```
sed -i 's/git@github.com:/https:\/\/github.com\//' stack.yaml
```

## How to use

See the [HEP.Kinematics.Antler](./src/HEP/Kinematics/Antler.hs) module and the example code, [mat.hs](./examples/mat.hs).

For a real-world application, see [mat-analysis](https://github.com/cbpark/mat-analysis).

## Reference

If you use this library, please cite the paper given below.

``` bibtex
@article{Park:2020rol,
    author = "Park, Chan Beom",
    title = "{A singular way to search for heavy resonances in missing energy events}",
    eprint = "2005.12297",
    archivePrefix = "arXiv",
    primaryClass = "hep-ph",
    reportNumber = "CTPU-PTC-20-14",
    month = "5",
    year = "2020"
}
```
