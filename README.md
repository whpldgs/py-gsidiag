# py-gsidiag
python interface to GSI binary diagnostic files.

to install ([numpy](http://numpy.org) and fortran compiler (preferably
[gfortran](https://gcc.gnu.org/wiki/GFortran)) required):

* `python setup.py build`
   - If you change the fortran compiler, you may have to add the
flags `config_fc --fcompiler=<compiler name>` when `setup.py` is run
(see docs for [numpy.distutils](http://docs.scipy.org/doc/numpy-dev/f2py/distutils.html)).
* `python setup.py install`

*Probably will not work on Windows!*
