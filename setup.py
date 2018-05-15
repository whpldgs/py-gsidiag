from numpy.distutils.core  import setup, Extension

# modules for reading GSI diagnostic files.
ext_diag_conv = Extension(name     = '_readconvobs',
                          sources  = ['src/_readconvobs.pyf', 'src/readconvobs.f90'])
ext_diag_sat = Extension(name     = '_readsatobs',
                         sources  = ['src/_readsatobs.pyf', 'src/readsatobs.f90', 'src/read_diag.f90'])

if __name__ == "__main__":
    setup(name = 'py-gsidiag',
          version           = "0.9.3",
          description       = "Python interface to GSI binary diagnostic files",
          author            = "Rahul Mahajan",
          author_email      = "rahul.mahajan@noaa.gov",
          url               = "http://github.com/aerorahul/py-gsidiag",
          ext_modules       = [ext_diag_conv, ext_diag_sat],
          packages          = ['gsidiag'],
          )
