import numpy as _np
import _readconvobs
import _readsatobs
__version__ = '0.0.1'


class GSIdiag(object):

    def __init__(self, filename, endian='native'):

        self.filename = filename
        self.endian = endian

        # Infer diag_type from quering the file itself
        try:
            npred, nobs = _readsatobs.get_num_satobs(filename, endian=endian)
            self.npred, self.nobs = npred, nobs
            self.diag_type = 'RADIANCE'
        except:
            try:
                nobs = _readconvobs.get_num_convobs(filename, endian=endian)
                self.nobs = nobs
                self.diag_type = 'CONVENTIONAL'
            except:
                raise Exception('Unrecognizable type of diagnostic file %s' % self.filename)

        # Read the file as part of initialization
        if self.diag_type in ['CONV', 'CONVENTIONAL']:
            self._read_convobs()
        elif self.diag_type in ['RAD', 'RADIANCE']:
            self._read_radobs()

        return


    def print_info(self):

        print '%s contains...' % self.filename
        print
        keys = self.__dict__.keys()
        print ', '.join(str(x) for x in keys)
        print

        return


    def _read_convobs(self):

        h_x, x_obs, x_err, x_lon, x_lat, x_press, x_time, \
            x_code, x_errorig, x_type, x_use, x_station_id, x_stnelev =\
            _readconvobs.get_convobs_data(self.filename,  self.nobs,
                                          endian=self.endian)

        self.hx = h_x
        self.obs = x_obs
        self.oberr = x_err
        self.lon = x_lon
        self.lat = x_lat
        self.press = x_press
        self.time = x_time
        self.stnelev = x_stnelev
        self.code = x_code
        self.oberr_orig = x_errorig
        self.used = x_use
        self.obtype = \
            _np.array((x_type.tostring()).replace('\x00', '')[:-1].split('|'))
        self.station_ids =\
            _np.array((x_station_id.tostring()).replace(
                '\x00', '')[:-1].split('|'))

        return


    def _read_radobs(self):

        h_x, h_xnobc, x_obs, x_err, x_lon, x_lat, x_time, \
            x_channum, x_errorig, x_biaspred, x_use, x_qcmark,  \
            x_water_frac, x_land_frac, x_ice_frac, x_snow_frac = \
            _readsatobs.get_satobs_data(
                self.filename, self.nobs, self.npred, endian=self.endian)

        self.hx = h_x
        self.biascorr = h_x - h_xnobc
        self.obs = x_obs
        self.oberr = x_err
        self.lon = x_lon
        self.lat = x_lat
        self.time = x_time
        self.channel = x_channum
        self.biaspred = x_biaspred
        self.oberr_orig = x_errorig
        self.used = x_use
        self.qcmark = x_qcmark
        self.water_frac = x_water_frac
        self.land_frac = x_land_frac
        self.ice_frac = x_ice_frac
        self.snow_frac = x_snow_frac

        return


    def get_data(self, qty, *args, **kwargs):
        '''
        Helper function to get data.
        '''

        ndim, val = None, None
        exec('ndim = len(self.%s.shape)' % qty)

        # First get the index of the desired data
        if self.diag_type in ['CONV', 'CONVENTIONAL']:
            indx = self._get_convdiag_indices(*args, **kwargs)
        elif self.diag_type in ['RAD', 'RADIANCE']:
            indx = self._get_raddiag_indices(*args, **kwargs)

        if ndim == 1:
            exec('val = self.%s[indx]' % qty)
        elif ndim > 1:
            exec('val = self.%s[:,indx]' % qty)

        return val


    def _get_convdiag_indices(self,obtype,code=None,used=None):
        '''
        Given parameters, get the indicies of observation locations from a conventional diagnostic file
        INPUT:
            obtype : observation type e.g. 'ps', 'u', 'v', 't' etc
            code   : KX (default: None)
            used   : qc flag (default: None) e.g. 0, 1
        OUTPUT:
            index  : indices of the requested data in the file
        '''

        indx = self.obtype == obtype.rjust(3)
        if code != None: indx = _np.logical_and(indx, self.code==code)
        if used != None: indx = _np.logical_and(indx, self.used==used)

        return indx


    def _get_raddiag_indices(self,ichan,used=None,oberr=None,water=False,land=False,ice=False,snow=False,snowice=False):
        '''
        Given parameters, get the indicies of observation locations from a radiance diagnostic file
        INPUT:
            ichan  : channel number
            used   : qc flag (default: None) e.g. 0, 1
            oberr  : filter through observation error (default: None) e.g. 1.e9
            water  : filter observations over water (default: False)
            land   : filter observations over land (default: False)
            ice    : filter observations over ice (default: False)
            snow   : filter observations over snow (default: False)
            snowice: filter observations over snowice (default: False)
        OUTPUT:
            index  : indices of the requested data in the file
        '''

        indx = self.channel == ichan
        if used  != None: indx = _np.logical_and(indx,self.used==used)
        if oberr != None: indx = _np.logical_and(indx,self.oberr<oberr)
        if water:         indx = _np.logical_and(indx,self.water_frac<0.99)
        if land:          indx = _np.logical_and(indx,self.land_frac<0.01)
        if ice:           indx = _np.logical_and(indx,self.ice_frac<0.99)
        if snow:          indx = _np.logical_and(indx,self.snow_frac<0.99)
        if snowice:       indx = _np.logical_and(indx,self.snow_frac+self.ice_frac<0.99)

        return indx
