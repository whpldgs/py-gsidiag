#!/usr/bin/env python

import numpy as _np
import _readconvobs
import _readsatobs


class GSIdiag(object):

    def __init__(self, fname, diag_type, endian='native'):
        '''
        Initialize a GSI diagnostic object, by reading it
        INPUT:
            fname  : name of the diagnostic file
            dtype  : type of the diagnostic file e.g. conv / rad
            endian : endian-ness of the file
        RESULT:
            self   : GSIdiag object containing the contents of the file
        '''

        self.filename = fname
        self.endian = endian

        # set diagnostic type
        if diag_type.upper() in ['CONV', 'CONVENTIONAL']:
            self.diag_type = 'CONVENTIONAL'
        elif diag_type.upper() in ['RAD', 'RADIANCE']:
            self.diag_type = 'RADIANCE'
        else:
            raise Exception('Unrecognizable diagnostic file type %s' % diag_type)

        if self.diag_type in ['RADIANCE']:
            npred, nobs = _readsatobs.get_num_satobs(fname, endian=endian)
            self.npred, self.nobs = npred, nobs
            self._read_radobs()
        elif self.diag_type in ['CONVENTIONAL']:
            nobs = _readconvobs.get_num_convobs(fname, endian=endian)
            self.nobs = nobs
            self._read_convobs()

        return


    def print_info(self):
        '''
        Print the contents of the GSI diagnostic object
        '''

        print '%s is a %s diagnostic file and contains ...' % (self.filename, self.diag_type)
        print
        keys = self.__dict__.keys()
        print ', '.join(str(x) for x in keys)
        print

        return


    def _read_convobs(self):
        '''
        Read the data from the conventional diagnostic file during initialization
        '''

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
        '''
        Read the data from the radiance diagnostic file during initialization
        '''

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


    def _get_data(self, qty, indx):
        '''
        Helper function to get data.
        '''

        ndim, val = None, None
        exec('ndim = len(self.%s.shape)' % qty)

        if ndim == 1:
            exec('val = self.%s[indx]' % qty)
        elif ndim > 1:
            exec('val = self.%s[:,indx]' % qty)

        return val


    def get_data_conv(self, qty, obtype, used=None, code=None):
        '''
        Given parameters, get the data from a conventional diagnostic file
        INPUT:
            qty    : diagnostic quantity to extract e.g. hx, station_ids, etc
            obtype : observation type e.g. 'ps', 'u', 'v', 't' etc
            used   : qc flag (default: None) e.g. 0, 1
            code   : KX (default: None)
        OUTPUT:
            val    : requested data
        '''

        assert self.diag_type == 'CONVENTIONAL', ('This is not a CONVENTIONAL diagnostic file')

        indx = self._get_indx_conv(obtype, code=code, used=used)
        val = self._get_data(qty, indx)

        return val


    def _get_indx_conv(self, obtype, used=None, code=None):
        '''
        Given parameters, get the indicies of observation locations from a conventional diagnostic file
        INPUT:
            obtype : observation type e.g. 'ps', 'u', 'v', 't' etc
            used   : qc flag (default: None) e.g. 0, 1
            code   : KX (default: None)
        OUTPUT:
            indx   : indices of the requested data in the file
        '''

        indx = self.obtype == obtype.rjust(3)
        if used != None: indx = _np.logical_and(indx, self.used==used)
        if code != None: indx = _np.logical_and(indx, self.code==code)

        return indx


    def get_data_rad(self, qty, ichan, used=None, oberr=None, water=False, land=False, ice=False, snow=False, snowice=False):
        '''
        Given parameters, get the data from a radiance diagnostic file
        INPUT:
            qty    : diagnostic quantity to extract e.g. hx, biaspred, etc
            ichan  : channel number
            used   : qc flag (default: None) e.g. 0, 1
            oberr  : filter through observation error (default: None) e.g. 1.e9
            water  : filter observations over water (default: False)
            land   : filter observations over land (default: False)
            ice    : filter observations over ice (default: False)
            snow   : filter observations over snow (default: False)
            snowice: filter observations over snowice (default: False)
        OUTPUT:
            val    : requested data
        '''

        assert self.diag_type == 'RADIANCE', ('This is not a RADIANCE diagnostic file')

        indx = self._get_indx_rad(ichan, used=used, oberr=oberr, water=water, land=land, ice=ice, snow=snow, snowice=snowice)
        val = self._get_data(qty, indx)

        return val


    def _get_indx_rad(self, ichan, used=None, oberr=None, water=False, land=False, ice=False, snow=False, snowice=False):
        '''
        Given parameters, get the indicies of observation locations from a radiance diagnostic file
        INPUT:
            qty    : diagnostic quantity to extract e.g. hx, station_ids, etc
            ichan  : channel number
            used   : qc flag (default: None) e.g. 0, 1
            oberr  : filter through observation error (default: None) e.g. 1.e9
            water  : filter observations over water (default: False)
            land   : filter observations over land (default: False)
            ice    : filter observations over ice (default: False)
            snow   : filter observations over snow (default: False)
            snowice: filter observations over snowice (default: False)
        OUTPUT:
            indx   : indices of the requested data in the file
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
