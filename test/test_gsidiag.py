#!/usr/bin/env python

import gsidiag
import numpy as np

# Conventional diagnostic file

convobsfile = 'diag_conv_ges.2015102700'
diag_conv = gsidiag.GSIdiag(convobsfile, 'CONVENTIONAL', endian='big')
nobsall = diag_conv.nobs
print 'total number of obs = %d ' % nobsall

# print OmF stats for all ship ps obs used in assimilation
obtype = 'ps'
ship_ps_hx = diag_conv.get_data_conv('hx', obtype, used=1, code=180)
ship_ps_obs = diag_conv.get_data_conv('obs', obtype, used=1, code=180)
fitsq = ((ship_ps_hx - ship_ps_obs)**2).mean()
print 'total number of ship surface pressure obs used in assimilation = %d ' % len(ship_ps_obs)
print 'OmF for all of  ship surface pressure obs used in assimilation = %f ' % np.sqrt(fitsq)

# Radiance diagnostic file

radobsfile = 'diag_amsua_n15_ges.2016030100'
diag_rad = gsidiag.GSIdiag(radobsfile, 'RADIANCE', endian='big')
nobsall = diag_rad.nobs
print 'total number of obs = %d ' % nobsall

# print OmF stats for all used obs from channel 7 with reasonable oberr
ichan = 7
hx = diag_rad.get_data_rad('hx', ichan, used=1, oberr=1.e9)
obs = diag_rad.get_data_rad('obs', ichan, used=1, oberr=1.e9)
fitsq = ((hx - obs)**2).mean()

print '%d obs used for channel %d out of %d' % (len(obs), ichan, nobsall)
print 'RMS OmF for used obs for channel %d = %f' % (ichan, np.sqrt(fitsq))

sind = 6
print 'Sample info'
print 'obs[%d] = %f, hx[%d] = %f, biascorr[%d] = %f, sum(biaspred[%d]) = %f' % (sind, diag_rad.obs[sind], sind, diag_rad.hx[sind], sind, diag_rad.biascorr[sind], sind, diag_rad.biaspred[1:,sind].sum())
