#!/usr/bin/env python

# -------------------------------------------------------------------
#  author:      Julien Ruffault (julien.ruff&@gmail.com) & Arsene Druel
#  Date  :      2020-03-07
# -------------------------------------------------------------------
# ERA5-land data downloader for area, variables and time of interest.
# - Last modifiedd:  1-12-08 by Arsene Druel (add restart)
# -------------------------------------------------------------------
# !!!!! : Requires cdsapi (see at https://cds.climate.copernicus.eu/api-how-to)
# -------------------------------------------------------------------

import cdsapi
c = cdsapi.Client()
import os

## OPTIONS ##
#############
# subset
lonmin = -25
lonmax =  51
latmin =  18.5
latmax =  81

# define years (between 1950 and 2020 include)
ystart = 1950
yend   = 1980


# Variables (Main surface variables for functionnal forest modeling)
VARS = ['10m_v_component_of_wind',
        '2m_temperature',
        'surface_solar_radiation_downwards',
        '2m_dewpoint_temperature',
        'surface_pressure',
        'total_precipitation',
        '10m_u_component_of_wind',]


# to relaunch from a specific date: True  (else: False)
restart = False # To launch again a download from  specific year and month
if restart:
    yrestart = 1964 # Specific year
    mrestart = 6    # Specific month


## SCRIPT ## (do not change below)
############
else:
    yrestart = -999
    mrestart = 0

the_subset = "/".join(["{:.2f}".format(x) for x in [latmax, lonmin, latmin, lonmax]])


# Years 
YEARS  =  [x for x in map(str, range(ystart, yend+1))]
MONTHS =  [x for x in map(str, range(1, 13))]

for Y in YEARS:
    for M in MONTHS:
        if ( (not restart) or ( int(Y) >  yrestart ) or ( int(Y) == yrestart and int(M) >= mrestart) ):
            target_filename = 'ERA5_land_Europe/era5_land-hourly_Europe-allV-'+M+'-'+Y+'.nc'
            print(target_filename)
            if not os.path.isfile(target_filename):
                c.retrieve(
	                'reanalysis-era5-land',
	                {
	                    'variable':VARS,
                            "area": the_subset,
      	                    'year': Y,
			     'month': M,
		            'day':[
		                '01','02','03',
	  	                '04','05','06',
		                '07','08','09',
		                '10','11','12',
		                '13','14','15',
		                '16','17','18',
		                '19','20','21',
		                '22','23','24',
		                '25','26','27',
		                '28','29','30',
		                '31'
		            ],
	                    'time':[
	                        '00:00','01:00','02:00',
	                        '03:00','04:00','05:00',
	                        '06:00','07:00','08:00',
	                        '09:00','10:00','11:00',
	                        '12:00','13:00','14:00',
	                        '15:00','16:00','17:00',
	                        '18:00','19:00','20:00',
	                        '21:00','22:00','23:00'
	                    ],
	                    'format':'netcdf'
	                },
	                target_filename)

