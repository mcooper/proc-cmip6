import pandas as pd
import xarray as xr
import numpy as np
import os
import math

#Each pixel is ~30m, so a 500x500 scene is 15km x 15km
SCENE_SIZE = 500

os.chdir(os.path.expanduser('~/maldat/raw/'))

mal = pd.read_csv('../train/all_comb.csv')[['id', 'latitude', 'longitude']]
mal['lat_simple'] = mal['latitude'].apply(math.floor)
mal['lon_simple'] = mal['longitude'].apply(math.floor)

smp_df = mal[['lon_simple', 'lat_simple']].drop_duplicates()

def read_tif(f):
    """read in a tif file with xr.open_rasterio and name the tiff"""
    if os.path.isfile(f):
        r = xr.open_rasterio(f).values
    else:
        #If a file is missing, its in the ocean.  Use ones for sea level elevation, 
        #also for ocean landcover type in ocean file
        r = np.ones([1, 3601, 3601])
    return r

def latcode(lat):
    if lat >= 0:
        o = 'N'
    else:
        o = 'S'
    return(o + str((100 + abs(lat)))[1:3])

def loncode(lon):
    if lon >= 0:
        o = 'E'
    else:
        o = 'W'
    return(o + str((1000 + abs(lon)))[1:4])

def get_coord_codes(latitude, longitude):
    cc_lat = math.floor(latitude) 
    cc_lon = math.floor(longitude)
    uc_lat = math.ceil(latitude + 0.000001)
    uc_lon = math.floor(longitude)
    ur_lat = math.ceil(latitude + 0.000001)
    ur_lon = math.ceil(longitude + 0.000001)
    cr_lat = math.floor(latitude)
    cr_lon = math.ceil(longitude + 0.000001)
    lr_lat = math.floor(latitude) - 1
    lr_lon = math.ceil(longitude + 0.000001)
    lc_lat = math.floor(latitude) - 1
    lc_lon = math.floor(longitude)
    ll_lat = math.floor(latitude) - 1
    ll_lon = math.floor(longitude) - 1
    cl_lat = math.floor(latitude)
    cl_lon = math.floor(longitude) - 1
    ul_lat = math.ceil(latitude + 0.0000001)
    ul_lon = math.floor(longitude) - 1
    
    codedict = {'cc': latcode(cc_lat) + loncode(cc_lon),
                 'uc': latcode(uc_lat) + loncode(uc_lon),
                 'ur': latcode(ur_lat) + loncode(ur_lon),
                 'cr': latcode(cr_lat) + loncode(cr_lon),
                 'lr': latcode(lr_lat) + loncode(lr_lon),
                 'lc': latcode(lc_lat) + loncode(lc_lon),
                 'll': latcode(ll_lat) + loncode(ll_lon),
                 'cl': latcode(cl_lat) + loncode(cl_lon),
                 'ul': latcode(ul_lat) + loncode(ul_lon)}
    return(codedict)

def read_mosaic_one(codedict, pref='GTM'):
    if pref == 'GTM':
        fname = 'ASTGTM/ASTGTMV003_XXXXXXX_dem.tif'
    else:
        fname = 'ASTWBD/ASTWBDV001_XXXXXXX_att.tif'
    
    newd = {}
    for v in codedict.keys():
        tif = read_tif(fname.replace('XXXXXXX', codedict[v]))
        #They overlap by a pixel, so remove the last pixel from the arrays
        newd[v] = tif[ :, slice(0, 3600), slice(0, 3600)]
    
    res = np.concatenate((
        np.concatenate((newd['ul'], newd['uc'], newd['ur']), axis=2),
        np.concatenate((newd['cl'], newd['cc'], newd['cr']), axis=2),
        np.concatenate((newd['ll'], newd['lc'], newd['lr']), axis=2)),
        axis=1)
    
    return res

def read_mosaic_both(codedict):
    gtm = read_mosaic_one(codedict, 'GTM')
    wbd = read_mosaic_one(codedict, 'WBD')
    fin = np.concatenate((gtm, wbd), axis=0)
    return fin

def find_index(val):
    midpoint=round(3601*(val - math.floor(val))) + 3600
    high = int(midpoint + SCENE_SIZE/2)
    low = int(midpoint - SCENE_SIZE/2)
    return slice(low, high)

def write_nparray(res, filename):
    """Utility function to write array as ascii file
    Array must be only two dimensions"""
    with open(filename, 'w') as dst:
        dst.write('ncols ' + str(res.shape[1]) + '\n')
        dst.write('nrows ' + str(res.shape[2]) + '\n')
        dst.write('xllcorner 0\n')
        dst.write('yllcorner 0\n')
        dst.write('cellsize 0.5\n')
        dst.write('nodata_value -9999\n')
        dst.write('nodata_value -9999\n')
        for i in res[0]:
            dst.write(' '.join([str(j) for j in i]) + '\n')
        dst.close()

# Iterate over scene coordinates and create scenes
for r in smp_df.reset_index().iterrows():
    print(r[0]/len(smp_df))
    code = get_coord_codes(r[1]['lat_simple'], r[1]['lon_simple'])
    scene = read_mosaic_both(code)
    
    # Get all points within the scene, then determine and save subset for each point
    sel = mal.loc[(mal.lat_simple == r[1]['lat_simple']) & (mal.lon_simple == r[1]['lon_simple'])]
    for s in sel.iterrows():
        scene_sub = scene[ : , find_index(1 - s[1]['latitude']), find_index(s[1]['longitude'])]
        np.save('../train/nn_topo/' + str(int(s[1]['id'])), scene_sub)

os.system('telegram "Done Processing Malaria Topo"')
os.system('sudo poweroff')


# Read in and comebine all images
# Ensuring that they are sorted in order by 'id'
fs = os.listdir('../train/nn_topo')
d = {}
for f in fs:
    d[int(f[:-4])] = f

keys = list(d.keys())
keys.sort()

allimg = []
for k in keys:
    print(keys.index(k)/len(keys))
    f = d[k]
    allimg.append(np.load('../train/nn_topo/' + f))

allimg = np.stack(allimg, axis=3)
np.save('../train/alltopo', allimg)


import h5py
f = h5py.File("train/alltopo.hdf5", "w")
f.create_dataset("alltopo", dtype='f', data=alltopo)



