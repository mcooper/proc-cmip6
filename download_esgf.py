#https://esgf-pyclient.readthedocs.io/en/latest/examples.html
from pyesgf.search import SearchConnection
from pyesgf.logon import LogonManager
import xarray as xr
import os

lm = LogonManager()

OPENID = 'https://esgf-data.dkrz.de/esgf-idp/openid/amadoukone'

lm.logon_with_openid(openid=OPENID, password=None, bootstrap=True)
lm.is_logged_on()

os.chdir('/home/mattcoop/maldat/raw/CMIP6/daily')

conn = SearchConnection('https://esgf-node.llnl.gov/esg-search',
                        distrib=True)

experiments = ['ssp126', 'ssp245', 'ssp370', 'ssp585']
variables = ['tasmax', 'pr']

sources = ['BCC-CSM2-MR', 'CMCC-ESM2', 'MRI-ESM2-0']

# Worked for everything except IPSL-CM6A-LR

def get_opendap_url(e, s, v):
    ctx = conn.new_context(
        latest = True,
        replica = False,
        project='CMIP6',
        source_id=s,
        experiment_id=e,
        variable=v,
        grid_label=['gn', 'gr'],
        variant_label='r1i1p1f1',
        frequency='day'
    )
    results = ctx.search()
    files = results[0].file_context().search()
    return(files[0].opendap_url)

def process(e, s, v):
    if e + '_' + s + '_' + v + '.nc' in os.listdir():
       return(None) 
    url = get_opendap_url(e, s, v)
    ds = xr.open_dataset(url, chunks={'time': 120})
    ds = ds.assign_coords(lon=(((ds.lon + 180) % 360) - 180)).sortby('lon')
    ds.to_netcdf(e + '_' + s + '_' + v + '.nc', 'w', 'NETCDF3_CLASSIC')
    print(e + '_' + s + '_' + v + '.nc')

for s in sources:
    for e in experiments:
        for v in variables:
            process(e, s, v)

