import os, sys, re, glob

files = glob.glob("/workspace/Shared/Michael/CMIP5/data/OImon/mon/historical/r1i1p1/*.nc")

for nc in files:
    print "working on: "+nc
    os.system("cdo remapbil,t42grid "+nc+" "+os.path.join("/workspace/Shared/Michael/CMIP5/data/OImon/mon/historical/t42/",os.path.basename(nc)[:-3]+"_t42grid.nc"))

# this is the command to do this in cdo
# cdo remapbil,t42grid /workspace/Shared/Michael/CMIP5/data/OImon/mon/historical/r1i1p1/sic_OImon_GFDL-CM3_historical_r1i1p1_186001-186412.nc /workspace/Shared/Michael/CMIP5/data/OImon/mon/historical/r1i1p1/sic_OImon_GFDL-CM3_historical_r1i1p1_186001-186412_CHANGED.nc
