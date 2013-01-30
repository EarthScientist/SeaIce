import os, sys, re, glob

files = glob.glob("/workspace/Shared/Michael/CMIP5/data/OImon/mon/historical/r1i1p1/*sic_*.nc")

for nc in files:
    print "working on: "+nc
    os.system("cdo remapbil,t85grid "+nc+" "+os.path.join("/workspace/Shared/Michael/CMIP5/data/OImon/mon/historical/t85/",os.path.basename(nc)[:-3]+"_t85grid.nc"))

# this is the command to do this in cdo
# cdo remapbil,t42grid /workspace/Shared/Michael/CMIP5/data/OImon/mon/historical/r1i1p1/sic_OImon_GFDL-CM3_historical_r1i1p1_186001-186412.nc /workspace/Shared/Michael/CMIP5/data/OImon/mon/historical/r1i1p1/sic_OImon_GFDL-CM3_historical_r1i1p1_186001-186412_CHANGED.nc
