import sys
import re
import imp
import csv
import argparse
import xml.etree.ElementTree
import math as m
import numpy as np
import time as t
import dateutil.parser
from shapely import geometry
from datetime import date, timedelta, datetime
from netCDF4 import Dataset

# Tricks for python 2.7 vs 3.5
try:
   urllib_info = imp.find_module('urllib')
   urllib = imp.load_module('urllib', *urllib_info)
   imp.find_module('request', urllib.__path__)
   from urllib.request import urlopen as urlopen
except ImportError:
   from urllib2 import urlopen as urlopen

# Analyze command line arguments
parser = argparse.ArgumentParser(description='Extract some measures from IMR OPEnDAP server.')
parser.add_argument('--data_file',nargs=1,help='Output CSV file'    ,default='imr_data.csv'             ,type=str  ,metavar='output.csv')
parser.add_argument('--long_lim' ,nargs=2,help='Longitude Interval' ,default=[5.0, 8.0 ]                ,type=float,metavar='MIN MAX')
parser.add_argument('--lat_lim'  ,nargs=2,help='Latitude Interval'  ,default=[55.0,60.0]                ,type=float,metavar='MIN MAX')
parser.add_argument('--date_lim' ,nargs=2,help='Dates Interval'     ,default=['2002/01/01','2002/05/30'],type=str  ,metavar='YYYY/MM/DD YYYY/MM/DD')
parser.add_argument('--depth_lim',nargs=2,help='Depth Interval'     ,default=[10.0,20.0]                ,type=float,metavar='MIN MAX')
parser.add_argument('--verbose'  ,nargs=1,help='Verbose Level [0-4]',default=[0]                        ,type=int  ,metavar='1')
parser.add_argument('--show_time',nargs=1,help='Show Time'          ,default=[0]                        ,type=int  ,metavar='1')
args = parser.parse_args()

data_file = args.data_file[0]
min_lon   = args.long_lim[0]
max_lon   = args.long_lim[1]
min_lat   = args.lat_lim[0]
max_lat   = args.lat_lim[1]
min_date  = dateutil.parser.parse(args.date_lim[0])
max_date  = dateutil.parser.parse(args.date_lim[1])
min_depth = args.depth_lim[0]
max_depth = args.depth_lim[1]
verbose   = args.verbose[0]
show_time = args.show_time[0]

#Convert a naive date to an aware date
def aware_date(date):
    #d = datetime(date.year,date.month,date.day,tzinfo=timezone.utc)
    d = datetime(date.year,date.month,date.day)
    return d

#Convert a date to a time (number of days since 1950) 
def date2time(date):
    #t = aware_date(date) - datetime(1950,1,1,tzinfo=timezone.utc)
    t = aware_date(date) - datetime(1950,1,1)
    return t.days
    
#Convert a time (number of days since 1950) to a date 
def time2date(time):
    #d = datetime(1950,1,1,tzinfo=timezone.utc) + timedelta(days=time)
    d = datetime(1950,1,1) + timedelta(days=time)
    return d

#Load data from one file and concatenate to global variables
def load_data(opendap_url,geo_bounds,time_depth_bounds,vess_name,vess_id,verbose=0,show_time=0):

    if verbose > 0: print("Processing",opendap_url)
    global lon, lat, tim, dep, temp, sal, cond, prof, vana, vaid
    start_chrono = t.time()
    dataset = Dataset(opendap_url,"r")
    if verbose > 3: print(dataset)
    if show_time: print("  -Opening file: %.4f s" % float(t.time()-start_chrono))
    
    #First check global attributes of dataset to see if there is any matching data
    lon_start   = float(dataset.getncattr("geospatial_lon_min"))
    lon_end     = float(dataset.getncattr("geospatial_lon_max"))
    lat_start   = float(dataset.getncattr("geospatial_lat_min"))
    lat_end     = float(dataset.getncattr("geospatial_lat_max"))
    depth_start = float(dataset.getncattr("geospatial_vertical_min"))
    depth_end   = float(dataset.getncattr("geospatial_vertical_max"))
    date_start  = aware_date(dateutil.parser.parse(dataset.getncattr("time_coverage_start")))
    date_end    = aware_date(dateutil.parser.parse(dataset.getncattr("time_coverage_end")))
            
    if verbose > 1: 
        print("     lon_start   = %.3f" % lon_start)
        print("     lon_end     = %.3f" % lon_end)
        print("     lat_start   = %.3f" % lat_start)
        print("     lat_end     = %.3f" % lat_end)
        print("     date_start  =",date_start,date2time(date_start))
        print("     date_end    =",date_end  ,date2time(date_end))
        print("     depth_start = %.0f" % depth_start)
        print("     depth_end   = %.0f" % depth_end)

    date_start             = date2time(date_start)
    date_end               = date2time(date_end)
    data_geo_bounds        = geometry.box(lon_start,lat_start,lon_end,lat_end)
    data_time_depth_bounds = geometry.box(date_start,depth_start,date_end,depth_end)
        
    if show_time: print("  -Defining bounds: %.4f s" % float(t.time()-start_chrono))
        
    if not data_geo_bounds.intersects(geo_bounds) or not data_time_depth_bounds.intersects(time_depth_bounds):
        dataset.close()
        return
    
    #Get all latitude, longitude and time values
    #Here: All latitude, longitude and time are downloaded !!
    longitude = dataset.variables["LONGITUDE"][:]
    latitude  = dataset.variables["LATITUDE"][:]
    alltime   = dataset.variables["TIME"][:]

    min_time  = time_depth_bounds.bounds[0]
    min_depth = time_depth_bounds.bounds[1]
    max_time  = time_depth_bounds.bounds[2]
    max_depth = time_depth_bounds.bounds[3]
    
    if verbose > 1: 
        print("     min_time   =",min_time)
        print("     min_depth  =",min_depth)
        print("     max_time   =",max_time)
        print("     max_depth  =",max_depth)
        print("     #positions =",alltime.size)

    if verbose > 2:
        print("     geo_bounds = ", geo_bounds)
    
    # Profile ID
    pid = 0
    if len(prof) > 0: pid = np.amax(prof)
        
    #Loop through time dimension to find which indices are wihin the bounding box
    for it in range(0,alltime.size):
        lon_lat = geometry.point.Point(longitude[it],latitude[it])
        if verbose > 2: print("       lon_lat = ", lon_lat)
        if geo_bounds.contains(lon_lat) and min_time <= alltime[it] <= max_time:
            if verbose > 0: print("  => Position:",longitude[it],latitude[it])
            #Date/time is stored as days since 01/01/1950
            date = time2date(np.asscalar(alltime[it]))
            if verbose > 0: print("  => Date Time:",date)
            #Here: All depths are downloaded !!
            #Note: First depth can be 0, 1 2 or 3 m...So we cannot get by indices
            depth = dataset.variables["DEPH"][it,:] # slice cannot be done in R
            depth = depth[~depth.mask]
            if verbose > 2: print("     #depths = ", depth.size)
            min_ide = depth.size
            max_ide = 0
            for ide in range(0,depth.size):
                if min_depth >= depth[ide]:
                    if verbose > 2: print("     ",ide,": ",min_depth,">=",depth[ide])
                    min_ide = ide
                if max_depth >= depth[ide]:
                    if verbose > 2: print("     ",ide,": ",max_depth,">=",depth[ide])
                    max_ide = ide
            nd = max_ide - min_ide + 1
            if verbose > 2: print("     ",min_ide,max_ide,nd)
            if max_ide <= 0 or nd <= 0:
                if verbose > 0: print("    => No measure selected.")
            else:
                if verbose > 0: print("    => Selecting",nd,"measures...")
                
                #New profile id
                pid = pid + 1
        
                # Append to global variables
                lon  = np.ma.append(lon,np.repeat(longitude[it],nd))
                lat  = np.ma.append(lat,np.repeat(latitude[it],nd))
                tim  = np.ma.append(tim,np.repeat(alltime[it],nd))
                dep  = np.ma.append(dep,depth[min_ide:max_ide+1])                         # slice cannot be done in R
                temp = np.ma.append(temp,dataset.variables["TEMP"][it,min_ide:max_ide+1]) # slice cannot be done in R
                sal  = np.ma.append(sal,dataset.variables["PSAL"][it,min_ide:max_ide+1])  # slice cannot be done in R
                cond = np.ma.append(cond,dataset.variables["CNDC"][it,min_ide:max_ide+1]) # slice cannot be done in R
                prof = np.ma.append(prof,np.repeat(pid,nd))
                vana = np.ma.append(vana,np.repeat(vess_name,nd))
                vaid = np.ma.append(vaid,np.repeat(vess_id,nd))

                lt = len(temp)
                if len(lon) != lt or len(lat) != lt or len(tim) != lt or len(dep) != lt or len(sal) != lt or len(cond) != lt:
                    print("ERROR in", opendap_url)
                    dataset.close()  
                    sys.exit(2)
                             
    if show_time: print("  -Retrieving measures: %.4f s" % float(t.time()-start_chrono))
        
    dataset.close()     

#Following code demonstrates using the catalog to find list of datatsets to processing
def load_all_data(geo_bounds,min_date,max_date,min_depth,max_depth,server_type="thredds",verbose=0,show_time=0):
    #Download subcatalog
    if server_type is "thredds":
        catalog_url = "http://opendap1-test.nodc.no/thredds/catalogs/physics/physics_point_yearly.xml"
    else:
        catalog_url = "http://opendap1.nodc.no/opendap/physics/point/yearly/catalog.xml"
    contents = urllib.request.urlopen(catalog_url).read()
    
    #Parse xml
    e = xml.etree.ElementTree.fromstring(contents)
    
    #Vessels dictionnary
    max_vaid = 1
    vessels = {}
    
    #Find dataset list
    root_dataset = e.find('{http://www.unidata.ucar.edu/namespaces/thredds/InvCatalog/v1.0}dataset')
    for child in root_dataset:
        if server_type is "thredds":
            file_name = child.get('urlPath')
            url = "http://opendap1-test.nodc.no/thredds/dodsC/"+file_name
        else:
            file_name = child.get('ID')
            url = "http://opendap1.nodc.no"+file_name

        vess_name = re.search('58(..)_CTD', file_name)
        if vess_name:
            vess_name = vess_name.group(1)
        else:
            print("ERROR: wrong file name", file_name)
            sys.exit(2)
        if vess_name in vessels:
            vess_id = vessels[vess_name]
        else:
            vessels[vess_name] = max_vaid
            vess_id = max_vaid
            max_vaid = max_vaid + 1
        min_time = date2time(min_date)
        max_time = date2time(max_date)
        time_depth_bounds = geometry.box(min_time,min_depth,max_time,max_depth)
        load_data(url,geo_bounds,time_depth_bounds,vess_name,vess_id,verbose,show_time)

        
#######################################################
# Global script
# Filter for a certain bounding box between a given
# depth interval and time interval
#######################################################

print("Creating",data_file,"...")

if verbose > 0:
    print("  data_file =",data_file)
    print("  min_lon   =",min_lon)
    print("  max_lon   =",max_lon)
    print("  min_lat   =",min_lat)
    print("  max_lat   =",max_lat)
    print("  min_date  =",min_date,date2time(min_date))
    print("  max_date  =",max_date,date2time(max_date))
    print("  min_depth =",min_depth)
    print("  max_depth =",max_depth)
    print("  verbose   =",verbose)
    print("  show_time =",show_time)
    
# Global variables
lon  = np.ma.array([])
lat  = np.ma.array([])
tim  = np.ma.array([])
dep  = np.ma.array([])
temp = np.ma.array([])
sal  = np.ma.array([])
cond = np.ma.array([])
prof = np.ma.array([])
vana = np.ma.array([])
vaid = np.ma.array([])

start_chrono = t.time()

geo_bounds = geometry.box(min_lon,min_lat,max_lon,max_lat)
load_all_data(geo_bounds,min_date,max_date,min_depth,max_depth,"thredds",verbose,show_time)

elapsed_time = t.time() - start_chrono
print("Total number of selected samples:",len(temp))
print("Saving",data_file)

#Dump CSV file
with open(data_file, 'w') as csvfile:
    csvwriter = csv.writer(csvfile, delimiter=',')
    csvwriter.writerow(["Longitude", "Latitude", "Time", "Depth", "Temperature", "Salinity", "Conductivity", "Profil_id", "Vessel_name", "Vessel_id"])
    for i in range(0,len(temp)-1):
        csvwriter.writerow([lon[i],lat[i],tim[i],dep[i],temp[i],sal[i],cond[i],prof[i],vana[i],vaid[i]])
        
print("Elapsed time: %d s" % elapsed_time)
