#small test library of functions
from datetime import timedelta, datetime
import calendar
import numpy as np

def convert_year_fraction(yearfrac):
    '''
    This function will read in a year fraction and return a datetime element
    Very inefficiently at the moment
    '''
    #preallocate datetime array to return
    #date=np.empty(np.size(yearfrac), dtype=np.datetime64)
    date=[] #can't figure out good prellocation yet
    for index in xrange(np.size(yearfrac)):
        year=yearfrac[index].astype(int)
        leap_add=calendar.isleap(year).astype(int)
        d=timedelta(days=(yearfrac[index]-year)*(365+leap_add))
        day_one=datetime(year,1,1)
        date.append(d+day_one)
        
    #year=yearfrac.astype(int)
    #leap_add=np.asarray([calendar.isleap(a).astype(int) for a in year])
    #
    #d=np.array([timedelta(days=(yearfrac[i]-year[i])*(365+ leap_add[i])) for i in xrange(np.size(yearfrac))])
    #day_one=datetime(year,1,1)
    #date=d+day_one
    return date
