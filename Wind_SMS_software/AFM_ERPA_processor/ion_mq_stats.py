def ion_mq_stats(ion_name):

    '''
    Define dictionary of ion names and mass, charge values
    
    List of ions taken from Gruesbeck thesis 2013
    
    Inputs:
        ion_name - string name of ion
    '''
    ion_q={
        'H+':1,'H+_D':1, 'He+':1, 'He2+':2, 'C4+':4, 'C5+':5, 'C6+':6,
        'O+':1, 'O6+':6, 'O7+':7, 'Ne+':1,
        'Fe8+':8, 'Fe9+': 9, 'Fe10+':10, 'Fe11+':11,
        'Fe12+':12, 'Fe14+':14, 'Fe16+': 16, 'N+':1,
    } #electron charge
        
    ion_m={
        'H+':1.0079, 'H+_D':1.0079,'He+':4.0026, 'He2+':4.0026, 'C4+':12.011, 'C5+':12.011, 'C6+':12.011,
        'O+':15.999, 'O6+':15.999, 'O7+':15.999, 'Ne+': 20.180,
        'Fe8+':55.845, 'Fe9+': 55.845, 'Fe10+':55.845, 'Fe11+':55.845,
        'Fe12+':55.845, 'Fe14+':55.845, 'Fe16+': 55.845, 'N+':14.007
    } #atomic mass units, these are based on isotope ratios at Earth, so not exactly valid in SW/ M-sphere...
    #but, this is what jacob uses in his processor.
    

    return ion_m[ion_name],ion_q[ion_name]
