'''
Calculate some interest rate stuff

'''
import numpy as np
import matplotlib.pyplot as plt
r=np.linspace(0.0, 1.0, 100)

annual_rate=r
cont_rate=np.exp(r)-1


fig=plt.figure()
ax=plt.subplot(111)
plt.plot(r,annual_rate, marker='o', color='red')
plt.plot(r,cont_rate, marker='o', color='blue')
ax.set_xlabel(' "Interest Rate" ')
ax.set_ylabel('Effective Annual Interest Rate')

plt.show()
