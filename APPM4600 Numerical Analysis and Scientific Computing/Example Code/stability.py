########################################################################
# This python script presents examples of algorithm stability illustrated
# on the notes (equivalent to stability_example.m)
# APPM 4600 Fall 2022
########################################################################
import mypkg.my2DPlotB
import numpy as np; # import numpy

# number of terms 
N = 11
#  example 1
# recursion vs formula
p = np.zeros(N) # actual sequence
p1 = np.zeros(N) # approximation
p2= np.zeros(N) # recursion relation
# Initialize the approximations
p[0] = 1.
p[1] = 1./3.
p1[0] = 1.
p1[1] = 1./3.
p2[0] = 1.
p2[1] = 1./3.
# we compute each sequence as described in the notes.
for j in range(2,N):
    p[j] = np.power(1./3.,j)
    p1[j] = np.power(1./3.,j) - 0.125*1.e-5*np.power(3.,j)
    p2[j] = (10./3.)*p2[j-1]-p2[j-2]
  
  
# plot curves for the first N iterates for the actual sequence, approximation and
# recursion relation
x = np.arange(0,N)
plt = mypkg.my2DPlotB(x[2:N],p[2:N])
plt.labels('iteration','value')
plt.addPlot(x[2:N],p1[2:N])
plt.addPlot(x[2:N],p2[2:N])
plt.show()

input()
# plot of the absolute errors between the sequence and both methods
plt2 = mypkg.my2DPlotB(x[2:N],np.abs(p[2:N]-p1[2:N]))
plt2.labels('iteration','absolute err')
plt2.addPlot(x[2:N],np.abs(p[2:N]-p2[2:N]))
plt2.logy()
plt2.show()
input()
# plot of relative errors between the sequence and both methods
plt3 = mypkg.my2DPlotB(x[2:N],np.abs(p[2:N]-p1[2:N])/np.abs(p[2:N]))
plt3.addPlot(x[2:N],np.abs(p[2:N]-p2[2:N])/np.abs(p[2:N]))
plt3.logy()
plt3.show()

