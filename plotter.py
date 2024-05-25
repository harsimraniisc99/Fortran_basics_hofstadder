
import numpy as np
import matplotlib.pyplot as plt

file=open("eigenvalues.dat",'r')

data=file.readlines()
E1=[]
E2=[]
for line in data:
    lined=line.replace('(','').replace(')','').replace("\n","")
    line_data=lined.split(",")
    E1.append(float(line_data[0]))
    E2.append(float(line_data[1]))

E1=np.array(E1)
E2=np.array(E2)

En1=E1.reshape((101,101))
En2=E2.reshape((101,101))

x=np.linspace(-5,5,101)
y=np.linspace(-5,5,101)

x,y=np.meshgrid(x,y)

fig = plt.figure(figsize =(14, 9))
ax = plt.axes(projection ='3d')
 
# Creating plot
ax.plot_surface(x, y, En1)
plt.title("First Eigenvalue")
ax.plot_surface(x, y, En2)
plt.title("Second Eigenvalue")
plt.show()

