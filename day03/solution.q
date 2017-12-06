i:289326
p:(1 0;0 1;-1 0;0 -1)
w:(enlist 0 0),sums p@(where 1+where (1+floor sqrt i)#2) mod 4
{sum abs x} w i-1
n:(raze (1-til 3),/:\:1-til 3)+\:
d:w!1,1_(count w)#0N
e:{i>d w x-1}{d[w x]:sum d n w x;x+1}/1
d w e-1
\\
