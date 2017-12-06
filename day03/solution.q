i:289326
w:sums (1 0;0 1;-1 0;0 -1)@(where 1+where (1+floor sqrt i)#2) mod 4
{sum abs x} w i-2
n:(raze (1-til 3),/:\:1-til 3)+\:
d:(enlist 0 0)!enlist 1
e:{i>d w x-1}{d[w x]:sum d n w x;x+1}/0
d w e-1
\\
