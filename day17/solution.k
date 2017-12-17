/ k 2016.08.09
s:382                 / step
i:{(y#z),x,(y:y+1)_z} / insert x after index y in z
a:*|2017{(p+1;n;i[n:1+x 1;p:(#c)!s+*x;c:*|x])}/(0;0;,0)
a@(#a)!1+a?2017
b:(50000000{(1+c!s+*x;c:1+*|x)}\(0;0))[;0] / TODO calculate properly
*|&1=b
\\
