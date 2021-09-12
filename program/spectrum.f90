program spectrum
implicit none 
integer::i,o
real(8)::T,te,w,v,wx,wy,wz,B,D,a,r,be,At,gamma,lambda,Ta,Tb,f,j

do i=1,2

write(*,*) 'What status ?'
write(*,*) '(please write 1 or 2. Example:A is 1,B is 2)'
read(*,*) o

write(*,*) 'What is the value of v ?'
read(*,*) v

if(o==1)then
te=50203.63d0
w=1460.64d0
wx=13.872d0
wy=0.0103d0
wz=-0.00197d0
be=1.4546d0
a=0.018d0
r=-8.80d0*10d0**(-5d0)
D=6.15d0*10d0**(-6d0)
gamma=-0.003d0
lambda=-1.33d0
open(1,file='spectrumA.dat')
write(1,*) 'A state'
write(1,*) 'T=',T(v)
write(1,*) 'B=',B(v)
write(1,*) 'D=',D
close(1)
Ta=T(v)
else if(o==2)then
te=59619.35d0
w=1733.39d0
wx=14.122d0
wy=-0.0569d0
wz=0.00361d0
be=1.63745d0
a=0.01791d0
r=7.7d0*10d0**(-5d0)
D=5.90d0*10d0**(-6d0)
At=42.24d0
open(1,file='spectrumB.dat')
write(1,*) 'B state'
write(1,*) 'T=',T(v)
write(1,*) 'B=',B(v)
write(1,*) 'D=',D
close(1)
Tb=T(v)
end if
end do

f=abs(Ta-Tb)
j=1d0/f*10d0**7d0
write(*,*) j

stop
end program


function T(v)
real(8)::T,te,w,v,wx,wy,wz

T=te+w*(v+1d0/2d0)-wx*(v+1d0/2d0)**2d0+wy*(v+1d0/2d0)**3d0-wz*(v+1d0/2d0)**4d0

return
end function


function B(v)
real(8)::B,be,a,v,r

B=be-a*(v+1d0/2d0)+r*(v+1d0/2d0)**2d0

return
end function










