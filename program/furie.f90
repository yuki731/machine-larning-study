program furie1
implicit none
integer::i,k
real(8)::x,pi,f,fx,fxt

pi=4d0*atan(1d0)



open(1,file='furie1.dat')
open(2,file='furie2.dat')
open(3,file='furie3.dat')


do i=1,100
x=-pi+((2d0*pi)/100d0)*i
write(1,*) x,f(x)
write(3,*) x,fx(x)

if(-pi<=x.and.x<=0d0)then
fxt=0d0
write(2,*) x,fxt
else if(0d0<x.and.x<pi)then
fxt=sin(x)
write(2,*) x,fxt
end if



end do

close(3)
close(2)
close(1)

stop
end program

function f(x)
real(8)::f,x,pi
pi=4d0*atan(1d0)
f=1d0/pi+1d0/2d0*sin(x)-(2d0/(3d0*pi))*cos(2d0*x)-(2d0/(15d0*pi))*cos(4d0*x)-(2d0/(35d0*pi))*cos(6d0*x)

return
end function

function fx(x)
integer::i 
real(8)::pi,x,a,b,fx
pi=4d0*atan(1d0)

b=0d0

do i=1,1000
a=((-2)/(pi*(4d0*i**2d0-1d0)))*cos(2d0*i*x)

b=b+a
end do

fx=1d0/pi+1d0/2d0*sin(x)+b
return
end function








