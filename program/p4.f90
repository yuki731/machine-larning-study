program furie1
implicit none
integer::i,k
real(8)::x,pi,f,fx,fxt,e

pi=4d0*atan(1d0)

e=exp(1d0)

open(1,file='furie4,1.dat')
open(2,file='furie4,2.dat')


do i=1,100
x=-1d0+(2d0/100d0)*i
write(1,*) x,f(x)


fxt=exp(abs(x))

write(2,*) x,fxt


end do


close(2)
close(1)

stop
end program

function f(x)
real(8)::f,x,pi,e
pi=4d0*atan(1d0)
e=exp(1d0)
f=e-1d0-(2d0/(pi**2d0+1d0))*(e+1d0)*cos(x)+(2d0/(4d0*pi**2d0+1d0))*(e-1d0)*cos(2d0*x)-(2d0/(9d0*pi**2d0+1d0))*(e+1d0)*cos(3d0*x)

return
end function