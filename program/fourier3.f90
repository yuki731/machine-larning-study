program furie1
implicit none
integer::i,k
real(8)::x,pi,f,fx,fxt

pi=4d0*atan(1d0)



open(1,file='furie3,1.dat')
open(2,file='furie3,2.dat')


do i=1,100
x=-pi+((2d0*pi)/100d0)*i
write(1,*) x,f(x)


fxt=x**2d0

write(2,*) x,fxt


end do


close(2)
close(1)

stop
end program

function f(x)
real(8)::f,x,pi
pi=4d0*atan(1d0)
f=2d0/3d0*pi**2d0-4d0*cos(x)+cos(2d0*x)-(4d0/(9d0))*cos(3d0*x)

return
end function

