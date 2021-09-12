program kannsu
implicit none
integer::i,n 
real(8)::x,f

n=1000
x=0d0

open(1,file='kansu.dat')
do i=1,n

x=i*0.1d0

f=-(1d0/24d0)*x**4d0-4d0/21d0*x**3d0-1d0/168d0*x**2d0

write(1,*) x,f

end do

stop 
end program