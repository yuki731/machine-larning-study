program main
implicit none
complex :: n1,n2,n3,i,rp,rs,rp1,rs1,rs2,rp2,delta
real(8) :: pi,degree(1000),alpha(1000),beta(1000),beta1(1000)
integer :: k

!定数
pi=4.0*atan(1.0d0)
i=cmplx(0.0d0,1.0d0)
n1=cmplx(1.0d0,0.0d0) !空気の屈折率：n=1.0-0i
n2=cmplx(1.46d0,0.0d0) !薄膜の屈折率
n3=cmplx(3.85d0,-0.02d0) !シリコンの屈折率 n=3.85-0.02i

open(1,file='7-1-3a.dat')

do k = 1,900

degree(k)=0.1d0*dble(k)

alpha(k)=degree(k)/180.0d0*pi
beta(k)=asin((sin(alpha(k)))/real(n2)*real(n1)) 
beta1(k)=asin((sin(beta1(k)))/real(n3)*real(n2))
delta=2.0d0*pi/(632.8d-9)*n2*(40d-9)*cos(beta1(k))

rp1=(n2*cos(alpha(k))-n1*cos(beta(k)))/(n2*cos(alpha(k))+n1*cos(beta(k)))
rs1=(n1*cos(alpha(k))-n2*cos(beta(k)))/(n1*cos(alpha(k))+n2*cos(beta(k)))

rp2=(n3*cos(beta(k))-n2*cos(beta1(k)))/(n3*cos(beta(k))+n2*cos(beta1(k)))
rs2=(n2*cos(beta(k))-n3*cos(beta1(k)))/(n2*cos(beta(k))+n3*cos(beta1(k)))

rp=(rp1+rp2*exp(-2.0d0*i*delta))/(1.0d0+rp1*rp2*exp(-2.0d0*i*delta))
rs=(rs1+rs2*exp(-2.0d0*i*delta))/(1.0d0+rs1*rs2*exp(-2.0*i*delta))

write(1,*)degree(k),abs(rp),abs(rs)
end do
close(1) 
end program main