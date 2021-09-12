program main
implicit none
complex :: n,n1,n2,n3,i,rp,rs,rp1,rs1,rp2,rs2
real(8) :: alpha,beta,ganma,pi,d,delta,ramda,delta_p,delta_s,del
integer :: k

!定数
pi=4.0*atan(1.0d0)
i=cmplx(0.0d0,1.0d0)
n1=cmplx(1.0d0,0.0d0) !空気の屈折率：n=1.0-0i
n2=cmplx(1.46d0,0.0d0) !薄膜の屈折率
n3=cmplx(3.85d0,-0.02d0) !シリコンの屈折率 n=3.85-0.02i
d=50.0d0*10.0d0**(-9.0d0)
ramda=632.8d0*10.0d0**(-9.0d0)

open(1,file='7-1-4a.dat')

!入射角入力
write(*,*) 'alpha='
read(*,*) alpha

alpha=alpha*pi/180.0d0
beta=asin((sin(alpha))/real(n2)*real(n1))  
ganma=asin((sin(beta))/real(n3)*real(n2))  

do k=0,200

d=(dble(k))*10.0d0**(-9.0d0)

delta=(2.0d0*pi/ramda)*n2*d*cos(beta)

rp1=(n2*cos(alpha)-n1*cos(beta))/(n2*cos(alpha)+n1*cos(beta))
rs1=(n1*cos(alpha)-n2*cos(beta))/(n1*cos(alpha)+n2*cos(beta))

rp2=(n3*cos(beta)-n2*cos(ganma))/(n3*cos(beta)+n2*cos(ganma))
rs2=(n2*cos(beta)-n3*cos(ganma))/(n2*cos(beta)+n3*cos(ganma))

rp=(rp1+rp2*exp(-2.0d0*i*delta))/(1.0+rp1*rp2*exp(-2.0d0*i*delta))
rs=(rs1+rs2*exp(-2.0d0*i*delta))/(1.0+rs1*rs2*exp(-2.0d0*i*delta))

n=rp/rs

delta_p=atan2(imag(rp),real(rp))
delta_s=atan2(imag(rs),real(rs))

del=delta_p-delta_s

write(1,*) k,abs(n),tan(del)

end do

close(1)

end program main
