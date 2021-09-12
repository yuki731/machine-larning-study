program sample
implicit none
real(8)::a,b
integer::n

write(*,*) 'n=?'
read(*,*) n
write(*,*) 'a=?'
read(*,*) n
write(*,*) 'b=?'
read(*,*) n

write(*,*) a*n+b

stop
end program