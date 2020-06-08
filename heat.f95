program FTCS
parameter(kx=11, kt=1.0e5)
integer :: x,t
real :: temp(kx,kt), time(kt), position(kx), pi, c, dx, dt, k

k = 4.0
dx = 0.1
dt = 1.0e-5
c = k*(dt)/(dx)**2
pi = acos(-1.)

open(1, file='heat1.dat', status= 'unknown')
open(3, file= 'heat2.dat', status= 'unknown')

position(1)=0.0
position(kx)=1.0

do x=1,kx-1
  position(x+1)=position(x)+dx
end do

time(1)=0.0

do t=1,kt-1
  time(t+1)=time(t)+ t*dt
end do

do t=1,kt
  temp(1,t)=0.0
  temp(kx,t)=0.0
end do

do t=1,kt
  do x=2,kx-1
    temp(x,t)= 100.0* sin(pi*position(x))
  end do
end do

do t=1, kt-1
  do x=2,kx-1
    temp(x,t+1)=temp(x,t)+ c*(temp(x+1,t)-2* temp(x,t)+temp(x-1,t))
  end do
end do

do t=1,kt
  write(1,2)time(t), (temp(x,t), x=1, kx)
end do
2 format(11(f7.3,1x))

do x=1,kx
  write(3,4)position(x), (temp(x,t),t=1,10000,1000)
end do
4 format(12(f7.3,1x))

end program FTCS 
  
  
  
  
  