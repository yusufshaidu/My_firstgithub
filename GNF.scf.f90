program flakes
implicit none
real(8)::y1,y2,alat,x0
character(len=4)::C,Li,H,O
REAL(8),ALLOCATABLE, DIMENSION(:) :: xcoord
integer::i,j,k,N

N=4
ALLOCATE(Xcoord(N))

alat=4.66*2 !In unit of bohr

x0=0.0

do i=1,N
xcoord(i)=x0 + (i-1)*alat/2.0

IF(xcoord(i)==(i-1)*alat) then
y1=alat/sqrt(3.0)
y2=2.0*alat/sqrt(3.0)

else if (xcoord(i)==(2*(i-1)-1)*alat/2.0) then
y1=alat/(2*sqrt(3.0))
y2=5.0*alat/(2*sqrt(3.0))
end if
write(*,*) 'C',xcoord(i),y1,0.0
write(*,*) 'C',xcoord(i),-y1,0.0
write(*,*) 'C',xcoord(i),y2,0.0
write(*,*) 'C',xcoord(i),-y2,0.0
end do
end program



