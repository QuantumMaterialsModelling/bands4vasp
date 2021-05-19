program getcirclines
  implicit none
  integer, parameter :: dp = kind(0.0d0)
  integer :: n,i,nweight
  real(dp), parameter :: pi = 4.0_dp*atan(1.0),eps=0.00001_dp
  real(dp) :: r,rs,re,dr,x,y,cx,cy,sx,sy,sz,ex,ey,ez,cz,alpha,dalpha,as,ae,ab,controla
  real(dp) :: rsx,rsy,rex,rey,w1,w2,w
  character(30) :: inchar,filename,kcoords
  logical :: ldr=.false.
  
  call get_command_argument(1,inchar)
  read(inchar,*) n
  call get_command_argument(2,kcoords)
  call get_command_argument(3,filename)
  call get_command_argument(4,inchar)
  read(inchar,*) nweight
  if(nweight==1)then
    call get_command_argument(5,inchar)
    read(inchar,*) w1
    call get_command_argument(6,inchar)
    read(inchar,*) w2
  end if

  open(unit=24,file=trim(kcoords),status='old',action='read')
    read(24,*) cx,cy,cz
    read(24,*) sx,sy,sz
    read(24,*) ex,ey,ez
  close(24)


 write(*,*) 'Get points from KPOINTS file'
 write(*,'(A,3f12.8)') 'center      ::',cx,cy,cz
 write(*,'(A,3f12.8)') 'start point ::',sx,sy,sz
 write(*,'(A,3f12.8)') 'end point   ::',ex,ey,ez
 write(*,*) ''
 rsx=sx-cx
 rsy=sy-cy
 rex=ex-cx
 rey=ey-cy


  rs=sqrt(rsx**2+rsy**2)
  re=sqrt(rex**2+rey**2)
  dr=0.0_dp
  if(abs(rs-re)>eps)then
    dr=(re-rs)/real(n-1,dp)
    ldr=.true.
  end if


 as=acos(rsx/sqrt(rsx**2+rsy**2))
 if(rsy<0.0_dp) as=2.0*pi-as
 ae=acos(rex/sqrt(rex**2+rey**2))
 if(rey<0.0_dp) ae=2.0*pi-ae

 ab=acos((rsx*rex+rsy*rey)/(sqrt(rsx*rsx+rsy*rsy)*sqrt(rex*rex+rey*rey)))
 controla=as+ab
 if(controla>2.0_dp*pi)controla=controla-2.0_dp*pi

 if(abs(controla-ae)>eps)ab=2.0_dp*pi-ab


  dalpha=ab/real(n-1,dp)
  alpha=as+dalpha
  r=rs+dr


  open(unit=23,file=trim(filename),status='replace',action='write')
  if(nweight==1)then
    write(23,'(4f12.8)') sx,sy,sz,w1
  else
    write(23,'(3f12.8)') sx,sy,sz
  end if
  do i=1,n-2
    if(nweight==1)then
      if(abs(w1-w2)<=eps)then
        w=w1
      else
        w=w1+((w2-w1)*(real(i,dp)/real(n,dp)))
      end if
      write(23,'(4f12.8)') cx+r*cos(alpha),cy+r*sin(alpha),cz,w
    else
      write(23,'(3f12.8)') cx+r*cos(alpha),cy+r*sin(alpha),cz
    end if
    alpha=alpha+dalpha
    if(ldr)r=r+dr
  end do
  if(nweight==1)then
    write(23,'(4f12.8)') ex,ey,ez,w2
  else
    write(23,'(3f12.8)') ex,ey,ez
  end if

  close(23)

end program getcirclines
