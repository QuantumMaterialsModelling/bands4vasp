program getsurfacelines
  implicit none
  integer, parameter :: dp = kind(0.0d0)
  integer :: n, i
  real(dp) :: x(3), y(3), z(3), w(3)
  real(dp) :: t, dt
  real(dp) :: start(3), endp(3)
  real(dp) :: wstart, wend
  character(128) :: coordsfile, outfile, inchar, line*256
  integer :: ios
  logical :: has_w = .false.
  real(dp) :: x4, y4, z4, w4


  call get_command_argument(1, inchar)
  read(inchar,*) n
  call get_command_argument(2, coordsfile)
  call get_command_argument(3, outfile)

  ! Einlesen: 3 Punkte: 1, 2, 3, optional Gewicht
  open(unit=10, file=trim(coordsfile), status='old', action='read', iostat=ios)
  if (ios /= 0) then
    write(*,*) 'Error: Cannot open ', trim(coordsfile)
    stop 2
  end if

  read(10,'(A)',iostat=ios) line
  if (ios /= 0) then
    write(*,*) 'Error: reading ', trim(coordsfile)
    stop 3
  end if
  if (count([(line(i:i).eq.' ', i=1,len_trim(line))])>=3) then
    has_w = .true.
    read(line,*) x(1), y(1), z(1), w(1)
    read(10,*) x(2), y(2), z(2), w(2)
    read(10,*) x(3), y(3), z(3), w(3)
  else
    has_w = .false.
    read(line,*) x(1), y(1), z(1)
    read(10,*) x(2), y(2), z(2)
    read(10,*) x(3), y(3), z(3)
  end if
  close(10)

  ! Berechne Punkt 4: Punkt4 = Punkt3 + (Punkt1 - Punkt2)
  x4 = x(3) + (x(1) - x(2))
  y4 = y(3) + (y(1) - y(2))
  z4 = z(3) + (z(1) - z(2))
  if (has_w) then
    w4 = w(3) + (w(1) - w(2))
  end if

  dt = 1.0_dp / real(n-1, dp)

  open(unit=11, file=trim(outfile), status='replace', action='write')

  do i = 0, n-1
    t = dt * real(i, dp)
    ! Interpolierte Startpunkte: von Punkt 1 zu Punkt 4
    start(1) = x(1) + t*(x4 - x(1))
    start(2) = y(1) + t*(y4 - y(1))
    start(3) = z(1) + t*(z4 - z(1))
    ! Interpolierte Endpunkte: von Punkt 2 zu Punkt 3
    endp(1) = x(2) + t*(x(3) - x(2))
    endp(2) = y(2) + t*(y(3) - y(2))
    endp(3) = z(2) + t*(z(3) - z(2))
    if (has_w) then
      wstart = w(1) + t*(w4 - w(1))
      wend   = w(2) + t*(w(3) - w(2))
      write(11,'(4f16.10)') start(1), start(2), start(3), wstart
      write(11,'(4f16.10)') endp(1),  endp(2),  endp(3),  wend
    else
      write(11,'(3f16.10)') start(1), start(2), start(3)
      write(11,'(3f16.10)') endp(1),  endp(2),  endp(3)
    end if
  end do

  close(11)

end program getsurfacelines
