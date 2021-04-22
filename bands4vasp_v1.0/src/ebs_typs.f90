MODULE ebs_typs
USE MATH
 IMPLICIT NONE

                                                                     
 TYPE :: BAND 
   INTEGER :: PATH = 0
   INTEGER :: SPN = 1
   INTEGER :: NKPT = 0
   INTEGER :: BND = 0
   REAL(KIND=DP) :: K = 0.0_DP            !K => KPOINTDISTANCE
   REAL(KIND=DP) :: E = 0.0_DP            !E>ENERGY
   REAL(KIND=DP) :: W = 0.0_DP            !W> BLOCH CHARACTER
   REAL(KIND=DP), DIMENSION(:),ALLOCATABLE :: OC
   LOGICAL :: LOCU = .FALSE.                !OCUPATION
 END TYPE BAND

 TYPE :: GRID
   REAL(KIND=DP) :: DIST  !KPOINTDISTANCE
   REAL(KIND=DP),DIMENSION(3) :: A  !REC VEKTOR
   REAL(KIND=DP),DIMENSION(3) :: B  !REC VEKTOR
 END TYPE GRID


 TYPE :: FILENUMBERS
  INTEGER :: NK, KPTS, IKPTS, ORBIT, BND, IONS, SPIN
  LOGICAL :: SLIMPRJ, OLDPRO, KCONV, LKT
 END TYPE FILENUMBERS

 TYPE :: NUMBERS
   INTEGER :: PATH, BAND, ORB
   INTEGER :: TKPTS, TCOL, COL, SAMEK
   INTEGER,DIMENSION(2) :: FIT=0, SPEC=0
   LOGICAL,DIMENSION(2) :: SPECDAT=.FALSE.
 END TYPE NUMBERS

 TYPE :: PARAMETERS
   INTEGER :: POINTS, FORB, IREFLECT, FGRID, SPN, NSION
   INTEGER,DIMENSION(2) :: KSKIP,SION=(/ 0, 0 /)
   REAL(DP) :: TRASH, EDIF, EGAP, DBLOCH
   REAL(DP) :: EFERMI,  ODISTINCT, EPS, DGRAD
   REAL(DP) :: SIGMA,SPECDELTA
   REAL(DP),DIMENSION(2) :: FBORDER,WDIF

   REAL(KIND=DP), DIMENSION(2,3) :: SYMPOINTS
   CHARACTER(LEN=20),DIMENSION(:),ALLOCATABLE :: ORBNAMES
   LOGICAL :: BAND=.TRUE.,BANDPLOT=.TRUE.,ROOTSCALC=.TRUE.,LSURFACE
   LOGICAL :: POLY=.FALSE.,LBAV=.FALSE.,LOAV=.TRUE.,NORM=.FALSE.
   LOGICAL :: SPECFUN=.TRUE.,SYMREC=.TRUE.,SLIMSPEC=.TRUE.,MULTIFERMI=.FALSE.
 END TYPE PARAMETERS


!interface printebs
! subroutine aprintebs(ebs,fname)
!         type(band),dimension(:,:,:) :: ebs
!         character(len=*) :: fname
! end subroutine aprintebs
!
! subroutine bprintebs(ebs,fname)
!         type(band),dimension(:,:) :: ebs
!         character(len=*) :: fname
! end subroutine bprintebs
 
! subroutine cprintebs(ebs,fname)
!         type(band),dimension(:) :: ebs
!         character(len=*),optional :: fname
! end subroutine cprintebs

!end interface printebs

interface printebs
 module procedure aprintebs
 module procedure bprintebs
 module procedure cprintebs
 module procedure dprintebs
end interface printebs


 CONTAINS

subroutine aprintebs(ebs,fname)
type(band),dimension(:,:,:) :: ebs
integer :: i,j,k
character(len=*) :: fname

if ( .not. fname == '*' ) then
 open(unit=12,file=trim(fname), status='replace')
  do i=1,size(ebs,1)
   do j=1,size(ebs,2)
    do k=1,size(ebs,3)
     if(ebs(i,j,k)%locu) write(12,'(I6,20(F12.8,1X))') &
     & ebs(i,j,k)%nkpt, ebs(i,j,k)%k,ebs(i,j,k)%e,ebs(i,j,k)%w, &
     ebs(i,j,k)%oc(:)
    end do
   end do
  end do
 close(12)
else
  do i=1,size(ebs,1)
   do j=1,size(ebs,2)
    do k=1,size(ebs,3)
     if(ebs(i,j,k)%locu) write(*,'(I6,20(F12.8,1X))') &
     & ebs(i,j,k)%nkpt,ebs(i,j,k)%k,ebs(i,j,k)%e,ebs(i,j,k)%w, &
     & ebs(i,j,k)%oc(:)
    end do
   end do
  end do
end if

end subroutine aprintebs


subroutine bprintebs(ebs,fname)
type(band),dimension(:,:) :: ebs
integer :: i,j
character(len=*) :: fname

if ( .not. fname == '*' ) then
 open(unit=12,file=trim(fname), status='replace')
  do i=1,size(ebs,1)
   do j=1,size(ebs,2)
    if(ebs(i,j)%locu) write(12,'(I6,20(F12.8,1X))')  &
    & ebs(i,j)%nkpt,ebs(i,j)%k,ebs(i,j)%e,ebs(i,j)%w, &
    & ebs(i,j)%oc(:)
   end do
  end do
 close(12)
else
  do i=1,size(ebs,1)
   do j=1,size(ebs,2)
    if(ebs(i,j)%locu) write(*,'(I6,20(F12.8,1X))')  &
    & ebs(i,j)%nkpt,ebs(i,j)%k,ebs(i,j)%e,ebs(i,j)%w, &
    & ebs(i,j)%oc(:)
   end do
  end do
end if
end subroutine bprintebs

subroutine cprintebs(ebs,fname)
type(band),dimension(:) :: ebs
integer :: i
character(len=*) :: fname

if ( .not. fname == '*' ) then
 open(unit=12,file=trim(fname), status='replace')
   do i=1,size(ebs,1)
     if(ebs(i)%locu) write(12,'(I6,20(F12.8,1X))')  &
    & ebs(i)%nkpt,ebs(i)%k,ebs(i)%e,ebs(i)%w, &
    & ebs(i)%oc(:)
   end do
 close(12)
else
  do i=1,size(ebs,1)
     if(ebs(i)%locu) write(*,'(I6,20(F12.8,1X))')  &
    & ebs(i)%nkpt,ebs(i)%k,ebs(i)%e,ebs(i)%w, &
    & ebs(i)%oc(:)
  end do
end if

end subroutine cprintebs

subroutine dprintebs(ebs,fname)
type(band) :: ebs
character(len=*) :: fname

if ( .not. fname == '*' ) then
 open(unit=12,file=trim(fname), status='replace')
     if(ebs%locu) write(12,'(I6,20(F12.8,1X))')  &
     & ebs%nkpt,ebs%k,ebs%e,ebs%w!,ebs%oc(:)
 close(12)
else
     if(ebs%locu) write(*,'(I6,20(F12.8,1X))')  &
     & ebs%nkpt,ebs%k,ebs%e,ebs%w!,ebs%oc(:)
end if

end subroutine dprintebs

END MODULE ebs_typs
