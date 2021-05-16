MODULE EBS_METHODS
 USE MATH
 USE EBS_TYPS
 USE MYLATTICE
 IMPLICIT NONE
 INTEGER, PARAMETER :: K15=SELECTED_INT_KIND(15)
 REAL(DP),DIMENSION(3,3) :: M_INV
 TYPE( NUMBERS ) :: N
 TYPE( FILENUMBERS ), DIMENSION(:), ALLOCATABLE :: NFILE
 TYPE( PARAMETERS ) :: PAR
 TYPE( BAND ), DIMENSION(:,:,:), ALLOCATABLE :: EBS
 LOGICAL, DIMENSION(:,:), ALLOCATABLE :: LPOS


 CONTAINS


ELEMENTAL  SUBROUTINE ZERO(KBS)
  TYPE( BAND ), INTENT(INOUT) :: KBS
  REAL(DP), DIMENSION(SIZE(KBS%OC)) :: V
   
   V=0.0_DP
   KBS = BAND ( 0,0,KBS%NKPT,0,0.0_DP,0.0_DP,0.0_DP,V,.FALSE.)

END SUBROUTINE ZERO

ELEMENTAL SUBROUTINE SETUP_ORBIT(KBS,D)
 TYPE(BAND),INTENT(INOUT) :: KBS
 INTEGER,INTENT(IN) :: D
 ALLOCATE(KBS%OC(D))
 KBS%OC=0.0_DP
END SUBROUTINE SETUP_ORBIT


INTEGER FUNCTION NITEMS(CHA)
 CHARACTER(LEN=*) :: CHA
 CHARACTER(LEN=LEN_TRIM(CHA)) :: CDUM
 INTEGER :: I,N
 LOGICAL :: NEWWORD

 CDUM=ADJUSTL(TRIM(CHA))
 N =  0
 NEWWORD=.TRUE.
 DO I=1,LEN(CDUM)  
   IF (CDUM(I:I).NE.' ')THEN
     IF (NEWWORD) THEN
       N = N + 1
       NEWWORD=.FALSE.
     END IF
   ELSE
     NEWWORD=.TRUE.
   END IF
 END DO
 NITEMS=N

END FUNCTION NITEMS


SUBROUTINE WRITE_PROGRESS(S,N)
 INTEGER :: I,S,N,PROG,NBAR
    PROG=INT(0.5_DP+REAL(S*100,DP)/REAL(N,DP))
    PROG=MAX(PROG,0)
    PROG=MIN(PROG,100)
    WRITE(*,'(A)',ADVANCE='NO') ACHAR(27)//'[70D'
    WRITE(*,'(I3,A)',ADVANCE='NO') PROG,'%  ['
    NBAR=INT(0.5+REAL(PROG)/2.0)
    DO I=1,NBAR
      WRITE(*,'(A)',ADVANCE='NO') '|'
    END DO
    DO I=1,50-NBAR
      WRITE(*,'(A)',ADVANCE='NO') ' '
    END DO
    WRITE(*,'(A)',ADVANCE='NO') ']'
    IF(S==N)THEN
      WRITE(*,*) ''
      WRITE(*,*) ''
    END IF
  END SUBROUTINE WRITE_PROGRESS


SUBROUTINE CLEAN_DISPLAY_LINE()
  WRITE(*,'(A)',ADVANCE='NO') ACHAR(27)//'[70D'
END SUBROUTINE CLEAN_DISPLAY_LINE



SUBROUTINE READ_INPAR(PATHNAME,LPATH)
 IMPLICIT NONE
 INTEGER :: I,POS,IOST
 REAL(DP) :: RDUM
 CHARACTER(1) :: CDUM
 CHARACTER(50) :: CHARA
 CHARACTER(3), DIMENSION(:), ALLOCATABLE :: PATHNAME
 LOGICAL :: LPATH,LORBIT
 ALLOCATE(PATHNAME(N%PATH+1))
 OPEN(UNIT=11, FILE='tempINPAR', STATUS='OLD', ACTION='READ')
 READ(11,'(A)') CHARA

 POS=SCAN(CHARA,' ')

 IF((POS>0).AND.(POS<LEN_TRIM(CHARA)))THEN
   READ(CHARA(1:POS-1),*) PAR%FBORDER(1)
   READ(CHARA(POS+1:),*,IOSTAT=IOST) PAR%FBORDER(2)
   IF(IOST>0)THEN
      PAR%FBORDER(2)=ABS(PAR%FBORDER(1))
      PAR%FBORDER(1)=-PAR%FBORDER(2)
   END IF
 ELSE
   READ(CHARA,*) RDUM
   PAR%FBORDER(1)=-ABS(RDUM)
   PAR%FBORDER(2)=ABS(RDUM)
 END IF

 IF(PAR%FBORDER(2)<PAR%FBORDER(1))THEN
   RDUM=PAR%FBORDER(1)
   PAR%FBORDER(1)=PAR%FBORDER(2)
   PAR%FBORDER(2)=RDUM
 END IF


 READ(11,'(A)') CHARA
 POS=SCAN(CHARA,' ')
 IF((POS>0).AND.(POS<LEN_TRIM(CHARA)))THEN
   READ(CHARA(1:POS-1),*) PAR%WDIF(1)
   READ(CHARA(POS+1:),*,IOSTAT=IOST) PAR%WDIF(2)
   IF(IOST>0)THEN
      PAR%WDIF(2)=ABS(PAR%WDIF(1))
      PAR%WDIF(1)=-PAR%WDIF(2)
   END IF
 ELSE
   READ(CHARA,*) RDUM
   PAR%WDIF(1)=-ABS(RDUM)
   PAR%WDIF(2)=ABS(RDUM)
 END IF
 IF(PAR%WDIF(2)<PAR%WDIF(1))THEN
   RDUM=PAR%WDIF(1)
   PAR%WDIF(1)=PAR%WDIF(2)
   PAR%WDIF(2)=RDUM
 END IF


  READ(11,*) PAR%EDIF
  READ(11,*) PAR%LBAV
  READ(11,*) PAR%LOAV
  READ(11,*) PAR%EGAP
  READ(11,*) PAR%TRASH
  READ(11,*) PAR%DBLOCH
  READ(11,*) CHARA
  IF((SCAN(CHARA,'t')+SCAN(CHARA,'T'))>0)THEN
    PAR%BAND=.TRUE.
  ELSE
    PAR%BAND=.FALSE.
  END IF
  READ(11,*) CHARA
  IF((SCAN(CHARA,'t')+SCAN(CHARA,'T'))>0)THEN
    PAR%NORM=.TRUE.
  ELSE
    PAR%NORM=.FALSE.
  END IF
  READ(11,*) PAR%DGRAD
  READ(11,*) PAR%POINTS
  READ(11,'(A)') CHARA
  IF((SCAN(CHARA,'t')+SCAN(CHARA,'T'))>0)THEN
    PAR%POLY=.TRUE.
  ELSE
    PAR%POLY=.FALSE.
  END IF
  READ(11,*) PAR%EPS
  READ(11,*) PAR%FORB
  READ(11,*) PAR%ODISTINCT
  READ(11,'(A)') CHARA
  IF((SCAN(CHARA,'t')+SCAN(CHARA,'T'))>0)THEN
    PAR%SYMREC=.TRUE.
  ELSE
    PAR%SYMREC=.FALSE.
  END IF
  READ(11,'(A)') CHARA
  IF(CHARA(1:1)=='#')THEN
    PAR%IREFLECT=1
    READ(11,'(A)') CHARA
    IF(CHARA(1:1)/='#')THEN
      READ(CHARA,*)PAR%SYMPOINTS(1,:)
      PAR%IREFLECT=4
    END IF
 ELSE
    READ(CHARA,*)PAR%SYMPOINTS(1,:)
    PAR%IREFLECT=4
    READ(11,'(A)') CHARA
    IF(CHARA(1:1)/='#')THEN
      READ(CHARA,*)PAR%SYMPOINTS(2,:)
      PAR%IREFLECT=2
    END IF
  END IF
  READ(11,'(A)') CHARA
  IF((SCAN(CHARA,'t')+SCAN(CHARA,'T'))>0)THEN
    PAR%BANDPLOT=.TRUE.
  ELSE
    PAR%BANDPLOT=.FALSE.
  END IF
  READ(11,*) PAR%SIGMA
  READ(11,'(A)') CHARA
  IF((SCAN(CHARA,'t')+SCAN(CHARA,'T'))>0)THEN
    PAR%SPECFUN=.TRUE.
  ELSE
    PAR%SPECFUN=.FALSE.
  END IF
  READ(11,*) PAR%SPECDELTA
  READ(11,'(A)') CHARA
  IF((SCAN(CHARA,'t')+SCAN(CHARA,'T'))>0)THEN
    PAR%SLIMSPEC=.TRUE.
  ELSE
    PAR%SLIMSPEC=.FALSE.
  END IF
  READ(11,*) PAR%FGRID


  READ(11,'(A)') CHARA
  IF(SCAN(CHARA,'-')>0)THEN
    POS=INDEX(CHARA,'-')
    READ(CHARA(:POS-1),'(I10)') PAR%SION(1)
    READ(CHARA(POS+1:),'(I10)') PAR%SION(2)
    IF(PAR%SION(2)>0)THEN
      PAR%NSION=1+PAR%SION(2)-PAR%SION(1)
    ELSE
      PAR%NSION=1
    END IF
  ELSE
    PAR%SION(2)=0
    PAR%NSION=1
    READ(CHARA,'(I10)') PAR%SION(1)
    IF(PAR%SION(1)<0)THEN
      PAR%SION(1)=0
    END IF
  END IF


  READ(11,'(A)') CHARA
  IF(SCAN(CHARA,'-')>0)THEN
    POS=INDEX(CHARA,'-')
    READ(CHARA(:POS-1),'(I10)') PAR%KSKIP(1)
    READ(CHARA(POS+1:),'(I10)') PAR%KSKIP(2)
    WRITE(*,'(A,I4,A,I4)') 'Skipping k-point ',PAR%KSKIP(1),' to k-point ',PAR%KSKIP(2)
  ELSE
    PAR%KSKIP(1)=0
    READ(CHARA,'(I10)') PAR%KSKIP(2)
    IF(PAR%KSKIP(2)>0) WRITE(*,'(A,I4,A)') 'Skipping the first ',PAR%KSKIP(2),' k-points'
  END IF
  READ(11,'(A)') CHARA
  IF((SCAN(CHARA,'t')+SCAN(CHARA,'T'))>0)THEN
    PAR%ROOTSCALC=.TRUE.
  ELSE
    PAR%ROOTSCALC=.FALSE.
  END IF

 
   READ(11,*)
   READ(11,'(A1)') CHARA
  IF((SCAN(CHARA,'t')+SCAN(CHARA,'T'))>0)THEN
    PAR%MULTIFERMI=.TRUE.
  ELSE
    PAR%MULTIFERMI=.FALSE.
  END IF
 
  CLOSE(UNIT=11)


 IF(PAR%FBORDER(1)>PAR%WDIF(1))THEN
   WRITE(*,*) 'WARNING: The lower energy bound for the raw data is'
   WRITE(*,*) '         less than for the manipulated one'
   WRITE(*,'(A,f9.5)') '  ==>    set both intervals to:',PAR%FBORDER(1)
   PAR%WDIF(1)=PAR%FBORDER(1)
 END IF

 IF(PAR%FBORDER(2)<PAR%WDIF(2))THEN
   WRITE(*,*) 'WARNING: The uper energy bound for the raw data is'
   WRITE(*,*) '         less than for the manipulated one'
   WRITE(*,'(A,f9.5)') '  ==>    set both intervals to:',PAR%FBORDER(2)
   PAR%WDIF(2)=PAR%FBORDER(2)
 END IF

 OPEN(UNIT=11, FILE='tempINPAR', STATUS='OLD',POSITION='APPEND', ACTION='WRITE')

 IF(PAR%WDIF(1)*PAR%WDIF(2)>=0.0_DP)THEN
   PAR%ROOTSCALC=.FALSE.
   PAR%LSURFACE=.FALSE.
   WRITE(11,'(I1)') 0
 ELSE
   WRITE(11,'(I1)') 1
 END IF

 CLOSE(11)


END SUBROUTINE READ_INPAR



!*************************** Read all filenames ********************************
SUBROUTINE GET_FILENAMES(FNAMEDIR,FILEDIR,PLOTFILE,BANDSPEC,OUTGRID,FERMIFILE,FITTPOINTS, &
            & ACOLBAND,FSUR,FSURSPEC,STATFILE,LATTFILE,SPINFILE, ORBITSTATS )
  CHARACTER(LEN=*) :: FNAMEDIR,FILEDIR,PLOTFILE,BANDSPEC,OUTGRID,FERMIFILE,FITTPOINTS
  CHARACTER(LEN=*) :: ACOLBAND,FSUR,FSURSPEC,STATFILE,LATTFILE,SPINFILE,ORBITSTATS
  CHARACTER(LEN=50) :: DATADIR
  INTEGER :: I,UN=34


  OPEN(UNIT=UN,FILE=TRIM(FNAMEDIR),STATUS='OLD',ACTION='READ')
    READ(UN,'(A)') FILEDIR
    READ(UN,*)
    READ(UN,'(A)') PLOTFILE
    READ(UN,'(A)') BANDSPEC
    READ(UN,'(A)') OUTGRID
    READ(UN,'(A)') FERMIFILE
    READ(UN,'(A)') FITTPOINTS
    READ(UN,'(A)') LATTFILE
    DO I=1,8
      READ(UN,*)
    END DO
    READ(UN,'(A)') ACOLBAND
    READ(UN,*)
    READ(UN,*)
    READ(UN,'(A)') FSUR
    READ(UN,'(A)') FSURSPEC
    READ(UN,'(A)') SPINFILE
    READ(UN,'(A)') DATADIR
    READ(UN,*)
    READ(UN,*)
    READ(UN,'(A)') STATFILE
    READ(UN,'(A)') ORBITSTATS


  CLOSE(UN)

  PLOTFILE=TRIM(DATADIR)//TRIM(PLOTFILE)
  BANDSPEC=TRIM(DATADIR)//TRIM(BANDSPEC)
  OUTGRID=TRIM(DATADIR)//TRIM(OUTGRID)
  FITTPOINTS=TRIM(DATADIR)//TRIM(FITTPOINTS)
  ACOLBAND=TRIM(DATADIR)//'bandindexplot/'//TRIM(ACOLBAND)
  FSUR=TRIM(DATADIR)//TRIM(FSUR)
  FSURSPEC=TRIM(DATADIR)//TRIM(FSURSPEC)
  STATFILE=TRIM(DATADIR)//TRIM(STATFILE)
  ORBITSTATS=TRIM(DATADIR)//TRIM(ORBITSTATS)
  LATTFILE=TRIM(DATADIR)//TRIM(LATTFILE)
  SPINFILE=TRIM(DATADIR)//TRIM(SPINFILE)

END SUBROUTINE GET_FILENAMES



! Write vector according to a new basis
!***********************************************************************

SUBROUTINE CHANGE_BASIS(M,A,X)
  INTEGER :: I,R
  REAL(DP), DIMENSION(3)     :: A,X
  REAL(DP), DIMENSION(3,3)   :: M

  DO R=1,3
    X(R)=0.d0
  END DO

  DO R=1,3
    DO I=1,3
        X(R)=X(R)+M(R,I)*A(I)
    END DO
  END DO

  RETURN
END SUBROUTINE CHANGE_BASIS


! Calculate distance beetween k points
!***********************************************************************

REAL(KIND=DP) FUNCTION KDISTANCE(K,K0,BASE)
  IMPLICIT NONE
  REAL(KIND=DP), DIMENSION(3) :: K,K0,KC,K0C
  REAL(KIND=DP), DIMENSION(3,3) :: BASE

!Cartesian conversion
  KC(1)=K(1)*BASE(1,1) + K(2)*BASE(1,2) + K(3)*BASE(1,3)
  KC(2)=K(1)*BASE(2,1) + K(2)*BASE(2,2) + K(3)*BASE(2,3)
  KC(3)=K(1)*BASE(3,1) + K(2)*BASE(3,2) + K(3)*BASE(3,3)
  K0C(1)=K0(1)*BASE(1,1) + K0(2)*BASE(1,2) + K0(3)*BASE(1,3)
  K0C(2)=K0(1)*BASE(2,1) + K0(2)*BASE(2,2) + K0(3)*BASE(2,3)
  K0C(3)=K0(1)*BASE(3,1) + K0(2)*BASE(3,2) + K0(3)*BASE(3,3)

!Distance calculation
  KDISTANCE=SQRT((KC(1)-K0C(1))**2+(KC(2)-K0C(2))**2+(KC(3)-K0C(3))**2)
  RETURN

END FUNCTION KDISTANCE



SUBROUTINE READ_LATTICE(FILEDIR,LATTICE,LUNFOLD,LATTFILE,LREAD)
 IMPLICIT NONE
 TYPE (LATT) :: LATTICE
 INTEGER :: I,J,NSCALE,SCALEX,SCALEY,SCALEZ,IO_ERROR,LPOS,LREAD
 CHARACTER(LEN=*) :: FILEDIR,LATTFILE
 CHARACTER(255) :: INPLIN
 CHARACTER(LEN=1000) :: DATANAME
 REAL(KIND=DP), DIMENSION(3,3) :: TLATT
 LOGICAL :: LUNFOLD

 OPEN(UNIT=44,FILE=TRIM(FILEDIR), STATUS='OLD', ACTION='READ')
  READ(44,*)
  READ(44,*) LPOS
  READ(44,*)
  READ(44,'(A)') DATANAME
 CLOSE(44)
 OPEN(UNIT=11, FILE=TRIM(DATANAME), STATUS='OLD', ACTION='READ', IOSTAT=IO_ERROR)
  IF (IO_ERROR /= 0) THEN
    WRITE(*,*) "WARNING: Can not open ",TRIM(DATANAME) , &
    & " file. Error Nr.:", IO_ERROR
    WRITE(*,*) "set real space basis vectors to unity"
    DO I=1,3
      DO J=1,3
        IF (I == J) THEN
          LATTICE%A(I,J)=1.0_DP
        ELSE
          LATTICE%A(I,J)=0.0_DP
        END IF
      END DO
    END DO
  ELSE
    READ(11,*)
    !POSCAR file
    IF((LPOS==1).AND.(LREAD==0))THEN
      READ(11,'(A)') INPLIN
      NSCALE=NITEMS(INPLIN)
      IF (NSCALE==3)THEN
        LATTICE%SCALE=1
        READ(INPLIN,*) SCALEX,SCALEY,SCALEZ
      ELSE
        READ(INPLIN,*) LATTICE%SCALE
        SCALEX=1
        SCALEY=1
        SCALEZ=1
      END IF
      DO I=1,3
        READ(11,*) LATTICE%A(1,I),LATTICE%A(2,I),LATTICE%A(3,I)
      END DO
      IF (LATTICE%SCALE<0.0_DP) THEN
        CALL LATTIC(LATTICE)
        LATTICE%SCALE=(ABS(LATTICE%SCALE)  &
        &        / ABS(LATTICE%OMEGA))**(1.0_DP/3.0_DP)
      ENDIF

      LATTICE%A(1,:) =LATTICE%A(1,:)*SCALEX*LATTICE%SCALE
      LATTICE%A(2,:) =LATTICE%A(2,:)*SCALEY*LATTICE%SCALE
      LATTICE%A(3,:) =LATTICE%A(3,:)*SCALEZ*LATTICE%SCALE

    !No POSCAR file
    ELSE
      DO I=1,3
        READ(11,*) TLATT(1,I),TLATT(2,I),TLATT(3,I)
      END DO
      IF(LUNFOLD)THEN
        LATTICE%B=TLATT
        CALL RLATTIC(LATTICE)
      ELSE
        LATTICE%A=TLATT
      END IF
    END IF
  END IF
  CALL LATTIC(LATTICE)
  IF (LATTICE%OMEGA<0) THEN
    WRITE(*,*)'ERROR: the triple product of the basis vectors ', &
    &     'is negative exchange two basis vectors'
    STOP 1
  ENDIF

 CLOSE(11)

 IF(LREAD==0)THEN
 OPEN(UNIT=12, FILE=TRIM(LATTFILE), STATUS='REPLACE', ACTION='WRITE')

    WRITE(12,'(A)') '#Lattice of the structure'
    IF(LUNFOLD)THEN
      DO I=1,3
         WRITE(12,'(3(F20.14))') LATTICE%B(1,I),LATTICE%B(2,I),LATTICE%B(3,I)
      END DO
    ELSE
      DO I=1,3
         WRITE(12,'(3(F20.14))') LATTICE%A(1,I),LATTICE%A(2,I),LATTICE%A(3,I)
      END DO
    END IF

 CLOSE(12)
 END IF
END SUBROUTINE READ_LATTICE





!================================================================ 
!===========================  PROCAR ============================ 
!================================================================ 

SUBROUTINE READHEAD_PROCAR_PRIM(FILEDIR)
 IMPLICIT NONE
 INTEGER :: IO_ERROR, LET, I, K, KMIN, SPN, NFKPTS
 CHARACTER(200) :: DATANAME
 CHARACTER(300) :: CDUM
 CHARACTER(10), DIMENSION(100) :: ORBNAMES
 CHARACTER(LEN=*) :: FILEDIR
 N%TKPTS=0
 N%TCOL=0
 SPN=1
 ALLOCATE(NFILE(N%PATH))
 NFILE(:)%ORBIT=0
 NFILE(:)%OLDPRO=.FALSE.
 OPEN(UNIT=1000, FILE=TRIM(FILEDIR), STATUS='OLD', ACTION='READ') 
  READ(1000,*)
  READ(1000,*)
  READ(1000,*)
  READ(1000,*)
  DO K=1,N%PATH
    READ(1000,'(A)') DATANAME
    OPEN(UNIT=11, FILE=TRIM(DATANAME), STATUS='OLD', ACTION='READ', IOSTAT=IO_ERROR)
     IF (IO_ERROR /= 0) THEN
       WRITE(*,*) "Can not open ",TRIM(DATANAME) , &
       &        " file. Error Nr.:", IO_ERROR
       WRITE(*,*) "STOPED program"
       STOP 1
     END IF
       DO 
         READ(11,'(A)', IOSTAT=IO_ERROR) CDUM
         IF(SPN==2)STOP
         IF (IO_ERROR /= 0) THEN
             WRITE(*,*) "Can not read ",TRIM(DATANAME) , &
             &        " file. Error Nr.:", IO_ERROR
             WRITE(*,*) "STOPED program"
             STOP 1
         END IF
         IF(CDUM(1:14)=='# of k-points:')EXIT
       END DO
       READ(CDUM,1,IOSTAT=IO_ERROR) NFILE(K)%KPTS, NFILE(K)%BND, NFILE(K)%IONS     !Read #kpoints,#bands,#ions
       1  FORMAT(14x, I5,9x, 11x, I4,9x,10x, I4)
       IF(IO_ERROR/=0)THEN
         READ(CDUM,2,IOSTAT=IO_ERROR) NFILE(K)%KPTS, NFILE(K)%BND, NFILE(K)%IONS     !Read #kpoints,#bands,#ions
         2  FORMAT(14x, I5,9x, 11x, I5,9x,10x, I5)
         IF(IO_ERROR/=0)THEN
          WRITE(*,*)'ERROR: PROCAR structure of ',TRIM(DATANAME),' unknown!'
          WRITE(*,*) "STOPED program"
          STOP 1
         END IF
       ELSE
         NFILE(K)%OLDPRO=.TRUE.
       END IF
       NFILE(K)%NK=NFILE(K)%KPTS-MIN(PAR%KSKIP(2),NFILE(K)%KPTS)+MAX(PAR%KSKIP(1),0)
       N%TKPTS=N%TKPTS+NFILE(K)%NK
       N%TCOL=N%TCOL+(NFILE(K)%NK*NFILE(K)%BND)
       NFILE(K)%IKPTS=N%TKPTS
       DO I=1,5  !go to line 7/8/9
          READ(11,*)
       END DO

       !read numbers of orbitals
       READ(11,'(A)') CDUM
       I = 4
       LET = -1
       DO WHILE( CDUM(I:I+2).NE.'tot')
  
         IF (CDUM(I:I).NE.' ') THEN
           LET = LET + 1
         ELSE IF ((CDUM(I:I).EQ.' ').AND.(LET/=-1)) THEN
           NFILE(K)%ORBIT = NFILE(K)%ORBIT + 1
           ORBNAMES(NFILE(K)%ORBIT)=TRIM(ADJUSTL(CDUM((I-1-LET):(I-1))))
           LET=-1
         END IF
         I=I+1
       END DO
       ORBNAMES(NFILE(K)%ORBIT+1)='tot'
       IF (K>1) THEN
         IF ( NFILE(K)%ORBIT > MINVAL(NFILE(:K-1)%ORBIT)) THEN
           WRITE(*,*) "WARNING: Number of orbitals in ", &
           &        TRIM(DATANAME)," differs"
           WRITE(*,*) "Number of orbitals will set to ", MINVAL(NFILE(:K-1)%ORBIT)
         ELSE
           KMIN=K
         END IF
       ELSE
         KMIN=K
       END IF
    CLOSE(11)
  END DO
 CLOSE(1000)
 N%ORB=MINVAL(NFILE(:)%ORBIT)
 N%ORB=N%ORB+1
 IF(PAR%FORB>N%ORB)THEN
   WRITE(*,'(A,I4,A)') "WARNING: Number of orbital in INPAR ", &
   & PAR%FORB, " is higher"
   WRITE(*,'(A)') "than the occuring orbitals in PROCAR"
   WRITE(*,'(A)') "Set to all orbitals => PLOTORB=0 "
   PAR%FORB=0
 END IF
 IF(PAR%FORB<0)PAR%FORB=N%ORB
 N%BAND=MAXVAL(NFILE(:)%BND)


 ALLOCATE(PAR%ORBNAMES(N%ORB))
 FORALL(I=1:N%ORB) PAR%ORBNAMES(I)=TRIM(ORBNAMES(I))
END SUBROUTINE READHEAD_PROCAR_PRIM






SUBROUTINE READBODY_PROCAR_PRIM(FILEDIR,TEBS,KGRID,LATTICE,LUNFOLD,LFINISH)
 IMPLICIT NONE
 TYPE( BAND ), DIMENSION(:), ALLOCATABLE :: TEBS
 TYPE( LATT ) :: LATTICE
 TYPE( GRID ), DIMENSION(:), ALLOCATABLE :: KGRID
 REAL(KIND=DP) ::  DIST,TOTDIST,MAXDIST,ENERGY,BLOCH,DELTA,TOT,STOT,SORBIT
 REAL(KIND=DP), DIMENSION(3) :: VKPT,KSCF0,  pvec
 REAL(KIND=DP), DIMENSION(:), ALLOCATABLE :: ORBITS
 INTEGER ::  I, IOCHECK,NKPT, J, IJUMP, PROG, UNARRAY,NBAND
 INTEGER :: T1,T2,T3,UN,ND,K,B,P,COL,NION,IND, BND,SPN
 INTEGER,DIMENSION(3) :: T
 CHARACTER(LEN=*) :: FILEDIR
 CHARACTER(16) :: JUNK
 CHARACTER(20) :: TARRAY
 CHARACTER(200) :: DATANAME,FENERGYFILE
 CHARACTER(100) :: CDUM
 CHARACTER(600) :: SDUM
 LOGICAL :: NEWPATH, LUNFOLD, LFINISH
 TOTDIST=0.0_DP
 MAXDIST=0.0_DP
 DIST=0.0_DP
 LFINISH=.FALSE.
 UNARRAY=9924
 TARRAY='tempprocararray.temp'
 PAR%SPN=1
 IF(PAR%FORB==0)THEN
   WRITE(*,'(A,A)',ADVANCE='NO') 'Selected all orbitals :: ',TRIM(PAR%ORBNAMES(1))
   DO I=2,N%ORB-1
     WRITE(*,'(A,A)',ADVANCE='NO') ', ',TRIM(PAR%ORBNAMES(I))
   END DO
   WRITE(*,*) ''
 ELSE IF(PAR%FORB==N%ORB)THEN
   WRITE(*,'(A,A)') 'Selected the total amount of all orbitals:'
   WRITE(*,'(A,A)',ADVANCE='NO') '=> ',TRIM(PAR%ORBNAMES(1))
   DO I=2,N%ORB-1
     WRITE(*,'(A,A)',ADVANCE='NO') ' + ',TRIM(PAR%ORBNAMES(I))
   END DO
   WRITE(*,*)
 ELSE
   WRITE(*,'(A,A)') 'Selected orbital :: ',TRIM(PAR%ORBNAMES(PAR%FORB))
 END IF
 IF((PAR%SION(1)>0).AND.(ALL(PAR%SION(1)<=NFILE(:)%IONS)))THEN
   IF(PAR%SION(2)>0)THEN
     IF(ALL(PAR%SION(2)<=NFILE(:)%IONS))THEN
       WRITE(*,'(A,I5,A,I5)') 'Selected IONs ::',PAR%SION(1),'-',PAR%SION(2)
     ELSE
       WRITE(*,'(A)') 'WARNING: The upper ION range in INPAR file is to high.'
       WRITE(*,'(A,I5)') '         Not all/None of the files include ION ::',PAR%SION(2)
       PAR%SION(2)=MINVAL(NFILE(:)%IONS)
       WRITE(*,'(A,I5)') '       Set the uper ION range to ',PAR%SION(2)
       PAR%NSION=1+PAR%SION(2)-PAR%SION(1)
     END IF
   ELSE
      WRITE(*,'(A,I5)') 'Selected ION ::',PAR%SION(1)
   END IF
 END IF
 WRITE(*,*) ''
 ALLOCATE(KGRID(N%PATH),ORBITS(N%ORB))
 K=0
 COL=1
 WRITE(*,*) ''
 WRITE(*,'(A)') '                 reading PROCAR[.prim] file(s)'
 OPEN(UNIT=UNARRAY, FILE=TARRAY, STATUS='REPLACE', ACTION='WRITE' )
 OPEN(UNIT=9999, FILE=TRIM(FILEDIR), STATUS='OLD', ACTION='READ')
  READ(9999,*)
  READ(9999,*)
  READ(9999,'(A)') FENERGYFILE
  READ(9999,*)
  IF(PAR%MULTIFERMI) OPEN(UNIT=9998, FILE=TRIM(FENERGYFILE), STATUS='OLD',ACTION='READ')
  DO P=1,N%PATH
    CALL WRITE_PROGRESS(P,N%PATH)
    NEWPATH=.TRUE.
    NFILE(P)%SPIN=1
    IF(PAR%MULTIFERMI) READ(9998,*) PAR%EFERMI
    READ(9999,'(A)') DATANAME
    UN=30+P
    OPEN(UNIT=UN, FILE=TRIM(DATANAME), STATUS='OLD',  ACTION='READ')
     spin: DO SPN=1,2
       DO I=1,30
         READ(UN,'(A)',IOSTAT=IOCHECK) CDUM
         IF(IOCHECK/=0)EXIT spin
         IF(CDUM(1:14)=='# of k-points:')EXIT
       END DO
       IF(I>30)THEN
         WRITE(*,*)'ERROR2: PROCAR structure of ',TRIM(DATANAME),' unknown!'
         STOP 1
       END IF
       IF(SPN==2)THEN
         K=K-NFILE(P)%NK
         PAR%SPN=2
         NFILE(P)%SPIN=2
       END IF
       IF(NFILE(P)%IONS==1)THEN
         IJUMP=1
       ELSE
         IJUMP=2
       END IF
       KPOINTS: DO B=1,NFILE(P)%KPTS
         K=K+1
         DO ND=1,4*NFILE(P)%IONS
           READ(UN,'(A)') CDUM
           IF(CDUM(2:2)=='k')EXIT
         END DO
         IF(NFILE(P)%OLDPRO)THEN
           READ(CDUM,11,IOSTAT=IOCHECK) NKPT, (VKPT(ND), ND=1,3)
           11  FORMAT(9X,I4,5X,3F11.8)
         ELSE
           READ(CDUM,112,IOSTAT=IOCHECK) NKPT, (VKPT(ND), ND=1,3)
           112  FORMAT(9X,I5,5X,3F11.8)
         END IF
         IF (IOCHECK/=0) EXIT


         IF((NKPT>=PAR%KSKIP(1)).AND.(NKPT<=PAR%KSKIP(2)))THEN
           DO I=1,NFILE(P)%BND
            DO ND=1,4*NFILE(P)%IONS
             READ(UN,'(A100)') CDUM
             IF(CDUM(1:5)=='band')EXIT
            END DO
            DO ND=1,NFILE(P)%IONS+IJUMP+2
             READ(UN,*)
            END DO
           END DO
           K=K-1
           CYCLE KPOINTS
         END IF
         IF (NEWPATH) THEN
           KSCF0=VKPT
           IF ( P > 1 ) THEN
             TOTDIST = MAXDIST
             KGRID(P-1)%DIST=TOTDIST
           END IF
           KGRID(P)%A=VKPT
           NEWPATH=.FALSE.
         ELSE
           DIST=TOTDIST+KDISTANCE(VKPT,KSCF0,LATTICE%B)
           IF(DIST>MAXDIST)MAXDIST=DIST
         END IF
         bnd: DO I=1,NFILE(P)%BND

           DO ND=1,4*NFILE(P)%IONS
             READ(UN,'(A100)') CDUM
             IF(CDUM(1:5)=='band')EXIT
           END DO

           IF (LUNFOLD) THEN
             IF(NFILE(P)%OLDPRO)THEN
               READ(CDUM,12) NBAND,ENERGY,BLOCH
               12 FORMAT(5X,I3,9X,F14.8,34X,E15.7)
             ELSE
               READ(CDUM,122) NBAND,ENERGY,BLOCH
               122 FORMAT(5X,I5,9X,F14.8,34X,E15.7)
             END IF
           ELSE
             IF(NFILE(P)%OLDPRO)THEN
               READ(CDUM,13) NBAND,ENERGY
               13 FORMAT(5X,I3,9X,F14.8)
             ELSE
               READ(CDUM,132) NBAND,ENERGY
               132 FORMAT(5X,I5,9X,F14.8)
            END IF
            BLOCH=1.0_DP
           END IF
           ENERGY = ENERGY-PAR%EFERMI

          
          IF((ENERGY<0.0_DP).AND.(ENERGY>N%ENMAX(SPN)))THEN
             N%ENMAX(SPN)=ENERGY
             N%LENMAX(SPN)=.TRUE.
          ELSE IF ((ENERGY>=0.0_DP).AND.(ENERGY<N%EPMIN(SPN)))THEN
             N%EPMIN(SPN)=ENERGY
             N%LEPMIN(SPN)=.TRUE.
          END IF
          
          IF ((ENERGY>=1.2_DP*PAR%FBORDER(1)).AND.(ENERGY<=1.2_DP*PAR%FBORDER(2).AND.(BLOCH>PAR%TRASH)))THEN
         
                 
         IF((PAR%SION(1)>0).AND.(PAR%SION(1)<=NFILE(P)%IONS)) THEN
          
           DO J=1,IJUMP
              READ(UN,*)
            END DO
            DO J=1,PAR%SION(1)-1
              READ(UN,*)
            END DO

            TOT=0.0_DP
            ORBITS(:)=0.0_DP

            DO J=1,PAR%NSION

              IF(NFILE(P)%OLDPRO)THEN
                READ(UN,'(I4)',ADVANCE='NO') NION
              ELSE
                READ(UN,'(I6)',ADVANCE='NO') NION
              END IF
              DO ND=1,NFILE(P)%ORBIT
                IF(ND>N%ORB)THEN
                  READ(UN,'(A7)',ADVANCE='NO') JUNK
                ELSE
                  READ(UN,'(F6.3,1X)',ADVANCE='NO') SORBIT  !read orbital characters
                  ORBITS(ND)=ORBITS(ND)+SORBIT
                END IF
              END DO
              READ(UN,'(F6.3)') STOT
              TOT=TOT+STOT
              ORBITS(NFILE(P)%ORBIT+1)=TOT

            END DO

            DO J=1,NFILE(P)%IONS
                READ(UN,'(A6)') JUNK
                IF(SCAN(JUNK,'tot')>0)EXIT
            END DO
            READ(UN,*)
          ELSE
           DO J=1,NFILE(P)%IONS+IJUMP !Ignore the datas for the single ions
             READ(UN,*)
           END DO

           IF(NFILE(P)%OLDPRO)THEN
             READ(UN,'(A4)',ADVANCE='NO') JUNK
           ELSE
             READ(UN,'(A6)',ADVANCE='NO') JUNK
           END IF
           DO ND=1,NFILE(P)%ORBIT
             IF(ND>N%ORB)THEN
               READ(UN,'(A7)',ADVANCE='NO') JUNK
             ELSE
               READ(UN,'(F6.3,1X)',ADVANCE='NO') ORBITS(ND)  !read orbital characters
             END IF
           END DO
           READ(UN,'(F6.3)') TOT
           READ(UN,*)
           ORBITS(NFILE(P)%ORBIT+1)=TOT
           !Normalisation of orbital characters
           DO J=1,N%ORB
             ORBITS(J)=ORBITS(J)/TOT
           END DO
          END IF
           WRITE(UNARRAY,332) SPN,P,K,NBAND,DIST,ENERGY,BLOCH,ORBITS(:)

           COL=COL+1
           IF(COL-1>400000)THEN
             PAR%FBORDER(1)=PAR%FBORDER(1)/2.0_DP
             PAR%FBORDER(2)=PAR%FBORDER(2)/2.0_DP
             CALL CLEAN_DISPLAY_LINE()
             WRITE(*,*) 'WARNING: There are to much energy states.'
             WRITE(*,'(A,2(F9.4," - "))') 'Set the energy interval to: ',PAR%FBORDER
             DEALLOCATE(KGRID)
             CLOSE(UN)
             CLOSE(UNARRAY)
             CLOSE(9999)
             IF(PAR%MULTIFERMI) CLOSE(9998)
             RETURN
           END IF
         ELSE
           DO ND=1,NFILE(P)%IONS+IJUMP+2
             READ(UN,*)
           END DO
         END IF
       END DO bnd
     END DO KPOINTS
     END DO spin
    CLOSE(UN)
    KGRID(P)%B=VKPT
  END DO
 CLOSE(9999)
 IF(PAR%MULTIFERMI) CLOSE(9998)
 CLOSE(UNARRAY)
 KGRID(N%PATH)%DIST=MAXDIST
 COL=COL-1
 N%TCOL=COL
 ALLOCATE(TEBS(N%TCOL))
 CALL SETUP_ORBIT(TEBS,N%ORB)
 CALL ZERO(TEBS)

 OPEN(UNIT=UNARRAY, FILE=TARRAY, STATUS='OLD', ACTION='READ' )

   COL=1
   IOCHECK = 0
   DO WHILE ( IOCHECK == 0 )

     READ(UNARRAY,'(A)',IOSTAT=IOCHECK) SDUM
     IF( IOCHECK /= 0 ) EXIT
     READ(SDUM,332) TEBS(COL)%SPN,TEBS(COL)%PATH,TEBS(COL)%NKPT,&
     & TEBS(COL)%BND,TEBS(COL)%K,TEBS(COL)%E,TEBS(COL)%W,TEBS(COL)%OC(:)
     TEBS(COL)%LOCU=.TRUE.
     COL=COL+1
   END DO

 CLOSE(UNARRAY)


 LFINISH=.TRUE.

 332 FORMAT(I1,1X,I5,1X,I8,1X,I4,1X,40F24.16)

END SUBROUTINE READBODY_PROCAR_PRIM






!====================================================================== 
!=============================   PRJCAR  ============================== 
!====================================================================== 

SUBROUTINE READHEAD_PRJCAR(FILEDIR)
 INTEGER ::  I,ISP,IOCHECK,NKPT,UN,P,BND
 CHARACTER(LEN=*) :: FILEDIR
 CHARACTER(200) :: DATANAME
 CHARACTER(300) :: CDUM
 CHARACTER(80) :: SCDUM
 CHARACTER(2) :: B1,B2,B3
 CHARACTER(1) :: A
 LOGICAL :: LEXIST,LDUM

 PAR%SPN=1
 ALLOCATE(NFILE(N%PATH))
 NFILE(:)%ORBIT=0
 NFILE(:)%IKPTS=0
 OPEN(UNIT=1000, FILE=TRIM(FILEDIR), STATUS='OLD', ACTION='READ')
  READ(1000,*)
  READ(1000,*)
  READ(1000,*)
  READ(1000,*)

  path: DO P=1,N%PATH
    NFILE(P)%SLIMPRJ=.FALSE.
    READ(1000,'(A)') DATANAME
    UN=30+P
    OPEN(UNIT=UN, FILE=TRIM(DATANAME), STATUS='OLD',  ACTION='READ')
     DO I=1,60
       READ(UN,'(A)') SCDUM
       READ(SCDUM,'(40X,A1,I6)',IOSTAT=IOCHECK) A,NKPT
       IF((IOCHECK==0).AND.(A==':').AND.(NKPT>0)) EXIT
     END DO
     IF(IOCHECK/=0)THEN
       WRITE(*,*)'ERROR: PRJCAR structure of ', TRIM(DATANAME), &
       & ' unknown:',IOCHECK
       STOP 1
     END IF
     DO I=1,20
       READ(UN,'(A)') CDUM
       SCDUM=ADJUSTL(CDUM)
       IF(SCDUM(1:5)=='NKPTS') SCDUM=ADJUSTL(SCDUM(6:))
       IF((SCDUM(1:3)=='KPT').OR.(SCDUM(1:3)=='kpt')) SCDUM=ADJUSTL(SCDUM(4:))
       B1=SCDUM(1:2)
       SCDUM=ADJUSTL(SCDUM(3:))
       B2=SCDUM(1:2)
       SCDUM=ADJUSTL(SCDUM(3:))
       B3=SCDUM(1:2)
       IF((B1=='b1').AND.(B2=='b2').AND.(B3=='b3')) EXIT
     END DO
     NFILE(P)%KPTS=NKPT
  
     DO I=1,NFILE(P)%KPTS
       READ(UN,*)
     END DO
     
     NFILE(P)%BND=1
     spin: DO
       READ(UN,'(A)') CDUM
       IF(CDUM(1:15)/='spin component:')CYCLE
       READ(CDUM,'(15X,I4)') ISP
       IF(ISP>1)PAR%SPN=2
       READ(UN,'(A)') CDUM
       kpoint: DO
         IF(CDUM(1:7)=='k-point')THEN
           READ(CDUM,'(33X,I6)') NKPT
           band: DO
             READ(UN,'(A)',IOSTAT=IOCHECK) CDUM
             IF(IOCHECK/=0)EXIT spin
             IF(CDUM(1:5)=='band:')THEN
               READ(CDUM,'(5X,I6)') BND
               IF (BND>NFILE(P)%BND) NFILE(P)%BND=BND
             ELSE IF (CDUM(1:7)=='k-point') THEN
               EXIT band
             ELSE IF (CDUM(1:15)=='spin component:')THEN
               READ(CDUM,'(15X,I4)') ISP
               IF(ISP>1)PAR%SPN=2
               READ(UN,'(A)') CDUM
               CYCLE kpoint
             END IF
           END DO band
         ELSE
            READ(UN,'(A)',IOSTAT=IOCHECK) CDUM
            IF(IOCHECK/=0)EXIT spin
         END IF
       END DO kpoint
     END DO spin
     NFILE(P)%NK=NKPT-MIN(PAR%KSKIP(2),NKPT)+MAX(PAR%KSKIP(1),0)
     NFILE(P:)%IKPTS=NFILE(P)%IKPTS+NFILE(P)%NK
     NFILE(P)%SPIN=ISP 
    CLOSE(UN)
  END DO path
 CLOSE(1000)
 N%BAND=MAXVAL(NFILE(:)%BND)
 N%TKPTS=NFILE(N%PATH)%IKPTS
 N%TCOL=SUM(NFILE(:)%NK)*N%BAND*PAR%SPN
 N%ORB=1
 PAR%FORB=1
END SUBROUTINE READHEAD_PRJCAR









SUBROUTINE READ_PRJCAR(FILEDIR, TEBS, KGRID, LATTICE )
 IMPLICIT NONE
 TYPE( BAND ), DIMENSION(:), ALLOCATABLE :: TEBS
 TYPE( LATT ) :: LATTICE
 TYPE( GRID ), DIMENSION(:), ALLOCATABLE :: KGRID
 REAL(KIND=DP) ::  TOTDIST, DELTA, TOT, ENERGY, KAPPA_SUM
 REAL(KIND=DP), DIMENSION(3) :: NVKPT,VKPT,KSCF0
 REAL(KIND=DP), DIMENSION(3,3) :: M_INV
 REAL(KIND=DP), DIMENSION(:), ALLOCATABLE :: SLIMIND,BLOCHS,KDIST
 INTEGER ::  I,ISP, IOCHECK,NKPT,TOTKPT, J, IJUMP, NI, NJ,MNKPT
 INTEGER :: UN,UNARRAY, ND, K, B, P, COL, IND, CIND, BND
 INTEGER :: ISTAT,IDUM1,IDUM2
 INTEGER,DIMENSION(3) :: IDUM,T
 INTEGER, DIMENSION(:), ALLOCATABLE :: MAP,KMAP
 CHARACTER(LEN=*) :: FILEDIR
 CHARACTER(1) :: A
 CHARACTER(20) :: FORMA,TARRAY
 CHARACTER(200) :: DATANAME,FENERGYFILE
 CHARACTER(300) :: CDUM
 CHARACTER(80) :: SCDUM
 CHARACTER(2) :: B1,B2,B3
 LOGICAL :: NEWPATH,STUCT,LEXIST,SLIM_PRJCAR,LDUM,LMINV,LKFIRST
 TOTKPT=0
 TOTDIST=0.0_DP
 UNARRAY=9924
 TARRAY='tempprjcararray.temp'
 T=(/1,0,1/)
 KSCF0=(/0.0_DP,0.0_DP,0.0_DP/)
 MNKPT=MAXVAL(NFILE(:)%KPTS)
 ALLOCATE(BLOCHS(MNKPT),KGRID(N%PATH),KDIST(MNKPT),MAP(MNKPT),KMAP(MNKPT))
 COL=1
 LMINV=.FALSE.
 WRITE(*,'(A)') '                   reading PRJCAR file(s)'
 OPEN(UNIT=UNARRAY, FILE=TARRAY, STATUS='REPLACE', ACTION='WRITE' )
 OPEN(UNIT=9999, FILE=TRIM(FILEDIR), STATUS='OLD', ACTION='READ')
  READ(9999,*)
  READ(9999,*)
  READ(9999,'(A)') FENERGYFILE
  READ(9999,*)
 IF(PAR%MULTIFERMI) OPEN(UNIT=9998, FILE=TRIM(FENERGYFILE), STATUS='OLD',ACTION='READ')
  path: DO P=1,N%PATH
    CALL WRITE_PROGRESS(P,N%PATH)
    NEWPATH=.TRUE.
    IF(PAR%MULTIFERMI) READ(9998,*) PAR%EFERMI
    READ(9999,'(A)') DATANAME
    UN=30+P
    OPEN(UNIT=UN, FILE=TRIM(DATANAME), STATUS='OLD',  ACTION='READ')
     DO I=1,16
       READ(UN,'(A)') SCDUM
       IF(SCDUM(1:24)=="Transformation Matrix M'")THEN
        LMINV=.TRUE.
        EXIT
       ELSE IF(SCDUM(1:18)=='number of k-points')THEN
        EXIT
       END IF
     END DO
     IF(LMINV)THEN
       DO I=1,3
         READ(UN,*) M_INV(:,I)
       END DO
       READ(UN,'(A)') SCDUM
       READ(UN,'(A)') SCDUM
     END IF
     I=1
     DO WHILE(I<10)
       READ(SCDUM,'(40X,A1,I6)',IOSTAT=IOCHECK) A,NKPT
       IF((IOCHECK==0).AND.(A==':').AND.(NKPT>0)) EXIT
       READ(UN,'(A)') SCDUM
       I=I+1
     END DO
     IF(IOCHECK/=0)THEN
       WRITE(*,*)'ERROR: PRJCAR structure of ', TRIM(DATANAME), &
       & ' unknown:',IOCHECK
       STOP 1
     END IF
      DO I=1,10
       READ(UN,'(A)') CDUM
       SCDUM=ADJUSTL(CDUM)
       IF(SCDUM(1:5)=='NKPTS') THEN
          SCDUM=ADJUSTL(SCDUM(6:))
       END IF
       IF((SCDUM(1:3)=='KPT').OR.(SCDUM(1:3)=='kpt')) SCDUM=ADJUSTL(SCDUM(4:))
       B1=SCDUM(1:2)
       SCDUM=ADJUSTL(SCDUM(3:))
       B2=SCDUM(1:2)
       SCDUM=ADJUSTL(SCDUM(3:))
       B3=SCDUM(1:2)
       IF((B1=='b1').AND.(B2=='b2').AND.(B3=='b3')) EXIT
     END DO
     LKFIRST=.TRUE.
     KDIST(:)=0.0_DP
     KMAP(:)=0
     K=1
     DO I=1,NFILE(P)%KPTS
       READ(UN,*,IOSTAT=IOCHECK) IDUM1,(VKPT(J), J=1,3),IDUM2
       IF((I>=PAR%KSKIP(1)).AND.(I<=PAR%KSKIP(2)))CYCLE
       IF(LKFIRST)THEN
         KSCF0=VKPT
         LKFIRST=.FALSE.
       END IF
       KMAP(I)=K
       KDIST(I)=KDISTANCE(VKPT,KSCF0,LATTICE%B)
       K=K+1
     END DO
     KGRID(P)%A=KSCF0(:)
     KGRID(P)%B=VKPT(:)

     KGRID(P)%DIST=TOTDIST + MAXVAL(KDIST(:))
     IF (P>1) THEN
         TOTKPT=NFILE(P-1)%IKPTS
     END IF

     DO I=1,10
       READ(UN,'(A)') CDUM
       IF(CDUM(1:15)=='spin component:')EXIT
     END DO
 
     I=1
     spins: DO WHILE (I<=NFILE(P)%SPIN)
       IF(CDUM(1:15)/='spin component:')THEN
         READ(UN,'(A)',IOSTAT=IOCHECK) CDUM
         IF(IOCHECK/=0)EXIT
         CYCLE
       END IF
       READ(CDUM,'(15X,I4)') I
       READ(UN,'(A)') CDUM
       K=1
       kpoint: DO WHILE (K<=NFILE(P)%KPTS)    
         IF(CDUM(1:7)=='k-point')THEN
           READ(CDUM,'(33X,I6)') K
           BND=1
           READ(UN,'(A)',IOSTAT=IOCHECK) CDUM
           bands: DO WHILE (BND<=NFILE(P)%BND)
             IF(IOCHECK/=0)EXIT spins
             IF(CDUM(1:5)=='band:')THEN
               READ(CDUM,'(5X,I6,9X,F14.7,11X,E15.7)') BND,ENERGY,KAPPA_SUM
               ENERGY=ENERGY-PAR%EFERMI


          IF((ENERGY<0.0_DP).AND.(ENERGY>N%ENMAX(I)))THEN
             N%ENMAX(I)=ENERGY
             N%LENMAX(I)=.TRUE.
          ELSE IF ((ENERGY>=0.0_DP).AND.(ENERGY<N%EPMIN(I)))THEN
             N%EPMIN(I)=ENERGY
             N%LEPMIN(I)=.TRUE.
          END IF
          

               IF((ENERGY<PAR%FBORDER(1)).OR.(ENERGY>PAR%FBORDER(2))) THEN
                 BND=BND+1
                 READ(UN,'(A)',IOSTAT=IOCHECK) CDUM
                 CYCLE
               END IF
               CIND=0
               DO J=1,INT(NFILE(P)%KPTS/7.0)+1
                 READ(UN,'(A)',IOSTAT=IOCHECK) CDUM
                 IF(IOCHECK/=0)EXIT spins
                 IF(CDUM(1:5)=='band:')EXIT
                 IF (CDUM(1:7)=='k-point')EXIT
                 IF (CDUM(1:4)=='spin') EXIT
                 IF(SCAN(CDUM,':')>0) THEN
                   NFILE(P)%SLIMPRJ=.TRUE.
                 ELSE
                   NFILE(P)%SLIMPRJ=.FALSE.
                 END IF
                 IND=NITEMS(CDUM)
                 IF (NFILE(P)%SLIMPRJ) THEN
                   IND=IND/2
                   FORMA="(  (I4,1X,E15.7,1X))"
                   WRITE(FORMA(2:3),'(I2)') IND
                   READ(CDUM,FORMA) (MAP(NI),BLOCHS(NI), NI=CIND+1,CIND+IND)
                   CIND=CIND+IND
                 ELSE
                   FORMA="(  E15.7)"
                   WRITE(FORMA(2:3),'(I2)') IND
                   READ(CDUM,FORMA) (BLOCHS(NI), NI=CIND+1,CIND+IND)
                   FORALL(NJ=CIND+1:CIND+IND) MAP(NJ)=NJ
                   CIND=CIND+IND
                 END IF
               END DO
               DO NI=1,CIND
                 IF(KAPPA_SUM<=PAR%TRASH)CYCLE
                 IF((KAPPA_SUM>0.001_DP).AND.(PAR%NORM)) & 
                 &        BLOCHS(NI)=BLOCHS(NI)/KAPPA_SUM
                 IF(BLOCHS(NI)<=PAR%TRASH) CYCLE
                 NJ=KMAP(MAP(NI))
                 IF(NJ==0)CYCLE
              WRITE(UNARRAY,333) P, I, TOTKPT + NJ, BND, &
              & TOTDIST + KDIST(MAP(NI)), ENERGY, BLOCHS(NI)
                 COL=COL+1
               END DO
             ELSE IF (CDUM(1:7)=='k-point') THEN
               EXIT bands
             ELSE IF (CDUM(1:4)=='spin') THEN
               EXIT kpoint
             ELSE
               READ(UN,'(A)',IOSTAT=IOCHECK) CDUM
               CYCLE
             END IF
             BND=BND+1
           END DO bands
           K=K+1
         ELSE
           READ(UN,'(A)',IOSTAT=IOCHECK) CDUM
           IF(IOCHECK/=0)EXIT
         END IF
       END DO kpoint
       I=I+1
     END DO spins
    CLOSE(UN)
    TOTDIST=KGRID(P)%DIST
  END DO path
 CLOSE(9999)
 IF(PAR%MULTIFERMI) CLOSE(9998)
 CLOSE(UNARRAY)

!allocate and prepare the array for the unfiltered dat TEBS
 DEALLOCATE(BLOCHS,MAP,KMAP,KDIST)
 ALLOCATE(TEBS(COL))
 CALL SETUP_ORBIT(TEBS,N%ORB)
 CALL ZERO(TEBS)

!write everything in the unfiltered array
OPEN(UNIT=UNARRAY, FILE=TARRAY, STATUS='OLD', ACTION='READ' )
COL=1

ISTAT = 0
DO WHILE ( ISTAT == 0 )

  READ(UNARRAY,'(A)',IOSTAT=ISTAT) CDUM
  IF( ISTAT /= 0 ) EXIT
  READ(CDUM,333) TEBS(COL)%PATH,TEBS(COL)%SPN, &
  & TEBS(COL)%NKPT,TEBS(COL)%BND,TEBS(COL)%K,TEBS(COL)%E,TEBS(COL)%W
  TEBS(COL)%LOCU=.TRUE.
  COL=COL+1
END DO

CLOSE(UNARRAY)

 N%TCOL=COL
   333 FORMAT(I5,I3, 1X, I7, 1X, I5, 1X, 3F24.16)
END SUBROUTINE READ_PRJCAR






SUBROUTINE SETUP_EBS()
 N%SAMEK=INT(REAL(N%TCOL)/REAL(N%TKPTS))+10
 ALLOCATE(EBS( N%TKPTS, N%SAMEK,PAR%SPN ),LPOS( N%TKPTS, N%SAMEK ))
 CALL SETUP_ORBIT(EBS,N%ORB)
 CALL ZERO(EBS)

END SUBROUTINE SETUP_EBS







SUBROUTINE CHARACTERISE_ENERGIES( TEBS, LUNFOLD )
 IMPLICIT NONE
 TYPE( BAND ), DIMENSION(:), INTENT(IN) :: TEBS
 REAL(KIND=DP), PARAMETER :: THIN=0.000001_DP
 REAL(KIND=DP) :: DIF, EDIST, EDISTSUM, EDELTA, EMIN, EMAX
 REAL(KIND=DP), DIMENSION(2) :: SWEIGHT
 REAL(KIND=DP), DIMENSION(2,N%ORB) :: SOC
 REAL(KIND=DP), DIMENSION(N%SAMEK) :: WEIGHT, ENERGY
 REAL(KIND=DP), DIMENSION(N%SAMEK,N%ORB) :: OC
 INTEGER :: K,NCOL,NO,I,J,NBEGIN,SPN
 INTEGER, DIMENSION(N%SAMEK) :: AV
 INTEGER, DIMENSION(N%SAMEK,N%SAMEK) :: FK
 LOGICAL :: LUNFOLD
 LOGICAL, DIMENSION(N%SAMEK) :: FOUNDK

 SPIN: DO SPN=1,PAR%SPN
 NBEGIN = 1
 KDO: DO K = 1,N%TKPTS
   WEIGHT = 0.0_DP
   ENERGY = 0.0_DP
   OC = 0.0_DP
   AV=0
   FOUNDK = .FALSE.
   IF(NBEGIN>SIZE(TEBS))EXIT
   NCOL = NBEGIN
   DO WHILE (TEBS(NCOL)%SPN/=SPN)
     NCOL=NCOL+1
     IF( NCOL > SIZE(TEBS,1) ) EXIT KDO
   END DO

   DO WHILE ( TEBS(NCOL)%NKPT == K )
     IF (TEBS(NCOL)%LOCU.AND.(TEBS(NCOL)%SPN==SPN)) THEN  !check only if the position is ocupied
       DO J=1,N%SAMEK
         IF (FOUNDK(J))THEN
            DIF=ABS(TEBS(NCOL)%E-TEBS(FK(J,AV(J)))%E)
            IF((.NOT.PAR%BAND).AND.(TEBS(NCOL)%BND/=TEBS(FK(J,AV(J)))%BND))CYCLE
            IF(.NOT.ORBIT_DIF(TEBS(NCOL)%OC(:),TEBS(FK(J,AV(J)))%OC(:)))CYCLE
         END IF
         IF ((FOUNDK(J).AND.(DIF<=PAR%EDIF)).OR.(.NOT.FOUNDK(J))) THEN
           WEIGHT(J) = WEIGHT(J) + TEBS(NCOL)%W 
           ENERGY(J) = ENERGY(J) + TEBS(NCOL)%E*TEBS(NCOL)%W
           FORALL(NO=1:N%ORB) OC(J,NO)=OC(J,NO)+TEBS(NCOL)%OC(NO)
           AV(J)=AV(J)+1
           FK(J,AV(J)) = NCOL
           FOUNDK(J) = .TRUE.
           EXIT
         END IF
       END DO    
     END IF
     NCOL=NCOL+1
     IF( NCOL > SIZE(TEBS,1) ) EXIT
   END DO
   IF (ANY(FOUNDK)) THEN
    !Locate the place in EBS
     DO J=1,N%SAMEK
       IF(FOUNDK(J))THEN
         IF (LUNFOLD) THEN
           ENERGY(J)=ENERGY(J)/WEIGHT(J)
           IF((ENERGY(J)>=PAR%WDIF(1)).AND.(ENERGY(J)<=PAR%WDIF(2)))THEN
             IF(AV(J)==1)THEN
               ENERGY(J)=TEBS(FK(J,1))%E
               WEIGHT(J)=TEBS(FK(J,1))%W
               FORALL(NO=1:N%ORB) OC(J,NO)=TEBS(FK(J,1))%OC(NO)
             ELSE
               IF((PAR%LBAV).OR.(PAR%LOAV))THEN
                 DO I=1,AV(J)
                   IF(I==1)THEN
                     EMIN=TEBS(FK(J,I))%E
                     EMAX=TEBS(FK(J,I))%E
                   ELSE
                     EMIN=MIN(EMIN,TEBS(FK(J,I))%E)
                     EMAX=MAX(EMAX,TEBS(FK(J,I))%E)
                   END IF
                 END DO
                 EDIST=EMAX-EMIN
               END IF

               IF(PAR%LBAV)THEN
                 IF(EDIST<THIN)THEN
                   WEIGHT(J)=WEIGHT(J)/REAL(AV(J))
                 ELSE
                   EDISTSUM=0.0_DP
                   SWEIGHT=0.0_DP
                   DO I=1,AV(J)
                     EDELTA=ABS(TEBS(FK(J,I))%E-ENERGY(J))
                     EDISTSUM=EDISTSUM+EDELTA
                     SWEIGHT(1)=SWEIGHT(1)+EDELTA*TEBS(FK(J,I))%W
                     SWEIGHT(2)=SWEIGHT(2)+EDIST*TEBS(FK(J,I))%W
                   END DO
                   WEIGHT(J)=SWEIGHT(2)-SWEIGHT(1)
                   EDISTSUM=AV(J)*EDIST-EDISTSUM
                   WEIGHT(J)=WEIGHT(J)/EDISTSUM
                 END IF
               END IF

               IF(PAR%LOAV)THEN
                 IF(EDIST<THIN)THEN
                   FORALL(NO=1:N%ORB) OC(J,NO)=OC(J,NO)/REAL(AV(J))
                 ELSE
                   EDISTSUM=0.0_DP
                   SOC=0.0_DP
                   DO I=1,AV(J)
                     EDELTA=ABS(TEBS(FK(J,I))%E-ENERGY(J))
                     EDISTSUM=EDISTSUM+EDELTA
                     DO NO=1,N%ORB
                       SOC(1,NO)=SOC(1,NO)+EDELTA*TEBS(FK(J,I))%OC(NO)
                       SOC(2,NO)=SOC(2,NO)+EDIST*TEBS(FK(J,I))%OC(NO)
                     END DO
                   END DO
                   FORALL(NO=1:N%ORB) OC(J,NO)=SOC(2,NO)-SOC(1,NO)
                   EDISTSUM=AV(J)*EDIST-EDISTSUM
                   DO NO=1,N%ORB
                     OC(J,NO)=OC(J,NO)/EDISTSUM
                   END DO
                 END IF
               END IF
             END IF
           END IF
         ELSE 
           ENERGY(J)=ENERGY(J)/REAL(AV(J))
           OC(J,:)=OC(J,:)/REAL(AV(J))
           SWEIGHT=1.0_DP
           WEIGHT=1.0_DP
         END IF
         IF((ENERGY(J)<PAR%WDIF(1)).AND.(ENERGY(J)>PAR%WDIF(2)))THEN!.OR.(WEIGHT(J)<=PAR%TRASH))THEN
           CALL ZERO(EBS(K,J,SPN))
         ELSE

           EBS(K,J,SPN)%PATH = TEBS(FK(J,1))%PATH
           EBS(K,J,SPN)%SPN = SPN
           EBS(K,J,SPN)%NKPT = K
           EBS(K,J,SPN)%BND = TEBS(FK(J,1))%BND
           EBS(K,J,SPN)%K = TEBS(FK(J,1))%K
           EBS(K,J,SPN)%E = ENERGY(J)
           EBS(K,J,SPN)%W = WEIGHT(J)
           EBS(K,J,SPN)%OC(:) = OC(J,:)
           EBS(K,J,SPN)%LOCU = .TRUE.
         END IF
       ELSE
         CALL ZERO(EBS(K,J,SPN))
       END IF
     END DO
     FOUNDK=.FALSE.
   END IF
   NBEGIN=NCOL
 END DO KDO
 END DO SPIN
END SUBROUTINE CHARACTERISE_ENERGIES




LOGICAL FUNCTION WRITE_PLOTDATA(PLOTFILE,ACOLBAND,TEBS,FILETYPE,SPINFILE,ORBITSTATS,LREAD)
 IMPLICIT NONE
 TYPE( BAND ),DIMENSION(:),INTENT(IN) :: TEBS
 CHARACTER(LEN=*) :: PLOTFILE,ACOLBAND,SPINFILE,ORBITSTATS
 CHARACTER(LEN=60) :: PFILE,SPFILE,BPFILE,EMPTYORBS,OSTATS
 CHARACTER(LEN=6) :: CSPN
 REAL(DP) :: MAXORB
 INTEGER :: B, K, I,SPN,NBANDS, FILETYPE,OR,OBEGIN,OEND,ACOLEN,EOPOS,EOLEN,LREAD
 INTEGER, DIMENSION(N%BAND) :: BNDINDEX
 LOGICAL, DIMENSION(N%ORB) :: NOORB
 LOGICAL :: OFIRST

 SPIN: DO SPN=1,PAR%SPN
   OFIRST=.TRUE.
 IF(PAR%SPN==2)THEN
   WRITE(CSPN,'(A5,I1)') '.spin',SPN
   ACOLEN=LEN_TRIM(ACOLBAND)+8
   BPFILE=TRIM(ACOLBAND)//CSPN//'.'
   PFILE=TRIM(PLOTFILE)//CSPN//'.dat'
   OSTATS=TRIM(ORBITSTATS)//CSPN//'.dat'
   SPFILE=TRIM(PLOTFILE)//'.slim'//CSPN//'.dat'
 ELSE
   ACOLEN=LEN_TRIM(ACOLBAND)+1
   BPFILE=TRIM(ACOLBAND)
   PFILE=TRIM(PLOTFILE)//'.dat'
   OSTATS=TRIM(ORBITSTATS)//'.dat'
   SPFILE=TRIM(PLOTFILE)//'.slim.dat'
 END IF
 NOORB=.TRUE.
 BNDINDEX(:)=0
 NBANDS=0
 OBEGIN=PAR%FORB
 OEND=PAR%FORB
 IF(FILETYPE/=3)THEN
   IF(PAR%FORB==0)THEN
     OBEGIN=1
     IF((PAR%SION(1)<1).AND.(PAR%SION(2)<1))THEN
        OEND=N%ORB-1
     ELSE
        OEND=N%ORB
     END IF
   END IF
   MAXORB=0.0_DP
   DO I=1,SIZE(TEBS,1)
     IF (TEBS(I)%LOCU.AND.(TEBS(I)%SPN==SPN)) THEN
       DO OR=OBEGIN,OEND
         MAXORB=MAX(MAXORB,TEBS(I)%OC(OR))
         IF(TEBS(I)%OC(OR)>0.01_DP) NOORB(OR)=.FALSE.
       END DO
     END IF
   END DO


 OPEN(UNIT=75, FILE=TRIM(OSTATS), STATUS='REPLACE', ACTION='WRITE')

   I=0
   EOPOS=1
   DO OR=OBEGIN,MAX(OEND,PAR%FORB)
     IF(NOORB(OR))THEN
       EOLEN=LEN_TRIM(ADJUSTL(PAR%ORBNAMES(OR)))
       EOLEN=EOLEN+EOPOS
       IF(OFIRST)THEN
         WRITE(EMPTYORBS(EOPOS:EOLEN),'(A)') TRIM(ADJUSTL(PAR%ORBNAMES(OR)))
         OFIRST=.FALSE.
       ELSE
         EOLEN=EOLEN+2
         WRITE(EMPTYORBS(EOPOS:EOLEN),'(A)') ', '//TRIM(ADJUSTL(PAR%ORBNAMES(OR)))
       END IF
       EOPOS=EOLEN+1
     ELSE
       WRITE(75,'(A1,I2,A)') '#',I,'ORBITAL='//TRIM(ADJUSTL(PAR%ORBNAMES(OR)))
       I=I+1
     END IF
   END DO
   WRITE(75,'(A,I3,",",I3)') '#LAYOUT=',GET_LAYOUT(I)
   WRITE(75,'(A,I3,",",I3)') '#SLAYOUT=',GET_SQRT_LAYOUT(I)
   IF(I==0)THEN
     WRITE(75,'(A)') '#noorbital'
     IF(PAR%SPN==2)THEN
       WRITE(*,'(A,I1,A)') 'Raw data (spin=',SPN,'):: No orbitalcharcter greater than 0.01 found'
     ELSE
       WRITE(*,'(A)') 'Raw data:: No orbitalcharcter greater than 0.01 found'
     END IF
     WRITE(*,*) ''
   ELSE
     WRITE(75,'(A,F4.2)') '#maxorb ',MAXORB
     IF((ANY(NOORB(1:OEND))).AND.(PAR%FORB==0))THEN
       WRITE(75,'(A)') '#emptyorbs '//EMPTYORBS(1:EOLEN)
       IF(PAR%SPN==2)THEN
         WRITE(*,'(A,I1,A)') 'The following orbital(s) do not have values above 0.01 for spin=',SPN,' :'
       ELSE
         WRITE(*,'(A)') 'The following orbital(s) do not have values above 0.01:'
       END IF
       WRITE(*,'(A)') EMPTYORBS(1:EOLEN)
       WRITE(*,*)
     END IF
   END IF

 CLOSE(75)

 END IF

 IF(LREAD==0)THEN
 OPEN(UNIT=65, FILE=TRIM(PFILE), STATUS='REPLACE', ACTION='WRITE')
 IF(FILETYPE/=3)THEN
       DO OR=1,N%ORB
         WRITE(65,'(A1,I2,A)') '#',OR-1,'ALLORBITS='//TRIM(ADJUSTL(PAR%ORBNAMES(OR)))
       END DO
 END IF
   DO I=1,SIZE(TEBS)
     IF(TEBS(I)%LOCU.AND.(TEBS(I)%E>=PAR%FBORDER(1)).AND.(TEBS(I)%E<=PAR%FBORDER(2)).AND.(TEBS(I)%SPN==SPN)) THEN
         WRITE(65,'(3(F14.8))',ADVANCE='NO') TEBS(I)%K,TEBS(I)%E,TEBS(I)%W
         DO OR=1,N%ORB
           WRITE(65,'(F14.8)',ADVANCE='NO') TEBS(I)%OC(OR)
         END DO
         WRITE(65,'(3(1X,I5))') TEBS(I)%BND,TEBS(I)%NKPT,TEBS(I)%PATH
     END IF
   END DO
 CLOSE(65)
END IF



 IF(PAR%BANDPLOT)THEN
   DO I=1,SIZE(TEBS)
      BNDINDEX=MAKE_BAND_LIST(BNDINDEX,TEBS(I)%BND,NBANDS)
   END DO
   DO K=1,NBANDS
     B=BNDINDEX(K)
     IF(B<10)THEN
       WRITE(BPFILE(ACOLEN:ACOLEN),'(I1)') B
     ELSE IF((B>9).AND.(B<100))THEN
       WRITE(BPFILE(ACOLEN:ACOLEN+1),'(I2)') B
     ELSE IF ((B>99).AND.(B<1000))THEN
       WRITE(BPFILE(ACOLEN:ACOLEN+2),'(I3)') B
     ELSE
       WRITE(BPFILE(ACOLEN:ACOLEN+3),'(I4)') B
     END IF
     OPEN(UNIT=75,FILE=TRIM(BPFILE)//'.dat',STATUS='REPLACE',ACTION='WRITE')
   DO I=1,SIZE(TEBS)
   IF(TEBS(I)%LOCU.AND.(TEBS(I)%SPN==SPN))THEN
     IF((TEBS(I)%BND==B).AND.(TEBS(I)%E>=1.2_DP*PAR%FBORDER(1)).AND.(TEBS(I)%E<=1.2_DP*PAR%FBORDER(2))) THEN
       WRITE(75,'(3(F14.8),1X,I4)') TEBS(I)%K,TEBS(I)%E,&
       & TEBS(I)%W, TEBS(I)%BND
     END IF
   END IF
   END DO
 CLOSE(75)
 END DO
END IF

 NOORB=.TRUE.
 IF(FILETYPE/=3)THEN
 DO K=1,N%TKPTS
   DO I=1,N%SAMEK
     IF((EBS(K,I,SPN)%LOCU).AND.(EBS(K,I,SPN)%E>=PAR%WDIF(1)).AND.(EBS(K,I,SPN)%E<=PAR%WDIF(2)))THEN
       IF(PAR%FORB==0)THEN
         DO OR=1,N%ORB
           IF(EBS(K,I,SPN)%OC(OR)>0.01_DP) NOORB(OR)=.FALSE.
         END DO
       ELSE
         IF(EBS(K,I,SPN)%OC(PAR%FORB)>0.01_DP) NOORB(PAR%FORB)=.FALSE.
       END IF
     END IF
   END DO
 END DO
 END IF

 OPEN(UNIT=75, FILE=TRIM(SPFILE), STATUS='REPLACE', ACTION='WRITE')
 IF(FILETYPE/=3)THEN
  IF(PAR%FORB==0)THEN
    I=0
      DO OR=1,N%ORB
        IF(NOORB(OR))CYCLE
        WRITE(75,'(A1,I2,A)') '#',I,'ORBITAL='//TRIM(ADJUSTL(PAR%ORBNAMES(OR)))
        I=I+1
      END DO
    WRITE(75,'(A,2I3)') '#LAYOUT=',GET_LAYOUT(I)
  ELSE
    I=1
    WRITE(75,'(A)') '# 1ORBITAL='//TRIM(ADJUSTL(PAR%ORBNAMES(PAR%FORB)))
    IF(NOORB(PAR%FORB)) I=0
  END IF
  IF(I==0)THEN
    WRITE(75,'(A)') '#noorbital'
    IF(PAR%SPN==2)THEN
      WRITE(*,'(A,I1,A)') 'Processed data (spin=',SPN,'):: No orbitalcharcter greater than 0.01 found'
    ELSE
      WRITE(*,'(A)') 'Processed data:: No orbitalcharcter greater than 0.01 found'
    END IF
    WRITE(*,*) ''
  END IF
END IF
  DO K=1,N%TKPTS
    DO I=1,N%SAMEK
     IF((EBS(K,I,SPN)%LOCU).AND.(EBS(K,I,SPN)%E>=PAR%WDIF(1)).AND.(EBS(K,I,SPN)%E<=PAR%WDIF(2)))THEN
       IF(PAR%FORB==0)THEN
         WRITE(75,'(3(F14.8))',ADVANCE='NO') EBS(K,I,SPN)%K,EBS(K,I,SPN)%E,EBS(K,I,SPN)%W
         DO OR=1,N%ORB
           IF(NOORB(OR))CYCLE
           WRITE(75,'(F14.8)',ADVANCE='NO') EBS(K,I,SPN)%OC(OR)
         END DO
         WRITE(75,'(1X,I4)') EBS(K,I,SPN)%BND
       ELSE
         WRITE(75,'(4(F14.8),1X,I4)') EBS(K,I,SPN)%K,EBS(K,I,SPN)%E,&
         & EBS(K,I,SPN)%W,EBS(K,I,SPN)%OC(PAR%FORB),EBS(K,I,SPN)%BND
       END IF
     END IF
    END DO
  END DO
 CLOSE(75)
 END DO SPIN

 OPEN(UNIT=150,FILE=TRIM(SPINFILE),STATUS='REPLACE',ACTION='WRITE')
   WRITE(150,'(I1)') PAR%SPN
 CLOSE(150)
 WRITE_PLOTDATA=.TRUE.
 RETURN

END FUNCTION WRITE_PLOTDATA





SUBROUTINE WRITE_FILEDATA(STATFILE,FILETYPE)
  CHARACTER(LEN=*) :: STATFILE
  INTEGER :: FILETYPE,P,SPN,UN=191

  OPEN(UNIT=UN,FILE=TRIM(STATFILE),STATUS='REPLACE',ACTION='WRITE')

    WRITE(UN,'(A)') '#This file contains internal information of the choosen VASP files'
    WRITE(UN,'(A)') '#It is used for the --readsave option (see documentation)'
    WRITE(UN,'(6(I8,1X),2(I1,1X))') N%PATH,N%BAND,N%ORB,N%TKPTS,N%TCOL,N%SAMEK,FILETYPE,PAR%SPN
    DO SPN=1,PAR%SPN
      WRITE(UN,'(2L1)')  N%LENMAX(SPN), N%LEPMIN(SPN)
      WRITE(UN,'(2F19.12)')  N%ENMAX(SPN), N%EPMIN(SPN)
    END DO
          
    IF(FILETYPE/=3)THEN
      DO P=1,N%ORB
        WRITE(UN,'(A)') TRIM(ADJUSTL(PAR%ORBNAMES(P)))
      END DO
    END IF
    DO P=1,N%PATH
      WRITE(UN,'(7(I7,1X))')NFILE(P)%NK,NFILE(P)%KPTS,NFILE(P)%IKPTS, &
      & NFILE(P)%ORBIT,NFILE(P)%BND,NFILE(P)%IONS,NFILE(P)%SPIN
    END DO

  CLOSE(UN)
END SUBROUTINE WRITE_FILEDATA



SUBROUTINE READ_FILEDATA(STATFILE,FILETYPE)
  CHARACTER(LEN=*) :: STATFILE
  CHARACTER(LEN=20) :: CDUM
  INTEGER :: FILETYPE,SPN,I,P,UN=192

  
  OPEN(UNIT=UN,FILE=TRIM(STATFILE),STATUS='OLD',ACTION='READ')

    READ(UN,*)
    READ(UN,*)
    READ(UN,'(6(I8,1X),2(I1,1X))') N%PATH,N%BAND,N%ORB,N%TKPTS,N%TCOL,N%SAMEK,FILETYPE,PAR%SPN
    DO SPN=1,PAR%SPN
      READ(UN,'(2L1)')  N%LENMAX(SPN), N%LEPMIN(SPN)
      READ(UN,'(2F19.12)')  N%ENMAX(SPN), N%EPMIN(SPN)
    END DO
          
    ALLOCATE(PAR%ORBNAMES(N%ORB))
    IF(FILETYPE/=3)THEN
    DO P=1,N%ORB
      READ(UN,'(A)') CDUM
      PAR%ORBNAMES(P)=TRIM(ADJUSTL(CDUM))
    END DO
    END IF

    ALLOCATE(NFILE(N%PATH))
    DO P=1,N%PATH
      READ(UN,'(7(I7,1X))')NFILE(P)%NK,NFILE(P)%KPTS,NFILE(P)%IKPTS, &
      & NFILE(P)%ORBIT,NFILE(P)%BND,NFILE(P)%IONS,NFILE(P)%SPIN
    END DO

  CLOSE(UN)

 IF(PAR%FORB<0)PAR%FORB=N%ORB

 IF(FILETYPE/=3)THEN

 IF(PAR%FORB==0)THEN
   WRITE(*,'(A,A)',ADVANCE='NO') 'Selected all orbitals :: ',TRIM(PAR%ORBNAMES(1))
   DO I=2,N%ORB-1
     WRITE(*,'(A,A)',ADVANCE='NO') ', ',TRIM(PAR%ORBNAMES(I))
   END DO
   WRITE(*,*) ''
 ELSE IF(PAR%FORB==N%ORB)THEN
   WRITE(*,'(A,A)') 'Selected the total amount of all orbitals:'
   WRITE(*,'(A,A)',ADVANCE='NO') '=> ',TRIM(PAR%ORBNAMES(1))
   DO I=2,N%ORB-1
     WRITE(*,'(A,A)',ADVANCE='NO') ' + ',TRIM(PAR%ORBNAMES(I))
   END DO
   WRITE(*,*)
 ELSE
   WRITE(*,'(A,A)') 'Selected orbital :: ',TRIM(PAR%ORBNAMES(PAR%FORB))
 END IF

 WRITE(*,*) ''
 END IF

END SUBROUTINE READ_FILEDATA




SUBROUTINE READ_GRID(OUTGRID,KGRID)
  CHARACTER(LEN=*) :: OUTGRID
  TYPE( GRID ),DIMENSION(:),ALLOCATABLE :: KGRID
  INTEGER :: I

  ALLOCATE(KGRID(N%PATH))
  OPEN(UNIT=95, FILE=TRIM(OUTGRID), STATUS='OLD', ACTION='READ')
    READ(95,*)
    READ(95,*)
    DO I=1,N%PATH
      READ(95,*) 
    END DO
    DO I=1,N%PATH
      READ(95,'(5X,7F17.12)') KGRID(I)%DIST,KGRID(I)%A,KGRID(I)%B
    END DO
  CLOSE(95)

END SUBROUTINE READ_GRID







SUBROUTINE READ_PLOTDATA(PLOTFILE,TEBS,FILETYPE)
 IMPLICIT NONE
 TYPE( BAND ),DIMENSION(:),ALLOCATABLE :: TEBS
 REAL(KIND=DP),DIMENSION(N%ORB) :: ORBVALS
 CHARACTER(LEN=*) :: PLOTFILE
 CHARACTER(LEN=300) :: BCDUM
 CHARACTER(LEN=60) :: PFILE,CDUM
 CHARACTER(LEN=20) :: FORMA='(  (F14.8),3(1X,I5))'
 CHARACTER(LEN=6) :: CSPN
 INTEGER :: I,SPN,NORB, FILETYPE,OR,EFFORB,IOCHECK
 I=1
 SPIN: DO SPN=1,PAR%SPN
   IF(PAR%SPN==2)THEN
     WRITE(CSPN,'(A5,I1)') '.spin',SPN
     PFILE=TRIM(PLOTFILE)//CSPN//'.dat'
   ELSE
     PFILE=TRIM(PLOTFILE)//'.dat'
   END IF

   IF(SPN==1)THEN
     ALLOCATE(TEBS(N%TCOL))
     CALL SETUP_ORBIT(TEBS,N%ORB)
     CALL ZERO(TEBS)
   END IF

   OPEN(UNIT=65, FILE=TRIM(PFILE), STATUS='OLD', ACTION='READ')
    IF(FILETYPE/=3)THEN

      DO OR=1,N%ORB
        READ(65,'(A)') CDUM
        IF(.NOT.(CDUM(4:12)=='ALLORBITS'))EXIT
      END DO
    END IF

    READ(65,'(A)',IOSTAT=IOCHECK) BCDUM
    NORB=NITEMS(TRIM(ADJUSTL(BCDUM)))-3
    IF(NORB<10)THEN
      WRITE(FORMA(3:3),'(I1)')NORB
    ELSE
      WRITE(FORMA(2:3),'(I2)')NORB
    END IF

    DO WHILE ( IOCHECK == 0 )
      IF(I>1)READ(65,'(A)',IOSTAT=IOCHECK) BCDUM

      IF( IOCHECK /= 0 ) EXIT

        READ(BCDUM,FORMA) TEBS(I)%K,TEBS(I)%E,TEBS(I)%W, &
        & TEBS(I)%OC,TEBS(I)%BND,TEBS(I)%NKPT,TEBS(I)%PATH

      TEBS(I)%SPN=SPN
      TEBS(I)%LOCU=.TRUE.
      I=I+1
    END DO
   CLOSE(65)
 END DO SPIN

END SUBROUTINE READ_PLOTDATA







FUNCTION GET_LAYOUT(I)
 INTEGER :: I,D,N
 INTEGER,DIMENSION(2) :: GET_LAYOUT
 REAL :: P,TYN=1.0E-6

 SELECT CASE( I )
   CASE( :3 )
     GET_LAYOUT=(/ 1, I /)
   CASE( 4 )
     GET_LAYOUT=(/ 2, 2 /)
   CASE( 5:6 )
     GET_LAYOUT=(/ 3, 2 /)
   CASE( 7:8 )
     GET_LAYOUT=(/ 4, 2 /)
   CASE( 9 )
     GET_LAYOUT=(/ 3, 3 /)
   CASE( 10 )
     GET_LAYOUT=(/ 2, 5 /)
   CASE( 11:12 )
     GET_LAYOUT=(/ 4, 3 /)
   CASE( 13:15 )
     GET_LAYOUT=(/ 5, 3 /)
   CASE( 16 )
     GET_LAYOUT=(/ 4, 4 /)
   CASE DEFAULT
     IF((ABS(MOD(SQRT(REAL(I)),1.0))<TYN).OR. &
     & (ABS(MOD(SQRT(REAL(I)),1.0)-1.0)<TYN))THEN
       D=INT(SQRT(REAL(I))+0.5)
       GET_LAYOUT=(/ D, D /)
     ELSE
       D=INT(SQRT(REAL(I)))
       P=REAL(I)/REAL(D)
       IF((P-FLOOR(P))<TYN)THEN
         GET_LAYOUT=(/ D, INT(P+0.5) /)
       ELSE
         GET_LAYOUT=(/ D, INT(P)+1 /)
       END IF
     END IF
   END SELECT

END FUNCTION GET_LAYOUT



FUNCTION GET_SQRT_LAYOUT(I)
 INTEGER :: I,D,N
 INTEGER,DIMENSION(2) :: GET_SQRT_LAYOUT
 REAL :: P,TYN=1.0E-6

 SELECT CASE( I )
   CASE( :0 )
     GET_SQRT_LAYOUT=(/ 1, 1 /)
   CASE( 1:3 )
     GET_SQRT_LAYOUT=(/ 1, I /)
   CASE( 4 )
     GET_SQRT_LAYOUT=(/ 2, 2 /)
   CASE( 5:6 )
     GET_SQRT_LAYOUT=(/ 2, 3 /)
   CASE( 7:9 )
     GET_SQRT_LAYOUT=(/ 3, 3 /)
   CASE( 10:12 )
     GET_SQRT_LAYOUT=(/ 3, 4 /)
   CASE( 13:16 )
     GET_SQRT_LAYOUT=(/ 4, 4 /)
   CASE DEFAULT
     IF((ABS(MOD(SQRT(REAL(I)),1.0))<TYN).OR. &
     & (ABS(MOD(SQRT(REAL(I)),1.0)-1.0)<TYN))THEN
       D=INT(SQRT(REAL(I))+0.5)
       GET_SQRT_LAYOUT=(/ D, D /)
     ELSE
       D=INT(SQRT(REAL(I)))
       P=REAL(I)/REAL(D)
       IF((P-FLOOR(P))<TYN)THEN
         GET_SQRT_LAYOUT=(/ D, INT(P+0.5) /)
       ELSE
         GET_SQRT_LAYOUT=(/ D, INT(P)+1 /)
       END IF
     END IF
   END SELECT

END FUNCTION GET_SQRT_LAYOUT



FUNCTION MAKE_BAND_LIST(VEC,NUM,NBANDS)
  INTEGER :: NUM,NBANDS, N,I,J
  INTEGER, DIMENSION(:), INTENT(IN) :: VEC
  INTEGER, DIMENSION(SIZE(VEC)) :: TEMP,MAKE_BAND_LIST
  
  IF(NBANDS==0)THEN
    NBANDS=1
    MAKE_BAND_LIST(1)=NUM
    RETURN
  END IF

  DO N=1,NBANDS
    IF(VEC(N)==NUM)EXIT
  END DO
  IF(N>NBANDS)THEN
    TEMP=0
    DO I=1,NBANDS
      IF(VEC(I)>NUM)THEN
        TEMP(I)=NUM
        DO J=I,NBANDS
          TEMP(J+1)=VEC(J)
        END DO
        EXIT
      END IF
      TEMP(I)=VEC(I)
      IF(I==NBANDS)TEMP(I+1)=NUM
    END DO
    NBANDS=NBANDS+1
    MAKE_BAND_LIST=TEMP
  ELSE
    MAKE_BAND_LIST=VEC
  END IF


END FUNCTION MAKE_BAND_LIST


LOGICAL FUNCTION ORBIT_DIF(OC1,OC2)
 REAL(DP),DIMENSION(:),INTENT(IN) :: OC1,OC2
 INTEGER :: I
 ORBIT_DIF=.FALSE.
 DO I=1,SIZE(OC1)
   IF(ABS(OC1(I)-OC2(I))>PAR%ODISTINCT) RETURN
 END DO
 ORBIT_DIF=.TRUE.

END FUNCTION ORBIT_DIF



SUBROUTINE FINDFITTPOINTS(FP, LFIT, SPN)
 IMPLICIT NONE
 TYPE( BAND ) :: EPOINT,BEPOINT
 TYPE( BAND ), ALLOCATABLE :: TFP(:),FP(:,:),STATFP(:,:)
 INTEGER :: I,J,K,P,NK,NJ,BNJ,NA,KK,NFP,NCROS,NN,SPN
 INTEGER, DIMENSION(2*PAR%POINTS,2) :: HITNUM
 REAL(DP),PARAMETER :: PI=1.0_DP*ATAN(1.0)
 REAL(DP) :: WEIGHT,DDIST,DGRAD,DALPHA,UPE,DOWNE
 REAL(DP), DIMENSION(2*PAR%POINTS-1) :: GRAD,DIST,ALPHA
 LOGICAL :: LFIT,LCROS,LEXIST1,LEXIST2
 LOGICAL, DIMENSION(N%TKPTS,N%SAMEK) :: POSTEMP
 WRITE(*,'(A)') '                   looking for fermiroots'
 LFIT = .FALSE.
 LCROS = .FALSE.
 N%FIT(SPN) = 0
 NCROS = 0
 NFP = 2*PAR%POINTS
 FORALL(K=1:N%TKPTS,J=1:N%SAMEK) LPOS(K,J)=EBS(K,J,SPN)%LOCU
 FORALL(K=1:N%TKPTS,J=1:N%SAMEK) POSTEMP(K,J)=LPOS(K,J)
 ALLOCATE(TFP(NFP),STATFP(N%TKPTS,NFP))
 CALL SETUP_ORBIT(TFP,N%ORB)
 CALL SETUP_ORBIT(STATFP,N%ORB)
 CALL SETUP_ORBIT(EPOINT,N%ORB)
 CALL ZERO(TFP)
 CALL ZERO(STATFP)

 K=1
 DO P=1,N%PATH
   IF((N%PATH>=15).OR.(N%TCOL>250000)) CALL WRITE_PROGRESS(P,N%PATH)
   kpoints: DO
     IF( NFILE(P)%IKPTS <= K ) THEN
       K=K+1
       EXIT
     END IF
     HITNUM=0
     samek: DO J=1,N%SAMEK
       IF(LPOS(K,J))THEN!.AND.(ABS(EBS(K,I,J)%E)<=PAR%WDIF)) THEN
         LEXIST1=.TRUE.
         DO WHILE ( LEXIST1 )
           CALL NEIGHBOUR(EBS(K,J,SPN)%E,K,EBS(K,J,SPN)%BND,LEXIST1,EPOINT,NJ,SPN)
 
           IF ( LEXIST1 ) THEN
             IF (( ABS(EBS(K,J,SPN)%E-EPOINT%E) <= PAR%EGAP ) &
             & .AND.( EBS(K,J,SPN)%E*EPOINT%E <= 0.0_DP ) &
             & .AND. (ABS(EBS(K,J,SPN)%W-EPOINT%W) <= PAR%DBLOCH) &
             & .AND. (ORBIT_DIF(EBS(K,J,SPN)%OC(:),EPOINT%OC(:)))) THEN

              CALL NEIGHBOUR(EPOINT%E,-K-1,EPOINT%BND,LEXIST2,BEPOINT,BNJ,SPN)
              IF(LEXIST2)THEN
                 IF (.NOT.(( ABS(BEPOINT%E-EPOINT%E) <= PAR%EGAP ) &
                 & .AND.( BEPOINT%E*EPOINT%E <= 0.0_DP ) &
                 & .AND. (ABS(BEPOINT%W-EPOINT%W) <= PAR%DBLOCH) &
                 & .AND. (ORBIT_DIF(BEPOINT%OC(:),EPOINT%OC(:))))) BNJ=J
               END IF

               CALL ZERO(TFP)
               LPOS=POSTEMP
               LCROS=.TRUE.
               TFP( PAR%POINTS ) = EBS(K,BNJ,SPN)
               LPOS(K,BNJ)=.FALSE.
               LPOS(K+1,NJ)=.FALSE.
               NA=PAR%POINTS
               HITNUM(NA,:)=(/K,BNJ/)
               HITNUM(NA+1,:)=(/K+1,NJ/)
               TFP( NA+1 ) = EPOINT
               DIST(NA)=ABS(EPOINT%E+EBS(K,BNJ,SPN)%E) 
               GRAD(NA)=(EPOINT%E-EBS(K,BNJ,SPN)%E)
               ALPHA(NA)=1.0_DP*ATAN(GRAD(NA))
             
               !RIGHT HAND SIDE
               KK = K+1
               DO NA = PAR%POINTS+2,NFP,1
                 LEXIST2=.TRUE.
                 DO WHILE (LEXIST2)
             CALL NEIGHBOUR(TFP(NA-1)%E+GRAD(NA-2),KK,TFP(NA-1)%BND,LEXIST2,EPOINT,NJ,SPN)

                   IF (LEXIST2) THEN
                     GRAD(NA-1)=(EPOINT%E-TFP(NA-1)%E)
                     ALPHA(NA-1)=1.0_DP*ATAN(GRAD(NA-1))
                     DALPHA=ALPHA(NA-2)+PAR%DGRAD*PI
                     IF(DALPHA>1.8_DP*PI)THEN
                       UPE=TFP(NA-1)%E+PAR%EGAP
                     ELSE
                       UPE=TFP(NA-1)%E+TAN(DALPHA)
                     END IF
                     UPE=MIN(UPE,PAR%WDIF(2))

                     DALPHA=ALPHA(NA-2)-PAR%DGRAD*PI
                     IF(ABS(DALPHA)>1.8_DP*PI)THEN
                       DOWNE=TFP(NA-1)%E-PAR%EGAP
                     ELSE
                       DOWNE=TFP(NA-1)%E+TAN(DALPHA)
                     END IF
                     DOWNE=MAX(DOWNE,PAR%WDIF(1))
 
                     IF (( ABS(TFP(NA-1)%E-EPOINT%E) <= PAR%EGAP ).AND. &
                & (EPOINT%E < UPE) .AND. (EPOINT%E > DOWNE ) .AND. &
                & (ABS(EPOINT%W-TFP(NA-1)%W)<=PAR%DBLOCH).AND. &
                       & (ORBIT_DIF(EPOINT%OC(:),TFP(NA-1)%OC(:))))THEN
                         EXIT
                     ELSE
                      LPOS(KK+1,NJ)=.FALSE.
                       CYCLE
                     END IF
                   END IF
                 END DO
                 LPOS=POSTEMP
                 IF ((.NOT.LEXIST2).OR.(EPOINT%PATH/=P)) EXIT
                 KK = KK+1
                 TFP(NA) = EPOINT
                 LPOS(KK,NJ)=.FALSE.
                 HITNUM(NA,:)=(/KK,NJ/)
               END DO

               KK = K
               !LEFT HAND SIDE
               DO NA = PAR%POINTS-1,1,-1
                 LEXIST2=.TRUE.
                 DO WHILE (LEXIST2)
          CALL NEIGHBOUR(TFP(NA+1)%E-GRAD(NA+1),-KK,TFP(NA+1)%BND,LEXIST2,EPOINT,NJ,SPN)
                   IF (LEXIST2) THEN
                      GRAD(NA)=TFP(NA+1)%E-EPOINT%E
                     ALPHA(NA)=1.0_DP*ATAN(GRAD(NA))
                     DALPHA=-ALPHA(NA+1)+PAR%DGRAD*PI

                     IF(DALPHA>1.8_DP*PI)THEN
                       UPE=TFP(NA+1)%E+PAR%EGAP
                     ELSE
                       UPE=TFP(NA+1)%E+TAN(DALPHA)
                     END IF
                     UPE=MIN(UPE,PAR%WDIF(2))

                     DALPHA=-ALPHA(NA+1)-PAR%DGRAD*PI
                     IF(ABS(DALPHA)>1.8_DP*PI)THEN
                       DOWNE=TFP(NA+1)%E-PAR%EGAP
                     ELSE
                       DOWNE=TFP(NA+1)%E+TAN(DALPHA)
                     END IF
                     DOWNE=MAX(DOWNE,PAR%WDIF(1))

                     IF (( ABS(TFP(NA+1)%E-EPOINT%E) <= PAR%EGAP ).AND. &
                       & (EPOINT%E<UPE).AND.(EPOINT%E>DOWNE).AND. &
                     & (ABS(EPOINT%W-TFP(NA+1)%W)<=PAR%DBLOCH).AND. &
                     & (ORBIT_DIF(EPOINT%OC(:),TFP(NA+1)%OC(:))))THEN
                        EXIT
                     ELSE
                       LPOS(KK-1,NJ)=.FALSE.
                       CYCLE
                     END IF
                   END IF
                 END DO
                 LPOS=POSTEMP
                 IF ((.NOT.LEXIST2).OR.(EPOINT%PATH/=P)) EXIT
                 KK = KK-1
                 TFP(NA) = EPOINT
                 LPOS(KK,NJ)=.FALSE.
                 HITNUM(NA,:)=(/KK,NJ/)
               END DO

               KK=0
               DO NA=1,NFP
                 IF(TFP(NA)%LOCU)KK=KK+1
               END DO
               IF(KK>PAR%POINTS)THEN
                 LCROS=.TRUE.
                 EXIT samek
               ELSE
                 LCROS=.FALSE.
                 HITNUM=0
                 CALL ZERO(TFP)
                 LPOS=POSTEMP
                 CYCLE samek
               END IF
             ELSE
               LPOS(K+1,NJ)=.FALSE.
               CYCLE
             END IF !Überprüfung ob Schnittpunkt
           END IF !LEXIST
         END DO
         LPOS=POSTEMP
       END IF !LPOS
     END DO samek

     IF (LCROS) THEN
       LCROS=.FALSE.
        NCROS = NCROS + 1
        FORALL( NA = 1:NFP) STATFP(NCROS,NA) = TFP(NA)
        DO NA=1,NFP
          IF(HITNUM(NA,1)>0) POSTEMP(HITNUM(NA,1), &
          &        HITNUM(NA,2))=.FALSE.
        END DO
        LFIT=.TRUE.
     END IF
     CALL ZERO(TFP)
     LPOS=POSTEMP
     HITNUM=0
     K=K+1
   END DO kpoints
 END DO
 ALLOCATE(FP(NCROS,NFP))
 CALL SETUP_ORBIT(FP,N%ORB)
 CALL ZERO(FP)
 FORALL(I=1:NCROS, J=1:NFP ) FP(I,J) = STATFP(I,J)
 N%FIT(SPN)=NCROS

 DEALLOCATE(TFP,STATFP)

END SUBROUTINE FINDFITTPOINTS




SUBROUTINE WRITE_FITTPOINTS(FP,FPFILE)
 TYPE( BAND ), DIMENSION(:,:), INTENT(IN) :: FP
 CHARACTER(LEN=*) :: FPFILE
 INTEGER :: I,J,FORB
 FORB=PAR%FORB
 IF(PAR%FORB<1)FORB=1
 OPEN(UNIT=13, FILE=TRIM(FPFILE), STATUS='REPLACE', ACTION='WRITE')
   DO I=1,SIZE(FP,1)
     DO J=1,SIZE(FP,2)
       IF(FP(I,J)%LOCU) WRITE(13,'(4F14.8,1X,I4)') &
       &  FP(I,J)%K,FP(I,J)%E,FP(I,J)%W,FP(I,J)%OC(FORB)
     END DO
   END DO
 CLOSE(13)

END SUBROUTINE WRITE_FITTPOINTS



!***************************************************************************
!***  This Subroutine looks for the next Neighbour to position 'K' *********
!***  with the value 'Energy' and position 'K' and saves it in 'NEBS' ******
!***  LEXIST = .TRUE. >> It founds a neighbour on position 'NK' ************
!***  If K is negative it looks on the left hand side **********************
!***************************************************************************

SUBROUTINE NEIGHBOUR(ENERGY,K,BND,LEXIST,NEBS,NJ,SPN)
 TYPE( BAND ) :: NEBS
 REAL(KIND=DP) :: ENERGY
 INTEGER :: K, NK, J, NJ, NKO,BND,SPN
 LOGICAL :: LEXIST, FIRST
  FIRST=.TRUE.
  LEXIST=.FALSE.
  NK=ABS(K+1)
  IF (( NK > N%TKPTS ).OR.( NK < 1 )) RETURN
  DO J=1,N%SAMEK
    IF(LPOS(NK,J))THEN
      IF((.NOT.PAR%BAND).AND.(BND/=EBS(NK,J,SPN)%BND))CYCLE
      IF (FIRST) THEN
        NEBS=EBS(NK,J,SPN)
        NJ=J
        LEXIST=.TRUE.
        FIRST=.FALSE.
      ELSE
        IF (ABS(ENERGY-EBS(NK,J,SPN)%E)<ABS(ENERGY-NEBS%E)) THEN
          NEBS=EBS(NK,J,SPN)
          NJ=J
        END IF
      END IF
    END IF
  END DO
  IF (LEXIST) THEN
    IF(ABS(ENERGY-NEBS%E)<=PAR%EGAP) RETURN
    LEXIST=.FALSE.
  END IF
 
END SUBROUTINE NEIGHBOUR



SUBROUTINE LINEAR_FIT( FP, ROOTS, PLOTFILE, SPN )
 IMPLICIT NONE
 TYPE( BAND ), DIMENSION(:,:), INTENT(INOUT) :: FP
 TYPE( BAND ), DIMENSION(:), ALLOCATABLE :: TEMPROOTS,ROOTS
 REAL(KIND=DP), DIMENSION(:), INTENT(IN) :: GRID
 INTEGER :: I,J, T, KA, NP, NTFIT,SPN
 REAL(DP) :: XM,YM,NORM,VECPRO,DIST,PDIST,DSUM,KMIN,KMAX
 REAL(DP), DIMENSION(2) :: ALPHA, SWEIGHT
 REAL(DP), DIMENSION(2,N%ORB) :: SOC
 REAL(DP), DIMENSION(2,2) :: PLOT
 CHARACTER(LEN=*) :: PLOTFILE
 LOGICAL :: LFIRST
 NP=SIZE(FP,2)
 ALLOCATE(TEMPROOTS(N%FIT(SPN)))
 CALL SETUP_ORBIT(TEMPROOTS,N%ORB)
 CALL ZERO(TEMPROOTS)
 NTFIT=0
 OPEN(UNIT=17, FILE=TRIM(PLOTFILE), STATUS='REPLACE', ACTION='WRITE')
  DO KA = 1,N%FIT(SPN)
    VECPRO = 0.0D0
    NORM = 0.0D0
    XM = 0.0D0
    YM = 0.0D0
    T = 0
    LFIRST=.TRUE.
    DO I=1,NP
      IF (.NOT.FP(KA,I)%LOCU) CYCLE
      IF(LFIRST)THEN
        KMIN=FP(KA,I)%K
        KMAX=FP(KA,I)%K
        LFIRST=.FALSE.
      ELSE
        KMIN=MIN(KMIN,FP(KA,I)%K)
        KMAX=MAX(KMAX,FP(KA,I)%K)
      END IF
      VECPRO = VECPRO + FP(KA,I)%K*FP(KA,I)%E
      NORM = NORM + FP(KA,I)%K*FP(KA,I)%K
      XM = XM + FP(KA,I)%K
      YM = YM + FP(KA,I)%E
      T = T + 1
    END DO
    XM = XM/(1.0_DP*T)
    YM = YM/(1.0_DP*T)

    ALPHA(2)=(VECPRO-T*XM*YM)/(NORM-T*(XM**2))
    ALPHA(1)=YM-ALPHA(2)*XM

    TEMPROOTS(KA)%K=-ALPHA(1)/ALPHA(2)
    IF((TEMPROOTS(KA)%K<KMIN).OR.(TEMPROOTS(KA)%K>KMAX))THEN
      FORALL(I=1:NP)FP(KA,I)%LOCU=.FALSE. 
      CYCLE
    END IF
    TEMPROOTS(KA)%LOCU =  .TRUE.

!Get the weighted bloch and orbital average
    DIST=0.0_DP
    DO I=1,NP-1
      IF ((.NOT.FP(KA,I)%LOCU).OR.(.NOT.FP(KA,I+1)%LOCU)) CYCLE
      PDIST=SQRT((FP(KA,I)%K-FP(KA,I+1)%K)**2+(FP(KA,I)%E-FP(KA,I+1)%E)**2)
      DIST=DIST+PDIST
    END DO
    DSUM=0.0_DP
    SWEIGHT=0.0_DP
    SOC=0.0_DP
    DO I=1,NP
      IF (.NOT.FP(KA,I)%LOCU) CYCLE
      PDIST=SQRT((TEMPROOTS(KA)%K-FP(KA,I)%K)**2+FP(KA,I)%E**2)
      DSUM=DSUM+PDIST
      SWEIGHT(1)=SWEIGHT(1)+PDIST*FP(KA,I)%W
      SWEIGHT(2)=SWEIGHT(2)+DIST*FP(KA,I)%W
      DO J=1,N%ORB
        SOC(1,J)=SOC(1,J)+PDIST*FP(KA,I)%OC(J)
        SOC(2,J)=SOC(2,J)+DIST*FP(KA,I)%OC(J)
      END DO

    END DO
    FORALL(J=1:N%ORB) TEMPROOTS(KA)%OC(J)=SOC(2,J)-SOC(1,J)
    TEMPROOTS(KA)%W=SWEIGHT(2)-SWEIGHT(1)
    DSUM=T*DIST-DSUM

    FORALL(J=1:N%ORB) TEMPROOTS(KA)%OC(J)=TEMPROOTS(KA)%OC(J)/DSUM
    TEMPROOTS(KA)%W=TEMPROOTS(KA)%W/DSUM


!Get the outher points which where used for the linear regression
    DO I=1,NP
      IF (FP(KA,I)%LOCU) THEN
        PLOT(1,1)=(FP(KA,I)%E-ALPHA(1))/ALPHA(2)
        PLOT(1,2)=FP(KA,I)%E
        TEMPROOTS(KA)%PATH = FP(KA,I)%PATH
        EXIT
      END IF
    END DO

    DO I=NP,1,-1
      IF (FP(KA,I)%LOCU) THEN
        PLOT(2,1)=(FP(KA,I)%E-ALPHA(1))/ALPHA(2)
        PLOT(2,2)=FP(KA,I)%E
        EXIT
      END IF
    END DO

    IF (ALPHA(2)>=0)THEN
      PLOT(1,2)=MAX(MINVAL(FP(KA,:)%E),PAR%WDIF(1))
      PLOT(2,2)=MIN(MAXVAL(FP(KA,:)%E),PAR%WDIF(2))
    ELSE
      PLOT(1,2)=MIN(MAXVAL(FP(KA,:)%E),PAR%WDIF(2))
      PLOT(2,2)=MAX(MINVAL(FP(KA,:)%E),PAR%WDIF(1))
    END IF
    PLOT(1,1)=(PLOT(1,2)-ALPHA(1))/ALPHA(2)
    PLOT(2,1)=(PLOT(2,2)-ALPHA(1))/ALPHA(2)
    WRITE(17,208) PLOT(1,:)
    WRITE(17,208) PLOT(2,:)
    208 FORMAT(2(F19.16,1X))
    WRITE(17,*) ""
    WRITE(17,*) ""
    NTFIT=NTFIT+1
  END DO
 CLOSE(17)
 ALLOCATE(ROOTS(N%FIT(SPN)))
 CALL SETUP_ORBIT(ROOTS,N%ORB)
 CALL ZERO(ROOTS)
 KA=1
 DO I=1,N%FIT(SPN)
   IF(TEMPROOTS(I)%LOCU)THEN
     ROOTS(KA)=TEMPROOTS(I)
     KA=KA+1
   END IF
 END DO
 N%FIT(SPN)=NTFIT
 DEALLOCATE(TEMPROOTS)
END SUBROUTINE LINEAR_FIT



SUBROUTINE POLYNOM_INTERPOL_ROOTS( FP, ROOTS, PLOTFILE, KGRID, SPN)
 IMPLICIT NONE
 TYPE( BAND ), DIMENSION(:,:), INTENT(INOUT) :: FP
 TYPE( BAND ), DIMENSION(:), ALLOCATABLE :: ROOTS
 TYPE( GRID ), DIMENSION(N%PATH+1) :: KGRID
 CHARACTER(LEN=*) :: PLOTFILE
 INTEGER :: I, J, L, S,P, KA, NP, N0,NOCU, NFIT, DFIT, SPN, NTFIT, D=15
 REAL(DP) :: X,X1,X2, DX, F, FO, DSUM, DIST, PDIST
 REAL(DP), DIMENSION(2) :: SWEIGHT
 REAL(DP), DIMENSION(2,N%ORB) :: SOC
 REAL(DP), DIMENSION(:), ALLOCATABLE :: A,OCHAR
 REAL(DP), DIMENSION(:,:), ALLOCATABLE :: AN,XY,X12
 INTEGER, DIMENSION(:), ALLOCATABLE :: MAP,CON
 LOGICAL :: FIRST
 LOGICAL, DIMENSION(SIZE(FP,1)) :: LFP
 NFIT=0
 DFIT=SIZE(FP,1)
 NP=SIZE(FP,2)


 ALLOCATE(AN(DFIT,NP),X12(NP*DFIT,2),MAP(NP*DFIT),OCHAR(N%ORB),CON(N%ORB))
 OPEN(UNIT=16, FILE=TRIM(PLOTFILE), STATUS='REPLACE', ACTION='WRITE')
  DO KA = 1,DFIT
    NOCU=0
    DO I=1,NP
      IF(FP(KA,I)%LOCU)NOCU=NOCU+1
    END DO
    ALLOCATE(XY(NOCU,2))
    NOCU=0
    DO I=1,NP
      IF(FP(KA,I)%LOCU)THEN
        NOCU=NOCU+1
        XY(NOCU,:)=(/ FP(KA,I)%K, FP(KA,I)%E /)
      END IF
    END DO
    DO I=1,NP
      IF(FP(KA,I)%LOCU)THEN
        X=FP(KA,I)%K
        EXIT
      END IF
    END DO
    DO I=NP,1,-1
      IF(FP(KA,I)%LOCU)THEN
        DX=(MAXVAL(FP(KA,:)%K)-X)/(1.0_DP*D-1.0_DP)
      END IF
    END DO
    CALL GAUS_INTERPOL(XY,A)
    DEALLOCATE(XY)
    FORALL(I=1:SIZE(A)) AN(KA,I)=A(I)
    F=0.0_DP
    N0=0
    FIRST=.TRUE.
    I=1
    DO
      F=POLYNOM(A,X)
      IF((F<=PAR%WDIF(1)).AND.( F>=PAR%WDIF(2)))THEN
        X=X+DX
        DX=DX*MAX(REAL(D-I),1.0)/REAL(D)
        IF(FIRST)THEN
          CYCLE
        ELSE
          EXIT
        END IF
      END IF
      FIRST=.FALSE.
      WRITE(16,'(2f14.8,1X)') X,F
      IF(I>1)THEN
        IF(F*FO<0.0D0)THEN
          N0=N0+1
          X12(NFIT+N0,1)=X
          X12(NFIT+N0,2)=X-DX
          MAP(NFIT+N0)=KA
        END IF
      END IF
      FO=F
      X=X+DX
      IF(I==D)EXIT
      I=I+1
    END DO
    WRITE(16,*) ""
    WRITE(16,*) ""
    NFIT=NFIT+N0
    DEALLOCATE(A)
  END DO

  !--------------------------------
  !Get the coordinates of the roots
  !and calculate the values of them
  !--------------------------------
  ALLOCATE(ROOTS(NFIT),A(NOCU))
  CALL SETUP_ORBIT(ROOTS,N%ORB)
  CALL ZERO(ROOTS)
  NTFIT=0
  DO I=1,NFIT
    X1=X12(I,1)
    X2=X12(I,2)
    FORALL(J=1:SIZE(A)) A(J)=AN(MAP(I),J)
    ROOTS(I)%K=REGULAR_FALSI(A,X1,X2,PAR%EPS)
    DO J=1,NP
      IF (FP(MAP(I),J)%LOCU)THEN
        P = FP(MAP(I),J)%PATH
        EXIT
      END IF
    END DO
    IF(P==1)THEN
      DIST=0.0_DP
    ELSE
      DIST=KGRID(P-1)%DIST
    END IF
    IF((ROOTS(I)%K<DIST).OR.(ROOTS(I)%K>KGRID(P)%DIST))CYCLE
    ROOTS(I)%LOCU=.TRUE.
    ROOTS(I)%PATH=P
 !Calculate the average bloch- and orbitalcharacter
    FIRST=.TRUE.
    DO J=1,NP
      IF (.NOT.FP(MAP(I),J)%LOCU) CYCLE
      IF(FIRST)THEN
        X1=FP(MAP(I),J)%K
        FIRST=.FALSE.
      END IF
      X2=FP(MAP(I),J)%K
    END DO

!Get the weighted bloch and orbital average
    DIST=POLYNOM_DISTANCE(A,X1,X2,D)
    DSUM=0.0_DP
    SWEIGHT=0.0_DP
    SOC=0.0_DP
    S=0
    DO J=1,NP
      IF (.NOT.FP(MAP(I),J)%LOCU) CYCLE
      PDIST=POLYNOM_DISTANCE(A,ROOTS(I)%K,FP(MAP(I),J)%K,D)
      DSUM=DSUM+PDIST
      SWEIGHT(1)=SWEIGHT(1)+PDIST*FP(MAP(I),J)%W
      SWEIGHT(2)=SWEIGHT(2)+DIST*FP(MAP(I),J)%W
      DO KA=1,N%ORB
        SOC(1,KA)=SOC(1,KA)+PDIST*FP(MAP(I),J)%OC(KA)
        SOC(2,KA)=SOC(2,KA)+DIST*FP(MAP(I),J)%OC(KA)
      END DO
      S=S+1
    END DO
    FORALL(J=1:N%ORB) ROOTS(I)%OC(J)=SOC(2,J)-SOC(1,J)
    ROOTS(I)%W=SWEIGHT(2)-SWEIGHT(1)
    DSUM=S*DIST-DSUM

    FORALL(J=1:N%ORB) ROOTS(I)%OC(J)=ROOTS(I)%OC(J)/DSUM
    ROOTS(I)%W=ROOTS(I)%W/DSUM
    NTFIT=NTFIT+1
  END DO

 CLOSE(16)
 N%FIT(SPN)=NTFIT
 LFP=.FALSE.
 DO I=1,NFIT
   IF(.NOT.ROOTS(I)%LOCU)CYCLE
   LFP(MAP(I))=.TRUE.
 END DO

 DO I=1,DFIT
   IF(.NOT.LFP(I))FORALL(J=1:NP) FP(I,J)%LOCU=.FALSE.
 END DO

END SUBROUTINE POLYNOM_INTERPOL_ROOTS





SUBROUTINE WRITE_FERMIFILE(FERMIFILE,ROOTS,ROOTSXYZ,KGRID,LATTICE,FILETYPE)
  CHARACTER(LEN=*) :: FERMIFILE
  TYPE( BAND ), DIMENSION(:), INTENT(IN) :: ROOTS
  TYPE( GRID ), DIMENSION(N%PATH+1) :: KGRID
  TYPE( LATT ) :: LATTICE
  REAL(DP) :: TDIST,DIST1
  REAL(DP),DIMENSION(3) :: CAR,VEC
  REAL(DP),DIMENSION(:,:), ALLOCATABLE :: ROOTSXYZ
  INTEGER :: I,J,D,FILETYPE
  OPEN(UNIT=851, FILE=TRIM(FERMIFILE)//'.temp', STATUS='REPLACE', ACTION='WRITE')

   WRITE(851,'(A)',ADVANCE='NO') '#FERMIROOTS:: considered '
   IF(PAR%KSKIP(2)>0)THEN
     IF(PAR%KSKIP(1)>0)THEN
       WRITE(851,'(A,I5,A,I5,A,I5)',ADVANCE='NO') 'k-points 1-',PAR%KSKIP(1)-1,' and k-points ', &
            &   PAR%KSKIP(2)+1,'-',MAXVAL(NFILE(:)%KPTS)
     ELSE
       WRITE(851,'(A,I5,A,I5)',ADVANCE='NO') 'k-points ',PAR%KSKIP(2)+1,'-', MAXVAL(NFILE(:)%KPTS)
     END IF
   ELSE
     WRITE(851,'(A)',ADVANCE='NO') 'all k-points'
   END IF

  IF(PAR%SION(1)>0)THEN
    IF(PAR%SION(2)>0)THEN
      WRITE(851,'(A,I5,A,I5)') ' and IONS ',PAR%SION(1),'-',PAR%SION(2)
    ELSE
      WRITE(851,'(A,I5)') ' and ION ',PAR%SION(1)
    END IF
  ELSE
     WRITE(851,'(A)') ' and all IONS'
  END IF


   WRITE(851,'(A)') '#1-3| x,y,z cartesian coordinates'
   WRITE(851,'(A)') '# 4 | Summed k-distance EBS roots'
   WRITE(851,'(A)') '# 5 | k-distance to the left pathpoint'
   WRITE(851,'(A)') '# 6 | average Bloch character'
   IF(FILETYPE/=3)THEN
     IF(PAR%FORB<1)THEN
       DO I=1,N%ORB
         WRITE(851,'(A1,I2,A)') '#',I+6,' | average orbital character '//TRIM(PAR%ORBNAMES(I))
       END DO
       WRITE(851,'(A1,I2,A)') '#',N%ORB+7,' | pathnumber'
     ELSE
       WRITE(851,'(A)') '# 7 | average orbital character '//TRIM(PAR%ORBNAMES(PAR%FORB))
       WRITE(851,'(A)') '# 8 | pathnumber'
     END IF
   ELSE
     WRITE(851,'(A)') '# 7 | pathnumber'
   END IF
   D=SIZE(ROOTS)
   IF(ALLOCATED(ROOTSXYZ))DEALLOCATE(ROOTSXYZ)
   ALLOCATE(ROOTSXYZ(D,3))
   DO I=1,D
     IF(.NOT.ROOTS(I)%LOCU)CYCLE
     IF(ROOTS(I)%PATH>1)THEN
       DIST1=ABS(KGRID(ROOTS(I)%PATH-1)%DIST-ROOTS(I)%K)
     ELSE
       DIST1=ROOTS(I)%K
     END IF
     TDIST=KDISTANCE(KGRID(ROOTS(I)%PATH)%A,KGRID(ROOTS(I)%PATH)%B,LATTICE%B)
     VEC=KGRID(ROOTS(I)%PATH)%B-KGRID(ROOTS(I)%PATH)%A
     VEC=KGRID(ROOTS(I)%PATH)%A+(DIST1/TDIST)*VEC
     CALL CHANGE_BASIS(LATTICE%B,VEC,CAR)
     ROOTSXYZ(I,:)=CAR(:)
     IF(FILETYPE/=3)THEN
       WRITE(851,108,ADVANCE='NO') CAR,ROOTS(I)%K,DIST1,ROOTS(I)%W
       108 FORMAT(4F18.12,2(F16.12,1X))
       IF(PAR%FORB<1)THEN
         DO J=1,N%ORB
           WRITE(851,'(F16.12,1X)',ADVANCE='NO') ROOTS(I)%OC(J)
         END DO
       ELSE
         WRITE(851,'(F16.12,1X)',ADVANCE='NO') ROOTS(I)%OC(PAR%FORB)
       END IF
       WRITE(851,'(I6)') ROOTS(I)%PATH
     ELSE
       WRITE(851,109) CAR,ROOTS(I)%K,DIST1,ROOTS(I)%W,ROOTS(I)%PATH
       109 FORMAT(4F18.12,2(F16.12,1X),I6)
     END IF
   END DO
  CLOSE(851)


END SUBROUTINE WRITE_FERMIFILE



SUBROUTINE ENERGYGAP( TEBS, EGAP, SEGAP, FFILE, KGRID, SPN )
  TYPE( BAND ),DIMENSION(:),INTENT(IN) :: TEBS
  TYPE( GRID ), DIMENSION(N%PATH) :: KGRID
  REAL(DP), DIMENSION(2) :: EGAP, SEGAP
  REAL(DP) :: EDELTA,EMAX,EMIN
  CHARACTER(LEN=*) :: FFILE
  INTEGER :: I,J,SPN
  LOGICAL :: FIRST=.TRUE.

  DO I=1,SIZE(TEBS,1)
    IF((.NOT.TEBS(I)%LOCU).OR.(TEBS(I)%SPN/=SPN))CYCLE
    IF(FIRST)THEN
       EMAX=TEBS(I)%E
       EMIN=TEBS(I)%E
       FIRST=.FALSE.
    ELSE
      IF(TEBS(I)%E>EMAX)EMAX=TEBS(I)%E
      IF(TEBS(I)%E<EMIN)EMIN=TEBS(I)%E
    END IF
  END DO
  IF(N%LEPMIN(SPN))THEN
    EGAP(1)=N%EPMIN(SPN)
  ELSE
    EGAP(1)=MAX(EMAX,0.0_DP)
  END IF
  IF(N%LENMAX(SPN))THEN
    EGAP(2)=N%ENMAX(SPN)
  ELSE
    EGAP(2)=MIN(EMIN,0.0_DP)
  END IF

  DO I=1,SIZE(TEBS,1)
    IF((.NOT.TEBS(I)%LOCU).OR.(TEBS(I)%SPN/=SPN))CYCLE
    IF(TEBS(I)%E>=0.0_DP)THEN
      IF(TEBS(I)%E<EGAP(1))EGAP(1)=TEBS(I)%E
    ELSE
      IF(TEBS(I)%E>EGAP(2))EGAP(2)=TEBS(I)%E
    END IF
  END DO
  IF(N%LEPMIN(SPN))THEN
    SEGAP(1)=N%EPMIN(SPN)
  ELSE
    SEGAP(1)=MAX(MAXVAL(EBS(:,:,SPN)%E),0.0_DP)
  END IF

  IF(N%LENMAX(SPN))THEN
    SEGAP(2)=N%ENMAX(SPN)
  ELSE
    SEGAP(2)=MIN(MINVAL(EBS(:,:,SPN)%E),0.0_DP)
  END IF

  DO I=1,SIZE(EBS,1)
    DO J=1,SIZE(EBS,2)
      IF(.NOT.EBS(I,J,SPN)%LOCU)CYCLE
      IF(EBS(I,J,SPN)%E>=0.0_DP)THEN
        IF(EBS(I,J,SPN)%E<SEGAP(1))SEGAP(1)=EBS(I,J,SPN)%E
      ELSE
        IF(EBS(I,J,SPN)%E>SEGAP(2))SEGAP(2)=EBS(I,J,SPN)%E
      END IF
    END DO
  END DO


  OPEN(UNIT=85, FILE=TRIM(FFILE), STATUS='REPLACE', ACTION='WRITE')
    WRITE(85,'(F14.8)') KGRID(N%PATH)%DIST
    WRITE(85,'(F14.8)') 0.4*KGRID(N%PATH)%DIST
    WRITE(85,'(F14.8)') EGAP(1)
    WRITE(85,'(F14.8)') EGAP(2)
    EDELTA=EGAP(1)-EGAP(2)
    IF(EDELTA<1.0_DP)THEN
      WRITE(85,'(F6.4)') EDELTA
    ELSE
      WRITE(85,'(F0.4)') EDELTA
    END IF
    WRITE(85,'(F14.8)') MAX(PAR%FBORDER(1),EGAP(2))+0.5_DP*(MIN(PAR%FBORDER(2),EGAP(1))-MAX(PAR%FBORDER(1),EGAP(2)))
    WRITE(85,'(F14.8)') SEGAP(1)
    WRITE(85,'(F14.8)') SEGAP(2)
    EDELTA=SEGAP(1)-SEGAP(2)
    IF(EDELTA<1.0_DP)THEN
      WRITE(85,'(F6.4)') EDELTA
    ELSE
      WRITE(85,'(F0.4)') EDELTA
    END IF
    WRITE(85,'(F14.8)') MAX(PAR%WDIF(1),SEGAP(2))+0.5_DP*(MIN(PAR%WDIF(2),SEGAP(1))-MAX(PAR%WDIF(1),SEGAP(2)))

  CLOSE(85)
END SUBROUTINE ENERGYGAP




SUBROUTINE WRITE_GRID(OUTGRID,KGRID)
  CHARACTER(LEN=*) :: OUTGRID
  TYPE( GRID ), DIMENSION(N%PATH) :: KGRID
  REAL(DP) :: DIST1, DIST2, DISTANCE
  INTEGER :: I,J

  DIST1=1.2_DP*(PAR%FBORDER(2)-PAR%FBORDER(1))
  DIST2=DIST1/4.0_DP
  DIST1=PAR%FBORDER(1)-0.2_DP*ABS(PAR%FBORDER(1))
  OPEN(UNIT=95, FILE=TRIM(OUTGRID), STATUS='REPLACE', ACTION='WRITE')
   WRITE(95,*) '#KPOINTgrid ',N%PATH+1
   WRITE(95,'(A1,I1,1X,F0.8)') '#',1,0.0_DP
   DO I=1,N%PATH
     SELECT CASE(I+1)
       CASE(:9)
         WRITE(95,'(A1,I1,1X,F0.8)') '#',I+1,KGRID(I)%DIST
       CASE(10:99)
         WRITE(95,'(A1,I2,1X,F0.8)') '#',I+1,KGRID(I)%DIST
       CASE(100:999)
         WRITE(95,'(A1,I3,1X,F0.8)') '#',I+1,KGRID(I)%DIST
       CASE(1000:9999)
         WRITE(95,'(A1,I4,1X,F0.8)') '#',I+1,KGRID(I)%DIST
       CASE(10000:99999)
         WRITE(95,'(A1,I5,1X,F0.8)') '#',I+1,KGRID(I)%DIST
       CASE(100000:999999)
         WRITE(95,'(A1,I6,1X,F0.8)') '#',I+1,KGRID(I)%DIST
     END SELECT
   END DO
   DO I=1,N%PATH
     WRITE(95,'(A5,7F17.12)') '#KVEC',KGRID(I)%DIST,KGRID(I)%A,KGRID(I)%B
   END DO
   DO I=1,N%PATH
     DISTANCE=DIST1
     WRITE(95,*) ""
     WRITE(95,*) ""
     DO J=1,9
       WRITE(95,'(F0.8,1X,F6.2)') KGRID(I)%DIST,DISTANCE
       DISTANCE = DISTANCE + DIST2
     END DO
   END DO
 CLOSE(95)
END SUBROUTINE WRITE_GRID










!##############################################################################
!############################### Fermisurface #################################
!##############################################################################


SUBROUTINE FERMISURFACE(TEBS,OROOTS,ROOTSXYZ,KGRID,LATTICE,BANDSPEC,FERMIFILE,SPECFILE,FILETYPE,SPN)
 TYPE( BAND ), DIMENSION(:), INTENT(IN) :: TEBS,OROOTS
 TYPE( BAND ), DIMENSION(:), ALLOCATABLE :: ROOTS
 TYPE( LATT ) :: LATTICE
 TYPE( GRID ),DIMENSION(:),INTENT(IN) :: KGRID
 TYPE( GRID ),DIMENSION(:),ALLOCATABLE :: CGRID
 TYPE( SYMMETRY ) :: SYM
 CHARACTER(LEN=*) :: FERMIFILE,SPECFILE,BANDSPEC
 CHARACTER(LEN=60) :: FFILE,SFILE,BFILE,SPM3DFILE,EMPTYORBS
 CHARACTER(LEN=11) :: OCHAR
 CHARACTER(LEN=6) :: CNUM
 REAL(DP),PARAMETER :: THIN=0.0000001_DP,PI=2.0_DP*ATAN(1.0)
 REAL(DP), DIMENSION(:,:), INTENT(IN) :: ROOTSXYZ
 REAL(DP), DIMENSION(:,:), ALLOCATABLE :: FERMIXY,FSPECXY,FSU
 REAL(DP) :: FXY0,WSUM,OSUM,MAXORB
 INTEGER :: XRES, YRES, PROG, HPROG,SPN,EOPOS,EOLEN
 INTEGER, DIMENSION(3) :: VEC
 REAL(DP),DIMENSION(3,3) :: PX,PXNORM,PXINV,LWORK,BINV
 REAL(DP),DIMENSION(4,3) :: KCORNERS
 REAL(DP),DIMENSION(3) :: ORIGIN,VEC0,NVEC,RNVEC,VEC1,VEC2,AREAL,BREAL
 REAL(DP),DIMENSION(2) :: SVEC,FXY,XBORDER,YBORDER,NXBORDER,NYBORDER
 REAL(DP) :: ANGLE,OANGLE,XDIST,XMAX,YDIST,YMAX,D,AS,AO,ZMIN,ZMAX
 INTEGER :: I,J,K,KPT,P,RN,SIGN1,SIGN2,FILETYPE,CON,IOCHECK,ONUM,OEND
 LOGICAL,DIMENSION(N%ORB) :: NOORB
 LOGICAL :: OFIRST,LASTPROG


 LASTPROG=.TRUE.
 IF((PAR%SION(1)<1).AND.(PAR%SION(2)<1))THEN
     OEND=N%ORB-1
 ELSE
     OEND=N%ORB
 END IF


!********************* setup Filenames ****************
 IF(PAR%SPN==2)THEN
   WRITE(CNUM,'(A5,I1)') '.spin',SPN
   FFILE=TRIM(FERMIFILE)//CNUM//'.temp'
   SFILE=TRIM(SPECFILE)//CNUM//'.temp'
   SPM3DFILE=TRIM(SPECFILE)//CNUM//'.pm3d.dat'
   BFILE=TRIM(BANDSPEC)//CNUM//'.dat'
 ELSE
   FFILE=TRIM(FERMIFILE)//'.temp'
   SFILE=TRIM(SPECFILE)//'.temp'
   SPM3DFILE=TRIM(SPECFILE)//'.pm3d.dat'
   BFILE=TRIM(BANDSPEC)//'.dat'
 END IF

 WRITE(*,'(A)') '            start projection on the fermisurface'
 IF(PAR%SPECFUN)THEN
   IF(PAR%SLIMSPEC)THEN
     HPROG=2+N%PATH+PAR%IREFLECT*(N%FIT(SPN)+N%SPEC(SPN))
   ELSE
     HPROG=2+N%PATH+PAR%IREFLECT*(N%FIT(SPN)+2*N%SPEC(SPN))
   END IF
 ELSE
   HPROG=2+N%PATH+PAR%IREFLECT*N%FIT(SPN)
 END IF
 CALL WRITE_PROGRESS(1,HPROG)
 NOORB(:)=.TRUE.
 RN=SIZE(OROOTS,1)

 !********** Check all Orbitals **********************
 IF(N%FIT(SPN)>0)THEN
 ALLOCATE(ROOTS(N%FIT(SPN)))
 P=1
 ONUM=0
 MAXORB=0.0_DP
 DO I=1,RN
  IF(OROOTS(I)%LOCU)THEN
    ROOTS(P)=OROOTS(I)
    IF(FILETYPE/=3)THEN
      IF(PAR%FORB==0)THEN
        DO J=1,OEND
          MAXORB=MAX(MAXORB,OROOTS(I)%OC(J))
          IF(OROOTS(I)%OC(J)>0.01_DP)THEN
            NOORB(J)=.FALSE.
            ONUM=ONUM+1
          END IF
        END DO
      ELSE
        MAXORB=MAX(MAXORB,OROOTS(I)%OC(PAR%FORB))
        IF(OROOTS(I)%OC(PAR%FORB)>0.01_DP) NOORB(PAR%FORB)=.FALSE.
      END IF
    END IF
    P=P+1
  END IF
 END DO
 RN=N%FIT(SPN)
 ALLOCATE(FERMIXY(RN,2))
 END IF
 XDIST=0.0_DP
 XBORDER=0.0_DP
 YBORDER=0.0_DP




!============ figure out the basisvecors for the surface ===============

ALLOCATE(CGRID(N%PATH))
 XMAX=0.0_DP
 YMAX=0.0_DP
 PX=0.0_DP
 DO P=1,N%PATH
   CALL WRITE_PROGRESS(1+P,HPROG)
   CALL CHANGE_BASIS(LATTICE%B,KGRID(P)%A,AREAL)
   CALL CHANGE_BASIS(LATTICE%B,KGRID(P)%B,BREAL)
   CGRID(P)%A=AREAL
   CGRID(P)%B=BREAL
   DO K=1,N%PATH
     IF(P==K)CYCLE
     YDIST=KDISTANCE(KGRID(P)%A,KGRID(K)%A,LATTICE%B)
     IF((YDIST>THIN).AND.(YDIST>XMAX))THEN
       XMAX=YDIST
       PX(:,2)=KGRID(K)%A-KGRID(P)%A
       ORIGIN=KGRID(P)%A
     END IF
   END DO
 END DO


 PROG=1+N%PATH


 CALL CHANGE_BASIS(LATTICE%B,PX(:,2),BREAL)
 PX(:,2)=BREAL
 PX(:,1)=CGRID(1)%B-CGRID(1)%A
 CALL CHANGE_BASIS(LATTICE%B,ORIGIN,AREAL)
 ORIGIN=AREAL

 DO I=1,3
   IF(ORIGIN(I)<THIN)ORIGIN(I)=0.0_DP
 END DO



 IF(XMAX<THIN)THEN
   VEC1=PX(:,1)
   VEC2=CGRID(N%PATH)%B-CGRID(N%PATH)%A
   D=SQRT(DDOT_PRODUCT(VEC1,VEC1)*DDOT_PRODUCT(VEC2,VEC2))
   OANGLE=ACOS(DDOT_PRODUCT(VEC1,VEC2)/D)
   VEC0=VEC2
   DO P=2,N%PATH-1
     VEC2=CGRID(P)%B-CGRID(P)%A
     D=SQRT(DDOT_PRODUCT(VEC1,VEC1)*DDOT_PRODUCT(VEC2,VEC2))
     ANGLE=ACOS(DDOT_PRODUCT(VEC1,VEC2)/D)
     IF(ABS(PI-ANGLE)<ABS(PI-OANGLE))THEN
       OANGLE=ANGLE
       VEC0=VEC2
     END IF
   END DO
   PX(:,1)=VEC1
   PX(:,2)=VEC0
   ORIGIN=CGRID(1)%A
 END IF

 VEC1=PX(:,1)
 VEC2=PX(:,2)
 
 PX(:,1)=VEC1(:)/SQRT(DDOT_PRODUCT(VEC1,VEC1))
 PX(:,2)=VEC2(:)/SQRT(DDOT_PRODUCT(VEC2,VEC2))

 CALL EXPRO(RNVEC,PX(:,1),PX(:,2))

 PX(:,3)=RNVEC(:)/SQRT(DDOT_PRODUCT(RNVEC,RNVEC))

 CALL MAKE_ORTHONORMAL_BASIS(RNVEC,PXNORM)

 PXINV=PXNORM
 CALL INVERT_MATRIX(PXINV)


!************************* Now the basisvectors are known and stored in PX and ORIGIN ********************


!========================= figure out the preliminary borders ========================

XDIST=0.0_DP
YDIST=0.0_DP

DO P=1,N%PATH
  VEC1=DMATMUL(PXINV,CGRID(P)%A-ORIGIN)
  VEC2=DMATMUL(PXINV,CGRID(P)%B-ORIGIN)
  IF(P==1)THEN
    XBORDER(1)=MIN(VEC1(1),VEC2(1))
    XBORDER(2)=MAX(VEC1(1),VEC2(1))
    YBORDER(1)=MIN(VEC1(2),VEC2(2))
    YBORDER(2)=MAX(VEC1(2),VEC2(2))
  ELSE
    XBORDER(1)=MIN(XBORDER(1),VEC1(1),VEC2(1))
    XBORDER(2)=MAX(XBORDER(2),VEC1(1),VEC2(1))
    YBORDER(1)=MIN(YBORDER(1),VEC1(2),VEC2(2))
    YBORDER(2)=MAX(YBORDER(2),VEC1(2),VEC2(2))
  END IF
END DO



!========================= are the given Symmetrypoints on the same plane? ==============================

!**************** Check the Symmetry option *********************
 IF(PAR%IREFLECT>1)THEN
   DO I=1,2
     IF(PAR%SYMREC)THEN
      SYM%BSYM(I,:)=PAR%SYMPOINTS(I,:)
      CALL CHANGE_BASIS(LATTICE%B,PAR%SYMPOINTS(I,:),SYM%ASYM(I,:))
     ELSE
      SYM%ASYM(I,:)=PAR%SYMPOINTS(I,:)
      LWORK=LATTICE%B
      CALL INVERT_MATRIX(LWORK)
      CALL CHANGE_BASIS(LWORK,PAR%SYMPOINTS(I,:),SYM%BSYM(I,:))
     END IF
     VEC1=DMATMUL(PXINV,SYM%ASYM(I,:)-ORIGIN)
     SYM%S2D(I,1:2)=VEC1(1:2)
   END DO



  IF (ABS(DDOT_PRODUCT(SYM%ASYM(1,:)-ORIGIN,RNVEC))>THIN)THEN
       CALL CLEAN_DISPLAY_LINE()

       WRITE(*,'(A)') 'WARNING: The 1st symmetrypoint from the INPAR file'
       WRITE(*,'(A)') '             cartesian                 ||              reciprocal'
       WRITE(*,'(3F13.7,A4,3F13.7)') SYM%ASYM(1,:),' || ',SYM%BSYM(1,:)
       WRITE(*,*) 'is not an element of the surface'
       WRITE(*,*) 'symmetrypoints are deactivated'
       IF(PAR%SPECFUN)THEN
         HPROG=HPROG-(PAR%IREFLECT-1)*(N%FIT(SPN)+N%SPEC(SPN))
       ELSE
         HPROG=HPROG-(PAR%IREFLECT-1)*N%FIT(SPN)
       END IF

       PAR%IREFLECT=1
     END IF

     IF(PAR%IREFLECT==2)THEN

     IF (ABS(DDOT_PRODUCT(SYM%ASYM(2,:)-ORIGIN,RNVEC))>THIN)THEN
       CALL CLEAN_DISPLAY_LINE()
       WRITE(*,'(A)') 'WARNING: The 2nd symmetrypoint from the INPAR file'
       WRITE(*,'(A)') '             cartesian                 ||              reciprocal'
       WRITE(*,'(3F13.7,A4,3F13.7)') SYM%ASYM(2,:),' || ',SYM%BSYM(2,:)
       WRITE(*,*) 'is not an element of the surface'
       WRITE(*,*) 'symmetrypoint deactivated'
       IF(PAR%SPECFUN)THEN
         HPROG=HPROG+2*(N%FIT(SPN)+N%SPEC(SPN))
       ELSE
         HPROG=HPROG+2*N%FIT(SPN)
       END IF
       PAR%IREFLECT=4
     END IF
   END IF

   IF(PAR%IREFLECT==2)THEN
     IF(ABS(KDISTANCE(SYM%BSYM(2,:),SYM%BSYM(1,:),LATTICE%B))<THIN)THEN
       CALL CLEAN_DISPLAY_LINE()
       WRITE(*,'(A)') 'WARNING: The symmetrypoints from the INPAR file'
       WRITE(*,'(A)') '             cartesian                 ||              reciprocal'
       WRITE(*,'(3F13.7,A4,3F13.7)') SYM%ASYM(1,:),' || ',SYM%BSYM(1,:)
       WRITE(*,'(3F13.7,A4,3F13.7)') SYM%ASYM(2,:),' || ',SYM%BSYM(2,:)
       WRITE(*,*) 'are more or less the same'
       WRITE(*,*) 'take point nr. 1 for pointreflection'
       IF(PAR%SPECFUN)THEN
         HPROG=HPROG+2*(N%FIT(SPN)+N%SPEC(SPN))
       ELSE
         HPROG=HPROG+2*N%FIT(SPN)
       END IF

       PAR%IREFLECT=4
     END IF
   END IF
 END IF

 PROG=PROG+1
 CALL WRITE_PROGRESS(PROG,HPROG)

!************ Get the x-y-coordinates on the plane *********************
 IF(N%FIT(SPN)>0)THEN
   DO I=1,N%FIT(SPN)
     VEC1=DMATMUL(PXINV,ROOTSXYZ(I,:)-ORIGIN)
     FERMIXY(I,1:2)=VEC1(1:2)
   END DO
 END IF


!***************** adjust the borders due to symmetry operations **************************
IF(PAR%IREFLECT==4)THEN
   XDIST=MAX(ABS(SYM%S2D(1,1)-XBORDER(1)),ABS(SYM%S2D(1,1)-XBORDER(2)))
   XBORDER(1)=SYM%S2D(1,1)-XDIST
   XBORDER(2)=SYM%S2D(1,1)+XDIST
   YDIST=MAX(ABS(SYM%S2D(1,2)-YBORDER(1)),ABS(SYM%S2D(1,2)-YBORDER(2)))
   YBORDER(1)=SYM%S2D(1,2)-YDIST
   YBORDER(2)=SYM%S2D(1,2)+YDIST
ELSE IF (PAR%IREFLECT==2)THEN
  NXBORDER=XBORDER
  NYBORDER=YBORDER
   DO I=1,2
     DO J=1,2
      SVEC(1)=XBORDER(I)
      SVEC(2)=YBORDER(J)
      SVEC=AXIAL_REFLECTION(SYM%S2D(1,:),SYM%S2D(2,:),SVEC)
      NXBORDER(1)=MIN(NXBORDER(1),SVEC(1))
      NXBORDER(2)=MAX(NXBORDER(2),SVEC(1))
      NYBORDER(1)=MIN(NYBORDER(1),SVEC(2))
      NYBORDER(2)=MAX(NYBORDER(2),SVEC(2))
     END DO
   END DO
   XBORDER=NXBORDER
   YBORDER=NYBORDER
END IF


!************************* Calculate the k-vectors of the outer points ***************************
 BINV=LATTICE%B
 CALL INVERT_MATRIX(BINV)

VEC1(1)=XBORDER(1)
VEC1(2)=YBORDER(1)
VEC1(3)=0.0_DP
VEC2=DMATMUL(PXNORM,VEC1)+ORIGIN

CALL CHANGE_BASIS(BINV,VEC2,VEC1)

KCORNERS(1,:)=VEC1(:)

VEC1(1)=XBORDER(2)
VEC1(2)=YBORDER(1)
VEC1(3)=0.0_DP

VEC2=DMATMUL(PXNORM,VEC1)+ORIGIN

CALL CHANGE_BASIS(BINV,VEC2,VEC1)

KCORNERS(2,:)=VEC1(:)

VEC1(1)=XBORDER(2)
VEC1(2)=YBORDER(2)
VEC1(3)=0.0_DP
VEC2=DMATMUL(PXNORM,VEC1)+ORIGIN

CALL CHANGE_BASIS(BINV,VEC2,VEC1)


KCORNERS(3,:)=VEC1(:)


VEC1(1)=XBORDER(1)
VEC1(2)=YBORDER(2)
VEC1(3)=0.0_DP
VEC2=DMATMUL(PXNORM,VEC1)+ORIGIN

CALL CHANGE_BASIS(BINV,VEC2,VEC1)

KCORNERS(4,:)=VEC1(:)


 !************************************ Spectral Fermisurface *******************************
 IF(PAR%SPECFUN.AND.N%SPECDAT(SPN))THEN
  IF(.NOT.PAR%SLIMSPEC)ALLOCATE(FSPECXY(PAR%IREFLECT*N%SPEC(SPN),4))

    OPEN(UNIT=201,FILE=TRIM(SFILE),STATUS='REPLACE',ACTION='WRITE')
    WRITE(201,'(A3,2F18.12)') '#x ',XBORDER
    WRITE(201,'(A3,2F18.12)') '#y ',YBORDER
    DO I=1,4
      WRITE(201,'(A6,I1,A1,2(F0.4,", "),F0.4)') '#KEDGE',I,' ',KCORNERS(I,:)
    END DO
    SIGN1=1
    SIGN2=1

    IOCHECK=0
    CON=0

    DO K=1,PAR%IREFLECT
       
       OPEN(UNIT=21,FILE=TRIM(BFILE),STATUS='OLD',ACTION='READ')

    DO I=1,N%SPEC(SPN)
         PROG=PROG+1
         CALL WRITE_PROGRESS(PROG,HPROG)
         READ(21,'(I5,3(F22.16,1X))',IOSTAT=IOCHECK) P,YDIST,AS,AO
         IF(IOCHECK/=0)EXIT
         IF(P>1)THEN
           YDIST=YDIST-KGRID(P-1)%DIST
         ELSE
           FXY(2)=YDIST
         END IF

         D=KDISTANCE(KGRID(P)%A,KGRID(P)%B,LATTICE%B)
         VEC1=KGRID(P)%B-KGRID(P)%A
         VEC1=KGRID(P)%A+(YDIST/D)*VEC1
         CALL CHANGE_BASIS(LATTICE%B,VEC1,AREAL)
         VEC2=DMATMUL(PXINV,AREAL)
         FXY(1:2)=VEC2(1:2)

      IF(PAR%IREFLECT==4)THEN
        FXY(1)=SYM%S2D(1,1)+SIGN1*(FXY(1)-SYM%S2D(1,1))
        FXY(2)=SYM%S2D(1,2)+SIGN2*(FXY(2)-SYM%S2D(1,2))
      ELSE IF((PAR%IREFLECT==2).AND.(K==2))THEN
         FXY=AXIAL_REFLECTION(SYM%S2D(1,:),SYM%S2D(2,:),FXY)
      END IF
      WRITE(201,'(4F18.12)') FXY(1), FXY(2), AS, AO
      IF(.NOT.PAR%SLIMSPEC)THEN
         CON=CON+1
         FSPECXY(CON,:)=(/ FXY(1), FXY(2), AS, AO /)
      END IF
    END DO
    SIGN2=-1*SIGN2
    IF(SIGN2>0)SIGN1=-1*SIGN1

     REWIND(21)
     CLOSE(21)

    END DO
   CLOSE(201)

 
   IF(.NOT.PAR%SLIMSPEC)THEN
     IF(ALLOCATED(FSU))DEALLOCATE(FSU)
     CALL SORT(FSPECXY,FSU)
     DEALLOCATE(FSPECXY)
     CALL UNIQUE(FSU,FSPECXY)
     CALL MAKE_PM3D(FSPECXY,XBORDER,YBORDER,TRIM(SPM3DFILE))
     PROG=PROG+PAR%IREFLECT*N%SPEC(SPN)
     CALL WRITE_PROGRESS(PROG,HPROG)
   END IF

 END IF
!*********************************************************************************************
!================================ Write data for Fermisurface ================================
!*********************************************************************************************
IF(N%FIT(SPN)>0)THEN

 OPEN(UNIT=211,FILE=TRIM(FFILE),STATUS='REPLACE',ACTION='WRITE')
  IF(FILETYPE/=3)THEN
    K=0
    IF(PAR%FORB==0)THEN
      IF(ONUM>0)THEN
        OFIRST=.TRUE.
        EOPOS=1
        DO I=1,MAX(OEND,PAR%FORB)
          IF(NOORB(I))THEN
            EOLEN=LEN_TRIM(ADJUSTL(PAR%ORBNAMES(I)))
            EOLEN=EOLEN+EOPOS
            IF(OFIRST)THEN
              WRITE(EMPTYORBS(EOPOS:EOLEN),'(A)') TRIM(ADJUSTL(PAR%ORBNAMES(I)))
              OFIRST=.FALSE.
            ELSE
              EOLEN=EOLEN+2
              WRITE(EMPTYORBS(EOPOS:EOLEN),'(A)') ', '//TRIM(ADJUSTL(PAR%ORBNAMES(I)))
            END IF
            EOPOS=EOLEN+1
          ELSE
            WRITE(211,'(A1,I2,A)') '#',K,'ORBITAL='//TRIM(ADJUSTL(PAR%ORBNAMES(I)))
            K=K+1
          END IF
        END DO
      END IF
    ELSE
      K=1
      WRITE(211,'(A)') '#0ORBITAL='//TRIM(ADJUSTL(PAR%ORBNAMES(PAR%FORB)))
      IF(NOORB(PAR%FORB))K=0
    END IF
    WRITE(211,'(A,F6.4)') '#maxorb ',MAXORB
    IF(K==0)WRITE(211,'(A)') '#noorbital'

    IF((ANY(NOORB(1:N%ORB-1))).AND.(PAR%FORB==0).AND.(ONUM>0))THEN
       WRITE(211,'(A)') '#emptyorbs '//EMPTYORBS(1:EOLEN)
    END IF
    WRITE(211,'(A,I3,",",I3)') '#LAYOUT=',GET_SQRT_LAYOUT(K)
  END IF


  WRITE(211,'(A3,2F18.12)') '#x ',XBORDER
  WRITE(211,'(A3,2F18.12)') '#y ',YBORDER

    DO I=1,4
      WRITE(211,'(A6,I1,A1,2(F0.4,", "),F0.4)') '#KEDGE',I,' ',KCORNERS(I,:)
    END DO

  SIGN1=1
  SIGN2=1

  DO K=1,PAR%IREFLECT
    DO I=1,N%FIT(SPN)
      PROG=PROG+1
      CALL WRITE_PROGRESS(PROG,HPROG)
      IF(PAR%IREFLECT==4)THEN
        FXY(1)=SYM%S2D(1,1)+SIGN1*(FERMIXY(I,1)-SYM%S2D(1,1))
        FXY(2)=SYM%S2D(1,2)+SIGN2*(FERMIXY(I,2)-SYM%S2D(1,2))
      ELSE IF((PAR%IREFLECT==2).AND.(K==2))THEN
         FXY=AXIAL_REFLECTION(SYM%S2D(1,:),SYM%S2D(2,:),FERMIXY(I,:))
      ELSE
         FXY(1)=FERMIXY(I,1)
         FXY(2)=FERMIXY(I,2)
      END IF
      IF(PAR%FORB==0)THEN
         WRITE(211,'(3F18.12)',ADVANCE='NO') FXY(1), FXY(2), ROOTS(I)%W
         DO J=1,N%ORB
           IF(NOORB(J))CYCLE
           WRITE(211,'(F18.12)',ADVANCE='NO') ROOTS(I)%OC(J)
         END DO
         WRITE(211,*) ''
      ELSE
         WRITE(211,'(4F18.12)') FXY(1), FXY(2), ROOTS(I)%W, ROOTS(I)%OC(PAR%FORB)
      END IF
    END DO
    SIGN2=-1*SIGN2
    IF(SIGN2>0)SIGN1=-1*SIGN1
  END DO  
 CLOSE(211)


     IF((ANY(NOORB(1:OEND))).AND.(PAR%FORB==0))THEN
         IF(PROG<HPROG) CALL WRITE_PROGRESS(100,100)
         LASTPROG=.FALSE.
       IF(PAR%SPN==2)THEN
         WRITE(*,'(A)') 'The following orbital(s) do not have values above 0.01'
         WRITE(*,'(A,I1,A)') 'at the Fermi surface for spin=',SPN,':'
       ELSE
         WRITE(*,'(A)') 'The following orbital(s) do not have values above 0.01 at the Fermi surface:'
       END IF
       WRITE(*,'(A)') EMPTYORBS(1:EOLEN)
       WRITE(*,*)
     END IF

 END IF
 CALL PRINT_FGRID(TRIM(FERMIFILE)//'.grid.dat',XBORDER,YBORDER,PAR%FGRID)
 

 IF((PROG<HPROG).AND.(LASTPROG)) CALL WRITE_PROGRESS(100,100)

  
END SUBROUTINE FERMISURFACE





SUBROUTINE MAKE_ORTHONORMAL_BASIS(ONVEC,ORTHO)
  REAL(KIND=DP),PARAMETER :: THIN=0.000001_DP,PI=2.0_DP*ATAN(1.0)
  REAL(KIND=DP),DIMENSION(3) :: ONVEC,NVEC,XVEC,YVEC
  REAL(KIND=DP),DIMENSION(3,3) :: ORTHO
  REAL(KIND=DP) :: D, ALPHA,ANGLE

  XVEC=(/1.0_DP,0.0_DP,0.0_DP/)
  YVEC=(/0.0_DP,1.0_DP,0.0_DP/)
  NVEC=ONVEC/SQRT(DDOT_PRODUCT(ONVEC,ONVEC))
  ANGLE=ACOS(DDOT_PRODUCT(XVEC,NVEC))
  ALPHA=ANGLE-PI

  IF(ABS(ALPHA)<THIN)THEN
    ORTHO(1,:)=XVEC(:)
  ELSE
    ORTHO(1,1)=COS(ALPHA)
    ORTHO(2,1)=0.0_DP
    ORTHO(3,1)=SIN(ALPHA)
  END IF

  ANGLE=ACOS(DDOT_PRODUCT(YVEC,NVEC))
  ALPHA=ANGLE-PI

  IF(ABS(ALPHA)<THIN)THEN
    ORTHO(2,:)=YVEC(:)
  ELSE
    ORTHO(1,2)=0.0_DP
    ORTHO(2,2)=COS(ALPHA)
    ORTHO(3,2)=SIN(ALPHA)
  END IF
  
  ORTHO(:,3)=NVEC(:)

END SUBROUTINE MAKE_ORTHONORMAL_BASIS





SUBROUTINE PRINT_FGRID(FNAME,XBORDER,YBORDER,N)
  CHARACTER(LEN=*) :: FNAME
  REAL(DP),DIMENSION(2) :: XBORDER,YBORDER
  REAL(DP) :: DX,DY,X,Y
  INTEGER :: I,J,N

  IF(PAR%FGRID<=0) RETURN

  DX=(XBORDER(2)-XBORDER(1))/(1.0_DP*(N+1))
  DY=(YBORDER(2)-YBORDER(1))/(1.0_DP*(N+1))

  X=XBORDER(1)+DX
  Y=YBORDER(1)+DY
  OPEN(UNIT=45,FILE=TRIM(FNAME),ACTION='WRITE',STATUS='REPLACE')
  DO I=1,N
    WRITE(45,'(2F14.8)') X,YBORDER(1)
    WRITE(45,'(2F14.8)') X,0.5_DP*(YBORDER(1)+YBORDER(2))
    WRITE(45,'(2F14.8)') X,YBORDER(2)
    WRITE(45,*) ''
    WRITE(45,*) ''
    WRITE(45,'(2F14.8)') XBORDER(1),Y
    WRITE(45,'(2F14.8)') 0.5_DP*(XBORDER(1)+XBORDER(2)),Y
    WRITE(45,'(2F14.8)') XBORDER(2),Y

    WRITE(45,*) ''
    WRITE(45,*) ''
    X=X+DX
    Y=Y+DY
  END DO

  CLOSE(45)
END SUBROUTINE PRINT_FGRID





SUBROUTINE MAKE_PM3D(DAT,XBORDER,YBORDER,PM3DFILE)
  REAL(DP),DIMENSION(:,:),INTENT(IN) :: DAT
  REAL(DP),DIMENSION(2) :: XBORDER,YBORDER
  REAL(DP),PARAMETER :: THIN=0.000001_DP
  REAL(DP) :: P,X,Y,XDIST,YDIST,X2,Y2,XDISTEMP,YDISTEMP,MINDIST=0.001_DP
  REAL(DP),DIMENSION(:),ALLOCATABLE :: MXDIST,MYDIST
  CHARACTER(LEN=*) :: PM3DFILE
  INTEGER :: I,IPOS(1),J,D1,D2,IX,IY,BCOUNT,RESX,RESY,UN=318
  LOGICAL :: LFX=.TRUE.,LFY=.TRUE.
  LOGICAL, DIMENSION(:),ALLOCATABLE :: DMASK

  XDIST=0.0_DP
  YDIST=0.0_DP
  D1=SIZE(DAT,1)
  D2=SIZE(DAT,2)
  ALLOCATE(MXDIST(D1),MYDIST(D1),DMASK(D1))
  MXDIST=0.0_DP
  MYDIST=0.0_DP
  DO I=1,D1
    IF(DAT(I,3)<THIN)CYCLE
    LFX=.TRUE.
    LFY=.TRUE.
    DO J=1,D1
      IF(I==J)CYCLE
      IF(DAT(J,3)<THIN)CYCLE
      XDISTEMP=ABS(DAT(I,1)-DAT(J,1))
      IF(XDISTEMP>MINDIST)THEN
         IF((XDISTEMP<MXDIST(I)).OR.LFX)THEN
           MXDIST(I)=XDISTEMP
           LFX=.FALSE.
         END IF
      END IF
      YDISTEMP=ABS(DAT(I,2)-DAT(J,2))
      IF(YDISTEMP>MINDIST)THEN
         IF((YDISTEMP<MYDIST(I)).OR.LFY)THEN
           MYDIST(I)=YDISTEMP
           LFY=.FALSE.
         END IF
      END IF
    END DO
  END DO


  DMASK=.TRUE.
  DO I=1,20
    IPOS=MINLOC(MXDIST,DMASK)
    DMASK(IPOS(1))=.FALSE.
  END DO

  DO I=1,20
    IPOS=MAXLOC(MXDIST,DMASK)
    DMASK(IPOS(1))=.FALSE.
  END DO 

  XDIST=0.0_DP
  DO I=1,D1
    IF(DMASK(I)) XDIST=XDIST+MXDIST(I)
  END DO

  XDIST=XDIST/REAL(D1-40,DP)

  DMASK=.TRUE.
  DO I=1,5
    IPOS=MINLOC(MYDIST,DMASK)
    DMASK(IPOS(1))=.FALSE.
  END DO

  DO I=1,5
    IPOS=MAXLOC(MYDIST,DMASK)
    DMASK(IPOS(1))=.FALSE.
  END DO 

  YDIST=0.0_DP
  DO I=1,D1
    IF(DMASK(I)) YDIST=YDIST+MYDIST(I)
  END DO

  YDIST=YDIST/REAL(D1-10,DP)


  RESX=MIN(NINT((XBORDER(2)-XBORDER(1))/XDIST)+1,100)
  RESY=MIN(NINT((YBORDER(2)-YBORDER(1))/YDIST)+1,100)

  XDIST=(XBORDER(2)-XBORDER(1))/REAL((RESX-1),DP)
  YDIST=(YBORDER(2)-YBORDER(1))/REAL((RESY-1),DP)
  X2=0.5_DP*XDIST
  Y2=0.5_DP*YDIST
  X=XBORDER(1)
  Y=YBORDER(1)
  OPEN(UNIT=UN,FILE=TRIM(PM3DFILE),ACTION='WRITE',STATUS='REPLACE')
    DO IX=1,RESX
      Y=YBORDER(1)
      DO IY=1,RESY
        P=0.0_DP
        BCOUNT=0
        DO I=1,D1
          IF( ((ABS(DAT(I,1)-X)<X2).OR.(DAT(I,1)==(X-X2))).AND. &
          &   (( ABS(DAT(I,2)-Y)<Y2).OR.(DAT(I,2)==(Y-Y2)))) THEN
             P=P+DAT(I,3)
             BCOUNT=BCOUNT+1
          END IF

        END DO

       IF(BCOUNT==0)THEN
         WRITE(UN,'(3F16.10)') X,Y,0.0_DP
       ELSE
         WRITE(UN,'(3F16.10)') X,Y,P/REAL(BCOUNT,DP)
       END IF

       Y=Y+YDIST
      END DO
      WRITE(UN,*) ''
      X=X+XDIST
    END DO
  CLOSE(UN)
  
END SUBROUTINE MAKE_PM3D





!#########################################################################
!######################## Spectral function ##############################
!#########################################################################

SUBROUTINE SPECTRAL_FUNCTION(TEBS,BANDSPEC,FILETYPE)
  TYPE(BAND),DIMENSION(:),INTENT(IN) :: TEBS
  CHARACTER(LEN=*) :: BANDSPEC
  CHARACTER(LEN=50) :: SPECFILE
  INTEGER :: I,J,kptOLD,kpt,approx,uni,FILETYPE,SPN,SPECLEN,FSURLEN
  REAL(DP) :: A,AO,Ptemp,Etemp,E,Emin,Emax,DeltaE
  REAL(DP) :: SIGMA,delta,S,THIN
  REAL(DP),DIMENSION(:),ALLOCATABLE :: Em,P,OR
  INTEGER :: COL,MAXKPT
  IF(PAR%LSURFACE)THEN
    WRITE(*,'(A)') '          write spectral function for fermisurface'
  ELSE
    WRITE(*,'(A)') '          write spectral function for bandstructure'
  END IF
  
  THIN=1.0E-5
  uni=28
  MAXKPT=MAXVAL(NFILE(:)%BND)
  SPECFILE=TRIM(BANDSPEC)
  SPECLEN=LEN_TRIM(SPECFILE)+1
  ALLOCATE(Em(MAXKPT+1),P(MAXKPT+1),OR(MAXKPT+1))
  
  SPIN: DO SPN=1,PAR%SPN
 
    IF(PAR%SPN==2) WRITE(SPECFILE(SPECLEN:SPECLEN+5),'(A5,I1)') '.spin',SPN
    approx=0
    N%SPEC(SPN)=0

     OPEN(UNIT=UNI,FILE=TRIM(SPECFILE)//'.dat',RECL=300,status="unknown")
      kptOLD=1
      i=1
      KPOINTS: DO COL=1,SIZE(TEBS,1)
       IF ((MOD(COL,100)==0).OR.(COL==SIZE(TEBS,1))) &
       &        CALL WRITE_PROGRESS(COL+(SPN-1)*SIZE(TEBS,1),PAR%SPN*SIZE(TEBS,1))

       !Still the same pc k-point as before?
       IF((TEBS(COL)%NKPT==kptOLD).AND.(TEBS(COL)%SPN==SPN)) THEN
        P(i)=TEBS(COL)%W
        Em(i)=TEBS(COL)%E
        OR(i)=TEBS(COL)%OC(PAR%FORB)
        i=i+1
       !Last line for current k-point already read
       END IF
       IF(((TEBS(COL)%NKPT/=kptOLD).AND.(TEBS(COL)%SPN==SPN)).OR.(COL==SIZE(TEBS,1))) THEN
         A=0.0_DP
         AO=0.0_DP
         IF(PAR%LSURFACE)THEN
           !Fermi surface step
           DO J=1,I-1
             CALL DELSTP(approx,Em(j)/PAR%SIGMA,delta,S)
             A=A+P(j)*delta
             AO=AO+OR(J)*delta
           END DO
           IF((A>THIN).OR.(.NOT.PAR%SLIMSPEC))THEN
             WRITE(UNI,'(I5,3(F22.16,1X))')  &
             & TEBS(MAX(COL-1,1))%PATH,TEBS(MAX(COL-1,1))%K,A,AO
             N%SPEC(SPN)=N%SPEC(SPN)+1
             IF(A>THIN)N%SPECDAT(SPN)=.TRUE.
           END IF
          ! end Fermi surface
         ELSE
           E=PAR%FBORDER(1)
           DO WHILE (E<=PAR%FBORDER(2))
             A=0.0_DP
             AO=0.0_DP
             DO J=1,I-1
               CALL DELSTP(APPROX,(E-Em(j))/PAR%SIGMA,DELTA,S)
               A=A+P(J)*DELTA
               AO=AO+OR(J)*DELTA
             END DO
             IF((A>THIN).OR.(.NOT.PAR%SLIMSPEC))THEN
               WRITE(UNI,'(I7,4(F22.16,1X))')  &
               & TEBS(MAX(COL-1,1))%NKPT,TEBS(MAX(COL-1,1))%K,E,A,AO
               N%SPEC(SPN)=N%SPEC(SPN)+1
             END IF
             E=E+PAR%SPECDELTA
           enddo
         END IF
         P(1)=TEBS(COL)%W
         Em(1)=TEBS(COL)%E
         OR(1)=TEBS(COL)%OC(PAR%FORB)
         i=2
         kptOLD=TEBS(COL)%NKPT
       ENDIF
      ENDDO KPOINTS
      WRITE(UNI,*) ''
      CLOSE(UNI)

      IF(N%SPEC(SPN)==0)THEN
          WRITE(*,'(A,I1)') 'Found no data of the spectral function'
        IF(PAR%LSURFACE)THEN
          IF(PAR%SPN==2)THEN
            WRITE(*,'(A,I1)') 'for the Fermi surface for spin=',SPN
          ELSE
            WRITE(*,'(A)') 'of the Fermi surface'
          END IF
        ELSE
          IF(PAR%SPN==2)THEN
            WRITE(*,'(A,I1)') 'for spin=',SPN
          END IF
        END IF
      END IF
    END DO SPIN
END SUBROUTINE SPECTRAL_FUNCTION


!******************** DELSTP    ****************************************
!
! Returns generalised delta and step functions (Methfessel & Paxton)
!
!  Input:
!      n > -1 : order of approximant; x : argument
!  Output:
!      D_n (x) ,  S_n (x)
!  Remarks:
!      D_n (x) = exp -x^2 * sum_i=0^n A_i H_2i(x)
!      S_n (x) = (1 - erf x)/2 + exp -x^2 * sum_i=1^n A_i H_{2i-1}(x)
!      where H is a Hermite polynomial and
!      A_i = (-1)^i / ( i! 4^i sqrt(pi) )
!
!***********************************************************************

      SUBROUTINE DELSTP(N,X,D,S)
      !USE constant
!      IMPLICIT REAL(8) (A-H,O-Z)
!      INTEGER, PARAMETER :: q=SELECTED_REAL_KIND(10) !USE prec
      INTEGER :: N,K,I
      REAL(DP) :: X,D,S,S0,H1,H2,H3,A,EX2
     REAL(DP),PARAMETER :: PI=4._DP*DATAN(1._DP)
!	PI=4._DP*DATAN(1._DP) !USE constant (?)

      IF (X<-1.E5_DP) THEN
         D=0._DP
         S=0._DP
         RETURN
      END IF
      IF (X>1.E5_DP) THEN
         D=0._DP
         S=1._DP
         RETURN
      END IF
!=======================================================================
!  If n < 0 : assume Gaussian type smearing
!  (must return  same as N=0 or ... )
!=======================================================================
      IF (N<0) THEN
         D=EXP(-(X*X))/SQRT(PI)
         S=0.5_DP+0.5_DP*ERRF(X)
         RETURN
      END IF
!=======================================================================
! Methfessel & Paxton
!=======================================================================
      EX2=EXP(-(X*X))
      S0=0.5_DP*ERRF(X)
      A=1._DP/SQRT(PI)
      K=0
      H1=1._DP
      H2=2._DP*X
      S=0._DP
      D=A
      DO I=1,N
         A=A/((-4._DP)*I)
         K=K+1
         H3=H1
         H1=H2
         H2=2._DP*X*H2-2*K*H3
         S=S+A*H1
         K=K+1
         H3=H1
         H1=H2
         H2=2._DP*X*H2-2*K*H3
         D=D+A*H1
      ENDDO
      D=D*EX2
      S=0.5_DP+S0-S*EX2
      RETURN
      END SUBROUTINE DELSTP

  FUNCTION ERRF(X)
!      INTEGER, PARAMETER :: q=SELECTED_REAL_KIND(10)
   REAL(DP) ERRF,X
   ERRF=ERF(X)
   RETURN
  END




END MODULE EBS_METHODS
