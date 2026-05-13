!File=lattice.f90
MODULE  MYLATTICE
USE MATH
IMPLICIT NONE

TYPE :: LATT
  REAL(KIND=DP) :: SCALE
  REAL(KIND=DP) :: A(3,3), B(3,3)
  REAL(KIND=DP) :: ANORM(3), BNORM(3)
  REAL(KIND=DP) :: OMEGA
  REAL(KIND=DP) :: AVEL(3,3)
  INTEGER :: INITlatv

END TYPE LATT


CONTAINS




!**************** SUBROUTINE LATTIC  *******************************
! RCS:  $Id: lattice.F,v 1.2 2001/04/05 10:34:10 kresse Exp $
!
!  subroutine for calculating the reciprocal lattice from the direct
!  lattice
!  in addition the norm of the lattice-vectors and the volume of
!  the basis-cell is calculated
!*******************************************************************

SUBROUTINE LATTIC(Mylatt)

 TYPE(LATT) Mylatt
 REAL(KIND=DP) Omega
 INTEGER I,J
 INTRINSIC SUM

 CALL EXPRO(Mylatt%B(1:3,1),Mylatt%A(1:3,2),Mylatt%A(1:3,3))
 CALL EXPRO(Mylatt%B(1:3,2),Mylatt%A(1:3,3),Mylatt%A(1:3,1))
 CALL EXPRO(Mylatt%B(1:3,3),Mylatt%A(1:3,1),Mylatt%A(1:3,2))

 Omega =Mylatt%B(1,1)*Mylatt%A(1,1)+Mylatt%B(2,1)*Mylatt%A(2,1) &
 &      +Mylatt%B(3,1)*Mylatt%A(3,1)

 DO I=1,3
  DO J=1,3
   Mylatt%B(I,J)=Mylatt%B(I,J)/Omega
  ENDDO
 ENDDO

 DO I=1,3
  Mylatt%ANORM(I)=SQRT(SUM(Mylatt%A(:,I)*Mylatt%A(:,I)))
  Mylatt%BNORM(I)=SQRT(SUM(Mylatt%B(:,I)*Mylatt%B(:,I)))
 ENDDO
 Mylatt%Omega=Omega
 RETURN
END SUBROUTINE LATTIC




!**************** SUBROUTINE RLATTIC  ******************************
!
!  subroutine for calculating the direct lattice from the real
!  lattice
!  in addition the norm of the lattice-vectors and the volume of
!  the basis-cell is calculated
!*******************************************************************

SUBROUTINE RLATTIC(Mylatt)

 TYPE(LATT) Mylatt
 REAL(KIND=DP) Omega
 INTEGER I,J
 INTRINSIC SUM

 CALL EXPRO(Mylatt%A(1:3,1),Mylatt%B(1:3,2),Mylatt%B(1:3,3))
 CALL EXPRO(Mylatt%A(1:3,2),Mylatt%B(1:3,3),Mylatt%B(1:3,1))
 CALL EXPRO(Mylatt%A(1:3,3),Mylatt%B(1:3,1),Mylatt%B(1:3,2))

 Omega =Mylatt%B(1,1)*Mylatt%A(1,1)+Mylatt%B(2,1)*Mylatt%A(2,1) &
 &      +Mylatt%B(3,1)*Mylatt%A(3,1)

 DO I=1,3
  DO J=1,3
   Mylatt%A(I,J)=Mylatt%A(I,J)/Omega
  ENDDO
 ENDDO

 DO I=1,3
  Mylatt%ANORM(I)=SQRT(SUM(Mylatt%A(:,I)*Mylatt%A(:,I)))
  Mylatt%BNORM(I)=SQRT(SUM(Mylatt%B(:,I)*Mylatt%B(:,I)))
 ENDDO
 Mylatt%Omega=1.0/Omega
 RETURN
END SUBROUTINE RLATTIC




!**************** SUBROUTINE EXPRO ***************************
! EXPRO
! caclulates the x-product of two vectors
!
!*************************************************************

SUBROUTINE EXPRO(H,U1,U2)
 IMPLICIT REAL(KIND=DP) (A-H,O-Z)
 DIMENSION H(3),U1(3),U2(3)

 H(1)=U1(2)*U2(3)-U1(3)*U2(2)
 H(2)=U1(3)*U2(1)-U1(1)*U2(3)
 H(3)=U1(1)*U2(2)-U1(2)*U2(1)

 RETURN
END SUBROUTINE EXPRO


END MODULE MYLATTICE
