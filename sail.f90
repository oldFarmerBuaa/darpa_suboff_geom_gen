program sail_gen
!-------------------------------------------------------!
!	PROGRAM TO GENERATE DARPA SUBOFF SAIL		!
!	FULL/MODEL RATIO = 24				!
!	POINTS CALCULATED BY EQUATIONS:	UNIT = FEET	!
! SAIL FOREBODY EQ.  FOR   3.032986 <= X <= 3.358507	!
!			   0.833333 <= Z <= 1.507813	!
! SAIL MID-BODY EQ.  FOR   3.358507 <= X <= 3.559028	!
!			   0.833333 <= Z <= 1.507813    !
! SAIL AFTERBODY EQ. FOR   3.559028 <= X <= 4.241319	!
!			   0.833333 <= Z <= 1.507813    !
! SAIL CAP EQ.	     FOR   3.032986 <= X <= 4.241319	!
!			   1.507813 <= Z <= 1.562501    !
! 	OFFSETS ARE COMPUTED EVERY 0.005 FT		!
!							!
!		Xiaolong Tang: tangxl@buaa.edu.cn	!
!				       01/26/2018	!
!		        	BEIHANG UNIVERSITY	!
!-------------------------------------------------------!
IMPLICIT NONE
REAL(8) A1,B1,A3,B3,C3,D3,E3,HMAX,DX,DX0,XXCST,XXAFN,XXFFN,XXMFN,XZST,XZEND
REAL(8) XZ,XX,ADUM
REAL(8) X,DM1,A,B,C,D,E,F,G,H,P
INTEGER I,J,NI,NP,ICON1,N_OMIT
DIMENSION NP(300),X(300,50,3)
CHARACTER(LEN=20) FILENAME
!-------------------------------!
!	DEFINE CONSTANTS	!
!-------------------------------!
A1    =  2.094759
B1    =  0.207178
A3    =  2.908891
B3    =  1.234491
C3    =  3.444817
D3    =  3.850435
E3    =  2.080019
HMAX  =  0.109375
!MAX HALF THICKNESS OF SAIL
DX    =  0.005
DX0   =  0.005
XXCST =  3.032986
XXAFN =  4.241319
XXFFN =  3.358507
XXMFN =  3.559028
XZST  =  1.507813
XX=XXCST-DX
DO 1000 I=1,300
  XZ=XZST
  X(I,1,3)=XZ
  ! (I,1,3) = (POINT I, LAYER 1, AXIS 1/2/3)
  ! AXIS 1 = XX = LENGTH, 2 = XY = WIDTH, 3 = XZ = HEIGHT
  J=1
  XX=XX+DX
  X(I,1,1)=XX
  IF(XX.GT.XXAFN) THEN
  ! XX REACHED THE ENDPOINT
  ! NI = NUMBER OF EFFECTIVE POINTS
    NI=I-1
    GOTO 1014
  ENDIF
  IF(XX.GT.XXFFN) GOTO 1002
!-------------------------------!
!    SAIL FOREBODY EQUATION     !
!-------------------------------!
  D=3.072000*(XX-3.032986)
  DM1=D-1
  A=2*D*(DM1**4)
  B=D*D*(DM1**3)/3
  C=1- ((DM1**4)*(4*D+1))
  X(I,1,2)=HMAX*(SQRT(A1*A+B1*B+C))
  GOTO 1004
!-------------------------------!
!    SAIL MID-BODY EQUATION     !
!-------------------------------!
1002 CONTINUE
  IF(XX.GT.XXMFN) GOTO 1003
  X(I,1,2)=HMAX
  GOTO 1004
!-------------------------------!
!    SAIL AFTER-BODY EQUATION   !
!-------------------------------!
1003 CONTINUE
  E=(4.241319-XX)/0.6822917
  F=E-1
  G=2.238361*E*F**4
  H=3.106529*(E**2)*(F**3)
  P=1-(F**4)*(4*E+1)
  X(I,1,2)=0.1093750*(G+H+P)
!-------------------------------!
!      SAIL CAP EQUATION        !
!-------------------------------!
1004 CONTINUE
  XZEND=(X(I,1,2)/2)+1.507813
  NP(I)=1
  DO 1008 J=2,50
    ICON1=0
1005 XZ=XZ+DX
  X(I,J,3)=XZ
  IF(XZ.GT.XZEND) THEN
    ICON1=ICON1+1
    IF(ICON1.EQ.1) THEN
      XZ=XZ-DX
      DX=0.0005
      GOTO 1005
    ENDIF
    IF(ICON1.EQ.2) THEN
      X(I,J,2)=0.0
      X(I,J,3)=XZEND
      NP(I)=J
      ICON1=0
      DX=DX0
      GOTO 1000
    ENDIF
  ENDIF
  ADUM=(X(I,1,2)**2)-((2*(XZ-XZST))**2)
  X(I,J,2)=SQRT(ADUM)
1008 CONTINUE
1000 CONTINUE
!-------------------------------!
!    WRITE OFFSETS TO FILE      !
!-------------------------------!
1014 OPEN (10,FILE="sail-parallel-top.dat",STATUS="UNKNOWN")
WRITE(10,*) NI,1
DO I=1,NI
  WRITE(10,4) X(I,1,1),X(I,1,2),X(I,1,3)
ENDDO
CLOSE(10)
OPEN (20,FILE="sail-parallel-bottom.dat",STATUS="UNKNOWN")
WRITE(20,*) NI,1
DO I=1,NI
  WRITE(20,4) X(I,1,1),X(I,1,2),0.0
ENDDO
CLOSE(20)
OPEN (30,FILE="sail-cap.dat",STATUS="UNKNOWN")
! OMIT OUTPUT EVERY 8 STEPS
N_OMIT=8
DO 1013 I=1,NI,N_OMIT
    ! THE LINE BELOW IS FORMAT DATA FOR ICEMCFD INPUT
    WRITE(30,*) NP(I)+1,1
!  WRITE(30,'(I3)') I
!  WRITE(30,'(A2,F7.3,A4)') "X=",X(I,1,1),"FEET"
!  WRITE(30,'(I5)') NP(I)+1
  WRITE(30,4) X(I,1,1),X(I,1,2),1.507813
  DO J=1,NP(I)
    WRITE(30,4) X(I,1,1),X(I,J,2),X(I,J,3)
  ENDDO
4 FORMAT(3F16.12)
1013 CONTINUE
CLOSE(30)
STOP
END

