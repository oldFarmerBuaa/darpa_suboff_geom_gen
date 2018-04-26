program hull_gen
!-------------------------------------------------------!
!	PROGRAM TO GENERATE DARPA SUBOFF HULL		!
!	FULL/MODEL RATIO = 24				!
!	POINTS CALCULATED BY EQUATIONS:	UNIT = FEET	!
! BOW EQ.	        FOR       0.0 <= X <= 3.333333	!
! PARALLEL MID-BODY EQ. FOR  3.333333 <= X <= 10.645833	!
! AFTERBODY EQ.		FOR 10.645833 <= X <= 13.979167	!
! AFTERBODY CAP EQ.	FOR 13.979167 <= X <= 14.291667	!
! 	OFFSETS ARE COMPUTED EVERY 0.1 FT		!
!	IN FIRST 0.5 FT, STEP IS SET TO 0.01 FT		!
!							!
!		Xiaolong Tang: tangxl@buaa.edu.cn	!
!				       01/26/2018	!
!		        	BEIHANG UNIVERSITY	!
!-------------------------------------------------------!
IMPLICIT NONE
REAL(8) RMAX,XB,XM,XA,XC,CB1,CB2,CB3,RH,XX,DX,A,A3,A4,B,R
REAL(8) XI,C1,C2,C3,C4,C5,C6
REAL(8) X,Y,Z
INTEGER I,NP
DIMENSION X(300),Y(300),Z(300)
REAL K0,K1
CHARACTER(LEN=20) FILENAME
!-------------------------------!
!	DEFINE CONSTANTS	!
!-------------------------------!
RMAX =  0.8333333
XB   =  3.333333
XM   = 10.645833
XA   = 13.979167
XC   = 14.291667
CB1  =  1.126395101
CB2  =  0.442874707
CB3  =  1.0/2.1
RH   =  0.1175
K0   =  10.0
K1   =  44.6244
XX   =  -0.01
DX   =   0.01
Z(1:300)=0.0
DO 1000 I=1,300
  NP=I
  XX=XX+DX
  IF(XX.GE.0.5) DX=0.1
  IF(XX.GE.XA)  DX=0.01
  IF(XX.GE.XB)  GO TO 200
!-------------------------------!
!       BOW EQUATION            !
!-------------------------------!
  A  = 0.3*XX-1.0
  A3 = A**3
  A4 = A**4
  B  = 1.2*XX+1.0
  R  = CB1*XX*A4+CB2*XX*XX*A3+1.0-A4*B
  R  = RMAX*(R**CB3)
  X(I) = XX
  Y(I) = R
  GO TO 1000
200 CONTINUE
  IF(XX.GE.XM) GO TO 400
!-------------------------------!
!   PARALLEL MID-BODY EQUATION  !
!-------------------------------!
  X(I) = XX
  Y(I) = RMAX
  GO TO 1000
400 CONTINUE
  IF(XX.GE.XA) GO TO 600
!-------------------------------!
!       AFTERBODY EQUATION      !
!-------------------------------!
  XI = (13.979167-XX)/3.333333
  C1 =             RH*RH
  C2 =                       RH*K0             *XI*XI
  C3 = ( 20.0 - 20.0*RH*RH - 4.0*RH*K0 - 0.333333*K1)*XI**3
  C4 = (-45.0 + 45.0*RH*RH + 6.0*RH*K0 +          K1)*XI**4
  C5 = ( 36.0 - 36.0*RH*RH - 4.0*RH*K0 -          K1)*XI**5
  C6 = (-10.0 + 10.0*RH*RH +     RH*K0 + 0.333333*K1)*XI**6
  R  = RMAX*(C1+C2+C3+C4+C5+C6)**0.5
  X(I) = XX
  Y(I) = R
  GO TO 1000
600 CONTINUE
IF(XX.GE.XC) GO TO 1100
!-------------------------------!
!     AFTERBODY CAP EQUATION    !
!-------------------------------!
  R = 1.0 - (3.2*XX - 44.733333)**2
  R = RH*RMAX*(R**0.5)
  X(I) = XX
  Y(I) = R
1000 CONTINUE
1100 CONTINUE
  X(NP)=XC
  Y(NP)=0.0
!-------------------------------!
!    WRITE OFFSETS TO FILE      !
!-------------------------------!
FILENAME = "hull.dat"
OPEN (10,FILE=FILENAME,STATUS="UNKNOWN")
WRITE(10,*) NP,1
DO I=1,NP
  WRITE(10,4) X(I),Y(I),0.0
ENDDO
!WRITE(6,1)
!1 FORMAT('DARPA2')
!WRITE(6,2)
!2 FORMAT('MODEL WITH (MODEL/FULL) = 24')
!WRITE(6,3) NP
!3 FORMAT(I5)
!WRITE(6,4) (X(I),Y(I),I=1,NP)
!4 FORMAT(2F10.5,3X,2F10.5,3X,2F10.5)
4 FORMAT(3F10.5)
STOP
END

