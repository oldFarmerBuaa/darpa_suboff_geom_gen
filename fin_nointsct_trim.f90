program appendage_gen
!-------------------------------------------------------!
!	PROGRAM TO GENERATE DARPA SUBOFF APPENDAGES	!
!	FULL/MODEL RATIO = 24				!
!	POINTS CALCULATED BY EQUATIONS:	UNIT = FEET	!
!	AXIAL LOCATION  = X  = H			!
!			= POSITIONS OF TE ON THE HULL	!
!			H(1) = 12.729617		!
!			H(2) = 13.146284 = BASELINE	!
!			H(3) = 13.562950		!
!	CHORD LENGTH    = CY = -0.466308*RR + 0.88859	!
!	MOUNTING POSITION: 0,90,180,270 DEGREES		!
!			   0 = TOP-DEAD CENTER OF HULL	!
!	NO INTERSECTION WILL HULL PERFROMED		!
!	OUTPUT FILE IS ICEMCFD FORMATED DATA		!
!		Xiaolong Tang: tangxl@buaa.edu.cn	!
!				       01/26/2018	!
!		        	BEIHANG UNIVERSITY	!
!-------------------------------------------------------!
IMPLICIT NONE
REAL(8) XXI,H,RH,AK0,AK1,RMAX,XXX,RR,Z
REAL(8) X,Y,HH,DX,XIB,XI,CY
REAL(8) A,B,C,D,E,RHA,RHAS,DELR,SRS,XINIT,RBSMAX,RO
REAL(8) TE_TRIM_AT
INTEGER I,J,K,NP,ITR,I750
DIMENSION XXI(20),H(3)
CHARACTER(LEN=27) FILENAME
!-------------------------------!
!	DEFINE CONSTANTS	!
!-------------------------------!
! XXI: POSITION ALONG CHORD
! NP : TOTAL NUMBER OF POINTS TO CALCULATE
RH    =   0.1175
AK0   =  10.0
AK1   =  44.6244
NP    =  SIZE(XXI)
RMAX  =   0.833333
DATA XXI /0.0  , 0.005, 0.0125, 0.025, 0.050, 0.075, 0.100,&
         &0.150, 0.200, 0.2500, 0.300, 0.400, 0.500, 0.600,&
         &0.700, 0.800, 0.9000, 0.950, 0.975, 1.000/
WRITE(*,*) "APPENDAGE TRAILING EDGE TRIMED AT ?% CHORD(0-100):"
READ(*,*) TE_TRIM_AT
TE_TRIM_AT = TE_TRIM_AT/100
J=1
DO I=1,NP
  IF(TE_TRIM_AT .LE. XXI(I)) THEN
    XXI(I)=TE_TRIM_AT
    GOTO 1000
  ENDIF
  J=J+1
ENDDO
! NP AFTER TRIMING
1000 NP=J
DATA H   /12.729617, 13.146284, 13.562950/
!-------------------------------!
!    LOOP ON TRAILING EDGE      !
!-------------------------------!
DO 900 K=1,3
  HH = H(K)
  WRITE(6,1) HH
1 FORMAT(//2X,'TRAILING EDGE LOCATED AT X = ',F10.5)
  DX = 0.05
  X  =HH+DX
  WRITE(FILENAME,'(A16,F5.3,A5,I1)') "appdg-noitsct-te",TE_TRIM_AT,"-case",K
  OPEN(10*K,FILE=FILENAME,STATUS="UNKNOWN")
!-------------------------------!
!    LOOP ON AXIAL X 	        !
!    STARTING AT TE		!
!    ENDING AT LE		!
!-------------------------------!
  DO 800 J=1,32
    X=X-DX
    IF(X.GT.HH) GOTO 800
!-------------------------------!
!    DEFINE HULL RADIUS AT X    !
!-------------------------------!
    XIB = (13.979167-X)/3.333333
    A   =               RH*RH +     RH*AK0                *XIB*XIB
    B   = ( 20.0 - 20.0*RH*RH - 4.0*RH*AK0 - 0.333333*AK1)*XIB**3
    C   = (-45.0 + 45.0*RH*RH + 6.0*RH*AK0 +          AK1)*XIB**4
    D   = ( 36.0 - 36.0*RH*RH - 4.0*RH*AK0 -          AK1)*XIB**5
    E   = (-10.0 + 10.0*RH*RH +     RH*AK0 + 0.333333*AK1)*XIB**6
    RHA = RMAX*(A+B+C+D+E)**0.5
    RHAS = RHA*RHA
    RR   = 0.075
    DELR = 0.025
    ITR  = 0
!-------------------------------!
!    LOOP ON RADIUS		!
!    BEGIN WITH R = 0.1		!
!-------------------------------!
    DO 700 I=1,31
      RR=RR+DELR
      CY=-0.466308*RR+0.88859
      XI=(X-HH)/CY+1.0
      IF(XI.LT.0.0 .OR. XI.GT.1.0) GOTO 700
!-------------------------------!
!    DEFINE APPENDAGES          !
!-------------------------------!
      Z = 0.29690*SQRT(XI) - 0.12600*XI - 0.35160*XI*XI&
      & + 0.28520*XI**3    - 0.10450*XI**4
      Z = CY*Z
      SRS = RR*RR + Z*Z
700 CONTINUE
      GOTO 800
!-------------------------------!
!    CALCULATE APPENDAGE   	!
!    SECTION AT GIVEN RADIUS    !
!-------------------------------!
    CY=-0.466308*RR+0.88859
    I750=0
    XINIT=(X-HH)/CY+1.0
!-------------------------------!
!    LOOP ON XI			!
!-------------------------------!
    DO 750 I=1,NP
      XI=XXI(I)
      IF(XI.LT.XINIT) GOTO 750
740 CONTINUE
      XI=XXI(I)
      IF(I750.EQ.0) XI=XINIT
      XXX=(XI-1.0)*CY+HH
      IF(XI.LT.0.0 .OR. XI.GT.1.0) GOTO 750
      Z = 0.29690*SQRT(XI) - 0.12600*XI - 0.35160*XI*XI&
      & + 0.28520*XI**3    - 0.10450*XI**4
      Z = CY*Z
!-------------------------------!
!    WRITE X,Y,+/-Z TO FILE     !
!-------------------------------!
      IF(I750.EQ.0) WRITE(6,2)
2 FORMAT(//6X,1HX,8X,3HR/Y,6X,6H(+/-)Z)
      WRITE(6,3) XXX,RR,Z
      WRITE(10*K,3) XXX,Z,RR
3 FORMAT(3F10.5)
      I750=I750+1
      RBSMAX=RR
      IF(I750.EQ.1) GOTO 740
750 CONTINUE
800 CONTINUE
!-------------------------------!
!    CALCULATE INTERSECTION     !
!    WITH HULL, NOW RADIUS      !
!    LARGER THAN HULL RADIUS   	!
!-------------------------------!
  DELR = 0.05
  DO 850 I=1,NP
    IF(I==1) WRITE(10*k,*)NP,NP
    RO=RR
    RR=RBSMAX+I*DELR
    IF(RR.GT.RMAX) RR=RMAX
    IF(RR.EQ.RO) GOTO 900
    CY=-0.466308*RR+0.88859
    WRITE(6,2)
    DO 840 J=1,NP
    XI=XXI(J)
    XXX=(XI-1.0)*CY+HH
    Z = 0.29690*SQRT(XI) - 0.12600*XI - 0.35160*XI*XI&
    & + 0.28520*XI**3    - 0.10450*XI**4
    Z=CY*Z
    WRITE(6,3) XXX,RR,Z
    WRITE(10*K,3) XXX,Z,RR
840 CONTINUE
850 CONTINUE
    CLOSE(10*K)
900 CONTINUE
STOP
END
