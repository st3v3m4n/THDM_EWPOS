c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich


* AllHA0eqHH.F
* generated 4-Jun-2018 11:05

C> @brief This subroutine calculates the scalar two-loop contribution \f$\delta\rho^{(2)}_{H}\f$ for \f$m_{A^0}=m_{H^0}\f$.
C>
C> @ingroup group_rho
	subroutine calcAllHA0eqHH
	implicit none


#include "AllHA0eqHH.h"

	ComplexType dup1, dup10, dup11, dup12, dup13, dup14, dup15
	ComplexType dup16, dup17, dup18, dup2, dup3, dup4, dup5
	ComplexType dup6, dup7, dup8, dup9

	RealType T134sub

	external T134sub

	ComplexType A0FH, B0FH

	external A0FH, B0FH

#include "paraTHDM.h"
#include "paraSM.h"
	AllHahl(1) = A0FH(Mh02)

	AllHahl(10) = B0FH(MHH2,MHp2,MHp2)

	AllHahl(11) = B0FH(MHp2,0.D0,Mh02)

	AllHahl(12) = B0FH(MHp2,0.D0,MHH2)

	AllHahl(13) = B0FH(MHp2,Mh02,MHp2)

	AllHahl(14) = B0FH(MHp2,MHH2,MHp2)

	AllHahl(15) = B0FH(Mh02,0.D0,0.D0)

	AllHahl(16) = B0FH(Mh02,0.D0,MHH2)

	AllHahl(17) = B0FH(Mh02,0.D0,MHp2)

	AllHahl(18) = B0FH(Mh02,Mh02,Mh02)

	AllHahl(19) = B0FH(Mh02,Mh02,MHH2)

	AllHahl(2) = A0FH(MHH2)

	AllHahl(20) = B0FH(Mh02,MHH2,MHH2)

	AllHahl(21) = B0FH(Mh02,MHp2,MHp2)

	AllHahl(22) = T134sub(MHH2,0.D0,0.D0)

	AllHahl(23) = T134sub(Mh02,0.D0,0.D0)

	AllHahl(24) = T134sub(MHH2,Mh02,0.D0)

	AllHahl(25) = T134sub(MHH2,Mh02,Mh02)

	AllHahl(26) = T134sub(MHH2,MHH2,MHH2)

	AllHahl(27) = T134sub(Mh02,Mh02,Mh02)

	AllHahl(28) = T134sub(MHH2,MHH2,Mh02)

	AllHahl(29) = T134sub(MHp2,0.D0,0.D0)

	AllHahl(3) = A0FH(MHp2)

	AllHahl(30) = T134sub(MHp2,Mh02,0.D0)

	AllHahl(31) = T134sub(MHp2,Mh02,Mh02)

	AllHahl(32) = T134sub(MHp2,MHH2,0.D0)

	AllHahl(33) = T134sub(MHp2,MHH2,Mh02)

	AllHahl(34) = T134sub(MHp2,MHH2,MHH2)

	AllHahl(35) = T134sub(MHp2,MHp2,Mh02)

	AllHahl(36) = T134sub(MHp2,MHp2,MHH2)

	AllHahl(37) = T134sub(MHp2,MHp2,MHp2)

	AllHahl(4) = B0FH(MHH2,0.D0,0.D0)

	AllHahl(5) = B0FH(MHH2,0.D0,MHp2)

	AllHahl(6) = B0FH(MHH2,0.D0,Mh02)

	AllHahl(7) = B0FH(MHH2,Mh02,MHH2)

	AllHahl(8) = B0FH(MHH2,MHH2,MHH2)

	AllHahl(9) = B0FH(MHH2,Mh02,Mh02)

        AllHahx(1539) =
     &   MHH2*S2B*(4*S2A + S2B)*SBA +
     &    (6*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*(4 - 7*SBA2))

        AllHahx(1546) =
     &   MHH2*S2B*(9*S2A + 4*S2B)*SBA +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*(7 - 27*SBA2))

        AllHahx(1547) =
     &   MHH2*(17*S2A - 26*S2B)*S2B*SBA -
     &    (60*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*(1 - SBA2))

        AllHahx(1548) =
     &   MHH2*S2B*(141*S2A + 34*S2B)*SBA +
     &    (8*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*(4 + 33*SBA2))

        AllHahx(1549) =
     &   MHH2*(27*S2A - 16*S2B)*S2B*SBA -
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*(4 + 43*SBA2))

	AllHahx(789) = 455*S2B + 18*(4*S2A - 21*S2B)*SBA2

	AllHahx(798) = 121*S2B + 3*(9*S2A + 23*S2B)*SBA2

        AllHahx(1375) =
     &   -(S2B*(1 - 25*SBA2)) - 2*CBA*SAB*(13 - 12*SBA2)

	AllHahx(1378) = S2B + 11*S2B*SBA2 + CBA*SAB*(101 + 54*SBA2)

	AllHahx(1381) = CBA*SAB*(43 - 27*SBA2) + 2*S2B*(1 + 8*SBA2)

        AllHahx(1384) =
     &   9*CBA*SAB*(43 + 18*SBA2) + 2*S2B*(3 + 121*SBA2)

	AllHahx(1409) = 9*S2A**2*S2B - S2B**3

	AllHahx(1410) = -3*CBA*(3*S2A - S2B) + 4*SAB

	AllHahx(1412) = 93*S2A**2 - 37*S2A*S2B + 2*S2B**2

	AllHahx(1413) = CBA*(3*S2A - S2B) + 4*SAB

	AllHahx(1415) = 41*CBA*(3*S2A - S2B) + 20*SAB

	AllHahx(1491) = 333*S2A**2 - 258*S2A*S2B + 13*S2B**2

	AllHahx(1497) = S2B - 54*S2B*SBA2 + 6*S2A*(1 + 7*SBA2)

	AllHahx(1517) = Mh02**2*MHH2**2 + 6*Mh02*MHH2**3

        AllHahx(1523) =
     &   9*Mh02**3 - 29*Mh02**2*MHH2 - 8*Mh02*MHH2**2 - 80*MHH2**3

        AllHahx(1524) =
     &   -5*MHH2*S2B*SAB*SBA +
     &    (13*MW2*SW2)/(Alfa*Pi)*((CB2*Lambda6 - Lambda7*SB2)*SBA2)

        AllHahx(1529) =
     &   MHH2*(149*S2A - 319*S2B)*S2B*SBA -
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*(13 - 210*SBA2))

        AllHahx(1530) =
     &   MHH2*S2B*(2569*S2A + 211*S2B)*SBA -
     &    (8*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*(1 - 111*SBA2))

        AllHahx(1531) =
     &   MHH2*S2B*(23*S2A + 11*S2B)*SBA -
     &    (12*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*(1 + 10*SBA2))

        AllHahx(1532) =
     &   MHH2*(1141*S2A - 725*S2B)*S2B*SBA -
     &    (24*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*(46 + 81*SBA2))

        AllHahx(1533) =
     &   MHH2*S2B*(1073*S2A + 665*S2B)*SBA +
     &    (6*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*(85 + 613*SBA2))

        AllHahx(1535) =
     &   MHH2*(619*S2A - 238*S2B)*S2B*SBA -
     &    (12*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*(50 - 81*SBA2))

        AllHahx(1536) =
     &   MHH2*(866*S2A - 301*S2B)*S2B*SBA +
     &    (6*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*(35 - 44*SBA2))

        AllHahx(1537) =
     &   MHH2*(248*S2A - 87*S2B)*S2B*SBA +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*(124 - 21*SBA2))

        AllHahx(1538) =
     &   MHH2*(1705*S2A - 624*S2B)*S2B*SBA -
     &    (8*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*(7 - 12*SBA2))

        AllHahx(1540) =
     &   Mh02**5*(113*S2A - 44*S2B)*S2B*SBA + MHH2**4*AllHahx(1539)

        AllHahx(1544) =
     &   44*Mh02**3 - 43*Mh02**2*MHH2 - 10*Mh02*MHH2**2 + 9*MHH2**3

        AllHahx(1545) =
     &   100*Mh02**3 - 105*Mh02**2*MHH2 - 36*Mh02*MHH2**2 +
     &    14*MHH2**3

        AllHahx(1550) =
     &   -4*Mh02**4*S2B*(49*S2A + 2*S2B)*SBA -
     &    MHH2**3*AllHahx(1546) - Mh02*MHH2**2*AllHahx(1547) +
     &    Mh02**3*AllHahx(1548) + 3*Mh02**2*MHH2*AllHahx(1549)

	AllHahx(1600) = Mh02**2 - 4*(Mh02*MHH2 + MHH2**2)

	AllHahx(631) = 4*S2A*S2B**3 - 7*S2B**4

	AllHahx(632) = 4*CB2**2*Lambda6*S2B + Lambda7*S2B**3

	AllHahx(633) = -7*Lambda7*S2B**4 + 4*S2A*AllHahx(632)

	AllHahx(634) = CB2**3*Lambda6 + CB2*Lambda7*S2A*S2B

	AllHahx(642) = 21*S2A*S2B**3 - 67*S2B**4

	AllHahx(643) = -67*Lambda7*S2B**4 + 21*S2A*AllHahx(632)

	AllHahx(651) = 3*S2A*S2B**3 - 5*S2B**4

	AllHahx(652) = -5*Lambda7*S2B**4 + 3*S2A*AllHahx(632)

	AllHahx(660) = 4*S2A*S2B**3 - 61*S2B**4

	AllHahx(661) = -61*Lambda7*S2B**4 + 4*S2A*AllHahx(632)

        AllHahx(758) =
     &   Lambda6*S2B**4*(25 - 378*SBA2) +
     &    72*Lambda7*S2B**2*SB2**2*SBA2

        AllHahx(759) =
     &   S2B**4*SB2*(25 - 378*SBA2) +
     &    48*CB2**2*SB2**3*(13 + 36*SBA2)

	AllHahx(768) = 146*S2B - 135*(2*S2A - 3*S2B)*SBA2

	AllHahx(776) = 3*Lambda6*S2A*S2B + 4*Lambda7*SB2**2

	AllHahx(779) = 4*S2B + 9*(4*S2A - 9*S2B)*SBA2

	AllHahx(787) = 7*S2B*(65 - 54*SBA2) + 72*S2A*SBA2

        AllHahx(788) =
     &   6*Lambda6*S2A*S2B*SB2*SBA2 - Lambda7*SB2**3*(21 + 40*SBA2)

        AllHahx(790) =
     &   24*Lambda7*(12*S2A + 13*S2B)*SB2**2*SBA2 +
     &    Lambda6*S2B**2*AllHahx(789)

	AllHahx(796) = 27*S2A*SBA2 + S2B*(121 + 69*SBA2)

        AllHahx(797) =
     &   27*Lambda6*S2A*S2B*SB2*SBA2 -
     &    4*Lambda7*SB2**3*(19 + 70*SBA2)

        AllHahx(799) =
     &   4*Lambda7*(27*S2A - S2B)*SB2**2*SBA2 +
     &    Lambda6*S2B**2*AllHahx(798)

        AllHahx(840) =
     &   S2A - 2*CBA2*S2B + 6*CAB*SBA + (S2A - 3*S2B)*SBA2

	AllHahx(1153) = 9*S2A**2 + 26*S2B**2

        AllHahx(1268) =
     &   CBA*MHH2*S2B**2 -
     &    2*((2*m12sq - MHH2*S2B)*SAB +
     &       (MW2*SW2)/(Alfa*Pi)*((CB2*Lambda6 - Lambda7*SB2)*SBA))

	AllHahx(1349) = 51*S2A**2 + 36*S2A*S2B - 80*S2B**2

        AllHahx(1376) =
     &   -9*S2A**2*(1 - 5*SBA2) - 10*S2B*(CBA*SAB - 2*S2B*SBA2) +
     &    3*S2A*AllHahx(1375)

        AllHahx(1379) =
     &   3*S2A**2*(1 + 5*SBA2) + S2B*(21*CBA*SAB + 2*S2B*SBA2) +
     &    S2A*AllHahx(1378)

        AllHahx(1382) =
     &   3*S2A**2*(2 - 17*SBA2) + S2B*(68*CBA*SAB + 11*S2B*SBA2) +
     &    S2A*AllHahx(1381)

        AllHahx(1385) =
     &   6*S2A**2*(3 + 34*SBA2) +
     &    S2B*(139*CBA*SAB + 58*S2B*SBA2) + S2A*AllHahx(1384)

	AllHahx(1406) = 349*S2A**2 - 96*S2A*S2B + 12*S2B**2

	AllHahx(1407) = -3*CBA*(113*S2A + 6*S2B) + 8*SAB

        AllHahx(1411) =
     &   MHH2*AllHahx(1409) -
     &    (3*MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*AllHahx(1410))

        AllHahx(1414) =
     &   MHH2*S2B*AllHahx(1412) +
     &    (6*MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*AllHahx(1413))

        AllHahx(1416) =
     &   -5*MHH2*AllHahx(1409) +
     &    (3*MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*AllHahx(1415))

	AllHahx(1451) = 18*S2A*(1 - 94*SBA2) + S2B*(29 - 12*SBA2)

	AllHahx(1456) = 71*CBA*SAB - 5*S2B*(3 + 16*SBA2)

	AllHahx(1460) = 322*CBA*S2B*SAB + 570*SAB**2 + 15*S2B**2*SBA2

	AllHahx(1461) = 280*CBA*SAB + S2B*(8 + 729*SBA2)

	AllHahx(1463) = S2B*(9 - 251*SBA2) - 3*S2A*(9 - 146*SBA2)

	AllHahx(1466) = 2*CBA*S2B*SAB + 6*SAB**2 + 3*S2B**2*SBA2

	AllHahx(1467) = 28*CBA*SAB + S2B*(2 + 123*SBA2)

	AllHahx(1469) = 3*S2A*(2 + 37*SBA2) - S2B*(2 + 43*SBA2)

	AllHahx(1472) = 327*CBA*S2B*SAB + 549*SAB**2 + 76*S2B**2*SBA2

	AllHahx(1473) = 692*CBA*SAB + S2B*(18 + 1825*SBA2)

	AllHahx(1475) = -3*S2A*(29 - 160*SBA2) + S2B*(29 + 293*SBA2)

	AllHahx(1479) = S2B*(2 - 19*SBA2) - 3*S2A*(2 - 15*SBA2)

	AllHahx(1485) = 135*S2A**2 - 114*S2A*S2B - 25*S2B**2

	AllHahx(1487) = 74*CBA*SAB + S2B*(8 + 251*SBA2)

	AllHahx(1489) = 210*CBA*SAB + S2B*(34 + 293*SBA2)

        AllHahx(1492) =
     &   8*CBA*(3*S2A - S2B)*SAB + 24*SAB**2 + SBA2*AllHahx(1491)

        AllHahx(1493) =
     &   44*CBA*S2B*SAB - 36*SAB**2 + 9*S2A**2*(1 - 124*SBA2) +
     &    S2B**2*(1 + 40*SBA2) -
     &    6*S2A*(S2B + 22*CBA*SAB - 45*S2B*SBA2)

        AllHahx(1498) =
     &   -2*MHH2*(S2A - 9*S2B)*S2B*SAB +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*AllHahx(1497))

	AllHahx(1503) = 7*Mh02**2 + 17*Mh02*MHH2 + 12*MHH2**2

	AllHahx(1504) = Mh02**2 + 47*Mh02*MHH2 - 12*MHH2**2

	AllHahx(1505) = 19*Mh02**2 - 55*Mh02*MHH2 - 36*MHH2**2

        AllHahx(1507) =
     &   4*m12sq*(Mh02 - MHH2) + Mh02*(25*Mh02 + 11*MHH2)*S2B

        AllHahx(1516) =
     &   Mh02**3 - 8*Mh02**2*MHH2 + 32*(Mh02*MHH2**2 + MHH2**3)

        AllHahx(1518) =
     &   Mh02**4*(S2A - 9*S2B) - 6*Mh02**3*MHH2*(S2A - 7*S2B) +
     &    64*MHH2**4*(S2A + S2B) + 16*(S2A - S2B)*AllHahx(1517)

        AllHahx(1520) =
     &   29*MHH2*S2B*SAB*SBA +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*(37 - 54*SBA2)*SBA2)

        AllHahx(1521) =
     &   3*MHH2*S2B*SAB*SBA +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*(3 - 2*SBA2)*SBA2)

        AllHahx(1522) =
     &   MHH2*S2B*SAB*SBA +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA2*(1 + 30*SBA2))

        AllHahx(1525) =
     &   -(m12sq*SAB*SBA*AllHahx(1523)) + 8*MHH2**3*AllHahx(1524)

	AllHahx(1528) = 13*Mh02*MHH2 + 4*(Mh02**2 + MHH2**2)

        AllHahx(1534) =
     &   -4*Mh02**5*(149*S2A - S2B)*S2B*SBA +
     &    Mh02*MHH2**3*AllHahx(1529) + Mh02**4*AllHahx(1530) +
     &    4*MHH2**4*AllHahx(1531) -
     &    Mh02**2*MHH2**2*AllHahx(1532) -
     &    Mh02**3*MHH2*AllHahx(1533)

        AllHahx(1541) =
     &   Mh02**2*MHH2**2*AllHahx(1535) +
     &    Mh02**3*MHH2*AllHahx(1536) -
     &    Mh02*MHH2**3*AllHahx(1537) - Mh02**4*AllHahx(1538) +
     &    4*AllHahx(1540)

        AllHahx(1543) =
     &   12*Mh02**3 - 11*Mh02**2*MHH2 - 2*Mh02*MHH2**2 + MHH2**3

        AllHahx(1551) =
     &   2*m12sq**2*(3*S2A - S2B)*SBA*AllHahx(1544) +
     &    m12sq*AllHahx(1550) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (CBA*Mh02*S2B*(CB2*Lambda6 - Lambda7*SB2)*AllHahx(1545))

	AllHahx(176) = -(S2B*(7 - 6*SBA2)) - 3*S2A*(2 - 3*SBA2)

	AllHahx(178) = S2B - 3*S2A*(2 - 3*SBA2) + 6*S2B*SBA2

	AllHahx(180) = 6*S2A - 7*S2B - 3*(3*S2A + 2*S2B)*SBA2

	AllHahx(207) = S2B*(29 + 6*SBA2) - 3*S2A*(2 + 33*SBA2)

	AllHahx(209) = 6*S2A + 55*S2B + 3*(33*S2A - 2*S2B)*SBA2

	AllHahx(211) = S2B*(19 - 6*SBA2) + 3*S2A*(2 + 33*SBA2)

        AllHahx(531) =
     &   (Mh02 - MHH2)*MHH2 - (3*Mh02 + MHH2)*MHp2 + 4*MHp2**2

	AllHahx(547) = 3*Mh02*MHH2 + 2*(Mh02**2 + MHH2**2)

        AllHahx(596) =
     &   4*m12sq*SAB*SBA2 +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*(1 + 3*SBA2))

        AllHahx(599) =
     &   2*m12sq*SAB*SBA2 +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*(1 + SBA2))

	AllHahx(629) = (2*CAB - 3*C2B*CBA)*SBA + 9*S2A*SBA2

	AllHahx(635) = -(CB2*Lambda7*S2B**2) + 4*AllHahx(634)

        AllHahx(636) =
     &   CB2*Lambda6*AllHahx(631) - SB2*AllHahx(633) +
     &    4*SB2**2*AllHahx(635)

	AllHahx(640) = -((8*CAB + 35*C2B*CBA)*SBA) + 90*S2A*SBA2

        AllHahx(644) =
     &   88*CB2**3*Lambda6 + CB2*Lambda7*(21*S2A*S2B - 22*S2B**2)

        AllHahx(645) =
     &   CB2*Lambda6*AllHahx(642) - SB2*AllHahx(643) +
     &    4*SB2**2*AllHahx(644)

	AllHahx(649) = (2*CAB - 71*C2B*CBA)*SBA + 162*S2A*SBA2

        AllHahx(653) =
     &   32*CB2**3*Lambda6 + CB2*Lambda7*(21*S2A*S2B - 8*S2B**2)

        AllHahx(654) =
     &   7*CB2*Lambda6*AllHahx(651) - 7*SB2*AllHahx(652) +
     &    4*SB2**2*AllHahx(653)

	AllHahx(658) = (11*CAB + 27*C2B*CBA)*SBA - 99*S2A*SBA2

        AllHahx(662) =
     &   28*CB2**3*Lambda6 + CB2*Lambda7*(S2A*S2B - 7*S2B**2)

        AllHahx(663) =
     &   CB2*Lambda6*AllHahx(660) - SB2*AllHahx(661) +
     &    16*SB2**2*AllHahx(662)

	AllHahx(667) = -2*(CAB + 2*C2B*CBA)*SBA + 27*S2A*SBA2

	AllHahx(669) = 4*CB2**3*Lambda6 - CB2*Lambda7*S2B**2

        AllHahx(670) =
     &   -13*S2B**4*(CB2*Lambda6 - Lambda7*SB2) +
     &    34*SB2**2*AllHahx(669)

	AllHahx(691) = 6*CAB*SBA + (3*S2A - S2B)*SBA2

        AllHahx(692) =
     &   8*CBA*SAB + 16*CAB*SBA + 6*S2A*SBA2 - S2B*(8 + SBA2)

	AllHahx(715) = 4*S2A*S2B**3 - 19*S2B**4

	AllHahx(716) = -19*Lambda7*S2B**4 + 4*S2A*AllHahx(632)

	AllHahx(723) = 3*S2A*S2B**3 - 16*S2B**4

	AllHahx(724) = -16*Lambda7*S2B**4 + 3*S2A*AllHahx(632)

	AllHahx(731) = 2*S2A*S2B**3 - 41*S2B**4

	AllHahx(732) = -41*Lambda7*S2B**4 + 2*S2A*AllHahx(632)

	AllHahx(739) = 21*S2A*S2B**3 - 74*S2B**4

	AllHahx(740) = -74*Lambda7*S2B**4 + 21*S2A*AllHahx(632)

        AllHahx(760) =
     &   48*CB2**3*Lambda6*SB2**2*(13 + 30*SBA2) +
     &    CB2*AllHahx(758) - Lambda7*AllHahx(759)

        AllHahx(766) =
     &   45*Lambda6*S2A*S2B*SB2*SBA2 +
     &    8*Lambda7*SB2**3*(1 + 12*SBA2)

	AllHahx(767) = 270*S2A*SBA2 - S2B*(146 + 405*SBA2)

        AllHahx(769) =
     &   -108*Lambda7*(10*S2A + S2B)*SB2**2*SBA2 +
     &    Lambda6*S2B**2*AllHahx(768)

        AllHahx(777) =
     &   -3*CB2**3*Lambda6*SB2**2 + CB2**2*SB2*AllHahx(776)

	AllHahx(778) = S2B*(4 - 81*SBA2) + 36*S2A*SBA2

        AllHahx(780) =
     &   12*Lambda7*(12*S2A + S2B)*SB2**2*SBA2 +
     &    Lambda6*S2B**2*AllHahx(779)

        AllHahx(791) =
     &   144*CB2**3*Lambda6*SB2**2*(7 + 22*SBA2) +
     &    Lambda7*S2B**3*SB2*AllHahx(787) +
     &    48*CB2**2*AllHahx(788) - CB2*S2B*AllHahx(790)

        AllHahx(800) =
     &   16*CB2**3*Lambda6*SB2**2*(19 + 69*SBA2) +
     &    Lambda7*S2B**3*SB2*AllHahx(796) +
     &    4*CB2**2*AllHahx(797) - CB2*S2B*AllHahx(799)

	AllHahx(841) = MHp2*(S2A - 2*CBA2*S2B) - MHH2*AllHahx(840)

        AllHahx(842) =
     &   MHp2*(S2A + S2B) - MHH2*(S2A + S2B + (S2A - S2B)*SBA2)

        AllHahx(1108) =
     &   3*Mh02*(Mh02 - 4*MHp2)*S2A +
     &    (4*Mh02 - 7*MHp2)*(Mh02 - 2*MHp2)*S2B

	AllHahx(1121) = 21*S2A**2 + 4*S2B**2

	AllHahx(1124) = 18*S2A**2 + 13*S2B**2

        AllHahx(1130) =
     &   MHp2*(63*S2A - 8*S2B)*S2B*SBA -
     &    (4*MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

        AllHahx(1131) =
     &   MHp2*(18*S2A - 5*S2B)*S2B*SBA -
     &    (4*MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

        AllHahx(1144) =
     &   MHp2*(9*S2A - 32*S2B)*S2B*SBA -
     &    (16*MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

        AllHahx(1145) =
     &   MHp2*(9*S2A - 10*S2B)*S2B*SBA -
     &    (8*MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

        AllHahx(1154) =
     &   MHp2**2*SBA2*AllHahx(1153) +
     &    (32*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*MHp2*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHahx(1169) =
     &   3 - (1 - 2*SW2)**2 - 4*(3 - 2*CW2)*SW2 + 12*SW2**2

        AllHahx(1350) =
     &   S2A**2 + 47*CBA*(S2A - S2B)*SAB + SBA2*AllHahx(1349)

	AllHahx(1351) = -3*S2A*(2 - 43*SBA2) + 11*S2B*(5 - 6*SBA2)

        AllHahx(1354) =
     &   62*CBA*S2A*SAB + S2A**2*(1 + 3*SBA2) -
     &    2*S2B*(5*CBA*SAB - 6*S2B*SBA2)

	AllHahx(1355) = S2B*(5 - 2*SBA2) - S2A*(3 + 14*SBA2)

        AllHahx(1357) =
     &   S2A**2*(1 - 25*SBA2) - 8*S2B*(2*CBA*SAB - 5*S2B*SBA2) -
     &    24*S2A*(3*CBA*SAB + 2*S2B*SBA2)

	AllHahx(1358) = S2A*(29 - 81*SBA2) - 4*S2B*(7 + 15*SBA2)

        AllHahx(1360) =
     &   S2A**2*(6 + 19*SBA2) + 3*S2A*(47*CBA*SAB + 6*S2B*SBA2) +
     &    S2B*(25*CBA*SAB + 9*S2B*SBA2)

	AllHahx(1361) = 3*S2B*(11 - 58*SBA2) - 5*S2A*(20 + 57*SBA2)

        AllHahx(1363) =
     &   S2A**2*(2 + SBA2) + 6*S2A*(7*CBA*SAB + 3*S2B*SBA2) -
     &    3*S2B*(6*CBA*SAB + 23*S2B*SBA2)

	AllHahx(1364) = -3*S2B*(3 - 2*SBA2) - S2A*(10 + 63*SBA2)

	AllHahx(1367) = 3*(3*S2A + S2B)*SBA2 + 2*CBA*SAB*(4 + 3*SBA2)

        AllHahx(1368) =
     &   (3*S2A + S2B)*(5 - SBA2) + 8*CBA*SAB*(4 + 9*SBA2)

        AllHahx(1369) =
     &   S2B + 7*S2B*SBA2 + 3*S2A*(1 + 7*SBA2) +
     &    4*CBA*SAB*(4 + 27*SBA2)

        AllHahx(1370) =
     &   (3*S2A + S2B)*(2 - 35*SBA2) - 2*CBA*SAB*(44 + 45*SBA2)

	AllHahx(1372) = 27*S2A**2 + 6*S2A*S2B - S2B**2

	AllHahx(1373) = 3*S2B - S2A*(4 + 3*SBA2)

        AllHahx(1374) =
     &   MHH2*S2B*(S2A + S2B)*SBA2 +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA*(1 - 3*SBA2))

        AllHahx(1377) =
     &   MHH2*S2B*AllHahx(1376) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(3*S2A + S2B)*(CB2*Lambda6 - Lambda7*SB2)*SBA*
     &       (43 + 15*SBA2))

        AllHahx(1380) =
     &   MHH2*S2B*AllHahx(1379) -
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(3*S2A + S2B)*(CB2*Lambda6 - Lambda7*SB2)*SBA*
     &       (7 + 48*SBA2))

        AllHahx(1383) =
     &   MHH2*S2B*AllHahx(1382) -
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(3*S2A + S2B)*(CB2*Lambda6 - Lambda7*SB2)*SBA*
     &       (10 + 63*SBA2))

        AllHahx(1386) =
     &   MHH2*S2B*AllHahx(1385) -
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(3*S2A + S2B)*(CB2*Lambda6 - Lambda7*SB2)*SBA*
     &       (14 + 255*SBA2))

	AllHahx(1390) = 58*S2A**2 - 60*S2A*S2B + 417*S2B**2

	AllHahx(1391) = 9*CBA*(93*S2A - 53*S2B) + 568*SAB

	AllHahx(1393) = 3*S2A**2 + 2*S2A*S2B - 9*S2B**2

	AllHahx(1394) = CBA*(3*S2A + 8*S2B) - 4*SAB

	AllHahx(1396) = 1075*S2A**2 + 182*S2A*S2B + 477*S2B**2

	AllHahx(1397) = CBA*(749*S2A - 80*S2B) - 772*SAB

	AllHahx(1399) = 307*S2A**2 - 141*S2A*S2B + 30*S2B**2

	AllHahx(1400) = CBA*(3661*S2A + 18*S2B) - 416*SAB

	AllHahx(1402) = 33*S2A**2 + 22*S2A*S2B - 107*S2B**2

	AllHahx(1403) = 7*CBA*(23*S2A - 36*S2B) + 164*SAB

        AllHahx(1405) =
     &   80*Mh02**5 - 336*Mh02**4*MHH2 - 33*Mh02**3*MHH2**2 +
     &    424*Mh02**2*MHH2**3 - 147*Mh02*MHH2**4 + 12*MHH2**5

        AllHahx(1417) =
     &   40*Mh02**3*S2A*(3*S2A - S2B)*S2B -
     &    MHH2**2*AllHahx(1411) + 2*Mh02**2*AllHahx(1414) -
     &    Mh02*MHH2*AllHahx(1416)

	AllHahx(1421) = 208*(CBA*S2B*SAB + SAB**2) + 449*S2B**2*SBA2

        AllHahx(1423) =
     &   S2B*(939 - 2132*SBA2) - 2*S2A*(619 - 1685*SBA2)

        AllHahx(1427) =
     &   S2B*(11413 - 18654*SBA2) - 18*S2A*(407 + 2122*SBA2)

	AllHahx(1430) = -330*(CBA*S2B*SAB + SAB**2) + 41*S2B**2*SBA2

	AllHahx(1432) = 3*S2A*(10 + 379*SBA2) - S2B*(67 + 648*SBA2)

        AllHahx(1436) =
     &   S2B*(15790 - 14511*SBA2) - 6*S2A*(607 + 12103*SBA2)

	AllHahx(1439) = -3657*(CBA*S2B*SAB + SAB**2) + 593*S2B**2*SBA2

        AllHahx(1441) =
     &   22*S2A + 1893*S2B - 2*(7024*S2A - 725*S2B)*SBA2

	AllHahx(1444) = 34*S2A**2 + 4*S2A*S2B - 7*S2B**2

        AllHahx(1447) =
     &   63*S2B**2*SBA2 + 2*S2A**2*(4 + 963*SBA2) +
     &    4*S2A*(8*CBA*SAB + 51*S2B*SBA2)

	AllHahx(1448) = S2B*(7 - 12*SBA2) - 6*S2A*(1 + 49*SBA2)

        AllHahx(1450) =
     &   384*(CBA*S2B*SAB + SAB**2) + 681*S2B**2*SBA2 -
     &    2*S2A**2*(264 + 31177*SBA2) -
     &    4*S2A*(640*CBA*SAB + 3479*S2B*SBA2)

	AllHahx(1452) = -12*SAB*(1 - 21*SBA2) + CBA*AllHahx(1451)

        AllHahx(1457) =
     &   -32*CBA*S2B*SAB + 288*SAB**2 - 202*S2B**2*SBA2 +
     &    3*S2A**2*(60 + 1291*SBA2) + 4*S2A*AllHahx(1456)

        AllHahx(1458) =
     &   9*CBA*(124*S2A - 15*S2B)*SBA2 +
     &    SAB*(23 + 12*SBA2 + 81*SBA2**2)

        AllHahx(1462) =
     &   24*S2A**2*(1 - 43*SBA2) + 2*AllHahx(1460) -
     &    S2A*AllHahx(1461)

        AllHahx(1464) =
     &   SAB*(241 + 132*SBA2 + 855*SBA2**2) + CBA*AllHahx(1463)

        AllHahx(1468) =
     &   6*S2A**2*(1 - 22*SBA2) + 4*AllHahx(1466) -
     &    S2A*AllHahx(1467)

        AllHahx(1470) =
     &   -2*SAB*(1 - 12*SBA2 - 9*SBA2**2) + CBA*AllHahx(1469)

        AllHahx(1474) =
     &   3*S2A**2*(18 - 1319*SBA2) + 4*AllHahx(1472) -
     &    S2A*AllHahx(1473)

        AllHahx(1476) =
     &   SAB*(457 + 276*SBA2 + 1647*SBA2**2) - CBA*AllHahx(1475)

	AllHahx(1478) = 21*S2A**2 + 16*S2A*S2B + 2*S2B**2

        AllHahx(1480) =
     &   4*MHH2*(2*S2A - 5*S2B)*S2B*SAB +
     &    (3*MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*AllHahx(1479))

        AllHahx(1486) =
     &   -8*CBA*(3*S2A - S2B)*SAB + 72*SAB**2 + SBA2*AllHahx(1485)

        AllHahx(1488) =
     &   -74*CBA*S2B*SAB - 570*SAB**2 - S2B**2*(4 - 209*SBA2) -
     &    9*S2A**2*(4 + 73*SBA2) + 3*S2A*AllHahx(1487)

        AllHahx(1490) =
     &   -210*CBA*S2B*SAB - 1098*SAB**2 - S2B**2*(17 - 410*SBA2) -
     &    9*S2A**2*(17 - 80*SBA2) + 3*S2A*AllHahx(1489)

        AllHahx(1494) =
     &   Mh02**4*MHH2*AllHahx(1492) + Mh02**3*MHH2**2*AllHahx(1493)

	AllHahx(1496) = 109*S2A**2 - 32*S2A*S2B + 8*S2B**2

        AllHahx(1499) =
     &   -(CBA*AllHahx(1498)) +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (SAB*(CB2*Lambda6 - Lambda7*SB2)*SBA*(23 + 93*SBA2))

        AllHahx(1506) =
     &   m12sq**2*AllHahx(1503) - m12sq*Mh02*S2B*AllHahx(1504) +
     &    Mh02**2*S2B**2*AllHahx(1505)

        AllHahx(1508) =
     &   -9*CBA2**3*m12sq**2*(3*Mh02 + MHH2) +
     &    CBA2**2*m12sq*AllHahx(1507)

        AllHahx(1513) =
     &   Mh02**2*MHH2*(1 - 22*SBA2) + Mh02*MHH2**2*(3 + 7*SBA2)

        AllHahx(1519) =
     &   -2*m12sq*(3*S2A + S2B)*AllHahx(1516) + S2B*AllHahx(1518)

        AllHahx(1526) =
     &   Mh02**3*MHH2*AllHahx(1520) - 3*Mh02**4*AllHahx(1521) +
     &    8*Mh02**2*MHH2**2*AllHahx(1522) - 2*Mh02*AllHahx(1525) +
     &    (64*MW2*SW2)/(Alfa*Pi)*
     &     (MHH2**4*(CB2*Lambda6 - Lambda7*SB2)*(1 - 3*SBA2)*SBA2)

        AllHahx(1542) =
     &   -16*m12sq**2*(Mh02 - MHH2)**2*(3*S2A - S2B)*SBA*
     &     AllHahx(1528) + Mh02*S2B*AllHahx(1534) +
     &    2*m12sq*AllHahx(1541)

        AllHahx(1552) =
     &   -(CBA2**2*AllHahx(1551)) +
     &    (18*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*CBA2**3*m12sq*(CB2*Lambda6 - Lambda7*SB2)*
     &       AllHahx(1543))

        AllHahx(177) =
     &   MHH2*(3*S2A - S2B)*S2B**2 - 2*m12sq*S2B*AllHahx(176)

	AllHahx(179) = 56*CB2**2*Lambda6 + Lambda7*S2B*AllHahx(178)

	AllHahx(181) = -32*Lambda7*SB2**2 + Lambda6*S2B*AllHahx(180)

	AllHahx(192) = S2B*(61 - 114*SBA2) + 3*S2A*(38 + 15*SBA2)

	AllHahx(194) = 38*S2A + 11*S2B + (15*S2A - 38*S2B)*SBA2

	AllHahx(196) = S2B*(15 - 38*SBA2) + S2A*(38 + 15*SBA2)

        AllHahx(208) =
     &   35*MHH2*(3*S2A - S2B)*S2B**2 + 2*m12sq*S2B*AllHahx(207)

	AllHahx(210) = 192*Lambda7*SB2**2 + Lambda6*S2B*AllHahx(209)

	AllHahx(212) = 336*CB2**2*Lambda6 + Lambda7*S2B*AllHahx(211)

        AllHahx(331) =
     &   4*Mh02**3 - 18*Mh02**2*MHH2 + 3*Mh02*MHH2**2 + 2*MHH2**3

        AllHahx(402) =
     &   MHp2*S2B*(4*S2A + 3*S2B) +
     &    (48*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHahx(532) =
     &   (Mh02 - MHH2)*(Mh02 + MHH2 - 2*MHp2)*S2A +
     &    S2B*AllHahx(531)

	AllHahx(539) = 2*m12sq - Mh02*(S2A - S2B)

	AllHahx(546) = 2*Mh02*MHH2 - (Mh02 + MHH2)*MHp2

        AllHahx(548) =
     &   2*Mh02**2*MHH2**2 + MHp2**4 +
     &    (Mh02 + MHH2)*(-3*Mh02*MHH2*MHp2 - 2*MHp2**3) +
     &    MHp2**2*AllHahx(547)

	AllHahx(551) = -(MHH2*MHp2**2) + Mh02*(MHH2**2 + MHp2**2)

        AllHahx(553) =
     &   Mh02*(Mh02 - MHp2)*(MHH2 - MHp2) +
     &    3*(Mh02 - MHH2)*(Mh02*MHH2 - MHp2**2)*SBA2

	AllHahx(561) = S2B*(2 - 3*SBA2) - 2*S2A*SBA2

	AllHahx(571) = 6*CAB*SBA + 3*S2A*SBA2 + S2B*(7 + 13*SBA2)

	AllHahx(576) = 2*CAB*SBA - S2B*(3 + SBA2)

	AllHahx(577) = S2B + 19*S2B*SBA2 - 4*(3*CAB*SBA + S2A*SBA2)

	AllHahx(585) = 6*CAB*SBA + S2B*(3 + 8*SBA2)

        AllHahx(593) =
     &   2*MHp2**2 - MHH2**2*(1 + SBA2) +
     &    Mh02*(MHH2 - 2*MHp2 + MHH2*SBA2)

        AllHahx(594) =
     &   MHp2*S2B**2*SBA2*(1 + SBA2) -
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHahx(597) =
     &   MHp2*S2B*SBA2*(S2B - 4*CBA*SAB + S2B*SBA2) -
     &    2*CBA*AllHahx(596)

        AllHahx(600) =
     &   MHp2*S2B**2*SBA2*(1 + SBA2) - 4*CBA*AllHahx(599)

        AllHahx(601) =
     &   MHp2*S2B*SBA2*(S2B - 4*CBA*SAB + S2B*SBA2) -
     &    CBA*AllHahx(596)

	AllHahx(630) = S2B**5*SBA2 + S2B**4*AllHahx(629)

        AllHahx(637) =
     &   S2B**4*(CB2*Lambda6 - Lambda7*SB2) - 3*SBA2*AllHahx(636)

	AllHahx(641) = -10*S2B**5*SBA2 + S2B**4*AllHahx(640)

        AllHahx(646) =
     &   800*CB2**3*Lambda6*SB2**2 - 800*CB2**2*Lambda7*SB2**3 -
     &    179*S2B**4*(CB2*Lambda6 - Lambda7*SB2) +
     &    12*SBA2*AllHahx(645)

	AllHahx(650) = -2*S2B**5*SBA2 + S2B**4*AllHahx(649)

        AllHahx(655) =
     &   128*CB2**3*Lambda6*SB2**2 - 128*CB2**2*Lambda7*SB2**3 -
     &    77*S2B**4*(CB2*Lambda6 - Lambda7*SB2) +
     &    12*SBA2*AllHahx(654)

	AllHahx(659) = 19*S2B**5*SBA2 + S2B**4*AllHahx(658)

        AllHahx(664) =
     &   336*CB2**3*Lambda6*SB2**2 - 336*CB2**2*Lambda7*SB2**3 -
     &    40*S2B**4*(CB2*Lambda6 - Lambda7*SB2) +
     &    3*SBA2*AllHahx(663)

	AllHahx(668) = -7*S2B**5*SBA2 + S2B**4*AllHahx(667)

        AllHahx(671) =
     &   400*CB2**3*Lambda6*SB2**2 - 400*CB2**2*Lambda7*SB2**3 -
     &    31*S2B**4*(CB2*Lambda6 - Lambda7*SB2) +
     &    12*SBA2*AllHahx(670)

        AllHahx(689) =
     &   CBA*SAB - (2*CAB + C2B*CBA)*SBA - (3*S2A + S2B)*SBA2

	AllHahx(693) = 4*Mh02**4*AllHahx(691) + MHH2**4*AllHahx(692)

	AllHahx(694) = 19*CAB*SBA + 2*(12*S2A - 5*S2B)*SBA2

        AllHahx(695) =
     &   -8*m12sq*SAB + (3*MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*(5 - 16*SBA2))

        AllHahx(709) =
     &   -29*S2B**4*(CB2*Lambda6 - Lambda7*SB2) +
     &    68*SB2**2*AllHahx(669)

	AllHahx(713) = (98*CAB + 29*C2B*CBA)*SBA + 54*S2A*SBA2

        AllHahx(717) =
     &   4*SB2**2*AllHahx(635) + CB2*Lambda6*AllHahx(715) -
     &    SB2*AllHahx(716)

	AllHahx(721) = (169*CAB + 23*C2B*CBA)*SBA + 135*S2A*SBA2

        AllHahx(725) =
     &   4*SB2**2*AllHahx(644) + 7*CB2*Lambda6*AllHahx(723) -
     &    7*SB2*AllHahx(724)

	AllHahx(729) = 2*(41*CAB + 2*C2B*CBA)*SBA + 81*S2A*SBA2

        AllHahx(733) =
     &   8*SB2**2*AllHahx(662) + CB2*Lambda6*AllHahx(731) -
     &    SB2*AllHahx(732)

	AllHahx(737) = (32*CAB + 7*C2B*CBA)*SBA + 22*S2A*SBA2

        AllHahx(741) =
     &   4*SB2**2*AllHahx(653) + CB2*Lambda6*AllHahx(739) -
     &    SB2*AllHahx(740)

	AllHahx(756) = 3*CAB*SBA + (9*S2A - 2*S2B)*SBA2

	AllHahx(757) = 8*CBA*SAB - 26*S2A*SBA2 - S2B*(9 + 2*SBA2)

        AllHahx(761) =
     &   32*m12sq*S2B**4*SAB +
     &    (MW2*SW2)/(Alfa*Pi)*(SBA*AllHahx(760))

        AllHahx(770) =
     &   -48*CB2**3*Lambda6*SB2**2*(4 + 39*SBA2) +
     &    24*CB2**2*AllHahx(766) +
     &    Lambda7*S2B**3*SB2*AllHahx(767) + CB2*S2B*AllHahx(769)

        AllHahx(781) =
     &   -48*SBA2*AllHahx(777) - Lambda7*S2B**3*SB2*AllHahx(778) +
     &    CB2*S2B*AllHahx(780)

	AllHahx(784) = 100*CBA*SAB + 72*CAB*SBA + 11*S2A*SBA2

	AllHahx(786) = 69*CAB*SBA + (72*S2A - 5*S2B)*SBA2

        AllHahx(792) =
     &   -8*m12sq*S2B**4*(25*SAB - 4*C2B*SBA) +
     &    (MW2*SW2)/(Alfa*Pi)*(SBA*AllHahx(791))

	AllHahx(794) = 84*CBA*SAB + 233*CAB*SBA + 192*S2A*SBA2

        AllHahx(801) =
     &   -28*m12sq*S2B**4*(2*SAB - C2B*SBA) +
     &    (MW2*SW2)/(Alfa*Pi)*(SBA*AllHahx(800))

	AllHahx(828) = 1 + 3*SBA2**2 + 2*(CBA2**2 + CBA2*SBA2)

	AllHahx(830) = 2 + 4*CBA2**2 + 5*CBA2*SBA2 + 6*SBA2**2

	AllHahx(838) = 2*CBA2 - (1 - SBA2)**2

        AllHahx(843) =
     &   -4*m12sq*(MHH2 - MHp2) + Mh02*AllHahx(841) -
     &    MHH2*AllHahx(842)

	AllHahx(1096) = (7 - 3*CBA2)*Mh02 - 2*(5 - 6*CBA2)*MHp2

        AllHahx(1099) =
     &   20*MHp2**2*S2B - 12*Mh02*MHp2*(S2A + 2*S2B) +
     &    Mh02**2*(3*S2A + 7*S2B)

	AllHahx(1104) = (4 - 3*CBA2)*Mh02 - (7 - 12*CBA2)*MHp2

        AllHahx(1105) =
     &   -(Mh02*(Mh02 + 5*MHp2)*S2B) + 2*m12sq*AllHahx(1104)

	AllHahx(1107) = 3*CBA2*(Mh02 - 4*MHp2) + 7*MHp2

        AllHahx(1109) =
     &   S2B*SBA*AllHahx(1108) -
     &    (8*MW2*SW2)/(Alfa*Pi)*(CB2*CBA*Lambda6*Mh02)

        AllHahx(1112) =
     &   (8 + 4*CBA2 + 27*CBA2**3)*Mh02**2 -
     &    2*(8 + 4*CBA2 + 63*CBA2**3)*Mh02*MHp2 +
     &    4*(2 + CBA2 + 18*CBA2**3)*MHp2**2

        AllHahx(1113) =
     &   (8 + 4*CBA2 + 27*CBA2**2)*Mh02**2 -
     &    2*(8 + 4*CBA2 + 63*CBA2**2)*Mh02*MHp2 +
     &    4*(2 + CBA2 + 18*CBA2**2)*MHp2**2

        AllHahx(1114) =
     &   (8 + 31*CBA2)*Mh02**2 - 2*(8 + 67*CBA2)*Mh02*MHp2 +
     &    4*(2 + 19*CBA2)*MHp2**2

	AllHahx(1120) = 27*S2A**2 + 4*S2B**2

        AllHahx(1122) =
     &   3*MHp2*SBA2*AllHahx(1121) +
     &    (8*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHahx(1125) =
     &   MHp2**2*SBA2*AllHahx(1124) +
     &    (16*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*MHp2*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHahx(1128) =
     &   m12sq*S2A*SBA - (MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2))

        AllHahx(113) =
     &   MHH2**2*SBA2 - MHp2**2*(1 + SBA2) +
     &    MHH2*(MHp2 + 3*MHp2*SBA2)

        AllHahx(1137) =
     &   8*Mh02**3 - 8*(8 + CBA2)*Mh02**2*MHp2 +
     &    (104 + 16*CBA2 + 9*CBA2**3)*Mh02*MHp2**2 -
     &    4*(12 + 2*CBA2 + 9*CBA2**3)*MHp2**3

        AllHahx(1138) =
     &   8*Mh02**3 - 8*(8 + CBA2)*Mh02**2*MHp2 +
     &    (104 + 16*CBA2 + 9*CBA2**2)*Mh02*MHp2**2 -
     &    4*(12 + 2*CBA2 + 9*CBA2**2)*MHp2**3

        AllHahx(1139) =
     &   8*Mh02**3 - 8*(8 + CBA2)*Mh02**2*MHp2 +
     &    (104 + 25*CBA2)*Mh02*MHp2**2 - 4*(12 + 11*CBA2)*MHp2**3

	AllHahx(1149) = 3*S2A**2*S2B**2 + 16*S2B**4

        AllHahx(1170) =
     &   -18*MHp2**3*SBA2 +
     &    MHH2*MHp2**2*(8 + 11*SBA2 - 16*CW2*SW2) -
     &    2*MHH2**2*MHp2*(7 + SBA2 - 10*CW2*SW2) +
     &    MHH2**3*AllHahx(1169)

	AllHahx(1219) = 3 - 4*(3 - 2*CW2)*SW2 + 12*SW2**2

	AllHahx(1265) = 2*Mh02**2 - 3*Mh02*MHH2 + MHH2**2

	AllHahx(1287) = MHH2**2 - 2*(Mh02**2 + Mh02*MHH2)

	AllHahx(1301) = -3*(4*Mh02 + MHH2)*S2A + (4*Mh02 - MHH2)*S2B

        AllHahx(1302) =
     &   -11*MHH2*S2A*S2B +
     &    (24*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHahx(1303) =
     &   MHH2*S2A*S2B - (3*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

	AllHahx(1319) = -((Mh02 - 6*MHH2)*(Mh02 - 2*MHH2)) + 4*MHH2**2

	AllHahx(1325) = 2*CBA2*MHH2 - (Mh02 - MHH2)*SBA2

	AllHahx(1348) = 2*S2A*S2B - 9*(S2A**2 + S2B**2)

        AllHahx(1352) =
     &   MHH2*S2B*AllHahx(1350) -
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA*AllHahx(1351))

        AllHahx(1353) =
     &   MHH2*S2B*(S2A + S2B)*SBA2 +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA*(1 - 3*SBA2))

        AllHahx(1356) =
     &   MHH2*S2B*AllHahx(1354) +
     &    (6*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA*AllHahx(1355))

        AllHahx(1359) =
     &   MHH2*S2B*AllHahx(1357) -
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA*AllHahx(1358))

        AllHahx(1362) =
     &   MHH2*S2B*AllHahx(1360) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA*AllHahx(1361))

        AllHahx(1365) =
     &   MHH2*S2B*AllHahx(1363) +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA*AllHahx(1364))

        AllHahx(1371) =
     &   -32*MHH2**4*(3*S2A + S2B)*SBA2 +
     &    3*Mh02**4*AllHahx(1367) -
     &    2*Mh02**2*MHH2**2*AllHahx(1368) +
     &    8*Mh02*MHH2**3*AllHahx(1369) + Mh02**3*MHH2*AllHahx(1370)

	AllHahx(138) = 6*C2B*SBA + SAB*SBA2*(1 + 6*SBA2)

        AllHahx(1387) =
     &   Mh02**5*S2B*(SBA2*AllHahx(1372) -
     &       3*CBA*SAB*AllHahx(1373)) -
     &    64*MHH2**4*(3*S2A + S2B)*AllHahx(1374) +
     &    4*Mh02**2*MHH2**2*AllHahx(1377) +
     &    16*Mh02*MHH2**3*AllHahx(1380) + Mh02**4*AllHahx(1383) -
     &    Mh02**3*MHH2*AllHahx(1386)

        AllHahx(1422) =
     &   -2*S2A**2*(138 + 9059*SBA2) -
     &    4*S2A*(525*CBA*SAB + 2038*S2B*SBA2) + 3*AllHahx(1421)

	AllHahx(1424) = 16*SAB*(7 - 723*SBA2) + 3*CBA*AllHahx(1423)

        AllHahx(1426) =
     &   24*(CBA*S2B*SAB + SAB**2) + S2A**2*(124 - 15957*SBA2) +
     &    8809*S2B**2*SBA2 - 4*S2A*(2438*CBA*SAB + 8043*S2B*SBA2)

	AllHahx(1428) = 2*SAB*(643 - 1983*SBA2) + CBA*AllHahx(1427)

        AllHahx(1431) =
     &   S2A**2*(8 - 2079*SBA2) -
     &    8*S2A*(17*CBA*SAB - 48*S2B*SBA2) - 4*AllHahx(1430)

	AllHahx(1433) = -2*SAB*(419 + 1713*SBA2) + CBA*AllHahx(1432)

        AllHahx(1435) =
     &   -10968*(CBA*S2B*SAB + SAB**2) + 4027*S2B**2*SBA2 +
     &    S2A**2*(366 + 22657*SBA2) -
     &    4*S2A*(304*CBA*SAB + 1961*S2B*SBA2)

	AllHahx(1437) = 4*SAB*(3421 + 14415*SBA2) + CBA*AllHahx(1436)

        AllHahx(1440) =
     &   S2A**2*(96 - 50473*SBA2) -
     &    8*(S2A*(273*CBA*SAB - 808*S2B*SBA2) + AllHahx(1439))

        AllHahx(1442) =
     &   2*SAB*(9191 + 38253*SBA2) + 3*CBA*AllHahx(1441)

        AllHahx(1445) =
     &   MHH2*S2B*AllHahx(1444) +
     &    (144*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*S2A*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHahx(1449) =
     &   MHH2*S2B*AllHahx(1447) +
     &    (8*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA*AllHahx(1448))

        AllHahx(1459) =
     &   -(MHH2*S2B*AllHahx(1457)) +
     &    (16*MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*AllHahx(1458))

        AllHahx(1465) =
     &   MHH2*S2B*AllHahx(1462) -
     &    (6*MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*AllHahx(1464))

        AllHahx(1471) =
     &   MHH2*S2B*AllHahx(1468) -
     &    (6*MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*AllHahx(1470))

        AllHahx(1477) =
     &   MHH2*S2B*AllHahx(1474) -
     &    (6*MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*AllHahx(1476))

        AllHahx(1481) =
     &   MHH2*S2B*(36*SAB**2 - SBA2*AllHahx(1478)) -
     &    CBA*AllHahx(1480) -
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (SAB*(CB2*Lambda6 - Lambda7*SB2)*SBA*
     &       (23 + 12*SBA2 + 81*SBA2**2))

	AllHahx(1482) = 57*S2A**2 - 10*S2A*S2B - 2*S2B**2

        AllHahx(1483) =
     &   2*MHH2*S2A*S2B*SAB +
     &    (9*MW2*SW2)/(Alfa*Pi)*
     &     ((3*S2A - S2B)*(CB2*Lambda6 - Lambda7*SB2)*SBA*SBA2)

        AllHahx(1495) =
     &   -24*Mh02**5*(3*S2A - S2B)**2*SBA2 -
     &    MHH2**5*AllHahx(1486) - Mh02*MHH2**4*AllHahx(1488) +
     &    Mh02**2*MHH2**3*AllHahx(1490) + 4*AllHahx(1494)

        AllHahx(1509) =
     &   CBA2*(Mh02 - 4*MHH2)*AllHahx(1506) -
     &    3*(Mh02 - 4*MHH2)**2*AllHahx(1508)

        AllHahx(1510) =
     &   9*Mh02**4 - 33*Mh02**3*MHH2 - 8*Mh02**2*MHH2**2 +
     &    64*Mh02*MHH2**3 - 32*MHH2**4

	AllHahx(1512) = 3*Mh02**2 - 2*Mh02*MHH2 - MHH2**2

        AllHahx(1514) =
     &   MHH2**3*(5 - 18*SBA2) - 8*Mh02**3*(1 + 9*SBA2) -
     &    6*AllHahx(1513)

        AllHahx(1527) =
     &   -((Mh02 - MHH2)*SBA*SBA2*AllHahx(1519)) +
     &    CBA*AllHahx(1526)

        AllHahx(1553) =
     &   CBA2*(Mh02 - 4*MHH2)*AllHahx(1542) +
     &    6*(Mh02 - 4*MHH2)**2*AllHahx(1552)

	AllHahx(1776) = -(m12sq*(9*S2A - S2B)) + Mh02*S2A*S2B

	AllHahx(1812) = Mh02**2 + 4*(Mh02*MHH2 + MHH2**2)

        AllHahx(182) =
     &   SAB*AllHahx(177) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (SBA*(SB2*AllHahx(179) + CB2*AllHahx(181)))

        AllHahx(188) =
     &   CB2*S2B**2*(47 - 204*SBA2 - 63*SBA2**2) +
     &    8*CB2**2*SB2*(7 - 12*SBA2 + 9*SBA2**2)

        AllHahx(193) =
     &   -53*MHH2*(3*S2A - S2B)*S2B**2 + 2*m12sq*S2B*AllHahx(192)

	AllHahx(195) = -64*Lambda7*SB2**2 + 3*Lambda6*S2B*AllHahx(194)

	AllHahx(197) = 112*CB2**2*Lambda6 - 3*Lambda7*S2B*AllHahx(196)

        AllHahx(203) =
     &   CB2*S2B**2*(13 - 84*SBA2 - 45*SBA2**2) -
     &    24*CB2**2*SB2*(7 - 12*SBA2 + 9*SBA2**2)

        AllHahx(213) =
     &   SAB*AllHahx(208) -
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (SBA*(CB2*AllHahx(210) - SB2*AllHahx(212)))

	AllHahx(269) = 19*S2B + 2*CAB*SBA - (S2A + 4*S2B)*SBA2

        AllHahx(270) =
     &   -2*(4*CAB + 3*C2B*CBA)*m12sq +
     &    (7*MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

	AllHahx(284) = Mh02**2*S2A + MHH2**2*(3*S2A + S2B)

	AllHahx(314) = Mh02**2 - 4*Mh02*MHH2 + MHH2**2

        AllHahx(321) =
     &   Mh02**2*(4*Mh02 - MHH2)*S2A - (Mh02 - MHH2)**3*S2B

	AllHahx(323) = CB2*S2B**2 - 4*CB2**2*SB2

	AllHahx(332) = 4*m12sq*(Mh02 - MHH2)**2 + S2A*AllHahx(331)

	AllHahx(388) = 2*CAB*SBA + 3*S2B*(3 + 10*SBA2)

	AllHahx(394) = MHH2*(2*S2A - 9*S2B) + MHp2*(8*S2A + 21*S2B)

        AllHahx(396) =
     &   5*MHp2*S2B**2 - 4*MHH2*S2B*(S2A + S2B) +
     &    (24*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHahx(398) =
     &   MHp2*S2A*S2B + (3*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHahx(399) =
     &   MHp2*S2B*(8*S2A + 15*S2B) +
     &    (48*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHahx(401) =
     &   MHp2*S2B**2 - (8*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

	AllHahx(403) = MHH2**2*(8*S2A - S2B)*S2B - MHH2*AllHahx(402)

        AllHahx(409) =
     &   6*m12sq*SAB*SBA2 +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*(2 + SBA2 + 6*SBA2**2))

	AllHahx(411) = 11*S2B - 10*CBA*SAB - (4*S2A - 7*S2B)*SBA2

        AllHahx(412) =
     &   6*m12sq*SAB*SBA2 +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*
     &       (1 + 2*SBA2 + 6*SBA2**2))

        AllHahx(420) =
     &   m12sq*(2*SAB + C2B*SBA) -
     &    (MW2*SW2)/(Alfa*Pi)*((CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHahx(425) =
     &   -2*m12sq*(4*SAB + C2B*SBA) +
     &    (3*MW2*SW2)/(Alfa*Pi)*((CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHahx(428) =
     &   -4*m12sq*SAB + (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHahx(430) =
     &   4*CBA*SAB - 10*CAB*SBA - 14*S2A*SBA2 + S2B*(7 + 11*SBA2)

	AllHahx(433) = S2B*(8 - 5*SBA2) - 6*S2A*SBA2

        AllHahx(434) =
     &   16*CBA*SAB - 4*CAB*SBA - 8*S2A*SBA2 - S2B*(28 + 111*SBA2)

        AllHahx(436) =
     &   2*CBA*SAB - CAB*SBA + S2B*(1 - 10*SBA2) - 4*S2A*SBA2

	AllHahx(438) = 4*CBA*SAB + 7*S2A*SBA2 - 2*S2B*(13 + 34*SBA2)

        AllHahx(439) =
     &   -2*m12sq*(3*CAB*SBA - 4*S2B*SBA2) + CBA*AllHahx(428)

	AllHahx(440) = MHp2*S2B**2*(6 + 19*SBA2) + 2*AllHahx(439)

	AllHahx(476) = 2*MHp2 + (Mh02 - MHH2)*(1 + SBA2)

	AllHahx(483) = MHp2**2 - 3*(MHH2**2 + MHH2*MHp2)

	AllHahx(487) = 2*C2B*CBA*m12sq + CAB*(8*m12sq + 3*MHp2*S2B)

	AllHahx(490) = 4*CAB*SBA + 5*S2A*SBA2 - 2*S2B*(7 + 13*SBA2)

	AllHahx(493) = 2*CAB*SBA + S2B*(3 + 14*SBA2)

	AllHahx(495) = 6*CAB*SBA + 5*S2A*SBA2 - S2B*(11 + 16*SBA2)

	AllHahx(497) = 3*S2A*SBA2 - S2B*(5 + 18*SBA2)

	AllHahx(499) = CAB*SBA - S2A*SBA2 + 5*S2B*(1 + SBA2)

	AllHahx(503) = S2A*SBA2 + 6*S2B*(1 + 2*SBA2)

	AllHahx(508) = CAB*SBA + 6*S2B*(1 + 2*SBA2)

	AllHahx(512) = 4*CAB*SBA - 4*S2A*SBA2 + 7*S2B*(3 + 7*SBA2)

        AllHahx(521) =
     &   Mh02**2 + MHH2**2 - 2*(Mh02 + MHH2)*MHp2 + 2*MHp2**2

        AllHahx(524) =
     &   SBA2 + (CBA2*AllHahx(521))/((Mh02 - MHp2)*(MHH2 - MHp2))

	AllHahx(533) = 4*MHH2*(MHH2 - MHp2)*SAB + CBA*AllHahx(532)

	AllHahx(537) = 2*Mh02*S2A - 2*MHp2*S2B + MHH2*(S2A + S2B)

        AllHahx(540) =
     &   -2*MHH2*(S2B - S2A*SBA2) +
     &    2*(CAB*Mh02*SBA + MHp2*S2B*(1 + SBA2)) -
     &    SBA2*AllHahx(539)

        AllHahx(545) =
     &   Mh02*MHH2 + 2*(Mh02**2 + MHH2**2) -
     &    5*(Mh02 + MHH2)*MHp2 + 5*MHp2**2

        AllHahx(549) =
     &   2*m12sq*MHH2*(Mh02 - MHp2)**2*(3*S2A - S2B) -
     &    (Mh02 - MHH2)*MHp2*S2A*S2B*AllHahx(546) +
     &    S2B**2*AllHahx(548)

        AllHahx(552) =
     &   Mh02*(MHH2 - MHp2)*MHp2 -
     &    Mh02**2*(MHH2 - MHp2 + 3*MHH2*SBA2) + 3*SBA2*AllHahx(551)

        AllHahx(554) =
     &   8*m12sq*Mh02*(Mh02 - MHp2)*(MHH2 - MHp2)*SAB*SB2 +
     &    (MW2*SW2)/(Alfa*Pi)*(Lambda6*S2B**2*SBA*AllHahx(553))

	AllHahx(558) = 2*(S2B + CAB*SBA) + S2A*SBA2

	AllHahx(559) = CAB*SBA + S2B*(1 - SBA2)

        AllHahx(562) =
     &   MHH2**3*(S2A + S2B)*SBA2 + MHH2**2*MHp2*AllHahx(561)

        AllHahx(563) =
     &   MHp2*S2B*(S2A + 2*S2B)*SBA2 +
     &    CBA*SBA*(-4*C2B*m12sq +
     &       (2*MW2*SW2)/(Alfa*Pi)*(CB2*Lambda6 - Lambda7*SB2))

	AllHahx(566) = S2B - 3*CAB*SBA - (S2A - 2*S2B)*SBA2

	AllHahx(567) = 3*CAB*SBA - 2*S2B*(2 + 3*SBA2)

	AllHahx(568) = S2B*(3 - SBA2) + S2A*SBA2

	AllHahx(570) = 2*CAB*SBA - S2B*(5 - 3*SBA2)

        AllHahx(572) =
     &   MHH2**5*(S2B + (S2A + 2*S2B)*SBA2) +
     &    MHH2**2*MHp2**3*AllHahx(571)

	AllHahx(573) = 2*CAB*SBA + S2B*(5 + 8*SBA2)

	AllHahx(574) = 4*S2A*SBA2 + 5*S2B*(1 + 2*SBA2)

        AllHahx(578) =
     &   3*MHH2**3*AllHahx(576) + MHH2*MHp2**2*AllHahx(577)

	AllHahx(579) = 6*CAB*SBA - S2A*SBA2 - S2B*(3 + SBA2)

	AllHahx(580) = 5*S2A*SBA2 + S2B*(11 + 5*SBA2)

	AllHahx(582) = 2*CAB*SBA - 2*S2A*SBA2 - S2B*(8 + 5*SBA2)

	AllHahx(583) = 6*CAB*SBA + 5*S2A*SBA2 + S2B*(6 + 19*SBA2)

	AllHahx(584) = 3*S2A*SBA2 - 2*S2B*(5 + 14*SBA2)

	AllHahx(590) = 2*MHH2**2 - MHH2*MHp2 - 4*MHp2**2

        AllHahx(591) =
     &   MHH2**3 + MHH2**2*MHp2 - 2*(MHH2*MHp2**2 + MHp2**3)

        AllHahx(602) =
     &   MHH2**2*S2B*SBA2*(S2B - 8*CBA*SAB + S2B*SBA2) +
     &    MHp2*AllHahx(600) - 2*MHH2*AllHahx(601)

	AllHahx(627) = 4*CAB*SBA - S2B*(14 - 3*SBA2) - 10*S2A*SBA2

	AllHahx(628) = 342*S2B - 228*CAB*SBA + (74*S2A - 135*S2B)*SBA2

        AllHahx(638) =
     &   2*m12sq*AllHahx(630) +
     &    (MW2*SW2)/(Alfa*Pi)*(CBA*SBA*AllHahx(637))

	AllHahx(639) = 133*S2B - 26*CAB*SBA - (205*S2A + 303*S2B)*SBA2

        AllHahx(647) =
     &   -6*m12sq*AllHahx(641) +
     &    (MW2*SW2)/(Alfa*Pi)*(CBA*SBA*AllHahx(646))

        AllHahx(648) =
     &   538*S2B - 264*CAB*SBA - (122*S2A + 585*S2B)*SBA2

        AllHahx(656) =
     &   -2*m12sq*AllHahx(650) +
     &    (MW2*SW2)/(Alfa*Pi)*(CBA*SBA*AllHahx(655))

	AllHahx(657) = 6*S2B - (358*S2A + 261*S2B)*SBA2

        AllHahx(665) =
     &   m12sq*AllHahx(659) +
     &    (MW2*SW2)/(Alfa*Pi)*(CBA*SBA*AllHahx(664))

	AllHahx(666) = 2*S2B + (26*S2A + 9*S2B)*SBA2

        AllHahx(672) =
     &   -2*m12sq*AllHahx(668) +
     &    (MW2*SW2)/(Alfa*Pi)*(CBA*SBA*AllHahx(671))

        AllHahx(681) =
     &   21*CBA*SAB + (9*CAB - 4*C2B*CBA)*SBA - 12*S2B*SBA2

        AllHahx(682) =
     &   -28*(3*CBA*SAB + 8*CAB*SBA) + 3*S2B*(28 + 53*SBA2)

        AllHahx(684) =
     &   84*S2B - 84*CBA*SAB - 436*CAB*SBA - (34*S2A - 99*S2B)*SBA2

        AllHahx(685) =
     &   21*CBA*SAB - (14*CAB + 17*C2B*CBA)*SBA -
     &    2*(12*S2A + 5*S2B)*SBA2

        AllHahx(688) =
     &   8*CBA*SAB + 110*CAB*SBA + 27*S2A*SBA2 - S2B*(8 + 15*SBA2)

        AllHahx(690) =
     &   m12sq*AllHahx(689) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA*(2 - 3*SBA2))

        AllHahx(696) =
     &   S2B*AllHahx(693) -
     &    2*MHH2**3*(2*m12sq*AllHahx(694) - CBA*AllHahx(695))

        AllHahx(703) =
     &   MHp2*S2B**2*(1 + SBA2) +
     &    MHH2*S2B*(S2A + 14*CBA*SAB - S2B*SBA2)

	AllHahx(707) = 6*S2A*SBA2 + S2B*(3 + 4*SBA2)

	AllHahx(708) = S2B**5*SBA2 - 3*S2B**4*(2*CAB*SBA + 3*S2A*SBA2)

        AllHahx(710) =
     &   400*CB2**3*Lambda6*SB2**2 - 400*CB2**2*Lambda7*SB2**3 -
     &    S2B**4*(CB2*Lambda6 - Lambda7*SB2) + 6*SBA2*AllHahx(709)

	AllHahx(714) = 14*S2B**5*SBA2 + S2B**4*AllHahx(713)

        AllHahx(718) =
     &   127*S2B**4*(CB2*Lambda6 - Lambda7*SB2) +
     &    6*SBA2*AllHahx(717)

	AllHahx(722) = 15*S2B**5*SBA2 + S2B**4*AllHahx(721)

        AllHahx(726) =
     &   400*CB2**3*Lambda6*SB2**2 - 400*CB2**2*Lambda7*SB2**3 +
     &    359*S2B**4*(CB2*Lambda6 - Lambda7*SB2) +
     &    6*SBA2*AllHahx(725)

	AllHahx(730) = S2B**5*SBA2 + S2B**4*AllHahx(729)

        AllHahx(734) =
     &   336*CB2**3*Lambda6*SB2**2 - 336*CB2**2*Lambda7*SB2**3 +
     &    65*S2B**4*(CB2*Lambda6 - Lambda7*SB2) +
     &    6*SBA2*AllHahx(733)

	AllHahx(738) = 38*S2B**5*SBA2 + 9*S2B**4*AllHahx(737)

        AllHahx(742) =
     &   64*CB2**3*Lambda6*SB2**2 - 64*CB2**2*Lambda7*SB2**3 +
     &    347*S2B**4*(CB2*Lambda6 - Lambda7*SB2) +
     &    6*SBA2*AllHahx(741)

	AllHahx(744) = 230*CAB*SBA + 4*S2B*(5 - 28*SBA2) + 77*S2A*SBA2

	AllHahx(748) = MHp2*S2B + 2*MHH2*(S2A + 3*S2B)

        AllHahx(752) =
     &   MHp2*S2B**2*(1 + SBA2) +
     &    MHH2*S2B*(S2A + 6*CBA*SAB - S2B*SBA2)

	AllHahx(755) = 42*S2B - 16*CAB*SBA + (24*S2A - 17*S2B)*SBA2

        AllHahx(762) =
     &   -32*m12sq*S2B**4*AllHahx(756) -
     &    2*MHH2*S2B**5*AllHahx(757) + CBA*AllHahx(761)

	AllHahx(765) = 21*CAB*SBA - (72*S2A + 5*S2B)*SBA2

        AllHahx(771) =
     &   4*m12sq*S2B**4*(25*SAB - 21*C2B*SBA) +
     &    (MW2*SW2)/(Alfa*Pi)*(SBA*AllHahx(770))

	AllHahx(773) = 8*CBA*SAB + 222*CAB*SBA + 79*S2A*SBA2

	AllHahx(775) = 3*CAB*SBA + (9*S2A + 2*S2B)*SBA2

        AllHahx(782) =
     &   -8*m12sq*S2B**4*(SAB - C2B*SBA) +
     &    (MW2*SW2)/(Alfa*Pi)*(SBA*AllHahx(781))

	AllHahx(785) = S2B*(85 + 202*SBA2) - 2*AllHahx(784)

	AllHahx(793) = 8*m12sq*S2B**4*AllHahx(786) + CBA*AllHahx(792)

	AllHahx(795) = S2B*(273 - 668*SBA2) + 4*AllHahx(794)

        AllHahx(802) =
     &   8*m12sq*S2B**4*(15*CAB*SBA + S2B*SBA2) + CBA*AllHahx(801)

        AllHahx(829) =
     &   -8*CBA2*(CBA*S2B*SAB - SAB**2) + S2B**2*AllHahx(828)

        AllHahx(839) =
     &   -((3*CBA2*S2A - 4*CBA*SAB)*SBA2) + S2B*AllHahx(838)

	AllHahx(866) = 3*SBA2 - 4*(1 - 2*SW2)**2

	AllHahx(867) = SBA2 - 2*(1 - 2*SW2)**2

	AllHahx(99) = 2*MHH2 + Mh02*SBA2 - 2*MHp2*(1 + SBA2)

        AllHahx(100) =
     &   S2B*(2*CAB*Mh02*SBA2 - S2B*SBA*AllHahx(99)) -
     &    SBA2*(4*CAB*m12sq -
     &       (2*MW2*SW2)/(Alfa*Pi)*
     &        (CBA*(CB2*Lambda6 - Lambda7*SB2)))

	AllHahx(1001) = Mh02 - ((Mh02 - MHp2)*MHp2)/(MHH2 - 4*MHp2)

        AllHahx(1003) =
     &   2*m12sq*(3*S2A - S2B) - (2*Mh02 + MHH2)*S2A*S2B +
     &    (6*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHahx(1011) =
     &   m12sq*(3*S2A + S2B)*SBA2 + MHH2*S2B*(S2B - S2A*SBA2)

        AllHahx(1017) =
     &   Mh02*(3*MHH2 - 8*MHp2) - 10*MHH2*MHp2 + 24*MHp2**2

	AllHahx(1019) = CBA2*(MHH2 - 4*MHp2) - 2*(Mh02 - 4*MHp2)*SBA2

	AllHahx(1023) = Mh02*S2B*(S2B + S2A*SBA2) - 2*AllHahx(1011)

        AllHahx(107) =
     &   -2*MHH2*MHp2*SBA2 + Mh02*(MHH2 - MHp2 + 2*MHH2*SBA2)

        AllHahx(1087) =
     &   -((2 - 3*CBA2)*Mh02**2) + 6*(1 - 3*CBA2)*Mh02*MHp2 -
     &    2*(5 - 12*CBA2)*MHp2**2

	AllHahx(1088) = Mh02**2 - 12*Mh02*MHp2 + 14*MHp2**2

        AllHahx(1091) =
     &   10*MHp2**2*S2B - 6*Mh02*MHp2*(2*S2A + S2B) +
     &    Mh02**2*(3*S2A + 2*S2B)

	AllHahx(1092) = Mh02**2 - 3*Mh02*MHp2 + 5*MHp2**2

        AllHahx(1097) =
     &   -(Mh02*(2*Mh02 + MHp2)*S2B) + m12sq*AllHahx(1096)

        AllHahx(110) =
     &   MHH2*MHp2**2*(1 + 2*CBA2 - 11*SBA2) -
     &    MHH2**2*MHp2*(2 + CBA2 - 10*SBA2) +
     &    MHH2**3*(1 - 4*SBA2) - MHp2**3*(CBA2 - 4*SBA2)

        AllHahx(1100) =
     &   S2B*SBA*AllHahx(1099) -
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(7*Mh02 - 10*MHp2)*(CB2*Lambda6 - Lambda7*SB2))

        AllHahx(1106) =
     &   2*(Mh02 - MHp2)**2*(2*m12sq - Mh02*S2B) +
     &    CBA2*MHp2*AllHahx(1105)

        AllHahx(111) =
     &   CBA2*MHp2**2 - MHH2*MHp2*(1 + 2*CBA2 - 2*SBA2) +
     &    MHH2**2*(1 + CBA2 - SBA2)

        AllHahx(1110) =
     &   AllHahx(1109) + (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(Lambda7*SB2*AllHahx(1104) +
     &         CB2*Lambda6*AllHahx(1107)))

        AllHahx(1115) =
     &   4*m12sq**2*AllHahx(1112) -
     &    4*m12sq*Mh02*S2B*AllHahx(1113) +
     &    Mh02**2*S2B**2*AllHahx(1114)

        AllHahx(112) =
     &   2*CBA2*MHp2**3 + MHH2**3*(1 + CBA2 - SBA2) -
     &    MHH2*MHp2**2*(1 + 3*CBA2 - SBA2) + 3*MHH2**2*MHp2*SBA2

        AllHahx(1123) =
     &   Mh02**4*SBA2*AllHahx(1120) - 2*Mh02**3*AllHahx(1122)

        AllHahx(1129) =
     &   CBA2**2*Mh02*S2B*AllHahx(1128) +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*CBA2**3*m12sq*(CB2*Lambda6 - Lambda7*SB2))

        AllHahx(114) =
     &   CBA2*(MHH2 - MHp2)**2*(2*MHH2 + MHp2) + MHH2*AllHahx(113)

        AllHahx(1140) =
     &   4*m12sq**2*AllHahx(1137) -
     &    4*m12sq*Mh02*S2B*AllHahx(1138) +
     &    Mh02**2*S2B**2*AllHahx(1139)

        AllHahx(116) =
     &   -2*m12sq*(3*S2A - S2B) + (2*Mh02 + MHH2)*S2A*S2B +
     &    (MHH2 - 2*MHp2)*S2B**2

        AllHahx(1166) =
     &   4*MHp2**2*SBA2 + MHH2*MHp2*(3 - 5*SBA2 - 4*CW2*SW2) -
     &    MHH2**2*(5 - SBA2 - 4*CW2*SW2)

        AllHahx(117) =
     &   (2*m12sq - MHH2*S2B)*SAB +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((1 - 3*CBA2)*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHahx(1171) =
     &   10*MHp2**3*SBA2 + MHH2**2*MHp2*(9 - SBA2 - 40*CW2*SW2) +
     &    2*AllHahx(1170) - 4*(1 - 8*CW2*SW2)*(MHH2*MHp2**2)

	AllHahx(1175) = 6*S2A*SBA2 - S2B*(2 + 5*SBA2)

	AllHahx(1177) = 2*MHH2*SBA2 + MHp2*(2 + 5*SBA2)

	AllHahx(1178) = 2*MHH2*SBA2 + MHp2*(2 - (1 - 6*SBA2)*SBA2)

        AllHahx(1218) =
     &   3*MHH2**3 - 14*MHH2**2*MHp2 - 10*MHp2**3*SBA2 +
     &    MHH2*MHp2**2*(8 + SBA2)

        AllHahx(1220) =
     &   -18*MHp2**3*SBA2 +
     &    MHH2*MHp2**2*(8 + 11*SBA2 - 16*CW2*SW2) -
     &    2*MHH2**2*MHp2*(7 + SBA2 - 10*CW2*SW2) +
     &    MHH2**3*AllHahx(1219)

        AllHahx(1247) =
     &   -2*MHH2*S2A*SB2 +
     &    (3*MW2*SW2)/(Alfa*Pi)*(CBA*Lambda6*S2B*SBA)

	AllHahx(1252) = Mh02*(S2A - S2B) + 2*MHH2*(S2A + S2B)

        AllHahx(1283) =
     &   (5*Mh02 + 4*MHH2)*S2A*S2B - 4*m12sq*(3*S2A + S2B)

        AllHahx(1284) =
     &   -(CAB*(2*CBA2*m12sq - Mh02*S2B)) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*(CBA2 - 2*SBA2))

        AllHahx(1288) =
     &   2*m12sq*(Mh02 - MHH2)*(3*S2A - S2B) +
     &    S2A*S2B*AllHahx(1287)

        AllHahx(1304) =
     &   8*Mh02**2*S2A*S2B - Mh02*AllHahx(1302) +
     &    2*(m12sq*AllHahx(1301) + MHH2*AllHahx(1303))

        AllHahx(1366) =
     &   Mh02**6*S2B*SBA2*AllHahx(1348) +
     &    2*Mh02**4*MHH2*AllHahx(1352) +
     &    128*MHH2**5*(S2A + S2B)*AllHahx(1353) -
     &    32*Mh02*MHH2**4*AllHahx(1356) +
     &    8*Mh02**2*MHH2**3*AllHahx(1359) +
     &    4*Mh02**3*MHH2**2*AllHahx(1362) - Mh02**5*AllHahx(1365)

        AllHahx(1388) =
     &   -(m12sq**2*(3*S2A + S2B)*AllHahx(1371)) +
     &    m12sq*AllHahx(1387)

        AllHahx(139) =
     &   -2*m12sq*AllHahx(138) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*(6 - SBA2 - 6*SBA2**2))

	AllHahx(142) = C2B*SBA*(1 + SBA2) - SAB*SBA2*(1 + 6*SBA2)

	AllHahx(1455) = Mh02**2 - 5*Mh02*MHH2 + 4*MHH2**2

	AllHahx(148) = S2A + S2B - 2*CBA*SAB + (S2A - S2B)*SBA2

        AllHahx(1484) =
     &   -96*Mh02**6*S2A*(3*S2A - S2B)*S2B*SBA2 +
     &    Mh02**3*MHH2**2*AllHahx(1459) +
     &    Mh02*MHH2**4*AllHahx(1465) +
     &    8*Mh02**4*MHH2*AllHahx(1471) -
     &    Mh02**2*MHH2**3*AllHahx(1477) -
     &    4*MHH2**5*AllHahx(1481) +
     &    16*Mh02**5*(MHH2*S2B*SBA2*AllHahx(1482) +
     &       2*CBA*AllHahx(1483))

        AllHahx(149) =
     &   18*CBA*SAB - S2B*(29 - 22*SBA2) - S2A*(9 + 4*SBA2)

        AllHahx(1511) =
     &   2*Mh02*AllHahx(1509) -
     &    (2*m12sq - Mh02*S2B)**2*SBA2*AllHahx(1510)

	AllHahx(1557) = Mh02**2 - 2*Mh02*MHH2 - 4*MHH2**2

	AllHahx(1659) = 3*CBA2*Mh02 + (1 + 3*CBA2)*MHH2 + 2*MHp2

        AllHahx(1667) =
     &   CB2*Lambda6 - Lambda7*SB2 +
     &    3*(CB2*Lambda6 - Lambda7*SB2)*SBA2

        AllHahx(1682) =
     &   -2*MHp2*S2B**2 - MHH2*S2B*(2*S2A + S2B) +
     &    (6*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

	AllHahx(1686) = 2*MHH2*(S2A - S2B) + Mh02*(S2A + S2B)

	AllHahx(171) = 4*CB2**2*Lambda6 - Lambda7*S2B**2

	AllHahx(1713) = (Mh02 - MHH2)**2 + (MHH2 - 2*MHp2)**2

        AllHahx(1716) =
     &   CBA*(2*m12sq - MHH2*S2B)*SAB + Mh02*S2B**2*SBA2

        AllHahx(173) =
     &   MHH2**2*S2B**2 + m12sq**2*(7 - 3*(4 - 3*SBA2)*SBA2)

	AllHahx(1731) = (Mh02 - MHH2)**2 + 2*(Mh02 - MHp2)**2

	AllHahx(1746) = (MHH2 - 2*MHp2)**2 + (Mh02 - MHp2)**2

	AllHahx(1747) = (Mh02 - 2*MHp2)**2 + (MHH2 - MHp2)**2

	AllHahx(1773) = 2*m12sq*(3*S2A - S2B) + Mh02*S2A*S2B

        AllHahx(1777) =
     &   (3*Mh02 - 2*MHH2 - 4*MHp2)*S2B**2 +
     &    6*((1 - CBA2)*MHH2*S2A*S2B + CBA2*AllHahx(1776))

	AllHahx(1788) = 2*MHH2**2 - MHH2*(3*Mh02 - 4*MHp2)

        AllHahx(1789) =
     &   9*MHH2*S2A*S2B + (3*Mh02 + MHH2 - 4*MHp2)*S2B**2

        AllHahx(1792) =
     &   -2*(CBA2 + 9*CBA2**3)*m12sq + (CBA2 + 9*CBA2**2)*Mh02*S2B

        AllHahx(1813) =
     &   4*m12sq**2 - 4*m12sq*(Mh02 + 2*MHH2)*S2A +
     &    S2A**2*AllHahx(1812)

	AllHahx(1821) = 7*Mh02**2 - 2*Mh02*MHH2 + 4*MHH2**2

        AllHahx(183) =
     &   CBA*AllHahx(182) +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (S2B*SAB*(CB2*Lambda6 - Lambda7*SB2)*SBA*(1 + 3*SBA2))

	AllHahx(1861) = MHH2**4 - MHH2**2*MHp2**2

        AllHahx(1870) =
     &   3*MHH2**2*SBA2*Re(AllHahl(4)) +
     &    2*(Mh02 - MHH2)**2*Re(AllHahl(6))

        AllHahx(189) =
     &   Lambda7*S2B**2*SB2*(61 - 228*SBA2 - 45*SBA2**2) -
     &    Lambda6*AllHahx(188)

        AllHahx(198) =
     &   -(SAB*AllHahx(193)) -
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (SBA*(CB2*AllHahx(195) + SB2*AllHahx(197)))

        AllHahx(204) =
     &   Lambda7*S2B**2*SB2*(29 + 12*SBA2 + 99*SBA2**2) +
     &    Lambda6*AllHahx(203)

        AllHahx(206) =
     &   35*MHH2**2*S2B**2 + m12sq**2*(29 + 12*SBA2 + 99*SBA2**2)

        AllHahx(214) =
     &   CBA*AllHahx(213) +
     &    (70*MW2*SW2)/(Alfa*Pi)*
     &     (S2B*SAB*(CB2*Lambda6 - Lambda7*SB2)*SBA*(1 + 3*SBA2))

	AllHahx(250) = S2A + S2B - 10*CBA*SAB + 2*S2B*SBA2

	AllHahx(251) = S2A + 2*CAB*SBA - S2B*(2 + SBA2)

	AllHahx(257) = S2A + S2B - 2*CBA*SAB + 2*S2B*SBA2

        AllHahx(261) =
     &   2*Mh02**2 - Mh02*(5*MHH2 - 2*MHp2) + (MHH2 - 2*MHp2)*MHp2

	AllHahx(263) = C2B*CBA*Mh02 + CAB*(Mh02 - MHp2)

	AllHahx(267) = 23*S2B - 6*CAB*SBA - 3*(S2A + S2B)*SBA2

        AllHahx(271) =
     &   MHH2*(3*S2A*S2B + 5*S2B**2)*SBA2 -
     &    MHp2*S2B*AllHahx(269) - 2*SBA*AllHahx(270)

	AllHahx(283) = Mh02**2 - 4*Mh02*MHH2 + 3*MHH2**2

	AllHahx(285) = Mh02*MHH2*(8*S2A + S2B) - 2*AllHahx(284)

	AllHahx(287) = Mh02 - 2*MHH2 - 6*(Mh02 - 3*MHH2)*SBA2

        AllHahx(289) =
     &   MHH2**2*(2*S2A - S2B) + 3*Mh02*MHH2*(S2A + S2B) -
     &    Mh02**2*(2*S2A + S2B)

        AllHahx(290) =
     &   m12sq*(3*S2A - S2B) +
     &    (3*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

	AllHahx(295) = C2B*CBA*Mh02 + CAB*(Mh02 - MHH2)

	AllHahx(31) = (1 - 3*CBA2)*m12sq*MHp2 - Mh02*(Mh02 - MHp2)*S2B

	AllHahx(311) = S2B + 2*CBA*SAB - 4*CAB*SBA - (S2A + S2B)*SBA2

	AllHahx(312) = CBA*SAB - (3*S2A + S2B)*SBA2

        AllHahx(317) =
     &   2*CAB*m12sq*MHH2 +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(Mh02 - MHH2)*(CB2*Lambda6 - Lambda7*SB2))

        AllHahx(322) =
     &   4*CB2**3*Lambda6*(Mh02 - MHH2)**3 +
     &    CB2*Lambda7*S2B*AllHahx(321)

	AllHahx(324) = Lambda7*S2B**2*SB2 - Lambda6*AllHahx(323)

	AllHahx(327) = Mh02*(3*S2A - S2B) - 6*CAB*MHH2*SBA

	AllHahx(330) = Mh02**2 - 2*Mh02*MHH2 - MHH2**2

	AllHahx(355) = 2*MHH2**2 - 3*MHH2*MHp2 + 6*MHp2**2

	AllHahx(358) = S2B*(11 - 5*SBA2) - S2A*SBA2

	AllHahx(363) = 2*CAB*SBA - 5*S2B*(1 - SBA2)

	AllHahx(364) = 4*CAB*SBA - S2B*(7 - 2*SBA2) - S2A*SBA2

	AllHahx(367) = 6*CAB*SBA - S2A*SBA2 - 3*S2B*(5 + 3*SBA2)

        AllHahx(368) =
     &   -2*C2B*m12sq + (7*MW2*SW2)/(Alfa*Pi)*
     &     (CB2*Lambda6 - Lambda7*SB2)

        AllHahx(370) =
     &   -2*C2B*m12sq + (5*MW2*SW2)/(Alfa*Pi)*
     &     (CB2*Lambda6 - Lambda7*SB2)

	AllHahx(374) = S2B + 8*CAB*SBA + (S2A + 15*S2B)*SBA2

	AllHahx(376) = S2B + 20*CAB*SBA + (S2A + 27*S2B)*SBA2

        AllHahx(377) =
     &   -2*C2B*m12sq + (3*MW2*SW2)/(Alfa*Pi)*
     &     (CB2*Lambda6 - Lambda7*SB2)

	AllHahx(379) = S2B - 6*CAB*SBA - (S2A - 4*S2B)*SBA2

        AllHahx(380) =
     &   C2B*m12sq - (2*MW2*SW2)/(Alfa*Pi)*
     &     (CB2*Lambda6 - Lambda7*SB2)

	AllHahx(383) = 2*S2B - CAB*SBA + S2A*SBA2

	AllHahx(384) = 10*CAB*SBA - S2A*SBA2 - S2B*(13 + 22*SBA2)

        AllHahx(387) =
     &   2*MHp2*S2B**2*(1 + 6*SBA2) -
     &    SBA*(12*CAB*m12sq - CBA*AllHahx(368))

        AllHahx(389) =
     &   4*SBA*(3*CAB*m12sq + CBA*AllHahx(380)) +
     &    MHp2*S2B*AllHahx(388)

        AllHahx(395) =
     &   S2B*AllHahx(394) +
     &    (12*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

	AllHahx(397) = 3*MHH2**2*S2B**2 + MHp2*AllHahx(396)

        AllHahx(400) =
     &   MHH2**2*(4*S2A - S2B)*S2B - 8*MHH2*AllHahx(398) -
     &    MHp2*AllHahx(399)

        AllHahx(404) =
     &   -7*MHH2**3*S2B**2 + 3*MHp2**2*AllHahx(401) +
     &    MHp2*AllHahx(403)

	AllHahx(406) = 20*CBA*SAB + 8*S2A*SBA2 - S2B*(11 + 3*SBA2)

	AllHahx(407) = Mh02*(S2A - S2B)*S2B - 2*m12sq*(3*S2A + S2B)

	AllHahx(418) = 8*S2B + 20*CAB*SBA + (14*S2A - 13*S2B)*SBA2

        AllHahx(419) =
     &   4*CBA*SAB - 10*CAB*SBA - 3*S2A*SBA2 + S2B*(4 + 3*SBA2)

        AllHahx(421) =
     &   m12sq*(3*CAB*SBA - 2*S2B*SBA2) + CBA*AllHahx(420)

        AllHahx(423) =
     &   4*CBA*SAB - 2*CAB*SBA + 3*S2A*SBA2 - S2B*(7 + 20*SBA2)

	AllHahx(424) = 13*S2B - 2*CAB*SBA - 4*(S2A - 7*S2B)*SBA2

        AllHahx(426) =
     &   -12*m12sq*(CAB*SBA - S2B*SBA2) + CBA*AllHahx(425)

        AllHahx(427) =
     &   8*CBA*SAB - 10*CAB*SBA - 7*S2A*SBA2 - S2B*(13 + 49*SBA2)

        AllHahx(429) =
     &   -6*m12sq*(CAB*SBA - 2*S2B*SBA2) + CBA*AllHahx(428)

        AllHahx(431) =
     &   -4*(3*CAB*m12sq*SBA + CBA*AllHahx(420)) +
     &    MHp2*S2B*AllHahx(430)

        AllHahx(435) =
     &   MHH2**4*AllHahx(433) +
     &    MHH2**3*(16*m12sq*SBA2 - MHp2*AllHahx(434))

        AllHahx(437) =
     &   -2*(3*CAB*m12sq*SBA + CBA*AllHahx(420)) +
     &    MHp2*S2B*AllHahx(436)

        AllHahx(441) =
     &   -(MHH2**2*MHp2*(2*AllHahx(426) -
     &         MHp2*S2B*AllHahx(438))) + MHp2**3*AllHahx(440)

	AllHahx(443) = S2B*(4 - 9*SBA2) - 6*S2A*SBA2

	AllHahx(444) = 8*CBA*SAB - 14*S2A*SBA2 - S2B*(14 + 71*SBA2)

	AllHahx(446) = MHp2*S2B**2*(2 + 11*SBA2) + 2*AllHahx(439)

	AllHahx(447) = 4*CBA*SAB - 4*S2A*SBA2 - S2B*(11 + 48*SBA2)

        AllHahx(464) =
     &   (Mh02 - MHp2)/(MHH2 - MHp2) + (MHH2 - MHp2)/(Mh02 - MHp2)

	AllHahx(467) = CBA*(Mh02 - MHH2)*S2B + 2*MHH2*SAB

	AllHahx(471) = 5*MHH2**2 + 6*MHH2*MHp2 + 3*MHp2**2

        AllHahx(472) =
     &   -3*MHH2**2*MHp2 + MHp2**3 + 6*(MHH2**3 + MHH2*MHp2**2)

	AllHahx(473) = -3*MHH2*MHp2**2 + 2*(MHH2**3 + MHp2**3)

        AllHahx(475) =
     &   -2*MHH2*S2B*SAB*SBA2 +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*(1 + SBA2))

        AllHahx(477) =
     &   2*C2B*m12sq*AllHahx(476) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*
     &       (MHH2 - 2*MHp2 + MHH2*SBA2))

	AllHahx(480) = MHp2*S2A - MHH2*(S2A - S2B)

	AllHahx(482) = 4*m12sq + (7*MHH2 + 4*MHp2)*S2B

        AllHahx(484) =
     &   2*(MHH2 - MHp2)*(MHH2 + MHp2)*S2A*S2B +
     &    S2B**2*AllHahx(483)

	AllHahx(486) = 10*CAB*SBA + S2B*(7 + 5*SBA2)

        AllHahx(488) =
     &   -(MHp2*(S2A*S2B - 5*S2B**2)*SBA2) -
     &    SBA*(2*AllHahx(487) -
     &       (10*MW2*SW2)/(Alfa*Pi)*
     &        (CBA*(CB2*Lambda6 - Lambda7*SB2)))

	AllHahx(489) = 7*S2B - 8*CAB*SBA - 2*(2*S2A - 3*S2B)*SBA2

        AllHahx(491) =
     &   4*SBA*(4*CAB*m12sq - CBA*AllHahx(377)) +
     &    MHp2*S2B*AllHahx(490)

        AllHahx(494) =
     &   -(SBA*(12*CAB*m12sq - CBA*AllHahx(368))) +
     &    MHp2*S2B*AllHahx(493)

        AllHahx(496) =
     &   2*SBA*(4*CAB*m12sq - CBA*AllHahx(377)) +
     &    MHp2*S2B*AllHahx(495)

        AllHahx(498) =
     &   8*SBA*(3*CAB*m12sq + CBA*AllHahx(380)) +
     &    MHp2*S2B*AllHahx(497)

        AllHahx(504) =
     &   MHH2**4*(S2B + (S2A + 3*S2B)*SBA2) -
     &    2*MHH2**3*MHp2*AllHahx(503)

	AllHahx(505) = S2A*SBA2 + S2B*(23 + 48*SBA2)

        AllHahx(509) =
     &   MHH2**4*(S2B - (S2A + S2B)*SBA2) +
     &    2*MHH2**3*MHp2*AllHahx(508)

	AllHahx(510) = 6*CAB*SBA - 3*S2A*SBA2 + S2B*(2 + 5*SBA2)

	AllHahx(511) = 2*CAB*SBA - S2B*(3 + 8*SBA2)

        AllHahx(513) =
     &   -2*SBA*(8*CAB*m12sq - CBA*AllHahx(370)) +
     &    MHp2*S2B*AllHahx(512)

        AllHahx(525) =
     &   -(CAB*(2*m12sq - Mh02*S2B)) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*(1 - 3*AllHahx(524)))

        AllHahx(530) =
     &   CBA2*(Mh02 - MHp2) + MHp2 + MHp2*SBA2 - MHH2*(1 + SBA2)

        AllHahx(534) =
     &   2*SB2*SBA*AllHahx(533) +
     &    (MW2*SW2)/(Alfa*Pi)*(Lambda6*S2B*AllHahx(530))

	AllHahx(538) = -2*m12sq*(3*S2A - S2B) + S2B*AllHahx(537)

        AllHahx(541) =
     &   -2*m12sq*(2*CAB*SBA + 3*S2A*SBA2) + S2B*AllHahx(540)

	AllHahx(542) = 3*CBA2**2*Mh02 + MHH2*(1 - 3*SBA2)*SBA2

        AllHahx(544) =
     &   (Mh02 - MHH2)*MHH2*(Mh02 + MHH2 - 2*MHp2)*S2A*S2B +
     &    m12sq*(MHH2 - MHp2)**2*(3*S2A + S2B)

        AllHahx(550) =
     &   Mh02*MHH2*S2B**2*AllHahx(545) + SBA2*AllHahx(549)

        AllHahx(555) =
     &   Mh02*MHH2*(Mh02 - MHp2)*(MHH2 - MHp2)*S2B**3*SAB -
     &    CB2*AllHahx(554) -
     &    (MW2*SW2)/(Alfa*Pi)*(Lambda7*S2B**2*SB2*SBA*AllHahx(552))

        AllHahx(560) =
     &   MHH2*AllHahx(558) - MHp2*(S2A*SBA2 + 2*AllHahx(559))

        AllHahx(592) =
     &   -3*Mh02**4 + MHH2**4 + 2*MHH2**3*MHp2 - 4*MHH2*MHp2**3 -
     &    2*MHp2**4 + 6*Mh02**2*AllHahx(590) -
     &    6*(Mh02**3*(MHH2 - 3*MHp2) + Mh02*AllHahx(591))

	AllHahx(620) = 2*S2A - 6*CBA*SAB - 2*CAB*SBA + S2B*(3 + SBA2)

        AllHahx(621) =
     &   MHp2*S2B*(1 + SBA2) + MHH2*(S2A - 2*CBA*SAB - S2B*SBA2)

	AllHahx(622) = 4*S2A + 6*CBA*SAB - 2*CAB*SBA + S2B*(3 - SBA2)

        AllHahx(623) =
     &   MHp2*S2B**2*SBA2*(1 + SBA2) -
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA*(3 + SBA2))

        AllHahx(624) =
     &   8*C2B*CBA*m12sq*SBA*(3 + SBA2) +
     &    MHH2*S2B*SBA2*AllHahx(622) + 2*AllHahx(623)

        AllHahx(673) =
     &   4*Mh02**5*S2B**5*AllHahx(627) +
     &    Mh02**4*(MHH2*S2B**5*AllHahx(628) + 16*AllHahx(638)) +
     &    2*Mh02**2*MHH2**2*
     &     (MHH2*S2B**5*AllHahx(639) - 2*AllHahx(647)) -
     &    Mh02**3*MHH2*(MHH2*S2B**5*AllHahx(648) -
     &       4*AllHahx(656)) -
     &    Mh02*MHH2**3*(MHH2*S2B**5*AllHahx(657) -
     &       16*AllHahx(665)) -
     &    4*MHH2**4*(MHH2*S2B**5*AllHahx(666) + 2*AllHahx(672))

	AllHahx(676) = 2*Mh02*S2A + MHp2*S2B + MHH2*(S2A + S2B)

        AllHahx(683) =
     &   8*m12sq*AllHahx(681) + MHH2*S2B*AllHahx(682) +
     &    (64*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHahx(686) =
     &   MHH2*S2B*AllHahx(684) + 8*m12sq*AllHahx(685) +
     &    (16*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA*(13 - 12*SBA2))

        AllHahx(697) =
     &   Mh02**4*(MHH2*S2B*AllHahx(688) - 16*AllHahx(690)) -
     &    Mh02*AllHahx(696)

        AllHahx(702) =
     &   2*S2A - 2*(3*CBA*SAB + CAB*SBA) + S2B*(3 + SBA2)

        AllHahx(704) =
     &   SBA2*AllHahx(703) -
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA*(3 + SBA2))

	AllHahx(706) = 6*CAB*SBA + (S2A - 2*S2B)*SBA2

        AllHahx(711) =
     &   2*MHH2*S2B**5*AllHahx(707) + 8*m12sq*AllHahx(708) +
     &    (MW2*SW2)/(Alfa*Pi)*(CBA*SBA*AllHahx(710))

        AllHahx(712) =
     &   226*S2B + 1056*CAB*SBA + (434*S2A - 915*S2B)*SBA2

        AllHahx(719) =
     &   -2*m12sq*AllHahx(714) +
     &    (MW2*SW2)/(Alfa*Pi)*(CBA*SBA*AllHahx(718))

	AllHahx(720) = 2*S2B - 136*CAB*SBA - (46*S2A - 1325*S2B)*SBA2

        AllHahx(727) =
     &   -4*m12sq*AllHahx(722) +
     &    (MW2*SW2)/(Alfa*Pi)*(CBA*SBA*AllHahx(726))

	AllHahx(728) = 62*S2A*SBA2 + S2B*(55 + 186*SBA2)

        AllHahx(735) =
     &   -2*m12sq*AllHahx(730) +
     &    (MW2*SW2)/(Alfa*Pi)*(CBA*SBA*AllHahx(734))

        AllHahx(736) =
     &   780*CAB*SBA + 9*S2B*(30 - 179*SBA2) + 410*S2A*SBA2

        AllHahx(743) =
     &   -2*m12sq*AllHahx(738) +
     &    (MW2*SW2)/(Alfa*Pi)*(CBA*SBA*AllHahx(742))

        AllHahx(745) =
     &   8*SBA*(4*CAB*m12sq - CBA*AllHahx(377)) +
     &    MHH2*S2B*AllHahx(744)

        AllHahx(749) =
     &   MHH2**2*(4*S2A + S2B) - Mh02**2*(8*S2A + 7*S2B) +
     &    2*Mh02*AllHahx(748)

	AllHahx(750) = 3*S2A - S2B + 6*CAB*SBA

        AllHahx(753) =
     &   SBA2*AllHahx(752) -
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA*(3 + SBA2))

        AllHahx(763) =
     &   Mh02**5*S2B**5*AllHahx(755) - MHH2**4*AllHahx(762)

        AllHahx(764) =
     &   138*S2B + 400*CBA*SAB + 3288*CAB*SBA +
     &    (2012*S2A + 137*S2B)*SBA2

	AllHahx(772) = -4*m12sq*S2B**4*AllHahx(765) + CBA*AllHahx(771)

	AllHahx(774) = S2B*(674 - 181*SBA2) - 8*AllHahx(773)

	AllHahx(783) = -8*m12sq*S2B**4*AllHahx(775) + CBA*AllHahx(782)

        AllHahx(803) =
     &   Mh02*MHH2**3*(MHH2*S2B**5*AllHahx(785) -
     &       2*AllHahx(793)) +
     &    Mh02**2*MHH2**2*(MHH2*S2B**5*AllHahx(795) +
     &       12*AllHahx(802))

	AllHahx(852) = 2*Mh02**2 - Mh02*(5*MHH2 - 4*MHp2) - MHH2*MHp2

        AllHahx(865) =
     &   MHH2**2 + MHp2**2*(1 - 5*SBA2) - MHH2*MHp2*(5 + SBA2)

	AllHahx(868) = 3*SBA2 - 2*(1 - 2*SW2)**2

        AllHahx(869) =
     &   2*MHH2*MHp2*AllHahx(866) - MHH2**2*AllHahx(867) -
     &    2*MHp2**2*AllHahx(868)

	AllHahx(873) = MHH2 - 2*MHp2 + (MHH2 - 6*MHp2)*SBA2

        AllHahx(874) =
     &   MHH2 + 3*(MHH2 - 2*MHp2)*SBA2**2 - 2*(MHp2 + MHH2*SBA2)

	AllHahx(892) = 32 + 16*CBA2 - 12*CBA2**2 + 81*CBA2**3

	AllHahx(893) = -32 - 16*CBA2 - 60*CBA2**2 + 297*CBA2**3

	AllHahx(894) = 16 + 8*CBA2 + 12*CBA2**2 + 27*CBA2**3

        AllHahx(905) =
     &   -3*MHp2*(3*S2A - 2*S2B)*S2B*SBA +
     &    (8*MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

        AllHahx(906) =
     &   MHp2*S2B**2*SBA +
     &    (2*MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

        AllHahx(909) =
     &   MHp2*(33*S2A - 37*S2B)*S2B*SBA -
     &    (23*MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

        AllHahx(910) =
     &   MHp2*S2B*(267*S2A + 26*S2B)*SBA -
     &    (20*MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

        AllHahx(912) =
     &   3*MHp2*S2A*S2B*SBA +
     &    (4*MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

        AllHahx(964) =
     &   SW2*(6*MHH2**4 - 11*MHH2**3*MHp2 - 4*MHH2**2*MHp2**2 +
     &      66*MHH2*MHp2**3 - 8*MHp2**4)

        AllHahx(966) =
     &   6*MHH2**4 - 11*MHH2**3*MHp2 - 4*MHH2**2*MHp2**2 +
     &    66*MHH2*MHp2**3 - 8*MHp2**4

        AllHahx(972) =
     &   6*MHp2**5*SBA2 + MHH2*MHp2**4*(4 - 9*SBA2 + 3*SBA2**2)

        AllHahx(1012) =
     &   SBA*AllHahx(1011) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*(1 + 3*SBA2**2))

        AllHahx(1018) =
     &   2*CAB*(MHH2 - 4*MHp2)*(2*m12sq - Mh02*S2B) +
     &    S2B**2*SBA*AllHahx(1017)

        AllHahx(1020) =
     &   -2*(Mh02 - 4*MHp2)*(2*m12sq - MHH2*S2B)*SAB*SBA +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*AllHahx(1019))

        AllHahx(1024) =
     &   -2*CAB*(2*m12sq - Mh02*S2B) - SBA*AllHahx(1023) +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*(1 + 3*SBA2**2))

        AllHahx(1043) =
     &   (CBA2*(Mh02 - 2*MHH2))/(Mh02 - MHp2) -
     &    (2*MHH2)/(MHH2 - MHp2)

        AllHahx(1052) =
     &   CBA2*(Mh02 - MHH2)*(MHH2 - MHp2) + MHH2*(Mh02 - MHp2)*SBA2

        AllHahx(1063) =
     &   CBA*AllHahx(538) +
     &    2*((2*m12sq - MHH2*S2B)*SAB +
     &       (MW2*SW2)/(Alfa*Pi)*
     &        ((1 - 3*CBA2)*(CB2*Lambda6 - Lambda7*SB2)*SBA))

	AllHahx(1084) = 2*MHp2*S2B - Mh02*(3*S2A + S2B)

        AllHahx(1089) =
     &   4*(Mh02 - MHp2)**2*(2*m12sq - Mh02*S2B) -
     &    CBA2*(2*m12sq*AllHahx(1087) - Mh02*S2B*AllHahx(1088))

        AllHahx(1090) =
     &   (Mh02 - 2*MHp2)*S2B**2*SBA -
     &    (2*MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

        AllHahx(1093) =
     &   (Mh02 - 2*MHp2)*S2B*SBA*AllHahx(1091) -
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*AllHahx(1092))

        AllHahx(1098) =
     &   2*(Mh02 - MHp2)**2*(2*m12sq - Mh02*S2B) +
     &    CBA2*MHp2*AllHahx(1097)

        AllHahx(1101) =
     &   CBA2*AllHahx(1100) +
     &    (6*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*CBA2**2*(Mh02 - 4*MHp2)*
     &       (CB2*Lambda6 - Lambda7*SB2))

	AllHahx(1103) = 3*Mh02**2 - 14*Mh02*MHp2 + 8*MHp2**2

        AllHahx(1111) =
     &   2*((Mh02 - MHp2)**2*AllHahx(1090) + CAB*AllHahx(1106)) +
     &    CBA2*MHp2*AllHahx(1110)

	AllHahx(1136) = Mh02**2 - 8*Mh02*MHp2 + 12*MHp2**2

        AllHahx(115) =
     &   Mh02**3*AllHahx(111) - Mh02**2*AllHahx(112) +
     &    MHp2*(MHH2*AllHahx(110) + Mh02*AllHahx(114))

	AllHahx(1161) = S2B - (3*S2A - S2B)*SBA2

        AllHahx(1162) =
     &   MHH2*S2B*(1 + 2*SBA2) - 2*m12sq*(1 - (1 - 3*SBA2)*SBA2)

        AllHahx(1163) =
     &   10*MHp2**3*SBA2 + MHH2**2*MHp2*(9 - SBA2 - 40*CW2*SW2) -
     &    2*(1 - 2*SW2)**2*MHH2**3 -
     &    4*(1 - 8*CW2*SW2)*(MHH2*MHp2**2)

        AllHahx(1176) =
     &   2*(MHH2**2 + MHp2**2)*S2B*SBA2 + MHH2*MHp2*AllHahx(1175)

        AllHahx(1179) =
     &   SAB*(MHH2*S2B*AllHahx(1177) - 2*m12sq*AllHahx(1178)) -
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*AllHahx(1178))

        AllHahx(1214) =
     &   -(MHH2*MHp2*(6 + SBA2 + 8*CW2*SW2)) +
     &    2*(MHH2**2 + MHp2**2*AllHahx(868))

        AllHahx(1221) =
     &   3*MHH2*(MHH2 - 4*MHp2)**2*MHp2**2*SBA2 -
     &    4*(MHH2 - MHp2)*(2*MHp2*AllHahx(1218) -
     &       MHH2*AllHahx(1220))

	AllHahx(1241) = 6*Mh02**2 + 17*Mh02*MHH2 - 5*MHH2**2

	AllHahx(1243) = 6*Mh02**2 - 7*Mh02*MHH2 + MHH2**2

	AllHahx(1245) = 3*S2A - S2B - 12*CAB*SBA

        AllHahx(1246) =
     &   -2*MHH2*S2A*SB2 +
     &    (9*MW2*SW2)/(Alfa*Pi)*(CBA*Lambda6*S2B*SBA)

	AllHahx(1248) = 4*m12sq*(3*S2A - S2B)*SB2 + S2B*AllHahx(1247)

	AllHahx(1253) = -2*m12sq*(3*S2A + S2B) + S2B*AllHahx(1252)

        AllHahx(1254) =
     &   -(CAB*(2*m12sq - Mh02*S2B)) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*(1 - 3*SBA2))

	AllHahx(1275) = 2*MHH2*(3*S2A - S2B) + Mh02*(3*S2A + S2B)

        AllHahx(1289) =
     &   -6*CAB*MHH2*(2*CBA2*m12sq - Mh02*S2B) -
     &    SBA*AllHahx(1288) +
     &    (6*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*(MHH2 - Mh02*SBA2))

	AllHahx(129) = S2B*(22 - 29*SBA2) - 28*S2A*SBA2

        AllHahx(130) =
     &   8*CBA*SAB + S2B*(150 - 89*SBA2) - 4*S2A*(1 + 41*SBA2)

	AllHahx(1308) = CBA2*Mh02 - 2*(Mh02 - MHH2)*SBA2

        AllHahx(131) =
     &   16*CBA*SAB - 8*S2A*(1 + 6*SBA2) + S2B*(99 + 41*SBA2)

        AllHahx(132) =
     &   20*CBA*SAB - 2*S2A*(5 + 58*SBA2) + S2B*(151 + 61*SBA2)

	AllHahx(134) = 2*CAB*SBA - (S2A + S2B)*SBA2

        AllHahx(135) =
     &   MHp2*(S2A + 14*CAB*SBA - 7*S2B*SBA2) + MHH2*AllHahx(134)

        AllHahx(137) =
     &   36*CBA*SAB*SBA2 - S2B*(40 - 65*SBA2 + 3*SBA2**2)

	AllHahx(14) = 8*CBA*SAB - 4*S2A*(1 - SBA2) + 9*S2B*SBA2

        AllHahx(140) =
     &   MHp2*S2B**2*(20 + 9*SBA2 - 7*SBA2**2) - 4*CBA*AllHahx(139)

        AllHahx(141) =
     &   2*CBA*SAB*SBA2 + S2B*(22 + 21*SBA2 + 11*SBA2**2)

        AllHahx(143) =
     &   -2*m12sq*AllHahx(142) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*
     &       (1 + 2*SBA2 + 6*SBA2**2))

	AllHahx(146) = 2*S2A - 4*CBA*SAB + S2B*(19 + 3*SBA2)

        AllHahx(147) =
     &   24*CBA*SAB - S2B*(16 - 17*SBA2) - 2*S2A*(6 + 5*SBA2)

	AllHahx(150) = S2A + 12*CAB*SBA + (S2A - 6*S2B)*SBA2

        AllHahx(155) =
     &   -2*m12sq*(5*C2B*SBA + SAB*SBA2) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*(5 - SBA2))

	AllHahx(157) = 4*CBA*SAB*SBA2 - S2B*(19 + 15*SBA2)

	AllHahx(158) = 16*CBA*SAB*SBA2 - S2B*(21 + 16*SBA2 - SBA2**2)

	AllHahx(159) = C2B*SBA*(5 - SBA2) + 2*SAB*SBA2

        AllHahx(1620) =
     &   S2A*(2*CBA2**2 + 3*CBA2*SBA2) + SBA2*(S2B + S2A*SBA2)

	AllHahx(1628) = 9*(Mh02 + MHH2)*S2A + 5*(Mh02 - MHH2)*S2B

	AllHahx(1630) = 9*S2A**2 - S2B**2

        AllHahx(1631) =
     &   2*(Mh02 - MHH2)**2*S2A**2 + MHH2*(Mh02 + 2*MHH2)*S2B**2

        AllHahx(1646) =
     &   CBA*(Mh02 - MHH2)*S2B**2 - 2*(2*m12sq - MHH2*S2B)*SAB

        AllHahx(1657) =
     &   m12sq*(MHH2 + 2*MHp2) - (2*MHH2**2 + MHH2*MHp2)*S2B

        AllHahx(1660) =
     &   3*CBA2**2*Mh02 + CBA2*(MHH2 + 2*MHp2) + SBA2*AllHahx(1659)

        AllHahx(1665) =
     &   -4*MHH2*S2B*SAB + 2*m12sq*(SAB + 3*SAB*SBA2) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*(1 + 3*SBA2))

        AllHahx(1666) =
     &   Mh02*S2B*(S2A + S2B) - 2*m12sq*(3*S2A + S2B) -
     &    MHH2*(S2A*S2B + S2B**2)

        AllHahx(1668) =
     &   -(CAB*(2*m12sq - Mh02*S2B)) +
     &    (MW2*SW2)/(Alfa*Pi)*(CBA*AllHahx(1667))

        AllHahx(1675) =
     &   -2*m12sq*(3*CBA2*Mh02 + MHH2 + 2*MHp2) +
     &    Mh02*(3*Mh02 + MHH2 + 2*MHp2)*S2B

        AllHahx(1677) =
     &   2*MHH2**2*S2B - m12sq*(MHH2 + 2*MHp2 + 3*MHH2*SBA2)

        AllHahx(1678) =
     &   -2*MHp2*S2B*SAB +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*(1 + 3*SBA2))

        AllHahx(1680) =
     &   2*CB2*Lambda6*MHp2 - Lambda7*(MHH2 + 2*MHp2)*SB2 +
     &    3*MHH2*(CB2*Lambda6 - Lambda7*SB2)*SBA2

        AllHahx(1683) =
     &   (Mh02**2 + MHH2**2)*S2A*S2B +
     &    (MHH2**2 + 2*MHH2*MHp2)*S2B**2 + Mh02*AllHahx(1682)

	AllHahx(1687) = -2*m12sq*(3*S2A + S2B) + S2B*AllHahx(1686)

        AllHahx(17) = -6*CBA*SAB - 14*CAB*SBA +
     &    2*S2A*(1 + SBA2) + S2B*(3 + 7*SBA2)

	AllHahx(1705) = 2*m12sq*SAB - MHH2*S2B*(CBA*S2B + SAB)

	AllHahx(1706) = 4*m12sq*SAB - MHH2*S2B*(CBA*S2B + 2*SAB)

	AllHahx(1708) = MHH2*S2B*(CBA*S2A - SAB) + 2*m12sq*SAB*SBA2

	AllHahx(1709) = MHH2*S2B*(CBA*S2A - 2*SAB) + 4*m12sq*SAB*SBA2

        AllHahx(1714) =
     &   2*(MHH2 - MHp2)**2 + Mh02**2*SBA2 +
     &    MHH2**2*(CBA2 + 4*SBA2) + CBA2*AllHahx(1713)

        AllHahx(1715) =
     &   MHH2*S2B*SAB - (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHahx(1717) =
     &   MHH2*S2B*AllHahx(1716) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*AllHahx(467))

        AllHahx(172) =
     &   MHH2*S2B**3*SAB*(1 + 3*SBA2) -
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (SB2*SBA*(7 - 3*(4 - 3*SBA2)*SBA2)*AllHahx(171))

        AllHahx(1722) =
     &   2*(MHH2 - MHp2)**2*Re(AllHahl(5)) +
     &    CBA2*(Mh02 - MHH2)**2*Re(AllHahl(6))

        AllHahx(1730) =
     &   CAB*(2*m12sq - Mh02*S2B) + (Mh02 - 2*MHH2)*S2B**2*SBA

	AllHahx(1732) = 3*Mh02**2*SBA2 + 2*CBA2*AllHahx(1731)

        AllHahx(1733) =
     &   2*CAB*(2*m12sq - Mh02*S2B) + (Mh02 - 2*MHH2)*S2B**2*SBA

        AllHahx(1735) =
     &   Mh02*S2A*S2B*SBA +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*CBA2*(CB2*Lambda6 - Lambda7*SB2))

        AllHahx(1736) =
     &   CAB**2*(2*CBA2*m12sq - Mh02*S2B)**2 +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (CBA*CBA2*Mh02*S2A*S2B*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHahx(1741) =
     &   CBA2*(Mh02 - MHp2)**2*Re(AllHahl(11)) +
     &    (MHH2 - MHp2)**2*(1 + SBA2)*Re(AllHahl(12))

	AllHahx(1743) = Mh02**4 - Mh02**2*MHp2**2

        AllHahx(1748) =
     &   (MHH2 - MHp2)**2 + CBA2*AllHahx(1746) + SBA2*AllHahx(1747)

	AllHahx(1764) = MHH2**2 + 4*(Mh02**2 + Mh02*MHH2)

	AllHahx(1767) = (3*CBA2 + 5*CBA2**2)*Mh02 + CBA2**2*MHH2

	AllHahx(1769) = Mh02**2 + Mh02*(3*MHH2 - 8*MHp2)

        AllHahx(1774) =
     &   -(CBA2*(Mh02 + MHH2 - 8*MHp2)*S2B**2) +
     &    3*((CBA2 - 2*CBA2**2)*MHH2*S2A*S2B +
     &       CBA2**2*AllHahx(1773))

        AllHahx(1775) =
     &   -10*CBA2*(2*m12sq - MHH2*S2B)*SAB + CBA*AllHahx(1774)

        AllHahx(1778) =
     &   CBA*Lambda7*SB2*AllHahx(1777) -
     &    CB2*Lambda6*(18*CBA2*MHH2*S2B*SAB + CBA*AllHahx(1777))

        AllHahx(1779) =
     &   CBA*(2*m12sq - Mh02*S2A)*S2B + 12*CBA2*m12sq*SAB

	AllHahx(1780) = Mh02*S2A*S2B - 2*m12sq*(3*S2A + S2B)

        AllHahx(1781) =
     &   CBA2*Lambda7*MHH2*S2B*SAB*SB2*SBA2 +
     &    CB2*CBA*Lambda6*m12sq*S2A*SBA2**2

        AllHahx(1790) =
     &   -3*(Mh02*MHH2 + 2*MHH2**2)*S2A*S2B**2 +
     &    S2B**3*AllHahx(1788) + 2*m12sq*AllHahx(1789)

        AllHahx(1793) =
     &   -(CB2*(CBA2 + 9*CBA2**2)*Lambda6*Mh02*S2B) +
     &    Lambda7*SB2*AllHahx(1792)

	AllHahx(1798) = -Mh02**2 + 2*Mh02*MHp2 - MHp2**2

	AllHahx(18) = S2A - 2*CBA*SAB + (S2A + 4*S2B)*SBA2

	AllHahx(1803) = 2*CBA2**3*m12sq - CBA2**2*Mh02*S2B

        AllHahx(1806) =
     &   4*(m12sq**2 + m12sq*(2*Mh02 + MHH2)*S2A) +
     &    S2A**2*AllHahx(1764)

	AllHahx(1810) = 4*m12sq**2 - 4*m12sq*Mh02*S2B + Mh02**2*S2B**2

        AllHahx(1811) =
     &   2*m12sq*(Mh02 - 2*MHH2)*S2B**2 -
     &    (Mh02**2 - 2*Mh02*MHH2)*S2B**3

	AllHahx(1814) = Mh02**2 - 4*Mh02*MHH2 + 4*MHH2**2

        AllHahx(1815) =
     &   -12*m12sq*(Mh02 + 2*MHH2)*S2A**2*S2B +
     &    12*m12sq**2*(3*S2A**2 + 2*S2A*S2B) +
     &    S2B**2*AllHahx(1813) + S2B**4*AllHahx(1814)

        AllHahx(1822) =
     &   10*m12sq**2 + 2*m12sq*(4*Mh02 - 7*MHH2)*S2A +
     &    S2A**2*AllHahx(1821)

	AllHahx(1823) = -Mh02**2 - Mh02*(MHH2 - 2*MHp2) - 4*MHp2**2

	AllHahx(1825) = 2*m12sq*MHH2*S2B**2 - MHH2**2*S2B**3

        AllHahx(1827) =
     &   2*m12sq*(Mh02 - 2*MHp2)*S2B**2 -
     &    (Mh02**2 - 2*Mh02*MHp2)*S2B**3

	AllHahx(1828) = Mh02**2 - 4*Mh02*MHp2 + 4*MHp2**2

	AllHahx(1832) = CBA*MHH2*S2B**2 - 2*(2*m12sq - MHH2*S2B)*SAB

	AllHahx(1836) = -Mh02**2 + 2*Mh02*MHH2 - MHH2**2

        AllHahx(1842) =
     &   CBA2*(Mh02 - MHH2)**2 + 2*(MHH2 - MHp2)**2 +
     &    Mh02**2*SBA2 + MHH2**2*(CBA2 + 4*SBA2)

        AllHahx(1845) =
     &   2*CBA2*AllHahx(1731) +
     &    3*Mh02**2*SBA2*(1 + 2*Re(AllHahl(15)))

	AllHahx(1856) = 3*CBA2*MHH2**2 + 4*(MHH2 - MHp2)**2*SBA2

        AllHahx(1860) =
     &   MHH2**2 - MHp2**2 + (MHH2 - MHp2)*(MHH2 + MHp2)*SBA2**2

        AllHahx(1862) =
     &   3*SBA2*AllHahx(1861)*Re(AllHahl(4)) +
     &    2*(Mh02 - MHH2)**2*(MHH2 - MHp2)*(MHH2 + MHp2)*
     &     Re(AllHahl(6))

        AllHahx(1864) =
     &   CBA2*(Mh02 - MHp2)**2*(1 + SBA2)*Re(AllHahl(11)) +
     &    (MHH2 - MHp2)**2*(1 + SBA2)**2*Re(AllHahl(12))

        AllHahx(1871) =
     &   CBA2*AllHahx(1870) +
     &    4*(MHH2 - MHp2)**2*
     &     (Re(AllHahl(5)) + SBA2**2*Re(AllHahl(5)))

        AllHahx(190) =
     &   -53*MHH2*S2B**3*SAB*(1 + 3*SBA2) +
     &    (MW2*SW2)/(Alfa*Pi)*(SBA*AllHahx(189))

        AllHahx(199) =
     &   CBA*AllHahx(198) +
     &    (106*MW2*SW2)/(Alfa*Pi)*
     &     (S2B*SAB*(CB2*Lambda6 - Lambda7*SB2)*SBA*(1 + 3*SBA2))

        AllHahx(205) =
     &   35*MHH2*S2B**3*SAB*(1 + 3*SBA2) +
     &    (MW2*SW2)/(Alfa*Pi)*(SBA*AllHahx(204))

        AllHahx(229) =
     &   -2*(2*CAB + C2B*CBA)*m12sq +
     &    (3*MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

	AllHahx(231) = 10*S2B - 88*CAB*SBA - (52*S2A + 23*S2B)*SBA2

        AllHahx(232) =
     &   -((CAB + C2B*CBA)*m12sq) +
     &    (MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

	AllHahx(235) = S2B**2 + S2B*(S2A - 10*CBA*SAB)

	AllHahx(242) = MHp2*(17*S2B - 12*CAB*SBA) + MHH2*S2A*SBA2

        AllHahx(243) =
     &   -2*(4*CAB + C2B*CBA)*m12sq +
     &    (5*MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

	AllHahx(246) = 2*CAB*SBA - S2A*SBA2 - S2B*(19 + 2*SBA2)

	AllHahx(247) = MHp2*(3 - 4*SBA2) + 9*MHH2*SBA2

        AllHahx(252) =
     &   S2B*SBA2*(MHH2*AllHahx(250) + MHp2*AllHahx(251)) +
     &    (8*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

	AllHahx(253) = S2A + S2B - 18*CBA*SAB + 2*S2B*SBA2

        AllHahx(254) =
     &   MHp2*S2B**2*SBA2 -
     &    (8*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHahx(258) =
     &   Mh02**2*(S2A + 2*CAB*SBA - S2B*SBA2) -
     &    Mh02*(MHH2*AllHahx(250) + MHp2*AllHahx(251)) -
     &    MHp2*(2*MHp2*S2B - MHH2*AllHahx(257))

        AllHahx(262) =
     &   Mh02*(2*Mh02 + MHH2 - 3*MHp2)*S2A - S2B*AllHahx(261)

        AllHahx(264) =
     &   -(CAB*Mh02*(5*Mh02 - 2*MHp2)*S2B) -
     &    2*m12sq*AllHahx(263) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(2*Mh02 - MHp2)*(CB2*Lambda6 - Lambda7*SB2))

	AllHahx(266) = S2B*(18 + 7*SBA2) - 4*(4*CAB*SBA + S2A*SBA2)

        AllHahx(268) =
     &   -8*SBA*AllHahx(232) +
     &    S2B*(2*MHH2*S2A*SBA2 - MHp2*AllHahx(267))

        AllHahx(275) =
     &   Mh02**2*(S2A + 2*CAB*SBA - S2B*SBA2) -
     &    MHp2*(2*MHp2*S2B - MHH2*AllHahx(257))

        AllHahx(286) =
     &   4*m12sq*(3*S2A + S2B)*AllHahx(283) +
     &    (Mh02 + 2*MHH2)*S2B*AllHahx(285)

        AllHahx(288) =
     &   Mh02*(Mh02 - 4*MHH2)*(2*m12sq - MHH2*S2B)*SAB +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((Mh02 - MHH2)*(CB2*Lambda6 - Lambda7*SB2)*SBA*
     &       AllHahx(287))

	AllHahx(29) = MHp2*S2B**2 + 4*CAB*(1 - 3*CBA2)*m12sq*SBA

        AllHahx(291) =
     &   S2B*AllHahx(289) + 2*(Mh02 - 2*MHH2)*AllHahx(290)

        AllHahx(296) =
     &   CAB*Mh02*(Mh02 - 2*MHH2)*S2B + 2*m12sq*AllHahx(295)

        AllHahx(297) =
     &   8*Mh02**2 - 6*Mh02*MHH2 + MHH2**2 +
     &    6*(Mh02 - MHH2)**2*SBA2

        AllHahx(30) = 3*Mh02**2*S2A +
     &    (Mh02 - MHp2)*(2*Mh02 + MHp2)*S2B

        AllHahx(301) =
     &   m12sq*(3*S2A - S2B) +
     &    (6*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

	AllHahx(305) = CAB*SBA + (3*S2A - S2B)*SBA2

        AllHahx(307) =
     &   S2B - 2*CBA*SAB + 4*CAB*SBA + (5*S2A + 4*S2B)*SBA2

	AllHahx(308) = CBA*SAB - CAB*SBA - 6*S2A*SBA2

	AllHahx(310) = 2*Mh02**4*S2A + MHH2**4*S2B

        AllHahx(313) =
     &   MHH2*S2B*AllHahx(311) - 4*m12sq*AllHahx(312) -
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA*(1 - 6*SBA2))

        AllHahx(32) = -2*CAB*AllHahx(31) -
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(1 - 3*CBA2)*(Mh02 - MHp2)*
     &       (CB2*Lambda6 - Lambda7*SB2))

        AllHahx(325) =
     &   4*SB2**2*AllHahx(322) -
     &    Mh02**2*(4*Mh02 - MHH2)*S2A*S2B*AllHahx(324)

        AllHahx(328) =
     &   Mh02*(2*Mh02 + MHH2)*S2A*S2B - 2*m12sq*AllHahx(327) -
     &    (6*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(Mh02 + MHH2)*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHahx(344) =
     &   11*Mh02**3 + MHH2**3 + 3*Mh02**2*(MHH2 - 4*MHp2)

	AllHahx(345) = 3*MHH2**2*MHp2 - 4*MHp2**3

	AllHahx(347) = 2*MHH2**2 + MHH2*(Mh02 - 2*MHp2)

	AllHahx(349) = 2*Mh02 + MHH2 - 2*MHp2 + MHH2*SBA2

	AllHahx(350) = 2*Mh02 + MHH2 - 2*MHp2 + (3*MHH2 - 2*MHp2)*SBA2

	AllHahx(354) = 4*m12sq + (5*MHH2 + 6*MHp2)*S2B

        AllHahx(356) =
     &   -2*(MHH2 - MHp2)*(MHH2 + MHp2)*S2A*S2B +
     &    S2B**2*AllHahx(355)

        AllHahx(359) =
     &   MHH2**4*(S2B - S2A*SBA2) -
     &    2*MHH2**3*MHp2*(3*S2B - S2A*SBA2) +
     &    MHH2**2*MHp2**2*AllHahx(358)

        AllHahx(365) =
     &   -(MHH2**5*(S2A + S2B)*SBA2) + MHH2**4*MHp2*AllHahx(363) -
     &    2*MHH2**3*MHp2**2*AllHahx(364)

        AllHahx(369) =
     &   MHp2*S2B*AllHahx(367) -
     &    2*SBA*(12*CAB*m12sq - CBA*AllHahx(368))

        AllHahx(371) =
     &   2*MHp2*S2B**2*(2 + 7*SBA2) +
     &    SBA*(8*CAB*m12sq - CBA*AllHahx(370))

	AllHahx(373) = 11*S2B*SBA2 - 2*(CAB*SBA + S2A*SBA2)

        AllHahx(375) =
     &   -(SBA*(8*CAB*m12sq - CBA*AllHahx(370))) +
     &    MHp2*S2B*AllHahx(374)

        AllHahx(378) =
     &   MHp2*S2B*AllHahx(376) +
     &    2*SBA*(4*CAB*m12sq - CBA*AllHahx(377))

        AllHahx(381) =
     &   MHp2*S2B*AllHahx(379) -
     &    8*SBA*(3*CAB*m12sq + CBA*AllHahx(380))

        AllHahx(385) =
     &   2*MHH2**4*AllHahx(383) + MHH2**3*MHp2*AllHahx(384)

	AllHahx(386) = 12*CAB*SBA + S2A*SBA2 + S2B*(13 + 72*SBA2)

        AllHahx(390) =
     &   MHp2**3*AllHahx(387) + MHH2**2*MHp2*AllHahx(389)

        AllHahx(405) =
     &   -8*m12sq*(Mh02 - MHH2)*(Mh02 - MHp2)**2*(3*S2A - S2B) +
     &    4*Mh02**4*S2B*(2*S2A + 3*S2B) - 2*Mh02**3*AllHahx(395) +
     &    MHH2*MHp2*AllHahx(397) - Mh02**2*AllHahx(400) +
     &    Mh02*AllHahx(404)

	AllHahx(416) = 16*CAB*SBA + S2B*(10 - 3*SBA2) + 6*S2A*SBA2

	AllHahx(417) = 10*S2B + 16*CAB*SBA + (6*S2A - 7*S2B)*SBA2

        AllHahx(422) =
     &   S2B*(MHp2*AllHahx(418) - 2*MHH2*AllHahx(419)) +
     &    8*AllHahx(421)

        AllHahx(432) =
     &   MHH2**3*S2B*AllHahx(423) -
     &    MHp2**2*(MHp2*S2B*AllHahx(424) + 2*AllHahx(426)) -
     &    MHH2*MHp2*(MHp2*S2B*AllHahx(427) - 2*AllHahx(429)) +
     &    MHH2**2*AllHahx(431)

        AllHahx(442) =
     &   S2B*AllHahx(435) + 4*MHH2*MHp2**2*AllHahx(437) +
     &    2*AllHahx(441)

        AllHahx(445) =
     &   MHH2**3*AllHahx(443) +
     &    MHH2**2*(16*m12sq*SBA2 - MHp2*AllHahx(444))

        AllHahx(448) =
     &   MHp2**2*AllHahx(446) -
     &    MHH2*MHp2*(2*AllHahx(429) - MHp2*S2B*AllHahx(447))

	AllHahx(454) = 2*MHp2*S2B - (Mh02 - MHH2)*(S2A + S2B)

	AllHahx(457) = 1 - CBA2*(4 - SBA2) + 6*SBA2 + SBA2**2

	AllHahx(462) = MHH2 + 2*Mh02*SBA2 - MHp2*(1 + 2*SBA2)

        AllHahx(463) =
     &   (MHH2 - MHp2)*(1 + SBA2) +
     &    (Mh02 - MHH2)*(2 + (MHp2*SBA2)/MHH2)

        AllHahx(468) =
     &   -4*C2B*m12sq + S2B*(2*CAB*CBA*Mh02 - SBA*AllHahx(467)) +
     &    (2*MW2*SW2)/(Alfa*Pi)*(CB2*Lambda6 - Lambda7*SB2)

        AllHahx(474) =
     &   Mh02**5 + MHH2**2*MHp2**3 - Mh02**4*(4*MHH2 + 3*MHp2) +
     &    Mh02**3*AllHahx(471) - Mh02**2*AllHahx(472) +
     &    Mh02*MHH2*AllHahx(473)

        AllHahx(478) =
     &   2*(MHH2**2 - MHH2*MHp2)*S2B*SAB*SBA2 +
     &    Mh02*AllHahx(475) - SBA*AllHahx(477)

	AllHahx(48) = -1 + SBA2 + 2*SBA2**2 + 3*CBA2*(2 + SBA2)

	AllHahx(481) = -6*CAB*(MHH2 - MHp2)*SBA + SBA2*AllHahx(480)

        AllHahx(485) =
     &   -(SBA2*AllHahx(484)) +
     &    (MHH2 - MHp2)*SBA*
     &     (-2*(2*C2B*CBA*m12sq + CAB*AllHahx(482)) +
     &       (6*MW2*SW2)/(Alfa*Pi)*
     &        (CBA*(CB2*Lambda6 - Lambda7*SB2)))

        AllHahx(526) =
     &   -2*MHH2*S2B*(S2B - S2A*SBA2) + SBA2*AllHahx(407) +
     &    2*(MHp2*S2B**2*(1 + SBA2) + SBA*AllHahx(525))

	AllHahx(529) = Mh02 - CBA2*MHH2 - (1 - CBA2)*MHp2

        AllHahx(543) =
     &   -(CBA2**2*Mh02*AllHahx(538)) + MHH2*SBA2*AllHahx(541) +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA*AllHahx(542))

        AllHahx(556) =
     &   CBA*AllHahx(555) +
     &    S2B**2*(Mh02*SBA2*AllHahx(544) - 1/2.D0*AllHahx(550))

	AllHahx(613) = 2*Mh02**2 - 7*Mh02*MHH2 + 14*MHH2**2

	AllHahx(615) = MHH2*(1 - SBA2) + Mh02*SBA2

        AllHahx(619) =
     &   4*Mh02**3 - 21*Mh02**2*MHH2 + 21*Mh02*MHH2**2 - 4*MHH2**3

	dup1 = Mh02**2*AllHahx(620) + 2*MHH2*AllHahx(621)

	AllHahx(625) = dup1*S2B*SBA2 - Mh02*AllHahx(624)

	AllHahx(626) = 17*Mh02*MHH2 - 4*(Mh02**2 + MHH2**2)

        AllHahx(674) =
     &   -2*CBA2**2*(Mh02 - MHH2)**2*(2*Mh02 - MHH2 - MHp2)*S2B**6*
     &     AllHahx(626) + CBA2*AllHahx(673)

        AllHahx(677) =
     &   -2*m12sq*AllHahx(327) + Mh02*S2B*AllHahx(676) -
     &    (6*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(Mh02 + MHH2)*(CB2*Lambda6 - Lambda7*SB2)*SBA)

	AllHahx(701) = Mh02**2 - MHH2**2 + Mh02*(MHH2 - MHp2)

        AllHahx(705) =
     &   -8*C2B*CBA*m12sq*(Mh02 - 2*MHH2)*SBA*(3 + SBA2) -
     &    Mh02*(MHH2*S2B*SBA2*AllHahx(622) + 2*AllHahx(623)) +
     &    Mh02**2*S2B*SBA2*AllHahx(702) + 2*MHH2*AllHahx(704)

        AllHahx(746) =
     &   -8*Mh02**6*S2B**5*AllHahx(706) + 4*MHH2**5*AllHahx(711) -
     &    Mh02**4*MHH2*(MHH2*S2B**5*AllHahx(712) -
     &       4*AllHahx(719)) +
     &    Mh02**2*MHH2**3*(MHH2*S2B**5*AllHahx(720) +
     &       4*AllHahx(727)) -
     &    2*Mh02*MHH2**4*(MHH2*S2B**5*AllHahx(728) +
     &       4*AllHahx(735)) +
     &    Mh02**3*MHH2**2*(MHH2*S2B**5*AllHahx(736) -
     &       4*AllHahx(743)) + 2*Mh02**5*S2B**4*AllHahx(745)

        AllHahx(751) =
     &   S2B*AllHahx(749) + 8*m12sq*(Mh02 - MHH2)*AllHahx(750)

        AllHahx(754) =
     &   8*C2B*CBA*m12sq*SBA*(3 + SBA2) -
     &    Mh02*S2B*SBA2*AllHahx(702) + 2*AllHahx(753)

        AllHahx(804) =
     &   4*AllHahx(763) - Mh02**3*MHH2*
     &     (MHH2*S2B**5*AllHahx(764) - 8*AllHahx(772)) -
     &    Mh02**4*(MHH2*S2B**5*AllHahx(774) - 16*AllHahx(783)) +
     &    2*AllHahx(803)

	AllHahx(822) = SAB*SBA2 - C2B*SBA*(1 + SBA2)

        AllHahx(826) =
     &   4*m12sq - 2*(MHp2*S2B + CBA*MHH2*SAB) + 2*MHH2*S2B*SBA2 +
     &    Mh02*(2*CAB*SBA - S2B*SBA2)

	AllHahx(833) = CB2**2*Lambda6**2 + Lambda7**2*SB2**2

        AllHahx(85) = 2*CBA2*Mh02*(Mh02 - MHp2) +
     &    (MHH2 - MHp2)*(Mh02 + MHH2 - MHp2)*(1 + SBA2)

        AllHahx(853) =
     &   -2*(Mh02 - 2*MHp2)*(MHH2 - MHp2)*S2B + S2A*AllHahx(852)

        AllHahx(860) =
     &   (2*CBA2)/(Mh02 - MHp2)**2 + SBA2/(MHH2 - MHp2)**2

        AllHahx(861) =
     &   (2*CBA2*Mh02)/(Mh02 - MHp2) +
     &    ((2*Mh02 - MHH2)*SBA2)/(MHH2 - MHp2)

        AllHahx(870) =
     &   MHH2*(MHH2 - 4*MHp2)*AllHahx(865) -
     &    (MHH2 - MHp2)**2*AllHahx(869)

	AllHahx(872) = S2B - (3*S2A - 2*S2B)*SBA2

        AllHahx(875) =
     &   SAB*(MHH2*S2B*AllHahx(873) - 2*m12sq*AllHahx(874)) -
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*AllHahx(874))

	AllHahx(885) = 99*S2A**2 + 28*S2A*S2B + 16*S2B**2

	AllHahx(889) = 27*S2A**2 - 42*S2A*S2B + 8*S2B**2

        AllHahx(895) =
     &   Mh02**2*AllHahx(892) - Mh02*MHp2*AllHahx(893) -
     &    4*MHp2**2*AllHahx(894)

        AllHahx(896) =
     &   (32 + 10*CBA2 + 75*CBA2**2)*Mh02**2 +
     &    (32 + 46*CBA2 - 267*CBA2**2)*Mh02*MHp2 -
     &    4*(16 + 14*CBA2 + 33*CBA2**2)*MHp2**2

        AllHahx(897) =
     &   (32 + 85*CBA2)*Mh02**2 + (32 - 221*CBA2)*Mh02*MHp2 -
     &    4*(16 + 47*CBA2)*MHp2**2

        AllHahx(899) =
     &   3*Mh02*MHp2*(3*S2A - 2*S2B) + 4*MHp2**2*S2B +
     &    Mh02**2*(27*S2A + 2*S2B)

        AllHahx(907) =
     &   Mh02**2*S2B*(27*S2A + 2*S2B)*SBA - Mh02*AllHahx(905) +
     &    4*MHp2*AllHahx(906)

        AllHahx(91) = (2*(Mh02 - MHp2))/(Mh02 - MHH2) -
     &    (MHH2 - MHp2)/(Mh02 - MHp2)

        AllHahx(927) =
     &   -((Mh02 + 2*MHH2)*S2A*S2B) + 2*m12sq*(3*S2A + S2B) +
     &    (6*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

	AllHahx(94) = MHH2**2 - Mh02*(2*MHH2 - MHp2)

	AllHahx(947) = -20 + 13*SBA2 - 36*SBA2**2 + 135*SBA2**3

	AllHahx(948) = 16 - 9*SBA2 + 12*SBA2**2 + 27*SBA2**3

	AllHahx(949) = 40 + 11*SBA2 - 12*SBA2**2 + 81*SBA2**3

	AllHahx(950) = 136 + 55*SBA2 - 108*SBA2**2 + 621*SBA2**3

        AllHahx(952) =
     &   10*MHH2**6 - 71*MHH2**5*MHp2 + 112*MHH2**4*MHp2**2 +
     &    60*MHH2**3*MHp2**3 + 24*MHH2**2*MHp2**4

        AllHahx(953) =
     &   SW2*(6*MHH2**6 - 11*MHH2**5*MHp2 - 4*MHH2**4*MHp2**2 +
     &      66*MHH2**3*MHp2**3 - 8*MHH2**2*MHp2**4)

        AllHahx(954) =
     &   6*MHH2**6 - 11*MHH2**5*MHp2 - 4*MHH2**4*MHp2**2 +
     &    66*MHH2**3*MHp2**3 - 8*MHH2**2*MHp2**4

        AllHahx(957) =
     &   11*MHH2**4 - 55*MHH2**3*MHp2 + 104*MHH2**2*MHp2**2 -
     &    144*MHH2*MHp2**3 + 192*MHp2**4

        AllHahx(959) =
     &   MHH2**4 - 9*MHH2**3*MHp2 + 24*MHH2**2*MHp2**2 -
     &    16*MHH2*MHp2**3

        AllHahx(961) =
     &   3*MHH2**4 - 23*MHH2**3*MHp2 + 40*MHH2**2*MHp2**2 +
     &    16*MHH2*MHp2**3

        AllHahx(963) =
     &   5*MHH2**4 - 17*MHH2**3*MHp2 - 20*MHH2**2*MHp2**2 +
     &    32*MHH2*MHp2**3

        AllHahx(965) =
     &   -((Lambda6*Lambda7*S2B**2 - 2*AllHahx(833))*AllHahx(964))

        AllHahx(967) =
     &   -((Lambda6*Lambda7*S2B**2 - 2*AllHahx(833))*AllHahx(966))

	AllHahx(970) = S2B*(8 - 13*SBA2) + 75*S2A*SBA2

        AllHahx(971) =
     &   3*S2A*(2 - 27*SBA2)*SBA2 - S2B*(8 - 7*SBA2 - 6*SBA2**2)

        AllHahx(973) =
     &   3*S2A*SBA2*(2 + 9*SBA2) + S2B*(44 - 13*SBA2 + 24*SBA2**2)

        AllHahx(974) =
     &   18*S2A*(2 - 15*SBA2)*SBA2 +
     &    S2B*(128 + 41*SBA2 + 63*SBA2**2)

        AllHahx(975) =
     &   27*S2A*(2 - 23*SBA2)*SBA2 +
     &    S2B*(56 + 79*SBA2 + 66*SBA2**2)

        AllHahx(976) =
     &   MHH2**5*AllHahx(971) + 64*S2B*AllHahx(972) -
     &    16*MHH2**2*MHp2**3*AllHahx(973) +
     &    4*MHH2**3*MHp2**2*AllHahx(974) -
     &    MHH2**4*MHp2*AllHahx(975)

	AllHahx(978) = 567*S2A*SBA2 - S2B*(56 + 145*SBA2)

	AllHahx(982) = 117*S2A*SBA2 - 4*S2B*(16 + 13*SBA2)

        AllHahx(984) =
     &   -6*MHp2*S2B*SAB*SBA2 +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*
     &       (4 - 9*SBA2 + 3*SBA2**2))

        AllHahx(987) =
     &   5*MHH2**5 - MHH2**4*MHp2 - 40*MHH2**3*MHp2**2 -
     &    48*MHH2**2*MHp2**3 + 192*MHH2*MHp2**4

        AllHahx(988) =
     &   25*MHH2**5 - 189*MHH2**4*MHp2 + 312*MHH2**3*MHp2**2 +
     &    176*MHH2**2*MHp2**3

        AllHahx(989) =
     &   SW2*(6*MHH2**5 - 11*MHH2**4*MHp2 - 4*MHH2**3*MHp2**2 +
     &      66*MHH2**2*MHp2**3 - 8*MHH2*MHp2**4)

        AllHahx(990) =
     &   6*MHH2**5 - 11*MHH2**4*MHp2 - 4*MHH2**3*MHp2**2 +
     &    66*MHH2**2*MHp2**3 - 8*MHH2*MHp2**4

        AllHahx(992) =
     &   -2*MHp2*S2B*SAB*(16 - 3*SBA2 + 33*SBA2**2) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*AllHahx(947))

        AllHahx(993) =
     &   -12*MHp2*S2B*SAB*SBA2 +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*AllHahx(948))

        AllHahx(994) =
     &   MHp2*S2B*SAB*(136 + SBA2 + 567*SBA2**2) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*AllHahx(949))

        AllHahx(995) =
     &   -8*MHp2*S2B*SAB*(20 + 5*SBA2 - 117*SBA2**2) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*AllHahx(950))

        AllHahx(10) = 6*CBA*SAB - 10*CAB*SBA -
     &    2*S2A*(1 - SBA2) + S2B*(-3 + 5*SBA2)

        AllHahx(1000) =
     &   Mh02 + ((Mh02 - MHH2)*(MHH2 - 2*MHp2))/(MHH2 - 4*MHp2)

	AllHahx(1009) = CBA2*(MHH2 - MHp2) - (Mh02 - MHp2)*SBA2

        AllHahx(1013) =
     &   -2*CAB*(2*m12sq - Mh02*S2B) -
     &    Mh02*S2B*SBA*(S2B + S2A*SBA2) + 2*AllHahx(1012)

	AllHahx(1032) = Mh02**2 + 6*MHH2*MHp2 - 2*Mh02*(MHH2 + MHp2)

        AllHahx(1048) =
     &   CBA2*(Mh02 - 2*MHp2)*(MHH2 - MHp2) -
     &    (Mh02 - MHH2)*MHp2*(1 + SBA2)

        AllHahx(1053) =
     &   (Mh02**2*AllHahx(1009) + Mh02*AllHahx(1052))/
     &     ((Mh02 - MHp2)*(MHH2 - MHp2)) +
     &    AllHahx(861)*(3*Mh02 + 3/2.D0*MHH2)

        AllHahx(106) =
     &   ((Mh02 - MHp2)**2*(MHH2 - MHp2))/MHH2 +
     &    (MHH2*(Mh02 - MHp2)*(Mh02 + MHp2))/(MHH2 - MHp2)

        AllHahx(1081) =
     &   -(m12sq*(Mh02 + CBA2*(Mh02 - 4*MHp2))) +
     &    Mh02*(Mh02 - 2*MHp2)*S2B

	AllHahx(1082) = (Mh02 - 4*MHp2)*S2A - (Mh02 - 2*MHp2)*S2B

        AllHahx(1085) =
     &   -4*CAB*(m12sq - 3*CBA2*m12sq + Mh02*S2B) +
     &    S2B*SBA*AllHahx(1084) +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(1 - 3*CBA2)*(CB2*Lambda6 - Lambda7*SB2))

        AllHahx(1094) =
     &   2*CAB*AllHahx(1089) + 4*(Mh02 - MHp2)**2*AllHahx(1090) +
     &    CBA2*AllHahx(1093) +
     &    (6*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*CBA2**2*(Mh02 - 4*MHp2)*(Mh02 - 2*MHp2)*
     &       (CB2*Lambda6 - Lambda7*SB2))

	AllHahx(11) = 8*CBA*SAB - 4*S2A*(1 - SBA2) + 3*S2B*SBA2

        AllHahx(1102) =
     &   4*((Mh02 - MHp2)**2*AllHahx(1090) + CAB*AllHahx(1098)) +
     &    MHp2*AllHahx(1101)

	dup2 = 2*MHp2*S2B*SBA2 - MHH2*AllHahx(1161)

        AllHahx(1168) =
     &   CBA*dup2*S2B - 2*SAB*AllHahx(1162) +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*
     &       (1 - (1 - 3*SBA2)*SBA2))

	AllHahx(123) = 2*SAB*(1 + 3*SBA2) + CBA*(S2B - 3*S2A*SBA2)

        AllHahx(1242) =
     &   -2*(4*Mh02 - MHH2)*MHH2*(CBA2*S2A - 6*CAB*SBA) +
     &    S2A*SBA2*AllHahx(1241)

        AllHahx(1244) =
     &   -6*CBA2*(4*Mh02 - MHH2)*MHH2 + SBA2*AllHahx(1243)

        AllHahx(1249) =
     &   -2*CBA2*(4*Mh02 - MHH2)*MHH2*
     &     (4*m12sq*SB2*AllHahx(1245) + S2B*AllHahx(1246)) +
     &    SBA2*AllHahx(1243)*AllHahx(1248)

        AllHahx(125) =
     &   (2*m12sq - MHH2*S2B)*SAB +
     &    (MW2*SW2)/(Alfa*Pi)*((CB2*Lambda6 - Lambda7*SB2)*SBA)

	AllHahx(1259) = 2*CBA2 + (1 - Mh02/MHH2)*SBA2

	AllHahx(1273) = 6*Mh02**2 - 6*Mh02*MHH2 + MHH2**2

        AllHahx(1274) =
     &   -(m12sq*(Mh02 + CBA2*(Mh02 - 4*MHH2))) +
     &    Mh02*(Mh02 - 2*MHH2)*S2B

	AllHahx(1276) = -4*m12sq*(3*S2A - S2B) + S2B*AllHahx(1275)

	AllHahx(1277) = 4*CBA2*MHH2 - Mh02*(1 + CBA2 + 6*SBA2)

        AllHahx(144) =
     &   MHH2**2*S2B*AllHahx(137) + MHp2*AllHahx(140) -
     &    2*MHH2*(MHp2*S2B*AllHahx(141) + 2*CBA*AllHahx(143))

        AllHahx(15) = 4*MHp2*S2B*(1 + SBA2) +
     &    2*Mh02*AllHahx(10) - MHH2*AllHahx(14)

        AllHahx(156) =
     &   MHp2*S2B**2*(6 + 5*SBA2 - SBA2**2) - 4*CBA*AllHahx(155)

        AllHahx(160) =
     &   -2*m12sq*AllHahx(159) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*(5 - 3*SBA2))

        AllHahx(161) =
     &   MHH2**2*S2B*AllHahx(157) -
     &    MHH2*(MHp2*S2B*AllHahx(158) + 2*CBA*AllHahx(160))

        AllHahx(1616) =
     &   4*MHp2*S2B - 3*Mh02*(S2B + S2A*SBA2) -
     &    MHH2*(S2B + 3*S2A*(3 + 2*SBA2))

        AllHahx(1617) =
     &   CBA2**2*(3*S2A - S2B) + 12*CBA2*S2A*SBA2 +
     &    (3*S2A + S2B)*SBA2**2

        AllHahx(1619) =
     &   6*MHH2*S2A - 2*(MHH2 + 2*MHp2)*S2B + 3*Mh02*(S2A + S2B)

        AllHahx(1621) =
     &   4*MHp2*S2B*(CBA2 - SBA2) +
     &    MHH2*(3*S2A - S2B)*(CBA2 + 2*SBA2) + 3*Mh02*AllHahx(1620)

	AllHahx(1623) = 3*(2*Mh02 + MHH2)*S2A - (MHH2 - 4*MHp2)*S2B

	AllHahx(1624) = 9*Mh02*S2A - (3*Mh02 + MHH2 - 4*MHp2)*S2B

        AllHahx(1626) =
     &   -2*(1 + 3*CBA2**2)*m12sq + (1 + 3*CBA2)*Mh02*S2B

        AllHahx(1629) =
     &   S2A*S2B*SBA*AllHahx(1628) -
     &    (12*MW2*SW2)/(Alfa*Pi)*
     &     (SAB*(CB2*Lambda6 - Lambda7*SB2)*(1 + 3*SBA2**2))

	AllHahx(1639) = CBA*(MHH2 - 2*MHp2)*S2B - MHH2*SAB

        AllHahx(1640) =
     &   CBA*(MHH2 - 2*MHp2)*S2B**2 + 2*(2*m12sq - MHH2*S2B)*SAB

        AllHahx(1645) =
     &   CBA2*(MHH2 - 2*MHp2)**2 + (Mh02 - 2*MHp2)**2*SBA2

        AllHahx(1647) =
     &   2*CAB*CBA*(2*m12sq - Mh02*S2B) + SBA*AllHahx(1646)

	AllHahx(1653) = Mh02**2 - 2*Mh02*MHH2 + MHH2**2

	AllHahx(1654) = Mh02*MHH2 - MHH2**2 + 2*(Mh02 - MHH2)*MHp2

	AllHahx(1655) = -3*(Mh02 + MHH2)*S2A + (Mh02 - MHH2)*S2B

	AllHahx(1658) = 3*m12sq*MHH2*SBA2**2 + SBA2*AllHahx(1657)

        AllHahx(1661) =
     &   (CB2*Lambda6 - Lambda7*SB2)*
     &    (3*MHH2*SBA2**2 + AllHahx(1660))

	AllHahx(1663) = 2*m12sq*(3*S2A - S2B) + (Mh02 - MHH2)*S2A*S2B

        AllHahx(1664) =
     &   -(CAB*(2*CBA2*m12sq - Mh02*S2B)) +
     &    (MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

        AllHahx(1669) =
     &   2*CBA*SBA2*AllHahx(1665) -
     &    CBA2*(SBA2*AllHahx(1666) - 2*SBA*AllHahx(1668))

        AllHahx(1676) =
     &   CAB*CBA2*AllHahx(1675) +
     &    (3*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*CBA2**2*Mh02*(CB2*Lambda6 - Lambda7*SB2))

        AllHahx(1679) =
     &   -2*SAB*AllHahx(1677) + MHH2*AllHahx(1678) +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (MHp2*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHahx(1688) =
     &   -2*CAB*(2*m12sq - Mh02*S2B) - SBA*AllHahx(1687) +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*(1 + 3*SBA2))

        AllHahx(1691) =
     &   CBA*MHH2*(3*S2A - S2B)*S2B +
     &    4*SAB*(m12sq - 2*MHH2*S2B + 3*m12sq*SBA2) +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*(1 + 3*SBA2))

        AllHahx(1696) =
     &   CAB*(2*m12sq - Mh02*S2B) + (Mh02 - 2*MHH2)*S2B**2*SBA -
     &    (2*MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

        AllHahx(1697) =
     &   Mh02*MHH2*S2B**2*SBA2 +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(Mh02 - 2*MHH2)*(CB2*Lambda6 - Lambda7*SB2)*SBA)

	AllHahx(1699) = 4*m12sq**2 + MHH2**2*S2B**2

	AllHahx(170) = 9*S2A**2 + 12*S2A*S2B + 7*S2B**2

        AllHahx(1700) =
     &   -2*m12sq*MHH2*S2B*SAB +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((2*m12sq - MHH2*S2B)*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHahx(1703) =
     &   (Mh02 + 2*MHH2)*S2A*S2B - 2*m12sq*(3*S2A + S2B)

        AllHahx(1707) =
     &   (2*m12sq - MHH2*S2B)*SAB*AllHahx(1705) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*AllHahx(1706))

        AllHahx(1721) =
     &   -3*CBA2*MHH2**2*(1 - Re(AllHahl(4))) -
     &    4*(MHH2 - MHp2)**2*SBA2*(1 - Re(AllHahl(5)))

	dup3 = Lambda6*Lambda7*S2B**2 - 2*AllHahx(833)

        AllHahx(1737) =
     &   Mh02**2*S2A**2*S2B**2*SBA2 -
     &    4*CAB*(2*CBA2*m12sq - Mh02*S2B)*AllHahx(1735) +
     &    4*AllHahx(1736) -
     &    (2*MW2**2*SW2**2)/(Alfa2*Pi**2)*(CBA2**3*dup3)

	AllHahx(174) = Lambda6*S2B**2 - 2*Lambda7*SB2**2

        AllHahx(1744) =
     &   (Mh02 - MHH2)**2*Re(AllHahl(16)) +
     &    2*(Mh02 - MHp2)**2*Re(AllHahl(17))

        AllHahx(1745) =
     &   2*CBA2*(Mh02 - MHp2)*(Mh02 + MHp2)*AllHahx(1744) +
     &    3*SBA2*AllHahx(1743)*Re(AllHahl(15))

        AllHahx(1755) =
     &   2*CBA2*AllHahx(1744) + 3*Mh02**2*SBA2*Re(AllHahl(15))

        AllHahx(1765) =
     &   4*m12sq*(2*Mh02 + MHH2)*S2A + S2A**2*AllHahx(1764)

        AllHahx(1768) =
     &   -(CBA2*(4*Mh02 + 3*MHH2 - 8*MHp2)*S2B**2) +
     &    3*S2A*S2B*AllHahx(1767)

        AllHahx(1770) =
     &   -3*(5*Mh02**2 + Mh02*MHH2)*S2A*S2B**2 +
     &    S2B**3*AllHahx(1769)

        AllHahx(1771) =
     &   4*m12sq**2 - 2*m12sq*(Mh02 + MHH2)*S2B + Mh02*MHH2*S2B**2

        AllHahx(1782) =
     &   10*CB2*CBA2*Lambda6*(2*m12sq - MHH2*S2B)*SAB +
     &    Lambda7*SB2*AllHahx(1775) + SBA2*AllHahx(1778) +
     &    3*SBA2**2*(CB2*Lambda6*AllHahx(1779) +
     &       CBA*Lambda7*SB2*AllHahx(1780)) + 18*AllHahx(1781)

	AllHahx(1784) = -2*CBA2 + 3*CBA2**2 + 9*CBA2**4

        AllHahx(1785) =
     &   (4*CBA2 - 3*CBA2**3)*
     &    (Lambda6*Lambda7*S2B**2 - 2*AllHahx(833))

	AllHahx(1786) = -(Lambda6*Lambda7*S2B**2) + 2*AllHahx(833)

        AllHahx(1791) =
     &   -12*m12sq**2*(3*S2A + S2B)*SBA2**2 + SBA2*AllHahx(1790)

        AllHahx(1794) =
     &   -(CB2*Lambda6*SBA*AllHahx(1774)) + 2*CAB*AllHahx(1793)

        AllHahx(1795) =
     &   -3*(Mh02 + 2*MHH2)*S2A*S2B*SAB*SBA2**2 +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CAB*CB2*(CBA2 + 9*CBA2**3)*Lambda6)

	AllHahx(1797) = 2*CBA2**2 + 3*CBA2*SBA2*Re(AllHahl(15))

        AllHahx(1799) =
     &   -((Mh02 - MHH2)**2*Re(AllHahl(16))) +
     &    2*AllHahx(1798)*Re(AllHahl(17))

        AllHahx(1801) =
     &   4*CBA2**3*m12sq**2 - 4*CBA2**2*m12sq*Mh02*S2B +
     &    CBA2*Mh02**2*S2B**2

	AllHahx(1802) = 2*CBA2**2*m12sq*Mh02*S2B - CBA2*Mh02**2*S2B**2

        AllHahx(1804) =
     &   -((CB2*Lambda6 - Lambda7*SB2)*
     &      (CBA2**2*Mh02*S2A*S2B*SBA - 2*CAB*AllHahx(1803)))

        AllHahx(1809) =
     &   -(Lambda6*Lambda7*S2B**2) + 2*AllHahx(833) +
     &    9*SBA2**2*AllHahx(1786)

	AllHahx(1820) = 4*m12sq**2 - 4*m12sq*MHH2*S2B + MHH2**2*S2B**2

        AllHahx(1824) =
     &   6*m12sq*(2*Mh02 + MHH2)*S2A**2*S2B -
     &    6*m12sq**2*(9*S2A**2 - 2*S2A*S2B) +
     &    S2B**2*AllHahx(1822) - 2*S2B**4*AllHahx(1823)

        AllHahx(1826) =
     &   CBA*AllHahx(1825) +
     &    (18*MW2*SW2)/(Alfa*Pi)*(Lambda7*m12sq*SB2*SBA*SBA2**2)

	AllHahx(1830) = -MHH2**2 + 2*MHH2*MHp2 - MHp2**2

        AllHahx(1831) =
     &   -4*(CAB**2*AllHahx(1810) + CAB*SBA*AllHahx(1811)) -
     &    S2B**4*SBA2*AllHahx(1814) +
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*AllHahx(1733))

        AllHahx(1833) =
     &   2*SAB**2*AllHahx(1820) +
     &    (MW2**2*SW2**2)/(Alfa2*Pi**2)*(SBA2*AllHahx(1786)) -
     &    2*(CBA*SAB*AllHahx(1825) +
     &       (MW2*SW2)/(Alfa*Pi)*
     &        ((CB2*Lambda6 - Lambda7*SB2)*SBA*AllHahx(1832)))

	AllHahx(1835) = m12sq**2*S2A + m12sq*Mh02*S2A**2

        AllHahx(1837) =
     &   AllHahx(1836)*Re(AllHahl(6)) - MHH2**2*Re(AllHahl(8))

        AllHahx(1848) =
     &   2*CBA2*(MHH2 - MHp2)**2*Re(AllHahl(5)) +
     &    CBA2**2*(Mh02 - MHH2)**2*Re(AllHahl(6))

        AllHahx(1863) =
     &   CBA2*AllHahx(1862) +
     &    4*(MHH2 - MHp2)**2*AllHahx(1860)*Re(AllHahl(5))

	AllHahx(187) = 45*S2A**2 - 228*S2A*S2B - 61*S2B**2

        AllHahx(19) = Mh02*AllHahx(17) -
     &    2*(MHp2*S2B*(1 + SBA2) + MHH2*AllHahx(18))

        AllHahx(191) =
     &   m12sq**2*S2B**2*SAB**2*(61 - 228*SBA2 - 45*SBA2**2) -
     &    m12sq*SAB*AllHahx(190)

	AllHahx(2) = -4*CBA2*(Mh02 - MHH2) + MHH2 - MHp2

	AllHahx(202) = 99*S2A**2 - 12*S2A*S2B + 29*S2B**2

	AllHahx(220) = (Mh02 - MHH2)*S2A - 2*MHp2*S2B

	AllHahx(222) = 6*Mh02**2*S2A - (Mh02 + MHp2)**2*S2B

	AllHahx(225) = 46*S2B - 64*CAB*SBA - (28*S2A - 25*S2B)*SBA2

	AllHahx(226) = 3*MHp2*(3 - SBA2) + 10*MHH2*SBA2

        AllHahx(227) =
     &   -2*CAB*m12sq + (MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2))

	AllHahx(228) = S2B*(9 + SBA2) + 4*(CAB*SBA + S2A*SBA2)

        AllHahx(230) =
     &   MHH2*(S2A*S2B + 5*S2B**2)*SBA2 - 2*SBA*AllHahx(229)

        AllHahx(233) =
     &   MHp2*S2B*AllHahx(231) -
     &    4*(MHH2*S2A*S2B*SBA2 - 4*SBA*AllHahx(232))

        AllHahx(236) =
     &   -2*MHp2*S2B**2 - Mh02*S2B*(S2A + 2*CAB*SBA - S2B*SBA2) +
     &    MHH2*AllHahx(235)

	AllHahx(238) = 11*MHp2*S2B**2 - 24*SBA*AllHahx(227)

	AllHahx(239) = 13*MHp2*S2B**2 - 12*SBA*AllHahx(227)

        AllHahx(244) =
     &   14*MHH2*S2B**2*SBA2 + S2B*AllHahx(242) -
     &    2*SBA*AllHahx(243)

	AllHahx(245) = 58*S2B - 28*CAB*SBA + (2*S2A + 15*S2B)*SBA2

        AllHahx(248) =
     &   Mh02**3*MHp2*S2B*AllHahx(246) -
     &    MHp2**3*(4*SBA*AllHahx(227) - S2B**2*AllHahx(247))

        AllHahx(255) =
     &   -16*C2B*CBA*m12sq*(Mh02 - 2*MHp2)*SBA -
     &    Mh02**2*S2B*SBA2*(S2A + 2*CAB*SBA - S2B*SBA2) +
     &    Mh02*AllHahx(252) -
     &    MHp2*(MHH2*S2B*SBA2*AllHahx(253) - 2*AllHahx(254))

        AllHahx(265) =
     &   2*Mh02**2*(Mh02 - MHp2)*S2B**2 -
     &    MHp2*(S2B*SBA2*AllHahx(262) - 2*SBA*AllHahx(264))

        AllHahx(27) = (2*Mh02*MHp2 + MHp2**2)*S2B -
     &    Mh02**2*(6*S2A + S2B)

        AllHahx(28) = -2*CAB*
     &     (m12sq - 3*CBA2*m12sq + Mh02*S2B) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(1 - 3*CBA2)*(CB2*Lambda6 - Lambda7*SB2))

        AllHahx(292) =
     &   -2*CAB*MHH2*(Mh02 + 2*MHH2)*(2*m12sq - Mh02*S2B)*SBA -
     &    SBA2*AllHahx(286) + 2*CBA*AllHahx(288) -
     &    CBA2*(Mh02 - 4*MHH2)*AllHahx(291)

        AllHahx(298) =
     &   -((4*Mh02 - MHH2)*AllHahx(296)) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*AllHahx(297))

        AllHahx(300) =
     &   Mh02**3 - 3*Mh02**2*MHH2 - Mh02*MHH2**2 - MHH2**3

        AllHahx(302) =
     &   (Mh02 - MHH2)*(2*Mh02**2 + MHH2**2)*S2A*S2B +
     &    2*(Mh02 - MHH2)**2*AllHahx(301)

        AllHahx(306) =
     &   MHH2*S2B*(S2A + S2B)*SBA2 - 2*m12sq*AllHahx(305) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA*(1 - 6*SBA2))

        AllHahx(309) =
     &   MHH2*S2B*AllHahx(307) +
     &    4*(m12sq*AllHahx(308) +
     &       (MW2*SW2)/(Alfa*Pi)*
     &        (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA*(1 - 6*SBA2)))

        AllHahx(315) =
     &   -2*m12sq*(3*S2A - S2B) + (2*Mh02 + MHH2)*S2A*S2B

        AllHahx(329) =
     &   -4*CAB*MHH2*(m12sq + Mh02*S2B)*SBA + CBA2*AllHahx(328)

        AllHahx(33) = (3 + 2*CBA2)*Mh02**3*S2B**2 -
     &    Mh02**2*AllHahx(29) -
     &    (Mh02 - MHp2)*S2B*SBA2*AllHahx(30) -
     &    2*Mh02*SBA*AllHahx(32)

        AllHahx(346) =
     &   -4*Mh02**4 + MHp2*AllHahx(344) - Mh02*AllHahx(345)

	AllHahx(348) = -2*m12sq*(MHH2 - MHp2) + S2B*AllHahx(347)

        AllHahx(351) =
     &   -2*C2B*m12sq*AllHahx(349) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*AllHahx(350))

        AllHahx(353) =
     &   6*CAB*(MHH2 - MHp2)*SBA +
     &    (MHH2*S2A - MHp2*(S2A + S2B))*SBA2

        AllHahx(357) =
     &   SBA2*AllHahx(356) +
     &    (MHH2 - MHp2)*SBA*
     &     (-2*(2*C2B*CBA*m12sq + CAB*AllHahx(354)) +
     &       (6*MW2*SW2)/(Alfa*Pi)*
     &        (CBA*(CB2*Lambda6 - Lambda7*SB2)))

        AllHahx(382) =
     &   MHH2**3*S2B*AllHahx(373) + 2*MHH2*MHp2*AllHahx(375) -
     &    MHH2**2*AllHahx(378) - MHp2**2*AllHahx(381)

	AllHahx(40) = 2*MHH2 - MHp2*(1 - 3*SBA2)*SBA2

        AllHahx(41) = -(MHH2*S2B*(MHH2 + MHp2*SBA2)) +
     &    m12sq*AllHahx(40)

	AllHahx(44) = -5 + 3*SBA2 + CBA2*(3 + 2*SBA2)

        AllHahx(449) =
     &   Mh02**4*S2B*(MHH2*AllHahx(416) - MHp2*AllHahx(417)) -
     &    Mh02**3*(MHH2 - MHp2)*AllHahx(422) -
     &    2*Mh02**2*AllHahx(432) - Mh02*AllHahx(442) +
     &    MHH2*MHp2*(S2B*AllHahx(445) + 2*AllHahx(448))

	AllHahx(453) = 2*C2B*m12sq + (MW2*SW2)/(Alfa*Pi)*(Lambda7*SB2)

        AllHahx(455) =
     &   -2*SB2*SBA*(4*MHH2*SAB - CBA*AllHahx(454)) +
     &    (MW2*SW2)/(Alfa*Pi)*(Lambda6*S2B*(1 + SBA2))

        AllHahx(458) =
     &   2*(3*CBA2*S2A - 4*CBA*SAB)*SBA2 + S2B*AllHahx(457)

        AllHahx(46) = 2*m12sq*SAB +
     &    (MW2*SW2)/(Alfa*Pi)*((CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHahx(479) =
     &   CBA2**2*S2B**2*AllHahx(474) +
     &    2*CBA*(Mh02 - MHH2)*MHH2*(Mh02 - MHp2)*AllHahx(478)

	AllHahx(49) = (3*CBA2*S2A - 4*CBA*SAB)*SBA2 - S2B*AllHahx(48)

        AllHahx(518) =
     &   (2*CBA2)/(Mh02 - MHH2) - (1 + SBA2)/(MHH2 - MHp2)

        AllHahx(557) =
     &   (Mh02 - MHp2)*(MHH2 - MHp2)*S2B**2*AllHahx(543) +
     &    2*CBA2*AllHahx(556)

	AllHahx(56) = MHH2**2 - 3*MHH2*MHp2 + MHp2**2

        AllHahx(611) =
     &   2*Mh02**3*(S2A - S2B) + 8*MHH2**3*(S2A + 3*S2B) +
     &    Mh02**2*MHH2*(S2A + 11*S2B) -
     &    Mh02*MHH2**2*(11*S2A + 27*S2B)

        AllHahx(614) =
     &   -4*m12sq*(Mh02 - MHH2)**2 + Mh02*S2B*AllHahx(613)

        AllHahx(616) =
     &   (Mh02 - 4*MHH2)*(2*m12sq - MHH2*S2B)*SAB +
     &    (3*MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*
     &       (Mh02 - 2*AllHahx(615)))

	AllHahx(62) = (Mh02 - MHp2)**2 + CBA2*(MHH2 - MHp2)**2

	AllHahx(63) = (1 - CBA2)*MHH2 + CBA2*MHp2

        AllHahx(64) = (Mh02 - MHp2)**4 -
     &    2*(Mh02 - MHp2)*(MHH2 - MHp2)**3

        AllHahx(65) = 2*(Mh02 - MHp2)**3*(MHH2 - MHp2) -
     &    (MHH2 - MHp2)**4

        AllHahx(675) =
     &   MHH2*S2B**4*AllHahx(619)*AllHahx(625) + Mh02*AllHahx(674)

	AllHahx(69) = 3*(Mh02 - MHH2)*S2A + (Mh02 + MHH2)*S2B

        AllHahx(70) = (Mh02 + MHH2)*S2A*S2B -
     &    (6*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHahx(747) =
     &   2*CBA2**2*(Mh02 - MHH2)**2*S2B**6*AllHahx(626)*
     &     AllHahx(701) - MHH2*S2B**4*AllHahx(619)*AllHahx(705) +
     &    CBA2*AllHahx(746)

        AllHahx(78) = MHH2 +
     &    (CBA2*(Mh02 - MHp2)*(Mh02 + MHH2 - MHp2))/MHH2 - MHp2

        AllHahx(805) =
     &   2*S2B**4*AllHahx(619)*
     &     (CBA2**2*AllHahx(751) + MHH2*AllHahx(754)) +
     &    CBA2*AllHahx(804)

        AllHahx(81) = (CBA2*(Mh02 + MHH2 - MHp2))/MHH2 +
     &    (2*(MHH2 - MHp2)*SBA2)/(Mh02 - MHp2)

        AllHahx(810) =
     &   S2A - 2*CBA*SAB - S2B*(7 - 22*SBA2) + 11*S2A*SBA2

        AllHahx(811) =
     &   S2B - 18*CBA*SAB - 8*S2B*SBA2 + S2A*(9 + 4*SBA2)

        AllHahx(812) =
     &   12*CBA*SAB + S2B*(14 + SBA2) - S2A*(6 + 13*SBA2)

	AllHahx(814) = 4*S2A - 8*CBA*SAB + S2B*(21 + SBA2)

        AllHahx(815) =
     &   -(MHp2**2*(S2A + 6*CAB*SBA - 3*S2B*SBA2)) +
     &    2*MHH2**2*AllHahx(134) + MHH2*MHp2*AllHahx(150)

        AllHahx(818) =
     &   12*CBA*SAB*SBA2 - S2B*(14 - 19*SBA2 - 5*SBA2**2)

	AllHahx(820) = 4*CBA*SAB*SBA2 + S2B*(3 - 21*SBA2 - 16*SBA2**2)

        AllHahx(821) =
     &   4*CBA*SAB*SBA2 + S2B*(17 - 34*SBA2 - 19*SBA2**2)

        AllHahx(823) =
     &   2*m12sq*AllHahx(822) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*(1 + 2*SBA2))

        AllHahx(83) = (CBA2*(Mh02 - MHH2 - MHp2))/MHH2 +
     &    ((MHH2 - MHp2)*(Mh02 - MHH2 + MHp2)*SBA2)/
     &     (Mh02*(Mh02 - MHp2))

        AllHahx(854) =
     &   -2*m12sq*(Mh02 - MHp2)*(3*S2A - S2B) + S2B*AllHahx(853)

        AllHahx(855) =
     &   -2*(1 - 3*CBA2)*(MHH2 - MHp2) + 3*(Mh02 - MHp2)*SBA2

        AllHahx(862) =
     &   MHp2*AllHahx(860) - AllHahx(861)/(4*Mh02 - MHH2)

        AllHahx(871) =
     &   AllHahx(870) + 4*(MHH2 - MHp2)**2*MHp2**2*
     &     (Sqrt(CW2) + SW)**2*(1 - 2*Sqrt(CW2)*SW)

        AllHahx(876) =
     &   CBA*MHH2*(MHH2 - 2*MHp2)*S2B*AllHahx(872) + 2*AllHahx(875)

	AllHahx(879) = 3*Mh02**2 - 11*Mh02*MHp2 - 4*MHp2**2

	AllHahx(880) = Mh02**2 - 5*Mh02*MHp2 + 4*MHp2**2

	AllHahx(884) = 81*S2A**2 + 12*S2A*S2B + 16*S2B**2

        AllHahx(886) =
     &   3*MHp2*S2B*SBA2*AllHahx(885) +
     &    (8*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(3*S2A + 8*S2B)*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHahx(887) =
     &   MHp2**2*S2B**4*SBA2 -
     &    (MW2**2*SW2**2)/(Alfa2*Pi**2)*dup3 +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*MHp2*S2B**2*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHahx(888) =
     &   3*MHp2**2*(S2A - 2*S2B)*S2B**3*SBA2 +
     &    (2*MW2**2*SW2**2)/(Alfa2*Pi**2)*dup3 +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (CBA*MHp2*(3*S2A - 8*S2B)*S2B*
     &       (CB2*Lambda6 - Lambda7*SB2)*SBA)

	AllHahx(89) = Mh02**2 - MHH2*(2*Mh02 - MHp2)

        AllHahx(890) =
     &   MHp2**2*S2B**2*SBA2*AllHahx(889) +
     &    (16*MW2**2*SW2**2)/(Alfa2*Pi**2)*dup3 -
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*MHp2*S2B*(15*S2A + 8*S2B)*
     &       (CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHahx(898) =
     &   4*m12sq**2*AllHahx(895) - 4*m12sq*Mh02*S2B*AllHahx(896) +
     &    Mh02**2*S2B**2*AllHahx(897)

	AllHahx(900) = Mh02**2 + Mh02*MHp2 - 2*MHp2**2

        AllHahx(904) =
     &   MHp2*S2B**2*SBA +
     &    (MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

        AllHahx(908) =
     &   -(m12sq*AllHahx(907)) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (CBA*Mh02*(25*Mh02 + 11*MHp2)*S2B*
     &       (CB2*Lambda6 - Lambda7*SB2))

	AllHahx(917) = CBA2/(Mh02 - MHp2)**2 + 2/(MHH2 - MHp2)**2

        AllHahx(918) =
     &   CBA2/(Mh02 - MHp2)**2 + (2*SBA2)/(MHH2 - MHp2)**2

        AllHahx(92) = MHH2 -
     &    (Mh02*(MHH2 - MHp2)**2)/(Mh02 - MHp2)**2 + MHp2 -
     &    (Mh02*(MHH2 - MHp2)*(MHH2 + MHp2))/
     &     (MHH2*(Mh02 - MHp2)) - 2*MHH2*AllHahx(91)

        AllHahx(920) =
     &   (CBA2*(Mh02 - 2*MHH2))/(Mh02 - MHp2) -
     &    (2*MHH2*SBA2)/(MHH2 - MHp2)

        AllHahx(943) =
     &   S2B**2*(127 - 3*SBA2) + 6*(9*S2A**2 + 16*S2A*S2B)*SBA2

        AllHahx(944) =
     &   9*(15*S2A**2 - 7*S2A*S2B)*SBA2 + S2B**2*(43 + 7*SBA2)

        AllHahx(95) = MHH2*(Mh02 - MHp2)**2*(CBA2 - 2*SBA2) +
     &    (MHH2 - MHp2)*AllHahx(94)

        AllHahx(951) =
     &   192*MHp2**4*SBA2 + 8*MHH2**2*MHp2**2*AllHahx(947) +
     &    16*MHH2*MHp2**3*AllHahx(948) + MHH2**4*AllHahx(949) -
     &    MHH2**3*MHp2*AllHahx(950)

        AllHahx(955) =
     &   5*MHH2**6 - 17*MHH2**5*MHp2 - 20*MHH2**4*MHp2**2 +
     &    32*MHH2**3*MHp2**3 + SBA2*AllHahx(952) +
     &    2*SW2*AllHahx(953) - 2*SW2**2*AllHahx(954)

        AllHahx(958) =
     &   (Lambda6*Lambda7*S2B**2 - 2*AllHahx(833))*AllHahx(957)

        AllHahx(96) = MHH2**2 - Mh02*(MHH2 + MHp2) -
     &    MHH2*MHp2*SBA2 + MHp2**2*(1 + SBA2)

        AllHahx(960) =
     &   -((Lambda6*Lambda7*S2B**2 - 2*AllHahx(833))*AllHahx(959))

        AllHahx(962) =
     &   -((Lambda6*Lambda7*S2B**2 - 2*AllHahx(833))*AllHahx(961))

        AllHahx(968) =
     &   -((Lambda6*Lambda7*S2B**2 - 2*AllHahx(833))*
     &       AllHahx(963)) + 2*SW2*AllHahx(965) -
     &    2*SW2**2*AllHahx(967)

	AllHahx(977) = MHH2**6*S2B*AllHahx(970) + 2*m12sq*AllHahx(976)

        AllHahx(979) =
     &   MHp2*S2B*SAB*AllHahx(978) -
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*AllHahx(971))

        AllHahx(98) = (CBA2*(MHH2 - MHp2))/
     &     (MHH2*(Mh02 - MHp2)) - SBA2/Mh02

        AllHahx(980) =
     &   8*MHp2*S2B**2*SAB*(2 - 3*SBA2) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*AllHahx(973))

        AllHahx(981) =
     &   44*MHp2*S2B*SAB*(3*S2A*SBA2 + S2B*(4 + SBA2)) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*AllHahx(974))

        AllHahx(983) =
     &   -8*MHp2*S2B*SAB*AllHahx(982) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*AllHahx(975))

        AllHahx(985) =
     &   MHH2*MHp2**4*AllHahx(984) +
     &    (6*MW2*SW2)/(Alfa*Pi)*
     &     (MHp2**5*(CB2*Lambda6 - Lambda7*SB2)*SBA*SBA2)

        AllHahx(991) =
     &   40*MHH2**5 - 136*MHH2**4*MHp2 - 160*MHH2**3*MHp2**2 +
     &    256*MHH2**2*MHp2**3 + SBA2*AllHahx(987) +
     &    3*SBA2**2*AllHahx(988) + 16*SW2*AllHahx(989) -
     &    16*SW2**2*AllHahx(990)

        AllHahx(996) =
     &   -5*MHH2**5*S2B*SAB*(8 + SBA2 + 15*SBA2**2) +
     &    8*MHH2**2*MHp2**2*AllHahx(992) +
     &    16*MHH2*MHp2**3*AllHahx(993) + MHH2**4*AllHahx(994) -
     &    MHH2**3*MHp2*AllHahx(995) +
     &    (192*MW2*SW2)/(Alfa*Pi)*
     &     (MHp2**4*(CB2*Lambda6 - Lambda7*SB2)*SBA*SBA2)

	AllHahx(1030) = MHH2 - ((MHH2 - MHp2)*MHp2)/(Mh02 - 4*MHp2)

        AllHahx(105) =
     &   2*Mh02**2*MHH2**2 + 4*Mh02*MHH2*MHp2**2 -
     &    2*(Mh02 + MHH2)*MHp2**3 + MHp2**4

        AllHahx(1083) =
     &   4*CAB*AllHahx(1081) + Mh02*S2B*SBA*AllHahx(1082) +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(Mh02 + CBA2*(Mh02 - 4*MHp2))*
     &       (CB2*Lambda6 - Lambda7*SB2))

        AllHahx(12) = 4*MHp2*S2B*(1 + SBA2) +
     &    2*Mh02*AllHahx(10) - MHH2*AllHahx(11)

        AllHahx(124) =
     &   4*m12sq*(SAB + 3*SAB*SBA2**2) - MHH2*S2B*AllHahx(123) +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*(1 + 3*SBA2**2))

        AllHahx(1250) =
     &   Mh02*S2B**3*AllHahx(1242) - CB2*AllHahx(1249) +
     &    (3*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*Lambda7*S2B**2*SB2*SBA*AllHahx(1244))

        AllHahx(126) =
     &   CBA2*(MHH2 - 2*MHp2)*S2B**2 + 2*CBA*AllHahx(125)

        AllHahx(1278) =
     &   -4*CAB*AllHahx(1274) + Mh02*SBA*AllHahx(1276) +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*AllHahx(1277))

        AllHahx(128) =
     &   MHH2**2 + 2*Mh02*(MHH2 - 4*MHp2) - 4*MHH2*MHp2 - 8*MHp2**2

	AllHahx(133) = MHH2**2 - 5*MHH2*MHp2 + 4*MHp2**2

        AllHahx(1618) =
     &   S2B*SAB*SBA*AllHahx(1616) +
     &    (3*MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*AllHahx(1617))

        AllHahx(162) =
     &   4*MHH2**3*S2B**2*(1 + SBA2) - MHp2**2*AllHahx(156) +
     &    MHp2*AllHahx(161)

        AllHahx(1622) =
     &   MHH2*S2B*SAB*SBA*AllHahx(1619) -
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*AllHahx(1621))

        AllHahx(1627) =
     &   (2*m12sq - Mh02*S2B)*(2*m12sq - MHH2*S2B)*SAB -
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*AllHahx(1626))

        AllHahx(1636) =
     &   CAB*(2*m12sq - Mh02*S2B) + (Mh02 - 2*MHp2)*S2B**2*SBA

        AllHahx(1637) =
     &   2*CAB*(2*m12sq - Mh02*S2B) + (Mh02 - 2*MHp2)*S2B**2*SBA

        AllHahx(1641) =
     &   (2*m12sq - MHH2*S2B)*SAB*
     &     (2*m12sq*SAB + S2B*AllHahx(1639)) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*AllHahx(1640))

        AllHahx(1643) =
     &   (Mh02 - MHp2)**2 - (Mh02 - MHp2)**2*Re(AllHahl(11))

	AllHahx(1651) = 3*CBA2**2*Mh02 + CBA2*(MHH2 + 2*MHp2)

	AllHahx(1652) = 3*Mh02**2 + Mh02*(MHH2 + 2*MHp2)

        AllHahx(1656) =
     &   S2A*S2B*AllHahx(1653) - S2B**2*AllHahx(1654) -
     &    2*m12sq*AllHahx(1655)

	AllHahx(166) = -m12sq - MHH2*S2B + 3*m12sq*SBA2

        AllHahx(1662) =
     &   2*SAB*AllHahx(1658) +
     &    (MW2*SW2)/(Alfa*Pi)*(SBA*AllHahx(1661))

        AllHahx(1670) =
     &   CBA2*(SBA2*AllHahx(1663) + 6*SBA*AllHahx(1664))*
     &     Re(AllHahl(1)) + AllHahx(1669)*Re(AllHahl(2))

        AllHahx(1671) =
     &   2*CAB*(2*m12sq - Mh02*S2B)*SBA + (Mh02 - MHH2)*S2B**2*SBA2

        AllHahx(1672) =
     &   (2*m12sq - MHH2*S2B)*SAB*SBA2 +
     &    (MW2*SW2)/(Alfa*Pi)*((CB2*Lambda6 - Lambda7*SB2)*SBA)

	AllHahx(168) = MHH2*S2B*(CBA*S2A - 2*SAB) + 2*SBA2*AllHahx(46)

	AllHahx(175) = 2*CB2**2*Lambda6**2 - Lambda7*AllHahx(174)

	AllHahx(1763) = 2*CAB**2*(CBA2 + 9*CBA2**3) + 9*CBA2**2*S2A**2

	AllHahx(1766) = 20*CAB**2*CBA2*Mh02**2 + CBA2**2*AllHahx(1765)

        AllHahx(1772) =
     &   -12*CBA2**2*m12sq**2*(3*S2A - S2B) +
     &    2*m12sq*AllHahx(1768) + CBA2*AllHahx(1770) -
     &    6*CBA*SAB*AllHahx(1771)

        AllHahx(1783) =
     &   6*CAB*CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA2*
     &     AllHahx(1626) + SBA*AllHahx(1782)

        AllHahx(1787) =
     &   -((-(Lambda6*Lambda7*S2B**2) + 2*Lambda7**2*SB2**2)*
     &       AllHahx(1784)) -
     &    6*(CB2**2*(CBA2**2 + 3*CBA2**4)*Lambda6**2 +
     &       SBA2*AllHahx(1785)) - 9*CBA2**2*SBA2**2*AllHahx(1786)

        AllHahx(1796) =
     &   -(SAB*AllHahx(1791)) + 2*m12sq*AllHahx(1795) +
     &    (MW2*SW2)/(Alfa*Pi)*AllHahx(1794)

        AllHahx(1800) =
     &   (2*CBA2 + CBA2**2)*MHH2**2 +
     &    4*(CBA2**2*Mh02 - CBA2*MHH2)*MHp2 -
     &    Mh02**2*AllHahx(1797) +
     &    2*((CBA2 - CBA2**2)*MHp2**2 + CBA2**2*AllHahx(1799))

        AllHahx(1805) =
     &   CBA2*Mh02**2*S2A**2*S2B**2*SBA2 +
     &    4*CAB**2*AllHahx(1801) +
     &    (2*MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (CBA2**4*AllHahx(1786)) -
     &    4*(CAB*S2A*SBA*AllHahx(1802) +
     &       (MW2*SW2)/(Alfa*Pi)*(CBA*AllHahx(1804)))

        AllHahx(1838) =
     &   2*m12sq**2*S2B**2 -
     &    6*S2B*(m12sq*MHH2*S2A**2 + 2*AllHahx(1835)) +
     &    S2B**4*AllHahx(1837) -
     &    (2*MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (AllHahx(1786)*(Re(AllHahl(7)) - Re(AllHahl(21))))

        AllHahx(1851) =
     &   2*CBA2**2*AllHahx(1744) +
     &    3*CBA2*Mh02**2*SBA2*Re(AllHahl(15))

        AllHahx(223) =
     &   CBA2*Mh02**2*S2B**2 + 4*Mh02*SBA*AllHahx(28) -
     &    S2B*SBA2*AllHahx(222)

        AllHahx(234) =
     &   Mh02**3*S2B*AllHahx(225) +
     &    2*MHp2**2*(S2B**2*AllHahx(226) - 4*SBA*AllHahx(227)) -
     &    2*Mh02*MHp2*(3*MHp2*S2B*AllHahx(228) + 2*AllHahx(230)) -
     &    Mh02**2*AllHahx(233)

        AllHahx(240) =
     &   17*Mh02**2*S2B**2 - MHp2*AllHahx(238) +
     &    2*Mh02*AllHahx(239)

        AllHahx(249) =
     &   8*Mh02**4*S2B**2 - 2*Mh02*MHp2**2*AllHahx(244) +
     &    Mh02**2*MHp2*(2*AllHahx(230) + MHp2*S2B*AllHahx(245)) +
     &    2*AllHahx(248)

        AllHahx(256) =
     &   CBA2**2*(4*Mh02 - 3*MHp2)*(Mh02 - MHp2)**2*S2B**2 -
     &    (Mh02 - MHp2)*AllHahx(255)

        AllHahx(26) = 2*Mh02**4 + 4*Mh02**2*MHp2**2 -
     &    4*Mh02*MHp2**3 + MHp2**4

	AllHahx(294) = 4*Mh02**2 - 5*Mh02*MHH2 + MHH2**2

        AllHahx(299) =
     &   CBA2*(Mh02*S2B**2 - 12*SBA*AllHahx(227))*AllHahx(294) +
     &    4*SBA*AllHahx(298)

        AllHahx(3) = 16*CAB*CBA2*Mh02*SBA +
     &    3*(MHH2 - MHp2)*S2B*SBA2**2 -
     &    2*SBA2*(8*CBA*MHH2*SAB - S2B*AllHahx(2))

        AllHahx(303) =
     &   (4*Mh02 - MHH2)*S2B**2*AllHahx(300) - 4*Mh02*AllHahx(302)

        AllHahx(34) = MHp2*SBA*AllHahl(31)*
     &     (S2B*SBA*AllHahx(27) + 4*Mh02*AllHahx(28)) +
     &    2*AllHahl(30)*AllHahx(33)

	AllHahx(340) = 2*CBA2*(Mh02 - MHp2) + (MHH2 - MHp2)*(1 + SBA2)

	AllHahx(341) = CBA2*(Mh02 - MHp2) + 2*(MHH2 - MHp2)*SBA2

        AllHahx(342) =
     &   (((Mh0 - MHp)*(Mh0 + MHp)*AllHahl(2)**2)/(MHH2 - MHp2) +
     &      ((MHH2 - MHp2)*MHp2*AllHahl(34))/(Mh02 - MHp2))/MHH2

        AllHahx(352) =
     &   CBA2**2*S2B**2*AllHahx(346) -
     &    2*CBA*(Mh02 - MHH2)*(Mh02 - MHp2)*
     &     (2*SAB*SBA2*AllHahx(348) - SBA*AllHahx(351))

        AllHahx(38) = 6*MHH2**3 + MHp2**3*SBA2 +
     &    2*MHH2*MHp2**2*(3 + SBA2)

	AllHahx(39) = 6*S2A*SBA2 - S2B*(16 + SBA2)

        AllHahx(4) = -2*C2B*m12sq +
     &    (MW2*SW2)/(Alfa*Pi)*(CB2*Lambda6 - Lambda7*SB2)

        AllHahx(42) = 2*SAB*AllHahx(41) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*AllHahx(40))

	AllHahx(45) = (3*CBA2*S2A - 4*CBA*SAB)*SBA2 - S2B*AllHahx(44)

        AllHahx(456) =
     &   CAB*CBA*Mh02*S2B**2 - S2B*(1 + SBA2)*AllHahx(453) +
     &    CB2*AllHahx(455)

        AllHahx(47) = 3*CBA2*MHp2*S2B**2 -
     &    2*CBA*(1 - 3*SBA2)*SBA2*AllHahx(46)

        AllHahx(50) = 2*CBA*(1 - 3*SBA2)*SBA2*AllHahx(46) +
     &    MHp2*S2B*AllHahx(49)

	AllHahx(53) = 2*Mh02**2 - MHH2*MHp2 + Mh02*(MHH2 + MHp2)

	AllHahx(54) = Mh02*(MHH2 - MHp2) + MHH2*(2*MHH2 + MHp2)

        AllHahx(57) = Mh02**2*MHH2 - MHH2**2*(MHH2 - 2*MHp2) +
     &    2*Mh02*AllHahx(56)

        AllHahx(61) = Mh02**3 + MHH2**3 -
     &    2*(Mh02**2 + MHH2**2)*MHp2 + (Mh02 + MHH2)*MHp2**2

        AllHahx(612) =
     &   -4*m12sq*(Mh02 - MHH2)**2*(3*S2A + S2B) + S2B*AllHahx(611)

        AllHahx(617) =
     &   CAB*SBA*AllHahx(614) + CBA*(Mh02 - MHH2)*AllHahx(616)

        AllHahx(66) = MHH2*(Mh02 - MHp2)**4 +
     &    Mh02*(MHH2 - MHp2)**4 -
     &    (Mh02 - MHH2)**2*MHp2*AllHahx(62) +
     &    (Mh0 - MHH)*(Mh0 + MHH)*(Mh02 - MHp2)*(MHH2 - MHp2)*
     &     (MHH2*MHp2 - Mh02*AllHahx(63)) +
     &    SBA2*(MHH2*AllHahx(64) - Mh02*AllHahx(65))

        AllHahx(68) = (MHH2*(Mh02 - MHp2))/
     &     (Mh02*(MHH2 - MHp2)) +
     &    (Mh02*(MHH2 - MHp2))/(MHH2*(Mh02 - MHp2))

	AllHahx(7) = CBA2*(Mh02 - MHH2)*S2B + 2*CBA*MHH2*SAB

	AllHahx(71) = 2*m12sq*AllHahx(69) - (Mh02 - MHH2)*AllHahx(70)

        AllHahx(74) = 4*MHH2*SB2*SBA +
     &    (MW2*SW2)/(Alfa*Pi)*(CBA*Lambda6)

        AllHahx(75) = ((Mh02 - 2*MHH2)*(Mh02 - MHp2))/
     &     (MHH2 - MHp2) - (Mh02*(MHH2 - MHp2))/(Mh02 - MHp2)

        AllHahx(76) = (Mh02 + 2*MHH2)*S2A*S2B -
     &    2*m12sq*(3*S2A + S2B) -
     &    (6*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHahx(79) = -2*CAB*(2*m12sq - Mh02*S2B) -
     &    (Mh02 - 2*MHH2)*S2B**2*SBA +
     &    (2*MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

        AllHahx(80) = (MHH2 - MHp2)**2*SBA2**2 +
     &    (MHH2 - MHp2)*SBA2*AllHahx(78) -
     &    ((Mh02 - MHH2 + MHp2)*SBA*AllHahx(79))/S2B**2

        AllHahx(806) =
     &   (4*AllHahl(2)*AllHahx(747))/((Mh02 - MHH2)**2*MHH2) +
     &    AllHahx(805)/(Mh02 - MHH2)

	AllHahx(809) = S2B*(2 - 11*SBA2) - 4*S2A*SBA2

        AllHahx(813) =
     &   MHH2**4*MHp2*AllHahx(810) +
     &    MHH2**2*MHp2**3*AllHahx(811) +
     &    MHH2**3*MHp2**2*AllHahx(812)

        AllHahx(816) =
     &   MHH2*MHp2**4*AllHahx(814) + Mh02*AllHahx(133)*AllHahx(815)

        AllHahx(856) =
     &   8*CAB*(MHH2 - MHp2)*(m12sq - 3*CBA2*m12sq + Mh02*S2B) -
     &    SBA*AllHahx(854) +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*AllHahx(855))

        AllHahx(881) =
     &   -9*CBA2**4*AllHahx(879) + 4*CBA2**3*AllHahx(880)

        AllHahx(882) =
     &   Mh02**3 - Mh02**2*MHp2 - 4*Mh02*MHp2**2 + 4*MHp2**3

        AllHahx(883) =
     &   (Mh02 - 2*MHp2)*S2B**2*SBA2 -
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHahx(891) =
     &   Mh02**4*S2B**2*SBA2*AllHahx(884) -
     &    Mh02**3*S2B*AllHahx(886) - 128*MHp2**2*AllHahx(887) -
     &    32*Mh02*MHp2*AllHahx(888) - 4*Mh02**2*AllHahx(890)

        AllHahx(90) = CBA2**2*(Mh02 - MHH2)**2*MHp2 +
     &    CBA2*(MHH2 - MHp2)*AllHahx(89)

        AllHahx(919) =
     &   AllHahx(79)**2*AllHahx(917) +
     &    SBA2*AllHahx(76)**2*AllHahx(918)

        AllHahx(921) =
     &   (-2*MHH2*AllHahx(79)**2)/(MHH2 - MHp2) +
     &    SBA2*AllHahx(76)**2*AllHahx(920)

        AllHahx(93) = ((MHH2 + CBA2*(Mh02 - MHp2))*
     &       (MHH2 - MHp2)**2)/(Mh02*MHH2) - CBA2*AllHahx(92)

	AllHahx(939) = MHH2*MHp2**5*(1 - 4*SBA2) + 2*MHp2**6*SBA2

	AllHahx(940) = S2B*(23 - 14*SBA2) + 6*S2A*SBA2

        AllHahx(941) =
     &   3*(27*S2A**2 - 4*S2A*S2B)*SBA2 + S2B**2*(48 + 11*SBA2)

        AllHahx(942) =
     &   3*(207*S2A**2 - 44*S2A*S2B)*SBA2 + S2B**2*(320 + 63*SBA2)

        AllHahx(945) =
     &   MHH2**3*MHp2**3*AllHahx(943) +
     &    MHH2**4*MHp2**2*AllHahx(944)

	AllHahx(956) = m12sq**2*AllHahx(951) + 2*S2B**2*AllHahx(955)

        AllHahx(969) =
     &   SBA2**2*AllHahx(958) + 12*SBA2**3*AllHahx(960) -
     &    27*SBA2**4*AllHahx(962) - 8*SBA2*AllHahx(968)

        AllHahx(97) = MHH2 + MHp2 -
     &    (4*(Mh02 - MHp2)*(MHH2 - MHp2)*SBA2)/(Mh02 - MHH2) +
     &    (2*AllHahx(95))/(Mh02 - MHH2)**2 +
     &    AllHahx(96)/(Mh02 - MHp2)

        AllHahx(986) =
     &   SAB*AllHahx(977) - MHH2**5*AllHahx(979) -
     &    16*MHH2**2*MHp2**3*AllHahx(980) +
     &    4*MHH2**3*MHp2**2*AllHahx(981) -
     &    MHH2**4*MHp2*AllHahx(983) + 64*S2B*AllHahx(985)

        AllHahx(997) =
     &   -2*m12sq*AllHahx(996) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (S2B*(CB2*Lambda6 - Lambda7*SB2)*SBA*AllHahx(991))

	AllHahx(1) = 5*MHH2**2 - 10*MHH2*MHp2 + 2*MHp2**2

        AllHahx(127) =
     &   CBA*AllHahl(26)*AllHahx(124) +
     &    (1 - SBA2)*AllHahl(36)*AllHahx(126)

        AllHahx(1613) =
     &   (Mh02*AllHahl(1))/(Mh02 - MHp2) -
     &    (MHH2*AllHahl(2))/(MHH2 - MHp2)

	AllHahx(1615) = Mh02*MHH2 - 4*(Mh02 + MHH2)*MHp2 + 4*MHp2**2

        AllHahx(1638) =
     &   (Mh02 - 2*MHp2)**2*S2B**4*SBA2 +
     &    4*CAB*(2*m12sq - Mh02*S2B)*AllHahx(1636) +
     &    (2*MW2**2*SW2**2)/(Alfa2*Pi**2)*(CBA2*AllHahx(175)) -
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*AllHahx(1637))

        AllHahx(1642) =
     &   CBA2*(MHH2 - 2*MHp2)**2*S2B**4 + 4*AllHahx(1641) +
     &    (2*MW2**2*SW2**2)/(Alfa2*Pi**2)*(SBA2*AllHahx(175))

        AllHahx(165) =
     &   4*m12sq*SAB - MHH2*S2B*(CBA*S2B + 2*SAB) +
     &    (2*MW2*SW2)/(Alfa*Pi)*((CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHahx(167) =
     &   CBA*MHH2*S2B*(3*S2A + S2B) + 4*SAB*AllHahx(166) -
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*(1 - 3*SBA2))

        AllHahx(1673) =
     &   -2*CAB*SBA*(2*m12sq*AllHahx(1651) -
     &       CBA2*S2B*AllHahx(1652)) + CBA2*SBA2*AllHahx(1656) +
     &    2*(CBA*AllHahx(1662) + AllHahx(1670)) -
     &    4*(CBA2*AllHahx(1671) - 2*CBA*AllHahx(1672))*
     &     Re(AllHahl(3))

        AllHahx(1674) =
     &   1 + 2*((AllHahl(1) - AllHahl(2))/(Mh02 - MHH2) +
     &       (AllHahl(2) - AllHahl(3))/(MHH2 - MHp2)) -
     &    (2*(AllHahl(1) - AllHahl(3)))/(Mh02 - MHp2)

        AllHahx(169) =
     &   3*AllHahx(165)**2 + 9*AllHahx(168)**2 -
     &    2*(AllHahx(165)**2 + 3*AllHahx(168)**2)

	AllHahx(23) = 5*Mh02**2 - 10*Mh02*MHp2 + 2*MHp2**2

        AllHahx(24) = -2*CAB*(2*m12sq - Mh02*S2B) -
     &    (Mh02 - 2*MHp2)*S2B**2*SBA +
     &    (2*MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

        AllHahx(25) = -2*CAB*(2*CBA2*m12sq - Mh02*S2B) +
     &    Mh02*S2A*S2B*SBA +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*CBA2*(CB2*Lambda6 - Lambda7*SB2))

	AllHahx(281) = 5*Mh02**2 - 16*Mh02*MHH2 + 2*MHH2**2

        AllHahx(282) =
     &   10*Mh02**2 + 6*Mh02*MHH2 - 8*MHH2**2 + MHH2**4/Mh02**2

        AllHahx(35) = (MHp2*SBA2*AllHahl(29)*AllHahx(26))/
     &     (Mh02 - MHp2)**2 + AllHahx(34)/S2B**2

        AllHahx(37) = 2*MHH2**4 + 4*MHH2**2*MHp2**2 -
     &    4*MHH2*MHp2**3 + MHp2**4

        AllHahx(43) = CBA*S2B*
     &     (S2B*AllHahx(38) + MHH2**2*MHp2*AllHahx(39)) +
     &    4*MHH2*AllHahx(42)

        AllHahx(5) = 3 +
     &    (S2B*AllHahx(3) + 16*CBA*SBA*AllHahx(4))/
     &     ((MHH2 - MHp2)*S2B**2)

        AllHahx(51) = CBA2*MHp2**3*S2B**2*SBA2 -
     &    MHH2**3*S2B*AllHahx(45) + MHH2*MHp2*AllHahx(47) +
     &    MHH2**2*AllHahx(50)

        AllHahx(55) = (MHH2**2*AllHahl(22)*AllHahx(53))/
     &     (Mh02*(MHH2 - MHp2)) -
     &    (Mh02**2*AllHahl(23)*AllHahx(54))/(MHH2*(Mh02 - MHp2))

        AllHahx(58) = CBA2*Mh02*(Mh02 - MHp2)*(MHH2 - MHp2) -
     &    SBA2*AllHahx(57)

	AllHahx(59) = 2*m12sq + (2*Mh02 + MHH2)*S2A

        AllHahx(60) = m12sq*S2A +
     &    (MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHahx(67) = Mh02 + MHH2 +
     &    (2*AllHahx(61))/((Mh02 - MHp2)*(MHH2 - MHp2)) +
     &    AllHahx(66)/((Mh02 - MHp2)**2*(MHH2 - MHp2)**2)

        AllHahx(72) = -((Mh02 + MHH2)*(1 - AllHahx(68))) -
     &    ((Mh02 - MHH2)**2*(Mh02 + MHH2 - 2*MHp2)*AllHahx(71))/
     &     (Mh02*MHH2*(Mh02 - MHp2)*(MHH2 - MHp2)*S2B**2)

        AllHahx(77) = -2*CB2*AllHahx(74) +
     &    SBA*(Mh02*S2B**2 +
     &       (SBA2 + (CBA2*AllHahx(75))/MHH2)*AllHahx(76)) +
     &    2*(CAB*(2*m12sq - Mh02*S2B) +
     &       (MW2*SW2)/(Alfa*Pi)*(CBA*Lambda7*SB2))

        AllHahx(8) = -2*SBA*
     &     (CAB*CBA2*Mh02*S2B + CBA*AllHahx(4)) +
     &    S2B*SBA2*AllHahx(7)

        AllHahx(817) =
     &   8*MHp2**5*S2B*(3 - SBA2) + MHH2**5*AllHahx(809) +
     &    2*AllHahx(813) - 2*AllHahx(816)

        AllHahx(82) = CBA*(MHH2 - 2*MHp2)*S2B**2 +
     &    2*((2*m12sq - MHH2*S2B)*SAB +
     &       (MW2*SW2)/(Alfa*Pi)*((CB2*Lambda6 - Lambda7*SB2)*SBA))

        dup4 = -(AllHahx(79)*AllHahx(165)) +
     &    3*SBA*AllHahx(76)*AllHahx(168)

        AllHahx(857) =
     &   (-2*CBA*dup4*MHH2*AllHahl(26))/
     &     ((Mh02 - MHH2)*(MHH2 - MHp2)) +
     &    (CBA2*MHp2*AllHahl(31)*(S2B*AllHahx(59) - 6*AllHahx(60))*
     &       AllHahx(856))/((Mh02 - MHp2)*(MHH2 - MHp2)**2)

        AllHahx(946) =
     &   384*S2B**2*AllHahx(939) -
     &    64*MHH2**2*MHp2**4*S2B*AllHahx(940) +
     &    MHH2**6*AllHahx(941) - MHH2**5*MHp2*AllHahx(942) +
     &    8*AllHahx(945)

        AllHahx(998) =
     &   -8*SAB**2*AllHahx(956) + 2*CBA*S2B*AllHahx(986) +
     &    4*SAB*AllHahx(997) +
     &    (MW2**2*SW2**2)/(Alfa2*Pi**2)*AllHahx(969)

        AllHahx(999) =
     &   CAB*(2*m12sq - Mh02*S2B) -
     &    (MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2)) +
     &    1/2.D0*((Mh02 - 2*MHp2)*S2B**2*SBA)

        AllHahx(1614) =
     &   1 + 2*((MHp2*AllHahl(3))/((Mh02 - MHp2)*(MHH2 - MHp2)) +
     &       AllHahx(1613)/(Mh02 - MHH2))

        AllHahx(36) = (Mh02*SBA2*AllHahl(23)*AllHahx(23))/
     &     (Mh02 - MHp2)**2 -
     &    (2*SBA*(AllHahl(35)*AllHahx(24) -
     &         3*AllHahl(27)*AllHahx(25)))/S2B**2 +
     &    AllHahx(35)/Mh02**2

        AllHahx(52) = -((CBA2*MHp2*SBA2*AllHahl(29)*
     &         AllHahx(37))/(MHH2 - MHp2)**2) -
     &    (CBA*AllHahl(34)*AllHahx(43) -
     &       2*AllHahl(32)*AllHahx(51))/S2B**2

        AllHahx(6) = (CBA2*MHH2*SBA2*AllHahl(22)*AllHahx(1))/
     &     (MHH2 - MHp2)**2 + AllHahl(3)**2*AllHahx(5)

        AllHahx(73) = -(Mh02*(MHH2 - MHp2)*SBA2*
     &       (1 + (2*AllHahl(1))/Mh02)**2) +
     &    4*((1 + 3*SBA2)*AllHahl(3)**2 +
     &       (SBA2*AllHahx(55))/(Mh02 - MHH2) +
     &       (AllHahl(25)*AllHahx(58)*
     &          (S2B*AllHahx(59) - 6*AllHahx(60)))/
     &        (Mh02*(Mh02 - MHp2)*(MHH2 - MHp2)*S2B**2) +
     &       AllHahl(24)*(AllHahx(67) + SBA2*AllHahx(72)))

	AllHahx(9) = 3*CBA2**2 + (8*AllHahx(8))/((Mh02 - MHp2)*S2B**2)

        AllHahx(1290) =
     &   2*(Mh02 - MHH2)*SBA*AllHahx(79) -
     &    CBA*(4*Mh02 - MHH2)*AllHahx(1268)

        AllHahx(1570) =
     &   SBA2*AllHahx(1283) -
     &    SBA*(6*CAB*(2*CBA2*m12sq - Mh02*S2B) -
     &       (6*MW2*SW2)/(Alfa*Pi)*
     &        (CBA*(CB2*Lambda6 - Lambda7*SB2)*(CBA2 - 2*SBA2)))

	dup5 = SBA2*AllHahx(76)**2 + AllHahx(79)**2

        AllHahx(1582) =
     &   -4*dup5*MHH2**2 +
     &    (Mh02 - 6*MHH2)*(Mh02 - 2*MHH2)*SBA2*AllHahx(76)**2

        AllHahx(1601) =
     &   dup5*Mh02*(Mh02 - 6*MHH2) +
     &    2*SBA*AllHahx(76)*AllHahx(79)*AllHahx(1600)

        AllHahx(834) =
     &   2*m12sq*S2B**2*SAB +
     &    (MW2*SW2)/(Alfa*Pi)*(SB2*SBA*AllHahx(171))

        AllHahx(1269) =
     &   4*(Mh02 - MHH2)*SBA*AllHahx(79) -
     &    CBA*(4*Mh02 - MHH2)*AllHahx(1268)

        AllHahx(1291) =
     &   (4*Mh02 - MHH2)*AllHahx(79)**2 +
     &    (S2B*AllHahx(59) - 6*AllHahx(60))*AllHahx(1290)

	dup6 = AllHahx(79)**2 + SBA2*AllHahx(927)**2

        AllHahx(1328) =
     &   dup6*MHH2 + (Mh02 - 2*MHH2)*SBA*AllHahx(76)*AllHahx(79)

        AllHahx(1408) =
     &   MHH2**2*S2B**2*AllHahx(1406) -
     &    (4*MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     ((7 - 36*SBA2 - 387*SBA2**2)*AllHahx(175)) -
     &    (12*MW2*SW2)/(Alfa*Pi)*
     &     (MHH2*S2B*(CB2*Lambda6 - Lambda7*SB2)*SBA*AllHahx(1407))

        AllHahx(1559) =
     &   SBA2*AllHahx(1253) -
     &    SBA*(2*CAB*(2*m12sq - Mh02*S2B) -
     &       (2*MW2*SW2)/(Alfa*Pi)*
     &        (CBA*(CB2*Lambda6 - Lambda7*SB2)*(1 - 3*SBA2)))

        AllHahx(1565) =
     &   AllHahx(165)*(SBA2*AllHahx(76) - CBA*AllHahx(165)) +
     &    3*SBA2*AllHahx(76)*AllHahx(168)

	AllHahx(1602) = 2*dup5*(Mh02 - 2*MHH2)*MHH2 + AllHahx(1601)

        AllHahx(586) =
     &   MHp2*S2B*(S2B - 2*CAB*SBA + 3*S2B*SBA2) -
     &    CBA*SBA*AllHahx(4)

        AllHahx(595) =
     &   MHp2*S2B*(S2B - 4*CBA*SAB + S2B*SBA2) + 4*CBA*AllHahx(46)

        AllHahx(832) =
     &   m12sq**2*S2B**2*SAB**2 +
     &    (MW2*SW2)/(Alfa*Pi)*(m12sq*SAB*SB2*SBA*AllHahx(171))

        AllHahx(835) =
     &   4*CBA*MHp2*AllHahx(834) +
     &    (MW2**2*SW2**2)/(Alfa2*Pi**2)*(dup3*SBA2)

	AllHahx(844) = 2*MHH2*S2B*SAB*SBA2 - SBA*(3 - SBA2)*AllHahx(4)

        AllHahx(1116) =
     &   9*CBA*Mh02*S2A*S2B*SB2*SBA*AllHahx(1103) +
     &    (2*MW2*SW2)/(Alfa*Pi)*((Mh02 - MHp2)**2*AllHahx(174))

        AllHahx(1132) =
     &   Mh02**3*(27*S2A - 4*S2B)*S2B*SBA +
     &    8*MHp2**2*AllHahx(904) - 2*Mh02**2*AllHahx(1130) +
     &    4*Mh02*MHp2*AllHahx(1131)

        AllHahx(1141) =
     &   -9*CBA*Mh02*(Mh02 - 4*MHp2)*MHp2*S2A*S2B*SB2*SBA +
     &    (4*MW2*SW2)/(Alfa*Pi)*((Mh02 - MHp2)**2*AllHahx(174))

        AllHahx(1146) =
     &   8*Mh02**3*S2B**2*SBA - 16*MHp2**2*AllHahx(904) +
     &    Mh02**2*AllHahx(1144) - 4*Mh02*MHp2*AllHahx(1145)

        AllHahx(1155) =
     &   -(S2B**2*AllHahx(1154)) +
     &    (32*MW2**2*SW2**2)/(Alfa2*Pi**2)*dup3

        AllHahx(1266) =
     &   -2*(4*Mh02 - MHH2)*MHH2*AllHahx(79) +
     &    (3*MW2*SW2)/(Alfa*Pi)*(CBA*Lambda7*SB2*AllHahx(1244))

        AllHahx(1270) =
     &   -((4*Mh02 - MHH2)*
     &       (3*AllHahx(25)*AllHahx(79) - AllHahx(79)**2)) +
     &    (S2B*AllHahx(59) - 6*AllHahx(60))*AllHahx(1269)

        AllHahx(1279) =
     &   4*(Mh02 - MHH2)*AllHahx(25)**2 + AllHahx(25)*AllHahx(1278)

        AllHahx(1292) =
     &   2*(4*Mh02 - MHH2)*AllHahx(79)*AllHahx(1289) +
     &    (2*Mh02 - MHH2)*AllHahx(1291)

        AllHahx(1298) =
     &   5*CBA2*Mh02*(Mh02 - 4*MHH2)*
     &     (S2B*AllHahx(59) - 6*AllHahx(60)) -
     &    2*(4*Mh02 - MHH2)*MHH2*SBA2*AllHahx(76)

        AllHahx(1320) =
     &   2*dup5*(4*Mh02 - MHH2)**2*MHH2**2 +
     &    CBA2*Mh02**2*(Mh02 - 4*MHH2)**2*
     &     (S2B*AllHahx(59) - 6*AllHahx(60))**2

	dup7 = -(SBA2*AllHahx(1253)) - 2*SBA*AllHahx(1254)

        AllHahx(1322) =
     &   (Mh02 - 2*MHH2)*(Mh02 - MHH2)*SBA*SBA2*AllHahx(76)*
     &     AllHahx(79) - CBA*dup7*Mh02*(Mh02 - 4*MHH2)*AllHahx(165)

        AllHahx(1324) =
     &   6*MHH2*SBA*AllHahx(25) + (MHH2 - Mh02*SBA2)*AllHahx(1003)

        AllHahx(1326) =
     &   2*CBA2*MHH2*(S2B*AllHahx(59) - 6*AllHahx(60))*
     &     AllHahx(76) + (MHH2 - Mh02*SBA2)*AllHahx(76)**2

        AllHahx(1389) =
     &   -2*MHH2**2*S2A*S2B**2*(16*S2A + S2B) +
     &    (3*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*MHH2*S2B*(29*S2A + 2*S2B)*
     &       (CB2*Lambda6 - Lambda7*SB2)*SBA) +
     &    (27*MW2**2*SW2**2)/(Alfa2*Pi**2)*(SBA2**2*AllHahx(175))

        AllHahx(1392) =
     &   MHH2**2*S2B**2*AllHahx(1390) +
     &    (MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     ((125 - 72*SBA2 - 126*SBA2**2)*AllHahx(175)) +
     &    (3*MW2*SW2)/(Alfa*Pi)*
     &     (MHH2*S2B*(CB2*Lambda6 - Lambda7*SB2)*SBA*AllHahx(1391))

        AllHahx(1395) =
     &   MHH2**2*S2B**2*AllHahx(1393) -
     &    (3*MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     ((7 - 18*SBA2 + 45*SBA2**2)*AllHahx(175)) +
     &    (6*MW2*SW2)/(Alfa*Pi)*
     &     (MHH2*S2B*(CB2*Lambda6 - Lambda7*SB2)*SBA*AllHahx(1394))

        AllHahx(1398) =
     &   MHH2**2*S2B**2*AllHahx(1396) -
     &    (27*MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     ((91 - 96*SBA2 + 768*SBA2**2)*AllHahx(175)) -
     &    (6*MW2*SW2)/(Alfa*Pi)*
     &     (MHH2*S2B*(CB2*Lambda6 - Lambda7*SB2)*SBA*AllHahx(1397))

        AllHahx(1401) =
     &   2*MHH2**2*S2B**2*AllHahx(1399) +
     &    (4*MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     ((85 + 18*SBA2 + 2898*SBA2**2)*AllHahx(175)) +
     &    (3*MW2*SW2)/(Alfa*Pi)*
     &     (MHH2*S2B*(CB2*Lambda6 - Lambda7*SB2)*SBA*AllHahx(1400))

        AllHahx(1404) =
     &   -3*MHH2**2*S2B**2*AllHahx(1402) +
     &    (MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     ((541 - 1062*SBA2 + 3168*SBA2**2)*AllHahx(175)) +
     &    (6*MW2*SW2)/(Alfa*Pi)*
     &     (MHH2*S2B*(CB2*Lambda6 - Lambda7*SB2)*SBA*AllHahx(1403))

        AllHahx(1418) =
     &   80*Mh02**7*S2A**2*S2B**2 +
     &    m12sq**2*(3*S2A - S2B)**2*AllHahx(1405) -
     &    Mh02**5*AllHahx(1408) - m12sq*AllHahx(619)*AllHahx(1417)

        AllHahx(1453) =
     &   MHH2**2*S2B**2*AllHahx(1450) +
     &    (192*MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (SBA2*(1 + 10*SBA2 + 36*SBA2**2)*AllHahx(175)) -
     &    (32*MW2*SW2)/(Alfa*Pi)*
     &     (MHH2*S2B*(CB2*Lambda6 - Lambda7*SB2)*SBA*AllHahx(1452))

        AllHahx(1500) =
     &   MHH2**2*S2B**2*(72*SAB**2 - SBA2*AllHahx(1496)) -
     &    4*MHH2*S2B*AllHahx(1499) +
     &    (6*MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (SBA2*(24 - 61*SBA2 + 183*SBA2**2)*AllHahx(175))

        AllHahx(333) =
     &   S2B*AllHahx(59) -
     &    (6*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHahx(408) =
     &   -2*CAB*(2*m12sq - Mh02*S2B)*SBA + SBA2*AllHahx(407)

        AllHahx(500) =
     &   MHp2*S2B*(S2B - 2*CAB*SBA + 3*S2B*SBA2) +
     &    2*SBA*AllHahx(227)

	AllHahx(564) = MHp2*S2B**2*(1 + SBA2) + CBA*SBA*AllHahx(4)

        AllHahx(587) =
     &   MHH2**2*MHp2*(3*CBA*SBA*AllHahx(4) +
     &       MHp2*S2B*AllHahx(585)) + MHp2**3*AllHahx(586)

        AllHahx(598) =
     &   -(MHH2**3*S2B*SBA2*(S2B - 8*CBA*SAB + S2B*SBA2)) -
     &    4*C2B*CBA*m12sq*SBA*AllHahx(593) -
     &    MHp2**2*AllHahx(594) + MHH2*MHp2*SBA2*AllHahx(595) +
     &    MHH2**2*AllHahx(597)

	AllHahx(678) = 2*MHp2*S2B*(1 + SBA2) + MHH2*AllHahx(622)

        AllHahx(831) =
     &   4*CBA2*(CBA*S2B - 2*SAB)*AllHahx(46) +
     &    MHp2*S2B**2*(8*CBA*CBA2*SAB - S2B*AllHahx(830))

        AllHahx(836) =
     &   2*MHp2**2*S2B**6 + 8*AllHahx(832) - S2B**2*AllHahx(835)

        AllHahx(845) =
     &   2*(1 - 3*SBA2)*SBA2*AllHahx(46) +
     &    (CBA*S2B*AllHahx(843) + 2*MHH2*AllHahx(844))/
     &     (MHH2 - MHp2)

        AllHahx(1004) =
     &   2*(Mh02 - MHp2)*SBA2*AllHahx(76) -
     &    CBA2*(MHH2 - MHp2)*AllHahx(1003)

        AllHahx(1025) =
     &   -2*CBA*(MHH2 - MHp2)*SBA*AllHahx(82) +
     &    (Mh02 - MHp2)*AllHahx(1024)

        AllHahx(1117) =
     &   9*CB2*CBA*Lambda6*Mh02*S2A*S2B*SBA*AllHahx(1103) -
     &    Lambda7*AllHahx(1116) +
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     (CB2**2*Lambda6**2*(Mh02 - MHp2)**2)

        AllHahx(1119) =
     &   -3*MHp2**2*S2B**4*SBA2 +
     &    (2*MW2**2*SW2**2)/(Alfa2*Pi**2)*dup3 -
     &    (5*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*MHp2*S2B**2*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHahx(1126) =
     &   -(S2B**2*AllHahx(1125)) +
     &    (4*MW2**2*SW2**2)/(Alfa2*Pi**2)*dup3

	dup8 = Mh02*S2B**2*SBA - 2*AllHahx(904)

        AllHahx(1133) =
     &   8*dup8*m12sq*(Mh02 - MHp2)**2 + Mh02*S2B*AllHahx(1132)

        AllHahx(1142) =
     &   -9*CB2*CBA*Lambda6*Mh02*(Mh02 - 4*MHp2)*MHp2*S2A*S2B*
     &     SBA - Lambda7*AllHahx(1141) +
     &    (8*MW2*SW2)/(Alfa*Pi)*
     &     (CB2**2*Lambda6**2*(Mh02 - MHp2)**2)

        AllHahx(1147) =
     &   -16*dup8*m12sq*(Mh02 - MHp2)**2 + Mh02*S2B*AllHahx(1146)

        AllHahx(1150) =
     &   -3*MHp2**2*SBA2*AllHahx(1149) +
     &    (16*MW2**2*SW2**2)/(Alfa2*Pi**2)*dup3 -
     &    (32*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*MHp2*S2B**2*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHahx(1151) =
     &   -6*MHp2**2*S2B**4*SBA2 +
     &    (13*MW2**2*SW2**2)/(Alfa2*Pi**2)*dup3 -
     &    (10*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*MHp2*S2B**2*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHahx(1152) =
     &   MHp2**2*S2B**4*SBA2 -
     &    (3*MW2**2*SW2**2)/(Alfa2*Pi**2)*dup3 +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*MHp2*S2B**2*(CB2*Lambda6 - Lambda7*SB2)*SBA)

	AllHahx(1156) = -2*Mh02**4*S2B**4*SBA2 + Mh02**2*AllHahx(1155)

        AllHahx(1215) =
     &   -2*(MHH2 - MHp2)*(1 + 3*SBA2)*AllHahx(82) +
     &    (MHH2 - 4*MHp2)*AllHahx(1168)

        AllHahx(1267) =
     &   -(Mh02*S2B**3*AllHahx(1242)) + CB2*AllHahx(1249) +
     &    S2B**2*(SBA2*AllHahx(1003)*AllHahx(1265) -
     &       SBA*AllHahx(1266))

        AllHahx(1271) =
     &   -2*(Mh02 - MHH2)*(S2B*AllHahx(59) - 6*AllHahx(60))*
     &     AllHahx(1250) +
     &    (4*Mh02 - MHH2)*MHH2*S2B**2*AllHahx(1270)

        AllHahx(1299) =
     &   -2*CBA*dup7*Mh02*(Mh02 - 4*MHH2)*(4*Mh02 - MHH2)*
     &     AllHahx(165) + (Mh02 - MHH2)*SBA*AllHahx(79)*
     &     AllHahx(1298)

        AllHahx(1310) =
     &   -((Mh02 - MHH2)*SBA2*AllHahx(76)) +
     &    CBA2*(Mh02 - 2*MHH2)*AllHahx(1003)

        AllHahx(1321) =
     &   CBA2*Mh02*(Mh02 - 4*MHH2)**2*(8*Mh02 - 3*MHH2)*
     &     (S2B*AllHahx(59) - 6*AllHahx(60))**2 -
     &    dup5*(4*Mh02 - MHH2)**2*AllHahx(1319) + 2*AllHahx(1320)

        AllHahx(1323) =
     &   (Mh02 - MHH2)*SBA*AllHahx(79)*AllHahx(1298) +
     &    2*(4*Mh02 - MHH2)*AllHahx(1322)

        AllHahx(1327) =
     &   CBA2*(Mh02 - 4*MHH2)**2*(4*Mh02 - MHH2)*
     &     (S2B*AllHahx(59) - 6*AllHahx(60))*AllHahx(1324) -
     &    (Mh02 - 4*MHH2)*(4*Mh02 - MHH2)**2*
     &     (AllHahx(79)**2*AllHahx(1325) + SBA2*AllHahx(1326))

        AllHahx(1343) =
     &   (2*(Mh02 - MHH2)*SBA*AllHahx(79))/(4*Mh02 - MHH2) +
     &    CBA*AllHahx(165)

        AllHahx(1419) =
     &   32*Mh02**6*AllHahx(1389) -
     &    2*Mh02**2*MHH2**4*AllHahx(1392) +
     &    Mh02**3*MHH2**3*AllHahx(1398) +
     &    2*Mh02**4*MHH2**2*AllHahx(1401) +
     &    Mh02*MHH2**5*AllHahx(1404) +
     &    4*(MHH2**6*AllHahx(1395) + MHH2*AllHahx(1418))

        AllHahx(1425) =
     &   MHH2**2*S2B**2*AllHahx(1422) +
     &    (8*MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (SBA2*(47 + 210*SBA2 + 1224*SBA2**2)*AllHahx(175)) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (MHH2*S2B*(CB2*Lambda6 - Lambda7*SB2)*SBA*AllHahx(1424))

        AllHahx(1429) =
     &   MHH2**2*S2B**2*AllHahx(1426) +
     &    (12*MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (SBA2*(33 - 1702*SBA2 + 2868*SBA2**2)*AllHahx(175)) +
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     (MHH2*S2B*(CB2*Lambda6 - Lambda7*SB2)*SBA*AllHahx(1428))

        AllHahx(1434) =
     &   MHH2**2*S2B**2*AllHahx(1431) +
     &    (3*MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (SBA2*(564 - 301*SBA2 + 3885*SBA2**2)*AllHahx(175)) +
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     (MHH2*S2B*(CB2*Lambda6 - Lambda7*SB2)*SBA*AllHahx(1433))

        AllHahx(1438) =
     &   -(MHH2**2*S2B**2*AllHahx(1435)) +
     &    (12*MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (SBA2*(835 + 2313*SBA2 + 4899*SBA2**2)*AllHahx(175)) -
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (MHH2*S2B*(CB2*Lambda6 - Lambda7*SB2)*SBA*AllHahx(1437))

        AllHahx(1443) =
     &   MHH2**2*S2B**2*AllHahx(1440) +
     &    (4*MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (SBA2*(7621 + 7038*SBA2 + 49140*SBA2**2)*AllHahx(175))-
     &      (4*MW2*SW2)/(Alfa*Pi)*
     &     (MHH2*S2B*(CB2*Lambda6 - Lambda7*SB2)*SBA*AllHahx(1442))

        AllHahx(1446) =
     &   -24*Mh02**10*S2A**2*S2B**2 + Mh02**9*S2B*AllHahx(1445) +
     &    (8*MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (MHH2**8*(1 - 3*SBA2)**2*AllHahx(175))

        AllHahx(1454) =
     &   8*Mh02**8*S2B*AllHahx(1449) + Mh02**7*AllHahx(1453)

        AllHahx(1501) =
     &   m12sq**2*AllHahx(1455)*AllHahx(1495) -
     &    MHH2**7*AllHahx(1500)

        AllHahx(151) =
     &   -(MHp2*(S2A + 22*CAB*SBA - 11*S2B*SBA2)) +
     &    MHH2*AllHahx(150)

        AllHahx(1515) =
     &   9*CBA2**4*AllHahx(626)**2*AllHahx(1512) +
     &    2*CBA2**3*(Mh02 - 4*MHH2)**2*(4*Mh02 - MHH2)*
     &     AllHahx(1514)

        AllHahx(1554) =
     &   2*(2*m12sq - Mh02*S2B)*AllHahx(294)*AllHahx(1527) +
     &    Mh02*AllHahx(1553)

        AllHahx(1723) =
     &   (Mh02**2 + 4*MHH2**2)*S2B**4 + AllHahx(1703)**2 +
     &    (18*MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (CBA2*SBA2*AllHahx(175)) -
     &    (12*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA*AllHahx(1703))

        AllHahx(1738) =
     &   AllHahx(315)**2 +
     &    (12*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA*
     &       (6*m12sq*S2A - S2B*AllHahx(59))) +
     &    (3*MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     ((1 + 6*CBA2*SBA2)*AllHahx(175))

        AllHahx(1816) =
     &   (CB2*Lambda6 - Lambda7*SB2)*
     &    ((Mh02 - 2*MHH2)*S2B**2 + 3*SBA2*AllHahx(1703))

        AllHahx(272) =
     &   5*MHp2*S2B**2 - MHH2*(S2A*S2B + 6*S2B**2)*SBA2 +
     &    2*SBA*AllHahx(229)

        AllHahx(316) =
     &   S2B**2*AllHahx(314) + (Mh02 - MHH2)*AllHahx(315)

        AllHahx(334) =
     &   S2B*AllHahx(332) -
     &    4*(Mh02 - MHH2)*(2*Mh02 - MHH2)*AllHahx(333)

        AllHahx(410) =
     &   11*MHp2*S2B**2*SBA2*(1 + SBA2) +
     &    4*(SBA2*AllHahx(408) - CBA*AllHahx(409))

        AllHahx(413) =
     &   MHp2*S2B*SBA2*AllHahx(411) +
     &    2*(SBA2*AllHahx(408) - CBA*AllHahx(412))

	AllHahx(501) = MHH2**4*S2B*AllHahx(499) + MHp2**3*AllHahx(500)

	AllHahx(506) = MHp2*S2B**2*(7 + 15*SBA2) + 2*SBA*AllHahx(227)

        AllHahx(522) =
     &   (Mh02 - MHH2)*(Mh02 - MHp2)*MHp2*S2B**2 -
     &    AllHahx(315)*AllHahx(521)

        AllHahx(565) =
     &   S2B*AllHahx(562) + MHH2*MHp2*AllHahx(563) -
     &    2*MHp2**2*AllHahx(564)

        AllHahx(569) =
     &   MHH2**2*S2B*AllHahx(566) +
     &    MHH2*(CBA*SBA*AllHahx(4) + MHp2*S2B*AllHahx(567)) -
     &    MHp2*(CBA*SBA*AllHahx(4) - MHp2*S2B*AllHahx(568))

        AllHahx(575) =
     &   -2*MHp2**5*S2B**2*(1 + 2*SBA2) -
     &    S2B*(-(MHH2**4*MHp2*AllHahx(570)) + AllHahx(572)) +
     &    2*MHH2*MHp2**3*(2*CBA*SBA*AllHahx(4) +
     &       MHp2*S2B*AllHahx(573)) -
     &    MHH2**3*MHp2*(4*CBA*SBA*AllHahx(4) -
     &       MHp2*S2B*AllHahx(574))

        AllHahx(581) =
     &   S2B*AllHahx(578) +
     &    MHp2**2*(4*CBA*SBA*AllHahx(4) + MHp2*S2B*AllHahx(579)) -
     &    MHH2**2*(4*CBA*SBA*AllHahx(4) - MHp2*S2B*AllHahx(580))

        AllHahx(588) =
     &   MHH2**4*S2B*AllHahx(582) -
     &    MHH2**3*(2*CBA*SBA*AllHahx(4) - MHp2*S2B*AllHahx(583)) +
     &    MHH2*MHp2**2*(6*CBA*SBA*AllHahx(4) -
     &       MHp2*S2B*AllHahx(584)) - 2*AllHahx(587)

        AllHahx(603) =
     &   MHH2*(MHH2 - MHp2)**2*MHp2*S2B**2*SBA2*(1 + SBA2) +
     &    Mh02*AllHahx(598) + Mh02**2*AllHahx(602)

        AllHahx(679) =
     &   -4*CBA*SBA*(3 + SBA2)*AllHahx(4) + S2B*SBA2*AllHahx(678)

        AllHahx(687) =
     &   -2*m12sq*AllHahx(305) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA*(1 - 6*SBA2))

        AllHahx(837) =
     &   MHH2**2*S2B**4*AllHahx(829) +
     &    2*MHH2*S2B**3*AllHahx(831) + 4*CBA2*AllHahx(836)

        AllHahx(846) =
     &   -2*CBA2*MHp2*S2B**2*(1 + SBA2) + MHH2*S2B*AllHahx(839) +
     &    CBA*AllHahx(845)

        AllHahx(1002) =
     &   (2*MHH2*SBA*AllHahx(24))/((Mh02 - MHH2)*(MHH2 - MHp2)) +
     &    (CBA*AllHahx(82)*AllHahx(1001))/(Mh02 - MHp2)**2

        AllHahx(1005) =
     &   2*MHH2*(Mh02 - MHp2)*SBA*AllHahx(24) +
     &    (MHH2 - 2*MHp2)*AllHahx(1004)

        AllHahx(1016) =
     &   (MHH2 - 4*MHp2)*(Mh02 - MHp2)*(1 + SBA2)*AllHahx(24)**2 +
     &    CBA2*(Mh02 - 4*MHp2)*(MHH2 - MHp2)*AllHahx(82)**2

        AllHahx(1034) =
     &   CBA2*(Mh02 - 2*MHp2)*(S2B*AllHahx(59) - 6*AllHahx(60)) +
     &    CBA*Mh02*AllHahx(82)

        AllHahx(1054) =
     &   (9*Mh02**2*AllHahx(25))/(Mh02 - MHH2) +
     &    (MHp2*AllHahx(856)*(Mh02 + 1/2.D0*MHp2))/(MHH2 - MHp2)**2

        AllHahx(1064) =
     &   (Mh02 - MHp2)*SBA2*AllHahx(76) -
     &    CBA2*(MHH2 - MHp2)*AllHahx(1003)

        AllHahx(1073) =
     &   (dup4*MHH2**2)/(Mh02 - MHH2) +
     &    (MHp2**2*AllHahx(24)*AllHahx(82))/(Mh02 - MHp2)

        AllHahx(108) =
     &   CBA2*(Mh02 - MHp2)*AllHahx(89) +
     &    (MHH2 - MHp2)*AllHahx(107)

        AllHahx(1118) =
     &   CAB**2*AllHahx(1115) +
     &    (MW2*SW2)/(Alfa*Pi)*(CBA2**2*AllHahx(1117))

        AllHahx(1127) =
     &   -16*(MHp2**2*AllHahx(887) + Mh02*MHp2*AllHahx(1119)) -
     &    S2B**2*AllHahx(1123) + 4*Mh02**2*AllHahx(1126)

        AllHahx(1134) =
     &   -8*dup8*(Mh02 - MHp2)**2*(2*m12sq - Mh02*S2B) +
     &    18*AllHahx(1103)*AllHahx(1129) - CBA2*AllHahx(1133)

        AllHahx(1143) =
     &   CAB**2*AllHahx(1140) -
     &    (MW2*SW2)/(Alfa*Pi)*(CBA2**2*MHp2*AllHahx(1142))

        AllHahx(1148) =
     &   8*dup8*(Mh02 - 6*MHp2)*(Mh02 - MHp2)**2*
     &     (2*m12sq - Mh02*S2B) -
     &    18*(Mh02 - 4*MHp2)*MHp2**2*AllHahx(1129) +
     &    CBA2*MHp2*AllHahx(1147)

        AllHahx(1157) =
     &   Mh02**3*AllHahx(1150) + 16*Mh02*MHp2**2*AllHahx(1151) +
     &    32*MHp2**3*AllHahx(1152) - 4*MHp2*AllHahx(1156)

        AllHahx(1167) =
     &   AllHahx(870) - 2*(MHH2 - 4*MHp2)*(MHH2 - MHp2)*
     &     AllHahx(1166) +
     &    4*(MHH2 - MHp2)**2*MHp2**2*(Sqrt(CW2) + SW)**2*
     &     (1 - 2*Sqrt(CW2)*SW)

        AllHahx(1172) =
     &   MHH2*(MHH2 - 4*MHp2)**2*MHp2*AllHahx(1168) +
     &    (MHH2 - MHp2)*AllHahx(82)*AllHahx(1171)

        AllHahx(1174) =
     &   MHH2*(1 + SBA2)*AllHahx(82)**2 + MHp2*AllHahx(165)**2

        AllHahx(118) =
     &   -(((Mh02 - MHp2)*SBA2*AllHahx(71))/
     &       (Mh02*(MHH2 - MHp2))) + CBA2*AllHahx(116) +
     &    2*CBA*AllHahx(117)

        AllHahx(1216) =
     &   MHH2*(MHH2 - 4*MHp2)*AllHahx(1215) +
     &    4*(MHH2 - 2*MHp2)*(MHH2 - MHp2)**2*(Sqrt(CW2) + SW)**2*
     &     AllHahx(82)*(1 - 2*Sqrt(CW2)*SW)

        AllHahx(1222) =
     &   (MHH2 - MHp2)*AllHahx(82)*AllHahx(1163) +
     &    MHH2*(MHH2 - 4*MHp2)**2*MHp2*AllHahx(1168)

        AllHahx(1223) =
     &   -(MHH2*(MHH2 - 4*MHp2)**2*AllHahx(876)) +
     &    2*AllHahx(82)*AllHahx(1167)

        AllHahx(1255) =
     &   (-5*CBA2*(Mh02 - 4*MHH2)*
     &       (S2B*AllHahx(59) - 6*AllHahx(60)))/(4*Mh02 - MHH2) +
     &    (2*MHH2*SBA2*AllHahx(76))/Mh02

        AllHahx(1261) =
     &   6*SBA*AllHahx(25) +
     &    ((MHH2 - Mh02*SBA2)*AllHahx(1003))/MHH2

        AllHahx(1262) =
     &   (2*dup5*MHH2**2)/(Mh02 - 4*MHH2)**2 +
     &    (CBA2*Mh02**2*(S2B*AllHahx(59) - 6*AllHahx(60))**2)/
     &     (4*Mh02 - MHH2)**2

        AllHahx(1272) =
     &   -4*(Mh02 - MHH2)*(S2B*AllHahx(59) - 6*AllHahx(60))*
     &     AllHahx(1267) + AllHahx(1271)

        AllHahx(1280) =
     &   3*(2*Mh02 - MHH2)*SBA*AllHahx(25) +
     &    CBA2*Mh02*(S2B*AllHahx(59) - 6*AllHahx(60))

        AllHahx(1285) =
     &   AllHahx(79)**2 + (S2B*AllHahx(59) - 6*AllHahx(60))*
     &     (SBA2*AllHahx(1283) + 6*SBA*AllHahx(1284))

        AllHahx(1305) =
     &   -12*CBA*Mh02*(MHH2*S2B*SAB - SBA2*AllHahx(46)) +
     &    CBA2*AllHahx(1304)

        AllHahx(1306) =
     &   2*dup6*MHH2**2 + (Mh02 - 2*MHH2)**2*SBA*AllHahx(76)*
     &     AllHahx(79)

        AllHahx(1307) =
     &   Mh02*(Mh02 - 4*MHH2)**2*AllHahx(76)*AllHahx(1305) +
     &    2*(Mh02 - MHH2)**2*AllHahx(1306)

        AllHahx(1309) =
     &   4*MHH2*(S2B*AllHahx(59) - 6*AllHahx(60))*AllHahx(76) +
     &    (Mh02 - 2*MHH2)*AllHahx(927)**2

        AllHahx(1311) =
     &   (Mh02 - MHH2)*SBA2**2*AllHahx(76)**2 +
     &    CBA*dup7*Mh02*AllHahx(165) +
     &    SBA*AllHahx(79)*AllHahx(1310)

        AllHahx(1315) =
     &   -(CBA2*SBA*(S2B*AllHahx(59) - 6*AllHahx(60))*
     &       AllHahx(79)) + CBA*SBA2*AllHahx(76)*AllHahx(165)

        AllHahx(1336) =
     &   4*m12sq*SAB - MHH2*S2B*(CBA*S2B + 2*SAB) +
     &    3*AllHahx(168) +
     &    (2*MW2*SW2)/(Alfa*Pi)*((CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHahx(1338) =
     &   CBA2*(S2B*AllHahx(59) - 6*AllHahx(60)) + 2*SBA*AllHahx(79)

        AllHahx(1344) =
     &   AllHahx(79)**2 + (S2B*AllHahx(59) - 6*AllHahx(60))*
     &     AllHahx(1343)

        AllHahx(1420) =
     &   (Mh02 - MHH2)*(4*Mh02 - MHH2)**2*MHH2*SBA2*
     &     (S2B*AllHahx(1366) + 4*AllHahx(1388)) +
     &    CBA2**2*Mh02*(Mh02 - 4*MHH2)*AllHahx(1419)

        AllHahx(1502) =
     &   4*Mh02**6*MHH2**2*AllHahx(1425) -
     &    Mh02**5*MHH2**3*AllHahx(1429) -
     &    8*Mh02**2*MHH2**6*AllHahx(1434) -
     &    2*Mh02**4*MHH2**4*AllHahx(1438) +
     &    Mh02**3*MHH2**5*AllHahx(1443) - 16*SBA2*AllHahx(1446) -
     &    MHH2*AllHahx(1454) +
     &    8*Mh02*(m12sq*AllHahx(1455)*AllHahx(1484) -
     &       2*AllHahx(1501))

        AllHahx(152) =
     &   MHH2**4*AllHahx(148) - MHH2**2*MHp2**2*AllHahx(149) -
     &    Mh02*AllHahx(133)*AllHahx(151)

        AllHahx(1555) =
     &   4*CAB**2*(Mh02 - MHH2)*(4*Mh02 - MHH2)**2*AllHahx(1511) -
     &    2*CAB*(4*Mh02 - MHH2)*AllHahx(1554) +
     &    (3*MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (Mh02*AllHahx(175)*AllHahx(1515))

        AllHahx(1681) =
     &   -(m12sq*SBA2*AllHahx(1655)) +
     &    (MW2*SW2)/(Alfa*Pi)*(CB2*CBA*Lambda6*MHH2*SBA)

        AllHahx(1718) =
     &   AllHahx(1703)**2 +
     &    (3*MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     ((1 + 6*CBA2*SBA2)*AllHahx(175)) -
     &    (12*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA*AllHahx(1703))

        AllHahx(1719) =
     &   (2*m12sq - MHH2*S2B)*SAB*
     &     (2*m12sq*SAB + S2B*AllHahx(1639)) +
     &    CAB*(2*m12sq - Mh02*S2B)*AllHahx(1696) +
     &    SAB**2*AllHahx(1699) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*AllHahx(1640))

        AllHahx(1724) =
     &   4*CAB*(2*m12sq - Mh02*S2B)*AllHahx(1696) -
     &    4*S2B**2*AllHahx(1697) + SBA2*AllHahx(1723) +
     &    (2*MW2**2*SW2**2)/(Alfa2*Pi**2)*(CBA2*AllHahx(175))

        AllHahx(1734) =
     &   (Mh02 - 2*MHH2)**2*S2B**4 + AllHahx(1703)**2 +
     &    (18*MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (CBA2*SBA2*AllHahx(175)) -
     &    (12*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA*AllHahx(1703))

        AllHahx(1739) =
     &   -((Mh02 - 2*MHp2)**2*S2B**4*SBA2) -
     &    4*CAB*(2*m12sq - Mh02*S2B)*AllHahx(1636) -
     &    CBA2*AllHahx(1738) +
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*AllHahx(1637))

        AllHahx(1753) =
     &   S2B**4*AllHahx(1741) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*AllHahx(1733)) +
     &    AllHahx(1638)*Re(AllHahl(13)) +
     &    AllHahx(1642)*Re(AllHahl(14))

        AllHahx(1758) =
     &   2*CBA2*(AllHahx(1731) + 2*AllHahx(1744)) +
     &    3*Mh02**2*SBA2*(1 + 2*Re(AllHahl(15)))

        AllHahx(1817) =
     &   2*CAB*(2*m12sq - Mh02*S2B)*(CB2*Lambda6 - Lambda7*SB2) +
     &    SBA*AllHahx(1816)

        AllHahx(184) =
     &   8*SAB**2*AllHahx(173) - 2*MHH2*AllHahx(183) +
     &    (MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (SBA2*(7 - 3*(4 - 3*SBA2)*SBA2)*AllHahx(175))

        AllHahx(1840) =
     &   24*m12sq**2*(3*S2A + S2B)*SAB*SBA2 +
     &    CBA*S2B**4*AllHahx(1615)

        AllHahx(1843) =
     &   CAB*(2*m12sq - Mh02*S2B)*AllHahx(1696) +
     &    SAB**2*AllHahx(1699)

        AllHahx(1857) =
     &   MHH2**2*S2B**4 + AllHahx(315)**2 +
     &    (12*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA*
     &       (6*m12sq*S2A - S2B*AllHahx(59))) +
     &    (18*MW2**2*SW2**2)/(Alfa2*Pi**2)*(CBA2*SBA2*AllHahx(175))

        AllHahx(1858) =
     &   CBA2*(MHH2 - 2*MHp2)**2*S2B**4 + 4*AllHahx(1641) +
     &    SBA2*AllHahx(1718)

        AllHahx(215) =
     &   8*SAB**2*AllHahx(206) - 2*MHH2*AllHahx(214) +
     &    (MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (SBA2*(29 + 12*SBA2 + 99*SBA2**2)*AllHahx(175))

	AllHahx(259) = 2*Mh02*S2B**2 - 3*SBA*AllHahx(227)

        AllHahx(273) =
     &   MHp2**3*(MHH2 + MHp2)*S2B**2*SBA2 +
     &    Mh02**2*MHp2*AllHahx(271) + Mh02*MHp2**2*AllHahx(272)

        AllHahx(276) =
     &   8*CBA*SBA*AllHahx(4) +
     &    S2B*SBA2*(MHH2*AllHahx(250) + MHp2*AllHahx(251))

        AllHahx(318) =
     &   Mh02*AllHahx(316) - 6*(Mh02 - MHH2)*SBA*AllHahx(317)

        AllHahx(335) =
     &   (4*Mh02 - MHH2)*MHH2*S2B**2*AllHahx(330) -
     &    2*Mh02*AllHahx(334)

	AllHahx(360) = MHp2*S2B**2*(2 - 3*SBA2) - SBA*AllHahx(227)

	AllHahx(361) = MHp2*S2B**2*(1 - 3*SBA2) - 2*SBA*AllHahx(227)

	AllHahx(366) = MHp2*S2B**2*(1 + 7*SBA2) + 2*SBA*AllHahx(227)

        AllHahx(414) =
     &   8*C2B*CBA*m12sq*SBA*(MHH2 - 2*MHp2 + MHH2*SBA2) +
     &    MHH2**2*S2B*SBA2*AllHahx(406) - MHp2*AllHahx(410) +
     &    2*MHH2*AllHahx(413)

        AllHahx(492) =
     &   MHH2**3*S2B*AllHahx(486) + MHp2**2*AllHahx(488) +
     &    MHH2*MHp2*(2*CBA*SBA*AllHahx(4) +
     &       MHp2*S2B*AllHahx(489)) + MHH2**2*AllHahx(491)

        AllHahx(502) =
     &   2*MHH2**2*MHp2*AllHahx(494) - MHH2**3*AllHahx(496) +
     &    MHH2*MHp2**2*AllHahx(498) - 2*AllHahx(501)

        AllHahx(507) =
     &   -2*MHp2**4*S2B**2*(1 + 2*SBA2) - S2B*AllHahx(504) -
     &    MHH2**2*MHp2*(4*SBA*AllHahx(227) +
     &       MHp2*S2B*AllHahx(505)) + 2*MHH2*MHp2**2*AllHahx(506)

        AllHahx(514) =
     &   S2B*AllHahx(509) +
     &    MHH2*MHp2**2*(2*CBA*SBA*AllHahx(4) +
     &       MHp2*S2B*AllHahx(510)) +
     &    2*MHp2**3*(4*SBA*AllHahx(227) - MHp2*S2B*AllHahx(511)) -
     &    MHH2**2*MHp2*AllHahx(513)

        AllHahx(519) =
     &   S2B*(2*CAB*Mh02 + SBA*AllHahx(220)) -
     &    CBA*(4*C2B*m12sq -
     &       (2*MW2*SW2)/(Alfa*Pi)*(CB2*Lambda6 - Lambda7*SB2))

        AllHahx(523) =
     &   Mh02*(Mh02 - MHp2)*(2*Mh02 - MHH2 - MHp2)*S2B**2 +
     &    SBA2*AllHahx(522)

        AllHahx(535) =
     &   CAB*CBA*Mh02*(Mh02 - MHH2)*S2B**2 -
     &    S2B*AllHahx(453)*AllHahx(530) + CB2*AllHahx(534)

        AllHahx(589) =
     &   Mh02**5*S2B*AllHahx(560) + MHH2**2*MHp2*AllHahx(565) +
     &    2*Mh02**4*AllHahx(569) + Mh02*AllHahx(575) +
     &    Mh02**3*AllHahx(581) - Mh02**2*AllHahx(588)

        AllHahx(604) =
     &   CBA2**2*Mh02**2*S2B**2*AllHahx(592) -
     &    (Mh02 - MHH2)*(Mh02 - MHp2)*AllHahx(603)

	AllHahx(680) = dup1*S2B*SBA2 - Mh02*AllHahx(679)

        AllHahx(698) =
     &   8*MHH2**5*S2B*(2*S2A + S2B)*SBA2 -
     &    Mh02**2*MHH2**2*AllHahx(683) +
     &    Mh02**3*MHH2*AllHahx(686) + 16*MHH2**4*AllHahx(687) +
     &    2*AllHahx(697)

        AllHahx(847) =
     &   -(AllHahx(837)/(MHH2 - 4*MHp2)) + S2B**4*AllHahx(846)

        AllHahx(901) =
     &   3*CBA*(Mh02 - 4*MHp2)*S2B*SB2*SBA*AllHahx(899) +
     &    (8*MW2*SW2)/(Alfa*Pi)*(AllHahx(174)*AllHahx(900))

        AllHahx(911) =
     &   -5*Mh02**3*(15*S2A - 2*S2B)*S2B*SBA +
     &    112*MHp2**2*AllHahx(904) + 4*Mh02*MHp2*AllHahx(909) +
     &    Mh02**2*AllHahx(910)

        AllHahx(913) =
     &   Mh02**2*S2B*(3*S2A + 8*S2B)*SBA - 32*MHp2*AllHahx(904) -
     &    4*Mh02*AllHahx(912)

        AllHahx(923) =
     &   (Mh02 - 2*MHH2)*(Mh02 - MHp2)*SBA*AllHahx(24) -
     &    2*CBA*Mh02*(MHH2 - MHp2)*AllHahx(82)

        AllHahx(932) =
     &   (Mh02 - MHp2)*(Mh02 - MHH2 - MHp2)*SBA*AllHahx(24) -
     &    CBA*(MHH2 - MHp2)*(Mh02 - MHH2 + MHp2)*AllHahx(82)

        AllHahx(933) =
     &   -((Mh02 + MHH2 - MHp2)*SBA*AllHahx(24)) +
     &    (Mh02 - MHH2 + MHp2)*SBA2*AllHahx(76)

	AllHahx(935) = SBA2**2*AllHahx(76)**2 + AllHahx(79)**2

        AllHahx(1006) =
     &   2*AllHahx(1002) -
     &    AllHahx(1005)/((Mh02 - MHp2)**2*(MHH2 - MHp2))

        AllHahx(1014) =
     &   CBA2*dup5*(MHH2 - MHp2) - (Mh02 - MHp2)*AllHahx(935)

        AllHahx(1021) =
     &   AllHahx(1016)/((Mh02 - MHp2)*(MHH2 - MHp2)) +
     &    (2*CBA*AllHahx(24)*
     &       (CBA*AllHahx(1018) - 2*AllHahx(1020)))/(Mh02 - MHH2)

        AllHahx(1035) =
     &   -((Mh02 - 2*MHp2)*(Mh02 - MHp2)*SBA2*AllHahx(927)) +
     &    2*(MHH2 - MHp2)*AllHahx(1034)

        AllHahx(1044) =
     &   SBA2*AllHahx(76)**2*AllHahx(920) +
     &    AllHahx(79)**2*AllHahx(1043)

        AllHahx(1058) =
     &   CBA2*(MHH2 - MHp2)*SBA2*AllHahx(927)**2 -
     &    (Mh02 - MHp2)*AllHahx(935)

        AllHahx(1059) =
     &   MHH2**2*AllHahx(1014) - Mh02*MHH2*AllHahx(1058)

        AllHahx(1065) =
     &   (Mh02 - MHp2)*SBA*AllHahx(24)*AllHahx(1063) +
     &    AllHahx(82)*AllHahx(1064)

        AllHahx(1070) =
     &   CBA*(MHH2 - MHp2)*SBA*AllHahx(82) -
     &    (Mh02 - MHp2)*AllHahx(1024)

        AllHahx(109) =
     &   (Mh02*(Mh02 - MHH2))/(MHH2 - MHp2) +
     &    AllHahx(108)/(Mh02 - MHH2)**2

        AllHahx(1135) =
     &   8*(Mh02 - 2*MHp2)*(Mh02 - MHp2)**2*S2B**2*AllHahx(883) +
     &    4*AllHahx(1118) - CBA2*AllHahx(1127) -
     &    4*((AllHahl(3)*AllHahx(24)*AllHahx(1111))/MHp2 +
     &       CAB*AllHahx(1134)) +
     &    (18*MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (CBA2**4*AllHahx(175)*AllHahx(1103))

        AllHahx(1158) =
     &   8*(Mh02 - MHp2)**2*S2B**2*AllHahx(883)*AllHahx(1136) +
     &    4*(AllHahx(1143) + CAB*AllHahx(1148)) -
     &    CBA2*AllHahx(1157) +
     &    (18*MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (CBA2**4*(Mh02 - 4*MHp2)*MHp2**2*AllHahx(175))

        AllHahx(1164) =
     &   CBA*dup2*S2B - 2*SAB*AllHahx(1162) +
     &    ((MHH2 - MHp2)*AllHahx(82)*AllHahx(1163))/
     &     (MHH2*(MHH2 - 4*MHp2)**2*MHp2) +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*
     &       (1 - (1 - 3*SBA2)*SBA2))

        AllHahx(1173) =
     &   -(MHH2*(MHH2 - 4*MHp2)**2*AllHahx(876)) +
     &    2*AllHahx(82)*AllHahx(1167) - 2*AllHahx(1172)

        AllHahx(1180) =
     &   MHp2*SBA2*(AllHahx(165)**2 + 9*AllHahx(168)**2) +
     &    2*AllHahx(1174) -
     &    AllHahx(82)*(CBA*S2B*AllHahx(1176) - 2*AllHahx(1179))

        AllHahx(1182) =
     &   (2 + SBA2)*AllHahx(165)**2 + 9*SBA2*AllHahx(168)**2

        AllHahx(1188) =
     &   CBA2*SBA*AllHahx(24)*(S2B*AllHahx(59) - 6*AllHahx(60)) +
     &    CBA*SBA2*AllHahx(76)*AllHahx(82)

        AllHahx(119) =
     &   AllHahx(115)/((Mh02 - MHH2)*(MHH2 - MHp2)**2) +
     &    ((Mh02 - MHp2)*AllHahx(118))/S2B**2

        AllHahx(1192) =
     &   (-6*CBA*AllHahx(24)*AllHahx(82))/(MHH2 - MHp2) +
     &    (CBA2*(S2B*AllHahx(59) - 6*AllHahx(60))*AllHahx(856))/
     &     (MHH2 - MHp2)**2

        AllHahx(1198) =
     &   CBA2*Mh02*(Mh02 - 4*MHH2)*
     &     (S2B*AllHahx(59) - 6*AllHahx(60))**2 +
     &    (4*Mh02 - MHH2)*MHH2*AllHahx(79)**2

	dup9 = SBA*AllHahx(76)*AllHahx(82) + AllHahx(24)*AllHahx(1063)

        AllHahx(1199) =
     &   CBA*dup9*(4*Mh02 - MHH2)*MHH2*SBA +
     &    CBA2*(S2B*AllHahx(59) - 6*AllHahx(60))**2*AllHahx(1052)

        AllHahx(1201) =
     &   -(AllHahx(24)**2*(MHp2*AllHahx(518) -
     &        (2*AllHahx(1048))/((Mh02 - MHH2)*(MHH2 - MHp2))))

        AllHahx(1202) =
     &   (2*MHH2*AllHahx(79)**2)/(MHH2 - MHp2) -
     &    SBA2*AllHahx(76)**2*AllHahx(920) + 4*AllHahx(1044)

        AllHahx(1217) =
     &   2*(MHH2 - MHp2)**2*AllHahx(82)*AllHahx(1214) -
     &    MHp2*AllHahx(1216)

        AllHahx(1233) =
     &   (3*CBA*dup4*SBA)/(Mh02 - MHH2) -
     &    (4*AllHahx(1014))/((Mh02 - 4*MHH2)*(Mh02 - MHp2))

        AllHahx(1239) =
     &   (4*(Mh02 - MHH2)*SBA*AllHahx(79))/(4*Mh02 - MHH2) +
     &    CBA*AllHahx(165)

        AllHahx(1256) =
     &   2*CBA*dup7*(Mh02 - 4*MHH2)*AllHahx(165) +
     &    (Mh02 - MHH2)*SBA*AllHahx(79)*AllHahx(1255)

        AllHahx(1257) =
     &   AllHahx(79)**2 + CBA*(S2B*AllHahx(59) - 6*AllHahx(60))*
     &     AllHahx(165)

        AllHahx(1260) =
     &   2*CBA2*(S2B*AllHahx(59) - 6*AllHahx(60))*AllHahx(76) +
     &    ((MHH2 - Mh02*SBA2)*AllHahx(76)**2)/MHH2

        AllHahx(1263) =
     &   (CBA2*(S2B*AllHahx(59) - 6*AllHahx(60))*AllHahx(1261))/
     &     (4*Mh02 - MHH2) +
     &    ((Mh02 - MHH2)*SBA2*AllHahx(1262))/(Mh02*MHH2)

        AllHahx(1282) =
     &   9*AllHahx(25)**2 -
     &    2*CBA2*(S2B*AllHahx(59) - 6*AllHahx(60))**2

        AllHahx(1312) =
     &   AllHahx(79)**2*AllHahx(1308) + CBA2*SBA2*AllHahx(1309) -
     &    2*AllHahx(1311)

        AllHahx(1313) =
     &   3*(Mh02 - 2*MHH2)*AllHahx(25) + MHH2*AllHahx(79)

        AllHahx(1337) =
     &   CBA*AllHahx(165)**2 - SBA2*AllHahx(76)*AllHahx(1336)

        AllHahx(1339) =
     &   -2*(Mh02 - MHH2)*SBA2*AllHahx(76) + MHH2*AllHahx(1338)

        AllHahx(1341) =
     &   AllHahx(76)*AllHahx(1305) +
     &    (2*(Mh02 - MHH2)**2*AllHahx(1306))/
     &     (Mh02*(Mh02 - 4*MHH2)**2)

        AllHahx(1345) =
     &   2*AllHahx(79)*AllHahx(1289) +
     &    (2*Mh02 - MHH2)*AllHahx(1344)

        AllHahx(1346) =
     &   ((Mh02 - MHH2)*(S2B*AllHahx(59) - 6*AllHahx(60))*
     &       AllHahx(1280))/(4*Mh02 - MHH2) + Mh02*AllHahx(1285)

        AllHahx(136) =
     &   -8*MHp2**4*S2B*(3 + SBA2) - MHH2**4*AllHahx(129) +
     &    MHH2**3*MHp2*AllHahx(130) +
     &    2*MHH2*MHp2**3*AllHahx(131) -
     &    2*MHH2**2*MHp2**2*AllHahx(132) +
     &    4*Mh02*AllHahx(133)*AllHahx(135)

        AllHahx(153) =
     &   32*MHp2**4*S2B*(1 + SBA2) - 4*MHH2*MHp2**3*AllHahx(146) +
     &    MHH2**3*MHp2*AllHahx(147) + 2*AllHahx(152)

        AllHahx(1556) =
     &   CBA2*AllHahx(1502) +
     &    2*(AllHahx(1420) + MHH2*AllHahx(1555))

        AllHahx(1632) =
     &   4*(Mh02 + MHH2 - 2*MHp2)*MHp2*S2B**4 +
     &    8*m12sq**2*AllHahx(1630) - S2B**2*AllHahx(1631) +
     &    (24*MW2**2*SW2**2)/(Alfa2*Pi**2)*AllHahx(175)

        AllHahx(1644) =
     &   CAB*(2*m12sq - Mh02*S2B)*AllHahx(1636) +
     &    (2*m12sq - MHH2*S2B)*SAB*
     &     (2*m12sq*SAB + S2B*AllHahx(1639))

        AllHahx(1684) =
     &   SBA2*AllHahx(1683) +
     &    2*(AllHahx(1681) +
     &       (MW2*SW2)/(Alfa*Pi)*(CBA*SBA*AllHahx(1680)))

        AllHahx(1690) =
     &   CBA*(6*m12sq*S2A - S2B*AllHahx(59)) +
     &    (6*MW2*SW2)/(Alfa*Pi)*
     &     (CBA2*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHahx(1710) =
     &   CBA2*MHH2**2*S2A**2*S2B**2 -
     &    4*SAB*(MHH2*S2B - 2*m12sq*SBA2)*AllHahx(1708) +
     &    (2*MW2**2*SW2**2)/(Alfa2*Pi**2)*(SBA2**3*AllHahx(175)) +
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*SBA2*AllHahx(1709))

        AllHahx(1720) =
     &   S2B**4*AllHahx(1714) - 16*m12sq*SAB*AllHahx(1715) -
     &    4*S2B*AllHahx(1717) + SBA2*AllHahx(1718) +
     &    4*AllHahx(1719) +
     &    (2*MW2**2*SW2**2)/(Alfa2*Pi**2)*AllHahx(175)

        AllHahx(1740) =
     &   -4*CAB*(2*m12sq - Mh02*S2B)*AllHahx(1730) -
     &    S2B**4*AllHahx(1732) - SBA2*AllHahx(1734) -
     &    9*AllHahx(1737) + 2*AllHahx(1739) +
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*AllHahx(1733))

        AllHahx(1742) =
     &   S2B**4*AllHahx(1741) + AllHahx(1638)*Re(AllHahl(13)) +
     &    AllHahx(1642)*Re(AllHahl(14))

        AllHahx(1749) =
     &   4*CAB*(2*m12sq - Mh02*S2B)*AllHahx(1730) +
     &    SBA2*AllHahx(1734) +
     &    (2*MW2**2*SW2**2)/(Alfa2*Pi**2)*(CBA2*AllHahx(175)) -
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*AllHahx(1733))

        AllHahx(1752) =
     &   4*AllHahx(1644) + S2B**4*AllHahx(1748) +
     &    (2*MW2**2*SW2**2)/(Alfa2*Pi**2)*AllHahx(175) -
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*AllHahx(1647))

        AllHahx(1754) =
     &   4*CAB*(2*m12sq - Mh02*S2B)*AllHahx(1730) +
     &    S2B**4*AllHahx(1732) + SBA2*AllHahx(1734) +
     &    9*AllHahx(1737) - 2*(AllHahx(1739) + AllHahx(1752)) -
     &    4*AllHahx(1753)

        AllHahx(1807) =
     &   -12*m12sq**2*(3*S2A**2 - 2*S2A*S2B) -
     &    S2B**2*AllHahx(1806) +
     &    12*(m12sq*(2*Mh02 + MHH2)*S2A**2*S2B +
     &       (MW2*SW2)/(Alfa*Pi)*
     &        (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA*AllHahx(315)))

        AllHahx(1818) =
     &   -4*(CAB**2*AllHahx(1810) + CAB*SBA*AllHahx(1811)) -
     &    SBA2*AllHahx(1815) +
     &    (4*MW2*SW2)/(Alfa*Pi)*(CBA*AllHahx(1817))

        AllHahx(1829) =
     &   -4*(CAB**2*AllHahx(1810) + CAB*SBA*AllHahx(1827)) -
     &    S2B**4*SBA2*AllHahx(1828) +
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*AllHahx(1637))

        AllHahx(1844) =
     &   -16*m12sq*SAB*AllHahx(1715) - 4*S2B*AllHahx(1717) +
     &    S2B**4*AllHahx(1842) + 4*AllHahx(1843) +
     &    (2*MW2**2*SW2**2)/(Alfa2*Pi**2)*AllHahx(175)

	AllHahx(185) = -8*m12sq*SAB*AllHahx(172) + S2B**2*AllHahx(184)

        AllHahx(1859) =
     &   4*AllHahx(1707) + 9*AllHahx(1710) +
     &    S2B**4*AllHahx(1856) + CBA2*AllHahx(1857) +
     &    2*AllHahx(1858)

        dup10 = AllHahx(1638)*Re(AllHahl(13)) +
     &    AllHahx(1642)*Re(AllHahl(14))

	AllHahx(1868) = dup10*(1 + SBA2) + S2B**4*AllHahx(1864)

        AllHahx(1869) =
     &   2*(1 + SBA2)*AllHahx(1752) - 2*AllHahx(1844) -
     &    SBA2*AllHahx(1859) + 4*AllHahx(1868)

        AllHahx(200) =
     &   -424*MHH2**2*S2B**2*SAB**2 + 2*MHH2*AllHahx(199) +
     &    (MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (SBA2*(61 - 228*SBA2 - 45*SBA2**2)*AllHahx(175))

	AllHahx(216) = -8*m12sq*SAB*AllHahx(205) + S2B**2*AllHahx(215)

        AllHahx(260) =
     &   -8*CBA*Mh02*SBA*AllHahx(4) + S2B*SBA2*AllHahx(258) +
     &    2*CBA2**2*(Mh02 - MHp2)*AllHahx(259)

        AllHahx(274) =
     &   Mh02**4*S2B*AllHahx(266) + 2*Mh02**3*AllHahx(268) -
     &    2*AllHahx(273)

        AllHahx(277) =
     &   CBA2**2*Mh02*(Mh02 - MHp2)**2*(5*Mh02 + 2*MHp2)*S2B**2 +
     &    (Mh02 - MHp2)*MHp2*
     &     (S2B*SBA2*AllHahx(275) - Mh02*AllHahx(276))

        AllHahx(319) =
     &   S2B*SBA2*AllHahx(310) - Mh02**3*AllHahx(313) +
     &    CBA2*Mh02*AllHahx(318)

        AllHahx(326) =
     &   MHH2*S2B**4*(CB2*Lambda6 - Lambda7*SB2)*AllHahx(294) -
     &    6*SBA2*AllHahx(325)

        AllHahx(336) =
     &   -24*m12sq*Mh02*(Mh02 - MHH2)**2*(3*Mh02 - MHH2)*S2A +
     &    (Mh02 - MHH2)*AllHahx(335)

        AllHahx(362) =
     &   S2B*AllHahx(359) - 4*MHH2*MHp2**2*AllHahx(360) +
     &    2*MHp2**3*AllHahx(361)

        AllHahx(372) =
     &   S2B*AllHahx(365) - 2*MHp2**4*AllHahx(366) +
     &    MHH2**2*MHp2**2*AllHahx(369) +
     &    2*MHH2*MHp2**3*AllHahx(371)

        AllHahx(391) =
     &   S2B*AllHahx(385) +
     &    MHH2*MHp2**2*(2*CBA*SBA*AllHahx(4) -
     &       MHp2*S2B*AllHahx(386)) + 2*AllHahx(390)

        AllHahx(415) =
     &   CBA2**2*AllHahx(405) -
     &    (Mh02 - MHH2)*(Mh02 - MHp2)*AllHahx(414)

        AllHahx(459) =
     &   CBA2*MHp2*S2B**2*(2 + SBA2) -
     &    2*CBA*(1 - 3*SBA2)*SBA2*AllHahx(46)

        AllHahx(469) =
     &   -(AllHahx(456)/(Mh02 - MHp2)) +
     &    (S2B*AllHahx(468))/(Mh02 - MHH2)

        AllHahx(515) =
     &   -(Mh02**5*S2B*AllHahx(481)) + Mh02**4*AllHahx(485) +
     &    Mh02**3*AllHahx(492) + Mh02**2*AllHahx(502) -
     &    MHH2*(MHp2*AllHahx(507) - Mh02*AllHahx(514))

        AllHahx(520) =
     &   (2*CBA*AllHahx(468))/(Mh02 - MHH2) -
     &    (CBA2*AllHahx(519))/(MHH2 - MHp2)

        AllHahx(527) =
     &   (CBA2*AllHahx(523))/((Mh02 - MHp2)*(MHH2 - MHp2)) -
     &    SBA2*AllHahx(526)

        AllHahx(536) =
     &   -(CBA2*(Mh02 - MHH2)*S2B**4*AllHahx(529)) +
     &    2*CBA*S2B*SBA*AllHahx(535)

        AllHahx(605) =
     &   CBA2*Mh02*AllHahx(589) + (MHH2 - MHp2)*AllHahx(604)

        AllHahx(609) =
     &   MHH2*S2B**2 - 2*S2B*AllHahx(59) + 12*AllHahx(60)

        AllHahx(699) =
     &   -2*CBA2**2*(Mh02 - 4*MHH2)*(Mh02 - MHH2)*(4*Mh02 - MHH2)*
     &     AllHahx(677) - MHH2*AllHahx(626)*AllHahx(680) +
     &    CBA2*AllHahx(698)

        AllHahx(819) =
     &   MHp2*S2B**2*SBA2*(3 + SBA2) - 2*CBA*SBA*AllHahx(4)

        AllHahx(827) =
     &   CBA2**2*MHH2*S2B**2 -
     &    4*CBA*(MHH2*S2B*SAB*SBA2 - SBA*AllHahx(4)) +
     &    CBA2*S2B*AllHahx(826)

        AllHahx(848) =
     &   AllHahx(847)/S2B**6 -
     &    8*CW2*SW2*(MHH2*(MHH2 + MHp2)*(1 + SBA2))/MHp2

        AllHahx(858) =
     &   2*MHH2*(Mh02 - MHp2)*SBA*AllHahx(24) +
     &    CBA*(2*Mh02 - MHH2)*(MHH2 - MHp2)*AllHahx(82)

        AllHahx(859) =
     &   (3*MHH2*SBA*AllHahx(25))/(MHH2 - MHp2) +
     &    (2*Mh02*SBA2*AllHahx(76))/(Mh02 - MHp2)

        AllHahx(86) = -((CBA2*(Mh0 - MHp)*(Mh0 + MHp)*
     &         (Mh02 - MHH2 - MHp2)*SBA2*AllHahx(76))/MHH2) +
     &    (SBA*AllHahx(24)*AllHahx(85))/Mh02

        AllHahx(902) =
     &   3*CB2*CBA*Lambda6*(Mh02 - 4*MHp2)*S2B*SBA*AllHahx(899) -
     &    Lambda7*AllHahx(901) +
     &    (16*MW2*SW2)/(Alfa*Pi)*(CB2**2*Lambda6**2*AllHahx(900))

        AllHahx(914) =
     &   Mh02*S2B*AllHahx(911) - 4*m12sq*(Mh02 - MHp2)*AllHahx(913)

        AllHahx(924) =
     &   (Mh02 - 2*MHH2)*(Mh02 - MHp2)*AllHahx(24)*AllHahx(79) -
     &    SBA2*AllHahx(76)*AllHahx(923)

        AllHahx(928) =
     &   -6*(Mh02 - MHp2)*AllHahx(76)*AllHahx(168) +
     &    CBA*(MHH2 - MHp2)*AllHahx(927)**2

        AllHahx(936) =
     &   MHH2*(1 + SBA2)*AllHahx(24)**2 +
     &    (Mh02 - MHH2 - MHp2)*AllHahx(24)*AllHahx(79) +
     &    MHp2*AllHahx(935)

        dup11 = -(CBA*MHH2*AllHahx(24)) +
     &    2*SBA*AllHahx(82)*AllHahx(1000)

        AllHahx(1007) =
     &   (4*dup11*AllHahx(999))/(Mh02 - MHH2)**2 +
     &    AllHahx(82)*AllHahx(1006)

        AllHahx(1015) =
     &   AllHahx(24)*AllHahx(1013) -
     &    (2*(MHH2 - MHp2)*AllHahx(1014))/
     &     ((Mh02 - 4*MHH2)*(Mh02 - MHp2))

        AllHahx(1033) =
     &   2*Mh02*SBA*AllHahx(82) +
     &    (CBA*AllHahx(24)*AllHahx(1032))/(Mh02 - 4*MHp2)

        AllHahx(1036) =
     &   -((Mh02 - 2*MHp2)*(Mh02 - MHp2)*AllHahx(79)) +
     &    SBA*AllHahx(1035)

        AllHahx(1040) =
     &   (CBA*MHp2*AllHahx(82))/(Mh02 - MHp2) -
     &    (4*(MHH2 - 2*MHp2)*SBA*AllHahx(999))/(Mh02 - MHH2)

        AllHahx(1041) =
     &   (4*CBA*SBA*AllHahx(24))/(Mh02 - MHH2) +
     &    (CBA2*AllHahx(82))/(Mh02 - MHp2)

        AllHahx(1042) =
     &   CBA*(MHH2 + 6*MHp2)*AllHahx(1040) +
     &    MHH2*(2*CBA*AllHahx(1040) - MHp2*AllHahx(1041))

        AllHahx(1049) =
     &   (MHp2**2*AllHahx(1021))/(MHH2 - 4*MHp2) +
     &    AllHahx(24)**2*(Mh02*MHp2*AllHahx(518) -
     &       ((3*Mh02 + 6*MHp2)*AllHahx(1048))/
     &        ((Mh02 - MHH2)*(MHH2 - MHp2)))

        AllHahx(1066) =
     &   (AllHahx(24)**2*AllHahx(518))/(Mh02 - 4*MHp2) -
     &    (CBA*AllHahx(1065))/((Mh02 - MHp2)**2*(MHH2 - MHp2))

        AllHahx(1086) =
     &   AllHahl(1)**2*AllHahx(24)*AllHahx(1085) +
     &    MHp2*AllHahl(31)*AllHahx(1085)**2

        AllHahx(1095) =
     &   CBA2*MHp2*AllHahl(37)*AllHahx(24)**2 -
     &    (AllHahl(35)*AllHahx(24)*AllHahx(1094))/(Mh02 - 4*MHp2)

        AllHahx(1159) =
     &   AllHahl(1)*AllHahx(1135) +
     &    (2*AllHahl(3)**2*AllHahx(24)*AllHahx(1102) -
     &       AllHahl(3)*AllHahx(1158))/MHp2

        AllHahx(1165) =
     &   -(AllHahx(1164)/(MHH2 - MHp2)**2) +
     &    8*CW2*SW2*AllHahx(82)/((MHH2 - 4*MHp2)*MHp2)

        AllHahx(1187) =
     &   -(AllHahx(24)**2*(Mh02*AllHahx(518) -
     &        (4*AllHahx(1048))/((Mh02 - MHH2)*(MHH2 - MHp2))))

        AllHahx(1189) =
     &   AllHahx(24)*AllHahx(1070) + (MHH2 - MHp2)*AllHahx(1188)

        AllHahx(120) =
     &   Mh02 + MHp2 - SBA2*(Mh02 + MHp2 - AllHahx(106)/Mh02) +
     &    2*AllHahx(109) + AllHahx(119)/MHH2

        AllHahx(1200) =
     &   (4*Mh02 - MHH2)*MHH2*AllHahx(1058) +
     &    CBA2*(MHH2 - MHp2)*AllHahx(1198) -
     &    (Mh02 - 4*MHH2)*AllHahx(1199)

        AllHahx(1212) =
     &   -3*MHH2*AllHahx(82)*AllHahx(124) -
     &    (MHH2 - 4*MHp2)*AllHahx(1182)

        AllHahx(1213) =
     &   AllHahl(2)**2*AllHahx(82)*AllHahx(1168) -
     &    AllHahl(34)*AllHahx(1180)

        AllHahx(1240) =
     &   -3*AllHahx(25)*AllHahx(79) + AllHahx(79)**2 +
     &    (S2B*AllHahx(59) - 6*AllHahx(60))*AllHahx(1239)

        AllHahx(1258) =
     &   (-2*(Mh02 - MHH2)*AllHahx(1256))/(Mh02 - 4*MHH2)**2 -
     &    CBA2*(3*AllHahx(25)*AllHahx(79) - AllHahx(1257))

        AllHahx(1264) =
     &   -((AllHahx(79)**2*AllHahx(1259) + SBA2*AllHahx(1260))/
     &       (Mh02 - 4*MHH2)) + AllHahx(1263)

	AllHahx(13) = 4*MHH2*S2B*SAB*SBA2 - SBA*(3 + SBA2)*AllHahx(4)

	dup12 = CBA*MHH2*AllHahx(76) + 6*Mh02*AllHahx(168)

        AllHahx(1340) =
     &   -3*dup12*SBA2*AllHahx(76) + 6*CBA*Mh02*AllHahx(165)**2 +
     &    2*(Mh02 - MHH2)*AllHahx(1337) +
     &    3*AllHahx(165)*AllHahx(1339)

        AllHahx(1347) =
     &   -(((Mh02 - MHH2)**2*SBA2*
     &         (S2B*AllHahx(59) - 6*AllHahx(60))**2*AllHahx(1273))/
     &       ((4*Mh02 - MHH2)**2*MHH2)) - MHH2*AllHahx(1282) +
     &    AllHahx(1345) - 2*AllHahx(1346)

        AllHahx(145) =
     &   4*CBA2**2*(MHH2 - MHp2)*MHp2*S2B**2*AllHahx(128) -
     &    CBA2*S2B*AllHahx(136) + 2*AllHahx(133)*AllHahx(144)

        AllHahx(154) =
     &   2*CBA2**2*(MHH2 - MHp2)**2*S2B**2*AllHahx(128) +
     &    CBA2*S2B*AllHahx(153)

        AllHahx(16) = -2*CBA2**2*(3*Mh02 - 2*MHH2)*S2B**2 +
     &    8*CBA*AllHahx(13) + CBA2*S2B*AllHahx(15)

        AllHahx(1625) =
     &   -6*CBA2*m12sq*(6*m12sq*S2A - S2B*AllHahx(59)) -
     &    S2B*(Mh02*S2B*AllHahx(1623) - 2*m12sq*AllHahx(1624))

        AllHahx(1633) =
     &   2*m12sq*AllHahx(1629) - SBA*AllHahx(1632) +
     &    (12*MW2*SW2)/(Alfa*Pi)*
     &     (MHH2*S2B*SAB*(CB2*Lambda6 - Lambda7*SB2)*(1 + 3*SBA2))

        AllHahx(1648) =
     &   4*AllHahx(1644) + S2B**4*AllHahx(1645) +
     &    (2*MW2**2*SW2**2)/(Alfa2*Pi**2)*AllHahx(175) -
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*AllHahx(1647)) -
     &    AllHahx(1638)*Re(AllHahl(13)) -
     &    AllHahx(1642)*Re(AllHahl(14))

        AllHahx(1685) =
     &   2*(SBA*AllHahx(1676) + CBA*SBA2*AllHahx(1679)) +
     &    CBA2*AllHahx(1684)

        AllHahx(1689) =
     &   3*AllHahx(25)*Re(AllHahl(1)) +
     &    AllHahx(1688)*Re(AllHahl(2)) +
     &    2*AllHahx(24)*Re(AllHahl(3))

        AllHahx(1692) =
     &   AllHahx(1690)*Re(AllHahl(1)) +
     &    AllHahx(1691)*Re(AllHahl(2)) +
     &    2*AllHahx(82)*Re(AllHahl(3))

        AllHahx(1698) =
     &   (Mh02**2 + 4*MHH2**2)*S2B**4*SBA2 +
     &    4*CAB*(2*m12sq - Mh02*S2B)*AllHahx(1696) -
     &    4*S2B**2*AllHahx(1697) +
     &    (2*MW2**2*SW2**2)/(Alfa2*Pi**2)*(CBA2*AllHahx(175))

        AllHahx(1701) =
     &   CBA2*MHH2**2*S2B**4 - 4*CBA*MHH2*S2B**2*AllHahx(125) +
     &    4*SAB**2*AllHahx(1699) + 8*SAB*AllHahx(1700) +
     &    (2*MW2**2*SW2**2)/(Alfa2*Pi**2)*(SBA2*AllHahx(175))

        AllHahx(1704) =
     &   AllHahx(1703)**2 +
     &    (18*MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (CBA2*SBA2*AllHahx(175)) -
     &    (12*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA*AllHahx(1703))

        AllHahx(1711) =
     &   CBA2*MHH2**2*S2B**4 + 4*AllHahx(1707) + 9*AllHahx(1710) +
     &    (2*MW2**2*SW2**2)/(Alfa2*Pi**2)*(SBA2*AllHahx(175))

        AllHahx(1712) =
     &   AllHahx(315)**2 +
     &    (12*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA*
     &       (6*m12sq*S2A - S2B*AllHahx(59))) +
     &    (18*MW2**2*SW2**2)/(Alfa2*Pi**2)*(CBA2*SBA2*AllHahx(175))

        dup13 = CBA2*AllHahx(1712)*Re(AllHahl(19)) +
     &    AllHahx(1638)*Re(AllHahl(21))

        AllHahx(1756) =
     &   2*dup13 + 9*AllHahx(1737)*Re(AllHahl(18)) +
     &    AllHahx(1749)*Re(AllHahl(20))

        AllHahx(1757) =
     &   Mh02*AllHahx(1754) -
     &    2*(Mh02 - 2*MHp2)*(S2B**4*AllHahx(1755) + AllHahx(1756))

        AllHahx(1808) =
     &   -(CBA2**2*AllHahx(1807)) +
     &    (18*MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (CBA2**3*SBA2*AllHahx(1786))

        AllHahx(1819) =
     &   -(CBA2*AllHahx(1818)) +
     &    (2*MW2**2*SW2**2)/(Alfa2*Pi**2)*(CBA2**2*AllHahx(1809))

        AllHahx(1834) =
     &   -(SBA2*AllHahx(1824)) -
     &    4*(SAB*AllHahx(1826) +
     &       S2B**4*AllHahx(1830)*Re(AllHahl(5))) -
     &    2*AllHahx(1831)*Re(AllHahl(7)) +
     &    4*(SAB**2*AllHahx(1820) +
     &       (MW2**2*SW2**2)/(Alfa2*Pi**2)*(CB2**2*Lambda6**2) +
     &       AllHahx(1833)*Re(AllHahl(8))) +
     &    2*AllHahx(1829)*Re(AllHahl(21))

        dup14 = AllHahx(1698)*Re(AllHahl(7)) +
     &    AllHahx(1701)*Re(AllHahl(8))

	AllHahx(1849) = CBA2*dup14 + S2B**4*AllHahx(1848)

        AllHahx(186) =
     &   CBA2*MHH2**2*S2B**4*AllHahx(170) + 2*AllHahx(185)

        AllHahx(20) = CBA2**2*(3*Mh02 - 2*MHH2)*S2B**2 +
     &    4*CBA*AllHahx(13) + CBA2*S2B*AllHahx(19)

        AllHahx(201) =
     &   CBA2*MHH2**2*S2B**4*AllHahx(187) -
     &    2*(8*AllHahx(191) + S2B**2*AllHahx(200))

        AllHahx(217) =
     &   CBA2*MHH2**2*S2B**4*AllHahx(202) + 2*AllHahx(216)

        AllHahx(221) =
     &   2*CBA*AllHahx(4) + S2B*(2*CAB*Mh02 + SBA*AllHahx(220))

        AllHahx(237) =
     &   2*MHH2*S2B**2*SBA2**2 + 8*CBA*SBA*AllHahx(4) +
     &    SBA2*AllHahx(236)

        AllHahx(278) =
     &   (4*AllHahl(3)*(MHp2*AllHahx(260) + CBA2*AllHahx(265)))/
     &     ((Mh02 - MHp2)*MHp2) +
     &    (CBA2*AllHahx(274) + 2*AllHahx(277))/(Mh02 - MHp2)**2

        AllHahx(293) =
     &   CBA*AllHahl(26)*AllHahx(165) -
     &    (AllHahl(28)*AllHahx(292))/((Mh02 - 4*MHH2)*MHH2)

        AllHahx(320) =
     &   2*Mh02*MHH2**2*AllHahx(306) - Mh02**2*MHH2*AllHahx(309) +
     &    AllHahx(319)

        AllHahx(337) =
     &   S2B**4*(2*Mh02*AllHahx(294)*AllHahx(329) -
     &       SBA2*AllHahx(336)) +
     &    (4*MW2*SW2)/(Alfa*Pi)*(CBA*Mh02*SBA*AllHahx(326))

        AllHahx(392) =
     &   Mh02**5*S2B*AllHahx(353) + Mh02**4*AllHahx(357) +
     &    MHH2*MHp2*AllHahx(362) - Mh02*AllHahx(372) -
     &    Mh02**3*AllHahx(382) - Mh02**2*AllHahx(391)

        AllHahx(450) =
     &   -((MHH2 - MHp2)*AllHahx(415)) + CBA2*AllHahx(449)

        AllHahx(460) =
     &   CBA2*MHp2**2*S2B**2*SBA2 + MHH2**2*S2B*AllHahx(458) +
     &    2*MHH2*AllHahx(459)

        AllHahx(516) =
     &   (MHH2 - MHp2)**2*AllHahx(479) - CBA2*MHH2*AllHahx(515)

        AllHahx(528) =
     &   CBA2*(Mh02 - MHp2)*AllHahx(518) -
     &    (SBA*AllHahx(520) - AllHahx(527)/Mh02)/S2B**2

        AllHahx(606) =
     &   (2*AllHahl(2)*(Mh02*MHH2*AllHahx(536) - AllHahx(557)))/
     &     (MHH2*(Mh02 - MHp2)*S2B**4) +
     &    AllHahx(605)/((Mh02 - MHH2)**2*(Mh02 - MHp2)**2*S2B**2)

        AllHahx(610) =
     &   4*CBA*MHH2*AllHahx(13) -
     &    CBA2**2*(Mh02 - MHH2)*AllHahx(609)

        AllHahx(700) =
     &   AllHahx(675)/((Mh02 - MHH2)**2*S2B**6) +
     &    (2*AllHahl(2)*AllHahx(699))/((Mh02 - MHH2)*MHH2*S2B**2)

        AllHahx(824) =
     &   MHH2**3*S2B*AllHahx(818) - 2*MHp2**2*AllHahx(819) -
     &    MHH2*MHp2*(2*CBA*AllHahx(160) + MHp2*S2B*AllHahx(820)) +
     &    MHH2**2*(MHp2*S2B*AllHahx(821) - 4*CBA*AllHahx(823))

        AllHahx(84) = -(S2B*AllHahx(59)) +
     &    6*(m12sq*S2A + (MW2*SW2)/(Alfa*Pi)*
     &        (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA))

        AllHahx(849) =
     &   4*(MHH2 - MHp2)*SBA2 +
     &    (2*CBA2*AllHahx(82)**2)/((MHH2 - 4*MHp2)*S2B**4) +
     &    AllHahx(827)/S2B**2 + AllHahx(848)

        AllHahx(863) =
     &   AllHahx(858)/((Mh02 - MHp2)**2*(MHH2 - MHp2)) -
     &    (2*AllHahx(859))/(Mh02 - MHH2) +
     &    (S2B*AllHahx(59) - 6*AllHahx(60))*AllHahx(862)

        AllHahx(87) = ((Mh02 - MHH2 + MHp2)*SBA2**2*
     &       AllHahx(76))/Mh02 + CBA*AllHahx(81)*AllHahx(82) -
     &    CBA2*AllHahx(83)*AllHahx(84) - AllHahx(86)/(MHH2 - MHp2)

        AllHahx(877) =
     &   2*AllHahx(82)*AllHahx(871) -
     &    MHH2*(MHH2 - 4*MHp2)**2*AllHahx(876)

        AllHahx(903) =
     &   CAB**2*AllHahx(898) +
     &    (MW2*SW2)/(Alfa*Pi)*(CBA2**2*AllHahx(902))

        AllHahx(915) =
     &   -32*dup8*(2*m12sq - Mh02*S2B)*AllHahx(900) -
     &    6*CBA2**2*(Mh02 - 4*MHp2)*AllHahx(908) +
     &    CBA2*AllHahx(914) +
     &    (108*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*CBA2**3*m12sq*(CB2*Lambda6 - Lambda7*SB2)*
     &       AllHahx(879))

        AllHahx(922) =
     &   (-2*CBA2*MHH2*SBA2*(S2B*AllHahx(59) - 6*AllHahx(60))*
     &       AllHahx(76))/(MHH2 - MHp2) +
     &    (CBA*Mh02*SBA*AllHahx(79)*AllHahx(165))/(Mh02 - MHp2)

        AllHahx(925) =
     &   (CBA2*(Mh02 - 2*MHH2)*AllHahx(79)**2)/(Mh02 - 4*MHH2) -
     &    (6*CBA*Mh02*SBA2*AllHahx(76)*AllHahx(168))/
     &     (Mh02 - MHH2) - AllHahx(924)/(MHH2 - MHp2)**2

        AllHahx(929) =
     &   CBA*(MHH2 - MHp2)*AllHahx(79)**2 +
     &    2*(Mh02 - MHp2)*(SBA2*AllHahx(76)*AllHahx(82) +
     &       SBA*AllHahx(79)*AllHahx(165)) + SBA2*AllHahx(928)

        AllHahx(1008) =
     &   (-2*MHp2*SBA*AllHahl(37)*AllHahx(24)*AllHahx(82))/
     &     ((Mh02 - MHp2)*(MHH2 - MHp2)) +
     &    AllHahl(36)*AllHahx(1007)

        AllHahx(1031) =
     &   (2*CBA*Mh02*SBA*AllHahx(82))/
     &     ((Mh02 - MHH2)*(Mh02 - MHp2)) -
     &    ((1 + SBA2)*AllHahx(24)*AllHahx(1030))/(MHH2 - MHp2)**2

        AllHahx(1037) =
     &   (2*CBA*AllHahx(1033))/(Mh02 - MHH2)**2 -
     &    AllHahx(1036)/((Mh02 - MHp2)*(MHH2 - MHp2)**2)

        AllHahx(1160) =
     &   2*(CBA2*(3*AllHahl(27)*AllHahx(25)*AllHahx(1083) +
     &          AllHahx(1086)) + AllHahx(1095)) -
     &    AllHahx(1159)/(Mh02 - 4*MHp2)

        AllHahx(1190) =
     &   (AllHahx(82)*AllHahx(1041))/(MHH2 - 4*MHp2) +
     &    AllHahx(1189)/((Mh02 - MHp2)*(MHH2 - MHp2)**2)

        AllHahx(121) =
     &   (MHp2*SBA2*(((Mh02 - MHp2)*AllHahl(31)*AllHahx(84))/
     &          S2B**2 + (AllHahl(29)*AllHahx(105))/
     &          (MHH2*(Mh02 - MHp2))))/(Mh02*(MHH2 - MHp2)) +
     &    AllHahl(30)*AllHahx(120)

        AllHahx(1251) =
     &   S2B**2*AllHahx(1240) -
     &    (2*(Mh02 - MHH2)*(S2B*AllHahx(59) - 6*AllHahx(60))*
     &       AllHahx(1250))/((4*Mh02 - MHH2)**2*MHH2)

        AllHahx(163) =
     &   AllHahx(145)/(MHH2 - MHp2) +
     &    (4*AllHahl(3)*(MHp2*AllHahx(154) +
     &         2*AllHahx(133)*AllHahx(162)))/
     &     ((MHH2 - MHp2)**2*MHp2)

        AllHahx(1634) =
     &   -24*m12sq**2*(3*S2A + S2B)*SAB*SBA*SBA2 -
     &    4*m12sq*AllHahx(1618) -
     &    2*(S2B*AllHahx(1622) +
     &       CAB*(CBA*AllHahx(1625) - 6*AllHahx(1627))) +
     &    CBA*AllHahx(1633)

        AllHahx(1649) =
     &   (MHH2 - MHp2)**2 + CBA2*AllHahx(1643) +
     &    AllHahx(1648)/S2B**4 +
     &    (MHH2 - MHp2)**2*(SBA2 - (1 + SBA2)*Re(AllHahl(12)))

        AllHahx(1693) =
     &   AllHahx(1685) + 2*
     &     (CBA2*SBA*AllHahx(1689) + CBA*SBA2*AllHahx(1692))

        AllHahx(1694) =
     &   AllHahx(1685) - 2*
     &     (CBA2*SBA*AllHahx(1689) + CBA*SBA2*AllHahx(1692))

        AllHahx(1702) =
     &   dup14/S2B**4 + 2*(MHH2 - MHp2)**2*Re(AllHahl(5)) +
     &    CBA2*(Mh02 - MHH2)**2*Re(AllHahl(6))

        AllHahx(1839) =
     &   8*CAB**2*(CBA2 + 9*CBA2**2)*m12sq*Mh02*S2B -
     &    4*m12sq**2*AllHahx(1763) - S2B**2*AllHahx(1766) +
     &    2*CBA*AllHahx(1796) + S2B**4*AllHahx(1800) +
     &    CBA2*AllHahx(1834) - 2*CBA2**2*AllHahx(1838) +
     &    2*(CAB*SBA*AllHahx(1772) +
     &       (MW2*SW2)/(Alfa*Pi)*AllHahx(1783)) +
     &    (MW2**2*SW2**2)/(Alfa2*Pi**2)*AllHahx(1787) -
     &    9*AllHahx(1805)*Re(AllHahl(18)) -
     &    2*AllHahx(1808)*Re(AllHahl(19)) -
     &    AllHahx(1819)*Re(AllHahl(20))

        AllHahx(21) = -2*CBA2**2*(3*Mh02 - 2*MHH2)*S2B**2 +
     &    CBA2*S2B*AllHahx(12) + 8*CBA*AllHahx(13) +
     &    (4*AllHahl(2)*AllHahx(16))/MHH2 -
     &    (8*AllHahl(2)**2*AllHahx(20))/MHH2**2

        AllHahx(218) =
     &   (8*AllHahl(2)**2*AllHahx(186))/MHH2**2 - AllHahx(201) -
     &    (4*AllHahl(2)*AllHahx(217))/MHH2

        AllHahx(224) =
     &   (2*SBA*AllHahx(221))/(Mh02 - MHp2) + AllHahx(223)/Mh02**2

        AllHahx(241) =
     &   CBA2*AllHahx(234) -
     &    2*(Mh02 - MHp2)*(2*MHp2*AllHahx(237) -
     &       CBA2**2*AllHahx(240))

        AllHahx(279) =
     &   (AllHahl(3)*(CBA2*AllHahx(249) - 2*MHp2*AllHahx(256)))/
     &     ((Mh02 - MHp2)**2*MHp2) - (AllHahl(1)*AllHahx(278))/Mh02

        AllHahx(304) =
     &   -6*SBA*AllHahl(27)*AllHahx(25) + 2*AllHahx(293) -
     &    (AllHahl(1)**2*(Mh02*AllHahx(299) + SBA2*AllHahx(303)))/
     &     (Mh02**2*AllHahx(294))

        AllHahx(338) =
     &   (-2*AllHahl(24)*AllHahx(320))/(MHH2*S2B**2) +
     &    (AllHahl(25)*AllHahx(337))/(S2B**6*AllHahx(294))

        AllHahx(343) =
     &   -((SBA*AllHahl(35)*AllHahx(24)*AllHahx(340))/
     &       (MHH2 - MHp2)) +
     &    (CBA*AllHahl(36)*AllHahx(82)*AllHahx(341))/
     &     (Mh02 - MHp2) + CBA2*SBA2*AllHahx(76)*AllHahx(342)

        AllHahx(393) =
     &   -((MHH2 - MHp2)**2*AllHahx(352)) + CBA2*AllHahx(392)

        AllHahx(451) =
     &   (8*CBA2*(MHH - MHp)*(MHH + MHp)*SBA2*AllHahl(1)**2*
     &       (S2B*AllHahx(59) - 6*AllHahx(60)))/Mh02 -
     &    AllHahx(450)/((Mh02 - MHH2)*(MHH2 - MHp2))

        AllHahx(461) =
     &   (-4*CBA*SBA*AllHahx(456))/(MHH2 - MHp2) +
     &    (S2B*AllHahx(460))/MHH2**2

        AllHahx(607) =
     &   2*Mh02*AllHahl(3)*AllHahx(528) -
     &    AllHahx(606)/(MHH2 - MHp2)

        AllHahx(618) =
     &   (Mh02 - 4*MHH2)*AllHahx(610) +
     &    2*CBA2*(SBA2*AllHahx(612) + 2*AllHahx(617))

        AllHahx(807) =
     &   (4*AllHahl(1)*AllHahx(700))/Mh02 - AllHahx(806)/S2B**6

        AllHahx(825) =
     &   CBA2**2*(MHH2 - MHp2)**2*MHp2*S2B**2*AllHahx(128) +
     &    AllHahx(133)*AllHahx(824)

        AllHahx(850) =
     &   AllHahx(849)/MHH2 -
     &    2*(1 - 2*SW2)**2*((MHH2 + MHp2)*(1 + SBA2))/MHp2

        AllHahx(864) =
     &   (6*Mh02*SBA*AllHahl(27)*AllHahx(25))/
     &     ((Mh02 - MHH2)*(Mh02 - MHp2)) + AllHahl(25)*AllHahx(863)

        AllHahx(878) =
     &   -(AllHahx(877)/
     &       (MHH2*(MHH2 - 4*MHp2)**2*(MHH2 - MHp2)**2)) +
     &    16*CW2*SW2*AllHahx(82)/(MHH2 - 4*MHp2)

        AllHahx(88) = (CBA2**2*(Mh02 - MHp2)**2)/MHH2 +
     &    AllHahx(80)/Mh02 + AllHahx(87)/S2B**2

        AllHahx(916) =
     &   -32*S2B**2*AllHahx(882)*AllHahx(883) -
     &    CBA2*AllHahx(891) - 4*AllHahx(903) +
     &    4*CAB*AllHahx(915) +
     &    (6*MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (AllHahx(175)*AllHahx(881))

        AllHahx(926) =
     &   MHp2*AllHahx(919) - AllHahx(921)/(Mh02 - 4*MHH2) -
     &    (2*AllHahx(922))/(Mh02 - MHH2) -
     &    AllHahx(925)/(Mh02 - MHp2)

	AllHahx(1635) = CBA*SBA*AllHahx(1615) - AllHahx(1634)/S2B**4

	AllHahx(164) = 16*AllHahx(127) - AllHahx(163)/(MHH2 - 4*MHp2)

        AllHahx(1650) =
     &   dup10/S2B**4 + (AllHahl(3)*AllHahx(1649))/MHp2 +
     &    CBA2*(Mh02 - MHp2)**2*Re(AllHahl(11)) +
     &    (MHH2 - MHp2)**2*(1 + SBA2)*Re(AllHahl(12))

        AllHahx(1695) =
     &   AllHahx(1673) + AllHahx(1674)*AllHahx(1693) +
     &    (2*AllHahl(2)*AllHahx(1694))/MHH2

        AllHahx(219) =
     &   (8*AllHahl(26)*(8*AllHahx(165)*AllHahx(167) +
     &         3*AllHahx(169)))/(MHH2*S2B**4) +
     &    (3*AllHahx(218))/S2B**6

	AllHahx(22) = -24*CBA2*SBA2*AllHahl(22) + AllHahx(21)/S2B**2

        AllHahx(280) =
     &   8*CBA2*AllHahl(1)**2*AllHahx(224) -
     &    AllHahx(241)/(Mh02 - MHp2) + 4*AllHahx(279)

        AllHahx(339) =
     &   (SBA2*(Mh02*AllHahl(23)*AllHahx(281) +
     &         MHH2*AllHahl(22)*AllHahx(282)))/(Mh02 - MHH2)**2 -
     &    AllHahx(304)/S2B**2 + AllHahx(338)/Mh02**2

        AllHahx(452) =
     &   -8*AllHahx(343) +
     &    (4*AllHahl(3)*AllHahx(393))/
     &     ((Mh02 - MHH2)*(Mh02 - MHp2)**2*(MHH2 - MHp2)**2) +
     &    AllHahx(451)/(Mh02 - MHp2)

        AllHahx(808) =
     &   (8*AllHahl(2)**2*AllHahx(618))/
     &     ((Mh02 - MHH2)*MHH2*S2B**2) +
     &    AllHahx(807)/(4*Mh02 - MHH2)

        AllHahx(851) =
     &   (CBA2*S2B*AllHahx(817) + 2*AllHahx(825))/
     &     ((MHH2 - 4*MHp2)*(MHH2 - MHp2)**2*S2B**2) +
     &    4*MHH2*AllHahl(3)*AllHahx(850)

        AllHahx(1593) =
     &   CBA2*SBA*AllHahx(79)*AllHahx(84) +
     &    CBA*SBA2*AllHahx(76)*AllHahx(165)

        AllHahx(1595) =
     &   -4*MHH2*AllHahx(76)*AllHahx(84) +
     &    (Mh02 - 2*MHH2)*AllHahx(927)**2

        AllHahx(1596) =
     &   (Mh02 - MHH2)*SBA2**2*AllHahx(76)**2 +
     &    SBA*AllHahx(79)*AllHahx(1310) -
     &    CBA*Mh02*AllHahx(165)*AllHahx(1559)

	AllHahx(1569) = 9*AllHahx(25)**2 - 2*CBA2*AllHahx(84)**2

        AllHahx(1572) =
     &   (4*Mh02 - MHH2)*AllHahx(79)**2 - AllHahx(84)*AllHahx(1290)

        AllHahx(1575) =
     &   2*(4*Mh02 - MHH2)*MHH2*SBA2*AllHahx(76) +
     &    5*CBA2*Mh02*(Mh02 - 4*MHH2)*AllHahx(84)

        AllHahx(1576) =
     &   (Mh02 - 2*MHH2)*(Mh02 - MHH2)*SBA*SBA2*AllHahx(76)*
     &     AllHahx(79) + CBA*Mh02*(Mh02 - 4*MHH2)*AllHahx(165)*
     &     AllHahx(1559)

        AllHahx(1578) =
     &   2*(Mh02 - MHH2)*SBA2*AllHahx(76) -
     &    MHH2*(2*SBA*AllHahx(79) - CBA2*AllHahx(84))

        AllHahx(1583) =
     &   2*(4*Mh02 - MHH2)**2*MHH2**2*
     &     (SBA2*AllHahx(76)**2 + AllHahx(79)**2) +
     &    CBA2*Mh02**2*(Mh02 - 4*MHH2)**2*AllHahx(84)**2

        AllHahx(1585) =
     &   (MHH2 - Mh02*SBA2)*AllHahx(76)**2 -
     &    2*CBA2*MHH2*AllHahx(76)*AllHahx(84)

        AllHahx(1588) =
     &   -2*CBA*Mh02*(Mh02 - 4*MHH2)*(4*Mh02 - MHH2)*AllHahx(165)*
     &     AllHahx(1559) +
     &    (Mh02 - MHH2)*SBA*AllHahx(79)*AllHahx(1575)

        AllHahx(1597) =
     &   AllHahx(79)**2*AllHahx(1308) + CBA2*SBA2*AllHahx(1595) -
     &    2*AllHahx(1596)

        AllHahx(1293) =
     &   -((2*Mh02 - MHH2)*SBA*AllHahx(79)) + AllHahx(1280)

        AllHahx(1329) =
     &   2*(Mh02 - MHH2)*SBA2*AllHahx(1306) -
     &    Mh02*(Mh02 - 4*MHH2)*AllHahx(1312)

        AllHahx(1560) =
     &   (Mh02 - MHH2)*SBA2*AllHahx(76) - 2*CBA2*MHH2*AllHahx(84)

        AllHahx(1561) =
     &   (MHH2 - Mh02*SBA2)*AllHahx(76)**2 -
     &    (Mh02 - MHH2)*AllHahx(79)**2 -
     &    2*CBA2*MHH2*AllHahx(76)*AllHahx(84)

        AllHahx(1566) =
     &   -3*(2*Mh02 - MHH2)*SBA*AllHahx(25) + CBA2*Mh02*AllHahx(84)

        AllHahx(1567) =
     &   (4*Mh02 - 3*MHH2)*(Mh02 - MHH2)*SBA2*AllHahx(1003) +
     &    4*MHH2*((2*Mh02 - MHH2)*SBA*AllHahx(79) + AllHahx(1566))

        AllHahx(1571) =
     &   MHH2**2*AllHahx(1569) +
     &    2*Mh02*MHH2*(AllHahx(79)**2 - AllHahx(84)*AllHahx(1570))

        AllHahx(1573) =
     &   2*(4*Mh02 - MHH2)*AllHahx(79)*AllHahx(1289) +
     &    (2*Mh02 - MHH2)*AllHahx(1572)

        AllHahx(1577) =
     &   (Mh02 - MHH2)*SBA*AllHahx(79)*AllHahx(1575) -
     &    2*(4*Mh02 - MHH2)*AllHahx(1576)

        AllHahx(1579) =
     &   -6*CBA*Mh02*AllHahx(165)**2 -
     &    2*(Mh02 - MHH2)*AllHahx(1337) +
     &    3*(dup12*SBA2*AllHahx(76) + AllHahx(165)*AllHahx(1578))

        AllHahx(1584) =
     &   CBA2*Mh02*(Mh02 - 4*MHH2)**2*(8*Mh02 - 3*MHH2)*
     &     AllHahx(84)**2 +
     &    (4*Mh02 - MHH2)**2*
     &     ((Mh02 - 6*MHH2)*(Mh02 - 2*MHH2)*AllHahx(79)**2 +
     &       AllHahx(1582)) + 2*AllHahx(1583)

        AllHahx(1586) =
     &   CBA2*(Mh02 - 4*MHH2)**2*(4*Mh02 - MHH2)*AllHahx(84)*
     &     AllHahx(1324) +
     &    (Mh02 - 4*MHH2)*(4*Mh02 - MHH2)**2*
     &     (AllHahx(79)**2*AllHahx(1325) + SBA2*AllHahx(1585))

        AllHahx(1590) =
     &   (Mh02 - MHH2)*SBA2*AllHahx(1583) - Mh02*AllHahx(1586)

        AllHahx(1604) =
     &   2*(Mh02 - MHH2)*SBA2*AllHahx(1306) -
     &    Mh02*(Mh02 - 4*MHH2)*AllHahx(1597)

        AllHahx(1294) =
     &   (4*Mh02 - 3*MHH2)*(Mh02 - MHH2)*SBA2*AllHahx(1003) -
     &    4*MHH2*AllHahx(1293)

        AllHahx(1330) =
     &   Mh02*(Mh02 - MHH2)*SBA2*AllHahx(1328) - AllHahx(1329)

        AllHahx(1562) =
     &   MHH2*(2*CBA2*AllHahx(79)**2 +
     &       4*CBA*AllHahx(165)*AllHahx(1559)) +
     &    SBA*AllHahx(79)*AllHahx(1560) + SBA2*AllHahx(1561)

        AllHahx(1568) =
     &   4*CBA*(4*Mh02 - MHH2)**2*MHH2*AllHahx(1565) +
     &    CBA2*(4*Mh02 - MHH2)*AllHahx(84)*AllHahx(1567)

        AllHahx(1574) =
     &   (Mh02 - MHH2)**2*SBA2*AllHahx(84)**2*AllHahx(1273) +
     &    (4*Mh02 - MHH2)**2*AllHahx(1571) +
     &    (4*Mh02 - MHH2)*MHH2*
     &     (2*(Mh02 - MHH2)*AllHahx(84)*AllHahx(1566) -
     &       AllHahx(1573))

        AllHahx(1580) =
     &   8*(Mh02 - MHH2)*(4*Mh02 - MHH2)*AllHahx(1577) -
     &    CBA*(Mh02 - 4*MHH2)**2*(4*Mh02 - MHH2)**2*AllHahx(1579)

        AllHahx(1587) =
     &   (Mh02 - MHH2)*SBA2*AllHahx(1584) - 2*Mh02*AllHahx(1586)

        AllHahx(1603) =
     &   2*Mh02*(Mh02 - 4*MHH2)*AllHahx(1562) -
     &    (Mh02 - MHH2)*SBA2*AllHahx(1602)

        AllHahx(1605) =
     &   Mh02*(Mh02 - MHH2)*SBA2*AllHahx(1328) - AllHahx(1604)

        AllHahx(1076) =
     &   CBA*Mh02*AllHahx(82) - CBA2*(Mh02 - 2*MHp2)*AllHahx(84)

        AllHahx(1193) =
     &   AllHahx(24)*AllHahx(1025) - 2*(MHH2 - MHp2)*AllHahx(1188)

        AllHahx(1281) =
     &   -9*(4*Mh02 - MHH2)**2*AllHahx(1279) +
     &    4*(Mh02 - MHH2)*(4*Mh02 - MHH2)*
     &     (S2B*AllHahx(59) - 6*AllHahx(60))*AllHahx(1280)

        AllHahx(1286) =
     &   MHH2**2*AllHahx(1282) + 2*Mh02*MHH2*AllHahx(1285)

        AllHahx(1295) =
     &   MHH2*AllHahx(1292) -
     &    2*(Mh02 - MHH2)*(S2B*AllHahx(59) - 6*AllHahx(60))*
     &     AllHahx(1294)

        AllHahx(1331) =
     &   (Mh02 - MHH2)*SBA2*AllHahx(1320) + Mh02*AllHahx(1327) -
     &    2*(4*Mh02 - MHH2)**2*AllHahx(1330)

        AllHahx(1558) =
     &   3*AllHahx(25)*AllHahx(79) - AllHahx(79)**2 +
     &    CBA*AllHahx(84)*AllHahx(165)

        AllHahx(1563) =
     &   -(dup5*(Mh02 - 2*MHH2)*(Mh02 - MHH2)*MHH2*SBA2) +
     &    Mh02*(Mh02 - 4*MHH2)*AllHahx(1562)

        AllHahx(1581) =
     &   (Mh02 - 4*MHH2)**2*
     &     (2*(Mh02 - MHH2)*AllHahx(1568) - CBA2*AllHahx(1574)) +
     &    MHH2*AllHahx(1580)

        AllHahx(101) =
     &   (CBA2*(MHH2 - MHp2)*SBA2*AllHahx(84))/(Mh02 - MHp2) -
     &    SBA*AllHahx(100)

        AllHahx(1022) =
     &   CBA*SBA2*AllHahx(76)*AllHahx(82) -
     &    CBA2*SBA*AllHahx(24)*AllHahx(84)

        AllHahx(1026) =
     &   2*(MHH2 - MHp2)*AllHahx(1022) - AllHahx(24)*AllHahx(1025)

        AllHahx(1045) =
     &   (4*Mh02 - MHH2)*MHH2*AllHahx(79)**2 +
     &    CBA2*Mh02*(Mh02 - 4*MHH2)*AllHahx(84)**2

        AllHahx(1067) =
     &   CBA*dup9*(4*Mh02 - MHH2)*MHH2*SBA +
     &    CBA2*AllHahx(84)**2*AllHahx(1052)

        AllHahx(1077) =
     &   -((Mh02 - 2*MHp2)*(Mh02 - MHp2)*SBA2*AllHahx(927)) +
     &    2*(MHH2 - MHp2)*AllHahx(1076)

        AllHahx(1181) =
     &   MHH2**2*AllHahx(82)*AllHahx(1168) - MHH2*AllHahx(1180)

        AllHahx(1183) =
     &   MHH2*AllHahx(82)*AllHahx(124) -
     &    (MHH2 - MHp2)*AllHahx(1182)

        AllHahx(1194) =
     &   CBA*CBA2*(MHH2 - MHp2)**2*
     &     (S2B*AllHahx(59) - 6*AllHahx(60))*AllHahx(82) -
     &    (Mh02 - MHp2)*AllHahx(1193)

        AllHahx(1203) =
     &   (9*Mh02*SBA*AllHahx(25)*AllHahx(84))/(Mh02 - MHH2) -
     &    (2*AllHahx(84)**2*
     &       (2*Mh02*AllHahx(1009) + AllHahx(1052)))/
     &     ((4*Mh02 - MHH2)*(MHH2 - MHp2))

        AllHahx(1205) =
     &   -2*AllHahx(84)**2*AllHahx(861) +
     &    AllHahx(1045)/((Mh02 - 4*MHH2)*(Mh02 - MHp2))

        AllHahx(1207) =
     &   CBA2*(Mh02 - 2*MHH2)*AllHahx(79)**2 +
     &    (4*MHH2*AllHahx(1058))/(MHH2 - MHp2)

        AllHahx(1224) =
     &   MHH2*(MHH2 - 4*MHp2)**2*AllHahx(1180) +
     &    2*AllHahx(82)*AllHahx(1217)

        AllHahx(1230) =
     &   2*AllHahx(1002) -
     &    AllHahx(1005)/((Mh02 - MHp2)**2*(MHH2 - MHp2)) +
     &    (8*AllHahx(1040))/(MHH2 - 4*MHp2)

        AllHahx(1234) =
     &   (Mh02 - 4*MHH2)*(S2B*AllHahx(59) - 6*AllHahx(60))**2*
     &     AllHahx(1052) - (MHH2 - MHp2)*AllHahx(1198)

        AllHahx(1296) =
     &   MHH2*AllHahx(1281) +
     &    2*((Mh02 - MHH2)**2*SBA2*
     &        (S2B*AllHahx(59) - 6*AllHahx(60))**2*AllHahx(1273) +
     &       (4*Mh02 - MHH2)**2*AllHahx(1286)) -
     &    2*(4*Mh02 - MHH2)*AllHahx(1295)

        AllHahx(1300) =
     &   -(CBA2*Mh02*(Mh02 - 4*MHH2)**2*(4*Mh02 - MHH2)*
     &       (3*AllHahx(25)*AllHahx(79) - AllHahx(1257))) +
     &    2*(Mh02 - MHH2)*AllHahx(1299)

        AllHahx(1314) =
     &   4*m12sq*SAB - MHH2*S2B*(CBA*S2B + 2*SAB) -
     &    CBA*AllHahx(84) +
     &    (2*MW2*SW2)/(Alfa*Pi)*((CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHahx(1332) =
     &   (Mh02 - MHH2)*SBA2*AllHahx(1321) -
     &    4*(4*Mh02 - MHH2)*MHH2*AllHahx(1323) +
     &    2*Mh02*AllHahx(1327) - AllHahx(1331)

        AllHahx(1564) =
     &   2*(Mh02 - MHH2)**2*SBA*SBA2*AllHahx(76)*AllHahx(79)*
     &     AllHahx(1557) +
     &    CBA2*Mh02*(Mh02 - 4*MHH2)**2*MHH2*AllHahx(1558) -
     &    2*(Mh02 - MHH2)*AllHahx(1563)

        AllHahx(1759) =
     &   -2*dup13 - (Mh02 - 2*MHp2)**2*S2B**4*SBA2 -
     &    4*CAB*(2*m12sq - Mh02*S2B)*AllHahx(1636) -
     &    CBA2*AllHahx(1738) +
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*AllHahx(1637)) -
     &    9*AllHahx(1737)*Re(AllHahl(18)) -
     &    AllHahx(1749)*Re(AllHahl(20))

        AllHahx(1852) =
     &   (MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*AllHahx(1733)) +
     &    AllHahx(1698)*Re(AllHahl(7)) +
     &    AllHahx(1701)*Re(AllHahl(8))

        AllHahx(465) =
     &   CBA*AllHahx(82) - CBA2*AllHahx(84) +
     &    SBA2*AllHahx(76)*AllHahx(464)

	AllHahx(102) = (MHH2 - MHp2)*SBA2**2 + AllHahx(101)/S2B**2

        AllHahx(1027) =
     &   CBA*CBA2*(MHH2 - MHp2)**2*AllHahx(82)*AllHahx(84) -
     &    (Mh02 - MHp2)*AllHahx(1026)

        AllHahx(1038) =
     &   (CBA*MHp2**2*AllHahx(24)*AllHahx(82))/(MHH2 - MHp2) +
     &    (3*CBA2*Mh02**2*AllHahx(25)*AllHahx(84))/(Mh02 - MHH2)

        AllHahx(1046) =
     &   AllHahx(1044) - (CBA2*AllHahx(1045))/
     &     ((4*Mh02 - MHH2)*(Mh02 - MHp2))

        AllHahx(1050) =
     &   (2*CBA2*MHH2*SBA2*AllHahx(76)*AllHahx(84))/
     &     (MHH2 - MHp2) +
     &    (CBA*Mh02*SBA*AllHahx(79)*AllHahx(165))/(Mh02 - MHp2)

        AllHahx(1055) =
     &   -(AllHahx(858)/((Mh02 - MHp2)**2*(MHH2 - MHp2))) +
     &    (2*AllHahx(859))/(Mh02 - MHH2) + AllHahx(84)*AllHahx(862)

        AllHahx(1056) =
     &   -((SBA*(((2*Mh02*MHp2 + MHp2**2)*AllHahx(856))/
     &            (MHH2 - MHp2)**2 + AllHahx(1054)))/(Mh02 - MHp2))
     &      + AllHahx(1055)*(3*Mh02 + 3/2.D0*MHH2)

        AllHahx(1068) =
     &   CBA2*(MHH2 - MHp2)*AllHahx(1045) +
     &    (4*Mh02 - MHH2)*MHH2*AllHahx(1058) -
     &    (Mh02 - 4*MHH2)*AllHahx(1067)

        AllHahx(1071) =
     &   (MHH2 - MHp2)*AllHahx(1022) + AllHahx(24)*AllHahx(1070)

        AllHahx(1074) =
     &   3*SBA*AllHahx(1073) +
     &    (MHp2*AllHahx(929)*(MHH2 + 1/2.D0*MHp2))/(Mh02 - MHp2)**2

        AllHahx(1078) =
     &   -((Mh02 - 2*MHp2)*(Mh02 - MHp2)*AllHahx(79)) +
     &    SBA*AllHahx(1077)

        AllHahx(1184) =
     &   2*AllHahx(1181) +
     &    3*MHH2*(MHp2*AllHahx(1182) - AllHahx(1183))

        AllHahx(1195) =
     &   (2*AllHahx(1021))/((Mh02 - 4*MHp2)*(MHH2 - 4*MHp2)) +
     &    AllHahx(1194)/((Mh02 - MHp2)**2*(MHH2 - MHp2)**2)

        AllHahx(1208) =
     &   (-6*CBA*Mh02*SBA2*AllHahx(76)*AllHahx(168))/
     &     (Mh02 - MHH2) - AllHahx(924)/(MHH2 - MHp2)**2 +
     &    AllHahx(1207)/(Mh02 - 4*MHH2)

        AllHahx(1225) =
     &   AllHahx(82)**2*AllHahx(1221) +
     &    2*AllHahx(82)*(MHH2*AllHahx(1222) -
     &       MHp2*AllHahx(1223)) + MHp2*AllHahx(1224)

        AllHahx(1229) =
     &   (CBA2*(S2B*AllHahx(59) - 6*AllHahx(60))**2*AllHahx(861))/
     &     (4*Mh02 - MHH2) -
     &    (MHp2*AllHahx(82)*AllHahx(1041))/(MHH2 - 4*MHp2)

        AllHahx(1231) =
     &   (4*dup11*AllHahx(999))/(Mh02 - MHH2)**2 +
     &    AllHahx(82)*AllHahx(1230)

        AllHahx(1235) =
     &   (4*Mh02 - MHH2)*MHH2*AllHahx(1058) - CBA2*AllHahx(1234)

        AllHahx(1297) =
     &   2*Mh02**2*AllHahx(1272) + Mh02*S2B**2*AllHahx(1296)

        AllHahx(1316) =
     &   CBA2**2*MHH2*(S2B*AllHahx(59) - 6*AllHahx(60))**2 +
     &    CBA2*Mh02*AllHahx(165)*AllHahx(1314) -
     &    (Mh02 - MHH2)*AllHahx(1315)

        AllHahx(1725) =
     &   S2B**4*AllHahx(1722) + AllHahx(1724)*Re(AllHahl(7)) +
     &    AllHahx(1701)*Re(AllHahl(8)) +
     &    AllHahx(1642)*Re(AllHahl(10))

        AllHahx(1726) =
     &   MHH2**2*S2B**4 + AllHahx(315)**2 +
     &    (12*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA*
     &       (6*m12sq*S2A - S2B*AllHahx(59))) +
     &    (18*MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (CBA2*SBA2*AllHahx(175)) - AllHahx(1712)*Re(AllHahl(9))

        AllHahx(1750) =
     &   -2*dup13 + 4*AllHahx(1644) + S2B**4*AllHahx(1748) +
     &    (2*MW2**2*SW2**2)/(Alfa2*Pi**2)*AllHahx(175) -
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*AllHahx(1647)) -
     &    9*AllHahx(1737)*Re(AllHahl(18)) -
     &    AllHahx(1749)*Re(AllHahl(20))

        AllHahx(1760) =
     &   -4*CAB*(2*m12sq - Mh02*S2B)*AllHahx(1730) -
     &    SBA2*AllHahx(1734) - 9*AllHahx(1737) + 2*AllHahx(1752) -
     &    S2B**4*AllHahx(1758) + 2*AllHahx(1759) +
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*AllHahx(1733))

        AllHahx(1841) =
     &   4*m12sq*AllHahx(1618) +
     &    2*(S2B*AllHahx(1622) +
     &       CAB*(CBA*AllHahx(1625) - 6*AllHahx(1627))) -
     &    CBA*AllHahx(1633) + SBA*AllHahx(1840)

        AllHahx(1846) =
     &   -4*CAB*(2*m12sq - Mh02*S2B)*AllHahx(1730) -
     &    SBA2*AllHahx(1734) - 9*AllHahx(1737) +
     &    2*(AllHahx(1759) + AllHahx(1844)) -
     &    S2B**4*AllHahx(1845) +
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*AllHahx(1733))

        AllHahx(1853) =
     &   4*CAB*(2*m12sq - Mh02*S2B)*AllHahx(1730) +
     &    S2B**4*AllHahx(1732) + SBA2*AllHahx(1734) +
     &    9*AllHahx(1737) - 2*(AllHahx(1739) + AllHahx(1844)) -
     &    4*AllHahx(1852)

        AllHahx(1865) =
     &   dup10*(1 + SBA2) + S2B**4*AllHahx(1864) -
     &    AllHahx(1698)*Re(AllHahl(7)) -
     &    AllHahx(1701)*Re(AllHahl(8))

        dup15 = SBA2*AllHahx(1704)*Re(AllHahl(7)) +
     &    AllHahx(1642)*Re(AllHahl(10))

        AllHahx(1866) =
     &   2*dup15 + AllHahx(1711)*Re(AllHahl(8)) +
     &    CBA2*AllHahx(1712)*Re(AllHahl(9))

        AllHahx(1872) =
     &   2*dup14 + SBA2*AllHahx(1866) + S2B**4*AllHahx(1871)

        AllHahx(466) =
     &   1 - (2*AllHahx(462))/(Mh02 - MHH2) -
     &    AllHahx(463)/(Mh02 - MHp2) + AllHahx(465)/(MHH2*S2B**2)

        AllHahx(930) =
     &   (Mh02 - MHp2)*(Mh02 - MHH2 - MHp2)*SBA*AllHahx(24) +
     &    2*CBA2*(MHH2 - MHp2)*MHp2*AllHahx(84)

	AllHahx(931) = Mh02*AllHahx(82)**2 + CBA2*MHp2*AllHahx(84)**2

        AllHahx(934) =
     &   CBA2*AllHahx(84)*AllHahx(932) +
     &    CBA*(Mh02 - MHp2)*AllHahx(82)*AllHahx(933)

        AllHahx(1010) =
     &   CBA*AllHahx(82) -
     &    (2*(Mh02 - MHp2)*AllHahx(84)*AllHahx(1009))/
     &     ((4*Mh02 - MHH2)*(MHH2 - MHp2))

        AllHahx(1028) =
     &   (2*AllHahx(1021))/((Mh02 - 4*MHp2)*(MHH2 - 4*MHp2)) -
     &    AllHahx(1027)/((Mh02 - MHp2)**2*(MHH2 - MHp2)**2)

        AllHahx(103) =
     &   (SBA2*AllHahx(76)*AllHahx(98))/S2B**2 + AllHahx(102)/Mh02

        AllHahx(1039) =
     &   (CBA*dup4*MHH2**2)/((Mh02 - MHH2)*(MHH2 - MHp2)) +
     &    AllHahx(1038)/(Mh02 - MHp2)

        AllHahx(1047) =
     &   (Mh02 + 6*MHH2)*AllHahx(1044) + 2*Mh02*AllHahx(1046)

        AllHahx(1051) =
     &   MHp2*AllHahx(919) - AllHahx(921)/(Mh02 - 4*MHH2) -
     &    AllHahx(925)/(Mh02 - MHp2) -
     &    (2*AllHahx(1050))/(Mh02 - MHH2)

        AllHahx(1057) =
     &   (4*AllHahx(84)**2*AllHahx(1053))/(4*Mh02 - MHH2) +
     &    AllHahx(84)*AllHahx(1056)

        AllHahx(1060) =
     &   (CBA*(2*MHH2*MHp2 + MHp2**2)*AllHahx(929))/
     &     (Mh02 - MHp2)**2 -
     &    (4*AllHahx(1059))/((Mh02 - 4*MHH2)*(Mh02 - MHp2))

        AllHahx(1069) =
     &   MHp2*AllHahx(1066) -
     &    AllHahx(1068)/
     &     ((Mh02 - 4*MHH2)*(4*Mh02 - MHH2)*(Mh02 - MHp2)*
     &       (MHH2 - MHp2))

        AllHahx(1072) =
     &   (AllHahx(82)*AllHahx(1041))/(MHH2 - 4*MHp2) +
     &    AllHahx(1071)/((Mh02 - MHp2)*(MHH2 - MHp2)**2)

        AllHahx(1075) =
     &   AllHahx(1074)/(MHH2 - MHp2) -
     &    1/2.D0*((MHH2 + 2*MHp2)*AllHahx(1007))

        AllHahx(1079) =
     &   (2*CBA*AllHahx(1033))/(Mh02 - MHH2)**2 -
     &    AllHahx(1078)/((Mh02 - MHp2)*(MHH2 - MHp2)**2)

        AllHahx(1185) =
     &   AllHahx(82)*AllHahx(1173) +
     &    (MHH2 - 4*MHp2)**2*AllHahx(1184)

        AllHahx(1196) =
     &   -((CBA*AllHahx(929))/((Mh02 - MHp2)**2*(MHH2 - MHp2))) +
     &    (8*AllHahx(1021))/((Mh02 - 4*MHp2)*(MHH2 - 4*MHp2)) +
     &    (SBA*AllHahx(1192))/(Mh02 - MHp2) - 2*AllHahx(1195)

        AllHahx(1226) =
     &   (2*AllHahl(3)**2*AllHahx(82)*AllHahx(1217) +
     &      AllHahl(3)*AllHahx(1225))/MHp2

        AllHahx(1232) =
     &   CBA2*(S2B*AllHahx(59) - 6*AllHahx(60))*AllHahx(863) +
     &    (8*AllHahx(1044))/(Mh02 - 4*MHH2) +
     &    2*(AllHahx(926) + MHp2*AllHahx(1190)) +
     &    4*AllHahx(1229) + CBA*AllHahx(1231)

        AllHahx(1236) =
     &   (CBA*MHH2*MHp2*AllHahx(929))/(Mh02 - MHp2)**2 +
     &    MHH2**2*AllHahx(1233) -
     &    (Mh02*(AllHahx(1200) - 2*AllHahx(1235)))/
     &     ((Mh02 - 4*MHH2)*(4*Mh02 - MHH2)*(Mh02 - MHp2))

	dup16 = 2*CBA*MHH2*SBA*AllHahx(165) - CBA2*AllHahx(1313)

        AllHahx(1342) =
     &   dup16*AllHahx(79) +
     &    ((Mh02 - MHH2)*AllHahx(1312))/(Mh02 - 4*MHH2) +
     &    2*AllHahx(1316) - SBA2*AllHahx(1341)

        AllHahx(1727) =
     &   4*AllHahx(1707) + 9*AllHahx(1710) + 2*AllHahx(1720) -
     &    S2B**4*AllHahx(1721) - 2*AllHahx(1725) +
     &    CBA2*AllHahx(1726) - AllHahx(1711)*Re(AllHahl(8))

        AllHahx(1751) =
     &   -2*S2B**4*AllHahx(1745) +
     &    (Mh02 - MHp2)*(Mh02 + MHp2)*
     &     (AllHahx(1740) + 4*AllHahx(1742) + 2*AllHahx(1750))

        AllHahx(1761) =
     &   4*(2*Mh02 - MHp2)*AllHahx(1742) + MHp2*AllHahx(1760)

        AllHahx(1847) =
     &   -4*CBA2**2*S2B**4*AllHahx(1744) +
     &    2*CBA*SBA*AllHahx(1841) + CBA2*AllHahx(1846)

        AllHahx(1854) =
     &   -2*CBA*SBA*AllHahx(1841) - 4*S2B**4*AllHahx(1848) +
     &    CBA2*AllHahx(1853)

        AllHahx(1867) =
     &   2*S2B**4*AllHahx(1863) +
     &    (MHH2 - MHp2)*(MHH2 + MHp2)*
     &     (2*AllHahx(1844) + SBA2*AllHahx(1859) -
     &       4*AllHahx(1865) -
     &       2*((1 + SBA2)*AllHahx(1752) - SBA2*AllHahx(1866)))

        AllHahx(1873) =
     &   MHH2*AllHahx(1869) + 2*(MHH2 - 2*MHp2)*AllHahx(1872)

        AllHahx(470) =
     &   CBA2*AllHahx(466) + (2*CBA*SBA*AllHahx(469))/S2B**3

        AllHahx(937) =
     &   (Mh02 - MHp2)*SBA2*AllHahx(76)*AllHahx(930) -
     &    CBA2*(MHH2 - MHp2)**2*AllHahx(931) -
     &    (MHH2 - MHp2)*AllHahx(934) -
     &    (Mh02 - MHp2)**2*AllHahx(936)

        AllHahx(1029) =
     &   (CBA2*AllHahl(1)**2*AllHahx(84)*AllHahx(1010))/
     &     (Mh02 - MHp2)**2 +
     &    (AllHahl(2)**2*AllHahx(1015))/(MHH2 - MHp2)**2 -
     &    AllHahl(3)**2*AllHahx(1028)

        AllHahx(104) =
     &   AllHahx(90)/(Mh02 - MHp2)**2 - SBA2*AllHahx(93) -
     &    CBA2*AllHahx(97) - (MHH2 - MHp2)*AllHahx(103)

        AllHahx(1186) =
     &   4*MHH2*AllHahl(3)*AllHahx(82)*AllHahx(1165) +
     &    AllHahx(1185)/((MHH2 - 4*MHp2)**2*(MHH2 - MHp2)**2)

        AllHahx(1227) =
     &   -6*MHp2*SBA2*AllHahl(37)*AllHahx(82)**2 +
     &    2*AllHahl(26)*AllHahx(1212) + 6*AllHahx(1213) -
     &    (3*AllHahx(1226))/(MHH2*(MHH2 - 4*MHp2)**2)

        AllHahx(1728) =
     &   2*dup15 + (AllHahl(2)*AllHahx(1727))/MHH2 +
     &    AllHahx(1711)*Re(AllHahl(8)) +
     &    CBA2*AllHahx(1712)*Re(AllHahl(9))

        AllHahx(1762) =
     &   AllHahx(1751) - 2*
     &     (AllHahl(1)*AllHahx(1757) + AllHahl(3)*AllHahx(1761))

        AllHahx(1850) =
     &   MHH2*AllHahx(1847) + 4*(2*Mh02 - MHH2)*AllHahx(1849)

        AllHahx(1855) =
     &   -2*(Mh02 - 2*MHH2)*
     &     (CBA2*AllHahx(1756) + S2B**4*AllHahx(1851)) +
     &    Mh02*AllHahx(1854)

        AllHahx(517) =
     &   2*MHH2*AllHahl(3)*AllHahx(470) +
     &    AllHahx(516)/
     &     ((Mh02 - MHH2)**2*(Mh02 - MHp2)*(MHH2 - MHp2)**2*S2B**2)

        AllHahx(938) =
     &   -((CBA*MHp2*AllHahl(34)*AllHahx(929))/(MHH2 - MHp2)) +
     &    (2*AllHahl(33)*AllHahx(937))/(MHH2 - MHp2)**2

        AllHahx(122) =
     &   (SBA*AllHahl(28)*AllHahx(77))/S2B**2 -
     &    AllHahl(33)*AllHahx(88) - AllHahl(32)*AllHahx(104) +
     &    CBA2*AllHahx(121)

        AllHahx(1729) =
     &   2*AllHahx(1702) + AllHahx(1728)/S2B**4 +
     &    3*CBA2*MHH2**2*Re(AllHahl(4)) +
     &    4*(MHH2 - MHp2)**2*SBA2*Re(AllHahl(5))

        AllHahx(608) =
     &   (AllHahl(2)**2*AllHahx(461))/S2B**3 +
     &    (AllHahl(2)*AllHahx(517))/MHH2 +
     &    (AllHahl(1)*AllHahx(607))/Mh02

        AllHahx(1594) =
     &   CBA2**2*MHH2*AllHahx(84)**2 +
     &    CBA2*Mh02*AllHahx(165)*AllHahx(1314) -
     &    (Mh02 - MHH2)*AllHahx(1593)

        AllHahx(1589) =
     &   CBA2*Mh02*(Mh02 - 4*MHH2)**2*(4*Mh02 - MHH2)*
     &     AllHahx(1558) + 2*(Mh02 - MHH2)*AllHahx(1588)

        AllHahx(1598) =
     &   2*(Mh02 - 4*MHH2)**2*AllHahx(1594) +
     &    (Mh02 - 4*MHH2)*(Mh02 - MHH2)*AllHahx(1597)

        AllHahx(1591) =
     &   (4*Mh02 - MHH2)*MHH2*AllHahx(1589) +
     &    (Mh02 - MHH2)*AllHahx(1590)

        AllHahx(1599) =
     &   -(SBA2*AllHahx(1307)) +
     &    Mh02*(dup16*(Mh02 - 4*MHH2)**2*AllHahx(79) +
     &       AllHahx(1598))

	AllHahx(1317) = dup16*AllHahx(79) + 2*AllHahx(1316)

	AllHahx(1592) = (Mh02 - MHH2)*AllHahx(1587) - AllHahx(1591)

        AllHahx(1606) =
     &   -2*(Mh02 - MHH2)**2*SBA*SBA2*AllHahx(76)*AllHahx(79)*
     &     AllHahx(1557) -
     &    CBA2*Mh02*(Mh02 - 4*MHH2)**2*MHH2*AllHahx(1558) +
     &    AllHahx(1599) + 2*(Mh02 - MHH2)*
     &     (AllHahx(1563) - AllHahx(1603) - 2*AllHahx(1605))

        AllHahx(1204) =
     &   -((MHp2*SBA*AllHahx(856))/
     &       ((Mh02 - MHp2)*(MHH2 - MHp2)**2)) +
     &    (Mh02*AllHahx(1010))/(Mh02 - MHp2)**2

        AllHahx(1318) =
     &   (Mh02 - 4*MHH2)*(Mh02 - MHH2)*AllHahx(1312) +
     &    (Mh02 - 4*MHH2)**2*AllHahx(1317)

        AllHahx(1607) =
     &   Mh02*(AllHahx(1581) + 2*AllHahx(1592)) -
     &    2*(4*Mh02 - MHH2)**2*MHH2*AllHahx(1606)

        AllHahx(1874) =
     &   -16*m12sq*SAB*AllHahx(1715) - 4*S2B*AllHahx(1717) +
     &    S2B**4*AllHahx(1842) + 4*AllHahx(1843) +
     &    SBA2*AllHahx(1866) +
     &    (2*MW2**2*SW2**2)/(Alfa2*Pi**2)*AllHahx(175)

        AllHahx(1206) =
     &   AllHahx(1203)/(Mh02 - MHp2) - AllHahx(84)*AllHahx(1204) +
     &    (2*AllHahx(1205))/(4*Mh02 - MHH2)

        AllHahx(1209) =
     &   AllHahx(937)/((Mh02 - MHp2)**2*(MHH2 - MHp2)**2) +
     &    AllHahx(1068)/
     &     ((Mh02 - 4*MHH2)*(4*Mh02 - MHH2)*(Mh02 - MHp2)*
     &       (MHH2 - MHp2))

        AllHahx(1333) =
     &   (4*Mh02 - MHH2)**2*
     &     (SBA2*AllHahx(1307) - Mh02*AllHahx(1318)) +
     &    2*((4*Mh02 - MHH2)*MHH2*AllHahx(1300) +
     &       (Mh02 - MHH2)*AllHahx(1332))

        AllHahx(1608) =
     &   (2*AllHahl(2)**2*AllHahx(1564) +
     &      (AllHahl(2)*AllHahx(1607))/(4*Mh02 - MHH2)**2)/MHH2

        AllHahx(1875) =
     &   4*dup14 - 2*(1 + SBA2)*AllHahx(1752) +
     &    SBA2*AllHahx(1859) +
     &    2*(S2B**4*AllHahx(1871) + AllHahx(1874))

        AllHahx(1061) =
     &   (2*(Mh02 + MHH2 + MHp2)*AllHahx(937))/(Mh02 - MHp2)**2 +
     &    MHH2**2*AllHahx(1015)

        AllHahx(1062) =
     &   ((Mh02 + MHH2 + MHp2)*AllHahx(937))/(MHH2 - MHp2)**2 +
     &    CBA2*Mh02**2*AllHahx(84)*AllHahx(1010)

        AllHahx(1191) =
     &   AllHahx(937)/((Mh02 - MHp2)**2*(MHH2 - MHp2)**2) +
     &    CBA*AllHahx(1007)

        dup17 = -(AllHahx(24)*AllHahx(1031)) -
     &    AllHahx(999)*AllHahx(1079)

        AllHahx(1210) =
     &   MHp2*(AllHahx(919) - 2*AllHahx(1066)) -
     &    (4*AllHahx(1201))/(Mh02 - 4*MHp2) +
     &    AllHahx(1202)/(Mh02 - 4*MHH2) -
     &    2*(AllHahx(1050)/(Mh02 - MHH2) + CBA2*AllHahx(1206)) -
     &    AllHahx(1208)/(Mh02 - MHp2) +
     &    2*(dup17 + CBA2*AllHahx(84)*AllHahx(1055) +
     &       AllHahx(1209))

        AllHahx(1228) =
     &   (MHH2*AllHahx(937))/(Mh02 - MHp2)**2 +
     &    MHH2**2*AllHahx(1015)

        AllHahx(1334) =
     &   -(CBA2*(Mh02 - 4*MHH2)**2*AllHahx(1297)) +
     &    MHH2*S2B**2*AllHahx(1333)

        AllHahx(1609) =
     &   AllHahx(1556)/((4*Mh02 - MHH2)**2*MHH2) + 4*AllHahx(1608)

        AllHahx(1876) =
     &   4*(2*MHH2 - MHp2)*AllHahx(1868) - MHp2*AllHahx(1875)

        AllHahx(1080) =
     &   12*SBA*AllHahx(1039) -
     &    8*((AllHahx(82)*AllHahx(1042))/(MHH2 - 4*MHp2) +
     &       AllHahx(1049)/(Mh02 - 4*MHp2)) -
     &    (3*Mh02 + 2*MHH2)*AllHahx(1051) -
     &    2*(dup17*(3*Mh02 + 6*MHp2) +
     &       MHH2*(CBA*AllHahx(1007) + 2*AllHahx(1051)) +
     &       CBA2*AllHahx(1057) + AllHahx(1061)/(MHH2 - MHp2)**2 +
     &       AllHahx(1062)/(Mh02 - MHp2)**2) +
     &    4*Mh02*AllHahx(1069) -
     &    4*(AllHahx(1047)/(Mh02 - 4*MHH2) +
     &       MHp2*(CBA*AllHahx(1007) + MHH2*AllHahx(1072))) +
     &    2*(MHp2**2*AllHahx(1028) + AllHahx(1060)/(MHH2 - MHp2) +
     &       CBA*AllHahx(1075))

        dup18 = -(AllHahx(24)*AllHahx(1031)) -
     &    AllHahx(999)*AllHahx(1037)

        AllHahx(1197) =
     &   4*(dup18 + (AllHahx(82)*
     &          (4*CBA*AllHahx(1040) - MHH2*AllHahx(1041)))/
     &        (MHH2 - 4*MHp2)) - 2*Mh02*AllHahx(1066) -
     &    (4*AllHahx(1187))/(Mh02 - 4*MHp2) +
     &    2*(MHH2*AllHahx(1190) + AllHahx(1191)) +
     &    MHp2*AllHahx(1196)

        AllHahx(1211) =
     &   -4*AllHahl(3)*AllHahx(1066) +
     &    (4*AllHahl(2)*AllHahx(1200))/
     &     ((Mh02 - 4*MHH2)*(4*Mh02 - MHH2)*MHH2*(Mh02 - MHp2)*
     &       (MHH2 - MHp2)) + AllHahx(1210)

        AllHahx(1237) =
     &   (2*AllHahx(1228))/(MHH2 - MHp2)**2 + MHH2*AllHahx(1232) -
     &    (2*AllHahx(1236))/(MHH2 - MHp2)

        AllHahx(1335) =
     &   (4*Mh02*AllHahl(2)*
     &       (AllHahx(1258) - (Mh02 - MHH2)*AllHahx(1264)))/S2B**4+
     &      AllHahx(1334)/
     &     ((Mh02 - 4*MHH2)**2*(4*Mh02 - MHH2)**2*MHH2*S2B**6)

        AllHahx(1610) =
     &   -8*CBA*AllHahl(26)*AllHahx(1340) +
     &    24*AllHahl(28)*AllHahx(1342) -
     &    24*CBA2*(3*AllHahl(27)*AllHahx(25)*AllHahx(1278) +
     &       AllHahl(25)*AllHahx(1347)) -
     &    (3*AllHahx(1609))/(Mh02*(Mh02 - 4*MHH2)**2)

        AllHahx(1877) =
     &   -AllHahx(1867) + 2*AllHahl(2)*AllHahx(1873) -
     &    2*AllHahl(3)*AllHahx(1876)

        AllHahx(1238) =
     &   48*dup18*AllHahl(35) -
     &    24*AllHahl(36)*AllHahx(82)*AllHahx(878) +
     &    24*(SBA*AllHahx(857) +
     &       CBA2*(S2B*AllHahx(59) - 6*AllHahx(60))*AllHahx(864) +
     &       AllHahl(28)*AllHahx(926) +
     &       AllHahx(938)/(Mh02 - MHp2)**2 + CBA*AllHahx(1008) +
     &       AllHahx(1029)) -
     &    3*(AllHahx(916)/((Mh02 - 4*MHp2)*(Mh02 - MHp2)) +
     &       AllHahx(1080)) -
     &    (12*AllHahx(1160))/(Mh02 - MHp2)**2 +
     &    12*((AllHahl(2)*AllHahx(1186))/MHH2 +
     &       AllHahl(3)*AllHahx(1197) + AllHahl(1)*AllHahx(1211))+
     &      (4*AllHahx(1227))/(MHH2 - MHp2)**2 +
     &    ((3*(CBA2*S2B**2*AllHahx(946) - 2*AllHahx(998)))/
     &        ((MHH2 - 4*MHp2)**2*(MHH2 - MHp2)) +
     &       12*AllHahl(2)*
     &        (4*MHH2*AllHahl(3)*AllHahx(1190) + AllHahx(1237)))/
     &     MHH2

        AllHahx(1611) =
     &   (24*CBA2*AllHahl(1)**2*AllHahx(1251))/S2B**6 -
     &    (12*AllHahl(1)*AllHahx(1335))/Mh02 + AllHahx(1610)/S2B**4

        AllHahx(1878) =
     &   (CBA2*AllHahx(1762))/(Mh02 - MHp2)**2 -
     &    (2*(Mh02 + MHH2)*AllHahx(1839))/(Mh02 - MHH2) +
     &    (2*(AllHahl(2)*AllHahx(1850) +
     &         AllHahl(1)*AllHahx(1855)))/(Mh02 - MHH2)**2 +
     &    AllHahx(1877)/(MHH2 - MHp2)**2

        AllHahx(1612) =
     &   144*AllHahx(122) + SBA2*AllHahx(219) -
     &    9*(MHH2*AllHahx(22) + AllHahx(280)/S2B**2) +
     &    72*(AllHahx(52)/MHH2**2 + CBA2*AllHahx(339)) +
     &    (9*AllHahx(164) - 18*AllHahx(452))/S2B**2 -
     &    72*(AllHahx(6) + AllHahl(3)**2*AllHahx(9) +
     &       CBA2*AllHahx(36) + AllHahx(608)) +
     &    (9*AllHahx(808))/(Mh02 - 4*MHH2) -
     &    36*(CBA2*AllHahx(73) + (AllHahl(2)*AllHahx(851))/MHH2) +
     &    3*(AllHahx(1238)/S2B**4 + AllHahx(1611)/(Mh02 - MHH2)**2)

        AllHahx(1879) =
     &   2*CBA*SBA*AllHahx(1614)*AllHahx(1635) +
     &    16*AllHahx(1650) - (2*AllHahx(1695))/S2B**2 +
     &    4*SBA2*AllHahx(1729) + AllHahx(1878)/S2B**4

        AllHA0eqHH = Alfa2/(MW2**2*Pi**2*SW2**2)*
     &    (1/36864.D0*AllHahx(1612) + 1/2048.D0*AllHahx(1879))


	end
