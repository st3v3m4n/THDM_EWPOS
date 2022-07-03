c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich


* AllHindMasses.F
* generated 5-Jun-2018 10:19

C> @brief This subroutine calculates the scalar two-loop contribution \f$\delta\rho^{(2)}_{H}\f$ for general values of the scalar masses.
C>
C> @ingroup group_rho
	subroutine calcAllHindMasses
	implicit none


#include "AllHindMasses.h"

	ComplexType dup1, dup10, dup2, dup3, dup4, dup5, dup6, dup7
	ComplexType dup8, dup9

	RealType T134sub

	external T134sub

	ComplexType A0FH, B0FH

	external A0FH, B0FH

#include "paraTHDM.h"
#include "paraSM.h"
	AllHil(1) = A0FH(MA02)

	AllHil(10) = B0FH(MA02,0.D0,MHp2)

	AllHil(11) = B0FH(MA02,0.D0,Mh02)

	AllHil(12) = B0FH(MA02,0.D0,MHH2)

	AllHil(13) = B0FH(MA02,MA02,Mh02)

	AllHil(14) = B0FH(MA02,MA02,MHH2)

	AllHil(15) = B0FH(Mh02,0.D0,0.D0)

	AllHil(16) = B0FH(Mh02,0.D0,MA02)

	AllHil(17) = B0FH(Mh02,0.D0,MHp2)

	AllHil(18) = B0FH(Mh02,MA02,MA02)

	AllHil(19) = B0FH(Mh02,Mh02,Mh02)

	AllHil(2) = A0FH(Mh02)

	AllHil(20) = B0FH(Mh02,MHH2,MHH2)

	AllHil(21) = B0FH(Mh02,Mh02,MHH2)

	AllHil(22) = B0FH(Mh02,MHp2,MHp2)

	AllHil(23) = B0FH(MHH2,0.D0,0.D0)

	AllHil(24) = B0FH(MHH2,0.D0,MA02)

	AllHil(25) = B0FH(MHH2,0.D0,MHp2)

	AllHil(26) = B0FH(MHH2,MA02,MA02)

	AllHil(27) = B0FH(MHH2,Mh02,Mh02)

	AllHil(28) = B0FH(MHH2,MHH2,MHH2)

	AllHil(29) = B0FH(MHH2,Mh02,MHH2)

	AllHil(3) = A0FH(MHH2)

	AllHil(30) = B0FH(MHH2,MHp2,MHp2)

	AllHil(31) = T134sub(MA02,0.D0,0.D0)

	AllHil(32) = T134sub(Mh02,0.D0,0.D0)

	AllHil(33) = T134sub(Mh02,MA02,0.D0)

	AllHil(34) = T134sub(Mh02,MA02,MA02)

	AllHil(35) = T134sub(Mh02,Mh02,MA02)

	AllHil(36) = T134sub(Mh02,Mh02,Mh02)

	AllHil(37) = T134sub(MHH2,0.D0,0.D0)

	AllHil(38) = T134sub(MHH2,MA02,0.D0)

	AllHil(39) = T134sub(MHH2,MA02,MA02)

	AllHil(4) = A0FH(MHp2)

	AllHil(40) = T134sub(MHH2,Mh02,0.D0)

	AllHil(41) = T134sub(MHH2,Mh02,MA02)

	AllHil(42) = T134sub(MHH2,Mh02,Mh02)

	AllHil(43) = T134sub(MHH2,MHH2,MA02)

	AllHil(44) = T134sub(MHH2,MHH2,Mh02)

	AllHil(45) = T134sub(MHH2,MHH2,MHH2)

	AllHil(46) = T134sub(MHp2,MA02,0.D0)

	AllHil(47) = T134sub(MHp2,Mh02,0.D0)

	AllHil(48) = T134sub(MHp2,Mh02,MA02)

	AllHil(49) = T134sub(MHp2,MHH2,0.D0)

	AllHil(5) = B0FH(MHp2,0.D0,MA02)

	AllHil(50) = T134sub(MHp2,0.D0,0.D0)

	AllHil(51) = T134sub(MHp2,Mh02,Mh02)

	AllHil(52) = T134sub(MHp2,MHH2,Mh02)

	AllHil(53) = T134sub(MHp2,MHp2,Mh02)

	AllHil(54) = T134sub(MHp2,MA02,MA02)

	AllHil(55) = T134sub(MHp2,MHp2,MA02)

	AllHil(56) = T134sub(MHp2,MHH2,MA02)

	AllHil(57) = T134sub(MHp2,MHp2,MHH2)

	AllHil(58) = T134sub(MHp2,MHH2,MHH2)

	AllHil(59) = T134sub(MHp2,MHp2,MHp2)

	AllHil(6) = B0FH(MHp2,0.D0,Mh02)

	AllHil(7) = B0FH(MHp2,0.D0,MHH2)

	AllHil(8) = B0FH(MHp2,Mh02,MHp2)

	AllHil(9) = B0FH(MHp2,MHH2,MHp2)

	AllHix(790) = MHH2**2 - MHH2*MHp2 + MHp2**2

	AllHix(1482) = 9*S2A**2 + 26*S2B**2

	AllHix(785) = MHH2*S2A - MHp2*(4*S2A + S2B)

	AllHix(788) = 2*MHp2*S2B - MHH2*(2*S2A + S2B)

	AllHix(791) = -2*MHH2*(MHH2 - 2*MHp2)*S2A + S2B*AllHix(790)

	AllHix(1143) = 4*MA02**2 - 2*MA02*MHH2 + MHH2**2

        AllHix(1149) =
     &   -2*MA02*MHH2**2*(6*S2A - 7*S2B) +
     &    3*MHH2**3*(3*S2A - S2B) + 4*MA02**3*S2B -
     &    6*MA02**2*MHH2*(S2A + 3*S2B)

	AllHix(1150) = 4*MA02**2 + 2*MA02*MHH2 - 3*MHH2**2

        AllHix(1151) =
     &   3*MHH2**2*(1 - 3*SBA2) - 4*MA02*MHH2*(2 - 3*SBA2) +
     &    2*MA02**2*(1 + 3*SBA2)

        AllHix(1438) =
     &   3*Mh02*(Mh02 - 4*MHp2)*S2A +
     &    (4*Mh02 - 7*MHp2)*(Mh02 - 2*MHp2)*S2B

	AllHix(1451) = 21*S2A**2 + 4*S2B**2

	AllHix(1454) = 18*S2A**2 + 13*S2B**2

        AllHix(1459) =
     &   MHp2*(63*S2A - 8*S2B)*S2B*SBA -
     &    (4*MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

        AllHix(1460) =
     &   MHp2*(18*S2A - 5*S2B)*S2B*SBA -
     &    (4*MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

        AllHix(1473) =
     &   MHp2*(9*S2A - 32*S2B)*S2B*SBA -
     &    (16*MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

        AllHix(1474) =
     &   MHp2*(9*S2A - 10*S2B)*S2B*SBA -
     &    (8*MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

        AllHix(1483) =
     &   MHp2**2*SBA2*AllHix(1482) +
     &    (32*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*MHp2*(CB2*Lambda6 - Lambda7*SB2)*SBA)

	AllHix(1556) = MHH2**2 - 3*MHH2*MHp2 + 5*MHp2**2

	AllHix(1612) = 3*MHH2**2 - 4*MHH2*MHp2 - 2*MHp2**2

        AllHix(569) = 2*MA02**3*Mh02 - 2*MA02**2*Mh02**2 -
     &    MA02*Mh02**3 + Mh02**4

        AllHix(570) = MA02**3 + 2*MA02**2*Mh02 -
     &    4*MA02*Mh02**2 + Mh02**3

        AllHix(572) = 6*MA02**4 - MA02**3*Mh02 -
     &    MA02**2*Mh02**2 + 5*MA02*Mh02**3 - Mh02**4

        AllHix(573) = 11*MA02**3 - 6*MA02**2*Mh02 +
     &    8*MA02*Mh02**2 - Mh02**3

	AllHix(574) = 6*MA02**2 + 3*MA02*Mh02 - Mh02**2

        AllHix(639) = 4*(Mh02 + 2*MHH2)*S2A -
     &    (4*Mh02 + 3*MHH2 - 11*MHp2)*S2B

        AllHix(783) = 2*MHp2**2*SB2 +
     &    (3*MW2*SW2)/(Alfa*Pi)*(CBA*Lambda6*(MHH2 - 2*MHp2)*SBA)

        AllHix(786) = MHH2*SB2*AllHix(785) -
     &    (3*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*Lambda6*(MHH2 - 3*MHp2)*S2B*SBA)

        AllHix(789) = MHH2*SB2*AllHix(788) +
     &    (9*MW2*SW2)/(Alfa*Pi)*(CBA*Lambda6*MHp2*S2B*SBA)

        AllHix(792) = SB2*AllHix(791) +
     &    (3*MW2*SW2)/(Alfa*Pi)*(CBA*Lambda6*(MHH2 + MHp2)*S2B*SBA)

	AllHix(105) = Mh02*MHH2**2 - MHH2**3

	AllHix(1064) = 2*Mh02*(2*MA02 + Mh02) - (MA02 + 5*Mh02)*MHH2

	AllHix(107) = 3*CBA2*Mh02*MHH2 - (5 + 3*CBA2)*MHH2**2

	AllHix(1144) = 3*MHH2*(2*MA02 + MHH2)*S2A + S2B*AllHix(1143)

	AllHix(1145) = 4*MA02**2 + MA02*MHH2 - 2*MHH2**2

        AllHix(1146) =
     &   2*MA02*MHH2 - 4*MA02**2*(1 - 3*SBA2) -
     &    MHH2**2*(1 + 3*SBA2)

        AllHix(1147) =
     &   -2*MA02*MHH2 + 4*MA02**2*(1 - 3*SBA2) +
     &    MHH2**2*(1 + 3*SBA2)

        AllHix(1152) =
     &   -(CBA*S2B*AllHix(1149)) -
     &    4*SAB*(MHH2*S2B*AllHix(1150) - m12sq*AllHix(1151)) +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*AllHix(1151))

	AllHix(1162) = 2*Mh02*S2A - 2*MA02*S2B + MHH2*(S2A + S2B)

	AllHix(1240) = -5*CB2*S2B**2 + 4*CB2**2*SB2

	AllHix(1243) = 7*CB2*S2B**2 + 4*CB2**2*SB2

        AllHix(1268) =
     &   -15*MHp2*S2B**2*SBA +
     &    (4*MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

        AllHix(1287) =
     &   8*MHp2*S2B**2*SBA2 +
     &    (5*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHix(1288) =
     &   9*MHp2*S2B**2*SBA2 +
     &    (22*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHix(1290) =
     &   27*MHp2*S2B**2*SBA2 +
     &    (88*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

	AllHix(137) = CBA2*Mh02 + (14 - CBA2)*MHH2

	AllHix(1405) = (Mh02 + 2*MHH2)*S2A - (Mh02 - 2*MHp2)*S2B

	AllHix(1426) = (7 - 3*CBA2)*Mh02 - 2*(5 - 6*CBA2)*MHp2

        AllHix(1429) =
     &   20*MHp2**2*S2B - 12*Mh02*MHp2*(S2A + 2*S2B) +
     &    Mh02**2*(3*S2A + 7*S2B)

	AllHix(1434) = (4 - 3*CBA2)*Mh02 - (7 - 12*CBA2)*MHp2

        AllHix(1435) =
     &   -(Mh02*(Mh02 + 5*MHp2)*S2B) + 2*m12sq*AllHix(1434)

	AllHix(1437) = 3*CBA2*(Mh02 - 4*MHp2) + 7*MHp2

        AllHix(1439) =
     &   S2B*SBA*AllHix(1438) -
     &    (8*MW2*SW2)/(Alfa*Pi)*(CB2*CBA*Lambda6*Mh02)

        AllHix(1442) =
     &   (8 + 4*CBA2 + 27*CBA2**3)*Mh02**2 -
     &    2*(8 + 4*CBA2 + 63*CBA2**3)*Mh02*MHp2 +
     &    4*(2 + CBA2 + 18*CBA2**3)*MHp2**2

        AllHix(1443) =
     &   (8 + 4*CBA2 + 27*CBA2**2)*Mh02**2 -
     &    2*(8 + 4*CBA2 + 63*CBA2**2)*Mh02*MHp2 +
     &    4*(2 + CBA2 + 18*CBA2**2)*MHp2**2

        AllHix(1444) =
     &   (8 + 31*CBA2)*Mh02**2 - 2*(8 + 67*CBA2)*Mh02*MHp2 +
     &    4*(2 + 19*CBA2)*MHp2**2

	AllHix(1450) = 27*S2A**2 + 4*S2B**2

        AllHix(1452) =
     &   3*MHp2*SBA2*AllHix(1451) +
     &    (8*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHix(1455) =
     &   MHp2**2*SBA2*AllHix(1454) +
     &    (16*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*MHp2*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHix(1466) =
     &   8*Mh02**3 - 8*(8 + CBA2)*Mh02**2*MHp2 +
     &    (104 + 16*CBA2 + 9*CBA2**3)*Mh02*MHp2**2 -
     &    4*(12 + 2*CBA2 + 9*CBA2**3)*MHp2**3

        AllHix(1467) =
     &   8*Mh02**3 - 8*(8 + CBA2)*Mh02**2*MHp2 +
     &    (104 + 16*CBA2 + 9*CBA2**2)*Mh02*MHp2**2 -
     &    4*(12 + 2*CBA2 + 9*CBA2**2)*MHp2**3

        AllHix(1468) =
     &   8*Mh02**3 - 8*(8 + CBA2)*Mh02**2*MHp2 +
     &    (104 + 25*CBA2)*Mh02*MHp2**2 - 4*(12 + 11*CBA2)*MHp2**3

	AllHix(1478) = 3*S2A**2*S2B**2 + 16*S2B**4

	AllHix(1500) = 2*Mh02*S2A - 2*MHp2*S2B + MHH2*(S2A + S2B)

        AllHix(1557) =
     &   4*(MHH2 - MHp2)**2 -
     &    3*(MHH2 - 4*MHp2)*(MHH2 - 2*MHp2)*SBA2**2 +
     &    2*SBA2*AllHix(1556)

	AllHix(1566) = MHH2*(4 - 9*SBA2) - 6*MHp2*(1 - 2*SBA2)

        AllHix(1567) =
     &   MHH2*(5*MHH2 - 6*MHp2)*S2B + 2*m12sq*AllHix(1566)

	AllHix(1590) = (MHH2 - MHp2)**2 + MHH2*MHp2*SBA2

	AllHix(1591) = 4*m12sq - (2*MHH2 - 3*MHp2)*S2B

	AllHix(1592) = -2*(MHH2 - MHp2)**2 + 3*MHp2**2*SBA2**2

	AllHix(1593) = -3*MHp2**2*SBA2**2 + 2*AllHix(1590)

        AllHix(1598) =
     &   3*MHp2**2*(1 - SBA2) + MHH2**2*(3 - SBA2) -
     &    MHH2*MHp2*(6 - SBA2)

        AllHix(1599) =
     &   4*(MHH2 - MHp2)**2*SAB +
     &    CBA*(MHH2**2 - 4*MHp2**2)*S2A*SBA2

	AllHix(1600) = 5*MHH2**2 - 2*MHH2*MHp2 - 6*MHp2**2

        AllHix(1601) =
     &   -2*MHH2*MHp2*(6 - SBA2) +
     &    6*MHp2**2*(1 - (1 - 2*SBA2)*SBA2) +
     &    MHH2**2*(6 - SBA2*(2 + 3*SBA2))

        AllHix(1611) =
     &   2*(MHp2**2 + MHH2**2*(1 - SBA2)) - MHH2*MHp2*(4 - 5*SBA2)

        AllHix(1613) =
     &   -8*(MHH2 - MHp2)**2*SAB + 3*CBA*S2A*SBA2*AllHix(1612)

        AllHix(1614) =
     &   MHH2**2*(4 - (4 - 9*SBA2)*SBA2) -
     &    2*MHH2*MHp2*(4 - (5 - 6*SBA2)*SBA2) +
     &    2*MHp2**2*(2 - 3*SBA2**2)

        AllHix(1615) =
     &   -(MHH2*S2B*SBA2*AllHix(1600)) + 2*m12sq*AllHix(1614)

        AllHix(575) = -4*MA02**4*Mh02 + 4*MA02**3*Mh02**2 -
     &    2*(MA02**2*Mh02**3 + MA02*MHH2**4) + MHH2*AllHix(572) -
     &    MHH2**2*AllHix(573) + MHH2**3*AllHix(574)

        AllHix(577) = 2*MA02**3 - 3*MA02**2*Mh02 -
     &    2*MA02*Mh02**2 + 3*Mh02**3

	AllHix(578) = 2*MA02**2 - 5*MA02*Mh02 + 3*Mh02**2

        AllHix(595) = 4*(Mh02 + 2*MHH2)*S2A +
     &    (11*MA02 - 4*Mh02 - 3*MHH2)*S2B

	AllHix(640) = -8*m12sq*(3*S2A + S2B) + S2B*AllHix(639)

	AllHix(642) = -3*(MHH2 - MHp2)*SAB + C2B*MHH2*SBA

        AllHix(643) = 3*MHp2*S2B*SAB +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*(1 + 3*SBA2))

	AllHix(765) = 19*S2B + 2*CAB*SBA - (S2A + 4*S2B)*SBA2

        AllHix(766) = -2*(4*CAB + 3*C2B*CBA)*m12sq +
     &    (7*MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

        AllHix(782) = 2*(MHH2 + 2*MHp2)*S2A*SB2 +
     &    (3*MW2*SW2)/(Alfa*Pi)*(CBA*Lambda6*S2B*SBA)

        AllHix(784) = -2*MHH2*(MHH2 - 2*MHp2)*S2A*SB2 +
     &    S2B*AllHix(783)

	AllHix(787) = Mh02**5*S2A*SB2 + Mh02*MHH2**2*AllHix(786)

	AllHix(793) = Mh02**2*MHH2*AllHix(789) - Mh02**3*AllHix(792)

        AllHix(968) = -63*Mh02*S2A*S2B*SBA +
     &    (2*MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

        AllHix(969) = Mh02*(27*S2A - S2B)*S2B*SBA +
     &    (2*MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

        AllHix(976) = -4*(1 - 9*CBA2**2)*MA02**2 + 4*Mh02**2 -
     &    3*MA02*(Mh02 + 3*CBA2**2*Mh02)

        AllHix(977) = -4*(1 - 9*CBA2)*MA02**2 + 4*Mh02**2 -
     &    3*MA02*(Mh02 + 3*CBA2*Mh02)

	AllHix(978) = 8*MA02**2 - 3*MA02*Mh02 + Mh02**2

	AllHix(985) = 3*S2A**2*S2B**2 + 2*S2B**4

        AllHix(992) = Mh02*(9*S2A - 11*S2B)*S2B*SBA +
     &    (6*MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

	AllHix(104) = CBA2*Mh02*MHH2 - (4 + CBA2)*MHH2**2

	AllHix(106) = -2*Mh02*MHH2**3 + 2*MHH2**4 + MA02*AllHix(105)

        AllHix(1065) =
     &   -2*(2*MA02 - Mh02)*(MA02 - MHH2)*S2B + S2A*AllHix(1064)

        AllHix(108) = MA02**3 + MHH2**3 -
     &    MA02**2*(CBA2*Mh02 - (5 + CBA2)*MHH2) +
     &    4*CBA2*AllHix(105) - MA02*AllHix(107)

        AllHix(109) = 2*MA02**2 + CBA2*Mh02*MHH2 +
     &    (2 - CBA2)*MHH2**2 - MA02*(CBA2*Mh02 - (2 + CBA2)*MHH2)

	AllHix(1091) = MA02*(Mh02 - 4*MHH2) + 5*Mh02*MHH2 - 2*MHH2**2

	AllHix(111) = MA02**2*MHH2 + 3*MA02*MHH2**2 - 4*MHH2**3

	AllHix(1111) = 2*MA02**2 - 3*MA02*MHH2 + MHH2**2

	AllHix(1118) = MHH2 + (4*MA02 - MHH2)*SBA2

        AllHix(1120) =
     &   m12sq*(MHH2 - 2*MA02*(1 - 2*SBA2)) -
     &    MHH2*(MA02*S2B + m12sq*SBA2)

        AllHix(1123) =
     &   2*(MA02**2 + MHH2**2)*S2B - MA02*MHH2*(3*S2A + 5*S2B)

        AllHix(1124) =
     &   m12sq*(MA02 - 2*MHH2) - (2*MA02 - MHH2)*MHH2*S2B

        AllHix(1125) =
     &   CB2*Lambda6*(2*MHH2 - 3*MA02*SBA2) +
     &    Lambda7*SB2*(MA02 - 2*MHH2 + 3*MA02*SBA2)

        AllHix(1126) =
     &   6*m12sq*SAB*SBA2 + (MW2*SW2)/(Alfa*Pi)*(CB2*Lambda6*SBA)

	AllHix(113) = MA02**2*MHH2**2 + MA02*MHH2**3

	AllHix(1131) = -4*MA02*(2 - 3*SBA2) + MHH2*(5 - 3*SBA2)

        AllHix(1136) =
     &   2*MA02*MHH2*(1 - 9*SBA2) - 4*MA02**2*(1 - 6*SBA2) -
     &    MHH2**2*(1 - 3*SBA2)

        AllHix(1137) =
     &   -8*MHH2*S2B*SAB -
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*(1 - 9*SBA2))

        AllHix(1138) =
     &   5*MHH2*S2B*SAB + (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*(1 - 6*SBA2))

        AllHix(1139) =
     &   2*MHH2*S2B*SAB + (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*(1 - 3*SBA2))

        AllHix(1148) =
     &   CBA*(2*MA02 - MHH2)*S2B*AllHix(1144) -
     &    4*SAB*(MHH2*S2B*AllHix(1145) - m12sq*AllHix(1146)) -
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*AllHix(1147))

	AllHix(1163) = -2*m12sq*(3*S2A - S2B) + S2B*AllHix(1162)

	AllHix(1187) = 2*MA02**2 + MA02*(MA02 + 1/2.D0*MHH2)

	AllHix(119) = MA02**2 + 3*MA02*MHH2 - 4*MHH2**2

	AllHix(1241) = 4*Lambda7*S2B**2*SB2 + Lambda6*AllHix(1240)

	AllHix(1244) = -8*Lambda7*S2B**2*SB2 + Lambda6*AllHix(1243)

        AllHix(1247) =
     &   -3*MHp2*S2B**4*SBA2 +
     &    (MW2*SW2)/(Alfa*Pi)*(CBA*SBA*AllHix(1241))

        AllHix(1249) =
     &   -5*MHp2*S2B**4*SBA2 +
     &    (MW2*SW2)/(Alfa*Pi)*(CBA*SBA*AllHix(1244))

	AllHix(1253) = Mh02**2 - 2*Mh02*MHp2 - 8*MHp2**2

	AllHix(1255) = 3*Mh02**2 - 8*MHp2**2

        AllHix(1256) =
     &   2*MHp2*S2B**2*SBA +
     &    (3*MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

        AllHix(1259) =
     &   -4*MHp2*S2B**2*SBA +
     &    (7*MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

        AllHix(1261) =
     &   2*MHp2*S2B**2*SBA +
     &    (MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

        AllHix(1262) =
     &   29*MHp2*S2B**2*SBA +
     &    (18*MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

        AllHix(1264) =
     &   -8*MHp2*S2B**2*SBA +
     &    (37*MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

        AllHix(1265) =
     &   103*MHp2*S2B**2*SBA +
     &    (106*MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

        AllHix(1267) =
     &   (11*Mh02 - 3*MHp2)*S2B**2*SBA -
     &    (8*MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

        AllHix(1269) =
     &   11*Mh02**2*S2B**2*SBA + 12*MHp2*AllHix(1261) +
     &    Mh02*AllHix(1268)

        AllHix(1275) =
     &   MHp2*S2B**2*SBA2 +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHix(1276) =
     &   15*MHp2*S2B**2*SBA2 +
     &    (22*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHix(1277) =
     &   65*MHp2*S2B**2*SBA2 +
     &    (36*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHix(1282) =
     &   63*MHp2*S2B**2*SBA2 +
     &    (94*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHix(1283) =
     &   169*MHp2*S2B**2*SBA2 +
     &    (140*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHix(1289) =
     &   -21*Mh02**3*S2B**2*SBA2 + 24*MHp2**2*AllHix(1275) -
     &    6*Mh02*MHp2*AllHix(1287) + Mh02**2*AllHix(1288)

        AllHix(1291) =
     &   23*Mh02**2*S2B**2*SBA2 - Mh02*AllHix(1290) +
     &    (24*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*MHp2*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHix(1306) =
     &   CBA2*(2*MA02 - Mh02)*(MA02 - MHp2) + 2*MA02*(Mh02 - MHp2)

	AllHix(1335) = 32 + 16*CBA2 - 12*CBA2**2 + 81*CBA2**3

	AllHix(1336) = -32 - 16*CBA2 - 60*CBA2**2 + 297*CBA2**3

	AllHix(1337) = 16 + 8*CBA2 + 12*CBA2**2 + 27*CBA2**3

        AllHix(1347) =
     &   -3*MHp2*(3*S2A - 2*S2B)*S2B*SBA +
     &    (8*MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

        AllHix(1348) =
     &   MHp2*S2B**2*SBA +
     &    (2*MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

	AllHix(135) = MA02**3 - CBA2*MA02**2*Mh02

        AllHix(1351) =
     &   MHp2*(33*S2A - 37*S2B)*S2B*SBA -
     &    (23*MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

        AllHix(1352) =
     &   MHp2*S2B*(267*S2A + 26*S2B)*SBA -
     &    (20*MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

        AllHix(1354) =
     &   3*MHp2*S2A*S2B*SBA +
     &    (4*MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

	AllHix(136) = Mh02*MHH2**3 - MHH2**4

	AllHix(1372) = 2*Mh02**2 - Mh02*(5*MHH2 - 4*MHp2) - MHH2*MHp2

        AllHix(138) = MA02**3 - (9*MA02 + CBA2*Mh02)*MHH2**2 +
     &    CBA2*MHH2**3 + MA02**2*AllHix(137)

        AllHix(139) = 5*MA02**2 + MHH2**2 +
     &    MA02*(CBA2*Mh02 - (3 + CBA2)*MHH2) +
     &    CBA2*(-(Mh02*MHH2) + MHH2**2)

	AllHix(1390) = CBA2*(MHH2 - MHp2)**2 + 2*(Mh02 - MHp2)**2*SBA2

        AllHix(140) = (7 - 2*CBA2)*MHH2**3 +
     &    2*(CBA2*Mh02*MHH2**2 + MHp2**3)

	AllHix(1406) = -2*m12sq*(3*S2A + S2B) + S2B*AllHix(1405)

        AllHix(1417) =
     &   -((2 - 3*CBA2)*Mh02**2) + 6*(1 - 3*CBA2)*Mh02*MHp2 -
     &    2*(5 - 12*CBA2)*MHp2**2

	AllHix(1418) = Mh02**2 - 12*Mh02*MHp2 + 14*MHp2**2

	AllHix(142) = MA02**2*MHH2**2 + MHH2**4

        AllHix(1421) =
     &   10*MHp2**2*S2B - 6*Mh02*MHp2*(2*S2A + S2B) +
     &    Mh02**2*(3*S2A + 2*S2B)

	AllHix(1422) = Mh02**2 - 3*Mh02*MHp2 + 5*MHp2**2

        AllHix(1427) =
     &   -(Mh02*(2*Mh02 + MHp2)*S2B) + m12sq*AllHix(1426)

	AllHix(143) = MA02**2*MHH2 - MHH2**3

        AllHix(1430) =
     &   S2B*SBA*AllHix(1429) -
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(7*Mh02 - 10*MHp2)*(CB2*Lambda6 - Lambda7*SB2))

        AllHix(1436) =
     &   2*(Mh02 - MHp2)**2*(2*m12sq - Mh02*S2B) +
     &    CBA2*MHp2*AllHix(1435)

        AllHix(1440) =
     &   AllHix(1439) + (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(Lambda7*SB2*AllHix(1434) +
     &         CB2*Lambda6*AllHix(1437)))

        AllHix(1445) =
     &   4*m12sq**2*AllHix(1442) - 4*m12sq*Mh02*S2B*AllHix(1443) +
     &    Mh02**2*S2B**2*AllHix(1444)

        AllHix(1453) =
     &   Mh02**4*SBA2*AllHix(1450) - 2*Mh02**3*AllHix(1452)

        AllHix(1469) =
     &   4*m12sq**2*AllHix(1466) - 4*m12sq*Mh02*S2B*AllHix(1467) +
     &    Mh02**2*S2B**2*AllHix(1468)

	AllHix(1501) = -2*m12sq*(3*S2A - S2B) + S2B*AllHix(1500)

	AllHix(1517) = 4*MHp2**2 + MHp2*(3*MHH2 + 2*MHp2)

	AllHix(1549) = MHH2*(S2A + S2B) - 2*MHp2*(2*S2A + S2B)

	AllHix(1554) = 6*S2A*SBA2 - S2B*(4 + 3*SBA2)

	AllHix(1558) = MHH2**3*S2B*(4 - SBA2) - 2*m12sq*AllHix(1557)

        AllHix(1559) =
     &   4*MHp2*S2B*SAB*(2 - 3*SBA2) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*
     &       (4 + (2 - 3*SBA2)*SBA2))

        AllHix(1560) =
     &   MHp2*S2B*SAB*(2 - 7*SBA2) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*
     &       (4 + 3*SBA2 - 9*SBA2**2))

        AllHix(1565) =
     &   2*MHH2*MHp2*(6*S2A - 7*S2B) - MHH2**2*(9*S2A - 4*S2B) +
     &    12*MHp2**2*S2B

        AllHix(1568) =
     &   SAB*AllHix(1567) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*AllHix(1566))

	AllHix(1574) = 2*MHH2 - MHp2*(4 - SBA2)

        AllHix(1575) =
     &   2*MHH2**2 + MHp2**2*(2 - (7 - 12*SBA2)*SBA2) -
     &    MHH2*MHp2*(4 - (4 - 3*SBA2)*SBA2)

        AllHix(1582) =
     &   4*MHH2**2 + 2*MHp2**2*(2 - (5 - 6*SBA2)*SBA2) -
     &    MHH2*MHp2*(8 - (7 - 3*SBA2)*SBA2)

	AllHix(1589) = 4*(MHH2 - MHp2)**2*SAB + 3*CBA*MHp2**2*S2A*SBA2

        AllHix(1594) =
     &   CBA*(MHH2 - 2*MHp2)*S2B**2*AllHix(1590) +
     &    SAB*(MHH2*MHp2*SBA2*AllHix(1591) -
     &       2*m12sq*AllHix(1592)) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*AllHix(1593))

        AllHix(1597) =
     &   -2*m12sq*(MHH2 - MHp2)**2 + MHH2**2*(MHH2 - 2*MHp2)*S2B

        AllHix(1602) =
     &   3*MHH2*S2B*AllHix(1599) -
     &    2*(CBA*(MHH2 - 2*MHp2)*S2B**2*AllHix(1598) +
     &       SAB*(MHH2*S2B*SBA2*AllHix(1600) +
     &          2*m12sq*AllHix(1601)))

        AllHix(1603) =
     &   8*MHp2*(MHH2**2 + MHp2**2) + (MHH2 - 4*MHp2)*AllHix(1601)

	AllHix(1605) = -((MHH2 + 2*MHp2)*(-3 - 2*SBA2))

        AllHix(1607) =
     &   MHp2*S2B*SAB + (2*MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHix(1616) =
     &   MHH2*S2B*AllHix(1613) +
     &    2*(CBA*(MHH2 - 2*MHp2)*S2B**2*AllHix(1611) +
     &       SAB*AllHix(1615) +
     &       (MW2*SW2)/(Alfa*Pi)*
     &        ((CB2*Lambda6 - Lambda7*SB2)*SBA*AllHix(1614)))

	AllHix(1623) = -3*CBA2*(MHH2 - MHp2) + (Mh02 - MHp2)*SBA2

        AllHix(1650) =
     &   6*Mh02*S2A + 3*MHH2*(S2A - S2B) + 2*(MA02 + 2*MHp2)*S2B

	AllHix(1651) = 2*MA02 - 3*(Mh02 + MHH2) + 4*MHp2

        AllHix(1658) =
     &   CBA2*(S2B - 3*S2A*SBA2) - S2A*(CBA2**2 + 2*SBA2**2)

	AllHix(1667) = 2*MA02**2 - MA02*MHH2 - 2*(MHH2 - 2*MHp2)*MHp2

	AllHix(1712) = (MA02 - Mh02)**2 + (2*MA02 - MHH2)**2

	AllHix(1713) = (2*MA02 - Mh02)**2 + (MA02 - MHH2)**2

        AllHix(1715) =
     &   CBA*(2*MA02 - MHH2)*S2B**2 - (2*m12sq - MHH2*S2B)*SAB

	AllHix(1763) = CBA2*(MA02 - Mh02)**2 + 2*(MA02 - MHp2)**2

	AllHix(1764) = CBA2*(2*MA02 - MHH2)**2 + SBA2*AllHix(1713)

	AllHix(188) = (1 - 3*CBA2)*m12sq*MHp2 - Mh02*(Mh02 - MHp2)*S2B

        AllHix(198) = (Mh02 - MHH2)**3*(Mh02 + MHH2)*S2A +
     &    Mh02**2*MHH2**2*S2B

        AllHix(200) = 2*(Mh02 - MHH2)**3*S2A +
     &    Mh02*MHH2*(Mh02 + MHH2)*S2B

        AllHix(209) = 2*Mh02**3 - Mh02**2*MHH2 +
     &    3*Mh02*MHH2**2 - 2*MHH2**3

	AllHix(231) = 2*CAB*SBA + S2B*(4 - SBA2)

        AllHix(288) = 2*MA02**3*Mh02**2 - 2*MA02**2*Mh02**3 -
     &    MA02*Mh02**4 + Mh02**5

        AllHix(289) = MA02**3*Mh02 + 2*MA02**2*Mh02**2 -
     &    4*MA02*Mh02**3 + Mh02**4

	AllHix(290) = 2*MA02**2*Mh02 - MA02*Mh02**2 - Mh02**3

	AllHix(295) = MA02**2*Mh02**2 + MA02*Mh02**3

        AllHix(328) = 2*MA02**3 - 7*MA02**2*Mh02 +
     &    8*MA02*Mh02**2 - 3*Mh02**3

	AllHix(331) = 8*Mh02*MHH2**2 + 3*MHH2**3

        AllHix(380) = 2*MHH2*S2B*SBA2 -
     &    MHp2*(S2B + 8*CAB*SBA - 4*S2B*SBA2)

	AllHix(382) = 2*CAB*SBA - S2B*(4 + SBA2)

	AllHix(383) = 2*CAB*SBA - S2B*(5 + SBA2)

	AllHix(385) = 6*CAB*SBA - S2B*(5 + 3*SBA2)

	AllHix(392) = 2*CAB*SBA + S2B*(7 - SBA2)

	AllHix(393) = 2*CAB*SBA + S2B*(18 - SBA2)

	AllHix(395) = 2*CAB*MHp2*SBA + (MHH2 - MHp2)*S2B*SBA2

	AllHix(398) = -4*CAB*SBA + S2B*(-13 + 2*SBA2)

	AllHix(411) = 2*CAB*SBA + S2B*(10 - SBA2)

	AllHix(412) = 2*CAB*SBA + S2B*(14 - SBA2)

	AllHix(414) = 4*CAB*SBA + S2B*(7 - 2*SBA2)

        AllHix(415) = Mh02**2*(9*MHp2 - 2*MHH2*SBA2) -
     &    2*MHp2**2*(MHp2 - MHH2*SBA2)

        AllHix(417) = MHH2*S2B*SBA2 +
     &    2*MHp2*(S2B - 2*CAB*SBA + S2B*SBA2)

        AllHix(449) = 4*MA02**2 - 3*(MA02*Mh02 + Mh02**2) +
     &    2*(MA02 + 2*Mh02)*MHH2

	AllHix(452) = CAB*MA02 - (CAB + C2B*CBA)*Mh02

	AllHix(458) = MHp2*S2B**2 + MHH2*S2B*(S2A + 10*CBA*SAB)

        AllHix(492) = S2A - 2*CBA2*S2B + 8*CAB*SBA +
     &    (S2A - 4*S2B)*SBA2

	AllHix(524) = 4*CBA*SAB - S2B*(3 + 2*SBA2)

        AllHix(543) = Mh02**4 + Mh02**3*MHH2 -
     &    4*Mh02**2*MHH2**2 + Mh02*MHH2**3 + MHH2**4

	AllHix(544) = Mh02**2*MHH2 + Mh02*MHH2**2

	AllHix(545) = -Mh02**3 - MHH2**3 + AllHix(544)

	AllHix(546) = Mh02**2 - 2*Mh02*MHH2 + MHH2**2

        AllHix(548) = 5*Mh02**3*MHH2 - 12*Mh02**2*MHH2**2 -
     &    Mh02*MHH2**3 - 2*MHH2**4

        AllHix(549) = 2*Mh02**3 - Mh02**2*MHH2 -
     &    14*Mh02*MHH2**2 - 8*MHH2**3

	AllHix(550) = 4*Mh02**2 - 5*Mh02*MHH2 - 15*MHH2**2

	AllHix(554) = 2*Mh02**2*MHH2 - 5*Mh02*MHH2**2

	AllHix(555) = 5*Mh02**2 - 3*Mh02*MHH2 - 2*MHH2**2

	AllHix(556) = Mh02**2*MHH2**2 + Mh02**3*MHp2

        AllHix(579) = -2*MA02**3*Mh02 + 5*MA02**2*Mh02**2 -
     &    3*MA02*Mh02**3 + MHH2*AllHix(577) - MHH2**2*AllHix(578)

        AllHix(585) = 3*Mh02**2*(2*S2A - S2B) -
     &    5*Mh02*MHH2*S2B - 3*MHH2**2*(2*S2A + S2B)

	AllHix(586) = Mh02*(S2A - 5*S2B) - MHH2*(S2A + 5*S2B)

        AllHix(587) = 2*Mh02**2*MHH2*(7*S2A - 16*S2B) +
     &    Mh02**3*(6*S2A - 7*S2B) - MHH2**3*(6*S2A + 7*S2B) -
     &    2*Mh02*MHH2**2*(7*S2A + 16*S2B)

        AllHix(588) = Mh02**2*(14*S2A - 23*S2B) -
     &    89*Mh02*MHH2*S2B - MHH2**2*(14*S2A + 23*S2B)

	AllHix(590) = -2*(MA02 - Mh02)*SAB + C2B*Mh02*SBA

        AllHix(591) = MHH2*S2B*SAB +
     &    (MW2*SW2)/(Alfa*Pi)*((CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHix(592) = 2*MHH2*S2B*SAB +
     &    (MW2*SW2)/(Alfa*Pi)*((CB2*Lambda6 - Lambda7*SB2)*SBA)

	AllHix(596) = -8*m12sq*(3*S2A + S2B) + S2B*AllHix(595)

        AllHix(598) = -3*MHH2*S2B*SAB +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*(1 + 6*SBA2))

	AllHix(599) = 3*(MA02 - MHH2)*SAB + C2B*MHH2*SBA

        AllHix(600) = -2*MHH2*S2B*SAB +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*(1 + 3*SBA2))

	AllHix(622) = MHH2*MHp2**2 - MHp2**3

	AllHix(624) = 3*MHH2**3 - 8*MHH2**2*MHp2

        AllHix(629) = MHp2**2*(14*S2A - 29*S2B) -
     &    2*MHH2*MHp2*(7*S2A - 22*S2B) - 11*MHH2**2*S2B

	AllHix(630) = 6*(MHH2 - MHp2)*S2A - (3*MHH2 - 7*MHp2)*S2B

        AllHix(631) = 4*MHp2**3*(2*S2A - 21*S2B) +
     &    145*MHH2*MHp2**2*S2B + 3*MHH2**3*(2*S2A + S2B) -
     &    2*MHH2**2*MHp2*(7*S2A + 22*S2B)

        AllHix(632) = 50*MHp2**3*S2B -
     &    MHH2**3*(6*S2A + 7*S2B) -
     &    4*MHH2*MHp2**2*(2*S2A + 21*S2B) +
     &    MHH2**2*MHp2*(14*S2A + 29*S2B)

	AllHix(635) = 2*(Mh02 - MHp2)*SAB + C2B*Mh02*SBA

        AllHix(641) = -8*CAB*(2*m12sq - Mh02*S2B)*SBA +
     &    SBA2*AllHix(640)

        AllHix(644) = 4*MHH2**2*S2B*SAB +
     &    2*m12sq*AllHix(642) - MHH2*AllHix(643) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (MHp2*(CB2*Lambda6 - Lambda7*SB2)*SBA*(1 + 6*SBA2))

	AllHix(690) = 2*CBA*SAB - S2A*(1 - SBA2) - 3*S2B*SBA2

	AllHix(692) = 4*S2A - 8*CBA*SAB - (2*S2A - 17*S2B)*SBA2

        AllHix(693) = 6*CBA*SAB + 20*CAB*SBA -
     &    S2A*(2 - SBA2) - S2B*(3 + 10*SBA2)

        AllHix(699) = -2*m12sq*SAB*SBA2 +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*(3 - SBA2))

	AllHix(704) = S2A - 2*CBA*SAB + (2*S2A + 3*S2B)*SBA2

	AllHix(707) = 4*CBA*SAB - S2B*(2 + 3*SBA2)

        AllHix(710) = -2*m12sq*SAB*(1 - 3*SBA2)*SBA2 +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*
     &       (3 - 2*SBA2 + 3*SBA2**2))

	AllHix(743) = S2A + 2*CAB*SBA - S2B*(2 + SBA2)

        AllHix(744) = MHH2*(S2A - 10*CBA*SAB + 2*S2B*SBA2) +
     &    MHp2*AllHix(743)

        AllHix(750) = (MA02 - 2*MHp2)*S2B +
     &    MHH2*(S2A - 2*CBA*SAB + 2*S2B*SBA2)

        AllHix(755) = 2*Mh02**2 - Mh02*(5*MHH2 - 2*MHp2) +
     &    (MHH2 - 2*MHp2)*MHp2

	AllHix(757) = C2B*CBA*Mh02 + CAB*(Mh02 - MHp2)

	AllHix(763) = 23*S2B - 6*CAB*SBA - 3*(S2A + S2B)*SBA2

        AllHix(767) = MHH2*(3*S2A*S2B + 5*S2B**2)*SBA2 -
     &    MHp2*S2B*AllHix(765) - 2*SBA*AllHix(766)

        AllHix(794) = Mh02**4*AllHix(782) -
     &    MHH2**3*AllHix(784) - 2*AllHix(787) + 2*AllHix(793)

	AllHix(833) = MHH2**2 - 3*MHH2*MHp2 - MHp2**2

	AllHix(834) = MHH2**2 + 3*MHH2*MHp2 - MHp2**2

        AllHix(835) = 4*MHH2**3 - 10*MHH2**2*MHp2 +
     &    11*MHH2*MHp2**2 - 4*MHp2**3

        AllHix(856) = Mh02**5 + Mh02**4*MHH2 -
     &    4*Mh02**3*MHH2**2 + Mh02**2*MHH2**3 + Mh02*MHH2**4

	AllHix(858) = Mh02**3 - 2*Mh02**2*MHH2 + Mh02*MHH2**2

	AllHix(87) = MA02**3 - 9*MA02**2*MHH2 - 2*MHH2**3

	AllHix(870) = Mh02**2*MHH2**2*MHp2 + Mh02**3*MHp2**2

        AllHix(871) = Mh02**3*MHH2**2 +
     &    (Mh02**4 + Mh02**3*MHH2)*MHp2 + Mh02**2*MHp2**3 +
     &    MHp2**2*AllHix(544)

        AllHix(873) = 5*Mh02**3*MHH2 - 2*Mh02**2*MHH2**2 -
     &    3*Mh02*MHH2**3

	AllHix(874) = 2*Mh02**3 + 3*Mh02**2*MHH2 - 5*Mh02*MHH2**2

	AllHix(88) = 9*MA02**2 + 8*MA02*MHH2 - 13*MHH2**2

	AllHix(90) = MA02**2*MHH2 + MA02*MHH2**2

        AllHix(903) = -2*m12sq*SAB*SBA2*(1 + 6*SBA2) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*(4 - SBA2 - 6*SBA2**2))

	AllHix(905) = 32*CBA*SAB - S2B*(23 + 17*SBA2)

	AllHix(906) = 6*CBA*SAB + S2B*(9 + 13*SBA2)

	AllHix(91) = MHH2**3*MHp2 + MA02*MHH2*MHp2**2

	AllHix(945) = -4*(2 - 3*CBA2)*MA02 + (5 - 3*CBA2)*Mh02

	AllHix(957) = 18*MA02**2 - 31*MA02*Mh02 + 7*Mh02**2

	AllHix(959) = 18*S2A**2 - S2B**2

	AllHix(96) = 11*MHp2 - 4*(Mh02 - MHH2)*SBA2

        AllHix(965) = m12sq*S2A*SBA -
     &    (MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

        AllHix(967) = (2*MA02 - Mh02)*S2B**2*SBA +
     &    (2*MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

	AllHix(97) = 11*MHp2 - (Mh02 - MHH2)*SBA2

        AllHix(970) = 4*MA02**2*S2B*(18*S2A + S2B)*SBA +
     &    2*MA02*AllHix(968) + Mh02*AllHix(969)

        AllHix(979) = m12sq**2*AllHix(976) -
     &    m12sq*Mh02*S2B*AllHix(977) + Mh02**2*S2B**2*AllHix(978)

	AllHix(981) = 9*S2A**2 + 19*S2B**2

        AllHix(983) = Mh02*S2B**2*SBA2 -
     &    (8*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

	AllHix(988) = 4*MA02**2 + 3*MA02*Mh02 - 4*Mh02**2

        AllHix(990) = -(Mh02*(18*S2A - S2B)*S2B*SBA) +
     &    (4*MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

        AllHix(991) = Mh02*S2B**2*SBA -
     &    (2*MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

        AllHix(993) = 18*CBA2*(4*MA02 - Mh02)*AllHix(965) +
     &    Mh02*AllHix(992)

	AllHix(100) = MA02*MHH2**2 + (MA02**2 + MA02*MHH2)*MHp2

	AllHix(1003) = MA02**2 + 10*MA02*MHH2 - 2*MHH2**2

	AllHix(101) = MHH2**2*MHp2 + MA02*MHp2**2

        AllHix(1018) =
     &   2*MA02*(MHH2 - MHp2) + (2*MA02 - MHH2)*(MA02 - MHp2)*SBA2

        AllHix(1060) =
     &   -(CAB*(2*m12sq - Mh02*S2B)) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*(1 - 3*SBA2))

        AllHix(1066) =
     &   2*m12sq*(MA02 - Mh02)*(3*S2A - S2B) + S2B*AllHix(1065)

        AllHix(1067) =
     &   -2*(1 - 3*CBA2)*(MA02 - MHH2) + 3*(MA02 - Mh02)*SBA2

	AllHix(1087) = MA02 + ((MA02 - Mh02)*Mh02)/(4*Mh02 - MHH2)

	AllHix(1088) = 6*MA02*Mh02 - 2*(MA02 + Mh02)*MHH2 + MHH2**2

        AllHix(1092) =
     &   -2*(MA02 - Mh02)*(2*MA02 - MHH2)*S2B + S2A*AllHix(1091)

        AllHix(1094) =
     &   3*CBA2*MHH2 + MA02*(2 - 3*CBA2 - 6*SBA2) -
     &    2*Mh02*(1 - 3*SBA2)

        AllHix(110) = MA02**2*AllHix(104) + CBA2*AllHix(106) +
     &    MHp2*AllHix(108) - 2*MHp2**2*AllHix(109)

	AllHix(1107) = 3*MHH2*S2A + (2*MA02 - MHH2)*S2B

	AllHix(1108) = -m12sq - MHH2*S2B + 3*m12sq*SBA2

        AllHix(1117) =
     &   (4*MA02 - MHH2)*MHH2*S2A + (2*MA02 - MHH2)**2*S2B

        AllHix(1119) =
     &   2*Lambda7*MA02*SB2 +
     &    (CB2*Lambda6 - Lambda7*SB2)*AllHix(1118)

        AllHix(112) = 2*MHH2**4 -
     &    2*(MA02*MHH2 - MHH2**2)*MHp2**2 + MHp2*AllHix(111)

        AllHix(1121) =
     &   SAB*AllHix(1120) -
     &    (MW2*SW2)/(Alfa*Pi)*(CB2*Lambda6*MA02*SBA)

        AllHix(1127) =
     &   CBA*S2B*AllHix(1123) - 4*SAB*AllHix(1124) -
     &    2*MA02*AllHix(1126) +
     &    (2*MW2*SW2)/(Alfa*Pi)*(SBA*AllHix(1125))

        AllHix(1130) =
     &   MHH2**2*(3*S2A - 5*S2B) - 6*MA02*MHH2*(2*S2A - 3*S2B) -
     &    16*MA02**2*S2B

	AllHix(1132) = MHH2*(2*MA02 + MHH2)*S2B - m12sq*AllHix(1131)

	AllHix(1133) = 4*MA02*(2 - 3*SBA2) - MHH2*(5 - 3*SBA2)

        AllHix(1135) =
     &   MHH2**2*(3*S2A - S2B) - 2*MA02*MHH2*(6*S2A - S2B) -
     &    4*MA02**2*S2B

        AllHix(114) = (MA02 + MHH2)*MHp2**3*S2B**2 +
     &    CBA*S2B*SAB*AllHix(113)

        AllHix(1140) =
     &   2*m12sq*SAB*AllHix(1136) - 2*MA02*MHH2*AllHix(1137) -
     &    4*MA02**2*AllHix(1138) - MHH2**2*AllHix(1139)

        AllHix(116) = MA02**3 - 3*MA02**2*MHH2 +
     &    3*MA02*MHH2**2 + 11*MHH2**3

        AllHix(1164) =
     &   CBA*AllHix(1163) +
     &    2*((2*m12sq - MHH2*S2B)*SAB +
     &       (MW2*SW2)/(Alfa*Pi)*
     &        ((1 - 3*CBA2)*(CB2*Lambda6 - Lambda7*SB2)*SBA))

	AllHix(118) = 2*C2B*CBA*m12sq - CAB*CBA2*Mh02*S2B

	AllHix(1188) = (MA02*MHH2 + 2*AllHix(1187))/(4*MA02 - MHH2)

	AllHix(1195) = Mh02 + 2*(MA02 + 1/2.D0*Mh02)

        AllHix(120) = 2*MHH2**3 - 2*(MA02 - MHH2)*MHp2**2 -
     &    AllHix(90) + MHp2*AllHix(119)

	AllHix(1205) = 1

        AllHix(1218) =
     &   2*(MA02 + MHp2)*SB2*SBA +
     &    (MW2*SW2)/(Alfa*Pi)*(CBA*Lambda6)

        AllHix(1220) =
     &   2*CBA*(MA02 + MHp2)*SB2*SBA +
     &    (MW2*SW2)/(Alfa*Pi)*(CBA2*Lambda6)

	AllHix(1226) = 2*MA02*Mh02 - Mh02**2 - 2*(3*MA02 - Mh02)*MHp2

	AllHix(1227) = MA02**2 - (5*MA02 - Mh02)*MHp2

        AllHix(1239) =
     &   2*MHp2*S2B**2*SBA2 +
     &    (3*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHix(1242) =
     &   -5*MHp2*S2B**4*SBA2 +
     &    (2*MW2*SW2)/(Alfa*Pi)*(CBA*SBA*AllHix(1241))

        AllHix(1245) =
     &   -9*MHp2*S2B**4*SBA2 +
     &    (2*MW2*SW2)/(Alfa*Pi)*(CBA*SBA*AllHix(1244))

	AllHix(1248) = 9*Mh02*S2B**4*SBA2 + 8*AllHix(1247)

        AllHix(1250) =
     &   -4*Mh02**2*S2B**4*SBA2 - 2*MHp2*AllHix(1242) +
     &    Mh02*AllHix(1249)

	AllHix(1254) = 2*MA02**2*(Mh02 - 4*MHp2) + MA02*AllHix(1253)

	AllHix(1257) = S2B**2*SBA*AllHix(1255) - 2*Mh02*AllHix(1256)

        AllHix(1270) =
     &   -32*MA02**4*S2B**2*SBA + 4*MA02**3*AllHix(1267) +
     &    MA02**2*AllHix(1269)

        AllHix(1278) =
     &   256*MHp2**4*AllHix(1275) - 32*Mh02*MHp2**3*AllHix(1276) -
     &    Mh02**4*AllHix(1277)

        AllHix(1284) =
     &   1024*MHp2**4*AllHix(1275) -
     &    32*Mh02*MHp2**3*AllHix(1282) - Mh02**4*AllHix(1283)

        AllHix(1286) =
     &   -3*(5*Mh02 - MHp2)*S2B**2*SBA2 +
     &    (16*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

	AllHix(1292) = MA02**2*AllHix(1289) - MA02**3*AllHix(1291)

        AllHix(1297) =
     &   (CBA2*(Mh02 - 2*MHp2))/(MA02 - Mh02) + MHp2/(MA02 - MHp2)

	AllHix(132) = 2*CAB*S2B - S2B**2*SBA

	AllHix(1328) = 99*S2A**2 + 28*S2A*S2B + 16*S2B**2

	AllHix(1332) = 27*S2A**2 - 42*S2A*S2B + 8*S2B**2

        AllHix(1338) =
     &   Mh02**2*AllHix(1335) - Mh02*MHp2*AllHix(1336) -
     &    4*MHp2**2*AllHix(1337)

        AllHix(1339) =
     &   (32 + 10*CBA2 + 75*CBA2**2)*Mh02**2 +
     &    (32 + 46*CBA2 - 267*CBA2**2)*Mh02*MHp2 -
     &    4*(16 + 14*CBA2 + 33*CBA2**2)*MHp2**2

        AllHix(1340) =
     &   (32 + 85*CBA2)*Mh02**2 + (32 - 221*CBA2)*Mh02*MHp2 -
     &    4*(16 + 47*CBA2)*MHp2**2

        AllHix(1342) =
     &   3*Mh02*MHp2*(3*S2A - 2*S2B) + 4*MHp2**2*S2B +
     &    Mh02**2*(27*S2A + 2*S2B)

        AllHix(1349) =
     &   Mh02**2*S2B*(27*S2A + 2*S2B)*SBA - Mh02*AllHix(1347) +
     &    4*MHp2*AllHix(1348)

	AllHix(1360) = Mh02**2 - (5*Mh02 - MHH2)*MHp2

	AllHix(1366) = Mh02 - ((Mh02 - MHp2)*MHp2)/(MHH2 - 4*MHp2)

        AllHix(1373) =
     &   -2*(Mh02 - 2*MHp2)*(MHH2 - MHp2)*S2B + S2A*AllHix(1372)

        AllHix(141) = -((10 - CBA2)*MA02**2*MHH2**2) +
     &    MHH2*AllHix(135) - CBA2*AllHix(136) + MHp2*AllHix(138) -
     &    2*MHp2**2*AllHix(139) + MA02*AllHix(140)

	AllHix(1414) = 2*MHp2*S2B - Mh02*(3*S2A + S2B)

        AllHix(1419) =
     &   4*(Mh02 - MHp2)**2*(2*m12sq - Mh02*S2B) -
     &    CBA2*(2*m12sq*AllHix(1417) - Mh02*S2B*AllHix(1418))

        AllHix(1420) =
     &   (Mh02 - 2*MHp2)*S2B**2*SBA -
     &    (2*MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

        AllHix(1423) =
     &   (Mh02 - 2*MHp2)*S2B*SBA*AllHix(1421) -
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*AllHix(1422))

        AllHix(1428) =
     &   2*(Mh02 - MHp2)**2*(2*m12sq - Mh02*S2B) +
     &    CBA2*MHp2*AllHix(1427)

        AllHix(1431) =
     &   CBA2*AllHix(1430) +
     &    (6*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*CBA2**2*(Mh02 - 4*MHp2)*
     &       (CB2*Lambda6 - Lambda7*SB2))

	AllHix(1433) = 3*Mh02**2 - 14*Mh02*MHp2 + 8*MHp2**2

        AllHix(144) = -2*MA02*MHH2**3 +
     &    2*(MA02*MHH2 - MHH2**2)*MHp2**2 + AllHix(142) -
     &    MHp2*AllHix(143)

        AllHix(1441) =
     &   2*((Mh02 - MHp2)**2*AllHix(1420) + CAB*AllHix(1436)) +
     &    CBA2*MHp2*AllHix(1440)

        AllHix(146) = 2*MHH2**3 + 3*MHH2**2*MHp2 -
     &    3*MHH2*MHp2**2 + MHp2**3

	AllHix(1465) = Mh02**2 - 8*Mh02*MHp2 + 12*MHp2**2

        AllHix(148) = -(MA02**2*MHH2) + 2*MA02*MHH2**2 -
     &    MHH2**3 - (MA02 - MHH2)*
     &     (-((MA02 + MHH2)*MHp2) + 2*MHp2**2)

        AllHix(1490) =
     &   -2*CAB*(2*m12sq - Mh02*S2B) + SBA*AllHix(1406) +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*(1 - 3*SBA2))

        AllHix(1495) =
     &   2*CBA2*Mh02*(MHH2 - MHp2) +
     &    (2*Mh02 - MHH2)*(Mh02 - MHp2)*SBA2

        AllHix(1502) =
     &   CBA*AllHix(1501) +
     &    2*((2*m12sq - MHH2*S2B)*SAB +
     &       (MW2*SW2)/(Alfa*Pi)*
     &        ((1 - 3*CBA2)*(CB2*Lambda6 - Lambda7*SB2)*SBA))

	AllHix(1505) = Mh02*(5*MHH2 + MHp2) - 2*MHH2*(MHH2 + 2*MHp2)

	AllHix(1524) = 4*MHp2**2 + 2*MHp2*(Mh02 + MHp2)

	AllHix(1534) = MHH2 + MHp2 + 1/2.D0*(Mh02 + MHH2 + MHp2)

	AllHix(1547) = MHH2*(3*S2A - S2B) + 2*MHp2*S2B

	AllHix(1550) = -4*(MHH2 - 2*MHp2)*SAB + CBA*AllHix(1549)

	AllHix(1553) = 3*S2A*SBA2 - 2*S2B*(2 + SBA2)

        AllHix(1555) =
     &   MHp2**2*S2B*(2 + 5*SBA2) + MHH2*MHp2*AllHix(1554)

        AllHix(1561) =
     &   SAB*AllHix(1558) - MHH2**2*AllHix(1559) +
     &    2*MHH2*MHp2*AllHix(1560) -
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (MHp2**2*(CB2*Lambda6 - Lambda7*SB2)*SBA*
     &       (2 + 5*SBA2 - 12*SBA2**2))

        AllHix(1564) =
     &   -3*(MHH2 - MHp2)*(MHH2 - MHp2*(1 - SBA2)) +
     &    (MHH2 - MHp2)**2*(5 + 2*SBA2)

        AllHix(1571) =
     &   3*MHH2*(MHH2 - 4*MHp2)*S2A -
     &    (4*MHH2 - 7*MHp2)*(MHH2 - 2*MHp2)*S2B

	AllHix(1573) = 2*MHH2**2 + MHp2**2*(2 - (7 - 12*SBA2)*SBA2)

	AllHix(1576) = MHH2**2*S2B*AllHix(1574) - 2*m12sq*AllHix(1575)

        AllHix(1577) =
     &   MHp2*S2B*SAB*(2 + 5*SBA2) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*
     &       (4 - (4 - 3*SBA2)*SBA2))

        AllHix(1580) =
     &   3*MHH2*(MHH2 - 4*MHp2)*S2A -
     &    (7*MHH2 - 10*MHp2)*(MHH2 - 2*MHp2)*S2B

        AllHix(1583) =
     &   -2*MHH2**2*S2B*(MHH2 - MHp2*(2 - SBA2)) +
     &    m12sq*AllHix(1582)

	AllHix(1584) = 2*MHH2**2 + MHp2**2*(2 - (5 - 6*SBA2)*SBA2)

        AllHix(1585) =
     &   2*MHp2*S2B*SAB*(2 + SBA2) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*
     &       (8 - (7 - 3*SBA2)*SBA2))

        AllHix(1587) =
     &   3*(MHH2 - MHp2)*(MHH2 - MHp2*(1 - SBA2)) +
     &    (MHH2 - MHp2)**2*(5 + 2*SBA2)

	AllHix(160) = -3*MHp2 + MHH2*(2 - SBA2)

	AllHix(161) = 2*MHp2**2 - MHH2*MHp2*(3 - SBA2) + MHH2**2*SBA2

	AllHix(162) = MHH2**2 + 2*MHp2**2 - MHH2*MHp2*(2 + SBA2)

        AllHix(1624) =
     &   (2*Mh02 - MHH2)*(Mh02 - MHp2)*SBA2 - Mh02*AllHix(1623)

	AllHix(1639) = Mh02*S2A*SBA2 + MHH2*(S2B + S2A*(3 + 2*SBA2))

	AllHix(1643) = 9*(Mh02 + MHH2)*S2A + 5*(Mh02 - MHH2)*S2B

        AllHix(1645) =
     &   2*MA02**2 - MA02*(Mh02 + MHH2) -
     &    2*(Mh02 + MHH2 - 2*MHp2)*MHp2

	AllHix(1646) = 9*S2A**2 - S2B**2

	AllHix(1647) = 2*(Mh02 - MHH2)**2*S2A**2 + 3*Mh02*MHH2*S2B**2

        AllHix(1652) =
     &   Mh02*S2B*AllHix(1650) -
     &    2*m12sq*(9*Mh02*S2A + S2B*AllHix(1651))

        AllHix(1654) =
     &   -2*(1 + 3*CBA2**2)*m12sq + (1 + 3*CBA2)*Mh02*S2B

        AllHix(1656) =
     &   S2A*(2*CBA2**2 + 3*CBA2*SBA2) + SBA2*(S2B + S2A*SBA2)

        AllHix(1657) =
     &   -3*MHH2*(2*MHH2*S2A + Mh02*(S2A + S2B))*SAB*SBA +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     ((MA02 + 2*MHp2)*(CB2*Lambda6 - Lambda7*SB2)*
     &       (CBA2 - SBA2))

        AllHix(1659) =
     &   -2*(MA02 + 2*MHp2)*S2B**2*SAB*SBA +
     &    (3*MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*AllHix(1658))

        AllHix(1664) =
     &   MHH2*(Mh02 + 2*MHH2)*S2A + 2*(Mh02 - 2*MHp2)*MHp2*S2B

        AllHix(1668) =
     &   (Mh02 - MHH2)*(2*Mh02 + 3*MHH2)*S2A + S2B*AllHix(1667)

	AllHix(1670) = MA02 + 2*MHp2 + 3*(CBA2*Mh02 + MHH2*SBA2)

	AllHix(1673) = (Mh02 + 2*MHH2)*S2A*S2B - 2*m12sq*(3*S2A + S2B)

        AllHix(1691) =
     &   CBA*(Mh02 - MHH2)*S2B**2 - 2*(2*m12sq - MHH2*S2B)*SAB

	AllHix(170) = 4*CAB*SBA - S2B*(1 + CBA2 - 2*SBA2) + 3*S2A*SBA2

	AllHix(1701) = CBA*(2*MA02 - MHH2)*S2B + MHH2*SAB

        AllHix(1702) =
     &   CBA*(2*MA02 - MHH2)*S2B**2 - 2*(2*m12sq - MHH2*S2B)*SAB

        AllHix(1714) =
     &   2*(MA02 - MHp2)**2 + CBA2*AllHix(1712) + SBA2*AllHix(1713)

	AllHix(1718) = (MA02 - Mh02)**2 + 2*(Mh02 - MHp2)**2

        AllHix(1721) =
     &   Mh02*S2A*S2B*SBA +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*CBA2*(CB2*Lambda6 - Lambda7*SB2))

        AllHix(1722) =
     &   CAB**2*(2*CBA2*m12sq - Mh02*S2B)**2 +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (CBA*CBA2*Mh02*S2A*S2B*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHix(1724) =
     &   -2*m12sq*(3*S2A - S2B) + (2*Mh02 + MHH2)*S2A*S2B

	AllHix(1729) = MA02**2*Mh02**2 - Mh02**4

	AllHix(173) = S2B + CBA2*S2B + 4*CAB*SBA + 3*(S2A + S2B)*SBA2

	AllHix(1749) = (MA02 - MHH2)**2 + 2*(MHH2 - MHp2)**2

	AllHix(1752) = MHH2*S2B*(CBA*S2A - SAB) + 2*m12sq*SAB*SBA2

	AllHix(1753) = MHH2*S2B*(CBA*S2A - 2*SAB) + 4*m12sq*SAB*SBA2

	AllHix(1758) = MA02**2*MHH2**2 - MHH2**4

	AllHix(1783) = -Mh02**2 + 2*Mh02*MHp2 - MHp2**2

	AllHix(1785) = -MHH2**2 + 2*MHH2*MHp2 - MHp2**2

	AllHix(1797) = (MA02 - MHp2)**2 + CBA2*(Mh02 - MHp2)**2

	AllHix(1798) = (Mh02 - 2*MHp2)**2 + (MHH2 - MHp2)**2

	AllHix(1799) = CBA2*(MHH2 - 2*MHp2)**2 + SBA2*AllHix(1798)

	AllHix(1805) = (MHH2 - 2*MHp2)**2 + (Mh02 - MHp2)**2

	AllHix(186) = MHp2*S2B**2 + 4*CAB*(1 - 3*CBA2)*m12sq*SBA

        AllHix(187) = 3*Mh02**2*S2A +
     &    (Mh02 - MHp2)*(2*Mh02 + MHp2)*S2B

        AllHix(189) = -2*CAB*AllHix(188) -
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(1 - 3*CBA2)*(Mh02 - MHp2)*
     &       (CB2*Lambda6 - Lambda7*SB2))

	AllHix(196) = 3*(Mh02 - MHH2)*S2A + (Mh02 + MHH2)*S2B

        AllHix(199) = -2*SB2*AllHix(198) +
     &    (3*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*Lambda6*(Mh02 - MHH2)**3*S2B*SBA)

        AllHix(201) = (Mh02 + MHH2)*SB2*AllHix(200) -
     &    (3*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*Lambda6*(Mh02 - MHH2)**3*S2B*SBA)

        AllHix(210) = -2*MA02**4 -
     &    Mh02*(Mh02 - MHH2)*MHH2**2 -
     &    MA02**2*(4*Mh02 - MHH2)*(Mh02 + MHH2) +
     &    MA02**3*(5*Mh02 + MHH2) + MA02*AllHix(209)

	AllHix(225) = 4*CAB*SBA - S2B*(9 + 2*SBA2)

	AllHix(226) = -2*MHp2**3 + Mh02*MHp2*(MHp2 + 2*MHH2*SBA2)

        AllHix(227) = MHH2*S2B*SBA2 +
     &    MHp2*(S2B - 2*CAB*SBA + S2B*SBA2)

	AllHix(229) = 2*CAB*SBA + S2B*(2 - SBA2)

	AllHix(230) = 2*CAB*SBA - S2B*(2 + SBA2)

        AllHix(232) = MHH2*MHp2**3*S2B*SBA2 -
     &    Mh02*MHp2**2*(2*MHH2*S2B*SBA2 - MHp2*AllHix(231))

	AllHix(234) = 2*CAB*SBA - S2B*(8 + SBA2)

	AllHix(235) = 2*CAB*SBA - S2B*(6 + SBA2)

        AllHix(237) = 2*MHH2*S2B*SBA2 +
     &    MHp2*(S2B - 2*CAB*SBA + S2B*SBA2)

	AllHix(238) = -8*CAB*SBA + S2B*(3 + 4*SBA2)

	AllHix(239) = 2*CAB*SBA - S2B*(3 + SBA2)

	AllHix(240) = Mh02**4 + MHp2**3*(MHp2 + MHH2*SBA2)

        AllHix(244) = MHH2*S2B*SAB*SBA2 -
     &    (MW2*SW2)/(Alfa*Pi)*((CB2*Lambda6 - Lambda7*SB2)*SBA)

	AllHix(246) = MHp2*S2B**2*SBA2 + 2*CBA*AllHix(244)

	AllHix(248) = MHp2*S2B**2*SBA2 + 4*CBA*AllHix(244)

	AllHix(253) = MA02*Mh02 - (MA02 + Mh02)*MHH2 - 2*MHH2**2

        AllHix(254) = MA02**4 + 4*MA02**2*Mh02*MHH2 +
     &    2*Mh02**2*MHH2**2 - 2*MA02**3*(Mh02 + MHH2)

        AllHix(260) = 2*Mh02**3 - 3*Mh02**2*MHH2 +
     &    Mh02*MHH2**2 - 2*MHH2**3

        AllHix(265) = (Mh02 + MHH2)*S2A*S2B -
     &    (6*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

	AllHix(270) = MA02**3*Mh02 + MA02**2*Mh02**2

	AllHix(28) = -Mh02**2 + MA02*(3*Mh02 - 2*MHp2)

        AllHix(284) = 4*MA02**4*Mh02 + MA02**3*Mh02**2 -
     &    6*MA02**2*Mh02**3 - 3*MA02*Mh02**4

        AllHix(285) = 4*MA02**3*Mh02 - MA02**2*Mh02**2 -
     &    8*MA02*Mh02**3 - Mh02**4

	AllHix(286) = 2*MA02**2*Mh02 - 5*MA02*Mh02**2 - Mh02**3

        AllHix(291) = MA02**3*Mh02**3 - 2*MA02**2*Mh02**4 +
     &    MA02*Mh02**5 + (MA02*Mh02 - Mh02**2)*MHH2**4 -
     &    MHH2*AllHix(288) + MHH2**2*AllHix(289) -
     &    MHH2**3*AllHix(290)

	AllHix(296) = -(MA02**3*Mh02) - Mh02**4 + AllHix(295)

	AllHix(297) = MA02**2*Mh02 - 2*MA02*Mh02**2 + Mh02**3

	AllHix(299) = 3*MA02**2*Mh02**2 - 5*MA02*Mh02**3

	AllHix(300) = MA02**2*Mh02**3 + MA02**3*Mh02*MHH2

	AllHix(312) = (Mh02 + 2*MHH2)*S2A + (2*MA02 - Mh02)*S2B

        AllHix(329) = 7*MA02**3*Mh02 - 8*MA02**2*Mh02**2 +
     &    3*MA02*Mh02**3 + MHH2*AllHix(328)

        AllHix(330) = -2*MA02**4 + 7*MA02**3*Mh02 -
     &    8*MA02**2*Mh02**2 + 3*MA02*Mh02**3 + MHH2*AllHix(328)

        AllHix(332) = -2*MA02**4 - 3*Mh02*MHH2**3 +
     &    MA02**3*(2*Mh02 + 7*MHH2) -
     &    MA02**2*(7*Mh02*MHH2 + 8*MHH2**2) + MA02*AllHix(331)

        AllHix(334) = CBA2*(MA02 - Mh02)*Mh02 -
     &    (MA02 - MHH2)*MHH2*SBA2

        AllHix(340) = 2*MA02**2 + Mh02**2 + MHH2**2 -
     &    2*MA02*(Mh02 + MHH2)

	AllHix(357) = CB2**2*Lambda6**2 + Lambda7**2*SB2**2

	AllHix(372) = 2*m12sq*(2*MA02 - Mh02) + Mh02*MHH2*S2B

	AllHix(377) = MA02**3 + 11*Mh02**3 + 3*Mh02**2*(MA02 - 4*MHp2)

	AllHix(378) = 3*MA02**2*MHp2 - 4*MHp2**3

	AllHix(38) = (2*MA02)/(MA02 - MHH2)**2 + 1/(MA02 - MHp2)

        AllHix(381) = 2*Mh02**3*(2*CAB*SBA - S2B*SBA2) -
     &    2*(MHp2**2*S2B*(MHp2 - MHH2*SBA2) +
     &       Mh02*MHp2*(2*MHH2*S2B*SBA2 - MHp2*AllHix(229))) +
     &    Mh02**2*AllHix(380)

        AllHix(384) = MHp2*S2B*(4*MHp2 + MHH2*SBA2) -
     &    Mh02**2*AllHix(382) -
     &    Mh02*(MHH2*S2B*SBA2 - MHp2*AllHix(383))

        AllHix(386) = 2*MHp2**2*S2B*(MHp2 + MHH2*SBA2) +
     &    Mh02**3*(2*CAB*SBA - S2B*SBA2) -
     &    Mh02*MHp2*(3*MHH2*S2B*SBA2 - 2*MHp2*AllHix(230)) +
     &    Mh02**2*(MHH2*S2B*SBA2 - MHp2*AllHix(385))

        AllHix(389) = 17*Mh02**3 - 9*Mh02**2*MHp2 +
     &    18*Mh02*MHp2**2 - 6*MHp2**3

        AllHix(390) = 9*Mh02**3 + MA02**2*(5*Mh02 - 9*MHp2) -
     &    25*Mh02**2*MHp2 - 10*Mh02*MHp2**2 + 14*MHp2**3

        AllHix(394) = MHp2*S2B*(9*MHp2 - MHH2*SBA2) +
     &    Mh02**2*AllHix(392) +
     &    Mh02*(MHH2*S2B*SBA2 - MHp2*AllHix(393))

        AllHix(396) = 2*MHp2**2*S2B*(MHp2 - MHH2*SBA2) +
     &    Mh02*MHp2*(MHH2*S2B*SBA2 - 2*MHp2*AllHix(231)) +
     &    Mh02**3*AllHix(382) + Mh02**2*AllHix(395)

	AllHix(397) = 5*Mh02**3 + 2*MHp2**2*(3*MHp2 - MHH2*SBA2)

        AllHix(399) = 2*Mh02**2*AllHix(230) +
     &    Mh02*(2*MHH2*S2B*SBA2 + MHp2*AllHix(398))

	AllHix(401) = 11*MHp2*S2B**2*SBA2 + 2*CBA*AllHix(244)

	AllHix(402) = 11*MHp2*S2B**2*SBA2 + 8*CBA*AllHix(244)

        AllHix(409) = 2*Mh02**3 + 3*Mh02**2*MHp2 -
     &    3*Mh02*MHp2**2 + MHp2**3

        AllHix(413) = MHp2*S2B*(10*MHp2 - MHH2*SBA2) +
     &    Mh02**2*AllHix(411) +
     &    Mh02*(MHH2*S2B*SBA2 - MHp2*AllHix(412))

        AllHix(416) = -2*Mh02*MHp2**2*AllHix(239) +
     &    Mh02**3*AllHix(414) - S2B*AllHix(415)

        AllHix(418) = -2*MHH2*MHp2**2*S2B*SBA2 +
     &    Mh02**3*(2*CAB*SBA - S2B*SBA2) + Mh02**2*AllHix(395) +
     &    Mh02*MHp2*AllHix(417)

        AllHix(421) = Mh02**2 + 2*MA02*(MA02 + Mh02) -
     &    2*(3*MA02 + 2*Mh02)*MHp2 + 5*MHp2**2 +
     &    (Mh02 - MHH2)*(MA02 - MHp2)*SBA2

	AllHix(442) = MHp2*S2B**2 + MHH2*S2B*(S2A + 4*CBA*SAB)

	AllHix(446) = 2*S2A + 3*S2B - 6*CBA*SAB - 2*CAB*SBA

        AllHix(447) = (3*MA02 + 2*MHp2)*S2B +
     &    2*MHH2*(S2A + 4*CBA*SAB) - 2*MA02*(3*CBA*SAB + CAB*SBA)

	AllHix(450) = 4*(MA02 - Mh02)*Mh02 + SBA2*AllHix(449)

        AllHix(451) = 8*CAB*Mh02*SBA -
     &    (3*MA02 - 2*Mh02 - MHH2)*S2A*SBA2

        AllHix(453) = -2*
     &     (CAB*MA02*Mh02*S2B + m12sq*AllHix(452)) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(MA02 - 2*Mh02)*(CB2*Lambda6 - Lambda7*SB2))

        AllHix(459) = SBA2*AllHix(458) -
     &    (12*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

	AllHix(461) = 16*CAB*SBA + S2B*(2 - 11*SBA2) - 2*S2A*SBA2

        AllHix(462) = -2*(4*CAB + C2B*CBA)*m12sq +
     &    (5*MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

        AllHix(463) = MHH2*(S2A*S2B + 7*S2B**2)*SBA2 -
     &    2*SBA*AllHix(462)

	AllHix(464) = 2*S2A*SBA2 + S2B*(2 + 3*SBA2)

        AllHix(465) = -2*(2*CAB + C2B*CBA)*m12sq +
     &    (3*MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

        AllHix(466) = MHH2*(S2A*S2B + 2*S2B**2)*SBA2 -
     &    2*SBA*AllHix(465)

	AllHix(467) = 16*CAB*SBA + S2B*(10 + 3*SBA2)

	AllHix(487) = MHp2*S2B + CBA*MHH2*SAB + CAB*Mh02*SBA

	AllHix(49) = MHH2**2 - 2*MHp2**2 + MHH2*MHp2*(3 - 2*SBA2)

	AllHix(491) = 4*m12sq - MHH2*S2A - MA02*S2B

	AllHix(493) = MHp2*(S2A - 2*CBA2*S2B) - MHH2*AllHix(492)

	AllHix(500) = S2A + 10*CAB*SBA + (S2A - 5*S2B)*SBA2

	AllHix(502) = S2A - 2*CBA*SAB + 3*(S2A - S2B)*SBA2

	AllHix(503) = 2*S2B + S2A*(2 + SBA2) - 4*(CBA*SAB + S2B*SBA2)

        AllHix(508) = -4*m12sq*SAB*SBA2 +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*(4 - 3*SBA2))

	AllHix(510) = 8*CBA*SAB - S2B*(9 + 5*SBA2)

	AllHix(511) = 2*CBA*SAB + S2B*(14 + 3*SBA2)

        AllHix(512) = m12sq*SAB +
     &    (MW2*SW2)/(Alfa*Pi)*((CB2*Lambda6 - Lambda7*SB2)*SBA)

	AllHix(523) = MHH2**2*(3*S2A - 2*S2B) + MHp2*(MHH2 + MHp2)*S2B

	AllHix(534) = 6*MHH2*S2A + (2*MA02 - MHH2)*S2B

        AllHix(547) = Mh02**4*MHH2 - Mh02**3*MHH2**2 -
     &    Mh02**2*MHH2**3 + Mh02*MHH2**4 - MHp2*AllHix(543) -
     &    2*MHp2**2*AllHix(545) - MHp2**3*AllHix(546)

        AllHix(551) = Mh02**4*MHH2 - Mh02**3*MHH2**2 -
     &    Mh02**2*MHH2**3 + 4*(Mh02 - 2*MHH2)*MHp2**4 -
     &    MHp2*AllHix(548) + MHp2**2*AllHix(549) -
     &    MHp2**3*AllHix(550)

        AllHix(557) = -3*Mh02**3*MHH2 +
     &    2*(Mh02 - MHH2)*MHp2**3 + MHp2*AllHix(554) -
     &    MHp2**2*AllHix(555) + 3*AllHix(556)

        AllHix(563) = (Mh02 - MHp2)/(MHH2 - MHp2) +
     &    (MHH2 - MHp2)/(Mh02 - MHp2)

        AllHix(584) = 6*m12sq*(MA02 - Mh02) -
     &    (3*MA02 - 4*Mh02)*Mh02*S2B

        AllHix(589) = 16*m12sq*(MA02 - Mh02)*(MA02 - MHH2)*
     &     (2*MA02 - Mh02 - MHH2) + 12*MA02**4*S2B -
     &    Mh02*MHH2*AllHix(585) + 8*MA02**3*AllHix(586) +
     &    MA02*AllHix(587) - MA02**2*AllHix(588)

        AllHix(593) = 2*m12sq*AllHix(590) -
     &    2*Mh02*AllHix(591) + MA02*AllHix(592)

        AllHix(597) = -8*CAB*(2*m12sq - Mh02*S2B)*SBA +
     &    SBA2*AllHix(596)

        AllHix(601) = MA02*AllHix(598) +
     &    2*(m12sq*AllHix(599) - MHH2*AllHix(600))

	AllHix(609) = Mh02**4 + Mh02**3*MHH2 - Mh02*MHH2**3 - MHH2**4

	AllHix(611) = Mh02**4 - 9*Mh02**2*MHH2**2 + MHH2**4

	AllHix(612) = -4*(Mh02**3 + MHH2**3) + 23*AllHix(544)

	AllHix(613) = 53*Mh02*MHH2 + 8*(Mh02**2 + MHH2**2)

        AllHix(623) = 3*Mh02**3*(MHH2 - MHp2) -
     &    2*MHH2*MHp2**3 + 2*MHp2**4 -
     &    8*Mh02**2*(MHH2*MHp2 - MHp2**2) + 7*Mh02*AllHix(622)

        AllHix(625) = -3*MHH2**3*MHp2 +
     &    (7*Mh02*MHH2 + 8*MHH2**2)*MHp2**2 -
     &    (2*Mh02 + 7*MHH2)*MHp2**3 + 2*MHp2**4 + Mh02*AllHix(624)

        AllHix(633) = 16*m12sq*(Mh02 + MHH2 - 2*MHp2)*
     &     (Mh02 - MHp2)*(MHH2 - MHp2) + Mh02**2*AllHix(629) +
     &    Mh02**3*AllHix(630) - Mh02*AllHix(631) - MHp2*AllHix(632)

        AllHix(634) = 6*m12sq*(Mh02 - MHp2) -
     &    Mh02*(4*Mh02 - 3*MHp2)*S2B

        AllHix(636) = 2*Mh02*AllHix(591) - MHp2*AllHix(592) -
     &    2*m12sq*AllHix(635)

        AllHix(638) = 8*m12sq*(3*S2A - S2B) + 11*MHp2*S2B**2 -
     &    Mh02*S2B*(8*S2A + 3*S2B) - 4*MHH2*(S2A*S2B + S2B**2) +
     &    (24*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

	AllHix(645) = (MHH2 - MHp2)*AllHix(641) + 4*CBA*AllHix(644)

	AllHix(654) = -4*(Mh02 - MHH2)*S2A + 7*MHH2*S2B

	AllHix(656) = 4*CBA*SAB - S2A*(2 - SBA2) - 7*S2B*SBA2

        AllHix(657) = -6*CBA*SAB - 8*CAB*SBA +
     &    S2A*(2 - SBA2) + S2B*(3 + 4*SBA2)

	AllHix(659) = S2A - 2*CBA*SAB - (3*S2A - 4*S2B)*SBA2

        AllHix(660) = 6*CBA*SAB + 14*CAB*SBA -
     &    2*S2A*(1 - 3*SBA2) - S2B*(3 + 7*SBA2)

        AllHix(666) = -4*m12sq*SAB*SBA2 +
     &    (3*MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*(1 - SBA2))

	AllHix(668) = 8*CBA*SAB - S2B*(3 + 2*SBA2)

	AllHix(674) = -4*(Mh02 - 7*MHH2)*S2A + 25*MHH2*S2B

	AllHix(676) = 4*S2A - 8*CBA*SAB - (52*S2A - 27*S2B)*SBA2

        AllHix(677) = 6*CBA*SAB + 14*CAB*SBA -
     &    2*S2A*(1 - SBA2) - S2B*(3 + 7*SBA2)

	AllHix(679) = 2*S2A - 4*CBA*SAB - (12*S2A - 5*S2B)*SBA2

        AllHix(680) = 2*S2A - 6*CBA*SAB - 14*CAB*SBA +
     &    S2B*(3 + 7*SBA2)

	AllHix(685) = 32*CBA*SAB - S2B*(9 + 8*SBA2)

        AllHix(686) = m12sq*(SAB + 6*SAB*SBA2) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*(1 + 3*SBA2))

        AllHix(691) = 2*MHp2*S2B + Mh02*AllHix(677) -
     &    2*MHH2*AllHix(690)

        AllHix(694) = 4*MHp2*S2B + MHH2*AllHix(692) +
     &    2*Mh02*AllHix(693)

        AllHix(695) = 2*S2A - 6*CBA*SAB - 26*CAB*SBA +
     &    S2B*(3 + 13*SBA2)

	AllHix(697) = 6*MA02 - MHH2*(3 - SBA2)

        AllHix(698) = MHH2*S2B**2*SBA2*(1 + 2*SBA2) +
     &    2*CBA*AllHix(666)

        AllHix(700) = MHH2*S2B*(S2B + 4*CBA*SAB)*SBA2 -
     &    2*CBA*AllHix(699)

	AllHix(703) = MHp2*S2B + MHH2*(S2A - 2*CBA*SAB + 3*S2A*SBA2)

        AllHix(705) = Mh02*AllHix(677) +
     &    2*(MHp2*S2B + MHH2*AllHix(704))

	AllHix(709) = 8*CBA*SAB - S2B*(2 + 3*SBA2)

	AllHix(711) = -2*C2B*m12sq*SBA*(3 - SBA2) + AllHix(710)

	AllHix(725) = 10*S2B - 88*CAB*SBA - (52*S2A + 23*S2B)*SBA2

        AllHix(726) = -((CAB + C2B*CBA)*m12sq) +
     &    (MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

        AllHix(729) = 10*CBA*MHH2*SAB +
     &    Mh02*(S2A + 2*CAB*SBA - S2B*SBA2)

	AllHix(736) = MHp2*(17*S2B - 12*CAB*SBA) + MHH2*S2A*SBA2

	AllHix(739) = 2*CAB*SBA - S2A*SBA2 - S2B*(19 + 2*SBA2)

	AllHix(74) = 2*MA02**2 - (6*MA02 - Mh02)*MHH2

	AllHix(740) = MHp2*(3 - 4*SBA2) + 9*MHH2*SBA2

        AllHix(745) = SBA2*(MA02*S2B**2 + S2B*AllHix(744)) +
     &    (8*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHix(746) = MA02*S2B**2 +
     &    MHH2*S2B*(S2A - 18*CBA*SAB + 2*S2B*SBA2)

        AllHix(747) = MHp2*S2B**2*SBA2 -
     &    (8*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHix(751) = Mh02**2*(S2A + 2*CAB*SBA - S2B*SBA2) +
     &    MHp2*AllHix(750)

        AllHix(752) = MA02*S2B +
     &    MHH2*(S2A - 10*CBA*SAB + 2*S2B*SBA2) + MHp2*AllHix(743)

        AllHix(756) = Mh02*(2*Mh02 + MHH2 - 3*MHp2)*S2A -
     &    S2B*AllHix(755)

        AllHix(758) = -(CAB*Mh02*(5*Mh02 - 2*MHp2)*S2B) -
     &    2*m12sq*AllHix(757) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(2*Mh02 - MHp2)*(CB2*Lambda6 - Lambda7*SB2))

	AllHix(762) = S2B*(18 + 7*SBA2) - 4*(4*CAB*SBA + S2A*SBA2)

        AllHix(764) = -8*SBA*AllHix(726) +
     &    S2B*(2*MHH2*S2A*SBA2 - MHp2*AllHix(763))

	AllHix(781) = MHH2**4 + Mh02**3*(MHH2 - 3*MHp2)

	AllHix(812) = Mh02*(MHH2 - 6*MHp2) + 2*MHp2**2

	AllHix(818) = MHH2**2 + MHH2*MHp2 + 10*MHp2**2

        AllHix(819) = 2*MHH2**3 - MHH2**2*MHp2 -
     &    MHH2*MHp2**2 + 4*MHp2**3

        AllHix(820) = MHH2**3 + 3*MHH2**2*MHp2 -
     &    3*MHH2*MHp2**2 + 11*MHp2**3

        AllHix(829) = (CBA2*(Mh02 - MHH2 - MHp2))/MHH2 +
     &    ((MHH2 - MHp2)*(Mh02 - MHH2 + MHp2)*SBA2)/
     &     (Mh02*(Mh02 - MHp2))

        AllHix(836) = Mh02**3*(MHH2 - 2*MHp2) -
     &    Mh02**2*AllHix(833) - Mh02*MHp2*AllHix(834) +
     &    MHp2*AllHix(835)

	AllHix(840) = -4/(Mh02 - MHH2) + (1 - MHp2/Mh02)/MHH2

	AllHix(847) = Mh02**3*MHH2 + Mh02**2*MHH2**2

        AllHix(852) = 2*Mh02**5 + Mh02**4*MHH2 +
     &    12*Mh02**3*MHH2**2 - 5*Mh02**2*MHH2**3

        AllHix(853) = 8*Mh02**4 + 14*Mh02**3*MHH2 +
     &    Mh02**2*MHH2**2 - 2*Mh02*MHH2**3

	AllHix(854) = 15*Mh02**3 + 5*Mh02**2*MHH2 - 4*Mh02*MHH2**2

	AllHix(862) = Mh02**2*MHH2**2 + Mh02*MHH2**3

	AllHix(863) = -Mh02**3 - Mh02**2*MHH2 + 2*Mh02*MHH2**2

        AllHix(865) = 5*Mh02**3*MHH2**2 - 2*Mh02**2*MHH2**3 -
     &    3*Mh02*MHH2**4

        AllHix(866) = 2*Mh02**3*MHH2 + 3*Mh02**2*MHH2**2 -
     &    5*Mh02*MHH2**3

	AllHix(867) = Mh02**2*MHH2 - Mh02*MHH2**2

	AllHix(869) = CBA2*(Mh02 - MHp2) - MHH2*SBA2

        AllHix(872) = MHH2*(Mh02**4 + Mh02*MHp2**3) +
     &    2*AllHix(870) - AllHix(871)

        AllHix(875) = -3*Mh02**3*MHH2**2 + 3*Mh02**2*MHH2**3 +
     &    2*(Mh02**2 - Mh02*MHH2)*MHp2**3 + MHp2*AllHix(873) -
     &    MHp2**2*AllHix(874)

        AllHix(89) = -4*MA02**3*MHH2 + 7*MA02**2*MHH2**2 +
     &    4*MA02*MHH2**3 + 5*MHH2**4 - 2*(MA02 - 3*MHH2)*MHp2**3 +
     &    2*MHp2*AllHix(87) + MHp2**2*AllHix(88)

	AllHix(899) = 8*CBA*SAB + 23*S2B*SBA2 - 4*S2A*(1 + 13*SBA2)

	AllHix(900) = 2*S2A + 4*S2B - 4*CBA*SAB + 3*(4*S2A - S2B)*SBA2

        AllHix(904) = MHp2*S2B**2*SBA2*(9 + 11*SBA2) -
     &    4*CBA*AllHix(903)

        AllHix(907) = MHH2**2*S2B*AllHix(905) -
     &    2*MHH2*(4*CBA*AllHix(686) + MHp2*S2B*AllHix(906))

        AllHix(92) = -(MA02*MHH2**3) +
     &    MHH2**2*(MA02**2 - 2*MHp2**2) - MHp2*AllHix(90) +
     &    2*AllHix(91)

	AllHix(925) = 27*S2A**2 - 42*S2A*S2B + 7*S2B**2

	AllHix(926) = 6*(2 + 9*CBA2)*S2A - (11 + 42*CBA2)*S2B

        AllHix(928) = -3*Mh02*S2B*(89*S2A + 15*S2B)*SBA +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(47 - 267*CBA2)*(CB2*Lambda6 - Lambda7*SB2))

        AllHix(929) = Mh02*(66*S2A - 53*S2B)*S2B*SBA +
     &    (12*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(1 + 11*CBA2)*(CB2*Lambda6 - Lambda7*SB2))

        AllHix(930) = Mh02*(75*S2A - S2B)*S2B*SBA +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(1 + 75*CBA2)*(CB2*Lambda6 - Lambda7*SB2))

        AllHix(933) = Mh02*S2B*SBA*AllHix(926) -
     &    (12*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(1 - 4*CBA2 - 9*CBA2**2)*
     &       (CB2*Lambda6 - Lambda7*SB2))

	AllHix(94) = -3*MA02**3 + 25*MHH2**3 + 9*AllHix(90)

	AllHix(942) = 2*MA02*S2B - Mh02*(3*S2A + S2B)

	AllHix(946) = -(Mh02*(2*MA02 + Mh02)*S2B) + m12sq*AllHix(945)

        AllHix(947) = 16*MA02**2*S2B -
     &    6*MA02*Mh02*(2*S2A + 3*S2B) + Mh02**2*(3*S2A + 5*S2B)

	AllHix(948) = 4*(2 - 3*CBA2)*MA02 - (5 - 3*CBA2)*Mh02

        AllHix(950) = -4*(1 - 6*CBA2)*MA02**2 +
     &    2*(1 - 9*CBA2)*MA02*Mh02 - (1 - 3*CBA2)*Mh02**2

	AllHix(951) = 10*MA02**2 - 8*MA02*Mh02 + Mh02**2

        AllHix(952) = 4*MA02**2*S2B + Mh02**2*(3*S2A + S2B) -
     &    2*MA02*Mh02*(6*S2A + S2B)

        AllHix(953) = -4*(1 - 6*CBA2)*MA02**2 +
     &    2*(1 - 9*CBA2)*MA02*Mh02 - (1 - 3*CBA2)*Mh02**2

	AllHix(955) = 8*MA02**2 - 14*MA02*Mh02 + 3*Mh02**2

	AllHix(956) = CBA2**2*m12sq**2 - CBA2*m12sq*Mh02*S2B

        AllHix(958) = (2*MA02 + Mh02)*
     &     (m12sq**2 - m12sq*Mh02*S2B) + Mh02*S2B**2*AllHix(957)

        AllHix(966) = CBA2*Mh02*S2B*AllHix(965) +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*CBA2**2*m12sq*(CB2*Lambda6 - Lambda7*SB2))

        AllHix(971) = -2*m12sq*(2*MA02 + Mh02)*AllHix(967) +
     &    Mh02*S2B*AllHix(970)

	AllHix(973) = -2*(1 - 2*CBA2)*MA02 + (1 - CBA2)*Mh02

        AllHix(974) = (4*MA02 - Mh02)*Mh02*S2A -
     &    (2*MA02 - Mh02)**2*S2B

        AllHix(98) = 11*MA02**2*S2B +
     &    MHp2*(8*CAB*Mh02*SBA + S2B*AllHix(96)) -
     &    2*MA02*(2*CAB*Mh02*SBA + S2B*AllHix(97))

	AllHix(980) = -(MA02**4*S2B**4*SBA2) + CAB**2*AllHix(979)

        AllHix(989) = -(AllHix(967)*AllHix(988)) +
     &    (18*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*CBA2**2*MA02*(4*MA02 - Mh02)*
     &       (CB2*Lambda6 - Lambda7*SB2))

        AllHix(994) = 8*MA02**3*S2B**2*SBA +
     &    2*MA02**2*AllHix(990) + 4*Mh02**2*AllHix(991) +
     &    MA02*AllHix(993)

	AllHix(1000) = 2*MA02*MHH2 - MHH2**2 - 2*(3*MA02 - MHH2)*MHp2

        AllHix(1004) =
     &   MA02**2*MHH2*(MHH2 - 2*MHp2) + MHp2**2*AllHix(1003)

        AllHix(1013) =
     &   4*MHp2*(1 - SBA2) - MHH2*(4 - SBA2) + 2*MA02*(2 + SBA2)

        AllHix(1015) =
     &   MHp2*(MHH2 + (MHH2 - 2*MHp2)*SBA2) -
     &    MA02*(MHp2 + (MHH2 - 2*MHp2)*SBA2)

        AllHix(1016) =
     &   MA02*(MHp2 + (MHH2 - 2*MHp2)*SBA2) +
     &    MHp2*(2*MHp2*SBA2 - MHH2*(1 + SBA2))

        AllHix(102) = -(MHH2*(MA02**2 - 2*MHp2**2)) +
     &    AllHix(100) - 2*AllHix(101)

        AllHix(1024) =
     &   4*MA02**2 - MHH2**2 - 4*MHH2*(MA02 - 2*MHp2) - 8*MHp2**2

        AllHix(1025) =
     &   CBA*(2*MA02 + MHH2 - 4*MHp2)*S2B**2 +
     &    (2*m12sq - MHH2*S2B)*SAB

	AllHix(1052) = MA02**2 - Mh02*(5*MA02 - MHH2)

        AllHix(1057) =
     &   CAB*(2*m12sq - Mh02*S2B) -
     &    (MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2)) -
     &    1/2.D0*((2*MA02 - Mh02)*S2B**2*SBA)

        AllHix(1068) =
     &   8*CAB*(MA02 - MHH2)*(m12sq - 3*CBA2*m12sq + Mh02*S2B) +
     &    SBA*AllHix(1066) +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*AllHix(1067))

        AllHix(1089) =
     &   (2*CBA2*AllHix(1087))/(MA02 - Mh02)**2 +
     &    (SBA2*AllHix(1088))/((MA02 - MHH2)**2*(4*Mh02 - MHH2))

        AllHix(1093) =
     &   -2*m12sq*(MA02 - MHH2)*(3*S2A + S2B) + S2B*AllHix(1092)

        AllHix(1095) =
     &   4*(MA02 - Mh02)*SAB*(m12sq + MHH2*S2B - 3*m12sq*SBA2) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*AllHix(1094))

        AllHix(1098) =
     &   CBA2*(Mh02 - 2*MHH2)*(MA02 - MHH2) -
     &    2*(MA02 - Mh02)*MHH2*SBA2

	AllHix(1104) = MHH2*(S2A + S2B) - 2*MA02*(2*S2A + S2B)

        AllHix(1109) =
     &   CBA*S2B*AllHix(1107) + 4*SAB*AllHix(1108) -
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*(1 - 3*SBA2))

        AllHix(1122) =
     &   CBA*S2B*AllHix(1117) + 4*AllHix(1121) +
     &    (2*MW2*SW2)/(Alfa*Pi)*(SBA*AllHix(1119))

        AllHix(1134) =
     &   CBA*S2B*AllHix(1130) + 4*SAB*AllHix(1132) +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*AllHix(1133))

        AllHix(1141) =
     &   CBA*(2*MA02 - MHH2)*S2B*AllHix(1135) - 2*AllHix(1140)

        AllHix(115) = S2B**2*AllHix(110) -
     &    2*CBA*S2B*SAB*AllHix(112) + 2*AllHix(114)

        AllHix(1169) =
     &   (2*CBA2*Mh02)/(MA02 - Mh02) +
     &    ((2*Mh02 - MHH2)*SBA2)/(MA02 - MHH2)

        AllHix(117) = -4*MHH2**4 - 12*MHH2**2*MHp2**2 +
     &    4*MHH2*MHp2**3 + MHp2*AllHix(116)

	AllHix(12) = MHH2**2 - 4*MHH2*MHp2 + 2*MHp2**2

        AllHix(121) = (MA02 - MHH2)*(MA02 + 2*MHH2 - 2*MHp2)*
     &     (MHH2 - MHp2)*AllHix(118) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*AllHix(120))

	AllHix(1212) = 2*MA02*(Mh02 - 3*MHp2) - Mh02*(Mh02 - 2*MHp2)

	AllHix(1213) = MA02*(Mh02 - 5*MHp2) + MHp2**2

        AllHix(1215) =
     &   4*MA02*SB2*SBA + (MW2*SW2)/(Alfa*Pi)*(CBA*Lambda6)

        AllHix(1216) =
     &   4*MHp2*SB2*SBA + (MW2*SW2)/(Alfa*Pi)*(CBA*Lambda6)

        AllHix(1217) =
     &   (MA02 - Mh02 + MHp2)*S2B**2*SBA +
     &    (2*MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

        AllHix(1219) =
     &   -(CB2*AllHix(1218)) +
     &    (MW2*SW2)/(Alfa*Pi)*(CBA*Lambda7*SB2)

        AllHix(1221) =
     &   Mh02**2*S2B**2*SBA2 -
     &    (2*MW2*SW2)/(Alfa*Pi)*(Lambda7*AllHix(1220))

	AllHix(1224) = CBA2*MA02 - Mh02 + (1 - CBA2)*MHp2

        AllHix(1228) =
     &   CBA2*(MA02 - MHp2)**2*AllHix(1226) +
     &    2*(Mh02 - MHp2)**2*AllHix(1227)

	AllHix(1230) = MA02 - Mh02 + CBA2*(2*MA02 - 2*MHp2)

	AllHix(1234) = Mh02**2 - Mh02*MHp2 - 12*MHp2**2

        AllHix(1235) =
     &   9*Mh02**3 + 7*Mh02**2*MHp2 - 44*Mh02*MHp2**2 + 64*MHp2**3

        AllHix(1236) =
     &   53*Mh02**3 - 37*Mh02**2*MHp2 - 188*Mh02*MHp2**2 +
     &    256*MHp2**3

        AllHix(1246) =
     &   3*Mh02**3*S2B**4*SBA2 -
     &    4*(Mh02**2*S2B**2*AllHix(1239) + MHp2**2*AllHix(1242)) +
     &    Mh02*MHp2*AllHix(1245)

        AllHix(1251) =
     &   20*MA02**3*S2B**4*SBA2 - MA02**2*AllHix(1248) +
     &    2*MA02*AllHix(1250)

	AllHix(1258) = -4*S2B**2*SBA*AllHix(1254) + Mh02*AllHix(1257)

        AllHix(1260) =
     &   MHp2*S2B**2*SBA +
     &    (MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

        AllHix(1293) =
     &   32*MA02**5*S2B**4*SBA2 +
     &    S2B**2*(4*MA02**4*AllHix(1286) - AllHix(1292))

        AllHix(1305) =
     &   (CBA2*AllHix(1212))/(MA02 - Mh02)**2 -
     &    AllHix(1213)/(MA02 - MHp2)**2 - 4*AllHix(1297)

        AllHix(131) = MA02**2 + MHH2**2 -
     &    2*(MA02 + MHH2)*MHp2 + 2*MHp2**2

	AllHix(1322) = 3*Mh02**2 - 11*Mh02*MHp2 - 4*MHp2**2

	AllHix(1323) = Mh02**2 - 5*Mh02*MHp2 + 4*MHp2**2

	AllHix(1327) = 81*S2A**2 + 12*S2A*S2B + 16*S2B**2

        AllHix(1329) =
     &   3*MHp2*S2B*SBA2*AllHix(1328) +
     &    (8*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(3*S2A + 8*S2B)*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHix(133) = -4*C2B*m12sq - 2*MHH2*S2B*SAB*SBA +
     &    CBA*(MHH2*S2B**2*SBA + Mh02*AllHix(132)) +
     &    (2*MW2*SW2)/(Alfa*Pi)*(CB2*Lambda6 - Lambda7*SB2)

	dup1 = Lambda6*Lambda7*S2B**2 - 2*AllHix(357)

        AllHix(1330) =
     &   MHp2**2*S2B**4*SBA2 -
     &    (MW2**2*SW2**2)/(Alfa2*Pi**2)*dup1 +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*MHp2*S2B**2*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHix(1331) =
     &   3*MHp2**2*(S2A - 2*S2B)*S2B**3*SBA2 +
     &    (2*MW2**2*SW2**2)/(Alfa2*Pi**2)*dup1 +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (CBA*MHp2*(3*S2A - 8*S2B)*S2B*
     &       (CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHix(1333) =
     &   MHp2**2*S2B**2*SBA2*AllHix(1332) +
     &    (16*MW2**2*SW2**2)/(Alfa2*Pi**2)*dup1 -
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*MHp2*S2B*(15*S2A + 8*S2B)*
     &       (CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHix(1341) =
     &   4*m12sq**2*AllHix(1338) - 4*m12sq*Mh02*S2B*AllHix(1339) +
     &    Mh02**2*S2B**2*AllHix(1340)

	AllHix(1343) = Mh02**2 + Mh02*MHp2 - 2*MHp2**2

        AllHix(1350) =
     &   -(m12sq*AllHix(1349)) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (CBA*Mh02*(25*Mh02 + 11*MHp2)*S2B*
     &       (CB2*Lambda6 - Lambda7*SB2))

        AllHix(1361) =
     &   (CBA2*AllHix(1360))/((4*Mh02 - MHH2)*(Mh02 - MHp2)**2) -
     &    1/2.D0*(MHp2*SBA2)/(MHH2 - MHp2)**2

        AllHix(1374) =
     &   -2*m12sq*(Mh02 - MHp2)*(3*S2A - S2B) + S2B*AllHix(1373)

        AllHix(1375) =
     &   -2*(1 - 3*CBA2)*(MHH2 - MHp2) + 3*(Mh02 - MHp2)*SBA2

	AllHix(1386) = CBA2*(MHH2 - MHp2) - (Mh02 - MHp2)*SBA2

        AllHix(1389) =
     &   CBA2*(Mh02 - 2*MHH2)*(MHH2 - MHp2) -
     &    2*MHH2*(Mh02 - MHp2)*SBA2

        AllHix(1397) =
     &   CAB*(2*m12sq - Mh02*S2B) -
     &    (MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2)) +
     &    1/2.D0*((Mh02 - 2*MHp2)*S2B**2*SBA)

        AllHix(1411) =
     &   -(m12sq*(Mh02 + CBA2*(Mh02 - 4*MHp2))) +
     &    Mh02*(Mh02 - 2*MHp2)*S2B

	AllHix(1412) = (Mh02 - 4*MHp2)*S2A - (Mh02 - 2*MHp2)*S2B

        AllHix(1415) =
     &   -4*CAB*(m12sq - 3*CBA2*m12sq + Mh02*S2B) +
     &    S2B*SBA*AllHix(1414) +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(1 - 3*CBA2)*(CB2*Lambda6 - Lambda7*SB2))

        AllHix(1424) =
     &   2*CAB*AllHix(1419) + 4*(Mh02 - MHp2)**2*AllHix(1420) +
     &    CBA2*AllHix(1423) +
     &    (6*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*CBA2**2*(Mh02 - 4*MHp2)*(Mh02 - 2*MHp2)*
     &       (CB2*Lambda6 - Lambda7*SB2))

        AllHix(1432) =
     &   4*((Mh02 - MHp2)**2*AllHix(1420) + CAB*AllHix(1428)) +
     &    MHp2*AllHix(1431)

	AllHix(145) = S2B**2*AllHix(141) - 2*CBA*S2B*SAB*AllHix(144)

        AllHix(147) = MA02**3*MHH2 - 3*MA02**2*MHH2**2 -
     &    MHH2**4 + MA02*AllHix(146)

        AllHix(149) = (MA02 - MHH2)*(MA02 - MHH2 - 2*MHp2)*
     &     (MHH2 - MHp2)*AllHix(118) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*AllHix(148))

        AllHix(1506) =
     &   -2*(MHH2 - 2*MHp2)*(Mh02 - MHp2)*S2B + S2A*AllHix(1505)

        AllHix(1508) =
     &   3*CBA2*(MHH2 - MHp2) - 2*(Mh02 - MHp2)*(1 - 3*SBA2)

        AllHix(1548) =
     &   4*SAB*AllHix(1108) + CBA*S2B*AllHix(1547) -
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*(1 - 3*SBA2))

	AllHix(156) = MHH2**2 + MHp2**2 - MA02*(MHH2 + MHp2)

        AllHix(1562) =
     &   CBA*(MHH2 - 2*MHp2)*S2B*
     &     (MHH2**2*AllHix(1553) - 2*AllHix(1555)) + 2*AllHix(1561)

        AllHix(157) = 1 + (MA02 - MHH2)/(MA02 - MHp2) +
     &    (2*(MHH2 - MHp2)*SBA2)/(MA02 - MHH2)

        AllHix(1572) =
     &   -2*(MHH2 - 2*MHp2)*(MHH2 - MHp2)**2*S2B +
     &    MHp2*SBA2*AllHix(1571)

        AllHix(1578) =
     &   -(SAB*AllHix(1576)) - MHH2*MHp2*AllHix(1577) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*AllHix(1573))

        AllHix(1581) =
     &   -4*(MHH2 - 2*MHp2)*(MHH2 - MHp2)**2*S2B +
     &    MHp2*SBA2*AllHix(1580)

        AllHix(1586) =
     &   -(MHH2*MHp2*AllHix(1585)) +
     &    2*(SAB*AllHix(1583) +
     &       (MW2*SW2)/(Alfa*Pi)*
     &        ((CB2*Lambda6 - Lambda7*SB2)*SBA*AllHix(1584)))

        AllHix(159) = MHH2**2 + 4*MHp2**2 +
     &    MA02**2*(2 - SBA2) - MHH2*MHp2*(3 + SBA2) +
     &    MA02*(MHH2 - 5*MHp2 + (MHH2 + MHp2)*SBA2)

        AllHix(163) = MA02**3 + MA02**2*AllHix(160) +
     &    MA02*AllHix(161) + MHH2*AllHix(162)

	AllHix(1640) = (2*MA02 - 3*Mh02 + 4*MHp2)*S2B - 3*AllHix(1639)

        AllHix(1641) =
     &   CBA2**2*(3*S2A - S2B) + 12*CBA2*S2A*SBA2 +
     &    (3*S2A + S2B)*SBA2**2

        AllHix(1644) =
     &   S2A*S2B*SBA*AllHix(1643) -
     &    (12*MW2*SW2)/(Alfa*Pi)*
     &     (SAB*(CB2*Lambda6 - Lambda7*SB2)*(1 + 3*SBA2**2))

        AllHix(1655) =
     &   (2*m12sq - Mh02*S2B)*(2*m12sq - MHH2*S2B)*SAB -
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*AllHix(1654))

        AllHix(1660) =
     &   S2B*AllHix(1657) - MHH2*AllHix(1659) +
     &    (3*MW2*SW2)/(Alfa*Pi)*
     &     (Mh02*(CB2*Lambda6 - Lambda7*SB2)*AllHix(1656))

	AllHix(1665) = -2*m12sq*MHH2*(3*S2A + S2B) + S2B*AllHix(1664)

	AllHix(1669) = -2*m12sq*Mh02*(3*S2A - S2B) + S2B*AllHix(1668)

        AllHix(1671) =
     &   MHH2*(MA02 + 3*MHH2 + 2*MHp2)*S2B*SAB -
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*AllHix(1670))

        AllHix(1674) =
     &   SBA*AllHix(1673) -
     &    (6*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA2)

	AllHix(1684) = CBA*(MHH2 - 2*MHp2)*S2B - MHH2*SAB

        AllHix(1685) =
     &   CBA*(MHH2 - 2*MHp2)*S2B**2 + 2*(2*m12sq - MHH2*S2B)*SAB

        AllHix(1690) =
     &   CBA2*(MHH2 - 2*MHp2)**2 + (Mh02 - 2*MHp2)**2*SBA2

        AllHix(1692) =
     &   2*CAB*CBA*(2*m12sq - Mh02*S2B) + SBA*AllHix(1691)

        AllHix(1696) =
     &   6*MA02 - CBA2*(2*Mh02 + 4*MHH2 - 5*MHp2) - MHp2 -
     &    (4*Mh02 + 2*MHH2 - 5*MHp2)*SBA2

        AllHix(1698) =
     &   2*CAB*(2*m12sq - Mh02*S2B) - (2*MA02 - Mh02)*S2B**2*SBA

        AllHix(1699) =
     &   CAB*(2*MA02 - Mh02)*S2B**2*(2*m12sq - Mh02*S2B)*SBA +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*AllHix(1698))

        AllHix(1703) =
     &   (2*m12sq - MHH2*S2B)*SAB*
     &     (2*m12sq*SAB - S2B*AllHix(1701)) -
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*AllHix(1702))

        AllHix(171) = -2*CAB*m12sq +
     &    (MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

	AllHix(1719) = 3*Mh02**2*SBA2 + 2*CBA2*AllHix(1718)

        AllHix(172) = Mh02*S2B*AllHix(170) -
     &    2*(1 - 3*CBA2)*SBA*AllHix(171)

        AllHix(1723) =
     &   Mh02**2*S2A**2*S2B**2*SBA2 -
     &    4*CAB*(2*CBA2*m12sq - Mh02*S2B)*AllHix(1721) +
     &    4*AllHix(1722) -
     &    (2*MW2**2*SW2**2)/(Alfa2*Pi**2)*(CBA2**3*dup1)

        AllHix(1728) =
     &   CBA2*(MA02 - Mh02)**2*Re(AllHil(11)) +
     &    (MA02 - MHH2)**2*SBA2*Re(AllHil(12))

        AllHix(1730) =
     &   (MA02 - Mh02)**2*Re(AllHil(16)) +
     &    2*(Mh02 - MHp2)**2*Re(AllHil(17))

        AllHix(1731) =
     &   (MA02 - Mh02)*(MA02 + Mh02)*
     &     (2*AllHix(1728) - 2*CBA2*AllHix(1730) +
     &       4*(MA02 - MHp2)**2*Re(AllHil(10))) -
     &    3*SBA2*AllHix(1729)*Re(AllHil(15))

        AllHix(174) = -2*(1 - 3*CBA2)*SBA*AllHix(171) +
     &    Mh02*S2B*AllHix(173)

        AllHix(1743) =
     &   2*CBA2*(AllHix(1718) + 2*AllHix(1730)) +
     &    3*Mh02**2*SBA2*(1 + 2*Re(AllHil(15)))

	AllHix(1750) = 3*CBA2*MHH2**2 + 2*SBA2*AllHix(1749)

        AllHix(1759) =
     &   (MA02 - MHH2)**2*Re(AllHil(24)) +
     &    2*(MHH2 - MHp2)**2*Re(AllHil(25))

        AllHix(1760) =
     &   (MA02 - MHH2)*(MA02 + MHH2)*
     &     (2*AllHix(1728) - 2*SBA2*AllHix(1759) +
     &       4*(MA02 - MHp2)**2*Re(AllHil(10))) -
     &    3*CBA2*AllHix(1758)*Re(AllHil(23))

        AllHix(177) = (MA02**2 + 2*MA02*Mh02)*S2B -
     &    Mh02**2*(6*S2A + S2B)

	AllHix(1774) = 4*m12sq**2 - 4*m12sq*Mh02*S2B + Mh02**2*S2B**2

        AllHix(1775) =
     &   2*m12sq*(2*MA02 - Mh02)*S2B**2 -
     &    (2*MA02*Mh02 - Mh02**2)*S2B**3

	AllHix(1776) = 4*MA02**2 - 4*MA02*Mh02 + Mh02**2

        AllHix(1778) =
     &   2*m12sq*(2*MA02 - MHH2)*S2B**2 -
     &    (2*MA02*MHH2 - MHH2**2)*S2B**3

	AllHix(1779) = 4*m12sq**2 - 4*m12sq*MHH2*S2B + MHH2**2*S2B**2

        AllHix(178) = -2*CAB*
     &     (m12sq - 3*CBA2*m12sq + Mh02*S2B) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(1 - 3*CBA2)*(CB2*Lambda6 - Lambda7*SB2))

        AllHix(1780) =
     &   -(CBA*(2*MA02 - MHH2)*S2B**2) + 2*(2*m12sq - MHH2*S2B)*SAB

	AllHix(1782) = -MA02**2 + 2*MA02*MHp2 - MHp2**2

        AllHix(1784) =
     &   -(MA02*(Mh02 + 2*MHH2)) + AllHix(1783)*Re(AllHil(6))

        AllHix(1786) =
     &   5*MA02**2 - 2*(2*Mh02 + MHH2)*(MA02 - MHp2) - 5*MHp2**2 +
     &    2*AllHix(1785)*Re(AllHil(7))

        AllHix(1788) =
     &   2*m12sq*(Mh02 - 2*MHp2)*S2B**2 -
     &    (Mh02**2 - 2*Mh02*MHp2)*S2B**3

	AllHix(1789) = Mh02**2 - 4*Mh02*MHp2 + 4*MHp2**2

        AllHix(1791) =
     &   2*m12sq*(MHH2 - 2*MHp2)*S2B**2 -
     &    (MHH2**2 - 2*MHH2*MHp2)*S2B**3

	AllHix(1793) = 4*MA02**2 - 4*MA02*MHH2 + MHH2**2

	AllHix(1794) = MHH2**2 - 4*MHH2*MHp2 + 4*MHp2**2

        AllHix(18) = (11 - 3*CBA2)*MA02**2*Mh02 +
     &    (9 + CBA2)*Mh02**3 + (2 + CBA2)*(MA02**3 + MA02*Mh02**2)

        AllHix(1802) =
     &   (MA02 - MHp2)**2*Re(AllHil(5)) +
     &    CBA2*(Mh02 - MHp2)**2*Re(AllHil(6)) +
     &    (MHH2 - MHp2)**2*SBA2*Re(AllHil(7))

        AllHix(1806) =
     &   (MA02 - MHp2)**2 + SBA2*AllHix(1798) + CBA2*AllHix(1805)

	AllHix(1808) = MHH2**4 - MHH2**2*MHp2**2

	AllHix(1815) = Mh02**4 - Mh02**2*MHp2**2

        AllHix(185) = (2*Mh02*MHp2 + MHp2**2)*S2B -
     &    Mh02**2*(6*S2A + S2B)

        AllHix(19) = (6 + CBA2)*MA02**2 +
     &    3*(3 - CBA2)*MA02*Mh02 + (9 + 2*CBA2)*Mh02**2

        AllHix(190) = (3 + 2*CBA2)*Mh02**3*S2B**2 -
     &    Mh02**2*AllHix(186) -
     &    (Mh02 - MHp2)*S2B*SBA2*AllHix(187) -
     &    2*Mh02*SBA*AllHix(189)

        AllHix(197) = m12sq*AllHix(196) -
     &    (3*MW2*SW2)/(Alfa*Pi)*(CBA*Lambda7*(Mh02 - MHH2)*SB2*SBA)

        AllHix(202) = -2*MA02**2*(Mh02**3 + MHH2**3)*S2B*SB2 +
     &    (Mh02 + MHH2)*AllHix(199) + 2*MA02*AllHix(201)

	AllHix(207) = MA02*Mh02 - (MA02 + 3*Mh02)*MHH2 + MHH2**2

        AllHix(211) = (MA02 - Mh02)**2*(MA02 - MHH2)**2*
     &     (Mh02 - MHH2)*SBA2 + CBA2*Mh02*AllHix(210)

        AllHix(213) = 2*m12sq*(3*S2A - S2B) -
     &    (2*Mh02 + MHH2)*S2A*S2B +
     &    (6*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHix(214) = (2*MA02 - Mh02)*S2B**2*SBA2 -
     &    (CBA2*(MA02 - MHH2)*AllHix(213))/(MA02 - Mh02) +
     &    SBA*(-2*CAB*(2*m12sq - Mh02*S2B) +
     &       (2*MW2*SW2)/(Alfa*Pi)*
     &        (CBA*(CB2*Lambda6 - Lambda7*SB2)))

	AllHix(219) = CBA2*(Mh02 - MHp2)*S2B - 2*CAB*Mh02*SBA

	AllHix(22) = MA02**2 + CBA2*Mh02*(Mh02 - MHp2) + MHp2**2

	AllHix(222) = MA02**2*(2*Mh02 - MHp2) + 2*MHp2**3

        AllHix(223) = 6*Mh02*MHp2**2 - 2*MHp2**3 -
     &    3*(Mh02**3 + Mh02**2*MHp2)

        AllHix(228) = Mh02**3*AllHix(225) - S2B*AllHix(226) +
     &    2*Mh02**2*AllHix(227)

	AllHix(23) = 2*Mh02**2 - 3*Mh02*MHp2 + MHp2**2

        AllHix(233) = Mh02**2*MHp2*
     &     (MHH2*S2B*SBA2 - 4*MHp2*AllHix(229)) +
     &    Mh02**4*AllHix(230) +
     &    Mh02**3*(MHH2*S2B*SBA2 + MHp2*AllHix(230)) +
     &    2*AllHix(232)

        AllHix(236) = MHp2*S2B*(2*MHp2 + MHH2*SBA2) -
     &    Mh02**2*AllHix(234) -
     &    Mh02*(MHH2*S2B*SBA2 - MHp2*AllHix(235))

        AllHix(241) = MA02**4*S2B +
     &    2*Mh02*MHp2**2*AllHix(237) -
     &    Mh02**2*MHp2*(2*MHH2*S2B*SBA2 + MHp2*AllHix(238)) -
     &    2*(Mh02**3*MHp2*AllHix(239) + S2B*AllHix(240))

	AllHix(243) = MA02**2 - MA02*Mh02 + 2*(Mh02 - MHp2)*MHp2

        AllHix(245) = (Mh02*MHp2**2 + MHp2**3)*S2B**2*SBA2 -
     &    2*Mh02**2*(MHp2*S2B**2*SBA2 + CBA*AllHix(244))

        AllHix(247) = (Mh02**2 - 2*MHp2**2)*S2B**2*SBA2 +
     &    Mh02*AllHix(246)

	AllHix(249) = MA02**3*S2B**2*SBA2 - Mh02*MHp2*AllHix(248)

        AllHix(255) = (Mh02**2*AllHil(32)*AllHix(253))/
     &     (Mh02 - MHH2) +
     &    (MA02*AllHil(31)*AllHix(254))/(Mh02*(MA02 - MHH2))

	AllHix(26) = CBA2*(Mh02 - MHp2)**2 + 3*Mh02*MHp2

        AllHix(261) = 2*MA02**4 - Mh02**2*(Mh02 - MHH2)*MHH2 -
     &    MA02**2*(Mh02 - 4*MHH2)*(Mh02 + MHH2) -
     &    MA02**3*(Mh02 + 5*MHH2) + MA02*AllHix(260)

        AllHix(263) = 2*m12sq*(3*S2A - S2B) -
     &    (2*Mh02 + MHH2)*S2A*S2B + (2*MA02 - MHH2)*S2B**2

        AllHix(264) = (2*m12sq - MHH2*S2B)*SAB +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((1 - 3*CBA2)*(CB2*Lambda6 - Lambda7*SB2)*SBA)

	AllHix(266) = 2*m12sq*AllHix(196) - (Mh02 - MHH2)*AllHix(265)

	AllHix(271) = MA02**2*Mh02**3 + MHH2*AllHix(270)

	AllHix(272) = MA02*Mh02**3*MHH2 + MA02**2*Mh02*MHH2**2

        AllHix(273) = (MA02*Mh02**2 + Mh02**3)*MHH2**2 +
     &    MA02*Mh02*MHH2**3

	AllHix(275) = MA02**3*Mh02**2 - MA02**2*Mh02**3

        AllHix(276) = 2*MA02**3*Mh02 + 3*MA02**2*Mh02**2 -
     &    5*MA02*Mh02**3

	AllHix(277) = 5*MA02**2*Mh02 - 2*MA02*Mh02**2 - 3*Mh02**3

        AllHix(279) = MA02**5 + MA02**4*Mh02 -
     &    5*MA02**3*Mh02**2 + 3*MA02**2*Mh02**3

	AllHix(280) = -MA02**4 - MA02*Mh02**3 + AllHix(270)

        AllHix(281) = 3*MA02**3 - 5*MA02**2*Mh02 +
     &    MA02*Mh02**2 + Mh02**3

	AllHix(282) = MA02**2 - 2*MA02*Mh02 + Mh02**2

        AllHix(287) = 11*MA02**3*Mh02**3 + 2*MA02*Mh02**5 -
     &    6*(MA02**4*Mh02**2 + MA02**2*Mh02**4) +
     &    Mh02**2*MHH2**4 + MHH2*AllHix(284) -
     &    MHH2**2*AllHix(285) + MHH2**3*AllHix(286)

        AllHix(29) = CBA2**2*(MA02 - Mh02)**2*MHp2 -
     &    CBA2*(MA02 - MHp2)*AllHix(28)

        AllHix(292) = 2*CAB*(MA02 - Mh02)**2*Mh02**2*
     &     (MA02 - MHH2)*(Mh02 - MHH2)*SBA + S2A*SBA2*AllHix(291)

	AllHix(294) = CBA2*(MA02 - Mh02) + MHH2*SBA2

        AllHix(298) = MA02**3*Mh02**2 - 2*MA02**2*Mh02**3 +
     &    MA02*Mh02**4 + MHH2*AllHix(296) + MHH2**2*AllHix(297)

        AllHix(301) = 2*MA02**3*Mh02**2 -
     &    3*(MA02*Mh02 - Mh02**2)*MHH2**3 + MHH2**2*AllHix(277) -
     &    MHH2*AllHix(299) - 2*AllHix(300)

	AllHix(304) = -1 + MA02/Mh02 + Mh02/(MA02 - Mh02)

	AllHix(306) = CBA2*(MA02 - Mh02) - (MA02 - MHH2)*SBA2

	AllHix(307) = 2*C2B*m12sq + (MW2*SW2)/(Alfa*Pi)*(Lambda7*SB2)

	AllHix(308) = (Mh02 - MHH2)*S2A - 2*MA02*S2B

	AllHix(313) = -2*m12sq*(3*S2A + S2B) + S2B*AllHix(312)

        AllHix(314) = SBA2*AllHix(313) -
     &    SBA*(2*CAB*(2*m12sq - Mh02*S2B) -
     &       (2*MW2*SW2)/(Alfa*Pi)*
     &        (CBA*(CB2*Lambda6 - Lambda7*SB2)*(1 - 3*SBA2)))

	AllHix(320) = 2*MA02**2 - MA02*Mh02 - Mh02**2

	AllHix(322) = 6*MA02**4 - 13*MA02**3*Mh02 + 3*MA02**2*Mh02**2

	AllHix(323) = 2*MA02**3 + 3*MA02**2*Mh02 + MA02*Mh02**2

	AllHix(33) = Mh02*MHp2 + MA02*(Mh02 + 3*MHp2)

        AllHix(333) = CBA2*
     &     (CB2*Lambda6*AllHix(329) - Lambda7*SB2*AllHix(330)) -
     &    (CB2*Lambda6 - Lambda7*SB2)*SBA2*AllHix(332)

        AllHix(335) = C2B*m12sq*(MA02 - Mh02)*(MA02 - MHH2)*
     &     AllHix(334) + (MW2*SW2)/(Alfa*Pi)*
     &     (CB2*CBA2*Lambda6*MA02**4)

	AllHix(338) = Mh02*S2A - MA02*S2B - MHH2*(S2A + S2B)

        AllHix(356) = 2*MA02*(Mh02 + MHH2) +
     &    Mh02*(MHH2 - 4*MHp2) + 8*MHp2**2 -
     &    4*(MA02**2 + MHH2*MHp2)

	AllHix(358) = -(Lambda6*Lambda7*S2B**2) + 2*AllHix(357)

        AllHix(362) = -2*m12sq*(2*MA02 + Mh02 - 4*MHp2) +
     &    MHH2*(Mh02 - 4*MHp2)*S2B

	AllHix(363) = CBA2*(MHH2 - 4*MHp2) - (Mh02 - 4*MHp2)*SBA2

        AllHix(364) = MHH2*S2B*SAB*SBA +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*(CBA2 - SBA2))

	AllHix(367) = 4*MA02**2 + Mh02*MHH2 - 2*MA02*(Mh02 + MHH2)

        AllHix(368) = CB2*Lambda6*(2*m12sq - MHH2*S2B)*SAB +
     &    (MW2*SW2)/(Alfa*Pi)*(CB2**2*Lambda6**2*SBA)

	AllHix(369) = Lambda6*S2B**2 - 2*Lambda7*SB2**2

        AllHix(370) = 2*(2*m12sq - MHH2*S2B)*SAB*SB2 +
     &    (MW2*SW2)/(Alfa*Pi)*(SBA*AllHix(369))

        AllHix(373) = -2*MA02*AllHix(364) +
     &    SAB*SBA*AllHix(372) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*(CBA2*MHH2 - Mh02*SBA2))

	AllHix(379) = -4*Mh02**4 + MHp2*AllHix(377) - Mh02*AllHix(378)

        AllHix(387) = MA02**3*MHp2*S2B - Mh02*AllHix(381) -
     &    MA02**2*AllHix(384) + MA02*AllHix(386)

        AllHix(39) = MA02/(MA02 - MHp2) -
     &    (2*(MA02**2 + MHp2**2))/(MA02 - MHH2)**2 +
     &    2*MHp2*AllHix(38)

        AllHix(391) = MA02**3*(7*Mh02 - 3*MHp2) -
     &    MA02*AllHix(389) - Mh02*AllHix(390)

	AllHix(40) = MHH2**2 - MA02*(2*MHH2 - MHp2)

        AllHix(400) = 2*MA02**3*(2*Mh02 - MHp2)*S2B -
     &    MA02**2*AllHix(394) + MA02*AllHix(396) -
     &    Mh02*(S2B*AllHix(397) + MHp2*AllHix(399))

        AllHix(403) = 8*C2B*CBA*m12sq*(MA02 - 2*MHp2)*SBA -
     &    11*MA02**2*S2B**2*SBA2 + 2*MA02*AllHix(401) -
     &    MHp2*AllHix(402)

	AllHix(406) = -(MA02*Mh02) - Mh02**2 + 2*Mh02*MHp2

        AllHix(407) = MA02**2 + Mh02**2 -
     &    2*(MA02 + Mh02)*MHp2 + 2*MHp2**2 +
     &    (Mh02 - MHH2)*(MA02 + Mh02 - 2*MHp2)*SBA2

        AllHix(410) = MA02**3*Mh02 - 3*MA02**2*Mh02**2 -
     &    Mh02**4 + MA02*AllHix(409)

        AllHix(419) = MA02**3*(Mh02 + MHp2)*S2B -
     &    MA02**2*AllHix(413) + MA02*AllHix(416) - Mh02*AllHix(418)

        AllHix(42) = 11*MA02**2*MHH2 + 9*MHH2**3 +
     &    2*(MA02**3 + MA02*MHH2**2)

	AllHix(422) = -2*CAB*Mh02*(MA02 - MHp2)*SBA + S2B*AllHix(421)

	AllHix(429) = 6*Mh02**2*S2A - (MA02 + Mh02)**2*S2B

	AllHix(43) = 2*MA02**2 + 3*MHH2*(MA02 + MHH2)

        AllHix(432) = 2*MA02**4*SBA2 -
     &    MA02**3*(2*MHH2*SBA2 + Mh02*(2 + SBA2))

        AllHix(433) = -(MHH2*(S2A*S2B + S2B**2)*SBA2) -
     &    Mh02*S2B*(S2B - (S2A + 4*S2B)*SBA2) +
     &    SBA*(-4*(2*CAB + C2B*CBA)*m12sq +
     &       (6*MW2*SW2)/(Alfa*Pi)*
     &        (CBA*(CB2*Lambda6 - Lambda7*SB2)))

	AllHix(434) = 16*CAB*SBA - 5*S2B*(2 - SBA2) + 6*S2A*SBA2

        AllHix(435) = -(MHH2*(3*S2A*S2B + 2*S2B**2)*SBA2) +
     &    SBA*(-4*(4*CAB + 3*C2B*CBA)*m12sq +
     &       (14*MW2*SW2)/(Alfa*Pi)*
     &        (CBA*(CB2*Lambda6 - Lambda7*SB2)))

	AllHix(436) = 6*S2B - 16*CAB*SBA - (4*S2A - 7*S2B)*SBA2

        AllHix(437) = -(MHH2*S2A*S2B*SBA2) +
     &    SBA*(-4*(CAB + C2B*CBA)*m12sq +
     &       (4*MW2*SW2)/(Alfa*Pi)*
     &        (CBA*(CB2*Lambda6 - Lambda7*SB2)))

	AllHix(439) = MHp2*S2B + MHH2*(S2A - 2*CBA*SAB)

	AllHix(44) = MHH2**2 + (MA02 - 2*MHH2)*MHp2

	AllHix(440) = 2*S2A - 6*CBA*SAB - 2*CAB*SBA + S2B*(3 + SBA2)

	AllHix(441) = 2*S2A - 2*(3*CBA*SAB + CAB*SBA) + S2B*(3 + SBA2)

        AllHix(443) = SBA2*AllHix(442) -
     &    (6*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHix(448) = 2*MA02*AllHix(439) +
     &    Mh02**2*AllHix(446) - Mh02*AllHix(447)

        AllHix(454) = S2B**2*AllHix(450) +
     &    2*Mh02*S2B*AllHix(451) + 4*SBA*AllHix(453)

	dup2 = Mh02*S2B*SBA2*AllHix(441) - 2*AllHix(443)

        AllHix(460) = dup2*Mh02 +
     &    24*C2B*CBA*m12sq*(2*MA02 - Mh02)*SBA -
     &    MA02*(Mh02*S2B*SBA2*AllHix(441) - 2*AllHix(459))

	AllHix(47) = MA02**2 - MHH2*(2*MA02 - MHp2)

	AllHix(474) = 96*CAB*SBA + 52*S2A*SBA2 + S2B*(10 + 21*SBA2)

        AllHix(475) = -(MHH2*(S2A*S2B + 2*S2B**2)*SBA2) +
     &    SBA*(-4*(2*CAB + C2B*CBA)*m12sq +
     &       (6*MW2*SW2)/(Alfa*Pi)*
     &        (CBA*(CB2*Lambda6 - Lambda7*SB2)))

	AllHix(476) = 18*S2B - 64*CAB*SBA - (28*S2A - 25*S2B)*SBA2

	AllHix(477) = 5*S2B + 16*CAB*SBA + 2*(6*S2A + S2B)*SBA2

        AllHix(478) = -(MHH2*S2B**2*SBA2) +
     &    SBA*(-2*CAB*m12sq +
     &       (MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2)))

	AllHix(481) = 2*Mh02*S2B**2 + 3*SBA*AllHix(171)

	AllHix(485) = -8 + 3*SBA2 + 24*SW2 - 24*SW2**2

        AllHix(488) = -4*m12sq - S2B*(CBA2*MHH2 + Mh02*SBA2) +
     &    2*AllHix(487)

        AllHix(489) = 2*SAB*
     &     (m12sq + MHH2*S2B - 3*m12sq*SBA2) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*(1 - 3*SBA2))

        AllHix(490) = CBA2*S2B*(3*MHH2*S2A + 2*MHp2*S2B) +
     &    MHH2*S2B**2*SBA2 - 2*CBA*AllHix(489)

        AllHix(494) = -(MHH2**2*(S2A - 2*S2B)*SBA2) +
     &    (MHH2 - MHp2)*AllHix(491) - Mh02*AllHix(493)

        AllHix(50) = -(MHH2**2*MHp2*(1 - SBA2)) -
     &    MA02**2*(3*MHH2 - MHp2*(2 + SBA2)) + MA02*AllHix(49)

        AllHix(501) = -2*MHH2**2*S2A*SBA2 -
     &    MHp2**2*(S2A + 2*CAB*SBA - S2B*SBA2) +
     &    MHH2*MHp2*AllHix(500)

        AllHix(504) = MHH2**3*AllHix(502) +
     &    MHH2**2*(MA02*S2B - MHp2*AllHix(503))

	AllHix(507) = MHp2*(4 - SBA2) + 2*MHH2*SBA2

        AllHix(509) = MHp2*S2B**2*(5 - 2*SBA2)*SBA2 +
     &    2*CBA*AllHix(508)

        AllHix(513) = MHH2**2*S2B*AllHix(510) +
     &    MHH2*(MHp2*S2B*AllHix(511) - 8*CBA*AllHix(512))

	AllHix(522) = MHH2**2*(6*S2A - S2B) + MHp2*(2*MHH2 + MHp2)*S2B

        AllHix(535) = -4*MHH2*AllHix(489) +
     &    CBA*S2B*(MA02**2*S2B + MHH2*AllHix(534))

	AllHix(537) = MHH2**2*(3*S2A - 2*S2B) + MA02*(MA02 + MHH2)*S2B

	AllHix(552) = S2A*S2B*AllHix(547) - S2B**2*AllHix(551)

	AllHix(561) = Mh02*S2A - MHp2*S2B - MHH2*(S2A + S2B)

        AllHix(583) = 8*m12sq*(3*S2A - S2B) +
     &    (11*MA02 - 4*MHH2)*S2B**2 +
     &    S2B*(-4*MHH2*S2A - Mh02*(8*S2A + 3*S2B)) +
     &    (24*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHix(594) = S2B*SBA2*AllHix(589) +
     &    (MA02 - Mh02)*(MA02 - MHH2)*
     &     (-4*CAB*SBA*AllHix(584) + 4*CBA*AllHix(593))

	AllHix(602) = -((MA02 - MHH2)*AllHix(597)) + 4*CBA*AllHix(601)

        AllHix(610) = Mh02**4*MHH2 - Mh02**3*MHH2**2 +
     &    Mh02**2*MHH2**3 - Mh02*MHH2**4 +
     &    2*(Mh02**3 - MHH2**3)*MHp2**2 -
     &    (Mh02 - MHH2)*(Mh02 + MHH2)*MHp2**3 - MHp2*AllHix(609)

        AllHix(614) = 26*(Mh02 + MHH2)*MHp2**4 - 14*MHp2**5 +
     &    MHp2*AllHix(611) + MHp2**2*AllHix(612) -
     &    MHp2**3*AllHix(613)

	AllHix(618) = 7*Mh02*MHH2**2 + 8*MHH2**3

	AllHix(619) = 3*MHH2**4 - 8*MHH2**3*MHp2

        AllHix(621) = CBA2*Mh02*(Mh02 - MHp2) -
     &    MHH2*(MHH2 - MHp2)*SBA2

        AllHix(626) = -((CB2*Lambda6 - Lambda7*SB2)*
     &      (CBA2*AllHix(623) - SBA2*AllHix(625)))

        AllHix(637) = S2B*SBA2*AllHix(633) -
     &    4*(Mh02 - MHp2)*(MHH2 - MHp2)*
     &     (CAB*SBA*AllHix(634) - CBA*AllHix(636))

        AllHix(646) = CBA2**2*(Mh02 - MHp2)**2*AllHix(638) +
     &    (Mh02 - MHp2)*SBA2*AllHix(645)

	AllHix(655) = -2*MA02**4*S2B + MHH2**3*AllHix(654)

        AllHix(658) = -2*MHp2*S2B + MHH2*AllHix(656) +
     &    Mh02*AllHix(657)

        AllHix(661) = 2*(MHp2*S2B + MHH2*AllHix(659)) +
     &    Mh02*AllHix(660)

	AllHix(662) = 2*S2A - 6*CBA*SAB - 2*CAB*SBA + S2B*(3 + SBA2)

	AllHix(663) = MHp2*S2B + MHH2*(S2A - 2*CBA*SAB + 2*S2B*SBA2)

	AllHix(665) = MA02*(3 - SBA2) + 2*MHH2*SBA2

	AllHix(667) = MHH2*S2B**2*SBA2 - CBA*AllHix(666)

        AllHix(669) = MA02**2*S2B**2*(1 + 2*SBA2) -
     &    MHH2*(8*CBA*AllHix(512) - MHH2*S2B*AllHix(668))

	AllHix(675) = -6*MA02**3*S2B + MHH2**2*AllHix(674)

        AllHix(678) = 4*MHp2*S2B + MHH2*AllHix(676) +
     &    2*Mh02*AllHix(677)

        AllHix(681) = -2*MHp2*S2B - MHH2*AllHix(679) +
     &    Mh02*AllHix(680)

	AllHix(683) = 8*CBA*SAB + S2B*(7 + 8*SBA2)

        AllHix(684) = -2*m12sq*SAB*SBA2*(1 + 6*SBA2) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*(3 - SBA2 - 6*SBA2**2))

        AllHix(687) = MA02**2*S2B**2*(9 + 8*SBA2) -
     &    MHH2*(MHH2*S2B*AllHix(685) - 8*CBA*AllHix(686))

        AllHix(696) = -8*MA02**3*S2B*SBA2 -
     &    MHH2**2*AllHix(691) + MA02*MHH2*AllHix(694) -
     &    MA02**2*(2*AllHix(439) - Mh02*AllHix(695))

        AllHix(701) = -(MA02**2*S2B**2*SBA2*(3 + 2*SBA2)) +
     &    4*C2B*CBA*m12sq*SBA*AllHix(697) + MHH2*AllHix(698) +
     &    2*MA02*AllHix(700)

        AllHix(706) = -4*MA02**2*S2B*SBA2 +
     &    MA02*(Mh02*AllHix(662) - 2*AllHix(703)) +
     &    MHH2*AllHix(705)

	AllHix(716) = (Mh02 - MHH2)*S2A - 2*MHp2*S2B

	AllHix(718) = 6*Mh02**2*S2A - (Mh02 + MHp2)**2*S2B

	AllHix(721) = 46*S2B - 64*CAB*SBA - (28*S2A - 25*S2B)*SBA2

	AllHix(722) = 3*MHp2*(3 - SBA2) + 10*MHH2*SBA2

	AllHix(723) = S2B*(9 + SBA2) + 4*(CAB*SBA + S2A*SBA2)

        AllHix(724) = MHH2*(S2A*S2B + 5*S2B**2)*SBA2 -
     &    2*SBA*AllHix(465)

        AllHix(727) = MHp2*S2B*AllHix(725) -
     &    4*(MHH2*S2A*S2B*SBA2 - 4*SBA*AllHix(726))

        AllHix(730) = (MA02 - 2*MHp2)*S2B**2 +
     &    S2B*(MHH2*S2A - AllHix(729))

	AllHix(732) = 11*MHp2*S2B**2 - 24*SBA*AllHix(171)

	AllHix(733) = 13*MHp2*S2B**2 - 12*SBA*AllHix(171)

        AllHix(737) = 14*MHH2*S2B**2*SBA2 -
     &    2*SBA*AllHix(462) + S2B*AllHix(736)

	AllHix(738) = 58*S2B - 28*CAB*SBA + (2*S2A + 15*S2B)*SBA2

        AllHix(741) = Mh02**3*MHp2*S2B*AllHix(739) -
     &    MHp2**3*(4*SBA*AllHix(171) - S2B**2*AllHix(740))

        AllHix(748) = -16*C2B*CBA*m12sq*(Mh02 - 2*MHp2)*SBA -
     &    Mh02**2*S2B*SBA2*(S2A + 2*CAB*SBA - S2B*SBA2) +
     &    Mh02*AllHix(745) -
     &    MHp2*(SBA2*AllHix(746) - 2*AllHix(747))

        AllHix(75) = 2*(MA02 + Mh02)*MHH2**2 - MHH2**3 +
     &    Mh02*AllHix(74)

        AllHix(759) = 2*Mh02**2*(Mh02 - MHp2)*S2B**2 -
     &    MHp2*(S2B*SBA2*AllHix(756) - 2*SBA*AllHix(758))

	AllHix(774) = MHH2**2 - 3*MHH2*MHp2 + MHp2**2

	AllHix(777) = 2*Mh02**2 - MHH2*MHp2 + Mh02*(MHH2 + MHp2)

	AllHix(778) = Mh02*(MHH2 - MHp2) + MHH2*(2*MHH2 + MHp2)

        AllHix(780) = MHH2**4 - 2*MHH2**3*MHp2 +
     &    4*MHH2*MHp2**3 + MHp2**4

        AllHix(813) = Mh02**3 - 2*Mh02**2*(MHH2 + MHp2) -
     &    MHH2*AllHix(812)

	AllHix(816) = 4/(Mh02 - MHH2) + (MHH2 - MHp2)/(Mh02*MHH2)

        AllHix(821) = Mh02**2*AllHix(818) +
     &    MHp2*(-4*Mh02**3 + AllHix(819)) - Mh02*AllHix(820)

        AllHix(827) = (2*CBA2*(Mh02 - MHp2))/(MHH2 - MHp2) +
     &    ((Mh02 + MHH2 - MHp2)*SBA2)/Mh02

        AllHix(828) = (CBA2*(Mh02 + MHH2 - MHp2))/MHH2 +
     &    (2*(MHH2 - MHp2)*SBA2)/(Mh02 - MHp2)

        AllHix(837) = CBA2*(Mh02 - MHH2)*(Mh02 - MHp2)**2*
     &     (MHH2 - MHp2)**2 - MHH2*SBA2*AllHix(836)

        AllHix(838) = -2*m12sq*(3*S2A - S2B) +
     &    (2*Mh02 + MHH2)*S2A*S2B + (MHH2 - 2*MHp2)*S2B**2

	AllHix(84) = MHH2*MHp2 + MA02*(MHH2 + 3*MHp2)

        AllHix(846) = 3*Mh02**3*MHH2**2 - Mh02**2*MHH2**3 -
     &    2*Mh02*MHH2**4

	AllHix(848) = -5*Mh02*MHH2**3 - MHH2**4 + 3*AllHix(847)

        AllHix(849) = Mh02**3 + 5*Mh02**2*MHH2 -
     &    3*(Mh02*MHH2**2 + MHH2**3)

	AllHix(850) = 2*Mh02**2 + Mh02*MHH2 - 3*MHH2**2

        AllHix(855) = Mh02**4*MHH2**2 + Mh02**3*MHH2**3 -
     &    Mh02**2*MHH2**4 + 4*(2*Mh02**2 - Mh02*MHH2)*MHp2**4 -
     &    MHp2*AllHix(852) + MHp2**2*AllHix(853) -
     &    MHp2**3*AllHix(854)

        AllHix(864) = -(Mh02**3*MHH2**2) + Mh02**2*MHH2**3 +
     &    (Mh02**2 - Mh02*MHH2)*MHp2**3 +
     &    MHp2*(2*Mh02**3*MHH2 - AllHix(862)) + MHp2**2*AllHix(863)

        AllHix(868) = -3*Mh02**3*MHH2**3 + 3*Mh02**2*MHH2**4 +
     &    MHp2*AllHix(865) - MHp2**2*AllHix(866) +
     &    2*MHp2**3*AllHix(867)

        AllHix(876) = -2*C2B*m12sq*Mh02*(Mh02 - MHH2)*
     &     (Mh02 - MHp2)*(MHH2 - MHp2)*AllHix(869) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*
     &       (CBA2*AllHix(872) + SBA2*AllHix(875)))

	AllHix(879) = 2*MHH2**2 - 2*MHH2*MHp2 + MHp2**2

	AllHix(881) = CBA2*(Mh02 - MHp2) - (MHH2 - MHp2)*SBA2

        AllHix(886) = CBA2/MHH2 -
     &    ((Mh02 - MHp2)*SBA2)/(Mh02*(MHH2 - MHp2))

	AllHix(889) = Mh02**2 - (2*Mh02 - MHH2)*MHp2

	AllHix(897) = 2*MA02 - MHp2*(4 + 3*SBA2)

        AllHix(898) = MHH2*S2A*SBA2 -
     &    MHp2*(S2A + 10*CAB*SBA - 5*S2B*SBA2)

        AllHix(9) = MA02**3 - MA02**2*(2*Mh02 + MHH2) +
     &    MA02*Mh02*(Mh02 + 2*MHH2) + Mh02**2*(MHH2 - 2*MHp2) -
     &    2*(2*Mh02 - MHp2)*(MHH2 - MHp2)*MHp2

        AllHix(901) = MHH2**2*AllHix(899) -
     &    2*MHH2*(2*MA02*S2B - MHp2*AllHix(900))

        AllHix(908) = 8*C2B*CBA*m12sq*SBA*
     &     (4*MHp2 + MHH2*SBA2) + MHp2*AllHix(904) +
     &    SBA2*AllHix(907)

        AllHix(913) = 12*(1 - 4*CBA2 - 9*CBA2**2)*MA02**2 +
     &    (17 + 60*CBA2 - 297*CBA2**2)*MA02*Mh02 +
     &    (7 - 12*CBA2 + 81*CBA2**2)*Mh02**2

        AllHix(914) = 12*(1 + 11*CBA2)*MA02**2 -
     &    (47 - 267*CBA2)*MA02*Mh02 - (1 + 75*CBA2)*Mh02**2

	AllHix(915) = 36*MA02**2 + 55*MA02*Mh02 - 19*Mh02**2

	AllHix(917) = 297*S2A**2 + 84*S2A*S2B + 11*S2B**2

	AllHix(918) = (10 - 99*CBA2)*S2A - (1 + 14*CBA2)*S2B

	AllHix(921) = 81*S2A**2 + 12*S2A*S2B + 7*S2B**2

	AllHix(922) = -3*(2 - 27*CBA2)*S2A - (7 - 6*CBA2)*S2B

        AllHix(924) = Mh02*(24*S2A - 5*S2B)*S2B*SBA2 -
     &    (24*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(1 - 2*CBA2)*(CB2*Lambda6 - Lambda7*SB2)*SBA)

	AllHix(93) = S2B**2*AllHix(89) + 2*CBA*S2B*SAB*AllHix(92)

        AllHix(931) = 24*MA02**3*S2B**2*SBA -
     &    MA02*Mh02*AllHix(928) + 2*MA02**2*AllHix(929) -
     &    Mh02**2*AllHix(930)

	AllHix(938) = 4*CBA2*MA02 - (1 + CBA2)*Mh02

	AllHix(939) = -((2*MA02 - Mh02)*Mh02*S2B) + m12sq*AllHix(938)

	AllHix(940) = (4*MA02 - Mh02)*S2A - (2*MA02 - Mh02)*S2B

        AllHix(943) = -4*CAB*
     &     (m12sq - 3*CBA2*m12sq + Mh02*S2B) +
     &    S2B*SBA*AllHix(942) +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(1 - 3*CBA2)*(CB2*Lambda6 - Lambda7*SB2))

        AllHix(949) = 4*CAB*AllHix(946) +
     &    S2B*SBA*AllHix(947) +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*AllHix(948))

        AllHix(95) = 7*MA02**3*MHH2 - 5*MA02**2*MHH2**2 -
     &    17*MA02*MHH2**3 - 9*MHH2**4 -
     &    2*(9*MA02*MHH2 - 5*MHH2**2)*MHp2**2 +
     &    2*(3*MA02 - 7*MHH2)*MHp2**3 + MHp2*AllHix(94)

        AllHix(954) = -4*CAB*
     &     (m12sq*AllHix(950) - Mh02*S2B*AllHix(951)) -
     &    (2*MA02 - Mh02)*S2B*SBA*AllHix(952) +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*AllHix(953))

        AllHix(975) = 4*CAB*
     &     (MA02*Mh02*S2B - m12sq*AllHix(973)) +
     &    S2B*SBA*AllHix(974) +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*AllHix(973))

        AllHix(99) = 8*C2B*CBA*m12sq*(MA02 - 2*MHp2)*SBA +
     &    CBA2*S2B*AllHix(98)

	AllHix(999) = MA02*(MHH2 - 5*MHp2) + MHp2**2

        AllHix(10) = 2*MA02*(Mh02 - MHH2)*(Mh02 - MHp2)**2*
     &     (MHH2 - MHp2)*AllHil(1) -
     &    Mh02*(MA02 - MHH2)*AllHil(2)*AllHix(9)

        AllHix(1001) =
     &   AllHix(999)/(MA02 - MHp2)**2 -
     &    (SBA2*AllHix(1000))/(MA02 - MHH2)**2

        AllHix(1005) =
     &   -2*(5*MA02 - MHH2)*(MHH2**2*MHp2 + MHp2**3) +
     &    (MA02 - MHp2)**2*SBA2*AllHix(1000) + 2*AllHix(1004)

        AllHix(1012) =
     &   MHH2**2 - 2*MHH2*(MA02 + MHp2) + 2*(MA02**2 + MHp2**2)

        AllHix(1014) =
     &   -2*(2*m12sq - MHH2*S2B)*SAB*(4 - SBA2) +
     &    CBA*S2B**2*AllHix(1013)

	AllHix(1017) = (MHH2 + 2*MHp2)*(AllHix(1015) - 2*AllHix(1016))

	AllHix(1021) = MA02 - MHH2 + 2*(MA02 - MHp2)*SBA2

        AllHix(103) = 2*SBA2*AllHix(93) +
     &    S2B**2*SBA2**2*AllHix(95) +
     &    (MA02 - MHH2)*(MHH2 - MHp2)*AllHix(99) +
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA*AllHix(102))

	AllHix(1050) = CBA2*(MA02 - MHH2) - (MA02 - Mh02)*SBA2

	AllHix(1105) = 4*(2*MA02 - MHH2)*SAB + CBA*AllHix(1104)

	AllHix(1106) = MHH2 - (4*MA02 - MHH2)*SBA2

        AllHix(1159) =
     &   -2*CAB*(2*m12sq - Mh02*S2B) + SBA*AllHix(313) +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*(1 - 3*SBA2))

        AllHix(1214) =
     &   (CBA2*AllHix(1212))/(MA02 - Mh02)**2 -
     &    AllHix(1213)/(MA02 - MHp2)**2

        AllHix(122) = SBA2*AllHix(115) -
     &    S2B**2*SBA2**2*AllHix(117) + 2*SBA*AllHix(121)

        AllHix(1222) =
     &   CAB**2*(2*m12sq - Mh02*S2B)**2 +
     &    CB2**2*AllHix(1215)*AllHix(1216) -
     &    CAB*(2*m12sq - Mh02*S2B)*AllHix(1217) +
     &    (MW2**2*SW2**2)/(Alfa2*Pi**2)*(CBA2*Lambda7**2*SB2**2) +
     &    S2B**2*(Mh02*SBA*AllHix(1219) + 1/4.D0*AllHix(1221))

        AllHix(1225) =
     &   CAB**2*(2*m12sq - Mh02*S2B)**2 +
     &    CB2**2*AllHix(1215)*AllHix(1216) -
     &    CAB*(2*m12sq - Mh02*S2B)*AllHix(1217) +
     &    (2*(MA02 - MHp2)*AllHix(1057)**2*AllHix(1224))/
     &     ((4*MA02 - Mh02)*(Mh02 - MHp2)) +
     &    (MW2**2*SW2**2)/(Alfa2*Pi**2)*(CBA2*Lambda7**2*SB2**2) +
     &    S2B**2*(Mh02*SBA*AllHix(1219) + 1/4.D0*AllHix(1221))

        AllHix(1229) =
     &   (2*MA02 - Mh02)*AllHix(1222) +
     &    (AllHix(1057)**2*AllHix(1228))/
     &     ((4*MA02 - Mh02)*(Mh02 - MHp2)**2)

        AllHix(1237) =
     &   -32*MA02**3*(Mh02 - 4*MHp2) + 4*MA02**2*AllHix(1234) +
     &    Mh02*AllHix(1235) - MA02*AllHix(1236)

        AllHix(1238) =
     &   -24*(MA02 - Mh02)*Mh02**2*(Mh02 - MHp2) +
     &    CBA2*AllHix(1237)

        AllHix(125) = MA02*MHH2 + 2*(MA02**2 + MHH2**2) -
     &    5*(MA02 + MHH2)*MHp2 + 5*MHp2**2

        AllHix(1252) =
     &   Mh02*AllHix(1246) + (Mh02 - 4*MHp2)*AllHix(1251)

	AllHix(127) = MHH2 - MHp2*(1 - SBA2) - MA02*SBA2

	AllHix(128) = CBA*(Mh02 - MHH2)*S2B + 2*MHH2*SAB

	AllHix(1296) = MA02 - Mh02 + 2*CBA2*(MA02 - MHp2)

        AllHix(13) = MA02**3 + MA02*MHH2*(2*Mh02 + MHH2) -
     &    MA02**2*(Mh02 + 2*MHH2) - 2*(MHH2 - MHp2)**2*MHp2 +
     &    Mh02*AllHix(12)

        AllHix(1324) =
     &   -9*CBA2**4*AllHix(1322) + 4*CBA2**3*AllHix(1323)

        AllHix(1325) =
     &   Mh02**3 - Mh02**2*MHp2 - 4*Mh02*MHp2**2 + 4*MHp2**3

        AllHix(1326) =
     &   (Mh02 - 2*MHp2)*S2B**2*SBA2 -
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHix(1334) =
     &   Mh02**4*S2B**2*SBA2*AllHix(1327) -
     &    Mh02**3*S2B*AllHix(1329) - 128*MHp2**2*AllHix(1330) -
     &    32*Mh02*MHp2*AllHix(1331) - 4*Mh02**2*AllHix(1333)

        AllHix(134) = S2B**2*SBA2*AllHix(131) +
     &    CBA*(MA02 + MHH2 - 2*MHp2)*SBA*AllHix(133)

        AllHix(1376) =
     &   8*CAB*(MHH2 - MHp2)*(m12sq - 3*CBA2*m12sq + Mh02*S2B) -
     &    SBA*AllHix(1374) +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*AllHix(1375))

	AllHix(1396) = MHH2 - ((MHH2 - MHp2)*MHp2)/(Mh02 - 4*MHp2)

        AllHix(1413) =
     &   4*CAB*AllHix(1411) + Mh02*S2B*SBA*AllHix(1412) +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(Mh02 + CBA2*(Mh02 - 4*MHp2))*
     &       (CB2*Lambda6 - Lambda7*SB2))

        AllHix(150) = SBA2*AllHix(145) -
     &    2*(S2B**2*SBA2**2*AllHix(147) + SBA*AllHix(149))

        AllHix(1507) =
     &   2*m12sq*(MHH2 - MHp2)*(3*S2A + S2B) + S2B*AllHix(1506)

        AllHix(1509) =
     &   -4*(Mh02 - MHp2)*SAB*(m12sq + MHH2*S2B - 3*m12sq*SBA2) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*AllHix(1508))

        AllHix(158) = (-4*MHH2*(MHH2 - MHp2)*SBA2)/
     &     (MA02 - MHH2) + (2*AllHix(156))/(MA02 - MHp2) +
     &    MHp2*AllHix(157)

	AllHix(1635) = 1/(MA02 - MHH2) + 1/(MHH2 - MHp2)

        AllHix(1642) =
     &   S2B*SAB*SBA*AllHix(1640) +
     &    (3*MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*AllHix(1641))

	AllHix(167) = 2*MA02**2 - 10*MA02*Mh02 + 5*Mh02**2

        AllHix(1672) =
     &   -4*m12sq*SAB*(MA02 + 2*MHp2 + 3*MHH2*SBA2) +
     &    CBA*AllHix(1669) + 2*AllHix(1671)

        AllHix(168) = MA02**5 - 4*MA02**4*Mh02 +
     &    4*MA02**3*Mh02**2 + 2*MA02*Mh02**4

        AllHix(1681) =
     &   CAB*(2*m12sq - Mh02*S2B) + (Mh02 - 2*MHp2)*S2B**2*SBA

        AllHix(1682) =
     &   2*CAB*(2*m12sq - Mh02*S2B) + (Mh02 - 2*MHp2)*S2B**2*SBA

        AllHix(1686) =
     &   (2*m12sq - MHH2*S2B)*SAB*
     &     (2*m12sq*SAB + S2B*AllHix(1684)) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*AllHix(1685))

        AllHix(1688) =
     &   (Mh02 - MHp2)**2 - (Mh02 - MHp2)**2*Re(AllHil(6))

        AllHix(1697) =
     &   -8*(CBA*(2*m12sq - MHH2*S2B)*SAB +
     &       CAB*(2*m12sq - Mh02*S2B)*SBA) + S2B**2*AllHix(1696)

        AllHix(1739) =
     &   2*CBA2*AllHix(1730) + 3*Mh02**2*SBA2*Re(AllHil(15))

        AllHix(175) = MA02**3*S2B**2*SBA2 +
     &    Mh02**2*AllHix(172) - MA02*Mh02*AllHix(174)

        AllHix(1767) =
     &   2*SBA2*AllHix(1759) + 3*CBA2*MHH2**2*Re(AllHil(23))

	AllHix(1772) = (MA02 - MHp2)*(2*m12sq*S2B**2 - MHH2*S2B**3)

	AllHix(1773) = (MA02 - MHp2)*(2*m12sq*S2B**2 - Mh02*S2B**3)

        AllHix(1777) =
     &   -4*CAB**2*AllHix(1774) - S2B**4*SBA2*AllHix(1776) +
     &    4*(CAB*SBA*AllHix(1775) +
     &       (MW2*SW2)/(Alfa*Pi)*
     &        (CBA*(CB2*Lambda6 - Lambda7*SB2)*AllHix(1698)))

        AllHix(1781) =
     &   -2*CBA*SAB*AllHix(1778) +
     &    (MW2**2*SW2**2)/(Alfa2*Pi**2)*(SBA2*AllHix(358)) +
     &    2*(SAB**2*AllHix(1779) +
     &       (MW2*SW2)/(Alfa*Pi)*
     &        ((CB2*Lambda6 - Lambda7*SB2)*SBA*AllHix(1780)))

        AllHix(1787) =
     &   -2*(MA02 - CBA2*(Mh02 + 2*MHH2))*MHp2 +
     &    2*(AllHix(1728) + CBA2*AllHix(1784)) +
     &    SBA2*AllHix(1786) -
     &    AllHix(1782)*(-2*Re(AllHil(5)) + 4*Re(AllHil(10)))

        AllHix(1790) =
     &   -4*(CAB**2*AllHix(1774) + CAB*SBA*AllHix(1788)) -
     &    S2B**4*SBA2*AllHix(1789) +
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*AllHix(1682))

        AllHix(1792) =
     &   (MW2**2*SW2**2)/(Alfa2*Pi**2)*(SBA2*AllHix(358)) +
     &    2*(SAB**2*AllHix(1779) + CBA*SAB*AllHix(1791) +
     &       (MW2*SW2)/(Alfa*Pi)*
     &        ((CB2*Lambda6 - Lambda7*SB2)*SBA*AllHix(1685)))

        AllHix(1795) =
     &   -((2*MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &       (AllHix(358)*(Re(AllHil(8)) - Re(AllHil(13))))) -
     &    S2B**4*(AllHix(1794)*Re(AllHil(9)) -
     &       AllHix(1793)*Re(AllHil(14)))

        AllHix(1809) =
     &   2*(MHH2 - MHp2)*(MHH2 + MHp2)*SBA2*AllHix(1759) +
     &    3*CBA2*AllHix(1808)*Re(AllHil(23))

        AllHix(1816) =
     &   2*CBA2*(Mh02 - MHp2)*(Mh02 + MHp2)*AllHix(1730) +
     &    3*SBA2*AllHix(1815)*Re(AllHil(15))

        AllHix(184) = 2*Mh02**4 + 4*Mh02**2*MHp2**2 -
     &    4*Mh02*MHp2**3 + MHp2**4

        AllHix(191) = MHp2*SBA*AllHil(51)*
     &     (4*Mh02*AllHix(178) + S2B*SBA*AllHix(185)) +
     &    2*AllHil(47)*AllHix(190)

	AllHix(195) = Mh02**4 + Mh02**3*MHH2 + Mh02*MHH2**3 + MHH2**4

        AllHix(20) = (2 - CBA2)*MA02**2*Mh02**2 +
     &    CBA2*MA02*Mh02**3 + 3*(MA02**3*Mh02 + Mh02**4) -
     &    4*(MA02 + Mh02)*MHp2**3 - MHp2*AllHix(18) +
     &    MHp2**2*AllHix(19)

        AllHix(203) = (2*MA02 - Mh02 - MHH2)*(Mh02 - MHH2)**2*
     &     S2B*AllHix(197) - CB2*AllHix(202)

        AllHix(208) = (MA02 - MHH2)*
     &    ((Mh02*(MA02 + MHH2))/(MA02 - Mh02) -
     &      ((MA0 - Mh0)*(MA0 + Mh0)*AllHix(207))/
     &       (Mh02*(Mh02 - MHH2)))

        AllHix(218) = CBA2*(MA02 - Mh02)*Mh02 -
     &    2*(MA02 - MHp2)**2*SBA2

        AllHix(224) = MA02**4 - 3*Mh02**4 + 18*Mh02**3*MHp2 -
     &    24*Mh02**2*MHp2**2 + 6*Mh02*AllHix(222) -
     &    2*(MA02**3*(3*Mh02 - MHp2) + MHp2**4 - MA02*AllHix(223))

        AllHix(24) = Mh02*AllHix(22) -
     &    MA02*(2*Mh02*MHp2 + CBA2*AllHix(23))

        AllHix(242) = MA02**2*AllHix(228) - MA02*AllHix(233) +
     &    MA02**3*AllHix(236) - Mh02*AllHix(241)

        AllHix(250) = 4*C2B*CBA*m12sq*Mh02*SBA*AllHix(243) +
     &    MA02*AllHix(245) + MA02**2*AllHix(247) -
     &    (Mh02 - MHp2)*AllHix(249)

	AllHix(258) = -2/(Mh02 - MHH2) + (1 - MA02/Mh02)/MHH2

        AllHix(262) = CBA2*(MA02 - Mh02)**2*(MA02 - MHH2)**2*
     &     (Mh02 - MHH2) + MHH2*SBA2*AllHix(261)

        AllHix(267) = CBA2*AllHix(263) - 2*CBA*AllHix(264) +
     &    ((MA02 - Mh02)*SBA2*AllHix(266))/(Mh02*(MA02 - MHH2))

        AllHix(27) = MA02**3 - Mh02*MHp2**2 -
     &    MA02**2*(2*Mh02 + MHp2) + MA02*AllHix(26)

        AllHix(274) = -(Mh02**2*(MA02**3 + MHH2**3)) +
     &    AllHix(271) - 2*AllHix(272) + AllHix(273)

        AllHix(278) = -3*(MA02*Mh02 - Mh02**2)*MHH2**4 +
     &    2*MHH2*AllHix(275) - MHH2**2*AllHix(276) +
     &    MHH2**3*AllHix(277)

        AllHix(283) = MA02**5*Mh02 - 2*MA02**4*Mh02**2 +
     &    MA02**3*Mh02**3 - MHH2*AllHix(279) -
     &    3*MHH2**2*AllHix(280) - MHH2**3*AllHix(281) +
     &    MHH2**4*AllHix(282)

	AllHix(293) = -(S2B**2*SBA2*AllHix(287)) + S2B*AllHix(292)

        AllHix(30) = -3*(MA02 - MHp2)**2*(Mh02 - MHp2)**2*
     &     SBA2 + Mh02*AllHix(29)

        AllHix(302) = -2*C2B*m12sq*(MA02 - Mh02)*Mh02*
     &     (MA02 - MHH2)*(Mh02 - MHH2)*AllHix(294) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*
     &       (CBA2*AllHix(298) - SBA2*AllHix(301)))

        AllHix(305) = ((MA02 - Mh02)*MHH2)/
     &     (Mh02*(MA02 - MHH2)) +
     &    ((MA0 - MHH)*(MA0 + MHH)*AllHix(304))/MHH2

        AllHix(319) = 4*m12sq*(MA02 - Mh02) -
     &    (2*MA02 - 3*Mh02)*Mh02*S2B

        AllHix(321) = -(Mh02**4*MHH2) -
     &    (MA02 - Mh02)*MHH2**4 + MA02*(Mh02**4 + Mh02**3*MHH2) +
     &    MA02**3*(Mh02**2 - MHH2**2) +
     &    Mh02**3*(-2*MA02**2 + MHH2**2) + MHH2**3*AllHix(320)

        AllHix(324) = 6*MA02**4*Mh02 + 2*MA02**3*Mh02**2 +
     &    MA02*(Mh02**4 + MHH2**4) -
     &    4*(MA02**5 + MA02**2*(Mh02**3 + MHH2**3)) +
     &    MHH2*AllHix(322) + MHH2**2*AllHix(323)

        AllHix(326) = MA02**4 + Mh02*MHH2**3 -
     &    MA02**3*(Mh02 + 3*MHH2) +
     &    3*MA02**2*(Mh02*MHH2 + MHH2**2) -
     &    MA02*(3*Mh02*MHH2**2 + MHH2**3)

        AllHix(327) = -2*(MA02**4 - MA02**3*Mh02)*MHH2 +
     &    7*(MA02**3 - MA02**2*Mh02)*MHH2**2 -
     &    8*(MA02**2 - MA02*Mh02)*MHH2**3 + 3*(MA02 - Mh02)*MHH2**4

	AllHix(336) = -2*AllHix(335) + (MW2*SW2)/(Alfa*Pi)*AllHix(333)

	AllHix(34) = -2*Mh02**2 - 3*(MA02**2 + MHp2**2) + 2*AllHix(33)

        AllHix(344) = S2B*(2*CAB*Mh02 + SBA*AllHix(308)) -
     &    CBA*(4*C2B*m12sq -
     &       (2*MW2*SW2)/(Alfa*Pi)*(CB2*Lambda6 - Lambda7*SB2))

        AllHix(35) = CAB*(2*m12sq - Mh02*S2B) -
     &    (MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

        AllHix(359) = S2B**4*AllHix(356) -
     &    (2*MW2**2*SW2**2)/(Alfa2*Pi**2)*AllHix(358)

        AllHix(361) = CBA*(2*MA02 + MHH2 - 4*MHp2)*S2B**2 +
     &    2*((2*m12sq - MHH2*S2B)*SAB +
     &       (MW2*SW2)/(Alfa*Pi)*((CB2*Lambda6 - Lambda7*SB2)*SBA))

        AllHix(365) = SAB*SBA*AllHix(362) +
     &    2*MA02*AllHix(364) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*AllHix(363))

        AllHix(371) = S2B**4*SBA*AllHix(367) -
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (2*AllHix(368) - Lambda7*AllHix(370))

        AllHix(4) = Mh02*(MHH2 - MHp2)*AllHil(33) -
     &    MHH2*(Mh02 - MHp2)*AllHil(38)

        AllHix(404) = CBA2**2*S2B**2*AllHix(391) -
     &    2*CBA2*S2B*AllHix(400) -
     &    (MA02 - Mh02)*(Mh02 - MHp2)*AllHix(403)

        AllHix(41) = (-3*MHH2**2)/(MA02 - MHp2) +
     &    MHH2*AllHix(39) -
     &    (2*(MHH2 - MHp2)*SBA2*AllHix(40))/(MA02 - MHH2)**2

        AllHix(430) = CBA2*Mh02**2*S2B**2 +
     &    4*Mh02*SBA*AllHix(178) - S2B*SBA2*AllHix(429)

        AllHix(438) = S2B**2*AllHix(432) +
     &    2*MA02**2*Mh02*AllHix(433) -
     &    MA02*Mh02**2*(Mh02*S2B*AllHix(434) + 2*AllHix(435)) -
     &    Mh02**3*(Mh02*S2B*AllHix(436) - 4*AllHix(437))

        AllHix(444) = -(Mh02*(dup2 - 24*C2B*CBA*m12sq*SBA)) -
     &    MA02*S2B*SBA2*(2*AllHix(439) - Mh02*AllHix(440))

        AllHix(45) = -2*MA02**2*MHH2**2 +
     &    4*(MA02 + MHH2)*MHp2**3 + MHp2*AllHix(42) -
     &    3*(MHH2*(MA02**3 + MHH2**3) + MHp2**2*AllHix(43)) +
     &    (MA02 - MHH2)*(MA02 - MHp2)*SBA2*AllHix(44)

	AllHix(472) = 2*CAB*Mh02*SBA - (Mh02 - MHH2)*S2B*SBA2

        AllHix(479) = 6*MA02**3*S2B**2*(3 + SBA2) -
     &    MA02*Mh02*(Mh02*S2B*AllHix(474) + 4*AllHix(475)) +
     &    Mh02**2*(4*AllHix(437) - Mh02*S2B*AllHix(476)) +
     &    2*MA02**2*(Mh02*S2B*AllHix(477) + 4*AllHix(478))

        AllHix(48) = MA02*(MHH2 - MHp2)**2*SBA2 +
     &    (MA02 - MHp2)*AllHix(47)

        AllHix(480) = 24*C2B*CBA*m12sq*SBA -
     &    Mh02*S2B*SBA2*AllHix(441) + 2*AllHix(443)

        AllHix(482) = MA02**2*S2B**2 +
     &    Mh02*(Mh02*S2B**2 - 3*SBA*AllHix(171)) + MA02*AllHix(481)

        AllHix(486) = -4*MHp2**2*(3*SBA2 - 8*CW2*SW2) +
     &    MHH2*MHp2*AllHix(485) + 2*(1 - 2*SW2)**2*MHH2**2

	AllHix(499) = 2*MA02*S2B - MHp2*(S2A + 4*S2B - 2*CBA*SAB)

        AllHix(5) = Mh02*(MA02 - MHH2)*AllHil(47) -
     &    (MA02 - Mh02)*MHH2*AllHil(49)

        AllHix(505) = MHp2**3*S2B*(MA02 - MHp2*(2 - SBA2)) -
     &    Mh02*(MHH2 - MHp2)*AllHix(501) + MHp2*AllHix(504)

        AllHix(51) = -3*CBA2*(MA02 - MHp2)**2*
     &     (MHH2 - MHp2)**2 + MHH2*SBA2*AllHix(50)

        AllHix(514) = -4*C2B*CBA*m12sq*SBA*AllHix(507) +
     &    MHp2*AllHix(509) - SBA2*AllHix(513)

        AllHix(519) = 2*m12sq*SAB +
     &    (MW2*SW2)/(Alfa*Pi)*((CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHix(521) = 2*MHH2**4 + 4*MHH2**2*MHp2**2 -
     &    4*MHH2*MHp2**3 + MHp2**4

        AllHix(53) = (2*m12sq - MHH2*S2B)*SAB +
     &    (MW2*SW2)/(Alfa*Pi)*((CB2*Lambda6 - Lambda7*SB2)*SBA)

	AllHix(531) = 2*MA02**2 - 10*MA02*MHH2 + 5*MHH2**2

        AllHix(532) = MA02**5 - 4*MA02**4*MHH2 +
     &    4*MA02**3*MHH2**2 + 2*MA02*MHH2**4

        AllHix(553) = 4*m12sq*(Mh02 - MHp2) -
     &    Mh02*(3*Mh02 - 2*MHp2)*S2B

	AllHix(57) = 2*CBA2*(MA02 - Mh02) + (MA02 - MHH2)*SBA2

        AllHix(603) = CBA2*AllHix(594) +
     &    (MA02 - MHH2)*(CBA2**2*(MA02 - Mh02)**2*AllHix(583) -
     &       (MA02 - Mh02)*SBA2*AllHix(602))

	AllHix(606) = 2*CBA2*(Mh02 - MHp2) + (MHH2 - MHp2)*SBA2

	AllHix(607) = CBA2*(Mh02 - MHp2) + 2*(MHH2 - MHp2)*SBA2

	AllHix(615) = S2A*S2B*AllHix(610) - S2B**2*AllHix(614)

        AllHix(617) = -(MHH2**3*MHp2) +
     &    3*(Mh02*MHH2 + MHH2**2)*MHp2**2 -
     &    (Mh02 + 3*MHH2)*MHp2**3 + MHp2**4 +
     &    Mh02*(MHH2**3 - 3*MHH2**2*MHp2)

        AllHix(620) = -3*MHH2**4*MHp2 -
     &    (2*Mh02*MHH2 + 7*MHH2**2)*MHp2**3 + 2*MHH2*MHp2**4 +
     &    MHp2**2*AllHix(618) + Mh02*AllHix(619)

        AllHix(627) = 2*C2B*m12sq*(Mh02 - MHp2)*(MHH2 - MHp2)*
     &     AllHix(621) + (MW2*SW2)/(Alfa*Pi)*AllHix(626)

        AllHix(63) = (2*CBA2*(MA02 - Mh02))/(MA02 - MHH2) -
     &    ((MA02 - Mh02 - MHH2)*SBA2)/Mh02

        AllHix(64) = (CBA2*(MA02 - Mh02 - MHH2))/MHH2 -
     &    (2*(MA02 - MHH2)*SBA2)/(MA02 - Mh02)

	AllHix(647) = CBA2*AllHix(637) + (MHH2 - MHp2)*AllHix(646)

	AllHix(651) = 6*MHH2**2*S2A + (MA02 + MHH2)**2*S2B

	AllHix(652) = MHH2*S2B**2*SBA2 - 4*CBA*AllHix(489)

        AllHix(66) = (CBA2*(MA02 - Mh02)*(MA02 - Mh02 + MHH2))/
     &     ((MA02 - MHH2)*MHH2) - ((MA02 + Mh02 - MHH2)*SBA2)/Mh02

        AllHix(664) = SBA2*AllHix(655) +
     &    2*MA02**2*MHH2*AllHix(658) + MA02*MHH2**2*AllHix(661) -
     &    MA02**3*(Mh02*AllHix(662) - 2*AllHix(663))

        AllHix(670) = 4*C2B*CBA*m12sq*SBA*AllHix(665) +
     &    2*MA02*AllHix(667) + SBA2*AllHix(669)

	AllHix(672) = 6*CBA2*(Mh02 - MHH2) + (MA02 - MHH2)*SBA2

        AllHix(68) = (CBA2*(MA02 - Mh02 + MHH2))/MHH2 -
     &    ((MA02 - MHH2)*(MA02 + Mh02 - MHH2)*SBA2)/
     &     ((MA02 - Mh02)*Mh02)

        AllHix(682) = SBA2*AllHix(675) +
     &    MA02*MHH2*AllHix(678) + 2*MA02**2*AllHix(681)

        AllHix(688) = 8*C2B*CBA*m12sq*SBA*
     &     (3*MA02 + MHH2*SBA2) +
     &    MA02*(CBA2**2*(3*Mh02 - 2*MHH2)*S2B**2 -
     &       2*(MHH2*S2B*SBA2*AllHix(683) + 2*CBA*AllHix(684))) -
     &    SBA2*AllHix(687)

        AllHix(702) = CBA2**2*(3*Mh02 - 2*MHH2)*
     &     (MA02 - MHH2)**2*S2B**2 + CBA2*S2B*AllHix(696) +
     &    2*(MA02 - MHH2)*AllHix(701)

	AllHix(71) = MA02*Mh02*(Mh02 - 3*MHH2) + MA02**2*MHH2

        AllHix(719) = CBA2*Mh02**2*S2B**2 +
     &    4*Mh02*SBA*AllHix(178) - S2B*SBA2*AllHix(718)

	AllHix(72) = Mh02**2 - 2*Mh02*MHH2 - MHH2**2

        AllHix(728) = Mh02**3*S2B*AllHix(721) -
     &    2*(MHp2**2*(4*SBA*AllHix(171) - S2B**2*AllHix(722)) +
     &       Mh02*MHp2*(3*MHp2*S2B*AllHix(723) + 2*AllHix(724))) -
     &    Mh02**2*AllHix(727)

        AllHix(734) = 17*Mh02**2*S2B**2 - MHp2*AllHix(732) +
     &    2*Mh02*AllHix(733)

        AllHix(742) = 8*Mh02**4*S2B**2 -
     &    2*Mh02*MHp2**2*AllHix(737) +
     &    Mh02**2*MHp2*(2*AllHix(724) + MHp2*S2B*AllHix(738)) +
     &    2*AllHix(741)

        AllHix(749) = CBA2**2*(4*Mh02 - 3*MHp2)*
     &     (Mh02 - MHp2)**2*S2B**2 - (Mh02 - MHp2)*AllHix(748)

        AllHix(76) = CBA2*(MA02 - Mh02)*Mh02*(MA02 - MHH2) -
     &    SBA2*AllHix(75)

        AllHix(775) = Mh02**2*MHH2 - MHH2**2*(MHH2 - 2*MHp2) +
     &    2*Mh02*AllHix(774)

        AllHix(779) = -((MHH2**2*AllHil(37)*AllHix(777))/
     &       (Mh02*(MHH2 - MHp2))) +
     &    (Mh02**2*AllHil(32)*AllHix(778))/(MHH2*(Mh02 - MHp2))

	AllHix(79) = CBA2*(Mh02 - MHH2)*S2B + 2*CBA*MHH2*SAB

        AllHix(8) = MA02**2 - MA02*(Mh02 + MHH2) -
     &    Mh02*(MHH2 - 2*MHp2) + 2*(MHH2 - MHp2)*MHp2

        AllHix(80) = -2*C2B*m12sq +
     &    (MW2*SW2)/(Alfa*Pi)*(CB2*Lambda6 - Lambda7*SB2)

	AllHix(800) = MHp2*(1 - SBA2) - 2*MHH2*(2 + SBA2)

        AllHix(802) = 2*MHH2**4*MHp2**2 + 4*MHH2**2*MHp2**4 -
     &    4*MHH2*MHp2**5 + MHp2**6

        AllHix(803) = AllHix(486)/(MHH2 - 4*MHp2) +
     &    2*(1 - 2*SW2)**2*(MHH2 + MHp2)

	AllHix(806) = 8*CBA2*(Mh02 - MHH2) - 3*(MHH2 - MHp2)*SBA2

        AllHix(811) = 2*Mh02**2*MHH2**2 +
     &    4*Mh02*MHH2*MHp2**2 - 2*(Mh02 + MHH2)*MHp2**3 + MHp2**4

        AllHix(814) = MHH2*(Mh02 - MHp2)*(MHH2 - MHp2)*SBA2 +
     &    CBA2*AllHix(813)

        AllHix(817) = MHH2 + MHp2 -
     &    (MHH2 - MHp2)*((Mh02*(MHH2 + MHp2))/
     &        (MHH2*(Mh02 - MHp2)) +
     &       (Mh0 - MHp)*(Mh0 + MHp)*AllHix(816))

        AllHix(822) = -((Mh02 - MHH2)*(Mh02 - MHp2)**2*
     &       (MHH2 - MHp2)**2*SBA2) + CBA2*Mh02*AllHix(821)

        AllHix(823) = (CBA2*(MHH2 - MHp2))/
     &     (MHH2*(Mh02 - MHp2)) - SBA2/Mh02

        AllHix(824) = -((Mh02 - 2*MHp2)*S2B**2*SBA2) -
     &    (CBA2*(MHH2 - MHp2)*AllHix(213))/(Mh02 - MHp2) +
     &    SBA*(-2*CAB*(2*m12sq - Mh02*S2B) +
     &       (2*MW2*SW2)/(Alfa*Pi)*
     &        (CBA*(CB2*Lambda6 - Lambda7*SB2)))

        AllHix(826) = CBA2*(Mh02 - MHp2) +
     &    ((MHH2 - MHp2)*(Mh02 + MHH2 - MHp2)*SBA2)/Mh02

        AllHix(839) = AllHix(837)/
     &     ((Mh02 - MHH2)*(MHH2 - MHp2)**2) +
     &    ((Mh02 - MHp2)*(2*CBA*AllHix(264) + CBA2*AllHix(838)))/
     &     S2B**2

	AllHix(85) = -2*MHH2**2 - 3*(MA02**2 + MHp2**2) + 2*AllHix(84)

        AllHix(851) = Mh02**3*MHH2**3 - Mh02**2*MHH2**4 -
     &    (Mh02 - MHH2)*MHp2**5 - MHp2*AllHix(846) +
     &    MHp2**2*AllHix(848) - MHp2**3*AllHix(849) +
     &    MHp2**4*AllHix(850)

        AllHix(877) = SAB*SBA2*
     &     (4*m12sq*AllHix(864) - S2B*AllHix(868)) +
     &    SBA*AllHix(876)

        AllHix(880) = (Mh02*(MHH2 - MHp2))/(Mh02 - MHp2) +
     &    ((Mh02 - MHp2)*AllHix(879))/(Mh02*(MHH2 - MHp2))

        AllHix(894) = 2*(MHH2*S2B*SAB - SBA*AllHix(80)) +
     &    CBA*S2B*AllHix(716)

	AllHix(895) = 6*MHH2**2*S2A + (MHH2 + MHp2)**2*S2B

        AllHix(902) = MHH2**3*(28*S2A + 25*S2B)*SBA2 +
     &    2*MHp2**2*S2B*AllHix(897) -
     &    4*Mh02*(MHH2 - MHp2)*AllHix(898) + MHp2*AllHix(901)

        AllHix(909) = 2*CBA2**2*(2*Mh02 + MHH2)*MHp2*S2B**2 +
     &    AllHix(908)

        AllHix(916) = m12sq**2*AllHix(913) +
     &    m12sq*Mh02*S2B*AllHix(914) - Mh02**2*S2B**2*AllHix(915)

	AllHix(919) = 2*CB2**2*Lambda6**2 - Lambda7*AllHix(369)

        AllHix(920) = Mh02**2*S2B**2*SBA2*AllHix(917) -
     &    (12*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*Mh02*S2B*(CB2*Lambda6 - Lambda7*SB2)*SBA*
     &       AllHix(918)) -
     &    (2*MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (CBA2*(17 + 60*CBA2 - 297*CBA2**2)*AllHix(919))

        AllHix(923) = Mh02**2*S2B**2*SBA2*AllHix(921) +
     &    (2*MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (CBA2*(7 - 12*CBA2 + 81*CBA2**2)*AllHix(919)) +
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*Mh02*S2B*(CB2*Lambda6 - Lambda7*SB2)*SBA*
     &       AllHix(922))

        AllHix(941) = -4*CAB*AllHix(939) +
     &    Mh02*S2B*SBA*AllHix(940) +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*AllHix(938))

	AllHix(1) = 4*MA02**2 - 15*MA02*MHp2 + 6*MHp2**2

        AllHix(1049) =
     &   -((Mh02 + 2*MHH2)*S2A*S2B) + 2*m12sq*(3*S2A + S2B) +
     &    (6*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHix(11) = -3*(MA02 - Mh02)*(MA02 - MHH2)*
     &     (Mh02 - MHH2)*(Mh02 - MHp2)*AllHix(8) + 4*AllHix(10)

        AllHix(123) = -(AllHix(103)/(MHH2 - MHp2)) +
     &    (4*AllHil(4)*AllHix(122))/(MHH2 - MHp2)**2

        AllHix(126) = 2*CBA2*(MA02 - MHp2)**2*(MHH2 - MHp2) +
     &    MHH2*SBA2*AllHix(125)

        AllHix(129) = -4*C2B*m12sq +
     &    S2B*(2*CAB*CBA*Mh02 - SBA*AllHix(128)) +
     &    (2*MW2*SW2)/(Alfa*Pi)*(CB2*Lambda6 - Lambda7*SB2)

        AllHix(130) = (MA02 - MHH2)*S2B**4*SBA2*AllHix(127) +
     &    CBA*(MA02 - MHp2)*S2B**2*SBA*AllHix(129)

        AllHix(14) = -((MA02 - Mh02)*(MA02 - MHH2)**2*
     &       (Mh02 - MHH2)*MHp2*AllHil(4)) +
     &    MHH2*(Mh02 - MHp2)*AllHil(3)*AllHix(13)

        AllHix(151) = (2*AllHil(4)*AllHix(134))/
     &     (MA02 - MHH2) - AllHix(150)/(MA02 - MHH2)**2

	AllHix(153) = 1/(MA02 - MHp2) + (2*SBA2)/(MA02 - MHH2)

        AllHix(154) = 2*CBA2*(MA02 - MHp2)**2 -
     &    MHH2*(MA02 - 2*MHH2 + MHp2)*SBA2

	AllHix(16) = CBA2**2*Mh02 + MHH2*SBA2**2

        AllHix(1634) =
     &   (MA02*AllHil(1))/(MA02 - MHH2) -
     &    (Mh02*AllHil(2))/(Mh02 - MHH2)

        AllHix(1636) =
     &   (Mh02*AllHil(2))/(Mh02 - MHp2) -
     &    MHH2*AllHil(3)*AllHix(1635)

        AllHix(1638) =
     &   2*MA02**2 + 3*Mh02*MHH2 + 4*MHp2**2 -
     &    2*(Mh02 + MHH2)*(MA02 + 2*MHp2)

        AllHix(1683) =
     &   (Mh02 - 2*MHp2)**2*S2B**4*SBA2 +
     &    4*CAB*(2*m12sq - Mh02*S2B)*AllHix(1681) +
     &    (2*MW2**2*SW2**2)/(Alfa2*Pi**2)*(CBA2*AllHix(919)) -
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*AllHix(1682))

        AllHix(1687) =
     &   CBA2*(MHH2 - 2*MHp2)**2*S2B**4 + 4*AllHix(1686) +
     &    (2*MW2**2*SW2**2)/(Alfa2*Pi**2)*(SBA2*AllHix(919))

        AllHix(169) = Mh02*AllHil(32)*AllHix(167) +
     &    (AllHil(31)*AllHix(168))/Mh02**2

	dup3 = AllHix(1683)*Re(AllHil(8)) + AllHix(1687)*Re(AllHil(9))

        AllHix(1707) =
     &   dup3/S2B**4 + (MA02 - MHp2)**2*Re(AllHil(5)) +
     &    CBA2*(Mh02 - MHp2)**2*Re(AllHil(6)) +
     &    (MHH2 - MHp2)**2*SBA2*Re(AllHil(7))

        AllHix(1710) =
     &   -(((MA02 - MHp2)*AllHix(1697))/S2B**2) + 2*AllHix(1707)

        AllHix(176) = -2*CAB*(2*CBA2*m12sq - Mh02*S2B) +
     &    Mh02*S2A*S2B*SBA +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*CBA2*(CB2*Lambda6 - Lambda7*SB2))

        AllHix(1796) =
     &   MHp2**2*(S2B**4 - 5*CBA2*S2B**4) +
     &    MA02**2*(S2B**4 + 5*CBA2*S2B**4) -
     &    8*(CBA*SAB*AllHix(1772) + CAB*SBA*AllHix(1773)) +
     &    S2B**4*AllHix(1787) +
     &    2*(CBA2*AllHix(1795) + AllHix(1790)*Re(AllHil(8))) -
     &    4*AllHix(1792)*Re(AllHil(9)) -
     &    2*AllHix(1777)*Re(AllHil(13)) +
     &    4*AllHix(1781)*Re(AllHil(14))

	AllHix(182) = 5*Mh02**2 - 10*Mh02*MHp2 + 2*MHp2**2

        AllHix(183) = -2*CAB*(2*m12sq - Mh02*S2B) -
     &    (Mh02 - 2*MHp2)*S2B**2*SBA +
     &    (2*MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

        AllHix(192) = (MHp2*SBA2*AllHil(50)*AllHix(184))/
     &     (Mh02 - MHp2)**2 + AllHix(191)/S2B**2

	AllHix(194) = -2*Mh02*MHH2 + 3*(Mh02**2 + MHH2**2)

        AllHix(2) = 14*MA02 - 3*MHp2 +
     &    (4*(MA02 + 2*MHp2)*AllHil(4))/MHp2

        AllHix(204) = MA02*AllHix(195) +
     &    ((MA02 - Mh02)*(MA02 - MHH2)*AllHix(203))/
     &     (Mh02*MHH2*S2B**3)

	AllHix(206) = CBA2*(MA02 - Mh02) + 2*(MA02 - MHH2)*SBA2

        AllHix(21) = AllHil(4)**2 +
     &    (AllHil(33)*AllHix(20))/((MA02 - MHp2)*(Mh02 - MHp2)**2)

        AllHix(25) = (Mh02*(MA02 - 3*Mh02 + 2*MHp2))/
     &     (MA02 - MHp2) - (2*AllHix(24))/(MA02 - Mh02)**2

        AllHix(251) = CBA2**2*Mh02**2*S2B**2*AllHix(224) +
     &    CBA2*Mh02*S2B*AllHix(242) +
     &    (MA02 - Mh02)*(Mh02 - MHp2)*AllHix(250)

        AllHix(259) = MA02 + Mh02 -
     &    (MA02 - Mh02)*(((MA02 + Mh02)*MHH2)/
     &        (Mh02*(MA02 - MHH2)) +
     &       (MA0 - MHH)*(MA0 + MHH)*AllHix(258))

        AllHix(268) = AllHix(262)/
     &     ((MA02 - MHH2)**2*(Mh02 - MHH2)) +
     &    ((MA02 - Mh02)*AllHix(267))/S2B**2

        AllHix(303) = -(S2B**2*SBA2**2*AllHix(283)) +
     &    CBA2*AllHix(293) +
     &    2*CBA*(SAB*SBA2*
     &        (4*m12sq*AllHix(274) + S2B*AllHix(278)) +
     &       SBA*AllHix(302))

        AllHix(31) = (2*CBA2*AllHix(27))/(MA02 - Mh02)**2 -
     &    AllHix(30)/(Mh02*(Mh02 - MHp2)**2)

        AllHix(32) = -2*CAB*(2*m12sq - Mh02*S2B) +
     &    (2*MA02 - Mh02)*S2B**2*SBA +
     &    (2*MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

        AllHix(325) = 2*CAB*(MA02 - Mh02)**2*(MA02 - MHH2)*SBA*
     &     AllHix(319) + SBA2*
     &     (S2A*S2B*AllHix(321) + S2B**2*AllHix(324))

        AllHix(337) = SAB*SBA2*
     &     (4*m12sq*AllHix(326) + S2B*AllHix(327)) +
     &    SBA*AllHix(336)

	AllHix(36) = -(S2B**2*SBA2*AllHix(34)) + 4*Mh02*SBA*AllHix(35)

        AllHix(360) = -(SBA*AllHix(359)) +
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     ((2*m12sq - MHH2*S2B)*SAB*(CB2*Lambda6 - Lambda7*SB2))

        AllHix(366) = CAB*(2*m12sq - Mh02*S2B)*AllHix(361) -
     &    S2B**2*AllHix(365)

        AllHix(445) = -4*CBA2**2*(MA02 - Mh02)**2*Mh02*
     &     (MA02 + Mh02)*S2B**2 + CBA2*AllHix(438) +
     &    MA02*(MA02 - Mh02)*AllHix(444)

        AllHix(46) = AllHil(4)**2 + AllHil(49)*AllHix(41) -
     &    (AllHil(38)*AllHix(45))/((MA02 - MHp2)*(MHH2 - MHp2)**2)

	dup4 = MHH2*S2B*SAB*SBA2 - SBA*AllHix(80)

        AllHix(473) = -12*CBA*dup4 +
     &    CBA2**2*(MA02 - Mh02)*S2B**2 + 6*CBA2*S2B*AllHix(472)

        AllHix(483) = CBA2*AllHix(479) +
     &    2*(MA02 - Mh02)*(MA02*AllHix(480) +
     &       8*CBA2**2*AllHix(482))

        AllHix(506) = -(MHH2**4*(4*S2A + 7*S2B)*SBA2) -
     &    2*MHH2*MHp2**2*AllHix(499) + 2*AllHix(505)

        AllHix(515) = CBA2**2*(2*Mh02 + MHH2)*(MHH2 - MHp2)**2*
     &     MHp2*S2B**2 - MHH2*(MHH2 - MHp2)*AllHix(514)

	AllHix(517) = 5*MHH2**2 - 10*MHH2*MHp2 + 2*MHp2**2

	AllHix(518) = CBA2*(MHH2 - 2*MHp2)*S2B**2 + 2*CBA*AllHix(53)

        AllHix(52) = (2*SBA2*AllHix(48))/(MA02 - MHH2)**2 -
     &    AllHix(51)/(MHH2*(MHH2 - MHp2)**2)

	AllHix(520) = MHH2*S2B*(CBA*S2A - 2*SAB) + 2*SBA2*AllHix(519)

        AllHix(533) = MHH2*AllHil(37)*AllHix(531) +
     &    (AllHil(31)*AllHix(532))/MHH2**2

	AllHix(54) = CBA2*(2*MA02 - MHH2)*S2B**2 - 2*CBA*AllHix(53)

	AllHix(56) = MA02*(Mh02 - MHH2) + Mh02*(2*Mh02 + MHH2)

        AllHix(564) = m12sq*S2A +
     &    (MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

	AllHix(58) = 2*m12sq + (2*Mh02 + MHH2)*S2A

        AllHix(6) = (MA0 - Mh0)*(MA0 + Mh0)*(MA02 - Mh02)*
     &     (MA0 - MHH)*(MA0 + MHH)*(MA02 - MHH2)*AllHix(4) +
     &    2*(Mh0 - MHp)*(MHH - MHp)*(Mh0 + MHp)*(MHH + MHp)*
     &     (Mh02 - MHp2)*(MHH2 - MHp2)*AllHix(5)

        AllHix(616) = -2*CAB*(Mh02 - MHp2)**2*(MHH2 - MHp2)*
     &     SBA*AllHix(553) + SBA2*AllHix(615)

        AllHix(62) = CBA2*(MA02 - Mh02) -
     &    ((MA02 - MHH2)*(MA02 - Mh02 - MHH2)*SBA2)/Mh02

        AllHix(628) = SAB*SBA2*
     &     (4*m12sq*AllHix(617) - S2B*AllHix(620)) +
     &    SBA*AllHix(627)

        AllHix(648) = (8*CBA2*(MHH - MHp)*(MHH + MHp)*SBA2*
     &       AllHil(2)**2*(S2B*AllHix(58) - 6*AllHix(564)))/Mh02 -
     &    AllHix(647)/(MHH2 - MHp2)

        AllHix(65) = CBA*(2*MA02 - MHH2)*S2B**2 -
     &    2*((2*m12sq - MHH2*S2B)*SAB +
     &       (MW2*SW2)/(Alfa*Pi)*((CB2*Lambda6 - Lambda7*SB2)*SBA))

        AllHix(67) = (Mh02 + 2*MHH2)*S2A*S2B -
     &    2*m12sq*(3*S2A + S2B) -
     &    (6*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHix(671) = CBA2**2*MA02*(3*Mh02 - 2*MHH2)*
     &     (MA02 - MHH2)**2*S2B**2 - CBA2*S2B*AllHix(664) -
     &    2*(MA02 - MHH2)*MHH2*AllHix(670)

        AllHix(673) = -12*SBA*
     &     (CAB*CBA2*Mh02*S2B + CBA*AllHix(80)) +
     &    S2B*SBA2*(12*CBA*MHH2*SAB + S2B*AllHix(672))

        AllHix(689) = CBA2*S2B*AllHix(682) +
     &    2*(MA02 - MHH2)*AllHix(688)

        AllHix(7) = 2*MA02*(Mh02 - MHp2)**2*(MHH2 - MHp2)**2 -
     &    (MA02 - Mh02)**2*(MA02 - MHH2)**2*MHp2

        AllHix(73) = -((MA02 - Mh02)*(MA02 - MHH2)*MHH2*
     &       SBA2) + CBA2*(2*AllHix(71) - Mh02*AllHix(72))

        AllHix(776) = CBA2*Mh02*(Mh02 - MHp2)*(MHH2 - MHp2) -
     &    SBA2*AllHix(775)

	AllHix(801) = MHp2**2*SBA2**2 + MHp2*SBA2*AllHix(800)

        AllHix(804) = (CBA2*AllHix(802))/
     &     (MHH2**2*(MHH2 - MHp2)**2) + 2*MHH2*AllHix(803)

        AllHix(807) = -16*SBA*
     &     (CAB*CBA2*Mh02*S2B + CBA*AllHix(80)) +
     &    S2B*SBA2*(16*CBA*MHH2*SAB + S2B*AllHix(806))

        AllHix(81) = S2B*SBA2*AllHix(79) -
     &    2*SBA*(CAB*CBA2*Mh02*S2B + CBA*AllHix(80))

        AllHix(815) = (CBA2*MHp2*AllHil(50)*AllHix(811))/
     &     Mh02 + (AllHil(44)*AllHix(67)*AllHix(814))/S2B**2

        AllHix(825) = CBA2*AllHix(817) +
     &    AllHix(822)/(Mh02*(Mh02 - MHH2)*(Mh02 - MHp2)**2) -
     &    ((MHH2 - MHp2)*(AllHix(67)*AllHix(823) -
     &         AllHix(824)/Mh02))/S2B**2

        AllHix(83) = CBA*(MHH2 - 2*MHp2)*S2B**2 +
     &    2*((2*m12sq - MHH2*S2B)*SAB +
     &       (MW2*SW2)/(Alfa*Pi)*((CB2*Lambda6 - Lambda7*SB2)*SBA))

	AllHix(86) = 4*CBA*MHH2*AllHix(53) - CBA2*S2B**2*AllHix(85)

        AllHix(896) = (2*CBA*AllHix(894))/(MHH2 - MHp2) +
     &    (MHH2*AllHix(652) + CBA2*S2B*AllHix(895))/MHH2**2

        AllHix(910) = CBA2*S2B*AllHix(902) -
     &    2*(MHH2 - MHp2)*AllHix(909)

	AllHix(912) = 4*MA02**2 - 5*MA02*Mh02 + Mh02**2

        AllHix(124) = 8*CBA*AllHil(57)*AllHix(83) -
     &    (8*AllHil(56)*AllHix(86))/MHH2 +
     &    AllHix(123)/(MA02 - MHH2)

        AllHix(15) = -8*AllHix(6) +
     &    8*(Mh02 - MHH2)*AllHil(46)*AllHix(7) +
     &    (MHH2 - MHp2)*AllHix(11) + 4*(MA02 - Mh02)*AllHix(14)

	dup5 = S2B**4*AllHix(126) + MHH2*AllHix(130)

        AllHix(152) = (-2*dup5*AllHil(3))/
     &     (MHH2*(MA02 - MHp2)*S2B**4) + AllHix(151)/S2B**2

        AllHix(155) = (2*CBA*SBA*AllHix(129))/
     &     ((MA02 - MHH2)*S2B**2) +
     &    (MHH2 - MHp2)*SBA2*AllHix(153) -
     &    AllHix(154)/(MHH2*(MA02 - MHp2))

        AllHix(1637) =
     &   (MHp2*AllHil(4))/((Mh02 - MHp2)*(MHH2 - MHp2)) -
     &    AllHix(1634)/(MA02 - Mh02) + AllHix(1636)/(Mh02 - MHH2)

        AllHix(17) = CBA2*SBA2*
     &     (Mh02*(1 + (2*AllHil(2))/Mh02)**2 +
     &       MHH2*(1 + (2*AllHil(3))/MHH2)**2) +
     &    (1 + (2*AllHil(2))/Mh02)*(1 + (2*AllHil(3))/MHH2)*
     &     AllHix(16)

        AllHix(193) = (Mh02*SBA2*AllHil(32)*AllHix(182))/
     &     (Mh02 - MHp2)**2 +
     &    (2*SBA*(3*AllHil(36)*AllHix(176) -
     &         AllHil(53)*AllHix(183)))/S2B**2 +
     &    AllHix(192)/Mh02**2

        AllHix(205) = MA02**4*(Mh02 + MHH2) +
     &    (6*MA02**2 + Mh02*MHH2)*(Mh02**3 + MHH2**3) -
     &    2*(MA02**3*AllHix(194) + AllHix(204))

        AllHix(3) = -10*MA02**2 + 5*MHp2**2 + AllHil(1)**2 +
     &    3*AllHil(4)**2 + MHp2*(-11*MA02 - 2*AllHil(46)) +
     &    (AllHil(4)*AllHix(1))/MHp2 +
     &    MA02*(10*AllHil(46) - (AllHil(1)*AllHix(2))/MA02)

        AllHix(37) = -(CBA2*
     &       (AllHix(21) + AllHil(47)*AllHix(25))) +
     &    AllHil(46)*AllHix(31) -
     &    (SBA*AllHil(34)*AllHix(32) +
     &       (AllHil(48)*AllHix(36))/Mh02)/S2B**2

        AllHix(55) = -(SBA2*AllHix(46)) +
     &    AllHil(46)*AllHix(52) - (AllHil(39)*AllHix(54))/S2B**2

	AllHix(82) = 3*CBA2**2 + (8*AllHix(81))/((Mh02 - MHp2)*S2B**2)

        AllHix(911) = 8*SBA2*AllHil(3)**2*AllHix(896) -
     &    (8*AllHil(4)**2*AllHix(807) + AllHix(910))/(MHH2 - MHp2)

        AllHix(1446) =
     &   9*CBA*Mh02*S2A*S2B*SB2*SBA*AllHix(1433) +
     &    (2*MW2*SW2)/(Alfa*Pi)*((Mh02 - MHp2)**2*AllHix(369))

        AllHix(1461) =
     &   Mh02**3*(27*S2A - 4*S2B)*S2B*SBA +
     &    8*MHp2**2*AllHix(1260) - 2*Mh02**2*AllHix(1459) +
     &    4*Mh02*MHp2*AllHix(1460)

        AllHix(1470) =
     &   -9*CBA*Mh02*(Mh02 - 4*MHp2)*MHp2*S2A*S2B*SB2*SBA +
     &    (4*MW2*SW2)/(Alfa*Pi)*((Mh02 - MHp2)**2*AllHix(369))

        AllHix(1475) =
     &   8*Mh02**3*S2B**2*SBA - 16*MHp2**2*AllHix(1260) +
     &    Mh02**2*AllHix(1473) - 4*Mh02*MHp2*AllHix(1474)

        AllHix(1484) =
     &   -(S2B**2*AllHix(1483)) +
     &    (32*MW2**2*SW2**2)/(Alfa2*Pi**2)*dup1

        AllHix(571) = MA02**3*Mh02**2 - 2*MA02**2*Mh02**3 +
     &    MA02*Mh02**4 + (MA02 - Mh02)*MHH2**4 -
     &    MHH2**3*AllHix(320) - MHH2*AllHix(569) +
     &    MHH2**2*AllHix(570)

        AllHix(962) = -((126*MW2*SW2)/(Alfa*Pi)*
     &       (CBA*Mh02*S2A*S2B*(CB2*Lambda6 - Lambda7*SB2)*SBA)) +
     &    (MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     ((1 - 63*CBA2**2)*AllHix(919))

        AllHix(1043) =
     &   -2*(5*MA02 - MHH2)*(MHH2**2*MHp2 + MHp2**3) +
     &    (MA02 - MHp2)**2*SBA2*AllHix(1000) +
     &    2*(MA02**2*MHH2*(MHH2 - 2*MHp2) + MHp2**2*AllHix(1003))

        AllHix(1114) =
     &   -4*SAB*(m12sq + MHH2*S2B - 3*m12sq*SBA2) +
     &    CBA*S2B*AllHix(1107) -
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*(1 - 3*SBA2))

	dup6 = MHH2*S2B*AllHix(1105) + 2*AllHix(519)*AllHix(1106)

        AllHix(1153) =
     &   9*dup6*MHH2*AllHix(520) +
     &    MA02*(MA02 + 2*MHH2)*AllHix(1109)**2 -
     &    AllHix(65)*AllHix(1152)

        AllHix(1177) =
     &   (9*MHH2*AllHix(520))/(Mh02 - MHH2) -
     &    (MA02*(CBA*AllHix(1093) + 2*AllHix(1095)))/
     &     (MA02 - Mh02)**2

        AllHix(1404) =
     &   CBA*(MHH2 - MHp2)*AllHix(83) +
     &    2*(Mh02 - MHp2)*SBA*AllHix(183)

        AllHix(1447) =
     &   9*CB2*CBA*Lambda6*Mh02*S2A*S2B*SBA*AllHix(1433) -
     &    Lambda7*AllHix(1446) +
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     (CB2**2*Lambda6**2*(Mh02 - MHp2)**2)

        AllHix(1449) =
     &   -3*MHp2**2*S2B**4*SBA2 +
     &    (2*MW2**2*SW2**2)/(Alfa2*Pi**2)*dup1 -
     &    (5*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*MHp2*S2B**2*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHix(1456) =
     &   -(S2B**2*AllHix(1455)) +
     &    (4*MW2**2*SW2**2)/(Alfa2*Pi**2)*dup1

        AllHix(1458) =
     &   CBA2**2*Mh02*S2B*AllHix(965) +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*CBA2**3*m12sq*(CB2*Lambda6 - Lambda7*SB2))

	dup7 = Mh02*S2B**2*SBA - 2*AllHix(1260)

        AllHix(1462) =
     &   8*dup7*m12sq*(Mh02 - MHp2)**2 + Mh02*S2B*AllHix(1461)

        AllHix(1471) =
     &   -9*CB2*CBA*Lambda6*Mh02*(Mh02 - 4*MHp2)*MHp2*S2A*S2B*
     &     SBA - Lambda7*AllHix(1470) +
     &    (8*MW2*SW2)/(Alfa*Pi)*
     &     (CB2**2*Lambda6**2*(Mh02 - MHp2)**2)

        AllHix(1476) =
     &   -16*dup7*m12sq*(Mh02 - MHp2)**2 + Mh02*S2B*AllHix(1475)

        AllHix(1479) =
     &   -3*MHp2**2*SBA2*AllHix(1478) +
     &    (16*MW2**2*SW2**2)/(Alfa2*Pi**2)*dup1 -
     &    (32*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*MHp2*S2B**2*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHix(1480) =
     &   -6*MHp2**2*S2B**4*SBA2 +
     &    (13*MW2**2*SW2**2)/(Alfa2*Pi**2)*dup1 -
     &    (10*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*MHp2*S2B**2*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHix(1481) =
     &   MHp2**2*S2B**4*SBA2 -
     &    (3*MW2**2*SW2**2)/(Alfa2*Pi**2)*dup1 +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*MHp2*S2B**2*(CB2*Lambda6 - Lambda7*SB2)*SBA)

	AllHix(1485) = -2*Mh02**4*S2B**4*SBA2 + Mh02**2*AllHix(1484)

        AllHix(1510) =
     &   (9*MHH2*AllHix(520))/(Mh02 - MHH2) -
     &    (MHp2*(CBA*AllHix(1507) + 2*AllHix(1509)))/
     &     (Mh02 - MHp2)**2

        AllHix(1617) =
     &   3*MHp2*AllHix(83)**2 +
     &    2*AllHix(1548)**2*(MHH2 + 1/2.D0*MHp2)

        AllHix(525) = MHp2*S2B*(S2B - 4*CBA*SAB) +
     &    2*CBA*(1 - 3*SBA2)*AllHix(519)

	AllHix(576) = S2A*S2B*AllHix(571) - S2B**2*AllHix(575)

        AllHix(768) = 5*MHp2*S2B**2 -
     &    MHH2*(S2A*S2B + 6*S2B**2)*SBA2 + 2*SBA*AllHix(465)

	AllHix(857) = -Mh02**4 - Mh02*MHH2**3 + AllHix(847)

        AllHix(963) = Mh02**2*S2B**2*(63*S2A**2 + S2B**2)*
     &     SBA2 - 2*CBA2*AllHix(962)

        AllHix(984) = Mh02**2*S2B**4*SBA2 -
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*Mh02*S2B**2*(CB2*Lambda6 - Lambda7*SB2)*SBA) +
     &    (2*MW2**2*SW2**2)/(Alfa2*Pi**2)*(CBA2*AllHix(919))

        AllHix(986) = -3*Mh02**2*SBA2*AllHix(985) -
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*Mh02*(18*CBA2*S2A - S2B)*S2B*
     &       (CB2*Lambda6 - Lambda7*SB2)*SBA) +
     &    (2*MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (CBA2*(1 - 9*CBA2**2)*AllHix(919))

        AllHix(1030) =
     &   AllHix(83)**2*(4*AllHix(1015) + MHp2*AllHix(1021))

        AllHix(1034) =
     &   -4*MHp2*AllHix(153) +
     &    (MHH2*AllHix(1021))/((MA02 - MHH2)*(MA02 - MHp2))

        AllHix(1059) =
     &   2*(MA02 - Mh02)*SBA*AllHix(32) -
     &    CBA*(MA02 - MHH2)*AllHix(65)

        AllHix(1070) =
     &   (MA02*SBA*AllHix(32))/((4*MA02 - Mh02)*(MA02 - MHH2)) +
     &    (CBA*Mh02*AllHix(65))/((MA02 - Mh02)*(Mh02 - MHH2))

        AllHix(1072) =
     &   CBA*Mh02*AllHix(65) +
     &    CBA2*(2*MA02 - Mh02)*(S2B*AllHix(58) - 6*AllHix(564))

	AllHix(1078) = MHH2*AllHix(32)**2 + MA02*SBA2*AllHix(67)**2

        AllHix(1079) =
     &   (MA02 - Mh02)*(MA02 - Mh02 + MHH2)*SBA*AllHix(32) +
     &    2*CBA2*MA02*(MA02 - MHH2)*
     &     (S2B*AllHix(58) - 6*AllHix(564))

        AllHix(1081) =
     &   CBA*(MA02 - Mh02 - MHH2)*AllHix(65) -
     &    CBA2*(MA02 - Mh02 + MHH2)*
     &     (S2B*AllHix(58) - 6*AllHix(564))

        AllHix(1097) =
     &   (MA02*AllHix(67))/(MA02 - Mh02)**2 +
     &    (4*MHH2*AllHix(213))/((MA02 - MHH2)*(Mh02 - MHH2))

        AllHix(1099) =
     &   (-6*CBA*Mh02*AllHix(520))/(Mh02 - MHH2) +
     &    (AllHix(67)*AllHix(1098))/((Mh02 - 4*MHH2)*(MA02 - MHH2))

        AllHix(1112) =
     &   -2*(MA02 - MHH2)*AllHix(53) + CBA*S2B**2*AllHix(1111)

        AllHix(1113) =
     &   -(MHH2*AllHix(65)**2) + 9*(4*MA02 - MHH2)*AllHix(520)**2

        AllHix(1115) =
     &   9*dup6*AllHix(520) + 2*MA02*AllHix(1109)**2 -
     &    (4*MA02 - 3*MHH2)*AllHix(65)*AllHix(1114)

        AllHix(1128) =
     &   -16*(MA02 - MHH2)*AllHix(65)**2 +
     &    (4*MA02 - MHH2)*AllHix(1109)**2

        AllHix(1142) =
     &   -2*(2*MA02*MHH2 + MHH2**2)*AllHix(65)**2 +
     &    27*(4*MA02 - MHH2)*MHH2*AllHix(520)**2

        AllHix(1154) =
     &   9*dup6*MHH2*AllHix(520) +
     &    (MA02**2 + 2*MA02*MHH2)*AllHix(1109)**2 -
     &    AllHix(65)*AllHix(1148) + 1/2.D0*AllHix(1153)

        AllHix(1165) =
     &   (MA02 - Mh02)*SBA2*AllHix(67) -
     &    CBA2*(MA02 - MHH2)*AllHix(213)

        AllHix(1178) =
     &   (-4*AllHix(67)**2*AllHix(1098))/
     &     ((MA02 - Mh02)*(Mh02 - 4*MHH2)) +
     &    CBA*AllHix(67)*AllHix(1177)

        AllHix(1189) =
     &   (9*SBA*AllHix(176)*(S2B*AllHix(58) - 6*AllHix(564)))/
     &     (Mh02 - MHH2) +
     &    (2*(S2B*AllHix(58) - 6*AllHix(564))**2*AllHix(1050))/
     &     ((MA02 - MHH2)*(4*Mh02 - MHH2))

        AllHix(1196) =
     &   AllHix(67)**2*(2*MHH2**2*AllHix(1050) +
     &      (3*Mh02 + 6*MHH2)*AllHix(1098))

        AllHix(1263) =
     &   -9*Mh02**4*S2B**2*SBA + 2*Mh02**2*MHp2*AllHix(1259) +
     &    128*MHp2**3*AllHix(1260) -
     &    88*Mh02*MHp2**2*AllHix(1261) + Mh02**3*AllHix(1262)

        AllHix(1266) =
     &   -35*Mh02**4*S2B**2*SBA + 512*MHp2**3*AllHix(1260) -
     &    376*Mh02*MHp2**2*AllHix(1261) -
     &    2*Mh02**2*MHp2*AllHix(1264) + Mh02**3*AllHix(1265)

        AllHix(1273) =
     &   -33*MHp2**2*S2B**4*SBA2 +
     &    (12*MW2**2*SW2**2)/(Alfa2*Pi**2)*dup1 -
     &    (29*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*MHp2*S2B**2*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHix(1274) =
     &   23*MHp2**2*S2B**4*SBA2 +
     &    (6*MW2**2*SW2**2)/(Alfa2*Pi**2)*dup1 -
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*MHp2*S2B**2*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHix(1280) =
     &   -105*MHp2**2*S2B**4*SBA2 +
     &    (12*MW2**2*SW2**2)/(Alfa2*Pi**2)*dup1 -
     &    (103*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*MHp2*S2B**2*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHix(1281) =
     &   119*MHp2**2*S2B**4*SBA2 +
     &    (6*MW2**2*SW2**2)/(Alfa2*Pi**2)*dup1 +
     &    (8*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*MHp2*S2B**2*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHix(1300) =
     &   -2*AllHix(1222) +
     &    ((MA02 - MHp2)*AllHix(183)**2*AllHix(1296))/
     &     ((MA02 - Mh02)*(Mh02 - 4*MHp2))

	AllHix(1301) = MHp2*AllHix(32)**2 + MA02*AllHix(183)**2

        AllHix(1307) =
     &   AllHix(32)**2*(MA02*AllHix(1224) + 2*AllHix(1306))

        AllHix(1309) =
     &   4*(MA02 - Mh02 + MHp2)*AllHix(1222) - AllHix(1301)

        AllHix(1313) =
     &   (CBA2*MHp2)/(Mh02 - MHp2)**2 -
     &    (2*Mh02*AllHix(1224))/
     &     ((4*MA02 - Mh02)*(MA02 - MHp2)*(Mh02 - MHp2))

        AllHix(1314) =
     &   4*MA02*AllHix(1057)**2*AllHix(1224) -
     &    AllHix(32)**2*AllHix(1306)

        AllHix(1315) =
     &   (CBA2*AllHix(183)**2)/(MA02 - Mh02)**2 +
     &    (4*AllHix(1225))/(MA02 - MHp2)**2

        AllHix(1344) =
     &   3*CBA*(Mh02 - 4*MHp2)*S2B*SB2*SBA*AllHix(1342) +
     &    (8*MW2*SW2)/(Alfa*Pi)*(AllHix(369)*AllHix(1343))

        AllHix(1353) =
     &   -5*Mh02**3*(15*S2A - 2*S2B)*S2B*SBA +
     &    112*MHp2**2*AllHix(1260) + 4*Mh02*MHp2*AllHix(1351) +
     &    Mh02**2*AllHix(1352)

        AllHix(1355) =
     &   Mh02**2*S2B*(3*S2A + 8*S2B)*SBA - 32*MHp2*AllHix(1260) -
     &    4*Mh02*AllHix(1354)

        AllHix(1362) =
     &   CBA*(2*Mh02 - MHH2)*(MHH2 - MHp2)*AllHix(83) +
     &    2*MHH2*(Mh02 - MHp2)*SBA*AllHix(183)

        AllHix(1368) =
     &   2*(Mh02 - MHp2)*SBA2*AllHix(67) -
     &    CBA2*(MHH2 - MHp2)*AllHix(213)

        AllHix(1378) =
     &   -(CBA*(MHH2 - MHp2)*(Mh02 - MHH2 + MHp2)*AllHix(83)) +
     &    (Mh02 - MHp2)*(Mh02 - MHH2 - MHp2)*SBA*AllHix(183)

        AllHix(1379) =
     &   (Mh02 - MHH2 + MHp2)*SBA2*AllHix(67) -
     &    (Mh02 + MHH2 - MHp2)*SBA*AllHix(183)

	AllHix(1381) = MHp2*SBA2*AllHix(67)**2 + MHH2*AllHix(183)**2

        AllHix(1391) =
     &   AllHix(1389)/
     &     ((Mh02 - 4*MHH2)*(Mh02 - MHp2)*(MHH2 - MHp2)) -
     &    (MHp2*AllHix(1390))/((Mh02 - MHp2)**2*(MHH2 - MHp2)**2)

        AllHix(1392) =
     &   (-2*CBA2*MHH2*AllHix(213))/(MHH2 - MHp2) +
     &    (3*CBA*Mh02*AllHix(520))/(Mh02 - MHp2)

        AllHix(1398) =
     &   CBA*Mh02*AllHix(83) +
     &    CBA2*(Mh02 - 2*MHp2)*(S2B*AllHix(58) - 6*AllHix(564))

        AllHix(1407) =
     &   CBA2*(S2B*AllHix(58) - 6*AllHix(564))*AllHix(1404) +
     &    2*CBA*(Mh02 - MHp2)*AllHix(83)*
     &     (2*SBA*AllHix(1060) + SBA2*AllHix(1406))

        AllHix(1448) =
     &   CAB**2*AllHix(1445) +
     &    (MW2*SW2)/(Alfa*Pi)*(CBA2**2*AllHix(1447))

        AllHix(1457) =
     &   -16*(MHp2**2*AllHix(1330) + Mh02*MHp2*AllHix(1449)) -
     &    S2B**2*AllHix(1453) + 4*Mh02**2*AllHix(1456)

        AllHix(1463) =
     &   -8*dup7*(Mh02 - MHp2)**2*(2*m12sq - Mh02*S2B) +
     &    18*AllHix(1433)*AllHix(1458) - CBA2*AllHix(1462)

        AllHix(1472) =
     &   CAB**2*AllHix(1469) -
     &    (MW2*SW2)/(Alfa*Pi)*(CBA2**2*MHp2*AllHix(1471))

        AllHix(1477) =
     &   8*dup7*(Mh02 - 6*MHp2)*(Mh02 - MHp2)**2*
     &     (2*m12sq - Mh02*S2B) -
     &    18*(Mh02 - 4*MHp2)*MHp2**2*AllHix(1458) +
     &    CBA2*MHp2*AllHix(1476)

        AllHix(1486) =
     &   Mh02**3*AllHix(1479) + 16*Mh02*MHp2**2*AllHix(1480) +
     &    32*MHp2**3*AllHix(1481) - 4*MHp2*AllHix(1485)

        AllHix(1491) =
     &   (Mh02 - MHp2)*SBA2*AllHix(67) -
     &    CBA2*(MHH2 - MHp2)*AllHix(213)

        AllHix(1511) =
     &   (-4*AllHix(67)**2*AllHix(1389))/
     &     ((Mh02 - 4*MHH2)*(Mh02 - MHp2)) +
     &    CBA*AllHix(67)*AllHix(1510)

        AllHix(1515) =
     &   AllHix(67)**2*(2*MHH2**2*AllHix(1386) +
     &      (3*Mh02 + 6*MHH2)*AllHix(1389))

	AllHix(1518) = -(AllHix(83)**2*(MHH2*MHp2 - AllHix(1517)))

        AllHix(1519) =
     &   Mh02**2*AllHix(1386) + AllHix(1495)*(3*Mh02 + 3/2.D0*MHH2)

        AllHix(1525) =
     &   (18*MHH2**2*AllHix(520))/(Mh02 - MHH2) -
     &    ((2*MHH2*MHp2 + MHp2**2)*
     &       (CBA*AllHix(1507) + 2*AllHix(1509)))/(Mh02 - MHp2)**2

        AllHix(1539) =
     &   (8*CBA2*AllHix(83)**2)/(MHH2 - 4*MHp2) -
     &    (3*CBA*SBA*AllHix(83)*AllHix(183))/(MHH2 - MHp2)

        AllHix(1540) =
     &   (4*MHH2*AllHix(83)**2)/(MHH2 - 4*MHp2) -
     &    (MHp2*SBA*(S2B*AllHix(58) - 6*AllHix(564))*AllHix(1376))/
     &     (MHH2 - MHp2)**2

	AllHix(1588) = CBA*(MHH2 - 2*MHp2)*S2B**2 + 2*AllHix(53)

        AllHix(1595) =
     &   MHp2**2*SBA2*(3*AllHix(83)**2 + AllHix(1548)**2) +
     &    2*AllHix(83)*(MHH2*S2B*AllHix(1589) - 2*AllHix(1594))

        AllHix(1604) =
     &   -16*(MHH2*(MHH2 - MHp2)**2*AllHix(53) +
     &       MHp2*SAB*AllHix(1597)) -
     &    (MHH2 - 4*MHp2)*AllHix(1602) +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*AllHix(1603))

        AllHix(1606) =
     &   2*CBA*(MHH2 - 2*MHp2)*(MHH2 - MHp2)*S2B**2*AllHix(83) +
     &    AllHix(83)**2*AllHix(1605)

        AllHix(1608) =
     &   -3*(MHH2 - 4*MHp2)*SBA2*AllHix(83)**2 +
     &    16*MHH2*AllHix(83)*AllHix(1607)

        AllHix(1625) =
     &   (9*Mh02*AllHix(176))/(Mh02 - MHH2) +
     &    (MHp2*AllHix(1376))/(MHH2 - MHp2)**2

        AllHix(1716) =
     &   CAB*(2*MA02 - Mh02)*S2B**2*(2*m12sq - Mh02*S2B)*SBA +
     &    (2*m12sq - MHH2*S2B)*SAB*AllHix(1715) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*AllHix(1692))

        AllHix(1725) =
     &   AllHix(1724)**2 +
     &    (12*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA*
     &       (6*m12sq*S2A - S2B*AllHix(58))) +
     &    (3*MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     ((1 + 6*CBA2*SBA2)*AllHix(919))

        AllHix(1736) =
     &   -3*Mh02**2*SBA2 - 2*CBA2*AllHix(1718) + 4*AllHix(1728) +
     &    8*(MA02 - MHp2)**2*Re(AllHil(10))

        AllHix(1741) =
     &   AllHix(1728) + 2*(MA02 - MHp2)**2*Re(AllHil(10))

        AllHix(1755) =
     &   AllHix(1673)**2 +
     &    (3*MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     ((1 + 6*CBA2*SBA2)*AllHix(919)) -
     &    (12*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA*AllHix(1673))

        AllHix(1765) =
     &   4*CAB**2*(2*m12sq - Mh02*S2B)**2 - 4*AllHix(1716) +
     &    S2B**4*(AllHix(1763) + AllHix(1764)) +
     &    (2*MW2**2*SW2**2)/(Alfa2*Pi**2)*AllHix(919)

        AllHix(309) = 2*(MA02 - MHH2)*MHH2*SAB +
     &    CBA*(2*MA02 - Mh02 - MHH2)*AllHix(308)

        AllHix(423) = CBA*dup4*Mh02 +
     &    (MA02 - MHp2)*(Mh02 - MHp2)*S2B**2*SBA2

	AllHix(455) = MA02*S2A*S2B*SBA2 - 6*CBA*SBA*AllHix(80)

	AllHix(468) = -5*MHH2*S2B**2*SBA2 + 4*SBA*AllHix(171)

        AllHix(526) = -2*CBA*MHp2*(1 - 3*SBA2)*AllHix(519) +
     &    MHH2**2*S2B*AllHix(524) + MHH2*AllHix(525)

        AllHix(580) = -(SBA2*AllHix(576)) +
     &    SBA*(-2*(MA02 - Mh02)*(MA02 - MHH2)*(Mh02 - MHH2)*
     &        (2*C2B*CBA*m12sq*Mh02 - CAB*AllHix(319)) +
     &       (2*MW2*SW2)/(Alfa*Pi)*
     &        (CBA*(CB2*Lambda6 - Lambda7*SB2)*AllHix(579)))

        AllHix(708) = 2*CBA*(1 - 3*SBA2)*AllHix(519) +
     &    MHH2*S2B*AllHix(707)

	AllHix(753) = 2*Mh02*S2B**2 - 3*SBA*AllHix(171)

        AllHix(760) = 8*CBA*SBA*AllHix(80) +
     &    SBA2*(MA02*S2B**2 + S2B*AllHix(744))

        AllHix(769) = MHp2**3*(MHH2 + MHp2)*S2B**2*SBA2 +
     &    Mh02**2*MHp2*AllHix(767) + Mh02*MHp2**2*AllHix(768)

        AllHix(795) = (Mh02 - MHH2)**2*(Mh02 + MHH2 - 2*MHp2)*
     &     S2B*AllHix(197) + CB2*AllHix(794)

        AllHix(841) = MHH2**2*(Mh02 + MHp2) -
     &    ((Mh02 - MHp2)*AllHix(266))/S2B**2

        AllHix(859) = Mh02**5*MHH2 - Mh02**4*MHH2**2 -
     &    Mh02**3*MHH2**3 + Mh02**2*MHH2**4 - MHp2*AllHix(856) -
     &    2*MHp2**2*AllHix(857) - MHp2**3*AllHix(858)

        AllHix(882) = 2*MHH2*(MHH2 - MHp2)*SAB +
     &    CBA*(Mh02 + MHH2 - 2*MHp2)*AllHix(716)

        AllHix(932) = 3*Mh02*S2B*SBA*AllHix(918) +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(17 + 60*CBA2 - 297*CBA2**2)*
     &       (CB2*Lambda6 - Lambda7*SB2))

        AllHix(934) = Mh02*S2B*SBA*AllHix(922) +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(7 - 12*CBA2 + 81*CBA2**2)*
     &       (CB2*Lambda6 - Lambda7*SB2))

        AllHix(960) = Mh02**2*S2B**2*SBA2*AllHix(959) +
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*Mh02*S2B*(18*CBA2*S2A + S2B)*
     &       (CB2*Lambda6 - Lambda7*SB2)*SBA) +
     &    (36*MW2**2*SW2**2)/(Alfa2*Pi**2)*(CBA2**3*AllHix(919))

        AllHix(961) = Mh02**2*S2B**2*(27*S2A**2 + S2B**2)*
     &     SBA2 + (4*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*Mh02*(27*CBA2*S2A - S2B)*S2B*
     &       (CB2*Lambda6 - Lambda7*SB2)*SBA) +
     &    (2*MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (CBA2*(1 + 27*CBA2**2)*AllHix(919))

	AllHix(964) = -4*MA02**3*S2B**4*SBA2 + MA02*AllHix(963)

        AllHix(982) = Mh02**2*S2B**2*SBA2*AllHix(981) +
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*Mh02*(9*CBA2*S2A - 11*S2B)*S2B*
     &       (CB2*Lambda6 - Lambda7*SB2)*SBA) +
     &    (6*MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (CBA2*(1 + 3*CBA2**2)*AllHix(919))

        AllHix(987) = MA02**3*S2B**2*AllHix(983) +
     &    Mh02**2*AllHix(984) - MA02**2*AllHix(986)

        AllHix(1009) =
     &   ((MA02 - MHp2)*AllHix(65)*AllHix(127))/
     &     ((4*MA02 - MHH2)*(MHH2 - MHp2)) +
     &    (MW2*SW2)/(Alfa*Pi)*((CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHix(1019) =
     &   (MA02**2 - MA02*MHH2)*AllHix(127) +
     &    AllHix(1018)*(3*MA02 + 3/2.D0*MHH2)

        AllHix(1022) =
     &   -(MHp2**2*AllHix(153)) +
     &    (MHH2*MHp2*AllHix(1021))/((MA02 - MHH2)*(MA02 - MHp2))

        AllHix(1031) =
     &   (AllHix(65)**2*(MA02*AllHix(127) - 2*AllHix(1018)))/
     &     ((4*MA02 - MHH2)*(MHH2 - MHp2)) +
     &    AllHix(1030)/((MA02 - MHH2)*(MHH2 - 4*MHp2))

        AllHix(1035) =
     &   (-2*MA02*SBA2)/(MA02 - MHH2)**2 +
     &    AllHix(1034)/(MHH2 - 4*MHp2)

	AllHix(1041) = (4*MA02 - MHH2)*AllHix(127) + 4*AllHix(1018)

        AllHix(1042) =
     &   (SBA2*AllHix(65)**2)/(MHH2 - MHp2)**2 -
     &    (AllHix(65)*AllHix(83))/(MA02 - MHp2)**2

        AllHix(1054) =
     &   2*(MA02 - Mh02)*SBA2*AllHix(67) -
     &    CBA2*(MA02 - MHH2)*AllHix(213)

        AllHix(1061) =
     &   CBA2*(S2B*AllHix(58) - 6*AllHix(564))*AllHix(1059) -
     &    2*CBA*(MA02 - Mh02)*AllHix(65)*
     &     (SBA2*AllHix(313) + 2*SBA*AllHix(1060))

        AllHix(1071) =
     &   (MHH2*SBA*AllHix(1057)**2)/(MA02 - MHH2)**2 -
     &    1/4.D0*(AllHix(32)*AllHix(1070))

        AllHix(1073) =
     &   (MA02 - Mh02)*(2*MA02 - Mh02)*SBA2*AllHix(1049) -
     &    2*(MA02 - MHH2)*AllHix(1072)

        AllHix(1077) =
     &   Mh02*AllHix(65)**2 -
     &    CBA*(MA02 + Mh02 - MHH2)*AllHix(65)*
     &     (S2B*AllHix(58) - 6*AllHix(564)) +
     &    CBA2*MA02*(S2B*AllHix(58) - 6*AllHix(564))**2

        AllHix(1080) =
     &   (MA02 - Mh02)**2*AllHix(1078) +
     &    (MA02 - Mh02)*AllHix(67)*AllHix(1079)

        AllHix(1082) =
     &   CBA*(MA02 + Mh02 - MHH2)*SBA2*AllHix(65)*AllHix(67) +
     &    SBA*AllHix(32)*AllHix(1081)

        AllHix(1085) =
     &   2*(MA02 - Mh02)*MHH2*SBA*AllHix(32) -
     &    CBA*(MA02 - MHH2)*(2*Mh02 - MHH2)*AllHix(65)

        AllHix(1086) =
     &   (2*Mh02*SBA2*AllHix(67))/(MA02 - Mh02) +
     &    (3*MHH2*SBA*AllHix(176))/(MA02 - MHH2)

        AllHix(1096) =
     &   (MA02 - Mh02)*(Mh02 - 2*MHH2)*SBA*AllHix(32) +
     &    2*CBA*Mh02*(MA02 - MHH2)*AllHix(65)

        AllHix(1100) =
     &   (-2*MA02*SBA2*AllHix(1049))/(MA02 - MHH2)**2 +
     &    CBA2*AllHix(1097) + AllHix(1099)/(MA02 - Mh02)

        AllHix(1116) =
     &   8*MA02*AllHix(65)*AllHix(1112) +
     &    4*(MA02 - MHH2)*AllHix(1113) +
     &    (4*MA02 - MHH2)*AllHix(1115)

        AllHix(1129) =
     &   2*(4*MA02 - MHH2)*AllHix(65)*AllHix(1127) +
     &    MA02*AllHix(1128)

        AllHix(1155) =
     &   2*(MA02 - MHH2)*AllHix(1142) +
     &    (4*MA02 - MHH2)*AllHix(1154)

        AllHix(1166) =
     &   (MA02 - Mh02)*SBA*AllHix(32)*AllHix(1164) -
     &    AllHix(65)*AllHix(1165)

        AllHix(1170) =
     &   (9*SBA*AllHix(176)*(S2B*AllHix(58) - 6*AllHix(564)))/
     &     (Mh02 - MHH2) +
     &    (4*(S2B*AllHix(58) - 6*AllHix(564))**2*AllHix(1050))/
     &     ((MA02 - MHH2)*(4*Mh02 - MHH2))

        AllHix(1180) =
     &   -(CBA*(MA02 - MHH2)*AllHix(65)*AllHix(1159)) +
     &    AllHix(32)*AllHix(1165)

        AllHix(1190) =
     &   ((MA02**2 + 2*MA02*Mh02)*SBA*
     &       (S2B*AllHix(58) - 6*AllHix(564))*AllHix(1068))/
     &     (MA02 - MHH2)**2 + 4*AllHix(65)**2*AllHix(1188) -
     &    2*Mh02**2*AllHix(1189)

        AllHix(1197) =
     &   (9*CBA*MHH2**2*AllHix(67)*AllHix(520))/(Mh02 - MHH2) +
     &    (2*AllHix(32)**2*(4*MA02**2 + MA02*AllHix(1195)))/
     &     (4*MA02 - Mh02) -
     &    AllHix(1196)/((MA02 - Mh02)*(Mh02 - 4*MHH2))

        AllHix(1206) =
     &   (MA02*SBA*(S2B*AllHix(58) - 6*AllHix(564))*AllHix(1068))/
     &     (MA02 - MHH2)**2 + 4*AllHix(65)**2*AllHix(1205)

        AllHix(1271) =
     &   Mh02*AllHix(1263) - MA02*AllHix(1266) +
     &    2*(Mh02 - 4*MHp2)*AllHix(1270)

        AllHix(1279) =
     &   -9*Mh02**5*S2B**4*SBA2 + 4*Mh02**3*AllHix(1273) -
     &    8*Mh02**2*MHp2*AllHix(1274) - S2B**2*AllHix(1278)

        AllHix(1285) =
     &   -17*Mh02**5*S2B**4*SBA2 + 4*Mh02**3*AllHix(1280) -
     &    8*Mh02**2*MHp2*AllHix(1281) - S2B**2*AllHix(1284)

        AllHix(1298) =
     &   ((Mh02 - 4*MHp2)*AllHix(1296))/
     &     ((MA02 - Mh02)*(MA02 - MHp2)) - 8*AllHix(1297)

        AllHix(1299) =
     &   (MHp2*AllHix(32)**2)/(Mh02 - MHp2)**2 -
     &    (4*MA02*AllHix(183)**2)/(MA02 - Mh02)**2

        AllHix(1302) =
     &   4*(Mh02 - MHp2)*AllHix(1222) + 2*MHp2*AllHix(1300) +
     &    AllHix(1301)

        AllHix(1308) =
     &   (MHp2*AllHix(183)**2*AllHix(1296))/
     &     ((MA02 - Mh02)*(Mh02 - 4*MHp2)) -
     &    AllHix(1307)/((4*MA02 - Mh02)*(Mh02 - MHp2))

        AllHix(1310) =
     &   2*(Mh02 - 2*MHp2)*AllHix(1222) - 2*AllHix(1229) +
     &    AllHix(1309)

        AllHix(1345) =
     &   3*CB2*CBA*Lambda6*(Mh02 - 4*MHp2)*S2B*SBA*AllHix(1342) -
     &    Lambda7*AllHix(1344) +
     &    (16*MW2*SW2)/(Alfa*Pi)*(CB2**2*Lambda6**2*AllHix(1343))

        AllHix(1356) =
     &   Mh02*S2B*AllHix(1353) - 4*m12sq*(Mh02 - MHp2)*AllHix(1355)

        AllHix(1359) =
     &   (2*Mh02*SBA2*AllHix(67))/(Mh02 - MHp2) +
     &    (3*MHH2*SBA*AllHix(176))/(MHH2 - MHp2)

        AllHix(1363) =
     &   -(((2*Mh02 - MHH2)*SBA2*(S2B*AllHix(58) - 6*AllHix(564)))/
     &       (4*Mh02 - MHH2)) + AllHix(1362)/(Mh02 - MHp2)**2

        AllHix(1367) =
     &   (2*MHH2*SBA*AllHix(183))/((Mh02 - MHH2)*(MHH2 - MHp2)) +
     &    (CBA*AllHix(83)*AllHix(1366))/(Mh02 - MHp2)**2

        AllHix(1369) =
     &   2*MHH2*(Mh02 - MHp2)*SBA*AllHix(183) +
     &    (MHH2 - 2*MHp2)*AllHix(1368)

        AllHix(1388) =
     &   -2*CBA*Mh02*(MHH2 - MHp2)*AllHix(83) +
     &    (Mh02 - 2*MHH2)*(Mh02 - MHp2)*SBA*AllHix(183)

        AllHix(1393) =
     &   AllHix(67)*AllHix(1391) - (2*AllHix(1392))/(Mh02 - MHH2)

        AllHix(1399) =
     &   (Mh02 - 2*MHp2)*(Mh02 - MHp2)*SBA2*AllHix(1049) -
     &    2*(MHH2 - MHp2)*AllHix(1398)

        AllHix(1403) =
     &   CBA2*(Mh02 - 4*MHp2)*(MHH2 - MHp2)*AllHix(83)**2 +
     &    (MHH2 - 4*MHp2)*(Mh02 - MHp2)*SBA2*AllHix(183)**2

        AllHix(1408) =
     &   (Mh02 - MHp2)**2*SBA*SBA2*AllHix(67)*AllHix(183) +
     &    (MHH2 - MHp2)*AllHix(1407)

        AllHix(1464) =
     &   8*(Mh02 - 2*MHp2)*(Mh02 - MHp2)**2*S2B**2*AllHix(1326) +
     &    4*AllHix(1448) - CBA2*AllHix(1457) -
     &    4*((AllHil(4)*AllHix(183)*AllHix(1441))/MHp2 +
     &       CAB*AllHix(1463)) +
     &    (18*MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (CBA2**4*AllHix(919)*AllHix(1433))

        AllHix(1487) =
     &   8*(Mh02 - MHp2)**2*S2B**2*AllHix(1326)*AllHix(1465) +
     &    4*(AllHix(1472) + CAB*AllHix(1477)) -
     &    CBA2*AllHix(1486) +
     &    (18*MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (CBA2**4*(Mh02 - 4*MHp2)*MHp2**2*AllHix(919))

        AllHix(1492) =
     &   CBA*(MHH2 - MHp2)*AllHix(83)*AllHix(1490) +
     &    AllHix(183)*AllHix(1491)

        AllHix(1494) =
     &   (CBA2*MHp2*AllHix(83)**2)/(MHH2 - 4*MHp2) +
     &    (MHH2*SBA2*AllHix(67)**2*AllHix(1386))/
     &     ((Mh02 - 4*MHH2)*(MHH2 - MHp2))

        AllHix(1496) =
     &   (MHp2*AllHix(83)**2)/(MHH2 - 4*MHp2) -
     &    ((S2B*AllHix(58) - 6*AllHix(564))**2*AllHix(1495))/
     &     ((4*Mh02 - MHH2)*(MHH2 - MHp2))

        AllHix(1503) =
     &   (SBA2**2*AllHix(67)**2)/(Mh02 - 4*MHH2) +
     &    (CBA*SBA*(SBA*AllHix(67)*AllHix(83) +
     &         AllHix(183)*AllHix(1502)))/(Mh02 - MHp2)

        AllHix(1516) =
     &   3*CBA*MHp2**2*SBA*AllHix(83)*AllHix(183) -
     &    (SBA2*AllHix(1515))/(Mh02 - 4*MHH2)

        AllHix(1520) =
     &   (9*Mh02**2*SBA*AllHix(176)*
     &       (S2B*AllHix(58) - 6*AllHix(564)))/(Mh02 - MHH2) +
     &    2*(AllHix(1518)/(MHH2 - 4*MHp2) +
     &       ((S2B*AllHix(58) - 6*AllHix(564))**2*AllHix(1519))/
     &        ((4*Mh02 - MHH2)*(MHH2 - MHp2)))

        AllHix(1522) =
     &   (9*Mh02**2*AllHix(176))/(Mh02 - MHH2) +
     &    (MHp2*AllHix(1376)*(Mh02 + 1/2.D0*MHp2))/(MHH2 - MHp2)**2

        AllHix(1526) =
     &   (-4*AllHix(183)**2*AllHix(1524))/(Mh02 - 4*MHp2) +
     &    CBA*AllHix(67)*AllHix(1525)

        AllHix(1528) =
     &   (9*MHH2**2*AllHix(520))/(Mh02 - MHH2) -
     &    (MHp2*(CBA*AllHix(1507) + 2*AllHix(1509))*
     &       (MHH2 + 1/2.D0*MHp2))/(Mh02 - MHp2)**2

        AllHix(1531) =
     &   AllHix(83)*AllHix(1491) +
     &    (Mh02 - MHp2)*SBA*AllHix(183)*AllHix(1502)

        AllHix(1551) =
     &   2*(MHH2 + (MHH2 - 4*MHp2)*SBA2)*AllHix(519) +
     &    MHH2*S2B*AllHix(1550)

        AllHix(1596) =
     &   8*MHp2*AllHix(83)**2*AllHix(1587) -
     &    12*MHH2*(MHH2 - MHp2)*(MHH2 - MHp2*(1 - SBA2))*
     &     AllHix(83)*AllHix(1588) - (MHH2 - 4*MHp2)*AllHix(1595)

        AllHix(1626) =
     &   (4*(S2B*AllHix(58) - 6*AllHix(564))**2*AllHix(1624))/
     &     ((4*Mh02 - MHH2)*(MHH2 - MHp2)) +
     &    SBA*(S2B*AllHix(58) - 6*AllHix(564))*AllHix(1625)

        AllHix(1648) =
     &   -2*S2B**4*AllHix(1645) + 8*m12sq**2*AllHix(1646) -
     &    S2B**2*AllHix(1647) +
     &    (24*MW2**2*SW2**2)/(Alfa2*Pi**2)*AllHix(919)

        AllHix(1653) =
     &   6*CBA2*m12sq*(6*m12sq*S2A - S2B*AllHix(58)) +
     &    S2B*AllHix(1652)

        AllHix(1676) =
     &   CBA*(6*m12sq*S2A - S2B*AllHix(58)) +
     &    (6*MW2*SW2)/(Alfa*Pi)*
     &     (CBA2*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHix(1689) =
     &   CAB*(2*m12sq - Mh02*S2B)*AllHix(1681) +
     &    (2*m12sq - MHH2*S2B)*SAB*(2*m12sq*SAB + S2B*AllHix(1684))

        AllHix(1700) =
     &   4*CAB**2*(2*m12sq - Mh02*S2B)**2 +
     &    (2*MA02 - Mh02)**2*S2B**4*SBA2 - 4*AllHix(1699) +
     &    (2*MW2**2*SW2**2)/(Alfa2*Pi**2)*(CBA2*AllHix(919))

        AllHix(1704) =
     &   CBA2*(2*MA02 - MHH2)**2*S2B**4 + 4*AllHix(1703) +
     &    (2*MW2**2*SW2**2)/(Alfa2*Pi**2)*(SBA2*AllHix(919))

        AllHix(1717) =
     &   4*CAB**2*(2*m12sq - Mh02*S2B)**2 + S2B**4*AllHix(1714) -
     &    4*AllHix(1716) +
     &    (2*MW2**2*SW2**2)/(Alfa2*Pi**2)*AllHix(919)

        AllHix(1720) =
     &   (2*MA02 - Mh02)**2*S2B**4 + AllHix(1673)**2 +
     &    (18*MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (CBA2*SBA2*AllHix(919)) -
     &    (12*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA*AllHix(1673))

        AllHix(1726) =
     &   -((Mh02 - 2*MHp2)**2*S2B**4*SBA2) -
     &    4*CAB*(2*m12sq - Mh02*S2B)*AllHix(1681) -
     &    CBA2*AllHix(1725) +
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*AllHix(1682))

        AllHix(1732) =
     &   AllHix(1673)**2 +
     &    (18*MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (CBA2*SBA2*AllHix(919)) -
     &    (12*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA*AllHix(1673))

        AllHix(1733) =
     &   AllHix(1724)**2 +
     &    (12*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA*
     &       (6*m12sq*S2A - S2B*AllHix(58))) +
     &    (18*MW2**2*SW2**2)/(Alfa2*Pi**2)*(CBA2*SBA2*AllHix(919))

        AllHix(1742) =
     &   S2B**4*AllHix(1741) + AllHix(1700)*Re(AllHil(13)) +
     &    AllHix(1704)*Re(AllHil(14))

        AllHix(1751) =
     &   (2*MA02 - MHH2)**2*S2B**4 + AllHix(1724)**2 +
     &    (12*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA*
     &       (6*m12sq*S2A - S2B*AllHix(58))) +
     &    (18*MW2**2*SW2**2)/(Alfa2*Pi**2)*(CBA2*SBA2*AllHix(919))

        AllHix(1754) =
     &   CBA2*MHH2**2*S2A**2*S2B**2 -
     &    4*SAB*(MHH2*S2B - 2*m12sq*SBA2)*AllHix(1752) +
     &    (2*MW2**2*SW2**2)/(Alfa2*Pi**2)*(SBA2**3*AllHix(919)) +
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*SBA2*AllHix(1753))

        AllHix(1756) =
     &   CBA2*(MHH2 - 2*MHp2)**2*S2B**4 + 4*AllHix(1686) +
     &    SBA2*AllHix(1755)

        dup8 = AllHix(1700)*Re(AllHil(13)) +
     &    AllHix(1704)*Re(AllHil(14))

        AllHix(1766) =
     &   -4*dup8 + 4*AllHix(1703) -
     &    S2B**4*(4*AllHix(1741) - AllHix(1750)) +
     &    CBA2*AllHix(1751) + 9*AllHix(1754) + 2*AllHix(1756) -
     &    2*AllHix(1765)

        AllHix(1800) =
     &   4*AllHix(1689) + S2B**4*(AllHix(1797) + AllHix(1799)) +
     &    (2*MW2**2*SW2**2)/(Alfa2*Pi**2)*AllHix(919) -
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*AllHix(1692))

        AllHix(1818) =
     &   CAB*(2*MA02 - Mh02)*S2B**2*(2*m12sq - Mh02*S2B)*SBA +
     &    S2B**4*AllHix(1802) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*AllHix(1698)) +
     &    AllHix(1683)*Re(AllHil(8)) + AllHix(1687)*Re(AllHil(9))

        AllHix(310) = 2*SB2*SBA*AllHix(309) +
     &    (MW2*SW2)/(Alfa*Pi)*(Lambda6*S2B*AllHix(306))

        AllHix(424) = -(CBA2**2*(MA02 - Mh02)*Mh02*S2B**2) +
     &    2*AllHix(423)

	AllHix(456) = (MA02 - Mh02)*S2B**2*SBA2**2 + 2*AllHix(455)

        AllHix(469) = 2*MA02**3*S2B**2*(3 + 4*SBA2) +
     &    MA02*Mh02*(Mh02*S2B*AllHix(461) + 2*AllHix(463)) +
     &    Mh02**2*(Mh02*S2B*AllHix(464) - 2*AllHix(466)) -
     &    MA02**2*(Mh02*S2B*AllHix(467) - 2*AllHix(468))

	AllHix(495) = 3*MHH2*S2B*SAB*SBA2 - SBA*(4 - SBA2)*AllHix(80)

        AllHix(527) = CBA2*(MHH2 - MHp2)*S2B*AllHix(523) -
     &    MHH2*AllHix(526)

        AllHix(538) = MHH2*S2B*(S2B - 4*CBA*SAB + S2B*SBA2) -
     &    2*CBA*(1 - 3*SBA2)*AllHix(519)

        AllHix(539) = MHH2*S2B*(S2B + 4*CBA*SAB + S2B*SBA2) +
     &    2*CBA*(1 - 3*SBA2)*AllHix(519)

        AllHix(558) = (Mh02 - MHH2)*(Mh02 - MHp2)*
     &     (MHH2 - MHp2)*(2*C2B*CBA*m12sq*Mh02 + CAB*AllHix(553))+
     &      (MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*AllHix(557))

        AllHix(565) = CBA*AllHix(83) +
     &    SBA2*AllHix(67)*AllHix(563) +
     &    CBA2*(S2B*AllHix(58) - 6*AllHix(564))

        AllHix(581) = -2*CBA*(MA02 - Mh02)*(MA02 - MHH2)**2*
     &     (Mh02 - MHH2)*SBA2*(MHH2*S2B*SAB - SBA*AllHix(80)) +
     &    CBA2*AllHix(580)

        AllHix(712) = MA02*SBA2*AllHix(708) -
     &    MHH2*(MHH2*S2B*SBA2*AllHix(709) - 2*CBA*AllHix(711))

        AllHix(754) = S2B*SBA2*AllHix(751) -
     &    Mh02*(8*CBA*SBA*AllHix(80) + S2B*SBA2*AllHix(752)) +
     &    2*CBA2**2*(Mh02 - MHp2)*AllHix(753)

        AllHix(761) = CBA2**2*Mh02*(Mh02 - MHp2)**2*
     &     (5*Mh02 + 2*MHp2)*S2B**2 +
     &    (Mh02 - MHp2)*MHp2*
     &     (S2B*SBA2*AllHix(751) - Mh02*AllHix(760))

        AllHix(770) = Mh02**4*S2B*AllHix(762) +
     &    2*Mh02**3*AllHix(764) - 2*AllHix(769)

        AllHix(796) = MHp2*AllHix(781) -
     &    ((Mh02 - MHp2)*(MHH2 - MHp2)*AllHix(795))/
     &     (Mh02*MHH2*S2B**3)

        AllHix(842) = (MHH - MHp)*(MHH + MHp)*AllHix(840) +
     &    AllHix(841)/(Mh02*MHH2*(MHH2 - MHp2))

        AllHix(860) = 2*CAB*Mh02**2*(Mh02 - MHH2)*
     &     (Mh02 - MHp2)**2*(MHH2 - MHp2)*SBA +
     &    S2A*SBA2*AllHix(859)

        AllHix(883) = 2*SB2*SBA*AllHix(882) +
     &    (MW2*SW2)/(Alfa*Pi)*(Lambda6*S2B*AllHix(881))

        AllHix(890) = SBA*AllHix(183) +
     &    CBA2*AllHix(563)*(S2B*AllHix(58) - 6*AllHix(564))

        AllHix(927) = Mh02**2*S2B**2*SBA2*AllHix(925) -
     &    (6*MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (CBA2*(1 - 4*CBA2 - 9*CBA2**2)*AllHix(919)) +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*Mh02*S2B*(CB2*Lambda6 - Lambda7*SB2)*SBA*
     &       AllHix(926))

        AllHix(935) = -24*(1 - 2*CBA2)*MA02**3*S2B**2*SBA -
     &    MA02*Mh02*AllHix(932) + 2*MA02**2*AllHix(933) -
     &    Mh02**2*AllHix(934)

        AllHix(972) = 16*CAB**2*
     &     (9*AllHix(955)*AllHix(956) + Mh02*AllHix(958)) +
     &    4*MA02**2*AllHix(960) + Mh02**2*AllHix(961) -
     &    2*Mh02*AllHix(964) -
     &    4*CAB*(18*AllHix(955)*AllHix(966) - Mh02*AllHix(971))

        AllHix(995) = 16*AllHix(980) - MA02*Mh02*AllHix(982) +
     &    4*AllHix(987) - 4*CAB*
     &     (2*m12sq*AllHix(989) + Mh02*S2B*AllHix(994))

        AllHix(1008) =
     &   AllHix(65)/(MA02 - MHp2)**2 +
     &    (2*AllHix(83)*AllHix(153))/(MHH2 - 4*MHp2)

        AllHix(1010) =
     &   CBA*(MHH2 - 2*MHp2)*S2B**2 +
     &    2*((2*m12sq - MHH2*S2B)*SAB + AllHix(1009))

        AllHix(1020) =
     &   -(((2*MA02*MHp2 + MHp2**2)*SBA2)/(MHH2 - MHp2)**2) +
     &    (4*AllHix(1019))/
     &     ((4*MA02 - MHH2)*(MA02 - MHp2)*(MHH2 - MHp2))

        AllHix(1023) =
     &   -(((MA02**2 + 2*MA02*MHp2)*SBA2)/(MA02 - MHH2)**2) +
     &    (2*AllHix(1022))/(MHH2 - 4*MHp2)

        AllHix(1026) =
     &   CBA2*S2B**4*AllHix(1024) -
     &    4*(2*m12sq - MHH2*S2B)*SAB*AllHix(1025) +
     &    (2*MW2**2*SW2**2)/(Alfa2*Pi**2)*((4 - SBA2)*AllHix(919))

        AllHix(1027) =
     &   -(((MHH2 - 2*MHp2)*AllHix(65))/(MA02 - MHp2)**2) +
     &    (2*AllHix(83)*AllHix(1001))/(MHH2 - 4*MHp2)

        AllHix(1040) =
     &   (AllHil(4)*AllHix(65)*AllHix(83))/(MA02 - MHp2)**2 +
     &    (AllHil(3)*AllHix(65)**2*AllHix(127))/
     &     ((4*MA02 - MHH2)*(MA02 - MHp2)*(MHH2 - MHp2))

        AllHix(1053) =
     &   -((MHH2*SBA*AllHix(32))/((MA02 - MHH2)*(Mh02 - MHH2))) +
     &    (CBA*AllHix(65)*AllHix(1052))/
     &     ((MA02 - Mh02)**2*(4*MA02 - MHH2))

        AllHix(1055) =
     &   2*(MA02 - Mh02)*MHH2*SBA*AllHix(32) -
     &    (2*MA02 - MHH2)*AllHix(1054)

        AllHix(1058) =
     &   (SBA2*AllHix(1057)**2)/((4*MA02 - Mh02)*(MA02 - MHH2)) +
     &    1/4.D0*(CBA2*AllHix(65)**2)/((MA02 - Mh02)*(4*MA02 - MHH2))

        AllHix(1062) =
     &   (MA02 - Mh02)**2*SBA*SBA2*AllHix(32)*AllHix(67) +
     &    (MA02 - MHH2)*AllHix(1061)

        AllHix(1069) =
     &   (-6*Mh02*AllHil(36)*AllHix(176))/(Mh02 - MHH2) +
     &    (MA02*AllHil(35)*AllHix(1068))/(MA02 - MHH2)**2

        AllHix(1074) =
     &   4*AllHix(1071) + (AllHix(1057)*AllHix(1073))/
     &     ((MA02 - Mh02)*(MA02 - MHH2)**2)

        AllHix(1076) =
     &   CBA*AllHix(65) - (2*(MA02 - Mh02)*AllHix(213)*
     &       AllHix(1050))/((MA02 - MHH2)*(4*Mh02 - MHH2))

        AllHix(1083) =
     &   CBA2*(MA02 - MHH2)**2*AllHix(1077) + SBA2*AllHix(1080) -
     &    (MA02 - Mh02)*(MA02 - MHH2)*AllHix(1082)

        AllHix(1090) =
     &   AllHix(1085)/((MA02 - Mh02)**2*(MA02 - MHH2)) +
     &    (2*AllHix(1086))/(Mh02 - MHH2) +
     &    (S2B*AllHix(58) - 6*AllHix(564))*AllHix(1089)

        AllHix(1101) =
     &   (AllHix(67)*AllHix(1096))/
     &     ((MA02 - Mh02)*(MA02 - MHH2)**2) +
     &    AllHix(1049)*AllHix(1100)

        AllHix(1102) =
     &   -((CBA*MA02*AllHil(43)*AllHix(67)*
     &         (CBA*AllHix(1093) + 2*AllHix(1095)))/
     &       ((MA02 - Mh02)**2*(MA02 - MHH2))) -
     &    AllHil(44)*AllHix(1101)

        AllHix(1110) =
     &   AllHil(3)**2*AllHix(65)*AllHix(1109) +
     &    MA02*AllHil(43)*AllHix(1109)**2

        AllHix(1156) =
     &   2*AllHil(3)*AllHix(1116) -
     &    2*AllHil(1)*(12*AllHil(3)*AllHix(65)*AllHix(1122) -
     &       AllHix(1129)) -
     &    4*AllHix(65)*(AllHil(1)**2*AllHix(1134) -
     &       AllHil(39)*AllHix(1141)) + AllHix(1155)

        AllHix(1158) =
     &   -(AllHix(67)**2/((MA02 - Mh02)*(Mh02 - 4*MHH2))) +
     &    AllHix(213)**2/((MA02 - MHH2)*(4*Mh02 - MHH2))

        AllHix(1161) =
     &   (CBA2*MA02*SBA*(S2B*AllHix(58) - 6*AllHix(564))*
     &       AllHix(1068))/(MA02 - MHH2)**2 -
     &    (2*SBA2*AllHix(67)**2*AllHix(1098))/
     &     ((Mh02 - 4*MHH2)*(MA02 - MHH2))

        AllHix(1167) =
     &   (3*SBA2*AllHix(32)**2)/(4*MA02 - Mh02) +
     &    (CBA*AllHix(1166))/(MA02 - Mh02)**2

        AllHix(1181) =
     &   (CBA2*AllHix(65)**2)/(4*MA02 - MHH2) +
     &    (SBA*AllHix(1180))/(MA02 - MHH2)**2

        AllHix(1185) =
     &   16*AllHix(1058) -
     &    (CBA*SBA2*AllHix(67)*
     &       (CBA*AllHix(1093) + 2*AllHix(1095)))/
     &     ((MA02 - Mh02)**2*(MA02 - MHH2))

        AllHix(1192) =
     &   (SBA2*AllHix(32)**2)/(4*MA02 - Mh02) +
     &    (CBA*AllHix(1166))/(MA02 - Mh02)**2

        AllHix(1193) =
     &   (MHH2*AllHix(1181))/(MA02 - Mh02) +
     &    (Mh02*AllHix(1192))/(MA02 - MHH2)

        AllHix(1231) =
     &   -2*AllHix(1222) +
     &    ((MA02 - MHp2)*AllHix(183)**2*AllHix(1230))/
     &     ((MA02 - Mh02)*(Mh02 - 4*MHp2))

        AllHix(1232) =
     &   MHp2*AllHix(32)**2 + MA02*AllHix(183)**2 -
     &    4*(MA02 - Mh02 + MHp2)*AllHix(1222)

        AllHix(1272) =
     &   8*(MA02 - Mh02)*(Mh02 - MHp2)*AllHix(1258) +
     &    CBA2*AllHix(1271)

        AllHix(1294) =
     &   Mh02*AllHix(1279) - MA02*AllHix(1285) +
     &    4*(Mh02 - 4*MHp2)*AllHix(1293)

        AllHix(1311) =
     &   (AllHix(183)**2*AllHix(1305))/(Mh02 - 4*MHp2) +
     &    AllHix(1308)/(MA02 - MHp2) -
     &    AllHix(1310)/(MA02 - MHp2)**2

        AllHix(1346) =
     &   CAB**2*AllHix(1341) +
     &    (MW2*SW2)/(Alfa*Pi)*(CBA2**2*AllHix(1345))

        AllHix(1357) =
     &   -32*dup7*(2*m12sq - Mh02*S2B)*AllHix(1343) -
     &    6*CBA2**2*(Mh02 - 4*MHp2)*AllHix(1350) +
     &    CBA2*AllHix(1356) +
     &    (108*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*CBA2**3*m12sq*(CB2*Lambda6 - Lambda7*SB2)*
     &       AllHix(1322))

        AllHix(1364) =
     &   2*(AllHix(1359)/(Mh02 - MHH2) +
     &       (S2B*AllHix(58) - 6*AllHix(564))*AllHix(1361)) -
     &    AllHix(1363)/(MHH2 - MHp2)

        AllHix(1370) =
     &   2*AllHix(1367) - AllHix(1369)/
     &     ((Mh02 - MHp2)**2*(MHH2 - MHp2))

        AllHix(1387) =
     &   SBA*AllHix(183) +
     &    (2*(MHH2 - MHp2)*AllHix(67)*AllHix(1386))/
     &     ((Mh02 - 4*MHH2)*(Mh02 - MHp2))

        AllHix(1394) =
     &   -((AllHix(67)*AllHix(1388))/
     &       ((Mh02 - MHp2)*(MHH2 - MHp2)**2)) +
     &    AllHix(1049)*AllHix(1393)

        AllHix(1400) =
     &   (2*CBA*Mh02*AllHix(83)*AllHix(183))/(Mh02 - MHH2) +
     &    (AllHix(1397)*AllHix(1399))/(MHH2 - MHp2)**2

        AllHix(1402) =
     &   CBA*AllHix(83) + (2*(Mh02 - MHp2)*
     &       (S2B*AllHix(58) - 6*AllHix(564))*AllHix(1386))/
     &     ((4*Mh02 - MHH2)*(MHH2 - MHp2))

        AllHix(1409) =
     &   2*(Mh02 - MHp2)*(MHH2 - MHp2)*AllHix(1403) +
     &    (Mh02 - 4*MHp2)*(MHH2 - 4*MHp2)*AllHix(1408)

        AllHix(1416) =
     &   AllHil(2)**2*AllHix(183)*AllHix(1415) +
     &    MHp2*AllHil(51)*AllHix(1415)**2

        AllHix(1425) =
     &   CBA2*MHp2*AllHil(59)*AllHix(183)**2 -
     &    (AllHil(53)*AllHix(183)*AllHix(1424))/(Mh02 - 4*MHp2)

        AllHix(1488) =
     &   AllHil(2)*AllHix(1464) +
     &    (2*AllHil(4)**2*AllHix(183)*AllHix(1432) -
     &       AllHil(4)*AllHix(1487))/MHp2

        AllHix(1493) =
     &   CBA2*(MHH2 - MHp2)**2*AllHix(83)**2 +
     &    (MHH2 - 4*MHp2)*SBA*AllHix(1492)

        AllHix(1521) =
     &   -(AllHix(1516)/(MHH2 - MHp2)) + CBA2*AllHix(1520)

        AllHix(1523) =
     &   (Mh02**2*AllHix(1402))/(Mh02 - MHp2)**2 -
     &    (SBA*AllHix(1522))/(Mh02 - MHp2) +
     &    AllHix(1364)*(3*Mh02 + 3/2.D0*MHH2)

        AllHix(1527) =
     &   (4*MHp2**2*AllHix(1403))/
     &     ((Mh02 - 4*MHp2)*(MHH2 - 4*MHp2)*(Mh02 - MHp2)) -
     &    SBA2*AllHix(1526)

        AllHix(1532) =
     &   (SBA2*AllHix(183)**2)/(Mh02 - 4*MHp2) +
     &    (CBA*AllHix(1531))/(Mh02 - MHp2)**2

        AllHix(1552) =
     &   AllHil(3)**2*AllHix(83)*AllHix(1548) -
     &    MHp2*AllHil(58)*AllHix(1548)**2 -
     &    3*AllHil(45)*AllHix(520)*AllHix(1551)

        AllHix(1563) =
     &   MHp2*SBA2*AllHil(59)*AllHix(83)**2 -
     &    (AllHil(57)*AllHix(83)*AllHix(1562))/(MHH2 - 4*MHp2)

        AllHix(1627) =
     &   (2*SBA2*AllHix(67)**2*AllHix(1389))/
     &     ((Mh02 - 4*MHH2)*(MHH2 - MHp2)) + CBA2*AllHix(1626)

        AllHix(1628) =
     &   (2*SBA2*AllHix(183)**2)/(Mh02 - 4*MHp2) + AllHix(1532)

        AllHix(164) = AllHix(45)/(MHH2 - MHp2)**2 -
     &    (2*(MA02*AllHix(159) - 2*AllHix(163)))/(MHH2 - MHp2)

        AllHix(1649) =
     &   2*m12sq*AllHix(1644) - SBA*AllHix(1648) +
     &    (12*MW2*SW2)/(Alfa*Pi)*
     &     (MHH2*S2B*SAB*(CB2*Lambda6 - Lambda7*SB2)*(1 + 3*SBA2))

        AllHix(1661) =
     &   CAB*(CBA*AllHix(1653) + 6*AllHix(1655)) + S2B*AllHix(1660)

        AllHix(1666) =
     &   -4*CAB*MHp2*(2*m12sq - Mh02*S2B) + MA02*AllHix(32) +
     &    3*Mh02*AllHix(176) - SBA*AllHix(1665) +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*(2*MHp2 + 3*MHH2*SBA2))

        AllHix(1675) =
     &   -(AllHix(32)*Re(AllHil(1))) -
     &    3*AllHix(176)*Re(AllHil(2)) +
     &    AllHix(1674)*Re(AllHil(3)) - 2*AllHix(183)*Re(AllHil(4))

        AllHix(1677) =
     &   AllHix(65)*Re(AllHil(1)) - AllHix(1676)*Re(AllHil(2)) -
     &    3*AllHix(520)*Re(AllHil(3)) - 2*AllHix(83)*Re(AllHil(4))

        AllHix(1693) =
     &   4*AllHix(1689) + S2B**4*AllHix(1690) +
     &    (2*MW2**2*SW2**2)/(Alfa2*Pi**2)*AllHix(919) -
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*AllHix(1692)) -
     &    AllHix(1683)*Re(AllHil(8)) - AllHix(1687)*Re(AllHil(9))

        AllHix(1705) =
     &   dup8/S2B**4 + CBA2*(MA02 - Mh02)**2*Re(AllHil(11)) +
     &    (MA02 - MHH2)**2*SBA2*Re(AllHil(12))

        AllHix(1727) =
     &   -4*CAB**2*(2*m12sq - Mh02*S2B)**2 + 4*AllHix(1699) -
     &    S2B**4*AllHix(1719) - SBA2*AllHix(1720) -
     &    9*AllHix(1723) + 2*AllHix(1726)

        dup9 = CBA2*AllHix(1733)*Re(AllHil(21)) +
     &    AllHix(1683)*Re(AllHil(22))

        AllHix(1734) =
     &   2*dup9 + AllHix(1700)*Re(AllHil(18)) +
     &    9*AllHix(1723)*Re(AllHil(19)) +
     &    SBA2*AllHix(1732)*Re(AllHil(20))

        AllHix(1735) =
     &   2*S2B**4*AllHix(1731) +
     &    (MA02 - Mh02)*(MA02 + Mh02)*
     &     (4*dup8 + 2*AllHix(1717) + AllHix(1727) -
     &       2*AllHix(1734))

        AllHix(1757) =
     &   4*AllHix(1703) + S2B**4*AllHix(1750) +
     &    CBA2*AllHix(1751) + 9*AllHix(1754) + 2*AllHix(1756)

        AllHix(1761) =
     &   AllHix(1704)*Re(AllHil(26)) +
     &    CBA2*AllHix(1733)*Re(AllHil(27)) +
     &    9*AllHix(1754)*Re(AllHil(28)) +
     &    2*(SBA2*AllHix(1732)*Re(AllHil(29)) +
     &       AllHix(1687)*Re(AllHil(30)))

        AllHix(1762) =
     &   2*S2B**4*AllHix(1760) +
     &    (MA02 - MHH2)*(MA02 + MHH2)*
     &     (4*dup8 + 2*AllHix(1717) - AllHix(1757) -
     &       2*AllHix(1761))

        AllHix(1768) =
     &   MHH2*AllHix(1766) +
     &    2*(2*MA02 - MHH2)*(AllHix(1761) + S2B**4*AllHix(1767))

        AllHix(179) = -2*AllHil(34)*AllHix(32) +
     &    6*AllHil(36)*AllHix(176) +
     &    (MA02*AllHil(35)*
     &       (S2B*SBA*AllHix(177) + 4*Mh02*AllHix(178)))/Mh02**2

        AllHix(1801) =
     &   4*AllHix(1703) + S2B**4*AllHix(1750) +
     &    CBA2*AllHix(1751) + 9*AllHix(1754) +
     &    2*(AllHix(1756) + AllHix(1761) + S2B**4*AllHix(1767)) -
     &    2*AllHix(1800)

        AllHix(1803) =
     &   S2B**4*AllHix(1802) + AllHix(1683)*Re(AllHil(8)) +
     &    AllHix(1687)*Re(AllHil(9))

        AllHix(1807) =
     &   4*AllHix(1689) + S2B**4*AllHix(1806) +
     &    (2*MW2**2*SW2**2)/(Alfa2*Pi**2)*AllHix(919) -
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*AllHix(1692))

        AllHix(1811) =
     &   4*AllHix(1703) + S2B**4*AllHix(1750) +
     &    CBA2*AllHix(1751) + 9*AllHix(1754) + 2*AllHix(1756) -
     &    2*AllHix(1800) - 4*AllHix(1803)

        AllHix(1819) =
     &   4*CAB**2*(2*m12sq - Mh02*S2B)**2 + S2B**4*AllHix(1719) +
     &    SBA2*AllHix(1720) + 9*AllHix(1723) -
     &    2*(AllHix(1726) + AllHix(1807)) - 4*AllHix(1818)

        AllHix(212) = ((MA02 - MHH2)*SBA2*AllHix(67))/S2B**2 +
     &    AllHix(211)/((MA02 - Mh02)**2*(Mh02 - MHH2))

        AllHix(215) = (CBA2*(MA02 - MHH2)*AllHix(67))/
     &     ((MA02 - Mh02)*MHH2) - AllHix(214)/Mh02

        AllHix(220) = -2*CBA*SBA*AllHix(80) +
     &    S2B*(SBA2*AllHix(79) + CBA2*AllHix(219))

        AllHix(311) = CAB*CBA*(MA02 - Mh02)*Mh02*S2B**2 -
     &    S2B*AllHix(306)*AllHix(307) + CB2*AllHix(310)

        AllHix(339) = 2*CBA*(MHH2*S2B*SAB - SBA*AllHix(80)) +
     &    CBA2*S2B*AllHix(338)

        AllHix(351) = MA02*(AllHil(1) + 2*AllHil(55))*
     &    AllHix(83)*AllHix(183)

        AllHix(353) = -((AllHix(32)*AllHix(65))/
     &       (Mh02 - MHp2)) +
     &    (2*AllHix(83)*AllHix(183))/(MA02 - Mh02)

        AllHix(354) = -((AllHix(32)*AllHix(65))/
     &       (MHH2 - MHp2)) +
     &    (2*AllHix(83)*AllHix(183))/(MA02 - MHH2)

        AllHix(374) = CAB*(2*m12sq - Mh02*S2B)*AllHix(65) +
     &    S2B**2*AllHix(373)

        AllHix(388) = -2*CBA*dup4*(MA02 - Mh02)*
     &     (MA02 + 2*Mh02 - 2*MHp2)*(Mh02 - MHp2) -
     &    CBA2**2*S2B**2*AllHix(379) + CBA2*S2B*AllHix(387)

        AllHix(408) = -2*CBA*dup4*(MA02 + Mh02 - 2*MHp2) -
     &    CBA2*S2B*(2*CAB*SBA*AllHix(406) + S2B*AllHix(407))

        AllHix(420) = 2*CBA*dup4*(MA02 - Mh02)*
     &     (MA02 - Mh02 - 2*MHp2)*(Mh02 - MHp2) -
     &    2*CBA2**2*S2B**2*AllHix(410) + CBA2*S2B*AllHix(419)

        AllHix(425) = CBA2*Mh02*S2B*AllHix(422) +
     &    (MA02 - MHp2)*AllHix(424)

        AllHix(428) = 2*CBA*AllHix(80) +
     &    S2B*(2*CAB*Mh02 + SBA*AllHix(308))

        AllHix(457) = 6*CBA2**2*(MA02 - Mh02)*
     &     (Mh02*S2B**2 - 2*SBA*AllHix(171)) +
     &    S2B*SBA2*AllHix(448) + CBA2*AllHix(454) -
     &    Mh02*AllHix(456)

        AllHix(470) = 4*CBA2**2*(MA02 - Mh02)**2*(MA02 + Mh02)*
     &     S2B**2 + (MA02 - Mh02)*AllHix(460) + CBA2*AllHix(469)

        AllHix(496) = CBA2*S2B*AllHix(488) +
     &    SBA2*AllHix(490) +
     &    (CBA*(CBA*S2B*AllHix(494) - 2*MHH2*AllHix(495)))/
     &     (MHH2 - MHp2)

        AllHix(497) = AllHix(496)/S2B**2 +
     &    8*CW2*SW2*(MHH2*(MHH2 + MHp2)*SBA2)/MHp2

	dup10 = -4*MHH2*AllHix(489) + CBA*S2B*AllHix(522)

        AllHix(528) = CBA*dup10*MHp2*AllHil(58) +
     &    2*AllHil(49)*AllHix(527)

        AllHix(536) = -6*AllHil(45)*AllHix(520) +
     &    (MA02*AllHil(43)*AllHix(535))/MHH2**2

        AllHix(540) = CBA2*(MA02 - MHH2)*S2B*AllHix(537) -
     &    MHH2*(MHH2*AllHix(538) + MA02*AllHix(539))

        AllHix(559) = 2*CBA*(Mh02 - MHH2)*(Mh02 - MHp2)*
     &     (MHH2 - MHp2)**2*SBA2*(MHH2*S2B*SAB - SBA*AllHix(80)) +
     &    CBA2*(SBA2*AllHix(552) - 2*SBA*AllHix(558))

        AllHix(562) = 2*CBA*(MHH2*S2B*SAB - SBA*AllHix(80)) +
     &    CBA2*S2B*AllHix(561)

        AllHix(566) = (Mh02*(MHH2 - MHp2)*SBA2)/
     &     (Mh02 - MHp2) + AllHix(565)/S2B**2

        AllHix(582) = CBA2**2*(MA02 - Mh02)**3*
     &     (MA02 - MHH2)**2*(Mh02 - MHH2)*S2B**2 + MHH2*AllHix(581)

        AllHix(59) = -(S2B*AllHix(58)) +
     &    6*(m12sq*S2A + (MW2*SW2)/(Alfa*Pi)*
     &        (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA))

        AllHix(650) = 2*(MHH2*S2B*SAB - SBA*AllHix(80)) +
     &    CBA*S2B*AllHix(308)

        AllHix(713) = CBA2**2*(3*Mh02 - 2*MHH2)*(MA02 - MHH2)*
     &     S2B**2 + CBA2*S2B*AllHix(706) + 2*AllHix(712)

        AllHix(717) = 2*CBA*AllHix(80) +
     &    S2B*(2*CAB*Mh02 + SBA*AllHix(716))

        AllHix(731) = 2*MHH2*S2B**2*SBA2**2 +
     &    8*CBA*SBA*AllHix(80) + SBA2*AllHix(730)

        AllHix(771) = (4*AllHil(4)*
     &       (MHp2*AllHix(754) + CBA2*AllHix(759)))/
     &     ((Mh02 - MHp2)*MHp2) +
     &    (2*AllHix(761) + CBA2*AllHix(770))/(Mh02 - MHp2)**2

        AllHix(797) = Mh02**4*(MHH2 - 2*MHp2) +
     &    6*MHH2**3*MHp2**2 - 6*(Mh02**2 + MHH2**2)*MHp2**3 +
     &    MHH2*MHp2**4 + Mh02*AllHix(780) - 2*AllHix(796)

	AllHix(808) = CBA*dup10*MHp2**2 + 2*MHp2*AllHix(527)

	AllHix(843) = Mh02 + MHp2 - (Mh02 - MHp2)*AllHix(842)

	AllHix(861) = S2B**2*SBA2*AllHix(855) - S2B*AllHix(860)

        AllHix(884) = CAB*CBA*Mh02*(Mh02 - MHp2)*S2B**2 -
     &    S2B*AllHix(307)*AllHix(881) + CB2*AllHix(883)

        AllHix(885) = -((SBA*AllHix(183))/Mh02) +
     &    AllHix(67)*AllHix(823)

        AllHix(887) = (CBA*AllHix(83))/MHH2 +
     &    (S2B*AllHix(58) - 6*AllHix(564))*AllHix(886)

        AllHix(891) = (CBA2*AllHix(889))/(MHH2 - MHp2) +
     &    (SBA2*AllHix(67) + AllHix(890))/S2B**2

        AllHix(936) = MA02**3*S2B**2*AllHix(924) +
     &    MA02**2*AllHix(927) +
     &    CAB*(Mh02*S2B*AllHix(931) - 2*m12sq*AllHix(935))

        AllHix(944) = AllHil(2)**2*AllHix(32)*AllHix(943) +
     &    MA02*AllHil(35)*AllHix(943)**2

        AllHix(996) = 2*AllHix(32)*
     &     (AllHil(1)**2*AllHix(949) + AllHil(34)*AllHix(954)) +
     &    AllHil(2)*AllHix(972) +
     &    AllHil(1)*(12*AllHil(2)*AllHix(32)*AllHix(975) +
     &       AllHix(995))

        AllHix(1002) =
     &   ((MHH2 - 2*MHp2)*AllHix(65))/(MA02 - MHp2)**2 -
     &    (2*AllHix(83)*AllHix(1001))/(MHH2 - 4*MHp2)

        AllHix(1006) =
     &   -((2*MA02 - MHH2)*AllHix(83)) +
     &    (AllHix(65)*AllHix(1005))/
     &     ((4*MA02 - MHH2)*(MHH2 - MHp2)**2)

        AllHix(1007) =
     &   MHp2*AllHix(65)**2 +
     &    (MA02 - MHH2 + MHp2)*AllHix(65)*AllHix(83) +
     &    MA02*AllHix(83)**2

        AllHix(1011) =
     &   AllHil(4)**2*AllHix(83)*AllHix(1008) +
     &    (AllHil(1)**2*AllHix(65)*AllHix(1010))/(MA02 - MHp2)**2

        AllHix(1051) =
     &   SBA*AllHix(32) - (2*(MA02 - MHH2)*AllHix(67)*
     &       AllHix(1050))/((MA02 - Mh02)*(Mh02 - 4*MHH2))

        AllHix(1056) =
     &   2*AllHix(1053) - AllHix(1055)/
     &     ((MA02 - Mh02)**2*(MA02 - MHH2))

        AllHix(1063) =
     &   8*AllHix(1058) + AllHix(1062)/
     &     ((MA02 - Mh02)**2*(MA02 - MHH2)**2)

        AllHix(1075) =
     &   (CBA2*(S2B*AllHix(58) - 6*AllHix(564))*AllHix(1069))/
     &     (MA02 - Mh02) + 2*AllHil(34)*AllHix(1074)

        AllHix(1084) =
     &   CBA2*AllHil(2)**2*(S2B*AllHix(58) - 6*AllHix(564))*
     &     AllHix(1076) - (2*AllHil(41)*AllHix(1083))/
     &     (MA02 - MHH2)**2

        AllHix(1103) =
     &   CBA2*AllHil(42)*(S2B*AllHix(58) - 6*AllHix(564))*
     &     AllHix(1090) + SBA2*AllHix(1102)

        AllHix(1157) =
     &   12*dup6*AllHil(45)*AllHix(520) + 4*AllHix(1110) +
     &    AllHix(1156)/(4*MA02 - MHH2)

        AllHix(1160) =
     &   (SBA2**2*AllHix(67)**2)/((Mh02 - 4*MHH2)*(MA02 - MHH2)) +
     &    CBA2*SBA2*AllHix(1158) +
     &    (-((CBA2**2*AllHix(59)**2)/(4*Mh02 - MHH2)) +
     &       (CBA*SBA*(CBA*AllHix(32)*AllHix(59) +
     &            AllHix(65)*AllHix(1159)))/(MA02 - MHH2))/
     &     (MA02 - Mh02)

        AllHix(1223) =
     &   (AllHix(183)**2*AllHix(1214))/(Mh02 - 4*MHp2) -
     &    (2*(Mh02 - 2*MHp2)*AllHix(1222))/(MA02 - MHp2)**2

        AllHix(1233) =
     &   2*AllHil(1)**2*AllHix(1225) - 2*AllHil(34)*AllHix(1229) -
     &    AllHil(4)**2*AllHix(1231) - AllHil(48)*AllHix(1232)

        AllHix(1295) =
     &   4*CAB**2*(2*m12sq - Mh02*S2B)**2*AllHix(1238) -
     &    8*(MA02 - Mh02)*(Mh02 - MHp2)*AllHix(1252) -
     &    4*CAB*(2*m12sq - Mh02*S2B)*AllHix(1272) -
     &    CBA2*AllHix(1294) +
     &    (2*MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (CBA2**2*AllHix(919)*AllHix(1237))

        AllHix(1312) =
     &   (2*AllHil(4)*AllHix(183)**2*AllHix(1296))/
     &     ((MA02 - Mh02)*(Mh02 - 4*MHp2)*(MA02 - MHp2)) -
     &    AllHix(1311)

        AllHix(1358) =
     &   -32*S2B**2*AllHix(1325)*AllHix(1326) -
     &    CBA2*AllHix(1334) - 4*AllHix(1346) +
     &    4*CAB*AllHix(1357) +
     &    (6*MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (AllHix(919)*AllHix(1324))

        AllHix(1365) =
     &   (-6*Mh02*SBA*AllHil(36)*AllHix(176))/
     &     ((Mh02 - MHH2)*(Mh02 - MHp2)) + AllHil(42)*AllHix(1364)

        AllHix(1371) =
     &   (-2*MHp2*SBA*AllHil(59)*AllHix(183))/
     &     ((Mh02 - MHp2)*(MHH2 - MHp2)) + AllHil(57)*AllHix(1370)

        AllHix(1395) =
     &   (-6*CBA*MHH2*AllHil(45)*AllHix(67)*AllHix(520))/
     &     ((Mh02 - MHH2)*(MHH2 - MHp2)) +
     &    (AllHil(3)**2*AllHix(1049)*AllHix(1387))/
     &     (MHH2 - MHp2)**2 + AllHil(44)*AllHix(1394)

        AllHix(1401) =
     &   (SBA*AllHix(183)**2*AllHix(1396))/(MHH2 - MHp2)**2 -
     &    AllHix(1400)/(Mh02 - MHp2)

        AllHix(1410) =
     &   CBA2*AllHil(2)**2*(S2B*AllHix(58) - 6*AllHix(564))*
     &     AllHix(1402) + (AllHil(4)**2*AllHix(1409))/
     &     ((Mh02 - 4*MHp2)*(MHH2 - 4*MHp2)*(MHH2 - MHp2)**2)

        AllHix(1489) =
     &   2*(CBA2*(3*AllHil(36)*AllHix(176)*AllHix(1413) +
     &          AllHix(1416)) + AllHix(1425)) -
     &    AllHix(1488)/(Mh02 - 4*MHp2)

        AllHix(165) = AllHix(41) + 2*AllHix(158) -
     &    AllHix(164)/(MA02 - MHp2)

        AllHix(1662) =
     &   -24*m12sq**2*(3*S2A + S2B)*SAB*SBA*SBA2 -
     &    4*m12sq*AllHix(1642) + CBA*AllHix(1649) + 2*AllHix(1661)

        AllHix(1678) =
     &   -(CBA2*SBA*AllHix(1666)) + CBA*SBA2*AllHix(1672) +
     &    2*(CBA2*SBA*AllHix(1675) + CBA*SBA2*AllHix(1677))

        AllHix(1679) =
     &   CBA2*SBA*AllHix(1666) - CBA*SBA2*AllHix(1672) -
     &    2*(CBA2*SBA*AllHix(1675) + CBA*SBA2*AllHix(1677))

        AllHix(1694) =
     &   (MA02 - MHp2)**2 + CBA2*AllHix(1688) +
     &    AllHix(1693)/S2B**4 - (MA02 - MHp2)**2*Re(AllHil(5)) +
     &    (MHH2 - MHp2)**2*(SBA2 - SBA2*Re(AllHil(7)))

        AllHix(1706) =
     &   ((MA02 - MHp2)*AllHix(1697))/S2B**2 + 2*AllHix(1705) +
     &    4*(MA02 - MHp2)**2*Re(AllHil(10))

        AllHix(1709) =
     &   AllHix(1705) + 2*(MA02 - MHp2)**2*Re(AllHil(10))

        AllHix(180) = (2*AllHil(33)*AllHix(175))/Mh02**2 -
     &    SBA*AllHix(179)

        AllHix(1804) =
     &   MHp2*AllHix(1801) - 4*(2*MHH2 - MHp2)*AllHix(1803)

        AllHix(1810) =
     &   (MHH2 - MHp2)*(MHH2 + MHp2)*
     &     (AllHix(1757) + 2*AllHix(1761) - 4*AllHix(1803) -
     &       2*AllHix(1807)) + 2*S2B**4*AllHix(1809)

        AllHix(1812) =
     &   -2*(MHH2 - 2*MHp2)*(AllHix(1761) + S2B**4*AllHix(1767)) +
     &    MHH2*AllHix(1811)

        AllHix(1817) =
     &   -((Mh02 - MHp2)*(Mh02 + MHp2)*
     &       (AllHix(1727) - 2*AllHix(1734) + 4*AllHix(1803) +
     &         2*AllHix(1807))) + 2*S2B**4*AllHix(1816)

        AllHix(1820) =
     &   -2*(Mh02 - 2*MHp2)*(AllHix(1734) + S2B**4*AllHix(1739)) +
     &    Mh02*AllHix(1819)

        AllHix(216) = CBA2*(MA02 + MHH2 - AllHix(208)/MHH2) -
     &    AllHix(212)/Mh02 + ((MA02 - MHH2)*AllHix(215))/S2B**2

        AllHix(221) = AllHix(218)/(Mh02*(MA02 - MHp2)) +
     &    (2*AllHix(220))/((MA02 - Mh02)*S2B**2)

        AllHix(350) = ((MHH2*AllHil(39))/(Mh02 - MHH2) -
     &      (MHp2*AllHil(54))/(Mh02 - MHp2))*AllHix(32)*AllHix(65)

        AllHix(352) = (2*Mh02*AllHil(53)*AllHix(83)*
     &       AllHix(183))/(Mh02 - MHH2) - AllHix(351)/(MA02 - MHH2)

        AllHix(355) = Mh02*AllHil(2)*AllHix(353) +
     &    MHH2*((-4*AllHil(57)*AllHix(83)*AllHix(183))/
     &        (MA02 - MHH2) - AllHil(3)*AllHix(354))

        AllHix(375) = (2*Mh02*AllHil(34)*AllHix(32)*
     &       AllHix(65))/(Mh02 - MHH2) +
     &    (MHp2*AllHil(4)*(CBA*AllHix(371) - 2*AllHix(374)))/
     &     (MHH2 - MHp2)

        AllHix(405) = (4*AllHil(4)*AllHix(388))/
     &     (Mh02 - MHp2)**2 - AllHix(404)/(Mh02 - MHp2)

        AllHix(426) = AllHix(420)/(MA02 - Mh02)**2 +
     &    2*((AllHil(4)*AllHix(408))/(MA02 - Mh02) +
     &       (AllHil(2)*AllHix(425))/(Mh02*(MA02 - MHp2)))

        AllHix(431) = (-2*SBA*AllHix(428))/(MA02 - Mh02) +
     &    AllHix(430)/Mh02**2

        AllHix(471) = (2*AllHil(2)*AllHix(457))/
     &     ((MA02 - Mh02)*Mh02) + AllHix(470)/(MA02 - Mh02)**2

        AllHix(498) = (SBA2*AllHix(486))/
     &     ((MHH2 - 4*MHp2)*MHp2) + AllHix(497)/MHH2

        AllHix(529) = (CBA2*MHp2*AllHil(50)*AllHix(521))/
     &     (MHH2 - MHp2)**2 + AllHix(528)/S2B**2

        AllHix(541) = CBA*AllHix(536) -
     &    2*(AllHil(39)*AllHix(54) +
     &       (AllHil(38)*AllHix(540))/MHH2**2)

        AllHix(560) = CBA2**2*(Mh02 - MHH2)*(Mh02 - MHp2)**3*
     &     (MHH2 - MHp2)**2*S2B**2 - MHH2*AllHix(559)

        AllHix(567) = (SBA2*AllHix(562))/
     &     ((Mh02 - MHp2)*S2B**2) - CBA2*(SBA2 - AllHix(566)/MHH2)

        AllHix(60) = SBA*AllHil(34)*AllHix(32)*AllHix(57) -
     &    (CBA2*MA02*(MA02 - Mh02)*SBA2*AllHil(35)*AllHix(59))/Mh02

        AllHix(604) = (4*AllHil(3)*AllHix(582))/
     &     ((MA02 - MHH2)**2*(Mh02 - MHH2)*MHH2) +
     &    AllHix(603)/(MA02 - MHH2)

        AllHix(608) = -((SBA*AllHil(53)*AllHix(183)*
     &         AllHix(606))/(MHH2 - MHp2)) +
     &    ((CBA2*(MHH2 - MHp2)*MHp2*SBA2*AllHil(58)*AllHix(67))/
     &        MHH2 + CBA*AllHil(57)*AllHix(83)*AllHix(607))/
     &     (Mh02 - MHp2)

        AllHix(653) = (2*CBA*AllHix(650))/(MA02 - MHH2) -
     &    (CBA2*S2B*AllHix(651) + MHH2*AllHix(652))/MHH2**2

        AllHix(69) = -(SBA*AllHix(32)*AllHix(63)) +
     &    CBA*AllHix(64)*AllHix(65) - SBA2*AllHix(66)*AllHix(67) +
     &    CBA2*AllHix(59)*AllHix(68)

        AllHix(714) = AllHix(702)/(MA02 - MHH2)**2 +
     &    (2*AllHil(3)*AllHix(713))/((MA02 - MHH2)*MHH2)

        AllHix(720) = (2*SBA*AllHix(717))/(Mh02 - MHp2) +
     &    AllHix(719)/Mh02**2

        AllHix(735) = CBA2*AllHix(728) -
     &    2*(Mh02 - MHp2)*(2*MHp2*AllHix(731) -
     &       CBA2**2*AllHix(734))

        AllHix(77) = (MA02*(MA02 - MHH2)*SBA2*AllHil(43)*
     &       AllHix(67))/MHH2 +
     &    (AllHil(42)*AllHix(59)*AllHix(76))/(Mh02*(MA02 - MHH2))

        AllHix(772) = (AllHil(4)*
     &       (CBA2*AllHix(742) - 2*MHp2*AllHix(749)))/
     &     ((Mh02 - MHp2)**2*MHp2) - (AllHil(2)*AllHix(771))/Mh02

        AllHix(798) = 3*AllHil(4)**2 -
     &    AllHix(779)/(Mh02 - MHH2) -
     &    (AllHil(40)*AllHix(797))/
     &     ((Mh02 - MHp2)**2*(MHH2 - MHp2)**2)

        AllHix(805) = AllHix(496)/S2B**2 -
     &    16*CW2*SW2*(MHH2*(MHH2 + MHp2)*SBA2)/MHp2

        AllHix(809) = (2*MHp2**2*AllHix(807))/(MHH2 - MHp2) -
     &    SBA2*(4*MHp2*AllHix(518) + AllHix(808)/MHH2**2)

        AllHix(844) = ((Mh02 - MHp2)*MHp2*SBA2*AllHil(51)*
     &       AllHix(59))/(Mh02*(MHH2 - MHp2)*S2B**2) +
     &    AllHil(47)*(AllHix(839)/MHH2 - SBA2*AllHix(843))

        AllHix(878) = S2B**2*SBA2**2*AllHix(851) +
     &    CBA2*AllHix(861) - 2*CBA*AllHix(877)

        AllHix(888) = CBA2*(SBA2 - (SBA2*AllHix(880))/MHH2) -
     &    (2*CBA*SBA*AllHix(884))/
     &     ((Mh02 - MHp2)*(MHH2 - MHp2)*S2B**3) -
     &    (SBA2*AllHix(885) + CBA2*AllHix(887))/S2B**2

        AllHix(892) = -((CBA2*SBA*AllHix(717))/
     &       ((MHH2 - MHp2)*S2B**2)) + (SBA2*AllHix(891))/Mh02

        AllHix(937) = -48*MA02**4*S2B**4*SBA2 -
     &    16*CAB**2*AllHix(916) + MA02*Mh02*AllHix(920) -
     &    Mh02**2*AllHix(923) + 4*AllHix(936)

        AllHix(997) = 6*AllHil(36)*AllHix(176)*AllHix(941) -
     &    2*AllHix(944) - AllHix(996)/(4*MA02 - Mh02)

        AllHix(166) = 3*CBA2*(MA02 - MHp2)**2 -
     &    (dup5*MA02)/((MA02 - MHp2)*(MHH2 - MHp2)*S2B**4) -
     &    MHH2*(MHp2*AllHix(155) - SBA2*AllHix(165))

	AllHix(1663) = CBA*SBA*AllHix(1638) - AllHix(1662)/S2B**4

        AllHix(1680) =
     &   (AllHil(1)*AllHix(1678) + AllHil(2)*AllHix(1679))/
     &     (MA02 - Mh02) -
     &    (AllHil(1)*AllHix(1678) + AllHil(3)*AllHix(1679))/
     &     (MA02 - MHH2) -
     &    (AllHil(2)*AllHix(1678) + AllHil(4)*AllHix(1679))/
     &     (Mh02 - MHp2) +
     &    (AllHil(3)*AllHix(1678) + AllHil(4)*AllHix(1679))/
     &     (MHH2 - MHp2)

        AllHix(1695) =
     &   dup3/S2B**4 + (AllHil(4)*AllHix(1694))/MHp2 +
     &    (MA02 - MHp2)**2*Re(AllHil(5)) +
     &    CBA2*(Mh02 - MHp2)**2*Re(AllHil(6)) +
     &    (MHH2 - MHp2)**2*SBA2*Re(AllHil(7))

        AllHix(1708) =
     &   MHp2*AllHix(1706) - 2*(2*MA02 - MHp2)*AllHix(1707)

        AllHix(1711) =
     &   2*(MA02 - 2*MHp2)*AllHix(1709) + MA02*AllHix(1710)

        AllHix(181) = -((SBA2*AllHix(169))/(MA02 - Mh02)**2) +
     &    AllHix(180)/S2B**2

        AllHix(217) = (CBA*AllHil(39)*AllHix(65)*AllHix(206))/
     &     ((MA02 - Mh02)*S2B**2) + SBA2*AllHil(38)*AllHix(216)

        AllHix(252) = 2*Mh02*AllHil(4)*AllHix(221) +
     &    AllHix(251)/((MA02 - Mh02)**2*(Mh02 - MHp2)**2*S2B**2)

        AllHix(376) = -8*
     &     (AllHix(350)/(MHH2 - MHp2) + AllHix(352)/(MA02 - Mh02))-
     &      (4*AllHix(355))/(Mh02 - MHH2) +
     &    3*(CBA*AllHix(360) - 2*AllHix(366)) +
     &    (4*AllHix(375))/(Mh02 - MHp2)

        AllHix(427) = 8*SBA*AllHil(53)*AllHix(183) -
     &    AllHix(405)/(MA02 - Mh02) -
     &    (4*AllHil(1)*AllHix(426))/(Mh02 - MHp2)

        AllHix(484) = -8*CBA2*AllHil(2)**2*AllHix(431) -
     &    (4*AllHil(2)*AllHix(445))/((MA02 - Mh02)**2*Mh02) +
     &    4*AllHil(1)*AllHix(471) -
     &    (8*AllHil(1)**2*AllHix(473) - AllHix(483))/(MA02 - Mh02)

        AllHix(516) = 4*MHH2*AllHil(4)*AllHix(498) -
     &    (CBA2*S2B*AllHix(506) + 2*AllHix(515))/
     &     ((MHH2 - MHp2)**2*S2B**2)

        AllHix(530) = (CBA2*MHH2*AllHil(37)*AllHix(517))/
     &     (MHH2 - MHp2)**2 +
     &    (2*(AllHil(57)*AllHix(518) -
     &         3*CBA*AllHil(45)*AllHix(520)))/S2B**2 +
     &    AllHix(529)/MHH2**2

        AllHix(542) = (CBA2*AllHix(533))/(MA02 - MHH2)**2 +
     &    AllHix(541)/S2B**2

        AllHix(568) = AllHix(560)/
     &     ((Mh02 - MHH2)*(Mh02 - MHp2)*(MHH2 - MHp2)**2*S2B**2) +
     &    2*MHH2*AllHil(4)*AllHix(567)

        AllHix(605) = (-8*CBA2*(MA0 - Mh0)*(MA0 + Mh0)*SBA2*
     &       AllHil(3)**2*AllHix(67))/((MA02 - MHH2)*MHH2) +
     &    AllHix(604)/(MA02 - Mh02)

        AllHix(61) = (CBA2*MHH2**2*SBA2*AllHil(37)*AllHix(56))/
     &     (Mh02*(Mh02 - MHH2)) - AllHix(60)/S2B**2

        AllHix(649) = 8*
     &     ((CBA2*(Mh0 - MHp)*(Mh0 + MHp)*SBA2*AllHil(3)**2*
     &          AllHix(67))/(MHH2*(MHH2 - MHp2)) + AllHix(608)) -
     &    (4*AllHil(4)*(CBA2*AllHix(616) - 2*CBA*AllHix(628)))/
     &     ((Mh02 - MHp2)**2*(MHH2 - MHp2)**2) -
     &    AllHix(648)/(Mh02 - MHp2)

        AllHix(70) = ((MA02 - MHH2)**2*SBA2**2)/Mh02 +
     &    (CBA2*(MA02 - Mh02)*AllHix(62))/MHH2 + AllHix(69)/S2B**2

        AllHix(715) = -8*SBA2*AllHil(3)**2*AllHix(653) +
     &    (8*AllHil(1)**2*AllHix(673) + AllHix(689))/
     &     (MA02 - MHH2) +
     &    4*((AllHil(3)*AllHix(671))/((MA02 - MHH2)**2*MHH2) +
     &       AllHil(1)*AllHix(714))

        AllHix(773) = 8*CBA2*AllHil(2)**2*AllHix(720) -
     &    AllHix(735)/(Mh02 - MHp2) + 4*AllHix(772)

        AllHix(78) = -((SBA2*AllHil(44)*AllHix(67)*AllHix(73))/
     &       ((MA02 - MHH2)*MHH2)) + CBA2*AllHix(77)

        AllHix(799) = (AllHil(42)*
     &       (S2B*AllHix(58) - 6*AllHix(564))*AllHix(776))/
     &     (Mh02*(Mh02 - MHp2)*(MHH2 - MHp2)*S2B**2) +
     &    SBA2*AllHix(798)

        AllHix(810) = 8*AllHix(801) + SBA2*AllHix(804) -
     &    2*MHp2*AllHix(805) - AllHix(809)/S2B**2

        AllHix(893) = AllHix(878)/
     &     ((Mh02 - MHH2)*(Mh02 - MHp2)**2*(MHH2 - MHp2)*S2B**2) +
     &    2*Mh02*(AllHil(3)*AllHix(888) - AllHil(4)*AllHix(892))

        AllHix(998) = AllHix(937)/AllHix(912) -
     &    (4*AllHix(997))/(MA02 - Mh02)**2

        AllHix(1316) =
     &   MA02*AllHix(1222) +
     &    (AllHix(1057)**2*AllHix(1228))/
     &     ((4*MA02 - Mh02)*(Mh02 - MHp2)**2) + 1/4.D0*AllHix(1301)

        AllHix(1609) =
     &   -54*(MHH2 - MHp2)*AllHix(520)**2 +
     &    2*MHp2*AllHix(1548)**2 + 9*AllHix(520)*AllHix(1551)

        AllHix(1036) =
     &   -(((MHH2 - 2*MHp2)*AllHix(65))/(MA02 - MHp2)**2) +
     &    (2*AllHix(83)*AllHix(1001))/(MHH2 - 4*MHp2) -
     &    MHp2*AllHix(1008)

        AllHix(1044) =
     &   -((2*MA02 - MHH2)*AllHix(83)) + MA02*AllHix(1010) +
     &    (AllHix(65)*AllHix(1043))/
     &     ((4*MA02 - MHH2)*(MHH2 - MHp2)**2)

	AllHix(1171) = Mh02*AllHix(1076) + AllHix(1085)/(MA02 - MHH2)

        AllHix(1176) =
     &   (-4*AllHix(67)**2*AllHix(1050))/
     &     ((MA02 - Mh02)*(Mh02 - 4*MHH2)*(MA02 - MHH2)) +
     &    (AllHix(1049)*AllHix(1051))/(MA02 - MHH2)**2

        AllHix(1317) =
     &   (Mh02*(MA02 - MHp2)*AllHix(32)**2*AllHix(1224))/
     &     ((4*MA02 - Mh02)*(Mh02 - MHp2)) + 4*AllHix(1316)

        AllHix(1382) =
     &   2*CBA2*(MHH2 - MHp2)*MHp2*AllHix(59) +
     &    (Mh02 - MHp2)*(Mh02 - MHH2 - MHp2)*SBA*AllHix(183)

        AllHix(1542) =
     &   (8*AllHix(1403))/
     &     ((Mh02 - 4*MHp2)*(MHH2 - 4*MHp2)*(Mh02 - MHp2)) +
     &    (CBA*SBA2*AllHix(67)*
     &       (CBA*AllHix(1507) + 2*AllHix(1509)))/(Mh02 - MHp2)**2

        AllHix(1569) =
     &   -36*(MHH2 - MHp2)*AllHix(520)**2 +
     &    2*MHp2*AllHix(1548)**2 + 9*AllHix(520)*AllHix(1551) +
     &    AllHix(83)*(CBA*S2B*AllHix(1565) + 2*AllHix(1568))

	AllHix(1610) = MHp2**2*AllHix(1548)**2 + MHH2*AllHix(1609)

        AllHix(1618) =
     &   AllHix(83)*AllHix(1616) -
     &    SBA2*(9*MHH2*AllHix(520)*AllHix(1551) +
     &       MHp2*AllHix(1617))

        AllHix(1737) =
     &   CAB*(2*MA02 - Mh02)*S2B**2*(2*m12sq - Mh02*S2B)*SBA +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*AllHix(1698)) +
     &    AllHix(1700)*Re(AllHil(13)) + AllHix(1704)*Re(AllHil(14))

        AllHix(1744) =
     &   (Mh02 - 2*MHp2)**2*S2B**4 + AllHix(1732)*Re(AllHil(20))

        AllHix(345) = SBA*AllHix(32) + SBA2*AllHix(67) -
     &    (CBA2*AllHix(59)*AllHix(340))/
     &     ((MA02 - Mh02)*(MA02 - MHH2))

        AllHix(1037) =
     &   AllHix(1007)/(MA02 - MHp2)**2 - AllHix(83)*AllHix(1036)

        AllHix(1045) =
     &   MHp2*AllHix(65)**2 + MA02*AllHix(83)**2 +
     &    AllHix(65)*((MA02 - MHH2 + MHp2)*AllHix(83) +
     &       AllHix(1044))

        AllHix(1172) =
     &   (2*AllHix(1086))/(Mh02 - MHH2) +
     &    (S2B*AllHix(58) - 6*AllHix(564))*AllHix(1089) +
     &    AllHix(1171)/(MA02 - Mh02)**2

        AllHix(1175) =
     &   (4*MA02*AllHix(65)**2)/((MA02 - Mh02)*(4*MA02 - MHH2)) +
     &    (S2B*AllHix(58) - 6*AllHix(564))*AllHix(1090) -
     &    (4*(S2B*AllHix(58) - 6*AllHix(564))**2*AllHix(1169))/
     &     (4*Mh02 - MHH2)

	AllHix(1179) = MHH2*AllHix(1176) + AllHix(1178)/(MA02 - MHH2)

        AllHix(1182) =
     &   -(AllHix(1083)/((MA02 - Mh02)**2*(MA02 - MHH2)**2)) -
     &    SBA2*AllHix(1101) + (MA02*AllHix(1181))/(MA02 - Mh02)

        AllHix(1186) =
     &   (6*Mh02**2*SBA*AllHix(176))/
     &     ((MA02 - Mh02)*(Mh02 - MHH2)) -
     &    (2*Mh02 + MHH2)*AllHix(1090)

        AllHix(1194) =
     &   (9*CBA*(MA02 - MHH2)*AllHix(67)*AllHix(520))/
     &     (Mh02 - MHH2) + AllHix(1049)*AllHix(1051)

        AllHix(1200) =
     &   (MA02*(MA02 - Mh02)*(MA02 + 2*Mh02)*SBA*AllHix(1068))/
     &     (MA02 - MHH2)**2 + 2*Mh02**2*AllHix(1076)

        AllHix(1201) =
     &   -2*(MA02 + Mh02 + MHH2)*AllHix(1083) -
     &    CBA*MA02*(MA02 - MHH2)*(MA02 + 2*MHH2)*SBA2*AllHix(67)*
     &     (CBA*AllHix(1093) + 2*AllHix(1095))

        AllHix(1202) =
     &   -(((MA02 + Mh02 + MHH2)*AllHix(1083))/(MA02 - MHH2)**2) -
     &    (CBA*MA02*MHH2*SBA2*AllHix(67)*
     &       (CBA*AllHix(1093) + 2*AllHix(1095)))/(MA02 - MHH2)

        AllHix(1207) =
     &   -(AllHix(1083)/((MA02 - Mh02)**2*(MA02 - MHH2)**2)) +
     &    (MHH2*AllHix(1181))/(MA02 - Mh02) +
     &    (Mh02*AllHix(1192))/(MA02 - MHH2)

        AllHix(1208) =
     &   32*AllHix(1058) - 2*AllHix(1063) -
     &    (CBA*SBA2*AllHix(67)*
     &       (CBA*AllHix(1093) + 2*AllHix(1095)))/
     &     ((MA02 - Mh02)**2*(MA02 - MHH2))

        AllHix(1318) =
     &   AllHix(32)**2*AllHix(1313) +
     &    (4*AllHix(1314))/
     &     ((4*MA02 - Mh02)*(MA02 - MHp2)*(Mh02 - MHp2)) -
     &    MA02*AllHix(1315) + AllHix(1317)/(MA02 - MHp2)**2

	AllHix(1377) = CBA2*MHp2*AllHix(59)**2 + Mh02*AllHix(83)**2

        AllHix(1380) =
     &   CBA2*AllHix(59)*AllHix(1378) +
     &    CBA*(Mh02 - MHp2)*AllHix(83)*AllHix(1379)

        AllHix(1383) =
     &   (Mh02 - MHp2)**2*AllHix(1381) -
     &    (Mh02 - MHp2)*AllHix(67)*AllHix(1382)

        AllHix(1498) =
     &   -(AllHix(59)**2/((4*Mh02 - MHH2)*(MHH2 - MHp2))) +
     &    AllHix(67)**2/((Mh02 - 4*MHH2)*(Mh02 - MHp2))

        AllHix(1512) =
     &   (MHH2*AllHix(1049)*AllHix(1387))/(MHH2 - MHp2)**2 +
     &    AllHix(1394) - AllHix(1511)/(MHH2 - MHp2)

        AllHix(1529) =
     &   (2*MHp2**2*SBA*AllHix(183))/
     &     ((Mh02 - MHp2)*(MHH2 - MHp2)) -
     &    (MHH2 + 2*MHp2)*AllHix(1370)

        AllHix(1536) =
     &   (MHp2*AllHix(1409))/(Mh02 - 4*MHp2) -
     &    2*MHH2*(Mh02 - MHp2)*AllHix(1493)

        AllHix(1541) =
     &   -2*((MHH2*AllHix(1493))/
     &        ((MHH2 - 4*MHp2)*(MHH2 - MHp2)**2) +
     &       MHp2*AllHix(1539)) + CBA2*AllHix(1540)

        AllHix(1543) =
     &   -4*SBA2*AllHix(183)**2 + 2*Mh02*AllHix(1532) +
     &    MHp2*AllHix(1542)

        AllHix(1544) =
     &   CBA2*(MHH2 - MHp2)**2*AllHix(1377) +
     &    (MHH2 - MHp2)*AllHix(1380) + SBA2*AllHix(1383) +
     &    (MHp2*AllHix(1409))/((Mh02 - 4*MHp2)*(MHH2 - 4*MHp2))

        AllHix(1570) =
     &   (4*AllHix(83)**2*AllHix(1564))/(MHH2 - 4*MHp2) -
     &    SBA2*AllHix(1569)

        AllHix(1619) =
     &   AllHix(83)*AllHix(1604) -
     &    4*(MHH2 - MHp2)**2*AllHix(1606) - MHp2**2*AllHix(1608) +
     &    (MHH2 - 4*MHp2)*(SBA2*AllHix(1610) - 1/2.D0*AllHix(1618))

        AllHix(1738) =
     &   -4*CAB**2*(2*m12sq - Mh02*S2B)**2 - SBA2*AllHix(1720) -
     &    9*AllHix(1723) + 2*(AllHix(1717) + AllHix(1726)) +
     &    S2B**4*AllHix(1736) + 4*AllHix(1737)

        AllHix(1745) =
     &   -2*dup9 - 4*CAB*(2*m12sq - Mh02*S2B)*AllHix(1681) -
     &    CBA2*AllHix(1725) - SBA2*AllHix(1744) +
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*AllHix(1682)) -
     &    AllHix(1700)*Re(AllHil(18)) -
     &    9*AllHix(1723)*Re(AllHil(19))

        AllHix(1746) =
     &   -4*CAB**2*(2*m12sq - Mh02*S2B)**2 + 4*AllHix(1699) -
     &    SBA2*AllHix(1720) - 9*AllHix(1723) -
     &    S2B**4*AllHix(1743) + 2*(AllHix(1717) + AllHix(1745))

        AllHix(1769) =
     &   4*AllHix(1703) + CBA2*AllHix(1751) + 9*AllHix(1754) +
     &    2*(AllHix(1756) + AllHix(1761)) - 2*AllHix(1765) +
     &    S2B**4*(AllHix(1750) + 2*AllHix(1767))

        AllHix(315) = ((MA0 - Mh0)*(MA0 + Mh0)*AllHix(59))/
     &     (Mh02*(MA02 - MHH2)) +
     &    ((MA0 - MHH)*(MA0 + MHH)*AllHix(67))/((MA02 - Mh02)*MHH2)

        AllHix(341) = CBA2*AllHix(59) + CBA*AllHix(65) -
     &    (SBA2*AllHix(67)*AllHix(340))/
     &     ((MA02 - Mh02)*(MA02 - MHH2))

        AllHix(346) = (CBA2*(MA02 - Mh02)*MHH2)/
     &     (MA02 - MHH2) - AllHix(345)/S2B**2

        AllHix(830) = ((Mh0 - MHp)*(Mh0 + MHp)*
     &       (Mh02 - MHH2 - MHp2)*SBA2*AllHix(67))/
     &     (MHH2*(MHH2 - MHp2)) - AllHix(59)*AllHix(829)

        AllHix(1028) =
     &   (2*MA02 + MHH2)*AllHix(65)*AllHix(1006) +
     &    2*(MA02 + MHH2 + MHp2)*AllHix(1007)

        AllHix(1032) =
     &   -((AllHix(65)*AllHix(1006) + 2*AllHix(1007))/
     &       (MA02 - MHp2)**2) + AllHix(83)*AllHix(1027) -
     &    (2*AllHix(1031))/(MA02 - MHp2)

        AllHix(1038) =
     &   (MHp2*SBA2*AllHix(65)**2)/(MHH2 - MHp2)**2 -
     &    (2*MA02*AllHix(65)*AllHix(83))/(MA02 - MHp2)**2 +
     &    AllHix(83)**2*((16*AllHix(1015))/
     &        ((MA02 - MHH2)*(MHH2 - 4*MHp2)*(MA02 - MHp2)) +
     &       2*AllHix(1035)) + 2*AllHix(1037)

        AllHix(1046) =
     &   (MA02*SBA2*AllHix(83)**2)/(MA02 - MHH2)**2 +
     &    (AllHix(65)**2*AllHix(1041))/
     &     ((4*MA02 - MHH2)*(MA02 - MHp2)*(MHH2 - MHp2)) -
     &    MHp2*AllHix(1042) - AllHix(1045)/(MA02 - MHp2)**2

        AllHix(1168) =
     &   -((CBA2**2*AllHix(59)**2)/
     &       ((MA02 - Mh02)*(4*Mh02 - MHH2))) +
     &    (SBA2**2*AllHix(67)**2)/
     &     ((Mh02 - 4*MHH2)*(MA02 - MHH2)) + CBA2*SBA2*AllHix(1158)

        AllHix(1173) =
     &   (-4*(S2B*AllHix(58) - 6*AllHix(564))**2*AllHix(1169))/
     &     (4*Mh02 - MHH2) - (Mh02*AllHix(1170))/(MA02 - Mh02) +
     &    (S2B*AllHix(58) - 6*AllHix(564))*AllHix(1172)

        AllHix(1183) =
     &   -(CBA*AllHix(65)*AllHix(1056)) + CBA2*AllHix(1175) +
     &    2*(Mh02*(AllHix(1160) - 2*AllHix(1168)) +
     &       SBA2*AllHix(1179) + AllHix(1182))

        AllHix(1191) =
     &   3*(S2B*AllHix(58) - 6*AllHix(564))*AllHix(1186) -
     &    (2*AllHix(1190))/(MA02 - Mh02) +
     &    (8*(S2B*AllHix(58) - 6*AllHix(564))**2*AllHix(1169)*
     &       (3*Mh02 + 3/2.D0*MHH2))/(4*Mh02 - MHH2)

        AllHix(1198) =
     &   (MHH2**2*AllHix(1194))/(MA02 - MHH2)**2 +
     &    (2*AllHix(1197))/(MA02 - MHH2) -
     &    3/2.D0*((Mh02 + 2*MHH2)*AllHix(1101))

        AllHix(1199) =
     &   MA02**2*AllHix(1063) -
     &    2*SBA*AllHix(1074)*(MA02 + 1/2.D0*Mh02)

        AllHix(1203) =
     &   CBA2*(S2B*AllHix(58) - 6*AllHix(564))*AllHix(1200) +
     &    AllHix(1201)/(MA02 - MHH2)**2 + 4*AllHix(1202)

        AllHix(1209) =
     &   -2*CBA*AllHix(65)*AllHix(1056) +
     &    4*((SBA2*AllHix(32)**2)/(MA02 - MHH2) +
     &       SBA*AllHix(1074)) +
     &    (CBA2*AllHix(1206))/(MA02 - Mh02) + 2*AllHix(1207) +
     &    MA02*AllHix(1208)

        AllHix(1303) =
     &   4*AllHix(1223) + CBA2*AllHix(1299) +
     &    2*((AllHix(183)**2*AllHix(1298))/(Mh02 - 4*MHp2) +
     &       AllHix(1302)/(MA02 - MHp2)**2)

        AllHix(1319) =
     &   (8*AllHil(4)*AllHix(1222))/(MA02 - MHp2)**2 +
     &    (2*AllHil(2)*AllHix(32)**2*AllHix(1224))/
     &     ((4*MA02 - Mh02)*(MA02 - MHp2)*(Mh02 - MHp2)) +
     &    AllHix(1318)

        AllHix(1384) =
     &   CBA2*(MHH2 - MHp2)**2*AllHix(1377) +
     &    (MHH2 - MHp2)*AllHix(1380) + SBA2*AllHix(1383)

        AllHix(1499) =
     &   -((CBA2**2*AllHix(59)**2)/
     &       ((4*Mh02 - MHH2)*(Mh02 - MHp2))) +
     &    (SBA2**2*AllHix(67)**2)/
     &     ((Mh02 - 4*MHH2)*(MHH2 - MHp2)) - CBA2*SBA2*AllHix(1498)

        AllHix(1504) =
     &   (CBA2**2*AllHix(59)**2)/((4*Mh02 - MHH2)*(Mh02 - MHp2)) +
     &    CBA2*SBA2*AllHix(1498) - AllHix(1503)/(MHH2 - MHp2)

        AllHix(1530) =
     &   (SBA2*AllHix(67)*AllHix(1528))/(MHH2 - MHp2) +
     &    3/2.D0*(AllHix(83)*AllHix(1529))

        AllHix(1533) =
     &   -(AllHix(1384)/((Mh02 - MHp2)**2*(MHH2 - MHp2)**2)) +
     &    MHH2*AllHix(1504) + (MHp2*AllHix(1532))/(MHH2 - MHp2)

        AllHix(1545) =
     &   2*CBA*AllHix(83)*AllHix(1370) + 4*SBA*AllHix(1401) -
     &    AllHix(1541)/(Mh02 - MHp2) +
     &    AllHix(1543)/(MHH2 - MHp2) -
     &    (2*AllHix(1544))/((Mh02 - MHp2)**2*(MHH2 - MHp2)**2)

        AllHix(1579) =
     &   AllHix(1570) - (4*AllHil(4)*AllHix(83)*
     &       (CBA*S2B*AllHix(1572) - 2*AllHix(1578)))/
     &     ((MHH2 - 4*MHp2)*MHp2)

        AllHix(1620) =
     &   -((4*AllHil(4)**2*AllHix(83)*
     &          (CBA*S2B*AllHix(1581) - 2*AllHix(1586)) +
     &         2*AllHil(4)*AllHix(1596))/MHp2) + AllHix(1619)

        AllHix(1740) =
     &   Mh02*AllHix(1738) -
     &    2*(2*MA02 - Mh02)*(AllHix(1734) + S2B**4*AllHix(1739))

        AllHix(1747) =
     &   4*(MA02 - 2*Mh02)*AllHix(1742) - MA02*AllHix(1746)

        AllHix(1770) =
     &   4*(MA02 - 2*MHH2)*AllHix(1742) + MA02*AllHix(1769)

        AllHix(1813) =
     &   -4*CAB**2*(2*m12sq - Mh02*S2B)**2 + 4*AllHix(1699) -
     &    SBA2*AllHix(1720) - 9*AllHix(1723) -
     &    S2B**4*AllHix(1743) + 2*(AllHix(1745) + AllHix(1807))

        AllHix(256) = ((MA0 - MHH)*(MA0 + MHH)*AllHil(2)**2*
     &       AllHix(59))/(Mh02*S2B**2) + AllHix(255)/MHH2

        AllHix(316) = (CBA2*
     &       (CBA2*AllHix(263) - 2*CBA*AllHix(264)))/MHH2 +
     &    SBA2*(AllHix(314)/Mh02 - CBA2*AllHix(315))

        AllHix(342) = (Mh02*(MA02 - MHH2)*SBA2)/
     &     (MA02 - Mh02) - AllHix(341)/S2B**2

        AllHix(347) = CBA2 +
     &    (CBA2*(MA02 - Mh02))/(MA02 - MHH2) - AllHix(346)/Mh02

        AllHix(831) = ((Mh02 - MHH2 + MHp2)*SBA2**2*
     &       AllHix(67))/Mh02 - SBA*AllHix(183)*AllHix(827) +
     &    CBA*AllHix(83)*AllHix(828) + CBA2*AllHix(830)

        AllHix(1029) =
     &   16*CBA*(MA02 - MHH2 + MHp2)*S2B**2*(2*m12sq - MHH2*S2B)*
     &     SAB - 16*(2*m12sq - MHH2*S2B)**2*SAB**2 -
     &    (8*AllHix(83)**2*AllHix(1017))/
     &     ((MA02 - MHH2)*(MHH2 - 4*MHp2)*(MA02 - MHp2)) -
     &    4*(CBA2*S2B**4*AllHix(1012) +
     &       AllHix(83)**2*AllHix(1023)) - SBA2*AllHix(1026) +
     &    2*(AllHix(65)**2*AllHix(1020) +
     &       (MHH2 + 2*MHp2)*AllHix(83)*AllHix(1027)) -
     &    (2*AllHix(1028))/(MA02 - MHp2)**2 +
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*AllHix(1014))

        AllHix(1033) =
     &   (4*AllHil(4)*AllHix(83)**2*AllHix(1021))/
     &     ((MA02 - MHH2)*(MHH2 - 4*MHp2)*(MA02 - MHp2)) +
     &    AllHix(1032)

        AllHix(1039) =
     &   (2*SBA2*AllHil(54)*AllHix(65)**2)/(MHH2 - MHp2)**2 +
     &    (AllHil(4)*AllHix(1038))/MHp2

        AllHix(1047) =
     &   (2*SBA2*AllHil(55)*AllHix(83)**2)/(MA02 - MHH2)**2 +
     &    (AllHil(1)*(2*AllHix(1040) + AllHix(1046)))/MA02

        AllHix(1174) =
     &   (-2*AllHix(1083))/((MA02 - Mh02)**2*(MA02 - MHH2)**2) -
     &    SBA2*AllHix(1101) +
     &    2*(SBA*AllHix(1074) + AllHix(1161)/(MA02 - Mh02) +
     &       (MA02*AllHix(1167))/(MA02 - MHH2) +
     &       MHH2*(AllHix(1160) - 2*AllHix(1168)) +
     &       CBA2*AllHix(1173))

        AllHix(1184) =
     &   (12*CBA*SBA2*AllHil(45)*AllHix(67)*AllHix(520))/
     &     ((MA02 - MHH2)*(Mh02 - MHH2)) +
     &    (AllHil(3)*AllHix(1183))/MHH2

        AllHix(1204) =
     &   MA02**2*AllHix(1185) +
     &    2*((2*MA02 + Mh02)*SBA*AllHix(1074) +
     &       Mh02*MHH2*(AllHix(1160) - 2*AllHix(1168)) +
     &       MA02*AllHix(1193)) + SBA2*AllHix(1198) -
     &    AllHix(1199) - 3/2.D0*
     &     (CBA*(2*MA02 + MHH2)*AllHix(65)*AllHix(1056)) -
     &    1/2.D0*(CBA2*AllHix(1191)) +
     &    1/2.D0*AllHix(1203)/(MA02 - Mh02)**2

        AllHix(1210) =
     &   4*((AllHil(3)*AllHix(1181))/(MA02 - Mh02) +
     &       (AllHil(2)*AllHix(1192))/(MA02 - MHH2)) + AllHix(1209)

        AllHix(1304) =
     &   (2*CBA2*AllHil(54)*AllHix(32)**2)/(Mh02 - MHp2)**2 +
     &    (AllHil(4)*AllHix(1303))/MHp2

        AllHix(1320) =
     &   (-2*CBA2*AllHil(55)*AllHix(183)**2)/(MA02 - Mh02)**2 +
     &    (AllHil(1)*AllHix(1319))/MA02

        AllHix(1385) =
     &   -((CBA2*MHp2*SBA*AllHil(51)*
     &         (S2B*AllHix(58) - 6*AllHix(564))*AllHix(1376))/
     &       (Mh02 - MHp2)) +
     &    (2*AllHil(52)*AllHix(1384))/(Mh02 - MHp2)**2

        AllHix(1546) =
     &   (2*CBA*SBA2*AllHil(58)*AllHix(67)*
     &       (CBA*AllHix(1507) + 2*AllHix(1509)))/
     &     ((Mh02 - MHp2)**2*(MHH2 - MHp2)) +
     &    (AllHil(4)*AllHix(1545))/MHp2

        AllHix(1621) =
     &   4*SBA2*AllHix(1552) - 4*AllHix(1563) +
     &    2*AllHil(3)*AllHix(1579) - AllHix(1620)/(MHH2 - 4*MHp2)

        AllHix(1622) =
     &   AllHil(3)*AllHix(1504) +
     &    (AllHil(4)*AllHix(1532))/(MHH2 - MHp2)

        AllHix(1748) =
     &   -AllHix(1735) + 2*
     &     (AllHil(2)*AllHix(1740) + AllHil(1)*AllHix(1747))

        AllHix(1771) =
     &   -AllHix(1762) - 2*AllHil(3)*AllHix(1768) +
     &    2*AllHil(1)*AllHix(1770)

        AllHix(1814) =
     &   4*(2*Mh02 - MHp2)*AllHix(1803) + MHp2*AllHix(1813)

	AllHix(257) = AllHil(1)**2 - AllHix(256)/(MA02 - Mh02)

        AllHix(317) = CBA2*(SBA2 - SBA2*AllHix(305)) +
     &    (2*CBA*SBA*AllHix(311))/
     &     ((MA02 - Mh02)*(MA02 - MHH2)*S2B**3) +
     &    AllHix(316)/S2B**2

        AllHix(343) = (SBA2*AllHix(339))/
     &     ((MA02 - Mh02)*S2B**2) + CBA2*(SBA2 - AllHix(342)/MHH2)

        AllHix(348) = (CBA2*SBA*AllHix(344))/
     &     ((MA02 - MHH2)*S2B**2) + SBA2*AllHix(347)

        AllHix(832) = ((MHH2 - MHp2)**2*SBA2**2)/Mh02 +
     &    (CBA2*(Mh02 - MHp2)*AllHix(826))/MHH2 +
     &    AllHix(831)/S2B**2

        AllHix(1048) =
     &   -8*(AllHil(57)*AllHix(83)*AllHix(1002) +
     &       (AllHil(39)*AllHix(65)*AllHix(1006) +
     &          2*AllHil(56)*AllHix(1007))/(MA02 - MHp2)**2 +
     &       AllHix(1011)) + AllHix(1029) +
     &    4*AllHil(3)*AllHix(1033) - 4*MHp2*AllHix(1039) +
     &    8*MA02*AllHix(1047)

        AllHix(1211) =
     &   4*(CBA*AllHil(39)*AllHix(65)*AllHix(1056) +
     &       AllHil(1)**2*AllHix(1063)) -
     &    4*((SBA2*AllHil(3)**2*AllHix(1049)*AllHix(1051))/
     &        (MA02 - MHH2)**2 + SBA*AllHix(1075) +
     &       AllHix(1084)/(MA02 - Mh02)**2 + AllHix(1103)) +
     &    (SBA2*AllHix(1157))/(MA02 - MHH2)**2 - AllHix(1204) -
     &    2*(AllHil(2)*(4*AllHil(3)*AllHix(1160) + AllHix(1174)) +
     &       MHH2*AllHix(1184) + AllHil(1)*AllHix(1210))

        AllHix(1321) =
     &   -16*AllHil(53)*AllHix(1223) +
     &    (16*AllHix(1233))/(MA02 - MHp2)**2 -
     &    AllHix(1295)/
     &     ((MA02 - Mh02)*(4*MA02 - Mh02)*(Mh02 - 4*MHp2)*
     &       (Mh02 - MHp2)) - 4*MHp2*AllHix(1304) +
     &    8*AllHil(2)*AllHix(1312) - 8*MA02*AllHix(1320)

        AllHix(1821) =
     &   (2*(MA02 + MHp2)*AllHix(1796))/(MA02 - MHp2) +
     &    SBA2*(AllHix(1771)/(MA02 - MHH2)**2 -
     &       (2*AllHil(4)*AllHix(1804) - AllHix(1810) -
     &          2*AllHil(3)*AllHix(1812))/(MHH2 - MHp2)**2) +
     &    CBA2*(AllHix(1748)/(MA02 - Mh02)**2 +
     &       (AllHix(1817) +
     &          2*(AllHil(4)*AllHix(1814) + AllHil(2)*AllHix(1820))
     &          )/(Mh02 - MHp2)**2)

        AllHix(269) = SBA2*AllHix(257) +
     &    AllHil(33)*(SBA2*AllHix(259) - AllHix(268)/MHH2)

        AllHix(318) = -(AllHix(303)/
     &       ((MA02 - Mh02)**2*(MA02 - MHH2)*(Mh02 - MHH2)*S2B**2))
     &      + 2*Mh02*AllHil(3)*AllHix(317)

        AllHix(349) = (CBA2*AllHix(325) + 2*CBA*AllHix(337))/
     &     ((MA02 - Mh02)**2*(MA02 - MHH2)**2*S2B**2) +
     &    2*(AllHil(3)*AllHix(343) + AllHil(2)*AllHix(348))

        AllHix(845) = SBA2*
     &     (AllHix(815)/(MHH2*(Mh02 - MHp2)*(MHH2 - MHp2)) -
     &       AllHil(49)*AllHix(825)) - AllHil(52)*AllHix(832) +
     &    CBA2*AllHix(844)

        AllHix(1822) =
     &   16*AllHix(1695) +
     &    4*(CBA*SBA*AllHix(1637)*AllHix(1663) +
     &       AllHix(1680)/S2B**2 +
     &       (AllHil(4)*AllHix(1708) + AllHil(1)*AllHix(1711))/
     &        (MA02 - MHp2)**2) - AllHix(1821)/S2B**4

        AllHix(1497) =
     &   AllHix(1384)/(Mh02 - MHp2)**2 -
     &    (MHp2*AllHix(1493))/((MHH2 - 4*MHp2)*(Mh02 - MHp2))

        AllHix(1535) =
     &   MHH2**2*SBA2*AllHix(1049)*AllHix(1387) -
     &    (2*AllHix(1384)*AllHix(1534))/(Mh02 - MHp2)**2

        AllHix(1629) =
     &   AllHix(1384)/(MHH2 - MHp2)**2 +
     &    CBA2*Mh02*(S2B*AllHix(58) - 6*AllHix(564))*AllHix(1402)

        AllHix(1513) =
     &   CBA2*(S2B*AllHix(58) - 6*AllHix(564))*AllHix(1364) -
     &    CBA*AllHix(83)*AllHix(1370) -
     &    (4*(2*AllHix(1494) - CBA2*AllHix(1496)))/(Mh02 - MHp2) +
     &    (2*AllHix(1497))/(MHH2 - MHp2)**2 -
     &    2*(Mh02*(2*AllHix(1499) + AllHix(1504)) +
     &       SBA2*AllHix(1512))

        AllHix(1537) =
     &   (CBA2*(2*Mh02*MHp2 + MHp2**2)*SBA*
     &       (S2B*AllHix(58) - 6*AllHix(564))*AllHix(1376))/
     &     (Mh02 - MHp2) + AllHix(1535) -
     &    (MHp2*AllHix(1536))/((MHH2 - 4*MHp2)*(Mh02 - MHp2)**2)

        AllHix(1630) =
     &   -(SBA*AllHix(1401)) + AllHix(1629)/(Mh02 - MHp2)**2

        AllHix(1514) =
     &   (4*AllHil(4)*AllHix(1493))/
     &     ((MHH2 - 4*MHp2)*(Mh02 - MHp2)*(MHH2 - MHp2)**2) -
     &    AllHix(1513)

        AllHix(1538) =
     &   -3*(Mh02 + 2*MHH2)*SBA2*AllHix(1394) +
     &    2*(CBA2*(S2B*AllHix(58) - 6*AllHix(564))*AllHix(1523) +
     &       CBA*AllHix(1530)) -
     &    4*(AllHix(1521)/(Mh02 - MHp2) +
     &       Mh02*(2*MHH2*AllHix(1499) + AllHix(1533))) -
     &    2*((3*Mh02 + 6*MHp2)*SBA*AllHix(1401) +
     &       AllHix(1527)/(MHH2 - MHp2) +
     &       AllHix(1537)/(MHH2 - MHp2)**2)

        AllHix(1631) =
     &   -(SBA2*AllHix(1394)) -
     &    2*(MHH2*(2*AllHix(1499) + AllHix(1504)) +
     &       AllHix(1627)/(Mh02 - MHp2) +
     &       (MHp2*AllHix(1628))/(MHH2 - MHp2)) +
     &    2*(CBA2*(S2B*AllHix(58) - 6*AllHix(564))*AllHix(1364) +
     &       AllHix(1630))

        AllHix(1632) =
     &   -(AllHix(1358)/((Mh02 - 4*MHp2)*(Mh02 - MHp2))) -
     &    8*CBA2*(S2B*AllHix(58) - 6*AllHix(564))*AllHix(1365) +
     &    8*CBA*AllHix(83)*AllHix(1371) -
     &    (8*AllHix(1385))/(MHH2 - MHp2)**2 +
     &    8*SBA2*AllHix(1395) + 16*SBA*AllHil(53)*AllHix(1401) -
     &    (8*AllHix(1410) + 4*AllHix(1489))/(Mh02 - MHp2)**2 +
     &    4*AllHil(3)*AllHix(1514) - AllHix(1538) +
     &    4*MHp2*AllHix(1546) +
     &    (2*AllHix(1621))/(MHH2 - MHp2)**2 +
     &    4*AllHil(2)*(4*AllHix(1622) - AllHix(1631))

        AllHix(1633) =
     &   CBA2*(-8*(AllHix(181) + AllHix(193)) -
     &       (16*SBA2*AllHil(40)*AllHix(205))/
     &        ((MA02 - Mh02)**2*(MA02 - MHH2)**2) + 16*AllHix(269))
     &      + 8*((AllHil(3)*
     &          (2*MHH2*AllHil(4)*AllHix(155) - AllHix(166)))/MHH2+
     &         (AllHil(2)*AllHix(252))/Mh02 +
     &       (AllHil(2)*AllHix(318))/Mh02) +
     &    2*(AllHix(124)/S2B**2 + (CBA*SBA*AllHix(376))/S2B**4) -
     &    (2*AllHix(427) + AllHix(484))/S2B**2 +
     &    4*((CBA2*SBA2*AllHix(15))/
     &        ((MA02 - Mh02)*(MA02 - MHH2)*(Mh02 - MHH2)*
     &          (Mh02 - MHp2)*(MHH2 - MHp2)) +
     &       (MA02 - MHp2)*AllHix(17) +
     &       (AllHil(3)*AllHix(516))/MHH2) -
     &    SBA2*(8*AllHix(530) - 8*AllHix(542)) -
     &    8*(AllHix(3) + AllHil(4)**2*AllHix(82) +
     &       (AllHil(1)*AllHix(152))/(MHH2 - MHp2) +
     &       AllHil(1)*AllHix(349) + (AllHil(3)*AllHix(568))/MHH2)-
     &      (-2*(AllHix(605) + AllHix(649)) - AllHix(715) +
     &       AllHix(773))/S2B**2 -
     &    16*(AllHix(61)/(MA02 - MHH2) +
     &       AllHix(78)/((MA02 - Mh02)*S2B**2) + CBA2*AllHix(799))-
     &      (4*AllHil(4)*AllHix(810))/MHp2 +
     &    16*(AllHix(37) + AllHix(55) + AllHil(41)*AllHix(70) +
     &       AllHix(217) + AllHix(845)) -
     &    (8*AllHil(2)*AllHix(893))/Mh02 - AllHix(911)/S2B**2 -
     &    (-(CBA2*AllHix(998)) - AllHix(1048) - 2*AllHix(1211) -
     &       AllHix(1321) - AllHix(1632))/S2B**4

        AllHindMasses =
     &   Alfa2/(MW2**2*Pi**2*SW2**2)*
     &    (1/4096.D0*AllHix(1633) + 1/2048.D0*AllHix(1822))



	end
