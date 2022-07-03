c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich


* AllHHHeqHp.F
* generated 30-May-2018 16:40

C> @brief This subroutine calculates the scalar two-loop contribution \f$\delta\rho^{(2)}_{H}\f$ for \f$m_{H^0}=m_{H^\pm}\f$.
C>
C> @ingroup group_rho
	subroutine calcAllHHHeqHp
	implicit none


#include "AllHHHeqHp.h"

	ComplexType dup1, dup10, dup11, dup12, dup2, dup3, dup4, dup5
	ComplexType dup6, dup7, dup8, dup9

	RealType T134sub

	external T134sub

	ComplexType A0FH, B0FH

	external A0FH, B0FH

#include "paraTHDM.h"
#include "paraSM.h"
	AllHhhl(1) = A0FH(MA02)

	AllHhhl(10) = B0FH(MHp2,Mh02,Mh02)

	AllHhhl(11) = B0FH(MA02,0.D0,Mh02)

	AllHhhl(12) = B0FH(MA02,0.D0,MHp2)

	AllHhhl(13) = B0FH(MA02,MA02,Mh02)

	AllHhhl(14) = B0FH(MA02,MA02,MHp2)

	AllHhhl(15) = B0FH(Mh02,0.D0,0.D0)

	AllHhhl(16) = B0FH(Mh02,0.D0,MA02)

	AllHhhl(17) = B0FH(Mh02,0.D0,MHp2)

	AllHhhl(18) = B0FH(Mh02,MA02,MA02)

	AllHhhl(19) = B0FH(Mh02,Mh02,Mh02)

	AllHhhl(2) = A0FH(Mh02)

	AllHhhl(20) = B0FH(Mh02,Mh02,MHp2)

	AllHhhl(21) = B0FH(Mh02,MHp2,MHp2)

	AllHhhl(22) = T134sub(MA02,0.D0,0.D0)

	AllHhhl(23) = T134sub(Mh02,0.D0,0.D0)

	AllHhhl(24) = T134sub(Mh02,MA02,0.D0)

	AllHhhl(25) = T134sub(Mh02,MA02,MA02)

	AllHhhl(26) = T134sub(Mh02,Mh02,MA02)

	AllHhhl(27) = T134sub(Mh02,Mh02,Mh02)

	AllHhhl(28) = T134sub(MHp2,0.D0,0.D0)

	AllHhhl(29) = T134sub(MHp2,MA02,MA02)

	AllHhhl(3) = A0FH(MHp2)

	AllHhhl(30) = T134sub(MHp2,MA02,0.D0)

	AllHhhl(31) = T134sub(MHp2,Mh02,0.D0)

	AllHhhl(32) = T134sub(MHp2,Mh02,MA02)

	AllHhhl(33) = T134sub(MHp2,Mh02,Mh02)

	AllHhhl(34) = T134sub(MHp2,MHp2,MA02)

	AllHhhl(35) = T134sub(MHp2,MHp2,Mh02)

	AllHhhl(36) = T134sub(MHp2,MHp2,MHp2)

	AllHhhl(4) = B0FH(MHp2,0.D0,MA02)

	AllHhhl(5) = B0FH(MHp2,0.D0,Mh02)

	AllHhhl(6) = B0FH(MHp2,Mh02,MHp2)

	AllHhhl(7) = B0FH(MHp2,MHp2,MHp2)

	AllHhhl(8) = B0FH(MHp2,0.D0,0.D0)

	AllHhhl(9) = B0FH(MHp2,MA02,MA02)

	AllHhhx(515) = 146*S2B - 135*(2*S2A - 3*S2B)*SBA2

	AllHhhx(525) = 455*S2B + 18*(4*S2A - 21*S2B)*SBA2

	AllHhhx(535) = 4*S2B + 9*(4*S2A - 9*S2B)*SBA2

	AllHhhx(543) = 121*S2B + 3*(9*S2A + 23*S2B)*SBA2

        AllHhhx(867) =
     &   3*S2A*(10 - 117*SBA2)*SBA2 -
     &    4*S2B*(30 - 22*SBA2 - 9*SBA2**2)

        AllHhhx(1050) =
     &   CBA*SAB - CAB*SBA + S2B*(3 - 2*SBA2) - 2*S2A*SBA2

	AllHhhx(111) = 2*S2B*(48 - 29*SBA2) + 159*S2A*SBA2

        AllHhhx(1115) =
     &   -63*Mh02*S2A*S2B*SBA +
     &    (2*MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

        AllHhhx(1116) =
     &   Mh02*(27*S2A - S2B)*S2B*SBA +
     &    (2*MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

        AllHhhx(112) =
     &   3*S2A*SBA2*(38 + 15*SBA2) +
     &    2*S2B*(48 + 28*SBA2 - 57*SBA2**2)

        AllHhhx(1123) =
     &   -4*(1 - 9*CBA2**2)*MA02**2 + 4*Mh02**2 -
     &    3*MA02*(Mh02 + 3*CBA2**2*Mh02)

        AllHhhx(1124) =
     &   -4*(1 - 9*CBA2)*MA02**2 + 4*Mh02**2 -
     &    3*MA02*(Mh02 + 3*CBA2*Mh02)

	AllHhhx(1125) = 8*MA02**2 - 3*MA02*Mh02 + Mh02**2

	AllHhhx(1132) = 3*S2A**2*S2B**2 + 2*S2B**4

        AllHhhx(1139) =
     &   Mh02*(9*S2A - 11*S2B)*S2B*SBA +
     &    (6*MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

        AllHhhx(114) =
     &   3*S2A*SBA2*(38 + 15*SBA2) +
     &    2*S2B*(48 + 20*SBA2 - 45*SBA2**2)

        AllHhhx(116) =
     &   S2A*SBA2*(38 + 15*SBA2) + 2*S2B*(16 + 4*SBA2 - 15*SBA2**2)

	AllHhhx(129) = 2*S2B*(24 - 23*SBA2) + 105*S2A*SBA2

        AllHhhx(130) =
     &   3*S2A*SBA2*(2 + 33*SBA2) +
     &    2*S2B*(24 - 20*SBA2 - 3*SBA2**2)

        AllHhhx(1316) =
     &   SBA2 - 2*(Sqrt(CW2) + SW)**2*(1 - 2*Sqrt(CW2)*SW)

        AllHhhx(132) =
     &   3*S2A*SBA2*(2 + 33*SBA2) + 2*S2B*(24 + 4*SBA2 - 3*SBA2**2)

        AllHhhx(134) =
     &   3*S2A*SBA2*(2 + 33*SBA2) +
     &    2*S2B*(24 + 28*SBA2 - 3*SBA2**2)

	AllHhhx(1436) = 2*Mh02*S2A - 2*MA02*S2B + MHp2*(S2A + S2B)

        AllHhhx(1508) =
     &   4*CAB*(2*m12sq - Mh02*S2B)*SBA +
     &    S2B**2*(CBA2*MHp2 + 2*(Mh02 - 2*MHp2)*SBA2)

        AllHhhx(322) =
     &   Lambda7*S2B**2*(16 - 23*SBA2) -
     &    16*CB2**2*Lambda6*(1 - 3*SBA2)*SBA2

	AllHhhx(333) = 9*S2A*SBA2**2 + S2B*(6 + SBA2)

	AllHhhx(450) = 4*CB2**2*Lambda6*S2B + Lambda7*S2B**3

	AllHhhx(452) = CB2**3*Lambda6 + CB2*Lambda7*S2A*S2B

	AllHhhx(476) = 3*S2A*S2B**3 - 5*S2B**4

	AllHhhx(477) = -5*Lambda7*S2B**4 + 3*S2A*AllHhhx(450)

	AllHhhx(485) = 4*S2A*S2B**3 - 61*S2B**4

	AllHhhx(486) = -61*Lambda7*S2B**4 + 4*S2A*AllHhhx(450)

        AllHhhx(487) =
     &   28*CB2**3*Lambda6 + CB2*Lambda7*(S2A*S2B - 7*S2B**2)

	AllHhhx(506) = 624*CB2**2*SB2**3 + S2B**4*SB2*(25 - 378*SBA2)

        AllHhhx(507) =
     &   Lambda6*S2B**4*(25 - 378*SBA2) -
     &    360*Lambda7*S2B**2*SB2**2*SBA2

	AllHhhx(513) = 8*Lambda7*SB2**3 + 45*Lambda6*S2A*S2B*SB2*SBA2

	AllHhhx(514) = 270*S2A*SBA2 - S2B*(146 + 405*SBA2)

        AllHhhx(516) =
     &   -36*Lambda7*(30*S2A - 13*S2B)*SB2**2*SBA2 +
     &    Lambda6*S2B**2*AllHhhx(515)

	AllHhhx(523) = 7*S2B*(65 - 54*SBA2) + 72*S2A*SBA2

	AllHhhx(524) = 7*Lambda7*SB2**3 - 2*Lambda6*S2A*S2B*SB2*SBA2

        AllHhhx(526) =
     &   72*Lambda7*(4*S2A + 11*S2B)*SB2**2*SBA2 +
     &    Lambda6*S2B**2*AllHhhx(525)

	AllHhhx(533) = -(CB2**2*S2A*S2B*SB2) + CB2**3*SB2**2

	AllHhhx(534) = S2B*(4 - 81*SBA2) + 36*S2A*SBA2

        AllHhhx(536) =
     &   36*Lambda7*(4*S2A - S2B)*SB2**2*SBA2 +
     &    Lambda6*S2B**2*AllHhhx(535)

	AllHhhx(541) = 76*Lambda7*SB2**3 - 27*Lambda6*S2A*S2B*SB2*SBA2

	AllHhhx(542) = 27*S2A*SBA2 + S2B*(121 + 69*SBA2)

        AllHhhx(544) =
     &   12*Lambda7*(9*S2A + 23*S2B)*SB2**2*SBA2 +
     &    Lambda6*S2B**2*AllHhhx(543)

	AllHhhx(573) = 4*S2A*S2B**3 - 19*S2B**4

	AllHhhx(574) = -19*Lambda7*S2B**4 + 4*S2A*AllHhhx(450)

	AllHhhx(581) = 2*S2A*S2B**3 - 41*S2B**4

	AllHhhx(582) = -41*Lambda7*S2B**4 + 2*S2A*AllHhhx(450)

	AllHhhx(590) = 3*S2A*S2B**3 - 16*S2B**4

	AllHhhx(591) = -16*Lambda7*S2B**4 + 3*S2A*AllHhhx(450)

	AllHhhx(613) = (3*S2A + S2B)*SBA2 - 3*(3*S2A - S2B)*SBA2**2

        AllHhhx(681) =
     &   4*Mh02**3 - 18*Mh02**2*MHp2 + 3*Mh02*MHp2**2 + 2*MHp2**3

	AllHhhx(820) = -4 + 42*SBA2 - 60*SBA2**2 + 81*SBA2**3

	AllHhhx(825) = -24 + 68*SBA2 - 60*SBA2**2 + 351*SBA2**3

	AllHhhx(826) = 14 + 9*SBA2 - 18*SBA2**2 + 27*SBA2**3

	AllHhhx(831) = -8 + 38*SBA2 - 44*SBA2**2 + 189*SBA2**3

	AllHhhx(832) = 14 + 45*SBA2 - 90*SBA2**2 + 135*SBA2**3

        AllHhhx(837) =
     &   -2*Lambda7*SB2*(4 - 11*SBA2 + 3*SBA2**2) +
     &    5*CB2*Lambda6*(2 - 5*SBA2 + 3*SBA2**2)

        AllHhhx(842) =
     &   9*S2A*(22 - 189*SBA2)*SBA2 -
     &    2*S2B*(364 - 227*SBA2 - 159*SBA2**2)

        AllHhhx(847) =
     &   4*S2A*(10 - 27*SBA2)*SBA2 - S2B*(2 - 19*SBA2 - 61*SBA2**2)

        AllHhhx(852) =
     &   S2A*(2 - 27*SBA2)*SBA2 - 2*S2B*(4 - 3*SBA2 - SBA2**2)

        AllHhhx(857) =
     &   3*S2A*SBA2*(2 + 9*SBA2) + S2B*(4 - 40*SBA2 + 9*SBA2**2)

        AllHhhx(860) =
     &   18*S2A*(2 - 15*SBA2)*SBA2 -
     &    S2B*(128 - 74*SBA2 - 63*SBA2**2)

        AllHhhx(861) =
     &   27*S2A*(2 - 23*SBA2)*SBA2 -
     &    2*S2B*(108 - 79*SBA2 - 33*SBA2**2)

        AllHhhx(862) =
     &   3*S2A*SBA2*(2 + 9*SBA2) - S2B*(4 + 17*SBA2 - 24*SBA2**2)

        AllHhhx(863) =
     &   3*MA02**5*SBA2 + MA02**4*MHp2*(4 - 8*SBA2 + 3*SBA2**2)

	AllHhhx(865) = 4*S2B*(30 - 31*SBA2) + 321*S2A*SBA2

        AllHhhx(866) =
     &   3*S2A*(10 - 117*SBA2)*SBA2 -
     &    4*S2B*(23 - 22*SBA2 - 9*SBA2**2)

	AllHhhx(868) = 112*CB2**2*Lambda6 + Lambda7*S2B*AllHhhx(867)

	AllHhhx(901) = -7 + SBA2 + 3*SBA2**2 + 54*SBA2**3

        AllHhhx(913) =
     &   Lambda7*S2B**2*(4 - 41*SBA2) -
     &    32*CB2**2*Lambda6*(1 - 3*SBA2)*SBA2

	AllHhhx(918) = 2*S2B*(4 - 5*SBA2) + 27*S2A*SBA2**2

	AllHhhx(923) = S2B*(46 - 117*SBA2) + 72*S2A*SBA2**2

	AllHhhx(928) = 5*S2B*(44 - 69*SBA2) + 576*S2A*SBA2**2

	AllHhhx(93) = 6*S2A + 8*S2B - 3*(3*S2A + 2*S2B)*SBA2

	AllHhhx(931) = MA02**4*MHp2*(4 - 41*SBA2) + 13*MA02**5*SBA2

	AllHhhx(932) = S2B*(38 - 51*SBA2) + 117*S2A*SBA2**2

	AllHhhx(933) = 5*S2B*(44 - 69*SBA2) + 576*S2A*SBA2**2

	AllHhhx(935) = S2B*(38 - 51*SBA2) + 117*S2A*SBA2

	AllHhhx(936) = S2B*(8 - 17*SBA2) + 39*S2A*SBA2**2

	AllHhhx(937) = 56*CB2**2*Lambda6 - Lambda7*S2B*AllHhhx(932)

	AllHhhx(95) = S2A*(2 - 3*SBA2) - 2*S2B*SBA2

	AllHhhx(97) = 6*S2A - 8*S2B - 3*(3*S2A + 2*S2B)*SBA2

	AllHhhx(980) = -2*S2B*(1 - SBA2) + S2A*SBA2

	AllHhhx(981) = 6*CAB*SBA + (S2A - S2B)*SBA2

	AllHhhx(983) = C2B*CBA*m12sq + 2*CAB*(m12sq + 2*MHp2*S2B)

	AllHhhx(985) = 4*CAB*SBA + S2B*(4 - SBA2) - 2*S2A*SBA2

	AllHhhx(1009) = Mh02*(S2A - S2B)*S2B - 2*m12sq*(3*S2A + S2B)

        AllHhhx(1021) =
     &   8*CBA*SAB - 12*CAB*SBA - 7*S2A*SBA2 - S2B*(11 + 23*SBA2)

        AllHhhx(1022) =
     &   -2*m12sq*(4*SAB + C2B*SBA) +
     &    (3*MW2*SW2)/(Alfa*Pi)*((CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHhhx(1025) =
     &   -4*m12sq*SAB + (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA)

	AllHhhx(1027) = 4*CBA*SAB + 7*S2A*SBA2 - S2B*(26 + 35*SBA2)

        AllHhhx(1028) =
     &   m12sq*(2*SAB + C2B*SBA) -
     &    (MW2*SW2)/(Alfa*Pi)*((CB2*Lambda6 - Lambda7*SB2)*SBA)

	AllHhhx(1031) = 16*CAB*SBA + 6*S2A*SBA2 - S2B*(10 + 7*SBA2)

	AllHhhx(1032) = S2B*(4 - 11*SBA2) - 6*S2A*SBA2

	AllHhhx(1034) = 2*CBA*SAB - CAB*SBA - S2A*SBA2

	AllHhhx(1041) = 10*S2B - 16*CAB*SBA - 3*(2*S2A - S2B)*SBA2

	AllHhhx(1042) = S2B*(12 - 7*SBA2) - 6*S2A*SBA2

        AllHhhx(1044) =
     &   4*CBA*SAB - 4*CAB*SBA + 3*S2A*SBA2 - S2B*(13 + 6*SBA2)

	AllHhhx(1049) = S2B*(7 + 13*SBA2) - 4*(CAB*SBA + S2A*SBA2)

        AllHhhx(1051) =
     &   -2*m12sq*(3*CAB*SBA - 4*S2B*SBA2) + CBA*AllHhhx(1025) +
     &    2*MHp2*S2B*AllHhhx(1050)

	AllHhhx(1052) = 4*CBA*SAB + S2B*(7 - 23*SBA2) - 4*S2A*SBA2

        AllHhhx(1053) =
     &   -2*m12sq*(3*CAB*SBA - 4*S2B*SBA2) + CBA*AllHhhx(1025)

	AllHhhx(106) = -96 - 40*SBA2 + 204*SBA2**2 + 63*SBA2**3

	AllHhhx(1092) = -4*(2 - 3*CBA2)*MA02 + (5 - 3*CBA2)*Mh02

	AllHhhx(1104) = 18*MA02**2 - 31*MA02*Mh02 + 7*Mh02**2

	AllHhhx(1106) = 18*S2A**2 - S2B**2

        AllHhhx(1112) =
     &   m12sq*S2A*SBA - (MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2))

        AllHhhx(1114) =
     &   (2*MA02 - Mh02)*S2B**2*SBA +
     &    (2*MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

        AllHhhx(1117) =
     &   4*MA02**2*S2B*(18*S2A + S2B)*SBA + 2*MA02*AllHhhx(1115) +
     &    Mh02*AllHhhx(1116)

        AllHhhx(1126) =
     &   m12sq**2*AllHhhx(1123) - m12sq*Mh02*S2B*AllHhhx(1124) +
     &    Mh02**2*S2B**2*AllHhhx(1125)

	AllHhhx(1128) = 9*S2A**2 + 19*S2B**2

        AllHhhx(113) =
     &   MHp2*S2B**2*AllHhhx(111) - 2*m12sq*S2B*AllHhhx(112)

        AllHhhx(1130) =
     &   Mh02*S2B**2*SBA2 -
     &    (8*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

	AllHhhx(1135) = 4*MA02**2 + 3*MA02*Mh02 - 4*Mh02**2

        AllHhhx(1137) =
     &   -(Mh02*(18*S2A - S2B)*S2B*SBA) +
     &    (4*MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

        AllHhhx(1138) =
     &   Mh02*S2B**2*SBA -
     &    (2*MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

        AllHhhx(1140) =
     &   18*CBA2*(4*MA02 - Mh02)*AllHhhx(1112) + Mh02*AllHhhx(1139)

        AllHhhx(115) =
     &   -32*CB2**2*Lambda6*(4 - 3*SBA2)*SBA2 +
     &    Lambda7*S2B*AllHhhx(114)

	AllHhhx(1153) = 2*MA02**2 - 2*MA02*Mh02 + (Mh02 - MHp2)*MHp2

        AllHhhx(117) =
     &   -32*Lambda7*SB2**2*(2 - 3*SBA2)*SBA2 +
     &    3*Lambda6*S2B*AllHhhx(116)

	AllHhhx(1206) = MHp2**2 - 2*(Mh02**2 + Mh02*MHp2)

	AllHhhx(1219) = CBA2*MHp2 + 2*(Mh02 - 2*MHp2)*SBA2

        AllHhhx(1222) =
     &   4*m12sq*(3*S2A - S2B) - (4*Mh02 + 5*MHp2)*S2A*S2B +
     &    (12*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHhhx(1225) =
     &   CBA2*MHp2*S2B**2 - 2*CBA*(2*m12sq - MHp2*S2B)*SAB

        AllHhhx(1226) =
     &   2*CAB*(2*m12sq - Mh02*S2B)*SBA +
     &    (Mh02 - 2*MHp2)*S2B**2*SBA2

	AllHhhx(1230) = Mh02**2 - 6*Mh02*MHp2 + 8*MHp2**2

	AllHhhx(125) = -48 - 8*SBA2 + 84*SBA2**2 + 45*SBA2**3

        AllHhhx(1293) =
     &   16*CW2*SW2 - (Sqrt(CW2) + SW)**2*(1 - 2*Sqrt(CW2)*SW)

        AllHhhx(131) =
     &   MHp2*S2B**2*AllHhhx(129) - 2*m12sq*S2B*AllHhhx(130)

	AllHhhx(1312) = 3*CBA2*MHp2 - (Mh02 - MHp2)*SBA2

	AllHhhx(1317) = 3*CBA2*MHp2 - (Mh02 - MHp2)*AllHhhx(1316)

	AllHhhx(1324) = 2*Mh02*(2*MA02 + Mh02) - (MA02 + 5*Mh02)*MHp2

        AllHhhx(133) =
     &   384*CB2**2*Lambda6*SBA2 + Lambda7*S2B*AllHhhx(132)

        AllHhhx(135) =
     &   192*Lambda7*SB2**2*SBA2 + Lambda6*S2B*AllHhhx(134)

	AllHhhx(1437) = -2*m12sq*(3*S2A - S2B) + S2B*AllHhhx(1436)

        AllHhhx(1728) =
     &   -2*m12sq*(9*S2A - S2B) + (2*Mh02 + MHp2)*S2A*S2B

	AllHhhx(1762) = Mh02**2 + 4*(Mh02*MHp2 + MHp2**2)

	AllHhhx(208) = 2*MA02**2 - (7*MA02 - 2*Mh02)*MHp2 + 3*MHp2**2

	AllHhhx(273) = CBA2*(2*Mh02*S2A + MHp2*S2B) - 2*CBA*MHp2*SAB

        AllHhhx(275) =
     &   Mh02*(MHp2 - 3*CBA2*MHp2) +
     &    (MA02**2 - MA02*(Mh02 + MHp2))*(1 - 3*CBA2 - 6*SBA2) -
     &    3*(Mh02**2 + MHp2**2)*SBA2

        AllHhhx(277) =
     &   MHp2*S2A*S2B - 2*(m12sq*(3*S2A - S2B) + MA02*S2B**2)

        AllHhhx(317) =
     &   4*MA02**4*MHp2*(16 - 23*SBA2) +
     &    (24*MA02**5 + MHp2**5)*SBA2

	AllHhhx(318) = S2B*(23 - 17*SBA2) + 18*S2A*SBA2**2

	AllHhhx(319) = S2B*(52 - 53*SBA2) + 36*S2A*SBA2**2

	AllHhhx(320) = 9*S2A*SBA2**2 + S2B*(12 + SBA2)

        AllHhhx(323) =
     &   -(CB2*Lambda6*S2B**2*(16 - 27*SBA2 + 12*SBA2**2)) +
     &    SB2*AllHhhx(322)

	AllHhhx(325) = S2B*(23 - 17*SBA2) + 18*S2A*SBA2

	AllHhhx(326) = 36*S2A*SBA2**2 + S2B*(22 - 35*SBA2 + 3*SBA2**2)

        AllHhhx(327) =
     &   -2*CB2**2*Lambda6*(24 + SBA2 - 3*SBA2**2) +
     &    Lambda7*S2B*AllHhhx(318)

	AllHhhx(329) = S2B*(52 - 53*SBA2) + 36*S2A*SBA2

        AllHhhx(330) =
     &   36*S2A*SBA2**2 + S2B*(28 - 57*SBA2 + 12*SBA2**2)

        AllHhhx(331) =
     &   -16*CB2**2*Lambda6*(6 + SBA2 - 3*SBA2**2) +
     &    Lambda7*S2B*AllHhhx(319)

	AllHhhx(334) = 24*CB2**2*SB2 + CB2*S2B*AllHhhx(333)

	AllHhhx(400) = 4*C2B*SBA*(1 + SBA2) - 5*SAB*SBA2*(1 + 6*SBA2)

	AllHhhx(404) = 4*C2B*SBA + SAB*SBA2*(1 + 6*SBA2)

	AllHhhx(449) = 4*S2A*S2B**3 - 7*S2B**4

	AllHhhx(451) = -7*Lambda7*S2B**4 + 4*S2A*AllHhhx(450)

	AllHhhx(453) = -(CB2*Lambda7*S2B**2) + 4*AllHhhx(452)

	AllHhhx(460) = 21*S2A*S2B**3 - 67*S2B**4

	AllHhhx(461) = -67*Lambda7*S2B**4 + 21*S2A*AllHhhx(450)

        AllHhhx(462) =
     &   88*CB2**3*Lambda6 + CB2*Lambda7*(21*S2A*S2B - 22*S2B**2)

	AllHhhx(469) = 4*CB2**3*Lambda6 - CB2*Lambda7*S2B**2

	AllHhhx(474) = (2*CAB - 71*C2B*CBA)*SBA + 162*S2A*SBA2

        AllHhhx(478) =
     &   32*CB2**3*Lambda6 + CB2*Lambda7*(21*S2A*S2B - 8*S2B**2)

        AllHhhx(479) =
     &   7*CB2*Lambda6*AllHhhx(476) - 7*SB2*AllHhhx(477) +
     &    4*SB2**2*AllHhhx(478)

	AllHhhx(483) = (11*CAB + 27*C2B*CBA)*SBA - 99*S2A*SBA2

        AllHhhx(488) =
     &   CB2*Lambda6*AllHhhx(485) - SB2*AllHhhx(486) +
     &    16*SB2**2*AllHhhx(487)

        AllHhhx(508) =
     &   -48*CB2**3*Lambda6*SB2**2*(13 + 30*SBA2) +
     &    Lambda7*AllHhhx(506) - CB2*AllHhhx(507)

        AllHhhx(517) =
     &   -48*CB2**3*Lambda6*SB2**2*(4 + 39*SBA2) +
     &    24*CB2**2*AllHhhx(513) +
     &    Lambda7*S2B**3*SB2*AllHhhx(514) + CB2*S2B*AllHhhx(516)

        AllHhhx(527) =
     &   -144*CB2**3*Lambda6*SB2**2*(7 + 22*SBA2) -
     &    Lambda7*S2B**3*SB2*AllHhhx(523) +
     &    144*CB2**2*AllHhhx(524) + CB2*S2B*AllHhhx(526)

        AllHhhx(537) =
     &   144*Lambda6*SBA2*AllHhhx(533) -
     &    Lambda7*S2B**3*SB2*AllHhhx(534) + CB2*S2B*AllHhhx(536)

        AllHhhx(545) =
     &   -16*CB2**3*Lambda6*SB2**2*(19 + 69*SBA2) +
     &    4*CB2**2*AllHhhx(541) -
     &    Lambda7*S2B**3*SB2*AllHhhx(542) + CB2*S2B*AllHhhx(544)

	AllHhhx(552) = S2A - 22*CBA*SAB - S2B*(2 - 3*SBA2)

	AllHhhx(565) = 21*S2A*S2B**3 - 74*S2B**4

	AllHhhx(566) = -74*Lambda7*S2B**4 + 21*S2A*AllHhhx(450)

	AllHhhx(571) = (98*CAB + 29*C2B*CBA)*SBA + 54*S2A*SBA2

        AllHhhx(575) =
     &   4*SB2**2*AllHhhx(453) + CB2*Lambda6*AllHhhx(573) -
     &    SB2*AllHhhx(574)

	AllHhhx(579) = 2*(41*CAB + 2*C2B*CBA)*SBA + 81*S2A*SBA2

        AllHhhx(583) =
     &   8*SB2**2*AllHhhx(487) + CB2*Lambda6*AllHhhx(581) -
     &    SB2*AllHhhx(582)

	AllHhhx(588) = (169*CAB + 23*C2B*CBA)*SBA + 135*S2A*SBA2

        AllHhhx(592) =
     &   4*SB2**2*AllHhhx(462) + 7*CB2*Lambda6*AllHhhx(590) -
     &    7*SB2*AllHhhx(591)

        AllHhhx(672) =
     &   Mh02**2*(4*Mh02 - MHp2)*S2A - (Mh02 - MHp2)**3*S2B

	AllHhhx(674) = CB2*S2B**2 - 4*CB2**2*SB2

	AllHhhx(682) = 4*m12sq*(Mh02 - MHp2)**2 + S2A*AllHhhx(681)

	AllHhhx(725) = Mh02**2 - Mh02*MHp2 + MHp2**2

	AllHhhx(753) = MA02*Mh02 - (2*Mh02 - MHp2)*MHp2

        AllHhhx(796) =
     &   MHp2*S2B*SAB + (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*(2 - 3*SBA2))

	AllHhhx(810) = -10 + 9*SBA2 - 18*SBA2**2 + 27*SBA2**3

        AllHhhx(811) =
     &   Lambda7*SB2*SBA2*(13 - 12*SBA2 - 27*SBA2**2) +
     &    2*CB2*Lambda6*(5 - 11*SBA2 + 15*SBA2**2)

	AllHhhx(814) = -4 + 6*SBA2 - 4*SBA2**2 + 27*SBA2**3

	AllHhhx(815) = -8 + 18*SBA2 - 12*SBA2**2 + 81*SBA2**3

	AllHhhx(816) = 16*CB2**2*Lambda6 - Lambda7*S2B**2*AllHhhx(815)

	AllHhhx(819) = 2 + 27*SBA2 - 54*SBA2**2 + 81*SBA2**3

        AllHhhx(821) =
     &   3*CB2*Lambda6*(2 - 5*SBA2 + 2*SBA2**2) +
     &    Lambda7*SB2*AllHhhx(820)

	AllHhhx(824) = -38 + 59*SBA2 - 42*SBA2**2 + 324*SBA2**3

        AllHhhx(827) =
     &   -(Lambda7*S2B**2*AllHhhx(825)) +
     &    4*CB2**2*Lambda6*AllHhhx(826)

	AllHhhx(830) = -38 + 69*SBA2 - 42*SBA2**2 + 432*SBA2**3

        AllHhhx(833) =
     &   -3*Lambda7*S2B**2*AllHhhx(831) +
     &    4*CB2**2*Lambda6*AllHhhx(832)

        AllHhhx(838) =
     &   -4*CB2**2*Lambda6*SB2*(2 - 3*SBA2 + 9*SBA2**2) +
     &    S2B**2*AllHhhx(837)

	AllHhhx(840) = 4*S2B*(182 - 193*SBA2) + 1503*S2A*SBA2

        AllHhhx(841) =
     &   9*S2A*(22 - 189*SBA2)*SBA2 -
     &    2*S2B*(193 - 233*SBA2 - 141*SBA2**2)

        AllHhhx(843) =
     &   24*CB2**2*Lambda6*(57 + 2*SBA2 - 6*SBA2**2) +
     &    Lambda7*S2B*AllHhhx(842)

	AllHhhx(845) = S2B*(48 - 55*SBA2) + 34*S2A*SBA2

        AllHhhx(846) =
     &   2*S2A*(10 - 27*SBA2)*SBA2 - S2B*(48 - 2*SBA2 - 53*SBA2**2)

        AllHhhx(848) =
     &   -4*CB2**2*SB2*(94 + 15*SBA2 - 45*SBA2**2) +
     &    CB2*S2B*AllHhhx(847)

	AllHhhx(850) = 8*S2B*(1 - SBA2) + 25*S2A*SBA2

        AllHhhx(851) =
     &   3*S2A*(2 - 27*SBA2)*SBA2 - 2*S2B*(11 - 9*SBA2 - 3*SBA2**2)

	AllHhhx(853) = 8*CB2**2*Lambda6 + 3*Lambda7*S2B*AllHhhx(852)

	AllHhhx(855) = S2B*(12 - 13*SBA2) + 33*S2A*SBA2

        AllHhhx(856) =
     &   3*S2A*SBA2*(2 + 9*SBA2) + S2B*(12 - 49*SBA2 + 36*SBA2**2)

        AllHhhx(858) =
     &   4*CB2**2*SB2*(8 - 9*SBA2 + 27*SBA2**2) +
     &    CB2*S2B*AllHhhx(857)

        AllHhhx(864) =
     &   -3*MHp2**5*AllHhhx(852) -
     &    4*MA02**2*MHp2**3*AllHhhx(860) +
     &    MA02*MHp2**4*AllHhhx(861) +
     &    16*MA02**3*MHp2**2*AllHhhx(862) - 64*S2B*AllHhhx(863)

        AllHhhx(869) =
     &   MHp2*S2B**2*SAB*AllHhhx(865) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (SBA*(CB2*Lambda6*S2B*AllHhhx(866) - SB2*AllHhhx(868)))

	AllHhhx(872) = -40 + 118*SBA2 - 108*SBA2**2 + 621*SBA2**3

	AllHhhx(873) = -4 + 28*SBA2 - 36*SBA2**2 + 135*SBA2**3

        AllHhhx(874) =
     &   6*MA02**4 - MA02**3*MHp2*(7 - 12*SBA2 - 27*SBA2**2)

        AllHhhx(898) =
     &   CB2*Lambda6*(8 - 85*SBA2 + 96*SBA2**2) +
     &    Lambda7*SB2*(24 + 37*SBA2 - 144*SBA2**3)

        AllHhhx(914) =
     &   -(CB2*Lambda6*S2B**2*(4 - 49*SBA2 + 24*SBA2**2)) +
     &    SB2*AllHhhx(913)

	AllHhhx(916) = 2*S2B*(4 - 5*SBA2) + 27*S2A*SBA2

	AllHhhx(917) = 2*S2B*(3 - 5*SBA2) + 27*S2A*SBA2**2

	AllHhhx(919) = 8*CB2**2*Lambda6 - Lambda7*S2B*AllHhhx(918)

	AllHhhx(921) = S2B*(46 - 117*SBA2) + 72*S2A*SBA2

        AllHhhx(922) =
     &   72*S2A*SBA2**2 - S2B*(2 + 125*SBA2 - 24*SBA2**2)

        AllHhhx(924) =
     &   -32*CB2**2*Lambda6*(6 + SBA2 - 3*SBA2**2) +
     &    Lambda7*S2B*AllHhhx(923)

	AllHhhx(926) = 5*S2B*(44 - 69*SBA2) + 576*S2A*SBA2

        AllHhhx(927) =
     &   576*S2A*SBA2**2 + S2B*(92 - 349*SBA2 + 12*SBA2**2)

        AllHhhx(929) =
     &   -16*CB2**2*Lambda6*(32 + SBA2 - 3*SBA2**2) +
     &    Lambda7*S2B*AllHhhx(928)

        AllHhhx(934) =
     &   -(MHp2**5*AllHhhx(918)) +
     &    4*MA02**3*MHp2**2*AllHhhx(923) - 8*S2B*AllHhhx(931) +
     &    2*MA02*MHp2**4*AllHhhx(932) -
     &    MA02**2*MHp2**3*AllHhhx(933)

        AllHhhx(938) =
     &   MHp2*S2B**2*SAB*AllHhhx(935) -
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (SBA*(3*CB2*Lambda6*S2B*AllHhhx(936) +
     &         SB2*AllHhhx(937)))

        AllHhhx(94) = MHp2*(3*S2A - 2*S2B)*S2B**2 +
     &    2*m12sq*S2B*AllHhhx(93)

	AllHhhx(96) = 64*CB2**2*Lambda6 - 3*Lambda7*S2B*AllHhhx(95)

	AllHhhx(963) = S2A*SBA2 + 2*S2B*(1 + 2*SBA2)

	AllHhhx(965) = 2*C2B*CBA*m12sq + CAB*(4*m12sq + 5*MHp2*S2B)

	AllHhhx(968) = 8*CAB*SBA + 5*S2A*SBA2 - S2B*(13 + 9*SBA2)

	AllHhhx(970) = 2*CAB*SBA - S2B*(4 - 3*SBA2)

	AllHhhx(975) = S2B - 12*CAB*SBA - (4*S2A - S2B)*SBA2

	AllHhhx(977) = 6*CAB*SBA - S2B*(9 - 5*SBA2)

	AllHhhx(98) = -32*Lambda7*SB2**2 + Lambda6*S2B*AllHhhx(97)

	AllHhhx(982) = MHp2**4*AllHhhx(980) - Mh02**4*AllHhhx(981)

        AllHhhx(984) =
     &   -(MHp2*(S2A*S2B - 2*S2B**2)*SBA2) -
     &    SBA*(2*AllHhhx(983) -
     &       (3*MW2*SW2)/(Alfa*Pi)*
     &        (CBA*(CB2*Lambda6 - Lambda7*SB2)))

        AllHhhx(1000) =
     &   MHp2*S2B*(6*S2A + S2B) -
     &    (12*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHhhx(1001) =
     &   MHp2*S2B*(2*S2A + 5*S2B) -
     &    (12*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHhhx(1002) =
     &   MHp2*S2B**2 + (8*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHhhx(1004) =
     &   MHp2*(2*S2A - 3*S2B)*S2B -
     &    (12*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHhhx(1005) =
     &   MHp2*(2*S2A - 13*S2B)*S2B +
     &    (12*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

	AllHhhx(1008) = 11*S2B + 24*CBA*SAB + (8*S2A - 3*S2B)*SBA2

        AllHhhx(1010) =
     &   -2*CAB*(2*m12sq - Mh02*S2B)*SBA + SBA2*AllHhhx(1009)

        AllHhhx(1011) =
     &   3*m12sq*SAB*SBA2 +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*(1 + SBA2 + 3*SBA2**2))

        AllHhhx(1013) =
     &   S2B*(33 - 17*SBA2) + 8*(5*CBA*SAB + 2*S2A*SBA2)

        AllHhhx(1014) =
     &   4*m12sq*SAB*SBA2 +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*(1 + SBA2 + 4*SBA2**2))

	AllHhhx(1016) = 33*S2B + 16*CBA*SAB + (8*S2A - 25*S2B)*SBA2

        AllHhhx(1017) =
     &   6*m12sq*SAB*SBA2 +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*(1 + SBA2 + 6*SBA2**2))

	AllHhhx(1020) = 4*S2B - 24*CAB*SBA - (14*S2A - 3*S2B)*SBA2

        AllHhhx(1023) =
     &   -24*m12sq*(CAB*SBA - S2B*SBA2) + MHp2*S2B*AllHhhx(1021) +
     &    2*CBA*AllHhhx(1022)

	AllHhhx(1024) = 8*CBA*SAB + S2B*(22 - 47*SBA2) - 14*S2A*SBA2

        AllHhhx(1026) =
     &   -6*m12sq*(CAB*SBA - 2*S2B*SBA2) + CBA*AllHhhx(1025)

        AllHhhx(1029) =
     &   MHp2*S2B*AllHhhx(1027) -
     &    4*(3*CAB*m12sq*SBA + CBA*AllHhhx(1028))

        AllHhhx(1033) =
     &   Mh02**4*AllHhhx(1031) -
     &    MHp2**3*(16*m12sq*SBA2 + MHp2*AllHhhx(1032))

	AllHhhx(1035) = S2B*(4 + 59*SBA2) - 8*AllHhhx(1034)

        AllHhhx(1036) =
     &   -12*m12sq*(CAB*SBA - S2B*SBA2) + CBA*AllHhhx(1022)

        AllHhhx(1037) =
     &   13*S2B + 4*CBA*SAB - 12*CAB*SBA - 2*(7*S2A - 2*S2B)*SBA2

	AllHhhx(1038) = 3*S2B*(4 - SBA2) + 8*(CBA*SAB + S2A*SBA2)

        AllHhhx(1039) =
     &   m12sq*(3*CAB*SBA - 2*S2B*SBA2) + CBA*AllHhhx(1028)

        AllHhhx(1043) =
     &   Mh02**3*AllHhhx(1041) +
     &    MHp2**2*(16*m12sq*SBA2 + MHp2*AllHhhx(1042))

        AllHhhx(1045) =
     &   -4*(3*CAB*m12sq*SBA + CBA*AllHhhx(1028)) +
     &    MHp2*S2B*AllHhhx(1044)

        AllHhhx(1046) =
     &   -3*(4*CAB*SBA + S2A*SBA2) + 4*(CBA*SAB + S2B*(1 + SBA2))

	AllHhhx(1048) = Mh02*(2 - 9*SBA2) + MHp2*(2 - SBA2)

        AllHhhx(1054) =
     &   Mh02**2*S2B*AllHhhx(1049) - 2*Mh02*AllHhhx(1051) +
     &    MHp2*(MHp2*S2B*AllHhhx(1052) + 2*AllHhhx(1053))

        AllHhhx(107) =
     &   -8*CB2**2*SB2*SBA2*(8 - 12*SBA2 + 9*SBA2**2) +
     &    CB2*S2B**2*AllHhhx(106)

	AllHhhx(1072) = 27*S2A**2 - 42*S2A*S2B + 7*S2B**2

	AllHhhx(1073) = 6*(2 + 9*CBA2)*S2A - (11 + 42*CBA2)*S2B

        AllHhhx(1075) =
     &   -3*Mh02*S2B*(89*S2A + 15*S2B)*SBA +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(47 - 267*CBA2)*(CB2*Lambda6 - Lambda7*SB2))

        AllHhhx(1076) =
     &   Mh02*(66*S2A - 53*S2B)*S2B*SBA +
     &    (12*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(1 + 11*CBA2)*(CB2*Lambda6 - Lambda7*SB2))

        AllHhhx(1077) =
     &   Mh02*(75*S2A - S2B)*S2B*SBA +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(1 + 75*CBA2)*(CB2*Lambda6 - Lambda7*SB2))

        AllHhhx(1080) =
     &   Mh02*S2B*SBA*AllHhhx(1073) -
     &    (12*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(1 - 4*CBA2 - 9*CBA2**2)*
     &       (CB2*Lambda6 - Lambda7*SB2))

	AllHhhx(1089) = 2*MA02*S2B - Mh02*(3*S2A + S2B)

        AllHhhx(1093) =
     &   -(Mh02*(2*MA02 + Mh02)*S2B) + m12sq*AllHhhx(1092)

        AllHhhx(1094) =
     &   16*MA02**2*S2B - 6*MA02*Mh02*(2*S2A + 3*S2B) +
     &    Mh02**2*(3*S2A + 5*S2B)

	AllHhhx(1095) = 4*(2 - 3*CBA2)*MA02 - (5 - 3*CBA2)*Mh02

        AllHhhx(1097) =
     &   -4*(1 - 6*CBA2)*MA02**2 + 2*(1 - 9*CBA2)*MA02*Mh02 -
     &    (1 - 3*CBA2)*Mh02**2

	AllHhhx(1098) = 10*MA02**2 - 8*MA02*Mh02 + Mh02**2

        AllHhhx(1099) =
     &   4*MA02**2*S2B + Mh02**2*(3*S2A + S2B) -
     &    2*MA02*Mh02*(6*S2A + S2B)

        AllHhhx(1100) =
     &   -4*(1 - 6*CBA2)*MA02**2 + 2*(1 - 9*CBA2)*MA02*Mh02 -
     &    (1 - 3*CBA2)*Mh02**2

	AllHhhx(1102) = 8*MA02**2 - 14*MA02*Mh02 + 3*Mh02**2

	AllHhhx(1103) = CBA2**2*m12sq**2 - CBA2*m12sq*Mh02*S2B

        AllHhhx(1105) =
     &   (2*MA02 + Mh02)*(m12sq**2 - m12sq*Mh02*S2B) +
     &    Mh02*S2B**2*AllHhhx(1104)

        AllHhhx(1113) =
     &   CBA2*Mh02*S2B*AllHhhx(1112) +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*CBA2**2*m12sq*(CB2*Lambda6 - Lambda7*SB2))

        AllHhhx(1118) =
     &   -2*m12sq*(2*MA02 + Mh02)*AllHhhx(1114) +
     &    Mh02*S2B*AllHhhx(1117)

	AllHhhx(1120) = -2*(1 - 2*CBA2)*MA02 + (1 - CBA2)*Mh02

        AllHhhx(1121) =
     &   (4*MA02 - Mh02)*Mh02*S2A - (2*MA02 - Mh02)**2*S2B

	AllHhhx(1127) = -(MA02**4*S2B**4*SBA2) + CAB**2*AllHhhx(1126)

        AllHhhx(1136) =
     &   -(AllHhhx(1114)*AllHhhx(1135)) +
     &    (18*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*CBA2**2*MA02*(4*MA02 - Mh02)*
     &       (CB2*Lambda6 - Lambda7*SB2))

        AllHhhx(1141) =
     &   8*MA02**3*S2B**2*SBA + 2*MA02**2*AllHhhx(1137) +
     &    4*Mh02**2*AllHhhx(1138) + MA02*AllHhhx(1140)

        AllHhhx(1154) =
     &   (2*MA02 - Mh02 - MHp2)*(Mh02 - MHp2)*S2A -
     &    2*S2B*AllHhhx(1153)

	AllHhhx(1173) = CBA2*(Mh02 + MHp2) - MHp2*(3 + SBA2)

        AllHhhx(118) =
     &   SAB*AllHhhx(113) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (SBA*(SB2*AllHhhx(115) - CB2*AllHhhx(117)))

	AllHhhx(1193) = 2*MHp2*(S2A - S2B) + Mh02*(S2A + S2B)

	AllHhhx(1195) = MW2/(Alfa*Pi)*(SW2 + SW2*(1 + 3*SBA2))

        AllHhhx(1207) =
     &   2*m12sq*(Mh02 - MHp2)*(3*S2A - S2B) +
     &    S2A*S2B*AllHhhx(1206)

        AllHhhx(1213) =
     &   (5*Mh02 + 4*MHp2)*S2A*S2B - 4*m12sq*(3*S2A + S2B)

        AllHhhx(1214) =
     &   -(CAB*(2*CBA2*m12sq - Mh02*S2B)) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*(CBA2 - 2*SBA2))

        AllHhhx(1220) =
     &   4*CAB*(2*m12sq - Mh02*S2B)*SBA + S2B**2*AllHhhx(1219)

        AllHhhx(1221) =
     &   (2*m12sq - MHp2*S2B)*SAB +
     &    (3*MW2*SW2)/(Alfa*Pi)*((CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHhhx(1227) =
     &   Mh02*AllHhhx(1225) + (Mh02 - MHp2)*AllHhhx(1226) -
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(2*Mh02 - MHp2)*(CB2*Lambda6 - Lambda7*SB2)*SBA)

	AllHhhx(1233) = Mh02*(S2A - S2B) + 2*MHp2*(S2A + S2B)

        AllHhhx(126) =
     &   24*CB2**2*SB2*SBA2*(8 - 12*SBA2 + 9*SBA2**2) +
     &    CB2*S2B**2*AllHhhx(125)

        AllHhhx(1265) =
     &   -2*MHp2*S2A*SB2 +
     &    (3*MW2*SW2)/(Alfa*Pi)*(CBA*Lambda6*S2B*SBA)

	AllHhhx(1269) = 2*Mh02**2 - 3*Mh02*MHp2 + MHp2**2

        AllHhhx(1325) =
     &   -2*(2*MA02 - Mh02)*(MA02 - MHp2)*S2B + S2A*AllHhhx(1324)

        AllHhhx(136) =
     &   SAB*AllHhhx(131) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (SBA*(SB2*AllHhhx(133) - CB2*AllHhhx(135)))

        AllHhhx(1438) =
     &   CBA*AllHhhx(1437) +
     &    2*((2*m12sq - MHp2*S2B)*SAB +
     &       (MW2*SW2)/(Alfa*Pi)*
     &        ((1 - 3*CBA2)*(CB2*Lambda6 - Lambda7*SB2)*SBA))

	AllHhhx(160) = 3*S2B - 6*CBA*SAB - 2*CAB*SBA

        AllHhhx(163) =
     &   4*MA02**2 - 3*(MA02*Mh02 + Mh02**2) +
     &    2*(MA02 + 2*Mh02)*MHp2

	AllHhhx(1657) = MHp2*S2B*(CBA*S2A - 2*SAB) + 4*m12sq*SAB*SBA2

	AllHhhx(166) = CAB*MA02 - (CAB + C2B*CBA)*Mh02

	AllHhhx(1660) = CBA*(2*MA02 - MHp2)*S2B + MHp2*SAB

        AllHhhx(1661) =
     &   CBA*(2*MA02 - MHp2)*S2B**2 - 2*(2*m12sq - MHp2*S2B)*SAB

        AllHhhx(1667) =
     &   CBA*(Mh02 - MHp2)*S2B**2 - 2*(2*m12sq - MHp2*S2B)*SAB

        AllHhhx(1729) =
     &   -((2*MA02 - 3*Mh02 + 4*MHp2)*S2B**2) +
     &    3*CBA2*AllHhhx(1728)

	AllHhhx(1731) = 2*m12sq - (Mh02 + 2*MHp2)*S2A

        AllHhhx(176) =
     &   -2*(4*CAB + C2B*CBA)*m12sq +
     &    (5*MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

        AllHhhx(1763) =
     &   4*m12sq**2 - 4*m12sq*(Mh02 + 2*MHp2)*S2A +
     &    S2A**2*AllHhhx(1762)

	AllHhhx(1779) = 7*Mh02**2 - 2*Mh02*MHp2 + 4*MHp2**2

        AllHhhx(179) =
     &   -2*(2*CAB + C2B*CBA)*m12sq +
     &    (3*MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

	AllHhhx(1792) = (Mh02 - MHp2)**2 + MHp2**2

	AllHhhx(201) = 9*Mh02**2 - 3*Mh02*MHp2 + MHp2**2

        AllHhhx(206) =
     &   MA02**2 - Mh02*MHp2 + 2*MHp2**2 - MA02*(Mh02 + MHp2)

        AllHhhx(207) =
     &   -8*CB2*(MA02 - Mh02)*MHp2*SB2**2 + S2B**2*SB2*AllHhhx(206)

        AllHhhx(209) =
     &   8*CB2**2*(MA02 - Mh02)*MHp2*SB2 + CB2*S2B**2*AllHhhx(208)

        AllHhhx(210) =
     &   8*CB2*(MA02 - Mh02)*MHp2*SB2**2 + S2B**2*SB2*AllHhhx(208)

	AllHhhx(212) = MA02 - Mh02 - MHp2*(2 + SBA2)

	AllHhhx(222) = 14*CAB*SBA + S2A*SBA2 + S2B*(9 + 29*SBA2)

        AllHhhx(223) =
     &   -2*C2B*m12sq + (7*MW2*SW2)/(Alfa*Pi)*
     &     (CB2*Lambda6 - Lambda7*SB2)

	AllHhhx(225) = 4*CAB*SBA + S2B*(19 - 13*SBA2) - S2A*SBA2

        AllHhhx(226) =
     &   -2*C2B*m12sq + (5*MW2*SW2)/(Alfa*Pi)*
     &     (CB2*Lambda6 - Lambda7*SB2)

	AllHhhx(229) = 4*CAB*SBA - 2*S2A*SBA2 - S2B*(2 + 3*SBA2)

	AllHhhx(231) = 2*C2B*CBA*m12sq + CAB*(4*m12sq - MHp2*S2B)

	AllHhhx(233) = 2*S2B + 8*CAB*SBA + (2*S2A - 3*S2B)*SBA2

        AllHhhx(234) =
     &   -2*C2B*m12sq + (3*MW2*SW2)/(Alfa*Pi)*
     &     (CB2*Lambda6 - Lambda7*SB2)

	AllHhhx(237) = 4*CAB*SBA - 3*S2B*(2 - SBA2)

	AllHhhx(238) = S2B*(2 - SBA2) - S2A*SBA2

	AllHhhx(240) = 24*CAB*SBA + S2B*(13 - SBA2) + S2A*SBA2

	AllHhhx(242) = 6*CAB*SBA - S2B*(2 - 3*SBA2)

	AllHhhx(244) = 8*CAB*SBA + S2B*(5 - 21*SBA2) - S2A*SBA2

        AllHhhx(245) =
     &   C2B*m12sq - (2*MW2*SW2)/(Alfa*Pi)*
     &     (CB2*Lambda6 - Lambda7*SB2)

	AllHhhx(248) = S2B*(14 - SBA2) - 2*S2A*SBA2

	AllHhhx(249) = 14*CAB*SBA + (2*S2A + 5*S2B)*SBA2

	AllHhhx(252) = 10*CAB*SBA + S2B*(2 - 9*SBA2) - 2*S2A*SBA2

	AllHhhx(254) = 16*S2B + 12*CAB*SBA + (2*S2A + 5*S2B)*SBA2

	AllHhhx(301) = -2 + 3*SBA2 - 6*SBA2**2 + 9*SBA2**3

        AllHhhx(302) =
     &   CB2*S2B**2*(28 - 29*SBA2 + 24*SBA2**2) +
     &    16*CB2**2*SB2*AllHhhx(301)

        AllHhhx(305) =
     &   2*CB2*Lambda6*(2 - 3*SBA2 + 3*SBA2**2) -
     &    Lambda7*SB2*(2 - 3*SBA2 + 9*SBA2**3)

        AllHhhx(308) =
     &   2*CB2*Lambda6*(5 - SBA2 + 3*SBA2**2) -
     &    Lambda7*SB2*(8 + SBA2 + 9*SBA2**3)

        AllHhhx(321) =
     &   -2*S2B*AllHhhx(317) - 4*MA02**2*MHp2**3*AllHhhx(318) +
     &    4*MA02**3*MHp2**2*AllHhhx(319) +
     &    MA02*MHp2**4*AllHhhx(320)

        AllHhhx(324) =
     &   MHp2*S2B**3*SAB*(16 - 23*SBA2) +
     &    (MW2*SW2)/(Alfa*Pi)*(SBA*AllHhhx(323))

        AllHhhx(328) =
     &   -2*MHp2*S2B**2*SAB*AllHhhx(325) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (SBA*(CB2*Lambda6*S2B*AllHhhx(326) -
     &         2*SB2*AllHhhx(327)))

        AllHhhx(332) =
     &   MHp2*S2B**2*SAB*AllHhhx(329) -
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (SBA*(CB2*Lambda6*S2B*AllHhhx(330) - SB2*AllHhhx(331)))

        AllHhhx(335) =
     &   MHp2*S2B**2*SAB*(9*S2A*SBA2 + S2B*(12 + SBA2)) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (SBA*(Lambda7*S2B*SB2*AllHhhx(320) -
     &         Lambda6*AllHhhx(334)))

        AllHhhx(391) =
     &   16*CBA*SAB + 3*S2B*(45 - 11*SBA2) - 8*S2A*(1 - 6*SBA2)

	AllHhhx(392) = S2B - 2*CBA*SAB - 6*CAB*SBA + 3*S2B*SBA2

	AllHhhx(396) = C2B*SBA*(2 + SBA2) - SAB*SBA2*(1 + 6*SBA2)

        AllHhhx(398) =
     &   -54 + 16*CW2**2 + 21*SBA2 + 17*SBA2**2 +
     &    32*(2 - CW2)*SW2 - 48*SW2**2

        AllHhhx(401) =
     &   -2*m12sq*AllHhhx(400) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*
     &       (4 + 9*SBA2 + 30*SBA2**2))

        AllHhhx(402) =
     &   -70 + 8*CW2**2 + 57*SBA2 + 5*SBA2**2 + 16*(2 - CW2)*SW2 -
     &    24*SW2**2

        AllHhhx(405) =
     &   -2*m12sq*AllHhhx(404) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*(4 - SBA2 - 6*SBA2**2))

        AllHhhx(406) =
     &   -184 + 48*CW2**2 + 97*SBA2 + 39*SBA2**2 +
     &    96*(2 - CW2)*SW2 - 144*SW2**2

	AllHhhx(416) = S2A - 2*(S2B + CBA*SAB) + 3*S2B*SBA2

	AllHhhx(419) = S2A - 6*CBA*SAB + CAB*SBA - S2B*(1 - SBA2)

        AllHhhx(433) =
     &   8*m12sq*(12*S2A - 5*S2B) - 3*MHp2*(2*S2A*S2B - 15*S2B**2)

        AllHhhx(434) =
     &   -4*(2*m12sq - MHp2*S2B)*SAB +
     &    (3*MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*(5 - 16*SBA2))

        AllHhhx(437) =
     &   CBA*SAB - (2*CAB + C2B*CBA)*SBA - (3*S2A + S2B)*SBA2

	AllHhhx(447) = (2*CAB - 3*C2B*CBA)*SBA + 9*S2A*SBA2

        AllHhhx(454) =
     &   CB2*Lambda6*AllHhhx(449) - SB2*AllHhhx(451) +
     &    4*SB2**2*AllHhhx(453)

	AllHhhx(458) = -((8*CAB + 35*C2B*CBA)*SBA) + 90*S2A*SBA2

        AllHhhx(463) =
     &   CB2*Lambda6*AllHhhx(460) - SB2*AllHhhx(461) +
     &    4*SB2**2*AllHhhx(462)

	AllHhhx(467) = -2*(CAB + 2*C2B*CBA)*SBA + 27*S2A*SBA2

        AllHhhx(470) =
     &   -13*S2B**4*(CB2*Lambda6 - Lambda7*SB2) +
     &    34*SB2**2*AllHhhx(469)

	AllHhhx(473) = 503*S2B - 90*CAB*SBA - (61*S2A + 145*S2B)*SBA2

	AllHhhx(475) = -2*S2B**5*SBA2 + S2B**4*AllHhhx(474)

        AllHhhx(480) =
     &   128*CB2**3*Lambda6*SB2**2 - 128*CB2**2*Lambda7*SB2**3 -
     &    77*S2B**4*(CB2*Lambda6 - Lambda7*SB2) +
     &    12*SBA2*AllHhhx(479)

	AllHhhx(484) = 19*S2B**5*SBA2 + S2B**4*AllHhhx(483)

        AllHhhx(489) =
     &   336*CB2**3*Lambda6*SB2**2 - 336*CB2**2*Lambda7*SB2**3 -
     &    40*S2B**4*(CB2*Lambda6 - Lambda7*SB2) +
     &    3*SBA2*AllHhhx(488)

	AllHhhx(494) = 2*MHp2*S2A + (MA02 + 17*MHp2)*S2B

        AllHhhx(498) =
     &   MHp2*(S2A - 12*CBA*SAB) -
     &    Mh02*(S2A + 2*CAB*SBA - S2B*SBA2)

        AllHhhx(500) =
     &   4*CB2*Lambda6 - 4*Lambda7*SB2 +
     &    (CB2*Lambda6 - Lambda7*SB2)*SBA2

	AllHhhx(504) = 3*CAB*SBA + (9*S2A - 2*S2B)*SBA2

	AllHhhx(505) = 8*CBA*SAB - 26*S2A*SBA2 + S2B*(9 + 4*SBA2)

        AllHhhx(509) =
     &   -32*m12sq*S2B**4*SAB +
     &    (MW2*SW2)/(Alfa*Pi)*(SBA*AllHhhx(508))

	AllHhhx(512) = 21*CAB*SBA - (72*S2A + 5*S2B)*SBA2

        AllHhhx(518) =
     &   4*m12sq*S2B**4*(25*SAB - 21*C2B*SBA) +
     &    (MW2*SW2)/(Alfa*Pi)*(SBA*AllHhhx(517))

	AllHhhx(520) = 100*CBA*SAB + 64*CAB*SBA + 11*S2A*SBA2

	AllHhhx(522) = 69*CAB*SBA + (72*S2A - 5*S2B)*SBA2

        AllHhhx(528) =
     &   8*m12sq*S2B**4*(25*SAB - 4*C2B*SBA) +
     &    (MW2*SW2)/(Alfa*Pi)*(SBA*AllHhhx(527))

	AllHhhx(530) = 8*CBA*SAB + 218*CAB*SBA + 79*S2A*SBA2

	AllHhhx(532) = 3*CAB*SBA + (9*S2A + 2*S2B)*SBA2

        AllHhhx(538) =
     &   -8*m12sq*S2B**4*(SAB - C2B*SBA) +
     &    (MW2*SW2)/(Alfa*Pi)*(SBA*AllHhhx(537))

        AllHhhx(546) =
     &   28*m12sq*S2B**4*(2*SAB - C2B*SBA) +
     &    (MW2*SW2)/(Alfa*Pi)*(SBA*AllHhhx(545))

	AllHhhx(553) = MA02*S2B**2*(1 - SBA2) + MHp2*S2B*AllHhhx(552)

        AllHhhx(559) =
     &   -29*S2B**4*(CB2*Lambda6 - Lambda7*SB2) +
     &    68*SB2**2*AllHhhx(469)

	AllHhhx(563) = (32*CAB + 7*C2B*CBA)*SBA + 22*S2A*SBA2

        AllHhhx(567) =
     &   4*SB2**2*AllHhhx(478) + CB2*Lambda6*AllHhhx(565) -
     &    SB2*AllHhhx(566)

	AllHhhx(572) = 14*S2B**5*SBA2 + S2B**4*AllHhhx(571)

        AllHhhx(576) =
     &   127*S2B**4*(CB2*Lambda6 - Lambda7*SB2) +
     &    6*SBA2*AllHhhx(575)

	AllHhhx(580) = S2B**5*SBA2 + S2B**4*AllHhhx(579)

        AllHhhx(584) =
     &   336*CB2**3*Lambda6*SB2**2 - 336*CB2**2*Lambda7*SB2**3 +
     &    65*S2B**4*(CB2*Lambda6 - Lambda7*SB2) +
     &    6*SBA2*AllHhhx(583)

	AllHhhx(589) = 15*S2B**5*SBA2 + S2B**4*AllHhhx(588)

        AllHhhx(593) =
     &   400*CB2**3*Lambda6*SB2**2 - 400*CB2**2*Lambda7*SB2**3 +
     &    359*S2B**4*(CB2*Lambda6 - Lambda7*SB2) +
     &    6*SBA2*AllHhhx(592)

        AllHhhx(595) =
     &   222*CAB*SBA + 2*S2B*(72 - 47*SBA2) + 77*S2A*SBA2

        AllHhhx(610) =
     &   3*S2A*(1 - 3*SBA2)*SBA2 - S2B*(6 - SBA2 - 3*SBA2**2)

	AllHhhx(611) = MHp2**2 + MA02**2*(1 + SBA2 - 3*SBA2**2)

	AllHhhx(623) = CBA*S2B*(2*m12sq + MA02*S2B) - 4*m12sq*SAB

        AllHhhx(624) =
     &   CB2*Lambda6*MHp2*(CBA*S2B - 2*SAB) +
     &    CBA*Lambda7*(MA02 + MHp2)*S2B*SB2

        AllHhhx(627) =
     &   8*m12sq*SAB*SB2 + (MW2*SW2)/(Alfa*Pi)*(Lambda6*S2B**2*SBA)

	AllHhhx(647) = Mh02**2*S2A + MHp2**2*(3*S2A + S2B)

	AllHhhx(657) = Mh02**2 - 5*Mh02*MHp2 + 2*MHp2**2

        AllHhhx(664) =
     &   2*CBA*SAB - 4*CAB*SBA + S2B*(3 - SBA2) - S2A*SBA2

	AllHhhx(665) = CBA*SAB - (3*S2A + S2B)*SBA2

        AllHhhx(673) =
     &   4*CB2**3*Lambda6*(Mh02 - MHp2)**3 +
     &    CB2*Lambda7*S2B*AllHhhx(672)

	AllHhhx(675) = Lambda7*S2B**2*SB2 - Lambda6*AllHhhx(674)

	AllHhhx(705) = 2*MA02**2 - (6*MA02 - Mh02)*MHp2

        AllHhhx(726) =
     &   Mh02**2*MHp2**2 - MA02*Mh02*MHp2*(Mh02 + MHp2) +
     &    MA02**2*AllHhhx(725)

	AllHhhx(728) = MHp2*(3*S2A - S2B) - Mh02*(3*S2A + S2B)

        AllHhhx(737) =
     &   2*Mh02**3 - 3*Mh02**2*MHp2 + Mh02*MHp2**2 - 2*MHp2**3

	AllHhhx(741) = 3*(Mh02 - MHp2)*S2A + (Mh02 + MHp2)*S2B

        AllHhhx(742) =
     &   (Mh02 + MHp2)*S2A*S2B -
     &    (6*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHhhx(747) =
     &   -2*MHp2**3*SBA2 - Mh02**2*MHp2*(3 + SBA2) +
     &    Mh02**3*(1 + 2*SBA2) + Mh02*MHp2**2*(4 + 3*SBA2)

        AllHhhx(748) =
     &   MHp2**2*(1 - SBA2) + 4*Mh02**2*SBA2 +
     &    Mh02*MHp2*(5 + 3*SBA2)

	AllHhhx(749) = MHp2*(3 + SBA2) + Mh02*(3 + 5*SBA2)

        AllHhhx(754) =
     &   2*(MA02 - Mh02)**2*MHp2*SBA2 + (MA02 - MHp2)*AllHhhx(753)

        AllHhhx(757) =
     &   MA02*Mh02*(3*Mh02 + MHp2) - MA02**2*(5*Mh02 + MHp2)

        AllHhhx(795) =
     &   MHp2*S2B*SAB*SBA2 +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*(1 + SBA2 - 3*SBA2**2))

        AllHhhx(797) =
     &   -2*m12sq*(MA02 - MHp2)*SAB*(1 - 3*SBA2) +
     &    MHp2*AllHhhx(796)

        AllHhhx(812) =
     &   4*CB2**2*Lambda6*SB2*AllHhhx(810) + S2B**2*AllHhhx(811)

        AllHhhx(817) =
     &   3*CB2*Lambda6*S2B**2*AllHhhx(814) + SB2*AllHhhx(816)

        AllHhhx(822) =
     &   -4*CB2**2*Lambda6*SB2*AllHhhx(819) + S2B**2*AllHhhx(821)

        AllHhhx(828) =
     &   CB2*Lambda6*S2B**2*AllHhhx(824) + SB2*AllHhhx(827)

        AllHhhx(834) =
     &   CB2*Lambda6*S2B**2*AllHhhx(830) + SB2*AllHhhx(833)

        AllHhhx(839) =
     &   -8*MHp2*S2B**3*SAB*(1 - 2*SBA2) +
     &    (MW2*SW2)/(Alfa*Pi)*(SBA*AllHhhx(838))

        AllHhhx(844) =
     &   MHp2*S2B**2*SAB*AllHhhx(840) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (SBA*(CB2*Lambda6*S2B*AllHhhx(841) - SB2*AllHhhx(843)))

        AllHhhx(849) =
     &   -2*MHp2*S2B**2*SAB*AllHhhx(845) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (SBA*(2*Lambda7*S2B*SB2*AllHhhx(846) -
     &         Lambda6*AllHhhx(848)))

        AllHhhx(854) =
     &   3*MHp2*S2B**2*SAB*AllHhhx(850) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (SBA*(CB2*Lambda6*S2B*AllHhhx(851) - SB2*AllHhhx(853)))

        AllHhhx(859) =
     &   MHp2*S2B**2*SAB*AllHhhx(855) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (SBA*(Lambda7*S2B*SB2*AllHhhx(856) -
     &         Lambda6*AllHhhx(858)))

        AllHhhx(870) =
     &   -(m12sq*(MA02 - MHp2)*S2B*SAB*AllHhhx(864)) +
     &    MA02*MHp2**5*AllHhhx(869)

        AllHhhx(875) =
     &   MHp2**4*AllHhhx(815) - MA02*MHp2**3*AllHhhx(872) +
     &    8*MA02**2*MHp2**2*AllHhhx(873) + 16*SBA2*AllHhhx(874)

	AllHhhx(892) = -44 - 53*SBA2 + 96*SBA2**2 + 432*SBA2**3

        AllHhhx(893) =
     &   Lambda7*S2B**2*(76 + 5*SBA2 - 576*SBA2**3) +
     &    64*CB2**2*Lambda6*AllHhhx(301)

        AllHhhx(899) =
     &   64*CB2**2*Lambda6*SB2*AllHhhx(301) + S2B**2*AllHhhx(898)

        AllHhhx(915) =
     &   MHp2*S2B**3*SAB*(4 - 41*SBA2) +
     &    (MW2*SW2)/(Alfa*Pi)*(SBA*AllHhhx(914))

        AllHhhx(920) =
     &   MHp2*S2B**2*SAB*AllHhhx(916) -
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (SBA*(CB2*Lambda6*S2B*AllHhhx(917) + SB2*AllHhhx(919)))

        AllHhhx(925) =
     &   MHp2*S2B**2*SAB*AllHhhx(921) -
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (SBA*(CB2*Lambda6*S2B*AllHhhx(922) - SB2*AllHhhx(924)))

        AllHhhx(930) =
     &   MHp2*S2B**2*SAB*AllHhhx(926) -
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (SBA*(CB2*Lambda6*S2B*AllHhhx(927) - SB2*AllHhhx(929)))

        AllHhhx(939) =
     &   -(m12sq*S2B*SAB*AllHhhx(934)) + MA02*MHp2**4*AllHhhx(938)

	AllHhhx(948) = MA02**3 - 3*MA02**2*Mh02 + 3*MA02*Mh02**2

	AllHhhx(950) = MA02 + (Mh02 - MHp2)*(2 + SBA2)

        AllHhhx(951) =
     &   MHp2*S2B*SAB*SBA2 -
     &    (MW2*SW2)/(Alfa*Pi)*((CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHhhx(952) =
     &   -3*MHp2*S2B*SAB*SBA2 +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*(2 + SBA2))

	AllHhhx(955) = 2*CAB*SBA - S2B*(4 + 5*SBA2)

	AllHhhx(956) = 2*CAB*SBA - S2B*(5 + 12*SBA2)

	AllHhhx(958) = 2*CAB*SBA + (S2A - 3*S2B)*SBA2

	AllHhhx(959) = 4*CAB*SBA - 3*S2A*SBA2 - 7*S2B*(2 + SBA2)

	AllHhhx(960) = 2*CAB*SBA - S2B*(13 - 5*SBA2) - 3*S2A*SBA2

	AllHhhx(961) = S2A*SBA2 + S2B*(11 + 29*SBA2)

        AllHhhx(964) =
     &   Mh02**5*(6*CAB*SBA + S2A*SBA2) - MHp2**5*AllHhhx(963)

        AllHhhx(966) =
     &   -(MHp2*S2B**2*SBA2) -
     &    SBA*(2*AllHhhx(965) -
     &       (6*MW2*SW2)/(Alfa*Pi)*
     &        (CBA*(CB2*Lambda6 - Lambda7*SB2)))

	AllHhhx(967) = 5*S2A*SBA2 - 2*S2B*(1 + 5*SBA2)

        AllHhhx(969) =
     &   2*SBA*(12*CAB*m12sq - CBA*AllHhhx(223)) +
     &    MHp2*S2B*AllHhhx(968)

        AllHhhx(971) =
     &   SBA*(8*CAB*m12sq - CBA*AllHhhx(226)) +
     &    MHp2*S2B*AllHhhx(970)

	AllHhhx(973) = 6*CAB*SBA - S2B*(15 - 14*SBA2) - 4*S2A*SBA2

	AllHhhx(974) = 2*S2A*SBA2 + S2B*(8 + 17*SBA2)

        AllHhhx(976) =
     &   -2*SBA*(8*CAB*m12sq - CBA*AllHhhx(226)) +
     &    MHp2*S2B*AllHhhx(975)

        AllHhhx(978) =
     &   4*SBA*(3*CAB*m12sq + CBA*AllHhhx(245)) +
     &    MHp2*S2B*AllHhhx(977)

        AllHhhx(99) = SAB*AllHhhx(94) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (SBA*(SB2*AllHhhx(96) + CB2*AllHhhx(98)))

        AllHhhx(993) =
     &   MHp2*S2B*(2*S2A + S2B) -
     &    (12*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHhhx(994) =
     &   MHp2*S2B*(3*S2A + 14*S2B) -
     &    (6*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHhhx(995) =
     &   MHp2*S2B*(S2A + S2B) +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHhhx(997) =
     &   MHp2*S2B*(S2A + 3*S2B) -
     &    (6*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHhhx(998) =
     &   MHp2*S2B*(S2A + 3*S2B) +
     &    (6*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHhhx(100) =
     &   CBA*AllHhhx(99) +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (S2B*SAB*(CB2*Lambda6 - Lambda7*SB2)*SBA*(2 + 3*SBA2))

        AllHhhx(1003) =
     &   Mh02**3*S2B*(8*S2A + 11*S2B) - Mh02*MHp2*AllHhhx(1000) -
     &    MHp2**2*AllHhhx(1001) - 3*Mh02**2*AllHhhx(1002)

        AllHhhx(1006) =
     &   Mh02**3*(4*S2A - 3*S2B)*S2B - 7*MHp2**3*S2B**2 -
     &    Mh02*MHp2*AllHhhx(1004) - Mh02**2*AllHhhx(1005)

        AllHhhx(1012) =
     &   MHp2*S2B*SBA2*AllHhhx(1008) +
     &    4*(SBA2*AllHhhx(1010) - 2*CBA*AllHhhx(1011))

        AllHhhx(1015) =
     &   MHp2*S2B*SBA2*AllHhhx(1013) +
     &    4*(2*SBA2*AllHhhx(1010) - 3*CBA*AllHhhx(1014))

        AllHhhx(1018) =
     &   MHp2*S2B*SBA2*AllHhhx(1016) +
     &    4*(SBA2*AllHhhx(1010) - CBA*AllHhhx(1017))

        AllHhhx(1030) =
     &   Mh02**3*S2B*AllHhhx(1020) - 2*Mh02**2*AllHhhx(1023) +
     &    MHp2**2*(MHp2*S2B*AllHhhx(1024) + 4*AllHhhx(1026)) +
     &    2*Mh02*MHp2*AllHhhx(1029)

        AllHhhx(1040) =
     &   S2B*AllHhhx(1033) +
     &    Mh02*MHp2**2*(MHp2*S2B*AllHhhx(1035) -
     &       4*AllHhhx(1036)) +
     &    2*Mh02**2*MHp2*(2*AllHhhx(1026) +
     &       MHp2*S2B*AllHhhx(1037)) +
     &    Mh02**3*(MHp2*S2B*AllHhhx(1038) - 8*AllHhhx(1039))

        AllHhhx(1047) =
     &   S2B*AllHhhx(1043) +
     &    2*(Mh02*MHp2*AllHhhx(1045) +
     &       Mh02**2*(4*AllHhhx(1039) - MHp2*S2B*AllHhhx(1046)))

	AllHhhx(105) = -96 - 56*SBA2 + 228*SBA2**2 + 45*SBA2**3

        AllHhhx(1055) =
     &   MA02**4*S2B**2*AllHhhx(1048) + MA02**3*AllHhhx(1054)

        AllHhhx(1061) =
     &   12*(1 - 4*CBA2 - 9*CBA2**2)*MA02**2 +
     &    (17 + 60*CBA2 - 297*CBA2**2)*MA02*Mh02 +
     &    (7 - 12*CBA2 + 81*CBA2**2)*Mh02**2

        AllHhhx(1062) =
     &   12*(1 + 11*CBA2)*MA02**2 - (47 - 267*CBA2)*MA02*Mh02 -
     &    (1 + 75*CBA2)*Mh02**2

	AllHhhx(1063) = 36*MA02**2 + 55*MA02*Mh02 - 19*Mh02**2

	AllHhhx(1065) = 297*S2A**2 + 84*S2A*S2B + 11*S2B**2

	AllHhhx(1066) = (10 - 99*CBA2)*S2A - (1 + 14*CBA2)*S2B

	AllHhhx(1068) = 81*S2A**2 + 12*S2A*S2B + 7*S2B**2

	AllHhhx(1069) = -3*(2 - 27*CBA2)*S2A - (7 - 6*CBA2)*S2B

        AllHhhx(1071) =
     &   Mh02*(24*S2A - 5*S2B)*S2B*SBA2 -
     &    (24*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(1 - 2*CBA2)*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHhhx(1078) =
     &   24*MA02**3*S2B**2*SBA - MA02*Mh02*AllHhhx(1075) +
     &    2*MA02**2*AllHhhx(1076) - Mh02**2*AllHhhx(1077)

        AllHhhx(108) =
     &   -(Lambda7*S2B**2*SB2*AllHhhx(105)) + Lambda6*AllHhhx(107)

	AllHhhx(1085) = 4*CBA2*MA02 - (1 + CBA2)*Mh02

        AllHhhx(1086) =
     &   -((2*MA02 - Mh02)*Mh02*S2B) + m12sq*AllHhhx(1085)

	AllHhhx(1087) = (4*MA02 - Mh02)*S2A - (2*MA02 - Mh02)*S2B

        AllHhhx(1090) =
     &   -4*CAB*(m12sq - 3*CBA2*m12sq + Mh02*S2B) +
     &    S2B*SBA*AllHhhx(1089) +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(1 - 3*CBA2)*(CB2*Lambda6 - Lambda7*SB2))

        AllHhhx(1096) =
     &   4*CAB*AllHhhx(1093) + S2B*SBA*AllHhhx(1094) +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*AllHhhx(1095))

        AllHhhx(1101) =
     &   -4*CAB*(m12sq*AllHhhx(1097) - Mh02*S2B*AllHhhx(1098)) -
     &    (2*MA02 - Mh02)*S2B*SBA*AllHhhx(1099) +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*AllHhhx(1100))

        AllHhhx(1122) =
     &   4*CAB*(MA02*Mh02*S2B - m12sq*AllHhhx(1120)) +
     &    S2B*SBA*AllHhhx(1121) +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*AllHhhx(1120))

	AllHhhx(1155) = 6*(MA02 - MHp2)*MHp2*SAB + CBA*AllHhhx(1154)

	AllHhhx(1159) = (Mh02 + 2*MHp2)*S2A + (2*MA02 - Mh02)*S2B

	AllHhhx(1170) = 4*MHp2 - Mh02*(1 + CBA2 - SBA2)

	AllHhhx(1172) = Mh02**2 - (2 + CBA2)*Mh02*MHp2 + 2*MHp2**2

	AllHhhx(1174) = 2*MHp2**2 + Mh02**2*SBA2 + Mh02*AllHhhx(1173)

        AllHhhx(119) =
     &   MHp2**2*S2B**2*SAB**2*(96 - 217*SBA2) +
     &    CBA*MHp2*AllHhhx(118)

	AllHhhx(1194) = -2*m12sq*(3*S2A + S2B) + S2B*AllHhhx(1193)

        AllHhhx(1196) =
     &   4*m12sq*SAB - MHp2*S2B*(CBA*S2B + 2*SAB) +
     &    2*(CB2*Lambda6 - Lambda7*SB2)*SBA*AllHhhx(1195)

	AllHhhx(1200) = 2*MHp2*(3*S2A - S2B) + Mh02*(3*S2A + S2B)

        AllHhhx(1208) =
     &   -6*CAB*MHp2*(2*CBA2*m12sq - Mh02*S2B) -
     &    SBA*AllHhhx(1207) +
     &    (6*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*(MHp2 - Mh02*SBA2))

	AllHhhx(1234) = -2*m12sq*(3*S2A + S2B) + S2B*AllHhhx(1233)

	AllHhhx(124) = -48 + 40*SBA2 + 12*SBA2**2 + 99*SBA2**3

	AllHhhx(1248) = 3*CBA2 + SBA2 - (Mh02*SBA2)/MHp2

	AllHhhx(1259) = 6*Mh02**2 + 17*Mh02*MHp2 - 5*MHp2**2

	AllHhhx(1261) = 6*Mh02**2 - 7*Mh02*MHp2 + MHp2**2

	AllHhhx(1263) = 3*S2A - S2B - 12*CAB*SBA

        AllHhhx(1264) =
     &   -2*MHp2*S2A*SB2 +
     &    (9*MW2*SW2)/(Alfa*Pi)*(CBA*Lambda6*S2B*SBA)

	AllHhhx(1266) = 4*m12sq*(3*S2A - S2B)*SB2 + S2B*AllHhhx(1265)

        AllHhhx(127) =
     &   -(Lambda7*S2B**2*SB2*AllHhhx(124)) + Lambda6*AllHhhx(126)

        AllHhhx(1326) =
     &   2*m12sq*(MA02 - Mh02)*(3*S2A - S2B) + S2B*AllHhhx(1325)

        AllHhhx(1327) =
     &   -2*(1 - 3*CBA2)*(MA02 - MHp2) + 3*(MA02 - Mh02)*SBA2

	AllHhhx(1332) = MA02 + ((MA02 - Mh02)*Mh02)/(4*Mh02 - MHp2)

	AllHhhx(1333) = 6*MA02*Mh02 - 2*(MA02 + Mh02)*MHp2 + MHp2**2

        AllHhhx(1340) =
     &   2*(MA02 + MHp2)*SB2*SBA +
     &    (MW2*SW2)/(Alfa*Pi)*(CBA*Lambda6)

        AllHhhx(1342) =
     &   2*CBA*(MA02 + MHp2)*SB2*SBA +
     &    (MW2*SW2)/(Alfa*Pi)*(CBA2*Lambda6)

	AllHhhx(1366) = MA02**2 - Mh02*(5*MA02 - MHp2)

        AllHhhx(137) =
     &   MHp2**2*S2B**2*SAB**2*(48 - 151*SBA2) +
     &    CBA*MHp2*AllHhhx(136)

	AllHhhx(1373) = MA02*(Mh02 - 5*MHp2) + MHp2**2

        AllHhhx(1375) =
     &   CBA2/(MA02 - Mh02)**2 + (2*SBA2)/(MA02 - MHp2)**2

        AllHhhx(1383) =
     &   (2*MA02)/(MA02 - MHp2) +
     &    (CBA2*(2*MA02 - Mh02))/(Mh02 - MHp2)

	AllHhhx(1384) = 2/(MA02 - MHp2)**2 + CBA2/(Mh02 - MHp2)**2

	AllHhhx(1420) = -1 + (CBA2*(MA02 - MHp2))/(Mh02 - MHp2) + SBA2

	AllHhhx(159) = 2*S2A + 3*S2B - 6*CBA*SAB - 2*CAB*SBA

	AllHhhx(1592) = 3*CBA2*MHp2*S2A + (2*MA02 + MHp2)*S2B

	AllHhhx(1593) = 9*CBA2*MHp2*S2A - 2*(MA02 + 2*MHp2)*S2B

        AllHhhx(1595) =
     &   S2A*(2*CBA2**2 + 3*CBA2*SBA2) + SBA2*(S2B + S2A*SBA2)

	AllHhhx(1598) = 9*(Mh02 + MHp2)*S2A + 5*(Mh02 - MHp2)*S2B

	AllHhhx(1600) = 2*MA02**2 - MA02*(Mh02 + MHp2)

	AllHhhx(1601) = 9*S2A**2 - S2B**2

        AllHhhx(1602) =
     &   -2*(Mh02 - MHp2)**2*S2A**2 + (Mh02 - 4*MHp2)*MHp2*S2B**2

        AllHhhx(161) =
     &   2*MHp2*(S2A + S2B + 4*CBA*SAB) + MA02*AllHhhx(160)

	AllHhhx(1615) = MA02 + 3*CBA2*Mh02 + (2 + 3*CBA2)*MHp2

	AllHhhx(1625) = 4*m12sq*MHp2 - 5*MHp2**2*S2B

	AllHhhx(164) = 4*(MA02 - Mh02)*Mh02 + SBA2*AllHhhx(163)

	AllHhhx(1646) = 2*m12sq*SAB - MHp2*S2B*(CBA*S2B + SAB)

	AllHhhx(1647) = 4*m12sq*SAB - MHp2*S2B*(CBA*S2B + 2*SAB)

        AllHhhx(165) =
     &   8*CAB*Mh02*SBA - (3*MA02 - 2*Mh02 - MHp2)*S2A*SBA2

        AllHhhx(1651) =
     &   -2*m12sq*(3*S2A - S2B) + (2*Mh02 + MHp2)*S2A*S2B

        AllHhhx(1653) =
     &   (Mh02 + 2*MHp2)*S2A*S2B - 2*m12sq*(3*S2A + S2B)

	AllHhhx(1655) = 9*S2A**2*S2B**2 + 2*S2B**4

	AllHhhx(1656) = MHp2*S2B*(CBA*S2A - SAB) + 2*m12sq*SAB*SBA2

        AllHhhx(1662) =
     &   (2*m12sq - MHp2*S2B)*SAB*
     &     (2*m12sq*SAB - S2B*AllHhhx(1660)) -
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*AllHhhx(1661))

	AllHhhx(1666) = CBA2*MHp2**2 + (Mh02 - 2*MHp2)**2*SBA2

        AllHhhx(1668) =
     &   2*CAB*CBA*(2*m12sq - Mh02*S2B) + SBA*AllHhhx(1667)

        AllHhhx(167) =
     &   -2*(CAB*MA02*Mh02*S2B + m12sq*AllHhhx(166)) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(MA02 - 2*Mh02)*(CB2*Lambda6 - Lambda7*SB2))

	AllHhhx(1680) = (MA02 - Mh02)**2 + (2*MA02 - MHp2)**2

        AllHhhx(1682) =
     &   CBA*(2*MA02 - MHp2)*S2B**2 - (2*m12sq - MHp2*S2B)*SAB

	AllHhhx(1685) = (MA02 - Mh02)**2 + 2*(Mh02 - MHp2)**2

        AllHhhx(1687) =
     &   2*CAB*(2*m12sq - Mh02*S2B) - (2*MA02 - Mh02)*S2B**2*SBA

	AllHhhx(1690) = CB2**2*Lambda6**2 + Lambda7**2*SB2**2

        AllHhhx(1691) =
     &   Mh02*S2A*S2B*SBA +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*CBA2*(CB2*Lambda6 - Lambda7*SB2))

        AllHhhx(1692) =
     &   CAB**2*(2*CBA2*m12sq - Mh02*S2B)**2 +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (CBA*CBA2*Mh02*S2A*S2B*(CB2*Lambda6 - Lambda7*SB2)*SBA)

	AllHhhx(1717) = MHp2**2 + 4*(Mh02**2 + Mh02*MHp2)

	AllHhhx(1720) = (3*CBA2 + 5*CBA2**2)*Mh02 + CBA2**2*MHp2

	AllHhhx(1722) = Mh02**2 - Mh02*(4*MA02 + MHp2)

        AllHhhx(173) =
     &   MHp2*S2B*(S2A + S2B + 10*CBA*SAB)*SBA2 -
     &    (12*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHhhx(1730) =
     &   CBA*Lambda7*SB2*AllHhhx(1729) -
     &    CB2*Lambda6*(18*CBA2*MHp2*S2B*SAB + CBA*AllHhhx(1729))

        AllHhhx(1732) =
     &   CBA*Lambda7*SB2*AllHhhx(1653) +
     &    CB2*Lambda6*(12*CBA2*m12sq*SAB + CBA*S2B*AllHhhx(1731))

        AllHhhx(1733) =
     &   CBA2*Lambda7*MHp2*S2B*SAB*SB2*SBA2 +
     &    CB2*CBA*Lambda6*m12sq*S2A*SBA2**2

	AllHhhx(1740) = (2*MA02 - 3*Mh02)*MHp2 + 4*MHp2**2

        AllHhhx(1741) =
     &   9*MHp2*S2A*S2B - (2*MA02 - 3*Mh02 + MHp2)*S2B**2

        AllHhhx(1744) =
     &   -2*(CBA2 + 9*CBA2**3)*m12sq + (CBA2 + 9*CBA2**2)*Mh02*S2B

	AllHhhx(1747) = 4*m12sq**2 - 4*m12sq*Mh02*S2B + Mh02**2*S2B**2

        AllHhhx(1748) =
     &   2*m12sq*(2*MA02 - Mh02)*S2B**2 -
     &    (2*MA02*Mh02 - Mh02**2)*S2B**3

	AllHhhx(1749) = 4*MA02**2 - 4*MA02*Mh02 + Mh02**2

	AllHhhx(175) = 16*CAB*SBA + S2B*(2 - 11*SBA2) - 2*S2A*SBA2

	AllHhhx(1754) = 2*CBA2**3*m12sq - CBA2**2*Mh02*S2B

        AllHhhx(1757) =
     &   4*(m12sq**2 + m12sq*(2*Mh02 + MHp2)*S2A) +
     &    S2A**2*AllHhhx(1717)

        AllHhhx(1761) =
     &   2*m12sq*(Mh02 - 2*MHp2)*S2B**2 -
     &    (Mh02**2 - 2*Mh02*MHp2)*S2B**3

	AllHhhx(1764) = Mh02**2 - 4*Mh02*MHp2 + 4*MHp2**2

        AllHhhx(1765) =
     &   -12*m12sq*(Mh02 + 2*MHp2)*S2A**2*S2B +
     &    12*m12sq**2*(3*S2A**2 + 2*S2A*S2B) +
     &    S2B**2*AllHhhx(1763) + 2*S2B**4*AllHhhx(1764)

        AllHhhx(177) =
     &   MHp2*(S2A*S2B + 7*S2B**2)*SBA2 - 2*SBA*AllHhhx(176)

	AllHhhx(1770) = -MA02**2 + 2*MA02*MHp2 - MHp2**2

	AllHhhx(1772) = Mh02**2 + (MA02 - Mh02)**2*Re(AllHhhl(16))

	AllHhhx(1773) = -Mh02**2 + 2*Mh02*MHp2 - MHp2**2

	AllHhhx(178) = 2*S2A*SBA2 + S2B*(2 + 3*SBA2)

        AllHhhx(1780) =
     &   10*m12sq**2 + 2*m12sq*(4*Mh02 - 7*MHp2)*S2A +
     &    S2A**2*AllHhhx(1779)

	AllHhhx(1781) = MA02*Mh02 - Mh02**2 - 2*(MA02**2 + MHp2**2)

	AllHhhx(1783) = 2*m12sq*MHp2*S2B**2 - MHp2**2*S2B**3

	AllHhhx(1786) = CBA*MHp2*S2B**2 - 2*(2*m12sq - MHp2*S2B)*SAB

        AllHhhx(1793) =
     &   (MA02 - MHp2)**2 + (Mh02 - 2*MHp2)**2*SBA2 +
     &    CBA2*AllHhhx(1792)

        AllHhhx(1795) =
     &   2*CBA2*AllHhhx(1685) +
     &    3*Mh02**2*SBA2*(1 + 2*Re(AllHhhl(15)))

        AllHhhx(180) =
     &   MHp2*(S2A*S2B + 2*S2B**2)*SBA2 - 2*SBA*AllHhhx(179)

        AllHhhx(1808) =
     &   (MA02 - MHp2)**2*Re(AllHhhl(4)) +
     &    CBA2*(Mh02 - MHp2)**2*Re(AllHhhl(5))

	AllHhhx(181) = 16*CAB*SBA + S2B*(10 + 3*SBA2)

	AllHhhx(1810) = 3*CBA2*MHp2**2 + 2*(MA02 - MHp2)**2*SBA2

        AllHhhx(1813) =
     &   2*(MA02 - MHp2)**2*SBA2*Re(AllHhhl(4)) +
     &    3*CBA2*MHp2**2*Re(AllHhhl(8))

	AllHhhx(199) = MA02**4 - MA02**3*(Mh02 - 2*MHp2)

        AllHhhx(200) =
     &   Mh02**4 + 5*Mh02**3*MHp2 + 3*Mh02**2*MHp2**2 -
     &    3*Mh02*MHp2**3 + MHp2**4

	AllHhhx(202) = 2*Mh02**4 + MA02**2*AllHhhx(201)

        AllHhhx(204) =
     &   (3*MA02**2 - MA02*Mh02)*MHp2 - (8*MA02 - Mh02)*MHp2**2 +
     &    5*MHp2**3

        AllHhhx(211) =
     &   8*CB2**2*Lambda6*(MA02 - Mh02)*MHp2*SB2 +
     &    Lambda7*AllHhhx(207) -
     &    SBA2*(Lambda6*AllHhhx(209) - Lambda7*AllHhhx(210))

        AllHhhx(213) =
     &   -2*C2B*m12sq*(MA02 - MHp2)*AllHhhx(212) +
     &    (MW2*SW2)/(Alfa*Pi)*(CB2*Lambda6*AllHhhx(206))

	AllHhhx(216) = 2*CAB*SBA - S2B*(2 + 13*SBA2)

	AllHhhx(218) = CAB*SBA - S2B*(7 - 8*SBA2)

	AllHhhx(220) = 3*S2B - 10*CAB*SBA - S2A*SBA2

	AllHhhx(221) = S2A*SBA2 - S2B*(23 + 2*SBA2)

        AllHhhx(224) =
     &   MHp2*S2B*AllHhhx(222) +
     &    2*SBA*(12*CAB*m12sq - CBA*AllHhhx(223))

        AllHhhx(227) =
     &   MHp2*S2B*AllHhhx(225) +
     &    2*SBA*(8*CAB*m12sq - CBA*AllHhhx(226))

        AllHhhx(23) = -8*(S2B + CBA*SAB) + 21*S2B*SBA2 +
     &    4*S2A*(1 + SBA2)

        AllHhhx(230) =
     &   MHp2**4*(S2A + 2*S2B)*SBA2 +
     &    Mh02**4*(6*CAB*SBA + S2A*SBA2) +
     &    Mh02*MHp2**3*AllHhhx(229)

        AllHhhx(232) =
     &   MHp2*(S2A*S2B - 2*S2B**2)*SBA2 +
     &    SBA*(AllHhhx(231) -
     &       (3*MW2*SW2)/(Alfa*Pi)*
     &        (CBA*(CB2*Lambda6 - Lambda7*SB2)))

        AllHhhx(235) =
     &   MHp2*S2B*AllHhhx(233) +
     &    2*SBA*(4*CAB*m12sq - CBA*AllHhhx(234))

        AllHhhx(239) =
     &   Mh02**5*(6*CAB*SBA + (S2A + S2B)*SBA2) +
     &    Mh02*MHp2**4*AllHhhx(237) - MHp2**5*AllHhhx(238)

        AllHhhx(241) =
     &   2*SBA*(8*CAB*m12sq - CBA*AllHhhx(226)) +
     &    MHp2*S2B*AllHhhx(240)

        AllHhhx(243) =
     &   -2*SBA*(4*CAB*m12sq - CBA*AllHhhx(234)) +
     &    MHp2*S2B*AllHhhx(242)

        AllHhhx(246) =
     &   MHp2*S2B*AllHhhx(244) +
     &    8*SBA*(3*CAB*m12sq + CBA*AllHhhx(245))

	AllHhhx(250) = MHp2**4*AllHhhx(248) - Mh02**4*AllHhhx(249)

        AllHhhx(251) =
     &   12*MHp2*S2B**2*SBA2 +
     &    SBA*(-2*(C2B*CBA*m12sq - 3*CAB*MHp2*S2B) +
     &       (MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2)))

        AllHhhx(253) =
     &   2*SBA*(12*CAB*m12sq - CBA*AllHhhx(223)) +
     &    MHp2*S2B*AllHhhx(252)

        AllHhhx(255) =
     &   8*SBA*(3*CAB*m12sq + CBA*AllHhhx(245)) +
     &    MHp2*S2B*AllHhhx(254)

	AllHhhx(259) = (6 - CBA2)*MA02 + (4 + CBA2)*Mh02

	AllHhhx(26) = S2A + 10*CAB*SBA - (S2A + 5*S2B)*SBA2

	AllHhhx(263) = CBA*(Mh02 - MHp2)*S2B + 2*MHp2*SAB

        AllHhhx(266) =
     &   2*MA02**2 + Mh02**2 + MHp2**2 - 2*MA02*(Mh02 + MHp2)

	AllHhhx(27) = 2*(S2B + CBA*SAB) - S2A*(1 - SBA2) + 3*S2B*SBA2

	AllHhhx(288) = 3*MHp2*S2A + (2*MA02 - MHp2)*S2B

        AllHhhx(303) =
     &   Lambda7*S2B**2*SB2*(20 - 17*SBA2 + 36*SBA2**3) -
     &    Lambda6*AllHhhx(302)

        AllHhhx(306) =
     &   4*CB2**2*Lambda6*SB2*AllHhhx(301) + S2B**2*AllHhhx(305)

        AllHhhx(309) =
     &   4*CB2**2*Lambda6*SB2*AllHhhx(301) + S2B**2*AllHhhx(308)

	AllHhhx(35) = 4*CAB*SBA - S2B*(1 + CBA2 - 2*SBA2) + 3*S2A*SBA2

	AllHhhx(355) = 4*S2A - 8*CBA*SAB - S2B*(13 - 21*SBA2)

	AllHhhx(356) = S2B - 2*(CBA*SAB + CAB*SBA) + S2B*SBA2

	AllHhhx(361) = C2B*SBA*(9 - 4*SBA2) + 8*SAB*SBA2

	AllHhhx(364) = SAB*SBA2 - C2B*SBA*(2 + SBA2)

	AllHhhx(367) = 10*SAB*SBA2 - C2B*SBA*(14 + 9*SBA2)

	AllHhhx(38) = S2B + CBA2*S2B + 4*CAB*SBA + 3*(S2A + S2B)*SBA2

	AllHhhx(386) = 28*S2A*SBA2 + 11*S2B*(2 + 3*SBA2)

        AllHhhx(387) =
     &   20*CBA*SAB - 2*S2A*(5 - 58*SBA2) + S2B*(155 - 53*SBA2)

        AllHhhx(388) =
     &   30*CBA*SAB + 58*CAB*SBA - 2*S2A*(5 - 4*SBA2) -
     &    S2B*(15 + 29*SBA2)

        AllHhhx(389) =
     &   6*CBA*SAB - 22*CAB*SBA - S2B*(3 - 11*SBA2) -
     &    2*S2A*(1 - 5*SBA2)

        AllHhhx(390) =
     &   8*CBA*SAB - 4*S2A*(1 - 41*SBA2) + S2B*(150 + 109*SBA2)

        AllHhhx(393) =
     &   MHp2*AllHhhx(391) + 4*Mh02*(2*S2A + 3*AllHhhx(392))

	AllHhhx(395) = 40*CBA*SAB*SBA2 - S2B*(20 - 21*SBA2 + SBA2**2)

        AllHhhx(397) =
     &   m12sq*AllHhhx(396) -
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*(1 + SBA2 + 3*SBA2**2))

        AllHhhx(403) =
     &   2*CBA*AllHhhx(401) -
     &    MHp2*S2B*(86*CBA*SAB*SBA2 + S2B*AllHhhx(402))

        AllHhhx(407) =
     &   16*CBA*AllHhhx(405) +
     &    MHp2*S2B*(48*CBA*SAB*SBA2 + S2B*AllHhhx(406))

	AllHhhx(412) = 2*Mh02*S2A + MA02*S2B + MHp2*(S2A + 2*S2B)

	AllHhhx(413) = Mh02*(3*S2A - S2B) - 6*CAB*MHp2*SBA

	AllHhhx(417) = MA02*S2B*(1 - SBA2) + MHp2*AllHhhx(416)

	AllHhhx(420) = MA02*S2B*(1 - SBA2) + 2*MHp2*AllHhhx(419)

	AllHhhx(423) = S2B*(2 - SBA2) + 3*(2*CAB*SBA + S2A*SBA2)

	AllHhhx(425) = 21*CBA*SAB + 60*CAB*SBA + 2*S2B*(2 - 33*SBA2)

        AllHhhx(426) =
     &   21*CBA*SAB + (9*CAB - 4*C2B*CBA)*SBA - 12*S2B*SBA2

        AllHhhx(428) =
     &   42*(S2B + CBA*SAB + 6*CAB*SBA) + 17*(S2A - 4*S2B)*SBA2

        AllHhhx(429) =
     &   21*CBA*SAB - (14*CAB + 17*C2B*CBA)*SBA -
     &    2*(12*S2A + 5*S2B)*SBA2

	AllHhhx(431) = CAB*SBA + (3*S2A - S2B)*SBA2

        AllHhhx(435) =
     &   -4*CAB*(19*m12sq - 4*MHp2*S2B)*SBA - SBA2*AllHhhx(433) +
     &    2*CBA*AllHhhx(434)

        AllHhhx(436) =
     &   8*CBA*SAB + 118*CAB*SBA + S2B*(42 - 17*SBA2) + 27*S2A*SBA2

        AllHhhx(438) =
     &   m12sq*AllHhhx(437) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA*(2 - 3*SBA2))

        AllHhhx(442) =
     &   MA02*S2B**2*(1 - SBA2) + 2*MHp2*S2B*AllHhhx(419)

	AllHhhx(448) = S2B**5*SBA2 + S2B**4*AllHhhx(447)

        AllHhhx(455) =
     &   S2B**4*(CB2*Lambda6 - Lambda7*SB2) - 3*SBA2*AllHhhx(454)

	AllHhhx(459) = -10*S2B**5*SBA2 + S2B**4*AllHhhx(458)

        AllHhhx(464) =
     &   800*CB2**3*Lambda6*SB2**2 - 800*CB2**2*Lambda7*SB2**3 -
     &    179*S2B**4*(CB2*Lambda6 - Lambda7*SB2) +
     &    12*SBA2*AllHhhx(463)

	AllHhhx(468) = -7*S2B**5*SBA2 + S2B**4*AllHhhx(467)

        AllHhhx(471) =
     &   400*CB2**3*Lambda6*SB2**2 - 400*CB2**2*Lambda7*SB2**3 -
     &    31*S2B**4*(CB2*Lambda6 - Lambda7*SB2) +
     &    12*SBA2*AllHhhx(470)

        AllHhhx(481) =
     &   MHp2*S2B**5*AllHhhx(473) + 4*m12sq*AllHhhx(475) -
     &    (2*MW2*SW2)/(Alfa*Pi)*(CBA*SBA*AllHhhx(480))

	AllHhhx(482) = 161*S2B + 8*CAB*SBA - (179*S2A + 211*S2B)*SBA2

        AllHhhx(490) =
     &   m12sq*AllHhhx(484) +
     &    (MW2*SW2)/(Alfa*Pi)*(CBA*SBA*AllHhhx(489))

        AllHhhx(495) =
     &   MHp2**2*(2*S2A - 9*S2B) - Mh02**2*(4*S2A - 7*S2B) +
     &    Mh02*AllHhhx(494)

	AllHhhx(496) = 3*S2A - S2B + 6*CAB*SBA

	AllHhhx(499) = (MA02 - 2*MHp2)*S2B**2 + S2B*AllHhhx(498)

        AllHhhx(501) =
     &   -2*C2B*m12sq*(4 + SBA2) + (MW2*SW2)/(Alfa*Pi)*AllHhhx(500)

	AllHhhx(503) = 16*CAB*SBA - 24*S2A*SBA2 - S2B*(46 + 7*SBA2)

        AllHhhx(510) =
     &   32*m12sq*S2B**4*AllHhhx(504) +
     &    2*MHp2*S2B**5*AllHhhx(505) + CBA*AllHhhx(509)

        AllHhhx(511) =
     &   138*S2B - 400*CBA*SAB - 3120*CAB*SBA -
     &    (2012*S2A + 227*S2B)*SBA2

	AllHhhx(519) = -4*m12sq*S2B**4*AllHhhx(512) + CBA*AllHhhx(518)

	AllHhhx(521) = 3*S2B*(87 - 8*SBA2) + 2*AllHhhx(520)

	AllHhhx(529) = -8*m12sq*S2B**4*AllHhhx(522) + CBA*AllHhhx(528)

	AllHhhx(531) = -3*S2B*(274 + 73*SBA2) + 8*AllHhhx(530)

	AllHhhx(539) = -8*m12sq*S2B**4*AllHhhx(532) + CBA*AllHhhx(538)

        AllHhhx(540) =
     &   475*S2B + 336*CBA*SAB + 848*CAB*SBA +
     &    (768*S2A - 317*S2B)*SBA2

        AllHhhx(547) =
     &   -8*m12sq*S2B**4*(15*CAB*SBA + S2B*SBA2) + CBA*AllHhhx(546)

        AllHhhx(551) =
     &   SBA2*AllHhhx(442) +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA*(4 + SBA2))

        AllHhhx(554) =
     &   SBA2*AllHhhx(553) +
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA*(4 + SBA2))

	AllHhhx(558) = S2B**5*SBA2 - 3*S2B**4*(2*CAB*SBA + 3*S2A*SBA2)

        AllHhhx(560) =
     &   400*CB2**3*Lambda6*SB2**2 - 400*CB2**2*Lambda7*SB2**3 -
     &    S2B**4*(CB2*Lambda6 - Lambda7*SB2) + 6*SBA2*AllHhhx(559)

	AllHhhx(564) = 38*S2B**5*SBA2 + 9*S2B**4*AllHhhx(563)

        AllHhhx(568) =
     &   64*CB2**3*Lambda6*SB2**2 - 64*CB2**2*Lambda7*SB2**3 +
     &    347*S2B**4*(CB2*Lambda6 - Lambda7*SB2) +
     &    6*SBA2*AllHhhx(567)

        AllHhhx(570) =
     &   470*CAB*SBA + 7*S2B*(65 - 47*SBA2) + 217*S2A*SBA2

        AllHhhx(577) =
     &   -2*m12sq*AllHhhx(572) +
     &    (MW2*SW2)/(Alfa*Pi)*(CBA*SBA*AllHhhx(576))

	AllHhhx(578) = 16*CAB*SBA - 17*S2B*(7 - 4*SBA2) + 62*S2A*SBA2

        AllHhhx(585) =
     &   -2*m12sq*AllHhhx(580) +
     &    (MW2*SW2)/(Alfa*Pi)*(CBA*SBA*AllHhhx(584))

	AllHhhx(587) = 24*CAB*SBA - S2B*(417 - 382*SBA2) - 23*S2A*SBA2

        AllHhhx(594) =
     &   -4*m12sq*AllHhhx(589) +
     &    (MW2*SW2)/(Alfa*Pi)*(CBA*SBA*AllHhhx(593))

        AllHhhx(596) =
     &   8*SBA*(4*CAB*m12sq - CBA*AllHhhx(234)) +
     &    MHp2*S2B*AllHhhx(595)

	AllHhhx(60) = 4*m12sq**2 + MHp2**2*S2B**2

	AllHhhx(606) = -2*MHp2 + 3*MA02*SBA2*(1 - (2 - 3*SBA2)*SBA2)

	AllHhhx(61) = Lambda6*S2B**2 - 2*Lambda7*SB2**2

	AllHhhx(621) = MHp2*S2B*(CBA*S2B - 2*SAB) + 4*m12sq*SAB

        AllHhhx(625) =
     &   MHp2*SAB*AllHhhx(623) +
     &    (MW2*SW2)/(Alfa*Pi)*(SBA*AllHhhx(624))

        AllHhhx(628) =
     &   16*CBA2*MA02*MHp2*S2B**4*SB2**2 -
     &    4*CBA*(MA02 + MHp2)*S2B**2*SB2*AllHhhx(627) +
     &    AllHhhx(627)**2

        AllHhhx(631) =
     &   MA02*MHp2*(8 - 9*SBA2) - MHp2**2*(2 - 3*SBA2) -
     &    6*MA02**2*SBA2

	AllHhhx(646) = Mh02**2 - 4*Mh02*MHp2 + 3*MHp2**2

	AllHhhx(648) = Mh02*MHp2*(8*S2A + S2B) - 2*AllHhhx(647)

	AllHhhx(650) = Mh02 - 2*MHp2 - 6*(Mh02 - 3*MHp2)*SBA2

        AllHhhx(652) =
     &   MHp2**2*(2*S2A - S2B) + 3*Mh02*MHp2*(S2A + S2B) -
     &    Mh02**2*(2*S2A + S2B)

        AllHhhx(653) =
     &   m12sq*(3*S2A - S2B) +
     &    (3*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHhhx(658) =
     &   (Mh02 - MHp2)*(2*Mh02 + MHp2)*S2A + S2B*AllHhhx(657)

	AllHhhx(659) = (Mh02 - MHp2)*(3*S2A - S2B) + 6*CAB*MHp2*SBA

	AllHhhx(660) = CAB*(1 - 3*CBA2)*SBA + (3*S2A - S2B)*SBA2

	AllHhhx(661) = -3*CBA2*(Mh02 - MHp2)**2 + MHp2**2*(1 - 6*SBA2)

	AllHhhx(663) = 2*Mh02**4*S2A + MHp2**4*S2B

        AllHhhx(666) =
     &   MHp2*S2B*AllHhhx(664) - 4*m12sq*AllHhhx(665) -
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA*(1 - 6*SBA2))

	AllHhhx(669) = CBA*SAB - CAB*SBA - 6*S2A*SBA2

        AllHhhx(676) =
     &   4*SB2**2*AllHhhx(673) -
     &    Mh02**2*(4*Mh02 - MHp2)*S2A*S2B*AllHhhx(675)

        AllHhhx(678) =
     &   Mh02*(2*Mh02 + MHp2)*S2A*S2B - 2*m12sq*AllHhhx(413) -
     &    (6*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(Mh02 + MHp2)*(CB2*Lambda6 - Lambda7*SB2)*SBA)

	AllHhhx(680) = Mh02**2 - 2*Mh02*MHp2 - MHp2**2

        AllHhhx(695) =
     &   CBA2*Lambda7*Mh02*S2B**2 +
     &    2*CB2**2*Lambda6*MHp2*(1 - SBA2)

        AllHhhx(697) =
     &   2*CBA2*Mh02 - MHp2*(1 - SBA2) + MA02*(1 - 2*CBA2 - SBA2)

	AllHhhx(70) = C2B*CBA*Mh02 + CAB*(Mh02 - MHp2)

	AllHhhx(702) = MA02*Mh02*(Mh02 - 3*MHp2) + MA02**2*MHp2

        AllHhhx(706) =
     &   2*(MA02 + Mh02)*MHp2**2 - MHp2**3 + Mh02*AllHhhx(705)

        AllHhhx(716) =
     &   MA02**2 + Mh02**2 - Mh02*MHp2 + MHp2**2 -
     &    MA02*(Mh02 + 2*MHp2)

        AllHhhx(717) =
     &   CAB*(2*m12sq - Mh02*S2B) -
     &    (MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

	AllHhhx(721) = 2*(MA02 - Mh02)**3 - (MA02 - MHp2)**3

	AllHhhx(722) = (MA02 - Mh02)**3 - 2*(MA02 - MHp2)**3

        AllHhhx(727) =
     &   (2*MA02 - Mh02 - MHp2)*(Mh02 - MHp2)**3*S2A -
     &    S2B*AllHhhx(726)

        AllHhhx(729) =
     &   -(m12sq*(Mh02 - MHp2)**2*AllHhhx(728)) +
     &    (3*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(Mh02 - MHp2)**3*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHhhx(738) =
     &   2*MA02**4 - Mh02**2*(Mh02 - MHp2)*MHp2 -
     &    MA02**2*(Mh02 - 4*MHp2)*(Mh02 + MHp2) -
     &    MA02**3*(Mh02 + 5*MHp2) + MA02*AllHhhx(737)

        AllHhhx(743) =
     &   2*m12sq*AllHhhx(741) - (Mh02 - MHp2)*AllHhhx(742)

        AllHhhx(750) =
     &   Mh02*(Mh02 - MHp2)*MHp2*(Mh02 - MHp2*(2 - SBA2)) +
     &    2*MA02**4*(1 + SBA2) - MA02*AllHhhx(747) +
     &    MA02**2*AllHhhx(748) - MA02**3*AllHhhx(749)

        AllHhhx(755) =
     &   CBA2**2*(MA02 - Mh02)**2*MHp2 - CBA2*AllHhhx(754)

        AllHhhx(758) =
     &   MA02**4 + Mh02**2*MHp2**2 - 2*MA02**3*(Mh02 + MHp2) -
     &    MHp2*AllHhhx(757)

        AllHhhx(759) =
     &   CBA2*Mh02*(MA02 - MHp2) - (MA02 - Mh02)*MHp2*SBA2

        AllHhhx(760) =
     &   -((Mh02 + 2*MHp2)*S2A*S2B) + 2*m12sq*(3*S2A + S2B) +
     &    (6*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

	AllHhhx(773) = 8*CBA*SAB - S2B*(4 + 11*SBA2)

	AllHhhx(777) = C2B*SBA*(15 - 4*SBA2) + 10*SAB*SBA2

	AllHhhx(780) = C2B*SBA*(2 - SBA2) + 2*SAB*SBA2

	AllHhhx(790) = S2A + S2B - 2*CBA*SAB + (3*S2A - S2B)*SBA2

	AllHhhx(791) = -6*CBA*SAB + 2*(S2A + CAB*SBA) + S2B*(3 - SBA2)

	AllHhhx(792) = S2A + S2B - 2*CBA*SAB + 2*S2A*SBA2

        AllHhhx(793) =
     &   6*CBA*SAB + 2*CAB*SBA - 2*S2A*(1 - SBA2) - S2B*(3 + SBA2)

        AllHhhx(798) =
     &   2*C2B*m12sq*SBA*(MA02 - MHp2*SBA2) - MA02*AllHhhx(795) +
     &    SBA2*AllHhhx(797)

        AllHhhx(813) =
     &   MHp2*S2B**3*SAB*(7 - 33*SBA2)*SBA2 +
     &    (MW2*SW2)/(Alfa*Pi)*(SBA*AllHhhx(812))

        AllHhhx(818) =
     &   MHp2*S2B**3*SAB*(8 - 12*SBA2 - 75*SBA2**2) +
     &    (MW2*SW2)/(Alfa*Pi)*(SBA*AllHhhx(817))

        AllHhhx(823) =
     &   MHp2*S2B**3*SAB*(4 - 12*SBA2 - 51*SBA2**2) -
     &    (MW2*SW2)/(Alfa*Pi)*(SBA*AllHhhx(822))

        AllHhhx(829) =
     &   MHp2*S2B**3*SAB*(24 - 38*SBA2 - 321*SBA2**2) +
     &    (MW2*SW2)/(Alfa*Pi)*(SBA*AllHhhx(828))

        AllHhhx(835) =
     &   3*MHp2*S2B**3*SAB*(8 - 16*SBA2 - 167*SBA2**2) +
     &    (MW2*SW2)/(Alfa*Pi)*(SBA*AllHhhx(834))

        AllHhhx(84) = 2*m12sq*(3*S2A - S2B) -
     &    (2*Mh02 + MHp2)*S2A*S2B + MHp2*S2B**2

        AllHhhx(894) =
     &   CB2*Lambda6*S2B**2*AllHhhx(892) + SB2*AllHhhx(893)

        AllHhhx(896) =
     &   MHp2*S2B*SAB*(4 - 2*SBA2 - 27*SBA2**2) -
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*
     &       (4 - 2*SBA2 - 27*SBA2**3))

        AllHhhx(900) =
     &   MHp2*S2B**3*SAB*(24 + 37*SBA2 - 144*SBA2**2) +
     &    (MW2*SW2)/(Alfa*Pi)*(SBA*AllHhhx(899))

        AllHhhx(907) =
     &   MA02**3*MHp2*(24 + 37*SBA2 - 144*SBA2**3) +
     &    MA02*MHp2**3*(16 - 5*SBA2 - 117*SBA2**3)

        AllHhhx(92) = 5*MHp2**2*S2B**2 +
     &    4*m12sq**2*(8 - 3*(4 - 3*SBA2)*SBA2)

        AllHhhx(949) =
     &   Mh02**4 - 6*Mh02**3*MHp2 + 12*Mh02**2*MHp2**2 -
     &    12*Mh02*MHp2**3 + 4*MHp2**4 -
     &    (Mh02 - 2*MHp2)*AllHhhx(948)

        AllHhhx(953) =
     &   2*C2B*m12sq*SBA*AllHhhx(950) + MA02*AllHhhx(951) -
     &    (Mh02 - MHp2)*AllHhhx(952)

        AllHhhx(957) =
     &   MHp2**2*S2B*(6 + 17*SBA2) - Mh02**2*AllHhhx(955) +
     &    Mh02*MHp2*AllHhhx(956)

        AllHhhx(992) =
     &   m12sq*(MA02 - Mh02)**2*(MA02 - MHp2)*(Mh02 - MHp2)*
     &     (3*S2A - S2B) + MA02**4*(2*Mh02 - MHp2)*S2B**2

        AllHhhx(996) =
     &   Mh02**4*(4*S2A - 3*S2B)*S2B - 3*MHp2**4*S2B**2 -
     &    2*Mh02*MHp2**2*AllHhhx(993) + 2*Mh02**3*AllHhhx(994) -
     &    6*Mh02**2*MHp2*AllHhhx(995)

        AllHhhx(999) =
     &   Mh02**2*S2B*(2*S2A + 5*S2B) - MHp2*AllHhhx(997) -
     &    Mh02*AllHhhx(998)

        AllHhhx(1007) =
     &   4*AllHhhx(992) - MA02*AllHhhx(996) -
     &    2*MA02**3*AllHhhx(999) + MA02**2*AllHhhx(1003) +
     &    Mh02*MHp2*AllHhhx(1006)

        AllHhhx(1019) =
     &   -11*MA02**3*S2B**2*(1 - SBA2)*SBA2 +
     &    8*C2B*CBA*m12sq*(MA02 - MHp2)*SBA*
     &     (MA02 - MHp2*(2 + SBA2)) + MHp2**2*AllHhhx(1012) -
     &    MA02*MHp2*AllHhhx(1015) + MA02**2*AllHhhx(1018)

        AllHhhx(1056) =
     &   MA02**2*AllHhhx(1030) + MA02*AllHhhx(1040) +
     &    Mh02*MHp2*AllHhhx(1047) - 2*AllHhhx(1055)

        AllHhhx(1064) =
     &   m12sq**2*AllHhhx(1061) + m12sq*Mh02*S2B*AllHhhx(1062) -
     &    Mh02**2*S2B**2*AllHhhx(1063)

        AllHhhx(1088) =
     &   -4*CAB*AllHhhx(1086) + Mh02*S2B*SBA*AllHhhx(1087) +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*AllHhhx(1085))

        AllHhhx(109) =
     &   MHp2*S2B**3*SAB*(96 - 58*SBA2 - 159*SBA2**2) +
     &    (MW2*SW2)/(Alfa*Pi)*(SBA*AllHhhx(108))

	AllHhhx(1148) = -1 + MA02/Mh02 + Mh02/(MA02 - Mh02)

        AllHhhx(1151) =
     &   CBA2*Mh02 - MHp2*(2 + SBA2) + MA02*(2 - CBA2 + SBA2)

        AllHhhx(1152) =
     &   2*C2B*m12sq + (MW2*SW2)/(Alfa*Pi)*(Lambda7*SB2)

        AllHhhx(1156) =
     &   -2*SB2*SBA*AllHhhx(1155) +
     &    (MW2*SW2)/(Alfa*Pi)*(Lambda6*S2B*AllHhhx(1151))

	AllHhhx(1160) = -2*m12sq*(3*S2A + S2B) + S2B*AllHhhx(1159)

        AllHhhx(1161) =
     &   -(CAB*(2*m12sq - Mh02*S2B)) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*(1 - 3*SBA2))

	AllHhhx(1167) = Mh02**2 + MHp2**2 - MA02*(Mh02 + MHp2)

	AllHhhx(1169) = MA02*(Mh02 - MHp2) - MHp2*(Mh02 + 2*MHp2)

        AllHhhx(1171) =
     &   (2 - CBA2)*MA02**2 + (Mh02 - 2*MHp2)**2 + Mh02**2*SBA2 -
     &    MA02*AllHhhx(1170)

        AllHhhx(1175) =
     &   MA02**3 + MA02**2*(Mh02 - MHp2*(3 - SBA2)) +
     &    Mh02*AllHhhx(1172) + MA02*AllHhhx(1174)

        AllHhhx(1177) =
     &   (Mh02*(MA0 - MHp)*(MA0 + MHp))/(MA02 - Mh02) +
     &    (MA02*(MA02 - Mh02))/(MA02 - MHp2)

        AllHhhx(1197) =
     &   -2*CAB*(2*m12sq - Mh02*S2B)*SBA - SBA2*AllHhhx(1194) +
     &    CBA*AllHhhx(1196)

        AllHhhx(1199) =
     &   -(m12sq*(Mh02 + CBA2*(Mh02 - 4*MHp2))) +
     &    Mh02*(Mh02 - 2*MHp2)*S2B

	AllHhhx(1201) = -4*m12sq*(3*S2A - S2B) + S2B*AllHhhx(1200)

	AllHhhx(1202) = 4*CBA2*MHp2 - Mh02*(1 + CBA2 + 6*SBA2)

	AllHhhx(1204) = 6*Mh02**2 - 6*Mh02*MHp2 + MHp2**2

        AllHhhx(1231) =
     &   2*m12sq*(3*S2A - S2B) - (2*Mh02 + MHp2)*S2A*S2B +
     &    (6*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHhhx(1260) =
     &   -2*(4*Mh02 - MHp2)*MHp2*(CBA2*S2A - 6*CAB*SBA) +
     &    S2A*SBA2*AllHhhx(1259)

        AllHhhx(1262) =
     &   -6*CBA2*(4*Mh02 - MHp2)*MHp2 + SBA2*AllHhhx(1261)

        AllHhhx(1267) =
     &   -2*CBA2*(4*Mh02 - MHp2)*MHp2*
     &     (4*m12sq*SB2*AllHhhx(1263) + S2B*AllHhhx(1264)) +
     &    SBA2*AllHhhx(1261)*AllHhhx(1266)

        AllHhhx(128) =
     &   MHp2*S2B**3*SAB*(48 - 46*SBA2 - 105*SBA2**2) +
     &    (MW2*SW2)/(Alfa*Pi)*(SBA*AllHhhx(127))

        AllHhhx(1328) =
     &   8*CAB*(MA02 - MHp2)*(m12sq - 3*CBA2*m12sq + Mh02*S2B) +
     &    SBA*AllHhhx(1326) +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*AllHhhx(1327))

        AllHhhx(1334) =
     &   (2*CBA2*AllHhhx(1332))/(MA02 - Mh02)**2 +
     &    (SBA2*AllHhhx(1333))/((MA02 - MHp2)**2*(4*Mh02 - MHp2))

        AllHhhx(1337) =
     &   4*MA02*SB2*SBA + (MW2*SW2)/(Alfa*Pi)*(CBA*Lambda6)

        AllHhhx(1338) =
     &   4*MHp2*SB2*SBA + (MW2*SW2)/(Alfa*Pi)*(CBA*Lambda6)

        AllHhhx(1339) =
     &   (MA02 - Mh02 + MHp2)*S2B**2*SBA +
     &    (2*MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

        AllHhhx(1341) =
     &   -(CB2*AllHhhx(1340)) +
     &    (MW2*SW2)/(Alfa*Pi)*(CBA*Lambda7*SB2)

        AllHhhx(1343) =
     &   Mh02**2*S2B**2*SBA2 -
     &    (2*MW2*SW2)/(Alfa*Pi)*(Lambda7*AllHhhx(1342))

        AllHhhx(1345) =
     &   CAB*(2*m12sq - Mh02*S2B) -
     &    (MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2)) +
     &    1/2.D0*((Mh02 - 2*MHp2)*S2B**2*SBA)

        AllHhhx(1356) =
     &   CAB**2*(2*m12sq - Mh02*S2B)**2 +
     &    CB2**2*AllHhhx(1337)*AllHhhx(1338) -
     &    CAB*(2*m12sq - Mh02*S2B)*AllHhhx(1339) +
     &    (MW2**2*SW2**2)/(Alfa2*Pi**2)*(CBA2*Lambda7**2*SB2**2) +
     &    S2B**2*(Mh02*SBA*AllHhhx(1341) + 1/4.D0*AllHhhx(1343))

	AllHhhx(1364) = CBA2*(MA02 - MHp2) - (MA02 - Mh02)*SBA2

        AllHhhx(1368) =
     &   CAB*(2*m12sq - Mh02*S2B) -
     &    (MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2)) -
     &    1/2.D0*((2*MA02 - Mh02)*S2B**2*SBA)

        AllHhhx(1380) =
     &   CAB**2*(2*m12sq - Mh02*S2B)**2 +
     &    CB2**2*AllHhhx(1337)*AllHhhx(1338) -
     &    CAB*(2*m12sq - Mh02*S2B)*AllHhhx(1339) +
     &    (MW2**2*SW2**2)/(Alfa2*Pi**2)*(CBA2*Lambda7**2*SB2**2) +
     &    S2B**2*(Mh02*SBA*
     &        (-(CB2*AllHhhx(1340)) +
     &          (MW2*SW2)/(Alfa*Pi)*(CBA*Lambda7*SB2)) +
     &       1/4.D0*AllHhhx(1343))

        AllHhhx(1385) =
     &   AllHhhx(1383)/(4*MA02 - Mh02) - MHp2*AllHhhx(1384)

        AllHhhx(1393) =
     &   CAB*(2*m12sq - Mh02*S2B) -
     &    S2B**2*SBA*(MA02 - 1/2.D0*Mh02) -
     &    (MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

        AllHhhx(1409) =
     &   (2*CBA2*Mh02)/(MA02 - Mh02) +
     &    ((2*Mh02 - MHp2)*SBA2)/(MA02 - MHp2)

        AllHhhx(1419) =
     &   CBA2*(2*MA02 - Mh02)*(MA02 - MHp2) +
     &    2*MA02*(Mh02 - MHp2)*(1 - SBA2)

	AllHhhx(142) = (Mh02 - MHp2)*S2A - 2*MA02*S2B

        AllHhhx(1421) =
     &   ((2*MA02 + Mh02)*AllHhhx(1419))/(Mh02 - MHp2) +
     &    MA02*Mh02*AllHhhx(1420)

	AllHhhx(144) = 6*Mh02**2*S2A - (MA02 + Mh02)**2*S2B

        AllHhhx(147) =
     &   2*MA02**4*SBA2 - MA02**3*(2*MHp2*SBA2 + Mh02*(2 + SBA2))

        AllHhhx(148) =
     &   -(MHp2*(S2A*S2B + S2B**2)*SBA2) -
     &    Mh02*S2B*(S2B - (S2A + 4*S2B)*SBA2) +
     &    SBA*(-4*(2*CAB + C2B*CBA)*m12sq +
     &       (6*MW2*SW2)/(Alfa*Pi)*
     &        (CBA*(CB2*Lambda6 - Lambda7*SB2)))

	AllHhhx(149) = 16*CAB*SBA - 5*S2B*(2 - SBA2) + 6*S2A*SBA2

        AllHhhx(150) =
     &   -(MHp2*(3*S2A*S2B + 2*S2B**2)*SBA2) +
     &    SBA*(-4*(4*CAB + 3*C2B*CBA)*m12sq +
     &       (14*MW2*SW2)/(Alfa*Pi)*
     &        (CBA*(CB2*Lambda6 - Lambda7*SB2)))

	AllHhhx(151) = 6*S2B - 16*CAB*SBA - (4*S2A - 7*S2B)*SBA2

        AllHhhx(152) =
     &   -(MHp2*S2A*S2B*SBA2) +
     &    SBA*(-4*(CAB + C2B*CBA)*m12sq +
     &       (4*MW2*SW2)/(Alfa*Pi)*
     &        (CBA*(CB2*Lambda6 - Lambda7*SB2)))

	AllHhhx(154) = 2*S2A - 6*CBA*SAB - 2*CAB*SBA + S2B*(3 + SBA2)

        AllHhhx(155) =
     &   -2*MHp2*(S2A + S2B - 2*CBA*SAB) + Mh02*AllHhhx(154)

        AllHhhx(156) =
     &   MHp2*S2B*(S2A + S2B + 4*CBA*SAB)*SBA2 -
     &    (6*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHhhx(1583) =
     &   2*MA02*S2B - 3*Mh02*(S2B + S2A*SBA2) +
     &    MHp2*(S2B - 3*S2A*(3 + 2*SBA2))

        AllHhhx(1584) =
     &   CBA2**2*(3*S2A - S2B) + 12*CBA2*S2A*SBA2 +
     &    (3*S2A + S2B)*SBA2**2

	AllHhhx(1586) = 3*(2*Mh02 + MHp2)*S2A + (2*MA02 + MHp2)*S2B

	AllHhhx(1587) = 9*Mh02*S2A + (2*MA02 - 3*Mh02 + MHp2)*S2B

        AllHhhx(1589) =
     &   -2*(1 + 3*CBA2**2)*m12sq + (1 + 3*CBA2)*Mh02*S2B

	AllHhhx(1591) = 3*MHp2*S2A - (MA02 + 2*MHp2)*S2B

        AllHhhx(1594) =
     &   6*MHp2*S2A*SBA2**2 + CBA2*AllHhhx(1592) +
     &    SBA2*AllHhhx(1593)

        AllHhhx(1596) =
     &   MHp2*S2B*(S2A + S2B)*SAB*SBA -
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*AllHhhx(1595))

        AllHhhx(1599) =
     &   S2A*S2B*SBA*AllHhhx(1598) -
     &    (12*MW2*SW2)/(Alfa*Pi)*
     &     (SAB*(CB2*Lambda6 - Lambda7*SB2)*(1 + 3*SBA2**2))

        AllHhhx(1613) =
     &   2*m12sq*(MA02 + 2*MHp2) - (MA02*MHp2 + 5*MHp2**2)*S2B

        AllHhhx(1616) =
     &   3*CBA2**2*Mh02 + CBA2*(MA02 + 2*MHp2) + SBA2*AllHhhx(1615)

        AllHhhx(162) =
     &   -2*MA02*MHp2*(S2A + S2B - 2*CBA*SAB) -
     &    Mh02**2*AllHhhx(159) + Mh02*AllHhhx(161)

	AllHhhx(1626) = 6*m12sq*MHp2*SBA2**2 + SBA2*AllHhhx(1625)

        AllHhhx(1627) =
     &   (CB2*Lambda6 - Lambda7*SB2)*
     &    (2*CBA2 + (2 + 3*CBA2)*SBA2 + 3*SBA2**2)

	AllHhhx(1632) = 2*MHp2*(S2A - 2*S2B) + Mh02*(S2A + 2*S2B)

        AllHhhx(1643) =
     &   CAB*(2*m12sq - Mh02*S2B) + (Mh02 - 2*MHp2)*S2B**2*SBA

        AllHhhx(1644) =
     &   2*CAB*(2*m12sq - Mh02*S2B) + (Mh02 - 2*MHp2)*S2B**2*SBA

        AllHhhx(1648) =
     &   (2*m12sq - MHp2*S2B)*SAB*AllHhhx(1646) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*AllHhhx(1647))

        AllHhhx(168) =
     &   S2B**2*AllHhhx(164) + 2*Mh02*S2B*AllHhhx(165) +
     &    4*SBA*AllHhhx(167)

        AllHhhx(1681) =
     &   (2*MA02 - Mh02)**2*SBA2 + (MA02 - MHp2)**2*(2 + SBA2) +
     &    CBA2*AllHhhx(1680)

        AllHhhx(1683) =
     &   CAB*(2*MA02 - Mh02)*S2B**2*(2*m12sq - Mh02*S2B)*SBA +
     &    (2*m12sq - MHp2*S2B)*SAB*AllHhhx(1682) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*AllHhhx(1668))

	AllHhhx(1686) = 3*Mh02**2*SBA2 + 2*CBA2*AllHhhx(1685)

        AllHhhx(1688) =
     &   CAB*(2*MA02 - Mh02)*S2B**2*(2*m12sq - Mh02*S2B)*SBA +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*AllHhhx(1687))

        AllHhhx(1693) =
     &   Mh02**2*S2A**2*S2B**2*SBA2 -
     &    4*CAB*(2*CBA2*m12sq - Mh02*S2B)*AllHhhx(1691) +
     &    4*AllHhhx(1692) -
     &    (2*MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (CBA2**3*(Lambda6*Lambda7*S2B**2 - 2*AllHhhx(1690)))

        AllHhhx(1697) =
     &   CBA2*(MA02 - Mh02)**2*Re(AllHhhl(11)) +
     &    (MA02 - MHp2)**2*(2 + SBA2)*Re(AllHhhl(12))

	AllHhhx(1700) = MA02**2*Mh02**2 - Mh02**4

        AllHhhx(1701) =
     &   (MA02 - Mh02)**2*Re(AllHhhl(16)) +
     &    2*(Mh02 - MHp2)**2*Re(AllHhhl(17))

        AllHhhx(1711) =
     &   2*CBA2*(AllHhhx(1685) + 2*AllHhhx(1701)) +
     &    3*Mh02**2*SBA2*(1 + 2*Re(AllHhhl(15)))

        AllHhhx(1718) =
     &   4*m12sq*(2*Mh02 + MHp2)*S2A + S2A**2*AllHhhx(1717)

	AllHhhx(172) = 2*S2A - 6*CBA*SAB - 2*CAB*SBA + S2B*(3 + SBA2)

        AllHhhx(1721) =
     &   CBA2*(4*MA02 - 4*Mh02 + MHp2)*S2B**2 +
     &    3*S2A*S2B*AllHhhx(1720)

        AllHhhx(1723) =
     &   -3*(5*Mh02**2 + Mh02*MHp2)*S2A*S2B**2 +
     &    S2B**3*AllHhhx(1722)

        AllHhhx(1724) =
     &   4*m12sq**2 - 2*m12sq*(Mh02 + MHp2)*S2B + Mh02*MHp2*S2B**2

	AllHhhx(1736) = -2*CBA2 + 3*CBA2**2 + 9*CBA2**4

        AllHhhx(1737) =
     &   (4*CBA2 - 3*CBA2**3)*
     &    (Lambda6*Lambda7*S2B**2 - 2*AllHhhx(1690))

	AllHhhx(1738) = -(Lambda6*Lambda7*S2B**2) + 2*AllHhhx(1690)

        AllHhhx(174) =
     &   24*C2B*CBA*m12sq*(2*MA02 - Mh02)*SBA -
     &    Mh02*(2*AllHhhx(156) - Mh02*S2B*SBA2*AllHhhx(172)) -
     &    MA02*(Mh02*S2B*SBA2*AllHhhx(172) - 2*AllHhhx(173))

        AllHhhx(1742) =
     &   -3*(Mh02*MHp2 + 2*MHp2**2)*S2A*S2B**2 +
     &    S2B**3*AllHhhx(1740) + 2*m12sq*AllHhhx(1741)

        AllHhhx(1743) =
     &   -(m12sq*(Mh02 + 2*MHp2)*S2A*S2B) +
     &    2*m12sq**2*(3*S2A + S2B)

        AllHhhx(1750) =
     &   -4*CAB**2*AllHhhx(1747) - S2B**4*SBA2*AllHhhx(1749) +
     &    4*(CAB*SBA*AllHhhx(1748) +
     &       (MW2*SW2)/(Alfa*Pi)*
     &        (CBA*(CB2*Lambda6 - Lambda7*SB2)*AllHhhx(1687)))

        AllHhhx(1752) =
     &   4*CBA2**3*m12sq**2 - 4*CBA2**2*m12sq*Mh02*S2B +
     &    CBA2*Mh02**2*S2B**2

	AllHhhx(1753) = 2*CBA2**2*m12sq*Mh02*S2B - CBA2*Mh02**2*S2B**2

        AllHhhx(1755) =
     &   -((CB2*Lambda6 - Lambda7*SB2)*
     &      (CBA2**2*Mh02*S2A*S2B*SBA - 2*CAB*AllHhhx(1754)))

        AllHhhx(1758) =
     &   -12*m12sq**2*(3*S2A**2 - 2*S2A*S2B) -
     &    S2B**2*AllHhhx(1757) +
     &    12*(m12sq*(2*Mh02 + MHp2)*S2A**2*S2B +
     &       (MW2*SW2)/(Alfa*Pi)*
     &        (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA*AllHhhx(1651)))

        AllHhhx(1760) =
     &   -2*Lambda6*Lambda7*S2B**2 + 4*AllHhhx(1690) +
     &    9*SBA2**2*AllHhhx(1738)

        AllHhhx(1771) =
     &   2*AllHhhx(1770)*Re(AllHhhl(4)) +
     &    3*Mh02**2*SBA2*Re(AllHhhl(15))

        AllHhhx(1774) =
     &   -(MA02*Mh02) + AllHhhx(1772) +
     &    AllHhhx(1773)*(Re(AllHhhl(5)) - 2*Re(AllHhhl(17)))

	AllHhhx(1776) = m12sq**2*S2A + m12sq*Mh02*S2A**2

	AllHhhx(1778) = 4*m12sq**2 - 4*m12sq*MHp2*S2B + MHp2**2*S2B**2

        AllHhhx(1782) =
     &   6*m12sq*(2*Mh02 + MHp2)*S2A**2*S2B -
     &    6*m12sq**2*(9*S2A**2 - 2*S2A*S2B) +
     &    S2B**2*AllHhhx(1780) - 2*S2B**4*AllHhhx(1781)

        AllHhhx(1784) =
     &   CBA*AllHhhx(1783) +
     &    (18*MW2*SW2)/(Alfa*Pi)*(Lambda7*m12sq*SB2*SBA*SBA2**2)

        AllHhhx(1785) =
     &   -4*(CAB**2*AllHhhx(1747) + CAB*SBA*AllHhhx(1761)) -
     &    S2B**4*SBA2*AllHhhx(1764) +
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*AllHhhx(1644))

        AllHhhx(1787) =
     &   2*SAB**2*AllHhhx(1778) +
     &    (MW2**2*SW2**2)/(Alfa2*Pi**2)*(SBA2*AllHhhx(1738)) -
     &    2*(CBA*SAB*AllHhhx(1783) +
     &       (MW2*SW2)/(Alfa*Pi)*
     &        ((CB2*Lambda6 - Lambda7*SB2)*SBA*AllHhhx(1786)))

        AllHhhx(1798) =
     &   CBA2*(MA02 - MHp2)**2*Re(AllHhhl(4)) +
     &    CBA2**2*(Mh02 - MHp2)**2*Re(AllHhhl(5))

	AllHhhx(188) = 96*CAB*SBA + 52*S2A*SBA2 + S2B*(10 + 21*SBA2)

        AllHhhx(189) =
     &   -(MHp2*(S2A*S2B + 2*S2B**2)*SBA2) +
     &    SBA*(-4*(2*CAB + C2B*CBA)*m12sq +
     &       (6*MW2*SW2)/(Alfa*Pi)*
     &        (CBA*(CB2*Lambda6 - Lambda7*SB2)))

	AllHhhx(19) = S2A - 6*CAB*SBA + (S2A + 3*S2B)*SBA2

	AllHhhx(190) = 18*S2B - 64*CAB*SBA - (28*S2A - 25*S2B)*SBA2

	AllHhhx(191) = 5*S2B + 16*CAB*SBA + 2*(6*S2A + S2B)*SBA2

        AllHhhx(192) =
     &   -(MHp2*S2B**2*SBA2) +
     &    SBA*(-2*CAB*m12sq +
     &       (MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2)))

        AllHhhx(20) = -8*(S2B + CBA*SAB) + 15*S2B*SBA2 +
     &    4*S2A*(1 + SBA2)

        AllHhhx(203) =
     &   Mh02*AllHhhx(199) + MA02*AllHhhx(200) - MHp2*AllHhhx(202)

        AllHhhx(205) =
     &   -4*m12sq*(MA02 - MHp2)**2*S2B**2 + S2B**3*AllHhhx(204)

        AllHhhx(214) =
     &   -(S2B**2*AllHhhx(213)) + (MW2*SW2)/(Alfa*Pi)*AllHhhx(211)

        AllHhhx(236) =
     &   S2B*AllHhhx(230) + 2*Mh02**2*MHp2*AllHhhx(232) -
     &    Mh02**3*AllHhhx(235)

        AllHhhx(24) = -4*MA02*S2B*(1 - SBA2) +
     &    4*Mh02*AllHhhx(19) - MHp2*AllHhhx(23)

        AllHhhx(247) =
     &   S2B*AllHhhx(239) - Mh02**3*MHp2*AllHhhx(241) +
     &    Mh02**4*AllHhhx(243) + Mh02**2*MHp2**2*AllHhhx(246)

        AllHhhx(256) =
     &   S2B*AllHhhx(250) + 2*Mh02**2*MHp2*AllHhhx(251) -
     &    Mh02*MHp2**2*AllHhhx(253) + Mh02**3*AllHhhx(255)

        AllHhhx(260) =
     &   (2 - CBA2)*MA02**2 + (2 + CBA2)*MA02*Mh02 + Mh02**2 +
     &    5*MHp2**2 - MHp2*AllHhhx(259)

        AllHhhx(261) =
     &   CBA2*Mh02**2 - MA02*(CBA2*Mh02 + 4*MHp2) +
     &    2*(MA02**2 + MHp2**2)

        AllHhhx(264) =
     &   -4*C2B*m12sq + S2B*(2*CAB*CBA*Mh02 - SBA*AllHhhx(263)) +
     &    (2*MW2*SW2)/(Alfa*Pi)*(CB2*Lambda6 - Lambda7*SB2)

	AllHhhx(272) = Mh02 + MA02*(1 + 2*SBA2) - 2*(MA02 + Mh02*SBA2)

        AllHhhx(28) = MA02*S2B*(1 - SBA2) - Mh02*AllHhhx(26) -
     &    MHp2*AllHhhx(27)

        AllHhhx(287) =
     &   MHp2**2*(1 - SBA2) - 2*MA02*MHp2*(2 - SBA2) +
     &    5*MA02**2*SBA2

	AllHhhx(289) = -m12sq - MHp2*S2B + 3*m12sq*SBA2

        AllHhhx(290) =
     &   CBA*S2B*AllHhhx(288) + 4*SAB*AllHhhx(289) -
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*(1 - 3*SBA2))

	AllHhhx(293) = 48*MA02**6 - MHp2**6

	AllHhhx(299) = 12*MA02**4 - MHp2**4

        AllHhhx(300) =
     &   MHp2*S2B*SAB - (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHhhx(304) =
     &   MHp2*S2B**3*SAB*(20 - 17*SBA2 + 36*SBA2**2) +
     &    (MW2*SW2)/(Alfa*Pi)*(SBA*AllHhhx(303))

        AllHhhx(307) =
     &   MHp2*S2B**3*SAB*(2 - 3*SBA2 + 9*SBA2**2) -
     &    (MW2*SW2)/(Alfa*Pi)*(SBA*AllHhhx(306))

        AllHhhx(310) =
     &   MHp2*S2B**3*SAB*(8 + SBA2 + 9*SBA2**2) -
     &    (MW2*SW2)/(Alfa*Pi)*(SBA*AllHhhx(309))

        AllHhhx(312) =
     &   16*MA02**3*MHp2*(2 - 3*SBA2 + 9*SBA2**3) +
     &    MA02*MHp2**3*(8 + SBA2 + 9*SBA2**3) -
     &    2*MA02**2*MHp2**2*(20 - 17*SBA2 + 36*SBA2**3) +
     &    2*SBA2*AllHhhx(299)

	AllHhhx(343) = MHp2*S2B*(1 - 3*SBA2) - 2*m12sq*(1 - 3*SBA2**2)

        AllHhhx(347) =
     &   12*CBA*SAB - S2A*(6 - 13*SBA2) + S2B*(16 - 3*SBA2)

        AllHhhx(348) =
     &   18*CBA*SAB - 4*CAB*SBA - S2A*(6 - 13*SBA2) -
     &    S2B*(9 - 2*SBA2)

	AllHhhx(349) = 4*CAB*SBA - (S2A + 2*S2B)*SBA2

	AllHhhx(350) = 4*S2A*SBA2 + S2B*(2 + 15*SBA2)

        AllHhhx(351) =
     &   6*CBA*SAB - 70*CAB*SBA - S2B*(3 - 35*SBA2) -
     &    2*S2A*(1 - 11*SBA2)

        AllHhhx(352) =
     &   2*CBA*SAB - S2A*(1 - 11*SBA2) + S2B*(7 + 31*SBA2)

        AllHhhx(353) =
     &   18*CBA*SAB + S2B*(19 - 35*SBA2) - S2A*(9 - 4*SBA2)

        AllHhhx(354) =
     &   54*CBA*SAB + 70*CAB*SBA - 2*S2A*(9 - 4*SBA2) -
     &    S2B*(27 + 35*SBA2)

        AllHhhx(357) =
     &   MHp2*AllHhhx(355) - 2*Mh02*(2*S2A + 3*AllHhhx(356))

        AllHhhx(359) =
     &   4*CBA*SAB*SBA2 + S2B*(42 - 19*SBA2 - 23*SBA2**2)

        AllHhhx(360) =
     &   6*CBA*SAB*SBA2 - S2B*(103 - 44*SBA2 - 59*SBA2**2)

        AllHhhx(362) =
     &   -2*m12sq*AllHhhx(361) +
     &    (3*MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*(3 - 4*SBA2))

	AllHhhx(363) = 16*CBA*SAB*SBA2 - 3*S2B*(2 - SBA2 - SBA2**2)

        AllHhhx(365) =
     &   m12sq*AllHhhx(364) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*(1 + SBA2))

	AllHhhx(366) = 22*CBA*SAB*SBA2 - S2B*(15 - 7*SBA2 - 8*SBA2**2)

        AllHhhx(368) =
     &   2*m12sq*AllHhhx(367) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*(14 + 19*SBA2))

	AllHhhx(373) = 2*MHp2 + MA02*(1 - 3*SBA2)*SBA2

        AllHhhx(374) =
     &   -(MHp2*S2B*(MHp2 - MA02*SBA2)) + m12sq*AllHhhx(373)

	AllHhhx(379) = S2B - (S2B + 4*CBA*SAB)*SBA2

	AllHhhx(381) = 4*CBA*SAB*SBA2 - S2B*(5 - 3*SBA2 - 2*SBA2**2)

	AllHhhx(384) = CBA2*(Mh02 - MHp2)*S2B + 2*CBA*MHp2*SAB

        AllHhhx(394) =
     &   -8*MA02**4*S2B*(11 + SBA2) -
     &    MHp2**3*(4*Mh02*AllHhhx(349) + MHp2*AllHhhx(386)) -
     &    2*MA02**2*MHp2*(MHp2*AllHhhx(387) - Mh02*AllHhhx(388)) -
     &    MA02*MHp2**2*(2*Mh02*AllHhhx(389) - MHp2*AllHhhx(390)) +
     &    2*MA02**3*AllHhhx(393)

        AllHhhx(414) =
     &   Mh02*S2B*AllHhhx(412) - 2*m12sq*AllHhhx(413) -
     &    (6*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(Mh02 + MHp2)*(CB2*Lambda6 - Lambda7*SB2)*SBA)

	AllHhhx(415) = 17*Mh02*MHp2 - 4*(Mh02**2 + MHp2**2)

        AllHhhx(418) =
     &   Mh02**2*(S2A + 2*CAB*SBA - S2B*SBA2) + MHp2*AllHhhx(417)

        AllHhhx(424) =
     &   -(MHp2**5*(2*S2A - S2B)*SBA2) + Mh02**5*AllHhhx(423)

        AllHhhx(427) =
     &   MHp2*S2B*AllHhhx(425) - 2*m12sq*AllHhhx(426) -
     &    (16*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHhhx(43) = (MA02**2 + 2*MA02*Mh02)*S2B -
     &    Mh02**2*(6*S2A + S2B)

        AllHhhx(430) =
     &   -(MHp2*S2B*AllHhhx(428)) + 4*m12sq*AllHhhx(429) +
     &    (8*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA*(13 - 12*SBA2))

        AllHhhx(432) =
     &   -2*m12sq*AllHhhx(431) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA*(1 - 6*SBA2))

        AllHhhx(439) =
     &   Mh02*MHp2**3*AllHhhx(435) -
     &    Mh02**4*(MHp2*S2B*AllHhhx(436) - 16*AllHhhx(438))

        AllHhhx(44) = -2*CAB*
     &     (m12sq - 3*CBA2*m12sq + Mh02*S2B) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(1 - 3*CBA2)*(CB2*Lambda6 - Lambda7*SB2))

        AllHhhx(441) =
     &   4*Mh02**3 - 21*Mh02**2*MHp2 + 21*Mh02*MHp2**2 - 4*MHp2**3

	AllHhhx(445) = 4*CAB*SBA - 10*S2A*SBA2 - S2B*(18 + 5*SBA2)

	AllHhhx(446) = 490*S2B - 212*CAB*SBA + (74*S2A + 61*S2B)*SBA2

        AllHhhx(456) =
     &   2*m12sq*AllHhhx(448) +
     &    (MW2*SW2)/(Alfa*Pi)*(CBA*SBA*AllHhhx(455))

	AllHhhx(457) = 435*S2B + 16*CAB*SBA - (205*S2A + 274*S2B)*SBA2

        AllHhhx(465) =
     &   -6*m12sq*AllHhhx(459) +
     &    (MW2*SW2)/(Alfa*Pi)*(CBA*SBA*AllHhhx(464))

	AllHhhx(466) = -5*S2B*(2 - 7*SBA2) + 26*S2A*SBA2

        AllHhhx(472) =
     &   -2*m12sq*AllHhhx(468) +
     &    (MW2*SW2)/(Alfa*Pi)*(CBA*SBA*AllHhhx(471))

        AllHhhx(491) =
     &   Mh02**4*MHp2*AllHhhx(481) +
     &    Mh02**2*MHp2**3*(MHp2*S2B**5*AllHhhx(482) -
     &       8*AllHhhx(490))

        AllHhhx(497) =
     &   S2B*AllHhhx(495) + 4*m12sq*(Mh02 - MHp2)*AllHhhx(496)

        AllHhhx(502) =
     &   (MA02 - 3*MHp2)*S2B**2*SBA2**2 - SBA2*AllHhhx(499) -
     &    2*CBA*SBA*AllHhhx(501)

        AllHhhx(53) = CBA*(2*MA02 - MHp2)*(3*S2A - 2*S2B) -
     &    2*(6*MA02 - MHp2)*SAB

	AllHhhx(54) = 4*MA02**2 + 6*MA02*MHp2 - MHp2**2

        AllHhhx(548) =
     &   -4*Mh02**5*S2B**5*AllHhhx(503) + 4*MHp2**4*AllHhhx(510) +
     &    Mh02**3*MHp2*(MHp2*S2B**5*AllHhhx(511) +
     &       8*AllHhhx(519)) -
     &    2*Mh02*MHp2**3*(MHp2*S2B**5*AllHhhx(521) -
     &       2*AllHhhx(529)) +
     &    Mh02**4*(MHp2*S2B**5*AllHhhx(531) + 16*AllHhhx(539)) +
     &    2*Mh02**2*MHp2**2*
     &     (MHp2*S2B**5*AllHhhx(540) - 12*AllHhhx(547))

	AllHhhx(55) = MHp2*(2 - 3*SBA2) + 6*MA02*SBA2

	AllHhhx(550) = Mh02**2 + 4*MHp2**2 - Mh02*(MA02 + 5*MHp2)

        AllHhhx(555) =
     &   -4*C2B*CBA*m12sq*(Mh02 - 2*MHp2)*SBA*(4 + SBA2) -
     &    Mh02**2*S2B*SBA2*(S2A + 2*CAB*SBA - S2B*SBA2) +
     &    Mh02*AllHhhx(551) - MHp2*AllHhhx(554)

	AllHhhx(557) = 6*CAB*SBA + 2*S2B*(2 - SBA2) + S2A*SBA2

        AllHhhx(561) =
     &   -6*MHp2*S2B**5*(S2B - 2*S2A*SBA2) +
     &    8*m12sq*AllHhhx(558) +
     &    (MW2*SW2)/(Alfa*Pi)*(CBA*SBA*AllHhhx(560))

        AllHhhx(562) =
     &   528*CAB*SBA + S2B*(1274 - 1033*SBA2) + 410*S2A*SBA2

        AllHhhx(569) =
     &   -2*m12sq*AllHhhx(564) +
     &    (MW2*SW2)/(Alfa*Pi)*(CBA*SBA*AllHhhx(568))

	AllHhhx(58) = 4*CB2**2*Lambda6 - Lambda7*S2B**2

        AllHhhx(586) =
     &   Mh02**4*MHp2*(MHp2*S2B**5*AllHhhx(570) -
     &       2*AllHhhx(577)) +
     &    Mh02*MHp2**4*(MHp2*S2B**5*AllHhhx(578) + 4*AllHhhx(585))

        AllHhhx(59) = MHp2*S2B**3*SAB -
     &    (MW2*SW2)/(Alfa*Pi)*(SB2*SBA*AllHhhx(58))

        AllHhhx(597) =
     &   Mh02**2*MHp2**3*(MHp2*S2B**5*AllHhhx(587) +
     &       2*AllHhhx(594)) + Mh02**5*S2B**4*AllHhhx(596)

        AllHhhx(604) =
     &   3*MA02*S2B*SAB*SBA2**2 +
     &    (MW2*SW2)/(Alfa*Pi)*((CB2*Lambda6 - Lambda7*SB2)*SBA)

	AllHhhx(607) = -(MHp2**3*S2B**2) + 2*m12sq**2*AllHhhx(606)

        AllHhhx(608) =
     &   3*MA02*S2B*SAB*SBA2 +
     &    (2*MW2*SW2)/(Alfa*Pi)*((CB2*Lambda6 - Lambda7*SB2)*SBA)

	AllHhhx(62) = 2*CB2**2*Lambda6**2 - Lambda7*AllHhhx(61)

	AllHhhx(620) = CBA2*S2B**8 + 4*S2B**6*SAB**2

        AllHhhx(622) =
     &   (MW2**2*SW2**2)/(Alfa2*Pi**2)*(Lambda6*S2B**6*SBA2) +
     &    (2*MW2*SW2)/(Alfa*Pi)*(S2B**4*SB2*SBA*AllHhhx(621))

        AllHhhx(626) =
     &   -(CBA2*MHp2*(MA02 + MHp2)*S2B**8) + 2*S2B**5*AllHhhx(625)

        AllHhhx(629) =
     &   CB2**2*AllHhhx(628) +
     &    (MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (Lambda7**2*S2B**4*SB2**2*SBA2)

        AllHhhx(649) =
     &   4*m12sq*(3*S2A + S2B)*AllHhhx(646) +
     &    (Mh02 + 2*MHp2)*S2B*AllHhhx(648)

        AllHhhx(651) =
     &   Mh02*(Mh02 - 4*MHp2)*(2*m12sq - MHp2*S2B)*SAB +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((Mh02 - MHp2)*(CB2*Lambda6 - Lambda7*SB2)*SBA*
     &       AllHhhx(650))

        AllHhhx(654) =
     &   S2B*AllHhhx(652) + 2*(Mh02 - 2*MHp2)*AllHhhx(653)

        AllHhhx(662) =
     &   CBA2*Mh02*(S2B*AllHhhx(658) - 2*m12sq*AllHhhx(659)) -
     &    4*m12sq*MHp2**2*AllHhhx(660) +
     &    2*(MHp2**3*S2B*(S2A + S2B)*SBA2 +
     &       (MW2*SW2)/(Alfa*Pi)*
     &        (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA*AllHhhx(661)))

	AllHhhx(667) = S2B*SBA2*AllHhhx(663) - Mh02**3*AllHhhx(666)

        AllHhhx(668) =
     &   S2B + 2*CBA*SAB - 4*CAB*SBA - (5*S2A + 4*S2B)*SBA2

        AllHhhx(670) =
     &   m12sq*AllHhhx(669) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA*(1 - 6*SBA2))

        AllHhhx(679) =
     &   -4*CAB*MHp2*(m12sq + Mh02*S2B)*SBA + CBA2*AllHhhx(678)

	AllHhhx(691) = 2*CBA2*Mh02 - MHp2*(1 - SBA2)

	AllHhhx(693) = 2*MHp2*(1 - SBA2) + Mh02*(1 - 6*CBA2 - SBA2)

        AllHhhx(696) =
     &   -2*CB2*Lambda7*MHp2*SB2**2*(1 - SBA2) + SB2*AllHhhx(695)

        AllHhhx(698) =
     &   -(CAB*(2*m12sq - Mh02*S2B)*AllHhhx(697)) +
     &    (2*MW2*SW2)/(Alfa*Pi)*(CB2*CBA*CBA2*Lambda6*Mh02)

	AllHhhx(7) = 2*MA02*S2B - (Mh02 - MHp2)*(S2A + 2*S2B)

        AllHhhx(703) =
     &   -((MA02 - Mh02)*(MA02 - MHp2)*MHp2*SBA2) -
     &    CBA2*(Mh02*AllHhhx(680) - 2*AllHhhx(702))

        AllHhhx(707) =
     &   CBA2*(MA02 - Mh02)*Mh02*(MA02 - MHp2) - SBA2*AllHhhx(706)

        AllHhhx(71) = CAB*Mh02*(Mh02 - 2*MHp2)*S2B +
     &    2*m12sq*AllHhhx(70)

        AllHhhx(710) =
     &   CBA2*(MA02 - Mh02)*(MA02 - Mh02 - MHp2) +
     &    (MA02 - MHp2)*MHp2*(1 - SBA2)

        AllHhhx(712) =
     &   (CBA2*(MA02 - Mh02 - MHp2))/MHp2 -
     &    (2*(MA02 - MHp2)*SBA2)/(MA02 - Mh02)

        AllHhhx(713) =
     &   (CBA2*(MA02 - Mh02)*(MA02 - Mh02 + MHp2))/
     &     ((MA02 - MHp2)*MHp2) - ((MA02 + Mh02 - MHp2)*SBA2)/Mh02

        AllHhhx(714) =
     &   (CBA2*(MA02 - Mh02 + MHp2))/MHp2 -
     &    ((MA02 - MHp2)*(MA02 + Mh02 - MHp2)*SBA2)/
     &     ((MA02 - Mh02)*Mh02)

        AllHhhx(715) =
     &   (2*CBA2*(MA02 - Mh02))/(MA02 - MHp2) -
     &    ((MA02 - Mh02 - MHp2)*SBA2)/Mh02

	AllHhhx(718) = S2B**2*SBA*AllHhhx(716) + 2*Mh02*AllHhhx(717)

        AllHhhx(72) = 8*Mh02**2 - 6*Mh02*MHp2 + MHp2**2 +
     &    6*(Mh02 - MHp2)**2*SBA2

        AllHhhx(723) =
     &   Mh02*(MA02 - MHp2)*AllHhhx(721) -
     &    (MA02 - Mh02)*MHp2*AllHhhx(722)

        AllHhhx(730) =
     &   (Mh02 + MHp2)*S2B*AllHhhx(727) -
     &    2*(2*MA02 - Mh02 - MHp2)*AllHhhx(729)

        AllHhhx(734) =
     &   (11 - 3*CBA2)*MA02**2*Mh02 + (9 + CBA2)*Mh02**3 +
     &    (2 + CBA2)*(MA02**3 + MA02*Mh02**2)

        AllHhhx(735) =
     &   (6 + CBA2)*MA02**2 + 3*(3 - CBA2)*MA02*Mh02 +
     &    (9 + 2*CBA2)*Mh02**2

        AllHhhx(739) =
     &   CBA2*(MA02 - Mh02)**2*(MA02 - MHp2)**2*(Mh02 - MHp2) +
     &    MHp2*SBA2*AllHhhx(738)

        AllHhhx(751) =
     &   2*(MA02 - Mh02)**2*(MA02 - MHp2)**2*(Mh02 - MHp2)*SBA2 +
     &    CBA2*Mh02*AllHhhx(750)

        AllHhhx(756) =
     &   (MA02 - MHp2)**2*(Mh02 - MHp2)**2*SBA2 - Mh02*AllHhhx(755)

        AllHhhx(76) = m12sq*(3*S2A - S2B) +
     &    (6*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHhhx(766) =
     &   18*S2A + 27*S2B - 54*CBA*SAB - 190*CAB*SBA -
     &    (8*S2A - 95*S2B)*SBA2

        AllHhhx(767) =
     &   -9*(S2A + S2B) + 18*CBA*SAB + (4*S2A - 39*S2B)*SBA2

	AllHhhx(768) = S2A + S2B - 2*CBA*SAB - (S2A - 2*S2B)*SBA2

        AllHhhx(769) =
     &   6*CBA*SAB + 10*CAB*SBA - 2*S2A*(1 - SBA2) -
     &    S2B*(3 + 5*SBA2)

	AllHhhx(770) = S2A + S2B - 2*CBA*SAB + 2*S2B*SBA2

        AllHhhx(771) =
     &   2*S2A - 6*CBA*SAB - 30*CAB*SBA + 3*S2B*(1 + 5*SBA2)

        AllHhhx(772) =
     &   18*CBA*SAB + 40*CAB*SBA - S2A*(6 - 5*SBA2) -
     &    S2B*(9 + 20*SBA2)

	AllHhhx(774) = -2*S2A*(6 - 5*SBA2) + 3*AllHhhx(773)

	AllHhhx(776) = 2*CBA*SAB*SBA2 - S2B*(29 - 31*SBA2 + 2*SBA2**2)

        AllHhhx(778) =
     &   -2*m12sq*AllHhhx(777) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*(15 - 14*SBA2))

	AllHhhx(779) = 2*CBA*SAB*SBA2 + S2B*(3 - 4*SBA2 + SBA2**2)

        AllHhhx(781) =
     &   -2*m12sq*AllHhhx(780) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*(2 - 3*SBA2))

        AllHhhx(782) =
     &   40*CBA*SAB*SBA2 - S2B*(78 - 67*SBA2 - 11*SBA2**2)

        AllHhhx(783) =
     &   -2*m12sq*(7*C2B*SBA + 2*SAB*SBA2) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*(7 - 2*SBA2))

	AllHhhx(786) = -3 + 2*SBA2 + SBA2**2 + 8*SW2 - 8*SW2**2

        AllHhhx(787) =
     &   1 + CBA2**2 + CBA2*SBA2 - SBA2**2 - 8*SW2 + 8*SW2**2

	AllHhhx(788) = -10 + 7*SBA2 + 3*SBA2**2 + 24*SW2 - 24*SW2**2

	AllHhhx(79) = Mh02*(S2A + 4*S2B) - MHp2*(S2A + 6*S2B)

        AllHhhx(794) =
     &   4*MA02**2*S2B*SBA2 +
     &    MA02*(2*MHp2*AllHhhx(790) - Mh02*AllHhhx(791)) -
     &    MHp2*(2*MHp2*AllHhhx(792) + Mh02*AllHhhx(793))

        AllHhhx(799) =
     &   (MA02 - MHp2)*MHp2*S2B**2*SBA2**2 + 2*CBA*AllHhhx(798)

	AllHhhx(803) = MA02**5*MHp2*(10 - 19*SBA2) + 6*MA02**6*SBA2

	AllHhhx(804) = S2B*(10 - 27*SBA2) + 12*S2A*SBA2

        AllHhhx(805) =
     &   -2*S2B**2*(48 - 59*SBA2) +
     &    3*(207*S2A**2 - 44*S2A*S2B)*SBA2

        AllHhhx(806) =
     &   S2B**2*(1 - 6*SBA2) - 3*(15*S2A**2 - 7*S2A*S2B)*SBA2

        AllHhhx(807) =
     &   2*S2B**2*(8 - 9*SBA2) - 3*(27*S2A**2 - 4*S2A*S2B)*SBA2

        AllHhhx(808) =
     &   S2B**2*(47 - 4*SBA2) - 6*(9*S2A**2 + 16*S2A*S2B)*SBA2

        AllHhhx(81) = -(CAB*(2*m12sq - Mh02*S2B)) +
     &    (MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

        AllHhhx(836) =
     &   -96*MA02**5*S2B**2*SBA2*AllHhhx(300) +
     &    16*MA02**4*MHp2*AllHhhx(813) - MHp2**5*AllHhhx(818) +
     &    8*MA02**3*MHp2**2*AllHhhx(823) +
     &    2*MA02*MHp2**4*AllHhhx(829) -
     &    3*MA02**2*MHp2**3*AllHhhx(835)

	AllHhhx(888) = S2B**2*(40 - 129*SBA2) + 48*S2A**2*SBA2

	AllHhhx(889) = 39*S2A**2*SBA2 - S2B**2*(4 + SBA2)

        AllHhhx(895) =
     &   MHp2*S2B**3*SAB*(76 + 5*SBA2 - 576*SBA2**2) +
     &    (MW2*SW2)/(Alfa*Pi)*(SBA*AllHhhx(894))

        AllHhhx(897) =
     &   52*MA02**4*SBA2*AllHhhx(300) + MHp2**4*AllHhhx(896)

        AllHhhx(908) =
     &   -52*MA02**4*SBA2 -
     &    MA02**2*MHp2**2*(76 + 5*SBA2 - 576*SBA2**3) -
     &    MHp2**4*(4 - 2*SBA2 - 27*SBA2**3) + 2*AllHhhx(907)

        AllHhhx(909) =
     &   2*MHp2**2*S2B**2*SAB**2*(76 - 571*SBA2) -
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     (MHp2*S2B*SAB*(CB2*Lambda6 - Lambda7*SB2)*SBA*
     &       (76 + 5*SBA2 - 576*SBA2**2)) +
     &    (MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (SBA2*(76 + 5*SBA2 - 576*SBA2**3)*AllHhhx(62))

        AllHhhx(91) = MHp2*S2B**3*SAB*(2 + 3*SBA2) -
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (SB2*SBA*(8 - 3*(4 - 3*SBA2)*SBA2)*AllHhhx(58))

        AllHhhx(910) =
     &   2*MHp2**2*S2B**2*SAB**2*(4 - 29*SBA2) -
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     (MHp2*S2B*SAB*(CB2*Lambda6 - Lambda7*SB2)*SBA*
     &       (4 - 2*SBA2 - 27*SBA2**2)) +
     &    (MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (SBA2*(4 - 2*SBA2 - 27*SBA2**3)*AllHhhx(62))

        AllHhhx(954) =
     &   CBA2**2*Mh02*S2B**2*AllHhhx(949) +
     &    2*CBA*(MA02 - Mh02)*(Mh02 - MHp2)*MHp2*AllHhhx(953)

        AllHhhx(104) =
     &   -3*(15*S2A**2 - 76*S2A*S2B)*SBA2 + 8*S2B**2*(12 + 7*SBA2)

        AllHhhx(1057) =
     &   2*CBA2**2*AllHhhx(1007) +
     &    (MA02 - Mh02)*(Mh02 - MHp2)*AllHhhx(1019) +
     &    CBA2*AllHhhx(1056)

	AllHhhx(1060) = 4*MA02**2 - 5*MA02*Mh02 + Mh02**2

        AllHhhx(1149) =
     &   ((MA02 - Mh02)*MHp2)/(Mh02*(MA02 - MHp2)) +
     &    ((MA0 - MHp)*(MA0 + MHp)*AllHhhx(1148))/MHp2

        AllHhhx(1157) =
     &   CAB*CBA*Mh02*(MA02 + Mh02 - 2*MHp2)*S2B**2 -
     &    S2B*AllHhhx(1151)*AllHhhx(1152) + CB2*AllHhhx(1156)

        AllHhhx(1166) =
     &   (2*MA02 - Mh02 - MHp2)/(MA02 - MHp2) +
     &    (2*CBA2*(Mh02 - MHp2))/(MA02 - Mh02)

        AllHhhx(1168) =
     &   (-2*CBA2*Mh02*(Mh02 - MHp2))/(MA02 - Mh02) +
     &    AllHhhx(1167)/(MA02 - MHp2)

        AllHhhx(1176) =
     &   (Mh02**3*SBA2*AllHhhx(1169))/((MA02 - Mh02)*MHp2) +
     &    (2*Mh02*(MA02*AllHhhx(1171) - 2*AllHhhx(1175)))/
     &     (MA02 - MHp2)

        AllHhhx(1178) =
     &   AllHhhx(707)/((MA02 - Mh02)*(MA02 - MHp2)) +
     &    SBA2*AllHhhx(1177)

        AllHhhx(1180) =
     &   -(CBA2*Mh02*(MA02 - 2*Mh02 + MHp2)) +
     &    2*(MA02 - MHp2)**2*SBA2

        AllHhhx(1203) =
     &   -4*CAB*AllHhhx(1199) + Mh02*SBA*AllHhhx(1201) +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*AllHhhx(1202))

        AllHhhx(123) =
     &   8*S2B**2*(6 - 5*SBA2) - 3*(33*S2A**2 - 4*S2A*S2B)*SBA2

        AllHhhx(1256) =
     &   CBA*MHp2*S2B**2 -
     &    2*((2*m12sq - MHp2*S2B)*SAB +
     &       (MW2*SW2)/(Alfa*Pi)*((CB2*Lambda6 - Lambda7*SB2)*SBA))

        AllHhhx(1268) =
     &   Mh02*S2B**3*AllHhhx(1260) - CB2*AllHhhx(1267) +
     &    (3*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*Lambda7*S2B**2*SB2*SBA*AllHhhx(1262))

        AllHhhx(1350) =
     &   CBA*MHp2*S2B*(3*S2A + S2B) + 4*SAB*AllHhhx(289) -
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*(1 - 3*SBA2))

        AllHhhx(145) =
     &   CBA2*Mh02**2*S2B**2 + 4*Mh02*SBA*AllHhhx(44) -
     &    S2B*SBA2*AllHhhx(144)

        AllHhhx(153) =
     &   S2B**2*AllHhhx(147) + 2*MA02**2*Mh02*AllHhhx(148) -
     &    MA02*Mh02**2*(Mh02*S2B*AllHhhx(149) + 2*AllHhhx(150)) -
     &    Mh02**3*(Mh02*S2B*AllHhhx(151) - 4*AllHhhx(152))

        AllHhhx(157) =
     &   MA02*S2B*SBA2*AllHhhx(155) +
     &    Mh02*(24*C2B*CBA*m12sq*SBA -
     &       Mh02*S2B*SBA2*AllHhhx(154) + 2*AllHhhx(156))

        AllHhhx(1585) =
     &   S2B*SAB*SBA*AllHhhx(1583) +
     &    (3*MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*AllHhhx(1584))

        AllHhhx(1590) =
     &   (2*m12sq - Mh02*S2B)*(2*m12sq - MHp2*S2B)*SAB -
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*AllHhhx(1589))

        AllHhhx(1597) =
     &   -2*MHp2*S2B*SAB*SBA*AllHhhx(1591) -
     &    3*Mh02*AllHhhx(1596) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*AllHhhx(1594))

	AllHhhx(16) = MA02*(Mh02 - MHp2) + Mh02*(2*Mh02 + MHp2)

	AllHhhx(1609) = Mh02**2 - 2*Mh02*MHp2 + MHp2**2

	AllHhhx(1610) = MA02*(Mh02 - MHp2) + 2*Mh02*MHp2 - 2*MHp2**2

	AllHhhx(1611) = -3*(Mh02 + MHp2)*S2A + (Mh02 - MHp2)*S2B

	AllHhhx(1614) = 6*m12sq*MHp2*SBA2**2 + SBA2*AllHhhx(1613)

        AllHhhx(1617) =
     &   (CB2*Lambda6 - Lambda7*SB2)*
     &    (3*MHp2*SBA2**2 + AllHhhx(1616))

	AllHhhx(1619) = (Mh02 - MHp2)*S2B**2*SBA2 - 2*SBA*AllHhhx(81)

	AllHhhx(1620) = 2*m12sq*(3*S2A - S2B) + (Mh02 - MHp2)*S2A*S2B

        AllHhhx(1621) =
     &   -(CAB*(2*CBA2*m12sq - Mh02*S2B)) +
     &    (MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

	AllHhhx(1623) = (Mh02*MHp2 - MHp2**2)*(S2A*S2B + 2*S2B**2)

        AllHhhx(1624) =
     &   2*CAB*(2*m12sq - Mh02*S2B)*SBA - m12sq*(3*S2A + S2B)*SBA2

        AllHhhx(1628) =
     &   SAB*AllHhhx(1626) +
     &    (MW2*SW2)/(Alfa*Pi)*(MHp2*SBA*AllHhhx(1627))

	AllHhhx(1633) = -2*m12sq*(3*S2A + S2B) + S2B*AllHhhx(1632)

	AllHhhx(1637) = CBA*(3*S2A - 2*S2B) - 10*SAB

        AllHhhx(1645) =
     &   (Mh02 - 2*MHp2)**2*S2B**4*SBA2 +
     &    4*CAB*(2*m12sq - Mh02*S2B)*AllHhhx(1643) +
     &    (2*MW2**2*SW2**2)/(Alfa2*Pi**2)*(CBA2*AllHhhx(62)) -
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*AllHhhx(1644))

        AllHhhx(1649) =
     &   CBA2*MHp2**2*S2B**4 + 4*AllHhhx(1648) +
     &    (2*MW2**2*SW2**2)/(Alfa2*Pi**2)*(SBA2*AllHhhx(62))

        AllHhhx(1684) =
     &   4*CAB**2*(2*m12sq - Mh02*S2B)**2 + S2B**4*AllHhhx(1681) -
     &    4*AllHhhx(1683) +
     &    (2*MW2**2*SW2**2)/(Alfa2*Pi**2)*AllHhhx(62)

        AllHhhx(1702) =
     &   2*CBA2*(MA02 - Mh02)*(MA02 + Mh02)*AllHhhx(1701) +
     &    3*SBA2*AllHhhx(1700)*Re(AllHhhl(15))

        AllHhhx(1709) =
     &   2*CBA2*AllHhhx(1701) + 3*Mh02**2*SBA2*Re(AllHhhl(15))

	AllHhhx(1716) = 2*CAB**2*(CBA2 + 9*CBA2**3) + 9*CBA2**2*S2A**2

	AllHhhx(1719) = 20*CAB**2*CBA2*Mh02**2 + CBA2**2*AllHhhx(1718)

        AllHhhx(1725) =
     &   -12*CBA2**2*m12sq**2*(3*S2A - S2B) +
     &    2*m12sq*AllHhhx(1721) + CBA2*AllHhhx(1723) -
     &    6*CBA*SAB*AllHhhx(1724)

        AllHhhx(1739) =
     &   -((-(Lambda6*Lambda7*S2B**2) + 2*Lambda7**2*SB2**2)*
     &       AllHhhx(1736)) -
     &    6*(CB2**2*(CBA2**2 + 3*CBA2**4)*Lambda6**2 +
     &       SBA2*AllHhhx(1737)) - 9*CBA2**2*SBA2**2*AllHhhx(1738)

        AllHhhx(1751) =
     &   -(CBA2*AllHhhx(1750)) +
     &    (2*MW2**2*SW2**2)/(Alfa2*Pi**2)*(CBA2**2*AllHhhx(1738))

        AllHhhx(1756) =
     &   CBA2*Mh02**2*S2A**2*S2B**2*SBA2 +
     &    4*CAB**2*AllHhhx(1752) +
     &    (2*MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (CBA2**4*AllHhhx(1738)) -
     &    4*(CAB*S2A*SBA*AllHhhx(1753) +
     &       (MW2*SW2)/(Alfa*Pi)*(CBA*AllHhhx(1755)))

        AllHhhx(1759) =
     &   -(CBA2**2*AllHhhx(1758)) +
     &    (18*MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (CBA2**3*SBA2*AllHhhx(1738))

        AllHhhx(1775) =
     &   (CBA2 - CBA2**2)*MA02**2 - CBA2*AllHhhx(1771) -
     &    2*((CBA2*MA02 - CBA2**2*Mh02)*MHp2 +
     &       CBA2**2*AllHhhx(1774))

        AllHhhx(1777) =
     &   -2*m12sq**2*S2B**2 +
     &    6*S2B*(m12sq*MHp2*S2A**2 + 2*AllHhhx(1776)) +
     &    (2*MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (AllHhhx(1738)*Re(AllHhhl(6))) +
     &    MHp2**2*S2B**4*Re(AllHhhl(7))

        AllHhhx(1788) =
     &   MHp2**2*S2B**4 - SBA2*AllHhhx(1782) -
     &    4*SAB*AllHhhx(1784) - 2*AllHhhx(1785)*Re(AllHhhl(6)) +
     &    4*(SAB**2*AllHhhx(1778) +
     &       (MW2**2*SW2**2)/(Alfa2*Pi**2)*(CB2**2*Lambda6**2) +
     &       AllHhhx(1787)*Re(AllHhhl(7)))

        dup1 = AllHhhx(1645)*Re(AllHhhl(6)) +
     &    AllHhhx(1649)*Re(AllHhhl(7))

	AllHhhx(1799) = CBA2*dup1 + S2B**4*AllHhhx(1798)

        AllHhhx(1801) =
     &   2*CBA2**2*AllHhhx(1701) +
     &    3*CBA2*Mh02**2*SBA2*Re(AllHhhl(15))

	AllHhhx(186) = 2*CAB*Mh02*SBA - (Mh02 - MHp2)*S2B*SBA2

        AllHhhx(193) =
     &   6*MA02**3*S2B**2*(3 + SBA2) -
     &    MA02*Mh02*(Mh02*S2B*AllHhhx(188) + 4*AllHhhx(189)) +
     &    Mh02**2*(4*AllHhhx(152) - Mh02*S2B*AllHhhx(190)) +
     &    2*MA02**2*(Mh02*S2B*AllHhhx(191) + 4*AllHhhx(192))

        AllHhhx(194) =
     &   24*C2B*CBA*m12sq*SBA + 2*AllHhhx(156) -
     &    Mh02*S2B*SBA2*AllHhhx(172)

        AllHhhx(21) = -4*MA02*S2B*(1 - SBA2) +
     &    4*Mh02*AllHhhx(19) - MHp2*AllHhhx(20)

        AllHhhx(215) =
     &   CBA2**2*S2B**4*AllHhhx(203) +
     &    CBA*(MA02 - Mh02)*(Mh02 - MHp2)*
     &     (SAB*SBA2*AllHhhx(205) + SBA*AllHhhx(214))

        AllHhhx(262) =
     &   CBA2*Mh02*AllHhhx(260) + (Mh02 - MHp2)*SBA2*AllHhhx(261)

        AllHhhx(294) =
     &   -16*MA02**4*MHp2**2*(15 - 17*SBA2) +
     &    16*MA02**5*MHp2*(8 - 13*SBA2) + SBA2*AllHhhx(293)

	AllHhhx(295) = S2B**2*(74 - 63*SBA2) + 36*S2A**2*SBA2

	AllHhhx(296) = 5*S2B**2*(8 - 3*SBA2) + 36*S2A**2*SBA2

	AllHhhx(297) = 9*S2A**2*SBA2 + S2B**2*(8 + 9*SBA2)

        AllHhhx(311) =
     &   2*S2B**2*SBA2*AllHhhx(299)*AllHhhx(300) -
     &    2*MA02**2*MHp2**2*AllHhhx(304) +
     &    16*MA02**3*MHp2*AllHhhx(307) + MA02*MHp2**3*AllHhhx(310)

	AllHhhx(32) = 2*MA02**2 - 10*MA02*Mh02 + 5*Mh02**2

        AllHhhx(33) = MA02**5 - 4*MA02**4*Mh02 +
     &    4*MA02**3*Mh02**2 + 2*MA02*Mh02**4

        AllHhhx(341) =
     &   (2*m12sq - MHp2*S2B)*SAB +
     &    (MW2*SW2)/(Alfa*Pi)*((CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHhhx(344) =
     &   SAB*AllHhhx(343) -
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*(1 - 3*SBA2**2))

        AllHhhx(346) =
     &   8*MA02**2 + 4*MA02*(3*Mh02 - 2*MHp2) - 3*Mh02*MHp2 +
     &    2*MHp2**2

        AllHhhx(358) =
     &   8*MA02**5*S2B*(1 - 2*SBA2) -
     &    2*MA02**2*MHp2**2*
     &     (MHp2*AllHhhx(347) - Mh02*AllHhhx(348)) -
     &    MHp2**4*(4*Mh02*AllHhhx(349) + MHp2*AllHhhx(350)) -
     &    MA02*MHp2**3*(Mh02*AllHhhx(351) - 2*MHp2*AllHhhx(352)) +
     &    MA02**3*MHp2*(2*MHp2*AllHhhx(353) - Mh02*AllHhhx(354)) +
     &    2*MA02**4*AllHhhx(357)

        AllHhhx(36) = -2*CAB*m12sq +
     &    (MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

        AllHhhx(371) =
     &   -6*MHp2**3 - 2*MA02**2*MHp2*(3 - SBA2) + MA02**3*SBA2

	AllHhhx(372) = S2B*(16 - SBA2) + 6*S2A*SBA2

        AllHhhx(375) =
     &   2*SAB*AllHhhx(374) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*AllHhhx(373))

	AllHhhx(377) = MA02*MHp2*(3 - SBA2) - MA02**2*SBA2

	AllHhhx(378) = S2B*(3 - 2*SBA2) + 3*S2A*SBA2

        AllHhhx(385) =
     &   12*CAB*CBA2*Mh02*SBA +
     &    (MA02 - MHp2)*(S2B - S2B*SBA2**2) - 6*SBA2*AllHhhx(384)

        AllHhhx(4) = CBA2*(MA02 + MHp2)**2*S2B +
     &    2*MHp2**2*(3*CBA2*S2A + S2B)

        AllHhhx(440) =
     &   4*S2B*AllHhhx(424) - 2*Mh02**2*MHp2**2*AllHhhx(427) -
     &    Mh02**3*MHp2*AllHhhx(430) - 8*MHp2**4*AllHhhx(432) +
     &    AllHhhx(439)

	AllHhhx(49) = MA02**2 - 5*MA02*MHp2 + MHp2**2

        AllHhhx(492) =
     &   -16*MHp2**6*S2B**6*SBA2 - 4*Mh02**6*S2B**5*AllHhhx(445) -
     &    Mh02**5*(MHp2*S2B**5*AllHhhx(446) + 16*AllHhhx(456)) -
     &    2*Mh02**3*MHp2**2*
     &     (MHp2*S2B**5*AllHhhx(457) - 2*AllHhhx(465)) +
     &    4*Mh02*MHp2**4*(MHp2*S2B**5*AllHhhx(466) +
     &       2*AllHhhx(472)) + 2*AllHhhx(491)

	AllHhhx(50) = 6*MA02**2 - 6*MA02*MHp2 + MHp2**2

        AllHhhx(549) =
     &   4*S2B**4*AllHhhx(441)*
     &     (CBA2**2*AllHhhx(497) + MHp2*AllHhhx(502)) +
     &    CBA2*AllHhhx(548)

        AllHhhx(556) =
     &   CBA2**2*(Mh02 - MHp2)**2*S2B**6*AllHhhx(415)*
     &     AllHhhx(550) - MHp2*S2B**4*AllHhhx(441)*AllHhhx(555)

        AllHhhx(56) = 2*m12sq*SAB +
     &    (MW2*SW2)/(Alfa*Pi)*((CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHhhx(598) =
     &   8*Mh02**6*S2B**5*AllHhhx(557) - 4*MHp2**5*AllHhhx(561) -
     &    Mh02**3*MHp2**2*(MHp2*S2B**5*AllHhhx(562) -
     &       4*AllHhhx(569)) + 2*AllHhhx(586) - 2*AllHhhx(597)

	AllHhhx(601) = S2B - (3*S2A - S2B)*SBA2

        AllHhhx(602) =
     &   3*(3*S2A**2 - 2*S2A*S2B)*SBA2 + S2B**2*(4 + 3*SBA2)

        AllHhhx(605) =
     &   MHp2**2*S2B**3*SAB - MHp2*S2B**2*AllHhhx(604) +
     &    (3*MW2*SW2)/(2.D0*Alfa*Pi)*
     &     (MA02*SB2*SBA*SBA2*(1 - (2 - 3*SBA2)*SBA2)*AllHhhx(58))

	AllHhhx(635) = MHp2*S2B*(CBA*S2A - 2*SAB) + 2*SBA2*AllHhhx(56)

        AllHhhx(638) =
     &   4*m12sq*SAB - MHp2*S2B*(CBA*S2B + 2*SAB) +
     &    (2*MW2*SW2)/(Alfa*Pi)*((CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHhhx(655) =
     &   -2*CAB*MHp2*(Mh02 + 2*MHp2)*(2*m12sq - Mh02*S2B)*SBA -
     &    SBA2*AllHhhx(649) + 2*CBA*AllHhhx(651) -
     &    CBA2*(Mh02 - 4*MHp2)*AllHhhx(654)

        AllHhhx(671) =
     &   Mh02*AllHhhx(662) + AllHhhx(667) +
     &    Mh02**2*MHp2*(MHp2*S2B*AllHhhx(668) - 4*AllHhhx(670))

        AllHhhx(692) =
     &   -2*MA02**2*(1 - 2*CBA2 - SBA2) + Mh02*AllHhhx(691)

        AllHhhx(694) =
     &   S2B**4*SBA2*AllHhhx(693) -
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*S2B**2*(CB2*Lambda6 - Lambda7*SB2)*SBA*
     &       (1 - 2*CBA2 - SBA2))

        AllHhhx(699) =
     &   -(S2B**2*AllHhhx(698)) +
     &    (2*MW2*SW2)/(Alfa*Pi)*(CBA*AllHhhx(696))

        AllHhhx(701) =
     &   -2*CAB*(2*m12sq - Mh02*S2B) -
     &    (Mh02 - 2*MHp2)*S2B**2*SBA +
     &    (2*MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

        AllHhhx(711) =
     &   CBA2**2*(MA02 - Mh02)**2 -
     &    ((MA02 - MHp2)*SBA2*AllHhhx(710))/Mh02

        AllHhhx(724) =
     &   -2*Mh02*(MA02 - MHp2)**2 + 2*CBA2*MA02*(Mh02 - MHp2)**2 +
     &    (SBA2*AllHhhx(723))/(MA02 - MHp2)**2

        AllHhhx(73) = -((4*Mh02 - MHp2)*AllHhhx(71)) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*AllHhhx(72))

        AllHhhx(731) =
     &   3*Mh02**2 - Mh02*(MA02 + 2*MHp2) -
     &    (SBA2*AllHhhx(730))/((MA02 - Mh02)*Mh02*MHp2*S2B**2)

        AllHhhx(733) =
     &   ((MA02 - Mh02)**2*(MA02 - MHp2))/MHp2 -
     &    ((MA02 - Mh02)*(MA02 + Mh02)*MHp2)/(MA02 - MHp2)

        AllHhhx(736) =
     &   (2 - CBA2)*MA02**2*Mh02**2 + CBA2*MA02*Mh02**3 +
     &    3*(MA02**3*Mh02 + Mh02**4) - 4*(MA02 + Mh02)*MHp2**3 +
     &    MHp2*(2*(MA02 - Mh02)*(MA02 - MHp2)**2*SBA2 -
     &       AllHhhx(734)) + MHp2**2*AllHhhx(735)

        AllHhhx(740) =
     &   2*m12sq*(3*S2A - S2B) - (2*Mh02 + MHp2)*S2A*S2B +
     &    (2*MA02 - MHp2)*S2B**2

        AllHhhx(75) = Mh02**3 - 3*Mh02**2*MHp2 -
     &    Mh02*MHp2**2 - MHp2**3

        AllHhhx(752) =
     &   2*CBA2**2*MA02*(Mh02 - MHp2)**2 +
     &    AllHhhx(751)/(Mh02*(Mh02 - MHp2))

        AllHhhx(77) = (Mh02 - MHp2)*(2*Mh02**2 + MHp2**2)*S2A*
     &     S2B + 2*(Mh02 - MHp2)**2*AllHhhx(76)

        AllHhhx(775) =
     &   24*MA02**4*S2B*SBA2 +
     &    MA02**2*MHp2*(Mh02*AllHhhx(766) + 2*MHp2*AllHhhx(767)) -
     &    MHp2**3*(2*MHp2*AllHhhx(768) + Mh02*AllHhhx(769)) +
     &    4*MA02**3*(2*MHp2*AllHhhx(770) - Mh02*AllHhhx(771)) +
     &    MA02*MHp2**2*(2*Mh02*AllHhhx(772) - MHp2*AllHhhx(774))

        AllHhhx(784) =
     &   4*MA02**3*S2B**2*(13 - 10*SBA2 - 3*SBA2**2) -
     &    MA02*MHp2*(MHp2*S2B*AllHhhx(776) - 2*CBA*AllHhhx(778)) -
     &    MHp2**2*(MHp2*S2B*AllHhhx(779) + 2*CBA*AllHhhx(781)) +
     &    MA02**2*(MHp2*S2B*AllHhhx(782) - 8*CBA*AllHhhx(783))

        AllHhhx(789) =
     &   MHp2**2*AllHhhx(786) + 4*MA02**2*AllHhhx(787) -
     &    MA02*MHp2*AllHhhx(788)

        AllHhhx(8) = 4*CAB*CBA*Mh02 -
     &    SBA*(6*MHp2*SAB - CBA*AllHhhx(7))

        AllHhhx(80) = 8*CAB*CBA*Mh02 -
     &    SBA*(10*MHp2*SAB + CBA*AllHhhx(79))

        AllHhhx(800) =
     &   CBA2**2*(3*Mh02 - 2*MHp2)*(MA02 - MHp2)*S2B**2 -
     &    CBA2*S2B*AllHhhx(794) - 2*AllHhhx(799)

        AllHhhx(809) =
     &   -64*S2B**2*AllHhhx(803) +
     &    32*MA02**4*MHp2**2*S2B*AllHhhx(804) +
     &    MA02*MHp2**5*AllHhhx(805) +
     &    24*MA02**2*MHp2**4*AllHhhx(806) + MHp2**6*AllHhhx(807) +
     &    8*MA02**3*MHp2**3*AllHhhx(808)

        AllHhhx(85) = (2*m12sq - MHp2*S2B)*SAB +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((1 - 3*CBA2)*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHhhx(885) =
     &   MA02**5*MHp2*(8 - 95*SBA2) -
     &    2*MA02**4*MHp2**2*(17 - 71*SBA2) + 26*MA02**6*SBA2

	AllHhhx(886) = -2*S2B**2*(2 - SBA2) + 27*S2A**2*SBA2

	AllHhhx(887) = S2B**2*(4 - 63*SBA2) + 192*S2A**2*SBA2

        AllHhhx(890) =
     &   MA02**3*MHp2**3*AllHhhx(888) + MA02*MHp2**5*AllHhhx(889)

        AllHhhx(9) = -2*C2B*m12sq +
     &    (MW2*SW2)/(Alfa*Pi)*(CB2*Lambda6 - Lambda7*SB2)

	AllHhhx(90) = 9*S2A**2 + 12*S2A*S2B + 8*S2B**2

	AllHhhx(945) = MA02*Mh02 - (MA02 + Mh02)*MHp2 - 2*MHp2**2

        AllHhhx(946) =
     &   MA02**4 + 4*MA02**2*Mh02*MHp2 + 2*Mh02**2*MHp2**2 -
     &    2*MA02**3*(Mh02 + MHp2)

	AllHhhx(1) = 2*MA02**2 - 10*MA02*MHp2 + 5*MHp2**2

	dup2 = S2B*AllHhhx(8) + 2*(2 + SBA2)*AllHhhx(9)

        AllHhhx(10) = -(dup2/(MA02 - MHp2)) +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*(1 - 3*SBA2)*SBA2)/MHp2

        AllHhhx(1147) =
     &   CBA2*Mh02*(MA02 - 2*Mh02 + MHp2) - 2*(MA02 - MHp2)**2*SBA2

        AllHhhx(1150) =
     &   -((Mh02 - MHp2)**2/
     &       ((Mh0 - MHp)*(Mh0 + MHp)*(MA02 - MHp2))) -
     &    SBA2*(1 - AllHhhx(1149))

        AllHhhx(1158) =
     &   -(CBA2**2*(Mh02 - MHp2)) +
     &    (CBA*SBA*AllHhhx(1157))/((MA02 - MHp2)*S2B**3)

        AllHhhx(12) = (Mh02 + 2*MHp2)*S2A*S2B -
     &    2*m12sq*(3*S2A + S2B) -
     &    (6*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

	AllHhhx(14) = 2*m12sq + (2*Mh02 + MHp2)*S2A

        AllHhhx(15) = -(S2B*AllHhhx(14)) +
     &    6*(m12sq*S2A + (MW2*SW2)/(Alfa*Pi)*
     &        (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA))

        AllHhhx(158) =
     &   -4*CBA2**2*(MA02 - Mh02)**2*Mh02*(MA02 + Mh02)*S2B**2 +
     &    CBA2*AllHhhx(153) + MA02*(MA02 - Mh02)*AllHhhx(157)

        AllHhhx(1580) =
     &   (MA02*AllHhhl(1))/(MA02 - MHp2) -
     &    (Mh02*AllHhhl(2))/(Mh02 - MHp2)

	AllHhhx(1582) = 2*MA02**2 - Mh02*MHp2 - 2*MA02*(Mh02 + MHp2)

	AllHhhx(1607) = 3*CBA2**2*Mh02 + CBA2*(MA02 + 2*MHp2)

	AllHhhx(1608) = 3*Mh02**2 + Mh02*(MA02 + 2*MHp2)

        AllHhhx(1612) =
     &   S2A*S2B*AllHhhx(1609) - S2B**2*AllHhhx(1610) -
     &    2*m12sq*AllHhhx(1611)

        AllHhhx(1618) =
     &   SAB*AllHhhx(1614) +
     &    (MW2*SW2)/(Alfa*Pi)*(SBA*AllHhhx(1617))

        AllHhhx(1622) =
     &   (2*CBA*SBA2*AllHhhx(341) - CBA2*AllHhhx(1619))*
     &     Re(AllHhhl(1)) +
     &    CBA2*(SBA2*AllHhhx(1620) + 6*SBA*AllHhhx(1621))*
     &     Re(AllHhhl(2))

        AllHhhx(1629) =
     &   -(CBA2*(SBA2*AllHhhx(1623) + 2*MHp2*AllHhhx(1624))) +
     &    2*CBA*AllHhhx(1628)

        AllHhhx(1634) =
     &   -4*CAB*(2*m12sq - Mh02*S2B) - SBA*AllHhhx(1633) +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*(2 + 3*SBA2))

        AllHhhx(1636) =
     &   CBA*(6*m12sq*S2A - S2B*AllHhhx(14)) +
     &    (6*MW2*SW2)/(Alfa*Pi)*
     &     (CBA2*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHhhx(1638) =
     &   2*(2 + 3*SBA2)*AllHhhx(56) + MHp2*S2B*AllHhhx(1637)

        AllHhhx(1650) =
     &   dup1/S2B**4 + (MA02 - MHp2)**2*Re(AllHhhl(4)) +
     &    CBA2*(Mh02 - MHp2)**2*Re(AllHhhl(5))

        AllHhhx(17) = (MA02*(MA02 - Mh02)*AllHhhl(26)*
     &       AllHhhx(15))/S2B**2 +
     &    (MHp2**2*AllHhhl(28)*AllHhhx(16))/(Mh02 - MHp2)

        AllHhhx(187) =
     &   CBA2**2*(MA02 - Mh02)*S2B**2 -
     &    12*CBA*(MHp2*S2B*SAB*SBA2 - SBA*AllHhhx(9)) +
     &    6*CBA2*S2B*AllHhhx(186)

        AllHhhx(2) = MA02**5 - 4*MA02**4*MHp2 +
     &    4*MA02**3*MHp2**2 + 2*MA02*MHp2**4

        AllHhhx(267) =
     &   m12sq*S2A + (MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHhhx(298) =
     &   2*S2B**2*AllHhhx(294) + 4*MA02**3*MHp2**3*AllHhhx(295) -
     &    2*MA02**2*MHp2**4*AllHhhx(296) +
     &    MA02*MHp2**5*AllHhhx(297)

        AllHhhx(34) = Mh02*AllHhhl(23)*AllHhhx(32) +
     &    (AllHhhl(22)*AllHhhx(33))/Mh02**2

        AllHhhx(342) =
     &   CBA2*(2*MA02 - MHp2)*S2B**2 - 2*CBA*AllHhhx(341)

        AllHhhx(345) =
     &   CBA2*MHp2*S2B*(S2B + 3*S2A*SBA2) + 2*CBA*AllHhhx(344)

        AllHhhx(376) =
     &   -(CBA2*S2B*(S2B*AllHhhx(371) +
     &         MA02*MHp2**2*AllHhhx(372))) +
     &    4*CBA*MHp2*AllHhhx(375)

        AllHhhx(41) = -2*CAB*(2*m12sq - Mh02*S2B) +
     &    (2*MA02 - Mh02)*S2B**2*SBA +
     &    (2*MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2))

        AllHhhx(42) = -2*CAB*(2*CBA2*m12sq - Mh02*S2B) +
     &    Mh02*S2A*S2B*SBA +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*CBA2*(CB2*Lambda6 - Lambda7*SB2))

        AllHhhx(5) = -8*CBA*MHp2*(m12sq + MHp2*S2B)*SAB +
     &    S2B*AllHhhx(4)

        AllHhhx(51) = 2*(4*MA02 - MHp2)*MHp2*AllHhhx(49) +
     &    (MA02 - MHp2)**2*SBA2*AllHhhx(50)

        AllHhhx(52) = CBA*(2*MA02 - MHp2)*S2B**2 -
     &    2*((2*m12sq - MHp2*S2B)*SAB +
     &       (MW2*SW2)/(Alfa*Pi)*((CB2*Lambda6 - Lambda7*SB2)*SBA))

        AllHhhx(599) =
     &   AllHhhx(549)/(Mh02 - MHp2) +
     &    (4*AllHhhl(3)*(2*AllHhhx(556) - CBA2*AllHhhx(598)))/
     &     ((Mh02 - MHp2)**2*MHp2)

	AllHhhx(6) = 1 + (24*CBA*m12sq*SAB)/(MHp2*S2B**2)

        AllHhhx(603) =
     &   -2*S2B**2*(MHp2**3 - 2*MA02**3*SBA2) -
     &    4*MA02**2*MHp2*S2B*AllHhhx(601) +
     &    MA02*MHp2**2*AllHhhx(602)

	AllHhhx(644) = 5*Mh02**2 - 16*Mh02*MHp2 + 2*MHp2**2

        AllHhhx(645) =
     &   10*Mh02**2 + 6*Mh02*MHp2 - 8*MHp2**2 + MHp2**4/Mh02**2

        AllHhhx(656) =
     &   CBA*AllHhhl(36)*AllHhhx(638) -
     &    (AllHhhl(35)*AllHhhx(655))/((Mh02 - 4*MHp2)*MHp2)

	AllHhhx(69) = 4*Mh02**2 - 5*Mh02*MHp2 + MHp2**2

	AllHhhx(690) = CBA2*(MA02 - Mh02) + 2*(MA02 - MHp2)*SBA2

        AllHhhx(700) =
     &   S2B**4*SBA2*AllHhhx(692) + MA02*AllHhhx(694) +
     &    2*SBA*AllHhhx(699)

        AllHhhx(732) =
     &   (2*CBA2*Mh02*(Mh02 - MHp2))/(MA02 - Mh02) +
     &    AllHhhx(724)/(MA02 - Mh02)**2 -
     &    AllHhhx(731)/(MA02 - MHp2)

        AllHhhx(74) = CBA2*(Mh02*S2B**2 - 12*SBA*AllHhhx(36))*
     &     AllHhhx(69) + 4*SBA*AllHhhx(73)

        AllHhhx(78) = (4*Mh02 - MHp2)*S2B**2*AllHhhx(75) -
     &    4*Mh02*AllHhhx(77)

        AllHhhx(785) =
     &   CBA2**2*(MA02 - MHp2)**2*S2B**2*AllHhhx(346) -
     &    CBA2*S2B*AllHhhx(775) + 2*(MA02 - MHp2)*AllHhhx(784)

        AllHhhx(801) =
     &   -4*MA02*(CBA2 - 4*SW2**2) -
     &    2*((1 - 2*CBA2)*MHp2 + AllHhhx(789)/(4*MA02 - MHp2)) -
     &    AllHhhx(800)/((MA02 - MHp2)*S2B**2) -
     &    16*SW2*(MA02 + CW2*MHp2)

        AllHhhx(891) =
     &   8*S2B**2*AllHhhx(885) - MHp2**6*AllHhhx(886) -
     &    3*MA02**2*MHp2**4*AllHhhx(887) + 6*AllHhhx(890)

        AllHhhx(947) =
     &   (Mh02**2*AllHhhl(23)*AllHhhx(945))/(Mh02 - MHp2) +
     &    (MA02*AllHhhl(22)*AllHhhx(946))/(Mh02*(MA02 - MHp2))

        AllHhhx(11) = 3 + (4*CBA2*(MA02 - MHp2))/MHp2 -
     &    (SBA2*AllHhhx(5))/(MHp2**2*S2B**2) -
     &    SBA2**2*AllHhhx(6) + (2*CBA*SBA*AllHhhx(10))/S2B**2

        AllHhhx(13) = 1 +
     &    ((MA02 - Mh02)*SBA2*AllHhhx(12))/
     &     ((MA02 - MHp2)*MHp2*S2B**2)

        AllHhhx(1581) =
     &   1 + 2*((MHp2*AllHhhl(3))/((MA02 - MHp2)*(Mh02 - MHp2)) +
     &       AllHhhx(1580)/(MA02 - Mh02))

	dup3 = 2*m12sq*AllHhhx(1607) - CBA2*S2B*AllHhhx(1608)

        AllHhhx(1630) =
     &   -2*CAB*dup3*SBA + CBA2*SBA2*AllHhhx(1612) +
     &    2*(CBA*AllHhhx(1618) + AllHhhx(1622) +
     &       (AllHhhx(1629)*Re(AllHhhl(3)))/MHp2)

        AllHhhx(1631) =
     &   -1 + (2*(AllHhhl(1) - AllHhhl(2)))/(MA02 - Mh02) -
     &    2*((AllHhhl(1) - AllHhhl(3))/(MA02 - MHp2) +
     &       (AllHhhl(2) - AllHhhl(3))/(Mh02 - MHp2))

        AllHhhx(18) = AllHhhl(1)**2 -
     &    (((MA0 - MHp)*(MA0 + MHp)*AllHhhl(2)**2*AllHhhx(15))/
     &        ((MA02 - Mh02)*S2B**2) + AllHhhx(17)/(MA02 - MHp2))/
     &     Mh02

        AllHhhx(3) = MHp2*AllHhhl(28)*AllHhhx(1) +
     &    (AllHhhl(22)*AllHhhx(2))/MHp2**2

	AllHhhx(48) = 4*MA02**2 - 5*MA02*MHp2 + MHp2**2

        AllHhhx(802) =
     &   -(AllHhhx(785)/
     &       ((MA02 - MHp2)**2*(4*MA02 - MHp2)*S2B**2)) +
     &    (2*AllHhhl(3)*AllHhhx(801))/MHp2

        AllHhhx(1514) =
     &   2*CBA2*MHp2*AllHhhx(12)*AllHhhx(15) +
     &    (Mh02 - MHp2)*AllHhhx(701)**2

        AllHhhx(1446) =
     &   SBA2*AllHhhx(1160) -
     &    SBA*(2*CAB*(2*m12sq - Mh02*S2B) -
     &       (2*MW2*SW2)/(Alfa*Pi)*
     &        (CBA*(CB2*Lambda6 - Lambda7*SB2)*(1 - 3*SBA2)))

        AllHhhx(1556) =
     &   2*(Mh02 - MHp2)*MHp2*SBA2*AllHhhx(12) +
     &    CBA2*Mh02*(Mh02 - 4*MHp2)*
     &     (S2B*AllHhhx(14) - 6*AllHhhx(267))

        AllHhhx(1567) =
     &   (Mh02 - 6*MHp2)*SBA2**2*AllHhhx(12)**2 -
     &    8*MHp2*SBA*SBA2*AllHhhx(12)*AllHhhx(701)

	dup4 = SBA2*AllHhhx(12)**2 + AllHhhx(701)**2

        AllHhhx(1569) =
     &   -4*dup4*MHp2**2 +
     &    (Mh02 - 6*MHp2)*(Mh02 - 2*MHp2)*AllHhhx(701)**2

        AllHhhx(1109) =
     &   -((126*MW2*SW2)/(Alfa*Pi)*
     &       (CBA*Mh02*S2A*S2B*(CB2*Lambda6 - Lambda7*SB2)*SBA)) +
     &    (MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     ((1 - 63*CBA2**2)*AllHhhx(62))

        AllHhhx(1235) =
     &   (Mh02 - MHp2)*AllHhhx(701)**2 +
     &    1/2.D0*((Mh02 - 2*MHp2)*SBA2*AllHhhx(760)**2)

        AllHhhx(1236) =
     &   -2*CBA2*MHp2*AllHhhx(12)*
     &     (S2B*AllHhhx(14) - 6*AllHhhx(267)) +
     &    (Mh02 - MHp2)*AllHhhx(701)**2

        AllHhhx(1276) =
     &   2*(Mh02 - MHp2)*SBA*AllHhhx(701) -
     &    CBA*(4*Mh02 - MHp2)*AllHhhx(1256)

        AllHhhx(1299) =
     &   SBA2**2*AllHhhx(12)**2 +
     &    10*(Sqrt(CW2) + SW)**2*AllHhhx(701)**2*
     &     (1 - 2*Sqrt(CW2)*SW)

        AllHhhx(1503) =
     &   SBA2*AllHhhx(1234) -
     &    SBA*(2*CAB*(2*m12sq - Mh02*S2B) -
     &       (2*MW2*SW2)/(Alfa*Pi)*
     &        (CBA*(CB2*Lambda6 - Lambda7*SB2)*(1 - 3*SBA2)))

        AllHhhx(1504) =
     &   (Mh02 - 2*MHp2)*(Mh02 - MHp2)*SBA2*AllHhhx(12) +
     &    CBA2*Mh02*(Mh02 - 4*MHp2)*AllHhhx(15)

	AllHhhx(1509) = SBA2*AllHhhx(12)**2 - 2*CBA2*AllHhhx(15)**2

        AllHhhx(1511) =
     &   -3*(Mh02 - 2*MHp2)*AllHhhx(42)*AllHhhx(701) -
     &    2*MHp2*AllHhhx(701)**2 + 2*AllHhhx(15)*AllHhhx(1227)

        AllHhhx(1512) =
     &   -((Mh02 - MHp2)**2*AllHhhx(701)**2*AllHhhx(1293)) +
     &    2*CBA*(Mh02 - MHp2)*MHp2*AllHhhx(638)*AllHhhx(1503)

        AllHhhx(1521) =
     &   (MHp2 - Mh02*SBA2)*AllHhhx(12)**2 -
     &    2*CBA2*MHp2*AllHhhx(12)*AllHhhx(15) -
     &    (Mh02 - MHp2)*AllHhhx(701)**2

        AllHhhx(1524) =
     &   -2*(Mh02 - MHp2)*MHp2*SBA2*AllHhhx(12) +
     &    CBA2*Mh02*(Mh02 - 4*MHp2)*AllHhhx(15)

	dup5 = -2*SBA*AllHhhx(1161) - SBA2*AllHhhx(1234)

        AllHhhx(1549) =
     &   -2*CBA*dup5*(Mh02 - MHp2)*MHp2*AllHhhx(638) +
     &    (Mh02 - MHp2)**2*(Sqrt(CW2) + SW)**2*AllHhhx(701)**2*
     &     (1 - 2*Sqrt(CW2)*SW)

        AllHhhx(1557) =
     &   -2*CBA*dup5*Mh02*(Mh02 - 4*MHp2)*AllHhhx(638) +
     &    SBA*AllHhhx(701)*AllHhhx(1556)

        AllHhhx(1562) =
     &   -(dup4*(Mh02 - 6*MHp2)) +
     &    4*MHp2*SBA*AllHhhx(12)*AllHhhx(701)

        AllHhhx(1568) =
     &   -((Mh02 - 2*MHp2)*AllHhhx(1567)) +
     &    2*Mh02*(Mh02 - 4*MHp2)*(Sqrt(CW2) + SW)**2*
     &     AllHhhx(701)**2*(1 - 2*Sqrt(CW2)*SW)

        AllHhhx(1570) =
     &   CBA2*Mh02*(Mh02 - 4*MHp2)**2*(8*Mh02 - 3*MHp2)*
     &     (S2B*AllHhhx(14) - 6*AllHhhx(267))**2 +
     &    (4*Mh02 - MHp2)**2*AllHhhx(1569)

        AllHhhx(1110) =
     &   Mh02**2*S2B**2*(63*S2A**2 + S2B**2)*SBA2 -
     &    2*CBA2*AllHhhx(1109)

        AllHhhx(1131) =
     &   Mh02**2*S2B**4*SBA2 -
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*Mh02*S2B**2*(CB2*Lambda6 - Lambda7*SB2)*SBA) +
     &    (2*MW2**2*SW2**2)/(Alfa2*Pi**2)*(CBA2*AllHhhx(62))

        AllHhhx(1133) =
     &   -3*Mh02**2*SBA2*AllHhhx(1132) -
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*Mh02*(18*CBA2*S2A - S2B)*S2B*
     &       (CB2*Lambda6 - Lambda7*SB2)*SBA) +
     &    (2*MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (CBA2*(1 - 9*CBA2**2)*AllHhhx(62))

        AllHhhx(1237) =
     &   (Mh02 - MHp2)*SBA2**2*AllHhhx(12)**2 -
     &    CBA2*AllHhhx(1235) + SBA2*AllHhhx(1236)

        AllHhhx(1277) =
     &   (4*Mh02 - MHp2)*AllHhhx(701)**2 +
     &    (S2B*AllHhhx(14) - 6*AllHhhx(267))*AllHhhx(1276)

        AllHhhx(1296) =
     &   -((Mh02 - MHp2)*SBA2*AllHhhx(12)) +
     &    CBA2*(Mh02 - 2*MHp2)*AllHhhx(1231)

        AllHhhx(1298) =
     &   dup4*MHp2 + (Mh02 - 2*MHp2)*SBA*AllHhhx(12)*AllHhhx(701)

        AllHhhx(1300) =
     &   -(CBA2*AllHhhx(1235)) + SBA2*AllHhhx(1236) +
     &    (Mh02 - MHp2)*AllHhhx(1299)

        AllHhhx(1302) =
     &   (Mh02 - MHp2)*SBA2**2*AllHhhx(12)**2 +
     &    CBA*dup5*Mh02*AllHhhx(638) - CBA2*AllHhhx(1235) +
     &    SBA2*AllHhhx(1236)

        AllHhhx(1307) =
     &   dup4*MHp2**2*SBA2 +
     &    Mh02*(Mh02 - 4*MHp2)*(Sqrt(CW2) + SW)**2*AllHhhx(701)**2*
     &     (1 - 2*Sqrt(CW2)*SW)

        AllHhhx(1311) =
     &   6*MHp2*SBA*AllHhhx(42) + (MHp2 - Mh02*SBA2)*AllHhhx(1231)

        AllHhhx(1313) =
     &   (MHp2 - Mh02*SBA2)*AllHhhx(12)**2 +
     &    2*CBA2*MHp2*AllHhhx(12)*
     &     (S2B*AllHhhx(14) - 6*AllHhhx(267))

        AllHhhx(1396) =
     &   2*(MA02 - Mh02)*SBA*AllHhhx(41) -
     &    CBA*(MA02 - MHp2)*AllHhhx(52)

        AllHhhx(1465) =
     &   2*CBA2*MA02*(MA02 - MHp2)*AllHhhx(15) -
     &    (MA02 - Mh02)*(MA02 - Mh02 + MHp2)*SBA*AllHhhx(41)

        AllHhhx(1467) =
     &   CBA2*(MA02 - Mh02 + MHp2)*AllHhhx(15) +
     &    CBA*(MA02 - Mh02 - MHp2)*AllHhhx(52)

        AllHhhx(1495) =
     &   (Mh02 - 2*MHp2)*(Mh02 - MHp2)*SBA2*AllHhhx(12) -
     &    CBA2*Mh02*(Mh02 - 4*MHp2)*
     &     (S2B*AllHhhx(14) - 6*AllHhhx(267))

        AllHhhx(1499) =
     &   (MHp2 - Mh02*SBA2)*AllHhhx(12)**2 +
     &    2*CBA2*MHp2*AllHhhx(12)*
     &     (S2B*AllHhhx(14) - 6*AllHhhx(267)) -
     &    (Mh02 - MHp2)*AllHhhx(701)**2

        AllHhhx(1505) =
     &   -2*CBA*Mh02*(Mh02 - 4*MHp2)*AllHhhx(638)*AllHhhx(1503) +
     &    SBA*AllHhhx(701)*AllHhhx(1504)

        AllHhhx(1506) =
     &   CBA*AllHhhx(15)*AllHhhx(638) +
     &    3*AllHhhx(42)*AllHhhx(701) - AllHhhx(701)**2

        AllHhhx(1510) =
     &   -2*Mh02**2*AllHhhx(638)*
     &     (2*CBA2*AllHhhx(1221) - CBA*AllHhhx(1508)) +
     &    CBA2*Mh02*MHp2*AllHhhx(1509)

        AllHhhx(1513) =
     &   CBA2*(Mh02 - 4*MHp2)**2*AllHhhx(1511) +
     &    4*(Mh02 - 4*MHp2)*AllHhhx(1512)

        AllHhhx(1522) =
     &   3*CBA2*MHp2*AllHhhx(701)**2 + SBA2*AllHhhx(1521)

        AllHhhx(1525) =
     &   -2*CBA*Mh02*(Mh02 - 4*MHp2)*AllHhhx(638)*AllHhhx(1503) +
     &    SBA*AllHhhx(701)*AllHhhx(1524)

        AllHhhx(1527) =
     &   dup4*(Mh02 - 6*MHp2)*SBA2 -
     &    6*(Mh02 - 4*MHp2)*(Sqrt(CW2) + SW)**2*AllHhhx(701)**2*
     &     (1 - 2*Sqrt(CW2)*SW)

        AllHhhx(1558) =
     &   -(dup4*(Mh02 - 2*MHp2)*(Mh02 - MHp2)*SBA2) +
     &    2*AllHhhx(1557)

        AllHhhx(1563) =
     &   SBA2*AllHhhx(1562) +
     &    2*(Mh02 - 4*MHp2)*(Sqrt(CW2) + SW)**2*AllHhhx(701)**2*
     &     (1 - 2*Sqrt(CW2)*SW)

        AllHhhx(1571) =
     &   (4*Mh02 - MHp2)**2*AllHhhx(1568) - SBA2*AllHhhx(1570)

        AllHhhx(278) =
     &   MA02*(MA02 - MHp2)*(Mh02 - MHp2)*S2B +
     &    Mh02*S2A*AllHhhx(266)

        AllHhhx(280) =
     &   (1 - 3*CBA2)*(MA02 - Mh02)*(MA02 - MHp2) -
     &    3*SBA2*AllHhhx(266)

        AllHhhx(614) =
     &   6*SB2*AllHhhx(58) -
     &    S2B*(CB2*Lambda6 - Lambda7*SB2)*AllHhhx(613)

        AllHhhx(683) =
     &   S2B*AllHhhx(14) -
     &    (6*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA)

        AllHhhx(879) =
     &   -64*MHp2**2*S2B**2*SAB**2 -
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     (MHp2*S2B*SAB*(CB2*Lambda6 - Lambda7*SB2)*SBA*
     &       (1 - 33*SBA2)) +
     &    (MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (SBA2*(7 - 12*SBA2 - 27*SBA2**2)*AllHhhx(62))

        AllHhhx(902) =
     &   2*CB2**2*SB2*AllHhhx(301) + CB2*S2B**2*AllHhhx(901)

        AllHhhx(986) =
     &   2*SBA*(4*CAB*m12sq - CBA*AllHhhx(234)) +
     &    MHp2*S2B*AllHhhx(985)

        AllHhhx(987) =
     &   MHp2*S2B*(S2B + 14*CAB*SBA - 4*S2B*SBA2) +
     &    4*SBA*(4*CAB*m12sq - CBA*AllHhhx(234))

        AllHhhx(1079) =
     &   3*Mh02*S2B*SBA*AllHhhx(1066) +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(17 + 60*CBA2 - 297*CBA2**2)*
     &       (CB2*Lambda6 - Lambda7*SB2))

        AllHhhx(1081) =
     &   Mh02*S2B*SBA*AllHhhx(1069) +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(7 - 12*CBA2 + 81*CBA2**2)*
     &       (CB2*Lambda6 - Lambda7*SB2))

        AllHhhx(1107) =
     &   Mh02**2*S2B**2*SBA2*AllHhhx(1106) +
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*Mh02*S2B*(18*CBA2*S2A + S2B)*
     &       (CB2*Lambda6 - Lambda7*SB2)*SBA) +
     &    (36*MW2**2*SW2**2)/(Alfa2*Pi**2)*(CBA2**3*AllHhhx(62))

        AllHhhx(1108) =
     &   Mh02**2*S2B**2*(27*S2A**2 + S2B**2)*SBA2 +
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*Mh02*(27*CBA2*S2A - S2B)*S2B*
     &       (CB2*Lambda6 - Lambda7*SB2)*SBA) +
     &    (2*MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (CBA2*(1 + 27*CBA2**2)*AllHhhx(62))

	AllHhhx(1111) = -4*MA02**3*S2B**4*SBA2 + MA02*AllHhhx(1110)

        AllHhhx(1129) =
     &   Mh02**2*S2B**2*SBA2*AllHhhx(1128) +
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*Mh02*(9*CBA2*S2A - 11*S2B)*S2B*
     &       (CB2*Lambda6 - Lambda7*SB2)*SBA) +
     &    (6*MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (CBA2*(1 + 3*CBA2**2)*AllHhhx(62))

        AllHhhx(1134) =
     &   MA02**3*S2B**2*AllHhhx(1130) + Mh02**2*AllHhhx(1131) -
     &    MA02**2*AllHhhx(1133)

        AllHhhx(1209) =
     &   CBA*AllHhhx(638) +
     &    (2*(Mh02 - MHp2)*SBA*AllHhhx(701))/(4*Mh02 - MHp2)

        AllHhhx(1223) =
     &   6*CBA*(MHp2*S2B*SAB - SBA2*AllHhhx(56)) +
     &    CBA2*AllHhhx(1222)

        AllHhhx(1228) =
     &   SBA2*AllHhhx(12)**2 -
     &    2*CBA2*(S2B*AllHhhx(14) - 6*AllHhhx(267))**2 +
     &    2*AllHhhx(701)**2

        AllHhhx(1232) =
     &   4*(Mh02 - MHp2)*MHp2**2*SBA2*AllHhhx(12) +
     &    CBA2*Mh02*AllHhhx(1230)*AllHhhx(1231)

	AllHhhx(1238) = 4*CBA*dup5*MHp2*AllHhhx(638) + AllHhhx(1237)

        AllHhhx(1250) =
     &   6*SBA*AllHhhx(42) +
     &    ((MHp2 - Mh02*SBA2)*AllHhhx(1231))/MHp2

        AllHhhx(1251) =
     &   (2*dup4*MHp2**2)/(Mh02 - 4*MHp2)**2 +
     &    (CBA2*Mh02**2*(S2B*AllHhhx(14) - 6*AllHhhx(267))**2)/
     &     (4*Mh02 - MHp2)**2

        AllHhhx(1257) =
     &   4*(Mh02 - MHp2)*SBA*AllHhhx(701) -
     &    CBA*(4*Mh02 - MHp2)*AllHhhx(1256)

        AllHhhx(1270) =
     &   -2*(4*Mh02 - MHp2)*MHp2*AllHhhx(701) +
     &    (3*MW2*SW2)/(Alfa*Pi)*(CBA*Lambda7*SB2*AllHhhx(1262))

        AllHhhx(1273) =
     &   4*(Mh02 - MHp2)*AllHhhx(42)**2 + AllHhhx(42)*AllHhhx(1203)

        AllHhhx(1278) =
     &   2*(4*Mh02 - MHp2)*AllHhhx(701)*AllHhhx(1208) +
     &    (2*Mh02 - MHp2)*AllHhhx(1277)

        AllHhhx(1284) =
     &   -2*(4*Mh02 - MHp2)*MHp2*SBA2*AllHhhx(12) +
     &    5*CBA2*Mh02*(Mh02 - 4*MHp2)*
     &     (S2B*AllHhhx(14) - 6*AllHhhx(267))

        AllHhhx(1286) =
     &   -(CBA*dup5*Mh02*(Mh02 - 4*MHp2)*AllHhhx(638)) +
     &    (Mh02 - 2*MHp2)*(Mh02 - MHp2)*SBA*SBA2*AllHhhx(12)*
     &     AllHhhx(701)

        AllHhhx(1290) =
     &   SBA2*AllHhhx(12)**2 -
     &    2*CBA2*(S2B*AllHhhx(14) - 6*AllHhhx(267))**2

        AllHhhx(1292) =
     &   3*(Mh02 - 2*MHp2)*AllHhhx(42)*AllHhhx(701) +
     &    2*(MHp2*AllHhhx(701)**2 +
     &       (S2B*AllHhhx(14) - 6*AllHhhx(267))*AllHhhx(1227))

        AllHhhx(1294) =
     &   2*CBA*dup5*(Mh02 - MHp2)*MHp2*AllHhhx(638) +
     &    (Mh02 - MHp2)**2*AllHhhx(701)**2*AllHhhx(1293)

        AllHhhx(1297) =
     &   (Mh02 - 2*MHp2)**2*(Mh02 - MHp2)*SBA2*AllHhhx(12) +
     &    Mh02*(Mh02 - 4*MHp2)*AllHhhx(1296)

        AllHhhx(1301) =
     &   2*(Mh02 - MHp2)*SBA2*AllHhhx(1298) +
     &    (Mh02 - 4*MHp2)*AllHhhx(1300)

        AllHhhx(1303) =
     &   2*dup4*(Mh02 - MHp2)*MHp2**2*SBA2 +
     &    SBA*AllHhhx(701)*AllHhhx(1297) +
     &    Mh02*(Mh02 - 4*MHp2)*AllHhhx(1302)

        AllHhhx(1306) =
     &   CBA2*Mh02*(Mh02 - 4*MHp2)**2*(8*Mh02 - 3*MHp2)*
     &     (S2B*AllHhhx(14) - 6*AllHhhx(267))**2 +
     &    (Mh02 - 6*MHp2)*(Mh02 - 2*MHp2)*(4*Mh02 - MHp2)**2*
     &     AllHhhx(701)**2

        AllHhhx(1308) =
     &   -((Mh02 - 6*MHp2)*(Mh02 - 2*MHp2)*SBA2**2*
     &       AllHhhx(12)**2) + 4*AllHhhx(1307) +
     &    2*Mh02*(Mh02 - 4*MHp2)*(Sqrt(CW2) + SW)**2*
     &     AllHhhx(701)**2*(1 - 2*Sqrt(CW2)*SW)

        AllHhhx(1310) =
     &   2*dup4*(4*Mh02 - MHp2)**2*MHp2**2 +
     &    CBA2*Mh02**2*(Mh02 - 4*MHp2)**2*
     &     (S2B*AllHhhx(14) - 6*AllHhhx(267))**2

        AllHhhx(1314) =
     &   CBA2*(Mh02 - 4*MHp2)**2*(4*Mh02 - MHp2)*
     &     (S2B*AllHhhx(14) - 6*AllHhhx(267))*AllHhhx(1311) -
     &    (Mh02 - 4*MHp2)*(4*Mh02 - MHp2)**2*
     &     (AllHhhx(701)**2*AllHhhx(1312) + SBA2*AllHhhx(1313))

        AllHhhx(1318) =
     &   CBA2*(Mh02 - 4*MHp2)**2*(4*Mh02 - MHp2)*
     &     (S2B*AllHhhx(14) - 6*AllHhhx(267))*AllHhhx(1311) -
     &    (Mh02 - 4*MHp2)*(4*Mh02 - MHp2)**2*
     &     (SBA2*AllHhhx(1313) + AllHhhx(701)**2*AllHhhx(1317))

        AllHhhx(1386) =
     &   (MA02*SBA*AllHhhx(41))/((4*MA02 - Mh02)*(MA02 - MHp2)) +
     &    (CBA*Mh02*AllHhhx(52))/((MA02 - Mh02)*(Mh02 - MHp2))

        AllHhhx(1388) =
     &   -(CBA2*(2*MA02 - Mh02)*AllHhhx(15)) + CBA*Mh02*AllHhhx(52)

	dup6 = -(SBA2*AllHhhx(1160)) - 2*SBA*AllHhhx(1161)

        AllHhhx(1397) =
     &   2*CBA*dup6*(MA02 - Mh02)*AllHhhx(52) +
     &    CBA2*(S2B*AllHhhx(14) - 6*AllHhhx(267))*AllHhhx(1396)

        AllHhhx(1415) =
     &   CBA*Mh02*AllHhhx(52) +
     &    CBA2*(2*MA02 - Mh02)*(S2B*AllHhhx(14) - 6*AllHhhx(267))

        AllHhhx(1422) =
     &   -2*CAB*(2*m12sq - Mh02*S2B) + SBA*AllHhhx(1160) +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*(1 - 3*SBA2))

        AllHhhx(1439) =
     &   (MA02 - Mh02)*SBA2*AllHhhx(12) -
     &    CBA2*(MA02 - MHp2)*AllHhhx(1231)

        AllHhhx(1442) =
     &   -(CBA*(MA02 - MHp2)*AllHhhx(52)*AllHhhx(1422)) +
     &    AllHhhx(41)*AllHhhx(1439)

        AllHhhx(1460) =
     &   (4*Mh02 - MHp2)*SBA2*AllHhhx(12)**2 +
     &    CBA2*(Mh02 - 4*MHp2)*AllHhhx(15)**2

        AllHhhx(1466) =
     &   (MA02 - Mh02)*MHp2*AllHhhx(41)**2 -
     &    AllHhhx(12)*AllHhhx(1465)

        AllHhhx(1468) =
     &   CBA*(MA02 + Mh02 - MHp2)*SBA2*AllHhhx(12)*AllHhhx(52) +
     &    SBA*AllHhhx(41)*AllHhhx(1467)

        AllHhhx(1496) =
     &   2*CBA*dup5*Mh02*(Mh02 - 4*MHp2)*AllHhhx(638) +
     &    SBA*AllHhhx(701)*AllHhhx(1495)

        AllHhhx(1498) =
     &   -(dup4*(Mh02 - 2*MHp2)*MHp2*SBA2) +
     &    2*Mh02*(Mh02 - 4*MHp2)*(Sqrt(CW2) + SW)**2*
     &     AllHhhx(701)**2*(1 - 2*Sqrt(CW2)*SW)

        AllHhhx(1500) =
     &   3*CBA2*MHp2*AllHhhx(701)**2 + SBA2*AllHhhx(1499)

        AllHhhx(1507) =
     &   4*(Mh02 - MHp2)*AllHhhx(1505) +
     &    CBA2*Mh02*(Mh02 - 4*MHp2)**2*AllHhhx(1506)

        AllHhhx(1523) =
     &   (Mh02 - MHp2)*AllHhhx(1498) +
     &    Mh02*(Mh02 - 4*MHp2)*AllHhhx(1522)

        AllHhhx(1526) =
     &   dup4*(Mh02 - 2*MHp2)*(Mh02 - MHp2)*SBA2 + 2*AllHhhx(1525)

        AllHhhx(1528) =
     &   2*(Mh02 - 4*MHp2)*AllHhhx(1522) -
     &    (Mh02 - MHp2)*AllHhhx(1527)

        AllHhhx(1540) =
     &   -(Mh02*S2B**3*AllHhhx(1260)) + CB2*AllHhhx(1267) -
     &    S2B**2*(SBA2*AllHhhx(1231)*AllHhhx(1269) +
     &       SBA*AllHhhx(1270))

        dup7 = -3*SBA2*AllHhhx(12)*AllHhhx(635) +
     &    AllHhhx(638)*AllHhhx(1197)

        AllHhhx(1564) =
     &   3*CBA*dup7*(Mh02 - 4*MHp2)**2*(Mh02 - MHp2) +
     &    (Mh02 - MHp2)**2*AllHhhx(1563)

        AllHhhx(1673) =
     &   AllHhhx(1653)**2 +
     &    (3*MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     ((1 + 6*CBA2*SBA2)*AllHhhx(62)) -
     &    (12*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA*AllHhhx(1653))

        AllHhhx(1766) =
     &   (CB2*Lambda6 - Lambda7*SB2)*
     &    (2*(Mh02 - 2*MHp2)*S2B**2 + 3*SBA2*AllHhhx(1653))

        AllHhhx(274) =
     &   2*MHp2*S2A*SBA2*AllHhhx(266) +
     &    (MA02 - Mh02)*(MA02 - MHp2)*AllHhhx(273)

        AllHhhx(276) =
     &   m12sq*(3*S2A + S2B)*SBA*AllHhhx(266) +
     &    (MW2*SW2)/(Alfa*Pi)*(CBA*Lambda7*SB2*AllHhhx(275))

        AllHhhx(279) =
     &   CBA2*(MA02 - Mh02)*(MA02 - MHp2)*AllHhhx(277) +
     &    S2B*SBA2*AllHhhx(278)

        AllHhhx(281) =
     &   8*m12sq*(MA02 - Mh02)*(MA02 - MHp2)*SAB*SB2 +
     &    (MW2*SW2)/(Alfa*Pi)*(Lambda6*S2B**2*SBA*AllHhhx(280))

        AllHhhx(612) =
     &   -(MHp2*S2B**3*SAB*(1 - 2*SBA2)) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (SB2*SBA*(1 + SBA2 - 3*SBA2**2)*AllHhhx(58))

        AllHhhx(615) =
     &   -2*MHp2*S2B**2*SAB*AllHhhx(378) +
     &    (MW2*SW2)/(Alfa*Pi)*(SBA*AllHhhx(614))

        AllHhhx(684) =
     &   S2B*AllHhhx(682) -
     &    4*(Mh02 - MHp2)*(2*Mh02 - MHp2)*AllHhhx(683)

        AllHhhx(876) =
     &   -2*MHp2**2*S2B**2*SAB**2*(40 - 631*SBA2) +
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     (MHp2*S2B*SAB*(CB2*Lambda6 - Lambda7*SB2)*SBA*
     &       (40 - 64*SBA2 - 567*SBA2**2)) +
     &    (MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (SBA2*AllHhhx(62)*AllHhhx(872))

        AllHhhx(877) =
     &   -2*MHp2**2*S2B**2*SAB**2*(4 - 127*SBA2) +
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     (MHp2*S2B*SAB*(CB2*Lambda6 - Lambda7*SB2)*SBA*
     &       (4 - 10*SBA2 - 117*SBA2**2)) +
     &    (MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (SBA2*AllHhhx(62)*AllHhhx(873))

        AllHhhx(878) =
     &   -2*MHp2**2*S2B**2*SAB**2*(8 - 87*SBA2) +
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     (MHp2*S2B*SAB*(CB2*Lambda6 - Lambda7*SB2)*SBA*
     &       (8 - 12*SBA2 - 75*SBA2**2)) +
     &    (MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (SBA2*AllHhhx(62)*AllHhhx(815))

        AllHhhx(903) =
     &   Lambda7*S2B**2*SB2*(16 - 5*SBA2 - 117*SBA2**3) +
     &    2*Lambda6*AllHhhx(902)

        AllHhhx(988) =
     &   -(S2B*AllHhhx(982)) + 2*Mh02**3*AllHhhx(984) -
     &    Mh02*MHp2**2*AllHhhx(986) + Mh02**2*MHp2*AllHhhx(987)

        AllHhhx(1074) =
     &   Mh02**2*S2B**2*SBA2*AllHhhx(1072) -
     &    (6*MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (CBA2*(1 - 4*CBA2 - 9*CBA2**2)*AllHhhx(62)) +
     &    (2*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*Mh02*S2B*(CB2*Lambda6 - Lambda7*SB2)*SBA*
     &       AllHhhx(1073))

        AllHhhx(1082) =
     &   -24*(1 - 2*CBA2)*MA02**3*S2B**2*SBA -
     &    MA02*Mh02*AllHhhx(1079) + 2*MA02**2*AllHhhx(1080) -
     &    Mh02**2*AllHhhx(1081)

        AllHhhx(110) =
     &   8*m12sq**2*SAB**2 +
     &    (MW2**2*SW2**2)/(Alfa2*Pi**2)*(SBA2*AllHhhx(62))

        AllHhhx(1119) =
     &   16*CAB**2*(9*AllHhhx(1102)*AllHhhx(1103) +
     &       Mh02*AllHhhx(1105)) + 4*MA02**2*AllHhhx(1107) +
     &    Mh02**2*AllHhhx(1108) - 2*Mh02*AllHhhx(1111) -
     &    4*CAB*(18*AllHhhx(1102)*AllHhhx(1113) -
     &       Mh02*AllHhhx(1118))

        AllHhhx(1142) =
     &   16*AllHhhx(1127) - MA02*Mh02*AllHhhx(1129) +
     &    4*AllHhhx(1134) -
     &    4*CAB*(2*m12sq*AllHhhx(1136) + Mh02*S2B*AllHhhx(1141))

        AllHhhx(1182) =
     &   CBA2*(MA02 - Mh02)*(MA02 - Mh02 + MHp2) -
     &    AllHhhx(703)/(MA02 - Mh02)

        AllHhhx(1191) =
     &   (Mh02 - MHp2)*SBA2*AllHhhx(12) -
     &    (Mh02 + MHp2)*SBA*AllHhhx(701)

        AllHhhx(1210) =
     &   AllHhhx(701)**2 +
     &    (S2B*AllHhhx(14) - 6*AllHhhx(267))*AllHhhx(1209)

        AllHhhx(1212) =
     &   3*(2*Mh02 - MHp2)*SBA*AllHhhx(42) +
     &    CBA2*Mh02*(S2B*AllHhhx(14) - 6*AllHhhx(267))

        AllHhhx(1215) =
     &   AllHhhx(701)**2 +
     &    (S2B*AllHhhx(14) - 6*AllHhhx(267))*
     &     (SBA2*AllHhhx(1213) + 6*SBA*AllHhhx(1214))

        AllHhhx(1224) =
     &   -(AllHhhx(638)*(CBA*AllHhhx(1220) -
     &         2*CBA2*AllHhhx(1221))) +
     &    SBA2*AllHhhx(12)*AllHhhx(1223)

        AllHhhx(1229) =
     &   3*(Mh02 - 2*MHp2)*AllHhhx(42)*AllHhhx(701) +
     &    2*(S2B*AllHhhx(14) - 6*AllHhhx(267))*AllHhhx(1227) +
     &    MHp2*AllHhhx(1228)

        AllHhhx(1239) =
     &   (SBA*AllHhhx(701)*AllHhhx(1232))/
     &     (Mh02*(Mh02 - 4*MHp2)**2) +
     &    AllHhhx(1238)/(Mh02 - 4*MHp2)

        AllHhhx(1244) =
     &   (2*MHp2*SBA2*AllHhhx(12))/Mh02 -
     &    (5*CBA2*(Mh02 - 4*MHp2)*
     &       (S2B*AllHhhx(14) - 6*AllHhhx(267)))/(4*Mh02 - MHp2)

        AllHhhx(1249) =
     &   ((MHp2 - Mh02*SBA2)*AllHhhx(12)**2)/MHp2 +
     &    2*CBA2*AllHhhx(12)*(S2B*AllHhhx(14) - 6*AllHhhx(267))

        AllHhhx(1252) =
     &   (CBA2*(S2B*AllHhhx(14) - 6*AllHhhx(267))*AllHhhx(1250))/
     &     (4*Mh02 - MHp2) +
     &    ((Mh02 - MHp2)*SBA2*AllHhhx(1251))/(Mh02*MHp2)

        AllHhhx(1258) =
     &   -((4*Mh02 - MHp2)*
     &       (3*AllHhhx(42)*AllHhhx(701) - AllHhhx(701)**2)) +
     &    (S2B*AllHhhx(14) - 6*AllHhhx(267))*AllHhhx(1257)

        AllHhhx(1271) =
     &   -(Mh02*S2B**3*AllHhhx(1260)) + CB2*AllHhhx(1267) +
     &    S2B**2*(SBA2*AllHhhx(1231)*AllHhhx(1269) -
     &       SBA*AllHhhx(1270))

        AllHhhx(1274) =
     &   4*(Mh02 - MHp2)*(4*Mh02 - MHp2)*
     &     (S2B*AllHhhx(14) - 6*AllHhhx(267))*AllHhhx(1212) -
     &    9*(4*Mh02 - MHp2)**2*AllHhhx(1273)

        AllHhhx(1285) =
     &   -2*CBA*dup5*Mh02*(Mh02 - 4*MHp2)*(4*Mh02 - MHp2)*
     &     AllHhhx(638) + (Mh02 - MHp2)*SBA*AllHhhx(701)*
     &     AllHhhx(1284)

        AllHhhx(1287) =
     &   (Mh02 - MHp2)*SBA*AllHhhx(701)*AllHhhx(1284) +
     &    2*(4*Mh02 - MHp2)*AllHhhx(1286)

        AllHhhx(1289) =
     &   -2*dup4*(Mh02 - MHp2)**2*MHp2**2 +
     &    Mh02**2*(Mh02 - 4*MHp2)**2*AllHhhx(12)*AllHhhx(1223)

        AllHhhx(1291) =
     &   2*Mh02**2*AllHhhx(638)*
     &     (CBA*AllHhhx(1220) - 2*CBA2*AllHhhx(1221)) +
     &    CBA2*Mh02*MHp2*AllHhhx(1290)

        AllHhhx(1295) =
     &   CBA2*(Mh02 - 4*MHp2)**2*AllHhhx(1292) +
     &    4*(Mh02 - 4*MHp2)*AllHhhx(1294)

        AllHhhx(1304) =
     &   -(SBA*AllHhhx(701)*AllHhhx(1297)) - Mh02*AllHhhx(1301) +
     &    4*AllHhhx(1303)

        AllHhhx(1309) =
     &   SBA2*AllHhhx(1306) - (4*Mh02 - MHp2)**2*AllHhhx(1308)

        AllHhhx(1315) =
     &   (Mh02 - MHp2)*SBA2*AllHhhx(1310) + Mh02*AllHhhx(1314)

        AllHhhx(1319) =
     &   (Mh02 - MHp2)*SBA2*AllHhhx(1310) + Mh02*AllHhhx(1318)

	AllHhhx(1346) = SBA2*AllHhhx(12)**2 + 2*AllHhhx(701)**2

        AllHhhx(1358) =
     &   (MA02 - Mh02)*(MA02 - Mh02 + MHp2)*SBA*AllHhhx(41) +
     &    2*CBA2*MA02*(MA02 - MHp2)*
     &     (S2B*AllHhhx(14) - 6*AllHhhx(267))

        AllHhhx(1360) =
     &   CBA*(MA02 - Mh02 - MHp2)*AllHhhx(52) -
     &    CBA2*(MA02 - Mh02 + MHp2)*
     &     (S2B*AllHhhx(14) - 6*AllHhhx(267))

        AllHhhx(1370) =
     &   2*(MA02 - Mh02)*SBA2*AllHhhx(12) -
     &    CBA2*(MA02 - MHp2)*AllHhhx(1231)

        AllHhhx(1378) =
     &   (MA02 - Mh02)*(Mh02 - 2*MHp2)*SBA*AllHhhx(41) +
     &    2*CBA*Mh02*(MA02 - MHp2)*AllHhhx(52)

        AllHhhx(1387) =
     &   (MHp2*SBA*AllHhhx(1368)**2)/(MA02 - MHp2)**2 -
     &    1/4.D0*(AllHhhx(41)*AllHhhx(1386))

        AllHhhx(1389) =
     &   -((MA02 - Mh02)*(2*MA02 - Mh02)*SBA2*AllHhhx(760)) +
     &    2*(MA02 - MHp2)*AllHhhx(1388)

        AllHhhx(1398) =
     &   (MA02 - Mh02)**2*SBA*SBA2*AllHhhx(12)*AllHhhx(41) +
     &    (MA02 - MHp2)*AllHhhx(1397)

	AllHhhx(1402) = SBA2**2*AllHhhx(12)**2 - AllHhhx(701)**2

        AllHhhx(1404) =
     &   CBA2*(MA02 - MHp2)*SBA2*AllHhhx(12)**2 +
     &    (MA02 - Mh02)*AllHhhx(701)**2

        AllHhhx(1412) =
     &   (9*SBA*AllHhhx(42)*(S2B*AllHhhx(14) - 6*AllHhhx(267)))/
     &     (Mh02 - MHp2) +
     &    (2*(S2B*AllHhhx(14) - 6*AllHhhx(267))**2*AllHhhx(1364))/
     &     ((MA02 - MHp2)*(4*Mh02 - MHp2))

        AllHhhx(1416) =
     &   -((MA02 - Mh02)*(2*MA02 - Mh02)*SBA2*AllHhhx(760)) +
     &    2*(MA02 - MHp2)*AllHhhx(1415)

        AllHhhx(1423) =
     &   -(CBA*AllHhhx(41)*(S2B*AllHhhx(14) - 6*AllHhhx(267))) +
     &    AllHhhx(52)*AllHhhx(1422)

        AllHhhx(1440) =
     &   (MA02 - Mh02)*SBA*AllHhhx(41)*AllHhhx(1438) -
     &    AllHhhx(52)*AllHhhx(1439)

        AllHhhx(1443) =
     &   4*AllHhhx(1356) + (SBA*AllHhhx(1442))/(MA02 - Mh02)

        AllHhhx(1463) =
     &   CBA2*MA02*AllHhhx(15)**2 +
     &    CBA*(MA02 + Mh02 - MHp2)*AllHhhx(15)*AllHhhx(52) +
     &    Mh02*AllHhhx(52)**2

        AllHhhx(1469) =
     &   SBA2*AllHhhx(1466) - (MA02 - MHp2)*AllHhhx(1468)

        AllHhhx(1471) =
     &   AllHhhx(1419)/(Mh02 - MHp2) + MA02*AllHhhx(1420)

        AllHhhx(1485) =
     &   (2*AllHhhx(1419))/(Mh02 - MHp2) + Mh02*AllHhhx(1420)

        AllHhhx(1501) =
     &   (Mh02 - MHp2)*AllHhhx(1498) +
     &    Mh02*(Mh02 - 4*MHp2)*AllHhhx(1500)

        AllHhhx(1658) =
     &   (MW2**2*SW2**2)/(Alfa2*Pi**2)*AllHhhx(62) +
     &    (9*MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*AllHhhx(1657))

        AllHhhx(1669) =
     &   CAB*(2*m12sq - Mh02*S2B)*AllHhhx(1643) +
     &    (2*m12sq - MHp2*S2B)*SAB*AllHhhx(1646) -
     &    (MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*AllHhhx(1668))

        AllHhhx(1672) =
     &   CBA2*MHp2**2*S2A**2*S2B**2 -
     &    4*SAB*(MHp2*S2B - 2*m12sq*SBA2)*AllHhhx(1656) +
     &    (2*MW2**2*SW2**2)/(Alfa2*Pi**2)*(SBA2**3*AllHhhx(62)) +
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     ((CB2*Lambda6 - Lambda7*SB2)*SBA*SBA2*AllHhhx(1657))

        AllHhhx(1674) =
     &   CBA2*MHp2**2*S2B**4 + 4*AllHhhx(1648) + SBA2*AllHhhx(1673)

	AllHhhx(169) = MA02*S2A*S2B*SBA2 - 6*CBA*SBA*AllHhhx(9)

        AllHhhx(1694) =
     &   AllHhhx(1651)**2 +
     &    (12*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA*
     &       (6*m12sq*S2A - S2B*AllHhhx(14))) +
     &    (3*MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     ((1 + 6*CBA2*SBA2)*AllHhhx(62))

        AllHhhx(1703) =
     &   2*(Mh02 - 2*MHp2)**2*S2B**4 + AllHhhx(1653)**2 +
     &    (18*MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (CBA2*SBA2*AllHhhx(62)) -
     &    (12*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA*AllHhhx(1653))

        AllHhhx(1726) =
     &   CBA2*(4*MA02 - Mh02 + 3*MHp2)*S2B**2 +
     &    3*CBA2**2*AllHhhx(1620)

        AllHhhx(1727) =
     &   -10*CBA2*(2*m12sq - MHp2*S2B)*SAB + CBA*AllHhhx(1726)

        AllHhhx(1767) =
     &   4*CAB*(2*m12sq - Mh02*S2B)*(CB2*Lambda6 - Lambda7*SB2) +
     &    SBA*AllHhhx(1766)

        AllHhhx(1790) =
     &   24*m12sq**2*(3*S2A + S2B)*SAB*SBA2 +
     &    CBA*S2B**4*AllHhhx(1582)

        AllHhhx(1811) =
     &   (2*MA02 - MHp2)**2*S2B**4 + AllHhhx(1651)**2 +
     &    (12*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA*
     &       (6*m12sq*S2A - S2B*AllHhhx(14))) +
     &    (18*MW2**2*SW2**2)/(Alfa2*Pi**2)*(CBA2*SBA2*AllHhhx(62))

	AllHhhx(182) = -5*MHp2*S2B**2*SBA2 + 4*SBA*AllHhhx(36)

	AllHhhx(217) = MHp2*S2B**2*(12 + SBA2) + 4*SBA*AllHhhx(36)

        AllHhhx(282) =
     &   S2B**3*AllHhhx(274) - 2*S2B**2*SBA*AllHhhx(276) +
     &    2*CB2*(2*SB2*AllHhhx(279) + CBA*AllHhhx(281))

        AllHhhx(316) =
     &   2*MHp2**2*S2B**2*SAB**2*(20 + 19*SBA2) -
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     (MHp2*S2B*SAB*(CB2*Lambda6 - Lambda7*SB2)*SBA*
     &       (20 - 17*SBA2 + 36*SBA2**2)) +
     &    (MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (SBA2*(20 - 17*SBA2 + 36*SBA2**3)*AllHhhx(62))

        AllHhhx(336) =
     &   2*((24*MA02**5 + MHp2**5)*S2B**2*SBA2*AllHhhx(300) +
     &       m12sq*S2B*SAB*AllHhhx(321)) +
     &    8*MA02**4*MHp2*AllHhhx(324) -
     &    2*MA02**2*MHp2**3*AllHhhx(328) -
     &    4*MA02**3*MHp2**2*AllHhhx(332) -
     &    MA02*MHp2**4*AllHhhx(335)

        AllHhhx(399) =
     &   CBA2**2*MA02*AllHhhx(346) - 4*MA02**3*AllHhhx(398)

        AllHhhx(609) =
     &   12*MA02*S2B*SAB*(CB2*Lambda6 - Lambda7*SB2)*SBA*SBA2 +
     &    (MW2*SW2)/(Alfa*Pi)*AllHhhx(62)

        AllHhhx(616) =
     &   -2*MHp2**2*S2B**2*AllHhhx(300) +
     &    2*(m12sq*S2B*SAB*
     &        (MA02*MHp2*AllHhhx(610) + 2*S2B*AllHhhx(611)) +
     &       MA02**2*AllHhhx(612)) - MA02*MHp2*AllHhhx(615)

        AllHhhx(63) = S2B**2*(2*m12sq - MHp2*S2B)*SAB +
     &    (MW2*SW2)/(Alfa*Pi)*(SB2*SBA*AllHhhx(58))

	AllHhhx(639) = 9*AllHhhx(635)**2 + 2*AllHhhx(638)**2

        AllHhhx(685) =
     &   (4*Mh02 - MHp2)*MHp2*S2B**2*AllHhhx(680) -
     &    2*Mh02*AllHhhx(684)

        AllHhhx(761) =
     &   (MA02 - Mh02)*SBA*AllHhhx(41) +
     &    CBA2*(MA02 - MHp2)*(S2B*AllHhhx(14) - 6*AllHhhx(267))

	AllHhhx(82) = (Mh02 - 2*MHp2)*S2B**2*SBA2 - 2*SBA*AllHhhx(81)

        AllHhhx(871) =
     &   -192*MA02**6*S2B**2*SBA2*AllHhhx(300) +
     &    32*MA02**5*MHp2*AllHhhx(839) -
     &    MA02**2*MHp2**4*AllHhhx(844) -
     &    6*MA02**3*MHp2**3*AllHhhx(849) - MHp2**6*AllHhhx(854) +
     &    16*MA02**4*MHp2**2*AllHhhx(859) + 2*AllHhhx(870)

        AllHhhx(904) =
     &   MHp2*S2B**3*SAB*(16 - 5*SBA2 - 117*SBA2**2) +
     &    (MW2*SW2)/(Alfa*Pi)*(SBA*AllHhhx(903))

        AllHhhx(911) =
     &   2*MHp2**2*S2B**2*SAB**2*(24 - 107*SBA2) -
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     (MHp2*S2B*SAB*(CB2*Lambda6 - Lambda7*SB2)*SBA*
     &       (24 + 37*SBA2 - 144*SBA2**2)) +
     &    (MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (SBA2*(24 + 37*SBA2 - 144*SBA2**3)*AllHhhx(62))

        AllHhhx(912) =
     &   4*MHp2**2*S2B**2*SAB**2*(8 - 61*SBA2) -
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     (MHp2*S2B*SAB*(CB2*Lambda6 - Lambda7*SB2)*SBA*
     &       (16 - 5*SBA2 - 117*SBA2**2)) +
     &    (MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (SBA2*(16 - 5*SBA2 - 117*SBA2**3)*AllHhhx(62))

        AllHhhx(940) =
     &   -104*MA02**5*S2B**2*SBA2*AllHhhx(300) -
     &    8*MA02**4*MHp2*AllHhhx(915) - MHp2**5*AllHhhx(920) +
     &    4*MA02**3*MHp2**2*AllHhhx(925) -
     &    MA02**2*MHp2**3*AllHhhx(930) + 2*AllHhhx(939)

        AllHhhx(962) =
     &   Mh02**3*S2B*AllHhhx(958) -
     &    Mh02*MHp2*(8*SBA*AllHhhx(36) + MHp2*S2B*AllHhhx(959)) +
     &    Mh02**2*(4*SBA*AllHhhx(36) + MHp2*S2B*AllHhhx(960)) +
     &    MHp2**2*(4*SBA*AllHhhx(36) - MHp2*S2B*AllHhhx(961))

        AllHhhx(972) =
     &   S2B*AllHhhx(964) + Mh02**4*AllHhhx(966) -
     &    Mh02**3*MHp2*(2*CBA*SBA*AllHhhx(9) +
     &       MHp2*S2B*AllHhhx(967)) +
     &    Mh02**2*MHp2**2*AllHhhx(969) -
     &    2*Mh02*MHp2**3*AllHhhx(971)

        AllHhhx(979) =
     &   Mh02*MHp2**2*(2*CBA*SBA*AllHhhx(9) -
     &       MHp2*S2B*AllHhhx(973)) +
     &    MHp2**3*(4*SBA*AllHhhx(36) - MHp2*S2B*AllHhhx(974)) +
     &    Mh02**3*AllHhhx(976) +
     &    2*(Mh02**4*S2B*(3*CAB*SBA + S2A*SBA2) +
     &       Mh02**2*MHp2*AllHhhx(978))

	AllHhhx(989) = MA02**5*S2B**2*(1 + 2*SBA2) + Mh02*AllHhhx(988)

        AllHhhx(101) =
     &   2*SAB**2*AllHhhx(92) - 2*MHp2*AllHhhx(100) +
     &    (MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (SBA2*(8 - 3*(4 - 3*SBA2)*SBA2)*AllHhhx(62))

        AllHhhx(1067) =
     &   Mh02**2*S2B**2*SBA2*AllHhhx(1065) -
     &    (2*MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (CBA2*(17 + 60*CBA2 - 297*CBA2**2)*AllHhhx(62)) -
     &    (12*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*Mh02*S2B*(CB2*Lambda6 - Lambda7*SB2)*SBA*
     &       AllHhhx(1066))

        AllHhhx(1070) =
     &   Mh02**2*S2B**2*SBA2*AllHhhx(1068) +
     &    (2*MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (CBA2*(7 - 12*CBA2 + 81*CBA2**2)*AllHhhx(62)) +
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*Mh02*S2B*(CB2*Lambda6 - Lambda7*SB2)*SBA*
     &       AllHhhx(1069))

        AllHhhx(1083) =
     &   MA02**3*S2B**2*AllHhhx(1071) + MA02**2*AllHhhx(1074) +
     &    CAB*(Mh02*S2B*AllHhhx(1078) - 2*m12sq*AllHhhx(1082))

        AllHhhx(1091) =
     &   AllHhhl(2)**2*AllHhhx(41)*AllHhhx(1090) +
     &    MA02*AllHhhl(26)*AllHhhx(1090)**2

        AllHhhx(1143) =
     &   2*AllHhhx(41)*(AllHhhl(1)**2*AllHhhx(1096) +
     &       AllHhhl(25)*AllHhhx(1101)) +
     &    AllHhhl(2)*AllHhhx(1119) +
     &    AllHhhl(1)*(12*AllHhhl(2)*AllHhhx(41)*AllHhhx(1122) +
     &       AllHhhx(1142))

        AllHhhx(1162) =
     &   ((MA0 - MHp)*(MA0 + MHp)*AllHhhx(12))/
     &     ((MA02 - Mh02)*MHp2) -
     &    ((MA0 - Mh0)*(MA0 + Mh0)*
     &       (S2B*AllHhhx(14) - 6*AllHhhx(267)))/
     &     (Mh02*(MA02 - MHp2))

        AllHhhx(1183) =
     &   ((MA02 + Mh02 - MHp2)*SBA2)/Mh02 -
     &    AllHhhx(1182)/((MA02 - MHp2)*MHp2)

        AllHhhx(1184) =
     &   -2*CAB*(2*m12sq - Mh02*S2B) -
     &    (Mh02 - 2*MHp2)*S2B**2*SBA + AllHhhx(41)*AllHhhx(715) +
     &    2*(AllHhhx(718)/Mh02 +
     &       (MW2*SW2)/(Alfa*Pi)*(CBA*(CB2*Lambda6 - Lambda7*SB2)))

        AllHhhx(1186) =
     &   -3*Mh02**2 + Mh02*(MA02 + 2*MHp2) +
     &    (SBA2*AllHhhx(730))/((MA02 - Mh02)*Mh02*MHp2*S2B**2)

	AllHhhx(1190) = 2*Mh02*AllHhhx(638)**2 - MHp2*AllHhhx(701)**2

        AllHhhx(1192) =
     &   -(CBA2*MHp2*(S2B*AllHhhx(14) - 6*AllHhhx(267))) +
     &    2*AllHhhx(1191)

        AllHhhx(120) =
     &   AllHhhx(105)*AllHhhx(110) - 2*AllHhhx(119) +
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     (MHp2*S2B*SAB*(CB2*Lambda6 - Lambda7*SB2)*SBA*
     &       (96 - 58*SBA2 - 159*SBA2**2))

        AllHhhx(1205) =
     &   9*AllHhhx(42)**2 -
     &    2*CBA2*(S2B*AllHhhx(14) - 6*AllHhhx(267))**2

        AllHhhx(1211) =
     &   2*AllHhhx(701)*AllHhhx(1208) +
     &    (2*Mh02 - MHp2)*AllHhhx(1210)

        AllHhhx(1216) =
     &   ((Mh02 - MHp2)*(S2B*AllHhhx(14) - 6*AllHhhx(267))*
     &       AllHhhx(1212))/(4*Mh02 - MHp2) + Mh02*AllHhhx(1215)

        AllHhhx(1240) =
     &   (4*dup4*(Mh02 - MHp2)**2*MHp2**2*SBA2)/
     &     (Mh02*(Mh02 - 4*MHp2)**2) - 2*Mh02*AllHhhx(1224) +
     &    CBA2*AllHhhx(1229) + 2*(Mh02 - MHp2)*AllHhhx(1239)

        AllHhhx(1245) =
     &   2*CBA*dup5*(Mh02 - 4*MHp2)*AllHhhx(638) +
     &    (Mh02 - MHp2)*SBA*AllHhhx(701)*AllHhhx(1244)

        AllHhhx(1246) =
     &   CBA*(S2B*AllHhhx(14) - 6*AllHhhx(267))*AllHhhx(638) +
     &    AllHhhx(701)**2

        AllHhhx(1253) =
     &   -((AllHhhx(701)**2*AllHhhx(1248) + SBA2*AllHhhx(1249))/
     &       (Mh02 - 4*MHp2)) + AllHhhx(1252)

        AllHhhx(1272) =
     &   (4*Mh02 - MHp2)*MHp2*S2B**2*AllHhhx(1258) -
     &    2*(Mh02 - MHp2)*(S2B*AllHhhx(14) - 6*AllHhhx(267))*
     &     (AllHhhx(1268) + 2*AllHhhx(1271))

        AllHhhx(1288) =
     &   -(CBA2*Mh02*(Mh02 - 4*MHp2)**2*(4*Mh02 - MHp2)*
     &       (3*AllHhhx(42)*AllHhhx(701) - AllHhhx(1246))) +
     &    2*(Mh02 - MHp2)*(AllHhhx(1285) - 2*AllHhhx(1287))

        AllHhhx(1305) =
     &   -((Mh02 - 4*MHp2)**2*AllHhhx(1291)) -
     &    Mh02*AllHhhx(1295) +
     &    2*(SBA2*AllHhhx(1289) + (Mh02 - MHp2)*AllHhhx(1304))

        AllHhhx(1320) =
     &   (Mh02 - MHp2)*AllHhhx(1309) + 2*AllHhhx(1315) -
     &    AllHhhx(1319)

        AllHhhx(1330) =
     &   2*(MA02 - Mh02)*MHp2*SBA*AllHhhx(41) -
     &    CBA*(MA02 - MHp2)*(2*Mh02 - MHp2)*AllHhhx(52)

        AllHhhx(1331) =
     &   (2*Mh02*SBA2*AllHhhx(12))/(MA02 - Mh02) +
     &    (3*MHp2*SBA*AllHhhx(42))/(MA02 - MHp2)

        AllHhhx(1347) =
     &   -((SBA2**2*AllHhhx(12)**2)/(MA02 - MHp2)) +
     &    (CBA2*AllHhhx(1346))/(MA02 - Mh02)

	AllHhhx(1351) = 9*AllHhhx(635)**2 + 4*AllHhhx(638)**2

	AllHhhx(1355) = AllHhhx(701)**2 - SBA2**2*AllHhhx(760)**2

        AllHhhx(1359) =
     &   (MA02 - Mh02)*MHp2*AllHhhx(41)**2 +
     &    AllHhhx(12)*AllHhhx(1358)

        AllHhhx(1361) =
     &   CBA*(MA02 + Mh02 - MHp2)*SBA2*AllHhhx(12)*AllHhhx(52) +
     &    SBA*AllHhhx(41)*AllHhhx(1360)

        AllHhhx(1367) =
     &   -((MHp2*SBA*AllHhhx(41))/((MA02 - MHp2)*(Mh02 - MHp2))) +
     &    (CBA*AllHhhx(52)*AllHhhx(1366))/
     &     ((MA02 - Mh02)**2*(4*MA02 - MHp2))

        AllHhhx(1369) =
     &   CBA*MHp2*AllHhhx(41) +
     &    (2*SBA*AllHhhx(52)*AllHhhx(1333))/(4*MA02 - MHp2)

        AllHhhx(1371) =
     &   2*(MA02 - Mh02)*MHp2*SBA*AllHhhx(41) -
     &    (2*MA02 - MHp2)*AllHhhx(1370)

        AllHhhx(1374) =
     &   (MA02 - MHp2)*MHp2*SBA2**2*AllHhhx(12)**2 +
     &    AllHhhx(701)**2*AllHhhx(1373)

        AllHhhx(1376) =
     &   (2*CBA2*AllHhhx(701)**2)/(MA02 - Mh02)**2 +
     &    SBA2*AllHhhx(12)**2*AllHhhx(1375)

        AllHhhx(1377) =
     &   (CBA2*MHp2*SBA2*AllHhhx(12)*AllHhhx(15))/(MA02 - MHp2) +
     &    (CBA*Mh02*SBA*AllHhhx(638)*AllHhhx(701))/(MA02 - Mh02)

        AllHhhx(1379) =
     &   (6*CBA*Mh02*AllHhhx(635))/(Mh02 - MHp2) +
     &    AllHhhx(1378)/(MA02 - MHp2)**2

        AllHhhx(138) =
     &   AllHhhx(110)*AllHhhx(124) - 2*AllHhhx(137) +
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     (MHp2*S2B*SAB*(CB2*Lambda6 - Lambda7*SB2)*SBA*
     &       (48 - 46*SBA2 - 105*SBA2**2))

        AllHhhx(1381) =
     &   (CBA2*AllHhhx(1346))/((MA02 - Mh02)*(Mh02 - 4*MHp2)) -
     &    (4*AllHhhx(1380))/(MA02 - MHp2)**2

        AllHhhx(1390) =
     &   -((CBA*Mh02*AllHhhx(41)*AllHhhx(52))/(Mh02 - MHp2)**2) +
     &    4*AllHhhx(1387) -
     &    (AllHhhx(1368)*AllHhhx(1389))/
     &     ((MA02 - Mh02)*(MA02 - MHp2)**2)

        AllHhhx(1394) =
     &   (CBA2*AllHhhx(41))/(4*MA02 - Mh02) +
     &    (2*CBA*SBA*AllHhhx(52))/(4*MA02 - MHp2)

        AllHhhx(1399) =
     &   S2B**2*AllHhhx(1343) + AllHhhx(1398)/(MA02 - Mh02)**2

        AllHhhx(1403) =
     &   (CBA2*(Mh02 - 2*MHp2)*AllHhhx(1346))/(MA02 - Mh02) -
     &    (2*MHp2*AllHhhx(1402))/(MA02 - MHp2)

        AllHhhx(1405) =
     &   CBA2*(Mh02 - 4*MHp2)*
     &     (S2B*AllHhhx(14) - 6*AllHhhx(267))**2 +
     &    2*(4*Mh02 - MHp2)*AllHhhx(701)**2

        AllHhhx(1406) =
     &   (4*Mh02 - MHp2)*SBA2*AllHhhx(12)**2 +
     &    CBA2*(Mh02 - 4*MHp2)*
     &     (S2B*AllHhhx(14) - 6*AllHhhx(267))**2

        AllHhhx(1407) =
     &   (4*Mh02 - MHp2)*AllHhhx(1404) +
     &    CBA2*(MA02 - MHp2)*AllHhhx(1405) -
     &    (MA02 - Mh02)*SBA2*AllHhhx(1406)

        AllHhhx(1413) =
     &   ((MA02**2 + 2*MA02*Mh02)*SBA*
     &       (S2B*AllHhhx(14) - 6*AllHhhx(267))*AllHhhx(1328))/
     &     (MA02 - MHp2)**2 - 2*Mh02**2*AllHhhx(1412)

        AllHhhx(1417) =
     &   -((CBA*Mh02*AllHhhx(41)*AllHhhx(52))/(Mh02 - MHp2)**2) +
     &    4*AllHhhx(1387) -
     &    (AllHhhx(1368)*AllHhhx(1416))/
     &     ((MA02 - Mh02)*(MA02 - MHp2)**2)

        AllHhhx(1424) =
     &   (MA02 - Mh02)*AllHhhx(701)**2 +
     &    CBA2*(MA02 - MHp2)*SBA2*AllHhhx(760)**2 -
     &    CBA*(Mh02 - 4*MHp2)*SBA*AllHhhx(1423)

        AllHhhx(1427) =
     &   (CBA*MA02*AllHhhx(52))/(MA02 - Mh02) +
     &    (2*(2*MA02 - MHp2)*SBA*AllHhhx(1368))/(Mh02 - MHp2)

        AllHhhx(1428) =
     &   -(SBA2*AllHhhx(12)*AllHhhx(52)) +
     &    2*SBA*AllHhhx(638)*AllHhhx(701)

        AllHhhx(1429) =
     &   6*(MA02 - Mh02)*AllHhhx(12)*AllHhhx(635) -
     &    CBA*(MA02 - MHp2)*AllHhhx(760)**2

        AllHhhx(1435) =
     &   (2*CBA*SBA*AllHhhx(41))/(Mh02 - MHp2) +
     &    (CBA2*AllHhhx(52))/(MA02 - Mh02)

        AllHhhx(1441) =
     &   (AllHhhx(41)**2*AllHhhx(1420))/(4*MA02 - Mh02) +
     &    (CBA*AllHhhx(1440))/(MA02 - Mh02)**2

        AllHhhx(1444) =
     &   (AllHhhx(52)*AllHhhx(1435))/(4*MA02 - MHp2) +
     &    AllHhhx(1443)/(MA02 - MHp2)**2

        AllHhhx(1456) =
     &   (9*SBA*AllHhhx(42)*(S2B*AllHhhx(14) - 6*AllHhhx(267)))/
     &     (Mh02 - MHp2) +
     &    (4*(S2B*AllHhhx(14) - 6*AllHhhx(267))**2*AllHhhx(1364))/
     &     ((MA02 - MHp2)*(4*Mh02 - MHp2))

        AllHhhx(1457) =
     &   (MA02*SBA*(S2B*AllHhhx(14) - 6*AllHhhx(267))*
     &       AllHhhx(1328))/((MA02 - Mh02)*(MA02 - MHp2)**2) -
     &    (4*(S2B*AllHhhx(14) - 6*AllHhhx(267))**2*AllHhhx(1409))/
     &     (4*Mh02 - MHp2)

        AllHhhx(1486) =
     &   Mh02*AllHhhx(1441) -
     &    (2*AllHhhx(41)**2*AllHhhx(1485))/(4*MA02 - Mh02)

        AllHhhx(1492) =
     &   CBA*AllHhhx(638) +
     &    (4*(Mh02 - MHp2)*SBA*AllHhhx(701))/(4*Mh02 - MHp2)

        AllHhhx(1603) =
     &   -2*S2B**4*AllHhhx(1600) + 8*m12sq**2*AllHhhx(1601) +
     &    S2B**2*AllHhhx(1602) +
     &    (24*MW2**2*SW2**2)/(Alfa2*Pi**2)*AllHhhx(62)

        AllHhhx(1652) =
     &   AllHhhx(1651)**2 +
     &    (12*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA*
     &       (6*m12sq*S2A - S2B*AllHhhx(14))) +
     &    (18*MW2**2*SW2**2)/(Alfa2*Pi**2)*(CBA2*SBA2*AllHhhx(62))

        AllHhhx(1654) =
     &   AllHhhx(1653)**2 +
     &    (18*MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (CBA2*SBA2*AllHhhx(62)) -
     &    (12*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA*AllHhhx(1653))

        AllHhhx(1659) =
     &   8*AllHhhx(1648) + CBA2*MHp2**2*AllHhhx(1655) -
     &    36*SAB*(MHp2*S2B - 2*m12sq*SBA2)*AllHhhx(1656) +
     &    4*SBA2*AllHhhx(1658) +
     &    (18*MW2**2*SW2**2)/(Alfa2*Pi**2)*(SBA2**3*AllHhhx(62))

        AllHhhx(1663) =
     &   CBA2*(2*MA02 - MHp2)**2*S2B**4 + 4*AllHhhx(1662) +
     &    (2*MW2**2*SW2**2)/(Alfa2*Pi**2)*(SBA2*AllHhhx(62))

        AllHhhx(1670) =
     &   S2B**4*AllHhhx(1666) + 4*AllHhhx(1669) +
     &    (2*MW2**2*SW2**2)/(Alfa2*Pi**2)*AllHhhx(62)

        AllHhhx(1689) =
     &   (2*MA02 - Mh02)**2*S2B**4 + AllHhhx(1653)**2 +
     &    (18*MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (CBA2*SBA2*AllHhhx(62)) -
     &    (12*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA*AllHhhx(1653))

        AllHhhx(1695) =
     &   -((Mh02 - 2*MHp2)**2*S2B**4*SBA2) -
     &    4*CAB*(2*m12sq - Mh02*S2B)*AllHhhx(1643) -
     &    CBA2*AllHhhx(1694) +
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*AllHhhx(1644))

        AllHhhx(1698) =
     &   4*CAB**2*(2*m12sq - Mh02*S2B)**2 +
     &    (2*MA02 - Mh02)**2*S2B**4*SBA2 - 4*AllHhhx(1688) +
     &    (2*MW2**2*SW2**2)/(Alfa2*Pi**2)*(CBA2*AllHhhx(62))

	AllHhhx(170) = (MA02 - Mh02)*S2B**2*SBA2**2 + 2*AllHhhx(169)

        AllHhhx(1704) =
     &   8*CAB*(2*m12sq - Mh02*S2B)*AllHhhx(1643) +
     &    SBA2*AllHhhx(1703) +
     &    (4*MW2**2*SW2**2)/(Alfa2*Pi**2)*(CBA2*AllHhhx(62)) -
     &    (8*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*AllHhhx(1644))

        AllHhhx(1707) =
     &   CAB*(2*MA02 - Mh02)*S2B**2*(2*m12sq - Mh02*S2B)*SBA +
     &    S2B**4*AllHhhx(1697) +
     &    (MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*AllHhhx(1687)) +
     &    AllHhhx(1698)*Re(AllHhhl(13)) +
     &    AllHhhx(1663)*Re(AllHhhl(14))

        AllHhhx(1734) =
     &   10*CB2*CBA2*Lambda6*(2*m12sq - MHp2*S2B)*SAB +
     &    Lambda7*SB2*AllHhhx(1727) + SBA2*AllHhhx(1730) +
     &    3*SBA2**2*AllHhhx(1732) + 18*AllHhhx(1733)

        AllHhhx(1745) =
     &   CB2*Lambda6*SBA*AllHhhx(1726) +
     &    2*CAB*(CB2*Lambda6 - Lambda7*SB2)*AllHhhx(1744)

        AllHhhx(1768) =
     &   -8*(CAB**2*AllHhhx(1747) + CAB*SBA*AllHhhx(1761)) -
     &    SBA2*AllHhhx(1765) +
     &    (4*MW2*SW2)/(Alfa*Pi)*(CBA*AllHhhx(1767))

        AllHhhx(1794) =
     &   4*AllHhhx(1669) + S2B**4*AllHhhx(1793) +
     &    (2*MW2**2*SW2**2)/(Alfa2*Pi**2)*AllHhhx(62)

        AllHhhx(1803) =
     &   -4*CAB**2*(2*m12sq - Mh02*S2B)**2 -
     &    S2B**4*AllHhhx(1686) + 4*AllHhhx(1688) -
     &    SBA2*AllHhhx(1689) - 9*AllHhhx(1693) +
     &    2*(AllHhhx(1695) + AllHhhx(1794))

        AllHhhx(1804) =
     &   CBA2*dup1 + CBA2**2*(Mh02 - MHp2)**2*S2B**4*Re(AllHhhl(5))

        AllHhhx(1809) =
     &   S2B**4*AllHhhx(1808) + AllHhhx(1645)*Re(AllHhhl(6)) +
     &    AllHhhx(1649)*Re(AllHhhl(7))

        AllHhhx(1812) =
     &   4*AllHhhx(1662) + 9*AllHhhx(1672) + 2*AllHhhx(1674) +
     &    S2B**4*AllHhhx(1810) + CBA2*AllHhhx(1811)

        AllHhhx(1814) =
     &   S2B**4*AllHhhx(1813) +
     &    2*SBA2*AllHhhx(1654)*Re(AllHhhl(6)) +
     &    AllHhhx(1659)*Re(AllHhhl(7)) +
     &    AllHhhx(1663)*Re(AllHhhl(9)) +
     &    CBA2*AllHhhx(1652)*Re(AllHhhl(10))

        AllHhhx(1815) =
     &   (MA02 - MHp2)*(MA02 + MHp2)*
     &    (AllHhhx(1812) + 2*AllHhhx(1814))

	AllHhhx(1820) = (1 - SBA2)*AllHhhx(1684) + SBA2*AllHhhx(1814)

        AllHhhx(183) =
     &   2*MA02**3*S2B**2*(3 + 4*SBA2) +
     &    MA02*Mh02*(Mh02*S2B*AllHhhx(175) + 2*AllHhhx(177)) +
     &    Mh02**2*(Mh02*S2B*AllHhhx(178) - 2*AllHhhx(180)) -
     &    MA02**2*(Mh02*S2B*AllHhhx(181) - 2*AllHhhx(182))

	AllHhhx(195) = 2*Mh02*S2B**2 + 3*SBA*AllHhhx(36)

        AllHhhx(219) =
     &   Mh02**2*S2B*AllHhhx(216) - MHp2*AllHhhx(217) +
     &    2*Mh02*(2*SBA*AllHhhx(36) - MHp2*S2B*AllHhhx(218))

        AllHhhx(228) =
     &   Mh02**3*S2B*AllHhhx(220) +
     &    MHp2**2*(4*SBA*AllHhhx(36) - MHp2*S2B*AllHhhx(221)) +
     &    Mh02**2*AllHhhx(224) - Mh02*MHp2*AllHhhx(227)

        AllHhhx(268) =
     &   SBA2*AllHhhx(12) + SBA*AllHhhx(41) +
     &    (CBA2*AllHhhx(266)*(S2B*AllHhhx(14) - 6*AllHhhx(267)))/
     &     ((MA02 - Mh02)*(MA02 - MHp2))

        AllHhhx(283) =
     &   (MA02 - MHp2)*(2 + SBA2) -
     &    AllHhhx(282)/((MA0 - MHp)*(MA0 + MHp)*MHp2*S2B**4)

        AllHhhx(313) =
     &   2*MHp2**2*S2B**2*SAB**2 -
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     (MHp2*S2B*SAB*(CB2*Lambda6 - Lambda7*SB2)*SBA) +
     &    (MW2**2*SW2**2)/(Alfa2*Pi**2)*(SBA2*AllHhhx(62))

        AllHhhx(314) =
     &   4*MHp2**2*S2B**2*SAB**2*(1 + 3*SBA2) -
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     (MHp2*S2B*SAB*(CB2*Lambda6 - Lambda7*SB2)*SBA*
     &       (2 - 3*SBA2 + 9*SBA2**2)) +
     &    (MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (SBA2*(2 - 3*SBA2 + 9*SBA2**3)*AllHhhx(62))

        AllHhhx(315) =
     &   4*MHp2**2*S2B**2*SAB**2*(4 + 5*SBA2) -
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     (MHp2*S2B*SAB*(CB2*Lambda6 - Lambda7*SB2)*SBA*
     &       (8 + SBA2 + 9*SBA2**2)) +
     &    (MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (SBA2*(8 + SBA2 + 9*SBA2**3)*AllHhhx(62))

	AllHhhx(337) = MA02**2*MHp2**2*AllHhhx(316) - CBA*AllHhhx(336)

        AllHhhx(37) = Mh02*S2B*AllHhhx(35) -
     &    2*(1 - 3*CBA2)*SBA*AllHhhx(36)

        AllHhhx(39) = -2*(1 - 3*CBA2)*SBA*AllHhhx(36) +
     &    Mh02*S2B*AllHhhx(38)

        AllHhhx(408) =
     &   MHp2**2*(MHp2*S2B*AllHhhx(395) + 8*CBA*AllHhhx(397)) -
     &    S2B**2*AllHhhx(399) + 2*MA02*MHp2*AllHhhx(403) +
     &    MA02**2*AllHhhx(407)

        AllHhhx(421) =
     &   2*CBA*SBA*(4 + SBA2)*AllHhhx(9) + S2B*SBA2*AllHhhx(420)

        AllHhhx(443) =
     &   2*CBA*SBA*(4 + SBA2)*AllHhhx(9) + SBA2*AllHhhx(442)

        AllHhhx(617) =
     &   CBA*AllHhhx(616) +
     &    (MW2*SW2)/(Alfa*Pi)*(MHp2*SBA2*AllHhhx(609))

        AllHhhx(632) =
     &   -(SBA2*AllHhhx(52)*AllHhhx(290)) +
     &    ((MA02 - MHp2)*AllHhhx(52)**2*AllHhhx(631))/
     &     ((4*MA02 - MHp2)**2*MHp2)

        AllHhhx(64) = 2*SAB**2*AllHhhx(60) -
     &    2*CBA*MA02*AllHhhx(63) -
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     (MHp2*S2B*SAB*(CB2*Lambda6 - Lambda7*SB2)*SBA) +
     &    (MW2**2*SW2**2)/(Alfa2*Pi**2)*(SBA2*AllHhhx(62))

	AllHhhx(640) = (4*MA02 - MHp2)*AllHhhx(639)

        AllHhhx(677) =
     &   MHp2*S2B**4*(CB2*Lambda6 - Lambda7*SB2)*AllHhhx(69) -
     &    6*SBA2*AllHhhx(676)

        AllHhhx(686) =
     &   -24*m12sq*Mh02*(Mh02 - MHp2)**2*(3*Mh02 - MHp2)*S2A +
     &    (Mh02 - MHp2)*AllHhhx(685)

        AllHhhx(744) =
     &   -2*CBA*AllHhhx(85) + CBA2*AllHhhx(740) +
     &    ((MA02 - Mh02)*SBA2*AllHhhx(743))/(Mh02*(MA02 - MHp2))

        AllHhhx(762) =
     &   CBA2*S2B**2*AllHhhx(758) -
     &    (MA02 - MHp2)*(AllHhhx(759)*AllHhhx(760) +
     &       MHp2*AllHhhx(761))

        AllHhhx(83) = CBA2*(Mh02 - 4*MHp2)*MHp2*S2B**4 -
     &    (Mh02 - MHp2)*S2B**2*AllHhhx(82)

        AllHhhx(86) = CBA2*MHp2*S2B**2 -
     &    (4*(Mh02 - MHp2)*SBA2*AllHhhx(12))/(Mh02 - 4*MHp2) +
     &    2*(CBA2*AllHhhx(84) - 2*CBA*AllHhhx(85))

        AllHhhx(905) =
     &   MA02**3*MHp2*AllHhhx(900) + MA02*MHp2**3*AllHhhx(904)

        AllHhhx(941) =
     &   MA02**3*MHp2*AllHhhx(911) + MA02*MHp2**3*AllHhhx(912) +
     &    CBA*AllHhhx(940)

        AllHhhx(990) =
     &   MA02**4*S2B*AllHhhx(957) + MA02**3*AllHhhx(962) +
     &    MA02*AllHhhx(972) - MA02**2*AllHhhx(979) -
     &    MHp2*AllHhhx(989)

	AllHhhx(102) = -8*m12sq*SAB*AllHhhx(91) + S2B**2*AllHhhx(101)

        AllHhhx(1084) =
     &   -48*MA02**4*S2B**4*SBA2 - 16*CAB**2*AllHhhx(1064) +
     &    MA02*Mh02*AllHhhx(1067) - Mh02**2*AllHhhx(1070) +
     &    4*AllHhhx(1083)

        AllHhhx(1144) =
     &   6*AllHhhl(27)*AllHhhx(42)*AllHhhx(1088) -
     &    2*AllHhhx(1091) - AllHhhx(1143)/(4*MA02 - Mh02)

	AllHhhx(1163) = dup6/Mh02 + CBA2*AllHhhx(1162)

        AllHhhx(1185) =
     &   CBA*AllHhhx(52)*AllHhhx(712) -
     &    CBA2*(S2B*AllHhhx(14) - 6*AllHhhx(267))*AllHhhx(714) +
     &    SBA2*AllHhhx(12)*AllHhhx(1183) - SBA*AllHhhx(1184)

        AllHhhx(1198) =
     &   -2*dup7*(Mh02 - MHp2) - 3*CBA*AllHhhx(1190) +
     &    3*(SBA2*AllHhhx(12)*
     &        (CBA*MHp2*AllHhhx(12) + 6*Mh02*AllHhhx(635)) +
     &       AllHhhx(638)*AllHhhx(1192))

	AllHhhx(121) = 8*m12sq*SAB*AllHhhx(109) + S2B**2*AllHhhx(120)

        AllHhhx(1217) =
     &   -(((Mh02 - MHp2)**2*SBA2*
     &         (S2B*AllHhhx(14) - 6*AllHhhx(267))**2*AllHhhx(1204))
     &        /((4*Mh02 - MHp2)**2*MHp2)) - MHp2*AllHhhx(1205) +
     &    AllHhhx(1211) - 2*AllHhhx(1216)

        AllHhhx(1241) =
     &   -(AllHhhx(1240)/(Mh02 - MHp2)**2) +
     &    (4*(Sqrt(CW2) + SW)**2*AllHhhx(701)**2*
     &       (1 - 2*Sqrt(CW2)*SW))/(Mh02 - 4*MHp2)

        AllHhhx(1247) =
     &   (-2*(Mh02 - MHp2)*AllHhhx(1245))/(Mh02 - 4*MHp2)**2 -
     &    CBA2*(3*AllHhhx(42)*AllHhhx(701) - AllHhhx(1246))

        AllHhhx(1254) =
     &   -(AllHhhx(1253)/(Mh02 - MHp2)) +
     &    (2*(Sqrt(CW2) + SW)**2*AllHhhx(701)**2*
     &       (1 - 2*Sqrt(CW2)*SW))/((Mh02 - 4*MHp2)*MHp2)

        AllHhhx(1321) =
     &   (4*Mh02 - MHp2)**2*AllHhhx(1305) -
     &    2*((4*Mh02 - MHp2)*MHp2*AllHhhx(1288) +
     &       (Mh02 - MHp2)*AllHhhx(1320)) +
     &    32*CW2*SW2*(Mh02*(Mh02 - 4*MHp2)*AllHhhx(69)**2*
     &       AllHhhx(701)**2)

        AllHhhx(1329) =
     &   (-6*Mh02*AllHhhl(27)*AllHhhx(42))/(Mh02 - MHp2) +
     &    (MA02*AllHhhl(26)*AllHhhx(1328))/(MA02 - MHp2)**2

        AllHhhx(1335) =
     &   AllHhhx(1330)/((MA02 - Mh02)**2*(MA02 - MHp2)) +
     &    (2*AllHhhx(1331))/(Mh02 - MHp2) +
     &    (S2B*AllHhhx(14) - 6*AllHhhx(267))*AllHhhx(1334)

        AllHhhx(1344) =
     &   CAB**2*(2*m12sq - Mh02*S2B)**2 +
     &    CB2**2*AllHhhx(1337)*AllHhhx(1338) -
     &    CAB*(2*m12sq - Mh02*S2B)*AllHhhx(1339) +
     &    (MW2**2*SW2**2)/(Alfa2*Pi**2)*(CBA2*Lambda7**2*SB2**2) +
     &    1/4.D0*(SBA*SBA2*AllHhhx(12)*AllHhhx(41)) +
     &    S2B**2*(Mh02*SBA*AllHhhx(1341) + 1/4.D0*AllHhhx(1343))

        AllHhhx(1348) =
     &   AllHhhx(1345)**2/(MA02 - MHp2) + 1/4.D0*AllHhhx(1347)

        AllHhhx(1352) =
     &   -27*AllHhhx(635)**2 - 12*AllHhhx(638)**2 + 2*AllHhhx(1351)

        AllHhhx(1354) =
     &   Mh02*AllHhhx(52)**2 -
     &    CBA*(MA02 + Mh02 - MHp2)*AllHhhx(52)*
     &     (S2B*AllHhhx(14) - 6*AllHhhx(267)) +
     &    CBA2*MA02*(S2B*AllHhhx(14) - 6*AllHhhx(267))**2

        AllHhhx(1357) =
     &   MHp2*AllHhhx(41)**2 + MA02*AllHhhx(1355) -
     &    4*(MA02 - Mh02 + MHp2)*AllHhhx(1356)

        AllHhhx(1362) =
     &   SBA2*AllHhhx(1359) - (MA02 - MHp2)*AllHhhx(1361)

        AllHhhx(1365) =
     &   CBA*AllHhhx(52) -
     &    (2*(MA02 - Mh02)*AllHhhx(1231)*AllHhhx(1364))/
     &     ((MA02 - MHp2)*(4*Mh02 - MHp2))

        AllHhhx(1372) =
     &   2*(AllHhhx(52)*AllHhhx(1367) +
     &       (AllHhhx(1368)*AllHhhx(1369))/(Mh02 - MHp2)**2) -
     &    (AllHhhx(52)*AllHhhx(1371))/
     &     ((MA02 - Mh02)**2*(MA02 - MHp2))

        AllHhhx(1382) =
     &   (2*AllHhhx(1374))/((Mh02 - 4*MHp2)*(MA02 - MHp2)**2) -
     &    MA02*AllHhhx(1376) - (4*AllHhhx(1377))/(Mh02 - MHp2) +
     &    (SBA2*AllHhhx(12)*AllHhhx(1379))/(MA02 - Mh02) -
     &    (Mh02 - 2*MHp2)*AllHhhx(1381)

	AllHhhx(139) = 8*m12sq*SAB*AllHhhx(128) + S2B**2*AllHhhx(138)

        AllHhhx(1391) =
     &   (4*(2*MA02 - Mh02)*AllHhhx(1380))/(MA02 - MHp2)**2 +
     &    AllHhhx(41)**2*AllHhhx(1385) + 2*SBA*AllHhhx(1390)

        AllHhhx(1395) =
     &   -((CBA2*AllHhhx(52)**2)/
     &       ((MA02 - Mh02)*(4*MA02 - MHp2))) +
     &    (4*(1 - SBA2)*AllHhhx(1393)**2)/
     &     ((4*MA02 - Mh02)*(MA02 - MHp2)) -
     &    (AllHhhx(41)*AllHhhx(1394))/(Mh02 - MHp2)

        AllHhhx(1400) =
     &   CAB**2*(2*m12sq - Mh02*S2B)**2 +
     &    CB2**2*AllHhhx(1337)*AllHhhx(1338) -
     &    CAB*(2*m12sq - Mh02*S2B)*AllHhhx(1339) +
     &    Mh02*S2B**2*SBA*AllHhhx(1341) +
     &    (MW2**2*SW2**2)/(Alfa2*Pi**2)*(CBA2*Lambda7**2*SB2**2) +
     &    1/4.D0*AllHhhx(1399)

        AllHhhx(1408) =
     &   4*MHp2**2*AllHhhx(1348) + (Mh02 + 2*MHp2)*AllHhhx(1403) -
     &    (Mh02*MHp2*AllHhhx(1407))/
     &     ((MA02 - Mh02)*(MA02 - MHp2)*(4*Mh02 - MHp2))

        AllHhhx(1418) =
     &   (4*(2*MA02 - Mh02)*AllHhhx(1356))/(MA02 - MHp2)**2 +
     &    AllHhhx(41)**2*AllHhhx(1385) + 2*SBA*AllHhhx(1417)

        AllHhhx(1425) =
     &   CBA2*(MA02 - MHp2)*AllHhhx(1405) -
     &    (MA02 - Mh02)*SBA2*AllHhhx(1406) +
     &    (4*Mh02 - MHp2)*AllHhhx(1424)

        AllHhhx(1426) =
     &   (2*AllHhhx(41)**2*AllHhhx(1421))/(4*MA02 - Mh02) +
     &    (Mh02*MHp2*AllHhhx(1425))/
     &     ((MA02 - Mh02)*(Mh02 - 4*MHp2)*(4*Mh02 - MHp2))

        AllHhhx(143) =
     &   2*CBA*AllHhhx(9) + S2B*(2*CAB*Mh02 + SBA*AllHhhx(142))

        AllHhhx(1430) =
     &   CBA*(MA02 - MHp2)*AllHhhx(701)**2 +
     &    (MA02 - Mh02)*AllHhhx(1428) - 1/2.D0*(SBA2*AllHhhx(1429))

	dup8 = 4*CBA*AllHhhx(1427) - MHp2*AllHhhx(1435)

        AllHhhx(1445) =
     &   (2*dup8*AllHhhx(52))/(4*MA02 - MHp2) +
     &    (Mh02*AllHhhx(1441))/(MA02 - MHp2) + MHp2*AllHhhx(1444)

        AllHhhx(1484) =
     &   (AllHhhl(2)*AllHhhx(1441))/(MA02 - MHp2) +
     &    AllHhhl(3)*AllHhhx(1444)

        AllHhhx(1493) =
     &   -3*AllHhhx(42)*AllHhhx(701) + AllHhhx(701)**2 +
     &    (S2B*AllHhhx(14) - 6*AllHhhx(267))*AllHhhx(1492)

        AllHhhx(1588) =
     &   6*CBA2*m12sq*(6*m12sq*S2A - S2B*AllHhhx(14)) +
     &    S2B*(Mh02*S2B*AllHhhx(1586) - 2*m12sq*AllHhhx(1587))

        AllHhhx(1604) =
     &   2*m12sq*AllHhhx(1599) - SBA*AllHhhx(1603) +
     &    (12*MW2*SW2)/(Alfa*Pi)*
     &     (MHp2*S2B*SAB*(CB2*Lambda6 - Lambda7*SB2)*(1 + 3*SBA2))

        AllHhhx(1664) =
     &   2*SBA2*AllHhhx(1654)*Re(AllHhhl(6)) +
     &    AllHhhx(1659)*Re(AllHhhl(7)) +
     &    AllHhhx(1663)*Re(AllHhhl(9)) +
     &    CBA2*AllHhhx(1652)*Re(AllHhhl(10))

        AllHhhx(1671) =
     &   (MA02 - MHp2)**2 + CBA2*(Mh02 - MHp2)**2 +
     &    AllHhhx(1670)/S2B**4

        AllHhhx(1696) =
     &   -4*CAB**2*(2*m12sq - Mh02*S2B)**2 -
     &    S2B**4*AllHhhx(1686) + 4*AllHhhx(1688) -
     &    SBA2*AllHhhx(1689) - 9*AllHhhx(1693) + 2*AllHhhx(1695)

        AllHhhx(1699) =
     &   S2B**4*AllHhhx(1697) + AllHhhx(1698)*Re(AllHhhl(13)) +
     &    AllHhhx(1663)*Re(AllHhhl(14))

        AllHhhx(1705) =
     &   AllHhhx(1698)*Re(AllHhhl(18)) +
     &    9*AllHhhx(1693)*Re(AllHhhl(19)) +
     &    2*CBA2*AllHhhx(1652)*Re(AllHhhl(20)) +
     &    AllHhhx(1704)*Re(AllHhhl(21))

        AllHhhx(1708) =
     &   -4*CAB**2*(2*m12sq - Mh02*S2B)**2 -
     &    S2B**4*AllHhhx(1686) - SBA2*AllHhhx(1689) -
     &    9*AllHhhx(1693) + 2*(AllHhhx(1684) + AllHhhx(1695)) +
     &    4*AllHhhx(1707)

        AllHhhx(171) =
     &   6*CBA2**2*(MA02 - Mh02)*
     &     (Mh02*S2B**2 - 2*SBA*AllHhhx(36)) -
     &    S2B*SBA2*AllHhhx(162) + CBA2*AllHhhx(168) -
     &    Mh02*AllHhhx(170)

        AllHhhx(1735) =
     &   6*CAB*CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA2*
     &     AllHhhx(1589) + SBA*AllHhhx(1734)

        AllHhhx(1746) =
     &   SAB*(SBA2*AllHhhx(1742) - 6*SBA2**2*AllHhhx(1743)) +
     &    (MW2*SW2)/(Alfa*Pi)*AllHhhx(1745)

        AllHhhx(1769) =
     &   -(CBA2*AllHhhx(1768)) +
     &    (2*MW2**2*SW2**2)/(Alfa2*Pi**2)*(CBA2**2*AllHhhx(1760))

        AllHhhx(1802) =
     &   (Mh02 - 2*MHp2)*AllHhhx(1705) +
     &    2*Mh02*(MA02 - MHp2)**2*S2B**4*Re(AllHhhl(4))

        AllHhhx(1819) =
     &   (MA02 - 2*MHp2)*(1 - SBA2)*AllHhhx(1699) +
     &    MA02*AllHhhx(1809)

        AllHhhx(1821) =
     &   2*AllHhhx(1794) - SBA2*AllHhhx(1812) - 2*AllHhhx(1820)

        AllHhhx(184) =
     &   4*CBA2**2*(MA02 - Mh02)**2*(MA02 + Mh02)*S2B**2 +
     &    (MA02 - Mh02)*AllHhhx(174) + CBA2*AllHhhx(183)

        AllHhhx(196) =
     &   MA02**2*S2B**2 + Mh02*(Mh02*S2B**2 - 3*SBA*AllHhhx(36)) +
     &    MA02*AllHhhx(195)

	AllHhhx(22) = 5*MHp2*S2B*SAB*SBA2 - SBA*(4 + SBA2)*AllHhhx(9)

        AllHhhx(25) = -4*CBA2**2*(2*Mh02 + MHp2)*S2B**2 +
     &    8*CBA*AllHhhx(22) + CBA2*S2B*AllHhhx(24)

        AllHhhx(257) =
     &   MA02**5*S2B**2*(MHp2 - Mh02*(3 - 8*SBA2)) +
     &    MA02**4*AllHhhx(219) + MA02**3*AllHhhx(228) +
     &    Mh02*MHp2*AllHhhx(236) - MA02*AllHhhx(247) -
     &    MA02**2*AllHhhx(256)

        AllHhhx(265) =
     &   (CBA2*AllHhhx(143))/(MA02 - MHp2) +
     &    (CBA*AllHhhx(264))/(Mh02 - MHp2)

        AllHhhx(269) =
     &   (CBA2*(MA02 - Mh02)*MHp2)/(MA02 - MHp2) -
     &    AllHhhx(268)/S2B**2

	AllHhhx(271) = dup2/(MA02 - Mh02) + AllHhhx(264)/(Mh02 - MHp2)

        AllHhhx(284) =
     &   AllHhhx(272)/(Mh02 - MHp2) - AllHhhx(283)/(MA02 - Mh02)

        AllHhhx(29) = CBA2**2*(2*Mh02 + MHp2)*S2B**2 +
     &    2*CBA*AllHhhx(22) + CBA2*S2B*AllHhhx(28)

        AllHhhx(291) =
     &   ((MA02 - MHp2)*AllHhhx(52)**2*AllHhhx(287))/
     &     ((4*MA02 - MHp2)**2*MHp2) +
     &    SBA2*AllHhhx(52)*AllHhhx(290)

        AllHhhx(338) =
     &   8*m12sq**2*SAB**2*AllHhhx(312) +
     &    2*SBA2*AllHhhx(299)*AllHhhx(313) +
     &    16*MA02**3*MHp2*AllHhhx(314) +
     &    MA02*MHp2**3*AllHhhx(315) - 2*AllHhhx(337)

        AllHhhx(369) =
     &   4*MA02**4*S2B**2*(5 - 3*SBA2 - 2*SBA2**2) +
     &    2*MA02**3*(4*CBA*SBA*AllHhhx(9) -
     &       MHp2*S2B*AllHhhx(359)) -
     &    MA02**2*MHp2*(MHp2*S2B*AllHhhx(360) +
     &       2*CBA*AllHhhx(362)) -
     &    MHp2**3*(MHp2*S2B*AllHhhx(363) - 8*CBA*AllHhhx(365)) +
     &    MA02*MHp2**2*(3*MHp2*S2B*AllHhhx(366) -
     &       2*CBA*AllHhhx(368))

        AllHhhx(380) =
     &   -2*CBA*(1 - 3*SBA2)*SBA2*AllHhhx(56) +
     &    MHp2*S2B*AllHhhx(379)

        AllHhhx(382) =
     &   2*CBA*(1 - 3*SBA2)*SBA2*AllHhhx(56) +
     &    MHp2*S2B*AllHhhx(381)

        AllHhhx(40) = MA02**3*S2B**2*SBA2 +
     &    Mh02**2*AllHhhx(37) - MA02*Mh02*AllHhhx(39)

        AllHhhx(409) =
     &   CBA2*S2B*AllHhhx(394) - 2*(MA02 - MHp2)*AllHhhx(408)

        AllHhhx(422) =
     &   CBA2**2*(Mh02 - 4*MHp2)*(Mh02 - MHp2)*(4*Mh02 - MHp2)*
     &     AllHhhx(414) - MHp2*AllHhhx(415)*
     &     (S2B*SBA2*AllHhhx(418) - Mh02*AllHhhx(421))

        AllHhhx(444) =
     &   CBA2**2*Mh02*(Mh02 - MHp2)**2*(MA02 + 5*Mh02 + MHp2)*
     &     S2B**6*AllHhhx(415) -
     &    MHp2*S2B**4*AllHhhx(441)*
     &     (S2B*SBA2*AllHhhx(418) - Mh02*AllHhhx(443))

        AllHhhx(45) = 2*AllHhhl(25)*AllHhhx(41) -
     &    6*AllHhhl(27)*AllHhhx(42) -
     &    (MA02*AllHhhl(26)*
     &       (S2B*SBA*AllHhhx(43) + 4*Mh02*AllHhhx(44)))/Mh02**2

        AllHhhx(57) = MHp2*S2B*AllHhhx(53) +
     &    (AllHhhx(52)*AllHhhx(54))/(4*MA02 - MHp2) +
     &    2*AllHhhx(55)*AllHhhx(56)

        AllHhhx(618) =
     &   4*(SAB**2*AllHhhx(607) + MHp2**2*S2B*SAB*AllHhhx(608)) -
     &    2*AllHhhx(617) +
     &    (3*MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (MA02*SBA2**2*(1 - (2 - 3*SBA2)*SBA2)*AllHhhx(62))

        AllHhhx(630) =
     &   S2B**4*SBA2*AllHhhx(52)*AllHhhx(290) +
     &    MHp2**2*AllHhhx(620) - 2*Lambda7*AllHhhx(622) +
     &    2*AllHhhx(626) + 4*AllHhhx(629)

        AllHhhx(641) =
     &   2*(4*MA02 - MHp2)*AllHhhx(638)**2 - SBA2*AllHhhx(640)

	AllHhhx(65) = -8*m12sq*SAB*AllHhhx(59) + S2B**2*AllHhhx(64)

        AllHhhx(66) = CBA2*(2*MA02 - MHp2)*MHp2*S2B**8 +
     &    2*S2B**2*AllHhhx(65)

        AllHhhx(687) =
     &   S2B**4*(2*Mh02*AllHhhx(69)*AllHhhx(679) -
     &       SBA2*AllHhhx(686)) +
     &    (4*MW2*SW2)/(Alfa*Pi)*(CBA*Mh02*SBA*AllHhhx(677))

        AllHhhx(704) =
     &   SBA*AllHhhx(701) -
     &    (SBA2*AllHhhx(12)*AllHhhx(703))/
     &     ((MA02 - Mh02)*(MA02 - MHp2)*MHp2)

        AllHhhx(708) =
     &   -((MA02*(MA02 - MHp2)*SBA2*AllHhhl(34)*AllHhhx(12))/
     &       MHp2) + (AllHhhl(33)*
     &       (S2B*AllHhhx(14) - 6*AllHhhx(267))*AllHhhx(707))/
     &     (Mh02*(MA02 - MHp2))

        AllHhhx(719) =
     &   CBA*AllHhhx(52)*AllHhhx(712) -
     &    SBA2*AllHhhx(12)*AllHhhx(713) -
     &    CBA2*(S2B*AllHhhx(14) - 6*AllHhhx(267))*AllHhhx(714) -
     &    SBA*(AllHhhx(41)*AllHhhx(715) + (2*AllHhhx(718))/Mh02)

        AllHhhx(745) =
     &   AllHhhx(739)/((MA02 - MHp2)**2*(Mh02 - MHp2)) +
     &    ((MA02 - Mh02)*AllHhhx(744))/S2B**2

        AllHhhx(763) =
     &   (MA02 - MHp2)**2*SBA2**2 -
     &    AllHhhx(756)/(Mh02 - MHp2)**2 -
     &    (SBA2*AllHhhx(762))/((MA02 - Mh02)*MHp2*S2B**2)

        AllHhhx(87) = (-4*AllHhhx(83))/(Mh02 - 4*MHp2) +
     &    S2B**2*AllHhhx(86)

        AllHhhx(906) =
     &   MA02**2*MHp2**2*AllHhhx(895) + S2B**2*AllHhhx(897) -
     &    2*AllHhhx(905)

        AllHhhx(942) =
     &   52*MA02**4*SBA2*AllHhhx(313) -
     &    8*m12sq**2*SAB**2*AllHhhx(908) +
     &    MA02**2*MHp2**2*AllHhhx(909) + MHp2**4*AllHhhx(910) -
     &    2*AllHhhx(941)

        AllHhhx(991) =
     &   (MA02 - MHp2)**2*AllHhhx(954) + CBA2*MHp2*AllHhhx(990)

        AllHhhx(103) =
     &   CBA2*MHp2**2*S2B**4*AllHhhx(90) + 2*AllHhhx(102)

        AllHhhx(1058) =
     &   (-4*AllHhhl(3)*AllHhhx(991))/
     &     ((MA02 - MHp2)**2*(Mh02 - MHp2)*MHp2) +
     &    AllHhhx(1057)/((MA0 - MHp)*(MA0 + MHp))

        AllHhhx(1145) =
     &   AllHhhx(1084)/AllHhhx(1060) -
     &    (4*AllHhhx(1144))/(MA02 - Mh02)**2

        AllHhhx(1164) =
     &   (CBA2*(2*CBA*AllHhhx(85) - CBA2*AllHhhx(740)))/MHp2 +
     &    SBA2*AllHhhx(1163)

        AllHhhx(1179) =
     &   2*Mh02*(MHp2*AllHhhx(1166) + 2*AllHhhx(1168)) +
     &    AllHhhx(1176)/(Mh02 - MHp2) -
     &    (2*(S2B*AllHhhx(14) - 6*AllHhhx(267))*AllHhhx(1178))/
     &     S2B**2

        AllHhhx(1181) =
     &   CBA2*AllHhhx(1150) + (2*AllHhhx(1158))/(MA02 - Mh02) +
     &    AllHhhx(1164)/S2B**2 + AllHhhx(1180)/(Mh02*(MA02 - MHp2))

        AllHhhx(1218) =
     &   CBA*AllHhhl(36)*AllHhhx(1198) -
     &    3*CBA2*(3*AllHhhl(27)*AllHhhx(42)*AllHhhx(1203) +
     &       AllHhhl(33)*AllHhhx(1217))

        AllHhhx(122) =
     &   -(CBA2*MHp2**2*S2B**4*AllHhhx(104)) + 2*AllHhhx(121)

        AllHhhx(1242) =
     &   AllHhhx(1241) + 16*CW2*SW2*AllHhhx(701)**2/(Mh02 - 4*MHp2)

        AllHhhx(1255) =
     &   AllHhhx(1247)/(Mh02 - MHp2)**2 + AllHhhx(1254) +
     &    8*CW2*SW2*AllHhhx(701)**2/((Mh02 - 4*MHp2)*MHp2)

        AllHhhx(1336) =
     &   (SBA*AllHhhx(1329))/(MA02 - Mh02) +
     &    AllHhhl(33)*AllHhhx(1335)

        AllHhhx(1349) =
     &   AllHhhx(1344)/(MA02 - MHp2)**2 -
     &    (2*AllHhhx(1348))/(Mh02 - 4*MHp2)

        AllHhhx(1353) =
     &   -(SBA2*(8*AllHhhx(638)*AllHhhx(1350) - AllHhhx(1352))) +
     &    12*(Sqrt(CW2) + SW)**2*AllHhhx(638)**2*
     &     (1 - 2*Sqrt(CW2)*SW) + 48*CW2*SW2*AllHhhx(1256)**2

        AllHhhx(1363) =
     &   CBA2*(MA02 - MHp2)**2*AllHhhx(1354) -
     &    (MA02 - Mh02)**2*AllHhhx(1357) +
     &    (MA02 - Mh02)*AllHhhx(1362)

        AllHhhx(1392) =
     &   (CBA2*AllHhhl(2)**2*AllHhhx(15)*AllHhhx(1365))/
     &     (MA02 - Mh02)**2 + CBA*AllHhhl(29)*AllHhhx(1372) +
     &    AllHhhl(35)*AllHhhx(1382) - AllHhhl(25)*AllHhhx(1391)

        AllHhhx(140) =
     &   CBA2*MHp2**2*S2B**4*AllHhhx(123) - 2*AllHhhx(139)

        AllHhhx(1401) =
     &   AllHhhx(1395) - (2*AllHhhx(1400))/(MA02 - MHp2)**2

        AllHhhx(146) =
     &   (-2*SBA*AllHhhx(143))/(MA02 - Mh02) + AllHhhx(145)/Mh02**2

        AllHhhx(1494) =
     &   (-2*(Mh02 - MHp2)*(S2B*AllHhhx(14) - 6*AllHhhx(267))*
     &       AllHhhx(1268))/((4*Mh02 - MHp2)**2*MHp2) +
     &    S2B**2*AllHhhx(1493)

        AllHhhx(1605) =
     &   -24*m12sq**2*(3*S2A + S2B)*SAB*SBA*SBA2 -
     &    4*m12sq*AllHhhx(1585) +
     &    2*(CAB*(CBA*AllHhhx(1588) + 6*AllHhhx(1590)) +
     &       S2B*AllHhhx(1597)) + CBA*AllHhhx(1604)

        AllHhhx(1635) =
     &   MA02*AllHhhx(41) + 3*Mh02*AllHhhx(42) + MHp2*AllHhhx(1634)

        AllHhhx(1639) =
     &   MA02*AllHhhx(52) - Mh02*AllHhhx(1636) - MHp2*AllHhhx(1638)

	dup9 = -(CBA2*SBA*AllHhhx(41)) + CBA*SBA2*AllHhhx(52)

	dup10 = 3*CBA2*SBA*AllHhhx(42) + CBA*SBA2*AllHhhx(1636)

	dup11 = CBA2*SBA*AllHhhx(1634) + CBA*SBA2*AllHhhx(1638)

        AllHhhx(1641) =
     &   CBA2*SBA*AllHhhx(1635) - CBA*SBA2*AllHhhx(1639) +
     &    2*dup9*Re(AllHhhl(1)) -
     &    2*(dup10*Re(AllHhhl(2)) + dup11*Re(AllHhhl(3)))

        AllHhhx(1665) =
     &   AllHhhx(1664)/S2B**4 +
     &    2*(MA02 - MHp2)**2*SBA2*Re(AllHhhl(4)) +
     &    3*CBA2*MHp2**2*Re(AllHhhl(8))

        AllHhhx(1706) =
     &   -((MA02 - Mh02)*(MA02 + Mh02)*
     &       (2*AllHhhx(1684) + AllHhhx(1696) + 4*AllHhhx(1699)))+
     &      2*(S2B**4*AllHhhx(1702) +
     &       (MA02 - Mh02)*(MA02 + Mh02)*AllHhhx(1705))

        AllHhhx(1710) =
     &   Mh02*AllHhhx(1708) -
     &    2*(2*MA02 - Mh02)*(AllHhhx(1705) + S2B**4*AllHhhx(1709))

        AllHhhx(1789) =
     &   8*CAB**2*(CBA2 + 9*CBA2**2)*m12sq*Mh02*S2B -
     &    4*m12sq**2*AllHhhx(1716) - S2B**2*AllHhhx(1719) -
     &    2*CBA*AllHhhx(1746) + S2B**4*AllHhhx(1775) +
     &    2*CBA2**2*AllHhhx(1777) + CBA2*AllHhhx(1788) +
     &    2*(CAB*SBA*AllHhhx(1725) +
     &       (MW2*SW2)/(Alfa*Pi)*AllHhhx(1735)) +
     &    (MW2**2*SW2**2)/(Alfa2*Pi**2)*AllHhhx(1739) -
     &    AllHhhx(1751)*Re(AllHhhl(18)) -
     &    9*AllHhhx(1756)*Re(AllHhhl(19)) -
     &    2*AllHhhx(1759)*Re(AllHhhl(20)) -
     &    AllHhhx(1769)*Re(AllHhhl(21))

        AllHhhx(185) =
     &   (2*AllHhhl(2)*AllHhhx(171))/((MA02 - Mh02)*Mh02) +
     &    AllHhhx(184)/(MA02 - Mh02)**2

        AllHhhx(197) =
     &   CBA2*AllHhhx(193) +
     &    2*(MA02 - Mh02)*(MA02*AllHhhx(194) +
     &       8*CBA2**2*AllHhhx(196))

        AllHhhx(258) =
     &   2*(MA02 - MHp2)*AllHhhx(215) + CBA2*S2B**2*AllHhhx(257)

        AllHhhx(270) =
     &   AllHhhx(262)/(Mh02*(MA02 - MHp2)*(Mh02 - MHp2)) -
     &    (SBA*AllHhhx(265))/S2B**2 -
     &    SBA2*(CBA2 - AllHhhx(269)/Mh02)

        AllHhhx(285) =
     &   (CBA*SBA*AllHhhx(271))/S2B**2 + CBA2*AllHhhx(284)

        AllHhhx(292) =
     &   CBA2*(2*MA02 - MHp2)*MHp2*S2B**8 + 2*S2B**2*AllHhhx(65) +
     &    S2B**4*AllHhhx(291)

        AllHhhx(30) = -4*CBA2**2*(2*Mh02 + MHp2)*S2B**2 +
     &    CBA2*S2B*AllHhhx(21) + 8*CBA*AllHhhx(22) +
     &    (4*AllHhhl(3)*AllHhhx(25))/MHp2 -
     &    (16*AllHhhl(3)**2*AllHhhx(29))/MHp2**2

	AllHhhx(339) = -8*m12sq*SAB*AllHhhx(311) + S2B**2*AllHhhx(338)

        AllHhhx(370) =
     &   CBA2**2*MA02*(MA02 - MHp2)**2*S2B**2*AllHhhx(346) -
     &    CBA2*S2B*AllHhhx(358) - 2*(MA02 - MHp2)*AllHhhx(369)

        AllHhhx(383) =
     &   CBA2*(MA02 - MHp2)*S2B*
     &     (S2B*AllHhhx(377) - MHp2**2*AllHhhx(378)) +
     &    MHp2*(MHp2*AllHhhx(380) + MA02*AllHhhx(382))

        AllHhhx(410) =
     &   -8*AllHhhl(1)**2*(12*CBA*SBA*AllHhhx(9) +
     &       S2B*AllHhhx(385)) + AllHhhx(409)/(4*MA02 - MHp2)

        AllHhhx(46) = (2*AllHhhl(24)*AllHhhx(40))/Mh02**2 +
     &    SBA*AllHhhx(45)

        AllHhhx(493) =
     &   (-4*AllHhhl(3)*(AllHhhx(422) + CBA2*AllHhhx(440)))/
     &     ((Mh02 - MHp2)*MHp2*S2B**2) +
     &    (2*AllHhhx(444) - CBA2*AllHhhx(492))/
     &     ((Mh02 - MHp2)**2*S2B**6)

	AllHhhx(619) = 16*m12sq*SAB*AllHhhx(605) + S2B**2*AllHhhx(618)

        AllHhhx(67) = S2B**4*SBA2*AllHhhx(52)*AllHhhx(57) +
     &    (2*MA02 - MHp2)*AllHhhx(66)

        AllHhhx(688) =
     &   (-2*AllHhhl(31)*AllHhhx(671))/(MHp2*S2B**2) +
     &    (AllHhhl(33)*AllHhhx(687))/(S2B**6*AllHhhx(69))

        AllHhhx(709) =
     &   AllHhhl(35)*AllHhhx(704) -
     &    (CBA2*AllHhhx(708))/(MA02 - Mh02)

	AllHhhx(720) = AllHhhx(711)/MHp2 + AllHhhx(719)/S2B**2

        AllHhhx(746) =
     &   SBA2*(MA02 + Mh02 + AllHhhx(733)/Mh02) -
     &    AllHhhx(736)/((MA02 - MHp2)*(Mh02 - MHp2)**2) -
     &    AllHhhx(745)/MHp2

        AllHhhx(764) =
     &   AllHhhx(752)/(MA02 - Mh02)**2 - AllHhhx(763)/Mh02

        AllHhhx(88) = (2*CBA*S2B**2*SBA*
     &       (2*(4 + SBA2)*AllHhhx(9) + S2B*AllHhhx(80)))/
     &     (Mh02 - MHp2) + (CBA2*AllHhhx(87))/MHp2

	AllHhhx(943) = -8*m12sq*SAB*AllHhhx(906) + S2B**2*AllHhhx(942)

        AllHhhx(1059) =
     &   (8*CBA2*SBA2*AllHhhx(947))/MHp2 -
     &    AllHhhx(1058)/((Mh0 - MHp)*(Mh0 + MHp)*S2B**2)

        AllHhhx(1146) =
     &   4*Mh02*(MA02 - MHp2)*SBA2*(1 + (2*AllHhhl(2))/Mh02)**2 +
     &    AllHhhx(1145)/S2B**4

        AllHhhx(1165) =
     &   -(AllHhhx(1147)/(Mh02*(MA02 - MHp2))) +
     &    CBA2*AllHhhx(1150) + (2*AllHhhx(1158))/(MA02 - Mh02) +
     &    AllHhhx(1164)/S2B**2

        AllHhhx(1243) =
     &   AllHhhx(1218)/(Mh02 - MHp2)**2 +
     &    3*AllHhhl(35)*AllHhhx(1242)

        AllHhhx(141) =
     &   (8*SBA2*AllHhhl(3)**2*AllHhhx(103))/MHp2**2 -
     &    AllHhhx(122) + (4*AllHhhl(3)*AllHhhx(140))/MHp2

	AllHhhx(1606) = CBA*SBA*AllHhhx(1582) - AllHhhx(1605)/S2B**4

        AllHhhx(1640) =
     &   CBA2*SBA*AllHhhx(1635) - CBA*SBA2*AllHhhx(1639) -
     &    2*dup9*Re(AllHhhl(1)) +
     &    2*(dup10*Re(AllHhhl(2)) + dup11*Re(AllHhhl(3)))

        AllHhhx(1642) =
     &   -2*CAB*dup3*SBA + CBA2*SBA2*AllHhhx(1612) +
     &    2*(CBA*AllHhhx(1618) + AllHhhx(1622)) -
     &    (-(AllHhhl(3)*AllHhhx(1641)) -
     &       2*AllHhhx(1629)*Re(AllHhhl(3)))/MHp2

        AllHhhx(198) =
     &   -8*CBA2*AllHhhl(2)**2*AllHhhx(146) -
     &    (4*AllHhhl(2)*AllHhhx(158))/((MA02 - Mh02)**2*Mh02) +
     &    4*AllHhhl(1)*AllHhhx(185) -
     &    (8*AllHhhl(1)**2*AllHhhx(187) - AllHhhx(197))/
     &     (MA02 - Mh02)

        AllHhhx(286) =
     &   AllHhhx(258)/
     &     ((MA02 - Mh02)**2*(MA02 - MHp2)**2*(Mh02 - MHp2)*S2B**4)
     &      + 2*(AllHhhl(2)*AllHhhx(270) + AllHhhl(3)*AllHhhx(285))

	AllHhhx(31) = -24*CBA2*SBA2*AllHhhl(28) + AllHhhx(30)/S2B**2

        AllHhhx(340) =
     &   (4*AllHhhl(3)*AllHhhx(292))/((MA02 - MHp2)**2*S2B**8) -
     &    (CBA2*S2B**4*AllHhhx(298) + 2*AllHhhx(339))/
     &     (MHp2*S2B**6*AllHhhx(48)**2)

        AllHhhx(411) =
     &   -16*((1 + SBA2)*AllHhhl(29)*AllHhhx(342) +
     &       AllHhhl(36)*AllHhhx(345)) +
     &    (4*AllHhhl(3)*AllHhhx(370))/
     &     ((MA02 - MHp2)**2*(4*MA02 - MHp2)*MHp2) -
     &    (8*(AllHhhl(34)*AllHhhx(376) -
     &         2*AllHhhl(30)*AllHhhx(383)))/MHp2**2 +
     &    AllHhhx(410)/(MA02 - MHp2)

        AllHhhx(47) = -((SBA2*AllHhhx(34))/(MA02 - Mh02)**2) +
     &    AllHhhx(46)/S2B**2

        AllHhhx(600) =
     &   (4*AllHhhl(2)*AllHhhx(493))/Mh02 - AllHhhx(599)/S2B**6

        AllHhhx(68) = (S2B**4*AllHhhx(51)*AllHhhx(52)**2)/
     &     (MHp2*AllHhhx(48)**2) + AllHhhx(67)/(MA02 - MHp2)**2

        AllHhhx(689) =
     &   (SBA2*(Mh02*AllHhhl(23)*AllHhhx(644) +
     &         MHp2*AllHhhl(28)*AllHhhx(645)))/(Mh02 - MHp2)**2 +
     &    (2*(3*SBA*AllHhhl(27)*AllHhhx(42) - AllHhhx(656)))/
     &     S2B**2 + AllHhhx(688)/Mh02**2

        AllHhhx(765) =
     &   (AllHhhl(25)*AllHhhx(700))/((MA02 - MHp2)*S2B**4) +
     &    ((CBA*AllHhhl(29)*AllHhhx(52)*AllHhhx(690))/
     &        (MA02 - Mh02) - AllHhhx(709))/S2B**2 +
     &    AllHhhl(32)*AllHhhx(720) -
     &    CBA2*(AllHhhl(31)*AllHhhx(732) -
     &       AllHhhl(24)*AllHhhx(746)) + AllHhhl(30)*AllHhhx(764)

        AllHhhx(89) = -((CBA2*AllHhhl(2)**2*
     &         (Mh02*AllHhhx(74) + SBA2*AllHhhx(78)))/
     &       (Mh02**2*S2B**2*AllHhhx(69))) +
     &    (AllHhhl(3)**2*AllHhhx(88))/S2B**4

        AllHhhx(1515) =
     &   (Mh02 - MHp2)*SBA2**2*AllHhhx(12)**2 -
     &    CBA2*AllHhhx(1235) -
     &    CBA*Mh02*AllHhhx(638)*AllHhhx(1503) + SBA2*AllHhhx(1514)

        AllHhhx(1517) =
     &   -(CBA2*AllHhhx(1235)) + (Mh02 - MHp2)*AllHhhx(1299) +
     &    SBA2*AllHhhx(1514)

        AllHhhx(1552) =
     &   SBA2*AllHhhx(1298) +
     &    5*(Mh02 - 4*MHp2)*(Sqrt(CW2) + SW)**2*AllHhhx(701)**2*
     &     (1 - 2*Sqrt(CW2)*SW)

        AllHhhx(1447) =
     &   CBA2*AllHhhx(15)*AllHhhx(1396) +
     &    2*CBA*(MA02 - Mh02)*AllHhhx(52)*AllHhhx(1446)

        AllHhhx(1516) =
     &   2*dup4*(Mh02 - MHp2)*MHp2**2*SBA2 +
     &    SBA*AllHhhx(701)*AllHhhx(1297) +
     &    Mh02*(Mh02 - 4*MHp2)*AllHhhx(1515)

        AllHhhx(1518) =
     &   2*(Mh02 - MHp2)*SBA2*AllHhhx(1298) +
     &    (Mh02 - 4*MHp2)*AllHhhx(1517)

        AllHhhx(1531) =
     &   -2*(Mh02 - MHp2)*(S2B*AllHhhx(14) - 6*AllHhhx(267))*
     &     AllHhhx(1212) + AllHhhx(1278)

        AllHhhx(1550) =
     &   SBA2*AllHhhx(1298) +
     &    (Mh02 - 4*MHp2)*(Sqrt(CW2) + SW)**2*AllHhhx(701)**2*
     &     (1 - 2*Sqrt(CW2)*SW)

        AllHhhx(1553) =
     &   (Mh02 - 4*MHp2)*AllHhhx(1237) +
     &    2*(Mh02 - MHp2)*AllHhhx(1552)

        AllHhhx(1279) =
     &   -((2*Mh02 - MHp2)*SBA*AllHhhx(701)) + AllHhhx(1212)

        AllHhhx(1448) =
     &   (MA02 - Mh02)**2*SBA*SBA2*AllHhhx(12)*AllHhhx(41) -
     &    (MA02 - MHp2)*AllHhhx(1447)

        AllHhhx(1519) =
     &   -(SBA*AllHhhx(701)*AllHhhx(1297)) + 4*AllHhhx(1516) -
     &    Mh02*AllHhhx(1518)

        AllHhhx(1534) =
     &   -(CBA2*Mh02*(Mh02 - 4*MHp2)**2*(4*Mh02 - MHp2)*
     &       (3*AllHhhx(42)*AllHhhx(701) - AllHhhx(1246))) +
     &    2*(Mh02 - MHp2)*AllHhhx(1285)

        AllHhhx(1535) =
     &   4*(4*Mh02 - MHp2)*MHp2*AllHhhx(1287) -
     &    (Mh02 - MHp2)*AllHhhx(1309) - 2*AllHhhx(1315) +
     &    AllHhhx(1319)

        AllHhhx(1541) =
     &   (4*Mh02 - MHp2)*MHp2*S2B**4*AllHhhx(1258) -
     &    2*(Mh02 - MHp2)*S2B**2*
     &     (S2B*AllHhhx(14) - 6*AllHhhx(267))*AllHhhx(1268)

        AllHhhx(1551) =
     &   CBA2*(Mh02 - 4*MHp2)**2*AllHhhx(1292) -
     &    4*(Mh02 - 4*MHp2)*AllHhhx(1549) -
     &    16*(Mh02 - MHp2)**2*AllHhhx(1550)

        AllHhhx(1554) =
     &   SBA*AllHhhx(701)*AllHhhx(1297) - 4*AllHhhx(1303) +
     &    Mh02*AllHhhx(1553)

        AllHhhx(1559) =
     &   2*(Mh02 - 4*MHp2)*AllHhhx(1500) -
     &    (Mh02 - MHp2)*AllHhhx(1527)

        AllHhhx(1572) =
     &   (4*Mh02 - MHp2)**2*AllHhhx(1305) -
     &    2*(4*Mh02 - MHp2)*MHp2*AllHhhx(1534) +
     &    2*(Mh02 - MHp2)*AllHhhx(1535)

        AllHhhx(1280) =
     &   (4*Mh02 - 3*MHp2)*(Mh02 - MHp2)*SBA2*AllHhhx(1231) -
     &    4*MHp2*AllHhhx(1279)

        AllHhhx(1449) =
     &   S2B**2*AllHhhx(1343) + AllHhhx(1448)/(MA02 - Mh02)**2

        AllHhhx(1520) =
     &   (Mh02 - 4*MHp2)**2*AllHhhx(1510) - Mh02*AllHhhx(1513) -
     &    2*(SBA2*AllHhhx(1289) + (Mh02 - MHp2)*AllHhhx(1519))

        AllHhhx(1536) =
     &   (4*Mh02 - MHp2)*MHp2*AllHhhx(1534) -
     &    (Mh02 - MHp2)*AllHhhx(1535)

        AllHhhx(1545) =
     &   -9*(4*Mh02 - MHp2)**2*MHp2*AllHhhx(42)*AllHhhx(1203) +
     &    4*(Mh02 - MHp2)*(4*Mh02 - MHp2)*
     &     (S2B*AllHhhx(14) - 6*AllHhhx(267))*AllHhhx(1280)

        AllHhhx(1546) =
     &   -27*Mh02**2*(4*Mh02 - MHp2)**2*MHp2*AllHhhx(42)**2 +
     &    Mh02*(4*Mh02 - MHp2)*(S2B*AllHhhx(14) - 6*AllHhhx(267))*
     &     AllHhhx(1280)*(Mh02 + 3/2.D0*MHp2)

        AllHhhx(1555) =
     &   2*SBA2*AllHhhx(1289) - (Mh02 - 4*MHp2)**2*AllHhhx(1291) -
     &    Mh02*AllHhhx(1551) - 2*(Mh02 - MHp2)*AllHhhx(1554)

        AllHhhx(1573) =
     &   4*(Mh02 - MHp2)**2*AllHhhx(1571) - AllHhhx(1572)

        AllHhhx(880) =
     &   6*MA02**4*AllHhhx(313) - MA02**3*MHp2*AllHhhx(879)

        AllHhhx(1275) =
     &   MHp2**2*AllHhhx(1205) + 2*Mh02*MHp2*AllHhhx(1215)

        AllHhhx(1281) =
     &   MHp2*AllHhhx(1278) -
     &    2*(Mh02 - MHp2)*(S2B*AllHhhx(14) - 6*AllHhhx(267))*
     &     AllHhhx(1280)

        AllHhhx(1410) =
     &   (MA02*(MA02 - Mh02)*(MA02 + 2*Mh02)*SBA*AllHhhx(1328))/
     &     (MA02 - MHp2)**2 + 2*Mh02**2*AllHhhx(1365)

        AllHhhx(1432) =
     &   2*AllHhhx(1367) -
     &    AllHhhx(1371)/((MA02 - Mh02)**2*(MA02 - MHp2))

        AllHhhx(1450) =
     &   CAB**2*(2*m12sq - Mh02*S2B)**2 +
     &    CB2**2*AllHhhx(1337)*AllHhhx(1338) -
     &    CAB*(2*m12sq - Mh02*S2B)*AllHhhx(1339) +
     &    Mh02*S2B**2*SBA*AllHhhx(1341) +
     &    (MW2**2*SW2**2)/(Alfa2*Pi**2)*(CBA2*Lambda7**2*SB2**2) +
     &    1/4.D0*AllHhhx(1449)

        AllHhhx(1461) =
     &   (4*Mh02 - MHp2)*AllHhhx(1404) +
     &    CBA2*(MA02 - MHp2)*AllHhhx(1405) -
     &    (MA02 - Mh02)*SBA2*AllHhhx(1460)

        AllHhhx(1464) =
     &   MHp2*AllHhhx(41)**2 + MA02*AllHhhx(1355) -
     &    4*(MA02 - Mh02 + MHp2)*AllHhhx(1380)

        AllHhhx(1472) =
     &   -(CBA2*(MA02 - MHp2)*AllHhhx(1405)) -
     &    (4*Mh02 - MHp2)*AllHhhx(1424) +
     &    (MA02 - Mh02)*SBA2*AllHhhx(1460)

        dup12 = 3*SBA*AllHhhx(12)*AllHhhx(635) -
     &    2*AllHhhx(638)*AllHhhx(701)

        AllHhhx(1477) =
     &   (-3*dup12*MHp2*SBA)/(Mh02 - MHp2) +
     &    (2*MA02*AllHhhx(1430))/(MA02 - Mh02)**2

        AllHhhx(1497) =
     &   -(CBA2*Mh02*(Mh02 - 4*MHp2)**2*
     &       (3*AllHhhx(42)*AllHhhx(701) - AllHhhx(1246))) -
     &    4*(Mh02 - MHp2)*AllHhhx(1496)

        AllHhhx(1529) =
     &   MHp2*AllHhhx(1507) + AllHhhx(1520) -
     &    2*(Mh02 - MHp2)*(AllHhhx(1523) + 2*MHp2*AllHhhx(1526) -
     &       Mh02*AllHhhx(1528)) +
     &    32*CW2*SW2*(Mh02*(Mh02 - 4*MHp2)*(Mh02 - MHp2)**2*
     &       AllHhhx(701)**2)

        AllHhhx(1675) =
     &   (2*MA02 - MHp2)**2*S2B**4 + AllHhhx(1651)**2 +
     &    (12*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA*
     &       (6*m12sq*S2A - S2B*AllHhhx(14))) +
     &    (18*MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     (CBA2*SBA2*AllHhhx(62)) - AllHhhx(1652)*Re(AllHhhl(10))

        AllHhhx(1712) =
     &   AllHhhx(1651)**2 +
     &    (12*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*SBA*
     &       (6*m12sq*S2A - S2B*AllHhhx(14))) +
     &    (3*MW2**2*SW2**2)/(Alfa2*Pi**2)*
     &     ((1 + 6*CBA2*SBA2)*AllHhhx(62)) +
     &    2*AllHhhx(1652)*Re(AllHhhl(20))

        AllHhhx(881) =
     &   -(MA02*MHp2**3*AllHhhx(876)) +
     &    8*(m12sq**2*SAB**2*AllHhhx(875) +
     &       MA02**2*MHp2**2*AllHhhx(877)) +
     &    MHp2**4*AllHhhx(878) + 16*SBA2*AllHhhx(880)

        AllHhhx(1282) =
     &   MHp2*AllHhhx(1274) +
     &    2*((Mh02 - MHp2)**2*SBA2*
     &        (S2B*AllHhhx(14) - 6*AllHhhx(267))**2*AllHhhx(1204)+
     &         (4*Mh02 - MHp2)**2*AllHhhx(1275)) -
     &    2*(4*Mh02 - MHp2)*AllHhhx(1281)

        AllHhhx(1411) =
     &   (-18*Mh02**2*SBA*AllHhhx(42))/
     &     ((MA02 - Mh02)*(Mh02 - MHp2)) +
     &    3*(2*Mh02 + MHp2)*AllHhhx(1335) +
     &    AllHhhx(1410)/(MA02 - Mh02)**2

        AllHhhx(1431) =
     &   (-3*dup12*MHp2**2*SBA)/(Mh02 - MHp2) +
     &    ((MA02**2 + 2*MA02*MHp2)*AllHhhx(1430))/(MA02 - Mh02)**2

        AllHhhx(1433) =
     &   (2*AllHhhx(1368)*AllHhhx(1369))/(Mh02 - MHp2)**2 +
     &    AllHhhx(52)*AllHhhx(1432)

        AllHhhx(1451) =
     &   (-8*(1 - SBA2)*AllHhhx(1393)**2)/
     &     ((4*MA02 - Mh02)*(MA02 - MHp2)) +
     &    2*((CBA2*AllHhhx(52)**2)/
     &        ((MA02 - Mh02)*(4*MA02 - MHp2)) +
     &       (AllHhhx(41)*AllHhhx(1394))/(Mh02 - MHp2)) +
     &    AllHhhx(1395) - (2*AllHhhx(1450))/(MA02 - MHp2)**2

        AllHhhx(1453) =
     &   3*(MA02 + Mh02 + MHp2)*AllHhhx(1363) +
     &    CBA*MA02*(MA02 - MHp2)*(MA02 + 2*MHp2)*AllHhhx(1430)

        AllHhhx(1458) =
     &   Mh02*AllHhhx(1335) +
     &    (Mh02**2*AllHhhx(1365))/(MA02 - Mh02)**2

        AllHhhx(1462) =
     &   AllHhhx(1403) - (MHp2*AllHhhx(1461))/
     &     ((MA02 - Mh02)*(MA02 - MHp2)*(4*Mh02 - MHp2))

        AllHhhx(1470) =
     &   CBA2*(MA02 - MHp2)**2*AllHhhx(1463) -
     &    (MA02 - Mh02)**2*AllHhhx(1464) +
     &    (MA02 - Mh02)*AllHhhx(1469)

        AllHhhx(1473) =
     &   MA02*AllHhhx(1441) -
     &    (2*AllHhhx(41)**2*AllHhhx(1471))/(4*MA02 - Mh02) +
     &    (MHp2*AllHhhx(1472))/
     &     ((MA02 - Mh02)*(Mh02 - 4*MHp2)*(4*Mh02 - MHp2))

        AllHhhx(1476) =
     &   (S2B*AllHhhx(14) - 6*AllHhhx(267))*AllHhhx(1335) -
     &    (4*(S2B*AllHhhx(14) - 6*AllHhhx(267))**2*AllHhhx(1409))/
     &     (4*Mh02 - MHp2)

        AllHhhx(1478) =
     &   AllHhhx(52)*AllHhhx(1432) +
     &    2*((AllHhhx(1368)*AllHhhx(1369))/(Mh02 - MHp2)**2 +
     &       AllHhhx(1477)/(MA02 - MHp2))

        AllHhhx(1479) =
     &   16*MHp2*AllHhhx(1348) + 4*AllHhhx(1403) -
     &    (Mh02*(2*AllHhhx(1407) - AllHhhx(1425)))/
     &     ((MA02 - Mh02)*(MA02 - MHp2)*(4*Mh02 - MHp2))

        AllHhhx(1487) =
     &   (4*(2*MA02 - Mh02)*AllHhhx(1356) -
     &       AllHhhx(1363)/(MA02 - Mh02)**2)/(MA02 - MHp2)**2 +
     &    AllHhhx(41)**2*AllHhhx(1385) + 2*SBA*AllHhhx(1417) -
     &    CBA*AllHhhx(1433)

        AllHhhx(1488) =
     &   (CBA2*SBA*(S2B*AllHhhx(14) - 6*AllHhhx(267))*
     &       AllHhhx(1328))/((MA02 - Mh02)*(MA02 - MHp2)**2) -
     &    8*AllHhhx(1395) + 4*AllHhhx(1401) -
     &    (2*CBA*AllHhhx(1430))/((MA02 - Mh02)**2*(MA02 - MHp2))

        AllHhhx(1502) =
     &   MHp2*AllHhhx(1497) + 2*(Mh02 - MHp2)*AllHhhx(1501) +
     &    16*CW2*SW2*(Mh02*(Mh02 - 4*MHp2)*(Mh02 - MHp2)**2*
     &       AllHhhx(701)**2)

        AllHhhx(1676) =
     &   4*AllHhhx(1662) + 9*AllHhhx(1672) + 2*AllHhhx(1674) +
     &    CBA2*AllHhhx(1675) -
     &    2*SBA2*AllHhhx(1654)*Re(AllHhhl(6)) -
     &    AllHhhx(1659)*Re(AllHhhl(7)) -
     &    AllHhhx(1663)*Re(AllHhhl(9))

        AllHhhx(1713) =
     &   -((Mh02 - 2*MHp2)**2*S2B**4*SBA2) -
     &    4*CAB*(2*m12sq - Mh02*S2B)*AllHhhx(1643) -
     &    CBA2*AllHhhx(1712) +
     &    (4*MW2*SW2)/(Alfa*Pi)*
     &     (CBA*(CB2*Lambda6 - Lambda7*SB2)*AllHhhx(1644)) -
     &    AllHhhx(1698)*Re(AllHhhl(18)) -
     &    9*AllHhhx(1693)*Re(AllHhhl(19)) -
     &    AllHhhx(1704)*Re(AllHhhl(21))

        AllHhhx(1791) =
     &   4*m12sq*AllHhhx(1585) -
     &    2*(CAB*(CBA*AllHhhx(1588) + 6*AllHhhx(1590)) +
     &       S2B*AllHhhx(1597)) - CBA*AllHhhx(1604) +
     &    SBA*AllHhhx(1790)

        AllHhhx(1796) =
     &   -4*CAB**2*(2*m12sq - Mh02*S2B)**2 + 4*AllHhhx(1688) -
     &    SBA2*AllHhhx(1689) - 9*AllHhhx(1693) +
     &    2*(AllHhhx(1713) + AllHhhx(1794)) - S2B**4*AllHhhx(1795)

        AllHhhx(1807) =
     &   (MA02 - MHp2)*(MA02 + MHp2)*
     &    (AllHhhx(1684) + 2*AllHhhx(1699))

        AllHhhx(1817) =
     &   -2*(1 - SBA2)*(AllHhhx(1684) + 2*AllHhhx(1699)) +
     &    2*AllHhhx(1794) - SBA2*AllHhhx(1812)

        AllHhhx(636) =
     &   -8*m12sq*S2B**2*SAB*AllHhhx(59) + S2B**4*AllHhhx(64)

	AllHhhx(882) = 2*CBA*AllHhhx(871) - (MA02 - MHp2)*AllHhhx(881)

        AllHhhx(1187) =
     &   (2*CBA2*Mh02*(Mh02 - MHp2))/(MA02 - Mh02) +
     &    AllHhhx(724)/(MA02 - Mh02)**2 -
     &    SBA2*(MA02 + Mh02 + AllHhhx(733)/Mh02) +
     &    AllHhhx(745)/MHp2 +
     &    (AllHhhx(736)/(Mh02 - MHp2)**2 + AllHhhx(1186))/
     &     (MA02 - MHp2)

        AllHhhx(1283) =
     &   2*Mh02**2*S2B**2*AllHhhx(1272) + Mh02*S2B**4*AllHhhx(1282)

        AllHhhx(1414) =
     &   (S2B*AllHhhx(14) - 6*AllHhhx(267))*AllHhhx(1411) +
     &    (2*AllHhhx(1413))/(MA02 - Mh02) -
     &    (8*(S2B*AllHhhx(14) - 6*AllHhhx(267))**2*AllHhhx(1409)*
     &       (3*Mh02 + 3/2.D0*MHp2))/(4*Mh02 - MHp2)

        AllHhhx(1434) =
     &   (-4*AllHhhx(1431))/(MA02 - MHp2) -
     &    3*(2*MA02 + MHp2)*AllHhhx(1433) +
     &    (16*AllHhhx(52)*AllHhhx(1427)*(MA02 + 3/2.D0*MHp2))/
     &     (4*MA02 - MHp2)

        AllHhhx(1452) =
     &   MHp2**2*AllHhhx(1349) +
     &    (AllHhhx(41)**2*AllHhhx(1419)*(MA02 + 1/2.D0*Mh02))/
     &     ((4*MA02 - Mh02)*(MA02 - MHp2)*(Mh02 - MHp2)) +
     &    1/2.D0*((Mh02 + 2*MHp2)*AllHhhx(1403))/(Mh02 - 4*MHp2) -
     &    1/2.D0*(MA02**2*AllHhhx(1451))

        AllHhhx(1454) =
     &   (-3*CBA*dup12*(MA02 - MHp2)*MHp2**2*SBA)/(Mh02 - MHp2) +
     &    AllHhhx(1453)/(MA02 - Mh02)**2

        AllHhhx(1459) =
     &   -((Mh02**2*AllHhhx(1456))/(MA02 - Mh02)) +
     &    Mh02*AllHhhx(1457) +
     &    (S2B*AllHhhx(14) - 6*AllHhhx(267))*AllHhhx(1458)

        AllHhhx(1474) =
     &   MA02*AllHhhx(1376) + (4*AllHhhx(1377))/(Mh02 - MHp2) -
     &    (SBA2*AllHhhx(12)*AllHhhx(1379))/(MA02 - Mh02) +
     &    (Mh02 - 2*MHp2)*AllHhhx(1381) +
     &    AllHhhx(41)**2*AllHhhx(1385) -
     &    (4*AllHhhx(1462))/(Mh02 - 4*MHp2) +
     &    (4*(2*MA02 - Mh02)*AllHhhx(1380) -
     &       2*(AllHhhx(1374)/(Mh02 - 4*MHp2) +
     &          AllHhhx(1470)/(MA02 - Mh02)**2))/(MA02 - MHp2)**2+
     &      2*(SBA*AllHhhx(1390) + AllHhhx(1473)/(MA02 - MHp2))

        AllHhhx(1489) =
     &   (4*dup8*AllHhhx(52))/(4*MA02 - MHp2) +
     &    2*(MHp2*AllHhhx(1444) + AllHhhx(1486)/(MA02 - MHp2) +
     &       AllHhhx(1487)) + MA02*AllHhhx(1488)

        AllHhhx(1677) =
     &   AllHhhx(1676)/S2B**4 +
     &    2*(MA02 - MHp2)**2*SBA2*(1 - Re(AllHhhl(4))) +
     &    3*CBA2*MHp2**2*(1 - Re(AllHhhl(8)))

        AllHhhx(1714) =
     &   -4*CAB**2*(2*m12sq - Mh02*S2B)**2 + 4*AllHhhx(1688) -
     &    SBA2*AllHhhx(1689) - 9*AllHhhx(1693) -
     &    S2B**4*AllHhhx(1711) + 2*(AllHhhx(1684) + AllHhhx(1713))

        AllHhhx(1797) =
     &   -4*CBA2**2*S2B**4*AllHhhx(1701) +
     &    2*CBA*SBA*AllHhhx(1791) + CBA2*AllHhhx(1796)

        AllHhhx(1805) =
     &   2*CBA*SBA*AllHhhx(1791) + CBA2*AllHhhx(1803) +
     &    4*AllHhhx(1804)

        AllHhhx(1816) =
     &   -2*(1 - SBA2)*AllHhhx(1807) +
     &    (MA02 - MHp2)*(MA02 + MHp2)*
     &     (2*AllHhhx(1794) + 4*AllHhhx(1809)) - SBA2*AllHhhx(1815)

        AllHhhx(1818) =
     &   2*(2*MA02 - MHp2)*
     &     (2*AllHhhx(1809) - SBA2*AllHhhx(1814)) +
     &    MHp2*AllHhhx(1817)

        AllHhhx(633) =
     &   CBA2*(2*MA02 - MHp2)*MHp2*S2B**8 + 2*S2B**2*AllHhhx(65) -
     &    S2B**4*AllHhhx(632)

        AllHhhx(637) =
     &   CBA2*(2*MA02 - MHp2)*MHp2*S2B**8 +
     &    3*S2B**4*SBA2*AllHhhx(52)*AllHhhx(635) + 2*AllHhhx(636)

	AllHhhx(883) = -8*m12sq*SAB*AllHhhx(836) + S2B**2*AllHhhx(882)

        AllHhhx(1188) =
     &   MA02*AllHhhx(270) + AllHhhx(700)/((MA02 - MHp2)*S2B**4) +
     &    AllHhhx(711)/MHp2 + AllHhhx(1185)/S2B**2 -
     &    CBA2*AllHhhx(1187)

        AllHhhx(1322) =
     &   CBA2*(Mh02 - 4*MHp2)**2*AllHhhx(1283) +
     &    MHp2*S2B**4*AllHhhx(1321)

        AllHhhx(1455) =
     &   3*(Mh02 + 2*MHp2)*AllHhhx(1382) - CBA2*AllHhhx(1414) -
     &    3*(2*MA02 + Mh02)*AllHhhx(1418) +
     &    (4*AllHhhx(1426))/(MA02 - MHp2) - CBA*AllHhhx(1434) -
     &    4*MA02*AllHhhx(1445) +
     &    8*(AllHhhx(1408)/(Mh02 - 4*MHp2) + AllHhhx(1452)) +
     &    (2*AllHhhx(1454))/(MA02 - MHp2)**2

        AllHhhx(1475) =
     &   2*CBA2*AllHhhx(1459) +
     &    Mh02*((-4*AllHhhl(3)*AllHhhx(1425))/
     &        ((MA02 - Mh02)*(Mh02 - 4*MHp2)*(MA02 - MHp2)*
     &          (4*Mh02 - MHp2)) + AllHhhx(1474))

        AllHhhx(1490) =
     &   (-4*CBA*AllHhhl(34)*AllHhhx(1430))/
     &     ((MA02 - Mh02)**2*(MA02 - MHp2)) +
     &    (AllHhhl(1)*(4*AllHhhx(1484) + AllHhhx(1489)))/MA02

        AllHhhx(1678) =
     &   2*(2 - SBA2)*(AllHhhx(1650) - AllHhhx(1671)) +
     &    SBA2*AllHhhx(1677)

        AllHhhx(1715) =
     &   4*(MA02 - 2*Mh02)*AllHhhx(1699) - MA02*AllHhhx(1714)

        AllHhhx(1800) =
     &   MHp2*AllHhhx(1797) + 4*(2*Mh02 - MHp2)*AllHhhx(1799)

        AllHhhx(1806) =
     &   2*((Mh02 - 2*MHp2)*S2B**4*AllHhhx(1801) +
     &       CBA2*AllHhhx(1802)) + Mh02*AllHhhx(1805)

        AllHhhx(1822) =
     &   AllHhhx(1816) - 2*AllHhhl(3)*AllHhhx(1818) +
     &    2*AllHhhl(1)*(4*AllHhhx(1819) + MA02*AllHhhx(1821))

        AllHhhx(634) =
     &   AllHhhl(3)**2*AllHhhx(630) + AllHhhl(1)**2*AllHhhx(633)

	AllHhhx(642) = 3*MHp2*AllHhhx(637) - S2B**4*AllHhhx(641)

        AllHhhx(884) =
     &   CBA2*(MA02 - MHp2)*S2B**4*AllHhhx(809) + 2*AllHhhx(883)

        AllHhhx(1189) =
     &   CBA2*AllHhhx(1179) +
     &    Mh02*(MHp2*AllHhhx(1181) - AllHhhx(1188))

        AllHhhx(1323) =
     &   (4*Mh02*AllHhhl(3)*AllHhhx(1255))/S2B**4 -
     &    AllHhhx(1322)/
     &     ((Mh02 - 4*MHp2)**2*(Mh02 - MHp2)**2*(4*Mh02 - MHp2)**2*
     &       MHp2*S2B**8)

        AllHhhx(1679) =
     &   -2*(2 - SBA2)*AllHhhx(1650) + SBA2*AllHhhx(1665) +
     &    (AllHhhl(3)*AllHhhx(1678))/MHp2

        AllHhhx(1823) =
     &   -((CBA2*(AllHhhx(1706) +
     &           2*(AllHhhl(2)*AllHhhx(1710) +
     &              AllHhhl(1)*AllHhhx(1715))))/(MA02 - Mh02)**2)+
     &      (2*(Mh02 + MHp2)*AllHhhx(1789))/(Mh02 - MHp2) -
     &    (2*(AllHhhl(3)*AllHhhx(1800) -
     &         AllHhhl(2)*AllHhhx(1806)))/(Mh02 - MHp2)**2 +
     &    AllHhhx(1822)/(MA02 - MHp2)**2

        AllHhhx(643) =
     &   (-3*AllHhhl(34)*(CBA2*S2B**4*AllHhhx(603) +
     &         2*AllHhhx(619)))/S2B**6 -
     &    (3*AllHhhx(634) - AllHhhl(36)*AllHhhx(642))/S2B**8

        AllHhhx(944) =
     &   (AllHhhx(884) - 4*AllHhhl(3)*
     &       (CBA2*S2B**4*AllHhhx(891) + 2*AllHhhx(943)))/MHp2

        AllHhhx(1824) =
     &   2*(CBA*SBA*AllHhhx(1581)*AllHhhx(1606) +
     &       (AllHhhx(1630) + AllHhhx(1631)*AllHhhx(1640) -
     &          2*AllHhhx(1642))/S2B**2) + 4*AllHhhx(1679) -
     &    AllHhhx(1823)/S2B**4

        AllHhhx(1530) =
     &   4*CBA*dup7*(4*Mh02 - MHp2)**2*MHp2 +
     &    CBA2*(4*Mh02 - MHp2)*(S2B*AllHhhx(14) - 6*AllHhhx(267))*
     &     AllHhhx(1280)

        AllHhhx(1532) =
     &   (Mh02 - MHp2)**2*SBA2*
     &     (S2B*AllHhhx(14) - 6*AllHhhx(267))**2*AllHhhx(1204) +
     &    (4*Mh02 - MHp2)**2*AllHhhx(1275) -
     &    (4*Mh02 - MHp2)*MHp2*AllHhhx(1531)

        AllHhhx(1565) =
     &   9*Mh02*(4*Mh02 - MHp2)**2*MHp2*AllHhhx(42)*
     &     AllHhhx(1203) - 2*AllHhhx(1532)*(Mh02 + 1/2.D0*MHp2)

        AllHhhx(1533) =
     &   CBA*(4*Mh02 - MHp2)**2*MHp2*AllHhhx(1198) +
     &    2*(Mh02 - MHp2)*AllHhhx(1530) + CBA2*AllHhhx(1532)

        AllHhhx(1542) =
     &   2*Mh02**2*S2B**4*AllHhhx(1532) + Mh02**3*AllHhhx(1541)

        AllHhhx(1547) =
     &   4*(Mh02 - MHp2)**2*(2*Mh02 - MHp2)**2*(2*Mh02 + MHp2)*
     &     SBA2*AllHhhx(1231)**2 + MHp2*AllHhhx(1532)

        AllHhhx(1560) =
     &   MHp2*AllHhhx(1497) +
     &    2*(AllHhhx(1305) +
     &       (Mh02 - MHp2)*
     &        (AllHhhx(1501) - 2*MHp2*AllHhhx(1558) -
     &          Mh02*AllHhhx(1559)))

        AllHhhx(1566) =
     &   CBA*(4*Mh02 - MHp2)**2*MHp2**2*AllHhhx(1198) -
     &    CBA2*AllHhhx(1565)

        AllHhhx(1480) =
     &   (2*AllHhhx(1374))/(Mh02 - 4*MHp2) +
     &    AllHhhx(1470)/(MA02 - Mh02)**2

        AllHhhx(1537) =
     &   (Mh02 - 4*MHp2)**2*AllHhhx(1533) - 2*AllHhhx(1536)

        AllHhhx(1543) =
     &   -4*Mh02**3*(Mh02 - MHp2)*S2B**2*
     &     (S2B*AllHhhx(14) - 6*AllHhhx(267))*AllHhhx(1540) +
     &    AllHhhx(1542)

        AllHhhx(1548) =
     &   Mh02**2*AllHhhx(1545) + 2*(Mh02 - MHp2)*AllHhhx(1546) +
     &    Mh02*AllHhhx(1547)

        AllHhhx(1561) =
     &   MHp2*(Mh02 + 2*MHp2)*AllHhhx(1555) +
     &    2*MHp2**2*AllHhhx(1560)

        AllHhhx(1574) =
     &   8*(4*Mh02 - MHp2)**2*MHp2**2*AllHhhx(1564) +
     &    (Mh02 - 4*MHp2)**2*AllHhhx(1566) - 2*MHp2*AllHhhx(1573)

        AllHhhx(1481) =
     &   MA02*AllHhhx(1376) + (4*AllHhhx(1377))/(Mh02 - MHp2) -
     &    (SBA2*AllHhhx(12)*AllHhhx(1379))/(MA02 - Mh02) +
     &    (Mh02 - 2*MHp2)*AllHhhx(1381) -
     &    AllHhhx(1480)/(MA02 - MHp2)**2

        AllHhhx(1538) =
     &   -2*(4*Mh02 - MHp2)**2*MHp2*AllHhhx(1529) +
     &    Mh02*AllHhhx(1537) +
     &    32*CW2*SW2*(Mh02**2*(Mh02 - 4*MHp2)*AllHhhx(69)**2*
     &       AllHhhx(701)**2)

        AllHhhx(1544) =
     &   CBA*Mh02*(4*Mh02 - MHp2)**2*MHp2**2*S2B**4*
     &     AllHhhx(1198) + CBA2*AllHhhx(1543)

        AllHhhx(1575) =
     &   2*CBA2*(Mh02 - 4*MHp2)**2*AllHhhx(1548) +
     &    (4*Mh02 - MHp2)**2*AllHhhx(1561) + Mh02*AllHhhx(1574)

        AllHhhx(1482) =
     &   -8*MHp2*AllHhhx(1349) +
     &    (4*AllHhhx(52)*(2*CBA*AllHhhx(1427) -
     &         MA02*AllHhhx(1435)))/(4*MA02 - MHp2) +
     &    CBA2*AllHhhx(1476) - CBA*AllHhhx(1478) -
     &    (2*AllHhhx(1479))/(Mh02 - 4*MHp2) +
     &    2*(MA02*AllHhhx(1444) + AllHhhx(1481))

        AllHhhx(1539) =
     &   (2*AllHhhl(3)**2*AllHhhx(1502) +
     &      (AllHhhl(3)*AllHhhx(1538))/(4*Mh02 - MHp2)**2)/MHp2

        AllHhhx(1576) =
     &   2*(Mh02 - 4*MHp2)**2*AllHhhx(1544) + S2B**4*AllHhhx(1575)

        AllHhhx(1483) =
     &   (4*CBA*dup12*SBA*AllHhhl(36))/
     &     ((MA02 - MHp2)*(Mh02 - MHp2)) +
     &    (AllHhhl(3)*AllHhhx(1482))/MHp2

        AllHhhx(1577) =
     &   (4*AllHhhx(1539))/S2B**4 +
     &    AllHhhx(1576)/((4*Mh02 - MHp2)**2*MHp2*S2B**8)

        AllHhhx(1491) =
     &   -72*CBA2*(S2B*AllHhhx(14) - 6*AllHhhx(267))*
     &     AllHhhx(1336) + 288*AllHhhl(3)**2*AllHhhx(1349) +
     &    (8*AllHhhl(36)*AllHhhx(1353))/MHp2 +
     &    (144*AllHhhl(32)*AllHhhx(1363))/
     &     ((MA02 - Mh02)**2*(MA02 - MHp2)**2) +
     &    72*AllHhhx(1392) - 144*AllHhhl(1)**2*AllHhhx(1401) +
     &    9*AllHhhx(1455) -
     &    36*((AllHhhl(2)*AllHhhx(1475))/Mh02 +
     &       MHp2*AllHhhx(1483) + MA02*AllHhhx(1490))

        AllHhhx(1578) =
     &   (8*CBA2*AllHhhl(2)**2*AllHhhx(1494))/S2B**6 +
     &    AllHhhx(1577)/(Mh02*(Mh02 - 4*MHp2)**2)

        AllHhhx(1579) =
     &   -72*AllHhhx(89) +
     &    CBA2*((-72*SBA2*AllHhhx(3))/(MA02 - MHp2)**2 +
     &       144*(AllHhhl(3)**2*AllHhhx(13) - SBA2*AllHhhx(18)) +
     &       72*AllHhhx(689)) - 144*AllHhhx(765) +
     &    AllHhhl(1)*(-72*AllHhhx(286) +
     &       36*(AllHhhx(340) + AllHhhx(802))) +
     &    (3*AllHhhx(141) + (9*AllHhhx(944))/AllHhhx(48)**2)/
     &     S2B**6 + (18*AllHhhx(1059))/(MA02 - Mh02) -
     &    9*(MHp2*AllHhhx(31) + AllHhhx(411)/S2B**2 +
     &       CBA2*AllHhhx(1146)) +
     &    72*(AllHhhl(3)**2*AllHhhx(11) + CBA2*AllHhhx(47) +
     &       (AllHhhl(29)*AllHhhx(68))/S2B**8 +
     &       (AllHhhl(2)*(2*Mh02*AllHhhl(3)*AllHhhx(1165) +
     &            AllHhhx(1189)))/Mh02) +
     &    24*(AllHhhx(643)/(MA02 - MHp2)**2 +
     &       AllHhhx(1243)/S2B**4) -
     &    (36*AllHhhl(2)*AllHhhx(1323))/Mh02 -
     &    AllHhhx(1491)/S2B**4 +
     &    9*(AllHhhx(198)/S2B**2 +
     &       AllHhhx(600)/((Mh02 - 4*MHp2)*(4*Mh02 - MHp2)) +
     &       AllHhhx(1578)/(Mh02 - MHp2)**2)

        AllHHHeqHp = -(Alfa2/(MW2**2*Pi**2*SW2**2)*
     &      (1/36864.D0*AllHhhx(1579) + 1/2048.D0*AllHhhx(1824)))

	end
