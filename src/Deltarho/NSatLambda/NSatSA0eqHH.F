
c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich


* NSatSA0eqHH.F
* generated 22-Dec-2016 15:32

C> @brief This subroutine calculates \f$\delta\rho^{\alpha_t \lambda_i}_\mathrm{NS}\f$ with \f$m_{A^0}=m_{H^0}\f$.
C>
C> @ingroup group_rho
	subroutine calcNSatSA0eqHH
	implicit none


#include "NSatSA0eqHH.h"


	RealType T134sub

	external T134sub

	ComplexType A0FH, B0FH

	external A0FH, B0FH

#include "paraTHDM.h"
#include "paraSM.h"
	NSatSahl(1) = A0FH(MT2)

	NSatSahl(2) = B0FH(MHH2,MT2,MT2)

	NSatSahl(3) = B0FH(MHp2,0.D0,MT2)

	NSatSahl(4) = A0FH(MHp2)

	NSatSahl(5) = A0FH(MHH2)

	NSatSahl(6) = T134sub(MT2,MHp2,0.D0)

	NSatSahl(7) = T134sub(MT2,MT2,MHH2)

	NSatSahl(8) = T134sub(MT2,MHH2,0.D0)

	NSatSahl(9) = T134sub(MT2,MT2,MHp2)

	NSatSahx(20) = MHp2**2 + 36*MT2**2 - 14*MT2*MHp2

        NSatSahx(58) =
     &   19*MHp2**3 - 96*MT2**3 + 256*MT2**2*MHp2 - 136*MT2*MHp2**2

        NSatSahx(59) =
     &   3*MHp2**3 - 20*MT2**3 + 44*MT2**2*MHp2 - 22*MT2*MHp2**2

        NSatSahx(60) =
     &   2*MHp2**3 + 72*MT2**3 - 24*MT2**2*MHp2 - 7*MT2*MHp2**2

	NSatSahx(62) = 8*MHH2**2 - 18*MHH2*MHp2 + 5*MHp2**2

	NSatSahx(63) = 3*MHH2**2 + 8*MHH2*MHp2 - MHp2**2

	NSatSahx(64) = 5*MHH2**2 + 6*MHH2*MHp2 - MHp2**2

	NSatSahx(18) = MHp2**2 + 4*MT2**2 - 6*MT2*MHp2

        NSatSahx(19) =
     &   MHp2**3 + 16*MT2**3 + 12*MT2**2*MHp2 - 10*MT2*MHp2**2

        NSatSahx(21) =
     &   MHp2**2*(MHp2 - 4*MT2)**2 + MHH2*MHp2*NSatSahx(20)

        NSatSahx(23) =
     &   MHp2**3 - 72*MT2**3 + 56*MT2**2*MHp2 - 14*MT2*MHp2**2

        NSatSahx(24) =
     &   3*MHp2**3 + 16*MT2**3 + 52*MT2**2*MHp2 - 24*MT2*MHp2**2

	NSatSahx(25) = MHp2**2 - 4*(MT2**2 + MT2*MHp2)

        NSatSahx(3) = MHp2**4 + 160*MT2**4 - 16*MT2**3*MHp2 -
     &    56*MT2**2*MHp2**2 + 7*MT2*MHp2**3

	NSatSahx(5) = MT2**4*MHp2 + MT2**3*MHp2**2

	NSatSahx(51) = 10*MHH2**2 - MHp2**2

	NSatSahx(52) = MHp2**2 + 10*MT2**2 - 4*MT2*MHp2

	NSatSahx(54) = MHH2*(MHp2 - 6*MT2) + 5*MT2**2

        NSatSahx(61) =
     &   MHH2*MHp2*NSatSahx(58) - 4*MHH2**2*NSatSahx(59) -
     &    2*MHp2**2*NSatSahx(60)

        NSatSahx(65) =
     &   (MHH2 - MHp2)*(2*MHH2 - MHp2)*MHp2**3 -
     &    MT2*(MHp2**2*NSatSahx(62)) -
     &    4*MT2**2*(MHp2*NSatSahx(63)) + 8*MT2**3*NSatSahx(64)

	NSatSahx(67) = 3*MHH2**2 + 10*MHH2*MHp2 + 2*MHp2**2

	NSatSahx(68) = MHH2**2 + 10*MHH2*MHp2 + 5*MHp2**2

	NSatSahx(69) = MHH2**2 - 10*MHH2*MHp2 - 15*MHp2**2

        NSatSahx(10) =
     &   7*MHp2**3 + 16*MT2**3 + 24*MT2**2*MHp2 - 40*MT2*MHp2**2

	NSatSahx(13) = MHH2**4*MHp2 + 16*MT2**3*MHp2**2

        NSatSahx(14) =
     &   5*MHp2**4 - 288*MT2**4 + 136*MT2**2*MHp2**2 -
     &    48*MT2*MHp2**3

        NSatSahx(15) =
     &   11*MHp2**4 + 32*MT2**4 - 256*MT2**3*MHp2 +
     &    248*MT2**2*MHp2**2 - 92*MT2*MHp2**3

        NSatSahx(16) =
     &   5*MHp2**5 - 128*MT2**5 + 224*MT2**4*MHp2 -
     &    672*MT2**3*MHp2**2 + 440*MT2**2*MHp2**3 - 88*MT2*MHp2**4

        NSatSahx(2) = 15*MHp2**4 + 64*MT2**4 -
     &    48*MT2**3*MHp2 + 104*MT2**2*MHp2**2 - 87*MT2*MHp2**3

        NSatSahx(22) =
     &   4*MT2*NSatSahx(21) + MHH2**3*NSatSahx(18) -
     &    MHH2**2*NSatSahx(19)

        NSatSahx(26) =
     &   -(4*MT2*(MHp2**2*(MHp2 - 4*MT2)**2)) +
     &    2*MHH2*MHp2*NSatSahx(23) + MHH2**2*NSatSahx(24) +
     &    MHH2**3*NSatSahx(25)

	NSatSahx(37) = MHp2**2 - 4*MT2**2 + 11*MT2*MHp2

	NSatSahx(4) = 5*MHH2**3*(MHp2 - 4*MT2)**2 + 2*MT2*NSatSahx(3)

	NSatSahx(42) = MHH2*(MHp2 - MT2) - 2*MT2*(MHp2 - 2*MT2)

	NSatSahx(46) = MHp2**2 - 6*MT2**2 - 3*MT2*MHp2

        NSatSahx(47) =
     &   MHp2**4 + 4*MT2**4 - 14*MT2**3*MHp2 + 19*MT2**2*MHp2**2 -
     &    8*MT2*MHp2**3

        NSatSahx(48) =
     &   3*MHp2**4 + 48*MT2**4 - 80*MT2**3*MHp2 +
     &    68*MT2**2*MHp2**2 - 25*MT2*MHp2**3

        NSatSahx(53) =
     &   2*MHH2**3 - MT2*NSatSahx(51) + MHH2*NSatSahx(52)

	NSatSahx(55) = MHH2**2*(MHp2 - MT2) - MHp2*NSatSahx(54)

        NSatSahx(6) = 2*MHp2**5 + 64*MT2**5 -
     &    266*MT2**2*MHp2**3 + 37*MT2*MHp2**4 + 224*NSatSahx(5)

	NSatSahx(66) = 2/MT2*(NSatSahl(1)*NSatSahx(65)) - NSatSahx(61)

        NSatSahx(70) =
     &   MHH2*MHp2**3*(MHH2 + 2*MHp2) -
     &    2*MT2*(MHp2**2*NSatSahx(67)) +
     &    6*MT2**2*(MHp2*NSatSahx(68)) + 4*MT2**3*NSatSahx(69)

	NSatSahx(8) = MHp2**2 + 2*MT2**2 - 5*MT2*MHp2

        NSatSahx(9) = MHp2**3 - 80*MT2**3 + 72*MT2**2*MHp2 -
     &    16*MT2*MHp2**2

        NSatSahx(11) =
     &   8*MHH2**2*NSatSahx(8) - MHp2*NSatSahx(9) -
     &    MHH2*NSatSahx(10)

        NSatSahx(17) =
     &   -(4*MT2*(MHH2*MHp2*NSatSahx(14))) +
     &    2*(MHp2 - 4*MT2)**2*NSatSahx(13) +
     &    MHH2**3*NSatSahx(15) + MHH2**2*NSatSahx(16)

        NSatSahx(27) =
     &   -(2/MT2*(NSatSahl(1)*NSatSahx(22))) +
     &    1/MT2**2*(NSatSahl(1)**2*NSatSahx(26))

        NSatSahx(30) =
     &   MHp2**3 - 124*MT2**3 + 80*MT2**2*MHp2 - 16*MT2*MHp2**2

        NSatSahx(31) =
     &   3*MHp2**3 - 8*MT2**3 + 32*MT2**2*MHp2 - 20*MT2*MHp2**2

        NSatSahx(32) =
     &   MHp2**3 + 12*MT2**3 + 4*MT2**2*MHp2 - 6*MT2*MHp2**2

	NSatSahx(34) = 5*MHH2**2 + 18*MHH2*MHp2 + MHp2**2

	NSatSahx(35) = MHH2**2 + 22*MHH2*MHp2 + MHp2**2

        NSatSahx(38) =
     &   MHH2**3*(7*MHp2 - 4*MT2) - 40*MT2**2*MHp2**2 +
     &    24*MT2*(MHH2*MHp2*(MHp2 + 3*MT2)) -
     &    4*MHH2**2*NSatSahx(37)

	NSatSahx(39) = MHH2*(MHH2 + 2*MHp2) - 6*MT2*(MHH2 + MHp2)

	NSatSahx(41) = MHH2*MHp2 - MHp2**2 - MT2*(2*MHH2 - MHp2)

	NSatSahx(43) = MHH2**2*(MHp2 - MT2) - MHp2*NSatSahx(42)

        NSatSahx(45) =
     &   MHH2**3 - MHH2**2*(MHp2 + 4*MT2) - MT2**2*MHp2 +
     &    MT2*(MHH2*(3*MHp2 + 2*MT2))

        NSatSahx(49) =
     &   MHp2**2*(MHp2 - 2*MT2)**2*NSatSahx(46) +
     &    2*MHH2**2*NSatSahx(47) - MHH2*MHp2*NSatSahx(48)

        NSatSahx(56) =
     &   2/MT2*(NSatSahl(1)*NSatSahx(55)) -
     &    ((MHp2 - 4*MT2)**2*NSatSahx(53))/
     &     ((MHp - 2*MT)**2*(MHp + 2*MT)**2)

        NSatSahx(57) =
     &   MHH2**2*(MHH2 + 2*MHp2) + 4*MT2**2*(5*MHH2 + MHp2) -
     &    2*MT2*(MHH2*(5*MHH2 + 4*MHp2))

        NSatSahx(7) = MHH2**2*NSatSahx(2) + MHp2*NSatSahx(4) -
     &    MHH2*NSatSahx(6)

        NSatSahx(71) =
     &   (NSatSahl(4)*NSatSahx(66) + 2*NSatSahl(9)*NSatSahx(70))/
     &    MHp2

        NSatSahx(78) =
     &   2*MHH2**2 - 4*MHH2*MHp2 + 3*MHp2**2 -
     &    1/MT2*((MHH2**2 - 2*MHp2**2)*Re(NSatSahl(1)))

	NSatSahx(79) = MHH2**2 + MHp2*(MHH2 - 2*MT2)*Re(NSatSahl(2))

        NSatSahx(81) =
     &   3*MHH2**2 - 8*MHH2*MHp2 + 4*MHp2**2 +
     &    1/MT2*((MHH2**2 - 2*MHp2**2)*Re(NSatSahl(1)))

	NSatSahx(82) = MHH2 - (MHH2 - 2*MT2)*Re(NSatSahl(2))

	NSatSahx(83) = 3*MHp2 - 2*(MHp2 - MT2)*Re(NSatSahl(3))

        NSatSahx(1) = (MHH2 - MHp2)*MHp2**3 -
     &    4*MT2**3*(MHH2 - 5*MHp2) +
     &    4*MT2**2*((2*MHH2 - 5*MHp2)*MHp2) -
     &    2*MT2*((3*MHH2 - 4*MHp2)*MHp2**2)

        NSatSahx(12) =
     &   MT2*NSatSahx(7) + 4*MT2**2*(NSatSahl(1)*NSatSahx(11))

	NSatSahx(28) = -(MT2*NSatSahx(17)) + 4*MT2**3*NSatSahx(27)

        NSatSahx(33) =
     &   -2*MHp2**2*NSatSahx(30) + MHH2**2*NSatSahx(31) -
     &    4*MHH2*MHp2*NSatSahx(32)

        NSatSahx(36) =
     &   MHH2*MHp2**2*(MHH2 + 2*MHp2) - MT2*(MHp2*NSatSahx(34)) +
     &    2*MT2**2*NSatSahx(35)

        NSatSahx(40) =
     &   -(MT2*(((MHp2 - 4*MT2)**2*NSatSahx(38))/
     &         ((MHp - 2*MT)**2*(MHp + 2*MT)**2) -
     &        4*MHp2*NSatSahl(1)*NSatSahx(39)))

        NSatSahx(44) =
     &   (2*MHH2 - MHp2 - MT2)*NSatSahl(6)*NSatSahx(41) +
     &    NSatSahl(7)*NSatSahx(43)

        NSatSahx(50) =
     &   ((MHp2 - MT2)*NSatSahl(8)*NSatSahx(45))/(MHH2 - MT2) +
     &    (NSatSahl(9)*NSatSahx(49))/
     &     (MHp2*(MHp - 2*MT)**2*(MHp + 2*MT)**2)

        NSatSahx(72) =
     &   (NSatSahl(5)*NSatSahx(56))/(MHH2 - MT2) +
     &    (2*MHp2*NSatSahl(7)*NSatSahx(57))/
     &     (MHH2*(MHH2 - 4*MT2)) -
     &    NSatSahx(71)/((MHp - 2*MT)**2*(MHp + 2*MT)**2)

        NSatSahx(75) =
     &   -MHH2 - MHp2 + 1/MT2*((5*MHH2 - 7*MHp2)*Re(NSatSahl(1)))

        NSatSahx(76) =
     &   (MHH2 - 3*MHp2)*(MHH2 - 2*MT2)*Re(NSatSahl(2)) +
     &    (3*MHH2 - MHp2)*(MHp2 - MT2)*Re(NSatSahl(3))

        NSatSahx(80) =
     &   3*MHH2*MHp2**2 - MHp2**3 - 2*MHp2*NSatSahx(79) +
     &    2*(MT2*NSatSahx(78) +
     &       MHH2**2*(MHp2 - MT2)*Re(NSatSahl(3)))

        NSatSahx(84) =
     &   MHH2**3 - 2*MT2*NSatSahx(81) + 2*MHp2**2*NSatSahx(82) -
     &    MHH2**2*NSatSahx(83)

        NSatSahx(29) =
     &   -8*NSatSahl(1)**2*NSatSahx(1) +
     &    NSatSahx(12)/(MHH2 - MT2) +
     &    NSatSahx(28)/(MHH2*(MHH2 - 4*MT2))

        NSatSahx(73) =
     &   -(MT2*(NSatSahl(4)*
     &          (NSatSahx(33) + 4*NSatSahl(1)*NSatSahx(36)))/
     &        (MHp2*(MHp - 2*MT)**2*(MHp + 2*MT)**2)) -
     &    MT2*NSatSahx(72) +
     &    (NSatSahl(5)*NSatSahx(40))/(MHH2*(MHH2 - 4*MT2)) +
     &    2*NSatSahx(44) - 2*NSatSahx(50)

        NSatSahx(77) =
     &   MHH2**2 - MHp2**2 + 2*MT2*NSatSahx(75) - 2*NSatSahx(76)

        NSatSahx(85) =
     &   (NSatSahl(4)*NSatSahx(80))/MHp2 -
     &    (NSatSahl(5)*NSatSahx(84))/MHH2

        NSatSahx(74) =
     &   NSatSahx(29)/((MHH2 - MHp2)*MHp2*(MHp2 - 4*MT2)**2) +
     &    (2*NSatSahx(73))/(MHH2 - MHp2)**2

	NSatSahx(86) = (MHH2 - MHp2)*NSatSahx(77) + 2*NSatSahx(85)

        NSatSA0eqHH = -((3*Alfa2)/(256.D0*MW2**2*Pi**2*SW2**2)*
     &      (MT2*NSatSahx(86)/(MHH2 - MHp2)**2 + NSatSahx(74))/TB2)

	end
