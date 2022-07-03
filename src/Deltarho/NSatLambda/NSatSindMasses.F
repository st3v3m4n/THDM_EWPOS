c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich


* NSatSindMasses.F
* generated 22-Dec-2016 15:26

C> @brief This subroutine calculates \f$\delta\rho^{\alpha_t \lambda_i}_\mathrm{NS}\f$
C> with independent masses of the non-standard scalars.
C>
C> @ingroup group_rho
	subroutine calcNSatSindMasses
	implicit none


#include "NSatSindMasses.h"


	RealType T134sub

	external T134sub

	ComplexType A0FH, B0FH

	external A0FH, B0FH

#include "paraTHDM.h"
#include "paraSM.h"
	NSatSil(1) = A0FH(MT2)

	NSatSil(10) = T134sub(MT2,MHH2,0.D0)

	NSatSil(11) = T134sub(MT2,MHp2,0.D0)

	NSatSil(12) = T134sub(MT2,MT2,MHp2)

	NSatSil(13) = T134sub(MT2,MA02,0.D0)

	NSatSil(2) = B0FH(MHH2,MT2,MT2)

	NSatSil(3) = B0FH(MA02,MT2,MT2)

	NSatSil(4) = A0FH(MHH2)

	NSatSil(5) = A0FH(MA02)

	NSatSil(6) = B0FH(MHp2,0.D0,MT2)

	NSatSil(7) = A0FH(MHp2)

	NSatSil(8) = T134sub(MT2,MT2,MA02)

	NSatSil(9) = T134sub(MT2,MT2,MHH2)

	NSatSix(107) = 14*MHH2**2 - MHp2**2

	NSatSix(108) = MHp2**2 + 14*MT2**2 - 4*MT2*MHp2

	NSatSix(110) = (MHH2 - MHp2)*MHp2 - MT2*(2*MHH2 - 7*MHp2)

        NSatSix(154) =
     &   MHH2**2 + 1/MT2*((MA02 - MHH2)*(MA02 + MHH2)*
     &       Re(NSatSil(1)))

	NSatSix(164) = MHp2**2 + MT2*(3*MA02 - MHp2)

        NSatSix(167) =
     &   -2*(MA02 - MHp2)**2 +
     &    1/MT2*((MA02**2 - 2*MHp2**2)*Re(NSatSil(1)))

	NSatSix(168) = 3*MHp2**2 - 2*MHp2*(MA02 + MHp2*Re(NSatSil(3)))

        NSatSix(170) =
     &   2*(MA02 - MHp2)**2 +
     &    1/MT2*((MA02**2 - 2*MHp2**2)*Re(NSatSil(1)))

	NSatSix(171) = 3*MHp2 - 2*(MHp2 - MT2)*Re(NSatSil(6))

        NSatSix(178) =
     &   2*MHH2**2 - 4*MHH2*MHp2 + 4*MHp2**2 -
     &    1/MT2*((MHH2**2 - 2*MHp2**2)*Re(NSatSil(1)))

	NSatSix(179) = MHH2**2 + MHp2*(MHH2 - 4*MT2)*Re(NSatSil(2))

        NSatSix(181) =
     &   4*MHH2**2 - 12*MHH2*MHp2 + 6*MHp2**2 +
     &    1/MT2*((MHH2**2 - 2*MHp2**2)*Re(NSatSil(1)))

	NSatSix(182) = MHH2 - (MHH2 - 4*MT2)*Re(NSatSil(2))

        NSatSix(19) = MHp2**3 - 80*MT2**3 - 20*MT2**2*MHp2 +
     &    15*MT2*MHp2**2

	NSatSix(64) = 6*MA02**2 - MHp2**2

	NSatSix(65) = MHp2**2 + 6*MT2**2 - 4*MT2*MHp2

        NSatSix(109) =
     &   2*MHH2**3 - MT2*NSatSix(107) + MHH2*NSatSix(108)

	NSatSix(111) = MT2**2*(MHH2 - 6*MHp2) + MHH2*NSatSix(110)

	NSatSix(113) = 2*MHp2**2 - 32*MT2**2 + 3*MT2*MHp2

	NSatSix(114) = MHp2**2 + 2*MT2**2 - 4*MT2*MHp2

	NSatSix(115) = 19*MHp2**2 + 16*MT2**2 - 68*MT2*MHp2

	NSatSix(117) = MHp2**2 + 4*MT2**2 - 3*MT2*MHp2

	NSatSix(118) = MHp2**2 + 4*MT2**2 - 2*MT2*MHp2

	NSatSix(121) = MHp2**2 - 4*MT2**2 + 6*MT2*MHp2

	NSatSix(122) = 5*MHp2**2 - 3*MT2**2 + MT2*MHp2

	NSatSix(123) = MHH2**3 - MT2**2*(3*MHp2 - 2*MT2)

	NSatSix(125) = 3*MHp2**2 - 3*MT2**2 - 2*MT2*MHp2

        NSatSix(132) =
     &   MHp2**2*(MHp2 - 4*MT2) + 3*MHH2*MHp2*(MHp2 - 3*MT2) -
     &    MHH2**2*(MHp2 - MT2)

        NSatSix(133) =
     &   3*MHH2*MHp2*(MHp2 - 6*MT2) + 2*MHp2**2*(MHp2 - 4*MT2) +
     &    MHH2**2*(MHp2 + 2*MT2)

        NSatSix(136) =
     &   MHH2*(MHp2 + 7*MT2) + (MHH2 + 2*MHp2)*NSatSil(1)

	NSatSix(138) = MHp2**2 - 6*MT2**2 - MT2*MHp2

	NSatSix(139) = 3*MHp2**2 + 4*MT2**2 - 10*MT2*MHp2

	NSatSix(140) = MHp2**2 + 50*MT2**2 - 18*MT2*MHp2

        NSatSix(141) =
     &   2*MHH2*MHp2*(MHp2 - 7*MT2) + MHH2**2*(MHp2 - MT2) +
     &    3*MT2*MHp2**2

	NSatSix(144) = MHp2**2 + 10*MT2**2 - 8*MT2*MHp2

	NSatSix(155) = 2*MA02**2 - 2*MA02*MHH2 + NSatSix(154)

	NSatSix(156) = 3*MHH2 - 2*(MHH2 - 4*MT2)*Re(NSatSil(2))

        NSatSix(158) =
     &   3*MA02**2 - 6*MA02*MHH2 + 2*MHH2**2 -
     &    1/MT2*((MA02 - MHH2)*(MA02 + MHH2)*Re(NSatSil(1)))

	NSatSix(159) = 3*MHH2**2 - 2*MHH2*(MA02 + MHH2*Re(NSatSil(3)))

        NSatSix(163) =
     &   (MA02 - 3*MHp2)*Re(NSatSil(3)) + 3*MHp2*Re(NSatSil(6))

        NSatSix(165) =
     &   (5*MA02 - 7*MHp2)*Re(NSatSil(1)) +
     &    NSatSix(164)*Re(NSatSil(6))

        NSatSix(169) =
     &   -MHp2**3 - 2*MT2*NSatSix(167) + MA02*NSatSix(168) +
     &    2*MA02**2*(MHp2 - MT2)*Re(NSatSil(6))

        NSatSix(17) = 15*MHp2**3 - 32*MT2**3 +
     &    12*MT2**2*MHp2 - 43*MT2*MHp2**2

        NSatSix(172) =
     &   MA02**3 - 2*MT2*NSatSix(170) - MA02**2*NSatSix(171) +
     &    2*MA02*MHp2**2*(1 - Re(NSatSil(3)))

        NSatSix(175) =
     &   -2*(MHH2 + MHp2) +
     &    1/MT2*((5*MHH2 - 7*MHp2)*Re(NSatSil(1)))

        NSatSix(176) =
     &   (MHH2 - 3*MHp2)*(MHH2 - 4*MT2)*Re(NSatSil(2)) +
     &    (3*MHH2 - MHp2)*(MHp2 - MT2)*Re(NSatSil(6))

        NSatSix(18) = 2*MHp2**4 - 32*MT2**4 -
     &    136*MT2**3*MHp2 - 118*MT2**2*MHp2**2 + 53*MT2*MHp2**3

        NSatSix(180) =
     &   3*MHH2*MHp2**2 - MHp2**3 - 2*MHp2*NSatSix(179) +
     &    2*(MT2*NSatSix(178) +
     &       MHH2**2*(MHp2 - MT2)*Re(NSatSil(6)))

        NSatSix(183) =
     &   MHH2**3 - 2*MT2*NSatSix(181) - MHH2**2*NSatSix(171) +
     &    2*MHp2**2*NSatSix(182)

	NSatSix(20) = 5*MHH2**3*(MHp2 - 4*MT2) + 2*MT2*NSatSix(19)

	NSatSix(22) = 15*MHp2**2 - 8*MT2**2 - 28*MT2*MHp2

	NSatSix(23) = MHp2**2 + 40*MT2**2 - 20*MT2*MHp2

        NSatSix(29) = 15*MHp2**3 - 16*MT2**3 +
     &    24*MT2**2*MHp2 - 71*MT2*MHp2**2

        NSatSix(3) = 2*MHH2**3 + 32*MT2**3 + 66*MT2**2*MHH2 -
     &    25*MT2*MHH2**2

        NSatSix(30) = 2*MHp2**4 - 64*MT2**4 +
     &    112*MT2**3*MHp2 - 202*MT2**2*MHp2**2 + 29*MT2*MHp2**3

        NSatSix(31) = MHp2**3 - 32*(MT2**3 + MT2**2*MHp2) +
     &    3*MT2*MHp2**2

	NSatSix(33) = MHp2**2 + 24*MT2**2 - 8*MT2*MHp2

	NSatSix(34) = MHp2**2 + 56*MT2**2 - 8*MT2*MHp2

        NSatSix(4) = 3*MHH2**4 - 64*MT2**4 + 176*MT2**3*MHH2 -
     &    140*MT2**2*MHH2**2 + 10*MT2*MHH2**3

	NSatSix(42) = 2*MHp2**2 + 4*MT2**2 - 7*MT2*MHp2

	NSatSix(43) = MHp2**2 + 44*MT2**2 - 16*MT2*MHp2

	NSatSix(45) = 3*MA02**2 + 7*MA02*MHp2 + 14*MHp2**2

	NSatSix(66) = 2*MA02**3 - MT2*NSatSix(64) + MA02*NSatSix(65)

        NSatSix(67) = -(MA02*MHp2*(MA02 - MHp2 + 5*MT2)) +
     &    MT2**2*(MA02 + 4*MHp2)

        NSatSix(69) = 3*MHp2**3 - 16*MT2**3 + 34*MT2**2*MHp2 -
     &    20*MT2*MHp2**2

        NSatSix(70) = 2*MHp2**3 + 16*MT2**3 - 4*MT2**2*MHp2 -
     &    9*MT2*MHp2**2

        NSatSix(71) = 19*MHp2**3 + 224*MT2**2*MHp2 -
     &    128*(MT2**3 + MT2*MHp2**2)

	NSatSix(73) = 6*MA02**2 - 15*MA02*MHp2 + 4*MHp2**2

	NSatSix(74) = 5*MA02**2 + 4*MA02*MHp2 + MHp2**2

	NSatSix(77) = MHp2**2 - MT2**2 - 3*MT2*MHp2

	NSatSix(82) = MHp2**2 - 2*MT2**2 + 8*MT2*MHp2

	NSatSix(92) = 3*MA02**2 + 8*MA02*MHp2 + MHp2**2

	NSatSix(93) = MA02**2 + 6*MA02*MHp2 + MHp2**2

	NSatSix(1) = 3*MA02**4*MHH2 + 16*MT2**3*MHH2**2

	NSatSix(100) = (MA02 - MHH2)*MHH2**3 - 32*MT2**3*(MA02 + MHH2)

	NSatSix(103) = 4*MHH2**2 - 6*MHH2*MHp2 + MHp2**2

	NSatSix(104) = MHH2**2 - 3*MHH2*MHp2 - 2*MHp2**2

	NSatSix(105) = MHH2**2 - 6*MHH2*MHp2 - 3*MHp2**2

        NSatSix(11) = 15*MA02**3 - 17*MA02**2*MHH2 +
     &    5*MA02*MHH2**2 + MHH2**3

        NSatSix(112) =
     &   2/MT2*(NSatSil(1)*NSatSix(111)) -
     &    ((MHp2 - 4*MT2)*NSatSix(109))/((MHp - 2*MT)*(MHp + 2*MT))

        NSatSix(116) =
     &   2*MHp2**2*NSatSix(113) + 12*MHH2**2*NSatSix(114) -
     &    MHH2*MHp2*NSatSix(115)

        NSatSix(119) =
     &   2*MHH2**2*(MHp2 - 3*MT2)*(MHp2 + 2*MT2) -
     &    3*MHH2*MHp2*NSatSix(117) + MHp2**2*NSatSix(118)

        NSatSix(12) = 4*MA02**3 + 5*MA02**2*MHH2 -
     &    3*MA02*MHH2**2 + 2*MHH2**3

        NSatSix(124) =
     &   -(MT2*(MHH2*NSatSix(122))) + MHH2**2*NSatSix(121) -
     &    MHp2*NSatSix(123)

        NSatSix(126) =
     &   2*MHH2**2*(MHp2 - 2*MT2) +
     &    MHp2*(MHp2 - MT2)*(MHp2 + 2*MT2) - MHH2*NSatSix(125)

	NSatSix(129) = MHH2**3*MHp2 - 8*MT2**2*MHp2**2

	NSatSix(13) = 4*MA02**2 - MA02*MHH2 + MHH2**2

        NSatSix(130) =
     &   11*MHp2**3 - 16*MT2**3 + 48*MT2**2*MHp2 - 46*MT2*MHp2**2

        NSatSix(131) =
     &   5*MHp2**3 + 144*MT2**3 + 64*MT2**2*MHp2 - 54*MT2*MHp2**2

        NSatSix(134) =
     &   4/MT2*(NSatSil(1)*NSatSix(132)) +
     &    1/MT2**2*(NSatSil(1)**2*NSatSix(133))

        NSatSix(137) =
     &   MHH2**2*(7*MHp2 - 6*MT2) + 16*MT2*MHp2**2 -
     &    4*MHp2*NSatSix(136)

        NSatSix(142) =
     &   -4*MHH2*MHp2*NSatSix(138) + MHH2**2*NSatSix(139) -
     &    2*MHp2**2*NSatSix(140) + 4*NSatSil(1)*NSatSix(141)

        NSatSix(145) =
     &   -(6*MT2*(MHp2**2*(MHp2 - 5*MT2))) +
     &    MHH2**2*(MHp2**2 - 2*MT2*(MHp2 + MT2)) +
     &    2*MHH2*MHp2*NSatSix(144)

	NSatSix(146) = MHH2**2 + 2*MHH2*(MHp2 - 5*MT2) - 2*MT2*MHp2

        NSatSix(150) =
     &   MA02 + MHH2 + 4/MT2*((MA02 - MHH2)*Re(NSatSil(1)))

	NSatSix(151) = MHH2**2 + 4*MT2*(3*MA02 - MHH2)

        NSatSix(152) =
     &   3*MHH2*Re(NSatSil(2)) + (MA02 - 3*MHH2)*Re(NSatSil(3))

        NSatSix(157) =
     &   MA02**3 - 4*MT2*NSatSix(155) - MA02**2*NSatSix(156) +
     &    2*MA02*MHH2**2*(1 - Re(NSatSil(3)))

        NSatSix(160) =
     &   -MHH2**3 + 4*MT2*NSatSix(158) + MA02*NSatSix(159) +
     &    2*MA02**2*(MHH2 - 4*MT2)*Re(NSatSil(2))

        NSatSix(166) =
     &   MA02**2 - MHp2**2 - 2*MA02*NSatSix(163) + 2*NSatSix(165)

        NSatSix(173) =
     &   (NSatSil(7)*NSatSix(169))/MHp2 -
     &    (NSatSil(5)*NSatSix(172))/MA02

        NSatSix(177) =
     &   MHH2**2 - MHp2**2 + 2*MT2*NSatSix(175) - 2*NSatSix(176)

        NSatSix(184) =
     &   (NSatSil(7)*NSatSix(180))/MHp2 -
     &    (NSatSil(4)*NSatSix(183))/MHH2

        NSatSix(2) = 3*MHH2**5 - 1024*MT2**5 +
     &    320*MT2**4*MHH2 - 496*MT2**3*MHH2**2 +
     &    64*MT2**2*MHH2**3 - 4*MT2*MHH2**4

        NSatSix(21) = MHH2**2*NSatSix(17) - MHH2*NSatSix(18) +
     &    MHp2*NSatSix(20)

        NSatSix(24) = 8*MHH2**2*(MHp2 - MT2) -
     &    MHH2*NSatSix(22) - MHp2*NSatSix(23)

	NSatSix(27) = MHp2**2 + 2*MT2**2 - 5*MT2*MHp2

        NSatSix(32) = 5*MA02**3*(MHp2 - 4*MT2)**2 +
     &    2*MT2*(MHp2*NSatSix(31)) + MA02**2*NSatSix(29) -
     &    MA02*NSatSix(30)

        NSatSix(35) = 8*MA02**2*(MHp2 - 5*MT2) -
     &    MHp2*NSatSix(33) + MA02*NSatSix(34)

        NSatSix(38) = 11*MHp2**3 - 176*MT2**3 +
     &    200*MT2**2*MHp2 - 86*MT2*MHp2**2

        NSatSix(39) = 5*MHp2**3 + 48*MT2**3 + 24*MT2**2*MHp2 -
     &    26*MT2*MHp2**2

        NSatSix(40) = 5*MHp2**4 + 192*MT2**4 -
     &    304*MT2**3*MHp2 + 240*MT2**2*MHp2**2 - 58*MT2*MHp2**3

        NSatSix(44) = MT2*MA02**2 - MA02*NSatSix(42) -
     &    MHp2*NSatSix(43)

        NSatSix(46) = MHp2*(MA02 + MHp2)*(MA02 + 2*MHp2) +
     &    24*MT2**2*(MA02 + 3*MHp2) - 2*MT2*NSatSix(45)

        NSatSix(49) = 3*MHH2**3 - 448*MT2**3 +
     &    184*MT2**2*MHH2 - 32*MT2*MHH2**2

	NSatSix(5) = 2*MA02**3*NSatSix(3) - MA02*MHH2*NSatSix(4)

        NSatSix(50) = MHH2**3 - 256*MT2**3 + 100*MT2**2*MHH2 -
     &    14*MT2*MHH2**2

        NSatSix(51) = MHH2**3 - 64*MT2**3 + 26*MT2**2*MHH2 -
     &    7*MT2*MHH2**2

	NSatSix(55) = MHH2**2 + 36*(MT2**2 + MT2*MHH2)

	NSatSix(60) = 8*MA02**2 - 13*MA02*MHp2 + 4*MHp2**2

	NSatSix(61) = 10*MA02**2 - 19*MA02*MHp2 + 5*MHp2**2

	NSatSix(62) = 9*MA02**2 - 22*MA02*MHp2 + 5*MHp2**2

        NSatSix(68) = 2/MT2*(NSatSil(1)*NSatSix(67)) +
     &    ((MHp2 - 4*MT2)**2*NSatSix(66))/
     &     ((MHp - 2*MT)**2*(MHp + 2*MT)**2)

        NSatSix(7) = MHH2**3 + 16*MT2**3 + 13*MT2**2*MHH2 -
     &    8*MT2*MHH2**2

        NSatSix(72) = 4*MA02**2*NSatSix(69) +
     &    2*MHp2**2*NSatSix(70) - MA02*MHp2*NSatSix(71)

        NSatSix(75) = (MA02 - MHp2)*(2*MA02 - MHp2)*MHp2**3 +
     &    16*MT2**3*(MA02*(2*MA02 + 3*MHp2)) -
     &    MT2*(MHp2**2*NSatSix(73)) - 4*MT2**2*(MHp2*NSatSix(74))

        NSatSix(78) = MA02**3*(MHp2 - 2*MT2) -
     &    MA02**2*(MHp2**2 - 4*MT2**2) + MT2**2*MHp2**2 +
     &    MT2*(MA02*NSatSix(77))

	NSatSix(79) = MHp2*(MHp2 - 4*MT2) - MA02*(MHp2 - 2*MT2)

        NSatSix(8) = 3*MHH2**3 + 16*MT2**3 + 60*MT2**2*MHH2 -
     &    27*MT2*MHH2**2

        NSatSix(83) = MA02**3*(7*MHp2 - 2*MT2) -
     &    16*MT2**2*MHp2**2 + 16*MT2*(MA02*MHp2*(MHp2 + 2*MT2)) -
     &    4*MA02**2*NSatSix(82)

	NSatSix(84) = MA02*(MA02 + 2*MHp2) - 4*MT2*(2*MA02 + MHp2)

	NSatSix(86) = 3*MHp2**2 + 20*MT2**2 - 18*MT2*MHp2

	NSatSix(87) = MHp2**2 + 10*MT2**2 - 7*MT2*MHp2

        NSatSix(88) = MHp2**3 - 48*MT2**3 + 38*MT2**2*MHp2 -
     &    10*MT2*MHp2**2

        NSatSix(9) = MHH2**4 + 64*MT2**4 + 20*MT2**3*MHH2 +
     &    5*MT2**2*MHH2**2 - 7*MT2*MHH2**3

	NSatSix(90) = 14*MA02*MHp2 + 5*(MA02**2 + MHp2**2)

        NSatSix(94) = MA02*MHp2**2*(MA02 + 2*MHp2) -
     &    2*MT2*(MHp2*NSatSix(92)) + 6*MT2**2*NSatSix(93)

	NSatSix(95) = MA02*(MA02 + 2*MHp2) - 6*MT2*(MA02 + MHp2)

        NSatSix(10) = MT2**2*(MHH2**2*(MHH2 - 4*MT2)**2) -
     &    MT2*(MA02*MHH2*NSatSix(8)) - MA02**3*NSatSix(7) +
     &    MA02**2*NSatSix(9)

        NSatSix(101) =
     &   -(2*MT2*((3*MA02 - 2*MHH2)*MHH2**2*(MA02 + MHH2))) +
     &    2*MT2**2*((7*MA02 - 3*MHH2)*MHH2*(MA02 + 3*MHH2)) +
     &    MA02*NSatSix(100)

        NSatSix(106) =
     &   2*MHH2**2*MHp2**3 - 3*MHH2*MHp2**4 + MHp2**5 -
     &    2*MT2*(MHp2**2*NSatSix(103)) +
     &    4*MT2**2*(MHp2*NSatSix(104)) - 4*MT2**3*NSatSix(105)

        NSatSix(120) =
     &   -((NSatSil(4)*NSatSix(112))/(MHH2 - MT2)) +
     &    (NSatSil(7)*(2/MT2*(NSatSil(1)*NSatSix(119)) +
     &         NSatSix(116)))/(MHp2*(MHp2 - 4*MT2))

        NSatSix(127) =
     &   MHp2*(MHH2 - 4*MT2)*(MHH2 - MHp2 + 2*MT2)*NSatSil(9) +
     &    (NSatSil(10)*NSatSix(124))/(MHH2 - MT2) +
     &    NSatSil(11)*NSatSix(126)

        NSatSix(135) =
     &   -(4*MT2**2*NSatSix(134)) +
     &    2*(MHp2 - 4*MT2)*NSatSix(129) + MHH2**2*NSatSix(130) +
     &    MHH2*MHp2*NSatSix(131)

        NSatSix(14) = MA02*(5*MA02 - 3*MHH2)*(MA02 - MHH2)*
     &     MHH2**2 - 2*MT2*(MHH2*NSatSix(11)) +
     &    8*MT2**2*NSatSix(12) - 32*MT2**3*NSatSix(13)

        NSatSix(143) =
     &   ((MHp2 - 4*MT2)*NSatSil(4)*NSatSix(137))/MHH2 +
     &    (NSatSil(7)*NSatSix(142))/MHp2

        NSatSix(147) =
     &   MHH2*NSatSil(12)*NSatSix(145) -
     &    MHp2**2*(MHp2 - 4*MT2)*NSatSil(9)*NSatSix(146)

        NSatSix(153) =
     &   MA02**2 - MHH2**2 + 4*MT2*NSatSix(150) -
     &    2*MA02*NSatSix(152) + 2*NSatSix(151)*Re(NSatSil(2))

        NSatSix(16) = (MHH2 - MHp2)*MHp2**2 +
     &    2*MT2**2*(MHH2 - 5*MHp2) - MT2*((3*MHH2 - 5*MHp2)*MHp2)

        NSatSix(161) =
     &   (NSatSil(5)*NSatSix(157))/MA02 -
     &    (NSatSil(4)*NSatSix(160))/MHH2

	NSatSix(174) = (MA02 - MHp2)*NSatSix(166) + 2*NSatSix(173)

	NSatSix(185) = (MHH2 - MHp2)*NSatSix(177) + 2*NSatSix(184)

        NSatSix(25) = MT2*NSatSix(21) +
     &    4*MT2**2*(NSatSil(1)*NSatSix(24))

        NSatSix(28) = MHp2*(MHp2 - 5*MT2)*(MHp2 - 2*MT2) -
     &    MA02*NSatSix(27)

        NSatSix(36) = MT2*NSatSix(32) +
     &    4*MT2**2*(NSatSil(1)*NSatSix(35))

        NSatSix(41) = 2*MA02**3*(MHp2 - 4*MT2)**2 -
     &    4*MT2*(MHp2*NSatSix(39)) + MA02**2*NSatSix(38) +
     &    MA02*NSatSix(40)

        NSatSix(47) = 4/MT2*(NSatSil(1)*NSatSix(44)) +
     &    1/MT2**2*(NSatSil(1)**2*NSatSix(46))

        NSatSix(52) = MA02*MHH2*NSatSix(49) -
     &    MA02**2*NSatSix(50) - 2*MHH2**2*NSatSix(51)

        NSatSix(53) = (4*MA02 - MHH2)*MHH2**2 -
     &    MT2*((25*MA02 - MHH2)*MHH2) + 16*MT2**2*(2*MA02 + MHH2)

        NSatSix(56) = 2*MA02**4 - MA02**3*(3*MHH2 + 26*MT2) +
     &    20*MT2**2*MHH2**2 -
     &    2*MT2*(MA02*MHH2*(7*MHH2 + 20*MT2)) + MA02**2*NSatSix(55)

	NSatSix(57) = 3*MA02**2 - MT2*(3*MA02 + 5*MHH2)

        NSatSix(59) = (MA02 - MHp2)*(2*MA02 - MHp2)*MHp2 +
     &    MT2**2*MA02 - MT2*(2*MA02 - MHp2)**2

        NSatSix(6) = -(4*MT2*NSatSix(5)) +
     &    (MHH2 - 4*MT2)**2*NSatSix(1) - MA02**2*NSatSix(2)

        NSatSix(63) = (MA02 - MHp2)*(2*MA02 - MHp2)*MHp2**3 -
     &    2*MT2*(MHp2**2*NSatSix(60)) +
     &    4*MT2**2*(MHp2*NSatSix(61)) - 4*MT2**3*NSatSix(62)

        NSatSix(76) = (NSatSil(5)*NSatSix(68))/(MA02 - MT2) +
     &    (NSatSil(7)*(2/MT2*(NSatSil(1)*NSatSix(75)) +
     &         NSatSix(72)))/(MHp2*(MHp - 2*MT)**2*(MHp + 2*MT)**2)

        NSatSix(80) = (NSatSil(13)*NSatSix(78))/(MA02 - MT2) +
     &    MA02*NSatSil(8)*NSatSix(79)

        NSatSix(85) = -(MT2*
     &      (((MHp2 - 4*MT2)**2*NSatSix(83))/
     &         ((MHp - 2*MT)**2*(MHp + 2*MT)**2) -
     &        4*MHp2*NSatSil(1)*NSatSix(84)))

        NSatSix(89) = MA02**2*NSatSix(86) -
     &    2*MHp2*(2*MA02*NSatSix(87) + NSatSix(88))

        NSatSix(91) = MA02*MHp2*(MA02 + 2*MHp2) +
     &    16*MT2**2*(2*MA02 + MHp2) - MT2*NSatSix(90)

        NSatSix(96) = -((NSatSil(12)*NSatSix(94))/
     &       ((MHp - 2*MT)**2*(MHp + 2*MT)**2)) +
     &    (MHp2*NSatSil(8)*NSatSix(95))/(MA02 - 4*MT2)

	NSatSix(98) = 9*MA02**2 - 2*MA02*MHH2 + MHH2**2

        NSatSix(99) = MA02**2*(MA02 - MHH2) -
     &    2*MT2*(MA02*(5*MA02 - 3*MHH2))

        NSatSix(102) =
     &   -((NSatSil(8)*(2*MT2**2*NSatSix(98) + MHH2*NSatSix(99)))/
     &       (MA02*(MA02 - 4*MT2))) +
     &    (NSatSil(9)*NSatSix(101))/
     &     (MHH2*(MHH - 2*MT)**2*(MHH + 2*MT)**2)

        NSatSix(128) =
     &   -(MT2*NSatSix(120)) +
     &    (2*NSatSil(12)*NSatSix(106))/(MHp2*(MHp2 - 4*MT2)) -
     &    2*NSatSix(127)

        NSatSix(148) =
     &   (MHH2 - MHp2)*NSatSix(135) + 2*MHH2*MHp2*NSatSix(143) -
     &    4*NSatSix(147)

        NSatSix(15) = -(MT2*
     &      (NSatSix(6) + 16*NSatSil(1)*NSatSix(10) -
     &        4*NSatSil(1)**2*NSatSix(14)))

	NSatSix(162) = (MA02 - MHH2)*NSatSix(153) - 2*NSatSix(161)

        NSatSix(186) =
     &   NSatSix(174)/(MA02 - MHp2)**2 +
     &    NSatSix(185)/(MHH2 - MHp2)**2

        NSatSix(26) = 8*NSatSil(1)**2*NSatSix(16) -
     &    NSatSix(25)/(MHH2 - MT2)

        NSatSix(37) = 8*NSatSil(1)**2*NSatSix(28) +
     &    NSatSix(36)/(MA02 - MT2)

	NSatSix(48) = -(MT2*NSatSix(41)) + 4*MT2**3*NSatSix(47)

        NSatSix(54) = MT2*
     &    (NSatSix(52)/
     &       ((MA02 - MHH2)**2*(MHH - 2*MT)**2*(MHH + 2*MT)**2) -
     &      (4*NSatSil(1)*NSatSix(53))/
     &       ((MA02 - MHH2)*(MHH2 - 4*MT2)**2))

        NSatSix(58) = -(MT2*
     &      (NSatSix(56)/((MA02 - MHH2)**2*(MA02 - 4*MT2)) -
     &        (4*NSatSil(1)*NSatSix(57))/
     &         ((MA02 - MHH2)*(MA0 - 2*MT)*(MA0 + 2*MT))))

        NSatSix(81) = MT2*NSatSix(76) +
     &    2*NSatSil(11)*NSatSix(59) -
     &    2*((NSatSil(12)*NSatSix(63))/
     &        ((MHp - 2*MT)**2*(MHp + 2*MT)**2) + NSatSix(80))

        NSatSix(97) = -(MT2*
     &       (-((NSatSil(7)*
     &              (NSatSix(89) + 4*NSatSil(1)*NSatSix(91)))/
     &            ((MHp - 2*MT)**2*(MHp + 2*MT)**2)) -
     &         2*NSatSix(96))) -
     &    (NSatSil(5)*NSatSix(85))/(MA02*(MA02 - 4*MT2))

        NSatSix(149) =
     &   4*MT2*NSatSix(102)/(MA02 - MHH2)**2 +
     &    NSatSix(15)/
     &     (MA02*(MA02 - MHH2)*MHH2*(MA0 - 2*MT)*(MA0 + 2*MT)*
     &       (MHH2 - 4*MT2)**2) -
     &    NSatSix(26)/
     &     ((MHH2 - MHp2)*MHp2*(MHp - 2*MT)*(MHp + 2*MT)) +
     &    (NSatSix(37) + NSatSix(48)/(MA02 - 4*MT2))/
     &     ((MA02 - MHp2)*(MHp2 - 4*MT2)**2) +
     &    2*((NSatSil(4)*NSatSix(54))/MHH2 +
     &       (NSatSil(5)*NSatSix(58))/MA02 +
     &       NSatSix(81)/(MA02 - MHp2)**2) -
     &    (2*NSatSix(97))/(MA02 - MHp2)**2 +
     &    (-(MT2*NSatSix(148)/(MHH2*MHp2*(MHp2 - 4*MT2))) -
     &       2*NSatSix(128))/(MHH2 - MHp2)**2

	NSatSix(187) = NSatSix(162)/(MA02 - MHH2)**2 - NSatSix(186)

        NSatSindMasses =
     &   -(Alfa2/(MW2**2*Pi**2*SW2**2)*
     &      (3/512.D0*NSatSix(149) - (3*MT2)/512.D0*NSatSix(187))/TB2)

	end
