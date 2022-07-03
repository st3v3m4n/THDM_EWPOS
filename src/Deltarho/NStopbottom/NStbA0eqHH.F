c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich


* NStbA0eqHH.F
* generated 29-Jan-2017 14:20

C> @brief This subroutine calculates \f$\delta\rho^{(2)}_\mathrm{tb,NS}\f$ with \f$m_{A^0}=m_{H^0}\f$.
C>
C> @ingroup group_rho
	subroutine calcNStbA0eqHH
	implicit none


#include "NStbA0eqHH.h"

	ComplexType dup1, dup2, dup3

	RealType T134sub

	external T134sub

	ComplexType A0FH, B0FH

	external A0FH, B0FH

#include "paraTHDM.h"
#include "paraSM.h"
	NStbahl(1) = B0FH(MHH2,MB2,MB2)

	NStbahl(10) = B0FH(MT2,MB2,MHp2)

	NStbahl(11) = B0FH(MT2,MHH2,MT2)

	NStbahl(12) = T134sub(MHH2,MB2,MB2)

	NStbahl(13) = T134sub(MHp2,MB2,MB2)

	NStbahl(14) = T134sub(MT2,MHH2,MB2)

	NStbahl(15) = T134sub(MT2,MT2,MHH2)

	NStbahl(16) = T134sub(MT2,MHp2,MB2)

	NStbahl(17) = T134sub(MT2,MT2,MHp2)

	NStbahl(2) = B0FH(MHH2,MT2,MT2)

	NStbahl(3) = A0FH(MB2)

	NStbahl(4) = A0FH(MT2)

	NStbahl(5) = A0FH(MHH2)

	NStbahl(6) = A0FH(MHp2)

	NStbahl(7) = B0FH(MHp2,MB2,MT2)

	NStbahl(8) = B0FH(MB2,MB2,MHH2)

	NStbahl(9) = B0FH(MB2,MHp2,MT2)

	NStbahx(167) = -2 + 8*TB*Yuk3 + 3*TB2*Yuk3**2

	NStbahx(168) = 1 + 6*TB*Yuk3 + 4*TB2*Yuk3**2

	NStbahx(194) = -5 + 6*TB*Yuk3 + 8*TB2*Yuk3**2

	NStbahx(198) = -1 + 28*TB*Yuk3 + 8*TB2*Yuk3**2

	NStbahx(199) = -20 + 8*TB*Yuk3 + 3*TB2*Yuk3**2

	NStbahx(200) = 3 + 2*TB*Yuk3 + TB2*Yuk3**2

	NStbahx(201) = 9 + 24*TB*Yuk3 + 7*TB2*Yuk3**2

	NStbahx(220) = MT2**2 + MT2*((MHp2 - MT2)*TB*Yuk3)

	NStbahx(222) = -3 + 24*TB*Yuk3 + 7*TB2*Yuk3**2

	NStbahx(223) = -4 + 38*TB*Yuk3 + 5*TB2*Yuk3**2

	NStbahx(224) = -2 + 2*TB*Yuk3 + 3*TB2*Yuk3**2

	NStbahx(225) = -7 + 16*TB*Yuk3 + TB2*Yuk3**2

	NStbahx(305) = MHp2 + MT2 + 2*(MB2 - MT2)*(TB*Yuk3)

	NStbahx(326) = TB - TB2*(2*Yuk3 + TB*Yuk3**2)

	NStbahx(327) = 5*TB + 3*TB2*(2*Yuk3 + TB*Yuk3**2)

	NStbahx(329) = 3*TB + TB2*(4*Yuk3 + 3*TB*Yuk3**2)

	NStbahx(330) = 2*TB + TB2*(8*Yuk3 + 3*TB*Yuk3**2)

	NStbahx(133) = 4*MT2**3 + 3*MT2**2*MHp2

	NStbahx(134) = -2 + 6*TB*Yuk3 + 3*TB2*Yuk3**2

	NStbahx(135) = 5 + 24*TB*Yuk3 - TB2*Yuk3**2

	NStbahx(137) = 1 + 12*TB*Yuk3 - 7*TB2*Yuk3**2

	NStbahx(138) = -3 + 4*TB*Yuk3 + 2*TB2*Yuk3**2

	NStbahx(139) = 1 + 4*TB*Yuk3 + 2*TB2*Yuk3**2

	NStbahx(140) = 9 + 14*TB*Yuk3 + 6*TB2*Yuk3**2

	NStbahx(142) = -4 + 2*TB*Yuk3 + TB2*Yuk3**2

	NStbahx(143) = 3 + 16*TB*Yuk3 + 10*TB2*Yuk3**2

	NStbahx(144) = 4 + 24*TB*Yuk3 + 11*TB2*Yuk3**2

	NStbahx(146) = -1 + 8*TB*Yuk3 + 4*TB2*Yuk3**2

	NStbahx(147) = 3 + 28*TB*Yuk3 + 12*TB2*Yuk3**2

	NStbahx(151) = 1 + 2*TB*Yuk3 - 3*TB2*Yuk3**2

	NStbahx(153) = 9 + 12*TB*Yuk3 - 8*TB2*Yuk3**2

	NStbahx(154) = 3 + 8*TB*Yuk3 - 2*TB2*Yuk3**2

	NStbahx(156) = 1 + 6*TB*Yuk3 + TB2*Yuk3**2

	NStbahx(157) = 4 + 6*TB*Yuk3 + TB2*Yuk3**2

	NStbahx(158) = 31 + 32*TB*Yuk3 + 29*TB2*Yuk3**2

	NStbahx(160) = -2 + 8*TB*Yuk3 + 7*TB2*Yuk3**2

	NStbahx(161) = -3 + 2*TB*Yuk3 + TB2*Yuk3**2

	NStbahx(162) = 33 + 16*TB*Yuk3 + 17*TB2*Yuk3**2

	NStbahx(163) = 9 + 8*(TB*Yuk3 + TB2*Yuk3**2)

	NStbahx(165) = 29 + 48*TB*Yuk3 + 29*TB2*Yuk3**2

	NStbahx(166) = 25 + 24*TB*Yuk3 + 42*TB2*Yuk3**2

        NStbahx(169) =
     &   -(MT2**4*NStbahx(167)) + MT2**3*(MHp2*NStbahx(168))

	NStbahx(178) = 3 + 8*TB*Yuk3 - 20*TB2*Yuk3**2

	NStbahx(180) = 11 + 36*TB*Yuk3 - 21*TB2*Yuk3**2

	NStbahx(181) = 4 + 8*TB*Yuk3 - 11*TB2*Yuk3**2

	NStbahx(183) = 23 + 16*TB*Yuk3 - 32*TB2*Yuk3**2

	NStbahx(184) = 15 + 52*TB*Yuk3 + 8*TB2*Yuk3**2

	NStbahx(186) = -32 + 16*TB*Yuk3 + 23*TB2*Yuk3**2

	NStbahx(187) = 8 + 6*TB*Yuk3 - 5*TB2*Yuk3**2

	NStbahx(188) = 1 + 12*TB*Yuk3 + TB2*Yuk3**2

	NStbahx(189) = 9 + 28*TB*Yuk3 + 15*TB2*Yuk3**2

	NStbahx(191) = -11 + 8*TB*Yuk3 + 4*TB2*Yuk3**2

	NStbahx(192) = 1 + 2*TB*Yuk3 + 3*TB2*Yuk3**2

	NStbahx(193) = 11 + 8*TB*Yuk3 + 11*TB2*Yuk3**2

        NStbahx(195) =
     &   MHp2**5*TB2*Yuk3**2 - MT2**3*(MHp2**2*NStbahx(194))

	NStbahx(197) = 2*MHp2**2 + 5*MT2**2 - 6*MT2*MHp2

        NStbahx(202) =
     &   MT2**2*(MHp2*NStbahx(198)) - MT2**3*NStbahx(199) -
     &    MT2*(MHp2**2*NStbahx(201)) + 2*MHp2**3*NStbahx(200)

	NStbahx(207) = 3*MHp2**2 - 4*MT2**2 - 5*MT2*MHp2

        NStbahx(208) =
     &   MHp2**3 + 23*MT2**3 - 16*MT2**2*MHp2 + 18*MT2*MHp2**2

	NStbahx(221) = MHp2**2 - 5*MT2*MHp2 + 2*NStbahx(220)

        NStbahx(226) =
     &   -(MT2*(MHp2**2*NStbahx(222))) +
     &    MT2**2*(MHp2*NStbahx(223)) - MT2**3*NStbahx(225) +
     &    MHp2**3*NStbahx(224)

	NStbahx(259) = -27 + 80*TB*Yuk3 + 5*TB2*Yuk3**2

	NStbahx(260) = 45 + 32*TB*Yuk3 - 5*TB2*Yuk3**2

	NStbahx(261) = 35 + 112*TB*Yuk3 + 5*TB2*Yuk3**2

	NStbahx(274) = 28*TB2*Yuk3 + 11*TB*(1 + TB2*Yuk3**2)

	NStbahx(275) = 4*TB2*Yuk3 + TB*(10 + TB2*Yuk3**2)

	NStbahx(276) = 36*TB2*Yuk3 + TB*(11 + 5*TB2*Yuk3**2)

	NStbahx(278) = 20*TB2*Yuk3 - TB*(21 - 5*TB2*Yuk3**2)

	NStbahx(279) = 20*TB2*Yuk3 + 11*TB*(1 + TB2*Yuk3**2)

	NStbahx(280) = 40*TB2*Yuk3 + TB*(30 + 13*TB2*Yuk3**2)

	NStbahx(283) = 5*TB + TB2*(8*Yuk3 + 5*TB*Yuk3**2)

	NStbahx(284) = TB + 2*TB2*(2*Yuk3 + 5*TB*Yuk3**2)

	NStbahx(286) = 40*TB2*Yuk3 - 3*TB*(1 + TB2*Yuk3**2)

	NStbahx(287) = 40*TB2*Yuk3 + TB*(13 + 30*TB2*Yuk3**2)

	NStbahx(295) = MHH2 + MT2 + 2*(MB2 - MT2)*(TB*Yuk3)

	NStbahx(302) = MB2 - MHp2 + MT2 + 4*MB2*(TB*Yuk3)

	NStbahx(304) = (MB2 - MHp2)**2 - MT2*(MB2 + MHp2)

	NStbahx(306) = (MHp2 - MT2)**2 - MB2*NStbahx(305)

        NStbahx(316) =
     &   (MT2 + MB2*(TB*Yuk3))**2 - MHp2*(MT2 + MB2*(TB2*Yuk3**2))

	NStbahx(320) = 2*TB2*Yuk3 + TB*(1 - TB2*Yuk3**2)

	NStbahx(322) = TB + TB2*(4*Yuk3 + TB*Yuk3**2)

	NStbahx(324) = 3*TB + 2*TB2*(4*Yuk3 + TB*Yuk3**2)

        NStbahx(328) =
     &   -2*MHp2**2*TB*TB2*Yuk3**2 + MT2**2*NStbahx(326) +
     &    MT2*(MHp2*NStbahx(327))

        NStbahx(331) =
     &   3*MHp2**2*TB*TB2*Yuk3**2 - 2*MT2*(MHp2*NStbahx(329)) +
     &    MT2**2*NStbahx(330)

	NStbahx(4) = 52*MB2**2 + 3*MHH2**2 - 26*MB2*MHH2

	NStbahx(52) = 10*TB + TB2*(8*Yuk3 - 3*TB*Yuk3**2)

	NStbahx(53) = 5*TB + 2*TB2*(4*Yuk3 + TB*Yuk3**2)

	NStbahx(55) = 3*TB - 2*TB2*(4*Yuk3 + 5*TB*Yuk3**2)

	NStbahx(71) = 11*MHH2*TB2*Yuk3**2 - 4*MT2*(1 - 9*TB2*Yuk3**2)

	NStbahx(72) = -4*MT2 + MHH2*(1 - 13*TB2*Yuk3**2)

	NStbahx(87) = 5*MHH2*TB2*Yuk3**2 - 4*MT2*(1 - TB2*Yuk3**2)

	NStbahx(100) = -42 + 384*TB*Yuk3 + 7*TB2*Yuk3**2

	NStbahx(101) = -11 + 128*TB*Yuk3 - 11*TB2*Yuk3**2

	NStbahx(102) = 7 + 64*TB*Yuk3 + 22*TB2*Yuk3**2

	NStbahx(103) = 69 + 224*TB*Yuk3 + 42*TB2*Yuk3**2

	NStbahx(105) = 15 + 256*TB*Yuk3 - 15*TB2*Yuk3**2

	NStbahx(106) = 1 + 320*TB*Yuk3 + TB2*Yuk3**2

	NStbahx(107) = 42 + 224*TB*Yuk3 + 69*TB2*Yuk3**2

	NStbahx(109) = 7 + 384*TB*Yuk3 - 42*TB2*Yuk3**2

	NStbahx(110) = -3 + 128*TB*Yuk3 - 100*TB2*Yuk3**2

	NStbahx(113) = 27 + 176*TB*Yuk3 - 50*TB2*Yuk3**2

	NStbahx(114) = 5 + 24*TB*Yuk3 - 36*TB2*Yuk3**2

	NStbahx(116) = 5 + 80*TB*Yuk3 - 27*TB2*Yuk3**2

	NStbahx(118) = -31 + 48*TB*Yuk3 - TB2*Yuk3**2

	NStbahx(119) = 9 + 32*TB*Yuk3 + TB2*Yuk3**2

	NStbahx(120) = 15 + 80*TB*Yuk3 + TB2*Yuk3**2

	NStbahx(122) = 38 + 128*TB*Yuk3 - 9*TB2*Yuk3**2

	NStbahx(123) = 5 + 9*TB*Yuk3 + TB2*Yuk3**2

	NStbahx(124) = -3 + 12*TB*Yuk3 - 2*TB2*Yuk3**2

	NStbahx(125) = 5 + 56*TB*Yuk3 + 18*TB2*Yuk3**2

	NStbahx(127) = 25 + 104*TB*Yuk3 - 19*TB2*Yuk3**2

	NStbahx(128) = -6 + 4*TB*Yuk3 + 3*TB2*Yuk3**2

        NStbahx(129) =
     &   MHp2**3*(32*TB + 5*TB2*Yuk3) -
     &    8*MT2*(MHp2**2*(9*TB + 5*TB2*Yuk3))

	NStbahx(132) = 4*(MHp2 - 2*MT2)*TB + (3*MHp2 - 4*MT2)*TB2*Yuk3

        NStbahx(136) =
     &   MHp2**3*Yuk3*(12*TB + TB2*Yuk3) -
     &    MT2*(MHp2**2*NStbahx(135)) - 2*NStbahx(133)*NStbahx(134)

        NStbahx(141) =
     &   MHp2**4*Yuk3*(4*TB - TB2*Yuk3) -
     &    MT2**2*(MHp2**2*NStbahx(137)) - 8*MT2**4*NStbahx(138) -
     &    MT2*(MHp2**3*NStbahx(139)) + 2*MT2**3*(MHp2*NStbahx(140))

        NStbahx(145) =
     &   MHp2**3*(1 - 3*TB2*Yuk3**2) + 4*MT2**3*NStbahx(142) +
     &    MT2*(MHp2**2*NStbahx(143)) - MT2**2*(MHp2*NStbahx(144))

        NStbahx(148) =
     &   MHp2**2*Yuk3*(12*TB + 5*TB2*Yuk3) -
     &    4*MT2**2*NStbahx(146) - MT2*(MHp2*NStbahx(147))

	NStbahx(150) = MHp2**2 + 4*MT2**2 - 6*MT2*MHp2

	NStbahx(152) = 9*MHp2*TB2*Yuk3**2 - 2*MT2*NStbahx(151)

        NStbahx(155) =
     &   31*MHp2**2*TB2*Yuk3**2 - 2*MT2*(MHp2*NStbahx(153)) -
     &    4*MT2**2*NStbahx(154)

        NStbahx(159) =
     &   25*MHp2**3*TB2*Yuk3**2 - 8*MT2**3*NStbahx(156) -
     &    4*MT2**2*(MHp2*NStbahx(157)) - MT2*(MHp2**2*NStbahx(158))

        NStbahx(16) = MHH2**2*(2*MHH2 - 3*MHp2) -
     &    2*MB2*(MHH2*(4*MHH2 - 7*MHp2)) + 4*MB2**2*(MHH2 - 3*MHp2)

        NStbahx(164) =
     &   MHp2**4*TB2*Yuk3**2 - 2*MT2**3*(MHp2*NStbahx(160)) +
     &    4*MT2**4*NStbahx(161) + MT2**2*(MHp2**2*NStbahx(162)) -
     &    MT2*(MHp2**3*NStbahx(163))

        NStbahx(170) =
     &   9*MHp2**4*TB2*Yuk3**2 + MT2**2*(MHp2**2*NStbahx(165)) -
     &    MT2*(MHp2**3*NStbahx(166)) + 4*NStbahx(169)

	NStbahx(176) = (2*MB2 - MHp2)*TB2*Yuk3**2 + 4*MT2*(TB*Yuk3)

	NStbahx(179) = 21*MHp2*TB2*Yuk3**2 - MT2*NStbahx(178)

        NStbahx(182) =
     &   35*MHp2**2*TB2*Yuk3**2 - MT2*(MHp2*NStbahx(180)) -
     &    3*MT2**2*NStbahx(181)

        NStbahx(185) =
     &   29*MHp2**3*TB2*Yuk3**2 -
     &    MT2**2*(MHp2*(3 + 2*TB2*Yuk3**2)) -
     &    MT2**3*NStbahx(183) - MT2*(MHp2**2*NStbahx(184))

        NStbahx(190) =
     &   12*MHp2**4*TB2*Yuk3**2 + MT2**4*NStbahx(186) +
     &    2*MT2**2*(MHp2**2*NStbahx(187)) +
     &    6*MT2**3*(MHp2*NStbahx(188)) - MT2*(MHp2**3*NStbahx(189))

        NStbahx(196) =
     &   MT2**4*(MHp2*(2 + 3*TB2*Yuk3**2)) -
     &    3*MT2**5*NStbahx(191) + 2*MT2*(MHp2**4*NStbahx(192)) -
     &    MT2**2*(MHp2**3*NStbahx(193)) - 2*NStbahx(195)

        NStbahx(203) =
     &   -(MB2*((MHp2 - MT2)*NStbahx(202))) +
     &    (MHp2 - MT2)**3*NStbahx(197)

        NStbahx(209) =
     &   (MHp2 + 2*MT2)*NStbahx(207) + 2*TB*Yuk3*NStbahx(208)

	NStbahx(212) = 5 + 38*TB*Yuk3 - 4*TB2*Yuk3**2

	NStbahx(214) = 7 + 24*TB*Yuk3 - 3*TB2*Yuk3**2

	NStbahx(216) = -2 + 16*TB*Yuk3 + 7*TB2*Yuk3**2

	NStbahx(217) = -1 + 32*TB*Yuk3 - TB2*Yuk3**2

        NStbahx(219) =
     &   MT2 + 2*(3*MHp2 + 8*MT2)*TB*Yuk3 -
     &    (9*MHp2 + 7*MT2)*TB2*Yuk3**2

        NStbahx(227) =
     &   -(MB2*NStbahx(226)) + (MHp2 - MT2)**2*NStbahx(221)

	NStbahx(231) = 1 + 2*TB*Yuk3 - 4*TB2*Yuk3**2

	NStbahx(233) = 15 + 32*TB*Yuk3 - 12*TB2*Yuk3**2

	NStbahx(234) = 2 + 4*TB*Yuk3 - 3*TB2*Yuk3**2

	NStbahx(236) = 3 + 6*TB*Yuk3 - 2*TB2*Yuk3**2

	NStbahx(237) = -7 + 12*TB*Yuk3 + TB2*Yuk3**2

	NStbahx(238) = 4 + 8*TB*Yuk3 - TB2*Yuk3**2

	NStbahx(239) = 13 + 16*TB*Yuk3 + 2*TB2*Yuk3**2

	NStbahx(241) = -1 + 24*TB*Yuk3 + 5*TB2*Yuk3**2

	NStbahx(242) = 2 + 4*TB*Yuk3 + TB2*Yuk3**2

	NStbahx(243) = 12 + 28*TB*Yuk3 + 3*TB2*Yuk3**2

	NStbahx(245) = 21 + 40*TB*Yuk3 + 7*TB2*Yuk3**2

	NStbahx(246) = 6 + 14*TB*Yuk3 + 9*TB2*Yuk3**2

	NStbahx(251) = -19 + 104*TB*Yuk3 + 25*TB2*Yuk3**2

	NStbahx(252) = -2 + 12*TB*Yuk3 - 3*TB2*Yuk3**2

	NStbahx(254) = -50 + 176*TB*Yuk3 + 27*TB2*Yuk3**2

	NStbahx(255) = 3 + 4*TB*Yuk3 - 6*TB2*Yuk3**2

	NStbahx(256) = 1 + 9*TB*Yuk3 + 5*TB2*Yuk3**2

	NStbahx(257) = -1 + 24*TB*Yuk3 - 18*TB2*Yuk3**2

        NStbahx(262) =
     &   MHp2**3*(1 + 5*TB2*Yuk3**2) + MT2**3*NStbahx(259) +
     &    MT2**2*(MHp2*NStbahx(260)) - MT2*(MHp2**2*NStbahx(261))

	NStbahx(263) = -9 + 128*TB*Yuk3 + 38*TB2*Yuk3**2

	NStbahx(264) = 1 + 56*TB*Yuk3 + 12*TB2*Yuk3**2

        NStbahx(269) =
     &   MHH2**2 + 3*MHH2*MHp2 + 8*MHp2**2 - 13*MT2*(MHH2 - MHp2)

	NStbahx(27) = 1 + 4*TB*Yuk3 + TB2*Yuk3**2

        NStbahx(271) =
     &   (29*MHp2 + 21*MT2)*TB*TB2*Yuk3**2 -
     &    5*MT2*(TB + 4*TB2*Yuk3)

        NStbahx(272) =
     &   (18*MHp2 - 11*MT2)*TB*TB2*Yuk3**2 -
     &    MT2*(5*TB + 36*TB2*Yuk3)

        NStbahx(277) =
     &   8*MHp2**3*TB*TB2*Yuk3**2 - MT2*(MHp2**2*NStbahx(274)) -
     &    2*MT2**2*(MHp2*NStbahx(275)) + MT2**3*NStbahx(276)

	NStbahx(28) = 6 + 8*TB*Yuk3 + TB2*Yuk3**2

        NStbahx(281) =
     &   -3*MHp2**3*TB*TB2*Yuk3**2 + MT2**3*NStbahx(278) +
     &    MT2*(MHp2**2*NStbahx(279)) - MT2**2*(MHp2*NStbahx(280))

        NStbahx(285) =
     &   -3*MHp2**2*TB*TB2*Yuk3**2 + 7*MT2**2*NStbahx(283) -
     &    2*MT2*(MHp2*NStbahx(284))

        NStbahx(288) =
     &   -19*MHp2**2*TB*TB2*Yuk3**2 + MT2**2*NStbahx(286) +
     &    MT2*(MHp2*NStbahx(287))

	NStbahx(292) = MB2 - MHH2 + MT2 + 4*MB2*(TB*Yuk3)

	NStbahx(294) = (MB2 - MHH2)**2 - MT2*(MB2 + MHH2)

	NStbahx(296) = (MHH2 - MT2)**2 - MB2*NStbahx(295)

	NStbahx(301) = 4*MB2*MT2 - (MB2 - MHp2 + MT2)**2

        NStbahx(303) =
     &   MB2*((MB2 - MHp2 + MT2)*TB2*Yuk3**2) + MT2*NStbahx(302)

        NStbahx(307) =
     &   MB2*(TB2*Yuk3**2*NStbahx(304)) - MT2*NStbahx(306)

        NStbahx(310) =
     &   (MT2 + MB2*(TB*Yuk3))**2 - MHH2*(MT2 + MB2*(TB2*Yuk3**2))

	NStbahx(313) = TB + TB2*(2*Yuk3 + TB*Yuk3**2)

        NStbahx(319) =
     &   MT2*((MHp2 - MT2)**2*(3*MHH2 - 4*MHp2 + 2*MT2)) +
     &    2*MB2**4*(TB2*Yuk3**2)

        NStbahx(321) =
     &   (3*MHH2 - 8*MHp2)*TB*TB2*Yuk3**2 + 2*MT2*NStbahx(320)

        NStbahx(323) =
     &   5*MHp2**2*TB*TB2*Yuk3**2 - 4*MT2*(MHp2*NStbahx(313)) -
     &    MT2**2*NStbahx(322)

	NStbahx(325) = -6*MHp2*TB*TB2*Yuk3**2 + MT2*NStbahx(324)

	NStbahx(332) = 2*(MHp2 - MT2)*NStbahx(328) + MHH2*NStbahx(331)

        NStbahx(337) =
     &   MHp2**2 - 2*MT2*(MHp2*(1 - 4*TB*Yuk3)) -
     &    MT2**2*(7 + 8*TB*Yuk3)

	NStbahx(37) = MHH2**2 - 4*MT2**2 - 2*MT2*MHH2

	NStbahx(38) = MHH2**2 + 20*MT2**2 - 10*MT2*MHH2

        NStbahx(384) =
     &   1/MT2*((MT2 + MB2*(TB2*Yuk3**2))*
     &       (Re(NStbahl(3)) - Re(NStbahl(6)))) -
     &    2*Re(NStbahl(5)) +
     &    2*(MT2*(3 + 1/MT2*Re(NStbahl(4))) +
     &       (MHH2 - 2*MT2)*Re(NStbahl(11)))

        NStbahx(395) =
     &   MT2*(Re(NStbahl(4)) + (MHH2 - 2*MT2)*Re(NStbahl(11)))

	NStbahx(40) = (MB2 + MT2)*TB + 4*MB2*(TB2*Yuk3)

	NStbahx(46) = 6*MHH2**2 - 28*MT2**2 - MT2*MHH2

	NStbahx(48) = 13*TB + TB2*(8*Yuk3 - 3*TB*Yuk3**2)

	NStbahx(49) = 74*TB + TB2*(8*Yuk3 + 5*TB*Yuk3**2)

	NStbahx(5) = 60*MB2**2 + (NStbahl(5)*NStbahx(4))/MHH2

	NStbahx(50) = -168*MT2**3 + MHH2**3*(1 - TB2*Yuk3**2)

        NStbahx(54) = -18*MHH2**2*TB*TB2*Yuk3**2 +
     &    4*MT2**2*NStbahx(52) + MT2*(MHH2*NStbahx(53))

	NStbahx(56) = 3*MHH2*TB*TB2*Yuk3**2 + 4*MT2*NStbahx(55)

	NStbahx(69) = MT2*MHH2 + MB2*(MHH2 - 8*MT2)

        NStbahx(70) = MT2*(MB2 + MT2)*(4*MB2 - MHH2) +
     &    MB2*(TB2*Yuk3**2*NStbahx(69))

        NStbahx(73) = MT2**2*MHH2 - 28*MB2**3*(TB2*Yuk3**2) +
     &    MB2**2*NStbahx(71) + MB2*MT2*NStbahx(72)

        NStbahx(77) = MHH2**2*(2*MHH2 - 3*MHp2) -
     &    2*MT2*(MHH2*(4*MHH2 - 7*MHp2)) + 4*MT2**2*(MHH2 - 3*MHp2)

	NStbahx(79) = 3*MHp2**2 - 16*MT2**2 - 4*MT2*MHp2

	NStbahx(80) = 3*MHp2**2 + 16*MT2**2 - 16*MT2*MHp2

	NStbahx(82) = 2*MHH2**2 - 3*MHH2*(MHp2 + 2*MT2) + 10*MT2*MHp2

        NStbahx(86) = -64*MT2**2 + MHH2**2*(1 - TB2*Yuk3**2) +
     &    4*MT2*(MHH2*(4 + TB2*Yuk3**2))

	NStbahx(88) = 16*MB2**3*(TB2*Yuk3**2) - MB2**2*NStbahx(87)

	NStbahx(93) = -7 + 64*TB*Yuk3 - 27*TB2*Yuk3**2

	NStbahx(95) = -173 + 192*TB*Yuk3 + TB2*Yuk3**2

	NStbahx(96) = -27 + 64*TB*Yuk3 - 7*TB2*Yuk3**2

	NStbahx(97) = 11 + 64*TB*Yuk3 + 9*TB2*Yuk3**2

	NStbahx(98) = 143 + 320*TB*Yuk3 + 29*TB2*Yuk3**2

	NStbahx(10) = 5*MHH2**2 + 4*MHH2*MHp2 + 3*MHp2**2

        NStbahx(104) =
     &   37*MHp2**4*TB2*Yuk3**2 - MT2**4*NStbahx(100) +
     &    2*MT2**2*(MHp2**2*NStbahx(101)) -
     &    8*MT2*(MHp2**3*NStbahx(102)) +
     &    4*MT2**3*(MHp2*NStbahx(103))

        NStbahx(108) =
     &   36*MHp2**3*TB2*Yuk3**2 - MT2*(MHp2**2*NStbahx(105)) -
     &    MT2**3*NStbahx(106) - 2*MT2**2*(MHp2*NStbahx(107))

        NStbahx(11) = MHH2 + MHp2 +
     &    1/MB2**2*((2*MB2 - MHH2)*NStbahl(3)**2)

        NStbahx(111) =
     &   54*MHp2**2*TB2*Yuk3**2 - MT2**2*NStbahx(109) -
     &    2*MT2*(MHp2*NStbahx(110))

        NStbahx(115) =
     &   96*MHp2**2*TB*Yuk3 + MT2**2*NStbahx(113) +
     &    2*MT2*(MHp2*NStbahx(114))

        NStbahx(117) =
     &   2*MHp2*Yuk3*(32*TB - 5*TB2*Yuk3) + MT2*NStbahx(116)

        NStbahx(121) =
     &   MHp2**3*(5 + TB2*Yuk3**2) + MT2*(MHp2**2*NStbahx(118)) +
     &    MT2**3*NStbahx(119) - MT2**2*(MHp2*NStbahx(120))

        NStbahx(126) =
     &   MHp2**4*Yuk3*(16*TB + 5*TB2*Yuk3) + MT2**4*NStbahx(122) +
     &    16*MT2**2*(MHp2**2*NStbahx(123)) -
     &    12*MT2**3*(MHp2*NStbahx(124)) -
     &    2*MT2*(MHp2**3*NStbahx(125))

        NStbahx(130) =
     &   MT2**3*NStbahx(127) - 6*MT2**2*(MHp2*NStbahx(128)) +
     &    Yuk3*NStbahx(129)

	NStbahx(14) = -2*MHH2**2 - (10*MB2 - 3*MHH2)*MHp2 + 6*MB2*MHH2

        NStbahx(149) =
     &   MT2**2*((3*MHp2 - 4*MT2)*(MHp2 - MT2)**3) -
     &    MB2**5*(Yuk3*NStbahx(132)) - MB2**3*NStbahx(136) +
     &    MB2**2*NStbahx(141) -
     &    MB2*MT2*((MHp2 - MT2)*NStbahx(145)) + MB2**4*NStbahx(148)

        NStbahx(17) = (4*MB2 - MHH2)*(2*MB2 - MHp2)*MHp2*
     &     NStbahl(13) + NStbahl(12)*NStbahx(16)

        NStbahx(171) =
     &   -(4*MB2**6*(TB2*Yuk3**2)) +
     &    MT2*((MHp2 - MT2)**3*NStbahx(150)) +
     &    2*MB2**5*NStbahx(152) - MB2**4*NStbahx(155) +
     &    MB2**3*NStbahx(159) + MB2*((MHp2 - MT2)*NStbahx(164)) -
     &    MB2**2*NStbahx(170)

	NStbahx(177) = MT2**2*(2*MB2 - MHp2) + MB2**2*NStbahx(176)

        NStbahx(204) =
     &   5*MB2**7*(TB2*Yuk3**2) - MB2**6*NStbahx(179) +
     &    MB2**5*NStbahx(182) - MB2**4*NStbahx(185) +
     &    MB2**3*NStbahx(190) + MB2**2*NStbahx(196) -
     &    MT2**2*NStbahx(203)

        NStbahx(210) =
     &   -((MHp2 + MT2)**2*(MHp2**2 - 2*MT2**2)*TB2*Yuk3**2) +
     &    MT2*NStbahx(209)

	NStbahx(211) = 1 + 23*TB*Yuk3 - 4*TB2*Yuk3**2

        NStbahx(213) =
     &   MHp2**2*Yuk3*(6*TB - 13*TB2*Yuk3) +
     &    MT2*(MHp2*NStbahx(212))

        NStbahx(215) =
     &   MHp2**3*Yuk3*(2*TB - 7*TB2*Yuk3) +
     &    MT2*(MHp2**2*NStbahx(214))

        NStbahx(218) =
     &   MT2**2*(MHp2*NStbahx(216)) + MT2**3*NStbahx(217)

	NStbahx(228) = MB2**5*NStbahx(219) + MT2**2*NStbahx(227)

	NStbahx(230) = MHp2*(1 - 4*TB*Yuk3) + MT2*(3 + 4*TB*Yuk3)

	NStbahx(232) = 15*MHp2*TB2*Yuk3**2 - 4*MT2*NStbahx(231)

        NStbahx(235) =
     &   21*MHp2**2*TB2*Yuk3**2 - MT2*(MHp2*NStbahx(233)) -
     &    8*MT2**2*NStbahx(234)

	NStbahx(24) = 8*MHp2*TB + (MHH2 + 7*MHp2)*TB2*Yuk3

        NStbahx(240) =
     &   3*MHp2**4*TB2*Yuk3**2 + 6*MT2**3*(MHp2*NStbahx(236)) -
     &    MT2**2*(MHp2**2*NStbahx(237)) - 4*MT2**4*NStbahx(238) -
     &    MT2*(MHp2**3*NStbahx(239))

        NStbahx(244) =
     &   MHp2**4*(3 - TB2*Yuk3**2) + 4*MT2**4*(1 + 2*TB*Yuk3) +
     &    MT2**2*(MHp2**2*NStbahx(241)) -
     &    MT2*(MHp2**3*NStbahx(242)) - MT2**3*(MHp2*NStbahx(243))

        NStbahx(247) =
     &   13*MHp2**3*TB2*Yuk3**2 - 8*MT2**3*NStbahx(236) -
     &    MT2*(MHp2**2*NStbahx(245)) - 2*MT2**2*(MHp2*NStbahx(246))

        NStbahx(249) =
     &   MT2 - (2*MHp2 - 9*MT2)*TB2*Yuk3**2 + 32*MT2*(TB*Yuk3)

	NStbahx(25) = MT2 - 2*(MHp2 + MT2)*TB2*Yuk3**2

	NStbahx(250) = MHp2*(5 + 16*TB*Yuk3) + MT2*(5 - 16*TB*Yuk3)

        NStbahx(253) =
     &   MHp2**3*TB2*Yuk3**2 +
     &    8*MT2*(MHp2**2*Yuk3*(8*TB - TB2*Yuk3)) +
     &    MT2**3*NStbahx(251) + 6*MT2**2*(MHp2*NStbahx(252))

        NStbahx(258) =
     &   MHp2**4*TB2*Yuk3**2 + MT2**4*NStbahx(254) +
     &    12*MT2**3*(MHp2*NStbahx(255)) +
     &    16*MT2**2*(MHp2**2*NStbahx(256)) +
     &    2*MT2*(MHp2**3*NStbahx(257))

        NStbahx(26) = -2*MHp2*Yuk3*(4*TB - TB2*Yuk3) +
     &    MT2*(7 - 6*TB2*Yuk3**2)

        NStbahx(265) =
     &   -(MB2*((MHp2 - MT2)*NStbahx(262))) -
     &    MB2**4*(MT2*NStbahx(263) + 2*MHp2*NStbahx(264))

        NStbahx(270) =
     &   -(13*MB2**4*((MHH2 - MHp2)*TB2*Yuk3**2)) +
     &    MT2*((MHp2 - MT2)**2*NStbahx(269))

        NStbahx(273) =
     &   MHH2**2*TB*TB2*Yuk3**2 + MHH2*NStbahx(271) -
     &    MHp2*NStbahx(272)

        NStbahx(289) =
     &   MHH2**2*TB*NStbahx(25) + MHp2*NStbahx(285) +
     &    MHH2*NStbahx(288)

        NStbahx(29) = MHp2**2*TB2*Yuk3**2 -
     &    2*MT2*(MHp2*NStbahx(27)) + MT2**2*NStbahx(28)

        NStbahx(293) =
     &   MB2*((MB2 - MHH2 + MT2)*TB2*Yuk3**2) + MT2*NStbahx(292)

        NStbahx(297) =
     &   MB2*(TB2*Yuk3**2*NStbahx(294)) - MT2*NStbahx(296)

	NStbahx(299) = MHp2**2 - MHH2*(2*MHp2 - MT2)

        NStbahx(30) = MHp2**2*TB2*Yuk3**2 -
     &    2*MHp2*(MT2 + MT2*(TB2*Yuk3**2)) -
     &    MT2**2*(2 - TB2*Yuk3**2)

        NStbahx(300) =
     &   MHH2*(MT2 + 4*MT2*(TB*Yuk3)) + TB2*Yuk3**2*NStbahx(299)

        NStbahx(308) =
     &   1/(MB2 - MT2)*NStbahx(307) +
     &    (MHp2*(MB2 - MHp2 + MT2)*NStbahx(303))/NStbahx(301)

        NStbahx(311) =
     &   MHH2*(MT2 + MB2*(TB2*Yuk3**2)) +
     &    1/(MB2 - MT2)*((MHH2 - MHp2)*NStbahx(310))

        NStbahx(312) =
     &   MT2*(MHH2 - 2*MHp2 + 2*MT2) + 2*MB2**2*(TB2*Yuk3**2)

        NStbahx(314) =
     &   (MHH2 - 2*MHp2)*TB*TB2*Yuk3**2 + 2*MT2*NStbahx(313)

        NStbahx(333) =
     &   MB2**3*NStbahx(321) +
     &    MB2**2*(2*NStbahx(323) + MHH2*NStbahx(325)) +
     &    MB2*NStbahx(332) + TB*NStbahx(319)

        NStbahx(336) =
     &   MT2 - 2*(MHp2 - 3*MT2)*TB2*Yuk3**2 + 8*MT2*(TB*Yuk3)

	NStbahx(338) = MHH2*(MHp2 - MT2)**2 - MHp2*NStbahx(337)

        NStbahx(339) =
     &   MHp2**2*TB2*Yuk3**2 + MT2**2*(6 - 7*TB2*Yuk3**2) -
     &    2*MT2*(MHp2*NStbahx(27))

	NStbahx(34) = MHH2*TB*Yuk3 - 2*MT2*(4 + 5*TB*Yuk3)

        NStbahx(342) =
     &   MHH2*(MT2 + MB2*(TB2*Yuk3**2)) -
     &    1/(MB2 - MT2)*((MHH2 - MHp2)*NStbahx(310))

	NStbahx(35) = MHH2*TB*Yuk3 - 2*MT2*(6 + 5*TB*Yuk3)

	NStbahx(362) = (MB2 - MHp2 + MT2)*TB + 4*MB2*(TB2*Yuk3)

	NStbahx(369) = MB2**2 - MB2*(MHp2 - MT2)

        NStbahx(372) =
     &   MB2*(TB2*Yuk3**2*Re(NStbahl(3))) + 2*MT2*Re(NStbahl(4))

        NStbahx(382) =
     &   4*MB2*(TB2*Yuk3) +
     &    1/MT2*((MB2 - MHp2 + MT2)*TB*(MT2 + MB2*(TB2*Yuk3**2)))

        NStbahx(383) =
     &   1/MT2*((MT2 + MB2*(TB2*Yuk3**2))*
     &       (Re(NStbahl(3)) - Re(NStbahl(6)))) -
     &    2*Re(NStbahl(5)) +
     &    2*(Re(NStbahl(4)) + (MHH2 - 2*MT2)*Re(NStbahl(11)))

        NStbahx(385) =
     &   MB2*(TB2*Yuk3*(4 + TB*Yuk3)) + 1/2.D0*(TB*NStbahx(384)) -
     &    1/2.D0*(NStbahx(382)*Re(NStbahl(10)))

        NStbahx(387) =
     &   Re(NStbahl(3)) - Re(NStbahl(5)) -
     &    (2*MB2 - MHH2)*Re(NStbahl(8))

        NStbahx(39) = 2*MB2*
     &     ((MHH2 - 4*MT2)*(MHH2 - 2*MT2)*TB2*Yuk3) +
     &    TB*(MT2*NStbahx(37) + MB2*NStbahx(38))

	NStbahx(391) = 3*MT2*TB + MB2*(TB2*Yuk3*(4 + TB*Yuk3))

	NStbahx(393) = MB2**2*MT2 - MT2**3

        NStbahx(394) =
     &   4*MB2*MT2*(TB2*Yuk3) +
     &    (MB2 - MHp2 + MT2)*TB*(MT2 + MB2*(TB2*Yuk3**2))

        NStbahx(396) =
     &   2*MT2*Re(NStbahl(5)) - 2*NStbahx(395) -
     &    (MT2 + MB2*(TB2*Yuk3**2))*
     &     (Re(NStbahl(3)) - Re(NStbahl(6)))

	NStbahx(397) = 3*MB2**2 - 4*MB2*MT2 + MT2**2

        NStbahx(41) = MB2*(MB2 + MT2)*(TB*TB2*Yuk3**2) +
     &    MT2*NStbahx(40)

	NStbahx(44) = MHH2*TB*Yuk3 - 8*MT2*(1 + TB*Yuk3)

        NStbahx(47) = 28*MB2**4*((MHH2 - 4*MT2)*TB2*Yuk3**2) +
     &    MT2**2*(MHH2*NStbahx(46))

        NStbahx(51) = -(4*MT2**2*(MHH2*NStbahx(48))) +
     &    MT2*(MHH2**2*NStbahx(49)) + 2*TB*NStbahx(50)

	NStbahx(57) = MB2**2*NStbahx(54) + MB2**3*NStbahx(56)

        NStbahx(6) = 5*MHH2**3 - 48*(MB2**3 + MB2*MHH2**2) +
     &    2*MHH2*NStbahx(5)

	NStbahx(63) = 3*MHH2**2 + 52*MT2**2 - 26*MT2*MHH2

        NStbahx(66) = 5*MB2 - 3*MT2 +
     &    8*(MB2 - MT2)*(MHH2 - 2*MT2)/(MHH2 - 4*MT2)

        NStbahx(74) = 2*MB2*(NStbahl(5)*NStbahx(70)) +
     &    MB2**2*NStbahx(73)

        NStbahx(78) = MHp2*(MHH2 - 4*MT2)*(MHp2 - 2*MT2)*
     &     NStbahl(17) + NStbahl(15)*NStbahx(77)

        NStbahx(81) = 6*MHH2**3 + MHH2**2*(3*MHp2 - 20*MT2) +
     &    64*MT2**2*NStbahl(4) - 4*MT2*NStbahx(79) -
     &    16*(MHH2 - 2*MT2)*NStbahl(4)**2 + MHH2*NStbahx(80)

        NStbahx(83) = 2*MHH2**3 - MHH2**2*(MHp2 + 12*MT2) -
     &    8*MT2**2*MHp2 + 4*
     &     (MT2*(MHH2*(MHp2 + 6*MT2)) + NStbahl(4)*NStbahx(82))

        NStbahx(85) = -(MB2*(MB2 + MT2)*
     &       ((MHH2 - 4*MT2)*TB2*Yuk3**2)) + MT2*NStbahx(69)

        NStbahx(89) = MT2**2*(MHH2*(5*MHH2 - 16*MT2)) -
     &    MB2*MT2*NStbahx(86) + (MHH2 - 4*MT2)*NStbahx(88)

	NStbahx(9) = 2*MHH2**2 + MHH2*MHp2 + MHp2**2

	NStbahx(92) = 6*MHp2**2 - 3*MT2**2 - 19*MT2*MHp2

	NStbahx(94) = 10*MHp2*TB2*Yuk3**2 - MT2*NStbahx(93)

        NStbahx(99) = 6*MHp2**4*TB2*Yuk3**2 -
     &    MT2**3*(MHp2*NStbahx(95)) + MT2**4*NStbahx(96) -
     &    3*MT2*(MHp2**3*NStbahx(97)) +
     &    MT2**2*(MHp2**2*NStbahx(98))

        NStbahx(1) = 12*MB2**3 - 2*MHH2**3 - 38*MB2**2*MHH2 +
     &    17*MB2*MHH2**2

        NStbahx(112) =
     &   3*MB2**6*(TB2*Yuk3**2) +
     &    MT2*((MHp2 - MT2)**3*NStbahx(92)) + MB2**5*NStbahx(94) +
     &    MB2*((MHp2 - MT2)*NStbahx(99)) - MB2**2*NStbahx(104) +
     &    2*MB2**3*NStbahx(108) - MB2**4*NStbahx(111)

        NStbahx(12) = 64*MB2**3 - 4*MB2*NStbahx(10) +
     &    16*MB2**2*NStbahx(11) + 3*MHH2*NStbahx(9)

        NStbahx(13) = MHH2**2*(2*MHH2 - MHp2) +
     &    4*MB2*((2*MB2 - MHH2)*(3*MHH2 - MHp2))

        NStbahx(131) =
     &   MT2**2*((MHp2 - MT2)**3*(MHp2 + MT2)) +
     &    MB2**6*(Yuk3*(16*TB - 5*TB2*Yuk3)) +
     &    MB2**4*NStbahx(115) - MB2**5*NStbahx(117) +
     &    MB2*MT2*((MHp2 - MT2)*NStbahx(121)) +
     &    MB2**2*NStbahx(126) - 2*MB2**3*NStbahx(130)

        NStbahx(15) = 4*MB2**2*(MHH2 - MHp2) -
     &    (4*MB2 - MHH2)*MHp2*NStbahl(6) - NStbahl(5)*NStbahx(14)

        NStbahx(172) =
     &   1/(MB2 - MT2)*(NStbahl(4)*NStbahx(149)) - NStbahx(171)

	NStbahx(174) = MT2**2 + MB2**2*(TB2*Yuk3**2)

        NStbahx(18) = (4*MB2 - MHH2)*(4*MB2 - MHp2)*MHp2*
     &     NStbahl(6) + 2*NStbahx(17)

	NStbahx(2) = -9*MHH2 + 2/MB2**2*((3*MB2 - MHH2)*NStbahl(3)**2)

        NStbahx(229) =
     &   2*MB2**6*(Yuk3*(TB - TB2*Yuk3)) + MB2**2*NStbahx(210) +
     &    MB2**4*(2*MT2**2*NStbahx(211) + NStbahx(213)) -
     &    MB2**3*(NStbahx(215) + 2*NStbahx(218)) - NStbahx(228)

        NStbahx(248) =
     &   4*MB2**6*(TB2*Yuk3**2) -
     &    MT2**2*(MHp2*(MHp2 - MT2)**2*NStbahx(230)) -
     &    MB2**5*NStbahx(232) + MB2**4*NStbahx(235) +
     &    MB2**2*NStbahx(240) + MB2*MT2*NStbahx(244) -
     &    MB2**3*NStbahx(247)

        NStbahx(266) =
     &   MB2**6*(TB2*Yuk3**2) + MB2**5*NStbahx(249) -
     &    MT2**2*((MHp2 - MT2)**3*NStbahx(250)) +
     &    2*MB2**3*NStbahx(253) - MB2**2*NStbahx(258) +
     &    MT2*NStbahx(265)

        NStbahx(268) =
     &   MHH2*(MHH2 - 3*MT2 - MB2*(TB*Yuk3))*NStbahl(15) -
     &    MHp2*(MHp2 - 3*MT2 - MB2*(TB*Yuk3))*NStbahl(17)

        NStbahx(298) =
     &   1/(MB2 - MT2)*((MHH2 - MHp2)*NStbahx(297)) +
     &    MHH2*NStbahx(293)

        NStbahx(3) = 84*MB2**3 - 2*MHH2**3 - 72*MB2**2*MHH2 +
     &    21*MB2*MHH2**2

        NStbahx(309) =
     &   MB2**2*(MHH2*TB2*Yuk3**2) + MT2*NStbahx(299) +
     &    MB2*NStbahx(300) + (MHH2 - MHp2)*NStbahx(308)

        NStbahx(31) = MT2*((MHH2 - MHp2)*(MHp2 - MT2)**2) +
     &    MB2**3*(Yuk3*NStbahx(24)) +
     &    MB2**2*(MHH2*NStbahx(25) + MHp2*NStbahx(26)) -
     &    MB2*(MHp2*NStbahx(29) - MHH2*NStbahx(30))

        NStbahx(315) =
     &   2*MHH2*NStbahl(4)*NStbahx(311) +
     &    (MHH2**2*(MB2*NStbahx(314) + TB*NStbahx(312)))/TB

        NStbahx(340) =
     &   MB2**3*((MHH2 - MHp2)*TB2*Yuk3**2) +
     &    MB2**2*(MHH2*NStbahx(25) - MHp2*NStbahx(336)) +
     &    MT2*NStbahx(338) +
     &    MB2*(MHH2*NStbahx(30) - MHp2*NStbahx(339))

        NStbahx(352) =
     &   MT2 - MB2*(TB2*Yuk3**2*Re(NStbahl(1))) -
     &    MT2*Re(NStbahl(2))

        NStbahx(353) =
     &   MB2**2*(TB2*Yuk3**2*Re(NStbahl(1))) +
     &    MT2**2*(1/MT2*Re(NStbahl(4)) + Re(NStbahl(2)))

	NStbahx(357) = MT2*TB + MB2*(TB2*Yuk3*(2 + TB*Yuk3))

        NStbahx(359) =
     &   (4*MB2 - MHH2)*Re(NStbahl(1)) + 2*Re(NStbahl(3))

        NStbahx(36) = 4*MB2**3*(TB*Yuk3) -
     &    MT2*(MHH2**2*(2 + TB*Yuk3)) + 2*MB2**2*NStbahx(34) -
     &    MB2*(MHH2*NStbahx(35))

        NStbahx(363) =
     &   MB2*((MB2 - MHp2 + MT2)*TB*TB2*Yuk3**2) + MT2*NStbahx(362)

	NStbahx(366) = MT2**2 + MT2*(MB2 - MHp2)

	NStbahx(368) = 2*MB2**2 - MB2*(MHH2 - MHp2 + 2*MT2)

	NStbahx(370) = 4*MB2*MT2*Yuk3 + TB*Yuk3**2*NStbahx(369)

	NStbahx(381) = 3*MB2*(TB*TB2*Yuk3**2) + MT2*(TB + 4*TB2*Yuk3)

	dup1 = -(TB*NStbahx(383)) + NStbahx(382)*Re(NStbahl(10))

        NStbahx(386) =
     &   3*MT2*TB + MB2*(TB2*Yuk3*(4 + TB*Yuk3)) -
     &    2*(dup1 + 1/MT2*(NStbahl(4)*NStbahx(385)))

        NStbahx(388) =
     &   -(2*MB2*(TB2*Yuk3**2*NStbahx(387))) -
     &    (MT2 + MB2*(TB2*Yuk3**2))*
     &     (Re(NStbahl(4)) - Re(NStbahl(6)))

        NStbahx(389) =
     &   2*MB2*NStbahx(381) - TB*NStbahx(388) -
     &    NStbahx(363)*Re(NStbahl(9))

	dup2 = TB*NStbahx(388) + NStbahx(363)*Re(NStbahl(9))

        NStbahx(392) =
     &   2*dup2 - 2*(2*MB2 - MT2)*dup1 - MB2*NStbahx(381) +
     &    (4*MB2 - 3*MT2)*NStbahx(391)

	dup3 = TB*NStbahx(396) + NStbahx(394)*Re(NStbahl(10))

        NStbahx(398) =
     &   (-MB2**3 + MB2*MT2**2)*NStbahx(381) +
     &    NStbahx(391)*NStbahx(393) +
     &    2*((MB2 - 3*MT2)*(MB2 - MT2)*dup3 + dup2*NStbahx(397))

	NStbahx(399) = (3*MB2 - 4*MT2)*NStbahx(381) + MT2*NStbahx(391)

	NStbahx(400) = (MB2 - 2*MT2)*dup2 + MB2*dup3

        NStbahx(42) = MT2*
     &     (NStbahl(15)*NStbahx(39))/(MHH2 - 4*MT2) +
     &    (MB2 - MHH2 + MT2)*NStbahl(14)*NStbahx(41)

	NStbahx(45) = MT2*(MHH2*(2 + TB*Yuk3)) + MB2*NStbahx(44)

        NStbahx(58) = MB2*MT2*NStbahx(51) - 3*TB*NStbahx(47) -
     &    (MHH2 - 4*MT2)*NStbahx(57)

        NStbahx(61) = 2*MHH2**3 - 12*MT2**3 + 38*MT2**2*MHH2 -
     &    17*MT2*MHH2**2

        NStbahx(62) = 5*MHH2**3 + 120*MT2**2*MHH2 -
     &    48*(MT2**3 + MT2*MHH2**2)

        NStbahx(64) = 2*MHH2**3 - 84*MT2**3 + 72*MT2**2*MHH2 -
     &    21*MT2*MHH2**2 + NStbahl(4)*NStbahx(63)

        NStbahx(67) = -(MB2*(MB2 + MT2)*(TB2*Yuk3**2)) +
     &    MT2*NStbahx(66)

        NStbahx(68) = -(MHH2*(TB + 2*TB2*Yuk3)) +
     &    8*MT2*(TB + TB2*Yuk3)

	NStbahx(7) = -63*MHH2**3 + 4/MB2*(NStbahl(3)*NStbahx(6))

        NStbahx(75) = 2*MB2*(NStbahl(4)*NStbahx(41))/TB -
     &    NStbahx(74)/(4*MB2 - MHH2)

        NStbahx(84) = 4*MHp2*(MHH2 - 4*MT2)*
     &     (MHp2 - 4*(MT2 + NStbahl(4)))*NStbahl(6) +
     &    8*NStbahx(78) - (MHH2 - MHp2)*NStbahx(81) -
     &    4*NStbahl(5)*NStbahx(83)

        NStbahx(90) = 1/(MB2 - MT2)**2*
     &     (NStbahl(4)*NStbahx(85)) +
     &    1/(MB2 - MT2)*NStbahx(89)/(4*MB2 - MHH2)

        NStbahx(173) =
     &   -(4/(MB2 - MT2)*(NStbahl(4)*NStbahx(131))) -
     &    NStbahx(112) + (8*NStbahl(6)*NStbahx(172))/MHp2

        NStbahx(19) = (MHH2 - MHp2)*NStbahx(12) +
     &    4*NStbahl(5)*NStbahx(13) + 16*NStbahl(3)*NStbahx(15) -
     &    4*NStbahx(18)

	NStbahx(21) = MT2 + (3*MB2 - MHH2)*TB*Yuk3

	NStbahx(22) = MT2 + (3*MB2 - MHp2)*TB*Yuk3

	NStbahx(23) = MB2**2 + (MHp2 - MT2)**2 - 2*MB2*(MHp2 + MT2)

        NStbahx(267) =
     &   -(4/(MB2 - MT2)**2*
     &       (NStbahl(4)*NStbahx(229))/
     &        ((MB - MHp - MT)**2*(MB + MHp - MT)**2*
     &          (MB - MHp + MT)**2*(MB + MHp + MT)**2)) +
     &    1/(MB2 - MT2)*((2*NStbahl(6)*NStbahx(248))/MHp2 +
     &        NStbahx(266))/NStbahx(23)**2

        NStbahx(32) = MT2*
     &    (2*NStbahl(4)**2 -
     &      (NStbahl(4)*NStbahx(31))/((MHH2 - MHp2)*NStbahx(23)))

        NStbahx(350) =
     &   MB2*(TB2*Yuk3**2*Re(NStbahl(1))) + MT2*Re(NStbahl(2))

        NStbahx(351) =
     &   MB2**2*(TB2*Yuk3**2*
     &       (1/MB2*Re(NStbahl(3)) + Re(NStbahl(1)))) +
     &    MT2**2*(1/MT2*Re(NStbahl(4)) + Re(NStbahl(2)))

        NStbahx(354) =
     &   MB2*(TB2*Yuk3**2*(MHH2 + 2*Re(NStbahl(3)))) -
     &    4*NStbahx(174) + MHH2*NStbahx(352) + 2*NStbahx(353)

        NStbahx(356) =
     &   -(MT2*(MHH2 - 4*MT2)) + MB2*((4*MB2 - MHH2)*TB2*Yuk3**2)

        NStbahx(358) =
     &   (2*MB2 - MHp2)*TB*(MT2 + MB2*(TB2*Yuk3**2)) +
     &    2*MT2*NStbahx(357)

        NStbahx(360) =
     &   MB2*(TB2*Yuk3**2*NStbahx(359)) +
     &    MT2*(-((MHH2 - 4*MT2)*Re(NStbahl(2))) + 2*Re(NStbahl(4)))

        NStbahx(361) =
     &   -(MT2*(MHH2*Re(NStbahl(2)))) -
     &    MB2*(TB2*Yuk3**2*
     &       (MHH2*Re(NStbahl(1)) - 2*Re(NStbahl(3)))) +
     &    2*MT2*Re(NStbahl(4))

        NStbahx(364) =
     &   (MT2 + MB2*(TB2*Yuk3**2))*
     &     (Re(NStbahl(3)) + Re(NStbahl(4))) +
     &    (NStbahx(363)*Re(NStbahl(7)))/TB

        NStbahx(367) =
     &   MT2*(2*MB2 + MHH2 - MHp2) -
     &    2*(MT2**2 + TB2*Yuk3**2*(2*MB2**2 - MB2*MHH2)*
     &        Re(NStbahl(1))) +
     &    2*((-2*MT2**2 + MT2*MHH2)*Re(NStbahl(2)) +
     &       (MT2 - MB2*(TB2*Yuk3**2))*
     &        (Re(NStbahl(3)) - Re(NStbahl(4))) +
     &       NStbahx(366)*Re(NStbahl(7)))

        NStbahx(371) =
     &   4*MB2*MT2*Yuk3 - TB*Yuk3**2*NStbahx(368) +
     &    2*NStbahx(370)*Re(NStbahl(7))

        NStbahx(390) =
     &   -2*dup2 + MB2*NStbahx(381) + MT2*NStbahx(386) -
     &    1/MB2*(NStbahl(3)*NStbahx(389))

        NStbahx(401) =
     &   2*MT2*(NStbahl(4)*NStbahx(392)) - NStbahx(398) -
     &    2*NStbahl(3)*(MB2*NStbahx(399) - 2*NStbahx(400))

        NStbahx(43) = MB2*
     &     (Yuk3*NStbahl(12)*NStbahx(36))/(4*MB2 - MHH2) +
     &    NStbahx(42)/TB2

        NStbahx(59) = (8*MB2)/(MB2 - MT2)**2*
     &     (Yuk3*NStbahl(3)**2*NStbahx(45)) -
     &    1/(MB2 - MT2)*NStbahx(58)/((MHH2 - 4*MT2)*TB2)

        NStbahx(65) = -192*MT2**4 + 144*MT2**3*MHH2 +
     &    88*MT2**2*MHH2**2 +
     &    MT2*(-63*MHH2**3 + 32*(MHH2 - 3*MT2)*NStbahl(4)**2) -
     &    8*NStbahl(15)*NStbahx(61) + 4*NStbahl(4)*NStbahx(62) +
     &    8*(MHH2**4 + NStbahl(5)*NStbahx(64))

        NStbahx(76) = MT2*
     &     ((2*(MT2*(MHH2*TB) - MB2*NStbahx(68))*NStbahl(4)**2)/
     &        ((MHH2 - 4*MT2)*TB) + NStbahl(4)*NStbahx(67)) -
     &    1/MB2*(NStbahl(3)*NStbahx(75))

        NStbahx(8) = 192*MB2**4 - 88*MB2**2*MHH2**2 +
     &    16*MB2**3*NStbahx(2) - MB2*NStbahx(7) -
     &    8*(MHH2**4 + NStbahl(12)*NStbahx(1)) +
     &    8*NStbahl(5)*NStbahx(3)

        NStbahx(91) = MT2*NStbahx(84)/(MHH2 - MHp2)**2 -
     &    4*NStbahl(5)*NStbahx(90)

        NStbahx(20) = NStbahx(8)/((4*MB2 - MHH2)**2*MHH2) -
     &    NStbahx(19)/((4*MB2 - MHH2)*(MHH2 - MHp2)**2)

        NStbahx(33) = -(MB2*
     &       (2*Yuk3**2*NStbahl(3)**2 -
     &         (2*Yuk3*(MHH2*NStbahl(12)*NStbahx(21) -
     &              MHp2*NStbahl(13)*NStbahx(22)))/
     &          ((MHH2 - MHp2)*TB))) + NStbahx(32)/TB2

        NStbahx(355) =
     &   MHH2*NStbahx(350) - 2*NStbahx(351) +
     &    (NStbahl(5)*NStbahx(354))/MHH2

        NStbahx(365) =
     &   -(MHp2*(NStbahx(356) - NStbahx(358)/TB + NStbahx(360) +
     &         NStbahx(361))) + 2*(2*MHH2 - MHp2)*NStbahx(364)

        NStbahx(60) = 8/(MB2 - MT2)**2*NStbahx(43) -
     &    NStbahx(59)/(4*MB2 - MHH2)

        NStbahx(343) =
     &   MT2 + MB2*(TB2*Yuk3**2) +
     &    ((MB2 - MHp2 - MT2)*NStbahx(303))/NStbahx(23)

        NStbahx(317) =
     &   MT2 + MB2*(TB2*Yuk3**2) + 1/(MB2 - MT2)*NStbahx(316) -
     &    ((MB2 + MHp2 - MT2)*NStbahx(303))/NStbahx(23)

	NStbahx(344) = 1/(MB2 - MT2)*NStbahx(316) - NStbahx(343)

        NStbahx(374) =
     &   (2*MB2 - MHp2)*TB*(MT2 + MB2*(TB2*Yuk3**2)) +
     &    2*(MT2*NStbahx(357) + NStbahx(363)*Re(NStbahl(7)))

        NStbahx(175) =
     &   -(2*MT2*(MT2 + MB2*(TB*Yuk3))**2) + MHp2*NStbahx(174)

        NStbahx(282) =
     &   MHH2**2*TB*NStbahx(30) + MHp2*NStbahx(277) -
     &    MHH2*NStbahx(281)

        NStbahx(318) =
     &   MHH2*(MT2 + MB2*(TB2*Yuk3**2)) +
     &    (MHH2 - MHp2)*NStbahx(317)

        NStbahx(341) =
     &   1/(MB2 - MT2)*(MT2 - MB2*(TB2*Yuk3**2)) +
     &    (2*MHp2*NStbahx(303))/((MHH2 - MHp2)*NStbahx(23))

        NStbahx(345) =
     &   MHH2*(MT2 + MB2*(TB2*Yuk3**2)) -
     &    (MHH2 - MHp2)*NStbahx(344)

        NStbahx(375) =
     &   1/2.D0*NStbahx(374)/TB +
     &    (MT2 + MB2*(TB2*Yuk3**2))*
     &     (Re(NStbahl(3)) + Re(NStbahl(4)))

        NStbahx(205) =
     &   -(NStbahl(17)*NStbahx(175)) + NStbahl(13)*NStbahx(177) -
     &    (NStbahl(16)*NStbahx(204))/NStbahx(23)**2

        NStbahx(290) =
     &   MB2**3*NStbahx(273) + MB2*NStbahx(282) +
     &    MB2**2*NStbahx(289) + TB*NStbahx(270)

        NStbahx(334) =
     &   2*MHp2*NStbahl(4)*NStbahx(318) +
     &    (MHp2**2*NStbahx(333))/(TB*NStbahx(23))

        NStbahx(346) =
     &   NStbahl(4)*NStbahx(341) -
     &    (NStbahl(5)*NStbahx(342) - NStbahl(6)*NStbahx(345))/
     &     (MHH2 - MHp2)**2

        NStbahx(373) =
     &   MB2*(TB2*Yuk3**2*NStbahx(359)) -
     &    MT2*((MHH2 - 4*MT2)*Re(NStbahl(2))) -
     &    MHH2*NStbahx(350) + 2*NStbahx(372)

        NStbahx(376) =
     &   -(MT2*(MHH2 - 4*MT2)) +
     &    MB2*((4*MB2 - MHH2)*TB2*Yuk3**2) - 2*NStbahx(375)

        NStbahx(379) =
     &   NStbahx(358) - TB*(MT2 + MB2*(TB2*Yuk3**2))*
     &     (Re(NStbahl(3)) + Re(NStbahl(4))) -
     &    NStbahx(363)*Re(NStbahl(7))

        NStbahx(206) =
     &   (NStbahl(3)**2 + NStbahl(4)**2)*NStbahx(174) -
     &    NStbahx(205)

        NStbahx(291) =
     &   -((8*MT2)/(MB2 - MT2)*NStbahx(268)) +
     &    NStbahx(290)/(TB*NStbahx(23))

        NStbahx(335) =
     &   2*NStbahl(14)*NStbahx(298) - 2*NStbahl(16)*NStbahx(309) +
     &    (NStbahl(5)*NStbahx(315))/MHH2 -
     &    (NStbahl(6)*NStbahx(334))/MHp2

        NStbahx(347) =
     &   MB2**2/(MB2 - MT2)*
     &     NStbahx(340)/((MHH2 - MHp2)*NStbahx(23)) -
     &    2*MB2*NStbahx(346)

        NStbahx(377) =
     &   -((MHH2 - 2*MHp2)*NStbahx(373)) + MHH2*NStbahx(376)

        NStbahx(380) =
     &   (NStbahl(6)*NStbahx(379))/MHp2 +
     &    TB*(MT2 + MB2*(TB2*Yuk3**2))*
     &     (Re(NStbahl(3)) + Re(NStbahl(4))) +
     &    NStbahx(363)*Re(NStbahl(7))

        NStbahx(348) =
     &   1/(MB2 - MT2)**2*(-4*NStbahx(76) + 8*NStbahx(206)) +
     &    NStbahx(173)/NStbahx(23)**2 -
     &    4*NStbahl(3)*NStbahx(267) -
     &    2*(MT2*NStbahx(65)/(MHH2*(MHH2 - 4*MT2)**2) +
     &       NStbahx(91)/(MHH2 - 4*MT2) +
     &       NStbahx(291)/(MHH2 - MHp2)) +
     &    8*(1/MB2*(NStbahl(3)*NStbahx(347)) +
     &       NStbahx(335)/(MHH2 - MHp2)**2)

        NStbahx(378) =
     &   ((MHH2 + MHp2)*(TB*NStbahx(367) + TB2*NStbahx(371)))/
     &     ((MHH2 - MHp2)*TB) -
     &    (2*NStbahl(5)*NStbahx(377))/(MHH2 - MHp2)**2

        NStbahx(402) =
     &   -(1/(MB2 - MT2)**2*NStbahx(401)) + 8*NStbahx(380) +
     &    4*NStbahx(390)

        NStbahx(349) =
     &   -(2*MB2*(Yuk3**2*NStbahx(20))) -
     &    8/(MB2 - MT2)*NStbahx(33) + NStbahx(60)/TB -
     &    NStbahx(348)/TB2

        NStbahx(403) =
     &   8*NStbahx(355) + (4*NStbahl(6)*NStbahx(365))/
     &     (MHH2 - MHp2)**2 - 2*NStbahx(378) - NStbahx(402)/TB

        NStbA0eqHH = -(Alfa2/(MW2**2*Pi**2*SW2**2)*
     &      (3/1024.D0*NStbahx(349) - 3/512.D0*NStbahx(403)/TB2))

	end
