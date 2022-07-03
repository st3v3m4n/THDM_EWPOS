c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich


* NStbindMasses.F
* generated 29-Jan-2017 13:55

C> @brief This subroutine calculates \f$\delta\rho^{(2)}_\mathrm{tb,NS}\f$
C> with independent masses of the non-standard scalars.
C>
C> @ingroup group_rho

	subroutine calcNStbindMasses
	implicit none


#include "NStbindMasses.h"

	ComplexType dup1, dup2

	RealType T134sub

	external T134sub

	ComplexType A0FH, B0FH

	external A0FH, B0FH

#include "paraTHDM.h"
#include "paraSM.h"
	NStbil(1) = B0FH(MA02,MT2,MT2)

	NStbil(10) = B0FH(MHp2,MB2,MT2)

	NStbil(11) = B0FH(MB2,MA02,MB2)

	NStbil(12) = B0FH(MB2,MB2,MHH2)

	NStbil(13) = B0FH(MB2,MHp2,MT2)

	NStbil(14) = B0FH(MT2,MB2,MHp2)

	NStbil(15) = B0FH(MT2,MA02,MT2)

	NStbil(16) = B0FH(MT2,MHH2,MT2)

	NStbil(17) = T134sub(MB2,MB2,MA02)

	NStbil(18) = T134sub(MHp2,MB2,MB2)

	NStbil(19) = T134sub(MHH2,MB2,MB2)

	NStbil(2) = B0FH(MA02,MB2,MB2)

	NStbil(20) = T134sub(MT2,MB2,MA02)

	NStbil(21) = T134sub(MT2,MT2,MA02)

	NStbil(22) = T134sub(MT2,MHH2,MB2)

	NStbil(23) = T134sub(MT2,MT2,MHH2)

	NStbil(24) = T134sub(MT2,MHp2,MB2)

	NStbil(25) = T134sub(MT2,MT2,MHp2)

	NStbil(3) = B0FH(MHH2,MB2,MB2)

	NStbil(4) = B0FH(MHH2,MT2,MT2)

	NStbil(5) = A0FH(MHH2)

	NStbil(6) = A0FH(MT2)

	NStbil(7) = A0FH(MB2)

	NStbil(8) = A0FH(MA02)

	NStbil(9) = A0FH(MHp2)

	NStbix(489) = 3*MHp2 + 2*(MT2 + MT2*(TB*Yuk3))

	NStbix(237) = -2 + 8*TB*Yuk3 + 3*TB2*Yuk3**2

	NStbix(238) = 1 + 6*TB*Yuk3 + 4*TB2*Yuk3**2

	NStbix(264) = -5 + 6*TB*Yuk3 + 8*TB2*Yuk3**2

	NStbix(268) = -1 + 28*TB*Yuk3 + 8*TB2*Yuk3**2

	NStbix(269) = -20 + 8*TB*Yuk3 + 3*TB2*Yuk3**2

	NStbix(270) = 3 + 2*TB*Yuk3 + TB2*Yuk3**2

	NStbix(271) = 9 + 24*TB*Yuk3 + 7*TB2*Yuk3**2

	NStbix(290) = MT2**2 + MT2*((MHp2 - MT2)*TB*Yuk3)

	NStbix(292) = -3 + 24*TB*Yuk3 + 7*TB2*Yuk3**2

	NStbix(293) = -4 + 38*TB*Yuk3 + 5*TB2*Yuk3**2

	NStbix(294) = -2 + 2*TB*Yuk3 + 3*TB2*Yuk3**2

	NStbix(295) = -7 + 16*TB*Yuk3 + TB2*Yuk3**2

	NStbix(445) = MHp2 + 2*MT2 + 4*(MB2 - MT2)*(TB*Yuk3)

	NStbix(461) = -1 + 2*TB*Yuk3 + TB2*Yuk3**2

	NStbix(462) = 5 + 6*TB*Yuk3 + 3*TB2*Yuk3**2

	NStbix(464) = 3 + 4*TB*Yuk3 + 3*TB2*Yuk3**2

	NStbix(465) = 2 + 8*TB*Yuk3 + 3*TB2*Yuk3**2

	NStbix(483) = -2*MHp2**2 + MHH2*(3*MHp2 - MT2)

	NStbix(485) = MHp2*(2*MHp2 + MT2) - MHH2*(3*MHp2 + 2*MT2)

	NStbix(486) = 3*MHH2 - 2*MHp2 + 4*(MHH2 - MHp2)*TB*Yuk3

        NStbix(488) = (3*MHH2 - 2*MHp2)*(MB2**2 + MHp2**2) +
     &    MT2**2*MHH2 - 2*MT2*((2*MHH2 - MHp2)*MHp2)

        NStbix(490) = -(MHp2*
     &       (MT2 + 2*(MHp2 + MT2*(TB*Yuk3)))) + MHH2*NStbix(489)

	NStbix(126) = -7 + 8*TB*Yuk3 + 6*TB2*Yuk3**2

	NStbix(154) = -1 - 4/MT2*NStbil(6) + 2/MT2**2*NStbil(6)**2

	NStbix(203) = 4*MT2**3 + 3*MT2**2*MHp2

	NStbix(204) = -2 + 6*TB*Yuk3 + 3*TB2*Yuk3**2

	NStbix(205) = 5 + 24*TB*Yuk3 - TB2*Yuk3**2

	NStbix(207) = 1 + 12*TB*Yuk3 - 7*TB2*Yuk3**2

	NStbix(208) = -3 + 4*TB*Yuk3 + 2*TB2*Yuk3**2

	NStbix(209) = 1 + 4*TB*Yuk3 + 2*TB2*Yuk3**2

	NStbix(210) = 9 + 14*TB*Yuk3 + 6*TB2*Yuk3**2

	NStbix(212) = -4 + 2*TB*Yuk3 + TB2*Yuk3**2

	NStbix(213) = 3 + 16*TB*Yuk3 + 10*TB2*Yuk3**2

	NStbix(214) = 4 + 24*TB*Yuk3 + 11*TB2*Yuk3**2

	NStbix(216) = -1 + 8*TB*Yuk3 + 4*TB2*Yuk3**2

	NStbix(217) = 3 + 28*TB*Yuk3 + 12*TB2*Yuk3**2

	NStbix(221) = 1 + 2*TB*Yuk3 - 3*TB2*Yuk3**2

	NStbix(223) = 9 + 12*TB*Yuk3 - 8*TB2*Yuk3**2

	NStbix(224) = 3 + 8*TB*Yuk3 - 2*TB2*Yuk3**2

	NStbix(226) = 1 + 6*TB*Yuk3 + TB2*Yuk3**2

	NStbix(227) = 4 + 6*TB*Yuk3 + TB2*Yuk3**2

	NStbix(228) = 31 + 32*TB*Yuk3 + 29*TB2*Yuk3**2

	NStbix(230) = -2 + 8*TB*Yuk3 + 7*TB2*Yuk3**2

	NStbix(231) = -3 + 2*TB*Yuk3 + TB2*Yuk3**2

	NStbix(232) = 33 + 16*TB*Yuk3 + 17*TB2*Yuk3**2

	NStbix(233) = 9 + 8*(TB*Yuk3 + TB2*Yuk3**2)

	NStbix(235) = 29 + 48*TB*Yuk3 + 29*TB2*Yuk3**2

	NStbix(236) = 25 + 24*TB*Yuk3 + 42*TB2*Yuk3**2

        NStbix(239) = -(MT2**4*NStbix(237)) +
     &    MT2**3*(MHp2*NStbix(238))

	NStbix(248) = 3 + 8*TB*Yuk3 - 20*TB2*Yuk3**2

	NStbix(250) = 11 + 36*TB*Yuk3 - 21*TB2*Yuk3**2

	NStbix(251) = 4 + 8*TB*Yuk3 - 11*TB2*Yuk3**2

	NStbix(253) = 23 + 16*TB*Yuk3 - 32*TB2*Yuk3**2

	NStbix(254) = 15 + 52*TB*Yuk3 + 8*TB2*Yuk3**2

	NStbix(256) = -32 + 16*TB*Yuk3 + 23*TB2*Yuk3**2

	NStbix(257) = 8 + 6*TB*Yuk3 - 5*TB2*Yuk3**2

	NStbix(258) = 1 + 12*TB*Yuk3 + TB2*Yuk3**2

	NStbix(259) = 9 + 28*TB*Yuk3 + 15*TB2*Yuk3**2

	NStbix(261) = -11 + 8*TB*Yuk3 + 4*TB2*Yuk3**2

	NStbix(262) = 1 + 2*TB*Yuk3 + 3*TB2*Yuk3**2

	NStbix(263) = 11 + 8*TB*Yuk3 + 11*TB2*Yuk3**2

        NStbix(265) = MHp2**5*TB2*Yuk3**2 -
     &    MT2**3*(MHp2**2*NStbix(264))

	NStbix(267) = 2*MHp2**2 + 5*MT2**2 - 6*MT2*MHp2

        NStbix(272) = MT2**2*(MHp2*NStbix(268)) -
     &    MT2**3*NStbix(269) - MT2*(MHp2**2*NStbix(271)) +
     &    2*MHp2**3*NStbix(270)

	NStbix(277) = 3*MHp2**2 - 4*MT2**2 - 5*MT2*MHp2

        NStbix(278) = MHp2**3 + 23*MT2**3 - 16*MT2**2*MHp2 +
     &    18*MT2*MHp2**2

	NStbix(291) = MHp2**2 - 5*MT2*MHp2 + 2*NStbix(290)

        NStbix(296) = -(MT2*(MHp2**2*NStbix(292))) +
     &    MT2**2*(MHp2*NStbix(293)) - MT2**3*NStbix(295) +
     &    MHp2**3*NStbix(294)

	NStbix(329) = -27 + 80*TB*Yuk3 + 5*TB2*Yuk3**2

	NStbix(330) = 45 + 32*TB*Yuk3 - 5*TB2*Yuk3**2

	NStbix(331) = 35 + 112*TB*Yuk3 + 5*TB2*Yuk3**2

	NStbix(339) = MB2 - MHp2 + MT2 + 4*MB2*(TB*Yuk3)

	NStbix(362) = TB + TB2*(2*Yuk3 + TB*Yuk3**2)

	NStbix(369) = TB + TB2*(2*Yuk3 - TB*Yuk3**2)

	NStbix(371) = TB + TB2*(4*Yuk3 + TB*Yuk3**2)

	NStbix(373) = TB - TB2*(2*Yuk3 + TB*Yuk3**2)

	NStbix(374) = 5*TB + 3*TB2*(2*Yuk3 + TB*Yuk3**2)

	NStbix(378) = 3*TB + 2*TB2*(4*Yuk3 + TB*Yuk3**2)

	NStbix(380) = 3*TB + TB2*(4*Yuk3 + 3*TB*Yuk3**2)

	NStbix(381) = 2*TB + TB2*(8*Yuk3 + 3*TB*Yuk3**2)

	NStbix(406) = 5 + 16*TB*Yuk3 - 29*TB2*Yuk3**2

	NStbix(408) = 5 + 32*TB*Yuk3 + 3*TB2*Yuk3**2

	NStbix(411) = 3 + 32*TB*Yuk3 + 5*TB2*Yuk3**2

	NStbix(412) = 11 + 32*TB*Yuk3 + 11*TB2*Yuk3**2

	NStbix(414) = -29 + 16*TB*Yuk3 + 5*TB2*Yuk3**2

	NStbix(415) = 11 + 16*TB*Yuk3 + 11*TB2*Yuk3**2

	NStbix(416) = 38 + 32*TB*Yuk3 + 13*TB2*Yuk3**2

	NStbix(421) = -7 + 32*TB*Yuk3 - 7*TB2*Yuk3**2

	NStbix(422) = 13 + 32*TB*Yuk3 + 38*TB2*Yuk3**2

	NStbix(424) = 39 + 64*TB*Yuk3 + 39*TB2*Yuk3**2

	NStbix(432) = MHH2 + 2*MT2 + 4*(MB2 - MT2)*(TB*Yuk3)

	NStbix(436) = 1 + 2*TB*Yuk3 + TB2*Yuk3**2

        NStbix(444) = 2*MB2**2 + MHp2*(MHp2 - MT2) -
     &    MB2*(3*MHp2 + 2*MT2)

        NStbix(446) = MHp2**2 + 2*MT2**2 - 3*MT2*MHp2 -
     &    MB2*NStbix(445)

        NStbix(452) = (3*MHH2 - 2*MHp2)*(MHp2 - MT2)**2 +
     &    MB2**2*MHH2 - 2*MB2*((2*MHH2 - MHp2)*(MHp2 + MT2))

	NStbix(455) = 1 + 2*TB*Yuk3 - TB2*Yuk3**2

	NStbix(457) = 1 + 4*TB*Yuk3 + TB2*Yuk3**2

	NStbix(459) = 3 + 8*TB*Yuk3 + 2*TB2*Yuk3**2

        NStbix(463) = 2*MHp2**2*TB2*Yuk3**2 +
     &    MT2**2*NStbix(461) - MT2*(MHp2*NStbix(462))

        NStbix(466) = 3*MHp2**2*TB2*Yuk3**2 -
     &    2*MT2*(MHp2*NStbix(464)) + MT2**2*NStbix(465)

        NStbix(484) = 4*MB2*((MHH2 - MHp2)*MHp2*TB*Yuk3) -
     &    (MHp2 - MT2)*NStbix(483)

	NStbix(487) = MT2*NStbix(486) + 2*TB2*Yuk3**2*NStbix(485)

	NStbix(491) = -(2*MT2*NStbix(490)) + TB2*Yuk3**2*NStbix(488)

        NStbix(1) = MA02**2 - MA02*(6*MB2 + MHH2) +
     &    2*MB2*(8*MB2 + MHH2)

	NStbix(108) = 4*(MB2 - MT2)*Yuk3 + (3*MB2 - MT2)*(TB*Yuk3**2)

        NStbix(114) = -3*MT2**2 + 3*MB2**2*(TB2*Yuk3**2) +
     &    MB2*(MT2 - MT2*(TB2*Yuk3**2))

        NStbix(115) = -3*MT2**3 + 3*MB2**3*(TB2*Yuk3**2) -
     &    MB2*MT2**2*(4 + TB2*Yuk3**2) +
     &    MB2**2*(MT2 + 4*MT2*(TB2*Yuk3**2))

	NStbix(127) = 2*MHp2*Yuk3*(8*TB - TB2*Yuk3) + MT2*NStbix(126)

	NStbix(128) = 4*TB*Yuk3 - TB2*Yuk3**2

	NStbix(130) = 2 + 4*TB*Yuk3 - TB2*Yuk3**2

	NStbix(131) = -1 + 4*TB*Yuk3 - TB2*Yuk3**2

	NStbix(133) = 6 + 4*TB*Yuk3 + TB2*Yuk3**2

	NStbix(138) = 7*TB + 2*TB2*(4*Yuk3 - 3*TB*Yuk3**2)

	NStbix(140) = TB + TB2*(8*Yuk3 + TB*Yuk3**2)

	NStbix(141) = 6*TB + TB2*(12*Yuk3 + TB*Yuk3**2)

	NStbix(146) = 2*MHH2**2 - MHH2*(3*MHp2 + 2*MT2) + 6*MT2*MHp2

	NStbix(152) = 2*MA02**2 - 3*MA02*(MHp2 + 2*MT2) + 10*MT2*MHp2

	NStbix(155) = 3*MHp2**2 - 10*MT2*MHp2 - 8*MT2**2*NStbix(154)

	NStbix(163) = -7 + 64*TB*Yuk3 - 27*TB2*Yuk3**2

	NStbix(165) = -173 + 192*TB*Yuk3 + TB2*Yuk3**2

	NStbix(166) = -27 + 64*TB*Yuk3 - 7*TB2*Yuk3**2

	NStbix(167) = 11 + 64*TB*Yuk3 + 9*TB2*Yuk3**2

	NStbix(168) = 143 + 320*TB*Yuk3 + 29*TB2*Yuk3**2

	NStbix(170) = -42 + 384*TB*Yuk3 + 7*TB2*Yuk3**2

	NStbix(171) = -11 + 128*TB*Yuk3 - 11*TB2*Yuk3**2

	NStbix(172) = 7 + 64*TB*Yuk3 + 22*TB2*Yuk3**2

	NStbix(173) = 69 + 224*TB*Yuk3 + 42*TB2*Yuk3**2

	NStbix(175) = 15 + 256*TB*Yuk3 - 15*TB2*Yuk3**2

	NStbix(176) = 1 + 320*TB*Yuk3 + TB2*Yuk3**2

	NStbix(177) = 42 + 224*TB*Yuk3 + 69*TB2*Yuk3**2

	NStbix(179) = 7 + 384*TB*Yuk3 - 42*TB2*Yuk3**2

	NStbix(180) = -3 + 128*TB*Yuk3 - 100*TB2*Yuk3**2

	NStbix(183) = 27 + 176*TB*Yuk3 - 50*TB2*Yuk3**2

	NStbix(184) = 5 + 24*TB*Yuk3 - 36*TB2*Yuk3**2

	NStbix(186) = 5 + 80*TB*Yuk3 - 27*TB2*Yuk3**2

	NStbix(188) = -31 + 48*TB*Yuk3 - TB2*Yuk3**2

	NStbix(189) = 9 + 32*TB*Yuk3 + TB2*Yuk3**2

	NStbix(190) = 15 + 80*TB*Yuk3 + TB2*Yuk3**2

	NStbix(192) = 38 + 128*TB*Yuk3 - 9*TB2*Yuk3**2

	NStbix(193) = 5 + 9*TB*Yuk3 + TB2*Yuk3**2

	NStbix(194) = -3 + 12*TB*Yuk3 - 2*TB2*Yuk3**2

	NStbix(195) = 5 + 56*TB*Yuk3 + 18*TB2*Yuk3**2

	NStbix(197) = 25 + 104*TB*Yuk3 - 19*TB2*Yuk3**2

	NStbix(198) = -6 + 4*TB*Yuk3 + 3*TB2*Yuk3**2

        NStbix(199) = MHp2**3*(32*TB + 5*TB2*Yuk3) -
     &    8*MT2*(MHp2**2*(9*TB + 5*TB2*Yuk3))

	NStbix(2) = -((MA02 + 2*MB2)*MHH2) + MHH2**2 + 6*MB2*MA02

	NStbix(202) = 4*(MHp2 - 2*MT2)*TB + (3*MHp2 - 4*MT2)*TB2*Yuk3

        NStbix(206) = MHp2**3*Yuk3*(12*TB + TB2*Yuk3) -
     &    MT2*(MHp2**2*NStbix(205)) - 2*NStbix(203)*NStbix(204)

        NStbix(211) = MHp2**4*Yuk3*(4*TB - TB2*Yuk3) -
     &    MT2**2*(MHp2**2*NStbix(207)) - 8*MT2**4*NStbix(208) -
     &    MT2*(MHp2**3*NStbix(209)) + 2*MT2**3*(MHp2*NStbix(210))

        NStbix(215) = MHp2**3*(1 - 3*TB2*Yuk3**2) +
     &    4*MT2**3*NStbix(212) + MT2*(MHp2**2*NStbix(213)) -
     &    MT2**2*(MHp2*NStbix(214))

        NStbix(218) = MHp2**2*Yuk3*(12*TB + 5*TB2*Yuk3) -
     &    4*MT2**2*NStbix(216) - MT2*(MHp2*NStbix(217))

	NStbix(220) = MHp2**2 + 4*MT2**2 - 6*MT2*MHp2

	NStbix(222) = 9*MHp2*TB2*Yuk3**2 - 2*MT2*NStbix(221)

        NStbix(225) = 31*MHp2**2*TB2*Yuk3**2 -
     &    2*MT2*(MHp2*NStbix(223)) - 4*MT2**2*NStbix(224)

        NStbix(229) = 25*MHp2**3*TB2*Yuk3**2 -
     &    8*MT2**3*NStbix(226) - 4*MT2**2*(MHp2*NStbix(227)) -
     &    MT2*(MHp2**2*NStbix(228))

	NStbix(23) = 2*MA02**2 - 3*MA02*(2*MB2 + MHp2) + 10*MB2*MHp2

        NStbix(234) = MHp2**4*TB2*Yuk3**2 -
     &    2*MT2**3*(MHp2*NStbix(230)) + 4*MT2**4*NStbix(231) +
     &    MT2**2*(MHp2**2*NStbix(232)) - MT2*(MHp2**3*NStbix(233))

        NStbix(240) = 9*MHp2**4*TB2*Yuk3**2 +
     &    MT2**2*(MHp2**2*NStbix(235)) -
     &    MT2*(MHp2**3*NStbix(236)) + 4*NStbix(239)

	NStbix(246) = (2*MB2 - MHp2)*TB2*Yuk3**2 + 4*MT2*(TB*Yuk3)

	NStbix(249) = 21*MHp2*TB2*Yuk3**2 - MT2*NStbix(248)

        NStbix(252) = 35*MHp2**2*TB2*Yuk3**2 -
     &    MT2*(MHp2*NStbix(250)) - 3*MT2**2*NStbix(251)

        NStbix(255) = 29*MHp2**3*TB2*Yuk3**2 -
     &    MT2**2*(MHp2*(3 + 2*TB2*Yuk3**2)) - MT2**3*NStbix(253) -
     &    MT2*(MHp2**2*NStbix(254))

        NStbix(260) = 12*MHp2**4*TB2*Yuk3**2 +
     &    MT2**4*NStbix(256) + 2*MT2**2*(MHp2**2*NStbix(257)) +
     &    6*MT2**3*(MHp2*NStbix(258)) - MT2*(MHp2**3*NStbix(259))

        NStbix(266) = MT2**4*(MHp2*(2 + 3*TB2*Yuk3**2)) -
     &    3*MT2**5*NStbix(261) + 2*MT2*(MHp2**4*NStbix(262)) -
     &    MT2**2*(MHp2**3*NStbix(263)) - 2*NStbix(265)

        NStbix(273) = -(MB2*((MHp2 - MT2)*NStbix(272))) +
     &    (MHp2 - MT2)**3*NStbix(267)

        NStbix(279) = (MHp2 + 2*MT2)*NStbix(277) +
     &    2*TB*Yuk3*NStbix(278)

	NStbix(282) = 5 + 38*TB*Yuk3 - 4*TB2*Yuk3**2

	NStbix(284) = 7 + 24*TB*Yuk3 - 3*TB2*Yuk3**2

	NStbix(286) = -2 + 16*TB*Yuk3 + 7*TB2*Yuk3**2

	NStbix(287) = -1 + 32*TB*Yuk3 - TB2*Yuk3**2

        NStbix(289) = MT2 + 2*(3*MHp2 + 8*MT2)*TB*Yuk3 -
     &    (9*MHp2 + 7*MT2)*TB2*Yuk3**2

	NStbix(297) = -(MB2*NStbix(296)) + (MHp2 - MT2)**2*NStbix(291)

	NStbix(301) = 1 + 2*TB*Yuk3 - 4*TB2*Yuk3**2

	NStbix(303) = 15 + 32*TB*Yuk3 - 12*TB2*Yuk3**2

	NStbix(304) = 2 + 4*TB*Yuk3 - 3*TB2*Yuk3**2

	NStbix(306) = 3 + 6*TB*Yuk3 - 2*TB2*Yuk3**2

	NStbix(307) = -7 + 12*TB*Yuk3 + TB2*Yuk3**2

	NStbix(308) = 4 + 8*TB*Yuk3 - TB2*Yuk3**2

	NStbix(309) = 13 + 16*TB*Yuk3 + 2*TB2*Yuk3**2

	NStbix(311) = -1 + 24*TB*Yuk3 + 5*TB2*Yuk3**2

	NStbix(312) = 2 + 4*TB*Yuk3 + TB2*Yuk3**2

	NStbix(313) = 12 + 28*TB*Yuk3 + 3*TB2*Yuk3**2

	NStbix(315) = 21 + 40*TB*Yuk3 + 7*TB2*Yuk3**2

	NStbix(316) = 6 + 14*TB*Yuk3 + 9*TB2*Yuk3**2

	NStbix(321) = -19 + 104*TB*Yuk3 + 25*TB2*Yuk3**2

	NStbix(322) = -2 + 12*TB*Yuk3 - 3*TB2*Yuk3**2

	NStbix(324) = -50 + 176*TB*Yuk3 + 27*TB2*Yuk3**2

	NStbix(325) = 3 + 4*TB*Yuk3 - 6*TB2*Yuk3**2

	NStbix(326) = 1 + 9*TB*Yuk3 + 5*TB2*Yuk3**2

	NStbix(327) = -1 + 24*TB*Yuk3 - 18*TB2*Yuk3**2

        NStbix(332) = MHp2**3*(1 + 5*TB2*Yuk3**2) +
     &    MT2**3*NStbix(329) + MT2**2*(MHp2*NStbix(330)) -
     &    MT2*(MHp2**2*NStbix(331))

	NStbix(333) = -9 + 128*TB*Yuk3 + 38*TB2*Yuk3**2

	NStbix(334) = 1 + 56*TB*Yuk3 + 12*TB2*Yuk3**2

        NStbix(340) = MB2*((MB2 - MHp2 + MT2)*TB2*Yuk3**2) +
     &    MT2*NStbix(339)

	NStbix(342) = MA02 + 1/(MB2 - MT2)*((MA02 - MHp2)*MHp2)

	NStbix(348) = TB + 6*TB2*(2*Yuk3 + TB*Yuk3**2)

	NStbix(350) = 6*TB - TB2*(8*Yuk3 + 7*TB*Yuk3**2)

        NStbix(354) = 1/(MB2 - MT2)*
     &     ((MA02 - MB2 - MT2)*(MA02 + MB2 - MHp2 - MT2)) -
     &    4*MB2*(TB*Yuk3)

	NStbix(361) = MT2*(MHp2 - MT2) - MB2**2*(TB2*Yuk3**2)

	NStbix(363) = MHp2*TB*TB2*Yuk3**2 - MT2*NStbix(362)

        NStbix(366) = 2*MA02 - MHp2 -
     &    1/(MB2 - MT2)*((MA02 - MHp2)*MHp2)

        NStbix(368) = MT2*((MHp2 - MT2)**2*(2*MHp2 - MT2)) -
     &    MB2**4*(TB2*Yuk3**2)

	NStbix(370) = -4*MHp2*TB*TB2*Yuk3**2 + MT2*NStbix(369)

        NStbix(372) = 5*MHp2**2*TB*TB2*Yuk3**2 -
     &    4*MT2*(MHp2*NStbix(362)) - MT2**2*NStbix(371)

        NStbix(375) = -2*MHp2**2*TB*TB2*Yuk3**2 +
     &    MT2**2*NStbix(373) + MT2*(MHp2*NStbix(374))

	NStbix(377) = MT2*(MHp2 - MT2)**2 + MB2**3*(TB2*Yuk3**2)

	NStbix(379) = -6*MHp2*TB*TB2*Yuk3**2 + MT2*NStbix(378)

        NStbix(382) = 3*MHp2**2*TB*TB2*Yuk3**2 -
     &    2*MT2*(MHp2*NStbix(380)) + MT2**2*NStbix(381)

	NStbix(388) = 5*TB + TB2*(40*Yuk3 + 19*TB*Yuk3**2)

	NStbix(390) = TB + 2*TB2*(4*Yuk3 + 3*TB*Yuk3**2)

	NStbix(391) = 48*TB2*Yuk3 + 31*TB*(1 + TB2*Yuk3**2)

	NStbix(393) = 24*TB2*Yuk3 + 11*TB*(1 + TB2*Yuk3**2)

	NStbix(394) = 8*TB2*Yuk3 + TB*(6 + TB2*Yuk3**2)

	NStbix(395) = 40*TB2*Yuk3 + TB*(19 + 5*TB2*Yuk3**2)

	NStbix(399) = 5*TB + TB2*(24*Yuk3 + 5*TB*Yuk3**2)

	NStbix(407) = 37*MHp2*TB2*Yuk3**2 - MT2*NStbix(406)

	NStbix(409) = 26*MHp2*TB2*Yuk3**2 - MT2*NStbix(408)

        NStbix(413) = 8*MHp2**3*TB2*Yuk3**2 -
     &    2*MT2**2*(MHp2*(14 + TB2*Yuk3**2)) +
     &    MT2**3*NStbix(411) - MT2*(MHp2**2*NStbix(412))

        NStbix(417) = 3*MHp2**3*TB2*Yuk3**2 -
     &    MT2**3*NStbix(414) - MT2*(MHp2**2*NStbix(415)) +
     &    MT2**2*(MHp2*NStbix(416))

        NStbix(418) = MHp2**2*TB2*Yuk3**2 -
     &    2*MHp2*(MT2 + MT2*(TB2*Yuk3**2)) -
     &    MT2**2*(2 - TB2*Yuk3**2)

	NStbix(420) = MT2 - 2*(MHp2 + MT2)*TB2*Yuk3**2

        NStbix(423) = 23*MHp2**2*TB2*Yuk3**2 -
     &    MT2**2*NStbix(421) - MT2*(MHp2*NStbix(422))

        NStbix(425) = MHp2**2*TB2*Yuk3**2 -
     &    2*MHp2*(MT2 + 14*MT2*(TB2*Yuk3**2)) + MT2**2*NStbix(424)

	NStbix(429) = MB2 - MHH2 + MT2 + 4*MB2*(TB*Yuk3)

        NStbix(431) = 2*MB2**2 + MHH2*(MHH2 - MT2) -
     &    MB2*(3*MHH2 + 2*MT2)

        NStbix(433) = MHH2**2 + 2*MT2**2 - 3*MT2*MHH2 -
     &    MB2*NStbix(432)

	NStbix(437) = (MHH2 - 2*MHp2)*TB2*Yuk3**2 + 2*MT2*NStbix(436)

        NStbix(439) = 2*(MT2 + MB2*(TB*Yuk3))**2 -
     &    MHH2*(MT2 + MB2*(TB2*Yuk3**2))

	NStbix(447) = MB2*(TB2*Yuk3**2*NStbix(444)) - MT2*NStbix(446)

        NStbix(450) = 2*(MT2 + MB2*(TB*Yuk3))**2 -
     &    MHp2*(MT2 + MB2*(TB2*Yuk3**2))

        NStbix(451) = (3*MHH2 - 2*MHp2)*(MHp2 - MT2)**2 +
     &    MB2**2*MHH2 - 2*MB2*((2*MHH2 - MHp2)*MHp2)

        NStbix(453) = -(2*MT2**2*(2*MHH2 - MHp2)) -
     &    4*MT2*((MHH2 - MHp2)*(MB2 + MHp2 - MT2)*TB*Yuk3) +
     &    TB2*Yuk3**2*NStbix(452)

        NStbix(456) = (3*MHH2 - 8*MHp2)*TB2*Yuk3**2 +
     &    2*MT2*NStbix(455)

        NStbix(458) = 5*MHp2**2*TB2*Yuk3**2 -
     &    4*MT2*(MHp2*NStbix(436)) - MT2**2*NStbix(457)

	NStbix(460) = 6*MHp2*TB2*Yuk3**2 - MT2*NStbix(459)

	NStbix(467) = -2*(MHp2 - MT2)*NStbix(463) + MHH2*NStbix(466)

        NStbix(471) = MHp2**2*(1 - 4*TB*Yuk3) -
     &    2*MT2*(MHp2*(1 - 8*TB*Yuk3)) - MT2**2*(7 + 12*TB*Yuk3)

	NStbix(474) = -1 + 4*TB*Yuk3 + 2*TB2*Yuk3**2

	NStbix(476) = -2 + 8*TB*Yuk3 + TB2*Yuk3**2

	NStbix(478) = 6 + 8*TB*Yuk3 - 7*TB2*Yuk3**2

	NStbix(49) = -2*MT2*(3*MB2 + MT2) + (MB2 + MT2)*MA02

        NStbix(492) = MT2*NStbix(484) - MB2**2*NStbix(487) -
     &    MB2*NStbix(491)

	NStbix(545) = Re(NStbil(8))/MA02 - Re(NStbil(15))

        NStbix(546) = MA02*NStbix(545) + Re(NStbil(5)) -
     &    (MHH2 - 4*MT2)*Re(NStbil(16))

        NStbix(553) = -Re(NStbil(5)) + 2*Re(NStbil(7)) -
     &    Re(NStbil(8)) + MA02*Re(NStbil(11)) -
     &    (4*MB2 - MHH2)*Re(NStbil(12))

	NStbix(556) = Re(NStbil(8))/MA02 - Re(NStbil(11))

	NStbix(64) = -MT2**3 + MB2**3*(TB2*Yuk3**2)

	NStbix(65) = 5*TB - 2*TB2*(2*Yuk3 - 31*TB*Yuk3**2)

	NStbix(66) = 62*TB - TB2*(4*Yuk3 - 5*TB*Yuk3**2)

	NStbix(68) = 3*TB + 2*TB2*(2*Yuk3 + TB*Yuk3**2)

	NStbix(69) = 2*TB + TB2*(4*Yuk3 + 3*TB*Yuk3**2)

	NStbix(71) = 35*TB + TB2*(4*Yuk3 + 3*TB*Yuk3**2)

	NStbix(72) = 3*TB + TB2*(4*Yuk3 + 35*TB*Yuk3**2)

        NStbix(73) = -9*MT2**4 + 9*MB2**4*(TB2*Yuk3**2) -
     &    7*MB2**2*MT2**2*(1 - TB2*Yuk3**2)

	NStbix(78) = 4*TB2*Yuk3 + TB*(29 - TB2*Yuk3**2)

	NStbix(83) = MHH2 - 2*MT2*(3 + 1/MT2*NStbil(6))

        NStbix(86) = MA02**2 - MA02*(MHH2 + 6*MT2) +
     &    2*MT2*(MHH2 + 8*MT2)

	NStbix(87) = MA02*MHH2 - MHH2**2 - 2*MT2*(3*MA02 - MHH2)

	NStbix(90) = 3*MHH2**2 + 48*MT2**2 + 28*MT2*MHH2

	NStbix(91) = 3*MHH2**2 + 32*MT2**2 + 28*MT2*MHH2

        NStbix(92) = MA02**2 - MA02*(MHH2 + 8*MT2) +
     &    4*MT2*(MHH2 + 2*MT2)

        NStbix(94) = MA02**2 - MA02*(MHH2 + 12*MT2) +
     &    8*MT2*(MHH2 + 2*MT2)

        NStbix(10) = MA02**2 - MA02*(12*MB2 + MHH2) +
     &    8*MB2*(2*MB2 + MHH2)

	NStbix(103) = TB - TB2*(4*Yuk3 + 29*TB*Yuk3**2)

	NStbix(106) = -4*(3*MB2 - MT2)*MT2 + (MB2 + MT2)*MA02

	NStbix(109) = (3*MB2 - MT2)*MT2*TB - MB2*(TB2*NStbix(108))

	NStbix(125) = 4*(MHH2 - 3*MHp2)*TB - (MHH2 + 7*MHp2)*TB2*Yuk3

        NStbix(129) = MHp2*NStbix(127) -
     &    MHH2*(MT2 + 2*(MHp2 + MT2)*NStbix(128))

	NStbix(13) = 88*MB2 - MHH2*(19 - (6*NStbil(5))/MHH2)

        NStbix(132) = MHp2**2*Yuk3*(4*TB - TB2*Yuk3) +
     &    MT2**2*NStbix(130) - 2*MT2*(MHp2*NStbix(131))

        NStbix(134) = MHp2**2*Yuk3*(4*TB - TB2*Yuk3) +
     &    2*MHp2*(MT2 + MT2*(TB2*Yuk3**2)) - MT2**2*NStbix(133)

	NStbix(139) = 2*MHp2*TB*TB2*Yuk3**2 + MT2*NStbix(138)

        NStbix(142) = MHp2**2*TB2*Yuk3*(4 + TB*Yuk3) -
     &    2*MT2*(MHp2*NStbix(140)) + MT2**2*NStbix(141)

        NStbix(147) = MHp2*(MHp2 - 4*MT2)*NStbil(25) +
     &    NStbil(23)*NStbix(146)

        NStbix(148) = 2*MHH2**2 - MHH2*(MHp2 + 10*MT2) +
     &    4*(MT2*MHp2 + (2*MHH2 - 3*MHp2)*NStbil(6))

        NStbix(149) = 6*MHH2**2 + 3*(MHH2*MHp2 + MHp2**2) -
     &    2*MT2*(7*MHH2 + 5*MHp2) - 32*(MT2**2 + MT2*NStbil(6)) -
     &    16*NStbil(6)**2

	NStbix(15) = 3*MA02 - 2*MB2*(5 - 1/MB2*NStbil(7))

	NStbix(150) = MHp2 - 2*MT2*(3 + 2/MT2*NStbil(6))

        NStbix(153) = MHp2**2*(MA02 - 4*MT2)*NStbil(25) +
     &    MA02*NStbil(21)*NStbix(152)

        NStbix(156) = 6*MA02**3 + MA02**2*(3*MHp2 - 2*MT2) -
     &    4*MT2*(MHp2*(3*MHp2 + 2*MT2)) + MA02*NStbix(155)

	NStbix(157) = 2*MA02**2 + 8*MT2**2 - MA02*(MHp2 + 6*MT2)

	NStbix(158) = 2*MA02**2 - MA02*(3*MHp2 + 4*MT2) + 8*MT2*MHp2

	NStbix(159) = MHp2 - 2*MT2*(1 + 2/MT2*NStbil(6))

	NStbix(162) = 6*MHp2**2 - 3*MT2**2 - 19*MT2*MHp2

	NStbix(164) = 10*MHp2*TB2*Yuk3**2 - MT2*NStbix(163)

        NStbix(169) = 6*MHp2**4*TB2*Yuk3**2 -
     &    MT2**3*(MHp2*NStbix(165)) + MT2**4*NStbix(166) -
     &    3*MT2*(MHp2**3*NStbix(167)) +
     &    MT2**2*(MHp2**2*NStbix(168))

        NStbix(174) = 37*MHp2**4*TB2*Yuk3**2 -
     &    MT2**4*NStbix(170) + 2*MT2**2*(MHp2**2*NStbix(171)) -
     &    8*MT2*(MHp2**3*NStbix(172)) + 4*MT2**3*(MHp2*NStbix(173))

        NStbix(178) = 36*MHp2**3*TB2*Yuk3**2 -
     &    MT2*(MHp2**2*NStbix(175)) - MT2**3*NStbix(176) -
     &    2*MT2**2*(MHp2*NStbix(177))

        NStbix(18) = 8*MB2**2 + 3*MHp2**2 - 10*MB2*MHp2 -
     &    16*NStbil(7)**2

        NStbix(181) = 54*MHp2**2*TB2*Yuk3**2 -
     &    MT2**2*NStbix(179) - 2*MT2*(MHp2*NStbix(180))

        NStbix(185) = 96*MHp2**2*TB*Yuk3 +
     &    MT2**2*NStbix(183) + 2*MT2*(MHp2*NStbix(184))

        NStbix(187) = 2*MHp2*Yuk3*(32*TB - 5*TB2*Yuk3) +
     &    MT2*NStbix(186)

        NStbix(191) = MHp2**3*(5 + TB2*Yuk3**2) +
     &    MT2*(MHp2**2*NStbix(188)) + MT2**3*NStbix(189) -
     &    MT2**2*(MHp2*NStbix(190))

        NStbix(196) = MHp2**4*Yuk3*(16*TB + 5*TB2*Yuk3) +
     &    MT2**4*NStbix(192) + 16*MT2**2*(MHp2**2*NStbix(193)) -
     &    12*MT2**3*(MHp2*NStbix(194)) -
     &    2*MT2*(MHp2**3*NStbix(195))

        NStbix(200) = MT2**3*NStbix(197) -
     &    6*MT2**2*(MHp2*NStbix(198)) + Yuk3*NStbix(199)

        NStbix(219) = MT2**2*
     &     ((3*MHp2 - 4*MT2)*(MHp2 - MT2)**3) -
     &    MB2**5*(Yuk3*NStbix(202)) - MB2**3*NStbix(206) +
     &    MB2**2*NStbix(211) -
     &    MB2*MT2*((MHp2 - MT2)*NStbix(215)) + MB2**4*NStbix(218)

        NStbix(24) = (MA02 - 4*MB2)*MHp2**2*NStbil(18) +
     &    MA02*NStbil(17)*NStbix(23)

        NStbix(241) = -(4*MB2**6*(TB2*Yuk3**2)) +
     &    MT2*((MHp2 - MT2)**3*NStbix(220)) +
     &    2*MB2**5*NStbix(222) - MB2**4*NStbix(225) +
     &    MB2**3*NStbix(229) + MB2*((MHp2 - MT2)*NStbix(234)) -
     &    MB2**2*NStbix(240)

	NStbix(247) = MT2**2*(2*MB2 - MHp2) + MB2**2*NStbix(246)

        NStbix(274) = 5*MB2**7*(TB2*Yuk3**2) -
     &    MB2**6*NStbix(249) + MB2**5*NStbix(252) -
     &    MB2**4*NStbix(255) + MB2**3*NStbix(260) +
     &    MB2**2*NStbix(266) - MT2**2*NStbix(273)

        NStbix(280) = -((MHp2 + MT2)**2*(MHp2**2 - 2*MT2**2)*
     &       TB2*Yuk3**2) + MT2*NStbix(279)

	NStbix(281) = 1 + 23*TB*Yuk3 - 4*TB2*Yuk3**2

        NStbix(283) = MHp2**2*Yuk3*(6*TB - 13*TB2*Yuk3) +
     &    MT2*(MHp2*NStbix(282))

        NStbix(285) = MHp2**3*Yuk3*(2*TB - 7*TB2*Yuk3) +
     &    MT2*(MHp2**2*NStbix(284))

	NStbix(288) = MT2**2*(MHp2*NStbix(286)) + MT2**3*NStbix(287)

	NStbix(298) = MB2**5*NStbix(289) + MT2**2*NStbix(297)

        NStbix(3) = MA02*NStbil(17)*NStbix(1) +
     &    (MA02 - 4*MB2)*NStbil(19)*NStbix(2)

	NStbix(300) = MHp2*(1 - 4*TB*Yuk3) + MT2*(3 + 4*TB*Yuk3)

	NStbix(302) = 15*MHp2*TB2*Yuk3**2 - 4*MT2*NStbix(301)

        NStbix(305) = 21*MHp2**2*TB2*Yuk3**2 -
     &    MT2*(MHp2*NStbix(303)) - 8*MT2**2*NStbix(304)

        NStbix(31) = 2*MB2*(MHH2 - MHp2) -
     &    (2*MHH2 - 3*MHp2)*NStbil(5) - MHp2*NStbil(9)

        NStbix(310) = 3*MHp2**4*TB2*Yuk3**2 +
     &    6*MT2**3*(MHp2*NStbix(306)) -
     &    MT2**2*(MHp2**2*NStbix(307)) - 4*MT2**4*NStbix(308) -
     &    MT2*(MHp2**3*NStbix(309))

        NStbix(314) = MHp2**4*(3 - TB2*Yuk3**2) +
     &    4*MT2**4*(1 + 2*TB*Yuk3) +
     &    MT2**2*(MHp2**2*NStbix(311)) -
     &    MT2*(MHp2**3*NStbix(312)) - MT2**3*(MHp2*NStbix(313))

        NStbix(317) = 13*MHp2**3*TB2*Yuk3**2 -
     &    8*MT2**3*NStbix(306) - MT2*(MHp2**2*NStbix(315)) -
     &    2*MT2**2*(MHp2*NStbix(316))

        NStbix(319) = MT2 - (2*MHp2 - 9*MT2)*TB2*Yuk3**2 +
     &    32*MT2*(TB*Yuk3)

	NStbix(320) = MHp2*(5 + 16*TB*Yuk3) + MT2*(5 - 16*TB*Yuk3)

        NStbix(323) = MHp2**3*TB2*Yuk3**2 +
     &    8*MT2*(MHp2**2*Yuk3*(8*TB - TB2*Yuk3)) +
     &    MT2**3*NStbix(321) + 6*MT2**2*(MHp2*NStbix(322))

        NStbix(328) = MHp2**4*TB2*Yuk3**2 +
     &    MT2**4*NStbix(324) + 12*MT2**3*(MHp2*NStbix(325)) +
     &    16*MT2**2*(MHp2**2*NStbix(326)) +
     &    2*MT2*(MHp2**3*NStbix(327))

        NStbix(335) = -(MB2*((MHp2 - MT2)*NStbix(332))) -
     &    MB2**4*(MT2*NStbix(333) + 2*MHp2*NStbix(334))

        NStbix(347) = MHp2**2*(TB + 4*TB2*Yuk3) -
     &    2*MT2*(MHp2*TB) - MT2**2*(7*TB + 4*TB2*Yuk3)

	NStbix(349) = -2*MHp2*TB*TB2*Yuk3**2 + MT2*NStbix(348)

        NStbix(351) = MHp2**2*TB*TB2*Yuk3**2 -
     &    2*MT2*(MHp2*NStbix(140)) + MT2**2*NStbix(350)

        NStbix(355) = MB2/(MB2 - MT2)*
     &     ((MA02 - MB2 - MT2)*(MA02 - MB2 - MHp2 + MT2)*TB2*
     &       Yuk3**2) - MT2*NStbix(354)

        NStbix(356) = 4*MB2*MT2*(TB*Yuk3) +
     &    (MB2 + MT2)*(MT2 + MB2*(TB2*Yuk3**2))

        NStbix(360) = (MA02 - MB2 - MHp2 + MT2)*NStbil(6) -
     &    (MA02 + MB2 - MHp2 - MT2)*NStbil(7)

        NStbix(364) = MA02*TB*(MT2 + MB2*(TB2*Yuk3**2)) -
     &    2*(MB2*NStbix(363) + TB*NStbix(361))

        NStbix(376) = -(MB2**3*NStbix(370)) -
     &    MB2**2*NStbix(372) - MB2*((MHp2 - MT2)*NStbix(375)) +
     &    TB*NStbix(368)

        NStbix(383) = MB2**2*NStbix(379) + MB2*NStbix(382) +
     &    3*TB*NStbix(377)

        NStbix(387) = MT2*((MHp2 - MT2)**2*(8*MHp2 + 9*MT2)) +
     &    9*MB2**4*(TB2*Yuk3**2)

	NStbix(389) = -10*MHp2*TB*TB2*Yuk3**2 + MT2*NStbix(388)

        NStbix(392) = 7*MHp2**2*TB*TB2*Yuk3**2 +
     &    2*MT2*(MHp2*NStbix(390)) - MT2**2*NStbix(391)

        NStbix(396) = 8*MHp2**3*TB*TB2*Yuk3**2 -
     &    MT2*(MHp2**2*NStbix(393)) -
     &    2*MT2**2*(MHp2*NStbix(394)) + MT2**3*NStbix(395)

	NStbix(398) = -(MT2*(MHp2 - 3*MT2)) + 3*MB2**2*(TB2*Yuk3**2)

	NStbix(4) = MA02*(4*MB2 + MHH2) - MHH2*(8*MB2 + MHH2)

	NStbix(400) = -3*MHp2*TB*TB2*Yuk3**2 + MT2*NStbix(399)

        NStbix(405) = MHH2**2 + 3*MHH2*MHp2 + 8*MHp2**2 -
     &    17*MT2*(MHH2 - MHp2)

        NStbix(410) = MHH2**2*TB2*Yuk3**2 + MHH2*NStbix(407) -
     &    MHp2*NStbix(409)

        NStbix(419) = MHp2*NStbix(413) + MHH2*NStbix(417) +
     &    MHH2**2*NStbix(418)

	NStbix(42) = (MB2 + MT2)*TB + 4*MB2*(TB2*Yuk3)

        NStbix(426) = MHH2**2*NStbix(420) - MHH2*NStbix(423) +
     &    MHp2*NStbix(425)

        NStbix(430) = MB2*((MB2 - MHH2 + MT2)*TB2*Yuk3**2) +
     &    MT2*NStbix(429)

	NStbix(434) = MB2*(TB2*Yuk3**2*NStbix(431)) - MT2*NStbix(433)

        NStbix(438) = MT2*(MHH2 - 2*MHp2 + 2*MT2) +
     &    2*MB2**2*(TB2*Yuk3**2) + MB2*NStbix(437)

        NStbix(44) = 4*MT2**2 + MB2**2*(TB*Yuk3) -
     &    MB2*MT2*(4 + 3*TB*Yuk3)

        NStbix(440) = MHH2*(MT2 + MB2*(TB2*Yuk3**2)) +
     &    1/(MB2 - MT2)*((MHH2 - MHp2)*NStbix(439))

	NStbix(442) = MHp2**2 - MHH2*(2*MHp2 - MT2)

        NStbix(443) = MHH2*(MT2 + 4*MT2*(TB*Yuk3)) +
     &    TB2*Yuk3**2*NStbix(442)

        NStbix(468) = MT2*
     &     ((MHp2 - MT2)**2*(3*MHH2 - 4*MHp2 + 2*MT2)) +
     &    2*MB2**4*(TB2*Yuk3**2) + MB2**3*NStbix(456) +
     &    MB2**2*(2*NStbix(458) - MHH2*NStbix(460)) +
     &    MB2*NStbix(467)

	NStbix(47) = 4*MT2 + (MB2 + 3*MT2)*(TB*Yuk3)

        NStbix(472) = MHH2*(MHp2 - MT2)**2*(1 - 4*TB*Yuk3) -
     &    MHp2*NStbix(471)

        NStbix(473) = MT2 - 2*(MHp2 - 3*MT2)*TB2*Yuk3**2 +
     &    4*MT2*(TB*Yuk3)

	NStbix(475) = 2*MHp2*TB2*Yuk3**2 + MT2*NStbix(474)

        NStbix(477) = MHp2**2*TB2*Yuk3**2 +
     &    2*MT2*(MHp2*NStbix(131)) + MT2**2*NStbix(476)

        NStbix(479) = MHp2**2*TB2*Yuk3**2 -
     &    2*MHp2*(MT2 + MT2*(TB2*Yuk3**2)) + MT2**2*NStbix(478)

        NStbix(482) = MHH2*(MT2 + MB2*(TB2*Yuk3**2)) -
     &    1/(MB2 - MT2)*((MHH2 - MHp2)*NStbix(439))

	NStbix(499) = MT2*TB + MB2*(TB2*Yuk3*(2 + TB*Yuk3))

	NStbix(50) = 2*MB2*((MA02 - 4*MT2)*TB2*Yuk3) + TB*NStbix(49)

	NStbix(500) = (MB2 - MHp2 + MT2)*TB + 4*MB2*(TB2*Yuk3)

	NStbix(521) = MA02 - MHH2 - 2*(MA02 - 2*MHH2)*Re(NStbil(1))

        NStbix(522) = MA02 + 4*MB2 - MHH2 -
     &    2*(MA02 - 2*MHH2)*Re(NStbil(2)) +
     &    2*(4*MB2 - MHH2)*Re(NStbil(3))

	NStbix(533) = MB2**2 - MB2*(MHp2 - MT2)

        NStbix(539) = MT2*Re(NStbil(1)) +
     &    MB2*(TB2*Yuk3**2*Re(NStbil(2)))

        NStbix(540) = (MA02 - MHp2 + 2*MT2)*TB2*Yuk3**2 +
     &    2/MB2*((MT2 - MB2*(TB2*Yuk3**2))*Re(NStbil(7)))

	NStbix(544) = 3*MT2*TB + MB2*(TB2*Yuk3*(4 + TB*Yuk3))

        NStbix(547) = MT2*(NStbix(546) - 2*Re(NStbil(6))) -
     &    (MT2 + MB2*(TB2*Yuk3**2))*(Re(NStbil(7)) - Re(NStbil(9)))

        NStbix(549) = -(MA02*NStbix(545)) +
     &    (MHH2 - 4*MT2)*Re(NStbil(16))

        NStbix(554) = MB2*(TB2*Yuk3**2*NStbix(553)) +
     &    (MT2 + MB2*(TB2*Yuk3**2))*(Re(NStbil(6)) - Re(NStbil(9)))

        NStbix(557) = MA02*NStbix(556) + Re(NStbil(5)) -
     &    2*Re(NStbil(7)) + (4*MB2 - MHH2)*Re(NStbil(12))

	NStbix(56) = 22*TB + TB2*(20*Yuk3 - 3*TB*Yuk3**2)

        NStbix(560) = 4*MB2*(TB2*Yuk3) +
     &    1/MT2*((MB2 - MHp2 + MT2)*TB*(MT2 + MB2*(TB2*Yuk3**2)))

        NStbix(561) = -(1/MT2*
     &       ((MT2 + MB2*(TB2*Yuk3**2))*
     &         (Re(NStbil(7)) - Re(NStbil(9))))) +
     &    MA02*NStbix(545) + Re(NStbil(5)) - 2*Re(NStbil(6)) -
     &    (MHH2 - 4*MT2)*Re(NStbil(16))

	NStbix(563) = MB2**2*MT2 - MT2**3

	NStbix(564) = 3*MB2**2 - 4*MB2*MT2 + MT2**2

	NStbix(58) = 3*TB - 2*TB2*(10*Yuk3 + 11*TB*Yuk3**2)

	NStbix(6) = 48*MB2**2 + 3*MHH2**2 + 28*MB2*MHH2

	NStbix(61) = 4*MB2*(MB2 - 3*MT2) + (MB2 + MT2)*MA02

        NStbix(63) = -9*MT2**2 + 9*MB2**2*(TB2*Yuk3**2) +
     &    MB2*(MT2 - MT2*(TB2*Yuk3**2))

        NStbix(67) = MB2**2*MT2*NStbix(65) -
     &    MB2*MT2**2*NStbix(66) - 33*TB*NStbix(64)

	NStbix(7) = 32*MB2**2 + 3*MHH2**2 + 28*MB2*MHH2

        NStbix(70) = MB2**2*MT2*NStbix(68) -
     &    MB2*MT2**2*NStbix(69) + 9*TB*NStbix(64)

        NStbix(74) = MB2*MT2**3*NStbix(71) -
     &    MB2**3*MT2*NStbix(72) - TB*NStbix(73)

	NStbix(77) = (MB2 + MT2)*TB + 2*MB2*(TB2*Yuk3)

        NStbix(79) = 23*MT2**2*TB +
     &    MB2**2*(TB2*Yuk3*(4 + 5*TB*Yuk3)) - MB2*MT2*NStbix(78)

        NStbix(8) = MA02**2 - MA02*(8*MB2 + MHH2) +
     &    4*MB2*(2*MB2 + MHH2) + 3*(MA02 - 4*MB2)*NStbil(5)

	NStbix(84) = -2*NStbil(23) + (3*NStbil(5)*NStbix(83))/MHH2

        NStbix(88) = MA02*NStbil(21)*NStbix(86) -
     &    (MA02 - 4*MT2)*NStbil(23)*NStbix(87)

        NStbix(89) = MA02*(MHH2 + 4*MT2) -
     &    MHH2*(MHH2 + 8*MT2) - 12*(MA02 - MHH2)*NStbil(6)

        NStbix(93) = 3*MA02**3 +
     &    MT2*(64*NStbil(6)**2 - 4*(8*MA02**2 - NStbix(91))) -
     &    MA02*NStbix(90) + 16*NStbil(6)*NStbix(92)

        NStbix(95) = -4*(MA02 - MHH2)*(3*MA02 - 8*MT2)*
     &     NStbil(6) + MA02*NStbix(94)

        NStbix(100) = -5*MT2**2 + 5*MB2**2*(TB2*Yuk3**2) +
     &    MB2*(MT2 - MT2*(TB2*Yuk3**2))

        NStbix(104) = 23*MB2**2*(TB*TB2*Yuk3**2) +
     &    MT2**2*(5*TB + 4*TB2*Yuk3) + MB2*MT2*NStbix(103)

	NStbix(107) = 2*MB2*((MA02 - 4*MT2)*TB2*Yuk3) + TB*NStbix(106)

        NStbix(11) = -4*(3*MA02 - 8*MB2)*(MA02 - MHH2)*
     &     NStbil(7) + MA02*NStbix(10)

        NStbix(110) = -((32*(MB2 - MT2)*MT2**2)/
     &       (MA02 - 4*MT2)) + NStbix(109)/TB

        NStbix(112) = MB2*(MB2 + MT2)*
     &     ((MA02 - 4*MT2)*TB2*Yuk3**2) - MT2*NStbix(106)

        NStbix(120) = -((4*MB2 - MHp2)*MHp2*NStbil(18)) +
     &    (4*MB2 - MHH2)*MHH2*NStbil(19)

        NStbix(135) = MT2*((MHH2 - MHp2)*(MHp2 - MT2)**2) -
     &    MB2**3*(Yuk3*NStbix(125)) - MB2**2*NStbix(129) -
     &    MB2*(MHH2*NStbix(132) - MHp2*NStbix(134))

	NStbix(137) = MT2*TB + MB2*(TB2*Yuk3*(4 + TB*Yuk3))

        NStbix(14) = 28*MB2**2 - 5*MHH2**2 -
     &    4*MB2*(-(1/MB2*(NStbil(7)*NStbix(13))) +
     &       14*NStbil(19)) + 12*(6*MB2 - MHH2)*NStbil(5) -
     &    24*NStbil(7)**2 + MHH2*(49*MB2 + 8*NStbil(19))

        NStbix(143) = MT2*((MHp2 - MT2)**2*TB) -
     &    MB2**3*(TB2*Yuk3*(4 + 7*TB*Yuk3)) - MB2**2*NStbix(139) +
     &    MB2*NStbix(142)

        NStbix(151) = 8*NStbix(147) -
     &    4*NStbil(5)*NStbix(148) - (MHH2 - MHp2)*NStbix(149) +
     &    4*MHp2*NStbil(9)*NStbix(150)

        NStbix(16) = 5*MA02**2 - 28*MB2**2 +
     &    MB2*(-MA02 + 24*NStbil(17)) +
     &    4*(3*MA02 - 8*MB2)*NStbil(7) - 8*NStbil(7)**2 -
     &    4*MA02*(2*NStbil(17) - (NStbil(8)*NStbix(15))/MA02)

        NStbix(160) = 8*NStbix(153) -
     &    (MA02 - MHp2)*NStbix(156) -
     &    4*NStbil(8)*(MA02*NStbix(157) +
     &       4*NStbil(6)*NStbix(158)) +
     &    4*MHp2*(MA02 - 4*MT2)*NStbil(9)*NStbix(159)

        NStbix(182) = 3*MB2**6*(TB2*Yuk3**2) +
     &    MT2*((MHp2 - MT2)**3*NStbix(162)) + MB2**5*NStbix(164) +
     &    MB2*((MHp2 - MT2)*NStbix(169)) - MB2**2*NStbix(174) +
     &    2*MB2**3*NStbix(178) - MB2**4*NStbix(181)

        NStbix(19) = 6*MA02**3 - MA02**2*(2*MB2 - 3*MHp2) -
     &    4*MB2*(MHp2*(2*MB2 + 3*MHp2)) + MA02*NStbix(18)

	NStbix(20) = 2*MA02**2 + 8*MB2**2 - MA02*(6*MB2 + MHp2)

        NStbix(201) = MT2**2*((MHp2 - MT2)**3*(MHp2 + MT2)) +
     &    MB2**6*(Yuk3*(16*TB - 5*TB2*Yuk3)) +
     &    MB2**4*NStbix(185) - MB2**5*NStbix(187) +
     &    MB2*MT2*((MHp2 - MT2)*NStbix(191)) +
     &    MB2**2*NStbix(196) - 2*MB2**3*NStbix(200)

	NStbix(21) = 2*MA02**2 - MA02*(4*MB2 + 3*MHp2) + 8*MB2*MHp2

        NStbix(22) = 2*MB2*(MA02*(MA02 - MHp2)) +
     &    (MA02 - 4*MB2)*MHp2*NStbil(9)

        NStbix(242) = 1/(MB2 - MT2)*(NStbil(6)*NStbix(219)) -
     &    NStbix(241)

	NStbix(244) = MT2**2 + MB2**2*(TB2*Yuk3**2)

        NStbix(25) = (MA02 - 4*MB2)*(2*MB2 - MHp2)*MHp2*
     &     NStbil(9) - 2*NStbix(24)

	NStbix(27) = 32*MB2**2 - 3*MHH2**2 + 4*MB2*MHH2

	NStbix(28) = 2*MHH2**2 + 3*(2*MB2 - MHH2)*MHp2

	NStbix(29) = -2*MHH2**2 - (4*MB2 - MHH2)*MHp2 + 10*MB2*MHH2

        NStbix(299) = 2*MB2**6*(Yuk3*(TB - TB2*Yuk3)) +
     &    MB2**2*NStbix(280) +
     &    MB2**4*(2*MT2**2*NStbix(281) + NStbix(283)) -
     &    MB2**3*(NStbix(285) + 2*NStbix(288)) - NStbix(298)

	NStbix(30) = -MHp2**3 + 6*MB2*MHp2**2

        NStbix(318) = 4*MB2**6*(TB2*Yuk3**2) -
     &    MT2**2*(MHp2*(MHp2 - MT2)**2*NStbix(300)) -
     &    MB2**5*NStbix(302) + MB2**4*NStbix(305) +
     &    MB2**2*NStbix(310) + MB2*MT2*NStbix(314) -
     &    MB2**3*NStbix(317)

	NStbix(32) = 1/MB2*(NStbil(7)*NStbix(31)) - MHH2*NStbil(19)

        NStbix(336) = MB2**6*(TB2*Yuk3**2) +
     &    MB2**5*NStbix(319) -
     &    MT2**2*((MHp2 - MT2)**3*NStbix(320)) +
     &    2*MB2**3*NStbix(323) - MB2**2*NStbix(328) +
     &    MT2*NStbix(335)

	NStbix(346) = MB2*(TB*TB2*Yuk3**2) + MT2*(TB + 4*TB2*Yuk3)

        NStbix(352) = MB2**3*(TB*TB2*Yuk3**2) +
     &    MT2*NStbix(347) + MB2**2*NStbix(349) + MB2*NStbix(351)

        NStbix(365) = -(2/(MB2 - MT2)*
     &       ((MT2 + MB2*(TB2*Yuk3**2))*NStbix(360))) +
     &    NStbix(364)/TB

	NStbix(37) = MB2*(MHH2 - 10*MT2) + MT2*(MHH2 + 2*MT2)

        NStbix(386) = MA02*(MA02 - 2*(MT2 + MB2*(TB*Yuk3)))*
     &     NStbil(21) - MHp2*(MHp2 - 2*(MT2 + MB2*(TB*Yuk3)))*
     &     NStbil(25)

	NStbix(39) = MHH2*TB*Yuk3 - 2*MT2*(4 + 5*TB*Yuk3)

        NStbix(397) = MB2**3*NStbix(389) -
     &    MB2**2*NStbix(392) + MB2*NStbix(396) + TB*NStbix(387)

        NStbix(401) = MA02**2*TB*(MT2 + MB2*(TB2*Yuk3**2)) -
     &    MA02*(MB2*NStbix(400) + 3*TB*NStbix(398))

        NStbix(404) = MHH2*(MHH2 - 4*MT2)*NStbil(23) -
     &    MHp2*(MHp2 - 4*MT2)*NStbil(25)

        NStbix(427) = -(17*MB2**4*
     &       ((MHH2 - MHp2)*TB2*Yuk3**2)) +
     &    MT2*((MHp2 - MT2)**2*NStbix(405)) + MB2**3*NStbix(410) +
     &    MB2*NStbix(419) + MB2**2*NStbix(426)

	NStbix(43) = MB2*(MB2 + MT2)*(TB*TB2*Yuk3**2) + MT2*NStbix(42)

        NStbix(435) = 1/(MB2 - MT2)*
     &     ((MHH2 - MHp2)*NStbix(434)) + MHH2*NStbix(430)

        NStbix(441) = MHH2**2*NStbix(438) +
     &    2*MHH2*NStbil(6)*NStbix(440)

        NStbix(45) = ((MB2 - 3*MT2)*MT2)/TB2 +
     &    32*MB2**2*(MB2 - MT2)*Yuk3**2/(MA02 - 4*MB2) -
     &    (Yuk3*NStbix(44))/TB

        NStbix(48) = MA02*(2*MT2 + (MB2 + MT2)*(TB*Yuk3)) -
     &    2*MB2*NStbix(47)

        NStbix(480) = MB2**3*((MHH2 - MHp2)*TB2*Yuk3**2) +
     &    MT2*NStbix(472) -
     &    MB2**2*(MHp2*NStbix(473) + MHH2*NStbix(475)) +
     &    MB2*(MHH2*NStbix(477) - MHp2*NStbix(479))

	NStbix(498) = MA02 - 2*(MT2 + (MA02 - 2*MHp2)*Re(NStbil(1)))

	NStbix(5) = -2*NStbix(3) + (MA02 - 4*MB2)*NStbil(5)*NStbix(4)

        NStbix(501) = MB2*
     &     ((MB2 - MHp2 + MT2)*TB*TB2*Yuk3**2) + MT2*NStbix(500)

        NStbix(502) = (2*MB2 - MHp2)*TB*
     &     (MT2 + MB2*(TB2*Yuk3**2)) +
     &    2*(MT2*NStbix(499) + NStbix(501)*Re(NStbil(10)))

        NStbix(507) = (2*MB2 - MHp2)*TB*
     &     (MT2 + MB2*(TB2*Yuk3**2)) + 2*MT2*NStbix(499)

	NStbix(508) = MB2 - MA02*Re(NStbil(2)) + 2*Re(NStbil(7))

	NStbix(513) = (4*MB2 - MHH2)*Re(NStbil(3)) + 2*Re(NStbil(7))

	NStbix(516) = MHH2 - 6*MT2 + 2*(MHH2 - 4*MT2)*Re(NStbil(4))

        NStbix(519) = MA02 + 4*MB2 - MHH2 +
     &    2*(MA02*Re(NStbil(2)) + (4*MB2 - MHH2)*Re(NStbil(3)))

        NStbix(523) = 4*MT2**2 +
     &    MB2*(TB2*Yuk3**2*NStbix(522)) +
     &    MT2*(NStbix(521) - 2*(MHH2 - 4*MT2)*Re(NStbil(4)))

        NStbix(524) = MT2*Re(NStbil(6)) +
     &    MB2*(TB2*Yuk3**2*Re(NStbil(7)))

	dup1 = -(MA02*Re(NStbil(2))) + 2*Re(NStbil(7))

        NStbix(526) = MB2*
     &     ((-2*dup1 + MA02 + 4*MB2 - MHH2)*TB2*Yuk3**2) +
     &    MT2*(MA02 - MHH2 + 4*MT2 + 2*MA02*Re(NStbil(1)) -
     &       4*Re(NStbil(6)))

        NStbix(527) = MB2*(TB2*Yuk3**2*NStbix(513)) +
     &    MT2*(-((MHH2 - 4*MT2)*Re(NStbil(4))) + 2*Re(NStbil(6)))

        NStbix(530) = NStbix(507) -
     &    TB*(MT2 + MB2*(TB2*Yuk3**2))*
     &     (Re(NStbil(6)) + Re(NStbil(7))) -
     &    NStbix(501)*Re(NStbil(10))

	NStbix(532) = 4*MB2**2 - MB2*(MHH2 - MHp2 + 2*MT2)

	NStbix(534) = 4*MB2*MT2*Yuk3 + TB*Yuk3**2*NStbix(533)

	NStbix(536) = MT2**2 + MT2*(MB2 - MHp2)

	NStbix(538) = (MA02 + 2*MB2 - MHp2)*TB + 4*MB2*(TB2*Yuk3)

        NStbix(541) = MB2*NStbix(540) +
     &    2*(MB2*MT2*Re(NStbil(10)) + MA02*NStbix(539)) -
     &    2*((MT2 - MB2*(TB2*Yuk3**2))*Re(NStbil(6)) +
     &       (-MT2**2 + MT2*MHp2)*Re(NStbil(10)))

        NStbix(543) = 4*MB2*MT2*(TB2*Yuk3) +
     &    (MB2 - MHp2 + MT2)*TB*(MT2 + MB2*(TB2*Yuk3**2))

        NStbix(548) = 2*MT2*NStbix(544) - TB*NStbix(547) -
     &    NStbix(543)*Re(NStbil(14))

        NStbix(55) = -(MT2**2*(6*MHH2 + 11*MT2)) +
     &    11*MB2**3*(TB2*Yuk3**2)

        NStbix(550) = MT2*
     &     (NStbix(549) - Re(NStbil(5)) + 2*Re(NStbil(6))) +
     &    (MT2 + MB2*(TB2*Yuk3**2))*(Re(NStbil(7)) - Re(NStbil(9)))

	NStbix(552) = 3*MB2*(TB*TB2*Yuk3**2) + MT2*(TB + 4*TB2*Yuk3)

        NStbix(555) = 2*MB2*NStbix(552) + TB*NStbix(554) -
     &    NStbix(501)*Re(NStbil(13))

        NStbix(558) = MB2*(TB2*Yuk3**2*NStbix(557)) -
     &    (MT2 + MB2*(TB2*Yuk3**2))*(Re(NStbil(6)) - Re(NStbil(9)))

	dup2 = TB*NStbix(558) + NStbix(501)*Re(NStbil(13))

        NStbix(562) = 2*dup2 + (4*MB2 - 3*MT2)*NStbix(544) -
     &    MB2*NStbix(552) -
     &    2*(2*MB2 - MT2)*(TB*NStbix(561) +
     &       NStbix(560)*Re(NStbil(14)))

        NStbix(565) = (-MB2**3 + MB2*MT2**2)*NStbix(552) +
     &    NStbix(544)*NStbix(563) +
     &    2*((MB2 - 3*MT2)*(MB2 - MT2)*
     &        (TB*NStbix(547) + NStbix(543)*Re(NStbil(14))) +
     &       dup2*NStbix(564))

        NStbix(566) = MT2*NStbix(544) +
     &    (3*MB2 - 4*MT2)*NStbix(552) - 2*TB*NStbix(547)

        NStbix(567) = (MB2 - 2*MT2)*dup2 +
     &    MB2*(NStbix(543)*Re(NStbil(14)))

	NStbix(57) = 2*MHH2*TB*(1 - TB2*Yuk3**2) + MT2*NStbix(56)

	NStbix(59) = 18*MHH2*TB*TB2*Yuk3**2 + MT2*NStbix(58)

	NStbix(62) = 2*MT2*(MA02 - 4*MB2) + TB*Yuk3*NStbix(61)

        NStbix(75) = 16*MB2*MT2*NStbix(70) +
     &    2*MA02**3*TB*NStbix(63) - MA02**2*NStbix(67) +
     &    4*MA02*NStbix(74)

        NStbix(80) = MT2*
     &    (2*NStbil(6)**2*NStbix(77) - NStbil(6)*NStbix(79))

        NStbix(85) = 5*MHH2**2 - 28*MT2**2 +
     &    MT2*(-49*MHH2 + 56*NStbil(23)) + 24*NStbil(6)**2 +
     &    4*((19*MHH2 - 88*MT2)*NStbil(6) + MHH2*NStbix(84))

        NStbix(9) = 3*MA02**3 +
     &    MB2*(64*NStbil(7)**2 - 4*(8*MA02**2 - NStbix(7))) -
     &    MA02*NStbix(6) + 16*NStbil(7)*NStbix(8)

        NStbix(96) = -8*NStbix(88) +
     &    4*(MA02 - 4*MT2)*NStbil(5)*NStbix(89) -
     &    (MA02 - MHH2)*NStbix(93) - 4*NStbil(8)*NStbix(95)

	NStbix(98) = 3*MA02 - 2*MT2*(5 - 1/MT2*NStbil(6))

        NStbix(101) = (MB2 + MT2)/(MB2 - MT2)**2*
     &     ((MT2 - MB2*(TB2*Yuk3**2))*NStbil(6)) +
     &    1/(MB2 - MT2)*NStbix(100)

        NStbix(111) = MT2*
     &    ((2*NStbil(6)**2*NStbix(107))/((MA02 - 4*MT2)*TB) -
     &      NStbil(6)*NStbix(110))

        NStbix(12) = -4*NStbix(5) + (MA02 - MHH2)*NStbix(9) +
     &    4*NStbil(8)*NStbix(11)

	NStbix(121) = MB2*(NStbil(7)**2 - NStbix(120)/(MHH2 - MHp2))

	NStbix(122) = 2*MT2 - (MA02 - 2*MB2)*TB*Yuk3

	NStbix(123) = 2*MT2 + (2*MB2 - MHp2)*TB*Yuk3

	NStbix(124) = MB2**2 + (MHp2 - MT2)**2 - 2*MB2*(MHp2 + MT2)

        NStbix(136) = MT2*
     &    (2*NStbil(6)**2 -
     &      (NStbil(6)*NStbix(135))/((MHH2 - MHp2)*NStbix(124)))

        NStbix(144) = MT2*
     &    (2*NStbil(6)**2 -
     &      (NStbil(6)*(MA02*NStbix(124)*NStbix(137) -
     &           MHp2*NStbix(143)))/((MA02 - MHp2)*TB*NStbix(124)))

        NStbix(161) = NStbix(151)/(MHH2 - MHp2)**2 +
     &    NStbix(160)/((MA02 - MHp2)**2*(MA02 - 4*MT2))

        NStbix(17) = NStbix(14)/(4*MB2 - MHH2) +
     &    NStbix(16)/(MA02 - 4*MB2)

        NStbix(243) = -(4/(MB2 - MT2)*
     &       (NStbil(6)*NStbix(201))) - NStbix(182) +
     &    (8*NStbil(9)*NStbix(242))/MHp2

        NStbix(26) = (MA02 - MHp2)*NStbix(19) +
     &    16*NStbil(7)*NStbix(22) +
     &    4*(NStbil(8)*(MA02*NStbix(20) +
     &          4*NStbil(7)*NStbix(21)) + NStbix(25))

        NStbix(33) = -6*MHH2**3 + 3*MHp2**3 + 32*MB2**2*MHH2 +
     &    MB2*(14*MHH2**2 - 10*MHp2**2 + 16*NStbix(32)) +
     &    16*(MHH2 - MHp2)*NStbil(7)**2 -
     &    8*(-MHp2**2 + 4*MB2*MHp2)*NStbil(18) - MHp2*NStbix(27) +
     &    8*NStbil(19)*NStbix(28) + 4*NStbil(5)*NStbix(29) -
     &    (4*NStbil(9)*NStbix(30))/MHp2

        NStbix(337) = -(4/(MB2 - MT2)**2*
     &       (NStbil(6)*NStbix(299))/
     &        ((MB - MHp - MT)**2*(MB + MHp - MT)**2*
     &          (MB - MHp + MT)**2*(MB + MHp + MT)**2)) +
     &    1/(MB2 - MT2)*((2*NStbil(9)*NStbix(318))/MHp2 +
     &        NStbix(336))/NStbix(124)**2

	NStbix(35) = MB2**2 + MT2**2 - MT2*MHH2 - MB2*(MHH2 - 6*MT2)

	NStbix(38) = 2*MB2*((MHH2 - 4*MT2)*TB2*Yuk3) + TB*NStbix(37)

        NStbix(40) = 2*MB2**2*(TB*Yuk3) +
     &    MT2*(MHH2*(2 + TB*Yuk3)) + MB2*NStbix(39)

        NStbix(402) = (8*MT2)/(MB2 - MT2)*NStbix(386) -
     &    (MHp2*NStbix(397) + NStbix(124)*NStbix(401))/
     &     (TB*NStbix(124))

        NStbix(428) = (8*MT2)/(MB2 - MT2)*NStbix(404) -
     &    NStbix(427)/NStbix(124)

        NStbix(46) = 2*MB2*(NStbil(6)*NStbix(43))/(TB*TB2) +
     &    MB2**2*NStbix(45)

        NStbix(503) = MB2*((MA02 - 2*MB2)*TB2*Yuk3**2) +
     &    MT2*NStbix(498) + NStbix(502)/TB

        NStbix(504) = MB2*(dup1*(MA02 - 2*MHp2)*TB2*Yuk3**2) +
     &    MA02*(MT2 + MB2*(TB2*Yuk3**2))*
     &     (Re(NStbil(6)) + Re(NStbil(7)))

        NStbix(506) = (MT2 + MB2*(TB2*Yuk3**2))*
     &     (Re(NStbil(6)) + Re(NStbil(7))) +
     &    (NStbix(501)*Re(NStbil(10)))/TB

        NStbix(509) = MB2*
     &     (TB2*Yuk3**2*(MA02 - 2*NStbix(508))) +
     &    MT2*(MA02 - 2*MT2 + 2*MA02*Re(NStbil(1)) -
     &       4*Re(NStbil(6))) + NStbix(507)/TB

        NStbix(511) = MT2*(MHH2 - 6*MT2) -
     &    MB2*((6*MB2 - MHH2)*TB2*Yuk3**2) + NStbix(502)/TB

        NStbix(512) = MT2*
     &    ((MHH2 - 4*MT2)*Re(NStbil(4)) - 2*Re(NStbil(6)))

        NStbix(514) = MB2*
     &     ((MHH2 - 2*MHp2)*TB2*Yuk3**2*NStbix(513)) +
     &    MHH2*(MT2 + MB2*(TB2*Yuk3**2))*
     &     (Re(NStbil(6)) + Re(NStbil(7)))

        NStbix(517) = -(MB2*
     &       (TB2*Yuk3**2*(6*MB2 - MHH2 + 2*NStbix(513)))) +
     &    MT2*(NStbix(516) - 4*Re(NStbil(6))) + NStbix(507)/TB

        NStbix(520) = 4*MT2**2 +
     &    MB2*(TB2*Yuk3**2*NStbix(519)) +
     &    MT2*(MA02 - MHH2 + 2*MA02*Re(NStbil(1)) -
     &       2*(MHH2 - 4*MT2)*Re(NStbil(4)))

	NStbix(525) = MA02*NStbix(523) + 8*(MA02 - MHH2)*NStbix(524)

	NStbix(528) = MHH2*NStbix(526) + 2*(2*MA02 - MHH2)*NStbix(527)

        NStbix(531) = (NStbil(9)*NStbix(530))/MHp2 +
     &    TB*(MT2 + MB2*(TB2*Yuk3**2))*
     &     (Re(NStbil(6)) + Re(NStbil(7))) +
     &    NStbix(501)*Re(NStbil(10))

        NStbix(535) = 4*MB2*MT2*Yuk3 -
     &    TB*Yuk3**2*NStbix(532) + 2*NStbix(534)*Re(NStbil(10))

        NStbix(537) = -4*MT2**2 + MT2*(2*MB2 + MHH2 - MHp2) -
     &    2*(TB2*Yuk3**2*(4*MB2**2 - MB2*MHH2)*Re(NStbil(3)) +
     &       (MT2 - MB2*(TB2*Yuk3**2))*
     &        (Re(NStbil(6)) - Re(NStbil(7)))) +
     &    2*((-4*MT2**2 + MT2*MHH2)*Re(NStbil(4)) +
     &       NStbix(536)*Re(NStbil(10)))

        NStbix(542) = MT2*NStbix(538) + TB*NStbix(541) +
     &    2*TB2*NStbix(534)*Re(NStbil(10))

        NStbix(551) = MB2*MT2*(TB2*Yuk3*(4 + TB*Yuk3)) -
     &    1/MT2*(NStbil(6)*NStbix(548)) +
     &    TB*(3*MT2**2 + 2*NStbix(550)) -
     &    2*NStbix(543)*Re(NStbil(14))

        NStbix(559) = -2*dup2 + MB2*NStbix(552) -
     &    1/MB2*(NStbil(7)*NStbix(555))

        NStbix(568) = 2*MT2*(NStbil(6)*NStbix(562)) -
     &    NStbix(565) - 2*NStbil(7)*
     &     (MB2*NStbix(566) - 2*NStbix(567))

        NStbix(60) = MB2*MT2*NStbix(57) + MB2**2*NStbix(59) +
     &    3*TB*NStbix(55)

        NStbix(76) = (8*MB2)/(MB2 - MT2)**2*
     &     (Yuk3*NStbil(7)**2*NStbix(62)) -
     &    1/(MB2 - MT2)*NStbix(75)/((MA02 - 4*MT2)*TB2)

        NStbix(81) = 2*MB2*
     &     (Yuk3*(2*MT2 + (MB2 + MT2)*(TB*Yuk3))*NStbil(7)**2) +
     &    NStbix(80)/TB2

        NStbix(97) = NStbix(85)/(MHH2 - 4*MT2) +
     &    NStbix(96)/((MA02 - MHH2)**2*(MA02 - 4*MT2))

        NStbix(99) = 5*MA02**2 - 28*MT2**2 +
     &    MT2*(-MA02 + 24*NStbil(21)) +
     &    4*(3*MA02 - 8*MT2)*NStbil(6) - 8*NStbil(6)**2 -
     &    4*MA02*(2*NStbil(21) - (NStbil(8)*NStbix(98))/MA02)

        NStbix(145) = 2*MB2*
     &     (Yuk3*(MA02*NStbil(17)*NStbix(122) -
     &          MHp2*NStbil(18)*NStbix(123)))/((MA02 - MHp2)*TB) -
     &    2*Yuk3**2*(MB2*NStbil(7)**2 + NStbix(121)) +
     &    (NStbix(136) + NStbix(144))/TB2

        NStbix(34) = -NStbix(17) +
     &    (NStbix(12)/(MA02 - MHH2)**2 +
     &       NStbix(26)/(MA02 - MHp2)**2)/(MA02 - 4*MB2) -
     &    NStbix(33)/(MHH2 - MHp2)**2

        NStbix(36) = 4*MB2*MT2*
     &     ((MHH2 - 2*(MB2 + MT2))*Yuk3)/TB -
     &    ((MT2 + MB2*(TB2*Yuk3**2))*NStbix(35))/TB2

        NStbix(41) = MT2*(NStbil(23)*NStbix(38))/TB2 +
     &    MB2*(Yuk3*NStbil(19)*NStbix(40))

        NStbix(505) = 4*MT2*((MA02 - 2*MHp2)*Re(NStbil(6))) +
     &    MA02*NStbix(503) + 2*NStbix(504)

	NStbix(510) = 2*(2*MA02 - MHp2)*NStbix(506) + MHp2*NStbix(509)

        NStbix(515) = MHH2*NStbix(511) -
     &    2*(MHH2 - 2*MHp2)*NStbix(512) + 2*NStbix(514)

	NStbix(518) = 2*(2*MHH2 - MHp2)*NStbix(506) + MHp2*NStbix(517)

        NStbix(529) = (MA02 - MHH2)*(MA02 + MHH2)*
     &     NStbix(520) + 2*
     &     (NStbil(8)*NStbix(525) - NStbil(5)*NStbix(528))

        NStbix(569) = -(1/(MB2 - MT2)**2*NStbix(568)) +
     &    8*NStbix(531) + ((MHH2 + MHp2)*
     &       (TB2*NStbix(535) + TB*NStbix(537)))/(MHH2 - MHp2) +
     &    ((MA02 + MHp2)*NStbix(542))/(MA02 - MHp2) +
     &    4*(NStbix(551) + NStbix(559))

        NStbix(82) = 1/(MB2 - MT2)*NStbix(60)/TB2 -
     &    4/(MB2 - MT2)**2*NStbix(81) - NStbix(76)/(MA02 - 4*MB2)

        NStbix(570) = 2*
     &     ((NStbil(8)*NStbix(505) - NStbil(9)*NStbix(510))/
     &        (MA02 - MHp2)**2 +
     &       (NStbil(5)*NStbix(515) - NStbil(9)*NStbix(518))/
     &        (MHH2 - MHp2)**2) - NStbix(529)/(MA02 - MHH2)**2 +
     &    NStbix(569)/TB

        NStbix(343) = MT2 + MB2*(TB2*Yuk3**2) +
     &    ((MB2 - MHp2 - MT2)*NStbix(340))/NStbix(124)

        NStbix(357) = 1/(MB2 - MT2)*
     &     (MT2 - MB2*(TB2*Yuk3**2)) - NStbix(340)/NStbix(124)

        NStbix(113) = MT2*(MB2 + MT2)*(MA02 - 4*MB2) -
     &    MB2*(TB2*Yuk3**2*NStbix(61))

        NStbix(116) = 16*MB2*MT2*NStbix(114) +
     &    MA02**2*NStbix(100) - 4*MA02*NStbix(115)

        NStbix(245) = -(2*MT2*(MT2 + MB2*(TB*Yuk3))**2) +
     &    MHp2*NStbix(244)

        NStbix(341) = 1/(MB2 - MT2)*
     &     (MT2 - MB2*(TB2*Yuk3**2)) +
     &    (2*MHp2*NStbix(340))/((MA02 - MHp2)*NStbix(124))

        NStbix(344) = (MT2 + MB2*(TB2*Yuk3**2))*NStbix(342) +
     &    (MA02 - MHp2)*NStbix(343)

        NStbix(358) = (2*MA02 - MHp2)*
     &     (MT2 + MB2*(TB2*Yuk3**2)) -
     &    (MA02 - MHp2)*(MB2 - MHp2 + MT2)*NStbix(357)

        NStbix(367) = ((MA02 - MHp2)*(MB2 + MHp2 - MT2)*
     &       NStbix(340))/NStbix(124) -
     &    (MT2 + MB2*(TB2*Yuk3**2))*NStbix(366)

        NStbix(448) = -(1/(MB2 - MT2)*NStbix(447)) +
     &    (MHp2*(MB2 - MHp2 + MT2)*NStbix(340))/NStbix(124)

        NStbix(454) = 1/(MB2 - MT2)*
     &     ((MHH2 - MHp2)*NStbix(450)) +
     &    (MT2*NStbix(451) + MB2*NStbix(453))/NStbix(124)

        NStbix(481) = 1/(MB2 - MT2)*
     &     (MT2 - MB2*(TB2*Yuk3**2)) +
     &    (2*MHp2*NStbix(340))/((MHH2 - MHp2)*NStbix(124))

        NStbix(493) = 1/(MB2 - MT2)*
     &     ((MHH2 - MHp2)*NStbix(450)) + NStbix(492)/NStbix(124)

        NStbix(51) = (MB2 - MT2)**2*
     &     (TB*(MT2 + MB2*(TB2*Yuk3**2))) - MA02*NStbix(43)

        NStbix(102) = (-MB2 - MT2)*
     &     ((MT2 - MB2*(TB2*Yuk3**2))*NStbil(5)) +
     &    (NStbil(6)*NStbix(43))/TB

        NStbix(117) = 1/(MB2 - MT2)**2*
     &     (NStbil(7)*NStbix(113)) -
     &    1/(MB2 - MT2)*NStbix(116)/(MA02 - 4*MT2)

        NStbix(275) = -(NStbil(25)*NStbix(245)) +
     &    NStbil(18)*NStbix(247) -
     &    (NStbil(24)*NStbix(274))/NStbix(124)**2

        NStbix(345) = NStbil(6)*NStbix(341) +
     &    (NStbil(9)*NStbix(344))/(MA02 - MHp2)**2

        NStbix(359) = MA02*NStbil(20)*NStbix(355) -
     &    NStbil(24)*(MA02*NStbix(356) - MHp2*NStbix(358))

        NStbix(384) = 2*MHp2*NStbil(6)*NStbix(367) +
     &    (MHp2**2*(2*NStbix(376) - MA02*NStbix(383)))/
     &     (TB*NStbix(124))

        NStbix(449) = MB2**2*(MHH2*TB2*Yuk3**2) +
     &    MT2*NStbix(442) + MB2*NStbix(443) -
     &    (MHH2 - MHp2)*NStbix(448)

        NStbix(469) = 2*MHp2*NStbil(6)*NStbix(454) +
     &    (MHp2**2*NStbix(468))/NStbix(124)

        NStbix(494) = NStbil(6)*NStbix(481) -
     &    (NStbil(5)*NStbix(482) + NStbil(9)*NStbix(493))/
     &     (MHH2 - MHp2)**2

        NStbix(52) = MT2*
     &     (MA02*NStbil(21)*NStbix(50))/(MA02 - 4*MT2) +
     &    NStbil(20)*NStbix(51)

	NStbix(105) = 2*MB2*NStbix(102) + MB2**2*NStbix(104)/TB

        NStbix(118) = 1/(MB2 - MT2)**2*
     &     (NStbil(6)*NStbix(112))/(MA02 - 4*MT2) +
     &    NStbix(117)/(MA02 - 4*MB2)

        NStbix(276) = (NStbil(6)**2 + NStbil(7)**2)*
     &     NStbix(244) - NStbix(275)

        NStbix(353) = -(2*MB2*NStbix(345)) +
     &    MB2**2/(MB2 - MT2)*
     &     (MA02*NStbix(124)*NStbix(346) - MHp2*NStbix(352))/
     &      ((MA02 - MHp2)*TB*NStbix(124))

        NStbix(385) = 2*NStbix(359) +
     &    MA02*NStbil(8)*NStbix(365) + (NStbil(9)*NStbix(384))/MHp2

        NStbix(470) = 2*NStbil(22)*NStbix(435) +
     &    (NStbil(5)*NStbix(441))/MHH2 -
     &    2*NStbil(24)*NStbix(449) - (NStbil(9)*NStbix(469))/MHp2

        NStbix(495) = MB2**2/(MB2 - MT2)*
     &     NStbix(480)/((MHH2 - MHp2)*NStbix(124)) -
     &    2*MB2*NStbix(494)

        NStbix(53) = MB2*
     &     (MA02*Yuk3*NStbil(17)*NStbix(48))/(MA02 - 4*MB2) +
     &    NStbix(52)/TB2

        NStbix(119) = -(MT2*
     &       (NStbix(97) + NStbix(99)/(MA02 - 4*MT2))) -
     &    1/(MB2 - MT2)**2*
     &     (-(2/MB2*(NStbil(7)*NStbix(105))) + 2*NStbix(111)) +
     &    4*NStbil(5)*NStbix(101) - 4*NStbil(8)*NStbix(118)

        NStbix(338) = MT2*NStbix(161) -
     &    8/(MB2 - MT2)**2*NStbix(276) -
     &    NStbix(243)/NStbix(124)**2 + 4*NStbil(7)*NStbix(337)

        NStbix(403) = 4*
     &     (1/MB2*(NStbil(7)*NStbix(353)) +
     &       NStbix(385)/(MA02 - MHp2)**2) +
     &    NStbix(402)/(MA02 - MHp2)

        NStbix(496) = NStbix(428)/(MHH2 - MHp2) +
     &    4*(1/MB2*(NStbil(7)*NStbix(495)) +
     &       NStbix(470)/(MHH2 - MHp2)**2)

	NStbix(54) = 1/MB2*(NStbil(7)*NStbix(46)) - (2*NStbix(53))/TB

        NStbix(497) = 1/(MB2 - MT2)**2*
     &     (8*(NStbil(22)*NStbix(36) - NStbix(41)/TB) +
     &       4*NStbix(54)) + 8/(MB2 - MT2)*NStbix(145) +
     &    NStbix(82)/TB + 2*
     &     (MB2*(Yuk3**2*NStbix(34)) + NStbix(119)/TB2) +
     &    (-2*NStbix(338) + 2*(NStbix(403) + NStbix(496)))/TB2

        NStbindMasses =
     &   Alfa2/(MW2**2*Pi**2*SW2**2)*
     &    (3/2048.D0*NStbix(497) - 3/512.D0*NStbix(570)/TB2)

	end
