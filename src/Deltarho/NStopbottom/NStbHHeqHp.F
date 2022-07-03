c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich


* NStbHHeqHp.F
* generated 29-Jan-2017 14:53

C> @brief This subroutine calculates \f$\delta\rho^{(2)}_\mathrm{tb,NS}\f$ with \f$m_{H^0}=m_{H^\pm}\f$.
C>
C> @ingroup group_rho
	subroutine calcNStbHHeqHp
	implicit none


#include "NStbHHeqHp.h"

	ComplexType dup1, dup2

	RealType T134sub

	external T134sub

	ComplexType A0FH, B0FH

	external A0FH, B0FH

#include "paraTHDM.h"
#include "paraSM.h"
	NStbhhl(1) = A0FH(MT2)

	NStbhhl(10) = B0FH(MB2,MHp2,MT2)

	NStbhhl(11) = B0FH(MT2,MB2,MHp2)

	NStbhhl(12) = B0FH(MT2,MA02,MT2)

	NStbhhl(13) = B0FH(MT2,MHp2,MT2)

	NStbhhl(14) = T134sub(MB2,MB2,MA02)

	NStbhhl(15) = T134sub(MHp2,MB2,MB2)

	NStbhhl(16) = T134sub(MT2,MB2,MA02)

	NStbhhl(17) = T134sub(MT2,MT2,MA02)

	NStbhhl(18) = T134sub(MT2,MHp2,MB2)

	NStbhhl(19) = T134sub(MT2,MT2,MHp2)

	NStbhhl(2) = A0FH(MB2)

	NStbhhl(3) = B0FH(MHp2,MB2,MB2)

	NStbhhl(4) = B0FH(MHp2,MB2,MT2)

	NStbhhl(5) = B0FH(MHp2,MT2,MT2)

	NStbhhl(6) = A0FH(MHp2)

	NStbhhl(7) = A0FH(MA02)

	NStbhhl(8) = B0FH(MB2,MA02,MB2)

	NStbhhl(9) = B0FH(MB2,MB2,MHp2)

	NStbhhx(335) = 26 + 20*TB*Yuk3 - 21*TB2*Yuk3**2

	NStbhhx(336) = 34 + 28*TB*Yuk3 - 21*TB2*Yuk3**2

	NStbhhx(337) = 17 + 28*TB*Yuk3 - 4*TB2*Yuk3**2

	NStbhhx(341) = -26 + 28*TB*Yuk3 + 15*TB2*Yuk3**2

	NStbhhx(342) = -6 + 14*TB*Yuk3 + 9*TB2*Yuk3**2

	NStbhhx(343) = -14 + 20*TB*Yuk3 + 3*TB2*Yuk3**2

	NStbhhx(345) = 21 + 20*TB*Yuk3 - 2*TB2*Yuk3**2

	NStbhhx(346) = 2 + 4*TB*Yuk3 + TB2*Yuk3**2

	NStbhhx(347) = 33 + 28*TB*Yuk3 + 2*TB2*Yuk3**2

	NStbhhx(377) = -4 + 36*TB*Yuk3 + 11*TB2*Yuk3**2

	NStbhhx(378) = 4 + 24*TB*Yuk3 + 11*TB2*Yuk3**2

	NStbhhx(295) = -2 + SW2*(14 - 23*TB2*Yuk3**2)

	NStbhhx(323) = -9 + 2*SW2**2 - 18*TB*Yuk3

	NStbhhx(329) = 18*TB + (9 - 8*SW2**2)*(TB2*Yuk3)

        NStbhhx(333) =
     &   -18 + 135*TB2*Yuk3**2 + 12*SW2*(7 - 34*TB2*Yuk3**2) -
     &    16*SW2**2*(7 - 26*TB2*Yuk3**2)

        NStbhhx(338) =
     &   16*SW2**2*NStbhhx(335) - 12*SW2*NStbhhx(336) +
     &    9*NStbhhx(337)

        NStbhhx(339) =
     &   27 - 6*SW2*(21 - 10*TB2*Yuk3**2) +
     &    56*SW2**2*(3 - 2*TB2*Yuk3**2)

        NStbhhx(344) =
     &   -(6*SW2*NStbhhx(341)) + 8*SW2**2*NStbhhx(343) +
     &    9*NStbhhx(342)

        NStbhhx(348) =
     &   16*SW2**2*NStbhhx(345) - 12*SW2*NStbhhx(347) +
     &    63*NStbhhx(346)

        NStbhhx(349) =
     &   9*(2 + 5*TB2*Yuk3**2) + 16*SW2**2*(7 + 4*TB2*Yuk3**2) -
     &    12*SW2*(7 + 8*TB2*Yuk3**2)

	NStbhhx(356) = 1 + 2*TB*Yuk3 - 5*TB2*Yuk3**2

	NStbhhx(358) = 19 + 16*TB*Yuk3 - 65*TB2*Yuk3**2

	NStbhhx(359) = 1 + 2*TB*Yuk3 - 2*TB2*Yuk3**2

	NStbhhx(361) = 71 + 72*TB*Yuk3 - 86*TB2*Yuk3**2

	NStbhhx(362) = 1 + 2*TB*Yuk3 - TB2*Yuk3**2

	NStbhhx(364) = -14 + 8*TB*Yuk3 + 29*TB2*Yuk3**2

	NStbhhx(365) = 45 + 60*TB*Yuk3 - 19*TB2*Yuk3**2

	NStbhhx(366) = 10 + 7*TB*Yuk3 - 5*TB2*Yuk3**2

	NStbhhx(367) = 2 + 4*TB*Yuk3 - TB2*Yuk3**2

	NStbhhx(369) = -5 + 4*TB*Yuk3 + 12*TB2*Yuk3**2

	NStbhhx(370) = -3 + 2*TB*Yuk3 + TB2*Yuk3**2

	NStbhhx(371) = 5 + 10*TB*Yuk3 - TB2*Yuk3**2

	NStbhhx(372) = 19 + 16*TB*Yuk3 + 6*TB2*Yuk3**2

	NStbhhx(373) = 34 + 88*TB*Yuk3 + 7*TB2*Yuk3**2

	NStbhhx(375) = -13 + 24*TB*Yuk3 + 7*TB2*Yuk3**2

	NStbhhx(376) = -4 + 4*TB*Yuk3 + 3*TB2*Yuk3**2

        NStbhhx(379) =
     &   -(MT2**3*(MHp2**2*NStbhhx(377))) +
     &    MT2**2*(MHp2**3*NStbhhx(378))

        NStbhhx(206) =
     &   27*(MB2 - 2*MHp2)*MHp2 - 3*MT2**2 + MT2*(14*MB2 - 69*MHp2)

	NStbhhx(220) = 8*TB2*Yuk3 - 3*TB*(5 + 3*TB2*Yuk3**2)

	NStbhhx(226) = 96*TB2*Yuk3 - TB*(34 + 3*TB2*Yuk3**2)

	NStbhhx(227) = 16*TB2*Yuk3 - TB*(19 + 18*TB2*Yuk3**2)

	NStbhhx(279) = -1 + SW2*(16 - 23*TB2*Yuk3**2)

	NStbhhx(288) = -1 + 11*SW2*(1 - TB2*Yuk3**2)

	NStbhhx(296) = SW2*(16 - 23*TB2*Yuk3**2) - CW2*NStbhhx(295)

	NStbhhx(318) = -3*(MB2 - MHp2)**2 - 5*MT2**2 + 8*MT2*MHp2

	NStbhhx(322) = 3*MB2*(3 + 4*SW2) - 4*SW2**2*(MB2 + MHp2 - MT2)

        NStbhhx(324) =
     &   (3 - 2*SW2)**2*(MHp2 - MT2) + 2*MB2*NStbhhx(323)

        NStbhhx(330) =
     &   (3 - 4*SW2)**2*((MB2 - MHp2)*TB2*Yuk3) +
     &    2*MT2*NStbhhx(329)

        NStbhhx(332) =
     &   MT2*(9 + 72*SW2 - 32*SW2**2) -
     &    2*(9 - 42*SW2 + 56*SW2**2)*MHp2

        NStbhhx(334) =
     &   12*(9 - 33*SW2 + 28*SW2**2)*(MHp2*TB2*Yuk3**2) +
     &    MT2*NStbhhx(333)

        NStbhhx(340) =
     &   -(12*(9 - 33*SW2 + 28*SW2**2)*(MHp2**2*TB2*Yuk3**2)) +
     &    MT2**2*NStbhhx(338) + 2*MT2*(MHp2*NStbhhx(339))

        NStbhhx(350) =
     &   4*(9 - 33*SW2 + 28*SW2**2)*(MHp2**3*TB2*Yuk3**2) +
     &    2*MT2**2*(MHp2*NStbhhx(344)) - MT2**3*NStbhhx(348) -
     &    3*MT2*(MHp2**2*NStbhhx(349))

	NStbhhx(355) = MHp2**2 + 9*MT2**2 - 12*MT2*MHp2

	NStbhhx(357) = 17*MHp2*TB2*Yuk3**2 - 4*MT2*NStbhhx(356)

        NStbhhx(360) =
     &   26*MHp2**2*TB2*Yuk3**2 - MT2*(MHp2*NStbhhx(358)) -
     &    20*MT2**2*NStbhhx(359)

        NStbhhx(363) =
     &   16*MHp2**3*TB2*Yuk3**2 -
     &    2*MT2*(MHp2**2*(17 - 40*TB2*Yuk3**2)) -
     &    MT2**2*(MHp2*NStbhhx(361)) - 40*MT2**3*NStbhhx(362)

        NStbhhx(368) =
     &   MHp2**4*TB2*Yuk3**2 + MT2*(MHp2**3*NStbhhx(364)) -
     &    MT2**3*(MHp2*NStbhhx(365)) -
     &    4*MT2**2*(MHp2**2*NStbhhx(366)) - 10*MT2**4*NStbhhx(367)

        NStbhhx(374) =
     &   MHp2**5*TB2*Yuk3**2 - 2*MT2*(MHp2**4*NStbhhx(369)) -
     &    8*MT2**3*(MHp2**2*NStbhhx(370)) +
     &    4*MT2**5*NStbhhx(371) +
     &    2*MT2**2*(MHp2**3*NStbhhx(372)) +
     &    MT2**4*(MHp2*NStbhhx(373))

        NStbhhx(380) =
     &   MHp2**5*(1 - TB2*Yuk3**2) + 4*MT2**5*(1 + 2*TB*Yuk3) +
     &    MT2**4*(MHp2*NStbhhx(375)) -
     &    2*MT2*(MHp2**4*NStbhhx(376)) + 2*NStbhhx(379)

	NStbhhx(428) = 1 + SW2*(11 + TB2*Yuk3**2)

	NStbhhx(436) = 2 + SW2*(16 + TB2*Yuk3**2)

	NStbhhx(204) = 3*MHp2**2 + 5*MT2**2 - 12*MT2*MHp2

        NStbhhx(207) =
     &   -(4*MT2*(2*MB2 - 3*MHp2 - 6*MT2)) + TB*Yuk3*NStbhhx(206)

        NStbhhx(209) =
     &   25*MHp2**3 - 14*MT2**3 + 17*MT2**2*MHp2 + 30*MT2*MHp2**2

	NStbhhx(210) = 26*MHp2**2 + 37*MT2**2 + 3*MT2*MHp2

	NStbhhx(211) = MHp2**2 + 2*MT2**2 + 12*MT2*MHp2

        NStbhhx(213) =
     &   -(MHp2**4*(4 - 5*TB*Yuk3)) +
     &    2*MT2*(MHp2**3*(6 - 7*TB*Yuk3)) - MT2**4*(8 - 3*TB*Yuk3)

	NStbhhx(214) = 4*TB2*Yuk3 - TB*(5 + 16*TB2*Yuk3**2)

	NStbhhx(215) = 5*MHp2**4 + 14*MT2**4 - 13*MT2*MHp2**3

	NStbhhx(216) = 4*TB2*Yuk3 - 5*TB*(11 + 2*TB2*Yuk3**2)

        NStbhhx(218) =
     &   21*MHp2**4*TB*Yuk3 + MT2**4*(16 - 37*TB*Yuk3) -
     &    6*MT2**3*(MHp2*(28 + 3*TB*Yuk3))

	NStbhhx(219) = 2*TB2*Yuk3 + TB*(7 - 15*TB2*Yuk3**2)

        NStbhhx(221) =
     &   MT2**3*((9*MHp2 - 14*MT2)*TB) + MT2*(MHp2**3*NStbhhx(220))

	NStbhhx(223) = TB - 2*TB2*(4*Yuk3 - 3*TB*Yuk3**2)

	NStbhhx(224) = 2*TB2*Yuk3 - TB*(15 - 7*TB2*Yuk3**2)

	NStbhhx(225) = 16*TB2*Yuk3 - 17*TB*(1 + TB2*Yuk3**2)

        NStbhhx(228) =
     &   MT2**4*(MHp2*NStbhhx(226)) + MT2*(MHp2**4*NStbhhx(227))

	NStbhhx(280) = SW2*(17 - 23*TB2*Yuk3**2) - CW2*NStbhhx(279)

	NStbhhx(285) = 96*TB2*Yuk3 - TB*(20 - 11*TB2*Yuk3**2)

	NStbhhx(286) = 68*TB2*Yuk3 - 5*TB*(9 - 5*TB2*Yuk3**2)

	NStbhhx(289) = SW2*(12 - 11*TB2*Yuk3**2) - CW2*NStbhhx(288)

	NStbhhx(292) = 48*TB2*Yuk3 + TB*(163 - 53*TB2*Yuk3**2)

	NStbhhx(293) = 19*TB2*Yuk3 - 8*TB*(3 - TB2*Yuk3**2)

	NStbhhx(294) = 22*TB2*Yuk3 + TB*(5 + 11*TB2*Yuk3**2)

        NStbhhx(297) =
     &   -147 - 34*TB2*Yuk3**2 - 16*SW2**2*(14 - 23*TB2*Yuk3**2) +
     &    16*NStbhhx(296)

        NStbhhx(319) =
     &   -(4*MT2*((MB2 - MHp2 + 3*MT2)*TB*Yuk3)) +
     &    TB2*Yuk3**2*NStbhhx(318)

	NStbhhx(321) = (MB2 - MHp2 - MT2)**2 - 4*MT2*MHp2

        NStbhhx(325) =
     &   MB2*(TB2*Yuk3**2*NStbhhx(322)) - MT2*NStbhhx(324)

	NStbhhx(328) = -(MB2 + MHp2 - MT2)**2 + 4*MB2*MHp2

        NStbhhx(331) =
     &   3*MT2**2*(3 + 8*SW2) + 16*MT2*SW2**2*(MB2 - MHp2 - MT2) +
     &    MB2*(Yuk3*NStbhhx(330))

        NStbhhx(351) =
     &   4*MB2**4*(9 - 33*SW2 + 28*SW2**2)*(TB2*Yuk3**2) +
     &    MT2*((MHp2 - MT2)**2*NStbhhx(332)) -
     &    MB2**3*NStbhhx(334) - MB2**2*NStbhhx(340) -
     &    MB2*NStbhhx(350)

        NStbhhx(381) =
     &   -(4*MB2**7*(TB2*Yuk3**2)) +
     &    MT2**2*(MHp2*(MHp2 - MT2)**2*NStbhhx(355)) +
     &    MB2**6*NStbhhx(357) - MB2**5*NStbhhx(360) +
     &    MB2**4*NStbhhx(363) - 2*MB2**3*NStbhhx(368) -
     &    MB2**2*NStbhhx(374) + MB2*MT2*NStbhhx(380)

	NStbhhx(403) = 40 + 40*TB*Yuk3 + 7*TB2*Yuk3**2

	NStbhhx(404) = -2 + 4*TB*Yuk3 - 11*TB2*Yuk3**2

	NStbhhx(414) = 96*TB2*Yuk3 + TB*(11 - 20*TB2*Yuk3**2)

	NStbhhx(415) = 80*TB2*Yuk3 - TB*(3 - 5*TB2*Yuk3**2)

	NStbhhx(416) = 324*TB2*Yuk3 + TB*(34 + 115*TB2*Yuk3**2)

	NStbhhx(422) = 1 + 7*SW2*(2 - TB2*Yuk3**2)

	NStbhhx(429) = SW2**2*(11 + TB2*Yuk3**2) + CW2*NStbhhx(428)

	NStbhhx(437) = SW2**2*(16 + TB2*Yuk3**2) + CW2*NStbhhx(436)

	NStbhhx(12) = 44*MB2 - (9*(2*MB2 - MHp2)*NStbhhl(6))/MHp2

	NStbhhx(136) = 4*TB + 3*TB2*(7*Yuk3 + TB*Yuk3**2)

	NStbhhx(137) = 4*TB + TB2*(3*Yuk3 + 4*TB*Yuk3**2)

	NStbhhx(138) = 5*TB - 2*TB2*(5*Yuk3 + 2*TB*Yuk3**2)

	NStbhhx(147) = MB2 - MHp2 + MT2 + 4*MB2*(TB*Yuk3)

	NStbhhx(160) = TB + TB2*(2*Yuk3 - TB*Yuk3**2)

	NStbhhx(162) = TB + TB2*(4*Yuk3 + TB*Yuk3**2)

	NStbhhx(164) = TB - TB2*(2*Yuk3 + TB*Yuk3**2)

	NStbhhx(165) = 5*TB + 3*TB2*(2*Yuk3 + TB*Yuk3**2)

	NStbhhx(169) = 3*TB + 2*TB2*(4*Yuk3 + TB*Yuk3**2)

	NStbhhx(171) = 3*TB + TB2*(4*Yuk3 + 3*TB*Yuk3**2)

	NStbhhx(172) = 2*TB + TB2*(8*Yuk3 + 3*TB*Yuk3**2)

	NStbhhx(201) = 6*MB2**2 + 5*MT2*MHp2 - MB2*(5*MHp2 + 2*MT2)

        NStbhhx(202) =
     &   MT2**2*((2*MB2 - MHp2)*TB) + MB2*MT2*(MHp2*TB2*Yuk3)

        NStbhhx(205) =
     &   5*MB2**7*(TB2*Yuk3**2) -
     &    MT2**2*((MHp2 - MT2)**3*NStbhhx(204))

        NStbhhx(208) =
     &   MT2*((3*MB2 - 13*MHp2 - 18*MT2)*TB) +
     &    TB2*Yuk3*NStbhhx(207)

        NStbhhx(212) =
     &   MT2*(TB*NStbhhx(210) - 8*TB2*Yuk3*NStbhhx(211)) +
     &    2*TB*TB2*Yuk3**2*NStbhhx(209)

        NStbhhx(217) =
     &   MT2**2*(MHp2**2*NStbhhx(214)) -
     &    MT2**3*(MHp2*NStbhhx(216)) - TB2*Yuk3*NStbhhx(213) -
     &    TB*NStbhhx(215)

        NStbhhx(222) =
     &   4*MT2**2*(MHp2**2*NStbhhx(219)) - TB2*Yuk3*NStbhhx(218) +
     &    2*NStbhhx(221)

        NStbhhx(229) =
     &   -3*MHp2**5*TB*TB2*Yuk3**2 + 3*MT2**5*NStbhhx(223) -
     &    4*MT2**3*(MHp2**2*NStbhhx(224)) -
     &    2*MT2**2*(MHp2**3*NStbhhx(225)) + NStbhhx(228)

	NStbhhx(253) = 1 + 2*SW2*(5 - 6*TB2*Yuk3**2)

	NStbhhx(258) = 472*TB2*Yuk3 + 3*TB*(6 + 13*TB2*Yuk3**2)

	NStbhhx(259) = 34*TB2*Yuk3 + TB*(39 + 56*TB2*Yuk3**2)

	NStbhhx(261) = -1 - 2*SW2*(4 - 3*TB2*Yuk3**2)

	NStbhhx(267) = 228*TB2*Yuk3 + 5*TB*(2 - 5*TB2*Yuk3**2)

	NStbhhx(269) = 78*TB2*Yuk3 + TB*(5 - 23*TB2*Yuk3**2)

	NStbhhx(270) = 628*TB2*Yuk3 + TB*(59 + 3*TB2*Yuk3**2)

	NStbhhx(272) = 4*TB2*Yuk3 - TB*(23 - 5*TB2*Yuk3**2)

	NStbhhx(273) = 100*TB2*Yuk3 + TB*(85 + TB2*Yuk3**2)

	NStbhhx(274) = 36*TB2*Yuk3 + TB*(95 + 3*TB2*Yuk3**2)

	NStbhhx(275) = 60*TB2*Yuk3 - TB*(75 + 7*TB2*Yuk3**2)

        NStbhhx(277) =
     &   2*MHp2**3*(32 + 5*TB*Yuk3) -
     &    MT2*(MHp2**2*(16 + 61*TB*Yuk3))

	NStbhhx(278) = 32*TB2*Yuk3 + TB*(57 - 40*TB2*Yuk3**2)

        NStbhhx(281) =
     &   -187 - 62*TB2*Yuk3**2 - 32*SW2**2*(16 - 23*TB2*Yuk3**2) +
     &    32*NStbhhx(280)

	NStbhhx(283) = 28*TB2*Yuk3 - TB*(10 - 11*TB2*Yuk3**2)

	NStbhhx(284) = 240*TB2*Yuk3 + TB*(51 + 53*TB2*Yuk3**2)

        NStbhhx(287) =
     &   MT2**3*(MHp2*NStbhhx(285)) + MT2*(MHp2**3*NStbhhx(286))

        NStbhhx(290) =
     &   -232 - 27*TB2*Yuk3**2 - 352*SW2**2*(1 - TB2*Yuk3**2) +
     &    32*NStbhhx(289)

        NStbhhx(298) =
     &   MHp2**4*TB2*Yuk3*(16 + 5*TB*Yuk3) +
     &    MT2**2*(MHp2**2*NStbhhx(292)) -
     &    8*MT2**3*(MHp2*NStbhhx(293)) -
     &    2*MT2*(MHp2**3*NStbhhx(294)) +
     &    MT2**4*(324*TB2*Yuk3 - TB*NStbhhx(297))

	NStbhhx(311) = MA02 + 1/(MB2 - MT2)*((MA02 - MHp2)*MHp2)

	NStbhhx(315) = (MB2 - MHp2)**2 - MT2*(MB2 + MHp2)

        NStbhhx(316) =
     &   (MB2 - MHp2)**2 - MT2*(MB2 + MHp2) +
     &    2*MB2*((MB2 - MHp2 - MT2)*TB*Yuk3)

        NStbhhx(320) =
     &   -5*MT2**3 - 3*MT2*(MB2 - MHp2)**2 + 8*MT2**2*MHp2 +
     &    MB2*NStbhhx(319)

	NStbhhx(386) = 23 + 14*TB*Yuk3 - 4*TB2*Yuk3**2

	NStbhhx(387) = -1 + 56*TB*Yuk3 - TB2*Yuk3**2

	NStbhhx(388) = -11 + 4*TB*Yuk3 - 2*TB2*Yuk3**2

	NStbhhx(389) = 46 + 40*TB*Yuk3 + 11*TB2*Yuk3**2

	NStbhhx(391) = -4 + 14*TB*Yuk3 + 23*TB2*Yuk3**2

	NStbhhx(392) = 5 + 8*TB*Yuk3 + 3*TB2*Yuk3**2

	NStbhhx(393) = 17 + 20*TB*Yuk3 + 17*TB2*Yuk3**2

	NStbhhx(395) = 1 + 28*TB*Yuk3 + 29*TB2*Yuk3**2

	NStbhhx(397) = 11 + 40*TB*Yuk3 + 46*TB2*Yuk3**2

	NStbhhx(398) = 7 + 40*(TB*Yuk3 + TB2*Yuk3**2)

	NStbhhx(400) = 29 + 28*TB*Yuk3 + TB2*Yuk3**2

	NStbhhx(401) = -5 + 4*TB*Yuk3 - 5*TB2*Yuk3**2

	NStbhhx(402) = 3 + 8*TB*Yuk3 + 5*TB2*Yuk3**2

        NStbhhx(405) =
     &   MT2**3*(MHp2*NStbhhx(403)) - MT2*(MHp2**3*NStbhhx(404))

	NStbhhx(408) = 36*TB2*Yuk3 + TB*(3 + 95*TB2*Yuk3**2)

	NStbhhx(410) = TB + TB2*(34*Yuk3 + 45*TB*Yuk3**2)

	NStbhhx(411) = 244*TB2*Yuk3 + TB*(27 + 200*TB2*Yuk3**2)

	NStbhhx(413) = 5*(MHp2 + MT2)*TB + 16*(MHp2 - MT2)*TB2*Yuk3

        NStbhhx(417) =
     &   50*MHp2**3*TB*TB2*Yuk3**2 +
     &    2*MT2**2*(MHp2*NStbhhx(414)) +
     &    MT2*(MHp2**2*NStbhhx(415)) + MT2**3*NStbhhx(416)

	NStbhhx(419) = 19*TB2*Yuk3 + 8*TB*(1 - 3*TB2*Yuk3**2)

	NStbhhx(420) = 14*TB2*Yuk3 - TB*(3 + 13*TB2*Yuk3**2)

	NStbhhx(421) = 240*TB2*Yuk3 + TB*(53 + 51*TB2*Yuk3**2)

        NStbhhx(423) =
     &   -94 - 155*TB2*Yuk3**2 - 32*SW2*(13 - 7*TB2*Yuk3**2) +
     &    224*SW2**2*(2 - TB2*Yuk3**2) + 32*CW2*NStbhhx(422)

	NStbhhx(425) = 28*TB2*Yuk3 + TB*(11 - 10*TB2*Yuk3**2)

	NStbhhx(426) = 78*TB2*Yuk3 - TB*(23 - 5*TB2*Yuk3**2)

	NStbhhx(427) = 22*TB2*Yuk3 + TB*(11 + 5*TB2*Yuk3**2)

        NStbhhx(430) =
     &   -7 - 10*TB2*Yuk3**2 - 32*SW2*(10 + TB2*Yuk3**2) +
     &    32*NStbhhx(429)

	NStbhhx(432) = 48*TB2*Yuk3 - TB*(53 - 163*TB2*Yuk3**2)

	NStbhhx(433) = 32*TB2*Yuk3 - TB*(40 - 57*TB2*Yuk3**2)

	NStbhhx(434) = 4*TB2*Yuk3 + TB*(5 - 23*TB2*Yuk3**2)

	NStbhhx(435) = 68*TB2*Yuk3 + 5*TB*(5 - 9*TB2*Yuk3**2)

        NStbhhx(438) =
     &   -67 - 59*TB2*Yuk3**2 - 32*SW2*(14 + TB2*Yuk3**2) +
     &    32*NStbhhx(437)

	NStbhhx(65) = 19*MHp2**2 - 24*MT2**2 - 88*MT2*MHp2

	NStbhhx(69) = MHp2**2 + 17*MT2**2 - 6*MT2*MHp2

        NStbhhx(72) = 13*MHp2 -
     &    8/MT2*((MA02 - MHp2 - 2*MT2)*NStbhhl(1))

	NStbhhx(74) = MA02*(MHp2 - 6*MT2) + 2*MT2*MHp2

	NStbhhx(76) = 5*MHp2 + 2/MT2*((3*MA02 - 2*MHp2)*NStbhhl(1))

	NStbhhx(101) = -2 + 2*TB*Yuk3 + TB2*Yuk3**2

	NStbhhx(102) = 14 + 12*TB*Yuk3 - TB2*Yuk3**2

	NStbhhx(103) = 11 + 4*TB*Yuk3 + 2*TB2*Yuk3**2

	NStbhhx(104) = 38 + 48*TB*Yuk3 + 13*TB2*Yuk3**2

	NStbhhx(106) = -5 + 2*TB*Yuk3 + TB2*Yuk3**2

	NStbhhx(107) = 1 + 40*TB*Yuk3 + 13*TB2*Yuk3**2

	NStbhhx(108) = 21 + 8*TB*Yuk3 + 15*TB2*Yuk3**2

	NStbhhx(109) = 3 + 40*TB*Yuk3 + 19*TB2*Yuk3**2

	NStbhhx(111) = 14 + 24*TB*Yuk3 - 17*TB2*Yuk3**2

	NStbhhx(112) = -1 + 4*TB*Yuk3 + 2*TB2*Yuk3**2

	NStbhhx(113) = 19 + 216*TB*Yuk3 + 34*TB2*Yuk3**2

	NStbhhx(116) = 5*MHp2**2 - 2*MT2**2 - MT2*MHp2

	NStbhhx(118) = TB + 2*TB2*(Yuk3 - 2*TB*Yuk3**2)

	NStbhhx(120) = 5*TB + TB2*(6*Yuk3 - 5*TB*Yuk3**2)

	NStbhhx(121) = 11*TB - TB2*(16*Yuk3 - TB*Yuk3**2)

	NStbhhx(122) = 4*TB - TB2*(2*Yuk3 + TB*Yuk3**2)

	NStbhhx(123) = 11*TB + TB2*(6*Yuk3 + 5*TB*Yuk3**2)

	NStbhhx(125) = 3*TB - TB2*(11*Yuk3 - 3*TB*Yuk3**2)

	NStbhhx(126) = 19*TB - 4*TB2*(Yuk3 + 4*TB*Yuk3**2)

        NStbhhx(128) =
     &   8*MHp2**4*TB*Yuk3**2 + 5*MT2**4*(Yuk3*(4 + TB*Yuk3))

	NStbhhx(129) = 21*TB2*Yuk3 + TB*(3 + 4*TB2*Yuk3**2)

	NStbhhx(13) = 24*MB2**2 - 19*MHp2**2 + 2*MHp2*NStbhhx(12)

	NStbhhx(132) = 4*TB + 5*TB2*(2*Yuk3 - TB*Yuk3**2)

	NStbhhx(134) = 16*TB + TB2*(4*Yuk3 - 19*TB*Yuk3**2)

        NStbhhx(135) =
     &   MHp2**5*TB2*Yuk3**2 - 4*MT2**2*(MHp2**3*(1 - TB2*Yuk3**2))

        NStbhhx(139) =
     &   MT2**3*(MHp2**2*NStbhhx(136)) +
     &    MT2*(MHp2**4*NStbhhx(137)) + MT2**5*NStbhhx(138)

	NStbhhx(145) = MB2 - 2*MHp2 + MT2 + 4*MB2*(TB*Yuk3)

        NStbhhx(148) =
     &   MB2*((MB2 - MHp2 + MT2)*TB2*Yuk3**2) + MT2*NStbhhx(147)

	NStbhhx(152) = MT2*(MHp2 - MT2) - MB2**2*(TB2*Yuk3**2)

        NStbhhx(157) =
     &   2*MA02 - MHp2 - 1/(MB2 - MT2)*((MA02 - MHp2)*MHp2)

        NStbhhx(159) =
     &   MT2*((MHp2 - MT2)**2*(2*MHp2 - MT2)) -
     &    MB2**4*(TB2*Yuk3**2)

	NStbhhx(161) = -4*MHp2*TB*TB2*Yuk3**2 + MT2*NStbhhx(160)

        NStbhhx(166) =
     &   -2*MHp2**2*TB*TB2*Yuk3**2 + MT2**2*NStbhhx(164) +
     &    MT2*(MHp2*NStbhhx(165))

	NStbhhx(168) = MT2*(MHp2 - MT2)**2 + MB2**3*(TB2*Yuk3**2)

	NStbhhx(170) = -6*MHp2*TB*TB2*Yuk3**2 + MT2*NStbhhx(169)

        NStbhhx(173) =
     &   3*MHp2**2*TB*TB2*Yuk3**2 - 2*MT2*(MHp2*NStbhhx(171)) +
     &    MT2**2*NStbhhx(172)

	NStbhhx(179) = 5*TB + TB2*(40*Yuk3 + 19*TB*Yuk3**2)

	NStbhhx(181) = TB + 2*TB2*(4*Yuk3 + 3*TB*Yuk3**2)

	NStbhhx(182) = 48*TB2*Yuk3 + 31*TB*(1 + TB2*Yuk3**2)

	NStbhhx(184) = 24*TB2*Yuk3 + 11*TB*(1 + TB2*Yuk3**2)

	NStbhhx(185) = 8*TB2*Yuk3 + TB*(6 + TB2*Yuk3**2)

	NStbhhx(186) = 40*TB2*Yuk3 + TB*(19 + 5*TB2*Yuk3**2)

	NStbhhx(190) = 5*TB + TB2*(24*Yuk3 + 5*TB*Yuk3**2)

	NStbhhx(194) = 4*(MB2 - MT2)*Yuk3 + (3*MB2 - MT2)*(TB*Yuk3**2)

        NStbhhx(203) =
     &   -(MB2*(TB*TB2*Yuk3**2*NStbhhx(201))) + 2*NStbhhx(202)

        NStbhhx(230) =
     &   MB2**5*NStbhhx(208) + MB2**4*NStbhhx(212) +
     &    MB2*MT2*((MHp2 - MT2)*NStbhhx(217)) +
     &    MB2**3*NStbhhx(222) - MB2**2*NStbhhx(229) -
     &    TB*NStbhhx(205)

	NStbhhx(234) = 40*MHp2**2 - 37*MT2**2 + 61*MT2*MHp2

	NStbhhx(236) = 252*TB2*Yuk3 + TB*(7 - 106*TB2*Yuk3**2)

	NStbhhx(238) = 500*TB2*Yuk3 - TB*(298 - 61*TB2*Yuk3**2)

	NStbhhx(239) = 252*TB2*Yuk3 - TB*(106 - 7*TB2*Yuk3**2)

	NStbhhx(240) = 252*TB2*Yuk3 - TB*(110 + TB2*Yuk3**2)

	NStbhhx(241) = 500*TB2*Yuk3 + 7*TB*(54 + 11*TB2*Yuk3**2)

	NStbhhx(243) = 188*TB2*Yuk3 + TB*(17 - 101*TB2*Yuk3**2)

	NStbhhx(244) = 1004*TB2*Yuk3 - TB*(58 + 175*TB2*Yuk3**2)

	NStbhhx(246) = 472*TB2*Yuk3 + 3*TB*(13 + 6*TB2*Yuk3**2)

	NStbhhx(247) = 500*TB2*Yuk3 + TB*(69 + 40*TB2*Yuk3**2)

	NStbhhx(248) = 1752*TB2*Yuk3 - TB*(211 + 252*TB2*Yuk3**2)

        NStbhhx(250) =
     &   59*MHp2**4*TB2*Yuk3**2 -
     &    216*MT2**3*(MHp2*(1 - TB2*Yuk3**2))

	NStbhhx(251) = 34*TB2*Yuk3 + TB*(56 + 39*TB2*Yuk3**2)

	NStbhhx(252) = 188*TB2*Yuk3 + TB*(19 + 67*TB2*Yuk3**2)

        NStbhhx(254) =
     &   188 + 211*TB2*Yuk3**2 + 128*SW2**2*(5 - 6*TB2*Yuk3**2) -
     &    192*SW2*(3 - 4*TB2*Yuk3**2) + 64*CW2*NStbhhx(253)

        NStbhhx(256) =
     &   10*MHp2**5*TB2*Yuk3**2 -
     &    121*MT2**2*(MHp2**3*(1 - TB2*Yuk3**2))

	NStbhhx(257) = 252*TB2*Yuk3 - TB*(25 + 86*TB2*Yuk3**2)

        NStbhhx(260) =
     &   MT2**4*(MHp2*NStbhhx(258)) + MT2**3*(MHp2**2*NStbhhx(259))

        NStbhhx(262) =
     &   -111 - 58*TB2*Yuk3**2 - 128*SW2**2*(4 - 3*TB2*Yuk3**2) +
     &    64*(SW2*(7 - 6*TB2*Yuk3**2) + CW2*NStbhhx(261))

	NStbhhx(265) = 2*MT2*Yuk3 + 5*(MB2 - MT2)*(TB*Yuk3**2)

        NStbhhx(268) =
     &   4*MHp2*TB2*Yuk3*(32 - 5*TB*Yuk3) + MT2*NStbhhx(267)

        NStbhhx(271) =
     &   192*MHp2**2*TB2*Yuk3 + 4*MT2*(MHp2*NStbhhx(269)) +
     &    MT2**2*NStbhhx(270)

        NStbhhx(276) =
     &   -(MT2**2*(MHp2*NStbhhx(273))) + MT2**3*NStbhhx(274) +
     &    MT2*(MHp2**2*NStbhhx(275)) + MHp2**3*NStbhhx(272)

        NStbhhx(282) =
     &   4*MT2**2*(MHp2*NStbhhx(278)) +
     &    MT2**3*(872*TB2*Yuk3 - TB*NStbhhx(281)) +
     &    2*TB2*Yuk3*NStbhhx(277)

        NStbhhx(291) =
     &   2*MT2**2*(MHp2**2*NStbhhx(284)) +
     &    MT2**4*(244*TB2*Yuk3 - TB*NStbhhx(290)) +
     &    MHp2**4*NStbhhx(283) - 4*NStbhhx(287)

        NStbhhx(299) =
     &   MB2**7*(TB2*Yuk3*(16 - 5*TB*Yuk3)) + MB2**3*NStbhhx(298)

	NStbhhx(305) = TB + 6*TB2*(2*Yuk3 + TB*Yuk3**2)

	NStbhhx(307) = 6*TB - TB2*(8*Yuk3 + 7*TB*Yuk3**2)

        NStbhhx(317) =
     &   MB2*(TB2*Yuk3**2*NStbhhx(315)) + MT2*NStbhhx(316)

        NStbhhx(385) =
     &   3*MHp2**2 - 4*MHp2*(MT2 + 2*MT2*(TB*Yuk3)) +
     &    MT2**2*(7 + 8*TB*Yuk3)

        NStbhhx(390) =
     &   3*MHp2**4*TB2*Yuk3**2 - 4*MT2**3*(MHp2*NStbhhx(386)) +
     &    2*MT2**2*(MHp2**2*NStbhhx(387)) -
     &    2*MT2*(MHp2**3*NStbhhx(388)) + MT2**4*NStbhhx(389)

        NStbhhx(394) =
     &   MHp2**3*Yuk3*(4*TB + 5*TB2*Yuk3) +
     &    2*MT2**2*(MHp2*NStbhhx(391)) +
     &    3*MT2*(MHp2**2*NStbhhx(392)) + MT2**3*NStbhhx(393)

        NStbhhx(396) =
     &   6*MHp2*Yuk3*(4*TB + 3*TB2*Yuk3) + MT2*NStbhhx(395)

        NStbhhx(399) =
     &   6*MHp2**2*Yuk3*(4*TB + 3*TB2*Yuk3) +
     &    MT2**2*NStbhhx(397) + 2*MT2*(MHp2*NStbhhx(398))

        NStbhhx(406) =
     &   MT2**4*NStbhhx(400) + 6*MT2**2*(MHp2**2*NStbhhx(402)) -
     &    MHp2**4*NStbhhx(401) - 2*NStbhhx(405)

	NStbhhx(409) = 84*MHp2*TB*TB2*Yuk3**2 + MT2*NStbhhx(408)

        NStbhhx(412) =
     &   138*MHp2**2*TB*TB2*Yuk3**2 + 4*MT2*(MHp2*NStbhhx(410)) +
     &    MT2**2*NStbhhx(411)

        NStbhhx(418) =
     &   MT2**3*((MHp2 - MT2)**3*NStbhhx(413)) +
     &    MB2**4*NStbhhx(417)

        NStbhhx(424) =
     &   27*MHp2**4*TB*TB2*Yuk3**2 +
     &    16*MT2**3*(MHp2*NStbhhx(419)) +
     &    4*MT2*(MHp2**3*NStbhhx(420)) +
     &    2*MT2**2*(MHp2**2*NStbhhx(421)) +
     &    MT2**4*(872*TB2*Yuk3 - TB*NStbhhx(423))

        NStbhhx(431) =
     &   -(2*MT2**2*(MHp2**2*(61*TB + 16*TB2*Yuk3))) -
     &    4*MT2**3*(MHp2*NStbhhx(426)) +
     &    4*MT2*(MHp2**3*NStbhhx(427)) +
     &    MT2**4*(228*TB2*Yuk3 - TB*NStbhhx(430)) +
     &    MHp2**4*NStbhhx(425)

        NStbhhx(439) =
     &   -(2*MT2**2*(MHp2**2*NStbhhx(432))) +
     &    4*MT2**3*(MHp2*NStbhhx(433)) -
     &    4*MT2*(MHp2**3*NStbhhx(435)) -
     &    MT2**4*(628*TB2*Yuk3 - TB*NStbhhx(438)) +
     &    MHp2**4*NStbhhx(434)

	NStbhhx(44) = -2*MT2*(3*MB2 + MT2) + (MB2 + MT2)*MA02

	NStbhhx(54) = 7*TB + 2*TB2*(4*Yuk3 - 3*TB*Yuk3**2)

	NStbhhx(56) = TB + TB2*(8*Yuk3 + TB*Yuk3**2)

	NStbhhx(57) = 6*TB + TB2*(12*Yuk3 + TB*Yuk3**2)

	NStbhhx(66) = 27*MHp2**2 - 4/MT2*(NStbhhl(1)*NStbhhx(65))

	NStbhhx(67) = 11*MHp2 + 12/MT2**2*((MHp2 - MT2)*NStbhhl(1)**2)

	NStbhhx(68) = 2*MHp2**2 + 6*MT2**2 - 5*MT2*MHp2

	NStbhhx(70) = 9*(MHp2 - 2*MT2)*NStbhhl(1) + 2*NStbhhx(69)

        NStbhhx(73) = 9*MA02**2 + MA02*(3*MHp2 + 2*MT2) -
     &    16*MT2**2*(2 + 1/MT2**2*NStbhhl(1)**2) -
     &    2*MT2*NStbhhx(72)

        NStbhhx(75) = MA02*(MA02 - 2*MHp2 + 4*MT2)*
     &     NStbhhl(17) + NStbhhl(19)*NStbhhx(74)

	NStbhhx(77) = MA02*(MHp2 + 4*MT2) - 2*MT2*NStbhhx(76)

	NStbhhx(78) = 3*MA02 - 2*(MHp2 + MT2*(3 + 2/MT2*NStbhhl(1)))

        NStbhhx(85) = -5*MT2**2 + 5*MB2**2*(TB2*Yuk3**2) +
     &    MB2*(MT2 - MT2*(TB2*Yuk3**2))

        NStbhhx(86) = -3*MT2**2 + 3*MB2**2*(TB2*Yuk3**2) +
     &    MB2*(MT2 - MT2*(TB2*Yuk3**2))

        NStbhhx(87) = -3*MT2**3 + 3*MB2**3*(TB2*Yuk3**2) -
     &    MB2*MT2**2*(4 + TB2*Yuk3**2) +
     &    MB2**2*(MT2 + 4*MT2*(TB2*Yuk3**2))

	NStbhhx(93) = 19 + 72*TB*Yuk3 - 25*TB2*Yuk3**2

	NStbhhx(94) = -1 + 10*TB*Yuk3 + 5*TB2*Yuk3**2

	NStbhhx(96) = -19 + 68*TB*Yuk3 + 21*TB2*Yuk3**2

	NStbhhx(97) = 17 + 22*TB*Yuk3 - 10*TB2*Yuk3**2

	NStbhhx(98) = -1 + 2*TB*Yuk3 + TB2*Yuk3**2

	NStbhhx(99) = 30 + 16*TB*Yuk3 - TB2*Yuk3**2

        NStbhhx(10) = -11*MHp2 +
     &    12/MB2**2*((MB2 - MHp2)*NStbhhl(2)**2)

        NStbhhx(100) =
     &   2*MHp2**4*Yuk3*(2*TB + 9*TB2*Yuk3) -
     &    MT2**3*(MHp2*NStbhhx(96)) -
     &    2*MT2**2*(MHp2**2*NStbhhx(97)) - 20*MT2**4*NStbhhx(98) -
     &    MT2*(MHp2**3*NStbhhx(99))

        NStbhhx(105) =
     &   7*MHp2**5*TB2*Yuk3**2 - 28*MT2*MHp2**4 -
     &    20*MT2**5*NStbhhx(101) -
     &    4*MT2**3*(MHp2**2*NStbhhx(102)) -
     &    2*MT2**2*(MHp2**3*NStbhhx(103)) +
     &    MT2**4*(MHp2*NStbhhx(104))

	NStbhhx(11) = 17*MB2**2 + MHp2**2 - 6*MB2*MHp2

        NStbhhx(110) =
     &   5*MHp2**4*(1 - TB2*Yuk3**2) - 4*MT2**4*NStbhhx(106) +
     &    MT2**3*(MHp2*NStbhhx(107)) +
     &    MT2*(MHp2**3*NStbhhx(108)) -
     &    MT2**2*(MHp2**2*NStbhhx(109))

        NStbhhx(114) =
     &   24*MHp2**3*Yuk3*(TB + 3*TB2*Yuk3) -
     &    4*MT2*(MHp2**2*NStbhhx(111)) - 20*MT2**3*NStbhhx(112) -
     &    MT2**2*(MHp2*NStbhhx(113))

        NStbhhx(117) =
     &   -(2*MB2**7*(TB2*Yuk3**2)) +
     &    MT2**2*((MHp2 - MT2)**3*NStbhhx(116))

	NStbhhx(119) = -5*MHp2*TB*TB2*Yuk3**2 + 2*MT2*NStbhhx(118)

        NStbhhx(124) =
     &   3*MHp2**4*TB*(1 - TB2*Yuk3**2) +
     &    MT2**3*(MHp2*NStbhhx(120)) +
     &    MT2**2*(MHp2**2*NStbhhx(121)) - 2*MT2**4*NStbhhx(122) +
     &    MT2*(MHp2**3*NStbhhx(123))

        NStbhhx(127) =
     &   16*MHp2**3*TB*TB2*Yuk3**2 + 10*MT2**3*(TB + 4*TB2*Yuk3) +
     &    2*MT2*(MHp2**2*NStbhhx(125)) + MT2**2*(MHp2*NStbhhx(126))

        NStbhhx(130) =
     &   MT2*(MHp2**3*(2*TB + 11*TB2*Yuk3)) -
     &    10*MT2**3*(MHp2*TB*(1 - TB2*Yuk3**2)) +
     &    MT2**2*(MHp2**2*NStbhhx(129)) - TB2*NStbhhx(128)

	NStbhhx(131) = 7*TB - TB2*(2*Yuk3 + 13*TB*Yuk3**2)

	NStbhhx(133) = MHp2**2*TB*TB2*Yuk3**2 + MT2**2*NStbhhx(132)

	NStbhhx(14) = 27*MHp2**2 + 4/MB2*(NStbhhl(2)*NStbhhx(13))

        NStbhhx(140) =
     &   MT2**4*(MHp2*NStbhhx(134)) + 5*TB*NStbhhx(135) -
     &    2*NStbhhx(139)

        NStbhhx(143) =
     &   1/(MB2 - MT2)*((MA02 - MB2 - MT2)*
     &       (MA02 + MB2 - MHp2 - MT2)) - 4*MB2*(TB*Yuk3)

        NStbhhx(146) =
     &   MB2*((MB2 - 2*MHp2 + MT2)*TB2*Yuk3**2) + MT2*NStbhhx(145)

        NStbhhx(151) =
     &   (MA02 - MB2 - MHp2 + MT2)*NStbhhl(1) -
     &    (MA02 + MB2 - MHp2 - MT2)*NStbhhl(2)

        NStbhhx(16) = MA02 -
     &    4/MB2*((3*MA02 - 8*MB2)*NStbhhl(2)) - 24*NStbhhl(14)

	NStbhhx(17) = 3*MA02 - 2*MB2*(5 - 1/MB2*NStbhhl(2))

        NStbhhx(174) =
     &   MB2**2*NStbhhx(170) + MB2*NStbhhx(173) + 3*TB*NStbhhx(168)

        NStbhhx(178) =
     &   MT2*((MHp2 - MT2)**2*(8*MHp2 + 9*MT2)) +
     &    9*MB2**4*(TB2*Yuk3**2)

	NStbhhx(180) = -10*MHp2*TB*TB2*Yuk3**2 + MT2*NStbhhx(179)

        NStbhhx(183) =
     &   7*MHp2**2*TB*TB2*Yuk3**2 + 2*MT2*(MHp2*NStbhhx(181)) -
     &    MT2**2*NStbhhx(182)

        NStbhhx(187) =
     &   8*MHp2**3*TB*TB2*Yuk3**2 - MT2*(MHp2**2*NStbhhx(184)) -
     &    2*MT2**2*(MHp2*NStbhhx(185)) + MT2**3*NStbhhx(186)

	NStbhhx(189) = -(MT2*(MHp2 - 3*MT2)) + 3*MB2**2*(TB2*Yuk3**2)

	NStbhhx(191) = -3*MHp2*TB*TB2*Yuk3**2 + MT2*NStbhhx(190)

	NStbhhx(195) = (3*MB2 - MT2)*MT2*TB - MB2*(TB2*NStbhhx(194))

	NStbhhx(198) = 2*MT2 - MHp2*(5 - 2*TB*Yuk3)

        NStbhhx(235) =
     &   -(37*MB2**7*(TB2*Yuk3**2)) +
     &    MT2**2*((MHp2 - MT2)**3*NStbhhx(234))

	NStbhhx(237) = -172*MHp2*TB*TB2*Yuk3**2 + MT2*NStbhhx(236)

	NStbhhx(24) = -MT2**3 + MB2**3*(TB2*Yuk3**2)

        NStbhhx(242) =
     &   24*MHp2**4*TB*(1 - TB2*Yuk3**2) -
     &    MT2**3*(MHp2*NStbhhx(238)) + MT2**4*NStbhhx(239) -
     &    MT2*(MHp2**3*NStbhhx(240)) +
     &    MT2**2*(MHp2**2*NStbhhx(241))

        NStbhhx(245) =
     &   254*MHp2**2*TB*TB2*Yuk3**2 - 4*MT2*(MHp2*NStbhhx(243)) -
     &    MT2**2*NStbhhx(244)

        NStbhhx(249) =
     &   100*MHp2**3*TB*TB2*Yuk3**2 -
     &    4*MT2**2*(MHp2*NStbhhx(246)) -
     &    2*MT2*(MHp2**2*NStbhhx(247)) - MT2**3*NStbhhx(248)

	NStbhhx(25) = 5*TB - 2*TB2*(2*Yuk3 - 31*TB*Yuk3**2)

        NStbhhx(255) =
     &   -(4*MT2**2*(MHp2**2*NStbhhx(251))) +
     &    4*MT2*(MHp2**3*NStbhhx(252)) +
     &    MT2**4*(1752*TB2*Yuk3 - TB*NStbhhx(254)) +
     &    TB*NStbhhx(250)

	NStbhhx(26) = 62*TB - TB2*(4*Yuk3 - 5*TB*Yuk3**2)

        NStbhhx(263) =
     &   MT2*(MHp2**4*NStbhhx(257)) +
     &    MT2**5*(1004*TB2*Yuk3 + TB*NStbhhx(262)) +
     &    4*TB*NStbhhx(256) - 4*NStbhhx(260)

	NStbhhx(266) = 2*MT2**2*TB - MB2*(TB2*NStbhhx(265))

	NStbhhx(28) = 3*TB + 2*TB2*(2*Yuk3 + TB*Yuk3**2)

	NStbhhx(29) = 2*TB + TB2*(4*Yuk3 + 3*TB*Yuk3**2)

        NStbhhx(300) =
     &   MT2**3*((27*MHp2 - 19*MT2)*(MHp2 - MT2)**3*TB) +
     &    MB2**6*NStbhhx(268) - MB2**5*NStbhhx(271) +
     &    MB2*MT2**2*((MHp2 - MT2)*NStbhhx(276)) +
     &    MB2**4*NStbhhx(282) + MB2**2*MT2*NStbhhx(291) -
     &    2*NStbhhx(299)

        NStbhhx(304) =
     &   MHp2**2*(TB + 4*TB2*Yuk3) - 2*MT2*(MHp2*TB) -
     &    MT2**2*(7*TB + 4*TB2*Yuk3)

	NStbhhx(306) = -2*MHp2*TB*TB2*Yuk3**2 + MT2*NStbhhx(305)

        NStbhhx(308) =
     &   MHp2**2*TB*TB2*Yuk3**2 - 2*MT2*(MHp2*NStbhhx(56)) +
     &    MT2**2*NStbhhx(307)

	NStbhhx(31) = 35*TB + TB2*(4*Yuk3 + 3*TB*Yuk3**2)

	NStbhhx(32) = 3*TB + TB2*(4*Yuk3 + 35*TB*Yuk3**2)

        NStbhhx(33) = -9*MT2**4 + 9*MB2**4*(TB2*Yuk3**2) -
     &    7*MB2**2*MT2**2*(1 - TB2*Yuk3**2)

	NStbhhx(37) = (MB2 + MT2)*TB + 4*MB2*(TB2*Yuk3)

        NStbhhx(39) = 4*MT2**2 + MB2**2*(TB*Yuk3) -
     &    MB2*MT2*(4 + 3*TB*Yuk3)

        NStbhhx(407) =
     &   MB2**6*(Yuk3*(8*TB + 7*TB2*Yuk3)) +
     &    MT2**2*((MHp2 - MT2)**2*NStbhhx(385)) +
     &    MB2**2*NStbhhx(390) - 2*MB2**3*NStbhhx(394) -
     &    MB2**5*NStbhhx(396) + MB2**4*NStbhhx(399) -
     &    MB2*MT2*NStbhhx(406)

	NStbhhx(42) = 4*MT2 + (MB2 + 3*MT2)*(TB*Yuk3)

        NStbhhx(440) =
     &   19*MB2**7*(TB*TB2*Yuk3**2) - MB2**6*NStbhhx(409) +
     &    MB2**5*NStbhhx(412) + MB2**3*NStbhhx(424) +
     &    MB2*MT2**2*NStbhhx(431) + MB2**2*MT2*NStbhhx(439) -
     &    2*NStbhhx(418)

	NStbhhx(45) = 2*MB2*((MA02 - 4*MT2)*TB2*Yuk3) + TB*NStbhhx(44)

        NStbhhx(463) =
     &   2*Re(NStbhhl(1)) + MA02*Re(NStbhhl(12)) +
     &    (MHp2 - 4*MT2)*Re(NStbhhl(13))

	NStbhhx(466) = Re(NStbhhl(7))/MA02 - Re(NStbhhl(12))

	NStbhhx(469) = Re(NStbhhl(7))/MA02 - Re(NStbhhl(8))

        NStbhhx(472) =
     &   2*Re(NStbhhl(2)) - Re(NStbhhl(7)) + MA02*Re(NStbhhl(8)) -
     &    (4*MB2 - MHp2)*Re(NStbhhl(9))

	NStbhhx(477) = 3*MB2**2 - 4*MB2*MT2 + MT2**2

        NStbhhx(5) = (MA02 - MHp2)*(MA02 - 2*MB2 - MHp2) +
     &    (3*MA02 - 2*MHp2)*NStbhhl(6)

	NStbhhx(55) = 2*MHp2*TB*TB2*Yuk3**2 + MT2*NStbhhx(54)

        NStbhhx(58) = MHp2**2*TB2*Yuk3*(4 + TB*Yuk3) -
     &    2*MT2*(MHp2*NStbhhx(56)) + MT2**2*NStbhhx(57)

        NStbhhx(62) = MA02 -
     &    4/MT2*((3*MA02 - 8*MT2)*NStbhhl(1)) - 24*NStbhhl(17)

	NStbhhx(63) = 3*MA02 - 2*MT2*(5 - 1/MT2*NStbhhl(1))

        NStbhhx(71) = 96*MT2**3 + MT2*NStbhhx(66) -
     &    4*MT2**2*NStbhhx(67) - 8*NStbhhl(19)*NStbhhx(68) +
     &    8*(MHp2**3 + NStbhhl(6)*NStbhhx(70))

        NStbhhx(79) = (MA02 - MHp2)*NStbhhx(73) -
     &    8*NStbhhx(75) - 4*
     &     (NStbhhl(6)*NStbhhx(77) - MA02*NStbhhl(7)*NStbhhx(78))

	NStbhhx(82) = -4*(3*MB2 - MT2)*MT2 + (MB2 + MT2)*MA02

        NStbhhx(88) = 16*MB2*MT2*NStbhhx(86) +
     &    MA02**2*NStbhhx(85) - 4*MA02*NStbhhx(87)

	NStbhhx(9) = 6*MB2**2 + 2*MHp2**2 - 5*MB2*MHp2

	NStbhhx(91) = 7*MHp2**2 + 4*MT2**2 - 7*MT2*MHp2

	NStbhhx(92) = 8*(MHp2 - MT2)*TB + (21*MHp2 - 4*MT2)*TB2*Yuk3

        NStbhhx(95) = 8*MHp2**2*Yuk3*(3*TB + 8*TB2*Yuk3) -
     &    MT2*(MHp2*NStbhhx(93)) - 4*MT2**2*NStbhhx(94)

	NStbhhx(1) = 3*MA02**2 - 16*MB2**2 + 14*MB2*MA02

        NStbhhx(115) =
     &   MT2**2*((MHp2 - MT2)**3*NStbhhx(91)) -
     &    MB2**6*(Yuk3*NStbhhx(92)) + MB2**5*NStbhhx(95) +
     &    2*MB2**3*NStbhhx(100) - MB2**2*NStbhhx(105) -
     &    MB2*MT2*((MHp2 - MT2)*NStbhhx(110)) - MB2**4*NStbhhx(114)

        NStbhhx(141) =
     &   -(MB2**6*NStbhhx(119)) -
     &    MB2*MT2*((MHp2 - MT2)*NStbhhx(124)) -
     &    MB2**4*NStbhhx(127) - 2*MB2**3*NStbhhx(130) +
     &    MB2**5*(MT2*(MHp2*NStbhhx(131)) + 2*NStbhhx(133)) -
     &    MB2**2*NStbhhx(140) + TB*NStbhhx(117)

        NStbhhx(144) =
     &   MB2/(MB2 - MT2)*((MA02 - MB2 - MT2)*
     &       (MA02 - MB2 - MHp2 + MT2)*TB2*Yuk3**2) -
     &    MT2*NStbhhx(143)

        NStbhhx(15) = 96*MB2**3 + 8*MHp2**3 +
     &    4*MB2**2*NStbhhx(10) + MB2*NStbhhx(14) -
     &    8*NStbhhl(15)*NStbhhx(9) + 16*NStbhhl(6)*NStbhhx(11)

	NStbhhx(153) = TB + TB2*(2*Yuk3 + TB*Yuk3**2)

        NStbhhx(177) =
     &   MA02*(MA02 - 2*(MT2 + MB2*(TB*Yuk3)))*NStbhhl(17) -
     &    MHp2*(MHp2 - 2*(MT2 + MB2*(TB*Yuk3)))*NStbhhl(19)

        NStbhhx(18) = 5*MA02**2 - MB2*NStbhhx(16) -
     &    4*(MB2**2*(7 + 2/MB2**2*NStbhhl(2)**2) +
     &       MA02*(2*NStbhhl(14) - (NStbhhl(7)*NStbhhx(17))/MA02))

        NStbhhx(188) =
     &   MB2**3*NStbhhx(180) - MB2**2*NStbhhx(183) +
     &    MB2*NStbhhx(187) + TB*NStbhhx(178)

        NStbhhx(192) =
     &   MA02**2*TB*(MT2 + MB2*(TB2*Yuk3**2)) -
     &    MA02*(MB2*NStbhhx(191) + 3*TB*NStbhhx(189))

        NStbhhx(196) =
     &   -((32*(MB2 - MT2)*MT2**2)/(MA02 - 4*MT2)) +
     &    NStbhhx(195)/TB

        NStbhhx(197) =
     &   5*MT2**2 - 2*MB2**2*(TB2*Yuk3**2) -
     &    MB2*MT2*(5 - 2*TB*Yuk3)

        NStbhhx(199) =
     &   MT2**2*(5*MHp2 - 6*MT2) -
     &    2*MB2**2*((MHp2 - 2*MT2)*TB2*Yuk3**2) +
     &    MB2*MT2*NStbhhx(198)

	NStbhhx(2) = MA02**2 + 2*MA02*(2*MB2 - MHp2)

	NStbhhx(21) = 4*MB2*(MB2 - 3*MT2) + (MB2 + MT2)*MA02

        NStbhhx(23) = -9*MT2**2 + 9*MB2**2*(TB2*Yuk3**2) +
     &    MB2*(MT2 - MT2*(TB2*Yuk3**2))

        NStbhhx(264) =
     &   -(MB2**6*NStbhhx(237)) -
     &    MB2*MT2*((MHp2 - MT2)*NStbhhx(242)) -
     &    MB2**5*NStbhhx(245) + MB2**4*NStbhhx(249) +
     &    MB2**3*NStbhhx(255) - MB2**2*NStbhhx(263) +
     &    TB*NStbhhx(235)

        NStbhhx(27) = MB2**2*MT2*NStbhhx(25) -
     &    MB2*MT2**2*NStbhhx(26) - 33*TB*NStbhhx(24)

	NStbhhx(3) = MA02*(6*MB2 - MHp2) - 2*MB2*MHp2

        NStbhhx(30) = MB2**2*MT2*NStbhhx(28) -
     &    MB2*MT2**2*NStbhhx(29) + 9*TB*NStbhhx(24)

        NStbhhx(301) =
     &   2*NStbhhl(2)**2*NStbhhx(266) +
     &    (NStbhhl(1)*NStbhhx(300))/
     &     ((MB - MHp - MT)**2*(MB + MHp - MT)**2*
     &       (MB - MHp + MT)**2*(MB + MHp + MT)**2)

	NStbhhx(303) = MB2*(TB*TB2*Yuk3**2) + MT2*(TB + 4*TB2*Yuk3)

        NStbhhx(309) =
     &   MB2**3*(TB*TB2*Yuk3**2) + MT2*NStbhhx(304) +
     &    MB2**2*NStbhhx(306) + MB2*NStbhhx(308)

        NStbhhx(34) = MB2*MT2**3*NStbhhx(31) -
     &    MB2**3*MT2*NStbhhx(32) - TB*NStbhhx(33)

        NStbhhx(38) = MB2*(MB2 + MT2)*(TB*TB2*Yuk3**2) +
     &    MT2*NStbhhx(37)

	NStbhhx(4) = 2*MA02 + 1/MB2**2*((MA02 - MHp2)*NStbhhl(2)**2)

        NStbhhx(40) = ((MB2 - 3*MT2)*MT2)/TB2 +
     &    32*MB2**2*(MB2 - MT2)*Yuk3**2/(MA02 - 4*MB2) -
     &    (Yuk3*NStbhhx(39))/TB

        NStbhhx(43) = MA02*(2*MT2 + (MB2 + MT2)*(TB*Yuk3)) -
     &    2*MB2*NStbhhx(42)

	NStbhhx(455) = MT2 - (4*MB2 - MHp2)*TB2*Yuk3**2*Re(NStbhhl(3))

	NStbhhx(457) = MT2**2 + MB2**2*(TB2*Yuk3**2)

        NStbhhx(464) =
     &   MT2*(NStbhhx(463) - Re(NStbhhl(7))) +
     &    (MT2 + MB2*(TB2*Yuk3**2))*Re(NStbhhl(2)) -
     &    (2*MT2 + MB2*(TB2*Yuk3**2))*Re(NStbhhl(6))

        NStbhhx(467) =
     &   MA02*NStbhhx(466) - 2*Re(NStbhhl(1)) -
     &    (MHp2 - 4*MT2)*Re(NStbhhl(13))

        NStbhhx(470) =
     &   MA02*NStbhhx(469) - 2*Re(NStbhhl(2)) +
     &    (4*MB2 - MHp2)*Re(NStbhhl(9))

        NStbhhx(473) =
     &   MB2*(TB2*Yuk3**2*NStbhhx(472)) +
     &    (MT2 + MB2*(TB2*Yuk3**2))*Re(NStbhhl(1)) -
     &    (MT2 + 2*MB2*(TB2*Yuk3**2))*Re(NStbhhl(6))

	NStbhhx(476) = MB2**2*MT2 - MT2**3

        NStbhhx(478) =
     &   (MB2 - 3*MT2)*(MB2 - MT2)*Re(NStbhhl(11)) +
     &    NStbhhx(477)*Re(NStbhhl(10))

	NStbhhx(52) = MB2**2 + (MHp2 - MT2)**2 - 2*MB2*(MHp2 + MT2)

	NStbhhx(53) = MT2*TB + MB2*(TB2*Yuk3*(4 + TB*Yuk3))

        NStbhhx(59) = MT2*((MHp2 - MT2)**2*TB) -
     &    MB2**3*(TB2*Yuk3*(4 + 7*TB*Yuk3)) - MB2**2*NStbhhx(55) +
     &    MB2*NStbhhx(58)

	NStbhhx(6) = 2/MB2*(NStbhhl(2)*NStbhhx(5)) + 5*MHp2*NStbhhl(6)

        NStbhhx(64) = 5*MA02**2 - MT2*NStbhhx(62) -
     &    4*(MT2**2*(7 + 2/MT2**2*NStbhhl(1)**2) +
     &       MA02*(2*NStbhhl(17) - (NStbhhl(7)*NStbhhx(63))/MA02))

	NStbhhx(7) = 3*MA02 - 2*(MHp2 + MB2*(3 + 2/MB2*NStbhhl(2)))

        NStbhhx(80) = NStbhhx(71)/(MHp2*(MHp2 - 4*MT2)) +
     &    NStbhhx(79)/(MA02 - MHp2)**2

        NStbhhx(83) = MB2*(MB2 + MT2)*
     &     ((MA02 - 4*MT2)*TB2*Yuk3**2) - MT2*NStbhhx(82)

        NStbhhx(142) =
     &   -(1/(MB2 - MT2)**2*
     &       (NStbhhl(1)*NStbhhx(115))/
     &        ((MB - MHp - MT)**2*(MB + MHp - MT)**2*
     &          (MB - MHp + MT)**2*(MB + MHp + MT)**2)) +
     &    2/(MB2 - MT2)*NStbhhx(141)/(TB*NStbhhx(52)**2)

        NStbhhx(19) = NStbhhx(15)/((4*MB2 - MHp2)*MHp2) +
     &    NStbhhx(18)/(MA02 - 4*MB2)

        NStbhhx(193) =
     &   (8*MT2)/(MB2 - MT2)*NStbhhx(177) -
     &    (MHp2*NStbhhx(188) + NStbhhx(52)*NStbhhx(192))/
     &     (TB*NStbhhx(52))

	NStbhhx(22) = 2*MT2*(MA02 - 4*MB2) + TB*Yuk3*NStbhhx(21)

        NStbhhx(302) =
     &   1/(MB2 - MT2)*NStbhhx(264)/NStbhhx(52)**2 -
     &    4/(MB2 - MT2)**2*NStbhhx(301)

        NStbhhx(35) = 16*MB2*MT2*NStbhhx(30) +
     &    2*MA02**3*TB*NStbhhx(23) - MA02**2*NStbhhx(27) +
     &    4*MA02*NStbhhx(34)

        NStbhhx(41) = 2*MB2*
     &     (NStbhhl(1)*NStbhhx(38))/(TB*TB2) + MB2**2*NStbhhx(40)

	NStbhhx(446) = (MB2 - MHp2 + MT2)*TB + 4*MB2*(TB2*Yuk3)

	NStbhhx(448) = (MB2 - MHp2)*TB*Yuk3 + MT2*(4 + TB*Yuk3)

        NStbhhx(450) =
     &   (MB2 - MHp2 + MT2)*Re(NStbhhl(4)) +
     &    (MHp2 - 4*MT2)*Re(NStbhhl(5))

	NStbhhx(453) = MA02**2 - MHp2**2 + 2*MA02*NStbhhl(7)

        NStbhhx(454) =
     &   2*MT2 + (-2*MB2 + MT2)*(TB*Yuk3) +
     &    NStbhhx(448)*Re(NStbhhl(4))

        NStbhhx(456) =
     &   -2*MT2**2 + MT2*NStbhhx(450) + MB2*NStbhhx(455) -
     &    (MT2 - MB2*(TB2*Yuk3**2))*
     &     (Re(NStbhhl(1)) - Re(NStbhhl(2)))

	NStbhhx(461) = 3*MT2*TB + MB2*(TB2*Yuk3*(4 + TB*Yuk3))

	NStbhhx(462) = 3*MB2*(TB*TB2*Yuk3**2) + MT2*(TB + 4*TB2*Yuk3)

        NStbhhx(468) =
     &   -(MT2*NStbhhx(467)) +
     &    (MT2 + MB2*(TB2*Yuk3**2))*Re(NStbhhl(2)) -
     &    (2*MT2 + MB2*(TB2*Yuk3**2))*Re(NStbhhl(6))

        NStbhhx(471) =
     &   MB2*(TB2*Yuk3**2*NStbhhx(470)) -
     &    (MT2 + MB2*(TB2*Yuk3**2))*Re(NStbhhl(1)) +
     &    (MT2 + 2*MB2*(TB2*Yuk3**2))*Re(NStbhhl(6))

	NStbhhx(483) = MT2*NStbhhx(461) + (3*MB2 - 4*MT2)*NStbhhx(462)

	NStbhhx(50) = 2*MT2 - (MA02 - 2*MB2)*TB*Yuk3

	NStbhhx(51) = 2*MT2 + (2*MB2 - MHp2)*TB*Yuk3

        NStbhhx(60) = MT2*
     &    (2*NStbhhl(1)**2 -
     &      (NStbhhl(1)*(MA02*NStbhhx(52)*NStbhhx(53) -
     &           MHp2*NStbhhx(59)))/((MA02 - MHp2)*TB*NStbhhx(52)))

        NStbhhx(8) = 9*MA02**3 - (3*MA02 - 26*MB2)*MHp2**2 -
     &    16*MB2**2*NStbhhx(4) -
     &    (4*MA02*(MHp2**2 + 4*MB2*MHp2)*NStbhhl(6))/MHp2 -
     &    2*MHp2*NStbhhx(1) - 8*NStbhhl(14)*NStbhhx(2) +
     &    8*(MB2*NStbhhx(6) + NStbhhl(15)*NStbhhx(3)) +
     &    2*MA02**2*(MB2 + (2*NStbhhl(7)*NStbhhx(7))/MA02)

	NStbhhx(81) = NStbhhx(64)/(MA02 - 4*MT2) - NStbhhx(80)

	NStbhhx(20) = -(NStbhhx(8)/(MA02 - MHp2)**2) + NStbhhx(19)

        NStbhhx(36) = (8*MB2)/(MB2 - MT2)**2*
     &     (Yuk3*NStbhhl(2)**2*NStbhhx(22)) -
     &    1/(MB2 - MT2)*NStbhhx(35)/((MA02 - 4*MT2)*TB2)

        NStbhhx(445) =
     &   2*Re(NStbhhl(2)) + (4*MB2 - MHp2)*Re(NStbhhl(3))

        NStbhhx(447) =
     &   MB2*((MB2 - MHp2 + MT2)*TB*TB2*Yuk3**2) + MT2*NStbhhx(446)

        NStbhhx(449) =
     &   4*MT2 - 2*(2*MB2 - MT2)*(TB*Yuk3) -
     &    NStbhhx(448)*Re(NStbhhl(4))

        NStbhhx(451) =
     &   -4*MT2**2 + MT2*(2*MB2 - NStbhhx(450)) +
     &    MB2*((4*MB2 - MHp2)*TB2*Yuk3**2*Re(NStbhhl(3))) +
     &    (MT2 - MB2*(TB2*Yuk3**2))*
     &     (Re(NStbhhl(1)) - Re(NStbhhl(2)))

        NStbhhx(61) = -(MB2*
     &       (2*Yuk3**2*NStbhhl(2)**2 -
     &         (2*Yuk3*(MA02*NStbhhl(14)*NStbhhx(50) -
     &              MHp2*NStbhhl(15)*NStbhhx(51)))/
     &          ((MA02 - MHp2)*TB))) + NStbhhx(60)/TB2

        NStbhhx(452) =
     &   MB2*(TB2*Yuk3**2*NStbhhx(445)) + 2*MT2*Re(NStbhhl(1)) -
     &    MT2*((MHp2 - 4*MT2)*Re(NStbhhl(5))) -
     &    (NStbhhl(6)*(MB2*(TB2*Yuk3*NStbhhx(449)) +
     &         TB*NStbhhx(451)))/(MHp2*TB) -
     &    (MT2 + MB2*(TB2*Yuk3**2))*
     &     (Re(NStbhhl(1)) + Re(NStbhhl(2))) -
     &    (NStbhhx(447)*Re(NStbhhl(4)))/TB

        NStbhhx(353) =
     &   MT2*(MHp2*(MB2 + MHp2 - MT2)*NStbhhx(148)) +
     &    (MB2 - MHp2 - MT2)*NStbhhx(52)*NStbhhx(146)

        NStbhhx(326) =
     &   4*SW2**2*(TB2*Yuk3**2) +
     &    MT2*(9 + 4*(3 - 2*SW2)*SW2)*
     &     ((MB2 + MHp2 - MT2)*NStbhhx(148))/NStbhhx(52)**2

        NStbhhx(354) =
     &   MT2 + MB2*(TB2*Yuk3**2) + NStbhhx(353)/NStbhhx(52)**2

        NStbhhx(312) =
     &   MT2 + MB2*(TB2*Yuk3**2) +
     &    ((MB2 - MHp2 - MT2)*NStbhhx(148))/NStbhhx(52)

        NStbhhx(327) =
     &   MT2*(3 - 2*SW2)**2 + MB2*NStbhhx(326) -
     &    ((MB2 - MHp2 + MT2)*NStbhhx(325))/NStbhhx(321)

        NStbhhx(352) =
     &   (2*NStbhhx(331))/NStbhhx(328) +
     &    NStbhhx(351)/NStbhhx(52)**2

        NStbhhx(382) =
     &   4*(1 - 2*SW2)**2*NStbhhx(354) +
     &    1/(MB2 - MT2)**2*
     &     NStbhhx(381)/
     &      ((MB - MHp - MT)**2*(MB + MHp - MT)**2*
     &        (MB - MHp + MT)**2*(MB + MHp + MT)**2)

	NStbhhx(154) = MHp2*TB*TB2*Yuk3**2 - MT2*NStbhhx(153)

        NStbhhx(163) =
     &   5*MHp2**2*TB*TB2*Yuk3**2 - 4*MT2*(MHp2*NStbhhx(153)) -
     &    MT2**2*NStbhhx(162)

        NStbhhx(310) =
     &   1/(MB2 - MT2)*(MT2 - MB2*(TB2*Yuk3**2)) +
     &    (2*MHp2*NStbhhx(148))/((MA02 - MHp2)*NStbhhx(52))

        NStbhhx(313) =
     &   (MT2 + MB2*(TB2*Yuk3**2))*NStbhhx(311) +
     &    (MA02 - MHp2)*NStbhhx(312)

        NStbhhx(383) =
     &   -(18/(MB2 - MT2)*NStbhhx(320)/NStbhhx(52)) +
     &    2/MB2*NStbhhx(327) - 2*NStbhhx(352) -
     &    (9*NStbhhx(382))/MHp2

        NStbhhx(149) =
     &   1/(MB2 - MT2)*(MT2 - MB2*(TB2*Yuk3**2)) -
     &    NStbhhx(148)/NStbhhx(52)

        NStbhhx(155) =
     &   MA02*TB*(MT2 + MB2*(TB2*Yuk3**2)) -
     &    2*(MB2*NStbhhx(154) + TB*NStbhhx(152))

        NStbhhx(158) =
     &   ((MA02 - MHp2)*(MB2 + MHp2 - MT2)*NStbhhx(148))/
     &     NStbhhx(52) - (MT2 + MB2*(TB2*Yuk3**2))*NStbhhx(157)

        NStbhhx(167) =
     &   -(MB2**3*NStbhhx(161)) - MB2**2*NStbhhx(163) -
     &    MB2*((MHp2 - MT2)*NStbhhx(166)) + TB*NStbhhx(159)

        NStbhhx(200) =
     &   2*MB2*((MA02 - 4*MT2)*TB2*Yuk3) + TB*NStbhhx(82)

        NStbhhx(231) =
     &   NStbhhl(15)*NStbhhx(203) -
     &    (NStbhhl(18)*NStbhhx(230))/NStbhhx(52)**2

        NStbhhx(314) =
     &   NStbhhl(1)*NStbhhx(310) +
     &    (NStbhhl(6)*NStbhhx(313))/(MA02 - MHp2)**2

        NStbhhx(384) =
     &   -(288*CW2*SW2*NStbhhx(317)/(MHp2*NStbhhx(52))) +
     &    NStbhhx(383)

        NStbhhx(46) = (MB2 - MT2)**2*
     &     (TB*(MT2 + MB2*(TB2*Yuk3**2))) - MA02*NStbhhx(38)

        NStbhhx(84) = MT2*(MB2 + MT2)*(MA02 - 4*MB2) -
     &    MB2*(TB2*Yuk3**2*NStbhhx(21))

        NStbhhx(150) =
     &   MHp2**2*(MT2 + MB2*(TB2*Yuk3**2)) + MA02*NStbhhx(146) +
     &    (MA02 - MHp2)*MHp2*(MB2 - MHp2 + MT2)*NStbhhx(149)

        NStbhhx(156) =
     &   -(2/(MB2 - MT2)*((MT2 + MB2*(TB2*Yuk3**2))*
     &         NStbhhx(151))) + NStbhhx(155)/TB

        NStbhhx(175) =
     &   2*MHp2*NStbhhl(1)*NStbhhx(158) +
     &    (MHp2**2*(2*NStbhhx(167) - MA02*NStbhhx(174)))/
     &     (TB*NStbhhx(52))

        NStbhhx(232) =
     &   MT2*(NStbhhl(1)**2*NStbhhx(200))/(MA02 - 4*MT2) +
     &    NStbhhx(231)

        NStbhhx(441) =
     &   -(9/(MB2 - MT2)**2*
     &       (2*NStbhhl(1)*NStbhhx(407) + NStbhhx(440)/TB)/
     &        ((MB - MHp - MT)**2*(MB + MHp - MT)**2*
     &          (MB - MHp + MT)**2*(MB + MHp + MT)**2)) +
     &    36*NStbhhx(314) - 2*NStbhhl(6)*NStbhhx(384)

        NStbhhx(458) =
     &   MB2*(TB2*Yuk3**2*NStbhhx(445)) +
     &    MT2*(2*Re(NStbhhl(1)) - (MHp2 - 4*MT2)*Re(NStbhhl(5))) -
     &    (MT2 + MB2*(TB2*Yuk3**2))*
     &     (Re(NStbhhl(1)) + Re(NStbhhl(2)))

        NStbhhx(47) = MT2*
     &     (MA02*NStbhhl(17)*NStbhhx(45))/(MA02 - 4*MT2) +
     &    NStbhhl(16)*NStbhhx(46)

        NStbhhx(479) =
     &   (MB2 - 3*MT2)*(MB2 - MT2)*NStbhhx(468) -
     &    NStbhhx(471)*NStbhhx(477)

	dup1 = TB*NStbhhx(471) + NStbhhx(447)*Re(NStbhhl(10))

        NStbhhx(481) =
     &   2*dup1 + (4*MB2 - 3*MT2)*NStbhhx(461) - MB2*NStbhhx(462)

        NStbhhx(89) = 1/(MB2 - MT2)**2*
     &     (NStbhhl(2)*NStbhhx(84)) -
     &    1/(MB2 - MT2)*NStbhhx(88)/(MA02 - 4*MT2)

        NStbhhx(176) =
     &   2*(MA02*NStbhhl(16)*NStbhhx(144) -
     &       NStbhhl(18)*NStbhhx(150)) +
     &    MA02*NStbhhl(7)*NStbhhx(156) +
     &    (NStbhhl(6)*NStbhhx(175))/MHp2

        NStbhhx(233) =
     &   MT2*(NStbhhl(1)*NStbhhx(196)) -
     &    2*(NStbhhl(1)**2*NStbhhx(197) +
     &       NStbhhl(19)*NStbhhx(199) + NStbhhx(232)/TB)

        NStbhhx(442) =
     &   (18*MB2**2)/(MB2 - MT2)*
     &     (MA02*NStbhhx(52)*NStbhhx(303) - MHp2*NStbhhx(309))/
     &      ((MA02 - MHp2)*TB*NStbhhx(52)) - MB2*NStbhhx(441)

        NStbhhx(459) =
     &   -(MHp2*(MB2*MT2*NStbhhx(153) - 2*TB*NStbhhx(457))) +
     &    (2*MA02 - MHp2)*(TB*NStbhhx(458) -
     &       NStbhhx(447)*Re(NStbhhl(4)))

        NStbhhx(465) =
     &   2*MT2*NStbhhx(461) + TB*NStbhhx(464) -
     &    NStbhhx(447)*Re(NStbhhl(11))

        NStbhhx(474) =
     &   2*MB2*NStbhhx(462) + TB*NStbhhx(473) -
     &    NStbhhx(447)*Re(NStbhhl(10))

        NStbhhx(48) = MB2*
     &     (MA02*Yuk3*NStbhhl(14)*NStbhhx(43))/(MA02 - 4*MB2) +
     &    NStbhhx(47)/TB2

        NStbhhx(480) =
     &   (-MB2**3 + MB2*MT2**2)*NStbhhx(462) +
     &    NStbhhx(461)*NStbhhx(476) +
     &    2*NStbhhx(447)*NStbhhx(478) - 2*TB*NStbhhx(479)

	dup2 = -(TB*NStbhhx(468)) + NStbhhx(447)*Re(NStbhhl(11))

	NStbhhx(482) = -(2*(2*MB2 - MT2)*dup2) + MT2*NStbhhx(481)

	NStbhhx(484) = (MB2 - 2*MT2)*dup1 + MB2*dup2

        NStbhhx(90) = 1/(MB2 - MT2)**2*
     &     (NStbhhl(1)*NStbhhx(83))/(MA02 - 4*MT2) +
     &    NStbhhx(89)/(MA02 - 4*MB2)

        NStbhhx(443) =
     &   18*MT2*NStbhhx(81) - 36/(MB2 - MT2)**2*NStbhhx(233) -
     &    4/MB2*(NStbhhl(2)*NStbhhx(442)) +
     &    72*(NStbhhl(7)*NStbhhx(90) +
     &       (NStbhhl(6)*NStbhhx(142))/MHp2) -
     &    (72*NStbhhx(176))/(MA02 - MHp2)**2 -
     &    (18*NStbhhx(193))/(MA02 - MHp2) + (9*NStbhhx(302))/TB

        NStbhhx(460) =
     &   NStbhhx(453)*(MB2*(TB2*Yuk3*NStbhhx(454)) +
     &       TB*NStbhhx(456)) + 2*NStbhhl(6)*NStbhhx(459)

        NStbhhx(475) =
     &   -2*(dup1 + dup2) + MT2*NStbhhx(461) + MB2*NStbhhx(462) -
     &    1/MT2*(NStbhhl(1)*NStbhhx(465)) -
     &    1/MB2*(NStbhhl(2)*NStbhhx(474))

        NStbhhx(485) =
     &   -NStbhhx(480) + 2*NStbhhl(1)*NStbhhx(482) -
     &    2*NStbhhl(2)*(MB2*NStbhhx(483) - 2*NStbhhx(484))

        NStbhhx(49) = 1/MB2*(NStbhhl(2)*NStbhhx(41)) -
     &    (2*NStbhhx(48))/TB

        NStbhhx(444) =
     &   18*MB2*(Yuk3**2*NStbhhx(20)) -
     &    36/(MB2 - MT2)**2*NStbhhx(49) -
     &    72/(MB2 - MT2)*NStbhhx(61) +
     &    (9*NStbhhx(36))/((MA02 - 4*MB2)*TB) + NStbhhx(443)/TB2

        NStbhhx(486) =
     &   -(1/(MB2 - MT2)**2*NStbhhx(485)) +
     &    (2*NStbhhx(460))/(MA02 - MHp2)**2 + 4*NStbhhx(475)

        NStbHHeqHp = -(Alfa2/(MW2**2*Pi**2*SW2**2)*
     &      (1/6144.D0*NStbhhx(444) -
     &        3/512.D0*(4*NStbhhx(452) - NStbhhx(486)/TB)/TB2))

	end
