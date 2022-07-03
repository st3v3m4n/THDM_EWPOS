c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich


* NStbA0eqHp.F
* generated 29-Jan-2017 14:38

C> @brief This subroutine calculates \f$\delta\rho^{(2)}_\mathrm{tb,NS}\f$ with \f$m_{A^0}=m_{H^\pm}\f$.
C>
C> @ingroup group_rho
	subroutine calcNStbA0eqHp
	implicit none


#include "NStbA0eqHp.h"

	ComplexType dup1, dup2

	RealType T134sub

	external T134sub

	ComplexType A0FH, B0FH

	external A0FH, B0FH

#include "paraTHDM.h"
#include "paraSM.h"
	NStbapl(1) = A0FH(MT2)

	NStbapl(10) = B0FH(MB2,MHp2,MT2)

	NStbapl(11) = B0FH(MT2,MB2,MHp2)

	NStbapl(12) = B0FH(MT2,MHH2,MT2)

	NStbapl(13) = B0FH(MT2,MHp2,MT2)

	NStbapl(14) = T134sub(MHH2,MB2,MB2)

	NStbapl(15) = T134sub(MHp2,MB2,MB2)

	NStbapl(16) = T134sub(MT2,MHH2,MB2)

	NStbapl(17) = T134sub(MT2,MT2,MHH2)

	NStbapl(18) = T134sub(MT2,MHp2,MB2)

	NStbapl(19) = T134sub(MT2,MT2,MHp2)

	NStbapl(2) = A0FH(MB2)

	NStbapl(3) = B0FH(MHp2,MB2,MB2)

	NStbapl(4) = B0FH(MHp2,MB2,MT2)

	NStbapl(5) = B0FH(MHp2,MT2,MT2)

	NStbapl(6) = A0FH(MHp2)

	NStbapl(7) = A0FH(MHH2)

	NStbapl(8) = B0FH(MB2,MB2,MHH2)

	NStbapl(9) = B0FH(MB2,MB2,MHp2)

	NStbapx(384) = -12 + 272*TB*Yuk3 + 127*TB2*Yuk3**2

	NStbapx(388) = -12 + 80*TB*Yuk3 + 31*TB2*Yuk3**2

	NStbapx(295) = 12*TB*Yuk3 + (9 - 8*SW2)*(TB2*Yuk3**2)

	NStbapx(359) = -3 + 80*TB*Yuk3 + 26*TB2*Yuk3**2

	NStbapx(363) = -19 + 80*TB*Yuk3 + 26*TB2*Yuk3**2

	NStbapx(372) = -105 + 296*TB*Yuk3 + 144*TB2*Yuk3**2

	NStbapx(376) = -9 + 40*TB*Yuk3 + 16*TB2*Yuk3**2

	NStbapx(383) = -8 + 272*TB*Yuk3 + 127*TB2*Yuk3**2

        NStbapx(385) =
     &   -(CW2*(4 + SW2*NStbapx(383))) + SW2*NStbapx(384)

	NStbapx(387) = -8 + 80*TB*Yuk3 + 31*TB2*Yuk3**2

        NStbapx(389) =
     &   -(CW2*(4 + SW2*NStbapx(387))) + SW2*NStbapx(388)

	NStbapx(461) = -1 + 3*SW2*(2 - TB2*Yuk3**2)

	NStbapx(471) = -1 + 3*SW2*(10 - TB2*Yuk3**2)

	NStbapx(478) = -1 + SW2*(27 - TB2*Yuk3**2)

	NStbapx(486) = -2 + 3*SW2*(14 - TB2*Yuk3**2)

	NStbapx(489) = 1 + SW2*(9 + TB2*Yuk3**2)

        NStbapx(275) =
     &   -3*MT2*(21 - 72*SW2 + 56*SW2**2) +
     &    (9 + 12*SW2 + 56*SW2**2)*MHp2

        NStbapx(276) =
     &   -(MT2**2*(36 - 117*SW2 + 112*SW2**2)) +
     &    MT2*(27 - 84*SW2 + 56*SW2**2)*MHp2 +
     &    (9 - 33*SW2 + 56*SW2**2)*MHp2**2

        NStbapx(278) =
     &   5*MB2**3 - 10*(MHp2 - MT2)**3 -
     &    2*MB2**2*(10*MHp2 - 9*MT2) +
     &    MB2*((MHp2 - MT2)*(25*MHp2 + 33*MT2))

        NStbapx(279) =
     &   (MHp2 - MT2)**3 + MB2**2*(MHp2 - 3*MT2) -
     &    2*MB2*((MHp2 - MT2)*(MHp2 + 2*MT2))

	NStbapx(284) = -9 + 2*SW2**2 - 18*TB*Yuk3

	NStbapx(293) = 18*TB + (9 - 8*SW2**2)*(TB2*Yuk3)

	NStbapx(296) = 16*SW2**2*(MB2 - MHp2) + 3*MB2*NStbapx(295)

	NStbapx(358) = -1 + 80*TB*Yuk3 + 26*TB2*Yuk3**2

        NStbapx(360) =
     &   -(CW2*(2 + SW2*NStbapx(358))) + SW2*NStbapx(359)

	NStbapx(362) = -17 + 80*TB*Yuk3 + 26*TB2*Yuk3**2

        NStbapx(364) =
     &   -(CW2*(2 + SW2*NStbapx(362))) + SW2*NStbapx(363)

	NStbapx(371) = -103 + 296*TB*Yuk3 + 144*TB2*Yuk3**2

        NStbapx(373) =
     &   -(CW2*(2 + SW2*NStbapx(371))) + SW2*NStbapx(372)

	NStbapx(375) = -7 + 40*TB*Yuk3 + 16*TB2*Yuk3**2

        NStbapx(377) =
     &   -(CW2*(2 + SW2*NStbapx(375))) + SW2*NStbapx(376)

	NStbapx(380) = 13 + 66*TB*Yuk3 - 51*TB2*Yuk3**2

	NStbapx(381) = 1 + 10*TB*Yuk3 + 5*TB2*Yuk3**2

	NStbapx(382) = 131 + 240*TB*Yuk3 + 67*TB2*Yuk3**2

        NStbapx(386) =
     &   -173 + 1052*TB*Yuk3 + 18*TB2*Yuk3**2 -
     &    8*SW2**2*NStbapx(383) + 8*NStbapx(385)

        NStbapx(390) =
     &   -29 + 340*TB*Yuk3 - 14*TB2*Yuk3**2 -
     &    8*SW2**2*NStbapx(387) + 8*NStbapx(389)

	NStbapx(444) = 33 + 46*TB*Yuk3 + 14*TB2*Yuk3**2

	NStbapx(462) = SW2*(7 - 3*TB2*Yuk3**2) - CW2*NStbapx(461)

	NStbapx(466) = 66*TB2*Yuk3 - TB*(51 - 13*TB2*Yuk3**2)

	NStbapx(467) = 11*TB2*Yuk3 - TB*(4 + 25*TB2*Yuk3**2)

	NStbapx(469) = 2 + 3*SW2*(2 + TB2*Yuk3**2)

	NStbapx(472) = SW2*(31 - 3*TB2*Yuk3**2) - CW2*NStbapx(471)

	NStbapx(479) = SW2*(28 - TB2*Yuk3**2) - CW2*NStbapx(478)

	NStbapx(487) = SW2*(44 - 3*TB2*Yuk3**2) - CW2*NStbapx(486)

	NStbapx(490) = SW2**2*(9 + TB2*Yuk3**2) + CW2*NStbapx(489)

	NStbapx(127) = MHp2 + 2*MT2 + 4*(MB2 - MT2)*(TB*Yuk3)

	NStbapx(138) = -1 + 2*TB*Yuk3 + TB2*Yuk3**2

	NStbapx(139) = 5 + 6*TB*Yuk3 + 3*TB2*Yuk3**2

	NStbapx(141) = 3 + 4*TB*Yuk3 + 3*TB2*Yuk3**2

	NStbapx(142) = 2 + 8*TB*Yuk3 + 3*TB2*Yuk3**2

	NStbapx(193) = -2 + SW2*(39 + 2*TB2*Yuk3**2)

	NStbapx(196) = 1 + SW2*(5 - 7*TB2*Yuk3**2)

	NStbapx(204) = -1 + SW2*(27 - 25*TB2*Yuk3**2)

	NStbapx(207) = 2 + SW2*(6 - 7*TB2*Yuk3**2)

	NStbapx(216) = -2 + 5*SW2*(1 - 2*TB2*Yuk3**2)

	NStbapx(219) = 2 + SW2*(54 + 73*TB2*Yuk3**2)

	NStbapx(271) = MB2 - 2*MHp2 + MT2 + 4*MB2*(TB*Yuk3)

	NStbapx(274) = MT2**4 + MT2**2*(MB2**2 + 3*MHp2**2)

        NStbapx(277) =
     &   30*MB2**3*SW2 + (-9 + 48*SW2 - 56*SW2**2)*
     &     (MHp2 - MT2)**3 - MB2**2*NStbapx(275) +
     &    2*MB2*NStbapx(276)

        NStbapx(280) =
     &   -9*(3*MB2 - 2*MHp2)*(MB2 - MHp2)**2 -
     &    4*MB2**2*(27 - 54*SW2 + 56*SW2**2)*
     &     ((MB2 - MHp2 - MT2)*TB*Yuk3) + 6*SW2*NStbapx(278) +
     &    56*SW2**2*NStbapx(279)

	NStbapx(283) = 3*MB2*(3 + 4*SW2) - 4*SW2**2*(MB2 + MHp2 - MT2)

        NStbapx(285) =
     &   (3 - 2*SW2)**2*(MHp2 - MT2) + 2*MB2*NStbapx(284)

        NStbapx(294) =
     &   8*SW2**2*(MB2 - MHp2 - MT2) + MB2*(Yuk3*NStbapx(293))

        NStbapx(297) =
     &   MT2**2*(9 + 8*(3 - 2*SW2)*SW2) +
     &    MB2*(3 - 4*SW2)**2*((MB2 - MHp2)*TB2*Yuk3**2) +
     &    MT2*NStbapx(296)

	NStbapx(330) = -10 + 8*TB*Yuk3 + 3*TB2*Yuk3**2

	NStbapx(331) = 55 + 36*TB*Yuk3 + 10*TB2*Yuk3**2

	NStbapx(343) = 37 + 436*TB*Yuk3 - 467*TB2*Yuk3**2

	NStbapx(344) = 27 + 580*TB*Yuk3 - 93*TB2*Yuk3**2

	NStbapx(345) = -5 + 118*TB*Yuk3 + 35*TB2*Yuk3**2

	NStbapx(347) = -253 + 588*TB*Yuk3 + 7*TB2*Yuk3**2

	NStbapx(348) = -97 + 52*TB*Yuk3 + 3*TB2*Yuk3**2

	NStbapx(349) = -9 + 4*TB*Yuk3 - 5*TB2*Yuk3**2

	NStbapx(350) = -311 + 460*TB*Yuk3 - 27*TB2*Yuk3**2

	NStbapx(351) = -191 + 84*TB*Yuk3 - 27*TB2*Yuk3**2

	NStbapx(353) = 10 + 212*TB*Yuk3 - 49*TB2*Yuk3**2

	NStbapx(354) = -10 + 292*TB*Yuk3 - 31*TB2*Yuk3**2

	NStbapx(356) = 37 + 112*TB*Yuk3 + 110*TB2*Yuk3**2

        NStbapx(357) =
     &   2*MHp2**4*(32*TB + 5*TB2*Yuk3) -
     &    MT2*(MHp2**3*(272*TB + 117*TB2*Yuk3))

        NStbapx(361) =
     &   -237 + 664*TB*Yuk3 + 622*TB2*Yuk3**2 -
     &    16*SW2**2*NStbapx(358) + 16*NStbapx(360)

        NStbapx(365) =
     &   19 + 840*TB*Yuk3 - 82*TB2*Yuk3**2 -
     &    16*SW2**2*NStbapx(362) + 16*NStbapx(364)

	NStbapx(367) = -27 + 104*TB*Yuk3 + 16*TB2*Yuk3**2

	NStbapx(368) = -10 + 36*TB*Yuk3 + 3*TB2*Yuk3**2

	NStbapx(369) = 54 + 656*TB*Yuk3 + TB2*Yuk3**2

	NStbapx(370) = 51 + 880*TB*Yuk3 + 109*TB2*Yuk3**2

        NStbapx(374) =
     &   -488 + 2124*TB*Yuk3 - 141*TB2*Yuk3**2 -
     &    16*SW2**2*NStbapx(371) + 16*NStbapx(373)

        NStbapx(378) =
     &   -136 + 292*TB*Yuk3 + 3*TB2*Yuk3**2 -
     &    16*SW2**2*NStbapx(375) + 16*NStbapx(377)

        NStbapx(391) =
     &   MHp2**5*Yuk3*(16*TB + 5*TB2*Yuk3) -
     &    10*MT2*(MHp2**4*NStbapx(381)) +
     &    MT2**2*(MHp2**3*NStbapx(382)) +
     &    MT2**4*(MHp2*NStbapx(386)) -
     &    4*(MT2**3*(MHp2**2*NStbapx(380)) + MT2**5*NStbapx(390))

	NStbapx(418) = 9 + 22*TB*Yuk3 - 10*TB2*Yuk3**2

	NStbapx(419) = 1 + 2*TB*Yuk3 - 2*TB2*Yuk3**2

	NStbapx(421) = 145 + 296*TB*Yuk3 - 19*TB2*Yuk3**2

	NStbapx(422) = 13 + 34*TB*Yuk3 + 10*TB2*Yuk3**2

	NStbapx(424) = 274 + 56*TB*Yuk3 - 125*TB2*Yuk3**2

	NStbapx(425) = -7 + 14*TB*Yuk3 + 8*TB2*Yuk3**2

	NStbapx(426) = 10 + 2*TB*Yuk3 + 3*TB2*Yuk3**2

	NStbapx(427) = -26 + 12*TB*Yuk3 - 5*TB2*Yuk3**2

	NStbapx(428) = -6 + 32*TB*Yuk3 - 11*TB2*Yuk3**2

	NStbapx(430) = -77 + 116*TB*Yuk3 + 131*TB2*Yuk3**2

	NStbapx(431) = 25 + 30*TB*Yuk3 - 16*TB2*Yuk3**2

	NStbapx(432) = 5 + 10*TB*Yuk3 - TB2*Yuk3**2

	NStbapx(433) = 6 + 28*TB*Yuk3 + 5*TB2*Yuk3**2

	NStbapx(434) = 57 + 28*TB*Yuk3 + 36*TB2*Yuk3**2

	NStbapx(436) = 4 + 12*TB*Yuk3 - 5*TB2*Yuk3**2

	NStbapx(437) = 42 + 50*TB*Yuk3 + 9*TB2*Yuk3**2

	NStbapx(438) = 101 + 400*TB*Yuk3 + 214*TB2*Yuk3**2

        NStbapx(440) =
     &   MT2 - (4*MHp2 + 5*MT2)*TB2*Yuk3**2 + 2*MT2*(TB*Yuk3)

	NStbapx(442) = 151 + 176*TB*Yuk3 + 19*TB2*Yuk3**2

	NStbapx(443) = 55 + 28*TB*Yuk3 + 30*TB2*Yuk3**2

        NStbapx(445) =
     &   MT2*(MHp2**4*(9 + 7*TB2*Yuk3**2)) +
     &    MT2**3*(MHp2**2*NStbapx(444))

	NStbapx(449) = 52*TB2*Yuk3 + TB*(3 - 97*TB2*Yuk3**2)

	NStbapx(451) = 796*TB2*Yuk3 + TB*(19 - 641*TB2*Yuk3**2)

	NStbapx(452) = 292*TB2*Yuk3 + 3*TB*(1 - 56*TB2*Yuk3**2)

	NStbapx(454) = 340*TB2*Yuk3 - TB*(14 + 61*TB2*Yuk3**2)

	NStbapx(455) = 262*TB2*Yuk3 - TB*(5 + 141*TB2*Yuk3**2)

	NStbapx(456) = 2124*TB2*Yuk3 - TB*(141 + 520*TB2*Yuk3**2)

	NStbapx(458) = TB + 2*TB2*(328*Yuk3 + 27*TB*Yuk3**2)

	NStbapx(459) = 1052*TB2*Yuk3 + TB*(18 - 205*TB2*Yuk3**2)

	NStbapx(460) = 272*TB2*Yuk3 - TB*(27 + 251*TB2*Yuk3**2)

        NStbapx(463) =
     &   50 + 13*TB2*Yuk3**2 - 96*SW2**2*(2 - TB2*Yuk3**2) +
     &    32*NStbapx(462)

	NStbapx(465) = 880*TB2*Yuk3 + TB*(109 + 51*TB2*Yuk3**2)

        NStbapx(468) =
     &   MT2**3*(MHp2**2*NStbapx(466)) + MT2*(MHp2**4*NStbapx(467))

        NStbapx(470) =
     &   29 - 27*TB2*Yuk3**2 + 96*SW2**2*(2 + TB2*Yuk3**2) -
     &    32*SW2*(4 + 3*TB2*Yuk3**2) + 32*CW2*NStbapx(469)

        NStbapx(473) =
     &   -654 + 269*TB2*Yuk3**2 - 96*SW2**2*(10 - TB2*Yuk3**2) +
     &    32*NStbapx(472)

	NStbapx(475) = 36*TB2*Yuk3 + TB*(3 - 10*TB2*Yuk3**2)

	NStbapx(476) = 118*TB2*Yuk3 + 5*TB*(7 - TB2*Yuk3**2)

	NStbapx(477) = 10*TB2*Yuk3 + TB*(5 + TB2*Yuk3**2)

        NStbapx(480) =
     &   -63 - 10*TB2*Yuk3**2 - 32*SW2**2*(27 - TB2*Yuk3**2) +
     &    32*NStbapx(479)

	NStbapx(482) = 104*TB2*Yuk3 + TB*(16 - 27*TB2*Yuk3**2)

	NStbapx(483) = 4*TB2*Yuk3 - TB*(5 + 9*TB2*Yuk3**2)

	NStbapx(484) = 112*TB2*Yuk3 + TB*(110 + 37*TB2*Yuk3**2)

	NStbapx(485) = 240*TB2*Yuk3 + TB*(67 + 131*TB2*Yuk3**2)

        NStbapx(488) =
     &   -531 + 37*TB2*Yuk3**2 - 96*SW2**2*(14 - TB2*Yuk3**2) +
     &    32*NStbapx(487)

        NStbapx(491) =
     &   17 - 10*TB2*Yuk3**2 - 32*SW2*(8 + TB2*Yuk3**2) +
     &    32*NStbapx(490)

	NStbapx(100) = -7 + 32*TB*Yuk3 - 7*TB2*Yuk3**2

	NStbapx(101) = 13 + 32*TB*Yuk3 + 38*TB2*Yuk3**2

	NStbapx(103) = 39 + 64*TB*Yuk3 + 39*TB2*Yuk3**2

	NStbapx(111) = MHH2 + 2*MT2 + 4*(MB2 - MT2)*(TB*Yuk3)

	NStbapx(115) = 1 + 2*TB*Yuk3 + TB2*Yuk3**2

	NStbapx(124) = MB2 - MHp2 + MT2 + 4*MB2*(TB*Yuk3)

        NStbapx(126) =
     &   2*MB2**2 + MHp2*(MHp2 - MT2) - MB2*(3*MHp2 + 2*MT2)

        NStbapx(128) =
     &   MHp2**2 + 2*MT2**2 - 3*MT2*MHp2 - MB2*NStbapx(127)

	NStbapx(132) = 1 + 2*TB*Yuk3 - TB2*Yuk3**2

	NStbapx(134) = 1 + 4*TB*Yuk3 + TB2*Yuk3**2

	NStbapx(136) = 3 + 8*TB*Yuk3 + 2*TB2*Yuk3**2

	NStbapx(14) = 32*MB2**2 + 5*MHp2**2 - 30*MB2*MHp2

        NStbapx(140) =
     &   2*MHp2**2*TB2*Yuk3**2 + MT2**2*NStbapx(138) -
     &    MT2*(MHp2*NStbapx(139))

        NStbapx(143) =
     &   3*MHp2**2*TB2*Yuk3**2 - 2*MT2*(MHp2*NStbapx(141)) +
     &    MT2**2*NStbapx(142)

        NStbapx(146) =
     &   2*(MT2 + MB2*(TB*Yuk3))**2 -
     &    MHp2*(MT2 + MB2*(TB2*Yuk3**2))

	NStbapx(194) = SW2*(41 + 2*TB2*Yuk3**2) - CW2*NStbapx(193)

	NStbapx(197) = SW2**2*(5 - 7*TB2*Yuk3**2) + CW2*NStbapx(196)

	NStbapx(205) = SW2*(28 - 25*TB2*Yuk3**2) - CW2*NStbapx(204)

	NStbapx(208) = SW2**2*(6 - 7*TB2*Yuk3**2) + CW2*NStbapx(207)

	NStbapx(217) = SW2*(7 - 10*TB2*Yuk3**2) - CW2*NStbapx(216)

	NStbapx(220) = SW2**2*(54 + 73*TB2*Yuk3**2) + CW2*NStbapx(219)

	NStbapx(265) = MHp2 + MT2 + 2*(MB2 + MHp2 - MT2)*TB*Yuk3

	NStbapx(269) = MB2**2 + 3*(MHp2 - MT2)**2 - 4*MB2*(MHp2 + MT2)

        NStbapx(272) =
     &   MB2*((MB2 - 2*MHp2 + MT2)*TB2*Yuk3**2) + MT2*NStbapx(271)

        NStbapx(281) =
     &   -(9*MT2**3*(7*MB2 + 6*MHp2)) +
     &    MB2*(TB2*Yuk3**2*NStbapx(277)) - MT2*NStbapx(280) +
     &    18*NStbapx(274)

	NStbapx(282) = (MB2 - MHp2 - MT2)**2 - 4*MT2*MHp2

        NStbapx(286) =
     &   MB2*(TB2*Yuk3**2*NStbapx(283)) - MT2*NStbapx(285)

        NStbapx(291) =
     &   16*MT2*SW2**2 + MB2*(3 - 4*SW2)**2*(TB2*Yuk3**2)

	NStbapx(292) = -(MB2 + MHp2 - MT2)**2 + 4*MB2*MHp2

        NStbapx(298) =
     &   3*MT2**2*(3 + 8*SW2) + 2*MT2*NStbapx(294) +
     &    1/MT2*((MB2 - MHp2)*NStbapx(297))

	NStbapx(310) = 8*MT2 + MHp2*(2 - 5*TB*Yuk3)

	NStbapx(315) = 6*MHp2**2 - 13*MT2**2 + 45*MT2*MHp2

        NStbapx(317) =
     &   9*MHp2**3 - 6*MT2**3 + 29*MT2**2*MHp2 + 42*MT2*MHp2**2

	NStbapx(319) = 3*MB2 - 5*(MHp2 + 2*MT2)

	NStbapx(321) = -58 + 64*TB*Yuk3 + 21*TB2*Yuk3**2

	NStbapx(322) = -7 + 16*TB*Yuk3 + 13*TB2*Yuk3**2

	NStbapx(323) = 21 + 8*TB*Yuk3 + 21*TB2*Yuk3**2

	NStbapx(324) = -19 + 2*Yuk3*(4*TB - 9*TB2*Yuk3)

	NStbapx(325) = -13 + 2*Yuk3*(12*TB + 5*TB2*Yuk3)

	NStbapx(327) = -5 + 4*TB*Yuk3 - 5*TB2*Yuk3**2

	NStbapx(328) = -13 + 4*TB*Yuk3 - 14*TB2*Yuk3**2

	NStbapx(329) = 29 + 4*Yuk3*(7*TB + 4*TB2*Yuk3)

        NStbapx(332) =
     &   -(MT2**4*NStbapx(330)) + MT2**3*(MHp2*NStbapx(331))

	NStbapx(334) = -12 + 16*TB*Yuk3 + 13*TB2*Yuk3**2

	NStbapx(335) = 5 + 4*TB*Yuk3 + 7*TB2*Yuk3**2

	NStbapx(336) = 13 + 16*TB*Yuk3 - 7*TB2*Yuk3**2

	NStbapx(337) = 13 + 108*TB*Yuk3 + 13*TB2*Yuk3**2

        NStbapx(340) =
     &   (MB2 - MT2)*(5*MHp2 - 4*MT2) -
     &    2*MB2*((4*MB2 + MHp2 - 8*MT2)*TB*Yuk3)

	NStbapx(342) = MHp2**2 - 28*MT2**2 + 19*MT2*MHp2

        NStbapx(346) =
     &   192*MHp2**3*TB*Yuk3 - MT2**2*(MHp2*NStbapx(343)) -
     &    4*(MT2**3*NStbapx(344) + MT2*(MHp2**2*NStbapx(345)))

        NStbapx(352) =
     &   -(MT2**3*(MHp2*NStbapx(347))) + 4*MT2**4*NStbapx(348) +
     &    MT2**2*(MHp2**2*NStbapx(350)) -
     &    MT2*(MHp2**3*NStbapx(351)) + MHp2**4*NStbapx(349)

        NStbapx(355) =
     &   -4*MHp2**2*Yuk3*(32*TB - 5*TB2*Yuk3) +
     &    4*MT2**2*NStbapx(353) + MT2*(MHp2*NStbapx(354))

        NStbapx(366) =
     &   4*MT2**2*(MHp2**2*NStbapx(356)) +
     &    MT2**3*(MHp2*NStbapx(361)) - 4*MT2**4*NStbapx(365) +
     &    2*Yuk3*NStbapx(357)

        NStbapx(379) =
     &   2*MT2**2*(MHp2**3*NStbapx(370)) +
     &    MT2**4*(MHp2*NStbapx(374)) -
     &    4*(MT2*(MHp2**4*NStbapx(367)) +
     &       MT2**3*(MHp2**2*NStbapx(369)) + MT2**5*NStbapx(378))+
     &      MHp2**5*NStbapx(368)

        NStbapx(392) =
     &   MB2**7*((MHp2 - 4*MT2)*Yuk3*(16*TB - 5*TB2*Yuk3)) +
     &    MB2**3*NStbapx(391)

	NStbapx(397) = 1 + 44*TB*Yuk3 - 3*TB2*Yuk3**2

	NStbapx(399) = 21 + 22*TB*Yuk3 - 6*TB2*Yuk3**2

	NStbapx(400) = -2 + 104*TB*Yuk3 + 3*TB2*Yuk3**2

	NStbapx(401) = -11 + 4*TB*Yuk3 + 2*TB2*Yuk3**2

	NStbapx(402) = 1 + 24*TB*Yuk3 + TB2*Yuk3**2

	NStbapx(404) = -6 + 22*TB*Yuk3 + 21*TB2*Yuk3**2

	NStbapx(405) = 15 + 32*TB*Yuk3 + TB2*Yuk3**2

	NStbapx(406) = 1 + 68*TB*Yuk3 + TB2*Yuk3**2

	NStbapx(408) = 2 + 4*TB*Yuk3 - 11*TB2*Yuk3**2

	NStbapx(409) = -3 + 44*TB*Yuk3 + TB2*Yuk3**2

	NStbapx(410) = 20 + 56*TB*Yuk3 + 7*TB2*Yuk3**2

	NStbapx(411) = 1 + 32*TB*Yuk3 + 15*TB2*Yuk3**2

	NStbapx(413) = 3 + 104*TB*Yuk3 - 2*TB2*Yuk3**2

	NStbapx(414) = 7 + 56*TB*Yuk3 + 20*TB2*Yuk3**2

        NStbapx(417) =
     &   7*MHp2**2 - 2*MT2*(MHp2*(7 + 4*TB*Yuk3)) +
     &    MT2**2*(13 + 8*TB*Yuk3)

        NStbapx(420) =
     &   115*MHp2**2*TB2*Yuk3**2 - 8*MT2*(MHp2*NStbapx(418)) -
     &    80*MT2**2*NStbapx(419)

        NStbapx(423) =
     &   128*MHp2**3*TB2*Yuk3**2 - 160*MT2**3*NStbapx(132) -
     &    MT2*(MHp2**2*NStbapx(421)) - 8*MT2**2*(MHp2*NStbapx(422))

        NStbapx(429) =
     &   7*MHp2**6*TB2*Yuk3**2 - 16*MT2**6*(1 + 2*TB*Yuk3) +
     &    MT2**4*(MHp2**2*NStbapx(424)) +
     &    8*MT2**5*(MHp2*NStbapx(425)) +
     &    2*MT2**2*(MHp2**4*NStbapx(427)) -
     &    4*(MT2*(MHp2**5*NStbapx(426)) +
     &       MT2**3*(MHp2**3*NStbapx(428)))

        NStbapx(435) =
     &   20*MHp2**5*TB2*Yuk3**2 - MT2**3*(MHp2**2*NStbapx(430)) -
     &    2*MT2**2*(MHp2**3*NStbapx(431)) -
     &    8*MT2**5*NStbapx(432) + 8*MT2**4*(MHp2*NStbapx(433)) -
     &    MT2*(MHp2**4*NStbapx(434))

	NStbapx(441) = MB2**8*(TB2*Yuk3**2) + MB2**7*NStbapx(440)

        NStbapx(446) =
     &   -5*MHp2**5*(1 - TB2*Yuk3**2) - 8*MT2**5*(7 + 6*TB*Yuk3) +
     &    MT2**4*(MHp2*NStbapx(442)) +
     &    2*MT2**2*(MHp2**3*NStbapx(443)) - 4*NStbapx(445)

	NStbapx(448) = 5*(MHp2 + MT2)*TB + 16*(MHp2 - MT2)*TB2*Yuk3

	NStbapx(450) = -309*MHp2*TB*TB2*Yuk3**2 + 4*MT2*NStbapx(449)

        NStbapx(453) =
     &   420*MHp2**2*TB*TB2*Yuk3**2 - MT2*(MHp2*NStbapx(451)) -
     &    4*MT2**2*NStbapx(452)

        NStbapx(457) =
     &   -246*MHp2**3*TB*TB2*Yuk3**2 + 8*MT2**3*NStbapx(454) +
     &    4*MT2*(MHp2**2*NStbapx(455)) + MT2**2*(MHp2*NStbapx(456))

	NStbapx(46) = -7 + 8*TB*Yuk3 + 6*TB2*Yuk3**2

        NStbapx(464) =
     &   24*MHp2**4*TB*TB2*Yuk3**2 - MT2**3*(MHp2*NStbapx(459)) -
     &    MT2*(MHp2**3*NStbapx(460)) -
     &    2*(MT2**2*(MHp2**2*NStbapx(458)) +
     &       MT2**4*(840*TB2*Yuk3 - TB*NStbapx(463)))

        NStbapx(474) =
     &   3*MHp2**5*TB*TB2*Yuk3**2 +
     &    2*MT2**2*(MHp2**3*NStbapx(465)) +
     &    4*MT2**5*(580*TB2*Yuk3 - TB*NStbapx(470)) +
     &    MT2**4*(MHp2*(664*TB2*Yuk3 - TB*NStbapx(473))) +
     &    8*NStbapx(468)

        NStbapx(481) =
     &   -(8*MT2**5*(5*TB - 16*TB2*Yuk3)) -
     &    2*MT2**2*(MHp2**3*(117*TB + 272*TB2*Yuk3)) +
     &    4*MT2**3*(MHp2**2*NStbapx(476)) +
     &    20*MT2*(MHp2**4*NStbapx(477)) -
     &    MT2**4*(MHp2*(292*TB2*Yuk3 + TB*NStbapx(480))) +
     &    MHp2**5*NStbapx(475)

        NStbapx(492) =
     &   -(4*MT2**3*(MHp2**2*NStbapx(484))) +
     &    2*MT2**2*(MHp2**3*NStbapx(485)) -
     &    MT2**4*(MHp2*(436*TB2*Yuk3 + TB*NStbapx(488))) +
     &    4*(MT2*(MHp2**4*NStbapx(482)) +
     &       MT2**5*(212*TB2*Yuk3 - TB*NStbapx(491))) +
     &    MHp2**5*NStbapx(483)

	NStbapx(85) = 5 + 16*TB*Yuk3 - 29*TB2*Yuk3**2

	NStbapx(87) = 5 + 32*TB*Yuk3 + 3*TB2*Yuk3**2

	NStbapx(90) = 3 + 32*TB*Yuk3 + 5*TB2*Yuk3**2

	NStbapx(91) = 11 + 32*TB*Yuk3 + 11*TB2*Yuk3**2

	NStbapx(93) = -29 + 16*TB*Yuk3 + 5*TB2*Yuk3**2

	NStbapx(94) = 11 + 16*TB*Yuk3 + 11*TB2*Yuk3**2

	NStbapx(95) = 38 + 32*TB*Yuk3 + 13*TB2*Yuk3**2

        NStbapx(102) =
     &   23*MHp2**2*TB2*Yuk3**2 - MT2**2*NStbapx(100) -
     &    MT2*(MHp2*NStbapx(101))

        NStbapx(104) =
     &   MHp2**2*TB2*Yuk3**2 -
     &    2*MHp2*(MT2 + 14*MT2*(TB2*Yuk3**2)) + MT2**2*NStbapx(103)

	NStbapx(108) = MB2 - MHH2 + MT2 + 4*MB2*(TB*Yuk3)

        NStbapx(110) =
     &   2*MB2**2 + MHH2*(MHH2 - MT2) - MB2*(3*MHH2 + 2*MT2)

        NStbapx(112) =
     &   MHH2**2 + 2*MT2**2 - 3*MT2*MHH2 - MB2*NStbapx(111)

        NStbapx(116) =
     &   (MHH2 - 2*MHp2)*TB2*Yuk3**2 + 2*MT2*NStbapx(115)

        NStbapx(118) =
     &   2*(MT2 + MB2*(TB*Yuk3))**2 -
     &    MHH2*(MT2 + MB2*(TB2*Yuk3**2))

	NStbapx(123) = 4*MB2*MT2 - (MB2 - MHp2 + MT2)**2

        NStbapx(125) =
     &   MB2*((MB2 - MHp2 + MT2)*TB2*Yuk3**2) + MT2*NStbapx(124)

        NStbapx(129) =
     &   MB2*(TB2*Yuk3**2*NStbapx(126)) - MT2*NStbapx(128)

        NStbapx(133) =
     &   (3*MHH2 - 8*MHp2)*TB2*Yuk3**2 + 2*MT2*NStbapx(132)

        NStbapx(135) =
     &   5*MHp2**2*TB2*Yuk3**2 - 4*MT2*(MHp2*NStbapx(115)) -
     &    MT2**2*NStbapx(134)

	NStbapx(137) = 6*MHp2*TB2*Yuk3**2 - MT2*NStbapx(136)

        NStbapx(144) =
     &   -2*(MHp2 - MT2)*NStbapx(140) + MHH2*NStbapx(143)

        NStbapx(15) = 72*MB2**2 + 3*MHp2**2 - 28*MB2*MHp2 -
     &    (2*NStbapl(6)*NStbapx(14))/MHp2

        NStbapx(151) =
     &   MHp2**2*(1 - 4*TB*Yuk3) - 2*MT2*(MHp2*(1 - 8*TB*Yuk3)) -
     &    MT2**2*(7 + 12*TB*Yuk3)

	NStbapx(154) = -1 + 4*TB*Yuk3 + 2*TB2*Yuk3**2

	NStbapx(156) = -2 + 8*TB*Yuk3 + TB2*Yuk3**2

	NStbapx(158) = 6 + 8*TB*Yuk3 - 7*TB2*Yuk3**2

        NStbapx(170) =
     &   40*MHp2**3 - 12*MT2**3 - 273*MT2**2*MHp2 + 53*MT2*MHp2**2

	NStbapx(172) = 196*TB2*Yuk3 + TB*(7 - 2*TB2*Yuk3**2)

	NStbapx(174) = -2*MT2**6 + MHp2**6*(1 - TB2*Yuk3**2)

	NStbapx(175) = 196*TB2*Yuk3 + TB*(247 + 7*TB2*Yuk3**2)

	NStbapx(176) = 1596*TB2*Yuk3 + TB*(302 + 73*TB2*Yuk3**2)

	NStbapx(177) = 228*TB2*Yuk3 - TB*(230 + 97*TB2*Yuk3**2)

	NStbapx(178) = 2524*TB2*Yuk3 + 3*TB*(790 + 123*TB2*Yuk3**2)

	NStbapx(179) = 1940*TB2*Yuk3 + TB*(738 + 251*TB2*Yuk3**2)

	NStbapx(181) = 10060*TB2*Yuk3 + TB*(1202 - 2513*TB2*Yuk3**2)

	NStbapx(182) = 1960*TB2*Yuk3 - 11*TB*(1 + 4*TB2*Yuk3**2)

	NStbapx(183) = 7852*TB2*Yuk3 + TB*(1481 + 173*TB2*Yuk3**2)

	NStbapx(184) = 1116*TB2*Yuk3 + TB*(155 + 777*TB2*Yuk3**2)

	NStbapx(186) = 1840*TB2*Yuk3 + TB*(265 - 1587*TB2*Yuk3**2)

	NStbapx(187) = 980*TB2*Yuk3 + TB*(14 - 31*TB2*Yuk3**2)

	NStbapx(188) = 2724*TB2*Yuk3 + TB*(279 + 1726*TB2*Yuk3**2)

	NStbapx(190) = 10008*TB2*Yuk3 + TB*(2843 - 1236*TB2*Yuk3**2)

	NStbapx(191) = 2060*TB2*Yuk3 + TB*(221 + 1336*TB2*Yuk3**2)

	NStbapx(192) = 4200*TB2*Yuk3 + TB*(539 + 1482*TB2*Yuk3**2)

        NStbapx(195) =
     &   -383 + 319*TB2*Yuk3**2 - 32*SW2**2*(39 + 2*TB2*Yuk3**2) +
     &    32*NStbapx(194)

        NStbapx(198) =
     &   -20 + 11*TB2*Yuk3**2 - 64*SW2*(4 - 7*TB2*Yuk3**2) +
     &    64*NStbapx(197)

	NStbapx(200) = 76*TB2*Yuk3 - TB*(1 - 3*TB2*Yuk3**2)

        NStbapx(201) =
     &   67*MHp2**6*TB2*Yuk3**2 +
     &    3000*MT2**3*(MHp2**3*(1 - TB2*Yuk3**2))

	NStbapx(202) = 2222*TB2*Yuk3 + TB*(600 + 131*TB2*Yuk3**2)

	NStbapx(203) = 7852*TB2*Yuk3 + TB*(173 + 1481*TB2*Yuk3**2)

        NStbapx(206) =
     &   -1300 + 2843*TB2*Yuk3**2 -
     &    64*SW2**2*(27 - 25*TB2*Yuk3**2) + 64*NStbapx(205)

        NStbapx(209) =
     &   -33 - 14*TB2*Yuk3**2 - 32*SW2*(4 - 7*TB2*Yuk3**2) +
     &    32*NStbapx(208)

	NStbapx(211) = 196*TB2*Yuk3 - TB*(2 - 7*TB2*Yuk3**2)

        NStbapx(212) =
     &   5*MHp2**7*TB2*Yuk3**2 -
     &    83*MT2**2*(MHp2**5*(1 - TB2*Yuk3**2))

	NStbapx(213) = 228*TB2*Yuk3 - TB*(121 + 206*TB2*Yuk3**2)

	NStbapx(214) = 4200*TB2*Yuk3 + TB*(1482 + 539*TB2*Yuk3**2)

	NStbapx(215) = 2222*TB2*Yuk3 + TB*(131 + 600*TB2*Yuk3**2)

        NStbapx(218) =
     &   -1651 + 265*TB2*Yuk3**2 -
     &    160*SW2**2*(1 - 2*TB2*Yuk3**2) + 32*NStbapx(217)

        NStbapx(221) =
     &   -2577 + 1202*TB2*Yuk3**2 - 32*SW2*(52 + 73*TB2*Yuk3**2) +
     &    32*NStbapx(220)

        NStbapx(225) =
     &   5*MHp2**3 + 8*MT2**3 - 6*MT2**2*MHp2 - 13*MT2*MHp2**2

	NStbapx(227) = TB + 2*TB2*(Yuk3 - 2*TB*Yuk3**2)

	NStbapx(229) = 4*TB - TB2*(12*Yuk3 - 5*TB*Yuk3**2)

	NStbapx(230) = 33*TB - TB2*(2*Yuk3 + 37*TB*Yuk3**2)

	NStbapx(231) = 31*TB - TB2*(54*Yuk3 + 73*TB*Yuk3**2)

        NStbapx(233) =
     &   3*MHp2**5*TB*Yuk3**2 - 10*MT2**5*(Yuk3*(4 + TB*Yuk3))

	NStbapx(234) = 17*TB - 20*TB2*(Yuk3 - 7*TB*Yuk3**2)

	NStbapx(235) = 67*TB + 8*TB2*(37*Yuk3 + 5*TB*Yuk3**2)

	NStbapx(236) = 5*TB + TB2*(37*Yuk3 + 83*TB*Yuk3**2)

	NStbapx(238) = 8*TB + TB2*(4*Yuk3 - 11*TB*Yuk3**2)

	NStbapx(239) = 57*TB + TB2*(46*Yuk3 - 9*TB*Yuk3**2)

	NStbapx(240) = 7*TB + TB2*(2*Yuk3 + TB*Yuk3**2)

	NStbapx(241) = 23*TB + TB2*(6*Yuk3 + 17*TB*Yuk3**2)

	NStbapx(242) = 109*TB + TB2*(28*Yuk3 + 19*TB*Yuk3**2)

	NStbapx(243) = 32*MT2**6 + 3*MHp2**6*(1 - TB2*Yuk3**2)

	NStbapx(245) = 37*TB + TB2*(2*Yuk3 - 33*TB*Yuk3**2)

	NStbapx(246) = 5*TB - 2*TB2*(5*Yuk3 + 2*TB*Yuk3**2)

	NStbapx(247) = 18*TB + TB2*(17*Yuk3 + 66*TB*Yuk3**2)

	NStbapx(248) = 40*TB + TB2*(296*Yuk3 + 67*TB*Yuk3**2)

	NStbapx(249) = TB - TB2*(13*Yuk3 + 72*TB*Yuk3**2)

        NStbapx(250) =
     &   7*MHp2**6*TB2*Yuk3**2 +
     &    76*MT2**3*(MHp2**3*(1 - TB2*Yuk3**2))

	NStbapx(252) = 10*TB2*Yuk3 + TB*(4 - 5*TB2*Yuk3**2)

	NStbapx(253) = 4*TB2*Yuk3 - 11*TB*(1 - 3*TB2*Yuk3**2)

	NStbapx(254) = 4*TB2*Yuk3 + 5*TB*(3 + 4*TB2*Yuk3**2)

	NStbapx(256) = 4*TB2*Yuk3 + 11*TB*(3 - TB2*Yuk3**2)

	NStbapx(257) = 2*TB2*Yuk3 - TB*(4 - TB2*Yuk3**2)

	NStbapx(258) = 13*TB2*Yuk3 + TB*(72 - TB2*Yuk3**2)

	NStbapx(259) = 3*TB2*Yuk3 + 10*TB*(1 + TB2*Yuk3**2)

	NStbapx(260) = 12*TB2*Yuk3 - TB*(5 + 4*TB2*Yuk3**2)

	NStbapx(261) = 20*TB2*Yuk3 - TB*(140 + 17*TB2*Yuk3**2)

	NStbapx(264) = (MHp2 - MT2)**2 - MB2*(MHp2 + MT2)

	NStbapx(266) = (MHp2 - MT2)**2 - MB2*NStbapx(265)

        NStbapx(268) =
     &   (3*MHp2 - 8*MT2)*(MHp2 - MT2)**2 + 5*MB2**2*MHp2 -
     &    8*MB2*MHp2**2

	NStbapx(308) = 8*MB2**3 + MB2*(MHp2*(5*MHp2 + 22*MT2))

	NStbapx(309) = 9*MHp2*TB*Yuk3 + 4*MT2*(1 + 3*TB*Yuk3)

	NStbapx(311) = 8*MB2*(MHp2 + 3*MT2) - MHp2*NStbapx(310)

	NStbapx(314) = 3*MHp2**2 - 3*MT2**2 - 4*MT2*MHp2

        NStbapx(316) =
     &   10*MT2**2 + MT2*(13*MHp2 + 4*(11*MHp2 + 6*MT2)*TB*Yuk3) +
     &    TB2*Yuk3**2*NStbapx(315)

        NStbapx(318) =
     &   13*MT2**3 - 21*MT2**2*MHp2 +
     &    MT2*(26*MHp2**2 + 16*(2*MHp2 - MT2)**2*TB*Yuk3) +
     &    2*TB2*Yuk3**2*NStbapx(317)

	NStbapx(320) = MT2*(3 + 8*TB*Yuk3) + TB2*Yuk3**2*NStbapx(319)

        NStbapx(326) =
     &   3*MHp2**5*TB2*Yuk3**2 - MT2**4*(MHp2*NStbapx(321)) +
     &    4*MT2**3*(MHp2**2*NStbapx(322)) -
     &    2*MT2**2*(MHp2**3*NStbapx(323)) -
     &    MT2*(MHp2**4*NStbapx(324)) - MT2**5*NStbapx(325)

        NStbapx(333) =
     &   -(MT2*(MHp2**3*NStbapx(328))) -
     &    MT2**2*(MHp2**2*NStbapx(329)) + MHp2**4*NStbapx(327) +
     &    NStbapx(332)

        NStbapx(338) =
     &   13*MHp2**4*TB2*Yuk3**2 - MT2**4*NStbapx(334) +
     &    6*MT2*(MHp2**3*NStbapx(335)) -
     &    2*MT2**2*(MHp2*(MT2*NStbapx(337) + 2*MHp2*NStbapx(336)))

        NStbapx(341) =
     &   2*MB2**2*((MHp2 - 4*MT2)*TB2*Yuk3**2) + MT2*NStbapx(340)

	NStbapx(37) = 4*TB2*Yuk3 + TB*(29 - TB2*Yuk3**2)

        NStbapx(393) =
     &   -(3*MT2**3*((MHp2 - MT2)**3*NStbapx(342))) +
     &    MB2**5*NStbapx(346) +
     &    MB2*MT2**2*((MHp2 - MT2)*NStbapx(352)) +
     &    MB2**6*NStbapx(355) - MB2**4*NStbapx(366) -
     &    MB2**2*MT2*NStbapx(379) + 2*NStbapx(392)

        NStbapx(396) =
     &   3*MHp2**2 - MT2**2*(1 - 8*TB*Yuk3) +
     &    4*MT2*(MHp2*(1 - 2*TB*Yuk3))

        NStbapx(398) =
     &   6*MHp2*Yuk3*(4*TB - TB2*Yuk3) + MT2*NStbapx(397)

        NStbapx(403) =
     &   3*MHp2**4*TB2*Yuk3**2 - 4*MT2**3*(MHp2*NStbapx(399)) +
     &    MT2**4*NStbapx(400) - 2*MT2*(MHp2**3*NStbapx(401)) +
     &    6*MT2**2*(MHp2**2*NStbapx(402))

        NStbapx(407) =
     &   MHp2**3*Yuk3*(4*TB + TB2*Yuk3) +
     &    2*MT2**2*(MHp2*NStbapx(404)) +
     &    MT2*(MHp2**2*NStbapx(405)) + MT2**3*NStbapx(406)

        NStbapx(412) =
     &   -(MT2**4*NStbapx(409)) + 2*MT2**3*(MHp2*NStbapx(410)) -
     &    2*(MT2*(MHp2**3*NStbapx(408)) +
     &       MT2**2*(MHp2**2*NStbapx(411))) + MHp2**4*NStbapx(327)

        NStbapx(415) =
     &   6*MHp2**2*Yuk3*(4*TB - TB2*Yuk3) + MT2**2*NStbapx(413) +
     &    2*MT2*(MHp2*NStbapx(414))

	NStbapx(47) = 2*MHp2*Yuk3*(8*TB - TB2*Yuk3) + MT2*NStbapx(46)

	NStbapx(48) = 4*TB*Yuk3 - TB2*Yuk3**2

        NStbapx(493) =
     &   84*MB2**8*(TB*TB2*Yuk3**2) -
     &    2*MT2**3*(MHp2*(MHp2 - MT2)**3*NStbapx(448)) +
     &    MB2**7*NStbapx(450) + MB2**6*NStbapx(453) +
     &    MB2**5*NStbapx(457) + 2*MB2**4*NStbapx(464) +
     &    MB2**3*NStbapx(474) + MB2*MT2**2*NStbapx(481) -
     &    MB2**2*MT2*NStbapx(492)

	NStbapx(50) = 2 + 4*TB*Yuk3 - TB2*Yuk3**2

	NStbapx(51) = -1 + 4*TB*Yuk3 - TB2*Yuk3**2

	NStbapx(53) = 6 + 4*TB*Yuk3 + TB2*Yuk3**2

        NStbapx(532) =
     &   (MT2 + MB2*(TB2*Yuk3**2))*Re(NStbapl(2)) -
     &    (2*MT2 + MB2*(TB2*Yuk3**2))*Re(NStbapl(6))

	NStbapx(63) = 3*MHp2**2 - 56*MT2**2 + 2*MT2*MHp2

	NStbapx(64) = MHH2*MHp2 - MHp2**2 - 2*MT2*(2*MHH2 - 3*MHp2)

	NStbapx(77) = (MB2 + MT2)*TB + 4*MB2*(TB2*Yuk3)

	NStbapx(86) = 37*MHp2*TB2*Yuk3**2 - MT2*NStbapx(85)

	NStbapx(88) = 26*MHp2*TB2*Yuk3**2 - MT2*NStbapx(87)

        NStbapx(92) = 8*MHp2**3*TB2*Yuk3**2 -
     &    2*MT2**2*(MHp2*(14 + TB2*Yuk3**2)) +
     &    MT2**3*NStbapx(90) - MT2*(MHp2**2*NStbapx(91))

        NStbapx(96) = 3*MHp2**3*TB2*Yuk3**2 -
     &    MT2**3*NStbapx(93) - MT2*(MHp2**2*NStbapx(94)) +
     &    MT2**2*(MHp2*NStbapx(95))

        NStbapx(97) = MHp2**2*TB2*Yuk3**2 -
     &    2*MHp2*(MT2 + MT2*(TB2*Yuk3**2)) -
     &    MT2**2*(2 - TB2*Yuk3**2)

	NStbapx(99) = MT2 - 2*(MHp2 + MT2)*TB2*Yuk3**2

        NStbapx(105) =
     &   MHH2**2*NStbapx(99) - MHH2*NStbapx(102) +
     &    MHp2*NStbapx(104)

        NStbapx(109) =
     &   MB2*((MB2 - MHH2 + MT2)*TB2*Yuk3**2) + MT2*NStbapx(108)

	NStbapx(11) = 18*MB2**2 + 2*MHp2**2 - 13*MB2*MHp2

        NStbapx(113) =
     &   MB2*(TB2*Yuk3**2*NStbapx(110)) - MT2*NStbapx(112)

        NStbapx(117) =
     &   MT2*(MHH2 - 2*MHp2 + 2*MT2) + 2*MB2**2*(TB2*Yuk3**2) +
     &    MB2*NStbapx(116)

        NStbapx(119) =
     &   MHH2*(MT2 + MB2*(TB2*Yuk3**2)) +
     &    1/(MB2 - MT2)*((MHH2 - MHp2)*NStbapx(118))

	NStbapx(12) = MB2**3 + (5*MB2 - MHp2)*NStbapl(2)**2

	NStbapx(121) = MHp2**2 - MHH2*(2*MHp2 - MT2)

        NStbapx(122) =
     &   MHH2*(MT2 + 4*MT2*(TB*Yuk3)) + TB2*Yuk3**2*NStbapx(121)

        NStbapx(13) = 16*MB2**3 - MHp2**3 - 15*MB2**2*MHp2 +
     &    7*MB2*MHp2**2

        NStbapx(130) =
     &   1/(MB2 - MT2)*NStbapx(129) +
     &    (MHp2*(MB2 - MHp2 + MT2)*NStbapx(125))/NStbapx(123)

        NStbapx(145) =
     &   MT2*((MHp2 - MT2)**2*(3*MHH2 - 4*MHp2 + 2*MT2)) +
     &    2*MB2**4*(TB2*Yuk3**2) + MB2**3*NStbapx(133) +
     &    MB2**2*(2*NStbapx(135) - MHH2*NStbapx(137)) +
     &    MB2*NStbapx(144)

        NStbapx(152) =
     &   MHH2*(MHp2 - MT2)**2*(1 - 4*TB*Yuk3) - MHp2*NStbapx(151)

        NStbapx(153) =
     &   MT2 - 2*(MHp2 - 3*MT2)*TB2*Yuk3**2 + 4*MT2*(TB*Yuk3)

	NStbapx(155) = 2*MHp2*TB2*Yuk3**2 + MT2*NStbapx(154)

        NStbapx(157) =
     &   MHp2**2*TB2*Yuk3**2 + 2*MT2*(MHp2*NStbapx(51)) +
     &    MT2**2*NStbapx(156)

        NStbapx(159) =
     &   MHp2**2*TB2*Yuk3**2 - 2*MHp2*(MT2 + MT2*(TB2*Yuk3**2)) +
     &    MT2**2*NStbapx(158)

	NStbapx(16) = 25*MHp2**2 + 4/MB2*(NStbapl(2)*NStbapx(15))

        NStbapx(162) =
     &   MHH2*(MT2 + MB2*(TB2*Yuk3**2)) -
     &    1/(MB2 - MT2)*((MHH2 - MHp2)*NStbapx(118))

        NStbapx(168) =
     &   2*MT2*((8*MB2 - MHp2 - 4*MT2)*Yuk3) +
     &    (MB2 - MT2)*((4*MB2 - 5*MHp2)*TB*Yuk3**2)

        NStbapx(171) =
     &   -(12*MB2**8*((MHp2 - 4*MT2)*TB2*Yuk3**2)) +
     &    MT2**2*(MHp2*(MHp2 - MT2)**3*NStbapx(170))

	NStbapx(173) = -237*MHp2*TB*TB2*Yuk3**2 + 4*MT2*NStbapx(172)

        NStbapx(18) = 49*MB2 +
     &    (12*(6*MB2 - MHH2)*NStbapl(7))/MHH2 + 8*NStbapl(14)

        NStbapx(180) =
     &   4*MT2**5*(MHp2*NStbapx(175)) -
     &    MT2**2*(MHp2**4*NStbapx(176)) +
     &    MT2*(MHp2**5*NStbapx(177)) +
     &    MT2**3*(MHp2**3*NStbapx(178)) -
     &    MT2**4*(MHp2**2*NStbapx(179)) - 24*TB*NStbapx(174)

        NStbapx(185) =
     &   926*MHp2**4*TB*TB2*Yuk3**2 +
     &    MT2**2*(MHp2**2*NStbapx(181)) + 16*MT2**4*NStbapx(182) +
     &    4*MT2**3*(MHp2*NStbapx(183)) -
     &    4*MT2*(MHp2**3*NStbapx(184))

        NStbapx(189) =
     &   836*MHp2**3*TB*TB2*Yuk3**2 +
     &    4*MT2**2*(MHp2*NStbapx(186)) + 16*MT2**3*NStbapx(187) -
     &    MT2*(MHp2**2*NStbapx(188))

	NStbapx(19) = 88*MB2 - MHH2*(19 - (6*NStbapl(7))/MHH2)

        NStbapx(199) =
     &   312*MHp2**5*TB*TB2*Yuk3**2 +
     &    MT2**3*(MHp2**2*NStbapx(190)) -
     &    2*MT2*(MHp2**4*NStbapx(191)) +
     &    4*(MT2**2*(MHp2**3*NStbapx(192)) +
     &       MT2**4*(MHp2*TB*NStbapx(195))) +
     &    16*MT2**5*(1960*TB2*Yuk3 - TB*NStbapx(198))

        NStbapx(210) =
     &   24*MT2*(MHp2**5*NStbapx(200)) -
     &    4*MT2**2*(MHp2**4*NStbapx(202)) +
     &    4*MT2**5*(MHp2*NStbapx(203)) -
     &    MT2**4*(MHp2**2*(10008*TB2*Yuk3 + TB*NStbapx(206))) -
     &    16*MT2**6*(980*TB2*Yuk3 - TB*NStbapx(209)) +
     &    TB*NStbapx(201)

        NStbapx(222) =
     &   16*MT2**7*NStbapx(211) - MT2*(MHp2**6*NStbapx(213)) +
     &    4*MT2**3*(MHp2**4*NStbapx(215)) -
     &    4*(MT2**4*(MHp2**3*NStbapx(214)) +
     &       MT2**6*(MHp2*(1840*TB2*Yuk3 + TB*NStbapx(218)))) +
     &    MT2**5*(MHp2**2*(10060*TB2*Yuk3 + TB*NStbapx(221))) -
     &    8*TB*NStbapx(212)

        NStbapx(226) =
     &   8*MB2**8*((MHp2 - 4*MT2)*TB2*Yuk3**2) +
     &    MT2**2*(MHp2*(MHp2 - MT2)**3*NStbapx(225))

	NStbapx(228) = -15*MHp2*TB*TB2*Yuk3**2 + 4*MT2*NStbapx(227)

        NStbapx(232) =
     &   18*MHp2**4*TB*TB2*Yuk3**2 -
     &    160*MT2**4*(TB + 4*TB2*Yuk3) -
     &    14*MT2**2*(MHp2**2*NStbapx(229)) -
     &    8*MT2**3*(MHp2*NStbapx(230)) + MT2*(MHp2**3*NStbapx(231))

        NStbapx(237) =
     &   296*MT2**4*(MHp2*TB*(1 - TB2*Yuk3**2)) +
     &    MT2**2*(MHp2**3*NStbapx(234)) -
     &    2*MT2**3*(MHp2**2*NStbapx(235)) +
     &    2*MT2*(MHp2**4*NStbapx(236)) - 16*TB2*NStbapx(233)

        NStbapx(244) =
     &   -(2*MT2**4*(MHp2**2*NStbapx(238))) +
     &    MT2**3*(MHp2**3*NStbapx(239)) -
     &    8*MT2**5*(MHp2*NStbapx(240)) +
     &    MT2*(MHp2**5*NStbapx(241)) -
     &    MT2**2*(MHp2**4*NStbapx(242)) + TB*NStbapx(243)

        NStbapx(251) =
     &   4*MT2**5*(MHp2*NStbapx(245)) - 16*MT2**6*NStbapx(246) +
     &    MT2*(MHp2**5*NStbapx(247)) -
     &    MT2**4*(MHp2**2*NStbapx(248)) +
     &    MT2**2*(MHp2**4*NStbapx(249)) - 2*TB*NStbapx(250)

        NStbapx(255) =
     &   29*MHp2**3*TB*TB2*Yuk3**2 + 32*MT2**3*NStbapx(252) -
     &    8*MT2**2*(MHp2*NStbapx(253)) -
     &    2*MT2*(MHp2**2*NStbapx(254))

        NStbapx(262) =
     &   5*MHp2**7*TB*TB2*Yuk3**2 - 8*MT2**6*(MHp2*NStbapx(256)) -
     &    32*MT2**7*NStbapx(257) +
     &    2*MT2**3*(MHp2**4*NStbapx(258)) -
     &    2*MT2*(MHp2**6*NStbapx(259)) +
     &    14*MT2**5*(MHp2**2*NStbapx(260)) -
     &    MT2**4*(MHp2**3*NStbapx(261))

        NStbapx(267) =
     &   MB2*(TB2*Yuk3**2*NStbapx(264)) + MT2*NStbapx(266)

	NStbapx(303) = 5*MHp2**2 + 8*MT2**2 - 18*MT2*MHp2

        NStbapx(304) =
     &   MHp2**2*(5 - 2*TB*Yuk3) - 2*MT2*(MHp2*(11 - 4*TB*Yuk3)) +
     &    8*MT2**2*(3 + TB*Yuk3)

        NStbapx(305) =
     &   MHp2**2*TB2*Yuk3 + 4*MT2**2*(3*TB + 2*TB2*Yuk3) -
     &    2*MT2*(MHp2*(2*TB + 3*TB2*Yuk3))

	NStbapx(307) = 8*MB2**2 + MHp2**2 - 6*MB2*MHp2

	NStbapx(31) = 22*TB + TB2*(20*Yuk3 - 3*TB*Yuk3**2)

        NStbapx(312) =
     &   -(2*MB2**2*NStbapx(309)) - MT2*NStbapx(311) +
     &    TB*Yuk3*NStbapx(308)

	NStbapx(33) = 3*TB - 2*TB2*(10*Yuk3 + 11*TB*Yuk3**2)

        NStbapx(339) =
     &   MT2**2*((MHp2 - MT2)**3*NStbapx(314)) -
     &    MB2**5*NStbapx(316) + MB2**4*NStbapx(318) +
     &    MB2**6*NStbapx(320) + MB2**2*NStbapx(326) +
     &    MB2*MT2*((MHp2 - MT2)*NStbapx(333)) - MB2**3*NStbapx(338)

	NStbapx(36) = (MB2 + MT2)*TB + 2*MB2*(TB2*Yuk3)

        NStbapx(38) = 23*MT2**2*TB +
     &    MB2**2*(TB2*Yuk3*(4 + 5*TB*Yuk3)) - MB2*MT2*NStbapx(37)

	NStbapx(4) = 18*MHH2**2 - MHH2*MHp2 - 5*MHp2**2

        NStbapx(416) =
     &   MB2**6*(Yuk3*(8*TB - TB2*Yuk3)) +
     &    MT2**2*((MHp2 - MT2)**2*NStbapx(396)) -
     &    MB2**5*NStbapx(398) + MB2**2*NStbapx(403) -
     &    2*MB2**3*NStbapx(407) + MB2*MT2*NStbapx(412) +
     &    MB2**4*NStbapx(415)

	NStbapx(45) = 4*(MHH2 - 3*MHp2)*TB - (MHH2 + 7*MHp2)*TB2*Yuk3

        NStbapx(49) = MHp2*NStbapx(47) -
     &    MHH2*(MT2 + 2*(MHp2 + MT2)*NStbapx(48))

	NStbapx(5) = 7*(MHH2 - MHp2) + 2/MB2**2*(MHp2*NStbapl(2)**2)

        NStbapx(52) = MHp2**2*Yuk3*(4*TB - TB2*Yuk3) +
     &    MT2**2*NStbapx(50) - 2*MT2*(MHp2*NStbapx(51))

        NStbapx(531) =
     &   4*MB2*(TB2*Yuk3) +
     &    1/MT2*((MB2 - MHp2 + MT2)*TB*(MT2 + MB2*(TB2*Yuk3**2)))

        NStbapx(533) =
     &   1/MT2*NStbapx(532) - Re(NStbapl(7)) +
     &    (MHH2 - 4*MT2)*Re(NStbapl(12)) + MHp2*Re(NStbapl(13))

        NStbapx(54) = MHp2**2*Yuk3*(4*TB - TB2*Yuk3) +
     &    2*MHp2*(MT2 + MT2*(TB2*Yuk3**2)) - MT2**2*NStbapx(53)

	NStbapx(58) = 3*MHp2**2 + 72*MT2**2 - 28*MT2*MHp2

	NStbapx(59) = 2*MHp2**2 + 18*MT2**2 - 13*MT2*MHp2

        NStbapx(60) = MHp2**3 - 16*MT2**3 + 15*MT2**2*MHp2 -
     &    7*MT2*MHp2**2

	NStbapx(61) = 5*MHp2**2 + 32*MT2**2 - 30*MT2*MHp2

        NStbapx(65) = 9*MHH2**2*(MHp2 - 4*MT2) +
     &    MHp2*(2*MT2*(5*MHp2 + 28*MT2) - 16*NStbapl(1)**2) +
     &    MHH2*NStbapx(63) + 16*NStbapl(1)*NStbapx(64)

        NStbapx(66) = MHH2*(MHH2 - 2*MHp2)*(MHp2 - 4*MT2)*
     &     NStbapl(17) + MHp2*(MHH2*MHp2 - 2*MT2*(MHH2 + MHp2))*
     &     NStbapl(19)

	NStbapx(67) = MHH2*(MHp2 - 8*MT2) + 2*MT2*(MHp2 + 4*MT2)

	NStbapx(68) = MHH2*(3*MHp2 - 8*MT2) - 2*MHp2*(MHp2 - 2*MT2)

	NStbapx(69) = 3*MHH2 - 2*(MHp2 + MT2) - 4*NStbapl(1)

	NStbapx(7) = 8*MB2**2 + MHH2*MHp2 - 2*MB2*(4*MHH2 - MHp2)

	NStbapx(72) = MHH2 - 2*MT2*(3 + 1/MT2*NStbapl(1))

        NStbapx(78) = MB2*(MB2 + MT2)*(TB*TB2*Yuk3**2) +
     &    MT2*NStbapx(77)

	NStbapx(80) = TB - TB2*(4*Yuk3 + 29*TB*Yuk3**2)

        NStbapx(84) = MHH2**2 + 3*MHH2*MHp2 + 8*MHp2**2 -
     &    17*MT2*(MHH2 - MHp2)

        NStbapx(89) = MHH2**2*TB2*Yuk3**2 + MHH2*NStbapx(86) -
     &    MHp2*NStbapx(88)

        NStbapx(98) = MHp2*NStbapx(92) + MHH2*NStbapx(96) +
     &    MHH2**2*NStbapx(97)

	NStbapx(1) = -(MHH2*MHp2) + MHp2**2 + 2*MB2*(2*MHH2 - 3*MHp2)

        NStbapx(106) =
     &   -(17*MB2**4*((MHH2 - MHp2)*TB2*Yuk3**2)) +
     &    MT2*((MHp2 - MT2)**2*NStbapx(84)) + MB2**3*NStbapx(89) +
     &    MB2*NStbapx(98) + MB2**2*NStbapx(105)

        NStbapx(114) =
     &   1/(MB2 - MT2)*((MHH2 - MHp2)*NStbapx(113)) +
     &    MHH2*NStbapx(109)

        NStbapx(120) =
     &   MHH2**2*NStbapx(117) + 2*MHH2*NStbapl(1)*NStbapx(119)

        NStbapx(131) =
     &   MB2**2*(MHH2*TB2*Yuk3**2) + MT2*NStbapx(121) +
     &    MB2*NStbapx(122) + (MHH2 - MHp2)*NStbapx(130)

        NStbapx(160) =
     &   MB2**3*((MHH2 - MHp2)*TB2*Yuk3**2) + MT2*NStbapx(152) -
     &    MB2**2*(MHp2*NStbapx(153) + MHH2*NStbapx(155)) +
     &    MB2*(MHH2*NStbapx(157) - MHp2*NStbapx(159))

        NStbapx(169) =
     &   -(2*MT2**2*((4*MB2 - MHp2)*TB)) + MB2*(TB2*NStbapx(168))

        NStbapx(17) = -8*MHp2**3 + 56*MB2**2*MHp2 +
     &    MB2*NStbapx(16) + 8*NStbapl(15)*NStbapx(11) -
     &    16*NStbapx(12) + (16*NStbapl(6)*NStbapx(13))/MHp2

	NStbapx(2) = -3*MHH2*MHp2 + 2*MHp2**2 + 4*MB2*(2*MHH2 - MHp2)

        NStbapx(20) = 5*MHH2**2 -
     &    4*MB2**2*(7 - 6/MB2**2*NStbapl(2)**2) +
     &    4*MB2*(-(1/MB2*(NStbapl(2)*NStbapx(19))) +
     &       14*NStbapl(14)) - MHH2*NStbapx(18)

        NStbapx(223) =
     &   MB2**7*((MHp2 - 4*MT2)*NStbapx(173)) +
     &    MB2*MT2*((MHp2 - MT2)*NStbapx(180)) -
     &    MB2**5*NStbapx(185) + MB2**6*NStbapx(189) +
     &    MB2**4*NStbapx(199) + MB2**3*NStbapx(210) +
     &    MB2**2*NStbapx(222) + TB*NStbapx(171)

	NStbapx(25) = MB2*(MHH2 - 10*MT2) + MT2*(MHH2 + 2*MT2)

        NStbapx(263) =
     &   2*MB2**7*((MHp2 - 4*MT2)*NStbapx(228)) +
     &    MB2**5*NStbapx(232) + MB2**4*NStbapx(237) -
     &    MB2*MT2*((MHp2 - MT2)*NStbapx(244)) -
     &    2*MB2**3*NStbapx(251) + MB2**6*NStbapx(255) -
     &    MB2**2*NStbapx(262) + TB*NStbapx(226)

	NStbapx(27) = MHH2*TB*Yuk3 - 2*MT2*(4 + 5*TB*Yuk3)

        NStbapx(30) = -(MT2**2*(6*MHH2 + 11*MT2)) +
     &    11*MB2**3*(TB2*Yuk3**2)

        NStbapx(306) =
     &   MT2**2*NStbapx(303) - MB2*MT2*NStbapx(304) -
     &    2*MB2**2*(Yuk3*NStbapx(305))

        NStbapx(313) =
     &   2*MT2**2*(TB*NStbapx(307)) - MB2*(TB2*Yuk3*NStbapx(312))

	NStbapx(32) = 2*MHH2*TB*(1 - TB2*Yuk3**2) + MT2*NStbapx(31)

	NStbapx(34) = 18*MHH2*TB*TB2*Yuk3**2 + MT2*NStbapx(33)

        NStbapx(39) = MT2*
     &    (2*NStbapl(1)**2*NStbapx(36) - NStbapl(1)*NStbapx(38))

        NStbapx(42) = (4*MB2 - MHH2)*MHH2*NStbapl(14) -
     &    (4*MB2 - MHp2)*MHp2*NStbapl(15)

	NStbapx(44) = MB2**2 + (MHp2 - MT2)**2 - 2*MB2*(MHp2 + MT2)

        NStbapx(509) =
     &   1/MT2*((MT2 - MB2*(TB2*Yuk3**2))*Re(NStbapl(1))) -
     &    (MB2 - MHp2 + MT2)*Re(NStbapl(4)) - MHp2*Re(NStbapl(5))

	dup1 = 2*Re(NStbapl(2)) - MHp2*Re(NStbapl(3))

        NStbapx(511) =
     &   MB2*(dup1*TB2*Yuk3**2) +
     &    MT2*(2*Re(NStbapl(1)) - MHp2*Re(NStbapl(5))) -
     &    (MT2 + MB2*(TB2*Yuk3**2))*
     &     (Re(NStbapl(1)) + Re(NStbapl(2)))

        NStbapx(516) =
     &   (MHH2 - 4*MT2)*Re(NStbapl(12)) + MHp2*Re(NStbapl(13))

        NStbapx(518) =
     &   2*MT2**2*(3 + 1/MT2*Re(NStbapl(1))) +
     &    MT2*(NStbapx(516) - Re(NStbapl(7))) +
     &    (MT2 + MB2*(TB2*Yuk3**2))*Re(NStbapl(2)) -
     &    (2*MT2 + MB2*(TB2*Yuk3**2))*Re(NStbapl(6))

        NStbapx(521) =
     &   2*Re(NStbapl(2)) - Re(NStbapl(7)) -
     &    (4*MB2 - MHH2)*Re(NStbapl(8)) + MHp2*Re(NStbapl(9))

        NStbapx(528) =
     &   2*Re(NStbapl(2)) - Re(NStbapl(7)) -
     &    (4*MB2 - MHH2)*Re(NStbapl(8)) + MHp2*Re(NStbapl(9))

        NStbapx(534) =
     &   2*TB*(1/2.D0*NStbapx(533) + Re(NStbapl(1))) -
     &    NStbapx(531)*Re(NStbapl(11))

        NStbapx(55) = MT2*((MHH2 - MHp2)*(MHp2 - MT2)**2) -
     &    MB2**3*(Yuk3*NStbapx(45)) - MB2**2*NStbapx(49) -
     &    MB2*(MHH2*NStbapx(52) - MHp2*NStbapx(54))

        NStbapx(6) = -3*MHH2*MHp2*(3*MHH2 + MHp2) +
     &    2*MB2*NStbapx(4) + 8*MB2**2*NStbapx(5)

        NStbapx(62) = 16*MT2**3 - 56*MT2**2*MHp2 -
     &    25*MT2*MHp2**2 - 16*(MHp2 - 5*MT2)*NStbapl(1)**2 -
     &    4*NStbapl(1)*NStbapx(58) - 8*NStbapl(19)*NStbapx(59) +
     &    8*(MHp2**3 + (NStbapl(6)*
     &          (2*NStbapx(60) + NStbapl(1)*NStbapx(61)))/MHp2)

        NStbapx(70) = (MHH2 - MHp2)*NStbapx(65) -
     &    8*NStbapx(66) - 4*NStbapl(6)*
     &     (MHp2*NStbapx(67) - 4*NStbapl(1)*NStbapx(68)) +
     &    4*MHH2*(MHp2 - 4*MT2)*NStbapl(7)*NStbapx(69)

	NStbapx(73) = -2*NStbapl(17) + (3*NStbapl(7)*NStbapx(72))/MHH2

        NStbapx(75) = -5*MT2**2 + 5*MB2**2*(TB2*Yuk3**2) +
     &    MB2*(MT2 - MT2*(TB2*Yuk3**2))

        NStbapx(79) = (-MB2 - MT2)*
     &     ((MT2 - MB2*(TB2*Yuk3**2))*NStbapl(7)) +
     &    (NStbapl(1)*NStbapx(78))/TB

        NStbapx(8) = 2*(MHH2*MHp2 - 2*MB2*(MHH2 + MHp2))*
     &     NStbapl(15) + NStbapl(6)*NStbapx(7)

        NStbapx(81) = 23*MB2**2*(TB*TB2*Yuk3**2) +
     &    MT2**2*(5*TB + 4*TB2*Yuk3) + MB2*MT2*NStbapx(80)

        NStbapx(83) = MHH2*(MHH2 - 4*MT2)*NStbapl(17) -
     &    MHp2*(MHp2 - 4*MT2)*NStbapl(19)

        NStbapx(9) = -(MHH2*
     &      ((3*MHH2 - 2*(MB2 + MHp2))*NStbapl(7) -
     &        2*(MHH2 - 2*MHp2)*NStbapl(14)))

        NStbapx(10) = (MHH2 - MHp2)*NStbapx(6) +
     &    4*MHp2*NStbapx(8) - 4*(4*MB2 - MHp2)*NStbapx(9)

        NStbapx(107) =
     &   (8*MT2)/(MB2 - MT2)*NStbapx(83) - NStbapx(106)/NStbapx(44)

        NStbapx(21) = NStbapx(17)/(4*MB2 - MHp2)**2 -
     &    NStbapx(20)/(4*MB2 - MHH2)

        NStbapx(224) =
     &   8/(MB2 - MT2)**2*(NStbapl(2)**2*NStbapx(169)) -
     &    1/(MB2 - MT2)*NStbapx(223)/
     &      ((MHp2 - 4*MT2)*NStbapx(44)**2)

	NStbapx(23) = MB2**2 + MT2**2 - MT2*MHH2 - MB2*(MHH2 - 6*MT2)

	NStbapx(26) = 2*MB2*((MHH2 - 4*MT2)*TB2*Yuk3) + TB*NStbapx(25)

        NStbapx(28) = 2*MB2**2*(TB*Yuk3) +
     &    MT2*(MHH2*(2 + TB*Yuk3)) + MB2*NStbapx(27)

        NStbapx(3) = -(MHH2*(4*MB2 - MHp2)*NStbapl(7)) +
     &    (MHH2 - MHp2)*NStbapx(1) + NStbapl(6)*NStbapx(2)

        NStbapx(35) = MB2*MT2*NStbapx(32) +
     &    MB2**2*NStbapx(34) + 3*TB*NStbapx(30)

        NStbapx(40) = 2*MB2*
     &     (Yuk3*(2*MT2 + (MB2 + MT2)*(TB*Yuk3))*NStbapl(2)**2) +
     &    NStbapx(39)/TB2

	NStbapx(43) = MB2*(NStbapl(2)**2 - NStbapx(42)/(MHH2 - MHp2))

	NStbapx(499) = (MB2 - MHp2 + MT2)*TB + 4*MB2*(TB2*Yuk3)

	NStbapx(501) = TB + TB2*(2*Yuk3 + TB*Yuk3**2)

	NStbapx(502) = (MB2 - MHp2)*TB*Yuk3 + MT2*(4 + TB*Yuk3)

        NStbapx(504) =
     &   (MB2 - MHp2 + MT2)*Re(NStbapl(4)) + MHp2*Re(NStbapl(5))

	NStbapx(507) = MHH2**2 - MHp2**2 + 2*MHH2*NStbapl(7)

        NStbapx(508) =
     &   MT2*NStbapx(501) +
     &    1/MB2*(TB*(MT2 - MB2*(TB2*Yuk3**2))*Re(NStbapl(2))) +
     &    TB2*Yuk3*NStbapx(502)*Re(NStbapl(4))

        NStbapx(510) =
     &   -(MT2*NStbapx(509)) +
     &    MB2*(MHp2*TB2*Yuk3**2*Re(NStbapl(3)))

        NStbapx(514) =
     &   TB2*(3*MB2*(TB*Yuk3**2) + MT2*(Yuk3*(4 + TB*Yuk3))) +
     &    MT2*(TB + 4*TB2*Yuk3)

        NStbapx(515) =
     &   4*MB2*MT2*(TB2*Yuk3) +
     &    (MB2 - MHp2 + MT2)*TB*(MT2 + MB2*(TB2*Yuk3**2))

        NStbapx(517) =
     &   MT2*(NStbapx(516) + 2*Re(NStbapl(1)) - Re(NStbapl(7))) +
     &    (MT2 + MB2*(TB2*Yuk3**2))*Re(NStbapl(2)) -
     &    (2*MT2 + MB2*(TB2*Yuk3**2))*Re(NStbapl(6))

        NStbapx(519) =
     &   2*MB2*MT2*(TB2*Yuk3*(4 + TB*Yuk3)) + TB*NStbapx(518) -
     &    NStbapx(515)*Re(NStbapl(11))

	NStbapx(520) = 3*MB2*(TB*TB2*Yuk3**2) + MT2*(TB + 4*TB2*Yuk3)

        NStbapx(522) =
     &   MB2*(TB2*Yuk3**2*NStbapx(521)) +
     &    (MT2 + MB2*(TB2*Yuk3**2))*Re(NStbapl(1)) -
     &    (MT2 + 2*MB2*(TB2*Yuk3**2))*Re(NStbapl(6))

	NStbapx(525) = MB2**2*MT2 - MT2**3

	NStbapx(526) = 3*MT2*TB + MB2*(TB2*Yuk3*(4 + TB*Yuk3))

	NStbapx(527) = 3*MB2**2 - 4*MB2*MT2 + MT2**2

        NStbapx(529) =
     &   MB2*(TB2*Yuk3**2*NStbapx(528)) +
     &    (MT2 + MB2*(TB2*Yuk3**2))*Re(NStbapl(1)) -
     &    (MT2 + 2*MB2*(TB2*Yuk3**2))*Re(NStbapl(6))

        NStbapx(536) =
     &   (3*MB2 - 4*MT2)*NStbapx(520) + MT2*NStbapx(526) +
     &    2*TB*NStbapx(517)

        NStbapx(56) = MT2*
     &    (2*NStbapl(1)**2 -
     &      (NStbapl(1)*NStbapx(55))/((MHH2 - MHp2)*NStbapx(44)))

        NStbapx(71) = NStbapx(62)/(MHp2 - 4*MT2)**2 +
     &    NStbapx(70)/((MHH2 - MHp2)**2*(MHp2 - 4*MT2))

        NStbapx(74) = 5*MHH2**2 - 28*MT2**2 +
     &    MT2*(-49*MHH2 + 56*NStbapl(17)) + 24*NStbapl(1)**2 +
     &    4*((19*MHH2 - 88*MT2)*NStbapl(1) + MHH2*NStbapx(73))

        NStbapx(76) = (MB2 + MT2)/(MB2 - MT2)**2*
     &     ((MT2 - MB2*(TB2*Yuk3**2))*NStbapl(1)) +
     &    1/(MB2 - MT2)*NStbapx(75)

	NStbapx(82) = 2*MB2*NStbapx(79) + MB2**2*NStbapx(81)/TB

        NStbapx(22) = -(MB2*
     &      ((16*NStbapl(2)*NStbapx(3) + NStbapx(10))/
     &         ((4*MB2 - MHp2)*(MHH2 - MHp2)**2) - NStbapx(21)))

        NStbapx(24) = 4*MB2*MT2*
     &     ((MHH2 - 2*(MB2 + MT2))*Yuk3)/TB -
     &    ((MT2 + MB2*(TB2*Yuk3**2))*NStbapx(23))/TB2

        NStbapx(29) = MT2*(NStbapl(17)*NStbapx(26))/TB2 +
     &    MB2*(Yuk3*NStbapl(14)*NStbapx(28))

        NStbapx(41) = 1/(MB2 - MT2)*NStbapx(35)/TB2 -
     &    4/(MB2 - MT2)**2*NStbapx(40)

        NStbapx(500) =
     &   MB2*((MB2 - MHp2 + MT2)*TB*TB2*Yuk3**2) + MT2*NStbapx(499)

        NStbapx(503) =
     &   2*MT2*NStbapx(501) -
     &    1/MB2*(TB*(MT2 - MB2*(TB2*Yuk3**2))*Re(NStbapl(2))) -
     &    TB2*Yuk3*NStbapx(502)*Re(NStbapl(4))

        NStbapx(505) =
     &   -(MT2*NStbapx(504)) -
     &    MB2*(MHp2*TB2*Yuk3**2*Re(NStbapl(3))) +
     &    (MT2 - MB2*(TB2*Yuk3**2))*Re(NStbapl(1))

	dup2 = -(TB*NStbapx(529)) + NStbapx(500)*Re(NStbapl(10))

        NStbapx(530) =
     &   (-MB2**3 + MB2*MT2**2)*NStbapx(520) -
     &    2*(MB2 - 3*MT2)*(MB2 - MT2)*
     &     (TB*NStbapx(517) - NStbapx(515)*Re(NStbapl(11))) +
     &    NStbapx(525)*NStbapx(526) + 2*dup2*NStbapx(527)

	NStbapx(57) = 2*Yuk3**2*NStbapx(43) - NStbapx(56)/TB2

        NStbapx(506) =
     &   -(MB2*(dup1*TB2*Yuk3**2)) +
     &    MT2*(-2*Re(NStbapl(1)) + MHp2*Re(NStbapl(5))) +
     &    (MT2 + MB2*(TB2*Yuk3**2))*
     &     (Re(NStbapl(1)) + Re(NStbapl(2))) +
     &    ((NStbapl(6)*(MB2*NStbapx(503) + TB*NStbapx(505)))/
     &        MHp2 + NStbapx(500)*Re(NStbapl(4)))/TB

        NStbapx(288) =
     &   MT2*MHp2/(MHp2 - 4*MT2) +
     &    ((3*MB2 - MHp2 + MT2)*NStbapx(125))/NStbapx(44)

        NStbapx(163) =
     &   MT2 + MB2*(TB2*Yuk3**2) +
     &    ((MB2 - MHp2 - MT2)*NStbapx(125))/NStbapx(44)

	NStbapx(289) = -2*MT2 - MB2*(TB2*Yuk3**2) + NStbapx(288)

        NStbapx(439) =
     &   94*MHp2**4*TB2*Yuk3**2 - 80*MT2**4*NStbapx(50) +
     &    16*MT2**3*(MHp2*NStbapx(436)) -
     &    4*MT2*(MHp2**3*NStbapx(437)) -
     &    MT2**2*(MHp2**2*NStbapx(438))

        NStbapx(147) =
     &   MT2 + MB2*(TB2*Yuk3**2) + 1/(MB2 - MT2)*NStbapx(146) -
     &    ((MB2 + MHp2 - MT2)*NStbapx(125))/NStbapx(44)

	NStbapx(164) = 1/(MB2 - MT2)*NStbapx(146) - NStbapx(163)

        NStbapx(270) =
     &   24*MT2**3 - 16*MT2**2*(MB2 - MHp2) -
     &    4*MT2*(TB*Yuk3*NStbapx(44)) +
     &    (MHp2 - 4*MT2)*TB2*Yuk3**2*NStbapx(269)

        NStbapx(273) =
     &   MT2 + MB2*(TB2*Yuk3**2) -
     &    ((MB2 + MHp2 - MT2)*NStbapx(272))/NStbapx(44)

        NStbapx(287) =
     &   NStbapx(281)/NStbapx(44)**2 - NStbapx(286)/NStbapx(282)

        NStbapx(290) =
     &   -((MB2 + MT2)/(MB2 - MT2)**2*(MT2 - MB2*(TB2*Yuk3**2))) +
     &    2*(MB2*((MB2 - MHp2 - MT2)*NStbapx(125))/
     &         NStbapx(44)**2 + 1/(MB2 - MT2)*NStbapx(289)) -
     &    (2*NStbapx(273))/MHp2

        NStbapx(299) =
     &   -(MB2*(9 + 4*(3 - 2*SW2)*SW2)*
     &       ((MB2 - MHp2 - MT2)*NStbapx(125))/NStbapx(44)**2) +
     &    1/MT2*NStbapx(291) + NStbapx(298)/NStbapx(292)

        NStbapx(447) =
     &   MT2**2*(MHp2**2*(MHp2 - MT2)**2*NStbapx(417)) -
     &    MB2**6*NStbapx(420) + MB2**5*NStbapx(423) -
     &    MB2**2*NStbapx(429) + 2*MB2**3*NStbapx(435) -
     &    MB2**4*NStbapx(439) + MB2*MT2*(MHp2*NStbapx(446)) -
     &    16*NStbapx(441)

        NStbapx(148) =
     &   MHH2*(MT2 + MB2*(TB2*Yuk3**2)) +
     &    (MHH2 - MHp2)*NStbapx(147)

        NStbapx(161) =
     &   1/(MB2 - MT2)*(MT2 - MB2*(TB2*Yuk3**2)) +
     &    (2*MHp2*NStbapx(125))/((MHH2 - MHp2)*NStbapx(44))

        NStbapx(165) =
     &   MHH2*(MT2 + MB2*(TB2*Yuk3**2)) -
     &    (MHH2 - MHp2)*NStbapx(164)

        NStbapx(300) =
     &   18/(MB2 - MT2)*(MT2*NStbapx(268) + MB2*NStbapx(270))/
     &      ((MHp2 - 4*MT2)*NStbapx(44)) +
     &    36*(1 - 2*SW2)**2*NStbapx(273)/MHp2 + 4*NStbapx(287) +
     &    9*NStbapx(290) -
     &    2*((9 - 8*(12 - 13*SW2)*SW2)*
     &        ((MB2**2 - MB2*(MHp2 + MT2))*NStbapx(125))/
     &         NStbapx(44)**2 + NStbapx(299))

        NStbapx(394) =
     &   2*NStbapl(1)**2*NStbapx(341) -
     &    (NStbapl(1)*NStbapx(393))/NStbapx(44)**2

        NStbapx(494) =
     &   (2*NStbapl(6)*NStbapx(447))/(MHp2*NStbapx(44)**2) -
     &    NStbapx(493)/
     &     ((MB - MHp - MT)**2*(MB + MHp - MT)**2*
     &       (MB - MHp + MT)**2*(MB + MHp + MT)**2*TB)

        NStbapx(149) =
     &   (MHp2**2*NStbapx(145))/NStbapx(44) +
     &    2*MHp2*NStbapl(1)*NStbapx(148)

        NStbapx(166) =
     &   NStbapl(1)*NStbapx(161) -
     &    (NStbapl(7)*NStbapx(162) - NStbapl(6)*NStbapx(165))/
     &     (MHH2 - MHp2)**2

        NStbapx(301) =
     &   288*CW2*SW2*NStbapx(267)/(MHp2*NStbapx(44)) + NStbapx(300)

        NStbapx(395) =
     &   (2*NStbapl(18)*NStbapx(339))/NStbapx(44)**2 +
     &    NStbapx(394)/(MHp2 - 4*MT2)

        NStbapx(495) =
     &   (2*NStbapl(1)*NStbapx(416))/
     &     ((MB - MHp - MT)**2*(MB + MHp - MT)**2*
     &       (MB - MHp + MT)**2*(MB + MHp + MT)**2) +
     &    NStbapx(494)/(4*MB2 - MHp2)

        NStbapx(150) =
     &   2*NStbapl(16)*NStbapx(114) +
     &    (NStbapl(7)*NStbapx(120))/MHH2 -
     &    2*NStbapl(18)*NStbapx(131) -
     &    (NStbapl(6)*NStbapx(149))/MHp2

        NStbapx(167) =
     &   MB2**2/(MB2 - MT2)*
     &     NStbapx(160)/((MHH2 - MHp2)*NStbapx(44)) -
     &    2*MB2*NStbapx(166)

        NStbapx(302) =
     &   -(18/(MB2 - MT2)*NStbapx(263)/
     &        ((4*MB2 - MHp2)*(MHp2 - 4*MT2)*TB*NStbapx(44)**2)) +
     &    MHp2*NStbapl(1)*NStbapx(301)

        NStbapx(496) =
     &   -2*((NStbapl(19)*NStbapx(306))/(MHp2 - 4*MT2) +
     &       (NStbapl(15)*NStbapx(313))/((4*MB2 - MHp2)*TB)) +
     &    NStbapx(395) + NStbapl(2)*NStbapx(495)

        NStbapx(512) =
     &   MB2*MT2*(MHp2*NStbapx(501)) -
     &    (2*MHH2 - MHp2)*(TB*NStbapx(511) -
     &       NStbapx(500)*Re(NStbapl(4)))

        NStbapx(523) =
     &   2*MB2*NStbapx(520) + TB*NStbapx(522) -
     &    NStbapx(500)*Re(NStbapl(10))

        NStbapx(535) =
     &   -(MB2*NStbapx(520)) + (4*MB2 - 3*MT2)*NStbapx(526) +
     &    2*(dup2 + (2*MB2 - MT2)*NStbapx(534))

        NStbapx(537) =
     &   (MB2 - 2*MT2)*dup2 + MB2*(NStbapx(515)*Re(NStbapl(11)))

        NStbapx(497) =
     &   MT2*(18*NStbapx(71) - (18*NStbapx(74))/(MHH2 - 4*MT2)) +
     &    36/(MB2 - MT2)**2*
     &     (1/MB2*(NStbapl(2)*NStbapx(82)) + NStbapx(496)) +
     &    (18*NStbapx(107))/(MHH2 - MHp2) +
     &    72*(1/MB2*(NStbapl(2)*NStbapx(167)) +
     &       NStbapl(7)*NStbapx(76) + NStbapx(150)/(MHH2 - MHp2)**2
     &       ) - (9*NStbapx(224))/((4*MB2 - MHp2)*TB) -
     &    (8*NStbapl(6)*NStbapx(302))/MHp2

        NStbapx(513) =
     &   -(NStbapx(507)*(MB2*NStbapx(508) + TB*NStbapx(510))) +
     &    2*NStbapl(6)*NStbapx(512)

        NStbapx(524) =
     &   MB2*NStbapx(514) - 1/MT2*(NStbapl(1)*NStbapx(519)) -
     &    1/MB2*(NStbapl(2)*NStbapx(523)) +
     &    TB*(3*MT2**2 + 2*NStbapx(517)) +
     &    2*(TB*NStbapx(522) - NStbapx(500)*Re(NStbapl(10))) -
     &    2*NStbapx(515)*Re(NStbapl(11))

        NStbapx(538) =
     &   MT2*(NStbapl(1)*NStbapx(535)) -
     &    NStbapl(2)*(MB2*NStbapx(536) - 2*NStbapx(537))

        NStbapx(498) =
     &   72/(MB2 - MT2)**2*
     &     (NStbapl(16)*NStbapx(24) - NStbapx(29)/TB) -
     &    72/(MB2 - MT2)*NStbapx(57) - 18*Yuk3**2*NStbapx(22) +
     &    (9*NStbapx(41))/TB + NStbapx(497)/TB2

        NStbapx(539) =
     &   -(1/(MB2 - MT2)**2*(NStbapx(530) - 2*NStbapx(538))) +
     &    (2*NStbapx(513))/(MHH2 - MHp2)**2 - 4*NStbapx(524)

        NStbA0eqHp = Alfa2/(MW2**2*Pi**2*SW2**2)*
     &    (1/6144.D0*NStbapx(498) -
     &      3/512.D0*(4*NStbapx(506) - NStbapx(539)/TB)/TB2)

	end
