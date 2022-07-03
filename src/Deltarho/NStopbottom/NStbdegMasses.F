c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich


* NStbdegMasses.F
* generated 29-Jan-2017 14:12

C> @brief This subroutine calculates \f$\delta\rho^{(2)}_\mathrm{tb,NS}\f$
C> with degenerate masses of the non-standard scalars.
C>
C> @ingroup group_rho
	subroutine calcNStbdegMasses
	implicit none


#include "NStbdegMasses.h"

	ComplexType dup1

	RealType T134sub

	external T134sub

	ComplexType A0FH, B0FH

	external A0FH, B0FH

#include "paraTHDM.h"
#include "paraSM.h"
	NStbdl(1) = A0FH(MB2)

	NStbdl(10) = T134sub(MT2,MHH2,MB2)

	NStbdl(2) = A0FH(MHH2)

	NStbdl(3) = A0FH(MT2)

	NStbdl(4) = B0FH(MB2,MB2,MHH2)

	NStbdl(5) = B0FH(MB2,MHH2,MT2)

	NStbdl(6) = B0FH(MT2,MB2,MHH2)

	NStbdl(7) = B0FH(MT2,MHH2,MT2)

	NStbdl(8) = T134sub(MHH2,MB2,MB2)

	NStbdl(9) = T134sub(MT2,MT2,MHH2)

        NStbdx(121) = -3*MT2*(21 - 72*SW2 + 56*SW2**2) +
     &    (9 + 12*SW2 + 56*SW2**2)*MHH2

        NStbdx(122) = -(MT2**2*(36 - 117*SW2 + 112*SW2**2)) +
     &    MT2*(27 - 84*SW2 + 56*SW2**2)*MHH2 +
     &    (9 - 33*SW2 + 56*SW2**2)*MHH2**2

        NStbdx(124) = 5*MB2**3 - 10*(MHH2 - MT2)**3 -
     &    2*MB2**2*(10*MHH2 - 9*MT2) +
     &    MB2*((MHH2 - MT2)*(25*MHH2 + 33*MT2))

        NStbdx(125) = (MHH2 - MT2)**3 +
     &    MB2**2*(MHH2 - 3*MT2) -
     &    2*MB2*((MHH2 - MT2)*(MHH2 + 2*MT2))

	NStbdx(130) = (MHH2 - MT2)**2 - MB2*(MHH2 + 5*MT2)

	NStbdx(140) = 12*TB*Yuk3 + (9 - 8*SW2)*(TB2*Yuk3**2)

	NStbdx(309) = -2 + 2*TB2*Yuk3**2 - SW2*(2 - 3*TB2*Yuk3**2)

        NStbdx(104) = MB2**2 + 2*MHH2**2 + 5*MT2**2 -
     &    7*MT2*MHH2 - 5*MB2*(MHH2 - 2*MT2)

	NStbdx(106) = -3*(MHH2 - MT2)**2 + 2*MB2*(3*MHH2 + MT2)

	NStbdx(109) = MB2 - 2*MHH2 + MT2 + 4*MB2*(TB*Yuk3)

	NStbdx(115) = -9 + 2*SW2**2 - 18*TB*Yuk3

	NStbdx(120) = MT2**4 + MT2**2*(MB2**2 + 3*MHH2**2)

        NStbdx(123) = 30*MB2**3*SW2 +
     &    (-9 + 48*SW2 - 56*SW2**2)*(MHH2 - MT2)**3 -
     &    MB2**2*NStbdx(121) + 2*MB2*NStbdx(122)

        NStbdx(126) = -9*(3*MB2 - 2*MHH2)*(MB2 - MHH2)**2 -
     &    4*MB2**2*(27 - 54*SW2 + 56*SW2**2)*
     &     ((MB2 - MHH2 - MT2)*TB*Yuk3) + 6*SW2*NStbdx(124) +
     &    56*SW2**2*NStbdx(125)

        NStbdx(131) = -(2*MB2*
     &       ((3*MB2 - MHH2 + MT2)*TB*Yuk3)) + NStbdx(130)

	NStbdx(138) = 18*TB + (9 - 8*SW2**2)*(TB2*Yuk3)

	NStbdx(141) = 16*SW2**2*(MB2 - MHH2) + 3*MB2*NStbdx(140)

	NStbdx(224) = 13 + 80*TB*Yuk3 + 20*TB2*Yuk3**2

	NStbdx(225) = 11 + 80*TB*Yuk3 + 22*TB2*Yuk3**2

	NStbdx(226) = 2 - 2*TB2*Yuk3**2 + SW2*NStbdx(224)

	NStbdx(228) = -3 + 80*TB*Yuk3 + 20*TB2*Yuk3**2

	NStbdx(229) = -5 + 80*TB*Yuk3 + 22*TB2*Yuk3**2

	NStbdx(230) = 2 - 2*TB2*Yuk3**2 + SW2*NStbdx(228)

	NStbdx(294) = 560*TB2*Yuk3 + TB*(97 + 14*TB2*Yuk3**2)

	NStbdx(295) = 72*TB2*Yuk3 - TB*(23 + 99*TB2*Yuk3**2)

	NStbdx(299) = 96*TB2*Yuk3 + TB*(17 - 5*TB2*Yuk3**2)

	NStbdx(300) = 48*TB2*Yuk3 + TB*(23 + 5*TB2*Yuk3**2)

	NStbdx(302) = -1 + TB2*Yuk3**2 - SW2*(1 - 2*TB2*Yuk3**2)

	NStbdx(310) = SW2**2*(2 - 3*TB2*Yuk3**2) - CW2*NStbdx(309)

	NStbdx(317) = SW2**2*(1 - 2*TB2*Yuk3**2) - CW2*NStbdx(302)

	NStbdx(36) = -2 + 25*SW2 + 2*TB2*Yuk3**2

        NStbdx(103) = (3*MHH2 - 10*MT2)*(MHH2 - MT2)**2 +
     &    5*MB2**2*MHH2 - 8*MB2*MHH2**2

        NStbdx(105) = 3*MB2**3*((MHH2 - 4*MT2)*TB*Yuk3) -
     &    2*MB2*MT2*NStbdx(104)

        NStbdx(107) = 12*MT2**3 - 6*MT2**2*(3*MB2 - 4*MHH2) -
     &    (MHH2 - 4*MT2)*TB2*Yuk3**2*NStbdx(106)

        NStbdx(110) = MB2*((MB2 - 2*MHH2 + MT2)*TB2*Yuk3**2) +
     &    MT2*NStbdx(109)

	NStbdx(114) = 3*MB2*(3 + 4*SW2) - 4*SW2**2*(MB2 + MHH2 - MT2)

	NStbdx(116) = (3 - 2*SW2)**2*(MHH2 - MT2) + 2*MB2*NStbdx(115)

	NStbdx(118) = MB2 - MHH2 + MT2 + 4*MB2*(TB*Yuk3)

        NStbdx(127) = -(9*MT2**3*(7*MB2 + 6*MHH2)) +
     &    MB2*(TB2*Yuk3**2*NStbdx(123)) - MT2*NStbdx(126) +
     &    18*NStbdx(120)

        NStbdx(132) = MB2**2*
     &     ((MB2 - MHH2 + 3*MT2)*TB2*Yuk3**2) - MT2*NStbdx(131)

        NStbdx(139) = 8*SW2**2*(MB2 - MHH2 - MT2) +
     &    MB2*(Yuk3*NStbdx(138))

        NStbdx(142) = MT2**2*(9 + 8*(3 - 2*SW2)*SW2) +
     &    MB2*(3 - 4*SW2)**2*((MB2 - MHH2)*TB2*Yuk3**2) +
     &    MT2*NStbdx(141)

	NStbdx(199) = -16 + 272*TB*Yuk3 + 115*TB2*Yuk3**2

	NStbdx(200) = -20 + 272*TB*Yuk3 + 119*TB2*Yuk3**2

	NStbdx(201) = 4 - 4*TB2*Yuk3**2 + SW2*NStbdx(199)

	NStbdx(203) = -16 + 80*TB*Yuk3 + 19*TB2*Yuk3**2

	NStbdx(204) = -20 + 80*TB*Yuk3 + 23*TB2*Yuk3**2

	NStbdx(205) = 4 - 4*TB2*Yuk3**2 + SW2*NStbdx(203)

	NStbdx(210) = -109 + 376*TB*Yuk3 + 105*TB2*Yuk3**2

	NStbdx(211) = 14 + 560*TB*Yuk3 + 97*TB2*Yuk3**2

	NStbdx(213) = -119 + 296*TB*Yuk3 + 138*TB2*Yuk3**2

	NStbdx(214) = -121 + 296*TB*Yuk3 + 140*TB2*Yuk3**2

	NStbdx(215) = 2 - 2*TB2*Yuk3**2 + SW2*NStbdx(213)

	NStbdx(217) = -23 + 40*TB*Yuk3 + 10*TB2*Yuk3**2

	NStbdx(218) = -25 + 40*TB*Yuk3 + 12*TB2*Yuk3**2

	NStbdx(219) = 2 - 2*TB2*Yuk3**2 + SW2*NStbdx(217)

	NStbdx(222) = 53 + 80*TB*Yuk3 + 86*TB2*Yuk3**2

        NStbdx(223) = MHH2**4*(32*TB + 5*TB2*Yuk3) -
     &    MT2*(MHH2**3*(120*TB + 49*TB2*Yuk3))

        NStbdx(227) = 123 - 64*TB*Yuk3 - 151*TB2*Yuk3**2 -
     &    4*SW2*NStbdx(225) +
     &    4*(SW2**2*NStbdx(224) + CW2*NStbdx(226))

        NStbdx(231) = -5 - 236*TB*Yuk3 - 31*TB2*Yuk3**2 +
     &    4*SW2**2*NStbdx(228) -
     &    4*(SW2*NStbdx(229) - CW2*NStbdx(230))

	NStbdx(256) = 10 + 7*TB*Yuk3 - 28*TB2*Yuk3**2

	NStbdx(257) = 23 + 70*TB*Yuk3 + 36*TB2*Yuk3**2

	NStbdx(259) = 2 + 13*TB*Yuk3 + 10*TB2*Yuk3**2

	NStbdx(261) = 81 + 50*TB*Yuk3 + 8*TB2*Yuk3**2

	NStbdx(262) = 28 + 4*TB*Yuk3 + 29*TB2*Yuk3**2

	NStbdx(263) = 178 + 128*TB*Yuk3 + 51*TB2*Yuk3**2

	NStbdx(264) = 129 + 52*TB*Yuk3 + 59*TB2*Yuk3**2

	NStbdx(266) = -35 + 22*TB*Yuk3 + 58*TB2*Yuk3**2

	NStbdx(267) = 2 + 55*TB*Yuk3 + 13*TB2*Yuk3**2

	NStbdx(268) = 9 + 16*TB*Yuk3 + 30*TB2*Yuk3**2

	NStbdx(269) = 89 + 52*TB*Yuk3 + 107*TB2*Yuk3**2

	NStbdx(271) = -17 + 26*TB*Yuk3 + 100*TB2*Yuk3**2

	NStbdx(272) = 14 + 47*TB*Yuk3 - 2*TB2*Yuk3**2

	NStbdx(273) = 95 + 132*TB*Yuk3 + 126*TB2*Yuk3**2

	NStbdx(275) = 3 + 12*TB*Yuk3 - 53*TB2*Yuk3**2

	NStbdx(276) = -59 + 18*TB*Yuk3 + 34*TB2*Yuk3**2

	NStbdx(277) = -26 + 31*TB*Yuk3 + 14*TB2*Yuk3**2

	NStbdx(278) = 8 + 12*TB*Yuk3 + 55*TB2*Yuk3**2

	NStbdx(279) = 37 + 8*(TB*Yuk3 + TB2*Yuk3**2)

	NStbdx(283) = 12*TB2*Yuk3 + TB*(2 - 9*TB2*Yuk3**2)

	NStbdx(285) = 108*TB2*Yuk3 + TB*(23 - 31*TB2*Yuk3**2)

	NStbdx(286) = 208*TB2*Yuk3 + TB*(10 - 9*TB2*Yuk3**2)

	NStbdx(288) = 768*TB2*Yuk3 + TB*(127 - 319*TB2*Yuk3**2)

	NStbdx(289) = 328*TB2*Yuk3 + TB*(67 - 34*TB2*Yuk3**2)

	NStbdx(290) = 144*TB2*Yuk3 - TB*(11 + 65*TB2*Yuk3**2)

	NStbdx(292) = 880*TB2*Yuk3 + TB*(307 - 514*TB2*Yuk3**2)

        NStbdx(293) = -47 + (11 - 8*SW2)*(TB2*Yuk3**2) +
     &    8*(SW2**2 + CW2*(1 + SW2 - TB2*Yuk3**2))

        NStbdx(296) = MT2**2*(MHH2**2*NStbdx(294)) +
     &    MT2*(MHH2**3*NStbdx(295))

	NStbdx(298) = 16*TB2*Yuk3 + TB*(3 - 5*TB2*Yuk3**2)

        NStbdx(301) = MT2**3*(MHH2**2*NStbdx(299)) +
     &    MT2*(MHH2**4*NStbdx(300))

        NStbdx(303) = 51 + (-11 - 16*SW2)*(TB2*Yuk3**2) -
     &    16*SW2**2*(1 - 2*TB2*Yuk3**2) + 16*CW2*NStbdx(302)

	NStbdx(305) = 376*TB2*Yuk3 + TB*(105 - 109*TB2*Yuk3**2)

	NStbdx(306) = 104*TB2*Yuk3 - 3*TB*(9 - 25*TB2*Yuk3**2)

	NStbdx(307) = 8*TB2*Yuk3 - TB*(13 + 15*TB2*Yuk3**2)

        NStbdx(308) = -167 + (139 - 8*SW2)*(TB2*Yuk3**2) +
     &    8*(SW2**2 + CW2*(1 + SW2 - TB2*Yuk3**2))

        NStbdx(311) = -64 + (21 + 16*SW2)*(TB2*Yuk3**2) +
     &    16*NStbdx(310)

	NStbdx(313) = 88*TB2*Yuk3 + TB*(27 - 59*TB2*Yuk3**2)

	NStbdx(314) = 80*TB2*Yuk3 + TB*(86 + 53*TB2*Yuk3**2)

	NStbdx(315) = 136*TB2*Yuk3 + TB*(31 + 107*TB2*Yuk3**2)

        NStbdx(316) = -200 + (53 + 16*SW2)*(TB2*Yuk3**2) +
     &    16*NStbdx(310)

        NStbdx(318) = -11 + (11 + 16*SW2)*(TB2*Yuk3**2) +
     &    16*NStbdx(317)

	NStbdx(37) = SW2*(27 - 2*TB2*Yuk3**2) - CW2*NStbdx(36)

	NStbdx(48) = -2 + 2*TB2*Yuk3**2 + SW2*(52 - 75*TB2*Yuk3**2)

	NStbdx(58) = -2 + 2*TB2*Yuk3**2 - SW2*(12 - 5*TB2*Yuk3**2)

	NStbdx(100) = MHH2 + MT2 + 2*(MB2 + MHH2 - MT2)*TB*Yuk3

        NStbdx(108) = MT2*NStbdx(103) + MB2*NStbdx(107) +
     &    TB*Yuk3*NStbdx(105)

	NStbdx(113) = (MB2 - MHH2 - MT2)**2 - 4*MT2*MHH2

	NStbdx(117) = MB2*(TB2*Yuk3**2*NStbdx(114)) - MT2*NStbdx(116)

        NStbdx(119) = MB2*((MB2 - MHH2 + MT2)*TB2*Yuk3**2) +
     &    MT2*NStbdx(118)

        NStbdx(128) = (9 - 8*(12 - 13*SW2)*SW2)*
     &     ((MB2**2 - MB2*(MHH2 + MT2))*NStbdx(119)) -
     &    2*NStbdx(127)

	NStbdx(136) = 16*MT2*SW2**2 + MB2*(3 - 4*SW2)**2*(TB2*Yuk3**2)

	NStbdx(137) = -(MB2 + MHH2 - MT2)**2 + 4*MB2*MHH2

        NStbdx(143) = 3*MT2**2*(3 + 8*SW2) +
     &    2*MT2*NStbdx(139) + 1/MT2*((MB2 - MHH2)*NStbdx(142))

	NStbdx(161) = -6 + 8*TB*Yuk3 - TB2*Yuk3**2

        NStbdx(162) = 8*MT2 - (17*MHH2 - 8*MT2)*TB2*Yuk3**2 +
     &    12*(MHH2 + (MHH2 + 5*MT2)*TB*Yuk3)

	NStbdx(166) = 3 + 20*TB*Yuk3 - 11*TB2*Yuk3**2

	NStbdx(167) = -1 + 8*TB*Yuk3 - 6*TB2*Yuk3**2

	NStbdx(168) = -11 + 8*Yuk3*(3*TB - 8*TB2*Yuk3)

	NStbdx(171) = -28 + 6*TB*Yuk3 - TB2*Yuk3**2

	NStbdx(172) = -5 + 4*TB*Yuk3 - 5*TB2*Yuk3**2

	NStbdx(173) = -8 + 12*TB*Yuk3 - 9*TB2*Yuk3**2

	NStbdx(174) = -7 + 12*Yuk3*(TB - TB2*Yuk3)

        NStbdx(181) = MHH2*(2*MB2 + MHH2) + 6*MT2**2 -
     &    6*MT2*(MB2 + MHH2)

	NStbdx(184) = -9 + 12*TB*Yuk3 + 2*TB2*Yuk3**2

	NStbdx(185) = 27 + 160*TB*Yuk3 + 2*TB2*Yuk3**2

	NStbdx(186) = -41 + 16*TB*Yuk3 - 22*TB2*Yuk3**2

	NStbdx(187) = -157 + 128*TB*Yuk3 - 24*TB2*Yuk3**2

	NStbdx(189) = -5 + 96*TB*Yuk3 + 17*TB2*Yuk3**2

	NStbdx(190) = 1 + 16*TB*Yuk3 - 8*TB2*Yuk3**2

	NStbdx(191) = 11 + 348*TB*Yuk3 + 32*TB2*Yuk3**2

	NStbdx(193) = 5 + 124*TB*Yuk3 - 5*TB2*Yuk3**2

	NStbdx(194) = -5 + 128*TB*Yuk3 - 35*TB2*Yuk3**2

	NStbdx(196) = 75 + 104*TB*Yuk3 - 27*TB2*Yuk3**2

	NStbdx(197) = 5 + 48*TB*Yuk3 + 23*TB2*Yuk3**2

	NStbdx(198) = 107 + 136*TB*Yuk3 + 31*TB2*Yuk3**2

        NStbdx(202) = 482 - 880*TB*Yuk3 - 275*TB2*Yuk3**2 +
     &    8*SW2**2*NStbdx(199) -
     &    8*(SW2*NStbdx(200) - CW2*NStbdx(201))

        NStbdx(206) = 2 - 328*TB*Yuk3 - 35*TB2*Yuk3**2 +
     &    8*SW2**2*NStbdx(203) -
     &    8*(SW2*NStbdx(204) - CW2*NStbdx(205))

	NStbdx(208) = -59 + 88*TB*Yuk3 + 27*TB2*Yuk3**2

	NStbdx(209) = -5 + 16*TB*Yuk3 + 3*TB2*Yuk3**2

        NStbdx(212) = MT2**2*(MHH2**3*NStbdx(210)) -
     &    MT2**3*(MHH2**2*NStbdx(211))

        NStbdx(216) = 303 - 768*TB*Yuk3 - 111*TB2*Yuk3**2 +
     &    8*SW2**2*NStbdx(213) -
     &    8*(SW2*NStbdx(214) - CW2*NStbdx(215))

        NStbdx(220) = 15 - 108*TB*Yuk3 - 7*TB2*Yuk3**2 +
     &    8*SW2**2*NStbdx(217) -
     &    8*(SW2*NStbdx(218) - CW2*NStbdx(219))

        NStbdx(232) = MT2**2*(MHH2**2*NStbdx(222)) -
     &    MT2**3*(MHH2*NStbdx(227)) + 4*MT2**4*NStbdx(231) +
     &    Yuk3*NStbdx(223)

	NStbdx(237) = -1 + 4*TB*Yuk3 + 27*TB2*Yuk3**2

	NStbdx(239) = 15 + 2*TB*Yuk3 - 3*TB2*Yuk3**2

	NStbdx(240) = -4 + 3*TB*Yuk3 - TB2*Yuk3**2

	NStbdx(241) = -38 + 20*TB*Yuk3 - 3*TB2*Yuk3**2

	NStbdx(243) = 27 + 4*TB*Yuk3 - TB2*Yuk3**2

	NStbdx(244) = 17 + 5*TB*Yuk3 + TB2*Yuk3**2

	NStbdx(245) = 2 + TB*Yuk3 + 2*TB2*Yuk3**2

	NStbdx(246) = -1 + 3*TB*Yuk3 - 4*TB2*Yuk3**2

	NStbdx(248) = -3 + 2*TB*Yuk3 + 15*TB2*Yuk3**2

	NStbdx(249) = -11 + 20*TB*Yuk3 - 11*TB2*Yuk3**2

	NStbdx(252) = 1 + 5*TB*Yuk3 + 17*TB2*Yuk3**2

        NStbdx(255) = 7*MHH2**2 + 2*MT2**2*(7 + 2*TB*Yuk3) -
     &    MT2*(MHH2*(19 + 4*TB*Yuk3))

        NStbdx(258) = 103*MHH2**2*TB2*Yuk3**2 +
     &    4*MT2**2*NStbdx(256) - 2*MT2*(MHH2*NStbdx(257))

	NStbdx(26) = 260*TB2*Yuk3 + 3*TB*(83 + 21*TB2*Yuk3**2)

	NStbdx(260) = 12*MHH2*TB2*Yuk3**2 - MT2*NStbdx(259)

        NStbdx(265) = -5*MHH2**5*(1 - TB2*Yuk3**2) -
     &    4*MT2**5*(14 + 5*TB*Yuk3) +
     &    2*MT2**4*(MHH2*NStbdx(261)) -
     &    MT2*(MHH2**4*NStbdx(262)) -
     &    MT2**3*(MHH2**2*NStbdx(263)) +
     &    MT2**2*(MHH2**3*NStbdx(264))

	NStbdx(27) = 12*TB2*Yuk3 - TB*(51 + 37*TB2*Yuk3**2)

        NStbdx(270) = 45*MHH2**4*TB2*Yuk3**2 -
     &    4*MT2**3*(MHH2*NStbdx(266)) + 8*MT2**4*NStbdx(267) +
     &    2*MT2**2*(MHH2**2*NStbdx(268)) -
     &    MT2*(MHH2**3*NStbdx(269))

        NStbdx(274) = 101*MHH2**3*TB2*Yuk3**2 -
     &    2*MT2**2*(MHH2*NStbdx(271)) + 8*MT2**3*NStbdx(272) -
     &    MT2*(MHH2**2*NStbdx(273))

        NStbdx(280) = 7*MHH2**5*TB2*Yuk3**2 +
     &    MT2**2*(MHH2**3*NStbdx(275)) -
     &    4*MT2**4*(MHH2*NStbdx(276)) + 4*MT2**5*NStbdx(277) +
     &    2*MT2**3*(MHH2**2*NStbdx(278)) -
     &    MT2*(MHH2**4*NStbdx(279))

	NStbdx(282) = 5*(MHH2 + MT2)*TB + 16*(MHH2 - MT2)*TB2*Yuk3

	NStbdx(284) = 8*MHH2*TB*TB2*Yuk3**2 + MT2*NStbdx(283)

        NStbdx(287) = 110*MHH2**2*TB*TB2*Yuk3**2 +
     &    4*MT2**2*NStbdx(285) + MT2*(MHH2*NStbdx(286))

        NStbdx(291) = 138*MHH2**3*TB*TB2*Yuk3**2 +
     &    MT2**2*(MHH2*NStbdx(288)) + 4*MT2**3*NStbdx(289) +
     &    2*MT2*(MHH2**2*NStbdx(290))

        NStbdx(297) = 74*MHH2**4*TB*TB2*Yuk3**2 +
     &    MT2**3*(MHH2*NStbdx(292)) +
     &    8*MT2**4*(236*TB2*Yuk3 - TB*NStbdx(293)) + 2*NStbdx(296)

        NStbdx(304) = -(4*MT2**5*(5*TB - 16*TB2*Yuk3)) -
     &    2*MT2**2*(MHH2**3*(49*TB + 120*TB2*Yuk3)) -
     &    MT2**4*(MHH2*(128*TB2*Yuk3 - TB*NStbdx(303))) +
     &    MHH2**5*NStbdx(298) + 2*NStbdx(301)

	NStbdx(31) = -5*SW2 + 6*SW2**2 + CW2*(1 + 6*SW2)

        NStbdx(312) = 7*MHH2**5*TB*TB2*Yuk3**2 +
     &    MT2**2*(MHH2**3*NStbdx(305)) +
     &    MT2*(MHH2**4*NStbdx(307)) +
     &    MT2**4*(MHH2*(64*TB2*Yuk3 - TB*NStbdx(308))) +
     &    2*(MT2**3*(MHH2**2*NStbdx(306)) +
     &       MT2**5*(348*TB2*Yuk3 - TB*NStbdx(311)))

        NStbdx(319) = MHH2**5*TB*(4 - 11*TB2*Yuk3**2) +
     &    2*MT2**3*(MHH2**2*NStbdx(314)) -
     &    2*(MT2*(MHH2**4*NStbdx(313)) +
     &       MT2**2*(MHH2**3*NStbdx(315))) +
     &    MT2**4*(MHH2*(336*TB2*Yuk3 + TB*NStbdx(316))) -
     &    4*MT2**5*(124*TB2*Yuk3 - TB*NStbdx(318))

	NStbdx(33) = 376*TB2*Yuk3 + TB*(47 + 107*TB2*Yuk3**2)

	NStbdx(34) = 216*TB2*Yuk3 + TB*(356 + 453*TB2*Yuk3**2)

	NStbdx(38) = -55 - 100*SW2**2 + 55*TB2*Yuk3**2 + 4*NStbdx(37)

	NStbdx(46) = -2 + 2*TB2*Yuk3**2 + SW2*(25 - 12*TB2*Yuk3**2)

	NStbdx(49) = SW2*(54 - 77*TB2*Yuk3**2) - CW2*NStbdx(48)

	NStbdx(56) = -(CW2*(1 - 26*SW2)) - 27*SW2 + 26*SW2**2

	NStbdx(59) = SW2**2*(12 - 5*TB2*Yuk3**2) - CW2*NStbdx(58)

	NStbdx(94) = 72*TB - TB2*(227*Yuk3 + 80*TB*Yuk3**2)

	NStbdx(95) = 16*TB + TB2*(204*Yuk3 + 125*TB*Yuk3**2)

	NStbdx(101) = (MHH2 - MT2)**2 - MB2*NStbdx(100)

	NStbdx(11) = 80*TB2*Yuk3 + TB*(7 - 15*TB2*Yuk3**2)

	NStbdx(13) = 1648*TB2*Yuk3 + TB*(245 - 3*TB2*Yuk3**2)

	NStbdx(14) = 208*TB2*Yuk3 - 3*TB*(10 + 9*TB2*Yuk3**2)

        NStbdx(148) = 12*MB2**2 + MHH2*(3*MHH2 + 11*MT2) -
     &    2*MB2*(7*MHH2 + 4*MT2)

	NStbdx(15) = 3048*TB2*Yuk3 + 7*TB*(125 + 67*TB2*Yuk3**2)

	NStbdx(152) = 65*MHH2**2 + 28*MT2**2 + 78*MT2*MHH2

	NStbdx(153) = MHH2 + MT2 - 2*(5*MHH2 + 6*MT2)*TB*Yuk3

	NStbdx(155) = -11 + 20*TB*Yuk3 + 3*TB2*Yuk3**2

	NStbdx(156) = -14 + 12*TB*Yuk3 - TB2*Yuk3**2

        NStbdx(157) = -((5*MHH2 + 12*MT2)*TB2*Yuk3**2) -
     &    MT2*(17 - 16*TB*Yuk3)

	NStbdx(158) = 16*TB*Yuk3 - 27*(1 + TB2*Yuk3**2)

	NStbdx(16) = 1152*TB2*Yuk3 + TB*(253 + 999*TB2*Yuk3**2)

	NStbdx(160) = -7 + 8*TB*Yuk3 - 5*TB2*Yuk3**2

        NStbdx(163) = -(MT2**4*NStbdx(161)) +
     &    MT2**2*(MHH2*NStbdx(162))

	NStbdx(165) = 5*MHH2**2 + 6*MT2**2 - 14*MT2*MHH2

        NStbdx(169) = 63*MHH2**3*TB2*Yuk3**2 -
     &    4*MT2**2*(MHH2*NStbdx(166)) - 2*MT2**3*NStbdx(167) -
     &    MT2*(MHH2**2*NStbdx(168))

        NStbdx(175) = -2*
     &     (MT2**4*(11 - 4*TB*Yuk3) + MT2**3*(MHH2*NStbdx(171))) +
     &    MT2**2*(MHH2**2*NStbdx(173)) -
     &    MT2*(MHH2**3*NStbdx(174)) + MHH2**4*NStbdx(172)

	NStbdx(18) = 352*TB2*Yuk3 + TB*(83 - 193*TB2*Yuk3**2)

        NStbdx(182) = -(MHH2*(5*MHH2 - 22*MT2)) +
     &    2*TB*Yuk3*NStbdx(181)

        NStbdx(188) = MHH2**4*(11 - 4*TB2*Yuk3**2) +
     &    4*MT2**4*NStbdx(184) - MT2**3*(MHH2*NStbdx(185)) -
     &    MT2*(MHH2**3*NStbdx(186)) + MT2**2*(MHH2**2*NStbdx(187))

	NStbdx(19) = 272*TB2*Yuk3 - TB*(11 + 17*TB2*Yuk3**2)

        NStbdx(192) = 96*MHH2**3*TB*Yuk3 -
     &    2*MT2*(MHH2**2*NStbdx(189)) -
     &    21*MT2**2*(MHH2*NStbdx(190)) - 4*MT2**3*NStbdx(191)

        NStbdx(195) = -2*MHH2**2*Yuk3*(32*TB - 5*TB2*Yuk3) +
     &    4*MT2**2*NStbdx(193) + MT2*(MHH2*NStbdx(194))

	NStbdx(20) = 952*TB2*Yuk3 + TB*(155 + 1169*TB2*Yuk3**2)

        NStbdx(207) = MHH2**5*Yuk3*(16*TB + 5*TB2*Yuk3) -
     &    4*MT2**3*(MHH2**2*NStbdx(196)) -
     &    2*MT2*(MHH2**4*NStbdx(197)) +
     &    2*MT2**2*(MHH2**3*NStbdx(198)) -
     &    MT2**4*(MHH2*NStbdx(202)) + 4*MT2**5*NStbdx(206)

	NStbdx(22) = 32*MT2**6 + 3*MHH2**6*(1 - TB2*Yuk3**2)

        NStbdx(221) = -(2*MT2*(MHH2**4*NStbdx(208))) -
     &    MT2**4*(MHH2*NStbdx(216)) + 4*MT2**5*NStbdx(220) +
     &    MHH2**5*NStbdx(209) + 2*NStbdx(212)

	NStbdx(23) = 80*TB2*Yuk3 + TB*(167 + 7*TB2*Yuk3**2)

        NStbdx(233) = MT2**3*
     &     (MHH2*(7*MHH2 - 16*MT2)*(MHH2 - MT2)**3) +
     &    MB2**4*NStbdx(232)

        NStbdx(236) = 5*MHH2**2 -
     &    2*MT2*(MHH2*(5 + 2*TB*Yuk3)) + MT2**2*(7 + 4*TB*Yuk3)

	NStbdx(238) = 12*MHH2*Yuk3*(TB + 2*TB2*Yuk3) + MT2*NStbdx(237)

	NStbdx(24) = 58*TB2*Yuk3 - TB*(71 + 8*TB2*Yuk3**2)

        NStbdx(242) = 5*MHH2**4*TB2*Yuk3**2 +
     &    56*MT2**2*(MHH2**2*TB*Yuk3) -
     &    4*(MT2**3*(MHH2*NStbdx(239)) +
     &       MT2*(MHH2**3*NStbdx(240))) - MT2**4*NStbdx(241)

        NStbdx(247) = MT2**4*NStbdx(243) -
     &    4*MT2**3*(MHH2*NStbdx(244)) +
     &    8*MT2**2*(MHH2**2*NStbdx(245)) +
     &    4*MT2*(MHH2**3*NStbdx(246)) - MHH2**4*NStbdx(172)

	NStbdx(25) = 632*TB2*Yuk3 + TB*(501 + 127*TB2*Yuk3**2)

        NStbdx(250) = 4*MT2*(MHH2**2*NStbdx(245)) +
     &    2*(MHH2**3*Yuk3*(TB + 5*TB2*Yuk3) +
     &       MT2**2*(MHH2*NStbdx(248))) - MT2**3*NStbdx(249)

	NStbdx(251) = -3 + 20*TB*Yuk3 - 38*TB2*Yuk3**2

        NStbdx(253) = MHH2**2*Yuk3*(3*TB + 8*TB2*Yuk3) +
     &    MT2*(MHH2*NStbdx(252))

        NStbdx(28) = MT2**3*(MHH2**3*NStbdx(26)) +
     &    MT2*(MHH2**5*NStbdx(27))

        NStbdx(281) = 8*MB2**7*(TB2*Yuk3**2) +
     &    MT2**2*(MHH2*(MHH2 - MT2)**2*NStbdx(255)) +
     &    MB2**5*NStbdx(258) - 4*MB2**6*NStbdx(260) +
     &    MB2*MT2*NStbdx(265) + MB2**3*NStbdx(270) -
     &    MB2**4*NStbdx(274) - MB2**2*NStbdx(280)

	NStbdx(30) = 1280*TB2*Yuk3 + TB*(281 + 710*TB2*Yuk3**2)

        NStbdx(32) = 19*(1 + 2*TB2*Yuk3**2) +
     &    8*(1 - TB2*Yuk3**2)*NStbdx(31)

        NStbdx(320) = MT2**3*
     &     (MHH2*(MHH2 - MT2)**3*NStbdx(282)) -
     &    4*MB2**7*NStbdx(284) + MB2**6*NStbdx(287) -
     &    MB2**5*NStbdx(291) + MB2**4*NStbdx(297) -
     &    MB2*MT2**2*NStbdx(304) - 2*MB2**3*NStbdx(312) -
     &    MB2**2*MT2*NStbdx(319)

        NStbdx(35) = MT2*(MHH2**4*NStbdx(33)) +
     &    MT2**3*(MHH2**2*NStbdx(34))

        NStbdx(39) = 33*MHH2**5*TB2*Yuk3**2 -
     &    4*MT2**4*(MHH2*NStbdx(38))

	NStbdx(41) = 792*TB2*Yuk3 - TB*(165 - 263*TB2*Yuk3**2)

	NStbdx(42) = 80*TB2*Yuk3 - TB*(15 - 7*TB2*Yuk3**2)

        NStbdx(43) = 23*MHH2**7*TB2*Yuk3**2 -
     &    70*MT2**2*(MHH2**5*(1 - TB2*Yuk3**2))

	NStbdx(44) = 24*TB2*Yuk3 - TB*(89 + 87*TB2*Yuk3**2)

	NStbdx(45) = 1280*TB2*Yuk3 + TB*(710 + 281*TB2*Yuk3**2)

        NStbdx(47) = 201 - 91*TB2*Yuk3**2 -
     &    4*SW2*(27 - 14*TB2*Yuk3**2) +
     &    4*(SW2**2*(25 - 12*TB2*Yuk3**2) + CW2*NStbdx(46))

	NStbdx(5) = 16*MB2**2 + 5*MT2*MHH2 - 6*MB2*(MHH2 + 2*MT2)

        NStbdx(50) = 453 + 891*TB2*Yuk3**2 -
     &    8*SW2**2*(52 - 75*TB2*Yuk3**2) + 8*NStbdx(49)

	NStbdx(52) = 1648*TB2*Yuk3 - TB*(3 - 245*TB2*Yuk3**2)

	NStbdx(53) = 792*TB2*Yuk3 + TB*(263 - 165*TB2*Yuk3**2)

        NStbdx(54) = 17*MHH2**6*TB2*Yuk3**2 +
     &    319*MT2**3*(MHH2**3*(1 - TB2*Yuk3**2))

	NStbdx(55) = 128*TB2*Yuk3 - TB*(53 + 193*TB2*Yuk3**2)

        NStbdx(57) = 461 + 348*TB2*Yuk3**2 +
     &    8*(1 - TB2*Yuk3**2)*NStbdx(56)

        NStbdx(60) = 1 + 27*TB2*Yuk3**2 -
     &    8*SW2*(10 - 3*TB2*Yuk3**2) + 8*NStbdx(59)

	NStbdx(64) = 11*MHH2**2 + 24*MT2**2 - 44*MT2*MHH2

	NStbdx(66) = 80*TB + TB2*(227*Yuk3 - 72*TB*Yuk3**2)

	NStbdx(67) = TB - 2*TB2*(12*Yuk3 + 5*TB*Yuk3**2)

	NStbdx(68) = 10*TB + TB2*(37*Yuk3 + 101*TB*Yuk3**2)

	NStbdx(70) = TB + TB2*(6*Yuk3 - TB*Yuk3**2)

	NStbdx(71) = 4*TB + TB2*(25*Yuk3 + 112*TB*Yuk3**2)

	NStbdx(73) = 10*TB + TB2*(24*Yuk3 - TB*Yuk3**2)

	NStbdx(74) = 24*TB + TB2*(147*Yuk3 + 43*TB*Yuk3**2)

	NStbdx(75) = 183*TB + 4*TB2*(51*Yuk3 + 43*TB*Yuk3**2)

	NStbdx(76) = 133*TB + TB2*(136*Yuk3 + 377*TB*Yuk3**2)

        NStbdx(77) = 77*MHH2**5*TB2*Yuk3**2 +
     &    360*MT2**3*(MHH2**2*(1 - TB2*Yuk3**2))

	NStbdx(79) = 125*TB + 4*TB2*(51*Yuk3 + 4*TB*Yuk3**2)

	NStbdx(80) = 43*TB + 3*TB2*(49*Yuk3 + 8*TB*Yuk3**2)

	NStbdx(81) = 151*TB + 2*TB2*(156*Yuk3 + 395*TB*Yuk3**2)

        NStbdx(82) = 63*MHH2**4*TB2*Yuk3**2 -
     &    128*MT2**4*(1 - TB2*Yuk3**2)

	NStbdx(84) = 96*MT2**5 + 7*MHH2**5*(1 - TB2*Yuk3**2)

	NStbdx(85) = 25*TB2*Yuk3 + 4*TB*(22 + TB2*Yuk3**2)

	NStbdx(86) = 49*TB2*Yuk3 + 2*TB*(57 + 8*TB2*Yuk3**2)

	NStbdx(87) = 20*TB2*Yuk3 + TB*(43 + 46*TB2*Yuk3**2)

	NStbdx(88) = 116*TB2*Yuk3 + TB*(334 + 87*TB2*Yuk3**2)

        NStbdx(9) = 23*MHH2**3 + 40*MT2**3 - 102*MT2**2*MHH2 -
     &    33*MT2*MHH2**2

	NStbdx(90) = TB - TB2*(6*Yuk3 + TB*Yuk3**2)

	NStbdx(91) = 53*TB + 4*TB2*(5*Yuk3 + 9*TB*Yuk3**2)

	NStbdx(92) = 172*TB + 3*TB2*(68*Yuk3 + 61*TB*Yuk3**2)

        NStbdx(93) = 11*MHH2**6*TB2*Yuk3**2 +
     &    97*MT2**2*(MHH2**4*(1 - TB2*Yuk3**2))

        NStbdx(96) = MT2**5*(MHH2*NStbdx(94)) +
     &    MT2**4*(MHH2**2*NStbdx(95))

	NStbdx(99) = (MHH2 - MT2)**2 - MB2*(MHH2 + MT2)

        NStbdx(10) = 40*MB2**8*((MHH2 - 4*MT2)*TB2*Yuk3**2) +
     &    MT2**2*(MHH2*(MHH2 - MT2)**3*NStbdx(9))

	NStbdx(102) = MB2*(TB2*Yuk3**2*NStbdx(99)) + MT2*NStbdx(101)

	NStbdx(12) = -111*MHH2*TB*TB2*Yuk3**2 + 2*MT2*NStbdx(11)

	NStbdx(149) = 6*MT2*(MB2 - MHH2 - MT2) + TB*Yuk3*NStbdx(148)

        NStbdx(150) = MHH2*TB2*Yuk3*(2 - 5*TB*Yuk3) +
     &    2*MT2*(3*TB + 2*TB2*Yuk3)

	NStbdx(154) = 2*MT2*NStbdx(153) + TB2*Yuk3**2*NStbdx(152)

        NStbdx(159) = 2*MT2**3*
     &     (MHH2**2*(17 - 12*Yuk3*(TB + TB2*Yuk3))) +
     &    4*MT2**4*(MHH2*NStbdx(155)) - 2*MT2**5*NStbdx(156) -
     &    MT2**2*(MHH2**3*NStbdx(158)) + MHH2**4*NStbdx(157)

        NStbdx(164) = 29*MHH2**4*TB2*Yuk3**2 -
     &    3*MT2*(MHH2**3*NStbdx(160)) - 2*NStbdx(163)

        NStbdx(17) = 224*MHH2**4*TB*TB2*Yuk3**2 +
     &    4*MT2**3*(MHH2*NStbdx(13)) + 32*MT2**4*NStbdx(14) +
     &    MT2**2*(MHH2**2*NStbdx(15)) - MT2*(MHH2**3*NStbdx(16))

        NStbdx(170) = MT2**2*((MHH2 - MT2)**3*NStbdx(165)) +
     &    MB2**4*NStbdx(169)

        NStbdx(176) = 8*MB2**6*(TB*Yuk3) -
     &    MB2*((MHH2 - MT2)*NStbdx(175))

	NStbdx(178) = MHH2*(5 - 2*TB*Yuk3) - 12*MT2*(1 - TB*Yuk3)

	NStbdx(179) = MHH2*TB2*Yuk3 - 4*MT2*(TB + TB2*Yuk3)

        NStbdx(183) = -24*MT2**4 - 6*MT2**2*MHH2**2 +
     &    4*MT2**3*(4*MB2 + 7*MHH2) +
     &    MB2**2*((MHH2 - 4*MT2)*(MHH2 - 2*MT2)*TB2*Yuk3**2) -
     &    MB2*MT2*NStbdx(182)

        NStbdx(21) = 393*MHH2**3*TB*TB2*Yuk3**2 +
     &    8*MT2**2*(MHH2*NStbdx(18)) + 16*MT2**3*NStbdx(19) -
     &    MT2*(MHH2**2*NStbdx(20))

        NStbdx(234) = MB2**7*
     &     ((MHH2 - 4*MT2)*Yuk3*(16*TB - 5*TB2*Yuk3)) +
     &    MB2*MT2**2*((MHH2 - MT2)*NStbdx(188)) +
     &    MB2**5*NStbdx(192) + MB2**6*NStbdx(195) +
     &    MB2**3*NStbdx(207) - MB2**2*MT2*NStbdx(221) -
     &    2*NStbdx(233)

        NStbdx(254) = MB2**6*(Yuk3*(4*TB + 7*TB2*Yuk3)) +
     &    MT2**2*((MHH2 - MT2)**2*NStbdx(236)) -
     &    MB2**5*NStbdx(238) + MB2**2*NStbdx(242) -
     &    MB2*MT2*NStbdx(247) - 2*MB2**3*NStbdx(250) -
     &    MB2**4*(MT2**2*NStbdx(251) - 4*NStbdx(253))

        NStbdx(29) = -(4*MT2**5*(MHH2*NStbdx(23))) +
     &    4*MT2**2*(MHH2**4*NStbdx(24)) +
     &    MT2**4*(MHH2**2*NStbdx(25)) + 5*TB*NStbdx(22) -
     &    2*NStbdx(28)

        NStbdx(336) = MT2**2*(3 + 1/MT2*Re(NStbdl(3))) +
     &    MT2*((MHH2 - 2*MT2)*Re(NStbdl(7)))

        NStbdx(346) = 4*MB2*(TB2*Yuk3) +
     &    1/MT2*((MB2 - MHH2 + MT2)*TB*(MT2 + MB2*(TB2*Yuk3**2)))

        NStbdx(40) = 3*MT2**2*(MHH2**3*NStbdx(30)) +
     &    32*MT2**5*(208*TB2*Yuk3 - TB*NStbdx(32)) -
     &    2*(NStbdx(35) + TB*NStbdx(39))

        NStbdx(51) = -(2*MT2**3*(MHH2**4*NStbdx(41))) -
     &    16*MT2**7*NStbdx(42) + MT2*(MHH2**6*NStbdx(44)) +
     &    3*MT2**4*(MHH2**3*NStbdx(45)) +
     &    8*MT2**6*(MHH2*(352*TB2*Yuk3 - TB*NStbdx(47))) -
     &    MT2**5*(MHH2**2*(3048*TB2*Yuk3 + TB*NStbdx(50))) +
     &    TB*NStbdx(43)

	NStbdx(6) = 2*MT2*(6*MB2 - MHH2 - 2*MT2) + TB*Yuk3*NStbdx(5)

        NStbdx(61) = 2*MT2**5*(MHH2*NStbdx(52)) -
     &    MT2**2*(MHH2**4*NStbdx(53)) + MT2*(MHH2**5*NStbdx(55)) +
     &    MT2**4*(MHH2**2*(216*TB2*Yuk3 + TB*NStbdx(57))) -
     &    8*MT2**6*(272*TB2*Yuk3 - TB*NStbdx(60)) + 3*TB*NStbdx(54)

        NStbdx(65) = 24*MB2**7*((MHH2 - 4*MT2)*TB2*Yuk3**2) +
     &    MT2**2*(MHH2*(MHH2 - MT2)**3*NStbdx(64))

        NStbdx(69) = 215*MHH2**3*TB*TB2*Yuk3**2 +
     &    4*MT2**2*(MHH2*NStbdx(66)) - 32*MT2**3*NStbdx(67) -
     &    8*MT2*(MHH2**2*NStbdx(68))

        NStbdx(72) = 29*MHH2**2*TB*TB2*Yuk3**2 +
     &    16*MT2**2*NStbdx(70) - MT2*(MHH2*NStbdx(71))

        NStbdx(78) = 32*MT2**5*NStbdx(73) -
     &    8*MT2**4*(MHH2*NStbdx(74)) -
     &    2*MT2**2*(MHH2**3*NStbdx(75)) +
     &    MT2*(MHH2**4*NStbdx(76)) - TB*NStbdx(77)

        NStbdx(83) = -(4*MT2**2*(MHH2**2*NStbdx(79))) -
     &    8*MT2**3*(MHH2*NStbdx(80)) + MT2*(MHH2**3*NStbdx(81)) -
     &    3*TB*NStbdx(82)

        NStbdx(89) = -(4*MT2**4*(MHH2*NStbdx(85))) +
     &    4*MT2**3*(MHH2**2*NStbdx(86)) +
     &    MT2*(MHH2**4*NStbdx(87)) - MT2**2*(MHH2**3*NStbdx(88)) +
     &    TB*NStbdx(84)

        NStbdx(97) = 64*MT2**6*NStbdx(90) -
     &    MT2*(MHH2**5*NStbdx(91)) +
     &    2*MT2**3*(MHH2**3*NStbdx(92)) + TB*NStbdx(93) -
     &    4*NStbdx(96)

        NStbdx(151) = -(MT2**2*((8*MB2**2 + MHH2**2)*TB)) +
     &    2*MB2**2*(TB2*Yuk3*NStbdx(149)) +
     &    MB2*MT2*(MHH2*NStbdx(150))

        NStbdx(177) = 2*MB2**6*
     &     ((3*MB2 - 16*MHH2 - 11*MT2)*TB2*Yuk3**2) +
     &    MB2**5*NStbdx(154) + MB2**2*NStbdx(159) +
     &    MB2**3*NStbdx(164) + MT2*NStbdx(176) - NStbdx(170)

        NStbdx(180) = -(2*MT2**2*(3*MHH2 - 8*MT2)) +
     &    MB2*MT2*NStbdx(178) + MB2**2*(Yuk3*NStbdx(179))

        NStbdx(235) = 2*NStbdl(9)*NStbdx(183) -
     &    (NStbdl(3)*NStbdx(234))/
     &     ((MB - MHH - MT)**2*(MB + MHH - MT)**2*
     &       (MB - MHH + MT)**2*(MB + MHH + MT)**2)

	NStbdx(3) = MHH2 - 2*MT2*(1 - 1/MT2*NStbdl(3))

	NStbdx(328) = Re(NStbdl(1)) - (2*MB2 - MHH2)*Re(NStbdl(4))

	NStbdx(330) = (MB2 - MHH2 + MT2)*TB + 4*MB2*(TB2*Yuk3)

	NStbdx(334) = Re(NStbdl(3)) + (MHH2 - 2*MT2)*Re(NStbdl(7))

        NStbdx(337) = 2*NStbdx(336) +
     &    (MT2 + MB2*(TB2*Yuk3**2))*Re(NStbdl(1)) -
     &    (3*MT2 + MB2*(TB2*Yuk3**2))*Re(NStbdl(2))

        NStbdx(62) = -(MB2**5*NStbdx(17)) +
     &    MB2**6*NStbdx(21) - MB2*MT2*((MHH2 - MT2)*NStbdx(29)) +
     &    MB2**4*NStbdx(40) - MB2**2*NStbdx(51) +
     &    2*(MB2**7*((MHH2 - 4*MT2)*NStbdx(12)) +
     &       MB2**3*NStbdx(61)) + TB*NStbdx(10)

        NStbdx(7) = -(MT2**2*((4*MB2 - MHH2)*TB)) +
     &    MB2*(TB2*Yuk3*NStbdx(6))

	NStbdx(8) = MB2**2 + (MHH2 - MT2)**2 - 2*MB2*(MHH2 + MT2)

        NStbdx(98) = MB2**5*NStbdx(69) - 4*MB2**6*NStbdx(72) -
     &    MB2**3*NStbdx(78) + MB2**4*NStbdx(83) -
     &    MB2*MT2*((MHH2 - MT2)*NStbdx(89)) - MB2**2*NStbdx(97) +
     &    TB*NStbdx(65)

	NStbdx(1) = 32*MB2 - MHH2*(5 - (6*NStbdl(2))/MHH2)

        NStbdx(326) = TB2*
     &     (3*MB2*(TB*Yuk3**2) + MT2*(Yuk3*(4 + TB*Yuk3))) +
     &    MT2*(TB + 4*TB2*Yuk3)

	NStbdx(327) = 3*MB2*(TB*TB2*Yuk3**2) + MT2*(TB + 4*TB2*Yuk3)

        NStbdx(329) = -(2*MB2*(TB2*Yuk3**2*NStbdx(328))) +
     &    (MT2 + 3*MB2*(TB2*Yuk3**2))*Re(NStbdl(2)) -
     &    (MT2 + MB2*(TB2*Yuk3**2))*Re(NStbdl(3))

        NStbdx(331) = MB2*
     &     ((MB2 - MHH2 + MT2)*TB*TB2*Yuk3**2) + MT2*NStbdx(330)

        NStbdx(332) = 2*MB2*NStbdx(327) - TB*NStbdx(329) -
     &    NStbdx(331)*Re(NStbdl(5))

        NStbdx(333) = 4*MB2*MT2*(TB2*Yuk3) +
     &    (MB2 - MHH2 + MT2)*TB*(MT2 + MB2*(TB2*Yuk3**2))

        NStbdx(335) = 2*MT2*NStbdx(334) +
     &    (MT2 + MB2*(TB2*Yuk3**2))*Re(NStbdl(1)) -
     &    (3*MT2 + MB2*(TB2*Yuk3**2))*Re(NStbdl(2))

        NStbdx(338) = 2*MB2*MT2*(TB2*Yuk3*(4 + TB*Yuk3)) +
     &    TB*NStbdx(337) - NStbdx(333)*Re(NStbdl(6))

	NStbdx(340) = MB2**2*MT2 - MT2**3

	NStbdx(341) = 3*MT2*TB + MB2*(TB2*Yuk3*(4 + TB*Yuk3))

	NStbdx(342) = 3*MB2**2 - 4*MB2*MT2 + MT2**2

	dup1 = TB*NStbdx(329) + NStbdx(331)*Re(NStbdl(5))

        NStbdx(344) = (MB2 - 2*MT2)*dup1 +
     &    MB2*(NStbdx(333)*Re(NStbdl(6)))

        NStbdx(345) = (3*MB2 - 4*MT2)*NStbdx(327) +
     &    MT2*NStbdx(341) + 2*TB*NStbdx(335)

        NStbdx(4) = -4*(5*MHH2 - 32*MT2)*NStbdl(3) -
     &    24*NStbdl(3)**2 +
     &    (MHH2 - MT2)*(5*MHH2 + 28*MT2 - 8*NStbdl(9)) +
     &    12*NStbdl(2)*NStbdx(3)

        NStbdx(63) = 4/(MB2 - MT2)**2*
     &     (NStbdl(1)**2*NStbdx(7)) -
     &    1/(MB2 - MT2)*NStbdx(62)/((MHH2 - 4*MT2)*NStbdx(8)**2)

        NStbdx(2) = 24*NStbdl(1)**2 +
     &    12*(2*MB2 - MHH2)*NStbdl(2) +
     &    (MB2 - MHH2)*(28*MB2 + 5*MHH2 - 8*NStbdl(8)) -
     &    4*NStbdl(1)*NStbdx(1)

        NStbdx(339) = MB2*NStbdx(326) -
     &    1/MB2*(NStbdl(1)*NStbdx(332)) -
     &    1/MT2*(NStbdl(3)*NStbdx(338)) +
     &    TB*(3*MT2**2 + 2*NStbdx(335)) -
     &    2*(dup1 + NStbdx(333)*Re(NStbdl(6)))

        NStbdx(343) = (-MB2**3 + MB2*MT2**2)*NStbdx(327) -
     &    2*(MB2 - 3*MT2)*(MB2 - MT2)*
     &     (TB*NStbdx(335) - NStbdx(333)*Re(NStbdl(6))) +
     &    NStbdx(340)*NStbdx(341) + 2*dup1*NStbdx(342)

        NStbdx(111) = MT2 + MB2*(TB2*Yuk3**2) -
     &    ((MB2 + MHH2 - MT2)*NStbdx(110))/NStbdx(8)

        NStbdx(133) = (MB2 + MT2)*(MT2 - MB2*(TB2*Yuk3**2)) -
     &    2*(MB2 - MT2)*NStbdx(132)/NStbdx(8)

        NStbdx(134) = MT2/(MB2 - MT2)*
     &     (MHH2 - 2*MT2)/(MHH2 - 4*MT2) +
     &    MB2*((MB2 - MHH2 - MT2)*NStbdx(119))/NStbdx(8)**2

        NStbdx(112) = 1/(MB2 - MT2)*
     &     NStbdx(108)/((MHH2 - 4*MT2)*NStbdx(8)) +
     &    (1 - 2*SW2)**2*NStbdx(111)/MHH2

        NStbdx(129) = (2*NStbdx(117))/NStbdx(113) +
     &    NStbdx(128)/NStbdx(8)**2

        NStbdx(135) = -(1/(MB2 - MT2)**2*NStbdx(133)) -
     &    (2*(NStbdx(111) - MHH2*NStbdx(134)))/MHH2

        NStbdx(144) = -(MB2*(9 + 4*(3 - 2*SW2)*SW2)*
     &       ((MB2 - MHH2 - MT2)*NStbdx(119))/NStbdx(8)**2) +
     &    1/MT2*NStbdx(136) + NStbdx(143)/NStbdx(137)

        NStbdx(145) = 18*NStbdx(112) - NStbdx(129) +
     &    9*NStbdx(135) - NStbdx(144)

        NStbdx(321) = (2*NStbdl(2)*NStbdx(281))/NStbdx(8)**2 +
     &    NStbdx(320)/
     &     ((MB - MHH - MT)**2*(MB + MHH - MT)**2*
     &       (MB - MHH + MT)**2*(MB + MHH + MT)**2*TB)

        NStbdx(146) = 144*CW2*SW2*
     &     NStbdx(102)/(MHH2*NStbdx(8)) + NStbdx(145)

        NStbdx(322) = (2*NStbdl(3)*NStbdx(254))/NStbdx(8)**2 +
     &    NStbdx(321)/(4*MB2 - MHH2)

        NStbdx(347) = 1/MT2*(TB*NStbdx(335)) -
     &    NStbdx(346)*Re(NStbdl(6))

        NStbdx(147) = -(9/(MB2 - MT2)*
     &       NStbdx(98)/
     &        ((4*MB2 - MHH2)*(MHH2 - 4*MT2)*TB*NStbdx(8)**2)) +
     &    NStbdl(3)*NStbdx(146)

        NStbdx(323) = (2*NStbdl(8)*NStbdx(151))/
     &     ((4*MB2 - MHH2)*TB) -
     &    (2*NStbdl(10)*NStbdx(177))/NStbdx(8)**2 +
     &    (2*NStbdl(3)**2*NStbdx(180) + NStbdx(235))/
     &     (MHH2 - 4*MT2) + NStbdl(1)*NStbdx(322)

        NStbdx(348) = -(MB2*NStbdx(327)) +
     &    (4*MB2 - 3*MT2)*NStbdx(341) +
     &    2*(dup1 + (2*MB2 - MT2)*NStbdx(347))

        NStbdx(324) = -(9*MT2*NStbdx(4)/(MHH2 - 4*MT2)) -
     &    18/(MB2 - MT2)**2*NStbdx(323) +
     &    (9*NStbdx(63))/((4*MB2 - MHH2)*TB) +
     &    4*NStbdl(2)*NStbdx(147)

        NStbdx(349) = MT2*(NStbdl(3)*NStbdx(348)) +
     &    NStbdl(1)*(-(MB2*NStbdx(345)) + 2*NStbdx(344))

        NStbdx(325) = 9*MB2*
     &     (Yuk3**2*NStbdx(2))/(4*MB2 - MHH2) - NStbdx(324)/TB2

        NStbdx(350) = 1/(MB2 - MT2)**2*
     &     (NStbdx(343) - 2*NStbdx(349)) + 4*NStbdx(339)

        NStbdegMasses =
     &   Alfa2/(MW2**2*Pi**2*SW2**2)*
     &    (1/1536.D0*NStbdx(325) - 3/512.D0*NStbdx(350)/(TB*TB2))

	end
