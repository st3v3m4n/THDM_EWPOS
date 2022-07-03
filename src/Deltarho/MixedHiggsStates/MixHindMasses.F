c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich


* MixHindMasses.F
* generated 15-Dec-2015 17:11

C> @brief This subroutine calculates the two-loop correction \f$\delta\rho^{(2)}_\mathrm{H,Mix}\f$
C> with independent masses of the non-standard scalars.
C>
C> @ingroup group_rho

	subroutine calcMixHindMasses
	implicit none


#include "MixHindMasses.h"


	RealType T134sub

	external T134sub

	ComplexType A0FH, B0FH

	external A0FH, B0FH

#include "paraTHDM.h"
#include "paraSM.h"
	MixHil(1) = B0FH(MA02,0.D0,MHH2)

	MixHil(10) = B0FH(MHH2,0.D0,MHp2)

	MixHil(11) = B0FH(MHH2,Mh02,MHH2)

	MixHil(12) = A0FH(MHH2)

	MixHil(13) = A0FH(Mh02)

	MixHil(14) = T134sub(MHH2,MA02,0.D0)

	MixHil(15) = T134sub(Mh02,MA02,MA02)

	MixHil(16) = T134sub(MHH2,Mh02,MA02)

	MixHil(17) = T134sub(MHH2,MHH2,Mh02)

	MixHil(18) = T134sub(MHp2,MA02,0.D0)

	MixHil(19) = T134sub(MHp2,MHH2,0.D0)

	MixHil(2) = B0FH(MA02,0.D0,MHp2)

	MixHil(20) = T134sub(MHp2,MHp2,0.D0)

	MixHil(21) = T134sub(MHp2,MHp2,Mh02)

	MixHil(22) = T134sub(MHp2,Mh02,MA02)

	MixHil(23) = T134sub(MHp2,MHH2,Mh02)

	MixHil(3) = B0FH(MA02,MA02,Mh02)

	MixHil(4) = B0FH(MHp2,0.D0,MA02)

	MixHil(5) = B0FH(MHp2,0.D0,MHH2)

	MixHil(6) = B0FH(MHp2,Mh02,MHp2)

	MixHil(7) = A0FH(MHp2)

	MixHil(8) = A0FH(MA02)

	MixHil(9) = B0FH(MHH2,0.D0,MA02)

	MixHix(127) = 3*MHp2 + (MW2*SW2)/(Alfa*Pi)*Lambda5

        MixHix(176) = MA02 - 3*MHH2 - 2*(Mh02 + MHp2) +
     &    (2*MW2*SW2)/(Alfa*Pi)*Lambda5

        MixHix(179) = MA02 - 3*MHH2 - 2*(Mh02 + MHp2) +
     &    (2*MW2*SW2)/(Alfa*Pi)*Lambda5

	MixHix(1) = 2*MA02*MHH2 + 7*(MA02**2 + MHH2**2)

        MixHix(10) = MA02**2 + MHH2**2 -
     &    2*(MA02 + MHH2)*MHp2 + 2*MHp2**2

	MixHix(101) = -4*MHH2 + (3*MW2*SW2)/(Alfa*Pi)*Lambda5

	MixHix(102) = -5*MHH2 + (4*MW2*SW2)/(Alfa*Pi)*Lambda5

        MixHix(103) = 23*MHH2**2 +
     &    (3*MW2**2*SW2**2)/(Alfa2*Pi**2)*Lambda5**2 -
     &    (8*MW2*SW2)/(Alfa*Pi)*(Lambda5*MHH2)

        MixHix(106) = -23*Mh02 - 24*MHp2 +
     &    (16*MW2*SW2)/(Alfa*Pi)*Lambda5

        MixHix(107) = -4*Mh02**2 - 11*Mh02*MHp2 - 10*MHp2**2 +
     &    (4*MW2*SW2)/(Alfa*Pi)*(Lambda5*(Mh02 + 2*MHp2))

	MixHix(109) = -4*MHp2 + (3*MW2*SW2)/(Alfa*Pi)*Lambda5

        MixHix(110) = 23*MHp2**2 +
     &    (3*MW2**2*SW2**2)/(Alfa2*Pi**2)*Lambda5**2 -
     &    (8*MW2*SW2)/(Alfa*Pi)*(Lambda5*MHp2)

	MixHix(126) = -2*(MHH2 + MHp2) + (MW2*SW2)/(Alfa*Pi)*Lambda5

        MixHix(14) = MA02**2 + 3*MA02*MHH2 + MHH2**2 -
     &    5*(MA02 + MHH2)*MHp2 + 5*MHp2**2

	MixHix(141) = -2*MHp2 + (5*MW2*SW2)/(Alfa*Pi)*Lambda5

        MixHix(142) = (9*MHH2 - 5*MHp2)*MHp2 +
     &    (MW2*SW2)/(Alfa*Pi)*(Lambda5*(MHH2 - 3*MHp2))

        MixHix(173) = MA02 - 2*Mh02 - MHH2 +
     &    (2*MW2*SW2)/(Alfa*Pi)*Lambda5

	MixHix(174) = MHp2**2 + (MW2*SW2)/(Alfa*Pi)*(Lambda5*MHH2)

        MixHix(3) = (MA02 - MHH2)*(3*MA02 + MHH2) -
     &    2*(2*MA02 - MHH2)*MHp2

	MixHix(42) = Mh02*(Mh02 + MHH2) + MA02*(Mh02 + 3*MHH2)

	MixHix(61) = MHH2 + MHp2 - (MW2*SW2)/(Alfa*Pi)*Lambda5

        MixHix(7) = MA02**2 - MHH2*(3*MHH2 - 4*MHp2) +
     &    2*MA02*(MHH2 - MHp2)

	MixHix(75) = -7*Mh02 - 4*MHH2 + (8*MW2*SW2)/(Alfa*Pi)*Lambda5

	MixHix(76) = MHH2 + (MW2*SW2)/(Alfa*Pi)*Lambda5

	MixHix(80) = -7*Mh02 - 4*MHp2 + (8*MW2*SW2)/(Alfa*Pi)*Lambda5

	MixHix(81) = MHp2 + (MW2*SW2)/(Alfa*Pi)*Lambda5

	MixHix(90) = -5*MHp2 + (4*MW2*SW2)/(Alfa*Pi)*Lambda5

	MixHix(91) = -17*MHp2 + (4*MW2*SW2)/(Alfa*Pi)*Lambda5

	MixHix(92) = 17*MHp2 + (4*MW2*SW2)/(Alfa*Pi)*Lambda5

        MixHix(98) = -23*Mh02 - 24*MHH2 +
     &    (16*MW2*SW2)/(Alfa*Pi)*Lambda5

        MixHix(99) = -4*Mh02**2 - 11*Mh02*MHH2 - 10*MHH2**2 +
     &    (4*MW2*SW2)/(Alfa*Pi)*(Lambda5*(Mh02 + 2*MHH2))

        MixHix(100) = 20*MA02**3 - MA02**2*MixHix(98) -
     &    2*MA02*MixHix(99)

        MixHix(104) = 3*Mh02**3 - 2*Mh02**2*MixHix(101) -
     &    4*MHH2**2*MixHix(102) + Mh02*MixHix(103)

        MixHix(108) = 20*MA02**3 - MA02**2*MixHix(106) -
     &    2*MA02*MixHix(107)

        MixHix(11) = MA02 - 5*MHH2 +
     &    (MixHil(7)*MixHix(10))/((MA02 - MHp2)*MHp2)

        MixHix(111) = 3*Mh02**3 - 4*MHp2**2*MixHix(90) -
     &    2*Mh02**2*MixHix(109) + Mh02*MixHix(110)

	MixHix(117) = Mh02 + MHH2 - (MW2*SW2)/(Alfa*Pi)*Lambda5

	MixHix(123) = Mh02 + MHp2 - (MW2*SW2)/(Alfa*Pi)*Lambda5

        MixHix(13) = MA02**2 + 5*MA02*(MHH2 - 2*MHp2) +
     &    2*MHH2*(MHH2 + MHp2)

	MixHix(133) = MHH2**2 - 4*MHH2*MHp2 + MHp2**2

	MixHix(136) = -2*MHH2 + (5*MW2*SW2)/(Alfa*Pi)*Lambda5

	MixHix(137) = 6*MHH2 - 4*MHp2 + (MW2*SW2)/(Alfa*Pi)*Lambda5

        MixHix(138) = MHH2*(5*MHH2 - 9*MHp2) +
     &    (MW2*SW2)/(Alfa*Pi)*(Lambda5*(3*MHH2 - MHp2))

	MixHix(140) = -4*MHH2 + 6*MHp2 + (MW2*SW2)/(Alfa*Pi)*Lambda5

        MixHix(143) = -((MHH2 - MHp2)*MHp2*MixHix(141)) +
     &    Mh02*MixHix(142)

        MixHix(15) = -((MixHil(7)*MixHix(10))/(MA02 - MHH2)) +
     &    (MixHil(12)*MixHix(14))/(MA02 - MHp2)

        MixHix(157) = -3*MA02 + MHH2 - 2*(Mh02 + MHp2) +
     &    (2*MW2*SW2)/(Alfa*Pi)*Lambda5

        MixHix(160) = -3*MA02 + MHH2 - 2*(Mh02 + MHp2) +
     &    (2*MW2*SW2)/(Alfa*Pi)*Lambda5

        MixHix(2) = MA02**2*MHH2 + MA02*MHH2**2 +
     &    3*(MA02**3 + MHH2**3) + 4*(MA02 + MHH2)*MHp2**2 -
     &    MHp2*MixHix(1)

	MixHix(30) = -MHp2 + (2*MW2*SW2)/(Alfa*Pi)*Lambda5

	MixHix(31) = -7*MHH2 + MHp2 + (MW2*SW2)/(Alfa*Pi)*Lambda5

	MixHix(34) = -MHH2 + (2*MW2*SW2)/(Alfa*Pi)*Lambda5

	MixHix(35) = MHH2 - 7*MHp2 + (MW2*SW2)/(Alfa*Pi)*Lambda5

	MixHix(37) = 2*MHp2 + (MW2*SW2)/(Alfa*Pi)*Lambda5

        MixHix(4) = 2*(MA02 + MHH2)*MHp2**2 - 2*MHp2**3 +
     &    MA02*MixHix(3)

        MixHix(43) = 3*(MA02**2 + MHH2**2) +
     &    (2*MW2*SW2)/(Alfa*Pi)*(Lambda5*Mh02) - 2*MixHix(42)

        MixHix(46) = Mh02**2 + 2*MA02*(Mh02 - 5*MHH2) -
     &    6*Mh02*MHH2 + 10*MHH2**2

	MixHix(48) = -5*Mh02 + 2*MHH2 + (5*MW2*SW2)/(Alfa*Pi)*Lambda5

        MixHix(49) = Mh02*(Mh02 + 4*MHH2) -
     &    (MW2*SW2)/(Alfa*Pi)*(Lambda5*(Mh02 + 2*MHH2))

        MixHix(50) = -3*Mh02*(Mh02 + 3*MHH2) +
     &    (MW2*SW2)/(Alfa*Pi)*(Lambda5*(3*Mh02 + 5*MHH2))

	MixHix(54) = -2*MHH2 + (MW2*SW2)/(Alfa*Pi)*Lambda5

        MixHix(59) = 3*(MA02**2 + MHp2**2) -
     &    2*(Mh02**2 + Mh02*MHp2 + MA02*(Mh02 + 3*MHp2)) +
     &    (2*MW2*SW2)/(Alfa*Pi)*(Lambda5*Mh02)

        MixHix(62) = -3*(MHH2 - MHp2)**2 +
     &    2*(Mh02**2 + Mh02*MixHix(61))

        MixHix(65) = Mh02**2 + 2*MA02*(Mh02 - 5*MHp2) -
     &    6*Mh02*MHp2 + 10*MHp2**2

	MixHix(67) = -5*Mh02 + 2*MHp2 + (5*MW2*SW2)/(Alfa*Pi)*Lambda5

        MixHix(68) = Mh02*(Mh02 + 4*MHp2) -
     &    (MW2*SW2)/(Alfa*Pi)*(Lambda5*(Mh02 + 2*MHp2))

        MixHix(69) = -3*Mh02*(Mh02 + 3*MHp2) +
     &    (MW2*SW2)/(Alfa*Pi)*(Lambda5*(3*Mh02 + 5*MHp2))

	MixHix(72) = -2*MHp2 + (MW2*SW2)/(Alfa*Pi)*Lambda5

        MixHix(77) = 8*MA02**2 +
     &    (MW2**2*SW2**2)/(Alfa2*Pi**2)*Lambda5**2 -
     &    MA02*MixHix(75) - Mh02*MixHix(76)

        MixHix(8) = -2*(MA02 + MHH2)*MHp2**2 + 2*MHp2**3 +
     &    MHH2*MixHix(7)

        MixHix(82) = 8*MA02**2 +
     &    (MW2**2*SW2**2)/(Alfa2*Pi**2)*Lambda5**2 -
     &    MA02*MixHix(80) - Mh02*MixHix(81)

	MixHix(85) = 2*MHH2 + (MW2*SW2)/(Alfa*Pi)*Lambda5

	MixHix(87) = -4*(MHH2 + MHp2) + (3*MW2*SW2)/(Alfa*Pi)*Lambda5

        MixHix(88) = -6*MHH2*MHp2 - 5*(MHH2**2 + MHp2**2) +
     &    (4*MW2*SW2)/(Alfa*Pi)*(Lambda5*(MHH2 + MHp2))

        MixHix(89) = -10*MHH2*MHp2 + 23*(MHH2**2 + MHp2**2) +
     &    (3*MW2**2*SW2**2)/(Alfa2*Pi**2)*Lambda5**2 -
     &    (8*MW2*SW2)/(Alfa*Pi)*(Lambda5*(MHH2 + MHp2))

        MixHix(93) = 5*MHH2**3 - MHp2**2*MixHix(90) +
     &    MHH2*MHp2*MixHix(91) - MHH2**2*MixHix(92)

	MixHix(105) = (Mh02 - 4*MHH2)*MixHix(100) + Mh02*MixHix(104)

	MixHix(112) = (Mh02 - 4*MHp2)*MixHix(108) + Mh02*MixHix(111)

        MixHix(114) = MA02*(Mh02 - 4*MHH2) - 7*Mh02*MHH2 -
     &    8*MHH2**2 - (MW2**2*SW2**2)/(Alfa2*Pi**2)*Lambda5**2 +
     &    (MW2*SW2)/(Alfa*Pi)*(Lambda5*(Mh02 + 8*MHH2))

        MixHix(115) = Mh02**2 - 4*(3*MA02 - MHH2)*MHH2 +
     &    (2*MW2*SW2)/(Alfa*Pi)*(Lambda5*(MA02 + MHH2)) -
     &    Mh02*MixHix(85)

        MixHix(12) = 5*MA02*MHH2 + MHH2**2 +
     &    2*(MA02**2 + MHp2*MixHix(11))

        MixHix(120) = MA02*(Mh02 - 4*MHp2) - 7*Mh02*MHp2 -
     &    8*MHp2**2 - (MW2**2*SW2**2)/(Alfa2*Pi**2)*Lambda5**2 +
     &    (MW2*SW2)/(Alfa*Pi)*(Lambda5*(Mh02 + 8*MHp2))

        MixHix(121) = Mh02**2 - 4*(3*MA02 - MHp2)*MHp2 +
     &    (2*MW2*SW2)/(Alfa*Pi)*(Lambda5*(MA02 + MHp2)) -
     &    Mh02*MixHix(37)

	MixHix(132) = Mh02**2 - (MHH2 - MHp2)**2 + Mh02*(MHH2 + MHp2)

        MixHix(134) = Mh02**2 + 2*Mh02*(MHH2 + MHp2) +
     &    (MW2**2*SW2**2)/(Alfa2*Pi**2)*Lambda5**2 - 2*MixHix(133)

        MixHix(139) = Mh02**3 -
     &    2*MHH2*(MHH2 - MHp2)*MixHix(136) - Mh02**2*MixHix(137) +
     &    2*Mh02*MixHix(138)

	MixHix(144) = Mh02**3 - Mh02**2*MixHix(140) - 2*MixHix(143)

	MixHix(147) = -2*Mh02 + MHH2 + (2*MW2*SW2)/(Alfa*Pi)*Lambda5

	MixHix(148) = MHp2**2 + (MW2*SW2)/(Alfa*Pi)*(Lambda5*MA02)

        MixHix(149) = Mh02**2 + 4*(MA02**2 + MA02*Mh02) +
     &    (MW2**2*SW2**2)/(Alfa2*Pi**2)*Lambda5**2 -
     &    (2*MW2*SW2)/(Alfa*Pi)*(Lambda5*(2*MA02 + Mh02))

	MixHix(150) = MA02**2 - 2*MA02*MHp2 + MHp2**2

	MixHix(151) = MHH2**2 - 2*MHH2*MHp2 + MHp2**2

        MixHix(152) = Mh02**2 + 4*(Mh02*MHp2 + MHp2**2) +
     &    (MW2**2*SW2**2)/(Alfa2*Pi**2)*Lambda5**2 -
     &    (2*MW2*SW2)/(Alfa*Pi)*(Lambda5*(Mh02 + 2*MHp2))

        MixHix(16) = MixHix(13)/(MA02 - MHH2) -
     &    (2*MixHix(15))/(MHH2 - MHp2)

	MixHix(18) = 5*MA02 + MHH2*(3 + (10*MixHil(12))/MHH2)

	MixHix(20) = 4*MA02**2 - 15*MA02*MHp2 + 2*MHp2**2

	MixHix(21) = 4*MHH2**2 - 15*MHH2*MHp2 + 2*MHp2**2

        MixHix(22) = 14*MHH2 - 3*MHp2 +
     &    (4*(MHH2 + 2*MHp2)*MixHil(7))/MHp2

        MixHix(23) = 14*MA02 - 3*MHp2 +
     &    (4*(MA02 + 2*MHp2)*MixHil(7))/MHp2

        MixHix(25) = MA02*(MHH2 - MHp2)*
     &     (1 + (2*MixHil(8))/MA02) +
     &    MHH2*(MA02 - MHp2)*(1 + (2*MixHil(12))/MHH2)

	MixHix(29) = Mh02 + 2*MHH2 - (MW2*SW2)/(Alfa*Pi)*Lambda5

        MixHix(32) = 8*MHH2**2 +
     &    (MW2**2*SW2**2)/(Alfa2*Pi**2)*Lambda5**2 -
     &    4*MHH2*MixHix(30) - Mh02*MixHix(31)

        MixHix(36) = 8*MHp2**2 +
     &    (MW2**2*SW2**2)/(Alfa2*Pi**2)*Lambda5**2 -
     &    4*MHp2*MixHix(34) - Mh02*MixHix(35)

        MixHix(38) = Mh02**2 - 4*(3*MHH2 - MHp2)*MHp2 +
     &    (2*MW2*SW2)/(Alfa*Pi)*(Lambda5*(MHH2 + MHp2)) -
     &    Mh02*MixHix(37)

	MixHix(40) = MA02 - MHH2 - 4*MixHil(8) + 4*MixHil(12)

	MixHix(41) = -2*MA02 - Mh02 + (MW2*SW2)/(Alfa*Pi)*Lambda5

        MixHix(44) = 3*(MA02 - MHH2)**2*MixHil(14) -
     &    MixHil(16)*MixHix(43)

        MixHix(47) = Mh02**3 - 6*Mh02*(3*MA02 + Mh02)*MHH2 -
     &    2*(2*MA02 - 5*Mh02)*MHH2**2 +
     &    4*(MA02*Mh02**2 + MHH2**3) -
     &    (MW2*SW2)/(Alfa*Pi)*(Lambda5*MixHix(46))

        MixHix(5) = (MixHil(14)*MixHix(2))/(MA02 - MHp2) -
     &    (MixHil(18)*MixHix(4))/(MA02 - MHH2)

        MixHix(51) = 4*MA02**3 - 2*MA02**2*MixHix(48) +
     &    Mh02*MixHix(49) + 2*MA02*MixHix(50)

        MixHix(52) = -(Mh02*(Mh02 + 4*MHH2)) +
     &    (MW2*SW2)/(Alfa*Pi)*(Lambda5*(Mh02 + 2*MHH2))

	MixHix(53) = Mh02 - (MW2*SW2)/(Alfa*Pi)*Lambda5

	MixHix(55) = Mh02**2 - 2*(MA02**2 + MHH2**2) - Mh02*MixHix(54)

	MixHix(57) = MHH2 - MHp2 + 4*MixHil(7) - 4*MixHil(12)

	MixHix(58) = MA02 - MHp2 + 4*MixHil(7) - 4*MixHil(8)

        MixHix(6) = ((2*(MA02 + MHH2) - 5*MHp2)*MixHil(7))/
     &     MHp2 + 2*MixHil(20)

        MixHix(60) = 3*(MA02 - MHp2)**2*MixHil(18) -
     &    MixHil(22)*MixHix(59)

        MixHix(63) = 3*(MHH2 - MHp2)**2*MixHil(19) +
     &    MixHil(23)*MixHix(62)

        MixHix(66) = Mh02**3 - 6*Mh02*(3*MA02 + Mh02)*MHp2 -
     &    2*(2*MA02 - 5*Mh02)*MHp2**2 +
     &    4*(MA02*Mh02**2 + MHp2**3) -
     &    (MW2*SW2)/(Alfa*Pi)*(Lambda5*MixHix(65))

        MixHix(70) = 4*MA02**3 - 2*MA02**2*MixHix(67) +
     &    Mh02*MixHix(68) + 2*MA02*MixHix(69)

        MixHix(71) = -(Mh02*(Mh02 + 4*MHp2)) +
     &    (MW2*SW2)/(Alfa*Pi)*(Lambda5*(Mh02 + 2*MHp2))

	MixHix(73) = Mh02**2 - 2*(MA02**2 + MHp2**2) - Mh02*MixHix(72)

        MixHix(78) = MA02*MixHil(13)*MixHix(41)**2 +
     &    2*MA02**2*MixHix(77)

        MixHix(83) = MA02*MixHil(13)*MixHix(41)**2 +
     &    2*MA02**2*MixHix(82)

        MixHix(86) = Mh02**2 + 4*MHH2*(MHH2 - 3*MHp2) +
     &    (2*MW2*SW2)/(Alfa*Pi)*(Lambda5*(MHH2 + MHp2)) -
     &    Mh02*MixHix(85)

        MixHix(9) = MixHil(7)**2 +
     &    (MixHil(19)*MixHix(8))/((MA02 - MHH2)*(MA02 - MHp2))

        MixHix(94) = 3*Mh02**4 - 2*Mh02**3*MixHix(87) +
     &    16*MHH2*MHp2*MixHix(88) + Mh02**2*MixHix(89) +
     &    4*Mh02*MixHix(93)

        MixHix(96) = -4*MA02**2 - Mh02**2 +
     &    2*MA02*(Mh02 + 6*MHH2) +
     &    (MW2*SW2)/(Alfa*Pi)*(Lambda5*(Mh02 - 2*(MA02 + MHH2)))

        MixHix(97) = -4*MA02**2 - Mh02**2 +
     &    2*MA02*(Mh02 + 6*MHp2) +
     &    (MW2*SW2)/(Alfa*Pi)*(Lambda5*(Mh02 - 2*(MA02 + MHp2)))

        MixHix(113) = MixHil(8)**2*MixHix(41)*
     &     (MixHix(96)/(MA02 - MHH2)**2 -
     &       MixHix(97)/(MA02 - MHp2)**2) -
     &    MixHix(105)/(Mh02 - 4*MHH2) + MixHix(112)/(Mh02 - 4*MHp2)

        MixHix(116) = (-4*MixHil(12)*MixHix(114))/
     &     ((MA02 - MHH2)*MHH2) +
     &    (MixHil(12)**2*MixHix(29)*MixHix(115))/
     &     ((MA02 - MHH2)**2*MHH2**2)

        MixHix(135) = (2*MW2*SW2)/(Alfa*Pi)*
     &     (Lambda5*MixHix(132)) - Mh02*MixHix(134)

        MixHix(153) = 3*MA02**2 +
     &    MA02*(2*Mh02 - MHH2 - MHp2) + MHp2*MixHix(147) -
     &    2*MixHix(148) + (MA02 - MHH2)**2*Re(MixHil(1)) +
     &    2*(MA02 - MHp2)**2*Re(MixHil(2)) +
     &    MixHix(149)*Re(MixHil(3)) - MixHix(150)*Re(MixHil(4)) -
     &    MixHix(151)*Re(MixHil(5)) - MixHix(152)*Re(MixHil(6))

        MixHix(17) = MA02**2 + MHH2**2 +
     &    18*(MA02 + MHH2)*MHp2 + 34*MHp2**2 -
     &    8*(MA02*MHH2 + MixHix(5)/(MHH2 - MHp2) +
     &       MHp2*MixHix(6)) + 8*MixHix(9) +
     &    (4*MixHil(12)*MixHix(12))/(MA02 - MHH2) -
     &    4*MixHil(8)*MixHix(16)

        MixHix(19) = 17*(MA02**2 + MHH2**2) +
     &    4*(3*MA02 + 5*MHH2)*MixHil(12) +
     &    MHH2*(30*MA02 - 16*MixHil(14)) -
     &    4*(MixHil(8)**2 + MixHil(12)**2 +
     &       MA02*(4*MixHil(14) - (MixHil(8)*MixHix(18))/MA02))

        MixHix(24) = 10*(MA02**2 + MHH2**2) -
     &    2*(MHp2**2 + MixHil(7)**2) - MixHil(8)**2 -
     &    MixHil(12)**2 + MHp2*
     &     (11*(MA02 + MHH2) +
     &       2*(MixHil(18) + MixHil(19) - 2*MixHil(20))) -
     &    (MixHil(7)*(MixHix(20) + MixHix(21)))/MHp2 -
     &    MHH2*(10*MixHil(19) - (MixHil(12)*MixHix(22))/MHH2) -
     &    MA02*(10*MixHil(18) - (MixHil(8)*MixHix(23))/MA02)

        MixHix(26) = (MA02 + MHH2 - 2*MHp2)*MHp2*
     &     (1 + (2*MixHil(7))/MHp2) - MixHix(25)

        MixHix(27) = Mh02 + 2*MHp2 -
     &    ((Mh02 - 6*MHp2)*MixHil(7) + MixHil(7)**2)/MHp2 +
     &    (1 + MixHil(7)/MHp2)*MixHil(13) - MixHil(21)

	MixHix(28) = Mh02 + 2*MHp2 - (MW2*SW2)/(Alfa*Pi)*Lambda5

        MixHix(33) = (MHH2*MixHil(7)*MixHix(28)*MixHix(29))/
     &     (MHH2 - MHp2)**2 +
     &    (2*MHH2**2*MixHix(32))/((Mh02 - 4*MHH2)*(MHH2 - MHp2))

        MixHix(39) = (4*MixHil(7)*MixHix(36))/
     &     ((MHH2 - MHp2)*MHp2) +
     &    (MixHil(7)**2*MixHix(28)*MixHix(38))/
     &     ((MHH2 - MHp2)**2*MHp2**2)

        MixHix(45) = 11*(MA02 - MHH2)**2 +
     &    8*(MixHil(17)*MixHix(29) - MixHil(15)*MixHix(41)) +
     &    (4*(MA02 - MHH2)*MixHil(13)*MixHix(40) - 8*MixHix(44))/
     &     Mh02

        MixHix(56) = (MixHil(17)*MixHix(29)*MixHix(47))/
     &     (Mh02 - 4*MHH2) +
     &    (MixHil(15)*MixHix(41)*MixHix(51))/(4*MA02 - Mh02) +
     &    2*MixHil(16)*(2*MA02*MixHix(52) - MixHix(53)*MixHix(55))

        MixHix(64) = 11*
     &     ((MA02 - MHp2)**2 + (MHH2 - MHp2)**2) +
     &    8*(2*MixHil(21)*MixHix(28) + MixHil(17)*MixHix(29) -
     &       MixHil(15)*MixHix(41)) +
     &    (4*MixHil(13)*((MHH2 - MHp2)*MixHix(57) +
     &          (MA02 - MHp2)*MixHix(58)) -
     &       8*(MixHix(60) + MixHix(63)))/Mh02

        MixHix(74) = (MixHil(21)*MixHix(28)*MixHix(66))/
     &     (Mh02 - 4*MHp2) +
     &    (MixHil(15)*MixHix(41)*MixHix(70))/(4*MA02 - Mh02) +
     &    2*MixHil(22)*(2*MA02*MixHix(71) - MixHix(53)*MixHix(73))

        MixHix(79) = (MA02*MixHil(12)*MixHix(29)*MixHix(41))/
     &     (MA02 - MHH2)**2 +
     &    MixHix(78)/((4*MA02 - Mh02)*(MA02 - MHH2))

        MixHix(84) = (MA02*MixHil(7)*MixHix(28)*MixHix(41))/
     &     (MA02 - MHp2)**2 +
     &    MixHix(83)/((4*MA02 - Mh02)*(MA02 - MHp2))

        MixHix(95) = (MixHil(12)**2*MixHix(29)*MixHix(86))/
     &     (MHH2 - MHp2)**2 - MixHix(94)/(Mh02 - 4*MHp2)

	MixHix(165) = 2*Mh02 + 3*(MA02 + MHH2) - 2*MixHix(81)

	MixHix(168) = 2*Mh02 + 3*(MA02 + MHH2) - 2*MixHix(81)

	MixHix(128) = 4*MHH2**2 + MixHix(72)**2 - 4*MHH2*MixHix(127)

        MixHix(166) = (MA02 - MHH2)*MixHix(165) -
     &    (MA02 - MHH2)**2*Re(MixHil(9)) -
     &    2*(MHH2 - MHp2)**2*Re(MixHil(10)) -
     &    MixHix(29)**2*Re(MixHil(11))

        MixHix(169) = (MA02 - MHH2)*MixHix(168) +
     &    (MA02 - MHH2)**2*Re(MixHil(1)) +
     &    2*(MA02 - MHp2)**2*Re(MixHil(2)) +
     &    MixHix(41)**2*Re(MixHil(3))

        MixHix(170) = (MA02 - MHH2)**2*Re(MixHil(9)) +
     &    2*(MHH2 - MHp2)**2*Re(MixHil(10)) +
     &    MixHix(29)**2*Re(MixHil(11))

        MixHix(177) = (MHH2 - MHp2)*MixHix(176) -
     &    (MA02 - MHH2)**2*Re(MixHil(9)) -
     &    2*(MHH2 - MHp2)**2*Re(MixHil(10)) -
     &    MixHix(29)**2*Re(MixHil(11))

        MixHix(180) = (MHH2 - MHp2)*MixHix(179) +
     &    (MA02 - MHp2)**2*Re(MixHil(4)) +
     &    (MHH2 - MHp2)**2*Re(MixHil(5)) +
     &    MixHix(28)**2*Re(MixHil(6))

        MixHix(129) = Mh02**3 - 16*MHH2*MHp2*MixHix(61) -
     &    2*Mh02**2*MixHix(126) + Mh02*MixHix(128)

        MixHix(164) = 3*MA02**2 - 3*MHH2**2 +
     &    2*((MA02 - MHH2)*(Mh02 - MixHix(81)) +
     &       (MA02 - MHp2)**2*Re(MixHil(2))) +
     &    MixHix(41)**2*Re(MixHil(3)) +
     &    (MA02 - MHH2)**2*(Re(MixHil(1)) - Re(MixHil(9))) -
     &    2*(MHH2 - MHp2)**2*Re(MixHil(10)) -
     &    MixHix(29)**2*Re(MixHil(11))

	MixHix(171) = MHH2*MixHix(169) - (2*MA02 - MHH2)*MixHix(170)

        MixHix(175) = (MA02 - 2*Mh02)*MHH2 - 3*MHH2**2 -
     &    MHp2*MixHix(173) + 2*MixHix(174) +
     &    (MA02 - MHp2)**2*Re(MixHil(4)) +
     &    (MHH2 - MHp2)**2*Re(MixHil(5)) +
     &    MixHix(28)**2*Re(MixHil(6)) -
     &    (MA02 - MHH2)**2*Re(MixHil(9)) -
     &    2*(MHH2 - MHp2)**2*Re(MixHil(10)) -
     &    MixHix(29)**2*Re(MixHil(11))

	MixHix(181) = (MHH2 - 2*MHp2)*MixHix(170) + MHH2*MixHix(180)

        MixHix(118) = Mh02*MixHix(29)**2 +
     &    4*(Mh02 - 4*MHH2)*(MA02**2 + MA02*MixHix(117))

        MixHix(124) = Mh02*MixHix(28)**2 +
     &    4*(Mh02 - 4*MHp2)*(MA02**2 + MA02*MixHix(123))

        MixHix(130) = (MixHil(12)*MixHix(29)**2)/
     &     (MHH2 - MHp2) + MixHix(129)/(Mh02 - 4*MHp2)

        MixHix(154) = (MA02 - MHp2)**2 + (MHH2 - MHp2)**2 +
     &    MixHix(28)**2 - (MA02 - MHp2)**2*Re(MixHil(4)) -
     &    (MHH2 - MHp2)**2*Re(MixHil(5)) -
     &    MixHix(28)**2*Re(MixHil(6))

        MixHix(156) = (MA02 - MHH2)**2*Re(MixHil(1)) +
     &    2*(MA02 - MHp2)**2*Re(MixHil(2)) +
     &    MixHix(41)**2*Re(MixHil(3))

        MixHix(158) = (MA02 - MHp2)*MixHix(157) +
     &    (MA02 - MHp2)**2*Re(MixHil(4)) +
     &    (MHH2 - MHp2)**2*Re(MixHil(5)) +
     &    MixHix(28)**2*Re(MixHil(6))

        MixHix(161) = (MA02 - MHp2)*MixHix(160) -
     &    (MA02 - MHH2)**2*Re(MixHil(1)) -
     &    2*(MA02 - MHp2)**2*Re(MixHil(2)) -
     &    MixHix(41)**2*Re(MixHil(3))

        MixHix(162) = (MA02 - MHp2)**2*Re(MixHil(4)) +
     &    (MHH2 - MHp2)**2*Re(MixHil(5)) +
     &    MixHix(28)**2*Re(MixHil(6))

        MixHix(119) = (MixHil(12)*MixHix(29)**2)/
     &     (MA02 - MHH2) + MixHix(118)/(4*MA02 - Mh02)

        MixHix(122) = (-4*MixHil(7)*MixHix(120))/
     &     ((MA02 - MHp2)*MHp2) +
     &    (MixHil(7)**2*MixHix(28)*MixHix(121))/
     &     ((MA02 - MHp2)**2*MHp2**2)

        MixHix(125) = (MixHil(7)*MixHix(28)**2)/
     &     (MA02 - MHp2) + MixHix(124)/(4*MA02 - Mh02)

        MixHix(131) = (MixHil(7)*MixHix(28)**2)/
     &     ((Mh02 - 4*MHp2)*(MHH2 - MHp2)) -
     &    MixHix(130)/(Mh02 - 4*MHH2)

        MixHix(145) = (MixHil(17)*MixHix(29)*MixHix(139))/
     &     (Mh02 - 4*MHH2) +
     &    (MixHil(21)*MixHix(28)*MixHix(144))/(Mh02 - 4*MHp2)

        MixHix(155) = (MixHil(7)*MixHix(154))/MHp2 +
     &    (MA02 - MHp2)**2*Re(MixHil(4)) +
     &    (MHH2 - MHp2)**2*Re(MixHil(5)) +
     &    MixHix(28)**2*Re(MixHil(6))

	MixHix(159) = (MA02 - 2*MHp2)*MixHix(156) + MA02*MixHix(158)

	MixHix(163) = MHp2*MixHix(161) + (2*MA02 - MHp2)*MixHix(162)

        MixHix(146) = -MixHix(17) + MixHix(19) -
     &    4*MixHix(24) + 2*(1 + (2*MixHil(13))/Mh02)*MixHix(26) -
     &    (16*MixHix(27)*MixHix(28)**2)/(Mh02 - 4*MHp2) +
     &    (8*MixHil(12)*MixHix(33))/MHH2 -
     &    (4*MHp2**2*MixHix(39))/(Mh02 - 4*MHp2) - MixHix(45) +
     &    (4*MixHix(56))/(MA02 - MHH2)**2 + MixHix(64) -
     &    (4*MixHix(74))/(MA02 - MHp2)**2 -
     &    (MixHil(8)*(-8*MixHix(79) + 8*MixHix(84)))/MA02 -
     &    4*(MixHix(95)/(Mh02 - 4*MHH2) +
     &       MixHix(113)/(4*MA02 - Mh02)) +
     &    (4*(MHH2**2*MixHix(116) + 2*MixHil(13)*MixHix(119)))/
     &     (Mh02 - 4*MHH2) -
     &    (4*(MHp2**2*MixHix(122) + 2*MixHil(13)*MixHix(125)))/
     &     (Mh02 - 4*MHp2) - 8*MixHil(13)*MixHix(131) -
     &    (4*(2*MixHil(23)*MixHix(135) + MixHix(145)))/
     &     (MHH2 - MHp2)**2

	MixHix(167) = (MA02 - 2*MHH2)*MixHix(156) - MA02*MixHix(166)

	MixHix(178) = (2*MHH2 - MHp2)*MixHix(162) + MHp2*MixHix(177)

        MixHix(172) = (MA02 - MHH2)*(MA02 + MHH2)*
     &     MixHix(164) - 2*
     &     (MixHil(8)*MixHix(167) + MixHil(12)*MixHix(171))

        MixHix(182) = (MHH2 - MHp2)*(MHH2 + MHp2)*
     &     MixHix(175) - 2*
     &     (MixHil(7)*MixHix(178) - MixHil(12)*MixHix(181))

        MixHix(183) = MixHix(172)/(MA02 - MHH2)**2 +
     &    MixHix(182)/(MHH2 - MHp2)**2

        MixHix(184) = ((MA02 + MHp2)*MixHix(153))/
     &     (MA02 - MHp2) - 4*MixHix(155) -
     &    (2*(MixHil(8)*MixHix(159) - MixHil(7)*MixHix(163)))/
     &     (MA02 - MHp2)**2 - MixHix(183)

        MixHindMasses =
     &   -(Alfa2/(MW2**2*Pi**2*SW2**2)*
     &      (1/2048.D0*MixHix(146) + 1/512.D0*MixHix(184)))

	end
