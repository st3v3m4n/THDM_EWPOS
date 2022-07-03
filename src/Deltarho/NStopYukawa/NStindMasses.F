c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich



* NStindMasses.F
* generated 14-Dec-2015 11:38


C> @brief This subroutine calculates \f$\delta\rho^{(2)}_\mathrm{t,NS}\f$
C> with independent masses of the non-standard scalars.
C>
C> @ingroup group_rho
	subroutine calcNStindMasses
	implicit none


#include "NStindMasses.h"


	RealType T134sub

	external T134sub

	ComplexType A0FH, B0FH

	external A0FH, B0FH

#include "paraTHDM.h"
#include "paraSM.h"
	NStil(1) = A0FH(MT2)

	NStil(10) = B0FH(MT2,MHH2,MT2)

	NStil(11) = T134sub(MT2,MA02,0.D0)

	NStil(12) = T134sub(MT2,MT2,MA02)

	NStil(13) = T134sub(MT2,MHH2,0.D0)

	NStil(14) = T134sub(MT2,MT2,MHH2)

	NStil(15) = T134sub(MT2,MHp2,0.D0)

	NStil(16) = T134sub(MT2,MT2,MHp2)

	NStil(17) = T134sub(MHp2,0.D0,0.D0)

	NStil(2) = B0FH(MHp2,0.D0,MT2)

	NStil(3) = A0FH(MHp2)

	NStil(4) = B0FH(MHH2,MT2,MT2)

	NStil(5) = A0FH(MHH2)

	NStil(6) = B0FH(MA02,MT2,MT2)

	NStil(7) = A0FH(MA02)

	NStil(8) = B0FH(MT2,0.D0,MHp2)

	NStil(9) = B0FH(MT2,MA02,MT2)

	NStix(33) = 3*MHH2**2 + 32*MT2**2 + 28*MT2*MHH2

        NStix(34) = MA02**2 - MA02*(MHH2 + 8*MT2) +
     &    4*MT2*(MHH2 + 2*MT2)

	NStix(44) = -1 - 4/MT2*NStil(1) + 2/MT2**2*NStil(1)**2

	NStix(32) = 3*MHH2**2 + 48*MT2**2 + 28*MT2*MHH2

	NStix(35) = 8*MA02**2 - 4/MT2*(NStil(1)*NStix(34)) - NStix(33)

        NStix(37) = MA02**2 - MA02*(MHH2 + 12*MT2) +
     &    8*MT2*(MHH2 + 2*MT2)

	NStix(45) = 3*MHp2**2 - 10*MT2*MHp2 - 8*MT2**2*NStix(44)

	NStix(13) = 6*MHp2 - 1/MT2*((3*MHp2 - 4*MT2)*NStil(1))

	NStix(18) = 2*MHH2**2 - MHH2*(3*MHp2 + 2*MT2) + 6*MT2*MHp2

	NStix(21) = MHp2 + 1/MT2*((2*MHH2 - 3*MHp2)*NStil(1))

	NStix(25) = MHH2 - 2*MT2*(3 + 1/MT2*NStil(1))

        NStix(28) = MA02**2 - MA02*(MHH2 + 6*MT2) +
     &    2*MT2*(MHH2 + 8*MT2)

	NStix(29) = MA02*(MHH2 - 6*MT2) - MHH2*(MHH2 - 2*MT2)

        NStix(36) = 3*MA02**3 +
     &    MT2*(64*NStil(1)**2 - 4*NStix(35)) - MA02*NStix(32)

        NStix(38) = -4*(MA02 - MHH2)*(3*MA02 - 8*MT2)*
     &     NStil(1) + MA02*NStix(37)

	NStix(41) = 2*MA02**2 - 3*MA02*(MHp2 + 2*MT2) + 10*MT2*MHp2

        NStix(46) = 6*MA02**3 + MA02**2*(3*MHp2 - 2*MT2) -
     &    4*MT2*(MHp2*(3*MHp2 + 2*MT2)) + MA02*NStix(45)

	NStix(47) = 2*MA02**2 + 8*MT2**2 - MA02*(MHp2 + 6*MT2)

	NStix(48) = 2*MA02**2 - MA02*(3*MHp2 + 4*MT2) + 8*MT2*MHp2

	NStix(57) = MHH2**2 - MHH2*(MHp2 + 3*MT2) + 2*MT2*MHp2

	NStix(58) = MHp2**2 + 2*MT2**2 - 5*MT2*MHp2

	NStix(59) = MHp2**2 + 3*MT2**2 - 6*MT2*MHp2

	NStix(62) = MHH2**2 - MHH2*(MHp2 + MT2) + 2*MT2*MHp2

	NStix(65) = MHp2**2 + MT2**2 - 4*MT2*MHp2

	NStix(68) = -(MHp2*(MHp2 + 2*MT2)) + MA02*(MHp2 + 3*MT2)

	NStix(14) = MHp2**2 + 4*MT2**2 - MT2*NStix(13)

        NStix(17) = 6*MHH2**2 + 3*(MHH2*MHp2 + MHp2**2) -
     &    2*MT2*(7*MHH2 + 5*MHp2) - 32*(MT2**2 + MT2*NStil(1)) -
     &    16*NStil(1)**2

        NStix(19) = MHp2*(MHp2 - 4*MT2)*NStil(16) +
     &    NStil(14)*NStix(18)

	NStix(20) = MHp2 - 2*MT2*(3 + 2/MT2*NStil(1))

	NStix(22) = 2*MHH2**2 - MHH2*(MHp2 + 10*MT2) + 4*MT2*NStix(21)

        NStix(24) = 49*MHH2 -
     &    4/MT2*((19*MHH2 - 88*MT2)*NStil(1)) - 56*NStil(14)

	NStix(26) = -2*NStil(14) + (3*NStil(5)*NStix(25))/MHH2

        NStix(30) = (MA02*NStil(12)*NStix(28))/
     &     (MA02 - 4*MT2) - NStil(14)*NStix(29)

        NStix(31) = MA02*(MHH2 + 4*MT2) -
     &    MHH2*(MHH2 + 8*MT2) - 12*(MA02 - MHH2)*NStil(1)

	NStix(39) = (MA02 - MHH2)*NStix(36) + 4*NStil(7)*NStix(38)

        NStix(42) = MHp2**2*NStil(16) +
     &    (MA02*NStil(12)*NStix(41))/(MA02 - 4*MT2)

	NStix(43) = MHp2 - 2*MT2*(1 + 2/MT2*NStil(1))

        NStix(49) = (MA02 - MHp2)*NStix(46) +
     &    4*NStil(7)*(MA02*NStix(47) + 4*NStil(1)*NStix(48))

        NStix(60) = (MHH2 - MT2)*NStil(13)*NStix(57) +
     &    NStil(15)*(MHp2*NStix(58) - MHH2*NStix(59))

        NStix(61) = MHp2*(3*MHH2 - 4*MHp2 + 2*MT2) -
     &    2/MT2*((MHp2**2 - MHH2*(MHp2 + MT2))*NStil(1))

        NStix(63) = MHH2*(MHH2 - 2*MHp2 + 2*MT2) +
     &    2/MT2*(NStil(1)*NStix(62))

	NStix(66) = MHp2**2*(MHp2 - 3*MT2) - MA02*NStix(65)

        NStix(69) = MHp2*(3*MA02 - 4*MHp2 + 2*MT2) +
     &    2/MT2*(NStil(1)*NStix(68))

	NStix(70) = MT2 + 1/MT2*((MA02 - MHp2 + MT2)*NStil(1))

        NStix(98) = (MHH2 - 4*MT2)*Re(NStil(4)) +
     &    (MA02 - 2*MHH2)*Re(NStil(6))

	NStix(102) = Re(NStil(7))/MA02 - Re(NStil(9))

	NStix(11) = 19*MHp2 + 4/MT2*((MHp2 + MT2)*NStil(1))

	NStix(12) = 2*MHp2**2 + 5*MT2**2 - 6*MT2*MHp2

	NStix(15) = 3*MHp2**2 + (4*NStil(3)*NStix(14))/MHp2

        NStix(23) = -((MHH2 - MHp2)*NStix(17)) + 8*NStix(19) +
     &    4*MHp2*NStil(3)*NStix(20) - 4*NStil(5)*NStix(22)

        NStix(27) = 5*MHH2**2 -
     &    4*MT2**2*(7 - 6/MT2**2*NStil(1)**2) - MT2*NStix(24) +
     &    4*MHH2*NStix(26)

        NStix(40) = -8*NStix(30) + 4*NStil(5)*NStix(31) -
     &    NStix(39)/(MA02 - 4*MT2)

        NStix(50) = 8*NStix(42) + 4*MHp2*NStil(3)*NStix(43) -
     &    NStix(49)/(MA02 - 4*MT2)

        NStix(53) = MHH2**2 + 3*MHH2*MHp2 + 8*MHp2**2 -
     &    17*MT2*(MHH2 - MHp2)

        NStix(55) = MA02**2 + 3*MA02*MHp2 + 8*MHp2**2 -
     &    9*MT2*(MA02 - MHp2)

        NStix(64) = -(MT2*
     &       (NStil(3)*NStix(61) - NStil(5)*NStix(63))) +
     &    2*NStix(60)

        NStix(67) = MA02*(MA02 - MT2)*(MA02 - MHp2 - MT2)*
     &     NStil(11) + NStil(15)*NStix(66)

        NStix(71) = -(NStil(3)*NStix(69)) +
     &    MA02*NStil(7)*(MA02 - 2*MHp2 + 2*NStix(70))

	NStix(78) = MHH2 - MHp2 - 4*MT2 - 2*(MHp2 - MT2)*Re(NStil(2))

	NStix(80) = 2*MHp2 - 1/MT2*((2*MHH2 - 3*MHp2)*Re(NStil(1)))

	NStix(81) = MHp2**2 + MT2*(2*MHH2 - MHp2)

        NStix(82) = MHH2 - 4*MHH2*Re(NStil(2)) +
     &    2*(MHH2 - 4*MT2)*Re(NStil(4))

	NStix(86) = MHp2**2 + MT2*(2*MA02 - MHp2)

	NStix(87) = MHp2 - 2*MHp2*(2*Re(NStil(2)) - Re(NStil(6)))

        NStix(89) = (MHp2 - MT2)*Re(NStil(2)) +
     &    (MA02 - 2*MHp2)*Re(NStil(6))

	NStix(94) = MHH2 + 2/MT2*((MA02 - MHH2)*Re(NStil(1)))

	NStix(95) = MHH2**2 + 4*MT2*(2*MA02 - MHH2)

	NStix(96) = MHH2 - 2*MHH2*(2*Re(NStil(4)) - Re(NStil(6)))

	NStix(99) = MA02 - MHH2 + 4*MT2 - 2*NStix(98)

	NStix(1) = -33 - 92/MT2*NStil(1) + 8/MT2**2*NStil(1)**2

        NStix(10) = NStil(1)**2 + (MHp2 - 2*MT2)*NStil(16) +
     &    MHp2*NStil(17)

	NStix(100) = MA02*NStix(99) + 8*(MA02 - MHH2)*Re(NStil(1))

        NStix(103) = -(2*MT2*(3 + 1/MT2*Re(NStil(1)))) +
     &    MA02*NStix(102) + Re(NStil(3)) + Re(NStil(5)) -
     &    (MHp2 - MT2)*Re(NStil(8)) - (MHH2 - 4*MT2)*Re(NStil(10))

        NStix(16) = 3*MT2**3 + MT2**2*NStix(11) -
     &    2*MT2*NStix(15) - 8*NStil(15)*NStix(12)

        NStix(2) = MT2*((5 + 1/MT2*NStil(1))*NStil(5))/MHH2 +
     &    NStil(13)

        NStix(4) = 33*MA02 -
     &    4/MT2*((MA02 - 36*MT2)*NStil(1)) -
     &    8/MT2**2*((MA02 + 4*MT2)*NStil(1)**2)

	NStix(5) = 5*MA02 - 12*MT2 + 1/MT2*((MA02 + 4*MT2)*NStil(1))

        NStix(51) = NStix(23)/(MHH2 - MHp2)**2 +
     &    NStix(27)/(MHH2 - 4*MT2) + NStix(40)/(MA02 - MHH2)**2 +
     &    NStix(50)/(MA02 - MHp2)**2

        NStix(54) = MT2*NStix(53) +
     &    8*MHH2*(MHH2 - 4*MT2)*NStil(14) -
     &    8*MHp2*(MHp2 - 4*MT2)*NStil(16)

        NStix(56) = MT2*NStix(55) +
     &    8*MA02*(MA02 - 2*MT2)*NStil(12) -
     &    8*MHp2*(MHp2 - 2*MT2)*NStil(16)

        NStix(7) = MA02 - 4/MT2*((3*MA02 - 8*MT2)*NStil(1)) -
     &    24*NStil(12)

        NStix(72) = 2*MT2*((1 - 2/MT2*NStil(1))*NStil(1)) +
     &    NStix(64)/(MHH2 - MHp2)**2 +
     &    (MT2*NStix(71) + 2*NStix(67))/(MA02 - MHp2)**2

        NStix(77) = MHH2 - MHp2 -
     &    2*(MT2*(2 + 1/MT2*Re(NStil(1))) +
     &       (MHp2 - MT2)*Re(NStil(2))) +
     &    2*(MHH2 - 4*MT2)*Re(NStil(4))

        NStix(79) = MHH2*NStix(78) +
     &    2*(3*MHH2 - 4*MHp2)*Re(NStil(1)) -
     &    2*(MHH2 - 2*MHp2)*(MHH2 - 4*MT2)*Re(NStil(4))

	NStix(8) = 3*MA02 - 2*MT2*(5 - 1/MT2*NStil(1))

        NStix(83) = MHp2**2 + 2*MT2*NStix(80) -
     &    MHp2*NStix(82) - 2*NStix(81)*Re(NStil(2))

        NStix(85) = MA02 - MHp2 -
     &    2*(Re(NStil(1)) + (MHp2 - MT2)*Re(NStil(2))) +
     &    2*MA02*Re(NStil(6))

        NStix(88) = -MHp2**2 + MA02*NStix(87) +
     &    2*((2*MA02 - 3*MHp2)*Re(NStil(1)) +
     &       NStix(86)*Re(NStil(2)))

        NStix(90) = MA02*(MA02 - MHp2 - 2*NStix(89)) +
     &    2*(3*MA02 - 4*MHp2)*Re(NStil(1))

        NStix(93) = MA02 - MHH2 + 4*MT2 -
     &    2*(MHH2 - 4*MT2)*Re(NStil(4)) + 2*MA02*Re(NStil(6))

        NStix(97) = -MHH2**2 + 4*MT2*NStix(94) +
     &    MA02*NStix(96) + 2*NStix(95)*Re(NStil(4))

        NStix(101) = (MA02 - MHH2)*(MA02 + MHH2)*NStix(93) -
     &    2*NStil(5)*NStix(97) + 2*NStil(7)*NStix(100)

        NStix(104) = MT2*(3 + 4/MT2*Re(NStil(1))) +
     &    1/MT2*(NStil(1)*NStix(103))

        NStix(105) = (MHp2 - MT2)*Re(NStil(8)) +
     &    MA02*Re(NStil(9)) + (MHH2 - 4*MT2)*Re(NStil(10))

        NStix(106) = MT2*(9 + 4/MT2*Re(NStil(1))) +
     &    2*NStix(105) - 2*
     &     (Re(NStil(3)) + Re(NStil(5)) + Re(NStil(7)))

        NStix(107) = MT2 + 2*NStix(105) + 4*Re(NStil(1)) -
     &    2*(Re(NStil(3)) + Re(NStil(5)) + Re(NStil(7)))

        NStix(3) = -(16*MT2*NStil(14)) - MT2**2*NStix(1) -
     &    8*(MT2*NStil(13) + MHH2*NStil(14)) +
     &    2*MHH2*(9*MT2 + 4*NStix(2))

        NStix(52) = MT2*NStix(51) - 8*NStix(10) -
     &    NStix(16)/(MHp2 - MT2)

        NStix(6) = -36*MT2**3 + 18*MT2*MA02**2 +
     &    MT2**2*NStix(4) +
     &    8*(MT2*(NStil(7)*NStix(5)) + MA02**2*NStil(11)) -
     &    8*((-4*MT2**2 + 5*MT2*MA02)*NStil(11) +
     &       (MA02**2 - 2*MT2*MA02)*NStil(12))

        NStix(73) = NStix(54)/(MHH2 - MHp2) +
     &    NStix(56)/(MA02 - MHp2) - 4*NStix(72)

        NStix(75) = MHp2 - MT2*(2 - 1/MT2*Re(NStil(1))) -
     &    (MHp2 - MT2)*Re(NStil(2))

        NStix(84) = (MHH2 - MHp2)*(MHH2 + MHp2)*NStix(77) +
     &    2*(NStil(5)*NStix(79) + NStil(3)*NStix(83))

        NStix(9) = 5*MA02**2 - MT2*NStix(7) -
     &    4*(MT2**2*(7 + 2/MT2**2*NStil(1)**2) +
     &       MA02*(2*NStil(12) - (NStil(7)*NStix(8))/MA02))

        NStix(91) = (MA02 - MHp2)*(MA02 + MHp2)*NStix(85) -
     &    2*NStil(3)*NStix(88) + 2*NStil(7)*NStix(90)

        NStix(108) = -(2/MT2*(NStil(1)*NStix(106))) +
     &    NStix(101)/(MA02 - MHH2)**2 - 4*NStix(104) -
     &    8*NStix(105) + 3*NStix(107) +
     &    8*(Re(NStil(3)) + Re(NStil(5)) + Re(NStil(7)))

        NStix(74) = NStix(3) +
     &    (-(2*MT2*NStix(9)) + NStix(6))/(MA02 - 4*MT2) -
     &    2*(NStix(52) + NStix(73))

        NStix(76) = -((NStil(3)*NStix(75))/MHp2) +
     &    Re(NStil(1)) - (MHp2 - MT2)*Re(NStil(2))

        NStix(92) = NStix(84)/(MHH2 - MHp2)**2 +
     &    NStix(91)/(MA02 - MHp2)**2

        NStindMasses =
     &   Alfa2/(MW2**2*Pi**2*SW2**2)*
     &    (3/2048.D0*NStix(74) -
     &       (3*MT2)/512.D0*(8*NStix(76) + NStix(92) - NStix(108)))/
     &     TB2

	end
