c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich


* IHDMindMasses.F
* generated 1-Nov-2016 16:40

C> @brief This subroutine calculates the two-loop correction to the \f$\rho\f$ parameter in the IHDM
C> with independent masses of the non-standard scalars.
C>
C> @ingroup group_rho
	subroutine calcIHDMindMasses
	implicit none


#include "IHDMindMasses.h"


	RealType T134sub

	external T134sub

	ComplexType A0FH, B0FH

	external A0FH, B0FH

#include "paraTHDM.h"
#include "paraSM.h"
	IHDMil(1) = A0FH(MA02)

	IHDMil(10) = B0FH(MHp2,0.D0,MA02)

	IHDMil(11) = B0FH(MHp2,0.D0,MHH2)

	IHDMil(12) = A0FH(MHp2)

	IHDMil(13) = B0FH(MHp2,Mh02,MHp2)

	IHDMil(14) = T134sub(MHH2,MA02,0.D0)

	IHDMil(15) = T134sub(MHH2,Mh02,MA02)

	IHDMil(16) = T134sub(Mh02,MA02,MA02)

	IHDMil(17) = T134sub(MHH2,MHH2,Mh02)

	IHDMil(18) = T134sub(MHp2,MA02,0.D0)

	IHDMil(19) = T134sub(MHp2,MHH2,0.D0)

	IHDMil(2) = A0FH(Mh02)

	IHDMil(20) = T134sub(MHp2,MHp2,Mh02)

	IHDMil(21) = T134sub(MHp2,MHH2,Mh02)

	IHDMil(22) = T134sub(MHp2,Mh02,MA02)

	IHDMil(3) = B0FH(MA02,0.D0,MHH2)

	IHDMil(4) = B0FH(MA02,0.D0,MHp2)

	IHDMil(5) = B0FH(MA02,MA02,Mh02)

	IHDMil(6) = B0FH(MHH2,0.D0,MA02)

	IHDMil(7) = B0FH(MHH2,0.D0,MHp2)

	IHDMil(8) = B0FH(MHH2,Mh02,MHH2)

	IHDMil(9) = A0FH(MHH2)

	IHDMix(113) = Mh02*(2*MHH2 - 3*mu2sq) - 5*MHH2*mu2sq

	IHDMix(115) = MHH2**2 + (3*Mh02 - 5*MHH2)*mu2sq

	IHDMix(116) = MHH2*(MHH2 - 5*mu2sq) + Mh02*(2*MHH2 + mu2sq)

        IHDMix(12) = (MA02 - MHH2)*(3*MA02 + MHH2) -
     &    2*(2*MA02 - MHH2)*MHp2

	IHDMix(122) = Mh02*(2*MHp2 - 3*mu2sq) - 5*MHp2*mu2sq

	IHDMix(124) = MHp2**2 + (3*Mh02 - 5*MHp2)*mu2sq

	IHDMix(125) = MHp2*(MHp2 - 5*mu2sq) + Mh02*(2*MHp2 + mu2sq)

	IHDMix(14) = MHH2**2 - 2*MHH2*MHp2 + 4*MHp2**2

        IHDMix(19) = MA02**2 + 3*MA02*MHH2 + MHH2**2 -
     &    5*(MA02 + MHH2)*MHp2 + 5*MHp2**2

	IHDMix(68) = MA02*(MHH2 - 4*mu2sq) + mu2sq**2

	IHDMix(72) = MA02*(MHp2 - 4*mu2sq) + mu2sq**2

        IHDMix(78) = 3*(MHH2**2 + MHp2**2) +
     &    4*MHH2*(MHp2 - mu2sq) - 4*MHp2*mu2sq + 2*mu2sq**2

        IHDMix(84) = Mh02**2 - 8*MHH2*(5*MHH2 - 8*mu2sq) -
     &    16*Mh02*(MHH2 - 2*mu2sq)

	IHDMix(86) = 9*MHH2**2 - 16*MHH2*mu2sq + 12*mu2sq**2

        IHDMix(89) = Mh02**2 - 8*MHp2*(5*MHp2 - 8*mu2sq) -
     &    16*Mh02*(MHp2 - 2*mu2sq)

	IHDMix(91) = 9*MHp2**2 - 16*MHp2*mu2sq + 12*mu2sq**2

	IHDMix(100) = 12*MA02**2 - MA02*(Mh02 - 12*MHH2 + 16*mu2sq)

	IHDMix(104) = 2*MHp2**2 - 4*MHp2*mu2sq + mu2sq**2

	IHDMix(107) = 12*MHp2**2 + 8*mu2sq**2 - MHp2*(Mh02 + 16*mu2sq)

	IHDMix(108) = 12*MA02**2 - MA02*(Mh02 - 12*MHp2 + 16*mu2sq)

        IHDMix(117) = Mh02**2*mu2sq - 2*MHH2*IHDMix(115) -
     &    MA02*(Mh02**2 - 2*IHDMix(116))

        IHDMix(126) = Mh02**2*mu2sq - 2*MHp2*IHDMix(124) -
     &    MA02*(Mh02**2 - 2*IHDMix(125))

        IHDMix(13) = 2*(MA02 + MHH2)*MHp2**2 - 2*MHp2**3 +
     &    MA02*IHDMix(12)

	IHDMix(138) = 6*MA02 + Mh02 - 2*MHH2 + 4*MHp2 - 8*mu2sq

        IHDMix(139) = (MA02 - MHp2)**2*Re(IHDMil(10)) +
     &    (MHH2 - MHp2)**2*Re(IHDMil(11))

        IHDMix(141) = (MA02 - 2*MHp2)*(MA02 - mu2sq) +
     &    MA02*(MHp2 - mu2sq)

        IHDMix(145) = (MHp2 - mu2sq)*Re(IHDMil(2)) +
     &    (MA02 - MHp2)**2*Re(IHDMil(10)) +
     &    (MHH2 - MHp2)**2*Re(IHDMil(11))

        IHDMix(147) = (MA02 - mu2sq)*Re(IHDMil(2)) +
     &    (MA02 - MHH2)**2*Re(IHDMil(3))

        IHDMix(15) = 3*MA02**3 + MA02**2*(MHH2 - 7*MHp2) +
     &    MHH2*(3*MHH2 - 4*MHp2)*(MHH2 - MHp2) + MA02*IHDMix(14)

        IHDMix(150) = (MA02 - MHH2)*Re(IHDMil(2)) +
     &    (MA02 - MHH2)**2*Re(IHDMil(3))

	IHDMix(151) = -MA02**2 + 2*MA02*MHH2 - MHH2**2

	IHDMix(152) = -MHH2**2 + 2*MHH2*mu2sq - mu2sq**2

        IHDMix(154) = (MHH2 - MHp2)*Re(IHDMil(2)) +
     &    (MA02 - MHH2)**2*Re(IHDMil(6))

	IHDMix(157) = 2*MA02 - Mh02 - 6*MHH2 - 4*MHp2 + 8*mu2sq

        IHDMix(158) = (MHH2 - mu2sq)*Re(IHDMil(2)) +
     &    (MA02 - MHH2)**2*Re(IHDMil(6))

        IHDMix(162) = (MHH2 - 2*MHp2)*(MHH2 - mu2sq) +
     &    MHH2*(MHp2 - mu2sq)

        IHDMix(166) = (MA02 - 2*MHH2)*(MA02 - mu2sq) +
     &    MA02*(MHH2 - mu2sq)

	IHDMix(168) = Mh02 + 6*(MA02 + MHH2) - 4*MHp2 - 8*mu2sq

        IHDMix(17) = MA02**2 + 5*MA02*(MHH2 - 2*MHp2) +
     &    2*MHH2*(MHH2 + MHp2)

        IHDMix(18) = MA02**2 + MHH2**2 -
     &    2*(MA02 + MHH2)*MHp2 + 2*MHp2**2

        IHDMix(20) = (IHDMil(12)*IHDMix(18))/(MA02 - MHH2) -
     &    (IHDMil(9)*IHDMix(19))/(MA02 - MHp2)

        IHDMix(22) = 5*MA02*MHH2 + MHH2**2 +
     &    2*(MA02**2 + (MA02 - 5*MHH2)*MHp2)

	IHDMix(24) = MHH2**2 - MHH2*MHp2 - MHp2**2

        IHDMix(33) = 3*(MA02**2 + MHH2**2) -
     &    2*(Mh02*MHH2 + MA02*(Mh02 + 3*MHH2)) + 4*Mh02*mu2sq

        IHDMix(38) = -3*(MHH2 - MHp2)**2 +
     &    2*Mh02*(MHH2 + MHp2 - 2*mu2sq)

        IHDMix(42) = 3*(MA02**2 + MHp2**2) -
     &    2*(Mh02*MHp2 + MA02*(Mh02 + 3*MHp2)) + 4*Mh02*mu2sq

	IHDMix(45) = 2*MHH2**2 + MHH2*(MHp2 - 4*mu2sq) + mu2sq**2

	IHDMix(48) = 2*MHp2**2 + MHp2*(MHH2 - 4*mu2sq) + mu2sq**2

	IHDMix(50) = MHH2*(3*MHp2 - mu2sq) - MHp2*(MHp2 + mu2sq)

	IHDMix(53) = Mh02**2 - 2*MHH2*(2*Mh02 + MHH2)

        IHDMix(54) = Mh02**2 + 10*MHH2*(MHH2 - MHp2) -
     &    2*Mh02*(3*MHH2 - MHp2)

	IHDMix(57) = -3*MHp2*mu2sq + MHH2*(2*MHp2 + mu2sq)

	IHDMix(60) = MHH2*(MHH2 - 3*MHp2) + (MHH2 + MHp2)*mu2sq

        IHDMix(62) = 6*MHH2*MHp2 + 5*(MHH2**2 + MHp2**2) -
     &    8*(MHH2 + MHp2)*mu2sq

        IHDMix(63) = 9*(MHH2**2 + MHp2**2) +
     &    2*MHH2*(5*MHp2 - 8*mu2sq) - 16*MHp2*mu2sq + 12*mu2sq**2

        IHDMix(64) = 5*MHH2**3 -
     &    MHH2*MHp2*(3*MHp2 - 8*mu2sq) +
     &    MHp2**2*(5*MHp2 - 8*mu2sq) - MHH2**2*(3*MHp2 + 8*mu2sq)

	IHDMix(67) = MA02*Mh02 - 2*(Mh02 - 2*mu2sq)*mu2sq

        IHDMix(69) = 32*MA02**2 - Mh02**2 -
     &    4*Mh02*(MHH2 - 2*mu2sq) + 16*IHDMix(68)

        IHDMix(73) = 32*MA02**2 - Mh02**2 -
     &    4*Mh02*(MHp2 - 2*mu2sq) + 16*IHDMix(72)

        IHDMix(79) = Mh02**2*(MHH2 + MHp2) +
     &    16*MHH2*MHp2*(3*(MHH2 + MHp2) - 4*mu2sq) -
     &    4*Mh02*IHDMix(78)

        IHDMix(8) = 14*MHH2 - 3*MHp2 +
     &    (4*(MHH2 + 2*MHp2)*IHDMil(12))/MHp2

        IHDMix(85) = -40*MA02**3 -
     &    2*MA02**2*(9*Mh02 + 24*MHH2 - 32*mu2sq) + MA02*IHDMix(84)

        IHDMix(87) = Mh02**2*MHH2 -
     &    8*MHH2**2*(5*MHH2 - 8*mu2sq) - 2*Mh02*IHDMix(86)

        IHDMix(90) = -40*MA02**3 -
     &    2*MA02**2*(9*Mh02 + 24*MHp2 - 32*mu2sq) + MA02*IHDMix(89)

        IHDMix(92) = Mh02**2*MHp2 -
     &    8*MHp2**2*(5*MHp2 - 8*mu2sq) - 2*Mh02*IHDMix(91)

	IHDMix(96) = 2*MHH2**2 - 4*MHH2*mu2sq + mu2sq**2

	IHDMix(99) = 12*MHH2**2 + 8*mu2sq**2 - MHH2*(Mh02 + 16*mu2sq)

	IHDMix(1) = 5*MA02 + MHH2*(3 + (10*IHDMil(9))/MHH2)

	IHDMix(101) = Mh02*IHDMix(99) + (Mh02 - 4*MHH2)*IHDMix(100)

        IHDMix(103) = Mh02*mu2sq - 2*MHp2*(MHp2 + mu2sq) -
     &    MA02*(Mh02 - 6*MHp2 + 2*mu2sq)

        IHDMix(105) = Mh02**2 + 4*MA02*(Mh02 - 4*MHp2) -
     &    8*Mh02*mu2sq - 16*IHDMix(104)

	IHDMix(109) = Mh02*IHDMix(107) + (Mh02 - 4*MHp2)*IHDMix(108)

	IHDMix(11) = 2*(MA02 + MHH2) - MHp2*(1 - IHDMil(12)/MHp2)

	IHDMix(111) = MA02**2 + MHH2**2 - Mh02*(MHH2 - mu2sq)

	IHDMix(112) = Mh02*MHH2 - (Mh02 + 2*MHH2)*mu2sq

	IHDMix(120) = MA02**2 + MHp2**2 - Mh02*(MHp2 - mu2sq)

	IHDMix(121) = Mh02*MHp2 - (Mh02 + 2*MHp2)*mu2sq

        IHDMix(130) = (MA02 - MHp2)**2 + (MHH2 - MHp2)**2 +
     &    Mh02*(MHp2 - mu2sq)*(1 - Re(IHDMil(2))/Mh02) -
     &    (MA02 - MHp2)**2*Re(IHDMil(10)) -
     &    (MHH2 - MHp2)**2*Re(IHDMil(11)) +
     &    4*(MHp2 - mu2sq)**2*(1 - Re(IHDMil(13)))

	IHDMix(132) = -MA02**2 + 2*MA02*MHp2 - MHp2**2

	IHDMix(133) = -MHH2**2 + 2*MHH2*MHp2 - MHp2**2

	IHDMix(134) = -MHp2**2 + 2*MHp2*mu2sq - mu2sq**2

        IHDMix(135) = (MA02 - MHp2)*Re(IHDMil(2)) +
     &    (MA02 - MHH2)**2*Re(IHDMil(3))

	IHDMix(136) = -MA02**2 + 2*MA02*mu2sq - mu2sq**2

        IHDMix(140) = (MA02 - MHp2)*IHDMix(138) -
     &    2*IHDMix(139) - 8*(MHp2 - mu2sq)**2*Re(IHDMil(13))

        IHDMix(142) = IHDMix(141)*Re(IHDMil(2)) +
     &    (MA02 - MHH2)**2*(MA02 - 2*MHp2)*Re(IHDMil(3))

        IHDMix(143) = (MA02 - MHp2)**2*Re(IHDMil(4)) +
     &    2*(MA02 - mu2sq)**2*Re(IHDMil(5))

	IHDMix(146) = IHDMix(145) + 4*(MHp2 - mu2sq)**2*Re(IHDMil(13))

        IHDMix(148) = (MA02 - MHp2)*IHDMix(138) +
     &    2*IHDMix(147) + 4*(MA02 - MHp2)**2*Re(IHDMil(4)) +
     &    8*(MA02 - mu2sq)**2*Re(IHDMil(5))

        IHDMix(153) = -6*MA02**2 - Mh02*(MA02 - MHH2) +
     &    6*MHH2**2 - (MA02 - MHH2)*(-4*MHp2 - 8*mu2sq) -
     &    2*IHDMix(150) + 4*IHDMix(132)*Re(IHDMil(4)) +
     &    8*IHDMix(136)*Re(IHDMil(5)) -
     &    2*IHDMix(151)*Re(IHDMil(6)) -
     &    4*IHDMix(133)*Re(IHDMil(7)) - 8*IHDMix(152)*Re(IHDMil(8))

        IHDMix(155) = (2*MA02 - Mh02)*MHH2 - 6*MHH2**2 -
     &    (2*MA02 - Mh02 - 2*MHH2)*MHp2 +
     &    4*(MHp2**2 + IHDMix(133)*Re(IHDMil(7))) +
     &    8*((MHH2 - MHp2)*mu2sq + IHDMix(152)*Re(IHDMil(8))) -
     &    2*(IHDMix(154) + IHDMix(132)*Re(IHDMil(10)) +
     &       IHDMix(133)*Re(IHDMil(11))) -
     &    8*IHDMix(134)*Re(IHDMil(13))

        IHDMix(159) = (MHH2 - MHp2)*IHDMix(157) -
     &    2*IHDMix(158) - 4*(MHH2 - MHp2)**2*Re(IHDMil(7)) -
     &    8*(MHH2 - mu2sq)**2*Re(IHDMil(8))

        IHDMix(16) = -((IHDMil(18)*IHDMix(13))/
     &       (MA02 - MHH2)) + (IHDMil(14)*IHDMix(15))/(MA02 - MHp2)

        IHDMix(161) = 2*IHDMix(139) +
     &    (MHH2 - MHp2)*IHDMix(157) +
     &    8*(MHp2 - mu2sq)**2*Re(IHDMil(13))

        IHDMix(163) = IHDMix(162)*Re(IHDMil(2)) +
     &    (MA02 - MHH2)**2*(MHH2 - 2*MHp2)*Re(IHDMil(6))

        IHDMix(164) = (MHH2 - MHp2)**2*Re(IHDMil(7)) +
     &    2*(MHH2 - mu2sq)**2*Re(IHDMil(8))

        IHDMix(167) = IHDMix(166)*Re(IHDMil(2)) +
     &    (MA02 - 2*MHH2)*(MA02 - MHH2)**2*Re(IHDMil(3))

        IHDMix(169) = (MA02 - MHH2)*IHDMix(168) -
     &    2*(MA02 - MHH2)**2*Re(IHDMil(6)) -
     &    4*(MHH2 - MHp2)**2*Re(IHDMil(7)) -
     &    8*(MHH2 - mu2sq)**2*Re(IHDMil(8))

        IHDMix(171) = 2*IHDMix(147) +
     &    (MA02 - MHH2)*IHDMix(168) +
     &    4*(MA02 - MHp2)**2*Re(IHDMil(4)) +
     &    8*(MA02 - mu2sq)**2*Re(IHDMil(5))

        IHDMix(172) = IHDMix(158) +
     &    2*(MHH2 - MHp2)**2*Re(IHDMil(7)) +
     &    4*(MHH2 - mu2sq)**2*Re(IHDMil(8))

        IHDMix(21) = IHDMix(17)/(MA02 - MHH2) +
     &    (2*IHDMix(20))/(MHH2 - MHp2)

        IHDMix(23) = 2*IHDMil(12)*IHDMix(18) +
     &    (MA02 - MHp2)*IHDMix(22)

        IHDMix(25) = -3*MHH2**3 + 4*MHH2**2*MHp2 +
     &    MHH2*(MA02**2 - 2*MHp2**2) +
     &    2*(MHp2**3 + MA02*IHDMix(24))

        IHDMix(27) = -Mh02 -
     &    MHp2*(2 - IHDMil(12)**2/MHp2**2) + IHDMil(20)

        IHDMix(28) = Mh02 - 8*MHp2 + 4*mu2sq -
     &    ((Mh02 - 4*mu2sq)*IHDMil(12))/MHp2

	IHDMix(29) = Mh02**2 - 24*MHp2*(MHp2 - mu2sq) - 4*Mh02*mu2sq

        IHDMix(3) = MA02*(MHH2 - MHp2)*
     &     (1 + (2*IHDMil(1))/MA02) +
     &    MHH2*(MA02 - MHp2)*(1 + (2*IHDMil(9))/MHH2)

	IHDMix(31) = -MA02 + MHH2 + 4*IHDMil(1) - 4*IHDMil(9)

        IHDMix(32) = (MA02 - mu2sq)*IHDMil(16) +
     &    (MHH2 - mu2sq)*IHDMil(17)

        IHDMix(34) = 3*(MA02 - MHH2)**2*IHDMil(14) -
     &    IHDMil(15)*IHDMix(33)

	IHDMix(36) = -MA02 + MHp2 + 4*IHDMil(1) - 4*IHDMil(12)

	IHDMix(37) = MHH2 - MHp2 - 4*IHDMil(9) + 4*IHDMil(12)

        IHDMix(39) = 3*(MHH2 - MHp2)**2*IHDMil(19) +
     &    IHDMil(21)*IHDMix(38)

        IHDMix(40) = (MA02 - mu2sq)*IHDMil(16) +
     &    (MHp2 - mu2sq)*IHDMil(20)

        IHDMix(41) = (MHH2 - mu2sq)*IHDMil(17) +
     &    (MHp2 - mu2sq)*IHDMil(20)

        IHDMix(43) = 3*(MA02 - MHp2)**2*IHDMil(18) -
     &    IHDMil(22)*IHDMix(42)

	IHDMix(46) = Mh02**2 + 4*Mh02*(MHp2 - 2*mu2sq) - 16*IHDMix(45)

	IHDMix(49) = Mh02**2 + 4*Mh02*(MHH2 - 2*mu2sq) - 16*IHDMix(48)

	IHDMix(5) = 4*MA02**2 - 15*MA02*MHp2 + 6*MHp2**2

	IHDMix(51) = Mh02*(MHH2 - mu2sq) - 2*IHDMix(50)

	IHDMix(55) = 2*MHH2**3 + MHp2*IHDMix(53) - mu2sq*IHDMix(54)

        IHDMix(56) = Mh02*(MHH2 - mu2sq)*(MHp2 - mu2sq) +
     &    (MHH2 - MHp2)**2*mu2sq

        IHDMix(58) = -(Mh02**2*(MHH2 - mu2sq)) +
     &    2*((MHH2 - MHp2)*MHp2*(MHp2 - 5*mu2sq) + Mh02*IHDMix(57))

	IHDMix(6) = 4*MHH2**2 - 15*MHH2*MHp2 + 6*MHp2**2

	IHDMix(61) = Mh02*(MHp2 - mu2sq) + 2*IHDMix(60)

        IHDMix(65) = Mh02**3*(MHH2 + MHp2) +
     &    32*MHH2*MHp2*IHDMix(62) - 2*Mh02**2*IHDMix(63) -
     &    8*Mh02*IHDMix(64)

        IHDMix(7) = 14*MA02 - 3*MHp2 +
     &    (4*(MA02 + 2*MHp2)*IHDMil(12))/MHp2

	IHDMix(70) = 2*MA02*IHDMil(2)*IHDMix(67) + MA02**2*IHDMix(69)

	IHDMix(74) = 2*MA02*IHDMil(2)*IHDMix(67) + MA02**2*IHDMix(73)

	IHDMix(76) = Mh02*(MHp2 - 2*mu2sq) + 4*mu2sq**2

	IHDMix(77) = Mh02*(MHH2 - 2*mu2sq) + 4*mu2sq**2

        IHDMix(80) = (2*IHDMil(9)*IHDMix(77))/(MHH2 - MHp2) -
     &    IHDMix(79)/(Mh02 - 4*MHp2)

        IHDMix(82) = 2*MA02**2 - (6*MA02 - Mh02)*MHH2 -
     &    (Mh02 - 2*(MA02 + MHH2))*mu2sq

        IHDMix(83) = 2*MA02**2 - (6*MA02 - Mh02)*MHp2 -
     &    (Mh02 - 2*(MA02 + MHp2))*mu2sq

	IHDMix(88) = (Mh02 - 4*MHH2)*IHDMix(85) + Mh02*IHDMix(87)

	IHDMix(9) = 10*IHDMil(19) - (IHDMil(9)*IHDMix(8))/MHH2

	IHDMix(93) = (Mh02 - 4*MHp2)*IHDMix(90) + Mh02*IHDMix(92)

        IHDMix(95) = Mh02*mu2sq - 2*MHH2*(MHH2 + mu2sq) -
     &    MA02*(Mh02 - 6*MHH2 + 2*mu2sq)

        IHDMix(97) = Mh02**2 + 4*MA02*(Mh02 - 4*MHH2) -
     &    8*Mh02*mu2sq - 16*IHDMix(96)

        IHDMix(10) = -10*(MA02**2 + MHH2**2) + 10*MHp2**2 +
     &    IHDMil(1)**2 + IHDMil(9)**2 + 6*IHDMil(12)**2 +
     &    MHp2*(-11*(MA02 + MHH2) - 2*(IHDMil(18) + IHDMil(19))) +
     &    (IHDMil(12)*(IHDMix(5) + IHDMix(6)))/MHp2 +
     &    MA02*(10*IHDMil(18) - (IHDMil(1)*IHDMix(7))/MA02) +
     &    MHH2*IHDMix(9)

        IHDMix(102) = (2*IHDMil(9)*IHDMix(77))/(MA02 - MHH2) +
     &    IHDMix(101)/(4*MA02 - Mh02)

        IHDMix(106) = (4*(MHp2 - mu2sq)*IHDMil(12)**2*
     &       IHDMix(103))/((MA02 - MHp2)**2*MHp2**2) +
     &    (IHDMil(12)*IHDMix(105))/((MA02 - MHp2)*MHp2)

        IHDMix(110) = (2*IHDMil(12)*IHDMix(76))/
     &     (MA02 - MHp2) + IHDMix(109)/(4*MA02 - Mh02)

        IHDMix(131) = (IHDMil(12)*IHDMix(130))/MHp2 +
     &    (MHp2 - mu2sq)*Re(IHDMil(2)) +
     &    (MA02 - MHp2)**2*Re(IHDMil(10)) +
     &    (MHH2 - MHp2)**2*Re(IHDMil(11)) +
     &    4*(MHp2 - mu2sq)**2*Re(IHDMil(13))

        IHDMix(137) = 6*MA02**2 - (Mh02 - 2*MHH2)*MHp2 +
     &    MA02*(Mh02 - 2*(MHH2 + MHp2)) -
     &    4*(MHp2**2 + IHDMix(132)*Re(IHDMil(4))) -
     &    8*((MA02 - MHp2)*mu2sq + IHDMix(136)*Re(IHDMil(5))) +
     &    2*(IHDMix(135) + IHDMix(132)*Re(IHDMil(10)) +
     &       IHDMix(133)*Re(IHDMil(11))) +
     &    8*IHDMix(134)*Re(IHDMil(13))

        IHDMix(144) = MA02*IHDMix(140) - 2*IHDMix(142) -
     &    4*(MA02 - 2*MHp2)*IHDMix(143)

	IHDMix(149) = 2*(2*MA02 - MHp2)*IHDMix(146) - MHp2*IHDMix(148)

        IHDMix(156) = ((MA02 + MHH2)*IHDMix(153))/
     &     (MA02 - MHH2) -
     &    ((MHH2 + MHp2)*IHDMix(155))/(MHH2 - MHp2)

	IHDMix(160) = 2*(2*MHH2 - MHp2)*IHDMix(146) + MHp2*IHDMix(159)

        IHDMix(165) = MHH2*IHDMix(161) + 2*IHDMix(163) +
     &    4*(MHH2 - 2*MHp2)*IHDMix(164)

        IHDMix(170) = 4*(MA02 - 2*MHH2)*IHDMix(143) +
     &    2*IHDMix(167) - MA02*IHDMix(169)

	IHDMix(173) = MHH2*IHDMix(171) - 2*(2*MA02 - MHH2)*IHDMix(172)

        IHDMix(2) = -17*(MA02**2 + MHH2**2) -
     &    4*(3*MA02 + 5*MHH2)*IHDMil(9) +
     &    MHH2*(-30*MA02 + 16*IHDMil(14)) +
     &    4*(IHDMil(1)**2 + IHDMil(9)**2 +
     &       MA02*(4*IHDMil(14) - (IHDMil(1)*IHDMix(1))/MA02))

        IHDMix(26) = MA02**2 + MHH2**2 +
     &    18*(MA02 + MHH2)*MHp2 + 2*MHp2**2 -
     &    8*(MA02*MHH2 + IHDMil(12)*IHDMix(11) +
     &       IHDMix(16)/(MHH2 - MHp2)) - 4*IHDMil(1)*IHDMix(21) +
     &    (4*(IHDMil(9)*IHDMix(23) + 2*IHDMil(19)*IHDMix(25)))/
     &     ((MA02 - MHH2)*(MA02 - MHp2))

        IHDMix(30) = 4*(MHp2 - mu2sq)*IHDMix(27) +
     &    IHDMil(2)*IHDMix(28) + (IHDMil(12)*IHDMix(29))/MHp2

        IHDMix(35) = -11*(MA02 - MHH2)**2 - 16*IHDMix(32) +
     &    (4*(MA02 - MHH2)*IHDMil(2)*IHDMix(31) + 8*IHDMix(34))/
     &     Mh02

        IHDMix(4) = -((MA02 + MHH2 - 2*MHp2)*MHp2*
     &       (1 + (2*IHDMil(12))/MHp2)) + IHDMix(3)

        IHDMix(44) = -11*
     &     ((MA02 - MHp2)**2 + (MHH2 - MHp2)**2) -
     &    16*(IHDMix(40) + IHDMix(41)) +
     &    (-(IHDMil(2)*(-4*(MA02 - MHp2)*IHDMix(36) +
     &            4*(MHH2 - MHp2)*IHDMix(37))) +
     &       8*(IHDMix(39) + IHDMix(43)))/Mh02

        IHDMix(47) = (8*MHH2*(MHH2 - mu2sq)*(MHp2 - mu2sq)*
     &       IHDMil(12))/(MHH2 - MHp2)**2 -
     &    (MHH2**2*IHDMix(46))/((Mh02 - 4*MHH2)*(MHH2 - MHp2))

        IHDMix(52) = -((IHDMil(12)*IHDMix(49))/
     &       ((MHH2 - MHp2)*MHp2)) +
     &    (4*(MHp2 - mu2sq)*IHDMil(12)**2*IHDMix(51))/
     &     ((MHH2 - MHp2)**2*MHp2**2)

        IHDMix(59) = ((MHH2 - mu2sq)*IHDMil(17)*IHDMix(55))/
     &     (Mh02 - 4*MHH2) - 2*IHDMil(21)*IHDMix(56) -
     &    ((MHp2 - mu2sq)*IHDMil(20)*IHDMix(58))/(Mh02 - 4*MHp2)

        IHDMix(66) = (8*(MHH2 - mu2sq)*IHDMil(9)**2*
     &       IHDMix(61))/(MHH2 - MHp2)**2 +
     &    IHDMix(65)/(Mh02 - 4*MHp2)

        IHDMix(71) = (8*MA02*(MA02 - mu2sq)*(MHH2 - mu2sq)*
     &       IHDMil(9))/(MA02 - MHH2)**2 -
     &    IHDMix(70)/((4*MA02 - Mh02)*(MA02 - MHH2))

        IHDMix(75) = (8*MA02*(MA02 - mu2sq)*(MHp2 - mu2sq)*
     &       IHDMil(12))/(MA02 - MHp2)**2 -
     &    IHDMix(74)/((4*MA02 - Mh02)*(MA02 - MHp2))

        IHDMix(81) = (2*IHDMil(12)*IHDMix(76))/
     &     ((Mh02 - 4*MHp2)*(MHH2 - MHp2)) -
     &    IHDMix(80)/(Mh02 - 4*MHH2)

        IHDMix(94) = (MA02 - mu2sq)*IHDMil(1)**2*
     &     ((8*IHDMix(82))/(MA02 - MHH2)**2 -
     &       (8*IHDMix(83))/(MA02 - MHp2)**2) +
     &    IHDMix(88)/(Mh02 - 4*MHH2) - IHDMix(93)/(Mh02 - 4*MHp2)

        IHDMix(98) = (4*(MHH2 - mu2sq)*IHDMil(9)**2*
     &       IHDMix(95))/((MA02 - MHH2)**2*MHH2**2) +
     &    (IHDMil(9)*IHDMix(97))/((MA02 - MHH2)*MHH2)

        IHDMix(174) = 8*IHDMix(131) -
     &    ((MA02 + MHp2)*IHDMix(137))/(MA02 - MHp2) -
     &    (2*(IHDMil(1)*IHDMix(144) + IHDMil(12)*IHDMix(149)))/
     &     (MA02 - MHp2)**2 - IHDMix(156) -
     &    2*((IHDMil(12)*IHDMix(160) - IHDMil(9)*IHDMix(165))/
     &        (MHH2 - MHp2)**2 +
     &       (IHDMil(1)*IHDMix(170) + IHDMil(9)*IHDMix(173))/
     &        (MA02 - MHH2)**2)

        IHDMix(114) = 2*MA02**3 + Mh02*IHDMix(112) -
     &    2*(MA02**2*(MHH2 + 5*mu2sq) + MA02*IHDMix(113))

        IHDMix(123) = 2*MA02**3 + Mh02*IHDMix(121) -
     &    2*(MA02**2*(MHp2 + 5*mu2sq) + MA02*IHDMix(122))

        IHDMix(118) = ((MA02 - mu2sq)*IHDMil(16)*IHDMix(114))/
     &     (4*MA02 - Mh02) +
     &    ((MHH2 - mu2sq)*IHDMil(17)*IHDMix(117))/(Mh02 - 4*MHH2)

        IHDMix(127) = ((MA02 - mu2sq)*IHDMil(16)*IHDMix(123))/
     &     (4*MA02 - Mh02) +
     &    ((MHp2 - mu2sq)*IHDMil(20)*IHDMix(126))/(Mh02 - 4*MHp2)

        IHDMix(119) = 2*IHDMil(15)*
     &     (mu2sq*IHDMix(111) + MA02*IHDMix(112)) + IHDMix(118)

        IHDMix(128) = 2*IHDMil(22)*
     &     (mu2sq*IHDMix(120) + MA02*IHDMix(121)) + IHDMix(127)

        IHDMix(129) = IHDMix(2) + IHDMix(26) - IHDMix(35) +
     &    IHDMix(44) + (-16*(MHp2 - mu2sq)*IHDMix(30) +
     &       4*MHp2**2*IHDMix(52))/(Mh02 - 4*MHp2) +
     &    (IHDMil(1)*(4*IHDMix(71) - 4*IHDMix(75)))/MA02 +
     &    2*((1 + (2*IHDMil(2))/Mh02)*IHDMix(4) +
     &       IHDMix(66)/(Mh02 - 4*MHH2) +
     &       IHDMix(94)/(4*MA02 - Mh02)) +
     &    4*(IHDMil(2)*IHDMix(81) +
     &       (MHH2**2*IHDMix(98) - IHDMil(2)*IHDMix(102))/
     &        (Mh02 - 4*MHH2)) -
     &    4*(IHDMix(10) + (IHDMil(9)*IHDMix(47))/MHH2 +
     &       (MHp2**2*IHDMix(106) - IHDMil(2)*IHDMix(110))/
     &        (Mh02 - 4*MHp2)) +
     &    16*(IHDMix(59)/(MHH2 - MHp2)**2 +
     &       IHDMix(119)/(MA02 - MHH2)**2) -
     &    (16*IHDMix(128))/(MA02 - MHp2)**2

        IHDMindMasses =
     &   Alfa2/(MW2**2*Pi**2*SW2**2)*
     &    (1/2048.D0*IHDMix(129) + 1/1024.D0*IHDMix(174))

	end
