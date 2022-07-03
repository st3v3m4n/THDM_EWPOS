c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich


* NSatSHHeqHp.F
* generated 22-Dec-2016 15:42

C> @brief This subroutine calculates \f$\delta\rho^{\alpha_t\lambda_i}_\mathrm{NS}\f$ with \f$m_{H^0}=m_{H^\pm}\f$.
C>
C> @ingroup group_rho
	subroutine calcNSatSHHeqHp
	implicit none


#include "NSatSHHeqHp.h"


	RealType T134sub

	external T134sub

	ComplexType A0FH, B0FH

	external A0FH, B0FH

#include "paraTHDM.h"
#include "paraSM.h"
	NSatShhl(1) = A0FH(MT2)

	NSatShhl(2) = B0FH(MHp2,0.D0,MT2)

	NSatShhl(3) = B0FH(MHp2,MT2,MT2)

	NSatShhl(4) = A0FH(MA02)

	NSatShhl(5) = A0FH(MHp2)

	NSatShhl(6) = T134sub(MT2,MHp2,0.D0)

	NSatShhl(7) = T134sub(MT2,MA02,0.D0)

	NSatShhl(8) = T134sub(MT2,MT2,MA02)

	NSatShhl(9) = T134sub(MT2,MT2,MHp2)

	NSatShhx(32) = 6*MA02**2 - MHp2**2

	NSatShhx(33) = MHp2**2 + 6*MT2**2 - 4*MT2*MHp2

	NSatShhx(11) = 5*MA02**4*MHp2 + 16*MT2**3*MHp2**2

        NSatShhx(12) =
     &   11*MHp2**4 - 256*MT2**4 - 704*MT2**3*MHp2 +
     &    400*MT2**2*MHp2**2 - 102*MT2*MHp2**3

        NSatShhx(13) =
     &   MHp2**4 + 32*MT2**4 - 64*MT2**3*MHp2 +
     &    82*MT2**2*MHp2**2 - 18*MT2*MHp2**3

        NSatShhx(14) =
     &   MHp2**5 + 512*MT2**5 - 64*MT2**4*MHp2 +
     &    96*MT2**3*MHp2**2 + 88*MT2**2*MHp2**3 - 27*MT2*MHp2**4

        NSatShhx(16) =
     &   MHp2**3 + 16*MT2**3 + 14*MT2**2*MHp2 - 8*MT2*MHp2**2

        NSatShhx(17) =
     &   MHp2**4 + 64*MT2**4 + 24*MT2**3*MHp2 - 2*MT2**2*MHp2**2 -
     &    5*MT2*MHp2**3

        NSatShhx(18) =
     &   2*MHp2**3 + 16*(MT2**3 + MT2**2*MHp2) - 11*MT2*MHp2**2

	NSatShhx(20) = 6*MA02**2 - 5*MA02*MHp2 + 5*MHp2**2

        NSatShhx(21) =
     &   18*MA02**3 - 10*MA02**2*MHp2 + 19*MA02*MHp2**2 + MHp2**3

	NSatShhx(22) = MHp2**2 + 2*(MA02**2 + MA02*MHp2)

	NSatShhx(23) = 4*MA02**2 - MA02*MHp2 + MHp2**2

        NSatShhx(3) = 15*MHp2**3 - 16*MT2**3 +
     &    24*MT2**2*MHp2 - 71*MT2*MHp2**2

        NSatShhx(34) =
     &   2*MA02**3 - MT2*NSatShhx(32) + MA02*NSatShhx(33)

        NSatShhx(35) =
     &   -(MA02*MHp2*(MA02 - MHp2 + 5*MT2)) +
     &    MT2**2*(MA02 + 4*MHp2)

        NSatShhx(37) =
     &   3*MHp2**3 - 16*MT2**3 + 34*MT2**2*MHp2 - 20*MT2*MHp2**2

        NSatShhx(38) =
     &   2*MHp2**3 + 16*MT2**3 - 4*MT2**2*MHp2 - 9*MT2*MHp2**2

        NSatShhx(39) =
     &   19*MHp2**3 + 224*MT2**2*MHp2 - 128*(MT2**3 + MT2*MHp2**2)

        NSatShhx(4) = 2*MHp2**4 - 64*MT2**4 +
     &    112*MT2**3*MHp2 - 202*MT2**2*MHp2**2 + 29*MT2*MHp2**3

	NSatShhx(41) = 6*MA02**2 - 15*MA02*MHp2 + 4*MHp2**2

	NSatShhx(42) = 5*MA02**2 + 4*MA02*MHp2 + MHp2**2

	NSatShhx(46) = MHp2**2 - MT2**2 - 3*MT2*MHp2

        NSatShhx(5) = MHp2**3 - 32*(MT2**3 + MT2**2*MHp2) +
     &    3*MT2*MHp2**2

	NSatShhx(50) = 3*MHp2**2 - 16*MT2**2 - 12*MT2*MHp2

        NSatShhx(51) =
     &   7*MHp2**3 - 448*MT2**3 + 224*MT2**2*MHp2 - 60*MT2*MHp2**2

        NSatShhx(52) =
     &   MHp2**3 - 64*MT2**3 + 30*MT2**2*MHp2 - 8*MT2*MHp2**2

	NSatShhx(54) = 5*MA02**2 - 3*MA02*MHp2 + MHp2**2

	NSatShhx(55) = 5*MA02**2 - 2*MA02*MHp2 + MHp2**2

	NSatShhx(57) = 8*MA02**2 - 13*MA02*MHp2 + 4*MHp2**2

	NSatShhx(58) = 10*MA02**2 - 19*MA02*MHp2 + 5*MHp2**2

	NSatShhx(59) = 9*MA02**2 - 22*MA02*MHp2 + 5*MHp2**2

	NSatShhx(62) = 9*MA02**2 - 2*MA02*MHp2 + MHp2**2

	NSatShhx(64) = 6*MA02**2 + 9*MA02*MHp2 - MHp2**2

	NSatShhx(65) = 5*MA02**2 + 18*MA02*MHp2 - 3*MHp2**2

        NSatShhx(66) =
     &   MHp2**3*(2*MA02 + MHp2) - 32*MT2**3*(MA02 + MHp2)

	NSatShhx(7) = MHp2**2 + 24*MT2**2 - 8*MT2*MHp2

	NSatShhx(8) = MHp2**2 + 56*MT2**2 - 8*MT2*MHp2

	NSatShhx(1) = MHp2**2 + 2*MT2**2 - 5*MT2*MHp2

        NSatShhx(15) =
     &   -(8*MT2*(MA02*MHp2*NSatShhx(13))) +
     &    (MHp2 - 4*MT2)**2*NSatShhx(11) + MA02**3*NSatShhx(12) +
     &    2*MA02**2*NSatShhx(14)

        NSatShhx(19) =
     &   MT2**2*(MHp2**2*(MHp2 - 4*MT2)**2) -
     &    MT2*(MA02*MHp2*NSatShhx(18)) - MA02**3*NSatShhx(16) +
     &    MA02**2*NSatShhx(17)

        NSatShhx(24) =
     &   -(2*MT2*(MHp2*NSatShhx(21))) +
     &    16*MT2**2*((MA02 + MHp2)*NSatShhx(22)) -
     &    32*MT2**3*NSatShhx(23) + MA02*MHp2**2*NSatShhx(20)

	NSatShhx(27) = MA02**4 + MT2*(MA02*MHp2*(MHp2 - 4*MT2))

	NSatShhx(28) = 3*MHp2**2 - 44*MT2**2 - 4*MT2*MHp2

	NSatShhx(29) = MA02**3*(MHp2 - 7*MT2) + MT2**2*MHp2**2

        NSatShhx(36) =
     &   2/MT2*(NSatShhl(1)*NSatShhx(35)) +
     &    ((MHp2 - 4*MT2)**2*NSatShhx(34))/
     &     ((MHp - 2*MT)**2*(MHp + 2*MT)**2)

        NSatShhx(40) =
     &   4*MA02**2*NSatShhx(37) + 2*MHp2**2*NSatShhx(38) -
     &    MA02*MHp2*NSatShhx(39)

        NSatShhx(43) =
     &   (MA02 - MHp2)*(2*MA02 - MHp2)*MHp2**3 +
     &    16*MT2**3*(MA02*(2*MA02 + 3*MHp2)) -
     &    MT2*(MHp2**2*NSatShhx(41)) - 4*MT2**2*(MHp2*NSatShhx(42))

        NSatShhx(47) =
     &   MA02**3*(MHp2 - 2*MT2) - MA02**2*(MHp2**2 - 4*MT2**2) +
     &    MT2**2*MHp2**2 + MT2*(MA02*NSatShhx(46))

	NSatShhx(48) = MHp2*(MHp2 - 4*MT2) - MA02*(MHp2 - 2*MT2)

        NSatShhx(53) =
     &   2*MT2*(MHp2**2*NSatShhx(50)) - MA02*MHp2*NSatShhx(51) +
     &    4*MA02**2*NSatShhx(52)

        NSatShhx(56) =
     &   16*MT2**2*(MA02*(2*MA02 + MHp2)) -
     &    6*MT2*(MHp2*NSatShhx(55)) + MHp2**2*NSatShhx(54)

        NSatShhx(6) = 5*MA02**3*(MHp2 - 4*MT2)**2 +
     &    2*MT2*(MHp2*NSatShhx(5)) + MA02**2*NSatShhx(3) -
     &    MA02*NSatShhx(4)

        NSatShhx(60) =
     &   (MA02 - MHp2)*(2*MA02 - MHp2)*MHp2**3 -
     &    2*MT2*(MHp2**2*NSatShhx(57)) +
     &    4*MT2**2*(MHp2*NSatShhx(58)) - 4*MT2**3*NSatShhx(59)

        NSatShhx(63) =
     &   -(MA02**2*MHp2*(2*MA02 + MHp2 - 16*MT2)) -
     &    2*MT2**2*NSatShhx(62)

        NSatShhx(67) =
     &   -(2*MT2*(MHp2**2*NSatShhx(64))) +
     &    4*MT2**2*(MHp2*NSatShhx(65)) + MA02*NSatShhx(66)

        NSatShhx(9) = 8*MA02**2*(MHp2 - 5*MT2) -
     &    MHp2*NSatShhx(7) + MA02*NSatShhx(8)

        NSatShhx(10) =
     &   MT2*NSatShhx(6) + 4*MT2**2*(NSatShhl(1)*NSatShhx(9))

        NSatShhx(2) = MHp2*(MHp2 - 5*MT2)*(MHp2 - 2*MT2) -
     &    MA02*NSatShhx(1)

        NSatShhx(25) =
     &   -(MT2*(NSatShhx(15) + 16*NSatShhl(1)*NSatShhx(19) -
     &        4*NSatShhl(1)**2*NSatShhx(24)))

        NSatShhx(30) =
     &   -2*NSatShhx(27) + MA02**2*NSatShhx(28) - 4*NSatShhx(29)

        NSatShhx(31) =
     &   3*MA02**3 + 2*MA02*MHp2*(MHp2 - 5*MT2) -
     &    MA02**2*(2*MHp2 + 3*MT2) + MT2*MHp2**2

        NSatShhx(44) =
     &   (NSatShhl(4)*NSatShhx(36))/(MA02 - MT2) +
     &    (NSatShhl(5)*(2/MT2*(NSatShhl(1)*NSatShhx(43)) +
     &         NSatShhx(40)))/
     &     (MHp2*(MHp - 2*MT)**2*(MHp + 2*MT)**2)

        NSatShhx(45) =
     &   (MA02 - MHp2)*(2*MA02 - MHp2)*MHp2 + MT2**2*MA02 -
     &    MT2*(2*MA02 - MHp2)**2

        NSatShhx(49) =
     &   (NSatShhl(7)*NSatShhx(47))/(MA02 - MT2) +
     &    MA02*NSatShhl(8)*NSatShhx(48)

        NSatShhx(61) =
     &   (MT2*(NSatShhl(5)*
     &          (NSatShhx(53) + 4*NSatShhl(1)*NSatShhx(56)))/MHp2+
     &        2*NSatShhl(9)*NSatShhx(60))/
     &    ((MHp - 2*MT)**2*(MHp + 2*MT)**2)

        NSatShhx(68) =
     &   (NSatShhl(8)*NSatShhx(63))/(MA02*(MA02 - 4*MT2)) +
     &    (NSatShhl(9)*NSatShhx(67))/
     &     (MHp2*(MHp - 2*MT)**2*(MHp + 2*MT)**2)

        NSatShhx(71) =
     &   Re(NSatShhl(1)) + (MHp2 - MT2)*Re(NSatShhl(2)) -
     &    (MHp2 - 4*MT2)*Re(NSatShhl(3))

        NSatShhx(73) =
     &   MT2*(2 + 1/MT2*Re(NSatShhl(1))) +
     &    (MHp2 - MT2)*Re(NSatShhl(2)) -
     &    (MHp2 - 4*MT2)*Re(NSatShhl(3))

        NSatShhx(74) =
     &   -8*MA02*MHp2 + 2*MHp2**2 +
     &    MA02**2*(4 - 1/MT2*Re(NSatShhl(1)))

        NSatShhx(75) =
     &   (MHp2 - MT2)*Re(NSatShhl(2)) -
     &    (MHp2 - 4*MT2)*Re(NSatShhl(3))

        NSatShhx(26) =
     &   8*NSatShhl(1)**2*NSatShhx(2) +
     &    NSatShhx(10)/(MA02 - MT2) +
     &    NSatShhx(25)/(MA02*MHp2*(MA0 - 2*MT)*(MA0 + 2*MT))

        NSatShhx(69) =
     &   MT2*((NSatShhl(4)*
     &          (NSatShhx(30) + 4*NSatShhl(1)*NSatShhx(31)))/
     &        (MA02*(MA02 - 4*MT2)) + NSatShhx(44) + 2*NSatShhx(68)
     &       ) + 2*NSatShhl(6)*NSatShhx(45) - 2*NSatShhx(49) -
     &    NSatShhx(61)

        NSatShhx(72) =
     &   2*MT2*(MA02 + MHp2) + (3*MA02 - MHp2)*NSatShhx(71)

        NSatShhx(76) =
     &   MA02*NSatShhl(4)*NSatShhx(73) +
     &    (NSatShhl(5)*(MT2*NSatShhx(74) - MA02**2*NSatShhx(75)))/
     &     MHp2

        NSatShhx(70) =
     &   NSatShhx(26)/((MA02 - MHp2)*(MHp2 - 4*MT2)**2) +
     &    (2*NSatShhx(69))/(MA02 - MHp2)**2

	NSatShhx(77) = (MA02 - MHp2)*NSatShhx(72) + 2*NSatShhx(76)

        NSatSHHeqHp = -(Alfa2/(MW2**2*Pi**2*SW2**2)*
     &      (3/512.D0*NSatShhx(70) -
     &         (3*MT2)/256.D0*NSatShhx(77)/(MA02 - MHp2)**2)/TB2)

	end
