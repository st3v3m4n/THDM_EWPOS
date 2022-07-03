c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich


* NSat2indMasses.F
* generated 21-Dec-2016 14:47

	subroutine calcNSat2indMasses
	implicit none


#include "NSat2indMasses.h"


	RealType T134sub

	external T134sub

	ComplexType A0FH, B0FH

	external A0FH, B0FH

#include "paraTHDM.h"
#include "paraSM.h"
	NSat2il(1) = A0FH(MA02)

	NSat2il(10) = T134sub(MT2,MT2,MA02)

	NSat2il(11) = T134sub(MT2,MT2,MHH2)

	NSat2il(12) = T134sub(MHp2,0.D0,0.D0)

	NSat2il(13) = T134sub(MT2,MHp2,0.D0)

	NSat2il(14) = T134sub(MT2,MT2,MHp2)

	NSat2il(2) = A0FH(MHH2)

	NSat2il(3) = A0FH(MHp2)

	NSat2il(4) = B0FH(MT2,0.D0,MHp2)

	NSat2il(5) = B0FH(MT2,MA02,MT2)

	NSat2il(6) = B0FH(MT2,MHH2,MT2)

	NSat2il(7) = A0FH(MT2)

	NSat2il(8) = T134sub(MT2,MA02,0.D0)

	NSat2il(9) = T134sub(MT2,MHH2,0.D0)

	NSat2ix(26) = 3*MA02**2 - 4*MT2**2 - 5*MT2*MA02

	NSat2ix(8) = MT2**4 + MT2**2*MHp2**2

        NSat2ix(2) = 4*MHp2**4 - 8*MT2**4 - 4*MT2**3*MHp2 +
     &    44*MT2**2*MHp2**2 - 27*MT2*MHp2**3

	NSat2ix(24) = MA02**2 - 44*MT2**2 + 31*MT2*MA02

        NSat2ix(27) = 6*MA02**2 + 8*MT2**2 - 20*MT2*MA02 +
     &    1/MT2*(NSat2il(7)*NSat2ix(26))

        NSat2ix(9) = 5*MHp2**4 + 96*MT2**3*MHp2 -
     &    19*MT2*MHp2**3 - 32*NSat2ix(8)

	NSat2ix(1) = 2*MHp2**2 + 3*MT2**2 - 6*MT2*MHp2

	NSat2ix(10) = 151*MHp2**4 - 4/MT2*(NSat2il(7)*NSat2ix(9))

	NSat2ix(13) = 5*MHH2**2 + 88*MT2**2 - 44*MT2*MHH2

        NSat2ix(14) = 2*(MHH2 - 5*MT2) +
     &    1/MT2*((MHH2 - 6*MT2)*NSat2il(7))

	NSat2ix(16) = 5*MA02**2 - 8*MT2**2 - 32*MT2*MA02

        NSat2ix(17) = 4*MA02 - 2*MT2 +
     &    1/MT2*((3*MA02 + 2*MT2)*NSat2il(7))

        NSat2ix(18) = 1/MT2**2*
     &     ((2*MA02 - MT2)*NSat2il(7)**2) + NSat2il(10)

	NSat2ix(25) = 11*MA02**2 + 4/MT2*(NSat2il(7)*NSat2ix(24))

	NSat2ix(28) = -143*MT2**2 + (8*NSat2il(1)*NSat2ix(27))/MA02

        NSat2ix(3) = -8*(MHp2**4 + MT2**4) + 88*MT2**3*MHp2 -
     &    144*MT2**2*MHp2**2 + 63*MT2*MHp2**3 -
     &    1/MT2*(NSat2il(7)*NSat2ix(2))

	NSat2ix(40) = Re(NSat2il(1))/MA02 - Re(NSat2il(5))

        NSat2ix(11) = 32*MHp2**5 - 128*MT2**5 -
     &    272*MT2**4*MHp2 + 760*MT2**3*MHp2**2 -
     &    97*MT2**2*MHp2**3 - MT2*NSat2ix(10)

        NSat2ix(15) = 9*MHH2**2 +
     &    16*MT2**2*(23 - 1/MT2**2*NSat2il(7)**2) -
     &    8*MT2*(13*MHH2 + 6*NSat2il(11)) -
     &    4/MT2*(NSat2il(7)*NSat2ix(13)) +
     &    8*MHH2*(NSat2il(11) - (NSat2il(2)*NSat2ix(14))/MHH2)

        NSat2ix(19) = 21*MA02**2 + 32*MT2**2 -
     &    4/MT2*(NSat2il(7)*NSat2ix(16)) +
     &    4*MT2*(19*MA02 - 4*NSat2ix(18)) -
     &    8*MA02*(3*NSat2il(10) - (NSat2il(1)*NSat2ix(17))/MA02)

        NSat2ix(23) = (3*MA02 - 4*MT2)*NSat2il(7)**2 +
     &    MA02*(3*MA02 - 10*MT2)*NSat2il(10)

        NSat2ix(29) = 24*MA02**3 + 12*MT2**3 +
     &    MT2*NSat2ix(25) + MA02*NSat2ix(28)

	NSat2ix(32) = 11*MHH2 + 4/MT2*((17*MHH2 - 13*MT2)*NSat2il(7))

        NSat2ix(33) = 6*MHH2 - 4*MT2 +
     &    1/MT2*((3*MHH2 - MT2)*NSat2il(7))

	NSat2ix(34) = 3*MHH2**2 + 7*MT2**2 - 12*MT2*MHH2

        NSat2ix(4) = MT2*
     &     (NSat2il(3)*NSat2ix(3))/(MHp2*(MHp2 - 4*MT2)**2) -
     &    NSat2il(13)*NSat2ix(1)

        NSat2ix(41) = -(2*MT2*(3 + 1/MT2*Re(NSat2il(7)))) +
     &    MA02*NSat2ix(40) + Re(NSat2il(2)) + Re(NSat2il(3)) -
     &    (MHp2 - MT2)*Re(NSat2il(4)) -
     &    (MHH2 - 4*MT2)*Re(NSat2il(6))

        NSat2ix(6) = 3*MHp2**4 + 8*MT2**4 - 36*MT2**3*MHp2 +
     &    56*MT2**2*MHp2**2 - 24*MT2*MHp2**3

        NSat2ix(7) = 3*MHp2**3 - 8*MT2**3 + 24*MT2**2*MHp2 -
     &    18*MT2*MHp2**2

        NSat2ix(12) = MT2*NSat2ix(11)/(MHp2 - MT2) -
     &    8*(NSat2il(14)*NSat2ix(6) + NSat2il(7)**2*NSat2ix(7))

        NSat2ix(20) = NSat2ix(15)/(MHH2 - 4*MT2)**2 +
     &    NSat2ix(19)/(MA02*(MA02 - 4*MT2))

	NSat2ix(22) = NSat2il(7)**2 + (MHH2 - 2*MT2)*NSat2il(11)

	NSat2ix(30) = MT2*NSat2ix(29)/(MA02 - MT2) - 8*NSat2ix(23)

	NSat2ix(31) = 3*MA02**2 - MT2**2 - 4*MT2*MA02

        NSat2ix(35) = 3*MT2**3 - MT2**2*NSat2ix(32) -
     &    8*(MT2*(3*MHH2**2 + NSat2il(2)*NSat2ix(33)) +
     &       NSat2il(9)*NSat2ix(34))

        NSat2ix(37) = (MHp2 - MT2)*Re(NSat2il(4)) +
     &    MA02*Re(NSat2il(5)) + (MHH2 - 4*MT2)*Re(NSat2il(6))

        NSat2ix(38) = MT2 + 2*NSat2ix(37) -
     &    2*(Re(NSat2il(1)) + Re(NSat2il(2)) + Re(NSat2il(3))) +
     &    4*Re(NSat2il(7))

        NSat2ix(39) = MT2*(9 + 4/MT2*Re(NSat2il(7))) +
     &    2*NSat2ix(37) - 2*
     &     (Re(NSat2il(1)) + Re(NSat2il(2)) + Re(NSat2il(3)))

        NSat2ix(42) = MT2*(3 + 4/MT2*Re(NSat2il(7))) +
     &    1/MT2*(NSat2il(7)*NSat2ix(41))

	NSat2ix(5) = MHp2*NSat2il(12) - NSat2ix(4)/(MHp2 - MT2)

        NSat2ix(21) = MT2**2*NSat2ix(20) + 8*NSat2ix(5) +
     &    NSat2ix(12)/(MHp2*(MHp2 - 4*MT2)**2)

        NSat2ix(36) = -24*NSat2ix(22) +
     &    NSat2ix(30)/(MA02 - 4*MT2) +
     &    (8*NSat2il(8)*NSat2ix(31))/(MA02 - MT2) -
     &    NSat2ix(35)/(MHH2 - MT2)

        NSat2ix(43) = 2/MT2*(NSat2il(7)*NSat2ix(39)) +
     &    8*NSat2ix(37) - 3*NSat2ix(38) + 4*NSat2ix(42) -
     &    8*(Re(NSat2il(1)) + Re(NSat2il(2)) + Re(NSat2il(3)))

        NSat2indMasses =
     &   Alfa2/(MW2**2*Pi**2*SW2**2)*
     &    (3/2048.D0*(2*NSat2ix(21) + NSat2ix(36)) -
     &       (3*MT2)/512.D0*NSat2ix(43))/TB2


	end
