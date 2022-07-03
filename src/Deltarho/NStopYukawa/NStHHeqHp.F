c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich



* NStHHeqHp.F
* generated 14-Dec-2015 16:47

C> @brief This subroutine calculates \f$\delta\rho^{(2)}_\mathrm{t,NS}\f$ with \f$m_{H^0}=m_{H^\pm}\f$.
C>
C> @ingroup group_rho
	subroutine calcNStHHeqHp
	implicit none


#include "NStHHeqHp.h"


	RealType T134sub

	external T134sub

	ComplexType A0FH, B0FH

	external A0FH, B0FH

#include "paraTHDM.h"
#include "paraSM.h"
	NSthhl(1) = A0FH(MA02)

	NSthhl(10) = T134sub(MT2,MT2,MA02)

	NSthhl(11) = T134sub(MHp2,0.D0,0.D0)

	NSthhl(12) = T134sub(MT2,MHp2,0.D0)

	NSthhl(13) = T134sub(MT2,MT2,MHp2)

	NSthhl(2) = A0FH(MT2)

	NSthhl(3) = B0FH(MHp2,0.D0,MT2)

	NSthhl(4) = B0FH(MHp2,MT2,MT2)

	NSthhl(5) = A0FH(MHp2)

	NSthhl(6) = B0FH(MT2,0.D0,MHp2)

	NSthhl(7) = B0FH(MT2,MA02,MT2)

	NSthhl(8) = B0FH(MT2,MHp2,MT2)

	NSthhl(9) = T134sub(MT2,MA02,0.D0)

	NSthhx(17) = 19*MHp2**2 - 24*MT2**2 - 88*MT2*MHp2

	NSthhx(20) = MHp2**2 + 17*MT2**2 - 6*MT2*MHp2

	NSthhx(23) = MA02*(MHp2 - 6*MT2) + 2*MT2*MHp2

	NSthhx(25) = 13*MHp2 - 8/MT2*((MA02 - MHp2 - 2*MT2)*NSthhl(2))

	NSthhx(28) = 5*MHp2 + 2/MT2*((3*MA02 - 2*MHp2)*NSthhl(2))

	NSthhx(3) = 7*MHp2**2 + 4*MT2**2 - 7*MT2*MHp2

	NSthhx(33) = -(MHp2*(MHp2 + 2*MT2)) + MA02*(MHp2 + 3*MT2)

	NSthhx(36) = MHp2**2 + MT2**2 - 4*MT2*MHp2

	NSthhx(16) = 2*MHp2**2 + 6*MT2**2 - 5*MT2*MHp2

	NSthhx(18) = 27*MHp2**2 - 4/MT2*(NSthhl(2)*NSthhx(17))

	NSthhx(19) = 11*MHp2 + 12/MT2**2*((MHp2 - MT2)*NSthhl(2)**2)

	NSthhx(21) = 9*(MHp2 - 2*MT2)*NSthhl(2) + 2*NSthhx(20)

        NSthhx(24) = MA02*(MA02 - 2*MHp2 + 4*MT2)*NSthhl(10) +
     &    NSthhl(13)*NSthhx(23)

        NSthhx(26) = 9*MA02**2 + MA02*(3*MHp2 + 2*MT2) -
     &    16*MT2**2*(2 + 1/MT2**2*NSthhl(2)**2) - 2*MT2*NSthhx(25)

	NSthhx(27) = 3*MA02 - 2*(MHp2 + MT2*(3 + 2/MT2*NSthhl(2)))

	NSthhx(29) = MA02*(MHp2 + 4*MT2) - 2*MT2*NSthhx(28)

	NSthhx(32) = MT2 + 1/MT2*((MA02 - MHp2 + MT2)*NSthhl(2))

        NSthhx(34) = MHp2*(3*MA02 - 4*MHp2 + 2*MT2) +
     &    2/MT2*(NSthhl(2)*NSthhx(33))

	NSthhx(37) = MHp2**2*(MHp2 - 3*MT2) - MA02*NSthhx(36)

        NSthhx(4) = 10*MHp2**2 - 4*MT2**2 - 2*MT2*MHp2 +
     &    1/MT2*(NSthhl(2)*NSthhx(3))

	NSthhx(1) = 3*MHp2**2 + 5*MT2**2 - 12*MT2*MHp2

        NSthhx(14) = MA02**2 + 3*MA02*MHp2 + 8*MHp2**2 -
     &    9*MT2*(MA02 - MHp2)

	NSthhx(2) = 61*MHp2 + 4/MT2*((27*MHp2 - 19*MT2)*NSthhl(2))

        NSthhx(22) = 96*MT2**3 + MT2*NSthhx(18) -
     &    4*MT2**2*NSthhx(19) - 8*NSthhl(13)*NSthhx(16) +
     &    8*(MHp2**3 + NSthhl(5)*NSthhx(21))

        NSthhx(30) = 8*NSthhx(24) - (MA02 - MHp2)*NSthhx(26) -
     &    4*(MA02*NSthhl(1)*NSthhx(27) - NSthhl(5)*NSthhx(29))

        NSthhx(35) = MA02*NSthhl(1)*
     &     (MA02 - 2*MHp2 + 2*NSthhx(32)) - NSthhl(5)*NSthhx(34)

        NSthhx(38) = MA02*(MA02 - MT2)*(MA02 - MHp2 - MT2)*
     &     NSthhl(9) + NSthhl(12)*NSthhx(37)

	NSthhx(48) = Re(NSthhl(1))/MA02 - Re(NSthhl(7))

	NSthhx(5) = 5*MHp2**2 + (NSthhl(5)*NSthhx(4))/MHp2

        NSthhx(11) = 33*MA02 -
     &    4/MT2*((MA02 - 36*MT2)*NSthhl(2)) -
     &    8/MT2**2*((MA02 + 4*MT2)*NSthhl(2)**2)

        NSthhx(12) = 5*MA02 - 12*MT2 +
     &    1/MT2*((MA02 + 4*MT2)*NSthhl(2))

        NSthhx(15) = MT2*NSthhx(14) +
     &    8*MA02*(MA02 - 2*MT2)*NSthhl(10) -
     &    8*MHp2*(MHp2 - 2*MT2)*NSthhl(13)

        NSthhx(31) = NSthhx(22)/(MHp2*(MHp2 - 4*MT2)) -
     &    NSthhx(30)/(MA02 - MHp2)**2

        NSthhx(39) = MT2*((1 - 2/MT2*NSthhl(2))*NSthhl(2)) +
     &    (MT2*NSthhx(35) + 2*NSthhx(38))/(MA02 - MHp2)**2

	NSthhx(44) = 2*MHp2 + 1/MT2*((2*MA02 - MHp2)*Re(NSthhl(2)))

        NSthhx(45) = (MHp2 - MT2)*Re(NSthhl(3)) -
     &    (MHp2 - 4*MT2)*Re(NSthhl(4))

        NSthhx(47) = MT2*(4 - 1/MT2*Re(NSthhl(2))) -
     &    (MHp2 - MT2)*Re(NSthhl(3)) + (MHp2 - 4*MT2)*Re(NSthhl(4))

        NSthhx(49) = -(2*MT2*(3 + 1/MT2*Re(NSthhl(2)))) +
     &    MA02*NSthhx(48) + 2*Re(NSthhl(5)) -
     &    (MHp2 - MT2)*Re(NSthhl(6)) - (MHp2 - 4*MT2)*Re(NSthhl(8))

        NSthhx(50) = (MHp2 - MT2)*Re(NSthhl(6)) +
     &    MA02*Re(NSthhl(7)) + (MHp2 - 4*MT2)*Re(NSthhl(8))

        NSthhx(52) = MT2*(9 + 4/MT2*Re(NSthhl(2))) +
     &    2*NSthhx(50) - 2*Re(NSthhl(1)) - 4*Re(NSthhl(5))

        NSthhx(53) = MT2 + 2*NSthhx(50) - 2*Re(NSthhl(1)) +
     &    4*Re(NSthhl(2)) - 4*Re(NSthhl(5))

        NSthhx(6) = 37*MT2**3 - MT2**2*NSthhx(2) -
     &    8*(MT2*NSthhx(5) + NSthhl(12)*NSthhx(1))

        NSthhx(8) = MA02 -
     &    4/MT2*((3*MA02 - 8*MT2)*NSthhl(2)) - 24*NSthhl(10)

	NSthhx(9) = 3*MA02 - 2*MT2*(5 - 1/MT2*NSthhl(2))

        NSthhx(10) = 5*MA02**2 - MT2*NSthhx(8) -
     &    4*(MT2**2*(7 + 2/MT2**2*NSthhl(2)**2) +
     &       MA02*(2*NSthhl(10) - (NSthhl(1)*NSthhx(9))/MA02))

        NSthhx(13) = -36*MT2**3 + 18*MT2*MA02**2 +
     &    MT2**2*NSthhx(11) +
     &    8*(MT2*(NSthhl(1)*NSthhx(12)) + MA02**2*NSthhl(9)) -
     &    8*((-4*MT2**2 + 5*MT2*MA02)*NSthhl(9) +
     &       (MA02**2 - 2*MT2*MA02)*NSthhl(10))

        NSthhx(40) = MT2*NSthhx(31) -
     &    NSthhx(15)/(MA02 - MHp2) + 4*NSthhx(39)

	NSthhx(42) = MA02**2 - MHp2**2 + 2*MA02*NSthhl(1)

        NSthhx(43) = MT2*(2 + 1/MT2*Re(NSthhl(2))) +
     &    (MHp2 - MT2)*Re(NSthhl(3)) - (MHp2 - 4*MT2)*Re(NSthhl(4))

	NSthhx(46) = MT2*NSthhx(44) + (2*MA02 - MHp2)*NSthhx(45)

        NSthhx(51) = -(3*MT2*(1 + 1/MT2*Re(NSthhl(2)))) -
     &    1/MT2*(NSthhl(2)*NSthhx(49)) +
     &    (NSthhl(5)*NSthhx(47))/MHp2 - 2*NSthhx(50) +
     &    2*Re(NSthhl(1)) + (MHp2 - MT2)*Re(NSthhl(3)) -
     &    (MHp2 - 4*MT2)*Re(NSthhl(4)) + 4*Re(NSthhl(5))

	NSthhx(54) = 2/MT2*(NSthhl(2)*NSthhx(52)) - 3*NSthhx(53)

        NSthhx(7) = -40*NSthhl(2)**2 + 16*MHp2*NSthhl(11) -
     &    8*(5*MHp2 - 6*MT2)*NSthhl(13) - NSthhx(6)/(MHp2 - MT2)

        NSthhx(41) = NSthhx(7) -
     &    (2*MT2*NSthhx(10) - NSthhx(13))/(MA02 - 4*MT2) +
     &    2*NSthhx(40)

        NSthhx(55) = (2*
     &       (NSthhx(42)*NSthhx(43) - 2*NSthhl(5)*NSthhx(46)))/
     &     (MA02 - MHp2)**2 + 4*NSthhx(51) - NSthhx(54)

        NStHHeqHp = Alfa2/(MW2**2*Pi**2*SW2**2)*
     &    (3/2048.D0*NSthhx(41) + (3*MT2)/512.D0*NSthhx(55))/TB2

	end
