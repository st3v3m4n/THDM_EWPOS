c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich


* NSHHHeqHp.F
* generated 15-Dec-2015 12:37

C> @brief This subroutine calculates \f$\delta \rho^{(2)}_\mathrm{H,NS}\f$ for \f$m_{H^0}=m_{H^\pm}\f$.
C>
C> @ingroup group_rho
	subroutine calcNSHHHeqHp
	implicit none


#include "NSHHHeqHp.h"


	RealType T134sub

	external T134sub

	ComplexType A0FH, B0FH

	external A0FH, B0FH

#include "paraTHDM.h"
#include "paraSM.h"
	NSHhhl(1) = B0FH(MHp2,MA02,MA02)

	NSHhhl(2) = A0FH(MHp2)

	NSHhhl(3) = B0FH(MHp2,MHp2,MHp2)

	NSHhhl(4) = A0FH(MA02)

	NSHhhl(5) = T134sub(MHp2,MHp2,MHp2)

	NSHhhl(6) = T134sub(MHp2,MA02,MA02)

	NSHhhl(7) = T134sub(MHp2,MHp2,MA02)

        NSHhhx(6) = 5*MA02**3 - 23*MA02**2*MHp2 +
     &    10*MA02*MHp2**2 - MHp2**3

        NSHhhx(10) = (12*MA02 - 5*MHp2)*NSHhhl(5) -
     &    2*(3*MA02 - MHp2)*NSHhhl(7)

	NSHhhx(7) = 32*MA02**3 - (NSHhhl(2)*NSHhhx(6))/MHp2

        NSHhhx(9) = 6*MA02**3 - 58*MA02**2*MHp2 +
     &    29*MA02*MHp2**2 - 4*MHp2**3

        NSHhhx(11) = MA02*NSHhhl(6)*NSHhhx(9) +
     &    (4*MA02 - MHp2)**2*MHp2*(NSHhhl(2)**2 + NSHhhx(10))

	NSHhhx(3) = 7*MA02*MHp2 - 2*(MA02**2 + MHp2**2)

        NSHhhx(5) = 52*MA02**4 + 166*MA02**3*MHp2 -
     &    495*MA02**2*MHp2**2 + 212*MA02*MHp2**3 - 25*MHp2**4

        NSHhhx(8) = 12*MA02**4 - 39*MA02**2*MHp2**2 +
     &    9*MA02*MHp2**3 - MHp2**4 + 2*MHp2*NSHhhx(7)

        NSHhhx(12) = NSHhhl(2)*NSHhhx(5) +
     &    2*NSHhhl(4)*NSHhhx(8) - 2*NSHhhx(11)

	NSHhhx(17) = MA02**2 - MHp2**2 + 2*MA02*NSHhhl(4)

        NSHhhx(18) = 5*MHp2 +
     &    (2*MA02 - MHp2)*(Re(NSHhhl(1)) + 9*Re(NSHhhl(3)))

	NSHhhx(4) = -59*MHp2**2 + (3*NSHhhl(4)**2*NSHhhx(3))/MA02**2

        NSHhhx(1) = -121 - (412*NSHhhl(2))/MHp2 +
     &    (40*NSHhhl(2)**2)/MHp2**2

        NSHhhx(13) = 96*MA02**5 + 416*MA02**4*MHp2 -
     &    1575*MA02**2*MHp2**3 + 670*MA02*MHp2**4 - 79*MHp2**5 -
     &    8*MA02**3*NSHhhx(4) + 4*NSHhhx(12)

        NSHhhx(16) = Re(NSHhhl(1)) +
     &    (NSHhhl(2)*(10 - Re(NSHhhl(1)) - 9*Re(NSHhhl(3))))/
     &     MHp2 + 9*Re(NSHhhl(3))

        NSHhhx(19) = -2*NSHhhl(2)*NSHhhx(18) +
     &    NSHhhx(17)*(5 + Re(NSHhhl(1)) + 9*Re(NSHhhl(3)))

	NSHhhx(2) = 4*MA02**2 - 5*MA02*MHp2 + MHp2**2

        NSHhhx(14) = -136*NSHhhl(5) - 3*MHp2*NSHhhx(1) +
     &    (9*NSHhhx(13))/NSHhhx(2)**2

	NSHhhx(15) = -2*MHp2 + (MW2*SW2)/(Alfa*Pi)*Lambda5

	NSHhhx(20) = 2*NSHhhx(16) + NSHhhx(19)/(MA02 - MHp2)**2

        NSHHHeqHp = Alfa2/(MW2**2*Pi**2*SW2**2)*
     &    ((1/TB - TB)**2*(1/147456.D0*NSHhhx(14)/MHp2 -
     &        1/4096.D0*NSHhhx(20))*NSHhhx(15)**2)

	end
