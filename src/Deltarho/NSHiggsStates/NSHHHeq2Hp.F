c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich


* NSHHHeq2Hp.F
* generated 15-Dec-2015 13:47

	subroutine calcNSHHHeq2Hp
	implicit none


#include "NSHHHeq2Hp.h"


	RealType T134sub

	external T134sub

	ComplexType A0FH, B0FH

	external A0FH, B0FH

#include "paraTHDM.h"
#include "paraSM.h"
	NSHh2hl(1) = A0FH(MHp2)

	NSHh2hl(10) = T134sub(MHp2,MHp2,MA02)

	NSHh2hl(11) = T134sub(4*MHp2,MA02,MA02)

	NSHh2hl(12) = T134sub(4*MHp2,MHp2,MA02)

	NSHh2hl(13) = T134sub(MHp2,MHp2,MHp2)

	NSHh2hl(14) = T134sub(4*MHp2,4*MHp2,MHp2)

	NSHh2hl(15) = T134sub(4*MHp2,4*MHp2,4*MHp2)

	NSHh2hl(16) = T134sub(4*MHp2,4*MHp2,MA02)

	NSHh2hl(2) = B0FH(MHp2,MHp2,4*MHp2)

	NSHh2hl(3) = A0FH(MA02)

	NSHh2hl(4) = B0FH(MA02,MA02,4*MHp2)

	NSHh2hl(5) = A0FH(4*MHp2)

	NSHh2hl(6) = B0FH(4*MHp2,MA02,MA02)

	NSHh2hl(7) = B0FH(4*MHp2,MHp2,MHp2)

	NSHh2hl(8) = B0FH(4*MHp2,4*MHp2,4*MHp2)

	NSHh2hl(9) = T134sub(MHp2,MA02,MA02)

        NSHh2hx(17) = MA02**3 - MA02**2*MHp2 +
     &    2*MA02*MHp2**2 - 20*MHp2**3

	NSHh2hx(18) = 103*MA02**3 + (18*NSHh2hl(5)*NSHh2hx(17))/MHp2

        NSHh2hx(10) = 8*MA02**5 - 47*MA02**4*MHp2 +
     &    102*MA02**3*MHp2**2 - 1823*MA02**2*MHp2**3 +
     &    2840*MA02*MHp2**4 + 4752*MHp2**5

	NSHh2hx(11) = MA02**2 + 40*MA02*MHp2 - 104*MHp2**2

        NSHh2hx(13) = MA02**4 - 5*MA02**3*MHp2 +
     &    12*MA02**2*MHp2**2 - 13*MA02*MHp2**3 + 32*MHp2**4

        NSHh2hx(15) = 5*MA02**3 - 22*MA02**2*MHp2 +
     &    38*MA02*MHp2**2 - 48*MHp2**3

        NSHh2hx(16) = 7*MA02**4 - 10*MA02**3*MHp2 -
     &    444*MA02**2*MHp2**2 + 944*MA02*MHp2**3 + 232*MHp2**4

        NSHh2hx(19) = MA02**4 - 1119*MA02**2*MHp2**2 +
     &    2480*MA02*MHp2**3 - 2864*MHp2**4 + 2*MHp2*NSHh2hx(18)

        NSHh2hx(12) = -12*(MA02 + 2*MHp2)*NSHh2hl(3)**2 -
     &    (3*NSHh2hx(10))/(MA02 - 4*MHp2)**2 +
     &    4*NSHh2hl(11)*NSHh2hx(11)

        NSHh2hx(14) = -6*NSHh2hl(1) -
     &    (1/2.D0*((MA02 - 4*MHp2)*NSHh2hl(5)) -
     &       NSHh2hx(13)/(MA02 - 4*MHp2)**2)/MHp2

        NSHh2hx(20) = (18*NSHh2hl(1)**2*NSHh2hx(15) +
     &      NSHh2hl(5)*NSHh2hx(16) - NSHh2hl(1)*NSHh2hx(19))/MHp2

        NSHh2hx(3) = -19*MHp2 +
     &    (2*(MA02 + 2*MHp2)*NSHh2hl(3)**2)/MA02**2

        NSHh2hx(34) = Re(NSHh2hl(6)) + 2*Re(NSHh2hl(7)) +
     &    9*Re(NSHh2hl(8))

        NSHh2hx(36) = 5 + Re(NSHh2hl(6)) + 2*Re(NSHh2hl(7)) +
     &    9*Re(NSHh2hl(8))

	NSHh2hx(4) = 9*MA02**2 - 62*MA02*MHp2 + 56*MHp2**2

	NSHh2hx(6) = 5*MA02**2 - 16*MA02*MHp2 + 8*MHp2**2

        NSHh2hx(7) = 4*MA02**2 + 8*MHp2**2 -
     &    3*MA02*MHp2*(2 - NSHh2hl(5)/MHp2)

        NSHh2hx(1) = 1329 +
     &    (216*NSHh2hl(1)**2 + 16*NSHh2hl(5)**2)/MHp2**2 +
     &    (640*NSHh2hl(5) -
     &       4*NSHh2hl(1)*(265 + (4*NSHh2hl(5))/MHp2))/MHp2

        NSHh2hx(21) = 576*MHp2*NSHh2hl(12) +
     &    NSHh2hx(12)/MHp2 - 24*NSHh2hl(3)*NSHh2hx(14) -
     &    (4*NSHh2hx(20))/(MA02 - 4*MHp2)**2

	NSHh2hx(27) = MHp2*NSHh2hl(1) + (MA02 - 2*MHp2)*NSHh2hl(3)

        NSHh2hx(29) = -10 + 4*Re(NSHh2hl(2)) +
     &    Re(NSHh2hl(6)) + 2*Re(NSHh2hl(7)) + 9*Re(NSHh2hl(8))

        NSHh2hx(30) = -5 + 14*Re(NSHh2hl(2)) -
     &    Re(NSHh2hl(6)) - 2*Re(NSHh2hl(7)) - 9*Re(NSHh2hl(8))

        NSHh2hx(31) = -5 + 2*Re(NSHh2hl(2)) - Re(NSHh2hl(6)) -
     &    2*Re(NSHh2hl(7)) - 9*Re(NSHh2hl(8))

        NSHh2hx(33) = -5 + 2*Re(NSHh2hl(4)) - Re(NSHh2hl(6)) -
     &    2*Re(NSHh2hl(7)) - 9*Re(NSHh2hl(8))

        NSHh2hx(35) = -((MA02 - 2*MHp2)*NSHh2hx(34)) -
     &    2*MHp2*(5 - 2*Re(NSHh2hl(4)))

        NSHh2hx(37) = MA02*NSHh2hx(36) +
     &    2*(MA02 - 8*MHp2)*Re(NSHh2hl(4))

        NSHh2hx(5) = 9*MA02**3 - 296*MA02*MHp2**2 +
     &    304*MHp2**3 - MA02**2*NSHh2hx(3) +
     &    2*NSHh2hl(5)*NSHh2hx(4)

        NSHh2hx(8) = -2*NSHh2hl(5)**2 -
     &    12*(MA02 - 2*MHp2)*NSHh2hl(15) + 4*MA02*NSHh2hl(16) +
     &    (NSHh2hl(11)*NSHh2hx(6) + NSHh2hl(3)*NSHh2hx(7))/
     &     (MA02 - MHp2)

        NSHh2hx(2) = -8*NSHh2hl(13) - 32*NSHh2hl(14) -
     &    96*NSHh2hl(15) + MHp2*NSHh2hx(1)

        NSHh2hx(22) = (-8*NSHh2hl(9))/MHp2 +
     &    (144*MA02*NSHh2hl(10))/(MA02 - 4*MHp2)**2 -
     &    NSHh2hx(21)/(MA02 - MHp2)**2

        NSHh2hx(25) = (NSHh2hl(1)*(1 - Re(NSHh2hl(2))))/MHp2 +
     &    Re(NSHh2hl(2))

        NSHh2hx(26) = MA02**2 - MHp2**2 -
     &    2*(2*MA02 - MHp2)*NSHh2hl(1) + 2*MA02*NSHh2hl(3)

	NSHh2hx(28) = MA02**2 - MHp2**2 - 2*NSHh2hx(27)

        NSHh2hx(32) = -((4*NSHh2hl(5)*NSHh2hx(29) -
     &         2*NSHh2hl(1)*NSHh2hx(30))/MHp2) - 15*NSHh2hx(31)

        NSHh2hx(38) = (MA02 - 4*MHp2)*(MA02 + 4*MHp2)*
     &     NSHh2hx(33) - 2*
     &     (2*NSHh2hl(5)*NSHh2hx(35) + NSHh2hl(3)*NSHh2hx(37))

	NSHh2hx(9) = NSHh2hx(5)/(MA02 - MHp2) + 2*NSHh2hx(8)

        NSHh2hx(23) = NSHh2hx(2)/MHp2 +
     &    (36*NSHh2hx(9))/(MA02 - 4*MHp2)**2 + NSHh2hx(22)

	NSHh2hx(24) = -8*MHp2 + (MW2*SW2)/(Alfa*Pi)*Lambda5

        NSHh2hx(39) = 72*NSHh2hx(25) - NSHh2hx(32) +
     &    (9*NSHh2hx(38))/(MA02 - 4*MHp2)**2 +
     &    (18*(NSHh2hx(26)*Re(NSHh2hl(2)) -
     &         NSHh2hx(28)*Re(NSHh2hl(4))))/(MA02 - MHp2)**2

        NSHHHeq2Hp = Alfa2/(MW2**2*Pi**2*SW2**2)*
     &    ((1/TB - TB)**2*(1/147456.D0*NSHh2hx(23) +
     &        1/36864.D0*NSHh2hx(39))*NSHh2hx(24)**2)

	end
