c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich


* NSHHHeq2A0.F
* generated 15-Dec-2015 13:34

	subroutine calcNSHHHeq2A0
	implicit none


#include "NSHHHeq2A0.h"


	RealType T134sub

	external T134sub

	ComplexType A0FH, B0FH

	external A0FH, B0FH

#include "paraTHDM.h"
#include "paraSM.h"
	NSHh2al(1) = A0FH(4*MA02)

	NSHh2al(10) = T134sub(4*MA02,4*MA02,4*MA02)

	NSHh2al(11) = T134sub(MHp2,MA02,MA02)

	NSHh2al(12) = T134sub(MHp2,4*MA02,MA02)

	NSHh2al(13) = T134sub(MHp2,MHp2,MA02)

	NSHh2al(14) = T134sub(MHp2,MHp2,4*MA02)

	NSHh2al(15) = T134sub(MHp2,4*MA02,4*MA02)

	NSHh2al(16) = T134sub(MHp2,MHp2,MHp2)

	NSHh2al(2) = B0FH(MA02,MA02,4*MA02)

	NSHh2al(3) = B0FH(4*MA02,MA02,MA02)

	NSHh2al(4) = B0FH(4*MA02,4*MA02,4*MA02)

	NSHh2al(5) = B0FH(4*MA02,MHp2,MHp2)

	NSHh2al(6) = A0FH(MA02)

	NSHh2al(7) = A0FH(MHp2)

	NSHh2al(8) = B0FH(MHp2,4*MA02,MHp2)

	NSHh2al(9) = T134sub(4*MA02,4*MA02,MA02)

        NSHh2ax(18) = 14*MA02**3 + 13*MA02**2*MHp2 -
     &    11*MA02*MHp2**2 + 2*MHp2**3

        NSHh2ax(14) = 128*MA02**4 - 349*MA02**3*MHp2 +
     &    210*MA02**2*MHp2**2 - 47*MA02*MHp2**3 + 4*MHp2**4

        NSHh2ax(19) = 22*MHp2**3 -
     &    9*(4*MA02 - MHp2)**2*NSHh2al(7) +
     &    (9*NSHh2al(1)*NSHh2ax(18))/MA02

        NSHh2ax(21) = 1352*MA02**3 +
     &    (3*(4*MA02 - MHp2)**2*(MA02 + 2*MHp2)*NSHh2al(7))/MHp2

	NSHh2ax(15) = 1550*MA02**4 - (NSHh2al(7)*NSHh2ax(14))/MHp2

        NSHh2ax(16) = -((5*MA02 - 2*MHp2)*NSHh2al(7)**2) +
     &    48*MA02**2*NSHh2al(12)

        NSHh2ax(20) = 1024*MA02**4 - 274*MA02**3*MHp2 -
     &    147*MA02**2*MHp2**2 + MHp2**4 + 2*MA02*NSHh2ax(19)

        NSHh2ax(22) = 76*MA02**4 - 894*MA02**2*MHp2**2 +
     &    209*MA02*MHp2**3 - 14*MHp2**4 + MHp2*NSHh2ax(21)

	NSHh2ax(3) = 24*MA02**2 + 8*MA02*MHp2 - 5*MHp2**2

        NSHh2ax(4) = 128*MA02**3 - 288*MA02**2*MHp2 +
     &    129*MA02*MHp2**2 - 23*MHp2**3

	NSHh2ax(5) = 32*MA02**2 - 8*MA02*MHp2 + 3*MHp2**2

	NSHh2ax(6) = 52*MA02**2 - 50*MA02*MHp2 + 7*MHp2**2

	NSHh2ax(7) = 32*MA02**2 - 12*MA02*MHp2 + 7*MHp2**2

	NSHh2ax(11) = 116*MA02**2 - 55*MA02*MHp2 + 2*MHp2**2

        NSHh2ax(13) = 24*MA02**3 + 19*MA02**2*MHp2 -
     &    20*MA02*MHp2**2 + 4*MHp2**3

        NSHh2ax(17) = 3024*MA02**5 - 3887*MA02**3*MHp2**2 +
     &    390*MA02**2*MHp2**3 + 121*MA02*MHp2**4 - 16*MHp2**5 +
     &    4*MHp2*NSHh2ax(15) - 4*(4*MA02 - MHp2)**2*NSHh2ax(16)

        NSHh2ax(23) = (2*NSHh2al(6)*NSHh2ax(20) +
     &      NSHh2al(1)*NSHh2ax(22))/MA02

        NSHh2ax(39) = 5 + Re(NSHh2al(3)) + 9*Re(NSHh2al(4)) +
     &    2*Re(NSHh2al(5))

        NSHh2ax(8) = NSHh2al(14)*NSHh2ax(3) +
     &    (NSHh2al(7)*NSHh2ax(4) + NSHh2al(7)**2*NSHh2ax(5))/
     &     MHp2 - NSHh2al(1)*
     &     (3*NSHh2ax(6) + (NSHh2al(7)*NSHh2ax(7))/MHp2)

	NSHh2ax(1) = 52*MA02**2 - 21*MA02*MHp2 - 7*MHp2**2

        NSHh2ax(12) = 4*NSHh2al(13) +
     &    (NSHh2al(14)*NSHh2ax(11))/(MA02 - MHp2)**2

	NSHh2ax(2) = 4*MA02**2 - 5*MA02*MHp2 + MHp2**2

        NSHh2ax(24) = -72*NSHh2al(6)**2*NSHh2ax(13) +
     &    3*NSHh2ax(17) + 4*MA02*NSHh2ax(23)

        NSHh2ax(29) = -10 + 4*Re(NSHh2al(2)) +
     &    Re(NSHh2al(3)) + 9*Re(NSHh2al(4)) + 2*Re(NSHh2al(5))

        NSHh2ax(30) = -5 + 14*Re(NSHh2al(2)) -
     &    Re(NSHh2al(3)) - 9*Re(NSHh2al(4)) - 2*Re(NSHh2al(5))

        NSHh2ax(31) = -5 + 2*Re(NSHh2al(2)) - Re(NSHh2al(3)) -
     &    9*Re(NSHh2al(4)) - 2*Re(NSHh2al(5))

	NSHh2ax(34) = (MA02 - 2*MHp2)*NSHh2al(6) + MHp2*NSHh2al(7)

        NSHh2ax(37) = 5 + Re(NSHh2al(3)) + 9*Re(NSHh2al(4)) +
     &    2*Re(NSHh2al(5)) - 2*Re(NSHh2al(8))

        NSHh2ax(38) = (2*MA02 - MHp2)*
     &     (Re(NSHh2al(3)) + 9*Re(NSHh2al(4)) + 2*Re(NSHh2al(5)))-
     &      2*MA02*(5 - 2*Re(NSHh2al(8)))

        NSHh2ax(40) = MHp2*NSHh2ax(39) -
     &    2*(8*MA02 - MHp2)*Re(NSHh2al(8))

        NSHh2ax(9) = -4*NSHh2al(1)**2 +
     &    24*(2*MA02 - MHp2)*NSHh2al(10) +
     &    2*MHp2*(4*NSHh2al(15) + NSHh2al(16)) +
     &    NSHh2ax(8)/(MA02 - MHp2)

        NSHh2ax(10) = (9*NSHh2ax(1))/NSHh2ax(2) -
     &    (4*NSHh2ax(9))/(4*MA02 - MHp2)**2

        NSHh2ax(25) = (72*MHp2*NSHh2al(11))/
     &     (4*MA02 - MHp2)**2 -
     &    (4*NSHh2ax(12) + NSHh2ax(24)/NSHh2ax(2)**2)/MA02

        NSHh2ax(26) = 315 +
     &    (112*NSHh2al(1) - 28*NSHh2al(6))/MA02 +
     &    (4*NSHh2al(1)**2 + 8*NSHh2al(6)**2)/MA02**2

        NSHh2ax(32) = (4*NSHh2al(1)*NSHh2ax(29) -
     &       2*NSHh2al(6)*NSHh2ax(30))/MA02 + 15*NSHh2ax(31)

        NSHh2ax(33) = (NSHh2al(7)*(1 - Re(NSHh2al(8))))/MHp2 +
     &    Re(NSHh2al(8))

	NSHh2ax(35) = MA02**2 - MHp2**2 - 2*NSHh2ax(34)

        NSHh2ax(36) = MA02**2 - MHp2**2 + 2*MA02*NSHh2al(6) -
     &    2*(2*MA02 - MHp2)*NSHh2al(7)

        NSHh2ax(41) = ((4*MA02 + MHp2)*NSHh2ax(37))/
     &     (4*MA02 - MHp2) -
     &    (2*(2*NSHh2al(1)*NSHh2ax(38) + NSHh2al(7)*NSHh2ax(40)))/
     &     (4*MA02 - MHp2)**2

        NSHh2ax(27) = -9*NSHh2ax(10) + NSHh2ax(25) -
     &    (4*(8*NSHh2al(9) + 24*NSHh2al(10) - MA02*NSHh2ax(26)))/
     &     MA02

	NSHh2ax(28) = -8*MA02 + (MW2*SW2)/(Alfa*Pi)*Lambda5

        NSHh2ax(42) = NSHh2ax(32) - 72*NSHh2ax(33) +
     &    9*NSHh2ax(41) + (18*
     &       (NSHh2ax(35)*Re(NSHh2al(2)) -
     &         NSHh2ax(36)*Re(NSHh2al(8))))/(MA02 - MHp2)**2

        NSHHHeq2A0 = -(Alfa2/(MW2**2*Pi**2*SW2**2)*
     &      ((1/TB - TB)**2*
     &        (1/147456.D0*NSHh2ax(27) + 1/36864.D0*NSHh2ax(42))*
     &        NSHh2ax(28)**2))

	end
