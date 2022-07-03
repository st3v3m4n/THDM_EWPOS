c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich



* NSHindMasses.F
* generated 11-Dec-2015 12:23

C> @brief This subroutine calculates \f$\delta \rho^{(2)}_\mathrm{H,NS}\f$ with independent
C> masses of the non-standard scalars.
C>
C> @ingroup group_rho
	subroutine calcNSHindMasses
	implicit none


#include "NSHindMasses.h"


	RealType T134sub

	external T134sub

	ComplexType A0FH, B0FH

	external A0FH, B0FH

#include "paraTHDM.h"
#include "paraSM.h"
	NSHil(1) = A0FH(MHp2)

	NSHil(10) = T134sub(MHH2,MHH2,MA02)

	NSHil(11) = T134sub(MHH2,MHH2,MHH2)

	NSHil(12) = T134sub(MHp2,MA02,MA02)

	NSHil(13) = T134sub(MHp2,MHH2,MA02)

	NSHil(14) = T134sub(MHp2,MHp2,MA02)

	NSHil(15) = T134sub(MHp2,MHp2,MHH2)

	NSHil(16) = T134sub(MHp2,MHH2,MHH2)

	NSHil(17) = T134sub(MHp2,MHp2,MHp2)

	NSHil(2) = B0FH(MHp2,MHH2,MHp2)

	NSHil(3) = A0FH(MA02)

	NSHil(4) = B0FH(MA02,MA02,MHH2)

	NSHil(5) = B0FH(MHH2,MA02,MA02)

	NSHil(6) = B0FH(MHH2,MHH2,MHH2)

	NSHil(7) = B0FH(MHH2,MHp2,MHp2)

	NSHil(8) = A0FH(MHH2)

	NSHil(9) = T134sub(MHH2,MA02,MA02)

	NSHix(26) = 2*MHH2**2 - 3*MHH2*MHp2 + 7*MHp2**2

	NSHix(16) = 2*MHH2**2 - 9*MHH2*MHp2 + 4*MHp2**2

        NSHix(17) = 11*MHH2**3 - 40*MHH2**2*MHp2 +
     &    74*MHH2*MHp2**2 - 36*MHp2**3

        NSHix(18) = MHH2**3 + 14*MHH2**2*MHp2 +
     &    44*MHH2*MHp2**2 - 32*MHp2**3

        NSHix(19) = 7*MHH2**3 - 22*MHH2**2*MHp2 +
     &    34*MHH2*MHp2**2 - 16*MHp2**3

        NSHix(27) = 39*MHH2**2 - 150*MHH2*MHp2 + 84*MHp2**2 +
     &    (4*NSHil(1)*NSHix(26))/MHp2

	NSHix(3) = MHH2**2 + 8*MHH2*MHp2 - 6*MHp2**2

	NSHix(37) = MHH2**2 - MHH2*MHp2 - 12*MHp2**2

        NSHix(38) = 33*MHH2**3 - 17*MHH2**2*MHp2 -
     &    44*MHH2*MHp2**2 + 64*MHp2**3

        NSHix(39) = 77*MHH2**3 - 61*MHH2**2*MHp2 -
     &    188*MHH2*MHp2**2 + 256*MHp2**3

	NSHix(41) = MA02**2 + MA02*(MHH2 - 3*MHp2)

	NSHix(59) = 5 + Re(NSHil(5)) + 9*Re(NSHil(6)) + 2*Re(NSHil(7))

	NSHix(7) = MHH2**2 - 15*MHH2*MHp2 - 4*MHp2**2

	NSHix(8) = 2*MHH2**2 - 11*MHH2*MHp2 - MHp2**2

	NSHix(1) = MHH2**2 - 18*MHH2*MHp2 + 14*MHp2**2

        NSHix(12) = 16*MHH2**3 + MA02**2*(MHH2 - 4*MHp2) -
     &    47*MHH2**2*MHp2 + 44*MHH2*MHp2**2 - 16*MHp2**3

        NSHix(13) = 8*MHH2**3 - 23*MHH2**2*MHp2 +
     &    21*MHH2*MHp2**2 - 12*MHp2**3

        NSHix(14) = 6*MA02**2 + MHH2**2 - 2*MHH2*MHp2 +
     &    4*MHp2**2 - 3*MA02*(MHH2 + 2*MHp2)

        NSHix(2) = MHH2**3 - 6*MHH2**2*MHp2 +
     &    12*MHH2*MHp2**2 - 4*MHp2**3

        NSHix(20) = 4*MA02**3*NSHix(16) -
     &    2*MA02*MHH2*NSHix(17) + MA02**2*NSHix(18) +
     &    MHH2**2*NSHix(19)

        NSHix(22) = 8*MHH2**3 - 72*MHH2**2*MHp2 +
     &    129*MHH2*MHp2**2 - 92*MHp2**3

	NSHix(23) = 2*MHH2**2 - 2*MHH2*MHp2 + 3*MHp2**2

	NSHix(24) = MHH2**2 - 6*MHH2*MHp2 + 8*MHp2**2

	NSHix(25) = 3*MHH2**2 + 4*MHH2*MHp2 - 10*MHp2**2

	NSHix(28) = -63*MHp2**2 + (4*NSHil(8)*NSHix(27))/MHH2

        NSHix(29) = -153*MHp2 +
     &    (8*(MHH2 - 4*MHp2)*NSHil(8)**2)/MHH2**2

	NSHix(32) = 9*MHH2**2 - 22*MHH2*MHp2 + 22*MHp2**2

	NSHix(33) = MHH2**3 - 4*MHH2*MHp2**2 + 4*MHp2**3

        NSHix(34) = 3*MHH2**3 - 2*MHH2**2*MHp2 -
     &    6*MHH2*MHp2**2 + 8*MHp2**3

	NSHix(4) = MA02**3*(MHH2 - MHp2) + MA02*MHp2*NSHix(3)

        NSHix(40) = 32*MA02**3*(MHH2 - 4*MHp2) -
     &    4*MA02**2*NSHix(37) - MHH2*NSHix(38) + MA02*NSHix(39)

	NSHix(42) = MHH2*(MHH2 - 3*MHp2) - 2*NSHix(41)

	NSHix(44) = 8*MA02**2 - 6*MA02*MHH2 + MHH2**2

	NSHix(45) = 37*MA02 + (2*(4*MA02 - MHH2)*NSHil(8)**2)/MHH2**2

	NSHix(46) = 18*MA02**2 - 31*MA02*MHH2 + 7*MHH2**2

        NSHix(47) = 8*MA02**2 + MHH2**2 -
     &    3*MA02*MHH2*(1 - (2*NSHil(8))/MHH2)

	NSHix(48) = 10*MA02**2 - 8*MA02*MHH2 + MHH2**2

        NSHix(57) = -5 + 2*Re(NSHil(4)) - Re(NSHil(5)) -
     &    9*Re(NSHil(6)) - 2*Re(NSHil(7))

        NSHix(58) = MHH2*(5 - 2*Re(NSHil(4))) +
     &    (2*MA02 - MHH2)*(Re(NSHil(5)) + 9*Re(NSHil(6)) +
     &       2*Re(NSHil(7)))

	NSHix(60) = MA02*NSHix(59) + 2*(MA02 - 2*MHH2)*Re(NSHil(4))

        NSHix(62) = -5 + 2*Re(NSHil(2)) - Re(NSHil(5)) -
     &    9*Re(NSHil(6)) - 2*Re(NSHil(7))

        NSHix(63) = MHH2*(5 - 2*Re(NSHil(2))) +
     &    (MHH2 - 2*MHp2)*(-Re(NSHil(5)) - 9*Re(NSHil(6)) -
     &       2*Re(NSHil(7)))

	NSHix(64) = MHp2*NSHix(59) - 2*(2*MHH2 - MHp2)*Re(NSHil(2))

        NSHix(9) = 4*MA02**4 + 2*MHH2**3*MHp2 -
     &    2*MA02**3*(MHH2 + 6*MHp2) - MA02**2*NSHix(7) +
     &    MA02*MHH2*NSHix(8)

        NSHix(10) = 2*(MA02 - MHH2)*NSHil(8) -
     &    NSHix(9)/(MA02 - MHH2)**2

        NSHix(15) = (NSHil(1)*
     &       (MHp2*NSHix(12) - MA02*NSHix(13)))/
     &     ((MA02 - MHp2)*(MHH2 - MHp2)**2) +
     &    (2*NSHil(1)**2*NSHix(14))/(MA02 - MHp2)**2

        NSHix(21) = (4*(3*MA02 - MHH2 - 2*MHp2)*NSHil(1))/
     &     ((MA02 - MHH2)*(MA02 - MHp2)) +
     &    NSHix(20)/
     &     ((MA02 - MHH2)**2*(4*MA02 - MHH2)*(MHH2 - MHp2)**2)

        NSHix(30) = 117*MHH2**3 + 252*MHp2**3 +
     &    (MHH2*MHp2 - 4*MHp2**2)*(-32*NSHil(16) - 8*NSHil(17)) +
     &    (-4*NSHil(1)*NSHix(22) - 16*NSHil(1)**2*NSHix(23))/
     &     MHp2 - 48*NSHil(11)*NSHix(24) - 8*NSHil(15)*NSHix(25) +
     &    MHH2*NSHix(28) + 2*MHH2**2*NSHix(29)

        NSHix(35) = 2*MA02**3*(MHH2 - 3*MHp2) +
     &    MA02**2*NSHix(32) + MHH2*NSHix(33) - 2*MA02*NSHix(34)

        NSHix(43) = -(NSHix(40)/
     &       ((MA02 - MHH2)*(MHH2 - 4*MHp2))) +
     &    (8*NSHil(3)**2*NSHix(42))/(MA02 - MHp2)**2

        NSHix(49) = 36*MA02**3 + 19*(MA02**2*MHH2 + MHH2**3) -
     &    4*(2*MA02 + MHH2)*NSHil(3)**2 +
     &    8*(4*MA02**2 - MA02*MHH2)*NSHil(10) -
     &    12*NSHil(11)*NSHix(44) - 2*MHH2**2*NSHix(45) +
     &    4*(NSHil(8)*NSHix(46) + NSHil(3)*NSHix(47) +
     &       NSHil(9)*NSHix(48))

	NSHix(5) = MA02**2*NSHix(1) - MHH2*NSHix(2) + 2*NSHix(4)

	NSHix(55) = MHp2*NSHil(1) + (MA02 - 2*MHp2)*NSHil(3)

        NSHix(61) = -((MA02 - MHH2)*(MA02 + MHH2)*NSHix(57)) -
     &    2*NSHil(8)*NSHix(58) + 2*NSHil(3)*NSHix(60)

        NSHix(65) = (MHH2 - MHp2)*(MHH2 + MHp2)*NSHix(62) -
     &    2*NSHil(8)*NSHix(63) + 2*NSHil(1)*NSHix(64)

        NSHix(11) = (2*NSHil(1))/(MA02 - MHp2)**2 +
     &    NSHix(10)/((4*MA02 - MHH2)*(MA02 - MHp2)*(MHH2 - MHp2))

        NSHix(31) = (-4*NSHix(15))/(MA02 - MHH2) +
     &    4*NSHil(8)*NSHix(21) + NSHix(30)/(MHH2 - MHp2)**2

        NSHix(36) = MHp2*NSHil(12) +
     &    (NSHil(9)*NSHix(35))/((4*MA02 - MHH2)*(MA02 - MHp2)**2)

        NSHix(50) = NSHix(43)/(MHH2 - MHp2) -
     &    (4*NSHix(49))/(MA02 - MHH2)**2

	NSHix(53) = (NSHil(1)*(1 - Re(NSHil(2))))/MHp2 + Re(NSHil(2))

        NSHix(54) = MA02**2 - MHp2**2 -
     &    2*(2*MA02 - MHp2)*NSHil(1) + 2*MA02*NSHil(3)

	NSHix(56) = MA02**2 - MHp2**2 - 2*NSHix(55)

        NSHix(6) = 2*MA02*NSHil(14) -
     &    (NSHil(15)*NSHix(5))/((MHH2 - 4*MHp2)*(MA02 - MHp2)**2)

        NSHix(66) = NSHix(61)/(MA02 - MHH2)**2 -
     &    NSHix(65)/(MHH2 - MHp2)**2

        NSHix(51) = (16*MHH2*NSHil(13))/(MA02 - MHp2)**2 -
     &    (8*NSHix(6))/(MA02 - MHH2)**2 -
     &    NSHix(31)/(MHH2 - 4*MHp2) +
     &    8*(NSHil(3)*NSHix(11) + NSHix(36)/(MHH2 - MHp2)**2) +
     &    NSHix(50)/(4*MA02 - MHH2)

	NSHix(52) = -2*MHH2 + (MW2*SW2)/(Alfa*Pi)*Lambda5

        NSHix(67) = 8*NSHix(53) - NSHix(66) +
     &    (2*(NSHix(54)*Re(NSHil(2)) - NSHix(56)*Re(NSHil(4))))/
     &     (MA02 - MHp2)**2

        NSHindMasses =
     &   -(Alfa2/(MW2**2*Pi**2*SW2**2)*
     &      ((1/TB - TB)**2*
     &        (1/16384.D0*NSHix(51) - 1/4096.D0*NSHix(67))*NSHix(52)**2
     &        ))

	end
