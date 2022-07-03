c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich


* NSatSA0eqHp.F
* generated 22-Dec-2016 15:39

C> @brief This subroutine calculates \f$\delta\rho^{\alpha_t\lambda_i}_\mathrm{NS}\f$ with \f$m_{A^0}=m_{H^\pm}\f$.
C>
C> @ingroup group_rho
	subroutine calcNSatSA0eqHp
	implicit none


#include "NSatSA0eqHp.h"


	RealType T134sub

	external T134sub

	ComplexType A0FH, B0FH

	external A0FH, B0FH

#include "paraTHDM.h"
#include "paraSM.h"
	NSatSapl(1) = A0FH(MHH2)

	NSatSapl(2) = A0FH(MHp2)

	NSatSapl(3) = A0FH(MT2)

	NSatSapl(4) = B0FH(MHp2,0.D0,MT2)

	NSatSapl(5) = B0FH(MHp2,MT2,MT2)

	NSatSapl(6) = T134sub(MT2,MHH2,0.D0)

	NSatSapl(7) = T134sub(MT2,MHp2,0.D0)

	NSatSapl(8) = T134sub(MT2,MT2,MHH2)

	NSatSapl(9) = T134sub(MT2,MT2,MHp2)

        NSatSapx(14) =
     &   2*MHp2**3 + 160*MT2**3 + 52*MT2**2*MHp2 - 13*MT2*MHp2**2

        NSatSapx(4) = MHp2**3 - 80*MT2**3 - 20*MT2**2*MHp2 +
     &    15*MT2*MHp2**2

	NSatSapx(48) = 14*MHH2**2 - MHp2**2

	NSatSapx(49) = MHp2**2 + 14*MT2**2 - 4*MT2*MHp2

	NSatSapx(51) = (MHH2 - MHp2)*MHp2 - MT2*(2*MHH2 - 7*MHp2)

	NSatSapx(60) = MHp2**2 + 18*MT2**2 - 16*MT2*MHp2

        NSatSapx(61) =
     &   -(MT2**2*(17*MHp2 - 64*MT2)) + 6*MT2*(MHH2*(MHp2 - 5*MT2))

	NSatSapx(63) = MHp2**2 - 2*MT2**2 - 4*MT2*MHp2

	NSatSapx(64) = MHp2**2 + 24*MT2**2 - 6*MT2*MHp2

        NSatSapx(11) =
     &   11*MHp2**3 - 32*MT2**3 + 72*MT2**2*MHp2 - 66*MT2*MHp2**2

        NSatSapx(12) =
     &   2*MHp2**4 + 64*MT2**4 + 136*MT2**3*MHp2 +
     &    210*MT2**2*MHp2**2 - 49*MT2*MHp2**3

        NSatSapx(13) =
     &   MHp2**4 + 128*MT2**4 + 96*MT2**3*MHp2 +
     &    264*MT2**2*MHp2**2 - 63*MT2*MHp2**3

        NSatSapx(15) =
     &   5*MHH2**4*(MHp2 - 4*MT2) + 16*MT2**2*NSatSapx(14)

	NSatSapx(17) = MHp2**2 + 2*MT2**2 - 4*MT2*MHp2

	NSatSapx(18) = 21*MHp2**2 + 160*MT2**2 - 100*MT2*MHp2

        NSatSapx(19) =
     &   MHp2**3 + 16*MT2**3 - 26*MT2**2*MHp2 + 4*MT2*MHp2**2

        NSatSapx(2) = 15*MHp2**3 - 32*MT2**3 +
     &    12*MT2**2*MHp2 - 43*MT2*MHp2**2

        NSatSapx(20) =
     &   9*MHp2**3 + 32*MT2**3 - 4*MT2**2*MHp2 - 23*MT2*MHp2**2

	NSatSapx(22) = 11*MHp2**2 - 32*MT2**2 - 16*MT2*MHp2

        NSatSapx(23) =
     &   3*MHp2**3 - 64*MT2**3 - 184*MT2**2*MHp2 + 66*MT2*MHp2**2

        NSatSapx(3) = 2*MHp2**4 - 32*MT2**4 -
     &    136*MT2**3*MHp2 - 118*MT2**2*MHp2**2 + 53*MT2*MHp2**3

	NSatSapx(32) = MHp2**2 - 4*MT2**2 + 6*MT2*MHp2

	NSatSapx(33) = 5*MHp2**2 - 3*MT2**2 + MT2*MHp2

	NSatSapx(34) = MHH2**3 - MT2**2*(3*MHp2 - 2*MT2)

	NSatSapx(36) = 3*MHp2**2 - 3*MT2**2 - 2*MT2*MHp2

	NSatSapx(39) = MHp2**2 + 6*MT2**2 - 6*MT2*MHp2

	NSatSapx(40) = 7*MHp2**2 + 16*MT2**2 - 40*MT2*MHp2

	NSatSapx(43) = 4*MHH2**2 - 6*MHH2*MHp2 + MHp2**2

	NSatSapx(44) = MHH2**2 - 3*MHH2*MHp2 - 2*MHp2**2

	NSatSapx(45) = MHH2**2 - 6*MHH2*MHp2 - 3*MHp2**2

	NSatSapx(5) = 5*MHH2**3*(MHp2 - 4*MT2) + 2*MT2*NSatSapx(4)

        NSatSapx(50) =
     &   2*MHH2**3 - MT2*NSatSapx(48) + MHH2*NSatSapx(49)

	NSatSapx(52) = MT2**2*(MHH2 - 6*MHp2) + MHH2*NSatSapx(51)

	NSatSapx(54) = 2*MHp2**2 - 32*MT2**2 + 3*MT2*MHp2

	NSatSapx(55) = 19*MHp2**2 + 16*MT2**2 - 68*MT2*MHp2

	NSatSapx(57) = MHp2**2 + 4*MT2**2 - 3*MT2*MHp2

	NSatSapx(58) = MHp2**2 + 4*MT2**2 - 2*MT2*MHp2

        NSatSapx(62) =
     &   2*MHH2**3*(MHp2 - 2*MT2) + MHH2**2*NSatSapx(60) -
     &    2*MHp2*NSatSapx(61)

        NSatSapx(65) =
     &   -(6*MT2*(MHp2**2*(MHp2 - 2*MT2))) +
     &    2*MHH2**2*NSatSapx(63) + MHH2*MHp2*NSatSapx(64)

	NSatSapx(7) = 15*MHp2**2 - 8*MT2**2 - 28*MT2*MHp2

	NSatSapx(8) = MHp2**2 + 40*MT2**2 - 20*MT2*MHp2

        NSatSapx(16) =
     &   -(8*MT2*(MHH2*NSatSapx(12))) + MHH2**3*NSatSapx(11) +
     &    2*MHH2**2*NSatSapx(13) + MHp2*NSatSapx(15)

        NSatSapx(21) =
     &   -(MT2**2*(MHp2*NSatSapx(18))) + MT2*(MHH2*NSatSapx(20)) +
     &    MHH2**3*NSatSapx(17) - MHH2**2*NSatSapx(19)

        NSatSapx(24) =
     &   2*MHH2**3*(MHp2 - 2*MT2) -
     &    2*MT2*(MHp2*(7*MHp2 - 20*MT2)*(MHp2 + 8*MT2)) -
     &    MHH2**2*NSatSapx(22) + MHH2*NSatSapx(23)

	NSatSapx(27) = 3*MHp2**2 - 100*MT2**2 + 52*MT2*MHp2

	NSatSapx(28) = 17*MHp2**2 - 112*MT2**2 + 76*MT2*MHp2

	NSatSapx(30) = MHp2**2 - 8*MT2**2 + 17*MT2*MHp2

        NSatSapx(35) =
     &   -(MT2*(MHH2*NSatSapx(33))) + MHH2**2*NSatSapx(32) -
     &    MHp2*NSatSapx(34)

        NSatSapx(37) =
     &   2*MHH2**2*(MHp2 - 2*MT2) +
     &    MHp2*(MHp2 - MT2)*(MHp2 + 2*MT2) - MHH2*NSatSapx(36)

        NSatSapx(41) =
     &   2*MT2*(MHp2**2*(5*MHp2 - 32*MT2)) +
     &    4*MHH2**2*NSatSapx(39) - MHH2*MHp2*NSatSapx(40)

        NSatSapx(42) =
     &   MHH2*MHp2*(5*MHp2 - 12*MT2) + MHH2**2*(MHp2 - 6*MT2) -
     &    3*MHp2**2*(MHp2 - 2*MT2)

        NSatSapx(46) =
     &   2*MHH2**2*MHp2**3 - 3*MHH2*MHp2**4 + MHp2**5 -
     &    2*MT2*(MHp2**2*NSatSapx(43)) +
     &    4*MT2**2*(MHp2*NSatSapx(44)) - 4*MT2**3*NSatSapx(45)

        NSatSapx(53) =
     &   2/MT2*(NSatSapl(3)*NSatSapx(52)) -
     &    ((MHp2 - 4*MT2)*NSatSapx(50))/((MHp - 2*MT)*(MHp + 2*MT))

        NSatSapx(56) =
     &   12*MHH2**2*NSatSapx(17) + 2*MHp2**2*NSatSapx(54) -
     &    MHH2*MHp2*NSatSapx(55)

        NSatSapx(59) =
     &   2*MHH2**2*(MHp2 - 3*MT2)*(MHp2 + 2*MT2) -
     &    3*MHH2*MHp2*NSatSapx(57) + MHp2**2*NSatSapx(58)

        NSatSapx(6) = MHH2**2*NSatSapx(2) - MHH2*NSatSapx(3) +
     &    MHp2*NSatSapx(5)

        NSatSapx(66) =
     &   (NSatSapl(8)*NSatSapx(62))/
     &     ((MHH - 2*MT)**2*(MHH + 2*MT)**2) -
     &    (NSatSapl(9)*NSatSapx(65))/(MHp2*(MHp2 - 4*MT2))

        NSatSapx(9) = 8*MHH2**2*(MHp2 - MT2) -
     &    MHH2*NSatSapx(7) - MHp2*NSatSapx(8)

        NSatSapx(1) = (MHH2 - MHp2)*MHp2**2 +
     &    2*MT2**2*(MHH2 - 5*MHp2) - MT2*((3*MHH2 - 5*MHp2)*MHp2)

        NSatSapx(10) =
     &   MT2*NSatSapx(6) + 4*MT2**2*(NSatSapl(3)*NSatSapx(9))

        NSatSapx(25) =
     &   MT2*(NSatSapx(16) - 16*NSatSapl(3)*NSatSapx(21) +
     &      4*NSatSapl(3)**2*NSatSapx(24))

        NSatSapx(29) =
     &   4*MHH2**3*(MHp2 - 5*MT2) - 92*MT2**2*MHp2**2 +
     &    2*(MHH2**4 + MT2*(MHH2*NSatSapx(28))) -
     &    MHH2**2*NSatSapx(27)

        NSatSapx(31) =
     &   MHH2**3 - MHH2**2*(6*MHp2 + MT2) -
     &    MT2*(MHp2*(9*MHp2 + 32*MT2)) + 2*MHH2*NSatSapx(30)

        NSatSapx(38) =
     &   MHp2*(MHH2 - 4*MT2)*(MHH2 - MHp2 + 2*MT2)*NSatSapl(8) +
     &    (NSatSapl(6)*NSatSapx(35))/(MHH2 - MT2) +
     &    NSatSapl(7)*NSatSapx(37)

        NSatSapx(47) =
     &   (MT2*(NSatSapl(2)*
     &         (NSatSapx(41) + 4*NSatSapl(3)*NSatSapx(42))) +
     &      2*NSatSapl(9)*NSatSapx(46))/MHp2

        NSatSapx(67) =
     &   -((NSatSapl(1)*NSatSapx(53))/(MHH2 - MT2)) +
     &    (NSatSapl(2)*(2/MT2*(NSatSapl(3)*NSatSapx(59)) +
     &         NSatSapx(56)))/(MHp2*(MHp2 - 4*MT2)) -
     &    2*NSatSapx(66)

        NSatSapx(26) =
     &   8*NSatSapl(3)**2*NSatSapx(1) -
     &    NSatSapx(10)/(MHH2 - MT2) +
     &    NSatSapx(25)/(MHH2 - 4*MT2)**2

        NSatSapx(68) =
     &   -(MT2*(NSatSapl(1)*
     &          (NSatSapx(29) + 4*NSatSapl(3)*NSatSapx(31)))/
     &        ((MHH - 2*MT)**2*(MHH + 2*MT)**2)) +
     &    MT2*NSatSapx(67) + 2*NSatSapx(38) -
     &    NSatSapx(47)/(MHp2 - 4*MT2)

	NSatSapx(70) = 3 + (2*NSatSapl(1))/MHH2 - (2*NSatSapl(2))/MHp2

        NSatSapx(69) =
     &   -(NSatSapx(26)/
     &       ((MHH2 - MHp2)*MHp2*(MHp - 2*MT)*(MHp + 2*MT))) +
     &    (2*NSatSapx(68))/(MHH2 - MHp2)**2

	NSatSapx(71) = -4*MHH2*MHp2 + MHp2**2 + MHH2**2*NSatSapx(70)

        NSatSapx(72) =
     &   Re(NSatSapl(3)) + (MHp2 - MT2)*Re(NSatSapl(4)) -
     &    MHp2*Re(NSatSapl(5))

        NSatSA0eqHp = -(Alfa2/(MW2**2*Pi**2*SW2**2)*
     &      (3/512.D0*NSatSapx(69) -
     &         (3*MT2)/256.D0*
     &          (NSatSapx(71)*NSatSapx(72))/(MHH2 - MHp2)**2)/TB2)

	end
