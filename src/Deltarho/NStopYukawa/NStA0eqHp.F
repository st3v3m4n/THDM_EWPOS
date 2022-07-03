c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich


* NStA0eqHp.F
* generated 14-Dec-2015 16:15

C> @brief This subroutine calculates \f$\delta\rho^{(2)}_\mathrm{t,NS}\f$ with \f$m_{A^0}=m_{H^\pm}\f$.
C>
C> @ingroup group_rho

	subroutine calcNStA0eqHp
	implicit none


#include "NStA0eqHp.h"


	RealType T134sub

	external T134sub

	ComplexType A0FH, B0FH

	external A0FH, B0FH

#include "paraTHDM.h"
#include "paraSM.h"
	NStapl(1) = A0FH(MHH2)

	NStapl(10) = T134sub(MT2,MT2,MHp2)

	NStapl(11) = T134sub(MHp2,0.D0,0.D0)

	NStapl(12) = T134sub(MT2,MHH2,0.D0)

	NStapl(13) = T134sub(MT2,MHp2,0.D0)

	NStapl(2) = A0FH(MHp2)

	NStapl(3) = A0FH(MT2)

	NStapl(4) = B0FH(MHp2,0.D0,MT2)

	NStapl(5) = B0FH(MHp2,MT2,MT2)

	NStapl(6) = B0FH(MT2,0.D0,MHp2)

	NStapl(7) = B0FH(MT2,MHH2,MT2)

	NStapl(8) = B0FH(MT2,MHp2,MT2)

	NStapl(9) = T134sub(MT2,MT2,MHH2)

	NStapx(42) = MHH2*MHp2 - MHp2**2 - 2*MT2*(2*MHH2 - 3*MHp2)

	NStapx(27) = MHH2**2 - MHH2*(MHp2 + 3*MT2) + 2*MT2*MHp2

	NStapx(28) = MHp2**2 + 2*MT2**2 - 5*MT2*MHp2

	NStapx(29) = MHp2**2 + 3*MT2**2 - 6*MT2*MHp2

	NStapx(32) = MHH2**2 - MHH2*(MHp2 + MT2) + 2*MT2*MHp2

	NStapx(41) = 3*MHp2**2 - 56*MT2**2 + 2*MT2*MHp2

        NStapx(43) = MHp2*(5*MHp2 + 28*MT2) +
     &    8/MT2*(NStapl(3)*NStapx(42))

        NStapx(10) = 7*MHp2**3 - 16*MT2**3 + 16*MT2**2*MHp2 -
     &    19*MT2*MHp2**2

	NStapx(15) = 3*MHp2**2 + 72*MT2**2 - 28*MT2*MHp2

        NStapx(18) = MHp2**3 - 16*MT2**3 + 15*MT2**2*MHp2 -
     &    7*MT2*MHp2**2

	NStapx(19) = 5*MHp2**2 + 32*MT2**2 - 30*MT2*MHp2

	NStapx(22) = MHH2 - 2*MT2*(3 + 1/MT2*NStapl(3))

        NStapx(30) = (MHH2 - MT2)*NStapl(12)*NStapx(27) +
     &    NStapl(13)*(MHp2*NStapx(28) - MHH2*NStapx(29))

        NStapx(31) = MHp2*(3*MHH2 - 4*MHp2 + 2*MT2) -
     &    2/MT2*((MHp2**2 - MHH2*(MHp2 + MT2))*NStapl(3))

        NStapx(33) = MHH2*(MHH2 - 2*MHp2 + 2*MT2) +
     &    2/MT2*(NStapl(3)*NStapx(32))

	NStapx(39) = MHH2*(MHp2 - 8*MT2) + 2*MT2*(MHp2 + 4*MT2)

	NStapx(40) = MHH2*(3*MHp2 - 8*MT2) - 2*MHp2*(MHp2 - 2*MT2)

        NStapx(44) = 9*MHH2**2*(MHp2 - 4*MT2) +
     &    2*MT2*NStapx(43) - 16*MHp2*NStapl(3)**2 + MHH2*NStapx(41)

	NStapx(52) = (2*Re(NStapl(2)))/MHp2 - Re(NStapl(8))

	NStapx(6) = MHp2**2 - 28*MT2**2 + 19*MT2*MHp2

	NStapx(7) = 5*MHp2**2 + 4*MT2**2 - 9*MT2*MHp2

        NStapx(9) = 5*MHp2**3 + 8*MT2**3 - 6*MT2**2*MHp2 -
     &    13*MT2*MHp2**2

        NStapx(11) = 5*MHp2**3 +
     &    (NStapl(2)*(1/MT2*(NStapl(3)*NStapx(10)) + 2*NStapx(9)))/
     &     MHp2

	NStapx(14) = 2*MHp2**2 + 18*MT2**2 - 13*MT2*MHp2

	NStapx(16) = 25*MHp2**2 + 4/MT2*(NStapl(3)*NStapx(15))

	NStapx(17) = 7*MHp2 + 2/MT2**2*((MHp2 - 5*MT2)*NStapl(3)**2)

        NStapx(20) = MHp2**3 +
     &    (NStapl(2)*(2*NStapx(18) + NStapl(3)*NStapx(19)))/MHp2

	NStapx(23) = -2*NStapl(9) + (3*NStapl(1)*NStapx(22))/MHH2

        NStapx(24) = 49*MHH2 -
     &    4/MT2*((19*MHH2 - 88*MT2)*NStapl(3)) - 56*NStapl(9)

        NStapx(34) = -(MT2*
     &       (NStapl(2)*NStapx(31) - NStapl(1)*NStapx(33))) +
     &    2*NStapx(30)

        NStapx(36) = MHH2**2 + 3*MHH2*MHp2 + 8*MHp2**2 -
     &    17*MT2*(MHH2 - MHp2)

	NStapx(38) = 3*MHH2 - 2*(MHp2 + MT2) - 4*NStapl(3)

        NStapx(45) = 4*NStapl(2)*
     &     (MHp2*NStapx(39) - 4*NStapl(3)*NStapx(40)) -
     &    (MHH2 - MHp2)*NStapx(44)

        NStapx(46) = MHH2*(MHH2 - 2*MHp2)*NStapl(9) +
     &    (MHp2*(MHH2*MHp2 - 2*MT2*(MHH2 + MHp2))*NStapl(10))/
     &     (MHp2 - 4*MT2)

        NStapx(49) = MHH2**2 - MHp2**2 + 2*MHH2*NStapl(1) -
     &    2*(2*MHH2 - MHp2)*NStapl(2)

        NStapx(5) = 5*MHp2**3 - 8*MT2**3 + 26*MT2**2*MHp2 -
     &    23*MT2*MHp2**2

        NStapx(53) = -(2*MT2*(3 + 1/MT2*Re(NStapl(3)))) +
     &    MHp2*NStapx(52) + Re(NStapl(1)) -
     &    (MHp2 - MT2)*Re(NStapl(6)) - (MHH2 - 4*MT2)*Re(NStapl(7))

        NStapx(54) = (MHp2 - MT2)*Re(NStapl(6)) +
     &    (MHH2 - 4*MT2)*Re(NStapl(7)) + MHp2*Re(NStapl(8))

        NStapx(56) = MT2*(9 + 4/MT2*Re(NStapl(3))) +
     &    2*NStapx(54) - 2*Re(NStapl(1)) - 4*Re(NStapl(2))

        NStapx(57) = MT2 + 2*NStapx(54) - 2*Re(NStapl(1)) -
     &    4*Re(NStapl(2)) + 4*Re(NStapl(3))

        NStapx(8) = 53*MHp2**2 +
     &    12/MT2*(NStapl(3)*NStapx(6)) -
     &    8/MT2**2*(NStapl(3)**2*NStapx(7))

	NStapx(1) = 3*MHp2**2 - 3*MT2**2 - 4*MT2*MHp2

        NStapx(12) = 12*MT2**4 + 273*MT2**3*MHp2 -
     &    MT2**2*NStapx(8) - 8*MT2*NStapx(11) +
     &    8*NStapl(10)*NStapx(5)

	NStapx(2) = -33 - 92/MT2*NStapl(3) + 8/MT2**2*NStapl(3)**2

        NStapx(21) = 16*MT2**3 - MT2*NStapx(16) -
     &    8*(MT2**2*NStapx(17) + NStapl(10)*NStapx(14)) +
     &    8*NStapx(20)

        NStapx(25) = 5*MHH2**2 -
     &    4*MT2**2*(7 - 6/MT2**2*NStapl(3)**2) - MT2*NStapx(24) +
     &    4*MHH2*NStapx(23)

        NStapx(3) = MT2*
     &     ((5 + 1/MT2*NStapl(3))*NStapl(1))/MHH2 + NStapl(12)

        NStapx(35) = MT2*((1 - 2/MT2*NStapl(3))*NStapl(3)) +
     &    NStapx(34)/(MHH2 - MHp2)**2

        NStapx(37) = MT2*NStapx(36) +
     &    8*MHH2*(MHH2 - 4*MT2)*NStapl(9) -
     &    8*MHp2*(MHp2 - 4*MT2)*NStapl(10)

	NStapx(4) = MHp2**2 + 4*MT2**2 - 5*MT2*MHp2

        NStapx(47) = 4*MHH2*NStapl(1)*NStapx(38) -
     &    NStapx(45)/(MHp2 - 4*MT2) - 8*NStapx(46)

        NStapx(50) = 2*(1 - NStapl(2)/MHp2) +
     &    NStapx(49)/(MHH2 - MHp2)**2

        NStapx(51) = Re(NStapl(3)) +
     &    (MHp2 - MT2)*Re(NStapl(4)) - MHp2*Re(NStapl(5))

        NStapx(55) = MT2*(3 + 4/MT2*Re(NStapl(3))) +
     &    1/MT2*(NStapl(3)*NStapx(53)) + 2*NStapx(54) -
     &    2*Re(NStapl(1)) - 4*Re(NStapl(2))

	NStapx(58) = 2/MT2*(NStapl(3)*NStapx(56)) - 3*NStapx(57)

        NStapx(13) = -(16*MT2*NStapl(9)) - MT2**2*NStapx(2) -
     &    8*(MT2*NStapl(12) + MHH2*NStapl(9)) +
     &    16*MHp2*NStapl(11) +
     &    (8*NStapl(13)*NStapx(1))/(MHp2 - MT2) +
     &    2*MHH2*(9*MT2 + 4*NStapx(3)) - NStapx(12)/NStapx(4)

        NStapx(26) = NStapx(21)/(MHp2 - 4*MT2)**2 -
     &    NStapx(25)/(MHH2 - 4*MT2)

        NStapx(48) = MT2*NStapx(47)/(MHH2 - MHp2)**2 +
     &    4*NStapx(35) - NStapx(37)/(MHH2 - MHp2)

        NStapx(59) = 2*NStapx(50)*NStapx(51) - 4*NStapx(55) -
     &    NStapx(58)

        NStA0eqHp = Alfa2/(MW2**2*Pi**2*SW2**2)*
     &    (3/2048.D0*(NStapx(13) +
     &          2*(MT2*NStapx(26) + NStapx(48))) +
     &       (3*MT2)/512.D0*NStapx(59))/TB2

	end
