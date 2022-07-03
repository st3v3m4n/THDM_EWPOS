c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich


* NSHA0eqHH.F
* generated 15-Dec-2015 10:24

C> @brief This subroutine calculates \f$\delta \rho^{(2)}_\mathrm{H,NS}\f$ for \f$m_{A^0}=m_{H^0}\f$.
C>
C> @ingroup group_rho
	subroutine calcNSHA0eqHH
	implicit none


#include "NSHA0eqHH.h"


	RealType T134sub

	external T134sub

	ComplexType A0FH, B0FH

	external A0FH, B0FH

#include "paraTHDM.h"
#include "paraSM.h"
	NSHahl(1) = B0FH(MHH2,MHH2,MHH2)

	NSHahl(2) = A0FH(MHH2)

	NSHahl(3) = B0FH(MHH2,MHp2,MHp2)

	NSHahl(4) = A0FH(MHp2)

	NSHahl(5) = B0FH(MHp2,MHH2,MHp2)

	NSHahl(6) = T134sub(MHH2,MHH2,MHH2)

	NSHahl(7) = T134sub(MHp2,MHH2,MHH2)

	NSHahl(8) = T134sub(MHp2,MHp2,MHH2)

	NSHahl(9) = T134sub(MHp2,MHp2,MHp2)

        NSHahx(2) = 2*MHH2**4 - 13*MHH2**3*MHp2 +
     &    36*MHH2**2*MHp2**2 - 62*MHH2*MHp2**3 + 10*MHp2**4

        NSHahx(4) = 4*MHH2**4 - 21*MHH2**3*MHp2 +
     &    36*MHH2**2*MHp2**2 - 58*MHH2*MHp2**3 + 12*MHp2**4

        NSHahx(3) = 18*MHH2**4 - 138*MHH2**3*MHp2 +
     &    287*MHH2**2*MHp2**2 - 70*MHH2*MHp2**3 - 52*MHp2**4 +
     &    (2*NSHahl(4)*NSHahx(2))/MHp2

        NSHahx(5) = 4*MHH2**5 - 50*MHH2**4*MHp2 +
     &    185*MHH2**3*MHp2**2 - 230*MHH2**2*MHp2**3 +
     &    112*MHH2*MHp2**4 + 24*MHp2**5 + NSHahl(4)*NSHahx(4)

	NSHahx(7) = MHH2**2*MHp2**2 + MHH2*MHp2**3

        NSHahx(8) = MHH2**3 - 17*MHH2**2*MHp2 +
     &    52*MHH2*MHp2**2 - 6*MHp2**3

	NSHahx(6) = NSHahl(2)*NSHahx(3) - (NSHahl(4)*NSHahx(5))/MHp2

        NSHahx(9) = 15*MHH2**4 - 88*MHH2**3*MHp2 +
     &    24*MHp2**4 + 92*NSHahx(7) + 2*NSHahl(8)*NSHahx(8)

        NSHahx(10) = (NSHahx(6) + (MHH2 - MHp2)*NSHahx(9))/
     &    (MHH2 - 4*MHp2)**2

        NSHahx(16) = 5 + 12*Re(NSHahl(1)) + 2*Re(NSHahl(3)) -
     &    4*Re(NSHahl(5))

        NSHahx(17) = -2*(MHH2 - 2*MHp2)*
     &     (6*Re(NSHahl(1)) + Re(NSHahl(3))) +
     &    MHH2*(5 - 4*Re(NSHahl(5)))

        NSHahx(18) = MHp2*
     &     (5 + 12*Re(NSHahl(1)) + 2*Re(NSHahl(3))) -
     &    4*(2*MHH2 - MHp2)*Re(NSHahl(5))

        NSHahx(1) = -53 - (140*NSHahl(2))/MHH2 +
     &    (8*NSHahl(2)**2)/MHH2**2

        NSHahx(11) = 3*NSHahl(2)**2 -
     &    8*(MHH2 - 2*MHp2)*NSHahl(6) -
     &    (2*MHH2 + 5*MHp2)*NSHahl(7) - MHp2*NSHahl(9) +
     &    NSHahx(10)/MHH2

        NSHahx(14) = 6*Re(NSHahl(1)) +
     &    (NSHahl(2)*(7 - 6*Re(NSHahl(1)) - Re(NSHahl(3))))/MHH2 +
     &    Re(NSHahl(3))

        NSHahx(15) = (NSHahl(4)*(1 - Re(NSHahl(5))))/MHp2 +
     &    Re(NSHahl(5))

        NSHahx(19) = (MHH2 - MHp2)*(MHH2 + MHp2)*NSHahx(16) +
     &    2*(NSHahl(2)*NSHahx(17) - NSHahl(4)*NSHahx(18))

        NSHahx(12) = -((56*NSHahl(6) + 3*MHH2*NSHahx(1))/
     &       MHH2) - (18*NSHahx(11))/(MHH2 - MHp2)**2

	NSHahx(13) = -2*MHH2 + (MW2*SW2)/(Alfa*Pi)*Lambda5

        NSHahx(20) = 4*NSHahx(14) + 8*NSHahx(15) -
     &    NSHahx(19)/(MHH2 - MHp2)**2

        NSHA0eqHH = -(Alfa2/(MW2**2*Pi**2*SW2**2)*
     &      ((1/TB - TB)**2*
     &        (1/36864.D0*NSHahx(12) - 1/4096.D0*NSHahx(20))*
     &        NSHahx(13)**2))

	end
