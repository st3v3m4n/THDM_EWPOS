
c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich

* IHDMA0eqHH.F
* generated 1-Nov-2016 19:11

C> @brief This subroutine calculates the two-loop correction to the \f$\rho\f$ parameter in the IHDM for \f$m_{A^0}=m_{H^0}\f$.
C>
C> @ingroup group_rho
	subroutine calcIHDMA0eqHH
	implicit none


#include "IHDMA0eqHH.h"


	RealType T134sub

	external T134sub

	ComplexType A0FH, B0FH

	external A0FH, B0FH

#include "paraTHDM.h"
#include "paraSM.h"
	IHDMahl(1) = A0FH(Mh02)

	IHDMahl(10) = T134sub(MHp2,MHH2,Mh02)

	IHDMahl(11) = T134sub(MHp2,MHH2,0.D0)

	IHDMahl(2) = A0FH(MHH2)

	IHDMahl(3) = B0FH(MHH2,0.D0,MHp2)

	IHDMahl(4) = B0FH(MHH2,Mh02,MHH2)

	IHDMahl(5) = B0FH(MHp2,0.D0,MHH2)

	IHDMahl(6) = A0FH(MHp2)

	IHDMahl(7) = B0FH(MHp2,Mh02,MHp2)

	IHDMahl(8) = T134sub(MHH2,MHH2,Mh02)

	IHDMahl(9) = T134sub(MHp2,MHp2,Mh02)

        IHDMahx(25) = -Mh02 -
     &    MHH2*(2 - IHDMahl(2)**2/MHH2**2) + IHDMahl(8)

        IHDMahx(26) = Mh02 - 8*MHH2 + 4*mu2sq -
     &    ((Mh02 - 4*mu2sq)*IHDMahl(2))/MHH2

	IHDMahx(27) = Mh02**2 - 24*MHH2*(MHH2 - mu2sq) - 4*Mh02*mu2sq

	IHDMahx(29) = MHH2*(MHH2 - 3*MHp2) + (MHH2 + MHp2)*mu2sq

        IHDMahx(35) = 3*(MHH2**2 + MHp2**2) +
     &    4*MHH2*(MHp2 - mu2sq) - 4*MHp2*mu2sq + 2*mu2sq**2

	IHDMahx(43) = 2*MHp2**2 + MHp2*(MHH2 - 4*mu2sq) + mu2sq**2

	IHDMahx(45) = MHH2*(3*MHp2 - mu2sq) - MHp2*(MHp2 + mu2sq)

        IHDMahx(63) = MHH2 - MHp2 +
     &    (2*(MHH2 - mu2sq)*Re(IHDMahl(1)))/Mh02

	IHDMahx(64) = -MHH2**2 + 2*MHH2*mu2sq - mu2sq**2

        IHDMahx(66) = (MHH2 - MHp2)**2*Re(IHDMahl(5)) +
     &    2*(MHp2 - mu2sq)**2*Re(IHDMahl(7))

	IHDMahx(11) = 2*MHH2**2 + MHH2*(MHp2 - 4*mu2sq) + mu2sq**2

	IHDMahx(14) = Mh02**2 - 2*MHH2*(2*Mh02 + MHH2)

        IHDMahx(15) = Mh02**2 + 10*MHH2*(MHH2 - MHp2) -
     &    2*Mh02*(3*MHH2 - MHp2)

	IHDMahx(18) = -3*MHp2*mu2sq + MHH2*(2*MHp2 + mu2sq)

        IHDMahx(21) = 6*MHH2*MHp2 + 5*(MHH2**2 + MHp2**2) -
     &    8*(MHH2 + MHp2)*mu2sq

        IHDMahx(22) = 9*(MHH2**2 + MHp2**2) +
     &    2*MHH2*(5*MHp2 - 8*mu2sq) - 16*MHp2*mu2sq + 12*mu2sq**2

        IHDMahx(23) = 5*MHH2**3 -
     &    MHH2*MHp2*(3*MHp2 - 8*mu2sq) +
     &    MHp2**2*(5*MHp2 - 8*mu2sq) - MHH2**2*(3*MHp2 + 8*mu2sq)

        IHDMahx(28) = 4*(MHH2 - mu2sq)*IHDMahx(25) +
     &    IHDMahl(1)*IHDMahx(26) + (IHDMahl(2)*IHDMahx(27))/MHH2

        IHDMahx(3) = -MHH2 + 4*IHDMahl(2) +
     &    MHp2*(1 - (4*IHDMahl(6))/MHp2)

	IHDMahx(30) = Mh02*(MHp2 - mu2sq) + 2*IHDMahx(29)

	IHDMahx(34) = Mh02*(MHH2 - 2*mu2sq) + 4*mu2sq**2

        IHDMahx(36) = Mh02**2*(MHH2 + MHp2) +
     &    16*MHH2*MHp2*(3*(MHH2 + MHp2) - 4*mu2sq) -
     &    4*Mh02*IHDMahx(35)

        IHDMahx(39) = -Mh02 -
     &    MHp2*(2 - IHDMahl(6)**2/MHp2**2) + IHDMahl(9)

	IHDMahx(4) = 4*MHH2**2 - 13*MHH2*MHp2 + 5*MHp2**2

        IHDMahx(40) = Mh02 - 8*MHp2 + 4*mu2sq -
     &    ((Mh02 - 4*mu2sq)*IHDMahl(6))/MHp2

	IHDMahx(41) = Mh02**2 - 24*MHp2*(MHp2 - mu2sq) - 4*Mh02*mu2sq

        IHDMahx(44) = Mh02**2 + 4*Mh02*(MHH2 - 2*mu2sq) -
     &    16*IHDMahx(43)

	IHDMahx(46) = Mh02*(MHH2 - mu2sq) - 2*IHDMahx(45)

        IHDMahx(5) = -5*MHH2**2 + 13*MHH2*MHp2 - 4*MHp2**2 +
     &    (4*(MHH2**2 + MHH2*MHp2 + MHp2**2)*IHDMahl(6))/MHp2

	IHDMahx(50) = (MHH2 - MHp2)**2 + 2*(MHH2 - mu2sq)**2

	IHDMahx(52) = (MHH2 - MHp2)**2 + 2*(MHp2 - mu2sq)**2

	IHDMahx(56) = -8*mu2sq + Mh02*(1 + (2*Re(IHDMahl(1)))/Mh02)

	IHDMahx(57) = MHH2**2 + (MHH2 - MHp2)**2*Re(IHDMahl(3))

	IHDMahx(58) = MHp2**2 + (MHH2 - MHp2)**2*Re(IHDMahl(5))

        IHDMahx(60) = (MHH2 - MHp2)**2*Re(IHDMahl(3)) +
     &    2*(MHH2 - mu2sq)**2*Re(IHDMahl(4))

        IHDMahx(61) = (MHH2 - MHp2)*
     &     (Mh02 + 4*(MHH2 + MHp2 - 2*mu2sq)) -
     &    4*(MHH2 - MHp2)**2*Re(IHDMahl(5)) -
     &    8*(MHp2 - mu2sq)**2*Re(IHDMahl(7))

        IHDMahx(65) = -4*MHp2**2 + 4*IHDMahx(57) +
     &    Mh02*IHDMahx(63) -
     &    8*((MHH2 - MHp2)*mu2sq + IHDMahx(64)*Re(IHDMahl(4)))

	IHDMahx(67) = 2*IHDMahx(66) + (MHp2 - mu2sq)*Re(IHDMahl(1))

        IHDMahx(7) = -3*(MHH2 - MHp2)**2 +
     &    2*Mh02*(MHH2 + MHp2 - 2*mu2sq)

        IHDMahx(12) = Mh02**2 + 4*Mh02*(MHp2 - 2*mu2sq) -
     &    16*IHDMahx(11)

	IHDMahx(16) = 2*MHH2**3 + MHp2*IHDMahx(14) - mu2sq*IHDMahx(15)

        IHDMahx(17) = Mh02*(MHH2 - mu2sq)*(MHp2 - mu2sq) +
     &    (MHH2 - MHp2)**2*mu2sq

        IHDMahx(19) = -(Mh02**2*(MHH2 - mu2sq)) +
     &    2*((MHH2 - MHp2)*MHp2*(MHp2 - 5*mu2sq) +
     &       Mh02*IHDMahx(18))

	IHDMahx(2) = MHH2**2 + (MHH2 - MHp2)**2 + MHp2**2

        IHDMahx(24) = Mh02**3*(MHH2 + MHp2) +
     &    32*MHH2*MHp2*IHDMahx(21) - 2*Mh02**2*IHDMahx(22) -
     &    8*Mh02*IHDMahx(23)

        IHDMahx(31) = -IHDMahx(28) +
     &    (2*IHDMahl(2)**2*IHDMahx(30))/(MHH2 - MHp2)**2

	IHDMahx(33) = Mh02*(MHp2 - 2*mu2sq) + 4*mu2sq**2

        IHDMahx(37) = (2*IHDMahl(2)*IHDMahx(34))/
     &     (MHH2 - MHp2) - IHDMahx(36)/(Mh02 - 4*MHp2)

        IHDMahx(42) = 4*(MHp2 - mu2sq)*IHDMahx(39) +
     &    IHDMahl(1)*IHDMahx(40) + (IHDMahl(6)*IHDMahx(41))/MHp2

        IHDMahx(47) = -((IHDMahl(6)*IHDMahx(44))/
     &       ((MHH2 - MHp2)*MHp2)) +
     &    (4*(MHp2 - mu2sq)*IHDMahl(6)**2*IHDMahx(46))/
     &     ((MHH2 - MHp2)**2*MHp2**2)

        IHDMahx(51) = 2*IHDMahx(50) +
     &    Mh02*(MHH2 - mu2sq)*(1 - Re(IHDMahl(1))/Mh02) -
     &    2*(MHH2 - MHp2)**2*Re(IHDMahl(3)) -
     &    4*(MHH2 - mu2sq)**2*Re(IHDMahl(4))

        IHDMahx(53) = 2*IHDMahx(52) +
     &    Mh02*(MHp2 - mu2sq)*(1 - Re(IHDMahl(1))/Mh02) -
     &    2*(MHH2 - MHp2)**2*Re(IHDMahl(5)) -
     &    4*(MHp2 - mu2sq)**2*Re(IHDMahl(7))

        IHDMahx(54) = (MHH2 - mu2sq)**2*Re(IHDMahl(4)) +
     &    (MHp2 - mu2sq)**2*Re(IHDMahl(7))

        IHDMahx(59) = (MHH2 - MHp2)*IHDMahx(56) +
     &    4*IHDMahx(57) - 4*IHDMahx(58) +
     &    8*(MHH2 - mu2sq)**2*Re(IHDMahl(4)) -
     &    8*(MHp2 - mu2sq)**2*Re(IHDMahl(7))

        IHDMahx(6) = ((MHH2 - MHp2)*IHDMahl(1)*IHDMahx(3))/
     &     Mh02 - (IHDMahl(6)*IHDMahx(4))/MHp2 +
     &    (IHDMahl(2)*IHDMahx(5))/MHH2

        IHDMahx(62) = 4*(MHH2 - 2*MHp2)*IHDMahx(60) -
     &    MHH2*IHDMahx(61) +
     &    2*(MHH2 - MHp2)*(MHH2 - 2*mu2sq)*Re(IHDMahl(1))

	IHDMahx(68) = MHp2*IHDMahx(65) - 2*(2*MHH2 - MHp2)*IHDMahx(67)

        IHDMahx(8) = 3*(MHH2 - MHp2)**2*IHDMahl(11) +
     &    IHDMahl(10)*IHDMahx(7)

        IHDMahx(9) = IHDMahl(2)**2 + IHDMahl(6)**2 +
     &    (MHH2 - mu2sq)*IHDMahl(8) + (MHp2 - mu2sq)*IHDMahl(9)

        IHDMahx(1) = MHH2*(1 + (2*IHDMahl(2))/MHH2) -
     &    MHp2*(1 + (2*IHDMahl(6))/MHp2)

        IHDMahx(10) = -54*MHH2*MHp2 +
     &    8*(MHH2 + MHp2)*IHDMahl(11) + 11*IHDMahx(2) -
     &    4*IHDMahx(6) - (8*IHDMahx(8))/Mh02 + 16*IHDMahx(9)

        IHDMahx(13) = (8*MHH2*(MHH2 - mu2sq)*(MHp2 - mu2sq)*
     &       IHDMahl(6))/(MHH2 - MHp2)**2 -
     &    (MHH2**2*IHDMahx(12))/((Mh02 - 4*MHH2)*(MHH2 - MHp2))

        IHDMahx(20) = -(((MHH2 - mu2sq)*IHDMahl(8)*
     &         IHDMahx(16))/(Mh02 - 4*MHH2)) +
     &    2*IHDMahl(10)*IHDMahx(17) +
     &    ((MHp2 - mu2sq)*IHDMahl(9)*IHDMahx(19))/(Mh02 - 4*MHp2)

        IHDMahx(32) = IHDMahx(24)/(Mh02 - 4*MHp2) +
     &    4*(MHH2 - mu2sq)*IHDMahx(31)

        IHDMahx(38) = (2*IHDMahl(6)*IHDMahx(33))/
     &     ((Mh02 - 4*MHp2)*(MHH2 - MHp2)) -
     &    IHDMahx(37)/(Mh02 - 4*MHH2)

        IHDMahx(48) = 2*(MHp2 - mu2sq)*IHDMahx(42) -
     &    MHp2**2*IHDMahx(47)

        IHDMahx(55) = (IHDMahl(2)*IHDMahx(51))/MHH2 +
     &    (IHDMahl(6)*IHDMahx(53))/MHp2 + 4*IHDMahx(54) +
     &    (MHH2 + MHp2 - 2*mu2sq)*Re(IHDMahl(1)) +
     &    2*(MHH2 - MHp2)**2*(Re(IHDMahl(3)) + Re(IHDMahl(5)))

        IHDMahx(69) = (MHH2 - MHp2)*(MHH2 + MHp2)*
     &     IHDMahx(59) - 2*
     &     (IHDMahl(2)*IHDMahx(62) + IHDMahl(6)*IHDMahx(68))

        IHDMahx(49) = -IHDMahx(10) -
     &    (16*IHDMahx(20))/(MHH2 - MHp2)**2 +
     &    2*((MHH2 - MHp2)*(1 + (2*IHDMahl(1))/Mh02)*IHDMahx(1) +
     &       IHDMahx(32)/(Mh02 - 4*MHH2)) +
     &    4*IHDMahl(1)*IHDMahx(38) -
     &    4*((IHDMahl(2)*IHDMahx(13))/MHH2 +
     &       IHDMahx(48)/(Mh02 - 4*MHp2))

	IHDMahx(70) = 4*IHDMahx(55) - IHDMahx(69)/(MHH2 - MHp2)**2

        IHDMA0eqHH = Alfa2/(MW2**2*Pi**2*SW2**2)*
     &    (1/1024.D0*IHDMahx(49) + 1/512.D0*IHDMahx(70))

	end
