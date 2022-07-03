c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich


C>@brief This subroutine calculates the one-loop non-standard contribution to the \f$\rho\f$ parameter.
C>
C> @anchor ZWNS1Loop
C> @image latex ZWNS1Loop.eps "Non-standard one-loop contribution to the gauge-boson self-energies" width=\textwidth
C> @image html ZWNS1Loop.png "Non-standard one-loop contribution to the self-energies of the gauge bosons"
C>
C> The non-standard one-loop contribution \f$\Delta \rho^{(1)}_\mathrm{NS}\f$ is originating from
C> the extended scalar sector in the THDM.
C> The corresponding self-energies are shown @link ZWNS1Loop above @endlink.
C> For the numerical evaluation the [LoopTools](http://www.feynarts.de/looptools/) library is used.
C>
C>
C>@ingroup group_rho
	subroutine CalcdrhoNS1L(drhoNS1L)
	implicit none

C> @param[out] The parameter drhoNS1l gives the one-loop non-standard contribution \f$\Delta \rho_\mathrm{NS}^{(1)}\f$.
	double precision drhoNS1L

#include "paraTHDM.h"
#include "paraSM.h"
        drhoNS1L = -(Alfa/(MW2*Pi)*
     &      (CW2/2.D0*A0(MHp2) +
     &        1/SW2*((-1/4.D0 + (CW2 - SW2)**2/4.D0)*
     &            B0i(bb00,0.D0,0.D0,0.D0) +
     &           1/4.D0*(-B0i(bb00,0.D0,MA02,MHp2) +
     &              CBA2*(B0i(bb00,0.D0,MA02,Mh02) -
     &               B0i(bb00,0.D0,Mh02,MHp2)) +
     &              SBA2*(B0i(bb00,0.D0,MA02,MHH2) -
     &                 B0i(bb00,0.D0,MHH2,MHp2)) +
     &              (CW2 - SW2)**2*B0i(bb00,0.D0,MHp2,MHp2)))))

	end
