c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich



C> @brief This subroutine calculates the one-loop correction to the \f$\rho\f$ parameter from the top-Yukawa interaction.
C>
C>
C> @ingroup group_rho
	subroutine Calcdrhotop1L(drhotop1L)
	implicit none

C> @param[out] The parameter drhotop1L gives the one-loop contribution \f$\Delta \rho^{(1)}_{t}\f$.
	double precision drhotop1L

#include "paraTHDM.h"
#include "paraSM.h"
	drhotop1L = (3*Alfa*MT2)/(16.D0*MW2*Pi*SW2)

	end
