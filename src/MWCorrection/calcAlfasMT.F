c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich



#include "types.h"
C> @brief This Subroutine calculates \f$\alpha_s\left(m_t\right)\f$.

C> The required input-parameters are the top mass \f$m_t\f$
C> and the QCD scale \f$\Lambda_\mathrm{QCD}^{(5)}\f$,
C> which are set in the file paraSM.h.
C> The subroutine does not need any arguments, since \f$\alpha_s\left(m_t\right)\f$
C> is part of a common block in paraSM.h.

C> @ingroup group_MW
      subroutine calcAlfasMT()
      RealType AT, ALT
      RealType betacoeff0, betacoeff1, betacoeff2
      RealType coeffC1,coeffC2

#include "paraSM.h"

      AT=DLOG(MT**2/LambdaQCD**2)
      ALT=DLOG(AT)
      betacoeff0=11D0-10D0/3D0
      betacoeff1=102D0-38D0*5D0/3D0
      betacoeff2=(2857D0-5033D0*5D0/9D0+325D0/27D0*25D0)/2D0
      coeffC1=betacoeff1/(betacoeff0**2*AT)
      coeffC2=betacoeff2*betacoeff0/betacoeff1**2
      AlfasMT=4D0*PI/betacoeff0/AT*( 1D0-coeffC1*ALT
     .       +coeffC1**2*((ALT-.5D0)**2+coeffC2-1.25D0) )

      end
