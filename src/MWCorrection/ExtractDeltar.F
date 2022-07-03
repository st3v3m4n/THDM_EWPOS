c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich



C> @brief Function which extracts the value of \f$\Delta r\f$ that corresponds to a given value of \f$M_W\f$.
C>
C> For a specific \f$M_W\f$, which is given as an input-parameter, the function
C> returns the value of \f$\Delta r\f$, which fulfills the relation
C> \f[M_W^2 \left(1-\frac{M_W^2}{M_Z^2}\right)=\frac{\pi \alpha_{em}}{\sqrt{2} G_F}(1+\Delta r).\f]
C> The other parameters (\f$\alpha_{em},\;G_F,\;M_Z\f$) are defined in the file paraSM.h
C> and set by the routine @ref setsmparameter.
C>
C> @ingroup group_MW
      double precision function drvalue(mwvalue)
C> @param[in] The value of the W boson mass, from which the corresponding \f$\Delta r\f$ is extracted,
C> is given by the argument mwvalue.
      double precision mwvalue

#include "paraSM.h"


      drvalue=SQRT(2D0)*GF*mwvalue**2/(pi*Alfa)
     & *(1D0-mwvalue**2/MZ**2)-1D0

      end
