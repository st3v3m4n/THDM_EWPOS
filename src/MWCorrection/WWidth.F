c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich



#include "types.h"

C> @brief This subroutine calculates the width of the W boson.
C>
C> The width is needed for the conversion between the running width
C> and the fixed width definition of the W mass.

C> @ingroup group_MW
      subroutine calcWWidth(GammaW)
C> @param[out] GammaW gives the result for the width of the W boson
      RealType GammaW

#include"paraSM.h"

      GammaW=3D0*GF*MW**3/(2D0*sqrt2*pi)
     & *(1D0+2D0*AlfasMZ/(3D0*pi))

      end
