c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich



C> @brief Subroutine for updating the weak mixing angles with new values of \f$M_W\f$.
C>
C>By calling the subroutine @ref setweakangles the electroweak mixing angle is
C> calculated with the currently used mass of the W boson.
C> @ingroup group_MW
      subroutine SetWeakAngles

#include "paraSM.h"


      CW2=MW2/MZ2
      CW=SQRT(CW2)
      SW2=1-CW2
      SW=SQRT(SW2)



      end
