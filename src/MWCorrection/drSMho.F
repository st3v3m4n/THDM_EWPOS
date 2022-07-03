c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich


C> @brief This subroutine approximates the higher-order SM corrections to \f$\Delta r\f$ by using the result for \f$M_W\f$ from
C>  [hep-ph/0311148](https://arxiv.org/abs/hep-ph/0311148).
C>
C> The higher-order corrections in the SM are approximated by the constant term
C>
C>\f[\delta r^\mathrm{h.o.}_\mathrm{SM}=\Delta r^\mathrm{h.o.}_\mathrm{SM}-\Delta r^{(1)}_\mathrm{SM}\left(M_{W,\mathrm{SM}}\right).\f]
C> The contribution \f$\Delta r_\mathrm{SM}^\mathrm{h.o.}\f$ corresponds to the result of \f$\Delta r\f$ in the SM
C> with the corrections described in [hep-ph/0311148](https://arxiv.org/abs/hep-ph/0311148).
C> Since it is obtained technically from the SM result the W boson mass, \f$M_{W,\mathrm{SM}}\f$,
C> the subroutine \ref calcmwsmho has to be executed before calling \ref calcdrsmho.
C> The contribution \f$\Delta r^\mathrm{h.o.}_\mathrm{SM}\f$  is then extracted from \f$M_{W,\mathrm{SM}}\f$ with the function \ref drvalue.
C> In order to capture only the higher-order corrections, the one-loop SM contribution \f$\Delta r^{(1)}_\mathrm{SM}\f$
C> (which is calculated by the subroutine \ref calcdrsm1loop)
C>evaluated at \f$M_W=M_{W,\mathrm{SM}}\f$ is subtracted from \f$\Delta r^\mathrm{h.o.}_\mathrm{SM}\f$.
C> The result is the constant term \f$\delta r^\mathrm{h.o.}_\mathrm{SM}\f$.
C>
C> @ingroup group_MW
      Subroutine CalcdrSMho(drSMho)
C> @param[out] The parameter drSMho gives \f$\delta r^\mathrm{h.o.}_\mathrm{SM}\f$.
      double precision drSMho
      double precision drSM1Loop, MWsave, drvalue
      external drvalue
#include "paraSM.h"

      MWsave=MW

      MW=MWSMho
      MW2=MW**2
      call SetWeakAngles

      call CalcdrSM1Loop(drSM1Loop)

      drSMho=drvalue(MW)-drSM1Loop

      MW=MWsave
      MW2=MW**2
      call SetWeakAngles

      end
