c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich


c needs "ZWidthSM1Loop.F"
#include "types.h"

C> @brief Approximation of the higher-order corrections to the partial Z widths in the SM,
C>  \f$\Delta\Gamma^{\mathrm{(h.o.)}}_{f,\mathrm{SM}}\f$.
C>
C> The higher-order corrections are given by
C> \f[
C> \Delta\Gamma^\mathrm{(h.o.)}_{f,\mathrm{SM}}=\Gamma^f_\mathrm{SM}-\Gamma^{(1)}_\mathrm{SM}\left(M_{W,\text{SM}}\right).
C> \f]
C> \f$\Gamma_\mathrm{SM}\f$ is the parameterization
C> of the partial width in the SM from [arXiv:1401.2447](https://arxiv.org/abs/1401.2447) \cite Freitas:2014hra,
C>  which is implemented in @ref zwidthsm.
C> Since these result correspond to a calculation with \f$M_W=M_{W,\mathrm{SM}}\f$,
C> the one-loop result \f$\Gamma^{(1)}_\mathrm{SM}\f$
C> (implemented in @ref calczwidthssm1loop) with \f$M_W=M_{W,\mathrm{SM}}\f$ is subracted
C> in order to keep the dependence on \f$M_W\f$ at the one-loop order.
C>
C> @ingroup group_Zwidths
      subroutine CalcZWidthsSMho(GZSMho)
C> @param[out] The array GZSMho gives \f$\Delta\Gamma^{\mathrm{(h.o.)}}_{f,\mathrm{SM}}\f$.
C> The entries in the array correspond to:
C> - 1: partial width of the neutrino
C> - 2: partial width of the electron
C> - 3: partial width of the muon
C> - 4: partial width of the tau lepton
C> - 5: partial width of the up quark
C> - 6: partial width of the down quark
C> - 7: partial width of the charm quark
C> - 8: partial width of the strange quark
C> - 9: partial width of the bottom quark
      RealType GZSMho(9)
      RealType GZSM1Loopfit(9)
      RealType MWsave
      RealType ZWidthSM
      external ZWidthSM
      Integer ivar

#include "paraSM.h"

      call CalcMWSMho(2)

      MWsave=MW
      MW=MWSMho
      MW2=MW**2
      CW=MW/MZ
      CW2=MW2/MZ2
      SW2=1-CW2
      SW=SQRT(SW2)


      call CalcZWidthsSM1Loop(GZSM1Loopfit)


      Do 111 ivar=1,9

      GZSMho(ivar)=ZWidthSM(ivar)-GZSM1Loopfit(ivar)

 111  Continue



      MW=MWsave
      MW2=MW**2
      CW=MW/MZ
      CW2=MW2/MZ2
      SW2=1-CW2
      SW=SQRT(SW2)

      end
