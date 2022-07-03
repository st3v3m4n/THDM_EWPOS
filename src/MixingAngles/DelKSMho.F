c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich


#include "types.h"

C> @brief Estimation of \f$\delta\kappa^\mathrm{(h.o.)}_\mathrm{SM}\f$,
C> the higher-order correction to \f$\Delta\kappa\f$ in the SM.
C>
C> The higher-order corrections in the SM  are approximated by
C>\f[
C> \delta\kappa_\mathrm{SM}^{(\mathrm{h.o.})}=\Delta\kappa^{(\mathrm{h.o.)}}_\mathrm{SM}-\Delta\kappa^{(1)}_\mathrm{SM}\left(M_{W,\mathrm{SM}}\right).
C> \f]
C>
C> The part \f$\Delta\kappa^{(\mathrm{h.o.})}_\mathrm{SM}\f$ is extracted from \f$s^2_{l,\mathrm{SM}}\f$,
C> the prediction of the effective leptonic mixing in the SM for \f${M_W=M_{W,\mathrm{SM}}}\f$.
C> \f$s^2_{l,\mathrm{SM}}\f$ is derived in the subroutine @ref calcslepsmfit from
C>the parametrization given in [hep-ph/0608099v2](https://arxiv.org/abs/hep-ph/0608099) \cite Awramik:2006uz.
C> The part \f$\Delta\kappa^{(\mathrm{h.o.})}_\mathrm{SM}\f$ is then obtained by
C>\f[
C> \Delta\kappa^{(\mathrm{h.o.})}_\mathrm{SM}=\frac{s_{l,\mathrm{SM}}^2}{1-\frac{M_{W,\mathrm{SM}}^2}{M_Z^2}}-1.
C>\f]
C> The one-loop corrections in the SM \f$\Delta\kappa^{(1)}_\mathrm{SM}\f$
C> (evaluated for \f${M_W=M_{W,\mathrm{SM}}}\f$)
C>  are then subtracted from\f$\Delta\kappa^{(\mathrm{h.o.})}_\mathrm{SM}\f$,
C> in  order to capture only the higher-order corrections \f$\delta\kappa_\mathrm{SM}^{(\mathrm{h.o.})}\f$.
C> \f$\Delta\kappa^{(1)}_\mathrm{SM}\f$ is calculated in the subroutine @ref calcdelksm1loop.
C>
C> @ingroup group_mixangles
      subroutine CalcDelKSMho(DelKSMho)
      implicit none
C> @param[out] DelKSMho gives the result for \f$\delta\kappa^\mathrm{(h.o.)}_\mathrm{SM}\f$.
      RealType DelKSMho
      RealType DelKSM1Loop, MWsave
      RealType slepSMfit, DelKSMfit, drSM

#include "paraSM.h"

c save current value of MW
      MWsave=MW

c set MW and weak mixing angle to SM-value from fit

      call CalcMWSMho(2)
      MW=MWSMho
      MW2=MW**2
      call SetWeakAngles


c Extract SM-value of DeltaKappa
c from fit on eff. lept. mix. angle
c in arXiv:hep-ph/0608099
      call CalcslepSMfit(slepSMfit)

      DelKSMfit= slepSMfit/SW2 -1
c Calculate DeltaKappa in the SM at one-loop with SM-value of MW

      call CalcDelKSM1Loop(DelKSM1Loop,1)

c Approximate higher-order DeltaKappa
c by subtracting the one-loop result from the fit result

      DelKSMho= DelKSMfit-DelKSM1Loop

c set MW back to original value

      MW=MWsave
      MW2=MW**2
      call SetWeakAngles


      end
