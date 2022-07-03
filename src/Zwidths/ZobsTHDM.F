c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich


#include "types.h"

C> @brief Subroutine for the total width of the Z boson with THDM contributions
C>
C> Before executing the subroutine the partial Z widths have to be calculated
C> with the subroutine @ref calczwidthsthdm
C> and then passed by the input-array gzthdm.
C>
C> @ingroup group_Zwidths
      subroutine CalcGZtot(GZtot,GZpartial)
C> @param[out] GZtot gives the total width of the Z boson
      RealType GZtot
C> @param[in] The array GZpartial takes the partial Z widths in the THDM as input.
      RealType GZpartial(9)
      Integer J

      GZtot=3D0*GZpartial(1)

      Do 111 J=2,9
      GZtot=GZtot+GZpartial(J)
 111  CONTINUE

      end

C> @brief Subroutine for the hadronic part of the Z Width,
C> \f$\Gamma_\mathrm{had}\f$.
C>
C> Before executing the subroutine the partial Z widths have to be calculated
C> with the subroutine @ref calczwidthsthdm
C> and then passed by the input-array gzthdm.
C>
C> @ingroup group_Zwidths
      subroutine CalcGZhad(GZTHDMhad,GZTHDM)
C> @param[out] GZTHDMhad gives \f$\Gamma_\mathrm{had}\f$
      RealType GZTHDMhad
C> @param[in] The array GZTHDM takes the partial Z widths in the THDM as input.
      RealType GZTHDM(9)
      Integer K

      GZTHDMhad=0D0

      Do 112 K=5,9
      GZTHDMhad=GZTHDMhad+GZTHDM(K)
 112  CONTINUE

      end

C> Subroutine for the calculation of \f$R_l\f$ and \f$R_b\f$.
C>
C> The two ratios are defined as
C> \f[R_l=\frac{\Gamma_\mathrm{had}}{\Gamma_e}\f]
C> and
C> \f[R_b=\frac{\Gamma_b}{\Gamma_\mathrm{had}}\f]
C> with the hadronic decay width \f$\Gamma_\mathrm{had}\f$,
C> the partial decay width for \f$Z\rightarrow b\overline{b}\f$, \f$\Gamma_b\f$,
C> and the partial decay width for\f$Z\rightarrow e^+ e^-\f$, \f$\Gamma_e\f$.
C>
C> @ingroup group_Zwidths
      subroutine CalcZRatiosTHDM(Rl,Rb,GZTHDMhad,GZTHDM)
C> @param[out] Rl gives \f$R_l\f$.
      RealType Rl
C> @param[out] Rb gives \f$R_b\f$.
      RealType Rb
C> @param[in] GZTHDMhad takes the hadronic part of the Z width,
C> which can be calculated with the subroutine @ref calcgzhad.
      RealType GZTHDMhad
C> @param[in] The array GZTHDM takes the partial Z widths in the THDM,
C> which can be calculated with the subroutine @ref calczwidthsthdm.
      RealType GZTHDM(9)

      Rl=GZTHDMhad/GZTHDM(2)

      Rb=GZTHDM(9)/GZTHDMhad

      end

C> Subroutine for the calculation of the hadronic peak cross section in nbarn.
C>
C> The hadronic peak cross section is defined as
C> \f[\sigma^\mathrm{had}_0=\frac{12\pi}{M_Z^2}\cdot\frac{\Gamma_e\Gamma_\mathrm{had}}{\Gamma_Z^2},\f]
C> with the total Z width \f$\Gamma_Z\f$, the hadronic width \f$\Gamma_\mathrm{had}\f$,
C> and the partial width into \f$e^+e^-\f$, \f$\Gamma_e\f$.
C>
C> @ingroup group_Zwidths
      subroutine CalcsigmahTHDM(sigmah,GZTHDMtot,GZTHDMhad,GZTHDM)
C> @param[out] The parameter sigmah gives \f$\sigma^\mathrm{had}_0\f$.
      RealType sigmah
C> @param[in] GZTHDMtot takes the total width of the Z boson,
C> which can be calculated with the subroutine @ref calcgztot
      RealType GZTHDMtot
C> @param[in] GZTHDMhad takes the hadronic part of the Z width,
C> which can be calculated with the subroutine @ref calcgzhad.
      RealType GZTHDMhad
C> @param[in] The array GZTHDM takes the partial Z widths in the THDM,
C> which can be calculated with the subroutine @ref calczwidthsthdm.
      RealType GZTHDM(9)
#include "paraSM.h"

      sigmah=12*pi*GZTHDM(2)*GZTHDMhad/(MZ2*GZTHDMtot**2)
     & *0.389379338D6


      end
