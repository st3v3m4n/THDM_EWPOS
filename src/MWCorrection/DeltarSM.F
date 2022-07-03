c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright Wolfgang Hollik, Max-Planck-Institute for Physics in Munich




C> @brief Subroutine for the calculation of \f$\Delta r\f$ in the SM from Prof. Hollik.
C>
C> @ingroup group_MW
C>
C> The masses of the W and Z boson, which are needed as input-parameters,
C> have to be given in the fixed width definition (for details see the discussion in [hep-ph/0202131](https://arxiv.org/abs/hep-ph/0202131) \cite Freitas:2002ja)
C>
C> @param[in] mz: Mass of the Z boson in the fixed width definition.
C> @param[in] mw: Mass of the W boson in the fixed width definition.
C> @param[in] mt: Mass of the top-quark.
C> @param[in] mh: Mass of the SM Higgs boson.
C> @param[in] dal: Contribution to the photon vacuum polarization \f$\Delta\alpha\f$ from e,..,b.
C> @param[in] alfast: Strong coupling constant \f$\alpha_s\left(m_t^2\right)\f$ at the scale \f$m_t^2\f$
C> (calculated by the subroutine @ref calcalfasmt).
C> @param[out] deltar: Result for \f$\Delta r\f$ in the SM.

*****************************************************************
      SUBROUTINE RNORM(mz,mw,mt,mh,dal,alfast,deltar)
      IMPLICIT DOUBLE PRECISION (A-Z)
      PARAMETER(PI=3.1415926535897932384626433832795029D0,
     . ALFINV=137.035999074D0)
*      PARAMETER(ME=0.5110034D-3,MMU=0.10565943D0,MTAU=1.777D0,
*     .          MU=0.05811D0,MD=0.05831D0,MS=0.150D0,MC=1.5D0,
*     .          MU=0.003D0,MD=0.004D0,MS=0.150D0,MC=1.5D0,
      PARAMETER(ME=0.5110034D-4,MMU=0.10565943D-4,MTAU=1.1777D-5,
     .          MU=0.0001D0,MD=0.0002D0,MS=0.0001D0,MC=0.0002D0,
     .          MB=4.7D0)
      PARAMETER(QE=-1.D0,QU=2.D0/3.D0,QD=-1.D0/3.D0)
      Z  = MZ**2
      W  = MW**2
      t  = mt**2
      h  = mh**2
* sin(thetaw) etc
         CW=MW**2/MZ**2
         SW=1.D0-CW
         SW1=SQRT(SW)
         CW1=SQRT(CW)
* coupling constants:
* the lepton axial coupling is A, and the electron, up and down
* vector couplings are V, VU and VD.
         A=-1.D0/(4.D0*SW1*CW1)
         V=(1.D0-4.D0*SW)*A
         VU=(8.D0/3.D0*SW-1.D0)*A
         VD=(1.D0-4.D0/3.D0*SW)*A
*  mur = mu_ren  renormalization scale mu**2
      mur = w
      delta = 0d0
*  W selfenergy on-shell
      s = w
      m2 = me
      mm2 = m2**2
      fs12 = f(s,0d0,m2)
      b0 = delta-dlog(mm2/mur) +1d0 + fs12
      self  = (s-mm2/2d0)*B0 - s/3d0
     .        - mm2*(delta-dlog(mm2/mur))
     .        - mm2**2/(2d0*s) *fs12
      m2 = mmu
      mm2 = m2**2
      fs12 = f(s,0d0,m2)
      b0 = delta-dlog(mm2/mur) +1d0 + fs12
      self  = self
     .        + (s-mm2/2d0)*B0 - s/3d0
     .        - mm2*(delta-dlog(mm2/mur))
     .        - mm2**2/(2d0*s) *fs12
      m2 = mtau
      mm2 = m2**2
      fs12 = f(s,0d0,m2)
      b0 = delta-dlog(mm2/mur) +1d0 + fs12
      self  = self
     .        + (s-mm2/2d0)*B0 - s/3d0
     .        - mm2*(delta-dlog(mm2/mur))
     .        - mm2**2/(2d0*s) *fs12
      swlep = self
*
      m1 = mt
      m2 = mb
      mm1 = mt**2
      mm2 = mb**2
      fs12 = f(s,m1,m2)
      b0 = delta-dlog(m1*m2/mur)
     .     +1d0-(mm1+mm2)/(mm1-mm2)*dlog(m1/m2)
     .     + fs12
      self  =  (s-(mm1+mm2)/2d0)*B0 - s/3d0
     .        - mm1*(delta-dlog(mm1/mur))
     .        - mm2*(delta-dlog(mm2/mur))
     .        - (mm1-mm2)**2/(2d0*s) *fs12
      m1 = mc
      m2 = ms
      mm1 = mc**2
      mm2 = ms**2
      fs12 = f(s,m1,m2)
      b0 = delta-dlog(m1*m2/mur)
     .     +1d0-(mm1+mm2)/(mm1-mm2)*dlog(m1/m2)
     .     + fs12
      self  = self
     .        +(s-(mm1+mm2)/2d0)*B0 - s/3d0
     .        - mm1*(delta-dlog(mm1/mur))
     .        - mm2*(delta-dlog(mm2/mur))
     .        - (mm1-mm2)**2/(2d0*s) *fs12
      m1 = mu
      m2 = md
      mm1 = mu**2
      mm2 = md**2
      fs12 = f(s,m1,m2)
      b0 = delta-dlog(m1*m2/mur)
     .     +1d0-(mm1+mm2)/(mm1-mm2)*dlog(m1/m2)
     .     + fs12
      self  = self
     .        +(s-(mm1+mm2)/2d0)*B0 - s/3d0
     .        - mm1*(delta-dlog(mm1/mur))
     .        - mm2*(delta-dlog(mm2/mur))
     .        - (mm1-mm2)**2/(2d0*s) *fs12
      swqurk = 3d0*self
      resww = (swlep+swqurk)/(4d0*pi*alfinv*3d0*sw)
      resww = resww
     .  +( (-(CW/(3.D0*SW))*(7.D0*Z+17.D0*W-2.D0*(Z-W)**2/W)+W*SW/CW
     .    -(1.D0/(6.D0*SW))*(W/2.D0+Z-(Z-W)**2/(2.D0*W)))*F(W,MZ,MW)
     .   +((CW/(3.D0*SW))*(7.D0*Z+17.D0*W-4.D0*(Z-W))
     .   -(SW/CW)*W+(1.D0/(6.D0*SW))*(3.D0*W/2.D0))*Z*LOG(Z/W)/(Z-W)
     .    -(CW/(3.D0*SW))*(7.D0*Z+17.D0*W+2.D0*W/3.D0)
     .    +(SW/CW)*W+(1.D0/(6.D0*SW))*(-1.D0*W/3.D0-Z)
     .  -80.D0*W/9.D0
     .  +((11.D0*W/2.D0-H+(H-W)**2/(2.D0*W))*F(W,MH,MW)
     .    -(9.D0*W/2.D0)*H*LOG(H/W)/(H-W)+6.D0*W-H)/(6.D0*SW) )/
     .    (4D0*pi*alfinv)
        XWT = (MW/MT)**2
        DRSWTBW = (ALFAST/PI) * MT**2/(PI*ALFINV*SW)* F1(XWT)
*  W selfenergy at 0
      bud = delta-dlog(mu*md/mur)+1d0
     .      -(mu**2+md**2)/(mu**2-md**2)*dlog(mu/md)
      bcs = delta-dlog(mc*ms/mur)+1d0
     .      -(mc**2+ms**2)/(mc**2-ms**2)*dlog(mc/ms)
      btb = delta-dlog(mt*mb/mur)+1d0
     .      -(mt**2+mb**2)/(mt**2-mb**2)*dlog(mt/mb)
      dfud = (mu**2+md**2)/2d0
     .    -2d0*mu**2*md**2/(mu**2-md**2)*dlog(mu/md)
      dfcs = (mc**2+ms**2)/2d0
     .    -2d0*mc**2*ms**2/(mc**2-ms**2)*dlog(mc/ms)
      dftb = (mt**2+mb**2)/2d0
     .    -2d0*mt**2*mb**2/(mt**2-mb**2)*dlog(mt/mb)
      self0 = -(me**2+mmu**2+mtau**2)*3d0/4d0
     .       -3d0/2d0*(me**2*(delta-dlog(me**2/mur))
     .                +mmu**2*(delta-dlog(mmu**2/mur))
     .                +mtau**2*(delta-dlog(mtau**2/mur)))
     .                -3d0*(mu**2+md**2)/2d0*bud
     .                -3d0*(mc**2+ms**2)/2d0*bcs
     .                -3d0*(mt**2+mb**2)/2d0*btb
     .                -3d0*mu**2*(delta-dlog(mu**2/mur))
     .                -3d0*mc**2*(delta-dlog(mc**2/mur))
     .                -3d0*mt**2*(delta-dlog(mt**2/mur))
     .                -3d0*md**2*(delta-dlog(md**2/mur))
     .                -3d0*ms**2*(delta-dlog(ms**2/mur))
     .                -3d0*mb**2*(delta-dlog(mb**2/mur))
     .                -3d0/2d0*(dfud+dfcs+dftb)
      resw0 = self0/(4d0*pi*alfinv*3d0*sw)
      resw0 = resw0
     .   +( ((CW/SW)*(Z+3.D0*W)-W*SW/CW+W/(4.D0*SW))*Z*LOG(Z/W)/(Z-W)
     .   -(2.D0*CW/SW)*(Z+W)+W*SW/CW+(-7.D0*W-3.D0*Z)/(24.D0*SW)
     .  +W/3.D0-4.D0*W/3.D0
     .  +(-18.D0*H*W*LOG(H/W)/(H-W)+25.D0*W-3.D0*H)/(24.D0*SW) )/
     .   (4d0*pi*alfinv)
       DRSWTB0 = (ALFAST/PI) * MT**2/(PI*ALFINV*SW)* F1(0D0)
*  Z selfenergy on-shell
      dzmur = delta -dlog(z/mur)
      s = z
      selfz = 2d0*a**2*s*(dzmur-dlog(s/z)+5d0/3d0) *3d0
     .   + (v**2+a**2)*s*
     .   ( dzmur+2d0*dlog(mz/me)+(1d0+2*me**2/s)*f(s,me,me)-1d0/3d0 +
     .  dzmur+2d0*dlog(mz/mmu)+(1d0+2*mmu**2/s)*f(s,mmu,mmu)-1d0/3d0+
     .dzmur+dlog(z/mtau**2)+(1d0+2*mtau**2/s)*f(s,mtau,mtau)-1d0/3d0)
     .   + 3d0*(vu**2+a**2)*s*
     .   (dzmur+2d0*dlog(mz/mu)+(1d0+2*mu**2/s)*f(s,mu,mu)-1d0/3d0 +
     .    dzmur+2d0*dlog(mz/mc)+(1d0+2*mc**2/s)*f(s,mc,mc)-1d0/3d0 +
     .    dzmur+2d0*dlog(mz/mt)+(1d0+2*mt**2/s)*f(s,mt,mt)-1d0/3d0)
     .   + 3d0*(vd**2+a**2)*s*
     .   (dzmur+2d0*dlog(mz/md)+(1d0+2*md**2/s)*f(s,md,md)-1d0/3d0 +
     .    dzmur+2d0*dlog(mz/ms)+(1d0+2*ms**2/s)*f(s,ms,ms)-1d0/3d0 +
     .    dzmur+2d0*dlog(mz/mb)+(1d0+2*mb**2/s)*f(s,mb,mb)-1d0/3d0)
     .  - 3d0/(8d0*sw*cw)*(me**2*(dzmur+2d0*dlog(mz/me)+f(s,me,me))
     .            +mmu**2*(dzmur+2d0*dlog(mz/mmu)+f(s,mmu,mmu))
     .            +mtau**2*(dzmur+2d0*dlog(mz/mtau)+f(s,mtau,mtau))
     .          +3d0*mu**2*(dzmur+2d0*dlog(mz/mu)+f(s,mu,mu))
     .          +3d0*md**2*(dzmur+2d0*dlog(mz/md)+f(s,md,md))
     .          +3d0*mc**2*(dzmur+2d0*dlog(mz/mc)+f(s,mc,mc))
     .          +3d0*ms**2*(dzmur+2d0*dlog(mz/ms)+f(s,ms,ms))
     .          +3d0*mb**2*(dzmur+2d0*dlog(mz/mb)+f(s,mb,mb))
     .          +3d0*mt**2*(dzmur+2d0*dlog(mz/mt)+f(s,mt,mt)) )
      reszz = selfz/(3d0*pi*alfinv)
      reszz = reszz
     .+((((-10.D0*Z-20.D0*W)*CW**2+(2.D0*W+Z/4.D0)*(CW-SW)**2+3.D0*W)*
     .    F(Z,MW,MW)+Z*(-2.D0*CW**2/3.D0+(CW-SW)**2/6.D0) )*4.*A**2/3.D0
     .+((11.D0*Z-2.D0*H+(H-Z)**2/Z)*F(Z,MH,MZ)
     . +(11.D0*Z-2.D0*H)*(1.D0-(H+Z)*LOG(MH/MZ)/(H-Z)-LOG(MH*MZ/W))
     . -2.D0*H*LOG(H/W)-2.D0*Z*LOG(Z/W)+2.D0*Z/3.D0)*A**2/3.D0 )/
     .  (pi*alfinv)
        RZT = Z/(2.D0*MT)**2
        DRSZTZ = (ALFAST/PI) * 4.D0*MT**2/(PI*ALFINV)*
     .                     ( VU**2*V1(RZT) + A**2*A1(RZT) )
     .   +(alfast/pi)/(pi*alfinv)*(vd**2+a**2)*z*dlog(mt**2/z)
*  Pi_gamma(o)  photon vacuum polarization
      Pigt0 = qu**2/(alfinv*pi)*(delta - dlog(t/mur))
      Pigmz = dzmur +2d0*dlog(mz/me)
     .        + (1d0+2d0*me**2/z)*f(z,me,me) -1d0/3d0
     .        + dzmur +2d0*dlog(mz/mmu)
     .        + (1d0+2d0*mmu**2/z)*f(z,mmu,mmu) -1d0/3d0
     .        + dzmur +2d0*dlog(mz/mtau)
     .        + (1d0+2d0*mtau**2/z)*f(z,mtau,mtau) -1d0/3d0
     .        + 3d0*qu**2*(dzmur +2d0*dlog(mz/mu)
     .        + (1d0+2d0*mu**2/z)*f(z,mu,mu) -1d0/3d0)
     .        + 3d0*qd**2*(dzmur +2d0*dlog(mz/md)
     .        + (1d0+2d0*md**2/z)*f(z,md,md) -1d0/3d0)
     .        + 3d0*qu**2*(dzmur +2d0*dlog(mz/mc)
     .        + (1d0+2d0*mc**2/z)*f(z,mc,mc) -1d0/3d0)
     .        + 3d0*qd**2*(dzmur +2d0*dlog(mz/ms)
     .        + (1d0+2d0*ms**2/z)*f(z,ms,ms) -1d0/3d0)
     .        + 3d0*qd**2*(dzmur +2d0*dlog(mz/mb)
     .        + (1d0+2d0*mb**2/z)*f(z,mb,mb) -1d0/3d0)
      Pigmz = Pigmz/(3d0*pi*alfinv)
      Pigbos = -1d0/(6d0*pi*alfinv)
      Pig0 = dal + Pigt0 + Pigmz + Pigbos
******************************************************************
*   1-loop Deltar
      deltr1 = Pig0 -cw/sw*(reszz/z-resww/w)+(resw0-resww)/w +
     .  (6.D0+(7.D0-4.D0*SW)*LOG(CW)/(2.D0*SW))/(4.D0*PI*ALFINV*SW)
***************************
*   qcd corrections
*   light quarks from 1st and 2nd generation:
      DRqcdl = -(alfast/pi)/(pi*alfinv)/(4d0*sw)*(cw-sw)/sw* dlog(cw)
*   photon vacuum polarization from 3rd generation, not in dalfa:
      DRPGT =
     .    +(alfast/pi)/(pi*alfinv)*qd**2*2d0*dlog(mt/mz)
     .    +(alfast/pi)/(pi*alfinv)*qu**2*2d0*(2.404114d0-5d0/12d0)
      dr1qcd = -cw/sw*(DRSZTZ/z-DRSWTBW/w)+(DRSWTB0-DRSWTBW)/w
     .         +DRPGT*1d0 +DRqcdl*2d0
****  higher order corrections  **********************************
      GFERMI=1.166379D-5
      XXT=GFERMI*MT**2/(8D0*PI**2*DSQRT(2D0))
      xst=(mt/mz)**2/(16d0*alfinv*pi*sw*cw)
*      dr1qcd = dr1qcd-cw/sw*3d0*(xxt-xst)*(alfast/pi)*(-2.8599d0)
*      write(6,*) 'drqcd = ', dr1qcd
*   rescaling factor
      rescal = gfermi*w*dsqrt(2d0)* (alfinv*sw/pi)
*   2-loop qcd,  chetyrkin et al.
      lz=dlog(z/mt**2)
      dro1 = 3d0*xxt*(alfast/pi)*(-2.8599d0
     .       +(z/mt**2*(-1.564d0-0.6667d0*lz
     .                  +(0.1022d0+0.8889d0*lz)*sw)
     .  +z*z/mt**4*(-1.312d0+3.573d0*sw-3.582d0*sw*sw)))/rescal
*   3-loop QCD contribution
*   according to Chetyrkin et al, hep-ph9504413
      dro2 =  (ALFAST/PI)**2 *3d0*xxt * (-14.594d0
     .        +z/mt**2*(-17.224d0+0.08829d0*lz+0.4722d0*lz**2
     .        +(22.6367d0+1.2527d0*lz-0.8519d0*lz**2)*sw)
     .        +(z*z/mt**4)*(-7.7781d0-0.07226*lz+0.004938d0*lz**2
     .        +(21.497d0+0.05794*lz-0.006584d0*lz**2)*sw
     .        -21.0799*sw**2) )
*   more 3-loop contributions to rho, hep-ph/0302275
      dht = mh/mt-1d0
      x12 = 157.295d0 +112d0*dht-24.73d0*dht**2+7.39d0*dht**3
     .      -3.52d0*dht**4+2.06*dht**5
      x03 = 95.92d0-111.98d0*dht+8.099d0*dht**2+7.27d0*dht**4
     .      -15.6d0*dht**5
      dro12 = (alfast/pi)*xxt**2 *x12
      dro3 = xxt**3 *x03
*   4-loop contribution to rho (chetyrkin..; czakon..)
      dro4 = 3d0*xxt*(alfast/pi)**3*(-93.1501d0)
*
*      drqcd1 = -cw/sw*dro1
      drqcd1 = dr1qcd
      drqcd2 = -cw/sw*dro2
*      drqcd3 = -cw/sw*dro3
      drqcd3 = -cw/sw*dro4
*
      drqcdew2 = -cw/sw*dro12
      drew3    = -cw/sw*dro3
*   2-loop electroweak contribution
*   based on the parametrization in hep-ph/0608099
      dr1=deltr1-dal
      lh=dlog(mh/100d0)
      deltah=mh/100d0
      delal=dal/0.05907d0-1d0
      deltat=(mt/178d0)**2-1d0
*      delas=alfas/0.117d0-1d0
      deltaw=(mw+0.0274d0)/80.404d0-1d0
      r0=0.003354d0
      r1=-2.09d-4
      r2=2.54d-5
      r3=-7.85d-6
      r4=-2.33d-6
      r5=7.83d-3
      r6=3.38d-3
      r7=-9.89d-6
      r8=0.0939d0
      r9=0.204d0
      dr2=r0+r1*lh+r2*lh**2+r3*lh**4+r4*(deltah**2-1d0)
     .     +r5*deltat+r6*deltat**2+r7*deltat*lh
     .     +r8*deltaw+r9*deltaw*deltat
      deltr2=dal**2 + 2d0*dal*dr1 + dr2
*   reducible terms beyond 2-loop
      r2 = FBARB(mh/mt)
      drot = 3d0*xxt
      drots = -3d0*xxt*(alfast/pi)*2.8599d0
      drotss = -(ALFAST/PI)**2 *3d0*xxt *14.594d0
      drott = 3D0*XXT**2*r2
      drottt = dro3
      drotts = dro12
      drttt = 2d0*(cw/sw)**2*drot*drott-(cw/sw)**3*drot**3*0d0
      drtts = 2d0*(cw/sw)**2*drot*drots
      draaa = -dal*cw/sw*drott+ 0d0*dal*(cw/sw)**2*drot**2
     .        +0d0*dal**2*cw/sw*drot + 0d0*dal**3
      draas = -dal*cw/sw*drots
*   changing normalization of 2-loop ew dro
      DDRO2=3D0*(XXT**2-xst**2)*R2
      drbarb = -cw/sw*ddro2
**** output ************************************************
c      Print*,"Deltar-1Loop:",deltr1
      deltar = deltr1 +deltr2
     .        +drqcd1+drqcd2+drqcd3
     .        +drqcdew2+drew3
* so far without reducible terms beyond 2-loop order
      end
****************************************************************
      DOUBLE PRECISION FUNCTION F(S,MA,MB)
* Real part of the function F(s,ma,mb)
      IMPLICIT REAL*8(A-Z)
      PARAMETER(EPS=1.D-6)
      MA2=MA**2
      MB2=MB**2
      IF(ABS(S).LT.EPS) THEN
             F=0.0D0
      ELSEIF(MA.LT.EPS) THEN
           IF(S.GT.MB2+EPS) THEN
             F=1.D0+(1.D0-MB2/S)*LOG(1./(S/MB2-1.D0))
           ELSEIF(S.LT.MB2-EPS) THEN
             F=1.D0+(1.D0-MB2/S)*LOG(1./(1.D0-S/MB2))
           ELSE
             F=1.D0
           ENDIF
      ELSEIF(MB.LT.EPS) THEN
           IF(S.GT.MA2+EPS) THEN
             F=1.D0+(1.D0-MA2/S)*LOG(1./(S/MA2-1.D0))
           ELSEIF(S.LT.MA2-EPS) THEN
             F=1.D0+(1.D0-MA2/S)*LOG(1./(1.D0-S/MA2))
           ELSE
             F=1.D0
           ENDIF
      ELSE
          IF(ABS(MB-MA).LT.EPS) THEN
           F=2.D0
          ELSE
           F=1.D0+((MA2-MB2)/S-(MA2+MB2)/(MA2-MB2))*LOG(MB/MA)
          ENDIF
          IF(S.GE.(MA+MB)**2) THEN
            RPLUS=SQRT(S-(MA+MB)**2)
            RMIN =SQRT(S-(MA-MB)**2)
            F=F- RPLUS*RMIN*LOG((RPLUS+RMIN)**2/(4.D0*MA*MB))/S
          ELSEIF(S.LT.(MA-MB)**2) THEN
            RPLUS=SQRT((MA+MB)**2-S)
            RMIN =SQRT((MA-MB)**2-S)
            F=F+ RPLUS*RMIN*LOG((RPLUS+RMIN)**2/(4.D0*MA*MB))/S
          ELSE
            RPLUS=SQRT((MA+MB)**2-S)
            RMIN =SQRT(S-(MA-MB)**2)
            F=F- 2.D0*RPLUS*RMIN*ATAN(RMIN/RPLUS)/S
          ENDIF
        ENDIF
        END
      DOUBLE PRECISION FUNCTION G(S,MA,MB)
* Imaginary part of the function F(s,ma,mb)
      IMPLICIT DOUBLE PRECISION (A-Z)
      PARAMETER (PI=3.1415926535897932D0)
      G = 0.D0
      IF(S.GT.(MA+MB)**2)  G=PI*SQRT((S-(MA+MB)**2)*(S-(MA-MB)**2))/S
      END
      DOUBLE PRECISION FUNCTION P(S,M)
***************************************************************
* Real part of the 1-loop QED vacuumpolarisation contribution *
* from a fermion with mass m                                  *
*                                                             *
*   Relation with the function F:                             *
*     P(s,m) = 1/3 - (1 + 2m**2/s)*F(s,m,m)                   *
***************************************************************
      IMPLICIT DOUBLE PRECISION (A-Z)
      IF(S.EQ.0) THEN
        P = 0.D0
      ELSE IF(S.LT.0) THEN
        X = SQRT(1.D0-4.D0*M**2/S)
        P = -8.D0/3.D0+X**2-
     .          X*(3.D0-X**2)*LOG(-4.D0*M**2/(S*(1.D0+X)**2))/2.D0
      ELSE IF(S.LT.4.D0*M**2) THEN
        X = SQRT(4.D0*M**2/S-1.D0)
        P = -8.D0/3.D0-X**2+X*(3.D0+X**2)*ATAN(1.D0/X)
      ELSE
        X = SQRT(1.D0-4.D0*M**2/S)
        P = -8.D0/3.D0+X**2-
     .          X*(3.D0-X**2)*LOG(4.D0*M**2/(S*(1.D0+X)**2))/2.D0
      ENDIF
      END
****************************************************************
      FUNCTION FBARB(X)
      IMPLICIT REAL*8(A-Z)
*     COMMON/CDZCON/PI,PI2,F1,D3,ALFAI,AL4PI,AL2PI,AL1PI
      DATA P1/-0.74141D0/,P2/ -11.483D0  /,P3/  9.6577D0/,
     &     P4/ -6.7270D0/,P5/  3.0659D0  /,P6/-0.82053D0/,
     &     P7/ 0.11659D0/,P8/-0.67712D-02/
      PI=4D0*DATAN(1D0)
      PI2=PI*PI
      IF(X.LE.4D0) THEN
        FBARB=P1+P2*X+P3*X**2+P4*X**3+P5*X**4+P6*X**5+P7*X**6+P8*X**7
         ELSE
        RBTH=1/X**2
        ALRB=LOG(RBTH)
        FBARB=49D0/4D0+PI2+27D0/2D0*ALRB+3D0/2D0*ALRB**2
     &       +RBTH/3D0*(2D0-12D0*PI2+12D0*ALRB-27D0*ALRB**2)
     &       +RBTH**2/48D0*(1613-240*PI2-1500*ALRB-720 *ALRB**2)
      ENDIF
*
      END
C***********************************************************************
C***********************************************************************
C***********************************************************************

      FUNCTION V1(R)
      IMPLICIT DOUBLE PRECISION (A-Z)
      COMPLEX*16 CR,CV1
      PARAMETER(C1=1.0829D0,C2=8.4465D0,C3=-66.3923D0,C4=22.6086D0)
      PARAMETER(Z2=1.6449340668D0,Z3=1.202056903D0,PI=3.14159265359D0)
*      IF(R.LE.0.95D0)THEN
        Y= SQRT(1.D0-R)
        H = LOG(2.D0*Y)
        V1 = -4.D0*Z2*(3.D0-5.D0*Y**2)*H
     .           + C1 + 8.D0*PI*Y + C2*Y**2 + C3*Y**3 + C4*Y**4
*      ELSE
*        CR = DCMPLX(R,0.D0)
*        V1 = DREAL(CV1(CR))
*      ENDIF
      END
C-----------------------------------------------------------------------
      FUNCTION A1(R)
      IMPLICIT DOUBLE PRECISION (A-Z)
      COMPLEX*16 CR,CA1
      PARAMETER(C1=-0.6043D0,C2=-9.7824D0,C3=-6.5179D0,C4=0.8861D0)
      PARAMETER(Z2=1.6449340668D0,Z3=1.202056903D0,PI=3.14159265359D0)
*      IF(R.LE.0.95D0)THEN
        Y= SQRT(1.D0-R)
        H = LOG(2.D0*Y)
        A1 = 8.D0*Z2*Y**2*H
     .           + C1 + C2*Y**2 + C3*Y**3 + C4*Y**4
*      ELSE
*        CR = DCMPLX(R,0.D0)
*        A1 = DREAL(CA1(CR))
*      ENDIF
      END
C-----------------------------------------------------------------------
      FUNCTION F1(X)
      IMPLICIT DOUBLE PRECISION (A-Z)
      COMPLEX*16 CX,CF1
      PARAMETER(C1=-1.1881D0,C2=-2.0979D0,C3=4.1157D0,C4=-2.2082D0,
     .          C5=3.6968D0,C6=-2.1815D0)
      PARAMETER(Z2=1.6449340668D0,Z3=1.202056903D0,PI=3.14159265359D0)
*      IF(X.LE.0.95D0)THEN
        B = LOG(1.D0-X)
        F1 = (1.D0-X)**2*B*(3.D0/8.D0*B-Z2-9.D0/8.D0)
     .        + C1 + C2*X + C3*X**2 + C4*X**3 + C5*X**4 + C6*X**5
*      ELSE
*        CX = DCMPLX(X,0.D0)
*        F1 = DREAL(CF1(CX))
*      ENDIF
      END
