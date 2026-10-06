ccccc 
c
	module functions

	use SLy5_Model_parameters

	implicit double precision (a-h, o-z)

	real(8), parameter :: pi = 3.14159d0
	real(8), parameter :: dmass = 938.0d0 
	real(8), parameter :: rho_0 = 0.1604d0
	integer, parameter :: numzmax = 100
	integer, parameter :: numnmax = 300 

	
c	integer, dimension(0:1000) :: ndriplow, ndriphigh



	contains 

c    ------ Calculating dripline arrays -------- 

	subroutine dripline_arrays(ndriplow, ndriphigh)
	implicit double precision(a-h, o-z)

c	real(8), dimension(0:1000, 0:1000) :: fee
c	real(8), dimension(0:1000, 0:1000) :: etap_c, etan_c
	integer, intent(out) :: ndriplow(0:1000) 
	integer, intent(out) :: ndriphigh(0:1000) 

	temp0 = 0.0						
	rho_electron0 = 0.0


	ndriplow(0) = 1;    ndriphigh(0) = 1
      ndriplow(1) = 0;    ndriphigh(1) = 2
      ndriplow(2) = 1;    ndriphigh(2) = 4

30    continue	  
	   
      do 33 iz = 3, numzmax
      diz = dfloat(iz)
      iamin = nint(diz*1.2)
31    amin = dfloat(iamin)
      call binding(amin, diz, temp0, rho_electron0,res1, etapc, etanc)
      daless = amin - 1.0d0
      dzless = diz - 1.0d0
      call binding(daless, dzless, temp0,rho_electron0,res2,etapc,etanc)
      if(res1.le.res2) goto 32
      iamin = iamin + 1
      goto 31
32    ndriplow(iz) = iamin - iz

      iamax = iamin
34    continue     
      amax = dfloat(iamax)
      call binding(amax, diz, temp0, rho_electron0, res1, etap, etan)
      dmore = amax + 1.0
      
      call binding(dmore,diz,temp0, rho_electron0,res2,etap,etan)
      if (res1.le.res2) goto 35
      iamax = iamax + 1
	  inmax = iamax - iz
      if (inmax.ge.numnmax) then
      iamax = iz + numnmax
      go to 35
      end if
      go to 34
35    ndriphigh(iz) = iamax - iz

33	continue
	return
	end subroutine 

	

c	Defining common calculation, which will be needed for f and g

	subroutine calc1(betamun, betamuz, temp, proton_frac, dens_ratio,
	1derivnn, derivnz, derivzn, derivzz, funcn, funcz, fee)

	implicit double precision (a-h, o-z)
	integer, dimension(0:1000) :: ndriplow, ndriphigh
	real(8), dimension(0:1000, 0:1000) :: fee
	real(8), dimension(0:1000, 0:1000) :: etap_c, etan_c


	call dripline_arrays(ndriplow, ndriphigh)

	numa = 2000
	
      numz = nint(dfloat(numa)*proton_frac)
	numn = numa - numz 

	volf_ratio = 1.0d0/dens_ratio
	vol = volf_ratio*dfloat(numa)/0.1604d0
      vol_normal = dfloat(numa)/0.1604d0

	densbyrho0 = 1.0d0/volf_ratio
	dens = 0.1604d0/volf_ratio
      rho_electron = dens*proton_frac

	univ = (2.0d0*pi*temp)/(1240.0d0*1240.0d0)
	univ3by2 = univ**1.5d0
	univ5by2 = univ**2.5d0

      do i = 1, numzmax
	do j = 1, numnmax
	  fee(i, j) = 0.0d0
	  etap_c(i, j) = 0.0d0
	  etan_c(i, j) = 0.0d0
	end do
	end do

	dz_d = 1.0d0
      dn_d = 1.0d0
	da_d = dz_d + dn_d
      spin_d = 3.0
	call seitz_lightnuclei(da_d, dz_d, rho_electron, seitz_corr_d)
      bind_d = -2.225d0 - seitz_corr_d
      fee(1, 1) = -bind_d/temp + dlog(spin_d)

	dz_tr = 1.0d0
      dn_tr = 2.0d0
	da_tr = dz_tr + dn_tr
      spin_tr = 2.0
	call seitz_lightnuclei(da_tr,dz_tr,rho_electron,seitz_corr_tr)
      bind_tr = -8.482d0 - seitz_corr_tr
      fee(1, 2) = -bind_tr/temp + dlog(spin_tr)

	dz_H4 = 1.0d0
      dn_H4 = 3.0d0
	da_H4 = dz_H4 + dn_H4
      spin_H4 = 5.0
	call seitz_lightnuclei(da_H4, dz_H4, rho_electron, seitz_corr_H4)
      bind_H4 = -6.881d0 - seitz_corr_H4
      fee(1, 3) = -bind_H4/temp + dlog(spin_H4)

	dz_H5 = 1.0d0
      dn_H5 = 4.0d0
	da_H5 = dz_H5 + dn_H5
      spin_H5 = 2.0
	call seitz_lightnuclei(da_H5, dz_H5, rho_electron, seitz_corr_H5)
      bind_H5 = -6.682d0 - seitz_corr_H5
      fee(1, 4) = -bind_H5/temp + dlog(spin_H5)

	dz_H6 = 1.0d0
      dn_H6 = 5.0d0
	da_H6 = dz_H6 + dn_H6
      spin_H6 = 5.0
	call seitz_lightnuclei(da_H6, dz_H6, rho_electron, seitz_corr_H6)
      bind_H6 = -5.769d0 - seitz_corr_H6
      fee(1, 5) = -bind_H6/temp + dlog(spin_H6)

	dz_H7 = 1.0d0
      dn_H7 = 5.0d0
	da_H7 = dz_H7 + dn_H7
      spin_H7 = 2.0
	call seitz_lightnuclei(da_H7,dz_H7,rho_electron,seitz_corr_H7)
      bind_H7 = -6.580d0 - seitz_corr_H7
      fee(1, 6) = -bind_H7/temp + dlog(spin_H7)

	dz_he3 = 2.0d0
      dn_he3 = 1.0d0
	da_he3 = dz_he3 + dn_he3
      spin_he3 = 2.0
	call seitz_lightnuclei(da_he3,dz_he3,rho_electron,seitz_corr_he3)
      bind_he3 = -7.718d0 - seitz_corr_he3
      fee(2, 1) = -bind_he3/temp + dlog(spin_he3)

	dz_he4 = 2.0d0
      dn_he4 = 2.0d0
	da_he4 = dz_he4 + dn_he4
	call seitz_lightnuclei(da_he4,dz_he4,rho_electron,seitz_corr_he4)
      bind_he4 = -28.296d0 - seitz_corr_he4
      fee(2, 2) = -bind_he4/temp 

	dz_he5 = 2.0d0
      dn_he5 = 3.0d0
	da_he5 = dz_he5 + dn_he5
      spin_he5 = 4.0
	call seitz_lightnuclei(da_he5,dz_he5,rho_electron,seitz_corr_he5)
      bind_he5 = -27.561d0 - seitz_corr_he5
      fee(2, 3) = -bind_he5/temp + dlog(spin_he5)

	dz_he6 = 2.0d0
      dn_he6 = 4.0d0
	da_he6 = dz_he6 + dn_he6
      spin_he6 = 1.0
	call seitz_lightnuclei(da_he6,dz_he6,rho_electron,seitz_corr_he6)
      bind_he6 = -29.271d0 - seitz_corr_he6
      fee(2, 4) = -bind_he6/temp + dlog(spin_he6)

	dz_he7 = 2.0d0
      dn_he7 = 5.0d0
	da_he7 = dz_he7 + dn_he7
      spin_he7 = 4.0
	call seitz_lightnuclei(da_he7, dz_he7,rho_electron,seitz_corr_he7)
      bind_he7 = -28.861d0 - seitz_corr_he7
      fee(2, 5) = -bind_he7/temp + dlog(spin_he7)

	dz_he8 = 2.0d0
      dn_he8 = 6.0d0
	da_he8 = dz_he8 + dn_he8
      spin_he8 = 1.0
	call seitz_lightnuclei(da_he8,dz_he8,rho_electron,seitz_corr_he8)
      bind_he8 = -31.396d0 - seitz_corr_he8
      fee(2, 6) = -bind_he8/temp + dlog(spin_he8)

	dz_he9 = 2.0d0
      dn_he9 = 7.0d0
	da_he9 = dz_he9 + dn_he9
      spin_he9 = 2.0
	call seitz_lightnuclei(da_he9,dz_he9,rho_electron,seitz_corr_he9)
      bind_he9 = -30.141d0 - seitz_corr_he9
      fee(2, 7) = -bind_he9/temp + dlog(spin_he9)

	dz_he10 = 2.0d0
      dn_he10 = 8.0d0
	da_he10 = dz_he10 + dn_he10
      spin_he10 = 2.0
	call seitz_lightnuclei(da_he10, dz_he10, rho_electron
	1, seitz_corr_he10)
      bind_he10 = -29.951d0 - seitz_corr_he10
      fee(2, 8) = -bind_he10/temp + dlog(spin_he10)

      do iz = 3, numzmax
      do in = ndriplow(iz), ndriphigh(iz)
      if (in.gt.numnmax) goto 100
	ia = iz + in     
      da = dfloat(ia)
      dz = dfloat(iz)

	call binding(da, dz, temp, rho_electron, bind, etapc, etanc)
	fee(iz, in) = -(bind)/temp
	etap_c(iz, in) = etapc
	etan_c(iz, in) = etanc

100   continue
      end do
	end do

	emassp = dmass
	emassn = dmass

	potfp = 0.0d0
	potfn = 0.0d0

	sumn = 0.0d0;		sumz = 0.0d0
	derivnn = 0.0d0;	derivnz = 0.0d0
	derivzz = 0.0d0;    derivzn = 0.0d0
	sum_clust = 0.0d0

	etap = betamuz - (potfp/temp)
	etan = betamun - (potfn/temp)

	do iz = 0, numzmax
	do in = ndriplow(iz), ndriphigh(iz)
	if (in.gt.numnmax) goto 199
	  dz = dfloat(iz)
	  dn = dfloat(in)
	  da = dz + dn
	if (iz.eq.0.and.in.eq.1) then
	rhofn = univ3by2*(emassn**1.5d0)*exp(etan + (potfn/temp))
	dlhsn = rhofn/(2.0d0*univ3by2*(emassn**1.5d0))
      call Etainv(dlhsn, etan)

	else if (iz.eq.1.and.in.eq.0) then
	rhofp = univ3by2*(emassp**1.5d0)*exp(etap + (potfp/temp))
	dlhsp = rhofp/(2.0d0*univ3by2*(emassp**1.5d0))
      call Etainv(dlhsp, etap)

	call dmeanfield(rhofp,rhofn,etap,etan,temp,dkin_p,dkin_n,potfp
	1, potfn, poten_dens, emassp, emassn)

	else
c	  if(iz.eq.in) then
	  if(iz.eq.1.and.in.eq.1) then
	  fee_eff = fee(iz, in)
	  else if(iz.eq.1.and.in.eq.2) then	   
	  fee_eff = fee(iz, in)
	  else if(iz.eq.1.and.in.eq.3) then	   
	  fee_eff = fee(iz, in)
	  else if(iz.eq.1.and.in.eq.4) then	   
	  fee_eff = fee(iz, in)
	  else if(iz.eq.1.and.in.eq.5) then	   
	  fee_eff = fee(iz, in)
	  else if(iz.eq.1.and.in.eq.6) then	   
	  fee_eff = fee(iz, in)
	  else if(iz.eq.2.and.in.eq.1) then	   
	  fee_eff = fee(iz, in)
	  else if(iz.eq.2.and.in.eq.2) then	   
	  fee_eff = fee(iz, in)
	  else if(iz.eq.2.and.in.eq.3) then	   
	  fee_eff = fee(iz, in)
	  else if(iz.eq.2.and.in.eq.4) then	   
	  fee_eff = fee(iz, in)
	  else if(iz.eq.2.and.in.eq.5) then	   
	  fee_eff = fee(iz, in)
	  else if(iz.eq.2.and.in.eq.6) then	   
	  fee_eff = fee(iz, in)
	  else if(iz.eq.2.and.in.eq.7) then	   
	  fee_eff = fee(iz, in)
	  else if(iz.eq.2.and.in.eq.8) then	   
	  fee_eff = fee(iz, in)
	  else	   
  	  delta = (da - (2.0d0*dz))/da
	  delta2 = delta*delta
        rho = rho_0*(1.0d0 - (3.0d0*para_Lsym*delta2)/
	1(para_Ksat + para_Ksym*delta2))
	  rhopc = (rho*dz)/da
	  rhonc = rho - rhopc
	  fee_gas_cor1 = -((2.0d0/3.0d0)*(dkin_n + dkin_p))/temp
	  fee_gas_cor2 = poten_dens/temp
	  fee_gas_cor3n = rhofn*etan
	  fee_gas_cor3p = rhofp*etap
	  fee_gas_cor3 = fee_gas_cor3p + fee_gas_cor3n
	  fee_gas_cor = ((fee_gas_cor1+fee_gas_cor2+fee_gas_cor3)*da)/rho	
	  fee_eff = fee(iz, in) + fee_gas_cor
	  end if

	  termsumn = univ3by2*(dmass**1.5d0)*dn*((dz + dn)**1.5d0)
	1*exp((dn*(etan+(potfn/temp)))+(dz*(etap+(potfp/temp)))+fee_eff)

	  termsumz = univ3by2*(dmass**1.5d0)*dz*((dz+dn)**1.5d0)
	1*exp((dn*(etan+(potfn/temp)))+(dz*(etap+(potfp/temp)))+fee_eff)

	  sumn = sumn + termsumn
	  sumz = sumz + termsumz

	  termderivnn=univ3by2*(dmass**1.5d0)*(dn**2.0d0)*((dz+dn)**1.5d0)
	1*exp((dn*(etan+(potfn/temp)))+(dz*(etap+(potfp/temp)))+fee_eff)
	  termderivzz=univ3by2*(dmass**1.5d0)*(dz**2.0d0)*((dz+dn)**1.5d0)
     1*exp((dn*(etan+(potfn/temp)))+(dz*(etap+(potfp/temp)))+fee_eff)
	  termderivnz=univ3by2*(dmass**1.5d0)*(dn*dz)*((dz+dn)**1.5d0)
	1*exp((dn*(etan+(potfn/temp)))+(dz*(etap+(potfp/temp)))+fee_eff)
	  termderivzn=univ3by2*(dmass**1.5d0)*(dn*dz)*((dz+dn)**1.5d0)
	1*exp((dn*(etan+(potfn/temp)))+(dz*(etap+(potfp/temp)))+fee_eff)

	  derivnn = derivnn + termderivnn
	  derivnz = derivnz + termderivnz
	  derivzn = derivzn + termderivzn
	  derivzz = derivzz + termderivzz

	  termsum_clust = univ3by2*(dmass**1.5d0)*((dz+dn)**2.5d0)
	1*exp((dn*(etan+(potfn/temp)))+(dz*(etap+(potfp/temp)))+fee_eff)

	  sum_clust = sum_clust + termsum_clust

	end if
199   continue
	end do
	end do

	frac_clust = sum_clust/(sum_clust + rhofp + rhofn)
   	vol_av = vol - (vol_normal*frac_clust)

	  funcn = dfloat(numn) - (sumn*vol_av + rhofn*vol)
	  funcz = dfloat(numz) - (sumz*vol_av + rhofp*vol)

	  derivnn = -(derivnn*vol_av + rhofn*vol)
	  derivnz = -derivnz*vol_av
	  derivzn = -derivzn*vol_av
	  derivzz = -(derivzz*vol_av + rhofp*vol)

	return
	end subroutine

	
     
      subroutine calc2(betamun, betamuz, temp, proton_frac, dens_ratio,
	1funcn, funcz, vol_av)

	implicit double precision (a-h, o-z)
	integer, dimension(0:1000) :: ndriplow, ndriphigh
	real(8), dimension(0:1000, 0:1000) :: fee
	real(8), dimension(0:1000, 0:1000) :: etap_c, etan_c

	call dripline_arrays(ndriplow, ndriphigh)

	numa = 2000
	
      numz = nint(dfloat(numa)*proton_frac)
	numn = numa - numz 

	volf_ratio = 1.0d0/dens_ratio
	vol = volf_ratio*dfloat(numa)/0.1604d0
      vol_normal = dfloat(numa)/0.1604d0

	densbyrho0 = 1.0d0/volf_ratio
	dens = 0.1604d0/volf_ratio
      rho_electron = dens*proton_frac

	univ = (2.0d0*pi*temp)/(1240.0d0*1240.0d0)
	univ3by2 = univ**1.5d0
	univ5by2 = univ**2.5d0

      do i = 1, numzmax
	do j = 1, numnmax
	  fee(i, j) = 0.0d0
	  etap_c(i, j) = 0.0d0
	  etan_c(i, j) = 0.0d0
	end do
	end do

	dz_d = 1.0d0
      dn_d = 1.0d0
	da_d = dz_d + dn_d
      spin_d = 3.0
	call seitz_lightnuclei(da_d, dz_d, rho_electron, seitz_corr_d)
      bind_d = -2.225 - seitz_corr_d
      fee(1, 1) = -bind_d/temp + dlog(spin_d)

	dz_tr = 1.0d0
      dn_tr = 2.0d0
	da_tr = dz_tr + dn_tr
      spin_tr = 2.0
	call seitz_lightnuclei(da_tr,dz_tr,rho_electron,seitz_corr_tr)
      bind_tr = -8.482 - seitz_corr_tr
      fee(1, 2) = -bind_tr/temp + dlog(spin_tr)

	dz_H4 = 1.0d0
      dn_H4 = 3.0d0
	da_H4 = dz_H4 + dn_H4
      spin_H4 = 5.0
	call seitz_lightnuclei(da_H4, dz_H4, rho_electron, seitz_corr_H4)
      bind_H4 = -6.881 - seitz_corr_H4
      fee(1, 3) = -bind_H4/temp + dlog(spin_H4)

	dz_H5 = 1.0d0
      dn_H5 = 4.0d0
	da_H5 = dz_H5 + dn_H5
      spin_H5 = 2.0
	call seitz_lightnuclei(da_H5, dz_H5, rho_electron, seitz_corr_H5)
      bind_H5 = -6.682 - seitz_corr_H5
      fee(1, 4) = -bind_H5/temp + dlog(spin_H5)

	dz_H6 = 1.0d0
      dn_H6 = 5.0d0
	da_H6 = dz_H6 + dn_H6
      spin_H6 = 5.0
	call seitz_lightnuclei(da_H6, dz_H6, rho_electron, seitz_corr_H6)
      bind_H6 = -5.769 - seitz_corr_H6
      fee(1, 5) = -bind_H6/temp + dlog(spin_H6)

	dz_H7 = 1.0d0
      dn_H7 = 5.0d0
	da_H7 = dz_H7 + dn_H7
      spin_H7 = 2.0
	call seitz_lightnuclei(da_H7,dz_H7,rho_electron,seitz_corr_H7)
      bind_H7 = -6.580 - seitz_corr_H7
      fee(1, 6) = -bind_H7/temp + dlog(spin_H7)

	dz_he3 = 2.0d0
      dn_he3 = 1.0d0
	da_he3 = dz_he3 + dn_he3
      spin_he3 = 2.0
	call seitz_lightnuclei(da_he3,dz_he3,rho_electron,seitz_corr_he3)
      bind_he3 = -7.718 - seitz_corr_he3
      fee(2, 1) = -bind_he3/temp + dlog(spin_he3)

	dz_he4 = 2.0d0
      dn_he4 = 2.0d0
	da_he4 = dz_he4 + dn_he4
	call seitz_lightnuclei(da_he4,dz_he4,rho_electron,seitz_corr_he4)
      bind_he4 = -28.296 - seitz_corr_he4
      fee(2, 2) = -bind_he4/temp 

	dz_he5 = 2.0d0
      dn_he5 = 3.0d0
	da_he5 = dz_he5 + dn_he5
      spin_he5 = 4.0
	call seitz_lightnuclei(da_he5,dz_he5,rho_electron,seitz_corr_he5)
      bind_he5 = -27.561 - seitz_corr_he5
      fee(2, 3) = -bind_he5/temp + dlog(spin_he5)

	dz_he6 = 2.0d0
      dn_he6 = 4.0d0
	da_he6 = dz_he6 + dn_he6
      spin_he6 = 1.0
	call seitz_lightnuclei(da_he6,dz_he6,rho_electron,seitz_corr_he6)
      bind_he6 = -29.271 - seitz_corr_he6
      fee(2, 4) = -bind_he6/temp + dlog(spin_he6)

	dz_he7 = 2.0d0
      dn_he7 = 5.0d0
	da_he7 = dz_he7 + dn_he7
      spin_he7 = 4.0
	call seitz_lightnuclei(da_he7, dz_he7,rho_electron,seitz_corr_he7)
      bind_he7 = -28.861 - seitz_corr_he7
      fee(2, 5) = -bind_he7/temp + dlog(spin_he7)

	dz_he8 = 2.0d0
      dn_he8 = 6.0d0
	da_he8 = dz_he8 + dn_he8
      spin_he8 = 1.0
	call seitz_lightnuclei(da_he8,dz_he8,rho_electron,seitz_corr_he8)
      bind_he8 = -31.396 - seitz_corr_he8
      fee(2, 6) = -bind_he8/temp + dlog(spin_he8)

	dz_he9 = 2.0d0
      dn_he9 = 7.0d0
	da_he9 = dz_he9 + dn_he9
      spin_he9 = 2.0
	call seitz_lightnuclei(da_he9,dz_he9,rho_electron,seitz_corr_he9)
       bind_he9 = -30.141 - seitz_corr_he9
      fee(2, 7) = -bind_he9/temp + dlog(spin_he9)

	dz_he10 = 2.0d0
      dn_he10 = 8.0d0
	da_he10 = dz_he10 + dn_he10
      spin_he10 = 2.0
	call seitz_lightnuclei(da_he10, dz_he10, rho_electron
	1, seitz_corr_he10)
      bind_he10 = -29.951 - seitz_corr_he10
      fee(2, 8) = -bind_he10/temp + dlog(spin_he10)

      do iz = 3, numzmax
      do in = ndriplow(iz), ndriphigh(iz)
      if (in.gt.numnmax) goto 100
	ia = iz + in     
      da = dfloat(ia)
      dz = dfloat(iz)
	call binding(da, dz, temp, rho_electron, bind, etapc, etanc)
	fee(iz, in) = -(bind)/temp
	etap_c(iz, in) = etapc
	etan_c(iz, in) = etanc
100   continue
      end do
	end do

	emassp = dmass
	emassn = dmass

	potfp = 0.0d0
	potfn = 0.0d0

	sumn = 0.0d0;		sumz = 0.0d0
	derivnn = 0.0d0;	derivnz = 0.0d0
	derivzz = 0.0d0;    derivzn = 0.0d0
	sum_clust = 0.0d0

	etap = betamuz - (potfp/temp)
	etan = betamun - (potfn/temp)

	do iz = 0, numzmax
	do in = ndriplow(iz), ndriphigh(iz)
	if (in.gt.numnmax) goto 199
	  dz = dfloat(iz)
	  dn = dfloat(in)
	  da = dz + dn
	if (iz.eq.0.and.in.eq.1) then
	rhofn = univ3by2*(emassn**1.5d0)*exp(etan + (potfn/temp))
	dlhsn = rhofn/(2.0d0*univ3by2*(emassn**1.5d0))
      call Etainv(dlhsn, etan)

	else if (iz.eq.1.and.in.eq.0) then
	rhofp = univ3by2*(emassp**1.5d0)*exp(etap + (potfp/temp))
	dlhsp = rhofp/(2.0d0*univ3by2*(emassp**1.5d0))
      call Etainv(dlhsp, etap)

	call dmeanfield(rhofp,rhofn,etap,etan,temp,dkin_p,dkin_n,potfp
	1, potfn, poten_dens, emassp, emassn)

	else
c	  if(iz.eq.in) then
	  if(iz.eq.1.and.in.eq.1) then
	  fee_eff = fee(iz, in)
	  else if(iz.eq.1.and.in.eq.2) then	   
	  fee_eff = fee(iz, in)
	  else if(iz.eq.1.and.in.eq.3) then	   
	  fee_eff = fee(iz, in)
	  else if(iz.eq.1.and.in.eq.4) then	   
	  fee_eff = fee(iz, in)
	  else if(iz.eq.1.and.in.eq.5) then	   
	  fee_eff = fee(iz, in)
	  else if(iz.eq.1.and.in.eq.6) then	   
	  fee_eff = fee(iz, in)
	  else if(iz.eq.2.and.in.eq.1) then	   
	  fee_eff = fee(iz, in)
	  else if(iz.eq.2.and.in.eq.2) then	   
	  fee_eff = fee(iz, in)
	  else if(iz.eq.2.and.in.eq.3) then	   
	  fee_eff = fee(iz, in)
	  else if(iz.eq.2.and.in.eq.4) then	   
	  fee_eff = fee(iz, in)
	  else if(iz.eq.2.and.in.eq.5) then	   
	  fee_eff = fee(iz, in)
	  else if(iz.eq.2.and.in.eq.6) then	   
	  fee_eff = fee(iz, in)
	  else if(iz.eq.2.and.in.eq.7) then	   
	  fee_eff = fee(iz, in)
	  else if(iz.eq.2.and.in.eq.8) then	   
	  fee_eff = fee(iz, in)
	  else	   
  	  delta = (da - (2.0d0*dz))/da
	  delta2 = delta*delta
        rho = rho_0*(1.0d0 - (3.0d0*para_Lsym*delta2)/
	1(para_Ksat + para_Ksym*delta2))
	  rhopc = (rho*dz)/da
	  rhonc = rho - rhopc
	  fee_gas_cor1 = -((2.0d0/3.0d0)*(dkin_n + dkin_p))/temp
	  fee_gas_cor2 = poten_dens/temp
	  fee_gas_cor3n = rhofn*etan
	  fee_gas_cor3p = rhofp*etap
	  fee_gas_cor3 = fee_gas_cor3p + fee_gas_cor3n
	  fee_gas_cor = ((fee_gas_cor1+fee_gas_cor2+fee_gas_cor3)*da)/rho	
	  fee_eff = fee(iz, in) + fee_gas_cor
	  end if

	  termsumn = univ3by2*(dmass**1.5d0)*dn*((dz + dn)**1.5d0)
	1*exp((dn*(etan+(potfn/temp)))+(dz*(etap+(potfp/temp)))+fee_eff)

	  termsumz = univ3by2*(dmass**1.5d0)*dz*((dz+dn)**1.5d0)
	1*exp((dn*(etan+(potfn/temp)))+(dz*(etap+(potfp/temp)))+fee_eff)
	
	!	write(*,*) termsumn, termsumz 	

	  sumn = sumn + termsumn
	  sumz = sumz + termsumz

	  termderivnn=univ3by2*(dmass**1.5d0)*(dn**2.0d0)*((dz+dn)**1.5d0)
	1*exp((dn*(etan+(potfn/temp)))+(dz*(etap+(potfp/temp)))+fee_eff)
	  termderivzz=univ3by2*(dmass**1.5d0)*(dz**2.0d0)*((dz+dn)**1.5d0)
     1*exp((dn*(etan+(potfn/temp)))+(dz*(etap+(potfp/temp)))+fee_eff)
	  termderivnz=univ3by2*(dmass**1.5d0)*(dn*dz)*((dz+dn)**1.5d0)
	1*exp((dn*(etan+(potfn/temp)))+(dz*(etap+(potfp/temp)))+fee_eff)
	  termderivzn=univ3by2*(dmass**1.5d0)*(dn*dz)*((dz+dn)**1.5d0)
	1*exp((dn*(etan+(potfn/temp)))+(dz*(etap+(potfp/temp)))+fee_eff)

	  derivnn = derivnn + termderivnn
	  derivnz = derivnz + termderivnz
	  derivzn = derivzn + termderivzn
	  derivzz = derivzz + termderivzz

	  termsum_clust = univ3by2*(dmass**1.5d0)*((dz+dn)**2.5d0)
	1*exp((dn*(etan+(potfn/temp)))+(dz*(etap+(potfp/temp)))+fee_eff)

	  sum_clust = sum_clust + termsum_clust

	!write(*,*) termsumn, termsumz, termsum_clust

	end if
199   continue
	end do
	end do

	write(*,*) sumn, sumz, sum_clust

	frac_clust = sum_clust/(sum_clust + rhofp + rhofn)
   	vol_av = vol - (vol_normal*frac_clust)

	  funcn = dfloat(numn) - (sumn*vol_av + rhofn*vol)
	  funcz = dfloat(numz) - (sumz*vol_av + rhofp*vol)

	  derivnn = -(derivnn*vol_av + rhofn*vol)
	  derivnz = -derivnz*vol_av
	  derivzn = -derivzn*vol_av
	  derivzz = -(derivzz*vol_av + rhofp*vol)


	return
	end subroutine 




	end module functions 
	