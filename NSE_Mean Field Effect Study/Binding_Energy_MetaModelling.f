	subroutine binding(da,dz,temp,rho_electron,energy_tot,etap,etan)

	use SLy5_Model_parameters
      implicit real *8 (a-h, o-z)

	pi = 22.0d0/7.0d0
	coul_const = 0.864d0	
	num_charge = nint(dz)
	num_mass = nint(da)
	const_p = 3.0d0
	dmass = 938.0d0
	const_bs = 15.36563
	const_sigma0 = 1.09191

	rho_0 = 0.1604d0 
	b = 6.93147d0 !3.0103d0 

	delta = (da - (2.0d0*dz))/da
	delta2 = delta*delta
	prot_frac = dz/da

      rho = rho_0*(1.0d0 - (3.0d0*para_Lsym*delta2)/
	1(para_Ksat + para_Ksym*delta))

	rhop = (rho*dz)/da
	rhon = rho - rhop
	x = (rho - rho_0)/(3.0d0*rho_0)


	v0_is = para_Esat - para_Kinetic*(1.0d0 + para_kinetic_K0)
	v0_iv=para_Esym-(5.0d0/9.0d0)*para_Kinetic*(1.0d0+para_kinetic_K0+
	13.0d0*para_kinetic_Ksym)

	v1_is = -para_Kinetic*(2.0d0+5.0d0*para_kinetic_K0)
	v1_iv=para_Lsym-(5.0d0/9.0d0)*para_Kinetic*(2.0d0+5.0d0*
	1para_kinetic_K0+15.0d0*para_kinetic_Ksym)

	v2_is=para_Ksat-2.0d0*para_Kinetic*(-1.0d0+5.0d0*para_kinetic_K0)
	v2_iv=para_Ksym-(10.0d0/9.0d0)*para_Kinetic*(-1.0d0+5.0d0*
	1para_kinetic_K0+15.0d0*para_kinetic_Ksym)

	v3_is=para_Qsat-2.0d0*para_Kinetic*(4.0d0-5.0d0*para_kinetic_K0)
	v3_iv=para_Qsym-(10.0d0/9.0d0)*para_Kinetic*(4.0d0-5.0d0*
	1para_kinetic_K0-15.0d0*para_kinetic_Ksym)

	v4_is=para_Zsat-8.0d0*para_Kinetic*(-7.0d0+5.0d0*para_kinetic_K0)
	v4_iv=para_Zsym-(40.0d0/9.0d0)*para_Kinetic*(-7.0d0+5.0d0*
	1para_kinetic_K0+15.0d0*para_kinetic_Ksym)

	a4_is=(243.0d0*v0_is)-(81.0d0*v1_is)+((27.0d0*v2_is)/2.0d0)
	1-((3.0d0*v3_is)/2.0d0)+((1.0d0*v4_is)/8.0d0)
	a4_iv=(243.0d0*v0_iv)-(81.0d0*v1_iv)+((27.0d0*v2_iv)/2.0d0)
	1-((3.0d0*v3_iv)/2.0d0)+((1.0d0*v4_iv)/8.0d0)

	if(temp.eq.0.0d0) then
	f1_delta=((1.0d0+delta)**(5.0d0/3.0d0))
	1+((1.0d0-delta)**(5.0d0/3.0d0))
	f2_delta=delta*(((1.0d0+delta)**(5.0d0/3.0d0))
	1-((1.0d0-delta)**(5.0d0/3.0d0)))
	dkinetic1=(1.0d0+(para_kinetic_K0*rho/rho_0))*f1_delta
	dkinetic2=(para_kinetic_Ksym*rho/rho_0)*f2_delta
	dkinetic=0.5d0*para_Kinetic*((rho/rho_0)**(2.0d0/3.0d0))*
	1(dkinetic1+dkinetic2)
	else
	eff_factor_p1=(para_kinetic_K0-(para_kinetic_Ksym*delta))
	eff_factor_p=1.0d0+(eff_factor_p1*(rho/rho_0))
	emassp=dmass/eff_factor_p
	eff_factor_n1=(para_kinetic_K0+(para_kinetic_Ksym*delta))
	eff_factor_n=1.0d0+(eff_factor_n1*(rho/rho_0))
	emassn=dmass/eff_factor_n

      univ = (2.0d0*pi*temp)/(1240.0d0*1240.0d0)
	univ3by2=univ**1.5d0
	univ5by2=univ**2.5d0

	dlhsn = rhon/(2.0d0*univ3by2*(emassn**1.5d0))
      call Etainv(dlhsn, etan)
	dlhsp = rhop/(2.0d0*univ3by2*(emassp**1.5d0))
      call Etainv(dlhsp, etap)
	order3by2 = 1.5d0
	call Fermi(order3by2, etan, feta_kineticn)
	call Fermi(order3by2, etap, feta_kineticp)

	const_kinetic = 12.0d0*pi*univ5by2
	tau_n = const_kinetic*feta_kineticn*(emassn**2.5d0)
	ekin_n = (1240.0d0*1240.0d0*tau_n)/(8.0d0*pi*pi*emassn)
	tau_p = const_kinetic*feta_kineticp*(emassp**2.5d0)
	ekin_p = (1240.0d0*1240.0d0*tau_p)/(8.0d0*pi*pi*emassp)
	dkinetic = (ekin_n+ekin_p)/rho
	end if

	if(dz.eq.28.0.and.da.eq.56.0.and.temp.gt.0.0) then
	f1_delta_0 = ((1.0d0 + delta)**(5.0d0/3.0d0))
	1+((1.0d0 - delta)**(5.0d0/3.0d0))
	f2_delta_0 = delta*(((1.0d0+delta)**(5.0d0/3.0d0))
	1-((1.0d0-delta)**(5.0d0/3.0d0)))
	dkinetic1_0=(1.0d0+(para_kinetic_K0*rho/rho_0))*f1_delta_0
	dkinetic2_0=(para_kinetic_Ksym*rho/rho_0)*f2_delta_0
	dkinetic_0=0.5d0*para_Kinetic*((rho/rho_0)**(2.0d0/3.0d0))*
	1(dkinetic1_0 + dkinetic2_0)
	d_ex = (dkinetic - dkinetic_0)*da
	write(24, 224) temp, d_ex
	end if
224   format(2f9.3) 
	vpot0=v0_is+v0_iv*(delta**2.0d0)
	vpot1=(v1_is+v1_iv*(delta**2.0d0))*x
	vpot2=(1.0d0/2.0d0)*(v2_is+v2_iv*(delta**2.0d0))*x*x
	vpot3=(1.0d0/6.0d0)*(v3_is+v3_iv*(delta**2.0d0))*x*x*x
	vpot4=(1.0d0/24.0d0)*(v4_is+v4_iv*(delta**2.0d0))*x*x*x*x

	a4_is=(243.0d0*v0_is)-(81.0d0*v1_is)+((27.0d0*v2_is)/2.0d0)
	1-((3.0d0*v3_is)/2.0d0)+((1.0d0*v4_is)/8.0d0)
	a4_iv=(243.0d0*v0_iv)-(81.0d0*v1_iv)+((27.0d0*v2_iv)/2.0d0)
	1-((3.0d0*v3_iv)/2.0d0)+((1.0d0*v4_iv)/8.0d0)

	vpot_added=(a4_is+a4_iv*(delta**2.0d0))*(x**5.0d0)
	1*(dexp(-b*(1.0d0+(3.0d0*x))))
	vpot=vpot0+vpot1+vpot2+vpot3+vpot4+vpot_added
	if(temp.eq.0.0d0) then
	energy_bulk=(dkinetic+vpot)*da
	else
	energy_bulk_dens = -(2.0d0/3.0d0)*(ekin_n+ekin_p)+(vpot*rho)
	1+temp*((rhop*etap) + (rhon*etan))
	energy_bulk = (energy_bulk_dens*da)/rho
	end if

	sigma_neu = ((2.0d0**(const_p+1.0d0))+const_bs)
	sigma_deno = (prot_frac**(-const_p))+const_bs
	1+((1.0d0 - prot_frac)**(-const_p))
	sigma = const_sigma0*(sigma_neu/sigma_deno)
	radius0 = (3.0d0/(4.0d0*pi*rho))**(1.0d0/3.0d0)
	temp_c = 87.76d0*prot_frac*(1.0d0-prot_frac)
	1*((0.155/rho_0)**(1.0d0/3.0d0))
	energy_surface = 4.0d0*pi*radius0*radius0*(da**
	1(2.0d0/3.0d0))*sigma*((1.0d0-((temp/temp_c)**2.0d0))**2.0d0)

	seitz_factor1 = 1.5d0*(((2.0d0*rho_electron)/((1.0d0-delta)*rho))
	1**(1.0d0/3.0d0))
	seitz_factor2 = 0.5d0*((2.0d0*rho_electron)/((1.0d0 - delta)*rho))
	seitz = 1.0d0-(seitz_factor1-seitz_factor2)
c	seitz=1.0d0
	energy_coulomb = ((coul_const*seitz)/radius0)*((dz*dz)
	1/(da**(1.0d0/3.0d0)))

	energy_tot = energy_bulk + energy_surface + energy_coulomb

99    continue
      return
      end