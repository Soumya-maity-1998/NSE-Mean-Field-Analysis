cccc  Grand canonical model for neutron and proton
cccc	Date-02.04.2019
      implicit real *8 (a-h, o-z)

	dimension yy(0:1000, 0:1000), fee(0:1000, 0:1000)
	dimension ndriplow(0:1000), ndriphigh(0:1000)
	dimension yyrho(0:1000, 0:1000)
	dimension etap_c(0:1000, 0:1000), etan_c(0:1000, 0:1000)

      open(unit = 4, file = 'limz_.out', status = 'unknown')
	open(unit = 15, file = 'Mass_Fraction4_vs_rhoB_test.out', 
	1status = 'unknown')
	open(unit = 35, file = 'Bound4_vs_rhoB_test.out', 
	1status = 'unknown')
	open(unit = 10,file = 'Density4_vs_rhoB_yp=0.2_Temp=5_test.out',
     1status = 'unknown')
	open(unit = 12, file = 'isotopic4_rhoB_dependence_yp=0.2_Temp=5_
	1test.out', status = 'unknown')
	open(unit = 13, file = 'Hydrogen_isotope4_rhoB_test.out', status 
	1 = 'unknown') 
	open(unit = 14, file = "Helium_isotope4_rhoB_test.out", status = 
	1'unknown')
      
	open(unit = 20, file = 'BE_vs_Z_at_a=100.txt', status = 'unknown')

      open(unit=11,file='A0=2000_yp=0.2_Vf=3.33V0_T_dependence_Other_pro
	1perties_complete.out',status='unknown')
      open(unit=24,file='A0=2000_yp=0.2_Vf=3.33V0_T_dependence_excitatio
	1n_cluster.out',status='unknown')
      open(unit=23,file='A0=2000_yp=0.2_Vf=3.33V0_T_dependence_excitatio
	1n_gas.out',status='unknown')
      open(unit=25,file='A0=2000_yp=0.2_Vf=3.33V0_T_dependence_Mass_frac
	1tion.out',status='unknown')
      open(unit=26,file='A0=2000_yp=0.2_Vf=3.33V0_T_dependence_heavy.out
	1',status='unknown')

    

c      open(unit=21,file='A0=2000_yp=0.2_Vf=3.33V0_T=16MeV_free_energy1.o
c	1ut',status='unknown') 
c      open(unit=22,file='A0=2000_yp=0.2_Vf=3.33V0_T=16MeV_free_energy2.o
cj	1ut',status='unknown') 
c      open(unit=15,file='Others.out',status='unknown') 
c      open(unit=12,file='A0=300_Z0=92_Vf=2V0_to_50V0_T=10.0_MeV_mu_eta_U
c	1_conv.out',status='unknown') 
c      open(unit=13,file='A0=300_Z0=92_Vf=2V0_to_50V0_T=10.0_MeV_effectiv
c	1e_mass.out',status='unknown')

      pi = 3.14159265d0
	dmass = 938.0d0

	rho_0 = 0.1604d0
	para_Kinetic = 22.1d0
	para_kinetic_K0 = 0.4286d0
	para_kinetic_Ksym = 0.1786d0
	para_Esat = -15.98d0
	para_Esym = 32.03d0
	para_Lsym = 48.30d0
	para_Ksat = 230.0d0
	para_Ksym = -112.0d0
	para_Qsat = -364.0d0
	para_Qsym = 501.0d0
	para_Zsat = 1592.0d0
	para_Zsym = -3087.0d0
c
c	numn=28.0d0
c	numz=28.0d0
c


cccc ------------------------------------------------------------------ 
cccc  Maximum possible proton and neutron in a cluster are 100 and 300
cccc ------------------------------------------------------------------
      numzmax = 100
	numnmax = 300 
cccc ------------------------------------------------------------------
cccc  Calculating the driplines 
cccc ------------------------------------------------------------------
      ndriplow(0) = 1;    ndriphigh(0) = 1
      ndriplow(1) = 0;    ndriphigh(1) = 6
      ndriplow(2) = 1;    ndriphigh(2) = 8

cccc	Temperature and electron density are zero during dripline calculation only
	temp0 = 0.0d0
	rho_electron0 = 0.0d0

	goto 30
	 
	a = 100.0d0 	!!!!this part was to compare different BE cases.
	do i = 20, 95
	z  = a - dfloat(i) 
	
	call binding(a, z, temp0, rho_electron0, be1,etap,etan)
	write(20, '(i7, f10.4)') i, be1/a 
	end do

30    continue	  

      do 33 iz = 3, numzmax
      diz = dfloat(iz)
      iamin = nint(diz*1.2)
 31   amin = dfloat(iamin)
      call binding(amin,diz,temp0, rho_electron0,res1, etapc, etanc)
      daless = amin - 1.0d0
      dzless = diz - 1.0d0
      call binding(daless,dzless,temp0,rho_electron0,res2,etapc,etanc)
       if(res1.le.res2) goto 32
      iamin = iamin + 1
      goto 31
 32   ndriplow(iz) = iamin - iz
 
      iamax = iamin
 34   continue     
      amax = dfloat(iamax)
      call binding(amax,diz,temp0, rho_electron0,res1,etap,etan)
      dmore = amax + 1.0
      
      call binding(dmore,diz,temp0, rho_electron0,res2,etap,etan)
      if(res1.le.res2) goto 35
      iamax = iamax + 1
	inmax = iamax - iz
      if(inmax.ge.numnmax)then
      iamax = iz + numnmax
      go to 35
      end if
      go to 34
 35   ndriphigh(iz) = iamax - iz

 33   continue
      
      do 51 i = 0, numzmax
		write(4, *) i, ndriplow(i), ndriphigh(i)
 51   continue

	if (numnmax.gt.ndriphigh(numzmax)) then
		numnmax = ndriphigh(numzmax)
	end if
c	write(*,*)numnmax
	numamax = numzmax + numnmax

      do iz = 3, numzmax
		ndriphigh(iz) = ndriphigh(iz)*4 !before it was 3 in place of 2
      end do

	

cccc ------------------------------------------------------------------
ccc   Guesss value of BetaMu 
cccc ------------------------------------------------------------------
      betamuz = -0.2d0 ! at first these were -0.2 and -0.2 
      betamun = -0.2d0


cccc ------------------------------------------------------------------
cccc  Input Density, temperature and proton fraction 
cccc ------------------------------------------------------------------

	numa = 2000
	dyp = 0.01d0  
	do iyp = 0, 0  ! end point = 0, 50
	proton_frac = 0.2d0
	proton_frac = proton_frac - dyp*dfloat(iyp) 
	numz = nint(dfloat(numa)*proton_frac)
	numn = numa - numz

	tempi = 5.0d0

      do itemp = 1, 1
	temp = tempi - dfloat(itemp - 1)*0.1d0

	do idens = 50, 10, -1
c      dens_ratio = 0.1d0
	dens_ratio = 10.0d0**(-0.1d0*dfloat(idens))
	volf_ratio = 1.0d0/dens_ratio
cccc -------------------------------------------------------------------
	vol = volf_ratio*dfloat(numa)/0.1604d0
      vol_normal = dfloat(numa)/0.1604d0

	densbyrho0 = 1.0d0/volf_ratio
	dens = 0.1604d0/volf_ratio
      rho_electron = dens*proton_frac

      univ = (2.0d0*pi*temp)/(1240.0d0*1240.0d0)
	univ3by2 = univ**1.5d0
	univ5by2 = univ**2.5d0

	numnmax3 = numnmax*4 ! CHANGE THIS ACCORDINGLY TO 1, 2 AND 4 	                          
      do i = 1, numzmax
	do j = 1, numnmax3
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
      if (in.gt.numnmax3) goto 100
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

	iter = 0
	emassp = dmass
	emassn = dmass
	potfp = 0.0d0
	potfn = 0.0d0
ccc   Iterative technique for finding BetaMun and BetaMuz 
ccc  -----------------------------------------------------
200   continue
      iter = iter + 1
c	write(*,*)iter
	sumn = 0.0d0;    sumz = 0.0d0
	derivnn = 0.0d0;	derivnz = 0.0d0
	derivzz = 0.0d0;    derivzn = 0.0d0
	sum_clust = 0.0d0

	etap = betamuz - (potfp/temp)
	etan = betamun - (potfn/temp)

	do iz = 0, numzmax
	do in = ndriplow(iz), ndriphigh(iz)
	if (in.gt.numnmax3) goto 199
	  dz = dfloat(iz)
	  dn = dfloat(in)
	  da = dz + dn
	if (iz.eq.0.and.in.eq.1) then
	rhofn = univ3by2*(emassn**1.5d0)*exp(etan + (potfn/temp))
	dlhsn = rhofn/(2.0d0*univ3by2*(emassn**1.5d0))
      call Etainv(dlhsn, etan)

	else if(iz.eq.1.and.in.eq.0) then
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
	  termsumn = univ3by2*(dmass**1.5d0)*dn*((dz+dn)**1.5d0)
	1*exp((dn*(etan+(potfn/temp)))+(dz*(etap+(potfp/temp)))+fee_eff)
	  termsumz=univ3by2*(dmass**1.5d0)*dz*((dz+dn)**1.5d0)
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
	  termsum_clust=univ3by2*(dmass**1.5d0)*((dz+dn)**2.5d0)
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
	dlower = (derivnn*derivzz) - (derivnz*derivnz)
	dupper_n = (funcn*derivzz) - (funcz*derivnz)
	dupper_z = -(funcn*derivzn) + (funcz*derivnn)

      if (dabs(funcn).gt.1.0e-10.and.dabs(funcz).gt.1.0e-10) then
	betamun = betamun - (dupper_n/dlower)
	betamuz = betamuz - (dupper_z/dlower)
	goto 200
	end if

c	write(*,*)"Here"
554	continue
      vol_av_req = vol_av

	vol_av = vol_av_req
	iprint = 1
ccc----------------------------------------------------
      sum_frag = 0.0d0
      sum_prot = 0.0d0
	sum_neut = 0.0d0

	vol_av_ratio = vol_av/vol_normal
      univ_v = univ3by2*vol_av
      y = univ_v*(emassp**1.5d0)*dexp(betamuz)
      
	do iz = 0, numzmax
	do in = ndriplow(iz), ndriphigh(iz)
	if(in.gt.numnmax3) goto 1299
      ia = iz + in
	diz = dfloat(iz)
	din = dfloat(in)
	dia = diz + din
	if(iz.eq.0.and.in.eq.1) then
	rhofn = univ3by2*(emassn**1.5d0)*exp(betamun)
	dlhsn = rhofn/(2.0d0*univ3by2*(emassn**1.5d0))
      call Etainv(dlhsn, etan)
	yy(0, 1) = rhofn*vol
        sum_frag = sum_frag + yy(0, 1)
        sum_prot = sum_prot + (diz*yy(0, 1))
	  sum_neut = sum_neut + (din*yy(0, 1))
	else if(iz.eq.1.and.in.eq.0) then
	rhofp = univ3by2*(emassp**1.5d0)*exp(betamuz)
	dlhsp = rhofp/(2.0d0*univ3by2*(emassp**1.5d0))
      call Etainv(dlhsp, etap)
	yy(1, 0) = rhofp*vol
        sum_frag = sum_frag + yy(1, 0)
        sum_prot = sum_prot + (diz*yy(1, 0))
	  sum_neut = sum_neut + (din*yy(1, 0))

	call dmeanfield(rhofp, rhofn,etap,etan,temp,dkin_p,dkin_n,potfp
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
	  fee_eff=fee(iz,in)
	   else if(iz.eq.1.and.in.eq.5) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.1.and.in.eq.6) then	   
	   fee_eff=fee(iz,in)
	  else if(iz.eq.2.and.in.eq.1) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.2.and.in.eq.2) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.2.and.in.eq.3) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.2.and.in.eq.4) then	   
	  fee_eff=fee(iz,in)
	  else if(iz.eq.2.and.in.eq.5) then	   
	  fee_eff = fee(iz,in)
	  else if(iz.eq.2.and.in.eq.6) then	
	  fee_eff = fee(iz, in)    
	  else if(iz.eq.2.and.in.eq.7) then	   
	  fee_eff = fee(iz,in)
	  else if(iz.eq.2.and.in.eq.8) then	   
	  fee_eff = fee(iz, in)

	  else	   
  	  delta = (dia - (2.0d0*diz))/dia
	  delta2 = delta*delta
        rho = rho_0*(1.0d0 - (3.0d0*para_Lsym*delta2)/
	1(para_Ksat + para_Ksym*delta2))
	  rhopc = (rho*diz)/dia
	  rhonc = rho - rhopc
	  fee_gas_cor1 = -((2.0d0/3.0d0)*(dkin_n + dkin_p))/temp
	  fee_gas_cor2 = poten_dens/temp
	  fee_gas_cor3n = rhofn*etan
	  fee_gas_cor3p = rhofp*etap
	  fee_gas_cor3 = fee_gas_cor3p + fee_gas_cor3n
	  fee_gas_cor = ((fee_gas_cor1+fee_gas_cor2+fee_gas_cor3)*dia)/rho
	  	
	  fee_eff = fee(iz, in) + fee_gas_cor

	  if(diz.eq.28.0.and.dia.eq.56.0)then
	  factor_extra = temp*((rhofp*etap)+(rhofn*etan))*(dia/rho)
	  write(23, 223) temp, dens_ratio, proton_frac, fee_gas_cor
	1,fee_gas_cor1, fee_gas_cor2, fee_gas_cor3
	  end if
223     format(7f9.3)
 	  end if

        yy(iz,in)=univ3by2*(dmass**1.5d0)*vol_av*((diz+din)**1.5d0)*dexp
	1((diz*(etap+(potfp/temp)))+(din*(etan+(potfn/temp)))+fee_eff)

        sum_frag = sum_frag + yy(iz, in)
        sum_prot = sum_prot + (diz*yy(iz, in))
	  sum_neut = sum_neut + (din*yy(iz, in))
c	  end if
      end if
1299  continue
      end do
	end do


	do iz = 0, numzmax
	do in = ndriplow(iz), ndriphigh(iz)
	ia = iz + in
	yyrho(iz, in) = yy(iz, in)/vol
	write(12, 112) dens_ratio, proton_frac, temp, iz, ia, yy(iz, in)
	end do
	end do

112   format(3f8.3, 2i5, 1e12.5)


	H2 = 0.0d0;		He4 = 0.0d0 
	H3 = 0.0d0;		He6 = 0.0d0 
	H5 = 0.0d0;		He8 = 0.0d0 
	H7 = 0.0d0;		He10 = 0.0d0 

	do iz = 1, 2
		if (iz.eq.1) then 
			do in = ndriplow(iz), ndriphigh(iz) 
				if (in.eq.1) then
					H2 = H2 + yyrho(iz, in)
				else if (in.eq.2) then
					H3 = H3 + yyrho(iz, in) 
				else if (in.eq.4) then 
					H5 = H5 + yyrho(iz, in) 
				else if (in.eq.6) then 
					H7 = H7 + yyrho(iz, in) 
				end if 
			end do 
		else
			do in = ndriplow(iz), ndriphigh(iz) 
				if (in.eq.2) then 
					He4 = He4 + yyrho(iz, in) 
				else if (in.eq.4) then
					He6 = He6 + yyrho(iz, in) 
				else if (in.eq.6) then 
					He8 = He8 + yyrho(iz, in) 
				else if (in.eq.8) then 
					He10 = He10 + yyrho(iz, in) 
				end if 
			end do 
		end if 
	end do 


	write(13, 111) dens_ratio, H2, H3, H5, H7 
	write(14, 111) dens_ratio, He4, He6, He8, He10 

111	format(5e12.6)			 


	! To Calculate bound oservables 
	
	sum0 = 0.0d0 
	sum1 = 0.0d0; sum2 = 0.0d0; sum3 = 0.0d0 

	do iz = 1, numzmax
	do in = ndriplow(iz), ndriphigh(iz) 
	if (in.ge.1) then 
	sum0 = sum0 + yyrho(iz, in) 
	end if
	end do 
	end do 

	do iz = 1, numzmax
	do in = ndriplow(iz), ndriphigh(iz) 
	if (in.ge.1) then
	ia = iz + in  
	sum1 = sum1 + dfloat(iz)*yyrho(iz, in)
	sum2 = sum2 + dfloat(ia)*yyrho(iz, in) 
	sum3 = sum3 + (dfloat(ia - 2.0d0*iz)/dfloat(ia))*yyrho(iz, in)
	end if   
	end do 
	end do

	z_bound = sum1/sum0 
	a_bound = sum2/sum0
	aI_bound = sum3/sum0 

	write(35, 123) dens_ratio, z_bound, a_bound, aI_bound 
123	format(4f12.6) 


	sum1 = 0.0; sum2 = 0.0	
	sumH = 0.0; sumHe = 0.0 
	sumn = 0.0; sump = 0.0

	do in = ndriplow(1), ndriphigh(1)
	a = dfloat(1 + in) 
	sumH = sumH + a*yy(1, in)/vol 
	end do 

	do in = ndriplow(2), ndriphigh(2) 
	a = dfloat(2 + in) 
	sumHe = sumHe + a*yy(2, in)/vol 
	end do 

	do iz = 3, numzmax
	do in = ndriplow(iz), ndriphigh(iz) 
	a = dfloat(iz + in) 
	sum1 = sum1 + a*yy(iz, in)/vol 
	end do 
	end do 

	do iz = 0, numzmax 
	do in = ndriplow(iz), ndriphigh(iz) 
	if (in.gt.numnmax3) goto 400 
	a = dfloat(iz + in) 
	sum2 = sum2 + a*yy(iz, in)/vol
400   continue
      end do	
	end do

	xp_heavy = yy(1, 0)/(sum2*vol) 
	xn_heavy = yy(0, 1)/(sum2*vol) 
	xlight_heavy = (sumH + sumHe)/sum2 
	x_heavy = sum1/sum2 

	write(15, 1000) dens_ratio, xp_heavy, xn_heavy, xlight_heavy, 
	1x_heavy


	free_proton = 0.0;		free_neutron = 0.0 
      hydrogen_helium = 0.0;	heavy_nucleus = 0.0

      do iz = 0, numzmax 
          if (iz.eq.0) then 								   
              free_neutron = free_neutron + yy(0, 1)
          elseif (iz.eq.1) then 
              free_proton = free_proton + yy(1, 0) 
              do in = 1, ndriphigh(iz)
                  hydrogen_helium = hydrogen_helium + yy(iz, in) 
              end do 
          elseif (iz.eq.2) then 
              do in = ndriplow(iz), ndriphigh(iz)
                  hydrogen_helium = hydrogen_helium + yy(iz, in)
              end do 
          elseif (iz.ge.3) then 
              do in = ndriplow(iz), ndriphigh(iz) 
                  if (in.gt.numnmax) goto 250 
                  heavy_nucleus = heavy_nucleus + yy(iz, in) 
              end do 
250       continue
          end if				  
      end do

	write(10, 1000) dens_ratio, free_proton/vol, free_neutron/vol,
	1hydrogen_helium/vol, heavy_nucleus/vol
                            


1000  format(5ES20.10)


	goto 4000 ! will be calculated in a different code.

	sum_fragz = 0.0d0
      do iz = 0, numzmax
	dmulz = 0.0d0
	do in = ndriplow(iz), ndriphigh(iz)
	ia = iz + in
	sum_fragz = sum_fragz + dmulz
	end do
	end do

	sum_fraga = 0.0d0
	do ia = 1, numamax
	dmula = 0.0d0
	do iz = 0, numzmax
	do in = ndriplow(iz), ndriphigh(iz)
	iia = iz + in
	if(ia.eq.iia) then
	dmula = dmula + yy(iz,in)
	end if
	end do
	end do
	sum_fraga = sum_fraga + dmula
	end do
      write(*, *)

	rhosum = 0.0d0
	aboundrhosum = 0.0d0
      do iz = 0, numzmax
	do in = ndriplow(iz), ndriphigh(iz)
	dia = dfloat(iz) + dfloat(in)
	rhosum = rhosum + yyrho(iz, in)
	if(iz.eq.1.and.in.eq.0) then
	aboundrhosum = aboundrhosum 
	else if(iz.eq.0.and.in.eq.1) then
	aboundrhosum = aboundrhosum
	else
	aboundrhosum = aboundrhosum + (dia*yyrho(iz, in))
	end if
	end do
	end do
	abound = aboundrhosum/rhosum
	dmass_frac_Alpha4 = (4.0d0*yy(2, 2))/2000.0d0
	dmass_frac_Alpha8 = (8.0d0*yy(2, 6))/2000.0d0
	dmass_frac_C12 = (12.0d0*yy(6, 6))/2000.0d0
	dmass_frac_C15 = (15.0d0*yy(6, 9))/2000.0d0
	dmass_frac_C18 = (18.0d0*yy(6, 12))/2000.0d0
	dmass_frac_O16 = (16.0d0*yy(8, 8))/2000.0d0
	dmass_frac_O20 = (20.0d0*yy(8, 12))/2000.0d0
	dmass_frac_O24 = (24.0d0*yy(8, 16))/2000.0d0

	rhopsum = 0.0d0
	zboundrhosum = 0.0d0
      do iz = 0, numzmax
	do in = ndriplow(iz), ndriphigh(iz)
	diz = dfloat(iz)
	rhopsum = rhopsum + yyrho(iz, in)
	if (iz.eq.1.and.in.eq.0) then
	zboundrhosum = zboundrhosum 
	else if(iz.eq.0.and.in.eq.1) then
	zboundrhosum = zboundrhosum
	else
	zboundrhosum = zboundrhosum + (diz*yyrho(iz, in))
	end if
	end do
	end do
	zbound = zboundrhosum/rhopsum

	zheavy_neu = 0.0d0
	zheavy_deno = 0.0d0
	xheavy_neu = 0.0d0
	xheavy_deno = 0.0d0
      do iz = 0, numzmax
	do in = ndriplow(iz), ndriphigh(iz)
	  if(iz.eq.0) then
        diz = dfloat(iz)
	  xheavy_deno = xheavy_deno + (diz*yyrho(iz, in))
	  else if(iz.eq.1) then
	  diz = dfloat(iz)
	  xheavy_deno = xheavy_deno + (diz*yyrho(iz, in))
	  else if(iz.eq.2) then	   
	  diz = dfloat(iz)
	  xheavy_deno = xheavy_deno + (diz*yyrho(iz, in))
	  else
	  diz = dfloat(iz)
	  zheavy_neu = zheavy_neu + (diz*yyrho(iz, in))
	  zheavy_deno = zheavy_deno + yyrho(iz, in)	
	  xheavy_neu = xheavy_neu + (diz*yyrho(iz, in))
	  xheavy_deno = xheavy_deno + (diz*yyrho(iz, in))
	  end if
	end do
	end do
	if (zheavy_deno.gt.0.0d0) then
	zheavy = zheavy_neu/zheavy_deno
	else
	zheavy = 0.0d0
	end if

	if (xheavy_deno.gt.0.0d0) then
	xheavy = xheavy_neu/xheavy_deno
	else
	xheavy = 0.0d0
	end if

4000	continue


c	write(*,*)sum_frag,sum_fragz,sum_fraga
c	write(*,*)sum_prot,sum_neut

	betamuz_print = etap + (potfp/temp)
	betamun_print = etan + (potfn/temp)

	write(*, 122) dens_ratio, proton_frac, temp, iter
	1, sum_prot, sum_neut

	goto 3000 ! dont want to write these files for now.

	write(11, 121) densbyrho0, proton_frac, temp, betamuz_print
	1,betamun_print, rhofp, rhofn, abound, potfp, potfn, vol_av_ratio
	write(25, 125) densbyrho0, proton_frac, temp, dmass_frac_Alpha4
	1,dmass_frac_Alpha8, dmass_frac_C12, dmass_frac_C15, dmass_frac_C18
     2,dmass_frac_O16, dmass_frac_O20, dmass_frac_O24
	write(26, 121) densbyrho0, proton_frac, temp, abound,zbound,xheavy
	1, zheavy

3000	continue 

c	write(12,121)temp,betamuz_print,etap,potfpbytemp
c	1,betamun_print,etan,potfnbytemp

122   format(3f9.4, 1i7, 2f11.5)
121   format(11f11.6)
125   format(3f11.6, 8e12.5)
	iprint = 0

	end do
	end do
	end do 

999   continue

      stop
      end