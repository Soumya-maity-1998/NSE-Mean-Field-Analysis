cccc  Grand canonical model for neutron and proton
cccc	Original Date-02.04.2019 
c	For Zero Mean Field : 06.04.2026
c	To Study for Standard Drip Line only.

	use functions
      implicit real *8 (a-h, o-z)

	real(8), dimension(0:1000, 0:1000) :: yy, yyrho
	real(8), dimension(0:1000, 0:1000) :: fee !, etap_c, etan_c 
	integer, dimension(0:1000) :: ndriplow, ndriphigh


	open(unit = 1111, file = 'test_result1.out', status = 'unknown')
	open(unit = 1112, file = 'test_result2.out', status = 'unknown')
	open(unit = 1113, file = 'test_result3.out', status = 'unknown')
	open(unit = 1114, file = 'test_temp_detB.out', status = 'unknown')
	open(unit = 1115, file = 'test_roots.out', status = 'unknown')  

      open(unit = 4, file = 'limz_.out', status = 'unknown')
	open(unit = 15, file = 'Mass_Fraction_vs_Temp_NoMeanField.out',  
	1status = 'unknown')
	open(unit = 35, file = 'Bound_vs_Temp_NoMeanField.out', 
	1status = 'unknown')
	open(unit = 10,file= 'Density_vs_Temp_yp=0.2_rhoB=0.1_NoMeanField
     1.out',status = 'unknown')
	open(unit = 12, file = 'isotopic_Temp_dependence_yp=0.2_rhoB=0.1_
	1NoMeanField.out', status = 'unknown')
	open(unit = 13, file = 'Hydrogen_isotope_Temp_NoMeanField.out',  
	1 status = 'unknown') 
	open(unit = 14, file = "Helium_isotope_Temp_NoMeanField.out",  
	1status = 'unknown')
      
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


	numamax = 400
	
	call dripline_arrays(ndriplow, ndriphigh)

	do i = 0, 100
	write(4, '(3i8)') i, ndriplow(i), ndriphigh(i)
	end do

cccc ------------------------------------------------------------------
cccc   Guess value of BetaMu 
cccc ------------------------------------------------------------------
      betamuz = -0.2d0
      betamun = -0.2d0


cccc ------------------------------------------------------------------
cccc  Inputs: Density, temperature and proton fraction 
cccc ------------------------------------------------------------------


	dyp = 0.01d0  
c	do iyp = 0, 0 ! end points = 0, 59
	proton_frac = 0.2d0
c	proton_frac = proton_frac - dfloat(iyp)*dyp

	tempi = 16.0d0
      do itemp = 1, 131
	temp = tempi - dfloat(itemp - 1)*0.1d0

777 	x0 = betamun ! for each temp, using previous root instead of
	y0 = betamuz ! initial guess. 

c	do idens = 0, 0     !50, 10, -1
      dens_ratio = 0.1d0	!rhoB/rho0
c	dens_ratio = 10.0d0**(-0.1d0*dfloat(idens))


	write(*,'(3f8.4)') temp, x0, y0
	write(*,*) " enters calc1"

	
	call calc1(x0, y0, temp, proton_frac, dens_ratio, derivnn, derivnz
	1, derivzn, derivzz, funcn, funcz, fee) 

	write(*,*) temp, " leaves calc1"

	f0 = funcn
	g0 = funcz
	 
      aj11 = derivnn	! Matrix elements of J0.
	aj12 = derivnz
	aj21 = derivzn
	aj22 = derivzz
	
      b11 = aj11
	b12 = aj12
	b21 = aj21
	b22 = aj22 

	iter = 0
	
222   detB = (b11*b22 - b12*b21)
	write(1114, '(1f8.2, 1e20.5)') temp, detB
	
      if (detB.lt.10.0e-15) goto 777
	
      sx = (-f0*b22 + g0*b12)/detB 
	sy = (+f0*b21 - g0*b11)/detB

	write(1111,'(1f8.2, 7e12.2)') temp, b11, b12, b21, b22, 
	1b11*b22, b12*b21, detB
	
      x1 = x0 + sx 
	y1 = y0 + sy

	write(1112,'(1f8.2, 2f10.4, 2e18.6)') temp, x1, y1, sx, sy
	write(1113,'(1i8, 1f8.2)') iter, temp
	 
	write(*,*) temp, " enters calc2"
	
c	iter = 0 
      call calc2(x1, y1, temp, proton_frac, dens_ratio, funcn, funcz,
	1vol_av)

	write(*,*) temp, " leaves calc2"

	iter = iter + 1 
	
      f1 = funcn
	g1 = funcz 

	if (((f1*f1 + g1*g1)**0.5).le.10e-9) goto 2233

	z1 = (f1 - f0)
	z2 = (g1 - g0)
	
c     write(*,'(1f8.2, 2e12.3)') proton_frac, f1, g1 
c	goto 999

	t1 = (b11*sx + b12*sy)
	t2 = (b21*sx + b22*sy) 

	u1 = (z1 - t1) 
	u2 = (z2 - t2) 
	
c	write(*,'(4e12.3)') t1, t2, u1, u2 
c	goto 999
	snorm = (sx*sx + sy*sy)
	
	del_b11 = (u1*sx)/snorm 
	del_b12 = (u1*sy)/snorm
	del_b21 = (u2*sx)/snorm 
	del_b22 = (u2*sy)/snorm 

	b11 = b11 + del_b11 
	b12 = b12 + del_b12
	b21 = b21 + del_b21 
	b22 = b22 + del_b22

	f0 = f1 
	g0 = g1 

	x0 = x1
	y0 = y1 

	goto 222 
	
c      call calc2(x1, y1, temp, proton_frac, dens_ratio, funcn, funcz)

c	err = (f2*f2 + g2*g2)**0.5 
	
2233	continue  

	betamun = x1
	betamuz = y1

	write(1115, '(1f8.2, 4f15.4)') temp, x1, y1, x1*temp, y1*temp

c	goto 888



c	write(*, '(1i8, 3f8.2)') iter, proton_frac, temp, dens_ratio

c	goto 999
554	continue
	numa = 2000
c	numnmax = 300
      vol_av_req = vol_av

	vol_av = vol_av_req
	iprint = 1
	
ccc  ----------------------------------------------------
	
	volf_ratio = 1.0d0/dens_ratio
	vol = volf_ratio*dfloat(numa)/0.1604d0
      vol_normal = dfloat(numa)/0.1604d0

	densbyrho0 = 1.0d0/volf_ratio
	dens = 0.1604d0/volf_ratio
      rho_electron = dens*proton_frac

	univ = (2.0d0*pi*temp)/(1240.0d0*1240.0d0)
	univ3by2 = univ**1.5d0
	univ5by2 = univ**2.5d0

      sum_frag = 0.0d0
      sum_prot = 0.0d0
	sum_neut = 0.0d0
	
c	 forgot to include. included on 9th July, 1:43pm

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
c	 ------------------------------------------ 

	vol_av_ratio = vol_av/vol_normal
      univ_v = univ3by2*vol_av
      y = univ_v*(emassp**1.5d0)*dexp(betamuz)
      
	do iz = 0, numzmax
	do in = ndriplow(iz), ndriphigh(iz)
	if(in.gt.numnmax) goto 1299
      ia = iz + in
	diz = dfloat(iz)
	din = dfloat(in)
	dia = diz + din

	if (iz.eq.0.and.in.eq.1) then
	rhofn = univ3by2*(emassn**1.5d0)*dexp(betamun)
	dlhsn = rhofn/(2.0d0*univ3by2*(emassn**1.5d0))
      call Etainv(dlhsn, etan)
	yy(0, 1) = rhofn*vol
        sum_frag = sum_frag + yy(0, 1)
        sum_prot = sum_prot + (diz*yy(0, 1))
	  sum_neut = sum_neut + (din*yy(0, 1))

	else if (iz.eq.1.and.in.eq.0) then
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
	  if (iz.eq.1.and.in.eq.1) then
	  fee_eff = fee(iz, in)
	  else if (iz.eq.1.and.in.eq.2) then	   
	  fee_eff = fee(iz, in)
	  else if (iz.eq.1.and.in.eq.3) then	   
	  fee_eff = fee(iz, in)
	  else if (iz.eq.1.and.in.eq.4) then	   
	  fee_eff = fee(iz, in)
	  else if (iz.eq.1.and.in.eq.5) then	   
	  fee_eff = fee(iz, in)
	  else if (iz.eq.1.and.in.eq.6) then	   
	  fee_eff = fee(iz, in)
	  else if (iz.eq.2.and.in.eq.1) then	   
	  fee_eff = fee(iz, in)
	  else if (iz.eq.2.and.in.eq.2) then	   
	  fee_eff = fee(iz, in)
	  else if (iz.eq.2.and.in.eq.3) then	   
	  fee_eff = fee(iz,in)
	  else if (iz.eq.2.and.in.eq.4) then	   
	  fee_eff = fee(iz, in)
	  else if (iz.eq.2.and.in.eq.5) then	   
	  fee_eff = fee(iz, in)
	  else if (iz.eq.2.and.in.eq.6) then	
	  fee_eff = fee(iz, in)    
	  else if (iz.eq.2.and.in.eq.7) then	   
	  fee_eff = fee(iz, in)
	  else if (iz.eq.2.and.in.eq.8) then	   
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

	  if (diz.eq.28.0.and.dia.eq.56.0) then
	  factor_extra = temp*((rhofp*etap) + (rhofn*etan))*(dia/rho)
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

c	goto 999

	do iz = 0, numzmax
	do in = ndriplow(iz), ndriphigh(iz)
	ia = iz + in
	yyrho(iz, in) = yy(iz, in)/vol
	write(12, 112) dens_ratio, proton_frac, temp, iz, ia, yy(iz, in)
	end do
	end do

112   format(3f8.3, 2i5, 1e12.5)

	goto 9999


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


	write(13, 111) proton_frac, H2, H3, H5, H7 
	write(14, 111) proton_frac, He4, He6, He8, He10 

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

	write(35, 123) proton_frac, z_bound, a_bound, aI_bound 
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
	if (in.gt.numnmax) goto 400 
	a = dfloat(iz + in) 
	sum2 = sum2 + a*yy(iz, in)/vol
400   continue
      end do	
	end do

	xp_heavy = yy(1, 0)/(sum2*vol) 
	xn_heavy = yy(0, 1)/(sum2*vol) 
	xlight_heavy = (sumH + sumHe)/sum2 
	x_heavy = sum1/sum2 

	write(15, 1000) proton_frac, xp_heavy, xn_heavy, xlight_heavy, 
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

	write(10, 1000) proton_frac, free_proton/vol, free_neutron/vol,
	1hydrogen_helium/vol, heavy_nucleus/vol

1000  format(5ES20.10)

	betamuz_print = etap + (potfp/temp)
	betamun_print = etan + (potfn/temp)

9999	continue

c	write(*, 122) dens_ratio, proton_frac, temp, iter
c	1, sum_prot, sum_neut

122   format(3f9.4, 1i7, 2f11.5)
121   format(11f11.6)
125   format(3f11.6, 8e12.5)
	iprint = 0
 
999   continue
	   
	end do
c	end do
c	end do 


c999   continue
      stop
      end 