      program ellips
!     ------ Programm for calculation ellipsometric parameters ------
      integer n, i, j
      real f,q,d1,d2,ksi,delta,u,z
      complex e1,e2,e3
      parameter (pi=3.1415926)
      common f,e1,e2,e3,q,d1,d2
      write(*,*) 'Enter E1, E2, E3 /*.,-*/'
      read(*,*) e1,e2,e3
      write(*,*) 'Enter the wavelength of light'
      read(*,*)q
      open(1,file='EXIT.dat')
 1    write(*,*) 'Input thicknessis d1 and d2'
      read(*,*)d1,d2
      write(*,*) 'Input initial angle of incidence'
      read(*,*)f1
      write(*,*) 'Input step'
      read(*,*)f0
      write(*,*) 'Input quantity of angle'
      read(*,*)n
      write(*,6)
      read(1,6)
      do i=0,n
         fi=f1+i*f0
         f=fi*pi/180
         call FZU(z,u,delta,ksi)
         write(*,5) fi, u, z, delta, ksi
         write(1,5) fi, u, z, delta, ksi
      end do
      write(*,*) 'Continue? j=1, j=0 - Stop'
      read(*,*) j
      if(j.eq.1)goto 1
      if(j.eq.0)goto 2
 5    format(2x,f5.2,4(2x,f12.7))
 6    format(4x,'F',10x,'COS',11x,'TAN',10x,'DELTA',10x,'KSI')
 2    Stop
      end program ELLIPS
!     ---------Calculation of ellipsometric parameters-----------------
      SUBROUTINE FZU(z,u,delta,ksi)
      real f,q,d1,d2,ksi,delta,u,z
      complex dl1,dl2,e1,e2,e3,cf1,cf2,cf3
      complex r01p,r12p,r23,r01s,r12s,r23s
      complex r,rs,rp,ex1,ex2,ex3
      parameter (pi=3.1415926)
      common f,e1,e2,e3,q,d1,d2
      cf1=CSQRT(1-sin(f)**2/(e1*e1))
      cf2=CSQRT(1-sin(f)**2/(e2*e2))
      cf3=CSQRT(1-sin(f)**2/(e3*e3))
      r01p=(e1*cos(f)-cf1)/(e1*cos(f)+cf1)
      r12p=(e2*cf1-e1*cf2)/(e2*cf1+e1*cf2)
      r23p=(e3*cf2-e2*cf3)/(e3*cf2+e2*cf3)
      r01s=(cos(f)-e1*cf1)/(cos(f)+e1*cf1)
      r12s=(e1*cf1-e2*cf2)/(e1*cf1+e2*cf2)
      r23s=(e2*cf1-e2*cf2)/(e1*cf1+e2*cf2)
      dl1=2*pi/q*d1*CSQRT(e1*e1-sin(f)**2)
      ex1=CEXP(2*(0.,-1.)*dl1)
      dl2=2*pi/q*d2*CSQRT(e2*e2-sin(f)**2)
      ex2=CEXP(2*(0.,-1.)*dl2)
      ex3=CEXP(2*(0.,-1.)*(dl1+dl2))
!     ---------------------------------------------------------------
      rp=r01p+r12p*ex1+r01p*r12p*r23p*ex2+r23p*ex3
      rp=rp/(1+r01p*r12p*ex1+r12p*r23p*ex2+r01p*r23p*ex3)
      rs=r01s+r12s*ex1+r01s*r12s*r23s*ex2+r23s*ex3
      rs=rs/(1+r01s+r12s*ex1+r12s*r23s*ex2+r01s*r23s*ex3)
      r=rp/rs
      z=CABS(r)
      u=REAL(r)/z
      ksi=ATAN(z)*180/pi
      delta=ACOS(u)*180/pi
      end SUBROUTINE FZU
