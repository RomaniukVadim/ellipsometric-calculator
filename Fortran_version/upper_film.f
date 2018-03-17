      program n1d1
      real t,f,n1,q,d1,d2,g,co,a1,a2
      complex e1,e2,e3
      parameter(pi=3.1415926)
      common f,e2,e3,q,d2
      write(*,*) 'Input absorption index x1'
      read(*,*) x1
      data h1,h2,a1,a2/2*0.0001,2*0.0001/
      write(*,*) 'Input e2,e3/*.,-*/'
      read(*,*)e2,e3
      write(*,*) 'Input length of wave'
      read(*,*)q
      write(*,*) 'Input d2'
      open(2,file='N1D1.dat')
      write(2,334)
 58   write(*,*) 'Input angle of incidence'
      read(*,*)n1,d1
      f=ff*pi/180
      i=1
 10   Continue
      e1=cmplx(n1,-x1)
      call FZU(z0,u0,e1,d1)
      p=t-z0
      s=co-u0
      if(ABS(p).LE.a1.AND.ABS(s).LE.a2) goto 20
!     ******************************************************************
      g=n1+h1
      e1=cmplx(g,-x1)
      call FZU(dz1,du1,e1,d1)
      g=n1-h1
      e1=cmplx(g,-x1)
      call FZU(dz2,du2,e1,d1)
      dz=(dz1-dz2)/2./h1
      du=(du1-du2)/2./h2
!     ------------------------------------------------------------------
      n1-g+h1
      e1=cmplx(n1,-x1)
      g=d1+h2
      call FZU(dz1,du1,e1,g)
      g=d1-h2
      call FZU(dz2,du2,e1,g)
      xz=(dz1-dz2)/2./h2
      xu=(du1-du2)/2./h2
!     -------------------Calculation of determitants--------------------
      det0=dz*xu-xz*du
      det1=p*xu-s*xz
      det2=dz*s-p*du
      dd1=det1/det0
      dd2=det2/det0
      d1=g+h2
!     -----------------------------------------------------------------
      n1=n1+dd1
      d1=d1+dd2
      if(n1.lt.0) goto 25
      if(d1.lt.0) goto 25
!     -----------------------------------------------------------------
      i=i+1
      if(i.gt.50) goto 15
      goto 10
 15   write(*,*) 'i=50 !!!'
 25   write(*,*) 'No solution n1=',n1,'d1=',d1
!     ---------------Deriving of results--------------------------------
 20   Continue
 333  format(3x,'F',9x,'COS(^)',8x,'TAN(ksi)',6x,'N1',10x,'d1',4x,'i')
 334  format(3x,'F',12x,'COS(^)',11x,'TAN(ksi)',9x,'N1',12x,'d1',9x,'i')
 100  format(1x,f5.2,2(2x,f12.7),2(2x,f9.4),2x,i3)
      write(*,333)
      write(2,100)ff,co,t,n1,d1,i
      write()ff,co,t,n1,d1,i
      write(*,*) 'Repeat-1, new approximate values N01 and D01-2, fin-0'
      read(*,*)j
      if(j.eq.1) goto 58
      if(j.eq.2) goto 55
      Stop
      end program n1d1
!     --------------Calculation of ellipsometric parameters-------------
      SUBROUTINE FZU(z,u,delta,ksi)
      real ff,q,d1,d2,ksi,delta,u,z
      complex dl1,dl2,e1,e2,e3,cf1,cf2,cf3
      complex r01p,r12p,r23p,r01s,r12s,r23s
      complex r,rs,rp,ex1,ex2,ex3
      parameter (pi=3.1415926)
      common ff,e1,e2,e3,q,d1,d2
      f=ff*pi/180
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
