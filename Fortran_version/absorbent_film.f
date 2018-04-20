      program N2X2
      real t,co,f,q,d1,d2,n,x
      parameter(pi=3.1415926)
      common f,d1,d2,q,e1,e3
      data h1,h2,a1,a2/2*0.001,2*0.0001/
      write(*,*) 'Input e1,e3/*.,-*./'
      read(*,*)e1,e3
      open(2,file="N2X2.dat")
      write(2,334)
      write(*,*) 'Input layer thicknesses d1 and d2'
      read(*,*) d1,d2
 55   write(*,*) 'Input angle of incidence F'
      read(*,*)FF
      write(*,*) 'Input experimental data COS and TAN'
      read(*,*)co,t
 56   write(*,*) 'Input approximate values N0,X0'
      read(*,*)n,x
      f=ff*pi/180
      i=1
 10   continue
      e2=cmplx(n,-x)
      call FZU(z0,u0,e2)
      if(ABS(t-z0).le.a1.and.ABS(co-u0).le.a2) goto 20
!     *************************************************************
      te2=cmplx(n+h1,-x)
      call FZU(dz1,du1,e2)
      e2=cmplx(n-h1,-x)
      call FZU(dz2,du2,e2)
      dz=(dz1-dz2)/2./h1
      du=(du1-du2)/2./h2
      e2=cmplx(n,-x-h2)
!     *************************************************************
      call FZU(dz1,du1,e2)
      e2=cmplx(n,-x+h2)
      call FZU(dz2,du2,e2)
      xz=(dz1-dz2)/2./h2
      xu=(du1-du2)/2./h2
!     ***********Calculation of determinants***********************
      det0=dz*xu-xz*du
      det1=(t-z0)*xu-(co-u0)*xz
      det2=dz*(co-u0)-(t-z0)*du
      dd1=det1/det0
      dd2=det2/det0
!     *************************************************************
      n=n+dd1
      x=x+dd2
!     *************************************************************
      if(n.lt.0) goto 25
      if(x.lt.0) goto 25
      i=i+1
      if(i.eq.20) goto 25
      goto 10
 25   write(*,*) 'No solution!!! n2=',n,'x2=',x,'i=',i
!     ******************Deriving of results**************************
 20   continue
      write(*,333)
      write(2,100)ff,co,t,n,x,i
      write(*,100)ff,co,t,n,x,i
 333  format(3x,'F',10x,'COS(^)',8x,'TAN(ksi)',6x,'N',10x,'X',6x,'i')
 334  format(3x,'F',12x,'COS(^)',11x,'TAN(ksi)',9x,'N2',12x,'X2',8x,'i')
 100  format(1x,f5.2,2(2x,f12.5),2(2x,f9.4),2x,i3)
      write(*,*) "Continue? 1, new approxim. values N0 and X0 2, stop 0"
      read(*,*)j
      if(j.eq.2) goto 56
      if(j.eq.1) goto 55
      stop
      end program N2X2
!     ***********Calculation of ellipsometric parameters***********
            SUBROUTINE FZU(z,u,e2)
      real f,q,d1,d2,ksi,delta
      complex dl1,dl2,e1,e2,e3,cf1,cf2,cf3
      complex r01p,r12p,r23p,r01s,r12s,r23s
      complex r,rs,rp,ex1,ex2,ex3
      parameter (pi=3.1415926)
      common f,d1,d2,q,e1,e2
      cf1=CSQRT(1-sin(f)**2/(e1*e1))
      cf2=CSQRT(1-sin(f)**2/(e2*e2))
      cf3=CSQRT(1-sin(f)**2/(e3*e3))
      r01p=(e1*cos(f)-cf1)/(e1*cos(f)+cf1)
      r12p=(e2*cf1-e1*cf2)/(e2*cf1+e1*cf2)
      r23p=(e3*cf2-e2*cf3)/(e3*cf2+e2*cf3)
      r01s=(cos(f)-e1*cf1)/(cos(f)+e1*cf1)
      r12s=(e1*cf1-e2*cf2)/(e1*cf1+e2*cf2)
      r23s=(e2*cf2-e3*cf3)/(e2*cf3+e3*cf3)
      dl1=2*pi/q*d1*CSQRT(e1*e1-sin(f)**2)
      ex1=CEXP(2*(0.,-1.)*dl1)
      dl2=2*pi/q*d2*CSQRT(e2*e2-sin(f)**2)
      ex2=CEXP(2*(0.,-1.)*dl2)
      ex3=CEXP(2*(0.,-1.)*(dl1+dl2))
!     ---------------------------------------------------------------
      rp=r01p+r12p*ex1+r01p*r12p*r23p*ex2+r23p*ex3
      rp=rp/(1+r01p*r12p*ex1+r12p*r23p*ex2+r01p*r23p*ex3)
      rs=r01s+r12s*ex1+r01s*r12s*r23s*ex2+r23s*ex3
      rs=rs/(1+r01s*r12s*ex1+r12s*r23s*ex2+r01s*r23s*ex3)
      r=rp/rs
      z=CABS(r)
      u=REAL(r)/z
      ksi=ATAN(z)*180/pi
      delta=ACOS(u)*180/pi
      end SUBROUTINE FZU
