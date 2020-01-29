      program enumsaw
      parameter (maxedge=20,maxz=4,maxstep=20)
      implicit real*8 (a-h,o-z)
      dimension wk(maxstep),ce2(maxstep),ce4(maxstep),cr2(maxstep),
     c          cr4(maxstep),e2(maxstep),r2(maxstep)
      character lat(-maxedge:maxedge,-maxedge:maxedge)
      dimension lx(0:maxstep),ly(0:maxstep)
      dimension ldir(maxstep),ndr(2,maxz)
      real etime,time(2)
      data ndr/-1,0,1,0,0,-1,0,1/
      character*12 out
c
c
c
      if(iargc().ne.2) then
         write (*,*) 'Usage: ./enumsaw <nsteps> <outputfile>'
         call exit(1)
      endif

      call getarg(1, out)
      read(out, *) nstep
      if(nstep.lt.2) then
         write (*,*) 'Usage: ./enumsaw <nsteps> <outputfile>'
         write (*,*) 'nsteps needs to be > 1'
         call exit(1)
      endif

      nstep=min(nstep,maxstep)
      call getarg(2, out)
c   15 format(a12)
      do 30 i=2,nstep
         wk(i)=0.d0
         ce2(i)=0.d0
         ce4(i)=0.d0
         cr2(i)=0.d0
         cr4(i)=0.d0
   30 continue
      wk(1)=1.d0
      ce2(1)=1.d0
      ce4(1)=1.d0
      cr2(1)=1.d0
      cr4(1)=1.d0
      do 60 i=-nstep,nstep
      do 60 j=-nstep,nstep
   60 lat(i,j)='0'
      lat(0,0)='1'
      lat(1,0)='1'
      lx(0)=0
      ly(0)=0
      lx(1)=1
      ly(1)=0
      ldir(1)=1
      e2(1)=1.d0
      r2(1)=1.d0
      n=2
      ldir(2)=1
c
c
c
      t0=etime(time)
  200 continue
c
c
c
      k=ldir(n)
      i=lx(n-1)
      j=ly(n-1)
      do 300 l=k,maxz
         mx=i+ndr(1,l)
         my=j+ndr(2,l)
         if(lat(mx,my).eq.'0') then
            e2(n)=dfloat(mx)**2+dfloat(my)**2
            r2(n)=r2(n-1)+e2(n)
            do 260 k1=1,n-1
  260       r2(n)=r2(n)+dfloat(mx-lx(k1))**2+dfloat(my-ly(k1))**2
            wk(n)=wk(n)+1.d0
            ce2(n)=ce2(n)+e2(n)
            ce4(n)=ce4(n)+e2(n)**2
            cr2(n)=cr2(n)+r2(n)
            cr4(n)=cr4(n)+r2(n)**2
            if(n.lt.nstep) then
               ldir(n)=l+1
               lat(mx,my)='1'
               lx(n)=mx
               ly(n)=my
               n=n+1
               ldir(n)=1
               go to 200
            endif
         endif
c
c
c
  300 continue
      lat(i,j)='0'
      n=n-1
      if(n.le.1) go to 400
      go to 200
c
c
c
  400 t1=etime(time)
      ntrue=0
      do 50 i=1,nstep
         if(wk(i).eq.0) go to 70
         ntrue=ntrue+1
         v=1.d0/dfloat(ntrue)**2
         w=1.d0/wk(i)
         ce2(i)=ce2(i)*w
         sde=ce4(i)*w-ce2(i)**2
         if(sde.lt.0) sde=0.d0
         ce4(i)=dsqrt(sde)
         cr2(i)=cr2(i)*v*w
         sdr=cr4(i)*w*v**2-cr2(i)**2
         if(sdr.lt.0) sdr=0.d0
         cr4(i)=dsqrt(sdr)
   50 continue
   70 open(1,file=out)
      rewind(1)
      write(6,24)
   24 format(/1x,'steps',2x,'ms r_ee',9x,'sd',10x
     c         ,'ms r_g',10x,'sd',11x,'#wks/4')
      do 20 i=1,ntrue
         write(6,25) i,ce2(i),ce4(i),cr2(i),cr4(i),wk(i)
         write(1,25) i,ce2(i),ce4(i),cr2(i),cr4(i),wk(i)
   20 continue
   25 format(1x,i3,1x,1p,5(e12.5,2x))
      write(6,35) t1-t0
   35 format(/1x,'CPU time (sec) = ',1p,e12.5)
      stop 
      end
