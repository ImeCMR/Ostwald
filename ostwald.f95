!  This program will calculate equilibrium constant(Kc) and Equivalent conductivity at C = 0 using experimental data 
!
  program ostwald
  Implicit none
!
!  Defining variables
!
  integer, parameter:: n=8
  real, dimension(n):: x, y
  integer:: i
  real:: conc, lamdc, a0, a1, yfit, lamd0, Kc, sumx=0.0, sumy=0.0, sumxy=0.0, sumxx=0.0
!
! Open input file and creating output file
!
  open(unit=10, file ='input.dat', status='old')
  open(unit=20, file ='output.out', status='unknown')
!
! Reading the input file and calculate x, y, and summation values
!
  do i = 1,n
    read(10,*) conc, lamdc
    x(i)  = conc*lamdc*0.001
    y(i)  = 1.0/lamdc
    sumx  = sumx + x(i)
    sumy  = sumy + y(i)
    sumxy = sumxy + (x(i)*y(i))
    sumxx = sumxx + (x(i)**2)
  end do
!
! Calculating a1 and a0
!
  a1 = (n*sumxy - sumx*sumy)/(n*sumxx - sumx**2)
  a0 = (sumy - a1*sumx)/n
!
! Write values in to output file
!
  write(20,*) 'Slope                     = ', a1
  write(20,*) 'Intercept                 = ', a0
!
! Calculatng equilibrium constatnt (Kc) and Equivale conductivity at conc = 0 
! Write into output file
!
  lamd0 = 1.0/a0
  Kc    = (a0**2)/a1
  write(20,*) 'Equilibrium constant (Kc) = ', Kc
  write(20,*) 'Lamda 0                   = ', lamd0
!
! Calculating yfit values and write x, y and yfit values into output file
!
  do i = 1,n
    yfit = a0 + a1*x(i)
    write(20,'(3f12.5)') x(i), y(i), yfit
  end do
!
!
!
  write(6,*) '==============================================='
  write(6,*) '                                               '
  write(6,*) '           Your values are ready !             '
  write(6,*) '            Open output.out file               '
  write(6,*) '                                               '
  write(6,*) '==============================================='

end program ostwald




