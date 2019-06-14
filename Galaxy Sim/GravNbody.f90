!
!  Gravitational N-body Problem
!
!  Simulates gravitational interactions between n bodies, in arbitrary dimensions
!
!  To run: ./GravNbody -geometry 1400x1000
!
!  Notes:
!  ======
!    Masses are in solar mass (Mo)
!    Distances are in parsecs (pc)
!    Time is in kilo-years (kYr)
!
!  Units:
!  ======
!    1 julian year is exactly 365.25 julian days
!    1 julian day  is 86,400 SI seconds
!    1 AU = 1.4960 x 10^11 m is the average distance of the earth from the sun
!    1 AU/day  = 1.73148 x 10^6 m/s = 1731.48 km/s
!
!    Other units % conversions defined in astronomicalParams.data
!
!  Copyright Oscar Diplock, University of Adelaide
!
MODULE ODEs
  USE kinds
  USE astronomicalParams
  implicit none

  real(DP) :: G = 1.48780389e-34_dp ! AU^3 / kg / day^2                                                                                                                              
  ! ^ not parameter, able to swap units                                                                                                                                              

  integer :: nDim = 3, nBody, nBodyClusterOne         ! Number of spatial dimensions, total bodies, bodies in larger cluster                                                         
  ! ^ not parameter to be able to use any length array in RVToY and YToRV                                                                                                            

  real(DP), dimension(:,:), allocatable :: r, v       ! Position array r(iDim,iBody) & velocity array v(iDim,iBody)                                                                  
  real(DP), dimension(:), allocatable :: m, colour, y ! Mass and colour arrays (length = iBody), y = combined r+v vectors                                                            
  integer :: i, j, k, l                               ! iteration integers                                                                                                           

  real(DP) :: a = 0.0_DP  ! Plummer radius                                                                                                                                           

CONTAINS

  ! increment positions by solving odes for velocity and acceleration                                                                                                                
  !                                                                                                                                                                                  
  SUBROUTINE derivs(t, y, dydt)
    real(DP), intent(in) :: t, y(:)
    real(DP), intent(out) :: dydt(:)
    real(DP), dimension(nDim, nBody) :: dvdt, drdt

    call YToRV(y, r, v)

    call calcAcceleration(r, m, dvdt)

    ! drdt(:,i) = net velocity on particle i                                                                                                                                         
    drdt(:,:) = v(:,:)

    call RVToY(drdt, dvdt, dydt)

  END SUBROUTINE derivs

  SUBROUTINE calcAcceleration(r, m, accel)
    real(DP), intent(in) :: r(:,:), m(:)
    real(DP), intent(out) :: accel(:,:)

    accel(:,:) = 0.0_DP

    do i = 1, size(r(1,:))

       ! Iterate over all other bodies                                                                                                                                               
       do j = 1, size(r(1,:))

          ! Skip self                                                                                                                                                                
          if (i /= j) then

             ! Find x, y, z force components per body                                                                                                                                
             do k = 1, size(r(:,1))

                ! Check for divide by zero                                                                                                                                           
                if (r(k,j) /= r(k,i)) then

                   ! 1D component of force from one body j on other body i, added to previous forces calc'd from other bodies                                                        
                   ! accel(:,:) here is placeholder for F(:,:) used in a = F/m                                                                                                       
                   accel(k,i) = accel(k,i) + G * m(i) * m(j) * (r(k,j)-r(k,i))/( (sum((r(:,j)-r(:,i))**2) + a**2)**1.5)
                end if
             end do
          end if
       end do

       ! accel(:,i) = net acceleration due to grav. force on particle i (a = F/m)                                                                                                    
       accel(:,i) = accel(:,i) / m(i)

    end do

  END SUBROUTINE calcAcceleration

  ! pack r, v into single vector y                                                                                                                                                   
  !                                                                                                                                                                                  
  SUBROUTINE RVToY(r, v, y)
    ! size(r(:,1)) = nDim, size(r(1,:)) = nBody, to allow for array sections to be sent                                                                                              

    real(DP), intent(in) :: r(:,:), v(:,:)
    real(DP), intent(out), dimension(size(r(:,1)) * size(r(1,:)) * 2) :: y(:)
    
    nDim = size(r(:,1))
    nBody = size(r(1,:))

    do i = 1, 2*nDim
       if (i <= nDim) then
          y((i-1)*nBody + 1 : i*nBody) = r(i,:)

       else if (i > nDim) then
          y((i-1)*nBody + 1 : i*nBody) = v(i-nDim,:)

       end if
    end do

  END SUBROUTINE RVToY

  ! unpack y                                                                                                                                                                         
  !                                                                                                                                                                                  
  SUBROUTINE YToRV(y, r, v)
    ! size(r(:,1)) = nDim, size(r(1,:)) = nBody, to allow for array sections to be sent                                                                                              

    real(DP), intent(in) :: y(:)
    real(DP), intent(out) :: r(:,:), v(:,:)

    nDim = size(r(:,1))
    nBody = size(r(1,:))

    do i = 1, 2*nDim
       if (i <= nDim) then
          r(i,:) = y((i-1)*nBody + 1 : i*nBody)

       else if (i > nDim) then
          v(i-nDim,:) = y((i-1)*nBody + 1 : i*nBody)

       end if
    end do

  END SUBROUTINE YToRV

END MODULE ODEs



program main
  USE kinds
  USE ODEs
  USE odeInt
  USE easyPLplot
  USE rotationMatrix
  implicit none

  ! 3D graphics variables   
  real(DP) :: xmin, xmax, ymin, ymax, zmin, zmax
  real(DP) :: alt = 25.0_DP, az = 45.0_DP
  real(DP) :: xtick = 0.0_DP, ytick = 0.0_DP, ztick = 0.0_DP  ! interval for labels                                                                                                  
  integer  :: nxsub = 0,      nysub = 0,      nzsub = 0       ! number of subintervals per label                                                                                     
  logical  :: linkSizeToColour = .true.
  character(len=132) :: title

  ! call 'increment' arguments                                                                                                                                                       

  real(DP) :: t = 0.0_DP, dt, tNew, simLength                 ! Time step dt, in range [t, simLength]                                                                                
  real(DP), dimension(:), allocatable :: yNew, yErr

  real(DP) :: E ! total energy                                                                                                                                                       

  ! Globular Cluster Variables                                                                                                                                                       
  ! ======================                                                                                                                                                           
  !                                                                                                                                                                                  
  ! rInit = intial radial size (pc)                                                                                                                                                  
  ! h = thickness relative to diameter                                                                                                                                               
  ! str = multiplier for strength of gravity                                                                                                                                         
  ! mCluster = mass of a cluster (solar masses)                                                                                                                                      
  ! rCore = core radius (pc)                                                                                                                                                         
  ! dtkYears = time step in kilo years                                                                                                                                               
  ! vFactor = multiple of initial velocities of clusters                                                                                                                              
  ! galaxyspeedfactor = relative speed between galaxies                                                                                                                              
  ! rotAngleTwo = rotation angles for second galaxy                                                                                                                                  
  ! spin = boolean for spinning view                                                                                                                                                 

  real(DP) :: rInit, h, str, mCluster, rCore, dtkYears, vFactor, galaxySpeedFactor
  real(DP), dimension(:), allocatable :: rotAngle, rotAngleTwo, displacement, vGalaxy
  logical :: spin

  call initBodies()

  ! Set coordinate limits based on radius of cluster (+20% for eccentricity)                                                                                                         
  xmin = - sqrt(sum(displacement(:)**2))

  ! make bounds square                                                                                                                                                               
  xmax = -xmin

  ymin = xmin
  ymax = xmax

  zmin = xmin
  zmax = xmax

  write(title,'("Gravitational ", i3 ,"-body problem")') nbody ! Set title                                                                                                           

  ! Initialize the graphics window after the number of bodies (nBody) is defined                                                                                                     
  ! nDim is number of dimensions = 3

  call initializeNbodyPlot( WhiteOnBlack, nDim, nBody, 'xwin' )

  !  Select a colour palet for data mapping                                                                                                                                          
  !  Value 0.0 must be the background colour of cmap0 for blotting                                                                                                                   

  call PLspal1( "GravNbody.pal", 1 )

  ! Plot & spin the initial positions                                                                                                                                                

  call plotNbodyArray3D( xmin, xmax, ymin, ymax, zmin, zmax, &
       &                 xtick, nxsub, ytick, nysub, ztick, nzsub, &
       &                 '#fix', '#fiy', '#fiz', trim(title), &
       &                 r, colour, &             ! coordinates and colour to be plotted                                                                                             
       &                 alt, az, &               ! optional altitude and azimuth                                                                                                    
       &                 linkSizeToColour )       ! optional argument to control link between point size and co                                                                      

  ! Begin incrementing and plotting each step                                                                                                                                        

  ! Set up file for conservation values                                                                                                                                              
  open(102, file="ConservedQuantities.data", status="replace")
  write(102,*) 'Total Energy of System (E = K+V)'

  do l = 1,simLength
      ! Calc total energy of the system E                                                                                                                                             
     call totalEnergy(r, v, E)

     call RVToY(r, v, y)

     call increment(size(y), y, t, dt, derivs, yNew, yErr, tNew)
     y = yNew
     t = tNew

     call YToRV(y, r, v)

     ! Graph the current position of the bodies                                                                                                                                      
     !                                                                                                                                                                               

     call plotNbodyArray3D( xmin, xmax, ymin, ymax, zmin, zmax, &
          &                 xtick, nxsub, ytick, nysub, ztick, nzsub, &
          &                 '#fix', '#fiy', '#fiz', trim(title), &
          &                 r, colour, &             ! coordinates and colour to be plotted                                                                                          
          &                 alt, az, &               ! optional altitude and azimuth                                                                                                 
          &                 linkSizeToColour )       ! optional argument to control link between point size and colour                                                               

  end do

  ! full spin after conclusion of simulation                                                                                                                                         
  do i = 1,360

     az = az + 1

     call plotNbodyArray3D( xmin, xmax, ymin, ymax, zmin, zmax, &
          &                 xtick, nxsub, ytick, nysub, ztick, nzsub, &
          &                 '#fix', '#fiy', '#fiz', trim(title), &
          &                 r, colour, &             ! coordinates and colour to be plotted                                                                                          
          &                 alt, az, &               ! optional altitude and azimuth                                                                                                 
          &                 linkSizeToColour )       ! optional argument to control link between point size and colour                                                               
  end do

  !  Finished looping, time to end                                                                                                                                                   

  close(102)

  open(103,file="finalReport.data",status="replace")
  write(103,*) "Radial Distances: "
  do i = 1, nBody
     write(103,*) sqrt(sum(r(:,i)**2)) ! Parsecs (Pc)                                                                                                                                
  end do
  write(103,*) "Speeds: "
  do i = 1, nBody
     write(103,*) sqrt(sum(v(:,i)**2))*(3.086e13)/1000/365/24/60/60 ! Pc/Kyr converted to km/s                                                                                       
  end do
  close(103)

  call endNbodyPlot()
  
  deallocate(r)
  deallocate(colour)
  deallocate(m)
  deallocate(v)
  deallocate(y)
  deallocate(yNew)
  deallocate(yErr)
  deallocate(rotAngle)
  deallocate(rotAngleTwo)
  deallocate(vGalaxy)
  deallocate(displacement)

CONTAINS

  ! Subroutine to calculate total energy of the system (E = K + V)                                                                                                                   

  subroutine totalEnergy(r, v, E)
    real(DP), intent(in) :: r(:,:), v(:,:)
    real(DP), intent(out) :: E
    real(DP) :: kinE, potE

    kinE = 0.0_DP
    potE = 0.0_DP

    do i = 1, nBody

       kinE = kinE + 0.5_DP * m(i) * sum(v(:,i)**2)

       do j = i+1, nBody

          potE = potE - G * m(i) * m(j) / sqrt(sum( (r(:,j)-r(:,i))**2 ))

       end do
    end do

    E = kinE + potE

    ! Write energy to file to check for conservation                                                                                                                                 
    write(102,'(ES18.10)') E

  end subroutine totalEnergy

  subroutine initBodies()
    real(DP) :: phi, theta, rVec, r_1, r_2, r_3, zetaPhi, zetaTheta, zetaR, vNet, vAv, L_z, r_A, r_B, rSum
    real(DP), dimension(nDim) :: rCM, vBoost
    real(DP), dimension(nDim,nDim) :: R_M
    real(DP), dimension(:,:), allocatable :: accel
    integer :: start, finish     ! markers for array sections to change for each galaxy                                                                                              

    open(110, file="DesignerCollision.data", status="old")
    read(110,*)
    read(110,*)
    read(110,*)
    read(110,*)
    read(110,*)
    read(110,*)
    read(110,*)
    read(110,*) nBody

    ! Allocate arrays, cluster variables                                                                                                                                             

    allocate( accel(nDim, nBody) )
    allocate( m(nBody) )
    allocate( r(nDim, nBody) )
    allocate( v(nDim, nBody) )
    allocate( colour(nBody) )
    allocate( y(nDim * nBody * 2) ) ! Combined array for use in dervis subroutine                                                                                                    
    allocate( yNew(nDim * nBody * 2) )
    allocate( yErr(nDim * nBody * 2) )
    allocate( rotAngle(nDim) )
    allocate( rotAngleTwo(nDim) )
    allocate( displacement(nDim) )
    allocate( vGalaxy(nDim) )

    read(110,*) nBodyClusterOne
    read(110,*) simLength
    read(110,*) rInit
    read(110,*) h
    read(110,*) str
    read(110,*) mCluster

    mCluster = mCluster*nBody
    m(:) = mCluster/nBody

    read(110,*) rCore
    read(110,*) dt
    read(110,*) vFactor
    read(110,*) rotAngle(1), rotAngle(2), rotAngle(3)
    read(110,*) rotAngleTwo(1), rotAngleTwo(2), rotAngleTwo(3)
    read(110,*) displacement(1), displacement(2), displacement(3)
    read(110,*) vGalaxy(1), vGalaxy(2), vGalaxy(3)
    read(110,*) galaxySpeedFactor
    read(110,*) spin
    close(110)

    a = rCore/sqrt(2.0_DP**(0.4) - 1.0_DP)

    do l = 1,2      ! iteration for each galaxy                                                                                                                                      
       call random_seed()

       select case (l)
       case (1)
          start = 1
          finish = nBodyClusterOne
       
       case (2)
          start = nBodyClusterOne + 1
          finish = nBody

       end select

       ! init positions                                                                                                                                                              

       do i = start, finish
          G = G_pc3_Mo_Kyear2 * str

          linkSizeToColour = .false.

          if (l == 1) then
             colour(i) = 1.0
          else if (l == 2) then
             colour(i) = 0.5
          end if

          ! positions denser near centre, otherwise spherically symmetric in disc                                                                                                    

          zetaTheta = rand()
          zetaPhi = rand()
          zetaR = rand()
          phi = 2 * pi * zetaPhi
          theta = acos(1-2*zetaTheta)
          rVec = rInit*zetaR

          r(1,i) = rVec*sin(theta)*cos(phi)
          r(2,i) = rVec*sin(theta)*sin(phi)
          r(3,i) = h * rVec*cos(theta)

          do j = 1,nDim
             rCM(j) = sum(m(start:finish)*r(j,start:finish)) / mCluster
          end do

          r(:,i) = r(:,i) - rCM(:)
       end do

       ! Black Holes override (each 1/2 the mass of their own galaxy)                                                                                                                
       m(start) = sum(m(start:finish)) * 0.5
       r(:,start) = 0.0_DP
       colour(start) = 0.3

       call calcAcceleration(r(:,start:finish), m(start:finish), accel(:,start:finish))

       ! init velocities                                                                                                                                                             

       vAv = 0.0_DP

       do i = start, finish

          vNet = sqrt( sqrt(sum(accel(:,i)**2)) * sqrt(sum(r(:,i)**2)) ) * vFactor

          v(1,i) = vNet / sqrt( 1.0_DP + (accel(1,i)/accel(2,i))**2 )
          v(2,i) = - (accel(1,i) / accel(2,i)) * v(1,i)
          v(3,i) = 0.0_DP

          ! if z-component of angular velocity is negative, swap sign of v                                                                                                           
          if (r(1,i) * v(2,i) - r(2,i) * v(1,i) < 0) then
             v(:,i) = -v(:,i)
          end if

          ! Calc average v for larger first galaxy                                                                                                                                   
          if (l == 1) then
             vAv = (1.0_DP/finish) * sqrt(sum(v(:,i)**2)) + vAv
          end if

       end do

       ! Calc boost in velocity to start angular momentum around centre of mass between galaxies                                                                                     
       if (l == 1) then
          vBoost(:) = vGalaxy(:)/sqrt(sum(vGalaxy(:)**2)) * galaxySpeedFactor * vAv
       else if (l == 2) then
          vBoost(:) = -vBoost(:)
       end if

       do i = start,finish
          v(:,i) = v(:,i) + vBoost(:)
       end do

    end do

    ! rotate and seperate galaxies                                                                                                                                                   
    ! set rotation matrix based on file parameter "rotAngle[Two]" depending on galaxy                                                                                                
    ! displace r(:,:) by vectors either with magnitude r_A || r_B, direction of displacement(:)                                                                                      

    r_A = sum(m(nBodyClusterOne+1:nBody))/sum(m(1:nBodyClusterOne))
    r_B = 1
    rSum = r_A + r_B

    r_A = r_A/rSum
    r_B = r_B/rSum

    do i = 1, nBody
       if (i <= nBodyClusterOne) then
          R_M = rotateMzyx(rotAngle(1), rotAngle(2), rotAngle(3))
          r(:,i) = r(:,i) + r_A * displacement(:)

       else if (i > nBodyClusterOne) then
          R_M = rotateMzyx(rotAngleTwo(1), rotAngleTwo(2), rotAngleTwo(3))
          r(:,i) = r(:,i) - r_B * displacement(:)

       end if

       r(:,i) = matmul(R_M(:,:), r(:,i))
       v(:,i) = matmul(R_M(:,:), v(:,i))

    end do

    deallocate(accel)

  end subroutine initBodies

end program main
