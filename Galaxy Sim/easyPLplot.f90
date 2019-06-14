module easyPLplot
  use kinds
  use PLplot
  USE IFport
  implicit none

  ! Default viewport position ( overwritten by setSquare() )                                                                                                                         
  !                                                                                                                                                                                  
  real(dp) :: vpXmin = 0.12_dp, vpXmax = 0.88_dp ! = 1.0_dp - vpXmin                                                                                                                 
  real(dp) :: vpYmin = 0.13_dp, vpYmax = 0.88_dp 
  real(dp) :: vpFrac = 0.77_dp                   ! fraction of shortest length for square                                                                                            

  !  An estimate of the number of pixels in the viewport window                                                                                                                      
  !  Provided by subroutine countPixels( )                                                                                                                                           
  !                                                                                                                                                                                  
  real(dp) :: Xpix, Ypix

  !  Variables to save the plotting limits and associate attributes                                                                                                                  
  !                                                                                                                                                                                  
  real(dp) :: xminLim, xmaxLim, yminLim, ymaxLim
  real(dp) :: xtickSav, ytickSav
  integer  :: nxsubSav, nysubSav
  character(len=:), allocatable :: xlabelSav, ylabelSav, titleSav
  logical  :: squareSav = .false.
  integer  :: colPalet


  ! Variables to support blotting and point sizes in the nBody plotting routine                                                                                                      
  !                                                                                                                                                                                  
  logical  :: firstCall = .true.
  integer  :: nPointsSave
  real(DP) :: maxSymbolSize = 1.0_DP
  real(DP), dimension(:,:), allocatable :: rSave
  real(DP), dimension(:),   allocatable :: colourSave, ptSizeSave, ptSize

  real(DP) :: basexSave , baseySave, heightSave 
  real(DP) :: xminSave, xmaxSave, yminSave, ymaxSave, zminSave, zmaxSave 
  real(DP) :: azimuthSave, altitudeSave
  character(len=:), allocatable :: titleSave, xlabelSave, ylabelSave, zlabelSave
  real(DP) :: xtickSave, ytickSave, ztickSave
  integer  :: nxsubSave, nysubSave, nzsubSave

  ! 3D Plotting Observation Point                                                                                                                                                    
  real(DP), dimension(3) :: rObserve
  real(DP)               :: max_rObserve, mod_rObserve


  ! viewport colouring and edging                                                                                                                                                    
  !                                                                                                                                                                                  
  real(dp), dimension(4) :: xbounds, ybounds
  real(dp) :: edging = 2.0_dp
  integer  :: edgingPenWidth = 2, penWidth = 3

  ! The default Fortran binding for PLplot doesn't provide named constants for                                                                                                       
  ! its coded arguments. We define some named constants here to improve the                                                                                                          
  ! readability of our code.                                                                                                                                                         

  ! Description of Symbols for PLpoin[3]                                                                                                                                             
  !                                                                                                                                                                                  
  integer, parameter :: SmallDot    =  1  ! x06f provides a table of Hershey plotting symbols for PLpoin[3]                                                                          
  integer, parameter :: BigDot      = 17  ! See /usr/share/plplot5.9.8/examples/f95/ or                                                                                              
  integer, parameter :: SolarSymbol =  9  ! or ~dleinweber/PLplot/examples/ on titan                                                                                                 
  integer, parameter :: BigCircle   = 26 


  ! Font names & numbers                                                                                                                                                             
  !                                                                                                                                                                                  
  integer, parameter :: NormalFont = 1 !  Simple, fast and good looking with a pen with of 3 in 2D                                                                                   
  integer, parameter :: RomanFont  = 2 !  Nice looking                                                                                                                               
  integer, parameter :: ItalicFont = 3 !  Available for math symbols (#fi or #fr or #fn selects the                                                                                  
  integer, parameter :: ScriptFont = 4 !  font within a character string) )                                                                                                          


  !  Default 3D Parameters                                                                                                                                                           
  !                                                                                                                                                                                  
  real(DP) :: altitude    =  20.0_DP   !  A good place to start                                                                                                                      
  real(DP) :: azimuth     = 298.0_DP   !  Selected for the Lorenz Attractor                                                                                                          
  real(DP) :: azimuthStep =   0.2_DP   !  A nice turn rate                                                                                                                           

  real(DP) :: basex  = 2.0_DP          !  These cast the world coordinates onto a 3D shape                                                                                           
  real(DP) :: basey  = 2.0_DP          !  The relative length of each dimension is important                                                                                         
  real(DP) :: height = 2.0_DP          !  Equal amounts cast the world coordinates onto a cube                                                                                       

  real(DP) :: xmax2d =  2.0_DP         !  Bounds of the 2D projected image                                                                                                           
  real(DP) :: xmin2d = -2.0_dp         !  Smaller numbers = bigger on screen                                                                                                         
  real(DP) :: ymax2d =  2.3_DP         !  These are selected with regard to the base and height lengths                                                                              
  real(DP) :: ymin2d = -0.8_DP         !  Leave room for axis labels and diagonal projections                                                                                        

  logical  :: spinIt    = .false.      !  Turn the 3D object: see azimuthStep above                                                                                                  
  logical  :: animateIt = .true.       !  Animate a 3D curve by extending the final point plotted                                                                                    
  integer  :: ptStride  = 1            !  The number of new points to introduce in the animation                                                                                     

  ! Aspect ratio codes for use with the PLenv argument just                                                                                                                          
  !                                                                                                                                                                                  
  integer, parameter :: IndependentAxes = 0
  integer, parameter :: IsotropicAxes  =  1


  ! Selected axis codes for use with PLenv                                                                                                                                           
  !                                                                                                                                                                                  
  integer, parameter :: NoAxes  = -2           ! No box or annotation.                                                                                                               
  integer, parameter :: BoxOnly = -1           ! Draw box only.                                                                                                                      
  integer, parameter :: Labels = 0             ! Draw box, labeled with coordinate values around edge.
  integer, parameter :: LabelsAndZerolines = 1 ! In addition to box and labels, draw the two axes X = 0 and Y = 0.                                                                   
  integer, parameter :: LabelsAndTicklines = 2 ! Same as axis = 1, but also draw a grid at the major tick interval.                                                                  


  ! Description of colours for use in PLcol0                                                                                                                                         
  ! Use setBlackTextOnWhite() or setWhiteTextOnBlack() or setCustomPalet() to define                                                                                                 
  !                                                                                                                                                                                  
  integer :: White
  integer :: Black
  integer :: Dark_grey, Medium_grey, Light_grey
  integer :: Brown
  integer :: Red
  integer :: Orange
  integer :: Yellow
  integer :: Green
  integer :: Green_blue
  integer :: Cyan
  integer :: Blue
  integer :: Indigo
  integer :: Magenta
  integer :: Violet

  !  Legacy colours                                                                                                                                                                  
  !                                                                                                                                                                                  
  integer :: Wheat
  integer :: Grey
  integer :: Pink
  integer :: Salmon
  integer :: Turquoise
  integer :: Aquamarine
  integer :: BlueViolet


  !  Colour palet names                                                                                                                                                              
  integer, parameter :: Default      = 0
  integer, parameter :: WhiteOnBlack = 1
  integer, parameter :: BlackOnWhite = 2
  integer, parameter :: Custom       = 3


  interface plotPoints
     module procedure plotString, plotPoin
  end interface plotPoints


contains

  subroutine setCustomPalet()

    ! Since the default colours are subject to change, we'll define our own                                                                                                          
    !                                                                                                                                                                                
    call PLspal0( "cmap0_custom.pal" )

    ! Now define the colour names                                                                                                                                                    
    !                                                                                                                                                                                
    Medium_grey =  0  ! Background                                                                                                                                                   
    Black       =  1  ! Forefground                                                                                                                                                  
    Dark_grey   =  2
    Light_grey  =  3
    White       =  4
    Brown       =  5
    Red         =  6
    Orange      =  7
    Yellow      =  8
    Green       =  9
    Green_blue  = 10
    Cyan        = 11
    Blue        = 12
    Indigo      = 13
    Magenta     = 14
    Violet      = 15

    !  Define Legacy Colours                                                                                                                                                         
    !                                                                                                                                                                                
    Wheat       = Light_grey
    Grey        = Medium_grey
    Pink        = Red
    Salmon      = Orange
    Turquoise   = Green_blue
    Aquamarine  = Green_blue
    BlueViolet  = Indigo

  end subroutine setCustomPalet


  subroutine setWhiteTextOnBlack()
    ! Since the default colours are subject to change, we'll define our own                                                                                                          
    !                                                                                                                                                                                
    call PLspal0( "cmap0_white_on_black.pal" )

    ! Now define the colour names following the default                                                                                                                              
    !                                                                                                                                                                                
    Black      =  0  ! Background                                                                                                                                                    
    White      =  1  ! Foreground                                                                                                                                                    
    Yellow     =  2
    Green      =  3
    Aquamarine =  4
    Pink       =  5
    Wheat      =  6
    Grey       =  7
    Brown      =  8
    Blue       =  9
    BlueViolet = 10
    Cyan       = 11
    Turquoise  = 12
    Magenta    = 13
    Salmon     = 14
    Red        = 15
    !                                                                                                                                                                                
  end subroutine setWhiteTextOnBlack


  subroutine setBlackTextOnWhite()
    ! Since the default colours are subject to change, we'll define our own                                                                                                          
    !                                                                                                                                                                                
    call PLspal0( "cmap0_black_on_white.pal" )

    ! Now define the colour names following the default                                                                                                                              
    !                                                                                                                                                                                
    White      =  0  ! Background                                                                                                                                                    
    Black      =  1  ! Foreground                                                                                                                                                    
    Yellow     =  2
    Green      =  3
    Aquamarine =  4
    Pink       =  5
    Wheat      =  6
    Grey       =  7
    Brown      =  8
    Blue       =  9
    BlueViolet = 10
    Cyan       = 11
    Turquoise  = 12
    Magenta    = 13
    Salmon     = 14
    Red        = 15
    !                                                                                                                                                                                
  end subroutine setBlackTextOnWhite


  subroutine setSquare( frac )
    !                                                                                                                                                                                
    !  Sets the viewport to a Square occupying fract of the shortest dimension                                                                                                       
    !                                                                                                                                                                                
    real(dp), intent(in) :: frac

    real(dp) :: spXmin, spXmax, spYmin, spYmax
    real(dp) :: Xsize, Ysize, size

    call PLgspa( spXmin, spXmax, spYmin, spYmax )

    Xsize = spXmax - spXmin
    Ysize = spYmax - spYmin
    size = min( Xsize, Ysize ) * frac
    vpXmin = (Xsize - size) / 2.0_dp
    vpXmax = vpXmin + size
    vpYmin = (Ysize - size) / 2.0_dp
    vpYmax = vpYmin + size

    call PLsvpa( vpXmin, vpXmax, vpYmin, vpYmax )
    !                                                                                                                                                                                
    ! Now set vpXmin etc to relative sizes to be consistent with PLvpor                                                                                                              
    !                                                                                                                                                                                
    vpXmin = vpXmin/Xsize
    vpXmax = vpXmax/Xsize
    vpYmin = vpYmin/Ysize
    vpYmax = vpYmax/Ysize

    ! call plvpor( vpXmin, vpXmax, vpYmin, vpYmax )  ! Already done via PLsvpa                                                                                                       

  end subroutine setSquare


  subroutine countPixels()
    !                                                                                                                                                                                
    !  How many pixels are there in the viewport window                                                                                                                              
    !                                                                                                                                                                                
    real(dp) :: xp, yp                    ! DPI values                                                                                                                               
    integer  :: xleng, yleng, xoff, yoff  ! Lengths and offsets in pixels for X windows                                                                                              

    call PLgpage( xp, yp, xleng, yleng, xoff, yoff )

    Xpix = ( vpXmax - vpXmin ) * xleng
    Ypix = ( vpYmax - vpYmin ) * yleng

  end subroutine countPixels


  subroutine areaFill( xmin, xmax, ymin, ymax, array2d, paletName )
    real(dp),                 intent(in) :: xmin, xmax, ymin, ymax  ! grid coordinates                                                                                               
    real(dp), dimension(:,:), intent(in) :: array2d                 ! data to be plotted                                                                                             
    character(len=*),         intent(in) :: paletName

    integer, parameter :: nLevels = 256
    real(dp), dimension( nLevels ) :: shedge
    character(len = 1) :: defined
    integer, parameter :: fill_width = 2
    integer, parameter :: cont_color = 0, cont_width = 1  ! contour colour (0 = no contours) and line width                                                                          

    real(dp) :: base, incr
    integer  :: i

    ! Set nLevels levels from the minimum to the maximum of array2d                                                                                                                  
    !                                                                                                                                                                                
    base = minval( array2d )
    incr = ( maxval( array2d ) - base ) / (nLevels - 1)

    do i = 1, nLevels
       shedge(i) = base + (i-1) * incr
    end do

    call PLspal1( paletName, 1 )

    call PLshades( array2d, defined, xmin, xmax, ymin, ymax, &
         &         shedge, fill_width, cont_color, cont_width )

  end subroutine areaFill


  subroutine initializePlot( palet, xpixels, ypixels, device )
    integer, intent(in)  :: palet
    integer, intent(out) :: xpixels, ypixels
    character(len=*), optional, intent(in) :: device
    colPalet = palet

    ! Initialize the window                                                                                                                                                          
    !                                                                                                                                                                                
    call PLparseopts( PL_PARSE_FULL )             ! Process command-line arguments                                                                                                   

    if ( present(device) ) call PLsdev( device )  ! Select the output device. eg. 'xwin'                                                                                             

    !  Select the colour scheme                                                                                                                                                      
    !                                                                                                                                                                                
    select case( colPalet )
    case ( WhiteOnBlack )
       call setWhiteTextOnBlack()
    case ( BlackOnWhite )
       call setBlackTextOnWhite()
    case ( Custom )
       call setCustomPalet()
    case default
       call setCustomPalet()
    end select

    call PLinit()                      ! Initialize                                                                                                                                  

    call countPixels()                 ! Estimate the number of pixels in the viewport                                                                                               

    xpixels = Xpix
    ypixels = Ypix

  end subroutine initializePlot


  subroutine setupPlot( xmin, xmax, ymin, ymax, xtick, nxsub, ytick, nysub, xlabel, ylabel, title, square )
    real(dp), intent(in) :: xmin, xmax, ymin, ymax  ! grid coordinates                                                                                                               
    real(dp), intent(in) :: xtick, ytick            ! interval for labels                                                                                                            
    integer,  intent(in) :: nxsub, nysub            ! number of subintervals per labeled                                                                                             
    character(len = *), intent(in) :: xlabel, ylabel, title   ! labels                                                                                                               
    logical, optional,  intent(in) :: square        ! demand a square plot                                                                                                           

    ! Save the square attribute if it is present                                                                                                                                     
    if ( present(square) ) squareSav = square

    ! Save the coordinate limits                                                                                                                                                     
    xminLim = xmin
    xmaxLim = xmax
    yminLim = ymin
    ymaxLim = ymax

    ! Save the xtick, nxsub, ytick, nysub info and set                                                                                                                               
    xtickSav = xtick
    nxsubSav = nxsub
    ytickSav = ytick
    nysubSav = nysub

    ! Save the labels ( note deallocation and allocation is handled automatically )                                                                                                  
    !                                                                                                                                                                                
    xlabelSav = xlabel
    ylabelSav = ylabel
    titleSav  = title

    !  Create new plot and decorate it                                                                                                                                               
    !                                                                                                                                                                                
    call PLadv( 0 )                                      ! Clear the screen 

    if ( squareSav ) then
       call setSquare( vpFrac )                          ! Ensures the plot is square                                                                                                
    else
       call plvpor( vpXmin, vpXmax, vpYmin, vpYmax )     ! Determine where the plot sits in the window                                                                               
    end if

    call PLwind( xminLim, xmaxLim, yminLim, ymaxLim )    ! Set the coordinate limits                                                                                                 

    if ( colPalet == Custom ) then
       call PLpsty( 0 )                                  ! Select the area fill pattern to use                                                                                       
       call PLcol0( White )                              ! Paint the background white                                                                                                
       xbounds(1) = xminLim
       xbounds(2) = xminLim
       xbounds(3) = xmaxLim
       xbounds(4) = xmaxLim
       ybounds(1) = yminLim
       ybounds(2) = ymaxLim
       ybounds(3) = ymaxLim
       ybounds(4) = yminLim
       call PLfill( xbounds, ybounds )
    end if

    call decorateGraph( .true. )                         ! Graph the box, axes, numbers, and labels                                                                                  

  end subroutine setupPlot


  subroutine decorateGraph( tickMarks )
    logical, intent(in) :: tickMarks
    !                                                                                                                                                                                
    !  Set attributes                                                                                                                                                                
    !                                                                                                                                                                                
    call PLcol0( 1 )               ! Select the foreground colour                                                                                                                    
    call PLfont( NormalFont )      ! Select a nice font ( RomanFont too )                                                                                                            
    call PLschr( 0.0_dp, 1.0_dp )  ! Scale the font size with the second argument                                                                                                    
    call PLwid( penWidth )         ! Make the line width a little thicker                                                                                                            

    call PLlab( xlabelSav, ylabelSav, titleSav )  ! Label the graph                                                                                                                  

    !  redraw the tickmarks                                                                                                                                                          
    !                                                                                                                                                                                
    if ( tickMarks ) then
       call PLcol0( 1 )            ! Select the foreground colour                                                                                                                    
       call PLwid( penWidth )
       call plbox('bcnst', xtickSav, nxsubSav, 'bcnstv', ytickSav, nysubSav )  ! Graph the box, axes, and numbers                                                                    
    end if

    if ( colPalet == Custom ) then
       !                                                                                                                                                                             
       !  Make the plot area pretty                                                                                                                                                  
       !
       call PLwid ( edgingPenWidth )                        ! Make the line width a little thicker                                                                                   
       call PLwind( 0.0_dp, Xpix, 0.0_dp, Ypix )            ! Reset the coordinate limits                                                                                            
       call PLcol0( Dark_grey )
       call PLjoin( edging, edging,  edging, Ypix-edging )
       call PLjoin( edging, Ypix-edging,  Xpix-edging, Ypix-edging )
       call PLcol0( Light_grey )
       call PLjoin( edging+1, edging,  Xpix-edging, edging )
       call PLjoin( Xpix-edging+1, edging,  Xpix-edging+1, Ypix-edging-1 )

       call PLwind( xminLim, xmaxLim, yminLim, ymaxLim )    ! Restore the coordinate limits                                                                                          
    end if

  end subroutine decorateGraph


  subroutine plotString(x, y, pointType, pointColour, pointSize, animate )
    real(dp), intent(in), dimension(:) :: x, y
    character(len=*), intent(in) :: pointType
    integer,  intent(in) :: pointColour
    real(dp), intent(in) :: pointSize
    logical, optional, intent(in)  :: animate

    logical :: animateIt = .false.
    integer :: i, N

    if ( present(animate) ) animateIt = animate

    call PLfont( NormalFont )
    call PLschr( 0.0_dp, pointSize )  ! Scale the font size with the second argument                                                                                                 
    call PLwid( 1 )
    call PLcol0( pointColour)
    if ( animateIt ) then
       N = size(x)
       do i = 1, N
          call PLstring( x(i:i), y(i:i), pointType )  ! Plot the data                                                                                                                
          call sleepqq(20)  ! ms, -> approx 50 points per second                                                                                                                     
       end do
    else
       call PLstring( x(:), y(:), pointType )  ! Plot the data                                                                                                                       
    end if
    call PLschr( 0.0_dp, 1.0_dp )     ! Return the font size to normal                                                                                                               

  end subroutine plotString


  subroutine plotPoin(x, y, pointType, pointColour, pointSize, animate )
    real(dp), intent(in), dimension(:) :: x, y
    integer,  intent(in) :: pointType
    integer,  intent(in) :: pointColour
    real(dp), intent(in) :: pointSize
    logical, optional, intent(in)  :: animate

    logical :: animateIt = .false.
    integer :: i, N

    if ( present(animate) ) animateIt = animate

    call PLfont( NormalFont )
    call PLschr( 0.0_dp, pointSize )  ! Scale the font size with the second argument                                                                                                 
    call PLwid( 1 )
    call PLcol0( pointColour)
    if ( animateIt ) then
       N = size(x)
       do i = 1, N
          call PLpoin( x(i:i), y(i:i), pointType )    ! Plot the data                                                                                                                
          !  You can slow the animation with the following                                                                                                                           
          !  call sleepqq(20) ! ms, -> approx 50 points per second                                                                                                                   
       end do
    else
       call PLpoin( x(:), y(:), pointType )           ! Plot the data                                                                                                                
    end if
    call PLschr( 0.0_dp, 1.0_dp )    ! Return the font size to normal                                                                                                                

  end subroutine plotPoin


  subroutine plotArray3D( xmin, xmax, ymin, ymax, zmin, zmax, &
       &                  xtick, nxsub, ytick, nysub, ztick, nzsub, &
       &                  xlabel, ylabel, zlabel, title, &
       &                  x, y, z, col, &
       &                  alt, az, &
       &                  spin, animate, &
       &                  stride )
    real(DP), intent(in) :: xmin, xmax, ymin, ymax, zmin, zmax
    real(dp), intent(in) :: xtick, ytick, ztick     ! interval for labels                                                                                                            
    integer,  intent(in) :: nxsub, nysub, nzsub     ! number of subintervals per labeled                                                                                             
    character(len = *), intent(in) :: xlabel, ylabel, zlabel, title   ! labels                                                                                                       
    real(DP), dimension(:), intent(in) :: x, y, z, col
    real(DP), optional, intent(in) :: alt, az
    logical,  optional, intent(in) :: spin, animate
    integer,  optional, intent(in) :: stride

    integer :: nPoints, iPoints
    character(len=132) :: plotTitle

    if ( present(alt) ) altitude = alt
    if ( present(az ) ) azimuth  = az
    if ( present(spin)    ) spinIt    = spin
    if ( present(animate) ) animateIt = animate
    if ( present(stride) )  ptStride  = stride

    nPoints = size(x)

    call PLadv(0)
    call PLvpor(0.0_plflt, 1.0_plflt, 0.0_plflt, 0.9_plflt)  ! Use most of the window with room for a title                                                                          
    call PLwind(xmin2d, xmax2d, ymin2d, ymax2d)
    call PLw3d(basex, basey, height, xmin, xmax, ymin, ymax, zmin, zmax, altitude, azimuth)

    call PLcol0( 1 )               ! Select the foreground colour                                                                                                                    
    call PLfont( NormalFont )      ! Select a nice font ( RomanFont too )                                                                                                            
    call PLschr( 0.0_dp, 1.0_dp )  ! Scale the font size with the second argument                                                                                                    
    call PLwid( 1 )                ! Make sure the line width is thin for 3D                                                                                                         

    call PLbox3('bnstu', xlabel, xtick, nxsub, &
         &      'bnstu', ylabel, ytick, nysub, &
         &   'bcmnstuv', zlabel, ztick, nzsub  )  ! 'bcdmnstuv' was the default                                                                                                      
    write( plotTitle, '(a,a,f6.1,a,f6.1)') title, '  Alt =', altitude, ', Az =', mod(azimuth,360.0_DP)
    call PLmtex('t', 1.0_DP, 0.5_DP, 0.5_DP, plotTitle )

    ! This is not pretty, but it allows code reuse                                                                                                                                   
    !                                                                                                                                                                                
    do                                ! return here when done animating                                                                                                              
       if ( .not. animateIt ) then
          if ( spinIt ) then
             nPoints = 360 / azimuthStep ! i.e. turn it around once                                                                                                                  
             ptStride = 1
          else
             nPoints = 2+ptStride
          end if
       end if

       do iPoints = 2+ptStride, nPoints, ptStride

          if ( spinIt ) then

             !  Plot the new frame                                                                                                                                                   
             !                                                                                                                                                                       
             azimuth = azimuth + azimuthStep
             call PLw3d(basex, basey, height, xmin, xmax, ymin, ymax, zmin, zmax, altitude, azimuth)
             call PLcol0( 1 )  ! Select the foreground colour                                                                                                                        
             call PLbox3('bnstu', xlabel, xtick, nxsub, &
                  &      'bnstu', ylabel, ytick, nysub, &
                  &   'bcmnstuv', zlabel, ztick, nzsub  )
             write( plotTitle, '(a,a,f6.1,a,f6.1)') title, '  Alt =', altitude, ', Az =', mod(azimuth,360.0_DP)
             call PLmtex('t', 1.0_DP, 0.5_DP, 0.5_DP, plotTitle )

             ! Plot the new points in a line in colour                                                                                                                               
             !                                                                                                                                                                       
             if ( animateIt ) then
                call PLlineCol3( x(1:iPoints), y(1:iPoints), z(1:iPoints), col(1:iPoints) )
             else
                call PLlineCol3( x, y, z, col )
             end if

             !  Blot out the current frame                                                                                                                                           
             !                                                                                                                                                                       
             azimuth = azimuth - azimuthStep
             !  call PLwid( 1+2 ) ! gfortran: Make the line width thicker to get a better blot                                                                                       
             call PLw3d(basex, basey, height, xmin, xmax, ymin, ymax, zmin, zmax, altitude, azimuth)
             call PLcol0( 0 )  ! Select the background colour                                                                                                                        
             call PLbox3('bnstu', xlabel, xtick, nxsub, &
                  &      'bnstu', ylabel, ytick, nysub, &
                  &   'bcmnstuv', zlabel, ztick, nzsub  )
             write( plotTitle, '(a,a,f6.1,a,f6.1)') title, '  Alt =', altitude, ', Az =', mod(azimuth,360.0_DP)
             call PLmtex('t', 1.0_DP, 0.5_DP, 0.5_DP, plotTitle )
             !  call PLwid( 1 )  ! gfortran: Restore the thin line for 3D                                                                                                            

             !  Blot the current data                                                                                                                                                
             !                                                                                                                                                                       
             call PLcol1( 0.0_DP )
             if ( animateIt ) then
                call PLline3( x(1:iPoints-ptStride), y(1:iPoints-ptStride), z(1:iPoints-ptStride) )
             else
                call PLline3( x, y, z )
             end if


             ! Now refresh anything spoiled by the blot                                                                                                                              
             !                                                                                                                                                                       
             azimuth = azimuth + azimuthStep
             call PLw3d(basex, basey, height, xmin, xmax, ymin, ymax, zmin, zmax, altitude, azimuth)
             call PLcol0( 1 )  ! Select the foreground colour                                                                                                                        
             call PLbox3('bnstu', xlabel, xtick, nxsub, &
                  &      'bnstu', ylabel, ytick, nysub, &
                  &   'bcmnstuv', zlabel, ztick, nzsub  )

             write( plotTitle, '(a,a,f6.1,a,f6.1)') title, '  Alt =', altitude, ', Az =', mod(azimuth,360.0_DP)
             call PLmtex('t', 1.0_DP, 0.5_DP, 0.5_DP, plotTitle )

          end if

          ! Plot the points in a line in colour                                                                                                                                      
          !                                                                                                                                                                          
          if ( animateIt ) then
             call PLlineCol3( x(1:iPoints), y(1:iPoints), z(1:iPoints), col(1:iPoints) )
          else
             call PLlineCol3( x, y, z, col )
          end if

          if ( spinIt ) then
             call sleepqq(20)  ! ms, -> approx 50 frames pers second                                                                                                                 
          else
             call sleepqq(10)
          end if

       end do

       !  Spin it around once when we're done animating                                                                                                                              
       !                                                                                                                                                                             
       if ( animateIt .and. spinIt ) then
          animateIt = .false.
       else
          exit
       end if

    end do

  end subroutine plotArray3D


  subroutine PLlineCol3( x, y, z, col )
    real(DP), dimension(:), intent(in) :: x, y, z, col

    integer :: n
    integer :: i

    n = size(x)

    do i = 2, n

       call PLcol1( col(i) )
       call PLline3( x(i-1:i), y(i-1:i), z(i-1:i) )

    end do

  end subroutine PLlineCol3




  subroutine initializeNbodyPlot( palet, nDim, nBody, device )
    integer, intent(in) :: palet
    integer, intent(in) :: nDim, nBody
    character(len=*), optional, intent(in) :: device
    colPalet = palet

    ! Initialize the window                                                                                                                                                          
    !                                                                                                                                                                                
    call PLparseopts( PL_PARSE_FULL )             ! Process command-line arguments                                                                                                   

    if ( present(device) ) call PLsdev( device )  ! Select the output device. eg. 'xwin'                                                                                             

    !  Select the colour scheme                                                                                                                                                      
    !                                                                                                                                                                                
    select case( colPalet )
    case ( WhiteOnBlack )
       call setWhiteTextOnBlack()
       case ( BlackOnWhite )
       call setBlackTextOnWhite()
    case ( Custom )
       call setCustomPalet()
    case default
       call setCustomPalet()
    end select

    call PLinit()                      ! Initialize                                                                                                                                  

    ! We'll work with a higher altitude this time                                                                                                                                    
    !                                                                                                                                                                                
    altitude = 25.0_DP
    azimuth  = 45.0_DP

    ! The higher altitude requires some adjustment of the 3D to 2D mapping window                                                                                                    
    !                                                                                                                                                                                
    ymax2d =  2.0_DP ! was  2.3 for Lorenz Attractor                                                                                                                                 
    ymin2d = -0.9_DP ! was -0.8 for Lorenz Attractor                                                                                                                                 

    call PLadv(0)
    call PLvpor(0.0_plflt, 1.0_plflt, 0.0_plflt, 0.9_plflt)  ! Use most of the window with room for a title                                                                          
    call PLwind(xmin2d, xmax2d, ymin2d, ymax2d)
    call PLfont( NormalFont )      ! Select a nice font ( RomanFont too )                                                                                                            
    call PLwid( 1 )                ! Make sure the line width is thin for 3D                                                                                                         

    ! Allocate the arrays for blotting                                                                                                                                               
    !                                                                                                                                                                                
    allocate ( rSave(nDim,nBody), colourSave(nBody), ptSizeSave(nBody), ptSize(nBody) )

  end subroutine initializeNbodyPlot


  subroutine endNbodyPlot()
    call PLend() ! Close plot window                                                                                                                                                 
    deallocate ( rSave, colourSave, ptSizeSave, ptSize )
  end subroutine endNbodyPlot





  subroutine plotNbodyArray3D( xmin, xmax, ymin, ymax, zmin, zmax, &
       &                       xtick, nxsub, ytick, nysub, ztick, nzsub, &
       &                       xlabel, ylabel, zlabel, title, &
       &                       r, colour, &
       &                       alt, az, &
       &                       linkSizeToColour )
    real(DP), intent(in) :: xmin, xmax, ymin, ymax, zmin, zmax
    real(dp), intent(in) :: xtick, ytick, ztick     ! interval for labels                                                                                                            
    integer,  intent(in) :: nxsub, nysub, nzsub     ! number of subintervals per labeled                                                                                             
    character(len = *), intent(in) :: xlabel, ylabel, zlabel, title   ! labels 
    real(DP), dimension(:,:), intent(in) :: r
    real(DP), dimension(:),   intent(in) :: colour
    real(DP), optional, intent(in) :: alt, az
    logical,  optional, intent(in) :: linkSizeToColour

    integer :: nPoints, iPoints, i
    logical :: invariantFrame, invariantTitle, invariantBox
    logical :: linkSizeAndColour
    character(len=132) :: plotTitle

    if ( present(alt) ) altitude = alt
    if ( present(az ) ) azimuth  = az

    if ( present(linkSizeToColour) ) then
       linkSizeAndColour = linkSizeToColour
    else
       linkSizeAndColour = .true.  ! To support the legacy treatment                                                                                                                 
    end if

    nPoints = size(colour)

    !  Consider a cube at 45 degrees with an observer at the leading edge centred on the z-axis.                                                                                     
    !  Calculate the size of the particles using the distance to the observer's position rObserve                                                                                    
    !                                                                                                                                                                                
    !  Distance from the centre to the middle of the leading edge                                                                                                                    
    !                                                                                                                                                                                
    mod_rObserve = sqrt( (xmax-xmin)**2 + (ymax-ymin)**2 ) / 2.0_DP

    !  Distance from a back corner to the observer                                                                                                                                   
    !                                                                                                                                                                                
    max_rObserve = sqrt( (xmax-xmin)**2 + (ymax-ymin)**2 + ( (zmax-zmin)/2.0_DP )**2 )
    !                                                                                                                                                                                
    !  Observer's actual position                                                                                                                                                    
    !                                                                                                                                                                                
    rObserve(1) = -mod_rObserve * sin( azimuth  * pi_DP / 180.0_DP )
    rObserve(2) = -mod_rObserve * cos( azimuth  * pi_DP / 180.0_DP )
    rObserve(3) =  mod_rObserve * sin( altitude * pi_DP / 180.0_DP )
    !                                                                                                                                                                                
    forall (i=1:nPoints) ptSize(i) = 1.0_DP - sqrt( sum( (r(:,i)-rObserve(:))**2 ) / max_rObserve**2 )


    !  Plot the new frame                                                                                                                                                            
    !                                                                                                                                                                                
    call PLw3d(basex, basey, height, xmin, xmax, ymin, ymax, zmin, zmax, altitude, azimuth)
    call PLcol0( 1 )  ! Select the foreground colour                                                                                                                                 
    call PLschr( 0.0_dp, 1.0_dp )  ! Scale the font size with the second argument                                                                                                    
    call PLbox3('bnstu', xlabel, xtick, nxsub, &
         &      'bnstu', ylabel, ytick, nysub, &
         &   'bcmnstuv', zlabel, ztick, nzsub  )
    write( plotTitle, '(a,a,f6.1,a,f6.1)') title, '  Alt =', altitude, ', Az =', mod(azimuth,360.0_DP)
    call PLmtex('t', 1.0_DP, 0.5_DP, 0.5_DP, plotTitle )
    !                                                                                                                                                                                
    ! Plot the new points                                                                                                                                                            
    !                                                                                                                                                                                
    call PLsymbolColour3D( r, colour, linkSizeToColour=linkSizeAndColour )

    if ( .not. firstCall ) then

       invariantFrame = ( basexSave == basex .and. baseySave == basey .and. heightSave == height .and. &
            & xminSave == xmin .and. xmaxSave == xmax .and. yminSave == ymin .and. ymaxSave == ymax .and. &
            & zminSave == zmin .and. zmaxSave == zmax .and. &
            & altitudeSave == altitude .and. azimuthSave == azimuth )
       invariantTitle = ( titleSave == title .and. altitudeSave == altitude .and. azimuthSave == azimuth )
       invariantBox = ( xlabelSave == xlabel .and. ylabelSave == ylabel .and. zlabelSave == zlabel .and. &
            & xtickSave == xtick .and. ytickSave == ytick .and. ztickSave == ztick .and. &
            & nxsubSave == nxsub .and. nysubSave == nysub .and. nzsubSave == nzsub )

       !                                                                                                                                                                             
       !  Blot out only as necessary to reduce flicker                                                                                                                               
       !                                                                                                                                                                             
       if ( .not. invariantFrame ) then
          !  call PLwid( 1+2 )  ! gfortran: Make the line width thicker to get a better blot                                                                                         
          call PLw3d(basexSave, baseySave, heightSave, xminSave, xmaxSave, yminSave, ymaxSave, zminSave, zmaxSave, altitudeSave, azimuthSave)
          !  call PLwid( 1 )  ! gfortran: Restore the thin line for 3D                                                                                                               
       end if
       if ( (.not. invariantFrame) .or. (.not. invariantBox) ) then
          !  call PLwid( 1+2 )  ! gfortran: Make the line width thicker to get a better blot                                                                                         
          call PLcol0( 0 )  ! Select the background colour                                                                                                                           
          call PLschr( 0.0_dp, 1.0_dp )  ! Scale the font size with the second argument                                                                                              
          call PLbox3('bnstu', xlabelSave, xtickSave, nxsubSave, &
               &      'bnstu', ylabelSave, ytickSave, nysubSave, &
               &   'bcmnstuv', zlabelSave, ztickSave, nzsubSave  )
          !  call PLwid( 1 )  ! gfortran: Restore the thin line for 3D                                                                                                               
       end if
       if ( .not. invariantTitle ) then
          call PLcol0( 0 )  ! Select the background colour                                                                                                                           
          call PLschr( 0.0_dp, 1.0_dp )  ! Scale the font size with the second argument                                                                                              
          write( plotTitle, '(a,a,f6.1,a,f6.1)') titleSave, '  Alt =', altitudeSave, ', Az =', mod(azimuthSave,360.0_DP)
          call PLmtex('t', 1.0_DP, 0.5_DP, 0.5_DP, plotTitle )
       end if

       !  Blot the previous data                                                                                                                                                     
       !                                                                                                                                                                             
       call PLsymbolColour3D( rSave(:,1:nPointsSave), colourSave(1:nPointsSave), blot=.true., linkSizeToColour=linkSizeAndColour )

       !                                                                                                                                                                             
       ! Now refresh anything spoiled by the blot                                                                                                                                    
       !                                                                                                                                                                             
       if ( (.not. invariantFrame) .or. (.not. invariantBox) .or. (.not. invariantTitle) ) then
          call PLw3d(basex, basey, height, xmin, xmax, ymin, ymax, zmin, zmax, altitude, azimuth)
          call PLcol0( 1 )  ! Select the foreground colour                                                                                                                           
          call PLschr( 0.0_dp, 1.0_dp )  ! Scale the font size with the second argument                                                                                              
          call PLbox3('bnstu', xlabel, xtick, nxsub, &
               &      'bnstu', ylabel, ytick, nysub, &
               &   'bcmnstuv', zlabel, ztick, nzsub  )

          write( plotTitle, '(a,a,f6.1,a,f6.1)') title, '  Alt =', altitude, ', Az =', mod(azimuth,360.0_DP)
          call PLmtex('t', 1.0_DP, 0.5_DP, 0.5_DP, plotTitle )
       end if

       ! Plot the new points                                                                                                                                                         
       !                                                                                                                                                                             
       call PLsymbolColour3D( r, colour, linkSizeToColour=linkSizeAndColour )

    end if

    firstCall = .false.
    nPointsSave = nPoints
    rSave(:,1:nPointsSave) = r(:,1:nPointsSave)
    colourSave(1:nPointsSave) = colour(1:nPointsSave)
    ptSizeSave(1:nPointsSave) = ptSize(1:nPointsSave)

    basexSave  = basex
    baseySave  = basey
    heightSave = height
    xminSave = xmin
    xmaxSave = xmax
    yminSave = ymin
    ymaxSave = ymax
    zminSave = zmin
    zmaxSave = zmax
    altitudeSave = altitude
    azimuthSave  = azimuth
    titleSave  = title
    xlabelSave = xlabel
    ylabelSave = ylabel
    zlabelSave = zlabel
    xtickSave = xtick
    ytickSave = ytick
    ztickSave = ztick
    nxsubSave = nxsub
    nysubSave = nysub
    nzsubSave = nzsub

    call sleepqq(20)  ! ms, -> approx 50 frames pers second                                                                                                                          

  end subroutine plotNbodyArray3D



  subroutine PLsymbolColour3D( r, colour, blot, linkSizeToColour )
    real(DP), dimension(:,:), intent(in) :: r
    real(DP), dimension(:),   intent(in) :: colour
    logical,  optional, intent(in)       :: blot
    logical,  optional, intent(in)       :: linkSizeToColour

    integer :: nPoints
    integer :: i
    logical :: blotIt, linkSizeAndColour

    if ( present(blot) ) then
       blotIt = blot
    else
       blotIt = .false.
    end if

    if ( present(linkSizeToColour) ) then
       linkSizeAndColour = linkSizeToColour
    else
       linkSizeAndColour = .true.  ! To support the legacy treatment                                                                                                                 
    end if

    nPoints = size(colour)

    if ( blotIt ) then
       call PLcol1( 0.0_DP )  ! Select the background colour                                                                                                                         
       if (linkSizeAndColour) then
          do i = 1, nPointsSave
             !call PLssym( 0.0_dp, maxSymbolSize*colourSave(i) )               ! Scale the font size with the second argument                                                        
             call PLssym( 0.0_dp, maxSymbolSize*colourSave(i)*ptSizeSave(i) )  ! Alternative for Solar Systems                                                                       
             call PLpoin3( r(1,i:i), r(2,i:i), r(3,i:i), BigDot )
          end do
       else
          do i = 1, nPointsSave
             call PLssym( 0.0_dp, maxSymbolSize*ptSizeSave(i) )                ! Scale the font size with the second argument                                                        
             call PLpoin3( r(1,i:i), r(2,i:i), r(3,i:i), BigDot )
          end do
       end if
       !     call PLstring3( r(1,i:i), r(2,i:i), r(3,i:i), 'o' )                                                                                                                     
    else
       if (linkSizeAndColour) then
          do i = 1, nPoints
             call PLcol1( colour(i) )
             !  call PLssym( 0.0_dp, maxSymbolSize*colour(i) )        ! Scale the font size with the second argument                                                                 
             call PLssym( 0.0_dp, maxSymbolSize*colour(i)*ptSize(i) ) ! Alternative for Solar Systems: use both colour and distance                                                  
             call PLpoin3( r(1,i:i), r(2,i:i), r(3,i:i), BigDot )
          end do
       else
          do i = 1, nPoints
             call PLcol1( colour(i) )
             call PLssym( 0.0_dp, maxSymbolSize*ptSize(i) )           ! Scale the font size with the second argument                                                                 
             call PLpoin3( r(1,i:i), r(2,i:i), r(3,i:i), BigDot )
          end do
          !  call PLstring3( r(1,i:i), r(2,i:i), r(3,i:i), 'o' )                                                                                                                     
       end if
    end if

  end subroutine PLsymbolColour3D


end module easyPLplot
