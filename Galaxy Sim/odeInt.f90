!  5th order Runge-Kutta Cash-Karp Ordinary Differential Equation (ODE)
!  Initial Value Problem (IVP) integrators
!  Variable step size algorithms are also provided.
!
!  Throughout:
!  x is the independent integration variable
!  y is a vector of dependent variables
!  dydx is a vector of derivatives dy/dx assigned in subroutine derivs
!
!  h = dx is the step size
!
MODULE odeInt
  USE kinds
  USE RungeKutta
  implicit none

  INTERFACE increment
     MODULE PROCEDURE increment, varStep
  END INTERFACE increment

CONTAINS

  ! A 5th order Runge-Kutta Cash-Karp algorithm with fixed step size h
  ! Uncertainties estimated by differences in 4th and 5th order
  !
  SUBROUTINE increment(n, y, x, h, derivs, yNew, yErr, xNew)
    implicit none
    INTEGER,  intent(in)  :: n
    REAL(dp), intent(in)  :: y(n), x, h
    REAL(dp), intent(out) :: yNew(n), yErr(n), xNew
    INTERFACE
       SUBROUTINE derivs(x, y, dydx)
         USE kinds
         implicit none
         REAL(dp), intent(in)  :: x, y(:)
         REAL(dp), intent(out) :: dydx(:)
       END SUBROUTINE derivs
    END INTERFACE
    !
    ! Local Variables
    !
    REAL(dp)    ::  dydx(n)

    ! rkck does not calculate a derivative at t to save compute time in
    ! variable step-size implementations where the step-size may be
    ! found to be too big.
    !
    CALL derivs(x, y, dydx)
    CALL rkck(y, dydx, n, x, h, yNew, yErr, derivs)
    xNew = x + h

  END SUBROUTINE increment

  ! A 5th order Runge-Kutta Cash-Karp algorithm with variable step size h
  ! Uncertainties estimated by differences in 4th and 5th order
  ! The accuracy for each dynamical variable is specified in yErrReq
  !
  SUBROUTINE varStep(n, y, x, hTry, derivs, yErrReq, yNew, yErr, xNew, hDid, hNext, successful)
    implicit none
    INTEGER,                intent(in)  :: n           ! Number of dynamical variables
    REAL(dp), dimension(n), intent(in)  :: y           ! Current values of the variables
    REAL(dp),               intent(in)  :: x, hTry     ! Current position and trial step size
    INTERFACE
       SUBROUTINE derivs(x, y, dydx)                   ! Derivatives of the variables
         USE kinds
         implicit none
         REAL(dp), intent(in)  :: x, y(:)
         REAL(dp), intent(out) :: dydx(:)
       END SUBROUTINE derivs
    END INTERFACE
    REAL(dp), dimension(n), intent(in)  :: yErrReq     ! Requested accuracy for this step
    REAL(dp), dimension(n), intent(out) :: yNew, yErr  ! New variable values and actual errors
    REAL(dp),               intent(out) :: xNew        ! New position
    REAL(dp),               intent(out) :: hDid, hNext ! Successful step size and recommendation
    LOGICAL,                intent(out) :: successful  ! Flags if successful

    !  Local Variables
    !
    REAL(dp), dimension(n)  :: dydx
    REAL(dp) :: maxErr, h, hFactor
    REAL(dp) :: Safety=0.9_dp, pGrow=-0.2_dp, pShrink=-0.25_dp
    REAL(dp) :: LimitShrinkFactor=0.10_dp, LimitGrowthFactor=5.0_dp

    ! Execution begins
    !
    h = hTry
    successful = .true.
    CALL derivs(x, y, dydx)
    do
       CALL rkck(y, dydx, n, x, h, yNew, yErr, derivs)

       !  Compare the error obtained with the error requested ( yErrReq )
       !
       maxErr = maxval( abs( yErr(:)/yErrReq(:) ) )

       IF ( maxErr > 1.0_dp ) then
          !
          ! Obtained error exceeds that requested
          ! Calculate shrink factor
          !
          hFactor = Safety * (maxErr**pShrink)
          !  But let's limit the shrinkage to some fraction of the last h
          !
          IF ( hFactor < LimitShrinkFactor ) then
             h = h * LimitShrinkFactor
          ELSE
             h = h * hFactor
          END IF
          !
          !  Make sure h is still significant in the current precision
          !
          xNew = x + h
          IF ( xNew == x) then
             write(*,'(/,a)') 'WARNING: Requested error cannot be achieved.'
             successful = .false.
             h = 0.0_DP
          END IF
       ELSE
          !
          ! Error request was satisfied
          !
          EXIT
       END IF
    END DO

    hDid = h
    xNew = x + h

    !  Calculate the next reasonable step size
    !
    hFactor  = Safety * (maxErr**pGrow)
    !
    !  But let's limit the growth
    !
    IF ( hFactor  > LimitGrowthFactor ) then
       hNext = h * LimitGrowthFactor
    ELSE
       hNext = h * hFactor
    END IF

    RETURN

  END SUBROUTINE varStep


  ! A 5th order Runge-Kutta Cash-Karp solver with variable step size h
  ! This routine iterates a solution from xStart, yStart to xFinish
  !                                                                                                                                                                                  
  SUBROUTINE integrate(n, yStart, xStart, hTry, derivs, xFinish, yErrReq, yFinish, yErr, hAverage, successful)
    implicit none
    INTEGER,                intent(in)  :: n             ! Number of dynamical variables
    REAL(dp), dimension(n), intent(in)  :: yStart        ! Initial values of the variables
    REAL(dp),               intent(in)  :: xStart, hTry  ! Start position and trial step size
    INTERFACE
       SUBROUTINE derivs(x, y, dydx)                     ! Derivatives of the variables
         USE kinds
         implicit none
         REAL(dp), intent(in)  :: x, y(:)
         REAL(dp), intent(out) :: dydx(:)
       END SUBROUTINE derivs
    END INTERFACE
    REAL(dp),               intent(in)  :: xFinish       ! Final position to integrate to
    REAL(dp), dimension(n), intent(in)  :: yErrReq       ! Requested accuracy for each step
    REAL(dp), dimension(n), intent(out) :: yFinish, yErr ! Variable values at final position and error
    REAL(dp),               intent(out) :: hAverage      ! Average step size used
    LOGICAL,                intent(out) :: successful    ! Flags if successful

    ! Local Variables
    REAL(dp)               :: hNew, hDid, hNext, h
    REAL(dp)               :: x, xNew
    REAL(dp), dimension(n) :: y, accumError
    INTEGER                :: nIterations = 0

    y(:) = yStart(:)
    x = xStart
    h = hTry
    hAverage = 0.0_DP
    accumError(:) = 0.0_DP
    do
       CALL increment(n, y, x, h, derivs, yErrReq, yFinish, yErr, &
            & xNew, hDid, hNext, successful)

       if ( .not. successful ) exit
       y(:) = yFinish(:)
       x  = xNew
       !
       ! Accumulate the error in a worst case scenario
       !
       accumError(:) = accumError(:) + abs(yErr(:))
       !
       ! Calculate the average step size
       !
       hAverage = hAverage + hDid
       nIterations = nIterations + 1

       ! Are we done?
       !
       if (x >= xFinish) exit

       ! Are we about to overshoot?
       !
       if ( x + hNext > xFinish ) then
          h = xFinish - x
       else
          h = hNext
       endif
    end do

    if (successful) then
       yErr(:) = accumError(:)
       hAverage = hAverage / nIterations
    end if

    RETURN
  END SUBROUTINE integrate

END MODULE odeInt
