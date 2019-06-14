module rotationMatrix
  USE kinds
  implicit none

contains

  function rotateMzyx( kappa, phi, omega ) result (rM)
    real(DP), dimension(3,3) :: rM
    real(DP), intent(in) :: omega, phi, kappa  ! in degrees                                                                                                                          

    !  Local variables                                                                                                                                                               
    !                                                                                                                                                                                
    real(DP) :: Romega, Rphi, Rkappa  ! in radians                                                                                                                                   
    real(DP) :: cw, sw, cp, sp, ck, sk

    !  The rotation matrix below rotates the axes.                                                                                                                                   
    !  Change the sign of the angles to rotate the object                                                                                                                            
    !  Also convert degrees to radians                                                                                                                                               
    !                                                                                                                                                                                
    Romega = -omega * pi_DP /180.0_DP
    Rphi   = -phi   * pi_DP /180.0_DP
    Rkappa = -kappa * pi_DP /180.0_DP

    cw = cos(Romega)
    sw = sin(Romega)
    cp = cos(Rphi)
    sp = sin(Rphi)
    ck = cos(Rkappa)
    sk = sin(Rkappa)

    ! From last page of https://engineering.purdue.edu/~bethel/rot2.pdf                                                                                                              
    !                                                                                                                                                                                
    rM(1,1) =  cp * ck
    rM(1,2) =  cw * sk + sw * sp * ck
    rM(1,3) =  sw * sk - cw * sp * ck
    rM(2,1) = -cp * sk
    rM(2,2) =  cw * ck - sw * sp * sk
    rM(2,3) =  sw * ck + cw * sp * sk
    rM(3,1) =  sp
    rM(3,2) = -sw * cp
    rM(3,3) =  cw * cp

  end function rotateMzyx


end module rotationMatrix
