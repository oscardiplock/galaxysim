MODULE RungeKutta
  USE kinds
  implicit none

CONTAINS

  SUBROUTINE rkck (y, dydx, n, x, h, yout, yerr, derivs)
    implicit none
    !   INTEGER, parameter :: NMAX = 50                                                                                                                                              
    INTEGER     :: n
    REAL(dp)    :: h, x, dydx(n), y(n), yerr(n), yout(n) 
    !   EXTERNAL derivs                                                                                                                                                              
    INTERFACE
       SUBROUTINE derivs(x, y, dydx)
         USE kinds
         implicit none
         REAL(dp), intent(in)  :: x, y(:)
         REAL(dp), intent(out) :: dydx(:)
       END SUBROUTINE derivs
    END INTERFACE
    !U    USES derivs                                                                                                                                                                
    INTEGER  :: i 
    REAL(dp) :: ak2(n), ak3(n), ak4(n), ak5(n), ak6(n), ytemp(n),                   &
         A2, A3, A4, A5, A6, B21, B31, B32, B41, B42, B43,                          &
         B51, B52, B53, B54, B61, B62, B63, B64, B65, C1, C3, C4, C6, DC1,          &
         DC3, DC4, DC5, DC6
    PARAMETER (A2 = 0.2_DP, A3 = 0.3_DP, A4 = 0.6_DP, A5 = 1.0_DP, A6 = 0.875_DP, B21 =                &
         0.2_DP, B31 = 3.0_DP / 40.0_DP, B32 = 9.0_DP / 40.0_DP, B41 = 0.3_DP, B42 = -0.9_DP, B43 =    &
         1.2_DP, B51 = -11.0_DP / 54.0_DP, B52 = 2.5_DP, B53 = -70.0_DP / 27.0_DP, B54 = 35.0_DP /     &
         27.0_DP, B61 = 1631.0_DP / 55296.0_DP, B62 = 175.0_DP / 512.0_DP, B63 = 575.0_DP / 13824.0_DP,&
         B64 = 44275.0_DP / 110592.0_DP, B65 = 253.0_DP / 4096.0_DP, C1 = 37.0_DP / 378.0_DP, C3 =     &
         250.0_DP / 621.0_DP, C4 = 125.0_DP / 594.0_DP, C6 = 512.0_DP / 1771.0_DP, DC1 = C1 -          &
         2825.0_DP / 27648.0_DP, DC3 = C3 - 18575.0_DP / 48384.0_DP, DC4 = C4 - 13525.0_DP /           &
         55296.0_DP, DC5 = -277.0_DP / 14336.0_DP, DC6 = C6 - 0.25_DP)
    DO 11 i = 1, n
       ytemp(i) = y(i) + B21 * h * dydx(i)
11  END DO
    CALL derivs(x + A2 * h, ytemp, ak2)
    DO 12 i = 1, n
       ytemp(i) = y(i) + h * (B31 * dydx(i) + B32 * ak2(i) )
12  END DO
    CALL derivs (x + A3 * h, ytemp, ak3)
    DO 13 i = 1, n
       ytemp(i) = y(i) + h * (B41 * dydx(i) + B42 * ak2(i)        &
            + B43 * ak3(i) )
13  END DO
    CALL derivs (x + A4 * h, ytemp, ak4)
    DO 14 i = 1, n
       ytemp(i) = y(i) + h * (B51 * dydx(i) + B52 * ak2(i)        &
            + B53 * ak3(i) + B54 * ak4(i) )
14  END DO
    CALL derivs (x + A5 * h, ytemp, ak5)
    DO 15 i = 1, n
       ytemp(i) = y(i) + h * (B61 * dydx(i) + B62 * ak2(i)        &
            + B63 * ak3(i) + B64 * ak4(i) + B65 * ak5(i) )
15  END DO
    CALL derivs (x + A6 * h, ytemp, ak6)
    DO 16 i = 1, n
       yout(i) = y(i) + h * (C1 * dydx(i) + C3 * ak3(i) + C4 *    &
            ak4(i) + C6 * ak6(i) )
16  END DO
    DO 17 i = 1, n
       yerr(i) = h * (DC1 * dydx(i) + DC3 * ak3(i) + DC4 * ak4(i) &
            + DC5 * ak5(i) + DC6 * ak6(i) )
17  END DO
    RETURN
  END SUBROUTINE rkck

END MODULE RungeKutta
