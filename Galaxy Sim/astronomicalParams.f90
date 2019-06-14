MODULE astronomicalParams
  USE kinds
  implicit none

  ! Unit Conversion:  _ in name denotes /
  !
  real(DP), parameter :: km_au = 149597871.0_DP          
  real(DP), parameter :: au_pc = 206264.8_DP             
  real(DP), parameter :: s_min = 60.0_DP
  real(DP), parameter :: min_hour = 60.0_DP
  real(DP), parameter :: hour_day = 24.0_DP
  real(DP), parameter :: day_year = 365.25_DP
  real(DP), parameter :: year_Kyear = 1.0e3_DP

  ! Derived Unit Conversion
  !
  real(DP), parameter :: km_pc = km_au * au_pc
  real(DP), parameter :: s_Kyear = s_min * min_hour * hour_day * day_year * year_Kyear

  !  Relevant constants
  !
  real(DP), parameter :: G_au3_kg_day2  = 1.48780389e-34_DP                                 ! AU^3 / kg / day^2
  real(DP), parameter :: kg_Mo = 4.0_DP * pi_DP**2 / G_au3_kg_day2 / day_year**2            ! kg / Mo derived via G
  real(DP), parameter :: G_pc3_Mo_year2 = G_au3_kg_day2 * au_pc**(-3) * kg_Mo * day_year**2 ! pc^3 / Mo / year^2
  real(DP), parameter :: G_pc3_Mo_Kyear2 = G_pc3_Mo_year2 * year_Kyear**2                   ! pc^3 / Mo / Kyear^2

END MODULE astronomicalParams
