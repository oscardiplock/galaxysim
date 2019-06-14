module kinds
  implicit none
  integer,  parameter :: SP = kind(1.0)
  integer,  parameter :: DP = kind(1.0d0)
  integer,  parameter :: CP = selected_real_kind(32,99)

  real(DP), parameter :: pi    = 3.14159265358979323846264338327950_DP
  real(SP), parameter :: pi_SP = 3.14159265358979323846264338327950_SP
  real(DP), parameter :: pi_DP = 3.14159265358979323846264338327950_DP
  real(CP), parameter :: pi_CP = 3.14159265358979323846264338327950_CP
end module kinds
