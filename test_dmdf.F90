
program test_dmdf
  use dmdf
  implicit none
  integer :: r, nranks = 4096
  real :: real4_1d(50)
  real :: real4_2d(50,40)
  real :: real4_3d(50,40,30)
  real :: real4_4d(50,40,30,20)
  real :: real4_5d(50,40,30,20,10)
  real :: real8_1d(100)
  real :: real8_2d(100,40)
  real :: real8_3d(100,40,30)
  real :: real8_4d(100,40,30,20)
  real :: real8_5d(100,40,30,20,10)
  real4_1d = 0.
  real4_2d = 1.
  real4_3d = 2.
  real4_4d = 3.
  real4_5d = 4.
  real8_1d = 5.
  real8_2d = 6.
  real8_3d = 7.
  real8_4d = 8.
  real8_5d = 9.
  call dmdf_write(real4_1d,0,'testing','real4_1d',(/'x1'                    ,'t '/),.true. ,.false.)
  call dmdf_write(real4_2d,0,'testing','real4_2d',(/'x1','x2'               ,'t '/),.false.,.false.)
  call dmdf_write(real4_3d,0,'testing','real4_3d',(/'x1','x2','x3'          ,'t '/),.false.,.false.)
  call dmdf_write(real4_4d,0,'testing','real4_4d',(/'x1','x2','x3','x4'     ,'t '/),.false.,.false.)
  call dmdf_write(real4_5d,0,'testing','real4_5d',(/'x1','x2','x3','x4','x5','t '/),.false.,.false.)
  call dmdf_write(real8_1d,0,'testing','real8_1d',(/'x6'                    ,'t '/),.false.,.false.)
  call dmdf_write(real8_2d,0,'testing','real8_2d',(/'x6','x2'               ,'t '/),.false.,.false.)
  call dmdf_write(real8_3d,0,'testing','real8_3d',(/'x6','x2','x3'          ,'t '/),.false.,.false.)
  call dmdf_write(real8_4d,0,'testing','real8_4d',(/'x6','x2','x3','x4'     ,'t '/),.false.,.false.)
  call dmdf_write(real8_5d,0,'testing','real8_5d',(/'x6','x2','x3','x4','x5','t '/),.false.,.true. )

  real4_1d = 10.
  real4_2d = 20.
  real4_3d = 30.
  real4_4d = 40.
  real4_5d = 50.
  real8_1d = 10.
  real8_2d = 20.
  real8_3d = 30.
  real8_4d = 40.
  real8_5d = 50.
  call dmdf_write(real4_1d,0,'testing','real4_1d',(/'x1'                    ,'t '/),.true. ,.false.)
  call dmdf_write(real4_2d,0,'testing','real4_2d',(/'x1','x2'               ,'t '/),.false.,.false.)
  call dmdf_write(real4_3d,0,'testing','real4_3d',(/'x1','x2','x3'          ,'t '/),.false.,.false.)
  call dmdf_write(real4_4d,0,'testing','real4_4d',(/'x1','x2','x3','x4'     ,'t '/),.false.,.false.)
  call dmdf_write(real4_5d,0,'testing','real4_5d',(/'x1','x2','x3','x4','x5','t '/),.false.,.false. )
  call dmdf_write(real8_1d,0,'testing','real8_1d',(/'x6'                    ,'t '/),.false.,.false.)
  call dmdf_write(real8_2d,0,'testing','real8_2d',(/'x6','x2'               ,'t '/),.false.,.false.)
  call dmdf_write(real8_3d,0,'testing','real8_3d',(/'x6','x2','x3'          ,'t '/),.false.,.false.)
  call dmdf_write(real8_4d,0,'testing','real8_4d',(/'x6','x2','x3','x4'     ,'t '/),.false.,.false.)
  call dmdf_write(real8_5d,0,'testing','real8_5d',(/'x6','x2','x3','x4','x5','t '/),.false.,.true. )

  do r = 0 , nranks-1
    call dmdf_write(real8_2d,r,'testing2','myvar1',(/'x1','x2','t '/),.true.,.false.)
    call dmdf_write(real8_2d,r,'testing2','myvar2',(/'x1','x2','t '/),.false.,.false.)
    call dmdf_write(real8_2d,r,'testing2','myvar3',(/'x1','x2','t '/),.false.,.true.)
  enddo

  if (.not. success) write(*,*) error_string
end program test_dmdf



