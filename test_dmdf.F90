
program test_dmdf
  use mpi
  use dmdf
  implicit none
  integer :: rank, ierr
  real :: real4_1d(15)
  real :: real4_2d(15,14)
  real :: real4_3d(15,14,13)
  real :: real4_4d(15,14,13,12)
  real :: real4_5d(15,14,13,12,11)
  real :: real8_1d(15)
  real :: real8_2d(15,14)
  real :: real8_3d(15,14,13)
  real :: real8_4d(15,14,13,12)
  real :: real8_5d(15,14,13,12,11)
  call mpi_init(ierr)
  call mpi_comm_rank(MPI_COMM_WORLD, rank, ierr)

  write(*,*) 'beginning first transfer'

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
  call dmdf_write(real4_1d,rank,'testing','real4_1d',(/'x1'                    /),.true. ,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(real4_2d,rank,'testing','real4_2d',(/'x1','x2'               /),.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(real4_3d,rank,'testing','real4_3d',(/'x1','x2','x3'          /),.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(real4_4d,rank,'testing','real4_4d',(/'x1','x2','x3','x4'     /),.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(real4_5d,rank,'testing','real4_5d',(/'x1','x2','x3','x4','x5'/),.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(real8_1d,rank,'testing','real8_1d',(/'x6'                    /),.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(real8_2d,rank,'testing','real8_2d',(/'x6','x2'               /),.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(real8_3d,rank,'testing','real8_3d',(/'x6','x2','x3'          /),.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(real8_4d,rank,'testing','real8_4d',(/'x6','x2','x3','x4'     /),.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(real8_5d,rank,'testing','real8_5d',(/'x6','x2','x3','x4','x5'/),.false.,.true. ) ; if (.not. success) write(*,*) error_string

  write(*,*) 'beginning second transfer'

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
  call dmdf_write(real4_1d,rank,'testing','real4_1d',(/'x1'                    /),.true. ,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(real4_2d,rank,'testing','real4_2d',(/'x1','x2'               /),.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(real4_3d,rank,'testing','real4_3d',(/'x1','x2','x3'          /),.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(real4_4d,rank,'testing','real4_4d',(/'x1','x2','x3','x4'     /),.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(real4_5d,rank,'testing','real4_5d',(/'x1','x2','x3','x4','x5'/),.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(real8_1d,rank,'testing','real8_1d',(/'x6'                    /),.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(real8_2d,rank,'testing','real8_2d',(/'x6','x2'               /),.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(real8_3d,rank,'testing','real8_3d',(/'x6','x2','x3'          /),.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(real8_4d,rank,'testing','real8_4d',(/'x6','x2','x3','x4'     /),.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(real8_5d,rank,'testing','real8_5d',(/'x6','x2','x3','x4','x5'/),.false.,.true. ) ; if (.not. success) write(*,*) error_string

  call mpi_barrier(MPI_COMM_WORLD,ierr)
  write(*,*) 'finished'

end program test_dmdf



