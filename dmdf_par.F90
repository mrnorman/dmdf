
#define _NCERR(x) if ((x) /= NF90_NOERR) then; write(error_string,fmt='(A)') nf90mpi_strerror(x); success = .false.; return; endif
#define _ERR(x) write(error_string,fmt='(A)') x; success = .false.; return
#define _RET_IF_ERR if (.not. success) return


module dmdf
  use mpi
  use pnetcdf
  implicit none
  private

  integer :: ncid
  integer :: ranks_per_file
  integer :: comm
  integer :: rank
  integer :: nsync
  integer :: sync_counter = 0
  integer :: color
  integer :: key

  character(len=1024), public :: error_string
  logical            , public :: success
  
  interface dmdf_write
    module procedure dmdf_write_real4_1d
    !module procedure dmdf_write_real4_2d
    !module procedure dmdf_write_real4_3d
    !module procedure dmdf_write_real4_4d
    !module procedure dmdf_write_real4_5d

    !module procedure dmdf_write_real8_1d
    !module procedure dmdf_write_real8_2d
    !module procedure dmdf_write_real8_3d
    !module procedure dmdf_write_real8_4d
    !module procedure dmdf_write_real8_5d
  end interface

  public :: dmdf_init
  public :: dmdf_final
  public :: dmdf_write


contains


  subroutine dmdf_init(rank_in,comm_glob,fprefix,rpf,nsync_in)
    implicit none
    integer         , intent(in) :: rank_in
    integer         , intent(in) :: comm_glob
    character(len=*), intent(in) :: fprefix
    integer         , intent(in) :: rpf
    integer         , intent(in) :: nsync_in
    logical :: file_exists
    integer :: ierr
    character(len=256) :: fname

    rank = rank_in
    nsync = nsync_in

    !Create the local communicator
    ranks_per_file = rpf
    color = rank / ranks_per_file
    key = modulo( rank , ranks_per_file )
    call MPI_Comm_Split(comm_glob,color,key,comm,ierr)

    !Create the NetCDF File for this group of tasks
    write(fname,fmt='(A,A,I0.6,A)') trim(fprefix) , '_' , color , '.nc'
    ierr = nf90mpi_create( comm , trim(fname) , NF90_64BIT_OFFSET , MPI_INFO_NULL , ncid )
    _NCERR( ierr )
  end subroutine dmdf_init


  subroutine dmdf_final()
    implicit none
    _NCERR( nf90mpi_close( ncid ) )
  end subroutine dmdf_final


  subroutine dmdf_write_real4_1d(dat,vname,dnames,first)
    implicit none
    integer, parameter :: ndims = 2
    real(4)         , intent(in) :: dat(:)
    character(len=*), intent(in) :: vname
    character(len=*), intent(in) :: dnames(ndims-1)
    logical         , intent(in) :: first
    integer(kind=MPI_OFFSET_KIND) :: start(ndims), count(ndims), dsizes(ndims), unlim_len
    integer :: varid, ierr, dimids(ndims)
    success = .true.
    dsizes(1:ndims-1) = shape(dat)
    dsizes(ndims) = NF90_UNLIMITED
    _NCERR( nf90mpi_redef(ncid) )
    call procure_dimids(ndims,ncid,dnames,dsizes,dimids,unlim_len)     ; _RET_IF_ERR
    call procure_varid(ndims,ncid,vname,dimids,NF90_FLOAT,varid)       ; _RET_IF_ERR
    call compute_start_count(first,ndims,dsizes,unlim_len,start,count) ; _RET_IF_ERR
    _NCERR( nf90mpi_enddef(ncid) )
    call begin_collective_mode(ncid)                                   ; _RET_IF_ERR
    _NCERR( nf90mpi_put_var(ncid,varid,dat,start,count) )
    _NCERR( nf90mpi_end_indep_data(ncid) )
    sync_counter = sync_counter + 1
    if (sync_counter == nsync) ierr = nf90mpi_sync(ncid)
  end subroutine dmdf_write_real4_1d


  subroutine begin_collective_mode(ncid)
    implicit none
    integer, intent(in) :: ncid
    integer :: ierr
    ierr = nf90mpi_begin_indep_data(ncid)
    do while (ierr == NF90_EINDEP)
      ierr = nf90mpi_begin_indep_data(ncid)
    enddo
    _NCERR(ierr)
  end subroutine begin_collective_mode


  subroutine compute_start_count(first,ndims,dsizes,unlim_len,start,count)
    logical                      , intent(in   ) :: first
    integer                      , intent(in   ) :: ndims
    integer(kind=MPI_OFFSET_KIND), intent(in   ) :: dsizes(ndims)
    integer(kind=MPI_OFFSET_KIND), intent(in   ) :: unlim_len
    integer(kind=MPI_OFFSET_KIND), intent(  out) :: start(ndims)
    integer(kind=MPI_OFFSET_KIND), intent(  out) :: count(ndims)
    integer :: i
    do i = 1 , ndims
      if (dsizes(i) == NF90_UNLIMITED) then
        if (first) then
          start(i) = unlim_len+1
        else
          start(i) = unlim_len
        endif
        count(i) = 1
      else
        start(i) = 1
        count(i) = dsizes(i)
      endif
    enddo
  end subroutine compute_start_count


  subroutine procure_varid(ndims,ncid,vname_in,dimids,xtype,varid)
    implicit none
    integer         , intent(in   ) :: ndims
    integer         , intent(in   ) :: ncid
    character(len=*), intent(in   ) :: vname_in
    integer         , intent(in   ) :: dimids(ndims)
    integer         , intent(in   ) :: xtype
    integer         , intent(  out) :: varid
    integer :: i, ierr, xtype_file
    integer, allocatable :: dimids_file(:)
    character(len=128) :: vname
    write(vname,fmt='(A,I0.4,A,A)') 'K',key,'_',trim(vname_in)
    write(*,*) vname
    allocate(dimids_file(ndims))
    !This section procures the variable ID, whether by creating it or finding it.
    ierr = nf90mpi_inq_varid( ncid , trim(vname) , varid )
    if     (ierr == NF90_ENOTVAR) then
      !If the variable doesn't exist, then define it
      _NCERR( nf90mpi_def_var( ncid , trim(vname) , xtype , dimids , varid ) )
    elseif (ierr == NF90_NOERR  ) then
      !The variable already exists. Make sure it has the same dimension IDs.
      ierr = nf90mpi_inquire_variable( ncid , varid, xtype=xtype_file , dimids=dimids_file )
      _NCERR( ierr )
      do i = 1 , ndims
        if (dimids(i) /= dimids_file(i)) then
          _ERR( 'Specified variable dimensions differ from file variable dimensions' )
        endif
      enddo
      if (xtype_file /= xtype) then
        _ERR( 'Passed variable type differs from file variable type' )
      endif
    else
      !Different error. Handle it
      _NCERR( ierr )
    endif
    deallocate(dimids_file)
  end subroutine procure_varid


  subroutine procure_dimids(ndims,ncid,dnames,dsizes,dimids,unlim_len)
    implicit none
    integer                      , intent(in   ) :: ndims
    integer                      , intent(in   ) :: ncid
    character(len=*)             , intent(in   ) :: dnames(ndims)
    integer(kind=MPI_OFFSET_KIND), intent(in   ) :: dsizes(ndims)
    integer                      , intent(  out) :: dimids(ndims)
    integer(kind=MPI_OFFSET_KIND), intent(  out) :: unlim_len
    integer :: i, ierr
    integer(kind=MPI_OFFSET_KIND) :: len
    !The goal of this pass is to procure dimension IDs, whether by creating them or finding them.
    do i = 1 , ndims-1
      !If the dimension is defined already, get the dimension id
      ierr = nf90mpi_inq_dimid( ncid , trim(dnames(i)) , dimids(i) )
      if     (ierr == NF90_EBADDIM) then
        !If the dimension is not defined, then define it
        _NCERR( nf90mpi_def_dim( ncid , trim(dnames(i)) , dsizes(i) , dimids(i) ) )
      elseif (ierr == NF90_NOERR  ) then
        !If the dimension is defined, then make sure the sizes are the same
        _NCERR( nf90mpi_inquire_dimension( ncid , dimids(i) , len=len ) )
        if (len /= dsizes(i)) then
          _ERR( 'Specified dimension size does not match existing dimension size' )
        endif
      else
        !If there's a different error, then handle it
        _NCERR( ierr )
      endif
    enddo

    !If the dimension is defined already, get the dimension id
    ierr = nf90mpi_inq_dimid( ncid , 'unlim', dimids(i) )
    if     (ierr == NF90_EBADDIM) then
      !If the dimension is not defined, then define it, and set unlim_len to zero
      len = NF90_UNLIMITED
      _NCERR( nf90mpi_def_dim( ncid , 'unlim' , len , dimids(ndims) ) )
       unlim_len = 0
    elseif (ierr == NF90_NOERR  ) then
      !If the dimension is defined, get the length
      _NCERR( nf90mpi_inquire_dimension( ncid , dimids(ndims) , len=len ) )
      unlim_len = len
    else
      !If there's a different error, then handle it
      _NCERR( ierr )
    endif
  end subroutine procure_dimids


end module dmdf

