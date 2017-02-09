
#define _NCERR(x) if ((x) /= NF90_NOERR) then; write(error_string,fmt='(A)') nf90_strerror(x); success = .false.; return; endif
#define _ERR(x) write(error_string,fmt='(A)') x; success = .false.; return
#define _RET_IF_ERR if (.not. success) return


module dmdf
  use netcdf
  implicit none
  private

  integer :: ranks_per_file = 32
  integer :: ncid

  character(len=1024), public :: error_string
  logical            , public :: success
  
  interface dmdf_write
    module procedure dmdf_write_real4_1d
    module procedure dmdf_write_real4_2d
    module procedure dmdf_write_real4_3d
    module procedure dmdf_write_real4_4d
    module procedure dmdf_write_real4_5d

    module procedure dmdf_write_real8_1d
    module procedure dmdf_write_real8_2d
    module procedure dmdf_write_real8_3d
    module procedure dmdf_write_real8_4d
    module procedure dmdf_write_real8_5d
  end interface

  public :: dmdf_write


contains


  subroutine dmdf_write_real4_1d(dat,rank,fprefix,vname,dnames,first,last,rpf)
    implicit none
    integer, parameter :: ndims = 2
    real(4)         , intent(in) :: dat(:)
    integer         , intent(in) :: rank
    character(len=*), intent(in) :: fprefix
    character(len=*), intent(in) :: vname
    character(len=*), intent(in) :: dnames(ndims)
    logical         , intent(in) :: first
    logical         , intent(in) :: last
    integer         , intent(in), optional :: rpf
    integer :: dimids(ndims), start(ndims), count(ndims), dsizes(ndims)
    integer :: varid, unlim_len, ierr
    success = .true.
    dsizes(1:ndims-1) = shape(dat)
    dsizes(ndims) = -1
    call pre_process(ndims,dsizes,rpf)                                   ; _RET_IF_ERR
    call procure_fileid(first,rank,fprefix,ncid)                         ; _RET_IF_ERR
    ierr = nf90_redef(ncid)
    call procure_dimids(ndims,ncid,dnames,dsizes,dimids,unlim_len)       ; _RET_IF_ERR
    call procure_varid(ndims,ncid,vname,dimids,NF90_FLOAT,varid)         ; _RET_IF_ERR
    call compute_start_count(first,ndims,dsizes,unlim_len,start,count)   ; _RET_IF_ERR
    ierr = nf90_enddef(ncid)
    _NCERR( nf90_put_var(ncid,varid,dat,start,count) )
    call close_file(last,ncid)                                           ; _RET_IF_ERR
  end subroutine dmdf_write_real4_1d


  subroutine dmdf_write_real4_2d(dat,rank,fprefix,vname,dnames,first,last,rpf)
    implicit none
    integer, parameter :: ndims = 3
    real(4)         , intent(in) :: dat(:,:)
    integer         , intent(in) :: rank
    character(len=*), intent(in) :: fprefix
    character(len=*), intent(in) :: vname
    character(len=*), intent(in) :: dnames(ndims)
    logical         , intent(in) :: first
    logical         , intent(in) :: last
    integer         , intent(in), optional :: rpf
    integer :: dimids(ndims), start(ndims), count(ndims), dsizes(ndims)
    integer :: varid, unlim_len, ierr
    success = .true.
    dsizes(1:ndims-1) = shape(dat)
    dsizes(ndims) = -1
    call pre_process(ndims,dsizes,rpf)                                   ; _RET_IF_ERR
    call procure_fileid(first,rank,fprefix,ncid)                         ; _RET_IF_ERR
    ierr = nf90_redef(ncid)
    call procure_dimids(ndims,ncid,dnames,dsizes,dimids,unlim_len)       ; _RET_IF_ERR
    call procure_varid(ndims,ncid,vname,dimids,NF90_FLOAT,varid)         ; _RET_IF_ERR
    call compute_start_count(first,ndims,dsizes,unlim_len,start,count)   ; _RET_IF_ERR
    ierr = nf90_enddef(ncid)
    _NCERR( nf90_put_var(ncid,varid,dat,start,count) )
    call close_file(last,ncid)                                           ; _RET_IF_ERR
  end subroutine dmdf_write_real4_2d


  subroutine dmdf_write_real4_3d(dat,rank,fprefix,vname,dnames,first,last,rpf)
    implicit none
    integer, parameter :: ndims = 4
    real(4)         , intent(in) :: dat(:,:,:)
    integer         , intent(in) :: rank
    character(len=*), intent(in) :: fprefix
    character(len=*), intent(in) :: vname
    character(len=*), intent(in) :: dnames(ndims)
    logical         , intent(in) :: first
    logical         , intent(in) :: last
    integer         , intent(in), optional :: rpf
    integer :: dimids(ndims), start(ndims), count(ndims), dsizes(ndims)
    integer :: varid, unlim_len, ierr
    success = .true.
    dsizes(1:ndims-1) = shape(dat)
    dsizes(ndims) = -1
    call pre_process(ndims,dsizes,rpf)                                   ; _RET_IF_ERR
    call procure_fileid(first,rank,fprefix,ncid)                         ; _RET_IF_ERR
    ierr = nf90_redef(ncid)
    call procure_dimids(ndims,ncid,dnames,dsizes,dimids,unlim_len)       ; _RET_IF_ERR
    call procure_varid(ndims,ncid,vname,dimids,NF90_FLOAT,varid)         ; _RET_IF_ERR
    call compute_start_count(first,ndims,dsizes,unlim_len,start,count)   ; _RET_IF_ERR
    ierr = nf90_enddef(ncid)
    _NCERR( nf90_put_var(ncid,varid,dat,start,count) )
    call close_file(last,ncid)                                           ; _RET_IF_ERR
  end subroutine dmdf_write_real4_3d


  subroutine dmdf_write_real4_4d(dat,rank,fprefix,vname,dnames,first,last,rpf)
    implicit none
    integer, parameter :: ndims = 5
    real(4)         , intent(in) :: dat(:,:,:,:)
    integer         , intent(in) :: rank
    character(len=*), intent(in) :: fprefix
    character(len=*), intent(in) :: vname
    character(len=*), intent(in) :: dnames(ndims)
    logical         , intent(in) :: first
    logical         , intent(in) :: last
    integer         , intent(in), optional :: rpf
    integer :: dimids(ndims), start(ndims), count(ndims), dsizes(ndims)
    integer :: varid, unlim_len, ierr
    success = .true.
    dsizes(1:ndims-1) = shape(dat)
    dsizes(ndims) = -1
    call pre_process(ndims,dsizes,rpf)                                   ; _RET_IF_ERR
    call procure_fileid(first,rank,fprefix,ncid)                         ; _RET_IF_ERR
    ierr = nf90_redef(ncid)
    call procure_dimids(ndims,ncid,dnames,dsizes,dimids,unlim_len)       ; _RET_IF_ERR
    call procure_varid(ndims,ncid,vname,dimids,NF90_FLOAT,varid)         ; _RET_IF_ERR
    call compute_start_count(first,ndims,dsizes,unlim_len,start,count)   ; _RET_IF_ERR
    ierr = nf90_enddef(ncid)
    _NCERR( nf90_put_var(ncid,varid,dat,start,count) )
    call close_file(last,ncid)                                           ; _RET_IF_ERR
  end subroutine dmdf_write_real4_4d


  subroutine dmdf_write_real4_5d(dat,rank,fprefix,vname,dnames,first,last,rpf)
    implicit none
    integer, parameter :: ndims = 6
    real(4)         , intent(in) :: dat(:,:,:,:,:)
    integer         , intent(in) :: rank
    character(len=*), intent(in) :: fprefix
    character(len=*), intent(in) :: vname
    character(len=*), intent(in) :: dnames(ndims)
    logical         , intent(in) :: first
    logical         , intent(in) :: last
    integer         , intent(in), optional :: rpf
    integer :: dimids(ndims), start(ndims), count(ndims), dsizes(ndims)
    integer :: varid, unlim_len, ierr
    success = .true.
    dsizes(1:ndims-1) = shape(dat)
    dsizes(ndims) = -1
    call pre_process(ndims,dsizes,rpf)                                   ; _RET_IF_ERR
    call procure_fileid(first,rank,fprefix,ncid)                         ; _RET_IF_ERR
    ierr = nf90_redef(ncid)
    call procure_dimids(ndims,ncid,dnames,dsizes,dimids,unlim_len)       ; _RET_IF_ERR
    call procure_varid(ndims,ncid,vname,dimids,NF90_FLOAT,varid)         ; _RET_IF_ERR
    call compute_start_count(first,ndims,dsizes,unlim_len,start,count)   ; _RET_IF_ERR
    ierr = nf90_enddef(ncid)
    _NCERR( nf90_put_var(ncid,varid,dat,start,count) )
    call close_file(last,ncid)                                           ; _RET_IF_ERR
  end subroutine dmdf_write_real4_5d


  subroutine dmdf_write_real8_1d(dat,rank,fprefix,vname,dnames,first,last,rpf)
    implicit none
    integer, parameter :: ndims = 2
    real(8)         , intent(in) :: dat(:)
    integer         , intent(in) :: rank
    character(len=*), intent(in) :: fprefix
    character(len=*), intent(in) :: vname
    character(len=*), intent(in) :: dnames(ndims)
    logical         , intent(in) :: first
    logical         , intent(in) :: last
    integer         , intent(in), optional :: rpf
    integer :: dimids(ndims), start(ndims), count(ndims), dsizes(ndims)
    integer :: varid, unlim_len, ierr
    success = .true.
    dsizes(1:ndims-1) = shape(dat)
    dsizes(ndims) = -1
    call pre_process(ndims,dsizes,rpf)                                   ; _RET_IF_ERR
    call procure_fileid(first,rank,fprefix,ncid)                         ; _RET_IF_ERR
    ierr = nf90_redef(ncid)
    call procure_dimids(ndims,ncid,dnames,dsizes,dimids,unlim_len)       ; _RET_IF_ERR
    call procure_varid(ndims,ncid,vname,dimids,NF90_FLOAT,varid)         ; _RET_IF_ERR
    call compute_start_count(first,ndims,dsizes,unlim_len,start,count)   ; _RET_IF_ERR
    ierr = nf90_enddef(ncid)
    _NCERR( nf90_put_var(ncid,varid,dat,start,count) )
    call close_file(last,ncid)                                           ; _RET_IF_ERR
  end subroutine dmdf_write_real8_1d


  subroutine dmdf_write_real8_2d(dat,rank,fprefix,vname,dnames,first,last,rpf)
    implicit none
    integer, parameter :: ndims = 3
    real(8)         , intent(in) :: dat(:,:)
    integer         , intent(in) :: rank
    character(len=*), intent(in) :: fprefix
    character(len=*), intent(in) :: vname
    character(len=*), intent(in) :: dnames(ndims)
    logical         , intent(in) :: first
    logical         , intent(in) :: last
    integer         , intent(in), optional :: rpf
    integer :: dimids(ndims), start(ndims), count(ndims), dsizes(ndims)
    integer :: varid, unlim_len, ierr
    success = .true.
    dsizes(1:ndims-1) = shape(dat)
    dsizes(ndims) = -1
    call pre_process(ndims,dsizes,rpf)                                   ; _RET_IF_ERR
    call procure_fileid(first,rank,fprefix,ncid)                         ; _RET_IF_ERR
    ierr = nf90_redef(ncid)
    call procure_dimids(ndims,ncid,dnames,dsizes,dimids,unlim_len)       ; _RET_IF_ERR
    call procure_varid(ndims,ncid,vname,dimids,NF90_FLOAT,varid)         ; _RET_IF_ERR
    call compute_start_count(first,ndims,dsizes,unlim_len,start,count)   ; _RET_IF_ERR
    ierr = nf90_enddef(ncid)
    _NCERR( nf90_put_var(ncid,varid,dat,start,count) )
    call close_file(last,ncid)                                           ; _RET_IF_ERR
  end subroutine dmdf_write_real8_2d


  subroutine dmdf_write_real8_3d(dat,rank,fprefix,vname,dnames,first,last,rpf)
    implicit none
    integer, parameter :: ndims = 4
    real(8)         , intent(in) :: dat(:,:,:)
    integer         , intent(in) :: rank
    character(len=*), intent(in) :: fprefix
    character(len=*), intent(in) :: vname
    character(len=*), intent(in) :: dnames(ndims)
    logical         , intent(in) :: first
    logical         , intent(in) :: last
    integer         , intent(in), optional :: rpf
    integer :: dimids(ndims), start(ndims), count(ndims), dsizes(ndims)
    integer :: varid, unlim_len, ierr
    success = .true.
    dsizes(1:ndims-1) = shape(dat)
    dsizes(ndims) = -1
    call pre_process(ndims,dsizes,rpf)                                   ; _RET_IF_ERR
    call procure_fileid(first,rank,fprefix,ncid)                         ; _RET_IF_ERR
    ierr = nf90_redef(ncid)
    call procure_dimids(ndims,ncid,dnames,dsizes,dimids,unlim_len)       ; _RET_IF_ERR
    call procure_varid(ndims,ncid,vname,dimids,NF90_FLOAT,varid)         ; _RET_IF_ERR
    call compute_start_count(first,ndims,dsizes,unlim_len,start,count)   ; _RET_IF_ERR
    ierr = nf90_enddef(ncid)
    _NCERR( nf90_put_var(ncid,varid,dat,start,count) )
    call close_file(last,ncid)                                           ; _RET_IF_ERR
  end subroutine dmdf_write_real8_3d


  subroutine dmdf_write_real8_4d(dat,rank,fprefix,vname,dnames,first,last,rpf)
    implicit none
    integer, parameter :: ndims = 5
    real(8)         , intent(in) :: dat(:,:,:,:)
    integer         , intent(in) :: rank
    character(len=*), intent(in) :: fprefix
    character(len=*), intent(in) :: vname
    character(len=*), intent(in) :: dnames(ndims)
    logical         , intent(in) :: first
    logical         , intent(in) :: last
    integer         , intent(in), optional :: rpf
    integer :: dimids(ndims), start(ndims), count(ndims), dsizes(ndims)
    integer :: varid, unlim_len, ierr
    success = .true.
    dsizes(1:ndims-1) = shape(dat)
    dsizes(ndims) = -1
    call pre_process(ndims,dsizes,rpf)                                   ; _RET_IF_ERR
    call procure_fileid(first,rank,fprefix,ncid)                         ; _RET_IF_ERR
    ierr = nf90_redef(ncid)
    call procure_dimids(ndims,ncid,dnames,dsizes,dimids,unlim_len)       ; _RET_IF_ERR
    call procure_varid(ndims,ncid,vname,dimids,NF90_FLOAT,varid)         ; _RET_IF_ERR
    call compute_start_count(first,ndims,dsizes,unlim_len,start,count)   ; _RET_IF_ERR
    ierr = nf90_enddef(ncid)
    _NCERR( nf90_put_var(ncid,varid,dat,start,count) )
    call close_file(last,ncid)                                           ; _RET_IF_ERR
  end subroutine dmdf_write_real8_4d


  subroutine dmdf_write_real8_5d(dat,rank,fprefix,vname,dnames,first,last,rpf)
    implicit none
    integer, parameter :: ndims = 6
    real(8)         , intent(in) :: dat(:,:,:,:,:)
    integer         , intent(in) :: rank
    character(len=*), intent(in) :: fprefix
    character(len=*), intent(in) :: vname
    character(len=*), intent(in) :: dnames(ndims)
    logical         , intent(in) :: first
    logical         , intent(in) :: last
    integer         , intent(in), optional :: rpf
    integer :: dimids(ndims), start(ndims), count(ndims), dsizes(ndims)
    integer :: varid, unlim_len, ierr
    success = .true.
    dsizes(1:ndims-1) = shape(dat)
    dsizes(ndims) = -1
    call pre_process(ndims,dsizes,rpf)                                   ; _RET_IF_ERR
    call procure_fileid(first,rank,fprefix,ncid)                         ; _RET_IF_ERR
    ierr = nf90_redef(ncid)
    call procure_dimids(ndims,ncid,dnames,dsizes,dimids,unlim_len)       ; _RET_IF_ERR
    call procure_varid(ndims,ncid,vname,dimids,NF90_FLOAT,varid)         ; _RET_IF_ERR
    call compute_start_count(first,ndims,dsizes,unlim_len,start,count)   ; _RET_IF_ERR
    ierr = nf90_enddef(ncid)
    _NCERR( nf90_put_var(ncid,varid,dat,start,count) )
    call close_file(last,ncid)                                           ; _RET_IF_ERR
  end subroutine dmdf_write_real8_5d


  subroutine close_file(last,ncid)
    implicit none
    logical, intent(in) :: last
    integer, intent(in) :: ncid
    if (last) then
      _NCERR( nf90_close(ncid) )
    endif
  end subroutine close_file


  subroutine compute_start_count(first,ndims,dsizes,unlim_len,start,count)
    logical, intent(in   ) :: first
    integer, intent(in   ) :: ndims
    integer, intent(in   ) :: dsizes(ndims)
    integer, intent(in   ) :: unlim_len
    integer, intent(  out) :: start (ndims)
    integer, intent(  out) :: count (ndims)
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


  subroutine procure_varid(ndims,ncid,vname,dimids,xtype,varid)
    implicit none
    integer         , intent(in   ) :: ndims
    integer         , intent(in   ) :: ncid
    character(len=*), intent(in   ) :: vname
    integer         , intent(in   ) :: dimids(ndims)
    integer         , intent(in   ) :: xtype
    integer         , intent(  out) :: varid
    integer :: i, ierr, xtype_file
    integer, allocatable :: dimids_file(:)
    allocate(dimids_file(ndims))
    !This section procures the variable ID, whether by creating it or finding it.
    ierr = nf90_inq_varid( ncid , trim(vname) , varid )
    if     (ierr == NF90_ENOTVAR) then
      !If the variable doesn't exist, then define it
      _NCERR( nf90_def_var( ncid , trim(vname) , xtype , dimids , varid ) )
    elseif (ierr == NF90_NOERR  ) then
      !The variable already exists. Make sure it has the same dimension IDs.
      _NCERR( nf90_inquire_variable( ncid , varid, xtype=xtype_file , dimids=dimids_file ) )
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
    integer         , intent(in   ) :: ndims
    integer         , intent(in   ) :: ncid
    character(len=*), intent(in   ) :: dnames(ndims)
    integer         , intent(in   ) :: dsizes(ndims)
    integer         , intent(  out) :: dimids(ndims)
    integer         , intent(  out) :: unlim_len
    integer :: i, ierr
    integer                       :: len
    !The goal of this pass is to procure dimension IDs, whether by creating them or finding them.
    do i = 1 , ndims
      !If the dimension is defined already, get the dimension id
      ierr = nf90_inq_dimid( ncid , trim(dnames(i)) , dimids(i) )
      if     (ierr == NF90_EBADDIM) then
        !If the dimension is not defined, then define it
        _NCERR( nf90_def_dim( ncid , trim(dnames(i)) , dsizes(i) , dimids(i) ) )
        !If it's the unlimited dimension, then change size to zero (the current # of entries)
        if (dsizes(i) == NF90_UNLIMITED) unlim_len = 0
      elseif (ierr == NF90_NOERR  ) then
        !If the dimension is defined, then make sure the sizes are the same
        _NCERR( nf90_inquire_dimension( ncid , dimids(i) , len=len ) )
        !If it's the unlimited dimension, then change size the current # of entries
        if (dsizes(i) == NF90_UNLIMITED) then
          unlim_len = len
        elseif (len /= dsizes(i)) then
          _ERR( 'Specified dimension size does not match existing dimension size' )
        endif
      else
        !If there's a different error, then handle it
        _NCERR( ierr )
      endif
    enddo
  end subroutine procure_dimids


  subroutine procure_fileid(first,rank,fprefix,ncid)
    implicit none
    logical         , intent(in   ) :: first
    integer         , intent(in   ) :: rank
    character(len=*), intent(in   ) :: fprefix
    integer         , intent(  out) :: ncid
    logical :: file_exists
    integer :: groupnum
    character(len=256) :: fname
    if (first) then
      groupnum = rank / ranks_per_file
      write(fname,fmt='(A,A,I0.4,A)') trim(fprefix) , '_' , groupnum , '.nc'

      !If the file exists, open for writing. Otherwise, create
      inquire(file=trim(fname), exist=file_exists) 
      if (file_exists) then
        _NCERR( nf90_open  ( trim(fname) , NF90_WRITE        , ncid ) )
      else
        _NCERR( nf90_create( trim(fname) , NF90_64BIT_OFFSET , ncid ) )
      endif
    endif
  end subroutine procure_fileid


  subroutine pre_process(ndims,dsizes,rpf)
    implicit none
    integer , intent(in   ) :: ndims
    integer , intent(inout) :: dsizes(ndims)
    integer , intent(in), optional :: rpf
    integer :: i
    !User uses -1 to specify the unlimited dimension
    do i = 1 , ndims
      if (dsizes(i) == -1) dsizes(i) = NF90_UNLIMITED
    enddo
    if (present(rpf)) ranks_per_file = rpf
  end subroutine pre_process



end module dmdf

