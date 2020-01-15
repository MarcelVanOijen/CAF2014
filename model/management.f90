module management

use declare_parameters
use environment
implicit none
integer :: PRUN, NOPRUN
real    :: Nfert, prunFRC, prunFRT, thinFRT, treedens, thintreedens

Contains     

  Subroutine fert_prune_thin(year,doy,DAYS_FERT ,NFERTV ,DAYS_PRUNC,FRPRUNC, &
                                      DAYS_PRUNT,FRPRUNT,DAYS_THINT,FRTHINT)
  integer                  :: year,doy,i
  integer,dimension(100,2) :: DAYS_FERT, DAYS_PRUNC, DAYS_PRUNT, DAYS_THINT
  real   ,dimension(100  ) ::            FRPRUNC   , FRPRUNT   , FRTHINT
  integer,dimension(100  ) :: NFERTV
    Nfert   = 0
    PRUN    = 0
    NOPRUN  = 1
    prunFRT = 0
    thinFRT = 0
    do i=1,100    
      if ( (year==DAYS_FERT (i,1)) .and. (doy==DAYS_FERT (i,2)) ) then
        Nfert = NFERTMULT*NFERTV(i)/10000.
	  end if
      if ( (year==DAYS_PRUNC(i,1)) .and. (doy==DAYS_PRUNC(i,2)) ) then
        PRUN    = 1
        NOPRUN  = 0
        prunFRC = FRPRUNC(i)
	  end if
      if ( (year==DAYS_PRUNT(i,1)) .and. (doy==DAYS_PRUNT(i,2)) ) then
        prunFRT = FRPRUNT(i)
	  end if
      if ( (year==DAYS_THINT(i,1)) .and. (doy==DAYS_THINT(i,2)) ) then
        thinFRT = FRTHINT(i)
	  end if
    end do
    thintreedens = treedens * thinFRT
  end Subroutine fert_prune_thin
    
end module management      
