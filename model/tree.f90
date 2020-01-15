Module tree

use declare_parameters
use environment
use management
implicit none

! Morphology
real :: CA, CAtree, CBpertree, CSpertree, h, LA, LAIT

! NPP
real :: fLUEco2, fLUEt, GPP, NPPmaxN, PARabs, PARabsCrown

! Allocation
real :: fGILAI, fGIN, FL, FR

! NdemandOrgans 
real :: gCBmaxN, gCLmaxN, gCRmaxN, gCSmaxN, NBT, NLmaxT, NRT, NST

! gtreeNupt
real :: fNgrowth, gCBT, gCLT, gCRT, gCST, gNLTmax, gNBT, gNLT, gNRT, gNST
real :: NCLTnew, Ndemand, NLTmax, NuptTree

! CNtree
real :: dCBT, dCLT, dCRT, dCST
real :: dNBlitt, dNLT, dNRsomf
real :: harvCSTree, harvNSTree
real :: NCLT
real :: sCBTman, sCLTman, sCRTman
real :: sCBTsen, sCLTsen, sCRTsen
real :: sNLT

Contains

  Subroutine morphology(CLT,CST,CBT,treedens,LAIT)
  real :: CBT, CLT, CST, treedens, LAIT
  if (treedens.ne.0.0) then
      CSpertree = CSt/treedens
  else 
      CSpertree = CST
  endif
  h = KH*(CSpertree**KHEXP)
  if (treedens.ne.0.0) then 
      CBpertree = CBT/treedens
  else 
      CBpertree = CBT
  endif
  CAtree = KAC*(CBpertree**KACEXP)
  CA = CAtree*treedens
  if (CA.gt.1.0) CA = 1.0
  if (CA.lt.0.0) CA = 0.0
  LA = CLT * SLAT
  if (CA.ne.0.0) then
        LAIT = LA/CA
  else
        LAIT = LA
  endif
  end Subroutine morphology  

  Subroutine npp(fTranTree)
  real :: fTranTree
  real :: LUEtree
  fLUEco2     = 1 + beta*log(co2a/co20)
  fLUEt       = exp( -0.5*((T - toptt)/ttolt)**2. )
  LUEtree     = fLUEco2*1.15*fLUET*LUEMAX*fTranTree
  PARabsCrown = PAR * (1. - exp(-KEXTT*LAIT))
  PARabs      = PARabsCrown * CA
  GPP         = PARabs * LUEtree
  NPPmaxN     = GPP * (1-GAMMA)
  end Subroutine npp

  Subroutine allocation(fTranTree,LAIT)
  real :: fTranTree, LAIT
  fGILAI = (1. - LAIT/LAIMAXT)
  if (fGILAI.gt.1.0) fGILAI = 1.0
  if (fGILAI.lt.0.0) fGILAI = 0.0
  fGIN = (NCLT/NCLmaxT - fNCLminT)/(1 - fNCLminT)
  if (fGIN.gt.1.0) fGIN = 1.0
  if (fGIN.lt.0.0) fGIN = 0.0
  FL = FLmax * fGILAI * fGIN * fTranTree
  FR = (1 - (FL+FB+FS))
  end Subroutine allocation

  Subroutine NdemandOrgans
  gCLmaxN = FL * NPPmaxN
  gCBmaxN = FB * NPPmaxN
  gCSmaxN = FS * NPPmaxN
  gCRmaxN = FR * NPPmaxN
  NLTmax  = NCLmaxT * gCLmaxN
  NBT     = NCWT * gCBmaxN
  NST     = NCWT * gCSmaxN
  NRT     = NCRT * gCRmaxN
  end Subroutine NdemandOrgans

  Subroutine gtreeNupt(NsupTree,SA)
  real NsupTree, SA
  Ndemand  = NLTmax + NBT + NST + NRT
  NuptTree = min(NsupTree*SA, Ndemand)
  if (Ndemand.ne.0.0) then 
        fNgrowth = NuptTree/Ndemand
  else 
        fNgrowth = NuptTree
  endif
  if (fNgrowth.gt.1.0) fNgrowth = 1.0
  if (fNgrowth.lt.0.0) fNgrowth = 0.0
  gNLTmax = fNgrowth * NLTmax
  gNBT    = fNgrowth * NBT
  gNST    = fNgrowth * NST
  gNRT    = fNgrowth * NRT
  gNLT    = gNLTmax * fNgrowth
  NCLTnew = ( fNCLminT + fNgrowth*(1.-fNCLminT) ) * NCLmaxT 
  gCBT    = gNBT / NCWT
  gCST    = gNST / NCWT
  gCRT    = (gNRT + (gNLTmax - gNLT))/NCRT
  gCLT    = min(gNLT/NCLTnew,FL*NPPmaxN)
  end Subroutine gtreeNupt

  Subroutine CNtree(fTranTree,CRT,CST,CBT,CLT,NLT)
  real :: CBT, CLT, CRT, CST, NLT, fTranTree
! Roots  
  sCRTman    = thinFRT*CRT
  sCRTsen    = CRT/TCCRT
  dCRT       = sCRTman + sCRTsen
  dNRsomf    = dCRT * NCRT
! Stem 
  dCST       = thinFRT*CST
  harvCStree = dCST
  harvNStree = dCST*NCWT
! Branch
  sCBTman    = (thinFRT + prunFRT)*CBT
  sCBTsen    = CBT/TCCBT
  dCBT       = sCBTman + sCBTsen
  dNBlitt    = dCBT*NCWT
! Leaf C
  sCLTman    = (thinFRT + prunFRT)*CLT
  sCLTsen    = ( CLT / (fTranTree+FTCCLminT*(1-fTranTree)) ) / TCCLmaxT
  dCLT       = sCLTman + sCLTsen
  dNLT       = dCLT*NCLT
  end Subroutine CNtree
  
  Subroutine CNtree_N(CLT,NLT) 
  real :: CLT, NLT
  if (CLT.ne.0.0) then
     NCLT = NLT/CLT
  else
     NCLT = NLT     
  endif
  end Subroutine CNtree_N

end Module tree 

