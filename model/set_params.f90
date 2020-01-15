Subroutine set_params(pa)

use declare_parameters
implicit none
! As long as the total number of parameters stays below 120, the next line need not be changed
real pa(120)

!%%%%%% Initial constants and parameters for coffee

!! COFFEE
! Structural parameters
KSINKPPAR  =    pa( 1)      !
! Initial constants
CL0        =	pa( 2)      ! % (kg C m-2)        :
CP0        =    pa( 3)      ! % (kg C m-2)        :
CR0	       =    pa( 4)      ! % (kg C m-2)        :
CW0        =    pa( 5)      ! % (kg C m-2)        :
DVS0       =    pa( 6)     ! % (-)               :
! Parameters
CCONC      =    pa( 7)     ! % (kg C kg-1 DM)    :
DAYSPLNOP  =    pa( 8)	   ! % (d)               : 848-1213
DAYSPRNOP  =    pa( 9)     ! % (d)               :
FNCLMIN    =    pa(10)     ! % (-)               : 0.3-0.7
FSLAMIN    =    pa(11)     ! % (-)               : 0.55-0.70
FTCCLMIN   =    pa(12)     ! % (-)               :
KEXT       =	pa(13)     ! % (m2 m-2)          :
KFLPAR     =    pa(14)     ! % (MJ-1 PAR)        :
KNMIN      =	pa(15)     ! % (kg N m-2)        :
KNUPT	   =    pa(16)     ! % (kg N kg-1 C d-1) : 
KRAININT   =	pa(17)     ! % (mm [m2 m-2]-1)   :
NCLMAX     =	pa(18)     ! % (kg N kg-1 C)     : 0.06-0.08
NCP        =    pa(19)     ! % (kg N kg-1 C)     : 0.025-0.05
NCR	       =    pa(20)     ! % (kg N kg-1 C)     : 0.035-0.06
NCW	       =    pa(21)     ! % (kg N kg-1 C)     : 0.009-0.03
RAINdoyHI  =    pa(22)     ! % (mm)              :
RUBISC     =	pa(23)     ! % (g m-2)           : 0.5-1.5
SINKL	   =    pa(24)     ! % (-)               :
SINKPMAX   =    pa(25)     ! % (-)               :
SINKR      =    pa(26)     ! % (-)               :
SINKW      =    pa(27)     ! % (-)               :
SLAMAX     =    pa(28)     ! % (m2 kg-1 C)       : 23-36
TCCLMAX    =    pa(29)     ! % (d)               : 750-1200
TCCR       =    pa(30)     ! % (d)               : 1000-3650
TMATB      =    pa(31)     ! % (degC)            :
TMATT      =    pa(32)     ! % (degC d)          : 2780-3370
TRANCO     =    pa(33)     ! % (mm d-1)          :
YG         =    pa(34)     ! % (kg C kg-1 C)     :

!! TREES
! Initial constants
CBtree0    =    pa(35)     ! % (kg C tree-1)     : Gs: 0.72
CLtree0    =    pa(36)     ! % (kg C tree-1)     : Gs: 0.23
CRtree0    =    pa(37)     ! % (kg C tree-1)     : Gs: 1.28
CStree0    =    pa(38)     ! % (kg C tree-1)     : Gs: 0.81
TREEDENS0  =    pa(39)
! Parameters
BETA       =    pa(40)     ! % (-)               :
FB         =    pa(41)     ! % (kg C kg-1 C)     :
FLMAX      =    pa(42)     ! % (kg C kg-1 C)     :
FNCLMINT   =    pa(43)     ! % (-)               : Ca: 0.7(0.6-0.8), Ep: 0.6(0.5-0.7), Gs: 0.6(0.5-0.7)
FS         =    pa(44)     ! % (kg C kg-1 C)     :
FTCCLMINT  =    pa(45)     ! % (-)               : {Ca,Gs,Ti}: low, Ed: medium, Ep: medium(non-pollarded)-high(poll.)
GAMMA      =    pa(46)     ! % (kg C kg-1 C)     :
KAC        =    pa(47)     ! % (m2)              : Ep: 15.8
KACEXP     =    pa(48)     ! % (-)               : Ep: 0.55
KEXTT      =    pa(49)     ! % (m2 m-2)          : Ca: 0.32-0.72, Ed: 0.53-0.64, Ep: 0.50-0.87, Gs: low-0.345, Id: high
KH         =    pa(50)     ! % (m)               : Ti: 6.42 , Ed: 5.54-5.70
KHEXP      =    pa(51)     ! % (-)               : Ti: 0.235, Ed: 0.33-0.34
KNFIX      =    pa(52)
KNMINT     =    pa(53)     ! % (kg N m-2)        :
KNUPTT     =    pa(54)     ! % (kg N kg-1 C d-1) :
KRAININTT  =    pa(55)     ! % (mm [m2 m-2]-1)   :
LAIMAXT    =    pa(56)     ! % (m2 m-2)          :
LUEMAX     =    pa(57)     ! % (kg C MJ-1 PAR)   :
NCLMAXT    =    pa(58)     ! % (kg N kg-1 C)     : Ca: 0.085(0.08-0.09), Eglob: 0.04(0.035-0.045), Ep: 0.10(0.09-0.12), Gs: 0.09(0.085-0.10), Id: 0.065(0.055-0.07), NONFIX: 0.05(0.035-0.09)
NCRT       =    pa(59)     ! % (kg N kg-1 C)     : Gs: 0.035
NCWT       =    pa(60)     ! % (kg N kg-1 C)     : branches: 0.02-0.03, stem (Egran): 0.003
SHADEPROJ  =    pa(61)     ! % (m2 m-2)          :
SLAT       =    pa(62)     ! % (m2 kg-1 C)       : Eglob: 13-27, Egran: 24, Enit: 5-12, Ep: 38(31-58), Gs: 40-50, Id: 31-47
TCCBT      =    pa(63)     ! % (d)               : Gs: 1000, Ca: 7300
TCCLMAXT   =    pa(64)     ! % (d)               : Ca: 730, Ep: 81+-15, Gs: 100
TCCRT      =    pa(65)     ! % (d)               : Trop. trees: 3650, Ep: 5200, Ca: 7300
TDIFFMAX   =    pa(66)     ! % (degC)            : Ed: 1.5-1.9, Ijin: 3.9, Ti: 2.4-4
TOPTT      =    pa(67)     ! % (degC)            :
TRANCOT    =    pa(68)     ! % (mm d-1)          :
TTOLT      =    pa(69)     ! % (degC)            :
WOODDENS   =    pa(70)     ! % (kg C m-3)        : Ca: 215(180-235), Ed: 250(205-315), Ep: 125(115-155), Gs: 225(175-270), Ti: 185(160-200)

!! SOIL
! Initial constants
CLITT0     =    pa(71)     ! % (kg C m-2)        : Egran: 0.225
CNLITT0    =    pa(72)     ! % (kg C kg-1 N)     : Acaciamangium: 19-30, Egran: 31-56, Ep: 12(11-20), Gs: 10-20, Iedul: 14-19
CNSOMF0    =    pa(73)     ! % (kg C kg-1 N)     : SOM: 9-14
CNSOMS0    =    pa(74)     ! % (kg C kg-1 N)     : SOM: 9-14
CSOM0      =    pa(75)     ! % (kg C m-2)        : Variation 50-200%
FCSOMF0    =    pa(76)     ! % (-)               : 0.50-0.75
NMIN0      =    pa(77)     ! % (kg N m-2)        :
! Parameters
FLITTSOMF  =    pa(78)     ! % (kg kg-1)         :
FSOMFSOMS  =    pa(79)     ! % (kg kg-1)         :
FWCAD      =    pa(80)     ! % (-)               : 
FWCFC      =    pa(81)     ! % (-)               : 0.65
FWCWET     =    pa(82)     ! % (-)               : 0.87
FWCWP      =    pa(83)     ! % (-)               : 0.41
KNEMIT     =    pa(84)     ! % (kg N kg-1 N d-1) :
KRUNOFF    =    pa(85)
RNLEACH    =    pa(86)     ! % (kg N kg-1 N)     :
ROOTD      =    pa(87)     ! % (m)               :
RRUNBULK   =    pa(88)
SLOPE      =    pa(89)
TCLITT     =    pa(90)     ! % (d)               : CAF: 79, Coffee: ~50, Ep: 80(78-104), Euc: "slow", Gs: 60(33-70), Inga: 388(285-6000+)
TCSOMF     =    pa(91)     ! % (d)               : TROP. AFS: 6000-12000
TCSOMS     =    pa(92)     ! % (d)               : TROP. AFS: 18000-36500
WCST       =    pa(93)     ! % (m3 m-3)          : 0.633(0.62-0.66)

! ATMOSPHERE
CO2A       =    pa(94)

! LOCATION
LAT        =    pa(95)

! MULTIPLIERS FOR SENSITIVITY ANALYSIS
IOMULT     =    pa(96)
NFERTMULT  =    pa(97)
RAINMULT   =    pa(98)
TPLUS      =    pa(99)
FSINKPMAX0 =    pa(100)
KSINKPMAX  =    pa(101)

end Subroutine set_params
