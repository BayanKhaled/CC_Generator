#!/bin/sh
#
# Usage: >./ccbuild.sh -m {method: ccs, ccd, ccsd, ...}
#         "-m exe" compiles ccgen.f and generates no cc programs
#
# Get input option
while getopts m:v opt
do
    case $opt in
    m)
    METHOD=$OPTARG
    ;;
    v)
    echo "CC code generator"
    exit
    ;;
    esac
done
#
# Check if generator exists
#
if [ ! -f "ccgen.exe" ]; then
    echo "Code generator ccgen.exe not found. ccgen.f being compiled"
    if [ ! -f "ccgen.f" ]; then
	echo "Source program ccgen.f not found. Stop"
	exit
    fi
    gfortran -fdefault-integer-8 ccgen.f -o ccgen.exe
fi
#
#  Exit when "-m exe"
#
case $METHOD in
   exe)
      exit
      ;;
esac
#
#  Check if complex arithmetic is required
#
Done=F
while [ $Done = F ]; do
    echo "Generate spin-orbit cc programs ? [T or F]"
    read Complex
    if [ $Complex = "T" ] || [ $Complex = "F" ]; then
        Done=T
    else
        echo "Enter T or F."
    fi
done
#
#  Factorize diagrams
#
Done=F
while [ $Done = F ]; do
    echo "Factorize the diagrams ? [T or F]"
    read Facto
    if [ $Facto = "T" ] || [ $Facto = "F" ]; then
        Done=T
    else
        echo "Enter T or F."
    fi
done
#
#  Check finite-order truncation
#

#
#  Check if Lambda equations are required
#
Done=F
while [ $Done = F ]; do
    echo "Generate programs for Lambda-equations ? [T or F]"
    read Lambda
    if [ $Lambda = "T" ] || [ $Lambda = "F" ]; then
        Done=T
    else
        echo "Enter T or F."
    fi
done
#
#  Check if Jacobian right diagrams are required
#
Done=F
while [ $Done = F ]; do
    echo "Generate programs for Jacobian right equations ? [T or F]"
    read LinResR
    if [ $LinResR = "T" ] || [ $LinResR = "F" ]; then
        Done=T
    else
        echo "Enter T or F."
    fi
done
#
#  Check if Jacobian left diagrams are required
#
Done=F
while [ $Done = F ]; do
    echo "Generate programs for Jacobian left equations ? [T or F]"
    read LinResL
    if [ $LinResL = "T" ] || [ $LinResL = "F" ]; then
        Done=T
    else
        echo "Enter T or F."
    fi
done
#
#  Check if zeta equations are required
#
Done=F
while [ $Done = F ]; do
    echo "Generate programs for zeta-equations ? [T or F]"
    read Zeta
    if [ $Zeta = "T" ] || [ $Zeta = "F" ]; then
        Done=T
    else
        echo "Enter T or F."
    fi
done
#
#  Check if response density matrices are required
#
Done=F
while [ $Done = F ]; do
    echo "Generate programs for response density-matrices ? [T or F]"
    read RespDM
    if [ $RespDM = "T" ] || [ $RespDM = "F" ]; then
        Done=T
    else
        echo "Enter T or F."
    fi
done
#
#  Check if IP-EOM right equations are required
#
Done=F
while [ $Done = F ]; do
    echo "Generate programs for IP-EOM right equations ? [T or F]"
    read IPRight
    if [ $IPRight = "T" ] || [ $IPRight = "F" ]; then
        Done=T
    else
        echo "Enter T or F."
    fi
done
#
# Generate desired programs
#
# ./ccgen.exe Nmax Nrank MaxOrder OrderCut Facto(not in use) NoT1 Complx LinRes
#
#   Nmax     ... Maximum excitation level
#   Nrank    ... Excitation level onto which the CC eq. is projected
#   MaxOrder ... Maximum order allowed in the amplitude eq. (only if OrderCut=T)
#   OrderCut ... T if diagrams higher than MaxOrder-th order should be discarded
#   Facto    ... T if the diagrams should be factorized
#   NoT1     ... T if diagrams involving T1 vertex should be dropped
#   Complex  ... T for complex amplitudes
#   Lambda   ... T to generate Lambda diagrams
#   LinResR  ... T to generate CC Jacobian right diagrams
#   LinResL  ... T to generate CC Jacobian left diagrams
#   Zeta     ... T to generate zeta diagrams
#   RespDM   ... T to generate response density matrices
#   IPRight  ... T to generate IP-EOM right equations
#
case $METHOD in
   ccs)
      echo "bulding CCS programs"
      if [ $Complex = "Y" ]; then
         echo "  Spin-Orbit ver."
      fi
      ./ccgen.exe 1 1 0 F $Facto F $Complex $Lambda $LinResR $LinResL $Zeta $RespDM $IPRight
      mv CCeq_log CCS_T1_log
      exit
      ;;
   ccd)
      echo "bulding CCD programs"
      if [ $Complex = "Y" ]; then
         echo "  Spin-Orbit ver."
      fi
      ./ccgen.exe 2 2 0 F $Facto T $Complex $Lambda $LinResR $LinResL $Zeta $RespDM $IPRight
      mv CCeq_log CCD_T2_log
      exit
      ;;
   cc2)
      echo "bulding CC2 programs"
      if [ $Complex = "Y" ]; then
         echo "  Spin-Orbit ver."
      fi
      ./ccgen.exe 2 1 0 F $Facto F $Complex $Lambda $LinResR $LinResL $Zeta $RespDM $IPRight
      mv CCeq_log CC2_T1_log
      ./ccgen.exe 2 2 1 T $Facto F $Complex $Lambda $LinResR $LinResL $Zeta $RespDM $IPRight
      mv CCeq_log CC2_T2_log
      exit
      ;;
   ccsd)
      echo "bulding CCSD programs"
      if [ $Complex = "Y" ]; then
         echo "  Spin-Orbit ver."
      fi
      ./ccgen.exe 2 1 0 F $Facto F $Complex $Lambda $LinResR $LinResL $Zeta $RespDM $IPRight
      mv CCeq_log CCSD_T1_log
      ./ccgen.exe 2 2 0 F $Facto F $Complex $Lambda $LinResR $LinResL $Zeta $RespDM $IPRight
      mv CCeq_log CCSD_T2_log
      exit
      ;;
   ccsdt)
      echo "bulding CCSDT programs"
      if [ $Complex = "Y" ]; then
         echo "  Spin-Orbit ver."
      fi
      ./ccgen.exe 3 1 0 F $Facto F $Complex $Lambda $LinResR $LinResL $Zeta $RespDM $IPRight
      mv CCeq_log CCSDT_T1_log
      ./ccgen.exe 3 2 0 F $Facto F $Complex $Lambda $LinResR $LinResL $Zeta $RespDM $IPRight
      mv CCeq_log CCSDT_T2_log
      ./ccgen.exe 3 3 0 F $Facto F $Complex $Lambda $LinResR $LinResL $Zeta $RespDM $IPRight
      mv CCeq_log CCSDT_T3_log
      exit
      ;;
   ccsdtq)
      echo "bulding CCSDTQ programs"
      if [ $Complex = "Y" ]; then
         echo "  Spin-Orbit ver."
      fi
      ./ccgen.exe 4 1 0 F $Facto F $Complex $Lambda $LinResR $LinResL $Zeta $RespDM $IPRight
      mv CCeq_log CCSDTQ_T1_log
      ./ccgen.exe 4 2 0 F $Facto F $Complex $Lambda $LinResR $LinResL $Zeta $RespDM $IPRight
      mv CCeq_log CCSDTQ_T2_log
      ./ccgen.exe 4 3 0 F $Facto F $Complex $Lambda $LinResR $LinResL $Zeta $RespDM $IPRight
      mv CCeq_log CCSDTQ_T3_log
      ./ccgen.exe 4 4 0 F $Facto F $Complex $Lambda $LinResR $LinResL $Zeta $RespDM $IPRight
      mv CCeq_log CCSDTQ_T4_log
      exit
      ;;
    *)
      echo "invalied -m specification."
      exit
      ;;
esac

