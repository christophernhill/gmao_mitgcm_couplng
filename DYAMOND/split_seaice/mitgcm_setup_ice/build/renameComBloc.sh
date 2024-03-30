#! /usr/bin/env sh
#
# 64-bit single precision
sym64bitConst=E
#
# 32-bit single precision
sym64bitConst=D
# sed s'/ * _d  */'${sym64bitConst}'/g'
sed -e s'/ * _d  */'${sym64bitConst}'/g' -e '/^       *COMMON/s/\/ */\/y/' -e '/^       *common/s/\/ */\/y/'
