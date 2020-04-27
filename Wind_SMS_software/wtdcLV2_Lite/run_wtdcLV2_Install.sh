#!/bin/bash
# Script which will compile all of the necessary programs in the Wind/STICS software
# will also move files to their appropriate places

#install wtdcLV2_StaticT
cd wtdcLV2_Lite_DF_StaticT/src/
make clean
make
cd ../
mv src/wtdcLV2_Lite_DF_StaticT .
cd ../

#install wtdcLV2_AdaptCad
cd wtdcLV2_Lite_DF_AdaptCad/src/
make clean
make
cd ../
mv src/wtdcLV2_Lite_DF_AdaptCad .
cd ../

#install wtdcLV2_CSD tools
cd wtdcLV2_Lite_DF_StaticT/Tools/ChargeStateDistributions_Moments/src/
make clean
make
cd ../
mv src/wtdcLV2_CSD_Moments .
cd ../

cd ChargeStateDistributions_Suprathermal/src/
make clean
make
cd ../
mv src/wtdcLV2_CSD_Supra .
cd ../

#install wtdcLV2_IonicRatio Tools
cd IonicRatio_Moments/src/
make clean
make
cd ../
mv src/wtdcLV2_IonicRatio .
cd ../

cd IonicRatio_Suprathermal/src/
make clean
make
cd ../
mv src/wtdcLV2_IonicRatio_Supra .
cd ../../../
