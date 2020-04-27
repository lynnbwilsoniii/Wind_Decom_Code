#!/bin/bash
# Simple script file to run a wtdclv2 dataset. First it calls the driver, then it calls
# processor


echo "Starting the driver program..."
./driver.pl

echo ""
echo "Driver file created."
echo ""
echo "Beginning the STICS Processor..."
echo ""

./wtdcLV2_Lite_DF_AdaptCad

echo "Processor Complete!"