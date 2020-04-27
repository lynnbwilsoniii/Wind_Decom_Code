#!/bin/bash
# Simple script file to run a wtdclv2 dataset. First it calls the driver, then it calls
# processor


echo "Starting the driver program..."
./driver.pl

echo ""
echo "Driver file created."
echo ""
echo "Beginning the Charge State Distribution Processor..."
echo ""

./wtdcLV2_IonicRatio

echo "Processor Complete!"