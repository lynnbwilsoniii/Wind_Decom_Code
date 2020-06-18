# tds_items2.make - for tds_items2.for
# execute this file by saying "make -f tds_items2.make"

ff = f77 -C -e -xl -lV77 

tds_items2: tds_items2.o 
	$(ff) tds_items2.o -L/home/wind/lib -lWAVES -o tds_items2 

tds_items2.o: tds_items2.for
	$(ff) -c tds_items2.for 
