# tds_items.make - for tds_items.for
# execute this file by saying "make -f tds_items.make"

ff = f77 -C -e -xl -lV77 

tds_items: tds_items.o 
	$(ff) tds_items.o -L/home/wind/lib -lWAVES -o tds_items 

tds_items.o: tds_items.for
	$(ff) -c tds_items.for 
