# pkt.make - for pkt.for
# execute this file by saying "make -f pkt.make"

ff = f77 -C -e -xl -PIC -lV77 -w
wind_lib = -L/home/wind/lib -lWAVES

pkt: pkt.o 
	$(ff) pkt.o $(wind_lib) -o pkt

pkt.o: pkt.for
	$(ff) -c pkt.for
