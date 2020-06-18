# makefile, qdb.make
#
cc = cc -PIC -dalign -fsingle -DSPARC
objs = qdb_args.o qdb_main.o qdb.o
REFS = \
    -L/home/wind/v1.4m/lib -lWAVES \
    /usr/lang/SC3.0.1/lib/values-Xa.o \
    /usr/lang/SC3.0.1/lib/libV77.a \
    /usr/lang/SC3.0.1/lib/libF77.a \
    /usr/lang/SC3.0.1/lib/libM77.a \
    /usr/lang/SC3.0.1/lib/libcx.a \
    /usr/lang/SC3.0.1/lib/libansi.a \
    /usr/lang/SC3.0.1/lib/libsunmath.a \
    /usr/lang/SC3.0.1/lib/libm.a

qdb: $(objs)
	$(cc) $(objs) -o qdb $(REFS)

qdb.o: qdb.c
	$(cc) -c qdb.c

qdb_args.o: qdb_args.c
	$(cc) -c qdb_args.c

qdb_main.o: qdb_main.c
	$(cc) -c qdb_main.c
