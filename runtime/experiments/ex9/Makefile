
all: experiment9

experiment9: experiment9.o cmm.o types.o
	gcc experiment9.o cmm.o types.o -o experiment9

experiment9.o: ../experiment9.c
	gcc -c ../experiment9.c -o experiment9.o

cmm.o: ../cmm.c
	gcc -c ../cmm.c -o cmm.o

types.o: ../types.c
	gcc -c ../types.c -o types.o

