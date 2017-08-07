CFLAGS = -O2 -Wall -g 
DESTDIR =

.PHONY: all,clean

all: njconnect

njconnect: njconnect.c
	$(CC) $(CFLAGS) -o $@ $^ -ljack -lcurses

clean:
	rm -f njconnect

install: all
	install -Dm755 njconnect $(DESTDIR)/usr/bin/njconnect
