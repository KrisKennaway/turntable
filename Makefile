DISKIMAGE=turntable.dsk

system_files = turntable.system

src_files = $(wildcard *.s)
objects = $(patsubst %.s,%.o,$(src_files))

all: $(DISKIMAGE)

%.o : %.s
	ca65 -t apple2 --cpu 6502 -o $@ -l turntable.lst $<

%.system : %.o
	cl65 -t apple2 -C make/apple2-asm-system.cfg -u __EXEHDR__ --start-addr 0x2000 -o $@ $<

clean:
	rm -f $(objects) $(system_files) $(DISKIMAGE)

$(DISKIMAGE): $(bin_files) $(system_files)
	cp make/blank.dsk $(DISKIMAGE)
	java -jar make/AppleCommander.jar -d $(DISKIMAGE) NS.CLOCK.SYSTEM
	java -jar make/AppleCommander.jar -d $(DISKIMAGE) LOADER.SYSTEM

	java -jar make/AppleCommander.jar -as $(DISKIMAGE) tt.system < turntable.system
	java -jar make/AppleCommander.jar -p $(DISKIMAGE) SOUND.DATA BIN 0x2000 < out.a2d
