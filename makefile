#
# Makefile for YankIt
#
SRCS =        main.asm  archive.asm  shk.asm  file.asm  util.asm
OBJS =        main.root archive.root shk.root file.root util.root \
              main.a    archive.a    shk.a    file.a    util.a
LOBJS =       main      archive      shk      file      util
#SRCS =        main.asm  archive.asm  shk.asm  file.asm  util.asm  debug.asm
#OBJS =        main.root archive.root shk.root file.root util.root debug.root \
#              main.a    archive.a    shk.a    file.a    util.a    debug.a
#LOBJS =       main      archive      shk      file      util      debug

PROGRAM =     YankIt

#
# Main targets
#
all:          $(PROGRAM)

# this version of make didn't like "$(PROGRAM)" as a target
YankIt:       $(OBJS)
              delete $(PROGRAM)
              link $(LOBJS) keep=$(PROGRAM)

macgen:
              MACGEN main.asm  YankIt.Macros 13/ORCAInclude/m16.= \
                        13/AppleUtil/m16.= 13/AInclude/m16.=
              MACGEN file.asm File.Macros YankIt.Macros 13/ORCAInclude/m16.= \
                        13/AInclude/m16.texttool

clobber: clean
              delete $(PROGRAM)
clean:
              delete $(OBJS)

