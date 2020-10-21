# PATSHOMEQ="$(PATSHOME)"

# ATSCC=$(PATSHOMEQ)/bin/patscc
# ATSOPT=$(PATSHOMEQ)/bin/patsopt

ATSCC=patscc
ATSOPT=patsopt

# HX: Please uncomment the one you want, or skip it entirely
#
ATSCCFLAGS=-O2
# '-flto' enables link-time optimization such as inlining lib functions
#

cleanall::
#
######
#
# Please uncomment the following three lines and replace the name [test]
# with the name of the file you want to compile
#
main: __PROJECT-NAME__.dats ; \
  $(ATSCC) $(ATSCCFLAGS) -o $@ $< || echo $@ ": ERROR!!!"
run: ; \
make __PROJECT-NAME__; ./__PROJECT-NAME__
cleanall:: ; $(RMF) __PROJECT-NAME__

######
#
# You may find these rules useful
#
# %_sats.o: %.sats ; $(ATSCC) $(ATSCCFLAGS) -c $< || echo $@ ": ERROR!!!"
# %_dats.o: %.dats ; $(ATSCC) $(ATSCCFLAGS) -c $< || echo $@ ": ERROR!!!"
#
######
#
RMF=rm -f
#
######
#
clean:: ; $(RMF) *~
clean:: ; $(RMF) *_?ats.o
clean:: ; $(RMF) *_?ats.c
#
cleanall:: clean
#
###### end of [Makefile] ######
