
export

PROJECT = THDM_EWPOS

# variable for the Fortran compiler
FC = gfortran

FFLAGS = -ffixed-line-length-none

LIB=libEWPOTHDM.a

# change the variables LTlib and LTinclude to the directory of the LoopTools library and LoopTools include files.
LTlib= -L$(LT)/lib64 -looptools

LTinclude= -I$(LT)/include

intFLAGS = -I$(PROJDIR)/include $(LTinclude)

LDFLAGS = $(LTlib)

PROJDIR := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

DEMOPATH = $(PROJDIR)/Examples

SRCPATH = $(PROJDIR)/src

PARADIR = $(SRCPATH)/InputParameter

SECTSMDIR = $(SRCPATH)/SECT/SM

SECTTHDMDIR = $(SRCPATH)/SECT/THDM

MWDIR = $(SRCPATH)/MWCorrection

MIXANGLESDIR = $(SRCPATH)/MixingAngles

ZWIDTHDIR= $(SRCPATH)/Zwidths

SMCOUPDIR = $(ZWIDTHDIR)/effCouplingsSM

THDMCOUPDIR = $(ZWIDTHDIR)/effCouplingsTHDM

DRHODIR= $(SRCPATH)/Deltarho

VPATH = $(PROJDIR) $(PARADIR) $(SECTSMDIR) $(SECTTHDMDIR) $(MWDIR) $(OBJDIR) $(ZWIDTHDIR) \
 $(MIXANGLESDIR) $(SMCOUPDIR) $(THDMCOUPDIR) $(DRHODIR) $(DRHODIR)/LT $(DEMOPATH)

fermions = e mu tau u d s c b

effCoupSM = gnu $(addprefix gV, $(fermions)) $(addprefix gA, $(fermions))

effCoupTHDM = gnu $(addprefix gV, e u d b) $(addprefix gA, e u d b)

Filelist = SetSMParameter printSMpara ReadTHDMpara SetTHDMangles printTHDMpara printTHDMangles \
SigmaWSM SigmaZZSM SigmaAZSM SigmaAASM \
dMZ2SM dMW2SM dZWSM dZZZSM dZZASM dZAZSM dZAASM PiAA0SM dSWsqSM \
 SigmaWTHDM SigmaZZTHDM SigmaAZTHDM SigmaAATHDM \
PiAA0THDM dMW2THDM dMZ2THDM dZWTHDM dZZZTHDM dZZATHDM \
dZAZTHDM dZAATHDM dSWsqTHDM \
drSMho awramikpara drSM1Loop drns MWTHDM MWSM1Loop SetWeakAngles ExtractDeltar calcAlfasMT DeltarSM WMassSM WWidth \
$(addsuffix THDM,$(effCoupTHDM)) $(addsuffix SM, $(effCoupSM)) ZWidthSMho ZWidthSM1Loop \
 Rfactors ZWidthTHDM IHDMCouplings1L \
THDMCouplings1L ZobsTHDM mixanglesSM1Loop slepSMfit mixanglesTHDM \
 DelKSM1Loop DelKSMho DelKNS ZWidthSMpara DelgVSM1Loop DelgASM1Loop SetTreeCouplings ZWidthtree \
 drhoNSH1Loop drhotop1Loop drhoNStop2Loop drhoNSat2Loop drhoNSatLam2Loop\
  drhoNSHiggs2Loop drhoMixHiggs2Loop drhoIHDM2Loop drhoNStb2Loop drhoAllH2Loop dMh0HHfin \
A0del A0FH B0FH auxB Dilog T134 Set2Lpara

EXAMPLES = WMassexample slepexample ZWidthexample Deltarhoexample IHDMexample NoAlignexample

EXAMPLEBINS = $(addsuffix .bin, $(EXAMPLES))

EXAMPLEOBJECTS = $(addsuffix .o, $(EXAMPLES))

SOURCES = $(addsuffix .F, $(Filelist))

objects = $(addsuffix .o, $(Filelist))

all: lib $(EXAMPLEBINS)

lib: libEWPOTHDM.a

libEWPOTHDM.a: libEWPOTHDM.a($(objects))
	ar t $@
	ranlib $@

libEWPOTHDM.a($(objects)): libEWPOTHDM.a(%.o): %.F
	$(FC) $(FFLAGS) -c $(intFLAGS) $<
	ar cr libEWPOTHDM.a $*.o
	rm $*.o

dist: $(PROJECT).tar.gz

$(PROJECT).tar.gz: $(PROJECT)
	tar -czvf $@ $<
	rm -r $<

$(PROJECT):
	mkdir $@
	cp -r src $@
	cp -r include $@
	cp -r Literature $@
	cp -r doxyfiles $@
	cp makefile $@
	cp DOXYCONF $@
	cp README.md $@

# adjust the variable DOXYCOMMAND to the command which executes doxygen on your plattform
DOXYCOMMAND = doxygen

DOC :
	$(DOXYCOMMAND) DOXYCONF


$(EXAMPLEOBJECTS): %.o : %.F
		$(FC) $(FFLAGS) -c $(intFLAGS) $<

$(EXAMPLEBINS) : %.bin : %.o -lEWPOTHDM
	$(FC) $(FFLAGS) -o $@ $^ $(LDFLAGS)

.PHONY: clean all DOC

clean:
	 rm -rf $(EXAMPLEBINS) $(EXAMPLEOBJECTS) $(PROJECT).tar.gz
