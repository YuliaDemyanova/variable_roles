CLINGO_TAR=$(VARROLES_ROOT)/tools/clingo-4.5.4-linux-x86_64.tar.gz
CLINGO_BIN=$(VARROLES_ROOT)/tools/clingo-4.5.4-linux-x86_64/clingo

all: | $(CLINGO_BIN)

$(CLINGO_BIN): | $(CLINGO_TAR)
	tar -xzvf clingo-4.5.4-linux-x86_64.tar.gz

$(CLINGO_TAR):
	wget https://sourceforge.net/projects/potassco/files/clingo/4.5.4/clingo-4.5.4-linux-x86_64.tar.gz
