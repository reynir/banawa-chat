vendors:
	test ! -d $@
	mkdir vendors
	@./source.sh

banawa.hvt.target: | vendors
	@echo " BUILD main.exe"
	@dune build --root . --profile=release ./main.exe
	@echo " DESCR main.exe"
	@$(shell dune describe location \
		--context solo5 --no-print-directory --root . --display=quiet \
		./main.exe 1> $@ 2>&1)

banawa.hvt: banawa.hvt.target
	@echo " COPY banawa.hvt"
	@cp $(file < banawa.hvt.target) $@
	@chmod +w $@
	@echo " STRIP banawa.hvt"
	@strip $@

banawa.install: banawa.hvt
	@echo " GEN banawa.install"
	@ocaml install.ml > $@

all: banawa.install | vendors

.PHONY: clean
clean:
	if [ -d vendors ] ; then rm -fr vendors ; fi
	rm -f banawa.hvt.target
	rm -f banawa.hvt
	rm -f banawa.install

install: banawa.intall
	@echo " INSTALL banawa"
	opam-installer banawa.install
