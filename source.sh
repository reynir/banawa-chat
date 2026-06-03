#!/bin/sh

[ ! -d "vendors" ] && mkdir vendors
[ ! -d "vendors/bstr" ] && opam source bstr --dir vendors/bstr
[ ! -d "vendors/digestif" ] && opam source digestif --dir vendors/digestif
[ ! -d "vendors/gmp" ] && opam source gmp --dir vendors/gmp
[ ! -d "vendors/kdf" ] && opam source kdf --dir vendors/kdf
[ ! -d "vendors/mirage-crypto-rng-mkernel" ] && opam source mirage-crypto-rng-mkernel --dir vendors/mirage-crypto-rng-mkernel
[ ! -d "vendors/mkernel" ] && opam source mkernel --dir vendors/mkernel
[ ! -d "vendors/mnet" ] && opam source mnet --dir vendors/mnet
[ ! -d "vendors/x509" ] && opam source x509 --dir vendors/x509
[ ! -d "vendors/utcp" ] && opam source utcp --dir vendors/utcp
[ ! -d "vendors/ca-certs-nss" ] && opam source ca-certs-nss --dir vendors/ca-certs-nss
[ ! -d "vendors/mirage-ptime" ] && opam source mirage-ptime --dir vendors/mirage-ptime
[ ! -d "vendors/zarith" ] && opam source zarith --dir vendors/zarith
[ ! -d "vendors/awa" ] && opam source awa --dir vendors/awa
[ ! -d "vendors/mnottui" ] && opam source mnottui --dir vendors/mnottui
[ ! -d "vendors/tls" ] && opam source tls --dir vendors/tls
