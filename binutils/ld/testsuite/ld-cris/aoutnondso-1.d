#source: expdyn1.s
#as: --em=crisaout
#ld: -mcrisaout -r
#objdump: -p

# Prepare a.out object to be linked in to other tests.

.*:     file format a.out-cris
