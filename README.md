# adler32-golf
adler32 assembly language code golf  (smallest code size)

See my answers on http://codegolf.stackexchange.com/questions/78896/compute-the-adler-32-checksum

commands for building test-adler32 are in comments at the top of each .asm.

Best versions:

* [x86-16](http://codegolf.stackexchange.com/a/79550/30206): v6: 32 bytes
* x86-32: v3 and v9: 31 bytes  (v9 does the modulo of high inside the loop, so it overflows a lot later)
* [x86-64](http://codegolf.stackexchange.com/a/78972/30206): v3: 32 bytes
* [ARM Thumb-2](http://codegolf.stackexchange.com/a/79212/30206): 40 bytes
