// https://godbolt.org/g/CLTOZ4

#include <stdint.h>
const uint32_t m=65521;

// Borrowed from orlp's answer, as a simple reference implementation
uint32_t adler32_simple(const uint8_t *B) {
    uint32_t high=0, low=1, tmp = 0;
    do {
	tmp = *B++;
	low += tmp;
	high += low;
	low %= m; high %= m;   // non-deferred modulo if this is uncommented
    } while(*B);
    low %= m;  high %= m;
    return high<<16 | low;
}

// test possible algorithms for 16bit code.
// 65521 (0xFFF1) is more than half 65536, so modulo-reduction on carry doesn't lose information
uint32_t adler32_carry16(const char *B, uint16_t len) {
    uint32_t high=0, low=1, tmp = 0;
    do {
	tmp = *B++;

	low += tmp;
	if (low >= 65536) low -= m;
	low &= 0xFFFF;		// simulate 16bit register wrapping
	if (low >= m) low -= m;

	high += low;
	if (high >= 65536) high -= m;
	high &= 0xFFFF;
//      low %= m; high %= m;
	// high can be >= m on loop exit
    } while(--len);
    //low %= m;  high %= m;
    //high %= m;
    return high<<16 | low;
}



//#include <iostream>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <string>   // useful for the memset-style constructors that repeat a character n times




#ifdef __amd64__
 extern "C" unsigned golfed_adler32_amd64(int /*dummy1*/, const char *buf, int /*dummy2*/, unsigned len);
# define golfed_adler32(len, buf)   golfed_adler32_amd64(1234, buf, 5678, len)
# define USE_ZLIB
#elif  __i386__
 extern "C" uint32_t adler32_16wrapper  (uint16_t len /*cx*/, const char *buf /*ds:si*/);
 extern "C" uint32_t adler32_i386wrapper(uint32_t len /*ecx*/, const char *buf /*esi*/);
//# define golfed_adler32(len, buf)   adler32_16wrapper(len, buf)
# define golfed_adler32(len, buf)   adler32_i386wrapper(len, buf)
# define USE_ZLIB

#elif  __arm__
 extern "C" unsigned adler32arm_golf(const char *buf, unsigned len);
# define golfed_adler32(len, buf)   adler32arm_golf(buf, len)
#else
# error "no architecture"
#endif

#ifdef USE_ZLIB
# include <zlib.h>
 static unsigned zlib_adler(int length, const char* buffer) {
    uLong adler = adler32(0L, Z_NULL, 0);
    return adler32(adler, (const unsigned char*)buffer, length);
 }
#endif

static void test_adler(const char *str)
{
    unsigned len = strlen(str);
#ifdef USE_ZLIB
    unsigned zlib = zlib_adler(len, str);
#else
//    unsigned zlib = reference;
#endif
    unsigned reference = adler32_simple((const uint8_t*)str);
    unsigned golfed = golfed_adler32(len, str);
//    unsigned golfed = adler32_carry16(len, str);

    printf("%.50s (len=%d): zlib:%0#10x  c:%0#10x golfed:%0#10x\n", str, len, zlib, reference, golfed);
    // assert
    if (reference == golfed && zlib == reference)
	return;

    fputs("******   MISMATCH   ********\n", stderr);
    exit(1);
}

static const std::string tests[] = {
    // "",
    "Eagles are great!",
    "Programming Puzzles & Code Golf",
    //"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~",
    std::string(32, '~'),	// 32 repeats of ~
    std::string(1040, '?'),	// leaves low = 0xFFF1 % 0xFFF1 = 0

    std::string(4096, '~') // worst-case for overflow: ~ is ASCII 126, the highest printable ASCII code-point
    ,std::string(4096, 255) // 0xFF bytes: still fine
    ,std::string(5837, '~')  // largest string we can handle without SIGFPE (from a div exception)
    ,std::string(5838, '~')  // one larger
//    ,std::string(100000, '~') // ARM version handles this fine
    ,std::string(9999, 255) // 0xFF bytes: still fine
    ,std::string(65535, '~')
    // 16bit version truncates len to 16 bits, so it can't handle giant inputs
};

int main(int argc, char**argv)
{
    (void)argv; (void)argc;
    //puts ("adler test");

    for (const auto &test_str : tests)
	test_adler(test_str.c_str());

    return 0;
}
