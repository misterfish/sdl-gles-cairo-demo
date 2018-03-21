#include <stdbool.h>
#include <stdio.h>

unsigned short convert_888_565 (unsigned char r,  unsigned char g, unsigned char b)
{
    unsigned char rs, gs, bs;
    unsigned short rss, gss, bss;
    rs  = r  >> 3;
    gs  = g  >> 2;
    bs  = b  >> 3;
    rss = rs << 11;
    gss = gs << 5;
    bss = bs;
    return rss | gss | bss;
}

void transfer_888_565 (int num_pixels, unsigned char *from, unsigned short *to)
{
    unsigned char *end = from + num_pixels * 3;
    while (from < end)
        *to++ = convert_888_565 (*(from + 0), *(from + 1), *(from + 2)),
            from += 3;
}

void transfer_8888_565 (int num_pixels, unsigned char *from, unsigned short *to)
{
    unsigned char *end = from + num_pixels * 4;
    while (from < end)
        *to++ = convert_888_565 (*(from + 0), *(from + 1), *(from + 2)),
            from += 4;
}

void transfer_888_8880 (int num_pixels, unsigned char *from, unsigned char *to)
{
    unsigned char *end = from + num_pixels * 3;
    while (from < end) {
        *to++ = *from++;
        *to++ = *from++;
        *to++ = *from++;
        *to++ = 255;
    }
}
