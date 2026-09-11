/* Test-only display lease and grant adversary. No framebuffer/map authority. */
static const unsigned char id[] __attribute__((section(".cubit.id"), used)) = {
    0x43,0x42,0x49,0x44, 1,0, 2,0,
    2,23,0, 'i','d',
    'c','o','m','.','c','u','b','i','t','.','d','i','s','p','l','a','y','-','c','h','e','c','k',
    7,5,0, 'v','e','r','s','i','o','n', '0','.','1','.','0'
};
static const unsigned char caps[] __attribute__((section(".cubit.caps"), used)) = {
    0x54,0x49,0x42,0x43, 1,0, 1,0,
    2,3,22,0, 16,0,0,0,0,0,0,0, 0,0,0,0
};
