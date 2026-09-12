/* Test-only keyboard publisher plus ordinary desktop client. Never install
 * this authority in a normal application or the default boot profile. */
static const unsigned char id[] __attribute__((section(".cubit.id"), used)) = {
    0x43,0x42,0x49,0x44, 1,0, 2,0,
    2,21,0, 'i','d',
    'c','o','m','.','c','u','b','i','t','.','b','e','n','c','h','-','i','n','p','u','t',
    7,5,0, 'v','e','r','s','i','o','n', '0','.','1','.','0'
};
static const unsigned char caps[] __attribute__((section(".cubit.caps"), used)) = {
    0x54,0x49,0x42,0x43, 1,0, 2,0,
    /* Desktop endpoint, slot 21, role 15. */
    2,3,21,0, 15,0,0,0,0,0,0,0, 0,0,0,0,
    /* Keyboard notification publication, slot 13, role 1. */
    7,1,13,0, 1,0,0,0,0,0,0,0, 0,0,0,0
};
