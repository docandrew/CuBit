/* Development-only: no filesystem, process administration, or desktop grant.
 * Listen only on the QEMU lab interface, TCP 8080. No outbound authority.
 * Plaintext is suitable only behind an isolated loopback QEMU host forward. */
static const unsigned char caps[] __attribute__((section(".cubit.caps"), used)) = {
    0x54,0x49,0x42,0x43, 1,0, 2,0,
    10,3,11,0, 15,2,0,10, 0x90,0x1F,0x90,0x1F,32,2,0,0,
    2,3,25,0, 19,0,0,0, 0,0,0,0,0,0,0,0
};
static const unsigned char identity[] __attribute__((section(".cubit.id"), used)) = {
    0x43,0x42,0x49,0x44,1,0,1,0,
    2,21,0,'i','d',
    'c','o','m','.','c','u','b','i','t','.','c','c','l','-','c','o','n','t','r','o','l'
};
