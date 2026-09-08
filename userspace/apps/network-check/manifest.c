/* Boot-test scopes: generic inspection, TCP 10.0.2.0/24:18443, and
 * listener 10.0.2.15:8080. Only network=declared launch approval enables scopes. */
static const unsigned char caps[] __attribute__((section(".cubit.caps"), used)) = {
    0x54,0x49,0x42,0x43, 1,0, 3,0,
    2,3,11,0, 3,0,0,0, 0,0,0,0,0,0,0,0,
    10,3,30,0, 0,2,0,10, 0x0B,0x48,0x0B,0x48,24,1,0,0,
    10,3,31,0, 15,2,0,10, 0x90,0x1F,0x90,0x1F,32,2,0,0
};
