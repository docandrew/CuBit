/* Only this app's mixer session, not HDA, global input or display authority. */
static const unsigned char id[] __attribute__((section(".cubit.id"), used)) = {
    0x43,0x42,0x49,0x44, 1,0, 2,0,
    2,21,0, 'i','d',
    'c','o','m','.','c','u','b','i','t','.','b','e','n','c','h','.','a','u','d','i','o',
    7,5,0, 'v','e','r','s','i','o','n', '0','.','1','.','0'
};
static const unsigned char caps[] __attribute__((section(".cubit.caps"), used)) = {
    0x54,0x49,0x42,0x43, 1,0, 1,0,
    2,3,14,0, 9,0,0,0,0,0,0,0, 0,0,0,0
};
