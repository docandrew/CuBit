with Interfaces; with CuBit.Kernel_ABI; package CuBit.Kernel_Calls is
 function Call (Code : CuBit.Kernel_ABI.Operation; Target : Interfaces.Unsigned_64 := 0) return Interfaces.Unsigned_64;
end CuBit.Kernel_Calls;
