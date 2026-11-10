------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System.Storage_Elements;
with CuBit.Kernel_ABI;
with CuBit.Kernel_Calls;

package body CuBit.Exception_Support is

   Character_Modulus : constant := 256;

   procedure Put_Char_Stderr (C : Interfaces.C.int) is
      Item : aliased constant Character :=
        Character'Val (Natural (C) mod Character_Modulus);
      Ignore : constant Unsigned_64 :=
        CuBit.Kernel_Calls.Call
          (CuBit.Kernel_ABI.Write, CuBit.Kernel_ABI.Console,
           Unsigned_64 (System.Storage_Elements.To_Integer (Item'Address)), 1);
   begin
      null;
   end Put_Char_Stderr;

   function Executable_Load_Address return System.Address is (System.Null_Address);

end CuBit.Exception_Support;
