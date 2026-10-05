------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with System.Storage_Elements;
with CuBit.Launch_Arguments; use CuBit.Launch_Arguments;

package body CuBit.Launch_Arguments_C is

   function Validate
     (Block : System.Address; Length : Unsigned_32;
      Arguments, Environment, Directory :
        access Unsigned_32) return Interfaces.C.int
   is
      use type System.Address;
      use type System.Storage_Elements.Integer_Address;
   begin
      --  The libc is built without run-time checks: check what the proved
      --  code's precondition and the overlay below need, here.
      if Block = System.Null_Address or else Arguments = null
        or else Environment = null or else Directory = null
        or else Length < Header_Bytes or else Length > Maximum_Block_Bytes
        or else System.Storage_Elements.To_Integer (Block) >
                System.Storage_Elements.Integer_Address'Last
                  - System.Storage_Elements.Integer_Address (Length)
      then
         return 0;
      end if;
      declare
         Item : constant CuBit.Launch_Arguments.Block (1 .. Natural (Length))
         with Import, Address => Block;
      begin
         if CuBit.Launch_Arguments.Validate (Item) /= Valid then
            return 0;
         end if;
         Arguments.all := Unsigned_32 (Arguments_Declared (Item));
         Environment.all := Unsigned_32 (Environment_Declared (Item));
         Directory.all := Unsigned_32 (Directory_Declared (Item));
         return 1;
      end;
   end Validate;

end CuBit.Launch_Arguments_C;
