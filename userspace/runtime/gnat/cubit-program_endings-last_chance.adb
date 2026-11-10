------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System.Storage_Elements;
with CuBit.Kernel_ABI;
with CuBit.Kernel_Calls;

package body CuBit.Program_Endings.Last_Chance is

   type Flag is mod 2 ** 32 with Atomic, Size => 32;
   Entered : aliased Flag := 0;

   function Exchange (Item : not null access Flag; Value : Flag) return Flag;
   function Exchange (Item : not null access Flag; Value : Flag) return Flag is
      function Atomic_Exchange (Ptr : System.Address; Val : Flag; Model : Integer) return Flag
      with Import, Convention => Intrinsic, External_Name => "__atomic_exchange_4";
      Sequentially_Consistent : constant := 5;
   begin
      return Atomic_Exchange (Item.all'Address, Value, Sequentially_Consistent);
   end Exchange;

   function First_Entry return Boolean is
     (Exchange (Entered'Access, 1) = 0);

   procedure Write_Console (Text : String) is
      Ignore : constant Unsigned_64 :=
        CuBit.Kernel_Calls.Call
          (CuBit.Kernel_ABI.Write, CuBit.Kernel_ABI.Console,
           Unsigned_64 (System.Storage_Elements.To_Integer (Text'Address)),
           Unsigned_64 (Text'Length));
   begin
      null;
   end Write_Console;

   procedure Stop is
      Ignore : Unsigned_64;
   begin
      loop
         Ignore := CuBit.Kernel_Calls.Call (CuBit.Kernel_ABI.Exit_Process, Unhandled_Exception_Status);
      end loop;
   end Stop;

end CuBit.Program_Endings.Last_Chance;
