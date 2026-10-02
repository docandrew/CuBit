with Ada.Text_IO;
with Interfaces; use Interfaces;
with Interfaces.C;
with System;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_Native_TLB_IO;
with Intel_GPU_TLB_Registers;
with Intel_GPU_ADLN_TLB_Invalidate;
with Intel_GPU_Native_GuC_Invalidate;
procedure Native_TLB_IO_Tests is
   package C renames Interfaces.C;
   use type System.Address;
   use type C.int;
   function Mmap (Addr : System.Address; Length : C.size_t;
                  Prot, Flags, FD : C.int; Offset : C.long) return System.Address
     with Import, Convention => C, External_Name => "mmap";
   function Munmap (Addr : System.Address; Length : C.size_t) return C.int
     with Import, Convention => C, External_Name => "munmap";
   Checks : Natural := 0;
   Lose_At : Natural := 0;
   function Ready return Boolean is
   begin
      Checks := Checks + 1;
      return Lose_At = 0 or else Checks < Lose_At;
   end Ready;
   package IO is new Intel_GPU_Native_TLB_IO (Ready);
   package Invalid is new Intel_GPU_Native_TLB_IO (Ready);
   package Lost is new Intel_GPU_Native_TLB_IO (Ready);
   package Lost_Read is new Intel_GPU_Native_TLB_IO (Ready);
   package Wrong_Value is new Intel_GPU_Native_TLB_IO (Ready);
   package GuC_IO is new Intel_GPU_Native_TLB_IO (Ready, GuC_Only => True);
   package GuC_Denied is new Intel_GPU_Native_TLB_IO (Ready, GuC_Only => True);
   Base : constant Integer_Address := 16#61206000#;
   Addr : System.Address;
   type Page is array (Natural range 0 .. 1023) of Unsigned_32;
   Memory : Page with Import, Volatile, Address => To_Address (Base);
   OK : Boolean;
   Value : Unsigned_32;
   Clock_Calls : Natural := 0;
   procedure Clock_US (Now : out Unsigned_64; Available : out Boolean) is
   begin
      Clock_Calls := Clock_Calls + 1;
      -- Fake hardware completion after both actual mapped writes; this is
      -- host RAM, and explicitly NOT evidence of device-side invalidation.
      if Clock_Calls = 4 then
         pragma Assert (Memory (16#ED8# / 4) = 1 and Memory (16#EEC# / 4) = 1);
         Memory (16#ED8# / 4) := 0; Memory (16#EEC# / 4) := 0;
      end if;
      Now := Unsigned_64 (Clock_Calls); Available := True;
   end Clock_US;
   package Sequence is new Intel_GPU_ADLN_TLB_Invalidate
     (Ready, IO.Write_Register, IO.Read_Register, Clock_US);
   Attempt : Sequence.Attempt;
   Status : Sequence.Result;
   use type Sequence.Result;
   GuC_Clocks : Natural := 0;
   procedure GuC_Clock (Now : out Unsigned_64; Available : out Boolean) is
   begin
      GuC_Clocks := GuC_Clocks + 1;
      if GuC_Clocks = 3 then
         pragma Assert (Memory (16#EE8# / 4) = 1);
         Memory (16#EE8# / 4) := 16#FFFF_FFFE#;
      end if;
      Now := Unsigned_64 (GuC_Clocks); Available := True;
   end GuC_Clock;
   package GuC_Native is new Intel_GPU_Native_GuC_Invalidate (Ready, GuC_Clock);
   package GuC_Wait renames GuC_Native.Completion;
   GuC_Attempt : GuC_Wait.Attempt;
   GuC_Status : GuC_Wait.Result;
   use type GuC_Wait.Result;
begin
   for Offset in Unsigned_32 range 16#C000# .. 16#CFFF# loop
      pragma Assert (GuC_IO.Address_For (Offset) =
        (if Offset = Intel_GPU_TLB_Registers.GuC_Offset then
           Unsigned_64 (Base) + 16#EE8# else 0));
      if Offset = Intel_GPU_TLB_Registers.GFX_Offset or
        Offset = Intel_GPU_TLB_Registers.OA_Offset then
         pragma Assert (IO.Address_For (Offset) = Unsigned_64 (Base) + Unsigned_64 (Offset mod 4096));
      else pragma Assert (IO.Address_For (Offset) = 0); end if;
   end loop;
   -- Address hint only: never MAP_FIXED over an existing host mapping.
   Addr := Mmap (To_Address (Base), 4096, 3, 16#22#, -1, 0);
   pragma Assert (Addr = To_Address (Base));
   Memory := [others => 16#A5A5A5A5#];
   IO.Write_Register (Intel_GPU_TLB_Registers.GFX_Offset, 1, OK); pragma Assert (OK);
   IO.Write_Register (Intel_GPU_TLB_Registers.OA_Offset, 1, OK); pragma Assert (OK);
   for I in Memory'Range loop
      pragma Assert (Memory (I) = (if I = 16#ED8# / 4 or I = 16#EEC# / 4 then 1 else 16#A5A5A5A5#));
   end loop;
   IO.Read_Register (Intel_GPU_TLB_Registers.GFX_Offset, Value, OK);
   pragma Assert (OK and Value = 1 and not IO.Failed);
   Sequence.Execute (Attempt, Status);
   pragma Assert (Status = Sequence.Complete);
   GuC_Wait.Execute (GuC_Attempt, GuC_Status);
   pragma Assert (GuC_Status = GuC_Wait.Complete and not GuC_IO.Failed);
   GuC_Wait.Execute (GuC_Attempt, GuC_Status);
   pragma Assert (GuC_Status = GuC_Wait.Rejected and GuC_Clocks = 3);
   for I in Memory'Range loop
      pragma Assert (Memory (I) =
        (if I = 16#ED8# / 4 or I = 16#EEC# / 4 then 0
         elsif I = 16#EE8# / 4 then 16#FFFF_FFFE# else 16#A5A5A5A5#));
   end loop;
   GuC_Denied.Write_Register (Intel_GPU_TLB_Registers.GFX_Offset, 1, OK);
   pragma Assert (not OK and GuC_Denied.Failed);
   GuC_Denied.Write_Register (Intel_GPU_TLB_Registers.GuC_Offset, 1, OK);
   pragma Assert (not OK); -- domain violation remains latched
   Invalid.Write_Register (16#CEE8#, 1, OK); pragma Assert (not OK and Invalid.Failed);
   Invalid.Write_Register (Intel_GPU_TLB_Registers.GFX_Offset, 1, OK); pragma Assert (not OK);
   Wrong_Value.Write_Register (Intel_GPU_TLB_Registers.GFX_Offset, 3, OK);
   pragma Assert (not OK and Wrong_Value.Failed);
   Checks := 0; Lose_At := 2;
   Lost.Write_Register (Intel_GPU_TLB_Registers.GFX_Offset, 1, OK);
   pragma Assert (not OK and Lost.Failed); -- write may have happened: no retry
   Checks := 0;
   Lost_Read.Read_Register (Intel_GPU_TLB_Registers.OA_Offset, Value, OK);
   pragma Assert (not OK and Value = 0 and Lost_Read.Failed);
   pragma Assert (Munmap (Addr, 4096) = 0);
   Ada.Text_IO.Put_Line ("Native TLB IO PASS: exact mapped offsets/value, untouched guards, ownership-loss latch (host RAM, not GPU)");
end Native_TLB_IO_Tests;
