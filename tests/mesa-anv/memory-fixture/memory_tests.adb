with Ada.Text_IO;
with Interfaces; use Interfaces;
with Native_GPU_Memory;
with CuBit.Memory_Grants;
with CuBit.Grant_References;
procedure Memory_Tests is
   procedure C_Test with Import, Convention => C, External_Name => "memory_c_test";
   package G renames CuBit.Memory_Grants;
   package R renames CuBit.Grant_References;
   Value : aliased Unsigned_64 := 99;
   Word : Unsigned_64;
   Status : Unsigned_32;
   Before : Natural;
   Bad_References : constant array (1 .. 5) of Unsigned_64 :=
     [0, 4095, 4096, 16#1_0000_1000#, Unsigned_64'Last];
   procedure Reject (Slot, Ref, Offset, Bytes, Writable : Unsigned_64) is
      Saved : constant Natural := G.Calls;
   begin
      Value := 99;
      Status := Native_GPU_Memory.Acquire (Slot, Ref, Offset, Bytes, Writable, Value'Access);
      pragma Assert (Status = 1 and Value = 0 and G.Calls = Saved);
   end Reject;
begin
   for Slot in Unsigned_64 range 0 .. 63 loop
      for Writable in Unsigned_64 range 0 .. 1 loop
         for Success in Boolean loop
            G.Expected_Slot := Slot;
            G.Expected_Reference := (slot => 4095, generation => R.Maximum_Generation);
            G.Expected_Offset := 4096;
            G.Expected_Bytes := 8192;
            G.Expected_Access := (if Writable = 1 then G.Write_Access else G.Read_Access);
            G.Succeed := Success;
            Word := R.Encode (G.Expected_Reference);
            Before := G.Calls;
            Status := Native_GPU_Memory.Acquire (Slot, Word, 4096, 8192, Writable, Value'Access);
            pragma Assert (G.Calls = Before + 1);
            pragma Assert (Status = (if Success then 0 else 1));
            pragma Assert (Value = (if Success then 16#7000_1234_5000# else 0));
            Before := G.Returns;
            Status := Native_GPU_Memory.Return_Borrow (Word);
            pragma Assert (G.Returns = Before + 1 and
                           Status = (if Success then 0 else 1));
         end loop;
      end loop;
   end loop;
   Reject (64, Word, 0, 4096, 0);
   Reject (Unsigned_64'Last, Word, 0, 4096, 0);
   Reject (0, Word, 0, 0, 0);
   Reject (0, Word, Unsigned_64'Last, 2, 0);
   Reject (0, Word, 0, 4096, 2);
   Reject (0, Word, 0, 4096, Unsigned_64'Last);
   for Bad of Bad_References loop
      Reject (0, Bad, 0, 4096, 0);
      Before := G.Returns;
      Status := Native_GPU_Memory.Return_Borrow (Bad);
      pragma Assert (Status = 1 and G.Returns = Before);
   end loop;
   Before := G.Calls;
   Status := Native_GPU_Memory.Acquire (0, Word, 0, 4096, 0, null);
   pragma Assert (Status = 1 and G.Calls = Before);
   G.Succeed := True;
   G.Expected_Slot := 63;
   G.Expected_Access := G.Write_Access;
   Before := G.Calls;
   C_Test;
   pragma Assert (G.Calls = Before + 1);
   Ada.Text_IO.Put_Line ("Native memory FFI PASS: 256 acquire/return cases; malformed inputs rejected before transport");
end Memory_Tests;
