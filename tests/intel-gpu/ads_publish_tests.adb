with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_ADS_Initialization;
with Intel_GPU_ADS_Materialize;
with Intel_GPU_ADS_Layout;
with Intel_GPU_ADLN_Inventory;
with Intel_GPU_ADLN_Steering;
with Intel_GPU_GGTT_Reservations;
with Intel_GPU_GGTT_Publish;
procedure ADS_Publish_Tests is
   Capacity : constant Unsigned_64 := 16 * 1024 * 1024;
   Selected : constant Unsigned_64 := 2 * Capacity;
   DMA : constant Unsigned_64 := 16#10000000#;
   Buffer : Intel_GPU_ADS_Materialize.Bytes (0 .. Natural (Capacity) - 1);
   Reservations : Intel_GPU_GGTT_Reservations.Ledger;
   Reads, Writes : Natural := 0;
   Prepared : Boolean := False;
   function Range_Allowed (First, Bytes : Unsigned_64) return Boolean is
     (First >= Capacity and then First <= 3 * Capacity and then
      Bytes <= 3 * Capacity - First);
   procedure Prepare (GPU_Start, Bytes : Unsigned_64; Success : out Boolean) is
      Image : constant Intel_GPU_ADS_Initialization.Prepared_Image :=
        Intel_GPU_ADS_Initialization.Prepare
          (Intel_GPU_ADLN_Inventory.Decode (16#8086#, 16#46D2#, 0),
           Intel_GPU_ADLN_Steering.Decode (1, 63, 0), 0, 0, 16#1000000#,
           GPU_Start, Bytes, 16#801000#);
      Pointer : Unsigned_32 := 0;
   begin
      pragma Assert (GPU_Start = Selected and Bytes = Capacity);
      pragma Assert (Intel_GPU_GGTT_Reservations.Count (Reservations) = 2);
      pragma Assert (Reads = 4096 and Writes = 0 and Image.Valid);
      Intel_GPU_ADS_Materialize.Write (Image, Buffer, Success);
      pragma Assert (Success);
      for I in 0 .. 3 loop
         Pointer := Pointer or Shift_Left (Unsigned_32 (Buffer (4100 + I)), I * 8);
      end loop;
      pragma Assert (Unsigned_64 (Pointer) = Selected +
        Image.Layout.Offset (Intel_GPU_ADS_Layout.Policies));
      Prepared := True;
      -- Hosted byte preparation only; this callback models device visibility.
   end Prepare;
   procedure Read_PTE (Index : Unsigned_64; Value : out Unsigned_64;
                       Success : out Boolean) is
   begin
      pragma Assert (Index in Selected / 4096 .. Selected / 4096 + 4095);
      Reads := Reads + 1;
      Value := (if Writes = 0 then 0 else DMA + (Index - Selected / 4096) * 4096 + 1);
      Success := True;
   end Read_PTE;
   procedure Write_PTE (Index, Value : Unsigned_64; Success : out Boolean) is
   begin
      pragma Assert (Prepared and Reads = 4096);
      pragma Assert (Index = Selected / 4096 + Unsigned_64 (Writes));
      pragma Assert (Value = DMA + Unsigned_64 (Writes) * 4096 + 1);
      Writes := Writes + 1;
      Success := True;
   end Write_PTE;
   procedure Invalidate (Success : out Boolean) is
   begin
      pragma Assert (Reads = 8192 and Writes = 4096 and Prepared);
      Success := True;
   end Invalidate;
   package Publication is new Intel_GPU_GGTT_Publish
     (Range_Allowed, Prepare, Read_PTE, Write_PTE, Invalidate, Capacity);
   Object : Publication.Attempt;
   Status : Publication.Result;
   Claim : Intel_GPU_GGTT_Reservations.Result;
   Address : Unsigned_64;
   OK : Boolean;
   use type Publication.Result;
   use type Intel_GPU_GGTT_Reservations.Result;
begin
   Intel_GPU_GGTT_Reservations.Admit
     (Reservations, 16#200000#, Capacity, 2 * Capacity, OK);
   pragma Assert (OK);
   Intel_GPU_GGTT_Reservations.Reserve (Reservations, Capacity, Capacity, Claim);
   pragma Assert (Claim = Intel_GPU_GGTT_Reservations.Reserved);
   Publication.Publish_Available
     (Object, Reservations, DMA, Capacity, 4096, Address, Status);
   pragma Assert (Status = Publication.Published and Address = Selected);
   pragma Assert (Reads = 8192 and Writes = 4096);
   Ada.Text_IO.Put_Line
     ("ADS publish PASS: reserved dynamic address encoded into real 16MiB ADS before modeled PTE publication");
end ADS_Publish_Tests;
