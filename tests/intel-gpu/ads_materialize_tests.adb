with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_ADS_Initialization; use Intel_GPU_ADS_Initialization;
with Intel_GPU_ADS_Materialize; use Intel_GPU_ADS_Materialize;
with Intel_GPU_ADS_Layout; use Intel_GPU_ADS_Layout;
with Intel_GPU_ADLN_Inventory;
with Intel_GPU_ADLN_Steering;
procedure ADS_Materialize_Tests is
   Capacity : constant := 16 * 1024 * 1024;
   -- Hosted test storage only; production receives already-owned backing.
   Buffer : Bytes (7 .. Capacity + 6) := [others => 16#A5#];
   Image : constant Prepared_Image := Prepare
     (Intel_GPU_ADLN_Inventory.Decode (16#8086#, 16#46D2#, 0),
      Intel_GPU_ADLN_Steering.Decode (1, 63, 0), 0, 0, 16#1000000#,
      16#100000#, Capacity, 16#801000#);
   Bad : Prepared_Image;
   OK : Boolean;
   Expected : Unsigned_8;
begin
   pragma Assert (Image.Valid);
   Write (Image, Buffer, OK);
   pragma Assert (OK);
   for I in Natural range 0 .. Capacity - 1 loop
      Expected := 0;
      if I < Image.Header'Length then Expected := Image.Header (I);
      elsif Unsigned_64 (I) in Image.Layout.Offset (Policies) ..
        Image.Layout.Offset (Policies) + Unsigned_64 (Image.Policies'Length) - 1
      then Expected := Image.Policies (I - Natural (Image.Layout.Offset (Policies)));
      elsif Unsigned_64 (I) in Image.Layout.Offset (System_Info) ..
        Image.Layout.Offset (System_Info) + Unsigned_64 (Image.System_Info.Bytes'Length) - 1
      then Expected := Image.System_Info.Bytes (I - Natural (Image.Layout.Offset (System_Info)));
      elsif Unsigned_64 (I) in Image.Layout.Offset (Registers) ..
        Image.Layout.Offset (Registers) + Unsigned_64 (Image.Registers.Registers'Length) - 1
      then Expected := Image.Registers.Registers (I - Natural (Image.Layout.Offset (Registers)));
      elsif Unsigned_64 (I) in Image.Layout.Offset (Capture) ..
        Image.Layout.Offset (Capture) + Unsigned_64 (Image.Capture.Data'Length) - 1
      then Expected := Image.Capture.Data (I - Natural (Image.Layout.Offset (Capture)));
      end if;
      pragma Assert (Buffer (I + Buffer'First) = Expected);
   end loop;
   Buffer := [others => 16#A5#];
   Write (Image, Buffer (7 .. 7 + Natural (Image.Layout.Total) - 2), OK);
   pragma Assert (not OK and then (for all B of Buffer => B = 16#A5#));
   for Case_Number in 0 .. 2 loop
      Bad := Image;
      case Case_Number is
         when 0 => Bad.Layout.Bytes (Header) := 1;
         when 1 => Bad.Policies (76) := 0;
         when others => Bad.Valid := False;
      end case;
      Write (Bad, Buffer, OK);
      pragma Assert (not OK and then (for all B of Buffer => B = 16#A5#));
   end loop;
   Ada.Text_IO.Put_Line ("ADS materialize: PASS (16MiB exact bytes, shifted bounds, zero padding, no-write rejection)");
end ADS_Materialize_Tests;
