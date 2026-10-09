with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_Extent_Directory; use Intel_GPU_Extent_Directory;
with Intel_GPU_Physical_Extents;
procedure Extent_Directory_Scale_Tests is
   Block : constant Unsigned_64 := Intel_GPU_Physical_Extents.Block_Bytes;
   Count : constant Positive := 524_288;
   type Metadata is array (1 .. Count * 16) of Unsigned_8 with Alignment => 4096;
   type Metadata_Access is access Metadata;
   procedure Free is new Ada.Unchecked_Deallocation (Metadata, Metadata_Access);
   Storage : Metadata_Access := new Metadata'(others => 16#A5#);
   Base : constant Unsigned_64 := Unsigned_64 (To_Integer (Storage.all'Address));
   Pool : aliased Directory;
   Prefix : Borrowed_View;
   OK : Boolean;
   Bytes : Unsigned_64 := 0;
   Growths : Natural := 0;
   Maximum_Probes : Natural := 0;
   function Key (Index : Natural) return Unsigned_64 is
     (2 ** 40 + ((Unsigned_64 (Index) * 2053) mod Unsigned_64 (Count)) * 2 * Block);
begin
   Initialize (Pool, Unsigned_64 (Count) * Block, 2 ** 48, OK);
   pragma Assert (OK and Committed_Bytes (Pool) = 0);
   for Index in 0 .. Count - 1 loop
      if Index = Metadata_Capacity (Pool) then
         Bytes := Bytes + 65536;
         Extend_Metadata (Pool, Base, Bytes, OK);
         pragma Assert (OK);
         Growths := Growths + 1;
         pragma Assert (Resolve (Prefix, 0, 4096).Address = Key (0));
      end if;
      Append (Pool, Key (Index), OK);
      pragma Assert (OK and Last_Admission_Probes (Pool) <= Max_Admission_Probes);
      Maximum_Probes := Natural'Max (Maximum_Probes, Last_Admission_Probes (Pool));
      if Index = 0 then Prefix := Borrow (Pool); end if;
      if Index mod 4096 = 0 then
         Append (Pool, Key (Index), OK);
         pragma Assert (not OK and Last_Admission_Probes (Pool) <= Max_Admission_Probes);
         pragma Assert (Committed_Bytes (Pool) = Unsigned_64 (Index + 1) * Block);
      end if;
   end loop;
   pragma Assert (Committed_Bytes (Pool) = 2 ** 40 and Growths = 128);
   for Index in 0 .. Count - 1 loop
      declare
         Span : constant Intel_GPU_Physical_Extents.Span :=
           Resolve (Borrow (Pool), Unsigned_64 (Index) * Block + Block - 1,
             (if Index = Count - 1 then 1 else 2));
      begin
         pragma Assert (Span.Valid and Span.Address = Key (Index) + Block - 1 and Span.Bytes = 1);
      end;
   end loop;
   pragma Assert (Byte_Count (Prefix) = Block and not Resolve (Prefix, Block, 1).Valid);
   pragma Assert (not Resolve (Borrow (Pool), 2 ** 40 - 1, 2).Valid);
   Append (Pool, 4 * 2 ** 40, OK);
   pragma Assert (not OK and Committed_Bytes (Pool) = 2 ** 40);
   Quarantine (Pool);
   pragma Assert (not Valid (Prefix));
   -- All views cease use before releasing hosted metadata. This is not a
   -- production physical-backing retirement implementation.
   Free (Storage);
   Ada.Text_IO.Put_Line ("PASS synthetic1TiB directory:524288extents128growths max-probes=" &
     Natural'Image (Maximum_Probes) & " stable-prefix and all addresses checked; NO GPU/backing allocation");
end Extent_Directory_Scale_Tests;
