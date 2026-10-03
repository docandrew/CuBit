with Ada.Text_IO;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_Application_State;
with Intel_GPU_Metadata_Arena;
with Intel_GPU_Metadata_Initialize;
with Intel_GPU_Update_Storage;
with Intel_GPU_Table_Provenance;
procedure Update_Storage_Tests is
   package S renames Intel_GPU_Application_State;
   package P renames Intel_GPU_Table_Provenance;
   procedure Resolve_Page (Session, Ticket, Offset : Unsigned_64;
                           CPU, DMA : out Unsigned_64; Accepted : out Boolean) is
   begin
      Accepted := Session = 42 and Ticket in 17 .. 18 and Offset = 0;
      CPU := 16#10000000# + Ticket * 4096; DMA := Ticket * 4096;
   end Resolve_Page;
   package A is new P.Authority (Resolve_Page);
   type RAM is array (Natural range <>) of Unsigned_64;
   Index_RAM : RAM (0 .. 1023) := [others => 0] with Alignment => 4096;
   Ledger_Bytes : constant Unsigned_64 :=
     ((Unsigned_64 (S.Table_Pages) * Unsigned_64 (P.Mapping'Object_Size / 8) + 4095) / 4096 * 4096);
   Mirror_Bytes : constant Unsigned_64 :=
     Unsigned_64 (S.Table_Pages - S.Bootstrap_Table_Mirrors) * 4096;
   Bytes : constant Unsigned_64 := S.Update_Storage_Bytes + Ledger_Bytes + Mirror_Bytes;
   Total : constant Unsigned_64 := 2 * Bytes + Ledger_Bytes + Mirror_Bytes;
   Image_RAM : RAM (0 .. Natural (Total / 8) + 511) := [others => 16#CAFE#]
     with Alignment => 4096;
   Base : constant Unsigned_64 := Unsigned_64 (To_Integer (Image_RAM'Address));
   Commits, Reserves : Natural := 0;
   Committed : Unsigned_64 := 0;
   Owner : Boolean := True;
   function Owner_Ready return Boolean is (Owner);
   function Reserve (Count : Unsigned_64) return Unsigned_64 is
   begin
      pragma Assert (Count = Total);
      Reserves := Reserves + 1; return Base;
   end Reserve;
   function Commit (Address, Offset, Count : Unsigned_64) return Boolean is
   begin
      pragma Assert (Address = Base and Offset = Committed and Count <= 65536);
      Committed := Offset + Count;
      Commits := Commits + 1; return True;
   end Commit;
   package M is new Intel_GPU_Metadata_Arena
     (Reserve, Commit, Intel_GPU_Metadata_Initialize.Clear);
   package U is new Intel_GPU_Update_Storage (M, Owner_Ready);
   Object : U.Pool;
   OK : Boolean;
   Before : Natural;
begin
   S.Extend_Update_Index (Unsigned_64 (To_Integer (Index_RAM'Address)), 4096, OK);
   pragma Assert (OK);
   pragma Assert (U.Table_Metadata_Bytes = Ledger_Bytes);
   pragma Assert (U.Mirror_Metadata_Bytes = Mirror_Bytes);
   pragma Assert (S.VM.Metadata_Capacity (S.Updates (1).Candidate) = 4);
   U.Request (Object, 1, Total, OK);
   pragma Assert (OK and U.Pending (Object) and not U.Ready (Object));
   for Turn in 1 .. 20 loop
      U.Step (Object);
      exit when not U.Pending (Object);
   end loop;
   pragma Assert (U.Ready (Object) and P.Capacity (S.Updates (1).Table_Owners) >= S.Table_Pages);
   pragma Assert (S.VM.Metadata_Capacity (S.Updates (1).Candidate) = S.Table_Pages);
   for I in 17 .. 18 loop
      U.Request (Object, I, Total, OK); pragma Assert (OK and U.Pending (Object));
      for Turn in 1 .. 20 loop
         pragma Assert (not U.Ready (Object));
         Before := Commits;
         U.Step (Object);
         pragma Assert (Commits <= Before + 1);
         exit when not U.Pending (Object);
      end loop;
      pragma Assert (U.Ready (Object) and S.Has_Update (I));
      pragma Assert (S.VM.Metadata_Capacity (S.Updates (I).Candidate) = S.Table_Pages);
      pragma Assert (not S.VM.Sealed (S.Updates (I).Candidate) and
        S.VM.Used (S.Updates (I).Candidate) = 0 and not S.Updates (I).Tables.Ready);
      pragma Assert (P.Count (S.Updates (I).Table_Owners) = 0 and
                     P.Capacity (S.Updates (I).Table_Owners) >= S.Table_Pages);
      A.Install (S.Updates (I).Table_Owners, 42, 1, 1, Unsigned_64 (I), 0, OK);
      pragma Assert (OK);
   end loop;
   pragma Assert (Reserves = 1 and Committed = Total);
   pragma Assert (A.Lookup (S.Updates (17).Table_Owners, 42, 1, 1).Ticket = 17 and
                  A.Lookup (S.Updates (18).Table_Owners, 42, 1, 1).Ticket = 18);
   pragma Assert (P.Count (S.Items (1).Table_Owners) = 0);
   Before := Commits;
   U.Request (Object, 17, Total, OK);
   pragma Assert (OK and U.Ready (Object) and not U.Pending (Object) and Commits = Before);
   U.Request (Object, 19, Total, OK);
   pragma Assert (not OK and not U.Ready (Object) and not S.Has_Update (19));
   for Turn in 1 .. 10 loop U.Step (Object); end loop;
   pragma Assert (Commits = Before);
   U.Request (Object, 19, Total, OK); pragma Assert (not OK and Reserves = 1);
   for I in Natural (Total / 8) .. Image_RAM'Last loop
      pragma Assert (Image_RAM (I) = 16#CAFE#);
   end loop;
   Ada.Text_IO.Put_Line ("Update storage PASS: inline/demand ledger metadata, two images, one reservation, bounded commits, readiness after attachment, reuse, quota failure/no replay, guard intact");
end Update_Storage_Tests;
