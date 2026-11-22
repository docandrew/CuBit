with Ada.Text_IO;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_Application_State;
procedure Application_State_Tests is
   package S renames Intel_GPU_Application_State;
   use type S.Update_Access;
   type Memory is array (Natural range <>) of Unsigned_64;
   Metadata : Memory (0 .. 1535) := [others => 16#CAFE#] with Alignment => 4096;
   Base : constant Unsigned_64 := Unsigned_64 (To_Integer (Metadata'Address));
   One, Two : aliased S.Update_Record;
   Late : aliased S.Update_Record with Alignment => 4096;
   First : constant S.Update_Access := S.Updates (1);
   OK : Boolean;
   Image_Bytes : constant Unsigned_64 := S.Update_Storage_Bytes;
   Storage : Memory (0 .. Natural (Image_Bytes / 8) + 511) := [others => 16#CAFE#]
     with Alignment => 4096;
   Image_Base : constant Unsigned_64 := Unsigned_64 (To_Integer (Storage'Address));
   Shared_RAM : Memory (0 .. Natural (Image_Bytes / 8) - 1) := [others => 16#CAFE#]
     with Alignment => 4096;
   Shared_Base : constant Unsigned_64 := Unsigned_64 (To_Integer (Shared_RAM'Address));
   use type S.Installation_Phase;
   -- Test-only drain; production advances one bounded step per service turn.
   procedure Install_Fresh (Index : Positive; Address, Bytes : Unsigned_64;
                            Accepted : out Boolean) is
      Attempt : S.Installation;
      Before : Natural;
   begin
      S.Begin_Fresh_Update (Attempt, Index, Address, Bytes, Accepted);
      while S.State (Attempt) in S.Checking | S.Publishing loop
         Before := S.Checked (Attempt);
         S.Step_Fresh_Update (Attempt);
         pragma Assert (S.Checked (Attempt) - Before <= 64);
      end loop;
      Accepted := S.State (Attempt) = S.Complete;
   end Install_Fresh;
begin
   pragma Assert (S.Update_Capacity = S.Bootstrap_Updates);
   pragma Assert (First /= null and not S.Has_Update (17));
   S.Install_Update (17, One'Unchecked_Access, OK);
   pragma Assert (not OK);
   S.Extend_Update_Index (Base, 4096, OK);
   pragma Assert (OK and S.Update_Capacity > 256);
   pragma Assert (S.Updates (1) = First);
   for I in 17 .. S.Update_Capacity loop
      pragma Assert (not S.Has_Update (I));
   end loop;
   -- Only supplied images exist; growing hundreds of tickets creates none.
   S.Install_Update (17, One'Unchecked_Access, OK);
   pragma Assert (OK and S.Updates (17) = One'Unchecked_Access);
   S.Install_Update (256, Two'Unchecked_Access, OK);
   pragma Assert (OK and S.Updates (256) = Two'Unchecked_Access);
   S.Install_Update (18, One'Unchecked_Access, OK);
   pragma Assert (not OK and not S.Has_Update (18));
   S.Install_Update (17, Two'Unchecked_Access, OK);
   pragma Assert (not OK and S.Updates (17) = One'Unchecked_Access);
   S.Install_Update (18, First, OK);
   pragma Assert (not OK);
   S.Install_Update (18, null, OK);
   pragma Assert (not OK);
   S.Extend_Update_Index (Base, 8192, OK);
   pragma Assert (OK and S.Updates (17) = One'Unchecked_Access);
   pragma Assert (S.Updates (256) = Two'Unchecked_Access and S.Updates (1) = First);
   pragma Assert (not S.Has_Update (S.Update_Capacity + 1));
   pragma Assert (not S.VM.Sealed (S.Updates (17).Candidate));
   pragma Assert (not S.Updates (17).Tables.Ready);
   Install_Fresh (18, Image_Base + 1, Image_Bytes, OK);
   pragma Assert (not OK and Storage (0) = 16#CAFE#);
   Install_Fresh (18, Image_Base, Image_Bytes - 4096, OK);
   pragma Assert (not OK and Storage (0) = 16#CAFE#);
   declare Attempt : S.Installation; begin
      S.Begin_Fresh_Update (Attempt, 18, Image_Base, Image_Bytes, OK);
      pragma Assert (OK);
      S.Step_Fresh_Update (Attempt);
      pragma Assert (S.State (Attempt) = S.Publishing and S.Checked (Attempt) in 1 .. 64);
      pragma Assert (not S.Has_Update (18) and Storage (0) = 16#CAFE#);
      S.Extend_Update_Index (Base, 12288, OK); pragma Assert (OK);
      S.Step_Fresh_Update (Attempt);
      pragma Assert (S.State (Attempt) = S.Failed and S.Checked (Attempt) in 1 .. 64);
      S.Step_Fresh_Update (Attempt);
      pragma Assert (not S.Has_Update (18) and Storage (0) = 16#CAFE#);
      S.Begin_Fresh_Update (Attempt, 18, Image_Base, Image_Bytes, OK);
      pragma Assert (not OK);
   end;
   Install_Fresh (18, Image_Base, Image_Bytes, OK);
   pragma Assert (OK and S.Has_Update (18));
   pragma Assert (not S.Updates (18).Tables.Ready and not S.VM.Sealed (S.Updates (18).Candidate));
   pragma Assert (S.VM.Used (S.Updates (18).Candidate) = 0 and
     S.VM.Revision (S.Updates (18).Candidate) = 0);
   Install_Fresh (19, Image_Base, Image_Bytes, OK);
   pragma Assert (not OK and not S.Has_Update (19));
   Install_Fresh (19, Image_Base + 4096, Image_Bytes, OK);
   pragma Assert (not OK and not S.Has_Update (19));
   for I in Natural (Image_Bytes / 8) .. Storage'Last loop
      pragma Assert (Storage (I) = 16#CAFE#);
   end loop;
   S.Install_Update (1500, Late'Unchecked_Access, OK); pragma Assert (OK);
   declare
      Attempt : S.Installation;
      Before : Natural;
   begin
      S.Begin_Fresh_Update (Attempt, 19,
        Unsigned_64 (To_Integer (Late'Address)), Image_Bytes, OK);
      pragma Assert (OK);
      while S.State (Attempt) in S.Checking | S.Publishing loop
         Before := S.Checked (Attempt);
         S.Step_Fresh_Update (Attempt);
         pragma Assert (S.Checked (Attempt) - Before <= 64);
      end loop;
      pragma Assert (S.State (Attempt) = S.Failed and S.Checked (Attempt) in 1 .. 64);
      pragma Assert (not S.Has_Update (19) and S.Updates (1500) = Late'Unchecked_Access);
   end;
   -- Serialized service requests may both preflight before either publishes.
   -- Their backing outlives all registry accesses, including failed attempts.
   declare
      A, B : S.Installation;
   begin
      S.Begin_Fresh_Update (A, 20, Shared_Base, Image_Bytes, OK); pragma Assert (OK);
      S.Begin_Fresh_Update (B, 21, Shared_Base, Image_Bytes, OK); pragma Assert (OK);
      S.Step_Fresh_Update (A); S.Step_Fresh_Update (B);
      pragma Assert (S.State (A) = S.Publishing and S.State (B) = S.Publishing);
      S.Step_Fresh_Update (A); pragma Assert (S.State (A) = S.Complete);
      S.Updates (20).Table_Generation := 99;
      S.Step_Fresh_Update (B);
      pragma Assert (S.State (B) = S.Failed and not S.Has_Update (21));
      pragma Assert (S.Updates (20).Table_Generation = 99);
      S.Step_Fresh_Update (A); S.Step_Fresh_Update (B);
      pragma Assert (S.Updates (20).Table_Generation = 99);
   end;
   Ada.Text_IO.Put_Line ("Interleaved installation PASS: same-span preflights, one publication, stale loser cannot reinitialize winner");
   Ada.Text_IO.Put_Line ("Sparse VM updates PASS: index growth does not allocate images, stable references, duplicate/alias/null/uncommitted rejection");
   Ada.Text_IO.Put_Line ("Fresh VM storage PASS: typed initialization before publication, short/misaligned/overlapping spans rejected, neighboring guard intact");
   Ada.Text_IO.Put_Line ("Fresh installation PASS: <=64 probes per step, delayed publication, epoch change fails without initialization/replay");
   Ada.Text_IO.Put_Line ("Sparse overlap PASS: alias at index1500 rejected with <=64 range probes, no empty-slot scan");
   -- Test-owned images/metadata outlive every queue access; native owners must
   -- retain them for the service lifetime. This does not test native allocation.
end Application_State_Tests;
