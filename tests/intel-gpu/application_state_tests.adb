with Ada.Text_IO;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_Application_State;
procedure Application_State_Tests is
   package S renames Intel_GPU_Application_State;
   use type S.Update_Access;
   type Memory is array (Natural range <>) of Unsigned_64;
   Metadata : Memory (0 .. 1023) := [others => 16#CAFE#] with Alignment => 4096;
   Base : constant Unsigned_64 := Unsigned_64 (To_Integer (Metadata'Address));
   One, Two : aliased S.Update_Record;
   First : constant S.Update_Access := S.Updates (1);
   OK : Boolean;
   Image_Bytes : constant Unsigned_64 := S.Update_Storage_Bytes;
   Storage : Memory (0 .. Natural (Image_Bytes / 8) + 511) := [others => 16#CAFE#]
     with Alignment => 4096;
   Image_Base : constant Unsigned_64 := Unsigned_64 (To_Integer (Storage'Address));
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
   S.Install_Fresh_Update (18, Image_Base + 1, Image_Bytes, OK);
   pragma Assert (not OK and Storage (0) = 16#CAFE#);
   S.Install_Fresh_Update (18, Image_Base, Image_Bytes - 4096, OK);
   pragma Assert (not OK and Storage (0) = 16#CAFE#);
   S.Install_Fresh_Update (18, Image_Base, Image_Bytes, OK);
   pragma Assert (OK and S.Has_Update (18));
   pragma Assert (not S.Updates (18).Tables.Ready and not S.VM.Sealed (S.Updates (18).Candidate));
   pragma Assert (S.VM.Used (S.Updates (18).Candidate) = 0 and
     S.VM.Revision (S.Updates (18).Candidate) = 0);
   S.Install_Fresh_Update (19, Image_Base, Image_Bytes, OK);
   pragma Assert (not OK and not S.Has_Update (19));
   S.Install_Fresh_Update (19, Image_Base + 4096, Image_Bytes, OK);
   pragma Assert (not OK and not S.Has_Update (19));
   for I in Natural (Image_Bytes / 8) .. Storage'Last loop
      pragma Assert (Storage (I) = 16#CAFE#);
   end loop;
   Ada.Text_IO.Put_Line ("Sparse VM updates PASS: index growth does not allocate images, stable references, duplicate/alias/null/uncommitted rejection");
   Ada.Text_IO.Put_Line ("Fresh VM storage PASS: typed initialization before publication, short/misaligned/overlapping spans rejected, neighboring guard intact");
   -- Test-owned images/metadata outlive every queue access; native owners must
   -- retain them for the service lifetime. This does not test native allocation.
end Application_State_Tests;
