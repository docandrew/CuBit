with Ada.Text_IO; with Interfaces; with Interfaces.C;
with System; with System.Storage_Elements;
with Intel_GPU_Native_GGTT;
procedure Native_GGTT_Tests is
   use Interfaces; use System; use System.Storage_Elements;
   use type Interfaces.C.int;
   Owner, Permit : Boolean := False;
   Bytes : Unsigned_64 := 0;
   Owner_Checks, Fail_Owner_Check : Natural := 0;
   Permit_Checks, Fail_Permit_Check : Natural := 0;
   function Ready return Boolean is
   begin
      Owner_Checks := Owner_Checks + 1;
      return Owner and then Owner_Checks /= Fail_Owner_Check;
   end Ready;
   function Size return Unsigned_64 is (Bytes);
   function Allowed (Index, Value : Unsigned_64) return Boolean is
   begin
      Permit_Checks := Permit_Checks + 1;
      return Permit and then Permit_Checks /= Fail_Permit_Check and then
        Index = 7 and then Value = 16#100001#;
   end Allowed;
   package IO is new Intel_GPU_Native_GGTT (Ready, Size, Allowed);
   function Mmap (Address : System.Address; Length : Interfaces.C.size_t;
                  Protection, Flags, FD : Interfaces.C.int;
                  Offset : Interfaces.C.long) return System.Address
     with Import, Convention => C, External_Name => "mmap";
   function Munmap (Address : System.Address; Length : Interfaces.C.size_t)
     return Interfaces.C.int with Import, Convention => C, External_Name => "munmap";
   Mapping : System.Address;
   Value : Unsigned_64; OK : Boolean;
   Entry_Value : Unsigned_64 with Import, Volatile_Full_Access,
     Address => To_Address (16#64000038#);
begin
   IO.Read_PTE (7, Value, OK); pragma Assert (not OK);
   IO.Write_PTE (7, 16#100001#, OK); pragma Assert (not OK);
   Mapping := Mmap (To_Address (16#64000000#), 2_097_152, 3, 16#100022#, -1, 0);
   if Mapping /= To_Address (16#64000000#) then
      if Mapping /= To_Address (Integer_Address'Last) then
         declare Ignored : constant Interfaces.C.int := Munmap (Mapping, 2_097_152); begin null; end;
      end if;
      raise Program_Error with "cannot reserve native GGTT host fixture";
   end if;
   Owner := True; Bytes := 2_097_152;
   IO.Read_PTE (7, Value, OK); pragma Assert (OK and Value = 0);
   IO.Write_PTE (7, 16#100001#, OK); pragma Assert (not OK and Entry_Value = 0);
   Permit := True;
   IO.Write_PTE (7, 16#100001#, OK); pragma Assert (OK and Entry_Value = 16#100001#);
   IO.Read_PTE (7, Value, OK); pragma Assert (OK and Value = 16#100001#);
   Entry_Value := 16#AB25_AB25_AB25_AB25#;
   IO.Write_PTE (7, 16#100001#, OK); pragma Assert (OK and Entry_Value = 16#100001#);
   -- Replay prevention belongs to the publisher attempt/ledger, not PTE bits.
   -- Losing the exact allocation permit between admission and the store
   -- must preserve even nonzero inherited contents.
   Entry_Value := 16#AB25_AB25_AB25_AB25#;
   Permit_Checks := 0; Fail_Permit_Check := 2;
   IO.Write_PTE (7, 16#100001#, OK);
   pragma Assert (not OK and Permit_Checks = 2 and
     Entry_Value = 16#AB25_AB25_AB25_AB25#);
   Fail_Permit_Check := 0;
   Entry_Value := 0;
   IO.Write_PTE (7, 16#100003#, OK); pragma Assert (not OK and Entry_Value = 0);
   IO.Write_PTE (7, 1, OK); pragma Assert (not OK);
   IO.Write_PTE (8, 16#100001#, OK); pragma Assert (not OK);
   IO.Read_PTE (2_097_152 / 8, Value, OK); pragma Assert (not OK);
   IO.Read_PTE (Unsigned_64'Last, Value, OK); pragma Assert (not OK);
   Entry_Value := Unsigned_64'Last;
   IO.Read_PTE (7, Value, OK); pragma Assert (not OK);
   IO.Write_PTE (7, 16#100001#, OK); pragma Assert (not OK and Entry_Value = Unsigned_64'Last);
   Entry_Value := 0; Owner := False;
   IO.Write_PTE (7, 16#100001#, OK); pragma Assert (not OK and Entry_Value = 0);
   Owner := True; Owner_Checks := 0; Fail_Owner_Check := 2;
   IO.Write_PTE (7, 16#100001#, OK); pragma Assert (not OK and Entry_Value = 0);
   Owner_Checks := 0; Fail_Owner_Check := 3;
   IO.Write_PTE (7, 16#100001#, OK);
   pragma Assert (not OK and Entry_Value = 16#100001#); -- ambiguous store: retain backing
   Fail_Owner_Check := 0;
   Bytes := Unsigned_64'Last;
   IO.Read_PTE (7, Value, OK); pragma Assert (not OK);
   pragma Assert (Munmap (Mapping, 2_097_152) = 0);
   Ada.Text_IO.Put_Line ("native GGTT PASS: host volatile reads/stores, bounds, exact owned replacement (NOT GPU hardware)");
end Native_GGTT_Tests;
