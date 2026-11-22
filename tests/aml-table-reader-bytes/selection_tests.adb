pragma Ada_2022;
with Ada.Text_IO;
with AML_Table_Backing;
with AML_Table_Readers;
with AML_Table_Sources;
with AML_Copied_Tables;
with Firmware_Tables.Identifiers;
procedure Selection_Tests is
   package F renames Firmware_Tables;
   package I renames F.Identifiers;
   package B renames AML_Table_Backing;
   package R renames AML_Table_Readers;
   package S renames AML_Table_Sources;
   package C renames AML_Copied_Tables;
   use type F.Byte;
   use type R.Selection_Status;
   use type S.Source_Kind;
   use type S.Boot_Table_ID;
   use type S.Copied_Table_ID;
   use type C.Outcome;
   use type C.Table_ID;
   use type C.Store_Model;
   use type B.Spans;
   use type F.Bytes;
   Header_Size : constant Positive := F.Table_Header_Size;
   Boot_Capacity : constant Positive := 4;
   Copy_Capacity : constant Positive := 3;
   type Identities is array (Positive range <>) of I.Identity;
   Boot_Identities : constant Identities :=
     [("TEST", "OEMAAA", "TABLEONE"), ("TEST", "OEMBBB", "TABLETWO"),
      ("ABCD", "OEMAAA", "TABLEONE"), ("TEST", "OEMAAA", "TABLEONE")];
   Copy_Identities : constant Identities :=
     [("TEST", "OEMAAA", "TABLEONE"), ("TEST", "OEMCCC", "TABLETHR"),
      ("ABCD", "OEMCCC", "TABLETHR")];
   Boot : aliased B.State (Boot_Capacity, Boot_Capacity * Header_Size);
   Copies : C.State (Copy_Capacity, Copy_Capacity * Header_Size);
   Checks : Natural := 0;
   procedure Check (Condition : Boolean; Message : String) is
   begin
      if not Condition then raise Program_Error with Message; end if;
      Checks := Checks + 1;
   end Check;
   function Header (Identity : I.Identity) return F.Bytes is
      Data : F.Bytes (1 .. Header_Size) := [others => 0];
      Sum : F.Byte := 0;
      Length_Offset : constant Natural := 4;
      Revision_Index : constant Positive := 9;
      Checksum_Index : constant Positive := 10;
      OEM_Offset : constant Natural := 10;
      OEM_Table_Offset : constant Natural := 16;
   begin
      Data (Length_Offset + 1) := F.Byte (Header_Size);
      Data (Revision_Index) := 2;
      for J in Identity.Name'Range loop Data (J) := Character'Pos (Identity.Name (J)); end loop;
      for J in Identity.OEM'Range loop Data (OEM_Offset + J) := Character'Pos (Identity.OEM (J)); end loop;
      for J in Identity.OEM_Table'Range loop Data (OEM_Table_Offset + J) := Character'Pos (Identity.OEM_Table (J)); end loop;
      for Octet of Data loop Sum := Sum + Octet; end loop;
      Data (Checksum_Index) := 0 - Sum;
      return Data;
   end Header;
   function Model_Match (Actual : I.Identity; Query : I.Selection) return Boolean is
     (Actual.Name = Query.Name and then
      (not Query.Match_OEM or else Actual.OEM = Query.OEM) and then
      (not Query.Match_OEM_Table or else Actual.OEM_Table = Query.OEM_Table));
   procedure Query_Is (Query : I.Selection) is
      Expected_Kind : S.Source_Kind := S.No_Source;
      Expected_ID : Natural := 0;
      Before_Tables : constant B.Spans := Boot.Tables;
      Before_Data : constant F.Bytes := Boot.Data.Bytes;
      Before_Count : constant Natural := Boot.Count;
      Before_Copies : constant C.Store_Model := C.Model (Copies) with Ghost;
      Found : constant R.Selection_Result := R.Find_Table (Boot, Copies, Query);
   begin
      for J in 1 .. Boot.Count loop
         if Model_Match (Boot_Identities (J), Query) then
            Expected_Kind := S.Boot_Source; Expected_ID := J; exit;
         end if;
      end loop;
      if Expected_ID = 0 then
         for J in 1 .. C.Count (Copies) loop
            if Model_Match (Copy_Identities (J), Query) then
               Expected_Kind := S.Copied_Source; Expected_ID := J; exit;
            end if;
         end loop;
      end if;
      if Expected_ID = 0 then Check (Found.Status = R.No_Matching_Table, "absent query");
      else
         Check (Found.Status = R.Table_Selected, "selected query");
         Check (Found.Item.Kind = Expected_Kind and then Found.Extent = Header_Size, "source identity and extent");
         case Expected_Kind is
            when S.Boot_Source => Check (Found.Item.Boot = S.Boot_Table_ID (Expected_ID), "first boot slot");
            when S.Copied_Source => Check (Found.Item.Copied = S.Copied_Table_ID (Expected_ID), "first copy slot");
            when S.No_Source => raise Program_Error;
         end case;
      end if;
      Check (Boot.Count = Before_Count and then Boot.Tables = Before_Tables
        and then Boot.Data.Bytes = Before_Data, "boot input unchanged");
      pragma Assert (C.Model (Copies) = Before_Copies);
   end Query_Is;
   procedure Matrix is
      Names : constant array (Positive range <>) of F.Signature := ["TEST", "ABCD", "NONE"];
      OEMs : constant array (Positive range <>) of I.OEM_Name := ["OEMAAA", "OEMBBB", "OEMCCC", "ABSENT"];
      Tables : constant array (Positive range <>) of I.OEM_Table_Name := ["TABLEONE", "TABLETWO", "TABLETHR", "ABSENT00"];
   begin
      for Name of Names loop
         for OEM of OEMs loop
            for Table of Tables loop
               for Match_OEM in Boolean loop
                  for Match_Table in Boolean loop
                     Query_Is ((Name, Match_OEM, OEM, Match_Table, Table));
                  end loop;
               end loop;
            end loop;
         end loop;
      end loop;
   end Matrix;
   Report : C.Report;
   Query : constant I.Selection := (Name => "TEST", others => <>);
begin
   Boot.Tables := [others => (0, 0)]; Boot.Data.Bytes := [others => 0];
   for J in Boot_Identities'Range loop
      Boot.Tables (J) := ((J - 1) * Header_Size, Header_Size);
      Boot.Data.Bytes ((J - 1) * Header_Size + 1 .. J * Header_Size) := Header (Boot_Identities (J));
   end loop;
   Matrix;
   for J in Copy_Identities'Range loop
      C.Retain (Copies, Header (Copy_Identities (J)), 10000, Report);
      Check (Report.Status = C.Added and then Report.ID = C.Table_ID (J), "copy admission");
      Matrix;
   end loop;
   for Count in 1 .. Boot_Capacity loop Boot.Count := Count; Matrix; end loop;
   -- A malformed later span invalidates the inventory even when slot 1 matches.
   Boot.Tables (Boot_Capacity).Extent := Header_Size - 1;
   Check (R.Find_Table (Boot, Copies, Query).Status = R.Invalid_Selection_Inventory, "later short span");
   Boot.Tables (Boot_Capacity) := (Boot.Byte_Capacity, Header_Size);
   Check (R.Find_Table (Boot, Copies, Query).Status = R.Invalid_Selection_Inventory, "span outside backing");
   Boot.Tables (Boot_Capacity) := ((Boot_Capacity - 1) * Header_Size, Header_Size);
   Boot.Count := Boot_Capacity + 1;
   Check (R.Find_Table (Boot, Copies, Query).Status = R.Invalid_Selection_Inventory, "count exceeds quota");
   Boot.Count := 0;
   Matrix; -- Unused malformed boot slots are outside the active inventory.
   Ada.Text_IO.Put_Line ("Selection checks" & Checks'Image);
end Selection_Tests;
