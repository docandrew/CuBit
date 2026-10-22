with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Delays;
with AML_Namespace;
with AML_Table_Backing;
with Root_Test_Support; use Root_Test_Support;
procedure Field_Descriptor_Tests is
   package N is new AML_Namespace
     (Capacity => 48, Perform_Delay => AML_Delays.Unavailable_Provider);
   package C is new N.Owned.Collecting;
   pragma Compile_Time_Error
     (N.Region_Access_State'Pos (N.No_Access) /= 0
      or else N.Region_Access_State'Pos (N.Region_Access_State'First) /= 0
      or else N.Region_Access_State'Pos (N.Region_Access_State'Last) /= 0,
      "Region access domain must contain exactly No_Access");
   use type Bytes;
   use type Byte;
   use type Integer_Value;
   use type N.Region_Space_ID;
   use type N.Field_Bit_Position;
   use type C.Access_Status;
   use type C.Module_Outcome;
   use type C.Field_Description;
   use type C.Field_Key;
   use type C.Usage_Description;
   use type C.Audit_Value;
   A, B : C.Arena;
   Status : C.Access_Status;
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Label : String) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Label; end if;
   end Check;
   function Table (Code : Bytes; Width : Integer_Width; Signature : String := "DSDT") return Bytes is
      Data : Bytes (1 .. 36 + Code'Length) := [others => 0];
      Sum : Byte := 0;
   begin
      Data (1 .. 4) := Enc (Signature);
      for I in 1 .. 4 loop Data (4 + I) := Byte ((Data'Length / 256 ** (I - 1)) mod 256); end loop;
      Data (9) := (if Width = Bits_32 then 1 else 2);
      Data (11 .. 16) := Enc ("CUBIT "); Data (17 .. 24) := Enc ("OPREGION");
      Data (37 .. Data'Last) := Code;
      for B of Data loop Sum := Sum + B; end loop;
      Data (10) := 0 - Sum;
      return Data;
   end Table;

   procedure Exercise (Width : Integer_Width) is
      Raw_Address : constant Integer_Value := 16#1234_5678_8765_4321#;
      Raw_Length : constant Integer_Value := 16#0000_0001_0000_0080#;
      Code : constant Bytes := [16#5B#,16#80#] & Enc ("REG0") & [16#80#]
        & [16#0E#,16#21#,16#43#,16#65#,16#87#,16#78#,16#56#,16#34#,16#12#]
        & [16#0E#,16#80#,0,0,0,1,0,0,0]
        & Block ([16#5B#,16#81#], Enc ("REG0") & [16#FF#,0,3,
            16#03#,4,16#0B#,8] & Enc ("FLD0") & [13,
            16#01#,1,16#22#] & Enc ("FLD1") & [8])
        & Alias_Of ("FLD0", "ALS0");
      Data : constant Bytes := Table (Code, Width);
      Input : aliased AML_Table_Backing.State (1, Data'Length);
      Report : C.Module_Report;
      Init : N.Initialization_Report;
      Field_Node, Region_Node, Alias_Node, Second : N.Node_ID;
      Description, Alias_Description : C.Field_Description;
      Key, Alias_Key, Saved : C.Field_Key;
      procedure Load (Target : in out C.Arena) is
      begin
         C.Reset (Target, Status); Check (Status = C.Available, "reset");
         C.Execute_Modules (Target, Input, 1000, Report, Status);
         Check (Status = C.Available and then Report.Outcome = C.Modules_Completed,
           "module metadata");
         C.Seal (Target, Init, Status); Check (Status = C.Available, "seal");
      end Load;
      procedure Rejected (Node : N.Node_ID; Expected : C.Access_Status) is
      begin
         Description := Alias_Description; Key := Saved;
         C.Read_Field_Descriptor (A, Node, Description, Key, Status);
         Check (Status = Expected, "query rejection status");
         Check (Description = C.No_Field_Description and then Key = C.No_Field_Key,
           "query rejection clears outputs");
      end Rejected;
   begin
      Input.Count := 1; Input.Tables (1) := (0, Data'Length); Input.Data.Bytes := Data;
      Load (A); Load (B);
      C.Find_Child (A, N.Root, "FLD0", Field_Node, Status);
      Check (Status = C.Available and then Field_Node /= N.Root, "field node");
      C.Find_Child (A, N.Root, "REG0", Region_Node, Status);
      Check (Status = C.Available and then Region_Node /= N.Root, "region node");
      C.Find_Child (A, N.Root, "ALS0", Alias_Node, Status);
      Check (Status = C.Available and then Alias_Node /= N.Root, "alias node");
      C.Find_Child (A, N.Root, "FLD1", Second, Status);
      Check (Status = C.Available and then Second /= N.Root, "second field");
      declare
         Before : constant C.Audit_Value := C.Audit (A);
         Usage : constant C.Usage_Description := C.Observe_Usage (A);
         Pins : constant Natural := Natural (C.Retained_Count (A));
      begin
         C.Read_Field_Descriptor (A, Field_Node, Description, Key, Status);
         Check (Status = C.Available, "descriptor available");
         Check (C.Field_Key_Matches (A, Key), "authenticated key");
         Check (not C.Field_Key_Matches (B, Key), "foreign arena key");
         Check (not C.Field_Key_Matches (A, C.No_Field_Key), "empty key");
         Check (Description.Region.Space = 16#80#, "raw space");
         Check (Description.Region.Address = (if Width = Bits_32 then
           Raw_Address and 16#FFFF_FFFF# else Raw_Address), "raw address");
         Check (Description.Region.Length = (if Width = Bits_32 then
           Raw_Length and 16#FFFF_FFFF# else Raw_Length), "raw length");
         Check (Description.Region.Width = Width, "table width");
         Check (Description.Field.Region = Region_Node, "region edge copy");
         Check (Description.Field.Offset = 3, "reserved field offset");
         Check (Description.Field.Bits = 13, "field bit length");
         Check (Description.Field.Raw_Flags = 16#FF#, "raw field flags");
         Check (Description.Field.Access_Type = 4 and then
           Description.Field.Attribute = 16#0B# and then Description.Field.Access_Length = 8,
           "extended access attributes");
         C.Read_Field_Descriptor (A, Alias_Node, Alias_Description, Alias_Key, Status);
         Check (Status = C.Available and then Alias_Description = Description
           and then Alias_Key = Key, "alias canonicalization");
         Saved := Key;
         C.Read_Field_Descriptor (A, Second, Description, Key, Status);
         Check (Status = C.Available and then Description.Field.Offset = 16
           and then Description.Field.Bits = 8, "second field geometry");
         Check (Description.Field.Access_Type = 1 and then
           Description.Field.Attribute = 16#22# and then Description.Field.Access_Length = 0,
           "access field resets length");
         Rejected (Region_Node, C.Wrong_Kind);
         Rejected (N.Root, C.Wrong_Kind);
         Rejected (N.Node_ID'Last, C.Invalid_Value);
         Check (C.Audit (A) = Before, "namespace unchanged");
         Check (C.Observe_Usage (A) = Usage, "usage unchanged");
         Check (Natural (C.Retained_Count (A)) = Pins, "retention unchanged");
      end;
      C.Reset (A, Status); Check (Status = C.Available, "invalidate identities");
      Check (not C.Field_Key_Matches (A, Saved), "reset stale key");
      Rejected (Field_Node, C.Wrong_Phase);
      Load (A);
      Check (not C.Field_Key_Matches (A, Saved), "reloaded stale key");
      C.Read_Field_Descriptor (A, Field_Node, Description, Key, Status);
      Check (Status = C.Available and then Key /= Saved, "fresh same-position key");
   end Exercise;
begin
   C.Initialize (A, Status); Check (Status = C.Available, "initialize A");
   C.Initialize (B, Status); Check (Status = C.Available, "initialize B");
   for Width in Integer_Width loop Exercise (Width); end loop;
   Ada.Text_IO.Put_Line ("Field descriptor checks" & Checks'Image);
end Field_Descriptor_Tests;
