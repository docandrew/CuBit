--  An import's text limit crosses three representations: the VM's
--  (CCL.VM.Import_Text_Length, up to a whole string, 8 KiB), a host
--  contract's (CCL.Host_Values.Text_Limit, up to a host text value, 1 KiB)
--  and the CCLB module's (an unsigned number). A module is untrusted input:
--  a plain text limit above a host text value must be refused as bad
--  linkage, not converted into the host contract (where it does not fit:
--  before the two became distinct types this raised Constraint_Error under
--  checks and stored an out-of-range limit under -gnatp).
with Ada.Text_IO;
with Interfaces; use Interfaces;
with CCL.VM; use CCL.VM;
with CCL.Catalog;
with CCL.Host_Values;
with CCL.Format;
with CCL.Objects;

procedure Text_Limit_Tests is
   use type CCL.Format.Format_Error;
   use type CCL.Catalog.Intern_Result;
   use type CCL.Host_Values.Text_Limit;
   use type CCL.Host_Values.Value_Kind;
   package H renames CCL.Host_Values;
   Checks : Natural := 0;
   procedure Check (OK : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with "text limit check" & Checks'Image & ": " & What; end if;
   end Check;

   Host_Bound : constant := H.Maximum_Text_Length;
   Limits : constant CCL.Format.Resource_Limits := (Fuel => 32, Memory => 4096, In_Flight => 1);

   --  A program passing a text constant to import 0, whose argument limit is
   --  Limit, and the linkage the host declared for it.
   procedure Build (Limit : Import_Text_Length; Candidate : out Program; Links : out CCL.Catalog.Linkage_Table) is
      Index : Import_Index;
      Interned : CCL.Catalog.Intern_Result;
      Text : constant String := "hi";
   begin
      Candidate := (others => <>);
      Candidate.Length := 3;
      Candidate.Code (0) := (Op => Push_Text, Immediate => 0, others => <>);
      Candidate.Code (1) := (Op => Invoke_Import, others => <>);
      Candidate.Code (2) := (Op => Halt, others => <>);
      Candidate.Constants_Length := 1;
      Candidate.Constants (0) := (First => 1, Length => Text'Length);
      Candidate.Constant_Text (1 .. Text'Length) := Text;
      Candidate.Imports_Length := 1;
      Candidate.Imports (0) := (Argument => Text_Value, Result => Integer_Value,
                                Argument_Text_Limit => Limit, others => <>);
      CCL.Catalog.Initialize (Links);
      CCL.Catalog.Intern (Links,
        (Interface_Digest => [1, 2, 3, 4], Interface_Major => 1, Parameters => 1,
         Import => (Argument => H.Text_Value, Result => H.Integer_Value,
                    Argument_Text_Limit => H.To_Host_Limit (Import_Text_Length'Min (Limit, Host_Bound)),
                    others => <>),
         others => <>),
        Index, Interned);
      Check (Interned = CCL.Catalog.Linkage_Added, "linkage interned");
   end Build;

   Candidate, Decoded : Program;
   Links, Decoded_Links : CCL.Catalog.Linkage_Table;
   Verified : Validated_Program;
   Validation : Validation_Error;
   Bytes : CCL.Format.Byte_Array;
   Size : CCL.Format.Module_Length;
   Error : CCL.Format.Format_Error;
   Decoded_Limits : CCL.Format.Resource_Limits;
   Limit_At : Natural := 0;
   Found : Natural := 0;
begin
   --  The conversions between the two limit types.
   Check (H.Host_Limit_Fits (Host_Bound) and not H.Host_Limit_Fits (Host_Bound + 1), "host bound");
   Check (H.To_Host_Limit (Host_Bound) = Host_Bound, "to host at the bound");
   Check (H.To_Import_Limit (H.Text_Limit'Last) = Host_Bound, "to import widens");
   Check (H.Host_Limit_Fits (0), "zero fits");

   --  A plain text limit above a host text value is not a portable contract.
   Check (not H.Portable_Contract
            ((Argument => Text_Value, Result => Integer_Value,
              Argument_Text_Limit => Host_Bound + 1, others => <>),
             CCL.Objects.No_Schema, CCL.Objects.No_Schema),
          "oversized limit is not portable");
   Check (H.Portable_Contract
            ((Argument => Text_Value, Result => Integer_Value,
              Argument_Text_Limit => Host_Bound, others => <>),
             CCL.Objects.No_Schema, CCL.Objects.No_Schema),
          "limit at the bound is portable");

   --  At the bound a module round-trips.
   Build (Host_Bound, Candidate, Links);
   Verify (Candidate, Verified, Validation);
   Check (Validation = Valid, "program verifies");
   CCL.Format.Encode (Candidate, Links, Limits, Bytes, Size, Error, Validation);
   Check (Error = CCL.Format.Format_Valid and Validation = Valid, "encodes");
   CCL.Format.Decode (Bytes, Size, Decoded, Decoded_Links, Decoded_Limits, Error, Validation);
   Check (Error = CCL.Format.Format_Valid, "decodes at the bound");
   Check (Decoded.Imports (0).Argument_Text_Limit = Host_Bound, "VM limit kept");
   Check (CCL.Catalog.Element (Decoded_Links, 0).Import.Argument_Text_Limit = Host_Bound, "host limit kept");

   --  The module's limit is a CBOR uint16 (major type 0, 25): 1024 is
   --  19 04 00. Raise it past the host bound in the encoded bytes.
   for I in 0 .. Size - 3 loop
      if Bytes (I) = 16#19# and then Bytes (I + 1) = 16#04# and then
        Bytes (I + 2) = 16#00#
      then
         Found := Found + 1;
         Limit_At := I;
      end if;
   end loop;
   Check (Found = 1, "one encoded limit at the bound");
   Bytes (Limit_At + 1) := 16#08#;
   CCL.Format.Decode (Bytes, Size, Decoded, Decoded_Links, Decoded_Limits, Error, Validation);
   Check (Error = CCL.Format.Invalid_Linkage, "an oversized plain text limit is bad linkage");

   Ada.Text_IO.Put_Line ("Import text limits: PASS" & Checks'Image & " checks");
end Text_Limit_Tests;
