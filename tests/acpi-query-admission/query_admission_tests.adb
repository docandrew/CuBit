with Ada.Text_IO;
with ACPI_Query_Admission;
with ACPI_Protocol;
procedure Query_Admission_Tests is
   package W renames ACPI_Query_Admission.Wire;
   package P renames ACPI_Query_Admission.Pages;
   use type W.Word;
   Checks : Natural := 0;
   procedure Check (Condition : Boolean; Message : String) is
   begin
      Checks := Checks + 1;
      if not Condition then raise Program_Error with Message; end if;
   end Check;
   procedure Admission_Checks is
      package A renames ACPI_Query_Admission;
      use type A.Outcome, A.PID.Process_ID;
      High_Peer : constant W.Word := 16#FEDC_BA98_7654_3210#;
      Ref : constant A.References.Reference := (A.References.Global_Slot'Last, A.References.Generation'Last);
      Ref_Word : constant W.Word := A.References.Encode (Ref);
      procedure Example
        (Expected : A.Outcome; Peer : W.Word := High_Peer;
         Stamp : ACPI_Protocol.Role_Tag := ACPI_Protocol.Observer_Tag;
         Decoded : Boolean := True; Reference_Word : W.Word := Ref_Word;
         Length : W.Word := 4096; Request_Capacity, Response_Capacity : W.Word := 4096) is
         Sender : constant A.PID.Process_ID := A.PID.From_Word (Peer);
         Result : constant A.Decision := A.Admit
           (Sender, Stamp, Decoded, Reference_Word, Length, Request_Capacity, Response_Capacity);
      begin
         Check (Result.Status = Expected, "native admission expected typed outcome");
         if Result.Status = A.Admitted then
            Check (Result.Value.Sender = Sender and then A.PID.To_Word (Result.Value.Sender) = Peer,
              "native admission preserves opaque full-width peer");
            Check (A.References.Encode (Result.Value.Loan) = Reference_Word
              and then Result.Value.Length = Length, "native admission retains canonical reference and exact extent");
         end if;
      end Example;
   begin
      Example (A.Admitted);
      Example (A.Admitted, Peer => 1, Reference_Word => A.References.Encode ((0, 1)));
      Example (A.Admitted, Length => P.Frame_Byte_Count,
        Request_Capacity => P.Frame_Byte_Count, Response_Capacity => P.Frame_Byte_Count);
      Example (A.Admitted, Length => W.Word (Natural'Last),
        Request_Capacity => W.Word'Last, Response_Capacity => W.Word'Last);
      Example (A.Denied, Stamp => 0);
      Example (A.Denied, Stamp => 0, Peer => 0, Decoded => False, Reference_Word => 0, Length => W.Word'Last);
      Example (A.Malformed, Decoded => False);
      Example (A.Malformed, Decoded => False, Peer => 0, Reference_Word => 0);
      Example (A.Invalid_Sender, Peer => 0);
      Example (A.Invalid_Sender, Peer => 16#0100_0000#);
      Example (A.Invalid_Reference, Reference_Word => 0);
      Example (A.Invalid_Reference, Reference_Word => W.Word'Last);
      Example (A.Invalid_Reference, Reference_Word => A.References.Wire_Field_Base - 1);
      Example (A.Invalid_Reference, Reference_Word => A.References.Wire_Field_Base + A.References.Maximum_Slot + 1);
      Example (A.Invalid_Extent, Length => 0);
      Example (A.Invalid_Extent, Length => P.Frame_Byte_Count - 1);
      Example (A.Invalid_Extent, Length => 4097);
      Example (A.Invalid_Extent, Length => 4096, Request_Capacity => 4095);
      Example (A.Invalid_Extent, Length => 4096, Response_Capacity => 4095);
      Example (A.Invalid_Extent, Length => P.Frame_Byte_Count, Request_Capacity => 0);
      Example (A.Invalid_Extent, Length => P.Frame_Byte_Count, Response_Capacity => 0);
      Example (A.Invalid_Extent, Length => W.Word (Natural'Last) + 1,
        Request_Capacity => W.Word'Last, Response_Capacity => W.Word'Last);
      Example (A.Invalid_Extent, Length => W.Word'Last,
        Request_Capacity => W.Word'Last, Response_Capacity => W.Word'Last);
   end Admission_Checks;
begin
   Admission_Checks;
   Ada.Text_IO.Put_Line ("Admission checks" & Checks'Image);
end Query_Admission_Tests;
