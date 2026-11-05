pragma Ada_2022;
with Interfaces;
with ACPI_Region_Protocol;
with ACPI_Firmware_Data_Layouts;
with ACPI_PCI_Protocol;
-- Frontend/backend request codec only. Authentication, retained histories,
-- live scope/lease checks and authority are obligations of the controller.
package ACPI_Firmware_Data_Frames with Pure, SPARK_Mode is
   package IPC renames ACPI_Region_Protocol;
   package L renames ACPI_Firmware_Data_Layouts;
   package T renames L.T;
   subtype Word is Interfaces.Unsigned_64;
   use type Word, Interfaces.Unsigned_32;
   type Identifier is new Word;
   No_ID : constant Identifier := 0;
   type Operation is (Publish, Describe_Publication, Recover_Publication,
      Begin_Scope, Select_Datum, Read_Datum, Write_Datum, Query_Operation,
      Close_Scope, Describe_Scope);
   -- Descriptions/history and closing a scope cannot introduce a new Field
   -- effect. Recover_Publication may map retained backing, so is not passive.
   function Allowed_While_Quiescing (Kind : Operation) return Boolean is
      (Kind in Describe_Publication | Describe_Scope | Query_Operation | Close_Scope)
      with Global => null;
   First_Label : constant Interfaces.Unsigned_32 := 16#2B4#;
   Reserved_Last_Label : constant Interfaces.Unsigned_32 := 16#2BF#;
   Reply_Delta : constant Interfaces.Unsigned_32 := 16#F000#;
   Width_Bits : constant := 2;
   Width_Mask : constant Word := 2 ** Width_Bits - 1;
   -- Offsets are limited by the admitted physical address space. Upper
   -- unused packed bits are refused rather than silently truncated.
   Maximum_Offset : constant Word := L.Physical_Limit - 1;
   Microseconds_Per_Millisecond : constant Word := 1_000;
   Maximum_Scope_Deadline : constant Word :=
      (Word'Last - 1) / Microseconds_Per_Millisecond;
   pragma Compile_Time_Error
     (First_Label <= ACPI_PCI_Protocol.Describe_Operation
      or else First_Label + Operation'Pos (Operation'Last) > Reserved_Last_Label
      or else Reserved_Last_Label + Reply_Delta > Interfaces.Unsigned_32'Last
      or else T.Access_Width'Pos (T.Access_Width'Last) /= Width_Mask
      or else Maximum_Offset > Word'Last / 2 ** Width_Bits,
      "Firmware data request labels or packed geometry conflict");
   function Label_For (Kind : Operation) return Interfaces.Unsigned_32 is
      (First_Label + Operation'Pos (Kind));
   function Reply_Label_For (Kind : Operation) return Interfaces.Unsigned_32 is
      (Label_For (Kind) + Reply_Delta);
   type Request (Kind : Operation := Publish) is record
      -- Client publication/scope/descriptor identity, or backend descriptor/
      -- scope identity, according to Kind. Zero is never a valid identity.
      ID : Identifier := No_ID;
      case Kind is
         when Publish =>
            Region : L.Region;
            Publication_Deadline : IPC.Time_Instant := 0;
         when Recover_Publication =>
            Recovery_Deadline : IPC.Time_Instant := 0;
         when Begin_Scope =>
            Scope_Deadline : IPC.Time_Instant := 0;
            Lock_Domain, Lock_Sequence : Word := 0;
         when Select_Datum =>
            Record_ID, Client_Descriptor_ID : Identifier := No_ID;
            Offset : Word := 0;
            Width : T.Access_Width := T.Byte_Access;
         when Read_Datum | Query_Operation =>
            Operation_ID : Identifier := No_ID;
         when Write_Datum =>
            Write_Operation_ID : Identifier := No_ID;
            Value : Word := 0;
         when others => null;
      end case;
   end record;
   function Valid (Item : Request) return Boolean;
   type Decoded (Valid : Boolean := False) is record
      case Valid is
         when True => Item : Request;
         when False => null;
      end case;
   end record;
   function Encode (Item : Request) return IPC.Packet with Pre => Valid (Item);
   -- Has_Capability is independently obtained from receive metadata. Neither
   -- a capability nor its disposal obligation may be hidden by zero flags.
   function Decode (Item : IPC.Packet; Has_Capability : Boolean) return Decoded;
end ACPI_Firmware_Data_Frames;
