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
   use type IPC.Packet;
   use type Word, Interfaces.Unsigned_8, Interfaces.Unsigned_16, Interfaces.Unsigned_32;
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
   -- Declarative validity policy, including nonzero identities, finite
   -- deadlines and paired lock credentials. No hardware authority is implied.
   function Request_Well_Formed (Item : Request) return Boolean is
     (Item.ID /= No_ID and then
       (case Item.Kind is
          when Publish => L.Valid (Item.Region)
            and then Word (Item.Publication_Deadline) /= 0
            and then Word (Item.Publication_Deadline) /= Word (IPC.No_Finite_Deadline),
          when Recover_Publication => Word (Item.Recovery_Deadline) /= 0
            and then Word (Item.Recovery_Deadline) /= Word (IPC.No_Finite_Deadline),
          when Begin_Scope => Word (Item.Scope_Deadline) /= 0
            and then Word (Item.Scope_Deadline) /= Word (IPC.No_Finite_Deadline)
            and then Word (Item.Scope_Deadline) <= Maximum_Scope_Deadline
            and then ((Item.Lock_Domain = 0 and then Item.Lock_Sequence = 0)
              or else (Item.Lock_Domain /= 0 and then Item.Lock_Sequence /= 0)),
          when Select_Datum => Item.Record_ID /= No_ID
            and then Item.Client_Descriptor_ID /= No_ID and then Item.Offset <= Maximum_Offset,
          when Read_Datum | Query_Operation => Item.Operation_ID /= No_ID,
          when Write_Datum => Item.Write_Operation_ID /= No_ID,
          when others => True)) with Ghost, Global => null;
   function Valid (Item : Request) return Boolean
     with Post => Valid'Result = Request_Well_Formed (Item);
   type Decoded (Valid : Boolean := False) is record
      case Valid is
         when True => Item : Request;
         when False => null;
      end case;
   end record;
   -- Exact request wire format: canonical header and no ignored payload
   -- words. Kept separate from the executable encoder for proof clients.
   function Is_Encoding (Item : Request; Packet : IPC.Packet) return Boolean is
     (Packet.Label = Label_For (Item.Kind) and then Packet.Length = 4
      and then Packet.Flags = 0 and then Packet.Reserved = 0
      and then Packet.Data (0) = Word (Item.ID) and then
       (case Item.Kind is
          when Publish => Packet.Data (1) = Item.Region.Address
            and then Packet.Data (2) = Item.Region.Length
            and then Packet.Data (3) = Word (Item.Publication_Deadline),
          when Recover_Publication => Packet.Data (1) = Word (Item.Recovery_Deadline)
            and then Packet.Data (2) = 0 and then Packet.Data (3) = 0,
          when Begin_Scope => Packet.Data (1) = Word (Item.Scope_Deadline)
            and then Packet.Data (2) = Item.Lock_Domain and then Packet.Data (3) = Item.Lock_Sequence,
          when Select_Datum => Packet.Data (1) = Word (Item.Record_ID)
            and then Packet.Data (2) = (Interfaces.Shift_Left (Item.Offset, Width_Bits)
              or Word (T.Access_Width'Pos (Item.Width)))
            and then Packet.Data (3) = Word (Item.Client_Descriptor_ID),
          when Read_Datum | Query_Operation => Packet.Data (1) = Word (Item.Operation_ID)
            and then Packet.Data (2) = 0 and then Packet.Data (3) = 0,
          when Write_Datum => Packet.Data (1) = Word (Item.Write_Operation_ID)
            and then Packet.Data (2) = Item.Value and then Packet.Data (3) = 0,
          when others => Packet.Data (1) = 0 and then Packet.Data (2) = 0 and then Packet.Data (3) = 0))
     with Ghost, Global => null;
   -- Functional encoding model. The executable implementation must refine
   -- this value; callers can use it without inlining executable bodies.
   function Encoding_Model (Item : Request) return IPC.Packet is
     ((Label => Label_For (Item.Kind), Length => 4, Flags => 0, Reserved => 0,
       Data => (case Item.Kind is
          when Publish => [Word (Item.ID), Item.Region.Address, Item.Region.Length, Word (Item.Publication_Deadline)],
          when Recover_Publication => [Word (Item.ID), Word (Item.Recovery_Deadline), 0, 0],
          when Begin_Scope => [Word (Item.ID), Word (Item.Scope_Deadline), Item.Lock_Domain, Item.Lock_Sequence],
          when Select_Datum => [Word (Item.ID), Word (Item.Record_ID),
            Interfaces.Shift_Left (Item.Offset, Width_Bits) or Word (T.Access_Width'Pos (Item.Width)),
            Word (Item.Client_Descriptor_ID)],
          when Read_Datum | Query_Operation => [Word (Item.ID), Word (Item.Operation_ID), 0, 0],
          when Write_Datum => [Word (Item.ID), Word (Item.Write_Operation_ID), Item.Value, 0],
          when others => [Word (Item.ID), 0, 0, 0])))
     with Ghost, Global => null;
   function Encode (Item : Request) return IPC.Packet
     with Pre => Valid (Item), Post => Encode'Result = Encoding_Model (Item)
       and then Is_Encoding (Item, Encode'Result);
   -- Has_Capability is independently obtained from receive metadata. Neither
   -- a capability nor its disposal obligation may be hidden by zero flags.
   function Payload_Model (Item : IPC.Packet; Kind : Operation) return Request is
     (case Kind is
        when Publish => (Kind => Publish, ID => Identifier (Item.Data (0)),
          Region => (Item.Data (1), Item.Data (2)), Publication_Deadline => IPC.Time_Instant (Item.Data (3))),
        when Recover_Publication => (Kind => Recover_Publication, ID => Identifier (Item.Data (0)),
          Recovery_Deadline => IPC.Time_Instant (Item.Data (1))),
        when Begin_Scope => (Kind => Begin_Scope, ID => Identifier (Item.Data (0)),
          Scope_Deadline => IPC.Time_Instant (Item.Data (1)), Lock_Domain => Item.Data (2), Lock_Sequence => Item.Data (3)),
        when Select_Datum => (Kind => Select_Datum, ID => Identifier (Item.Data (0)),
          Record_ID => Identifier (Item.Data (1)), Client_Descriptor_ID => Identifier (Item.Data (3)),
          Offset => Interfaces.Shift_Right (Item.Data (2), Width_Bits),
          Width => T.Access_Width'Val (Item.Data (2) and Width_Mask)),
        when Read_Datum => (Kind => Read_Datum, ID => Identifier (Item.Data (0)),
          Operation_ID => Identifier (Item.Data (1))),
        when Query_Operation => (Kind => Query_Operation, ID => Identifier (Item.Data (0)),
          Operation_ID => Identifier (Item.Data (1))),
        when Write_Datum => (Kind => Write_Datum, ID => Identifier (Item.Data (0)),
          Write_Operation_ID => Identifier (Item.Data (1)), Value => Item.Data (2)),
        when Describe_Publication => (Kind => Describe_Publication, ID => Identifier (Item.Data (0))),
        when Close_Scope => (Kind => Close_Scope, ID => Identifier (Item.Data (0))),
        when Describe_Scope => (Kind => Describe_Scope, ID => Identifier (Item.Data (0))))
     with Ghost, Global => null;
   function Unused_Words_Zero (Item : IPC.Packet; Kind : Operation) return Boolean is
     (case Kind is
        when Publish | Begin_Scope | Select_Datum => True,
        when Recover_Publication | Read_Datum | Query_Operation => Item.Data (2) = 0 and then Item.Data (3) = 0,
        when Write_Datum => Item.Data (3) = 0,
        when others => Item.Data (1) = 0 and then Item.Data (2) = 0 and then Item.Data (3) = 0)
     with Ghost, Global => null;
   function Header_Valid (Item : IPC.Packet; Has_Capability : Boolean) return Boolean is
     (not Has_Capability and then Item.Length = 4 and then Item.Flags = 0 and then Item.Reserved = 0
      and then Item.Label >= First_Label and then Item.Label <= Label_For (Operation'Last)
      and then Item.Data (0) /= 0) with Ghost, Global => null;
   function Decoding_Model (Item : IPC.Packet; Has_Capability : Boolean) return Decoded is
     (if not Header_Valid (Item, Has_Capability) then (Valid => False)
      elsif not Unused_Words_Zero (Item, Operation'Val (Item.Label - First_Label))
        or else not Request_Well_Formed (Payload_Model (Item, Operation'Val (Item.Label - First_Label)))
      then (Valid => False)
      else (Valid => True, Item => Payload_Model (Item, Operation'Val (Item.Label - First_Label))))
     with Ghost, Global => null;
   function Decode (Item : IPC.Packet; Has_Capability : Boolean) return Decoded
     with Post => Decode'Result = Decoding_Model (Item, Has_Capability)
       and then (if Decode'Result.Valid then Valid (Decode'Result.Item));
end ACPI_Firmware_Data_Frames;
