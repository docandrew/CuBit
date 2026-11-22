pragma Ada_2022;
with Interfaces;
with CuBit.Protocols;
with CuBit.Channel_Contracts;
--  A copied host-bridge snapshot conveys firmware claims, never hardware
--  authority. Open only on devmgr's existing ACPI PCI-windows endpoint badge.
package CuBit.Host_Bridge_Windows with Pure, SPARK_Mode is
   subtype Word is Interfaces.Unsigned_64;
   use type Word;
   Schema : constant CuBit.Protocols.Schema_Id := 16#4143_5049_4842_0001#;
   Schema_Version : constant CuBit.Protocols.Protocol_Version := 1;
   Word_Bytes : constant Positive := 8;
   Message_Words : constant Positive := 3;
   Message_Bytes : constant Interfaces.Unsigned_32 := Interfaces.Unsigned_32 (Message_Words * Word_Bytes);
   Element : constant CuBit.Protocols.Schema_Contract :=
     (Identity => Schema, Version => Schema_Version,
      Sizing => CuBit.Protocols.Fixed_Size, Wire_Size => Message_Bytes);
   type Request_Field is (Root_ID, Sequence, Packet_Bytes);
   type Request is array (Request_Field) of Word with Component_Size => 64;
   type Result_Field is (Root_ID, Sequence, Disposition);
   type Result is array (Result_Field) of Word with Component_Size => 64;
   type Snapshot_State is (Complete, Incomplete, Failed) with Size => 64;
   for Snapshot_State use (Complete => 1, Incomplete => 2, Failed => 3);
   type Failure_Reason is (No_Failure, Unknown_Descriptor, Capacity, Quota,
      Malformed_Resource, Malformed_Coordinates, Evaluation_Failed,
      Missing_Resources, Wrong_Result_Type) with Size => 64;
   for Failure_Reason use
     (No_Failure => 0, Unknown_Descriptor => 1, Capacity => 2, Quota => 3,
      Malformed_Resource => 4, Malformed_Coordinates => 5, Evaluation_Failed => 6,
      Missing_Resources => 7, Wrong_Result_Type => 8);
   type Acceptance is (Accepted, Malformed, Identity_Mismatch,
      Stale_Sequence, Too_Large) with Size => 64;
   for Acceptance use (Accepted => 1, Malformed => 2, Identity_Mismatch => 3,
      Stale_Sequence => 4, Too_Large => 5);
   function State_Code (Value : Snapshot_State) return Word is (Word (Snapshot_State'Enum_Rep (Value)));
   function Reason_Code (Value : Failure_Reason) return Word is (Word (Failure_Reason'Enum_Rep (Value)));
   function Acceptance_Code (Value : Acceptance) return Word is (Word (Acceptance'Enum_Rep (Value)));
   function Valid_State_Code (Value : Word) return Boolean is (Value in 1 .. 3);
   function Valid_Reason_Code (Value : Word) return Boolean is (Value in 0 .. 8);
   function Valid_Acceptance_Code (Value : Word) return Boolean is (Value in 1 .. 5);
   function State_From_Code (Value : Word) return Snapshot_State is (Snapshot_State'Val (Value - 1))
     with Pre => Valid_State_Code (Value);
   function Reason_From_Code (Value : Word) return Failure_Reason is (Failure_Reason'Val (Value))
     with Pre => Valid_Reason_Code (Value);
   function Acceptance_From_Code (Value : Word) return Acceptance is (Acceptance'Val (Value - 1))
     with Pre => Valid_Acceptance_Code (Value);
   --  Schema v1 uses Request_Channels(Request, Result, 0, 0): a control page
   --  and one tiny ring page in each region, then measured client payload.
   --  Clients check these constants against their actual instance.
   Queue_Slot_Bits : constant Natural := 0;
   Control_Pages : constant Positive := 1;
   Queue_Pages : constant Positive := 1;
   Client_Ring_Pages : constant Positive := Control_Pages + Queue_Pages;
   Server_Pages : constant Positive := Control_Pages + Queue_Pages;
   Minimum_Payload_Pages : constant Positive := 1;
   Minimum_Client_Pages : constant Positive := Client_Ring_Pages + Minimum_Payload_Pages;
   Maximum_Client_Pages : constant Positive := CuBit.Channel_Contracts.Ring_Pages'Last;
   Contract : constant CuBit.Channel_Contracts.Contract :=
     (Element => (Identity => Schema, Version => Schema_Version,
                  Sizing => CuBit.Protocols.Fixed_Size, Wire_Size => Message_Bytes),
      Kind => CuBit.Channel_Contracts.Duplex,
      Policy => CuBit.Channel_Contracts.Lossless, Pages => Minimum_Client_Pages,
      Buffers => Server_Pages, Rule => CuBit.Channel_Contracts.Copy_Then_Validate);
   function Measured_Contract (Pages : CuBit.Channel_Contracts.Ring_Pages)
      return CuBit.Channel_Contracts.Contract is (Contract with delta Pages => Pages)
      with Pre => Pages >= Minimum_Client_Pages;
   --  Payload magic remains ACPIHBW1. Roots are opaque/nonzero, scoped to
   --  their opened channel; sequence must increase for each root. ACK means
   --  copied/validated only. devmgr alone owns PCI matching and BAR policy.
end CuBit.Host_Bridge_Windows;
