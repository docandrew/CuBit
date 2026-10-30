pragma Ada_2022;
with CuBit.Messages;
with ACPI_Query_Admission;
generic
   -- Bind the generated typed request decoder here. Selected identifies the
   -- passive-query variant; Decoded additionally confirms canonical framing,
   -- flags/reserved fields and required words. This bounded callback performs
   -- no acquisition, reply, IPC or reentry, and initializes every output on
   -- every path. No layout/selector is allocated here.
   with procedure Decode
     (Packet : CuBit.Messages.Message; Selected, Decoded : out Boolean;
      Reference_Word, Declared_Length : out ACPI_Query_Admission.Wire.Word);
package ACPI_Native_Query_Input with SPARK_Mode => Off is
   package Admission renames ACPI_Query_Admission;
   package Wire renames Admission.Wire;
   type Classification is record
      Selected : Boolean := False;
      Result : Admission.Decision;
   end record;
   -- In-process adapter only: Packet/Sender must be a kernel receive result.
   -- A caller-supplied word never substitutes for authorityTag or Sender.
   function Classify
     (Sender : CuBit.Messages.Process_ID; Packet : CuBit.Messages.Message;
      Request_Capacity, Response_Capacity : Wire.Word) return Classification;
   -- Receive directly from the service-request queue; events remain separate.
   -- Other requests retain their original packet for their owning dispatcher.
   -- Denied/malformed classifications need an immediate typed refusal, never
   -- loan/session admission. This procedure neither maps nor saves a reply.
   procedure Poll
     (Request_Capacity, Response_Capacity : Wire.Word;
      Sender : out CuBit.Messages.Process_ID; Packet : out CuBit.Messages.Message;
      Found : out Boolean; Result : out Classification);
end ACPI_Native_Query_Input;
