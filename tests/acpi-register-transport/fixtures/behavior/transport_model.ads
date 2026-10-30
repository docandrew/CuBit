pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Messages;
with CuBit.Handles;
with ACPI_Region_Protocol;
with ACPI_Region_Policy;
package Transport_Model is
   type Mode is
     (Normal, Timeout_After_Effect, Kernel_Error_After_Effect, Wrong_Label,
      Wrong_Length, Protocol_Flag, Accepts_Flag, Transferred_Flag, Refused_Flag,
      Offered_Flag, Reserved_Word, Oversize_Low, Oversize_High, Extra_Tail,
      Nonzero_Write_Value, Unknown_Hardware);
   Behavior : Mode := Normal;
   Now, Next_Time : Unsigned_64 := 10;
   Advance_Clock : Boolean := False;
   Clock_Reads, Calls : Natural := 0;
   Last_Deadline : Unsigned_64 := 0;
   function Cell return Unsigned_64;
   function Reads return Natural;
   function Writes return Natural;
   Last_Message : CuBit.Messages.Message;
   Register_ID : constant ACPI_Region_Protocol.Register_ID := 16#FEDC_1111_8765_4321#;
   Region : ACPI_Region_Policy.State;
   Ticket : Unsigned_64 := 0;
   procedure Reset;
   function Call
     (Target : CuBit.Handles.Endpoint; Msg : in out CuBit.Messages.Message;
      Deadline : Unsigned_64) return CuBit.Messages.MessageTag;
   procedure Finish (Accepted : out Boolean);
end Transport_Model;
