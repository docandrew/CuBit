pragma Ada_2022;
with CuBit.Messages;with ACPI_Grant_Byte_IO;
with ACPI_Query_Transport;with ACPI_Passive_Query.Pages;
generic
   type Context (<>) is limited private;
   with package Byte_IO is new ACPI_Grant_Byte_IO (<>);
   type Reply_Handle is private;
   No_Reply : Reply_Handle;
   with procedure Save_Reply (Handle : out Reply_Handle; Accepted : out Boolean);
   with procedure Answer
     (Handle : Reply_Handle; Value : ACPI_Query_Transport.Reply_Details; Delivered : out Boolean);
   with procedure Evaluate
     (Owner : in out Context; Request_Data : ACPI_Passive_Query.Pages.Byte_Array;
      Response_Data : out ACPI_Passive_Query.Pages.Byte_Array;
      Written, Required_Bytes : out ACPI_Query_Transport.Word; Accepted : out Boolean);
   with procedure Read_Clock (Now : out ACPI_Query_Transport.Word; Available : out Boolean);
   Buffer_Bytes : Positive := 4096;
   Retry_Milliseconds : Positive := 10;
package ACPI_Native_Query_Entry with SPARK_Mode => Off is
   -- Serialized native owner. Sender/Packet must be an actual kernel receive.
   -- Context is passed directly (normally Requests.State), never retained by
   -- pointer. Only a saved reply and an authenticated acquisition may persist.
   procedure Dispatch
     (Owner : in out Context; Sender : CuBit.Messages.Process_ID;
      Packet : CuBit.Messages.Message; Admission_Open : Boolean;
      Reply : out CuBit.Messages.Message; Handled, Reply_Now : out Boolean);
   -- One due cleanup attempt. Refused return does not count as runnable work.
   -- The caller merges Wake_Deadline with all other millisecond deadlines.
   procedure Advance (Owner : in out Context; Did_Work : out Boolean);
   function Wake_Deadline return ACPI_Query_Transport.Word;
   function Healthy return Boolean;
   function Quiet return Boolean;
   procedure Begin_Stop;
   -- One return/answer attempt, no evaluation/replay, no clock requirement.
   -- An incomplete drain must retain the owner before namespace retirement.
   procedure Drain (Owner : in out Context; Complete : out Boolean);
end ACPI_Native_Query_Entry;
