pragma Ada_2022;
with ACPI_Observer_Loans;
with ACPI_Query_Sessions;
with ACPI_Query_Pump;
with ACPI_Protocol;
with ACPI_Passive_Query.Pages;
package Query_Pump_Proof_Model with SPARK_Mode is
   package W renames ACPI_Passive_Query;
   package P renames W.Pages;
   subtype Buffer is P.Byte_Array (0 .. 1023);
   type Request_Kind is (None_Ready, Query_Ready, Other_Handled);
   -- Public unrestricted fields: both success/failure, arbitrary request/reply
   -- byte prefixes (zero extended), arbitrary lengths, clocks and request kinds
   -- remain feasible. Copy success has no 1024-byte extent restriction.
   type Context is record
      Incoming, Outgoing : Buffer := [others => 0];
      Acquire_OK, Copy_In_OK, Evaluate_OK, Copy_Out_OK, Return_OK : Boolean := False;
      Save_OK, Answer_OK, Clock_OK, Local_OK, Firmware_Work, Wait_OK : Boolean := False;
      Written, Required, Now, Deadline, Length, Peer, Loan, Saved_Handle : W.Word := 0;
      Kind : Request_Kind := None_Ready;
   end record;
   procedure Acquire (Owner : in out Context; Loan, Expected_Owner, Byte_Length : W.Word; Mapping : out W.Word; Accepted : out Boolean)
     with Global => null;
   procedure Copy_In (Owner : in out Context; Mapping : W.Word; Data : out P.Byte_Array; Accepted : out Boolean) with Global => null;
   procedure Evaluate (Owner : in out Context; Request_Data : P.Byte_Array; Response_Data : out P.Byte_Array;
                       Written, Required_Bytes : out W.Word; Accepted : out Boolean) with Global => null;
   procedure Copy_Out (Owner : in out Context; Mapping : W.Word; Data : P.Byte_Array; Accepted : out Boolean) with Global => null;
   procedure Release (Owner : in out Context; Loan, Mapping : W.Word; Accepted : out Boolean) with Global => null;
   package Loans is new ACPI_Observer_Loans (Context, W.Word, W.Word, 0, W.Word, 0, Acquire, Copy_In, Evaluate, Copy_Out, Release);
   procedure Save (Owner : in out Context; Handle : out W.Word; Accepted : out Boolean) with Global => null;
   procedure Answer (Owner : in out Context; Handle : W.Word; Operation : Loans.Report; Delivered : out Boolean) with Global => null;
   package Sessions is new ACPI_Query_Sessions (Loans, W.Word, 0, Save, Answer);
   procedure Prepare (Owner : in out Context; Now, Deadline : out W.Word; Clock_Usable, Ready : out Boolean) with Global => null;
   procedure Firmware (Owner : in out Context; Handled : out Boolean) with Global => null;
   procedure Poll (Owner : in out Context; Kind : out Request_Kind; Sender, Loan : out W.Word;
                   Stamp : out ACPI_Protocol.Role_Tag; Length : out W.Word) with Global => null;
   procedure Immediate (Owner : in out Context; Sender : W.Word; Result : Sessions.Report; Delivered : out Boolean) with Global => null;
   procedure Wait_Activity (Owner : in out Context; Deadline : W.Word; Usable : out Boolean) with Global => null;
   package Pump is new ACPI_Query_Pump (Sessions, W.Word'Last, Prepare, Firmware,
     Request_Kind, None_Ready, Query_Ready, Poll, Immediate, Wait_Activity);
   procedure Run (Item : in out Pump.State; Owner : in out Context; Delay_Ms : Pump.Retry_Interval;
                  Request_Data, Response_Data : out P.Byte_Array; Result : out Pump.Report)
     with Global => null, Pre => Pump.Valid (Item), Post => Pump.Valid (Item);
end Query_Pump_Proof_Model;
