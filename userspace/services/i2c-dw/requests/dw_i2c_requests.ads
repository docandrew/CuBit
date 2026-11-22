pragma Ada_2022;
with Interfaces;
with DW_I2C_Transfer;
with DW_I2C_Owner;
with DW_I2C_Request_Types;
generic
   with package Controller is new DW_I2C_Owner (<>);
   Capacity : DW_I2C_Transfer.Limit;
   type Caller_ID is private;
   type Grant_Reference is private;
   type Reply_Authority is private;
   with function Authorized (Caller : Caller_ID; Badge : Interfaces.Unsigned_64) return Boolean;
   with procedure Copy_Input
     (Caller : Caller_ID; Grant : Grant_Reference; Data : out Controller.Bytes;
      Result : out DW_I2C_Request_Types.Copy_Result);
   with procedure Copy_Output
     (Caller : Caller_ID; Grant : Grant_Reference; Data : Controller.Bytes;
      Result : out DW_I2C_Request_Types.Copy_Result);
   with procedure Answer
     (Reply : Reply_Authority; Item : DW_I2C_Request_Types.Response; Delivered : out Boolean);
package DW_I2C_Requests with SPARK_Mode is
   package T renames DW_I2C_Transfer;
   package Q renames DW_I2C_Request_Types;
   type Request is record
      Input, Output : Grant_Reference;
      Write_Count, Read_Count : T.Count;
      Deadline : T.Tick;
      Step_Budget : Controller.Work_Limit;
   end record;
   type Report is record
      Active, Faulted, Last_Reply_Delivered : Boolean := False;
      Last_Reply : Q.Response;
      Admission : Controller.Admission := Controller.Busy;
      Failure : Controller.Failure := Controller.None;
   end record;
   type State is limited private;
   function Observe (S : State) return Report;
   procedure Submit
     (S : in out State; Device : in out Controller.State;
      Caller : Caller_ID; Badge : Interfaces.Unsigned_64;
      Reply : Reply_Authority; Item : Request);
   procedure Advance (S : in out State; Device : in out Controller.State);
   -- One serialized request, bounded owned buffers, one saved reply. Caller and
   -- badge MUST come from kernel-stamped request metadata, not message words.
   -- The canonical badge supplies target/mode/max Hz. Initial hardware policy
   -- supports seven-bit mode and the Intel profile speed range; no message
   -- field can override this scope. Malformed/unsupported badges fail before
   -- grants or hardware. Authorized remains a required native policy callback.
   -- Each Submit consumes Reply exactly once now or after completion. Answer
   -- must consume native saved reply authority even when delivery fails.
   -- Copy callbacks acquire generation-checked grants with this exact expected
   -- owner, copy the requested slice, and return acquisition before returning.
   -- Uncertain includes failed release; it latches Faulted, never retries.
   -- Callbacks are bounded, non-reentrant and non-raising. No wire/catalog IDs
   -- or raw addresses here; native admission and grant callbacks remain to bind.
private
   type State is limited record
      Info : Report;
      Held_Caller : Caller_ID;
      Held_Output : Grant_Reference;
      Held_Reply : Reply_Authority;
      Ticket : T.Ticket := T.No_Ticket;
      Expected_Reads : T.Count := 0;
      Buffer : Controller.Bytes (1 .. Capacity) := [others => 0];
   end record;
end DW_I2C_Requests;
