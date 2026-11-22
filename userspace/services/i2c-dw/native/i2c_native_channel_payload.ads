pragma Ada_2022;
with I2C_Channel;
with DW_I2C_Transfer;
generic
   -- Caller derives this from the actual owner's Payload_Status. A true
   -- result means Not_Borrowed or Returnable, never In_Use/Retained.
   with function May_Release return Boolean;
package I2C_Native_Channel_Payload is
   package T renames DW_I2C_Transfer;
   package RC renames I2C_Channel.RC;
   type State is limited private;
   type Binding is (Bound, Already_Bound, Invalid_Counts, Invalid_Views);
   procedure Bind
     (S : in out State; Input, Output : RC.Payload_View;
      Write_Count, Read_Count : T.Count; Result : out Binding);
   -- Views must come from Channel_Servers, following authenticated admission.
   -- Input is the client's read-only area; Output is the server's writable
   -- area. No wire field is interpreted as a pointer or grant reference.
   function Is_Bound (S : State) return Boolean;
   procedure Read_Byte
     (S : in out State; Index : T.Count; Value : out T.Byte; Accepted : out Boolean);
   procedure Write_Byte
     (S : in out State; Index : T.Count; Value : T.Byte; Accepted : out Boolean);
   -- Sequential one-based byte providers: refusal never accesses memory.
   -- No copy, allocation, syscall, IPC, clearing or retry occurs here.
   type Releasing is (Released, Not_Bound, Not_Quiescent);
   procedure Release (S : in out State; Result : out Releasing);
   -- Release only forgets borrowed views; the event loop then closes/returns
   -- the channel's actual mappings. A refused release keeps both views intact.
   -- Caller serializes Bind/providers/Release on the controller's event loop.
private
   type State is limited record
      Active : Boolean := False;
      Input, Output : RC.Payload_View := RC.Empty_Payload;
      Writes, Reads, Input_Position, Output_Position : T.Count := 0;
   end record;
end I2C_Native_Channel_Payload;
