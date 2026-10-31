pragma Ada_2022;
with I2C_Transfer_Wire;
with HID_I2C_Grant_Creation;
with HID_I2C_Descriptors;
with HID_I2C_Transactions;
with CuBit.Grant_References;
with CuBit.Process_IDs;
generic
   Maximum_Transfer : I2C_Transfer_Wire.Capacity;
   with procedure Allocate (Bytes : I2C_Transfer_Wire.Word; Address : out I2C_Transfer_Wire.Word; OK : out Boolean);
   with procedure Release (Address, Bytes : I2C_Transfer_Wire.Word; OK : out Boolean);
   with procedure Clear (Address, Bytes : I2C_Transfer_Wire.Word);
   with procedure Store (Address : I2C_Transfer_Wire.Word; Data : HID_I2C_Descriptors.Bytes);
   with procedure Load (Address : I2C_Transfer_Wire.Word; Data : out HID_I2C_Descriptors.Bytes);
   with procedure Create (Address, Pages : I2C_Transfer_Wire.Word; Writable : Boolean;
                         Reference : out CuBit.Grant_References.Reference; Status : out HID_I2C_Grant_Creation.Status);
   with procedure Submit (Item : I2C_Transfer_Wire.Request; Token : I2C_Transfer_Wire.Word; Accepted : out Boolean);
   with procedure Revoke (Item : CuBit.Grant_References.Reference; Accepted : out Boolean);
   with function Retired (Item : CuBit.Grant_References.Reference) return Boolean;
package HID_I2C_Buffered_Client with SPARK_Mode is
   use type I2C_Transfer_Wire.Word;
   package W renames I2C_Transfer_Wire;
   package D renames HID_I2C_Descriptors;
   package P renames CuBit.Process_IDs;
   package G renames CuBit.Grant_References;
   package Transactions is new HID_I2C_Transactions (Submit, Revoke, Retired);
   use type Transactions.Phase, W.Count;
   Transfer_Capacity : constant W.Capacity := Maximum_Transfer;
   Page_Bytes : constant W.Word := G.Page_Bytes;
   Page_Count : constant W.Word := (W.Word (Maximum_Transfer) + Page_Bytes - 1) / Page_Bytes;
   Allocation_Bytes : constant W.Word := Page_Count * Page_Bytes;
   type Setup_Result is (Buffers_Ready, Already_Initialized, Client_Quarantined, Not_Idle, Invalid_Sender,
      Input_Allocation_Failed, Output_Allocation_Failed, Invalid_Allocation);
   type Start_Result is (Submitted, Request_Rejected, Client_Not_Ready, Client_Busy,
      Input_Grant_Refused, Output_Grant_Refused, Input_Grant_Uncertain, Output_Grant_Uncertain, Transaction_Refused, Submit_Draining);
   type Close_Result is (Closed, Still_Owned, Release_Refused);
   type State is limited private;
   function Ready (S : State) return Boolean;
   function Observe (S : State) return Transactions.Report;
   function Succeeded (S : State) return Boolean;
   function Data_Ready (S : State) return Boolean;
   function Rejection (S : State) return Transactions.Reason;
   procedure Initialize (S : in out State; Sender : P.Process_ID; Result : out Setup_Result);
   -- Allocate two disjoint private, whole-page spans, once. Allocator returns
   -- fresh owned zero-filled pages (or zero/False). Never derive addresses from
   -- references; Create is bound to the same admitted endpoint as Submit.
   -- Callbacks only touch the explicitly allocated span and never reenter.
   procedure Start (S : in out State; Write_Data : D.Bytes; Read_Count : W.Count;
      Token, Now : W.Word; Deadline : W.Millisecond; Clock_Available : Boolean;
      Result : out Start_Result);
   -- Reuse allocations only after old grants retire and caller acknowledges.
   -- Fresh per-request input grant is read-only; output grant is writable.
   procedure Receive (S : in out State; Token : W.Word; Sender : P.Process_ID;
      Delivery : Transactions.Delivery_Status; Label : W.Operation; Protocol : W.Word;
      Words : W.Message_Words; Now : W.Word; Clock_Available : Boolean;
      Result : out Transactions.Completion_Result);
   procedure Step (S : in out State; Now : W.Word; Clock_Available : Boolean);
   procedure Read (S : State; Data : out D.Bytes; OK : out Boolean);
   -- Exact successful read count only, after authoritative grant retirement.
   -- Failure returns an all-zero destination; this method never acknowledges.
   procedure Acknowledge (S : in out State) with Pre => Observe (S).At_Phase = Transactions.Finished;
   procedure Close (S : in out State; Result : out Close_Result);
   -- Close refuses in-flight/retiring work, frees only retired/unloaned own
   -- allocations and retains any allocation whose release is refused.
private
   type State is limited record
      Owner : Transactions.State;
      Input_Address, Output_Address : W.Word := 0;
      Sender : P.Process_ID := P.No_Process;
      Configured, Poisoned : Boolean := False;
      Expected_Reads : W.Count := 0;
      Last_Rejection : Transactions.Reason := Transactions.None;
   end record;
   function Ready (S : State) return Boolean is (S.Configured and then not S.Poisoned);
   function Observe (S : State) return Transactions.Report is (Transactions.Observe (S.Owner));
   function Succeeded (S : State) return Boolean is (Ready (S) and then Transactions.Succeeded (S.Owner));
   function Data_Ready (S : State) return Boolean is (Succeeded (S) and then S.Expected_Reads > 0);
   function Rejection (S : State) return Transactions.Reason is (S.Last_Rejection);
end HID_I2C_Buffered_Client;
