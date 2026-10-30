pragma Ada_2022;
with Interfaces;
with CuBit.Handles;
with CuBit.Process_IDs;
with CuBit.Grant_References;
with I2C_Native_Controller;
with I2C_Mapping_Policy;
with I2C_Configure_Replies;
with DW_I2C_Requests;
with DW_I2C_Request_Types;
generic
   with package Native is new I2C_Native_Controller (<>);
   with function Authorized
     (Caller : CuBit.Process_IDs.Process_ID; Badge : Interfaces.Unsigned_64) return Boolean;
   with procedure Copy_Input
     (Caller : CuBit.Process_IDs.Process_ID; Grant : CuBit.Grant_References.Reference;
      Data : out Native.Controller.Bytes; Result : out DW_I2C_Request_Types.Copy_Result);
   with procedure Copy_Output
     (Caller : CuBit.Process_IDs.Process_ID; Grant : CuBit.Grant_References.Reference;
      Data : Native.Controller.Bytes; Result : out DW_I2C_Request_Types.Copy_Result);
   with procedure Answer_Client
     (Reply : CuBit.Handles.Reply; Item : DW_I2C_Request_Types.Response; Delivered : out Boolean);
   with procedure Report_Startup
     (Item : I2C_Configure_Replies.Reply; Delivered : out Boolean);
package I2C_Native_Loop is
   package Controller renames Native.Controller;
   package Q renames DW_I2C_Request_Types;
   package W renames I2C_Configure_Replies;
   package Requests is new DW_I2C_Requests
     (Controller, Native.Capacity, CuBit.Process_IDs.Process_ID,
      CuBit.Grant_References.Reference, CuBit.Handles.Reply,
      Authorized, Copy_Input, Copy_Output, Answer_Client);
   type Phase is (Cold, Starting, Serving, Draining, Stopped);
   type Stop_Reason is (None, Startup_Refused, Startup_Failed,
      Report_Delivery_Failed, Client_Fault, Requested_Stop);
   type Start_Result is (Start_Accepted, Already_Started, Start_Reentry);
   type Submit_Result is (Reply_Taken, Submit_Reentry);
   type Stop_Result is (Stop_Started, Stop_Already_Requested, Stop_Not_Ready, Stop_Reentry);
   type Report is record
      At_Phase : Phase := Cold;
      Stopped_Because : Stop_Reason := None;
      Initial_Admission : Controller.Admission := Controller.Busy;
      Mapping : Native.Registers.Report;
      Startup_Reported, Startup_Delivered : Boolean := False;
      Stop_Requested : Boolean := False;
      Startup_Outcome : W.Reply;
      Last_Early_Reply : Q.Response;
      Last_Early_Delivered : Boolean := False;
   end record;
   type Snapshot is record
      Loop_Status : Report;
      Hardware : Controller.Report;
      Client_Status : Requests.Report;
   end record;
   type State is limited private;
   function Observe (S : State) return Snapshot;
   function Ready (S : State) return Boolean;
   function Needs_Step (S : State) return Boolean;
   procedure Begin_Startup
     (S : in out State; Registers : CuBit.Handles.Device_Memory;
      Facts : Controller.Startup; Virtual : I2C_Mapping_Policy.Address;
      Result : out Start_Result);
   procedure Submit
     (S : in out State; Caller : CuBit.Process_IDs.Process_ID;
      Badge : Interfaces.Unsigned_64; Reply : CuBit.Handles.Reply;
      Item : Requests.Request; Result : out Submit_Result);
   procedure Step (S : in out State);
   procedure Request_Stop (S : in out State; Result : out Stop_Result);
   function Stopped_Quiescent (S : State) return Boolean;
   -- Stop closes admission immediately, then drains the already-owned request
   -- with its existing finite work bound/deadline. No reset/transfer is replayed.
   -- Stopped alone is not evidence of quiescence: inspect Stopped_Quiescent.
   -- Faults preserve uncertainty and never become successful requested stops.
   -- Main supplies immutable Generic_PCI launch facts and generated authority.
   -- No configure IPC, startup deadline or saved configure reply exists here.
   -- Work-bounded startup reports once through the generic driver protocol.
   -- Report_Startup logs typed detail and reports Ready/Not_Present to devmgr;
   -- that handler must reply without calling this driver back. Main validates
   -- acknowledgment rather than assuming delivery. Reentry does no IO.
   -- Client admission opens only after accepted Ready. Reply_Taken owns a saved
   -- client reply even on immediate refusal; Submit_Reentry leaves it with caller.
   -- Grant/answer callbacks are bounded/non-raising, exact-owner and consuming
   -- of saved reply authority on success or failure. No implicit IO/reply retry.
private
   type State is limited record
      Info : Report;
      Device : Controller.State;
      Client : Requests.State;
      Busy : Boolean := False;
   end record;
end I2C_Native_Loop;
