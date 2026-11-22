with Owner_Test_Control;
package body CuBit.Messages is
   function Wait_For_Activity_Until (Deadline : Unsigned_64)
      return Activity_Result is
      pragma Unreferenced (Deadline);
   begin
      return raise Program_Error with "unexpected guest boundary: Wait_For_Activity_Until";
   end Wait_For_Activity_Until;
   procedure Wait_For_Activity_Or_Event
     (Deadline : Unsigned_64; Event : aliased out Message; Result : out Wake_Result) is
      pragma Unreferenced (Deadline, Event, Result);
   begin
      raise Program_Error with "unexpected guest boundary: Wait_For_Activity_Or_Event";
   end Wait_For_Activity_Or_Event;
   function syscall
     (call : Unsigned_64; arg0 : Unsigned_64 := 0; arg1 : Unsigned_64 := 0;
      arg2 : Unsigned_64 := 0; arg3 : Unsigned_64 := 0;
      arg4 : Unsigned_64 := 0; arg5 : Unsigned_64 := 0)
      return Unsigned_64 is
      pragma Unreferenced (call, arg0, arg1, arg2, arg3, arg4, arg5);
   begin
      return raise Program_Error with "unexpected guest boundary: syscall";
   end syscall;
   procedure receive (from : out Process_ID; msg : out Message) is
      pragma Unreferenced (from, msg);
   begin
      raise Program_Error with "unexpected guest boundary: receive";
   end receive;
   procedure receiveUntil
     (deadlineMs : Unsigned_64;
      from       : out Process_ID;
      msg        : out Message;
      received   : out Boolean) is
      pragma Unreferenced (deadlineMs, from, msg, received);
   begin
      raise Program_Error with "unexpected guest boundary: receiveUntil";
   end receiveUntil;
   function reply
     (replyTo : Process_ID; msg : Message) return Unsigned_64 is
      pragma Unreferenced (replyTo, msg);
   begin
      return raise Program_Error with "unexpected guest boundary: reply";
   end reply;
   function replyCap
     (saved : CuBit.Handles.Reply; msg : Message) return Unsigned_64 is
      pragma Unreferenced (saved, msg);
   begin
      return raise Program_Error with "unexpected guest boundary: replyCap";
   end replyCap;
   procedure replyWait
     (replyTo  : Process_ID;
      replyMsg : Message;
      from     : out Process_ID;
      msg      : in out Message) is
      pragma Unreferenced (replyTo, replyMsg, from, msg);
   begin
      raise Program_Error with "unexpected guest boundary: replyWait";
   end replyWait;
   procedure Poll_Service_Request
     (from  : out Process_ID;
      msg   : out Message;
      found : out Boolean) is
      pragma Unreferenced (from, msg, found);
   begin
      raise Program_Error with "unexpected guest boundary: Poll_Service_Request";
   end Poll_Service_Request;
   procedure Poll_Any_Ipc
     (from  : out Process_ID;
      msg   : out Message;
      found : out Boolean) is
      pragma Unreferenced (from, msg, found);
   begin
      raise Program_Error with "unexpected guest boundary: Poll_Any_Ipc";
   end Poll_Any_Ipc;
   function waitCompletion
     (entries : System.Address;
      max     : Unsigned_64;
      min     : Unsigned_64) return Unsigned_64 is
      pragma Unreferenced (entries, max, min);
   begin
      return raise Program_Error with "unexpected guest boundary: waitCompletion";
   end waitCompletion;
   function Poll_Completion
     (result : System.Address) return Unsigned_64 is
      pragma Unreferenced (result);
   begin
      return raise Program_Error with "unexpected guest boundary: Poll_Completion";
   end Poll_Completion;
   function Deadline_After (Milliseconds : Unsigned_64) return Unsigned_64 is
      pragma Unreferenced (Milliseconds);
   begin
      return raise Program_Error with "unexpected guest boundary: Deadline_After";
   end Deadline_After;
   function capSend
     (target : CuBit.Handles.Endpoint; msg : Message; Deadline : Unsigned_64)
      return MessageTag is
      pragma Unreferenced (target, msg, Deadline);
   begin
      return raise Program_Error with "unexpected guest boundary: capSend";
   end capSend;
   function capCall
     (target : CuBit.Handles.Endpoint; msg : in out Message; Deadline : Unsigned_64)
      return MessageTag is
      pragma Unreferenced (target, msg, Deadline);
   begin
      return raise Program_Error with "unexpected guest boundary: capCall";
   end capCall;
   function capSubmit
     (target : CuBit.Handles.Endpoint;
      msg    : Message;
      token  : Unsigned_64) return Boolean is
      pragma Unreferenced (target, msg, token);
   begin
      return raise Program_Error with "unexpected guest boundary: capSubmit";
   end capSubmit;
   procedure sendEvent
     (dest : Process_ID; msg : Message; authority : CuBit.Handles.Notification) is
      pragma Unreferenced (dest, msg, authority);
   begin
      raise Program_Error with "unexpected guest boundary: sendEvent";
   end sendEvent;
   procedure sendEvent
     (dest : Process_ID; msg : Message; authority : CuBit.Handles.Endpoint) is
      pragma Unreferenced (dest, msg, authority);
   begin
      raise Program_Error with "unexpected guest boundary: sendEvent";
   end sendEvent;
   function trySendEvent
     (dest : Process_ID; msg : Message; authority : CuBit.Handles.Notification)
      return Boolean is
      pragma Unreferenced (dest, msg, authority);
   begin
      return raise Program_Error with "unexpected guest boundary: trySendEvent";
   end trySendEvent;
   function trySendEvent
     (dest : Process_ID; msg : Message; authority : CuBit.Handles.Endpoint)
      return Boolean is
      pragma Unreferenced (dest, msg, authority);
   begin
      return raise Program_Error with "unexpected guest boundary: trySendEvent";
   end trySendEvent;
   function Poll_Event (msg : out Message) return Boolean is
      pragma Unreferenced (msg);
   begin
      return raise Program_Error with "unexpected guest boundary: Poll_Event";
   end Poll_Event;
   function Poll_Event (msg : out Message; publisher : out Process_ID) return Boolean is
      pragma Unreferenced (msg, publisher);
   begin
      return raise Program_Error with "unexpected guest boundary: Poll_Event";
   end Poll_Event;
   function killProcess
     (pid : Process_ID; authority : CuBit.Handles.Process) return Unsigned_64 is
      pragma Unreferenced (pid, authority);
   begin
      return raise Program_Error with "unexpected guest boundary: killProcess";
   end killProcess;
   procedure revokeGrant (id : Unsigned_64) is
      pragma Unreferenced (id);
   begin
      raise Program_Error with "unexpected guest boundary: revokeGrant";
   end revokeGrant;
   function portInp8 (ports : CuBit.Handles.IO_Ports; port : Unsigned_16) return Unsigned_64 is
      pragma Unreferenced (ports, port);
   begin
      return raise Program_Error with "unexpected guest boundary: portInp8";
   end portInp8;
   function portOutp8
     (ports : CuBit.Handles.IO_Ports; port : Unsigned_16; val : Unsigned_8) return Unsigned_64 is
      pragma Unreferenced (ports, port, val);
   begin
      return raise Program_Error with "unexpected guest boundary: portOutp8";
   end portOutp8;
   function portInp16 (ports : CuBit.Handles.IO_Ports; port : Unsigned_16) return Unsigned_64 is
      pragma Unreferenced (ports, port);
   begin
      return raise Program_Error with "unexpected guest boundary: portInp16";
   end portInp16;
   function portOutp16
     (ports : CuBit.Handles.IO_Ports; port : Unsigned_16; val : Unsigned_16) return Unsigned_64 is
      pragma Unreferenced (ports, port, val);
   begin
      return raise Program_Error with "unexpected guest boundary: portOutp16";
   end portOutp16;
   function portInp32 (ports : CuBit.Handles.IO_Ports; port : Unsigned_16) return Unsigned_64 is
      pragma Unreferenced (ports, port);
   begin
      return raise Program_Error with "unexpected guest boundary: portInp32";
   end portInp32;
   function portOutp32
     (ports : CuBit.Handles.IO_Ports; port : Unsigned_16; val : Unsigned_32) return Unsigned_64 is
      pragma Unreferenced (ports, port, val);
   begin
      return raise Program_Error with "unexpected guest boundary: portOutp32";
   end portOutp32;
   function mapInto
     (targetPID : Process_ID;
      physAddr  : Unsigned_64;
      virtAddr  : Unsigned_64;
      numPages  : Unsigned_64;
      flags     : Unsigned_64;
      authority : CuBit.Handles.Process;
      region    : CuBit.Handles.Device_Memory) return Unsigned_64 is
      pragma Unreferenced (targetPID, physAddr, virtAddr, numPages, flags, authority, region);
   begin
      return raise Program_Error with "unexpected guest boundary: mapInto";
   end mapInto;
   function setCpu
     (targetPID : Process_ID;
      cpu       : Unsigned_64;
      authority : CuBit.Handles.Process) return Unsigned_64 is
      pragma Unreferenced (targetPID, cpu, authority);
   begin
      return raise Program_Error with "unexpected guest boundary: setCpu";
   end setCpu;
   function setLatencyContract
     (latencyClass : Unsigned_64;
      periodUs     : Unsigned_64;
      budgetUs     : Unsigned_64;
      flags        : Unsigned_64 := 0) return Unsigned_64 is
      pragma Unreferenced (latencyClass, periodUs, budgetUs, flags);
   begin
      return raise Program_Error with "unexpected guest boundary: setLatencyContract";
   end setLatencyContract;
   function Registered_Driver (Driver : Unsigned_64) return Process_ID is
      pragma Unreferenced (Driver);
   begin
      return raise Program_Error with "unexpected guest boundary: Registered_Driver";
   end Registered_Driver;
   function Own_Process return Process_ID is
   begin return From_Word(16#FEDC_BA98_7654_3210#);
   end Own_Process;
   function registerDriver
     (driver : Unsigned_64; authority : CuBit.Handles.Notification) return Unsigned_64 is
      pragma Unreferenced (authority);
      package T renames Owner_Test_Control;
      use type T.Scenario;
   begin
      T.Registrations:=T.Registrations+1;T.Registered_Role:=driver;
      T.Sequence:=T.Sequence+1;T.Registered_At:=T.Sequence;
      T.Registration_Result:=(if T.Selected=T.Register_Refused then Unsigned_64'Last else To_Word(Own_Process));
      return T.Registration_Result;
   end registerDriver;
   procedure debugPrint (str : String) is
      pragma Unreferenced (str);
   begin
      raise Program_Error with "unexpected guest boundary: debugPrint";
   end debugPrint;
end CuBit.Messages;
