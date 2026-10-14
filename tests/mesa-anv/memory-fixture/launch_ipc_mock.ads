with Interfaces; use Interfaces;
with CuBit.Messages;
--  CuBit.Handles.IPC for the launch client test: submissions are recorded,
--  replies carry what the test puts in their envelope (authorityTag with
--  Capability_Transferred), handles are closed into a counter.
package CuBit.Handles.IPC is
   subtype Message is CuBit.Messages.Message;
   function Submit_Accepting (Target : Endpoint; Msg : Message; Token : Unsigned_64)
     return Boolean;
   type Carried_Kind is (Absent, Present, Refused);
   type Carried (Kind : Carried_Kind := Absent) is record
      case Kind is
         when Absent  => null;
         when Present => Word : Unsigned_64 := 0;
         when Refused => Why  : Refusal := ABI.Limit_Reached;
      end case;
   end record;
   function Carried_By (Reply_Message : Message) return Carried;
   type Accepted_Endpoint (OK : Boolean := False) is record
      case OK is
         when True  => Value : Endpoint;
         when False => Why   : Refusal := ABI.Wrong_Kind;
      end case;
   end record;
   function Accept_Endpoint (Item : Carried) return Accepted_Endpoint;
   type Close_Result (Closed : Boolean := False) is record
      case Closed is
         when True  => null;
         when False => Why : Refusal := ABI.Invalid_Handle;
      end case;
   end record;
   function Close (Item : Endpoint) return Close_Result;

   Submit_Result : Boolean := True;
   Endpoint_Kind : Boolean := True;   --  carried words are endpoints
   Submits, Closes : Natural := 0;
   Last_Submit : Message := CuBit.Messages.NULL_MESSAGE;
   Last_Target : Endpoint := No_Endpoint;
   Last_Token : Unsigned_64 := 0;
   Last_Closed : Unsigned_64 := 0;
end CuBit.Handles.IPC;
