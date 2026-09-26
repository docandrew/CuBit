with CCL.Objects;
with CuBit.Grant_References;
with Config_Object_Messages;
with CCL.Objects.Schemas;
with Config_Collections;

package body Config_Object_Receiver is
   package Wire renames Config_Object_Messages;
   package D renames Config_Object_Dispatch;
   package Grants renames CuBit.Memory_Grants;
   use type Interfaces.Unsigned_32;
   use type Interfaces.Unsigned_64;
   use type Wire.Operation;
   use type D.Disposition;
   use type Config_Collections.Result;
   use type Wire.Status;
   use type CCL.Objects.Schema_Key;
   Current_Reply : constant CuBit.Messages.CapabilitySlot := 63;
   function Needs_Recovery (Object : State) return Boolean is (Object.Failed_Transfer);
   function Waiting (Object : State) return Boolean is (D.Waiting (Object.Dispatch) or Object.Acquiring_Definition);
   function Definition_Pending (Object : State) return Boolean is (Object.Acquiring_Definition);

   procedure Deliver (Slot : CuBit.Messages.CapabilitySlot; Reply : CuBit.Messages.Message) is
      Ignored : Interfaces.Unsigned_64;
   begin
      --  Kernel reply consumes the selected authority even if its caller died.
      --  A failed delivery does not undo the operation or authorize a retry.
      Ignored := Send_Reply (Slot, Reply);
   end Deliver;

   procedure Deliver_Acquisition
     (Store : in out Config_Typed_Store.State; Subject : Config_Authority.Subject_ID;
      Slot : CuBit.Messages.CapabilitySlot; Reply : CuBit.Messages.Message)
   is
      Delivered : constant Interfaces.Unsigned_64 := Send_Reply (Slot, Reply);
      Closed : Config_Collections.Result;
   begin
      -- Only Open/Create replies carry a freshly minted handle in word 0.
      -- Successful Set replies carry a revision: never apply this path to them.
      -- Failed delivery does not roll back creation or mutate any stored value.
      if Delivered /= 1 and then Reply.tag.label = Wire.Status'Enum_Rep (Wire.Success) then
         Config_Typed_Store.Close (Store, Subject, Reply.words (0), Closed);
      end if;
   end Deliver_Acquisition;

   procedure Begin_Definition
     (Object : in out State; Store : in out Config_Typed_Store.State;
      Action : Definition_Operation;
      Authority : Config_Authority.Authority_State; Sender : CuBit.Messages.ProcessID;
      Request : CuBit.Messages.Message; Storage_Available : Boolean; Staged : out Boolean)
   is
      package A renames Config_Authority;
      Control : Wire.Open_Descriptor;
      Metadata : CCL.Objects.Schemas.Image;
      Contract : CCL.Objects.Binding;
      Reference : Grants.Grant_Reference;
      Address : System.Address;
      Good, Returned : Boolean;
      ID : Config_Collections.Collection_ID;
      Admission : Config_Collections.Result;
      Handle : Config_Collections.Handle;
      Requested : A.Rights;
   begin
      Staged := False;
      if Needs_Recovery (Object) or Saved_Reply_Slot = Current_Reply then
         Deliver (Current_Reply, Wire.Reply (Wire.Unavailable)); return;
      end if;
      if not Wire.Valid_Request (Request, Action) then
         Deliver (Current_Reply, Wire.Reply (Wire.Invalid_Request)); return;
      end if;
      if not A.Has_Profile (Authority, Sender) then
         Deliver (Current_Reply, Wire.Reply (Wire.Denied)); return;
      end if;
      Reference := CuBit.Grant_References.Decode (Request.words (0));
      Acquire (Reference, Sender, 0, Wire.Control_Bytes, Grants.Read_Access, Address, Good);
      if not Good then Deliver (Current_Reply, Wire.Reply (Wire.Unavailable)); return; end if;
      declare
         Shared : Wire.Open_Descriptor with Import, Volatile, Address => Address;
      begin
         Control := Shared;
      end;
      Return_Acquisition (Reference, Returned);
      if not Returned then
         Object.Failed_Transfer := True;
         Deliver (Current_Reply, Wire.Reply (Wire.Unavailable)); return;
      end if;
      if not Wire.Valid_Descriptor (Control) or else Control.Context /= Config_Collections.Machine_Context then
         Deliver (Current_Reply, Wire.Reply (Wire.Invalid_Request)); return;
      end if;
      Requested := [A.Read_Config => Control.Access_Rights /= Wire.Access_Mode'Enum_Rep (Wire.Write_Only),
                    A.Write_Config => Control.Access_Rights /= Wire.Access_Mode'Enum_Rep (Wire.Read_Only)];
      if ((Action = Wire.Create_Collection or Requested (A.Write_Config)) and then
          not A.Allows (Authority, Sender, Control.Name (1 .. Natural (Control.Name_Length)), A.Write_Config))
        or else (Requested (A.Read_Config) and then
          not A.Allows (Authority, Sender, Control.Name (1 .. Natural (Control.Name_Length)), A.Read_Config))
      then Deliver (Current_Reply, Wire.Reply (Wire.Denied)); return; end if;
      if Action = Wire.Open_Collection then
         -- Cached opens never queue disk work or compete for the saved reply.
         Config_Typed_Store.Open (Store, Authority, Sender,
           Control.Name (1 .. Natural (Control.Name_Length)), Control.Context,
           Requested, Control.Schema, Handle, Admission);
         if Admission /= Config_Collections.Missing then
            Deliver_Acquisition (Store, Sender, Current_Reply, Wire.Reply
              ((case Admission is
                  when Config_Collections.Opened => Wire.Success,
                  when Config_Collections.Denied => Wire.Denied,
                  when Config_Collections.Schema_Conflict => Wire.Schema_Mismatch,
                  when Config_Collections.Capacity_Exceeded | Config_Collections.Identity_Exhausted => Wire.Capacity_Exceeded,
                  when others => Wire.Unavailable), Handle));
            return;
         end if;
      end if;
      if Waiting (Object) or else not Storage_Available then
         Deliver (Current_Reply, Wire.Reply (if Waiting (Object) then Wire.Busy else Wire.Unavailable)); return;
      end if;
      if Action = Wire.Create_Collection then
         -- Type metadata is borrowed only AFTER namespace/rights admission.
         Acquire (Reference, Sender, Wire.Control_Bytes, CCL.Objects.Schemas.Native_Schema_Bytes,
           Grants.Read_Access, Address, Good);
         if not Good then Deliver (Current_Reply, Wire.Reply (Wire.Unavailable)); return; end if;
         declare
            Shared : CCL.Objects.Schemas.Image with Import, Volatile, Address => Address;
         begin
            Metadata := Shared;
         end;
         Return_Acquisition (Reference, Returned);
         if not Returned then
            Object.Failed_Transfer := True;
            Deliver (Current_Reply, Wire.Reply (Wire.Unavailable)); return;
         end if;
         CCL.Objects.Schemas.Read (Metadata, Contract, Good);
         if not Good or else CCL.Objects.Identity (Contract) /= Control.Schema then
            Deliver (Current_Reply, Wire.Reply (Wire.Schema_Mismatch)); return;
         end if;
         Config_Typed_Store.Check_Registration (Store, Control.Name (1 .. Natural (Control.Name_Length)),
           Contract, ID, Admission);
         if Admission not in Config_Collections.Registered | Config_Collections.Already_Registered then
            Deliver (Current_Reply, Wire.Reply
              (if Admission = Config_Collections.Capacity_Exceeded then Wire.Capacity_Exceeded else Wire.Schema_Mismatch));
            return;
         end if;
      end if;
      if Save_Reply (Saved_Reply_Slot) /= 1 then
         Deliver (Current_Reply, Wire.Reply (Wire.Unavailable)); return;
      end if;
      Object.Definition_Control := Control; Object.Definition_Contract := Contract;
      Object.Caller := Sender; Object.Grant_Revision := A.Revision (Authority, Sender);
      Object.Definition_Action := Action;
      Object.Acquiring_Definition := True; Staged := True;
   end Begin_Definition;

   procedure Pending_Definition
     (Object : State; Control : out Wire.Open_Descriptor; Contract : out CCL.Objects.Binding) is
   begin
      Control := Object.Definition_Control; Contract := Object.Definition_Contract;
   end Pending_Definition;

   procedure Finish_Definition
     (Object : in out State; Store : in out Config_Typed_Store.State;
      Authority : Config_Authority.Authority_State; Code : Wire.Status)
   is
      package A renames Config_Authority;
      Handle : Config_Collections.Handle := 0;
      Result : Config_Collections.Result;
      Reply_Code : Wire.Status := Code;
      Control : constant Wire.Open_Descriptor := Object.Definition_Control;
      Requested : A.Rights;
   begin
      if not Object.Acquiring_Definition then return; end if;
      Object.Acquiring_Definition := False;
      if Object.Grant_Revision /= A.Revision (Authority, Object.Caller) then
         -- Even Missing/schema errors must not disclose a delayed lookup to
         -- a caller whose authority lifetime has ended.
         Reply_Code := Wire.Denied;
      elsif Code = Wire.Success then
         Reply_Code := Wire.Denied;
         if Object.Definition_Action = Wire.Open_Collection or else
           A.Allows (Authority, Object.Caller, Control.Name (1 .. Natural (Control.Name_Length)), A.Write_Config)
         then
            Requested := [A.Read_Config => Control.Access_Rights /= Wire.Access_Mode'Enum_Rep (Wire.Write_Only),
                          A.Write_Config => Control.Access_Rights /= Wire.Access_Mode'Enum_Rep (Wire.Read_Only)];
            Config_Typed_Store.Open (Store, Authority, Object.Caller,
              Control.Name (1 .. Natural (Control.Name_Length)), Control.Context, Requested,
              Control.Schema, Handle, Result);
            Reply_Code := (case Result is when Config_Collections.Opened => Wire.Success,
              when Config_Collections.Denied => Wire.Denied,
              when Config_Collections.Capacity_Exceeded | Config_Collections.Identity_Exhausted => Wire.Capacity_Exceeded,
              when Config_Collections.Schema_Conflict => Wire.Schema_Mismatch,
              when others =>
                (if Object.Definition_Action = Wire.Create_Collection then Wire.Uncertain
                 else Wire.Unavailable));
         end if;
      end if;
      Deliver_Acquisition (Store, Object.Caller, Saved_Reply_Slot, Wire.Reply (Reply_Code, Handle));
   end Finish_Definition;

   procedure Handle
     (Object : in out State; Store : in out Config_Typed_Store.State;
      Authority : Config_Authority.Authority_State;
      Sender : CuBit.Messages.ProcessID; Request : CuBit.Messages.Message;
      Storage_Token : Interfaces.Unsigned_64; Staged : out Boolean)
   is
      Action : Wire.Operation := Wire.Open_Collection;
      Known : Boolean := False;
      Reference : Grants.Grant_Reference;
      Address : System.Address;
      Acquired, Returned, Reserved : Boolean := False;
      Input : Wire.Frame;
      Value : CCL.Objects.Image;
      Reply : CuBit.Messages.Message;
      Next : D.Disposition;
      Reply_Slot : CuBit.Messages.CapabilitySlot := Current_Reply;
   begin
      Staged := False;
      if Needs_Recovery (Object) or else Saved_Reply_Slot = Current_Reply then
         Deliver (Current_Reply, Wire.Reply (Wire.Unavailable)); return;
      end if;
      --  Never convert an unchecked wire word into an enumeration.
      for Candidate in Wire.Operation loop
         if Request.tag.label = Wire.Operation'Enum_Rep (Candidate) then
            Action := Candidate; Known := True; exit;
         end if;
      end loop;
      if not Known or else not Wire.Valid_Request (Request, Action) then
         Deliver (Current_Reply, Wire.Reply (Wire.Invalid_Request)); return;
      end if;
      if Action = Wire.Create_Collection then
         -- Only the owning service may start the durable multi-stage path.
         Deliver (Current_Reply, Wire.Reply (Wire.Unavailable)); return;
      end if;
      if Action = Wire.Set_Object then
         if not Config_Typed_Store.Check_Access
           (Store, Authority, Sender, Request.words (0), Config_Authority.Write_Config)
         then
            Deliver (Current_Reply, Wire.Reply (Wire.Denied)); return;
         end if;
         --  In particular, do not overwrite another client's saved reply.
         if Waiting (Object) then
            Deliver (Current_Reply, Wire.Reply (Wire.Busy)); return;
         end if;
      end if;
      if Action /= Wire.Close_Collection then
         Reference := CuBit.Grant_References.Decode
           (Request.words (if Action = Wire.Open_Collection then 0 else 1));
      end if;
      if Action in Wire.Open_Collection | Wire.Set_Object then
         Acquire (Reference, Sender,
           (if Action = Wire.Open_Collection then 0 else Wire.Value_Offset),
           (if Action = Wire.Open_Collection then Wire.Control_Bytes else CCL.Objects.Native_Image_Bytes),
           Grants.Read_Access, Address, Acquired);
         if not Acquired then
            Deliver (Current_Reply, Wire.Reply (Wire.Unavailable)); return;
         end if;
         if Action = Wire.Open_Collection then
            declare
               Shared : Wire.Open_Descriptor with Import, Volatile, Address => Address;
            begin
               Input.Control := Shared;
            end;
         else
            declare
               Shared : CCL.Objects.Image with Import, Volatile, Address => Address;
            begin
               Input.Value := Shared;
            end;
         end if;
         Return_Acquisition (Reference, Returned);
         if not Returned then
            Object.Failed_Transfer := True;
            Deliver (Current_Reply, Wire.Reply (Wire.Unavailable)); return;
         end if;
      end if;
      if Action = Wire.Set_Object then
         Reserved := Save_Reply (Saved_Reply_Slot) = 1;
         if Reserved then Reply_Slot := Saved_Reply_Slot; end if;
      end if;
      D.Handle (Object.Dispatch, Store, Authority, Sender, Action, Request,
                Input, Storage_Token, Reserved, Reply, Value, Next);
      if Next = D.Await_Storage then Staged := True; return; end if;
      if Action = Wire.Get_Object and then
        Reply.tag.label in Wire.Status'Enum_Rep (Wire.Success) | Wire.Status'Enum_Rep (Wire.Stale)
      then
         --  Only authorized, validated cache output reaches a caller's grant.
         Acquire (Reference, Sender, Wire.Value_Offset, CCL.Objects.Native_Image_Bytes,
                  Grants.Write_Access, Address, Acquired);
         if not Acquired then
            Reply := Wire.Reply (Wire.Unavailable);
         else
            declare
               Shared : CCL.Objects.Image with Import, Volatile, Address => Address;
            begin
               Shared := Value;
            end;
            Return_Acquisition (Reference, Returned);
            if not Returned then
               Object.Failed_Transfer := True;
               Reply := Wire.Reply (Wire.Unavailable);
            end if;
         end if;
      end if;
      if Action = Wire.Open_Collection then
         Deliver_Acquisition (Store, Sender, Reply_Slot, Reply);
      else
         Deliver (Reply_Slot, Reply);
      end if;
   end Handle;

   procedure Finish
     (Object : in out State; Store : in out Config_Typed_Store.State;
      Response : Config_Worker_Protocol.Frame)
   is
      Reply : CuBit.Messages.Message;
      Ready : Boolean;
   begin
      D.Finish (Object.Dispatch, Store, Response, Reply, Ready);
      if Ready then Deliver (Saved_Reply_Slot, Reply); end if;
   end Finish;

   procedure Lost
     (Object : in out State; Store : in out Config_Typed_Store.State;
      Session : Interfaces.Unsigned_64)
   is
      Reply : CuBit.Messages.Message;
      Ready : Boolean;
   begin
      D.Lost (Object.Dispatch, Store, Session, Reply, Ready);
      if Ready then Deliver (Saved_Reply_Slot, Reply); end if;
      if Object.Acquiring_Definition then
         Object.Acquiring_Definition := False;
         -- Create can have persisted its definition before the receipt or
         -- subsequent value recovery was lost. An Open lookup is read-only.
         Deliver (Saved_Reply_Slot, Wire.Reply
           (if Object.Definition_Action = Wire.Create_Collection then Wire.Uncertain
            else Wire.Unavailable));
      end if;
   end Lost;
end Config_Object_Receiver;
