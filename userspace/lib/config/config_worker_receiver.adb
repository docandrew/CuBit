with System;
with CuBit.Memory_Grants;
with Config_Worker_Messages;
with CCL.Objects.Schemas;

package body Config_Worker_Receiver is
   package P renames Config_Worker_Protocol;
   package T renames Config_Schema_Protocol;
   package Wire renames Config_Worker_Messages;
   package Grants renames CuBit.Memory_Grants;
   use type CCL.Objects.Schema_Key;
   function Needs_Recovery (Object : State) return Boolean is
     (Object.Failed_Transfer or Executor.Needs_Recovery (Object.Worker) or
      Type_Executor.Needs_Recovery (Object.Types));

   procedure Handle
     (Object : in out State; Sender : CuBit.Messages.ProcessID;
      Request : CuBit.Messages.Message;
      Reply : out CuBit.Messages.Message)
   is
      Reference : Grants.Grant_Reference;
      Address : System.Address;
      Acquired, Returned, Accepted : Boolean;
      Owned_Request, Owned_Response : P.Frame;
      Contract : CCL.Objects.Binding;
      Found : Boolean := False;
   begin
      Reply := Wire.Error (Wire.Denied);
      if not Authorized_Source (Sender, Request.authorityTag) then return; end if;
      Reply := Wire.Error (Wire.Unavailable);
      if Needs_Recovery (Object) then return; end if;
      Reply := Wire.Error (Wire.Invalid_Request);
      if Wire.Valid_Type_Request (Request) then
         Reference := (slot => Request.words (0), generation => Request.words (1));
         Grants.Acquire_Via_Capability
           (Owner_Endpoint, Reference, 0, T.Frame_Bytes, Grants.Read_Access, Address, Acquired);
         if not Acquired then return; end if;
         declare
            Input, Output : T.Frame;
         begin
            declare
               Shared : T.Frame with Import, Volatile, Address => Address;
            begin
               Input := Shared;
            end;
            Grants.Return_Acquisition (Reference, Returned);
            if not Returned then
               Object.Failed_Transfer := True;
               Reply := Wire.Error (Wire.Unavailable); return;
            end if;
            Type_Executor.Handle (Object.Types, Input, Output, Accepted);
            if not Accepted then return; end if;
            Reply := Wire.Error (Wire.Unavailable);
            Grants.Acquire_Via_Capability
              (Owner_Endpoint, Reference, 0, T.Frame_Bytes, Grants.Write_Access, Address, Acquired);
            if not Acquired then Object.Failed_Transfer := True; return; end if;
            declare
               Shared : T.Frame with Import, Volatile, Address => Address;
            begin
               Shared := Output;
            end;
            Grants.Return_Acquisition (Reference, Returned);
            if not Returned then Object.Failed_Transfer := True; return; end if;
         end;
         Reply := Wire.Type_Acknowledgment;
         return;
      end if;
      if Wire.Valid_Schema_Request (Request) then
         Reference := (slot => Request.words (0), generation => Request.words (1));
         Grants.Acquire_Via_Capability
           (Owner_Endpoint, Reference, 0, CCL.Objects.Schemas.Native_Schema_Bytes,
            Grants.Read_Access, Address, Acquired);
         if not Acquired then return; end if;
         declare
            Owned_Schema : CCL.Objects.Schemas.Image;
         begin
            declare
               Shared : CCL.Objects.Schemas.Image with Import, Volatile, Address => Address;
            begin
               Owned_Schema := Shared;
            end;
            Grants.Return_Acquisition (Reference, Returned);
            if not Returned then
               Object.Failed_Transfer := True;
               Reply := Wire.Error (Wire.Unavailable); return;
            end if;
            CCL.Objects.Schemas.Read (Owned_Schema, Contract, Accepted);
         end;
         if not Accepted then return; end if;
         for Index in 1 .. Object.Schema_Count loop
            if CCL.Objects.Identity (Object.Schemas (Index)) = CCL.Objects.Identity (Contract) then
               --  Idempotent acknowledgment, never silently redefine a key.
               if CCL.Objects.Same_Schema (Object.Schemas (Index), Contract) then
                  Reply := Wire.Schema_Acknowledgment;
               end if;
               return;
            end if;
         end loop;
         if Object.Schema_Count = Maximum_Schemas then
            Reply := Wire.Error (Wire.Unavailable); return;
         end if;
         Object.Schema_Count := Object.Schema_Count + 1;
         Object.Schemas (Object.Schema_Count) := Contract;
         Reply := Wire.Schema_Acknowledgment;
         return;
      end if;
      if not Wire.Valid_Request (Request) then return; end if;
      Reference := (slot => Request.words (0), generation => Request.words (1));
      Grants.Acquire_Via_Capability
        (Owner_Endpoint, Reference, 0, P.Frame_Bytes, Grants.Read_Access, Address, Acquired);
      if not Acquired then return; end if;
      declare
         Shared : P.Frame with Import, Volatile, Address => Address;
      begin
         Owned_Request := Shared;
      end;
      Grants.Return_Acquisition (Reference, Returned);
      if not Returned then
         Object.Failed_Transfer := True;
         Reply := Wire.Error (Wire.Unavailable);
         return;
      end if;
      --  The request mapping is released before disk IO. Revoking the original
      --  grant now prevents delivery, not an already accepted operation. No
      --  client memory can change the owned request while storage is blocked.
      for Index in 1 .. Object.Schema_Count loop
         if CCL.Objects.Identity (Object.Schemas (Index)) = Owned_Request.Value.Schema then
            Contract := Object.Schemas (Index); Found := True; exit;
         end if;
      end loop;
      if not Found then return; end if;
      Executor.Handle (Object.Worker, Owned_Request, Contract, Owned_Response, Accepted);
      if not Accepted then return; end if;
      Reply := Wire.Error (Wire.Unavailable);
      Grants.Acquire_Via_Capability
        (Owner_Endpoint, Reference, 0, P.Frame_Bytes, Grants.Write_Access, Address, Acquired);
      if not Acquired then Object.Failed_Transfer := True; return; end if;
      declare
         Shared : P.Frame with Import, Volatile, Address => Address;
      begin
         Shared := Owned_Response;
      end;
      Grants.Return_Acquisition (Reference, Returned);
      if not Returned then Object.Failed_Transfer := True; return; end if;
      Reply := Wire.Acknowledgment;
   end Handle;
end Config_Worker_Receiver;
