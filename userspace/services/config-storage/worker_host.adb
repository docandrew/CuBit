with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Settings;
with CCL.Objects.Persistence;
with Config_Database;
with Config_Database.Schemas;
with Config_Schema_Protocol;
with Config_Worker_Protocol;
with Config_Worker_Storage;
with Config_Worker_Messages;
with Config_Worker_Receiver;
with Config_Worker_Startup;
with CuBit.Handles;
with CuBit.Handles.IPC;
with CuBit.Handles.Services;
with Startup;

package body Worker_Host is
   --  The Config endpoint: the program's "config" startup entry. Its
   --  presence is not checked here: the generated Startup package's check
   --  stops the program before this library runs if the entry, or the
   --  whole startup table, is missing (CAP-003).
   function Config_Service return CuBit.Handles.Endpoint is (CuBit.Handles.Services.Config.Handle (Startup.Config));
   use type System.Address;

   function Database_Path
     (Buffer : System.Address; Capacity : Unsigned_64;
      Length : access Unsigned_64) return Unsigned_32
   is
      use all type CuBit.Settings.Outcome;
      package Setting renames Startup.Settings.Cubit_Config_Storage_Database;
      --  The answers the Rust worker tells apart (src/lib.rs): a path, or none.
      Path_Read : constant := 16#F000#;
      No_Path : constant := 16#F001#;
   begin
      if Length = null then return No_Path; end if;
      Length.all := 0;
      if Buffer = System.Null_Address then
         return No_Path;
      end if;
      --  Read once from bootstrap Config, BEFORE opening the database. There
      --  is deliberately no path default and no lookup in our own database.
      declare
         Stored : constant Setting.Reading := Setting.Read;
      begin
         if Stored.Kind /= Present then
            debugPrint ("CONFIG-STORAGE: reading " & Setting.Setting_Key & ": " &
                        CuBit.Settings.Outcome'Image (Stored.Kind) & ASCII.LF);
            return No_Path;
         end if;
         if Unsigned_64 (Stored.Item.Length) > Capacity then
            debugPrint ("CONFIG-STORAGE: " & Setting.Setting_Key & " is longer than the worker's buffer" & ASCII.LF);
            return No_Path;
         end if;
         declare
            Output : String (1 .. Stored.Item.Length) with Import, Address => Buffer;
         begin
            Output := CuBit.Settings.Image (Stored.Item);
         end;
         Length.all := Unsigned_64 (Stored.Item.Length);
      end;
      return Path_Read;
   end Database_Path;

   function Run (Database : System.Address) return Unsigned_32 is
   begin
      if Database = System.Null_Address then
         return 16#F001#;
      end if;
      declare
         function Authorized (Sender : Process_ID; Tag : Unsigned_64) return Boolean is
            Owner : constant Process_ID := Registered_Driver (DRIVER_CONFIG);
         begin
            --  Kernel-stamped sender only. The held endpoint additionally
            --  pins grant ownership/lifetime in Acquire_Via_Capability.
            return Config_Worker_Startup.Authorized_Config_Request (Sender, Tag, Owner);
         end Authorized;
         procedure Invoke
           (Action : Config_Worker_Protocol.Operation; Name, Context : String;
            Expected_Revision : Config_Worker_Protocol.Number;
            Schema : CCL.Objects.Schema_Key; Input : CCL.Objects.Persistence.Packet;
            Output : out Config_Worker_Storage.Reply) is
         begin
            Config_Database.Invoke (Database, Action, Name, Context, Expected_Revision,
                                    Schema, Input, Output);
         end Invoke;
         procedure Invoke_Type
           (Action : Config_Schema_Protocol.Operation; Name, Context : String;
            Contract : CCL.Objects.Binding; Recovered : out CCL.Objects.Binding;
            Result : out Config_Schema_Protocol.Reply_Kind) is
         begin
            Config_Database.Schemas.Invoke (Database, Action, Name, Context, Contract, Recovered, Result);
         end Invoke_Type;
         package Receiver is new Config_Worker_Receiver (Config_Service, Authorized, Invoke, Invoke_Type);
         Worker : Receiver.State;
         Sender : Process_ID;
         Request, Response : Message;
         Ignore : Unsigned_64;
      begin
         debugPrint ("CONFIG-STORAGE: ready" & ASCII.LF);
         loop
            receive (Sender, Request);
            if not Authorized (Sender, Request.authorityTag) then
               Ignore := reply (Sender, Config_Worker_Messages.Error (Config_Worker_Messages.Denied));
            else
               declare
                  --  FS/clock IPC may occur during database work: answer
                  --  through the request's own saved reply, never the
                  --  thread's current one.
                  Saved : constant CuBit.Handles.IPC.Saved_Reply := CuBit.Handles.IPC.Save_Reply;
               begin
                  if not Saved.Saved then
                     --  No usable reply: do not borrow, stage, or write.
                     Ignore := reply
                       (Sender, Config_Worker_Messages.Error (Config_Worker_Messages.Unavailable));
                  else
                     Receiver.Handle (Worker, Sender, Request, Response);
                     Ignore := replyCap (Saved.Handle, Response);
                     if Receiver.Needs_Recovery (Worker) then return 16#F004#; end if;
                  end if;
               end;
            end if;
         end loop;
      end;
   end Run;
end Worker_Host;
