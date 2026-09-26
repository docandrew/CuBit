with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Config_Reader;
with CuBit.Config_Inspection;
with CCL.Objects.Persistence;
with Config_Database;
with Config_Database.Schemas;
with Config_Schema_Protocol;
with Config_Worker_Protocol;
with Config_Worker_Storage;
with Config_Worker_Messages;
with Config_Worker_Receiver;
with Config_Worker_Startup;

package body Worker_Host is
   use type System.Address;
   use type CuBit.Config_Inspection.Status;
   Saved_Reply : constant CapabilitySlot := 62;
   Current_Reply : constant CapabilitySlot := 63;

   function Database_Path
     (Buffer : System.Address; Capacity : Unsigned_64;
      Length : access Unsigned_64) return Unsigned_32
   is
      Value : CuBit.Config_Inspection.Text;
      Result : CuBit.Config_Inspection.Status;
   begin
      if Length = null then return 16#F001#; end if;
      Length.all := 0;
      if Buffer = System.Null_Address or Capacity < CuBit.Config_Inspection.Maximum_Text then
         return 16#F001#;
      end if;
      --  Read once from bootstrap Config, BEFORE opening the database. There
      --  is deliberately no path default and no lookup in our own database.
      CuBit.Config_Reader.Query
        (CuBit.Config_Inspection.Read_Value, "cubit.config.storage.database", Value, Result);
      if Result /= CuBit.Config_Inspection.OK then
         return CuBit.Config_Inspection.Status'Enum_Rep (Result);
      end if;
      declare
         Output : String (1 .. CuBit.Config_Inspection.Maximum_Text)
           with Import, Address => Buffer;
      begin
         Output (1 .. Value.Length) := Value.Data (1 .. Value.Length);
      end;
      Length.all := Unsigned_64 (Value.Length);
      return 16#F000#;
   end Database_Path;

   function Run
     (Database : System.Address; Config_Endpoint : Unsigned_64) return Unsigned_32
   is
   begin
      if Database = System.Null_Address or Config_Endpoint /= Unsigned_64 (CAP_SLOT_CONFIG) then
         return 16#F001#;
      end if;
      declare
         function Authorized (Sender : ProcessID; Tag : Unsigned_64) return Boolean is
            Owner : constant Unsigned_64 := getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_CONFIG);
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
         package Receiver is new Config_Worker_Receiver (CAP_SLOT_CONFIG, Authorized, Invoke, Invoke_Type);
         Worker : Receiver.State;
         Sender : ProcessID;
         Request, Response : Message;
         Ignore : Unsigned_64;
      begin
         debugPrint ("CONFIG-STORAGE: ready" & ASCII.LF);
         loop
            receive (Sender, Request);
            if not Authorized (Sender, Request.authorityTag) then
               Ignore := replyCap (Current_Reply, Config_Worker_Messages.Error (Config_Worker_Messages.Denied));
            elsif saveReplyCap (Unsigned_64 (Saved_Reply)) /= 1 then
               --  The move syscall returns 1 on success, not POSIX-style 0.
               --  No usable reply capability: do not borrow, stage, or write.
               Ignore := replyCap (Current_Reply, Config_Worker_Messages.Error (Config_Worker_Messages.Unavailable));
            else
               --  FS/clock IPC may occur during database work. Never depend
               --  on the implicit reply slot surviving those operations.
               Receiver.Handle (Worker, Sender, Request, Response);
               Ignore := replyCap (Saved_Reply, Response);
               if Receiver.Needs_Recovery (Worker) then return 16#F004#; end if;
            end if;
         end loop;
      end;
   end Run;
end Worker_Host;
