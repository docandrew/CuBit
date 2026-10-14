with Interfaces; use Interfaces;
with System;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Launch_Configurations;
with CuBit.Process_IDs;
with CuBit.Startup_Tables;
with CuBit.Handles;
with CuBit.Handles.Boot_Roots;
with CuBit.Handles.C_ABI;
with CuBit.Handles.Control;
with CuBit.Handles.IPC;
with Launch_Configuration;
with Cpio;
with Native_GPU_Buffers;
with Intel_GPU_Buffer_Requests;
with Intel_GPU_Buffer_Backing;
with Intel_GPU_Buffer_Reply;
with Intel_GPU_Extent_Allocator;
procedure Accounting_Check is
   --  The boot image's size: the kernel's launch configuration for the
   --  process it starts in devmgr's place.
   function Boot_Image_Bytes return Unsigned_64 is
      Configuration : Launch_Configuration.Device_Manager_Configuration;
      Configured : Boolean;
   begin
      CuBit.Launch_Configurations.Get (Configuration, Configured);
      return (if Configured then Configuration.Boot_Image else 0);
   end Boot_Image_Bytes;
   package IPC renames CuBit.Handles.IPC;
   PID : constant Unsigned_64 := syscall (SYSCALL_GETPID);
   --  The supervisor runs in devmgr's place (boot roots); the child it
   --  spawns from the same ELF has none.
   Roots : constant CuBit.Handles.Boot_Roots.Roots := CuBit.Handles.Boot_Roots.Get;
   Is_Child : constant Boolean := not Roots.Valid;
   Child : Unsigned_64 := 0;
   --  Three session endpoints to the supervisor, offered to the child one
   --  per call: two admitted accounts and one the supervisor denies.
   Offer_Label : constant Unsigned_32 := 16#F031#;
   type Session is range 1 .. 3;
   Session_Badge : constant array (Session) of Unsigned_64 :=
     [16#4947#, 16#4948#, 16#4949#];
   Saved_Reply : CuBit.Handles.Reply;
   Ignore : Unsigned_64;
   function Owner return Boolean is (True);
   function Session_Of (Sender, Stamp : Unsigned_64) return Unsigned_64 is
     (if Sender = Child and then Stamp in 16#4947# | 16#4948# then Stamp else 0);
   package B is new Intel_GPU_Buffer_Requests (Session_Of, Owner);
   package R renames Intel_GPU_Buffer_Reply;
   package Client renames Native_GPU_Buffers;
   Object : B.Service;
   Calls, Requests : Natural := 0;
   procedure Check (Value : Boolean; Detail : String) is
   begin
      if not Value then
         debugPrint ("TEST: FAIL native accounting adapter: " & Detail & ASCII.LF);
         Ignore := syscall (SYSCALL_EXIT); loop null; end loop;
      end if;
   end Check;
   function Allocate (CPU : Unsigned_64) return Unsigned_64 is
   begin
      Calls := Calls + 1;
      return CuBit.Handles.Control.Allocate_DMA
        (CuBit.Process_IDs.From_Word (PID), Roots.Process_Root, 9, CPU, 3, 2 ** 32);
   end Allocate;
   package A is new Intel_GPU_Extent_Allocator (Owner, Allocate);
   Pool : A.Pool;
   ELF : System.Address;
   ELF_Size : Unsigned_64;
   Archive : Cpio.Archive;
   Msg, Response : Message := NULL_MESSAGE;
   From : Process_ID;
   OK, Found, Consumed : Boolean;
   Ticket : B.Ticket;
   Words : B.Words;
   View : R.Extent_View;
   Deadline : Unsigned_64;
begin
   if Is_Child then
      declare
         Limit, Charged : aliased Unsigned_64 := 999;
         Handle, Other : aliased Unsigned_32 := 0;
         Sessions : array (Session) of CuBit.Handles.Endpoint;
         function S (Item : Session) return CuBit.Handles.C_ABI.C_Endpoint is
           (CuBit.Handles.C_ABI.To_C (Sessions (Item)));
         procedure Query (Item : Session; Code : Unsigned_32;
                          Expected : Unsigned_64 := 0) is
         begin
            Limit := 999; Charged := 999;
            Check (Client.Query_Accounting (S (Item), Limit'Access, Charged'Access) = Code,
              "query status");
            Check (Charged = Expected and then Limit = (if Code = 0 then 12288 else 0),
              "query values");
         end Query;
      begin
         for Item in Session loop
            receive (From, Msg);
            Check (Msg.tag.label = Offer_Label and then IPC.Offers (Msg), "session offer");
            declare
               Taken : constant IPC.Accepted_Endpoint :=
                 IPC.Accept_Endpoint (IPC.Accept_Offered (From));
            begin
               Check (Taken.OK, "session accepted");
               Sessions (Item) := Taken.Value;
            end;
            Msg := NULL_MESSAGE;
            Msg.tag := (label => Offer_Label, length => 0, protocol => 0, ipc => <>, reserved => 0);
            Check (reply (From, Msg) = 1, "offer answer");
         end loop;
         Query (1, 3);
         Check (Client.Create (S (1), 8192, Handle'Access) = 0, "create");
         Query (1, 0, 8192);
         Query (2, 3);
         Query (3, 1);
         Check (Client.Create (S (1), 8192, Other'Access) = 3 and Other = 0, "quota denial");
         Query (1, 0, 8192);
         Check (Client.Create (S (1), 4096, Other'Access) = 0 and Other /= Handle,
           "smaller recovery");
         Check (Client.Close (S (1), Handle) = 0, "close");
         Query (1, 0, 12288);
         Check (Client.Create (S (2), 4096, Other'Access) = 0, "second account");
         Query (2, 0, 4096);
         Query (1, 0, 12288);
         Msg.tag := (label => 16#F030#, length => 0, protocol => 0, ipc => <>, reserved => 0);
         Msg.tag := capCall (Sessions (1), Msg, Wait_Forever);
         Check (Msg.tag = (label => 16#F030#, length => 0,
                           protocol => 0, ipc => <>, reserved => 0), "done reply");
         debugPrint ("TEST: PASS native accounting adapter child transport completed (NO GPU)" & ASCII.LF);
      end;
      Ignore := syscall (SYSCALL_EXIT); loop null; end loop;
   end if;
   debugPrint ("native accounting adapter: production client and handler, separate processes (NO GPU)" & ASCII.LF);
   B.Configure_Client_Budgets (Object, 12288, OK); Check (OK, "budget");
   Cpio.init (Archive, To_Address (16#0000_5000_0000_0000#),
     Boot_Image_Bytes, OK); Check (OK, "initrd");
   Cpio.fileView (Archive, Cpio.findFile (Archive, "devmgr.svc"), ELF, ELF_Size, OK);
   Check (OK, "ELF");
   declare
      Result : constant CuBit.Handles.Control.Spawn_Result :=
        CuBit.Handles.Control.Spawn
          (Roots.Process_Root, ELF, ELF_Size, 5, "accounting-child",
           CuBit.Process_IDs.No_Process, (Kind => CuBit.Startup_Tables.Standard_Limit));
   begin
      Check (Result.Spawned, "spawn");
      Child := CuBit.Process_IDs.To_Word (Result.Value.Child);
      Check (CuBit.Handles.Control.Resume
               (Result.Value.Child, Result.Value.Control, 0, 0), "resume");
      for Item in Session loop
         Msg := NULL_MESSAGE;
         Msg.tag := (label => Offer_Label, length => 0, protocol => 0, ipc => <>, reserved => 0);
         Msg.tag := IPC.Call_Offering
           (Result.Value.Mailbox, Msg, Wait_Forever,
            CuBit.Handles.Badged (Roots.Self_Grantable, CuBit.Handles.Read_Write,
                                  Session_Badge (Item)));
         Check (Msg.tag.label = Offer_Label, "endpoint offered");
      end loop;
   end;
   Deadline := syscall (SYSCALL_GETTIME) + 60_000;
   loop
      Poll_Service_Request (From, Msg, Found);
      if not Found then
         Check (syscall (SYSCALL_GETTIME) < Deadline, "deadline");
      else
         Check (CuBit.Process_IDs.To_Word (From) = Child, "kernel sender");
         if Msg.tag.label = 16#F030# then
            Check (Requests = 13 and Calls = 1, "request/backing counts");
            Check (B.Client_Usage (Object, 16#4947#).Charged = 12288 and
                   B.Client_Usage (Object, 16#4948#).Charged = 4096,
              "independent retained accounting");
            debugPrint ("native accounting adapter: thirteen authenticated calls and one DMA extent PASS" & ASCII.LF);
            Check (reply (From, Msg) = 1, "done delivery");
            exit;
         end if;
         Requests := Requests + 1;
         Ticket := 0;
         if Msg.tag.label = B.Accounting_Label then
            B.Query_Accounting (Object, CuBit.Process_IDs.To_Word (From), Msg.authorityTag,
              Msg.tag.label, Msg.tag.length, Msg.tag.protocol, Msg.tag.reserved,
              B.Words (Msg.words), Words);
         else
            Check (Msg.tag.label = B.Label, "label");
            B.Handle (Object, CuBit.Process_IDs.To_Word (From), Msg.authorityTag,
              Msg.tag.label, Msg.tag.length, Msg.tag.protocol, Msg.tag.reserved,
              B.Words (Msg.words), Words, Ticket);
         end if;
         if Ticket /= 0 then
            declare
               Saved : constant IPC.Saved_Reply := IPC.Save_Reply;
            begin
               Check (Saved.Saved, "saved reply");
               Saved_Reply := Saved.Handle;
            end;
            A.Acquire_Buffer (Pool, PID, B.Ticket_Slot (Ticket),
              Intel_GPU_Buffer_Backing.Page_Count (Msg.words (2) / 4096),
              B.Ticket_Generation (Ticket), View, OK);
            Check (OK, "backing");
            B.Complete (Object, Ticket, R.From_View (View), Words, Consumed);
            Check (Consumed, "completion");
         end if;
         Response.tag := Msg.tag;
         Response.words := MessageWords (Words);
         if Ticket /= 0 then
            Check (IPC.Answer (Saved_Reply, Response), "deferred delivery");
         else
            Check (reply (From, Response) = 1, "delivery");
         end if;
      end if;
   end loop;
   -- Retain supervisor-owned backing until the runner observes child success.
   loop Ignore := syscall (SYSCALL_SLEEP, 100); end loop;
end Accounting_Check;
