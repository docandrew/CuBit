with Interfaces; use Interfaces;
with System;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
with Cpio;
with Native_GPU_Buffers;
with Intel_GPU_Buffer_Requests;
with Intel_GPU_Buffer_Backing;
with Intel_GPU_Buffer_Reply;
with Intel_GPU_Extent_Allocator;
procedure Accounting_Check is
   PID : constant Unsigned_64 := syscall (SYSCALL_GETPID);
   Child : constant Unsigned_64 := 42;
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
      return syscall (SYSCALL_ALLOC_DMA, PID, 9, CPU, 3, 2 ** 32);
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
   if PID = Child then
      declare
         Limit, Charged : aliased Unsigned_64 := 999;
         Handle, Other : aliased Unsigned_32 := 0;
         procedure Query (Slot : Unsigned_64; Code : Unsigned_32;
                          Expected : Unsigned_64 := 0) is
         begin
            Limit := 999; Charged := 999;
            Check (Client.Query_Accounting (Slot, Limit'Access, Charged'Access) = Code,
              "query status");
            Check (Charged = Expected and then Limit = (if Code = 0 then 12288 else 0),
              "query values");
         end Query;
      begin
         Query (15, 3);
         Check (Client.Create (15, 8192, Handle'Access) = 0, "create");
         Query (15, 0, 8192);
         Query (16, 3);
         Query (17, 1);
         Check (Client.Create (15, 8192, Other'Access) = 3 and Other = 0, "quota denial");
         Query (15, 0, 8192);
         Check (Client.Create (15, 4096, Other'Access) = 0 and Other /= Handle,
           "smaller recovery");
         Check (Client.Close (15, Handle) = 0, "close");
         Query (15, 0, 12288);
         Check (Client.Create (16, 4096, Other'Access) = 0, "second account");
         Query (16, 0, 4096);
         Query (15, 0, 12288);
         Msg.tag := (16#F030#, 0, 0, 0);
         Msg.tag := capCall (15, Msg, Wait_Forever);
         Check (Msg.tag = (16#F030#, 0, 0, 0), "done reply");
         debugPrint ("TEST: PASS native accounting adapter child transport completed (NO GPU)" & ASCII.LF);
      end;
      Ignore := syscall (SYSCALL_EXIT); loop null; end loop;
   end if;
   debugPrint ("native accounting adapter: production client and handler, separate processes (NO GPU)" & ASCII.LF);
   B.Configure_Client_Budgets (Object, 12288, OK); Check (OK, "budget");
   Cpio.init (Archive, To_Address (16#0000_5000_0000_0000#),
     getInfo (SYSINFO_RAMDISK_SIZE), OK); Check (OK, "initrd");
   Cpio.fileView (Archive, Cpio.findFile (Archive, "devmgr.svc"), ELF, ELF_Size, OK);
   Check (OK, "ELF");
   Check (syscall (SYSCALL_SPAWN, Unsigned_64 (To_Integer (ELF)), ELF_Size,
     5, 0, Child, PID) = Child, "spawn");
   for Slot in Unsigned_64 range 15 .. 17 loop
      Check (syscall (SYSCALL_POLICY_MINT_CAPABILITY, Child, 1, PID,
        16#4947# + Slot - 15, 3, Slot) /= Unsigned_64'Last, "endpoint");
   end loop;
   Check (syscall (SYSCALL_RESUME, Child) = 0, "resume");
   Deadline := syscall (SYSCALL_GETTIME) + 60_000;
   loop
      Poll_Service_Request (From, Msg, Found);
      if not Found then
         Check (syscall (SYSCALL_GETTIME) < Deadline, "deadline");
      else
         Check (Unsigned_64 (From) = Child, "kernel sender");
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
            B.Query_Accounting (Object, Unsigned_64 (From), Msg.authorityTag,
              Msg.tag.label, Msg.tag.length, Msg.tag.flags, Msg.tag.reserved,
              B.Words (Msg.words), Words);
         else
            Check (Msg.tag.label = B.Label, "label");
            B.Handle (Object, Unsigned_64 (From), Msg.authorityTag,
              Msg.tag.label, Msg.tag.length, Msg.tag.flags, Msg.tag.reserved,
              B.Words (Msg.words), Words, Ticket);
         end if;
         if Ticket /= 0 then
            Check (saveReplyCap (58) = 1, "saved reply");
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
            Check (replyCap (58, Response) = 1, "deferred delivery");
         else
            Check (reply (From, Response) = 1, "delivery");
         end if;
      end if;
   end loop;
   -- Retain supervisor-owned backing until the runner observes child success.
   loop Ignore := syscall (SYSCALL_SLEEP, 100); end loop;
end Accounting_Check;
