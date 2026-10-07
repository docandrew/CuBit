------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Kernel_ABI; use CuBit.Kernel_ABI;
with CuBit.Kernel_Calls;
with CuBit.Libc_ABI; use CuBit.Libc_ABI;
with CuBit.Libc_Imports; use CuBit.Libc_Imports;
with CuBit.Launch_Arguments;
with CuBit.Launch_Grants;
with CuBit.Path_Names;
with CuBit.Child_Exits;
with CuBit.Child_Table;
with CuBit.Libc_Child_Outlets;
with CuBit.Outlet_Rings;

package body CuBit.Libc_Process is

   use type Interfaces.C.int;
   use type Interfaces.C.size_t;
   use type Interfaces.C.unsigned;
   use type Interfaces.C.long;
   use type System.Address;

   package LA renames CuBit.Launch_Arguments;
   package LG renames CuBit.Launch_Grants;
   package PN renames CuBit.Path_Names;
   package CO renames CuBit.Libc_Child_Outlets;
   package OR_T renames CuBit.Outlet_Rings;

   NUL : constant Character := Character'Val (0);

   --  The working directory (src/cubit/file.c until it is Ada): its name
   --  and whether one was chosen (only then is it passed to a child).
   Working_Directory_Bytes : constant := 4_096;     --  PATH_MAX
   function Working_Directory
     (Name : System.Address; Chosen : access int) return size_t
   with Import, Convention => C, External_Name => "__cubit_cwd_name";

   --  A launch request: the program name, the block, the delegated places,
   --  then the child's outlet rings, lent read-only to procmgr once.
   Request_Bytes : constant :=
     LA.Maximum_Name_Bytes + LA.Maximum_Block_Bytes + LG.Maximum_Bytes
     + OR_T.Maximum_Bytes;
   Request_Pages : constant := (Request_Bytes + Page_Bytes - 1) / Page_Bytes;
   Request : System.Address := System.Null_Address;
   Request_Grant : Unsigned_64 := 0;

   --  A wakeup that brought no exit event (other traffic is for other parts
   --  of the program): pause this long before looking again.
   Idle_Pause_Microseconds : constant := 1_000;
   --  While a child's diagnostics are watched, copy them at least this often.
   --  (Get_Time's milliseconds: the deadline's clock.)
   Forward_Pause_Milliseconds : constant := 10;

   --  Children started and not yet waited for, the encoder, and the
   --  request buffer, all under Child_Lock.
   Child_Lock : aliased Lock_Word := 0;
   --  Zero-filled (.bss): no children, and the encoder is started before
   --  each use.
   Children : CuBit.Child_Table.Table;
   pragma Suppress_Initialization (Children);

   Encoder : LA.Builder;
   pragma Suppress_Initialization (Encoder);

   --  The places this program's launcher delegated to it (a Launch_Grants
   --  region, Places (1 .. Places_Length)), asked of procmgr once: they do
   --  not change while it runs. Under Child_Lock.
   Places_Pages : constant := (LG.Maximum_Bytes + Page_Bytes - 1) / Page_Bytes;
   Places : LG.Bytes (1 .. LG.Maximum_Bytes);
   pragma Suppress_Initialization (Places);
   Places_Length : LG.Byte_Count := 0;
   Places_Known : Boolean := False;


   --  The length of the C string at Text, or Bound + 1 if it has no NUL in
   --  its first Bound + 1 bytes (only those are read).
   function C_Length (Text : System.Address; Bound : Natural) return Natural;

   function C_Length (Text : System.Address; Bound : Natural) return Natural is
      Bytes : constant String (1 .. Bound + 1) with Import, Address => Text;
   begin
      for K in Bytes'Range loop
         if Bytes (K) = NUL then
            return K - 1;
         end if;
      end loop;
      return Bound + 1;
   end C_Length;

   procedure Set_Errno (Value : int);

   procedure Set_Errno (Value : int) is
   begin
      Errno_Location.all := Value;
   end Set_Errno;

   --  Lend procmgr the request buffer, once. 0, or an errno value.
   function Lend_Request return int;

   function Lend_Request return int is
      Area : System.Address;
      Slot, Generation : Unsigned_64;
      Ignore : int;
   begin
      if Request /= System.Null_Address then
         return 0;
      end if;
      Area := mmap (System.Null_Address, Request_Pages * Page_Bytes,
                    int (PROT_READ + PROT_WRITE), int (MAP_PRIVATE + MAP_ANONYMOUS),
                    -1, 0);
      if Area = MAP_FAILED then
         return ENOMEM;
      end if;
      Slot := CuBit.Kernel_Calls.Call
        (Create_Shared_Memory_Grant_Via_Capability, Process_Manager_Slot,
         Unsigned_64 (To_Integer (Area)), Request_Pages, Grant_Read_Only);
      if Slot = Failed then
         Ignore := munmap (Area, Request_Pages * Page_Bytes);
         return EPERM;                    --  no process-manager authority
      end if;
      Generation := CuBit.Kernel_Calls.Call
        (Get_Owned_Shared_Memory_Grant_Generation, Slot);
      if Generation = Failed or else Generation = 0 then
         return EPERM;
      end if;
      Request_Grant := Shift_Left (Generation, Generation_Shift) or Slot;
      Request := Area;
      return 0;
   end Lend_Request;

   --  Ask procmgr for the delegated places, once (OP_DELEGATED_PLACES). 0,
   --  or an errno value.
   function Fetch_Places return int;

   function Fetch_Places return int is
      Area : System.Address;
      Slot, Generation : Unsigned_64;
      Ignore : int;
   begin
      if Places_Known then
         return 0;
      end if;
      Area := mmap (System.Null_Address, Places_Pages * Page_Bytes,
                    int (PROT_READ + PROT_WRITE), int (MAP_PRIVATE + MAP_ANONYMOUS),
                    -1, 0);
      if Area = MAP_FAILED then
         return ENOMEM;
      end if;
      Slot := CuBit.Kernel_Calls.Call
        (Create_Shared_Memory_Grant_Via_Capability, Process_Manager_Slot,
         Unsigned_64 (To_Integer (Area)), Places_Pages, Grant_Read_Write);
      if Slot = Failed then
         Ignore := munmap (Area, Places_Pages * Page_Bytes);
         return EPERM;
      end if;
      Generation := CuBit.Kernel_Calls.Call
        (Get_Owned_Shared_Memory_Grant_Generation, Slot);
      if Generation = Failed or else Generation = 0 then
         return EPERM;
      end if;
      declare
         M : aliased Message :=
           (Label => LG.Places_Operation, Length => LG.Places_Request_Words,
            Words => [Shift_Left (Generation, Generation_Shift) or Slot, 0, 0, 0],
            others => <>);
         Label : constant Unsigned_32 := Unsigned_32 (CuBit.Kernel_Calls.Call
           (Call_Via_Endpoint_Capability, Process_Manager_Slot,
            Unsigned_64 (To_Integer (M'Address))) and 16#FFFF_FFFF#);
         Answer : constant LG.Bytes (1 .. LG.Maximum_Bytes)
         with Import, Address => Area;
      begin
         if Label /= Reply_OK or else M.Words (0) > LG.Maximum_Bytes then
            return EPERM;
         end if;
         Places_Length := Natural (M.Words (0));
         Places (1 .. Places_Length) := Answer (1 .. Places_Length);
         if Places_Length > 0 and then not LG.Valid (Places (1 .. Places_Length)) then
            Places_Length := 0;
            return EPERM;
         end if;
      end;
      --  The grant stays lent: procmgr returns its acquisition after the
      --  answer, and nothing else uses the pages.
      Places_Known := True;
      return 0;
   end Fetch_Places;

   --  Add each string of the NUL-terminated vector at List (none if null).
   procedure Add_Strings (List : System.Address; Environment : Boolean;
                          Accepted : out Boolean);

   procedure Add_Strings (List : System.Address; Environment : Boolean;
                          Accepted : out Boolean) is
      type Address_Array is array (0 .. LA.Maximum_Strings) of System.Address;
      Vector : constant Address_Array with Import, Address => List;
   begin
      Accepted := True;
      if List = System.Null_Address then
         return;
      end if;
      for K in Vector'Range loop
         exit when Vector (K) = System.Null_Address;
         declare
            Length : constant Natural :=
              C_Length (Vector (K), LA.Maximum_Block_Bytes);
         begin
            if Length > LA.Maximum_Block_Bytes then
               Accepted := False;
               return;
            end if;
            declare
               Text : constant String (1 .. Length)
               with Import, Address => Vector (K);
            begin
               if Environment then
                  LA.Add_Environment (Encoder, Text, Accepted);
               else
                  LA.Add_Argument (Encoder, Text, Accepted);
               end if;
            end;
         end;
         if not Accepted then
            return;
         end if;
         if K = Vector'Last then          --  more strings than a block holds
            Accepted := False;
            return;
         end if;
      end loop;
   end Add_Strings;

   function Launch_Error (Failure : Unsigned_64) return int;

   function Launch_Error (Failure : Unsigned_64) return int is
   begin
      for Reason in LA.Launch_Failure loop
         if Failure = LA.Launch_Failure'Enum_Rep (Reason) then
            return (case Reason is
                      when LA.Malformed_Request  => EINVAL,
                      when LA.Grant_Unavailable  => ENOMEM,
                      when LA.Arguments_Rejected => E2BIG,
                      when LA.Spawn_Failed       => ENOENT,
                      when LA.Not_Granted        => EACCES);
         end if;
      end loop;
      return EIO;
   end Launch_Error;

   --  Start the program named at Path with the vectors; 0 or an errno
   --  value. Child_Lock is held.
   function Spawn_Locked
     (Result : System.Address; Name : String;
      Arguments, Environment : System.Address) return int;

   function Spawn_Locked
     (Result : System.Address; Name : String;
      Arguments, Environment : System.Address) return int
   is
      Accepted : Boolean;
      Length : LA.Present_Length;
      Rings : OR_T.Table;
      Slot : CO.Slot_Choice;
      Ring_Table : OR_T.Bytes (1 .. OR_T.Maximum_Bytes);
      Ring_Length : OR_T.Table_Length;
      Error : int := Lend_Request;
   begin
      if Error = 0 then
         Error := Fetch_Places;
      end if;
      if Error /= 0 then
         return Error;
      end if;
      LA.Start (Encoder);
      Add_Strings (Arguments, Environment => False, Accepted => Accepted);
      if Accepted then
         Add_Strings (Environment, Environment => True, Accepted => Accepted);
      end if;
      if Accepted then
         declare
            Directory : String (1 .. Working_Directory_Bytes);
            Chosen : aliased int := 0;
            Bytes : constant size_t :=
              Working_Directory (Directory'Address, Chosen'Access);
         begin
            if Chosen /= 0 and then Bytes in 1 .. Directory'Length then
               LA.Add_Directory
                 (Encoder, Directory (1 .. Natural (Bytes)), Accepted);
            end if;
         end;
      end if;
      if Accepted then
         LA.Finish (Encoder, Length, Accepted);  --  validated by Finish
      end if;
      if not Accepted then
         return E2BIG;
      end if;

      --  The child's diagnostics come back through this program (D3).
      CO.Prepare (Name, Rings, Slot);
      OR_T.Encode (Rings, Ring_Table, Ring_Length);
      declare
         Buffer : String (1 .. Name'Length + Length + Places_Length + Ring_Length)
         with Import, Address => Request;
         Block : constant LA.Block (1 .. Length) := Encoder.Data (1 .. Length);
         M : aliased Message :=
           (Label => LA.Launch_Operation, Length => LA.Launch_Request_Words,
            Words => [Request_Grant, Unsigned_64 (Name'Length),
                      Unsigned_64 (Length), LG.Request_Word (0, Places_Length, Ring_Length)],
            others => <>);
         Label : Unsigned_32;
      begin
         Buffer (1 .. Name'Length) := Name;
         for K in Block'Range loop
            Buffer (Name'Length + K) := Character'Val (Block (K));
         end loop;
         for K in 1 .. Places_Length loop
            Buffer (Name'Length + Length + K) := Character'Val (Places (K));
         end loop;
         for K in 1 .. Ring_Length loop
            Buffer (Name'Length + Length + Places_Length + K) :=
              Character'Val (Ring_Table (K));
         end loop;
         --  Registered under the lock waiting drains exit events under, so
         --  an early exit event cannot be missed.
         Label := Unsigned_32 (CuBit.Kernel_Calls.Call
           (Call_Via_Endpoint_Capability, Process_Manager_Slot,
            Unsigned_64 (To_Integer (M'Address))) and 16#FFFF_FFFF#);
         if Label = Reply_OK
           and then M.Words (0) in CuBit.Child_Table.Process_Number
         then
            CuBit.Child_Table.Started (Children, M.Words (0), M.Words (1));
            CO.Started (Slot, int (M.Words (0)));
            if Result /= System.Null_Address then
               declare
                  PID : int with Import, Address => Result;
               begin
                  PID := int (M.Words (0));
               end;
            end if;
            return 0;
         elsif Label = Reply_Error then
            CO.Abandon (Slot);
            return Launch_Error (M.Words (0));
         else
            CO.Abandon (Slot);
            return EPERM;
         end if;
      end;
   end Spawn_Locked;

   function Spawn
     (Result : System.Address; Path : System.Address;
      File_Actions : System.Address; Attributes : System.Address;
      Arguments, Environment : System.Address) return int
   is
      pragma Unreferenced (Attributes);  --  no signals, groups or classes
      Start : constant System.Address := Path;
      Length : Natural;
      Error : int;
   begin
      if File_Actions /= System.Null_Address then
         declare
            Actions : constant System.Address
            with Import,
                 Address => File_Actions + File_Actions_Pointer_Offset;
         begin
            if Actions /= System.Null_Address then
               return ENOTSUP;
            end if;
         end;
      end if;
      if Path = System.Null_Address then
         return ENOENT;
      end if;
      Length := C_Length (Start, PN.Maximum_Path_Bytes);
      if Length = 0 then
         return ENOENT;
      elsif Length > PN.Maximum_Path_Bytes then
         return ENAMETOOLONG;
      end if;
      declare
         Text : constant String (1 .. Length) with Import, Address => Start;
         Resolved : PN.Name;
         Resolved_Length : PN.Name_Length := 0;
         Status : PN.Resolution := PN.Resolved;
         First : Positive := 1;
         On_System : constant String := PN.System_Volume & PN.Separator;
      begin
         if (for some C of Text => C = PN.Separator) then
            declare
               Directory : String (1 .. Working_Directory_Bytes);
               Chosen : aliased int := 0;
               Bytes : constant size_t :=
                 Working_Directory (Directory'Address, Chosen'Access);
            begin
               if Chosen /= 0 and then Bytes in 1 .. Directory'Length then
                  PN.Resolve (Directory (1 .. Natural (Bytes)), Text,
                              PN.Maximum_Name_Bytes, Resolved, Resolved_Length,
                              Status);
               else
                  PN.Resolve (On_System, Text, PN.Maximum_Name_Bytes,
                              Resolved, Resolved_Length, Status);
               end if;
            end;
            case Status is
               when PN.Resolved => null;
               when PN.Empty_Path => return ENOENT;
               when PN.Too_Long => return ENAMETOOLONG;
               when PN.Invalid_Base => return EINVAL;
            end case;
            if Resolved_Length > On_System'Length
              and then Resolved (1 .. On_System'Length) = On_System
            then
               First := On_System'Length + 1;
            end if;
         else
            Resolved (1 .. Length) := Text;
            Resolved_Length := Length;
         end if;
         if Resolved_Length - First + 1 > LA.Maximum_Name_Bytes then
            return ENAMETOOLONG;
         end if;
         Lock (Child_Lock'Access);
         Error := Spawn_Locked
           (Result, Resolved (First .. Resolved_Length), Arguments, Environment);
         Unlock (Child_Lock'Access);
      end;
      return Error;
   end Spawn;

   function Spawn_By_Search
     (Result : System.Address; Path : System.Address;
      File_Actions : System.Address; Attributes : System.Address;
      Arguments, Environment : System.Address) return int
   is (Spawn (Result, Path, File_Actions, Attributes, Arguments,
                    Environment));

   --  Exits other threads received (Note_Event), until a waiter moves them
   --  into Children. Under Exit_Lock only, which nests inside Child_Lock.
   Pending_Capacity : constant := 32;
   Exit_Lock : aliased Lock_Word := 0;
   Pending : array (1 .. Pending_Capacity) of CuBit.Child_Exits.Report;
   pragma Suppress_Initialization (Pending);
   Pending_Count : Natural := 0;
   Pending_Lost : Natural := 0;

   procedure Note_Event (Item : System.Address) is
      M : constant CuBit.Kernel_ABI.Message with Import, Address => Item;
   begin
      if M.Label = CuBit.Child_Exits.Event_Label
        and then CuBit.Child_Exits.Valid (M.Length, M.Words (0), M.Words (1), M.Words (2))
      then
         Lock (Exit_Lock'Access);
         if Pending_Count < Pending_Capacity then
            Pending_Count := Pending_Count + 1;
            Pending (Pending_Count) := CuBit.Child_Exits.Decode
              (M.Words (0), M.Words (1), M.Words (2), M.Words (3));
         else
            Pending_Lost := Pending_Lost + 1;
         end if;
         Unlock (Exit_Lock'Access);
      end if;
   end Note_Event;

   --  Whether the stream dispatcher thread runs (it receives this process's
   --  messages and events; CuBit.Libc_Descriptors).
   function Dispatcher_Running return int
   with Import, Convention => C, External_Name => "__cubit_dispatcher_running";

   --  Move every queued exit event of our children into Children. Other
   --  events are dropped: nothing else in the libc consumes events.
   --  Child_Lock is held.
   procedure Drain_Events;

   procedure Drain_Events is
      M : aliased Message;
   begin
      Lock (Exit_Lock'Access);
      for K in 1 .. Pending_Count loop
         CuBit.Child_Table.Exited (Children, Pending (K));
      end loop;
      Pending_Count := 0;
      Unlock (Exit_Lock'Access);
      while CuBit.Kernel_Calls.Call
        (Receive_Event_Nonblocking, Unsigned_64 (To_Integer (M'Address)))
        = Event_Received
      loop
         if M.Label = CuBit.Child_Exits.Event_Label
           and then CuBit.Child_Exits.Valid
             (M.Length, M.Words (0), M.Words (1), M.Words (2))
         then
            CuBit.Child_Table.Exited
              (Children, CuBit.Child_Exits.Decode
                 (M.Words (0), M.Words (1), M.Words (2), M.Words (3)));
         end if;
      end loop;
   end Drain_Events;

   function Wait_With_Usage
     (Process : int; Status : System.Address; Options : int;
      Usage : System.Address) return int
   is
      Known_Options : constant int := WNOHANG + WUNTRACED + WCONTINUED;
      Found : int;
      Found_Status : CuBit.Child_Table.Wait_Status;
      Waiting, Ready, Forwarding : Boolean;
      Ignore : Unsigned_64;
   begin
      if (Interfaces.C.unsigned (Options)
          and not Interfaces.C.unsigned (Known_Options)) /= 0
      then
         Set_Errno (EINVAL);
         return -1;
      end if;
      if Usage /= System.Null_Address then
         declare
            Bytes : Storage_Array (1 .. Rusage_Bytes)
            with Import, Address => Usage;
         begin
            Bytes := [others => 0];      --  resource usage is not tracked
         end;
      end if;
      loop
         Lock (Child_Lock'Access);
         Drain_Events;
         CO.Forward_All;
         CuBit.Child_Table.Take (Children, Process, Found, Found_Status);
         if Found /= 0 then
            CO.Ended (Found);
         end if;
         Waiting := CuBit.Child_Table.Has_Child (Children, Process);
         Forwarding := CO.Any_Watched;
         Unlock (Child_Lock'Access);
         if Found /= 0 then
            if Status /= System.Null_Address then
               declare
                  Result : int with Import, Address => Status;
               begin
                  Result := Found_Status;
               end;
            end if;
            return Found;
         elsif not Waiting then
            Set_Errno (ECHILD);
            return -1;
         elsif (Interfaces.C.unsigned (Options)
                and Interfaces.C.unsigned (WNOHANG)) /= 0
         then
            return 0;
         end if;
         --  Woken by any traffic, not only events: when no exit event
         --  came, pause briefly rather than spin on someone else's.
         --  While children's diagnostics are watched, wake to copy them; and
         --  while the stream dispatcher runs, an exit it receives wakes no
         --  one (Note_Event), so look again soon.
         Ignore := CuBit.Kernel_Calls.Call
           (Wait_For_IPC_Or_Completion_Until_Monotonic_Millisecond,
            (if Forwarding or else Dispatcher_Running /= 0
             then CuBit.Kernel_Calls.Call (Get_Time) + Forward_Pause_Milliseconds
             else Forever));
         Lock (Child_Lock'Access);
         Drain_Events;
         Ready := Children.Ended_Count > 0;
         Unlock (Child_Lock'Access);
         if not Ready then
            Ignore := CuBit.Kernel_Calls.Call
              (Sleep_Until_Monotonic_Microsecond,
               CuBit.Kernel_Calls.Call (Read_Monotonic_Microseconds)
               + Idle_Pause_Microseconds);
         end if;
      end loop;
   end Wait_With_Usage;

   function Wait_For_Child (Process : int; Status : System.Address; Options : int)
     return int is (Wait_With_Usage (Process, Status, Options, System.Null_Address));

   function Run_Command (Command : System.Address) return int is
   begin
      --  POSIX: Run_Command(NULL) asks whether a command processor exists.
      if Command = System.Null_Address then
         return 0;
      end if;
      Set_Errno (ENOSYS);
      return -1;
   end Run_Command;

   function Open_Command_Pipe (Command, Mode : System.Address) return System.Address is
      pragma Unreferenced (Command, Mode);
   begin
      Set_Errno (ENOSYS);                --  there is no shell
      return System.Null_Address;
   end Open_Command_Pipe;

   procedure Debug_Write
     (Text : System.Address; Length : Interfaces.C.size_t)
   is
      Ignore : constant Unsigned_64 := CuBit.Kernel_Calls.Call
        (Write, Console, Unsigned_64 (To_Integer (Text)), Unsigned_64 (Length));
   begin
      null;
   end Debug_Write;

end CuBit.Libc_Process;
