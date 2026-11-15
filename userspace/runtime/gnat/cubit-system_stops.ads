------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The root stop (PWR-003 slice 1, docs/power-states.md): the power manager
--  asks procmgr (sysmgr) to stop the system, and procmgr asks the
--  filesystem to commit and devmgr to stop its drivers.
--
--  @description
--  Every call goes down the IPC graph: power.svc -> procmgr -> filesystem,
--  devmgr. Nobody calls back up; nobody waits without a deadline.
--
--  Stop_Apps_Operation and Stop_Operation (power.svc -> procmgr), the same
--  request. Stop_Apps_Operation stops the Apps stage only and answers
--  Stopped (no storage or driver stage): power.svc then has acpi.svc
--  prepare to sleep while every service and driver still runs. A later
--  Stop_Operation stops the rest. A Stop_Operation alone does all of it.
--  Only the process registered for
--  CuBit.Power_Requests.Service_Role (com.cubit.power, from the trusted
--  startup plan) may ask; anyone else is refused Not_Approved. Request,
--  Stop_Request_Words words:
--    Request_Word  the request id (never zero)
--    Final_Word    Final_Step'Enum_Rep: what the requester does after the
--                  stop. Power_Off keeps the Power_Tail running for the
--                  firmware step: the processes whose launch declared stop
--                  stage Power_Tail (CuBit.Child_Specs.Stop_Stage; a plan
--                  entry's (stop-stage power-tail), acpi.svc's), and
--                  devmgr's children it declared so when it started them
--                  (the drivers of the ACPI devices acpi.svc reported).
--                  Halt stops them last. The requester is always kept.
--    Grace_Word    Undeclared grace (low 32 bits): the stop grace of a
--                  process whose plan entry or Start_Child spec declares
--                  none; the storage deadline (high 32 bits): how long the
--                  filesystem's commit may take. Both Grace_Ms.
--    Driver_Word   devmgr's grace for its drivers (low 32 bits, Grace_Ms);
--                  the high 32 bits are 0.
--  procmgr answers at once only to refuse (Stop_Result Not_Approved,
--  Malformed, Already_Stopping, Unanswerable); an admitted request keeps
--  its reply and is answered Stopped when its stages ended. From the first
--  request on procmgr starts nothing (launches, Start_Child, switches and
--  restarts are refused or skipped). After a finished Stop_Apps_Operation
--  one Stop_Operation is admitted; anything else is Already_Stopping.
--  The stages, in order: Apps (programs started by programs), Services
--  (the startup plan's entries, in reverse plan order), the filesystem's
--  commit (Storage_Operation), Drivers that procmgr started for devmgr,
--  then devmgr's own children (Driver_Stop_Operation). Each process gets
--  Stop (CuBit.Control_Events), then its grace; past it, it is ended and
--  reported Forced. Outcomes are logged one line per process.
--  Reply, label Stop_Operation, Stop_Reply_Words words:
--    Reply_Request_Word  the request id
--    Result_Word         Stop_Result'Enum_Rep
--    Processes_Word      (Stopped) counts, 16 bits each from bit 0:
--                        stopped clean, exited failing, forced, not ended
--                        (Process_Count_Shift)
--    Stages_Word         (Stopped) Storage_Outcome (bits 0 .. 7), volumes
--                        committed (8 .. 15), volumes failed (16 .. 23),
--                        Driver_Outcome (24 .. 31), drivers stopped
--                        (32 .. 47), drivers forced (48 .. 63)
--
--  Storage_Operation (procmgr -> filesystem). Only the process registered
--  as DRIVER_PROCMGR. Request, Storage_Request_Words words: Request_Word.
--  The filesystem collects every client's dirty pages, commits, detaches
--  every writable volume (an ext3 journal is emptied and needs_recovery
--  cleared) and refuses writes from then on. It answers when done; procmgr
--  waits until the storage deadline. Reply, label Storage_Operation,
--  Storage_Reply_Words words: Reply_Request_Word, Storage_Status_Word
--  (Storage_Outcome'Enum_Rep), Volumes_Word (committed, bits 0 .. 15;
--  failed, 16 .. 31; read-only volumes passed over, 32 .. 47).
--
--  Driver_Stop_Operation (procmgr -> devmgr). Only devmgr's procmgr.
--  Request, Driver_Request_Words words: Request_Word, Driver_Grace_Word
--  (Grace_Ms), Keep_Tail_Word (Keep_Tail: its Power_Tail children stay;
--  Stop_Tail: they stop too). devmgr sends Stop to every child it started
--  except procmgr (and its Power_Tail when kept), answers at once, and ends
--  any still running at the grace. Reply, label
--  Driver_Stop_Operation, Driver_Reply_Words words: Reply_Request_Word,
--  Receipt_Word (Driver_Receipt'Enum_Rep), Asked_Word (children sent Stop).
--  Driver_Status_Operation (procmgr -> devmgr): Request_Word. Reply, label
--  Driver_Status_Operation, Driver_Reply_Words words: Reply_Request_Word,
--  Phase_Word (Driver_Phase'Enum_Rep), Driver_Counts_Word (stopped clean,
--  exited failing, forced, still running: 16 bits each from bit 0).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.System_Stops with Pure, SPARK_Mode is

   --  The power block of labels (CuBit.Power_Event_Exchange has 16#0270#
   --  .. 16#0272#, CuBit.Power_Requests 16#0273#).
   Stop_Operation          : constant Unsigned_32 := 16#0274#;
   Stop_Apps_Operation     : constant Unsigned_32 := 16#0275#;
   Driver_Stop_Operation   : constant Unsigned_32 := 16#0276#;
   Driver_Status_Operation : constant Unsigned_32 := 16#0277#;
   Storage_Operation       : constant Unsigned_32 := 16#0278#;

   --  CuBit.Child_Specs.Maximum_Grace_Ms.
   Maximum_Grace_Ms : constant := 600_000;
   subtype Grace_Ms is Unsigned_32 range 1 .. Maximum_Grace_Ms;

   Half_Word : constant := 2 ** 32;
   Low_Half  : constant Unsigned_64 := Half_Word - 1;

   Request_Word       : constant := 0;
   Reply_Request_Word : constant := 0;

   ---------------------------------------------------------------------------
   --  Stop_Operation
   ---------------------------------------------------------------------------
   Stop_Request_Words : constant := 4;
   Final_Word  : constant := 1;
   Grace_Word  : constant := 2;
   Driver_Word : constant := 3;

   Stop_Reply_Words : constant := 4;
   Result_Word      : constant := 1;
   Processes_Word   : constant := 2;
   Stages_Word      : constant := 3;

   type Final_Step is (Power_Off, Halt);
   for Final_Step use (Power_Off => 1, Halt => 2);

   type Stop_Result is
     (Stopped,           --  every stage ended; the counts say how
      Not_Approved,      --  not the registered power manager
      Malformed,         --  id zero, unknown final step, a grace out of range
      Already_Stopping,  --  a stop is in progress (or done)
      Unanswerable);     --  the reply could not be kept
   for Stop_Result use
     (Stopped          => 1,
      Not_Approved     => 2,
      Malformed        => 3,
      Already_Stopping => 4,
      Unanswerable     => 5);

   function Grace_Word_Of (Undeclared : Grace_Ms; Storage : Grace_Ms) return Unsigned_64 is
     (Unsigned_64 (Undeclared) + Unsigned_64 (Storage) * Half_Word);
   function Undeclared_Of (Word : Unsigned_64) return Unsigned_64 is (Word and Low_Half);
   function Storage_Of (Word : Unsigned_64) return Unsigned_64 is (Shift_Right (Word, 32));

   function Valid_Grace (Value : Unsigned_64) return Boolean is
     (Value in 1 .. Maximum_Grace_Ms);
   function Valid_Final (Word : Unsigned_64) return Boolean is
     (for some F in Final_Step => Final_Step'Enum_Rep (F) = Word);
   function To_Final (Word : Unsigned_64) return Final_Step is
     (if Word = Final_Step'Enum_Rep (Halt) then Halt else Power_Off)
   with Pre => Valid_Final (Word);

   --  A request procmgr can admit (who asks is checked separately).
   function Request_Valid (Length : Unsigned_64; W0, W1, W2, W3 : Unsigned_64) return Boolean is
     (Length = Stop_Request_Words and then W0 /= 0 and then Valid_Final (W1)
      and then Valid_Grace (Undeclared_Of (W2)) and then Valid_Grace (Storage_Of (W2))
      and then Valid_Grace (W3));

   --  16-bit fields of the counts words.
   Count_Field_Bits : constant := 16;
   Count_Ceiling    : constant := 2 ** Count_Field_Bits - 1;
   function Field (Value : Natural) return Unsigned_64 is
     (Unsigned_64 (Natural'Min (Value, Count_Ceiling)));
   function Field_Of (Word : Unsigned_64; Index : Natural) return Natural is
     (Natural (Shift_Right (Word, Count_Field_Bits * Index) and Count_Ceiling))
   with Pre => Index < 4;

   function Counts_Word (A, B, C, D : Natural) return Unsigned_64 is
     (Field (A) or Shift_Left (Field (B), Count_Field_Bits) or Shift_Left (Field (C), 2 * Count_Field_Bits)
      or Shift_Left (Field (D), 3 * Count_Field_Bits));

   ---------------------------------------------------------------------------
   --  Storage_Operation
   ---------------------------------------------------------------------------
   Storage_Request_Words : constant := 1;
   Storage_Reply_Words   : constant := 3;
   Storage_Status_Word   : constant := 1;
   Volumes_Word          : constant := 2;

   type Storage_Outcome is
     (Not_Asked,          --  (procmgr's record) the stage never ran
      Committed,          --  every writable volume committed and detached
      Partly_Committed,   --  some volume failed; it is logged by name
      Nothing_Written,    --  no writable volume
      Storage_Refused,    --  not procmgr, or malformed
      Storage_Timed_Out); --  (procmgr's record) no answer by the deadline
   for Storage_Outcome use
     (Not_Asked         => 0,
      Committed         => 1,
      Partly_Committed  => 2,
      Nothing_Written   => 3,
      Storage_Refused   => 4,
      Storage_Timed_Out => 5);

   function Valid_Storage (Word : Unsigned_64) return Boolean is
     (for some S in Storage_Outcome => Storage_Outcome'Enum_Rep (S) = Word);

   ---------------------------------------------------------------------------
   --  Driver_Stop_Operation and Driver_Status_Operation
   ---------------------------------------------------------------------------
   Driver_Request_Words : constant := 3;
   Driver_Status_Words  : constant := 1;
   Driver_Grace_Word    : constant := 1;
   Keep_Tail_Word       : constant := 2;
   Stop_Tail            : constant Unsigned_64 := 0;
   Keep_Tail            : constant Unsigned_64 := 1;
   Driver_Reply_Words   : constant := 3;
   Receipt_Word         : constant := 1;
   Asked_Word           : constant := 2;
   Phase_Word           : constant := 1;
   Driver_Counts_Word   : constant := 2;

   type Driver_Receipt is (Accepted, Driver_Not_Approved, Driver_Malformed, Driver_Already_Stopping);
   for Driver_Receipt use
     (Accepted => 1, Driver_Not_Approved => 2, Driver_Malformed => 3, Driver_Already_Stopping => 4);

   type Driver_Phase is (Not_Asked, Stopping, Finished);
   for Driver_Phase use (Not_Asked => 0, Stopping => 1, Finished => 2);

   type Driver_Outcome is (Not_Asked, Drivers_Stopped, Drivers_Refused, Drivers_Timed_Out);
   for Driver_Outcome use
     (Not_Asked => 0, Drivers_Stopped => 1, Drivers_Refused => 2, Drivers_Timed_Out => 3);

   function Valid_Phase (Word : Unsigned_64) return Boolean is
     (for some P in Driver_Phase => Driver_Phase'Enum_Rep (P) = Word);

   --  Stages_Word of a Stopped reply.
   Storage_Byte_Shift     : constant := 0;
   Committed_Byte_Shift   : constant := 8;
   Failed_Byte_Shift      : constant := 16;
   Driver_Byte_Shift      : constant := 24;
   Drivers_Stopped_Shift  : constant := 32;
   Drivers_Forced_Shift   : constant := 48;
   Byte_Ceiling           : constant := 255;

   function Stages_Word_Of
     (Storage : Storage_Outcome; Committed, Failed : Natural;
      Drivers : Driver_Outcome; Drivers_Stopped, Drivers_Forced : Natural) return Unsigned_64 is
     (Shift_Left (Unsigned_64 (Storage_Outcome'Enum_Rep (Storage)), Storage_Byte_Shift)
      or Shift_Left (Unsigned_64 (Natural'Min (Committed, Byte_Ceiling)), Committed_Byte_Shift)
      or Shift_Left (Unsigned_64 (Natural'Min (Failed, Byte_Ceiling)), Failed_Byte_Shift)
      or Shift_Left (Unsigned_64 (Driver_Outcome'Enum_Rep (Drivers)), Driver_Byte_Shift)
      or Shift_Left (Field (Drivers_Stopped), Drivers_Stopped_Shift)
      or Shift_Left (Field (Drivers_Forced), Drivers_Forced_Shift));

   function Name (R : Stop_Result) return String is
     (case R is
        when Stopped          => "STOPPED",
        when Not_Approved     => "NOT_APPROVED",
        when Malformed        => "MALFORMED",
        when Already_Stopping => "ALREADY_STOPPING",
        when Unanswerable     => "UNANSWERABLE");

   function Name (S : Storage_Outcome) return String is
     (case S is
        when Not_Asked         => "NOT_ASKED",
        when Committed         => "COMMITTED",
        when Partly_Committed  => "PARTLY_COMMITTED",
        when Nothing_Written   => "NOTHING_WRITTEN",
        when Storage_Refused   => "REFUSED",
        when Storage_Timed_Out => "TIMED_OUT");

   function Name (D : Driver_Outcome) return String is
     (case D is
        when Not_Asked         => "NOT_ASKED",
        when Drivers_Stopped   => "STOPPED",
        when Drivers_Refused   => "REFUSED",
        when Drivers_Timed_Out => "TIMED_OUT");

end CuBit.System_Stops;
