------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Switching a running startup-plan service to another program: the first
--  piece of system generations (LIVE-001 services, SELF-001 4a, REMOTE-001
--  "remote staging and live switch").
--
--  @description
--  A system generation pins programs by content address in the storehouse
--  (@system/storehouse/executables/<sha256>). Activating a generation
--  whose pin for a service changed asks procmgr to run that service from
--  the new program: procmgr's Switch_Operation below. procmgr stops the
--  running instance (Stop, then ends it after the caller's grace), starts
--  the program as the same startup-plan entry (the same identity,
--  approvals, parameters, priority and supervision), and watches it for
--  the caller's health window. A program that does not start, ends, or
--  has not registered its service role when the window closes is ended
--  and the program the entry ran before is started again (Rolled_Back).
--  Every request ends: at most Answer_Bound_Ms after procmgr took it.
--
--  v1 is a cold switch: no state is handed over (LIVE-001's typed handover
--  comes later), so the service's clients see its endpoint end (their
--  calls end with Ended) and connect again to the new registration.
--
--  Who may: procmgr admits a switch only when all of these hold
--  (Authorize, proved below):
--    - the startup plan entry's restart contract is not reboot: (replace
--      transparent) or (replace graceful), a per-service policy exception
--      written in the trusted plan (REMOTE-001; CCL.Configurations);
--    - the requester stages programs (its launch table has the staging
--      directory, "@system/work/generations/", as for a staged app launch)
--      and holds a read scope covering the program it names (ccl-remote's
--      covers the storehouse, where generations pin programs);
--    - the device owner's "Allow app replacement" switch is on (Desktop);
--    - the program is admitted by signature policy (Signature_Admits): the
--      seam for AUTH-001's owner-signed generations, unsigned today;
--    - no other switch is in progress (one at a time);
--  and the program's manifest is byte for byte the image program's (every
--  .cubit.* section): same identity, requests, scopes and parameters, so
--  a switch never widens what the entry holds (Content_Admits).
--
--  Request (Switch_Operation, Request_Words words):
--    words (0) a grant reference (CuBit.Grant_References wire form) the
--              requester lends procmgr, readable: the entry's executable
--              name, then the program's name
--    words (1) the entry's name length (1 .. Maximum_Service_Name_Bytes):
--              its startup-plan executable, "acpi.svc"
--    words (2) the program's name length: 0 for the image's own program,
--              else 1 .. Maximum_Program_Bytes
--    words (3) the deadlines (Deadlines_Word): the stop grace
--              (Stop_Grace_Ms, low 32 bits): how long the running instance
--              has to obey Stop before procmgr ends it, 0 ends it at once
--              (a service that reads no control events); the health window
--              (Health_Window_Ms, high 32 bits)
--  A Graceful entry needs a nonzero grace (Malformed otherwise).
--  Reply OK: words (0) Outcome'Enum_Rep, words (1) Health_Failure'Enum_Rep,
--  words (2) the identity of the process the entry runs now (0: none).
--  Reply ERR: words (0) Refusal'Enum_Rep; nothing was stopped or started.
--
--  At next boot (Boot_Stage_Operation, the same request words; the
--  deadlines word is not used and must be 0): a generation whose changed
--  pin names an entry with (replace reboot), acpi.svc's, is not switched
--  live. Its activator asks procmgr to admit the program for the next boot
--  instead: every check above except the restart contract and the switch
--  in progress (Authorize_For_Boot), and the same manifest check. Reply OK
--  (words (0) = 0): admitted; the activator then makes the generation
--  current and procmgr takes the pin when it next starts the plan
--  (procmgr's Boot_Generations re-checks it then). Nothing is stopped or
--  started now. Reply ERR: words (0) Refusal'Enum_Rep.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.Service_Replacement with SPARK_Mode, Pure is

   Switch_Operation     : constant := 16#0112#;
   Boot_Stage_Operation : constant := 16#0113#;
   Request_Words        : constant := 4;

   --  The startup plan's executable names (CCL.Configurations.Executable_Text).
   Maximum_Service_Name_Bytes : constant := 64;
   --  A program's CuBit name (CuBit.Launch_Arguments.Maximum_Name_Bytes).
   Maximum_Program_Bytes : constant := 255;
   Maximum_Request_Bytes : constant := Maximum_Service_Name_Bytes + Maximum_Program_Bytes;

   --  Named ceilings on the caller's deadlines (milliseconds).
   Maximum_Stop_Grace_Ms : constant := 10_000;
   Minimum_Health_Window_Ms : constant := 100;
   Maximum_Health_Window_Ms : constant := 30_000;
   subtype Stop_Grace_Ms is Unsigned_64 range 0 .. Maximum_Stop_Grace_Ms;
   subtype Health_Window_Ms is Unsigned_64
     range Minimum_Health_Window_Ms .. Maximum_Health_Window_Ms;
   --  How long an instance procmgr ended has to be gone (the kernel ends
   --  it; this bounds a process stuck in retirement).
   End_Deadline_Ms : constant := 5_000;

   --  Everything procmgr may take before it answers: the stop grace and an
   --  end, the health window, then ending the new instance on failure.
   --  Starting a program is synchronous inside procmgr and not counted;
   --  Start_Allowance_Ms covers two starts (the program, and the rollback).
   Start_Allowance_Ms : constant := 10_000;
   function Answer_Bound_Ms (Grace : Stop_Grace_Ms; Health : Health_Window_Ms) return Unsigned_64 is
     (Grace + Health + 2 * End_Deadline_Ms + Start_Allowance_Ms);
   --  At most 65 s: the caller's own exchange deadline must exceed it.
   Maximum_Answer_Bound_Ms : constant :=
     Maximum_Stop_Grace_Ms + Maximum_Health_Window_Ms + 2 * End_Deadline_Ms + Start_Allowance_Ms;

   --  Why procmgr refused; nothing was stopped or started.
   type Refusal is
     (Malformed,            --  the request's shape, lengths or deadlines
      Grant_Unavailable,    --  the names could not be read
      Unknown_Service,      --  no startup-plan entry runs that executable
      Not_Replaceable,      --  the entry's restart contract is reboot
      Not_Granted,          --  the requester does not stage that program
      Switch_Off,           --  "Allow app replacement" is off
      Unsigned_Refused,     --  signature policy refused the program (AUTH-001)
      Busy,                 --  another switch, or startup, is in progress
      Image_Unreadable,     --  the image's program could not be read
      Program_Unreadable,   --  the program could not be read, or its sections
      Manifest_Changed);    --  its manifest differs from the image's
   for Refusal use
     (Malformed => 1, Grant_Unavailable => 2, Unknown_Service => 3, Not_Replaceable => 4,
      Not_Granted => 5, Switch_Off => 6, Unsigned_Refused => 7, Busy => 8,
      Image_Unreadable => 9, Program_Unreadable => 10, Manifest_Changed => 11);

   --  How an admitted switch ended. Switched: the program runs and passed
   --  its health window. Rolled_Back: it failed (Health_Failure says how);
   --  the program the entry ran before runs again. Stop_Failed: the running
   --  instance did not end; nothing started. Rollback_Failed: it failed and
   --  the program before did not start again (the device's log says why);
   --  nothing runs.
   type Outcome is (Switched, Rolled_Back, Stop_Failed, Rollback_Failed);
   for Outcome use (Switched => 1, Rolled_Back => 2, Stop_Failed => 3, Rollback_Failed => 4);

   type Health_Failure is
     (Healthy,
      Start_Refused,     --  procmgr could not start it (its log says why)
      Exited_Early,      --  it ended inside the health window
      Not_Registered);   --  its service role was not registered at the end
   for Health_Failure use
     (Healthy => 0, Start_Refused => 1, Exited_Early => 2, Not_Registered => 3);

   --  Stable names for logs (native enumeration images are positions).
   function Refusal_Name (Why : Refusal) return String is
     (case Why is
        when Malformed => "MALFORMED", when Grant_Unavailable => "GRANT_UNAVAILABLE",
        when Unknown_Service => "UNKNOWN_SERVICE", when Not_Replaceable => "NOT_REPLACEABLE",
        when Not_Granted => "NOT_GRANTED", when Switch_Off => "SWITCH_OFF",
        when Unsigned_Refused => "UNSIGNED_REFUSED", when Busy => "BUSY",
        when Image_Unreadable => "IMAGE_UNREADABLE", when Program_Unreadable => "PROGRAM_UNREADABLE",
        when Manifest_Changed => "MANIFEST_CHANGED");

   function Is_Outcome (Word : Unsigned_64) return Boolean is
     (Word in Outcome'Enum_Rep (Outcome'First) .. Outcome'Enum_Rep (Outcome'Last));
   function Outcome_Of (Word : Unsigned_64) return Outcome is
     (case Word is
        when Outcome'Enum_Rep (Switched)    => Switched,
        when Outcome'Enum_Rep (Rolled_Back) => Rolled_Back,
        when Outcome'Enum_Rep (Stop_Failed) => Stop_Failed,
        when others                         => Rollback_Failed)
   with Pre => Is_Outcome (Word);
   function Is_Refusal (Word : Unsigned_64) return Boolean is
     (Word in Refusal'Enum_Rep (Refusal'First) .. Refusal'Enum_Rep (Refusal'Last));
   function Refusal_Of (Word : Unsigned_64) return Refusal
   with Pre => Is_Refusal (Word),
        Post => Refusal'Enum_Rep (Refusal_Of'Result) = Word;
   function Health_Of (Word : Unsigned_64) return Health_Failure is
     (case Word is
        when Health_Failure'Enum_Rep (Start_Refused)  => Start_Refused,
        when Health_Failure'Enum_Rep (Exited_Early)   => Exited_Early,
        when Health_Failure'Enum_Rep (Not_Registered) => Not_Registered,
        when others                                   => Healthy);

   --  Both deadlines in one message word.
   Health_Shift : constant := 32;
   Half_Word : constant := 2 ** Health_Shift;
   function Deadlines_Word (Grace : Stop_Grace_Ms; Health : Health_Window_Ms) return Unsigned_64 is
     (Grace + Health * Half_Word);
   function Grace_Of (Word : Unsigned_64) return Unsigned_64 is (Word mod Half_Word);
   function Health_Window_Of (Word : Unsigned_64) return Unsigned_64 is (Word / Half_Word);

   --  The request's numbers, before any name is read.
   function Request_Valid
     (Service_Bytes, Program_Bytes, Grace, Health : Unsigned_64) return Boolean is
     (Service_Bytes in 1 .. Maximum_Service_Name_Bytes
      and then Program_Bytes <= Maximum_Program_Bytes
      and then Grace <= Maximum_Stop_Grace_Ms
      and then Health in Minimum_Health_Window_Ms .. Maximum_Health_Window_Ms);

   ---------------------------------------------------------------------------
   --  Signature policy: the seam for owner-signed generations (AUTH-001).
   --  Nothing is signed or checked today: every program is Unsigned, and
   --  an unsigned program is admitted only while the device owner's switch
   --  is on (development). With AUTH-001 a generation signed by the
   --  owner's token becomes Owner_Signed, and the policy may then require
   --  it (for example, for any switch requested remotely).
   ---------------------------------------------------------------------------
   type Signature_Verdict is (Unsigned, Owner_Signed, Signature_Invalid);
   function Signature_Admits (Verdict : Signature_Verdict; Switch_On : Boolean) return Boolean is
     (case Verdict is
        when Unsigned          => Switch_On,
        when Owner_Signed      => Switch_On,
        when Signature_Invalid => False);

   ---------------------------------------------------------------------------
   --  Admission
   ---------------------------------------------------------------------------
   type Authority_Facts is record
      Entry_Known       : Boolean := False;   --  a plan entry runs that executable
      Entry_Replaceable : Boolean := False;   --  its restart contract is not reboot
      Requester_Stages  : Boolean := False;   --  its launch table stages the program
      Switch_On         : Boolean := False;   --  "Allow app replacement"
      Signature         : Signature_Verdict := Unsigned;
      In_Progress       : Boolean := False;   --  a switch or startup runs
   end record;

   type Decision (Admitted : Boolean := False) is record
      case Admitted is
         when True  => null;
         when False => Why : Refusal := Malformed;
      end case;
   end record;

   --  Admitted exactly when every fact allows it; otherwise the first
   --  refusal in the order listed (an unknown entry before its policy,
   --  policy before the requester, the requester before the switch).
   function Authorize (Facts : Authority_Facts) return Decision
   with Post => Authorize'Result.Admitted =
                  (Facts.Entry_Known and then Facts.Entry_Replaceable
                   and then Facts.Requester_Stages
                   and then Signature_Admits (Facts.Signature, Facts.Switch_On)
                   and then not Facts.In_Progress)
                and then (if not Facts.Entry_Known
                          then not Authorize'Result.Admitted
                               and then Authorize'Result.Why = Unknown_Service)
                and then (if Facts.Entry_Known and then not Facts.Entry_Replaceable
                          then not Authorize'Result.Admitted
                               and then Authorize'Result.Why = Not_Replaceable)
                and then (if not Facts.Switch_On then not Authorize'Result.Admitted);

   --  Boot_Stage_Operation's numbers: the names as for a switch, no deadlines.
   function Stage_Request_Valid (Service_Bytes, Program_Bytes, Deadlines : Unsigned_64) return Boolean is
     (Service_Bytes in 1 .. Maximum_Service_Name_Bytes
      and then Program_Bytes in 1 .. Maximum_Program_Bytes
      and then Deadlines = 0);

   --  Admitted for the next boot exactly when Authorize would admit it but
   --  for the restart contract and a switch in progress: those decide only
   --  how a program is taken live, never whether it may run.
   function Authorize_For_Boot (Facts : Authority_Facts) return Decision
   with Post => Authorize_For_Boot'Result.Admitted =
                  (Facts.Entry_Known and then Facts.Requester_Stages
                   and then Facts.Switch_On
                   and then Signature_Admits (Facts.Signature, Facts.Switch_On))
                and then (if not Facts.Entry_Known
                          then not Authorize_For_Boot'Result.Admitted
                               and then Authorize_For_Boot'Result.Why = Unknown_Service)
                and then (if Facts.Entry_Known and then Facts.Requester_Stages
                            and then not Facts.Switch_On
                          then not Authorize_For_Boot'Result.Admitted
                               and then Authorize_For_Boot'Result.Why = Switch_Off);

   --  After Authorize: the program's content. Same_Manifest: every .cubit.*
   --  section of the program equals the image program's, byte for byte.
   function Content_Admits
     (Image_Readable, Program_Readable, Same_Manifest : Boolean) return Decision
   with Post => Content_Admits'Result.Admitted =
                  (Image_Readable and then Program_Readable and then Same_Manifest);

end CuBit.Service_Replacement;
