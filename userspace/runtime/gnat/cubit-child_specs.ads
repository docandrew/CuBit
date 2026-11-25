------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Start_Child (SUP-001, docs/otp-supervision-study.md): the child spec a
--  supervisor hands procmgr, and procmgr's typed receipt.
--
--  @description
--  procmgr is the only process creator. A program holding a start-children
--  scope (its manifest's may_launch list; for device classes, the device
--  manager) asks procmgr to start a child with Start_Operation. procmgr
--  resolves the program, checks its typed parameters against the child's
--  manifest-declared parameter record, spawns it with the caller as its
--  supervisor (the kernel then sends the caller the child's
--  EVENT_CHILD_EXIT), records the supervision edge, and answers at once
--  with a receipt: the child's identity, or a typed Refusal. It never waits
--  for the child's readiness. When a supervisor ends, for any reason,
--  procmgr stops (reaps) its children.
--
--  Request: tag.label = Start_Operation, length = Request_Words;
--    words (0) = a grant reference (CuBit.Grant_References wire) to the
--                encoded spec, lent read-only to procmgr;
--    words (1) = the spec's byte count (1 .. Maximum_Spec_Bytes).
--  Reply REPLY_OK: words (0) = the child's identity (KERN-003); when the
--    spec asked for a sink (Write_Only_Sink) and the request accepted a
--    capability, the reply carries it (REPLY_WITH_CAPABILITY): an endpoint
--    to the child, Write and Grant, badged Sink_Badge.
--  Reply REPLY_ERR: words (0) = Refusal'Enum_Rep, words (1) = for
--    Launch_Refused, the CuBit.Launch_Arguments.Launch_Failure code.
--
--  Held children. A spec with Start Held has its child created, given its
--  table and parameters, and recorded, but not run: the supervisor first
--  sets up what the child needs before its first instruction (the device
--  manager admits the child's GPIO sink at the GPIO driver), then sends
--  Release_Operation: words (0) = the child's identity, words (1) =
--  Release_Decision'Enum_Rep. Run starts it; Withdraw ends it unrun.
--  Reply REPLY_OK: words (0) = the identity; REPLY_ERR: words (0) =
--  Release_Refusal'Enum_Rep. Only the child's supervisor may release it,
--  once.
--
--  A device-class spec (Source = Device_Class) names a class launch of
--  the startup plan, (driver-class "<ACPI_ID>" (Device_Class_Launch
--  facts => [...] policy => <record value>)): the plan writes the POLICY
--  fields of the child's parameter record, the supervisor supplies the
--  FACT fields the launch names (Facts), and procmgr type-checks both and
--  the merged record. Neither side can set the other's fields.
--
--  A binding names a startup-table label of the child, the badge its
--  endpoint carries, and its Provider: the process whose endpoint it is
--  (0: the role's registered provider). The device manager names the
--  provider when several serve one role (one I2C controller driver per
--  controller), so the badge reaches the controller it was admitted for.
--
--  Spec layout (little-endian), and nothing after the last dependency:
--     0  u32  Magic ("CSPC")         4  u16  Spec_Version (3)
--     6  u8   Restart_Kind           7  u8   Child_Kind
--     8  u8   Supervisor_Choice      9  u8   reserved (0)
--    10  u8   Parameter_Source      11  u8   fact count
--    12  u8   binding count         13  u8   dependency count
--    14  u8   Start_Mode            15  u8   Sink_Kind
--    16  u32  stop grace, milliseconds
--    20  u16  restart intensity     22  u8   Stop_Stage
--    23  u8   reserved (0)
--    24  u32  intensity window, milliseconds
--    28  u16  program name length   30  u16  value length (the given
--                                             value, or the class ID)
--    32  u64  sink badge (0 when Sink_Kind is No_Sink)
--    40  the program name, then the value or class ID
--    per fact:       u8 name length, name, i64 value
--    per binding:    u8 label length, label, u64 badge, u64 provider
--    per dependency: u64 process identity
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.Child_Specs with Pure, SPARK_Mode is

   --  procmgr's operation (kernel ipc_labels.ads: 16#0100# .. 16#01FF# are
   --  process operations).
   Start_Operation : constant := 16#0114#;
   Request_Words   : constant := 2;
   Reply_Words     : constant := 2;
   Release_Operation : constant := 16#0115#;
   Release_Words     : constant := 2;

   Magic        : constant Unsigned_32 := 16#43505343#;   --  "CSPC"
   Spec_Version : constant := 3;
   Header_Bytes : constant := 40;

   --  Named ceilings of one spec (it is one request, not a store).
   Maximum_Name_Bytes   : constant := 255;     --  CuBit.Launch_Arguments' names
   Maximum_Value_Bytes  : constant := 1024;    --  a startup plan's Value_Text
   Maximum_Class_Bytes  : constant := 8;       --  an ACPI _HID / _CID
   Maximum_Field_Bytes  : constant := 64;      --  a record field or startup label
   Maximum_Facts        : constant := 16;
   Maximum_Bindings     : constant := 8;
   Maximum_Dependencies : constant := 16;
   Fact_Overhead    : constant := 1 + 8;
   Binding_Overhead : constant := 1 + 8 + 8;
   Maximum_Spec_Bytes : constant :=
     Header_Bytes + Maximum_Name_Bytes + Maximum_Value_Bytes
     + Maximum_Facts * (Fact_Overhead + Maximum_Field_Bytes)
     + Maximum_Bindings * (Binding_Overhead + Maximum_Field_Bytes)
     + Maximum_Dependencies * 8;

   --  OTP's child restart types. Temporary: never restarted. Transient:
   --  restarted only after an abnormal end. Permanent: always restarted,
   --  within its intensity (Intensity restarts per Window_Ms); past it the
   --  failure goes to the supervisor.
   type Restart_Kind is (Temporary, Transient, Permanent);
   for Restart_Kind use (Temporary => 1, Transient => 2, Permanent => 3);

   --  Worker: does work. Supervisor: starts and supervises children itself.
   type Child_Kind is (Worker, Supervisor);
   for Child_Kind use (Worker => 1, Supervisor => 2);

   --  The root stop's stage (PWR-003, CuBit.System_Stops): Apps first, then
   --  Services (children before their supervisors), then the Power_Tail,
   --  which a power-off keeps running until the firmware's last step (the
   --  ACPI service and the drivers its AML uses). Services is 0, so a spec
   --  that names none is a service.
   type Stop_Stage is (Services, Apps, Power_Tail);
   for Stop_Stage use (Services => 0, Apps => 1, Power_Tail => 2);

   --  Who supervises the child. Only the caller in this version.
   type Supervisor_Choice is (Caller);
   for Supervisor_Choice use (Caller => 1);

   --  Where the child's typed parameters come from. No_Parameters: none
   --  (a child that declares a parameter record is then refused). Given:
   --  the caller's value, CCL source of the child's record type. Device_
   --  Class: the plan's class launch block plus the caller's facts.
   type Parameter_Source is (No_Parameters, Given, Device_Class);
   for Parameter_Source use (No_Parameters => 0, Given => 1, Device_Class => 2);

   --  Run_At_Once: the child runs when procmgr answers. Held: it waits,
   --  created but unrun, for its supervisor's Release.
   type Start_Mode is (Run_At_Once, Held);
   for Start_Mode use (Run_At_Once => 1, Held => 2);

   --  Whether the receipt carries a sink: an endpoint to the child, Write
   --  and Grant, badged Sink_Badge (the device manager's GPIO input badge,
   --  which the GPIO driver reads with Describe). Device manager only.
   type Sink_Kind is (No_Sink, Write_Only_Sink);
   for Sink_Kind use (No_Sink => 0, Write_Only_Sink => 1);

   --  How long a graceful stop may take before the supervisor may end the
   --  child (explicit, no default: a spec always says).
   Maximum_Grace_Ms : constant := 600_000;
   subtype Grace_Ms is Unsigned_32 range 1 .. Maximum_Grace_Ms;

   Maximum_Intensity : constant := 1_000;
   subtype Intensity_Count is Unsigned_16 range 0 .. Maximum_Intensity;
   Maximum_Window_Ms : constant := 3_600_000;
   subtype Window_Ms is Unsigned_32 range 0 .. Maximum_Window_Ms;

   subtype Name_Length  is Natural range 0 .. Maximum_Name_Bytes;
   subtype Value_Length is Natural range 0 .. Maximum_Value_Bytes;
   subtype Field_Length is Natural range 0 .. Maximum_Field_Bytes;
   subtype Fact_Count       is Natural range 0 .. Maximum_Facts;
   subtype Binding_Count    is Natural range 0 .. Maximum_Bindings;
   subtype Dependency_Count is Natural range 0 .. Maximum_Dependencies;
   subtype Spec_Length is Natural range 0 .. Maximum_Spec_Bytes;

   type Byte_Array is array (Positive range <>) of Unsigned_8;

   type Field_Text is record
      Length : Field_Length := 0;
      Data   : String (1 .. Maximum_Field_Bytes) := [others => ' '];
   end record;

   --  One fact the supervisor knows of the device: a field of the child's
   --  parameter record and its integer value (range types are integers).
   type Fact is record
      Field : Field_Text;
      Value : Integer_64 := 0;
   end record;

   --  A startup-table label of the child (a manifest service request) whose
   --  endpoint procmgr badges with Badge: only for the device manager, which
   --  derives badges from the device it admitted (an I2C target, a GPIO
   --  input).
   --  Provider: the process whose endpoint is badged (0: the role's
   --  registered provider).
   No_Provider : constant Unsigned_64 := 0;
   type Binding is record
      Label    : Field_Text;
      Badge    : Unsigned_64 := 0;
      Provider : Unsigned_64 := No_Provider;
   end record;

   type Fact_Array is array (1 .. Maximum_Facts) of Fact;
   type Binding_Array is array (1 .. Maximum_Bindings) of Binding;
   type Dependency_Array is array (1 .. Maximum_Dependencies) of Unsigned_64;

   type Spec is record
      Restart    : Restart_Kind := Temporary;
      --  When the root stop stops it (PWR-003): a declared fact, never
      --  inferred.
      Stage      : Stop_Stage := Services;
      Kind       : Child_Kind := Worker;
      Supervisor : Supervisor_Choice := Caller;
      Stop_Grace : Grace_Ms := Grace_Ms'First;
      Intensity  : Intensity_Count := 0;
      Window     : Window_Ms := 0;
      Name_Bytes : Name_Length := 0;
      Name       : String (1 .. Maximum_Name_Bytes) := [others => ' '];
      Source     : Parameter_Source := No_Parameters;
      Value_Bytes : Value_Length := 0;
      Value      : String (1 .. Maximum_Value_Bytes) := [others => ' '];
      Facts      : Fact_Count := 0;
      Fact_List  : Fact_Array := [others => (others => <>)];
      Bindings   : Binding_Count := 0;
      Binding_List : Binding_Array := [others => (others => <>)];
      Dependencies : Dependency_Count := 0;
      Dependency_List : Dependency_Array := [others => 0];
      Start      : Start_Mode := Run_At_Once;
      Sink       : Sink_Kind := No_Sink;
      Sink_Badge : Unsigned_64 := 0;
   end record;

   ---------------------------------------------------------------------------
   --  Character classes.
   ---------------------------------------------------------------------------
   --  A program name: printable, no space (as in launch tables).
   function Name_Character (C : Character) return Boolean is
     (C in '!' .. '~');
   --  CCL source of a value: printable, space, tab and line breaks.
   function Value_Character (C : Character) return Boolean is
     (C in ' ' .. '~' | ASCII.HT | ASCII.LF | ASCII.CR);
   --  An ACPI _HID/_CID: upper-case letters, digits, underscore.
   function Class_Character (C : Character) return Boolean is
     (C in 'A' .. 'Z' | '0' .. '9' | '_');
   --  A CCL record field name: a letter, then letters, digits, underscores.
   function Field_Character (C : Character; First : Boolean) return Boolean is
     (C in 'a' .. 'z' | 'A' .. 'Z' or else (not First and then C in '0' .. '9' | '_'));
   --  A startup label (CuBit.Startup_Tables names).
   function Label_Character (C : Character) return Boolean is
     (C in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' | '.');

   function Valid_Field (Item : Field_Text) return Boolean is
     (Item.Length >= 1
      and then (for all K in 1 .. Item.Length => Field_Character (Item.Data (K), K = 1)));
   function Valid_Label (Item : Field_Text) return Boolean is
     (Item.Length >= 1
      and then (for all K in 1 .. Item.Length => Label_Character (Item.Data (K))));

   function Same (A, B : Field_Text) return Boolean is
     (A.Length = B.Length and then A.Data (1 .. A.Length) = B.Data (1 .. B.Length));

   function Facts_Distinct (S : Spec) return Boolean is
     (for all I in 1 .. S.Facts =>
        (for all J in I + 1 .. S.Facts => not Same (S.Fact_List (I).Field, S.Fact_List (J).Field)));
   function Bindings_Distinct (S : Spec) return Boolean is
     (for all I in 1 .. S.Bindings =>
        (for all J in I + 1 .. S.Bindings => not Same (S.Binding_List (I).Label, S.Binding_List (J).Label)));

   --  Everything a decoded spec satisfies (and Encode requires).
   function Valid (S : Spec) return Boolean is
     (S.Name_Bytes >= 1
      and then (for all K in 1 .. S.Name_Bytes => Name_Character (S.Name (K)))
      --  Restarts need an intensity; a temporary child has none.
      and then (if S.Restart = Temporary then S.Intensity = 0 and then S.Window = 0
                else S.Intensity >= 1 and then S.Window >= 1)
      and then (case S.Source is
                  when No_Parameters => S.Value_Bytes = 0 and then S.Facts = 0,
                  when Given =>
                    S.Value_Bytes >= 1 and then S.Facts = 0
                    and then (for all K in 1 .. S.Value_Bytes => Value_Character (S.Value (K))),
                  when Device_Class =>
                    S.Value_Bytes in 1 .. Maximum_Class_Bytes
                    and then (for all K in 1 .. S.Value_Bytes => Class_Character (S.Value (K))))
      and then (for all I in 1 .. S.Facts => Valid_Field (S.Fact_List (I).Field))
      and then Facts_Distinct (S)
      and then (for all I in 1 .. S.Bindings =>
                  Valid_Label (S.Binding_List (I).Label) and then S.Binding_List (I).Badge /= 0)
      and then Bindings_Distinct (S)
      and then (for all I in 1 .. S.Dependencies => S.Dependency_List (I) /= 0)
      --  A sink has a badge; no sink, none.
      and then (S.Sink = No_Sink) = (S.Sink_Badge = 0));

   --  The encoded length of a valid spec.
   function Encoded_Length (S : Spec) return Spec_Length
     with Pre => S.Name_Bytes <= Maximum_Name_Bytes;

   procedure Encode (S : Spec; Bytes : out Byte_Array; Length : out Spec_Length)
     with Pre => Valid (S) and then Bytes'First = 1 and then Bytes'Length = Maximum_Spec_Bytes;

   --  Too_Short: shorter than the header and a name. Bad_Enumeration: an
   --  enumeration byte names no literal. Bad_Number: a grace,
   --  intensity or window out of range. Bad_Reserved: a reserved field is
   --  not zero. Bad_Length: a length field past its ceiling, or the bytes
   --  end early or late. Bad_Text: a character outside its class, a
   --  repeated fact or label, a zero badge or dependency, or another rule
   --  of Valid broken. Bad_Count: a count past its ceiling.
   type Decode_Status is
     (Decoded, Too_Short, Bad_Magic, Bad_Version, Bad_Enumeration, Bad_Number,
      Bad_Reserved, Bad_Length, Bad_Text, Bad_Count);

   procedure Decode (Bytes : Byte_Array; S : out Spec; Status : out Decode_Status)
     with Pre  => Bytes'First = 1 and then Bytes'Length <= Maximum_Spec_Bytes,
          Post => (if Status = Decoded then Valid (S));

   ---------------------------------------------------------------------------
   --  procmgr's answer.
   ---------------------------------------------------------------------------
   --  Sink_Not_Granted (below): a sink or a held start asked by a caller
   --  other than the device manager, or a sink procmgr could not give.
   --  Malformed_Request: the request's words or frame. Grant_Unavailable:
   --  the spec's grant could not be read. Malformed_Spec: the spec does not
   --  decode (word 1: the Decode_Status position). Not_Granted: the
   --  caller's start-children scope does not cover the program.
   --  Class_Not_Granted: a class launch from a caller that may not make
   --  one, or for a program whose manifest does not drive the class.
   --  No_Class_Block: the startup plan has no (driver-class ...) for the
   --  class. Facts_Mismatch: the facts given are not exactly the facts the
   --  block declares. Parameters_Too_Long: policy and facts merged do not
   --  fit one value. Bindings_Not_Granted: badge bindings from a caller
   --  that may not give them. Dependency_Unknown: a dependency is not a
   --  running child of the same supervisor. Supervision_Capacity: procmgr's
   --  supervision store could not grow. Launch_Refused: the launch itself
   --  was refused (word 1: the Launch_Failure code).
   type Refusal is
     (Malformed_Request, Grant_Unavailable, Malformed_Spec, Not_Granted,
      Class_Not_Granted, No_Class_Block, Facts_Mismatch, Parameters_Too_Long,
      Bindings_Not_Granted, Dependency_Unknown, Supervision_Capacity,
      Launch_Refused, Sink_Not_Granted);
   for Refusal use
     (Malformed_Request => 1, Grant_Unavailable => 2, Malformed_Spec => 3,
      Not_Granted => 4, Class_Not_Granted => 5, No_Class_Block => 6,
      Facts_Mismatch => 7, Parameters_Too_Long => 8, Bindings_Not_Granted => 9,
      Dependency_Unknown => 10, Supervision_Capacity => 11, Launch_Refused => 12,
      Sink_Not_Granted => 13);

   function Is_Refusal (Word : Unsigned_64) return Boolean is
     (Word in Refusal'Enum_Rep (Refusal'First) .. Refusal'Enum_Rep (Refusal'Last));
   function Refusal_Of (Word : Unsigned_64) return Refusal
     with Pre => Is_Refusal (Word);

   ---------------------------------------------------------------------------
   --  Releasing a held child.
   ---------------------------------------------------------------------------
   type Release_Decision is (Run, Withdraw);
   for Release_Decision use (Run => 1, Withdraw => 2);
   --  Malformed_Release: the request's words or frame. Not_Supervised: the
   --  caller is not the child's supervisor (or no such child). Not_Held:
   --  it was not started Held, or was released already. Resume_Failed: the
   --  kernel refused to start it (it is ended).
   type Release_Refusal is (Malformed_Release, Not_Supervised, Not_Held, Resume_Failed);
   for Release_Refusal use
     (Malformed_Release => 1, Not_Supervised => 2, Not_Held => 3, Resume_Failed => 4);

   function Is_Decision (Word : Unsigned_64) return Boolean is
     (Word in Release_Decision'Enum_Rep (Run) .. Release_Decision'Enum_Rep (Withdraw));
   function Decision_Of (Word : Unsigned_64) return Release_Decision is
     (if Word = Release_Decision'Enum_Rep (Run) then Run else Withdraw)
     with Pre => Is_Decision (Word);
   function Is_Release_Refusal (Word : Unsigned_64) return Boolean is
     (Word in Release_Refusal'Enum_Rep (Malformed_Release) .. Release_Refusal'Enum_Rep (Resume_Failed));
   function Release_Refusal_Of (Word : Unsigned_64) return Release_Refusal is
     (case Word is
        when 2 => Not_Supervised, when 3 => Not_Held, when 4 => Resume_Failed,
        when others => Malformed_Release)
     with Pre => Is_Release_Refusal (Word);
   function Release_Refusal_Name (Why : Release_Refusal) return String is
     (case Why is
        when Malformed_Release => "MALFORMED_RELEASE",
        when Not_Supervised    => "NOT_SUPERVISED",
        when Not_Held          => "NOT_HELD",
        when Resume_Failed     => "RESUME_FAILED");

   --  Stable log names (native 'Image of an enumeration is its position).
   function Refusal_Name (Why : Refusal) return String;
   function Status_Name (Status : Decode_Status) return String;
   function Restart_Name (Kind : Restart_Kind) return String is
     (case Kind is
        when Temporary => "temporary",
        when Transient => "transient",
        when Permanent => "permanent");
end CuBit.Child_Specs;
