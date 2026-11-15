pragma Ada_2022;
with Interfaces;
with Platform_Pool_ABI;
with ACPI_Firmware_Data_Layouts;
with AML_Identity;
generic
   type Authority is private;
   No_Authority : Authority;
   with function Inspect_Pool (Held : Authority) return Boolean;
   with function Arena_Is_Exclusive (First, Limit : Interfaces.Unsigned_64) return Boolean;
   with procedure Map_Chunk (Held : Authority; Physical, Virtual, Pages : Interfaces.Unsigned_64;
      Read_Only : Boolean; Status : out Interfaces.Unsigned_64;
      Why : out Platform_Pool_ABI.Refusal_Record);
   with procedure Read_Clock (Now : out Interfaces.Unsigned_64; Available : out Boolean);
   with function May_Access (For_Write : Boolean) return Boolean;
   with procedure Transfer (Virtual : Interfaces.Unsigned_64;
      Width : ACPI_Firmware_Data_Layouts.T.Access_Width; For_Write : Boolean;
      Input : Interfaces.Unsigned_64; Output : out Interfaces.Unsigned_64; Completed : out Boolean);
   Maximum_Regions, Maximum_Mappings : Positive;
package ACPI_Platform_Region_Cache with SPARK_Mode is
   package Layouts renames ACPI_Firmware_Data_Layouts;
   package ABI renames Platform_Pool_ABI;
   package T renames Layouts.T;
   subtype Word is Interfaces.Unsigned_64;
   No_Finite_Deadline : constant Word := Word'Last;
   type Phase is (Fresh, Initializing, Ready, Failed, Closed, Blocked);
   type Outcome is (Mapped, Reused, Not_Ready, Reentered, Invalid_Region,
      Invalid_Deadline, Clock_Unavailable, Clock_Reversed, Deadline_Expired,
      Region_Capacity, Mapping_Capacity, Arena_Exhausted, Kernel_Refused,
      Mapping_Unknown, Internal_Limit);
   type Handle is private;
   No_Region : constant Handle;
   type Map_Result is record
      Status : Outcome := Not_Ready;
      Region : Handle;
      Raw_Status : Word := 0;
      Why : ABI.Refusal_Record := [others => 0];
   end record;
   type History_State is (History_Unavailable, Previously_Unseen, Previously_Seen);
   type Source is record
      Name : String (1 .. 4) := "????";
      Instance, Namespace_Revision, Node : Word := 0;
   end record;
   type Region_Phase is (Preparing, Accessible, Refused, Data_Uncertain, Data_Fault);
   type Profile is record
      Available : Boolean := False;
      Origin : Source;
      Geometry : Layouts.Region;
      Stage : Region_Phase := Preparing;
      Result : Outcome := Not_Ready;
      Raw_Status : Word := 0;
      Why : ABI.Refusal_Record := [others => 0];
      History : History_State := History_Unavailable;
      Map_Calls, Confirmed_New_Pages, Read_Attempts, Write_Attempts : Word := 0;
      First_Operation, Last_Operation, Repeats : Word := 0;
      Needs_Note : Boolean := False;
   end record;
   type Data_Status is (Denied, Completed, Failed_Completed, Indeterminate);
   type Data_Result is record
      Status : Data_Status := Denied;
      Value, Operation : Word := 0;
   end record;
   type Observation is record
      Present : Boolean := False;
      Region : Handle;
      Offset : Word := 0;
      Width : T.Access_Width := T.Byte_Access;
      For_Write : Boolean := False;
      Input, Operation : Word := 0;
   end record;
   type Statistics is record
      Stage : Phase := Fresh;
      Working : Boolean := False;
      Regions, Mapped_Regions, Refused_Regions, Mapping_Slots, Published_Mappings : Natural := 0;
      Reserved_Bytes, Fenced_Bytes, Map_Calls : Word := 0;
      Uncertain : Boolean := False;
   end record;
   type Context is limited private;
   function Observe (State : Context) return Statistics;
   function Admitting (State : Context) return Boolean;
   function Pending (State : Context) return Boolean;
   function Uncertain (State : Context) return Observation;
   procedure Initialize (State : in out Context; Held : Authority;
      Arena_First, Arena_Limit : Word; Accepted : out Boolean);
   -- One lifetime attempt. Inspect_Pool must establish actual current pool
   -- tag/read-write/no-Grant rights; arena predicate is required, never assumed.
   -- Bind the same inspected immutable handle to every kernel mapping call.
   procedure Map_Pool_Region_For_AML (State : in out Context; Origin : Source;
      Geometry : Layouts.Region; Deadline_Microseconds : Word; Result : out Map_Result;
      History : History_State := History_Unavailable);
   -- P2SB/chipset discovery precedes _INI independently in devmgr. AML-computed
   -- bases select that existing pool; every rounded page is kernel-admitted.
   -- Adjacent members are allowed: only known-no-effect OUTSIDE_POOL refusals
   -- are split, in order, down to pages. Other refusals never retry.
   -- Any failed whole-region attempt fences ALL new VA spans/chunks, even
   -- successful prefixes. No holder unmap exists; no release/reset is invented.
   procedure Describe (State : Context; Item : Handle;
      Geometry : out Layouts.Region; Accepted : out Boolean);
   procedure Access_Region (State : in out Context; Item : Handle; Offset : Word;
      Width : T.Access_Width; For_Write : Boolean; Input : Word; Result : out Data_Result);
   -- Exact declared extent/alignment/width and current local transaction
   -- admission precede one scalar transfer. May_Access is a trusted lease/owner
   -- check, nonblocking/nonreentrant; it must not reject solely because this
   -- cache's Working flag is already set. No syscall per datum is introduced.
   procedure Close_Admission (State : in out Context);
   function Read_Profile (State : Context; Ordinal : Positive) return Profile;
   procedure Note_Drained (State : in out Context; Ordinal : Positive; Accepted : out Boolean);
   -- Caller drains logs only outside ALL owner/lease/pending/uncertain IO.
   -- Profile contains copied facts, never a mapping pointer or new authority.
private
   use type Word;
   subtype Region_Count is Natural range 0 .. Maximum_Regions;
   subtype Region_Index is Region_Count range 1 .. Region_Count'Last;
   subtype Mapping_Count is Natural range 0 .. Maximum_Mappings;
   subtype Mapping_Index is Mapping_Count range 1 .. Mapping_Count'Last;
   type Handle is record
      Owner : AML_Identity.Identity := AML_Identity.No_Identity;
      Slot : Region_Count := 0;
   end record;
   No_Region : constant Handle := (others => <>);
   type Region_Entry is record
      Info : Profile;
   end record;
   type Mapping_Phase is (Reserved, Complete, Fenced);
   type Mapping_Entry is record
      Stage : Mapping_Phase := Reserved;
      First_Page, Pages, Virtual, Confirmed : Word := 0;
   end record;
   type Region_Array is array (Region_Index) of Region_Entry;
   type Mapping_Array is array (Mapping_Index) of Mapping_Entry;
   type Order_Array is array (Mapping_Index) of Mapping_Count;
   type Context is limited record
      Stage : Phase := Fresh;
      Working, Seen_Clock : Boolean := False;
      Identity : AML_Identity.Identity := AML_Identity.No_Identity;
      Held : Authority := No_Authority;
      First, Limit, Cursor, Last_Clock, Calls, Operation : Word := 0;
      Regions : Region_Array;
      Maps : Mapping_Array;
      Order : Order_Array := [others => 0];
      Region_Last : Region_Count := 0;
      Map_Last, Published, Recent : Mapping_Count := 0;
      Captured : Observation;
   end record;
end ACPI_Platform_Region_Cache;
