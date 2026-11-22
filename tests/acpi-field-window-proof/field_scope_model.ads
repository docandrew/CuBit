pragma Ada_2022;
with Interfaces;
with ACPI_Global_Lock_Protocol;
package Field_Scope_Model with SPARK_Mode is
   package P renames ACPI_Global_Lock_Protocol;
   subtype Word is Interfaces.Unsigned_64;
   use type Word, P.Lock_Outcome;
   type Inputs is record
      Admission, IO_Available, Lock_Available, Acquire_Due, Release_Due : Boolean := False;
      IO_Deadline, Lock_Deadline : Word := 0;
      Acquire_Reply, Access_Reply, Release_Reply : P.Lock_Outcome := P.Not_Ready;
   end record;
   Configuration : Inputs;
   type Lease is limited record Prepared : Boolean := False; end record;
   function Admission return Boolean with Global => (Input => Configuration),
     Post => Admission'Result = Configuration.Admission;
   function Due (Item : Lease) return Boolean with Global => null,
     Post => Due'Result = Item.Prepared;
   procedure Inspect_Time (Deadline : out Word; Available : out Boolean)
     with Global => (Input => Configuration), Post => Deadline = Configuration.IO_Deadline
       and then Available = Configuration.IO_Available;
   procedure Window (Deadline : out Word; Budget : out P.Attempt_Budget; Available : out Boolean)
     with Global => (Input => Configuration), Post => Deadline = Configuration.Lock_Deadline
       and then Budget = P.Attempt_Budget'First and then Available = Configuration.Lock_Available;
   procedure Acquire (Item : in out Lease; Deadline : Word; Budget : P.Attempt_Budget; Result : out P.Lock_Outcome)
     with Global => (Input => Configuration),
       Pre => Deadline not in 0 | Word'Last,
       Post => Item.Prepared = Configuration.Acquire_Due and then Result = Configuration.Acquire_Reply;
   procedure Validate (Item : Lease; Result : out P.Lock_Outcome)
     with Global => (Input => Configuration), Post => Result = Configuration.Access_Reply;
   procedure Release (Item : in out Lease; Result : out P.Lock_Outcome)
     with Global => (Input => Configuration), Post => Item.Prepared = Configuration.Release_Due
       and then Result = Configuration.Release_Reply;
end Field_Scope_Model;
