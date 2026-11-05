pragma Ada_2022;
with Interfaces;
with ACPI_Backend_Binding_Types;
-- Internal mapping adapter, never a wire address or capability admission API.
-- The caller already owns the exact kernel-validated window and an exclusive
-- device VA reservation. Retain both after ANY attempted mapping, even false.
-- Callbacks return normally, enforce the named authority and never reenter.
generic
   type Authority is private;
   No_Authority : Authority;
   with procedure Map_Chunk
     (Held : Authority; Physical, Virtual, Pages : Interfaces.Unsigned_64;
      Read_Only : Boolean; Completed : out Boolean);
   with procedure Read_Clock
     (Now : out ACPI_Backend_Binding_Types.Tick; Available : out Boolean);
   Maximum_Pages, Maximum_Per_Call : Positive;
   Physical_Limit, Virtual_Limit : Interfaces.Unsigned_64;
package ACPI_Backend_Mapping_Chunks with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   subtype Word is Interfaces.Unsigned_64;
   package B renames ACPI_Backend_Binding_Types;
   use type Word;
   Page_Bytes : constant Word := 4_096;
   Maximum_Calls : constant Positive :=
     Maximum_Pages / Maximum_Per_Call +
       (if Maximum_Pages mod Maximum_Per_Call = 0 then 0 else 1);
   subtype Call_Count is Natural range 0 .. Maximum_Calls;
   type Phase is (Fresh, Preparing, Mapping, Complete, Stopped);
   type Outcome is (Not_Attempted, Mapped, Invalid_Request, Missing_Authority,
     Invalid_Deadline, Clock_Unavailable, Clock_Reversed, Deadline_Expired,
     Mapping_Indeterminate, Attempt_Limit, Already_Attempted, Reentered);
   type Statistics is record
      Stage : Phase := Fresh;
      Working : Boolean := False;
      Attempts : Call_Count := 0;
      Confirmed_Pages : Word := 0;
   end record;
   type Context is limited private;
   function Observe (State : Context) return Statistics;
   procedure Map_Exact
     (State : in out Context; Held : Authority; Physical, Virtual, Pages : Word;
      Read_Only : Boolean; Deadline : B.Tick; Result : out Outcome)
     with Post =>
       (if Result in Reentered | Already_Attempted then
          Observe (State) = Observe (State)'Old
        else not Observe (State).Working and then
          Observe (State).Confirmed_Pages <= Pages and then
          Observe (State).Stage = (if Result = Mapped then Complete else Stopped) and then
          (if Result = Mapped then Observe (State).Confirmed_Pages = Pages))
       and then Result /= Attempt_Limit;
   -- Mapped means every exact chunk returned success before the final deadline
   -- sample. A false callback can already have mapped some/all of its chunk.
   -- Confirmed_Pages excludes that unknown chunk; it is not a cleanup extent.
   -- No reset, replay, automatic unmap or release, including after bad input.
private
   type Context is limited record
      Stats : Statistics;
      Seen_Clock : Boolean := False;
      Last_Clock : B.Tick := 0;
   end record;
end ACPI_Backend_Mapping_Chunks;
