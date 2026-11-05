pragma Ada_2022;
with Interfaces; with ACPI_Global_Lock_Protocol;
-- Copied observations only, never an owner, FACS reader, timer or logger.
generic
   type Principal is private;
   No_Principal : Principal;
   type Scope_Tag is private;
   Allowed_Scope : Scope_Tag;
package ACPI_Lock_Trace with SPARK_Mode is
   package P renames ACPI_Global_Lock_Protocol;
   Maximum_Records : constant := 12;
   subtype Count is Natural range 0 .. Maximum_Records;
   subtype Index is Count range 1 .. Maximum_Records;
   subtype Word is Interfaces.Unsigned_64;
   type Signal_Result is (No_Signal, Signaled, Signal_Not_Sent, Signal_Unknown);
   type Snapshot is record
      Maintenance : Boolean := False;
      Request : P.Operation := P.Prepare;
      Reply_Valid : Boolean := False;
      Reply : P.Reply_Kind := P.Malformed;
      Outcome : P.Lock_Outcome := P.Not_Ready;
      Read_Present, Read_Available, CAS_Present : Boolean := False;
      Read_Word, Expected, Replacement, Observed : Interfaces.Unsigned_32 := 0;
      CAS_Known, CAS_Attempted, CAS_Exchanged : Boolean := False;
      CAS_Count : Word := 0;
      Signal : Signal_Result := No_Signal;
      Signal_Reads, Signal_Writes : Natural range 0 .. 2 := 0;
   end record;
   type Dump_Outcome is (Accepted, Already_Dumped, Unauthorized, Malformed);
   type Context is private;
   procedure Begin_Request (State : in out Context; Caller : Principal; Scope : Scope_Tag; Kind : P.Operation);
   procedure Finish_Request (State : in out Context; Reply : P.Reply);
   procedure Begin_Maintenance (State : in out Context);
   procedure Finish_Maintenance (State : in out Context; Outcome : P.Lock_Outcome);
   procedure Observe_Read (State : in out Context; Value : Interfaces.Unsigned_32; Available : Boolean);
   procedure Observe_CAS (State : in out Context; Expected,Replacement,Observed : Interfaces.Unsigned_32;
      Known,Attempted,Exchanged : Boolean);
   procedure Observe_Signal (State : in out Context; Result : Signal_Result; Reads,Writes : Natural);
   procedure Request_Dump (State : in out Context; Caller : Principal; Scope : Scope_Tag; Result : out Dump_Outcome);
   function Pending (State : Context) return Boolean;
   function Length (State : Context) return Count;
   function Dropped (State : Context) return Word;
   procedure Read (State : Context; Number : Index; Item : out Snapshot; Found : out Boolean);
   procedure Finish_Dump (State : in out Context);
private
   type Rows is array (Index) of Snapshot;
   type Context is record
      Owner, Caller : Principal := No_Principal;
      Active, Trusted_Request, Dumped, Dump_Pending, Finished : Boolean := False;
      Used : Count := 0;
      Next : Index := 1;
      Lost : Word := 0;
      Current : Snapshot;
      Items : Rows;
   end record;
end ACPI_Lock_Trace;
