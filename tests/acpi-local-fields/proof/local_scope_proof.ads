pragma Ada_2022;
with Interfaces;
with ACPI_Global_Lock_Protocol;
with ACPI_Local_Field_Scope;
package Local_Scope_Proof with SPARK_Mode is
   package P renames ACPI_Global_Lock_Protocol;
   type Lease is limited record Prepared : Boolean := False; end record;
   function Admission return Boolean with Global => null;
   function Due (Item : Lease) return Boolean with Global => null;
   procedure Window (Deadline : out Interfaces.Unsigned_64; Budget : out P.Attempt_Budget;
      Available : out Boolean) with Global => null;
   procedure Acquire (Item : in out Lease; Deadline : Interfaces.Unsigned_64;
      Budget : P.Attempt_Budget; Result : out P.Lock_Outcome) with Global => null;
   procedure Validate (Item : Lease; Result : out P.Lock_Outcome) with Global => null;
   procedure Release (Item : in out Lease; Result : out P.Lock_Outcome) with Global => null;
   package Scope is new ACPI_Local_Field_Scope (Lease,Admission,Due,Window,Acquire,Validate,Release,4);
end Local_Scope_Proof;
