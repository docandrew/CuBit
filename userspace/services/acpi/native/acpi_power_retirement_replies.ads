pragma Ada_2022;
with Interfaces;
with CuBit.Power_Event_Exchange;
package ACPI_Power_Retirement_Replies with Pure, SPARK_Mode is
   package E renames CuBit.Power_Event_Exchange;
   subtype Word is Interfaces.Unsigned_64;
   type Words is array (Natural range 0 .. 3) of Word;
   type Request is record
      Label : Interfaces.Unsigned_32 := 0;
      Length, Flags : Interfaces.Unsigned_8 := 0;
      Reserved : Interfaces.Unsigned_16 := 0;
      Authority : Word := 0;
      Data : Words := [others => 0];
   end record;
   type Response is record
      Handled : Boolean := False;
      Label : Interfaces.Unsigned_32 := 0;
      Length : Interfaces.Unsigned_8 := 0;
      Data : Words := [others => 0];
   end record;
   function Refuse (Input : Request; Count : Word; Source : E.Press_Source)
      return Response with Global => null;
   -- Terminal owner only: copies the known press facts, never keeps a reply,
   -- runs AML or infers the availability of a button/sleep register.
end ACPI_Power_Retirement_Replies;
