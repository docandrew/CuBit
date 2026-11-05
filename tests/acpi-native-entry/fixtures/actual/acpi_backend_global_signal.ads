pragma Ada_2022;
with Interfaces;
with ACPI_FADT;
with ACPI_Backend_Bindings;
with ACPI_PM1_Global_Release;
generic
   with package Bindings is new ACPI_Backend_Bindings (<>);
   type Hardware_State is limited private;
   with function Select_Access
     (Hardware : Hardware_State; ID : Bindings.W.Window_ID; Space : Bindings.P.Address_Space;
      Address : Interfaces.Unsigned_64; Width : Bindings.T.Access_Width; For_Write : Boolean)
      return Bindings.Access_View;
   -- Immediate borrowed view, one exact Word access, no retry or retained VA.
   -- Fixed controller callbacks return normally and cannot reenter/revoke.
   with procedure Transfer_Word
     (View : Bindings.Access_View; For_Write : Boolean; Input : Interfaces.Unsigned_16;
      Output : out Interfaces.Unsigned_16; Status : out ACPI_PM1_Global_Release.Completion);
package ACPI_Backend_Global_Signal with SPARK_Mode is
   type Bank is (PM1A, PM1B);
   type Windows is array (Bank) of Bindings.W.Window_ID;
   type Outcome is (Not_Prepared, Prepared, Already_Consumed, Reentered,
     Register_Unavailable, Bad_Register, Window_Unavailable,
     Signaled, Not_Sent, Indeterminate);
   type Report is record
      Status : Outcome := Not_Prepared;
      At_Bank : Bank := PM1A;
      Banks, Reads, Writes : Natural range 0 .. 2 := 0;
      Extended_Fallback : Boolean := False;
   end record;
   type Context is limited private;
   function Observe (State : Context) return Report;
   -- Trusted startup only. Scope/handle facts come from the existing retained
   -- binding owner. No public endpoint may supply these windows or addresses.
   procedure Prepare
     (State : in out Context; Hardware : Hardware_State;
      FADT : ACPI_FADT.Description; Held : Windows; Result : out Report);
   -- Per-release callback under the Global Lock owner's retained lease.
   -- Owner alone handles invocation/idempotence and completion uncertainty.
   -- All bank reads precede any write. Zero writes permits Not_Sent; partial
   -- completion is Indeterminate. Never replay after Indeterminate.
   procedure Emit (State : in out Context; Hardware : Hardware_State; Result : out Report);
private
   type Phase is (Fresh, Ready, Failed, Uncertain);
   type Descriptor is record
      Present : Boolean := False;
      Window : Bindings.W.Window_ID := Bindings.W.No_Window;
      Space : Bindings.P.Address_Space := Bindings.P.Memory_Space;
      Address : Interfaces.Unsigned_64 := 0;
   end record;
   type Descriptors is array (Bank) of Descriptor;
   type Context is limited record
      Stage : Phase := Fresh;
      Items : Descriptors;
      Info : Report;
      Working : Boolean := False;
   end record;
end ACPI_Backend_Global_Signal;
