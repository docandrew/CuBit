pragma Ada_2022;
with System.Address_To_Access_Conversions;
with System.Storage_Elements;
with ACPI_Backend_Lock_Atomic;
package body ACPI_Backend_Global_Lock is
   package Atomic renames ACPI_Backend_Lock_Atomic;
   package Addresses is new System.Address_To_Access_Conversions (Atomic.Word);
   use type Interfaces.Unsigned_32, Bindings.P.Address_Space;
   -- No acquisition until both retained FACS and release path were prepared.
   -- A definite no-write failure remains retryable by the ownership engine;
   -- any unknown signal effect closes the engine's lifecycle instead.
   function Signal_Ready return Boolean is
     (Signaling.Observe (Signal_State).Status in
        Signaling.Prepared | Signaling.Signaled | Signaling.Not_Sent);
   function Pointer (View : Bindings.Access_View) return not null Addresses.Object_Pointer is
     (Addresses.To_Pointer (System.Storage_Elements.To_Address
        (System.Storage_Elements.Integer_Address (View.Virtual_Address))));
   package body Callbacks is
      procedure Read_Word (Word : out Interfaces.Unsigned_32; Available : out Boolean) is
         View : constant Bindings.Access_View := FACS.Select_Word (FACS_State, Hardware, False);
      begin
         Word := 0;
         Available := Signal_Ready and then View.Allowed and then View.Space = Bindings.P.Memory_Space;
         if Available then Word := Atomic.Read_Word (Pointer (View)); end if;
         Observe_Word (Word, Available);
      end Read_Word;
      procedure Compare_Exchange
        (Expected, Replacement : Interfaces.Unsigned_32;
         Observed : out Interfaces.Unsigned_32; Result : out Atomic_Outcome)
      is
         View : constant Bindings.Access_View := FACS.Select_Word (FACS_State, Hardware, True);
      begin
         Observed := 0; Result := Not_Attempted;
         if not Signal_Ready or else not View.Allowed or else View.Space /= Bindings.P.Memory_Space then
            Observe_CAS (Expected, Replacement, Observed, False, False, False);
            return;
         end if;
         Observed := Atomic.Compare_Exchange (Pointer (View), Expected, Replacement);
         Result := (if Observed = Expected then Exchanged else Not_Exchanged);
         Observe_CAS (Expected, Replacement, Observed, True, True, Result = Exchanged);
      end Compare_Exchange;
      procedure Signal_Firmware (Result : out Signal_Outcome) is
         Report : Signaling.Report;
      begin
         Signaling.Emit (Signal_State, Hardware, Report);
         Observe_Signal (Report);
         Result := (case Report.Status is
            when Signaling.Signaled => Complete,
            when Signaling.Not_Sent => Not_Sent,
            when others => Signal_Unknown);
      end Signal_Firmware;
   end Callbacks;
end ACPI_Backend_Global_Lock;
