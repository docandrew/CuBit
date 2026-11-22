pragma Ada_2022;
with Interfaces;
with ACPI_FADT;
with ACPI_Backend_Bindings;
generic
   with package Bindings is new ACPI_Backend_Bindings (<>);
   type Hardware_State is limited private;
   with function Describe_Window
     (Hardware : Hardware_State; ID : Bindings.W.Window_ID) return Bindings.W.Window_Result;
   with function Select_Access
     (Hardware : Hardware_State; ID : Bindings.W.Window_ID; Space : Bindings.P.Address_Space;
      Address : Interfaces.Unsigned_64; Width : Bindings.T.Access_Width; For_Write : Boolean)
      return Bindings.Access_View;
   -- Independent retained-handle/cache-class fact, never firmware or an IPC
   -- Boolean. Real FACS atomics require coherent write-back RAM, not MMIO.
   with function Coherent_RAM (ID : Bindings.W.Window_ID) return Boolean;
   with procedure Read_Byte
     (Virtual : Interfaces.Unsigned_64; Value : out Interfaces.Unsigned_8; Completed : out Boolean);
package ACPI_Backend_FACS_Binding with SPARK_Mode is
   subtype Offset is Natural range 0 .. 63;
   type Outcome is (Not_Attempted, Ready, Already_Consumed, Reentered,
     Hardware_Reduced, No_FACS, Misaligned_FACS, Address_Overflow,
     Window_Unavailable, Wrong_Space, Rights_Insufficient, Incoherent_Memory,
     Prefix_Outside_Window, Read_Refused, Invalid_Header, Declared_Outside_Window);
   type Report is record
      Status : Outcome := Not_Attempted;
      Header_Bytes : Natural range 0 .. 64 := 0;
      Failed_Offset : Offset := 0;
      Declared_Length : Interfaces.Unsigned_32 := 0;
      Version : Interfaces.Unsigned_8 := 0;
   end record;
   type Context is limited private;
   function Observe (State : Context) return Report;
   -- Trusted startup only, with structurally admitted FADT and one retained
   -- binding owner. Prefer a present nonzero X_FACS before any admission gate;
   -- a denied preferred pointer never falls back to a different legacy FACS.
   -- One-shot; callbacks return normally and cannot retarget/revoke backing.
   procedure Prepare
     (State : in out Context; Hardware : Hardware_State; FADT : ACPI_FADT.Description;
      Window : Bindings.W.Window_ID; Result : out Report);
   -- Borrow a live DWORD view for one immediate trusted operation. Rechecks
   -- the same held ID on every use; no cached mapping or public address input.
   -- Never disable/revoke backing while the Global Lock owner may hold it.
   function Select_Word
     (State : Context; Hardware : Hardware_State; For_Write : Boolean)
      return Bindings.Access_View;
private
   type Phase is (Fresh, Preparing, Finished);
   type Context is limited record
      Stage : Phase := Fresh;
      Info : Report;
      Window : Bindings.W.Window_ID := Bindings.W.No_Window;
      Physical : Interfaces.Unsigned_64 := 0;
   end record;
end ACPI_Backend_FACS_Binding;
