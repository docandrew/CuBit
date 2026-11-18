pragma Ada_2022;
with ACPI_Backend_Kernel_Handles;
with ACPI_Backend_Binding_Types;
with ACPI_Backend_Memory_Access;
with ACPI_Backend_Port_Access;
with ACPI_Platform_Power_Completion;
with CuBit.Handles.Power;
with Startup;
package body ACPI_Backend_Sleep_Entry_Native is
   package W renames ACPI_Backend_Windows;
   package T renames W.T;
   use type Interfaces.Unsigned_64, T.Access_Width, W.P.Address_Space;
   function View_Allowed (View : Catalog.Bindings.Access_View) return Boolean is (View.Allowed);
   procedure Transfer_View (View : Catalog.Bindings.Access_View;
      Width : T.Access_Width; For_Write : Boolean;
      Input : Interfaces.Unsigned_64; Output : out Interfaces.Unsigned_64;
      Status : out ACPI_Sleep_Entry.Completion) is
      Done : Boolean;
   begin
      Output := 0; Status := ACPI_Sleep_Entry.Not_Attempted;
      if not View.Allowed or else Width not in T.Byte_Access | T.Word_Access
        or else (if For_Write then Input > W.Width_Mask (Width) else Input /= 0)
      then return; end if;
      case View.Space is
         when W.P.Memory_Space =>
            ACPI_Backend_Memory_Access.Transfer (View.Virtual_Address, Width, For_Write, Input, Output, Done);
         when W.P.IO_Space =>
            ACPI_Backend_Port_Access.Transfer (View.Ports, View.Port, Width, For_Write, Input, Output, Done);
      end case;
      if not Done then
         Output := 0;
         Status := (if For_Write then ACPI_Sleep_Entry.Indeterminate else ACPI_Sleep_Entry.Not_Attempted);
      elsif not For_Write and then Output > W.Width_Mask (Width) then
         Output := 0;
      else Status := ACPI_Sleep_Entry.Completed;
      end if;
   end Transfer_View;
   procedure Read_Clock (Now : out Interfaces.Unsigned_64; Available : out Boolean) is
      Value : ACPI_Backend_Binding_Types.Tick;
   begin
      ACPI_Backend_Kernel_Handles.Read_Clock (Value, Available);
      Now := Interfaces.Unsigned_64 (Value);
   end Read_Clock;
   procedure Enter_Final (Type_A, Type_B : ACPI_Sleep_Entry.E.Sleep_Type;
      Status : out ACPI_Sleep_Entry.Completion) is
      package P renames ACPI_Platform_Power_Completion;
      Reply : constant CuBit.Handles.Power.Outcome := CuBit.Handles.Power.Enter_State
        (Startup.Power, P.ABI.S5, P.ABI.Sleep_Type (Type_A), P.ABI.Sleep_Type (Type_B));
   begin
      Status := P.Classify (if Reply.Answered then (Decoded => True, Reason => Reply.Why)
        else (Decoded => False));
   end Enter_Final;
end ACPI_Backend_Sleep_Entry_Native;
