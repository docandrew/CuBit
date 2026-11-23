pragma Ada_2022;
with ACPI_Service_Core;
with ACPI_Region_Initialization;
with ACPI_Region_Initialization.Descriptor_Tests;
with AML_Delays;
with AML_Execute;
with Ada.Text_IO;
procedure Route_Tests is
   package C is new ACPI_Service_Core
     (Perform_Delay => AML_Delays.Unavailable_Provider,
      Namespace_Node_Capacity => 16, Retained_Result_Capacity => 2);
   type Context is null record;
   function Ready (Owner : Context; Region : C.Node_Descriptor;
     Data : C.Namespace.Operation_Region_Attributes) return Boolean is (True);
   function Permanent (Owner : Context; Region : C.Node_Descriptor) return Boolean is (False);
   procedure Invoke (Service : aliased in out C.State; Owner : in out Context;
     Node : C.Namespace.Node_ID; Args : AML_Execute.Arguments; Count, Budget : Natural;
     Result : out C.Values.Result; Status : out C.Values.Access_Status) is
   begin Result := (AML_Execute.No_Return,0); Status := C.Values.Available; end Invoke;
   package R is new ACPI_Region_Initialization (C, Context, Ready, Permanent, Invoke);
   package Tests is new R.Descriptor_Tests;
   Checks : Natural;
begin
   Tests.Run (Checks);
   Ada.Text_IO.Put_Line ("Descriptor checks" & Checks'Image);
end Route_Tests;
