pragma Ada_2022;
with AML_Execute;
package body ACPI_Bootstrap.Cached_S5 with SPARK_Mode is
   package C renames ACPI_Service;
   use type C.Context_Key, C.Namespace.Object_Kind, Objects.Query_Outcome, Objects.Register_Model;
   procedure Observe_Static
     (Service : aliased in out C.State; Item : C.Node_Descriptor;
      Budget : Natural; Result : out C.Values.Result; Status : out C.Values.Access_Status) is
   begin
      Result := (Status => AML_Execute.No_Return, Charged => 0);
      Status := C.Values.Wrong_Kind;
      if Budget = 0 or else not C.Current_Node (Service, Item)
        or else C.Values.Node_Kind (Item) /= C.Namespace.Package_Object then return; end if;
      C.Observe_Named_Value (Service, C.Values.Position (Item), Result, Status);
   end Observe_Static;
   procedure Invalidate (Item : in out Snapshot) is
   begin Item.Types := Empty_Types; Item.Context := C.No_Context; end Invalidate;
   procedure Retire (Item : in out Snapshot) is
   begin Item.Retiring := True; Invalidate (Item); end Retire;
   function Current_Types (Session : State; Item : Snapshot) return Copied_Types is
     (if not Item.Retiring and then Item.Types.Available
        and then Item.Context /= C.No_Context and then Device_Walk_Finished (Session)
        and then not Retained_Results_Pending (Session)
        and then C.Current_Context (Session.Core, Item.Context)
      then Item.Types else Empty_Types);
   procedure Prepare (Session : in out State; Item : in out Snapshot;
      Registers : Objects.Register_Model; Budget : Natural;
      Result : out Objects.Query_Report) is
      Anchor : C.Context_Key;
   begin
      Invalidate (Item);
      Result := (Kind => Objects.S5, Registers => Registers, others => <>);
      if Item.Retiring or else not Device_Walk_Finished (Session)
        or else Retained_Results_Pending (Session) then return; end if;
      Anchor := C.Capture_Context (Session.Core);
      if Anchor = C.No_Context then return; end if;
      Objects.Query (Session.Core, Objects.S5, Registers, Budget, Result);
      if Result.Outcome /= Objects.Types_Available
        or else not Result.A_Available
        or else (Registers = Objects.PM1A_And_B and then not Result.B_Available)
        or else Retained_Results_Pending (Session)
        or else not C.Current_Context (Session.Core, Anchor) then return; end if;
      Item.Types := (Available => True, Registers => Registers,
         Type_A => Result.Type_A, Type_B => Result.Type_B);
      Item.Context := Anchor;
   end Prepare;
end ACPI_Bootstrap.Cached_S5;
