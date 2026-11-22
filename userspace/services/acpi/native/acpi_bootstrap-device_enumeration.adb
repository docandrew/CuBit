pragma Ada_2022;
package body ACPI_Bootstrap.Device_Enumeration is
   function Ready (Session : State) return Boolean is
     (not Storage_Released (Session) and then Device_Walk_Finished (Session));
   procedure Start (Item : in out Run; Session : State; Budget : Natural; Allowed : out Admission) is
   begin
      Allowed := Not_Ready;
      if not Ready (Session) then return; end if;
      Allowed := Owner_Busy;
      if not Metadata_Owner_Admitted or else Retained_Results_Pending (Session) then return; end if;
      Allowed := Admitted; Cursor.Start (Item, Session.Core, Budget);
   end Start;
   procedure Advance (Item : in out Run; Session : State; Max_Nodes : Positive;
      Device : out ACPI_Service.Node_Descriptor; Reads : out Natural;
      Result : out Cursor.Step_Result; Allowed : out Admission) is
   begin
      Device := ACPI_Service.Values.No_Node; Reads := 0; Result := Cursor.No_Start;
      Allowed := Not_Ready;
      if not Ready (Session) then return; end if;
      Allowed := Owner_Busy;
      if not Metadata_Owner_Admitted or else Retained_Results_Pending (Session) then return; end if;
      Allowed := Admitted; Cursor.Advance (Item, Session.Core, Max_Nodes, Device, Reads, Result);
   end Advance;
end ACPI_Bootstrap.Device_Enumeration;
