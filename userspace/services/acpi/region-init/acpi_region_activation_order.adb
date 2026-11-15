pragma Ada_2022;
package body ACPI_Region_Activation_Order with SPARK_Mode is
   package N renames Core.Namespace;
   package V renames Core.Values;
   use type N.Object_Kind, N.Lookup_Status, V.Access_Status;
   procedure Required_Ancestor
     (Service : Core.State; Scope : N.Node_ID;
      Ancestor : out N.Node_ID; Accepted : out Boolean) is
      Description : Core.Node_Descriptor;
      Status : V.Access_Status;
      Found : N.Lookup_Result;
      Parent, Next : N.Node_ID;
      Initializer_Scope : Boolean;
   begin
      Ancestor := N.Root; Accepted := False;
      Core.Describe_Node (Service, Scope, Description, Status);
      if Status /= V.Available then return; end if;
      Parent := (if Scope = N.Root then N.Root else V.Parent_Position (Description));
      loop
         Core.Describe_Node (Service, Parent, Description, Status);
         if Status /= V.Available then return; end if;
         Initializer_Scope := Parent = N.Root or else
           V.Node_Kind (Description) in N.Device_Object | N.Processor_Object | N.Thermal_Zone_Object
           or else (V.Node_Name (Description) = "_SB_" and then V.Parent_Position (Description) = N.Root);
         if Initializer_Scope then
            Core.Find_Object (Service, Parent, "_INI", Found, Status);
            if Status /= V.Available then return; end if;
            if Found.Status = N.Found then Ancestor := Parent; Accepted := True; return; end if;
            if Found.Status /= N.Not_Found then return; end if;
         end if;
         if Parent = N.Root then Ancestor := Parent; Accepted := True; return; end if;
         Next := V.Parent_Position (Description);
         if Next >= Parent then return; end if;
         Parent := Next;
      end loop;
   end Required_Ancestor;
end ACPI_Region_Activation_Order;
