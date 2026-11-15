pragma Ada_2022;
package body ACPI_Consumer_Request_IDs with SPARK_Mode is
   function After_Allocation (Allocated : Dynamic_Request_ID) return Cursor is
   begin
      if Allocated = Request_ID'Last then
         return (Next => Allocated, Exhausted => True);
      else
         return (Next => Allocated + 1, Exhausted => False);
      end if;
   end After_Allocation;
   function Exhausted (Run : State) return Boolean is (Run.Position.Exhausted);
   function Next_Value (Run : State) return Dynamic_Request_ID is (Run.Position.Next);
   procedure Take (Run : in out State; Request : out Request_ID; Granted : out Boolean) is
   begin
      Granted := not Run.Position.Exhausted;
      if Granted then
         Request := Run.Position.Next;
         Run.Position := After_Allocation (Run.Position.Next);
      else
         Request := No_Request;
      end if;
   end Take;
end ACPI_Consumer_Request_IDs;
