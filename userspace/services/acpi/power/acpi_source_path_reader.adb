pragma Ada_2022;
with AML_Names;
package body ACPI_Source_Path_Reader is
   package P renames ACPI_Source_Paths;
   use type Core.Node_Descriptor,Core.Namespace.Object_Kind,Core.Values.Access_Status;
   procedure Read(Service:Core.State;Device:Core.Node_Descriptor;Value:out P.Path) is
      type Segments is array(Positive range 1..P.Maximum_Segments)of AML_Names.Segment;
      Parts:Segments:=[others=>"____"];
      Count:Natural range 0..P.Maximum_Segments:=0;
      Node:Core.Namespace.Node_ID:=Core.Values.Position(Device);
      Item:Core.Node_Descriptor;Result:Core.Values.Access_Status;
      Last:Natural range 0..P.Maximum_Characters:=0;
   begin
      Value:=P.Empty;
      if Device=Core.Values.No_Node or else not Core.Current_Node(Service,Device)
        or else Core.Values.Node_Kind(Device)/=Core.Namespace.Device_Object then return;end if;
      while Node/=Core.Namespace.Root loop
         if Count=P.Maximum_Segments then Value.State:=P.Too_Long;return;end if;
         Core.Describe_Node(Service,Node,Item,Result);
         if Result/=Core.Values.Available or else not Core.Current_Node(Service,Item)
           or else Core.Values.Parent_Position(Item)=Node then return;end if;
         Count:=Count+1;Parts(Count):=Core.Values.Node_Name(Item);Node:=Core.Values.Parent_Position(Item);
      end loop;
      if Count=0 or else not Core.Current_Node(Service,Device) then return;end if;
      Value.Text(1):=Character'Val(16#5C#);Last:=1;
      for I in reverse 1..Count loop
         Value.Text(Last+1..Last+4):=Parts(I);Last:=Last+4;
         if I>1 then Value.Text(Last+1):='.';Last:=Last+1;end if;
      end loop;
      Value.Length:=Last;Value.State:=P.Known;
   end Read;
end ACPI_Source_Path_Reader;
