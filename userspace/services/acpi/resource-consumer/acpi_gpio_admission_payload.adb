pragma Ada_2022;
with Firmware_Tables.Identifiers;
package body ACPI_GPIO_Admission_Payload with SPARK_Mode is
   package V renames Core.Values;package N renames Core.Namespace;
   package R renames Observation.Resources;
   use type V.Access_Status,N.Definition_Identity;
   use type R.IRQ.Resource_Role,R.IRQ.Interrupt_Mode,R.IRQ.Interrupt_Polarity;
   Bits_Per_Byte:constant:=8;
   procedure Build(Service:Core.State;Run:Source_Session;
      Epoch:Interfaces.Unsigned_64;History:Initializer_Provenance;Data:out P.Request;Result:out Outcome) is
      Item:constant Observation.Metadata:=Describe_Source(Run);
      Candidate:P.Request;
      Reverse_Path:P.Name_Path:=[others=>[others=>P.NUL]];
      Count:P.Path_Count:=0;Cursor:N.Node_ID;
      Node:Core.Node_Descriptor;Status:V.Access_Status;
      Definition:N.Definition_Identity;Table:Natural:=0;
   begin
      Data:=Empty_Payload;Result:=Stale_Observation;
      if not History.Complete then Result:=Incomplete_Initializer_History;return;end if;
      if not Current_Source(Run,Service) then return;end if;
      if Epoch=0 then Result:=Invalid_Epoch;return;end if;
      if Item.Status_Flags>Interfaces.Unsigned_64(Interfaces.Unsigned_32'Last) then Result:=Unsupported_Status;return;end if;
      Cursor:=V.Position(Item.Device);
      while Cursor/=N.Root loop
         if Count=P.Max_Path_Segments then Result:=Path_Too_Deep;return;end if;
         Core.Describe_Node(Service,Cursor,Node,Status);
         if Status/=V.Available or else not Core.Current_Node(Service,Node)
           or else V.Parent_Position(Node)>=Cursor then Result:=Invalid_Path;return;end if;
         Count:=Count+1;Reverse_Path(Count):=V.Node_Name(Node);
         Cursor:=V.Parent_Position(Node);
      end loop;
      if Count=0 then Result:=Invalid_Path;return;end if;
      Candidate.Segments:=Count;
      for I in 1..Count loop Candidate.Path(I):=Reverse_Path(Count+1-I);end loop;
      Core.Describe_Definition(Service,Item.Device,Definition,Status);
      if Status/=V.Available or else not N.Has_Definition(Definition) then Result:=Unknown_Definition;return;end if;
      for I in 1..Core.Table_Count(Service) loop
         if Core.Retained_Definition(Service,I)=Definition then
            if Table/=0 then Result:=Ambiguous_Definition;return;end if;
            Table:=I;
         end if;
      end loop;
      if Table=0 then Result:=Unknown_Definition;return;end if;
      declare Identity:constant Firmware_Tables.Identifiers.Identity:=Core.Table_Identity(Service,Table);begin
         if Identity.Name not in "DSDT"|"SSDT" then Result:=Unsupported_Table;return;end if;
         Candidate.Table:=Identity.Name;Candidate.Table_ID:=Identity.OEM_Table;
      end;
      Candidate.HID:=Item.Identifier;Candidate.STA:=Interfaces.Unsigned_32(Item.Status_Flags);
      Candidate.STA_From:=(if Item.Status_Defaulted then P.Default_STA else P.Evaluated_STA);
      Candidate.Epoch:=Epoch;
      Candidate.Initializer_Invocations:=History.Invocations;
      Candidate.Initializer_Ordered_Digest:=History.Ordered_Digest;
      case Item.UID.Kind is
         when Observation.No_UID=>null;
         when Observation.Integer_UID=>Candidate.UID_Type:=P.Integer_UID;Candidate.UID:=Item.UID.Number;
         when Observation.String_UID=>
            Candidate.UID_Type:=P.String_UID;
            for I in 1..Item.UID.Length loop
               Candidate.UID:=Candidate.UID or Interfaces.Shift_Left
                 (Interfaces.Unsigned_64(Character'Pos(Item.UID.Text(I))),(I-1)*Bits_Per_Byte);
            end loop;
      end case;
      Candidate.Spans:=R.Community_Resources;
      for I in R.Resource_Slot loop
         Candidate.Items(I):=(Base=>Interfaces.Unsigned_64(Item.Resources.Memory(I).Base),
            Length=>Interfaces.Unsigned_64(Item.Resources.Memory(I).Bytes),
            From=>P.Memory32_Fixed,Writable=>Item.Resources.Memory(I).Declared_Writeable);
      end loop;
      if Item.Resources.Interrupt.Present then
         if Item.Resources.Interrupt.Source.Present or else Item.Resources.Interrupt.Role/=R.IRQ.Consumer then
            Result:=Unsupported_Interrupt;return;
         end if;
         Candidate.IRQ:=(Present=>True,GSI=>Interfaces.Unsigned_32(Item.Resources.Interrupt.Number),
            Mode=>(if Item.Resources.Interrupt.Mode=R.IRQ.Level_Triggered then P.Level else P.Edge),
            Level_Of=>(if Item.Resources.Interrupt.Polarity=R.IRQ.Active_Low then P.Active_Low else P.Active_High),
            Share=>(if Item.Resources.Interrupt.Shared then P.Shared else P.Exclusive),
            Wakes=>(if Item.Resources.Interrupt.Wake_Capable then P.Wake_Capable else P.Not_Wake_Capable));
      end if;
      if not Current_Source(Run,Service) then return;end if;
      if not P.Valid(Candidate) then Result:=Invalid_Payload;return;end if;
      Data:=Candidate;Result:=Payload_Ready;
   end Build;
end ACPI_GPIO_Admission_Payload;
