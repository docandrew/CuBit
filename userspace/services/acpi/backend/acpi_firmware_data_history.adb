pragma Ada_2022;
package body ACPI_Firmware_Data_History with SPARK_Mode is
   use type Word, Identifier, IPC.Outcome, Store.Outcome;
   function Observe (Owner : Context) return Statistics is
     ((Store.Length (Owner.Items), Store.Capacity (Owner.Items), Owner.Working));
   function Observe_Capacity (Owner : Context) return Capacity_Report is
      R : constant Store.Report := Store.Observe (Owner.Items);
   begin
      return (Problem => (case R.Status is
         when Store.Stored => No_Capacity_Failure,
         when Store.Allocation_Refused => Process_Heap_Limit,
         when Store.Count_Representation_Exhausted => Entry_Limit,
         when Store.Byte_Representation_Exhausted => Byte_Limit),
         Demand => R.Demand, Capacity => Word (R.Capacity),
         Requested_Bytes => R.Requested_Bytes);
   end Observe_Capacity;
   function Valid_Key (Caller : Word; Descriptor, Operation_ID : Identifier)
      return Boolean is
     (Caller /= 0 and then Descriptor /= F.No_ID and then Operation_ID /= F.No_ID);
   function Find (Owner : Context; Caller : Word;
      Descriptor, Operation_ID : Identifier) return Natural is
   begin
      for I in 1 .. Store.Length (Owner.Items) loop
         declare Item : constant Entry_Record := Store.Read (Owner.Items, I); begin
            if Item.Caller = Caller and then Item.Descriptor = Descriptor
              and then Item.Operation_ID = Operation_ID then return I; end if;
         end;
      end loop;
      return 0;
   end Find;
   function Query (Owner : Context; Caller : Word;
      Descriptor, Operation_ID : Identifier) return Report is
      Index : Natural;
   begin
      if not Valid_Key (Caller, Descriptor, Operation_ID) then
         return (State => Invalid_Identity, others => <>);
      end if;
      Index := Find (Owner, Caller, Descriptor, Operation_ID);
      if Index = 0 then return (others => <>); end if;
      return Store.Read (Owner.Items, Index).Saved;
   end Query;
   procedure Submit (Owner : in out Context; Caller : Word;
      Descriptor, Operation_ID : Identifier; For_Write : Boolean; Input : Word;
      Result : out Report) is
      Index : Natural;
      Item : Entry_Record;
      Stored : Store.Outcome;
      Output : Word := 0;
      Status : IPC.Outcome := IPC.Indeterminate;
   begin
      Result := (State => Invalid_Identity, others => <>);
      if not Valid_Key (Caller, Descriptor, Operation_ID)
        or else (not For_Write and then Input /= 0) then return;
      elsif Owner.Working then Result.State := Reentered; return;
      end if;
      Owner.Working := True;
      Index := Find (Owner, Caller, Descriptor, Operation_ID);
      if Index /= 0 then
         Item := Store.Read (Owner.Items, Index);
         if Item.For_Write /= For_Write or else Item.Input /= Input then
            Result := (State => Identity_Conflict, others => <>);
         else Result := Item.Saved;
         end if;
         Owner.Working := False; return;
      end if;
      Item := (Caller, Descriptor, Operation_ID, For_Write, Input,
         (State => Running,
          Effect => (if For_Write then Replies.Write_Effect else Replies.Read_Effect), others => <>));
      Store.Append (Owner.Items, Item, Index, Stored);
      if Stored /= Store.Stored then
         Result := (State => Allocation_Refused, others => <>);
         Owner.Working := False; return;
      end if;
      -- From this point this identity can never dispatch again, including
      -- after lost reply, expiry, descriptor closure or callback failure.
      -- No allocation or borrowed storage pointer crosses Execute.
      Execute (Caller, Descriptor, For_Write, Input, Output, Status);
      Item.Saved := (State => Finished, Status => Status,
         Value => (if Status = IPC.Done and then not For_Write then Output else 0),
         Effect => (if For_Write then Replies.Write_Effect else Replies.Read_Effect));
      Store.Replace (Owner.Items, Index, Item);
      Result := Item.Saved;
      Owner.Working := False;
   end Submit;
end ACPI_Firmware_Data_History;
