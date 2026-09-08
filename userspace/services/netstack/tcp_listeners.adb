package body TCP_Listeners with SPARK_Mode is
   procedure Initialize (Item : out Table) is
   begin
      Item := (others => <>);
   end Initialize;

   function Find (Item : Table; Address : Unsigned_32; Port : Unsigned_16) return Handle is
   begin
      for E of Item.Entries loop
         if E.Id /= No_Handle and E.Address = Address and E.Port = Port then return E.Id; end if;
      end loop;
      return No_Handle;
   end Find;

   function Owned (Item : Table; Owner : Owner_Id; Listener : Handle) return Boolean is
   begin
      for E of Item.Entries loop
         if E.Id /= No_Handle and E.Id = Listener and E.Owner = Owner then return True; end if;
      end loop;
      return False;
   end Owned;

   procedure Bind
     (Item : in out Table; Owner : Owner_Id; Address : Unsigned_32;
      Port : Unsigned_16; Listener : out Handle; Status : out Bind_Status) is
   begin
      Listener := No_Handle;
      if Address = 0 or Port = 0 then Status := Invalid_Address; return; end if;
      if Find (Item, Address, Port) /= No_Handle then Status := Address_In_Use; return; end if;
      if Item.Next_Id = Unsigned_64'Last then Status := Handles_Exhausted; return; end if;
      Status := Table_Full;
      for E of Item.Entries loop
         if E.Id = No_Handle then
            Listener := Item.Next_Id; Item.Next_Id := Item.Next_Id + 1;
            E := (Id => Listener, Owner => Owner, Address => Address, Port => Port, others => <>);
            Status := Bound; return;
         end if;
      end loop;
   end Bind;

   procedure Reserve
     (Item : in out Table; Listener : Handle; Connection : Connection_Index;
      Deadline : Unsigned_64; Success : out Boolean) is
   begin
      Success := False;
      --  Check global connection ownership before changing any backlog.
      for E of Item.Entries loop
         for C of E.Pending loop
            if C.State /= Vacant and C.Connection = Connection then return; end if;
         end loop;
      end loop;
      for E of Item.Entries loop
         if E.Id /= No_Handle and E.Id = Listener then
            for C of E.Pending loop
               if C.State = Vacant then
                  C := (Handshaking, Connection, Deadline); Success := True; return;
               end if;
            end loop;
         end if;
      end loop;
   end Reserve;

   procedure Mark_Ready (Item : in out Table; Connection : Connection_Index) is
   begin
      for E of Item.Entries loop
         for C of E.Pending loop
            if C.State = Handshaking and C.Connection = Connection then C.State := Ready; end if;
         end loop;
      end loop;
   end Mark_Ready;

   procedure Accept_Ready
     (Item : in out Table; Owner : Owner_Id; Listener : Handle;
      Connection : out Connection_Index; Found : out Boolean) is
   begin
      Connection := 0; Found := False;
      for E of Item.Entries loop
         if E.Id /= No_Handle and E.Id = Listener and E.Owner = Owner then
            for C of E.Pending loop
               if C.State = Ready then
                  Connection := C.Connection; C := (others => <>); Found := True; return;
               end if;
            end loop;
         end if;
      end loop;
   end Accept_Ready;

   procedure Remove (Item : in out Table; Connection : Connection_Index) is
   begin
      for E of Item.Entries loop
         for C of E.Pending loop
            if C.State /= Vacant and C.Connection = Connection then C := (others => <>); end if;
         end loop;
      end loop;
   end Remove;

   procedure Close
     (Item : in out Table; Owner : Owner_Id; Listener : Handle;
      Children : out Connection_List; Success : out Boolean) is
   begin
      Children := [others => False]; Success := False;
      for E of Item.Entries loop
         if E.Id /= No_Handle and E.Id = Listener and E.Owner = Owner then
            for C of E.Pending loop
               if C.State /= Vacant then Children (C.Connection) := True; end if;
            end loop;
            E := (others => <>); Success := True; return;
         end if;
      end loop;
   end Close;

   procedure Expire
     (Item : in out Table; Now : Unsigned_64; Children : out Connection_List) is
   begin
      Children := [others => False];
      for E of Item.Entries loop
         for C of E.Pending loop
            if C.State /= Vacant and then Now >= C.Deadline then
               Children (C.Connection) := True; C := (others => <>);
            end if;
         end loop;
      end loop;
   end Expire;

   function Next_Deadline (Item : Table) return Unsigned_64 is
      Result : Unsigned_64 := Unsigned_64'Last;
   begin
      for E of Item.Entries loop
         for C of E.Pending loop
            if C.State /= Vacant then Result := Unsigned_64'Min (Result, C.Deadline); end if;
         end loop;
      end loop;
      return Result;
   end Next_Deadline;
end TCP_Listeners;
