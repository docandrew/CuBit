pragma Ada_2022;
with Heap_Growth;

package body Metric_Scopes is
   Initial_Processes : constant := 32;
   procedure Grow is new Heap_Growth (Entry_Blocks);

   function Find (T : Table; Process : Unsigned_64) return Natural;
   function Find (T : Table; Process : Unsigned_64) return Natural is
   begin
      for I in 0 .. T.Used - 1 loop
         if Entry_Blocks.Get (T.Entries, I).Process = Process then
            return I + 1;
         end if;
      end loop;
      return 0;
   end Find;

   procedure Set (T : in out Table; Process : Unsigned_64; Prefixes : Prefix_List; Accepted : out Boolean) is
      At_Entry : Natural := Find (T, Process);
      Item : Entry_Record;
      Grown : Boolean;
   begin
      Accepted := False;
      Item.Process := Process;
      for P of Prefixes loop
         if not (for all C of P.Text (1 .. P.Length) => C in 'a' .. 'z' | '0' .. '9' | '.' | '_' | '-') then
            return;
         end if;
         Item.Count := Item.Count + 1;
         Item.Prefixes (Item.Count) := P;
      end loop;
      if At_Entry = 0 then
         --  A free entry (a revoked process's), else a new one.
         for I in 0 .. T.Used - 1 loop
            if Entry_Blocks.Get (T.Entries, I).Process = 0 then
               At_Entry := I + 1;
               exit;
            end if;
         end loop;
      end if;
      if At_Entry = 0 then
         Grow (T.Entries, T.Used, Initial_Processes, Maximum_Processes, T.Used + 1, Grown);
         if not Grown then
            return;
         end if;
         T.Used := T.Used + 1;
         At_Entry := T.Used;
      end if;
      Entry_Blocks.Set (T.Entries, At_Entry - 1, Item);
      Accepted := True;
   end Set;

   procedure Revoke (T : in out Table; Process : Unsigned_64) is
      At_Entry : constant Natural := Find (T, Process);
   begin
      if At_Entry /= 0 then
         Entry_Blocks.Set (T.Entries, At_Entry - 1, (others => <>));
      end if;
   end Revoke;

   function Starts_With (Text, Prefix : String) return Boolean is
     (Text'Length >= Prefix'Length and then Text (Text'First .. Text'First + Prefix'Length - 1) = Prefix);

   function May_Read (T : Table; Process : Unsigned_64; Name : String) return Boolean is
      At_Entry : constant Natural := Find (T, Process);
   begin
      if Process = 0 or else At_Entry = 0 then
         return False;
      end if;
      declare
         E : constant Entry_Record := Entry_Blocks.Get (T.Entries, At_Entry - 1);
      begin
         return (for some I in 1 .. E.Count =>
                   Starts_With (Name, E.Prefixes (I).Text (1 .. E.Prefixes (I).Length)));
      end;
   end May_Read;

   function May_Read_Prefix (T : Table; Process : Unsigned_64; Prefix : String) return Boolean is
     (May_Read (T, Process, Prefix));
   function Has_Any (T : Table; Process : Unsigned_64) return Boolean is
     (Process /= 0 and then Find (T, Process) /= 0
      and then Entry_Blocks.Get (T.Entries, Find (T, Process) - 1).Count > 0);
end Metric_Scopes;
