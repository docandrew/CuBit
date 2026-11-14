pragma Ada_2022;
with CuBit.Channel_Contracts;
with CuBit.Channel_Protocol;
with CuBit.Metric_Outlet;
with CuBit.Process_IDs.Text;
with Heap_Growth;
with Metric_Strings;

package body Metric_Outlets is
   use CuBit.Messages;
   package Channels renames CuBit.Channels;
   package Outlet renames CuBit.Metric_Outlet;
   use type Channels.Take_Result;
   use type Channels.Side;
   use type CuBit.Channel_Contracts.Contract;

   Initial_Outlets : constant := 16;
   Initial_Keys : constant := 32;
   --  The machine, until clusters name nodes (Node_Id, DC-001).
   Local_Node : constant String := "local";
   Unnamed_Source : constant String := "unnamed";
   procedure Grow_Outlets is new Heap_Growth (Outlet_Blocks);
   procedure Grow_Keys is new Heap_Growth (Key_Blocks);

   function Add (A, B : Unsigned_64) return Unsigned_64 is
     (if B > Unsigned_64'Last - A then Unsigned_64'Last else A + B);

   function Open_Count (T : Table) return Natural is
      N : Natural := 0;
   begin
      for I in 0 .. T.Used - 1 loop
         if Outlet_Blocks.Get (T.Outlets, I).Link.Active then
            N := N + 1;
         end if;
      end loop;
      return N;
   end Open_Count;

   function Accounting (T : Table) return Totals is (T.Sums);

   --  A Describe: the series named, labelled from the opener.
   procedure Describe
     (O : in out Outlet_Record; Store : in out Metric_Series.Store; Sources : Metric_Sources.Table;
      Item : Outlet.Element);
   procedure Describe
     (O : in out Outlet_Record; Store : in out Metric_Series.Store; Sources : Metric_Sources.Table;
      Item : Outlet.Element)
   is
      Process : constant Unsigned_64 := To_Word (O.Owner);
      Identity : constant String := Metric_Sources.Identity_Of (Sources, Process);
      Program : constant String := Metric_Sources.Program_Of (Sources, Process);
      Instance : constant String := CuBit.Process_IDs.Text.Image (O.Owner);
      L : Metric_Series.Labels;
      Index : Metric_Series.Series_Count;
      Same : Boolean;
      Grown : Boolean;
      Key : constant Natural := Natural (Item.Key);
   begin
      Metric_Series.Intern (Store, Item.Name (1 .. Item.Length), L.Name);
      Metric_Series.Intern (Store, (if Identity'Length > 0 then Identity else Unnamed_Source), L.Source);
      Metric_Series.Intern (Store, (if Program'Length > 0 then Program else Unnamed_Source), L.Program);
      Metric_Series.Intern (Store, Instance, L.Instance);
      Metric_Series.Intern (Store, Local_Node, L.Node);
      Index := Metric_Series.No_Series;
      if L.Name /= Metric_Strings.No_String then
         Metric_Series.Find_Or_Add (Store, L, Item.Series, Item.Measure, Index, Same);
      end if;
      if Key > O.Key_Count then
         Grow_Keys (O.Keys, O.Key_Count, Initial_Keys, Outlet.Maximum_Series_Per_Source, Key, Grown);
         if not Grown then
            return;
         end if;
         for K in O.Key_Count .. Key - 1 loop
            Key_Blocks.Set (O.Keys, K, Metric_Series.No_Series);
         end loop;
         O.Key_Count := Key;
      end if;
      Key_Blocks.Set (O.Keys, Key - 1, Index);
   end Describe;

   function Series_Of (O : Outlet_Record; Key : Outlet.Series_Key) return Metric_Series.Series_Count is
     (if Natural (Key) > O.Key_Count then Metric_Series.No_Series else Key_Blocks.Get (O.Keys, Natural (Key) - 1));

   procedure Drain_One
     (T : in out Table; O : in out Outlet_Record; Store : in out Metric_Series.Store;
      Sources : Metric_Sources.Table; Now_Ms : Unsigned_64; Pending : in out Boolean);
   procedure Drain_One
     (T : in out Table; O : in out Outlet_Record; Store : in out Metric_Series.Store;
      Sources : Metric_Sources.Table; Now_Ms : Unsigned_64; Pending : in out Boolean)
   is
      Wire : Outlet.Element_Buffer;
      Length : Natural;
      Taken : Channels.Take_Result;
      Shed : constant Unsigned_64 := Channels.Shed_Count (O.Link);
   begin
      --  Labels first: wait (briefly) for the launcher's notice.
      if not Metric_Sources.Known (Sources, To_Word (O.Owner))
        and then Now_Ms >= O.Opened_Ms and then Now_Ms - O.Opened_Ms < Identity_Wait_Ms
      then
         --  The next pass (Drain_Interval_Ms) looks again.
         return;
      end if;
      for Count in 1 .. Batch_Elements loop
         Channels.Take (O.Link, Wire'Address, Wire'Length, Length, Taken);
         exit when Taken /= Channels.Taken;
         T.Sums.Elements := Add (T.Sums.Elements, 1);
         declare
            Item : constant Outlet.Decoded := Outlet.Decode (Wire, Length);
         begin
            if not Item.Success then
               T.Sums.Malformed := Add (T.Sums.Malformed, 1);
            else
               case Item.Value.Kind is
                  when Outlet.Describe =>
                     Describe (O, Store, Sources, Item.Value);
                  when Outlet.Sample =>
                     if Series_Of (O, Item.Value.Key) = Metric_Series.No_Series then
                        T.Sums.Unknown_Key := Add (T.Sums.Unknown_Key, 1);
                     else
                        Metric_Series.Append
                          (Store, Series_Of (O, Item.Value.Key), Item.Value.Time_Us, Item.Value.Value);
                     end if;
                  when Outlet.Loss =>
                     if Series_Of (O, Item.Value.Key) = Metric_Series.No_Series then
                        T.Sums.Unknown_Key := Add (T.Sums.Unknown_Key, Item.Value.Shed);
                     else
                        Metric_Series.Note_Shed
                          (Store, Series_Of (O, Item.Value.Key), Item.Value.Shed, Item.Value.First_Us,
                           Item.Value.Last_Us);
                     end if;
               end case;
            end if;
         end;
         if Count = Batch_Elements then
            Pending := True;
         end if;
      end loop;
      if Shed > O.Shed_Seen then
         T.Sums.Ring_Shed := Add (T.Sums.Ring_Shed, Shed - O.Shed_Seen);
      end if;
      O.Shed_Seen := Shed;
   end Drain_One;

   procedure Finish
     (T : in out Table; O : in out Outlet_Record; Store : in out Metric_Series.Store;
      Sources : Metric_Sources.Table; Now_Ms : Unsigned_64);
   procedure Finish
     (T : in out Table; O : in out Outlet_Record; Store : in out Metric_Series.Store;
      Sources : Metric_Sources.Table; Now_Ms : Unsigned_64)
   is
      Pending : Boolean := False;
   begin
      --  What a program wrote just before it ended is kept: no identity
      --  wait now (the end of time is past every wait).
      Drain_One (T, O, Store, Sources, Unsigned_64'Last, Pending);
      pragma Unreferenced (Now_Ms);
      Channels.Close (O.Link);
      Key_Blocks.Release (O.Keys);
      O := (others => <>);
   end Finish;

   procedure Open
     (T : in out Table; From : CuBit.Messages.Process_ID; Authority : Unsigned_64;
      Request : CuBit.Messages.Message; Now_Ms : Unsigned_64; Reply : out CuBit.Messages.Message)
   is
      Chosen : Natural := 0;
      Is_Open, Valid, Grown : Boolean;
      Offered : CuBit.Channel_Contracts.Contract;
      Opener_Side : Channels.Side;
      Ignore_Connector : Unsigned_16;
      O : Outlet_Record;
   begin
      Channels.Decode_Open (Request, Is_Open, Valid, Offered, Opener_Side, Ignore_Connector);
      if not Valid or else Offered /= Outlet.CONTRACT or else Opener_Side /= Channels.Producing then
         Reply := Channels.Refusal_Reply (CuBit.Channel_Protocol.Unknown_Type);
         return;
      end if;
      --  One outlet per process: a second open is refused (its ring keeps
      --  its first). Else a free entry, else a new one.
      for I in 0 .. T.Used - 1 loop
         declare
            Existing : constant Outlet_Record := Outlet_Blocks.Get (T.Outlets, I);
         begin
            if Existing.Link.Active and then Existing.Owner = From and then Existing.Authority = Authority then
               Reply := Channels.Refusal_Reply (CuBit.Channel_Protocol.No_Room);
               return;
            end if;
            if Chosen = 0 and then not Existing.Link.Active then
               Chosen := I + 1;
            end if;
         end;
      end loop;
      if Chosen = 0 then
         Grow_Outlets (T.Outlets, T.Used, Initial_Outlets, Maximum_Outlets, T.Used + 1, Grown);
         if not Grown then
            T.Sums.Refused_Outlets := Add (T.Sums.Refused_Outlets, 1);
            Reply := Channels.Refusal_Reply (CuBit.Channel_Protocol.No_Room);
            return;
         end if;
         T.Used := T.Used + 1;
         Chosen := T.Used;
         Outlet_Blocks.Set (T.Outlets, Chosen - 1, (others => <>));
      end if;
      Channels.Accept_Open (From, Request, Unsigned_64 (Chosen), O.Link, Reply);
      if O.Link.Active then
         O.Owner := From;
         O.Authority := Authority;
         O.Opened_Ms := Now_Ms;
         O.Shed_Seen := Channels.Shed_Count (O.Link);
      end if;
      Outlet_Blocks.Set (T.Outlets, Chosen - 1, O);
   end Open;

   procedure Close
     (T : in out Table; Store : in out Metric_Series.Store; Sources : Metric_Sources.Table;
      From : CuBit.Messages.Process_ID; Request : CuBit.Messages.Message; Now_Ms : Unsigned_64)
   is
      Number : constant Unsigned_64 := Channels.Number_Of (Request);
      O : Outlet_Record;
   begin
      if Number in 1 .. Unsigned_64 (T.Used) then
         O := Outlet_Blocks.Get (T.Outlets, Natural (Number) - 1);
         if O.Owner = From and then Channels.Closed_By (O.Link, From, Request) then
            Finish (T, O, Store, Sources, Now_Ms);
            Outlet_Blocks.Set (T.Outlets, Natural (Number) - 1, O);
         end if;
      end if;
   end Close;

   procedure Ended
     (T : in out Table; Store : in out Metric_Series.Store; Sources : Metric_Sources.Table;
      Event : CuBit.Control_Events.Event; Now_Ms : Unsigned_64)
   is
      O : Outlet_Record;
   begin
      for I in 0 .. T.Used - 1 loop
         O := Outlet_Blocks.Get (T.Outlets, I);
         if O.Link.Active and then Channels.Ended (O.Link, Event) then
            Finish (T, O, Store, Sources, Now_Ms);
            Outlet_Blocks.Set (T.Outlets, I, O);
         end if;
      end loop;
   end Ended;

   procedure Drain
     (T : in out Table; Store : in out Metric_Series.Store; Sources : Metric_Sources.Table;
      Now_Ms : Unsigned_64; Pending : out Boolean)
   is
      O : Outlet_Record;
   begin
      Pending := False;
      for I in 0 .. T.Used - 1 loop
         O := Outlet_Blocks.Get (T.Outlets, I);
         if O.Link.Active then
            Drain_One (T, O, Store, Sources, Now_Ms, Pending);
            Outlet_Blocks.Set (T.Outlets, I, O);
         end if;
      end loop;
   end Drain;
end Metric_Outlets;
