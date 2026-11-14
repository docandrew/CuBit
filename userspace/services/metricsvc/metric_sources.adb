pragma Ada_2022;
with CuBit.Channel_Contracts;
with CuBit.Channel_Protocol;
with Heap_Growth;

package body Metric_Sources is
   use CuBit.Messages;
   package Channels renames CuBit.Channels;
   package LS renames CuBit.Log_Sources;
   use type Channels.Take_Result;
   use type Channels.Side;
   use type CuBit.Channel_Contracts.Contract;
   use type LS.Entry_Kind;

   Initial_Sources : constant := 64;
   procedure Grow is new Heap_Growth (Source_Blocks);

   function Number_Of (Index : Positive) return Unsigned_64 is (First_Number + Unsigned_64 (Index));

   function Find (T : Table; Process : Unsigned_64) return Natural;
   function Find (T : Table; Process : Unsigned_64) return Natural is
   begin
      for I in reverse 0 .. T.Used - 1 loop
         if Source_Blocks.Get (T.Sources, I).Who.Source = Process then
            return I + 1;
         end if;
      end loop;
      return 0;
   end Find;

   procedure Started (T : in out Table; Who : LS.Source_Identity);
   procedure Started (T : in out Table; Who : LS.Source_Identity) is
      At_Entry : constant Natural := Find (T, Who.Source);
      Grown : Boolean;
      Oldest : Natural := 0;
   begin
      T.Order := T.Order + 1;
      if At_Entry /= 0 then
         Source_Blocks.Set (T.Sources, At_Entry - 1, (Who => Who, Exited => False, Order => T.Order));
         return;
      end if;
      Grow (T.Sources, T.Used, Initial_Sources, Maximum_Sources, T.Used + 1, Grown);
      if Grown then
         Source_Blocks.Set (T.Sources, T.Used, (Who => Who, Exited => False, Order => T.Order));
         T.Used := T.Used + 1;
         return;
      end if;
      --  At the ceiling: the longest-exited gives way.
      for I in 0 .. T.Used - 1 loop
         declare
            K : constant Known_Source := Source_Blocks.Get (T.Sources, I);
         begin
            if K.Exited and then (Oldest = 0 or else K.Order < Source_Blocks.Get (T.Sources, Oldest - 1).Order) then
               Oldest := I + 1;
            end if;
         end;
      end loop;
      if Oldest = 0 then
         T.Losses := T.Losses + 1;
      else
         Source_Blocks.Set (T.Sources, Oldest - 1, (Who => Who, Exited => False, Order => T.Order));
      end if;
   end Started;

   procedure Exited (T : in out Table; Process : Unsigned_64);
   procedure Exited (T : in out Table; Process : Unsigned_64) is
      At_Entry : constant Natural := Find (T, Process);
      K : Known_Source;
   begin
      if At_Entry /= 0 then
         K := Source_Blocks.Get (T.Sources, At_Entry - 1);
         K.Exited := True;
         Source_Blocks.Set (T.Sources, At_Entry - 1, K);
      end if;
   end Exited;

   procedure Drain_One (T : in out Table; R : in out Registrar; Pending : in out Boolean);
   procedure Drain_One (T : in out Table; R : in out Registrar; Pending : in out Boolean) is
      Wire : LS.Entry_Buffer;
      Length : Natural;
      Taken : Channels.Take_Result;
      Notice : LS.Source_Entry;
      Accepted : Boolean;
   begin
      for Count in 1 .. Batch_Notices loop
         Channels.Take (R.Link, Wire'Address, Wire'Length, Length, Taken);
         exit when Taken /= Channels.Taken;
         if Length = LS.Entry_Bytes then
            LS.Decode (Wire, Notice, Accepted);
            if Accepted then
               if Notice.Kind = LS.Started then
                  Started (T, Notice.Who);
               else
                  Exited (T, Notice.Who.Source);
               end if;
            end if;
         end if;
         if Count = Batch_Notices then
            Pending := True;
         end if;
      end loop;
   end Drain_One;

   procedure Finish (T : in out Table; R : in out Registrar);
   procedure Finish (T : in out Table; R : in out Registrar) is
      Pending : Boolean := False;
   begin
      Drain_One (T, R, Pending);
      Channels.Close (R.Link);
      R := (others => <>);
   end Finish;

   procedure Open
     (T : in out Table; From : CuBit.Messages.Process_ID; Authority : Unsigned_64;
      Request : CuBit.Messages.Message; Reply : out CuBit.Messages.Message)
   is
      Chosen : Natural := 0;
      Is_Open, Valid : Boolean;
      Offered : CuBit.Channel_Contracts.Contract;
      Opener_Side : Channels.Side;
      Ignore_Connector : Unsigned_16;
   begin
      Channels.Decode_Open (Request, Is_Open, Valid, Offered, Opener_Side, Ignore_Connector);
      if not Valid or else Offered /= LS.CONTRACT or else Opener_Side /= Channels.Producing then
         Reply := Channels.Refusal_Reply (CuBit.Channel_Protocol.Unknown_Type);
         return;
      end if;
      for I in T.Registrars'Range loop
         if T.Registrars (I).Link.Active and then T.Registrars (I).Owner = From
           and then T.Registrars (I).Authority = Authority
         then
            Chosen := I;
         end if;
      end loop;
      if Chosen = 0 then
         for I in T.Registrars'Range loop
            if not T.Registrars (I).Link.Active then
               Chosen := I;
               exit;
            end if;
         end loop;
      end if;
      if Chosen = 0 then
         Reply := Channels.Refusal_Reply (CuBit.Channel_Protocol.No_Room);
         return;
      end if;
      if T.Registrars (Chosen).Link.Active then
         Finish (T, T.Registrars (Chosen));
      end if;
      Channels.Accept_Open (From, Request, Number_Of (Chosen), T.Registrars (Chosen).Link, Reply);
      if T.Registrars (Chosen).Link.Active then
         T.Registrars (Chosen).Owner := From;
         T.Registrars (Chosen).Authority := Authority;
      end if;
   end Open;

   procedure Close (T : in out Table; From : CuBit.Messages.Process_ID; Request : CuBit.Messages.Message) is
      Number : constant Unsigned_64 := Channels.Number_Of (Request);
   begin
      if Owns (Number) then
         declare
            R : Registrar renames T.Registrars (Natural (Number - First_Number));
         begin
            if R.Owner = From and then Channels.Closed_By (R.Link, From, Request) then
               Finish (T, R);
            end if;
         end;
      end if;
   end Close;

   procedure Ended (T : in out Table; Event : CuBit.Control_Events.Event) is
   begin
      for R of T.Registrars loop
         if Channels.Ended (R.Link, Event) then
            Finish (T, R);
         end if;
      end loop;
   end Ended;

   procedure Drain (T : in out Table; Pending : out Boolean) is
   begin
      Pending := False;
      for R of T.Registrars loop
         if R.Link.Active then
            Drain_One (T, R, Pending);
         end if;
      end loop;
   end Drain;

   function Known (T : Table; Process : Unsigned_64) return Boolean is (Find (T, Process) /= 0);

   function Identity_Of (T : Table; Process : Unsigned_64) return String is
      At_Entry : constant Natural := Find (T, Process);
   begin
      return (if At_Entry = 0 then "" else LS.Identity_Of (Source_Blocks.Get (T.Sources, At_Entry - 1).Who));
   end Identity_Of;

   function Program_Of (T : Table; Process : Unsigned_64) return String is
      At_Entry : constant Natural := Find (T, Process);
   begin
      return (if At_Entry = 0 then "" else LS.Label (Source_Blocks.Get (T.Sources, At_Entry - 1).Who));
   end Program_Of;

   function Lost (T : Table) return Unsigned_64 is (T.Losses);
end Metric_Sources;
