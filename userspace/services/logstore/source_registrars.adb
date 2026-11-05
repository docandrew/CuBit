pragma Ada_2022;
with CuBit.Channel_Contracts;
with CuBit.Channel_Protocol;
with CuBit.Log_Sources;

package body Source_Registrars is
   use CuBit.Messages;
   package Channels renames CuBit.Channels;
   package LS renames CuBit.Log_Sources;
   use type Channels.Take_Result;
   use type Channels.Side;
   use type CuBit.Channel_Contracts.Contract;
   use type LS.Entry_Kind;

   function Number_Of (Index : Positive) return Unsigned_64 is
     (First_Number + Unsigned_64 (Index));

   --  Take up to a batch of R's notices into Identities.
   procedure Drain_One
     (R : in out Registrar; Identities : in out Source_Identities.Table;
      Now_Ms : Unsigned_64; Pending : in out Boolean);
   procedure Drain_One
     (R : in out Registrar; Identities : in out Source_Identities.Table;
      Now_Ms : Unsigned_64; Pending : in out Boolean)
   is
      Wire : LS.Entry_Buffer;
      Length : Natural;
      Taken : Channels.Take_Result;
      Notice : LS.Source_Entry;
      Accepted : Boolean;
   begin
      for Count in 1 .. Batch_Notices loop
         --  Copied out of the launcher's ring, then decoded (untrusted).
         Channels.Take (R.Link, Wire'Address, Wire'Length, Length, Taken);
         exit when Taken /= Channels.Taken;
         if Length = LS.Entry_Bytes then
            LS.Decode (Wire, Notice, Accepted);
            if Accepted then
               if Notice.Kind = LS.Started then
                  Source_Identities.Started (Identities, Notice.Who, Now_Ms);
               else
                  Source_Identities.Exited (Identities, Notice.Who.Source, Now_Ms);
               end if;
            end if;
         end if;
         if Count = Batch_Notices then
            Pending := True;
         end if;
      end loop;
   end Drain_One;

   procedure Finish
     (R : in out Registrar; Identities : in out Source_Identities.Table; Now_Ms : Unsigned_64);
   procedure Finish
     (R : in out Registrar; Identities : in out Source_Identities.Table; Now_Ms : Unsigned_64)
   is
      Pending : Boolean := False;
   begin
      Drain_One (R, Identities, Now_Ms, Pending);
      Channels.Close (R.Link);
      R := (others => <>);
   end Finish;

   procedure Open
     (Item : in out Table; Identities : in out Source_Identities.Table;
      From : CuBit.Messages.Process_ID; Authority : Unsigned_64;
      Request : CuBit.Messages.Message; Now_Ms : Unsigned_64; Reply : out CuBit.Messages.Message)
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
      --  The same launcher again: its earlier channel (its notices taken
      --  first, then let go), else a free entry.
      for I in Item'Range loop
         if Item (I).Link.Active
           and then Item (I).Owner = From and then Item (I).Authority = Authority
         then
            Chosen := I;
         end if;
      end loop;
      if Chosen = 0 then
         for I in Item'Range loop
            if not Item (I).Link.Active then
               Chosen := I;
               exit;
            end if;
         end loop;
      end if;
      if Chosen = 0 then
         Reply := Channels.Refusal_Reply (CuBit.Channel_Protocol.No_Room);
         return;
      end if;
      if Item (Chosen).Link.Active then
         Finish (Item (Chosen), Identities, Now_Ms);
      end if;
      Channels.Accept_Open (From, Request, Number_Of (Chosen), Item (Chosen).Link, Reply);
      if Item (Chosen).Link.Active then
         Item (Chosen).Owner := From;
         Item (Chosen).Authority := Authority;
      end if;
   end Open;

   procedure Close
     (Item : in out Table; Identities : in out Source_Identities.Table;
      From : CuBit.Messages.Process_ID; Request : CuBit.Messages.Message; Now_Ms : Unsigned_64)
   is
      Number : constant Unsigned_64 := Channels.Number_Of (Request);
   begin
      if Owns (Number) then
         declare
            R : Registrar renames Item (Natural (Number - First_Number));
         begin
            if R.Owner = From and then Channels.Closed_By (R.Link, From, Request) then
               Finish (R, Identities, Now_Ms);
            end if;
         end;
      end if;
   end Close;

   procedure Ended
     (Item : in out Table; Identities : in out Source_Identities.Table;
      Event : CuBit.Control_Events.Event; Now_Ms : Unsigned_64) is
   begin
      for R of Item loop
         if Channels.Ended (R.Link, Event) then
            Finish (R, Identities, Now_Ms);
         end if;
      end loop;
   end Ended;

   procedure Drain
     (Item : in out Table; Identities : in out Source_Identities.Table;
      Now_Ms : Unsigned_64; Pending : out Boolean) is
   begin
      Pending := False;
      for R of Item loop
         if R.Link.Active then
            Drain_One (R, Identities, Now_Ms, Pending);
         end if;
      end loop;
   end Drain;

   procedure Arm (Item : in out Table; Pending : out Boolean) is
   begin
      Pending := False;
      for R of Item loop
         if R.Link.Active and then not Channels.Arm (R.Link) then
            Pending := True;
         end if;
      end loop;
   end Arm;

   procedure Disarm (Item : in out Table) is
   begin
      for R of Item loop
         if R.Link.Active then
            Channels.Disarm (R.Link);
         end if;
      end loop;
   end Disarm;
end Source_Registrars;
