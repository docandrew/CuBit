------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Push subscriptions to metrics.svc's series (MET-002, DESIGN-metrics-push):
--  the elements metrics.svc puts into a subscriber's ring, and the ring's
--  contract.
--
--  @description
--  A reader opens a consuming channel to metrics.svc through its
--  metrics-observer endpoint (CuBit.Metric_Subscriptions) and names what it
--  wants with Subscribe: a selector, checked like every read against the
--  read scopes procmgr installed for it (its manifest's Scope.Metrics). From
--  then on every sample of a selected series that the store takes is put
--  into the ring, and the store's own gaps follow as Gap elements. The ring
--  is Shed_Newest and bounded: when the reader falls behind, metrics.svc
--  counts what it could not put and sends one Gap (Subscriber_Shed, series
--  0) with the count and time range as soon as there is room. Nothing is
--  lost silently; the reader refetches a gap's range through the query
--  path if it wants it.
--
--  Elements, little-endian, fixed Element_Bytes:
--    bytes 0 .. 3   kind (Element_Kind'Enum_Rep)
--    bytes 4 .. 7   a Gap's cause (Gap_Cause'Enum_Rep); zero for samples
--    bytes 8 .. 15  series id (CuBit.Metric_Query_Pages.Row_Id); zero in a
--                   Subscriber_Shed gap (the subscription's own loss)
--    Sample: 16 .. 23 time (microseconds, CuBit.Monotonic), 24 .. 31 value,
--            32 .. 39 zero
--    Gap:    16 .. 23 samples missing, 24 .. 31 the first's time,
--            32 .. 39 the last's
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Channel_Contracts;
with CuBit.Protocols;

package CuBit.Metric_Updates with Pure, SPARK_Mode is

   METRIC_UPDATE_SCHEMA : constant CuBit.Protocols.Schema_Id := 16#4D45_5452_4943_0002#;

   Element_Bytes : constant := 40;
   subtype Element_Index is Natural range 0 .. Element_Bytes - 1;
   type Element_Buffer is array (Element_Index) of Unsigned_8;

   --  64 KiB: about 1 600 samples between two reads; a reader that reads
   --  every frame never comes near it.
   Ring_Pages : constant := 16;

   CONTRACT : constant CuBit.Channel_Contracts.Contract :=
     (Element => (Identity => METRIC_UPDATE_SCHEMA, Version => 1,
                  Sizing => CuBit.Protocols.Fixed_Size, Wire_Size => Element_Bytes),
      Kind    => CuBit.Channel_Contracts.Queue,
      Policy  => CuBit.Channel_Contracts.Shed_Newest,
      Pages   => Ring_Pages,
      Buffers => 1,
      Rule    => CuBit.Channel_Contracts.Copy_Then_Validate);

   type Element_Kind is (Sample, Gap);
   for Element_Kind use (Sample => 1, Gap => 2);

   --  Producer_Shed and Store_Refused are the store's own gaps of the series
   --  (Metric_Series.Gap_Reason); Subscriber_Shed is this ring's.
   type Gap_Cause is (Producer_Shed, Store_Refused, Subscriber_Shed);
   for Gap_Cause use (Producer_Shed => 1, Store_Refused => 2, Subscriber_Shed => 3);

   type Element (Kind : Element_Kind := Sample) is record
      Series : Unsigned_64 := 0;
      case Kind is
         when Sample =>
            Time_Us, Value : Unsigned_64 := 0;
         when Gap =>
            Cause : Gap_Cause := Subscriber_Shed;
            Missing, First_Us, Last_Us : Unsigned_64 := 0;
      end case;
   end record;

   procedure Encode (Item : Element; Into : out Element_Buffer);

   type Decoded (Success : Boolean := False) is record
      case Success is
         when True => Value : Element;
         when False => null;
      end case;
   end record;
   --  Every word checked: a kind and cause it knows, reserved bytes zero,
   --  a gap's first time no later than its last.
   function Decode (From : Element_Buffer) return Decoded;
end CuBit.Metric_Updates;
