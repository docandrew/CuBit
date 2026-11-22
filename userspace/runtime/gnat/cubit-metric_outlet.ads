------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A program's metric outlet, com.cubit-os.stdmetric (MET-002): typed
--  metric elements in a ring the program owns and metrics.svc drains, the
--  way com.cubit.stdlog's records reach logstore (CuBit.Log_Publish_Rings).
--
--  @description
--  A program has the outlet only when its manifest declares it,
--  (Outlet "com.cubit-os.stdmetric" Element.Metric_Records). The
--  declaration requests metrics.svc's publisher endpoint as the optional
--  startup entry Startup_Binding; the launcher badges it for this program
--  alone, or leaves it Absent when metrics.svc is not running. Nothing else
--  is registered: a series appears in the store with its first sample.
--
--  The ring is a Shed_Newest queue: a full ring drops the new element and
--  the producer counts it against the element's series, then reports the
--  count as a Loss element once there is room again. No sample is lost
--  silently: metrics.svc stores each Loss as a typed gap of its series.
--
--  Elements, little-endian, one per ring entry:
--    bytes 0 .. 3   element kind (Element_Kind'Enum_Rep)
--    bytes 4 .. 7   series key (1 .. Maximum_Series_Per_Source), the
--                   producer's own numbering; metrics.svc joins it to the
--                   opener's identity, never trusting the element for it
--    Describe: byte 8 series kind, byte 9 unit, byte 10 name length,
--              bytes 11 .. 15 zero, then the name (Name_Charset)
--    Sample:   bytes 8 .. 15 time (microseconds of the published monotonic
--              clock, CuBit.Monotonic), bytes 16 .. 23 value
--    Loss:     bytes 8 .. 15 samples shed, bytes 16 .. 23 the first shed
--              sample's time, bytes 24 .. 31 the last's
--  A Counter's sample is an increment; a Gauge's the value it has from
--  then on; a Distribution's one observation (percentiles come from the
--  store). Values are producer claims; identity comes from the channel.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Channel_Contracts;
with CuBit.Protocols;
with CuBit.Program_Descriptions;

package CuBit.Metric_Outlet with Pure, SPARK_Mode is

   --  The outlet's qualified name, and the startup entry its declaration
   --  requests.
   Outlet_Name : constant String := CuBit.Program_Descriptions.Metric_Outlet_Name;
   Startup_Binding : constant String := CuBit.Program_Descriptions.Metric_Outlet_Binding;

   METRIC_ELEMENT_SCHEMA : constant CuBit.Protocols.Schema_Id := 16#4D45_5452_4943_0001#;

   --  The ring a program publishes into: 64 KiB, about 2 700 samples
   --  between two drains of metrics.svc. A larger producer sheds and
   --  counts, never waits.
   Default_Ring_Pages : constant := 16;

   --  A program numbers its series from 1, in declaration order, up to
   --  this named ceiling; one more is refused (Capacity) and counted.
   Maximum_Series_Per_Source : constant := 65_535;
   subtype Series_Count is Unsigned_32 range 0 .. Maximum_Series_Per_Source;
   subtype Series_Key is Series_Count range 1 .. Maximum_Series_Per_Source;

   type Element_Kind is (Describe, Sample, Loss);
   for Element_Kind use (Describe => 1, Sample => 2, Loss => 3);

   type Series_Kind is (Counter, Gauge, Distribution);
   for Series_Kind use (Counter => 1, Gauge => 2, Distribution => 3);

   type Unit is (Count, Microseconds, Nanoseconds, Bytes, Pixels);
   for Unit use (Count => 1, Microseconds => 2, Nanoseconds => 3, Bytes => 4, Pixels => 5);

   --  Series names: lowercase dotted words (desktop.latency.input_to_ready).
   Maximum_Name_Bytes : constant := 64;
   subtype Name_Length is Natural range 0 .. Maximum_Name_Bytes;
   subtype Name_Text is String (1 .. Maximum_Name_Bytes);
   function Name_Character (C : Character) return Boolean is
     (C in 'a' .. 'z' | '0' .. '9' | '.' | '_' | '-');
   function Valid_Name (Name : String) return Boolean is
     (Name'Length in 1 .. Maximum_Name_Bytes
      and then (for all C of Name => Name_Character (C))
      and then Name (Name'First) in 'a' .. 'z');

   Header_Bytes : constant := 16;
   --  Every element starts with its kind and key.
   Key_Bytes : constant := 8;
   Describe_Fixed_Bytes : constant := Header_Bytes;
   Sample_Bytes : constant := 24;
   Loss_Bytes : constant := 32;
   Maximum_Element_Bytes : constant := Describe_Fixed_Bytes + Maximum_Name_Bytes;
   subtype Element_Length is Natural range 0 .. Maximum_Element_Bytes;
   subtype Element_Index is Positive range 1 .. Maximum_Element_Bytes;
   type Element_Buffer is array (Element_Index) of Unsigned_8;

   CONTRACT : constant CuBit.Channel_Contracts.Contract :=
     (Element => (Identity => METRIC_ELEMENT_SCHEMA, Version => 1,
                  Sizing => CuBit.Protocols.Bounded_Size, Wire_Size => Maximum_Element_Bytes),
      Kind    => CuBit.Channel_Contracts.Queue,
      Policy  => CuBit.Channel_Contracts.Shed_Newest,
      Pages   => Default_Ring_Pages,
      Buffers => 1,
      Rule    => CuBit.Channel_Contracts.Copy_Then_Validate);

   type Element (Kind : Element_Kind := Sample) is record
      Key : Series_Key := Series_Key'First;
      case Kind is
         when Describe =>
            Series : Series_Kind := Counter;
            Measure : Unit := Count;
            Length : Name_Length := 0;
            Name : Name_Text := [others => ' '];
         when Sample =>
            Time_Us : Unsigned_64 := 0;
            Value : Unsigned_64 := 0;
         when Loss =>
            Shed : Unsigned_64 := 0;
            First_Us : Unsigned_64 := 0;
            Last_Us : Unsigned_64 := 0;
      end case;
   end record;

   function Valid (Item : Element) return Boolean is
     (case Item.Kind is
         when Describe => Item.Length > 0 and then Valid_Name (Item.Name (1 .. Item.Length)),
         when Sample => True,
         when Loss => Item.Shed > 0 and then Item.First_Us <= Item.Last_Us);

   --  The bytes an element occupies in the ring.
   function Length_Of (Item : Element) return Element_Length is
     (case Item.Kind is
         when Describe => Describe_Fixed_Bytes + Item.Length,
         when Sample => Sample_Bytes,
         when Loss => Loss_Bytes);

   procedure Encode (Item : Element; Into : out Element_Buffer; Length : out Element_Length)
   with Pre => Valid (Item),
        Post => Length = Length_Of (Item) and then Length > Key_Bytes;

   type Decode_Error is
     (Bad_Length, Unknown_Kind, Bad_Key, Bad_Series_Kind, Bad_Unit, Bad_Name, Nonzero_Reserved,
      Bad_Loss);
   type Decoded (Success : Boolean := False) is record
      case Success is
         when True => Value : Element;
         when False => Why : Decode_Error := Bad_Length;
      end case;
   end record;

   --  From the first Length bytes of From (a private copy of a ring entry):
   --  every field checked, nothing trusted.
   function Decode (From : Element_Buffer; Length : Natural) return Decoded
   with Post => (if Decode'Result.Success then Valid (Decode'Result.Value)
                   and then Length_Of (Decode'Result.Value) = Length);

end CuBit.Metric_Outlet;
