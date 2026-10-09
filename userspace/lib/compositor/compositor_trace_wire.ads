with Interfaces;
with Compositor_Input_Trace;
with Compositor_Source_Trace;
with Compositor_Render_Trace;
with Compositor_Frame_Trace;
--  Fixed 128-byte diagnostic event, sixteen little-endian unsigned words.
--  Producer incarnation belongs to the authenticated outer transport.
--  Event_ID must increase without wrapping within that incarnation.
--  This codec establishes representation only, never visibility or retirement.
package Compositor_Trace_Wire with Pure, SPARK_Mode is
   subtype Word is Interfaces.Unsigned_64;
   use type Word;
   package IT renames Compositor_Input_Trace;
   package ST renames Compositor_Source_Trace;
   package RT renames Compositor_Render_Trace;
   package FT renames Compositor_Frame_Trace;
   type Event_Kind is (Input_Event, Source_Event, Render_Event, Frame_Event);
   type Event (Kind : Event_Kind := Input_Event) is record
      Event_ID : Word := 0;
      case Kind is
         when Input_Event => Input : IT.Record_Value;
         when Source_Event => Source : ST.Record_Value;
         when Render_Event => Render : RT.Record_Value;
         when Frame_Event => Frame : FT.Record_Value;
      end case;
   end record;
   function Valid (Value : Event) return Boolean is
     (Value.Event_ID /= 0 and then
      (case Value.Kind is
         when Input_Event => IT.Valid (Value.Input),
         when Source_Event => ST.Valid (Value.Source),
         when Render_Event => RT.Valid (Value.Render),
         when Frame_Event => FT.Valid (Value.Frame)));
   type Packet is array (Natural range 0 .. 15) of Word;
   Magic : constant Word := 16#4354_5243_0000_0001#;
   type Decoded (Success : Boolean := False) is record
      case Success is
         when True => Value : Event;
         when False => null;
      end case;
   end record;
   function Decode (Data : Packet) return Decoded;
   pragma Annotate (GNATprove, Inline_For_Proof, Decode);
   procedure Lemma_Decoded_Valid (Data : Packet)
     with Ghost,
       Post => (if Decode (Data).Success then Valid (Decode (Data).Value));
   function Encode (Value : Event) return Packet
     with Pre => Valid (Value),
       Post => Decode (Encode'Result).Success and then
         Decode (Encode'Result).Value = Value;
private
   function Checked (V : Event) return Decoded is
     (if Valid (V) then (True, V) else (Success => False));

   function Decode (Data : Packet) return Decoded is
     (if Data (0) /= Magic or Data (2) = 0 then (Success => False)
      else
       (case Data (1) is
         when 1 =>
           (if (for some I in 7 .. 15 => Data (I) /= 0)
            then (Success => False)
            else Checked ((Input_Event, Data (2),
                           (Data (3), Data (4), Data (5), Data (6))))),
         when 2 =>
           (if (for some I in 8 .. 15 => Data (I) /= 0)
            then (Success => False)
            else Checked ((Source_Event, Data (2),
                           (Data (3), Data (4), Data (5), Data (6), Data (7))))),
         when 3 =>
           (if Data (3) > 1 or Data (4) > 1 or
               Data (14) /= 0 or Data (15) /= 0
            then (Success => False)
            else Checked ((Render_Event, Data (2),
                           (RT.Phase'Val (Data (3)), Natural (Data (4)),
                            Data (5), Data (6), Data (7), Data (8), Data (9),
                            Data (10), Data (11), Data (12), Data (13))))),
         when 4 =>
           (if Data (3) > 1 or (for some I in 8 .. 15 => Data (I) /= 0)
            then (Success => False)
            else Checked ((Frame_Event, Data (2),
                           (Natural (Data (3)), Data (4), Data (5),
                            Data (6), Data (7))))),
         when others => (Success => False)));

end Compositor_Trace_Wire;
