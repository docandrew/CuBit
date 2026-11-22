with Compositor_Input_Batches;

-- Native little-endian shared snapshot. Pointer access and grant ownership
-- belong to the transport, not this pure codec. Decode a local stable copy.
package Compositor_Input_Batch_Wire with SPARK_Mode, Pure is
   package B renames Compositor_Input_Batches;
   subtype Word is B.Word;
   use type Word;
   use type B.IQ.Event;
   subtype Identity is Word range 1 .. Word'Last;
   Magic : constant Word := 16#4355_4249_4E50_0001#;
   Header_Words : constant := 8;
   Event_Words : constant := 4;
   Word_Count : constant := Header_Words + Event_Words * B.Capacity;
   Byte_Count : constant := Word_Count * 8;
   type Snapshot_Words is array (Natural range 0 .. Word_Count - 1) of Word;

   function Valid_Event (E : B.IQ.Event) return Boolean;
   function Valid
     (Value : B.Batch; Surface : Identity; After : Word;
      Maximum : B.Limit) return Boolean is
     (Value.Length <= Maximum and then
      Value.Through = (if Value.Length = 0 then After
                       else Value.Items (Value.Length).Serial) and then
      (if Value.Length = 0 then not Value.More) and then
      (for all I in B.Limit =>
        (if I <= Value.Length then
           Value.Items (I).Valid and then Value.Items (I).Target = Surface
           and then Valid_Event (Value.Items (I)) and then
           Value.Items (I).Serial >
             (if I = 1 then After else Value.Items (I - 1).Serial)
         else Value.Items (I) = B.IQ.Event'(others => <>))));

   type Decoding (Accepted : Boolean := False) is record
      case Accepted is
         when True => Value : B.Batch;
         when False => null;
      end case;
   end record;

   function Decode
     (Wire : Snapshot_Words; Surface, Request : Identity; After : Word;
      Maximum : B.Limit := B.Capacity) return Decoding
   with Post => (if Decode'Result.Accepted then
     Valid (Decode'Result.Value, Surface, After, Maximum));

   function Encode
     (Value : B.Batch; Surface, Request : Identity; After : Word;
      Maximum : B.Limit := B.Capacity) return Snapshot_Words
   with Pre => Valid (Value, Surface, After, Maximum),
     Post => Encode'Result (0) = Magic and then
       Encode'Result (1) = Request and then Encode'Result (2) = Surface
       and then Encode'Result (3) = After
       and then Encode'Result (4) = Word (Value.Length)
       and then Encode'Result (5) = Value.Through
       and then Encode'Result (6) = (if Value.More then 1 else 0)
       and then Encode'Result (7) = 0
       and then (for all I in B.Limit =>
         Encode'Result (Header_Words + (I - 1) * Event_Words) = Value.Items (I).Kind
         and then Encode'Result (Header_Words + (I - 1) * Event_Words + 1) = Value.Items (I).Serial
         and then Encode'Result (Header_Words + (I - 1) * Event_Words + 2) = Value.Items (I).Payload0
         and then Encode'Result (Header_Words + (I - 1) * Event_Words + 3) = Value.Items (I).Payload1);
end Compositor_Input_Batch_Wire;
