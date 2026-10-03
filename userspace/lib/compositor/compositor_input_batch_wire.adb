with CuBit.Desktop_Protocol;
package body Compositor_Input_Batch_Wire with SPARK_Mode is
   package DP renames CuBit.Desktop_Protocol;
   use type DP.Status_Code;
   function Valid_Event (E : B.IQ.Event) return Boolean is
     (E.Kind /= 0 and then DP.Decode_Input_Result
       ((DP.Code (DP.Poll_Input), 4, 0, 0,
         [E.Kind, E.Serial, E.Payload0, E.Payload1]), DP.Poll_Input).Status = DP.Success);

   function Decode
     (Wire : Snapshot_Words; Surface, Request : Identity; After : Word;
      Maximum : B.Limit := B.Capacity) return Decoding
   is
      Value : B.Batch;
   begin
      if Wire (0) /= Magic or else Wire (1) /= Request or else
        Wire (2) /= Surface or else Wire (3) /= After or else
        Wire (4) > Word (Maximum) or else Wire (6) > 1 or else Wire (7) /= 0
      then
         return (Accepted => False);
      end if;
      Value.Length := B.Count (Wire (4));
      Value.Through := Wire (5);
      Value.More := Wire (6) = 1;
      for I in B.Limit loop
         declare
            Offset : constant Natural := Header_Words + (I - 1) * Event_Words;
         begin
            if I <= Value.Length then
               Value.Items (I) := (True, Wire (Offset + 1), Wire (Offset),
                 Surface, Wire (Offset + 2), Wire (Offset + 3));
            elsif Wire (Offset) /= 0 or else Wire (Offset + 1) /= 0 or else
              Wire (Offset + 2) /= 0 or else Wire (Offset + 3) /= 0
            then
               return (Accepted => False);
            end if;
         end;
      end loop;
      if not Valid (Value, Surface, After, Maximum) then
         return (Accepted => False);
      end if;
      return (True, Value);
   end Decode;

   function Encode
     (Value : B.Batch; Surface, Request : Identity; After : Word;
      Maximum : B.Limit := B.Capacity) return Snapshot_Words
   is
      Wire : Snapshot_Words := [others => 0];
   begin
      Wire (0 .. 7) := [Magic, Request, Surface, After, Word (Value.Length),
                       Value.Through, (if Value.More then 1 else 0), 0];
      for I in B.Limit loop
         declare
            Offset : constant Natural := Header_Words + (I - 1) * Event_Words;
         begin
            Wire (Offset .. Offset + 3) := [Value.Items (I).Kind,
              Value.Items (I).Serial, Value.Items (I).Payload0, Value.Items (I).Payload1];
         end;
         pragma Loop_Invariant
           (Wire (0) = Magic and Wire (1) = Request and Wire (2) = Surface
            and Wire (3) = After and Wire (4) = Word (Value.Length)
            and Wire (5) = Value.Through
            and Wire (6) = (if Value.More then 1 else 0) and Wire (7) = 0);
         pragma Loop_Invariant
           (for all J in B.Limit range 1 .. I =>
             Wire (Header_Words + (J - 1) * Event_Words) = Value.Items (J).Kind and then
             Wire (Header_Words + (J - 1) * Event_Words + 1) = Value.Items (J).Serial and then
             Wire (Header_Words + (J - 1) * Event_Words + 2) = Value.Items (J).Payload0 and then
             Wire (Header_Words + (J - 1) * Event_Words + 3) = Value.Items (J).Payload1);
      end loop;
      return Wire;
   end Encode;
end Compositor_Input_Batch_Wire;
