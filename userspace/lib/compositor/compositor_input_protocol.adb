with Interfaces;
package body Compositor_Input_Protocol with SPARK_Mode is
   use type Interfaces.Unsigned_8;
   use type Interfaces.Unsigned_16;
   use type Interfaces.Unsigned_32;
   function Encode (Value : Request) return DP.Wire_Message is
     (Label, 4, 0, 0, [Value.Surface, Value.After, GR.Encode (Value.Grant), Value.Identity]);
   function Decode (Wire : DP.Wire_Message) return Request_Decoding is
   begin
      if Wire.Label /= Label or else Wire.Length /= 4 or else Wire.Flags /= 0
        or else Wire.Reserved /= 0 or else Wire.Words (0) = 0
        or else Wire.Words (3) = 0 or else not GR.Valid_Wire (Wire.Words (2))
      then return (Accepted => False); end if;
      return (True, (Wire.Words (0), Wire.Words (1), GR.Decode (Wire.Words (2)), Wire.Words (3)));
   end Decode;
   function Encode (Value : Receipt) return DP.Wire_Message is
     (if Value.Status = DP.Success then
        (Label, 4, (if Value.More then 1 else 0), 0,
         [0, Value.Identity, W.Word (Value.Length), Value.Through])
      else (Label, 1, 0, 0, [DP.Status_Code'Enum_Rep (Value.Status), 0, 0, 0]));
   function Decode
     (Wire : DP.Wire_Message; Expected : W.Identity) return Receipt is
   begin
      if Wire.Label /= Label or else Wire.Reserved /= 0 then
         return (Status => DP.Invalid_Request);
      end if;
      if Wire.Length = 1 and then Wire.Flags = 0 and then
        Wire.Words (1) = 0 and then Wire.Words (2) = 0 and then Wire.Words (3) = 0
      then
         for Status in DP.Status_Code range DP.Denied .. DP.Resources_Exhausted loop
            if Wire.Words (0) = DP.Status_Code'Enum_Rep (Status) then return (Status => Status); end if;
         end loop;
      elsif Wire.Length = 4 and then Wire.Flags <= 1 and then
        Wire.Words (0) = 0 and then Wire.Words (1) = Expected and then
        Wire.Words (2) <= W.B.Capacity and then
        (Wire.Words (2) > 0 or else Wire.Flags = 0)
      then
         return (DP.Success, Expected, W.B.Count (Wire.Words (2)), Wire.Words (3), Wire.Flags = 1);
      end if;
      return (Status => DP.Invalid_Request);
   end Decode;
end Compositor_Input_Protocol;
