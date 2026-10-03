with CuBit.Desktop_Protocol;
with CuBit.Grant_References;
with Compositor_Input_Batch_Wire;

-- Desktop protocol extension, like the existing publication extension. The
-- endpoint authenticates sender identity; no authority is encoded here.
package Compositor_Input_Protocol with SPARK_Mode, Pure is
   package DP renames CuBit.Desktop_Protocol;
   package GR renames CuBit.Grant_References;
   package W renames Compositor_Input_Batch_Wire;
   use type W.Word;
   use type DP.Status_Code;
   Label : constant := 16#0823#;
   type Request is record
      Surface : W.Identity := 1;
      After : W.Word := 0;
      Grant : GR.Reference;
      Identity : W.Identity := 1;
   end record;
   type Request_Decoding (Accepted : Boolean := False) is record
      case Accepted is
         when True => Value : Request;
         when False => null;
      end case;
   end record;
   function Encode (Value : Request) return DP.Wire_Message;
   function Decode (Wire : DP.Wire_Message) return Request_Decoding;

   type Receipt (Status : DP.Status_Code := DP.Invalid_Request) is record
      case Status is
         when DP.Success =>
            Identity : W.Identity := 1;
            Length : W.B.Count := 0;
            Through : W.Word := 0;
            More : Boolean := False;
         when others => null;
      end case;
   end record;
   function Valid (Value : Receipt) return Boolean is
     (Value.Status /= DP.Success or else Value.Length > 0 or else not Value.More);
   function Encode (Value : Receipt) return DP.Wire_Message
     with Pre => Valid (Value);
   function Decode
     (Wire : DP.Wire_Message; Expected : W.Identity) return Receipt
     with Post => Valid (Decode'Result) and then
       (if Decode'Result.Status = DP.Success then Decode'Result.Identity = Expected);
end Compositor_Input_Protocol;
