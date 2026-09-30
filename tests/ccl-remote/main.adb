with Ada.Text_IO; use Ada.Text_IO;
with Interfaces;
with Control_HTTP; use Control_HTTP;
with Control_Wire;
with CBOR; use CBOR;
with CBOR.Encoding;
with CBOR.Decoding;
with CCL.Control;
with CCL.Sessions;

procedure Main is
   use type Interfaces.Unsigned_64;
   use type CBOR.SE_Offset;
   use type CBOR.Byte_Array;
   CRLF : constant String := ASCII.CR & ASCII.LF;
   Base : constant String := "POST /ccl HTTP/1.1" & CRLF &
     "Host: 127.0.0.1:18445" & CRLF & "Origin: " & Development_Origin & CRLF &
     "Content-Type: application/cbor" & CRLF;
   Complete : constant String := Base & "Content-Length: 1" & CRLF & CRLF & "x";
   R : Control_HTTP.Request;
   Q : Control_Wire.Request;
   Valid : Boolean;
   Session : CCL.Sessions.Session;
   Value : CCL.Control.Response;
   Encoded : Control_Wire.Response;
   function Query (Op : CBOR.UInt64; Text : String) return Byte_Array is
     (Encoding.Encode_Array (4) & Encoding.Encode_Unsigned (1) &
      Encoding.Encode_Unsigned (1) & Encoding.Encode_Unsigned (Op) & Encoding.Encode_Text_String (Text));
   procedure Reject (Text : String) is
   begin Parse (Text, R); pragma Assert (R.State = Rejected); end Reject;
begin
   for I in 0 .. Complete'Length - 1 loop
      Parse (Complete (1 .. I), R); pragma Assert (R.State = Incomplete);
   end loop;
   Parse (Complete, R);
   pragma Assert (R.State = Ready and R.Body_Length = 1 and R.Body_First = Complete'Last);
   Reject (Complete & "x");
   Reject (Base & "Content-Length: 1" & CRLF & "Content-Length: 1" & CRLF & CRLF & "x");
   Reject (Base & "Content-Length: 1101" & CRLF & CRLF);
   Reject (Base & "Content-Length: -1" & CRLF & CRLF);
   Reject (Base & "Content-Length: 1" & CRLF & "Transfer-Encoding: chunked" & CRLF & CRLF & "x");
   Reject (Base & "Content-Length: 1" & CRLF & "Origin: http://evil.example" & CRLF & CRLF & "x");
   Reject (Base & "Content-Length : 1" & CRLF & CRLF & "x");
   Reject (Base & "Content-Length: 1" & CRLF & " folded: bad" & CRLF & CRLF & "x");
   Reject (Base & "Content-Length: 1" & CRLF & "X-Test: a" & ASCII.LF & "b" & CRLF & CRLF & "x");
   Parse ("OPTIONS /ccl HTTP/1.1" & CRLF & "Host: 127.0.0.1:18445" & CRLF &
     "Origin: " & Development_Origin & CRLF & "Access-Control-Request-Method: POST" & CRLF &
     "Access-Control-Request-Headers: content-type" & CRLF & CRLF, R);
   pragma Assert (R.State = Ready and R.Verb = Preflight);
   Control_Wire.Decode (Query (2, "(+ 20 22)"), Q, Valid);
   pragma Assert (Valid and Q.Length = 9);
   CCL.Sessions.Initialize (Session);
   CCL.Control.Execute (Session, Q.Op, Q.Source (1 .. Q.Length), (others => <>), Value);
   pragma Assert (CCL.Sessions.Result_Image (Value.Outcome) = "Integer: 42");
   Control_Wire.Encode (Q, Value, Encoded);
   declare
      D : constant Decode_All_Result := Decoding.Decode_All_Strict (Encoded.Data (1 .. SE_Offset (Encoded.Length)));
   begin pragma Assert (D.Status = OK and D.Count = 9); end;
   --  A list result: typeCode 5, then its element type code and a definite
   --  array of elements (signed integers use CBOR major type 1).
   Control_Wire.Decode (Query (2, "[1 (- 0 2) 3]"), Q, Valid);
   pragma Assert (Valid);
   CCL.Control.Execute (Session, Q.Op, Q.Source (1 .. Q.Length), (others => <>), Value);
   pragma Assert (CCL.Sessions.Result_Image (Value.Outcome) = "List<Integer>: [1, -2, 3]");
   Control_Wire.Encode (Q, Value, Encoded);
   declare
      D : constant Decode_All_Result := Decoding.Decode_All_Strict (Encoded.Data (1 .. SE_Offset (Encoded.Length)));
   begin
      pragma Assert (D.Status = OK and D.Count = 15);
      pragma Assert (D.Items (1).Kind = MT_Array and D.Items (1).Arr_Count = 11);
      pragma Assert (D.Items (7).Kind = MT_Unsigned_Integer and D.Items (7).UInt_Value = 5);
      pragma Assert (D.Items (10).Kind = MT_Unsigned_Integer and D.Items (10).UInt_Value = 1);
      pragma Assert (D.Items (11).Kind = MT_Array and D.Items (11).Arr_Count = 3);
      pragma Assert (D.Items (13).Kind = MT_Negative_Integer and D.Items (13).NInt_Arg = 1);
      pragma Assert (D.Items (15).Kind = MT_Unsigned_Integer and D.Items (15).UInt_Value = 3);
   end;
   --  A long list carries its first 64 elements and its full length.
   Control_Wire.Decode (Query (3, "(range 1 100)"), Q, Valid);
   pragma Assert (Valid);
   CCL.Control.Execute (Session, Q.Op, Q.Source (1 .. Q.Length), (others => <>), Value);
   pragma Assert (Value.Outcome.List_Length = 64 and Value.Outcome.List_Total = 100);
   Control_Wire.Encode (Q, Value, Encoded);
   declare
      D : constant Decode_All_Result := Decoding.Decode_All_Strict (Encoded.Data (1 .. SE_Offset (Encoded.Length)));
   begin
      pragma Assert (D.Status = OK and D.Items (11).Arr_Count = 64);
      pragma Assert (D.Items (D.Count).Kind = MT_Unsigned_Integer and D.Items (D.Count).UInt_Value = 100);
   end;
   Put_Line ("PASS: list results as typed CBOR arrays");
   Control_Wire.Decode (Query (1, "not allowed"), Q, Valid); pragma Assert (not Valid);
   Control_Wire.Decode (Query (4, ""), Q, Valid); pragma Assert (not Valid);
   declare
      function Monitor_Query (Op, Target : CBOR.UInt64; Source : String := "") return Byte_Array is
        (Encoding.Encode_Array (5) & Encoding.Encode_Unsigned (1) &
         Encoding.Encode_Unsigned (1) & Encoding.Encode_Unsigned (Op) &
         Encoding.Encode_Text_String (Source) & Encoding.Encode_Unsigned (Target));
   begin
      Control_Wire.Decode (Monitor_Query (4, 0, "7"), Q, Valid); pragma Assert (Valid);
      Control_Wire.Decode (Monitor_Query (5, 1), Q, Valid); pragma Assert (Valid and Q.Target = 1);
      Control_Wire.Decode (Monitor_Query (6, 0), Q, Valid); pragma Assert (Valid);
      Control_Wire.Encode (Q, Value, Encoded);
      declare
         D : constant Decode_All_Result := Decoding.Decode_All_Strict (Encoded.Data (1 .. SE_Offset (Encoded.Length)));
      begin pragma Assert (D.Status = OK and D.Count = 16); end;
      Control_Wire.Decode (Monitor_Query (5, 0), Q, Valid); pragma Assert (not Valid);
      Control_Wire.Decode (Monitor_Query (4, 1, "7"), Q, Valid); pragma Assert (not Valid);
      Control_Wire.Decode (Monitor_Query (6, 0, "7"), Q, Valid); pragma Assert (not Valid);
      Control_Wire.Decode (Monitor_Query (2, 0, "7"), Q, Valid); pragma Assert (not Valid);
   end;
   Control_Wire.Decode (Query (2, "x") & Encoding.Encode_Unsigned (0), Q, Valid); pragma Assert (not Valid);
   Control_Wire.Decode (Query (2, "(type Color (enum Red)) Color.Red"), Q, Valid);
   pragma Assert (Valid);
   CCL.Control.Execute (Session, Q.Op, Q.Source (1 .. Q.Length), (others => <>), Value);
   pragma Assert (CCL.Sessions.Result_Image (Value.Outcome) = "Color.Red");
   Control_Wire.Encode (Q, Value, Encoded);
   pragma Assert (Encoded.Length = 0); -- no nominal-type erasure on the wire
   declare B : constant Byte_Array := Query (2, "(+ 20 22)"); begin
      for I in 0 .. B'Length - 1 loop
         Control_Wire.Decode (B (B'First .. B'First + SE_Offset (I) - 1), Q, Valid);
         pragma Assert (not Valid);
      end loop;
   end;
   Put_Line ("PASS: bounded HTTP framing, origin gates, strict CBOR and real CCL evaluation");
end Main;
