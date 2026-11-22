with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;
with Interfaces; use Interfaces;
with CBOR; use CBOR;
with CBOR.Encoding;
with CBOR.Decoding;
with Sample_Profile; use Sample_Profile;

procedure Main is
   use type CBOR.Byte_Array;
   use type CBOR.Byte;
   use type CBOR.SE_Offset;
   package E renames CBOR.Encoding;
   package D renames CBOR.Decoding;
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Name : String) is
   begin
      if not OK then
         Put_Line ("FAIL: " & Name);
         raise Program_Error with Name;
      end if;
      Checks := Checks + 1;
   end Check;

   function Envelope (Kind : Message_Kind; Fields : UInt64 := 4)
                      return Byte_Array is
     (E.Encode_Array (Fields) & E.Encode_Unsigned (1) &
      E.Encode_Unsigned (7) & E.Encode_Unsigned (Message_Kind'Enum_Rep (Kind)));

   Request : constant Byte_Array :=
     Envelope (Evaluate_Request) & E.Encode_Text_String ("(+ 20 22)");
   Diagnostic : constant Byte_Array :=
     Envelope (Evaluation_Error, 7) &
     E.Encode_Unsigned (Error_Kind'Enum_Rep (Type_Error)) &
     E.Encode_Unsigned (2) & E.Encode_Unsigned (5) &
     E.Encode_Text_String ("Expected Integer");
   Ref_Bytes : constant Byte_Array (1 .. Reference_Bytes) := [others => 16#AB#];
   Reference : constant Byte_Array :=
     Envelope (Endpoint_Reference) & E.Encode_Byte_String (Ref_Bytes);
   Result : constant Byte_Array :=
     Envelope (Unsigned_Result) & E.Encode_Unsigned (UInt64'Last);

   procedure Show (Name : String; Data : Byte_Array; Meaning : String) is
      Hex : constant String := "0123456789ABCDEF";
   begin
      Put_Line (Name & ":" & Data'Length'Image & " bytes; " & Meaning);
      Put ("  wire:");
      for B of Data loop
         Put (' ' & Hex (Natural (B / 16) + 1) & Hex (Natural (B mod 16) + 1));
      end loop;
      New_Line;
   end Show;

   procedure Expect_Status
     (Data : Byte_Array; Expected : Decode_Status; Name : String;
      Max_Depth : Natural := 16; Max_String : SE_Offset := 256) is
      R : constant Decode_All_Result := D.Decode_All_Strict
        (Data, Max_Depth => Max_Depth, Max_String_Len => Max_String);
   begin
      Check (R.Status = Expected, Name & " got " & R.Status'Image);
   end Expect_Status;

   procedure Check_Round_Trip (Data : Byte_Array) is
      R : constant Decode_All_Result := D.Decode_All_Strict (Data);
      Last : CBOR_Item;
   begin
      Check (R.Status = CBOR.OK, "sample tree decodes");
      Check (Sample_Profile.Valid (Data), "sample schema accepts");
      Check (R.Items (3).UInt_Value = 7, "request ID preserved");
      Last := R.Items (R.Count);
      case Last.Kind is
         when MT_Unsigned_Integer =>
            Check (Last.UInt_Value = UInt64'Last, "full unsigned64 preserved");
         when MT_Text_String =>
            declare
               Payload : constant Byte_Array := D.Get_String (Data, Last.TS_Ref);
            begin
               Check (Payload'Length > 0, "text reference valid");
               Check (E.Encode_Text_String_UTF8 (Payload) =
                      Data (Last.Head_Start .. Last.Item_End), "text bytes round trip");
            end;
         when MT_Byte_String =>
            Check (D.Get_String (Data, Last.BS_Ref) = Ref_Bytes,
                   "opaque reference bytes preserved, not authorized");
         when others =>
            Check (False, "unexpected sample payload");
      end case;
      for N in SE_Offset range 0 .. Data'Length - 1 loop
         Check (not Sample_Profile.Valid (Data (Data'First .. Data'First + N - 1)),
                "every truncated sample rejected");
      end loop;
      Check (not Sample_Profile.Valid (Data & E.Encode_Null), "trailing data rejected");
      declare
         Rebased : Byte_Array (100 .. 99 + Data'Length) := Data;
      begin
         Check (Sample_Profile.Valid (Rebased), "non-one positive lower bound");
      end;
   end Check_Round_Trip;
begin
   Put_Line ("CCL CBOR: Linux-hosted experiment; no live network or authority grants");
   Show ("Evaluate", Request, "(evaluate request:7 source:""(+ 20 22)"")");
   Show ("Diagnostic", Diagnostic, "(type-error request:7 line:2 column:5 ""Expected Integer"")");
   Show ("Endpoint", Reference, "(endpoint-reference request:7 <opaque, UNRESOLVED>)");
   Show ("Result", Result, "(unsigned64 request:7 18446744073709551615)");
   Check (Request = Byte_Array'(1 => 16#84#, 2 => 1, 3 => 7, 4 => 1,
     5 => 16#69#, 6 => 16#28#, 7 => 16#2B#, 8 => 16#20#, 9 => 16#32#,
     10 => 16#30#, 11 => 16#20#, 12 => 16#32#, 13 => 16#32#, 14 => 16#29#),
     "independent golden evaluation bytes");
   Check_Round_Trip (Request); Check_Round_Trip (Diagnostic);
   Check_Round_Trip (Reference); Check_Round_Trip (Result);

   Expect_Status ([1 => 16#1B#], Err_Truncated, "truncated uint64");
   Expect_Status ([1 => 16#18#, 2 => 16#17#], Err_Not_Well_Formed, "non-shortest integer");
   Expect_Status ([1 => 16#1C#], Err_Not_Well_Formed, "reserved additional information");
   Expect_Status ([1 => 16#FF#], Err_Not_Well_Formed, "standalone break");
   Expect_Status ([1 => 16#62#, 2 => 16#C0#, 3 => 16#80#], Err_Invalid_UTF8, "overlong UTF8");
   Expect_Status ([1 => 16#63#, 2 => 16#ED#, 3 => 16#A0#, 4 => 16#80#],
                  Err_Invalid_UTF8, "UTF8 surrogate");
   Expect_Status ([1 => 16#82#, 2 => 1], Err_Truncated, "incomplete array");
   Check (D.Decode (Byte_Array'(1 => 16#82#, 2 => 1)).Status = CBOR.OK,
          "single-item decoder validates header only");
   Expect_Status ([1 .. 17 => 16#81#, 18 => 0], Err_Depth_Exceeded, "depth bound");
   Expect_Status (E.Encode_Array (128) & Byte_Array'(1 .. 128 => 0),
                  Err_Too_Many_Items, "item bound includes container");
   Expect_Status (E.Encode_Text_String (String'(1 .. 129 => 'x')),
                  Err_String_Too_Long, "string bound", Max_String => 128);
   Expect_Status (E.Encode_Map (UInt64'Last), Err_Resource_Limit, "huge declared map");

   -- Valid CBOR does not imply a valid CuBit message or canonical encoding.
   Expect_Status ([1 => 16#A2#, 2 => 1, 3 => 1, 4 => 1, 5 => 2],
                  CBOR.OK, "duplicate keys not rejected by generic decoder");
   Check (not Sample_Profile.Valid
          (Byte_Array'(1 => 16#A2#, 2 => 1, 3 => 1, 4 => 1, 5 => 2)),
          "profile excludes maps including duplicate keys");
   Check (not Sample_Profile.Valid (E.Encode_Array_Start &
          Request (Request'First + 1 .. Request'Last) & E.Encode_Break),
          "indefinite arrays excluded");
   Check (not Sample_Profile.Valid (Envelope (Unsigned_Result) & E.Encode_Bool (True)),
          "wrong payload type rejected");
   Check (not Sample_Profile.Valid (Envelope (Unsigned_Result) &
          E.Encode_Float_Single (Byte_Array'(1 => 16#3F#, 2 => 16#80#, 3 => 0, 4 => 0))),
          "float is not unsigned integer");
   Check (not Sample_Profile.Valid (Envelope (Endpoint_Reference) &
          E.Encode_Byte_String (Byte_Array'(1 .. 15 => 0))), "reference size bound");
   Check (not Sample_Profile.Valid (Envelope (Evaluation_Error, 7) &
          E.Encode_Unsigned (2) & E.Encode_Unsigned (0) & E.Encode_Unsigned (5) &
          E.Encode_Text_String ("error")), "source positions are one-based");
   Check (not Sample_Profile.Valid (Envelope (Evaluation_Error, 7) &
          E.Encode_Unsigned (2) & E.Encode_Unsigned (1) & E.Encode_Unsigned (5) &
          E.Encode_Text_String (String'(1 .. 129 => 'x'))), "diagnostic size bound");
   Check (not Sample_Profile.Valid (E.Encode_Tag (1) & Request), "CBOR tag excluded");
   Check (not Sample_Profile.Valid (Envelope (Evaluate_Request) &
          E.Encode_Text_String (String'(1 .. 257 => 'x'))), "source bound");
   Check (not Sample_Profile.Valid (Byte_Array'(1 .. 513 => 0)), "frame size bound");
   Check (not Sample_Profile.Valid (Byte_Array'(-2 .. -1 => 0)), "negative input bounds rejected");
   Check (not Sample_Profile.Valid (Byte_Array'(1 .. 0 => 0)), "empty frame rejected");
   declare
      Bad : Byte_Array (Request'Range) := Request;
   begin
      Bad (2) := 2;
      Check (not Sample_Profile.Valid (Bad), "unknown version rejected");
      Bad := Request; Bad (3) := 0;
      Check (not Sample_Profile.Valid (Bad), "zero request ID rejected");
      Bad := Request; Bad (4) := 5;
      Check (not Sample_Profile.Valid (Bad), "unknown operation rejected");
   end;
   declare
      UTF8 : constant Byte_Array (1 .. 3) := [16#E2#, 16#98#, 16#83#];
      Latin1 : constant String := [1 => Character'Val (16#E9#)];
   begin
      Check (Sample_Profile.Valid (Envelope (Evaluate_Request) &
             E.Encode_Text_String_UTF8 (UTF8)), "UTF8 source preserved");
      Check (not Sample_Profile.Valid (Envelope (Evaluate_Request) &
             E.Encode_Text_String (Latin1)), "Latin1 text encoder is not UTF8 conversion");
   end;

   -- Exhaust every one- and two-byte input with all contracts enabled.
   -- Then deterministic mutations of real frames (not a coverage-guided fuzzer).
   declare
      Bytes : Byte_Array (1 .. 2);
      R : Decode_All_Result;
      Seed : Unsigned_32 := 16#C0B0_2026#;
      Packet : Byte_Array (1 .. 64);
      Accepted : Natural := 0;
   begin
      for A in Byte loop
         Bytes (1) := A;
         R := D.Decode_All_Strict (Bytes (1 .. 1), Max_Depth => 4, Max_String_Len => 32);
         Check (not Sample_Profile.Valid (Bytes (1 .. 1)), "one-byte frame rejected");
         for B in Byte loop
            Bytes (2) := B;
            R := D.Decode_All_Strict (Bytes, Max_Depth => 4, Max_String_Len => 32);
            Check (not Sample_Profile.Valid (Bytes), "two-byte frame rejected");
         end loop;
      end loop;
      for Trial in 1 .. 10_000 loop
         for B of Packet loop
            Seed := Seed xor Shift_Left (Seed, 13);
            Seed := Seed xor Shift_Right (Seed, 17);
            Seed := Seed xor Shift_Left (Seed, 5);
            B := Byte (Seed and 255);
         end loop;
         R := D.Decode_All_Strict (Packet, Max_Depth => 4, Max_String_Len => 32);
         if Sample_Profile.Valid (Packet) then Accepted := Accepted + 1; end if;
      end loop;
      Check (Accepted = 0, "deterministic random corpus has no valid sample frames");
      for Position in Request'Range loop
         for B in Byte loop
            declare
               Mutated : Byte_Array (Request'Range) := Request;
            begin
               Mutated (Position) := B;
               if Sample_Profile.Valid (Mutated) then
                  R := D.Decode_All_Strict (Mutated);
                  Check (R.Status = CBOR.OK, "accepted mutation remains well-formed");
               end if;
            end;
         end loop;
      end loop;
      Put_Line ("Malformed corpus: 256 single bytes, 65536 pairs, 10000 random frames, 3584 mutations");
   end;
   declare
      Start : constant Time := Clock;
      Accepted : Natural := 0;
      Iterations : constant := 20_000;
   begin
      for I in 1 .. Iterations loop
         if Sample_Profile.Valid (Request) then Accepted := Accepted + 1; end if;
      end loop;
      Check (Accepted = Iterations, "measurement results consumed");
      Put_Line ("Hosted schema validation us/message: " &
        Duration'Image (To_Duration (Clock - Start) * 1_000_000 / Iterations));
      Put_Line ("Decode_All_Result bytes: " & Integer'Image (Decode_All_Result'Size / 8));
   end;
   Put_Line ("PASS CCL CBOR evaluation; checks:" & Checks'Image);
end Main;
