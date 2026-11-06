pragma Ada_2022;
package body Llm_Responses with SPARK_Mode is
   use ASCII;
   use type P.Error_Kind;

   CR_Byte : constant Byte := Character'Pos (CR);
   LF_Byte : constant Byte := Character'Pos (LF);
   HT_Byte : constant Byte := Character'Pos (HT);
   Space_Byte : constant Byte := Character'Pos (' ');
   First_Printable : constant Byte := 16#20#;
   DEL_Byte : constant Byte := 16#7F#;
   Http_Prefix : constant String := "HTTP/";
   Status_OK : constant := 200;
   Lowest_Status : constant := 100;
   Highest_Status : constant := 599;
   Replacement : constant := 16#FFFD#;
   First_High_Surrogate : constant := 16#D800#;
   Last_High_Surrogate : constant := 16#DBFF#;
   First_Low_Surrogate : constant := 16#DC00#;
   Last_Low_Surrogate : constant := 16#DFFF#;
   Supplementary_Base : constant := 16#1_0000#;
   Surrogate_Bits : constant := 2**10;
   Maximum_Content_Length : constant := 2**48;

   function Char (B : Byte) return Character is (Character'Val (B));

   Zero_Byte : constant Byte := Character'Pos ('0');
   Nine_Byte : constant Byte := Character'Pos ('9');
   --  A decimal digit's value (0 for anything else).
   function Digit_Of (B : Byte) return Unsigned_64 is
     (if B in Zero_Byte .. Nine_Byte then Unsigned_64 (B - Zero_Byte) else 0);

   function Lower (C : Character) return Character is
     (if C in 'A' .. 'Z' then Character'Val (Character'Pos (C) + 32) else C);

   function Hex_Value (B : Byte) return Integer is
     (case Char (B) is
         when '0' .. '9' => Integer (B) - Character'Pos ('0'),
         when 'a' .. 'f' => Integer (B) - Character'Pos ('a') + 10,
         when 'A' .. 'F' => Integer (B) - Character'Pos ('A') + 10,
         when others => -1);

   ---------------------------------------------------------------------------
   --  Ending.
   ---------------------------------------------------------------------------
   procedure Finish (D : in out Decoder; Reason : P.Stop_Reason) is
   begin
      if not D.S.Done then
         D.S.Done := True;
         D.S.Result := Reason;
      end if;
   end Finish;

   procedure Fail (D : in out Decoder; Why : P.Problem) is
   begin
      if not D.S.Done then
         D.S.Fault := Why;
         Finish (D, P.Malformed_Response);
      end if;
   end Fail;

   function Start (Limit : Limits) return Decoder is
   begin
      return Result : Decoder (Capacity_For (Limit)) do
         Begin_Response (Result, Limit);
      end return;
   end Start;

   procedure Begin_Response (D : in out Decoder; Limit : Limits) is
   begin
      D.S := (Limit => Limit,
              Body_Limit => Response_Overhead_Bytes + Unsigned_64 (Limit.Max_Tokens) * Response_Bytes_Per_Token,
              others => <>);
   end Begin_Response;

   procedure Stop_With (D : in out Decoder; Reason : P.Stop_Reason) is
   begin
      Finish (D, Reason);
   end Stop_With;

   ---------------------------------------------------------------------------
   --  Classification of names.
   ---------------------------------------------------------------------------
   function Key_Of (Text : String) return Key_Id is
     (if Text = "type" then K_Type
      elsif Text = "message" then K_Message
      elsif Text = "model" then K_Model
      elsif Text = "usage" then K_Usage
      elsif Text = "input_tokens" then K_Input_Tokens
      elsif Text = "output_tokens" then K_Output_Tokens
      elsif Text = "cache_creation_input_tokens" then K_Cache_Creation
      elsif Text = "cache_read_input_tokens" then K_Cache_Read
      elsif Text = "content_block" then K_Content_Block
      elsif Text = "delta" then K_Delta
      elsif Text = "text" then K_Text
      elsif Text = "stop_reason" then K_Stop_Reason
      elsif Text = "error" then K_Error
      elsif Text = "id" then K_Id
      elsif Text = "name" then K_Name
      elsif Text = "partial_json" then K_Partial_Json
      elsif Text = "thinking" then K_Thinking
      elsif Text = "signature" then K_Signature
      elsif Text = "data" then K_Data
      else K_Other);

   function Block_Type_Of (Text : String) return Block_Type is
     (if Text = "text" then Text_Type
      elsif Text = "thinking" then Thinking_Type
      elsif Text = "redacted_thinking" then Redacted_Type
      elsif Text = "tool_use" then Tool_Use_Type
      else Unknown_Block);

   function Event_Of (Text : String) return Event_Kind is
     (if Text = "message_start" then Message_Start
      elsif Text = "content_block_start" then Content_Block_Start
      elsif Text = "content_block_delta" then Content_Block_Delta
      elsif Text = "content_block_stop" then Content_Block_Stop
      elsif Text = "message_delta" then Message_Delta
      elsif Text = "message_stop" then Message_Stop
      elsif Text = "ping" then Ping
      elsif Text = "error" then Error_Event
      else Unknown_Event);

   function Stop_Of_Text (Text : String) return P.Stop_Reason is
     (if Text = "end_turn" then P.End_Turn
      elsif Text = "max_tokens" then P.Max_Tokens
      elsif Text = "stop_sequence" then P.Stop_Sequence
      elsif Text = "tool_use" then P.Tool_Use
      elsif Text = "pause_turn" then P.Pause_Turn
      elsif Text = "refusal" then P.Refusal_Stop
      elsif Text = "model_context_window_exceeded" then P.Context_Window_Exceeded
      else P.Unknown_Stop_Reason);

   function Error_Of_Text (Text : String) return P.Error_Kind is
     (if Text = "invalid_request_error" then P.Invalid_Request
      elsif Text = "authentication_error" then P.Authentication
      elsif Text = "billing_error" then P.Billing
      elsif Text = "permission_error" then P.Permission
      elsif Text = "not_found_error" then P.Not_Found
      elsif Text = "request_too_large" then P.Request_Too_Large
      elsif Text = "rate_limit_error" then P.Rate_Limited
      elsif Text = "api_error" then P.Api_Internal
      elsif Text = "overloaded_error" then P.Overloaded
      elsif Text = "timeout_error" then P.Gateway_Timeout
      else P.Other_Error);

   function Error_Of_Status (Status : Natural) return P.Error_Kind is
     (case Status is
         when 400 => P.Invalid_Request,
         when 401 => P.Authentication,
         when 402 => P.Billing,
         when 403 => P.Permission,
         when 404 => P.Not_Found,
         when 413 => P.Request_Too_Large,
         when 429 => P.Rate_Limited,
         when 500 => P.Api_Internal,
         when 504 => P.Gateway_Timeout,
         when 529 => P.Overloaded,
         when others => P.Other_Error);

   --  Where a value at the current depth goes, from the event and the
   --  object keys above it.
   function Goal_Of (D : Decoder) return Target is
      K1 : constant Key_Id := (if D.S.Depth >= 1 then D.S.Keys (1) else K_Other);
      K2 : constant Key_Id := (if D.S.Depth >= 2 then D.S.Keys (2) else K_Other);
      K3 : constant Key_Id := (if D.S.Depth >= 3 then D.S.Keys (3) else K_Other);
      function Usage_Goal (K : Key_Id) return Target is
        (case K is
            when K_Input_Tokens => T_Input,
            when K_Output_Tokens => T_Output,
            when K_Cache_Creation => T_Cache_Write,
            when K_Cache_Read => T_Cache_Read,
            when others => T_None);
   begin
      if D.S.Depth = 0 or else D.S.Stack (D.S.Depth) /= In_Object then
         return T_None;
      elsif D.S.Depth = 1 then
         return (if K1 = K_Type then T_Type else T_None);
      end if;
      case D.S.Event is
         when Message_Start =>
            if D.S.Depth = 2 and then K1 = K_Message and then K2 = K_Model then
               return T_Model;
            elsif D.S.Depth = 3 and then K1 = K_Message and then K2 = K_Usage then
               return Usage_Goal (K3);
            end if;
         when Content_Block_Start =>
            if D.S.Depth = 2 and then K1 = K_Content_Block then
               case K2 is
                  when K_Text => return T_Text;
                  when K_Type => return T_Block_Type;
                  when K_Id => return T_Tool_Id;
                  when K_Name => return T_Tool_Name;
                  when K_Thinking => return T_Thinking;
                  when K_Signature => return T_Signature;
                  when K_Data => return T_Redacted;
                  when others => null;
               end case;
            end if;
         when Content_Block_Delta =>
            if D.S.Depth = 2 and then K1 = K_Delta then
               case K2 is
                  when K_Text => return T_Text;
                  when K_Partial_Json => return T_Tool_Input;
                  when K_Thinking => return T_Thinking;
                  when K_Signature => return T_Signature;
                  when others => null;
               end case;
            end if;
         when Message_Delta =>
            if D.S.Depth = 2 and then K1 = K_Delta and then K2 = K_Stop_Reason then
               return T_Stop;
            elsif D.S.Depth = 2 and then K1 = K_Usage then
               return Usage_Goal (K2);
            end if;
         when Error_Event =>
            if D.S.Depth = 2 and then K1 = K_Error and then K2 = K_Type then
               return T_Error_Type;
            elsif D.S.Depth = 2 and then K1 = K_Error and then K2 = K_Message then
               return T_Error_Message;
            end if;
         when others =>
            null;
      end case;
      return T_None;
   end Goal_Of;

   ---------------------------------------------------------------------------
   --  JSON.
   ---------------------------------------------------------------------------
   procedure Json_Reset (D : in out Decoder) is
   begin
      D.S.Depth := 0;
      D.S.Keys := [others => K_Other];
      D.S.Expect := Expect_Value;
      D.S.Lex := No_Lexeme;
      D.S.High_Surrogate := 0;
      D.S.Goal := T_None;
      D.S.Type_Seen := False;
      D.S.Type_Matches := False;
      D.S.Document_Broken := False;
   end Json_Reset;

   procedure Json_Fail (D : in out Decoder; Why : P.Problem) is
   begin
      if D.S.Error_Document then
         --  An error response's body only explains its status.
         D.S.Document_Broken := True;
      else
         Fail (D, Why);
      end if;
   end Json_Fail;

   ---------------------------------------------------------------------------
   --  Capture.
   ---------------------------------------------------------------------------
   procedure Put_Content_Byte (D : in out Decoder; Value : Byte) is
   begin
      if D.S.Content_End >= D.Capacity then
         Fail (D, P.Content_Too_Large);
         return;
      end if;
      D.S.Content_End := D.S.Content_End + 1;
      D.Content (D.S.Content_End) := Value;
   end Put_Content_Byte;

   --  A block's object opened (content_block_start): reserve its header.
   procedure Content_Begin (D : in out Decoder) is
   begin
      if not D.S.Limit.Capture or else D.S.Done then
         return;
      end if;
      D.S.Content_End := D.S.Content_Done;
      if D.Capacity - D.S.Content_End < P.Block_Header_Bytes then
         Fail (D, P.Content_Too_Large);
         return;
      end if;
      D.S.Block_Head := D.S.Content_End;
      D.S.Content_End := D.S.Content_End + P.Block_Header_Bytes;
      D.S.Block_Open := True;
      D.S.Block_Kind := Unknown_Block;
      D.S.Field_At := Field_A;
      D.S.Lengths := [others => 0];
      D.S.Tool_Id_Length := 0;
      D.S.Tool_Name_Length := 0;
   end Content_Begin;

   procedure Content_Put (D : in out Decoder; Field : Content_Field; Value : Byte) is
   begin
      if not D.S.Block_Open or else D.S.Done then
         return;
      elsif Field < D.S.Field_At then
         Fail (D, P.Bad_Content_Block);
         return;
      end if;
      D.S.Field_At := Field;
      Put_Content_Byte (D, Value);
      if not D.S.Done and then D.S.Lengths (Field) < Content_Count'Last then
         D.S.Lengths (Field) := D.S.Lengths (Field) + 1;
      end if;
   end Content_Put;

   procedure Content_Put_Text (D : in out Decoder; Field : Content_Field; Text : String) is
   begin
      for C of Text loop
         Content_Put (D, Field, Character'Pos (C));
      end loop;
   end Content_Put_Text;

   procedure Put_Header_Word (D : in out Decoder; Offset : Natural; Value : Natural; Width : Positive)
   with Pre => Offset <= P.Block_Header_Bytes and then Width <= P.Block_Header_Bytes - Offset
   is
      V : Natural := Value;
   begin
      if D.S.Block_Head > D.Capacity - P.Block_Header_Bytes then
         return;
      end if;
      for K in 1 .. Width loop
         D.Content (D.S.Block_Head + Offset + K) := Byte (V mod 256);
         V := V / 256;
      end loop;
   end Put_Header_Word;

   function Kind_Code (Kind : Block_Type) return Natural is
     (case Kind is
         when Text_Type => P.Block_Kind'Enum_Rep (P.Text_Block),
         when Thinking_Type => P.Block_Kind'Enum_Rep (P.Thinking_Block),
         when Redacted_Type => P.Block_Kind'Enum_Rep (P.Redacted_Block),
         when Tool_Use_Type | Unknown_Block => P.Block_Kind'Enum_Rep (P.Tool_Call_Block));

   function Name_Character (C : Character) return Boolean is
     (C in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-');

   --  content_block_start dispatched: the block's kind is known. A tool
   --  call's id and name become its first two fields.
   procedure Content_Started (D : in out Decoder) is
   begin
      if not D.S.Block_Open or else D.S.Done then
         return;
      end if;
      case D.S.Block_Kind is
         when Unknown_Block =>
            D.S.Block_Open := False;
            D.S.Content_End := D.S.Content_Done;
         when Tool_Use_Type =>
            if D.S.Tool_Id_Length = 0 or else D.S.Tool_Name_Length = 0
              or else (for some F in Content_Field => D.S.Lengths (F) /= 0)
              or else (for some I in 1 .. D.S.Tool_Id_Length => not Name_Character (D.S.Tool_Id (I)))
              or else (for some I in 1 .. D.S.Tool_Name_Length => not Name_Character (D.S.Tool_Name (I)))
            then
               Fail (D, P.Bad_Content_Block);
               return;
            end if;
            declare
               Id : constant String := D.S.Tool_Id (1 .. D.S.Tool_Id_Length);
               Name : constant String := D.S.Tool_Name (1 .. D.S.Tool_Name_Length);
            begin
               Content_Put_Text (D, Field_A, Id);
               Content_Put_Text (D, Field_B, Name);
            end;
            D.S.Field_At := Field_C;
         when Text_Type | Thinking_Type | Redacted_Type =>
            null;
      end case;
   end Content_Started;

   --  content_block_stop: the block is whole (or dropped, if it is empty
   --  text). A tool call with no input deltas has the empty object.
   procedure Content_Stopped (D : in out Decoder) is
   begin
      if not D.S.Block_Open or else D.S.Done then
         return;
      end if;
      if D.S.Block_Kind = Text_Type and then D.S.Lengths (Field_A) = 0 then
         D.S.Block_Open := False;
         D.S.Content_End := D.S.Content_Done;
         return;
      end if;
      if D.S.Block_Kind = Tool_Use_Type and then D.S.Lengths (Field_C) = 0 then
         Content_Put_Text (D, Field_C, "{}");
         if D.S.Done then
            return;
         end if;
      end if;
      D.S.Block_Open := False;
      Put_Header_Word (D, P.Block_Role_Offset, P.Role'Enum_Rep (P.Assistant), 1);
      Put_Header_Word (D, P.Block_Kind_Offset, Kind_Code (D.S.Block_Kind), 1);
      Put_Header_Word (D, P.Block_Error_Offset, 0, 2);
      Put_Header_Word (D, P.Block_A_Length_Offset, D.S.Lengths (Field_A), 4);
      Put_Header_Word (D, P.Block_B_Length_Offset, D.S.Lengths (Field_B), 4);
      Put_Header_Word (D, P.Block_C_Length_Offset, D.S.Lengths (Field_C), 4);
      D.S.Content_Done := D.S.Content_End;
      if D.S.Block_Kind = Tool_Use_Type and then D.S.Tool_Calls < Natural'Last then
         D.S.Tool_Calls := D.S.Tool_Calls + 1;
      end if;
   end Content_Stopped;

   procedure Put_Text (D : in out Decoder; B : Byte; Text : in out Output) is
   begin
      case D.S.Goal is
         when T_Text =>
            if Text.Count < Maximum_Output then
               Text.Count := Text.Count + 1;
               Text.Bytes (Text.Count) := B;
               if D.S.Text_Bytes < Unsigned_64'Last then
                  D.S.Text_Bytes := D.S.Text_Bytes + 1;
               end if;
            end if;
            Content_Put (D, Field_A, B);
         when T_Thinking | T_Redacted =>
            Content_Put (D, Field_A, B);
         when T_Signature =>
            Content_Put (D, Field_B, B);
         when T_Tool_Input =>
            Content_Put (D, Field_C, B);
         when T_Error_Message =>
            if D.S.Detail_Length < P.Maximum_Detail_Bytes then
               D.S.Detail_Length := D.S.Detail_Length + 1;
               D.S.Detail (D.S.Detail_Length) := Char (B);
            end if;
         when T_None | Number_Target =>
            null;
         when Capture_Target =>
            if D.S.Capture_Length < Maximum_Capture then
               D.S.Capture_Length := D.S.Capture_Length + 1;
               D.S.Capture (D.S.Capture_Length) := Char (B);
            else
               D.S.Capture_Over := True;
            end if;
      end case;
   end Put_Text;

   --  One byte of a string's content (after unescaping).
   procedure String_Content (D : in out Decoder; B : Byte; Text : in out Output) is
   begin
      if D.S.Is_Key then
         if D.S.Key_Length < Maximum_Key then
            D.S.Key_Length := D.S.Key_Length + 1;
            D.S.Key (D.S.Key_Length) := Char (B);
         else
            D.S.Key_Over := True;
         end if;
      else
         Put_Text (D, B, Text);
      end if;
   end String_Content;

   procedure Put_Code_Point
     (D : in out Decoder; Point : Natural; Text : in out Output)
   with Pre => Point <= 16#10_FFFF#
   is
   begin
      if Point < 16#80# then
         String_Content (D, Byte (Point), Text);
      elsif Point < 16#800# then
         String_Content (D, Byte (16#C0# + Point / 16#40#), Text);
         String_Content (D, Byte (16#80# + Point mod 16#40#), Text);
      elsif Point < 16#1_0000# then
         String_Content (D, Byte (16#E0# + Point / 16#1000#), Text);
         String_Content (D, Byte (16#80# + (Point / 16#40#) mod 16#40#), Text);
         String_Content (D, Byte (16#80# + Point mod 16#40#), Text);
      else
         String_Content (D, Byte (16#F0# + Point / 16#4_0000#), Text);
         String_Content (D, Byte (16#80# + (Point / 16#1000#) mod 16#40#), Text);
         String_Content (D, Byte (16#80# + (Point / 16#40#) mod 16#40#), Text);
         String_Content (D, Byte (16#80# + Point mod 16#40#), Text);
      end if;
   end Put_Code_Point;

   --  A lone high surrogate becomes U+FFFD before whatever follows it.
   procedure Flush_Surrogate (D : in out Decoder; Text : in out Output) is
   begin
      if D.S.High_Surrogate /= 0 then
         D.S.High_Surrogate := 0;
         Put_Code_Point (D, Replacement, Text);
      end if;
   end Flush_Surrogate;

   procedure Unit (D : in out Decoder; U : Code_Unit; Text : in out Output) is
   begin
      if U in First_High_Surrogate .. Last_High_Surrogate then
         Flush_Surrogate (D, Text);
         D.S.High_Surrogate := U;
      elsif U in First_Low_Surrogate .. Last_Low_Surrogate then
         if D.S.High_Surrogate in First_High_Surrogate .. Last_High_Surrogate then
            declare
               Point : constant Natural :=
                 Supplementary_Base + (D.S.High_Surrogate - First_High_Surrogate) * Surrogate_Bits +
                 (U - First_Low_Surrogate);
            begin
               D.S.High_Surrogate := 0;
               Put_Code_Point (D, Point, Text);
            end;
         else
            Put_Code_Point (D, Replacement, Text);
         end if;
      else
         Flush_Surrogate (D, Text);
         Put_Code_Point (D, U, Text);
      end if;
   end Unit;

   procedure Value_Complete (D : in out Decoder) is
   begin
      D.S.Goal := T_None;
      D.S.Lex := No_Lexeme;
      D.S.Expect := (if D.S.Depth = 0 then Expect_Nothing else Expect_Comma_Or_Close);
   end Value_Complete;

   --  A string value ended: keep what the client needs from it.
   procedure String_Value_End (D : in out Decoder) is
      Captured : constant String := D.S.Capture (1 .. D.S.Capture_Length);
   begin
      case D.S.Goal is
         when Capture_Target =>
            if D.S.Capture_Over then
               Json_Fail (D, P.Field_Too_Long);
               return;
            end if;
         when others =>
            null;
      end case;
      case D.S.Goal is
         when T_Type =>
            D.S.Type_Seen := True;
            D.S.Type_Matches :=
              (if D.S.Error_Document then Captured = "error"
               else Captured = D.S.Event_Name (1 .. D.S.Event_Name_Length));
         when T_Model =>
            D.S.Model := D.S.Capture;
            D.S.Model_Length := D.S.Capture_Length;
         when T_Stop =>
            D.S.Server_Stop := Stop_Of_Text (Captured);
            D.S.Server_Stop_Seen := True;
         when T_Error_Type =>
            D.S.Error_Seen := Error_Of_Text (Captured);
         when T_Block_Type =>
            D.S.Block_Kind := Block_Type_Of (Captured);
         when T_Tool_Id =>
            D.S.Tool_Id := D.S.Capture;
            D.S.Tool_Id_Length := D.S.Capture_Length;
         when T_Tool_Name =>
            D.S.Tool_Name := D.S.Capture;
            D.S.Tool_Name_Length := D.S.Capture_Length;
         when others =>
            null;
      end case;
      Value_Complete (D);
   end String_Value_End;

   procedure String_End (D : in out Decoder; Text : in out Output) is
   begin
      Flush_Surrogate (D, Text);
      if D.S.Is_Key then
         D.S.Is_Key := False;
         D.S.Lex := No_Lexeme;
         if D.S.Depth in 1 .. Tracked_Depth then
            D.S.Keys (D.S.Depth) :=
              (if D.S.Key_Over then K_Other else Key_Of (D.S.Key (1 .. D.S.Key_Length)));
         end if;
         D.S.Expect := Expect_Colon;
      else
         String_Value_End (D);
      end if;
   end String_End;

   procedure Number_End (D : in out Decoder) is
   begin
      if D.S.Number_At not in After_Zero | Int_Digits | Frac_Digits | Exp_Digits then
         Json_Fail (D, P.Json_Syntax);
         return;
      end if;
      if D.S.Goal in Number_Target then
         if not D.S.Number_Plain then
            Json_Fail (D, P.Type_Mismatch);
            return;
         end if;
         case D.S.Goal is
            when T_Input => D.S.Usage.Input := D.S.Number_Value;
            when T_Output => D.S.Usage.Output := D.S.Number_Value;
            when T_Cache_Write => D.S.Usage.Cache_Write := D.S.Number_Value;
            when T_Cache_Read => D.S.Usage.Cache_Read := D.S.Number_Value;
            when others => null;
         end case;
      end if;
      Value_Complete (D);
   end Number_End;

   function Number_Byte (B : Byte) return Boolean is
     (Char (B) in '0' .. '9' | '-' | '+' | '.' | 'e' | 'E');

   procedure Number_Next (D : in out Decoder; B : Byte) is
      C : constant Character := Char (B);
      Is_Digit : constant Boolean := C in '0' .. '9';
   begin
      if not Is_Digit then
         D.S.Number_Plain := False;
      end if;
      case D.S.Number_At is
         when After_Minus =>
            if C = '0' then D.S.Number_At := After_Zero;
            elsif Is_Digit then D.S.Number_At := Int_Digits;
            else Json_Fail (D, P.Json_Syntax); return; end if;
         when After_Zero =>
            if C = '.' then D.S.Number_At := After_Point;
            elsif C in 'e' | 'E' then D.S.Number_At := After_Exponent;
            else Json_Fail (D, P.Json_Syntax); return; end if;
         when Int_Digits =>
            if Is_Digit then null;
            elsif C = '.' then D.S.Number_At := After_Point;
            elsif C in 'e' | 'E' then D.S.Number_At := After_Exponent;
            else Json_Fail (D, P.Json_Syntax); return; end if;
         when After_Point =>
            if Is_Digit then D.S.Number_At := Frac_Digits;
            else Json_Fail (D, P.Json_Syntax); return; end if;
         when Frac_Digits =>
            if Is_Digit then null;
            elsif C in 'e' | 'E' then D.S.Number_At := After_Exponent;
            else Json_Fail (D, P.Json_Syntax); return; end if;
         when After_Exponent =>
            if C in '+' | '-' then D.S.Number_At := After_Exponent_Sign;
            elsif Is_Digit then D.S.Number_At := Exp_Digits;
            else Json_Fail (D, P.Json_Syntax); return; end if;
         when After_Exponent_Sign | Exp_Digits =>
            if Is_Digit then D.S.Number_At := Exp_Digits;
            else Json_Fail (D, P.Json_Syntax); return; end if;
      end case;
      if Is_Digit and then D.S.Number_Plain and then D.S.Goal in Number_Target then
         declare
            Digit : constant Unsigned_64 := Digit_Of (B);
         begin
            if D.S.Number_Value > (Llm_Costs.Maximum_Token_Count - Digit) / 10 then
               Json_Fail (D, P.Number_Overflow);
               return;
            end if;
            D.S.Number_Value := D.S.Number_Value * 10 + Digit;
         end;
      end if;
   end Number_Next;

   function Literal_Text (Kind : Literal_Kind) return String is
     (case Kind is
         when Literal_True => "true", when Literal_False => "false", when Literal_Null => "null");

   procedure Literal_Next (D : in out Decoder; B : Byte) is
      Expected : constant String := Literal_Text (D.S.Literal);
   begin
      if D.S.Literal_Position >= Expected'Length
        or else Char (B) /= Expected (Expected'First + D.S.Literal_Position)
      then
         Json_Fail (D, P.Json_Syntax);
         return;
      end if;
      D.S.Literal_Position := D.S.Literal_Position + 1;
      if D.S.Literal_Position = Expected'Length then
         Value_Complete (D);
      end if;
   end Literal_Next;

   procedure Open_Container (D : in out Decoder; Kind : Container) is
   begin
      if D.S.Depth = Maximum_Json_Depth then
         Json_Fail (D, P.Json_Too_Deep);
         return;
      end if;
      D.S.Depth := D.S.Depth + 1;
      D.S.Stack (D.S.Depth) := Kind;
      if D.S.Depth <= Tracked_Depth then
         D.S.Keys (D.S.Depth) := (if Kind = In_Array then K_Element else K_Other);
      end if;
      D.S.Expect := (if Kind = In_Object then Expect_Key_Or_Close else Expect_Value_Or_Close);
      if D.S.Event = Content_Block_Start and then D.S.Depth = 2 and then Kind = In_Object
        and then D.S.Keys (1) = K_Content_Block
      then
         Content_Begin (D);
      end if;
   end Open_Container;

   procedure Close_Container (D : in out Decoder) is
   begin
      if D.S.Depth > 0 then
         D.S.Depth := D.S.Depth - 1;
      end if;
      Value_Complete (D);
   end Close_Container;

   procedure Begin_Value (D : in out Decoder; B : Byte) is
      Goal : constant Target := Goal_Of (D);
      C : constant Character := Char (B);
   begin
      D.S.Goal := Goal;
      case C is
         when '{' | '[' =>
            if Goal /= T_None then
               Json_Fail (D, P.Type_Mismatch);
               return;
            end if;
            Open_Container (D, (if C = '{' then In_Object else In_Array));
         when '"' =>
            if Goal in Number_Target then
               Json_Fail (D, P.Type_Mismatch);
               return;
            end if;
            D.S.Lex := In_String;
            D.S.Is_Key := False;
            D.S.Capture_Length := 0;
            D.S.Capture_Over := False;
            D.S.High_Surrogate := 0;
         when '-' | '0' .. '9' =>
            if Goal /= T_None and then Goal not in Number_Target then
               Json_Fail (D, P.Type_Mismatch);
               return;
            end if;
            D.S.Lex := In_Number;
            D.S.Number_Plain := C /= '-';
            D.S.Number_Value := 0;
            D.S.Number_At := (if C = '-' then After_Minus elsif C = '0' then After_Zero else Int_Digits);
            if C in '1' .. '9' and then Goal in Number_Target then
               D.S.Number_Value := Digit_Of (B);
            end if;
         when 't' | 'f' | 'n' =>
            --  A stop reason or an error message may be null; nothing
            --  else this client keeps may be a literal.
            if Goal /= T_None and then not (C = 'n' and then Goal in T_Stop | T_Error_Message) then
               Json_Fail (D, P.Type_Mismatch);
               return;
            end if;
            D.S.Lex := In_Literal;
            D.S.Literal := (if C = 't' then Literal_True elsif C = 'f' then Literal_False else Literal_Null);
            D.S.Literal_Position := 1;
         when others =>
            Json_Fail (D, P.Json_Syntax);
      end case;
   end Begin_Value;

   procedure Structural (D : in out Decoder; B : Byte) is
      C : constant Character := Char (B);
   begin
      if B in Space_Byte | HT_Byte | LF_Byte | CR_Byte then
         return;
      end if;
      case D.S.Expect is
         when Expect_Nothing =>
            Json_Fail (D, P.Json_Syntax);
         when Expect_Colon =>
            if C = ':' then D.S.Expect := Expect_Value; else Json_Fail (D, P.Json_Syntax); end if;
         when Expect_Comma_Or_Close =>
            if D.S.Depth = 0 then
               Json_Fail (D, P.Json_Syntax);
            elsif C = ',' then
               D.S.Expect := (if D.S.Stack (D.S.Depth) = In_Object then Expect_Key else Expect_Value);
            elsif (C = '}' and then D.S.Stack (D.S.Depth) = In_Object)
              or else (C = ']' and then D.S.Stack (D.S.Depth) = In_Array)
            then
               Close_Container (D);
            else
               Json_Fail (D, P.Json_Syntax);
            end if;
         when Expect_Key_Or_Close | Expect_Key =>
            if C = '}' and then D.S.Expect = Expect_Key_Or_Close then
               Close_Container (D);
            elsif C = '"' then
               D.S.Lex := In_String;
               D.S.Is_Key := True;
               D.S.Key_Length := 0;
               D.S.Key_Over := False;
               D.S.Goal := T_None;
            else
               Json_Fail (D, P.Json_Syntax);
            end if;
         when Expect_Value_Or_Close =>
            if C = ']' then Close_Container (D); else Begin_Value (D, B); end if;
         when Expect_Value =>
            Begin_Value (D, B);
      end case;
   end Structural;

   procedure Json_Byte (D : in out Decoder; B : Byte; Text : in out Output) is
   begin
      if D.S.Done or else D.S.Document_Broken then
         return;
      end if;
      case D.S.Lex is
         when In_String =>
            if B = Character'Pos ('"') then
               String_End (D, Text);
            elsif B = Character'Pos ('\') then
               D.S.Lex := In_Escape;
            elsif B < First_Printable then
               Json_Fail (D, P.Json_Syntax);
            else
               Flush_Surrogate (D, Text);
               String_Content (D, B, Text);
            end if;
            return;
         when In_Escape =>
            D.S.Lex := In_String;
            case Char (B) is
               when '"' | '\' | '/' => Flush_Surrogate (D, Text); String_Content (D, B, Text);
               when 'b' => Unit (D, Character'Pos (BS), Text);
               when 'f' => Unit (D, Character'Pos (FF), Text);
               when 'n' => Unit (D, Character'Pos (LF), Text);
               when 'r' => Unit (D, Character'Pos (CR), Text);
               when 't' => Unit (D, Character'Pos (HT), Text);
               when 'u' => D.S.Lex := In_Unicode; D.S.Hex_Count := 0; D.S.Code := 0;
               when others => Json_Fail (D, P.Json_Syntax);
            end case;
            return;
         when In_Unicode =>
            declare
               V : constant Integer := Hex_Value (B);
            begin
               if V < 0 then
                  Json_Fail (D, P.Json_Syntax);
                  return;
               end if;
               D.S.Code := (D.S.Code mod 16#1000#) * 16 + V;
               if D.S.Hex_Count >= 3 then
                  D.S.Hex_Count := 0;
                  D.S.Lex := In_String;
                  Unit (D, D.S.Code, Text);
               else
                  D.S.Hex_Count := D.S.Hex_Count + 1;
               end if;
            end;
            return;
         when In_Number =>
            if Number_Byte (B) then
               Number_Next (D, B);
               return;
            end if;
            Number_End (D);
            if D.S.Done or else D.S.Document_Broken then
               return;
            end if;
         when In_Literal =>
            Literal_Next (D, B);
            return;
         when No_Lexeme =>
            null;
      end case;
      Structural (D, B);
   end Json_Byte;

   --  The event's (or document's) JSON is over: was it one whole value?
   procedure Json_Complete (D : in out Decoder; Complete : out Boolean) is
   begin
      if D.S.Lex = In_Number and then D.S.Depth = 0 then
         Number_End (D);
      end if;
      Complete := not D.S.Done and then not D.S.Document_Broken and then D.S.Expect = Expect_Nothing;
   end Json_Complete;

   ---------------------------------------------------------------------------
   --  Events.
   ---------------------------------------------------------------------------
   procedure Event_Reset (D : in out Decoder) is
   begin
      D.S.Event_Named := False;
      D.S.Event := No_Event;
      D.S.Event_Name_Length := 0;
      D.S.Event_Name_Over := False;
      D.S.Data_Seen := False;
      Json_Reset (D);
   end Event_Reset;

   procedure Check_Cost (D : in out Decoder) is
      use type P.Ceiling_Kind;
   begin
      if D.S.Limit.Ceiling = P.Nano_Usd and then
        (not D.S.Limit.Price.Known or else
         Llm_Costs.Worst_Case (D.S.Limit.Price, D.S.Usage, Llm_Costs.Token_Count (D.S.Limit.Max_Tokens))
           > D.S.Limit.Ceiling_Nano_Usd)
      then
         Finish (D, P.Cost_Budget);
      end if;
   end Check_Cost;

   procedure Dispatch (D : in out Decoder) is
      Complete : Boolean;
   begin
      if not D.S.Data_Seen then
         Event_Reset (D);
         return;
      end if;
      Json_Complete (D, Complete);
      if not Complete then
         Fail (D, P.Truncated_Event);
         return;
      end if;
      if D.S.Event /= Unknown_Event and then not (D.S.Type_Seen and then D.S.Type_Matches) then
         Fail (D, P.Type_Mismatch);
         return;
      end if;
      case D.S.Event is
         when Message_Start =>
            if D.S.Message_At /= Awaiting_Start then
               Fail (D, P.Out_Of_Order);
               return;
            end if;
            D.S.Message_At := In_Message;
            D.S.Has_Started := True;
            Check_Cost (D);
         when Content_Block_Start | Content_Block_Delta | Content_Block_Stop | Message_Delta =>
            if D.S.Message_At /= In_Message then
               Fail (D, P.Out_Of_Order);
               return;
            end if;
            if D.S.Event = Message_Delta then
               Check_Cost (D);
            elsif D.S.Event = Content_Block_Start then
               Content_Started (D);
            elsif D.S.Event = Content_Block_Stop then
               Content_Stopped (D);
            end if;
         when Message_Stop =>
            if D.S.Message_At /= In_Message then
               Fail (D, P.Out_Of_Order);
            elsif not D.S.Server_Stop_Seen then
               Fail (D, P.Missing_Stop_Reason);
            else
               Finish (D, D.S.Server_Stop);
            end if;
         when Error_Event =>
            D.S.Error := (if D.S.Error_Seen = P.No_Error then P.Other_Error else D.S.Error_Seen);
            Finish (D, P.Api_Error);
         when Ping | Unknown_Event | No_Event =>
            null;
      end case;
      Event_Reset (D);
   end Dispatch;

   --  A field's name ended (at its colon, or at the end of a line with none).
   function Field_Is (D : Decoder; Name : String) return Boolean is
     (not D.S.Field_Over and then D.S.Field_Text (1 .. D.S.Field_Length) = Name);

   procedure Field_Named (D : in out Decoder; Text : in out Output) is
      Kind : constant Sse_Field :=
        (if Field_Is (D, "event") then Event_Field
         elsif Field_Is (D, "data") then Data_Field
         else Other_Field);
   begin
      D.S.Field := Kind;
      case Kind is
         when Event_Field =>
            if D.S.Data_Seen then
               Fail (D, P.Bad_Event_Stream);
            else
               D.S.Event_Name_Length := 0;
               D.S.Event_Name_Over := False;
            end if;
         when Data_Field =>
            if not D.S.Event_Named then
               Fail (D, P.Data_Without_Event);
            elsif D.S.Data_Seen then
               --  Data lines join with a line feed.
               Json_Byte (D, LF_Byte, Text);
            else
               D.S.Data_Seen := True;
               Json_Reset (D);
            end if;
         when No_Field | Other_Field =>
            null;
      end case;
   end Field_Named;

   procedure Field_Ended (D : in out Decoder) is
   begin
      if D.S.Field = Event_Field then
         D.S.Event_Named := True;
         D.S.Event :=
           (if D.S.Event_Name_Over then Unknown_Event
            else Event_Of (D.S.Event_Name (1 .. D.S.Event_Name_Length)));
      end if;
      D.S.Field := No_Field;
   end Field_Ended;

   procedure Field_Value_Byte (D : in out Decoder; B : Byte; Text : in out Output) is
   begin
      case D.S.Field is
         when Event_Field =>
            if D.S.Event_Name_Length < Maximum_Event_Name then
               D.S.Event_Name_Length := D.S.Event_Name_Length + 1;
               D.S.Event_Name (D.S.Event_Name_Length) := Char (B);
            else
               D.S.Event_Name_Over := True;
            end if;
         when Data_Field =>
            Json_Byte (D, B, Text);
         when No_Field | Other_Field =>
            null;
      end case;
   end Field_Value_Byte;

   procedure Line_Ended (D : in out Decoder; Text : in out Output) is
   begin
      case D.S.Sse_At is
         when Line_Start =>
            Dispatch (D);
         when Field_Name =>
            Field_Named (D, Text);
            Field_Ended (D);
         when Field_Space | Field_Value =>
            Field_Ended (D);
         when Ignore_Line =>
            null;
      end case;
      D.S.Sse_At := Line_Start;
   end Line_Ended;

   procedure Sse_Byte (D : in out Decoder; B : Byte; Text : in out Output) is
   begin
      if D.S.Sse_CR then
         D.S.Sse_CR := False;
         if B = LF_Byte then
            return;
         end if;
      end if;
      if B = CR_Byte or else B = LF_Byte then
         D.S.Sse_CR := B = CR_Byte;
         Line_Ended (D, Text);
         return;
      end if;
      case D.S.Sse_At is
         when Line_Start =>
            if B = Character'Pos (':') then
               D.S.Sse_At := Ignore_Line;
            else
               D.S.Field_Text (1) := Char (B);
               D.S.Field_Length := 1;
               D.S.Field_Over := False;
               D.S.Sse_At := Field_Name;
            end if;
         when Field_Name =>
            if B = Character'Pos (':') then
               Field_Named (D, Text);
               D.S.Sse_At := Field_Space;
            elsif D.S.Field_Length < Maximum_Field_Name then
               D.S.Field_Length := D.S.Field_Length + 1;
               D.S.Field_Text (D.S.Field_Length) := Char (B);
            else
               D.S.Field_Over := True;
            end if;
         when Field_Space =>
            D.S.Sse_At := Field_Value;
            if B /= Space_Byte then
               Field_Value_Byte (D, B, Text);
            end if;
         when Field_Value =>
            Field_Value_Byte (D, B, Text);
         when Ignore_Line =>
            null;
      end case;
   end Sse_Byte;

   ---------------------------------------------------------------------------
   --  HTTP.
   ---------------------------------------------------------------------------
   procedure Body_End (D : in out Decoder) is
      Complete : Boolean;
   begin
      D.S.Phase := Body_Done;
      if D.S.Done then
         return;
      end if;
      if D.S.Error_Document then
         Json_Complete (D, Complete);
         D.S.Error :=
           (if Complete and then D.S.Type_Seen and then D.S.Type_Matches
              and then D.S.Error_Seen /= P.No_Error
            then D.S.Error_Seen else Error_Of_Status (D.S.Status));
         Finish (D, P.Api_Error);
      else
         --  An event cut off by the end is discarded; the message never
         --  ended.
         Finish (D, P.Disconnected);
      end if;
   end Body_End;

   procedure Body_Byte (D : in out Decoder; B : Byte; Text : in out Output) is
   begin
      if D.S.Body_Bytes >= D.S.Body_Limit then
         Fail (D, P.Response_Too_Large);
         return;
      end if;
      D.S.Body_Bytes := D.S.Body_Bytes + 1;
      if D.S.Error_Document then
         Json_Byte (D, B, Text);
      else
         Sse_Byte (D, B, Text);
      end if;
   end Body_Byte;

   procedure Begin_Body (D : in out Decoder) is
   begin
      if D.S.Status = Status_OK then
         if not D.S.Event_Stream then
            Fail (D, P.Bad_Content_Type);
            return;
         end if;
      else
         D.S.Error_Document := True;
         D.S.Event := Error_Event;
         Json_Reset (D);
      end if;
      if D.S.Chunked then
         D.S.Phase := Chunk_Size;
         D.S.Chunk_Digits := 0;
         D.S.Remaining := 0;
      elsif D.S.Have_Length then
         if D.S.Remaining = 0 then
            Body_End (D);
         else
            D.S.Phase := Body_Identity;
         end if;
      else
         D.S.Phase := Body_Until_Close;
      end if;
   end Begin_Body;

   function Equal_Folded (Text, Expected : String) return Boolean is
   begin
      if Text'Length /= Expected'Length then
         return False;
      end if;
      for K in 0 .. Text'Length - 1 loop
         pragma Loop_Invariant (Text'Length = Expected'Length);
         if Lower (Text (Text'First + K)) /= Expected (Expected'First + K) then
            return False;
         end if;
      end loop;
      return True;
   end Equal_Folded;

   procedure Header_Ended (D : in out Decoder) is
      Name : constant String := D.S.Name (1 .. D.S.Name_Length);
      Last : Natural := D.S.Value_Length;
   begin
      while Last > 0 and then D.S.Value (Last) in ' ' | HT loop
         pragma Loop_Invariant (Last <= Maximum_Header_Value);
         Last := Last - 1;
      end loop;
      declare
         Value : constant String := D.S.Value (1 .. Last);
         Event_Stream_Type : constant String := "text/event-stream";
      begin
         if D.S.Name_Over then
            null;
         elsif Name = "content-type" then
            D.S.Event_Stream := Value'Length >= Event_Stream_Type'Length and then
              Equal_Folded (Value (Value'First .. Value'First + Event_Stream_Type'Length - 1),
                            Event_Stream_Type);
         elsif Name = "transfer-encoding" then
            if D.S.Value_Over then
               Fail (D, P.Bad_Header);
            elsif Equal_Folded (Value, "chunked") then
               D.S.Chunked := True;
            elsif not Equal_Folded (Value, "identity") then
               Fail (D, P.Bad_Header);
            end if;
         elsif Name = "content-length" then
            if D.S.Value_Over or else Value'Length not in 1 .. Maximum_Length_Digits
              or else (for some C of Value => C not in '0' .. '9')
            then
               Fail (D, P.Bad_Header);
               return;
            end if;
            D.S.Remaining := 0;
            for C of Value loop
               pragma Loop_Invariant (D.S.Remaining <= Maximum_Content_Length);
               if D.S.Remaining > (Maximum_Content_Length - 9) / 10 then
                  Fail (D, P.Bad_Header);
                  return;
               end if;
               D.S.Remaining := D.S.Remaining * 10 + Digit_Of (Character'Pos (C));
            end loop;
            D.S.Have_Length := True;
         end if;
      end;
   end Header_Ended;

   procedure Status_Byte (D : in out Decoder; B : Byte) is
      C : constant Character := Char (B);
   begin
      if B = LF_Byte then
         D.S.Line_CR := False;
         if D.S.Status_At = Code and then D.S.Status_Digits = 3 then
            D.S.Status_At := Reason;
         end if;
         if D.S.Status_At /= Reason or else D.S.Status not in Lowest_Status .. Highest_Status then
            Fail (D, P.Bad_Status_Line);
            return;
         end if;
         D.S.Phase := Header_Line;
         D.S.Line_Empty := True;
         D.S.Name_Length := 0;
         D.S.Name_Over := False;
         D.S.In_Value := False;
         return;
      elsif D.S.Line_CR then
         Fail (D, P.Bad_Status_Line);
         return;
      elsif B = CR_Byte then
         D.S.Line_CR := True;
         return;
      end if;
      case D.S.Status_At is
         when Version =>
            if D.S.Version_Length < Http_Prefix'Length then
               if C /= Http_Prefix (Http_Prefix'First + D.S.Version_Length) then
                  Fail (D, P.Bad_Status_Line);
                  return;
               end if;
               D.S.Version_Length := D.S.Version_Length + 1;
            elsif C = ' ' and then D.S.Version_Length > Http_Prefix'Length then
               D.S.Status_At := Code;
            elsif C in '0' .. '9' | '.' and then D.S.Version_Length < Maximum_Status_Version then
               D.S.Version_Length := D.S.Version_Length + 1;
            else
               Fail (D, P.Bad_Status_Line);
            end if;
         when Code =>
            if C in '0' .. '9' and then D.S.Status_Digits < 3 and then D.S.Status <= 99 then
               D.S.Status := D.S.Status * 10 + Natural (Digit_Of (B));
               D.S.Status_Digits := D.S.Status_Digits + 1;
            elsif C = ' ' and then D.S.Status_Digits = 3 then
               D.S.Status_At := Reason;
            else
               Fail (D, P.Bad_Status_Line);
            end if;
         when Reason =>
            if B < First_Printable and then B /= HT_Byte then
               Fail (D, P.Bad_Status_Line);
            end if;
      end case;
   end Status_Byte;

   procedure Header_Byte (D : in out Decoder; B : Byte) is
      C : constant Character := Char (B);
   begin
      if B = LF_Byte then
         D.S.Line_CR := False;
         if D.S.Line_Empty then
            Begin_Body (D);
         elsif not D.S.In_Value then
            Fail (D, P.Bad_Header);
         else
            Header_Ended (D);
            D.S.Line_Empty := True;
            D.S.Name_Length := 0;
            D.S.Name_Over := False;
            D.S.In_Value := False;
         end if;
         return;
      elsif D.S.Line_CR then
         Fail (D, P.Bad_Header);
         return;
      elsif B = CR_Byte then
         D.S.Line_CR := True;
         return;
      end if;
      D.S.Line_Empty := False;
      if not D.S.In_Value then
         if C = ':' and then (D.S.Name_Length > 0 or else D.S.Name_Over) then
            D.S.In_Value := True;
            D.S.Value_Started := False;
            D.S.Value_Length := 0;
            D.S.Value_Over := False;
         elsif C in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' | '.' then
            if D.S.Name_Length < Maximum_Header_Name then
               D.S.Name_Length := D.S.Name_Length + 1;
               D.S.Name (D.S.Name_Length) := Lower (C);
            else
               D.S.Name_Over := True;
            end if;
         else
            Fail (D, P.Bad_Header);
         end if;
      elsif (B < First_Printable and then B /= HT_Byte) or else B = DEL_Byte then
         Fail (D, P.Bad_Header);
      elsif not D.S.Value_Started and then B in Space_Byte | HT_Byte then
         null;
      else
         D.S.Value_Started := True;
         if D.S.Value_Length < Maximum_Header_Value then
            D.S.Value_Length := D.S.Value_Length + 1;
            D.S.Value (D.S.Value_Length) := C;
         else
            D.S.Value_Over := True;
         end if;
      end if;
   end Header_Byte;

   procedure Chunk_Line_End (D : in out Decoder) is
   begin
      if D.S.Chunk_Digits = 0 then
         Fail (D, P.Bad_Chunk);
      elsif D.S.Remaining = 0 then
         D.S.Phase := Trailer_Line;
         D.S.Line_Empty := True;
      else
         D.S.Phase := Chunk_Data;
      end if;
   end Chunk_Line_End;

   procedure Push (D : in out Decoder; B : Byte; Text : out Output) is
   begin
      Text := (others => <>);
      if D.S.Done then
         return;
      end if;
      case D.S.Phase is
         when Status_Line | Header_Line =>
            if D.S.Header_Bytes = Maximum_Header_Section then
               Fail (D, P.Headers_Too_Large);
               return;
            end if;
            D.S.Header_Bytes := D.S.Header_Bytes + 1;
            if D.S.Phase = Status_Line then
               Status_Byte (D, B);
            else
               Header_Byte (D, B);
            end if;
         when Body_Identity =>
            Body_Byte (D, B, Text);
            if D.S.Remaining > 0 then
               D.S.Remaining := D.S.Remaining - 1;
            end if;
            if D.S.Remaining = 0 then
               Body_End (D);
            end if;
         when Body_Until_Close =>
            Body_Byte (D, B, Text);
         when Chunk_Size =>
            if B = LF_Byte then
               D.S.Line_CR := False;
               Chunk_Line_End (D);
            elsif D.S.Line_CR then
               Fail (D, P.Bad_Chunk);
            elsif B = CR_Byte then
               D.S.Line_CR := True;
            elsif B = Character'Pos (';') then
               D.S.Phase := Chunk_Extension;
            elsif Hex_Value (B) >= 0 and then D.S.Chunk_Digits < Maximum_Chunk_Digits then
               D.S.Remaining := D.S.Remaining * 16 + Unsigned_64 (Hex_Value (B));
               D.S.Chunk_Digits := D.S.Chunk_Digits + 1;
            else
               Fail (D, P.Bad_Chunk);
            end if;
         when Chunk_Extension =>
            if B = LF_Byte then
               D.S.Line_CR := False;
               Chunk_Line_End (D);
            end if;
         when Chunk_Data =>
            Body_Byte (D, B, Text);
            if D.S.Remaining > 0 then
               D.S.Remaining := D.S.Remaining - 1;
            end if;
            if D.S.Remaining = 0 then
               D.S.Phase := Chunk_Data_End;
               D.S.Line_CR := False;
            end if;
         when Chunk_Data_End =>
            if B = CR_Byte and then not D.S.Line_CR then
               D.S.Line_CR := True;
            elsif B = LF_Byte then
               D.S.Line_CR := False;
               D.S.Phase := Chunk_Size;
               D.S.Chunk_Digits := 0;
               D.S.Remaining := 0;
            else
               Fail (D, P.Bad_Chunk);
            end if;
         when Trailer_Line =>
            if B = LF_Byte then
               if D.S.Line_Empty then
                  Body_End (D);
               end if;
               D.S.Line_Empty := True;
            elsif B /= CR_Byte then
               D.S.Line_Empty := False;
            end if;
         when Body_Done =>
            null;
      end case;
   end Push;

   procedure End_Of_Input (D : in out Decoder) is
   begin
      if D.S.Done then
         return;
      elsif D.S.Phase = Body_Until_Close then
         Body_End (D);
      elsif D.S.Phase /= Body_Done then
         Finish (D, P.Disconnected);
      end if;
   end End_Of_Input;

   ---------------------------------------------------------------------------
   --  Results.
   ---------------------------------------------------------------------------
   function Finished (D : Decoder) return Boolean is (D.S.Done);
   function Started (D : Decoder) return Boolean is (D.S.Has_Started);
   function Stop_Of (D : Decoder) return P.Stop_Reason is (D.S.Result);
   function Error_Of (D : Decoder) return P.Error_Kind is (D.S.Error);
   function Problem_Of (D : Decoder) return P.Problem is (D.S.Fault);
   function Status_Of (D : Decoder) return Natural is
     (if D.S.Phase = Status_Line then 0 else D.S.Status);
   function Usage_Of (D : Decoder) return Llm_Costs.Usage is (D.S.Usage);
   function Cost_Known (D : Decoder) return Boolean is (D.S.Limit.Price.Known);
   function Cost_Of (D : Decoder) return Llm_Costs.Nano_Usd is
     (Llm_Costs.Cost (D.S.Limit.Price, D.S.Usage));
   function Model_Of (D : Decoder) return String is (D.S.Model (1 .. D.S.Model_Length));
   function Detail_Of (D : Decoder) return String is (D.S.Detail (1 .. D.S.Detail_Length));
   function Text_Bytes_Of (D : Decoder) return Unsigned_64 is (D.S.Text_Bytes);
   function Content_Length (D : Decoder) return Content_Count is (D.S.Content_Done);
   function Content_Byte (D : Decoder; Index : Positive) return Byte is (D.Content (Index));
   function Tool_Calls_Of (D : Decoder) return Natural is (D.S.Tool_Calls);
end Llm_Responses;
