pragma Ada_2022;
with Llm_Json;

package body Llm_Requests with SPARK_Mode is
   use ASCII;

   CRLF : constant String := CR & LF;
   Hex : constant String (1 .. 16) := "0123456789abcdef";
   First_Printable : constant := 16#20#;
   Byte_Base : constant := 256;

   --  How one character is written inside a JSON string.
   function Short_Escape (C : Character) return Boolean is
     (C in '"' | '\' | LF | CR | HT | BS | FF);
   function Needs_Escape (C : Character) return Boolean is
     (Character'Pos (C) < First_Printable or else C = DEL);
   function Escape_Width (C : Character) return Positive is
     (if Short_Escape (C) then 2 elsif Needs_Escape (C) then 6 else 1);

   function Escaped_Length (Text : String) return Natural is
      Total : Natural := 0;
   begin
      for I in Text'Range loop
         pragma Loop_Invariant (Total <= 6 * (I - Text'First));
         Total := Total + Escape_Width (Text (I));
      end loop;
      return Total;
   end Escaped_Length;

   function Decimal_Length (Value : Natural) return Positive is
     (if Value < 10 then 1 elsif Value < 100 then 2 elsif Value < 1_000 then 3
      elsif Value < 10_000 then 4 elsif Value < 100_000 then 5
      elsif Value < 1_000_000 then 6 elsif Value < 10_000_000 then 7
      elsif Value < 100_000_000 then 8 elsif Value < 1_000_000_000 then 9 else 10);

   function Field (Text : String; Offset, Width : Natural) return Natural is
      function Byte_At (K : Natural) return Natural is (Character'Pos (Text (Text'First + Offset + K)))
      with Pre => Width <= 3 and then K < Width and then Offset <= Maximum_Text and then Offset + K < Text'Length and then
                  Text'Length <= Maximum_Text and then Text'Last <= Maximum_Index,
           Post => Byte_At'Result < Byte_Base;
   begin
      return (if Width = 1 then Byte_At (0)
              elsif Width = 2 then Byte_At (0) + Byte_At (1) * Byte_Base
              else Byte_At (0) + Byte_At (1) * Byte_Base + Byte_At (2) * Byte_Base * Byte_Base);
   end Field;

   function Length_Field (Text : String; Offset : Natural) return Natural is
     (if Character'Pos (Text (Text'First + Offset + 3)) /= 0 then Maximum_Text + 1
      else Natural'Min (Field (Text, Offset, 3), Maximum_Text + 1));

   ---------------------------------------------------------------------------
   --  Walking sections.
   ---------------------------------------------------------------------------
   --  Offsets and lengths within one section (sums of two cannot overflow).
   subtype Span is Natural range 0 .. Maximum_Text + 1;
   type Tool_Facts is record
      Name_First, Name_Length, Description_First, Description_Length,
      Schema_First, Schema_Length, Next : Span := 0;
   end record;

   --  The tool entry at Offset (0-based within Tools).
   procedure Read_Tool (Tools : String; Offset : Natural; Facts : out Tool_Facts; OK : out Boolean)
   with Pre => Tools'Length <= Maximum_Text and then Tools'Last <= Maximum_Index and then Offset < Tools'Length,
        Post => (if OK then Facts.Next in Offset + 1 .. Tools'Length and then
                 Facts.Name_First + Facts.Name_Length <= Tools'Length and then
                 Facts.Description_First + Facts.Description_Length <= Tools'Length and then
                 Facts.Schema_First + Facts.Schema_Length <= Tools'Length)
   is
   begin
      Facts := (others => 0);
      OK := False;
      if Tools'Length - Offset < P.Tool_Header_Bytes then
         return;
      end if;
      Facts.Name_Length := Field (Tools, Offset + P.Tool_Name_Length_Offset, 2);
      Facts.Description_Length := Field (Tools, Offset + P.Tool_Description_Length_Offset, 2);
      Facts.Schema_Length := Length_Field (Tools, Offset + P.Tool_Schema_Length_Offset);
      if Facts.Name_Length + Facts.Description_Length + Facts.Schema_Length >
        Tools'Length - Offset - P.Tool_Header_Bytes
      then
         return;
      end if;
      Facts.Name_First := Offset + P.Tool_Header_Bytes;
      Facts.Description_First := Facts.Name_First + Facts.Name_Length;
      Facts.Schema_First := Facts.Description_First + Facts.Description_Length;
      Facts.Next := Facts.Schema_First + Facts.Schema_Length;
      OK := True;
   end Read_Tool;

   type Block_Facts is record
      Role_Code, Kind_Code, Error_Code, Reserved : Natural := 0;
      A_First, A_Length, B_First, B_Length, C_First, C_Length, Next : Span := 0;
   end record;

   procedure Read_Block
     (Conversation : String; Offset : Natural; Facts : out Block_Facts; OK : out Boolean)
   with Pre => Conversation'Length <= Maximum_Text and then Conversation'Last <= Maximum_Index and then
               Offset < Conversation'Length,
        Post => (if OK then Facts.Next in Offset + 1 .. Conversation'Length and then
                 Facts.A_First + Facts.A_Length <= Conversation'Length and then
                 Facts.B_First + Facts.B_Length <= Conversation'Length and then
                 Facts.C_First + Facts.C_Length <= Conversation'Length)
   is
   begin
      Facts := (others => 0);
      OK := False;
      if Conversation'Length - Offset < P.Block_Header_Bytes then
         return;
      end if;
      Facts.Role_Code := Field (Conversation, Offset + P.Block_Role_Offset, 1);
      Facts.Kind_Code := Field (Conversation, Offset + P.Block_Kind_Offset, 1);
      Facts.Error_Code := Field (Conversation, Offset + P.Block_Error_Offset, 1);
      Facts.Reserved := Field (Conversation, Offset + P.Block_Error_Offset + 1, 1);
      Facts.A_Length := Length_Field (Conversation, Offset + P.Block_A_Length_Offset);
      Facts.B_Length := Length_Field (Conversation, Offset + P.Block_B_Length_Offset);
      Facts.C_Length := Length_Field (Conversation, Offset + P.Block_C_Length_Offset);
      if Facts.A_Length + Facts.B_Length + Facts.C_Length >
        Conversation'Length - Offset - P.Block_Header_Bytes
      then
         return;
      end if;
      Facts.A_First := Offset + P.Block_Header_Bytes;
      Facts.B_First := Facts.A_First + Facts.A_Length;
      Facts.C_First := Facts.B_First + Facts.B_Length;
      Facts.Next := Facts.C_First + Facts.C_Length;
      OK := True;
   end Read_Block;

   function Slice (Text : String; First, Length : Span) return String is
     (Text (Text'First + First .. Text'First + First + Length - 1))
   with Pre => Text'Length <= Maximum_Text and then Text'Last <= Maximum_Index and then
               First + Length <= Text'Length,
        Post => Slice'Result'Length = Length;

   function Code (Value : P.Role) return Natural is (P.Role'Enum_Rep (Value));
   function Code (Value : P.Block_Kind) return Natural is (P.Block_Kind'Enum_Rep (Value));

   function Valid_Tools (Tools : String) return Boolean is
      Offset : Natural := 0;
      Facts : Tool_Facts;
      OK : Boolean;
   begin
      while Offset < Tools'Length loop
         pragma Loop_Invariant (Offset < Tools'Length);
         pragma Loop_Variant (Increases => Offset);
         Read_Tool (Tools, Offset, Facts, OK);
         if not OK
           or else not Valid_Tool_Name (Slice (Tools, Facts.Name_First, Facts.Name_Length))
           or else not Llm_Json.Valid_Object (Slice (Tools, Facts.Schema_First, Facts.Schema_Length))
         then
            return False;
         end if;
         Offset := Facts.Next;
      end loop;
      return True;
   end Valid_Tools;

   function Valid_Block (Conversation : String; Facts : Block_Facts) return Boolean is
     (Facts.Reserved = 0 and then Facts.Error_Code <= 1 and then
      Facts.Role_Code in Code (P.User) | Code (P.Assistant) and then
      (Facts.Error_Code = 0 or else Facts.Kind_Code = Code (P.Tool_Result_Block)) and then
      (if Facts.Kind_Code = Code (P.Text_Block) then
          Facts.A_Length > 0 and then Facts.B_Length = 0 and then Facts.C_Length = 0
       elsif Facts.Kind_Code = Code (P.Thinking_Block) then
          Facts.Role_Code = Code (P.Assistant) and then Facts.C_Length = 0
       elsif Facts.Kind_Code = Code (P.Redacted_Block) then
          Facts.Role_Code = Code (P.Assistant) and then Facts.B_Length = 0 and then Facts.C_Length = 0
       elsif Facts.Kind_Code = Code (P.Tool_Call_Block) then
          Facts.Role_Code = Code (P.Assistant) and then
          Valid_Tool_Id (Slice (Conversation, Facts.A_First, Facts.A_Length)) and then
          Valid_Tool_Name (Slice (Conversation, Facts.B_First, Facts.B_Length)) and then
          Llm_Json.Valid_Object (Slice (Conversation, Facts.C_First, Facts.C_Length))
       elsif Facts.Kind_Code = Code (P.Tool_Result_Block) then
          Facts.Role_Code = Code (P.User) and then Facts.C_Length = 0 and then
          Valid_Tool_Id (Slice (Conversation, Facts.A_First, Facts.A_Length))
       else False))
   with Pre => Conversation'Length <= Maximum_Text and then Conversation'Last <= Maximum_Index and then
               Facts.A_First + Facts.A_Length <= Conversation'Length and then
               Facts.B_First + Facts.B_Length <= Conversation'Length and then
               Facts.C_First + Facts.C_Length <= Conversation'Length;

   function Valid_Conversation (Conversation : String) return Boolean is
      Offset : Natural := 0;
      Facts : Block_Facts;
      OK : Boolean;
      Last_Role : Natural := 0;
   begin
      if Conversation'Length = 0 then
         return False;
      end if;
      while Offset < Conversation'Length loop
         pragma Loop_Invariant (Offset < Conversation'Length);
         pragma Loop_Variant (Increases => Offset);
         Read_Block (Conversation, Offset, Facts, OK);
         if not OK or else not Valid_Block (Conversation, Facts)
           or else (Offset = 0 and then Facts.Role_Code /= Code (P.User))
         then
            return False;
         end if;
         Last_Role := Facts.Role_Code;
         Offset := Facts.Next;
      end loop;
      return Last_Role = Code (P.User);
   end Valid_Conversation;

   ---------------------------------------------------------------------------
   --  Rendering: one walk measures the body (Measure) or writes it, so the
   --  length and the bytes cannot disagree.
   ---------------------------------------------------------------------------
   procedure Put (Into : in out String; Last : in out Natural; Measure : Boolean; Text : String)
   with Pre => Last <= Body_Ceiling and then (Measure or else
                 (Into'First = 1 and then Last <= Into'Last and then Into'Last < Body_Ceiling)),
        Post => Last <= Body_Ceiling and then (Measure or else Last <= Into'Last)
   is
   begin
      if Measure then
         Last := (if Text'Length >= Body_Ceiling - Last then Body_Ceiling else Last + Text'Length);
         return;
      end if;
      for C of Text loop
         pragma Loop_Invariant (Last <= Into'Last);
         if Last < Into'Last then
            Last := Last + 1;
            Into (Last) := C;
         end if;
      end loop;
   end Put;

   procedure Put_Escaped (Into : in out String; Last : in out Natural; Measure : Boolean; Text : String)
   with Pre => Last <= Body_Ceiling and then (Measure or else
                 (Into'First = 1 and then Last <= Into'Last and then Into'Last < Body_Ceiling)),
        Post => Last <= Body_Ceiling and then (Measure or else Last <= Into'Last)
   is
   begin
      for C of Text loop
         pragma Loop_Invariant
           (Last <= Body_Ceiling and then (Measure or else Last <= Into'Last));
         if C = '"' then Put (Into, Last, Measure, "\""");
         elsif C = '\' then Put (Into, Last, Measure, "\\");
         elsif C = LF then Put (Into, Last, Measure, "\n");
         elsif C = CR then Put (Into, Last, Measure, "\r");
         elsif C = HT then Put (Into, Last, Measure, "\t");
         elsif C = BS then Put (Into, Last, Measure, "\b");
         elsif C = FF then Put (Into, Last, Measure, "\f");
         elsif Needs_Escape (C) then
            Put (Into, Last, Measure, "\u00" & Hex (Character'Pos (C) / 16 + 1) &
                                       Hex (Character'Pos (C) mod 16 + 1));
         else
            Put (Into, Last, Measure, [1 => C]);
         end if;
      end loop;
   end Put_Escaped;

   function Escape (Text : String) return String is
      Result : String (1 .. Escaped_Length (Text)) := [others => ' '];
      Last : Natural := 0;
   begin
      if Result'Length >= Body_Ceiling then
         return Result;   --  not reached: Maximum_Text * 6 < Body_Ceiling
      end if;
      Put_Escaped (Result, Last, False, Text);
      return Result;
   end Escape;

   function Image (Value : Natural) return String
   with Post => Image'Result'Length = Decimal_Length (Value)
   is
      Length : constant Positive := Decimal_Length (Value);
      Result : String (1 .. Length);
      V : Natural := Value;
   begin
      for I in reverse Result'Range loop
         Result (I) := Hex (V mod 10 + 1);
         V := V / 10;
      end loop;
      return Result;
   end Image;

   Cache_Mark : constant String := ",""cache_control"":{""type"":""ephemeral""}";

   procedure Render
     (Model : String; Max_Tokens : P.Output_Tokens; System, Tools, Conversation : String;
      Cache : Boolean; Into : in out String; Last : in out Natural; Measure : Boolean)
   with Always_Terminates,
        Pre => Model'Length <= P.Maximum_Model_Bytes and then System'Length <= Maximum_Text and then
               Tools'Length <= Maximum_Text and then Conversation'Length <= Maximum_Text and then
               Tools'Last <= Maximum_Index and then Conversation'Last <= Maximum_Index and then
               Last <= Body_Ceiling and then (Measure or else
                 (Into'First = 1 and then Last <= Into'Last and then Into'Last < Body_Ceiling)),
        Post => Last <= Body_Ceiling and then (Measure or else Last <= Into'Last)
   is
      Offset : Natural;
      Tool : Tool_Facts;
      Block : Block_Facts;
      OK : Boolean;
      Role_Open : Natural := 0;
   begin
      Put (Into, Last, Measure, "{""model"":""");
      Put (Into, Last, Measure, Model);
      Put (Into, Last, Measure, """,""stream"":true");
      if Tools'Length > 0 then
         Put (Into, Last, Measure, ",""tools"":[");
         Offset := 0;
         while Offset < Tools'Length loop
            pragma Loop_Invariant
              (Offset < Tools'Length and then Last <= Body_Ceiling and then (Measure or else Last <= Into'Last));
            pragma Loop_Variant (Increases => Offset);
            Read_Tool (Tools, Offset, Tool, OK);
            exit when not OK;
            if Offset > 0 then
               Put (Into, Last, Measure, ",");
            end if;
            Put (Into, Last, Measure, "{""name"":""");
            Put (Into, Last, Measure, Slice (Tools, Tool.Name_First, Tool.Name_Length));
            Put (Into, Last, Measure, """,""description"":""");
            Put_Escaped (Into, Last, Measure, Slice (Tools, Tool.Description_First, Tool.Description_Length));
            Put (Into, Last, Measure, """,""input_schema"":");
            Put (Into, Last, Measure, Slice (Tools, Tool.Schema_First, Tool.Schema_Length));
            if Cache and then Tool.Next = Tools'Length then
               Put (Into, Last, Measure, Cache_Mark);
            end if;
            Put (Into, Last, Measure, "}");
            Offset := Tool.Next;
         end loop;
         Put (Into, Last, Measure, "]");
      end if;
      if System'Length > 0 then
         if Cache then
            Put (Into, Last, Measure, ",""system"":[{""type"":""text"",""text"":""");
            Put_Escaped (Into, Last, Measure, System);
            Put (Into, Last, Measure, """" & Cache_Mark & "}]");
         else
            Put (Into, Last, Measure, ",""system"":""");
            Put_Escaped (Into, Last, Measure, System);
            Put (Into, Last, Measure, """");
         end if;
      end if;
      Put (Into, Last, Measure, ",""messages"":[");
      Offset := 0;
      while Offset < Conversation'Length loop
         pragma Loop_Invariant
           (Offset < Conversation'Length and then Last <= Body_Ceiling and then
            (Measure or else Last <= Into'Last));
         pragma Loop_Variant (Increases => Offset);
         Read_Block (Conversation, Offset, Block, OK);
         exit when not OK;
         if Block.Role_Code /= Role_Open then
            if Role_Open /= 0 then
               Put (Into, Last, Measure, "]},");
            end if;
            Put (Into, Last, Measure,
                 (if Block.Role_Code = Code (P.Assistant) then "{""role"":""assistant"",""content"":["
                  else "{""role"":""user"",""content"":["));
            Role_Open := Block.Role_Code;
         else
            Put (Into, Last, Measure, ",");
         end if;
         declare
            A : constant String := Slice (Conversation, Block.A_First, Block.A_Length);
            B : constant String := Slice (Conversation, Block.B_First, Block.B_Length);
            C : constant String := Slice (Conversation, Block.C_First, Block.C_Length);
         begin
            if Block.Kind_Code = Code (P.Thinking_Block) then
               Put (Into, Last, Measure, "{""type"":""thinking"",""thinking"":""");
               Put_Escaped (Into, Last, Measure, A);
               Put (Into, Last, Measure, """,""signature"":""");
               Put_Escaped (Into, Last, Measure, B);
               Put (Into, Last, Measure, """");
            elsif Block.Kind_Code = Code (P.Redacted_Block) then
               Put (Into, Last, Measure, "{""type"":""redacted_thinking"",""data"":""");
               Put_Escaped (Into, Last, Measure, A);
               Put (Into, Last, Measure, """");
            elsif Block.Kind_Code = Code (P.Tool_Call_Block) then
               Put (Into, Last, Measure, "{""type"":""tool_use"",""id"":""");
               Put (Into, Last, Measure, A);
               Put (Into, Last, Measure, """,""name"":""");
               Put (Into, Last, Measure, B);
               Put (Into, Last, Measure, """,""input"":");
               Put (Into, Last, Measure, C);
            elsif Block.Kind_Code = Code (P.Tool_Result_Block) then
               Put (Into, Last, Measure, "{""type"":""tool_result"",""tool_use_id"":""");
               Put (Into, Last, Measure, A);
               Put (Into, Last, Measure, """,""content"":""");
               Put_Escaped (Into, Last, Measure, B);
               Put (Into, Last, Measure, (if Block.Error_Code = 1 then """,""is_error"":true" else """"));
            else
               Put (Into, Last, Measure, "{""type"":""text"",""text"":""");
               Put_Escaped (Into, Last, Measure, A);
               Put (Into, Last, Measure, """");
            end if;
         end;
         if Cache and then Block.Next = Conversation'Length then
            Put (Into, Last, Measure, Cache_Mark);
         end if;
         Put (Into, Last, Measure, "}");
         Offset := Block.Next;
      end loop;
      if Role_Open /= 0 then
         Put (Into, Last, Measure, "]}");
      end if;
      Put (Into, Last, Measure, "],""max_tokens"":");
      Put (Into, Last, Measure, Image (Natural (Max_Tokens)));
      Put (Into, Last, Measure, "}");
   end Render;

   function Body_Length
     (Model : String; Max_Tokens : P.Output_Tokens; System, Tools, Conversation : String;
      Cache : Boolean) return Natural
   is
      None : String (1 .. 0) := [others => ' '];
      Last : Natural := 0;
   begin
      Render (Model, Max_Tokens, System, Tools, Conversation, Cache, None, Last, Measure => True);
      return Last;
   end Body_Length;

   procedure Write_Body
     (Model : String; Max_Tokens : P.Output_Tokens; System, Tools, Conversation : String;
      Cache : Boolean; Into : out String)
   is
      Last : Natural := 0;
   begin
      Into := [others => ' '];
      if Into'Length = 0 then
         return;   --  not reached: a body is never empty
      end if;
      Render (Model, Max_Tokens, System, Tools, Conversation, Cache, Into, Last, Measure => False);
   end Write_Body;

   --  Fixed parts of the header.
   Request_Line  : constant String := "POST " & Messages_Path & " HTTP/1.1" & CRLF;
   Host_Name     : constant String := "host: ";
   Fixed_Headers : constant String :=
     "content-type: application/json" & CRLF &
     "accept: text/event-stream" & CRLF &
     "anthropic-version: " & Api_Version & CRLF &
     "user-agent: " & User_Agent & CRLF &
     "connection: close" & CRLF;
   Key_Name      : constant String := "x-api-key: ";
   Length_Name   : constant String := "content-length: ";

   function Header_Length (Host, Key : String; Content_Length : Natural) return Natural is
     (Request_Line'Length + Host_Name'Length + Host'Length + CRLF'Length +
      Fixed_Headers'Length + Key_Name'Length + Key'Length + CRLF'Length +
      Length_Name'Length + Decimal_Length (Content_Length) + CRLF'Length + CRLF'Length);

   procedure Write_Header
     (Host, Key : String; Content_Length : Natural; Into : out String)
   is
      Last : Natural := 0;
   begin
      Into := [others => ' '];
      Put (Into, Last, False, Request_Line);
      Put (Into, Last, False, Host_Name);
      Put (Into, Last, False, Host);
      Put (Into, Last, False, CRLF);
      Put (Into, Last, False, Fixed_Headers);
      Put (Into, Last, False, Key_Name);
      Put (Into, Last, False, Key);
      Put (Into, Last, False, CRLF);
      Put (Into, Last, False, Length_Name);
      Put (Into, Last, False, Image (Content_Length));
      Put (Into, Last, False, CRLF);
      Put (Into, Last, False, CRLF);
   end Write_Header;
end Llm_Requests;
