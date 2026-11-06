pragma Ada_2022;
with Interfaces;
with CCL.Types;

package body Agent_Results with SPARK_Mode is
   use type CCL.Types.Type_Reference;
   use type CCL.Types.Shape;
   use type Interfaces.Unsigned_64;
   package Views renames CCL.Objects.Views;

   Hex : constant String (1 .. 16) := "0123456789abcdef";
   Label_Room : constant := 64;
   First_Printable : constant := 16#20#;

   function Image (Value : Interfaces.Integer_64) return String is
      Text : constant String := Interfaces.Integer_64'Image (Value);
   begin
      return (if Text'Length > 0 and then Text (Text'First) = ' ' then Text (Text'First + 1 .. Text'Last)
              else Text);
   end Image;

   --  A writer: bytes up to Room land in Into, Total counts them all.
   type Writer is record
      Last : Natural := 0;
      Room : Natural := 0;
      Total : Natural := 0;
   end record;

   procedure Put (Into : in out String; W : in out Writer; Text : String)
   with Pre => Into'First = 1 and then W.Room <= Into'Last and then W.Last <= W.Room,
        Post => W.Room = W.Room'Old and then W.Last <= W.Room
   is
   begin
      for C of Text loop
         pragma Loop_Invariant (W.Room = W.Room'Loop_Entry and then W.Last <= W.Room);
         if W.Total < Natural'Last then
            W.Total := W.Total + 1;
         end if;
         if W.Last < W.Room then
            W.Last := W.Last + 1;
            Into (W.Last) := C;
         end if;
      end loop;
   end Put;

   procedure Put_String (Into : in out String; W : in out Writer; Text : String)
   with Pre => Into'First = 1 and then W.Room <= Into'Last and then W.Last <= W.Room,
        Post => W.Room = W.Room'Old and then W.Last <= W.Room
   is
   begin
      Put (Into, W, """");
      for C of Text loop
         pragma Loop_Invariant (W.Room = W.Room'Loop_Entry and then W.Last <= W.Room);
         if C = '"' then Put (Into, W, "\""");
         elsif C = '\' then Put (Into, W, "\\");
         elsif C = ASCII.LF then Put (Into, W, "\n");
         elsif C = ASCII.CR then Put (Into, W, "\r");
         elsif C = ASCII.HT then Put (Into, W, "\t");
         elsif Character'Pos (C) < First_Printable or else C = ASCII.DEL then
            Put (Into, W, "\u00" & Hex (Character'Pos (C) / 16 + 1) & Hex (Character'Pos (C) mod 16 + 1));
         else
            Put (Into, W, [1 => C]);
         end if;
      end loop;
      Put (Into, W, """");
   end Put_String;

   procedure Value
     (Object : Views.Snapshot; At_Cursor : Views.Cursor; Depth : Natural; Into : in out String;
      W : in out Writer)
   with Pre => Depth <= Maximum_Depth and then Into'First = 1 and then W.Room <= Into'Last and then
               W.Last <= W.Room,
        Post => W.Room = W.Room'Old and then W.Last <= W.Room,
        Subprogram_Variant => (Decreases => Maximum_Depth - Depth)
   is
      Kind : constant CCL.Types.Type_Reference := Views.Type_Of (Object, At_Cursor);
      Shape : constant CCL.Types.Description := Views.Describe (Object, At_Cursor);
   begin
      if Kind = CCL.Types.String_Type then
         Put_String (Into, W, Views.Text (Object, At_Cursor));
      elsif Kind = CCL.Types.Integer_Type or else Shape.Form = CCL.Types.Bounded then
         Put (Into, W, Image (CCL.Objects.Integer_Of (Views.Scalar (Object, At_Cursor))));
      elsif Kind = CCL.Types.Boolean_Type then
         Put (Into, W, (if Views.Scalar (Object, At_Cursor).First = 1 then "true" else "false"));
      elsif Kind = CCL.Types.Character_Type then
         Put_String (Into, W, [1 => Character'Val (Views.Scalar (Object, At_Cursor).First mod 256)]);
      elsif Depth = Maximum_Depth then
         Put (Into, W, "null");
      else
         case Shape.Form is
            when CCL.Types.Product =>
               Put (Into, W, "{");
               for F in 1 .. Shape.Count loop
                  pragma Loop_Invariant (W.Room = W.Room'Loop_Entry and then W.Last <= W.Room);
                  if F > 1 then
                     Put (Into, W, ",");
                  end if;
                  Put_String (Into, W, CCL.Types.Image (Shape.Parts (F).Identifier));
                  Put (Into, W, ":");
                  Value (Object, Views.Field (Object, At_Cursor, F), Depth + 1, Into, W);
               end loop;
               Put (Into, W, "}");
            when CCL.Types.Sum =>
               declare
                  Choice : constant Natural := Views.Alternative (Object, At_Cursor);
               begin
                  if Choice not in 1 .. Shape.Count then
                     Put (Into, W, "null");
                  elsif Shape.Parts (Choice).Payload in CCL.Types.Unit_Type | CCL.Types.Invalid_Type then
                     Put_String (Into, W, CCL.Types.Image (Shape.Parts (Choice).Identifier));
                  else
                     Put (Into, W, "{");
                     Put_String (Into, W, CCL.Types.Image (Shape.Parts (Choice).Identifier));
                     Put (Into, W, ":");
                     Value (Object, Views.Payload (Object, At_Cursor), Depth + 1, Into, W);
                     Put (Into, W, "}");
                  end if;
               end;
            when CCL.Types.Sequence =>
               Put (Into, W, "[");
               for K in 1 .. Views.Length (Object, At_Cursor) loop
                  pragma Loop_Invariant (W.Room = W.Room'Loop_Entry and then W.Last <= W.Room);
                  if K > 1 then
                     Put (Into, W, ",");
                  end if;
                  Value (Object, Views.Element (Object, At_Cursor, K), Depth + 1, Into, W);
               end loop;
               Put (Into, W, "]");
            when others =>
               Put (Into, W, "null");
         end case;
      end if;
   end Value;

   --  Close a rendering: the whole of it, or its first bytes and the label.
   procedure Finish (Into : in out String; W : Writer; Last : out Natural)
   with Pre => Into'First = 1 and then Into'Length >= Label_Room and then W.Room = Into'Length - Label_Room
               and then W.Last <= W.Room,
        Post => Last <= Into'Last
   is
   begin
      Last := W.Last;
      if W.Total > W.Last then
         declare
            Label : constant String :=
              ASCII.LF & Truncation_Label & Natural'Image (W.Last) & " of" & Natural'Image (W.Total) & " bytes]";
            N : constant Natural := Natural'Min (Label'Length, Into'Last - Last);
         begin
            Into (Last + 1 .. Last + N) := Label (Label'First .. Label'First + N - 1);
            Last := Last + N;
         end;
      end if;
   end Finish;

   procedure Render
     (Object : CCL.Objects.Views.Snapshot; Into : out String; Last : out Natural; Total : out Natural)
   is
      W : Writer := (Last => 0, Room => Into'Length - Label_Room, Total => 0);
   begin
      Into := [others => ' '];
      Value (Object, Views.Root (Object), 0, Into, W);
      Total := W.Total;
      Finish (Into, W, Last);
   end Render;

   procedure Render_Text (Text : String; Into : out String; Last : out Natural) is
      W : Writer := (Last => 0, Room => Into'Length - Label_Room, Total => 0);
   begin
      Into := [others => ' '];
      Put (Into, W, Text);
      Finish (Into, W, Last);
   end Render_Text;
end Agent_Results;
