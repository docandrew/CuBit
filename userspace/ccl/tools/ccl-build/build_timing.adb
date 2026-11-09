with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

package body Build_Timing is
   use Ada.Real_Time;
   use Ada.Strings.Unbounded;

   Schema : constant String := "ccl-build.timing/1";
   Line_Prefix : constant String := "timing ";
   Microseconds_Per_Second : constant := 1_000_000;
   --  Scopes nest this deep at most (a step's outputs: Store_Write inside
   --  Output_Hash's Put_File inside Recall's Contains, and so on); deeper
   --  ones are charged to the innermost tracked scope.
   Maximum_Depth : constant := 32;
   type Depth_Count is range 0 .. Maximum_Depth;
   subtype Depth_Index is Depth_Count range 1 .. Maximum_Depth;

   Is_Enabled : Boolean := False;
   Started : constant Time := Clock;
   Last_Edge : Time := Started;
   Spans : Category_Spans := [others => Time_Span_Zero];
   Counts : Counter_Values := [others => 0];
   Stack : array (Depth_Index) of Category := [others => Planning];
   Depth : Depth_Count := 0;
   Untracked : Natural := 0;   --  scopes entered beyond Maximum_Depth

   function Current return Category is (if Depth = 0 then Planning else Stack (Depth));

   procedure Charge (At_Time : Time) is
   begin
      Spans (Current) := Spans (Current) + (At_Time - Last_Edge);
      Last_Edge := At_Time;
   end Charge;

   procedure Enable is
   begin
      Is_Enabled := True;
   end Enable;

   function Enabled return Boolean is (Is_Enabled);

   procedure Enter (C : Category) is
   begin
      Charge (Clock);
      if Depth = Maximum_Depth then
         Untracked := Untracked + 1;
      else
         Depth := Depth + 1;
         Stack (Depth) := C;
      end if;
   end Enter;

   procedure Leave is
   begin
      Charge (Clock);
      if Untracked > 0 then
         Untracked := Untracked - 1;
      elsif Depth > 0 then
         Depth := Depth - 1;
      end if;
   end Leave;

   overriding procedure Initialize (S : in out Scope) is
   begin
      Enter (S.C);
   end Initialize;

   overriding procedure Finalize (S : in out Scope) is
   begin
      Leave;
   end Finalize;

   procedure Count (C : Counter; By : Unsigned_64 := 1) is
   begin
      Counts (C) := (if Unsigned_64'Last - Counts (C) < By then Unsigned_64'Last else Counts (C) + By);
   end Count;

   function Now return Totals is
      At_Time : constant Time := Clock;
   begin
      Charge (At_Time);
      return (At_Time => At_Time, Spans => Spans, Counts => Counts);
   end Now;

   function Microseconds (Span : Time_Span) return Unsigned_64 is
      Seconds : constant Duration := To_Duration (Span);
   begin
      return (if Seconds <= 0.0 then 0 else Unsigned_64 (Seconds * Microseconds_Per_Second));
   end Microseconds;

   function Image (Value : Unsigned_64) return String is
     (Ada.Strings.Fixed.Trim (Unsigned_64'Image (Value), Ada.Strings.Left));

   function Name (Text : String) return String is
     (Ada.Characters.Handling.To_Lower (Text));

   --  A JSON string: quotes, backslashes and control characters escaped.
   function Quoted (Text : String) return String is
      Result : Unbounded_String := To_Unbounded_String ("""");
   begin
      for C of Text loop
         case C is
            when '"' | '\' => Append (Result, '\' & C);
            when ASCII.NUL .. ASCII.US => Append (Result, ' ');
            when others => Append (Result, C);
         end case;
      end loop;
      return To_String (Result) & """";
   end Quoted;

   --  "us":{...},"counts":{...} for the interval Start .. Finish.
   function Fields (Start, Finish : Totals) return String is
      Result : Unbounded_String := To_Unbounded_String ("""us"":{");
      First : Boolean := True;
   begin
      for C in Category loop
         declare
            Used : constant Unsigned_64 := Microseconds (Finish.Spans (C) - Start.Spans (C));
         begin
            if Used > 0 then
               Append (Result, (if First then "" else ",") & Quoted (Name (C'Image)) & ":" & Image (Used));
               First := False;
            end if;
         end;
      end loop;
      Append (Result, "},""counts"":{");
      First := True;
      for C in Counter loop
         if Finish.Counts (C) > Start.Counts (C) then
            Append (Result, (if First then "" else ",") & Quoted (Name (C'Image)) & ":" &
                            Image (Finish.Counts (C) - Start.Counts (C)));
            First := False;
         end if;
      end loop;
      return To_String (Result) & "}";
   end Fields;

   procedure Put (Kind, Body_Text : String) is
   begin
      Ada.Text_IO.Put_Line (Line_Prefix & "{""schema"":" & Quoted (Schema) & ",""kind"":" & Quoted (Kind) &
                            "," & Body_Text & "}");
      Ada.Text_IO.Flush;
   end Put;

   procedure Report_Step (Label, Identity : String; Ran : Boolean; Start : Totals) is
      Finish : constant Totals := Now;
   begin
      if Is_Enabled then
         Put ("step", """label"":" & Quoted (Label) & ",""identity"":" & Quoted (Identity) &
                     ",""ran"":" & (if Ran then "true" else "false") &
                     ",""wall_us"":" & Image (Microseconds (Finish.At_Time - Start.At_Time)) &
                     "," & Fields (Start, Finish));
      end if;
   end Report_Step;

   procedure Report_Build (Components : String) is
      Start : constant Totals :=
        (At_Time => Started, Spans => [others => Time_Span_Zero], Counts => [others => 0]);
      Finish : constant Totals := Now;
   begin
      if Is_Enabled then
         Put ("build", """components"":" & Quoted (Components) &
                      ",""wall_us"":" & Image (Microseconds (Finish.At_Time - Start.At_Time)) &
                      "," & Fields (Start, Finish));
      end if;
   end Report_Build;

end Build_Timing;
