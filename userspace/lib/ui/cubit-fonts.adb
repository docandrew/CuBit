package body CuBit.Fonts is
   use type Interfaces.Unsigned_32;
   subtype Code is Natural range 32 .. 126;
   subtype Extended_Code is Natural range 16#80# .. 16#FF#;
   Fallback_Code : constant Code := Character'Pos ('?');

   function Native_Glyph (Font, Size, Code, At_Phase : Interfaces.Unsigned_32)
     return Glyph_Access
     with Import, Convention => C, External_Name => "cubit_font_glyph";

   type Advances is array (Code) of Interfaces.Unsigned_32 with Convention => C;
   type Pairs is array (Code, Code) of Interfaces.Integer_16 with Convention => C;
   type Extended_Advances is array (Extended_Code) of Interfaces.Unsigned_32 with Convention => C;
   type Layout is record
      Advance : Advances;
      Kerning : Pairs;
      Extended : Extended_Advances;
   end record with Convention => C;
   type Layout_Access is access constant Layout with Convention => C;
   function Native_Layout (Font : Interfaces.Unsigned_32) return Layout_Access
     with Import, Convention => C, External_Name => "cubit_font_layout";

   function Native_Register (Font : Interfaces.Unsigned_32; Bytes : System.Address; Length : Interfaces.Unsigned_64)
     return Interfaces.Unsigned_32
     with Import, Convention => C, External_Name => "cubit_font_register";

   --  Defensive fallback if the bundled asset cannot be decoded; do not
   --  dereference a failed FFI result. This is not a second font backend.
   Missing : aliased constant Glyph :=
     (Advance => Mono_Width * Glyph_Pen.Unit, Height => Line_Height, Origin => Origin,
      Ink_Left | Ink_Top | Ink_Width | Ink_Height | Baseline => 0,
      Alpha => [others => [others => 0]]);

   function Get (Font : Face; Ch : Character;
                 Size : Raster_Size := Normal; At_Phase : Phase := 0) return not null Glyph_Access
   is
      Result : constant Glyph_Access := Native_Glyph
        (Face'Pos (Font), Raster_Size'Pos (Size), Character'Pos (Ch),
         Interfaces.Unsigned_32 (At_Phase));
   begin
      return (if Result = null then Missing'Access else Result);
   end Get;

   procedure Register
     (Slot : Reading_Face; Bytes : System.Address; Length : Natural; Status : out Register_Status)
   is
      Answer : constant Interfaces.Unsigned_32 :=
        Native_Register (Face'Pos (Slot), Bytes, Interfaces.Unsigned_64 (Length));
   begin
      Status := (case Answer is when 0 => Registered, when 2 => Not_A_Font, when others => Refused);
   end Register;

   function Available (Font : Face) return Boolean is
     (Font = Monospace or else Native_Layout (Face'Pos (Font)) /= null);

   --  A size's em over the base em.
   type Scale is record
      Numerator, Denominator : Positive;
   end record;
   function Scale_Of (Size : Raster_Size) return Scale is
     (case Size is when Normal => (1, 1), when Double_Size => (2, 1), when Small => (3, 4));

   function Scaled (Value : Natural; Size : Raster_Size) return Natural is
     (Value * Scale_Of (Size).Numerator / Scale_Of (Size).Denominator);
   function Scaled_Pair (Value : Integer; Size : Raster_Size) return Integer is
     (Value * Scale_Of (Size).Numerator / Scale_Of (Size).Denominator);

   --  The code a face draws for Ch (Fallback_Code when it does not).
   function Code_Of (Font : Face; Ch : Character) return Natural is
     (if Character'Pos (Ch) in Code then Character'Pos (Ch)
      elsif Font in Reading_Face and then Character'Pos (Ch) in Extended_Code then Character'Pos (Ch)
      else Fallback_Code);

   function Advance_Of (Table : Layout_Access; Ch : Natural) return Natural is
     (Natural (Interfaces.Unsigned_32'Min
        ((if Ch in Code then Table.Advance (Ch) else Table.Extended (Ch)), Glyph_Pen.Advance_Limit)))
   with Pre => Table /= null and then (Ch in Code or else Ch in Extended_Code);

   procedure Lay_Out (Font : Face; Text : String; X : Glyph_Pen.Pixel; Size : Raster_Size := Normal) is
      Table : constant Layout_Access :=
        (if Font = Monospace then null else Native_Layout (Face'Pos (Font)));
      Pen : Glyph_Pen.Position := Glyph_Pen.Start (X);
      Previous : Natural := Fallback_Code;
      Current : Natural;
   begin
      for I in Text'Range loop
         Current := Code_Of (Font, Text (I));
         if Table /= null and then I > Text'First and then Previous in Code and then Current in Code then
            Pen := Glyph_Pen.Step
              (Pen, 0, Integer'Max (-Glyph_Pen.Kerning_Limit, Integer'Min
                 (Glyph_Pen.Kerning_Limit, Scaled_Pair (Integer (Table.Kerning (Previous, Current)), Size))));
         end if;
         Visit (I, Text (I), Pen);
         Pen := Glyph_Pen.Step
           (Pen, Natural'Min (Glyph_Pen.Advance_Limit,
                              Scaled ((if Table = null then Mono_Width * Glyph_Pen.Unit
                                       else Advance_Of (Table, Current)), Size)), 0);
         Previous := Current;
      end loop;
   end Lay_Out;

   function Width (Font : Face; Text : String; Size : Raster_Size := Normal) return Natural is
      Last : Glyph_Pen.Position := 0;
      Table : constant Layout_Access :=
        (if Font = Monospace then null else Native_Layout (Face'Pos (Font)));
   begin
      if Font = Monospace then
         return Scaled (Text'Length * Mono_Width, Size);
      elsif Table = null then
         return 0;   --  a reading face not registered: nothing is drawn
      end if;
      declare
         procedure Note (Index : Positive; Ch : Character; Pen : Glyph_Pen.Position) is
            pragma Unreferenced (Index);
         begin
            Last := Glyph_Pen.Step
              (Pen, Natural'Min (Glyph_Pen.Advance_Limit, Scaled (Advance_Of (Table, Code_Of (Font, Ch)), Size)), 0);
         end Note;
         procedure Measure is new Lay_Out (Note);
      begin
         Measure (Font, Text, 0, Size);
      end;
      return Glyph_Pen.Nearest (Last);
   end Width;

   function Baseline (Font : Face; Size : Raster_Size := Normal) return Natural is
     (Natural (Interfaces.Unsigned_32'Min (Get (Font, 'x', Size).Baseline, Interfaces.Unsigned_32 (Max_Width))));
   function Line (Font : Face; Size : Raster_Size := Normal) return Natural is
     (Natural (Interfaces.Unsigned_32'Min (Get (Font, 'x', Size).Height, Interfaces.Unsigned_32 (2 * Line_Height))));

   function Boundary (Font : Face; Text : String; Count : Natural) return Natural is
   begin
      if Count = 0 or else Text'Length = 0 then
         return 0;
      elsif Count >= Text'Length then
         return Width (Font, Text);
      end if;
      declare
         Last : constant Positive := Text'First + Count;
         Result : Natural := 0;
         procedure Note (Index : Positive; Ch : Character; Pen : Glyph_Pen.Position) is
            pragma Unreferenced (Ch);
         begin
            if Index = Last then Result := Glyph_Pen.Nearest (Pen); end if;
         end Note;
         procedure Find is new Lay_Out (Note);
      begin
         --  Through character Last: its pen includes the kerning before it.
         Find (Font, Text (Text'First .. Last), 0);
         return Result;
      end;
   end Boundary;

   function Boundary_At (Font : Face; Text : String; X : Natural) return Natural is
      Result : Natural := Text'Length;
      Found : Boolean := False;
      Previous : Natural := 0;
      procedure Note (Index : Positive; Ch : Character; Pen : Glyph_Pen.Position) is
         pragma Unreferenced (Ch);
         Here : constant Natural := Glyph_Pen.Nearest (Pen);
      begin
         --  Left of the midpoint between the boundaries around the previous
         --  character: the caret goes before it.
         if not Found and then Index > Text'First and then X < (Previous + Here) / 2 then
            Result := Index - 1 - Text'First;
            Found := True;
         end if;
         Previous := Here;
      end Note;
      procedure Scan is new Lay_Out (Note);
   begin
      if Text'Length = 0 then
         return 0;
      end if;
      Scan (Font, Text, 0);
      if not Found and then X < (Previous + Width (Font, Text)) / 2 then
         Result := Text'Length - 1;
      end if;
      return Result;
   end Boundary_At;
end CuBit.Fonts;
