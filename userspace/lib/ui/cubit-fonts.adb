package body CuBit.Fonts is
   use type Interfaces.Unsigned_32;
   subtype Code is Natural range 32 .. 126;
   Fallback_Code : constant Code := Character'Pos ('?');

   function Native_Glyph (Font, Size, Code, At_Phase : Interfaces.Unsigned_32)
     return Glyph_Access
     with Import, Convention => C, External_Name => "cubit_font_glyph";

   type Advances is array (Code) of Interfaces.Unsigned_32 with Convention => C;
   type Pairs is array (Code, Code) of Interfaces.Integer_16 with Convention => C;
   type Layout is record
      Advance : Advances;
      Kerning : Pairs;
   end record with Convention => C;
   type Layout_Access is access constant Layout with Convention => C;
   function Native_Layout (Font : Interfaces.Unsigned_32) return Layout_Access
     with Import, Convention => C, External_Name => "cubit_font_layout";

   --  Defensive fallback if the bundled asset cannot be decoded; do not
   --  dereference a failed FFI result. This is not a second font backend.
   Missing : aliased constant Glyph :=
     (Advance => Mono_Width * Glyph_Pen.Unit, Height => Line_Height, Origin => Origin,
      Ink_Left | Ink_Top | Ink_Width | Ink_Height | Reserved => 0,
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

   function Code_Of (Ch : Character) return Code is
     (if Character'Pos (Ch) in Code then Character'Pos (Ch) else Fallback_Code);

   procedure Lay_Out (Font : Face; Text : String; X : Glyph_Pen.Pixel) is
      Table : constant Layout_Access :=
        (if Font = Monospace then null else Native_Layout (Face'Pos (Font)));
      Pen : Glyph_Pen.Position := Glyph_Pen.Start (X);
      Previous : Code := Fallback_Code;
      Current : Code;
   begin
      for I in Text'Range loop
         Current := Code_Of (Text (I));
         if Table /= null and then I > Text'First then
            Pen := Glyph_Pen.Step
              (Pen, 0, Integer'Max (-Glyph_Pen.Kerning_Limit, Integer'Min
                 (Glyph_Pen.Kerning_Limit, Integer (Table.Kerning (Previous, Current)))));
         end if;
         Visit (I, Text (I), Pen);
         Pen := Glyph_Pen.Step
           (Pen, (if Table = null then Mono_Width * Glyph_Pen.Unit
                  else Natural (Interfaces.Unsigned_32'Min (Table.Advance (Current), Glyph_Pen.Advance_Limit))), 0);
         Previous := Current;
      end loop;
   end Lay_Out;

   function Width (Font : Face; Text : String) return Natural is
      Last : Glyph_Pen.Position := 0;
      Table : constant Layout_Access :=
        (if Font = Monospace then null else Native_Layout (Face'Pos (Font)));
   begin
      if Table = null then
         return Text'Length * Mono_Width;
      end if;
      declare
         procedure Note (Index : Positive; Ch : Character; Pen : Glyph_Pen.Position) is
            pragma Unreferenced (Index);
         begin
            Last := Glyph_Pen.Step
              (Pen, Natural (Interfaces.Unsigned_32'Min (Table.Advance (Code_Of (Ch)), Glyph_Pen.Advance_Limit)), 0);
         end Note;
         procedure Measure is new Lay_Out (Note);
      begin
         Measure (Font, Text, 0);
      end;
      return Glyph_Pen.Nearest (Last);
   end Width;

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
