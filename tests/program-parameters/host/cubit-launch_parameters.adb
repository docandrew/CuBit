--  Test body of CuBit.Launch_Parameters (Linux-hosted): the same search of
--  the trailer as the runtime's, over Launch_Double instead of the mapped
--  block; Refuse raises Launch_Double.Refused instead of exiting.
with CuBit.Outlet_Rings;
with Launch_Double;

package body CuBit.Launch_Parameters is
   package PR renames CuBit.Outlet_Rings;
   package PS renames CuBit.Parameter_Sections;
   use type PS.Validation;
   use type CuBit.Parameter_Signatures.Identity;

   procedure Get
     (Expected : CuBit.Parameter_Signatures.Identity;
      Value    : out String;
      Length   : out Natural;
      Result   : out Outcome)
   is
      Trailer : constant PR.Bytes (1 .. Launch_Double.Trailer_Length) :=
        [for K in 1 .. Launch_Double.Trailer_Length => Launch_Double.Trailer (K)];
      Rings_Present, Present : Boolean;
      Ring_Length : PR.Table_Length;
      Section_Length : PS.Section_Length;
   begin
      Value := [others => ' '];
      Length := 0;
      Result := No_Launch_Block;
      if not Launch_Double.Present then
         return;
      end if;
      Result := No_Parameters;
      PR.Measure (Trailer, Rings_Present, Ring_Length);
      declare
         Rest : constant PS.Bytes (1 .. Trailer'Length - Ring_Length) :=
           [for K in 1 .. Trailer'Length - Ring_Length => Trailer (Ring_Length + K)];
      begin
         PS.Measure (Rest, Present, Section_Length);
         if not Present then
            return;
         elsif PS.Validate (Rest (1 .. Section_Length)) /= PS.Valid then
            Result := Malformed_Section;
            return;
         elsif PS.Identity_Of (Rest (1 .. Section_Length)) /= Expected then
            Result := Other_Signature;
            return;
         end if;
         Length := Section_Length - PS.Header_Bytes;
         for K in 1 .. Length loop
            Value (K) := Character'Val (Rest (PS.Header_Bytes + K));
         end loop;
         Result := Found;
      end;
   end Get;

   procedure Refuse (Program : String; Why : Outcome) is
   begin
      raise Launch_Double.Refused with Program & ": " & Outcome'Image (Why);
   end Refuse;
end CuBit.Launch_Parameters;
