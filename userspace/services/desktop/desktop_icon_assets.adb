with Interfaces; use Interfaces;
with CuBit.QOI;
with Desktop_Asset_Files;
with Desktop_Icon_Store;
with Desktop_Logs;

package body Desktop_Icon_Assets is
   package Store renames Desktop_Icon_Store;
   package Files renames Desktop_Asset_Files;
   package Q renames CuBit.QOI;
   use type Store.Status;

   LF : constant Character := Character'Val (10);

   --  The whole read ends by this deadline; startup never waits longer on
   --  the filesystem for icons.
   Load_Budget_Ms : constant := 1_000;
   --  The worst QOI stream for the atlas: a five-byte Op_RGBA chunk per
   --  pixel. Longer files are rejected without reading them all.
   Op_RGBA_Bytes : constant := 5;
   Maximum_Bytes : constant Unsigned_64 :=
     Q.Header_Bytes + Q.Marker_Bytes + Op_RGBA_Bytes * Store.Atlas_Pixels;

   function Reason (Error : Q.Failure) return String is
     (case Error is
        when Q.No_Failure => "no image",
        when Q.Bad_Magic => "not a QOI file",
        when Q.Bad_Size => "invalid size",
        when Q.Over_Limit => "larger than the atlas",
        when Q.Bad_Channels => "invalid channel count",
        when Q.Bad_Colorspace => "invalid colour space",
        when Q.Run_Past_End => "run past the last pixel",
        when Q.Bad_Marker => "bad end marker",
        when Q.Trailing_Data => "data after the end marker",
        when Q.Truncated => "truncated");

   function Wants_More return Boolean is (Store.Wants_More);
   procedure Read is new Files.Read (Feed => Store.Feed, Wants_More => Wants_More);

   procedure Load is
      Result : Files.Read_Result;
      Path : String (1 .. Files.Maximum_Path);
      Length : Natural;
      Decoded : Store.Load_Result;
      Fallback : constant String := "; drawing the fallback icon glyph";
   begin
      if Store.Current /= Store.Not_Loaded then
         return;
      end if;
      Store.Begin_Load;
      Read (Package_Path & File_Name, Maximum_Bytes, Load_Budget_Ms, Result, Path, Length);
      Store.End_Load (not Files.Read_Whole (Result), Decoded);
      declare
         Shown : constant String :=
           (if Length = 0 then Package_Path & File_Name else Path (1 .. Length));
      begin
         if Decoded.Loaded then
            Desktop_Logs.Write ("desktop: icons loaded " & Shown & " bytes=" &
              Result.Bytes'Image & " ms=" & Result.Elapsed_Ms'Image & LF);
         elsif not Files.Read_Whole (Result) then
            Desktop_Logs.Warn ("desktop: icons " & Shown & " unavailable: " &
              Files.Describe (Result) & Fallback);
         elsif Decoded.Wrong_Size then
            Desktop_Logs.Warn ("desktop: icons " & Shown & " rejected: size" &
              Decoded.Width'Image & " x" & Decoded.Height'Image & " is not the expected" &
              Natural'Image (Store.Atlas_Width) & " x" & Natural'Image (Store.Atlas_Height) &
              Fallback);
         else
            Desktop_Logs.Warn ("desktop: icons " & Shown & " rejected: " &
              Reason (Decoded.Error) & Fallback);
         end if;
      end;
   end Load;
end Desktop_Icon_Assets;
