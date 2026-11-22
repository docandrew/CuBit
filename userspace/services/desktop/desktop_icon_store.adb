with Desktop_Icon_Store.Atlas_Memory;
package body Desktop_Icon_Store with SPARK_Mode,
  Refined_State => (State => (Phase_Now, Decoder, Mismatch, Starved, Desktop_Icon_Store.Atlas_Memory.Block))
is
   use type Q.Phase;
   package Memory renames Desktop_Icon_Store.Atlas_Memory;

   Phase_Now : Status := Not_Loaded;
   Decoder   : Q.Decoder;
   Mismatch  : Boolean := False;
   Starved   : Boolean := False;   --  Begin_Load found no memory

   function Current return Status is (Phase_Now) with Refined_Global => Phase_Now;

   function Decoding return Boolean is
     (Memory.Reserved and then Q.Valid (Decoder) and then Q.Limit (Decoder) = Atlas_Pixels)
     with Global => (Decoder, Memory.Block);

   function Pixel (Icon : Desktop_Icons.Icon_ID; Edge : Edge_Index; X, Y : Coordinate)
     return Interfaces.Unsigned_32 is
     (if Phase_Now = Loaded
      then Memory.Read
        ((Desktop_Icons.Icon_ID'Pos (Icon) * Desktop_Icons.Edges (Edge) + Y) * Atlas_Width +
         Desktop_Icons.Columns (Edge) + X)
      else Fallback (Desktop_Icons.Edges (Edge), X, Y))
     with Refined_Global => (Phase_Now, Memory.Block);

   procedure Begin_Load is
      Reserved : Boolean;
   begin
      Memory.Reserve (Reserved);
      Mismatch := False;
      Starved := not Reserved;
      if Reserved then
         Q.Start (Decoder, Atlas_Pixels);
         Phase_Now := Loading;
      else
         Phase_Now := Unavailable;
      end if;
   end Begin_Load;

   procedure Feed (Input : Q.Byte_Array) is
   begin
      if Mismatch or else not Decoding or else Q.Current (Decoder) = Q.Failed then
         return;
      end if;
      Memory.Feed (Decoder, Input);
      --  Reject another size as soon as the header says so.
      if Q.Current (Decoder) in Q.Reading_Pixels | Q.Reading_Marker | Q.Complete and then
        (Q.Width (Decoder) /= Atlas_Width or else Q.Height (Decoder) /= Atlas_Height)
      then
         Mismatch := True;
      end if;
   end Feed;

   function Wants_More return Boolean is
     (not Mismatch and then Decoding and then Q.Current (Decoder) /= Q.Failed)
     with Refined_Global => (Input => (Mismatch, Decoder, Memory.Block), Proof_In => Phase_Now);

   procedure End_Load (Read_Failed : Boolean; Result : out Load_Result) is
   begin
      Result := (No_Memory => Starved, others => <>);
      if Decoding then
         Q.Finish (Decoder);
         Result := (Loaded => not Read_Failed and then not Mismatch and then
                              Q.Current (Decoder) = Q.Complete,
                    Error => Q.Error (Decoder), Wrong_Size => Mismatch, No_Memory => False,
                    Width => Q.Width (Decoder), Height => Q.Height (Decoder));
      end if;
      Phase_Now := (if Result.Loaded then Loaded else Unavailable);
   end End_Load;
end Desktop_Icon_Store;
