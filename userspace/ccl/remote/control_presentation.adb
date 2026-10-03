with Interfaces;
with CBOR;
with CBOR.Encoding;
with CCL.Image_Store;
with CCL.Language;
with CCL.Completions;
with CCL.Literal_Tables;
with CCL.Periodic_Programs;
with CCL.Presentations;
with CCL.Sessions;
with CCL.Types;

package body Control_Presentation is
   use CBOR;
   use type CBOR.SE_Offset;
   use type CBOR.Byte;
   use type Interfaces.Unsigned_32;
   use type Interfaces.Unsigned_64;
   use type CCL.Control.Operation;
   use type CCL.Language.Interpretation_Status;
   use type CCL.Completions.Origin;
   Max_Response : constant := Control_Wire.Max_Response;
   pragma Compile_Time_Error
     (Control_Wire.Max_Image_Side /= CCL.Image_Store.Maximum_Side,
      "the wire's image bound must be the image store's");

   procedure Encode
     (Query : Control_Wire.Request; Value : CCL.Control.Response;
      Data : out Control_Wire.Response)
   is
      Failed : Boolean := False;
      procedure Put (Bytes : Byte_Array) is
      begin
         if Bytes'Length > SE_Offset (Max_Response - Data.Length) then
            Failed := True;
         else
            Data.Data (SE_Offset (Data.Length + 1) .. SE_Offset (Data.Length + Bytes'Length)) := Bytes;
            Data.Length := Data.Length + Bytes'Length;
         end if;
      end Put;
      procedure Number (N : Interfaces.Unsigned_64) is
      begin Put (Encoding.Encode_Unsigned (N)); end Number;
      procedure Flag (B : Boolean) is
      begin Put (Encoding.Encode_Simple (if B then Simple_True else Simple_False)); end Flag;
      procedure Text (S : String) is
      begin
         if S'Length > 4096 or else (for some C of S => Character'Pos (C) >= 128) then
            Failed := True;
         else
            declare
               Header : Byte_Array := Encoding.Encode_Unsigned (UInt64 (S'Length));
               Payload : Byte_Array (1 .. SE_Offset (S'Length));
            begin
               Header (Header'First) := Header (Header'First) or 16#60#;
               for I in Payload'Range loop
                  Payload (I) := Character'Pos (S (S'First + (Natural (I) - 1)));
               end loop;
               Put (Header); Put (Payload);
            end;
         end if;
      end Text;

      --  A byte string: CBOR major type 2.
      procedure Bytes_Header (Length : Natural) is
         Header : Byte_Array := Encoding.Encode_Unsigned (UInt64 (Length));
      begin
         Header (Header'First) := Header (Header'First) or 16#40#;
         Put (Header);
      end Bytes_Header;

      --  Operation 7: [ok, typeText, valueText, form, position, fuel, detail]
      --  after the common prefix (README, "Presentations").
      procedure Presentation (Item : CCL.Language.Interpretation_Result) is
         use type CCL.Presentations.Form;
         Shown : CCL.Presentations.Presentation;
         Reserve : constant := 16;   --  the bytes after the rows: closing fields
      begin
         CCL.Presentations.Describe (Item, Shown);
         Flag (Item.Status = CCL.Language.Succeeded);
         Text (CCL.Sessions.Result_Type_Image (Item));
         --  A table's value is its rows; do not send it twice.
         Text (if Shown.Kind in CCL.Presentations.Table | CCL.Presentations.Gallery then ""
               else CCL.Sessions.Result_Value_Image (Item));
         Number (CCL.Presentations.Form'Pos (Shown.Kind));
         Number (Interfaces.Unsigned_64 (Item.Diagnostic_Position));
         Number (Interfaces.Unsigned_64 (Item.Fuel_Remaining));
         case Shown.Kind is
            when CCL.Presentations.Failure | CCL.Presentations.Text =>
               Put (Encoding.Encode_Array (0));
            when CCL.Presentations.Gallery =>
               --  [total, [[width, height, imageId]...]]
               Put (Encoding.Encode_Array (2));
               Number (Interfaces.Unsigned_64 (Shown.Cells.Total));
               Put (Encoding.Encode_Array (UInt64 (Shown.Picture_Count)));
               for K in 1 .. Shown.Picture_Count loop
                  Put (Encoding.Encode_Array (3));
                  Number (Interfaces.Unsigned_64 (Shown.Pictures (K).Width));
                  Number (Interfaces.Unsigned_64 (Shown.Pictures (K).Height));
                  Number (Interfaces.Unsigned_64 (Shown.Pictures (K).Image));
               end loop;
            when CCL.Presentations.Picture =>
               Put (Encoding.Encode_Array (3));
               Number (Interfaces.Unsigned_64 (Shown.Image_Width));
               Number (Interfaces.Unsigned_64 (Shown.Image_Height));
               Number (Interfaces.Unsigned_64 (Shown.Image));
            when CCL.Presentations.Table =>
               Put (Encoding.Encode_Array (5));
               Text (CCL.Types.Image (Shown.Shape.Row_Type));
               Flag (Shown.Shape.Many);
               Number (Interfaces.Unsigned_64 (Shown.Cells.Total));
               Put (Encoding.Encode_Array (UInt64 (Shown.Shape.Count)));
               for F in 1 .. Shown.Shape.Count loop
                  Put (Encoding.Encode_Array (3));
                  Text (CCL.Types.Image (Shown.Shape.Fields (F).Identifier));
                  Text (CCL.Types.Image (Shown.Shape.Fields (F).Type_Name));
                  Flag (Shown.Shape.Fields (F).Numeric);
               end loop;
               --  As many whole rows as the response holds; total says how
               --  many there are.
               declare
                  function Row_Bytes (Row : CCL.Literal_Tables.Row_Index) return Natural is
                     Size : Natural := 1;
                  begin
                     for C in 1 .. Shown.Shape.Count loop
                        Size := Size + 9 + CCL.Presentations.Cell_Text (Item, Shown, Row, C)'Length;
                     end loop;
                     return Size;
                  end Row_Bytes;
                  Fitting : Natural := 0;
                  Room : Natural := (if Data.Length + 9 + Reserve < Max_Response
                                     then Max_Response - Data.Length - 9 - Reserve else 0);
               begin
                  while Fitting < Shown.Cells.Rows and then Row_Bytes (Fitting + 1) <= Room loop
                     Room := Room - Row_Bytes (Fitting + 1);
                     Fitting := Fitting + 1;
                  end loop;
                  Put (Encoding.Encode_Array (UInt64 (Fitting)));
                  for R in 1 .. Fitting loop
                     Put (Encoding.Encode_Array (UInt64 (Shown.Shape.Count)));
                     for C in 1 .. Shown.Shape.Count loop
                        Text (CCL.Presentations.Cell_Text (Item, Shown, R, C));
                     end loop;
                  end loop;
               end;
         end case;
      end Presentation;

      --  Operation 8: [known, width, height, firstRow, rowCount, rgb]: as
      --  many whole rows from First as the response holds.
      procedure Image_Rows (Id : CCL.Image_Store.Image_Id; First : Interfaces.Unsigned_64) is
         Width : constant Natural := CCL.Image_Store.Width (Id);
         Height : constant Natural := CCL.Image_Store.Height (Id);
         Header_Room : constant := 48;
         Count : Natural;
      begin
         if not CCL.Image_Store.Known (Id) or else First >= Interfaces.Unsigned_64 (Height) then
            Flag (False);
            Number (0); Number (0); Number (0); Number (0);
            Bytes_Header (0);
            return;
         end if;
         Count := Natural'Min (Height - Natural (First), (Max_Response - Header_Room) / (Width * 3));
         Flag (True);
         Number (Interfaces.Unsigned_64 (Width)); Number (Interfaces.Unsigned_64 (Height));
         Number (First); Number (Interfaces.Unsigned_64 (Count));
         Bytes_Header (Count * Width * 3);
         for Y in Natural (First) .. Natural (First) + Count - 1 loop
            for X in 0 .. Width - 1 loop
               declare
                  P : constant CCL.Image_Store.Pixel := CCL.Image_Store.Pixel_At (Id, X, Y);
               begin
                  Put ([CBOR.Byte (Interfaces.Shift_Right (P, 16) and 16#FF#),
                        CBOR.Byte (Interfaces.Shift_Right (P, 8) and 16#FF#),
                        CBOR.Byte (P and 16#FF#)]);
               end;
            end loop;
         end loop;
      end Image_Rows;
   begin
      Data := (others => <>);
      Put (Encoding.Encode_Array (case Query.Op is
                                     when CCL.Control.Present_Monitor => 12,
                                     when CCL.Control.Complete_Expression => 7,
                                     when CCL.Control.Read_Image_Rows => 9,
                                     when others => 10));
      Number (Control_Wire.Protocol_Version); Number (Query.Id);
      Number (CCL.Control.Operation'Enum_Rep (Query.Op));
      if Query.Op = CCL.Control.Complete_Expression then
         --  [prefixLength, [[name, origin, signature]...], beyond, signature]
         Number (Interfaces.Unsigned_64 (Value.Completion.Prefix_Length));
         Put (Encoding.Encode_Array (UInt64 (Value.Completion.Count)));
         for I in 1 .. Value.Completion.Count loop
            declare
               C : CCL.Completions.Candidate renames Value.Completion.Candidates (I);
            begin
               Put (Encoding.Encode_Array (3));
               Text (C.Suggestion.Name (1 .. C.Suggestion.Length));
               Number (CCL.Completions.Origin'Pos (C.Origin));
               Text (CCL.Completions.Describe (C.Suggestion, C.Origin));
            end;
         end loop;
         Flag (Value.Completion.Beyond);
         Text (if Value.Completion.Signature_Visible
               then CCL.Completions.Describe (Value.Completion.Signature, Value.Completion.Signature_Origin)
               else "");
      elsif Query.Op = CCL.Control.Present_Expression then
         Presentation (Value.Outcome);
      elsif Query.Op = CCL.Control.Present_Monitor then
         --  The presentation's seven fields, then the monitor's state and
         --  completed runs: [1,id,9,...presentation...,state,runs].
         Presentation (CCL.Periodic_Programs.Last_Result (Value.Monitor));
         Number (CCL.Periodic_Programs.Lifecycle'Pos (CCL.Periodic_Programs.State (Value.Monitor)));
         Number (CCL.Periodic_Programs.Completed_Runs (Value.Monitor));
      else
         Image_Rows (CCL.Image_Store.Image_Id (Interfaces.Unsigned_64'Min
           (Query.Target, Interfaces.Unsigned_64 (CCL.Image_Store.Image_Id'Last))), Query.Row);
      end if;
      if Failed then Data.Length := 0; end if;
   end Encode;
end Control_Presentation;
