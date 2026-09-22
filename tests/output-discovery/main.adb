with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Output_Discovery; use CuBit.Output_Discovery;
procedure Main is
   use type DP.Wire_Message;
   Checks : Natural := 0;
   procedure Check (Condition : Boolean) is
   begin
      Checks := Checks + 1;
      if not Condition then
         raise Program_Error with "check" & Checks'Image;
      end if;
   end Check;
   Sizes : constant array (Positive range <>) of Extent :=
     [1, 768, 1024, 1080, 1920, Extent'Last];
   Revisions : constant array (Positive range <>) of Catalog_Revision :=
     [1, 2, Catalog_Revision'Last];
   D : Description;
   W, Bad : DP.Wire_Message;
   C : Catalog;
begin
   for Service in Provider loop
      Check (Valid_Catalog_Request (Service, Catalog_Request (Service)));
      for Revision of Revisions loop
         for Count in Output_Count loop
            W := Encode_Summary (Service, (Revision, Count));
            Check (Decode_Summary (Service, W) = (True, (Revision, Count)));
            Bad := W;
            Bad.Words (2) := 18;
            Check (not Decode_Summary (Service, Bad).Valid);
            Bad := W;
            Bad.Words (1) := 0;
            Check (not Decode_Summary (Service, Bad).Valid);
         end loop;
         for Index in Output_Index loop
            W := Encode_Query (Service, (Revision, Index));
            Check (Decode_Query (Service, W) = (True, (Revision, Index)));
            Bad := W;
            Bad.Words (2) := 1;
            Check (not Decode_Query (Service, Bad).Valid);
         end loop;
      end loop;
      for Role in Output_Role loop
         for Source in Output_Source loop
            for Native in Native_Output_Number loop
               for Size of Sizes loop
                  D := (case Role is
                    when Detected_Only =>
                      (Detected_Only, Source, Native, Size, Size),
                    when Backend_Ready | Selected_For_Desktop =>
                      (Active_Role'(Role), Source, Native,
                       Size, Size, 1024, 768));
                  for Revision of Revisions loop
                     for Index in Output_Index loop
                        W := Encode_Description
                          (Service, ((Revision, Index), D));
                        Check (Decode_Description (Service, W) =
                          (True, ((Revision, Index), D)));
                        Bad := W;
                        Bad.Flags := 1;
                        Check (not Decode_Description (Service, Bad).Valid);
                        Bad := W;
                        Bad.Words (1) := W.Words (1) or 16#100_0000_0000#;
                        Check (not Decode_Description (Service, Bad).Valid);
                        Bad := W;
                        Bad.Words (2) := 0;
                        Check (not Decode_Description (Service, Bad).Valid);
                        Bad := W;
                        Bad.Words (3) :=
                          (if Role = Detected_Only then 1 else 0);
                        Check (not Decode_Description (Service, Bad).Valid);
                     end loop;
                  end loop;
               end loop;
            end loop;
         end loop;
      end loop;
      -- Every wire bit is checked: noncanonical replies must be rejected,
      -- otherwise decoding/re-encoding must preserve the message exactly.
      for Role in Output_Role loop
         D := (case Role is
           when Detected_Only => (Detected_Only, Virtio_GPU, 15, 1920, 1080),
           when Backend_Ready | Selected_For_Desktop =>
             (Active_Role'(Role), Virtio_GPU, 15, 1920, 1080, 1024, 768));
         W := Encode_Description (Service, ((7, 17), D));
         for Word in W.Words'Range loop
            for Bit in 0 .. 63 loop
               Bad := W;
               Bad.Words (Word) := Bad.Words (Word) xor Shift_Left (1, Bit);
               declare
                  Decoded : constant Description_Decoding :=
                    Decode_Description (Service, Bad);
               begin
                  Check (not Decoded.Valid or else
                    Encode_Description (Service, Decoded.Value) = Bad);
               end;
            end loop;
         end loop;
      end loop;
      for Length in Unsigned_8 loop
         Bad := W;
         Bad.Length := Length;
         Check (Decode_Description (Service, Bad).Valid = (Length = 4));
      end loop;
      for Count in Output_Count loop
         C.Count := Count;
         W := Respond (Service, C, 9, Catalog_Request (Service));
         Check (Decode_Summary (Service, W) = (True, (9, Count)));
         for Index in Output_Index loop
            W := Respond (Service, C, 9, Encode_Query (Service, (9, Index)));
            Check (Decode_Description (Service, W).Valid = (Index <= Count));
            if Index > Count then
               Check (W.Length = 1 and W.Words (0) =
                 DP.Status_Code'Enum_Rep (DP.Bad_Object));
            end if;
            W := Respond (Service, C, 9, Encode_Query (Service, (8, Index)));
            Check (W.Length = 1 and W.Words (0) =
              DP.Status_Code'Enum_Rep (DP.Bad_State));
         end loop;
      end loop;
      Bad := Catalog_Request (Service);
      Bad.Reserved := 1;
      W := Respond (Service, C, 9, Bad);
      Check (W.Length = 1 and W.Words (0) =
        DP.Status_Code'Enum_Rep (DP.Bad_Object));
   end loop;
   Put_Line ("PASS output discovery:" & Checks'Image & " checks");
end Main;
