pragma Ada_2022;
with Ada.Text_IO;
with Interfaces;
with Firmware_Tables;
with Fixture;
with ACPI_Host_Bridge_Resources;
with ACPI_Host_Bridge_Projection;
with ACPI_Host_Bridge_Wire;
with ACPI_Host_Bridge_Snapshot;
procedure Snapshot_Tests is
   package H renames ACPI_Host_Bridge_Resources;
   package P renames ACPI_Host_Bridge_Projection;
   package W renames ACPI_Host_Bridge_Wire;
   package S renames ACPI_Host_Bridge_Snapshot;
   use type H.Collection_Status, P.Outcome, S.Outcome, S.Header;
   use type W.Word, W.Row, Firmware_Tables.Byte;
   use type S.M.Failure_Reason;
   Items : H.Descriptor_List (7 .. 26);
   Rows : W.Rows (41 .. 60);
   Collected : H.Collection_Report;
   Projected : P.Outcome;
   Info : constant S.Header := (State => S.M.Complete, Root_ID => 16#F123_4567_ABCD_0123#,
     Sequence => W.Word'Last, Segment => (True, 65_535), Base_Bus => (True, 255),
     Required_Descriptors => 20, Reason => S.M.No_Failure);
   Need : constant Natural := S.Header_Bytes + Rows'Length * S.Row_Bytes + Fixture.Data'Length;
   Packet : Firmware_Tables.Bytes (77 .. 77 + Need + 9);
   Decoded : S.Header;
   Out_Rows : W.Rows (91 .. 111);
   Source : Firmware_Tables.Bytes (19 .. 19 + Fixture.Data'Length + 5);
   Encoded, Result : S.Report;
   Checks : Natural := 0;
   Unknown_Reasons : constant array (Positive range <>) of W.Word := [9, W.Word'Last];
   Reason_Word_Index : constant Natural := S.Header_Words - 1;
   procedure Check (OK : Boolean) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Checks'Image; end if; end Check;
   procedure Cleared is
   begin
      Check (Decoded.Root_ID = 0 and Result.Used = 0);
      for Row of Out_Rows loop Check (Row = W.Empty_Row); end loop;
      for Byte of Source loop Check (Byte = 0); end loop;
   end Cleared;
   procedure Set_Word (Data : in out Firmware_Tables.Bytes; Index : Natural; Value : W.Word) is
   begin
      for I in 0 .. S.Word_Bytes - 1 loop
         Data (Data'First + Index * S.Word_Bytes + I) :=
           Firmware_Tables.Byte (Interfaces.Shift_Right (Value, I * 8) and 16#FF#);
      end loop;
   end Set_Word;
   procedure Roundtrip (Raw : Firmware_Tables.Bytes) is
      Input : H.Descriptor_List (1 .. 1);
      Output, Back : W.Rows (Positive'Last .. Positive'Last);
      Raw_Back : Firmware_Tables.Bytes (Natural'Last - Raw'Length + 1 .. Natural'Last);
      Header : constant S.Header := (State => S.M.Complete, Root_ID => 1,
         Sequence => 1, Required_Descriptors => 1, others => <>);
      Data : Firmware_Tables.Bytes (1 .. S.Header_Bytes + S.Row_Bytes + Raw'Length);
   begin
      H.Collect (Raw, Input, Collected); Check (Collected.Status = H.Complete);
      P.Project (Input, 1, H.R.Byte_Count (Raw'Length), Output, Projected); Check (Projected = P.Complete);
      S.Encode (Header, Output, Raw, Data, Result); Check (Result.Status = S.Done);
      S.Decode (Data, Decoded, Back, Raw_Back, Result);
      Check (Result.Status = S.Done and Decoded = Header and Back (Back'First) = Output (Output'First));
      for I in 0 .. Raw'Length - 1 loop Check (Raw_Back (Raw_Back'First + I) = Raw (Raw'First + I)); end loop;
   end Roundtrip;
begin
   H.Collect (Fixture.Data, Items, Collected); Check (Collected.Status = H.Complete);
   P.Project (Items, Collected.Count, H.R.Byte_Count (Fixture.Data'Length), Rows, Projected);
   Check (Projected = P.Complete);
   S.Encode (Info, Rows, Fixture.Data, Packet, Encoded);
   Check (Encoded.Status = S.Done and Encoded.Used = Need);
   Check (Packet (Packet'First) = Character'Pos ('A') and Packet (Packet'First + 7) = Character'Pos ('1'));
   for I in Packet'First + Need .. Packet'Last loop Check (Packet (I) = 0); end loop;
   S.Decode (Packet (Packet'First .. Packet'First + Need - 1), Decoded, Out_Rows, Source, Result);
   Check (Result.Status = S.Done and Decoded = Info);
   for I in 0 .. Rows'Length - 1 loop Check (Out_Rows (Out_Rows'First + I) = Rows (Rows'First + I)); end loop;
   Check (Out_Rows (Out_Rows'Last) = W.Empty_Row);
   for I in Fixture.Data'Range loop Check (Source (Source'First + I - Fixture.Data'First) = Fixture.Data (I)); end loop;
   for Prefix in 0 .. Need - 1 loop
      S.Decode (Packet (Packet'First .. Packet'First + Prefix - 1), Decoded, Out_Rows, Source, Result);
      Check (Result.Status /= S.Done); Cleared;
   end loop;
   S.Decode (Packet (Packet'First .. Packet'First + Need), Decoded, Out_Rows, Source, Result);
   Check (Result.Status = S.Invalid_Length); Cleared;
   declare Small_Rows : W.Rows (1 .. 19); Small_Source : Firmware_Tables.Bytes (1 .. 1); begin
      S.Decode (Packet (Packet'First .. Packet'First + Need - 1), Decoded, Small_Rows, Small_Source, Result);
      Check (Result.Status = S.Insufficient_Capacity and Result.Required_Rows = 20);
      Check (Result.Required_Source_Bytes = Fixture.Data'Length and Decoded.Root_ID = 0);
      for Row of Small_Rows loop Check (Row = W.Empty_Row); end loop;
      Check (Small_Source (1) = 0);
   end;
   declare Small : Firmware_Tables.Bytes (1 .. Need - 1); begin
      S.Encode (Info, Rows, Fixture.Data, Small, Result);
      Check (Result.Status = S.Insufficient_Capacity and Result.Used = 0);
      for Byte of Small loop Check (Byte = 0); end loop;
   end;
   -- Mutate every structural header field and the last row. Refusals never
   -- publish an earlier valid row or retain data from the preceding success.
   for Field in 0 .. 13 loop
      declare Bad : Firmware_Tables.Bytes := Packet (Packet'First .. Packet'First + Need - 1); begin
         Set_Word (Bad, Field, (if Field in 5 | 6 then 0 else W.Word'Last));
         S.Decode (Bad, Decoded, Out_Rows, Source, Result);
         Check (Result.Status /= S.Done); Cleared;
      end;
   end loop;
   for Field in W.Column loop
      declare Bad : Firmware_Tables.Bytes := Packet (Packet'First .. Packet'First + Need - 1); begin
         Set_Word (Bad, S.Header_Words + 19 * S.Row_Words + W.Column'Pos (Field), W.Word'Last);
         S.Decode (Bad, Decoded, Out_Rows, Source, Result);
         -- Full-width geometry values are descriptive and can legitimately
         -- hold all bits; bounded tags/flags/offsets must be refused.
         if Field in W.Ordinal | W.Form | W.Kind | W.Address_Bits | W.General_Flags | W.Specific_Flags
           | W.Claims | W.Cache | W.Writable | W.Source_Index | W.Source_Name_Offset
           | W.Source_Name_Length | W.Descriptor_Offset | W.Descriptor_Extent
           | W.Encoded_Alignment | W.Decoding_Bits | W.Vendor_Type
         then Check (Result.Status /= S.Done); Cleared; end if;
      end;
   end loop;
   for State in S.M.Incomplete .. S.M.Failed loop
      declare No_Rows : W.Rows (1 .. 0); No_Bytes : Firmware_Tables.Bytes (1 .. 0);
         Header : S.Header := (State => State, Root_ID => 1, Sequence => 2,
           Required_Descriptors => W.Word'Last, Reason => S.M.Malformed_Resource, others => <>);
         Data : Firmware_Tables.Bytes (Natural'Last - S.Header_Bytes + 1 .. Natural'Last);
      begin
         for Reason in S.M.Failure_Reason loop
            if Reason /= S.M.No_Failure then
               Header.Reason := Reason;
               S.Encode (Header, No_Rows, No_Bytes, Data, Result); Check (Result.Status = S.Done);
               S.Decode (Data, Decoded, No_Rows, No_Bytes, Result);
               Check (Result.Status = S.Done and Decoded = Header);
               for Unknown of Unknown_Reasons loop
                  Set_Word (Data, Reason_Word_Index, Unknown);
                  S.Decode (Data, Decoded, No_Rows, No_Bytes, Result);
                  Check (Result.Status = S.Invalid_Header and Decoded.Root_ID = 0 and Result.Used = 0);
               end loop;
            end if;
         end loop;
         S.Encode (Header, No_Rows, No_Bytes, Data, Result); Check (Result.Status = S.Done and Result.Used = S.Header_Bytes);
         S.Decode (Data, Decoded, No_Rows, No_Bytes, Result); Check (Result.Status = S.Done and Decoded = Header);
         Header.Reason := S.M.No_Failure; S.Encode (Header, No_Rows, No_Bytes, Data, Result);
         Check (Result.Status = S.Invalid_Header); for Byte of Data loop Check (Byte = 0); end loop;
      end;
   end loop;
   declare
      Named_Bus : constant Firmware_Tables.Bytes :=
        [16#88#, 19, 0, 2, 12, 0, 0, 0, 0, 0, 255, 0, 0, 0, 0, 1, 0, 65, 66, 67, 68, 0, 16#79#, 0];
      Input : H.Descriptor_List (1 .. 1);
      Output, Back : W.Rows (1 .. 1);
      Raw, Raw_Back : Firmware_Tables.Bytes := Named_Bus;
      Header : constant S.Header := (State => S.M.Complete, Root_ID => 1,
         Sequence => 1, Required_Descriptors => 1, others => <>);
      Data : Firmware_Tables.Bytes (1 .. S.Header_Bytes + S.Row_Bytes + Named_Bus'Length);
   begin
      H.Collect (Named_Bus, Input, Collected); Check (Collected.Status = H.Complete);
      P.Project (Input, 1, H.R.Byte_Count (Raw'Length), Output, Projected);
      Check (Projected = P.Complete and Output (1) (W.Source_Name_Length) = 4);
      S.Encode (Header, Output, Raw, Data, Result); Check (Result.Status = S.Done);
      S.Decode (Data, Decoded, Back, Raw_Back, Result);
      Check (Result.Status = S.Done and Decoded = Header and Back (1) = Output (1));
      for I in 17 .. 21 loop
         Raw := Named_Bus;
         Raw (Raw'First + I) := (if I = 21 then 65 else 0);
         S.Encode (Header, Output, Raw, Data, Result);
         Check (Result.Status = S.Invalid_Row and Result.Used = 0);
         for Byte of Data loop Check (Byte = 0); end loop;
         S.Encode (Header, Output, Named_Bus, Data, Result); Check (Result.Status = S.Done);
         Data (Data'First + S.Header_Bytes + S.Row_Bytes + I) := Raw (Raw'First + I);
         S.Decode (Data, Decoded, Back, Raw_Back, Result);
         Check (Result.Status = S.Invalid_Row and Result.Used = 0 and Decoded.Root_ID = 0);
         Check (Back (1) = W.Empty_Row);
         for Byte of Raw_Back loop Check (Byte = 0); end loop;
      end loop;
   end;
   Roundtrip ([16#81#, 9, 0, 1, 16, 0, 32, 0, 0, 0, 16, 0, 16#79#, 0]);
   Roundtrip ([16#85#, 17, 0, 1, 0, 0, 16, 0, 0, 0, 32, 0,
      0, 16, 0, 0, 0, 16, 0, 0, 16#79#, 0]);
   Roundtrip ([16#86#, 9, 0, 1, 0, 0, 16, 0, 0, 16, 0, 0, 16#79#, 0]);
   Roundtrip ([16#4B#, 16#F8#, 16#03#, 8, 16#79#, 0]);
   Roundtrip ([16#47#, 1, 0, 1, 0, 2, 1, 8, 16#79#, 0]);
   declare
      Extended : Firmware_Tables.Bytes (1 .. 58) := [others => 0];
      Vendor : Firmware_Tables.Bytes :=
        [16#88#, 19, 0, 16#C7#, 12, 16#A5#, 0, 0, 0, 0, 255, 0, 0, 0, 0, 1, 0, 65, 66, 67, 68, 0, 16#79#, 0];
   begin
      Extended (1) := 16#8B#; Extended (2) := 53;
      Extended (5) := 12; Extended (6) := 1; Extended (7) := 1; Extended (57) := 16#79#;
      Roundtrip (Extended); Roundtrip (Vendor);
      Vendor (6) := 0; Roundtrip (Vendor);
   end;
   Ada.Text_IO.Put_Line ("Host bridge snapshot checks" & Checks'Image);
end Snapshot_Tests;
