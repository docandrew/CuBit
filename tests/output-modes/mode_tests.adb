--  Hosted regression for output mode changes: the Watch/Set_Mode codecs,
--  the backend's mode fitting and Desktop's scale-preserving re-layout.
--  Linux-hosted: no GPU, no IPC.
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Desktop_Protocol;
with CuBit.Output_Discovery;
with CuBit.Output_Modes;
with CuBit.Display_Layouts;
with Compositor_Layout_Resize;

procedure Mode_Tests is
   package DP renames CuBit.Desktop_Protocol;
   package OD renames CuBit.Output_Discovery;
   package OM renames CuBit.Output_Modes;
   package L renames CuBit.Display_Layouts;
   package G renames L.G;
   package R renames Compositor_Layout_Resize;
   use type DP.Wire_Message, DP.Status_Code, OM.Mode, OM.Byte_Count, L.Layout, L.Admission_Status,
     OM.Revision_Decoding, OM.Mode_Decoding, OM.Result_Decoding, G.UI_Scale,
     G.Scale_Component, G.Logical_Coordinate, G.Pixel_Edge;
   Checks, Failures : Natural := 0;

   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & What);
      end if;
   end Check;

   procedure Codecs is
      W : DP.Wire_Message;
   begin
      for Service in OD.Provider loop
         for Known in Unsigned_64'(1) .. 50 loop
            W := OM.Encode_Watch (Service, Known);
            Check (OM.Decode_Watch (Service, W) = (True, Known), "watch round trip");
         end loop;
         W := OM.Encode_Watch (Service, 7);
         W.Words (2) := 1;
         Check (not OM.Decode_Watch (Service, W).Valid, "watch reserved word");
         W := OM.Encode_Watch (Service, 7);
         W.Words (0) := 0;
         Check (not OM.Decode_Watch (Service, W).Valid, "watch revision zero");
         W := OM.Encode_Watch (Service, 7);
         W.Reserved := 1;
         Check (not OM.Decode_Watch (Service, W).Valid, "watch routing left in");
         W := OM.Encode_Set_Mode (Service, (1920, 1080));
         Check (OM.Decode_Set_Mode (Service, W) = (True, (1920, 1080)), "mode round trip");
         W.Words (0) := 0;
         Check (not OM.Decode_Set_Mode (Service, W).Valid, "zero width");
         W := OM.Encode_Set_Mode (Service, (1920, 1080));
         W.Words (1) := 65_536;
         Check (not OM.Decode_Set_Mode (Service, W).Valid, "oversized height");
         for S in DP.Status_Code loop
            W := OM.Encode_Result (Service, (S, (1280, 800)));
            Check (OM.Decode_Result (Service, W) = (True, (S, (1280, 800))), "result round trip");
         end loop;
         W := OM.Encode_Result (Service, (DP.Success, (1280, 800)));
         W.Words (0) := 7;
         Check (not OM.Decode_Result (Service, W).Valid, "unknown status");
         --  The two services' labels differ: a GPU answer is not a display answer.
         Check (not OM.Decode_Watch
                  (OD.Provider'Val (1 - OD.Provider'Pos (Service)),
                   OM.Encode_Watch (Service, 3)).Valid, "service labels distinct");
      end loop;
   end Codecs;

   procedure Fitting is
      Minimum : constant OM.Mode := (800, 600);
      --  virtio-gpu: two buffers per 16 MiB bank after 64 KiB of rings.
      Budget : constant OM.Byte_Count := (16#100_0000# - 16#1_0000#) / 2 / 4096 * 4096;
      F : OM.Mode;
   begin
      Check (OM.Fit ((1920, 1080), Minimum, Budget) = (1920, 1080), "1080p fits");
      Check (OM.Fit ((1024, 768), Minimum, Budget) = (1024, 768), "XGA fits");
      Check (OM.Fit ((640, 480), Minimum, Budget) = (800, 600), "raised to minimum");
      Check (OM.Fit ((1600, 400), Minimum, Budget) = (1600, 600), "one axis raised");
      F := OM.Fit ((2560, 1440), Minimum, Budget);
      Check (OM.Bytes (F) <= Budget and then F.Width < 2560 and then F.Height < 1440,
             "1440p scaled down");
      --  Aspect within rounding.
      Check (abs (Integer (F.Width) * 1440 - Integer (F.Height) * 2560) <= 2560 * 2,
             "aspect kept" & F.Width'Image & F.Height'Image);
      F := OM.Fit ((65_535, 65_535), Minimum, Budget);
      Check (OM.Bytes (F) <= Budget and then F.Width >= 800 and then F.Height >= 600,
             "huge request bounded");
      F := OM.Fit ((65_535, 700), Minimum, Budget);
      Check (OM.Bytes (F) <= Budget and then F.Height >= 600, "wide request bounded");
      --  Exhaustive-ish sweep of the contract.
      for W in 1 .. 300 loop
         for H in 1 .. 50 loop
            F := OM.Fit ((W * 13, H * 61), Minimum, Budget);
            Check (OM.Bytes (F) <= Budget and then F.Width >= 800 and then F.Height >= 600,
                   "sweep");
         end loop;
      end loop;
   end Fitting;

   procedure Layouts is
      Minimum : constant R.Minimum_Extent := (800, 480);
      function One (W, H : G.Physical_Extent; N, D : G.Scale_Component := 1) return L.Layout is
        ((Count => 1,
          Items => [1 => (Display => 1,
                          Geometry => (Width => W, Height => H, Scale => (N, D), others => <>)),
                    others => <>]));
      Saved, Fresh, Result : L.Layout;
   begin
      --  A 2x scaled 1920x1080 output resized to 2560x1600: scale kept.
      Saved := One (1920, 1080, 2, 1);
      Fresh := One (2560, 1600);
      Result := R.Carry_Settings (Saved, Fresh, Minimum);
      Check (Result.Items (1).Geometry.Scale = (2, 1) and then
             Result.Items (1).Geometry.Width = 2560 and then
             L.Validate (Result).Status = L.Accepted, "scale carried");
      --  Shrunk below the minimum logical size at 2x: unscaled instead.
      Fresh := One (1280, 800);
      Result := R.Carry_Settings (Saved, Fresh, Minimum);
      Check (Result = Fresh, "too small for the scale: unscaled");
      --  Unscaled stays unscaled.
      Saved := One (1024, 768);
      Fresh := One (1280, 800);
      Check (R.Carry_Settings (Saved, Fresh, Minimum) = Fresh, "unscaled stays");
      --  Two outputs: packed left to right at the new logical widths.
      Saved := (Count => 2, Items => [1 => (1, (Width => 1024, Height => 768, others => <>)),
                                      2 => (2, (Width => 1600, Height => 1200, Scale => (2, 1),
                                                X => 1024, others => <>)),
                                      others => <>]);
      Fresh := (Count => 2, Items => [1 => (1, (Width => 1280, Height => 800, others => <>)),
                                      2 => (2, (Width => 1920, Height => 1200, X => 1280, others => <>)),
                                      others => <>]);
      Result := R.Carry_Settings (Saved, Fresh, Minimum);
      Check (Result.Items (2).Geometry.Scale = (2, 1) and then
             Result.Items (2).Geometry.X = 1280 and then
             Result.Items (1).Geometry.X = 0 and then
             L.Validate (Result).Status = L.Accepted, "two outputs packed");
      --  A different display at that position: nothing carried.
      Fresh.Items (2).Display := 3;
      Result := R.Carry_Settings (Saved, Fresh, Minimum);
      Check (Result.Items (2).Geometry.Scale = (1, 1), "different display not carried");
   end Layouts;
   --  List_Mode (DISP-005): round trips, every verdict, hostile bits.
   procedure Listings is
      use type OM.Listing_Decoding, OM.Index_Decoding, OM.Admission;
      Item : OM.Listing;
      W, Bad : DP.Wire_Message;
   begin
      for Service in OD.Provider loop
         Check (OM.Decode_List_Request (Service, OM.Encode_List_Request (Service, 7)) = (True, 7),
                "list request round trip");
         Bad := OM.Encode_List_Request (Service, 7);
         Bad.Words (0) := 0;
         Check (not OM.Decode_List_Request (Service, Bad).Valid, "list index zero refused");
         for V in OM.Admission loop
            Item := (Index => 3, Count => 9,
                     Item => (Size => (3840, 2160), Refresh_Millihertz => 60_000, Pixel_kHz => 594_000,
                              Verdict => V, Preferred => V = OM.Admitted, Current => True));
            W := OM.Encode_Listing (Service, Item);
            Check (OM.Decode_Listing (Service, W) = (True, Item), "listing round trip");
            for Word in W.Words'Range loop
               for Bit in 0 .. 63 loop
                  Bad := W;
                  Bad.Words (Word) := Bad.Words (Word) xor Shift_Left (1, Bit);
                  declare
                     D : constant OM.Listing_Decoding := OM.Decode_Listing (Service, Bad);
                  begin
                     Check (not D.Valid or else OM.Encode_Listing (Service, D.Value) = Bad,
                            "listing canonical");
                  end;
               end loop;
            end loop;
         end loop;
         Item.Index := 9;
         Item.Count := 8;
         Bad := W;
         Bad.Words (0) := (Bad.Words (0) and not 16#FFFF_FFFF#) or (9 + 8 * 2 ** 16);
         Check (not OM.Decode_Listing (Service, Bad).Valid, "index above count refused");
      end loop;
   end Listings;
begin
   Codecs;
   Fitting;
   Listings;
   Layouts;
   Put_Line ("output modes:" & Checks'Image & " checks," & Failures'Image & " failures");
   Put_Line (if Failures = 0 then "OUTPUT-MODES: PASS" else "OUTPUT-MODES: FAIL");
end Mode_Tests;
