pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Display_Protocol;
with CuBit.Display_Planes;
with CuBit.Display_Plane_Protocol;
with CuBit.GPU_Plane_Protocol;
with CuBit.Display_Pool_Protocol;

--  Hosted regression tests for the display plane planner and its codecs.
--  Proof covers the planner's safety and priority contracts; these tests
--  pin concrete plans for 2 .. 4 cursors over 1 .. 2 outputs, clipping at
--  every edge, codec round trips and the rejection of every malformed field.
procedure Plane_Tests is
   package DPL renames CuBit.Display_Planes;
   package PP renames CuBit.Display_Plane_Protocol;
   package GP renames CuBit.GPU_Plane_Protocol;
   package Pool renames CuBit.Display_Pool_Protocol;
   package D renames CuBit.Display_Protocol;
   use type DPL.Backing_Kind, DPL.Plane_Count, DPL.Request_Count,
            DPL.Plan, DPL.Local_Coordinate, DPL.Space_Coordinate, DPL.Plane_Kind;
   use type PP.Cursor_Image;
   use all type PP.Request_Decoding, PP.Anchor_Decoding, PP.Visibility_Decoding,
     PP.Origin_Decoding, PP.Capability_Decoding, PP.Operation_Decoding,
     PP.Report_Decoding, PP.Plan_Frame_Decoding, PP.Image_Decoding,
     PP.Move_Decoding, PP.Identity_Decoding;
   use all type GP.Description_Decoding, GP.Plane_Decoding, GP.Show_Decoding,
     GP.Move_Decoding, GP.Buffer_Decoding;

   Checks : Natural := 0;
   procedure Check (Condition : Boolean; Name : String) is
   begin
      if not Condition then
         Ada.Text_IO.Put_Line ("FAIL " & Name);
         raise Program_Error with Name;
      end if;
      Checks := Checks + 1;
   end Check;

   Cursor_Plane : constant DPL.Plane_Descriptor :=
     (Kinds => [DPL.Cursor => True, others => False],
      Formats => [DPL.ARGB8888 => True, others => False],
      Max_Width => 64, Max_Height => 64, Scaler => False, Z => 7,
      Host_Pointer => False);
   --  An Intel-style universal plane offered to cursors above the primary.
   Sprite_Plane : constant DPL.Plane_Descriptor :=
     (Kinds => [DPL.Cursor | DPL.Overlay => True, others => False],
      Formats => [DPL.ARGB8888 | DPL.XRGB8888 | DPL.NV12 | DPL.P010 => True],
      Max_Width => 4096, Max_Height => 4096, Scaler => True, Z => 5,
      Host_Pointer => False);

   function Output
     (X : DPL.Space_Coordinate; Planes : DPL.Plane_Count;
      Width : DPL.Output_Extent := 1024; Height : DPL.Output_Extent := 768)
      return DPL.Output_State
   is
      Result : DPL.Output_State :=
        (Present => True, X => X, Y => 0, Width => Width, Height => Height,
         Count => Planes, Planes => [others => <>]);
   begin
      for P in 1 .. Planes loop
         Result.Planes (P) := (if P = 1 then Cursor_Plane else Sprite_Plane);
      end loop;
      return Result;
   end Output;

   function Cursor
     (X, Y : DPL.Space_Coordinate; Priority : DPL.Request_Priority;
      Width : DPL.Cursor_Extent := 19; Height : DPL.Cursor_Extent := 28;
      Hot : DPL.Hotspot_Coordinate := 2; Visible : Boolean := True;
      Absolute : Boolean := False)
      return DPL.Request_State is
     (DPL.Cursor_Request (True, Visible, Priority, X, Y, Width, Height,
                          Hot, Hot, Absolute));

   No_Outputs : constant DPL.Output_Table := [others => (others => <>)];
   No_Requests : constant DPL.Request_Table := [others => (others => <>)];

   procedure Planner_Cases is
      Outputs : DPL.Output_Table := No_Outputs;
      Requests : DPL.Request_Table := No_Requests;
      A, B : DPL.Plan;
   begin
      --  Two cursors, one output, one cursor plane (virtio-gpu): the
      --  primary pointer gets the plane; the agent pointer is composited.
      Outputs (0) := Output (0, 1);
      Requests (1) := Cursor (100, 100, DPL.Agent_Pointer);
      Requests (2) := Cursor (500, 300, DPL.Primary_Pointer);
      A := DPL.Plan_Planes (Requests, Outputs);
      Check (A.Backing (2) = DPL.Hardware and then A.Planes (2, 0) = 1,
             "primary pointer takes the only plane");
      Check (A.Backing (1) = DPL.Composited and then A.Planes (1, 0) = 0,
             "agent pointer composited");
      Check (A.Holders (0, 1) = 2, "holder view matches");
      Check (A = DPL.Plan_Planes (Requests, Outputs), "deterministic");
      --  Equal priority: the lower identity wins.
      Requests (1).Priority := DPL.Primary_Pointer;
      A := DPL.Plan_Planes (Requests, Outputs);
      Check (A.Backing (1) = DPL.Hardware and then A.Backing (2) = DPL.Composited,
             "tie goes to lower identity");
      --  Hiding the winner promotes the other.
      Requests (1).Visible := False;
      A := DPL.Plan_Planes (Requests, Outputs);
      Check (A.Backing (1) = DPL.Hidden and then A.Backing (2) = DPL.Hardware,
             "hidden cursor frees its plane");
      Requests (1).Visible := True;

      --  Four cursors, one Intel-like output: cursor plane + three sprites.
      Outputs (0) := Output (0, 4);
      Requests (3) := Cursor (10, 10, DPL.Remote_Pointer);
      Requests (4) := Cursor (20, 20, DPL.Secondary_Pointer);
      A := DPL.Plan_Planes (Requests, Outputs);
      for R in DPL.Request_Id range 1 .. 4 loop
         Check (A.Backing (R) = DPL.Hardware, "four cursors on four planes");
      end loop;
      Check (A.Planes (1, 0) = 1 and then A.Planes (2, 0) = 2 and then
             A.Planes (4, 0) = 3 and then A.Planes (3, 0) = 4,
             "planes offered in precedence order");
      --  Only two planes: the two highest priorities win.
      Outputs (0) := Output (0, 2);
      A := DPL.Plan_Planes (Requests, Outputs);
      Check (A.Backing (1) = DPL.Hardware and then A.Backing (2) = DPL.Hardware
             and then A.Backing (3) = DPL.Composited and then
             A.Backing (4) = DPL.Composited, "lowest priorities composited");

      --  Too large for the cursor plane but fits a sprite.
      Requests := No_Requests;
      Requests (1) := Cursor (100, 100, DPL.Primary_Pointer, 128, 128, 4);
      Requests (2) := Cursor (300, 100, DPL.Secondary_Pointer);
      A := DPL.Plan_Planes (Requests, Outputs);
      Check (A.Planes (1, 0) = 2 and then A.Planes (2, 0) = 1,
             "oversized cursor skips the cursor plane");
      Outputs (0) := Output (0, 1);
      A := DPL.Plan_Planes (Requests, Outputs);
      Check (A.Backing (1) = DPL.Composited and then A.Planes (2, 0) = 1,
             "oversized cursor composited, plane goes to the next");

      --  Two outputs side by side, one plane each.
      Outputs (0) := Output (0, 1);
      Outputs (1) := Output (1024, 1);
      Requests := No_Requests;
      Requests (1) := Cursor (100, 100, DPL.Primary_Pointer);
      Requests (2) := Cursor (1500, 100, DPL.Secondary_Pointer);
      A := DPL.Plan_Planes (Requests, Outputs);
      Check (A.Planes (1, 0) = 1 and then A.Planes (1, 1) = 0 and then
             A.Planes (2, 1) = 1 and then A.Planes (2, 0) = 0,
             "one cursor per output");
      --  The primary straddles the edge: it needs both planes, which
      --  pushes the secondary to software on output 1.
      Requests (1) := Cursor (1020, 100, DPL.Primary_Pointer);
      A := DPL.Plan_Planes (Requests, Outputs);
      Check (A.Backing (1) = DPL.Hardware and then A.Planes (1, 0) = 1 and then
             A.Planes (1, 1) = 1, "straddling cursor holds both outputs");
      Check (A.Backing (2) = DPL.Composited, "displaced by a straddler");
      --  A low-priority straddler cannot take a partial plane set.
      Requests (1) := Cursor (100, 100, DPL.Primary_Pointer);
      Requests (2) := Cursor (1020, 100, DPL.Secondary_Pointer);
      A := DPL.Plan_Planes (Requests, Outputs);
      Check (A.Backing (2) = DPL.Composited and then A.Planes (2, 1) = 0 and then
             A.Holders (1, 1) = 0, "no partial hardware cursor");
      --  Three cursors over two outputs.
      Requests (3) := Cursor (1800, 600, DPL.Agent_Pointer);
      A := DPL.Plan_Planes (Requests, Outputs);
      Check (A.Backing (3) = DPL.Hardware and then A.Planes (3, 1) = 1,
             "agent gets the free plane on output 1");
      --  Offscreen and absent outputs.
      Requests (3) := Cursor (5000, 5000, DPL.Agent_Pointer);
      A := DPL.Plan_Planes (Requests, Outputs);
      Check (A.Backing (3) = DPL.Composited, "offscreen visible cursor composited");
      Outputs (1).Present := False;
      A := DPL.Plan_Planes (Requests, Outputs);
      Check (A.Holders (1, 1) = 0, "absent output carries nothing");

      --  Host-pointer plane (virtio-gpu): only absolute cursors may use it,
      --  whatever their priority; a relative mouse stays composited.
      Outputs := No_Outputs;
      Outputs (0) := Output (0, 1);
      Outputs (0).Planes (1).Host_Pointer := True;
      Requests := No_Requests;
      Requests (1) := Cursor (100, 100, DPL.Primary_Pointer);
      A := DPL.Plan_Planes (Requests, Outputs);
      Check (A.Backing (1) = DPL.Composited and then A.Holders (0, 1) = 0,
             "relative pointer never on a host-pointer plane");
      Check (DPL.Prioritized (Requests, Outputs, A), "vacuously prioritized");
      Requests (2) := Cursor (300, 100, DPL.Agent_Pointer, Absolute => True);
      A := DPL.Plan_Planes (Requests, Outputs);
      Check (A.Backing (1) = DPL.Composited and then A.Backing (2) = DPL.Hardware,
             "absolute low-priority pointer takes the host-pointer plane");
      --  A scanout plane (Intel cursor plane) takes relative pointers.
      Outputs (0).Planes (1).Host_Pointer := False;
      A := DPL.Plan_Planes (Requests, Outputs);
      Check (A.Backing (1) = DPL.Hardware and then A.Backing (2) = DPL.Composited,
             "scanout plane goes by priority");

      --  Frame gating: hardware <-> composited swaps need a frame on the
      --  affected output; hide/show and renumbering do not.
      Outputs := No_Outputs;
      Outputs (0) := Output (0, 1);
      Outputs (1) := Output (1024, 1);
      Requests := No_Requests;
      Requests (1) := Cursor (100, 100, DPL.Secondary_Pointer);
      A := DPL.Plan_Planes (Requests, Outputs);
      Requests (2) := Cursor (200, 100, DPL.Primary_Pointer);
      B := DPL.Plan_Planes (Requests, Outputs);
      Check (DPL.Needs_Frame (A, B), "demotion needs a frame");
      Check (DPL.Frame_Affects (A, B, Requests, Outputs, 0) and then
             not DPL.Frame_Affects (A, B, Requests, Outputs, 1),
             "only the demotion's output waits");
      Requests (2).Visible := False;
      B := DPL.Plan_Planes (Requests, Outputs);
      Check (not DPL.Needs_Frame (A, B), "hidden newcomer changes nothing");
      Requests (1).Visible := False;
      B := DPL.Plan_Planes (Requests, Outputs);
      Check (not DPL.Needs_Frame (A, B), "hiding a hardware cursor is frame-free");
   end Planner_Cases;

   --  Exhaustive small cross-check of the proved properties over many
   --  layouts: 3 cursors, positions on a grid across two outputs, priorities
   --  and slot counts varied. Also checks that the plan is maximal: no
   --  composited onscreen cursor could have been given planes.
   procedure Planner_Sweep is
      Outputs : DPL.Output_Table := No_Outputs;
      Requests : DPL.Request_Table := No_Requests;
      A : DPL.Plan;
      Positions : constant array (1 .. 5) of DPL.Space_Coordinate :=
        [-10, 500, 1020, 1500, 2050];
      Plans : Natural := 0;
   begin
      for Slots0 in DPL.Plane_Count range 0 .. 2 loop
         for Slots1 in DPL.Plane_Count range 0 .. 2 loop
            Outputs (0) := Output (0, Slots0);
            Outputs (1) := Output (1024, Slots1);
            for P1 in Positions'Range loop
               for P2 in Positions'Range loop
                  for P3 in Positions'Range loop
                     for Prio in 0 .. 3 loop
                        Requests := No_Requests;
                        Requests (1) := Cursor (Positions (P1), 50,
                          DPL.Request_Priority (5 * (Prio mod 2)));
                        Requests (2) := Cursor (Positions (P2), 60,
                          DPL.Request_Priority (Prio / 2 * 7));
                        Requests (3) := Cursor (Positions (P3), 70, 3,
                          Visible => Prio /= 3);
                        A := DPL.Plan_Planes (Requests, Outputs);
                        Check (DPL.Valid_Plan (Requests, Outputs, A) and then
                               DPL.Prioritized (Requests, Outputs, A),
                               "sweep contracts");
                        Plans := Plans + 1;
                     end loop;
                  end loop;
               end loop;
            end loop;
         end loop;
      end loop;
      Ada.Text_IO.Put_Line ("sweep plans" & Plans'Image);
   end Planner_Sweep;

   procedure Placement_Cases is
      O : constant DPL.Output_State := Output (1024, 1);
      P : DPL.Placement;
   begin
      --  Pointer at the output's top-left corner, hotspot (2, 2).
      P := DPL.Place (Cursor (1024, 0, 0), O);
      Check (P.Visible and then P.X = -2 and then P.Y = -2, "negative origin");
      --  Image entirely left of the output except one column.
      P := DPL.Place (Cursor (1024 - 16, 10, 0), O);
      Check (P.Visible and then P.X = -18, "one visible column");
      P := DPL.Place (Cursor (1024 - 17, 10, 0), O);
      Check (not P.Visible, "fully left is invisible");
      P := DPL.Place (Cursor (1024 + 1023 + 2, 767 + 2, 0), O);
      Check (P.Visible and then P.X = 1023 and then P.Y = 767,
             "last pixel visible");
      P := DPL.Place (Cursor (1024 + 1024 + 2, 10, 0), O);
      Check (not P.Visible, "beyond the right edge");
      P := DPL.Place (Cursor (1100, 100, 0, Visible => False), O);
      Check (not P.Visible, "hidden is not placed");
   end Placement_Cases;

   procedure Codec_Cases is
      W, Bad : D.Wire_Message;
      Image : constant PP.Cursor_Image :=
        (Request => 3, Grant => (12_345, 7), Width => 19, Height => 28,
         Hot_X => 2, Hot_Y => 27);
      Move : constant PP.Move := (Request => 8, X => -DPL.Space_Limit,
                                  Y => DPL.Space_Limit);
      Report : PP.Report;
      Capability : constant PP.Capability :=
        (Planes => [DPL.Cursor => 1, DPL.Overlay => 3, DPL.Primary => 1],
         Host_Pointer_Cursors => 1,
         Max_Width => 256, Max_Height => 64, Capacity => DPL.Request_Capacity);
   begin
      W := PP.Encode (Image);
      Check (PP.Decode_Image (W) = (True, Image), "image round trip");
      for Word in 0 .. 3 loop
         for Bit in 0 .. 63 loop
            Bad := W;
            Bad.Words (Word) := Bad.Words (Word) xor Shift_Left (1, Bit);
            --  Any single flip either decodes to a different valid image or
            --  is rejected; it never decodes to the original.
            Check (not PP.Decode_Image (Bad).Valid or else
                   PP.Decode_Image (Bad).Value /= Image, "image bit flip");
         end loop;
      end loop;
      Bad := W; Bad.Reserved := 1;
      Check (not PP.Decode_Image (Bad).Valid, "routing must be normalized");
      Bad := W; Bad.Words (3) := 19 + 28 * 2 ** 16 + 19 * 2 ** 32;
      Check (not PP.Decode_Image (Bad).Valid, "hotspot outside image");
      Bad := W; Bad.Words (3) := 257 + 28 * 2 ** 16;
      Check (not PP.Decode_Image (Bad).Valid, "image wider than the limit");
      Bad := W; Bad.Words (0) := 0;
      Check (not PP.Decode_Image (Bad).Valid, "request zero");
      Bad := W; Bad.Words (0) := DPL.Request_Capacity + 1;
      Check (not PP.Decode_Image (Bad).Valid, "request beyond capacity");

      W := PP.Encode (Move);
      Check (PP.Decode_Move (W) = (True, Move), "extreme move round trip");
      Bad := W; Bad.Words (1) := 0 - DPL.Space_Limit - 1;
      Check (not PP.Decode_Move (Bad).Valid, "coordinate below range");
      Bad := W; Bad.Words (2) := DPL.Space_Limit + 1;
      Check (not PP.Decode_Move (Bad).Valid, "coordinate above range");
      for X in DPL.Space_Coordinate range -3 .. 3 loop
         Check (PP.Decode_Move (PP.Encode (PP.Move'(1, X, -X))) =
                  (True, (1, X, -X)), "small signed moves");
      end loop;

      for Kind in DPL.Plane_Kind loop
         W := PP.Encode (PP.Create_Request, PP.Identity'(5, Kind, 9, Kind = DPL.Cursor));
         Check (PP.Decode_Identity (PP.Create_Request, W) =
                  (True, (5, Kind, 9, Kind = DPL.Cursor)), "identity round trip");
         Check (not PP.Decode_Identity (PP.Set_Priority, W).Valid,
                "label is part of the message");
      end loop;
      W := PP.Encode (PP.Create_Request, PP.Identity'(5, DPL.Cursor, 9, False));
      Bad := W; Bad.Words (3) := 2;
      Check (not PP.Decode_Identity (PP.Create_Request, Bad).Valid, "bad absolute flag");
      Bad := W; Bad.Words (1) := 3;
      Check (not PP.Decode_Identity (PP.Create_Request, Bad).Valid, "bad kind");
      Bad := W; Bad.Words (2) := 16;
      Check (not PP.Decode_Identity (PP.Create_Request, Bad).Valid, "bad priority");
      Check (PP.Decode_Destroy (PP.Encode_Destroy (8)) = (True, 8), "destroy");
      Check (PP.Decode_Anchor (PP.Encode (PP.Anchor'(2, 255, 0))) =
               (True, (2, 255, 0)), "anchor");
      Check (PP.Decode_Visibility (PP.Encode (PP.Visibility'(4, True))) =
               (True, (4, True)), "visibility");
      Check (PP.Decode_Origin (PP.Encode (PP.Origin'(-1024, 2048))) =
               (True, (-1024, 2048)), "origin");
      W := PP.Encode (Capability);
      Check (PP.Decode_Capability (W) = (True, Capability), "capability");
      Bad := W; Bad.Words (1) := Bad.Words (1) + 2 ** 32;
      Check (not PP.Decode_Capability (Bad).Valid, "capability spare bits");
      Check (PP.Decode_Operation (16#0925#) = (True, PP.Move_Request),
             "operation lookup");
      Check (not PP.Decode_Operation (16#092A#).Valid,
             "plan frames are not plane requests");

      Report := (Status => D.DP.Success, Epoch => 77,
                 Proposed_Hardware => [1 => True, 8 => True, others => False],
                 Proposed_Composited => [2 => True, others => False],
                 Committed_Hardware => [8 => True, others => False],
                 Committed_Composited => [1 | 2 => True, others => False]);
      W := PP.Encode (PP.Move_Request, Report);
      Check (PP.Decode_Report (PP.Move_Request, W) = (True, Report),
             "report round trip");
      Bad := W; Bad.Words (0) := 7;
      Check (not PP.Decode_Report (PP.Move_Request, Bad).Valid, "bad status");
      Bad := W; Bad.Words (2) := 2 ** 32;
      Check (not PP.Decode_Report (PP.Move_Request, Bad).Valid, "spare set bits");

      declare
         F : constant PP.Plan_Frame :=
           ((Buffer => 2, Request => (11, 12, (1, 2, 3, 4))), 5);
      begin
         W := PP.Encode (F);
         Check (W.Label = PP.Submit_Plan_Frame and then W.Words (3) = 5,
                "plan frame layout");
         Check (PP.Decode_Plan_Frame (W) = (True, F), "plan frame round trip");
         Bad := W; Bad.Words (3) := 0;
         Check (not PP.Decode_Plan_Frame (Bad).Valid, "epoch zero rejected");
         Bad := W; Bad.Label := Pool.Submit_Frame;
         Check (not PP.Decode_Plan_Frame (Bad).Valid, "plain pool frame");
         Check (not Pool.Decode_Frame (W).Valid,
                "plain pool decoder rejects tagged frames");
      end;
   end Codec_Cases;

   procedure GPU_Codec_Cases is
      W, Bad : D.Wire_Message;
      Desc : constant GP.Description := (Count => 6, Descriptor => Sprite_Plane);
      Host : constant GP.Description :=
        (Count => 1, Descriptor => (Cursor_Plane with delta Host_Pointer => True));
      Show : constant GP.Show :=
        (Plane => 1, Hot_X => 2, Hot_Y => 3, At_Position => (-255, 65_534));
      Buffer : constant GP.Buffer := ((99, 3), 64, 64);
   begin
      W := GP.Encode (Desc);
      Check (GP.Decode_Description (W) = (True, Desc), "description");
      Check (GP.Decode_Description (GP.Encode (Host)) = (True, Host),
             "host-pointer description");
      Bad := GP.Encode (Host); Bad.Words (1) := Bad.Words (1) + 2 ** 41;
      Check (not GP.Decode_Description (Bad).Valid, "host-pointer spare bits");
      Bad := W; Bad.Words (1) := Bad.Words (1) or 2 ** 11;
      Check (not GP.Decode_Description (Bad).Valid, "unknown kind bit");
      Bad := W; Bad.Words (1) := Bad.Words (1) or 2 ** 20;
      Check (not GP.Decode_Description (Bad).Valid, "unknown format bit");
      Bad := W; Bad.Words (2) := 16_385;
      Check (not GP.Decode_Description (Bad).Valid, "plane width limit");
      Check (GP.Decode_Plane (GP.Query, GP.Encode (GP.Query, 3)) = (True, 3),
             "query plane");
      Check (not GP.Decode_Plane (GP.Hide_Plane, GP.Encode (GP.Query, 3)).Valid,
             "query is not hide");
      W := GP.Encode (Show);
      Check (GP.Decode_Show (W) = (True, Show), "show round trip");
      Bad := W; Bad.Words (2) := 70_000;
      Check (not GP.Decode_Show (Bad).Valid, "x beyond output range");
      W := GP.Encode (GP.Move'(2, (-1, -1)));
      Check (GP.Decode_Move (W) = (True, (2, (-1, -1))), "negative move");
      Check (W.Words (1) = 16#FFFF_FFFF_FFFF_FFFF#, "two's complement halves");
      W := GP.Encode (Buffer);
      Check (GP.Decode_Buffer (W) = (True, Buffer) and then W.Words (3) = 256,
             "buffer pitch");
      Bad := W; Bad.Words (3) := 512;
      Check (not GP.Decode_Buffer (Bad).Valid, "padded pitch rejected");
      Check (GP.Accepted_Reply (GP.Show_Plane,
               GP.Encode_Status (GP.Show_Plane, GP.Accepted)), "status");
   end GPU_Codec_Cases;
begin
   Planner_Cases;
   Planner_Sweep;
   Placement_Cases;
   Codec_Cases;
   GPU_Codec_Cases;
   Ada.Text_IO.Put_Line ("PASS display planes:" & Checks'Image & " checks");
end Plane_Tests;
