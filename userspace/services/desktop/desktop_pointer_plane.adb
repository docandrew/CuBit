pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants;
with CuBit.Desktop_Messages;
with CuBit.Display_Protocol;
with CuBit.Display_Plane_Protocol;
with Compositor_Requests;

package body Desktop_Pointer_Plane is
   package PP renames CuBit.Display_Plane_Protocol;
   package DSP renames CuBit.Display_Protocol;
   package MG renames CuBit.Memory_Grants;
   use type DPL.Plane_Count;
   use type DSP.DP.Status_Code;

   Pointer : constant DPL.Request_Id := 1;
   --  Same bound as display: a synchronous plane call never hangs desktop.
   Call_Deadline_Ms : constant := 1_000;
   Page_Bytes : constant := 4_096;
   Staging_Pixels : constant :=
     DPL.Cursor_Extent_Limit * DPL.Cursor_Extent_Limit;
   Staging_Pages : constant := Staging_Pixels * PP.Bytes_Per_Pixel / Page_Bytes;
   type Pixel_Array is array (0 .. Staging_Pixels - 1) of Unsigned_32;
   Staging : Pixel_Array := [others => 0] with Alignment => Page_Bytes;

   Started, Enabled, Shaped : Boolean := False;
   Max_Width, Max_Height : Natural := 0;
   Grant : MG.Grant_Reference;
   Latest : PP.Report;
   Composed_Hardware_State : Boolean := False;
   Composed_Epoch : DPL.Plan_Epoch := DPL.No_Epoch;
   Changed : Boolean := False;
   Move_Token : Unsigned_64 := 0;
   Want_X, Want_Y, Sent_X, Sent_Y : Integer := 0;
   Exact_Outputs : Boolean := True;

   function Call (Request : DSP.Wire_Message) return DSP.Wire_Message is
      Msg : Message := CuBit.Desktop_Messages.From_Wire (Request);
   begin
      Msg.tag := capCall (CapabilitySlot (CAP_SLOT_DISPLAY), Msg,
                          Deadline_After (Call_Deadline_Ms));
      return CuBit.Desktop_Messages.To_Wire (Msg);
   end Call;

   procedure Observe (Kind : PP.Operation; Wire : DSP.Wire_Message) is
      Decoded : constant PP.Report_Decoding := PP.Decode_Report (Kind, Wire);
      Was : constant Boolean := Hardware;
   begin
      if not Decoded.Valid then
         --  Keep the last known plan; display state did not change for us.
         return;
      end if;
      Latest := Decoded.Value;
      if Hardware /= Was then
         Changed := True;
      end if;
      --  A newer plan with the same pointer composition is the same frame.
      if Hardware = Composed_Hardware_State then
         Composed_Epoch := Latest.Epoch;
      end if;
   end Observe;

   function Hardware return Boolean is
     (Enabled and then Shaped and then Exact_Outputs and then
      Latest.Proposed_Hardware (Pointer));

   function Active return Boolean is (Enabled);

   procedure Note_Composed is
   begin
      Composed_Hardware_State := Hardware;
      Composed_Epoch := Latest.Epoch;
   end Note_Composed;

   function Composed_Hardware return Boolean is (Composed_Hardware_State);

   function Frame_Epoch return DPL.Plan_Epoch is
     (if Enabled then Composed_Epoch else DPL.No_Epoch);

   function Take_Change return Boolean is
      Result : constant Boolean := Changed;
   begin
      Changed := False;
      return Result;
   end Take_Change;

   function Token return Unsigned_64 is (Move_Token);

   procedure Start is
      Capability : PP.Capability_Decoding;
      Ok : Boolean;
   begin
      if Started then return; end if;
      Started := True;
      Capability := PP.Decode_Capability (Call (PP.Encode_Empty (PP.Query_Output)));
      --  PS/2 and USB mice report relative motion, so host-pointer planes
      --  (a virtual GPU's cursor, drawn by the host only for absolute
      --  pointers) cannot show this pointer. Only scanout planes count.
      if not Capability.Valid or else Capability.Value.Planes (DPL.Cursor) = 0 then
         if Capability.Valid and then Capability.Value.Host_Pointer_Cursors > 0 then
            debugPrint ("desktop: pointer composited (host-pointer cursor planes need absolute input)" & ASCII.LF);
         end if;
         return;
      end if;
      Max_Width := Natural (Capability.Value.Max_Width);
      Max_Height := Natural (Capability.Value.Max_Height);
      MG.Create_Via_Capability (CAP_SLOT_DISPLAY, Staging'Address,
                                Staging_Pages, False, Grant, Ok);
      if not Ok then return; end if;
      Observe (PP.Create_Request, Call (PP.Encode
        (PP.Create_Request, PP.Identity'
           (Pointer, DPL.Cursor, DPL.Primary_Pointer, Absolute => False))));
      Enabled := Latest.Status = DSP.DP.Success;
   end Start;

   procedure Place (Output : Natural; X, Y : Integer; Exact : Boolean) is
      Was : constant Boolean := Hardware;
   begin
      if not Enabled then return; end if;
      if Output = 0 then Exact_Outputs := Exact;
      else Exact_Outputs := Exact_Outputs and then Exact;
      end if;
      if Output <= Natural (DSP.Output_Number'Last) and then
        abs X <= DPL.Space_Limit and then abs Y <= DPL.Space_Limit
      then
         Observe (PP.Place_Output, Call (DSP.With_Output (PP.Encode
           (PP.Origin'(DPL.Space_Coordinate (X), DPL.Space_Coordinate (Y))),
            DSP.Output_Number (Output))));
      end if;
      Changed := Changed or else Hardware /= Was;
   end Place;

   --  Shape requests: the newest wanted image (copied at Set_Shape) and
   --  visibility, sent one at a time. Staging is rewritten only while no
   --  image request is in flight, since display reads it until the reply.
   type Shape_Step is (No_Step, Image_Step, Visibility_Step);
   Wanted : Pixel_Array := [others => 0];
   Wanted_Width, Wanted_Height, Wanted_Hot_X, Wanted_Hot_Y : Natural := 0;
   Image_Wanted : Boolean := False;
   Visible_Wanted, Visible_Sent : Boolean := False;
   Shape_Flight : Unsigned_64 := 0;
   Flight_Step : Shape_Step := No_Step;

   function Shape_Token return Unsigned_64 is (Shape_Flight);

   procedure Submit_Shape (Request : DSP.Wire_Message; Step : Shape_Step;
                           Sequence : in out Unsigned_64; Sent : out Boolean) is
      Token : Unsigned_64;
   begin
      Compositor_Requests.Allocate (Sequence, Token);
      Sent := Token /= 0 and then
        capSubmit (CAP_SLOT_DISPLAY, CuBit.Desktop_Messages.From_Wire (Request), Token);
      if Sent then
         Shape_Flight := Token;
         Flight_Step := Step;
      end if;
   end Submit_Shape;

   procedure Pump_Shape (Sequence : in out Unsigned_64) is
      Sent : Boolean;
   begin
      if Shape_Flight /= 0 or else not Enabled then return; end if;
      if Image_Wanted then
         for I in 0 .. Wanted_Width * Wanted_Height - 1 loop
            Staging (I) := Wanted (I);
         end loop;
         Submit_Shape (PP.Encode (PP.Cursor_Image'
           (Pointer, Grant, DPL.Cursor_Extent (Wanted_Width), DPL.Cursor_Extent (Wanted_Height),
            DPL.Hotspot_Coordinate (Wanted_Hot_X), DPL.Hotspot_Coordinate (Wanted_Hot_Y))),
           Image_Step, Sequence, Sent);
         if Sent then Image_Wanted := False; end if;
      elsif Visible_Wanted /= Visible_Sent then
         Submit_Shape (PP.Encode (PP.Visibility'(Pointer, Visible_Wanted)),
                       Visibility_Step, Sequence, Sent);
         if Sent then Visible_Sent := Visible_Wanted; end if;
      end if;
   end Pump_Shape;

   procedure Set_Shape
     (Pixels : System.Address; Width, Height, Hot_X, Hot_Y : Natural;
      Sequence : in out Unsigned_64)
   is
      Was : constant Boolean := Hardware;
   begin
      if not Enabled then return; end if;
      if Width not in 1 .. DPL.Cursor_Extent_Limit or else
        Height not in 1 .. DPL.Cursor_Extent_Limit or else
        Hot_X >= Width or else Hot_Y >= Height or else
        Width > Max_Width or else Height > Max_Height
      then
         --  Too large for any plane here: keep it composited.
         Image_Wanted := False;
         Visible_Wanted := False;
         Shaped := False;
         Changed := Changed or else Was;
         Pump_Shape (Sequence);
         return;
      end if;
      declare
         Source : array (0 .. Width * Height - 1) of Unsigned_32
           with Import, Address => Pixels;
      begin
         for I in Source'Range loop
            Wanted (I) := Source (I);
         end loop;
      end;
      Wanted_Width := Width;
      Wanted_Height := Height;
      Wanted_Hot_X := Hot_X;
      Wanted_Hot_Y := Hot_Y;
      Image_Wanted := True;
      Pump_Shape (Sequence);
   end Set_Shape;

   procedure Send (Sequence : in out Unsigned_64) is
      Token : Unsigned_64;
   begin
      if Move_Token /= 0 or else (Want_X = Sent_X and then Want_Y = Sent_Y) then
         return;
      end if;
      Compositor_Requests.Allocate (Sequence, Token);
      if Token /= 0 and then capSubmit (CAP_SLOT_DISPLAY,
           CuBit.Desktop_Messages.From_Wire (PP.Encode (PP.Move'
             (Pointer, DPL.Space_Coordinate (Want_X), DPL.Space_Coordinate (Want_Y)))),
           Token)
      then
         Move_Token := Token;
         Sent_X := Want_X;
         Sent_Y := Want_Y;
      end if;
   end Send;

   procedure Move (X, Y : Integer; Sequence : in out Unsigned_64) is
   begin
      if not Enabled or else abs X > DPL.Space_Limit or else
        abs Y > DPL.Space_Limit
      then
         return;
      end if;
      Want_X := X;
      Want_Y := Y;
      Send (Sequence);
   end Move;

   procedure Collect (Completion : CompletionEntry;
                      Sequence : in out Unsigned_64) is
      Was : constant Boolean := Hardware;
   begin
      if Completion.token = Shape_Flight then
         Shape_Flight := 0;
         if Completion.valid and then Completion.status = COMPLETION_OK then
            Observe ((if Flight_Step = Image_Step then PP.Set_Cursor_Image else PP.Set_Visibility),
                     CuBit.Desktop_Messages.To_Wire (Completion.msg));
            if Flight_Step = Image_Step and then Latest.Status = DSP.DP.Success then
               Shaped := True;
               Visible_Wanted := True;
            end if;
         end if;
         Flight_Step := No_Step;
         Changed := Changed or else Hardware /= Was;
         Pump_Shape (Sequence);
         return;
      end if;
      Move_Token := 0;
      if Completion.valid and then Completion.status = COMPLETION_OK then
         Observe (PP.Move_Request, CuBit.Desktop_Messages.To_Wire (Completion.msg));
      else
         Observe (PP.Move_Request, (others => <>));
      end if;
      Send (Sequence);
   end Collect;
end Desktop_Pointer_Plane;
