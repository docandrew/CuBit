with System.Storage_Elements; use System.Storage_Elements;
package body Intel_GPU_Primary_Flip is
   package VC renames Intel_GPU_Vblank_Clock;
   use type FS.Source;

   Configured, Is_Ready : Boolean := False;
   S : FS.State;
   Surface : GGTT_Bases := [others => 0];
   Memory : CPU_Addresses := [others => 0];
   Firmware : Unsigned_32 := 0;
   Problem_Buffer : Index := Index'First;
   Frame_Width, Frame_Height, Frame_Pitch : Positive := 1;

   --  Vblank clock: when the counter was last seen to change, and the period.
   Last_Count : Unsigned_32 := 0;
   Last_Change_Us : Unsigned_64 := 0;
   Clock_Known : Boolean := False;
   Period : VC.Period := VC.Default_Period_Us;
   --  The owed flip: counter and time at its arm.
   Armed_Count : Unsigned_32 := 0;
   Armed_Us : Unsigned_64 := 0;
   --  Counter at the last Take, for vblanks per interval.
   Report_Count : Unsigned_32 := 0;
   Report_Known : Boolean := False;

   Stats : Counters;
   procedure Count (Item : in out Natural) is
   begin
      if Item < Natural'Last then Item := Item + 1; end if;
   end Count;

   function Width return Positive is (Frame_Width);
   function Height return Positive is (Frame_Height);
   function Pitch return Positive is (Frame_Pitch);
   function Frame_Bytes return Unsigned_64 is
     (Unsigned_64 (Frame_Pitch) * Unsigned_64 (Frame_Height));
   function CPU_Of (Item : Index) return Unsigned_64 is (Memory (Item));
   function Ready return Boolean is (Is_Ready and then not S.Faulted);
   function Owed return Boolean is (S.Owed);

   Problem : Configure_Problem := Not_Called;
   function Last_Problem return Configure_Problem is (Problem);

   procedure Configure
     (Bases : GGTT_Bases; CPU : CPU_Addresses;
      Width, Height, Pitch : Positive; Firmware_Surface : Unsigned_32) is
      Page_Offset_Bits : constant Unsigned_32 := not Surface_Page_Mask;
   begin
      --  GGTT offset 0 is a valid surface (GOP framebuffers often sit
      --  there): no address value is used as a "none" sentinel.
      Problem := Ready_To_Flip;
      if (Firmware_Surface and Page_Offset_Bits) /= 0 then
         Problem := Firmware_Surface_Unaligned;
      end if;
      for I in Index loop
         if Problem = Ready_To_Flip then
            if (Bases (I) and Page_Offset_Bits) /= 0 then
               Problem := Buffer_Unaligned;
            elsif Bases (I) = Firmware_Surface then
               Problem := Buffer_Is_Firmware_Surface;
            elsif CPU (I) = 0 then
               Problem := Buffer_Not_Mapped;
            elsif (for some J in Index => J /= I and then Bases (J) = Bases (I)) then
               Problem := Buffers_Not_Distinct;
            end if;
            if Problem /= Ready_To_Flip then
               Problem_Buffer := I;
            end if;
         end if;
      end loop;
      Configured := Problem = Ready_To_Flip;
      Surface := Bases;
      Memory := CPU;
      Frame_Width := Width;
      Frame_Height := Height;
      Frame_Pitch := Pitch;
      Firmware := Firmware_Surface;
      S := (others => <>);
      Is_Ready := False;
   end Configure;

   function Problem_Text return String is
     (Problem_Name (Problem) &
      (if Problem in Buffer_Unaligned | Buffer_Is_Firmware_Surface | Buffer_Not_Mapped |
                     Buffers_Not_Distinct
       then " buffer" & Index'Image (Problem_Buffer) & " gpu=" &
            Unsigned_32'Image (Surface (Problem_Buffer))
       else "") &
      " firmware-surface=" & Unsigned_32'Image (Firmware));

   --  The source a PLANE_SURFLIVE word names; Known false for any other.
   procedure Decode_Live (Word : Unsigned_32; Item : out FS.Source; Known : out Boolean) is
      Page : constant Unsigned_32 := Word and Surface_Page_Mask;
   begin
      Item := FS.Firmware;
      Known := Word /= Unsigned_32'Last and then Page = Firmware;
      if Known then return; end if;
      for I in Index loop
         if Word /= Unsigned_32'Last and then Page = Surface (I) then
            Item := To_Buffer (I);
            Known := True;
            return;
         end if;
      end loop;
   end Decode_Live;

   procedure Note_Counter (Now : Unsigned_64) is
      Count_Now : constant Unsigned_32 := Read (Frame_Count_Offset);
   begin
      if not Clock_Known then
         Last_Count := Count_Now;
         Last_Change_Us := Now;
         Clock_Known := True;
      elsif Count_Now /= Last_Count then
         if Now > Last_Change_Us then
            Period := VC.Measure (Now - Last_Change_Us,
                                  VC.Frames_Between (Last_Count, Count_Now));
         end if;
         Last_Count := Count_Now;
         Last_Change_Us := Now;
      end if;
   end Note_Counter;

   --  Observe PLANE_SURFLIVE once; Latched when the owed frame is live.
   procedure Observe_Once (Latched : out Boolean) is
      Live : FS.Source;
      Known : Boolean;
      Now : constant Unsigned_64 := Now_Us;
   begin
      Latched := False;
      Decode_Live (Read (Live_Surface_Offset), Live, Known);
      Note_Counter (Now);
      if not Known then
         S.Faulted := True;
         Count (Stats.Faults);
         return;
      end if;
      FS.Observe (S, Live, Latched);
      if S.Faulted then
         Count (Stats.Faults);
         return;
      end if;
      if Latched then
         Count (Stats.Latched);
         if VC.Late (Armed_Count, Last_Count) then
            Count (Stats.Late);
         end if;
         if Now /= Unsigned_64'Last and then Now >= Armed_Us then
            Stats.Latency_Total_Us := Stats.Latency_Total_Us + (Now - Armed_Us);
            Stats.Latency_Max_Us := Unsigned_64'Max (Stats.Latency_Max_Us, Now - Armed_Us);
         end if;
      end if;
   end Observe_Once;

   --  Wait (bounded) until nothing is owed.
   procedure Await (Success : out Boolean) is
      Started : constant Unsigned_64 := Now_Us;
      Latched : Boolean;
      Now : Unsigned_64;
   begin
      Success := False;
      if Started = Unsigned_64'Last then return; end if;
      loop
         Observe_Once (Latched);
         if S.Faulted then return; end if;
         if not S.Owed then
            Success := True;
            return;
         end if;
         Now := Now_Us;
         if Now = Unsigned_64'Last or else Now < Started or else
           Now - Started >= Latch_Deadline_Us
         then
            return;
         end if;
         Pause;
      end loop;
   end Await;

   procedure Fill (Item : Index; Color : Unsigned_32; Success : out Boolean) is
      Words_Per_Row : constant Natural := Frame_Pitch / 4;
      type Pixels is array (0 .. Words_Per_Row * Frame_Height - 1) of Unsigned_32;
      Target : Pixels with Import, Address => To_Address (Integer_Address (Memory (Item)));
   begin
      --  Only a buffer the display engine cannot be reading.
      Success := FS.Writable (S, To_Buffer (Item));
      if not Success then
         Count (Stats.Writes_To_Live);
         return;
      end if;
      Target := [others => Color];
      Success := Flush (Memory (Item), Frame_Bytes);
   end Fill;

   procedure Arm (Item : Index; Success : out Boolean; Superseded : out Boolean) is
   begin
      Superseded := False;
      if not FS.Writable (S, To_Buffer (Item)) or else not FS.Valid (S) then
         Success := False;
         return;
      end if;
      Write (Surface_Offset, Surface (Item), Success);
      if not Success then return; end if;
      FS.Arm (S, To_Buffer (Item), Superseded);
      Armed_Count := Read (Frame_Count_Offset);
      Armed_Us := Now_Us;
      Count (Stats.Armed);
      if Superseded then Count (Stats.Superseded); end if;
   end Arm;

   procedure Take_Over (Color : Unsigned_32; Result : out Takeover_Result) is
      Done, Superseded, Written : Boolean;
   begin
      Is_Ready := False;
      if not Configured or else S.Faulted then
         Result := Not_Configured;
         return;
      end if;
      --  The firmware surface is live: every driver buffer is writable.
      for I in Index loop
         Fill (I, Color, Done);
         if not Done then
            Result := Write_Failed;
            return;
         end if;
      end loop;
      Arm (0, Done, Superseded);
      if not Done then
         Result := Write_Failed;
         return;
      end if;
      Await (Done);
      if Done then
         Is_Ready := True;
         Result := Taken;
         return;
      end if;
      Result := (if S.Faulted then Unexpected_Surface else Latch_Timeout);
      --  Abandon: arm the firmware surface again, then flips stay off.
      Write (Surface_Offset, Firmware, Written);
      if FS.Valid (S) then
         FS.Arm_Firmware (S, Superseded);
      end if;
      S.Faulted := True;
   end Take_Over;

   procedure Submit (Item : Index; First_Row, Rows : Natural;
                     Result : out Submit_Result; Superseded : out Boolean) is
      Done : Boolean;
   begin
      Superseded := False;
      if not Ready then
         Result := Not_Ready;
         return;
      end if;
      if not FS.Writable (S, To_Buffer (Item)) then
         --  display.svc wrote a buffer the display engine may be reading.
         Count (Stats.Writes_To_Live);
         Result := Not_Writable;
         return;
      end if;
      if Rows = 0 or else First_Row >= Frame_Height or else Rows > Frame_Height - First_Row or else
        not Flush (Memory (Item) + Unsigned_64 (First_Row) * Unsigned_64 (Frame_Pitch),
                   Unsigned_64 (Rows) * Unsigned_64 (Frame_Pitch))
      then
         Result := Write_Failed;
         return;
      end if;
      Arm (Item, Done, Superseded);
      Result := (if Done then Armed else Write_Failed);
   end Submit;

   procedure Service (Latched : out Boolean) is
   begin
      Latched := False;
      if Ready and then S.Owed then
         Observe_Once (Latched);
      end if;
   end Service;

   function Next_Poll_Us return Unsigned_64 is
      Now : constant Unsigned_64 := Now_Us;
   begin
      if not Ready or else not S.Owed or else Now = Unsigned_64'Last or else
        Now > VC.Time_Limit - 2 * VC.Maximum_Period_Us
      then
         return Unsigned_64'Last;
      end if;
      if not Clock_Known or else Last_Change_Us > Now then
         return Now;
      end if;
      return VC.Poll_At (Last_Change_Us, Now, Period);
   end Next_Poll_Us;

   procedure Clear (Color : Unsigned_32; Success : out Boolean) is
      Superseded : Boolean;
      First : constant Index := 0;
   begin
      Success := False;
      if not Ready or else S.Owed then return; end if;
      --  Refresh what is live (nothing is owed: Seen settles the rest).
      declare
         Latched : Boolean;
      begin
         Observe_Once (Latched);
      end;
      if S.Faulted then return; end if;
      if not FS.Writable (S, To_Buffer (First)) then
         if not FS.Has_Writable (S, To_Buffer (First)) then return; end if;
         declare
            Other : constant FS.Buffer := FS.First_Writable (S, To_Buffer (First));
            Item : constant Index :=
              Index (FS.Buffer'Pos (Other) - FS.Buffer'Pos (FS.Buffer'First));
         begin
            Fill (Item, Color, Success);
            if not Success then return; end if;
            Arm (Item, Success, Superseded);
            if not Success then return; end if;
            Await (Success);
            if not Success then return; end if;
         end;
      end if;
      Fill (First, Color, Success);
      if not Success then return; end if;
      Arm (First, Success, Superseded);
      if not Success then return; end if;
      Await (Success);
      if not Success then return; end if;
      for I in Index loop
         if I /= First then
            Fill (I, Color, Success);
            if not Success then return; end if;
         end if;
      end loop;
   end Clear;

   function Stall return Stall_View is
      Now : constant Unsigned_64 := Now_Us;
      Item : Stall_View;
   begin
      Item.Owed := S.Owed;
      Item.Faulted := S.Faulted;
      if S.Armed in FS.Buffer then
         Item.Armed := Index (FS.Buffer'Pos (S.Armed) - FS.Buffer'Pos (FS.Buffer'First));
      end if;
      Item.Owed_Us := (if S.Owed and then Now /= Unsigned_64'Last and then Now >= Armed_Us
                       then Now - Armed_Us else 0);
      Item.Surface_Word := Read (Surface_Offset);
      Item.Live_Word := Read (Live_Surface_Offset);
      Item.Frame_Count := Read (Frame_Count_Offset);
      return Item;
   end Stall;

   procedure Take (Item : out Counters) is
      Count_Now : constant Unsigned_32 := Read (Frame_Count_Offset);
   begin
      Item := Stats;
      Item.Period_Us := Period;
      Item.Vblanks := (if Report_Known then VC.Frames_Between (Report_Count, Count_Now) else 0);
      Report_Count := Count_Now;
      Report_Known := True;
      Stats := (others => <>);
   end Take;
end Intel_GPU_Primary_Flip;
