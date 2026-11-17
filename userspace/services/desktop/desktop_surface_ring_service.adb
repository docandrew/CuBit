------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Desktop's side of the windows' surface rings (see the spec).
------------------------------------------------------------------------------
pragma Ada_2022;
with System;
with Desktop_Heap_Objects;

package body Desktop_Surface_Ring_Service is

   use type System.Address;
   use type DP.Status_Code;
   use type DP.Input_Event_Kind;
   use type DP.Pixel_Extent;
   use type Wire.Request_Kind;
   use type SR.RC.Token;
   package Pub renames CuBit.Desktop_Protocol.Publication;

   ---------------------------------------------------------------------------
   --  What each ring is owed.
   ---------------------------------------------------------------------------
   type Present_Debt is record
      Active : Boolean := False;
      Tag    : SR.RC.Token := 0;
      Target : Unsigned_64 := 0;
   end record;
   type Present_Debts is array (1 .. SR.Frames_In_Flight) of Present_Debt;

   type Retirement_Debt is record
      Active : Boolean := False;
      Tag    : SR.RC.Token := 0;
      Target : Unsigned_64 := 0;
      Ticket : Unsigned_64 := 0;
   end record;
   type Retirement_Debts is array (1 .. SR.Retirements_In_Flight) of Retirement_Debt;

   type Input_Debt is record
      Active : Boolean := False;
      Tag    : SR.RC.Token := 0;
      Target : Unsigned_64 := 0;
      After  : Unsigned_64 := 0;
   end record;

   type Ring_Debts is record
      Input       : Input_Debt;
      Presents    : Present_Debts;
      Retirements : Retirement_Debts;
   end record;

   type Debt_Array is array (Positive range <>) of Ring_Debts;
   type Debt_Table (Count : Natural) is record
      Rings : Debt_Array (1 .. Count);
   end record;
   package Table_Heap is new Desktop_Heap_Objects (Debt_Table);
   use type Table_Heap.Reference;
   Table : Table_Heap.Reference;

   Ended_Counts : Reason_Counts := [others => 0];
   Skipped : Unsigned_64 := 0;

   procedure Count (Value : in out Unsigned_64) is
   begin
      if Value < Unsigned_64'Last then
         Value := Value + 1;
      end if;
   end Count;

   function Slot (I : Ring_Index) return Positive is (Positive (I));

   function Tracked (I : Ring_Index) return Boolean is
     (Table /= null and then I >= 1 and then Natural (I) <= Table.Count);

   procedure Grow (Count : Ring_Index; Grown : out Boolean) is
      Wanted : constant Natural := Natural (Count);
   begin
      Grown := Table /= null and then Wanted <= Table.Count;
      if Grown or else Count < 1 then
         return;
      end if;
      declare
         subtype New_Table is Debt_Table (Wanted);
         Room : constant System.Address := Table_Heap.Allocate (New_Table'Max_Size_In_Storage_Elements);
      begin
         if Room = System.Null_Address then
            return;
         end if;
         declare
            pragma Warnings (Off, "*may modify overlaid storage*");
            pragma Warnings (Off, "*to suppress initialization*");
            --  SEC-025: own Desktop's heap memory; no peer maps it.
            Fresh : New_Table with Address => Room;
            pragma Warnings (On, "*to suppress initialization*");
            pragma Warnings (On, "*may modify overlaid storage*");
         begin
            if Table /= null then
               Fresh.Rings (1 .. Table.Count) := Table.Rings;
            end if;
         end;
         Table_Heap.Free (Table);
         Table := Table_Heap.To_Reference (Room);
         Grown := True;
      end;
   end Grow;

   ---------------------------------------------------------------------------
   --  Answers.
   ---------------------------------------------------------------------------

   --  Write Value into ring I (if it is still open).
   procedure Answer (I : Ring_Index; Tag : SR.RC.Token; Value : Wire.Result)
     with Pre => Wire.Valid_Events (Value);
   procedure Answer (I : Ring_Index; Tag : SR.RC.Token; Value : Wire.Result) is
   begin
      if Is_Active (I) then
         Complete (I, Tag, Wire.Encode (Value));
      end if;
   end Answer;

   function Failed (Kind : Wire.Request_Kind; Status : DP.Status_Code) return Wire.Result is
     (case Kind is
        when Wire.Present => (Kind => Wire.Present, Status => Status),
        when Wire.Publish => (Kind => Wire.Publish, Status => Status, others => <>),
        when Wire.Await_Retirement => (Kind => Wire.Await_Retirement, Status => Status, others => <>),
        when Wire.Receive_Input => (Kind => Wire.Receive_Input, Status => Status, others => <>))
     with Pre => Status /= DP.Success;

   function Receipt_Answer (Kind : Wire.Request_Kind; Receipt : Pub.Receipt) return Wire.Result is
     (if Receipt.Status = DP.Success then
        (case Kind is
           when Wire.Publish =>
             (Kind => Wire.Publish, Status => DP.Success, Epoch => Receipt.Epoch, Ticket => Receipt.Ticket),
           when others =>
             (Kind => Wire.Await_Retirement, Status => DP.Success,
              Epoch => Receipt.Epoch, Ticket => Receipt.Ticket))
      else Failed (Kind, Receipt.Status))
     with Pre => Kind in Wire.Publish | Wire.Await_Retirement;

   procedure Ring_Ended (I : Ring_Index) is
   begin
      if Tracked (I) then
         Table.Rings (Slot (I)) := (others => <>);
      end if;
   end Ring_Ended;

   --  Ring I broke its protocol: say why, forget what it is owed, end it.
   procedure End_For (I : Ring_Index; Reason : End_Reason) is
   begin
      Count (Ended_Counts (Reason));
      Report ("desktop: surface ring" & Ring_Index'Image (I) & " (client" &
              Unsigned_64'Image (CuBit.Process_IDs.To_Word (Client_Of (I))) & ") ended: " &
              Reason_Text (Reason));
      Ring_Ended (I);
      End_Ring (I);
   end End_For;

   ---------------------------------------------------------------------------
   --  Input.
   ---------------------------------------------------------------------------

   --  Up to Input_Batch valid events for Target after serial After, in
   --  serial order. Found False: none queued.
   procedure Collect_Input
     (Target, After : Unsigned_64; Result : out Wire.Result; Found : out Boolean)
     with Post => (if Found then Result.Kind = Wire.Receive_Input and then Result.Status = DP.Success
                   and then Wire.Valid_Events (Result));
   procedure Collect_Input
     (Target, After : Unsigned_64; Result : out Wire.Result; Found : out Boolean)
   is
      Last : Unsigned_64 := After;
      Item : DP.Input_Envelope;
      Got : Boolean;
      Events : Wire.Event_Array := [others => (others => <>)];
      Taken : Wire.Event_Count := 0;
   begin
      while Taken < Wire.Input_Batch loop
         Next_Input (Target, Last, Item, Got);
         exit when not Got or else Item.Serial <= Last;
         Last := Item.Serial;
         --  Desktop queues only valid events; one that is not is counted
         --  and skipped rather than shown to the client.
         Item.More_Pending := False;
         if Item.Kind /= DP.No_Input and then DP.Valid_Input_Envelope (Item) then
            Taken := Taken + 1;
            Events (Taken) := Item;
         else
            Count (Skipped);
         end if;
      end loop;
      Found := Taken > 0;
      Result := (Kind => Wire.Receive_Input, Status => DP.Success, Count => Taken,
                 More => Found and then Input_After (Target, Last), Events => Events);
   end Collect_Input;

   procedure Input_Queued (Target : Unsigned_64) is
      Result : Wire.Result;
      Found : Boolean;
   begin
      if Table = null then
         return;
      end if;
      for R in Table.Rings'Range loop
         declare
            D : Input_Debt renames Table.Rings (R).Input;
         begin
            if D.Active and then D.Target = Target then
               Collect_Input (Target, D.After, Result, Found);
               if Found then
                  Answer (Ring_Index (R), D.Tag, Result);
                  D := (others => <>);
               end if;
            end if;
         end;
      end loop;
   end Input_Queued;

   ---------------------------------------------------------------------------
   --  Serving one request.
   ---------------------------------------------------------------------------

   --  Damage clamped to Width x Height. Whole: the whole surface (all
   --  zero, or bounds unknown). Empty: none of it is inside.
   procedure Clamp
     (Area : DP.Rectangle; Width, Height : Natural;
      Result : out DP.Rectangle; Whole, Empty : out Boolean) is
   begin
      Whole := (Area.Width = 0 and then Area.Height = 0) or else Width = 0 or else Height = 0;
      Result := (0, 0, 0, 0);
      Empty := False;
      if Whole then
         return;
      end if;
      Result := DP.Clip (Area, Natural'Min (Width, Natural (DP.Pixel_Extent'Last)),
                         Natural'Min (Height, Natural (DP.Pixel_Extent'Last)));
      Empty := Result.Width = 0 or else Result.Height = 0;
      if Empty then
         Result := (0, 0, 0, 0);
      end if;
   end Clamp;

   procedure Serve
     (I : Ring_Index; Item : SR.RC.Queues.Submission; Ended : out Boolean)
   is
      From : constant CuBit.Process_IDs.Process_ID := Client_Of (I);
      --  Item is Desktop's own copy: decoded once, never read again from
      --  the client's page.
      Decoded : constant Wire.Request_Decoding := Wire.Decode (Item.Item);
      Debts : Ring_Debts renames Table.Rings (Slot (I));
      Tag : constant SR.RC.Token := Item.Tag;
   begin
      Ended := False;
      if not Decoded.Valid then
         End_For (I, Malformed_Request);
         Ended := True;
         return;
      end if;
      declare
         R : Wire.Request renames Decoded.Value;
         Target : constant Unsigned_64 := Unsigned_64 (R.Surface);
         Allowed : constant DP.Status_Code := Surface_Access (From, Target);
         Attached_W, Attached_H, Source_W, Source_H : Natural;
      begin
         if Allowed /= DP.Success then
            Answer (I, Tag, Failed (R.Kind, Allowed));
            return;
         end if;
         Bounds (Target, Attached_W, Attached_H, Source_W, Source_H);
         case R.Kind is
            when Wire.Present =>
               declare
                  Free : Natural := 0;
                  Area : DP.Rectangle;
                  Whole, Empty : Boolean;
                  Status : DP.Status_Code;
               begin
                  for P in Debts.Presents'Range loop
                     if not Debts.Presents (P).Active then
                        Free := P;
                        exit;
                     end if;
                  end loop;
                  if Free = 0 then
                     End_For (I, Too_Many_Frames);
                     Ended := True;
                     return;
                  end if;
                  Clamp (R.Area, Attached_W, Attached_H, Area, Whole, Empty);
                  if Empty then
                     --  Nothing it names is inside the surface: nothing
                     --  to draw, so nothing to wait for.
                     Answer (I, Tag, (Kind => Wire.Present, Status => DP.Success));
                     return;
                  end if;
                  Present_Attached (From, (R.Surface, Area), Status);
                  if Status = DP.Success then
                     --  Answered once the frame showing it is drawn.
                     Debts.Presents (Free) := (True, Tag, Target);
                  else
                     Answer (I, Tag, Failed (Wire.Present, Status));
                  end if;
               end;

            when Wire.Publish =>
               declare
                  Staged, Published : Pub.Receipt;
                  Area : DP.Rectangle;
                  Whole, Empty : Boolean;
               begin
                  Stage (From, (R.Surface, R.Epoch, R.Grant), Staged);
                  if Staged.Status /= DP.Success then
                     Answer (I, Tag, Failed (Wire.Publish, Staged.Status));
                     return;
                  end if;
                  --  An area outside the buffer publishes the whole buffer
                  --  (all zero): conservative, never out of bounds.
                  Clamp (R.Area, Source_W, Source_H, Area, Whole, Empty);
                  Publish (From, (R.Surface, R.Epoch, Staged.Ticket, Area, R.Input_After), Published);
                  Answer (I, Tag, Receipt_Answer (Wire.Publish, Published));
               end;

            when Wire.Await_Retirement =>
               declare
                  Retired : Pub.Receipt;
                  Free : Natural := 0;
               begin
                  Retirement (From, (R.Surface, R.Ticket), Retired);
                  if Retired.Status /= DP.Bad_State then
                     Answer (I, Tag, Receipt_Answer (Wire.Await_Retirement, Retired));
                     return;
                  end if;
                  --  Still read: answered when it retires (Pump_Retirements).
                  for P in Debts.Retirements'Range loop
                     if not Debts.Retirements (P).Active then
                        Free := P;
                        exit;
                     end if;
                  end loop;
                  if Free = 0 then
                     End_For (I, Too_Many_Retirement_Waits);
                     Ended := True;
                     return;
                  end if;
                  Debts.Retirements (Free) := (True, Tag, Target, R.Ticket);
               end;

            when Wire.Receive_Input =>
               if Debts.Input.Active then
                  End_For (I, Input_Already_Requested);
                  Ended := True;
                  return;
               end if;
               declare
                  Result : Wire.Result;
                  Found : Boolean;
               begin
                  Collect_Input (Target, R.After, Result, Found);
                  if Found then
                     Answer (I, Tag, Result);
                  else
                     Debts.Input := (True, Tag, Target, R.After);
                  end if;
               end;
         end case;
      end;
   end Serve;

   ---------------------------------------------------------------------------
   --  Later answers.
   ---------------------------------------------------------------------------

   procedure Frame_Drawn is
   begin
      if Table = null then
         return;
      end if;
      for R in Table.Rings'Range loop
         for D of Table.Rings (R).Presents loop
            if D.Active then
               Answer (Ring_Index (R), D.Tag, (Kind => Wire.Present, Status => DP.Success));
               D := (others => <>);
            end if;
         end loop;
      end loop;
   end Frame_Drawn;

   procedure Pump_Retirements is
      Retired : Pub.Receipt;
   begin
      if Table = null then
         return;
      end if;
      for R in Table.Rings'Range loop
         for D of Table.Rings (R).Retirements loop
            if D.Active then
               if not Is_Active (Ring_Index (R)) then
                  D := (others => <>);
               else
                  Retirement (Client_Of (Ring_Index (R)),
                              (DP.Live_Surface_Name (D.Target), D.Ticket), Retired);
                  if Retired.Status /= DP.Bad_State then
                     Answer (Ring_Index (R), D.Tag, Receipt_Answer (Wire.Await_Retirement, Retired));
                     D := (others => <>);
                  end if;
               end if;
            end if;
         end loop;
      end loop;
   end Pump_Retirements;

   procedure Surface_Ended (Target : Unsigned_64) is
   begin
      if Table = null then
         return;
      end if;
      for R in Table.Rings'Range loop
         declare
            Debts : Ring_Debts renames Table.Rings (R);
            I : constant Ring_Index := Ring_Index (R);
         begin
            if Debts.Input.Active and then Debts.Input.Target = Target then
               Answer (I, Debts.Input.Tag, Failed (Wire.Receive_Input, DP.Bad_Object));
               Debts.Input := (others => <>);
            end if;
            for D of Debts.Presents loop
               if D.Active and then D.Target = Target then
                  Answer (I, D.Tag, Failed (Wire.Present, DP.Bad_Object));
                  D := (others => <>);
               end if;
            end loop;
            for D of Debts.Retirements loop
               if D.Active and then D.Target = Target then
                  Answer (I, D.Tag, Failed (Wire.Await_Retirement, DP.Bad_Object));
                  D := (others => <>);
               end if;
            end loop;
         end;
      end loop;
   end Surface_Ended;

   function Owed (I : Ring_Index) return Natural is
      Total : Natural := 0;
   begin
      if not Tracked (I) then
         return 0;
      end if;
      declare
         Debts : Ring_Debts renames Table.Rings (Slot (I));
      begin
         Total := (if Debts.Input.Active then 1 else 0);
         for D of Debts.Presents loop
            Total := Total + (if D.Active then 1 else 0);
         end loop;
         for D of Debts.Retirements loop
            Total := Total + (if D.Active then 1 else 0);
         end loop;
      end;
      return Total;
   end Owed;

   function Ended_Rings return Reason_Counts is (Ended_Counts);
   function Skipped_Input return Unsigned_64 is (Skipped);

end Desktop_Surface_Ring_Service;
