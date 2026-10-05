
package body CCL_Stream_Table with SPARK_Mode is
   use type CCL.Streams.Handle;
   use type CCL.Streams.View_Kind;
   use type CCL.Objects.Build_Result;
   use type Interfaces.Integer_64;

   function Handle_Of (Slot : Slot_Index; Opened : Generation) return CCL.Streams.Handle is
     (CCL.Streams.Handle (Opened * MAX_STREAMS + Slot));

   --  The open stream a handle names, or 0.
   function Slot_Of (Item : Table; Handle : CCL.Streams.Handle) return Natural is
     (if Handle = CCL.Streams.No_Handle then 0
      else
        (declare
            Slot : constant Slot_Index := Natural ((Handle - 1) mod MAX_STREAMS) + 1;
            Opened : constant Natural := Natural ((Handle - 1) / MAX_STREAMS);
         begin
           (if Item.Streams (Slot).Open and then Item.Streams (Slot).Opened = Opened
            then Slot else 0)));

   procedure Open_Timer
     (Item : in out Table; Period : Period_Ms; Now : Interfaces.Unsigned_64;
      Handle : out CCL.Streams.Handle)
   is
      Next : Generation;
   begin
      Handle := CCL.Streams.No_Handle;
      if Now > Interfaces.Unsigned_64'Last - Period then return; end if;
      for Slot in Slot_Index loop
         if not Item.Streams (Slot).Open then
            Next := (if Item.Streams (Slot).Opened = MAX_GENERATION then 0
                     else Item.Streams (Slot).Opened + 1);
            Item.Streams (Slot) := (Open => True, Opened => Next, Period => Period,
                                    Due => Now + Period, Timer => True, others => <>);
            Handle := Handle_Of (Slot, Next);
            return;
         end if;
      end loop;
   end Open_Timer;

   --  Append Value to Item's history; the stream's timing is unchanged.
   procedure Push (Item : in out Stream; Value : Interfaces.Integer_64)
     with Post => Item.Open = Item.Open'Old and then Item.Period = Item.Period'Old and then
                  Item.Due = Item.Due'Old;
   procedure Push (Item : in out Stream; Value : Interfaces.Integer_64) is
      use type History.Index;
      Released : Boolean;
   begin
      if History.Space (Item.Producer) = 0 then
         --  The ring is full: the table consumes its oldest element, lost.
         History.Accept_Consumed
           (Item.Producer, History.Consumed (Item.Producer) + 1, Released);
         if Released and then Item.Lost < CCL.Streams.Element_Total'Last then
            Item.Lost := Item.Lost + 1;
         end if;
      end if;
      if History.Space (Item.Producer) > 0 then
         History.Push (Item.Producer, Item.Elements, Value);
         if Item.Arrived < CCL.Streams.Element_Total'Last then Item.Arrived := Item.Arrived + 1; end if;
      end if;
   end Push;

   procedure Pump (Item : in out Table; Now : Interfaces.Unsigned_64; Changed : out Boolean) is
   begin
      Changed := False;
      for Slot in Slot_Index loop
         declare
            S : Stream renames Item.Streams (Slot);
         begin
         if S.Open and then S.Timer then
            --  More than a ring behind (the host was suspended): skip to
            --  the most recent ring's worth instead of replaying it all.
            if Now >= S.Due then
               declare
                  Behind : constant Interfaces.Unsigned_64 := (Now - S.Due) / S.Period;
               begin
                  --  Whole periods ahead, leaving a ring's worth still due.
                  if Behind >= Interfaces.Unsigned_64 (CAPACITY) then
                     S.Due := S.Due + (Behind - Interfaces.Unsigned_64 (CAPACITY - 1)) * S.Period;
                  end if;
               end;
            end if;
            for Step in 1 .. CAPACITY loop
               exit when S.Due > Now or else
                 S.Due > Interfaces.Unsigned_64 (Interfaces.Integer_64'Last) or else
                 S.Due > Interfaces.Unsigned_64'Last - S.Period;
               Push (S, Interfaces.Integer_64 (S.Due));
               S.Due := S.Due + S.Period;
               Changed := True;
            end loop;
         end if;
         end;
      end loop;
   end Pump;

   procedure Read
     (Item : Table; Request : CCL.Streams.View_Request; Reply : in out CCL.Streams.View_Reply)
   is
      Slot : constant Natural := Slot_Of (Item, Request.Stream);
      Built : CCL.Objects.Build_Result := CCL.Objects.Added;
      procedure Add (Value : CCL.Objects.Cell) is
      begin
         if Built = CCL.Objects.Added then
            CCL.Objects.Append (Reply.Elements, Value, Built);
         end if;
      end Add;
      procedure Add_Line (S : Stream; Line : Line_Slot) is
      begin
         if Built = CCL.Objects.Added then
            CCL.Objects.Append_Text
              (Reply.Elements, S.Lines (Line) (1 .. S.Lengths (Line)), Built);
         end if;
      end Add_Line;
   begin
      Reply.Elements := (others => <>);
      if Slot = 0 then
         Reply.Status := CCL.Streams.No_Such_Stream; return;
      end if;
      --  A handle names a stream or a task, never both: a stream view of a
      --  task, or a wait on a stream (a (task T n) or (stream T n) written
      --  over the other kind), names nothing here.
      if CCL.Streams."=" (Request.View, CCL.Streams.Wait_View) /= (Item.Streams (Slot).Kind = Task_Result) then
         Reply.Status := CCL.Streams.No_Such_Stream; return;
      end if;
      declare
         use type History.Index;
         S : Stream renames Item.Streams (Slot);
         Held : constant History.Fill_Count := S.Producer.Fill;
         Shown : constant Natural := Natural'Min (Request.Count, Held);
         Newest : constant History.Index := S.Producer.Produced - 1;
      begin
         Reply.Status := CCL.Streams.View_Answered;
         case Request.View is
            when CCL.Streams.Arrived_View => Reply.Total := S.Arrived;
            when CCL.Streams.Lost_View => Reply.Total := S.Lost;
            when CCL.Streams.Wait_View =>
               --  A task's result, once it completed.
               if S.Result_Slot = 0 then
                  Reply.Status := CCL.Streams.Stream_Empty;
               else
                  Reply.Elements := Item.Results (S.Result_Slot);
               end if;
            when CCL.Streams.Latest_View =>
               if S.Kind = Text_Elements then
                  if S.Text_Fill = 0 then
                     Reply.Status := CCL.Streams.Stream_Empty;
                  else
                     Add_Line (S, S.Text_Newest);
                  end if;
               elsif Held = 0 then
                  Reply.Status := CCL.Streams.Stream_Empty;
               else
                  Add (CCL.Objects.Integer_Cell (S.Elements (History.Slot_Of (Newest))));
               end if;
            when CCL.Streams.Window_View =>
               --  The newest Shown elements, oldest first.
               if S.Kind = Text_Elements then
                  declare
                     Lines_Shown : constant Natural := Natural'Min (Request.Count, S.Text_Fill);
                  begin
                     Add (CCL.Objects.Sequence_Cell (Lines_Shown));
                     for I in 1 .. Lines_Shown loop
                        Add_Line (S, (S.Text_Newest - 1 - (Lines_Shown - I) + TEXT_HISTORY)
                                     mod TEXT_HISTORY + 1);
                     end loop;
                  end;
               else
                  Add (CCL.Objects.Sequence_Cell (Shown));
                  for I in 1 .. Shown loop
                     Add (CCL.Objects.Integer_Cell
                            (S.Elements (History.Slot_Of (Newest - History.Index (Shown - I)))));
                  end loop;
               end if;
         end case;
      end;
      if Built /= CCL.Objects.Added then
         Reply.Status := CCL.Streams.No_Such_Stream;
      end if;
   end Read;

   procedure Open_Outlet
     (Item : in out Table; Kind : Element_Kind; Handle : out CCL.Streams.Handle;
      Pinned : Boolean := False)
   is
      Next : Generation;
   begin
      Handle := CCL.Streams.No_Handle;
      for Slot in Slot_Index loop
         if not Item.Streams (Slot).Open then
            Next := (if Item.Streams (Slot).Opened = MAX_GENERATION then 0
                     else Item.Streams (Slot).Opened + 1);
            Item.Streams (Slot) := (Open => True, Opened => Next, Kind => Kind,
                                    Pinned => Pinned, others => <>);
            Handle := Handle_Of (Slot, Next);
            return;
         end if;
      end loop;
   end Open_Outlet;

   procedure Push_Integer
     (Item : in out Table; Handle : CCL.Streams.Handle; Value : Interfaces.Integer_64;
      Pushed : out Boolean)
   is
      Slot : constant Natural := Slot_Of (Item, Handle);
   begin
      Pushed := Slot /= 0 and then not Item.Streams (Slot).Timer
        and then Item.Streams (Slot).Kind = Integer_Elements
        and then not Item.Streams (Slot).Finished;
      if Pushed then
         Push (Item.Streams (Slot), Value);
      end if;
   end Push_Integer;

   procedure Push_Text
     (Item : in out Table; Handle : CCL.Streams.Handle; Line : String; Pushed : out Boolean)
   is
      Slot : constant Natural := Slot_Of (Item, Handle);
      Kept : constant Natural := Natural'Min (Line'Length, MAX_LINE);
   begin
      Pushed := Slot /= 0 and then Item.Streams (Slot).Kind = Text_Elements
        and then not Item.Streams (Slot).Finished;
      if not Pushed then
         return;
      end if;
      declare
         S : Stream renames Item.Streams (Slot);
         Next : constant Line_Slot := S.Text_Newest mod TEXT_HISTORY + 1;
      begin
         S.Lines (Next) := [others => ' '];
         S.Lines (Next) (1 .. Kept) := Line (Line'First .. Line'First + Kept - 1);
         S.Lengths (Next) := Kept;
         S.Text_Newest := Next;
         if S.Text_Fill < TEXT_HISTORY then
            S.Text_Fill := S.Text_Fill + 1;
         elsif S.Lost < CCL.Streams.Element_Total'Last then
            S.Lost := S.Lost + 1;
         end if;
         if S.Arrived < CCL.Streams.Element_Total'Last then
            S.Arrived := S.Arrived + 1;
         end if;
      end;
   end Push_Text;

   procedure Open_Task
     (Item : in out Table; Handle : out CCL.Streams.Handle; Pinned : Boolean := False) is
   begin
      Open_Outlet (Item, Task_Result, Handle, Pinned);
   end Open_Task;

   procedure Complete_Task
     (Item : in out Table; Handle : CCL.Streams.Handle; Result : CCL.Objects.Image;
      Completed : out Boolean)
   is
      Slot : constant Natural := Slot_Of (Item, Handle);
   begin
      Completed := False;
      if Slot = 0 or else Item.Streams (Slot).Kind /= Task_Result
        or else Item.Streams (Slot).Finished
      then
         return;
      end if;
      for R in Item.Results'Range loop
         if not Item.Used (R) then
            Item.Used (R) := True;
            --  A view reply is a local image (CCL.Objects.Views
            --  .Capture_Local): the reader checks it against the type it
            --  waits for, so the producer's schema claim is not kept.
            Item.Results (R) := Result;
            Item.Results (R).Schema := CCL.Objects.No_Schema;
            Item.Streams (Slot).Result_Slot := R;
            Item.Streams (Slot).Finished := True;
            Item.Streams (Slot).Arrived := 1;
            Completed := True;
            return;
         end if;
      end loop;
   end Complete_Task;

   function Task_Done (Item : Table; Handle : CCL.Streams.Handle) return Boolean is
     (Slot_Of (Item, Handle) /= 0
      and then Item.Streams (Slot_Of (Item, Handle)).Kind = Task_Result
      and then Item.Streams (Slot_Of (Item, Handle)).Result_Slot /= 0);

   --  Free a closing stream's task result, if it had one.
   procedure Release_Result (Item : in out Table; Slot : Slot_Index) is
   begin
      if Item.Streams (Slot).Result_Slot /= 0 then
         Item.Used (Item.Streams (Slot).Result_Slot) := False;
      end if;
   end Release_Result;

   procedure Unpin (Item : in out Table; Handle : CCL.Streams.Handle) is
      Slot : constant Natural := Slot_Of (Item, Handle);
   begin
      if Slot /= 0 then
         Item.Streams (Slot).Pinned := False;
      end if;
   end Unpin;

   procedure End_Stream (Item : in out Table; Handle : CCL.Streams.Handle) is
      Slot : constant Natural := Slot_Of (Item, Handle);
   begin
      if Slot /= 0 then
         Item.Streams (Slot).Finished := True;
      end if;
   end End_Stream;

   function Ended (Item : Table; Handle : CCL.Streams.Handle) return Boolean is
     (Slot_Of (Item, Handle) /= 0 and then Item.Streams (Slot_Of (Item, Handle)).Finished);

   procedure Close (Item : in out Table; Handle : CCL.Streams.Handle) is
      Slot : constant Natural := Slot_Of (Item, Handle);
   begin
      if Slot /= 0 then
         Release_Result (Item, Slot);
         Item.Streams (Slot) := (Opened => Item.Streams (Slot).Opened, others => <>);
      end if;
   end Close;

   procedure Retain (Item : in out Table) is
   begin
      for Slot in Slot_Index loop
         if Item.Streams (Slot).Open and then not Item.Streams (Slot).Pinned and then
           not Held (Handle_Of (Slot, Item.Streams (Slot).Opened))
         then
            Release_Result (Item, Slot);
            Item.Streams (Slot) := (Opened => Item.Streams (Slot).Opened, others => <>);
         end if;
      end loop;
   end Retain;

   procedure Clear (Item : in out Table) is
   begin
      for S of Item.Streams loop
         S := (Opened => S.Opened, others => <>);
      end loop;
      Item.Used := [others => False];
   end Clear;

   function Next_Due (Item : Table) return Interfaces.Unsigned_64 is
      Earliest : Interfaces.Unsigned_64 := Interfaces.Unsigned_64'Last;
   begin
      for S of Item.Streams loop
         if S.Open and then S.Timer and then S.Due < Earliest then Earliest := S.Due; end if;
      end loop;
      return Earliest;
   end Next_Due;

   function Open_Count (Item : Table) return Natural is
      Count : Natural := 0;
   begin
      for Slot in Slot_Index loop
         pragma Loop_Invariant (Count <= Slot - 1);
         if Item.Streams (Slot).Open then Count := Count + 1; end if;
      end loop;
      return Count;
   end Open_Count;
end CCL_Stream_Table;
