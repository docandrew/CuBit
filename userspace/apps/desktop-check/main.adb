with Desktop_Input_Batch;
with Desktop_Frame_Pair;
with Desktop_Density_Text;
with Client_Frame_Damage;
with Client_Frame_Buffer;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Desktop_Protocol.Publication;
with CuBit.Memory_Grants;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Desktop_Protocol; use CuBit.Desktop_Protocol;
with CuBit.Desktop_Messages; use CuBit.Desktop_Messages;
procedure Main is
   Passed : Boolean := True;
   Wire, Response : Wire_Message;
   Created : Creation_Result;
   Names : array (Positive range 1 .. 128) of Surface_Name := [others => 0];
   Count : Natural := 0;
   Exhausted : Boolean := False;
   package MG renames CuBit.Memory_Grants;
   use type MG.Grant_Reference;
   Held : MG.Grant_Reference;
   Has_Held : Boolean := False;
   procedure Check (Condition : Boolean; Name : String) is
   begin
      if not Condition then
         Passed := False;
         debugPrint ("TEST: FAIL desktop protocol " & Name & ASCII.LF);
      end if;
   end Check;
   function Send (Wire : Wire_Message) return Wire_Message is
      Msg : Message := From_Wire (Wire);
   begin
      Msg.tag := capCall (CAP_SLOT_DESKTOP, Msg, CuBit.Messages.Wait_Forever);
      return To_Wire (Msg);
   end Send;
   procedure Check_Publication_Protocol is
      package Pub renames CuBit.Desktop_Protocol.Publication;
      use type Pub.Receipt;
      Fixture : constant Creation_Result := Decode_Creation_Result
        (Send (Encode_Create ((320, 200, Window_Surface))));
      Config : Pub.Configuration_Result;
      Grants : array (Positive range 1 .. 2) of MG.Grant_Reference;
      Receipts : array (Positive range 1 .. 2) of Pub.Receipt;
      OK : Boolean;
      Pages : Natural;
      Raw, Ignore : Unsigned_64;
      Current : Positive := 1;
      Other : Positive;
      Result, Request : Wire_Message;
      Resized : Resize_Result;
   begin
      Check (Fixture.Status = Success, "publication window created");
      if Fixture.Status /= Success then return; end if;
      Config := Pub.Decode_Configuration (Send (Pub.Encode_Query ((Fixture.Surface, 0), False)));
      Check (Config.Status = Success, "publication window configured");
      if Config.Status /= Success then return; end if;
      Pages := Natural ((Byte_Length (Config.Value.Layout) + 4095) / 4096);
      Raw := syscall (SYSCALL_SBRK, Unsigned_64 (Pages * 8192 + 4096));
      Check (Raw /= Unsigned_64'Last, "publication pair allocated");
      if Raw = Unsigned_64'Last then return; end if;
      declare
         Address : constant Integer_Address := Integer_Address ((Raw + 4095) and not Unsigned_64'(4095));
      begin
         for I in Grants'Range loop
            MG.Create_Via_Capability (CAP_SLOT_DESKTOP,
              To_Address (Address + Integer_Address ((I - 1) * Pages * 4096)), Pages, False, Grants (I), OK);
            Check (OK, "publication pair grant created");
            if not OK then return; end if;
         end loop;
         Check (Send (Pub.Encode_Publish ((1, Config.Value.Epoch, 1, (0, 0, 0, 0), 0))) =
                  Pub.Encode_Receipt ((Status => Denied), Pub.Publish_Label), "foreign publication denied");
         for Frame in 1 .. 13 loop
            Other := (if Current = 1 then 2 else 1);
            if Receipts (Current).Status = Success then
               Check (Pub.Decode_Receipt
                 (Send (Pub.Encode_Query ((Fixture.Surface, Receipts (Current).Ticket), True)), Pub.Retirement_Label)
                   = Receipts (Current), "retirement before producer rewrite");
            end if;
            declare
               Pixels : array (Natural range 0 .. Pages * 1024 - 1) of Unsigned_32
                 with Address => To_Address (Address + Integer_Address ((Current - 1) * Pages * 4096)), Volatile;
            begin
               for Y in 0 .. Natural (Config.Value.Layout.Height) - 1 loop
                  for X in 0 .. Natural (Config.Value.Layout.Width) - 1 loop
                     Pixels (Y * (Config.Value.Layout.Pitch / 4) + X) :=
                       (if Frame < 12 then 16#FF00_0000# or Unsigned_32 (Frame * 16#010101#)
                        elsif Frame = 13 and then X in 7 .. 19 and then Y in 9 .. 19 then 16#FF40_D080#
                        elsif Y < Natural (Config.Value.Layout.Height) / 2 then 16#FF20_60A0# else 16#FFE0_A040#);
                  end loop;
               end loop;
            end;
            Receipts (Current) := Pub.Decode_Receipt
              (Send (Pub.Encode_Stage ((Fixture.Surface, Config.Value.Epoch, Grants (Current)))), Pub.Stage_Label);
            Check (Receipts (Current).Status = Success, "replacement staged beside visible buffer");
            exit when Receipts (Current).Status /= Success;
            Request := Pub.Encode_Publish
              ((Fixture.Surface, Config.Value.Epoch, Receipts (Current).Ticket,
                (if Frame = 13 then (7, 9, 13, 11) else (0, 0, 0, 0)), 0));
            Result := Request; Result.Reserved := 1;
            Check (Send (Result) = Pub.Encode_Receipt ((Status => Invalid_Request), Pub.Publish_Label),
                   "malformed publication rejected");
            Result := Request; Result.Words (1) := Unsigned_64'Last;
            Check (Send (Result) = Pub.Encode_Receipt ((Status => Invalid_Request), Pub.Publish_Label),
                   "invalid publication ticket rejected");
            Result := Request; Result.Words (1) := Config.Value.Epoch + 1 +
              Shift_Left (Receipts (Current).Ticket, 32);
            Check (Send (Result) = Pub.Encode_Receipt ((Status => Bad_State), Pub.Publish_Label),
                   "wrong generation preserves candidate");
            Result := Request; Result.Words (1) := Config.Value.Epoch +
              Shift_Left (Receipts (Current).Ticket + 1, 32);
            Check (Send (Result) = Pub.Encode_Receipt ((Status => Bad_State), Pub.Publish_Label),
                   "wrong ticket preserves candidate");
            Check (Pub.Decode_Receipt (Send (Request), Pub.Publish_Label) = Receipts (Current),
                   "matching candidate becomes visible");
            Check (Send (Request) = Pub.Encode_Receipt ((Status => Bad_State), Pub.Publish_Label),
                   "duplicate publication cannot republish visible buffer");
            Check (Send (Pub.Encode_Query ((Fixture.Surface, Receipts (Current).Ticket), True)) =
                     Pub.Encode_Receipt ((Status => Bad_State), Pub.Retirement_Label),
                   "visible buffer cannot retire");
            Current := Other;
         end loop;
         debugPrint ("DESKTOP-PUBLICATION-PROTOCOL-VISIBLE: ready width=" &
           Natural'Image (Natural (Config.Value.Width)) & " height=" &
           Natural'Image (Natural (Config.Value.Height)) & ASCII.LF);
         -- Observer time only, never treated as proof of retirement/completion.
         Ignore := syscall (SYSCALL_YIELD, 0);
         Other := (if Current = 1 then 2 else 1);
         Resized := Decode_Resize_Result (Send (Encode_Resize ((Fixture.Surface, 352, 224))));
         Check (Resized.Status = Success, "visible publication resize");
         Config := Pub.Decode_Configuration (Send (Pub.Encode_Query ((Fixture.Surface, 0), False)));
         Check (Config.Status = Success, "visible replacement configuration");
         if Receipts (Other).Status = Success then
            Check (Send (Pub.Encode_Query ((Fixture.Surface, Receipts (Other).Ticket), True)) =
                     Pub.Encode_Receipt ((Status => Bad_State), Pub.Retirement_Label),
                   "resize preserves visible reader ownership");
         end if;
         Result := Send (Encode_Destroy ((Surface => Fixture.Surface)));
         Check (Decode_Status (Result, Destroy_Surface) = Success, "destroy visible and retiring slots");
         for I in Grants'Range loop
            MG.Revoke (Grants (I), OK);
            Check (OK and then MG.Retirement_Confirmed (Grants (I)), "published loan released exactly once");
         end loop;
      end;
      debugPrint ("DESKTOP-PUBLICATION-PROTOCOL-CHECK: complete" & ASCII.LF);
   end Check_Publication_Protocol;

   procedure Check_Publication_Display is
      package Pub renames CuBit.Desktop_Protocol.Publication;
      package Frames renames Client_Frame_Buffer;
      package Debt renames Client_Frame_Damage;
      use type Debt.Box;
      Repaint : Debt.State;
      Extent, Repair, Changed : Debt.Box;
      use type System.Address;
      Fixture : constant Creation_Result := Decode_Creation_Result
        (Send (Encode_Create ((320, 200, Window_Surface))));
      Config : Pub.Configuration_Result;
      Buffers : array (Positive range 1 .. 2) of Frames.Buffer;
      OK : Boolean;
      Pages : Natural;
      Ignore : Unsigned_64;
      Current : Positive := 1;
      Other : Positive;
      Result : Wire_Message;
      Resized : Resize_Result;
   begin
      Check (Fixture.Status = Success, "publication window created");
      if Fixture.Status /= Success then return; end if;
      Config := Pub.Decode_Configuration (Send (Pub.Encode_Query ((Fixture.Surface, 0), False)));
      Check (Config.Status = Success, "publication window configured");
      if Config.Status /= Success then return; end if;
      Extent := (0, 0, Natural (Config.Value.Layout.Width), Natural (Config.Value.Layout.Height));
      Repaint := Debt.Open (Extent);
      Pages := Natural ((Byte_Length (Config.Value.Layout) + 4095) / 4096);
      for I in Buffers'Range loop
         Frames.Allocate (Buffers (I), Natural (Byte_Length (Config.Value.Layout)), OK);
         Check (OK and Frames.Capacity (Buffers (I)) = Pages * 4096, "reclaimable frame allocation");
         if not OK then return; end if;
      end loop;
      Frames.Present (Buffers (1), 1, Config.Value.Epoch, (0, 0, 0, 0), OK);
      Check (not OK and Frames.Writable_Address (Buffers (1)) = System.Null_Address,
             "foreign stage refusal retains sealed memory");
      Frames.Prepare_Write (Buffers (1), OK);
      Check (OK, "confirmed stage refusal can reopen unborrowed memory");
      for Frame in 1 .. 15 loop
         Other := (if Current = 1 then 2 else 1);
         Frames.Prepare_Write (Buffers (Current), OK);
         Check (OK, "exact retirement restores producer write ownership");
         exit when not OK;
         Changed := (if Frame >= 13 then (7, 9, 20, 20) else Extent);
         Debt.Invalidate (Repaint, Changed);
         Repair := Debt.Required (Repaint, Current);
         Check (Debt.Publication_Damage (Repaint) = Changed,
                "repair debt does not inflate publication damage");
         if Frame = 15 then
            Check (Repair = Changed, "retired frame repairs only 143 stale pixels");
         end if;
         declare
            Pixels : array (Natural range 0 .. Pages * 1024 - 1) of Unsigned_32
              with Address => Frames.Writable_Address (Buffers (Current)), Volatile;
         begin
            for Y in Repair.Top .. Repair.Bottom - 1 loop
               for X in Repair.Left .. Repair.Right - 1 loop
                  Pixels (Y * (Config.Value.Layout.Pitch / 4) + X) :=
                    (if Frame < 12 then 16#FF00_0000# or Unsigned_32 (Frame * 16#010101#)
                     elsif Frame >= 13 and then X in 7 .. 19 and then Y in 9 .. 19 then (if Frame = 14 then 16#FFE0_4080# else 16#FF40_D080#)
                     elsif Y < Natural (Config.Value.Layout.Height) / 2 then 16#FF20_60A0# else 16#FFE0_A040#);
               end loop;
            end loop;
         end;
         Frames.Present (Buffers (Current), Fixture.Surface, Config.Value.Epoch,
           (if Frame >= 13 then (7, 9, 13, 11) else (0, 0, 0, 0)), OK);
         Check (OK, "protected frame staged and published");
         if OK then Debt.Published (Repaint, Current, Repair); end if;
         Check (Frames.Writable_Address (Buffers (Current)) = System.Null_Address,
                "published frame exposes no writable canvas");
         Frames.Present (Buffers (Current), Fixture.Surface, Config.Value.Epoch, (0, 0, 0, 0), OK);
         Check (not OK, "client prevents duplicate publication");
         Frames.Prepare_Write (Buffers (Current), OK);
         Check (not OK and Frames.Writable_Address (Buffers (Current)) = System.Null_Address,
                "visible frame cannot become writable");
         Current := Other;
      end loop;
      debugPrint ("DESKTOP-CLIENT-REPAINT-CHECK: complete frames=15 final-pixels=143" & ASCII.LF);
      debugPrint ("DESKTOP-PUBLICATION-VISIBLE: ready width=" &
        Natural'Image (Natural (Config.Value.Width)) & " height=" &
        Natural'Image (Natural (Config.Value.Height)) & ASCII.LF);
      Ignore := syscall (SYSCALL_SLEEP, 4000);
      Other := (if Current = 1 then 2 else 1);
      Resized := Decode_Resize_Result (Send (Encode_Resize ((Fixture.Surface, 352, 224))));
      Check (Resized.Status = Success, "visible publication resize");
      Config := Pub.Decode_Configuration (Send (Pub.Encode_Query ((Fixture.Surface, 0), False)));
      Check (Config.Status = Success, "visible replacement configuration");
      Frames.Prepare_Write (Buffers (Other), OK);
      Check (not OK, "resize preserves visible read ownership");
      Frames.Release (Buffers (Other), OK);
      Check (not OK and Frames.Capacity (Buffers (Other)) = Pages * 4096 and
             Frames.Writable_Address (Buffers (Other)) = System.Null_Address,
             "pending reader prevents backing reclamation");
      Frames.Allocate (Buffers (Other), 4096, OK);
      Check (not OK and Frames.Capacity (Buffers (Other)) = Pages * 4096,
             "quarantined frame cannot allocate over retained backing");
      Result := Send (Encode_Destroy ((Surface => Fixture.Surface)));
      Check (Decode_Status (Result, Destroy_Surface) = Success, "destroy protected publication surface");
      for I in Buffers'Range loop
         Frames.Release (Buffers (I), OK);
         Check (OK and Frames.Capacity (Buffers (I)) = 0, "frame backing reclaimed after grant retirement");
         Check (Frames.Writable_Address (Buffers (I)) = System.Null_Address, "released frame has no canvas");
         Frames.Allocate (Buffers (I), Pages * 4096, OK);
         Check (OK, "released handle can allocate again");
         if OK then
            declare
               First_Pixel : Unsigned_32 with Import, Volatile,
                 Address => Frames.Writable_Address (Buffers (I));
            begin
               Check (First_Pixel = 0, "reclaimed allocation is fresh zeroed backing");
            end;
         end if;
         Frames.Release (Buffers (I), OK);
         Check (OK, "unpublished backing reclaimed");
      end loop;
      debugPrint ("DESKTOP-PUBLICATION-DISPLAY-CHECK: complete" & ASCII.LF);
      debugPrint ("DESKTOP-CLIENT-FRAME-CHECK: complete" & ASCII.LF);
   end Check_Publication_Display;

   procedure Check_Publication_Grants is
      package Pub renames CuBit.Desktop_Protocol.Publication;
      use type Pub.Receipt;
      Fixture : constant Creation_Result := Decode_Creation_Result
        (Send (Encode_Create ((128, 96, Plain_Surface))));
      Raw : constant Unsigned_64 := syscall (SYSCALL_SBRK, 69_632);
      Grant, Short : MG.Grant_Reference;
      OK : Boolean;
      Config : Pub.Configuration_Result;
      Staged, Retired : Pub.Receipt;
      Request, Result : Wire_Message;
      Resized : Resize_Result;
      Previous : Unsigned_64 := 0;
   begin
      Check (Fixture.Status = Success and Raw /= Unsigned_64'Last,
             "publication fixture allocation");
      if Fixture.Status /= Success or Raw = Unsigned_64'Last then return; end if;
      declare
         Address : constant Integer_Address :=
           Integer_Address ((Raw + 4095) and not Unsigned_64'(4095));
      begin
         MG.Create_Via_Capability (CAP_SLOT_DESKTOP, To_Address (Address), 16, False, Grant, OK);
         Check (OK, "publication grant created");
         if not OK then return; end if;
         Config := Pub.Decode_Configuration
           (Send (Pub.Encode_Query ((Fixture.Surface, 0), False)));
         Check (Config.Status = Success, "publication configuration");
         if Config.Status /= Success then return; end if;
         MG.Create_Via_Capability (CAP_SLOT_DESKTOP, To_Address (Address), 1, False, Short, OK);
         Check (OK, "short publication grant created");
         if OK then
            Result := Send (Pub.Encode_Stage ((Fixture.Surface, Config.Value.Epoch, Short)));
            Check (Result = Pub.Encode_Receipt ((Status => Denied), Pub.Stage_Label),
                   "short publication grant denied");
            MG.Revoke (Short, OK);
            Check (OK and then MG.Retirement_Confirmed (Short), "failed stage retained no loan");
         end if;
         Request := Pub.Encode_Stage ((Fixture.Surface, Config.Value.Epoch, Grant));
         Request.Flags := 1;
         Check (Send (Request) = Pub.Encode_Receipt ((Status => Invalid_Request), Pub.Stage_Label),
                "malformed stage rejected");
         Check (Send (Pub.Encode_Stage ((1, Config.Value.Epoch, Grant))) =
                  Pub.Encode_Receipt ((Status => Denied), Pub.Stage_Label), "foreign stage denied");
         Check (Send (Pub.Encode_Query ((1, 1), True)) =
                  Pub.Encode_Receipt ((Status => Denied), Pub.Retirement_Label), "foreign retirement denied");
         for Attempt in 1 .. 140 loop
            Staged := Pub.Decode_Receipt
              (Send (Pub.Encode_Stage ((Fixture.Surface, Config.Value.Epoch, Grant))), Pub.Stage_Label);
            Check (Staged.Status = Success, "publication stage admitted");
            exit when Staged.Status /= Success;
            Check (Staged.Ticket > Previous and Staged.Epoch = Config.Value.Epoch,
                   "publication ticket never reused");
            Previous := Staged.Ticket;
            Result := Send (Pub.Encode_Stage ((Fixture.Surface, Config.Value.Epoch, Grant)));
            Check (Result = Pub.Encode_Receipt ((Status => Bad_State), Pub.Stage_Label),
                   "duplicate candidate denied");
            Retired := Pub.Decode_Receipt
              (Send (Pub.Encode_Query ((Fixture.Surface, Staged.Ticket), True)), Pub.Retirement_Label);
            Check (Retired.Status = Bad_State, "candidate is not retired");
            Result := Send (Encode_Attachment ((Fixture.Surface, Grant, Config.Value.Layout)));
            Check (Decode_Status (Result, Attach_Buffer) = Bad_State, "legacy attachment mixing denied");
            Result := Send (Encode_Present ((Fixture.Surface, (0, 0, 0, 0))));
            Check (Decode_Status (Result, Present_Surface) = Bad_State, "legacy present mixing denied");
            Resized := Decode_Resize_Result
              (Send (Encode_Resize ((Fixture.Surface, (if Attempt mod 2 = 1 then 160 else 128), 96))));
            Check (Resized.Status = Success, "staged fixture resized");
            Config := Pub.Decode_Configuration
              (Send (Pub.Encode_Query ((Fixture.Surface, 0), False)));
            Check (Config.Status = Success, "replacement configuration");
            exit when Config.Status /= Success;
            Retired := Pub.Decode_Receipt
              (Send (Pub.Encode_Query ((Fixture.Surface, Staged.Ticket), True)), Pub.Retirement_Label);
            Check (Retired = Staged, "exact stale candidate retirement receipt");
            Check (Pub.Decode_Receipt
                     (Send (Pub.Encode_Query ((Fixture.Surface, Staged.Ticket), True)), Pub.Retirement_Label) = Retired,
                   "retirement receipt repeatable");
            Check (Send (Pub.Encode_Stage ((Fixture.Surface, Staged.Epoch, Grant))) =
                     Pub.Encode_Receipt ((Status => Bad_State), Pub.Stage_Label), "stale stage rejected");
         end loop;
         if Config.Status = Success then
            Staged := Pub.Decode_Receipt
              (Send (Pub.Encode_Stage ((Fixture.Surface, Config.Value.Epoch, Grant))), Pub.Stage_Label);
            Check (Staged.Status = Success, "stage before destruction");
         end if;
         Result := Send (Encode_Destroy ((Surface => Fixture.Surface)));
         Check (Decode_Status (Result, Destroy_Surface) = Success, "destroy staged surface");
         MG.Revoke (Grant, OK);
         Check (OK and then MG.Retirement_Confirmed (Grant), "destroy returns candidate loan");
      end;
      debugPrint ("DESKTOP-PUBLICATION-GRANTS-CHECK: complete" & ASCII.LF);
   end Check_Publication_Grants;

   procedure Check_Configuration_Boundaries is
      package Pub renames CuBit.Desktop_Protocol.Publication;
      use type Pub.Configuration_Result;
      Fixture : constant Creation_Result := Decode_Creation_Result
        (Send (Encode_Create ((320, 200, Plain_Surface))));
      Before, After : Pub.Configuration_Result;
      Request, Result : Wire_Message;
      Resized : Resize_Result;
   begin
      Check (Fixture.Status = Success, "configuration fixture created");
      if Fixture.Status /= Success then return; end if;
      Request := Pub.Encode_Query ((Fixture.Surface, 0), Retirement => False);
      Before := Pub.Decode_Configuration (Send (Request));
      Check (Before.Status = Success, "owned surface configuration");
      if Before.Status = Success then
         Check (Before.Value.Width = 320 and Before.Value.Height = 200,
                "configuration logical extent");
         Check (Pub.Valid (Before.Value), "configuration physical density layout");
         Check (Pub.Decode_Configuration (Send (Request)) = Before,
                "unchanged query preserves generation");
         for Bad in 1 .. 5 loop
            Request := Pub.Encode_Query ((Fixture.Surface, 0), False);
            case Bad is
               when 1 => Request.Length := 1;
               when 2 => Request.Flags := 1;
               when 3 => Request.Reserved := 1;
               when 4 => Request.Words (1) := 1;
               when 5 => Request.Words (3) := 1;
            end case;
            Result := Send (Request);
            Check (Result = Pub.Encode_Configuration
                     ((Status => Invalid_Request)),
                   "malformed configuration query rejected");
         end loop;
         Request := Pub.Encode_Query ((Fixture.Surface, 0), False);
         Check (Pub.Decode_Configuration (Send (Request)) = Before,
                "malformed query preserves configuration");
         Resized := Decode_Resize_Result
           (Send (Encode_Resize ((Fixture.Surface, 352, 224))));
         Check (Resized.Status = Success, "configuration fixture resized");
         After := Pub.Decode_Configuration (Send (Request));
         Check (After.Status = Success, "resized configuration accepted");
         if After.Status = Success and then Resized.Status = Success then
            Check (After.Value.Width = Resized.Width and
                   After.Value.Height = Resized.Height and
                   After.Value.Epoch = Before.Value.Epoch + 1,
                   "resize advances configuration generation once");
            Check (Pub.Decode_Configuration (Send (Request)) = After,
                   "resized generation stable on repeat");
         end if;
      end if;
      Result := Send (Pub.Encode_Query ((1, 0), False));
      Check (Result = Pub.Encode_Configuration ((Status => Denied)),
             "foreign configuration denied");
      Result := Send (Pub.Encode_Query ((Live_Surface_Name'Last, 0), False));
      Check (Result = Pub.Encode_Configuration ((Status => Bad_Object)),
             "missing configuration surface");
      Result := Send (Encode_Destroy ((Surface => Fixture.Surface)));
      Check (Decode_Status (Result, Destroy_Surface) = Success,
             "configuration fixture destroyed");
      debugPrint ("DESKTOP-CONFIGURATION-CHECK: complete" & ASCII.LF);
   end Check_Configuration_Boundaries;
   procedure Check_Control_Boundaries (Owned : Live_Surface_Name) is
      Fixture : Creation_Result;
      Request, Result : Wire_Message;
      Controls : constant array (Positive range 1 .. 2) of Operation :=
        [Set_Pointer_Cursor, Destroy_Surface];
   begin
      for Op of Controls loop
         Request := (Code (Op), 4, 0, 0, [1, 0, 0, 0]);
         Result := Send (Request);
         Check (Decode_Status (Result, Op) = Denied,
                "foreign control denied");
         Request.Words (0) := Unsigned_64'Last;
         Result := Send (Request);
         Check (Decode_Status (Result, Op) = Bad_Object,
                "missing control target");
      end loop;
      for Style in Cursor_Style loop
         Result := Send (Encode_Cursor ((Owned, Style)));
         Check (Decode_Status (Result, Set_Pointer_Cursor) = Success,
                "owned cursor style accepted");
      end loop;
      for Malformation in 1 .. 6 loop
         Request := (Code (Set_Pointer_Cursor), 4, 0, 0,
                     [Unsigned_64 (Owned), 1, 0, 0]);
         case Malformation is
            when 1 => Request.Length := 1;
            when 2 => Request.Flags := 1;
            when 3 => Request.Reserved := 1;
            when 4 => Request.Words (2) := 1;
            when 5 => Request.Words (3) := 1;
            when 6 => Request.Words (1) := Unsigned_64'Last;
         end case;
         Result := Send (Request);
         Check (Result = Encode_Status (Set_Pointer_Cursor, Invalid_Request),
                "malformed cursor rejected");

         Fixture := Decode_Creation_Result
           (Send (Encode_Create ((320, 200, Plain_Surface))));
         Check (Fixture.Status = Success, "destroy boundary fixture");
         if Fixture.Status = Success then
            Request.Label := Code (Destroy_Surface);
            Request.Words (0) := Unsigned_64 (Fixture.Surface);
            Request.Words (1) := (if Malformation = 6 then 1 else 0);
            Result := Send (Request);
            Check (Result = Encode_Status (Destroy_Surface, Invalid_Request),
                   "malformed destroy rejected");
            Result := Send (Encode_Present ((Fixture.Surface, (0, 0, 0, 0))));
            Check (Result.Words (0) = 0, "malformed destroy preserves surface");
            Result := Send ((Code (Destroy_Surface), 4, 0, 0,
                             [Unsigned_64 (Fixture.Surface), 0, 0, 0]));
            Check (Decode_Status (Result, Destroy_Surface) = Success,
                   "valid destroy after rejection");
            Result := Send (Encode_Destroy ((Surface => Fixture.Surface)));
            Check (Decode_Status (Result, Destroy_Surface) = Bad_Object,
                   "double destroy rejected");
         end if;
      end loop;
      Result := Send (Encode_Cursor ((Owned, Default_Cursor)));
      Check (Decode_Status (Result, Set_Pointer_Cursor) = Success,
             "cursor restored after malformed requests");
   end Check_Control_Boundaries;
   procedure Check_Title_Boundaries (Owned : Live_Surface_Name) is
      Request, Result : Wire_Message;
   begin
      for Malformation in 1 .. 6 loop
         -- One byte, 'A', with otherwise zero padding.
         Request := (Code (Set_Window_Title), 4, 0, 0,
                     [Unsigned_64 (Owned), 65, 0, 2 ** 56]);
         case Malformation is
            when 1 => Request.Length := 3;
            when 2 => Request.Flags := 1;
            when 3 => Request.Reserved := 1;
            when 4 => Request.Words (3) := 24 * 2 ** 56;
            when 5 => Request.Words (1) := 65 + 66 * 256;
            when 6 => Request.Words (3) := 1 + 2 ** 56;
         end case;
         Result := Send (Request);
         Check (Result = Encode_Status (Set_Window_Title, Invalid_Request),
                "malformed title rejected");
      end loop;
      Request := (Code (Set_Window_Title), 4, 0, 0, [1, 0, 0, 0]);
      Check (Decode_Status (Send (Request), Set_Window_Title) = Denied,
             "foreign title denied");
      Request.Words (0) := Unsigned_64'Last;
      Check (Decode_Status (Send (Request), Set_Window_Title) = Bad_Object,
             "missing title target");
      for Size in Title_Length loop
         Result := Send (Encode_Title ((Owned, (Size, [others => 'A']))));
         Check (Result = Encode_Status (Set_Window_Title, Success),
                "valid title length accepted after malformed traffic");
      end loop;
      Result := Send (Encode_Title ((Owned, Make_Title (""))));
      Check (Result = Encode_Status (Set_Window_Title, Success),
             "empty title clears custom caption");
   end Check_Title_Boundaries;
   procedure Check_Session_Boundaries is
      Request, Result : Wire_Message;
      Fixture : Creation_Result;
   begin
      for Fault in 1 .. 4 loop
         Request := (Code (Hello), 4, 0, 0, [2 ** 32, 0, 0, 0]);
         case Fault is
            when 1 => Request.Length := 1;
            when 2 => Request.Flags := 1;
            when 3 => Request.Reserved := 1;
            when 4 => Request.Words (1) := 1;
         end case;
         Result := Send (Request);
         Check (Result = (Code (Hello), 2, 0, 0, [0, 4, 0, 0]),
                "malformed hello rejected");
         Request := (Code (Get_Information), Request.Length,
                     Request.Flags, Request.Reserved, [0, 0, 0, 0]);
         if Fault = 4 then Request.Words (0) := 1; end if;
         Result := Send (Request);
         Check (Result = (Code (Get_Information), 2, 0, 0, [0, 4, 0, 0]),
                "malformed information query rejected");

         Fixture := Decode_Creation_Result
           (Send (Encode_Create ((320, 200, Plain_Surface))));
         Check (Fixture.Status = Success, "goodbye boundary fixture");
         if Fixture.Status = Success then
            Request.Label := Code (Goodbye);
            Result := Send (Request);
            Check (Result = Encode_Status (Goodbye, Invalid_Request),
                   "malformed goodbye rejected");
            Result := Send (Encode_Present ((Fixture.Surface, (0, 0, 0, 0))));
            Check (Result.Words (0) = 0, "malformed goodbye preserves surface");
            Result := Send (Encode_Destroy ((Surface => Fixture.Surface)));
         end if;
      end loop;
      Result := Send ((Code (Hello), 4, 0, 0, [Unsigned_64'Last, 0, 0, 0]));
      Check (Result = (Code (Hello), 2, 0, 0, [0, 5, 0, 0]),
             "unsupported desktop version rejected");
      Check (Decode_Hello_Result (Send (Encode_Hello (Current_Revision))).Status = Success,
             "supported handshake after malformed traffic");
      Check (Decode_Information_Result (Send (Encode_Empty_Request (Get_Information))).Status = Success,
             "checked display information");
      Fixture := Decode_Creation_Result (Send (Encode_Create ((320, 200, Plain_Surface))));
      Check (Fixture.Status = Success, "session cleanup fixture");
      if Fixture.Status = Success then
         declare
            Other : constant Creation_Result :=
              Decode_Creation_Result (Send (Encode_Create ((320, 200, Plain_Surface))));
            Raw : constant Unsigned_64 := syscall (SYSCALL_SBRK, 8192);
            Grant : MG.Grant_Reference;
            Acquired : Boolean := False;
            Ok : Boolean;
         begin
            Check (Other.Status = Success, "second session surface");
            if Raw /= Unsigned_64'Last then
               declare
                  Address : constant Integer_Address :=
                    Integer_Address ((Raw + 4095) and not Unsigned_64'(4095));
                  Pixels : array (Natural range 0 .. 1023) of Unsigned_32
                    with Address => To_Address (Address), Volatile;
               begin
                  Pixels := [others => 16#FF80_8080#];
                  MG.Create_Via_Capability (CAP_SLOT_DESKTOP, To_Address (Address), 1, False, Grant, Ok);
                  Check (Ok, "session cleanup grant");
                  if Ok then
                     Result := Send (Encode_Attachment ((Fixture.Surface, Grant, (4, 2, 16))));
                     Acquired := Decode_Status (Result, Attach_Buffer) = Success;
                     Check (Acquired, "session cleanup acquisition");
                     MG.Revoke (Grant, Ok);
                     Check (Ok, "session cleanup pending revocation");
                     Request := Encode_Empty_Request (Goodbye); Request.Reserved := 1;
                     Check (Send (Request) = Encode_Status (Goodbye, Invalid_Request),
                            "invalid goodbye during pending revocation rejected");
                     Check (syscall (SYSCALL_GET_OWNED_SHARED_MEMORY_GRANT_GENERATION, Grant.slot) = Grant.generation,
                            "invalid goodbye retains acquisition");
                  end if;
               end;
            else
               Check (False, "session cleanup allocation");
            end if;
            Check (Send (Encode_Empty_Request (Goodbye)) = Encode_Status (Goodbye, Success),
                   "valid goodbye acknowledged");
            if Acquired then
               Check (syscall (SYSCALL_GET_OWNED_SHARED_MEMORY_GRANT_GENERATION, Grant.slot) = 0,
                      "goodbye releases pending acquisition");
            end if;
            Result := Send (Encode_Present ((Fixture.Surface, (0, 0, 0, 0))));
            Check (Decode_Status (Result, Present_Surface) = Bad_Object, "goodbye removes first owned surface");
            if Other.Status = Success then
               Result := Send (Encode_Present ((Other.Surface, (0, 0, 0, 0))));
               Check (Decode_Status (Result, Present_Surface) = Bad_Object, "goodbye removes second owned surface");
            end if;
            Result := Send (Encode_Present ((1, (0, 0, 0, 0))));
            Check (Decode_Status (Result, Present_Surface) = Denied, "goodbye preserves foreign surface");
            Check (Send (Encode_Empty_Request (Goodbye)) = Encode_Status (Goodbye, Success),
                   "repeated goodbye is harmless");
         end;
      end if;
   end Check_Session_Boundaries;
   procedure Check_Input_Overflow is
      Fixture, Other : Creation_Result;
      Event : Input_Result;
      Result : Wire_Message;
      Serial : Unsigned_64 := 0;

      function Poll (Target : Live_Surface_Name; After : Unsigned_64)
        return Input_Result is
        (Decode_Input_Result
           (Send (Encode_Input_Request ((Poll_Input, Target, After))), Poll_Input));
   begin
      Fixture := Decode_Creation_Result
        (Send (Encode_Create ((320, 200, Plain_Surface))));
      Other := Decode_Creation_Result
        (Send (Encode_Create ((320, 200, Plain_Surface))));
      Check (Fixture.Status = Success and Other.Status = Success,
             "input overflow fixtures created");
      if Fixture.Status = Success and Other.Status = Success then
         Event := Poll (Fixture.Surface, 0);
         Check (Event.Status = Success and then
                Event.Value.Kind = Surface_Configured,
                "input overflow initial configure");
         if Event.Status = Success then Serial := Event.Value.Serial; end if;

         -- The protocol fixture injects no device events. Plain surfaces
         -- start at (0,0); Desktop's boot cursor is (80,80), with no held
         -- buttons/modifiers. Recovery uses client-local coordinates: the
         -- current client inset (4,30) makes that position (76,50).
         for Index in 1 .. 32 loop
            Check (Decode_Resize_Result (Send (Encode_Resize
                     ((Fixture.Surface,
                       (if Index mod 2 = 1 then 321 else 320), 200)))).Status = Success,
                   "resize admitted up to input capacity");
         end loop;
         for Index in 1 .. 32 loop
            Event := Poll (Fixture.Surface, Serial);
            Check (Event.Status = Success and then
                   Event.Value.Kind = Surface_Configured and then
                   Event.Value.Serial = Serial + 1 and then
                   Event.Value.Payload0 = (if Index mod 2 = 1 then 321 else 320) and then
                   Event.Value.Payload1 = 200 and then
                   Event.Value.More_Pending = (Index < 32),
                   "full queue preserves every ordered configure and pending flag");
            if Event.Status = Success then Serial := Event.Value.Serial; end if;
         end loop;

         for Cycle in 1 .. 4 loop
            -- Synchronous resize acknowledgments establish that all 33
            -- events reached Desktop. The client deliberately does not poll
            -- this surface while filling its fixed 32-entry event queue.
            -- Alternating extents remain real configuration transitions.
            for Index in 1 .. 33 loop
               Check (Decode_Resize_Result (Send (Encode_Resize
                        ((Fixture.Surface,
                          (if Index mod 2 = 1 then 321 else 320), 200)))).Status = Success,
                      "resize admitted during input saturation");
            end loop;
            Event := Poll (Fixture.Surface, Serial);
            Check (Event.Status = Success and then
                   Event.Value.Kind = Input_Resynchronized and then
                   Event.Value.Serial = Serial + 33 and then
                   not Event.Value.More_Pending and then
                   Event.Value.Payload0 = (76 or Shift_Left (Unsigned_64'(50), 32)) and then
                   Event.Value.Payload1 = 0,
                   "queue overflow replaces old events with exact recovery");
            if Event.Status = Success then
               debugPrint ("TEST: input recovery cycle=" & Cycle'Image &
                 " kind=" & Event.Value.Kind'Image &
                 " serial=" & Event.Value.Serial'Image &
                 " x=" & Unsigned_64'(Event.Value.Payload0 and 16#FFFF_FFFF#)'Image &
                 " y=" & Unsigned_64'(Shift_Right (Event.Value.Payload0, 32))'Image &
                 " state=" & Event.Value.Payload1'Image & ASCII.LF);
            end if;
            if Event.Status = Success then Serial := Event.Value.Serial; end if;
            -- Repeating an old acknowledgment must not replay the consumed
            -- recovery event or expose any discarded configure event.
            Event := Poll (Fixture.Surface, 0);
            Check (Event.Status = Success and then Event.Value.Kind = No_Input,
                   "overflow recovery consumed once with no stale replay");

            Check (Decode_Resize_Result
                     (Send (Encode_Resize ((Fixture.Surface, 322, 201)))).Status = Success,
                   "fresh configure admitted after overflow");
            Event := Poll (Fixture.Surface, Serial);
            Check (Event.Status = Success and then
                   Event.Value.Kind = Surface_Configured and then
                   Event.Value.Serial = Serial + 1 and then
                   Event.Value.Payload0 = 322 and then Event.Value.Payload1 = 201 and then
                   not Event.Value.More_Pending,
                   "fresh input follows recovery with next serial and payload");
            if Event.Status = Success then Serial := Event.Value.Serial; end if;
         end loop;

         Event := Poll (Other.Surface, 0);
         Check (Event.Status = Success and then
                Event.Value.Kind = Surface_Configured and then
                Event.Value.Serial = 1 and then
                Event.Value.Payload0 = 320 and then Event.Value.Payload1 = 200 and then
                not Event.Value.More_Pending,
                "overflow preserves other surface queued configure");
      end if;
      if Fixture.Status = Success then
         Result := Send (Encode_Destroy ((Surface => Fixture.Surface)));
         Check (Decode_Status (Result, Destroy_Surface) = Success,
                "overflow fixture destroyed");
      end if;
      if Other.Status = Success then
         Result := Send (Encode_Destroy ((Surface => Other.Surface)));
         Check (Decode_Status (Result, Destroy_Surface) = Success,
                "isolated input fixture destroyed");
      end if;
      if Passed then
         debugPrint ("TEST: PASS desktop native input saturation and recovery" & ASCII.LF);
      end if;
   end Check_Input_Overflow;

   procedure Check_Input_Boundaries is
      Operations : constant array (Positive range 1 .. 2) of Operation := [Poll_Input, Wait_Input];
      Fixture : Creation_Result;
      Request, Result : Wire_Message;
   begin
      for Op of Operations loop
         for Fault in 1 .. 4 loop
            Fixture := Decode_Creation_Result (Send (Encode_Create ((320, 200, Plain_Surface))));
            Check (Fixture.Status = Success, "input validation fixture");
            if Fixture.Status = Success then
               Request := (Code (Op), 4, 0, 0,
                           [Unsigned_64 (Fixture.Surface), 0, (if Op = Wait_Input then 1 else 0), 0]);
               case Fault is
                  when 1 => Request.Length := 3;
                  when 2 => Request.Flags := 1;
                  when 3 => Request.Reserved := 1;
                  when 4 => Request.Words (3) := 1;
               end case;
               Check (Send (Request) = Encode_Status (Op, Invalid_Request), "malformed input rejected");
               Result := Send ((Code (Poll_Input), 4, 0, 0, [Unsigned_64 (Fixture.Surface), 0, 0, 0]));
               Check (Result.Length = 4 and then Result.Words (0) = 8,
                      "malformed input preserves queued configure");
               Result := Send (Encode_Destroy ((Surface => Fixture.Surface)));
            end if;
         end loop;
         Request := (Code (Op), 4, 0, 0, [1, 0, (if Op = Wait_Input then 1 else 0), 0]);
         Check (Send (Request) = Encode_Status (Op, Denied), "foreign input denied");
         Request.Words (0) := Unsigned_64'Last;
         Check (Send (Request) = Encode_Status (Op, Bad_Object), "missing input target");
      end loop;
      Fixture := Decode_Creation_Result
        (Send (Encode_Create ((320, 200, Plain_Surface))));
      Check (Fixture.Status = Success, "input waiter fixture");
      if Fixture.Status = Success then
         declare
            Event : Input_Result;
            Serial, Started, Deadline, Finished : Unsigned_64;
         begin
            Event := Decode_Input_Result
              (Send (Encode_Input_Request ((Poll_Input, Fixture.Surface, 0))), Poll_Input);
            Check (Event.Status = Success and then Event.Value.Kind = Surface_Configured,
                   "initial configure before deferred wait");
            if Event.Status = Success then
               Serial := Event.Value.Serial;
               --  Reuse the same channel: each timeout must consume its saved
               --  reply and leave the channel able to install another waiter.
               for Attempt in 1 .. 4 loop
                  Started := syscall (SYSCALL_GETTIME);
                  if Started > Unsigned_64'Last - 20 then
                     Check (False, "waiter monotonic clock available");
                     exit;
                  end if;
                  Deadline := Started + 20;
                  Event := Decode_Input_Result
                    (Send (Encode_Input_Request
                       ((Wait_Input, Fixture.Surface, Serial, Deadline))), Wait_Input);
                  Finished := syscall (SYSCALL_GETTIME);
                  Check (Event.Status = Success and then
                         Event.Value.Kind = No_Input and then
                         Event.Value.Serial = Serial,
                         "deferred wait expires without fabricated input");
                  Check (Finished >= Deadline and Finished /= Unsigned_64'Last,
                         "deferred wait does not return before deadline");
               end loop;
               Check (Decode_Resize_Result
                        (Send (Encode_Resize ((Fixture.Surface, 321, 201)))).Status = Success,
                      "configure after repeated timeout");
               Event := Decode_Input_Result
                 (Send (Encode_Input_Request
                    ((Wait_Input, Fixture.Surface, Serial, 0))), Wait_Input);
               Check (Event.Status = Success and then
                      Event.Value.Kind = Surface_Configured and then
                      Event.Value.Serial > Serial,
                      "queued input delivered after repeated waiter reuse");
            end if;
            Request := Encode_Input_Request ((Poll_Input, Fixture.Surface, 0));
            Request.Words (2) := 1;
            Check (Send (Request) = Encode_Status (Poll_Input, Invalid_Request),
                   "poll deadline rejected");
            Request.Words := [others => 0];
            Check (Send (Request) = Encode_Status (Poll_Input, Invalid_Request),
                   "zero input surface rejected");
            if Event.Status = Success then
               declare
                  Wait_Token : constant Unsigned_64 := 16#D350_0001#;
                  Bad_Token : constant Unsigned_64 := 16#D350_0002#;
                  Destroy_Token : constant Unsigned_64 := 16#D350_0003#;
                  Entries : CompletionRing := [others => NULL_COMPLETION];
                  Extra : CompletionEntry := NULL_COMPLETION;
                  Received : Unsigned_64;
                  Submitted : Boolean;
               begin
                  -- One async lane preserves submission order without a
                  -- sleep-based assumption that the waiter is installed.
                  Request := Encode_Input_Request
                    ((Wait_Input, Fixture.Surface, Event.Value.Serial, 0));
                  Submitted := capSubmit (CAP_SLOT_DESKTOP, From_Wire (Request), Wait_Token);
                  Request := Encode_Input_Request ((Poll_Input, Fixture.Surface, 0));
                  Request.Length := 3;
                  Submitted := Submitted and then
                    capSubmit (CAP_SLOT_DESKTOP, From_Wire (Request), Bad_Token);
                  Submitted := Submitted and then capSubmit
                    (CAP_SLOT_DESKTOP,
                     From_Wire (Encode_Destroy ((Surface => Fixture.Surface))), Destroy_Token);
                  Check (Submitted, "deferred destruction submitted");
                  if Submitted then
                     -- The headless runner supplies the outer watchdog.
                     Received := waitCompletion (Entries'Address, 3, 3);
                     Check (Received = 3, "all deferred destruction completions");
                     if Received = 3 then
                        for Item of Entries (0 .. 2) loop
                           Check (Item.status = COMPLETION_OK and Item.requestId /= 0,
                                  "deferred completion transport and identity");
                        end loop;
                        Check (Entries (0).token = Bad_Token and then
                               Decode_Status (To_Wire (Entries (0).msg), Poll_Input) = Invalid_Request,
                               "malformed concurrent request rejected independently");
                        Event := Decode_Input_Result (To_Wire (Entries (1).msg), Wait_Input);
                        Check (Entries (1).token = Wait_Token and then
                               Event.Status = Success and then Event.Value.Kind = Input_Resynchronized,
                               "destroy resolves original outstanding waiter");
                        Check (Entries (2).token = Destroy_Token and then
                               Decode_Status (To_Wire (Entries (2).msg), Destroy_Surface) = Success,
                               "destroy acknowledgement follows waiter resolution");
                        Check (Entries (0).requestId /= Entries (1).requestId and
                               Entries (1).requestId /= Entries (2).requestId and
                               Entries (0).requestId /= Entries (2).requestId,
                               "simultaneous requests have distinct identities");
                        Check (Poll_Completion (Extra'Address) = 0,
                               "no duplicate deferred completion");
                     end if;
                  else
                     Result := Send (Encode_Destroy ((Surface => Fixture.Surface)));
                  end if;

                  Fixture := Decode_Creation_Result
                    (Send (Encode_Create ((320, 200, Plain_Surface))));
                  Check (Fixture.Status = Success, "channel reuse after waiter destruction");
                  if Fixture.Status = Success then
                     Event := Decode_Input_Result
                       (Send (Encode_Input_Request ((Poll_Input, Fixture.Surface, 0))), Poll_Input);
                     if Event.Status = Success then
                        Started := syscall (SYSCALL_GETTIME);
                        Deadline := Started + 20;
                        Event := Decode_Input_Result
                          (Send (Encode_Input_Request
                             ((Wait_Input, Fixture.Surface, Event.Value.Serial, Deadline))), Wait_Input);
                        Check (Event.Status = Success and then Event.Value.Kind = No_Input and then
                               syscall (SYSCALL_GETTIME) >= Deadline,
                               "destroyed waiter slot accepts another deferred wait");
                     else
                        Check (False, "reused channel initial input");
                     end if;
                     Result := Send (Encode_Destroy ((Surface => Fixture.Surface)));
                     Check (Decode_Status (Result, Destroy_Surface) = Success,
                            "reused channel fixture released");
                  end if;
               end;
            else
               Result := Send (Encode_Destroy ((Surface => Fixture.Surface)));
            end if;
         end;
      end if;
   end Check_Input_Boundaries;
begin
   Wire := Encode_Create ((320, 200, Plain_Surface));
   Wire.Words (0) := Unsigned_64'Last;
   Response := Send (Wire);
   Check (Response.Words (0) = 0 and Decode_Creation_Result (Response).Status = Invalid_Request, "oversized width");
   Wire := Encode_Create ((320, 200, Plain_Surface)); Wire.Length := 3;
   Check (Decode_Creation_Result (Send (Wire)).Status = Invalid_Request, "short header");
   Wire := Encode_Create ((320, 200, Plain_Surface)); Wire.Reserved := 1;
   Check (Decode_Creation_Result (Send (Wire)).Status = Invalid_Request, "reserved header");
   Wire := Encode_Create ((320, 200, Plain_Surface)); Wire.Words (2) := Unsigned_64'Last;
   Check (Decode_Creation_Result (Send (Wire)).Status = Invalid_Request, "unknown kind");
   Created := Decode_Creation_Result (Send (Encode_Create ((320, 200, Plain_Surface))));
   Check (Created.Status = Success, "create after malformed traffic");
   if Created.Status /= Success then return; end if;
   Count := 1; Names (1) := Created.Surface;
   Check_Configuration_Boundaries;
   Check_Publication_Grants;
   Check_Publication_Protocol;
   Check_Publication_Display;
   declare
      Pair_OK : Boolean;
   begin
      Desktop_Frame_Pair (Pair_OK);
      Check (Pair_OK, "two-buffer owner");
      if Pair_OK then debugPrint ("DESKTOP-FRAME-PAIR-CHECK: PASS frames=12 resize=1" & ASCII.LF); end if;
   end;
   declare
      Text_OK : Boolean;
   begin
      Desktop_Density_Text (Text_OK);
      Check (Text_OK, "native fractional-density text pixels");
      if Text_OK then
         debugPrint ("DESKTOP-DENSITY-TEXT-CHECK: PASS scales=5 faces=2" & ASCII.LF);
      end if;
   end;
   Check_Control_Boundaries (Created.Surface);
   Check_Title_Boundaries (Created.Surface);
   Response := Send (Encode_Present ((Created.Surface, (0, 0, 10, 10))));
   Check (Response.Words (0) = 0, "own present");
   Wire := Encode_Present ((Created.Surface, (0, 0, 10, 10)));
   Wire.Words (1) := Unsigned_64'Last;
   Response := Send (Wire);
   Check (Response.Words (0) = Status_Code'Enum_Rep (Invalid_Request), "oversized damage");
   Response := Send (Encode_Present ((Created.Surface, (65_535, 65_535, 65_535, 65_535))));
   Check (Response.Words (0) = 0, "out-of-surface damage safely clipped");
   -- This test profile boots the internal desktop shell as surface 1 before
   -- this app. Its name is deliberately known; it confers no authority.
   Check (Created.Surface /= 1, "foreign surface fixture");
   Response := Send (Encode_Present ((1, (0, 0, 0, 0))));
   Check (Response.Words (0) = Status_Code'Enum_Rep (Denied), "foreign present denied");
   Response := Send (Encode_Present ((Live_Surface_Name'Last, (0, 0, 0, 0))));
   Check (Response.Words (0) = Status_Code'Enum_Rep (Bad_Object), "missing surface");
   declare
      Limits : Limits_Request :=
        (Created.Surface, (120, 80, 400, 300), [others => False]);
      Resized : Resize_Result;
      Applied : Limits_Result;
   begin
      Check (Decode_Resize_Result (Send (Encode_Resize ((1, 320, 200)))).Status = Denied,
             "foreign resize denied");
      Check (Decode_Resize_Result (Send (Encode_Resize ((Live_Surface_Name'Last, 320, 200)))).Status = Bad_Object,
             "missing resize surface");
      Wire := Encode_Limits (Limits); Wire.Words (0) := 1;
      Check (Decode_Limits_Result (Send (Wire)).Status = Denied, "foreign limits denied");
      Wire.Words (0) := Unsigned_64'Last;
      Check (Decode_Limits_Result (Send (Wire)).Status = Bad_Object, "missing limits surface");
      Applied := Decode_Limits_Result (Send (Encode_Limits (Limits)));
      Check (Applied.Status = Success and then Applied.Bounds = Limits.Bounds,
             "typed limits applied");
      for Field in 1 .. 2 loop
         Wire := Encode_Resize ((Created.Surface, 320, 200));
         Wire.Words (Field) := Unsigned_64'Last;
         Check (Decode_Resize_Result (Send (Wire)).Status = Invalid_Request,
                "oversized resize rejected");
         Wire := Encode_Limits (Limits); Wire.Words (Field) := Unsigned_64'Last;
         Check (Decode_Limits_Result (Send (Wire)).Status = Invalid_Request,
                "oversized limits rejected");
      end loop;
      Wire := Encode_Resize ((Created.Surface, 320, 200)); Wire.Length := 3;
      Check (Decode_Resize_Result (Send (Wire)).Status = Invalid_Request, "short resize rejected");
      Wire := Encode_Limits (Limits); Wire.Reserved := 1;
      Check (Decode_Limits_Result (Send (Wire)).Status = Invalid_Request, "reserved limits rejected");
      Wire := Encode_Limits (Limits); Wire.Words (3) := Feature_Bits ([others => True]) + 1;
      Check (Decode_Limits_Result (Send (Wire)).Status = Invalid_Request, "unknown features rejected");
      Wire := Encode_Limits (Limits); Wire.Words (2) := 119;
      Check (Decode_Limits_Result (Send (Wire)).Status = Invalid_Request, "contradictory limits rejected");
      Resized := Decode_Resize_Result (Send (Encode_Resize ((Created.Surface, 999, 999))));
      Check (Resized.Status = Success and then Resized.Width = 400 and then Resized.Height = 300,
             "rejected limits preserve prior bounds");
      Limits.Bounds := (120, 80, 0, 0);
      Check (Decode_Limits_Result (Send (Encode_Limits (Limits))).Status = Success,
             "unbounded maxima restored");
      Resized := Decode_Resize_Result (Send (Encode_Resize ((Created.Surface, 320, 200))));
      Check (Resized.Status = Success and then Resized.Width = 320 and then Resized.Height = 200,
             "resize after malformed traffic");
   end;
   declare
      Raw : constant Unsigned_64 := syscall (SYSCALL_SBRK, 8192);
      First, Second, Reused : MG.Grant_Reference;
      Ok : Boolean;
      Surface : constant Live_Surface_Name := Created.Surface;
   begin
      Check (Raw /= Unsigned_64'Last, "buffer allocation");
      if Raw /= Unsigned_64'Last then
         declare
            Address : constant Integer_Address :=
              Integer_Address ((Raw + 4095) and not Unsigned_64'(4095));
            Pixels : array (Natural range 0 .. 1023) of Unsigned_32
              with Address => To_Address (Address), Volatile;
         begin
            Pixels := [others => 16#FF80_4020#];
            MG.Create_Via_Capability
              (CAP_SLOT_DESKTOP, To_Address (Address), 1, False, First, Ok);
            Check (Ok, "create desktop grant");
            if Ok then
               -- More than the kernel's acquisition limit: each replacement
               -- must return exactly one old acquisition, even for one grant.
               for Attempt in 1 .. 140 loop
                  Response := Send (Encode_Attachment ((Surface, First, (4, 2, 16))));
                  Check (Response.Words (0) = 0, "balanced repeated attachment");
               end loop;
               Response := Send (Encode_Attachment ((Surface, First, (1024, 2, 4096))));
               Check (Response.Words (0) = Status_Code'Enum_Rep (Denied), "grant too short");
               Response := Send (Encode_Attachment ((1, First, (4, 2, 16))));
               Check (Response.Words (0) = Status_Code'Enum_Rep (Denied), "foreign attachment");
               Wire := Encode_Attachment ((Surface, First, (4, 2, 16)));
               Wire.Words (2) := 0;
               Response := Send (Wire);
               Check (Response.Words (0) = Status_Code'Enum_Rep (Invalid_Request), "zero generation");
               Wire := Encode_Attachment ((Surface, First, (4, 2, 16)));
               Wire.Words (3) := Unsigned_64'Last;
               Response := Send (Wire);
               Check (Response.Words (0) = Status_Code'Enum_Rep (Invalid_Request), "hostile buffer layout");
               MG.Revoke (First, Ok);
               Check (Ok, "revoke while attached");
               Response := Send (Encode_Attachment ((Surface, First, (4, 2, 16))));
               Check (Response.Words (0) = Status_Code'Enum_Rep (Denied), "pending revoke blocks new attachment");
               Check (syscall (SYSCALL_GET_OWNED_SHARED_MEMORY_GRANT_GENERATION,
                               First.slot) = First.generation, "old attachment retained on rejection");
               Response := Send (Encode_Present ((Surface, (0, 0, 0, 0))));
               Check (Response.Words (0) = 0, "present during pending revoke");
               MG.Create_Via_Capability
                 (CAP_SLOT_DESKTOP, To_Address (Address), 1, False, Second, Ok);
               Check (Ok, "replacement grant creation");
               if Ok then
                  Response := Send (Encode_Attachment ((Surface, Second, (4, 2, 16))));
                  Check (Response.Words (0) = 0, "replacement acquisition");
                  Check (syscall (SYSCALL_GET_OWNED_SHARED_MEMORY_GRANT_GENERATION,
                                  First.slot) = 0, "replacement completes old revoke");
                  MG.Create_Via_Capability
                    (CAP_SLOT_DESKTOP, To_Address (Address), 1, False, Reused, Ok);
                  --  One global grant table: the slot may go to another
                  --  process first; a reused slot carries a new generation.
                  Check (Ok and then Reused /= First, "replacement is a new reference");
                  Response := Send (Encode_Attachment ((Surface, First, (4, 2, 16))));
                  Check (Response.Words (0) = Status_Code'Enum_Rep (Denied), "stale attachment denied");
                  if Ok then MG.Revoke (Reused, Ok); end if;
                  MG.Revoke (Second, Ok);
                  Check (Ok, "second pending revoke");
                  Wire := Encode_Destroy ((Surface => Surface));
                  Wire.Reserved := 1;
                  Response := Send (Wire);
                  Check (Response = Encode_Status (Destroy_Surface, Invalid_Request),
                         "malformed destroy during pending revoke rejected");
                  Check (syscall (SYSCALL_GET_OWNED_SHARED_MEMORY_GRANT_GENERATION,
                                  Second.slot) = Second.generation,
                         "malformed destroy retains buffer acquisition");
                  Held := Second;
                  Has_Held := True;
               end if;
            end if;
         end;
      end if;
   end;
   for Attempt in 2 .. Names'Last loop
      Response := Send (Encode_Create ((320, 200, Plain_Surface)));
      Created := Decode_Creation_Result (Response);
      if Created.Status /= Success then
         Check (Created.Status = Resources_Exhausted and Response.Words (0) = 0, "unambiguous exhaustion");
         Exhausted := True;
         exit;
      end if;
      Count := Count + 1;
      Names (Count) := Created.Surface;
   end loop;
   Check (Exhausted, "bounded surface table");
   for Index in 1 .. Count loop
      Response := Send (Encode_Destroy ((Surface => Live_Surface_Name (Names (Index)))));
      Check (Decode_Status (Response, Destroy_Surface) = Success, "release owned surface");
   end loop;
   if Has_Held then
      Check (syscall (SYSCALL_GET_OWNED_SHARED_MEMORY_GRANT_GENERATION,
                      Held.slot) = 0, "destroy returns held acquisition");
   end if;
   Check_Input_Boundaries;
   declare
      Batch_OK : Boolean;
   begin
      Desktop_Input_Batch (Batch_OK);
      Check (Batch_OK, "native input batch transport");
   end;
   Check_Input_Overflow;
   Check_Session_Boundaries;
   Created := Decode_Creation_Result (Send (Encode_Create ((320, 200, Plain_Surface))));
   Check (Created.Status = Success, "service usable after exhaustion");
   if Created.Status = Success then
      declare
         Raw : constant Unsigned_64 := syscall (SYSCALL_SBRK, 8192);
         Grant : MG.Grant_Reference;
         Ok : Boolean;
      begin
         Check (Raw /= Unsigned_64'Last, "exit fixture allocation");
         if Raw /= Unsigned_64'Last then
            declare
               Address : constant Integer_Address :=
                 Integer_Address ((Raw + 4095) and not Unsigned_64'(4095));
               Pixels : array (Natural range 0 .. 1023) of Unsigned_32
                 with Address => To_Address (Address), Volatile;
            begin
               Pixels := [others => 16#FF40_8040#];
               MG.Create_Via_Capability
                 (CAP_SLOT_DESKTOP, To_Address (Address), 1, False, Grant, Ok);
               Check (Ok, "exit fixture grant");
               if Ok then
                  Response := Send (Encode_Attachment
                    ((Created.Surface, Grant, (4, 2, 16))));
                  Check (Response.Words (0) = 0, "exit fixture attachment");
               end if;
            end;
         end if;
      end;
   end if;
   -- Deliberately exit with the final buffer acquired. The headless driver
   -- requests a repaint after exit and requires the desktop's reap marker.
   if Passed then debugPrint ("DESKTOP-PROTOCOL-CHECK: PASS" & ASCII.LF); end if;
end Main;
