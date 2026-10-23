------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  virtio-input driver for absolute pointers (QEMU virtio-tablet-pci and
--  other virtio tablets). Publishes ABSOLUTE_POINTER source reports
--  (CuBit.Input, Absolute_Pointer wire layout) to the registered mouse
--  consumer, normally desktop.svc.
--
--  Startup: devmgr maps the modern register BAR, programs MSI-X entry 0 for
--  the vector in the manifest's device slot, allocates one DMA page and
--  sends Configure; the reply tells devmgr the queue vector is set so it may
--  unmask the entry and the function.
--
--  Event path: the device writes struct virtio_input_event into buffers
--  posted on the event queue, one event per buffer, a report per
--  SYN_REPORT. Each interrupt drains every used buffer, assembles frames
--  (Virtio_Input_Events), re-posts the buffers at once (QEMU discards a
--  whole report when the queue lacks room for it) and appends frames to
--  Absolute_Pending: the newest position replaces an unpublished one,
--  button transitions are never merged or lost.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with System.Machine_Code;

with CuBit.Messages; use CuBit.Messages;
with CuBit.Launch_Configurations;
with Launch_Configuration;
with CuBit.Input; use CuBit.Input;
with CuBit.Virtio_Input_Control;
with CuBit.Device_Interrupts;
with Startup;
with Input_Pending;
with Absolute_Pointer;
with Absolute_Pending;
with Virtio_Input_Events;
with Virtio_Input_Transport;

procedure main is
   use ASCII;
   package VT renames Virtio_Input_Transport;
   package VE renames Virtio_Input_Events;
   package AP renames Absolute_Pointer;
   use type VE.Outcome;
   use type Absolute_Pending.Append_Outcome;

   REPLY_OK  : constant Unsigned_32 := 16#F000#;
   REPLY_ERR : constant Unsigned_32 := 16#F001#;
   OP_READY  : constant Unsigned_32 := 16#FF00#;

   DMA_BASE : constant System.Address := To_Address (16#0000_7000_0000_0000#);

   --  Event queue (virtio-input queue 0). The status queue (1) carries LED
   --  state to keyboards; a pointer leaves it disabled.
   EVENT_QUEUE : constant Unsigned_16 := 0;
   --  The table entry every interrupt of this function uses.
   MSIX_ENTRY : constant Unsigned_16 := 0;
   QUEUE_CAPACITY : constant := 64;
   MINIMUM_QUEUE  : constant := 8;
   subtype Ring_Index is Natural range 0 .. QUEUE_CAPACITY - 1;

   --  One DMA page: descriptors, driver (avail) ring, device (used) ring
   --  and one event buffer per descriptor.
   DESC_OFF   : constant Storage_Offset := 16#000#;
   AVAIL_OFF  : constant Storage_Offset := 16#400#;
   USED_OFF   : constant Storage_Offset := 16#500#;
   EVENTS_OFF : constant Storage_Offset := 16#800#;
   PAGE_BYTES : constant := 4096;
   pragma Compile_Time_Error
     (EVENTS_OFF + QUEUE_CAPACITY * VE.Event_Bytes > PAGE_BYTES * CuBit.Virtio_Input_Control.DMA_Pages,
      "virtio-input rings and buffers exceed their DMA page");

   VRING_DESC_F_WRITE : constant Unsigned_16 := 2;

   type Vring_Desc is record
      Addr  : Unsigned_64;
      Len   : Unsigned_32;
      Flags : Unsigned_16;
      Next  : Unsigned_16;
   end record with Size => 128;
   for Vring_Desc use record
      Addr  at 0  range 0 .. 63;
      Len   at 8  range 0 .. 31;
      Flags at 12 range 0 .. 15;
      Next  at 14 range 0 .. 15;
   end record;
   type Desc_Array is array (Ring_Index) of Vring_Desc;
   type Avail_Array is array (Ring_Index) of Unsigned_16;
   type Vring_Avail is record
      Flags : Unsigned_16;
      Idx   : Unsigned_16;
      Ring  : Avail_Array;
   end record with Volatile;
   for Vring_Avail use record
      Flags at 0 range 0 .. 15;
      Idx   at 2 range 0 .. 15;
      Ring  at 4 range 0 .. QUEUE_CAPACITY * 16 - 1;
   end record;
   type Used_Elem is record
      Id  : Unsigned_32;
      Len : Unsigned_32;
   end record with Size => 64;
   for Used_Elem use record
      Id  at 0 range 0 .. 31;
      Len at 4 range 0 .. 31;
   end record;
   type Used_Array is array (Ring_Index) of Used_Elem;
   type Vring_Used is record
      Flags : Unsigned_16;
      Idx   : Unsigned_16;
      Ring  : Used_Array;
   end record with Volatile;
   for Vring_Used use record
      Flags at 0 range 0 .. 15;
      Idx   at 2 range 0 .. 15;
      Ring  at 4 range 0 .. QUEUE_CAPACITY * 64 - 1;
   end record;
   type Wire_Event is record
      Kind  : Unsigned_16;
      Code  : Unsigned_16;
      Value : Unsigned_32;
   end record with Size => VE.Event_Bytes * 8;
   for Wire_Event use record
      Kind  at 0 range 0 .. 15;
      Code  at 2 range 0 .. 15;
      Value at 4 range 0 .. 31;
   end record;
   type Event_Array is array (Ring_Index) of Wire_Event;

   Descs  : Desc_Array with Import, Volatile, Address => DMA_BASE + DESC_OFF;
   Avail  : Vring_Avail with Import, Address => DMA_BASE + AVAIL_OFF;
   Used   : Vring_Used with Import, Address => DMA_BASE + USED_OFF;
   Events : Event_Array with Import, Volatile, Address => DMA_BASE + EVENTS_OFF;

   Queue_Size : Unsigned_16 := QUEUE_CAPACITY;
   Avail_Idx  : Unsigned_16 := 0;
   Used_Seen  : Unsigned_16 := 0;
   DMA_Phys   : Unsigned_64 := 0;

   --  virtio-input configuration selectors (virtio 1.2 5.8.4).
   CFG_ID_NAME  : constant Unsigned_8 := 16#01#;
   CFG_EV_BITS  : constant Unsigned_8 := 16#11#;
   CFG_ABS_INFO : constant Unsigned_8 := 16#12#;
   ABS_INFO_MIN : constant := 0;
   ABS_INFO_MAX : constant := 4;
   ABS_INFO_BYTES : constant := 20;

   Assembler : VE.Assembler;
   Pending   : Absolute_Pending.State;
   Consumer  : Process_ID := No_Process;
   Announced : Boolean := False;
   --  The device broke the used-ring contract: stop touching the rings.
   Faulted   : Boolean := False;
   --  Publication accounting: replaced (latest position wins), explicit
   --  overflows, kernel credit refusals (kept and retried), device drops.
   Replaced, Overflows, Busy, Dropped : Unsigned_64 := 0;

   procedure Barrier is
   begin
      System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
   end Barrier;

   procedure printDec (Value : Unsigned_64) is
      Text : constant String := Value'Image;
   begin
      debugPrint (Text (Text'First + 1 .. Text'Last));
   end printDec;

   procedure Post (Id : Ring_Index) is
   begin
      Avail.Ring (Natural (Avail_Idx mod Queue_Size)) := Unsigned_16 (Id);
      Avail_Idx := Avail_Idx + 1;
   end Post;

   procedure Refresh_Consumer is
      Previous : constant Process_ID := Consumer;
   begin
      Consumer := Registered_Driver (DRIVER_MOUSE);
      if Previous /= Consumer then
         --  Never deliver the old consumer's backlog. Every absolute report
         --  carries the full pointer state, so the next one needs no
         --  separate snapshot; it is flagged for recovery.
         Absolute_Pending.Reset (Pending);
      end if;
   end Refresh_Consumer;

   procedure Flush is
      Item : Input_Pending.Item;
   begin
      for Attempt in 1 .. Input_Pending.Capacity loop
         exit when Consumer = No_Process or else Absolute_Pending.Count (Pending) = 0;
         Item := Absolute_Pending.Element (Pending, 0);
         if not trySendEvent (Consumer, Encode
           (Source_Report'(sourceIdentity => 0, sequence => Item.Sequence,
             generation => 1, device => CuBit.Input.ABSOLUTE_POINTER,
             delivery => REPLACEABLE_STATE,
             flags => [RESYNCHRONIZE => Item.Recover],
             payload => Item.Payload,
             snapshot => Pointer_Snapshot
               (Item.Payload and AP.Byte_Mask, Item.Observed_Ms))), Startup.Pointer_Events)
         then
            Busy := Busy + 1;
            exit;
         end if;
         Absolute_Pending.Acknowledge (Pending);
         if not Announced then
            Announced := True;
            debugPrint ("virtio-input: absolute pointer reports published" & LF);
         end if;
      end loop;
   end Flush;

   procedure Publish (R : AP.Report) is
      Outcome : Absolute_Pending.Append_Outcome;
   begin
      if Consumer = No_Process then
         return;
      end if;
      Absolute_Pending.Append (Pending, R, syscall (SYSCALL_GETTIME), Outcome);
      case Outcome is
         when Absolute_Pending.Appended => null;
         when Absolute_Pending.Replaced => Replaced := Replaced + 1;
         when Absolute_Pending.Overflowed =>
            Overflows := Overflows + 1;
            debugPrint ("virtio-input: pointer retention overflow; resynchronizing" & LF);
      end case;
   end Publish;

   --  Drain every used buffer, re-post it, and publish completed frames.
   procedure Drain (Progress : out Boolean) is
      Seen : Unsigned_16;
      Elem : Used_Elem;
      Id   : Ring_Index;
      Wire : Wire_Event;
      Result : VE.Outcome;
      Report : AP.Report;
   begin
      Progress := False;
      if Faulted then
         return;
      end if;
      Seen := Used.Idx;
      Barrier;
      if Seen - Used_Seen > Queue_Size then
         --  A device writing more than the ring holds is not trusted further.
         debugPrint ("virtio-input: invalid used index; stopping" & LF);
         VT.Fail;
         Faulted := True;
         return;
      end if;
      while Used_Seen /= Seen loop
         Elem := Used.Ring (Natural (Used_Seen mod Queue_Size));
         Used_Seen := Used_Seen + 1;
         if Elem.Id < Unsigned_32 (Queue_Size) then
            Id := Ring_Index (Elem.Id);
            if Elem.Len >= VE.Event_Bytes then
               Wire := Events (Id);
               VE.Feed (Assembler, (Wire.Kind, Wire.Code, Wire.Value), Result, Report);
               if Result = VE.Frame_Report then
                  Publish (Report);
               elsif Result = VE.Frame_Dropped then
                  --  The device lost events: published reports stay, and the
                  --  next one is explicitly flagged for recovery.
                  Dropped := Dropped + 1;
                  Absolute_Pending.Mark_Lost (Pending);
               end if;
            end if;
            Post (Id);
         end if;
         Progress := True;
      end loop;
      if Progress then
         Barrier;
         Avail.Idx := Avail_Idx;
         Barrier;
         VT.Notify (EVENT_QUEUE);
      end if;
   end Drain;

   function Read_Range (Axis : Unsigned_16; Found : out Boolean) return AP.Device_Range is
      Size : Unsigned_8;
      Min, Max : AP.Device_Value;
   begin
      VT.Select_Config (CFG_ABS_INFO, Unsigned_8 (Axis), Size);
      Found := Size >= ABS_INFO_BYTES / 2;
      if not Found then
         return (0, 0);
      end if;
      Min := VE.Signed (VT.Config_Word (ABS_INFO_MIN));
      Max := VE.Signed (VT.Config_Word (ABS_INFO_MAX));
      Found := Min < Max;
      return (Min, Max);
   end Read_Range;

   procedure Print_Name is
      Size : Unsigned_8;
      Name : String (1 .. 64) := [others => ' '];
      Length : Natural := 0;
      Byte : Unsigned_8;
   begin
      VT.Select_Config (CFG_ID_NAME, 0, Size);
      for I in 0 .. Natural'Min (Natural (Size), Name'Length) - 1 loop
         Byte := VT.Config_Byte (I);
         exit when Byte = 0;
         Length := Length + 1;
         Name (Length) := (if Byte in 16#20# .. 16#7E# then Character'Val (Byte) else '?');
      end loop;
      debugPrint ("virtio-input: device """ & Name (1 .. Length) & """" & LF);
   end Print_Name;

   Config_Sender : Process_ID;
   Config_Message : Message;
   Ignore : Unsigned_64;
   Ignore_Tag : MessageTag;

   procedure Reject (Why : String) is
   begin
      debugPrint ("virtio-input: " & Why & LF);
      VT.Reset;
      Ignore := reply (Config_Sender,
        (tag => (label => REPLY_ERR, length => 0, protocol => 0, ipc => <>, reserved => 0),
         authorityTag => 0, words => [others => 0]));
   end Reject;
begin
   debugPrint ("virtio-input: starting" & LF);

   --  Startup is bound to the registered device manager's kernel-supplied
   --  sender identity.
   receive (Config_Sender, Config_Message);
   declare
      use CuBit.Virtio_Input_Control;
      Valid : Boolean := Config_Sender /= No_Process and then
        Config_Sender = Registered_Driver (DRIVER_DEVMGR) and then
        Config_Message.tag.label = Operation'Enum_Rep (Configure) and then
        Config_Message.tag.length = 3 and then
        High (Config_Message.words (2)) = 0;
   begin
      if Valid then
         VT.Bind
           (Common     => Low (Config_Message.words (0)),
            Device     => High (Config_Message.words (0)),
            Notify     => Low (Config_Message.words (1)),
            Multiplier => High (Config_Message.words (1)),
            Mapped     => Low (Config_Message.words (2)),
            OK         => Valid);
      end if;
      --  devmgr programmed MSI-X entry MSIX_ENTRY for the vector in the
      --  manifest's device slot (docs/msi-interrupts.md); without it there
      --  is no interrupt path.
      if not Valid or else
        not CuBit.Device_Interrupts.Block_Of (Startup.Device).Valid
      then
         Ignore := reply (Config_Sender,
           (tag => (label => REPLY_ERR, length => 0, protocol => 0, ipc => <>, reserved => 0),
            authorityTag => 0, words => [others => 0]));
         return;
      end if;
   end;

   --  The DMA page's device address, from devmgr's launch configuration
   --  (devmgr allocated it at DMA_BASE).
   declare
      Configuration : Launch_Configuration.Queue_Configuration;
      Configured : Boolean;
   begin
      CuBit.Launch_Configurations.Get
        (Launch_Configuration.Virtio_Input, Configuration, Configured);
      if not Configured or else Configuration.DMA = 0 then
         Reject ("no DMA region configured");
         return;
      end if;
      DMA_Phys := Configuration.DMA;
   end;

   declare
      OK : Boolean;
      Event_Bits_Size : Unsigned_8;
      X_Range, Y_Range : AP.Device_Range;
      X_Found, Y_Found : Boolean;
      ABS_BITS_XY : constant Unsigned_8 := 16#03#;   --  ABS_X and ABS_Y
      EV_ABS_SUBSELECT : constant Unsigned_8 := Unsigned_8 (VE.EV_ABS);
   begin
      VT.Negotiate (OK);
      if not OK then
         Reject ("feature negotiation failed");
         return;
      end if;
      Print_Name;
      VT.Select_Config (CFG_EV_BITS, EV_ABS_SUBSELECT, Event_Bits_Size);
      if Event_Bits_Size = 0 or else (VT.Config_Byte (0) and ABS_BITS_XY) /= ABS_BITS_XY then
         --  A keyboard or relative mouse: not an absolute pointer.
         VT.Fail;
         Reject ("not an absolute pointer (no ABS_X/ABS_Y); declined");
         return;
      end if;
      X_Range := Read_Range (VE.ABS_X, X_Found);
      Y_Range := Read_Range (VE.ABS_Y, Y_Found);
      if not X_Found or else not Y_Found then
         VT.Fail;
         Reject ("absolute axes without a valid range");
         return;
      end if;
      Assembler := VE.Start (X_Range, Y_Range);
      debugPrint ("virtio-input: absolute x ");
      printDec (Unsigned_64 (X_Range.Minimum + 2 ** 31));
      debugPrint ("..");
      printDec (Unsigned_64 (X_Range.Maximum + 2 ** 31));
      debugPrint (" y ");
      printDec (Unsigned_64 (Y_Range.Minimum + 2 ** 31));
      debugPrint ("..");
      printDec (Unsigned_64 (Y_Range.Maximum + 2 ** 31));
      debugPrint (" (biased by 2^31)" & LF);

      --  The largest power of two the device holds, up to our capacity.
      declare
         Limit : constant Unsigned_16 := VT.Queue_Limit (EVENT_QUEUE);
      begin
         Queue_Size := QUEUE_CAPACITY;
         while Queue_Size > Limit and then Queue_Size > MINIMUM_QUEUE loop
            Queue_Size := Queue_Size / 2;
         end loop;
         if Queue_Size > Limit then
            Reject ("event queue too small");
            return;
         end if;
      end;

      declare
         Zero : array (0 .. PAGE_BYTES - 1) of Unsigned_8 with Import, Address => DMA_BASE;
      begin
         for B of Zero loop B := 0; end loop;
      end;
      for Id in 0 .. Natural (Queue_Size) - 1 loop
         Descs (Id) :=
           (Addr => DMA_Phys + Unsigned_64 (EVENTS_OFF) + Unsigned_64 (Id * VE.Event_Bytes),
            Len => VE.Event_Bytes, Flags => VRING_DESC_F_WRITE, Next => 0);
         Post (Id);
      end loop;
      Avail.Flags := 0;
      Barrier;
      Avail.Idx := Avail_Idx;

      VT.Setup_Queue
        (EVENT_QUEUE, Queue_Size,
         Desc   => DMA_Phys + Unsigned_64 (DESC_OFF),
         Driver => DMA_Phys + Unsigned_64 (AVAIL_OFF),
         Device => DMA_Phys + Unsigned_64 (USED_OFF),
         Vector => MSIX_ENTRY, OK => OK);
      if OK then
         --  Configuration changes need no interrupt for a pointer.
         VT.Set_Config_Vector (VT.NO_VECTOR, OK);
      end if;
      if not OK then
         Reject ("event queue setup failed");
         return;
      end if;
      VT.Start;
      VT.Notify (EVENT_QUEUE);
   end;

   Ignore := reply (Config_Sender,
     (tag => (label => REPLY_OK, length => 0, protocol => 0, ipc => <>, reserved => 0),
      authorityTag => 0, words => [others => 0]));
   debugPrint ("virtio-input: tablet ready; queue ");
   printDec (Unsigned_64 (Queue_Size));
   debugPrint (" vector ");
   printDec (Unsigned_64
     (CuBit.Device_Interrupts.Block_Of (Startup.Device).Head));
   debugPrint (LF & "");

   Ignore_Tag := capSend (Startup.Ready,
     (tag => (label => OP_READY, length => 0, protocol => 0, ipc => <>, reserved => 0),
      authorityTag => 0, words => [others => 0]), CuBit.Messages.Wait_Forever);

   loop
      declare
         Event : Message;
         Progress : Boolean;
         Deadline : Unsigned_64 := Unsigned_64'Last;
      begin
         --  Interrupt notifications carry no payload this driver needs:
         --  the used ring is the state. Consume them, then drain.
         while Poll_Event (Event) loop
            null;
         end loop;
         Refresh_Consumer;
         Drain (Progress);
         Flush;
         if Absolute_Pending.Count (Pending) > 0 then
            --  Back-pressure from the consumer: retry soon; an interrupt
            --  still wakes earlier.
            Deadline := Absolute_Pending.Wake_Deadline
              (Pending, syscall (SYSCALL_GETTIME), Deadline);
         end if;
         if not Progress then
            if Wait_For_Activity_Until (Deadline) = Unavailable then
               Ignore := syscall (SYSCALL_SLEEP, 1);
            end if;
         end if;
      end;
   end loop;
end main;
