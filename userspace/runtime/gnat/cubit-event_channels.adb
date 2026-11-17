------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with System.Machine_Code;
with System.Storage_Elements; use System.Storage_Elements;

package body CuBit.Event_Channels is

   use type System.Address;

   --  Compiler ordering only (x86 keeps stores ordered).
   procedure Barrier;
   procedure Barrier is
   begin
      System.Machine_Code.Asm ("", Volatile => True, Clobber => "memory");
   end Barrier;

   --  A store must be visible before a later load of the peer's word (the
   --  arm-then-check and publish-then-check handshakes).
   procedure Full_Fence;
   procedure Full_Fence is
   begin
      System.Machine_Code.Asm ("mfence", Volatile => True, Clobber => "memory");
   end Full_Fence;

   function At_Offset (Base : System.Address; Offset : Natural) return System.Address is
     (Base + Storage_Offset (Offset));

   function Load (Base : System.Address; Offset : Natural) return Unsigned_32;
   function Load (Base : System.Address; Offset : Natural) return Unsigned_32 is
      --  SEC-025: single-read the peer may write it; read once into a private copy.
      Word : constant Unsigned_32 with Import, Volatile, Address => At_Offset (Base, Offset);
   begin
      return Word;
   end Load;

   procedure Store (Base : System.Address; Offset : Natural; Value : Unsigned_32);
   procedure Store (Base : System.Address; Offset : Natural; Value : Unsigned_32) is
      --  SEC-025: own this side's region; only this side writes it.
      Word : Unsigned_32 with Import, Volatile, Address => At_Offset (Base, Offset);
   begin
      Word := Value;
   end Store;

   --  A fresh arming value: changes on every arming, never zero.
   function Next_Arming (Value : Unsigned_32) return Unsigned_32 is
     (if Value = Unsigned_32'Last then 1 else Value + 1);

   --  Ring the peer once per arming: Armed is its waiting word now.
   procedure Ring_Once (Armed : Unsigned_32; Last : in out Unsigned_32; Ring : out Boolean);
   procedure Ring_Once (Armed : Unsigned_32; Last : in out Unsigned_32; Ring : out Boolean) is
   begin
      Ring := Armed /= 0 and then Armed /= Last;
      if Ring then
         Last := Armed;
      end if;
   end Ring_Once;

   ---------------------------------------------------------------------------
   --  Producer
   ---------------------------------------------------------------------------
   procedure Attach_Producer (P : in out Producer; Own_Region, Consumer_Region : System.Address) is
   begin
      P.Own := Own_Region;
      P.Peer := Consumer_Region;
      P.Ring := Rings.New_Producer;
      P.Rung_Arming := 0;
      P.Room_Arming := 0;
      P.Lost := 0;
      if Own_Region /= System.Null_Address then
         Store (P.Own, Produced_At, 0);
         Store (P.Own, Room_Waiting_At, 0);
         Store (P.Own, Overflows_At, 0);
      end if;
   end Attach_Producer;

   procedure Detach_Producer (P : in out Producer) is
   begin
      P.Own := System.Null_Address;
      P.Peer := System.Null_Address;
   end Detach_Producer;

   function Producer_Attached (P : Producer) return Boolean is
     (P.Own /= System.Null_Address and then P.Peer /= System.Null_Address);

   function Can_Push (P : in out Producer) return Boolean is
      Accepted : Boolean;
   begin
      if not Producer_Attached (P) then
         return False;
      end if;
      Rings.Accept_Consumed (P.Ring, Rings.Index (Load (P.Peer, Consumed_At)), Accepted);
      return Rings.Space (P.Ring) > 0;
   end Can_Push;

   function Unconsumed (P : Producer) return Natural is (P.Ring.Fill);

   procedure Push (P : in out Producer; Item : Element; Pushed, Ring_Peer : out Boolean) is
   begin
      Pushed := False;
      Ring_Peer := False;
      if not Can_Push (P) then
         return;
      end if;
      declare
         --  SEC-025: own this side's ring; only this side writes it.
         Slots : Rings.Ring with Import, Address => At_Offset (P.Own, Ring_At);
      begin
         Rings.Push (P.Ring, Slots, Item);
      end;
      Barrier;
      Store (P.Own, Produced_At, Unsigned_32 (P.Ring.Produced));
      Full_Fence;
      Pushed := True;
      Ring_Once (Load (P.Peer, Consumer_Waiting_At), P.Rung_Arming, Ring_Peer);
   end Push;

   procedure Mark_Overflow (P : in out Producer) is
   begin
      P.Lost := (if P.Lost = Unsigned_32'Last then P.Lost else P.Lost + 1);
      if P.Own /= System.Null_Address then
         Store (P.Own, Overflows_At, P.Lost);
      end if;
   end Mark_Overflow;

   function Overflows (P : Producer) return Unsigned_32 is (P.Lost);

   function Arm_For_Room (P : in out Producer) return Boolean is
   begin
      if not Producer_Attached (P) then
         return True;
      end if;
      P.Room_Arming := Next_Arming (P.Room_Arming);
      Store (P.Own, Room_Waiting_At, P.Room_Arming);
      Full_Fence;
      if Can_Push (P) then
         Disarm_For_Room (P);
         return False;
      end if;
      return True;
   end Arm_For_Room;

   procedure Disarm_For_Room (P : Producer) is
   begin
      if P.Own /= System.Null_Address then
         Store (P.Own, Room_Waiting_At, 0);
      end if;
   end Disarm_For_Room;

   ---------------------------------------------------------------------------
   --  Consumer
   ---------------------------------------------------------------------------
   procedure Attach_Consumer (C : in out Consumer; Own_Region, Producer_Region : System.Address) is
   begin
      C.Own := Own_Region;
      C.Peer := Producer_Region;
      C.Ring := Rings.New_Consumer;
      C.Rung_Room := 0;
      C.Arming := 0;
      if Own_Region /= System.Null_Address then
         Store (C.Own, Consumed_At, 0);
         Store (C.Own, Consumer_Waiting_At, 0);
      end if;
   end Attach_Consumer;

   procedure Detach_Consumer (C : in out Consumer) is
   begin
      C.Own := System.Null_Address;
      C.Peer := System.Null_Address;
   end Detach_Consumer;

   function Consumer_Attached (C : Consumer) return Boolean is
     (C.Own /= System.Null_Address and then C.Peer /= System.Null_Address);

   function Can_Take (C : in out Consumer) return Boolean is
      Accepted : Boolean;
   begin
      if not Consumer_Attached (C) then
         return False;
      end if;
      Rings.Accept_Produced (C.Ring, Rings.Index (Load (C.Peer, Produced_At)), Accepted);
      return C.Ring.Available > 0;
   end Can_Take;

   procedure Peek (C : in out Consumer; Item : out Element; Got : out Boolean) is
      First : Element;
      pragma Warnings (Off, First);
   begin
      Item := First;
      Got := False;
      if not Can_Take (C) then
         return;
      end if;
      declare
         --  SEC-025: snapshot the producer may write its slots; the element
         --  is copied once and the caller decides on the copy.
         Slots : constant Rings.Ring with Import, Address => At_Offset (C.Peer, Ring_At);
      begin
         Barrier;
         Item := Slots (Rings.Head_Slot (C.Ring));
         Barrier;
      end;
      Got := True;
   end Peek;

   procedure Take (C : in out Consumer; Item : out Element; Got, Ring_Peer : out Boolean) is
      First : Element;
      pragma Warnings (Off, First);
   begin
      Item := First;
      Got := False;
      Ring_Peer := False;
      if not Can_Take (C) then
         return;
      end if;
      declare
         --  SEC-025: snapshot the producer may write its slots; Take copies
         --  the element once and the caller decides on the copy.
         Slots : constant Rings.Ring with Import, Address => At_Offset (C.Peer, Ring_At);
      begin
         Barrier;
         Rings.Take (C.Ring, Slots, Item);
         Barrier;
      end;
      Store (C.Own, Consumed_At, Unsigned_32 (C.Ring.Consumed));
      Full_Fence;
      Got := True;
      Ring_Once (Load (C.Peer, Room_Waiting_At), C.Rung_Room, Ring_Peer);
   end Take;

   function Peer_Overflows (C : Consumer) return Unsigned_32 is
     (if Consumer_Attached (C) then Load (C.Peer, Overflows_At) else 0);

   function Arm (C : in out Consumer) return Boolean is
   begin
      if not Consumer_Attached (C) then
         return True;
      end if;
      C.Arming := Next_Arming (C.Arming);
      Store (C.Own, Consumer_Waiting_At, C.Arming);
      Full_Fence;
      if Can_Take (C) then
         Disarm (C);
         return False;
      end if;
      return True;
   end Arm;

   procedure Disarm (C : Consumer) is
   begin
      if C.Own /= System.Null_Address then
         Store (C.Own, Consumer_Waiting_At, 0);
      end if;
   end Disarm;

end CuBit.Event_Channels;
