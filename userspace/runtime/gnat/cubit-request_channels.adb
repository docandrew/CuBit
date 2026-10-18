------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with System.Machine_Code;
with System.Storage_Elements; use System.Storage_Elements;

package body CuBit.Request_Channels is

   use type System.Address;

   --  Compiler barrier: x86-64 keeps stores in order and loads in order.
   procedure Barrier;
   procedure Barrier is
   begin
      System.Machine_Code.Asm ("", Volatile => True, Clobber => "memory");
   end Barrier;

   --  A store, then a load of the peer's word (the wake handshake): x86-64
   --  may let the load pass the store, so each side fences.
   procedure Full_Fence;
   procedure Full_Fence is
   begin
      System.Machine_Code.Asm ("mfence", Volatile => True, Clobber => "memory");
   end Full_Fence;

   function At_Offset (Base : System.Address; Offset : Natural) return System.Address is
     (Base + Storage_Offset (Offset));

   function Load (Base : System.Address; Offset : Natural) return Unsigned_32;
   function Load (Base : System.Address; Offset : Natural) return Unsigned_32 is
      Word : constant Unsigned_32 with Import, Volatile, Address => At_Offset (Base, Offset);
   begin
      return Word;
   end Load;

   procedure Store (Base : System.Address; Offset : Natural; Value : Unsigned_32);
   procedure Store (Base : System.Address; Offset : Natural; Value : Unsigned_32) is
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
   --  Client
   ---------------------------------------------------------------------------
   procedure Attach_Client (C : in out Client; Own_Region, Server_Region : System.Address) is
   begin
      C.Own := Own_Region;
      C.Peer := Server_Region;
      C.Ring := (others => <>);
      C.Rung_Arming := 0;
      C.Arming := 0;
      C.Room_Arming := 0;
   end Attach_Client;

   procedure Detach_Client (C : in out Client) is
   begin
      C.Own := System.Null_Address;
      C.Peer := System.Null_Address;
   end Detach_Client;

   function Client_Attached (C : Client) return Boolean is
     (C.Own /= System.Null_Address and then C.Peer /= System.Null_Address);

   function Can_Submit (C : in out Client) return Boolean is
      Accepted : Boolean;
   begin
      if not Client_Attached (C) then
         return False;
      end if;
      Queues.Accept_Taken (C.Ring, Queues.Submissions.Index (Load (C.Peer, Taken_At)), Accepted);
      return Queues.Can_Submit (C.Ring);
   end Can_Submit;

   function Outstanding (C : Client) return Natural is (C.Ring.Pending);

   procedure Submit (C : in out Client; Tag : Token; Item : Request;
                     Submitted, Ring_Peer : out Boolean) is
   begin
      Submitted := False;
      Ring_Peer := False;
      if not Can_Submit (C) then
         return;
      end if;
      declare
         Slots : Queues.Submissions.Ring with Import, Address => At_Offset (C.Own, Ring_At);
      begin
         Queues.Submit (C.Ring, Slots, Tag, Item);
      end;
      Barrier;
      Store (C.Own, Submitted_At, Unsigned_32 (C.Ring.Requests.Produced));
      Full_Fence;
      Submitted := True;
      Ring_Once (Load (C.Peer, Server_Waiting_At), C.Rung_Arming, Ring_Peer);
   end Submit;

   function Completions_Waiting (C : in out Client) return Boolean is
      Accepted : Boolean;
   begin
      if not Client_Attached (C) then
         return False;
      end if;
      Queues.Completions.Accept_Produced
        (C.Ring.Answers, Queues.Completions.Index (Load (C.Peer, Completed_At)), Accepted);
      return C.Ring.Answers.Available > 0;
   end Completions_Waiting;

   procedure Reap (C : in out Client; Item : out Queues.Completion; Got : out Boolean) is
      First : Queues.Completion;
      pragma Warnings (Off, First);
   begin
      Got := False;
      Item := First;
      if not Completions_Waiting (C) then
         return;
      end if;
      declare
         Slots : constant Queues.Completions.Ring with Import, Address => At_Offset (C.Peer, Ring_At);
      begin
         Barrier;
         Queues.Reap (C.Ring, Slots, Item, Got);
         Barrier;
         Store (C.Own, Reaped_At, Unsigned_32 (C.Ring.Answers.Consumed));
      end;
   end Reap;

   function Arm_For_Completions (C : in out Client) return Boolean is
   begin
      if not Client_Attached (C) then
         return True;
      end if;
      C.Arming := Next_Arming (C.Arming);
      Store (C.Own, Client_Waiting_At, C.Arming);
      Full_Fence;
      if Completions_Waiting (C) then
         Disarm_For_Completions (C);
         return False;
      end if;
      return True;
   end Arm_For_Completions;

   procedure Disarm_For_Completions (C : Client) is
   begin
      if C.Own /= System.Null_Address then
         Store (C.Own, Client_Waiting_At, 0);
      end if;
   end Disarm_For_Completions;

   function Arm_For_Room (C : in out Client) return Boolean is
   begin
      if not Client_Attached (C) then
         return True;
      end if;
      C.Room_Arming := Next_Arming (C.Room_Arming);
      Store (C.Own, Room_Waiting_At, C.Room_Arming);
      Full_Fence;
      if Can_Submit (C) then
         Disarm_For_Room (C);
         return False;
      end if;
      return True;
   end Arm_For_Room;

   procedure Disarm_For_Room (C : Client) is
   begin
      if C.Own /= System.Null_Address then
         Store (C.Own, Room_Waiting_At, 0);
      end if;
   end Disarm_For_Room;

   function Client_State (C : Client) return System.Address is (At_Offset (C.Own, Client_State_At));
   function Server_Status (C : Client) return System.Address is (At_Offset (C.Peer, Status_At));

   ---------------------------------------------------------------------------
   --  Server
   ---------------------------------------------------------------------------
   procedure Attach_Server (S : in out Server; Own_Region, Client_Region : System.Address) is
   begin
      S.Own := Own_Region;
      S.Peer := Client_Region;
      S.Ring := (others => <>);
      S.Rung_Arming := 0;
      S.Rung_Room := 0;
      S.Arming := 0;
   end Attach_Server;

   procedure Detach_Server (S : in out Server) is
   begin
      S.Own := System.Null_Address;
      S.Peer := System.Null_Address;
   end Detach_Server;

   function Server_Attached (S : Server) return Boolean is
     (S.Own /= System.Null_Address and then S.Peer /= System.Null_Address);

   function Can_Take (S : in out Server) return Boolean is
      Accepted : Boolean;
   begin
      if not Server_Attached (S) then
         return False;
      end if;
      Queues.Submissions.Accept_Produced
        (S.Ring.Requests, Queues.Submissions.Index (Load (S.Peer, Submitted_At)), Accepted);
      Queues.Accept_Reaped (S.Ring, Queues.Completions.Index (Load (S.Peer, Reaped_At)), Accepted);
      return Queues.Can_Take (S.Ring);
   end Can_Take;

   function Owed (S : Server) return Natural is (S.Ring.Owed);

   procedure Take (S : in out Server; Item : out Queues.Submission; Got, Ring_Peer : out Boolean) is
      First : Queues.Submission;
      pragma Warnings (Off, First);
   begin
      Got := False;
      Ring_Peer := False;
      Item := First;
      if not Can_Take (S) then
         return;
      end if;
      declare
         Slots : constant Queues.Submissions.Ring with Import, Address => At_Offset (S.Peer, Ring_At);
      begin
         Barrier;
         Queues.Take (S.Ring, Slots, Item);
         Barrier;
      end;
      Store (S.Own, Taken_At, Unsigned_32 (S.Ring.Requests.Consumed));
      Full_Fence;
      Got := True;
      Ring_Once (Load (S.Peer, Room_Waiting_At), S.Rung_Room, Ring_Peer);
   end Take;

   procedure Complete (S : in out Server; Tag : Token; Answer : Result; Ring_Peer : out Boolean) is
      Slots : Queues.Completions.Ring with Import, Address => At_Offset (S.Own, Ring_At);
   begin
      Queues.Complete (S.Ring, Slots, Tag, Answer);
      Barrier;
      Store (S.Own, Completed_At, Unsigned_32 (S.Ring.Answers.Produced));
      Full_Fence;
      Ring_Once (Load (S.Peer, Client_Waiting_At), S.Rung_Arming, Ring_Peer);
   end Complete;

   function Arm_For_Requests (S : in out Server) return Boolean is
   begin
      if not Server_Attached (S) then
         return True;
      end if;
      S.Arming := Next_Arming (S.Arming);
      Store (S.Own, Server_Waiting_At, S.Arming);
      Full_Fence;
      if Can_Take (S) then
         Disarm_For_Requests (S);
         return False;
      end if;
      return True;
   end Arm_For_Requests;

   procedure Disarm_For_Requests (S : Server) is
   begin
      if S.Own /= System.Null_Address then
         Store (S.Own, Server_Waiting_At, 0);
      end if;
   end Disarm_For_Requests;

   function Status_Block (S : Server) return System.Address is (At_Offset (S.Own, Status_At));
   function Peer_Client_State (S : Server) return System.Address is (At_Offset (S.Peer, Client_State_At));

end CuBit.Request_Channels;
