------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  devmgr's decision on a Consumer_Initialization_Complete
--  (CuBit.Consumer_Initialization_Exchange): its receipt, whether the
--  declaration is recorded, and which deferred launches it releases.
--
--  @description
--  The caller has already refused anything not in the canonical frame
--  (Canonical) and answered a repeated (sender, request id) from its
--  receipts; Decide sees only canonical, new requests.
--    * not through the firmware-data badge: Not_Approved;
--    * request id 0, epoch 0 or an unknown status: Malformed (epoch 0 is
--      no ACPI epoch: a GPIO bootstrap observation in epoch 0 is never
--      recorded either, so the two stay consistent);
--    * the first declaration per (sender, epoch) is recorded; the same
--      status again is Received and changes nothing; another status is
--      Conflicting (never a promotion);
--    * only a first status that ran to the end (CX.Ran_To_End: Completed
--      or Completed_With_Errors) releases, and only the sender's records
--      observed in that same epoch (Releases).
--  Pure and SPARK; the caller keeps the records and launches nothing here.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Consumer_Initialization_Exchange;

package Devmgr_Phase_Declarations with Pure, SPARK_Mode is
   package CX renames CuBit.Consumer_Initialization_Exchange;
   use type CX.Phase_Status;
   use type CX.Receipt;

   --  No ACPI namespace epoch is zero.
   No_Epoch : constant := 0;

   type Declaration is record
      Owner, Epoch : Unsigned_64 := 0;
      Status       : CX.Phase_Status := CX.Failed;
   end record;
   type Declaration_Array is array (Positive range <>) of Declaration;

   type Verdict is record
      Answer    : CX.Receipt := CX.Malformed;
      Status    : CX.Phase_Status := CX.Failed;   --  the declared status (when valid)
      Record_It : Boolean := False;   --  append (Owner, Epoch, Status)
      Release   : Boolean := False;   --  release Owner's records observed in Epoch
      Conflict  : Boolean := False;   --  Earlier holds the recorded status
      Earlier   : CX.Phase_Status := CX.Failed;
   end record;

   --  The canonical request frame: exactly Request_Words words, no
   --  protocol or IPC flags (nothing offered, nothing accepted), the
   --  reserved field zero, and every word past the request zero.
   type Word_Array is array (Natural range <>) of Unsigned_64;
   function Canonical
     (Length : Natural; Protocol : Unsigned_8; Flags_Clear : Boolean; Reserved : Unsigned_16;
      Words : Word_Array) return Boolean
   is (Length = CX.Request_Words and then Protocol = 0 and then Flags_Clear and then Reserved = 0
       and then (for all K in Words'Range => (if K >= CX.Request_Words then Words (K) = 0)));

   --  A recorded observation is released by a Ran_To_End status from its sender
   --  for exactly its epoch.
   function Releases (Record_Owner, Record_Epoch, Owner, Epoch : Unsigned_64) return Boolean is
     (Record_Owner = Owner and then Record_Epoch = Epoch and then Epoch /= No_Epoch);

   function Decide
     (Known : Declaration_Array; Approved : Boolean;
      Owner, Request, Epoch, Status_Word : Unsigned_64) return Verdict
   with Post =>
     (if Decide'Result.Release then
        Decide'Result.Answer = CX.Received and then Decide'Result.Record_It
        and then CX.Ran_To_End (Decide'Result.Status) and then Epoch /= No_Epoch
        and then Approved and then Request /= 0)
     and then (if Decide'Result.Record_It then
                 Decide'Result.Answer = CX.Received
                 and then (for all D of Known => not (D.Owner = Owner and then D.Epoch = Epoch)))
     and then (if not Approved then Decide'Result.Answer = CX.Not_Approved)
     and then (if Approved and then (Request = 0 or else Epoch = No_Epoch
                                     or else not CX.Valid_Status (Status_Word))
               then Decide'Result.Answer = CX.Malformed);

end Devmgr_Phase_Declarations;
