with Interfaces;
with System;
with Client_Frame_Buffer;
with Client_Frame_Damage;
with CuBit.Desktop_Protocol.Publication;
-- Serialized FFI owner: geometry and damage policy are delegated to the proved
-- protocol/geometry/repaint packages; allocation/protection/IPC to Frame_Buffer.
-- The caller retains authoritative drawing state and repaints Repair in full.
package Client_Frame_Pair with SPARK_Mode => Off is
   package DP renames CuBit.Desktop_Protocol;
   package Pub renames DP.Publication;
   package Debt renames Client_Frame_Damage;
   type Owner is limited private;
   procedure Reset (O : in out Owner; Ready : out Boolean);
   -- Query authenticates configuration; failure preserves the last valid one.
   -- Configure/Begin/Publish are serialized with all drawing through Address.
   procedure Configure (O : in out Owner; Surface : DP.Live_Surface_Name; OK : out Boolean);
   function Configuration (O : Owner) return Pub.Configuration_Result;
   procedure Begin_Paint (O : in out Owner; Changed : Debt.Box;
                          Repair : out Debt.Box; Ready : out Boolean);
   function Address (O : Owner) return System.Address;
   -- Rendered must cover all repair debt, including any deferred damage.
   -- Success withdraws writable access. Failure retains damage for retry.
   procedure Publish (O : in out Owner; Rendered : Debt.Box; Accepted : out Boolean;
                      Input_After : Interfaces.Unsigned_64 := 0);
   procedure Cancel_Paint (O : in out Owner);
   function Pending (O : Owner) return Boolean;
   function Allocated_Bytes (O : Owner) return Natural;
   -- Destroy the surface first. Pending/uncertain loans remain retained.
   -- Failed close is terminal for rendering; repeat Close to finish retirement.
   procedure Close (O : in out Owner; Released : out Boolean);
private
   type Buffers is array (Debt.Slot) of Client_Frame_Buffer.Buffer;
   type Owner is limited record
      Frames : Buffers;
      Config : Pub.Configuration_Result;
      Surface : DP.Live_Surface_Name := 1;
      Damage : Debt.State;
      Next : Debt.Slot := 1;
      Painting : Boolean := False;
      Closing : Boolean := False;
   end record;
end Client_Frame_Pair;
