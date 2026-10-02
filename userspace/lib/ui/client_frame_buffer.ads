with System;
with CuBit.Desktop_Protocol;
with CuBit.Desktop_Protocol.Publication;
with CuBit.Memory_Grants;
with Interfaces;
with Client_Frame_State;
-- Serialized owner of one reclaimable frame allocation. Handles cannot be
-- copied. Write pointers are valid only until Present or Release; Present
-- also removes write permission from the owner's mapping before IPC.
-- IPC authentication, kernel protection and backing retention are trusted
-- foreign boundaries. The pure state policy governs write eligibility.
package Client_Frame_Buffer with SPARK_Mode => Off is
   package DP renames CuBit.Desktop_Protocol;
   package Pub renames CuBit.Desktop_Protocol.Publication;
   type Buffer is limited private;
   procedure Allocate (B : in out Buffer; Bytes : Natural; OK : out Boolean);
   function Writable_Address (B : Buffer) return System.Address;
   function Capacity (B : Buffer) return Natural;
   procedure Prepare_Write (B : in out Buffer; Ready : out Boolean);
   procedure Present
     (B : in out Buffer; Surface : DP.Live_Surface_Name; Epoch : Pub.Identity;
      Area : DP.Rectangle; Accepted : out Boolean;
      Input_After : Interfaces.Unsigned_64 := 0);
   procedure Release (B : in out Buffer; Released : out Boolean);
private
   type Buffer is limited record
      Policy : Client_Frame_State.State;
      Base : Interfaces.Unsigned_64 := 0;
      Bytes : Natural := 0;
      Has_Grant : Boolean := False;
      Grant : CuBit.Memory_Grants.Grant_Reference;
      Surface : DP.Live_Surface_Name := 1;
   end record;
end Client_Frame_Buffer;
