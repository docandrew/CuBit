------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A source ring (IPC-009, DEV-002): one input source device's reports from
--  its driver to Desktop, a CuBit.Event_Channels instance carrying
--  CuBit.Input.Report_Words. The driver side is CuBit.Input_Source; the
--  consumer side is Desktop_Input_Sources.
--
--  @description
--  Establishment is the control plane, typed by Contract (its schema
--  identity), with no operation of its own: the driver asks devmgr, its
--  launcher, to introduce it to the peer serving Contract
--  (CuBit.Channels.Introduce). devmgr answers with an endpoint to Desktop,
--  badged CuBit.Input.Source_Ring_Tag, only to the input drivers it
--  launched, and never maps a ring. The driver opens one channel per source
--  device over it (CuBit.Channels.Open, Contract, producing); Desktop
--  admits an open only through that badge, and the channel it accepts is
--  the source's identity. After that the kernel only wakes: each side
--  rings its own region's grant when the other armed its waiting word.
--  A Desktop that restarts ends every ring (the grants end with it); the
--  drivers ask devmgr again and reopen.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Channel_Contracts;
with CuBit.Event_Channels;
with CuBit.Input;
with CuBit.Protocols;

package CuBit.Input_Rings is

   --  256 reports in flight per source: a burst of typing or a second of
   --  unread pointer motion (pointers coalesce in the driver meanwhile).
   Slot_Bits : constant := 8;
   package Channels is new CuBit.Event_Channels (CuBit.Input.Report_Words, Slot_Bits);

   Schema : constant CuBit.Protocols.Schema_Id := 16#494E_5055_5452_0001#;
   Element_Bytes : constant := 4 * 8;
   pragma Compile_Time_Error
     (CuBit.Input.Report_Words'Size /= Element_Bytes * 8, "a source report is four words");

   Contract : constant CuBit.Channel_Contracts.Contract :=
     (Element => (Identity => Schema, Version => 1,
                  Sizing => CuBit.Protocols.Fixed_Size, Wire_Size => Element_Bytes),
      Kind    => CuBit.Channel_Contracts.Duplex,
      Policy  => CuBit.Channel_Contracts.Lossless,
      Pages   => Channels.Producer_Pages,
      Buffers => Channels.Consumer_Pages,
      Rule    => CuBit.Channel_Contracts.Copy_Then_Validate);

   --  Desktop's admission limits: sources per driver (a USB controller's
   --  keyboards and pointers) and in all.
   Sources_Per_Driver : constant := 16;

end CuBit.Input_Rings;
