------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  xHCI register fields, TRB fields and completion codes, by name.
--
--  @description
--  Pure decoding and the one PORTSC write that acknowledges change bits.
--  References are to the eXtensible Host Controller Interface for USB
--  specification, revision 1.2 (May 2019):
--    5.3   capability registers (HCIVERSION, HCSPARAMS1, HCCPARAMS1);
--    5.4.1 USBCMD, 5.4.2 USBSTS, 5.4.5 CRCR, 5.4.8 PORTSC;
--    5.5.2 interrupter registers (IMAN, IMOD, ERDP);
--    6.2.3 endpoint context (EP State), 6.4 TRBs, 6.4.5 completion codes.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package XHCI_Registers with SPARK_Mode => On, Pure is

   ---------------------------------------------------------------------------
   --  Capability registers (5.3)
   ---------------------------------------------------------------------------
   HCIVERSION_SHIFT       : constant := 16;   --  in the CAPLENGTH dword
   HCSPARAMS1_SLOTS_MASK  : constant Unsigned_32 := 16#0000_00FF#;
   HCSPARAMS1_INTRS_SHIFT : constant := 8;
   HCSPARAMS1_INTRS_MASK  : constant Unsigned_32 := 16#0000_07FF#;
   HCSPARAMS1_PORTS_SHIFT : constant := 24;
   HCSPARAMS1_PORTS_MASK  : constant Unsigned_32 := 16#0000_00FF#;
   HCCPARAMS1_AC64        : constant Unsigned_32 := 2 ** 0;
   HCCPARAMS1_CSZ         : constant Unsigned_32 := 2 ** 2;

   function HCI_Version (Cap_Dword : Unsigned_32) return Unsigned_16 is
     (Unsigned_16 (Shift_Right (Cap_Dword, HCIVERSION_SHIFT)));
   function Max_Slots (HCS1 : Unsigned_32) return Unsigned_32 is
     (HCS1 and HCSPARAMS1_SLOTS_MASK);
   function Max_Interrupters (HCS1 : Unsigned_32) return Unsigned_32 is
     (Shift_Right (HCS1, HCSPARAMS1_INTRS_SHIFT) and HCSPARAMS1_INTRS_MASK);
   function Max_Ports (HCS1 : Unsigned_32) return Unsigned_32 is
     (Shift_Right (HCS1, HCSPARAMS1_PORTS_SHIFT) and HCSPARAMS1_PORTS_MASK);

   ---------------------------------------------------------------------------
   --  USBCMD (5.4.1), USBSTS (5.4.2), CRCR (5.4.5)
   ---------------------------------------------------------------------------
   USBCMD_RUN   : constant Unsigned_32 := 2 ** 0;
   USBCMD_HCRST : constant Unsigned_32 := 2 ** 1;
   USBCMD_INTE  : constant Unsigned_32 := 2 ** 2;

   USBSTS_HCH  : constant Unsigned_32 := 2 ** 0;
   USBSTS_HSE  : constant Unsigned_32 := 2 ** 2;   --  RW1C
   USBSTS_EINT : constant Unsigned_32 := 2 ** 3;   --  RW1C
   USBSTS_PCD  : constant Unsigned_32 := 2 ** 4;   --  RW1C
   USBSTS_CNR  : constant Unsigned_32 := 2 ** 11;
   USBSTS_HCE  : constant Unsigned_32 := 2 ** 12;

   CRCR_RCS : constant Unsigned_32 := 2 ** 0;
   CRCR_CA  : constant Unsigned_32 := 2 ** 2;
   CRCR_CRR : constant Unsigned_32 := 2 ** 3;

   ---------------------------------------------------------------------------
   --  Interrupter registers (5.5.2)
   ---------------------------------------------------------------------------
   IMAN_IP  : constant Unsigned_32 := 2 ** 0;   --  RW1C
   IMAN_IE  : constant Unsigned_32 := 2 ** 1;
   ERDP_EHB : constant Unsigned_64 := 2 ** 3;   --  RW1C (5.5.2.3.3)

   ---------------------------------------------------------------------------
   --  PORTSC (5.4.8)
   ---------------------------------------------------------------------------
   PORTSC_CCS : constant Unsigned_32 := 2 ** 0;    --  ROS
   PORTSC_PED : constant Unsigned_32 := 2 ** 1;    --  RW1CS: writing 1 disables
   PORTSC_OCA : constant Unsigned_32 := 2 ** 3;
   PORTSC_PR  : constant Unsigned_32 := 2 ** 4;    --  RW1S
   PORTSC_PLS_SHIFT : constant := 5;
   PORTSC_PLS_MASK  : constant Unsigned_32 := 16#F#;
   PORTSC_PP  : constant Unsigned_32 := 2 ** 9;
   PORTSC_SPEED_SHIFT : constant := 10;
   PORTSC_SPEED_MASK  : constant Unsigned_32 := 16#F#;
   PORTSC_PIC : constant Unsigned_32 := 2 ** 14 or 2 ** 15;
   PORTSC_LWS : constant Unsigned_32 := 2 ** 16;   --  strobe
   PORTSC_CSC : constant Unsigned_32 := 2 ** 17;
   PORTSC_PEC : constant Unsigned_32 := 2 ** 18;
   PORTSC_WRC : constant Unsigned_32 := 2 ** 19;
   PORTSC_OCC : constant Unsigned_32 := 2 ** 20;
   PORTSC_PRC : constant Unsigned_32 := 2 ** 21;
   PORTSC_PLC : constant Unsigned_32 := 2 ** 22;
   PORTSC_CEC : constant Unsigned_32 := 2 ** 23;
   PORTSC_CAS : constant Unsigned_32 := 2 ** 24;
   PORTSC_WAKE_ENABLES : constant Unsigned_32 := 2 ** 25 or 2 ** 26 or 2 ** 27;
   PORTSC_DR  : constant Unsigned_32 := 2 ** 30;
   PORTSC_WPR : constant Unsigned_32 := 2 ** 31;   --  RW1S warm reset

   --  All RW1C change bits (CSC, PEC, WRC, OCC, PRC, PLC, CEC).
   PORTSC_CHANGE_BITS : constant Unsigned_32 :=
     PORTSC_CSC or PORTSC_PEC or PORTSC_WRC or PORTSC_OCC or
     PORTSC_PRC or PORTSC_PLC or PORTSC_CEC;
   --  The ordinary read/write controls a PORTSC write must echo: power,
   --  indicators and wake enables. Everything else is a strobe, a RW1C or
   --  RW1S trigger, or read-only.
   PORTSC_PRESERVE : constant Unsigned_32 :=
     PORTSC_PP or PORTSC_PIC or PORTSC_WAKE_ENABLES;

   --  Port link states (PLS), 5.4.8 table 5-27.
   type Link_State is new Unsigned_32 range 0 .. 15;
   LINK_U0          : constant Link_State := 0;
   LINK_U3          : constant Link_State := 3;
   LINK_DISABLED    : constant Link_State := 4;
   LINK_RX_DETECT   : constant Link_State := 5;
   LINK_INACTIVE    : constant Link_State := 6;
   LINK_POLLING     : constant Link_State := 7;
   LINK_RECOVERY    : constant Link_State := 8;
   LINK_HOT_RESET   : constant Link_State := 9;
   LINK_COMPLIANCE  : constant Link_State := 10;
   LINK_RESUME      : constant Link_State := 15;

   --  Default protocol speed IDs (7.2.2.1.1; Intel's PSIs match them).
   type Port_Speed is new Unsigned_32 range 0 .. 15;
   SPEED_NONE  : constant Port_Speed := 0;
   SPEED_FULL  : constant Port_Speed := 1;
   SPEED_LOW   : constant Port_Speed := 2;
   SPEED_HIGH  : constant Port_Speed := 3;
   SPEED_SUPER : constant Port_Speed := 4;
   SPEED_SUPER_PLUS : constant Port_Speed := 5;

   function Link (Status : Unsigned_32) return Link_State is
     (Link_State (Shift_Right (Status, PORTSC_PLS_SHIFT) and PORTSC_PLS_MASK));
   function Speed (Status : Unsigned_32) return Port_Speed is
     (Port_Speed (Shift_Right (Status, PORTSC_SPEED_SHIFT) and PORTSC_SPEED_MASK));
   function Connected (Status : Unsigned_32) return Boolean is
     ((Status and PORTSC_CCS) /= 0);
   function Enabled (Status : Unsigned_32) return Boolean is
     ((Status and PORTSC_PED) /= 0);
   function In_Reset (Status : Unsigned_32) return Boolean is
     ((Status and (PORTSC_PR or PORTSC_WPR)) /= 0);
   function Changes (Status : Unsigned_32) return Unsigned_32 is
     (Status and PORTSC_CHANGE_BITS);
   function Is_Super_Speed (Status : Unsigned_32) return Boolean is
     (Speed (Status) >= SPEED_SUPER);

   --  The write that acknowledges exactly the change bits Status shows
   --  (5.4.8: RW1C) and echoes the preserved controls: it never disables
   --  the port (PED), starts a reset (PR, WPR) or strobes the link (LWS).
   function Acknowledge_Changes (Status : Unsigned_32) return Unsigned_32 is
     ((Status and PORTSC_PRESERVE) or (Status and PORTSC_CHANGE_BITS))
   with Post =>
     (Acknowledge_Changes'Result and
        (PORTSC_PED or PORTSC_PR or PORTSC_WPR or PORTSC_LWS)) = 0
     and then (Acknowledge_Changes'Result and PORTSC_CHANGE_BITS) =
                (Status and PORTSC_CHANGE_BITS)
     and then (Acknowledge_Changes'Result and PORTSC_PP) = (Status and PORTSC_PP);

   ---------------------------------------------------------------------------
   --  TRB fields (6.4)
   ---------------------------------------------------------------------------
   TRB_CYCLE         : constant Unsigned_32 := 2 ** 0;
   TRB_TOGGLE_CYCLE  : constant Unsigned_32 := 2 ** 1;   --  Link TRB TC
   TRB_EVENT_DATA    : constant Unsigned_32 := 2 ** 2;   --  Transfer Event ED
   TRB_ISP           : constant Unsigned_32 := 2 ** 2;   --  Normal/Data ISP
   TRB_IOC           : constant Unsigned_32 := 2 ** 5;
   TRB_IDT           : constant Unsigned_32 := 2 ** 6;
   TRB_TSP           : constant Unsigned_32 := 2 ** 9;   --  Reset Endpoint
   TRB_TYPE_SHIFT    : constant := 10;
   TRB_TYPE_MASK     : constant Unsigned_32 := 16#3F#;
   TRB_DIRECTION_IN  : constant Unsigned_32 := 2 ** 16;  --  Data/Status stage
   TRB_TRT_SHIFT     : constant := 16;                   --  Setup Stage TRT
   TRB_ENDPOINT_SHIFT : constant := 16;
   TRB_ENDPOINT_MASK : constant Unsigned_32 := 16#1F#;
   TRB_SLOT_SHIFT    : constant := 24;
   TRB_PORT_SHIFT    : constant := 24;                    --  PSC event param
   TRB_LENGTH_MASK   : constant Unsigned_32 := 16#00FF_FFFF#;
   TRB_CODE_SHIFT    : constant := 24;

   --  Setup Stage transfer types (6.4.1.2.1).
   TRANSFER_NO_DATA : constant Unsigned_32 := 0;
   TRANSFER_OUT     : constant Unsigned_32 := 2;
   TRANSFER_IN      : constant Unsigned_32 := 3;

   type TRB_Type is new Unsigned_32 range 0 .. 63;
   TRB_NORMAL           : constant TRB_Type := 1;
   TRB_SETUP_STAGE      : constant TRB_Type := 2;
   TRB_DATA_STAGE       : constant TRB_Type := 3;
   TRB_STATUS_STAGE     : constant TRB_Type := 4;
   TRB_LINK             : constant TRB_Type := 6;
   TRB_ENABLE_SLOT      : constant TRB_Type := 9;
   TRB_DISABLE_SLOT     : constant TRB_Type := 10;
   TRB_ADDRESS_DEVICE   : constant TRB_Type := 11;
   TRB_CONFIGURE_ENDPOINT : constant TRB_Type := 12;
   TRB_EVALUATE_CONTEXT : constant TRB_Type := 13;
   TRB_RESET_ENDPOINT   : constant TRB_Type := 14;
   TRB_STOP_ENDPOINT    : constant TRB_Type := 15;
   TRB_SET_TR_DEQUEUE   : constant TRB_Type := 16;
   TRB_TRANSFER_EVENT   : constant TRB_Type := 32;
   TRB_COMMAND_COMPLETION : constant TRB_Type := 33;
   TRB_PORT_STATUS_CHANGE : constant TRB_Type := 34;
   TRB_BANDWIDTH_REQUEST  : constant TRB_Type := 35;
   TRB_DOORBELL_EVENT     : constant TRB_Type := 36;
   TRB_HOST_CONTROLLER    : constant TRB_Type := 37;
   TRB_DEVICE_NOTIFICATION : constant TRB_Type := 38;
   TRB_MFINDEX_WRAP       : constant TRB_Type := 39;

   function Type_Field (Kind : TRB_Type) return Unsigned_32 is
     (Shift_Left (Unsigned_32 (Kind), TRB_TYPE_SHIFT));
   function Type_Of (Control : Unsigned_32) return TRB_Type is
     (TRB_Type (Shift_Right (Control, TRB_TYPE_SHIFT) and TRB_TYPE_MASK));
   function Slot_Field (Slot : Unsigned_32) return Unsigned_32 is
     (Shift_Left (Slot and 16#FF#, TRB_SLOT_SHIFT));
   function Endpoint_Field (DCI : Unsigned_32) return Unsigned_32 is
     (Shift_Left (DCI and TRB_ENDPOINT_MASK, TRB_ENDPOINT_SHIFT));
   function Slot_Of (Control : Unsigned_32) return Unsigned_32 is
     (Shift_Right (Control, TRB_SLOT_SHIFT));
   function Endpoint_Of (Control : Unsigned_32) return Unsigned_32 is
     (Shift_Right (Control, TRB_ENDPOINT_SHIFT) and TRB_ENDPOINT_MASK);
   function Port_Of (Parameter_Lo : Unsigned_32) return Unsigned_32 is
     (Shift_Right (Parameter_Lo, TRB_PORT_SHIFT));
   function Residual_Of (Status : Unsigned_32) return Unsigned_32 is
     (Status and TRB_LENGTH_MASK);

   ---------------------------------------------------------------------------
   --  Completion codes (6.4.5)
   ---------------------------------------------------------------------------
   type Completion_Code is new Unsigned_8;
   CC_INVALID             : constant Completion_Code := 0;
   CC_SUCCESS             : constant Completion_Code := 1;
   CC_DATA_BUFFER_ERROR   : constant Completion_Code := 2;
   CC_BABBLE              : constant Completion_Code := 3;
   CC_TRANSACTION_ERROR   : constant Completion_Code := 4;
   CC_TRB_ERROR           : constant Completion_Code := 5;
   CC_STALL               : constant Completion_Code := 6;
   CC_RESOURCE_ERROR      : constant Completion_Code := 7;
   CC_BANDWIDTH_ERROR     : constant Completion_Code := 8;
   CC_NO_SLOTS            : constant Completion_Code := 9;
   CC_SLOT_NOT_ENABLED    : constant Completion_Code := 11;
   CC_ENDPOINT_NOT_ENABLED : constant Completion_Code := 12;
   CC_SHORT_PACKET        : constant Completion_Code := 13;
   CC_RING_UNDERRUN       : constant Completion_Code := 14;
   CC_RING_OVERRUN        : constant Completion_Code := 15;
   CC_PARAMETER_ERROR     : constant Completion_Code := 17;
   CC_CONTEXT_STATE_ERROR : constant Completion_Code := 19;
   CC_EVENT_RING_FULL     : constant Completion_Code := 21;
   CC_MISSED_SERVICE      : constant Completion_Code := 23;
   CC_COMMAND_RING_STOPPED : constant Completion_Code := 24;
   CC_COMMAND_ABORTED     : constant Completion_Code := 25;
   CC_STOPPED             : constant Completion_Code := 26;
   CC_STOPPED_LENGTH_INVALID : constant Completion_Code := 27;
   CC_STOPPED_SHORT_PACKET : constant Completion_Code := 28;
   CC_EVENT_LOST          : constant Completion_Code := 32;
   CC_SPLIT_TRANSACTION_ERROR : constant Completion_Code := 36;

   function Code_Of (Status : Unsigned_32) return Completion_Code is
     (Completion_Code (Shift_Right (Status, TRB_CODE_SHIFT)));

   --  A short name for log lines.
   function Code_Name (Code : Completion_Code) return String is
     (case Code is
         when CC_INVALID => "invalid",
         when CC_SUCCESS => "success",
         when CC_DATA_BUFFER_ERROR => "data-buffer",
         when CC_BABBLE => "babble",
         when CC_TRANSACTION_ERROR => "transaction",
         when CC_TRB_ERROR => "trb",
         when CC_STALL => "stall",
         when CC_RESOURCE_ERROR => "resource",
         when CC_BANDWIDTH_ERROR => "bandwidth",
         when CC_NO_SLOTS => "no-slots",
         when CC_SLOT_NOT_ENABLED => "slot-off",
         when CC_ENDPOINT_NOT_ENABLED => "ep-off",
         when CC_SHORT_PACKET => "short",
         when CC_RING_UNDERRUN => "underrun",
         when CC_RING_OVERRUN => "overrun",
         when CC_PARAMETER_ERROR => "parameter",
         when CC_CONTEXT_STATE_ERROR => "context-state",
         when CC_EVENT_RING_FULL => "event-ring-full",
         when CC_MISSED_SERVICE => "missed-service",
         when CC_COMMAND_RING_STOPPED => "ring-stopped",
         when CC_COMMAND_ABORTED => "aborted",
         when CC_STOPPED => "stopped",
         when CC_STOPPED_LENGTH_INVALID => "stopped-len",
         when CC_STOPPED_SHORT_PACKET => "stopped-short",
         when CC_EVENT_LOST => "event-lost",
         when CC_SPLIT_TRANSACTION_ERROR => "split",
         when others => "other");

   ---------------------------------------------------------------------------
   --  Endpoint context (6.2.3)
   ---------------------------------------------------------------------------
   EP_STATE_MASK : constant Unsigned_32 := 16#7#;
   type Endpoint_State is new Unsigned_32 range 0 .. 7;
   EP_DISABLED : constant Endpoint_State := 0;
   EP_RUNNING  : constant Endpoint_State := 1;
   EP_HALTED   : constant Endpoint_State := 2;
   EP_STOPPED  : constant Endpoint_State := 3;
   EP_ERROR    : constant Endpoint_State := 4;
   function State_Of (Context_Dword0 : Unsigned_32) return Endpoint_State is
     (Endpoint_State (Context_Dword0 and EP_STATE_MASK));

end XHCI_Registers;
