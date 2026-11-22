with Interfaces; use Interfaces;
with Intel_GPU_Display_Registers;
with Intel_GPU_Display_Decode;
with Intel_GPU_Transcoder_Timing;
--  DISP-002: the display engine as the firmware left it, read-only: every
--  transcoder's function, timings and state, the DDI buffers, the combo
--  PLLs with their dividers and routing, the crystal and CD clock. Logged
--  register by register so one boot log says which port is lit, how, and
--  at what pixel clock (Boot Logs: "intel-gpu: display readout ...").
generic
   --  Read-only display MMIO (register BAR offset), all ones on failure.
   with function Read_MMIO (Offset : Unsigned_32) return Unsigned_32;
   with procedure Log (Text : String);
package Intel_GPU_Display_Readout is
   package R renames Intel_GPU_Display_Registers;
   package D renames Intel_GPU_Display_Decode;
   package T renames Intel_GPU_Transcoder_Timing;

   type Transcoder_State is record
      Configuration, Function_Word : Unsigned_32 := 0;
      Running : Boolean := False;
      Function_Of : D.DDI_Function;
      Words : T.Words;
      Timing : T.Timing;
   end record;
   type Transcoder_States is array (R.Transcoder) of Transcoder_State;
   type Port_Words is array (R.DDI_Port) of Unsigned_32;
   type PLL_State is record
      Enable_Word, CFGCR0, CFGCR1 : Unsigned_32 := 0;
      Status : D.PLL_Status;
      Dividers : D.Divider_Decoding;
   end record;
   type PLL_States is array (R.Combo_PLL) of PLL_State;

   type Snapshot is record
      Transcoders : Transcoder_States;
      Buffers : Port_Words := [others => 0];
      PLLs : PLL_States;
      Routing_Word, DSSM, CDCLK_Control, DE_PLL : Unsigned_32 := 0;
      Crystal : D.Crystal := D.Unknown;
      CDCLK_kHz : D.Clock_kHz := 0;
   end record;
   procedure Capture (Item : out Snapshot);

   --  What scans out on Pipe (its transcoder), decoded: port, signal and
   --  pixel clock (combo PLL only; zero when unknown).
   type Lit_Output (Found : Boolean := False) is record
      case Found is
         when True =>
            Port : R.DDI_Port;
            Mode : D.Signal_Mode;
            Timing : T.Timing;
            PLL : R.Combo_PLL;
            Pixel_kHz : D.Clock_kHz;
         when False => null;
      end case;
   end record;
   function Lit (Item : Snapshot; Pipe : R.Transcoder) return Lit_Output;

   --  One line per enabled transcoder, port buffer, PLL, then the clocks.
   procedure Describe (Item : Snapshot);
   function Hex (Value : Unsigned_32) return String;
   function Port_Name (Port : R.DDI_Port) return String;
   function Mode_Name (Mode : D.Signal_Mode) return String;
end Intel_GPU_Display_Readout;
