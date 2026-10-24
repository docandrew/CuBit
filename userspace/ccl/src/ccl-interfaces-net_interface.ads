--  One network interface's configuration as CCL types: every
--  `net.interface.<name>` setting is one Net_Interface, checked when the
--  configuration is compiled (CCL.Typed_Settings) and read back the same
--  way by netmgr (Net_Interface_Settings). <name> is the interface's
--  stable name (docs/net-link.md section 7): `pci-<bus>-<device>-<function>`
--  in lower-case hexadecimal, or `mac-<12 hex digits>` for a device with no
--  stable location.
--
--  - Automatic (also what an absent setting means): DHCP for IPv4, SLAAC
--    for IPv6.
--  - Static: one address, written as IPv6 text (IPv4 mapped,
--    ::ffff:a.b.c.d: the one address type), its prefix length over the
--    whole 128 bits (an IPv4 /24 is 120), a gateway (Gateway.None, the
--    default, or (Gateway.Via "<address>")) and the metric of the routes
--    it gives (lower wins, 100 by default).
--  - Disabled: the interface is left unconfigured.
package CCL.Interfaces.Net_Interface with SPARK_Mode, Pure is
   MAXIMUM_PREFIX : constant := 128;
   MAXIMUM_METRIC : constant := 65_535;
   DEFAULT_METRIC : constant := 100;
   TYPE_SOURCE : constant String :=
     "(type Prefix_Length (range 0 128)) " &
     "(type Route_Metric (range 0 65535)) " &
     "(type Gateway (variant (None) (Via String))) " &
     "(type Static_Interface (record (address String) (prefix Prefix_Length) " &
     "(gateway Gateway Gateway.None) (metric Route_Metric 100))) " &
     "(type Net_Interface (variant (Automatic) (Static Static_Interface) (Disabled)))";
   ROOT_TYPE_NAME : constant String := "Net_Interface";
end CCL.Interfaces.Net_Interface;
