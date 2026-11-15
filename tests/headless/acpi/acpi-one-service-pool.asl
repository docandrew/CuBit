/*
 * ACPI-ONE-001 regression table for headless --test acpi-one-service.
 *
 * The laptop (image 11) stopped at module startup: an SSDT's module-level
 * code read RBAR in the MBAR SystemMemory region (TCSS, 0xFEDC7110). That
 * region is a platform-register pool member, not firmware-reserved memory,
 * so the former acpi-backend.svc pair's remote register provider had no
 * route for it (its firmware-data path serves only firmware-reserved/NVS
 * spans) and module startup was refused.
 *
 * q35 shows the same shape with devmgr's declared test profile (QEMU's edu
 * device at 00:04.0, BAR0 0xFEA00000 with this device set): 0xFEAC0000
 * +0x10000 is a CHIPSET_SHARED pool member standing in for a GPIO
 * community. It sits inside a device BAR, so devmgr refuses it as firmware
 * data, and it stays admitted after edu's fixture driver binds (shared), so
 * there is no bind race. Module-level code here reads it during table load,
 * before any device _INI, exactly like the laptop's read. Note: q35's own
 * RCRB (0xFED1C000) is firmware-reserved and the pair could already read it,
 * so it does not reproduce the refusal.
 *
 * Rebuild the committed .aml (QEMU -acpitable takes the compiled table):
 *   /nix/store/72fphm0m9k2f8zd066ayxkqi44ia9ql5-acpica-tools-20260408/bin/iasl \
 *     -p acpi-one-service-pool acpi-one-service-pool.asl
 */
DefinitionBlock ("", "SSDT", 2, "CUBIT ", "ONESVC", 0x00000002)
{
    OperationRegion (GPCM, SystemMemory, 0xFEAC0000, 0x1000)
    Field (GPCM, DWordAcc, NoLock, Preserve)
    {
        GREG,  32
    }
    Name (GVAL, 0xFFFFFFFF)
    Store (GREG, GVAL)
}
