/*
 * CuBit OS - Config Store Client Library
 * Copyright (C) 2026 Jon Andrew
 *
 * Provides get/set/delete/list operations on the config store service
 * via IPC. Uses a single page-aligned grant buffer (1 page = 4KB)
 * shared with the config service.
 *
 * Returns: 0 = success, -1 = error/not-found, -2 = access denied.
 */
#include "cubit.h"

/* IPC reply labels (must match config service) */
#define REPLY_OK            0xF000
#define REPLY_ERR           0xF001
#define REPLY_ACCESS_DENIED 0xF007

/* Grant buffer: 1 page */
#define CFG_GRANT_PAGES  1
#define CFG_GRANT_SIZE   (CFG_GRANT_PAGES * 4096)

#define PAGE_SIZE_CFG    4096

/* IPC message structure (must match kernel Process.Message) */
typedef struct __attribute__((packed)) {
    uint32_t label;
    uint8_t  length;
    uint8_t  flags;
    uint16_t badge;
} cfg_ipc_tag_t;

typedef struct __attribute__((packed)) {
    cfg_ipc_tag_t tag;
    uint64_t      capBadge;
    uint64_t      words[4];
} cfg_ipc_message_t;

/* Lazy-init state */
static void    *cfg_grant_buf  = NULL;
static uint64_t cfg_grant_id   = 0;
static int      cfg_initialized = 0;

/*
 * Align pointer up to page boundary
 */
static void *cfg_page_align(void *ptr)
{
    uintptr_t addr = (uintptr_t)ptr;
    addr = (addr + PAGE_SIZE_CFG - 1) & ~((uintptr_t)PAGE_SIZE_CFG - 1);
    return (void *)addr;
}

/*
 * ensure_config_init - lazy init grant buffer + discover config PID
 */
static int ensure_config_init(void)
{
    if (cfg_initialized)
        return 0;

    /* Allocate 2 pages to guarantee page alignment */
    void *raw = cubit_sbrk(2 * PAGE_SIZE_CFG);
    if ((long)raw == -1)
        return -1;

    cfg_grant_buf = cfg_page_align(raw);

    /* Discover config service PID */
    long configPID = syscall2(SYSCALL_INFO,
                              SYSINFO_REGISTERED_DRIVER,
                              DRIVER_CONFIG);
    if (configPID <= 0)
        return -1;

    /* Create read-write grant (1 page) */
    long gid = syscall4(SYSCALL_GRANT, configPID,
                        cfg_grant_buf, CFG_GRANT_PAGES, 1 /* RW */);
    if (gid == (long)(-1UL))
        return -1;

    cfg_grant_id = (uint64_t)gid;
    cfg_initialized = 1;
    return 0;
}

/*
 * Send capCall to config service, return 0 on success
 */
static int cfg_call(cfg_ipc_message_t *msg)
{
    long ret = syscall2(SYSCALL_CAP_CALL, CAP_SLOT_CONFIG, msg);
    if (ret == (long)(-1UL))
        return -1;
    return 0;
}

/*
 * Map reply label to return code
 */
static int cfg_map_status(uint32_t label)
{
    if (label == REPLY_OK)
        return 0;
    if (label == REPLY_ACCESS_DENIED)
        return -2;
    return -1;  /* REPLY_ERR or unknown */
}

/*
 * cubit_config_get - Get a config value by key
 *
 * Writes the value into buf (up to buf_size bytes).
 * Sets *out_len to the actual value length.
 * Returns: 0=success, -1=not found/error, -2=access denied
 */
int cubit_config_get(const char *key, void *buf,
                     size_t buf_size, size_t *out_len)
{
    if (!key || !buf)
        return -1;

    if (ensure_config_init() < 0)
        return -1;

    size_t keyLen = strlen(key);
    if (keyLen == 0 || keyLen > CFG_GRANT_SIZE)
        return -1;

    /* Write key to grant buffer */
    memcpy(cfg_grant_buf, key, keyLen);

    cfg_ipc_message_t msg;
    msg.tag.label  = OP_CONFIG_GET;
    msg.tag.length = 2;
    msg.tag.flags  = 0;
    msg.tag.badge  = 0;
    msg.capBadge   = 0;
    msg.words[0]   = cfg_grant_id;
    msg.words[1]   = keyLen;
    msg.words[2]   = 0;
    msg.words[3]   = 0;

    if (cfg_call(&msg) < 0)
        return -1;

    int rc = cfg_map_status(msg.tag.label);
    if (rc != 0)
        return rc;

    /* Value is in grant buffer, copy to caller */
    size_t valueLen = (size_t)msg.words[0];
    size_t copyLen = valueLen < buf_size ? valueLen : buf_size;
    memcpy(buf, cfg_grant_buf, copyLen);

    if (out_len)
        *out_len = valueLen;

    return 0;
}

/*
 * cubit_config_set - Set a config key to a value
 *
 * Returns: 0=success, -1=error, -2=access denied
 */
int cubit_config_set(const char *key, const void *value,
                     size_t value_len)
{
    if (!key)
        return -1;

    if (ensure_config_init() < 0)
        return -1;

    size_t keyLen = strlen(key);
    if (keyLen == 0 || keyLen + value_len > CFG_GRANT_SIZE)
        return -1;

    /* Write key at offset 0, value at offset keyLen */
    memcpy(cfg_grant_buf, key, keyLen);
    if (value && value_len > 0)
        memcpy((char *)cfg_grant_buf + keyLen, value, value_len);

    cfg_ipc_message_t msg;
    msg.tag.label  = OP_CONFIG_SET;
    msg.tag.length = 3;
    msg.tag.flags  = 0;
    msg.tag.badge  = 0;
    msg.capBadge   = 0;
    msg.words[0]   = cfg_grant_id;
    msg.words[1]   = keyLen;
    msg.words[2]   = value_len;
    msg.words[3]   = 0;

    if (cfg_call(&msg) < 0)
        return -1;

    return cfg_map_status(msg.tag.label);
}

/*
 * cubit_config_delete - Delete a config key
 *
 * Returns: 0=success, -1=not found/error, -2=access denied
 */
int cubit_config_delete(const char *key)
{
    if (!key)
        return -1;

    if (ensure_config_init() < 0)
        return -1;

    size_t keyLen = strlen(key);
    if (keyLen == 0 || keyLen > CFG_GRANT_SIZE)
        return -1;

    /* Write key to grant buffer */
    memcpy(cfg_grant_buf, key, keyLen);

    cfg_ipc_message_t msg;
    msg.tag.label  = OP_CONFIG_DELETE;
    msg.tag.length = 2;
    msg.tag.flags  = 0;
    msg.tag.badge  = 0;
    msg.capBadge   = 0;
    msg.words[0]   = cfg_grant_id;
    msg.words[1]   = keyLen;
    msg.words[2]   = 0;
    msg.words[3]   = 0;

    if (cfg_call(&msg) < 0)
        return -1;

    return cfg_map_status(msg.tag.label);
}

/*
 * parse_decimal - parse an ASCII decimal string to uint64_t
 */
static uint64_t parse_decimal(const char *buf, size_t len)
{
    uint64_t result = 0;
    for (size_t i = 0; i < len; i++) {
        if (buf[i] >= '0' && buf[i] <= '9')
            result = result * 10 + (uint64_t)(buf[i] - '0');
        else
            return 0;
    }
    return result;
}

/*
 * cubit_config_list - List keys matching a prefix
 *
 * Writes NUL-separated key names into buf (up to buf_size bytes).
 * Sets *out_count to the number of matching keys.
 * Pass empty string ("") or NULL prefix to list all keys.
 * Returns: 0=success, -1=error, -2=access denied
 */
int cubit_config_list(const char *prefix, char *buf,
                      size_t buf_size, int *out_count)
{
    if (!buf)
        return -1;

    if (ensure_config_init() < 0)
        return -1;

    size_t prefixLen = prefix ? strlen(prefix) : 0;
    if (prefixLen > CFG_GRANT_SIZE)
        return -1;

    /* Write prefix to grant buffer (may be empty) */
    if (prefixLen > 0)
        memcpy(cfg_grant_buf, prefix, prefixLen);

    cfg_ipc_message_t msg;
    msg.tag.label  = OP_CONFIG_LIST;
    msg.tag.length = 2;
    msg.tag.flags  = 0;
    msg.tag.badge  = 0;
    msg.capBadge   = 0;
    msg.words[0]   = cfg_grant_id;
    msg.words[1]   = prefixLen;
    msg.words[2]   = 0;
    msg.words[3]   = 0;

    if (cfg_call(&msg) < 0)
        return -1;

    int rc = cfg_map_status(msg.tag.label);
    if (rc != 0)
        return rc;

    /* NUL-separated keys are in grant buffer, copy to caller */
    int count = (int)msg.words[0];

    /* Calculate total bytes of NUL-separated key data */
    size_t totalBytes = 0;
    const char *src = (const char *)cfg_grant_buf;
    int seen = 0;
    while (seen < count && totalBytes < CFG_GRANT_SIZE) {
        while (totalBytes < CFG_GRANT_SIZE && src[totalBytes] != '\0')
            totalBytes++;
        if (totalBytes < CFG_GRANT_SIZE) {
            totalBytes++;  /* include NUL separator */
            seen++;
        }
    }

    size_t copyLen = totalBytes < buf_size ? totalBytes : buf_size;
    memcpy(buf, cfg_grant_buf, copyLen);

    if (out_count)
        *out_count = count;

    return 0;
}

/*
 * cubit_resolve_scheme - Resolve a scheme name to driver/slot/pid
 *
 * Builds "scheme.<name>.driver" and "scheme.<name>.slot", queries config,
 * parses decimal values, resolves driver PID via sysinfo.
 * Returns: 0=success, -1=not found/error
 */
int cubit_resolve_scheme(const char *name, cubit_scheme_info_t *info)
{
    if (!name || !info)
        return -1;

    info->driver_id = 0;
    info->cap_slot  = 0;
    info->pid       = 0;
    info->found     = 0;

    size_t nameLen = strlen(name);
    if (nameLen == 0 || nameLen > 64)
        return -1;

    /* Build "scheme.<name>.driver" */
    char keyBuf[128];
    size_t pos = 0;

    memcpy(keyBuf, "scheme.", 7);
    pos = 7;
    memcpy(keyBuf + pos, name, nameLen);
    pos += nameLen;
    memcpy(keyBuf + pos, ".driver", 7);
    pos += 7;
    keyBuf[pos] = '\0';

    char valBuf[32];
    size_t valLen = 0;

    if (cubit_config_get(keyBuf, valBuf, sizeof(valBuf), &valLen) != 0)
        return -1;

    info->driver_id = parse_decimal(valBuf, valLen);

    /* Build "scheme.<name>.slot" */
    pos = 7 + nameLen;
    memcpy(keyBuf + pos, ".slot", 5);
    pos += 5;
    keyBuf[pos] = '\0';

    if (cubit_config_get(keyBuf, valBuf, sizeof(valBuf), &valLen) != 0)
        return -1;

    info->cap_slot = parse_decimal(valBuf, valLen);

    /* Resolve driver PID via sysinfo */
    long pid = syscall2(SYSCALL_INFO, SYSINFO_REGISTERED_DRIVER,
                        info->driver_id);
    if (pid <= 0 || pid == (long)(-1UL))
        return -1;

    info->pid   = (uint64_t)pid;
    info->found = 1;
    return 0;
}
