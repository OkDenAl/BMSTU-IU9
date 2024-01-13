#include <ntddk.h>
#include <ntifs.h>
#include <ndk/exfuncs.h>
#include <ndk/ketypes.h>
#include <ntstrsafe.h>

#define NDEBUG
#include <debug.h>

typedef unsigned char BYTE;

NTSTATUS
NTAPI
DriverEntry(IN PDRIVER_OBJECT DriverObject,
            IN PUNICODE_STRING RegistryPath)
{
    ULONG buffer_size = 0;
    NTSTATUS status = ZwQuerySystemInformation(SystemProcessInformation,
                                               NULL,
                                               0,
                                               &buffer_size);
    if (status != STATUS_INFO_LENGTH_MISMATCH)
        return status;

    PVOID buffer = ExAllocatePool(PagedPool, buffer_size);
    if (!buffer)
        return STATUS_MEMORY_NOT_ALLOCATED;

    status = ZwQuerySystemInformation(SystemProcessInformation,
                                      buffer,
                                      buffer_size,
                                      &buffer_size);
    if (NT_ERROR(status)) {
        ExFreePool(buffer);
        return status;
    }

    SYSTEM_PROCESS_INFORMATION* process_entry = buffer;
    do {
        if (process_entry->ImageName.Length) {
            DPRINT1("%lu : %S\n", process_entry->UniqueProcessId,
                                  process_entry->ImageName.Buffer);
        }
        process_entry = (SYSTEM_PROCESS_INFORMATION*)(
                (BYTE*)process_entry + process_entry->NextEntryOffset);
    } while (process_entry->NextEntryOffset);

    ExFreePool(buffer);
    return STATUS_SUCCESS;
}
